use httparse::Response;
use rustls::{ClientConnection, pki_types::ServerName};
use smol::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
};
use std::{net::ToSocketAddrs, sync::Arc};
use url::Url;

use crate::{
    Builder,
    error::{ScriptError, TypeError},
    ext::{ExternalType, ExternalValue, NativeFunction, NativeMethod, NativeMethodRef},
    ident::Ident,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleItemType, TupleType},
    script_value::{ContentType, ScriptValue, Tuple},
    stdlib::http::tls_stream::TlsStream,
};

mod tls_stream;

pub fn build(builder: &mut Builder) {
    builder.add_function("Http::fetch", FetchFunc);
}

struct ResponseType;
impl ExternalType for ResponseType {
    fn name(&self) -> Ident {
        "Response".into()
    }

    fn get_method(&self, name: &Ident) -> Option<NativeMethodRef> {
        match name.as_str() {
            "status" => Some(NativeMethodRef::new(Arc::new(StatusMethod))),
            "body" => Some(NativeMethodRef::new(Arc::new(BodyMethod))),
            _ => None,
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

struct ResponseValue {
    status_code: u16,
    body: Option<Arc<str>>,
}

impl ResponseValue {
    fn new(code: u16, body: Option<Arc<str>>) -> Self {
        Self {
            status_code: code,
            body,
        }
    }
}

impl ExternalValue for ResponseValue {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

struct FetchFunc;
impl NativeFunction for FetchFunc {
    fn arguments_type(&self) -> TupleType {
        let header_tuple = ScriptType::Tuple(TupleType::new(vec![
            TupleItemType::named("name", ScriptType::Str),
            TupleItemType::named("value", ScriptType::Str),
        ]));
        let args = vec![
            TupleItemType::unnamed(ScriptType::Str),
            TupleItemType::optional("mathod", ScriptType::Str),
            TupleItemType::optional("body", ScriptType::Str),
            TupleItemType::optional("headers", ScriptType::list_of(header_tuple)),
        ];

        TupleType::new(args)
    }

    fn return_type(&self, _arguments: &TupleType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Ext(Arc::new(ResponseType)))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let config = make_config();

        let mut args = arguments.iter_args();
        let url = args
            .get("url")
            .ok_or_else(|| ScriptError::panic("Expected URL"))?
            .as_string()?;
        let method = args
            .get("method")
            .map(|val| val.as_string())
            .transpose()?
            .unwrap_or_else(|| "GET".into());
        let body = args.get("body").map(|val| val.as_string()).transpose()?;
        let extra_headers = args.get("headers").map(|val| val.as_iterable());

        let url = Url::parse(&url).map_err(ScriptError::panic)?;
        let hostname = url
            .host_str()
            .ok_or_else(|| ScriptError::panic("Invalid URL"))?;
        let path = url.path();

        let sock_addr = (hostname, 443).to_socket_addrs().unwrap().next().unwrap();

        let server_name: ServerName = hostname.try_into().unwrap();

        smol::block_on(async move {
            let connection = ClientConnection::new(config.clone(), server_name.to_owned())
                .map_err(ScriptError::panic)?;

            // Connect to server
            let stream = TcpStream::connect(sock_addr).await.unwrap();

            // Create TlsStream and complete handshake
            let mut stream = TlsStream::new(connection, stream);
            stream.flush().await.unwrap();

            let mut headers = vec![
                format!("{method} {path} HTTP/1.1"),
                format!("Host: {hostname}"),
                "Connection: close".to_string(),
                "User-Agent: Taco/0.1".to_string(),
                "Accept: application/json".to_string(),
                "Content-type: application/json".to_string(),
                format!(
                    "Content-Length: {}",
                    body.as_ref().map(|s| s.len()).unwrap_or(0)
                ),
            ];

            if let Some(eh) = extra_headers {
                for h in eh {
                    let tup = h
                        .as_tuple()
                        .ok_or_else(|| ScriptError::panic("Expected tuple"))?;
                    let mut args = tup.iter_args();
                    headers.push(format!(
                        "{}: {}",
                        args.get("name").unwrap(),
                        args.get("value").unwrap()
                    ));
                }
            }

            headers.push("\r\n".into());

            let head = headers.join("\r\n");

            stream.write(head.as_bytes()).await.unwrap();
            if let Some(body) = body {
                stream.write(body.as_bytes()).await.unwrap();
            }
            stream.flush().await.unwrap();

            let mut buf = Vec::new();
            match stream.read_to_end(&mut buf).await {
                Ok(_) => {}
                Err(err) => match err.kind() {
                    // XXX https://docs.rs/rustls/latest/rustls/manual/_03_howto/index.html#unexpected-eof
                    std::io::ErrorKind::UnexpectedEof => {}
                    _ => panic!("{err}"),
                },
            }

            let mut headers = [httparse::EMPTY_HEADER; 32];
            let mut res = Response::new(&mut headers);

            match res.parse(&buf).map_err(ScriptError::panic)? {
                httparse::Status::Complete(n) => {
                    let code = res.code.unwrap_or_default();
                    let body = if buf.len() > n {
                        Some(parse_body(&res, &buf[n..])?)
                    } else {
                        None
                    };
                    let response = ResponseValue::new(code, body);

                    Ok(ScriptValue::Ext(Arc::new(ResponseType), Arc::new(response)))
                }
                httparse::Status::Partial => todo!("Partially parsed"),
            }
        })
    }
}

fn parse_body(res: &Response<'_, '_>, buf: &[u8]) -> Result<Arc<str>, ScriptError> {
    if let Some(h) = res
        .headers
        .iter()
        .find(|h| h.name.eq_ignore_ascii_case("transfer-encoding"))
    {
        let enc = str::from_utf8(h.value).map_err(ScriptError::panic)?;
        if enc.eq_ignore_ascii_case("chunked") {
            let mut b = String::new();

            let mut r = buf;
            while !r.is_empty() {
                let p = r
                    .iter()
                    .position(|c| *c == b'\r')
                    .ok_or_else(|| ScriptError::panic("Parse error"))?;
                if p == 0 {
                    break;
                }
                let (sz, rest) = r.split_at(p);
                let src = str::from_utf8(sz).map_err(ScriptError::panic)?;
                let size: usize = usize::from_str_radix(src, 16).map_err(ScriptError::panic)?;
                b.push_str(str::from_utf8(&rest[2..2 + size]).map_err(ScriptError::panic)?);

                r = &rest[2 + size + 2..];
            }

            Ok(b.into())
        } else {
            todo!("encoding: {:?}", h.value);
        }
    } else {
        let s = str::from_utf8(buf).map_err(ScriptError::panic)?;
        Ok(s.into())
    }
}

struct StatusMethod;
impl NativeMethod for StatusMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Int)
    }

    fn call(&self, _: &Interpreter, s: ScriptValue, _: &Tuple) -> Result<ScriptValue, ScriptError> {
        let val = s.downcast_ext::<ResponseValue>()?;
        Ok(ScriptValue::Int(val.status_code as i64))
    }
}

struct BodyMethod;
impl NativeMethod for BodyMethod {
    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        _: &Tuple,
    ) -> Result<ScriptValue, ScriptError> {
        let val = subject.downcast_ext::<ResponseValue>()?;

        match &val.body {
            Some(body) => Ok(ScriptValue::string_with_type(
                Arc::clone(body),
                ContentType::Json, // XXX Derive from headers
            )),
            None => Ok(ScriptValue::empty_string()),
        }
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
        Ok(ScriptType::Str)
    }
}

fn make_config() -> Arc<rustls::ClientConfig> {
    let root_store =
        rustls::RootCertStore::from_iter(webpki_roots::TLS_SERVER_ROOTS.iter().cloned());

    let config = rustls::ClientConfig::builder()
        .with_root_certificates(root_store)
        .with_no_client_auth();

    Arc::new(config)
}
