use std::{any::Any, net::SocketAddr, sync::Arc, time::Duration};

use async_lock::Mutex;
use async_trait::async_trait;
use smol::{
    Timer,
    future::FutureExt,
    io::{self, AsyncReadExt, AsyncWriteExt},
    net::{TcpListener, TcpStream, UdpSocket},
};

use crate::{
    Builder,
    error::{ScriptError, ScriptResult, TypeResult},
    ext::{
        ExternalType, ExternalValue, NativeFunction, NativeMethod, NativeMethodRef, Readable,
        Writable,
    },
    ident::Ident,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleItemType, TupleType},
    script_value::{ScriptValue, Tuple, TupleItem},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("UdpSocket::bind", UdpBind);
    builder.add_function("TcpStream::connect", TcpConnect);
    builder.add_function("TcpListener::listen", TcpListen);
}

struct UdpSocketType;

impl ExternalType for UdpSocketType {
    fn name(&self) -> Ident {
        "UdpSocket".into()
    }

    fn get_method(&self, name: &Ident) -> Option<NativeMethodRef> {
        match name.as_str() {
            "send_to" => Some(NativeMethodRef::new(Arc::new(SendToMethod))),
            "recv_from" => Some(NativeMethodRef::new(Arc::new(RecvFromMethod))),
            _ => None,
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_readable(&self) -> Option<ScriptType> {
        Some(ScriptType::Str)
    }

    fn as_writable(&self) -> Option<ScriptType> {
        None
    }
}

struct UdpSocketValue(UdpSocket);

impl UdpSocketValue {
    async fn bind(addr: &str) -> io::Result<Self> {
        let sock = UdpSocket::bind(addr).await?;
        Ok(Self(sock))
    }

    async fn send_to(&self, addr: &str, bytes: &[u8], timeout: Option<Duration>) -> io::Result<()> {
        self.0
            .send_to(bytes, addr)
            .or(make_timeout(timeout))
            .await?;
        Ok(())
    }

    async fn recv_from(&self, timeout: Option<Duration>) -> io::Result<(Vec<u8>, SocketAddr)> {
        let mut buf = [0u8; 10000];
        let (len, b) = self.0.recv_from(&mut buf).or(make_timeout(timeout)).await?;
        Ok((buf[..len].to_vec(), b))
    }
}

impl ExternalValue for UdpSocketValue {
    fn as_readable(&self) -> Option<&(dyn Readable + Send + Sync)> {
        Some(self)
    }

    fn as_writable(&self) -> Option<&(dyn Writable + Send + Sync)> {
        unimplemented!("writable for UdpSocket")
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[async_trait]
impl Readable for UdpSocketValue {
    async fn read(&self, _: &Interpreter) -> ScriptResult<Option<ScriptValue>> {
        let (buf, _) = self.recv_from(None).await.map_err(ScriptError::panic)?;

        let s = str::from_utf8(&buf).map_err(ScriptError::panic)?;

        Ok(Some(ScriptValue::string(s)))
    }
}

struct UdpBind;
impl NativeFunction for UdpBind {
    fn arguments_type(&self, _: &TupleType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Str))
    }

    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        let ext = ScriptType::Ext(Arc::new(UdpSocketType));
        Ok(ScriptType::fallible_of(ext, ScriptType::Str))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let addr = arguments.single()?.as_string()?;
        smol::block_on(async move {
            match UdpSocketValue::bind(&addr).await {
                Ok(value) => {
                    let ext = ScriptValue::Ext(Arc::new(UdpSocketType), Arc::new(value));
                    Ok(ScriptValue::ok(ext))
                }
                Err(err) => Ok(ScriptValue::err(ScriptValue::string(err.to_string()))),
            }
        })
    }
}

struct SendToMethod;
impl NativeMethod for SendToMethod {
    fn arguments_type(&self, _: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::new(vec![
            TupleItemType::named("addr", ScriptType::Str),
            TupleItemType::named("content", ScriptType::Str),
            TupleItemType::named("timeout", ScriptType::opt_of(ScriptType::Int)),
        ]))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        // XXX Need to define custom error types like "Net::Error"
        Ok(ScriptType::fallible_of(
            ScriptType::identity(),
            ScriptType::Str,
        ))
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let sock = subject.downcast_ext::<UdpSocketValue>()?;

        let mut args = arguments.iter_args();
        let addr = args
            .get("addr")
            .ok_or_else(|| ScriptError::panic("Expected address"))?
            .as_string()?;
        let content = args
            .get("content")
            .ok_or_else(|| ScriptError::panic("Expected content"))?
            .as_string()?;
        let timeout = as_opt_duration(args.get("timeout"))?;

        smol::block_on(async move {
            match sock
                .send_to(addr.as_ref(), content.as_bytes(), timeout)
                .await
            {
                Ok(_) => Ok(ScriptValue::ok(ScriptValue::identity())),
                Err(err) => Ok(ScriptValue::err(ScriptValue::string(err.to_string()))),
            }
        })
    }
}

struct RecvFromMethod;
impl NativeMethod for RecvFromMethod {
    fn arguments_type(&self, _: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::new(vec![TupleItemType::named(
            "timeout",
            ScriptType::opt_of(ScriptType::Int),
        )]))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        // FIXME Should use a rec here
        let tuple = ScriptType::Tuple(TupleType::new(vec![
            TupleItemType::unnamed(ScriptType::Str),
            TupleItemType::unnamed(ScriptType::Str),
        ]));

        Ok(ScriptType::fallible_of(tuple, ScriptType::Str))
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let sock = subject.downcast_ext::<UdpSocketValue>()?;

        let mut args = arguments.iter_args();
        let timeout = as_opt_duration(args.get("timeout"))?;

        smol::block_on(async move {
            match sock.recv_from(timeout).await {
                Ok((buf, addr)) => {
                    let s = str::from_utf8(&buf).map_err(ScriptError::panic)?; // XXX
                    let tuple = Arc::new(Tuple::new(vec![
                        TupleItem::unnamed(ScriptValue::string(s)),
                        TupleItem::unnamed(ScriptValue::string(addr.to_string())),
                    ]));
                    Ok(ScriptValue::ok(ScriptValue::Tuple(tuple)))
                }
                Err(err) => Ok(ScriptValue::err(ScriptValue::string(err.to_string()))),
            }
        })
    }
}

struct TcpListenerType;
impl ExternalType for TcpListenerType {
    fn name(&self) -> Ident {
        "TcpListener".into()
    }

    fn get_method(&self, name: &Ident) -> Option<NativeMethodRef> {
        match name.as_str() {
            "accept" => Some(NativeMethodRef::new(Arc::new(AcceptMethod))),
            _ => None,
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct TcpStreamType;
impl ExternalType for TcpStreamType {
    fn name(&self) -> Ident {
        "TcpStream".into()
    }

    fn get_method(&self, name: &Ident) -> Option<NativeMethodRef> {
        match name.as_str() {
            "send" => Some(NativeMethodRef::new(Arc::new(SendMethod))),
            "recv" => Some(NativeMethodRef::new(Arc::new(RecvMethod))),
            _ => None,
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_readable(&self) -> Option<ScriptType> {
        Some(ScriptType::Str)
    }

    fn as_writable(&self) -> Option<ScriptType> {
        Some(ScriptType::Str)
    }
}

struct TcpListenerValue(TcpListener);

impl ExternalValue for TcpListenerValue {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl TcpListenerValue {
    async fn listen(addr: &str) -> io::Result<Self> {
        let listener = TcpListener::bind(addr).await?;
        Ok(Self(listener))
    }
}

struct TcpStreamValue {
    recv_sock: Mutex<TcpStream>,
    send_sock: Mutex<TcpStream>,
}

impl TcpStreamValue {
    fn new(sock: TcpStream) -> Self {
        Self {
            recv_sock: Mutex::new(sock.clone()),
            send_sock: Mutex::new(sock),
        }
    }

    async fn connect(addr: &str, timeout: Option<Duration>) -> io::Result<Self> {
        let stream = TcpStream::connect(addr).or(make_timeout(timeout)).await?;
        Ok(Self::new(stream))
    }

    async fn recv(&self, timeout: Option<Duration>) -> io::Result<Option<Vec<u8>>> {
        let mut sock = self.recv_sock.lock().await;
        let mut buf = [0u8; 10000]; // XXX
        let len = sock.read(&mut buf).or(make_timeout(timeout)).await?;
        if len > 0 {
            Ok(Some(buf[..len].to_vec()))
        } else {
            Ok(None)
        }
    }

    async fn send(&self, bytes: &[u8], timeout: Option<Duration>) -> io::Result<()> {
        let mut sock = self.send_sock.lock().await;
        sock.write(bytes).or(make_timeout(timeout)).await?;

        Ok(())
    }

    async fn close(&self) -> io::Result<()> {
        let mut sock = self.send_sock.lock().await;
        sock.close().await?;

        Ok(())
    }
}

impl ExternalValue for TcpStreamValue {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_readable(&self) -> Option<&(dyn Readable + Send + Sync)> {
        Some(self)
    }

    fn as_writable(&self) -> Option<&(dyn Writable + Send + Sync)> {
        Some(self)
    }
}

#[async_trait]
impl Readable for TcpStreamValue {
    async fn read(&self, _: &Interpreter) -> ScriptResult<Option<ScriptValue>> {
        if let Some(buf) = self.recv(None).await.map_err(ScriptError::panic)? {
            let s = str::from_utf8(&buf).map_err(ScriptError::panic)?;

            Ok(Some(ScriptValue::string(s)))
        } else {
            Ok(None)
        }
    }
}

#[async_trait]
impl Writable for TcpStreamValue {
    async fn write(&self, _: &Interpreter, value: ScriptValue) -> ScriptResult<()> {
        let content = value.as_string()?;
        self.send(content.as_bytes(), None)
            .await
            .map_err(ScriptError::panic)?;
        Ok(())
    }

    async fn close(&self) -> ScriptResult<()> {
        self.close().await.map_err(ScriptError::panic)
    }
}

struct TcpConnect;
impl NativeFunction for TcpConnect {
    fn arguments_type(&self, _: &TupleType) -> TypeResult<TupleType> {
        Ok(TupleType::new(vec![
            TupleItemType::named("address", ScriptType::Str),
            TupleItemType::named("timeout", ScriptType::opt_of(ScriptType::Int)),
        ]))
    }

    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        let ext = ScriptType::Ext(Arc::new(TcpStreamType));
        Ok(ScriptType::fallible_of(ext, ScriptType::Str))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let mut args = arguments.iter_args();
        let addr = args
            .get("address")
            .ok_or_else(|| ScriptError::panic("Expected address"))?
            .as_string()?;
        let timeout = as_opt_duration(args.get("timeout"))?;

        smol::block_on(async move {
            match TcpStreamValue::connect(&addr, timeout).await {
                Ok(value) => {
                    let ext = ScriptValue::Ext(Arc::new(TcpStreamType), Arc::new(value));
                    Ok(ScriptValue::ok(ext))
                }
                Err(err) => Ok(ScriptValue::err(ScriptValue::string(err.to_string()))),
            }
        })
    }
}

struct TcpListen;
impl NativeFunction for TcpListen {
    fn arguments_type(&self, _: &TupleType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Str))
    }

    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        let ext = ScriptType::Ext(Arc::new(TcpListenerType));
        Ok(ScriptType::fallible_of(ext, ScriptType::Str))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let addr = arguments.single()?.as_string()?;
        smol::block_on(async move {
            match TcpListenerValue::listen(&addr).await {
                Ok(value) => {
                    let ext = ScriptValue::Ext(Arc::new(TcpListenerType), Arc::new(value));
                    Ok(ScriptValue::ok(ext))
                }
                Err(err) => Ok(ScriptValue::err(ScriptValue::string(err.to_string()))),
            }
        })
    }
}

struct AcceptMethod;
impl NativeMethod for AcceptMethod {
    fn arguments_type(&self, _: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::identity())
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        // FIXME Should use a rec here
        let ext = ScriptType::Ext(Arc::new(TcpStreamType));
        let tuple = ScriptType::Tuple(TupleType::new(vec![
            TupleItemType::unnamed(ext),
            TupleItemType::unnamed(ScriptType::Str),
        ]));

        Ok(ScriptType::fallible_of(tuple, ScriptType::Str))
    }

    fn call(&self, _: &Interpreter, subject: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let listener = subject.downcast_ext::<TcpListenerValue>()?;

        smol::block_on(async move {
            match listener.0.accept().await {
                Ok((stream, b)) => {
                    let ext = ScriptValue::Ext(
                        Arc::new(TcpStreamType),
                        Arc::new(TcpStreamValue::new(stream)),
                    );

                    let tuple = Arc::new(Tuple::new(vec![
                        TupleItem::unnamed(ext),
                        TupleItem::unnamed(ScriptValue::string(b.to_string())),
                    ]));
                    Ok(ScriptValue::ok(ScriptValue::Tuple(tuple)))
                }
                Err(err) => Ok(ScriptValue::err(ScriptValue::string(err.to_string()))),
            }
        })
    }
}

struct SendMethod;
impl NativeMethod for SendMethod {
    fn arguments_type(&self, _: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::new(vec![
            TupleItemType::named("content", ScriptType::Str),
            TupleItemType::named("timeout", ScriptType::opt_of(ScriptType::Int)),
        ]))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        // XXX Need to define custom error types like "Net::Error"
        Ok(ScriptType::fallible_of(
            ScriptType::identity(),
            ScriptType::Str,
        ))
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let sock = subject.downcast_ext::<TcpStreamValue>()?;

        let mut args = arguments.iter_args();
        let content = args
            .get("content")
            .ok_or_else(|| ScriptError::panic("Expected content"))?
            .as_string()?;
        let timeout = as_opt_duration(args.get("timeout"))?;

        smol::block_on(async move {
            match sock.send(content.as_bytes(), timeout).await {
                Ok(_) => Ok(ScriptValue::ok(ScriptValue::identity())),
                Err(err) => Ok(ScriptValue::err(ScriptValue::string(err.to_string()))),
            }
        })
    }
}

struct RecvMethod;
impl NativeMethod for RecvMethod {
    fn arguments_type(&self, _: &ScriptType) -> TypeResult<TupleType> {
        Ok(TupleType::new(vec![TupleItemType::named(
            "timeout",
            ScriptType::opt_of(ScriptType::Int),
        )]))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::fallible_of(
            ScriptType::opt_of(ScriptType::Str),
            ScriptType::Str,
        ))
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        arguments: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let sock = subject.downcast_ext::<TcpStreamValue>()?;

        let mut args = arguments.iter_args();
        let timeout = as_opt_duration(args.get("timeout"))?;

        smol::block_on(async move {
            match sock.recv(timeout).await {
                Ok(Some(s)) => {
                    let s = str::from_utf8(&s).map_err(ScriptError::panic)?;
                    let val = ScriptValue::string(s);
                    Ok(ScriptValue::ok(ScriptValue::opt(Some(val))))
                }
                Ok(None) => Ok(ScriptValue::ok(ScriptValue::opt(None))),
                Err(err) => Ok(ScriptValue::err(ScriptValue::string(err.to_string()))),
            }
        })
    }
}

fn as_opt_duration(arg: Option<ScriptValue>) -> ScriptResult<Option<Duration>> {
    Ok(arg
        .map(|a| a.as_int())
        .transpose()?
        .map(|ms| ms.try_into())
        .transpose()
        .map_err(ScriptError::panic)?
        .map(Duration::from_millis))
}

async fn make_timeout<T>(duration: Option<Duration>) -> io::Result<T> {
    match duration {
        Some(d) => Timer::after(d),
        None => Timer::never(),
    }
    .await;
    Err(io::ErrorKind::TimedOut.into())
}
