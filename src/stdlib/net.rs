use std::{any::Any, sync::Arc};

use smol::{io, net::UdpSocket};

use crate::{
    Builder,
    error::{ScriptError, TypeError},
    ext::{ExternalType, ExternalValue, NativeFunction, NativeMethod, NativeMethodRef},
    ident::Ident,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleItemType, TupleType},
    script_value::{ScriptValue, Tuple},
};

pub fn build(builder: &mut Builder) {
    builder.add_function("UdpSocket::bind", UdpBind);
}

struct ScriptSocketType;

impl ExternalType for ScriptSocketType {
    fn name(&self) -> Ident {
        "Socket".into()
    }

    fn get_method(&self, name: &Ident) -> Option<NativeMethodRef> {
        match name.as_str() {
            "send_to" => Some(NativeMethodRef::new(Arc::new(SendToMethod))),
            _ => None,
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct ScriptSocket(UdpSocket);

impl ScriptSocket {
    async fn bind(addr: &str) -> io::Result<Self> {
        let sock = UdpSocket::bind(addr).await?;
        Ok(Self(sock))
    }
}

impl ExternalValue for ScriptSocket {
    fn as_readable(&self) -> Option<&(dyn crate::ext::Readable + Send + Sync)> {
        unimplemented!("readable for socket")
    }

    fn as_writable(&self) -> Option<&(dyn crate::ext::Writable + Send + Sync)> {
        unimplemented!("writable for socket")
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct UdpBind;
impl NativeFunction for UdpBind {
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Str)
    }

    fn return_type(&self, _: &TupleType) -> ScriptType {
        let ext = ScriptType::Ext(Arc::new(ScriptSocketType));
        ScriptType::fallible_of(ext, ScriptType::Str)
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let addr = arguments.single()?.as_string()?;
        smol::block_on(async move {
            match ScriptSocket::bind(&addr).await {
                Ok(value) => {
                    let ext = ScriptValue::Ext(Arc::new(ScriptSocketType), Arc::new(value));
                    Ok(ScriptValue::ok(ext))
                }
                Err(err) => Ok(ScriptValue::err(ScriptValue::string(err.to_string()))),
            }
        })
    }
}

struct SendToMethod;
impl NativeMethod for SendToMethod {
    fn arguments_type(&self, _: &ScriptType) -> Result<TupleType, TypeError> {
        // TODO: We could be clever, and require only 'content' if socket is connected
        Ok(TupleType::new(vec![
            TupleItemType::named("addr", ScriptType::Str),
            TupleItemType::named("content", ScriptType::Str),
        ]))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> Result<ScriptType, TypeError> {
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
    ) -> Result<ScriptValue, ScriptError> {
        let sock = subject.downcast_ext::<ScriptSocket>()?;

        let mut args = arguments.iter_args();
        let addr = args
            .get("addr")
            .ok_or_else(|| ScriptError::panic("Expected address"))?
            .as_string()?;
        let content = args
            .get("content")
            .ok_or_else(|| ScriptError::panic("Expected content"))?
            .as_string()?;

        smol::block_on(async move {
            match sock.0.send_to(content.as_bytes(), addr.as_ref()).await {
                Ok(_) => Ok(ScriptValue::identity()),
                Err(err) => Ok(ScriptValue::err(ScriptValue::string(err.to_string()))),
            }
        })
    }
}
