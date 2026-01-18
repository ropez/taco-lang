use std::{any::Any, sync::Arc};

use async_lock::Mutex;
use async_trait::async_trait;
use smol::channel;

use crate::{
    Builder,
    error::{ScriptError, ScriptResult, TypeError, TypeResult},
    ext::{ExternalType, ExternalValue, NativeFunction, NativeMethodRef, Readable, Writable},
    ident::Ident,
    interpreter::{Interpreter, Scope},
    parser::Statement,
    script_type::{FunctionType, ScriptType, TupleType},
    script_value::{ScriptValue, Tuple, TupleItem},
};

type Task = smol::Task<Result<(), ScriptError>>;

pub fn build(builder: &mut Builder) {
    builder.add_function("apply", ActorFunc);
}

pub(crate) struct PipeType {
    lhs: Option<ScriptType>,
    rhs: Option<ScriptType>,
}

impl PipeType {
    pub(crate) fn new(lhs: Option<ScriptType>, rhs: Option<ScriptType>) -> Self {
        Self { lhs, rhs }
    }
}

impl ExternalType for PipeType {
    fn name(&self) -> Ident {
        "Pipe".into()
    }

    fn get_method(&self, _name: &Ident) -> Option<NativeMethodRef> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_readable(&self) -> Option<ScriptType> {
        self.lhs.clone()
    }

    fn as_writable(&self) -> Option<ScriptType> {
        self.rhs.clone()
    }
}

#[derive(Clone)]
pub(crate) struct PipeImpl {
    src: Arc<dyn ExternalValue + Send + Sync>,
    dst: Arc<dyn ExternalValue + Send + Sync>,
}

impl PipeImpl {
    pub(crate) fn new(
        src: Arc<dyn ExternalValue + Send + Sync>,
        dst: Arc<dyn ExternalValue + Send + Sync>,
    ) -> Self {
        Self { src, dst }
    }
}

pub(crate) fn exec_pipe(
    interpreter: &Interpreter,
    lhs: Arc<dyn ExternalValue + Send + Sync>,
    rhs: Arc<dyn ExternalValue + Send + Sync>,
) -> ScriptResult<()> {
    // XXX Cloning the interpreter feels wrong
    let i = interpreter.clone();

    // XXX
    // The 'spawn' function is only ment as convenience for "unit tests and small programs". It's
    // single threaded by default, and can be overridden by SMOL_THREADS. smol recommends creting
    // an Executor instead. Since 'spawn' is the only function in smol itself, by doing this we can
    // get rin of smol, and only use it's dependencies directly ('async-io', 'futures-lite' etc).
    let task = smol::spawn(async move {
        let src = lhs
            .as_readable()
            .ok_or_else(|| ScriptError::panic("src is not readable"))?;
        let dst = rhs
            .as_writable()
            .ok_or_else(|| ScriptError::panic("dst is not writable"))?;

        while let Some(val) = src.read(&i).await? {
            dst.write(&i, val.clone()).await?;
        }

        dst.close().await?;

        Ok(())
    });

    // Track running tasks, so that we can wait after the script has completed
    interpreter.tracker.track(task);

    Ok(())
}

pub(crate) fn exec_spawn(
    interpreter: &Interpreter,
    body: Arc<Vec<Statement>>,
    scope: Scope,
) -> ScriptResult<()> {
    // XXX Cloning the interpreter feels wrong
    let i = interpreter.clone();

    // XXX See above
    let task = smol::spawn(async move {
        i.execute_block(&body, scope)?;

        Ok(())
    });

    // Track running tasks, so that we can wait after the script has completed
    interpreter.tracker.track(task);

    Ok(())
}

impl ExternalValue for PipeImpl {
    fn as_readable(&self) -> Option<&(dyn Readable + Send + Sync)> {
        self.dst.as_readable()
    }

    fn as_writable(&self) -> Option<&(dyn Writable + Send + Sync)> {
        self.src.as_writable()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct ActorFunc;
impl NativeFunction for ActorFunc {
    fn arguments_type(&self, arguments: &TupleType) -> TypeResult<TupleType> {
        let arg = arguments.single()?;
        let params = arg.as_callable_params(&TupleType::from_single(ScriptType::Unknown))?;
        let ret = arg.as_callable_ret(&params)?;
        Ok(TupleType::from_single(ScriptType::Function(
            FunctionType::new(params, ret),
        )))
    }

    fn return_type(&self, arguments: &TupleType) -> TypeResult<ScriptType> {
        let arg = arguments.single().cloned()?;
        if let ScriptType::Function(fun) = arg {
            let lhs = fun.params.single().cloned().unwrap();

            // Not really a pipe, but the type works here
            let pipe = PipeType::new(Some(lhs.clone()), Some(fun.ret.clone()));

            Ok(ScriptType::Ext(Arc::new(pipe)))
        } else {
            Err(TypeError::invalid_argument("function", arg))
        }
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let callable = arguments.single().cloned()?;

        let t = Arc::new(ActorType(ScriptType::Infer(1), ScriptType::Infer(2))); // Dummy arguments, not used
        let v = Actor::new(callable);
        Ok(ScriptValue::Ext(t, Arc::new(v)))
    }
}

struct ActorType(ScriptType, ScriptType);
impl ExternalType for ActorType {
    fn name(&self) -> Ident {
        "Actor".into()
    }

    fn get_method(&self, _name: &Ident) -> Option<NativeMethodRef> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_readable(&self) -> Option<ScriptType> {
        Some(self.1.clone())
    }

    fn as_writable(&self) -> Option<ScriptType> {
        Some(self.0.clone())
    }
}

struct Actor {
    callable: ScriptValue,

    sender: channel::Sender<Option<ScriptValue>>,
    receiver: channel::Receiver<Option<ScriptValue>>,
}

impl Actor {
    fn new(callable: ScriptValue) -> Self {
        let (sender, receiver) = channel::unbounded::<Option<ScriptValue>>();
        Self {
            callable,
            sender,
            receiver,
        }
    }
}

impl ExternalValue for Actor {
    fn as_readable(&self) -> Option<&(dyn Readable + Send + Sync)> {
        Some(self)
    }

    fn as_writable(&self) -> Option<&(dyn Writable + Send + Sync)> {
        Some(self)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[async_trait]
impl Readable for Actor {
    async fn read(&self, interpreter: &Interpreter) -> ScriptResult<Option<ScriptValue>> {
        let value = self.receiver.recv().await.map_err(ScriptError::panic)?;
        if let Some(value) = value {
            let args = Tuple::new(vec![TupleItem::unnamed(value.clone())]);
            let val = interpreter.eval_callable(self.callable.clone(), &args)?;
            Ok(Some(val))
        } else {
            Ok(None)
        }
    }
}

#[async_trait]
impl Writable for Actor {
    async fn write(&self, _: &Interpreter, value: ScriptValue) -> ScriptResult<()> {
        self.sender
            .send(Some(value.clone()))
            .await
            .map_err(ScriptError::panic)?;
        Ok(())
    }

    async fn close(&self) -> ScriptResult<()> {
        self.sender.send(None).await.map_err(ScriptError::panic)?;
        Ok(())
    }
}

#[derive(Clone, Default)]
pub(crate) struct Tracker {
    tasks: Arc<Mutex<Vec<Task>>>,
}

impl Tracker {
    pub(crate) fn track(&self, task: Task) {
        let mut tasks = self.tasks.lock_arc_blocking();
        tasks.push(task);
    }

    pub(crate) fn wait_all(&self) -> Vec<ScriptError> {
        let mut errors = Vec::new();
        smol::block_on(async {
            while let Some(task) = self.pop().await {
                if let Err(err) = task.await {
                    errors.push(err);
                }
            }
        });
        errors
    }

    async fn pop(&self) -> Option<Task> {
        let mut tasks = self.tasks.lock().await;
        tasks.pop()
    }
}
