use std::{
    any::Any,
    collections::VecDeque,
    sync::{Arc, Mutex, RwLock},
    task::{Context, Poll, Waker},
};

use smol::future::{FutureExt, poll_fn};

use crate::{
    Builder,
    error::ScriptError,
    ext::{ExternalType, ExternalValue, NativeFunction, NativeMethodRef, Readable, Writable},
    ident::Ident,
    interpreter::{Interpreter, ScriptValue, Tuple, TupleItem},
    validate::{ScriptType, TupleType},
};

type Task = smol::Task<Result<(), ScriptError>>;

// Make "stream" or "actor" is a better name
pub fn build(builder: &mut Builder) {
    builder.add_function("apply", ActorFunc);
}

pub(crate) struct PipeType;
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
) -> Result<(), ScriptError> {
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

        while let Some(val) = poll_fn(|ctx| src.read(ctx, &i)).await? {
            poll_fn(|ctx| dst.write(ctx, &i, val.clone())).await?;
        }

        poll_fn(|ctx| dst.close(ctx)).await?;

        Ok(())
    });

    // XXX Must track running tasks
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
    fn arguments_type(&self) -> TupleType {
        TupleType::from_single(ScriptType::Function {
            params: TupleType::from_single(ScriptType::Infer(1)),
            ret: ScriptType::Infer(2).into(),
        })
    }

    fn return_type(&self, arguments: &TupleType) -> ScriptType {
        let arg = arguments.single().unwrap();
        if let ScriptType::Function { params, ret } = arg {
            let lhs = params.single().cloned().unwrap();
            ScriptType::Pipe(Some(Box::new(lhs.clone())), Some(ret.clone()))
        } else {
            todo!("error")
        }
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> Result<ScriptValue, ScriptError> {
        let callable = arguments.single()?;

        let t = Arc::new(ActorType(ScriptType::Infer(1), ScriptType::Infer(2))); // Dummy arguments, not used
        let v = Actor::new(callable.clone());
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

    // XXX Seems natural to use 'channel' here, but couldn't get the poll methods to work
    queue: Mutex<VecDeque<Option<ScriptValue>>>,
    waker: RwLock<Option<Waker>>,
}

impl Actor {
    fn new(callable: ScriptValue) -> Self {
        Self {
            callable,
            queue: Mutex::new(VecDeque::new()),
            waker: RwLock::new(None),
        }
    }

    fn next(&self) -> Result<Option<Option<ScriptValue>>, ScriptError> {
        let mut queue = self.queue.lock().map_err(ScriptError::panic)?;
        Ok(queue.pop_front())
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

impl Readable for Actor {
    fn read(
        &self,
        ctx: &mut Context,
        interpreter: &Interpreter,
    ) -> Poll<Result<Option<ScriptValue>, ScriptError>> {
        if let Some(value) = self.next()? {
            if let Some(value) = value {
                let args = Tuple::new(vec![TupleItem::unnamed(value.clone())]);
                let val = interpreter.eval_callable(self.callable.clone(), &args)?;
                Poll::Ready(Ok(Some(val)))
            } else {
                Poll::Ready(Ok(None))
            }
        } else {
            let mut waker = self.waker.write().map_err(ScriptError::panic)?;
            *waker = Some(ctx.waker().clone());
            Poll::Pending
        }
    }
}

impl Writable for Actor {
    fn write(
        &self,
        _: &mut Context,
        _: &Interpreter,
        value: ScriptValue,
    ) -> Poll<Result<(), ScriptError>> {
        let mut queue = self.queue.lock().map_err(ScriptError::panic)?;
        queue.push_back(Some(value.clone()));
        if let Some(waker) = self.waker.read().map_err(ScriptError::panic)?.as_ref() {
            waker.wake_by_ref();
        };
        Poll::Ready(Ok(()))
    }

    fn close(&self, _: &mut Context) -> Poll<Result<(), ScriptError>> {
        let mut queue = self.queue.lock().map_err(ScriptError::panic)?;
        queue.push_back(None);
        if let Some(waker) = self.waker.read().map_err(ScriptError::panic)?.as_ref() {
            waker.wake_by_ref();
        };
        Poll::Ready(Ok(()))
    }
}

#[derive(Clone, Default)]
pub(crate) struct Tracker {
    tasks: Arc<Mutex<Vec<Task>>>,
}

impl Tracker {
    pub(crate) fn track(&self, task: Task) {
        let mut tasks = self.tasks.lock().unwrap();
        tasks.push(task);
    }

    pub(crate) fn wait_all(&self) -> Vec<ScriptError> {
        let mut errors = Vec::new();
        smol::block_on(async {
            while let Some(mut task) = self.pop() {
                if let Err(err) = poll_fn(|cx| task.poll(cx)).await {
                    errors.push(err);
                }
            }
        });
        errors
    }

    fn pop(&self) -> Option<Task> {
        let mut tasks = self.tasks.lock().unwrap();
        tasks.pop()
    }
}
