use std::{
    io,
    sync::{Arc, RwLock},
};

#[derive(Clone)]
pub(crate) struct OutputAdapter(Arc<RwLock<String>>);

impl OutputAdapter {
    pub fn new() -> Self {
        Self(Arc::new(RwLock::new(String::new())))
    }

    pub fn clear(&self) {
        self.0.write().unwrap().clear();
    }

    pub fn output(&self) -> String {
        self.0.read().unwrap().clone()
    }
}

impl io::Write for OutputAdapter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut s = self.0.write().unwrap();
        s.push_str(str::from_utf8(buf).unwrap());
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
