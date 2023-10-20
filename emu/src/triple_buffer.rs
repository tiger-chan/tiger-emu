use std::sync::{Arc, RwLock};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
struct BufferIndex {
    pub fwd: usize,
    pub bck: usize,
}

impl Default for BufferIndex {
    fn default() -> Self {
        Self { bck: 1, fwd: 0 }
    }
}

pub struct TripleBuffer<Content> {
    buf: [Arc<RwLock<Content>>; 3],
    idx: Arc<RwLock<BufferIndex>>,
}

#[allow(unused)]
impl<Content> TripleBuffer<Content> {
    pub fn new(buf: [Arc<RwLock<Content>>; 3]) -> Self {
        Self {
            buf,
            idx: Arc::default(),
        }
    }

    pub fn submit(&mut self) {
        let mut idx = self.idx.write().unwrap();
        idx.bck = 3 - idx.fwd - idx.bck;
    }

    pub fn flip(&mut self) {
        let mut idx = self.idx.write().unwrap();
        idx.fwd = 3 - idx.fwd - idx.bck;
    }

    #[allow(unused)]
    pub fn back(&self) -> &Arc<RwLock<Content>> {
        &self.buf[self.idx.read().unwrap().bck]
    }

    pub fn back_mut(&mut self) -> &mut Arc<RwLock<Content>> {
        &mut self.buf[self.idx.read().unwrap().bck]
    }

    pub fn front(&self) -> &Arc<RwLock<Content>> {
        &self.buf[self.idx.read().unwrap().fwd]
    }

    #[allow(unused)]
    pub fn front_mut(&mut self) -> &mut Arc<RwLock<Content>> {
        &mut self.buf[self.idx.read().unwrap().fwd]
    }
}

impl<Content> Default for TripleBuffer<Content>
where
    Content: Default,
{
    fn default() -> Self {
        Self {
            buf: [Arc::default(), Arc::default(), Arc::default()],
            idx: Arc::default(),
        }
    }
}

impl<Content> Clone for TripleBuffer<Content> {
    fn clone(&self) -> Self {
        Self {
            buf: self.buf.clone(),
            idx: self.idx.clone(),
        }
    }
}
