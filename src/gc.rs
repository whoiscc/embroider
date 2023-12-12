use std::ptr::null_mut;

use crate::value::{ValueType, ValueTypeExt};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Addr(*mut Object);

unsafe impl Send for Addr {}
unsafe impl Sync for Addr {}

// both underlying (Rust) type and lifetime are dynamical
type Dyn = Box<dyn ValueTypeExt>;

struct Object {
    content: Dyn,
    #[allow(unused)]
    link: *mut Object,
}

#[derive(Debug)]
pub struct Allocator {
    link: *mut Object,
}

unsafe impl Send for Allocator {}

impl Default for Allocator {
    fn default() -> Self {
        Self { link: null_mut() }
    }
}

impl Allocator {
    pub fn alloc(&mut self, content: impl ValueType) -> Addr {
        let object = Box::new(Object {
            content: Box::new(content),
            link: self.link,
        });
        let addr = Box::leak(object) as _;
        self.link = addr;
        Addr(addr)
    }
}

impl Addr {
    pub fn access_shared(&self) -> &dyn ValueTypeExt {
        &*unsafe { &*self.0 }.content
    }

    pub fn access_exclusive(&mut self) -> &mut dyn ValueTypeExt {
        &mut *unsafe { &mut *self.0 }.content
    }
}
