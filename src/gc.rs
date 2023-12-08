use std::{any::Any, ptr::null_mut};

use crate::eval::Dyn;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Addr(*mut Object);

unsafe impl Send for Addr {}
unsafe impl Sync for Addr {}

#[derive(Debug)]
struct Object {
    content: Dyn,
    link: *mut Object,
}

#[derive(Debug)]
pub struct Allocator {
    link: *mut Object,
}

impl Default for Allocator {
    fn default() -> Self {
        Self { link: null_mut() }
    }
}

impl Allocator {
    pub fn alloc(&mut self, content: impl Any + Send + Sync + 'static) -> Addr {
        let object = Box::new(Object {
            content: Box::new(content),
            link: self.link,
        });
        let addr = Box::leak(object) as _;
        self.link = addr;
        Addr(addr)
    }
}

impl AsRef<Dyn> for Addr {
    fn as_ref(&self) -> &Dyn {
        &unsafe { &*self.0 }.content
    }
}

impl AsMut<Dyn> for Addr {
    fn as_mut(&mut self) -> &mut Dyn {
        &mut unsafe { &mut *self.0 }.content
    }
}
