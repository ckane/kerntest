use core::sync::atomic::{AtomicUsize, Ordering};
use core::cell::UnsafeCell;
use core::hint::spin_loop;
use core::ops::{Deref, DerefMut};
use snafu::prelude::*;

unsafe impl<T: Send> Send for KernSpinMutex<T> {}
unsafe impl<T: Send> Sync for KernSpinMutex<T> {}

#[derive(Debug)]
/// Spinlock implementation adapted from https://whenderson.dev/blog/rust-mutexes/
pub struct KernSpinMutex<T> {
    inner: UnsafeCell<T>,
    status: AtomicUsize,
}

pub struct KernSpinMutexGuard<'a, T> {
    mutex: &'a KernSpinMutex<T>
}

#[derive(Debug, Snafu)]
pub enum Error {
    /// Mutex is poisoned
    Poisoned,
}

type Result<T> = core::result::Result<T, Error>;

impl<T> KernSpinMutex<T> {
    pub const fn new(inner: T) -> Self {
        Self {
            inner: UnsafeCell::new(inner),
            status: AtomicUsize::new(0),
        }
    }

    pub fn lock(&self) -> Result<KernSpinMutexGuard<T>> {
        loop {
            match self.status.compare_exchange_weak(0, 1, Ordering::Acquire, Ordering::Relaxed) {
                Ok(_) => break,
                Err(2) => Err(Error::Poisoned)?,
                Err(_) => {},
            }
            spin_loop();
        };
        Ok(KernSpinMutexGuard { mutex: self })
    }
}

impl<T> Deref for KernSpinMutexGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.mutex.inner.get() }
    }
}

impl<T> DerefMut for KernSpinMutexGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.mutex.inner.get() }
    }
}

impl<T> Drop for KernSpinMutexGuard<'_, T> {
    fn drop(&mut self) {
        self.mutex.status.store(0, Ordering::Release);
    }
}
