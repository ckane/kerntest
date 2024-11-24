use alloc::string::String;
use alloc::sync::Arc;
use core::any::Any;
use linkme::distributed_slice;
use snafu::prelude::*;

/// Array that will enumerate the statically-linked driver entries
#[distributed_slice]
pub static DRIVERS: [DriverEntry];

#[derive(Snafu, Debug)]
pub enum Error {
    /// Initialization error: {reason}
    Initialization { reason: String },

    /// Asset {name} isn't provided
    AssetNotProvided { name: String },

    /// Asset {name} fatal error while fetching
    AssetFatalError { name: String },
}

pub type DriverError = Error;

pub(crate) trait DriverBus {
    fn get(&self, s: &str) -> Result<Arc<dyn Any + Sync + Send>, Error>;
    fn set(&self, s: &str, val: Arc<dyn Any + Sync +Send>) -> core::result::Result<(), crate::driver::Error>;
}

pub(crate) trait Driver {
    fn new(bus: &mut dyn DriverBus) -> Result<alloc::sync::Arc<dyn Driver>, Error> where Self: Sized;
    fn get(&self, s: &str) -> Result<Arc<dyn Any + Send + Sync>, Error>;
    fn set(&self, s: &str, val: Arc<dyn Any + Send + Sync>) -> Result<(), Error>;
}

#[derive(Clone, Debug)]
pub struct DriverEntry {
    pub name: &'static str,
    pub ctor: fn(&mut dyn DriverBus) -> Result<alloc::sync::Arc<dyn Driver>, Error>,
    pub req: &'static [&'static str],
    pub provides: &'static [&'static str],
}
