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

pub(crate) trait Driver {
    fn new() -> Result<alloc::sync::Arc<dyn Driver>, Error> where Self: Sized;
    fn get(&self, s: &str) -> Result<Arc<dyn Any>, Error>;
}

#[derive(Clone, Debug)]
pub struct DriverEntry {
    pub name: &'static str,
    pub ctor: fn() -> Result<alloc::sync::Arc<dyn Driver>, Error>,
    pub req: &'static [&'static str],
    pub provides: &'static [&'static str],
}
