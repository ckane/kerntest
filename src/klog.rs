use crate::GKARG;
use core::fmt::Write;
use log::Log;

pub struct KernLogger;

impl Log for KernLogger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, rec: &log::Record) {
        if !self.enabled(rec.metadata()) {
            return;
        }

        unsafe { write!((*GKARG).get_fb(), "[{}]", rec.level()) }.unwrap();
        if let Some(f) = rec.file() {
            unsafe { write!((*GKARG).get_fb(), "({}:{}) ", f, rec.line().unwrap_or(0)) }.unwrap();
        };
        unsafe { writeln!((*GKARG).get_fb(), "{}", rec.args()) }.unwrap();
    }

    fn flush(&self) {}
}
