use log::Log;
use crate::GKARG;
use core::fmt::Write;

pub struct KernLogger;

impl Log for KernLogger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, rec: &log::Record) {
        if !self.enabled(rec.metadata()) {
            return;
        }

        unsafe {
            write!(
                (*GKARG).get_fb(),
                "[{}]",
                rec.level())
        }.unwrap();
        if let Some(f) = rec.file() {
            unsafe {
                write!(
                    (*GKARG).get_fb(),
                    "({}:{}) ",
                    f,
                    rec.line().unwrap_or(0))
            }.unwrap();
        };
        unsafe {
            write!(
                (*GKARG).get_fb(),
                "{}\n",
                rec.args())
        }.unwrap();
    }

    fn flush(&self) {}
}
