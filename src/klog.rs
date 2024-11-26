use core::fmt::Write;
use log::Log;

pub struct KernLogger;

static mut LOG_OUTPUT: Option<*mut (dyn Write + Sync)> = None;

impl KernLogger {
    pub fn set_log_output(newlog: *mut (dyn Write + Sync)) {
        unsafe { LOG_OUTPUT = Some(newlog) };
    }
}

impl Log for KernLogger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, rec: &log::Record) {
        if !self.enabled(rec.metadata()) {
            return;
        }
        let _lock = crate::cpu::CritSection::new();
        if let Some(lo) = unsafe { LOG_OUTPUT } {
            if let Some(mm) = unsafe { lo.as_mut() } {
                write!(mm, "[{}]", rec.level()).unwrap();
                if let Some(f) = rec.file() {
                    write!(mm, "({}:{}) ", f, rec.line().unwrap_or(0)).unwrap();
                };
                writeln!(mm, "{}", rec.args()).unwrap();
            }
        }
    }

    fn flush(&self) {}
}
