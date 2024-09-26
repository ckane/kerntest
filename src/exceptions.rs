use alloc::vec::Vec;
use crate::interrupts::{InterruptDescriptorEntry, InterruptStack};
use log::error;

extern "x86-interrupt"
fn bp_trap(frame: InterruptStack) {
    error!("Breakpoint @ {:?}", frame);
    // This "fault" is recoverable
}

extern "x86-interrupt"
fn df_panic(frame: InterruptStack) {
    panic!("Double fault @ {:?}", frame)
}

pub fn attach_exceptions(idt: &mut Vec<InterruptDescriptorEntry>) {
    // Attach brakpoint handler
    idt[3] = InterruptDescriptorEntry::new_trap(bp_trap, 0x10, 0, 2);

    // Attach double-fault handler
    idt[8] = InterruptDescriptorEntry::new_trap(df_panic, 0x10, 0, 1);
}
