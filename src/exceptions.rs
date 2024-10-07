use alloc::vec::Vec;
use core::arch::asm;
use crate::interrupts::{InterruptDescriptorEntry, InterruptStack};
use log::error;

extern "x86-interrupt"
fn div0_trap(frame: InterruptStack) {
    panic!("Division by zero or overflow @ {:?}", frame);
}

extern "x86-interrupt"
fn dbg_trap(frame: InterruptStack) {
    panic!("Debug trap @ {:?}", frame);
}

extern "x86-interrupt"
fn nmi_int(frame: InterruptStack) {
    error!("NMI @ {:?}", frame);
}

extern "x86-interrupt"
fn bp_trap(frame: InterruptStack) {
    error!("Breakpoint @ {:?}", frame);
    // This "fault" is recoverable
}

extern "x86-interrupt"
fn of_trap(frame: InterruptStack) {
    error!("Overflow @ {:?}", frame);
    // This "fault" is recoverable
}

extern "x86-interrupt"
fn bre_panic(frame: InterruptStack) {
    panic!("Bound Range Exceeded @ {:?}", frame)
}

extern "x86-interrupt"
fn invop_panic(frame: InterruptStack) {
    panic!("Invalid opcode @ {:?}", frame)
}

extern "x86-interrupt"
fn dna_panic(frame: InterruptStack) {
    panic!("Device not available @ {:?}", frame)
}

extern "x86-interrupt"
fn df_panic(frame: InterruptStack, _error_code: u64) {
    panic!("Double fault @ {:?}", frame)
}

extern "x86-interrupt"
fn cso_panic(frame: InterruptStack) {
    panic!("Coprocessor segment overrun (SHOULD NOT HAPPEN) @ {:?}", frame)
}

extern "x86-interrupt"
fn tss_panic(frame: InterruptStack, error_code: u64) {
    panic!("Invalid TSS (IST on x64), error: {:#018x} @ {:?}", error_code, frame)
}

extern "x86-interrupt"
fn segpres_panic(frame: InterruptStack, error_code: u64) {
    panic!("Segment not Present, error: {:#018x} @ {:?}", error_code, frame)
}

extern "x86-interrupt"
fn stkseg_panic(frame: InterruptStack, error_code: u64) {
    panic!("Stack Segment Fault, error: {:#018x} @ {:?}", error_code, frame)
}

extern "x86-interrupt"
fn gpf_panic(frame: InterruptStack, error_code: u64) {
    let (mut a, mut c) = (0, 0);
    unsafe { asm!(
        "mov {},rax",
        "mov {},rcx",
        out(reg) a,
        out(reg) c,
    ) };
    panic!("General Protection Fault, error: {:#018x} @ {:?}. RAX: {:#018x}, RCX: {:#018x}", error_code, frame, a, c)
}

extern "x86-interrupt"
fn pf_panic(frame: InterruptStack, error_code: u64) {
    panic!("Page Fault, error: {:#018x} @ {:?}", error_code, frame)
}

extern "x86-interrupt"
fn fpe_panic(frame: InterruptStack) {
    panic!("x87 Floating Point Exception @ {:?}", frame)
}

extern "x86-interrupt"
fn ac_panic(frame: InterruptStack, error_code: u64) {
    panic!("Alignment Check, error: {:#018x} @ {:?}", error_code, frame)
}

extern "x86-interrupt"
fn mce_panic(frame: InterruptStack) {
    panic!("Machine Check Exception @ {:?}", frame)
}

extern "x86-interrupt"
fn simd_panic(frame: InterruptStack) {
    panic!("SIMD Floating Point Exception @ {:?}", frame)
}

extern "x86-interrupt"
fn virt_panic(frame: InterruptStack) {
    panic!("Virtualization Exception @ {:?}", frame)
}

extern "x86-interrupt"
fn cpe_panic(frame: InterruptStack, error_code: u64) {
    panic!("Control Protection Exception, error: {:#018x} @ {:?}", error_code, frame)
}

extern "x86-interrupt"
fn hypinj_panic(frame: InterruptStack) {
    panic!("Hypervisor Injection Exception @ {:?}", frame)
}

extern "x86-interrupt"
fn vmm_panic(frame: InterruptStack, error_code: u64) {
    panic!("VMM Communications Exception, error: {:#018x} @ {:?}", error_code, frame)
}

extern "x86-interrupt"
fn sec_panic(frame: InterruptStack, error_code: u64) {
    panic!("Security Exception, error: {:#018x} @ {:?}", error_code, frame)
}

pub fn attach_exceptions(idt: &mut Vec<InterruptDescriptorEntry>) {
    // Attach divide-by-zero handler
    idt[0] = InterruptDescriptorEntry::new_trap(div0_trap, 0x10, 0, 2);

    // Attach debug handler
    idt[1] = InterruptDescriptorEntry::new_trap(dbg_trap, 0x10, 0, 2);

    // Attach NMI handler
    idt[2] = InterruptDescriptorEntry::new_int(nmi_int, 0x10, 0, 3);

    // Attach breakpoint handler
    idt[3] = InterruptDescriptorEntry::new_trap(bp_trap, 0x10, 0, 2);

    // Attach overflow handler
    idt[4] = InterruptDescriptorEntry::new_trap(of_trap, 0x10, 0, 2);

    // Attach bound-range exceeded handler
    idt[5] = InterruptDescriptorEntry::new_trap(bre_panic, 0x10, 0, 2);

    // Attach invalid opcode handler
    idt[6] = InterruptDescriptorEntry::new_trap(invop_panic, 0x10, 0, 2);

    // Attach device not available handler
    idt[7] = InterruptDescriptorEntry::new_trap(dna_panic, 0x10, 0, 2);

    // Attach double-fault handler
    idt[8] = InterruptDescriptorEntry::new_trap_error_code(df_panic, 0x10, 0, 1);

    // Attach coprocessor segment overrun handler
    idt[9] = InterruptDescriptorEntry::new_trap(cso_panic, 0x10, 0, 2);

    // Attach coprocessor segment overrun handler
    idt[9] = InterruptDescriptorEntry::new_trap(cso_panic, 0x10, 0, 2);

    // Attach invalid TSS handler
    idt[10] = InterruptDescriptorEntry::new_trap_error_code(tss_panic, 0x10, 0, 2);

    // Attach Segment not Present handler
    idt[11] = InterruptDescriptorEntry::new_trap_error_code(segpres_panic, 0x10, 0, 2);

    // Attach Stack Segment Fault handler
    idt[12] = InterruptDescriptorEntry::new_trap_error_code(stkseg_panic, 0x10, 0, 2);

    // Attach General Protection Fault handler
    idt[13] = InterruptDescriptorEntry::new_trap_error_code(gpf_panic, 0x10, 0, 2);

    // Attach Page Fault handler
    idt[14] = InterruptDescriptorEntry::new_trap_error_code(pf_panic, 0x10, 0, 2);

    // Attach x87 FP Exception handler
    idt[16] = InterruptDescriptorEntry::new_trap(fpe_panic, 0x10, 0, 2);

    // Attach Alignment Check handler
    idt[17] = InterruptDescriptorEntry::new_trap_error_code(ac_panic, 0x10, 0, 2);

    // Attach Machine Check Exception handler
    idt[18] = InterruptDescriptorEntry::new_trap(mce_panic, 0x10, 0, 2);

    // Attach SIMD FP Exception handler
    idt[19] = InterruptDescriptorEntry::new_trap(simd_panic, 0x10, 0, 2);

    // Attach Virtualization Exception handler
    idt[20] = InterruptDescriptorEntry::new_trap(virt_panic, 0x10, 0, 2);

    // Attach Control Protection Exception handler
    idt[21] = InterruptDescriptorEntry::new_trap_error_code(cpe_panic, 0x10, 0, 2);

    // Attach Hypervisor Injection Exception handler
    idt[28] = InterruptDescriptorEntry::new_trap(hypinj_panic, 0x10, 0, 2);

    // Attach VMM Communications Exception handler
    idt[29] = InterruptDescriptorEntry::new_trap_error_code(vmm_panic, 0x10, 0, 2);

    // Attach Security Exception handler
    idt[30] = InterruptDescriptorEntry::new_trap_error_code(sec_panic, 0x10, 0, 2);
}
