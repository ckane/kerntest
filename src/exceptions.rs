use alloc::collections::BTreeSet;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::arch::{asm, naked_asm};
use crate::KERNEL;
use crate::driver::DriverBus;
use crate::cpu::{save_context, preempt_thread};
use crate::interrupts::{InterruptDescriptorEntry, InterruptStack};
use log::{error, info};

static mut INT_QUEUE: BTreeSet<&str> = BTreeSet::new();

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

#[naked]
extern "x86-interrupt"
fn timer_int(frame: InterruptStack) {
    unsafe {
        naked_asm!(
         "cli",
         "push r15; push r14; push r13; push r12; push r11; push r10; push r9; push r8",
         "push rdi; push rsi; push rbp; push rbx; push rdx; push rcx; push rax",
         "mov rax, [rsp+0x98]",
         "push rax",
         "mov rax, [rsp+0x98]",
         "push rax",
         "mov rax, [rsp+0x98]",
         "push rax",
         "mov rax, [rsp+0x98]",
         "push rax",
         "mov rax, [rsp+0x98]",
         "push rax",
         "mov rax, [rsp+0x30]",
         "call timer_int_inner",
         "mov rcx, [rax+0x30]",
         "mov rdx, [rax+0x38]",
         "mov rbx, [rax+0x40]",
         "mov rbp, [rax+0x48]",
         "mov rsi, [rax+0x50]",
         "mov rdi, [rax+0x58]",
         "mov r8, [rax+0x60]",
         "mov r9, [rax+0x68]",
         "mov r10, [rax+0x70]",
         "mov r11, [rax+0x78]",
         "mov r12, [rax+0x80]",
         "mov r13, [rax+0x88]",
         "mov r14, [rax+0x90]",
         "mov r15, [rax+0x98]",
         "push qword ptr [rax+0x20]",
         "push qword ptr [rax+0x18]",
         "push qword ptr [rax+0x10]",
         "push qword ptr [rax+0x08]",
         "push qword ptr [rax]",
         "mov rax, [rax+0x28]",
         "iretq",
        );
    };
}

#[no_mangle]
//extern "x86-interrupt"
extern "C"
fn timer_int_inner(context: crate::thread::ThreadContext) -> crate::thread::ThreadContext {
    /*let mut rsp1 = 0u64;
    unsafe { asm!(
        "mov {}, rsp",
        out(reg) rsp1,
    ) };*/
    crate::cpu::stop_ints();
    //info!("Timer (stk: {:#018x}) @ {:?}, Core: {}", rsp1, context, crate::cpu::get_core_id());
    unsafe { INT_QUEUE.insert("timer") };
    save_context(crate::cpu::get_core_id(), context);
    let h = preempt_thread(crate::cpu::get_core_id());
    //info!("New context @ {:?}", h);
    unsafe { KERNEL[0].set("timerint", Arc::new(true)).unwrap() };
    h
    //error!("Timer @ {:?}", frame2);
    // TODO: The thread context needs to be saved here. This means that the associated thread
    //       (which is now interrupted/paused) needs to be discovered and the context that we
    //       have saved needs to be stored in it. At this point, it will still be the "active"
    //       thread on the CPU, so perhaps 
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

    // Attach NMI handler
    idt[32] = InterruptDescriptorEntry::new_int(timer_int, 0x10, 0, 3);
}

/// If the timer int was queued, then return true and clear it from the queue
pub fn timer_queued() -> bool {
    unsafe { INT_QUEUE.remove("timer") }
}
