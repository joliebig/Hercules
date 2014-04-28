//struct pt_regs;
#if definedEx(CONFIG_LOCKDEP)
#if definedEx(CONFIG_X86_32)
struct pt_regs {unsigned long bx;}; // DEF LOCK & 32
#endif
#if !definedEx(CONFIG_X86_32)
struct pt_regs {unsigned long bx;}; // DEF LOCK & !32
#endif
struct kernel_vm86_regs {struct pt_regs pt;}; // USE LOCK
struct pt_regs; // REFWDD LOCK
struct pt_regs; // REFWDD LOCK
struct pt_regs; // REFWDD LOCK
#endif

#if !definedEx(CONFIG_LOCKDEP)
#if definedEx(CONFIG_X86_32)
struct pt_regs {unsigned long bx;}; // DEF !LOCK & 32
#endif
#if !definedEx(CONFIG_X86_32)
struct pt_regs {unsigned long bx;}; // DEF !LOCK & !32
#endif
struct kernel_vm86_regs {struct pt_regs pt;}; // USE !LOCK
struct pt_regs; // REFWDD !LOCK
struct pt_regs; // REFWDD !LOCK
#endif

#if definedEx(CONFIG_X86_32)
//332800
struct pt_regs; // REFWDD !LOCK & 32
#endif

//335381
struct pt_regs; // REFWDD

void main() {
	struct pt_regs instance;
}