typedef _Bool			bool;
typedef unsigned short u16;

/* general info */
struct pv_info {
	unsigned int kernel_rpl;
	int shared_kernel_pmd;

#if definedEx(CONFIG_X86_64)
	u16 extra_user_64bit_cs;  /* __USER_CS if none */
#endif
	int paravirt_enabled;
	const char *name;
};
extern struct pv_info pv_info;

struct pt_regs;

#if (defined(CONFIG_LOCKDEP) && defined(CONFIG_X86_32))
struct pt_regs {
	unsigned long cs;
	unsigned long flags;
};
#endif
#if (defined(CONFIG_LOCKDEP) && !defined(CONFIG_X86_32))
 struct pt_regs {
	unsigned long cs;
	unsigned long flags;
};
#endif

#if defined(CONFIG_LOCKDEP)


#if definedEx(CONFIG_X86_64)
static 
#if !definedEx(CONFIG_OPTIMIZE_INLINING)
inline __attribute__((always_inline))
#endif
#if definedEx(CONFIG_OPTIMIZE_INLINING)
inline __attribute__((no_instrument_function))
#endif
 bool user_64bit_mode(struct pt_regs *regs)
{
#if !definedEx(CONFIG_PARAVIRT)
	 /*
	  * On non-paravirt systems, this is the only long mode CPL 3
	  * selector.  We do not allow long mode selectors in the LDT.
	  */
	 return regs->cs == (
#if (definedEx(CONFIG_X86_32) && definedEx(CONFIG_LOCKDEP))
14
#endif
#if (!definedEx(CONFIG_X86_32) && definedEx(CONFIG_LOCKDEP))
6
#endif
*8+3);
#endif
#if definedEx(CONFIG_PARAVIRT)
	 /* Headers are too twisted for this to go in paravirt.h. */
	 return regs->cs == (
#if (definedEx(CONFIG_X86_32) && definedEx(CONFIG_LOCKDEP))
14
#endif
#if (!definedEx(CONFIG_X86_32) && definedEx(CONFIG_LOCKDEP))
6
#endif
*8+3) || regs->cs == pv_info.extra_user_64bit_cs;
#endif
}
#endif

#endif
void main() {
}
