#if definedEx(CONFIG_X86_32)

 struct pt_regs {
	unsigned long ss;
};
#endif
#if !definedEx(CONFIG_X86_32)

 struct pt_regs {
	unsigned long ss;
/* top of stack page */
};
#endif

 unsigned long regs_get_register(struct pt_regs *regs,
					      unsigned int offset)
{
	__builtin_offsetof(struct pt_regs,ss);
	
	
	
	if (__builtin_expect(!!(offset > (__builtin_offsetof(struct pt_regs,ss))), 0))
		return 0;
	return 1;
}
