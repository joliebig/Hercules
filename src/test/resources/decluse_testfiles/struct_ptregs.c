#if definedEx(CONFIG_X86_32)
 struct pt_regs {
    unsigned long ss;
};
#endif
#if !definedEx(CONFIG_X86_32)
 struct pt_regs {
    unsigned long ss;
	unsigned long bb;
	#if !definedEx(CONFIG_TEST)
	unsigned long tt;
	#endif
};
#endif
static unsigned long regs_get_register()
{
	struct pt_regs test;
	test.ss = 0;
	#if definedEx(CONFIG_X86_32)
	test.ss = 1;
	#endif
	#if !definedEx(CONFIG_X86_32)
	test.ss = 2;
	#endif
	#if (!definedEx(CONFIG_X86_32) && !definedEx(CONFIG_TEST))
	test.ss = 3;
	test.bb = 4;
	test.tt = 5;
	#endif
	
    return 0;
}