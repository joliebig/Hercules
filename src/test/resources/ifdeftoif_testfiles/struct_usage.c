#if definedEx(CONFIG_X86_32)
 struct pt_regs {
    unsigned long ss;
};
#endif
#if !definedEx(CONFIG_X86_32)
 struct pt_regs {
    unsigned long ss;
};
#endif
static unsigned long regs_get_register()
{
    return __builtin_expect((1 > (__builtin_offsetof(struct pt_regs,ss))), 0);
}