#if definedEx(CONFIG_X86_32)
typedef unsigned long	__kernel_ulong_t;
#endif
#if !definedEx(CONFIG_X86_32)
typedef unsigned long	__kernel_ulong_t;
#endif

int i = 20 * sizeof(__kernel_ulong_t);

char e[sizeof(__kernel_ulong_t)];