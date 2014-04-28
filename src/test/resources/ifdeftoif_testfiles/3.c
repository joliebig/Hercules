static 
#if !definedEx(CONFIG_OPTIMIZE_INLINING)
inline __attribute__((always_inline))
#endif
#if definedEx(CONFIG_OPTIMIZE_INLINING)
inline __attribute__((no_instrument_function))
#endif
 __attribute__((always_inline)) int x86_this_cpu_constant_test_bit(unsigned int nr,
                        const unsigned long  *addr)
{
	unsigned long  *a = (unsigned long *)addr + nr / 
#if definedEx(CONFIG_64BIT)
64
#endif
#if !definedEx(CONFIG_64BIT)
32
#endif
;
return 1;
}
