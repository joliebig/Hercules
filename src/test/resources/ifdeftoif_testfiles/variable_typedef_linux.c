typedef unsigned long long u64;
typedef unsigned int u32;

#if (defined CONFIG_PHYS_ADDR_T_64BIT)
typedef u64 phys_addr_t;

#elif !(defined CONFIG_PHYS_ADDR_T_64BIT)
typedef u32 phys_addr_t;

#endif

static 
//#if !(defined CONFIG_OPTIMIZE_INLINING)
//inline__attribute__((always_inline))
//#elif (defined CONFIG_OPTIMIZE_INLINING)
//inline__attribute__((no_instrument_function))
//#endif
 phys_addr_t get_max_mapped(void)
{
	phys_addr_t a;
	return a;
}