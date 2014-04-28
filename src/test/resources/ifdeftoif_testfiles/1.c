typedef signed int s32;
typedef unsigned int u32;

typedef signed long long s64;
typedef unsigned long long u64;

#if definedEx(CONFIG_PHYS_ADDR_T_64BIT)
typedef u64 phys_addr_t;
#endif
#if !definedEx(CONFIG_PHYS_ADDR_T_64BIT)
typedef u32 phys_addr_t;
#endif
typedef phys_addr_t resource_size_t;

extern int __check_region(struct  resource   * , resource_size_t , resource_size_t );
extern void __release_region(struct  resource   * , resource_size_t , resource_size_t );
