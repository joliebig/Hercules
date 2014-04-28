#if definedEx(CONFIG_X86_32)
typedef unsigned int	__kernel_size_t;
#endif
 #if !definedEx(CONFIG_X86_32)
typedef int	__kernel_size_t;
#endif
extern void *memdup_user(const void  *, __kernel_size_t);


// __kernel_size_t in extern func def should be duplicated