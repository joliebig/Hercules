
#if definedEx(CONFIG_X86_32)
typedef unsigned int	__kernel_size_t;
#endif
#if !definedEx(CONFIG_X86_32)
typedef unsigned long	__kernel_ulong_t;
typedef __kernel_ulong_t __kernel_size_t;
#endif

typedef __kernel_size_t		size_t;

extern void *memdup_user(const void  *, size_t);
extern char *strncpy(char *dest, const char *src, size_t count);
