typedef unsigned long long u64;

int main() {}

#if defined(CONFIG_LOCKDEP)

static 
#if !defined(CONFIG_OPTIMIZE_INLINING)
inline __attribute__((always_inline))
#endif
#if defined(CONFIG_OPTIMIZE_INLINING)
inline __attribute__((no_instrument_function))
#endif
 unsigned long long native_read_msr_safe(unsigned int msr,
						      int *err)
{
#if (defined(CONFIG_X86_64) && defined(CONFIG_LOCKDEP))
unsigned low, high;
#endif
#if (!defined(CONFIG_X86_64) && defined(CONFIG_LOCKDEP))
unsigned long long val;
#endif


	asm volatile("2: rdmsr ; xor %[err],%[err]\n"
		     "1:\n\t"
		     ".section .fixup,\"ax\"\n\t"
		     "3:  mov %[fault],%[err] ; jmp 1b\n\t"
		     ".previous\n\t"
		     " .section __ex_table,\"a\"\n" 
#if defined(CONFIG_X86_32)
" " ".balign 4" " "
#endif
#if !defined(CONFIG_X86_32)
" " ".balign 8" " "
#endif
 "\n" 
#if defined(CONFIG_X86_32)
" " ".long" " "
#endif
#if !defined(CONFIG_X86_32)
" " ".quad" " "
#endif
 "2b" "," "3b" "\n" " .previous\n"
		     : [err] "=r" (*err), 
#if (defined(CONFIG_X86_64) && defined(CONFIG_LOCKDEP))
"=a" (low), "=d" (high)
#endif
#if (!defined(CONFIG_X86_64) && defined(CONFIG_LOCKDEP))
"=A" (val)
#endif

		     : "c" (msr), [fault] "i" (-5));
	return 
#if (defined(CONFIG_X86_64) && defined(CONFIG_LOCKDEP))
((low) | ((u64)(high) << 32))
#endif
#if (!defined(CONFIG_X86_64) && defined(CONFIG_LOCKDEP))
(val)
#endif
;
}

#endif
