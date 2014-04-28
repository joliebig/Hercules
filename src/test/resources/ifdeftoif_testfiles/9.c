int nr_cpu_ids = 9;


/* verify cpu argument to cpumask_* operators */
static 
 unsigned int cpumask_check(unsigned int cpu)
{
	({ 
		//static bool __attribute__ ((__section__(".data.unlikely"))) __warned; 
		int __ret_warn_once = !!(cpu >= 
#if (definedEx(CONFIG_CPUMASK_OFFSTACK))

	#if (!definedEx(CONFIG_SMP) && definedEx(CONFIG_LOCKDEP))
	1
	#endif
	#if (definedEx(CONFIG_SMP) || !definedEx(CONFIG_LOCKDEP))
	nr_cpu_ids
	#endif

#endif
#if (!definedEx(CONFIG_CPUMASK_OFFSTACK))

	#if definedEx(CONFIG_SMP)
	8
	#endif
	#if !definedEx(CONFIG_SMP)
	1
	#endif

#endif
); 
/*
if (__builtin_expect(!!(__ret_warn_once), 0)) if (
#if (definedEx(CONFIG_BUG) && definedEx(CONFIG_LOCKDEP))
({ int __ret_warn_on = !!(!__warned); if (__builtin_expect(!!(__ret_warn_on), 0)) warn_slowpath_null("/local/TypeChef-Linux34-Analysis/TypeChef-LinuxAnalysis/linux-3.4/include/linux/cpumask.h", 108); __builtin_expect(!!(__ret_warn_on), 0); })
#endif
#if (!definedEx(CONFIG_BUG) && definedEx(CONFIG_LOCKDEP))
({ int __ret_warn_on = !!(!__warned); __builtin_expect(!!(__ret_warn_on), 0); })
#endif
) __warned = true; __builtin_expect(!!(__ret_warn_once), 0); 
*/

});

	return cpu;
}




