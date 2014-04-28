
struct task_struct;
#if (definedEx(CONFIG_LOCKDEP))

#if (definedEx(CONFIG_LOCKDEP) && definedEx(CONFIG_DEBUG_FORCE_WEAK_PER_CPU))
extern __attribute__((section(".discard"), unused)) char __pcpu_scope_current_task; extern  __attribute__((section(
#if (definedEx(CONFIG_SMP) && definedEx(CONFIG_LOCKDEP))
".data..percpu"
#endif
#if (!definedEx(CONFIG_SMP) && definedEx(CONFIG_LOCKDEP))
".data"
#endif
 "")))  __typeof__(struct task_struct *) current_task
#endif
#if (definedEx(CONFIG_LOCKDEP) && !definedEx(CONFIG_DEBUG_FORCE_WEAK_PER_CPU))
extern  __attribute__((section(
#if (definedEx(CONFIG_SMP) && definedEx(CONFIG_LOCKDEP))
".data..percpu"
#endif
#if (!definedEx(CONFIG_SMP) && definedEx(CONFIG_LOCKDEP))
".data"
#endif
 "")))  __typeof__(struct task_struct *) current_task
#endif
;
static void foo () {
	
	typeof(current_task) pfo_ret__;
}
#endif
