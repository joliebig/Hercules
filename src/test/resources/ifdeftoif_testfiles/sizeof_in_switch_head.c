struct task_struct;
#if definedEx(CONFIG_X86_32)
extern  __typeof__(struct task_struct *) current_task;
#endif
#if !definedEx(CONFIG_X86_32)
extern  __typeof__(struct task_struct *) current_task;
#endif

static inline struct task_struct *get_current(void)
{
	return ({ switch (sizeof(current_task)) { default: 1; } 0; });
}


// current_task in sizeof should be duplicated