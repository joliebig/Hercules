
struct task_struct;
#if (!definedEx(CONFIG_LOCKDEP))
int x=1;
#endif
#if (definedEx(CONFIG_LOCKDEP))
int x=0;
#endif

static int foo () {
 // ternary operator
	return ({x++; x;});
}

