
#if definedEx(CONFIG_LOCKDEP)

typedef struct cpumask { unsigned long bits[(((
#if definedEx(CONFIG_SMP)
8
#endif
#if !definedEx(CONFIG_SMP)
1
#endif
) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))]; } cpumask_t;

#endif

#if !definedEx(CONFIG_LOCKDEP)

typedef struct cpumask { unsigned long bits[(((
#if definedEx(CONFIG_SMP)
8
#endif
#if !definedEx(CONFIG_SMP)
1
#endif
) + (8 * sizeof(long)) - 1) / (8 * sizeof(long)))]; } cpumask_t;

#endif
