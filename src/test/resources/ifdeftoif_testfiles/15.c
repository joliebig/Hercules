//#define CONFIG_LOCKDEP
//#define CONFIG_PARAVIRT

#if defined(CONFIG_LOCKDEP) && defined(CONFIG_PARAVIRT)

struct paravirt_callee_save {
	void *func;
};
#endif

#if (defined(CONFIG_LOCKDEP) && defined(CONFIG_PARAVIRT) && (!defined(CONFIG_PARAVIRT)  &&  !defined(CONFIG_LOCKDEP) || defined(CONFIG_X86_PAE) || !defined(CONFIG_X86_32)))
struct pv_mmu_ops {
	struct paravirt_callee_save pmd_val;
	struct paravirt_callee_save make_pmd;
};
#endif


#if !defined(CONFIG_LOCKDEP) && defined(CONFIG_PARAVIRT)

struct paravirt_callee_save {
	void *func;
};
#endif

#if (!defined(CONFIG_LOCKDEP) && defined(CONFIG_PARAVIRT) && (defined(CONFIG_X86_PAE) || (!defined(CONFIG_PARAVIRT) && !defined(CONFIG_LOCKDEP)) || !defined(CONFIG_X86_32)))
struct pv_mmu_ops {
	struct paravirt_callee_save pmd_val;
	struct paravirt_callee_save make_pmd;
};
#endif
