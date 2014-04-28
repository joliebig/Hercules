#if (definedEx(CONFIG_B))
struct pv_mmu_ops {
#endif
#if (!definedEx(CONFIG_B))
struct pv_mmu_ops {
#endif

#if (definedEx(CONFIG_A))
	void (*leave)(void);
#endif
#if (!definedEx(CONFIG_A))
	void (*leave)(void);
#endif

};

extern struct pv_mmu_ops_a pv_mmu_ops_t;
