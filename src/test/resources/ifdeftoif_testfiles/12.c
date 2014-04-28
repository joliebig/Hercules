struct mm_struct;
typedef unsigned long long u64;

typedef u64 phys_addr_t;
typedef u64	pgprotval_t;
typedef struct pgprot { pgprotval_t pgprot; } pgprot_t;

typedef u64	pteval_t;
typedef u64	pmdval_t;
typedef u64	pudval_t;
typedef u64	pgdval_t;
typedef u64	pgprotval_t;
typedef union {
	struct {
		unsigned long pte_low, pte_high;
	};
	pteval_t pte;
} pte_t;

typedef struct { pudval_t pud; } pud_t;
typedef struct { pud_t pud; } pmd_t;

typedef struct { pgdval_t pgd; } pgd_t;

struct paravirt_callee_save {
	void *func;
};
struct pv_lazy_ops {
	/* Set deferred update mode, used for batching operations. */
	void (*enter)(void);
	void (*leave)(void);
};
#if definedEx(CONFIG_X86_LOCAL_APIC)
struct pv_mmu_ops {
	void (*set_pmd_at)(struct mm_struct *mm, unsigned long addr,
			   pmd_t *pmdp, pmd_t pmdval);
};

extern struct pv_mmu_ops pv_mmu_ops;

#endif

void native_pmd_val(pmd_t t);
void test();
int x;
#if definedEx(CONFIG_X86_LOCAL_APIC)

static void set_pmd_at(struct mm_struct *mm, unsigned long addr,
			      pmd_t *pmdp, pmd_t pmd)
{
	if (sizeof(pmdval_t) > sizeof(long))
		/* 5 arg words */
		pv_mmu_ops.set_pmd_at(mm, addr, pmdp, pmd);
	else
		
#if (definedEx(CONFIG_PARAVIRT) && definedEx(CONFIG_LOCKDEP) && definedEx(CONFIG_X86_32))
({
asm volatile("push %[mm];" "771:\n\t" "call *%c[paravirt_opptr];" "\n" "772:\n" ".pushsection .parainstructions,\"a\"\n" " " ".balign 4" " " "\n" " " ".long" " " " 771b\n" "  .byte " "%c[paravirt_typenum]" "\n" "  .byte 772b-771b\n" "  .short " "%c[paravirt_clobber]" "\n" ".popsection\n" "lea 4(%%esp),%%esp;" : "=a" (test), "=d" (test), "=c" (test) : [mm] "i" ((__builtin_offsetof(struct paravirt_patch_template,pv_mmu_ops.set_pmd_at) / sizeof(void *))), [mm] "i" (&(pv_mmu_ops.set_pmd_at)),"0"((u64)(mm)), "1"((u64)(addr)), "2"((u64)(pmdp))); })
#endif
;
}

#endif
