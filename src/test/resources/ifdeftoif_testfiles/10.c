
#if definedEx(CONFIG_GENERIC_BUG_RELATIVE_POINTERS)

struct bug_entry {
	unsigned long	bug_addr;
};

#endif

#if !definedEx(CONFIG_GENERIC_BUG_RELATIVE_POINTERS)

struct bug_entry {
	unsigned long	bug_addr;
};

#endif

static void foo() {
	asm volatile ("bla" "bla" "bla" "bla" "bla"::"i"("irgendein Pfad"), "i"(25), "i"(sizeof(struct  bug_entry   )));
}
