#ifdef CONFIG_X86_32
static inline int default_cpu_present_to_apicid(int mps_cpu)
{
	return 0;
}

static inline int
default_check_phys_apicid_present(int phys_apicid)
{
	return 0;
}
#else
extern int default_cpu_present_to_apicid(int mps_cpu);
extern int default_check_phys_apicid_present(int phys_apicid);
#endif
