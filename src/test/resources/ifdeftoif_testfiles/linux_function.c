static 
#if !(defined CONFIG_OPTIMIZE_INLINING)
inline__attribute__((always_inline))
#elif (defined CONFIG_OPTIMIZE_INLINING)
inline__attribute__((no_instrument_function))
#endif
 void arch_acpi_set_pdc_bits(u32 *buf)
{
	struct cpuinfo_x86 *c = &
#if (defined CONFIG_SMP)
(*({do{constvoid*__vpp_verify=(typeof((&(cpu_info))))((void*)0);(void)__vpp_verify;}while(0);({unsignedlong__ptr;__asm__("":"=r"(__ptr):"0"((typeof(*(&(cpu_info)))*)(&(cpu_info))));(typeof((typeof(*(&(cpu_info)))*)(&(cpu_info))))(__ptr+(((__per_cpu_offset[0]))));});}))
#elif !(defined CONFIG_SMP)
boot_cpu_data
#endif
;

	buf[2] |= ((0x0010)|(0x0008)|(0x0002)|(0x0100)|(0x0200));

	if ((__builtin_constant_p((4*32+7))&&(((((4*32+7))>>5)==0&&(1UL<<(((4*32+7))&31)&(
#if !(defined CONFIG_MATH_EMULATION)
(1<<((0*32+0)&31))
#elif (defined CONFIG_MATH_EMULATION)
0
#endif
|
#if !(defined CONFIG_X86_64) || (defined CONFIG_X86_64) && (defined CONFIG_PARAVIRT)
0
#elif (defined CONFIG_X86_64) && !(defined CONFIG_PARAVIRT)
(1<<((0*32+3))&31)
#endif
|
#if (defined CONFIG_X86_64)
(1<<((0*32+5)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
|
#if !(defined CONFIG_X86_64) && (defined CONFIG_X86_PAE) || (defined CONFIG_X86_64)
(1<<((0*32+6)&31))
#elif !(defined CONFIG_X86_64) && !(defined CONFIG_X86_PAE)
0
#endif
|
#if (defined CONFIG_X86_CMPXCHG64)
(1<<((0*32+8)&31))
#elif !(defined CONFIG_X86_CMPXCHG64)
0
#endif
|
#if !(defined CONFIG_X86_64) || (defined CONFIG_X86_64) && (defined CONFIG_PARAVIRT)
0
#elif (defined CONFIG_X86_64) && !(defined CONFIG_PARAVIRT)
(1<<((0*32+13))&31)
#endif
|
#if (defined CONFIG_X86_64)
(1<<((0*32+24)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
|
#if !(defined CONFIG_X86_64) && (defined CONFIG_X86_CMOV) || (defined CONFIG_X86_64)
(1<<((0*32+15)&31))
#elif !(defined CONFIG_X86_64) && !(defined CONFIG_X86_CMOV)
0
#endif
|
#if (defined CONFIG_X86_64)
(1<<((0*32+25)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
|
#if (defined CONFIG_X86_64)
(1<<((0*32+26)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
)))||((((4*32+7))>>5)==1&&(1UL<<(((4*32+7))&31)&(
#if (defined CONFIG_X86_64)
(1<<((1*32+29)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
|
#if (defined CONFIG_X86_USE_3DNOW)
(1<<((1*32+31)&31))
#elif !(defined CONFIG_X86_USE_3DNOW)
0
#endif
)))||((((4*32+7))>>5)==2&&(1UL<<(((4*32+7))&31)&0))||((((4*32+7))>>5)==3&&(1UL<<(((4*32+7))&31)&(
#if !(defined CONFIG_X86_64) && (defined CONFIG_X86_P6_NOP) || (defined CONFIG_X86_64)
(1<<((3*32+20)&31))
#elif !(defined CONFIG_X86_64) && !(defined CONFIG_X86_P6_NOP)
0
#endif
)))||((((4*32+7))>>5)==4&&(1UL<<(((4*32+7))&31)&0))||((((4*32+7))>>5)==5&&(1UL<<(((4*32+7))&31)&0))||((((4*32+7))>>5)==6&&(1UL<<(((4*32+7))&31)&0))||((((4*32+7))>>5)==7&&(1UL<<(((4*32+7))&31)&0))||((((4*32+7))>>5)==8&&(1UL<<(((4*32+7))&31)&0))||((((4*32+7))>>5)==9&&(1UL<<(((4*32+7))&31)&0)))?1:(__builtin_constant_p(((4*32+7)))?constant_test_bit(((4*32+7)),((unsignedlong*)((c)->x86_capability))):variable_test_bit(((4*32+7)),((unsignedlong*)((c)->x86_capability))))))
		buf[2] |= ((0x0008)|(0x0002)|(0x0020)|(0x0800)|(0x0001));

	if ((__builtin_constant_p((0*32+22))&&(((((0*32+22))>>5)==0&&(1UL<<(((0*32+22))&31)&(
#if !(defined CONFIG_MATH_EMULATION)
(1<<((0*32+0)&31))
#elif (defined CONFIG_MATH_EMULATION)
0
#endif
|
#if !(defined CONFIG_X86_64) || (defined CONFIG_X86_64) && (defined CONFIG_PARAVIRT)
0
#elif (defined CONFIG_X86_64) && !(defined CONFIG_PARAVIRT)
(1<<((0*32+3))&31)
#endif
|
#if (defined CONFIG_X86_64)
(1<<((0*32+5)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
|
#if !(defined CONFIG_X86_64) && (defined CONFIG_X86_PAE) || (defined CONFIG_X86_64)
(1<<((0*32+6)&31))
#elif !(defined CONFIG_X86_64) && !(defined CONFIG_X86_PAE)
0
#endif
|
#if (defined CONFIG_X86_CMPXCHG64)
(1<<((0*32+8)&31))
#elif !(defined CONFIG_X86_CMPXCHG64)
0
#endif
|
#if !(defined CONFIG_X86_64) || (defined CONFIG_X86_64) && (defined CONFIG_PARAVIRT)
0
#elif (defined CONFIG_X86_64) && !(defined CONFIG_PARAVIRT)
(1<<((0*32+13))&31)
#endif
|
#if (defined CONFIG_X86_64)
(1<<((0*32+24)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
|
#if !(defined CONFIG_X86_64) && (defined CONFIG_X86_CMOV) || (defined CONFIG_X86_64)
(1<<((0*32+15)&31))
#elif !(defined CONFIG_X86_64) && !(defined CONFIG_X86_CMOV)
0
#endif
|
#if (defined CONFIG_X86_64)
(1<<((0*32+25)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
|
#if (defined CONFIG_X86_64)
(1<<((0*32+26)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
)))||((((0*32+22))>>5)==1&&(1UL<<(((0*32+22))&31)&(
#if (defined CONFIG_X86_64)
(1<<((1*32+29)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
|
#if (defined CONFIG_X86_USE_3DNOW)
(1<<((1*32+31)&31))
#elif !(defined CONFIG_X86_USE_3DNOW)
0
#endif
)))||((((0*32+22))>>5)==2&&(1UL<<(((0*32+22))&31)&0))||((((0*32+22))>>5)==3&&(1UL<<(((0*32+22))&31)&(
#if !(defined CONFIG_X86_64) && (defined CONFIG_X86_P6_NOP) || (defined CONFIG_X86_64)
(1<<((3*32+20)&31))
#elif !(defined CONFIG_X86_64) && !(defined CONFIG_X86_P6_NOP)
0
#endif
)))||((((0*32+22))>>5)==4&&(1UL<<(((0*32+22))&31)&0))||((((0*32+22))>>5)==5&&(1UL<<(((0*32+22))&31)&0))||((((0*32+22))>>5)==6&&(1UL<<(((0*32+22))&31)&0))||((((0*32+22))>>5)==7&&(1UL<<(((0*32+22))&31)&0))||((((0*32+22))>>5)==8&&(1UL<<(((0*32+22))&31)&0))||((((0*32+22))>>5)==9&&(1UL<<(((0*32+22))&31)&0)))?1:(__builtin_constant_p(((0*32+22)))?constant_test_bit(((0*32+22)),((unsignedlong*)((c)->x86_capability))):variable_test_bit(((0*32+22)),((unsignedlong*)((c)->x86_capability))))))
		buf[2] |= (0x0004);

	/*
	 * If mwait/monitor is unsupported, C2/C3_FFH will be disabled
	 */
	if (!(__builtin_constant_p((4*32+3))&&(((((4*32+3))>>5)==0&&(1UL<<(((4*32+3))&31)&(
#if !(defined CONFIG_MATH_EMULATION)
(1<<((0*32+0)&31))
#elif (defined CONFIG_MATH_EMULATION)
0
#endif
|
#if !(defined CONFIG_X86_64) || (defined CONFIG_X86_64) && (defined CONFIG_PARAVIRT)
0
#elif (defined CONFIG_X86_64) && !(defined CONFIG_PARAVIRT)
(1<<((0*32+3))&31)
#endif
|
#if (defined CONFIG_X86_64)
(1<<((0*32+5)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
|
#if !(defined CONFIG_X86_64) && (defined CONFIG_X86_PAE) || (defined CONFIG_X86_64)
(1<<((0*32+6)&31))
#elif !(defined CONFIG_X86_64) && !(defined CONFIG_X86_PAE)
0
#endif
|
#if (defined CONFIG_X86_CMPXCHG64)
(1<<((0*32+8)&31))
#elif !(defined CONFIG_X86_CMPXCHG64)
0
#endif
|
#if !(defined CONFIG_X86_64) || (defined CONFIG_X86_64) && (defined CONFIG_PARAVIRT)
0
#elif (defined CONFIG_X86_64) && !(defined CONFIG_PARAVIRT)
(1<<((0*32+13))&31)
#endif
|
#if (defined CONFIG_X86_64)
(1<<((0*32+24)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
|
#if !(defined CONFIG_X86_64) && (defined CONFIG_X86_CMOV) || (defined CONFIG_X86_64)
(1<<((0*32+15)&31))
#elif !(defined CONFIG_X86_64) && !(defined CONFIG_X86_CMOV)
0
#endif
|
#if (defined CONFIG_X86_64)
(1<<((0*32+25)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
|
#if (defined CONFIG_X86_64)
(1<<((0*32+26)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
)))||((((4*32+3))>>5)==1&&(1UL<<(((4*32+3))&31)&(
#if (defined CONFIG_X86_64)
(1<<((1*32+29)&31))
#elif !(defined CONFIG_X86_64)
0
#endif
|
#if (defined CONFIG_X86_USE_3DNOW)
(1<<((1*32+31)&31))
#elif !(defined CONFIG_X86_USE_3DNOW)
0
#endif
)))||((((4*32+3))>>5)==2&&(1UL<<(((4*32+3))&31)&0))||((((4*32+3))>>5)==3&&(1UL<<(((4*32+3))&31)&(
#if !(defined CONFIG_X86_64) && (defined CONFIG_X86_P6_NOP) || (defined CONFIG_X86_64)
(1<<((3*32+20)&31))
#elif !(defined CONFIG_X86_64) && !(defined CONFIG_X86_P6_NOP)
0
#endif
)))||((((4*32+3))>>5)==4&&(1UL<<(((4*32+3))&31)&0))||((((4*32+3))>>5)==5&&(1UL<<(((4*32+3))&31)&0))||((((4*32+3))>>5)==6&&(1UL<<(((4*32+3))&31)&0))||((((4*32+3))>>5)==7&&(1UL<<(((4*32+3))&31)&0))||((((4*32+3))>>5)==8&&(1UL<<(((4*32+3))&31)&0))||((((4*32+3))>>5)==9&&(1UL<<(((4*32+3))&31)&0)))?1:(__builtin_constant_p(((4*32+3)))?constant_test_bit(((4*32+3)),((unsignedlong*)((c)->x86_capability))):variable_test_bit(((4*32+3)),((unsignedlong*)((c)->x86_capability))))))
		buf[2] &= ~((0x0200));
}

void main() {}