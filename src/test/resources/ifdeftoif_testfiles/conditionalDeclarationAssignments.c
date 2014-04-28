int a = 5 *
#if definedEx(A)
0
#else
1
#endif
;

enum  {
  PSSCAN_TTY = (1 << 14),
  PSSCAN_SMAPS = ((1 << 15) 
  #if definedEx(CONFIG_FEATURE_TOPMEM)
  * 1
  #endif
   
  #if !definedEx(CONFIG_FEATURE_TOPMEM)
  * 0
  #endif
  )
} ;

void main() {
	int x = 5 *
	#if definedEx(A)
0
#else
1
#endif
;
#if definedEx(C)
	int y = 5 *
#if definedEx(A)
(0 * a)
#else
(1 * a)
#endif
;
#endif
#if definedEx(Z)
	int z = 5 *
#if definedEx(B)
(0 * a)
#else
(1 * a)
#endif
;
#endif
}