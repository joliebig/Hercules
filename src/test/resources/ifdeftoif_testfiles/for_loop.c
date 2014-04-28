int a =  (5 
#if definedEx(A)
* 0
#endif
#if !definedEx(A)
* 1
#endif
);

int b =  (5 
#if definedEx(B)
* 0
#endif
#if !definedEx(B)
* 1
#endif
);
int c =  (5 
#if definedEx(C)
* 0
#endif
#if !definedEx(C)
* 1
#endif
);
void main()  {
  int x =  0;
  for (a = 0; b < 10; c++) {
	// Nothing
  }
  while(a) {
	// Nothing
  }
}