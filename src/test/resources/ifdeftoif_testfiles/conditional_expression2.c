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
  int x = 0;
  int y = 0;
  
  // first IF
  if (
  #if definedEx(D)
  x > 0
  #else
  a = 0
  #endif
  ) {
	x = 0;
  }
  
  // second IF
  if (
  #if definedEx(D)
  b > 0
  #elif definedEx(A)
  a = 0
  #elif definedEx(B)
  c = 0
  #else
  x = 0
  #endif
  )
  #if definedEx(D)
  {
	y = 0;
  }
  #else
    {
	y = 1;
  }
  #endif
  else if (
  #if definedEx(Y)
  b > 0
  #else
  x = 0
  #endif
  )
    #if definedEx(D)
  {
	y = 0;
	#if definedEx(Y)
	b = 0;
	#else
	x = 0;
	#endif
  }
  #else
    {
	y = 1;
	b = 1;
  }
  #endif
}