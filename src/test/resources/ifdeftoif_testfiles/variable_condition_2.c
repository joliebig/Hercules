int else_f =
#if defined(ELSE)
1
#else
0
#endif
;

int var_f2 =
#if defined(IFDEF_VAR2)
1
#else
0
#endif
;

int main(int argc, char **argv) {
    int toReturn=1;

    if (
#if defined(IFDEF_VAR)
      var_f2
      #else
      else_f
      #endif
      )
      #if defined(IFDEF_VAR2)
    {
      toReturn += 10;
    }
  #else
    {
      toReturn += 100;
    }
  #endif
    else if (else_f) {
      toReturn += 1000;
    }
    #if defined(IFDEF_VAR)
    else if (var_f2) {
      toReturn += 10000;
    }
    #endif
    else {
      toReturn += 100000;
    }

    if (
#if defined(IFDEF_VAR)
      var_f2
      #else
      else_f
      #endif
      )
      #if defined(IFDEF_VAR2)
    {
      toReturn += 1000000;
    }
  #else
    {
      toReturn += 10000000;
    }
  #endif
    else if (else_f) {
      toReturn += 100000000;
    }
    #if defined(IFDEF_VAR)
    else if (var_f2) {
      toReturn += 1000000000;
    }
    #endif

  #if defined(ELSE)
    else {
      toReturn += 10000000000;
    }
  #endif
    return toReturn;
}
