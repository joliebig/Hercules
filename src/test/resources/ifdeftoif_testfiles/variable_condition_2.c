/*
Test case for SQLite bug with th3 bugs/2010-03-05a.test cfg/64k.cfg

Variable in elseif-condition is duplicated.
Condition-check is not guarded by Hercules.

Correct ifdeftoif code would be:
                if( nOld==nNew ){
                    ...
                }else if( (! id2i.f_sqlite_default_memstatus) && _3778_sqlite3Config.bMemstat ){
                    ...
                }else if( (id2i.f_sqlite_default_memstatus) && _3777_sqlite3Config.bMemstat ){
                    ...
*/

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

//  #if defined(ELSE)
    else {
      toReturn += 100000;
    }
//  #endif
    return toReturn;
}
