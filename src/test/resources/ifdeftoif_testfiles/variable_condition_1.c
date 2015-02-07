// #include <stdio.h>
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

struct Sqlite3Config {
  int bMemstat;                     /* True to enable memory status */
};

struct Sqlite3Config sqlite3Config = {
   #if defined(IFDEF_VAR)
   1
   #else
   0
   #endif
};

struct Sqlite3Config_2 {
  int bMemstat;                     /* True to enable memory status */
};

struct Sqlite3Config sqlite3Config_2 = {
   #if defined(IFDEF_VAR_2)
   1
   #else
   0
   #endif
};

#define sqlite3GlobalConfig sqlite3Config
#define sqlite3GlobalConfig_2 sqlite3Config_2

int main(int argc, char **argv) {

    int nOld = 0;
    int nNew = 1;
    int toReturn=1;

    if( nOld==nNew ){
    } else if( sqlite3GlobalConfig.bMemstat ){ // should be 1 iff defined(IFDEF_VAR)
    #if defined(IFDEF_VAR)
        toReturn += 1;       // should be returned iff defined(IFDEF_VAR)
    #else
        toReturn += 2000;       // should not be reachable
    #endif
    }
    #if defined(ELSE)
    else {
        toReturn += 2;
    }
    #endif
    if( sqlite3GlobalConfig.bMemstat ){ // should be 1 iff defined(IFDEF_VAR)
        #if defined(IFDEF_VAR)
            toReturn += 5;       // should be returned iff defined(IFDEF_VAR)
        #else
            toReturn += 2000;       // should not be reachable
        #endif
    }
     #if defined(ELSE)
    else {
        toReturn += 20;
    }
    #endif
    if( sqlite3GlobalConfig.bMemstat ){ // should be 1 iff defined(IFDEF_VAR)
            #if defined(IFDEF_VAR)
                toReturn += 5;       // should be returned iff defined(IFDEF_VAR)
            #else
                toReturn += 2000;       // should not be reachable
            #endif
        } else if( sqlite3GlobalConfig_2.bMemstat ){ // should be 1 iff defined(IFDEF_VAR_2)
                    #if defined(IFDEF_VAR_2)
                        toReturn += 30;       // should be returned iff defined(IFDEF_VAR_2)
                    #else
                        toReturn += 2000;       // should not be reachable
                    #endif
                }
         #if defined(ELSE)
        else {
            toReturn += 20;
        }
        #endif
    //printf("Return: %d", toReturn);
    return toReturn;
}
