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
   #if definedEx(SQLITE_DEFAULT_MEMSTATUS)
   1
   #else
   0
   #endif
};

#define sqlite3GlobalConfig sqlite3Config

int main(int argc, char **argv) {

    int nOld = 0;
    int nNew = 1;

    if( nOld==nNew ){

    } else if( sqlite3GlobalConfig.bMemstat ){ // should be 1 iff defined(SQLITE_DEFAULT_MEMSTATUS)
    #if definedEx(SQLITE_DEFAULT_MEMSTATUS)
        return 1;       // should be returned iff defined(SQLITE_DEFAULT_MEMSTATUS)
    #else
        return 0;       // should not be reachable
    #endif
    }
    return 5; // should be returned iff ! defined(SQLITE_DEFAULT_MEMSTATUS)
}

