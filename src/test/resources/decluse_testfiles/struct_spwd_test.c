#if (definedEx(CONFIG_FEATURE_SHADOWPASSWDS) && definedEx(CONFIG_USE_BB_SHADOW))
struct spwd {
  char *sp_namp;
  long sp_flag;
} ;
#endif

#if (!definedEx(CONFIG_USE_BB_SHADOW) && definedEx(CONFIG_FEATURE_SHADOWPASSWDS))
struct spwd {
  char *sp_namp;
} ;
#endif

void main() {
  #if definedEx(CONFIG_FEATURE_SHADOWPASSWDS)
  struct spwd  spw;
  #endif
}