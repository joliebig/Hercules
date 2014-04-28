enum {
  #if definedEx(CONFIG_FEATURE_CPIO_P)
  OPTBIT_PASSTHROUGH,
  #endif
  #if !definedEx(CONFIG_FEATURE_CPIO_P)
  CPIO_OPT_PASSTHROUGH = (+ 0),
  #endif
  #if definedEx(CONFIG_FEATURE_CPIO_P)
  CPIO_OPT_PASSTHROUGH = ((1 << OPTBIT_PASSTHROUGH) + 0),
  #endif
};

void xpipe(int filedes[2]) ;
struct fd_pair {
  int rd;
  int wr;
} ;

void main() {
#if definedEx(CONFIG_FEATURE_CPIO_O)
	struct fd_pair  pp;
  if (CPIO_OPT_PASSTHROUGH) {
#if (definedEx(CONFIG_FEATURE_CPIO_O) && !definedEx(CONFIG_NOMMU))
    xpipe((&pp.rd));
#endif
}
#endif
}