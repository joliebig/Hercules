enum { COMMON_BUFSIZE = (8192 >= 256*sizeof(void*) ? 8192+1 : 256*sizeof(void*)) };
extern char bb_common_bufsiz1[COMMON_BUFSIZE];
int index_in_substrings(const  char *strings , const  char *key );
struct globals {
  #if definedEx(CONFIG_FEATURE_LS_COLOR)
  int show_color ;
  #endif
	unsigned all_fmt;
} __attribute__((__may_alias__));
enum  {
  OPT_g = (1 << 6),
  OPT_Q = (1 << 10),
  OPTBIT_color = (13 + (4 
  #if (definedEx(CONFIG_FTPD) || definedEx(CONFIG_FEATURE_LS_TIMESTAMPS))
  * 1
  #endif
   
  #if (!definedEx(CONFIG_FTPD) && !definedEx(CONFIG_FEATURE_LS_TIMESTAMPS))
  * 0
  #endif
  ) + (4 
  #if definedEx(CONFIG_FEATURE_LS_SORTFILES)
  * 1
  #endif
   
  #if !definedEx(CONFIG_FEATURE_LS_SORTFILES)
  * 0
  #endif
  ) + (2 
  #if definedEx(CONFIG_FEATURE_LS_FILETYPES)
  * 1
  #endif
   
  #if !definedEx(CONFIG_FEATURE_LS_FILETYPES)
  * 0
  #endif
  ) + (1 
  #if definedEx(CONFIG_FEATURE_LS_FOLLOWLINKS)
  * 1
  #endif
   
  #if !definedEx(CONFIG_FEATURE_LS_FOLLOWLINKS)
  * 0
  #endif
  ) + (1 
  #if definedEx(CONFIG_FEATURE_LS_RECURSIVE)
  * 1
  #endif
   
  #if !definedEx(CONFIG_FEATURE_LS_RECURSIVE)
  * 0
  #endif
  ) + (1 
  #if definedEx(CONFIG_FEATURE_HUMAN_READABLE)
  * 1
  #endif
   
  #if !definedEx(CONFIG_FEATURE_HUMAN_READABLE)
  * 0
  #endif
  ) + (2 
  #if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
  * 1
  #endif
   
  #if (!definedEx(CONFIG_FEATURE_FIND_CONTEXT) && !definedEx(CONFIG_SELINUX))
  * 0
  #endif
  ) + (2 
  #if definedEx(CONFIG_FEATURE_AUTOWIDTH)
  * 1
  #endif
   
  #if !definedEx(CONFIG_FEATURE_AUTOWIDTH)
  * 0
  #endif
  )),
  OPT_color = (1 << OPTBIT_color)
} ;
enum {
TERMINAL_WIDTH  = 80,           /* use 79 if terminal has linefold bug */
COLUMN_GAP      = 2,            /* includes the file type char */

/* what is the overall style of the listing */
STYLE_COLUMNS   = 1 << 21,      /* fill columns */
STYLE_LONG      = 2 << 21,      /* one record per line, extended info */
STYLE_SINGLE    = 3 << 21,      /* one record per line */
STYLE_MASK      = STYLE_SINGLE,

/* 51306 lrwxrwxrwx  1 root     root         2 May 11 01:43 /bin/view -> vi* */
/* what file information will be listed */
LIST_INO        = 1 << 0,
LIST_BLOCKS     = 1 << 1,
LIST_MODEBITS   = 1 << 2,
LIST_NLINKS     = 1 << 3,
LIST_ID_NAME    = 1 << 4,
LIST_ID_NUMERIC = 1 << 5,
LIST_CONTEXT    = 1 << 6,
LIST_SIZE       = 1 << 7,
//LIST_DEV        = 1 << 8, - unused, synonym to LIST_SIZE
LIST_DATE_TIME  = 1 << 9,
LIST_FULLTIME   = 1 << 10,
LIST_FILENAME   = 1 << 11,
LIST_SYMLINK    = 1 << 12,
LIST_FILETYPE   = 1 << 13,
LIST_EXEC       = 1 << 14,
LIST_MASK       = (LIST_EXEC << 1) - 1,

/* what files will be displayed */
DISP_DIRNAME    = 1 << 15,      /* 2 or more items? label directories */
DISP_HIDDEN     = 1 << 16,      /* show filenames starting with . */
DISP_DOT        = 1 << 17,      /* show . and .. */
DISP_NOLIST     = 1 << 18,      /* show directory as itself, not contents */
DISP_RECURSIVE  = 1 << 19,      /* show directory and everything below it */
DISP_ROWS       = 1 << 20,      /* print across rows */
DISP_MASK       = ((DISP_ROWS << 1) - 1) & ~(DISP_DIRNAME - 1),

/* how will the files be sorted (CONFIG_FEATURE_LS_SORTFILES) */
SORT_FORWARD    = 0,            /* sort in reverse order */
SORT_REVERSE    = 1 << 27,      /* sort in reverse order */

SORT_NAME       = 0,            /* sort by file name */
SORT_SIZE       = 1 << 28,      /* sort by file size */
SORT_ATIME      = 2 << 28,      /* sort by last access time */
SORT_CTIME      = 3 << 28,      /* sort by last change time */
SORT_MTIME      = 4 << 28,      /* sort by last modification time */
SORT_VERSION    = 5 << 28,      /* sort by version */
SORT_EXT        = 6 << 28,      /* sort by file name extension */
SORT_DIR        = 7 << 28,      /* sort by file or directory */
SORT_MASK       = (7 << 28) * 
#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
1
#endif
#if !definedEx(CONFIG_FEATURE_LS_SORTFILES)
0
#endif
,

/* which of the three times will be used */
TIME_CHANGE     = (1 << 23) * 
#if (definedEx(CONFIG_FTPD) || definedEx(CONFIG_FEATURE_LS_TIMESTAMPS))
1
#endif
#if (!definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) && !definedEx(CONFIG_FTPD))
0
#endif
,
TIME_ACCESS     = (1 << 24) * 
#if (definedEx(CONFIG_FTPD) || definedEx(CONFIG_FEATURE_LS_TIMESTAMPS))
1
#endif
#if (!definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) && !definedEx(CONFIG_FTPD))
0
#endif
,
TIME_MASK       = (3 << 23) * 
#if (definedEx(CONFIG_FTPD) || definedEx(CONFIG_FEATURE_LS_TIMESTAMPS))
1
#endif
#if (!definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) && !definedEx(CONFIG_FTPD))
0
#endif
,

FOLLOW_LINKS    = (1 << 25) * 
#if definedEx(CONFIG_FEATURE_LS_FOLLOWLINKS)
1
#endif
#if !definedEx(CONFIG_FEATURE_LS_FOLLOWLINKS)
0
#endif
,

LS_DISP_HR      = (1 << 26) * 
#if definedEx(CONFIG_FEATURE_HUMAN_READABLE)
1
#endif
#if !definedEx(CONFIG_FEATURE_HUMAN_READABLE)
0
#endif
,

LIST_SHORT      = LIST_FILENAME,
LIST_LONG       = LIST_MODEBITS | LIST_NLINKS | LIST_ID_NAME | LIST_SIZE |                   LIST_DATE_TIME | LIST_FILENAME | LIST_SYMLINK,


SPLIT_DIR       = 1,
SPLIT_FILE      = 0,
SPLIT_SUBDIR    = 2,
};

enum {
	LIST_MASK_TRIGGER	= 0,
	STYLE_MASK_TRIGGER	= STYLE_MASK,
	DISP_MASK_TRIGGER	= DISP_ROWS,
	SORT_MASK_TRIGGER	= SORT_MASK,
};

static const unsigned opt_flags[] = {
	LIST_SHORT | STYLE_COLUMNS, /* C */
	DISP_HIDDEN | DISP_DOT,     /* a */
	DISP_NOLIST,                /* d */
	LIST_INO,                   /* i */
	LIST_LONG | STYLE_LONG,     /* l - remember LS_DISP_HR in mask! */
	LIST_SHORT | STYLE_SINGLE,  /* 1 */
	0,                          /* g (don't show owner) - handled via OPT_g */
	LIST_ID_NUMERIC,            /* n */
	LIST_BLOCKS,                /* s */
	DISP_ROWS,                  /* x */
	0,                          /* Q (quote filename) - handled via OPT_Q */
	DISP_HIDDEN,                /* A */
	
#if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
1
#endif
#if (!definedEx(CONFIG_FEATURE_FIND_CONTEXT) && !definedEx(CONFIG_SELINUX))
0
#endif
 * LIST_CONTEXT, /* k (ignored if !SELINUX) */
#if (definedEx(CONFIG_FTPD) || definedEx(CONFIG_FEATURE_LS_TIMESTAMPS))
	TIME_CHANGE | (
#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
1
#endif
#if !definedEx(CONFIG_FEATURE_LS_SORTFILES)
0
#endif
 * SORT_CTIME),   /* c */
	LIST_FULLTIME,              /* e */
	
#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
1
#endif
#if !definedEx(CONFIG_FEATURE_LS_SORTFILES)
0
#endif
 * SORT_MTIME,   /* t */
	TIME_ACCESS | (
#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
1
#endif
#if !definedEx(CONFIG_FEATURE_LS_SORTFILES)
0
#endif
 * SORT_ATIME),   /* u */
#endif
#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
	SORT_SIZE,                  /* S */
	SORT_EXT,                   /* X */
	SORT_REVERSE,               /* r */
	SORT_VERSION,               /* v */
#endif
#if definedEx(CONFIG_FEATURE_LS_FILETYPES)
	LIST_FILETYPE | LIST_EXEC,  /* F */
	LIST_FILETYPE,              /* p */
#endif
#if definedEx(CONFIG_FEATURE_LS_FOLLOWLINKS)
	FOLLOW_LINKS,               /* L */
#endif
#if definedEx(CONFIG_FEATURE_LS_RECURSIVE)
	DISP_RECURSIVE,             /* R */
#endif
#if definedEx(CONFIG_FEATURE_HUMAN_READABLE)
	LS_DISP_HR,                 /* h */
#endif
#if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
	LIST_MODEBITS|LIST_NLINKS|LIST_CONTEXT|LIST_SIZE|LIST_DATE_TIME, /* K */
#endif
#if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
	LIST_MODEBITS|LIST_ID_NAME|LIST_CONTEXT, /* Z */
#endif
	(1U<<31)
	/* options after Z are not processed through opt_flags:
	 * T, w - ignored
	 */
};

void main() {
	unsigned i;
	unsigned opt;
	#if definedEx(CONFIG_FEATURE_LS_COLOR)
	static const char color_str[] __attribute__((aligned (1))) =  "always\0"
	"yes\0"
	"force\0"
	"auto\0"
	"tty\0"
	"if-tty\0";
	#endif
  
  
  #if definedEx(CONFIG_FEATURE_LS_COLOR)
  const char *color_opt =  color_str;
  #endif
	for (i = 0; opt_flags[i] != (1U<<31); i++) {
		if ((1 << i)) {
			unsigned flags = opt_flags[i];

			if (flags & LIST_MASK_TRIGGER)
				((*(struct globals*)&bb_common_bufsiz1).all_fmt ) &= ~LIST_MASK;
			if (flags & STYLE_MASK_TRIGGER)
				((*(struct globals*)&bb_common_bufsiz1).all_fmt ) &= ~STYLE_MASK;
			if (flags & SORT_MASK_TRIGGER)
				((*(struct globals*)&bb_common_bufsiz1).all_fmt ) &= ~SORT_MASK;
			if (flags & DISP_MASK_TRIGGER)
				((*(struct globals*)&bb_common_bufsiz1).all_fmt ) &= ~DISP_MASK;
			if (flags & TIME_MASK)
				((*(struct globals*)&bb_common_bufsiz1).all_fmt ) &= ~TIME_MASK;
			if (flags & LIST_CONTEXT)
				((*(struct globals*)&bb_common_bufsiz1).all_fmt ) |= STYLE_SINGLE;
			/* huh?? opt cannot be 'l' */
			//if (LS_DISP_HR && opt == 'l')
			//	all_fmt &= ~LS_DISP_HR;
			((*(struct globals*)&bb_common_bufsiz1).all_fmt ) |= flags;
		}
	}
	  #if definedEx(CONFIG_FEATURE_LS_COLOR)
  if ((opt & OPT_color)) {
    if ((color_opt[0] == 'n')) {
      ((*((struct  globals   *) (&bb_common_bufsiz1))).show_color = 0);
    }  
    else {
      switch (index_in_substrings(color_str
      #if definedEx(CONFIG_FEATURE_LS_COLOR)
      ,
      #endif
      color_opt)) {
        case 3:
        case 4:
        case 5:
        if (1) {
          case 0:
          case 1:
          case 2:
          ((*((struct  globals   *) (&bb_common_bufsiz1))).show_color = 1);
        }  
      }
    }
  }  
  #endif
}