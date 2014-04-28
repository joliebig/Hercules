#include "C:\Users\Flo\TypeChef\ifdeftoif\id2i_optionstruct.h"
enum  {
  COMMON_BUFSIZE = ((8192 >= (256 * sizeof(void *))) ? (8192 + 1) : (256 * sizeof(void *)))
} ;
extern char bb_common_bufsiz1[COMMON_BUFSIZE];
int index_in_substrings(const  char *strings , const  char *key );
struct  globals {
  int _132_show_color ;
  unsigned all_fmt ;
} __attribute__((__may_alias__)) ;
enum  {
  OPT_g = (1 << 6),
  OPT_Q = (1 << 10),
  OPTBIT_color = (13 + (4 
  #if (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD))
  * 1
  #endif
   
  #if (!definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) && !definedEx(CONFIG_FTPD))
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
enum  {
  TERMINAL_WIDTH = 80,
  COLUMN_GAP = 2,
  STYLE_COLUMNS = (1 << 21),
  STYLE_LONG = (2 << 21),
  STYLE_SINGLE = (3 << 21),
  STYLE_MASK = STYLE_SINGLE,
  LIST_INO = (1 << 0),
  LIST_BLOCKS = (1 << 1),
  LIST_MODEBITS = (1 << 2),
  LIST_NLINKS = (1 << 3),
  LIST_ID_NAME = (1 << 4),
  LIST_ID_NUMERIC = (1 << 5),
  LIST_CONTEXT = (1 << 6),
  LIST_SIZE = (1 << 7),
  LIST_DATE_TIME = (1 << 9),
  LIST_FULLTIME = (1 << 10),
  LIST_FILENAME = (1 << 11),
  LIST_SYMLINK = (1 << 12),
  LIST_FILETYPE = (1 << 13),
  LIST_EXEC = (1 << 14),
  LIST_MASK = ((LIST_EXEC << 1) - 1),
  DISP_DIRNAME = (1 << 15),
  DISP_HIDDEN = (1 << 16),
  DISP_DOT = (1 << 17),
  DISP_NOLIST = (1 << 18),
  DISP_RECURSIVE = (1 << 19),
  DISP_ROWS = (1 << 20),
  DISP_MASK = (((DISP_ROWS << 1) - 1) & (~ (DISP_DIRNAME - 1))),
  SORT_FORWARD = 0,
  SORT_REVERSE = (1 << 27),
  SORT_NAME = 0,
  SORT_SIZE = (1 << 28),
  SORT_ATIME = (2 << 28),
  SORT_CTIME = (3 << 28),
  SORT_MTIME = (4 << 28),
  SORT_VERSION = (5 << 28),
  SORT_EXT = (6 << 28),
  SORT_DIR = (7 << 28),
  SORT_MASK = ((7 << 28) 
  #if definedEx(CONFIG_FEATURE_LS_SORTFILES)
  * 1
  #endif
   
  #if !definedEx(CONFIG_FEATURE_LS_SORTFILES)
  * 0
  #endif
  ),
  TIME_CHANGE = ((1 << 23) 
  #if (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD))
  * 1
  #endif
   
  #if (!definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) && !definedEx(CONFIG_FTPD))
  * 0
  #endif
  ),
  TIME_ACCESS = ((1 << 24) 
  #if (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD))
  * 1
  #endif
   
  #if (!definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) && !definedEx(CONFIG_FTPD))
  * 0
  #endif
  ),
  TIME_MASK = ((3 << 23) 
  #if (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD))
  * 1
  #endif
   
  #if (!definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) && !definedEx(CONFIG_FTPD))
  * 0
  #endif
  ),
  FOLLOW_LINKS = ((1 << 25) 
  #if definedEx(CONFIG_FEATURE_LS_FOLLOWLINKS)
  * 1
  #endif
   
  #if !definedEx(CONFIG_FEATURE_LS_FOLLOWLINKS)
  * 0
  #endif
  ),
  LS_DISP_HR = ((1 << 26) 
  #if definedEx(CONFIG_FEATURE_HUMAN_READABLE)
  * 1
  #endif
   
  #if !definedEx(CONFIG_FEATURE_HUMAN_READABLE)
  * 0
  #endif
  ),
  LIST_SHORT = LIST_FILENAME,
  LIST_LONG = (LIST_MODEBITS | LIST_NLINKS | LIST_ID_NAME | LIST_SIZE | LIST_DATE_TIME | LIST_FILENAME | LIST_SYMLINK),
  SPLIT_DIR = 1,
  SPLIT_FILE = 0,
  SPLIT_SUBDIR = 2
} ;
enum  {
  LIST_MASK_TRIGGER = 0,
  STYLE_MASK_TRIGGER = STYLE_MASK,
  DISP_MASK_TRIGGER = DISP_ROWS,
  SORT_MASK_TRIGGER = SORT_MASK
} ;
static const unsigned opt_flags[] =  { (LIST_SHORT | STYLE_COLUMNS),  (DISP_HIDDEN | DISP_DOT),  DISP_NOLIST,  LIST_INO,  (LIST_LONG | STYLE_LONG),  (LIST_SHORT | STYLE_SINGLE),  0,  LIST_ID_NUMERIC,  LIST_BLOCKS,  DISP_ROWS,  0,  DISP_HIDDEN
#if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
,
#endif

#if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
 (1 * LIST_CONTEXT)
#endif

#if (!definedEx(CONFIG_FEATURE_FIND_CONTEXT) && !definedEx(CONFIG_SELINUX))
,
#endif

#if (!definedEx(CONFIG_FEATURE_FIND_CONTEXT) && !definedEx(CONFIG_SELINUX))
 (0 * LIST_CONTEXT)
#endif

#if (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD))
,
#endif

#if (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD))
 (TIME_CHANGE 
#if (definedEx(CONFIG_FEATURE_LS_SORTFILES) && (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD)))
| (1 * SORT_CTIME)
#endif
 
#if (!definedEx(CONFIG_FEATURE_LS_SORTFILES) && (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD)))
| (0 * SORT_CTIME)
#endif
)
#endif

#if (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD))
,
#endif

#if (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD))
 LIST_FULLTIME
#endif

#if (definedEx(CONFIG_FEATURE_LS_SORTFILES) && (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD)))
,
#endif

#if (definedEx(CONFIG_FEATURE_LS_SORTFILES) && (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD)))
 (1 * SORT_MTIME)
#endif

#if (!definedEx(CONFIG_FEATURE_LS_SORTFILES) && (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD)))
,
#endif

#if (!definedEx(CONFIG_FEATURE_LS_SORTFILES) && (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD)))
 (0 * SORT_MTIME)
#endif

#if (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD))
,
#endif

#if (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD))
 (TIME_ACCESS 
#if (definedEx(CONFIG_FEATURE_LS_SORTFILES) && (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD)))
| (1 * SORT_ATIME)
#endif
 
#if (!definedEx(CONFIG_FEATURE_LS_SORTFILES) && (definedEx(CONFIG_FEATURE_LS_TIMESTAMPS) || definedEx(CONFIG_FTPD)))
| (0 * SORT_ATIME)
#endif
)
#endif

#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
,
#endif

#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
 SORT_SIZE
#endif

#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
,
#endif

#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
 SORT_EXT
#endif

#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
,
#endif

#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
 SORT_REVERSE
#endif

#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
,
#endif

#if definedEx(CONFIG_FEATURE_LS_SORTFILES)
 SORT_VERSION
#endif

#if definedEx(CONFIG_FEATURE_LS_FILETYPES)
,
#endif

#if definedEx(CONFIG_FEATURE_LS_FILETYPES)
 (LIST_FILETYPE | LIST_EXEC)
#endif

#if definedEx(CONFIG_FEATURE_LS_FILETYPES)
,
#endif

#if definedEx(CONFIG_FEATURE_LS_FILETYPES)
 LIST_FILETYPE
#endif

#if definedEx(CONFIG_FEATURE_LS_FOLLOWLINKS)
,
#endif

#if definedEx(CONFIG_FEATURE_LS_FOLLOWLINKS)
 FOLLOW_LINKS
#endif

#if definedEx(CONFIG_FEATURE_LS_RECURSIVE)
,
#endif

#if definedEx(CONFIG_FEATURE_LS_RECURSIVE)
 DISP_RECURSIVE
#endif

#if definedEx(CONFIG_FEATURE_HUMAN_READABLE)
,
#endif

#if definedEx(CONFIG_FEATURE_HUMAN_READABLE)
 LS_DISP_HR
#endif

#if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
,
#endif

#if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
 (LIST_MODEBITS | LIST_NLINKS | LIST_CONTEXT | LIST_SIZE | LIST_DATE_TIME)
#endif

#if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
,
#endif

#if (definedEx(CONFIG_FEATURE_FIND_CONTEXT) || definedEx(CONFIG_SELINUX))
 (LIST_MODEBITS | LIST_ID_NAME | LIST_CONTEXT)
#endif
,  (1U << 31)};
void main()  {
  id2i_init();
  unsigned i;
  unsigned opt;
  static const char _132_color_str[] __attribute__((aligned (1))) =  "always\0" "yes\0" "force\0" "auto\0" "tty\0" "if-tty\0";
  const char *_132_color_opt =  _132_color_str;
  for ((i = 0); (opt_flags[i] != (1U << 31)); i++) {
    if ((1 << i)) {
      unsigned flags =  opt_flags[i];
      if ((flags & LIST_MASK_TRIGGER)) {
        ((*((struct  globals   *) (&bb_common_bufsiz1))).all_fmt &= (~ LIST_MASK));
      }  
      if ((flags & STYLE_MASK_TRIGGER)) {
        ((*((struct  globals   *) (&bb_common_bufsiz1))).all_fmt &= (~ STYLE_MASK));
      }  
      if ((flags & SORT_MASK_TRIGGER)) {
        ((*((struct  globals   *) (&bb_common_bufsiz1))).all_fmt &= (~ SORT_MASK));
      }  
      if ((flags & DISP_MASK_TRIGGER)) {
        ((*((struct  globals   *) (&bb_common_bufsiz1))).all_fmt &= (~ DISP_MASK));
      }  
      if ((flags & TIME_MASK)) {
        ((*((struct  globals   *) (&bb_common_bufsiz1))).all_fmt &= (~ TIME_MASK));
      }  
      if ((flags & LIST_CONTEXT)) {
        ((*((struct  globals   *) (&bb_common_bufsiz1))).all_fmt |= STYLE_SINGLE);
      }  
      ((*((struct  globals   *) (&bb_common_bufsiz1))).all_fmt |= flags);
    }  
  }
  if (((id2i.config_feature_ls_color ) )) {
    if ((opt & OPT_color)) {
      if ((_132_color_opt[0] == 'n')) {
        ((*((struct  globals   *) (&bb_common_bufsiz1)))._132_show_color = 0);
      }  
    }  
  }  
}