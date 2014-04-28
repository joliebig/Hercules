int main() {
	char params[] =
	                         "-a\0"
	                         "-o\0"
	
#if definedEx(CONFIG_FEATURE_FIND_NOT)
"!\0"
#endif

#if definedEx(CONFIG_DESKTOP)
	                         "-and\0"
	                         "-or\0"
	
#if definedEx(CONFIG_FEATURE_FIND_NOT)
"-not\0"
#endif

#endif
	                         "-print\0"
	
#if definedEx(CONFIG_FEATURE_FIND_PRINT0)
"-print0\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_DEPTH)
"-depth\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_PRUNE)
"-prune\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_DELETE)
"-delete\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_EXEC)
"-exec\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_PAREN)
"(\0"
#endif
	                         "-name\0"
	                         "-iname\0"
	
#if definedEx(CONFIG_FEATURE_FIND_PATH)
"-path\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_REGEX)
"-regex\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_TYPE)
"-type\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_PERM)
"-perm\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_MTIME)
"-mtime\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_MMIN)
"-mmin\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_NEWER)
"-newer\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_INUM)
"-inum\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_USER)
"-user\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_GROUP)
"-group\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_SIZE)
"-size\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_CONTEXT)
"-context\0"
#endif

	
#if definedEx(CONFIG_FEATURE_FIND_LINKS)
"-links\0"
#endif

	;
	return 0;
}