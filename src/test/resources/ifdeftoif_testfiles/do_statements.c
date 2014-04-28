void main() {
#if definedEx(CONFIG_PARAVIRT) && definedEx(CONFIG_PARAVIRT_DEBUG) && definedEx(CONFIG_BUG)
do {
	if (0)
	#if definedEx(CONFIG_DEBUG_BUGVERBOSE)
	do {
	int i;
	} while (0);
	#else
	do {
	int j;
	} while (0);
	#endif
} while (0);
#endif
}