typedef struct kebab {
	int doener;
	#if definedEx(A)
	const struct builtin *se_builtin;
	#endif
} kebab_t;

#if definedEx(A)
struct builtin {
	int zwiebeln;
	void (*bi_stream_fn)(int, kebab_t *) ;
} ;
#endif

int main() {
	kebab_t *sep, *sep2;
	int ctrl;
	#if definedEx(A)
	sep->se_builtin->bi_stream_fn(ctrl, sep);
	#endif
	return 0;
}
	