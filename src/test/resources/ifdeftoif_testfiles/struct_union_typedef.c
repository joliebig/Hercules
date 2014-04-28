#if definedEx(CONFIG_EXPR_MATH_SUPPORT_64)
typedef int arith_t;
#endif
#if !definedEx(CONFIG_EXPR_MATH_SUPPORT_64)
typedef long arith_t;
#endif

struct valinfo {
	union {                         /* The value itself. */
		arith_t i;
		char *s;
	} u;
};

static arith_t arithmetic_common(int op)
{
	arith_t ll, rr;
	if (rr == 0) {
		return 0;
	}
	return op;
}

void main() {
	arith_t ll, rr;
}