#if definedEx(CONFIG_FEATURE_TEST_64)
typedef long number_t;
#endif


#if !definedEx(CONFIG_FEATURE_TEST_64)
typedef int number_t;
#endif

static number_t primary(int i);

static number_t nexpr(int i)  {
	number_t res;
	(res = primary(i));
}

void main() {}