#if definedEx(CONFIG_FEATURE_TEST_64)
typedef long number_t;
#endif


#if !definedEx(CONFIG_FEATURE_TEST_64)
typedef int number_t;
#endif

number_t primary();

void foo() {
	number_t i;
	i = primary();
}

number_t primary() {
	return 0;
}