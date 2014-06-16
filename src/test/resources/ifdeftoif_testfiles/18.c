int cpio_main(int argc __attribute__ ((__unused__)), char **argv)
{
#if definedEx(CONFIG_FEATURE_CPIO_O)
	if (1) {
		if (2) /* we _require_ "-H newc" */
			1+10;
		if (3) {
			1+10;
		}
 dump:
		return 1;
	}
 skip:
#endif

	return 0;
}
