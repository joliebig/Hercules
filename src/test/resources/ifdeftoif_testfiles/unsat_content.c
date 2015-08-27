#ifdef CONFIG_ARCH_OMAP3

static int* var;

int func()
{
    return *var;
}

#endif /* CONFIG_ARCH_OMAP3 */

int main() {
#ifdef CONFIG_ARCH_OMAP3
    return 1;
#endif
    return 0;
}