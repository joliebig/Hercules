typedef unsigned char		uint8_t;
extern const char bb_default_login_shell[];
static void new_init_action(uint8_t action_type, const char *command, const char *cons)
{}

void main() {
#if definedEx(CONFIG_FEATURE_USE_INITTAB)
	char *token[4];

	if (0)
#endif
	{
		/* No inittab file - set up some default behavior */
		/* Reboot on Ctrl-Alt-Del */
		new_init_action(0x20, "reboot", "");
		/* Umount all filesystems on halt/reboot */
		new_init_action(0x40, "umount -a -r", "");
		/* Swapoff on halt/reboot */
		if (
#if definedEx(CONFIG_SWAPONOFF)
1
#endif
#if !definedEx(CONFIG_SWAPONOFF)
0
#endif
)
			new_init_action(0x40, "swapoff -a", "");
		/* Prepare to restart init when a QUIT is received */
		new_init_action(0x80, "init", "");
		/* Askfirst shell on tty1-4 */
		new_init_action(0x10, bb_default_login_shell, "");
//TODO: VC_1 instead of ""? "" is console -> ctty problems -> angry users
		new_init_action(0x10, bb_default_login_shell, 
#if definedEx(CONFIG_FEATURE_DEVFS)
"/dev/vc/2"
#endif
#if !definedEx(CONFIG_FEATURE_DEVFS)
"/dev/tty2"
#endif
);
		new_init_action(0x10, bb_default_login_shell, 
#if definedEx(CONFIG_FEATURE_DEVFS)
"/dev/vc/3"
#endif
#if !definedEx(CONFIG_FEATURE_DEVFS)
"/dev/tty3"
#endif
);
		new_init_action(0x10, bb_default_login_shell, 
#if definedEx(CONFIG_FEATURE_DEVFS)
"/dev/vc/4"
#endif
#if !definedEx(CONFIG_FEATURE_DEVFS)
"/dev/tty4"
#endif
);
		/* sysinit */
		new_init_action(0x01, "/etc/init.d/rcS", "");
		return;
	}
}