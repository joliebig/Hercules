#if (definedEx(CONFIG_PARAVIRT) && definedEx(CONFIG_LOCKDEP))
static inline void paravirt_release_pte(int pfn)
{
	({
        if (1) 
        #if (definedEx(CONFIG_PARAVIRT_DEBUG) && definedEx(CONFIG_DEBUG_BUGVERBOSE) && definedEx(CONFIG_BUG) && definedEx(CONFIG_LOCKDEP) && definedEx(CONFIG_PARAVIRT))
        {
          do {
			  ;
          } while (0);
        }
        #endif
        #if (!((definedEx(CONFIG_PARAVIRT_DEBUG) && definedEx(CONFIG_DEBUG_BUGVERBOSE) && definedEx(CONFIG_BUG) && definedEx(CONFIG_LOCKDEP) && definedEx(CONFIG_PARAVIRT))))
        ;
        #endif
    });
}
