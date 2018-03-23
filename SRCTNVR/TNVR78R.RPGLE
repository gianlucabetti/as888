000100991123     fFLEUC00F  IP   E             DISK    prefix(o_)
000200991123     fFIEUC00F  O    E             DISK
000300990914
000400991123     c                   clear                   fieuc000
000500991123
000600991123     c                   eval      EUClnp = O_EUCLNP
000700991123     c     O_EUCaas      add       1900          EUCAAS
000800991123     c                   eval      EUCnrs = O_EUCnrs
000900991123     c                   eval      EUCnsp = O_EUCnsp
001000991123     c                   eval      EUCfla = O_EUCfla
001100991123     c                   if        O_EUCdev > *zeros
001200991123     c     O_EUCdev      add       19000000      EUCdev
001300991123     c                   endif
001400991123     c                   if        O_EUCdat > *zeros
001500991123     c     O_EUCdat      add       19000000      EUCdat
001600991123     c                   endif
001700991123
001800991123     c                   write     fieuc000
