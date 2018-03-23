000100001113     H DECEDIT('0,') DATEDIT(*DMY.)
000200001113
000300030307     FAZORG00F  UP   E           K DISK
000400010205
000500030307     D WORGD15         S                   LIKE(ORGD15)
000600030307     D WORGD18         S                   LIKE(ORGD18)
000700030307     D WORGD25         S                   LIKE(ORGD25)
000800030307
000900030307     c                   eval      Worgd15 = ORGD15
001000030307     c                   eval      Worgd18 = ORGD18
001100030307     c                   eval      Worgd25 = ORGD25
001200030307
001300030307     c                   eval      ORGD15 = Worgd18
001400030307     c                   eval      ORGD18 = Worgd15
001500030307     c                   eval      ORGD29 = Worgd25
001600030307     c                   clear                   ORGD25
001700030307     c                   clear                   ORGFTR
001800030307
001900030307     C                   UPDATE    azorg
