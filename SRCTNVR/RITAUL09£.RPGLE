000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200100630     D trul09ds      E DS
000300100630     c                   clear                   Trul09ds
000400100630     c                   movel     'DEF'         i09mod
000500100630     c                   movel     '055'         i09tfa
000600100630     c                   movel     20100621      i09dta
000700100630     c                   call      'TRUL09R'
000800100630     c                   parm                    trul09ds
000900020503     C*
001000020502     C                   SETON                                        LR
