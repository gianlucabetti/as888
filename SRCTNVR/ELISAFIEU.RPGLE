000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200070927     D kpjba         E DS
000300070927     D param           DS
000400070927     D   kAas                         4  0
000500070927     D   kLnp                         3  0
000600070927     D   kNrs                         2  0
000700070927     D   kNsp                         7  0
000800070927     D   kTpr                         3A
000900070927     D   kNfp                        15A
001000070927     D   kDfp                         8  0
001100070927     D   kGrp                         3A
001200070927     D   kImp                        13  3
001300070927      *
001400070927     c                   eval      kaas=2007
001500070927     c                   eval      klnp=360
001600070927     c                   eval      knrs=00
001700070927     c                   eval      knsp=1688
001800070927     c                   eval      kdfp=20070930
001900070927     c                   eval      kimp=10000
002000061115     c
002100070927     c                   movel     param         kpjbu
002200070927     c                   call      'FIEU79R1'
002300070927     c                   parm                    kpjba
002400020503     C*
002500020502     C                   SETON                                        LR
