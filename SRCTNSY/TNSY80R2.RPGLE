000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*------------------------------------------------------------------------*
000300040414     Ftnsy80pf  o  A E             DISK
000400000000     C*------------------------------------------------------------------------*
000500000000     C* MAIN LINE
000600000000     C*------------------------------------------------------------------------*
000700040414     c     *entry        plist
000800040414     c                   parm                    sylib
000900040414     c                   parm                    syfile
001000040414     c                   parm                    symbr
001100040414     c                   parm                    sydata
001200040414     c
001300040414     c                   write     tnsy80
001400991118     C*
001500000000     C                   SETON                                        LR
