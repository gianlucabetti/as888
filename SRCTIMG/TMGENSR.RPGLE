000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400000103     Ftmmmp03l  if   e           k disk
000500000103     Ftmsmp02l  uf   e           k disk
000600000000     C*---------------------------------------------------------------*
000700000103     c                   exsr      tmsmp
000800990113     c                   seton                                        lr
000900991022     c*-------------------------------------------------
001000991022     c     tmsmp         begsr
001100991022     c*-------------------------------------------------
001200000103     c     ksmp          klist
001300000103     c                   kfld                    mmpaac
001400000103     c                   kfld                    mmpmat
001500000103     c     2000          setll     tmmmp03l
001600000103     c                   do        *hival
001700000103     c     2000          reade     tmmmp03l                               99
001800000103     c   99              leave
001900000103     c     ksmp          chain     tmsmp02l                           98
002000000103     c                   z-add     0             smpqgf
002100000103     c                   z-add     0             smpqgi
002200000103     c                   update    tmsmp000
002300000103     c                   enddo
002400991022     c                   endsr
