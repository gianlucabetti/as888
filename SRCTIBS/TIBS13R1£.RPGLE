000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300040902     ffnblp01l  if   e             disk
000400021022     ftitas30c  if   e           k disk
000500021022     fqsysprt   o    f  132        printer infds(stamp)
000600021022     d stamp           ds
000700021022     d  line                 367    368b 0
000800000607      *--------------------------------------------------------------------
000900000607     c
001000021022     c     kbol          klist
001100040902     c                   kfld                    blpaas
001200040902     c                   kfld                    blplnp
001300040902     c                   kfld                    blpnrs
001400040902     c                   kfld                    blpnsp
001500021022     c                   z-add     99            line
001600021022
001700040902     c     *start        setll     fnblp01l
001800000607     c                   do        *hival
001900040902     c                   read      fnblp01l                               99
002000020904     c   99              leave
002100040902     c                   if        blpaas < 2004
002200021022     c     kbol          chain     titas30c                           98
002300021022     c                   if        *in98
002400021022     c                   if        line > 60
002500021022     c                   except    testa
002600021022     c                   endif
002700021105     c                   except    nontrovata
002800040803     c                   endif
002900021022     c                   endif
003000021022
003100000607     c                   enddo
003200000607     c                   seton                                        lr
003300021022     O*------------------------------------------------------*
003400021022     OQSYSPRT   e            TESTA             2
003500040803     O                                           70 'Bolle presenti in filiale -
003600040803     O                                              Partenza senza TITAS'
003700040803     o                                          + 8 'MERGEBOL'
003800021022
003900021022     O          e            nontrovata  1
004000040902     o                       blplnp        z    + 1
004100040902     o                       blpaas        z    + 1
004200040902     o                       blpnrs        z    + 1
004300040902     o                       blpnsp        z    + 1
004400040902     o                       blpcbo             + 1
004500040902     o                       blplna        z    + 1
004600040902     o                       blpksc        z    + 1
004700040902     o                       blprsm             + 1
