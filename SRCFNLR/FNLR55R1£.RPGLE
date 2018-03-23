000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300000608     ffnlr55d1  cf   e             workstn
000400000607     D kpjba         e ds
000500000607     D dslr01        e ds
000600000607      *--------------------------------------------------------------------
000700000607     c
000800000607     c     *entry        plist
000900000607     c                   parm                    kpjba
001000000607     c                   move      *zeros        v1cndd
001100000607
001200000607     c                   do        *hival
001300000607     c                   exfmt     video1
001400000607     c   kc              leave
001500000607     C     '?'           SCAN      V1CNDD                                 32
001600000607     c                   if        *in32
001700000607     c                   clear                   dslr01
001800000607     c                   move      '§'           dlrric
001900000607     c                   move      4             dlrnpg
002000000607     c                   z-add     0             dlrnfv
002100000607     c                   movel     dslr01        kpjbu
002200000607     c                   call      'FNLR02R'
002300000607     c                   parm                    kpjba
002400000607     c                   movel     kpjbu         dslr01
002500000607     c                   movel     dlrnfv        v1cndd
002600000607     c                   else
002700000608     c                   move      v1cndd        dlrnfv5           5 0
002800000608     c                   z-add     dlrnfv5       dlrnfv
002900000607     c                   endif
003000000607     c                   if        *inkf and dlrnfv > 0
003100000607     c                   movel(p)  dlrnfv        kpjbu
003200000608     c                   call      'FNLR55R2'
003300000607     c                   parm                    kpjba
003400000607     c                   leave
003500000607     c                   endif
003600000607     c                   enddo
003700000607     c                   seton                                        lr
