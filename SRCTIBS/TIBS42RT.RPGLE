000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000201130930     D kpjba         e ds
000400131001     D tibs42ds      e ds
001800000607      *--------------------------------------------------------------------
001900000607     c
002000000607     c     *entry        plist
002100131001     c                   parm                    kpjba
002101131001     c                   clear                   tibs42ds
002102131001     c                   movel(p)  kpjbu         tibs42ds
002105131001     C                   call      'TIBS42R'
002106131001     C                   parm                    Tibs42Ds
002804130930     c                   eval      *inLR = *on
