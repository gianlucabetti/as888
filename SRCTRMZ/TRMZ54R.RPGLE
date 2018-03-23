000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300140917      *------------------------------------------------------------
000400140918     fagip02l   uf   e           k disk
000500140917     D kpjba         e ds
000600140917      *
000700140917     d dataiso         s               d   datfmt(*iso)
000800140917     d dataeur         s               d   datfmt(*eur)
000900000607      *--------------------------------------------------------------------
001000140917     c     *entry        plist
001100140917     c                   parm                    kpjba
001200140917      *
001300140917      * Imposta la UDATE  da TIME
001400140917     C                   TIME                    W0140            14 0
001500140917     C                   MOVE      W0140         UDATE8            8 0
001600140917     c                   move      udate8        dataeur
001700140917     c                   move      dataeur       dataiso
001800140918     c     dataiso       subdur    2:*y          dataiso
001900140917     c                   move      dataiso       alladata          8 0
002000140918     c     'C'           setll     agip02l
002100140918     c                   do        *hival
002200140918     c     'C'           reade     agip02l
002300140918     c                   if        %eof(agip02l)
002400140918     c                   leave
002500140918     c                   endif
002600140918     c                   move      DATA_00001    datan             8 0
002700140918     c                   if        datan  > alladata
002800140918     c                   iter
002900140918     c                   endif
003000140918     c                   delete    agip
003100140918     c                   enddo
003200140917      *
003300000607     c                   seton                                        lr
