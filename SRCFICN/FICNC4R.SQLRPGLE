000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300140917      *------------------------------------------------------------
000400140917     ftabel00f  if   e           k disk
000500140917     D kpjba         e ds
000600140917     d ds5A4         e ds
000700140917      *
000800140917     d dataiso         s               d   datfmt(*iso)
000900140917     d dataeur         s               d   datfmt(*eur)
001000000607      *--------------------------------------------------------------------
001100140917     c     *entry        plist
001200140917     c                   parm                    kpjba
001300140917      *
001400140917     C     Ktab          KLIST
001500140917     C                   KFLD                    tblkut
001600140917     C                   KFLD                    tblcod
001700140917     C                   KFLD                    tblkey
001800140930      *
001900140930     c                   seton                                        lr
002000140930      *
002100140917      * Imposta la UDATE  da TIME
002200140917     C                   TIME                    W0140            14 0
002300140917     C                   MOVE      W0140         UDATE8            8 0
002400140917     c                   move      udate8        dataeur
002500140917     c                   move      dataeur       dataiso
002600140917      * tabella 5a per pulizie
002700140917     c                   eval      tblkut = 1
002800140917     c                   eval      tblcod = '5A'
002900140917     c                   eval      tblkey = '4'
003000140917     c                   clear                   ds5a4
003100140917     c     ktab          chain(n)  tabel00f
003200140917     c                   if        %found(tabel00f)
003300140917     c                   movel     tbluni        ds5a4
003400140917     c                   end
003500140930     c                   if        §5a4fgts = 999
003600140930     c                   return
003700140930     c                   endif
003800140917     c     dataiso       subdur    §5A4Fgts:*d   dataiso
003900140917     c                   move      dataiso       alladata          8 0
004000140917      *     elimina tutto fino alla data ricavata da udate meno i giorni in tabella
004100140917     C/EXEC SQL
004200140917     C+ delete from FIfgts0F where fgtdst < :alladata
004300140917     C/END-EXEC
004400140917      *
004500140930     c                   return
