000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200140917      *------------------------------------------------------------
000300140917     ftabel00f  if   e           k disk
000400140917     D kpjba         e ds
000500140917     d ds5A4         e ds
000600140917      *
000700140917     d dataiso         s               d   datfmt(*iso)
000800140917     d dataeur         s               d   datfmt(*eur)
000900000607      *--------------------------------------------------------------------
001000140917     c     *entry        plist
001100140917     c                   parm                    kpjba
001200140917      *
001300140917     C     Ktab          KLIST
001400140917     C                   KFLD                    tblkut
001500140917     C                   KFLD                    tblcod
001600140917     C                   KFLD                    tblkey
001700140930      *
001800140930     c                   seton                                        lr
001900140930      *
002000140917      * Imposta la UDATE  da TIME
002100140917     C                   TIME                    W0140            14 0
002200140917     C                   MOVE      W0140         UDATE8            8 0
002300140917     c                   move      udate8        dataeur
002400140917     c                   move      dataeur       dataiso
002500140917      * tabella 5a per pulizie
002600140917     c                   eval      tblkut = 1
002700140917     c                   eval      tblcod = '5A'
002800140917     c                   eval      tblkey = '4'
002900140917     c                   clear                   ds5a4
003000141217     c     ktab          chain     tabel00f
003100140917     c                   if        %found(tabel00f)
003200140917     c                   movel     tbluni        ds5a4
003300140917     c                   end
003400140930     c                   if        §5A4nlvp = 999
003500140930     c                   return
003600140930     c                   endif
003700141217     c     dataiso       subdur    §5A4nlvp:*y   dataiso
003800140917     c                   move      dataiso       alladata          8 0
003900141217     c                   move      0101          alladata
004000141217     c
004100141217      *  elimina fino alla data ricavata da udate meno anno in corso e due anni precedenti completi
004200140917     C/EXEC SQL
004300141217     C+ delete from mzagi00f where agidtf < :alladata
004400140917     C/END-EXEC
004500141217     C/EXEC SQL
004600141217     C+ delete from mzimp00f where impdat < :alladata
004700141217     C/END-EXEC
004800141217     C/EXEC SQL
004900141217     C+ delete from mzigm00f where igmdat < :alladata
005000141217     C/END-EXEC
005100140917      *
005200140930     c                   return
