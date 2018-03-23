000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300140917      *------------------------------------------------------------
000400140917     ftabel00f  if   e           k disk
000500141211     ftfnlvp11s uf   e             disk
000600140917     D kpjba         e ds
000700141211     d ds5A5         e ds
000701161107     d ds5A6         e ds
000800140917      *
000900140917     d dataiso         s               d   datfmt(*iso)
001000140917     d dataeur         s               d   datfmt(*eur)
001100141211     D fila            S              3    DIM(29)
001101161107     D filb            S              3    DIM(29)
001200000607      *--------------------------------------------------------------------
001300140917     c     *entry        plist
001400140917     c                   parm                    kpjba
001500140917      *
001600140917     C     Ktab          KLIST
001700140917     C                   KFLD                    tblkut
001800140917     C                   KFLD                    tblcod
001900140917     C                   KFLD                    tblkey
002000140930      *
002100140930     c                   seton                                        lr
002200140930      *
002300140917      * Imposta la UDATE  da TIME
002400140917     C                   TIME                    W0140            14 0
002500140917     C                   MOVE      W0140         UDATE8            8 0
002600140917     c                   move      udate8        dataeur
002700140917     c                   move      dataeur       dataiso
002800140917      * tabella 5a per pulizie
002900140917     c                   eval      tblkut = 1
003000140917     c                   eval      tblcod = '5A'
003100141211     c                   eval      tblkey = '5'
003200141211     c                   clear                   ds5a5
003300141211     c     ktab          chain     tabel00f
003400140917     c                   if        %found(tabel00f)
003500141211     c                   movel     tbluni        ds5a5
003600140917     c                   end
003601161107     c                   eval      tblkey = '6'
003602161107     c                   clear                   ds5a6
003603161107     c     ktab          chain     tabel00f
003604161107     c                   if        %found(tabel00f)
003605161107     c                   movel     tbluni        ds5a6
003606161107     c                   end
003607161107      ***
003700141211     c                   if        §5A5nlvp = 999
003800140930     c                   return
003900140930     c                   endif
003901161107      ***
004000141211     c                   movea     §5AFILA       fila
004001161107      ***
004002161107     c                   movea     §6AFILA       filb
004003161107      ***
004100141211     c     dataiso       subdur    §5A5nlvp:*d   dataiso
004200140917     c                   move      dataiso       alladata          8 0
004300141211      *  elimina tutto fino alla data ricavata da udate meno i giorni in tabe
004400141211     c     *start        setll     tfnlvp11s
004500141211     c                   do        *hival
004600141211     c                   read      tfnlvp11s
004700141211     c                   if        %eof(tfnlvp11s)
004800141211     c                   leave
004900141211     c                   endif
005000141212     c                   if        p11wddc >= alladata
005100141212     c                   iter
005200141212     c                   endif
005300141211     c                   movel     p11caut       filaut            3
005400141211     c     filaut        lookup    fila                                   99
005401161107     c  n99filaut        lookup    filb                                   99
005500141211     c   99              iter
005600141211     c                   delete    WFNLVP11
005700141211     c                   enddo
005800140930     c                   return
