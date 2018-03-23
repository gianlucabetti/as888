000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300140917      *------------------------------------------------------------
000400170705     ftabel00f  if   e           k disk
000500170623     ftfnlvp5l  if   e           k disk
000600141211     ftfnlvp11s uf   e             disk
000700140917     D kpjba         e ds
000800170717     D ds5a4         e ds
000900140917      *
001000140917     d dataiso         s               d   datfmt(*iso)
001100140917     d dataeur         s               d   datfmt(*eur)
001200170717     d oggiiso         s               d   datfmt(*iso)
001300000607      *--------------------------------------------------------------------
001400170705      *
001500170705     C     Ktab          KLIST
001600170705     C                   KFLD                    tblkut
001700170705     C                   KFLD                    tblcod
001800170705     C                   KFLD                    tblkey
001900170705      *
002000140917     c     *entry        plist
002100140917     c                   parm                    kpjba
002200140917      *
002300140930     c                   seton                                        lr
002400140930      *
002500140917      * Imposta la UDATE  da TIME
002600140917     C                   TIME                    W0140            14 0
002700140917     C                   MOVE      W0140         UDATE8            8 0
002800140917     c                   move      udate8        dataeur
002900140917     c                   move      dataeur       dataiso
003000170623     c                   move      dataiso       oggi              8 0
003100170717     c                   move      dataiso       oggiiso
003200170705      **
003300170705      * tabella 5a(4) per pulizie
003400170705     c                   eval      tblkut = 1
003500170705     c                   eval      tblcod = '5A'
003600170705     c                   eval      tblkey = '4'
003700170705     c                   clear                   ds5a4
003800170705     c     ktab          chain     tabel00f
003900170705     c                   if        %found(tabel00f)
004000170705     c                   movel     tbluni        ds5a4
004100170705     c                   end
004200170705      ***
004300170705     c                   if        §5A4sPDA = 999
004400170705     c                   return
004500170705     c                   endif
004600170717      **
004700170717      *  elimina tutto fino alla data ricavata da udate meno i gg in tabella
004800170717     c     oggiiso       subdur    §5A4sPDA:*d   dataiso
004900170705     c                   move      dataiso       alladata          8 0
005000170717      ***
005100170717      *  Imposta una data limite da tabella x verifica con le filiali inserite
005200170717      *   sul file per NON essere pulite oppure, se superato il tempo, debbano
005300170717      *  essere considerate come NON presenti sul file e seguono le regole di
005400170717      *   pulizia dello standard.
005500170717     c     oggiiso       subdur    §5A4sPDAF:*d  dataiso
005600170717     c                   move      dataiso       dataLimit         8 0
005700170623      **
005800141211     c     *start        setll     tfnlvp11s
005900170623      ***
006000141211     c                   do        *hival
006100141211     c                   read      tfnlvp11s
006200170623      ***
006300141211     c                   if        %eof(tfnlvp11s)
006400141211     c                   leave
006500141211     c                   endif
006600170705      ***  alla data
006700170705     c                   if        p11wddc >= alladata
006800170705     c                   iter
006900170705     c                   endif
007000170623      ***  x filiale AUT
007100170623     c                   movel     p11caut       filaut            3 0
007200170623     c     filaut        chain     tfnlvp5l
007300170705     c                   if        %Found(tfnlvp5l)
007400170717      ** occorre controllare quando è stato inserito il record della filiale
007500170717     c                   if        VP5DIMM >= dataLimit
007600170623     c                   iter
007700170717     c                   endif
007800170717     c                   end
007900170717      ***
008000141211     c                   delete    WFNLVP11
008100141211     c                   enddo
008200170623      ***
008300140930     c                   return
