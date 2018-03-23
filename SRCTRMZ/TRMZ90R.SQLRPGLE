000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200031028     F*          passaggio dati da PC ad AS x FAIS         *
000300111216     Fmzgad01l  uf a E           K DISK    commit
000400120112     fqsysprt   o    f  132        printer oflind(*inof)
000500070925     d COMDTA1         s                   like(gaddTA)
000600070925     d COMDTA2         s                   like(gaddTA)
000700070730     d kpjba         e ds
000800111216     d mzgadpcds     e ds                  extname(mzgadpcf)
000900111216     c*
001000111216     c* verifico il periodo
001100111216     c*
001200111216     C/EXEC SQL
001300111216     C+ SELECT min( PCDTA ), max( PCDTA ) INTO :comdta1, :comdta2 FROM
001400111216     C+ mzgadpcf
001500111216     C/END-EXEC
001600111216     c*
001700111216     c* pulisco nel caso abbia già ricevuto
001800111216     c                   exsr      srdel
001900111216     c*
002000111216     c* inserisco i record
002100111216     c*
002200111216     C/EXEC SQL
002300111219     C+ DECLARE A1 CURSOR FOR SELECT * FROM mzgadpcf WHERE PCORA <> 0
002400111216     C/END-EXEC
002500111216
002600111216     C/EXEC SQL
002700111216     C+ OPEN A1
002800111216     C/END-EXEC
002900111216
003000111216     C                   dou       *inlr
003100111216
003200111216     C/EXEC SQL
003300111216     C+ FETCH NEXT FROM A1 INTO :mzgadpcds
003400111216     C/END-EXEC
003500111216
003600111216     C                   select
003700111216     C                   when      sqlcod = 100
003800111216     C                   seton                                        lr
003900111216     C                   when      sqlcod = 0
004000111216     c* dettaglio
004100111216     c                   exsr      srwrite
004200111216     C                   other
004300111216     C                   exsr      sqlerror
004400111216     C                   endsl
004500111216     C*
004600111216     C                   enddo
004700111216
004800111216     C/EXEC SQL
004900111216     C+ CLOSE A1
005000111216     C/END-EXEC
005100111216     c                   commit
005200120112     C                   EXSR      STAMPA
005300111216     C**********************************************************************
005400111216     c* scrive record MZgad00F
005500111216     C**********************************************************************
005600111216     c     srwrite       begsr
005700111216     c                   clear                   mzgad000
005800111216     c* società
005900111219     c                   MOVE      pcsoc         GADSOC
006000111216     c* nome e cognome dipendente
006100111216     c                   eval      gaddec = pcdec
006200111216     c* codice dipendente
006300111216     c                   eval      gadmat = pcmat
006400111216     c* data assenza
006500111216     c                   eval      gaddta = pcdta
006600111216     c* causale
006700111216     c                   eval      gadcau = pccau
006800111216     c* ora
006900111219     c                   move      pcora         com02             2 2
007000111219     c                   movel     pcora         com20             2 0
007100111219     c                   if        com02 <> 0
007200111219     c                   eval      gadora = com02 * 60 / 100
007300111219     c                   end
007400111219     c                   add       com20         gadora
007500111216     C*
007600111216     c     kgad          chain     mzgad01l
007700111216     c                   if        not %found(mzgad01l)
007800111216     c                   write     mzgad000
007900111216     c                   end
008000111216     c                   endsr
008100070730     C**********************************************************************
008200070904     C     srdel         begsr
008300070730     C**********************************************************************
008400070904     C/EXEC SQL
008500070904     C+ DELETE FROM MZGAD00F where
008600070925     C+ gaddta between :COMDTA1 and :comdta2
008700070904     C/END-EXEC
008800070730     C*
008900070904     c                   endsr
009000111216     C**********************************************************************
009100111216     C     sqlerror      begsr
009200111216     C**********************************************************************
009300111216     C*
009400111216     C                   call      'X66CHGJOB'
009500111216     C                   seton                                        lr
009600111216     C                   ENDSR
009700120112     C**********************************************************************
009800120112     c* stampa errori
009900120112     C**********************************************************************
010000120112     C     stampa        BEGSR
010100141028     c* verifico se ho elaborato il periodo richiesto
010200141028     c*
010300141028     C/EXEC SQL
010400141028     C+ SELECT min(gaddta ), max(gaddta ) INTO :comdta1, :comdta2 FROM
010500141028     C+ mzgad00f
010600141028     C/END-EXEC
010700141028     c*
010800120112     c*
010900120112     c                   except    testa
011000120112     c                   except    fine
011100120112     c*
011200120112     C                   ENDSR
011300020709     C**********************************************************************
011400020709     C     *inzsr        BEGSR
011500020709     C**********************************************************************
011600031106     c     *entry        plist
011700070730     c                   parm                    kpjba
011800111216     C     Kgad          KLIST                                                  *
011900111216     C                   KFLD                    gadsoc                         *
012000111216     C                   KFLD                    gadmat                         *
012100111216     C                   KFLD                    gaddta                         *
012200111216     C                   KFLD                    gadcau                         *
012300020709     C                   ENDSR
012400120112     OQSYSPRT   E            TESTA          2 02
012500120112     O                                         + 35 'RILEVAZIONI PRESENZE'
012600120112     O                                          113 'TRMZ90R'
012700120112     O                       UDATE              127 '  /  /  '
012800120112     O                       PAGE1         Z    132
012900141028     O          e            fine        1
013000141028     o                                         +  1 'Data inizio rilevaz.:'
013100141028     o                       comdta1           +  1
013200141028     o                                         +  1 'Data fine rilevaz.:'
013300141028     o                       comdta2           +  1
013400141028     O          e            fine        1
013500120112     O                                         + 50 '*** NESSUNA ANOMALIA ***'
