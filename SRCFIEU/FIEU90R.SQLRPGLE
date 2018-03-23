000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300030904      *       prompt di controllo dati ricevuti dal partner
000400000000      *****************************************************************
000500040826     fkfazn11l  if   E           k disk
000600041015     Ffieu90D   CF   E             WORKSTN
000700041015     F                                     SFILE(eu90ws1:NRR)
000800030116      *****************************************************************
000900030116     d KPJBA         E DS
001000030120     D Tibs36ds      E DS
001100000000     D CNCR80        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300940926     D*
001400000000     D WLBDAT          DS
001500940926     D  G02DAT                 1      8  0
001600940926     D  G02INV                 9     16  0
001700940926     D  G02ERR                17     17
001800940926     D  G02TGI                18     22  0
001900030205      *
002000041015     D  CodCC          s              2
002100041015     D  VarCC          s              1
002200041015     D  DesCC          s             20
002300041015      *
002400030116     D digits          C                   CONST('0123456789')
002500030116      *****************************************************************
002600000000     C     *ENTRY        PLIST
002700000000     C                   PARM                    KPJBA
002800041015     C                   movel     kpjbu         controlla         1
002900030120      *
003000030825     C* Imposta la UDATE  da TIME
003100030825     C                   TIME                    W0140            14 0
003200030825     C                   MOVE      W0140         UDATE8            8 0
003300030825     C*
003400030120      *  In base all'utente prende il P.O.
003500030120     C                   CLEAR                   Tibs36Ds
003600030120     c                   EVAL      I36ute = Knmus
003700030120     c                   EVAL      I36Tla = 'L'
003800030120     C                   CALL      'TIBS36R'
003900030120     C                   PARM                    Tibs36Ds
004000030827     c                   movel     KNMUS         ALFA3             3
004100030829     c                   setoff                                       0102
004200030829      * solo per EDP....:
004300030829     c                   if        ALFA3 ='EDP'
004400030829     c                   seton                                        01
004500030829     c                   end
004600030829      * per utenti di filiale:
004700030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
004800030829     c                   seton                                        02
004900030829     c                   end
005000000000      *--------------------------------------------------------------*
005100000000      *             M A I N      L I N E
005200000000      *--------------------------------------------------------------*
005300000000     C                   Z-ADD     1             CODUT
005400000000     C                   CALL      'X§PARUT'
005500000000     C                   PARM                    UT§DSE
005600000000     C                   MOVEL     RAGUT         RSUT             20
005700000000     C                   MOVEL     RAGUT         RSUT2            20
005800000000     C                   MOVEL     REC80         CNCR80
005900040819      *
006000041015     c                   exsr      imposta_sql
006100041015      *
006200041015      * Se deve essere chiamato a video
006300041015     c                   if        controlla = *blank
006400041015     c                   exsr      Video_sql
006500041015     c                   else
006600041015      *  se deve essere usato come controllo x altro pgm
006700041015     c                   clear                   kpjbu
006800041015     c                   clear                   tab_noOK          1
006900041015     c                   exsr      Ctrl_sql
007000041015      * se ci sono dei problemi imposta a "S" il primo flag della KPJBU
007100041015     c                   movel     tab_noOK      kpjbu
007200041015     c                   end
007300041015      *
007400000000     C     FINE          TAG
007500000000     C                   SETON                                        LR
007600040830      *--------------------------------------------------------------*
007700041015      *
007800041015      *--------------------------------------------------------------*
007900041015     c     imposta_SQL   begsr
008000041015      *
008100041015     C/EXEC SQL
008200041015     C+ Declare A1 Cursor for
008300041015     c+ SELECT TBLCOD as COD, substr(TBLKEY, 8, 1) as VARIA, substr(TBLUNI,
008400041015     c+ 1, 20) as DESCRIZIONE FROM tabel00f WHERE TBLFLG = ' ' and TBLCOD
008500041015     c+ ='CC' and substr( TBLKEY, 8, 1 ) not in (SELECT substr( EVAVAR, 1,
008600041015     c+ 1) FROM tneva00f where evaann= ' ')
008700041015     C/END-EXEC
008800041015      *          apertura cursore
008900041015     C/EXEC SQL
009000041015     C+ OPEN A1
009100041015     C/END-EXEC
009200041015      *
009300041015     c                   ENDsr
009400041015      *--------------------------------------------------------------*
009500041015     C     Legge_SQL     begSR
009600041015     C/EXEC SQL
009700041015     C+ Fetch Next From A1 Into :codCC, :varCC, :desCC
009800041015     C/END-EXEC
009900041015     C                   ENDSR
010000041015      *--------------------------------------------------------------*
010100041015     C     Chiude_SQL    begSR
010200041015     C/EXEC SQL
010300041015     C+ Close A1
010400041015     C/END-EXEC
010500041015     C                   ENDSR
010600041015      *--------------------------------------------------------------*
010700041015     c     video_sql     begsr
010800041015      *
010900041015      * video lista
011000041015     c                   exsr      ciclo_sql
011100041015      *
011200041015     C     INIZIO        tag
011300041015      *
011400041015     c                   write     eu90wz1
011500041015     c   34              write     eu90wn1
011600041015      *
011700041015     c                   dou       *inkl or *inkc
011800041015     c                   exfmt     eu90wc1
011900041015     C   KL              GOTO      INIZIO
012000041015     C   KC              GOTO      FINE_video
012100041015     c                   enddo
012200041015      *
012300041015     C                   GOTO      INIZIO
012400041015      *
012500041015     C     FINE_video    ENDSR
012600041015      *--------------------------------------------------------------*
012700041015     c     ctrl_sql      begsr
012800041015      *
012900041015     c                   exsr      Legge_SQL
013000041015     C                   SELECT
013100041015     C                   WHEN      SqlCod = 100
013200041015     c* x errori
013300041015     C                   WHEN      SqlCod < 0
013400041015     C                   seton                                        H2
013500041015     c                   goto      fine_ctrl
013600041015     c*
013700041015     C                   OTHER
013800041015     c*
013900041015     c                   eval      tab_noOK = 'S'
014000041015     c*
014100041015     C                   ENDSL
014200041015      *
014300041015     C     fine_ctrl     ENDSR
014400041015      *--------------------------------------------------------------*
014500041015     c     ciclo_sql     begsr
014600041015      *
014700041015     c                   seton                                        3334
014800041015     c                   write     eu90wc1
014900041015     c                   setoff                                       33
015000041015      *
015100041015     c                   z-add     0             nrr               5 0
015200041015     c                   z-add     1             srn               4 0
015300041015      *
015400041012     C                   DOU       SqlCod <> 0
015500041015      *
015600041015      *          lettura cursore
015700041015     c                   exsr      Legge_SQL
015800040830     C                   SELECT
015900040830     C                   WHEN      SqlCod = 100
016000041012     c* a fine file
016100041012     c                   leave
016200041012     c* x errori
016300041012     C                   WHEN      SqlCod < 0
016400041012     C                   seton                                        H1
016500041012     c                   goto      fine_ciclo
016600041012     c*
016700041012     C                   OTHER
016800041012     c*
016900041015     c                   eval      ws1var = varCC
017000041015     c                   eval      ws1des = desCC
017100041013      **
017200041013     c                   setoff                                       34
017300040830     c                   add       1             nrr
017400041015     c                   write     eu90ws1
017500040830      *
017600040830     C                   ENDSL
017700040830      *
017800040830     C                   ENDDO
017900041015      *
018000041015     C                   Exsr      chiude_SQL
018100040830     C*
018200041012     c     fine_ciclo    tag
018300040830     C                   ENDSR
018400041015      **********************************************************************
