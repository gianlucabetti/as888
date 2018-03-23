000100990122     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990122     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850125     F* MPH305R *-MP/38-*--------------------------------------------*
000400000000     F*-----------------*                                            *
000500850125     F*       - LANCIO STAMPA SITUAZ. RIEPILOG. A VALORE MP. -       *
000600850125     F*       ------------------------------------------------       *
000700850125     F*                                                   AZN.: H305 *
000800990122     F*         FILE : MPH305D-tmamp01L-ANTAB00F-                    *
000900000000     F*                                                              *
001000000000     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001100850125     F*                RICDATE   - CONTROLLO INVERSIONE DATE         *
001200990317     F*                tii104R   - RICERCA ANAGRAF.PRODOTTI          *
001300990317     F*                tii108R   - RICERCA CODICI  DI MAGAZZINO      *
001400990317     F*                TII110R   - RICERCA CLASSE  MERCEOLOGICA      *
001500990317     F*                TII111R   - RICERCA S/CLASS.MERCEOLOGICA      *
001600990317     F*                TII116R   - RICERCA CODICI  LISTINI MP        *
001700990317     F*                tii002r   - RICERCA CAUSALI DI MAGAZZINO      *
001800000000     F*                                                              *
001900850125     F*         SBM. : AZN.-H306 - STAMPA RIEPIL. A VALORE MP.       *
002000000000     F*--------------------------------------------------------------*
002100990317     FTII207D   CF   E             WORKSTN
002200990122     Ftmamp01L  IF   E           K DISK
002300000000     FANTAB00F  IF   E           K DISK
002400850206     D ERR             S             30    DIM(9) CTDATA PERRCD(1)              *ERRORI
002500000000     D KPJBA         E DS
002600000000     D*----- PASSAGGIO PARAMETRI
002700990122     D  DAL305               247    254  0
002800990122     D  AL305                255    262  0
002900990125     D  MAG305               263    269
003000990125     D  CAU305               270    272
003100990125     D  LIS305               273    275
003200990125     D  COL305               276    276  0
003300990125     D  FOR305               277    277
003400990125     D  LSG305               278    280
003500990125     D  CLG305               281    281  0
003600990125     D  TIP305               282    282  0
003700990125     D  DET305               283    283
003800850311     D  CLS305               289    291
003900850311     D  SCL305               292    296
004000990125     D  MAT305               297    308
004100990125     d  desdit               433    452
004200000000     D  MSGERR               453    502
004300990125     D soc001        E DS                  EXTNAME(xsoc001ds)
004400990125     D xsocds          DS          1000
004500990125     d dataiso         s               d   datfmt(*iso)
004600990125     d dataeur         s               d   datfmt(*eur)
004700000000     C*----------------------------------------------------*
004800000000     C     *ENTRY        PLIST                                                  *
004900000000     C                   PARM                    KPJBA                          *
005000000000     C*---------- ANAGRAFICO MATERIE PRIME :               *
005100000000     C     KEYAMP        KLIST                                                  *
005200850125     C                   KFLD                    MAT305                         *
005300000000     C*---------- ANAGRAFICO TABELLE :                     *
005400000000     C     KEYTAB        KLIST                                                  *
005500000000     C                   KFLD                    TABGRU                         *
005600000000     C                   KFLD                    TABCOD                         *
005700990125     C*---------- RICERCA DITTA :
005800990125     C                   MOVEL     'SOC001'      TIPXSC
005900990125     C                   MOVEL     *blank        SOCXSC
006000990125     C                   EXSR      REPSOC
006100990125     C     RTNXSC        IFNE      '1'
006200990125     C                   MOVEL     XSOCDS        SOC001
006300990125     C                   MOVEL     xscrgs        desdit
006400990125     C                   MOVEL     xscsoc        wsoc              3
006500990125     c                   end
006600990125     c                   time                    como14           14 0
006700990125     c                   move      como14        woggi             8 0
006800990125     c                   move      woggi         dataeur
006900990125     c                   move      dataeur       dataiso
007000990125     c                   move      dataiso       woggig            8 0
007100000000     C*-----------                                         *
007200990125     C                   MOVE      woggi         AL                             *
007300990125     C                   MOVE      woggi         dal                            *
007400850125     C                   MOVEL     '0101'        DAL                            *
007500850125     C                   MOVE      *ZEROS        DAL305                         *
007600850125     C                   MOVE      *ZEROS        AL305                          *
007700871121     C                   MOVE      '1      '     MAG305                         *
007800000000     C                   MOVE      *BLANKS       DESMAG                         *
007900850311     C                   MOVE      '999'         CAU305                         *
008000871121     C                   MOVE      '1  '         LIS305                         *
008100871121     C                   MOVE      '1'           COL305                         *
008200850311     C                   MOVE      'N'           FOR305                         *
008300850128     C                   MOVE      *BLANKS       DESLIS                         *
008400850206     C                   MOVE      '999'         LSG305                         *
008500850206     C                   MOVE      *ZEROS        CLG305                         *
008600850206     C                   MOVE      *BLANKS       DESLSG                         *
008700850206     C                   MOVE      *BLANKS       DESCAU                         *
008800850125     C                   MOVE      *ZEROS        TIP305                         *
008900850206     C                   MOVE      'D'           DET305                         *
009000871016     C                   MOVEL     '?'           INTERR           12
009100870108     C                   MOVEL     '9999999'     NOVE
009200871016     C                   MOVE      '9999999'     NOVE             12
009300000000     C     INIZIO        TAG                                                    *
009400870108     C                   MOVE      INTERR        MAT305                         *
009500000000     C                   MOVE      *BLANKS       DESART                         *
009600850125     C                   MOVE      '?  '         CLS305                         *
009700850125     C                   MOVE      *BLANKS       DESCLS                         *
009800850125     C                   MOVE      '?    '       SCL305                         *
009900850125     C                   MOVE      *BLANKS       DESSCL                         *
010000850206     C*--------- CONTROLLI SU PROMPT ----------------------*
010100850206     C     EMISPT        TAG                                                    *
010200850206     C                   EXFMT     MPH305PT                                     *
010300990409     C   Kc              GOTO      FINE                                         *
010400850206     C                   MOVEA     *ZEROS        *IN(1)                         *
010500850206     C                   MOVE      *BLANKS       ERRORE                         *
010600850125     C*---------- ROVESCIO E CONTROLLO DATA DAL :          *
010700990125     c     *eur          test(d)                 dal                    90
010800990125     c                   if        *in90
010900990125     c     *dmy          test(d)                 dal                    90
011000990125     c   90              goto      emispt
011100990125     c     *dmy          move      dal           dataeur
011200990125     c                   else
011300990125     c                   move      dal           dataeur
011400990125     c                   endif
011500990125     c                   move      dataeur       dataiso
011600990125     c                   move      dataiso       dal305
011700850125     C*---------- ROVESCIO E CONTROLLO DATA AL :           *
011800990125     c     *eur          test(d)                 al                     90
011900990125     c                   if        *in90
012000990125     c     *dmy          test(d)                 al                     90
012100990125     c   90              goto      emispt
012200990125     c     *dmy          move      al            dataeur
012300990125     c                   else
012400990125     c                   move      al            dataeur
012500990125     c                   endif
012600990125     c                   move      dataeur       dataiso
012700990125     c                   move      dataiso       al305
012800000000     C*---------- MAGAZZINO :                              *
012900850125     C                   MOVE      *BLANKS       DESMAG                         *
013000850125     C     MAG305        IFNE      '9999999'                                    *
013100850125     C     MAG305        IFEQ      '?      '                                    *
013200850125     C                   MOVE      *BLANKS       MAG305                         *
013300000000     C                   MOVE      *BLANKS       DAT153                         *
013400990317     C                   CALL      'TII108R'                                    *
013500850125     C                   PARM                    MAG305                         *
013600000000     C                   PARM                    DAT153           75            *
013700000000     C                   MOVEL     DAT153        DESMAG                         *
013800850206     C                   SETON                                            82    *
013900000000     C                   GOTO      EMISPT                                       *
014000000000     C                   END                                                    *
014100000000     C                   MOVE      '153'         TABGRU                         *
014200000000     C                   MOVE      *ZEROS        TABCOD                         *
014300850125     C                   MOVE      MAG305        TABCOD                         *
014400000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
014500850206     C   90              SETON                                            82    *
014600850125     C   90              MOVEL     ERR(2)        ERRORE                         *
014700000000     C   90              GOTO      EMISPT                                       *
014800000000     C                   MOVEL     TABDES        DESMAG                         *
014900000000     C                   END                                                    *
015000850311     C*---------- CAUSALE ---------------------------------*
015100850311     C     CAU305        IFNE      '999'                                        *
015200850311     C     CAU305        IFEQ      '?  '                                        *
015300850311     C                   MOVE      'MP'          PROCAU            2            *
015400850311     C                   MOVE      *BLANKS       CAU305                         *
015500850311     C                   MOVE      *BLANKS       DESCAU                         *
015600850311     C                   MOVE      *BLANKS       AZICAU           15            *
015700990317     C                   CALL      'TII002R'                                    *
015800850311     C                   PARM                    KPJBA                          *
015900850311     C                   PARM                    PROCAU                         *
016000850311     C                   PARM                    CAU305                         *
016100850311     C                   PARM                    DESCAU                         *
016200850311     C                   PARM                    AZICAU                         *
016300850311     C                   SETON                                            83    *
016400850311     C                   GOTO      EMISPT                                       *
016500850311     C                   END                                                    *
016600850311     C                   MOVE      '152'         TABGRU                         *
016700850311     C                   MOVE      *ZEROS        TABCOD                         *
016800850311     C                   MOVEL     'MP'          CINQUE            5            *
016900850311     C                   MOVE      CAU305        CINQUE            5            *
017000850311     C                   MOVE      CINQUE        TABCOD                         *
017100850311     C     KEYTAB        CHAIN     ANTAB00F                           90        *
017200850311     C   90              MOVEL     ERR(3)        ERRORE                         *
017300850311     C   90              SETON                                            83    *
017400850311     C   90              GOTO      EMISPT                                       *
017500850311     C                   MOVEL     TABDES        DESCAU                         *
017600850311     C                   END                                                    *
017700850206     C*---------- CONTROLLO LISTINO X VALORIZZAZIONE ------*
017800850125     C     LIS305        IFNE      '999'                                        *
017900850125     C     LIS305        IFEQ      '?  '                                        *
018000850125     C                   MOVE      *BLANKS       LIS305                         *
018100850125     C                   MOVE      *BLANKS       DS161            75            *
018200990317     C                   CALL      'TII116R'                                    *
018300850125     C                   PARM                    LIS305                         *
018400850125     C                   PARM                    DS161                          *
018500850128     C                   MOVEL     DS161         DESLIS                         *
018600850311     C                   SETON                                            84    *
018700850128     C                   GOTO      EMISPT                                       *
018800850125     C                   END                                                    *
018900850125     C                   MOVE      '161'         TABGRU                         *
019000850125     C                   MOVE      *ZEROS        TABCOD                         *
019100850125     C                   MOVE      LIS305        TABCOD                         *
019200850125     C     KEYTAB        CHAIN     ANTAB00F                           90        *
019300850206     C  N90              MOVEL     TABDES        DESLIS                         *
019400850125     C  N90COL305        COMP      0                                    9090    *
019500850311     C   90              SETON                                            84    *
019600850311     C   90              MOVEL     ERR(4)        ERRORE                         *
019700850125     C   90              GOTO      EMISPT                                       *
019800850125     C                   ELSE                                                   *
019900850125     C                   MOVE      *ZEROS        COL305                         *
020000850125     C                   END                                                    *
020100850206     C*---------- CONTROLLO LISTINO X GENERAZIONE ---------*
020200850206     C     LSG305        IFNE      '999'                                        *
020300850206     C     LSG305        IFEQ      '?  '                                        *
020400850206     C                   MOVE      *BLANKS       LSG305                         *
020500850206     C                   MOVE      *BLANKS       DS161                          *
020600990317     C                   CALL      'TII116R'                                    *
020700850206     C                   PARM                    LSG305                         *
020800850206     C                   PARM                    DS161                          *
020900850206     C                   MOVEL     DS161         DESLSG                         *
021000850311     C                   SETON                                            85    *
021100850206     C                   GOTO      EMISPT                                       *
021200850206     C                   END                                                    *
021300850206     C                   MOVE      '161'         TABGRU                         *
021400850206     C                   MOVE      *ZEROS        TABCOD                         *
021500850206     C                   MOVE      LSG305        TABCOD                         *
021600850206     C     KEYTAB        CHAIN     ANTAB00F                           90        *
021700850206     C  N90              MOVEL     TABDES        DESLSG                         *
021800850206     C  N90CLG305        COMP      0                                    9090    *
021900850311     C   90              SETON                                            85    *
022000850311     C   90              MOVEL     ERR(4)        ERRORE                         *
022100850206     C   90              GOTO      EMISPT                                       *
022200850206     C                   ELSE                                                   *
022300850206     C                   MOVE      *ZEROS        CLG305                         *
022400850206     C                   END                                                    *
022500850206     C*---------- TIPO SELEZIONE --------------------------*
022600850128     C     TIP305        COMP      0                                    9090    *
022700850206     C   90              MOVEL     ERR(5)        ERRORE                         *
022800850206     C   90              SETON                                            86    *
022900850128     C   90              GOTO      EMISPT                                       *
023000850206     C*---------- DETTAGLIO O TOTALE ----------------------*
023100850206     C     DET305        COMP      *BLANKS                                90    *
023200850206     C   90              MOVEL     ERR(6)        ERRORE                         *
023300850206     C   90              SETON                                            87    *
023400850206     C   90              GOTO      EMISPT                                       *
023500850128     C*---------- PRODOTTO --------------------------------*
023600850125     C     TIP305        IFEQ      1                                            *
023700850128     C                   MOVE      '?  '         CLS305                         *
023800850128     C                   MOVE      *BLANKS       DESCLS                         *
023900850128     C                   MOVE      '?    '       SCL305                         *
024000850128     C                   MOVE      *BLANKS       DESSCL                         *
024100870108     C     MAT305        IFEQ      INTERR                                       *
024200850125     C                   MOVE      *BLANKS       MAT305                         *
024300990317     C                   CALL      'TII104R'                                    *
024400850125     C                   PARM                    MAT305                         *
024500000000     C                   PARM                    DESART                         *
024600850206     C                   SETON                                            88    *
024700850125     C                   GOTO      EMISPT                                       *
024800000000     C                   END                                                    *
024900870108     C     MAT305        COMP      NOVE                                   90    *
025000990122     C  N90KEYAMP        CHAIN     tmamp01L                           90        *
025100850206     C   90              SETON                                            88    *
025200850206     C   90              MOVEL     ERR(7)        ERRORE                         *
025300000000     C   90              GOTO      EMISPT                                       *
025400850125     C                   GOTO      LANCIO                                       *
025500850125     C                   END                                                    *
025600850125     C*---------- CLASSE MERCEOLOGICA ---------------------*
025700850125     C     TIP305        IFEQ      2                                            *
025800870108     C                   MOVE      INTERR        MAT305                         *
025900850128     C                   MOVE      *BLANKS       DESART                         *
026000850128     C                   MOVE      '?    '       SCL305                         *
026100850128     C                   MOVE      *BLANKS       DESSCL                         *
026200850125     C     CLS305        CABEQ     '999'         LANCIO                         *
026300850125     C     CLS305        IFEQ      '?      '                                    *
026400850125     C                   MOVE      *BLANKS       CLS305                         *
026500000000     C                   MOVE      *BLANKS       DESCLS                         *
026600990317     C                   CALL      'TII110R'                                    *
026700850125     C                   PARM                    CLS305                         *
026800000000     C                   PARM                    DESCLS                         *
026900850206     C                   SETON                                            89    *
027000000000     C                   GOTO      EMISPT                                       *
027100000000     C                   END                                                    *
027200000000     C                   MOVE      '155'         TABGRU                         *
027300000000     C                   MOVE      *ZEROS        TABCOD                         *
027400850125     C                   MOVE      CLS305        TABCOD                         *
027500000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
027600850206     C   90              SETON                                            89    *
027700850206     C   90              MOVEL     ERR(8)        ERRORE                         *
027800000000     C   90              GOTO      EMISPT                                       *
027900850125     C                   GOTO      LANCIO                                       *
028000850125     C                   END                                                    *
028100850125     C*---------- S/CLASSE MERCEOLOGICA -------------------*
028200850125     C     TIP305        IFEQ      3                                            *
028300870108     C                   MOVE      INTERR        MAT305                         *
028400850128     C                   MOVE      *BLANKS       DESART                         *
028500850128     C                   MOVE      '?  '         CLS305                         *
028600850128     C                   MOVE      *BLANKS       DESCLS                         *
028700850125     C     SCL305        CABEQ     '99999'       LANCIO                         *
028800850125     C     SCL305        IFEQ      '?      '                                    *
028900850125     C                   MOVE      *BLANKS       SCL305                         *
029000000000     C                   MOVE      *BLANKS       DESSCL                         *
029100990317     C                   CALL      'TII111R'                                    *
029200850125     C                   PARM                    SCL305                         *
029300000000     C                   PARM                    DESSCL                         *
029400850206     C                   SETON                                            91    *
029500000000     C                   GOTO      EMISPT                                       *
029600000000     C                   END                                                    *
029700000000     C                   MOVE      '156'         TABGRU                         *
029800000000     C                   MOVE      *ZEROS        TABCOD                         *
029900850125     C                   MOVE      SCL305        TABCOD                         *
030000000000     C     KEYTAB        CHAIN     ANTAB00F                           90        *
030100850206     C   90              SETON                                            91    *
030200850206     C   90              MOVEL     ERR(9)        ERRORE                         *
030300000000     C   90              GOTO      EMISPT                                       *
030400850125     C                   GOTO      LANCIO                                       *
030500850125     C                   END                                                    *
030600850125     C                   GOTO      INIZIO                                       *
030700000000     C*---------- LANCIO STAMPA ---------------------------*
030800000000     C     LANCIO        TAG                                                    *
030900990317     C                   MOVE      'I208'        KCOAZ                          *
031000900109     C                   MOVEL     'KSDIFFE'     KQEBI                          *
031100000000     C                   CALL      'BCH09'                                      *
031200000000     C                   PARM                    KPJBA                          *
031300000000     C                   CALL      'BCH10'                                      *
031400000000     C                   PARM                    KPJBA                          *
031500000000     C                   GOTO      INIZIO                                       *
031600000000     C*----------------------------------------------------*
031700000000     C     FINE          TAG                                                    *
031800000000     C   90              MOVEL     'KPJB207'     KMSMN                          *
031900000000     C                   SETON                                            LR    *
032000990125     C*----------------------------------------------------*
032100990125     C* Reperimento dati società
032200990125     C*----------------------------------------------------*
032300990125     C     REPSOC        BEGSR
032400990125     C*
032500990125     C                   CALLB     'XSOC'
032600990125     C                   PARM                    TIPXSC            6
032700990125     C                   PARM                    SOCXSC            3
032800990125     C                   PARM                    CDSXSC            9 0
032900990125     C                   PARM                    MODXSC            3
033000990125     C                   PARM      *blanks       RTNXSC            1
033100990125     C                   PARM                    XSOCDS
033200990125     C                   PARM                    KPJBA
033300990125     C*
033400990125     C                   ENDSR
033500000000     C*----------------------------------------------------*
033600850206**    ERR                       ERRORI
033700850125*- Date ERRATE o non VALIDE -*  Err,1
033800850125*- Cod. Magazzino ERRATO !! -*  Err,2
033900850311*- Cod. CAUSALE      ERRATO -*  Err,3
034000850311*- Cod. Listino/Col. ERRATO -*  Err,4
034100850206*- Inserire Tipo STAMPA  !! -*  Err,5
034200850206*- Inserire DETT/TOTALE  !! -*  Err,6
034300850206*- Cod. Prodotto  ERRATO !! -*  Err,7
034400850206*- Cod. Classe    ERRATO !! -*  Err,8
034500850206*- Cod. S/Classe  ERRATO !! -*  Err,9
