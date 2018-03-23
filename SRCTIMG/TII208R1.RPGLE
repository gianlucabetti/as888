000100990125     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990125     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850201     F* MPH306R1 *-MP/38-*-------------------------------------------*
000400850201     F*------------------*                                           *
000500850201     F*       - ESEGUE STAMPA RIEPILOGO A VALORE -MP-                *
000600850201     F*       ---------------------------------------                *
000700850201     F*                                                   AZN.: H306 *
000800990125     F*         FILE : MPPRTSTD-tmpil00F-tmmmp01L-ANTAB00F           *
000900990125     F*                tmlmp01L                                      *
001000850201     F*                                                              *
001100850201     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001200850201     F*--------------------------------------------------------------*
001300850201     F*     IMPOSTAZIONE CHIAVI PILOTA :                             *
001400850201     F*             PILK01 = MAGAZZINO                  'XXXXXXX   ' *
001500850311     F*             PILK02 = CLASSE E/O S/CLASSE        'XXX  XXXXX' *
001600850201     F*             PILK03 = CODICE PRODOTTO            'XXXXXXX   ' *
001700850201     F*             PILK04 = VERSIONE                   'XX        ' *
001800850201     F*             PILFIL = DESCRIZIONE PRODOTTO E UMA 'XXXX.. XXX' *
001900850206     F*--------------------------------------------------------------*
002000850206     F*     07  -   STAMPA DETTAGLIO                                 *
002100850206     F*     08  -   STAMPA TOTALE                                    *
002200850206     F*     09  -   AGGIORNAMENTO LISTINO                            *
002300850201     F*--------------------------------------------------------------*
002400990125     Ftmpil00F  IP   E           K DISK
002500850201     FANTAB00F  IF   E           K DISK
002600990125     Ftmmmp01L  IF   E           K DISK
002700990125     Ftmlmp01L  UF A E           K DISK
002800850206     FMPPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
002900850206     F                                     USROPN
003000850204     D AZ              S              1    DIM(15)                              *AZ.CAUSALE   CIT
003100990420     D PZM             S             15  4 DIM(10)                              *PREZZO MEDIO
003200990420     D QPM             S             15  4 DIM(10)                              *QTA X PRZ. MEDIO
003300990420     D VPM             S             15  4 DIM(10)                              *VAL.X PRZ. MEDIO
003400850311     D EUW             S              8  1 DIM(10)                              *E/U     STAMPA
003500850311     D PMW             S              8  0 DIM(10)                              *PRZ.MED.STAMPA
003600850311     D VMW             S              8  1 DIM(10)                              *VAL.MED.STAMPA
003700990420     D EU              S             15  4 DIM(10)                              *E/U  QUANTITA
003800990420     D EUV             S             15  4 DIM(10)                              *E/U  VERSIONE
003900990420     D QMV             S             15  4 DIM(10)                              *QTA X MED.VERSIO
004000990420     D VMV             S             15  4 DIM(10)                              *VAL X MED.VERSIO
004100990420     D EUP             S             15  4 DIM(10)                              *E/U  PRODOTTO
004200990420     D QMP             S             15  4 DIM(10)                              *QTA X MED.PRODOT
004300990420     D VMP             S             15  4 DIM(10)                              *VAL X MED.PRODOT
004400990420     D EUC             S             15  4 DIM(10)                              *E/U  CLASSE
004500990420     D QMC             S             15  4 DIM(10)                              *QTA X MED.CLASSE
004600990420     D VMC             S             15  4 DIM(10)                              *VAL X MED.CLASSE
004700990420     D EUS             S             15  4 DIM(10)                              *E/U  S/CLASSE
004800990420     D QMS             S             15  4 DIM(10)                              *QTA X MED.S/CLAS
004900990420     D VMS             S             15  4 DIM(10)                              *VAL X MED.S/CLAS
005000990420     D EUM             S             15  4 DIM(10)                              *E/U  MAGAZZINO
005100990420     D QMM             S             15  4 DIM(10)                              *QTA X MED.MAGAZ.
005200990420     D VMM             S             15  4 DIM(10)                              *VAL.X MED.MAGAZ.
005300990420     D EUG             S             15  4 DIM(10)                              *E/U  GENERALE
005400990420     D QMG             S             15  4 DIM(10)                              *QTA X MED.GENER.
005500990420     D VMG             S             15  4 DIM(10)                              *VAL.X MED.GENER.
005600850318     D SCA             S              3    DIM(100)                             *TAB. CAUSALI ER.
005700850318     D SCF             S             10    DIM(100)                             *TAB. FLAG OPERAT
005800850314     D ERR             S             50    DIM(1) CTDATA PERRCD(1)              *ERRORI
005900850201     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 1
006000850201     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 2
006100850201     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 3
006200850201     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 4
006300850201     D SK5             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 5
006400850201     D SK6             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 6
006500850201     D KEY151          DS
006600850201     D*------------- CHIAVE TABELLA 151 :
006700850201     D  PROCED                 1      2
006800850204     D  ENUS                   3      3
006900850201     D  DESPRC                 1      3
007000850201     D DESENT          DS
007100850201     D*------------- DESCRIZIONI COLONNE ENTRATE :
007200850201     D  DE1                    1      9
007300850201     D  DE2                   10     18
007400850201     D  DE3                   19     27
007500850201     D  DE4                   28     36
007600850201     D  DE5                   37     45
007700850201     D DESUSC          DS
007800850201     D*------------- DESCRIZIONI COLONNE USCITE :
007900850201     D  DU1                    1      9
008000850201     D  DU2                   10     18
008100850201     D  DU3                   19     27
008200850201     D  DU4                   28     36
008300850201     D  DU5                   37     45
008400850318     D FLGOPE          DS
008500850318     D*------------- FLAG OPERATIVI DI CAUSALE :
008600850318     D  FO                     1     10
008700850318     D                                     DIM(10)                              *FLAG OPERATIVI
008800850201     D TB153         E DS                  EXTNAME(ANTAB153)
008900990125     D RECLIS        E DS                  EXTNAME(tmlmp00F)
009000990125     D  PZL                   29     68P 5
009100850204     D                                     DIM(5)                               *PREZZI LISTINO
009200850201     D                 DS
009300850201     D KPJBA         E DS
009400850201     D*----- PASSAGGIO PARAMETRI
009500990125     D  DAL305               247    254  0
009600990125     D  AL305                255    262  0
009700990125     D  MAG305               263    269
009800990125     D  CAU305               270    272
009900990125     D  LIS305               273    275
010000990125     D  COL305               276    276  0
010100990125     D  FOR305               277    277
010200990125     D  LSG305               278    280
010300990125     D  CLG305               281    281  0
010400990125     D  TIP305               282    282  0
010500990125     D  DET305               283    283
010600990125     D  CLS305               289    291
010700990125     D  SCL305               292    296
010800990125     D  MAT305               297    308
010900990125     d  desdit               433    452
011000990125     D  MSGERR               453    502
011100990125     d dataiso         s               d   datfmt(*iso)
011200990125     d dataeur         s               d   datfmt(*eur)
011300990125     d*---------------------------------------------------------------------
011400990125     Itmpil000      01
011500850201     I                                          PILK01        L4
011600850201     I                                          PILK02        L3
011700850201     I                                          PILK03        L2
011800850201     I                                          PILK04        L1
011900850206     C/EJECT
012000850201     C*----------------------------------------------------*
012100850201     C     *ENTRY        PLIST                                                  *
012200850201     C                   PARM                    KPJBA                          *
012300990125     c                   setoff                                       90
012400850201     C*---------- MOVIMENTI DI MAGAZZINO :                 *
012500850201     C*                                                    *
012600850201     C     KEYMMP        KLIST                                                  *
012700990125     C                   KFLD                    AA                4 0          *
012800850201     C                   KFLD                    MAGAZZ            7            *
012900871016     C                   KFLD                    PRODOT           12            *
013000850201     C*---------- ANAGRAFICO TABELLE :                     *
013100850201     C     KEYTAB        KLIST                                                  *
013200850201     C                   KFLD                    TABGRU                         *
013300850201     C                   KFLD                    TABCOD                         *
013400850206     C*---------- ANAGRAFICO LISTINI X VALORIZZ.           *
013500850201     C     KEYLMP        KLIST                                                  *
013600850201     C                   KFLD                    LIS305                         *
013700850201     C                   KFLD                    PRODOT                         *
013800850206     C*---------- ANAGRAFICO LISTINI X GENERAZ.            *
013900850206     C     KEYLSG        KLIST                                                  *
014000850206     C                   KFLD                    LSG305                         *
014100871016     C                   KFLD                    MATKKK           12            *
014200850405     C*---------- PRIMO CICLO -----------------------------*
014300850405     C   L4
014400850405     CANN02              DO                                                     *
014500850314     C                   Z-ADD     0             PAGE              4 0          *
014600850201     C*---------- ENTRATE/USCITE :                         *
014700850201     C                   MOVE      'MP'          PROCED                         *
014800850201     C                   MOVE      '151'         TABGRU                         *
014900850201     C                   MOVE      *ZEROS        TABCOD                         *
015000850204     C                   MOVE      'E'           ENUS                           *
015100850201     C                   MOVE      DESPRC        TABCOD                         *
015200850201     C     KEYTAB        CHAIN     ANTAB00F                           91        *
015300850201     C   91              MOVE      *ALL'*'       DESENT                         *
015400850201     C  N91              MOVEL     TABDES        DESENT                         *ENTRATE
015500850204     C                   MOVE      'U'           ENUS                           *
015600850201     C                   MOVE      DESPRC        TABCOD                         *
015700850201     C     KEYTAB        CHAIN     ANTAB00F                           91        *
015800850201     C   91              MOVE      *ALL'*'       DESUSC                         *
015900850201     C  N91              MOVEL     TABDES        DESUSC                         *USCITE
016000850206     C*---------- TIPO SELEZIONE --------------------------*
016100850201     C     TIP305        COMP      1                                      04    * X PRODOTTO
016200850311     C     TIP305        COMP      2                                      05    * X CLASSE+S/CL
016300850201     C     TIP305        COMP      3                                      06    * X S/CLASSE
016400850206     C*---------- TIPO STAMPA -----------------------------*
016500850206     C     DET305        IFNE      'N'                                          *NOSTAMPA
016600850206     C                   OPEN      MPPRTSTD                                     *
016700850206     C     DET305        COMP      'D'                                    07    *
016800850206     C     DET305        COMP      'T'                                    08    *
016900850206     C   07              SETON                                            08    *
017000850207     C                   SETON                                            11    *
017100850206     C                   END                                                    *
017200850201     C*---------- INVERSIONE DATA DAL :                    *
017300990125     C                   MOVE      DAL305        DATAiso                        *
017400990125     C                   move      dataiso       dataeur                        *
017500990125     C                   move      DATAeur       DATDAL            8 0          *
017600850201     C*---------- INVERSIONE DATA AL  :                    *
017700990125     C                   MOVE      AL305         DATAiso                        *
017800990125     C                   move      dataiso       dataeur                        *
017900990125     C                   move      DATAeur       DATAL             8 0          *
018000850206     C*---------- RICERCA LISTINO X VALORIZZAZIONE --------*
018100850201     C     LIS305        IFNE      '999'                                        *
018200850201     C                   MOVE      '161'         TABGRU                         *
018300850201     C                   MOVE      *ZEROS        TABCOD                         *
018400850201     C                   MOVE      LIS305        TABCOD                         *
018500850201     C     KEYTAB        CHAIN     ANTAB00F                           91        *
018600850201     C   91              MOVE      *ALL'*'       DESLIS           25            *
018700850201     C  N91              MOVEL     TABDES        DESLIS                         *
018800850206     C  N91              SETON                                            09    *
018900850201     C                   ELSE                                                   *
019000850201     C                   MOVE      *ALL'*'       DESLIS                         *
019100850201     C                   END                                                    *
019200850206     C*---------- RICERCA LISTINO X GENERAZIONE -----------*
019300850206     C     LSG305        IFNE      '999'                                        *
019400850206     C                   MOVE      '161'         TABGRU                         *
019500850206     C                   MOVE      *ZEROS        TABCOD                         *
019600850206     C                   MOVE      LSG305        TABCOD                         *
019700850206     C     KEYTAB        CHAIN     ANTAB00F                           91        *
019800850206     C   91              MOVE      *ALL'*'       DESLSG           25            *
019900850206     C  N91              MOVEL     TABDES        DESLSG                         *
020000850206     C  N91              SETON                                            10    *
020100850206     C                   ELSE                                                   *
020200850206     C                   MOVE      *ALL'*'       DESLSG                         *
020300850206     C                   END                                                    *
020400850318     C*---------- CARICAMENTO TABELLA CAUSALI E FLAG ------*
020500850318     C                   MOVEA     *BLANKS       SCA                            *
020600850318     C                   MOVEA     *BLANKS       SCF                            *
020700850318     C                   MOVE      '152'         TABGRU                         *
020800850318     C     TABGRU        SETLL     ANTAB00F                           91        *
020900850318     C   91              GOTO      ENDTAB                                       *
021000850318     C     SUTAB         TAG                                                    *
021100850318     C     TABGRU        READE     ANTAB00F                               91    *FINE FILE
021200850318     C   91              GOTO      ENDTAB                                       *
021300850318     C                   MOVE      TABCOD        CODICE            5            *
021400850318     C                   MOVEL     CODICE        PROCE2            2            *
021500850318     C     PROCE2        IFEQ      'MP'                                         *
021600850318     C     T             ADD       1             T                 3 0          *
021700850318     C                   MOVE      TABCOD        SCA(T)                         *
021800850403     C                   MOVE      TABDES        SCF(T)                         *
021900850318     C     T             CABEQ     100           ENDTAB                         *
022000850318     C                   END                                                    *
022100850318     C                   GOTO      SUTAB                                        *
022200850318     C     ENDTAB        TAG                                                    *
022300850318     C*-----------                                         *
022400850201     C                   SETON                                            02    *
022500850201     C                   END                                                    *
022600850405     C*---------- ROTTURA DI MAGAZZINO --------------------*
022700850405     C   L4              DO                                                     *
022800850405     C*---------- TOTALE  DI MAGAZZINO :                   *
022900870108     C   30              EXSR      TOTVER                                       *
023000850405     C   26              EXSR      TOTPRD                                       *
023100850405     C   27
023200850405     CANN04              EXSR      TOTSCL                                       *
023300850405     C   25
023400850405     CAN 05              EXSR      TOTCLS                                       *
023500850405     C   28              EXSR      TOTMAG                                       *
023600850405     C                   SETOFF                                           28    *
023700850405     C*---------- RICERCA MAGAZZINO -----------------------*
023800850405     C                   Z-ADD     0             PAGE2             4 0          *
023900850405     C                   Z-ADD     52            PAGE1             3 0          *
024000850405     C                   MOVE      'XXX'         OLDCLS            3            *
024100850417     C                   MOVE      'XXXXX'       OLDSCL            5            *
024200850201     C                   MOVEL     PILK01        MAGAZZ                         *
024300850201     C                   MOVEL     DAL305        AA                             *
024400850201     C                   MOVE      '153'         TABGRU                         *
024500850201     C                   MOVE      *ZEROS        TABCOD                         *
024600850201     C                   MOVE      MAGAZZ        TABCOD                         *
024700850201     C     KEYTAB        CHAIN     ANTAB00F                           91        *
024800850201     C   91              MOVEL     *ALL'*'       DESMAG           30            *
024900850201     C  N91              MOVEL     TABDES        TB153                          *
025000850201     C  N91              MOVEL     DES153        DESMAG                         *
025100850201     C  N91              MOVE      TIG153        TIGS              2            *
025200850201     C  N91              MOVE      TIM153        TIMS              1            *
025300990318     C  N91              MOVEl     CDC153        CDCS              8            *
025400850201     C                   END                                                    *
025500850405     C*---------- ROTTURA DI S/CLASSE-CLASSE --------------*
025600850201     C   L3
025700850201     CANN04              DO                                                     *
025800850405     C*---------- TOTALE  DI S/CLASSE :                    *
025900850405     C   30              EXSR      TOTVER                                       *
026000850405     C   26              EXSR      TOTPRD                                       *
026100850405     C                   MOVE      PILK02        SCL               5            *
026200850405     C                   MOVEL     PILK02        CLS               3            *
026300850405     C   27SCL           IFNE      OLDSCL                                       *
026400850405     C                   EXSR      TOTSCL                                       *
026500850405     C                   END                                                    *
026600850417     C                   MOVE      SCL           OLDSCL                         *
026700850405     C*---------- TOTALE  DI CLASSE :                      *
026800850417     C   05CLS           IFNE      OLDCLS                                       *
026900850417     C   27              EXSR      TOTSCL                                       *
027000850417     C   25              EXSR      TOTCLS                                       *
027100850405     C                   Z-ADD     52            PAGE1                          *
027200850405     C                   END                                                    *
027300850417     C                   MOVE      CLS           OLDCLS                         *
027400850405     C*---------- RICERCA S/CLASSE ------------------------*
027500850405     C   05                                                                     *
027600850405     COR 06              DO                                                     *
027700850405     C                   MOVE      '156'         TABGRU                         *
027800850405     C                   MOVE      *ZEROS        TABCOD                         *
027900850405     C                   MOVE      SCL           TABCOD                         *
028000850405     C     KEYTAB        CHAIN     ANTAB00F                           91        *
028100850405     C   91              MOVE      *ALL'*'       DESSCL           25            *
028200850405     C  N91              MOVEL     TABDES        DESSCL                         *
028300850418     C                   MOVE      SCL           KSCL              5            *
028400850405     C                   END                                                    *
028500850201     C*---------- RICERCA CLASSE --------------------------*
028600850201     C   05              DO                                                     *
028700850201     C                   MOVE      '155'         TABGRU                         *
028800850201     C                   MOVE      *ZEROS        TABCOD                         *
028900850201     C                   MOVE      CLS           TABCOD                         *
029000850201     C     KEYTAB        CHAIN     ANTAB00F                           91        *
029100850201     C   91              MOVE      *ALL'*'       DESCLS           25            *
029200850201     C  N91              MOVEL     TABDES        DESCLS                         *
029300850418     C                   MOVE      CLS           KCLS              3            *
029400850201     C                   END                                                    *
029500850201     C                   END                                                    *
029600850201     C*---------- ROTTURA DI PRODOTTO ---------------------*
029700850201     C   L2              DO                                                     *
029800850405     C*---------- TOTALE DI PRODOTTO :                     *
029900850405     C   30              EXSR      TOTVER                                       *
030000850405     C   26              EXSR      TOTPRD                                       *
030100850205     C                   MOVEL     PILK03        PRODOT                         *
030200850314     C                   Z-ADD     0             CTLVER            3 0          *
030300850201     C                   END                                                    *
030400850201     C*---------- ROTTURA DI VERSIONE ---------------------*
030500850201     C   L1              DO                                                     *
030600850405     C*---------- TOTALE DI VERSIONE :                     *
030700850405     C   30              EXSR      TOTVER                                       *
030800850314     C                   MOVEL     PILFIL        DESART           30            *
030900850314     C                   MOVE      PILFIL        UMA               3            *
031000850318     C                   MOVE      '999'         OLDCAU            3            *
031100850206     C*---------- RICERCA LISTINO X VALORIZZAZIONE :       *
031200850201     C                   Z-ADD     0             PREZZO           11 2          *
031300850311     C                   Z-ADD     0             PRZLIS           11 2          *
031400850206     C   09              DO                                                     *
031500850201     C                   Z-ADD     COL305        COL               1 0          *
031600990125     C     KEYLMP        CHAIN     tmlmp01L                           91        *
031700850311     C  N91              Z-ADD     PZL(COL)      PRZLIS                         *
031800850201     C                   END                                                    *
031900850405     C                   SETOFF                                       30        *
032000850201     C                   END                                                    *
032100850201     C*---------- DETTAGLIO DI PRODOTTO/VERSIONE ----------*
032200990125     C     KEYMMP        SETLL     tmmmp01L                           91  92    *
032300850207     C   92              DO                                                     *
032400850207     C     RICMOV        TAG                                                    *
032500990125     C     KEYMMP        READE     tmmmp01L                               91    *
032600850207     C   91              GOTO      FINE                                         *
032700850207     C*---------- CONTROLLO DATA MOVIMENTO :               *
032800850207     C     MMPDMV        CABLT     DAL305        RICMOV                         *
032900850207     C     MMPDMV        CABGT     AL305         RICMOV                         *
033000850207     C*---------- CONTROLLO CAUSALE :                      *
033100850207     C     CAU305        IFNE      '999'                                        *
033200850207     C     CAU305        CABNE     MMPCAU        RICMOV                         *
033300850207     C                   END                                                    *
033400850318     C*---------- CONTROLLO FLAG OPERATIVI :               *
033500850318     C     MMPCAU        IFNE      OLDCAU                                       *
033600850318     C                   Z-ADD     1             T                              *
033700850318     C     MMPCAU        LOOKUP    SCA(T)                                 14    *
033800850318     C   14              MOVE      SCF(T)        FLGOPE                         *
033900850318     C                   MOVE      MMPCAU        OLDCAU                         *
034000850318     C                   END                                                    *
034100850318     C     FOR305        IFEQ      'S'                                          *SI FORZATURA
034200850318     C                   MOVE      'L'           FO(1)                          *DA LISTINO
034300850318     C                   END                                                    *
034400850318     C     FO(1)         IFEQ      'L'                                          *PREZZO DA LIST
034500850318     C                   Z-ADD     PRZLIS        PREZZO                         *
034600850318     C                   ELSE                                                   *
034700850318     C                   Z-ADD     MMPPRZ        PREZZO                         *PREZZO DA MOV.
034800850318     C                   END                                                    *
034900850207     C*---------- IMPOSTO AZIONI CAUSALE ------------------*
035000850403     C                   MOVEA     MMPOPC        AZ                             *
035100850207     C     1             DO        10            X                 3 0          *
035200850207     C     AZ(X)         COMP      '+'                                    41    *   +
035300850207     C     AZ(X)         COMP      '-'                                    42    *   -
035400850207     C   41EUV(X)        ADD       MMPQTA        EUV(X)                         *VERSIONE
035500850207     C   42EUV(X)        SUB       MMPQTA        EUV(X)                         *VERSIONE
035600850314     C*---------- MEMOR. QTA/VAL. X MEDIO -----------------*
035700850319     C     PREZZO        COMP      0                                  1414      *
035800850319     C     FO(2)         COMP      'S'                                    15    *SI X MEDIO
035900850319     C   14                                                                     *
036000850319     COR 15              DO                                                     *
036100990420     C     PREZZO        MULT      MMPQTA        VALORE           15 4          *
036200850314     C   41QMV(X)        ADD       MMPQTA        QMV(X)                         *QTA VERSIONE
036300850314     C   42QMV(X)        SUB       MMPQTA        QMV(X)                         *
036400850314     C   41VMV(X)        ADD       VALORE        VMV(X)                         *VAL.VERSIONE
036500850314     C   42VMV(X)        SUB       VALORE        VMV(X)                         *
036600850314     C                   END                                                    *
036700850319     C                   END                                                    *
036800850319     C*----------------------------------------------------*
036900850319     C                   MOVE      PRODOT        MATKKK                         *
037000850405     C                   SETON                                        302829    *
037100850314     C                   GOTO      RICMOV                                       *
037200850314     C                   END                                                    *
037300850314     C*-----------                                         *
037400850201     C     FINE          TAG                                                    *
037500850201     C   90              MOVEL     'KPJB207'     KMSMN                          *
037600850201     C   90              SETON                                            LR    *
037700850405     C*----------- TOTALI ---------------------------------*
037800850405     CLRN90
037900850405     CAN 30              EXSR      TOTVER                                       *VERSIONE
038000850405     CLRN90
038100850405     CAN 26              EXSR      TOTPRD                                       *PRODOTTO
038200850405     CLRN90
038300850405     CAN 27
038400850405     CAN 05                                                                     *
038500850405     CORN90
038600850405     CAN 27
038700850405     CAN 06              EXSR      TOTSCL                                       *S/CLASSE
038800850405     CLRN90
038900850405     CAN 25
039000850405     CAN 05              EXSR      TOTCLS                                       *CLASSE
039100850405     CLRN90
039200850405     CAN 28              EXSR      TOTMAG                                       *MAGAZZINO
039300850201     C*----------- TOTALI GENERALI ------------------------*
039400850207     CLRN90
039500850207     CAN 29              DO                                                     *
039600850311     CLR                 SETON                                            36    *
039700850207     CLR                 EXSR      TOTALE                                       *
039800850314     CLR 11              EXSR      RUTSTA                                       *
039900850207     CLR                 Z-ADD     0             VMG                            *
040000850207     CLR                 END                                                    *
040100850405     C***------------------------------------------------***
040200850405     C**--------- GENERA TOTALE VERSIONE ----------------***
040300850405     C***------------------------------------------------***
040400850405     CSR   TOTVER        BEGSR                                                  *
040500850405     C                   SETON                                            31    *
040600850405     C     CTLVER        ADD       1             CTLVER                         *
040700870519     C                   EXSR      TOTALE                                       *
040800870519     C*     11             EXSR RUTSTA                     *
040900850405     C                   Z-ADD     0             EUV                            *
041000850405     C                   Z-ADD     0             QMV                            *
041100850405     C                   Z-ADD     0             VMV                            *
041200850405     C   10              EXSR      RUTLIS                                       *
041300850405     C                   SETOFF                                         3031    *
041400850405     C                   SETON                                            26    *
041500850405     CSR                 ENDSR                                                  *
041600850405     C***------------------------------------------------***
041700850405     C**--------- GENERA TOTALE PRODOTTO ----------------***
041800850405     C***------------------------------------------------***
041900850405     CSR   TOTPRD        BEGSR                                                  *
042000850405     C                   SETON                                            32    *
042100850405     C                   EXSR      TOTALE                                       *
042200850405     C   07
042300850405     CAN 11CTLVER        IFGT      1                                            *
042400870519     C                   EXSR      RUTSTA                                       *
042500850405     C                   END                                                    *
042600850405     C                   Z-ADD     0             EUP                            *
042700850405     C                   Z-ADD     0             QMP                            *
042800850405     C                   Z-ADD     0             VMP                            *
042900850405     C                   SETOFF                                         2632    *
043000850405     C                   SETON                                          27      *
043100850405     CSR                 ENDSR                                                  *
043200850405     C***------------------------------------------------***
043300850405     C**--------- GENERA TOTALE S/CLASSE ----------------***
043400850405     C***------------------------------------------------***
043500850405     CSR   TOTSCL        BEGSR                                                  *
043600850405     C                   SETON                                            33    *
043700850405     C   05              SETON                                            06    *
043800870519     C                   EXSR      TOTALE                                       *
043900850405     C   11              EXSR      RUTSTA                                       *
044000850405     C                   Z-ADD     0             EUS                            *
044100850405     C                   Z-ADD     0             QMS                            *
044200850405     C                   Z-ADD     0             VMS                            *
044300850405     C   05              SETOFF                                           06    *
044400850405     C                   SETOFF                                         2733    *
044500850405     C   05              SETON                                            25    *
044600850405     CSR                 ENDSR                                                  *
044700850405     C***------------------------------------------------***
044800850405     C**--------- GENERA TOTALE CLASSE ------------------***
044900850405     C***------------------------------------------------***
045000850405     CSR   TOTCLS        BEGSR                                                  *
045100850405     C                   SETON                                            33    *
045200850405     C                   EXSR      TOTALE                                       *
045300870519     C   11              EXSR      RUTSTA                                       *
045400850405     C                   Z-ADD     0             EUC                            *
045500850405     C                   Z-ADD     0             QMC                            *
045600850405     C                   Z-ADD     0             VMC                            *
045700850405     C                   SETOFF                                         2533    *
045800850405     CSR                 ENDSR                                                  *
045900850405     C***------------------------------------------------***
046000850405     C**--------- GENERA TOTALE MAGAZZINO ---------------***
046100850405     C***------------------------------------------------***
046200850405     CSR   TOTMAG        BEGSR                                                  *
046300850405     C                   SETON                                            35    *
046400850405     C                   EXSR      TOTALE                                       *
046500850405     C   11              EXSR      RUTSTA                                       *
046600850405     C                   Z-ADD     0             VMM                            *
046700850405     C                   SETOFF                                           35    *
046800850405     CSR                 ENDSR                                                  *
046900850201     C***------------------------------------------------***
047000850201     C**--------- EMISSIONE TOTALE ----------------------***
047100850201     C***------------------------------------------------***
047200850201     CSR   TOTALE        BEGSR                                                  *
047300850207     C                   Z-ADD     0             PZM                            *
047400850207     C                   Z-ADD     0             QPM                            *
047500850207     C                   Z-ADD     0             VPM                            *
047600990420     C                   Z-ADD     0             ENTRAT           15 4          *
047700990420     C                   Z-ADD     0             USCITE           15 4          *
047800850311     C                   Z-ADD     0             EUW                            *
047900850204     C                   Z-ADD     0             GIACEN            9 1          *
048000850311     C                   Z-ADD     0             PMW                            *
048100850204     C                   Z-ADD     0             PRZGIA            8 0          *
048200990420     C                   Z-ADD     0             PRZLSG           15 4          *PREZZO X LIST.
048300850311     C                   Z-ADD     0             VMW                            *
048400850204     C                   Z-ADD     0             TOTVAL            9 1          *
048500850205     C*--------- TOTALE DI VERSIONE :                      *VERSIONE
048600850205     C   31              DO                                                     *
048700850311     C                   Z-ADD     EUV           EU                             *
048800850311     C                   Z-ADD     QMV           QPM                            *
048900850311     C                   Z-ADD     VMV           VPM                            *
049000850311     C*-------------- SOMMO IN TOTALI X PRODOTTO :         *
049100850205     C     EUP           ADD       EUV           EUP                            *PRO.QTA.TOTALE
049200850205     C     QMP           ADD       QMV           QMP                            *PRO.QTA.X MED.
049300850205     C     VMP           ADD       VMV           VMP                            *PRO.VAL.X MED.
049400850311     C*-------------- SOMMO IN TOTALE X S/CLASSE :         *
049500850311     C     EUS           ADD       EUV           EUS                            *PRO.QTA.TOTALE
049600850311     C     QMS           ADD       QMV           QMS                            *PRO.QTA.X MED.
049700850311     C     VMS           ADD       VMV           VMS                            *PRO.VAL.X MED.
049800850311     C*-------------- SOMMO IN TOTALE X   CLASSE :         *
049900850311     C     EUC           ADD       EUV           EUC                            *PRO.QTA.TOTALE
050000850311     C     QMC           ADD       QMV           QMC                            *PRO.QTA.X MED.
050100850311     C     VMC           ADD       VMV           VMC                            *PRO.VAL.X MED.
050200850311     C*-------------- SOMMO IN TOTALE X   MAGAZZ.:         *
050300850311     C     EUM           ADD       EUV           EUM                            *PRO.QTA.TOTALE
050400850311     C     QMM           ADD       QMV           QMM                            *PRO.QTA.X MED.
050500850311     C     VMM           ADD       VMV           VMM                            *PRO.VAL.X MED.
050600850311     C*-------------- SOMMO IN TOTALE GENERALE :           *
050700850311     C     EUG           ADD       EUV           EUG                            *PRO.QTA.TOTALE
050800850311     C     QMG           ADD       QMV           QMG                            *PRO.QTA.X MED.
050900850311     C     VMG           ADD       VMV           VMG                            *PRO.VAL.X MED.
051000850205     C                   END                                                    *
051100850312     C***------------------------------------------------***
051200850312     C**--------- EMISSIONE TOTALE -------------------2--***
051300850312     C***------------------------------------------------***
051400850205     C*--------- TOTALE DI PRODOTTO :                      *PRODOTTO
051500850205     C   32              DO                                                     *
051600850205     C                   Z-ADD     EUP           EU                             *
051700850205     C                   Z-ADD     QMP           QPM                            *
051800850205     C                   Z-ADD     VMP           VPM                            *
051900850205     C                   END                                                    *
052000850205     C*--------- TOTALE DI CLASSE O S/CLASSE :             *CLASSE/SCL.
052100850311     C   33
052200850311     CAN 06              DO                                                     *
052300850311     C                   Z-ADD     EUS           EU                             *
052400850311     C                   Z-ADD     QMS           QPM                            *
052500850311     C                   Z-ADD     VMS           VPM                            *
052600850205     C                   END                                                    *
052700850311     C   33
052800850311     CAN 05
052900850311     CANN06              DO                                                     *
053000850311     C                   Z-ADD     EUC           EU                             *
053100850311     C                   Z-ADD     QMC           QPM                            *
053200850311     C                   Z-ADD     VMC           VPM                            *
053300850311     C                   END                                                    *
053400850205     C*--------- TOTALE DI MAGAZZINO                       *MAGAZZINO
053500850311     C   35              DO                                                     *
053600850311     C                   Z-ADD     EUM           EU                             *
053700850311     C                   Z-ADD     QMM           QPM                            *
053800850205     C                   Z-ADD     VMM           VPM                            *
053900850205     C                   END                                                    *
054000850318     C*--------- TOTALE GENERALE :                         *GENERALE
054100850318     C   36              DO                                                     *
054200850318     C                   Z-ADD     EUG           EU                             *
054300850318     C                   Z-ADD     QMG           QPM                            *
054400850318     C                   Z-ADD     VMG           VPM                            *
054500850318     C                   END                                                    *
054600850205     C***------------------------------------------------***
054700850312     C**--------- EMISSIONE TOTALE -------------------3--***
054800850205     C***------------------------------------------------***
054900850204     C*---------- ENTRATE :                                *
055000850204     C     1             DO        5             X                              *
055100850204     C     ENTRAT        ADD       EU(X)         ENTRAT                         *
055200850204     C                   END                                                    *
055300850204     C*---------- USCITE  :                                *
055400850204     C     6             DO        10            X                              *
055500850204     C     USCITE        ADD       EU(X)         USCITE                         *
055600850204     C                   END                                                    *
055700850204     C*---------- SALDO GIACENZA :                         *
055800990420     C     ENTRAT        SUB       USCITE        GIAC             15 4          *
055900850204     C                   Z-ADD     GIAC          GIACEN                         *
056000850311     C                   Z-ADD     EU            EUW                            *QTA STAMPA
056100850204     C*---------- GENERAZIONE MEDIO -----------------------*
056200850311     C     1             DO        10            X                              *
056300850204     C     QPM(X)        IFNE      0                                            *
056400850204     C     VPM(X)        DIV       QPM(X)        PZM(X)                         *
056500850204     C                   ELSE                                                   *
056600850204     C                   Z-ADD     0             PZM(X)                         *
056700850204     C                   END                                                    *
056800850314     C     PZM(X)        ADD       ,50           PMW(X)                         *
056900850204     C                   END                                                    *
057000850204     C*---------- VALORE GIACENZA :                        *
057100990420     C                   Z-ADD     0             PIPPO            15 4          *
057200850311     C     VPM           DIV       1000          VMW                            *VAL. STAMPA
057300850204     C     1             DO        5             X                              *
057400850204     C     PIPPO         ADD       VPM(X)        PIPPO                          *
057500850204     C                   END                                                    *
057600850204     C     6             DO        10            X                              *
057700850204     C     PIPPO         SUB       VPM(X)        PIPPO                          *
057800850204     C                   END                                                    *
057900850204     C     PIPPO         DIV       1000          TOTVAL                         *TOT. VALORE
058000850204     C*---------- PREZZO MEDIO GIACENZA :                  *
058100850204     C     GIAC          IFNE      0                                            *
058200990420     C     PIPPO         DIV       GIAC          PRZGIW           15 4          *
058300850318     C     PRZGIW        ADD       ,50           PRZGIW                         *
058400850318     C                   Z-ADD     PRZGIW        PRZGIA                         *
058500850315     C     PIPPO         DIV       GIAC          PRZLSG                         *
058600850204     C                   ELSE                                                   *
058700850204     C                   Z-ADD     0             PRZGIA                         *
058800850206     C                   Z-ADD     0             PRZLSG                         *
058900850204     C                   END                                                    *
059000850201     CSR                 ENDSR                                                  *
059100850405     C**--------- RUTINE DI STAMPA ----------------------***
059200850405     C***------------------------------------------------***
059300850405     CSR   RUTSTA        BEGSR                                                  *
059400850405     C     PAGE1         IFGE      50                                           *
059500850405     C                   EXCEPT    TESTA                                        *
059600850405     C                   Z-ADD     0             PAGE1                          *
059700850405     C                   END                                                    *
059800850405     C                   EXCEPT    TESTA1                                       *
059900850405     C   07
060000850405     CAN 31                                                                     *
060100850405     CORN31              EXCEPT    DETAIL                                       *
060200850405     CSR                 ENDSR                                                  *
060300850405     C**--------- GENERAZIONE LISTINO -------------------***
060400850405     C***------------------------------------------------***
060500850405     CSR   RUTLIS        BEGSR                                                  *
060600990125     C     KEYLSG        CHAIN     tmlmp01L                           99        *
060700850405     C   99              DO                                                     *
060800850405     C                   MOVE      ' '           LMPATT                         *
060900990125     c                   z-add     0             lmpute
061000990125     C                   time                    como14           14 0          *
061100990125     c                   move      como14        lmpdig
061200990125     c                   move      lmpdig        dataeur
061300990125     c                   move      dataeur       dataiso
061400990125     c                   move      dataiso       lmpdig
061500850405     C                   Z-ADD     0             LMPDFG                         *
061600850405     C                   MOVE      MATKKK        LMPMAT                         *
061700850405     C                   Z-ADD     0             PZL                            *
061800850405     C                   MOVE      *BLANKS       LMPFLG                         *
061900850405     C                   END                                                    *
062000850405     C                   Z-ADD     CLG305        CL                1 0          *
062100850405     C                   Z-ADD     PRZLSG        PZL(CL)                        *
062200990125     C   99              WRITE     tmlmp000                                     *
062300990125     C  N99              UPDATE    tmlmp000                                     *
062400850405     CSR                 ENDSR                                                  *
062500850201     O*--------------------------------------------------------------*
062600850201     O*     TESTATA                                                  *
062700850201     O*--------------------------------------------------------------*
062800850204     OMPPRTSTD  E            TESTA            01
062900850204     O                       SK1                132
063000850204     O                       DESDIT              20
063100850204     O                       UDATE         Y    118
063200850204     O                       PAGE          Z    132
063300850204     O          E            TESTA       1
063400850204     O                       SK2                132
063500990125     O                       DATDAL              89 '  /  /    '
063600990125     O                       DATAL              101 '  /  /    '
063700850204     O          E            TESTA       1
063800850204     O                       SK3                132
063900850204     O                       KNMUS              132
064000850204     O          E            TESTA       1
064100850204     O                                            4 'Pag.'
064200850314     O                       PAGE2         Z      9
064300850204     O                                           41 'Magazzino :'
064400850204     O                       MAGAZZ              49
064500850204     O                       DESMAG              80
064600850204     O                       TIGS                84
064700850204     O                       TIMS                86
064800850204     O                                           94 'CdC.:'
064900990318     O                       CDCS               103
065000850206     O          E            TESTA       1
065100850314     O               05                           9 'Classe  :'
065200850418     O               05      KCLS                15
065300850314     O               05      DESCLS              41
065400850314     O               06N05                        9 'S/Classe:'
065500850418     O               06N05   KSCL                15
065600850314     O               06N05   DESSCL              41
065700850206     O               09                          94 'Listino :'
065800850206     O               09      LIS305              99
065900850206     O               09      DESLIS             125
066000850206     O               09                         131 'Col.:'
066100850206     O               09      COL305             132
066200850314     O          E            TESTA       1
066300850418     O               05                           9 'S/Classe:'
066400850418     O               05      KSCL                15
066500850418     O               05      DESSCL              41
066600850314     O               10                          94 'Lis/Gen.:'
066700850314     O               10      LSG305              99
066800850314     O               10      DESLSG             125
066900850314     O               10                         131 'Col.:'
067000850206     O               10      CLG305             132
067100850314     O          E            TESTA       1
067200850314     O                       SK4                132
067300850314     O          E            TESTA       1
067400850314     O                       SK5                132
067500850314     O                       DE1                 19
067600850314     O                       DE2                 31
067700850314     O                       DE3                 42
067800850314     O                       DE4                 53
067900850314     O                       DE5                 64
068000850314     O                       DU1                 76
068100850314     O                       DU2                 87
068200850314     O                       DU3                 98
068300850314     O                       DU4                109
068400850314     O                       DU5                119
068500850314     O                                          131 ' GIACENZA '
068600850314     O          E            TESTA       1
068700850314     O                       SK5                132
068800850201     O*--------------------------------------------------------------*
068900850314     O*     TESTATA PRODOTTO/CLASSE/S.CLASSE/MAGAZZINO/TOT.FINALE    *
069000850201     O*--------------------------------------------------------------*
069100850405     O          E            TESTA1      1
069200850405     O                       PAGE1                9
069300850405     O                                            9 '*********'
069400871016     O               31                          21 'Prodotto XXXXXXXXXXXX'
069500871016     O               31      PRODOT              21
069600850314     O               31      DESART              54
069700871016     O               32                          21 'Tot.PROD.            '
069800871016     O               32      PRODOT              21
069900850314     O               32      DESART              54
070000850405     O               33 05N06                    32 'Totale Classe :  XXX:'
070100850418     O               33 05N06KCLS                31
070200850405     O               33 05N06DESCLS              65
070300850405     O               33 06                       32 'Tot. S/Classe: XXXXX:'
070400850418     O               33 06   KSCL                31
070500850405     O               33 06   DESSCL              65
070600850405     O               35                          32 'Totale Mag.: XXXXXXX:'
070700850405     O               35      MAGAZZ              31
070800850405     O               35      DESMAG              70
070900850405     O               36                          32 'Totale  GENERALE    :'
071000850314     O          E            TESTA1      0  0
071100871016     O               31      PRODOT              21
071200850314     O               31      DESART              54
071300870108     O               32                          21 'Tot.Prod.           :'
071400871016     O               32      PRODOT              21
071500850314     O               32      DESART              54
071600850405     O               33 05N06                    32 'Totale Classe :  XXX:'
071700850418     O               33 05N06KCLS                31
071800850405     O               33 05N06DESCLS              65
071900850405     O               33 06                       32 'Tot. S/Classe: XXXXX:'
072000850418     O               33 06   KSCL                31
072100850405     O               33 06   DESSCL              65
072200850405     O               35                          32 'Totale Mag.: XXXXXXX:'
072300850405     O               35      MAGAZZ              31
072400850405     O               35      DESMAG              70
072500850405     O               36                          32 'Totale  GENERALE    :'
072600850405     O          E    33      TESTA1      0  0
072700850405     O         OR    35
072800850405     O         OR    36
072900850405     O                                            9 '*********'
073000850405     O               33 05N06                    32 'Totale Classe :  XXX:'
073100850418     O               33 05N06KCLS                31
073200850405     O               33 05N06DESCLS              65
073300850405     O               33 06                       32 'Tot. S/Classe: XXXXX:'
073400850418     O               33 06   KSCL                31
073500850405     O               33 06   DESSCL              65
073600850405     O               35                          32 'Totale Mag.: XXXXXXX:'
073700850405     O               35      MAGAZZ              31
073800850405     O               35      DESMAG              70
073900850405     O               36                          32 'Totale  GENERALE    :'
074000850314     O*--------------------------------------------------------------*
074100850314     O*     DETTAGLIO   GIACENZA                                     *
074200850314     O*--------------------------------------------------------------*
074300850311     O          E            DETAIL      1
074400850405     O                       PAGE1              132
074500850204     O                       SK6                132
074600850314     O                                            5 'Q.tà:'
074700850314     O               31      UMA                  8
074800850314     O               32      UMA                  8
074900850311     O                       EUW(1)              20 '       , -'
075000850311     O                       EUW(2)              31 '       , -'
075100850311     O                       EUW(3)              42 '       , -'
075200850311     O                       EUW(4)              53 '       , -'
075300850311     O                       EUW(5)              65 '       , -'
075400850311     O                       EUW(6)              76 '       , -'
075500850311     O                       EUW(7)              87 '       , -'
075600850311     O                       EUW(8)              98 '       , -'
075700850311     O                       EUW(9)             109 '       , -'
075800850311     O                       EUW(10)            120 '       , -'
075900850204     O                       GIACEN             132 '      0 , -'
076000850311     O          E            DETAIL      1
076100850405     O                       PAGE1              132
076200850201     O                       SK6                132
076300850201     O                                            6 'MEDIO:'
076400850311     O                       PMW(1)              19 '        -'
076500850311     O                       PMW(2)              30 '        -'
076600850311     O                       PMW(3)              41 '        -'
076700850311     O                       PMW(4)              52 '        -'
076800850311     O                       PMW(5)              64 '        -'
076900850311     O                       PMW(6)              75 '        -'
077000850311     O                       PMW(7)              86 '        -'
077100850311     O                       PMW(8)              97 '        -'
077200850311     O                       PMW(9)             108 '        -'
077300850311     O                       PMW(10)            119 '        -'
077400850204     O                       PRZGIA             130 '        -'
077500850201     O          E            DETAIL      1
077600850405     O                       PAGE1              132
077700850201     O                       SK6                132
077800850204     O                                            7 'VAL/000'
077900850311     O                       VMW(1)              20 '       , -'
078000850311     O                       VMW(2)              31 '       , -'
078100850311     O                       VMW(3)              42 '       , -'
078200850311     O                       VMW(4)              53 '       , -'
078300850311     O                       VMW(5)              65 '       , -'
078400850311     O                       VMW(6)              76 '       , -'
078500850311     O                       VMW(7)              87 '       , -'
078600850311     O                       VMW(8)              98 '       , -'
078700850311     O                       VMW(9)             109 '       , -'
078800850311     O                       VMW(10)            120 '       , -'
078900850204     O                       TOTVAL             132 '      0 , -'
079000850314     O          E            DETAIL      1
079100850405     O                       PAGE1              132
079200850314     O                       SK5                132
079300850201     O*--------------------------------------------------------------*
079400850204**    ERR                       ERRORI
079500850314*- Cod. DITTA non Valido - Fare inserimento     -* Err,1
079600850201**   SK1
079700850201*-DESDIT-----------****-------------------------------------------SK01
079800850201-------------------------------------*** il XX/XX/XX      Pag. XXXSK01
079900850201**   SK2
080000850204                    ** Gest.Magazzino -M.P.-  VALORIZZAZIONE  RIEPSK02
080100990317ILOGATIVA  dal XX.XX.XX al XX.XX.XX   **        PGM.:   TII208R1  SK02
080200850201**   SK3
080300850201                    ***-------------------------------------------SK03
080400850201-------------------------------------***        Utente  XXXXXXXXXXSK03
080500850201**   SK4
080600850314*-------*--------------------E N T R A T E----------------------**SK04
080700850201*---------------------U S C I T E---------------------*----------*SK04
080800850201**   SK5
080900850201*-------*-----------*----------*----------*----------*----------**SK05
081000850201*---------*----------*----------*----------*----------*----------*SK05
081100850201**   SK6
081200850201*       *           :          :          :          :           *SK06
081300850204          :          :          :          :          :           SK06
