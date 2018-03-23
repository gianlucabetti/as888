000100990125     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990125     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850201     F* MPH306R *-MP/38-*--------------------------------------------*
000400850201     F*-----------------*                                            *
000500850201     F*       - GENERA GUIDA PER STAMPA RIEPIL. A VALORE -MP-        *
000600850201     F*       -----------------------------------------------        *
000700850201     F*                                                   AZN.: H306 *
000800990125     F*         FILE : MPPRTSTD-tmamp01L-tmamp03L-tmamp04L           *
000900990125     F*         FILE : ANTAB00F-tmpil00F-7PSMP01L                    *
001000850201     F*                                                              *
001100850201     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001200850201     F*--------------------------------------------------------------*
001300850201     F*     04      - RICHIESTA STAMPA DI UN PRODOTTO                *
001400850201     F*     05      - RICHIESTA STAMPA PER CLASSE   MERCEOLOGICA     *
001500850201     F*     06      - RICHIESTA STAMPA PER S/CLASSE MERCEOLOGICA     *
001600850201     F*--------------------------------------------------------------*
001700850201     F*     IMPOSTAZIONE CHIAVI PILOTA :                             *
001800850201     F*             PILK01 = MAGAZZINO                  'XXXXXXX   ' *
001900850311     F*             PILK02 = CLASSE E/O S/CLASSE        'XXX  XXXXX' *
002000850201     F*             PILK03 = CODICE PRODOTTO            'XXXXXXX   ' *
002100850201     F*             PILK04 = CODICE VERSIONE            'XX        ' *
002200850201     F*             PILFIL = DESCRIZIONE PRODOTTO E UMA 'XXXX.. XXX' *
002300850201     F*--------------------------------------------------------------*
002400990125     Ftmamp01L  IF   E           K DISK    USROPN
002500990125     F                                     RENAME(tmamp000:ALLART)
002600990125     Ftmamp03L  IF   E           K DISK    USROPN
002700990125     F                                     RENAME(tmamp000:CLSART)
002800990125     Ftmamp04L  IF   E           K DISK    USROPN
002900990125     F                                     RENAME(tmamp000:SCLART)
003000990125     Ftmsmp01L  IF   E           K DISK
003100850201     FANTAB00F  IF   E           K DISK
003200990125     Ftmpil00F  O    E           K DISK
003300850201     D MAG             S              7    DIM(100)                             *MAGAZZINI
003400850201     D ERR             S             50    DIM(4) CTDATA PERRCD(1)              *ERRORI
003500850628     D TB153         E DS                  EXTNAME(ANTAB153)
003600850628     D*----- DESCRIZIONE TABELLA MAGAZZINI :
003700850201     D KPJBA         E DS
003800850201     D*----- PASSAGGIO PARAMETRI
003900990125     D  DAL305               247    254  0
004000990125     D  AL305                255    262  0
004100990125     D  MAG305               263    269
004200990125     D  CAU305               270    272
004300990125     D  LIS305               273    275
004400990125     D  COL305               276    276  0
004500990125     D  FOR305               277    277
004600990125     D  LSG305               278    280
004700990125     D  CLG305               281    281  0
004800990125     D  TIP305               282    282  0
004900990125     D  DET305               283    283
005000990125     D  CLS305               289    291
005100990125     D  SCL305               292    296
005200990125     D  MAT305               297    308
005300990125     D  desdit               433    452
005400990125     D  MSGERR               453    502
005500850201     C*----------------------------------------------------*
005600850201     C     *ENTRY        PLIST                                                  *
005700850201     C                   PARM                    KPJBA                          *
005800850201     C*---------- ANAGRAFICO MATERIE PRIME :               *
005900850201     C     KEYAMP        KLIST                                                  *
006000850201     C                   KFLD                    MAT305                         *
006100850201     C*---------- SALDI DI MAGAZZINO :                     *
006200850201     C     KEYSMP        KLIST                                                  *
006300990125     C                   KFLD                    AA                4 0          *
006400850201     C                   KFLD                    MAGAZZ            7            *
006500850201     C*---------- ANAGRAFICO TABELLE :                     *
006600850201     C     KEYTAB        KLIST                                                  *
006700850201     C                   KFLD                    TABGRU                         *
006800850201     C                   KFLD                    TABCOD                         *
006900850201     C*---------- TEST TIPO STAMPA :                       *
007000850201     C     TIP305        COMP      1                                      04    *PRODOTTO
007100850201     C     TIP305        COMP      2                                      05    *CLASSE
007200850201     C     TIP305        COMP      3                                      06    *S/CLASSE
007300850201     C   05CLS305        COMP      '999'                              0101      *
007400850201     C   05
007500850201     CANN01              MOVE      *BLANKS       OLDCLS            3            *
007600850201     C   06SCL305        COMP      '99999'                            0101      *
007700850201     C   06
007800850201     CANN01              MOVE      *BLANKS       OLDSCL            5            *
007900850201     C*---------- RICERCA MAGAZZINI -----------------------*
008000850201     C                   Z-ADD     0             M                 3 0          *
008100850201     C                   MOVE      '153'         TABGRU                         *
008200850201     C     TABGRU        SETLL     ANTAB00F                           90        *
008300850201     C   90              MOVEL     ERR(2)        MSGERR                         *
008400850201     C   90              GOTO      FINE                                         *
008500850201     C     RICMAG        TAG                                                    *
008600850201     C     TABGRU        READE     ANTAB000                               91    *
008700850201     C   91
008800850201     CAN 71              GOTO      AVANTI                                       *
008900850201     C   91
009000850201     CANN71              SETON                                            90    *
009100850201     C   90              MOVEL     ERR(2)        MSGERR                         *
009200850201     C   90              GOTO      FINE                                         *
009300850628     C                   MOVEL     TABDES        TB153                          *
009400850201     C     MAG305        IFNE      '9999999'                                    *
009500850201     C     MAG305        CABNE     TABCOD        RICMAG                         *
009600850201     C                   MOVE      MAG305        MAGAZZ                         *
009700850201     C                   ELSE                                                   *
009800850628     C     VMP153        CABNE     'S'           RICMAG                         *ESCLUDE MAGAZ.
009900850201     C                   MOVE      TABCOD        MAGAZZ                         *
010000850201     C                   END                                                    *
010100850201     C                   SETOFF                                           99    *
010200850201     C                   MOVEL     DAL305        AA                             *
010300990125     C     KEYSMP        SETLL     tmsmp01L                           91  92    *
010400850201     C   91                                                                     *
010500850201     CORN92              SETON                                            99    *
010600850201     C   99              MOVEL     AL305         AA                             *
010700990125     C   99KEYSMP        SETLL     tmsmp01L                           91  92    *
010800850201     C   91                                                                     *
010900850201     CORN92              GOTO      RICMAG                                       *
011000850201     C     M             ADD       1             M                    71        *
011100850201     C                   MOVE      TABCOD        MAG(M)                         *
011200850201     C     M             CABEQ     100           AVANTI                         *
011300850201     C                   GOTO      RICMAG                                       *
011400850201     C     AVANTI        TAG                                                    *
011500850201     C*----------                                          *
011600850201     C                   Z-ADD     0             X                 3 0          *
011700850201     C     RITENT        TAG                                                    *
011800850201     C     X             IFEQ      100                                          *
011900850201     C                   SETON                                            90    *
012000850201     C                   MOVEL     ERR(3)        MSGERR                         *
012100850201     C                   GOTO      FINE                                         *
012200850201     C                   END                                                    *
012300990125     C   04              OPEN      tmamp01L                             90      *
012400990125     C   05              OPEN      tmamp03L                             90      *
012500990125     C   06              OPEN      tmamp04L                             90      *
012600850201     C   90X             ADD       1             X                              *
012700850201     C   90              GOTO      RITENT                                       *
012800850201     C*---------- RICERCA PRODOTTO ------------------------*
012900850201     C                   SETOFF                                           71    *
013000990125     C   04MAT305        SETLL     tmamp01L                           90        *
013100850201     C   05
013200990125     CAN 01CLS305        SETLL     tmamp03L                           90        *
013300850201     C   05
013400990125     CANN01OLDCLS        SETLL     tmamp03L                           90        *
013500850201     C   06
013600990125     CAN 01SCL305        SETLL     tmamp04L                           90        *
013700850201     C   06
013800990125     CANN01OLDSCL        SETLL     tmamp04L                           90        *
013900850201     C   90              GOTO      ENDMAT                                       *
014000850201     C     RICMAT        TAG                                                    *
014100850201     C   04MAT305        READE     ALLART                                 91    *
014200850201     C   05
014300850201     CANN01              READ      CLSART                                 91    *
014400850201     C   05
014500850201     CAN 01CLS305        READE     CLSART                                 91    *
014600850201     C   06
014700850201     CANN01              READ      SCLART                                 91    *
014800850201     C   06
014900850201     CAN 01SCL305        READE     SCLART                                 91    *
015000850201     C   91              GOTO      ENDMAT                                       *
015100850201     C*---------- CONTROLLO ROTTURA CLASSE :               *
015200850201     C   05CLS305        IFNE      '999'                                        *
015300850201     C     AMPCLM        CABLT     CLS305        RICMAT                         *
015400850201     C     AMPCLM        CABGT     CLS305        ENDMAT                         *
015500850201     C                   END                                                    *
015600850201     C   05              MOVEL     AMPCLM        PILK02                         *
015700850311     C   05              MOVE      AMPSCM        PILK02                         *
015800850417     C*---------- CONTROLLO ROTTURA S/CLASSE :             *
015900850417     C   06SCL305        IFNE      '99999'                                      *
016000850201     C     AMPSCM        CABLT     SCL305        RICMAT                         *
016100850201     C     AMPSCM        CABGT     SCL305        ENDMAT                         *
016200850201     C                   END                                                    *
016300850311     C   06              MOVE      AMPSCM        PILK02                         *
016400850201     C*---------- EMISSIONE RECORD GUIDA ------------------*
016500850201     C                   MOVEL     AMPMAT        PILK03                         *
016600850201     C                   MOVEL     AMPDMT        PILFIL                         *
016700850201     C                   MOVE      AMPUMA        PILFIL                         *
016800850201     C     1             DO        M             X                              *
016900850201     C     MAG(X)        CABEQ     *BLANKS       RICMAT                         *
017000850201     C                   MOVEL     MAG(X)        PILK01                         *
017100990125     C                   WRITE     tmpil000                                     *
017200850201     C                   END                                                    *
017300850201     C                   SETON                                            71    *
017400850201     C                   GOTO      RICMAT                                       *
017500850201     C     ENDMAT        TAG                                                    *
017600850201     C   91
017700850201     CANN71              SETON                                            90    *
017800850201     C   90              MOVEL     ERR(4)        MSGERR                         *
017900850201     C*----------------------------------------------------*
018000850201     C     FINE          TAG                                                    *
018100850201     C   90              MOVEL     'KPJB207'     KMSMN                          *
018200850201     C                   SETON                                            LR    *
018300850201     C*----------------------------------------------------*
018400850201     C/EJECT
018500850201**    ERR                       ERRORI
018600850201*- Cod. DITTA non Valido - Fare inserimento     -* Err,1
018700850201*- Non  Trovata Tabella Descrizione Magazzini    * Err,2
018800850201*- STAMPA non ESEGUITA - Archivi non Disponibili-* ERR,3
018900850201*- Non  Trovati PRODOTTI  riferentesi a Richiesta* Err,4
