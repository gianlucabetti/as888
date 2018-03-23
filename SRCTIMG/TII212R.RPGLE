000100990419     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990419     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850207     F* MPH932R *-MP/38-*--------------------------------------------*
000400000000     F*-----------------*                                            *
000500850207     F*       - GENERA GUIDA PER RIGENERAZ. SALDI MAGAZZ.-MP-        *
000600000000     F*       -----------------------------------------------        *
000700850207     F*                                                   AZN.: H931 *
000800850207     F*         FILE : ANAMP01L-ANAMP03L-ANAMP04L                    *
000900860627     F*                ANTAB00F-MPPIL00F-MPMMP01L                    *
001000860627     F*                MPSMP01L                                      *
001100000000     F*                                                              *
001200000000     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001300000000     F*--------------------------------------------------------------*
001400850207     F*     04      - RICHIESTA RIGEN. PER   PRODOTTO                *
001500850207     F*     05      - RICHIESTA RIGEN. PER CLASSE   MERCEOLOGICA     *
001600850207     F*     06      - RICHIESTA RIGEN. PER S/CLASSE MERCEOLOGICA     *
001700000000     F*--------------------------------------------------------------*
001800000000     F*     IMPOSTAZIONE CHIAVI PILOTA :                             *
001900000000     F*             PILK01 = MAGAZZINO                  'XXXXXXX   ' *
002000000000     F*             PILK02 = CLASSE O S/CLASSE          'XXX.X     ' *
002100000000     F*             PILK03 = CODICE PRODOTTO            'XXXXXXX   ' *
002200000000     F*             PILK04 = CODICE VERSIONE            'XX        ' *
002300000000     F*             PILFIL = DESCRIZIONE PRODOTTO E UMA 'XXXX.. XXX' *
002400000000     F*--------------------------------------------------------------*
002500990419     FtmAMP01L  IF   E           K DISK    USROPN
002600990419     F                                     RENAME(tmAMP000:ALLART)
002700990419     FtmAMP03L  IF   E           K DISK    USROPN
002800990419     F                                     RENAME(tmAMP000:CLSART)
002900990419     FtmAMP04L  IF   E           K DISK    USROPN
003000990419     F                                     RENAME(tmAMP000:SCLART)
003100990419     FtmMMP01L  IF   E           K DISK
003200990419     FtmSMP01L  UF   E           K DISK
003300000000     FANTAB00F  IF   E           K DISK
003400990419     FtmPIL00F  O    E           K DISK
003500000000     D MAG             S              7    DIM(100)                             *MAGAZZINI
003600000000     D ERR             S             50    DIM(4) CTDATA PERRCD(1)              *ERRORI
003700990419     D RECSAL        E DS                  EXTNAME(TMSMP00F)
003800860627     D*--------------------- ARCHIVIO SALDI :
003900990419     D  QTS                   86    145P 2
004000860627     D                                     DIM(10)                              *Q.TA'  SALDI
004100000000     D KPJBA         E DS
004200000000     D*----- PASSAGGIO PARAMETRI
004300990419     D  AAA930               247    250  0
004400990419     D  MAG930               251    257
004500990419     D  MAT930               276    287
004600990419     D  CLS930               267    269
004700990419     D  SCL930               270    274
004800990419     D  TIP930               275    275
004900000103     D  socpas               288    290
005000990419     D  MSGERR               453    502
005100990419     D soc001        E DS                  EXTNAME(xsoc001ds)
005200990419     D xsocds          DS          1000
005300000000     C*----------------------------------------------------*
005400000000     C     *ENTRY        PLIST                                                  *
005500000000     C                   PARM                    KPJBA                          *
005600000000     C*---------- ANAGRAFICO MATERIE PRIME :               *
005700000000     C     KEYAMP        KLIST                                                  *
005800850207     C                   KFLD                    MAT930                         *
005900860627     C*---------- MOVIM.DI MAGAZZINO :                     *
006000850207     C     KEYMMP        KLIST                                                  *
006100990419     C                   KFLD                    AA                4 0          *
006200000000     C                   KFLD                    MAGAZZ            7            *
006300860627     C*---------- SALDI DI MAGAZZINO :                     *
006400860627     C     KEYSMP        KLIST                                                  *
006500860627     C                   KFLD                    AA                             *
006600860627     C                   KFLD                    SALMAG            7            *
006700871019     C                   KFLD                    SALMAT           12            *
006800000000     C*---------- ANAGRAFICO TABELLE :                     *
006900000000     C     KEYTAB        KLIST                                                  *
007000000000     C                   KFLD                    TABGRU                         *
007100000000     C                   KFLD                    TABCOD                         *
007200860627     C*--                                                  *
007300860627     C                   MOVEL     AAA930        AA                             *
007400000000     C*---------- RICERCA DITTA :                          *
007500990419     C                   MOVEL     'SOC001'      TIPXSC
007600990419     C                   MOVEL     *blank        SOCXSC
007700990419     C                   EXSR      REPSOC
007800990419     C     RTNXSC        IFNE      '1'
007900990419     C                   MOVEL     XSOCDS        SOC001
008000990419     C                   MOVEL     xscsoc        socpas
008100990419     c                   end
008200990419     c                   time                    como14           14 0
008300990419     c                   move      como14        woggi             8 0
008400850207     C*---------- TEST TIPO RIGENERAZIONE :                *
008500850208     C     TIP930        IFEQ      '1'                                          *PRODOTTO
008600850208     C                   SETON                                            04    *
008700871019     C                   MOVE      *BLANKS       OLDMAT           12            *
008800871019     C                   MOVEL     '9999999'     NOVE             12
008900871019     C                   MOVE      '9999999'     NOVE             12
009000871019     C     MAT930        COMP      NOVE                                   01    *TUTTI
009100850208     C                   GOTO      SOTTO                                        *
009200850208     C                   END                                                    *
009300850208     C     TIP930        CABEQ     '2'           SOTTO                    05    *CLASSE
009400850208     C     TIP930        CABEQ     '3'           SOTTO                    06    *S/CLASSE
009500850207     C  N04
009600850207     CANN05
009700850207     CANN06              SETON                                        90        *
009800850207     C   90              MOVEL     ERR(4)        MSGERR                         *
009900850207     C                   GOTO      FINE                                         *
010000000000     C*----------------------------------------------------*
010100000000     C/EJECT
010200000000     C*---------- RICERCA MAGAZZINI -----------------------*
010300850207     C     SOTTO         TAG                                                    *
010400000000     C                   Z-ADD     0             M                 3 0          *
010500000000     C                   MOVE      '153'         TABGRU                         *
010600000000     C     TABGRU        SETLL     ANTAB00F                           90        *
010700000000     C   90              MOVEL     ERR(2)        MSGERR                         *
010800000000     C   90              GOTO      FINE                                         *
010900000000     C     RICMAG        TAG                                                    *
011000000000     C     TABGRU        READE     ANTAB000                               91    *
011100000000     C   91
011200000000     CAN 71              GOTO      AVANTI                                       *
011300000000     C   91
011400000000     CANN71              SETON                                            90    *
011500000000     C   90              MOVEL     ERR(2)        MSGERR                         *
011600000000     C   90              GOTO      FINE                                         *
011700850207     C     MAG930        IFNE      '9999999'                                    *
011800850207     C     MAG930        CABNE     TABCOD        RICMAG                         *
011900850207     C                   MOVE      MAG930        MAGAZZ                         *
012000000000     C                   ELSE                                                   *
012100000000     C                   MOVE      TABCOD        MAGAZZ                         *
012200000000     C                   END                                                    *
012300000000     C                   SETOFF                                           99    *
012400990419     C     KEYMMP        SETLL     tmMMP01L                           91  92    *
012500000000     C   91                                                                     *
012600000000     CORN92              GOTO      RICMAG                                       *
012700000000     C     M             ADD       1             M                    71        *
012800000000     C                   MOVE      TABCOD        MAG(M)                         *
012900000000     C     M             CABEQ     100           AVANTI                         *
013000000000     C                   GOTO      RICMAG                                       *
013100000000     C     AVANTI        TAG                                                    *
013200000000     C*----------                                          *
013300000000     C                   Z-ADD     0             X                 3 0          *
013400000000     C     RITENT        TAG                                                    *
013500000000     C     X             IFEQ      100                                          *
013600000000     C                   SETON                                            90    *
013700000000     C                   MOVEL     ERR(3)        MSGERR                         *
013800000000     C                   GOTO      FINE                                         *
013900000000     C                   END                                                    *
014000990419     C   04              OPEN      tmAMP01L                             90      *
014100990419     C   05              OPEN      tmAMP03L                             90      *
014200990419     C   06              OPEN      tmAMP04L                             90      *
014300000000     C   90X             ADD       1             X                              *
014400000000     C   90              GOTO      RITENT                                       *
014500000000     C*---------- RICERCA PRODOTTO ------------------------*
014600000000     C                   SETOFF                                           71    *
014700850207     C   04
014800990419     CANN01MAT930        SETLL     tmAMP01L                           90        *
014900850207     C   04
015000990419     CAN 01OLDMAT        SETLL     tmAMP01L                           90        *
015100990419     C   05CLS930        SETLL     tmAMP03L                           90        *
015200990419     C   06SCL930        SETLL     tmAMP04L                           90        *
015300000000     C   90              GOTO      ENDMAT                                       *
015400000000     C     RICMAT        TAG                                                    *
015500850207     C   04
015600850207     CANN01MAT930        READE     ALLART                                 91    *
015700850207     C   04
015800850207     CAN 01              READ      ALLART                                 91    *
015900850207     C   05CLS930        READE     CLSART                                 91    *
016000850207     C   06SCL930        READE     SCLART                                 91    *
016100000000     C   91              GOTO      ENDMAT                                       *
016200000000     C*---------- EMISSIONE RECORD GUIDA ------------------*
016300850207     C   05              MOVEL     AMPCLM        PILK02                         *
016400850207     C   06              MOVEL     AMPSCM        PILK02                         *
016500000000     C                   MOVEL     AMPMAT        PILK03                         *
016600000000     C                   MOVEL     AMPDMT        PILFIL                         *
016700000000     C                   MOVE      AMPUMA        PILFIL                         *
016800000000     C     1             DO        M             X                              *
016900000000     C     MAG(X)        CABEQ     *BLANKS       RICMAT                         *
017000000000     C                   MOVEL     MAG(X)        PILK01                         *
017100990419     C                   WRITE     tmPIL000                                     *
017200860627     C*--- PULIZIA SALDI                                   *
017300860627     C                   MOVEL     PILK01        SALMAG                         *
017400860627     C                   MOVEL     PILK03        SALMAT                         *
017500990419     C     KEYSMP        SETLL     tmSMP01L                           93        *
017600860627     C   93              GOTO      NOPUL                                        *
017700860627     C     PULSMP        TAG                                                    *
017800860627     C*          -------------                             *
017900990419     C     KEYSMP        READE     tmSMP01L                               93    *
018000860627     C   93              GOTO      NOPUL                                        *
018100860627     C                   Z-ADD     0             QTS                            *ENTRATE/USCITE
018200860627     C                   Z-ADD     0             SMPQGI                         *GIAC.INIZIALE
018300860627     C                   Z-ADD     0             SMPQGF                         *GIAC.FINALE
018400860627     C                   EXCEPT    UPSMP                                        *
018500860627     C                   GOTO      PULSMP                                       *
018600860627     C     NOPUL         TAG                                                    *
018700860627     C*          -------------                             *
018800000000     C                   END                                                    *
018900000000     C                   SETON                                            71    *
019000000000     C                   GOTO      RICMAT                                       *
019100000000     C     ENDMAT        TAG                                                    *
019200000000     C   91
019300000000     CANN71              SETON                                            90    *
019400000000     C   90              MOVEL     ERR(4)        MSGERR                         *
019500000000     C*----------------------------------------------------*
019600000000     C     FINE          TAG                                                    *
019700000103     C   90              MOVEL     'KPJB207'     KMSMN
019800000000     C                   SETON                                            LR    *
019900990419     C*----------------------------------------------------*
020000990419     C* Reperimento dati società
020100990419     C*----------------------------------------------------*
020200990419     C     REPSOC        BEGSR
020300990419     C*
020400990419     C                   CALLB     'XSOC'
020500990419     C                   PARM                    TIPXSC            6
020600990419     C                   PARM      socpas        SOCXSC            3
020700990419     C                   PARM                    CDSXSC            9 0
020800990419     C                   PARM                    MODXSC            3
020900990419     C                   PARM      *blanks       RTNXSC            1
021000990419     C                   PARM                    XSOCDS
021100990419     C                   PARM                    KPJBA
021200990419     C*
021300990419     C                   ENDSR
021400860627     O*--- AZZERAMENTO CAMPI SALDI M.P. -----------------------------*
021500990419     OtmSMP000  E            UPSMP
021600860627     O                       SMPQE1
021700860627     O                       SMPQE2
021800860627     O                       SMPQE3
021900860627     O                       SMPQE4
022000860627     O                       SMPQE5
022100860627     O                       SMPQU1
022200860627     O                       SMPQU2
022300860627     O                       SMPQU3
022400860627     O                       SMPQU4
022500860627     O                       SMPQU5
022600860627     O                       SMPQGI
022700860627     O                       SMPQGF
022800860627      /EJECT
022900000000**    ERR                       ERRORI
023000860627*- Cod. DITTA non Valido - Fare inserimento     -* Err,1
023100000000*- Non  Trovata Tabella Descrizione Magazzini    * Err,2
023200000000*- STAMPA non ESEGUITA - Archivi non Disponibili-* ERR,3
023300000000*- Non  Trovati PRODOTTI  riferentesi a Richiesta* Err,4
