000100990122     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990122     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850117     F* MPH120R *-MP/38-*--------------------------------------------*
000400850117     F*-----------------*                                            *
000500850117     F*       - LANCIO STAMPA  SITUAZIONE   MAGAZZINO M.P. -         *
000600850117     F*       ----------------------------------------------         *
000700850117     F*                                                   AZN.: H120 *
000800990122     F*         FILE : MPH120D-tmamp01L-ANTAB00F-                    *
000900850117     F*                                                              *
001000850117     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001100990317     F*                tii104R   - RICERCA ANAGRAF.PRODOTTI          *
001200990317     F*                tii108R   - RICERCA CODICI  DI MAGAZZINO      *
001300990317     F*                TII110R   - RICERCA CLASSE  MERCEOLOGICA      *
001400990317     F*                TII111R   - RICERCA S/CLASS.MERCEOLOGICA      *
001500850117     F*                                                              *
001600860522     F*         SBM. : AZN.-H121 - STAMPA MOVIMENTI DI MAGAZZINO     *
001700891229CALL F*--------------------------------------------------------------*
001800990317     FTII205D   CF   E             WORKSTN
001900990122     Ftmamp01L  IF   E           K DISK
002000850117     FANTAB00F  IF   E           K DISK
002100850117     D ERR             S             30    DIM(4) CTDATA PERRCD(1)              *ERRORI
002200850117     D KPJBA         E DS
002300850117     D*----- PASSAGGIO PARAMETRI
002400990122     D  AAA120               247    250  0
002500990122     D  SCE120               251    251
002600990122     D  UMI120               252    252
002700990122     D  MAG120               253    259
002800990122     D  CLS120               260    262
002900990122     D  SCL120               263    267
003000990122     D  MAT120               284    295
003100990122     D  SAL120               277    283  0
003200990122     D  desdit               433    452
003300850117     D  MSGERR               453    502
003400871121     D MPHDT1          DS           512
003500871121     D*-----DATA AREA NUMERATORE MOVIM. MAGAZZINO
003600871121     D  NUMDTA                 1      5  0
003700871121     D  FILL1                  6      8
003800871121     D  DATDTA                 9     14  0
003900871121     D  FILL2                 15     17
004000871121     D  MPHAA                 18     19  0
004100871121     D  FIL03                 20     50
004200871121     D  DGIO                  51     56  0
004300871121     D  FIL04                 57    100
004400871121     D  FILL5                101    300
004500871121     D  GIOTOL               301    302  0
004600871121     D  FILL6                303    512
004700990122     D soc001        E DS                  EXTNAME(xsoc001ds)
004800990122     D xsocds          DS          1000
004900850117     C*----------------------------------------------------*
005000850117     C     *ENTRY        PLIST                                                  *
005100850117     C                   PARM                    KPJBA                          *
005200990122     C*---------- RICERCA DITTA :
005300990122     C                   MOVEL     'SOC001'      TIPXSC
005400990122     C                   MOVEL     *blank        SOCXSC
005500990122     C                   EXSR      REPSOC
005600990122     C     RTNXSC        IFNE      '1'
005700990122     C                   MOVEL     XSOCDS        SOC001
005800990122     C                   MOVEL     xscrgs        desdit
005900990122     C                   MOVEL     xscsoc        wsoc              3
006000990122     c                   end
006100871121     C     *DTAARA       DEFINE                  MPHDT1                         *
006200850117     C*---------- ANAGRAFICO MATERIE PRIME :               *
006300850117     C     KEYAMP        KLIST                                                  *
006400850117     C                   KFLD                    MAT120                         *
006500850117     C*---------- ANAGRAFICO TABELLE :                     *
006600850117     C     KEYTAB        KLIST                                                  *
006700850117     C                   KFLD                    TABGRU                         *
006800850117     C                   KFLD                    TABCOD                         *
006900850117     C*-----------                                         *
007000871121     C                   IN        MPHDT1
007100871014     C                   MOVEL     '?'           INTERR           12
007200871014     C                   MOVEL     '9999999'     NOVE             12
007300861230     C                   MOVE      '9999999'     NOVE
007400850117     C                   MOVE      *ZEROS        AAA120                         *
007500871121     C                   MOVE      MPHAA         AAA120                         *
007600990122     c                   if        aaa120 > 40
007700990122     c                   add       1900          aaa120
007800990122     c                   else
007900990122     c                   add       2000          aaa120
008000990122     c                   end
008100850117     C                   MOVE      'G'           SCE120                         *
008200850117     C                   MOVE      'A'           UMI120                         *
008300871121     C                   MOVE      '1      '     MAG120                         *
008400850118     C                   MOVE      *BLANKS       DESMAG                         *
008500861230     C                   MOVE      NOVE          MAT120                         *
008600860522     C                   MOVE      *ZEROS        SAL120                         *
008700860522     C                   Z-ADD     9999999       SAL120                         *
008800850121     C     INIZIO        TAG                                                    *
008900850117     C                   MOVE      *BLANKS       DESART                         *
009000850118     C                   MOVE      '999'         CLS120                         *
009100850118     C                   MOVE      '99999'       SCL120                         *
009200850117     C*--------- CONTROLLI SU PROMPT ----------------------*
009300850117     C     EMISPT        TAG                                                    *
009400850117     C                   EXFMT     MPH120PT                                     *
009500990409     C   Kc              GOTO      FINE                                         *
009600850117     C                   MOVEA     *ZEROS        *IN(1)                         *
009700850117     C                   MOVE      *BLANKS       ERRORE                         *
009800850117     C*----------------------------------------------------*
009900850117     C                   MOVE      *BLANKS       DESMAG                         *
010000850117     C*---------- MAGAZZINO :                              *
010100850117     C     MAG120        IFNE      '9999999'                                    *
010200850117     C     MAG120        IFEQ      '?      '                                    *
010300850117     C                   MOVE      *BLANKS       MAG120                         *
010400850117     C                   MOVE      *BLANKS       DAT153                         *
010500990317     C                   CALL      'TII108R'                                    *
010600850117     C                   PARM                    MAG120                         *
010700850117     C                   PARM                    DAT153           75            *
010800850117     C                   MOVEL     DAT153        DESMAG                         *
010900850117     C                   GOTO      EMISPT                                       *
011000850117     C                   END                                                    *
011100850117     C                   MOVE      '153'         TABGRU                         *
011200850117     C                   MOVE      *ZEROS        TABCOD                         *
011300850117     C                   MOVE      MAG120        TABCOD                         *
011400850117     C     KEYTAB        CHAIN     ANTAB00F                           90        *
011500850117     C   90              SETON                                            91    *
011600850117     C   90              MOVEL     ERR(1)        ERRORE                         *
011700850117     C   90              GOTO      EMISPT                                       *
011800850117     C                   MOVEL     TABDES        DESMAG                         *
011900850117     C                   END                                                    *
012000850117     C*---------- PRODOTTO :                               *
012100861230     C     MAT120        IFNE      NOVE                                         *
012200861230     C     MAT120        IFEQ      INTERR                                       *
012300850117     C                   MOVE      *BLANKS       MAT120                         *
012400990317     C                   CALL      'TII104R'                                    *
012500850117     C                   PARM                    MAT120                         *
012600850117     C                   PARM                    DESART                         *
012700850117     C                   SETON                                        95        *
012800850117     C                   END                                                    *
012900990122     C     KEYAMP        CHAIN     tmamp01L                           90        *
013000850117     C   90              SETON                                            92    *
013100850117     C   90              MOVEL     ERR(2)        ERRORE                         *
013200850117     C   90              GOTO      EMISPT                                       *
013300850117     C                   MOVE      AMPCLM        CLS120                         *
013400850117     C                   MOVE      AMPSCM        SCL120                         *
013500850117     C   95              GOTO      EMISPT                                       *
013600850117     C                   GOTO      LANCIO                                       *
013700850117     C                   END                                                    *
013800871019     C                   MOVE      NOVE          MAT120                         *
013900850117     C*----------------------------------------------------*
014000850117     C                   MOVE      *BLANKS       DESCLS                         *
014100850117     C*---------- CLASSE MERCEOLOGICA :                    *
014200850117     C     CLS120        IFNE      '999'                                        *
014300850117     C     CLS120        IFEQ      '?      '                                    *
014400850117     C                   MOVE      *BLANKS       CLS120                         *
014500850117     C                   MOVE      *BLANKS       DESCLS                         *
014600990317     C                   CALL      'TII110R'                                    *
014700850117     C                   PARM                    CLS120                         *
014800850117     C                   PARM                    DESCLS                         *
014900850117     C                   GOTO      EMISPT                                       *
015000850117     C                   END                                                    *
015100850117     C                   MOVE      '155'         TABGRU                         *
015200850117     C                   MOVE      *ZEROS        TABCOD                         *
015300850117     C                   MOVE      CLS120        TABCOD                         *
015400850117     C     KEYTAB        CHAIN     ANTAB00F                           90        *
015500850117     C   90              SETON                                            93    *
015600850117     C   90              MOVEL     ERR(3)        ERRORE                         *
015700850117     C   90              GOTO      EMISPT                                       *
015800850117     C                   MOVEL     TABDES        DESCLS                         *
015900850117     C                   END                                                    *
016000850117     C*---------- S/CLASSE MERCEOLOGICA :                  *
016100850117     C                   MOVE      *BLANKS       DESSCL                         *
016200850117     C     SCL120        IFNE      '99999'                                      *
016300850117     C     SCL120        IFEQ      '?      '                                    *
016400850117     C                   MOVE      *BLANKS       SCL120                         *
016500850117     C                   MOVE      *BLANKS       DESSCL                         *
016600990317     C                   CALL      'TII111R'                                    *
016700850117     C                   PARM                    SCL120                         *
016800850117     C                   PARM                    DESSCL                         *
016900850117     C                   GOTO      EMISPT                                       *
017000850117     C                   END                                                    *
017100850117     C                   MOVE      '156'         TABGRU                         *
017200850117     C                   MOVE      *ZEROS        TABCOD                         *
017300850117     C                   MOVE      SCL120        TABCOD                         *
017400850117     C     KEYTAB        CHAIN     ANTAB00F                           90        *
017500850117     C   90              SETON                                            94    *
017600850117     C   90              MOVEL     ERR(4)        ERRORE                         *
017700850117     C   90              GOTO      EMISPT                                       *
017800850117     C                   MOVEL     TABDES        DESSCL                         *
017900850117     C                   END                                                    *
018000850117     C*---------- LANCIO STAMPA ---------------------------*
018100850117     C     LANCIO        TAG                                                    *
018200990317     C                   MOVE      'I206'        KCOAZ                          *
018300891229     C                   MOVEL     'KSDIFFE'     KQEBI                          *
018400850117     C                   CALL      'BCH09'                                      *
018500850117     C                   PARM                    KPJBA                          *
018600850117     C                   CALL      'BCH10'                                      *
018700850117     C                   PARM                    KPJBA                          *
018800850117     C                   GOTO      INIZIO                                       *
018900850117     C*----------------------------------------------------*
019000850117     C     FINE          TAG                                                    *
019100850117     C   90              MOVEL     'KPJB207'     KMSMN                          *
019200850117     C                   SETON                                            LR    *
019300990122     C*----------------------------------------------------*
019400990122     C* Reperimento dati società
019500990122     C*----------------------------------------------------*
019600990122     C     REPSOC        BEGSR
019700990122     C*
019800990122     C                   CALLB     'XSOC'
019900990122     C                   PARM                    TIPXSC            6
020000990122     C                   PARM                    SOCXSC            3
020100990122     C                   PARM                    CDSXSC            9 0
020200990122     C                   PARM                    MODXSC            3
020300990122     C                   PARM      *blanks       RTNXSC            1
020400990122     C                   PARM                    XSOCDS
020500990122     C                   PARM                    KPJBA
020600990122     C*
020700990122     C                   ENDSR
020800850117     C*----------------------------------------------------*
020900850117**    ERR                       ERRORI
021000850117*- Cod. Magazzino ERRATO !! -*  Err,1
021100850117*- Cod. PRODOTTO  ERRATO !! -*  Err,2
021200850117*- Cod. CLASSE    ERRATO !! -*  Err,3
021300850117*- Cod. S/CLASSE  ERRATO !! -*  Err,4
