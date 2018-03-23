000100990419     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990419     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850207     F* MPH930R *-MP/38-*--------------------------------------------*
000400850207     F*-----------------*                                            *
000500850207     F*       - LANCIO RIGENERAZIONE SALDI MAGAZZINO MP -            *
000600850207     F*       -------------------------------------------            *
000700850207     F*                                                   AZN.: H930 *
000800850207     F*         FILE : MPH930D-ANAMP01L-ANTAB00F-                    *
000900850207     F*                                                              *
001000850207     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001100850207     F*                ANU015R   - RICERCA ANAGRAF.PRODOTTI          *
001200850207     F*                ANU153R   - RICERCA CODICI  DI MAGAZZINO      *
001300850207     F*                ANU155R   - RICERCA CLASSE  MERCEOLOGICA      *
001400850207     F*                ANU156R   - RICERCA S/CLASS.MERCEOLOGICA      *
001500850207     F*                                                              *
001600850207     F*         SBM. : AZN.-H931 - RIGENERAZIONE SALDI DI MAGAZZ.    *
001700850207     F*--------------------------------------------------------------*
001800990419     Ftii210D   CF   E             WORKSTN
001900990419     FtmAMP01L  IF   E           K DISK
002000850207     FANTAB00F  IF   E           K DISK
002100990419     D ERR             S             30    DIM(6) CTDATA PERRCD(1)              *ERRORI
002200850207     D KPJBA         E DS
002300850207     D*----- PASSAGGIO PARAMETRI
002400990419     D  AAA930               247    250  0
002500990419     D  MAG930               251    257
002600990419     D  MAT930               276    287
002700990419     D  CLS930               267    269
002800990419     D  SCL930               270    274
002900990419     D  TIP930               275    275
003000000103     D  socpas               288    290
003100850207     D  MSGERR               453    502
003200990419     D soc001        E DS                  EXTNAME(xsoc001ds)
003300990419     D xsocds          DS          1000
003400850207     C*----------------------------------------------------*
003500850207     C     *ENTRY        PLIST                                                  *
003600850207     C                   PARM                    KPJBA                          *
003700850207     C*---------- ANAGRAFICO MATERIE PRIME :               *
003800850207     C     KEYAMP        KLIST                                                  *
003900850207     C                   KFLD                    MAT930                         *
004000850207     C*---------- ANAGRAFICO TABELLE :                     *
004100850207     C     KEYTAB        KLIST                                                  *
004200850207     C                   KFLD                    TABGRU                         *
004300850207     C                   KFLD                    TABCOD                         *
004400990419     C*---------- RICERCA DITTA :
004500990419     C                   MOVEL     'SOC001'      TIPXSC
004600990419     C                   MOVEL     *blank        SOCXSC
004700990419     C                   EXSR      REPSOC
004800990419     C     RTNXSC        IFNE      '1'
004900990419     C                   MOVEL     XSOCDS        SOC001
005000990419     C                   MOVEL     xscsoc        socpas
005100990419     c                   end
005200990419     c                   time                    como14           14 0
005300990419     c                   move      como14        woggi             8 0
005400850207     C*-----------                                         *
005500850207     C                   MOVE      *ZEROS        AAA930                         *
005600990419     C                   MOVE      woggi         AAA930                         *
005700850207     C                   MOVE      '?      '     MAG930                         *
005800850207     C                   MOVE      *BLANKS       DESMAG                         *
005900850207     C                   MOVE      'XXXXXXX'     MAT930                         *
006000870109     C                   MOVEL     'XXXXXXX'     MAT930                         *
006100850207     C                   MOVE      *BLANKS       DESART                         *
006200850207     C                   MOVE      'XXX'         CLS930                         *
006300850207     C                   MOVE      *BLANKS       DESCLS                         *
006400850207     C                   MOVE      'XXXXX'       SCL930                         *
006500850207     C                   MOVE      *BLANKS       DESSCL                         *
006600850208     C                   MOVE      *BLANKS       TIP930                         *
006700850207     C*--------- CONTROLLI SU PROMPT ----------------------*
006800850207     C     EMISPT        TAG                                                    *
006900850207     C                   EXFMT     MPH930PT                                     *
007000990419     C   Kc              GOTO      FINE                                         *
007100850207     C   KY              GOTO      LANCIO                                       *
007200850207     C                   MOVEA     *ZEROS        *IN(1)                         *
007300850207     C                   MOVE      *BLANKS       ERRORE                         *
007400850207     C*----------------------------------------------------*
007500990419     C     aaa930        comp      1998                                 90
007600990419     C   90              MOVEL     ERR(6)        ERRORE                         *
007700990419     c   90              goto      emispt
007800850207     C*---------- MAGAZZINO -------------------------------*
007900850207     C     MAG930        IFNE      '9999999'                                    *
008000850207     C     MAG930        IFEQ      '?      '                                    *
008100850207     C                   MOVE      *BLANKS       MAG930                         *
008200850207     C                   MOVE      *BLANKS       DAT153                         *
008300850207     C                   CALL      'ANU153R'                                    *
008400850207     C                   PARM                    MAG930                         *
008500850207     C                   PARM                    DAT153           75            *
008600850207     C                   MOVEL     DAT153        DESMAG                         *
008700850207     C                   SETON                                            91    *
008800850207     C                   GOTO      EMISPT                                       *
008900850207     C                   END                                                    *
009000850207     C                   MOVE      '153'         TABGRU                         *
009100850207     C                   MOVE      *ZEROS        TABCOD                         *
009200850207     C                   MOVE      MAG930        TABCOD                         *
009300850207     C     KEYTAB        CHAIN     ANTAB00F                           90        *
009400850207     C   90              SETON                                            91    *
009500850207     C   90              MOVEL     ERR(1)        ERRORE                         *
009600850207     C   90              GOTO      EMISPT                                       *
009700850207     C                   MOVEL     TABDES        DESMAG                         *
009800850207     C                   END                                                    *
009900850207     C*----------------------------------------------------*
010000850207     C/EJECT
010100850207     C*----------PRODOTTO ---------------------------------*
010200871019     C                   MOVEL     'XXXXXX'      ICS              12
010300870109     C                   MOVE      'XXXXXX'      ICS
010400871019     C                   MOVEL     '999999'      NOVE             12
010500870109     C                   MOVE      '999999'      NOVE
010600871019     C                   MOVEL     '?'           INTERR           12
010700870109     C     MAT930        IFNE      ICS                                          *
010800870109     C                   MOVE      'XXX'         CLS930                         *
010900870109     C                   MOVE      *BLANKS       DESCLS                         *
011000870109     C                   MOVE      'XXXXX'       SCL930                         *
011100870109     C                   MOVE      *BLANKS       DESSCL                         *
011200870109     C     MAT930        IFEQ      NOVE                                         *
011300870109     C                   SETON                                          1110    *
011400870109     C                   MOVE      '1'           TIP930                         *
011500870109     C                   GOTO      EMISPT                                       *
011600870109     C                   END                                                    *
011700870109     C     MAT930        IFEQ      INTERR                                       *
011800870109     C                   MOVE      *BLANKS       MAT930                         *
011900870109     C                   CALL      'ANU015R'                                    *
012000870109     C                   PARM                    MAT930                         *
012100870109     C                   PARM                    DESART                         *
012200870109     C                   SETON                                            92    *
012300870109     C                   GOTO      EMISPT                                       *
012400870109     C                   END                                                    *
012500990419     C  N90KEYAMP        CHAIN     tmAMP01L                           90        *
012600870109     C   90              SETON                                            92    *
012700870109     C   90              MOVEL     ERR(2)        ERRORE                         *
012800870109     C  N90              MOVEL     AMPDMT        DESART                         *
012900870109     C  N90              SETON                                          1011    *
013000870109     C  N90              MOVE      '1'           TIP930                         *
013100870109     C                   GOTO      EMISPT                                       *
013200870109     C                   END                                                    *
013300850207     C*----------------------------------------------------*
013400850207     C/EJECT
013500850207     C*---------- CLASSE MERCEOLOGICA ---------------------*
013600870109     C                   MOVE      ICS           MAT930                         *
013700850207     C                   MOVE      *BLANKS       DESART                         *
013800850207     C     CLS930        IFNE      'XXX'                                        *
013900850207     C                   MOVE      'XXXXX'       SCL930                         *
014000850207     C                   MOVE      *BLANKS       DESSCL                         *
014100850207     C     CLS930        IFEQ      '?      '                                    *
014200850207     C                   MOVE      *BLANKS       CLS930                         *
014300850207     C                   MOVE      *BLANKS       DESCLS                         *
014400850207     C                   CALL      'ANU155R'                                    *
014500850207     C                   PARM                    CLS930                         *
014600850207     C                   PARM                    DESCLS                         *
014700850207     C                   SETON                                            93    *
014800850207     C                   GOTO      EMISPT                                       *
014900850207     C                   END                                                    *
015000850207     C                   MOVE      '155'         TABGRU                         *
015100850207     C                   MOVE      *ZEROS        TABCOD                         *
015200850207     C                   MOVE      CLS930        TABCOD                         *
015300850207     C     KEYTAB        CHAIN     ANTAB00F                           90        *
015400850207     C   90              SETON                                            93    *
015500850207     C   90              MOVEL     ERR(3)        ERRORE                         *
015600850207     C  N90              MOVEL     TABDES        DESCLS                         *
015700850207     C  N90              SETON                                          1012    *
015800850208     C  N90              MOVE      '2'           TIP930                         *
015900850207     C                   GOTO      EMISPT                                       *
016000850207     C                   END                                                    *
016100850207     C*----------------------------------------------------*
016200850211     C/EJECT
016300850207     C*---------- S/CLASSE MERCEOLOGICA -------------------*
016400850207     C                   MOVE      'XXX'         CLS930                         *
016500850207     C                   MOVE      *BLANKS       DESCLS                         *
016600850207     C     SCL930        IFNE      'XXXXX'                                      *
016700850207     C     SCL930        IFEQ      '?      '                                    *
016800850207     C                   MOVE      *BLANKS       SCL930                         *
016900850207     C                   MOVE      *BLANKS       DESSCL                         *
017000850207     C                   CALL      'ANU156R'                                    *
017100850207     C                   PARM                    SCL930                         *
017200850207     C                   PARM                    DESSCL                         *
017300850207     C                   SETON                                            94    *
017400850207     C                   GOTO      EMISPT                                       *
017500850207     C                   END                                                    *
017600850207     C                   MOVE      '156'         TABGRU                         *
017700850207     C                   MOVE      *ZEROS        TABCOD                         *
017800850207     C                   MOVE      SCL930        TABCOD                         *
017900850207     C     KEYTAB        CHAIN     ANTAB00F                           90        *
018000850207     C   90              SETON                                            94    *
018100850207     C   90              MOVEL     ERR(4)        ERRORE                         *
018200850207     C  N90              MOVEL     TABDES        DESSCL                         *
018300850207     C  N90              SETON                                          1013    *
018400850208     C  N90              MOVE      '3'           TIP930                         *
018500850207     C                   GOTO      EMISPT                                       *
018600850207     C                   END                                                    *
018700850207     C*---------- MANCA RICHIESTA :                        *
018800850207     C                   SETON                                        90        *
018900850207     C                   MOVEL     ERR(5)        ERRORE                         *
019000850207     C                   GOTO      EMISPT                                       *
019100850207     C*----------------------------------------------------*
019200850211     C/SPACE 3
019300850207     C*---------- LANCIO RIGENERAZIONE --------------------*
019400850207     C     LANCIO        TAG                                                    *
019500000103     C                   MOVE      'I211'        KCOAZ                          *
019600140428     C                   MOVEL     'KSDIFFE'     KQEBI                          *
019700000103     C*                  CALL      'BCH09'                                      *
019800000103     C*                  PARM                    KPJBA                          *
019900000103     C*                  CALL      'BCH10'                                      *
020000000103     C                   CALL      'TII211C'                                    *
020100850207     C                   PARM                    KPJBA                          *
020200850207     C*----------------------------------------------------*
020300850207     C     FINE          TAG                                                    *
020400850207     C                   SETON                                            LR    *
020500990419     C*----------------------------------------------------*
020600990419     C* Reperimento dati società
020700990419     C*----------------------------------------------------*
020800990419     C     REPSOC        BEGSR
020900990419     C*
021000990419     C                   CALLB     'XSOC'
021100990419     C                   PARM                    TIPXSC            6
021200990419     C                   PARM                    SOCXSC            3
021300990419     C                   PARM                    CDSXSC            9 0
021400990419     C                   PARM                    MODXSC            3
021500990419     C                   PARM      *blanks       RTNXSC            1
021600990419     C                   PARM                    XSOCDS
021700990419     C                   PARM                    KPJBA
021800990419     C*
021900990419     C                   ENDSR
022000850207     C*----------------------------------------------------*
022100850207**    ERR                       ERRORI
022200850207*- Cod. Magazzino ERRATO !! -*  Err,1
022300850207*- Cod. Prodotto  ERRATO !! -*  Err,2
022400850207*- Cod. Classe    ERRATO !! -*  Err,3
022500850207*- Cod. S/Classe  ERRATO !! -*  Err,4
022600850207*- INSERIRE DATI X RIGENER. -*  Err,5
022700990419*- Anno ERRATO           !! -*  Err,6
