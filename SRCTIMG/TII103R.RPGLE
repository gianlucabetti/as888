000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F* ANU014R  *-AN/38-*-------------------------------------------*
000300000000     F*------------------*                                           *
000400000000     F*       - RICERCA ALFABETICA FORNITORI USO INTERNO -           *
000500000000     F*       --------------------------------------------           *
000600000000     F*                                                              *
000700000000     F*         FILE : ANFOP02L - ANFOP03L - ANFOP04L                *
000800000000     F*                ANU014D  -                                    *
000900000000     F*                                                              *
001000000000     F*--------------------------------------------------------------*
001100990118     FtmFOP02L  IF   E           K DISK    USROPN
001200990118     F                                     RENAME(tmFOP000:ANFOP2)
001300000000     F*---------
001400990118     FtmFOP03L  IF   E           K DISK    USROPN
001500990118     F                                     RENAME(tmFOP000:ANFOP3)
001600000000     F*---------
001700990118     FtmFOP04L  IF   E           K DISK    USROPN
001800990118     F                                     RENAME(tmFOP000:ANFOP4)
001900000000     F*---------
002000990317     Ftii103D   CF   E             WORKSTN
002100000000     F                                     SFILE(ANU14SF1:REC1)
002200000000     C*----------------------------------------------------*
002300000000     C     *ENTRY        PLIST                                                  *
002400990118     C                   PARM                    CODFOR            8            *
002500000000     C                   PARM                    DESCFA                         *
002600000000     C*----------------------------------------------------*
002700000000     C     DESCFA        CABNE     *BLANKS       SIALFA               111112    *
002800000000     C*--------- EMISSIONE PROMPT :  ----------------------*
002900000000     C     EMISPT        TAG                                                    *
003000000000     C                   SETOFF                                       10        *
003100000000     C                   WRITE     ANU0141                                      *TESTATA
003200000000     C                   EXFMT     ANU0142                                      *
003300990114     C   Kc              SETON                                        90        *
003400990114     C   Kc              GOTO      FINE                                         *
003500000000     C     DESCFA        CABEQ     *BLANKS       NOALFA                         *
003600000000     C*----------------------------------------------------*
003700000000     C     SIALFA        TAG                                                    *
003800990118     C  N02              OPEN      tmFOP02L                                     *
003900000000     C                   SETON                                        02        *
004000000000     C                   MOVEL     DESCFA        UNO1              1            *
004100860820     C                   MOVEL     DESCFA        DUE1              2            *
004200860820     C                   MOVEL     ' '           DUE1                           *
004300860820     C                   MOVEL     DESCFA        TRE1              3            *
004400860820     C                   MOVEL     '  '          TRE1                           *
004500990118     C     DESCFA        SETLL     tmFOP02L                           90        *
004600000000     C   90
004700000000     CANN11              GOTO      EMISPT                                       *
004800000000     C*---------- GESTIONE SUBFILE SOLO ALFABETICO --------*
004900000000     C*----------------------------------------------------*
005000000000     C                   Z-ADD     0             REC1              5 0          *
005100000000     C                   MOVE      *BLANKS       SCELTA                         *
005200000000     C*---------- PULIZIA SLFCT :                          *
005300000000     C                   SETON                                            70    *
005400000000     C                   WRITE     ANU14SC1                                     *
005500000000     C                   SETOFF                                       7071      *
005600000000     C*---------- CONTROLLI PER CARICAMENTO SFL :          *
005700000000     C     SOPRA         TAG                                                    *
005800000000     C                   READ      ANFOP2                                 91    *
005900000000     C   91              GOTO      EMISF1                                       *
006000000000     C                   MOVEL     FOPRSI        UNO2              1            *
006100860820     C                   MOVEL     FOPRSI        DUE2              2            *
006200860820     C                   MOVEL     ' '           DUE2                           *
006300860820     C                   MOVEL     FOPRSI        TRE2              3            *
006400860820     C                   MOVEL     '  '          TRE2                           *
006500000000     C     UNO1          CABNE     UNO2          EMISF1                         *
006600860820     C     DUE1          IFNE      *BLANKS                                      *
006700860820     C     DUE1          CABNE     DUE2          EMISF1                         *
006800860820     C                   END                                                    *
006900860820     C     TRE1          IFNE      *BLANKS                                      *
007000860820     C     TRE1          CABNE     TRE2          EMISF1                         *
007100860820     C                   END                                                    *
007200000000     C*---------- CARICAMENTO SFL :                        *
007300000000     C     REC1          ADD       1             REC1                 71        *
007400000000     C                   WRITE     ANU14SF1                                     *
007500000000     C                   GOTO      SOPRA                                        *
007600000000     C*---------- EMISSIONE SFL :                          *
007700000000     C     EMISF1        TAG                                                    *
007800000000     C                   SETON                                            10    *
007900000000     C                   WRITE     ANU0141                                      *TESTATA
008000000000     C                   EXFMT     ANU14SC1                                     *
008100990114     C   Kc              SETON                                            90    *
008200990114     C   Kc              GOTO      FINE                                         *
008300000000     C*---------- LETTURA RECORD VARIATI :                 *
008400000000     C   71              READC     ANU14SF1                               91    *
008500000000     C  N71                                                                     *
008600000000     COR 91              GOTO      EMISF1                                       *
008700000000     C     SCELTA        CABNE     'X'           EMISF1                         *
008800000000     C                   MOVE      FOPFOR        CODFOR                         *
008900000000     C                   MOVE      FOPRSI        DESCFA                         *
009000000000     C                   GOTO      FINE                                         *
009100000000     C     NOALFA        TAG                                                    *
009200000000     C*---------- GESTIONE SUBFILE GRUPPO MERC. :----------*
009300000000     C*----------------------------------------------------*
009400000000     C     GRUMER        CABEQ     *BLANKS       NOGRUP                         *
009500000000     C*---------- RICERCA GRUPPO MERCEOLOGICO :            *
009600000000     C     GRUMER        IFEQ      '? '                                         *
009700000000     C                   MOVE      *BLANKS       GRUMER                         *
009800000000     C                   MOVE      *BLANKS       DEGRME           25            *
009900990317     C                   CALL      'TII114R'                                    *
010000000000     C                   PARM                    GRUMER                         *
010100000000     C                   PARM                    DEGRME                         *
010200000000     C                   GOTO      EMISPT                                       *
010300000000     C                   ELSE                                                   *
010400000000     C                   Z-ADD     0             REC1              5 0          *
010500000000     C                   MOVE      *BLANKS       SCELTA                         *
010600990118     C  N03              OPEN      tmFOP03L                                     *
010700000000     C                   SETON                                        03        *
010800990118     C     GRUMER        SETLL     tmFOP03L                                     *
010900000000     C*---------- PULIZIA SLFCT :                          *
011000000000     C     SU1           TAG                                                    *
011100000000     C                   SETON                                            70    *
011200000000     C                   WRITE     ANU14SC1                                     *
011300000000     C                   SETOFF                                       7071      *
011400000000     C*---------- CARICAMENTO SFL :                        *
011500000000     C     1             DO        45            REC1                           *
011600000000     C     GRUMER        READE     ANFOP3                                 72    *
011700000000     C  N72              DO                                                     *
011800000000     C                   WRITE     ANU14SF1                                     *
011900000000     C                   SETON                                        71        *
012000000000     C                   END                                                    *
012100000000     C  N72              END                                                    *
012200000000     C*---------- EMISSIONE SFL :                          *
012300000000     C     SU2           TAG                                                    *
012400000000     C                   SETON                                            10    *
012500000000     C                   WRITE     ANU0141                                      *TESTATA
012600000000     C                   EXFMT     ANU14SC1                                     *
012700990114     C   Kc              SETON                                            90    *
012800990114     C   Kc              GOTO      FINE                                         *
012900000000     C*---------- LETTURA RECORD VARIATI :                 *
013000000000     C   71              READC     ANU14SF1                               91    *
013100000000     C  N71                                                                     *
013200000000     COR 91
013300000000     CANN72              GOTO      SU1                                          *
013400000000     C   91
013500000000     CAN 72              GOTO      SU2                                          *
013600000000     C     SCELTA        CABNE     'X'           SU2                            *
013700000000     C                   MOVE      FOPFOR        CODFOR                         *
013800000000     C                   MOVE      FOPRSI        DESCFA                         *
013900000000     C                   GOTO      FINE                                         *
014000000000     C                   END                                                    *
014100000000     C     NOGRUP        TAG                                                    *
014200000000     C*---------- GESTIONE SUBFILE SETTORE MERC.:----------*
014300000000     C*----------------------------------------------------*
014400000000     C     SETMER        CABEQ     *BLANKS       NOSETT                         *
014500000000     C*---------- RICERCA SETTORE MERCEOLOGICO :           *
014600000000     C     SETMER        IFEQ      '? '                                         *
014700000000     C                   MOVE      *BLANKS       SETMER                         *
014800000000     C                   MOVE      *BLANKS       DESEME           25            *
014900990317     C                   CALL      'TII115R'                                    *
015000000000     C                   PARM                    SETMER                         *
015100000000     C                   PARM                    DESEME                         *
015200000000     C                   GOTO      EMISPT                                       *
015300000000     C                   ELSE                                                   *
015400000000     C                   Z-ADD     0             REC1              5 0          *
015500000000     C                   MOVE      *BLANKS       SCELTA                         *
015600990118     C  N04              OPEN      tmfOP04L                                     *
015700000000     C                   SETON                                        04        *
015800990118     C     SETMER        SETLL     tmFOP04L                                     *
015900000000     C*---------- PULIZIA SLFCT :                          *
016000000000     C     SU3           TAG                                                    *
016100000000     C                   SETON                                            70    *
016200000000     C                   WRITE     ANU14SC1                                     *
016300000000     C                   SETOFF                                       7071      *
016400000000     C*---------- CARICAMENTO SFL :                        *
016500000000     C     1             DO        45            REC1                           *
016600000000     C     SETMER        READE     ANFOP4                                 72    *
016700000000     C  N72              DO                                                     *
016800000000     C                   WRITE     ANU14SF1                                     *
016900000000     C                   SETON                                        71        *
017000000000     C                   END                                                    *
017100000000     C  N72              END                                                    *
017200000000     C*---------- EMISSIONE SFL :                          *
017300000000     C     SU4           TAG                                                    *
017400000000     C                   SETON                                            10    *
017500000000     C                   WRITE     ANU0141                                      *TESTATA
017600000000     C                   EXFMT     ANU14SC1                                     *
017700990114     C   Kc              SETON                                            90    *
017800990114     C   Kc              GOTO      FINE                                         *
017900000000     C*---------- LETTURA RECORD VARIATI :                 *
018000000000     C   71              READC     ANU14SF1                               91    *
018100000000     C  N71                                                                     *
018200000000     COR 91
018300000000     CANN72              GOTO      SU3                                          *
018400000000     C   91
018500000000     CAN 72              GOTO      SU4                                          *
018600000000     C     SCELTA        CABNE     'X'           SU4                            *
018700000000     C                   MOVE      FOPFOR        CODFOR                         *
018800000000     C                   MOVE      FOPRSI        DESCFA                         *
018900000000     C                   GOTO      FINE                                         *
019000000000     C                   END                                                    *
019100000000     C     NOSETT        TAG                                                    *
019200000000     C*----------------------------------------------------*
019300000000     C*     USCITA PROGRAMMA                               *
019400000000     C*----------------------------------------------------*
019500000000     C     FINE          TAG                                                    *
019600000000     C   90              MOVEL     'ERRORE'      DESCFA                         *
019700000000     C                   SETON                                            LR    *
019800000000     C*----------------------------------------------------*
