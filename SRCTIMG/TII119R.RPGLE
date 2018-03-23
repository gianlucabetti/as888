000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200990118     F* ANU167R  *-AN/38-*-------------------------------------------*
000300000000     F*------------------*                                           *
000400000000     F*       - RICERCA ALFABETICA FORNITORI USO INTERNO -           *
000500000000     F*       --------------------------------------------           *
000600000000     F*                                                              *
000700990118     F*         FILE : tmcln02L - tmcln03L - tmcln04L                *
000800990118     F*                anu167D  -                                    *
000900000000     F*                                                              *
001000000000     F*--------------------------------------------------------------*
001100990115     FtmCLP02L  IF   E           K DISK    USROPN
001200990115     F                                     RENAME(tmCLP000:ANCLP2)
001300000000     F*---------
001400990115     FtmCLP03L  IF   E           K DISK    USROPN
001500990115     F                                     RENAME(tmCLP000:ANCLP3)
001600000000     F*---------
001700990115     FtmCLP04L  IF   E           K DISK    USROPN
001800990115     F                                     RENAME(tmCLP000:ANCLP4)
001900000000     F*---------
002000990317     FTII119D   CF   E             WORKSTN
002100000000     F                                     SFILE(ANU14SF1:REC1)
002200000000     C/SPACE 2
002300000000     C*----------------------------------------------------*
002400000000     C     *ENTRY        PLIST                                                  *
002500990115     C                   PARM                    CODFOR            8            *
002600000000     C                   PARM                    DESCFA                         *
002700000000     C*----------------------------------------------------*
002800000000     C/SPACE 2
002900000000     C*----------------------------------------------------*
003000000000     C     DESCFA        CABNE     *BLANKS       SIALFA               111112    *
003100000000     C*--------- EMISSIONE PROMPT :  ----------------------*
003200000000     C     EMISPT        TAG                                                    *
003300000000     C                   SETOFF                                       10        *
003400000000     C                   WRITE     ANU0141                                      *TESTATA
003500000000     C                   EXFMT     ANU0142                                      *
003600990114     C   Kc              SETON                                        90        *
003700990114     C   Kc              GOTO      FINE                                         *
003800000000     C     DESCFA        CABEQ     *BLANKS       NOALFA                         *
003900000000     C*----------------------------------------------------*
004000000000     C     SIALFA        TAG                                                    *
004100990115     C  N02              OPEN      tmCLP02L                                     *
004200000000     C                   SETON                                        02        *
004300000000     C                   MOVEL     DESCFA        UNO1              1            *
004400000000     C                   MOVEL     DESCFA        DUE1              2            *
004500000000     C                   MOVEL     ' '           DUE1                           *
004600000000     C                   MOVEL     DESCFA        TRE1              3            *
004700000000     C                   MOVEL     '  '          TRE1                           *
004800990115     C     DESCFA        SETLL     tmCLP02L                           90        *
004900000000     C   90
005000000000     CANN11              GOTO      EMISPT                                       *
005100000000     C*----------------------------------------------------*
005200000000     C/EJECT
005300000000     C*----------------------------------------------------*
005400000000     C*---------- GESTIONE SUBFILE SOLO ALFABETICO --------*
005500000000     C*----------------------------------------------------*
005600000000     C                   Z-ADD     0             REC1              5 0          *
005700000000     C                   MOVE      *BLANKS       SCELTA                         *
005800000000     C*---------- PULIZIA SLFCT :                          *
005900000000     C                   SETON                                            70    *
006000000000     C                   WRITE     ANU14SC1                                     *
006100000000     C                   SETOFF                                       7071      *
006200000000     C*---------- CONTROLLI PER CARICAMENTO SFL :          *
006300000000     C     SOPRA         TAG                                                    *
006400000000     C                   READ      ANCLP2                                 91    *
006500000000     C   91              GOTO      EMISF1                                       *
006600000000     C                   MOVEL     CLPRSI        UNO2              1            *
006700870324     C                   MOVEL     CLPRSI        DUE2              2            *
006800000000     C                   MOVEL     ' '           DUE2                           *
006900000000     C                   MOVEL     CLPRSI        TRE2              3            *
007000000000     C                   MOVEL     '  '          TRE2                           *
007100000000     C     UNO1          CABNE     UNO2          EMISF1                         *
007200000000     C     DUE1          IFNE      *BLANKS                                      *
007300000000     C     DUE1          CABNE     DUE2          EMISF1                         *
007400000000     C                   END                                                    *
007500000000     C     TRE1          IFNE      *BLANKS                                      *
007600000000     C     TRE1          CABNE     TRE2          EMISF1                         *
007700000000     C                   END                                                    *
007800000000     C*---------- CARICAMENTO SFL :                        *
007900000000     C     REC1          ADD       1             REC1                 71        *
008000000000     C                   WRITE     ANU14SF1                                     *
008100000000     C                   GOTO      SOPRA                                        *
008200000000     C*---------- EMISSIONE SFL :                          *
008300000000     C     EMISF1        TAG                                                    *
008400000000     C                   SETON                                            10    *
008500000000     C                   WRITE     ANU0141                                      *TESTATA
008600000000     C                   EXFMT     ANU14SC1                                     *
008700990114     C   Kc              SETON                                            90    *
008800990114     C   Kc              GOTO      FINE                                         *
008900000000     C*---------- LETTURA RECORD VARIATI :                 *
009000000000     C   71              READC     ANU14SF1                               91    *
009100000000     C  N71                                                                     *
009200000000     COR 91              GOTO      EMISF1                                       *
009300000000     C     SCELTA        CABNE     'X'           EMISF1                         *
009400000000     C                   MOVE      CLPKSC        CODFOR                         *
009500000000     C                   MOVE      CLPRSI        DESCFA                         *
009600000000     C                   GOTO      FINE                                         *
009700000000     C     NOALFA        TAG                                                    *
009800000000     C*----------------------------------------------------*
009900000000     C/EJECT
010000000000     C*----------------------------------------------------*
010100000000     C*---------- GESTIONE SUBFILE GRUPPO MERC. :----------*
010200000000     C*----------------------------------------------------*
010300000000     C     GRUMER        CABEQ     *BLANKS       NOGRUP                         *
010400000000     C*---------- RICERCA GRUPPO MERCEOLOGICO :            *
010500000000     C     GRUMER        IFEQ      '? '                                         *
010600000000     C                   MOVE      *BLANKS       GRUMER                         *
010700000000     C                   MOVE      *BLANKS       DEGRME           25            *
010800990317     C                   CALL      'TII114R'                                    *
010900000000     C                   PARM                    GRUMER                         *
011000000000     C                   PARM                    DEGRME                         *
011100000000     C                   GOTO      EMISPT                                       *
011200000000     C                   ELSE                                                   *
011300000000     C                   Z-ADD     0             REC1              5 0          *
011400000000     C                   MOVE      *BLANKS       SCELTA                         *
011500990115     C  N03              OPEN      tmCLP03L                                     *
011600000000     C                   SETON                                        03        *
011700990115     C     GRUMER        SETLL     tmCLP03L                                     *
011800000000     C*---------- PULIZIA SLFCT :                          *
011900000000     C     SU1           TAG                                                    *
012000000000     C                   SETON                                            70    *
012100000000     C                   WRITE     ANU14SC1                                     *
012200000000     C                   SETOFF                                       7071      *
012300000000     C*---------- CARICAMENTO SFL :                        *
012400000000     C     1             DO        45            REC1                           *
012500000000     C     GRUMER        READE     ANCLP3                                 72    *
012600000000     C  N72              DO                                                     *
012700000000     C                   WRITE     ANU14SF1                                     *
012800000000     C                   SETON                                        71        *
012900000000     C                   END                                                    *
013000000000     C  N72              END                                                    *
013100000000     C*---------- EMISSIONE SFL :                          *
013200000000     C     SU2           TAG                                                    *
013300000000     C                   SETON                                            10    *
013400000000     C                   WRITE     ANU0141                                      *TESTATA
013500000000     C                   EXFMT     ANU14SC1                                     *
013600990114     C   Kc              SETON                                            90    *
013700990114     C   Kc              GOTO      FINE                                         *
013800000000     C*---------- LETTURA RECORD VARIATI :                 *
013900000000     C   71              READC     ANU14SF1                               91    *
014000000000     C  N71                                                                     *
014100000000     COR 91
014200000000     CANN72              GOTO      SU1                                          *
014300000000     C   91
014400000000     CAN 72              GOTO      SU2                                          *
014500000000     C     SCELTA        CABNE     'X'           SU2                            *
014600000000     C                   MOVE      CLPKSC        CODFOR                         *
014700000000     C                   MOVE      CLPRSI        DESCFA                         *
014800000000     C                   GOTO      FINE                                         *
014900000000     C                   END                                                    *
015000000000     C     NOGRUP        TAG                                                    *
015100000000     C*----------------------------------------------------*
015200000000     C/EJECT
015300000000     C*----------------------------------------------------*
015400000000     C*---------- GESTIONE SUBFILE SETTORE MERC.:----------*
015500000000     C*----------------------------------------------------*
015600000000     C     SETMER        CABEQ     *BLANKS       NOSETT                         *
015700000000     C*---------- RICERCA SETTORE MERCEOLOGICO :           *
015800000000     C     SETMER        IFEQ      '? '                                         *
015900000000     C                   MOVE      *BLANKS       SETMER                         *
016000000000     C                   MOVE      *BLANKS       DESEME           25            *
016100990317     C                   CALL      'TII115R'                                    *
016200000000     C                   PARM                    SETMER                         *
016300000000     C                   PARM                    DESEME                         *
016400000000     C                   GOTO      EMISPT                                       *
016500000000     C                   ELSE                                                   *
016600000000     C                   Z-ADD     0             REC1              5 0          *
016700000000     C                   MOVE      *BLANKS       SCELTA                         *
016800990115     C  N04              OPEN      tmCLP04L                                     *
016900000000     C                   SETON                                        04        *
017000990115     C     SETMER        SETLL     tmCLP04L                                     *
017100000000     C*---------- PULIZIA SLFCT :                          *
017200000000     C     SU3           TAG                                                    *
017300000000     C                   SETON                                            70    *
017400000000     C                   WRITE     ANU14SC1                                     *
017500000000     C                   SETOFF                                       7071      *
017600000000     C*---------- CARICAMENTO SFL :                        *
017700000000     C     1             DO        45            REC1                           *
017800000000     C     SETMER        READE     ANCLP4                                 72    *
017900000000     C  N72              DO                                                     *
018000000000     C                   WRITE     ANU14SF1                                     *
018100000000     C                   SETON                                        71        *
018200000000     C                   END                                                    *
018300000000     C  N72              END                                                    *
018400000000     C*---------- EMISSIONE SFL :                          *
018500000000     C     SU4           TAG                                                    *
018600000000     C                   SETON                                            10    *
018700000000     C                   WRITE     ANU0141                                      *TESTATA
018800000000     C                   EXFMT     ANU14SC1                                     *
018900990114     C   Kc              SETON                                            90    *
019000990114     C   Kc              GOTO      FINE                                         *
019100000000     C*---------- LETTURA RECORD VARIATI :                 *
019200000000     C   71              READC     ANU14SF1                               91    *
019300000000     C  N71                                                                     *
019400000000     COR 91
019500000000     CANN72              GOTO      SU3                                          *
019600000000     C   91
019700000000     CAN 72              GOTO      SU4                                          *
019800000000     C     SCELTA        CABNE     'X'           SU4                            *
019900000000     C                   MOVE      CLPKSC        CODFOR                         *
020000000000     C                   MOVE      CLPRSI        DESCFA                         *
020100000000     C                   GOTO      FINE                                         *
020200000000     C                   END                                                    *
020300000000     C     NOSETT        TAG                                                    *
020400000000     C*----------------------------------------------------*
020500000000      /SPACE 2
020600000000     C*----------------------------------------------------*
020700000000     C*     USCITA PROGRAMMA                               *
020800000000     C*----------------------------------------------------*
020900000000     C     FINE          TAG                                                    *
021000000000     C   90              MOVEL     'ERRORE'      DESCFA                         *
021100000000     C                   SETON                                            LR    *
021200000000     C*----------------------------------------------------*
021300000000     C/EJECT
