000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*YES) CVTOPT(*NONE)
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F********************************************************************
000400000000     F*                                                                  *
000500000000     F*  BAK036R -  REPERISCE INFORMAZIONI JOB                           *
000600000000     F*                                                                  *
000700000000     F*  CREATO DA  VANCIO  NEL  MARZO  1994                             *
000800000000     F*                                                                  *
000900000000     D********************************************************************
001000000000     D*
001100000000     D*                PARAMETRI DI INPUT
001200000000     D*
001300000000     D*  JOBNAM036 - Nome job
001400000000     D*  USRNAM036 - Nome utente
001500000000     D*  JOBNBR036 - Numero job
001600000000     D*
001700000000     D*                PARAMETRI DI OUTPUT
001800000000     D*
001900000000     D*  ERR036    - Errore
002000000000     D*  JOBQ036   - Coda esecuzione job
002100000000     D*  JOBPTY036 - Priorità in coda
002200000000     D*  JOBSTS036 - Stato job
002300000000     D*  JOBTYP036 - Tipo job
002400000000     D*
002500000000     D********************************************************************
002600000000X0195X***** DS036           DS
002700000000  "  X*****  JNAM36                 1     10
002800000000  "  X*****  UNAM36                11     20
002900000000  "  X*****  JNBR36                21     26
003000000000  "  X*****  JOBQ36                27     36
003100000000  "  X*****  JOBP36                37     37
003200000000  "  X*****  JSTS36                38     47
003300000000  "  D* Parametri I/O
003400000000X0195D BAK036DS      E DS
003500000000     D* DS X FORMATO 'JOBI0300'
003600000000     D LIST            DS
003700000000     D  BYTRTN                 1      4B 0
003800000000     D  BYTVAL                 5      8B 0
003900000000X0195X*****  QJOBNM                 9     34
004000000000     D  JOBNAM                 9     18
004100000000     D  USRNAM                19     28
004200000000     D  JOBNBR                29     34
004300000000     D  INTID                 35     50
004400000000     D  JOBSTS                51     60
004500000000     D  JOBTYP                61     61
004600000000     D  SUBTYP                62     62
004700000000     D  JOBQ                  63     72
004800000000     D  JOBQLI                73     82
004900000000     D  JOBQPR                83     84
005000000000     D  OUTQ                  85     94
005100000000     D  OUTQLI                95    104
005200000000     D  OUTQPR               105    106
005300000000     D  PRTDEV               107    116
005400000000X0195X*****  SBMJOB               117    142
005500000000  "  X*****  SBMJO1               117    126
005600000000X0195D  SBMJOB               117    126
005700000000     D  SBMUSR               127    136
005800000000     D  SBMNBR               137    142
005900000000     D  MSGQ                 143    152
006000000000     D  MSGQLI               153    162
006100000000     D* NOME JOB COMPLETO
006200000000     D                 DS
006300000000     D  JOB                    1     26
006400000000     D  JOBN                   1     10
006500000000     D  USRN                  11     20
006600000000     D  JOBNU                 21     26
006700000000     C*****************************************************************
006800000000     C     *ENTRY        PLIST
006900000000X0195X*****              PARM                    DS036
007000000000X0195C                   PARM                    BAK036DS
007100000000     C* REPERIMENTO UTENTE E/O NUMERO JOB SE NON SPECIFICATI
007200000000X0195X*****UNAM36        IFEQ      '*ALL'
007300000000  "  C     USRNAM036     IFEQ      '*ALL'
007400000000X0195C     JOBNBR036     OREQ      '*ALL'
007500000000     C                   CALL      'BAK037R'
007600000000X0195X*****              PARM                    JNAM36
007700000000  "  X*****              PARM                    UNAM36
007800000000  "  X*****              PARM                    JNBR36
007900000000  "  C                   PARM                    JOBNAM036
008000000000  "  C                   PARM                    USRNAM036
008100000000X0195C                   PARM                    JOBNBR036
008200000000     C                   ENDIF
008300000000     C* PREPARAZIONE DATI PER API
008400000000X0195X*****              MOVEL     JNAM36        JOBNAM
008500000000  "  X*****              MOVEL     UNAM36        USRNAM
008600000000  "  X*****              MOVEL     JNBR36        JOBNBR
008700000000  "  X*****              MOVEL     JNAM36        JOBN
008800000000  "  X*****              MOVEL     UNAM36        USRN
008900000000  "  X*****              MOVEL     JNBR36        JOBNU
009000000000  "  C                   MOVEL     JOBNAM036     JOBNAM
009100000000  "  C                   MOVEL     USRNAM036     USRNAM
009200000000  "  C                   MOVEL     JOBNBR036     JOBNBR
009300000000  "  C                   MOVEL     JOBNAM036     JOBN
009400000000  "  C                   MOVEL     USRNAM036     USRN
009500000000X0195C                   MOVEL     JOBNBR036     JOBNU
009600000000     C* REPERIMENTO INFORMAZIONI JOB
009700000000     C                   CALL      'QUSRJOBI'                           21
009800000000     C                   PARM                    LIST
009900000000     C                   PARM      162           LISLEN            4 0
010000000000     C                   PARM      'JOBI0300'    FORMAT            8
010100000000     C                   PARM                    JOB              26
010200000000     C                   PARM                    IDFJOB           16
010300000000     C*
010400000000     C* PARAMETRI OUTPUT
010500000000X0195C                   EVAL      ERR036 = *IN21
010600000000  "  C                   IF        *IN21 = *OFF
010700000000  "  X**N21              MOVEL     JOBQ          JOBQ36
010800000000  "  X**N21              MOVE      JOBQPR        JOBP36
010900000000  "  X**N21              MOVE      JOBSTS        JSTS36
011000000000  "  C                   MOVE      JOBQ          JOBQ036
011100000000  "  C                   MOVE      JOBQPR        JOBPTY036
011200000000  "  C                   MOVE      JOBSTS        JOBSTS036
011300000000  "  C                   MOVE      JOBTYP        JOBTYP036
011400000000X0195C                   ENDIF
011500000000     C*
011600000000     C                   SETON                                        LR
