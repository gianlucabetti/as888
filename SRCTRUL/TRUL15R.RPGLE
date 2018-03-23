000100020916     H DATEDIT(*YMD)
000200971210      *--------------------------------------------------------------*
000300971212      *   UTILITY PER CONTROLLO LNP/LNA di una spedizione e appart.  *
000400971212      *                  allo stesso ramo aziendale                  *
000500971210      *--------------------------------------------------------------*
000600971212     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
000700971210     D*--------------------------------------------------------------*
000800971210     D*  SCHIERE                                                     *
000900971210     D*--------------------------------------------------------------*
001000971212     D ERR             S             78    DIM(7) CTDATA PERRCD(1)
001100971231      * Schiere rami aziendali
001200971231     D FIL             S              3  0 DIM(999)                             Schiera filiali
001300971231     D DIT             S              3    DIM(999)                             codice azienda
001400971210      *--------------------------------------------------------------*
001500971210      *  DS                                                          *
001600971210      *--------------------------------------------------------------*
001700971212     D TRUL15        E DS                  EXTNAME(TRUL15DS)
001800020916     D TIBS55        E DS                  EXTNAME(TIBS55DS)
001900971212      *--------------------------------------------------------------*
002000971212      *  FILE AZORG                                                  *
002100971212      *--------------------------------------------------------------*
002200971212     IAZORG01L  AA
002300971212     I                                  1    3  ORGDIT
002400971212     I                             P    4    5 0ORGFIL
002500971231     I                                  7    7  ORGFAG
002600971210      *--------------------------------------------------------------*
002700971210      *  PGM:  SPECIFICHE DI ESECUZIONE                              *
002800971210      *--------------------------------------------------------------*
002900971210      * Ricevo parametri in entrata
003000971210     C     *ENTRY        PLIST
003100971212     C                   PARM                    TRUL15
003200971210      * Inizializzo campi di errore
003300971212     C                   MOVEL     *BLANKS       O15ERR
003400971212     C                   MOVEL     *BLANKS       O15MSG
003500971231      *----------------------------------------------------------*
003600971231      * Controllo validità tipo lancio
003700971231      *----------------------------------------------------------*
003800971231     C     I15TLA        IFNE      ' '
003900971231     C     I15TLA        ANDNE     'C'
004000971231     C     I15TLA        ANDNE     'L'
004100971231     C                   MOVEL     ERR(1)        O15MSG
004200971231     C                   MOVEL     '1'           O15ERR
004300971231     C                   GOTO      FINE
004400971231     C                   END
004500971231      *----------------------------------------------------------*
004600971231      * Se Tipo lancio 'C' --> solo chiusura file
004700971231      *----------------------------------------------------------*
004800971231     C     I15TLA        IFEQ      'C'
004900971231     C                   GOTO      FINE
005000971231     C                   END
005100971231      *----------------------------------------------------------*
005200971231      * controllo che il tipo controllo sia corretto
005300971231      *----------------------------------------------------------*
005400971231     C     I15TPC        IFNE      ' '
005500971231     C     I15TPC        ANDNE     'R'
005600971231     C                   MOVEL     ERR(2)        O15MSG
005700971231     C                   MOVEL     '1'           O15ERR
005800971231     C                   GOTO      FINE
005900971231     C                   END
006000971231      *----------------------------------------------------------*
006100971231      * Se non immessa ne lnp e ne lna do errore
006200971231      *----------------------------------------------------------*
006300971231     C     I15LNP        IFEQ      0
006400971231     C     I15LNA        ANDEQ     0
006500971231     C                   MOVEL     ERR(3)        O15MSG
006600971231     C                   MOVEL     '1'           O15ERR
006700971231     C                   GOTO      FINE
006800971231     C                   END
006900971231      *----------------------------------------------------------*
007000971231      * controllo in che ramo aziendale sono in base a NSI
007100971231      *----------------------------------------------------------*
007200971231     C     I15NSI        IFNE      I50PSI
007300971231     C     I15NSI        OREQ      *BLANKS
007400971231      * Imposto DS per reperire librerie del S.Informativo
007500020916     C                   CLEAR                   TIBS55
007600971231      * Se non so in che sistema informativo lo imposto in DS
007700971231     C                   MOVEL     I15NSI        I50PSI
007800971231     C                   MOVEL     'L'           I50TLA
007900020916     C                   CALL      'TIBS55R'
008000020916     C                   PARM                    TIBS55
008100971231     C                   END
008200971231      * Se non ci sono errori imposto le librerie e i rami aziendali
008300971231     C     O50ERR        IFEQ      *BLANKS
008400971231     C                   MOVEL     O50PSI        I15NSI
008500020916     C                   MOVEL     O50PNT        O15RAM
008600971231     C                   ELSE
008700971231     C                   MOVEL     '1'           O15ERR
008800971231     C                   MOVEL     O50MSG        O15MSG
008900971231     C                   GOTO      FINE
009000971231     C                   END
009100971231      *----------------------------------------------------------*
009200971231      * La prima volta se tipo richiamo multiplo carico in sch.  *
009300971231      * AZORG per non appesantire troppo il pgm con continue chain
009400971231      *----------------------------------------------------------*
009500971231     C     I15RIC        IFEQ      'M'
009600971231     C     WPRIMA        IFNE      'N'
009700971231     C                   EXSR      CARORG
009800980119     C                   ELSE
009900980119     C                   XFOOT     FIL           WTOTF             8 0
010000980119     C     WTOTF         IFEQ      0
010100980119     C                   EXSR      CARORG
010200971231     C                   END
010300980119     C                   END
010400971231     C* Eseguo test schiere
010500971231     C                   EXSR      TSTSCH
010600971231     C*
010700971231     C                   ELSE
010800971231     C                   EXSR      TSTORG
010900971231     C                   END
011000971210      *----------------------------------------------------------*
011100971210      *  Fine lavoro : Tipo lancio
011200971210     C     FINE          TAG
011300971210      *
011400971212     C     I15TLA        IFNE      ' '
011500971210     C                   SETON                                        LR
011600971210     C                   ELSE
011700971210     C                   SETON                                        RT
011800971210     C                   END
011900971231      *----------------------------------------------------------*
012000971231      * Eseguo test filiali su AZORG
012100971231      *----------------------------------------------------------*
012200971231     C     TSTORG        BEGSR
012300971231      *----------------------------------------------------------*
012400971231      * Se immessa lnp controllo se esiste e a che ramo appart.
012500971231      *----------------------------------------------------------*
012600971231     C     I15LNP        IFNE      0
012700971231     C                   Z-ADD     I15LNP        ORGFIL
012800971231     C     ORGFIL        CHAIN     AZORG01L                           31
012900971231     C     *IN31         IFEQ      '1'
013000971231     C                   MOVEL     ERR(4)        O15MSG
013100971231     C                   MOVEL     '1'           O15ERR
013200971231     C                   GOTO      FINE
013300971231     C                   ELSE
013400971231     C                   MOVEL     ORGDIT        O15RLP
013500971231     C     O15RLP        IFNE      O15RAM
013600971231     C     I15TPC        ANDEQ     'R'
013700971231     C                   MOVEL     ERR(5)        O15MSG
013800971231     C                   MOVEL     '1'           O15ERR
013900971231     C                   GOTO      FINE
014000971231     C                   END
014100971231     C                   END
014200971231     C*
014300971231     C                   END
014400971231      *----------------------------------------------------------*
014500971231      * Se immessa lna controllo se esiste e a che ramo appart.
014600971231      *----------------------------------------------------------*
014700971231     C     I15LNA        IFNE      0
014800971231     C                   Z-ADD     I15LNA        ORGFIL
014900971231     C     ORGFIL        CHAIN     AZORG01L                           31
015000971231     C     *IN31         IFEQ      '1'
015100971231     C                   MOVEL     ERR(6)        O15MSG
015200971231     C                   MOVEL     '1'           O15ERR
015300971231     C                   GOTO      FINE
015400971231     C                   ELSE
015500971231     C                   MOVEL     ORGDIT        O15RLA
015600971231     C     O15RLA        IFNE      O15RAM
015700971231     C     I15TPC        ANDEQ     'R'
015800971231     C                   MOVEL     ERR(7)        O15MSG
015900971231     C                   MOVEL     '1'           O15ERR
016000971231     C                   GOTO      FINE
016100971231     C                   END
016200971231     C                   END
016300971231     C*
016400971231     C                   END
016500971231     C*
016600971231     C                   ENDSR
016700971231      *----------------------------------------------------------*
016800971231      * Eseguo test filiali su schiere
016900971231      *----------------------------------------------------------*
017000971231     C     TSTSCH        BEGSR
017100971231      *----------------------------------------------------------*
017200971231      * Se immessa lnp controllo se esiste e a che ramo appart.
017300971231      *----------------------------------------------------------*
017400971231     C     I15LNP        IFNE      0
017500971231     C                   Z-ADD     1             X
017600971231     C     I15LNP        LOOKUP    FIL(X)                                 32
017700971231     C     *IN32         IFEQ      '0'
017800971231     C                   MOVEL     ERR(4)        O15MSG
017900971231     C                   MOVEL     '1'           O15ERR
018000971231     C                   GOTO      FINE
018100971231     C                   ELSE
018200971231     C                   MOVEL     DIT(X)        O15RLP
018300971231     C     O15RLP        IFNE      O15RAM
018400971231     C     I15TPC        ANDEQ     'R'
018500971231     C                   MOVEL     ERR(5)        O15MSG
018600971231     C                   MOVEL     '1'           O15ERR
018700971231     C                   GOTO      FINE
018800971231     C                   END
018900971231     C                   END
019000971231     C*
019100971231     C                   END
019200971231      *----------------------------------------------------------*
019300971231      * Se immessa lna controllo se esiste e a che ramo appart.
019400971231      *----------------------------------------------------------*
019500971231     C     I15LNA        IFNE      0
019600971231     C                   Z-ADD     1             X
019700971231     C     I15LNA        LOOKUP    FIL(X)                                 32
019800971231     C     *IN32         IFEQ      '0'
019900971231     C                   MOVEL     ERR(6)        O15MSG
020000971231     C                   MOVEL     '1'           O15ERR
020100971231     C                   GOTO      FINE
020200971231     C                   ELSE
020300971231     C                   MOVEL     DIT(X)        O15RLA
020400971231     C     O15RLA        IFNE      O15RAM
020500971231     C     I15TPC        ANDEQ     'R'
020600971231     C                   MOVEL     ERR(7)        O15MSG
020700971231     C                   MOVEL     '1'           O15ERR
020800971231     C                   GOTO      FINE
020900971231     C                   END
021000971231     C                   END
021100971231     C*
021200971231     C                   END
021300971231     C*
021400971231     C                   ENDSR
021500971231      *----------------------------------------------------------*
021600971231      * Eseguo caricamento dati azorg
021700971231      *----------------------------------------------------------*
021800971231     C     CARORG        BEGSR
021900971231     C*
022000980127     C                   Z-ADD     0             X
022100980206     C     *LOVAL        SETLL     AZORG01L
022200971231     C                   READ      AZORG01L                               30
022300971231     C     *IN30         DOWEQ     '0'
022400971231     C     ORGFAG        IFEQ      'F'
022500971231     C     ORGFAG        OREQ      'A'
022600971231     C                   ADD       1             X                 3 0
022700971231     C                   MOVE      ORGFIL        FIL(X)
022800971231     C                   MOVE      ORGDIT        DIT(X)
022900971231     C                   END
023000971231     C                   READ      AZORG01L                               30
023100971231     C                   END
023200971231     C*
023300971231     C                   MOVEL     'N'           WPRIMA            1
023400971231     C*
023500971231     C                   ENDSR
023600971218**
023700971218Tipo lancio non previsto
023800971218Tipo controllo non previsto
023900971218E' necessario impostare almeno un punto operativo
024000971218Punto operativo di partenza inesistente
024100971218P.to operat. di partenza deve essere del ramo aziend. in cui lavori
024200971218Punto operativo di arrivo inesistente
024300971218P.to operat. di arrivo deve essere del ramo aziendale in cui lavori
