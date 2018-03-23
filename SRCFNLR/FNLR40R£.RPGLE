000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941109     H* FNLR40R *----------------------------------------------------*
000300911025     H*         - ABILITO DISABILITO CONSEGNA MERCE
000400000000     H*--------------------------------------------------------------*
000500941109     FFNLR40D   CF   E             WORKSTN
000600941109     FFNARB01L  UF   E           K DISK
000700911030     D L6              S              3  0 DIM(30)                              FIL GESTITE
000800920706     D* PER INTERROGAZIONE BOLLE ARRIVI
000900911023     D PARAM           DS
001000941109     D  GIA                    1     12  0
001100941109     D  RICH                  13     13
001200941109     D  VI5AAS                14     17  0
001300941109     D  VI5LNP                18     20  0
001400941109     D  VI5NRS                21     22  0
001500941109     D  VI5NSP                23     29  0
001600941109     D  VI5KEY                14     29
001700941109     D  VIDFLG                31     31
001800941109     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
001900941109     D DSLV50        E DS                  EXTNAME(FNLV50DS)
002000941109     D* DS PER TRUL06R - CARICAMENTO £X
002100941109     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002200941109     D  LIN                    1     90  0
002300941109     D                                     DIM(30)                              FIL. COMODO
002400941109     D* PASSAGGIO PARAMETRI
002500941109     D DSLR02        E DS
002600911024     D KPJBA         E DS
002700911024     D CNCR80        E DS
002800911024     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002900900518     C****************************************************************
003000900518     C*  RIEPILOGO INDICATORI
003100900518     C***************************************************************
003200920706     C* 01    - LANCIO PGM ABILITAZIONE MERCE
003300920706     C* 02    - PGM RICHIAMATO CON KEY
003400920813     C* 04    - CAMBIO FILIALE  CMD18
003500920813     C* 05    - REMOTO
003600941109     C* 28    - EMETTO VIDMSG
003700911023     C* 30/31 - LETTURA COMODO
003800911025     C* 40-45 - ERRORI
003900911025     C* 58-59 - ERRORI
004000900518     C* 90    - INDICA LA PRESENZA DI UN ERRORE
004100900518     C*****************************************************************
004200911024     C*
004300911025     C     INIZIO        TAG
004400920813     C                   SETOFF                                       04
004500911025     C* PULIZIA CAMPI
004600941109     C                   TIME                    W0140            14 0
004700941109     C                   MOVE      W0140         VIDAAS
004800911025     C                   MOVEL     *ZEROS        VIDLNP
004900911025     C                   MOVEL     *ZEROS        VIDNRS
005000911025     C                   MOVEL     *ZEROS        VIDNSP
005100911025     C                   MOVEL     'A'           VIDAD
005200920706     C*
005300941109     C* DR2RIC = 1 --> PGM RICHIAMAMTO
005400941109     C     DR2RIC        IFEQ      '1'
005500941109     C                   MOVEL     DR2AAS        VIDAAS
005600941109     C                   MOVEL     DR2LNP        VIDLNP
005700941109     C                   MOVEL     DR2NRS        VIDNRS
005800941109     C                   MOVEL     DR2NSP        VIDNSP
005900941109     C                   MOVEL     DR2FGS        VIDFIL
006000920813     C                   SETON                                        0204
006100920706     C                   END
006200911025     C*
006300900509     C     FOR01         TAG
006400941109     C                   EXFMT     LR40D01
006500911025     C** CMD3  - FINE LAVORO
006600900509     C   KC              GOTO      FINE
006700911025     C**
006800911025     C** CMD7  - INTERROGAZIONE BOLLE ARRIVI
006900911025     C   KG              DO
007000941109     C                   MOVEL     *ZEROS        VI5KEY
007100911213     C                   MOVEL     '2'           VIDFLG
007200911025     C                   MOVEL     PARAM         KPJBU
007300941109     C                   CALL      'FNLR36R'
007400911025     C                   PARM                    KPJBA
007500911025     C                   MOVEL     KPJBU         PARAM
007600941109     C*
007700911025     C* SE PASSATA KEY LA IMPOSTO
007800941109     C     VI5KEY        IFGT      *ZEROS
007900911025     C                   MOVEL     VI5AAS        VIDAAS
008000911025     C                   MOVEL     VI5LNP        VIDLNP
008100911025     C                   MOVEL     VI5NRS        VIDNRS
008200911025     C                   MOVEL     VI5NSP        VIDNSP
008300911025     C                   END
008400911025     C*
008500911025     C                   GOTO      FOR01
008600911025     C                   END
008700920813     C*
008800920813     C** CMD18- CAMBIO FILIALE
008900920813     C   KS              SETON                                        04
009000920813     C   KS              GOTO      FOR01
009100941109     C*
009200941109     C* 04 ON - CONTROLLO FILIALE IN GESTIONE
009300941109     C   04              EXSR      CTRFGS
009400941109     C   04
009500941109     CAN 90              GOTO      FOR01
009600920813     C*
009700920813     C*
009800900509     C* CONTROLLI FORMATO1
009900900509     C                   EXSR      CONTR
010000911025     C  NKF
010100911025     COR 90              GOTO      FOR01
010200901115     C*
010300911025     C* CMD6 - FORZATURA
010400911025     C   KF              EXSR      FORZ
010500920706     C   KF
010600920706     CANN02              GOTO      INIZIO
010700920706     C   KF
010800920706     CAN 02              GOTO      FINE
010900911023     C*
011000000000     C     FINE          TAG
011100911025     C** LANCIO PGM DI ABILITAZIONE BOLLE ALLA CONSEGNA
011200040505     C***  *IN01         IFEQ      *ON
011300040505     C***                MOVEL(P)  'KSDIFFE'     KQIEB
011400040505     C***                MOVEL     'LR27'        KCOAZ
011500040505     C***                CALL      'BCH10'
011600040505     C***                PARM                    KPJBA
011700040505     C***                CLEAR                   KQIEB
011800040505     C***                ENDIF
011900941109     C*
012000941109     C* CHIUDO PGM TRUL06R
012100941109     C                   MOVEL     'C'           D06TLA
012200941109     C                   MOVEL     DSUL06        KPJBU
012300941109     C                   CALL      'TRUL06R'
012400941109     C                   PARM                    KPJBA
012500941109     C*
012600000000     C                   SETON                                        LR
012700911030     C*
012800941109     C* OPERAZIONI INIZIALI ------------------------------------------*
012900941109     C     *INZSR        BEGSR
013000941212     C*
013100941212     C     *ENTRY        PLIST
013200941212     C                   PARM                    KPJBA
013300941212     C                   MOVEL     KPJBU         DSLR02
013400941109     C**
013500941109     C* KLIST
013600941109     C**
013700941109     C     KARB          KLIST
013800941109     C                   KFLD                    VIDAAS
013900941109     C                   KFLD                    VIDLNP
014000941109     C                   KFLD                    VIDNRS
014100941109     C                   KFLD                    VIDNSP
014200941109     C*
014300941109     C                   Z-ADD     1             CODUT
014400941109     C                   CALL      'X§PARUT'
014500941109     C                   PARM                    UT§DSE
014600941109     C                   MOVEL     RAGUT         VIDDSA
014700941109     C                   MOVEL     REC80         CNCR80
014800941109     C**
014900020424     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
015000941109     C**
015100020424     C***        REM       IFEQ 'REM'
015200020424     C***        REMFIL    ANDGT*ZEROS
015300020424     C***                  MOVELREMFIL    VIDFIL
015400020424     C***                  SETON                     05
015500020424     C***                  ELSE
015600020424     C***                  MOVELSIMFEL    VIDFIL
015700020424     C***                  END
015800020424     C                   Z-ADD     SIMPOU        VIDFIL
015900020424     C     SIMTPP        IFEQ      '2'
016000020424     C     SIMTPP        OREQ      *BLANKS
016100020424     C                   SETON                                        05
016200020424     C                   ENDIF
016300930205     C*
016400930205     C                   EXSR      CARL6
016500911030     C*
016600911030     C                   ENDSR
016700920813     C*
016800920813     C* CARICO L6 ------------------------------------------***********
016900920813     C     CARL6         BEGSR
017000941109     C*
017100941109     C                   CLEAR                   DSUL06
017200941109     C                   MOVE      '£6'          D06COD
017300941109     C                   MOVEL     VIDFIL        D06KEY
017400941109     C                   MOVEL     DSUL06        KPJBU
017500941109     C                   CALL      'TRUL06R'
017600941109     C                   PARM                    KPJBA
017700941109     C                   MOVEL     KPJBU         DSUL06
017800941109     C                   MOVEA     LIN           L6
017900920813     C                   ENDSR
018000900509     C*
018100900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
018200900509     C     CONTR         BEGSR
018300911025     C                   SETOFF                                       90
018400911025     C* SPEDIZIONE INESISTENTE O GIA' CONSEGNATA
018500941109     C     KARB          CHAIN     FNARB000                           30
018600911025     C   30              SETON                                        4190
018700911025     C   30              GOTO      ENDCTR
018800941109     C*
018900911030     C* SPEDIZIONE NON GESTIBILE
019000911030     C     ARBLNA        LOOKUP    L6                                     30
019100911030     C  N30              SETON                                        4490
019200911030     C  N30              GOTO      ENDCTR
019300911025     C*
019400911025     C* SPEDIZIONE IN CONSEGNA
019500911025     C     VIDAD         IFEQ      'A'
019600920813     C     ARBAMA        ANDNE     0
019700911025     C                   SETON                                        4290
019800911025     C                   GOTO      ENDCTR
019900911025     C                   END
020000911025     C* SPEDIZIONE NON FORZATA IN ABILITAZIONE
020100911025     C     VIDAD         IFEQ      'D'
020200920813     C     ARBAMA        ANDNE     3
020300911025     C                   SETON                                        4390
020400911025     C                   GOTO      ENDCTR
020500911025     C                   END
020600911023     C*
020700900514     C     ENDCTR        ENDSR
020800911025     C*
020900911025     C*--- FORZATURA BILITAZIONE DISABILITAZIONE ---------------------*
021000911025     C     FORZ          BEGSR
021100911025     C* ABILITAZIONE
021200911025     C     VIDAD         IFEQ      'A'
021300911025     C                   MOVEL     3             ARBAMA
021400941109     C                   UPDATE    FNARB000
021500911025     C                   END
021600911025     C* DISABILITAZIONE
021700911025     C     VIDAD         IFEQ      'D'
021800911025     C                   MOVEL     0             ARBAMA
021900970924     C     ARBFBS        IFEQ      ' '
022000911025     C                   MOVEL     'S'           ARBFBS
022100970924     C                   ELSE
022200970924     C     ARBFBS        IFEQ      'V'
022300040505     C     ARBFBS        oreq      'P'
022400970924     C                   MOVEL     'E'           ARBFBS
022500970924     C                   ENDIF
022600970924     C                   ENDIF
022700941109     C                   UPDATE    FNARB000
022800040505     C***                SETON                                        01
022900911025     C                   END
023000911025     C*
023100911025     C                   ENDSR
023200941109     C*
023300941109     C* CONTROLLO LA FILIALE IN GESTIONE -----------------------------*
023400941109     C     CTRFGS        BEGSR
023500941109     C                   SETOFF                                       90
023600941109     C                   CLEAR                   DSLV50
023700941109     C                   MOVEL     KNMUS         D50PRU
023800941109     C                   MOVEL     VIDFIL        D50FGS
023900941109     C                   CALL      'FNLV50R'
024000941109     C                   PARM                    DSLV50
024100941109     C*
024200941109     C     D50ERR        IFNE      ' '
024300941109     C                   MOVEL     D50MSG        VIDMSG
024400941109     C                   SETON                                        499028
024500941109     C                   GOTO      ENDCTF
024600941109     C                   ENDIF
024700941109     C* CARICO LA L6
024800941109     C                   EXSR      CARL6
024900941109     C*
025000941109     C     ENDCTF        ENDSR
