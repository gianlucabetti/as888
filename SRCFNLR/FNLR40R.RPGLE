000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941109     H* FNLR40R *----------------------------------------------------*
000300911025     H*         - ABILITO DISABILITO CONSEGNA MERCE
000400000000     H*--------------------------------------------------------------*
000500941109     FFNLR40D   CF   E             WORKSTN
000600941109     FFNARB01L  UF   E           K DISK
000700070329     Fazorg01l  iF   E           K DISK
000800911030     D L6              S              3  0 DIM(30)                              FIL GESTITE
000900920706     D* PER INTERROGAZIONE BOLLE ARRIVI
001000911023     D PARAM           DS
001100941109     D  GIA                    1     12  0
001200941109     D  RICH                  13     13
001300941109     D  VI5AAS                14     17  0
001400941109     D  VI5LNP                18     20  0
001500941109     D  VI5NRS                21     22  0
001600941109     D  VI5NSP                23     29  0
001700941109     D  VI5KEY                14     29
001800941109     D  VIDFLG                31     31
001900941109     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
002000941109     D DSLV50        E DS                  EXTNAME(FNLV50DS)
002100941109     D* DS PER TRUL06R - CARICAMENTO £X
002200941109     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002300941109     D  LIN                    1     90  0
002400941109     D                                     DIM(30)                              FIL. COMODO
002500941109     D* PASSAGGIO PARAMETRI
002600941109     D DSLR02        E DS
002700911024     D KPJBA         E DS
002800911024     D CNCR80        E DS
002900070329     D og148         E DS
003000911024     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003100900518     C****************************************************************
003200900518     C*  RIEPILOGO INDICATORI
003300900518     C***************************************************************
003400920706     C* 01    - LANCIO PGM ABILITAZIONE MERCE
003500920706     C* 02    - PGM RICHIAMATO CON KEY
003600920813     C* 04    - CAMBIO FILIALE  CMD18
003700920813     C* 05    - REMOTO
003800941109     C* 28    - EMETTO VIDMSG
003900911023     C* 30/31 - LETTURA COMODO
004000911025     C* 40-45 - ERRORI
004100911025     C* 58-59 - ERRORI
004200900518     C* 90    - INDICA LA PRESENZA DI UN ERRORE
004300900518     C*****************************************************************
004400911025     C     INIZIO        TAG
004500920813     C                   SETOFF                                       04
004600911025     C* PULIZIA CAMPI
004700941109     C                   TIME                    W0140            14 0
004800941109     C                   MOVE      W0140         VIDAAS
004900911025     C                   MOVEL     *ZEROS        VIDLNP
005000911025     C                   MOVEL     *ZEROS        VIDNRS
005100911025     C                   MOVEL     *ZEROS        VIDNSP
005200911025     C                   MOVEL     'A'           VIDAD
005300920706     C*
005400941109     C* DR2RIC = 1 --> PGM RICHIAMAMTO
005500941109     C     DR2RIC        IFEQ      '1'
005600941109     C                   MOVEL     DR2AAS        VIDAAS
005700941109     C                   MOVEL     DR2LNP        VIDLNP
005800941109     C                   MOVEL     DR2NRS        VIDNRS
005900941109     C                   MOVEL     DR2NSP        VIDNSP
006000941109     C                   MOVEL     DR2FGS        VIDFIL
006100920813     C                   SETON                                        0204
006200920706     C                   END
006300911025     C*
006400900509     C     FOR01         TAG
006500941109     C                   EXFMT     LR40D01
006600070329     C** F3  - FINE LAVORO
006700900509     C   KC              GOTO      FINE
006800911025     C**
006900070329     C** F7  - INTERROGAZIONE BOLLE ARRIVI
007000911025     C   KG              DO
007100941109     C                   MOVEL     *ZEROS        VI5KEY
007200911213     C                   MOVEL     '2'           VIDFLG
007300911025     C                   MOVEL     PARAM         KPJBU
007400941109     C                   CALL      'FNLR36R'
007500911025     C                   PARM                    KPJBA
007600911025     C                   MOVEL     KPJBU         PARAM
007700941109     C*
007800911025     C* SE PASSATA KEY LA IMPOSTO
007900941109     C     VI5KEY        IFGT      *ZEROS
008000911025     C                   MOVEL     VI5AAS        VIDAAS
008100911025     C                   MOVEL     VI5LNP        VIDLNP
008200911025     C                   MOVEL     VI5NRS        VIDNRS
008300911025     C                   MOVEL     VI5NSP        VIDNSP
008400911025     C                   END
008500911025     C*
008600911025     C                   GOTO      FOR01
008700911025     C                   END
008800920813     C*
008900070329     C** F18- CAMBIO FILIALE
009000920813     C   KS              SETON                                        04
009100920813     C   KS              GOTO      FOR01
009200941109     C*
009300941109     C* 04 ON - CONTROLLO FILIALE IN GESTIONE
009400070329    1c                   if        *in04
009500070329     C                   EXSR      CTRFGS
009600070329     c
009700070329    2c                   if        *in90
009800070329    3c                   if        dr2vid = ' '
009900070329     C                   GOTO      FOR01
010000070329   x3c                   else
010100070329     c* non gestisce video
010200070329     c                   movel     '1'           dr2err
010300070329     c                   goto      fine
010400070329    3c                   end
010500070329    2c                   endif
010600070329     c
010700070329     c* La prima volta che ho cambiato fil gestione emetto lo stesso
010800070329     c*  il video  anche senza errori
010900070329     c                   setoff                                       04
011000070329    2c                   if        dr2ric=' '
011100070329     c                   goto      for01
011200070329    2c                   endif
011300070329    1c                   endif
011400920813     C*
011500900509     C* CONTROLLI FORMATO1
011600900509     C                   EXSR      CONTR
011700070329     c
011800070329     c* errore primo video
011900070329     c                   if        *in90
012000070329     c* gestisce video
012100070329     c                   if        dr2vid = ' '
012200070329     C                   GOTO      FOR01
012300070329     c                   else
012400070329     c* non gestisce video
012500070329     c                   movel     '1'           dr2err
012600070329     c                   goto      fine
012700070329     c                   end
012800070329     c                   end
012900070329      *
013000070329     c                   if        not *inkf and dr2vid=' '
013100070329     C                   GOTO      FOR01
013200070329     c                   endif
013300901115     C*
013400070329     C* F6 - FORZATURA
013500911025     C   KF              EXSR      FORZ
013600920706     C   KF
013700920706     CANN02              GOTO      INIZIO
013800920706     C   KF
013900920706     CAN 02              GOTO      FINE
014000911023     C*
014100000000     C     FINE          TAG
014200941109     C*
014300941109     C* CHIUDO PGM TRUL06R
014400941109     C                   MOVEL     'C'           D06TLA
014500941109     C                   MOVEL     DSUL06        KPJBU
014600941109     C                   CALL      'TRUL06R'
014700941109     C                   PARM                    KPJBA
014800941109     C*
014900000000     C                   SETON                                        LR
015000911030     C*
015100941109     C* OPERAZIONI INIZIALI ------------------------------------------*
015200941109     C     *INZSR        BEGSR
015300941212     C*
015400941212     C     *ENTRY        PLIST
015500941212     C                   PARM                    KPJBA
015600941212     C                   MOVEL     KPJBU         DSLR02
015700941109     C**
015800941109     C* KLIST
015900941109     C**
016000941109     C     KARB          KLIST
016100941109     C                   KFLD                    VIDAAS
016200941109     C                   KFLD                    VIDLNP
016300941109     C                   KFLD                    VIDNRS
016400941109     C                   KFLD                    VIDNSP
016500941109     C*
016600941109     C                   Z-ADD     1             CODUT
016700941109     C                   CALL      'X§PARUT'
016800941109     C                   PARM                    UT§DSE
016900941109     C                   MOVEL     RAGUT         VIDDSA
017000941109     C                   MOVEL     REC80         CNCR80
017100070329     c
017200020424     C                   Z-ADD     SIMPOU        VIDFIL
017300020424     C     SIMTPP        IFEQ      '2'
017400020424     C     SIMTPP        OREQ      *BLANKS
017500020424     C                   SETON                                        05
017600020424     C                   ENDIF
017700930205     C*
017800930205     C                   EXSR      CARL6
017900911030     C*
018000911030     C                   ENDSR
018100920813     C*
018200920813     C* CARICO L6 ------------------------------------------***********
018300920813     C     CARL6         BEGSR
018400941109     C*
018500941109     C                   CLEAR                   DSUL06
018600941109     C                   MOVE      '£6'          D06COD
018700941109     C                   MOVEL     VIDFIL        D06KEY
018800941109     C                   MOVEL     DSUL06        KPJBU
018900941109     C                   CALL      'TRUL06R'
019000941109     C                   PARM                    KPJBA
019100941109     C                   MOVEL     KPJBU         DSUL06
019200941109     C                   MOVEA     LIN           L6
019300070329     c* Vedo se ha geo attiva
019400070329     c     vidfil        chain     azorg01l
019500070329     c                   if        %found(azorg01l)
019600070329     c                   movel     orgde8        og148
019700070329     c                   else
019800070329     c                   clear                   og148
019900070329     c                   endif
020000070329     c                   if        §oggeot='S'
020100070329     c                   seton                                        03
020200070329     c                   else
020300070329     c                   setoff                                       03
020400070329     c                   endif
020500070329     c
020600920813     C                   ENDSR
020700900509     C*
020800900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
020900900509     C     CONTR         BEGSR
021000911025     C                   SETOFF                                       90
021100911025     C* SPEDIZIONE INESISTENTE O GIA' CONSEGNATA
021200941109     C     KARB          CHAIN     FNARB000                           30
021300911025     C   30              SETON                                        4190
021400911025     C   30              GOTO      ENDCTR
021500941109     C*
021600911030     C* SPEDIZIONE NON GESTIBILE
021700911030     C     ARBLNA        LOOKUP    L6                                     30
021800911030     C  N30              SETON                                        4490
021900911030     C  N30              GOTO      ENDCTR
022000911025     C*
022100911025     C* SPEDIZIONE IN CONSEGNA
022200911025     C     VIDAD         IFEQ      'A'
022300920813     C     ARBAMA        ANDNE     0
022400911025     C                   SETON                                        4290
022500911025     C                   GOTO      ENDCTR
022600911025     C                   END
022700911025     C* SPEDIZIONE NON FORZATA IN ABILITAZIONE
022800911025     C     VIDAD         IFEQ      'D'
022900920813     C     ARBAMA        ANDNE     3
023000911025     C                   SETON                                        4390
023100911025     C                   GOTO      ENDCTR
023200911025     C                   END
023300911023     C*
023400900514     C     ENDCTR        ENDSR
023500911025     C*
023600911025     C*--- FORZATURA BILITAZIONE DISABILITAZIONE ---------------------*
023700911025     C     FORZ          BEGSR
023800911025     C* ABILITAZIONE
023900911025     C     VIDAD         IFEQ      'A'
024000911025     C                   MOVEL     3             ARBAMA
024100941109     C                   UPDATE    FNARB000
024200911025     C                   END
024300911025     C* DISABILITAZIONE
024400911025     C     VIDAD         IFEQ      'D'
024500911025     C                   MOVEL     0             ARBAMA
024600970924     C     ARBFBS        IFEQ      ' '
024700911025     C                   MOVEL     'S'           ARBFBS
024800970924     C                   ELSE
024900970924     C     ARBFBS        IFEQ      'V'
025000040505     C     ARBFBS        oreq      'P'
025100970924     C                   MOVEL     'E'           ARBFBS
025200970924     C                   ENDIF
025300970924     C                   ENDIF
025400941109     C                   UPDATE    FNARB000
025500911025     C                   END
025600911025     C*
025700911025     C                   ENDSR
025800941109     C*
025900941109     C* CONTROLLO LA FILIALE IN GESTIONE -----------------------------*
026000941109     C     CTRFGS        BEGSR
026100941109     C                   SETOFF                                       90
026200941109     C                   CLEAR                   DSLV50
026300941109     C                   MOVEL     KNMUS         D50PRU
026400941109     C                   MOVEL     VIDFIL        D50FGS
026500941109     C                   CALL      'FNLV50R'
026600941109     C                   PARM                    DSLV50
026700941109     C*
026800941109     C     D50ERR        IFNE      ' '
026900941109     C                   MOVEL     D50MSG        VIDMSG
027000941109     C                   SETON                                        499028
027100941109     C                   GOTO      ENDCTF
027200941109     C                   ENDIF
027300941109     C* CARICO LA L6
027400941109     C                   EXSR      CARL6
027500941109     C*
027600941109     C     ENDCTF        ENDSR
