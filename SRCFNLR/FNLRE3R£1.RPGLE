000100000000     H DECEDIT('0,') DATEDIT(*YMD.)
000200000000      **************************************************************************
000300941228      *  Nome programma:  FNLRE5R
000400000000      *  Descrizione   :  Stampa statistica arrivi.
000500000000      *                   Programma di lancio.
000600000000      *  Data creazione:  03 FEB 1994
000700000000      **************************************************************************
000800000000      *  La stampa vera e propria e' divisa in due parti: l'elaborazione e la
000900000000      *  stampa; questo per semplificare i passaggi ed agevolare la richiesta
001000000000      *  di RISTAMPA (R).
001100971126      *
001200971126      *  ATTENZIONE!!! QUESTO PGM SE LANCIATO IN SEDE NON VA BENE
001300971126      *                PERCHE' A VIDEO, AL POSTO DELLA FILIALE ELABORA-
001400971126      *                TORE DEVE RICHIEDERE IL TERMINAL DI PARTENZA.
001500971126      *                QUESTO E' DIVENTATO NECESSARIO IN SEGUITO ALLA
001600971126      *                CREAZIONE DEL SISTEMA INFORMATIVO DELLA S.D.I.
001700021031      *
001800021031      * 31/10/02 il lancio da sede è stato eliminato
001900971126      *
002000000000      **************************************************************************
002100950214      *  ??PASSAGGIO PARAMETRI:?
002200950214      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
002300950214      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
002400950214      *  DS1PGM   NOME PROGRAMMA
002500950214      *  DS1FPG   FLAG PROGRAMMA
002600950214      *  DS1FEL   FILIALE ELABORATORE (AREA CHE SI VUOLE STAMPARE)
002700950216      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA (E' LA LNA)
002800950214      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
002900950214      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
003000950214      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
003100950214      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
003200950404      *  DS1RIS   'R'=RISTAMPA, 'S'=ELABOR.+STAMPA, ' '=SOLO ELABOR.
003300970207      *  DS1FPG   STATISTICA SELEZIONATA  ( ' '=TUTTE )
003400950214      **************************************************************************
003500000000     FAZORG01L  IF   E           K DISK
003600000000     FTABEL00F  IF   E           K DISK
003700020916     FFNBTP11L  IF   E           K DISK
003800040209     F***AZCAE05L  IF   E           K DISK
003900941228     FFNLRE5D   CF   E             WORKSTN
004000000000      *
004100040219     D L1              S              3  0 DIM(30)                              FIL GESTITE
004200000000     D L6              S              3  0 DIM(30)
004300040209     D*FIL             S              3    DIM(29)
004400000000      *
004500950213     D DSLS01        E DS
004600000000     D WLBDAT          DS
004700941228     D  G02DAT                 1      8  0
004800941228     D  G02INV                 9     16  0
004900941228     D  G02ERR                17     17
005000941228     D  G02TGI                18     22  0
005100000000     D WGIDAT          DS
005200941228     D  GIODAT                 1      8  0
005300941228     D  GIOINV                 9     16  0
005400941228     D  GIOTGI                17     21  0
005500000000     D KPJBA         E DS
005600020424     D***                                     1   3 REM
005700020424     D***                                     4   6 REMFIL
005800000000     D DS5A          E DS
005900040923     D*DS5A2         E DS
006000040923     D DS5AS1        E DS
006100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006200950213     D* DS PER TRUL06R - CARICAMENTO £X
006300950213     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006400950314     D  T06                    1     90  0
006500950314     D                                     DIM(30)                              TRUL06R
006600000000     D CNCR80        E DS
006700021016
006800021016     d Tibs02Ds      e ds
006900021016      * Ds date statistiche
007000021016     d dSdf          e ds
007100040209     d fnlv54ds      e ds
007200040209      * Parametri per richiamo a TRUL09R
007300040209     d Trul09ds      e ds
007400040209     d  T09                   19    108  0 dim(30)
007500021016
007600000000     D                 DS
007700000000     D  TUTTE                  1     20    INZ('0 = TUTTE          ')
007800000000      *---------------------------------------------------------------
007900000000      * INDICATORI USATI
008000000000      *---------------------------------------------------------------
008100940426      * 43 - FIL.ELABOR. NON CONGRUENTE CON CD.FILIALE
008200000000      * 44 - ELABORAZIONE MOMENTANEAMENTE NON EFFETTUABILE
008300000000      * 45 - ELABORAZIONE NON EFFETTUABILE PER MANCANZA DI DATI
008400000000      * 46 - PER S=STAMPA SI PUO' SCEGLIERE SOLO UN GIORNO
008500000000      * 48 - PER S=STAMPA NON SI PUO' SCEGLIERE UNA SINGOLA FILIALE
008600000000      * 90 - GENERICO DI ERRORE
008700000000      *---------------------------------------------------------------
008800000000      *
008900000000      * DEFINIZIONE VARIABILI, KLIST E PLIST
009000000000     C                   EXSR      DEFVAR
009100000000      *
009200040209      * CARICAMENTO TABELLE
009300000000     C                   EXSR      CARTAB
009400000000      *
009500000000     C     INIZIO        TAG
009600000000     C                   Z-ADD     0             VIDDSD
009700000000     C                   Z-ADD     0             VIDDSA
009800000000     C                   Z-ADD     0             COMDSA
009900040209     c* tipo stampa e p.o. li propongo in base a chi lancia:
010000040209     c* se chi lancia è terminal di arrivo prpongo 0 e "T"
010100040209     c* altrimenti p.o.=chi lancia e tipo stampa ="L"
010200040209     c                   clear                   fnlv54ds
010300040209     c                   movel     'L'           i54tla
010400040209     c                   movel     'A'           i54tpt
010500040209     c                   z-add     vidfil        i54lin
010600040209     c                   z-add     wdate1        i54drf
010700040209     c                   call      'FNLV54R'
010800040209     c                   parm                    fnlv54ds
010900040209     c     o54err        ifeq      *blanks
011000040209     c     o54tfa        andeq     vidfil
011100040209     c                   clear                   vidlna
011200040209     c                   move      'T'           vidsta
011300040209     c                   else
011400040209     c                   z-add     vidfil        vidlna
011500040209     c                   move      'L'           vidsta
011600040209     c                   endif
011700040209     C*****              MOVE      'T'           VIDSTA                         TIPO STAMPA
011800000000     C                   MOVE      'R'           STARIS                         STAMPA/RISTAMPA
011900000000     C                   MOVE      TUTTE         DELNA
012000000000      *
012100000000     C     FOR01         TAG
012200941228     C                   EXFMT     LRE5D01
012300000000      *
012400000000      * CMD3 - FINE LAVORO
012500000000     C   KC              GOTO      FINE
012600000000      *
012700000000      * CONTROLLI
012800000000     C                   EXSR      CONTR
012900941230     C  NKF                                                                     F1=CONFERMA
013000000000     COR 90              GOTO      FOR01
013100000000      *
013200000000      * Se la data finale e' vuota, la riempo con quella iniziale
013300000000     C     COMDSA        IFEQ      *ZERO
013400000000     C                   Z-ADD     COMDSD        COMDSA
013500000000     C                   Z-ADD     VIDDSD        VIDDSA
013600000000     C                   ENDIF
013700950213     C*
013800950213     C                   CLEAR                   DSLS01
013900950214     C                   MOVEL     'A'           DS1TBO                         'A'=ARRIVI
014000950213     C                   Z-ADD     VIDDSD        DS1GDA                         GG/MM/AAAA DAL
014100950213     C                   Z-ADD     VIDDSA        DS1GAL                         GG/MM/AAAA AL
014200950213     C                   Z-ADD     COMDSD        DS1ADA                         AAAA/MM(GG DAL
014300950213     C                   Z-ADD     COMDSA        DS1AAL                         AAAA/MM/GG AL
014400950213     C                   Z-ADD     VIDFIL        DS1FGS
014500950313     C* METTO LA LNA IN DS1LNP PER UNIFORMARMI ALLE STAMPE GIORNALIERE
014600950213     C                   Z-ADD     VIDLNA        DS1LNP                         E' LA LNA
014700950213     C                   MOVEL     STARIS        DS1RIS
014800021031     c                   Z-Add     Simfel        Ds1Fel
014900950216     C                   MOVEL     VIDSTA        DS1FL1                         TIPO STAMPA
015000970207     C                   MOVEL     'FNLRE7C'     DS1PGM
015100970207     C* TIPO STATISTICA A=ARRIVI
015200970207     C                   MOVEL     'A'           DS1FPG
015300000000      *
015400970207     C                   MOVE      'LSC8'        KCOAZ
015500950213     C                   CLEAR                   KPJBU
015600950213     C                   MOVEL     DSLS01        KPJBU
015700000000     C                   CALL      'BCH10'
015800000000     C                   PARM                    KPJBA
015900970604     C*
016000970604     C* SOTTOMETTO TRASMISSIONE STATISTICA SE RIELABORAZIONE DATI
016100040923     C**** STARIS        IFEQ      'S'
016200040923     C****               CLEAR                   KPJBU
016300040923     C****               MOVEL     'LSC6'        KCOAZ
016400040923     C****               CALL      'BCH10'
016500040923     C****               PARM                    KPJBA
016600040923     C****               ENDIF
016700000000      *
016800000000     C     FINE          TAG
016900040209     c                   clear                   trul09ds
017000040209     c                   movel     'C'           i09tla
017100040209     c                   call      'TRUL09R'
017200040209     c                   parm                    trul09ds
017300971126     C*
017400000000     C                   SETON                                        LR
017500000000      **************************************************************************
017600000000      * ROUTINE DI CONTROLLO DATI FORMATO VIDEO
017700000000      **************************************************************************
017800000000     C     CONTR         BEGSR
017900000000      *
018000971126      * DATA ARRIVO DAL OBBLIGATORIA
018100971126     C                   MOVE      VIDDSD        G02DAT
018200971126     C                   MOVEL     *BLANK        G02ERR
018300971126     C                   CALL      'XSRDA8'
018400971126     C                   PARM                    WLBDAT
018500990413    1C     G02ERR        IFEQ      '1'
018600971126     C                   SETON                                        40  90
018700971126     C                   GOTO      ENDCTR
018800990413    1C                   ENDIF
018900971126     C                   Z-ADD     G02DAT        VIDDSD
019000971126     C                   Z-ADD     G02INV        COMDSD            8 0
019100971126     C*
019200000000      * SE C'E', DECODIFICO LA FILIALE
019300000000     C                   MOVE      TUTTE         DELNA
019400990413    1C     VIDLNA        IFNE      *ZERO
019500000000     C     VIDLNA        CHAIN     AZORG01L                           90
019600990413    2C  N90              DO
019700940426     C                   MOVE      *BLANKS       DELNA
019800940426     C                   MOVEL     ORGDES        DELNA
019900940426      *
020000990413    2C                   ENDDO
020100990413    1C                   ENDIF
020200000000      *
020300000000     C                   SETOFF                                       9002
020400040209     c*
020500040209     c* richiamo trul09r per sapere se chi lancia è ter-arr e di chi
020600040209     c                   clear                   trul09ds
020700040209     c                   eval      i09mod = 'TFU'
020800040209     c                   eval      i09tfa = vidfil
020900040209     c                   eval      i09dta = comdsd
021000040209     c                   call      'TRUL09R'
021100040209     c                   parm                    trul09ds
021200040209    2c     o09err        ifeq      *blanks
021300040209     c     t09(1)        andgt     *zeros
021400040209     c     vidlna        ifgt      *zeros
021500040209     c     vidlna        lookup    t09                                    31
021600040219     c* faccio anche lookup in £1 in quanto i secondi livello in arrivo su
021700040219     c* altro as devono poter essere lanciati anche dal loro terminal di
021800040219     c* partenza finchè il loro terminal di arrivo si trova su altro ogas
021900040219     c* visto che nel menù dei secondi liv. non c'è il richiamo al lancio
022000040219     c* statistica arrivi
022100040219     c     vidlna        lookup    l1                                     32
022200040219     c     *in31         ifeq      *off
022300040219     c     *in32         andeq     *off
022400040219     c                   seton                                        5190
022500040219     c                   goto      endctr
022600040219     c                   endif
022700040209     c                   endif
022800040209     c                   else
022900040209     c* p.o. non ter-arr può lanciare solo se stesso e solo per linea
023000040209     c     vidlna        ifne      vidfil
023100040209     c                   setoff                                       31
023200040209     c     vidlna        ifgt      0
023300040209     c     vidlna        lookup    l6                                     31
023400040209     c                   else
023500040209     c                   setoff                                       31
023600040209     c                   endif
023700040209     c  n31              seton                                        5190
023800040209     c  n31              goto      endctr
023900040209     c                   endif
024000040209     c     vidsta        ifne      'L'
024100040209     c                   seton                                        5290
024200040209     c                   goto      endctr
024300040209     c                   endif
024400040209     c                   endif
024500000000      *
024600000000      * SE SONO SIMFEL POSSO RICHIEDERE TUTTE ALTRIMENTI NO
024700040209    1C***  VIDLNA        IFNE      0
024800000000      *
024900000000      * CONTROLLO SE LINEA VALIDA
025000040209     C***  VIDLNA        LOOKUP    L1                                     31
025100940714     C*
025200940714     C* SE NON C'E IN L1 VEDO SE E' IL MIO TERMINAL
025300040209    2C***  *IN31         IFEQ      *OFF
025400040209     C***                MOVE      *BLANKS       WTROV             1
025500040209     C***  KCAE5         SETLL     AZCAE05L
025600040209     C***  KCAE5         READE     AZCAE05L                               30
025700040209    3C***  *IN30         DOWEQ     *OFF
025800040209     C***  WTROV         ANDEQ     *BLANKS
025900040209    4C***  CAEATB        IFEQ      *BLANKS
026000040209     C***  COMDSD        ANDGE     CAEDDE
026100040209     C***  COMDSD        ANDLE     CAEDSC
026200040209     C***                MOVEL     '1'           WTROV
026300040209    4C***                END
026400040209     C***  KCAE5         READE     AZCAE05L                               30
026500040209    3C***                ENDDO
026600940714     C*
026700040209    3C***  WTROV         IFEQ      *BLANKS
026800040209     C***                SETON                                        5190
026900040209     C***                GOTO      ENDCTR
027000040209    3C***                ENDIF
027100040209    2C***                ENDIF
027200000000      *
027300040209    2C***  VIDFIL        IFNE      SIMFEL
027400040209     C***  VIDLNA        ANDNE     VIDFIL
027500040209     C***  VIDLNA        LOOKUP    L6                                     31
027600040209     C**N31              SETON                                        5190
027700040209     C**N31              GOTO      ENDCTR
027800040209    2C***                ENDIF
027900040209    1C***                ENDIF
028000000000      *
028100000000      * CONTROLLO DATA ARRIVO AL
028200950213     C                   Z-ADD     *ZERO         COMDSA            8 0          DATA AL(AAMMGG)
028300990413    1C     VIDDSA        IFNE      0
028400000000     C                   MOVE      VIDDSA        G02DAT
028500000000     C                   MOVEL     *BLANK        G02ERR
028600941228     C                   CALL      'XSRDA8'
028700000000     C                   PARM                    WLBDAT
028800990413    2C     G02ERR        IFEQ      '1'
028900000000     C                   SETON                                        41  90
029000000000     C                   GOTO      ENDCTR
029100990413    2C                   ENDIF
029200980119     C*
029300990413      * La data finale non puo' essere superiore a OGGI
029400990413    2C     G02INV        IFGT      WDATE1
029500990413     C     COMDSD        ORGT      WDATE1
029600980119     C                   SETON                                        49  90
029700980119     C                   GOTO      ENDCTR
029800990413    2C                   ENDIF
029900941230     C                   Z-ADD     G02DAT        VIDDSA
030000941228     C                   Z-ADD     G02INV        COMDSA
030100000000      *
030200000000      * DATA DAL NO MAGGIORE DATA AL
030300990413    2C     COMDSD        IFGT      COMDSA
030400000000     C                   SETON                                        4290
030500000000     C                   GOTO      ENDCTR
030600990413    2C                   ENDIF
030700990413    1C                   ENDIF
030800000000      *
030900940517      * I test sulle pulizie degli archivi devono essere fatte solo se:
031000940517      *   .  richiesta la 'S'=Stampa
031100940517      *   .  oppure data DAL e' precedente alla 1° data della data area
031200950404     C     STARIS        IFEQ      'S'                                          'S'=ELAB.+STAMPA
031300021016     C**!!!COMDSD        ORLT      E6DAPE                                       DAL < DTAARA
031400021016     c     COMDSD        ORLT      §SdfArrP                                     DAL < DTAARA
031500000000      *
031600941228      *?   Test pulizia archivio bolle arrivo (FNARB00F, §5AARB)   ?
031700000000     C                   MOVE      *BLANKS       PGM
031800941228     C                   MOVEL     'FNLR84R'     PGM                            ARB-LR84
031900000000     C                   Z-ADD     §5AARB        $DATPU                         ARB
032000000000     C                   EXSR      TESTF
032100000000     C   45              GOTO      ENDCTR
032200000000      *
032300941228      *?   Test pulizia archivio bolle transito (FNBTP00F, §5ABTP)   ?
032400000000      * 1) SOLO SE CI SONO DEI RECORD
032500000000      * 2) SOLO SE LNA = 0  O = SIMFEL
032600000000     C     VIDLNA        IFEQ      0
032700000000     C     VIDLNA        OREQ      SIMFEL
032800000000      *
032900020916     C     SIMFEL        SETLL     FNBTP11L
033000020916     C     SIMFEL        READE     FNBTP11L                               30
033100000000     C                   MOVE      *BLANKS       PGM
033200000000     C     *IN30         IFEQ      *OFF
033300941228     C                   MOVEL     'FNLR85R'     PGM                            BTP-LR85
033400000000     C                   Z-ADD     §5ABTP        $DATPU                         BTP
033500000000     C                   EXSR      TESTF
033600000000     C   45              GOTO      ENDCTR
033700000000     C                   ENDIF
033800000000     C                   ENDIF
033900000000      *
034000941228      *?   Test pulizia archivio spunte arrivi (FNBRV00F, §5ABRV)   ?
034100000000      * per BRV sarebbe piu' giusto controllare i giorni effettivi con il campo
034200000000      * §5ASAC (circa 3 giorni effettivi), ma per semplicita' testo solo il
034300950121      * campo §5ABRV (gg. reali) che contiene circa 15 giorni
034400000000     C                   MOVE      *BLANKS       PGM
034500941228     C                   MOVEL     'FNLR10R'     PGM                            BRV-LR10
034600000000     C                   Z-ADD     §5ABRV        $DATPU                         BRV
034700000000     C                   EXSR      TESTF
034800000000     C   45              GOTO      ENDCTR
034900000000      *
035000941228      *?   Test pulizia archivio fogli partenze altre filiali (FVA)   ?
035100000000     C                   MOVE      *BLANKS       PGM
035200950121     C                   MOVEL     'FNLRA7R'     PGM                            FVA-LRA7
035300000000     C                   Z-ADD     §5AFVA        $DATPU                         FVA
035400000000     C                   EXSR      TESTF
035500000000     C   45              GOTO      ENDCTR
035600000000      *
035700941228      *?   Test pulizia archivio fogli vari (FNFVV00F, §5FVV)   ?
035800000000     C                   MOVE      *BLANKS       PGM
035900941228     C                   MOVEL     'FNLRA7R'     PGM                            FVV-LRA7
036000941228     C                   Z-ADD     §5AFVV        $DATPU                         FVV
036100000000     C                   EXSR      TESTF
036200000000     C   45              GOTO      ENDCTR
036300941228      *
036400941228      *?   Test pulizia archivio fogli viaggio partenze (FNFGV00F, §5FGV)   ?
036500941228     C                   MOVE      *BLANKS       PGM
036600941228     C                   MOVEL     'FNLRA7R'     PGM                            FGV-LRA7
036700941228     C                   Z-ADD     §5AFGV        $DATPU                         FGV
036800941228     C                   EXSR      TESTF
036900941228     C   45              GOTO      ENDCTR
037000940517     C                   ENDIF
037100000000      *
037200970718      *?   Test pulizia archivio statistiche arrivi (FNSAR00F, §5ASTA)   ?
037300000000     C     STARIS        IFEQ      'R'                                          'R'=RISTAMPA
037400000000     C                   MOVE      *BLANKS       PGM
037500941228     C                   MOVEL     'FNLRE4R'     PGM                            STA-LRE4
037600040923     C***                Z-ADD     §5ASTA        $DATPU            6 0          STA
037700040923     C                   Z-ADD     §5ASTS        $DATPU            6 0          STA
037800000000     C                   EXSR      TESTF
037900000000     C   45              GOTO      ENDCTR
038000000000     C                   ENDIF
038100000000      *
038200000000      * PER S=STAMPA SI PUO' SCEGLIERE SOLO UN GIORNO
038300950404     C     STARIS        IFEQ      'S'                                          'S'=ELAB.+STAMPA
038400000000     C     VIDDSA        ANDNE     VIDDSD
038500000000     C     VIDDSA        ANDNE     *ZERO
038600000000     C                   SETON                                        46  90
038700000000     C                   ENDIF
038800000000      *
038900000000      * PER S=STAMPA NON SI POSSONO SCEGLIERE SINGOLE FILIALI
039000950404     C     STARIS        IFEQ      'S'                                          'S'=ELAB.+STAMPA
039100000000     C     VIDLNA        ANDNE     *ZERO
039200000000     C                   SETON                                        48  90
039300000000     C                   ENDIF
039400000000      *
039500000000     C     ENDCTR        ENDSR
039600000000      **************************************************************************
039700000000      * Viene rilevato l'ultimo giorno di utilizzo del pgm che si
039800000000      * occupa della pulizia archivio (ogni file, o quasi, ha il proprio pgm);
039900970718      * nella tabella 5A  c'e' il nr.giorni di mantenimento dei dati; si
040000000000      * sottrae quest'ultimo dal giorno ultimo utilizzo per sapere se vi sono
040100000000      * ancora disponibili i dati per la statistica.
040200000000      **************************************************************************
040300000000     C     TESTF         BEGSR
040400000000      *
040500000000     C                   CLEAR                   PRIDAT
040600000000      *
040700000000      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA STATISTICHE ARRIVI
040800000000     C                   MOVE      *BLANKS       DAT
040900000000     C                   MOVE      *BLANKS       ERR
041000941230     C                   CALL      'TRUL49C'                            99
041100000000     C                   PARM                    PGM              10
041200000103     C                   PARM                    DAT               8
041300000000     C                   PARM                    ERR               1
041400000000      *
041500941228      * PROGRAMMA IN USO DATA ULTIMO ULIZZO = WDATE
041600000000     C     ERR           IFEQ      '1'
041700040923     c     dat           oreq      '00000000'
041800941228     C                   Z-ADD     WDATE1        G02INV
041900000000     C                   ELSE
042000000103     C                   MOVE      DAT           G02INV                         A SINISTRA
042100000000     C                   ENDIF
042200000000      *
042300000000      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
042400000000      * PULIZIA E TROVO LA PRIMA DATA AL DI SOTTO DELLA QUALE NON SI
042500000000      * POSSONO LANCIARE ELABORAZIONI PER MANCANZA DI DATI
042600000000      *
042700000000      * SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
042800000000     C     G02INV        IFNE      *ZEROS
042900000000     C                   Z-ADD     *ZEROS        G02DAT
043000000000     C                   Z-ADD     *ZEROS        G02TGI
043100000000     C                   MOVEL     '3'           G02ERR
043200941228     C                   CALL      'XSRDA8'
043300000000     C                   PARM                    WLBDAT
043400000000      *
043500000000     C                   SUB       $DATPU        G02TGI                         GG PULIZIA
043600000000     C                   Z-ADD     *ZEROS        GIODAT
043700000000     C                   Z-ADD     *ZEROS        GIOINV
043800000000     C                   Z-ADD     G02TGI        GIOTGI
043900941228     C                   CALL      'XSRGI8'
044000000000     C                   PARM                    WGIDAT
044100000000      *
044200941228     C                   Z-ADD     GIOINV        PRIDAT            8 0
044300000000     C                   ENDIF
044400000000      *
044500000000      * DATA DAL MAGGIORE DATA CALCOLATA
044600000000     C     COMDSD        IFLE      PRIDAT
044700000000     C                   SETON                                        4590
044800000000     C                   ENDIF
044900000000      *
045000000000     C                   ENDSR
045100000000      **************************************************************************
045200000000      * ROUTINE DI CARICAMENTO TABELLA £1 E £6
045300000000      **************************************************************************
045400000000     C     CARTAB        BEGSR
045500000000      *
045600000000      * VEDO SE SONO SIMFEL O REMOTO
045700020424     C***        REM       IFEQ 'REM'
045800040209     C***        REMFIL    ANDGT*ZEROS
045900020424     C***                  MOVELREMFIL    VIDLNA
046000020424     C***                  MOVELREMFIL    VIDFIL  30
046100020424     C***                  ELSE
046200020424     C***                  MOVELSIMFEL    VIDLNA
046300020424     C***                  MOVELSIMFEL    VIDFIL
046400020424     C***                  END
046500040209     C***                Z-ADD     SIMPOU        VIDLNA
046600020424     C                   Z-ADD     SIMPOU        VIDFIL            3 0
046700000000      *
046800000000      * CARICO TABELLA FILIALI GESTITE £1
046900040219     C                   CLEAR                   DSUL06
047000040219     C                   MOVE      '£1'          D06COD
047100040219     C                   MOVEL     SIMFEL        D06KEY
047200040219     C                   MOVEL     DSUL06        KPJBU
047300040219     C                   CALL      'TRUL06R'
047400040219     C                   PARM                    KPJBA
047500040219     C                   MOVEL     KPJBU         DSUL06
047600040219     C                   MOVEA     T06           L1
047700000000      *
047800000000      * CARICO TABELLA FILIALI GESTITE £6
047900950213     C                   CLEAR                   DSUL06
048000950213     C                   MOVE      '£6'          D06COD
048100950213     C                   MOVEL     VIDFIL        D06KEY
048200950213     C                   MOVEL     DSUL06        KPJBU
048300950213     C                   CALL      'TRUL06R'
048400950213     C                   PARM                    KPJBA
048500950213     C                   MOVEL     KPJBU         DSUL06
048600950314     C                   MOVEA     T06           L6
048700000000      *
048800000000      * Prelevo le due tabelle contenenti i giorni di pulizia degli archivi
048900000000     C                   Z-ADD     1             TBLKUT
049000000000     C                   MOVE      '5A'          TBLCOD
049100000000     C                   MOVEL     *BLANKS       TBLKEY
049200000000     C                   MOVEL     '1'           TBLKEY
049300000000     C     KTAB          CHAIN     TABEL00F                           30
049400000000     C   30              CLEAR                   DS5A
049500000000     C  N30              MOVEL     TBLUNI        DS5A
049600970718     C                   MOVEL     '5A'          TBLCOD
049700040923     C****               MOVEL(P)  '2'           TBLKEY
049800040923     C                   MOVEL(P)  'SEDE1'       TBLKEY
049900000000     C     KTAB          CHAIN     TABEL00F                           30
050000040923     C** 30              CLEAR                   DS5A2
050100040923     C**N30              MOVEL     TBLUNI        DS5A2
050200040923     C   30              CLEAR                   DS5AS1
050300040923     C  N30              MOVEL     TBLUNI        DS5AS1
050400021016      * Cerco la data ultima elaborazione dalla tabella
050500021016     c                   Clear                   Tibs02Ds
050600021016     c                   Clear                   dSdf
050700021016     c                   Eval      T02Mod = 'C'
050800021016     c                   Eval      T02Sif = Knsif
050900021016     c                   Eval      T02Cod = 'SDF'
051000021016     c                   Movel(p)  Simfel        T02Ke1
051100021016     c                   Call      'TIBS02R'
051200021016     c                   Parm                    Kpjba
051300021016     c                   Parm                    Tibs02Ds
051400021016     c                   If        T02Err = *Blanks
051500021016     c                   Movel     T02Uni        dSdf
051600021016     c                   EndIf
051700021016
051800021016     C**!!!              Z-ADD     E6DAUE        G02INV                         AAAA/MM/GG
051900021016     c                   Z-Add     §SdfArrU      G02Inv
052000941230     C                   Z-ADD     *ZERO         G02DAT
052100941230     C                   MOVEL     '3'           G02ERR
052200941230     C                   CALL      'XSRDA8'
052300941230     C                   PARM                    WLBDAT
052400941230     C                   Z-ADD     G02DAT        DAULEL                         GG/MM/AAAA
052500000000      *
052600000000     C                   ENDSR
052700000000      **************************************************************************
052800000000      * ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST
052900000000      **************************************************************************
053000000000     C     DEFVAR        BEGSR
053100000000      *
053200000000     C     *ENTRY        PLIST
053300000000     C                   PARM                    KPJBA
053400000000      *
053500000000     C                   Z-ADD     1             CODUT
053600000000     C                   CALL      'X§PARUT'
053700000000     C                   PARM                    UT§DSE
053800000000     C                   MOVEL     RAGUT         RSUT             20
053900000000     C                   MOVEL     REC80         CNCR80
054000000000      *
054100000000     C     KTAB          KLIST                                                  TABEL00F
054200000000     C                   KFLD                    TBLKUT
054300000000     C                   KFLD                    TBLCOD
054400000000     C                   KFLD                    TBLKEY
054500040209     C***  KCAE5         KLIST                                                  TABEL00F
054600040209     C***                KFLD                    KEPA
054700040209     C***                KFLD                    VIDLNA
054800040209     C***                KFLD                    VIDFIL
054900040209     C***                MOVEL     'A'           KEPA
055000980304     C*
055100040209     C***  *LIKE         DEFINE    CAEEPA        KEPA
055200941228      *
055300941228     C                   TIME                    WTIME            14 0          ORA E DATA
055400941228     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
055500941228      *
055600941228     C                   Z-ADD     WDATE         G02DAT                         GG/MM/AAAA
055700941228     C                   Z-ADD     *ZERO         G02INV
055800941228     C                   MOVEL     *BLANK        G02ERR
055900941228     C                   CALL      'XSRDA8'
056000941228     C                   PARM                    WLBDAT
056100941228     C                   Z-ADD     G02INV        WDATE1            8 0          AAAA/MM/GG
056200000000      *
056300000000     C                   ENDSR
