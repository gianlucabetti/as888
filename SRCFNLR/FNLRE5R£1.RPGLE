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
007800090929
007900090929     d wforza          s              1
008000000000      *---------------------------------------------------------------
008100000000      * INDICATORI USATI
008200000000      *---------------------------------------------------------------
008300940426      * 43 - FIL.ELABOR. NON CONGRUENTE CON CD.FILIALE
008400000000      * 44 - ELABORAZIONE MOMENTANEAMENTE NON EFFETTUABILE
008500000000      * 45 - ELABORAZIONE NON EFFETTUABILE PER MANCANZA DI DATI
008600000000      * 46 - PER S=STAMPA SI PUO' SCEGLIERE SOLO UN GIORNO
008700000000      * 48 - PER S=STAMPA NON SI PUO' SCEGLIERE UNA SINGOLA FILIALE
008800000000      * 90 - GENERICO DI ERRORE
008900000000      *---------------------------------------------------------------
009000000000      *
009100000000      * DEFINIZIONE VARIABILI, KLIST E PLIST
009200000000     C                   EXSR      DEFVAR
009300000000      *
009400040209      * CARICAMENTO TABELLE
009500000000     C                   EXSR      CARTAB
009600000000      *
009700000000     C     INIZIO        TAG
009800000000     C                   Z-ADD     0             VIDDSD
009900000000     C                   Z-ADD     0             VIDDSA
010000000000     C                   Z-ADD     0             COMDSA
010100040209     c* tipo stampa e p.o. li propongo in base a chi lancia:
010200040209     c* se chi lancia è terminal di arrivo prpongo 0 e "T"
010300040209     c* altrimenti p.o.=chi lancia e tipo stampa ="L"
010400040209     c                   clear                   fnlv54ds
010500040209     c                   movel     'L'           i54tla
010600040209     c                   movel     'A'           i54tpt
010700040209     c                   z-add     vidfil        i54lin
010800040209     c                   z-add     wdate1        i54drf
010900040209     c                   call      'FNLV54R'
011000040209     c                   parm                    fnlv54ds
011100040209     c     o54err        ifeq      *blanks
011200040209     c     o54tfa        andeq     vidfil
011300040209     c                   clear                   vidlna
011400040209     c                   move      'T'           vidsta
011500040209     c                   else
011600040209     c                   z-add     vidfil        vidlna
011700040209     c                   move      'L'           vidsta
011800040209     c                   endif
011900040209     C*****              MOVE      'T'           VIDSTA                         TIPO STAMPA
012000000000     C                   MOVE      'R'           STARIS                         STAMPA/RISTAMPA
012100000000     C                   MOVE      TUTTE         DELNA
012200000000      *
012300000000     C     FOR01         TAG
012400941228     C                   EXFMT     LRE5D01
012500000000      *
012600000000      * CMD3 - FINE LAVORO
012700000000     C   KC              GOTO      FINE
012800000000      *
012900000000      * CONTROLLI
013000000000     C                   EXSR      CONTR
013100090929     c*   per 45 e per EDP do msg forzabile
013200090929     c                   if        *in45 and
013300090929     c                                   %subst(knmus:1:3)='EDP'
013400090929     c                   if        wforza=' '
013500090929     c                   eval      wforza='S'
013600090929     c                   else
013700090929     c                   setoff                                       4590
013800090929     c                   endif
013900090929     c                   endif
014000090929     C  NKF
014100000000     COR 90              GOTO      FOR01
014200000000      *
014300000000      * Se la data finale e' vuota, la riempo con quella iniziale
014400000000     C     COMDSA        IFEQ      *ZERO
014500000000     C                   Z-ADD     COMDSD        COMDSA
014600000000     C                   Z-ADD     VIDDSD        VIDDSA
014700000000     C                   ENDIF
014800950213     C*
014900950213     C                   CLEAR                   DSLS01
015000950214     C                   MOVEL     'A'           DS1TBO                         'A'=ARRIVI
015100950213     C                   Z-ADD     VIDDSD        DS1GDA                         GG/MM/AAAA DAL
015200950213     C                   Z-ADD     VIDDSA        DS1GAL                         GG/MM/AAAA AL
015300950213     C                   Z-ADD     COMDSD        DS1ADA                         AAAA/MM(GG DAL
015400950213     C                   Z-ADD     COMDSA        DS1AAL                         AAAA/MM/GG AL
015500950213     C                   Z-ADD     VIDFIL        DS1FGS
015600950313     C* METTO LA LNA IN DS1LNP PER UNIFORMARMI ALLE STAMPE GIORNALIERE
015700950213     C                   Z-ADD     VIDLNA        DS1LNP                         E' LA LNA
015800950213     C                   MOVEL     STARIS        DS1RIS
015900021031     c                   Z-Add     Simfel        Ds1Fel
016000950216     C                   MOVEL     VIDSTA        DS1FL1                         TIPO STAMPA
016100970207     C                   MOVEL     'FNLRE7C'     DS1PGM
016200970207     C* TIPO STATISTICA A=ARRIVI
016300970207     C                   MOVEL     'A'           DS1FPG
016400000000      *
016500970207     C                   MOVE      'LSC8'        KCOAZ
016600950213     C                   CLEAR                   KPJBU
016700950213     C                   MOVEL     DSLS01        KPJBU
016800000000     C                   CALL      'BCH10'
016900000000     C                   PARM                    KPJBA
017000970604     C*
017100970604     C* SOTTOMETTO TRASMISSIONE STATISTICA SE RIELABORAZIONE DATI
017200040923     C**** STARIS        IFEQ      'S'
017300040923     C****               CLEAR                   KPJBU
017400040923     C****               MOVEL     'LSC6'        KCOAZ
017500040923     C****               CALL      'BCH10'
017600040923     C****               PARM                    KPJBA
017700040923     C****               ENDIF
017800000000      *
017900000000     C     FINE          TAG
018000040209     c                   clear                   trul09ds
018100040209     c                   movel     'C'           i09tla
018200040209     c                   call      'TRUL09R'
018300040209     c                   parm                    trul09ds
018400971126     C*
018500000000     C                   SETON                                        LR
018600000000      **************************************************************************
018700000000      * ROUTINE DI CONTROLLO DATI FORMATO VIDEO
018800000000      **************************************************************************
018900000000     C     CONTR         BEGSR
019000000000      *
019100971126      * DATA ARRIVO DAL OBBLIGATORIA
019200971126     C                   MOVE      VIDDSD        G02DAT
019300971126     C                   MOVEL     *BLANK        G02ERR
019400971126     C                   CALL      'XSRDA8'
019500971126     C                   PARM                    WLBDAT
019600990413    1C     G02ERR        IFEQ      '1'
019700971126     C                   SETON                                        40  90
019800971126     C                   GOTO      ENDCTR
019900990413    1C                   ENDIF
020000971126     C                   Z-ADD     G02DAT        VIDDSD
020100971126     C                   Z-ADD     G02INV        COMDSD            8 0
020200971126     C*
020300000000      * SE C'E', DECODIFICO LA FILIALE
020400000000     C                   MOVE      TUTTE         DELNA
020500990413    1C     VIDLNA        IFNE      *ZERO
020600000000     C     VIDLNA        CHAIN     AZORG01L                           90
020700990413    2C  N90              DO
020800940426     C                   MOVE      *BLANKS       DELNA
020900940426     C                   MOVEL     ORGDES        DELNA
021000940426      *
021100990413    2C                   ENDDO
021200990413    1C                   ENDIF
021300000000      *
021400000000     C                   SETOFF                                       9002
021500040209     c*
021600040209     c* richiamo trul09r per sapere se chi lancia è ter-arr e di chi
021700040209     c                   clear                   trul09ds
021800100701     c                   eval      i09mod = 'DEF'
021900040209     c                   eval      i09tfa = vidfil
022000040209     c                   eval      i09dta = comdsd
022100040209     c                   call      'TRUL09R'
022200040209     c                   parm                    trul09ds
022300040209    2c     o09err        ifeq      *blanks
022400040209     c     t09(1)        andgt     *zeros
022500040209     c     vidlna        ifgt      *zeros
022600040209     c     vidlna        lookup    t09                                    31
022700040219     c* faccio anche lookup in £1 in quanto i secondi livello in arrivo su
022800040219     c* altro as devono poter essere lanciati anche dal loro terminal di
022900040219     c* partenza finchè il loro terminal di arrivo si trova su altro ogas
023000040219     c* visto che nel menù dei secondi liv. non c'è il richiamo al lancio
023100040219     c* statistica arrivi
023200040219     c     vidlna        lookup    l1                                     32
023300040219     c     *in31         ifeq      *off
023400040219     c     *in32         andeq     *off
023500040219     c                   seton                                        5190
023600040219     c                   goto      endctr
023700040219     c                   endif
023800040209     c                   endif
023900040209     c                   else
024000040209     c* p.o. non ter-arr può lanciare solo se stesso e solo per linea
024100040209     c     vidlna        ifne      vidfil
024200040209     c                   setoff                                       31
024300040209     c     vidlna        ifgt      0
024400040209     c     vidlna        lookup    l6                                     31
024500040209     c                   else
024600040209     c                   setoff                                       31
024700040209     c                   endif
024800040209     c  n31              seton                                        5190
024900040209     c  n31              goto      endctr
025000040209     c                   endif
025100040209     c     vidsta        ifne      'L'
025200040209     c                   seton                                        5290
025300040209     c                   goto      endctr
025400040209     c                   endif
025500040209     c                   endif
025600000000      *
025700000000      * SE SONO SIMFEL POSSO RICHIEDERE TUTTE ALTRIMENTI NO
025800040209    1C***  VIDLNA        IFNE      0
025900000000      *
026000000000      * CONTROLLO SE LINEA VALIDA
026100040209     C***  VIDLNA        LOOKUP    L1                                     31
026200940714     C*
026300940714     C* SE NON C'E IN L1 VEDO SE E' IL MIO TERMINAL
026400040209    2C***  *IN31         IFEQ      *OFF
026500040209     C***                MOVE      *BLANKS       WTROV             1
026600040209     C***  KCAE5         SETLL     AZCAE05L
026700040209     C***  KCAE5         READE     AZCAE05L                               30
026800040209    3C***  *IN30         DOWEQ     *OFF
026900040209     C***  WTROV         ANDEQ     *BLANKS
027000040209    4C***  CAEATB        IFEQ      *BLANKS
027100040209     C***  COMDSD        ANDGE     CAEDDE
027200040209     C***  COMDSD        ANDLE     CAEDSC
027300040209     C***                MOVEL     '1'           WTROV
027400040209    4C***                END
027500040209     C***  KCAE5         READE     AZCAE05L                               30
027600040209    3C***                ENDDO
027700940714     C*
027800040209    3C***  WTROV         IFEQ      *BLANKS
027900040209     C***                SETON                                        5190
028000040209     C***                GOTO      ENDCTR
028100040209    3C***                ENDIF
028200040209    2C***                ENDIF
028300000000      *
028400040209    2C***  VIDFIL        IFNE      SIMFEL
028500040209     C***  VIDLNA        ANDNE     VIDFIL
028600040209     C***  VIDLNA        LOOKUP    L6                                     31
028700040209     C**N31              SETON                                        5190
028800040209     C**N31              GOTO      ENDCTR
028900040209    2C***                ENDIF
029000040209    1C***                ENDIF
029100000000      *
029200000000      * CONTROLLO DATA ARRIVO AL
029300950213     C                   Z-ADD     *ZERO         COMDSA            8 0          DATA AL(AAMMGG)
029400990413    1C     VIDDSA        IFNE      0
029500000000     C                   MOVE      VIDDSA        G02DAT
029600000000     C                   MOVEL     *BLANK        G02ERR
029700941228     C                   CALL      'XSRDA8'
029800000000     C                   PARM                    WLBDAT
029900990413    2C     G02ERR        IFEQ      '1'
030000000000     C                   SETON                                        41  90
030100000000     C                   GOTO      ENDCTR
030200990413    2C                   ENDIF
030300980119     C*
030400990413      * La data finale non puo' essere superiore a OGGI
030500990413    2C     G02INV        IFGT      WDATE1
030600990413     C     COMDSD        ORGT      WDATE1
030700980119     C                   SETON                                        49  90
030800980119     C                   GOTO      ENDCTR
030900990413    2C                   ENDIF
031000941230     C                   Z-ADD     G02DAT        VIDDSA
031100941228     C                   Z-ADD     G02INV        COMDSA
031200000000      *
031300000000      * DATA DAL NO MAGGIORE DATA AL
031400990413    2C     COMDSD        IFGT      COMDSA
031500000000     C                   SETON                                        4290
031600000000     C                   GOTO      ENDCTR
031700990413    2C                   ENDIF
031800990413    1C                   ENDIF
031900090929
032000090929      * PER S=STAMPA SI PUO' SCEGLIERE SOLO UN GIORNO
032100090929     C     STARIS        IFEQ      'S'                                          'S'=ELAB.+STAMPA
032200090929     C     VIDDSA        ANDNE     VIDDSD
032300090929     C     VIDDSA        ANDNE     *ZERO
032400090929     C                   SETON                                        46  90
032500090929     C                   ENDIF
032600090929      *
032700090929      * PER S=STAMPA NON SI POSSONO SCEGLIERE SINGOLE FILIALI
032800090929     C     STARIS        IFEQ      'S'                                          'S'=ELAB.+STAMPA
032900090929     C     VIDLNA        ANDNE     *ZERO
033000090929     C                   SETON                                        48  90
033100090929     C                   ENDIF
033200000000      *
033300940517      * I test sulle pulizie degli archivi devono essere fatte solo se:
033400940517      *   .  richiesta la 'S'=Stampa
033500940517      *   .  oppure data DAL e' precedente alla 1° data della data area
033600950404     C     STARIS        IFEQ      'S'                                          'S'=ELAB.+STAMPA
033700021016     C**!!!COMDSD        ORLT      E6DAPE                                       DAL < DTAARA
033800021016     c     COMDSD        ORLT      §SdfArrP                                     DAL < DTAARA
033900000000      *
034000941228      *?   Test pulizia archivio bolle arrivo (FNARB00F, §5AARB)   ?
034100000000     C                   MOVE      *BLANKS       PGM
034200941228     C                   MOVEL     'FNLR84R'     PGM                            ARB-LR84
034300000000     C                   Z-ADD     §5AARB        $DATPU                         ARB
034400000000     C                   EXSR      TESTF
034500000000     C   45              GOTO      ENDCTR
034600000000      *
034700941228      *?   Test pulizia archivio bolle transito (FNBTP00F, §5ABTP)   ?
034800000000      * 1) SOLO SE CI SONO DEI RECORD
034900000000      * 2) SOLO SE LNA = 0  O = SIMFEL
035000000000     C     VIDLNA        IFEQ      0
035100000000     C     VIDLNA        OREQ      SIMFEL
035200000000      *
035300020916     C     SIMFEL        SETLL     FNBTP11L
035400020916     C     SIMFEL        READE     FNBTP11L                               30
035500000000     C                   MOVE      *BLANKS       PGM
035600000000     C     *IN30         IFEQ      *OFF
035700941228     C                   MOVEL     'FNLR85R'     PGM                            BTP-LR85
035800000000     C                   Z-ADD     §5ABTP        $DATPU                         BTP
035900000000     C                   EXSR      TESTF
036000000000     C   45              GOTO      ENDCTR
036100000000     C                   ENDIF
036200000000     C                   ENDIF
036300000000      *
036400941228      *?   Test pulizia archivio spunte arrivi (FNBRV00F, §5ABRV)   ?
036500000000      * per BRV sarebbe piu' giusto controllare i giorni effettivi con il campo
036600000000      * §5ASAC (circa 3 giorni effettivi), ma per semplicita' testo solo il
036700950121      * campo §5ABRV (gg. reali) che contiene circa 15 giorni
036800000000     C                   MOVE      *BLANKS       PGM
036900941228     C                   MOVEL     'FNLR10R'     PGM                            BRV-LR10
037000000000     C                   Z-ADD     §5ABRV        $DATPU                         BRV
037100000000     C                   EXSR      TESTF
037200000000     C   45              GOTO      ENDCTR
037300000000      *
037400941228      *?   Test pulizia archivio fogli partenze altre filiali (FVA)   ?
037500000000     C                   MOVE      *BLANKS       PGM
037600950121     C                   MOVEL     'FNLRA7R'     PGM                            FVA-LRA7
037700000000     C                   Z-ADD     §5AFVA        $DATPU                         FVA
037800000000     C                   EXSR      TESTF
037900000000     C   45              GOTO      ENDCTR
038000000000      *
038100941228      *?   Test pulizia archivio fogli vari (FNFVV00F, §5FVV)   ?
038200000000     C                   MOVE      *BLANKS       PGM
038300941228     C                   MOVEL     'FNLRA7R'     PGM                            FVV-LRA7
038400941228     C                   Z-ADD     §5AFVV        $DATPU                         FVV
038500000000     C                   EXSR      TESTF
038600000000     C   45              GOTO      ENDCTR
038700941228      *
038800941228      *?   Test pulizia archivio fogli viaggio partenze (FNFGV00F, §5FGV)   ?
038900941228     C                   MOVE      *BLANKS       PGM
039000941228     C                   MOVEL     'FNLRA7R'     PGM                            FGV-LRA7
039100941228     C                   Z-ADD     §5AFGV        $DATPU                         FGV
039200941228     C                   EXSR      TESTF
039300941228     C   45              GOTO      ENDCTR
039400940517     C                   ENDIF
039500000000      *
039600970718      *?   Test pulizia archivio statistiche arrivi (FNSAR00F, §5ASTA)   ?
039700000000     C     STARIS        IFEQ      'R'                                          'R'=RISTAMPA
039800000000     C                   MOVE      *BLANKS       PGM
039900941228     C                   MOVEL     'FNLRE4R'     PGM                            STA-LRE4
040000040923     C***                Z-ADD     §5ASTA        $DATPU            6 0          STA
040100040923     C                   Z-ADD     §5ASTS        $DATPU            6 0          STA
040200000000     C                   EXSR      TESTF
040300000000     C   45              GOTO      ENDCTR
040400000000     C                   ENDIF
040500000000      *
040600000000      *
040700000000     C     ENDCTR        ENDSR
040800000000      **************************************************************************
040900000000      * Viene rilevato l'ultimo giorno di utilizzo del pgm che si
041000000000      * occupa della pulizia archivio (ogni file, o quasi, ha il proprio pgm);
041100970718      * nella tabella 5A  c'e' il nr.giorni di mantenimento dei dati; si
041200000000      * sottrae quest'ultimo dal giorno ultimo utilizzo per sapere se vi sono
041300000000      * ancora disponibili i dati per la statistica.
041400000000      **************************************************************************
041500000000     C     TESTF         BEGSR
041600000000      *
041700000000     C                   CLEAR                   PRIDAT
041800000000      *
041900000000      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA STATISTICHE ARRIVI
042000000000     C                   MOVE      *BLANKS       DAT
042100000000     C                   MOVE      *BLANKS       ERR
042200941230     C                   CALL      'TRUL49C'                            99
042300000000     C                   PARM                    PGM              10
042400000103     C                   PARM                    DAT               8
042500000000     C                   PARM                    ERR               1
042600000000      *
042700941228      * PROGRAMMA IN USO DATA ULTIMO ULIZZO = WDATE
042800000000     C     ERR           IFEQ      '1'
042900040923     c     dat           oreq      '00000000'
043000941228     C                   Z-ADD     WDATE1        G02INV
043100000000     C                   ELSE
043200000103     C                   MOVE      DAT           G02INV                         A SINISTRA
043300000000     C                   ENDIF
043400000000      *
043500000000      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
043600000000      * PULIZIA E TROVO LA PRIMA DATA AL DI SOTTO DELLA QUALE NON SI
043700000000      * POSSONO LANCIARE ELABORAZIONI PER MANCANZA DI DATI
043800000000      *
043900000000      * SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
044000000000     C     G02INV        IFNE      *ZEROS
044100000000     C                   Z-ADD     *ZEROS        G02DAT
044200000000     C                   Z-ADD     *ZEROS        G02TGI
044300000000     C                   MOVEL     '3'           G02ERR
044400941228     C                   CALL      'XSRDA8'
044500000000     C                   PARM                    WLBDAT
044600000000      *
044700000000     C                   SUB       $DATPU        G02TGI                         GG PULIZIA
044800000000     C                   Z-ADD     *ZEROS        GIODAT
044900000000     C                   Z-ADD     *ZEROS        GIOINV
045000000000     C                   Z-ADD     G02TGI        GIOTGI
045100941228     C                   CALL      'XSRGI8'
045200000000     C                   PARM                    WGIDAT
045300000000      *
045400941228     C                   Z-ADD     GIOINV        PRIDAT            8 0
045500000000     C                   ENDIF
045600000000      *
045700000000      * DATA DAL MAGGIORE DATA CALCOLATA
045800000000     C     COMDSD        IFLE      PRIDAT
045900000000     C                   SETON                                        4590
046000000000     C                   ENDIF
046100090929
046200090929     c* se sono edp e sono il ambiente di prova non do errore
046300090929     c                   movel     knmus         winizio           3
046400090929     c                   if        winizio='EDP' and knsif='FILTRAPFP'
046500090929     C                   SETOFF                                       4590
046600090929     C                   ENDIF
046700000000      *
046800000000     C                   ENDSR
046900000000      **************************************************************************
047000000000      * ROUTINE DI CARICAMENTO TABELLA £1 E £6
047100000000      **************************************************************************
047200000000     C     CARTAB        BEGSR
047300000000      *
047400000000      * VEDO SE SONO SIMFEL O REMOTO
047500020424     C***        REM       IFEQ 'REM'
047600040209     C***        REMFIL    ANDGT*ZEROS
047700020424     C***                  MOVELREMFIL    VIDLNA
047800020424     C***                  MOVELREMFIL    VIDFIL  30
047900020424     C***                  ELSE
048000020424     C***                  MOVELSIMFEL    VIDLNA
048100020424     C***                  MOVELSIMFEL    VIDFIL
048200020424     C***                  END
048300040209     C***                Z-ADD     SIMPOU        VIDLNA
048400020424     C                   Z-ADD     SIMPOU        VIDFIL            3 0
048500000000      *
048600000000      * CARICO TABELLA FILIALI GESTITE £1
048700040219     C                   CLEAR                   DSUL06
048800040219     C                   MOVE      '£1'          D06COD
048900040219     C                   MOVEL     SIMFEL        D06KEY
049000040219     C                   MOVEL     DSUL06        KPJBU
049100040219     C                   CALL      'TRUL06R'
049200040219     C                   PARM                    KPJBA
049300040219     C                   MOVEL     KPJBU         DSUL06
049400040219     C                   MOVEA     T06           L1
049500000000      *
049600000000      * CARICO TABELLA FILIALI GESTITE £6
049700950213     C                   CLEAR                   DSUL06
049800950213     C                   MOVE      '£6'          D06COD
049900950213     C                   MOVEL     VIDFIL        D06KEY
050000950213     C                   MOVEL     DSUL06        KPJBU
050100950213     C                   CALL      'TRUL06R'
050200950213     C                   PARM                    KPJBA
050300950213     C                   MOVEL     KPJBU         DSUL06
050400950314     C                   MOVEA     T06           L6
050500000000      *
050600000000      * Prelevo le due tabelle contenenti i giorni di pulizia degli archivi
050700000000     C                   Z-ADD     1             TBLKUT
050800000000     C                   MOVE      '5A'          TBLCOD
050900000000     C                   MOVEL     *BLANKS       TBLKEY
051000000000     C                   MOVEL     '1'           TBLKEY
051100000000     C     KTAB          CHAIN     TABEL00F                           30
051200000000     C   30              CLEAR                   DS5A
051300000000     C  N30              MOVEL     TBLUNI        DS5A
051400970718     C                   MOVEL     '5A'          TBLCOD
051500040923     C****               MOVEL(P)  '2'           TBLKEY
051600040923     C                   MOVEL(P)  'SEDE1'       TBLKEY
051700000000     C     KTAB          CHAIN     TABEL00F                           30
051800040923     C** 30              CLEAR                   DS5A2
051900040923     C**N30              MOVEL     TBLUNI        DS5A2
052000040923     C   30              CLEAR                   DS5AS1
052100040923     C  N30              MOVEL     TBLUNI        DS5AS1
052200021016      * Cerco la data ultima elaborazione dalla tabella
052300021016     c                   Clear                   Tibs02Ds
052400021016     c                   Clear                   dSdf
052500021016     c                   Eval      T02Mod = 'C'
052600021016     c                   Eval      T02Sif = Knsif
052700021016     c                   Eval      T02Cod = 'SDF'
052800021016     c                   Movel(p)  Simfel        T02Ke1
052900021016     c                   Call      'TIBS02R'
053000021016     c                   Parm                    Kpjba
053100021016     c                   Parm                    Tibs02Ds
053200021016     c                   If        T02Err = *Blanks
053300021016     c                   Movel     T02Uni        dSdf
053400021016     c                   EndIf
053500021016
053600021016     C**!!!              Z-ADD     E6DAUE        G02INV                         AAAA/MM/GG
053700021016     c                   Z-Add     §SdfArrU      G02Inv
053800941230     C                   Z-ADD     *ZERO         G02DAT
053900941230     C                   MOVEL     '3'           G02ERR
054000941230     C                   CALL      'XSRDA8'
054100941230     C                   PARM                    WLBDAT
054200941230     C                   Z-ADD     G02DAT        DAULEL                         GG/MM/AAAA
054300000000      *
054400000000     C                   ENDSR
054500000000      **************************************************************************
054600000000      * ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST
054700000000      **************************************************************************
054800000000     C     DEFVAR        BEGSR
054900000000      *
055000000000     C     *ENTRY        PLIST
055100000000     C                   PARM                    KPJBA
055200000000      *
055300000000     C                   Z-ADD     1             CODUT
055400000000     C                   CALL      'X§PARUT'
055500000000     C                   PARM                    UT§DSE
055600000000     C                   MOVEL     RAGUT         RSUT             20
055700000000     C                   MOVEL     REC80         CNCR80
055800000000      *
055900000000     C     KTAB          KLIST                                                  TABEL00F
056000000000     C                   KFLD                    TBLKUT
056100000000     C                   KFLD                    TBLCOD
056200000000     C                   KFLD                    TBLKEY
056300040209     C***  KCAE5         KLIST                                                  TABEL00F
056400040209     C***                KFLD                    KEPA
056500040209     C***                KFLD                    VIDLNA
056600040209     C***                KFLD                    VIDFIL
056700040209     C***                MOVEL     'A'           KEPA
056800980304     C*
056900040209     C***  *LIKE         DEFINE    CAEEPA        KEPA
057000941228      *
057100941228     C                   TIME                    WTIME            14 0          ORA E DATA
057200941228     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
057300941228      *
057400941228     C                   Z-ADD     WDATE         G02DAT                         GG/MM/AAAA
057500941228     C                   Z-ADD     *ZERO         G02INV
057600941228     C                   MOVEL     *BLANK        G02ERR
057700941228     C                   CALL      'XSRDA8'
057800941228     C                   PARM                    WLBDAT
057900941228     C                   Z-ADD     G02INV        WDATE1            8 0          AAAA/MM/GG
058000000000      *
058100000000     C                   ENDSR
