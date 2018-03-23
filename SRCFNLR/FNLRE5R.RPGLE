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
003800151012     Ffnfvv02l  IF   E           K DISK
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
005500151012     d Wdata8          DS
005600151012     d  dadata                 1      8  0
005700151012     d  adata                  9     16  0
005800151012     d  GioLav                17     21  0
005900151012
006000000000     D KPJBA         E DS
006100000000     D DS5A          E DS
006200040923     D DS5AS1        E DS
006300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006400950213     D* DS PER TRUL06R - CARICAMENTO £X
006500950213     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006600950314     D  T06                    1     90  0
006700950314     D                                     DIM(30)                              TRUL06R
006800000000     D CNCR80        E DS
006900021016
007000021016     d Tibs02Ds      e ds
007100151012     d xgiolavds     e ds
007200021016      * Ds date statistiche
007300021016     d dSdf          e ds
007400040209     d fnlv54ds      e ds
007500040209      * Parametri per richiamo a TRUL09R
007600040209     d Trul09ds      e ds
007700040209     d  T09                   19    108  0 dim(30)
007800021016
007900000000     D                 DS
008000000000     D  TUTTE                  1     20    INZ('0 = TUTTE          ')
008100090929
008200090929     d wforza          s              1
008300151012     d knpg            s                   like(fvvnpg) inz(2)
008400151012     d kfgs            s                   like(fvvfgs)
008500000000      *---------------------------------------------------------------
008600000000      * INDICATORI USATI
008700000000      *---------------------------------------------------------------
008800940426      * 43 - FIL.ELABOR. NON CONGRUENTE CON CD.FILIALE
008900000000      * 44 - ELABORAZIONE MOMENTANEAMENTE NON EFFETTUABILE
009000000000      * 45 - ELABORAZIONE NON EFFETTUABILE PER MANCANZA DI DATI
009100000000      * 46 - PER S=STAMPA SI PUO' SCEGLIERE SOLO UN GIORNO
009200000000      * 48 - PER S=STAMPA NON SI PUO' SCEGLIERE UNA SINGOLA FILIALE
009300000000      * 90 - GENERICO DI ERRORE
009400000000      *---------------------------------------------------------------
009500000000      *
009600000000      * DEFINIZIONE VARIABILI, KLIST E PLIST
009700000000     C                   EXSR      DEFVAR
009800000000      *
009900040209      * CARICAMENTO TABELLE
010000000000     C                   EXSR      CARTAB
010100000000      *
010200000000     C     INIZIO        TAG
010300000000     C                   Z-ADD     0             VIDDSD
010400000000     C                   Z-ADD     0             VIDDSA
010500000000     C                   Z-ADD     0             COMDSA
010600040209     c* tipo stampa e p.o. li propongo in base a chi lancia:
010700040209     c* se chi lancia è terminal di arrivo prpongo 0 e "T"
010800040209     c* altrimenti p.o.=chi lancia e tipo stampa ="L"
010900040209     c                   clear                   fnlv54ds
011000040209     c                   movel     'L'           i54tla
011100040209     c                   movel     'A'           i54tpt
011200040209     c                   z-add     vidfil        i54lin
011300040209     c                   z-add     wdate1        i54drf
011400040209     c                   call      'FNLV54R'
011500040209     c                   parm                    fnlv54ds
011600040209     c     o54err        ifeq      *blanks
011700040209     c     o54tfa        andeq     vidfil
011800040209     c                   clear                   vidlna
011900040209     c                   move      'T'           vidsta
012000040209     c                   else
012100040209     c                   z-add     vidfil        vidlna
012200040209     c                   move      'L'           vidsta
012300040209     c                   endif
012400040209     C*****              MOVE      'T'           VIDSTA                         TIPO STAMPA
012500000000     C                   MOVE      'R'           STARIS                         STAMPA/RISTAMPA
012600000000     C                   MOVE      TUTTE         DELNA
012700000000      *
012800000000     C     FOR01         TAG
012900941228     C                   EXFMT     LRE5D01
013000000000      *
013100000000      * CMD3 - FINE LAVORO
013200000000     C   KC              GOTO      FINE
013300000000      *
013400000000      * CONTROLLI
013500000000     C                   EXSR      CONTR
013600090929     c*   per 45 e per EDP do msg forzabile
013700090929     c                   if        *in45 and
013800090929     c                                   %subst(knmus:1:3)='EDP'
013900090929     c                   if        wforza=' '
014000090929     c                   eval      wforza='S'
014100090929     c                   else
014200090929     c                   setoff                                       4590
014300090929     c                   endif
014400090929     c                   endif
014500090929     C  NKF
014600000000     COR 90              GOTO      FOR01
014700000000      *
014800000000      * Se la data finale e' vuota, la riempo con quella iniziale
014900000000     C     COMDSA        IFEQ      *ZERO
015000000000     C                   Z-ADD     COMDSD        COMDSA
015100000000     C                   Z-ADD     VIDDSD        VIDDSA
015200000000     C                   ENDIF
015300950213     C*
015400950213     C                   CLEAR                   DSLS01
015500950214     C                   MOVEL     'A'           DS1TBO                         'A'=ARRIVI
015600950213     C                   Z-ADD     VIDDSD        DS1GDA                         GG/MM/AAAA DAL
015700950213     C                   Z-ADD     VIDDSA        DS1GAL                         GG/MM/AAAA AL
015800950213     C                   Z-ADD     COMDSD        DS1ADA                         AAAA/MM(GG DAL
015900950213     C                   Z-ADD     COMDSA        DS1AAL                         AAAA/MM/GG AL
016000950213     C                   Z-ADD     VIDFIL        DS1FGS
016100950313     C* METTO LA LNA IN DS1LNP PER UNIFORMARMI ALLE STAMPE GIORNALIERE
016200950213     C                   Z-ADD     VIDLNA        DS1LNP                         E' LA LNA
016300950213     C                   MOVEL     STARIS        DS1RIS
016400021031     c                   Z-Add     Simfel        Ds1Fel
016500950216     C                   MOVEL     VIDSTA        DS1FL1                         TIPO STAMPA
016600970207     C                   MOVEL     'FNLRE7C'     DS1PGM
016700970207     C* TIPO STATISTICA A=ARRIVI
016800970207     C                   MOVEL     'A'           DS1FPG
016900000000      *
017000970207     C                   MOVE      'LSC8'        KCOAZ
017100950213     C                   CLEAR                   KPJBU
017200950213     C                   MOVEL     DSLS01        KPJBU
017300000000     C                   CALL      'BCH10'
017400000000     C                   PARM                    KPJBA
017500970604     C*
017600970604     C* SOTTOMETTO TRASMISSIONE STATISTICA SE RIELABORAZIONE DATI
017700040923     C**** STARIS        IFEQ      'S'
017800040923     C****               CLEAR                   KPJBU
017900040923     C****               MOVEL     'LSC6'        KCOAZ
018000040923     C****               CALL      'BCH10'
018100040923     C****               PARM                    KPJBA
018200040923     C****               ENDIF
018300000000      *
018400000000     C     FINE          TAG
018500040209     c                   clear                   trul09ds
018600040209     c                   movel     'C'           i09tla
018700040209     c                   call      'TRUL09R'
018800040209     c                   parm                    trul09ds
018900971126     C*
019000000000     C                   SETON                                        LR
019100000000      **************************************************************************
019200000000      * ROUTINE DI CONTROLLO DATI FORMATO VIDEO
019300000000      **************************************************************************
019400000000     C     CONTR         BEGSR
019500000000      *
019600971126      * DATA ARRIVO DAL OBBLIGATORIA
019700971126     C                   MOVE      VIDDSD        G02DAT
019800971126     C                   MOVEL     *BLANK        G02ERR
019900971126     C                   CALL      'XSRDA8'
020000971126     C                   PARM                    WLBDAT
020100990413    1C     G02ERR        IFEQ      '1'
020200971126     C                   SETON                                        40  90
020300971126     C                   GOTO      ENDCTR
020400990413    1C                   ENDIF
020500971126     C                   Z-ADD     G02DAT        VIDDSD
020600971126     C                   Z-ADD     G02INV        COMDSD            8 0
020700971126     C*
020800000000      * SE C'E', DECODIFICO LA FILIALE
020900000000     C                   MOVE      TUTTE         DELNA
021000990413    1C     VIDLNA        IFNE      *ZERO
021100000000     C     VIDLNA        CHAIN     AZORG01L                           90
021200990413    2C  N90              DO
021300940426     C                   MOVE      *BLANKS       DELNA
021400940426     C                   MOVEL     ORGDES        DELNA
021500940426      *
021600990413    2C                   ENDDO
021700990413    1C                   ENDIF
021800000000      *
021900000000     C                   SETOFF                                       9002
022000040209     c*
022100040209     c* richiamo trul09r per sapere se chi lancia è ter-arr e di chi
022200040209     c                   clear                   trul09ds
022300100701     c                   eval      i09mod = 'DEF'
022400040209     c                   eval      i09tfa = vidfil
022500040209     c                   eval      i09dta = comdsd
022600040209     c                   call      'TRUL09R'
022700040209     c                   parm                    trul09ds
022800040209    2c     o09err        ifeq      *blanks
022900040209     c     t09(1)        andgt     *zeros
023000040209     c     vidlna        ifgt      *zeros
023100040209     c     vidlna        lookup    t09                                    31
023200040219     c* faccio anche lookup in £1 in quanto i secondi livello in arrivo su
023300040219     c* altro as devono poter essere lanciati anche dal loro terminal di
023400040219     c* partenza finchè il loro terminal di arrivo si trova su altro ogas
023500040219     c* visto che nel menù dei secondi liv. non c'è il richiamo al lancio
023600040219     c* statistica arrivi
023700040219     c     vidlna        lookup    l1                                     32
023800040219     c     *in31         ifeq      *off
023900040219     c     *in32         andeq     *off
024000040219     c                   seton                                        5190
024100040219     c                   goto      endctr
024200040219     c                   endif
024300040209     c                   endif
024400040209     c                   else
024500040209     c* p.o. non ter-arr può lanciare solo se stesso e solo per linea
024600040209     c     vidlna        ifne      vidfil
024700040209     c                   setoff                                       31
024800040209     c     vidlna        ifgt      0
024900040209     c     vidlna        lookup    l6                                     31
025000040209     c                   else
025100040209     c                   setoff                                       31
025200040209     c                   endif
025300040209     c  n31              seton                                        5190
025400040209     c  n31              goto      endctr
025500040209     c                   endif
025600040209     c     vidsta        ifne      'L'
025700040209     c                   seton                                        5290
025800040209     c                   goto      endctr
025900040209     c                   endif
026000040209     c                   endif
026100000000      *
026200000000      * CONTROLLO DATA ARRIVO AL
026300950213     C                   Z-ADD     *ZERO         COMDSA            8 0          DATA AL(AAMMGG)
026400990413    1C     VIDDSA        IFNE      0
026500000000     C                   MOVE      VIDDSA        G02DAT
026600000000     C                   MOVEL     *BLANK        G02ERR
026700941228     C                   CALL      'XSRDA8'
026800000000     C                   PARM                    WLBDAT
026900990413    2C     G02ERR        IFEQ      '1'
027000000000     C                   SETON                                        41  90
027100000000     C                   GOTO      ENDCTR
027200990413    2C                   ENDIF
027300980119     C*
027400941230     C                   Z-ADD     G02DAT        VIDDSA
027500941228     C                   Z-ADD     G02INV        COMDSA
027600000000      *
027700000000      * DATA DAL NO MAGGIORE DATA AL
027800990413    2C     COMDSD        IFGT      COMDSA
027900000000     C                   SETON                                        4290
028000000000     C                   GOTO      ENDCTR
028100990413    2C                   ENDIF
028200990413    1C                   ENDIF
028300150921     c
028400151012      * La data finale non puo' essere superiore al primo giorno lavorativo
028500151012    1c                   if        comdsd>0 and comdsa>0
028600150921    2C     comdsa        IFGT      WDATE1
028700150921     C     COMDSD        ORGT      WDATE1
028800150921     C                   SETON                                        49  90
028900150921     C                   GOTO      ENDCTR
029000150921    2C                   ENDIF
029100151012    1C                   ENDIF
029200151012     c
029300151012    1c                   if        comdsd>0 and comdsa=0
029400151012    3c                   if        comdsd>wdate1
029500151012     c* non do errore se esiste un foglio arrivi già aperto in quella data
029600151012    4c                   if        vidlna>0
029700151012     c                   eval      kfgs=vidlna
029800151012     c                   else
029900151012     c                   eval      kfgs=vidfil
030000151012    4c                   endif
030100151012     c     kfvv          chain     fnfvv02l
030200151012    4c                   if        not %found(fnfvv02l)
030300151012     C                   SETON                                        49  90
030400151012     C                   GOTO      ENDCTR
030500151012    4c                   endif
030600151012    3c                   endif
030700151012    1c                   endif
030800090929
030900090929      * PER S=STAMPA SI PUO' SCEGLIERE SOLO UN GIORNO
031000090929     C     STARIS        IFEQ      'S'                                          'S'=ELAB.+STAMPA
031100090929     C     VIDDSA        ANDNE     VIDDSD
031200090929     C     VIDDSA        ANDNE     *ZERO
031300090929     C                   SETON                                        46  90
031400090929     C                   ENDIF
031500090929      *
031600090929      * PER S=STAMPA NON SI POSSONO SCEGLIERE SINGOLE FILIALI
031700090929     C     STARIS        IFEQ      'S'                                          'S'=ELAB.+STAMPA
031800090929     C     VIDLNA        ANDNE     *ZERO
031900090929     C                   SETON                                        48  90
032000090929     C                   ENDIF
032100000000      *
032200940517      * I test sulle pulizie degli archivi devono essere fatte solo se:
032300940517      *   .  richiesta la 'S'=Stampa
032400940517      *   .  oppure data DAL e' precedente alla 1° data della data area
032500950404     C     STARIS        IFEQ      'S'                                          'S'=ELAB.+STAMPA
032600021016     C**!!!COMDSD        ORLT      E6DAPE                                       DAL < DTAARA
032700021016     c     COMDSD        ORLT      §SdfArrP                                     DAL < DTAARA
032800000000      *
032900941228      *?   Test pulizia archivio bolle arrivo (FNARB00F, §5AARB)   ?
033000000000     C                   MOVE      *BLANKS       PGM
033100941228     C                   MOVEL     'FNLR84R'     PGM                            ARB-LR84
033200000000     C                   Z-ADD     §5AARB        $DATPU                         ARB
033300000000     C                   EXSR      TESTF
033400000000     C   45              GOTO      ENDCTR
033500000000      *
033600941228      *?   Test pulizia archivio bolle transito (FNBTP00F, §5ABTP)   ?
033700000000      * 1) SOLO SE CI SONO DEI RECORD
033800000000      * 2) SOLO SE LNA = 0  O = SIMFEL
033900000000     C     VIDLNA        IFEQ      0
034000000000     C     VIDLNA        OREQ      SIMFEL
034100000000      *
034200020916     C     SIMFEL        SETLL     FNBTP11L
034300020916     C     SIMFEL        READE     FNBTP11L                               30
034400000000     C                   MOVE      *BLANKS       PGM
034500000000     C     *IN30         IFEQ      *OFF
034600941228     C                   MOVEL     'FNLR85R'     PGM                            BTP-LR85
034700000000     C                   Z-ADD     §5ABTP        $DATPU                         BTP
034800000000     C                   EXSR      TESTF
034900000000     C   45              GOTO      ENDCTR
035000000000     C                   ENDIF
035100000000     C                   ENDIF
035200000000      *
035300941228      *?   Test pulizia archivio spunte arrivi (FNBRV00F, §5ABRV)   ?
035400000000      * per BRV sarebbe piu' giusto controllare i giorni effettivi con il campo
035500000000      * §5ASAC (circa 3 giorni effettivi), ma per semplicita' testo solo il
035600950121      * campo §5ABRV (gg. reali) che contiene circa 15 giorni
035700000000     C                   MOVE      *BLANKS       PGM
035800941228     C                   MOVEL     'FNLR10R'     PGM                            BRV-LR10
035900000000     C                   Z-ADD     §5ABRV        $DATPU                         BRV
036000000000     C                   EXSR      TESTF
036100000000     C   45              GOTO      ENDCTR
036200000000      *
036300941228      *?   Test pulizia archivio fogli partenze altre filiali (FVA)   ?
036400000000     C                   MOVE      *BLANKS       PGM
036500950121     C                   MOVEL     'FNLRA7R'     PGM                            FVA-LRA7
036600000000     C                   Z-ADD     §5AFVA        $DATPU                         FVA
036700000000     C                   EXSR      TESTF
036800000000     C   45              GOTO      ENDCTR
036900000000      *
037000941228      *?   Test pulizia archivio fogli vari (FNFVV00F, §5FVV)   ?
037100000000     C                   MOVE      *BLANKS       PGM
037200941228     C                   MOVEL     'FNLRA7R'     PGM                            FVV-LRA7
037300941228     C                   Z-ADD     §5AFVV        $DATPU                         FVV
037400000000     C                   EXSR      TESTF
037500000000     C   45              GOTO      ENDCTR
037600941228      *
037700941228      *?   Test pulizia archivio fogli viaggio partenze (FNFGV00F, §5FGV)   ?
037800941228     C                   MOVE      *BLANKS       PGM
037900941228     C                   MOVEL     'FNLRA7R'     PGM                            FGV-LRA7
038000941228     C                   Z-ADD     §5AFGV        $DATPU                         FGV
038100941228     C                   EXSR      TESTF
038200941228     C   45              GOTO      ENDCTR
038300940517     C                   ENDIF
038400000000      *
038500970718      *?   Test pulizia archivio statistiche arrivi (FNSAR00F, §5ASTA)   ?
038600000000     C     STARIS        IFEQ      'R'                                          'R'=RISTAMPA
038700000000     C                   MOVE      *BLANKS       PGM
038800941228     C                   MOVEL     'FNLRE4R'     PGM                            STA-LRE4
038900040923     C***                Z-ADD     §5ASTA        $DATPU            6 0          STA
039000040923     C                   Z-ADD     §5ASTS        $DATPU            6 0          STA
039100000000     C                   EXSR      TESTF
039200000000     C   45              GOTO      ENDCTR
039300000000     C                   ENDIF
039400000000      *
039500000000      *
039600000000     C     ENDCTR        ENDSR
039700000000      **************************************************************************
039800000000      * Viene rilevato l'ultimo giorno di utilizzo del pgm che si
039900000000      * occupa della pulizia archivio (ogni file, o quasi, ha il proprio pgm);
040000970718      * nella tabella 5A  c'e' il nr.giorni di mantenimento dei dati; si
040100000000      * sottrae quest'ultimo dal giorno ultimo utilizzo per sapere se vi sono
040200000000      * ancora disponibili i dati per la statistica.
040300000000      **************************************************************************
040400000000     C     TESTF         BEGSR
040500000000      *
040600000000     C                   CLEAR                   PRIDAT
040700000000      *
040800000000      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA STATISTICHE ARRIVI
040900000000     C                   MOVE      *BLANKS       DAT
041000000000     C                   MOVE      *BLANKS       ERR
041100941230     C                   CALL      'TRUL49C'                            99
041200000000     C                   PARM                    PGM              10
041300000103     C                   PARM                    DAT               8
041400000000     C                   PARM                    ERR               1
041500000000      *
041600151012      * PROGRAMMA IN USO DATA ULTIMO UtiLIZZO = WDATE
041700000000     C     ERR           IFEQ      '1'
041800040923     c     dat           oreq      '00000000'
041900941228     C                   Z-ADD     WDATE1        G02INV
042000000000     C                   ELSE
042100000103     C                   MOVE      DAT           G02INV                         A SINISTRA
042200000000     C                   ENDIF
042300000000      *
042400000000      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
042500000000      * PULIZIA E TROVO LA PRIMA DATA AL DI SOTTO DELLA QUALE NON SI
042600000000      * POSSONO LANCIARE ELABORAZIONI PER MANCANZA DI DATI
042700000000      *
042800000000      * SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
042900000000     C     G02INV        IFNE      *ZEROS
043000000000     C                   Z-ADD     *ZEROS        G02DAT
043100000000     C                   Z-ADD     *ZEROS        G02TGI
043200000000     C                   MOVEL     '3'           G02ERR
043300941228     C                   CALL      'XSRDA8'
043400000000     C                   PARM                    WLBDAT
043500000000      *
043600000000     C                   SUB       $DATPU        G02TGI                         GG PULIZIA
043700000000     C                   Z-ADD     *ZEROS        GIODAT
043800000000     C                   Z-ADD     *ZEROS        GIOINV
043900000000     C                   Z-ADD     G02TGI        GIOTGI
044000941228     C                   CALL      'XSRGI8'
044100000000     C                   PARM                    WGIDAT
044200000000      *
044300941228     C                   Z-ADD     GIOINV        PRIDAT            8 0
044400000000     C                   ENDIF
044500000000      *
044600000000      * DATA DAL MAGGIORE DATA CALCOLATA
044700000000     C     COMDSD        IFLE      PRIDAT
044800000000     C                   SETON                                        4590
044900000000     C                   ENDIF
045000090929
045100090929     c* se sono edp e sono il ambiente di prova non do errore
045200090929     c                   movel     knmus         winizio           3
045300090929     c                   if        winizio='EDP' and knsif='FILTRAPFP'
045400090929     C                   SETOFF                                       4590
045500090929     C                   ENDIF
045600000000      *
045700000000     C                   ENDSR
045800000000      **************************************************************************
045900000000      * ROUTINE DI CARICAMENTO TABELLA £1 E £6
046000000000      **************************************************************************
046100000000     C     CARTAB        BEGSR
046200000000      *
046300000000      * VEDO SE SONO SIMFEL O REMOTO
046400020424     C***        REM       IFEQ 'REM'
046500040209     C***        REMFIL    ANDGT*ZEROS
046600020424     C***                  MOVELREMFIL    VIDLNA
046700020424     C***                  MOVELREMFIL    VIDFIL  30
046800020424     C***                  ELSE
046900020424     C***                  MOVELSIMFEL    VIDLNA
047000020424     C***                  MOVELSIMFEL    VIDFIL
047100020424     C***                  END
047200040209     C***                Z-ADD     SIMPOU        VIDLNA
047300020424     C                   Z-ADD     SIMPOU        VIDFIL            3 0
047400000000      *
047500000000      * CARICO TABELLA FILIALI GESTITE £1
047600040219     C                   CLEAR                   DSUL06
047700040219     C                   MOVE      '£1'          D06COD
047800040219     C                   MOVEL     SIMFEL        D06KEY
047900040219     C                   MOVEL     DSUL06        KPJBU
048000040219     C                   CALL      'TRUL06R'
048100040219     C                   PARM                    KPJBA
048200040219     C                   MOVEL     KPJBU         DSUL06
048300040219     C                   MOVEA     T06           L1
048400000000      *
048500000000      * CARICO TABELLA FILIALI GESTITE £6
048600950213     C                   CLEAR                   DSUL06
048700950213     C                   MOVE      '£6'          D06COD
048800950213     C                   MOVEL     VIDFIL        D06KEY
048900950213     C                   MOVEL     DSUL06        KPJBU
049000950213     C                   CALL      'TRUL06R'
049100950213     C                   PARM                    KPJBA
049200950213     C                   MOVEL     KPJBU         DSUL06
049300950314     C                   MOVEA     T06           L6
049400000000      *
049500000000      * Prelevo le due tabelle contenenti i giorni di pulizia degli archivi
049600000000     C                   Z-ADD     1             TBLKUT
049700000000     C                   MOVE      '5A'          TBLCOD
049800000000     C                   MOVEL     *BLANKS       TBLKEY
049900000000     C                   MOVEL     '1'           TBLKEY
050000000000     C     KTAB          CHAIN     TABEL00F                           30
050100000000     C   30              CLEAR                   DS5A
050200000000     C  N30              MOVEL     TBLUNI        DS5A
050300970718     C                   MOVEL     '5A'          TBLCOD
050400040923     C****               MOVEL(P)  '2'           TBLKEY
050500040923     C                   MOVEL(P)  'SEDE1'       TBLKEY
050600000000     C     KTAB          CHAIN     TABEL00F                           30
050700040923     C** 30              CLEAR                   DS5A2
050800040923     C**N30              MOVEL     TBLUNI        DS5A2
050900040923     C   30              CLEAR                   DS5AS1
051000040923     C  N30              MOVEL     TBLUNI        DS5AS1
051100021016      * Cerco la data ultima elaborazione dalla tabella
051200021016     c                   Clear                   Tibs02Ds
051300021016     c                   Clear                   dSdf
051400021016     c                   Eval      T02Mod = 'C'
051500021016     c                   Eval      T02Sif = Knsif
051600021016     c                   Eval      T02Cod = 'SDF'
051700021016     c                   Movel(p)  Simfel        T02Ke1
051800021016     c                   Call      'TIBS02R'
051900021016     c                   Parm                    Kpjba
052000021016     c                   Parm                    Tibs02Ds
052100021016     c                   If        T02Err = *Blanks
052200021016     c                   Movel     T02Uni        dSdf
052300021016     c                   EndIf
052400021016
052500021016     C**!!!              Z-ADD     E6DAUE        G02INV                         AAAA/MM/GG
052600021016     c                   Z-Add     §SdfArrU      G02Inv
052700941230     C                   Z-ADD     *ZERO         G02DAT
052800941230     C                   MOVEL     '3'           G02ERR
052900941230     C                   CALL      'XSRDA8'
053000941230     C                   PARM                    WLBDAT
053100941230     C                   Z-ADD     G02DAT        DAULEL                         GG/MM/AAAA
053200000000      *
053300000000     C                   ENDSR
053400000000      **************************************************************************
053500000000      * ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST
053600000000      **************************************************************************
053700000000     C     DEFVAR        BEGSR
053800000000      *
053900000000     C     *ENTRY        PLIST
054000000000     C                   PARM                    KPJBA
054100000000      *
054200000000     C                   Z-ADD     1             CODUT
054300000000     C                   CALL      'X§PARUT'
054400000000     C                   PARM                    UT§DSE
054500000000     C                   MOVEL     RAGUT         RSUT             20
054600000000     C                   MOVEL     REC80         CNCR80
054700000000      *
054800151012     c     kfvv          klist
054900151012     c                   kfld                    knpg
055000151012     c                   kfld                    comdsd
055100151012     c                   kfld                    kfgs
055200000000     C     KTAB          KLIST                                                  TABEL00F
055300000000     C                   KFLD                    TBLKUT
055400000000     C                   KFLD                    TBLCOD
055500000000     C                   KFLD                    TBLKEY
055600040209     C***  KCAE5         KLIST                                                  TABEL00F
055700040209     C***                KFLD                    KEPA
055800040209     C***                KFLD                    VIDLNA
055900040209     C***                KFLD                    VIDFIL
056000040209     C***                MOVEL     'A'           KEPA
056100980304     C*
056200040209     C***  *LIKE         DEFINE    CAEEPA        KEPA
056300941228      *
056400941228     C                   TIME                    WTIME            14 0          ORA E DATA
056500941228     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
056600941228      *
056700941228     C                   Z-ADD     WDATE         G02DAT                         GG/MM/AAAA
056800941228     C                   Z-ADD     *ZERO         G02INV
056900941228     C                   MOVEL     *BLANK        G02ERR
057000941228     C                   CALL      'XSRDA8'
057100941228     C                   PARM                    WLBDAT
057200941228     C                   Z-ADD     G02INV        WDATE1            8 0          AAAA/MM/GG
057300151012     c
057400151012      *controllo se udate è festivo non lancio l'elaborazione
057500151012     c                   move      wdate1        dadata
057600151012     c                   move      wdate1        adata
057700151012     c                   CALL      'XSRLAV8'
057800151012     c                   PARM                    Wdata8
057900151012     c                   if        giolav = 0
058000151012     c* se festivo cerco il primo giorno lavorativo successivo ad oggi
058100151012     c                   clear                   xgiolavds
058200151012     c                   eval      ixgldata=wdate1
058300151012     c                   eval      ixglfil =simfel
058400151012     c                   eval      ixglpa  ='A'
058500151012     c                   eval      ixgladd ='S'
058600151012     C* AGGIUNGO 1 AI GIORNI DI PULIZIA CONTENUTI IN TABELLA
058700151012     c                   eval      ixglgg  =1
058800151012     c                   eval      ixgllav ='S'
058900151012     c                   call      'XGIOLAV'
059000151012     c                   parm                    xgiolavds
059100151012     c                   Move      oxgldata      wdate1
059200151012     c                   endif
059300151012     c
059400000000     C                   ENDSR
