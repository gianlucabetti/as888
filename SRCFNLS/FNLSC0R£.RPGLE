000100020417     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200931203      *****************************************************************
000300941010      *  Nome programma:  FNLSC0R
000400931203      *  Descrizione   :  Stampa statistiche partenze
000500931203      *                   Programma di lancio (filtro)
000600931203      *  Data creazione:  30 SET 1993
000700931203      *****************************************************************
000800950210      *  ??PASSAGGIO PARAMETRI:?
000900950216      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
001000950210      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
001100950210      *  DS1FPG   FLAG PROGRAMMA (LASCIARE VUOTO)
001200950210      *  DS1FEL   FILIALE ELABORATORE (AREA CHE SI VUOLE STAMPARE)
001300950210      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA
001400950210      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
001500950210      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
001600950210      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
001700950210      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
001800950404      *  DS1RIS   'R'=RISTAMPA, 'S'=ELABOR.+STAMPA, ' '=SOLO ELABOR.
001900950216      *  DS1FL1   STATISTICA SELEZIONATA  ( ' '=TUTTE )
002000950210      *****************************************************************
002100941010     FFNLSC0D   CF   E             WORKSTN
002200991227     FAZCLN01L  IF   E           K DISK
002300000810     FAZORG01L  IF   E           K DISK
002400050829     FAZcae03l  IF   E           K DISK
002500020802     FFNBTP11L  IF   E           K DISK
002600920930     FTABEL00F  IF   E           K DISK
002700931203      *
002800050829     D** L1              S              3  0 DIM(30)
002900020417     D**!!! FIL             S              3    DIM(29)
003000970213     D K78             S              1    DIM(78)
003100970213     D MSG             S             78    DIM(1) CTDATA PERRCD(1)
003200931203      *
003300920320     D WLBDAT          DS
003400941010     D  G02DAT                 1      8  0
003500941010     D  G02INV                 9     16  0
003600941010     D  G02ERR                17     17
003700941010     D  G02TGI                18     22  0
003800950210     D                 DS
003900941010     D  VIDDSD                 1      8  0
004000931203     D  $GI                    1      2  0
004100931203     D  BCAMES                 3      4  0
004200991227     D  BCAANN                 5      8  0
004300931203      *
004400920930     D CNCR80        E DS
004500920930     D KPJBA         E DS
004600920930     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004700971201     D DSLV55        E DS                  EXTNAME(FNLV55DS)
004800950210     D DSLS01        E DS                  INZ
004900931203      * DS per i giorni del mes
005000931203     D GMEDS           DS
005100991227     D  GME                    1     31
005200991227     D                                     DIM(31)
005300000810     D OG143         E DS
005400000810     D DS5A          E DS
005500040921     D DS5AS1        E DS
005600970213     D DS7A2         E DS
005700941010     D WGIDAT          DS
005800941010     D  GIODAT                 1      8  0
005900941010     D  GIOINV                 9     16  0
006000941010     D  GIOTGI                17     21  0
006100970213     D                 DS
006200970213     D  WAA                    1      4
006300970213     D  WMM                    5      6
006400970213     D  WGG                    7      8
006500970213     D  WDATA                  1      8
006600000608     D* DS PER FNLSC4R1 - CONTROLLO ELABORAZIONE PER POSTE - PARAMETRI:
006700000608      * IC4LIN = LINEA
006800000608      * IC4FLG = I=INTERATTIVA (1 RICHIESTA) .. NON UTILIZZATO B=BATCH
006900000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
007000020417
007100020417      * DS per TRUL06R (caricamento £1)
007200050829     d** TRUl06ds      e ds
007300050829     d**  lin                    1     90  0 dim(30)
007400021016
007500050829     d KEPA            s                   Like(caeepa) inz('P')
007600050829     d KTFP            s                   Like(caeTFP)
007700050829     d KTFE            s                   Like(caeTFE)
007800050829     d IC4DRF          s              8s 0
007900050829     d CTRdsa          s                   like(comdsa)
008000050829
008100021016     d Tibs02Ds      e ds
008200021016      * Ds date statistiche
008300021016     d dSdf          e ds
008400931203      *
008500931203      *---------------------------------------------------------------*
008600000000     C     *ENTRY        PLIST
008700000000     C                   PARM                    KPJBA
008800000000     C                   Z-ADD     1             CODUT
008900920930     C                   CALL      'X§PARUT'
009000920930     C                   PARM                    UT§DSE
009100000000     C                   MOVEL     RAGUT         RSUT             20
009200920930     C                   MOVEL     REC80         CNCR80
009300931203      *---------------------------------------------------------------*
009400931203      * ACCESSO   TABEL
009500920930     C     KTAB          KLIST
009600920930     C                   KFLD                    CODUT
009700920930     C                   KFLD                    COD               2
009800920930     C                   KFLD                    KEY               8
009900931203     C     CNBCAK        KLIST
010000991227     C                   KFLD                    W0030             3 0
010100991227     C                   KFLD                    W0030
010200991227     C                   KFLD                    BCAANN
010300931203     C                   KFLD                    BCAMES
010400050829     C     KCAE          KLIST
010500050829     C                   KFLD                    KEPA
010600050829     C                   KFLD                    KTFP
010700050829     C                   KFLD                    KTFE
010800950210     C*
010900950210     C     *LIKE         DEFINE    VIDDSA        COMDSA
011000950210     C     *LIKE         DEFINE    VIDDSD        COMDSD
011100991227     C                   CLEAR                   W0030
011200941010     C*
011300941010     C* RILEVO LA DATA DEL GIORNO E LA INVERTO
011400941010     C                   TIME                    WTIME            14 0
011500941010     C                   MOVE      WTIME         WDATE             8 0
011600941010     C                   Z-ADD     WDATE         G02DAT
011700941010     C                   Z-ADD     *ZERO         G02INV
011800941010     C                   MOVEL     *BLANKS       G02ERR
011900941010     C                   CALL      'XSRDA8'
012000941010     C                   PARM                    WLBDAT
012100941010     C                   Z-ADD     G02INV        WDATEU            8 0
012200931203      *
012300900524     C                   Z-ADD     0             VIDDSD
012400900524     C                   Z-ADD     0             VIDDSA
012500950210     C                   Z-ADD     0             COMDSA
012600930923     C                   MOVE      *BLANKS       VIDTST
012700950118     C                   MOVE      'R'           VIDSTA
012800931203      *
012900920930     C                   EXSR      CARTAB
013000931203      *
013100941010     C                   WRITE     LSC0T01
013200900509     C     FOR01         TAG
013300941010     C                   EXFMT     LSC0D01
013400931203      *
013500050829      *  F3 - FINE LAVORO
013600900509     C   KC              GOTO      FINE
013700931203      * CONTROLLO
013800900524     C                   EXSR      CONTR
013900941010     C  NKF
014000900524     COR 90              GOTO      FOR01
014100010524      *
014200010524      * SE SCELTA STAMPA SOLO TOTALI
014300010524     C     VIDTOT        IFEQ      'S'
014400010524     C     VIDTST        ANDEQ     'E'
014500010524     C                   MOVEL     'S'           DS1FL5
014600010524     C                   ENDIF
014700940426      *
014800941010      * SE NON SIAMO IN SEDE IMPOSTO LA FILIALE ELABORATORE
014900941010     C     SIMFEL        IFNE      *ZERO
015000941010     C                   Z-ADD     SIMFEL        VIDARE
015100941010     C                   ENDIF
015200940224      *
015300941010      * Se la data fine periodo e' nulla la imposto come quella iniziale
015400941010     C     COMDSA        IFEQ      *ZERO
015500941010     C                   Z-ADD     VIDDSD        VIDDSA
015600941010     C                   Z-ADD     COMDSD        COMDSA
015700941010     C                   ENDIF
015800950210      *
015900950216     C                   MOVEL     'P'           DS1TBO
016000950210     C                   Z-ADD     VIDARE        DS1FEL
016100950210     C                   Z-ADD     VIDLNP        DS1LNP
016200950210     C                   Z-ADD     VIDDSD        DS1GDA
016300950210     C                   Z-ADD     VIDDSA        DS1GAL
016400950210     C                   Z-ADD     COMDSD        DS1ADA
016500950210     C                   Z-ADD     COMDSA        DS1AAL
016600970207     C                   MOVEL     VIDTST        DS1FPG
016700950210     C                   MOVEL     VIDSTA        DS1RIS
016800931129      *
016900941010      * Sottometto il pgm che si occupera' del lancio effettivo delle statist.
017000950210     C                   CLEAR                   KPJBU
017100950210     C                   MOVEL     DSLS01        KPJBU
017200941010     C                   CALL      'FNLSC8R'
017300931129     C                   PARM                    KPJBA
017400970305     C* SOTTOMETTO TRASMISISONE STATISTICHE SE RIELABORAIZONE DATI
017500040921     C***  VIDSTA        IFEQ      'S'
017600040921     C***  VIDTST        ANDNE     'E'
017700040921     C***                CLEAR                   KPJBU
017800040921     C***                MOVEL     'LSC6'        KCOAZ
017900040921     C***                CALL      'BCH10'
018000040921     C***                PARM                    KPJBA
018100040921     C***                ENDIF
018200931203      *
018300000000     C     FINE          TAG
018400971201     C*
018500971201     C                   MOVEL     'C'           D55TLA
018600971201     C                   CALL      'FNLV55R'
018700971201     C                   PARM                    DSLV55
018800971201     C*
018900000000     C                   SETON                                        LR
019000931203      *****************************************************************
019100931203      *    CONTROLLI FORMATO1
019200931203      *****************************************************************
019300900524     C     CONTR         BEGSR
019400931203      *
019500970213     C                   SETOFF                                       902858
019600000810     C                   SETOFF                                       59
019700931203      * DATA SPEDIZIONE DAL OBBLIGATORIA
019800970213     C                   CLEAR                   WLBDAT
019900920320     C                   MOVE      VIDDSD        G02DAT
020000941010     C                   CALL      'XSRDA8'
020100920320     C                   PARM                    WLBDAT
020200920320     C     G02ERR        IFEQ      '1'
020300920320     C                   SETON                                        40  90
020400920320     C                   GOTO      ENDCTR
020500920320     C                   END
020600941010     C                   Z-ADD     G02DAT        VIDDSD
020700941010     C                   Z-ADD     G02INV        COMDSD
020800931203      *
020900940202      * DATA SPEDIZIONE "AL" NON OBBLIGATORIA
021000941010     C                   Z-ADD     *ZERO         COMDSA
021100900524     C     VIDDSA        IFNE      0
021200970213     C                   CLEAR                   WLBDAT
021300920320     C                   MOVE      VIDDSA        G02DAT
021400941010     C                   CALL      'XSRDA8'
021500920320     C                   PARM                    WLBDAT
021600920320     C     G02ERR        IFEQ      '1'
021700920320     C                   SETON                                        41  90
021800920320     C                   GOTO      ENDCTR
021900920320     C                   END
022000941010     C                   Z-ADD     G02DAT        VIDDSA
022100941010     C                   Z-ADD     G02INV        COMDSA
022200931203      *
022300931203      * DATA DAL NO MAGGIORE DATA AL
022400900524     C     COMDSD        IFGT      COMDSA
022500900524     C                   SETON                                        4290
022600900524     C                   GOTO      ENDCTR
022700900524     C                   END
022800931203      *
022900900524     C                   END
023000940224      *
023100941010      * LA DATA FINE PERIODO NON PUO' ESSERE SUPERIORE A "IERI"
023200941010     C     COMDSA        IFGE      WDATEU
023300941010     C     COMDSD        ORGE      WDATEU
023400941010     C                   SETON                                        51  90
023500941010     C                   GOTO      ENDCTR
023600941010     C                   ENDIF
023700041203     c* non si può richiedere ristampa per la statistica controllo entrata
023800041210     c**                 if        vidsta='R'  and vidtst='E'
023900041210     C**                 SETON                                        56  90
024000041210     C**                 GOTO      ENDCTR
024100041210     c**                 endif
024200000810      *
024300000912      * P.O. RICHIEDENTE POSTE NON PUO' LANCIARE LA STATISTICA A TERRA
024400000810      *
024500000912     C     FGSPT         IFEQ      'S'
024600000810     C     VIDTST        ANDEQ     'T'
024700000810     C                   SETON                                        59  90
024800000810     C                   GOTO      ENDCTR
024900000810     C                   ENDIF
025000940623      *
025100970718      * Aggancio le due tabelle (5A ) per le date di pulizia archiv    i
025200941010     C                   MOVE      '5A'          COD
025300970213     C                   MOVEL(P)  '1'           KEY
025400941010     C     KTAB          CHAIN     TABEL00F                           30
025500941010     C   30              MOVE      *ZERO         DS5A
025600941010     C  N30              MOVEL     TBLUNI        DS5A
025700970718     C                   MOVE      '5A'          COD
025800040921     C*****              MOVEL(P)  '2'           KEY
025900040921     C                   MOVEL(P)  'SEDE1'       KEY
026000941010     C     KTAB          CHAIN     TABEL00F                           30
026100040921     C** 30              MOVE      *ZERO         DS5A2
026200040921     C**N30              MOVEL     TBLUNI        DS5A2
026300040921     C   30              MOVE      *ZERO         DS5As1
026400040921     C  N30              MOVEL     TBLUNI        DS5As1
026500000810      *
026600941010      * I test sulle pulizie degli archivi devono essere fatte solo se:
026700941010      *   .  richiesta la 'S'=Stampa
026800941010      *   .  oppure data DAL e' precedente alla 1° data della data area
026900941010      * Per ogni statistica verifico la data area
027000941010     C                   MOVE      *BLANKS       $TEST             1
027100941010      *
027200941012     C     VIDSTA        IFNE      'S'
027300941010     C     VIDTST        IFEQ      *BLANKS
027400941010     C     VIDTST        OREQ      'E'
027500021016     C**!!!COMDSD        IFLT      C3DAPE
027600021016     c                   If        ComDsd < §SdfEntP
027700941010     C                   MOVE      'S'           $TEST
027800941010     C                   ENDIF
027900941010     C                   ENDIF
028000941010      *
028100941010     C     VIDTST        IFEQ      *BLANKS
028200941010     C     VIDTST        OREQ      'B'
028300021016     C**!!!COMDSD        IFLT      C5DAPE
028400021016     c                   If        ComDsd < §SdfBolP
028500941010     C                   MOVE      'S'           $TEST
028600941010     C                   ENDIF
028700941010     C                   ENDIF
028800941010      *
028900941010     C     VIDTST        IFEQ      *BLANKS
029000941010     C     VIDTST        OREQ      'P'
029100021016     C**!!!COMDSD        IFLT      C1DAPE
029200021016     c                   If        ComDsd < §SdfParP
029300941010     C                   MOVE      'S'           $TEST
029400941010     C                   ENDIF
029500941010     C                   ENDIF
029600941010      *
029700941010     C     VIDTST        IFEQ      *BLANKS
029800941010     C     VIDTST        OREQ      'T'
029900021016     C**!!!COMDSD        IFLT      C2DAPE
030000021016     c                   If        ComDsd < §SdfTerP
030100941010     C                   MOVE      'S'           $TEST
030200941010     C                   ENDIF
030300941010     C                   ENDIF
030400941010     C                   ENDIF
030500941010      *
030600991202    1C     VIDSTA        IFEQ      'S'
030700941010     C     $TEST         OREQ      'S'
030800941010      *
030900941010      *?   Test pulizia archivio bolle partenze (FLBLP00F, §5ABLT)   ?
031000941010     C                   MOVE      *BLANKS       $PGM
031100950111     C                   MOVEL     'FNLS61R'     $PGM
031200941010     C                   Z-ADD     §5ABLP        $DATPU            6 0
031300941010     C                   EXSR      TESTF
031400941010     C   50              GOTO      ENDCTR
031500941010      *
031600941010      * Solo per Statistica Partenze e Statistica A Terra
031700991202    2C     VIDTST        IFEQ      *BLANKS
031800941010     C     VIDTST        OREQ      'P'
031900941010     C     VIDTST        OREQ      'T'
032000950111      *?   Test pulizia archivio fogli viaggio (FNFGV00F, §5AFGV)   ?
032100941010     C                   MOVE      *BLANKS       $PGM
032200950111     C                   MOVEL     'FNLRA7R'     $PGM
032300950111     C                   Z-ADD     §5AFGV        $DATPU
032400941010     C                   EXSR      TESTF
032500941010     C   50              GOTO      ENDCTR
032600941010      *?   Test pulizia archivio bolle transito (FNBTP00F, §5ABTP)   ?
032700020802     C     SIMFEL        SETLL     FNBTP11L
032800020802     C     SIMFEL        READE     FNBTP11L                               30
032900991202    3C     *IN30         IFEQ      *OFF
033000941010     C                   MOVE      *BLANKS       $PGM
033100950111     C                   MOVEL     'FNLR85R'     $PGM
033200941010     C                   Z-ADD     §5ABTP        $DATPU
033300941010     C                   EXSR      TESTF
033400941010     C   50              GOTO      ENDCTR
033500991202    3C                   ENDIF
033600991202    2C                   ENDIF
033700941010      *
033800941010      *?   Test pulizia archivio spunte arrivi (FLBRV00F, §5ABRV)   ?
033900941010      * Solo per Statistica Controllo Entrate
034000991202    2C     VIDTST        IFEQ      *BLANKS
034100941010     C     VIDTST        OREQ      'E'
034200941010     C                   MOVE      *BLANKS       $PGM
034300950111     C                   MOVEL     'FNLR10R'     $PGM
034400941010     C                   Z-ADD     §5ABRV        $DATPU
034500941010     C                   EXSR      TESTF
034600941010     C   50              GOTO      ENDCTR
034700991202    2C                   ENDIF
034800941010      *
034900991202    1C                   ENDIF
035000941010      *
035100040921      *?   Test pulizia archivio statistiche partenze?
035200941010      * SOLO SE RICHIESTA LA RISTAMPA (R)
035300941012     C     VIDSTA        IFEQ      'R'
035400941010     C                   MOVE      *BLANKS       $PGM
035500941010     C                   MOVEL     'FNLSC7R'     $PGM
035600040921     C*****              Z-ADD     §5ASTP        $DATPU
035700040921     C                   Z-ADD     §5ASTS        $DATPU
035800941010     C                   EXSR      TESTF
035900941010     C   50              GOTO      ENDCTR
036000941010     C                   ENDIF
036100931222      *
036200931222      * Se stampa/ristampa='S' si puo' chiedere solo 1 giorno
036300941012     C     VIDSTA        IFEQ      'S'
036400931222      *
036500931222     C     VIDDSA        IFNE      *ZERO
036600931222     C     VIDDSA        ANDNE     VIDDSD
036700931222     C                   SETON                                        44  90
036800931222     C                   GOTO      ENDCTR
036900931222     C                   ENDIF
037000931222      *
037100931222      * Per l'elaborazione si puo' richiedere solo l'area
037200931222     C     VIDLNP        IFNE      *ZERO
037300931222     C                   SETON                                        46  90
037400931222     C                   GOTO      ENDCTR
037500931222     C                   ENDIF
037600931224      *
037700931224      * In sede si puo' fare solo la ristampa
037800931224     C     SIMFEL        IFEQ      *ZERO
037900931224     C                   SETON                                        49  90
038000931224     C                   GOTO      ENDCTR
038100931224     C                   ENDIF
038200931222      *
038300931222     C                   ENDIF
038400931203      *
038500931203      * Se solo un giorno non puo' essere di festivo
038600931203     C     VIDTST        IFEQ      'T'
038700941012     C     VIDSTA        ANDEQ     'S'
038800991227     C     CNBCAK        CHAIN     AZCLN01L                           45
038900991227     C  N45              MOVE      CLNPOM        GMEDS
039000991227     C  N45GME($GI)      COMP      'F'                                    45
039100931203     C   45              SETON                                        90
039200931203     C                   ENDIF
039300970213     C* SE RICHIESTO UN RANGE VERIFICO CHE NON SIA A CAVALLO DELLA
039400970213     C* DATA DA CUI ELABORARE LE NUOVE STATISTICHE
039500970213     C* (SOLO SE RICHIESTA LA STATISTICA DEL PARTITO)
039600970213     C     VIDTST        IFEQ      *BLANKS
039700970213     C     VIDTST        OREQ      'P'
039800970213     C     COMDSD        IFLT      COMDSA
039900970213     C     §7ADEL        ANDGT     COMDSD
040000970213     C     §7ADEL        ANDLE     COMDSA
040100970213     C                   MOVEA     MSG(1)        K78
040200970213     C                   MOVE      §7ADEL        WDATA
040300970213     C                   MOVEA     WGG           K78(68)
040400970213     C                   MOVEA     WMM           K78(71)
040500970213     C                   MOVEA     WAA           K78(74)
040600970213     C                   MOVEA     K78           V1ZMSG
040700970213     C                   SETON                                        582890
040800970213     C                   GOTO      ENDCTR
040900970213     C                   END
041000970213     C                   END
041100931203      *
041200931203      * (NON E' OBBLIGATORIA SE 1°LVL, MA SE IMMESSA, DEVE ESISTERE IN '£1'(L1).
041300931203      * CONTROLLO LINEA DI PARTENZA SE IMMESSA
041400980707    1C     VIDLNP        IFNE      0
041500050829     c*
041600050829     c* Verifico se presente nel periodo richiesto
041700050829     c                   z-add     vidlnp        ktfp
041800050829     c                   z-add     simfel        ktfe
041900050829     c                   setoff                                       30
042000050829     c                   if        comdsa=0
042100050829     c                   eval      ctrdsa=comdsd
042200050829     c                   else
042300050829     c                   eval      ctrdsa=comdsa
042400050829     c                   endif
042500050829     c
042600050829     c     kcae          setll     azcae03l
042700050829     c     kcae          reade     azcae03l
042800050829     c                   dow       not %eof(azcae03l)  and not *in30
042900050829     c                   if        (caedde<comdsd and caedsc<comdsd) or
043000050829     c                             (caedde>CTRdsa and caedsc>CTRdsa)
043100050829     c     kcae          reade     azcae03l
043200050829     c                   else
043300050829     c                   seton                                        30
043400050829     c                   endif
043500050829     c                   enddo
043600050829     c
043700050829     c                   if        %eof(azcae03l)
043800050829     c                   seton                                        4390
043900050829     C                   GOTO      ENDCTR
044000050829     C                   ENDIF
044100050829     c
044200050829     C**   VIDLNP        LOOKUP    L1                                     30
044300050829    2C**   *IN30         IFEQ      *OFF
044400980707     C* SE NON C'E' IN L1 CONTROLLO SE NELLA DATA DI LANCIO C'ERA
044500980707     C* MA PUO' ESSERE LANCIATA SOLO UNA RISTAMPA
044600050829     C*+                 CLEAR                   DSLV55
044700050829     C**                 MOVEL     'P'           D55TPT
044800050829     C**                 MOVE      VIDLNP        D55LIN
044900050829     C**                 Z-ADD     WDATEU        D55DRF
045000050829    3C**   COMDSA        IFGT      0
045100050829     C**                 Z-ADD     COMDSA        D55DRF
045200050829     C**                 ELSE
045300050829     C**                 Z-ADD     COMDSD        D55DRF
045400050829    3C*+                 ENDIF
045500050829     C**                 CALL      'FNLV55R'
045600050829     C**                 PARM                    DSLV55
045700050829    3C**   SIMFEL        IFNE      D55TFP
045800050829     C**                 SETON                                        4390
045900050829     C**                 GOTO      ENDCTR
046000050829    3C**                 ENDIF
046100050829     C**                 MOVEL     'N'           WL1               1
046200050829   X2C**                 ELSE
046300050829     C**                 MOVEL     'S'           WL1               1
046400050829    2C**                 ENDIF
046500000912     C**
046600000606      * .. CONTROLLO CHE RISPETTI LE REGOLE DI ELABORAZIONE (PER POSTE)
046700000608     C                   CLEAR                   FNLSC4
046800000608     C                   Z-ADD     VIDLNP        IC4LIN
046900000608     C                   MOVE      'I'           IC4FLG
047000000608     C                   MOVEL     FNLSC4        KPJBU
047100050829     C* COME DATA DI RIFERIMENTO PASSO la data scadenza del terminal
047200050829     c*  trovato
047300050829     c                   z-add     caedsc        ic4drf
047400000608     C                   CALL      'FNLSC4R1'
047500000606     C                   PARM                    KPJBA
047600050829     C                   PARM                    Ic4drf
047700050829     C
047800000608     C                   MOVEL     KPJBU         FNLSC4
047900000606      *
048000000608     C     OC4ESE        IFNE      'S'
048100000606     C                   SETON                                        4390
048200000606     C                   GOTO      ENDCTR
048300000606     C                   ENDIF
048400000912      *
048500000912      * PER P.O. POSTE NON SI PUO' LANCIARE LA STATISTICA A TERRA
048600000912      *
048700000912     C     OC4PT1        IFEQ      'S'
048800000912     C     VIDTST        ANDEQ     'T'
048900000912     C                   SETON                                        53  90
049000000912     C                   GOTO      ENDCTR
049100000912     C                   ENDIF
049200000912      *
049300000606      *
049400980707    1C                   END
049500931222      *
049600931222      * O L'AREA DI PARTENZA O LA FILIALE DI PARTENZA
049700931222     C     VIDARE        IFNE      *ZERO
049800931222     C     VIDLNP        ANDNE     *ZERO
049900931222     C                   SETON                                        47  90
050000931222     C                   GOTO      ENDCTR
050100931222     C                   ENDIF
050200931222      *
050300931222      * IL COD. AREA DEVE ESSERE VALIDO
050400931222     C     VIDARE        IFNE      *ZERO
050500971201     C                   CLEAR                   DSLV55
050600971201     C                   MOVEL     'P'           D55TPT
050700971201     C                   MOVE      VIDARE        D55LIN
050800971201     C                   Z-ADD     WDATEU        D55DRF
050900971201     C                   CALL      'FNLV55R'
051000971201     C                   PARM                    DSLV55
051100971201     C     D55ERR        IFEQ      *BLANKS
051200971201     C     VIDARE        ANDNE     D55TFP
051300971201     C                   SETON                                        4890
051400931222     C                   GOTO      ENDCTR
051500931222     C                   ENDIF
051600971201     C                   ENDIF
051700980707     C* NON POSSO RIELABORARE SE NON E' PIU' IN L1
051800050829     c* controllo inutile: non posso mettere lnp se elaboro
051900050829     C**   VIDLNP        IFGT      0
052000050829     C**   VIDSTA        ANDEQ     'S'
052100050829     C**   WL1           ANDEQ     'N'
052200050829     C**                 SETON                                        4890
052300050829     C**                 GOTO      ENDCTR
052400050829     C**                 ENDIF
052500010524      *
052600010524      * STAMPA SOLO TOTALI X STATISTICA ENTRATE
052700010524     C     VIDTOT        IFEQ      'S'
052800010524     C     VIDTST        ANDNE     'E'
052900010601     C     VIDTST        ANDNE     ' '
053000010601     C                   SETON                                        54  90
053100010524     C                   GOTO      ENDCTR
053200010524     C                   ELSE
053300010524     C                   ENDIF
053400010524      *
053500900524     C     ENDCTR        ENDSR
053600931203      *****************************************************************
053700931203      *    CARICO TABELLE
053800931203      *****************************************************************
053900920930     C     CARTAB        BEGSR
054000931203      *
054100020417      * CARICO FILIALI GESTITE £1
054200050829     c**                 clear                   trul06ds
054300050829     c**                 eval      d06cod = '£1'
054400050829     c**                 movel     simfel        d06key
054500050829     c**                 movel     trul06ds      kpjbu
054600050829     c**                 call      'TRUL06R'
054700050829     c**                 parm                    kpjba
054800050829     c**                 movel     kpjbu         trul06ds
054900050829     c**                 movea     lin           l1
055000931203      *
055100931203      * VEDO SE SONO SIMFEL(1°LVL) O REMOTA (2°LVL)
055200931203      *SE LA RICHIEDENTE E' UN 1° LIVELLO, QUELLA DA STAMPARE PUO' ESSERE
055300931203      *SIA UN 1° CHE UN 2° LIVELLO. (MODIFICABILE DA VIDEO)
055400931203      *SE INVECE E' UN 2°LIVELLO, PUO' STAMPARE SOLO SE STESSA.(PROTECT A VIDEO)
055500931203      *
055600931203      * SE REMOTA (2°LVL), METTO SE STESSA NON MODIFICABILE (05=PR), PERCHE' IL
055700931203      * 2°LVL, PUO' VEDERE SOLO SE STESSA.ABILE (05=PR), PERCHE' IL
055800020513     c     simtpp        ifeq      '2'
055900020513     c     simtpp        oreq      *blanks
056000020513     c                   movel     simpou        ds1fgs
056100920930     C                   SETON                                        05
056200920930     C                   ELSE
056300931203      *
056400931203      * SE INVECE (1°LVL), METTO SIMFEL (SE STESSA), MA MODIFICABILE, PERCHE' IL
056500931203      * 1°LVL, PUO' VEDERE ANCHE LE 2°LVL.
056600950210     C                   MOVEL     SIMFEL        DS1FGS
056700920930     C                   END
056800000810     C                   MOVEL     DS1FGS        WPOSTE            3 0
056900000810     C****                 MOVELDS1FGS    VIDLNP
057000941010     C                   Z-ADD     *ZERO         VIDLNP
057100931203      *
057200000810      * CONTROLLO SE SONO POSTE (§OGPT=S)
057300000810      *
057400000810     C                   CLEAR                   §OGPT
057500000810     C     WPOSTE        CHAIN     AZORG01L                           57
057600000810     C  N57              MOVEL     ORGDE3        OG143
057700000912     C                   MOVEL     §OGPT         FGSPT             1
057800000810      *
057900931217     C     SIMFEL        COMP      *ZERO                                  06
058000021016
058100021016      * Recupero le date delle statistiche
058200021016     c                   Clear                   Tibs02Ds
058300021016     c                   Clear                   dSdf
058400021016     c                   Eval      T02Mod = 'C'
058500021016     c                   Eval      T02Sif = Knsif
058600021016     c                   Eval      T02Cod = 'SDF'
058700021016     c                   Movel(p)  Simfel        T02Ke1
058800021016     c                   Call      'TIBS02R'
058900021016     c                   Parm                    Kpjba
059000021016     c                   Parm                    Tibs02Ds
059100021016     c                   If        T02Err = *Blanks
059200021016     c                   Movel     T02Uni        dSdf
059300021016     c                   EndIf
059400021016
059500970213     c* PRELEVO LA DATA DA CUI ELABORARE LE NUOVE STATISTICHE DA TAB.7A
059600970213     C                   MOVEL     '7A'          COD
059700970213     C                   MOVEL(P)  '2'           KEY
059800970213     C     KTAB          CHAIN     TABEL00F                           30
059900970213     C     *IN30         IFEQ      *OFF
060000970213     C                   MOVEL     TBLUNI        DS7A2
060100970213     C     §7ADEL        IFEQ      *ZEROS
060200970213     C                   MOVE      *HIVAL        §7ADEL
060300970213     C                   END
060400970213     C                   ELSE
060500970213     C                   MOVE      *HIVAL        §7ADEL
060600970213     C                   END
060700931217      *
060800920930     C                   ENDSR
060900940224      **************************************************************************
061000940224      * Viene rilevato l'ultimo giorno di utilizzo del pgm che si
061100940224      * occupa della pulizia archivio (ogni file, o quasi, ha il proprio pgm);
061200970718      * nella tabella 5A  c'e' il nr.giorni di mantenimento dei dati; si
061300940224      * sottrae quest'ultimo dal giorno ultimo utilizzo per sapere se vi sono
061400940224      * ancora disponibili i dati per la statistica.
061500940224      **************************************************************************
061600940224     C     TESTF         BEGSR
061700940224      *
061800940224     C                   CLEAR                   PRIDAT
061900940224      *
062000940224      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA STATISTICHE ARRIVI
062100940224     C                   MOVE      *BLANKS       $DAT
062200940224     C                   MOVE      *BLANKS       $ERR
062300950111     C                   CALL      'TRUL49C'                            31
062400940224     C                   PARM                    $PGM             10
062500000103     C                   PARM                    $DAT              8
062600940224     C                   PARM                    $ERR              1
062700940224      *
062800941010      * PROGRAMMA IN USO DATA ULTIMO ULIZZO = DATA DEL GIORNO
062900040921      * idem se data ultimo utilizzo = 0
063000970306    1C     $ERR          IFEQ      '1'
063100950111     C     *IN31         OREQ      '1'
063200040921     c     $dat          oreq      '00000000'
063300970306     C                   MOVE      WDATEU        G02INV
063400970306   X1C                   ELSE
063500040921    2C***  $DAT          IFEQ      *BLANKS
063600950327     C                   CLEAR                   G02INV
063700040921   X2C****               ELSE
063800940224     C                   MOVE      $DAT          G02INV
063900040921    2C****               ENDIF
064000970306    1C                   ENDIF
064100940224      *
064200940224      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
064300940224      * PULIZIA E TROVO LA PRIMA DATA AL DI SOTTO DELLA QUALE NON SI
064400940224      * POSSONO LANCIARE ELABORAZIONI PER MANCANZA DI DATI
064500940224      *
064600940224      * SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
064700040921     C***  G02INV        IFNE      *ZEROS
064800940224     C                   Z-ADD     *ZEROS        G02DAT
064900940224     C                   Z-ADD     *ZEROS        G02TGI
065000940224     C                   MOVEL     '3'           G02ERR
065100941010     C                   CALL      'XSRDA8'
065200940224     C                   PARM                    WLBDAT
065300940224      *
065400940224     C                   SUB       $DATPU        G02TGI
065500940224     C                   Z-ADD     *ZEROS        GIODAT
065600940224     C                   Z-ADD     *ZEROS        GIOINV
065700940224     C                   Z-ADD     G02TGI        GIOTGI
065800941010     C                   CALL      'XSRGI8'
065900940224     C                   PARM                    WGIDAT
066000940224      *
066100941010     C                   Z-ADD     GIOINV        PRIDAT            8 0
066200040921     C*****              ENDIF
066300940224      *
066400940224      * DATA DAL MAGGIORE DATA CALCOLATA
066500940224     C     COMDSD        IFLE      PRIDAT
066600940224     C                   SETON                                        5090
066700940224     C                   ENDIF
066800041210     c* se sono edp e sono il ambiente di prova non do errore
066900041210     c                   movel     knmus         winizio           3
067000041210     c                   if        winizio='EDP' and knsif='FILTRAPFP'
067100041210     C                   SETOFF                                       5090
067200041210     C                   ENDIF
067300940224      *
067400940224     C                   ENDSR
067500970213**
067600970213Impossibile elaborare la stat.Partito per un periodo a cavallo del xx/xx/xxxx
