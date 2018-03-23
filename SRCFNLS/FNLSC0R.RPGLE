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
007900090914     d wforza          s              1
008000050829     d CTRdsa          s                   like(comdsa)
008100050829
008200021016     d Tibs02Ds      e ds
008300021016      * Ds date statistiche
008400021016     d dSdf          e ds
008500931203      *
008600931203      *---------------------------------------------------------------*
008700000000     C     *ENTRY        PLIST
008800000000     C                   PARM                    KPJBA
008900000000     C                   Z-ADD     1             CODUT
009000920930     C                   CALL      'X§PARUT'
009100920930     C                   PARM                    UT§DSE
009200000000     C                   MOVEL     RAGUT         RSUT             20
009300920930     C                   MOVEL     REC80         CNCR80
009400931203      *---------------------------------------------------------------*
009500931203      * ACCESSO   TABEL
009600920930     C     KTAB          KLIST
009700920930     C                   KFLD                    CODUT
009800920930     C                   KFLD                    COD               2
009900920930     C                   KFLD                    KEY               8
010000931203     C     CNBCAK        KLIST
010100991227     C                   KFLD                    W0030             3 0
010200991227     C                   KFLD                    W0030
010300991227     C                   KFLD                    BCAANN
010400931203     C                   KFLD                    BCAMES
010500050829     C     KCAE          KLIST
010600050829     C                   KFLD                    KEPA
010700050829     C                   KFLD                    KTFP
010800050829     C                   KFLD                    KTFE
010900950210     C*
011000950210     C     *LIKE         DEFINE    VIDDSA        COMDSA
011100950210     C     *LIKE         DEFINE    VIDDSD        COMDSD
011200991227     C                   CLEAR                   W0030
011300941010     C*
011400941010     C* RILEVO LA DATA DEL GIORNO E LA INVERTO
011500941010     C                   TIME                    WTIME            14 0
011600941010     C                   MOVE      WTIME         WDATE             8 0
011700941010     C                   Z-ADD     WDATE         G02DAT
011800941010     C                   Z-ADD     *ZERO         G02INV
011900941010     C                   MOVEL     *BLANKS       G02ERR
012000941010     C                   CALL      'XSRDA8'
012100941010     C                   PARM                    WLBDAT
012200941010     C                   Z-ADD     G02INV        WDATEU            8 0
012300931203      *
012400900524     C                   Z-ADD     0             VIDDSD
012500900524     C                   Z-ADD     0             VIDDSA
012600950210     C                   Z-ADD     0             COMDSA
012700930923     C                   MOVE      *BLANKS       VIDTST
012800950118     C                   MOVE      'R'           VIDSTA
012900931203      *
013000920930     C                   EXSR      CARTAB
013100931203      *
013200941010     C                   WRITE     LSC0T01
013300900509     C     FOR01         TAG
013400941010     C                   EXFMT     LSC0D01
013500931203      *
013600050829      *  F3 - FINE LAVORO
013700900509     C   KC              GOTO      FINE
013800931203      * CONTROLLO
013900900524     C                   EXSR      CONTR
014000090914     c*   per 50 e per EDP do msg forzabile
014100090914     c                   if        *in50 and
014200090914     c                                   %subst(knmus:1:3)='EDP'
014300090914     c                   if        wforza=' '
014400090914     c                   eval      wforza='S'
014500090914     c                   else
014600090914     c                   setoff                                       5090
014700090914     c                   endif
014800090914     c                   endif
014900941010     C  NKF
015000900524     COR 90              GOTO      FOR01
015100010524      *
015200010524      * SE SCELTA STAMPA SOLO TOTALI
015300010524     C     VIDTOT        IFEQ      'S'
015400010524     C     VIDTST        ANDEQ     'E'
015500010524     C                   MOVEL     'S'           DS1FL5
015600010524     C                   ENDIF
015700940426      *
015800941010      * SE NON SIAMO IN SEDE IMPOSTO LA FILIALE ELABORATORE
015900941010     C     SIMFEL        IFNE      *ZERO
016000941010     C                   Z-ADD     SIMFEL        VIDARE
016100941010     C                   ENDIF
016200940224      *
016300941010      * Se la data fine periodo e' nulla la imposto come quella iniziale
016400941010     C     COMDSA        IFEQ      *ZERO
016500941010     C                   Z-ADD     VIDDSD        VIDDSA
016600941010     C                   Z-ADD     COMDSD        COMDSA
016700941010     C                   ENDIF
016800950210      *
016900950216     C                   MOVEL     'P'           DS1TBO
017000950210     C                   Z-ADD     VIDARE        DS1FEL
017100950210     C                   Z-ADD     VIDLNP        DS1LNP
017200950210     C                   Z-ADD     VIDDSD        DS1GDA
017300950210     C                   Z-ADD     VIDDSA        DS1GAL
017400950210     C                   Z-ADD     COMDSD        DS1ADA
017500950210     C                   Z-ADD     COMDSA        DS1AAL
017600970207     C                   MOVEL     VIDTST        DS1FPG
017700950210     C                   MOVEL     VIDSTA        DS1RIS
017800931129      *
017900941010      * Sottometto il pgm che si occupera' del lancio effettivo delle statist.
018000950210     C                   CLEAR                   KPJBU
018100950210     C                   MOVEL     DSLS01        KPJBU
018200941010     C                   CALL      'FNLSC8R'
018300931129     C                   PARM                    KPJBA
018400970305     C* SOTTOMETTO TRASMISISONE STATISTICHE SE RIELABORAIZONE DATI
018500040921     C***  VIDSTA        IFEQ      'S'
018600040921     C***  VIDTST        ANDNE     'E'
018700040921     C***                CLEAR                   KPJBU
018800040921     C***                MOVEL     'LSC6'        KCOAZ
018900040921     C***                CALL      'BCH10'
019000040921     C***                PARM                    KPJBA
019100040921     C***                ENDIF
019200931203      *
019300000000     C     FINE          TAG
019400971201     C*
019500971201     C                   MOVEL     'C'           D55TLA
019600971201     C                   CALL      'FNLV55R'
019700971201     C                   PARM                    DSLV55
019800971201     C*
019900000000     C                   SETON                                        LR
020000931203      *****************************************************************
020100931203      *    CONTROLLI FORMATO1
020200931203      *****************************************************************
020300900524     C     CONTR         BEGSR
020400931203      *
020500970213     C                   SETOFF                                       902858
020600000810     C                   SETOFF                                       59
020700931203      * DATA SPEDIZIONE DAL OBBLIGATORIA
020800970213     C                   CLEAR                   WLBDAT
020900920320     C                   MOVE      VIDDSD        G02DAT
021000941010     C                   CALL      'XSRDA8'
021100920320     C                   PARM                    WLBDAT
021200920320     C     G02ERR        IFEQ      '1'
021300920320     C                   SETON                                        40  90
021400920320     C                   GOTO      ENDCTR
021500920320     C                   END
021600941010     C                   Z-ADD     G02DAT        VIDDSD
021700941010     C                   Z-ADD     G02INV        COMDSD
021800931203      *
021900940202      * DATA SPEDIZIONE "AL" NON OBBLIGATORIA
022000941010     C                   Z-ADD     *ZERO         COMDSA
022100900524     C     VIDDSA        IFNE      0
022200970213     C                   CLEAR                   WLBDAT
022300920320     C                   MOVE      VIDDSA        G02DAT
022400941010     C                   CALL      'XSRDA8'
022500920320     C                   PARM                    WLBDAT
022600920320     C     G02ERR        IFEQ      '1'
022700920320     C                   SETON                                        41  90
022800920320     C                   GOTO      ENDCTR
022900920320     C                   END
023000941010     C                   Z-ADD     G02DAT        VIDDSA
023100941010     C                   Z-ADD     G02INV        COMDSA
023200931203      *
023300931203      * DATA DAL NO MAGGIORE DATA AL
023400900524     C     COMDSD        IFGT      COMDSA
023500900524     C                   SETON                                        4290
023600900524     C                   GOTO      ENDCTR
023700900524     C                   END
023800931203      *
023900900524     C                   END
024000940224      *
024100941010      * LA DATA FINE PERIODO NON PUO' ESSERE SUPERIORE A "IERI"
024200941010     C     COMDSA        IFGE      WDATEU
024300941010     C     COMDSD        ORGE      WDATEU
024400941010     C                   SETON                                        51  90
024500941010     C                   GOTO      ENDCTR
024600941010     C                   ENDIF
024700041203     c* non si può richiedere ristampa per la statistica controllo entrata
024800041210     c**                 if        vidsta='R'  and vidtst='E'
024900041210     C**                 SETON                                        56  90
025000041210     C**                 GOTO      ENDCTR
025100041210     c**                 endif
025200000810      *
025300000912      * P.O. RICHIEDENTE POSTE NON PUO' LANCIARE LA STATISTICA A TERRA
025400000810      *
025500000912     C     FGSPT         IFEQ      'S'
025600000810     C     VIDTST        ANDEQ     'T'
025700000810     C                   SETON                                        59  90
025800000810     C                   GOTO      ENDCTR
025900000810     C                   ENDIF
026000940623      *
026100970718      * Aggancio le due tabelle (5A ) per le date di pulizia archiv    i
026200941010     C                   MOVE      '5A'          COD
026300970213     C                   MOVEL(P)  '1'           KEY
026400941010     C     KTAB          CHAIN     TABEL00F                           30
026500941010     C   30              MOVE      *ZERO         DS5A
026600941010     C  N30              MOVEL     TBLUNI        DS5A
026700970718     C                   MOVE      '5A'          COD
026800040921     C*****              MOVEL(P)  '2'           KEY
026900040921     C                   MOVEL(P)  'SEDE1'       KEY
027000941010     C     KTAB          CHAIN     TABEL00F                           30
027100040921     C** 30              MOVE      *ZERO         DS5A2
027200040921     C**N30              MOVEL     TBLUNI        DS5A2
027300040921     C   30              MOVE      *ZERO         DS5As1
027400040921     C  N30              MOVEL     TBLUNI        DS5As1
027500000810      *
027600941010      * I test sulle pulizie degli archivi devono essere fatte solo se:
027700941010      *   .  richiesta la 'S'=Stampa
027800941010      *   .  oppure data DAL e' precedente alla 1° data della data area
027900941010      * Per ogni statistica verifico la data area
028000941010     C                   MOVE      *BLANKS       $TEST             1
028100941010      *
028200941012     C     VIDSTA        IFNE      'S'
028300941010     C     VIDTST        IFEQ      *BLANKS
028400941010     C     VIDTST        OREQ      'E'
028500021016     C**!!!COMDSD        IFLT      C3DAPE
028600021016     c                   If        ComDsd < §SdfEntP
028700941010     C                   MOVE      'S'           $TEST
028800941010     C                   ENDIF
028900941010     C                   ENDIF
029000941010      *
029100941010     C     VIDTST        IFEQ      *BLANKS
029200941010     C     VIDTST        OREQ      'B'
029300021016     C**!!!COMDSD        IFLT      C5DAPE
029400021016     c                   If        ComDsd < §SdfBolP
029500941010     C                   MOVE      'S'           $TEST
029600941010     C                   ENDIF
029700941010     C                   ENDIF
029800941010      *
029900941010     C     VIDTST        IFEQ      *BLANKS
030000941010     C     VIDTST        OREQ      'P'
030100021016     C**!!!COMDSD        IFLT      C1DAPE
030200021016     c                   If        ComDsd < §SdfParP
030300941010     C                   MOVE      'S'           $TEST
030400941010     C                   ENDIF
030500941010     C                   ENDIF
030600941010      *
030700941010     C     VIDTST        IFEQ      *BLANKS
030800941010     C     VIDTST        OREQ      'T'
030900021016     C**!!!COMDSD        IFLT      C2DAPE
031000021016     c                   If        ComDsd < §SdfTerP
031100941010     C                   MOVE      'S'           $TEST
031200941010     C                   ENDIF
031300941010     C                   ENDIF
031400941010     C                   ENDIF
031500941010      *
031600991202    1C     VIDSTA        IFEQ      'S'
031700941010     C     $TEST         OREQ      'S'
031800941010      *
031900941010      *?   Test pulizia archivio bolle partenze (FLBLP00F, §5ABLT)   ?
032000941010     C                   MOVE      *BLANKS       $PGM
032100950111     C                   MOVEL     'FNLS61R'     $PGM
032200941010     C                   Z-ADD     §5ABLP        $DATPU            6 0
032300941010     C                   EXSR      TESTF
032400941010     C   50              GOTO      ENDCTR
032500941010      *
032600941010      * Solo per Statistica Partenze e Statistica A Terra
032700991202    2C     VIDTST        IFEQ      *BLANKS
032800941010     C     VIDTST        OREQ      'P'
032900941010     C     VIDTST        OREQ      'T'
033000950111      *?   Test pulizia archivio fogli viaggio (FNFGV00F, §5AFGV)   ?
033100941010     C                   MOVE      *BLANKS       $PGM
033200950111     C                   MOVEL     'FNLRA7R'     $PGM
033300950111     C                   Z-ADD     §5AFGV        $DATPU
033400941010     C                   EXSR      TESTF
033500941010     C   50              GOTO      ENDCTR
033600941010      *?   Test pulizia archivio bolle transito (FNBTP00F, §5ABTP)   ?
033700020802     C     SIMFEL        SETLL     FNBTP11L
033800020802     C     SIMFEL        READE     FNBTP11L                               30
033900991202    3C     *IN30         IFEQ      *OFF
034000941010     C                   MOVE      *BLANKS       $PGM
034100950111     C                   MOVEL     'FNLR85R'     $PGM
034200941010     C                   Z-ADD     §5ABTP        $DATPU
034300941010     C                   EXSR      TESTF
034400941010     C   50              GOTO      ENDCTR
034500991202    3C                   ENDIF
034600991202    2C                   ENDIF
034700941010      *
034800941010      *?   Test pulizia archivio spunte arrivi (FLBRV00F, §5ABRV)   ?
034900941010      * Solo per Statistica Controllo Entrate
035000991202    2C     VIDTST        IFEQ      *BLANKS
035100941010     C     VIDTST        OREQ      'E'
035200941010     C                   MOVE      *BLANKS       $PGM
035300950111     C                   MOVEL     'FNLR10R'     $PGM
035400941010     C                   Z-ADD     §5ABRV        $DATPU
035500941010     C                   EXSR      TESTF
035600941010     C   50              GOTO      ENDCTR
035700991202    2C                   ENDIF
035800941010      *
035900991202    1C                   ENDIF
036000941010      *
036100040921      *?   Test pulizia archivio statistiche partenze?
036200941010      * SOLO SE RICHIESTA LA RISTAMPA (R)
036300941012     C     VIDSTA        IFEQ      'R'
036400941010     C                   MOVE      *BLANKS       $PGM
036500941010     C                   MOVEL     'FNLSC7R'     $PGM
036600040921     C*****              Z-ADD     §5ASTP        $DATPU
036700040921     C                   Z-ADD     §5ASTS        $DATPU
036800941010     C                   EXSR      TESTF
036900941010     C   50              GOTO      ENDCTR
037000941010     C                   ENDIF
037100931222      *
037200931222      * Se stampa/ristampa='S' si puo' chiedere solo 1 giorno
037300941012     C     VIDSTA        IFEQ      'S'
037400931222      *
037500931222     C     VIDDSA        IFNE      *ZERO
037600931222     C     VIDDSA        ANDNE     VIDDSD
037700931222     C                   SETON                                        44  90
037800931222     C                   GOTO      ENDCTR
037900931222     C                   ENDIF
038000931222      *
038100931222      * Per l'elaborazione si puo' richiedere solo l'area
038200931222     C     VIDLNP        IFNE      *ZERO
038300931222     C                   SETON                                        46  90
038400931222     C                   GOTO      ENDCTR
038500931222     C                   ENDIF
038600931224      *
038700931224      * In sede si puo' fare solo la ristampa
038800931224     C     SIMFEL        IFEQ      *ZERO
038900931224     C                   SETON                                        49  90
039000931224     C                   GOTO      ENDCTR
039100931224     C                   ENDIF
039200931222      *
039300931222     C                   ENDIF
039400931203      *
039500931203      * Se solo un giorno non puo' essere di festivo
039600931203     C     VIDTST        IFEQ      'T'
039700941012     C     VIDSTA        ANDEQ     'S'
039800991227     C     CNBCAK        CHAIN     AZCLN01L                           45
039900991227     C  N45              MOVE      CLNPOM        GMEDS
040000991227     C  N45GME($GI)      COMP      'F'                                    45
040100931203     C   45              SETON                                        90
040200931203     C                   ENDIF
040300970213     C* SE RICHIESTO UN RANGE VERIFICO CHE NON SIA A CAVALLO DELLA
040400970213     C* DATA DA CUI ELABORARE LE NUOVE STATISTICHE
040500970213     C* (SOLO SE RICHIESTA LA STATISTICA DEL PARTITO)
040600970213     C     VIDTST        IFEQ      *BLANKS
040700970213     C     VIDTST        OREQ      'P'
040800970213     C     COMDSD        IFLT      COMDSA
040900970213     C     §7ADEL        ANDGT     COMDSD
041000970213     C     §7ADEL        ANDLE     COMDSA
041100970213     C                   MOVEA     MSG(1)        K78
041200970213     C                   MOVE      §7ADEL        WDATA
041300970213     C                   MOVEA     WGG           K78(68)
041400970213     C                   MOVEA     WMM           K78(71)
041500970213     C                   MOVEA     WAA           K78(74)
041600970213     C                   MOVEA     K78           V1ZMSG
041700970213     C                   SETON                                        582890
041800970213     C                   GOTO      ENDCTR
041900970213     C                   END
042000970213     C                   END
042100931203      *
042200931203      * (NON E' OBBLIGATORIA SE 1°LVL, MA SE IMMESSA, DEVE ESISTERE IN '£1'(L1).
042300931203      * CONTROLLO LINEA DI PARTENZA SE IMMESSA
042400980707    1C     VIDLNP        IFNE      0
042500050829     c*
042600050829     c* Verifico se presente nel periodo richiesto
042700050829     c                   z-add     vidlnp        ktfp
042800050829     c                   z-add     simfel        ktfe
042900050829     c                   setoff                                       30
043000050829     c                   if        comdsa=0
043100050829     c                   eval      ctrdsa=comdsd
043200050829     c                   else
043300050829     c                   eval      ctrdsa=comdsa
043400050829     c                   endif
043500050829     c
043600050829     c     kcae          setll     azcae03l
043700050829     c     kcae          reade     azcae03l
043800050829     c                   dow       not %eof(azcae03l)  and not *in30
043900050829     c                   if        (caedde<comdsd and caedsc<comdsd) or
044000050829     c                             (caedde>CTRdsa and caedsc>CTRdsa)
044100050829     c     kcae          reade     azcae03l
044200050829     c                   else
044300050829     c                   seton                                        30
044400050829     c                   endif
044500050829     c                   enddo
044600050829     c
044700050829     c                   if        %eof(azcae03l)
044800050829     c                   seton                                        4390
044900050829     C                   GOTO      ENDCTR
045000050829     C                   ENDIF
045100050829     c
045200050829     C**   VIDLNP        LOOKUP    L1                                     30
045300050829    2C**   *IN30         IFEQ      *OFF
045400980707     C* SE NON C'E' IN L1 CONTROLLO SE NELLA DATA DI LANCIO C'ERA
045500980707     C* MA PUO' ESSERE LANCIATA SOLO UNA RISTAMPA
045600050829     C*+                 CLEAR                   DSLV55
045700050829     C**                 MOVEL     'P'           D55TPT
045800050829     C**                 MOVE      VIDLNP        D55LIN
045900050829     C**                 Z-ADD     WDATEU        D55DRF
046000050829    3C**   COMDSA        IFGT      0
046100050829     C**                 Z-ADD     COMDSA        D55DRF
046200050829     C**                 ELSE
046300050829     C**                 Z-ADD     COMDSD        D55DRF
046400050829    3C*+                 ENDIF
046500050829     C**                 CALL      'FNLV55R'
046600050829     C**                 PARM                    DSLV55
046700050829    3C**   SIMFEL        IFNE      D55TFP
046800050829     C**                 SETON                                        4390
046900050829     C**                 GOTO      ENDCTR
047000050829    3C**                 ENDIF
047100050829     C**                 MOVEL     'N'           WL1               1
047200050829   X2C**                 ELSE
047300050829     C**                 MOVEL     'S'           WL1               1
047400050829    2C**                 ENDIF
047500000912     C**
047600000606      * .. CONTROLLO CHE RISPETTI LE REGOLE DI ELABORAZIONE (PER POSTE)
047700000608     C                   CLEAR                   FNLSC4
047800000608     C                   Z-ADD     VIDLNP        IC4LIN
047900000608     C                   MOVE      'I'           IC4FLG
048000000608     C                   MOVEL     FNLSC4        KPJBU
048100050829     C* COME DATA DI RIFERIMENTO PASSO la data scadenza del terminal
048200050829     c*  trovato
048300050829     c                   z-add     caedsc        ic4drf
048400000608     C                   CALL      'FNLSC4R1'
048500000606     C                   PARM                    KPJBA
048600050829     C                   PARM                    Ic4drf
048700050829     C
048800000608     C                   MOVEL     KPJBU         FNLSC4
048900000606      *
049000000608     C     OC4ESE        IFNE      'S'
049100000606     C                   SETON                                        4390
049200000606     C                   GOTO      ENDCTR
049300000606     C                   ENDIF
049400000912      *
049500000912      * PER P.O. POSTE NON SI PUO' LANCIARE LA STATISTICA A TERRA
049600000912      *
049700000912     C     OC4PT1        IFEQ      'S'
049800000912     C     VIDTST        ANDEQ     'T'
049900000912     C                   SETON                                        53  90
050000000912     C                   GOTO      ENDCTR
050100000912     C                   ENDIF
050200000912      *
050300000606      *
050400980707    1C                   END
050500931222      *
050600931222      * O L'AREA DI PARTENZA O LA FILIALE DI PARTENZA
050700931222     C     VIDARE        IFNE      *ZERO
050800931222     C     VIDLNP        ANDNE     *ZERO
050900931222     C                   SETON                                        47  90
051000931222     C                   GOTO      ENDCTR
051100931222     C                   ENDIF
051200931222      *
051300931222      * IL COD. AREA DEVE ESSERE VALIDO
051400931222     C     VIDARE        IFNE      *ZERO
051500971201     C                   CLEAR                   DSLV55
051600971201     C                   MOVEL     'P'           D55TPT
051700971201     C                   MOVE      VIDARE        D55LIN
051800971201     C                   Z-ADD     WDATEU        D55DRF
051900971201     C                   CALL      'FNLV55R'
052000971201     C                   PARM                    DSLV55
052100971201     C     D55ERR        IFEQ      *BLANKS
052200971201     C     VIDARE        ANDNE     D55TFP
052300971201     C                   SETON                                        4890
052400931222     C                   GOTO      ENDCTR
052500931222     C                   ENDIF
052600971201     C                   ENDIF
052700980707     C* NON POSSO RIELABORARE SE NON E' PIU' IN L1
052800050829     c* controllo inutile: non posso mettere lnp se elaboro
052900050829     C**   VIDLNP        IFGT      0
053000050829     C**   VIDSTA        ANDEQ     'S'
053100050829     C**   WL1           ANDEQ     'N'
053200050829     C**                 SETON                                        4890
053300050829     C**                 GOTO      ENDCTR
053400050829     C**                 ENDIF
053500010524      *
053600010524      * STAMPA SOLO TOTALI X STATISTICA ENTRATE
053700010524     C     VIDTOT        IFEQ      'S'
053800010524     C     VIDTST        ANDNE     'E'
053900010601     C     VIDTST        ANDNE     ' '
054000010601     C                   SETON                                        54  90
054100010524     C                   GOTO      ENDCTR
054200010524     C                   ELSE
054300010524     C                   ENDIF
054400010524      *
054500900524     C     ENDCTR        ENDSR
054600931203      *****************************************************************
054700931203      *    CARICO TABELLE
054800931203      *****************************************************************
054900920930     C     CARTAB        BEGSR
055000931203      *
055100020417      * CARICO FILIALI GESTITE £1
055200050829     c**                 clear                   trul06ds
055300050829     c**                 eval      d06cod = '£1'
055400050829     c**                 movel     simfel        d06key
055500050829     c**                 movel     trul06ds      kpjbu
055600050829     c**                 call      'TRUL06R'
055700050829     c**                 parm                    kpjba
055800050829     c**                 movel     kpjbu         trul06ds
055900050829     c**                 movea     lin           l1
056000931203      *
056100931203      * VEDO SE SONO SIMFEL(1°LVL) O REMOTA (2°LVL)
056200931203      *SE LA RICHIEDENTE E' UN 1° LIVELLO, QUELLA DA STAMPARE PUO' ESSERE
056300931203      *SIA UN 1° CHE UN 2° LIVELLO. (MODIFICABILE DA VIDEO)
056400931203      *SE INVECE E' UN 2°LIVELLO, PUO' STAMPARE SOLO SE STESSA.(PROTECT A VIDEO)
056500931203      *
056600931203      * SE REMOTA (2°LVL), METTO SE STESSA NON MODIFICABILE (05=PR), PERCHE' IL
056700931203      * 2°LVL, PUO' VEDERE SOLO SE STESSA.ABILE (05=PR), PERCHE' IL
056800020513     c     simtpp        ifeq      '2'
056900020513     c     simtpp        oreq      *blanks
057000020513     c                   movel     simpou        ds1fgs
057100920930     C                   SETON                                        05
057200920930     C                   ELSE
057300931203      *
057400931203      * SE INVECE (1°LVL), METTO SIMFEL (SE STESSA), MA MODIFICABILE, PERCHE' IL
057500931203      * 1°LVL, PUO' VEDERE ANCHE LE 2°LVL.
057600950210     C                   MOVEL     SIMFEL        DS1FGS
057700920930     C                   END
057800000810     C                   MOVEL     DS1FGS        WPOSTE            3 0
057900000810     C****                 MOVELDS1FGS    VIDLNP
058000941010     C                   Z-ADD     *ZERO         VIDLNP
058100931203      *
058200000810      * CONTROLLO SE SONO POSTE (§OGPT=S)
058300000810      *
058400000810     C                   CLEAR                   §OGPT
058500000810     C     WPOSTE        CHAIN     AZORG01L                           57
058600000810     C  N57              MOVEL     ORGDE3        OG143
058700000912     C                   MOVEL     §OGPT         FGSPT             1
058800000810      *
058900931217     C     SIMFEL        COMP      *ZERO                                  06
059000021016
059100021016      * Recupero le date delle statistiche
059200021016     c                   Clear                   Tibs02Ds
059300021016     c                   Clear                   dSdf
059400021016     c                   Eval      T02Mod = 'C'
059500021016     c                   Eval      T02Sif = Knsif
059600021016     c                   Eval      T02Cod = 'SDF'
059700021016     c                   Movel(p)  Simfel        T02Ke1
059800021016     c                   Call      'TIBS02R'
059900021016     c                   Parm                    Kpjba
060000021016     c                   Parm                    Tibs02Ds
060100021016     c                   If        T02Err = *Blanks
060200021016     c                   Movel     T02Uni        dSdf
060300021016     c                   EndIf
060400021016
060500970213     c* PRELEVO LA DATA DA CUI ELABORARE LE NUOVE STATISTICHE DA TAB.7A
060600970213     C                   MOVEL     '7A'          COD
060700970213     C                   MOVEL(P)  '2'           KEY
060800970213     C     KTAB          CHAIN     TABEL00F                           30
060900970213     C     *IN30         IFEQ      *OFF
061000970213     C                   MOVEL     TBLUNI        DS7A2
061100970213     C     §7ADEL        IFEQ      *ZEROS
061200970213     C                   MOVE      *HIVAL        §7ADEL
061300970213     C                   END
061400970213     C                   ELSE
061500970213     C                   MOVE      *HIVAL        §7ADEL
061600970213     C                   END
061700931217      *
061800920930     C                   ENDSR
061900940224      **************************************************************************
062000940224      * Viene rilevato l'ultimo giorno di utilizzo del pgm che si
062100940224      * occupa della pulizia archivio (ogni file, o quasi, ha il proprio pgm);
062200970718      * nella tabella 5A  c'e' il nr.giorni di mantenimento dei dati; si
062300940224      * sottrae quest'ultimo dal giorno ultimo utilizzo per sapere se vi sono
062400940224      * ancora disponibili i dati per la statistica.
062500940224      **************************************************************************
062600940224     C     TESTF         BEGSR
062700940224      *
062800940224     C                   CLEAR                   PRIDAT
062900940224      *
063000940224      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA STATISTICHE ARRIVI
063100940224     C                   MOVE      *BLANKS       $DAT
063200940224     C                   MOVE      *BLANKS       $ERR
063300950111     C                   CALL      'TRUL49C'                            31
063400940224     C                   PARM                    $PGM             10
063500000103     C                   PARM                    $DAT              8
063600940224     C                   PARM                    $ERR              1
063700940224      *
063800941010      * PROGRAMMA IN USO DATA ULTIMO ULIZZO = DATA DEL GIORNO
063900040921      * idem se data ultimo utilizzo = 0
064000970306    1C     $ERR          IFEQ      '1'
064100950111     C     *IN31         OREQ      '1'
064200040921     c     $dat          oreq      '00000000'
064300970306     C                   MOVE      WDATEU        G02INV
064400970306   X1C                   ELSE
064500040921    2C***  $DAT          IFEQ      *BLANKS
064600950327     C                   CLEAR                   G02INV
064700040921   X2C****               ELSE
064800940224     C                   MOVE      $DAT          G02INV
064900040921    2C****               ENDIF
065000970306    1C                   ENDIF
065100940224      *
065200940224      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
065300940224      * PULIZIA E TROVO LA PRIMA DATA AL DI SOTTO DELLA QUALE NON SI
065400940224      * POSSONO LANCIARE ELABORAZIONI PER MANCANZA DI DATI
065500940224      *
065600940224      * SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
065700040921     C***  G02INV        IFNE      *ZEROS
065800940224     C                   Z-ADD     *ZEROS        G02DAT
065900940224     C                   Z-ADD     *ZEROS        G02TGI
066000940224     C                   MOVEL     '3'           G02ERR
066100941010     C                   CALL      'XSRDA8'
066200940224     C                   PARM                    WLBDAT
066300940224      *
066400940224     C                   SUB       $DATPU        G02TGI
066500940224     C                   Z-ADD     *ZEROS        GIODAT
066600940224     C                   Z-ADD     *ZEROS        GIOINV
066700940224     C                   Z-ADD     G02TGI        GIOTGI
066800941010     C                   CALL      'XSRGI8'
066900940224     C                   PARM                    WGIDAT
067000940224      *
067100941010     C                   Z-ADD     GIOINV        PRIDAT            8 0
067200040921     C*****              ENDIF
067300940224      *
067400940224      * DATA DAL MAGGIORE DATA CALCOLATA
067500940224     C     COMDSD        IFLE      PRIDAT
067600940224     C                   SETON                                        5090
067700940224     C                   ENDIF
067800041210     c* se sono edp e sono il ambiente di prova non do errore
067900041210     c                   movel     knmus         winizio           3
068000041210     c                   if        winizio='EDP' and knsif='FILTRAPFP'
068100041210     C                   SETOFF                                       5090
068200041210     C                   ENDIF
068300940224      *
068400940224     C                   ENDSR
068500970213**
068600970213Impossibile elaborare la stat.Partito per un periodo a cavallo del xx/xx/xxxx
