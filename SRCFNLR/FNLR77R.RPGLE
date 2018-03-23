000100040527     H DECEDIT('0,') DATEDIT(*YMD.)
000200950123     H* FNLR77R *-----------------------------------------------------*
000300930914     H*                - STATISTICA CONSEGNE -         FILTRO         *
000400930914     H*---------------------------------------------------------------*
000500970123     FTABEL00F  IF A E           K DISK
000600980525     FAZCLN01L  IF   E           K DISK
000700040527     FAZorg01L  IF   E           K DISK
000800950123     FFNLR77D   CF   E             WORKSTN
000900930914     D L1              S              3  0 DIM(30)
001000970123     D DPE             S              8  0 DIM(30)
001100970123     D DUE             S              8  0 DIM(30)
001200970123     D*
001300070903     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG VIDEO
001400070903     D $pog            s              3    dim(250)
001500070903     D $DIG            s              1    dim(20)
001600070903     D $ARG            s              3    dim(50)
001700070903
001800980525     D*
001900980525     D K31             S              1    DIM(31)                              SKI DI COMODO
002000920309     D WLBDAT          DS
002100950123     D  G02DAT                 1      8  0
002200950123     D  G02INV                 9     16  0
002300950123     D  G02ERR                17     17
002400950123     D  G02TGI                18     22  0
002500930914     D WGIDAT          DS
002600950123     D  GIODAT                 1      8  0
002700950123     D  GIOINV                 9     16  0
002800950123     D  GIOTGI                17     21  0
002900980525     D                 DS
003000980525     D  WAA                    1      4  0
003100980525     D  WMM                    5      6  0
003200980525     D  WGG                    7      8  0
003300980525     D  WDATA                  1      8  0
003400900511     D KPJBA         E DS
003500070903     D TIBS34DS      E DS
003600070903     D TRUL31DS      E DS
003700070903     D TRUL31DS2     E DS
003800070903     D TIBS02DS      E DS
003900950313     D* DS PER TRUL06R - CARICAMENTO £X
004000950313     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004100950313     D  LIN                    1     90  0
004200950313     D                                     DIM(30)                              FIL. COMODO
004300070903     D*** CNCR80        E DS
004400930914     D DS5A          E DS
004500040922     D*DS5A2         E DS
004600040922     D DS5AS1        E DS
004700970123     D DS7AF         E DS
004800970123     D  ALFDPE                68     75
004900970123     D  ALFDUE                76     83
005000070903     D**** UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005100070903     d Azuteds       e ds                  extname(Azute00f)
005200070903     d dDatiute      e ds
005300070903     d dLat          e ds
005400070903     d dute01        e ds
005401100222
005402100222     d dvpoDECO      e ds
005500970207     D* PASSAGGIO DATI A PGM BATCH
005600970207     D DSLS01        E DS
005700040527     D fnlv55ds      E DS
005900050329     D og148         E DS
006100050331     D og143         E DS
006200070903     d
006300070903     d wabi            s                   like(UteAut)
006400090918     d wforza          s              1
006500090918     d wforzaR         s              1
006501120308     d predsd          s                   like(comdsd)
006600930914     C*---------------------------------------------------------------*
006700930914     C* INDICATORI USATI                                              *
006800930914     C*---------------------------------------------------------------*
006900060111     C* 01 - p.o. di primo   livello
007000060111     C* 30 - DI COMODO RIUTILIZZABILE
007100930914     C* 44 - ELABORAZIONE MOMENTANEAMENTE NON EFFETTUABILE
007200930914     C* 45 - ELABORAZIONE NON EFFETTUABILE PER MANCANZA DI DATI
007300980525     C* 46/53 - ERRORE
007400930914     C* 90 - GENERICO DI ERRORE
007500930914     C*---------------------------------------------------------------*
007600930914     C*
007700930914     C* DEFINIZIONE VARIABILI, KLIST E PLIST
007800930914     C                   EXSR      DEFVAR
007900930914     C*
008000950313     C* CARICAMENTO TABELLE
008100920309     C                   EXSR      CARTAB
008200900523     C*
008300920309     C                   Z-ADD     0             VIDDSD
008400920309     C                   Z-ADD     0             VIDDSA
008500920309     C                   Z-ADD     ELAFIL        VIDLNA
008600970325     C                   MOVEL     'L'           VIDSNC
008700970123     C                   MOVEL     'S'           VIDELA
008800050718     C                   MOVEL     ' '           VIDEsa
008801100222     C   02              MOVEL     'S'           VIDEAT
008900090918     c                   clear                   wforza
009000090918     c                   clear                   wforzaR
009001120308     c                   clear                   predsd
009500040527     C                   EVAL      VIDSPP='S'
010200920803     C*
010300900509     C     FOR01         TAG
010400950123     C                   EXFMT     LR77D01
010500920803     C*
010600950123     C* F3 = FINE
010700900509     C   KC              GOTO      FINE
010800930914     C*
010900930914     C* CONTROLLI
011000900509     C                   EXSR      CONTR
011100950123     C*
011200950123     C   90
011300950123     CORNKF              GOTO      FOR01
011400920903     C*
011500950123     C* F6 = ELABORAZIONE
011600920803     C*
011700970130     C     COMDSA        IFEQ      *ZERO
011800970130     C                   Z-ADD     VIDDSD        VIDDSA                         GG/MM/AA
011900970130     C                   Z-ADD     COMDSD        COMDSA                         AA/MM/GG
012000970130     C                   ENDIF
012100970130     C*
012200070903     c* Se vidlna non in £1, carico il suo simfel (ovvero terminal di
012300070903     c*  partenza alla data richiesta) e pulisco la £1 impostando
012400070903     c*  solo questo p.o.
012500070903     c                   if        vidlna>0
012600070903     c     vidlna        lookup    l1                                     31
012700070903     c                   if        not *in31
012800070903     c                   clear                   l1
012900070903     c                   movel     vidlna        l1(1)
013000070903     C                   CLEAR                   FNLV55DS
013100070903     C                   MOVEL     'P'           D55TPT
013200070903     C                   MOVE      vidlna        D55LIN
013300070903     C                   MOVE      comdsd        D55DRF
013400070903     C                   MOVEL     'L'           D55TLA
013500070903     C                   CALL      'FNLV55R'
013600070903     c                   parm                    fnlv55ds
013700070903     c                   movel     d55tfp        simfel
013800070903     c                   endif
013900070903     c                   endif
014000070903     c
014100970123     C                   Z-ADD     1             X
014200970123    1C     L1(X)         DOWGT     0
014300970123    2C     VIDLNA        IFEQ      0
014400970123     C     VIDLNA        OREQ      L1(X)
014500050329     C* ESCLUDO p.o. non operativi e europolitan se non richiesti in modo
014600050329     c*  specifico
014700050329    3c                   if        vidlna=0
014800050329     c     l1(x)         chain     azorg01l
014900050329    4c                   if        not %found(azorg01l)
015000050329     c                   clear                   og148
015100050331     c                   clear                   og143
015200050329    4c                   endif
015300050329     c                   movel     orgde8        og148
015400050331     c                   movel     orgde3        og143
015500050329    3c                   endif
015600050329     c
015700050331    3c                   select
015800050331     c                   when      vidlna=0 and §ogntw='EUP'
015900050331     c                   when      vidlna=0 and
016000050331     c                             §oglpo=' ' and §ogNTW<>'DPD' and §ogNTW<>
016100050331     c                              'FED' and §ogNTW<>'EEX'
016200050331   x3c                   other
016300050331     c
016400970130      * Se la data fine periodo e' nulla la imposto come quella iniziale
016500970130     C                   CLEAR                   DSLS01
016600970130     C                   MOVEL     'A'           DS1TBO                         'P'=PARTENZE
016700040527     C                   MOVEL     'FNLR78R'     DS1PGM
016800970207     C* TIPO DI STATISTICA ARRIVI: "C" CONSEGNE
016900970207     C                   MOVEL     'C'           DS1FPG
017000970207     C*
017100970130     C                   Z-ADD     SIMFEL        DS1FEL                                     ORE
017200970207     C                   Z-ADD     L1(X)         DS1FGS                                     ORE
017300970130     C                   Z-ADD     L1(X)         DS1LNP                                     ZA
017400970225     C                   Z-ADD     L1(X)         DS1LNA                                     ZA
017500970130     C                   Z-ADD     VIDDSD        DS1GDA                                     AA
017600970130     C                   Z-ADD     VIDDSA        DS1GAL                                     A
017700970130     C                   Z-ADD     COMDSD        DS1ADA                                     GG
017800970130     C                   Z-ADD     COMDSA        DS1AAL                                     GG
017900970130     C                   MOVEL     VIDELA        DS1RIS                         RIST/ELAB
018000970207     C                   MOVEL     VIDSNC        DS1FL1
018100040608     C                   MOVEL     VIDSPP        DS1FL5
018200050706     C                   MOVEL     VIDesa        DS1FL6
018201100218     C                   MOVEL     VIDeAT        DS1FL7
018300970307     C* E' SEMPRE RIELABORAZIONE SE CHIESTA SOLO LA LISTA,
018400970307     C*  ANCHE SE HANNO MESSO R
018500970307     C     VIDSNC        IFNE      'S'
018600970307     C                   MOVEL     'S'           DS1RIS
018700970307     C                   ENDIF
018800970130      *
018900970130      * Sottometto il pgm che si occupera' del lancio effettivo delle statist.
019000970130     C                   CLEAR                   KPJBU
019100970130     C                   MOVEL     DSLS01        KPJBU
019200970403     C* SE HO RICHIESTO SOLO LA LISTA SOTTOMETTO DIRETTAMENTE LR78
019300970403     C     DS1FL1        IFNE      'L'
019400970130     C                   CALL      'FNLSC8R'
019500970130     C                   PARM                    KPJBA
019600970403     C                   ELSE
019700970403     C                   MOVEL     'LR78'        KCOAZ
019800970403     C                   CALL      'BCH10'
019900970403     C                   PARM                    KPJBA
020000970207     C                   ENDIF
020100970403     C*
020200050331    3C                   ENDsl
020300050329    2C                   ENDIF
020400970207     C                   ADD       1             X
020500050329    1C                   ENDDO
020600900522     C*
020700000000     C     FINE          TAG
020800000000     C                   SETON                                        LR
020900930914     C*---------------------------------------------------------------*
021000930914     C*                    CONTR                                      *
021100930914     C* ROUTINE DI CONTROLLO DATI FORMATO VIDEO                       *
021200930914     C*---------------------------------------------------------------*
021300900509     C     CONTR         BEGSR
021400930914     C*
021500100222     C                   SETOFF                                       90
021600970317     C                   Z-ADD     0             COMDSA
021700970317     C                   Z-ADD     0             COMDSD
021800930914     C*
021900950313     C*  SE SONO SIMFEL POSSO RICHIEDERE TUTTE ALTRIMENTI SOLO QUELLE
022000970123     C*  PRESENTI IN £6 CHE HO COMUNQUE CARICATO NELLA SKIERA L1
022100930914IF  1C     VIDLNA        IFNE      0
022200920309     C     VIDLNA        LOOKUP    L1                                     31
022300070903     c* Se non presente, verifico se utente abilitato al P.O.
022400070903     c                   if        not *in31
022500070903     c                   movel     vidlna        w003a             3
022600070903     c     w003a         lookup    $pog                                   31
022700070903     c                   endif
022800070903     c
022900920309     C  N31              SETON                                        5190
023000920309     C  N31              GOTO      ENDCTR
023100930914E   1C                   ENDIF
023200920309     C*
023300920902     C* DATA CONSEGNA DAL OBBLIGATORIA
023400920309     C                   MOVE      VIDDSD        G02DAT
023500920309     C                   MOVEL     *BLANK        G02ERR
023600950123     C                   CALL      'XSRDA8'
023700920309     C                   PARM                    WLBDAT
023800930914IF  1C     G02ERR        IFEQ      '1'
023900920309     C                   SETON                                        40  90
024000920309     C                   GOTO      ENDCTR
024100930914E   1C                   ENDIF
024200950123     C                   Z-ADD     G02DAT        VIDDSD
024300970123     C                   Z-ADD     G02INV        COMDSD            8 0
024400970123     C                   Z-ADD     G02TGI        DALTGI
024500970123     C     *LIKE         DEFINE    G02TGI        DALTGI
024600970124     C     *LIKE         DEFINE    G02TGI        WTGI
024700970307     C**
024800970307     C* SE 'LISTA O ENTRAMBE E' SEMPRE 'S'
024900970307     C     VIDSNC        IFEQ      'L'
025000970307     C     VIDSNC        OREQ      'E'
025100970307     C                   MOVEL     'S'           VIDELA
025200970307     C                   ENDIF
025300900522     C*
025400980525     C* SE NON E' INSERITA LA DATA AL CONTROLLO CHE LA DATA DAL SIA MINORE O
025500980525     C* UGUALE ALLA DATA MASSIMA RICHIESTA (OGGI + 1 GIORNO NON FESTIVO)
025600980525     C     VIDDSA        IFEQ      0
025700980525     C     COMDSD        ANDGT     DATAPA
025800980525     C                   SETON                                        52  90
025900980525     C                   GOTO      ENDCTR
026000980525     C                   ENDIF
026100920902     C* DATA AL
026200930914IF  1C     VIDDSA        IFNE      0
026300970130     C*
026400920309     C                   MOVE      VIDDSA        G02DAT
026500920309     C                   MOVEL     *BLANK        G02ERR
026600950123     C                   CALL      'XSRDA8'
026700920309     C                   PARM                    WLBDAT
026800930914IF  2C     G02ERR        IFEQ      '1'
026900920309     C                   SETON                                        41  90
027000920309     C                   GOTO      ENDCTR
027100930914E   2C                   ENDIF
027200950123     C                   Z-ADD     G02DAT        VIDDSA
027300970123     C                   Z-ADD     G02INV        COMDSA            8 0
027400920309     C*
027500900523     C* DATA DAL NO MAGGIORE DATA AL
027600930914IF  2C     COMDSD        IFGT      COMDSA
027700950123     C                   SETON                                        4290
027800900523     C                   GOTO      ENDCTR
027900930914E   2C                   ENDIF
028000970130     C* SE RICHIESTA SOLO LA LISTA SPEDIZIONI NON IN CONSEGNA LA DATA
028100970130     C* AL DEVE ESSERE = 0
028200970130     C     VIDSNC        IFEQ      'L'
028300970130     C     COMDSA        ANDNE     COMDSD
028400970130     C                   SETON                                        4690
028500970130     C                   GOTO      ENDCTR
028600970130    2C                   ENDIF
028700970130     C*
028800970130     C* SE RICHIESTA LA RIELABORAZIONE, NON AMMESSA
028900970130     C     VIDELA        IFEQ      'S'
029000970130     C     COMDSA        ANDNE     COMDSD
029100970130     C                   SETON                                        5090
029200970130     C                   GOTO      ENDCTR
029300970130    2C                   ENDIF
029400980525     C* CONTROLLO CHE LA DATA AL SIA MINORE O UGUALE
029500980525     C* ALLA DATA MASSIMA RICHIESTA (OGGI + 1 GIORNO NON FESTIVO)
029600980525     C     COMDSA        IFGT      DATAPA
029700980525     C                   SETON                                        5390
029800980525     C                   GOTO      ENDCTR
029900980525     C                   ENDIF
030000970130E   1C                   ENDIF
030100970123     C* SE RICHIEDO L'ELABORAZIONE DATI POSSO RICHIEDERE ANCHE LA
030200970123     C*  STAMPA DELLA LISTA SPEDIZIONI NON IN CONSEGNA
030300970307     C***        VIDELA    IFEQ 'R'
030400970307     C***        VIDSNC    IFEQ 'L'
030500970307     C***        VIDSNC    OREQ 'E'
030600970307     C***                  SETON                     4890
030700970307     C***                  GOTO ENDCTR
030800970307E   2C***                  ENDIF
030900970307E   1C***                  ENDIF
031000970124     C* SE DATA DAL AL IMMETTERE SOLO "S" PER AVERE SOLO LA STATISTICA
031100970207    1C     VIDDSA        IFGT      0
031200970124     C     VIDDSA        ANDNE     VIDDSD
031300970124     C     VIDSNC        ANDEQ     'E'
031400970124     C                   SETON                                        4690
031500970124     C                   GOTO      ENDCTR
031600970207E   1C                   ENDIF
031700970207     C*
031800970130     C* IN SEDE POSSO RICHIEDERE SOLTANTO LA RISTAMPA
031900970207    1C     SIMFEL        IFEQ      0
032000970130     C     VIDELA        ANDEQ     'S'
032100970130     C                   SETON                                        5190
032200970130     C                   GOTO      ENDCTR
032300970207    1C                   ENDIF
032301120308     c* a cambio data dal pulisco flag di forzatura che pilota messaggio forzabile per EDP
032302120308     c                   if        comdsd<>predsd
032303120308     c                   clear                   wforza
032304120308     c                   clear                   wforzar
032305120308     c                   eval      predsd=comdsd
032306120308     c                   endif
032400930914     C*
032500930914     C* REPERISCO DATA ULTIMO UTILIZZO PROGRAMMA DI PULIZIA BOLLE
032600970207    1C     VIDELA        IFEQ      'S'
032700030114     C                   MOVEL     'FNLR84R'     wPGM
032800930914     C                   MOVEL     *BLANKS       DAT
032900930914     C                   MOVEL     *BLANKS       ERR
033000930914     C*
033100950123     C                   CALL      'TRUL49C'                            30
033200030114     C                   PARM                    wPGM             10
033300000103     C                   PARM                    DAT               8
033400930914     C                   PARM                    ERR               1
033500930914     C*
033600030114     C* PROGRAMMA IN USO o DATA ULTIMO UTILIZZO A ZERO UTILIZZO = UDATE
033700030114IF  2C                   IF        ERR = '1'  or DAT = '00000000'
033800940125     C                   Z-ADD     DATEU         G02INV
033900970207X   2C                   ELSE
034000950203     C                   CLEAR                   G02INV
034100940125     C                   MOVE      DAT           G02INV
034200970207    2C                   ENDIF
034300970207     C*
034400930914     C* SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
034500930914     C* PULIZIA E TROVO LA DATA AL DI SOTTO DELLA QUALE NON SI POSSONO
034600930914     C* LANCIARE ELABORAZIONI PER MANCANZA DI DATI
034700930914     C*
034800930914     C* SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
034900020218IF  2C**!!!      G02INV    IFEQ *ZEROS
035000020218     C**!!!                Z-ADD*ZEROS    GIOINV
035100020218X   2C**!!!                ELSE
035200930914     C                   Z-ADD     *ZEROS        G02DAT
035300930914     C                   Z-ADD     *ZEROS        G02TGI
035400030114     C                   MOVEL     '3'           G02err
035500950123     C                   CALL      'XSRDA8'
035600930914     C                   PARM                    WLBDAT
035700930914     C*
035800930914     C*
035900930914     C                   SUB       §5AARB        G02TGI
036000930914     C*
036100930914     C                   Z-ADD     *ZEROS        GIODAT
036200930914     C                   Z-ADD     *ZEROS        GIOINV
036300930914     C                   Z-ADD     G02TGI        GIOTGI
036400930914     C*
036500950123     C                   CALL      'XSRGI8'
036600930914     C                   PARM                    WGIDAT
036700020218E   2C**!!!                ENDIF
036800930914     C*
036900930914     C* DATA DAL MAGGIORE DATA CALCOLATA
037000090918     c* Per EDP errore forzabile
037100090918     c
037200090918    2c                   if        wforza=' ' or wforza='S' and
037300090918     c                             %subst(knmus:1:3)<>'EDP'
037400090918IF  3C     COMDSD        IFLE      GIOINV
037500940715     C                   SETON                                        4590
037600090918     c                   eval      wforza='S'
037700930914     C                   GOTO      ENDCTR
037800090918E   3C                   ENDIF
037900090918E   2C                   ENDIF
038000970207     C*
038100970207   X1C                   ELSE
038200970207     C**
038300030114     C                   MOVEL     'FNLRE4R'     wPGM                           RISTAMPA
038400970207     C                   MOVEL     *BLANKS       DAT
038500970207     C                   MOVEL     *BLANKS       ERR
038600970207     C*
038700970207     C                   CALL      'TRUL49C'                            30
038800030114     C                   PARM                    wPGM
038900000103     C                   PARM                    DAT               8
039000970207     C                   PARM                    ERR               1
039100970207     C*
039200030114     C* PROGRAMMA IN USO o DATA ULTIMO UTILIZZO A ZERO UTILIZZO = UDATE
039300030114IF  2C                   IF        ERR = '1'  or DAT = '00000000'
039400970207     C                   Z-ADD     DATEU         G02INV
039500970207X   2C                   ELSE
039600970207     C                   CLEAR                   G02INV
039700970207     C                   MOVE      DAT           G02INV
039800970207    2C                   ENDIF
039900970207     C*
040000970207     C* SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
040100970207     C* PULIZIA E TROVO LA DATA AL DI SOTTO DELLA QUALE NON SI POSSONO
040200970207     C* LANCIARE ELABORAZIONI PER MANCANZA DI DATI
040300970207     C*
040400970207     C* SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
040500020218IF  2C**!!!      G02INV    IFEQ *ZEROS
040600020218     C**!!!                Z-ADD*ZEROS    GIOINV
040700020218X   2C**!!!                ELSE
040800970207     C                   Z-ADD     *ZEROS        G02DAT
040900970207     C                   Z-ADD     *ZEROS        G02TGI
041000970207     C                   MOVEL     '3'           G02ERR
041100970207     C                   CALL      'XSRDA8'
041200970207     C                   PARM                    WLBDAT
041300970207     C*
041400970207     C* PULIZIA STASTISTICA ARRIVI
041500040922     C***                SUB       §5ASTA        G02TGI
041600040922     C                   SUB       §5ASTS        G02TGI
041700970207     C*
041800970207     C                   Z-ADD     *ZEROS        GIODAT
041900970207     C                   Z-ADD     *ZEROS        GIOINV
042000970207     C                   Z-ADD     G02TGI        GIOTGI
042100970207     C*
042200970207     C                   CALL      'XSRGI8'
042300970207     C                   PARM                    WGIDAT
042400020218E   2C**!!!                ENDIF
042500970207     C*
042600970207     C* DATA DAL MAGGIORE DATA CALCOLATA
042700970207IF  2C     COMDSD        IFLE      GIOINV
042800090918    3c                   if        wforzaR=' ' or wforzaR='S' and
042900090918     c                             %subst(knmus:1:3)<>'EDP'
043000970207     C                   SETON                                        4590
043100090918     c                   eval      wforzaR='S'
043200970207     C                   GOTO      ENDCTR
043300090918E   3C                   ENDIF
043400090918     c
043500090918E   2C                   ENDIF
043600970207    1C                   ENDIF
043700920803     C*
043800900509     C     ENDCTR        ENDSR
043900970123     C*---------------------------------------------------------------*
044000970123     C*                    CRTTAB                                     *
044100970123     C* CREO RECORD IN TABELLA DS7A
044200970123     C*---------------------------------------------------------------*
044300970123     C     CRTTAB        BEGSR
044400970123     C                   CLEAR                   DS7AF
044500970123     C                   CLEAR                   TABEL
044600970123     C                   MOVEL     *ZEROS        ALFDUE
044700970123     C                   MOVEL     *ZEROS        ALFDPE
044800970123     C                   MOVEL     DS7AF         TBLUNI
044900970123     C                   MOVEL     L1(X)         TBLKEY
045000970123     C                   MOVEL     '7A'          TBLCOD
045100970123     C                   MOVEL     '1'           TBLKUT
045200970123     C                   WRITE     TABEL
045300970123     C                   ENDSR
045400930914     C*---------------------------------------------------------------*
045500930914     C*                    CARTAB                                     *
045600950313     C* ROUTINE DI CARICAMENTO TABELLE £1 E £6                        *
045700930914     C*---------------------------------------------------------------*
045800930914     C     CARTAB        BEGSR
045900040527     C*
046000930914     C*
046100950313     C* SE FILIALE DI 2° LIVELLO CARICO £6
046200070903     C     dutlpo        IFEQ      '2'
046300070903     C     dutlpo        OREQ      *BLANKS
046400950313     C*
046500020429     C***                  MOVE KFIL      ELAFIL
046600070903     C                   Z-ADD     dutpou        ELAFIL
046700950313     C*
046800950313     C                   CLEAR                   DSUL06
046900950313     C                   MOVE      '£6'          D06COD
047000950313     C                   MOVEL     ELAFIL        D06KEY
047100950313     C                   MOVEL     'L'           D06TLA
047200950313     C                   MOVEL     DSUL06        KPJBU
047300950313     C                   CALL      'TRUL06R'
047400950313     C                   PARM                    KPJBA
047500950313     C                   MOVEL     KPJBU         DSUL06
047600970123     C                   MOVEA     LIN           L1
047700950313     C                   ELSE
047800950313     C* SE 1° LIVELLO CARICO £1
047900950313     C*
048000950313     C                   MOVE      SIMFEL        ELAFIL
048100060111     c                   seton                                        01
048200950313     C*
048300950313     C                   CLEAR                   DSUL06
048400950313     C                   MOVE      '£1'          D06COD
048500950313     C                   MOVEL     SIMFEL        D06KEY
048600950313     C                   MOVEL     'L'           D06TLA
048700950313     C                   MOVEL     DSUL06        KPJBU
048800950313     C                   CALL      'TRUL06R'
048900950313     C                   PARM                    KPJBA
049000950313     C                   MOVEL     KPJBU         DSUL06
049100950313     C                   MOVEA     LIN           L1
049200950313     C                   END
049201120124     c*
053000970123     C                   Z-ADD     1             X                 2 0
053100970123    1C     L1(X)         DOWGT     0
053200970123     C                   MOVEL     '7A'          COD
053300970123     C                   MOVEL(P)  L1(X)         KEY
053400970123     C     KTAB          CHAIN     TABEL                              31
053500970123     C*
053600970123    3C     *IN31         IFEQ      *ON
053700970123     C                   EXSR      CRTTAB
053800970123    3C                   ENDIF
053900970123     C*
054000970123     C                   MOVEL     TBLUNI        DS7AF
054100970123     C*
054200970123    3C     ALFDPE        IFLT      *ZEROS
054300970123     C                   MOVEL     *ZEROS        ALFDPE
054400970123    3C                   ENDIF
054500970123    3C     ALFDUE        IFLT      *ZEROS
054600970123     C                   MOVEL     *ZEROS        ALFDUE
054700970123    3C                   ENDIF
054800970123     C                   Z-ADD     §7ACPE        DPE(X)
054900970123     C                   Z-ADD     §7ACUE        DUE(X)
055000970123     C                   ADD       1             X
055100970123     C                   ENDDO
055200970207     C**
055300970207     C                   MOVEL     '5A'          COD
055400970207     C                   MOVEL     *BLANKS       KEY
055500970207     C                   MOVEL     '1'           KEY
055600970207     C     KTAB          CHAIN     TABEL00F                           30
055700970207IF  3C     *IN30         IFEQ      *ON
055800970207     C                   CLEAR                   DS5A
055900970207X   3C                   ELSE
056000970207     C                   MOVEL     TBLUNI        DS5A
056100970207E   3C                   ENDIF
056200970207     C*
056300970718     C                   MOVEL     '5A'          COD
056400040922     C****               MOVEL(P)  '2'           KEY
056500040922     C                   MOVEL(P)  'SEDE1'       KEY
056600970207     C     KTAB          CHAIN     TABEL00F                           30
056700970207IF  3C     *IN30         IFEQ      *ON
056800040922     C***                CLEAR                   DS5A2
056900040922     C                   CLEAR                   DS5As1
057000970207X   3C                   ELSE
057100040922     C***                MOVEL     TBLUNI        DS5A2
057200040922     C                   MOVEL     TBLUNI        DS5As1
057300970207E   3C                   ENDIF
057400930914     C*
057500930914     C                   ENDSR
057600930914     C*---------------------------------------------------------------*
057700930914     C*                    DEFVAR                                     *
057800930914     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
057900930914     C*---------------------------------------------------------------*
058000930914     C     DEFVAR        BEGSR
058100930914     C*
058200930914     C     *ENTRY        PLIST
058300930914     C                   PARM                    KPJBA
058400070903      *
058500070903     c     *dtaara       define    §azute        azuteds
058600070903     c     *dtaara       define    §datiute      ddatiute
058700070903     c                   in(E)     *dtaara
058800070903    1c                   If        %error  or RSUT = *blanks
058900070903     c                   Clear                   Tibs34ds
059000070903     c                   Call      'TIBS34R'
059100070903     c                   Parm                    Tibs34ds
059200070903     c                   In        *dtaara
059300070903    1c                   EndIf
059400070903
059500070903     c                   Clear                   wabi
059600070903     c                   Clear                   v1cmsg
059700070903     c                   Clear                   dLat
059800070903
059900070903      * Verifica errori e autorità profilo
060000070903s   1c                   Select
060100070903      * se ho errori nei dati utente esco dal pgm
060200070903w   1c                   When      DutErr = 'E'
060300070903     c                   GoTo      Fine
060400070903      * se non c'è l'abilitazione
060500070903      * --> se 1° livello, abilitazioni al terminal
060600070903      *     se 2° livello, abilitazioni al punto operativo
060700070903      *     se sede è impossibile ma se capita mando a fine il pgm
060800070903w   1c                   When      UteAut = *Blanks
060900070903if  2c                   If        DutLpo = '1'
061000070903     c                   Eval      wabi   = 'TP'
061100070903e   2c                   EndIf
061200070903if  2c                   If        DutLpo = '2'
061300070903     c                   Eval      wabi   = 'PO'
061400070903e   2c                   EndIf
061500070903if  2c                   If        DutLpo = 'S'
061600070903     c                   GoTo      Fine
061700070903e   2c                   EndIf
061800070903      * carica le abilitazioni del profilo
061900070903x   1c                   Other
062000070903     c                   Movel     UteFaf        Dute01
062100070903     c                   Eval      wabi = UteAut
062200070903e   1c                   EndSl
062300070903
062400070903      * controllo se ok l'abilitazione dell'utente
062500070903     c                   Clear                   Tibs02ds
062600070903     c                   Eval      T02Mod = 'C'
062700070903     c                   Eval      T02Sif = knsif
062800070903     c                   Eval      T02Cod = 'LAT'
062900070903     c                   Movel(p)  wabi          T02Ke1
063000070903     c                   Call      'TIBS02R'
063100070903     c                   Parm                    kpjba
063200070903     c                   Parm                    Tibs02ds
063300070903if  1c                   If        T02Err = *Blanks
063400070903     c                   Eval      dLat = T02Uni
063500070903e   1c                   EndIf
063600070903      * errore o non abilitato
063700070903if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
063800070903     c                   Seton                                        28
063900070903     C                   movel     msg(1)        v1cmsg
064000070903   x1c                   Else
064100070903      * Reperimento dei P.O. gestibili dall'utente
064200070903     c                   clear                   TRUL31DS
064300070903     c                   clear                   TRUL31DS2
064400070903     c                   eval      I31abi = wabi
064500070903     c                   eval      I31cdi = DUTdis
064600070903     c                   eval      I31car = DUTare
064700070903     c                   eval      I31cpo = DUTpou
064800070903     c                   call      'TRUL31R'
064900070903     c                   parm                    KPJBA
065000070903     c                   parm                    TRUL31DS
065100070903     c                   parm                    TRUL31DS2
065200070903if  2c                   if        O31pog > *zeros
065300070903     c                   movea     O31pog        $pog
065400070903     c                   movea     O31arg        $arg
065500070903     c                   movea     O31dig        $dig
065600070903x   2c                   else
065700070903     c                   Seton                                        28
065800070903     c                   movel     MSG(1)        v1cmsg
065900070903e   2c                   endif
066000070903    1c                   endif
066100070903     c
066200930914     C*
066300930914     C                   Z-ADD     1             CODUT
066400070903     C***                CALL      'X§PARUT'
066500070903     C***                PARM                    UT§DSE
066600070903     C***                MOVEL     RAGUT         RSUT
066700070903     C***                MOVEL     REC80         CNCR80
066800040527     C* GIRO UDATE
066900040527     C                   TIME                    WTIME            14 0          ORA & DATA
067000040527     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
067100040527     C                   Z-ADD     WDATE         G02DAT
067200040527     C                   Z-ADD     *ZERO         G02INV
067300040527     C                   MOVEL     *BLANKS       G02ERR
067400040527     C                   CALL      'XSRDA8'
067500040527     C                   PARM                    WLBDAT
067600040527     C                   Z-ADD     G02INV        DATEU             8 0          AAAA/MM/GG
067700930914     C*
067800980525     C* MI CALCOLO LA DATA MASSIMA DI RICHIESTA STAMPA CHE E' OGGI + IL
067900980525     C* PRIMO GIORNO NON FESTIVO
068000040527     C                   Z-ADD     g02inv        DATAPA            8 0
068100980525     C* ADDIZIONO 1 FINTANTO CHE NON TROVO IL PRIMO GIORNO LAVORATIVO
068200980525     C*
068300980525     C                   MOVEL     'F'           WFEST             1
068400980525     C     G02TGI        ADD       1             GIOTGI
068500980525     C                   ADD       1             G02TGI
068600980525     C*
068700980525    1C     WFEST         DOWNE     ' '
068800980525     C     G02TGI        ADD       1             GIOTGI
068900980525     C                   ADD       1             G02TGI
069000980525     C                   CALL      'XSRGI8'
069100980525     C                   PARM                    WGIDAT
069200980525     C*
069300980525     C                   Z-ADD     GIOINV        DATAPA            8 0
069400980525     C*
069500980525     C* IMPOSTO ANNO E MESE DELLA DATA TROVATA
069600980525     C                   Z-ADD     GIOINV        WDATA
069700980525     C                   CLEAR                   W0030
069800980525     C* CHAIN SUL CALENDARIO BARTOLINI
069900980525     C     KCLN          CHAIN     AZCLN01L                           30
070000980525     C   30              CLEAR                   CLNPOM
070100980525     C                   MOVEA     CLNPOM        K31
070200980525     C                   MOVEL     K31(WGG)      WFEST
070300980525    1C                   ENDDO
070301100222
070302100222      * Aggancio tabella VPO record DECO
070303100222     c                   Clear                   Dvpodeco
070304100222     c                   Clear                   Tibs02ds
070305100222     c                   Eval      T02Mod = 'C'
070306100222     c                   Eval      T02Sif = knsif
070307100222     c                   Eval      T02Cod = 'VPO'
070308100222     c                   Movel(p)  'DECO'        T02Ke1
070309100222     c                   Call      'TIBS02R'
070310100222     c                   Parm                    Kpjba
070311100222     c                   Parm                    Tibs02ds
070312100222     c                   If        T02Err = *Blanks
070313100222     c                   Movel     T02Uni        Dvpodeco
070314100222     c                   EndIf
070315100222     c
070316100222     c                   if        dateu>=§vpogma
070317100222     c                   seton                                        02
070318100222     c                   endif
070319100222
070400980525     C*
070500930914     C     KTAB          KLIST
070600070903     C                   KFLD                    CODUT             1 0
070700950123     C                   KFLD                    COD
070800950123     C                   KFLD                    KEY
070900980525     C* ACCESSO   AZCLN
071000980525     C     KCLN          KLIST
071100980525     C                   KFLD                    SIMFEL
071200980525     C                   KFLD                    W0030             3 0
071300980525     C                   KFLD                    WAA               4 0
071400980525     C                   KFLD                    WMM               2 0
071500930914     C*
071600950123     C     *LIKE         DEFINE    TBLCOD        COD
071700950123     C     *LIKE         DEFINE    TBLKEY        KEY
071800950313     C     *LIKE         DEFINE    VIDLNA        ELAFIL
071900950123     C*
072000930914     C                   ENDSR
072100070903**
072200070903Utente non abilitato alla Stampa delle C.A.                                   27
