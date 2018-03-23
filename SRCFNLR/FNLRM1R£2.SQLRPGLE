000100040527     H DECEDIT('0,') DATEDIT(*YMD.)
000200100312     H* FNLRM1R *-----------------------------------------------------*
000300930914     H*                - STATISTICA CONSEGNE -         FILTRO         *
000400100312     h*                  lancio unico da sede
000500930914     H*---------------------------------------------------------------*
000600970123     FTABEL00F  IF A E           K DISK
000700980525     FAZCLN01L  IF   E           K DISK
000800040527     FAZorg01L  IF   E           K DISK
000900100311     Fkplog05l  IF   E           K DISK
001000100310     FFNLRm1D   CF   E             WORKSTN
001100970123     D DPE             S              8  0 DIM(30)
001200970123     D DUE             S              8  0 DIM(30)
001300970123     D*
001400070903     D MSG             S             78    DIM(1) CTDATA PERRCD(1)              MSG VIDEO
001500070903     D $pog            s              3    dim(250)
001600070903     D $DIG            s              1    dim(20)
001700070903     D $ARG            s              3    dim(50)
001800070903
001900980525     D*
002000980525     D K31             S              1    DIM(31)                              SKI DI COMODO
002100920309     D WLBDAT          DS
002200950123     D  G02DAT                 1      8  0
002300950123     D  G02INV                 9     16  0
002400950123     D  G02ERR                17     17
002500950123     D  G02TGI                18     22  0
002600930914     D WGIDAT          DS
002700950123     D  GIODAT                 1      8  0
002800950123     D  GIOINV                 9     16  0
002900950123     D  GIOTGI                17     21  0
003000980525     D                 DS
003100980525     D  WAA                    1      4  0
003200980525     D  WMM                    5      6  0
003300980525     D  WGG                    7      8  0
003400980525     D  WDATA                  1      8  0
003500900511     D KPJBA         E DS
003600070903     D TIBS34DS      E DS
003700070903     D TRUL31DS      E DS
003800070903     D TRUL31DS2     E DS
003900070903     D TIBS02DS      E DS
004000950313     D* DS PER TRUL06R - CARICAMENTO £X
004100950313     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004200950313     D  LIN                    1     90  0
004300950313     D                                     DIM(30)                              FIL. COMODO
004400930914     D DS5A          E DS
004500040922     D DS5AS1        E DS
004600970123     D DS7AF         E DS
004700970123     D  ALFDPE                68     75
004800970123     D  ALFDUE                76     83
004900070903     d Azuteds       e ds                  extname(Azute00f)
005000070903     d dDatiute      e ds
005100070903     d dLat          e ds
005200070903     d dute01        e ds
005300100222
005400100222     d dvpoDECO      e ds
005500100310     d
005600100310     d tibs56ds      e ds
005700100310     d  ski                    7    756  0 dim(250)
005800100310     d                 ds
005900970207     D* PASSAGGIO DATI A PGM BATCH
006000970207     D DSLS01        E DS
006100040527     D fnlv55ds      E DS
006200050329     D og150         E DS
006300050329     D og148         E DS
006400070419     D og146         E DS
006500050331     D og143         E DS
006600070903     d
006700100312     d savsif          s                   like(knsif)
006800100312     d wabi            s                   like(UteAut)
006900090918     d wforza          s              1
007000100311     d wfine           s              1
007100090918     d wforzaR         s              1
007200100310     d X               s              4  0
007300100311     d Datasys         s               d   inz(*sys)
007400100311     d wrkgetlista     s           4096    varying
007500930914     C*---------------------------------------------------------------*
007600930914     C* INDICATORI USATI                                              *
007700930914     C*---------------------------------------------------------------*
007800060111     C* 01 - p.o. di primo   livello
007900060111     C* 30 - DI COMODO RIUTILIZZABILE
008000930914     C* 44 - ELABORAZIONE MOMENTANEAMENTE NON EFFETTUABILE
008100930914     C* 45 - ELABORAZIONE NON EFFETTUABILE PER MANCANZA DI DATI
008200980525     C* 46/53 - ERRORE
008300930914     C* 90 - GENERICO DI ERRORE
008400930914     C*---------------------------------------------------------------*
008500930914     C*
008600930914     C* DEFINIZIONE VARIABILI, KLIST E PLIST
008700930914     C                   EXSR      DEFVAR
008800930914     C*
008900950313     C* CARICAMENTO TABELLE
009000920309     C                   EXSR      CARTAB
009100900523     C*
009200100311     c     inizio        tag
009300100311     C*
009400920309     C                   Z-ADD     0             VIDDSD
009500920309     C                   Z-ADD     0             VIDDSA
009600920309     C                   Z-ADD     ELAFIL        VIDLNA
009700100311     C                   MOVEL     'N'           VIDSNC
009800100311     C                   MOVEL     'S'           VIDELA            1
009900050718     C                   MOVEL     ' '           VIDEsa
010000100222     C   02              MOVEL     'S'           VIDEAT
010100090918     c                   clear                   wforza
010200090918     c                   clear                   wforzaR
010300040527     C* IMPOSTO LA S NELLE SPUNTE PARTENZA SE ELAFIL FA IL
010400041215     C*  PICKING ED È TERMINAL ARRIVO oppure se elafil stampa i ddt si in
010500041215     c*  arrivo
010600041215     C                   IF        (§OGPCK='S'or §ogsda='S') AND D55TFA=ELAFIL
010700040527     C                   EVAL      VIDSPP='S'
010800040527     C                   ENDIF
010900070419     c*
011000070419     c* Imposto la "S" anche se ELAFIL effettua distinta consegna automatich
011100070419    1c                   if        fgsdda>0 and fgsdda<=DATEU
011200070419     C                   EVAL      VIDSPP='S'
011300070419     C                   ENDIF
011400920803     C*
011500900509     C     FOR01         TAG
011600100310     C                   EXFMT     LRm1D01
011700920803     C*
011800950123     C* F3 = FINE
011900900509     C   KC              GOTO      FINE
012000100311
012100100311     C* F7 = controllo se ci sono lavori in coda da elaboarare
012200100311     c                   if        *inkg
012300100311     C                   exsr      Contrbatch
012400100311     c                   goto      for01
012500100311     c                   endif
012600930914     C*
012700930914     C* CONTROLLI
012800900509     C                   EXSR      CONTR
012900950123     C*
013000950123     C   90
013100950123     CORNKF              GOTO      FOR01
013200920903     C*
013300950123     C* F6 = ELABORAZIONE
013400920803     C*
013500970130     C     COMDSA        IFEQ      *ZERO
013600970130     C                   Z-ADD     VIDDSD        VIDDSA                         GG/MM/AA
013700970130     C                   Z-ADD     COMDSD        COMDSA                         AA/MM/GG
013800970130     C                   ENDIF
013900970130     C*
014000100310      * Carico tutti i p.o
014100100310     c                   Clear                   Tibs56ds
014200100312     c                   Eval      I56Ppo = 001
014300100310     c                   Eval      I56Mod = 'POS'
014400100310     c                   Call      'TIBS56R'
014500100310     c                   Parm                    Tibs56ds
014600100310     c                   If        O56Err <> *blanks
014700100310     c                   Clear                   Ski
014800100310     c                   EndIf
014900100312     c* Salvo il S.I.
015000100312     c                   eval      savsif=knsif
015100100310
015200100310     c                   eval      x=1
015300100310    1c                   dow       ski(x)>0
015400100310     c                   movel     ski(x)        wlna              3 0
015500100310     c
015600100310    2c                   if        vidlna=0 or vidlna=wlna
015700100310     c
015800070903     C                   CLEAR                   FNLV55DS
015900070903     C                   MOVEL     'P'           D55TPT
016000100310     C                   MOVE      wlna          D55LIN
016100070903     C                   MOVE      comdsd        D55DRF
016200070903     C                   MOVEL     'L'           D55TLA
016300070903     C                   CALL      'FNLV55R'
016400070903     c                   parm                    fnlv55ds
016500070903     c                   movel     d55tfp        simfel
016600070903     c
016700050329     C* ESCLUDO p.o. non operativi e europolitan se non richiesti in modo
016800050329     c*  specifico
016900100310     c     wlna          chain     azorg01l
017000100310    3c                   if        not %found(azorg01l)
017100050329     c                   clear                   og148
017200050331     c                   clear                   og143
017300100310    3c                   endif
017400050329     c                   movel     orgde8        og148
017500050331     c                   movel     orgde3        og143
017600050329     c
017700050331    3c                   select
017800100310     C* ESCLUDO p.o.  europolitan
017900050331     c                   when      vidlna=0 and §ogntw='EUP'
018000100310     C* ESCLUDO p.o. non operativi
018100100310     c                   when      vidlna=0 and  §oglpo=' '
018200100310     c                   when      vidlna=0 and  (§ogNTW= 'DPD' or  §ogNTW=
018300100310     c                              'FED' or  §ogNTW= 'EEX'or §ogntw='PPT')
018400050331   x3c                   other
018500050331     c
018600970130      * Se la data fine periodo e' nulla la imposto come quella iniziale
018700970130     C                   CLEAR                   DSLS01
018800970130     C                   MOVEL     'A'           DS1TBO                         'P'=PARTENZE
018900040527     C                   MOVEL     'FNLR78R'     DS1PGM
019000970207     C* TIPO DI STATISTICA ARRIVI: "C" CONSEGNE
019100970207     C                   MOVEL     'C'           DS1FPG
019200970207     C*
019300970130     C                   Z-ADD     SIMFEL        DS1FEL                                     ORE
019400100310     C                   Z-ADD     wlna          DS1FGS                                     ORE
019500100310     C                   Z-ADD     wlna          DS1LNP                                     ZA
019600100310     C                   Z-ADD     wlna          DS1LNA                                     ZA
019700970130     C                   Z-ADD     VIDDSD        DS1GDA                                     AA
019800970130     C                   Z-ADD     VIDDSA        DS1GAL                                     A
019900970130     C                   Z-ADD     COMDSD        DS1ADA                                     GG
020000970130     C                   Z-ADD     COMDSA        DS1AAL                                     GG
020100970130     C                   MOVEL     VIDELA        DS1RIS                         RIST/ELAB
020200970207     C                   MOVEL     VIDSNC        DS1FL1
020300040608     C                   MOVEL     VIDSPP        DS1FL5
020400050706     C                   MOVEL     VIDesa        DS1FL6
020500100218     C                   MOVEL     VIDeAT        DS1FL7
020600100311     c
020700100311     c* Per superlancio non aggiorno mai la data in statistica
020800100311     C                   MOVEL     'N'           DS1FL8
020900970307     C* E' SEMPRE RIELABORAZIONE SE CHIESTA SOLO LA LISTA,
021000970307     C*  ANCHE SE HANNO MESSO R
021100100311     c                   select
021200100311     c* Elaboro senza stampare
021300100311     C     VIDSNC        wheneq    'N'
021400100311     C                   MOVEL     'S'           DS1FL1
021500100311     C                   MOVEL     ' '           DS1RIS
021600100311
021700100311     C     VIDSNC        whenne    'S'
021800970307     C                   MOVEL     'S'           DS1RIS
021900100311     C                   ENDsl
022000100311     c
022100970130      *
022200970130      * Sottometto il pgm che si occupera' del lancio effettivo delle statist.
022300970130     C                   CLEAR                   KPJBU
022400970130     C                   MOVEL     DSLS01        KPJBU
022500100311     c
022600970403     C* SE HO RICHIESTO SOLO LA LISTA SOTTOMETTO DIRETTAMENTE LR78
022700100311     c* se non sono in ambiente di prova
022800100312     c                   if        %subst(knsif:7:1)<>'P'
022900100311     c                   eval      kqieb='K'+ %editc(simfel:'X') +'SSPECL'
023000100311     c                   endif
023100100311     c
023200100312     c                   if        %subst(knsif:7:1)='P'
023300100312     C                   eval      %subst(knsif:1:8)='FILTRAPF'
023400100312     c                   else
023500100312     C                   eval      %subst(knsif:1:8)='FILTRA20'
023600100312     c                   endif
023700100312     c
023800100312     C                   MOVEL     'LR78'        KCOAZ
023900970403     C                   CALL      'BCH10'
024000970403     C                   PARM                    KPJBA
024100970403     C*
024200050331    3C                   ENDsl
024300050329    2C                   ENDIF
024400100310
024500970207     C                   ADD       1             X
024600050329    1C                   ENDDO
024700100311     c
024800100312     c                   eval      knsif=savsif
024900100311     c                   goto      inizio
025000900522     C*
025100000000     C     FINE          TAG
025200000000     C                   SETON                                        LR
025300930914     C*---------------------------------------------------------------*
025400930914     C*                    CONTR                                      *
025500930914     C* ROUTINE DI CONTROLLO DATI FORMATO VIDEO                       *
025600930914     C*---------------------------------------------------------------*
025700900509     C     CONTR         BEGSR
025800930914     C*
025900100222     C                   SETOFF                                       90
026000970317     C                   Z-ADD     0             COMDSA
026100970317     C                   Z-ADD     0             COMDSD
026200930914     C*
026300930914IF  1C     VIDLNA        IFNE      0
026400100310     c     vidlna        chain     azorg01l
026500100310     c                   if        not %found(azorg01l) or (orgfag<>'A' and
026600100310     c                             orgfag<>'F')
026700070903     c
026800100310     C                   SETON                                        5190
026900100310     C                   GOTO      ENDCTR
027000930914E   1C                   ENDIF
027100100310E   1C                   ENDIF
027200920309     C*
027300920902     C* DATA CONSEGNA DAL OBBLIGATORIA
027400920309     C                   MOVE      VIDDSD        G02DAT
027500920309     C                   MOVEL     *BLANK        G02ERR
027600950123     C                   CALL      'XSRDA8'
027700920309     C                   PARM                    WLBDAT
027800930914IF  1C     G02ERR        IFEQ      '1'
027900920309     C                   SETON                                        40  90
028000920309     C                   GOTO      ENDCTR
028100930914E   1C                   ENDIF
028200950123     C                   Z-ADD     G02DAT        VIDDSD
028300970123     C                   Z-ADD     G02INV        COMDSD            8 0
028400970123     C                   Z-ADD     G02TGI        DALTGI
028500970123     C     *LIKE         DEFINE    G02TGI        DALTGI
028600970124     C     *LIKE         DEFINE    G02TGI        WTGI
028700970307     C**
028800970307     C* SE 'LISTA O ENTRAMBE E' SEMPRE 'S'
028900970307     C     VIDSNC        IFEQ      'L'
029000970307     C     VIDSNC        OREQ      'E'
029100970307     C                   MOVEL     'S'           VIDELA
029200970307     C                   ENDIF
029300900522     C*
029400980525     C* SE NON E' INSERITA LA DATA AL CONTROLLO CHE LA DATA DAL SIA MINORE O
029500980525     C* UGUALE ALLA DATA MASSIMA RICHIESTA (OGGI + 1 GIORNO NON FESTIVO)
029600980525     C     VIDDSA        IFEQ      0
029700980525     C     COMDSD        ANDGT     DATAPA
029800980525     C                   SETON                                        52  90
029900980525     C                   GOTO      ENDCTR
030000980525     C                   ENDIF
030100920902     C* DATA AL
030200930914IF  1C     VIDDSA        IFNE      0
030300970130     C*
030400920309     C                   MOVE      VIDDSA        G02DAT
030500920309     C                   MOVEL     *BLANK        G02ERR
030600950123     C                   CALL      'XSRDA8'
030700920309     C                   PARM                    WLBDAT
030800930914IF  2C     G02ERR        IFEQ      '1'
030900920309     C                   SETON                                        41  90
031000920309     C                   GOTO      ENDCTR
031100930914E   2C                   ENDIF
031200950123     C                   Z-ADD     G02DAT        VIDDSA
031300970123     C                   Z-ADD     G02INV        COMDSA            8 0
031400920309     C*
031500900523     C* DATA DAL NO MAGGIORE DATA AL
031600930914IF  2C     COMDSD        IFGT      COMDSA
031700950123     C                   SETON                                        4290
031800900523     C                   GOTO      ENDCTR
031900930914E   2C                   ENDIF
032000970130     C* SE RICHIESTA SOLO LA LISTA SPEDIZIONI NON IN CONSEGNA LA DATA
032100970130     C* AL DEVE ESSERE = 0
032200970130     C     VIDSNC        IFEQ      'L'
032300970130     C     COMDSA        ANDNE     COMDSD
032400970130     C                   SETON                                        4690
032500970130     C                   GOTO      ENDCTR
032600970130    2C                   ENDIF
032700970130     C*
032800970130     C* SE RICHIESTA LA RIELABORAZIONE, NON AMMESSA
032900970130     C     VIDELA        IFEQ      'S'
033000970130     C     COMDSA        ANDNE     COMDSD
033100970130     C                   SETON                                        5090
033200970130     C                   GOTO      ENDCTR
033300970130    2C                   ENDIF
033400980525     C* CONTROLLO CHE LA DATA AL SIA MINORE O UGUALE
033500980525     C* ALLA DATA MASSIMA RICHIESTA (OGGI + 1 GIORNO NON FESTIVO)
033600980525     C     COMDSA        IFGT      DATAPA
033700980525     C                   SETON                                        5390
033800980525     C                   GOTO      ENDCTR
033900980525     C                   ENDIF
034000970130E   1C                   ENDIF
034100970124     C* SE DATA DAL AL IMMETTERE SOLO "S" PER AVERE SOLO LA STATISTICA
034200100312    1C     VIDDSA        IFGT      0
034300100312     C     VIDDSA        ANDNE     VIDDSD
034400100312     C     VIDSNC        ANDEQ     'E'
034500100312     C                   SETON                                        4690
034600100312     C                   GOTO      ENDCTR
034700100312E   1C                   ENDIF
034800970207     C*
034900970130     C* IN SEDE POSSO RICHIEDERE SOLTANTO LA RISTAMPA
035000100312    1C**   SIMFEL        IFEQ      0
035100100312     C**   VIDELA        ANDEQ     'S'
035200100312     C**                 SETON                                        5190
035300100312     C**                 GOTO      ENDCTR
035400100312    1C**                 ENDIF
035500930914     C*
035600930914     C* REPERISCO DATA ULTIMO UTILIZZO PROGRAMMA DI PULIZIA BOLLE
035700970207    1C     VIDELA        IFEQ      'S'
035800030114     C                   MOVEL     'FNLR84R'     wPGM
035900930914     C                   MOVEL     *BLANKS       DAT
036000930914     C                   MOVEL     *BLANKS       ERR
036100930914     C*
036200950123     C                   CALL      'TRUL49C'                            30
036300030114     C                   PARM                    wPGM             10
036400000103     C                   PARM                    DAT               8
036500930914     C                   PARM                    ERR               1
036600930914     C*
036700030114     C* PROGRAMMA IN USO o DATA ULTIMO UTILIZZO A ZERO UTILIZZO = UDATE
036800030114IF  2C                   IF        ERR = '1'  or DAT = '00000000'
036900940125     C                   Z-ADD     DATEU         G02INV
037000970207X   2C                   ELSE
037100950203     C                   CLEAR                   G02INV
037200940125     C                   MOVE      DAT           G02INV
037300970207    2C                   ENDIF
037400970207     C*
037500930914     C* SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
037600930914     C* PULIZIA E TROVO LA DATA AL DI SOTTO DELLA QUALE NON SI POSSONO
037700930914     C* LANCIARE ELABORAZIONI PER MANCANZA DI DATI
037800930914     C*
037900930914     C* SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
038000020218IF  2C**!!!      G02INV    IFEQ *ZEROS
038100020218     C**!!!                Z-ADD*ZEROS    GIOINV
038200020218X   2C**!!!                ELSE
038300930914     C                   Z-ADD     *ZEROS        G02DAT
038400930914     C                   Z-ADD     *ZEROS        G02TGI
038500030114     C                   MOVEL     '3'           G02err
038600950123     C                   CALL      'XSRDA8'
038700930914     C                   PARM                    WLBDAT
038800930914     C*
038900930914     C*
039000930914     C                   SUB       §5AARB        G02TGI
039100930914     C*
039200930914     C                   Z-ADD     *ZEROS        GIODAT
039300930914     C                   Z-ADD     *ZEROS        GIOINV
039400930914     C                   Z-ADD     G02TGI        GIOTGI
039500930914     C*
039600950123     C                   CALL      'XSRGI8'
039700930914     C                   PARM                    WGIDAT
039800020218E   2C**!!!                ENDIF
039900930914     C*
040000930914     C* DATA DAL MAGGIORE DATA CALCOLATA
040100090918     c* Per EDP errore forzabile
040200090918     c
040300090918    2c                   if        wforza=' ' or wforza='S' and
040400090918     c                             %subst(knmus:1:3)<>'EDP'
040500090918IF  3C     COMDSD        IFLE      GIOINV
040600940715     C                   SETON                                        4590
040700090918     c                   eval      wforza='S'
040800930914     C                   GOTO      ENDCTR
040900090918E   3C                   ENDIF
041000090918E   2C                   ENDIF
041100970207     C*
041200970207   X1C                   ELSE
041300970207     C**
041400030114     C                   MOVEL     'FNLRE4R'     wPGM                           RISTAMPA
041500970207     C                   MOVEL     *BLANKS       DAT
041600970207     C                   MOVEL     *BLANKS       ERR
041700970207     C*
041800970207     C                   CALL      'TRUL49C'                            30
041900030114     C                   PARM                    wPGM
042000000103     C                   PARM                    DAT               8
042100970207     C                   PARM                    ERR               1
042200970207     C*
042300030114     C* PROGRAMMA IN USO o DATA ULTIMO UTILIZZO A ZERO UTILIZZO = UDATE
042400030114IF  2C                   IF        ERR = '1'  or DAT = '00000000'
042500970207     C                   Z-ADD     DATEU         G02INV
042600970207X   2C                   ELSE
042700970207     C                   CLEAR                   G02INV
042800970207     C                   MOVE      DAT           G02INV
042900970207    2C                   ENDIF
043000970207     C*
043100970207     C* SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
043200970207     C* PULIZIA E TROVO LA DATA AL DI SOTTO DELLA QUALE NON SI POSSONO
043300970207     C* LANCIARE ELABORAZIONI PER MANCANZA DI DATI
043400970207     C*
043500970207     C* SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
043600020218IF  2C**!!!      G02INV    IFEQ *ZEROS
043700020218     C**!!!                Z-ADD*ZEROS    GIOINV
043800020218X   2C**!!!                ELSE
043900970207     C                   Z-ADD     *ZEROS        G02DAT
044000970207     C                   Z-ADD     *ZEROS        G02TGI
044100970207     C                   MOVEL     '3'           G02ERR
044200970207     C                   CALL      'XSRDA8'
044300970207     C                   PARM                    WLBDAT
044400970207     C*
044500970207     C* PULIZIA STASTISTICA ARRIVI
044600040922     C***                SUB       §5ASTA        G02TGI
044700040922     C                   SUB       §5ASTS        G02TGI
044800970207     C*
044900970207     C                   Z-ADD     *ZEROS        GIODAT
045000970207     C                   Z-ADD     *ZEROS        GIOINV
045100970207     C                   Z-ADD     G02TGI        GIOTGI
045200970207     C*
045300970207     C                   CALL      'XSRGI8'
045400970207     C                   PARM                    WGIDAT
045500020218E   2C**!!!                ENDIF
045600970207     C*
045700970207     C* DATA DAL MAGGIORE DATA CALCOLATA
045800970207IF  2C     COMDSD        IFLE      GIOINV
045900090918    3c                   if        wforzaR=' ' or wforzaR='S' and
046000090918     c                             %subst(knmus:1:3)<>'EDP'
046100970207     C                   SETON                                        4590
046200090918     c                   eval      wforzaR='S'
046300970207     C                   GOTO      ENDCTR
046400090918E   3C                   ENDIF
046500090918     c
046600090918E   2C                   ENDIF
046700970207    1C                   ENDIF
046800920803     C*
046900900509     C     ENDCTR        ENDSR
047000930914     C*---------------------------------------------------------------*
047100930914     C*                    CARTAB                                     *
047200950313     C* ROUTINE DI CARICAMENTO TABELLE £1 E £6                        *
047300930914     C*---------------------------------------------------------------*
047400930914     C     CARTAB        BEGSR
047500040527     C*
047600040527     C*
047700040527     C* VERIFICO SE P.O. ESEGUE PIKING DA ORGANIGRAMMA
047800040527     C     ELAFIL        CHAIN     AZORG01L
047900040527     C                   IF        %FOUND
048000040527     C                   MOVEL     ORGDF0        OG150
048100070419     c                   Movel     OrgDe6        Og146
048200040527     C                   ELSE
048300040527     C                   CLEAR                   OG150
048400070419     C                   CLEAR                   OG146
048500040527     C                   ENDIF
048600070419     c*
048700070419     c                   setoff                                       23
048800070419     c                   clear                   FGSdda
048900070419    1c                   if        §ogdda>*zeros
049000070419     c                   testn                   §ogdda               37
049100070419     c                   move      §ogdda        w001a             1
049200070419    2c                   if        *in37 and w001a>='0'
049300070419     c* Se la data = 20391231, come se non ci fosse
049400070419    3c                   if        §ogdda<>'20391231'
049500070419     c                   movel     §ogdda        FGSdda            8 0
049600070419    3c                   endif
049700070419    2c                   endif
049800070419    1c                   endif
049900070419     c
050000040527     C* VERIFICO SE ELAFIL È TERMINAL DI ARRIVO
050100041215     c                   if        §ogpck='S' or §ogsda='S'
050200040527     C                   CLEAR                   FNLV55DS
050300040527     C                   MOVEL     'A'           D55TPT
050400040527     C                   MOVE      ELAFIL        D55LIN
050500040527     C                   MOVE      DATEU         D55DRF
050600040527     C                   MOVEL     'L'           D55TLA
050700040527     C                   CALL      'FNLV55R'
050800040527     C                   PARM                    fnlv55ds
050900040527     c                   endif
051000040527     C
051100970207     C**
051200970207     C                   MOVEL     '5A'          COD
051300970207     C                   MOVEL     *BLANKS       KEY
051400970207     C                   MOVEL     '1'           KEY
051500970207     C     KTAB          CHAIN     TABEL00F                           30
051600970207IF  3C     *IN30         IFEQ      *ON
051700970207     C                   CLEAR                   DS5A
051800970207X   3C                   ELSE
051900970207     C                   MOVEL     TBLUNI        DS5A
052000970207E   3C                   ENDIF
052100970207     C*
052200970718     C                   MOVEL     '5A'          COD
052300040922     C                   MOVEL(P)  'SEDE1'       KEY
052400970207     C     KTAB          CHAIN     TABEL00F                           30
052500970207IF  3C     *IN30         IFEQ      *ON
052600040922     C                   CLEAR                   DS5As1
052700970207X   3C                   ELSE
052800040922     C                   MOVEL     TBLUNI        DS5As1
052900970207E   3C                   ENDIF
053000930914     C*
053100930914     C                   ENDSR
053200930914     C*---------------------------------------------------------------*
053300930914     C*                    DEFVAR                                     *
053400930914     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
053500930914     C*---------------------------------------------------------------*
053600930914     C     DEFVAR        BEGSR
053700930914     C*
053800930914     C     *ENTRY        PLIST
053900930914     C                   PARM                    KPJBA
054000070903      *
054100070903     c     *dtaara       define    §azute        azuteds
054200070903     c     *dtaara       define    §datiute      ddatiute
054300070903     c                   in(E)     *dtaara
054400070903    1c                   If        %error  or RSUT = *blanks
054500070903     c                   Clear                   Tibs34ds
054600070903     c                   Call      'TIBS34R'
054700070903     c                   Parm                    Tibs34ds
054800070903     c                   In        *dtaara
054900070903    1c                   EndIf
055000040527     C* GIRO UDATE
055100040527     C                   TIME                    WTIME            14 0          ORA & DATA
055200040527     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
055300040527     C                   Z-ADD     WDATE         G02DAT
055400040527     C                   Z-ADD     *ZERO         G02INV
055500040527     C                   MOVEL     *BLANKS       G02ERR
055600040527     C                   CALL      'XSRDA8'
055700040527     C                   PARM                    WLBDAT
055800040527     C                   Z-ADD     G02INV        DATEU             8 0          AAAA/MM/GG
055900930914     C*
056000980525     C* MI CALCOLO LA DATA MASSIMA DI RICHIESTA STAMPA CHE E' OGGI + IL
056100980525     C* PRIMO GIORNO NON FESTIVO
056200040527     C                   Z-ADD     g02inv        DATAPA            8 0
056300980525     C* ADDIZIONO 1 FINTANTO CHE NON TROVO IL PRIMO GIORNO LAVORATIVO
056400980525     C*
056500980525     C                   MOVEL     'F'           WFEST             1
056600980525     C     G02TGI        ADD       1             GIOTGI
056700980525     C                   ADD       1             G02TGI
056800980525     C*
056900980525    1C     WFEST         DOWNE     ' '
057000980525     C     G02TGI        ADD       1             GIOTGI
057100980525     C                   ADD       1             G02TGI
057200980525     C                   CALL      'XSRGI8'
057300980525     C                   PARM                    WGIDAT
057400980525     C*
057500980525     C                   Z-ADD     GIOINV        DATAPA            8 0
057600980525     C*
057700980525     C* IMPOSTO ANNO E MESE DELLA DATA TROVATA
057800980525     C                   Z-ADD     GIOINV        WDATA
057900980525     C                   CLEAR                   W0030
058000980525     C* CHAIN SUL CALENDARIO BARTOLINI
058100980525     C     KCLN          CHAIN     AZCLN01L                           30
058200980525     C   30              CLEAR                   CLNPOM
058300980525     C                   MOVEA     CLNPOM        K31
058400980525     C                   MOVEL     K31(WGG)      WFEST
058500980525    1C                   ENDDO
058600100222
058700100222      * Aggancio tabella VPO record DECO
058800100222     c                   Clear                   Dvpodeco
058900100222     c                   Clear                   Tibs02ds
059000100222     c                   Eval      T02Mod = 'C'
059100100222     c                   Eval      T02Sif = knsif
059200100222     c                   Eval      T02Cod = 'VPO'
059300100222     c                   Movel(p)  'DECO'        T02Ke1
059400100222     c                   Call      'TIBS02R'
059500100222     c                   Parm                    Kpjba
059600100222     c                   Parm                    Tibs02ds
059700100222     c                   If        T02Err = *Blanks
059800100222     c                   Movel     T02Uni        Dvpodeco
059900100222     c                   EndIf
060000100222     c
060100100222     c                   if        dateu>=§vpogma
060200100222     c                   seton                                        02
060300100222     c                   endif
060400100222
060500980525     C*
060600930914     C     KTAB          KLIST
060700070903     C                   KFLD                    CODUT             1 0
060800950123     C                   KFLD                    COD
060900100311     c                   KFLD                    KEY
061000980525     C* ACCESSO   AZCLN
061100980525     C     KCLN          KLIST
061200100312     C                   KFLD                    w0030
061300980525     C                   KFLD                    W0030             3 0
061400980525     C                   KFLD                    WAA               4 0
061500980525     C                   KFLD                    WMM               2 0
061600930914     C*
061700950123     C     *LIKE         DEFINE    TBLCOD        COD
061800950123     C     *LIKE         DEFINE    TBLKEY        KEY
061900950313     C     *LIKE         DEFINE    VIDLNA        ELAFIL
062000950123     C*
062100930914     C                   ENDSR
062200100311     C*---------------------------------------------------------------*
062300100311     C* Controllo lavori anche da elaborare                           *
062400100311     C*---------------------------------------------------------------*
062500100311     C     ContrBATCH    BEGSR
062600100311     c                   setoff                                       03
062700100311     c                   eval      wfine='N'
062800100311
062900100311     c     *cymd         movel     datasys       w0070             7 0
063000100311
063100100311     c                   dou       wfine='S'
063200100311     c
063300100311     c                   eval      wrkgetlista =
063400100311     c                             'select logkpj, logdac -
063500100311     c                              from kplog05l where -
063600100311     c                             lognmu=''' + knmus + ''' and logdaf=0 -
063700100311     c                             and logorf=0 '
063800100311     C/EXEC SQL
063900100311     C+ PREPARE s1 FROM :wrkgetlista
064000100311     C/END-EXEC
064100100311     C
064200100311     C/EXEC SQL
064300100311     C+ DECLARE A1 CURSOR FOR S1
064400100311     C/END-EXEC
064500100311     C
064600100311     C/EXEC SQL
064700100311     C+ OPEN a1
064800100311     C/END-EXEC
064900100311
065000100311     c                   clear                   conta             4 0
065100100311      *?Leggo il file
065200100311     c                   do        *hival
065300100311     C/EXEC SQL
065400100311     C+ FETCH NEXT FROM a1 INTO :logkpj, :logdac
065500100311     C/END-EXEC
065600100311
065700100311     c                   select
065800100311
065900100311     c                   when      sqlcod = 100
066000100311     c                   leave
066100100311     c                   when      sqlcod < 0
066200100311     c                   seton                                        H1
066300100311     c                   leave
066400100311     c                   other
066500100311
066600100311     c                   if        %subst(logkpj:135:4)='LR78'
066700100311     c                             and logdac=w0070
066800100311     c                   eval      conta=conta+1
066900100311     c                   endif
067000100311
067100100311     c                   endsl
067200100311
067300100311     c                   enddo
067400100311
067500100311     C/EXEC SQL
067600100311     C+ CLOSE a1
067700100311     C/END-EXEC
067800100311
067900100311     c                   if        conta>0
068000100311     c                   seton                                        03
068100100311     c                   else
068200100311     c                   setoff                                       03
068300100311     c                   endif
068400100311     c
068500100311     c                   exfmt     lrm1w02
068600100311     c
068700100311     C                   IF        *INKE
068800100311     C                   eval      wfine='N'
068900100311     c                   else
069000100311     C                   eval      wfine='S'
069100100311     c                   endif
069200100311
069300100311     c                   enddo
069400100311     c
069500100311     c
069600100311     C                   ENDSR
069700070903**
069800070903Utente non abilitato alla Stampa delle C.A.                                   27
