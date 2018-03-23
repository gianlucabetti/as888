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
006200050329     D og148         E DS
006300050331     D og143         E DS
006400070903     d
006500100312     d savsif          s                   like(knsif)
006600100312     d wabi            s                   like(UteAut)
006700090918     d wforza          s              1
006800100311     d wfine           s              1
006900090918     d wforzaR         s              1
007000100310     d X               s              4  0
007100100311     d Datasys         s               d   inz(*sys)
007200100311     d wrkgetlista     s           4096    varying
007300930914     C*---------------------------------------------------------------*
007400930914     C* INDICATORI USATI                                              *
007500930914     C*---------------------------------------------------------------*
007600060111     C* 01 - p.o. di primo   livello
007700060111     C* 30 - DI COMODO RIUTILIZZABILE
007800930914     C* 44 - ELABORAZIONE MOMENTANEAMENTE NON EFFETTUABILE
007900930914     C* 45 - ELABORAZIONE NON EFFETTUABILE PER MANCANZA DI DATI
008000980525     C* 46/53 - ERRORE
008100930914     C* 90 - GENERICO DI ERRORE
008200930914     C*---------------------------------------------------------------*
008300930914     C*
008400930914     C* DEFINIZIONE VARIABILI, KLIST E PLIST
008500930914     C                   EXSR      DEFVAR
008600930914     C*
008700950313     C* CARICAMENTO TABELLE
008800920309     C                   EXSR      CARTAB
008900900523     C*
009000100311     c     inizio        tag
009100100311     C*
009200920309     C                   Z-ADD     0             VIDDSD
009300920309     C                   Z-ADD     0             VIDDSA
009400920309     C                   Z-ADD     ELAFIL        VIDLNA
009500100311     C                   MOVEL     'N'           VIDSNC
009600100311     C                   MOVEL     'S'           VIDELA            1
009700050718     C                   MOVEL     ' '           VIDEsa
009800100222     C   02              MOVEL     'S'           VIDEAT
009900090918     c                   clear                   wforza
010000090918     c                   clear                   wforzaR
010100120224     c* Visto che l'ufficio qualità elabora il pomeriggio per il girno stesso
010200120224     c*  meglio non considerare le spunte partenza per avere una situazione più veriitiera
010300120224     c*  delle non in conssegna
010400120224     C                   EVAL      VIDSPP=' '
010500070419     c*
010600920803     C*
010700900509     C     FOR01         TAG
010800100310     C                   EXFMT     LRm1D01
010900920803     C*
011000950123     C* F3 = FINE
011100900509     C   KC              GOTO      FINE
011200100311
011300100311     C* F7 = controllo se ci sono lavori in coda da elaboarare
011400100311     c                   if        *inkg
011500100311     C                   exsr      Contrbatch
011600100311     c                   goto      for01
011700100311     c                   endif
011800930914     C*
011900930914     C* CONTROLLI
012000900509     C                   EXSR      CONTR
012100950123     C*
012200950123     C   90
012300950123     CORNKF              GOTO      FOR01
012400920903     C*
012500950123     C* F6 = ELABORAZIONE
012600920803     C*
012700970130     C     COMDSA        IFEQ      *ZERO
012800970130     C                   Z-ADD     VIDDSD        VIDDSA                         GG/MM/AA
012900970130     C                   Z-ADD     COMDSD        COMDSA                         AA/MM/GG
013000970130     C                   ENDIF
013100970130     C*
013200100310      * Carico tutti i p.o
013300100310     c                   Clear                   Tibs56ds
013400100312     c                   Eval      I56Ppo = 001
013500100310     c                   Eval      I56Mod = 'POS'
013600100310     c                   Call      'TIBS56R'
013700100310     c                   Parm                    Tibs56ds
013800100310     c                   If        O56Err <> *blanks
013900100310     c                   Clear                   Ski
014000100310     c                   EndIf
014100100312     c* Salvo il S.I.
014200100312     c                   eval      savsif=knsif
014300100310
014400100310     c                   eval      x=1
014500100310    1c                   dow       ski(x)>0
014600100310     c                   movel     ski(x)        wlna              3 0
014700100310     c
014800100310    2c                   if        vidlna=0 or vidlna=wlna
014900100310     c
015000070903     C                   CLEAR                   FNLV55DS
015100070903     C                   MOVEL     'P'           D55TPT
015200100310     C                   MOVE      wlna          D55LIN
015300070903     C                   MOVE      comdsd        D55DRF
015400070903     C                   MOVEL     'L'           D55TLA
015500070903     C                   CALL      'FNLV55R'
015600070903     c                   parm                    fnlv55ds
015700070903     c                   movel     d55tfp        simfel
015800070903     c
015900050329     C* ESCLUDO p.o. non operativi e europolitan se non richiesti in modo
016000050329     c*  specifico
016100100310     c     wlna          chain     azorg01l
016200100310    3c                   if        not %found(azorg01l)
016300050329     c                   clear                   og148
016400050331     c                   clear                   og143
016500100310    3c                   endif
016600050329     c                   movel     orgde8        og148
016700050331     c                   movel     orgde3        og143
016800050329     c
016900050331    3c                   select
017000100310     C* ESCLUDO p.o.  europolitan
017100050331     c                   when      vidlna=0 and §ogntw='EUP'
017200100310     C* ESCLUDO p.o. non operativi
017300100310     c                   when      vidlna=0 and  §oglpo=' '
017400100310     c                   when      vidlna=0 and  (§ogNTW= 'DPD' or  §ogNTW=
017500100310     c                              'FED' or  §ogNTW= 'EEX'or §ogntw='PPT')
017600050331   x3c                   other
017700050331     c
017800970130      * Se la data fine periodo e' nulla la imposto come quella iniziale
017900970130     C                   CLEAR                   DSLS01
018000970130     C                   MOVEL     'A'           DS1TBO                         'P'=PARTENZE
018100040527     C                   MOVEL     'FNLR78R'     DS1PGM
018200970207     C* TIPO DI STATISTICA ARRIVI: "C" CONSEGNE
018300970207     C                   MOVEL     'C'           DS1FPG
018400970207     C*
018500970130     C                   Z-ADD     SIMFEL        DS1FEL                                     ORE
018600100310     C                   Z-ADD     wlna          DS1FGS                                     ORE
018700100310     C                   Z-ADD     wlna          DS1LNP                                     ZA
018800100310     C                   Z-ADD     wlna          DS1LNA                                     ZA
018900970130     C                   Z-ADD     VIDDSD        DS1GDA                                     AA
019000970130     C                   Z-ADD     VIDDSA        DS1GAL                                     A
019100970130     C                   Z-ADD     COMDSD        DS1ADA                                     GG
019200970130     C                   Z-ADD     COMDSA        DS1AAL                                     GG
019300970130     C                   MOVEL     VIDELA        DS1RIS                         RIST/ELAB
019400970207     C                   MOVEL     VIDSNC        DS1FL1
019500040608     C                   MOVEL     VIDSPP        DS1FL5
019600050706     C                   MOVEL     VIDesa        DS1FL6
019700100218     C                   MOVEL     VIDeAT        DS1FL7
019800100311     c
019900100311     c* Per superlancio non aggiorno mai la data in statistica
020000100311     C                   MOVEL     'N'           DS1FL8
020100970307     C* E' SEMPRE RIELABORAZIONE SE CHIESTA SOLO LA LISTA,
020200970307     C*  ANCHE SE HANNO MESSO R
020300100311     c                   select
020400100311     c* Elaboro senza stampare
020500100311     C     VIDSNC        wheneq    'N'
020600100311     C                   MOVEL     'S'           DS1FL1
020700100311     C                   MOVEL     ' '           DS1RIS
020800100311
020900100311     C     VIDSNC        whenne    'S'
021000970307     C                   MOVEL     'S'           DS1RIS
021100100311     C                   ENDsl
021200100311     c
021300970130      *
021400970130      * Sottometto il pgm che si occupera' del lancio effettivo delle statist.
021500970130     C                   CLEAR                   KPJBU
021600970130     C                   MOVEL     DSLS01        KPJBU
021700100311     c
021800970403     C* SE HO RICHIESTO SOLO LA LISTA SOTTOMETTO DIRETTAMENTE LR78
021900100311     c* se non sono in ambiente di prova
022000100312     c                   if        %subst(knsif:7:1)<>'P'
022100100311     c                   eval      kqieb='K'+ %editc(simfel:'X') +'SSPECL'
022200100311     c                   endif
022300100311     c
022400100312     c                   if        %subst(knsif:7:1)='P'
022500100312     C                   eval      %subst(knsif:1:8)='FILTRAPF'
022600100312     c                   else
022700100312     C                   eval      %subst(knsif:1:8)='FILTRA20'
022800100312     c                   endif
022900100312     c
023000100312     C                   MOVEL     'LR78'        KCOAZ
023100970403     C                   CALL      'BCH10'
023200970403     C                   PARM                    KPJBA
023300970403     C*
023400050331    3C                   ENDsl
023500050329    2C                   ENDIF
023600100310
023700970207     C                   ADD       1             X
023800050329    1C                   ENDDO
023900100311     c
024000100312     c                   eval      knsif=savsif
024100100311     c                   goto      inizio
024200900522     C*
024300000000     C     FINE          TAG
024400000000     C                   SETON                                        LR
024500930914     C*---------------------------------------------------------------*
024600930914     C*                    CONTR                                      *
024700930914     C* ROUTINE DI CONTROLLO DATI FORMATO VIDEO                       *
024800930914     C*---------------------------------------------------------------*
024900900509     C     CONTR         BEGSR
025000930914     C*
025100100222     C                   SETOFF                                       90
025200970317     C                   Z-ADD     0             COMDSA
025300970317     C                   Z-ADD     0             COMDSD
025400930914     C*
025500930914IF  1C     VIDLNA        IFNE      0
025600100310     c     vidlna        chain     azorg01l
025700100310     c                   if        not %found(azorg01l) or (orgfag<>'A' and
025800100310     c                             orgfag<>'F')
025900070903     c
026000100310     C                   SETON                                        5190
026100100310     C                   GOTO      ENDCTR
026200930914E   1C                   ENDIF
026300100310E   1C                   ENDIF
026400920309     C*
026500920902     C* DATA CONSEGNA DAL OBBLIGATORIA
026600920309     C                   MOVE      VIDDSD        G02DAT
026700920309     C                   MOVEL     *BLANK        G02ERR
026800950123     C                   CALL      'XSRDA8'
026900920309     C                   PARM                    WLBDAT
027000930914IF  1C     G02ERR        IFEQ      '1'
027100920309     C                   SETON                                        40  90
027200920309     C                   GOTO      ENDCTR
027300930914E   1C                   ENDIF
027400950123     C                   Z-ADD     G02DAT        VIDDSD
027500970123     C                   Z-ADD     G02INV        COMDSD            8 0
027600970123     C                   Z-ADD     G02TGI        DALTGI
027700970123     C     *LIKE         DEFINE    G02TGI        DALTGI
027800970124     C     *LIKE         DEFINE    G02TGI        WTGI
027900970307     C**
028000970307     C* SE 'LISTA O ENTRAMBE E' SEMPRE 'S'
028100970307     C     VIDSNC        IFEQ      'L'
028200970307     C     VIDSNC        OREQ      'E'
028300970307     C                   MOVEL     'S'           VIDELA
028400970307     C                   ENDIF
028500900522     C*
028600980525     C* SE NON E' INSERITA LA DATA AL CONTROLLO CHE LA DATA DAL SIA MINORE O
028700980525     C* UGUALE ALLA DATA MASSIMA RICHIESTA (OGGI + 1 GIORNO NON FESTIVO)
028800980525     C     VIDDSA        IFEQ      0
028900980525     C     COMDSD        ANDGT     DATAPA
029000980525     C                   SETON                                        52  90
029100980525     C                   GOTO      ENDCTR
029200980525     C                   ENDIF
029300920902     C* DATA AL
029400930914IF  1C     VIDDSA        IFNE      0
029500970130     C*
029600920309     C                   MOVE      VIDDSA        G02DAT
029700920309     C                   MOVEL     *BLANK        G02ERR
029800950123     C                   CALL      'XSRDA8'
029900920309     C                   PARM                    WLBDAT
030000930914IF  2C     G02ERR        IFEQ      '1'
030100920309     C                   SETON                                        41  90
030200920309     C                   GOTO      ENDCTR
030300930914E   2C                   ENDIF
030400950123     C                   Z-ADD     G02DAT        VIDDSA
030500970123     C                   Z-ADD     G02INV        COMDSA            8 0
030600920309     C*
030700900523     C* DATA DAL NO MAGGIORE DATA AL
030800930914IF  2C     COMDSD        IFGT      COMDSA
030900950123     C                   SETON                                        4290
031000900523     C                   GOTO      ENDCTR
031100930914E   2C                   ENDIF
031200970130     C* SE RICHIESTA SOLO LA LISTA SPEDIZIONI NON IN CONSEGNA LA DATA
031300970130     C* AL DEVE ESSERE = 0
031400970130     C     VIDSNC        IFEQ      'L'
031500970130     C     COMDSA        ANDNE     COMDSD
031600970130     C                   SETON                                        4690
031700970130     C                   GOTO      ENDCTR
031800970130    2C                   ENDIF
031900970130     C*
032000970130     C* SE RICHIESTA LA RIELABORAZIONE, NON AMMESSA
032100970130     C     VIDELA        IFEQ      'S'
032200970130     C     COMDSA        ANDNE     COMDSD
032300970130     C                   SETON                                        5090
032400970130     C                   GOTO      ENDCTR
032500970130    2C                   ENDIF
032600980525     C* CONTROLLO CHE LA DATA AL SIA MINORE O UGUALE
032700980525     C* ALLA DATA MASSIMA RICHIESTA (OGGI + 1 GIORNO NON FESTIVO)
032800980525     C     COMDSA        IFGT      DATAPA
032900980525     C                   SETON                                        5390
033000980525     C                   GOTO      ENDCTR
033100980525     C                   ENDIF
033200970130E   1C                   ENDIF
033300970124     C* SE DATA DAL AL IMMETTERE SOLO "S" PER AVERE SOLO LA STATISTICA
033400100312    1C     VIDDSA        IFGT      0
033500100312     C     VIDDSA        ANDNE     VIDDSD
033600100312     C     VIDSNC        ANDEQ     'E'
033700100312     C                   SETON                                        4690
033800100312     C                   GOTO      ENDCTR
033900100312E   1C                   ENDIF
034000970207     C*
034100970130     C* IN SEDE POSSO RICHIEDERE SOLTANTO LA RISTAMPA
034200100312    1C**   SIMFEL        IFEQ      0
034300100312     C**   VIDELA        ANDEQ     'S'
034400100312     C**                 SETON                                        5190
034500100312     C**                 GOTO      ENDCTR
034600100312    1C**                 ENDIF
034700930914     C*
034800930914     C* REPERISCO DATA ULTIMO UTILIZZO PROGRAMMA DI PULIZIA BOLLE
034900970207    1C     VIDELA        IFEQ      'S'
035000030114     C                   MOVEL     'FNLR84R'     wPGM
035100930914     C                   MOVEL     *BLANKS       DAT
035200930914     C                   MOVEL     *BLANKS       ERR
035300930914     C*
035400950123     C                   CALL      'TRUL49C'                            30
035500030114     C                   PARM                    wPGM             10
035600000103     C                   PARM                    DAT               8
035700930914     C                   PARM                    ERR               1
035800930914     C*
035900030114     C* PROGRAMMA IN USO o DATA ULTIMO UTILIZZO A ZERO UTILIZZO = UDATE
036000030114IF  2C                   IF        ERR = '1'  or DAT = '00000000'
036100940125     C                   Z-ADD     DATEU         G02INV
036200970207X   2C                   ELSE
036300950203     C                   CLEAR                   G02INV
036400940125     C                   MOVE      DAT           G02INV
036500970207    2C                   ENDIF
036600970207     C*
036700930914     C* SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
036800930914     C* PULIZIA E TROVO LA DATA AL DI SOTTO DELLA QUALE NON SI POSSONO
036900930914     C* LANCIARE ELABORAZIONI PER MANCANZA DI DATI
037000930914     C*
037100930914     C* SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
037200020218IF  2C**!!!      G02INV    IFEQ *ZEROS
037300020218     C**!!!                Z-ADD*ZEROS    GIOINV
037400020218X   2C**!!!                ELSE
037500930914     C                   Z-ADD     *ZEROS        G02DAT
037600930914     C                   Z-ADD     *ZEROS        G02TGI
037700030114     C                   MOVEL     '3'           G02err
037800950123     C                   CALL      'XSRDA8'
037900930914     C                   PARM                    WLBDAT
038000930914     C*
038100930914     C*
038200930914     C                   SUB       §5AARB        G02TGI
038300930914     C*
038400930914     C                   Z-ADD     *ZEROS        GIODAT
038500930914     C                   Z-ADD     *ZEROS        GIOINV
038600930914     C                   Z-ADD     G02TGI        GIOTGI
038700930914     C*
038800950123     C                   CALL      'XSRGI8'
038900930914     C                   PARM                    WGIDAT
039000020218E   2C**!!!                ENDIF
039100930914     C*
039200930914     C* DATA DAL MAGGIORE DATA CALCOLATA
039300090918     c* Per EDP errore forzabile
039400090918     c
039500090918    2c                   if        wforza=' ' or wforza='S' and
039600090918     c                             %subst(knmus:1:3)<>'EDP'
039700090918IF  3C     COMDSD        IFLE      GIOINV
039800940715     C                   SETON                                        4590
039900090918     c                   eval      wforza='S'
040000930914     C                   GOTO      ENDCTR
040100090918E   3C                   ENDIF
040200090918E   2C                   ENDIF
040300970207     C*
040400970207   X1C                   ELSE
040500970207     C**
040600030114     C                   MOVEL     'FNLRE4R'     wPGM                           RISTAMPA
040700970207     C                   MOVEL     *BLANKS       DAT
040800970207     C                   MOVEL     *BLANKS       ERR
040900970207     C*
041000970207     C                   CALL      'TRUL49C'                            30
041100030114     C                   PARM                    wPGM
041200000103     C                   PARM                    DAT               8
041300970207     C                   PARM                    ERR               1
041400970207     C*
041500030114     C* PROGRAMMA IN USO o DATA ULTIMO UTILIZZO A ZERO UTILIZZO = UDATE
041600030114IF  2C                   IF        ERR = '1'  or DAT = '00000000'
041700970207     C                   Z-ADD     DATEU         G02INV
041800970207X   2C                   ELSE
041900970207     C                   CLEAR                   G02INV
042000970207     C                   MOVE      DAT           G02INV
042100970207    2C                   ENDIF
042200970207     C*
042300970207     C* SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
042400970207     C* PULIZIA E TROVO LA DATA AL DI SOTTO DELLA QUALE NON SI POSSONO
042500970207     C* LANCIARE ELABORAZIONI PER MANCANZA DI DATI
042600970207     C*
042700970207     C* SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
042800020218IF  2C**!!!      G02INV    IFEQ *ZEROS
042900020218     C**!!!                Z-ADD*ZEROS    GIOINV
043000020218X   2C**!!!                ELSE
043100970207     C                   Z-ADD     *ZEROS        G02DAT
043200970207     C                   Z-ADD     *ZEROS        G02TGI
043300970207     C                   MOVEL     '3'           G02ERR
043400970207     C                   CALL      'XSRDA8'
043500970207     C                   PARM                    WLBDAT
043600970207     C*
043700970207     C* PULIZIA STASTISTICA ARRIVI
043800040922     C***                SUB       §5ASTA        G02TGI
043900040922     C                   SUB       §5ASTS        G02TGI
044000970207     C*
044100970207     C                   Z-ADD     *ZEROS        GIODAT
044200970207     C                   Z-ADD     *ZEROS        GIOINV
044300970207     C                   Z-ADD     G02TGI        GIOTGI
044400970207     C*
044500970207     C                   CALL      'XSRGI8'
044600970207     C                   PARM                    WGIDAT
044700020218E   2C**!!!                ENDIF
044800970207     C*
044900970207     C* DATA DAL MAGGIORE DATA CALCOLATA
045000970207IF  2C     COMDSD        IFLE      GIOINV
045100090918    3c                   if        wforzaR=' ' or wforzaR='S' and
045200090918     c                             %subst(knmus:1:3)<>'EDP'
045300970207     C                   SETON                                        4590
045400090918     c                   eval      wforzaR='S'
045500970207     C                   GOTO      ENDCTR
045600090918E   3C                   ENDIF
045700090918     c
045800090918E   2C                   ENDIF
045900970207    1C                   ENDIF
046000920803     C*
046100900509     C     ENDCTR        ENDSR
046200930914     C*---------------------------------------------------------------*
046300930914     C*                    CARTAB                                     *
046400950313     C* ROUTINE DI CARICAMENTO TABELLE £1 E £6                        *
046500930914     C*---------------------------------------------------------------*
046600930914     C     CARTAB        BEGSR
046700970207     C**
046800970207     C                   MOVEL     '5A'          COD
046900970207     C                   MOVEL     *BLANKS       KEY
047000970207     C                   MOVEL     '1'           KEY
047100120224     c                   z-add     1             codut
047200970207     C     KTAB          CHAIN     TABEL00F                           30
047300970207IF  3C     *IN30         IFEQ      *ON
047400970207     C                   CLEAR                   DS5A
047500970207X   3C                   ELSE
047600970207     C                   MOVEL     TBLUNI        DS5A
047700970207E   3C                   ENDIF
047800970207     C*
047900970718     C                   MOVEL     '5A'          COD
048000040922     C                   MOVEL(P)  'SEDE1'       KEY
048100970207     C     KTAB          CHAIN     TABEL00F                           30
048200970207IF  3C     *IN30         IFEQ      *ON
048300040922     C                   CLEAR                   DS5As1
048400970207X   3C                   ELSE
048500040922     C                   MOVEL     TBLUNI        DS5As1
048600970207E   3C                   ENDIF
048700930914     C*
048800930914     C                   ENDSR
048900930914     C*---------------------------------------------------------------*
049000930914     C*                    DEFVAR                                     *
049100930914     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
049200930914     C*---------------------------------------------------------------*
049300930914     C     DEFVAR        BEGSR
049400930914     C*
049500930914     C     *ENTRY        PLIST
049600930914     C                   PARM                    KPJBA
049700070903      *
049800070903     c     *dtaara       define    §azute        azuteds
049900070903     c     *dtaara       define    §datiute      ddatiute
050000070903     c                   in(E)     *dtaara
050100070903    1c                   If        %error  or RSUT = *blanks
050200070903     c                   Clear                   Tibs34ds
050300070903     c                   Call      'TIBS34R'
050400070903     c                   Parm                    Tibs34ds
050500070903     c                   In        *dtaara
050600070903    1c                   EndIf
050700040527     C* GIRO UDATE
050800040527     C                   TIME                    WTIME            14 0          ORA & DATA
050900040527     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
051000040527     C                   Z-ADD     WDATE         G02DAT
051100040527     C                   Z-ADD     *ZERO         G02INV
051200040527     C                   MOVEL     *BLANKS       G02ERR
051300040527     C                   CALL      'XSRDA8'
051400040527     C                   PARM                    WLBDAT
051500040527     C                   Z-ADD     G02INV        DATEU             8 0          AAAA/MM/GG
051600930914     C*
051700980525     C* MI CALCOLO LA DATA MASSIMA DI RICHIESTA STAMPA CHE E' OGGI + IL
051800980525     C* PRIMO GIORNO NON FESTIVO
051900040527     C                   Z-ADD     g02inv        DATAPA            8 0
052000980525     C* ADDIZIONO 1 FINTANTO CHE NON TROVO IL PRIMO GIORNO LAVORATIVO
052100980525     C*
052200980525     C                   MOVEL     'F'           WFEST             1
052300980525     C     G02TGI        ADD       1             GIOTGI
052400980525     C                   ADD       1             G02TGI
052500980525     C*
052600980525    1C     WFEST         DOWNE     ' '
052700980525     C     G02TGI        ADD       1             GIOTGI
052800980525     C                   ADD       1             G02TGI
052900980525     C                   CALL      'XSRGI8'
053000980525     C                   PARM                    WGIDAT
053100980525     C*
053200980525     C                   Z-ADD     GIOINV        DATAPA            8 0
053300980525     C*
053400980525     C* IMPOSTO ANNO E MESE DELLA DATA TROVATA
053500980525     C                   Z-ADD     GIOINV        WDATA
053600980525     C                   CLEAR                   W0030
053700980525     C* CHAIN SUL CALENDARIO BARTOLINI
053800980525     C     KCLN          CHAIN     AZCLN01L                           30
053900980525     C   30              CLEAR                   CLNPOM
054000980525     C                   MOVEA     CLNPOM        K31
054100980525     C                   MOVEL     K31(WGG)      WFEST
054200980525    1C                   ENDDO
054300100222
054400100222      * Aggancio tabella VPO record DECO
054500100222     c                   Clear                   Dvpodeco
054600100222     c                   Clear                   Tibs02ds
054700100222     c                   Eval      T02Mod = 'C'
054800100222     c                   Eval      T02Sif = knsif
054900100222     c                   Eval      T02Cod = 'VPO'
055000100222     c                   Movel(p)  'DECO'        T02Ke1
055100100222     c                   Call      'TIBS02R'
055200100222     c                   Parm                    Kpjba
055300100222     c                   Parm                    Tibs02ds
055400100222     c                   If        T02Err = *Blanks
055500100222     c                   Movel     T02Uni        Dvpodeco
055600100222     c                   EndIf
055700100222     c
055800100222     c                   if        dateu>=§vpogma
055900100222     c                   seton                                        02
056000100222     c                   endif
056100100222
056200980525     C*
056300930914     C     KTAB          KLIST
056400070903     C                   KFLD                    CODUT             1 0
056500950123     C                   KFLD                    COD
056600100311     c                   KFLD                    KEY
056700980525     C* ACCESSO   AZCLN
056800980525     C     KCLN          KLIST
056900100312     C                   KFLD                    w0030
057000980525     C                   KFLD                    W0030             3 0
057100980525     C                   KFLD                    WAA               4 0
057200980525     C                   KFLD                    WMM               2 0
057300930914     C*
057400950123     C     *LIKE         DEFINE    TBLCOD        COD
057500950123     C     *LIKE         DEFINE    TBLKEY        KEY
057600950313     C     *LIKE         DEFINE    VIDLNA        ELAFIL
057700950123     C*
057800930914     C                   ENDSR
057900100311     C*---------------------------------------------------------------*
058000100311     C* Controllo lavori anche da elaborare                           *
058100100311     C*---------------------------------------------------------------*
058200100311     C     ContrBATCH    BEGSR
058300100311     c                   setoff                                       03
058400100311     c                   eval      wfine='N'
058500100311
058600100311     c     *cymd         movel     datasys       w0070             7 0
058700100311
058800100311     c                   dou       wfine='S'
058900100311     c
059000100311     c                   eval      wrkgetlista =
059100100311     c                             'select logkpj, logdac -
059200100311     c                              from kplog05l where -
059300100311     c                             lognmu=''' + knmus + ''' and logdaf=0 -
059400100311     c                             and logorf=0 '
059500100311     C/EXEC SQL
059600100311     C+ PREPARE s1 FROM :wrkgetlista
059700100311     C/END-EXEC
059800100311     C
059900100311     C/EXEC SQL
060000100311     C+ DECLARE A1 CURSOR FOR S1
060100100311     C/END-EXEC
060200100311     C
060300100311     C/EXEC SQL
060400100311     C+ OPEN a1
060500100311     C/END-EXEC
060600100311
060700100311     c                   clear                   conta             4 0
060800100311      *?Leggo il file
060900100311     c                   do        *hival
061000100311     C/EXEC SQL
061100100311     C+ FETCH NEXT FROM a1 INTO :logkpj, :logdac
061200100311     C/END-EXEC
061300100311
061400100311     c                   select
061500100311
061600100311     c                   when      sqlcod = 100
061700100311     c                   leave
061800100311     c                   when      sqlcod < 0
061900100311     c                   seton                                        H1
062000100311     c                   leave
062100100311     c                   other
062200100311
062300100311     c                   if        %subst(logkpj:135:4)='LR78'
062400100311     c                             and logdac=w0070
062500100311     c                   eval      conta=conta+1
062600100311     c                   endif
062700100311
062800100311     c                   endsl
062900100311
063000100311     c                   enddo
063100100311
063200100311     C/EXEC SQL
063300100311     C+ CLOSE a1
063400100311     C/END-EXEC
063500100311
063600100311     c                   if        conta>0
063700100311     c                   seton                                        03
063800100311     c                   else
063900100311     c                   setoff                                       03
064000100311     c                   endif
064100100311     c
064200100311     c                   exfmt     lrm1w02
064300100311     c
064400100311     C                   IF        *INKE
064500100311     C                   eval      wfine='N'
064600100311     c                   else
064700100311     C                   eval      wfine='S'
064800100311     c                   endif
064900100311
065000100311     c                   enddo
065100100311     c
065200100311     c
065300100311     C                   ENDSR
065400070903**
065500070903Utente non abilitato alla Stampa delle C.A.                                   27
