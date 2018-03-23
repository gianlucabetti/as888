000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      **************************************************************************
000300941228      *  Nome programma:  FNLRE6R (deriva da FLFR63R)
000400000000      *  Descrizione   :  Stampa statistica arrivi.
000500000000      *                   Primo programma di esecuzione: elaborazione dati e
000600090929      *                   scrittura del file statistiche FISAR00F.
000700000000      *                   Chiamata al pgm di stampa.
000800000000      *  Data creazione:  07 FEB 1994
000900000000      **************************************************************************
001000950214      *  ??PASSAGGIO PARAMETRI:?
001100950214      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
001200950214      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
001300950214      *  DS1PGM   NOME PROGRAMMA
001400950214      *  DS1FPG   FLAG PROGRAMMA
001500950214      *  DS1FEL   FILIALE ELABORATORE (AREA CHE SI VUOLE STAMPARE)
001600950313      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA (E' LA LNA)
001700950214      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
001800950214      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
001900950214      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
002000950214      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
002100950404      *  DS1RIS   'R'=RISTAMPA, 'S'=ELABOR.+STAMPA, ' '=SOLO ELABOR.
002200950216      *  DS1FL1   STATISTICA SELEZIONATA  ( ' '=TUTTE )
002300950214      **************************************************************************
002400000000      * ?Indicatori utilizzati:?
002500000000      *  30       Utilizzato per alcuni cicli
002600000000      *  31       Utilizzato per alcuni cicli
002700000000      *  32       Utilizzato per alcuni cicli
002800000000      *  33       Utilizzato per alcuni cicli
002900960926      *  97       Di comodo
003000000000      *  99       Generico, utilizzabile a piacere
003100000000      **************************************************************************
003200000000      *  Note x il programmatore:
003300950214      *  . alcuni campi di ARB vengono alterati con dei calcoli,
003400950214      *    quindi non mettere tale file di output
003500000000      **************************************************************************
003600000000     FAZORG01L  IF   E           K DISK    USROPN
003700990521     FFNANM04L  IF   E           K DISK    USROPN
003800941228     FFNARB01L  IF   E           K DISK    USROPN
003900941228     FFNARB29L  IF   E           K DISK    USROPN
004000941228     F                                     RENAME(FNARB000:FNARB029)
004100090309     FFnART27L  IF   E           K DISK    USROPN
004200070117     FFNBRV09L  IF   E           K DISK
004300070117     FFNBRV07L  IF   E           K DISK
004400070117     F                                     RENAME(FNBRV000:FNBRV007)
004500020823     FFNBTP11L  IF   E           K DISK    USROPN
004600020823     FFNBTP19L  IF   E           K DISK    USROPN
004700020823     F                                     RENAME(FNBTP000:FNBTP019)
004800020823     FFNBTT37L  IF   E           K DISK    USROPN
004900020823     F                                     RENAME(FNBTT000:FNBTT037)
005000020823     FFNBTT39L  IF   E           K DISK    USROPN
005100941229     FFNFVA02L  IF   E           K DISK    USROPN
005200000504     FFNFWA01L  IF   E           K DISK
005300020912     FFNFVV02L  IF   E           K DISK
005400090929     FFISAR01L  UF A E           K DISK
005500090929     FFISAR03L  UF   E           K DISK
005600090929     F                                     RENAME(FISAR000:FISAR003)
005700070117     FFNBRV02L  IF   E           K DISK    USROPN
005800070117     F                                     RENAME(FNBRV000:FISPP000)
005900021218     f                                     prefix(SPP:3)
006000000000     FTABEL00F  IF   E           K DISK
006100021016     fTntbe01l  Uf   e           k Disk
006200040128     FAZCAE02L  IF   E           K DISK
006300020912
006400020912     d xx              s              3  0                                      contatore
006500000000      *
006600971118     D ORG             S              3  0 DIM(500)                             COD.FIL. DA AZORG
006700971118     D OTA             S              3  0 DIM(500)                             TERM.ARR.DA AZORG
006800971118     D OTP             S              3  0 DIM(500)                             TERM.PAR.DA AZORG
006900040127     D LEL             S              3  0 DIM(60)                              P.O.POSTE     ORG
007000040128     D LELa            S              3  0 DIM(60)                              P.O.POSTE     ORG
007100040128     D LEl2            S              3  0 DIM(20)                              P.O.POSTE     ORG
007200040205     ddslel3           ds                  occurs(20)
007300040205     D LEl3                           3    DIM(20)                              P.O.POSTE     ORG
007400000000      * TOTALI TERMINAL DI PARTENZA
007500000000     D TFP             S              3  0 DIM(399)                             TERMINAL PARTENZA
007600000000     D CLL             S              7  0 DIM(399)                             TOT COLLI
007700000000     D P88             S              7  0 DIM(399)                             PIST.88 NON ARRIV
007800000000     D DIS             S              7  0 DIM(399)                             DISGUIDI
007900000000     D PAR             S              7  0 DIM(399)                             PARTITI NON SPUNT
008000940607     D F35             S              9  1 DIM(399)                             FINO  35 Q.LI
008100000000     D O35             S              9  1 DIM(399)                             OLTRE 35 Q.LI
008200000000     D PTA             S              9  1 DIM(399)                             PESO TASSABIL
008300000000     D VOL             S              9  4 DIM(399)                             VOLUME
008400090929     D FN              S              7  0 DIM(399)                             FRANCO C+D
008500090929     D FE              S              7  0 DIM(399)                             FRANCO "E"
008600090929     D FH              S              7  0 DIM(399)                             FRANCO "H"
008700090929     D AN              S              7  0 DIM(399)                             ASSEGN C+D
008800090929     D AE              S              7  0 DIM(399)                             ASSEGN "E"
008900090929     D AH              S              7  0 DIM(399)                             ASSEGN "H"
009000000620      * TOTALI LINEE DI ARRIVO TER PAR NO POSTE
009100000000     D LNA             S              3  0 DIM(29)                              LINEE PART
009200000000     D CLLA            S              7  0 DIM(29)                              TOT COLLI
009300000000     D P88A            S              7  0 DIM(29)                              PIST.88 NON ARRIV
009400000000     D DISA            S              7  0 DIM(29)                              DISGUIDI
009500000000     D PARA            S              7  0 DIM(29)                              PARTITI NON SPUNT
009600000000     D F35A            S              9  1 DIM(29)                              FINO  35 Q.LI
009700000000     D O35A            S              9  1 DIM(29)                              OLTRE 35 Q.LI
009800000000     D PTAA            S              9  1 DIM(29)                              PESO TASSABIL
009900000000     D VOLA            S              9  4 DIM(29)                              VOLUME
010000090929     D FNA             S              7  0 DIM(29)                              FRANCO C+D
010100090929     D FEA             S              7  0 DIM(29)                              FRANCO "E"
010200090929     D FHA             S              7  0 DIM(29)                              FRANCO "H"
010300090929     D ANA             S              7  0 DIM(29)                              ASSEGN C+D
010400090929     D AEA             S              7  0 DIM(29)                              ASSEGN "E"
010500090929     D AHA             S              7  0 DIM(29)                              ASSEGN "H"
010600000000      * TOTALI TRANSITI TERMINAL DI PARTENZA
010700000000     D TFPT            S              3  0 DIM(399)                             TERMINAL PARTENZA
010800000000     D CLLT            S              7  0 DIM(399)                             TOT COLLI
010900000000     D P88T            S              7  0 DIM(399)                             PIST.88 NON ARRIV
011000000000     D PART            S              7  0 DIM(399)                             PARTITI NON SPUNT
011100000000     D F35T            S              9  1 DIM(399)                             FINO  35 Q.LI
011200000000     D O35T            S              9  1 DIM(399)                             OLTRE 35 Q.LI
011300000000     D PTAT            S              9  1 DIM(399)                             PESO TASSABIL
011400000000     D VOLT            S              9  4 DIM(399)                             VOLUME
011500090929     D FNT             S              7  0 DIM(399)                             FRANCO C+D
011600090929     D FET             S              7  0 DIM(399)                             FRANCO "E"
011700090929     D FHT             S              7  0 DIM(399)                             FRANCO "H"
011800090929     D ANT             S              7  0 DIM(399)                             ASSEGN C+D
011900090929     D AET             S              7  0 DIM(399)                             ASSEGN "E"
012000090929     D AHT             S              7  0 DIM(399)                             ASSEGN "H"
012100000000      * LINEE DI ARRIVO DI CUI SOLO TERMINAL
012200000000     D LNAR            S              3  0 DIM(50)                              LINEA ARRIVO
012300000000     D CLLR            S              7  0 DIM(50)                              TOT COLLI
012400000000     D P88R            S              7  0 DIM(50)                              PIST.88 NON ARRIV
012500000000     D PARR            S              7  0 DIM(50)                              PARTITI NON SPUNT
012600000000      *
012700970605     D NPS             S              2  0 DIM(101)                             PIST.AUTOMAT.
012800980402     D* CARICO I COD PISTOLA FASULLI DA REIMPOSTARE
012900980402     D NPF             S              2  0 DIM(99)                              NPS CON FSL=F
013000970606     D TTR             S              1    DIM(10)                              TIPTRAINO DEFL
013100021016
013200021016     d kTbeCod         s                   Like(TbeCod)
013300021016     d kTbeKe1         s                   Like(TbeKe1)
013400021218      *
013500021218     d $Volum          s             15  5 inz
013600021218     d $Peso           s             15  5 inz
013700030213     d*** WsiVOL          s              1    inz
013800030213     d*** WsiPES          s              1    inz
013900021220     d Wvolmed         s                   like($Volum) inz
014000021220     d Wpesmed         s                   like($Peso)  inz
014100021016
014200950214     D DSLS01        E DS
014300941229      * DS x la schiera delle linee di arrivo (del file FNFVA)
014400000000     D                 DS
014500000000     D  FVALNA                 1      3  0
014600000000     D  FVAFFV                 4    243
014700000000     D  FVAFF2               244    450
014800000504     D  FWAFF3               451    690
014900000504     D  FWAFF4               691    900
015000000504     D*                                     901 903 VUOTO
015100000504     D  FFV                    1    903
015200000504     D                                     DIM(301)                             LINEE FOGLIO -SPP
015300941229      * DS x la schiera delle filiali di scarico (del file FNFVA)
015400000000     D                 DS
015500000000     D  LNA2                   1      3  0
015600000000     D  FVAFLP                 4    243
015700000000     D  FVAFL2               244    450
015800000504     D  FWAFL3               451    690
015900000504     D  FWAFL4               691    900
016000021218     D**VUOTO2               901    903
016100000504     D  FSP                    1    903
016200000504     D                                     DIM(301)                             LINEE SCARICO-SPP
016300941229      * $BOLLA contiene i campi chiave per la chain a FNART01L
016400000000     D                 DS
016500941229     D  $BOLLA                 1     16
016600941229     D  ARTAAS                 1      4  0
016700941229     D  ARTLNP                 5      7  0
016800941229     D  ARTNRS                 8      9  0
016900941229     D  ARTNSP                10     16  0
017000020823     D  ARTFLP                17     19  0
017100020823      * $BOLLT contiene i campi chiave per la chain a FNBTP11L
017200000000     D                 DS
017300941229     D  $BOLLT                 1     16
017400941229     D  BTTAAS                 1      4  0
017500941229     D  BTTLNP                 5      7  0
017600941229     D  BTTNRS                 8      9  0
017700941229     D  BTTNSP                10     16  0
017800020823     D  BTTFLP                17     19  0
017900040923     D WLBDAT          DS                  INZ
018000040923     D  G02DAT                 1      8  0
018100040923     D  G02INV                 9     16  0
018200040923     D  G02ERR                17     17
018300040923     D  G02TGI                18     22  0
018400000619     D OG143         E DS
018500000619     D DS3A          E DS
018600970605     D DS7J          E DS
018700970606     D DSTV          E DS
018800000000     D DS3I          E DS
018900941228     D DSQT1         E DS
019000000000     D KPJBA         E DS
019100000000     D CNCR80        E DS
019200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
019300040129     D SFU             S              3  0 DIM(30)                              L6 NO GESTITA
019400040129     D SFA             S              3  0 DIM(30)                              L6 NO GESTITA
019500000000      * DSUL06 serve per call a TRUL06R
019600941230     D DSUL06        E DS                  EXTNAME(TRUL06DS)
019700970502     D  L1                     1     90  0
019800000000     D                                     DIM(30)                              FILIALI GESTITE
019900040127      * Parametri per richiamo a TRUL09R
020000040127     d Trul09ds      e ds
020100040127     d  T09                   19    108  0 dim(30)
020200970502      * DSLV53 serve per call a FNLV53R - REPERISCE DATI FOGLIO
020300970502     D DSLV53        E DS                  EXTNAME(FNLV53DS)
020400970606     D* DS PER DSLV55R - PASSAGGIO PARAMETRI
020500970606     D DSLV55        E DS                  EXTNAME(FNLV55DS)
020600021016
020700021016      * Ds date statistiche
020800021016     d dSdf          e ds
020900090310     d
021000090310     d wrkgetlista     s           4096    varying
021100021016
021200000000      **************************************************************************
021300000000     C     *ENTRY        PLIST
021400000000     C                   PARM                    KPJBA
021500950214     C                   MOVEL     KPJBU         DSLS01
021600000000      *
021700000000     C     TABELK        KLIST                                                  TABEL00F
021800000000     C                   KFLD                    TBLKUT
021900000000     C                   KFLD                    TBLCOD
022000000000     C                   KFLD                    TBLKEY
022100000000     C     KTAB2         KLIST                                                  TABEL00F
022200000000     C                   KFLD                    TBLKUT
022300000000     C                   KFLD                    TBLCOD
022400000000     C     KARB          KLIST
022500000000     C                   KFLD                    VIDLNA            3 0
022600950214     C                   KFLD                    DS1ADA
022700020912     C     FNFVVK        KLIST                                                  FNFVV02L
022800000000     C                   KFLD                    WNPG
022900000000     C                   KFLD                    WDFV
023000020912     c                   Kfld                    Wfgs
023100941229     C     FNFVAK        KLIST                                                  FNFVA02L
023200000000     C                   KFLD                    FVANPG
023300000000     C                   KFLD                    FVANFA
023400020917     c                   Kfld                    FvaLai
023500000504     C     KFWA          KLIST                                                  FNFVA02L
023600000504     C                   KFLD                    FVALNP
023700000504     C                   KFLD                    FVANFV
023800000504     C                   KFLD                    FVALAI
023900070117     C     FNSPPK        KLIST                                                  FNBRV02L
024000941229     C                   KFLD                    SPPNPG
024100000000     C                   KFLD                    SPPNFV
024200941229     C                   KFLD                    SPPFGS
024300000000     C                   KFLD                    SPPLNA
024400070117     C     FNSP1K        KLIST                                                  FNBRV02L
024500950419     C                   KFLD                    SPPNPG
024600950419     C                   KFLD                    SPPNFV
024700950419     C                   KFLD                    SPPFGS
024800950419     C                   KFLD                    SPPLNA
024900950419     C                   KFLD                    SPPLNP
025000941228     C     FNARBK        KLIST                                                  FNARB01L
025100000000     C                   KFLD                    ARTAAS
025200000000     C                   KFLD                    ARTLNP
025300000000     C                   KFLD                    ARTNRS
025400000000     C                   KFLD                    ARTNSP
025500941229     C     FNARTK        KLIST                                                  FNART27L
025600000000     C                   KFLD                    ARTFLS
025700000000     C                   KFLD                    ARTLNA
025800000000     C                   KFLD                    ARTNRS
025900941230     C                   KFLD                    ARTNSC
026000020823     C     FNARTK1       KLIST                                                  FNART27L
026100040130     C***                KFLD                    SIMFEL
026200040130     C                   KFLD                    fvalai
026300020823     C                   KFLD                    ARTFLS
026400020823     C                   KFLD                    ARTLNA
026500020823     C                   KFLD                    ARTNRS
026600020823     C                   KFLD                    ARTNSC
026700020823     C     FLBTPK        KLIST                                                  FNBTP11L
026800020823     C                   KFLD                    BTTFLP
026900020823     C                   KFLD                    BTTAAS
027000941229     C                   KFLD                    BTTLNP
027100941229     C                   KFLD                    BTTNRS
027200941229     C                   KFLD                    BTTNSP
027300090929     C     FISARK        KLIST                                                  FISAR01L
027400000000     C                   KFLD                    SARFLE
027500000000     C                   KFLD                    SARTFA
027600000000     C                   KFLD                    SARTFP
027700000000     C                   KFLD                    SARLNA
027800000000     C                   KFLD                    SARTRE
027900000000     C                   KFLD                    SARDRE
028000090929     C     FLSA3K        KLIST                                                  FISAR03L
028100000000     C                   KFLD                    SARFLE
028200000000     C                   KFLD                    SARTFA
028300000000     C                   KFLD                    SARTRE
028400000000     C                   KFLD                    SARDRE
028500990521     C     FNANMK        KLIST                                                  FNANM04L
028600000000     C                   KFLD                    ANMCAA
028700000000     C                   KFLD                    ANMDAO
028800990521     C     FNAN4K        KLIST                                                  FNANM04L
028900000000     C                   KFLD                    ANMCAA
029000000000     C                   KFLD                    ANMDAO
029100000000     C                   KFLD                    ANMLNA
029200070117     C     FNBRVK        KLIST                                                  FNBRV09L
029300000000     C                   KFLD                    BRVNPG
029400000000     C                   KFLD                    BRVLNP
029500000000     C                   KFLD                    BRVLNA
029600000000     C                   KFLD                    BRVNRS
029700000000     C                   KFLD                    BRVNSC
029800070117     C     KBRV9         KLIST                                                  FNBRV09L
029900970513     C                   KFLD                    BRVNPG
030000970513     C                   KFLD                    BRVLNP
030100970513     C                   KFLD                    BRVLNA
030200970513     C                   KFLD                    BRVNRS
030300970513     C                   KFLD                    BRVNSC
030400070117     C     KBRV          KLIST                                                  FNBRV07L
030500970502     C                   KFLD                    BRVLNP
030600970502     C                   KFLD                    BRVLNA
030700970502     C                   KFLD                    BRVNRS
030800970502     C                   KFLD                    BRVNSC
030900020823     C     KDTA          KLIST
031000020823     C                   KFLD                    SIMFEL            3 0
031100020823     C                   KFLD                    DS1ADA
031200021016
031300021016     c     Ktbe          Klist
031400021016     c                   Kfld                    kTbeCod
031500021016     c                   Kfld                    kTbeKe1
031600040128     C*
031700090929     C     KCAE2         KLIST                                                  FISAR01L
031800040128     C                   KFLD                    KEPA
031900040128     C                   KFLD                    lel(c)
032000040128     C                   MOVEL     'A'           KEPA
032100000000      **************************************************************************
032200000000      *              M A I N      L I N E
032300000000      **************************************************************************
032400000000      *
032500000000      * Imposto il nr. di elementi delle schiere (per i cicli a venire)
032600000000     C                   Z-ADD     399           $MAX              3 0          ELEMENTI SCHIER
032700000000      *
032800000000     C                   OPEN      AZORG01L
032900941228     C                   OPEN      FNARB01L                                     BOLLE
033000941228     C                   OPEN      FNARB29L                                     BOLLE
033100090309     C                   OPEN      FnART27L                                     COLLI
033200020823     C                   OPEN      FNBTP11L                                     BOLLE TRANSITO
033300020823     C                   OPEN      FNBTP19L                                     BOLLE TRANSITO
033400020823     C                   OPEN      FNBTT37L                                     COLLI TRANSITO
033500020823     C                   OPEN      FNBTT39L                                     COLLI TRANSITO
033600070117     C                   OPEN      FNBRV02L                                     SPUNTE
033700941229     C                   OPEN      FNFVA02L
033800990521     C                   OPEN      FNANM04L                                     ANOMALIE
033900040923     c*
034000040923     C* DETERMINO DATA DI ESECUZIONE
034100040923     C                   TIME                    WTIME            14 0
034200040923     c*
034300040923     C                   CLEAR                   WLBDAT
034400040923     C                   MOVE      Wtime         G02DAT
034500040923     C                   CALL      'XSRDA8'
034600040923     C                   PARM                    WLBDAT
034700040923     C                   Z-ADD     G02INV        dateu             8 0
034800000000      *
034900941230      * Riempo alcune schiere
035000000000     C                   EXSR      CARTAB
035100000000      *
035200000000     C                   Z-ADD     *ZERO         $DTAAR            1 0          SALVA DTAARA
035300000000      *
035400000000      * Elaborazione archivi
035500000000     C                   EXSR      LEGGI
035600970605     C*
035700090929     C* Scrittura record in FISAR per linee dell'area o di cui solo
035800970605     C* terminal non elaborate (serve per il report di sede)
035900040130     C***                EXSR      WRTSAR
036000000000      *
036100000000      * Se ho elaborato qualcosa di valido aggiorno la data ultima elaborazione
036200000000     C     $DTAAR        IFGT      *ZERO
036300021016
036400021016     c                   Eval      kTbeCod = 'SDF'
036500021016     c                   Movel(p)  Simfel        kTbeKe1
036600021016     c     Ktbe          Chain     Tntbe01l
036700021016     c                   If        %Found(Tntbe01l)
036800021016     c                   Movel     TbeUni        dSdf
036900021016     c                   If        §SdfArrP = *Zeros or §SdfArrP > Ds1Ada
037000021016     c                   Z-Add     Ds1Ada        §SdfArrP
037100021016     c                   EndIf
037200021016     c                   If        §SdfArrU < Ds1Aal
037300021016     c                   Z-Add     Ds1Aal        §SdfArrU
037400021016     c                   EndIf
037500021016     c                   Movel     dSdf          TbeUni
037600021016     c                   Update    Tntbe000
037700021016     c                   EndIf
037800000000     C                   ENDIF
037900970505     C* CHIUSURA FILE
038000970505     C                   CLEAR                   DSLV53
038100970505     C                   MOVEL     'C'           D53TLA
038200970505     C                   CALL      'FNLV53R'
038300970505     C                   PARM                    DSLV53
038400970606     C* CHIUSURA FILE
038500970606     C                   CLEAR                   DSLV55
038600970606     C                   MOVEL     'C'           D55TLA
038700970606     C                   CALL      'FNLV55R'
038800970606     C                   PARM                    DSLV55
038900000000      *
039000000000     C                   SETON                                        LR
039100000000      **************************************************************************
039200000000      * LETTURA BOLLE PER CREAZIONE STAMPA
039300000000      **************************************************************************
039400000000     C     LEGGI         BEGSR
039500000000      *
039600000000      * Cancello dall'archivio eventuali record nella stessa data,solo se stampa
039700090929     C     SIMFEL        SETLL     FISAR01L
039800000000     C                   DO        *HIVAL
039900090929     C     SIMFEL        READE     FISAR01L                               31
040000990507     C  N31SARDRE        IFGE      DS1ADA
040100990507     C     SARDRE        ANDLE     DS1AAL
040200090929     C                   DELETE    FISAR000
040300000000     C                   ENDIF
040400000000     C  N31              ENDDO
040500000000      *
040600000000      * ----------------?  FASE 1: ELABORAZIONE BOLLE  ?------------------------
040700000000     C                   Z-ADD     1             C                 3 0
040800000000     C                   Z-ADD     *ZERO         $A                3 0          CONTAT. LNA
040900000000     C                   Z-ADD     *ZERO         $B                3 0          CONTAT. TFP
041000000000     C                   MOVE      *BLANKS       $CBLPR                         PRECEDENTE
041100000000     C                   Z-ADD     *ZERO         $TFAPR                         PRECEDENTE
041200000000     C                   Z-ADD     *ZERO         $PRIMO            1 0          PRIMO GIRO
041300000000      *
041400040127      * Eseguo i cicli di lettura su FNARB solo per le linee da elaborare;
041500000000      * evito cosi' di leggere, inutilmente, tutti i record bolle
041600040127     C     lel(c)        DOWNE     0                                            TABELLA '£1'
041700040127     C                   MOVEL     lel(c)        VIDLNA
041800000000      *
041900941228      * N.B.: il file FNARB29L e' ora ordinato anche come terminal di arrivo
042000000000      *       (terzo campo chiave); devo tenere conto del codice TFA poiche'
042100090929      *       dovro' scriverlo nel file statistiche (FISAR00F).
042200000000      *
042300941228      * Utilizzo questa tecnica:  leggo FNARB e riempo le schiere; quando
042400090929      * cambia TFA salvo le schiere nel file statistiche (FISAR) e le pulisco,
042500000000      * quindi continuo le letture ririempiendo le schiere. E cosi' via.
042600970502      *
042700970502     C     KARB          SETLL     FNARB29L
042800970502     C     KARB          READE     FNARB29L                               31
042900970502     C     *IN31         DOWEQ     *OFF
043000000000      *
043100000000     C                   EXSR      ELABB                                        ELABORAZ. BOLLA
043200000000      *
043300970502     C     KARB          READE     FNARB29L                               31
043400970502     C                   ENDDO                                                  *HIVAL
043500000000      *
043600000000     C                   ADD       1             C
043700000000     C                   ENDDO                                                  L1,C <> 0
043800000000      *
043900000000     C                   EXSR      SCASCH                                       SCA.SCHIERE BOL
044000000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
044100941228     C                   CLOSE     FNARB29L                                     FILE BOLLE
044200000000      *
044300000000      * ----------------?  FASE 2:  ELABORAZIONE COLLI  ?-----------------------
044400000000     C                   Z-ADD     1             C
044500000000     C                   MOVE      *BLANKS       $CBLPR                         PRECEDENTE
044600000000     C                   Z-ADD     *ZERO         $TFAPR                         PRECEDENTE
044700000000     C                   Z-ADD     *ZERO         $B                             CONTAT. TFP
044800000000     C                   Z-ADD     *ZERO         $PRIMO                         PRIMO GIRO
044900090310     C                   MOVE      *ZERO         $BOLPR                         PRECEDENTE
045000090310     C                   MOVE      *BLANKS       $OKBOL            1            SE BOLLA OK
045100090309     c* Preparo SQL per leggere i colli arrivati per linea di arrivo
045200090310     c                   exsr      PrepaSQL
045300090310      *?Leggo il file
045400090310     c                   do        *hival
045500090310     C/EXEC SQL
045600090310     C+ FETCH NEXT FROM a1 INTO :artaas, :artlnp, :artnrs, :artnsp, :artlna,
045700090310     C+                         :artflp
045800090310     C/END-EXEC
045900090310
046000090310     c                   select
046100090310
046200090310     c                   when      sqlcod = 100
046300090310     c                   leave
046400090310     c
046500090310     c                   when      sqlcod < 0
046600090310     c                   seton                                        H1
046700090310     c                   eval      *InLr = *On
046800090310     c                   return
046900090310
047000090310     c                   other
047100090310      *
047200090310      * Aggancio la testata della bolla (FNARB), se <> dalla precedente
047300090310     C     $BOLLA        IFNE      $BOLPR                                       <> PRECEDENTE
047400090310     C     FNARBK        CHAIN     FNARB01L                           99
047500090310     C   99              MOVE      'N'           $OKBOL                         NO OK BOLLA
047600090310     C  N99              MOVE      'S'           $OKBOL                         SI OK BOLLA
047700090310     C  N99              EXSR      ELABC1                                       CALCOLI BOLLA
047800090310     C                   MOVE      $BOLLA        $BOLPR
047900090310     C                   ENDIF
048000090310      *
048100090310     C     $OKBOL        IFEQ      'S'                                          SE BOLLA OK
048200090310     C                   EXSR      ELABC                                        CALCOLI SU COLL
048300090310     C                   ENDIF
048400090310
048500090310     c                   endsl
048600090310
048700090310     c                   enddo
048800090310
048900090310     C/EXEC SQL
049000090310     C+ CLOSE a1
049100090310     C/END-EXEC
049200090310
049300090929      * Termianti tutti i giri scarico su FISAR le ultime schiere in memoria
049400000000     C                   EXSR      SCASCH                                       SCA.SCHIERE COL
049500000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
049600941228     C                   CLOSE     FNARB01L                                     FILE BOLLE
049700000000      *
049800000000      * ----------------?  FASE 3: ELABORAZIONE TRANSITI  ?---------------------
049900000000      *
050000000000     C                   Z-ADD     *ZERO         $B                             CONTATORE TFP
050100000000     C                   MOVE      *BLANKS       $CBLPR                         PRECEDENTE
050200000406     C                   EXSR      IMPOT                                        SETLL
050300000000      *
050400020823     C     KDTA          READE     FNBTP19L                               31
050500000000     C     *IN31         DOWEQ     '0'
050600000000      *
050700000000     C                   EXSR      ELABT
050800000000      *
050900020823     C     KDTA          READE     FNBTP19L                               31
051000000000     C                   ENDDO                                                  *IN30DOWEQ'0'
051100000000      *
051200090929      * Terimanti tutti i giri scarico su FISAR le ultime schiere in memoria
051300000000     C                   EXSR      SCASCT                                       SCARICA SCHIERE
051400000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
051500020823     C                   CLOSE     FNBTP19L                                     FILE BOLLE TRAN
051600000000      *
051700000000      * ----------------?  FASE 4:  COLLI IN TRANSITO  ?------------------------
051800000000      *
051900000000     C                   MOVE      *ZERO         $BOLPR                         PRECEDENTE
052000000000     C                   MOVE      *BLANKS       $CBLPR                         PRECEDENTE
052100000000     C                   MOVE      *BLANKS       $OKBOL                         SE BOLLA OK
052200980708     C                   Z-ADD     *ZERO         $B                             CONTAT. TFP
052300020823     C     KDTA          SETLL     FNBTT39L
052400020823     C     KDTA          READE     FNBTT39L                               31
052500000000     C     *IN31         DOWEQ     '0'
052600000000      *
052700000000      * Aggancio la testata della bolla (FLBTP), se <> dalla precedente
052800000000     C     $BOLLT        IFNE      $BOLPR                                       <> PRECEDENTE
052900020823     C     FLBTPK        CHAIN     FNBTP11L                           99
053000000000     C   99              MOVE      'N'           $OKBOL                         NO OK BOLLA
053100000000     C  N99              MOVE      'S'           $OKBOL                         SI OK BOLLA
053200000000     C  N99              EXSR      ELACT1
053300000000     C                   MOVE      $BOLLT        $BOLPR                         PRECEDENTE
053400000000     C                   ENDIF
053500000000      *
053600000000     C     $OKBOL        IFEQ      'S'                                          SE BOLLA OK
053700000000     C                   EXSR      ELABCT
053800000000     C                   ENDIF
053900000000      *
054000020823     C     KDTA          READE     FNBTT39L                               31
054100000000     C                   ENDDO
054200000000      *
054300090929      * Terimanti tutti i giri scarico su FISAR le ultime schiere in memoria
054400000000     C                   EXSR      SCASCT                                       SCARICA SCHIERE
054500000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
054600020823     C                   CLOSE     FNBTT39L                                     FILE COLLI TRAN
054700020823     C                   CLOSE     FNBTP11L                                     FILE BOLLE TRAN
054800000000      *
054900000000      * --? FASE 5:  COLLI NON SPUNTATI IN PARTENZA E NON ARRIVATI (PIST.88) ?--
055000000000     C                   EXSR      NONARR                                       NON ARRIVATI
055100020823     C                   CLOSE     FNBTT37L                                     COLLI TRANSITO
055200000000      *
055300000000      * -------------?  FASE 5bis:  LNA DI CUI SOLO TERMINAL  ?-----------------
055400000000      * E' il completamento della fase 5, poiche' i record (della statistica) di
055500000000      * tipo 'R' vengono creati nella fase 5
055600000000      *
055700000000      * Rileggo dalla statistica tutti i record di tipo 'R'=DI CUI SOLO TERMINAL
055800000000      * per verificare se vi sono anche delle anomalie da aggiungervi
055900000000     C                   Z-ADD     SIMFEL        SARFLE
056000000000     C                   Z-ADD     SIMFEL        SARTFA
056100000000     C                   MOVE      'R'           SARTRE                         'R'=SOLO TERMIN.
056200950214     C                   Z-ADD     DS1ADA        SARDRE                         DATA
056300090929     C     FLSA3K        SETLL     FISAR03L
056400000000     C                   DO        *HIVAL
056500090929     C     FLSA3K        READE     FISAR03L                               31
056600000000     C  N31              DO
056700950418      *
056800950418      *? FASE 5bis  -  Parte 1  ·····  INCLUDO LE ANOMALIE ?
056900000000      *
057000000000      * Devo cercare fra le anomalie di tipo "50"
057100000000     C                   Z-ADD     50            ANMCAA                         50=ARRIV.NO PAR
057200950214     C                   Z-ADD     DS1ADA        ANMDAO                         DATA INIZIALE
057300000000     C                   Z-ADD     SARLNA        ANMLNA                         LINEA ARRIVO
057400000000      *
057500990521     C     FNAN4K        SETLL     FNANM04L
057600000000     C                   DO        *HIVAL
057700990521     C     FNAN4K        READE     FNANM04L                               32
057800000000     C  N32              ADD       1             SARCLL
057900990521     C  N32              ENDDO                                                  *HIVAL FNANM04L
058000000000      *
058100000000      * sommo anche le anomalie di tipo "160"
058200000000     C                   Z-ADD     160           ANMCAA                         160=ARR PART AL
058300000000      *
058400990521     C     FNAN4K        SETLL     FNANM04L
058500000000     C                   DO        *HIVAL
058600990521     C     FNAN4K        READE     FNANM04L                               32
058700000000     C  N32              ADD       1             SARCLL
058800990521     C  N32              ENDDO                                                  *HIVAL FNANM04L
058900950418      *
059000950418      *? FASE 5bis  -  Parte 2  ·····  I LOCALI (PUB E MENSE ALLA MODA) ?
059100950418      *
059200950419      * Per la filiale in esame (SARLNA) leggo i fogli (con le relative
059300950419      * spunte) di categ. 2 e 6 con data compresa nel periodo in esame
059400950418      *
059500950419      * Eseguo due volte l'operazione, una per ogni categoria
059600950419     C                   Z-ADD     2             WNPG                           2=ARRIVI
059700950418     C                   DO        2
059800020912      * devo ciclare per tutti i p.o. dell'area
059900020918     c                   Clear                   xx
060000020912     c                   do        30            xx
060100020912     c                   If        L1(xx) = *Zeros
060200020912     c                   Leave
060300020912     c                   EndIf
060400950419     C                   Z-ADD     DS1ADA        WDFV                           DATA INIZIALE
060500020912     C**!!!              Z-ADD     SIMFEL        WFLE                           FIL.ELABORATORE
060600020912     c                   Z-Add     L1(xx)        Wfgs
060700020912     C     FNFVVK        SETLL     FNFVV02L
060800020912     C     FNFVVK        READE     FNFVV02L                               33
060900950419     C     *IN33         DOWEQ     *OFF
061000950419      *
061100950419      * Leggo le spunte del foglio, ma solo quelle partenti dall'area
061200950419     C                   Z-ADD     FVVNPG        SPPNPG
061300950419     C                   Z-ADD     FVVNFV        SPPNFV
061400950419     C                   Z-ADD     FVVFGS        SPPFGS
061500950419     C                   Z-ADD     SARLNA        SPPLNA                         LINEA ARRIVO
061600950419     C                   Z-ADD     1             $B
061700950419     C     L1($B)        DOWGT     *ZERO
061800950419     C                   Z-ADD     L1($B)        SPPLNP                         LINEA PARTENZA
061900950419      *
062000950419      * Uso £1 per leggere meno spunte possibili
062100070117     C     FNSP1K        SETLL     FNBRV02L
062200070117     C     FNSP1K        READE     FNBRV02L                               32
062300950419     C     *IN32         DOWEQ     *OFF
062400950419     C                   ADD       1             SARCLL
062500070117     C     FNSP1K        READE     FNBRV02L                               32
062600950419     C                   ENDDO
062700950419     C*
062800950419     C                   ADD       1             $B                             ALTRA FIL.
062900950419     C                   ENDDO
063000950419      *
063100020912     C     FNFVVK        READE     FNFVV02L                               33
063200950419     C                   ENDDO
063300020912     c                   EndDo
063400950418      *
063500950419     C                   Z-ADD     6             WNPG                           6=ARRIVI 2° LVL
063600950418     C                   ENDDO
063700950418      *?                                                                       ?
063800000000      *
063900000000     C                   EXCEPT    AGGSAR
064000000000     C                   ENDDO
064100090929     C  N31              ENDDO                                                  *HIVAL FISAR03L
064200000000      *
064300000000      * ----------------?  FASE 6:  COLLI IN DISGUIDO  ?------------------------
064400000000      *
064500000000     C                   Z-ADD     *ZERO         $B                             CONTATORE TFP
064600971126     C                   Z-ADD     SIMFEL        $TFAPR                         PRECEDENTE
064700000000     C                   Z-ADD     *ZERO         $PRIMO                         PRIMO GIRO
064800000000      *
064900001127     C                   Z-ADD     56            ANMCAA
065000950214     C                   Z-ADD     DS1ADA        ANMDAO                         DATA INIZIALE
065100990521     C     FNANMK        SETLL     FNANM04L
065200990521     C     FNANMK        READE     FNANM04L                               31
065300970502    1C     *IN31         DOWEQ     *OFF
065400021023      * Deve elaborare solo le anomalie con ANMFLE della L1
065500040129     c***  AnmFle        Lookup    L1                                     99
065600040129      * Deve elaborare solo le anomalie con ANMFLE della lel
065700040129     c     AnmFle        Lookup    Lel                                    99
065800021023     c                   If        Not *In99
065900021023     c                   GoTo      Rileggi
066000021023     c                   EndIf
066100970502     C*
066200970502    2C     ANMAIE        IFEQ      'E'
066300000000      *
066400971126      *
066500970502    3C     $PRIMO        IFEQ      *ZERO                                        0=PRIMO GIRO
066600000000     C                   Z-ADD     1             $PRIMO                         1=GIA' PASSATO
066700000000     C                   Z-ADD     1             $DTAAR
066800970502    3C                   ENDIF
066900000000      *
067000000000      * Cerco l'indice di schiera per il terminal di partenza
067100000000     C                   MOVE      *BLANKS       $TRANS            1            NO TRANSITO
067200970606     C                   Z-ADD     1             D                 3 0
067300000000     C     ANMFLS        LOOKUP    ORG(D)                                 99     ON=TROVATO
067400000000     C   99              Z-ADD     OTP(D)        $LINEA
067500000000     C   99              EXSR      INDTFP                                       INDICE TFP (B)
067600000000      *
067700000000      * Cerco l'indice di schiera per la linea di arrivo
067800971126     C                   Z-ADD     SIMFEL        $LINEA
067900000000     C                   EXSR      INDLNA                                       INDICE LNA (A)
068000000000     C                   ADD       1             DIS(B)                         DEL TFP
068100000000     C                   ADD       1             DISA(A)                        DELLA LNA
068200000620    2C                   ENDIF
068300970502      *
068400021023     c     Rileggi       Tag
068500021023
068600990521     C     FNANMK        READE     FNANM04L                               31
068700970502    1C                   ENDDO
068800000000      *
068900971126     C     $PRIMO        IFGT      *ZEROS
069000000000     C                   EXSR      SCASCH                                       SCARICA SCHIERE
069100000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
069200971126     C                   END
069300000000      *
069400000000     C                   ENDSR
069500090310      **************************************************************************
069600090310      * Prepara SQL per lettura FNART
069700090310      **************************************************************************
069800090310     C     PrepaSQL      BEGSR
069900090310     c                   eval      wrkgetlista =
070000090310     c                             'select artaas, artlnp, artnrs, artnsp,' +
070100090310     c                             ' artlna, artflp  '+
070200090310     c                             ' from fnart00f  where' +
070300090310     c                             ' artdam = '+
070400090310     c                              %editc(ds1ada:'X') + ' and ' +
070500090310     c                             'artlna  in('
070600090310      * ciclo sulla sk delle linee da elaborare
070700090310     c                   z-add     1             c
070800090310     c                   Do        60            C
070900090310     c                   If        lel(c) = 0
071000090310     c                   Leave
071100090310     c                   EndIf
071200090310     c
071300090310     c                   If        C >1
071400090310     c                   eval      wrkgetlista =
071500090310     c                             %trim(wrkgetlista) + ','
071600090310     c                   EndIf
071700090310     c
071800090310     c                   eval      wrkgetlista =
071900090310     c                             %trim(wrkgetlista) + ' ''' +
072000090310     c                             %editc(lel(C):'X') + ''''
072100090310     c                   EndDo
072200090310     c
072300090310     c                   Eval      wrkgetlista =
072400090310     c                             %trim(wrkgetlista) + ')'
072500090310     c
072600090310      *?Ordinamento
072700090310     c                   eval      wrkgetlista = wrkgetlista +
072800090310     c                             ' order by artlna, artdam '
072900090310      *?Solo lettura
073000090310     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
073100090310     c
073200090310
073300090310     C/EXEC SQL
073400090310     C+ PREPARE s1 FROM :wrkgetlista
073500090310     C/END-EXEC
073600090310     C
073700090310     C/EXEC SQL
073800090310     C+ DECLARE A1 CURSOR FOR S1
073900090310     C/END-EXEC
074000090310     C
074100090310     C/EXEC SQL
074200090310     C+ OPEN a1
074300090310     C/END-EXEC
074400090310
074500090310     C                   ENDSR
074600000000      **************************************************************************
074700000000      * CARICAMENTO TABELLE INIZIALI
074800000000      **************************************************************************
074900000000     C     CARTAB        BEGSR
075000000000      *
075100000000     C                   Z-ADD     1             CODUT
075200000000     C                   CALL      'X§PARUT'
075300000000     C                   PARM                    UT§DSE
075400000000     C                   MOVEL     RAGUT         RSUT             20
075500000000     C                   MOVEL     REC80         CNCR80
075600000000      *
075700941228      * AGGANCIO LA TABELLA QT PER ALCUNI DATI AZIENDALI
075800941228     C                   Z-ADD     1             TBLKUT
075900941228     C                   MOVE      'QT'          TBLCOD
076000941228     C                   MOVEL     '1       '    TBLKEY
076100941228     C     TABELK        CHAIN     TABEL00F                           99
076200941228     C  N99              MOVEL     TBLUNI        DSQT1
076300941228     C   99              CLEAR                   DSQT1
076400000000      *
076500000000      * Prelevo la tabella delle variabili programmi (3I)
076600000000     C                   Z-ADD     1             TBLKUT
076700000000     C                   MOVE      '3I'          TBLCOD
076800000000     C                   MOVEL     '1       '    TBLKEY
076900000000     C     TABELK        CHAIN     TABEL00F                           99
077000000000     C  N99              MOVEL     TBLUNI        DS3I
077100000000     C   99              Z-ADD     20            §3IARR
077200000000      *
077300000000      * Carico una schiera contenente i TFA relativi alle filiali
077400000619     C                   Z-ADD     1             WW
077500000000     C     *LOVAL        SETLL     AZORG01L
077600960926     C                   DO        *HIVAL        $H                3 0
077700000000     C                   READ      AZORG01L                               31
077800000000     C  N31ORGFAG        IFNE      'V'                                          'V'=CENTRO COST
077900940720     C                   Z-ADD     ORGFIL        ORG($H)                        CD.FILIALE
078000970704     C                   CLEAR                   DSLV55
078100970704     C                   Z-ADD     ORGFIL        D55LIN
078200970704     C                   Z-ADD     DS1ADA        D55DRF
078300970704     C                   CALL      'FNLV55R'
078400970704     C                   PARM                    DSLV55
078500970704     C                   Z-ADD     D55TFA        OTA($H)                        TERM.ARRIVO
078600970704     C                   Z-ADD     D55TFP        OTP($H)                        TERM.PARTENZ
078700000000     C                   ENDIF
078800000000     C  N31              ENDDO
078900000000      *
079000000000      * Carico la tabella '£1' che contiene le filiali gestite
079100000000     C                   MOVE      KPJBU         $KPJBU                         SALVO KPJBU
079200941230     C                   MOVEL     '£1'          D06COD                         COD.TABELLA
079300020508     c                   movel     simfel        d06key
079400050802     c                   move      'S'           d06key
079500941230     C                   MOVEL     'L'           D06TLA                         CHIUDI CON LR
079600941230     C                   MOVEL     *BLANKS       D06ESC                         COMPRESA CHIAVE
079700050802     C                   MOVEL     ds1ada        D06drf                         COMPRESA CHIAVE
079800000000     C                   MOVE      *BLANKS       KPJBU
079900000000     C                   MOVEL     DSUL06        KPJBU
080000000000     C                   CALL      'TRUL06R'
080100000000     C                   PARM                    KPJBA
080200000000     C                   MOVEL     KPJBU         DSUL06
080300000000     C                   MOVE      $KPJBU        KPJBU                          RIPRIST. KPJBU
080400040127     c* carico schiera dei p.o. da elaborare
080500040127     c                   z-add     1             c
080600040127     c                   clear                   lel
080700040127     c                   clear                   ii                3 0
080800040127    1c     l1(c)         downe     *zeros
080900040127     c                   clear                   trul09ds
081000040127     c                   eval      i09mod = 'TFU'
081100040127     c                   eval      i09tfa = l1(c)
081200040127     c                   eval      i09dta = ds1ada
081300040127     c                   call      'TRUL09R'
081400040127     c                   parm                    trul09ds
081500040127    2c     o09err        ifeq      *blanks
081600040127     c                   z-add     1             ww                3 0
081700040127    3c     t09(ww)       downe     *zeros
081800040127     c     t09(ww)       lookup    lel                                    31
081900040130     c  n31              add       1             ii
082000040127     c  n31              eval      lel(ii) = t09(ww)
082100040127     c                   add       1             ww
082200040127    3c                   enddo
082300040127   x2c                   else
082400040127     c     l1(c)         lookup    lel                                    31
082500040130     c  n31              add       1             ii
082600040127     c  n31              eval      lel(ii) = l1(c)
082700040127    2c                   endif
082800040127     c                   add       1             c
082900040127    1c                   enddo
083000040128     c* carico altra schiera nel caso in cui uno o più p.o. della schiera
083100040128     c* lel sia a sua volta un secondo liv. in arrivo il cui ter-arr apparti
083200040128     c* ene ad altra £1
083300040128     c                   z-add     1             c
083400040129     c                   clear                   lel2
083500040205     c* pulisco ds multipla
083600040205     c                   do        20            ii
083700040205     c     ii            occur     dslel3
083800040205     c                   clear                   lel3
083900040205     c                   enddo
084000040205     c*
084100040128     c     lel(c)        downe     0
084200040128     c     kcae2         setll     azcae02l
084300040128     C     KCAE2         READE     AZCAE02L                               99
084400040128     C     *IN99         DOWEQ     *OFF
084500040128     C     CAEATB        IFEQ      *BLANKS
084600040128     C     DS1ADA        ANDGE     CAEDDE
084700040128     C     DS1ADA        ANDLE     CAEDSC
084800040128     C     CAETFE        LOOKUP    lel                                    99
084900040128     c     *in99         ifeq      *off
085000040205     c                   z-add     1             ii
085100040205     c     caetfe        lookup    lel2(ii)                               99
085200040205     c     *in99         ifeq      *off
085300040206     c     000           lookup    lel2(ii)                               99
085400040206     C   99              z-add     caetfe        lel2(ii)
085500040205     c                   endif
085600040205     c     ii            occur     dslel3
085700040205     c                   z-add     1             yy                3 0
085800040206     c     '   '         lookup    lel3(yy)                               99
085900040205     c   99              move      lel(c)        lel3(YY)
086000040128     c                   endif
086100040128     C                   ENDif
086200040128     C     KCAE2         READE     AZCAE02L                               99
086300040128     C                   ENDDO
086400040128     c                   add       1             c
086500040128     c                   enddo
086600040127     c
086700000000      *
086800000000      * Reperimento tabella per trasmissione archivio statistiche
086900040923     C***                Z-ADD     1             TBLKUT
087000040923     C***                MOVE      '5C'          TBLCOD
087100040923     C***                MOVE      '1       '    TBLKEY
087200040923     C***  TABELK        CHAIN     TABEL00F                           99
087300040923     C**N99              MOVEL     TBLUNI        DS5C
087400040923     C***99              CLEAR                   DS5C
087500970605     C**
087600970605     C**  CARICO TABELLA 7J - CODICI PISTOLA (CARICO SOLO QUELLI
087700970605     C*                                       AUTOMATICI)
087800970605     C                   Z-ADD     1             TBLKUT
087900970605     C                   MOVEL     '7J'          TBLCOD
088000970605     C                   Z-ADD     1             X                 3 0
088100980402     C                   Z-ADD     1             Y                 3 0
088200970605     C     KTAB2         SETLL     TABEL
088300970605     C     KTAB2         READE     TABEL                                  99
088400970605     C*
088500970606     C     *IN99         DOWEQ     *OFF
088600970606     C     TBLFLG        IFEQ      ' '
088700970605     C                   MOVEL     TBLUNI        DS7J
088800970605     C     §7JRPS        IFEQ      'A'
088900970605     C                   MOVEL     TBLKEY        NPS(X)
089000970605     C                   ADD       1             X
089100980402     C                   ENDIF
089200980402     C     §7JFSL        IFEQ      'F'
089300980402     C                   MOVEL     TBLKEY        NPF(Y)
089400980402     C                   ADD       1             Y
089500980402     C                   ENDIF
089600980402     C                   ENDIF
089700970605     C*
089800970605     C     KTAB2         READE     TABEL                                  99
089900970606     C                   ENDDO
090000970606     C***
090100970606     C* CARICO I TIPI TRAINO DI DEFLUENZA
090200970606     C***
090300970606     C                   Z-ADD     1             TBLKUT
090400970606     C                   MOVEL     'TV'          TBLCOD
090500970606     C                   Z-ADD     1             X                 3 0
090600970606     C     KTAB2         SETLL     TABEL
090700970606     C     KTAB2         READE     TABEL                                  99
090800970606     C*
090900970606     C     *IN99         DOWEQ     *OFF
091000970606     C     TBLFLG        IFEQ      ' '
091100970606     C                   MOVEL     TBLUNI        DSTV
091200970606     C     §TVDEF        IFEQ      'S'
091300970606     C                   MOVEL     TBLKEY        TTR(X)
091400970606     C                   ADD       1             X
091500970606     C                   ENDIF
091600970606     C                   ENDIF
091700970606     C*
091800970606     C     KTAB2         READE     TABEL                                  99
091900970606     C                   ENDDO
092000000000      *
092100000000     C     *LIKE         DEFINE    KPJBU         $KPJBU                         DI COMODO
092200941230     C     *LIKE         DEFINE    ARBCBO        $CBLPR                         PRECEDENTE
092300000000     C     *LIKE         DEFINE    ARBTFA        $TFAPR                         PRECEDENTE
092400000000     C     *LIKE         DEFINE    $BOLLA        $BOLPR                         PRECEDENTE
092500000000     C     *LIKE         DEFINE    FVVNPG        WNPG
092600000000     C     *LIKE         DEFINE    FVVDFV        WDFV
092700941229     C     *LIKE         DEFINE    FVVFGS        WFGS
092800040128     C     *LIKE         DEFINE    CAEEPA        KEPA
092900020912     C**!!!*LIKE         DEFINE    FVVFLE        WFLE
093000000000      *
093100000000     C                   ENDSR
093200000000      **************************************************************************
093300000000      *    IMPOSTO LETTURE LINEE TRANSITI
093400000000      **************************************************************************
093500000000     C     IMPOT         BEGSR
093600000000      *
093700000000      * MI POSIZIONO X > E VEDO SE SONO A FINE FINE
093800000000      *    ALTRIMENTI MI RIPOSIZIONO CON LA NUOVA LINEA LETTA
093900000000      *
094000020823     C     KDTA          SETLL     FNBTP19L
094100000000      *
094200000000     C                   ENDSR
094300000000      **************************************************************************
094400000000      * ELABORAZIONE DATI DELLA BOLLA E DEI PROPRI COLLI
094500000000      **************************************************************************
094600000000     C     ELABB         BEGSR
094700000000      *
094800000000      * Aggancio il tipo bolla dalla tabella 3A per sapere se c/servizio
094900000000      * solo se diverso dal precedente
095000941230     C     ARBCBO        IFNE      $CBLPR
095100000000     C                   Z-ADD     1             TBLKUT
095200000000     C                   MOVE      '3A'          TBLCOD
095300000000     C                   MOVE      *BLANKS       TBLKEY
095400941230     C                   MOVEL     ARBCBO        TBLKEY
095500000000     C     TABELK        CHAIN     TABEL00F                           99
095600000000     C  N99              MOVEL     TBLUNI        DS3A
095700000000     C   99              CLEAR                   DS3A
095800000000     C                   MOVEL     §3ATB1        $ASFR             1            ASSEGN./FRANCO
095900941230     C                   MOVEL     ARBCBO        $CBLPR                         PRECEDENTE
096000000000     C                   ENDIF
096100000000      *
096200000000      * Escludo i recuperi
096300000000     C     §3ARBL        IFNE      'R'                                          'R'=RECUPERI
096400000000      *
096500000000      * Solo al primo giro imposto TFA precedente
096600000000     C     $PRIMO        IFEQ      *ZERO                                        0=PRIMO GIRO
096700000000     C                   Z-ADD     ARBTFA        $TFAPR                         PRECEDENTE
096800000000     C                   Z-ADD     1             $PRIMO                         1=GIA' PASSATO
096900000000     C                   Z-ADD     1             $DTAAR
097000000000     C                   ENDIF
097100000000      *
097200000000      * Se TFA <> dal precedente salvo e pulisco le schiere
097300000000     C     ARBTFA        IFNE      $TFAPR                                       PRECEDENTE
097400000000     C     $PRIMO        ANDNE     *ZERO                                        NO PRIMO GIRO
097500000000     C                   EXSR      SCASCH                                       SCARICA SCHIERE
097600000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
097700980708     C                   Z-ADD     0             $B
097800000000     C                   Z-ADD     ARBTFA        $TFAPR                         PRECEDENTE
097900000000     C                   ENDIF
098000000000     C                   MOVE      *BLANKS       $TRANS                         NO TRANSITO
098100000000      *
098200000000      * Se c/servizio devo trattarlo come filiale a parte (solo per TFP)
098300000000      * Cerco l'indice di schiera per il terminal di partenza
098400000000     C     §3ARBL        IFEQ      'C'                                          'C'=C/SERVIZIO
098500000000     C                   MOVE      *ALL'9'       $LINEA
098600000000     C                   ELSE
098700000619     C* SE ARBLNP E' POSTE TENGO LUI COME TERMINAL DI PARTENZA
098800000619     C                   Z-ADD     1             D
098900000619     C     ARBLNP        LOOKUP    ORG(D)                                 99     ON=TROVATO
099000000619     C   99              Z-ADD     OTP(D)        $LINEA
099100000619     C  N99              Z-ADD     ARBTFP        $LINEA                         999=C/SERVIZIO
099200000619     C                   ENDIF
099300000000     C                   EXSR      INDTFP                                       INDICE TFP (B)
099400000000      *
099500000000      * Cerco l'indice di schiera per la filiale di arrivo
099600000000     C                   Z-ADD     VIDLNA        $LINEA
099700000000     C                   EXSR      INDLNA                                       INDICE LNA (A)
099800000000      *
099900090929      * SPEDIZIONI "E"
100000090929     c                   select
100100090929     C     ARBTSP        wheneq    'E'                                          'E'=ESPRESSI
100200000000      *
100300000000     C     $ASFR         IFEQ      'F'                                          'F'=FRANCO
100400000000     C                   ADD       1             FE(B)                          TFP FRANCHI   E
100500000000     C                   ADD       1             FEA(A)                         LNA    "      E
100600000620     C*
100700000000     C                   ELSE
100800000000     C                   ADD       1             AE(B)                          TFP ASSEGNATI E
100900000000     C                   ADD       1             AEA(A)                         LNA     "     E
101000000620     C                   ENDIF
101100090929
101200090929     C     ARBTSP        wheneq    'H'                                          'H'=h 10 30
101300090929      *
101400090929     C     $ASFR         IFEQ      'F'                                          'F'=FRANCO
101500090929     C                   ADD       1             Fh(B)                          TFP FRANCHI   H
101600090929     C                   ADD       1             FhA(A)                         LNA    "      H
101700090929     C*
101800090929     C                   ELSE
101900090929     C                   ADD       1             Ah(B)                          TFP ASSEGNATI H
102000090929     C                   ADD       1             AhA(A)                         LNA     "     H
102100090929     C                   ENDIF
102200000000      *
102300090929     C                   other                                                  C+D
102400000000      *
102500000000     C     $ASFR         IFEQ      'F'                                          'F'=FRANCO
102600000000     C                   ADD       1             FN(B)                          TFP FRANCHI   N
102700000000     C                   ADD       1             FNA(A)                         LNA    "      N
102800000620     C*
102900000000     C                   ELSE
103000000000     C                   ADD       1             AN(B)                          TFP ASSEGNATI N
103100000000     C                   ADD       1             ANA(A)                         LNA     "     N
103200000620     C                   ENDIF
103300000000      *
103400090929     C                   ENDSL
103500000000      *
103600000000     C                   ENDIF
103700000000      *
103800000000     C                   ENDSR
103900000000      **************************************************************************
104000000000      * Elaborazione su bolla arrivi
104100000000      * Questa routine faceva parte della ELABC, sono state separate per non
104200000000      * ripetere i calcoli della bolla per ogni collo
104300000000      **************************************************************************
104400000000     C     ELABC1        BEGSR
104500000000      *
104600021218      *
104700021218      * Occhio al peso nella bolla!!     Sulla bolla devo verificare i dati
104800021218      * misurati con la macchina smistacolli (ARBNCP=Numero colli rilevati,
104900021218      * ARBPKC=Peso rilevato)
105000030213      **** Verifico se sono stati pesati   tutti i colli
105100021218sel 1c                   select
105200050111     c                   when      ARBncp = ARBncl
105300021218     c                   z-add     ARBpkc        ARBpkf
105400021218    1c                   when      ARBpkc >= ARBpkf
105500021218     c                   z-add     ARBpkc        ARBpkf
105600021218e   1c                   endsl
105700021220      * Calcolo il PESO   unitario medio per collo da usare se non
105800021220      * tutti i colli sono stati pesati   dalla smistacolli
105900021220     c     ARBpkf        div(H)    ARBncl        Wpesmed
106000000000      *
106100000000      * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
106200000000      * misurati con la macchina smistacolli (ARBNCR=Numero colli rilevati,
106300941230      * ARBVLC=Volume rilevato)
106400030213      **** Verifico se sono stati volumati tutti i colli
106500050111     C     ARBNCR        IFEQ      ARBNCL
106600941230     C                   Z-ADD     ARBVLC        ARBVLF                         VOLUME RILEVATO
106700000000     C                   ELSE
106800941230     C     ARBVLC        IFGE      ARBVLF
106900941230     C                   Z-ADD     ARBVLC        ARBVLF                         VOLUME RILEVATO
107000000000     C                   ENDIF
107100000000     C                   ENDIF
107200021220      * Calcolo il VOLUME unitario medio per collo da usare se non
107300021220      * tutti i colli sono stati pesati   dalla smistacolli
107400021220     c     ARBvlf        div(H)    ARBncl        Wvolmed
107500000000      *
107600000000      * Verifico il tipo bolla con il "precedente"
107700941230     C     ARBCBO        IFNE      $CBLPR
107800000000     C                   Z-ADD     1             TBLKUT
107900000000     C                   MOVEL     '3A'          TBLCOD
108000000000     C                   MOVE      *BLANKS       TBLKEY
108100941230     C                   MOVEL     ARBCBO        TBLKEY
108200000000     C     TABELK        CHAIN     TABEL00F                           99
108300000000     C  N99              MOVEL     TBLUNI        DS3A
108400000000     C   99              CLEAR                   DS3A
108500941230     C                   MOVEL     ARBCBO        $CBLPR                         TIPO PRECEDENTE
108600000000     C                   ENDIF
108700000000      *
108800000000      * CALCOLO PESO TASSABILE (solo sotto i 35 quintali)
108900000000      * N.B.:  Va calcolato sull'intera bolla, indi diviso per il nr.colli
109000941230     C     ARBPKF        IFLE      3500
109100000000      *
109200000000     C     §3ARBL        IFNE      'C'                                          'C'=C/SERVIZIO
109300941230     C                   Z-ADD     ARBPKF        PESTAS
109400941230     C     ARBVLF        IFNE      0
109500941230     C     ARBVLF        MULT      §QTKPM        COMPKT            9 0
109600000000     C     COMPKT        IFGT      PESTAS
109700000000     C                   Z-ADD     COMPKT        PESTAS
109800000000     C                   ENDIF
109900000000     C                   ENDIF
110000000000      *
110100000000      * ARROTONDAMENTO DI 20 KG
110200000000     C                   Z-ADD     0             COMPKT
110300000000     C     PESTAS        DIV       §3IARR        COMPKT
110400000000     C                   MVR                     RESTO
110500000000     C     RESTO         IFGT      0
110600000000     C     COMPKT        MULT      §3IARR        PESTAS
110700000000     C                   ADD       §3IARR        PESTAS
110800000000     C                   ENDIF
110900000000      *
111000000000     C                   DIV       ARBNCL        PESTAS
111100000000      *
111200000000     C                   ENDIF
111300000000     C                   ENDIF
111400000000      *
111500000000     C                   ENDSR
111600000000      **************************************************************************
111700000000      * Elaborazione colli, assegnazione pesi e volumi
111800000000      **************************************************************************
111900000000     C     ELABC         BEGSR
112000000000      *
112100000000      * Se bolla di recupero la salto
112200000000     C     §3ARBL        IFNE      'R'                                          'R'=RECUPERI
112300000000      *
112400000000      * Solo al primo giro imposto TFA precedente
112500000000     C     $PRIMO        IFEQ      *ZERO                                        0=PRIMO GIRO
112600000000     C                   Z-ADD     ARBTFA        $TFAPR                         PRECEDENTE
112700000000     C                   Z-ADD     1             $PRIMO                         1=GIA' PASSATO
112800000000     C                   Z-ADD     1             $DTAAR
112900000000     C                   ENDIF
113000000000      *
113100000000      * Come per le bolle rompo quando cambia terminal di arrivo
113200000000     C     ARBTFA        IFNE      $TFAPR                                       PRECEDENTE
113300000000     C     $PRIMO        ANDNE     *ZERO                                        NO PRIMO GIRO
113400000000     C                   EXSR      SCASCH                                       SCARICA SCHIERE
113500000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
113600980708     C                   Z-ADD     0             $B
113700000000     C                   Z-ADD     ARBTFA        $TFAPR                         PRECEDENTE
113800000000     C                   ENDIF
113900000000     C                   MOVE      *BLANKS       $TRANS                         NO TRANSITO
114000000000      *
114100000000      * Se c/servizio devo trattarlo come filiale a parte (solo per TFP)
114200000000      * Cerco l'indice di schiera per il terminal di partenza
114300000000     C     §3ARBL        IFEQ      'C'                                          'C'=C/SERVIZIO
114400000000     C                   MOVE      *ALL'9'       $LINEA
114500000000     C                   ELSE
114600000619     C* SE ARBLNP E' POSTE TENGO LUI COME TERMINAL DI PARTENZA
114700000619     C                   Z-ADD     1             D
114800000619     C     ARBLNP        LOOKUP    ORG(D)                                 99     ON=TROVATO
114900000619     C   99              Z-ADD     OTP(D)        $LINEA
115000000619     C  N99              Z-ADD     ARBTFP        $LINEA                         999=C/SERVIZIO
115100000000     C                   ENDIF
115200000000     C                   EXSR      INDTFP                                       INDICE TFP (B)
115300000000      *
115400000000      * Cerco l'indice di schiera per la filiale di arrivo
115500000000     C                   Z-ADD     ARTLNA        $LINEA
115600000000     C                   EXSR      INDLNA                                       INDICE LNA (A)
115700000000      *
115800000000     C                   ADD       1             CLL(B)                         TFP NR.COLLI
115900000000     C                   ADD       1             CLLA(A)                        LNA NR.COLLI
116000000000      *
116100021218      *
116200021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
116300030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
116400030213      * Peso medio - per "standardizzare" con i pgm. FNLS*
116500030213      * ("metti" che qualche collo NON sia stato pesato in partenza...)
116600021220     c                   z-add     Wpesmed       $Peso
116700000000      *
116800000000      * CALCOLO PESO
116900021218     C     ARBPKF        IFLE      3500
117000000000     C                   ADD(H)    $PESO         F35(B)
117100000000     C                   ADD(H)    $PESO         F35A(A)
117200000000      *
117300000000      * CALCOLO PESO TASSABILE (solo sotto i 35 quintali)
117400000000      * N.B.:  Va calcolato sull'intera bolla, indi diviso per il nr.colli
117500000000     C     §3ARBL        IFNE      'C'                                          'C'=C/SERVIZIO
117600000000     C                   ADD       PESTAS        PTA(B)
117700000000     C                   ADD       PESTAS        PTAA(A)
117800000620     C                   ENDIF
117900000000      *
118000000000     C                   ELSE
118100000000     C                   ADD(H)    $PESO         O35(B)
118200000000     C                   ADD(H)    $PESO         O35A(A)
118300000620     C                   ENDIF
118400021218      *
118500021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
118600030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
118700030213      * Volume medio - per "standardizzare" con i pgm. FNLS*
118800030213      * ("metti" che qualche collo NON sia stato volumato in partenza...)
118900021220     c                   z-add     Wvolmed       $Volum
119000000000      *
119100000000      * CALCOLO VOLUME
119200000000     C     $VOLUM        IFEQ      0
119300000000     C                   Z-ADD     0,0001        $VOLUM
119400000000     C                   ENDIF
119500000000     C                   ADD(H)    $VOLUM        VOL(B)
119600000000     C                   ADD(H)    $VOLUM        VOLA(A)
119700000000      *
119800000000     C                   ENDIF
119900000000      *
120000000000     C                   ENDSR
120100000000      **************************************************************************
120200000000      * ELABORAZIONE DATI DELLA BOLLA TRANSITO E DEI PROPRI COLLI
120300000000      **************************************************************************
120400000000     C     ELABT         BEGSR
120500000619     C*
120600000619     C                   Z-ADD     1             D
120700000619     C     BTPLNP        LOOKUP    ORG(D)                                 99     ON=TROVATO
120800000619     C   99              Z-ADD     OTP(D)        $LINTR            3 0
120900000619     C  N99              Z-ADD     BTPTFP        $LINTR
121000000000      *
121100000000      * SE HO LETTO LINEA PARTENZA NUOVA MEMORIZZO
121200000000     C                   Z-ADD     1             B                 3 0
121300000619     C     $LINTR        LOOKUP    TFPT(B)                                32     TERMINAL PART.
121400000000     C  N32              ADD       1             $B                             CONTATORE TFPT
121500000000     C  N32              Z-ADD     $B            B
121600000703     C  N32              Z-ADD     $LINTR        TFPT(B)                        NUOVA FILIALE
121700000000      *
121800000000      * CHAIN SU TABELLE SOLO SE TIPO BOLLA <> DAL PRECEDENTE
121900941230     C     BTPCBO        IFNE      $CBLPR                                       <> PRECEDENTE
122000000000     C                   Z-ADD     1             TBLKUT
122100000000     C                   MOVE      '3A'          TBLCOD
122200000000     C                   MOVE      *BLANKS       TBLKEY
122300941230     C                   MOVEL     BTPCBO        TBLKEY
122400000000     C     TABELK        CHAIN     TABEL00F                           99
122500000000     C  N99              MOVEL     TBLUNI        DS3A
122600000000     C   99              CLEAR                   DS3A
122700941230     C                   MOVEL     BTPCBO        $CBLPR                         PRECEDENTE
122800000000     C                   ENDIF
122900000000      *
123000000000      * Recuperi esclusi
123100000000     C     §3ARBL        IFNE      'R'
123200000000      *
123300090929      * CALCOLO SPEDIZIONI
123400000000     C                   MOVEL     §3ATB1        $ASFR                          ASSEGN./FRANCO
123500090929     C                   SELECT
123600090929     C     BTPTSP        WHENEQ    'E'                                          'E'=ESPRESSO
123700000000      *
123800000000     C     $ASFR         IFEQ      'F'
123900000000     C                   ADD       1             FET(B)                         FRANCHI     ESP
124000000000     C                   ADD       1             FED               7 0             "        ESP
124100000000     C                   ELSE
124200000000     C                   ADD       1             AET(B)                         ASSEGNATI   ESP
124300000000     C                   ADD       1             AED               7 0             "        ESP
124400000000     C                   END
124500000000      *
124600090929     C     BTPTSP        WHENEQ    'H'                                          'H'=H 10 30
124700090929      *
124800090929     C     $ASFR         IFEQ      'F'
124900090929     C                   ADD       1             FHT(B)                         FRANCHI     "H"
125000090929     C                   ADD       1             FHD               7 0             "        "H"
125100090929     C                   ELSE
125200090929     C                   ADD       1             AHT(B)                         ASSEGNATI   "H"
125300090929     C                   ADD       1             AHD               7 0             "        "H"
125400090929     C                   END
125500090929      *
125600090929     C                   OTHER
125700000000      *
125800000000     C     $ASFR         IFEQ      'F'                                          'F'=FRANCO
125900000000     C                   ADD       1             FNT(B)                         FRANCHI     NOR
126000000000     C                   ADD       1             FND               7 0             "        NOR
126100000000     C                   ELSE
126200000000     C                   ADD       1             ANT(B)                         ASSEGNATI   NOR
126300000000     C                   ADD       1             AND               7 0             "        NOR
126400000000     C                   END
126500000000      *
126600090929     C                   ENDSL
126700000000      *
126800000000     C                   ENDIF                                                  $CBLPR<>'R'
126900000000      *
127000021218     C                   ENDSR
127100000000      **************************************************************************
127200000000      * Elaborazione bolla transiti
127300000000      **************************************************************************
127400000000     C     ELACT1        BEGSR
127500000000      *
127600000000      * Verifico il tipo bolla con il "precedente"
127700941230     C     BTPCBO        IFNE      $CBLPR
127800000000     C                   Z-ADD     1             TBLKUT
127900000000     C                   MOVEL     '3A'          TBLCOD
128000000000     C                   MOVE      *BLANKS       TBLKEY
128100941230     C                   MOVEL     BTPCBO        TBLKEY
128200000000     C     TABELK        CHAIN     TABEL00F                           99
128300000000     C  N99              MOVEL     TBLUNI        DS3A
128400000000     C   99              CLEAR                   DS3A
128500941230     C                   MOVEL     BTPCBO        $CBLPR                         TIPO PRECEDENTE
128600000000     C                   ENDIF
128700021218     *** *
128800021218     ***C     BTPPKB        DIV(H)    BTPNCL        $PESO                          PESO UNITARIO
128900021218      *
129000021218      * Occhio al peso nella bolla!!     Sulla bolla devo verificare i dati
129100021218      * misurati con la macchina smistacolli (BTPNCP=Numero colli rilevati,
129200021218      * BTPPKC=Peso rilevato)
129300030213      **** Verifico se sono stati pesati   tutti i colli
129400030213     c***                clear                   Wsipes
129500021218sel 1c                   select
129600050111     c                   when      BTPncp = BTPncl
129700021218     c                   z-add     BTPpkc        BTPpkb
129800030213     c***                eval      Wsipes =  'S'
129900021218    1c                   when      BTPpkc >= BTPpkb
130000021218     c                   z-add     BTPpkc        BTPpkb
130100021218e   1c                   endsl
130200000000      *
130300000000      * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
130400021218      * misurati con la macchina smistacolli (BTPNCR=Numero colli rilevati,
130500021218      * BTPVLC=Volume rilevato)
130600030213     c***                clear                   Wsivol
130700050111     C     BTPNCR        ifeq      BTPNCL
130800941230     C                   Z-ADD     BTPVLC        BTPVLB                         VOLUME RILEVATO
130900030213     c***                eval      Wsivol =  'S'
131000000000     C                   ELSE
131100941230     C     BTPVLC        IFGE      BTPVLB
131200941230     C                   Z-ADD     BTPVLC        BTPVLB                         VOLUME RILEVATO
131300000000     C                   ENDIF
131400000000     C                   ENDIF
131500000000      *
131600000000      * CALCOLO PESO TASSABILE
131700941230     C     BTPPKB        IFLE      3500
131800000000     C     §3ARBL        IFNE      'C'                                          'C'=C/SERVIZIO
131900000000      *
132000021220     ***C                   Z-ADD     $PESO         PESTAS
132100021220     C                   Z-ADD     BTPPKB        PESTAS
132200941230     C     BTPVLB        IFNE      0
132300941230     C     BTPVLB        MULT      §QTKPM        COMPKT
132400000000     C     COMPKT        IFGT      PESTAS
132500000000     C                   Z-ADD     COMPKT        PESTAS
132600000000     C                   ENDIF
132700000000     C                   ENDIF
132800000000      *
132900000000      * ARROTONDAMENTO DI 20 KG
133000000000     C                   Z-ADD     0             COMPKT
133100000000     C     PESTAS        DIV       §3IARR        COMPKT
133200000000     C                   MVR                     RESTO             3 0
133300000000     C     RESTO         IFGT      0
133400000000     C     COMPKT        MULT      §3IARR        PESTAS            9 1
133500000000     C                   ADD       §3IARR        PESTAS
133600000000     C                   ENDIF
133700000000      *
133800941230     C                   DIV       BTPNCL        PESTAS
133900000000      *
134000000000     C                   ENDIF
134100000000     C                   ENDIF
134200000000      *
134300000000     C                   ENDSR
134400000000      **************************************************************************
134500000000      * Elaborazione colli transiti, assegnazione pesi e volumi
134600000000      **************************************************************************
134700000000     C     ELABCT        BEGSR
134800000000      *
134900000000      * Se bolla di recupero la salto
135000000000     C     §3ARBL        IFNE      'R'                                          'R'=RECUPERI
135100000000      *
135200000000      * Verifico se ho gia' incontrato questo terminal di partenza
135300000703     C*
135400000703     C                   Z-ADD     1             D
135500000703     C     BTPLNP        LOOKUP    ORG(D)                                 99     ON=TROVATO
135600000703     C   99              Z-ADD     OTP(D)        $LINTR            3 0
135700000703     C  N99              Z-ADD     BTPTFP        $LINTR
135800000703      *
135900000000     C                   Z-ADD     1             B
136000000703     C     $LINTR        LOOKUP    TFPT(B)                                32     TERMINAL PART.
136100000000     C  N32              ADD       1             $B                             CONTATORE TFPT
136200000000     C  N32              Z-ADD     $B            B
136300000703     C  N32              Z-ADD     $LINTR        TFPT(B)                        NUOVA FILIALE
136400000000      *
136500000000     C                   ADD       1             CLLT(B)                         TFP NR.COLLI
136600000000     C                   ADD       1             CLLD              7 0          LNA NR.COLLI
136700000000      *
136800021218     ***C     BTTVUC        IFEQ      *ZERO
136900021218     *** *
137000021218     ***C     BTPVLB        DIV(H)    BTPNCL        $VOLUM                         VOLUME UNITARIO
137100021218     *** *
137200021218     ***C                   ELSE
137300021218     ***C                   Z-ADD     BTTVUC        $VOLUM                         DA SMISTACOLLI
137400021218     ***C                   ENDIF
137500021218      *
137600021218      * Calcolo il peso medio
137700030213    1c***                when      Wsipes =  'S' and BTTpuc > 0
137800030213     c***                z-add     BTTpuc        $Peso
137900021218     c     BTPpkb        div(H)    BTPncl        $Peso
138000000000      *
138100000000      * CALCOLO PESO
138200941230     C     BTPPKB        IFLE      3500
138300000000     C                   ADD(H)    $PESO         F35T(B)
138400000000     C                   ADD(H)    $PESO         F35D              9 1
138500000000      *
138600000000      * CALCOLO PESO TASSABILE
138700000000     C     §3ARBL        IFNE      'C'                                          'C'=C/SERVIZIO
138800000000     C                   ADD       PESTAS        PTAT(B)
138900000000     C                   ADD       PESTAS        PTAD              9 1
139000000000     C                   ENDIF
139100000000      *
139200000000     C                   ELSE
139300000000     C                   ADD(H)    $PESO         O35T(B)
139400000000     C                   ADD(H)    $PESO         O35D              9 1
139500000000     C                   ENDIF
139600021218      *
139700021218      * Calcolo il volume medio
139800030213    1c***                when      Wsivol =  'S' and BTTvuc > 0
139900030213     c***                z-add(H)  BTTvuc        $Volum
140000021218     c     BTPvlb        div(H)    BTPncl        $Volum
140100000000      *
140200000000      * CALCOLO VOLUME
140300000000     C     $VOLUM        IFEQ      0
140400000000     C                   Z-ADD     0,0001        $VOLUM
140500000000     C                   ENDIF
140600000000     C                   ADD(H)    $VOLUM        VOLT(B)
140700000000     C                   ADD(H)    $VOLUM        VOLD              9 4
140800000000      *
140900000000     C                   ENDIF
141000000000      *
141100000000     C                   ENDSR
141200000000      **************************************************************************
141300090929      * SCARICO LE SCHIERE NEL FILE DELLE STATISTICHE (FISAR00F)
141400000000      * La routine e' generica e controlla, prima di salvare, se esiste gia'
141500000000      * una precdente registrazione.  Tutte le schiere sono messe in ADD ai
141600000000      * campi del file sempre; sara' compito di chi chiama la routine di
141700000000      * avere piene solo le schiere interessate (es.: nella fase di calcolo dei
141800000000      * disguidi, le schiere piene sono solo DIS e DISA, oltre naturalmente a
141900000000      * TFP e LNA).
142000000000      **************************************************************************
142100000000     C     SCASCH        BEGSR
142200000000      *
142300000000      *?    T E R M I N A L    D I    P A R T E N Z A    ?
142400000000      *
142500000000     C                   Z-ADD     1             P                 4 0
142600000000     C     P             DOWLE     $MAX
142700000000      *
142800000000     C     CLL(P)        IFGT      *ZERO
142900000000     C     P88(P)        ORGT      *ZERO
143000000000     C     DIS(P)        ORGT      *ZERO
143100000000     C     PAR(P)        ORGT      *ZERO
143200000000     C     FN(P)         ORGT      *ZERO
143300000000     C     AN(P)         ORGT      *ZERO
143400000000     C     FE(P)         ORGT      *ZERO
143500000000     C     AE(P)         ORGT      *ZERO
143600090929     C     Fh(P)         ORGT      *ZERO
143700090929     C     Ah(P)         ORGT      *ZERO
143800090929     C                   CLEAR                   FISAR000
143900000000     C                   Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
144000000000     C                   Z-ADD     $TFAPR        SARTFA                         TFA PRECEDENTE
144100950214     C                   Z-ADD     DS1ADA        SARDRE                         DATA
144200000000     C                   Z-ADD     TFP(P)        SARTFP                         TERM.PARTENZA
144300000000     C                   Z-ADD     *ZERO         SARLNA                         LINEA DI ARRIVO
144400000000     C     TFP(P)        IFNE      999
144500000000     C                   MOVE      *BLANKS       SARTRE                         TIPO RECORD
144600000000     C                   ELSE
144700000000     C                   MOVE      'C'           SARTRE                         'C'=CONTO SERV.
144800000000     C                   ENDIF
144900090929     C     FISARK        CHAIN     FISAR01L                           99
145000000000     C                   ADD       CLL(P)        SARCLL                         COLLI
145100000000     C                   ADD       P88(P)        SARP88                         PIST.88 NON ARR
145200000000     C                   ADD       DIS(P)        SARDIS                         DISGUIDI
145300000000     C                   ADD       PAR(P)        SARPAR                         PARTITI NON SPU
145400000000     C                   ADD       PTA(P)        SARPTA                         PESO TASSABILE
145500000000     C                   ADD       F35(P)        SARF35                         FINO A 35 Q.
145600000000     C                   ADD       O35(P)        SARO35                         PESO OLTRE 35 Q
145700000000     C                   ADD       VOL(P)        SARVOL                         VOLUME
145800090929     C                   ADD       FN(P)         SARSNF                         SPED.C+D  FRANC
145900090929     C                   ADD       AN(P)         SARSNA                         SPED.C+D  ASSEG
146000090929     C                   ADD       FE(P)         SARSEF                         SPED."E"  FRANC
146100090929     C                   ADD       AE(P)         SARSEA                         SPED."E"  ASSEG
146200090929     C                   ADD       FH(P)         SARSHF                         SPED."H"  FRANC
146300090929     C                   ADD       AH(P)         SARSHA                         SPED."H"  ASSEG
146400040923     c                   z-add     dateu         sardtr
146500000000      *
146600090929     C   99              WRITE     FISAR000
146700090929     C  N99              UPDATE    FISAR000
146800000000     C                   ENDIF
146900000000     C                   ADD       1             P
147000000000      *
147100000000     C                   ENDDO
147200000000      *
147300000000      *?    L I N E E    D I    A R R I V O    ?
147400000000     C                   Z-ADD     1             P
147500000000     C     P             DOWLE     29
147600000000      *
147700090929     C* LNA CON TFP
147800970605     C     CLLA(P)       IFGT      *ZERO
147900970605     C     P88A(P)       ORGT      *ZERO
148000970605     C     DISA(P)       ORGT      *ZERO
148100970605     C     PARA(P)       ORGT      *ZERO
148200970605     C     FNA(P)        ORGT      *ZERO
148300970605     C     ANA(P)        ORGT      *ZERO
148400970605     C     FEA(P)        ORGT      *ZERO
148500970605     C     AEA(P)        ORGT      *ZERO
148600090929     C     FHA(P)        ORGT      *ZERO
148700090929     C     AHA(P)        ORGT      *ZERO
148800090929     C                   CLEAR                   FISAR000
148900000000     C                   Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
149000000000     C                   Z-ADD     $TFAPR        SARTFA                         TFA PRECEDENTE
149100950214     C                   Z-ADD     DS1ADA        SARDRE                         DATA
149200000000     C                   Z-ADD     *ZERO         SARTFP                         TERM.PARTENZA
149300000000     C                   Z-ADD     LNA(P)        SARLNA                         LINEA DI ARRIVO
149400000000     C                   MOVE      *BLANKS       SARTRE                         TIPO RECORD
149500090929     C     FISARK        CHAIN     FISAR01L                           99
149600000000     C                   ADD       CLLA(P)       SARCLL                         COLLI
149700000000     C                   ADD       P88A(P)       SARP88                         PIST.88 NON ARR
149800000000     C                   ADD       DISA(P)       SARDIS                         DISGUIDI
149900000000     C                   ADD       PARA(P)       SARPAR                         PARTITI NON SPU
150000000000     C                   ADD       PTAA(P)       SARPTA                         PESO TASSABILE
150100000000     C                   ADD       F35A(P)       SARF35                         FINO A 35 Q.
150200000000     C                   ADD       O35A(P)       SARO35                         PESO OLTRE 35 Q
150300000000     C                   ADD       VOLA(P)       SARVOL                         VOLUME
150400090929     C                   ADD       FNA(P)        SARSNF                         SPED.C+D  FRANC
150500090929     C                   ADD       ANA(P)        SARSNA                         SPED.C+D  ASSEG
150600090929     C                   ADD       FEA(P)        SARSEF                         SPED."E"  FRANC
150700090929     C                   ADD       AEA(P)        SARSEA                         SPED."E"  ASSEG
150800090929     C                   ADD       FHA(P)        SARSHF                         SPED."H"  FRANC
150900090929     C                   ADD       AHA(P)        SARSHA                         SPED."H"  ASSEG
151000040923     c                   z-add     dateu         sardtr
151100000000      *
151200090929     C   99              WRITE     FISAR000
151300090929     C  N99              UPDATE    FISAR000
151400000000     C                   ENDIF
151500000000     C                   ADD       1             P
151600000000      *
151700000000     C                   ENDDO
151800000000      *
151900000000     C                   ENDSR
152000000000      **************************************************************************
152100000000      * E' uguale alla routine SCASCH ma rivolta ai TRANSITI, con altra schiere
152200000000      **************************************************************************
152300000000     C     SCASCT        BEGSR
152400000000      *
152500000000      *?    T E R M I N A L    D I    P A R T E N Z A    ?
152600000000      *
152700000000     C                   Z-ADD     1             P
152800000000     C     P             DOWLE     $MAX
152900000000      *
153000000000     C     CLLT(P)       IFGT      *ZERO
153100000000     C     P88T(P)       ORGT      *ZERO
153200000000     C     PART(P)       ORGT      *ZERO
153300000000     C     FNT(P)        ORGT      *ZERO
153400000000     C     ANT(P)        ORGT      *ZERO
153500000000     C     FET(P)        ORGT      *ZERO
153600000000     C     AET(P)        ORGT      *ZERO
153700090929     C     FHT(P)        ORGT      *ZERO
153800090929     C     AHT(P)        ORGT      *ZERO
153900090929     C                   CLEAR                   FISAR000
154000000000     C                   Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
154100000000     C                   Z-ADD     SIMFEL        SARTFA                         TFA
154200950214     C                   Z-ADD     DS1ADA        SARDRE                         DATA
154300000000     C                   Z-ADD     TFPT(P)       SARTFP                         TERM.PARTENZA
154400000000     C                   Z-ADD     *ZERO         SARLNA                         LINEA DI ARRIVO
154500000000     C                   MOVE      'T'           SARTRE                         'T'=TRANSITI
154600090929     C     FISARK        CHAIN     FISAR01L                           99
154700000000     C                   ADD       CLLT(P)       SARCLL                         COLLI
154800000000     C                   ADD       P88T(P)       SARP88                         PIST.88 NON ARR
154900000000     C                   Z-ADD     *ZERO         SARDIS                         DISGUIDI
155000000000     C                   ADD       PART(P)       SARPAR                         PARTITI NON SPU
155100000000     C                   ADD       PTAT(P)       SARPTA                         PESO TASSABILE
155200000000     C                   ADD       F35T(P)       SARF35                         FINO A 35 Q.
155300000000     C                   ADD       O35T(P)       SARO35                         PESO OLTRE 35 Q
155400000000     C                   ADD       VOLT(P)       SARVOL                         VOLUME
155500090929     C                   ADD       FNT(P)        SARSNF                         SPED.C+D  FRANC
155600090929     C                   ADD       ANT(P)        SARSNA                         SPED.C+D  ASSEG
155700090929     C                   ADD       FET(P)        SARSEF                         SPED."E"  FRANC
155800090929     C                   ADD       AET(P)        SARSEA                         SPED."E"  ASSEG
155900090929     C                   ADD       FHT(P)        SARSHF                         SPED."H"  FRANC
156000090929     C                   ADD       AHT(P)        SARSHA                         SPED."H"  ASSEG
156100040923     c                   z-add     dateu         sardtr
156200000000      *
156300090929     C   99              WRITE     FISAR000
156400090929     C  N99              UPDATE    FISAR000
156500000000     C                   ENDIF
156600000000     C                   ADD       1             P
156700000000      *
156800000000     C                   ENDDO
156900000000      *
157000000000      *?    L I N E E    D I    A R R I V O    ?
157100000000     C     CLLD          IFGT      *ZERO
157200000000     C     P88D          ORGT      *ZERO
157300000000     C     PARD          ORGT      *ZERO
157400000000     C     FND           ORGT      *ZERO
157500000000     C     AND           ORGT      *ZERO
157600000000     C     FED           ORGT      *ZERO
157700000000     C     AED           ORGT      *ZERO
157800090929     C     FHD           ORGT      *ZERO
157900090929     C     AHD           ORGT      *ZERO
158000090929     C                   CLEAR                   FISAR000
158100000000     C                   Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
158200000000     C                   Z-ADD     SIMFEL        SARTFA                         TFA
158300950214     C                   Z-ADD     DS1ADA        SARDRE                         DATA
158400000000     C                   Z-ADD     *ZERO         SARTFP                         TERM.PARTENZA
158500000000     C                   Z-ADD     SIMFEL        SARLNA                         LINEA DI ARRIVO
158600000000     C                   MOVE      'T'           SARTRE                         TIPO RECORD
158700090929     C     FISARK        CHAIN     FISAR01L                           99
158800000000     C                   ADD       CLLD          SARCLL                         COLLI
158900000000     C                   ADD       P88D          SARP88                         PIST.88 NON ARR
159000000000     C                   Z-ADD     *ZERO         SARDIS                         DISGUIDI
159100000000     C                   ADD       PARD          SARPAR                         PARTITI NON SPU
159200000000     C                   ADD       PTAD          SARPTA                         PESO TASSABILE
159300000000     C                   ADD       F35D          SARF35                         FINO A 35 Q.
159400000000     C                   ADD       O35D          SARO35                         PESO OLTRE 35 Q
159500000000     C                   ADD       VOLD          SARVOL                         VOLUME
159600090929     C                   ADD       FND           SARSNF                         SPED.C+D  FRANC
159700090929     C                   ADD       AND           SARSNA                         SPED.C+D  ASSEG
159800090929     C                   ADD       FED           SARSEF                         SPED."E"  FRANC
159900090929     C                   ADD       AED           SARSEA                         SPED."E"  ASSEG
160000090929     C                   ADD       FHD           SARSHF                         SPED."H"  FRANC
160100090929     C                   ADD       AHD           SARSHA                         SPED."H"  ASSEG
160200040923     c                   z-add     dateu         sardtr
160300000000      *
160400090929     C   99              WRITE     FISAR000
160500090929     C  N99              UPDATE    FISAR000
160600000000     C                   ENDIF
160700000000      *
160800000000     C                   ENDSR
160900000000      **************************************************************************
161000090929      * SCARICO LE SCHIERE NEL FILE DELLE STATISTICHE (FISAR00F)
161100000000      * La routine e' generica e controlla, prima di salvare, se esiste gia'
161200000000      * una precdente registrazione.  Tutte le schiere sono messe in ADD ai
161300000000      * campi del file sempre; sara' compito di chi chiama la routine di
161400000000      * avere piene solo le schiere interessate (es.: nella fase di calcolo
161500000000      * delle LNA di cui solo ..., le schiere piene sono solo CLLR, P88R e PARR
161600000000      * oltre a LNAR)
161700000000      **************************************************************************
161800000000     C     SCASCR        BEGSR
161900000000      *
162000000000      *?    LINEE  DI  ARRIVO  DI  CUI  SOLO  TERMINAL    ?
162100000000      *
162200000000     C                   Z-ADD     1             P
162300000000     C     P             DOWLE     50
162400000000      *
162500000000     C     CLLR(P)       IFGT      *ZERO
162600000000     C     P88R(P)       ORGT      *ZERO
162700000000     C     PARR(P)       ORGT      *ZERO
162800090929     C                   CLEAR                   FISAR000
162900000000     C                   Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
163000000000     C                   Z-ADD     SIMFEL        SARTFA                         TFA
163100950214     C                   Z-ADD     DS1ADA        SARDRE                         DATA
163200000000     C                   Z-ADD     *ZERO         SARTFP                         TERM.PARTENZA
163300000000     C                   Z-ADD     LNAR(P)       SARLNA                         LINEA DI ARRIVO
163400000000     C                   MOVE      'R'           SARTRE                         'R'=SOLO TERM.
163500090929     C     FISARK        CHAIN     FISAR01L                           99
163600000000     C                   ADD       CLLR(P)       SARCLL                         COLLI
163700000000     C                   ADD       P88R(P)       SARP88                         PIST.88 NON ARR
163800000000     C                   Z-ADD     *ZERO         SARDIS                         DISGUIDI
163900000000     C                   ADD       PARR(P)       SARPAR                         PARTITI NON SPU
164000000000     C                   Z-ADD     *ZERO         SARPTA                         PESO TASSABILE
164100000000     C                   Z-ADD     *ZERO         SARF35                         FINO A 35 Q.
164200000000     C                   Z-ADD     *ZERO         SARO35                         PESO OLTRE 35 Q
164300000000     C                   Z-ADD     *ZERO         SARVOL                         VOLUME
164400090929     C                   Z-ADD     *ZERO         SARSNF
164500090929     C                   Z-ADD     *ZERO         SARSNA
164600090929     C                   Z-ADD     *ZERO         SARSEF
164700090929     C                   Z-ADD     *ZERO         SARSEA
164800090929     C                   Z-ADD     *ZERO         SARSHF
164900090929     C                   Z-ADD     *ZERO         SARSHA
165000040923     c                   z-add     dateu         sardtr
165100000000      *
165200090929     C   99              WRITE     FISAR000
165300090929     C  N99              UPDATE    FISAR000
165400000000     C                   ENDIF
165500000000     C                   ADD       1             P
165600000000      *
165700000000     C                   ENDDO
165800000000      *
165900000000     C                   ENDSR
166000000000      **************************************************************************
166100000000      * Azzeramento delle schiere di elaborazione
166200000000      **************************************************************************
166300000000     C     AZZSCH        BEGSR
166400970605     C*
166500970605     C* PRIMA DI CLEARARE LNA/LNAR MEMORIZZO IN UN'ALTRA SCHIERA LE
166600970605     C* LINEE ELABORATE
166700040130     C***                EXSR      AGGLNT
166800000000      *
166900000704     C                   CLEAR                   $B
167000000000     C                   CLEAR                   TFP                            TERMINAL PARTEN
167100000000     C                   CLEAR                   CLL                            TOT COLLI
167200000000     C                   CLEAR                   P88                            PIST.88 NON ARR
167300000000     C                   CLEAR                   DIS                            DISGUIDI
167400000000     C                   CLEAR                   PAR                            PARTITI NON SPU
167500000000     C                   CLEAR                   F35                            FINE  35 Q.LI
167600000000     C                   CLEAR                   O35                            OLTRE 35 Q.LI
167700000000     C                   CLEAR                   PTA                            PESO TASSABILE
167800000000     C                   CLEAR                   VOL                            VOLUME
167900090929     C                   CLEAR                   FN                             FRANCO C+D
168000090929     C                   CLEAR                   FE                             FRANCO "E"
168100090929     C                   CLEAR                   FH                             FRANCO "H"
168200090929     C                   CLEAR                   AN                             ASSEGN C+D
168300090929     C                   CLEAR                   AE                             ASSEGN "E"
168400090929     C                   CLEAR                   AH                             ASSEGN "H"
168500000000      *
168600940720     C                   Z-ADD     *ZERO         $A                             CONTATORE LNA
168700000000     C                   CLEAR                   LNA                            LINEE ARRIVO
168800000000     C                   CLEAR                   CLLA                           TOT COLLI
168900000000     C                   CLEAR                   P88A                           PIST.88 NON ARR
169000000000     C                   CLEAR                   DISA                           DISGUIDI
169100000000     C                   CLEAR                   PARA                           PARTITI NON SPU
169200000000     C                   CLEAR                   F35A                           FINE  35 Q.LI
169300000000     C                   CLEAR                   O35A                           OLTRE 35 Q.LI
169400000000     C                   CLEAR                   PTAA                           PESO TASSABILE
169500000000     C                   CLEAR                   VOLA                           VOLUME
169600090929     C                   CLEAR                   FNA                            FRANCO C+D
169700090929     C                   CLEAR                   FEA                            FRANCO "E"
169800090929     C                   CLEAR                   FHA                            FRANCO "H"
169900090929     C                   CLEAR                   ANA                            ASSEGN C+D
170000090929     C                   CLEAR                   AEA                            ASSEGN "E"
170100090929     C                   CLEAR                   AHA                            ASSEGN "H"
170200000000      *
170300000000     C                   CLEAR                   TFPT                           TERMINAL PARTEN
170400000000     C                   CLEAR                   CLLT                           TOT COLLI
170500000000     C                   CLEAR                   P88T                           PIST.88 NON ARR
170600000000     C                   CLEAR                   PART                           PARTITI NON SPU
170700000000     C                   CLEAR                   F35T                           FINE  35 Q.LI
170800000000     C                   CLEAR                   O35T                           OLTRE 35 Q.LI
170900000000     C                   CLEAR                   PTAT                           PESO TASSABILE
171000000000     C                   CLEAR                   VOLT                           VOLUME
171100090929     C                   CLEAR                   FNT                            FRANCO C+D
171200090929     C                   CLEAR                   FET                            FRANCO "E"
171300090929     C                   CLEAR                   FHT                            FRANCO "H"
171400090929     C                   CLEAR                   ANT                            ASSEGN C+D
171500090929     C                   CLEAR                   AET                            ASSEGN "E"
171600090929     C                   CLEAR                   AHT                            ASSEGN "H"
171700000000      *
171800000000     C                   CLEAR                   LNAR                           LINEA ARRIVO
171900000000     C                   CLEAR                   CLLR                           TOT COLLI
172000000000     C                   CLEAR                   P88R                           PIST.88 NON ARR
172100000000     C                   CLEAR                   PARR                           PARTITI NON SPU
172200000704     C                   CLEAR                   $X
172300000000      *
172400000000     C                   CLEAR                   CLLD                           TOT COLLI
172500000000     C                   CLEAR                   P88D                           PIST.88 NON ARR
172600000000     C                   CLEAR                   PARD                           PARTITI NON SPU
172700000000     C                   CLEAR                   F35D                           FINE  35 Q.LI
172800000000     C                   CLEAR                   O35D                           OLTRE 35 Q.LI
172900000000     C                   CLEAR                   PTAD                           PESO TASSABILE
173000000000     C                   CLEAR                   VOLD                           VOLUME
173100090929     C                   CLEAR                   FND                            FRANCO C+D
173200090929     C                   CLEAR                   FED                            FRANCO "E"
173300090929     C                   CLEAR                   FHD                            FRANCO "H"
173400090929     C                   CLEAR                   AND                            ASSEGN C+D
173500090929     C                   CLEAR                   AED                            ASSEGN "E"
173600090929     C                   CLEAR                   AHD                            ASSEGN "H"
173700000000      *
173800000000     C                   ENDSR
173900000000      **************************************************************************
174000000000      * Verifico i colli non spuntati in partenza e non arrivati, identificabili
174100000000      * con cod.pistola 88.
174200000000      **************************************************************************
174300000000     C     NONARR        BEGSR
174400000000      *
174500000000     C                   Z-ADD     *ZERO         $B                             CONTATORE TFP
174600000000     C                   Z-ADD     *ZERO         $X                             CONTATORE LNAR
174700000000     C                   Z-ADD     *ZERO         $TFAPR                         PRECEDENTE
174800000000     C                   Z-ADD     *ZERO         $PRIMO                         PRIMO GIRO
174900040128     c* primo giro: elaboro tutte le linee da elaborare --> schiera lel
175000040128     c                   movea     lel           lela
175100040128     c                   clear                   wsololna          1
175200040129    1c                   do        2
175300000000      *
175400040128      * Leggo tutti i fogli arrivi del giorno
175500000000      * Il ciclo di lettura deve essere ripetuto 2 volte (una con tipo 2 ed una
175600000000      * con tipo 6)
175700000000     C                   Z-ADD     2             WNPG                           2=ARRIVI
175800040129    2C                   DO        2
175900040128      * devo ciclare per tutti i p.o. da elaborare
176000020918     c                   Clear                   xx
176100040129    3c                   do        60            xx
176200040129    4c                   If        lela(xx) = *Zeros
176300020912     c                   Leave
176400040129    4c                   EndIf
176500040129    4c     wsololna      ifeq      *blanks
176600040129     c     lela(xx)      lookup    l1                                     99
176700040205     c                   else
176800040206     c     xx            occur     dslel3
176900040129    4c                   endif
177000040129     c* Devo leggere i fogli abbinati dalla linea solo se sono nel secondo
177100040129     c* giro (elaborazione fogli di lel2) oppure se sono nel primo giro
177200040129     c* purchè la linea da elaborare sia in £1
177300040129     c* Es.: se sto elaborando per 005,nella schiera lel c'è anche 050 che
177400040129     c* è un po terminal e un pò no ma in questo caso non mi serve andare
177500040129     c* a leggere le spunte dei f.v.p. abbinati da 50 perchè tali dati
177600040129     c* dovranno uscire nelle stampe per terminal di 050 e non di 005
177700040129    4c     wsololna      ifeq      *blanks
177800040129     c     *in99         andeq     *on
177900040129     c     wsololna      orne      *blanks
178000950214     C                   Z-ADD     DS1ADA        WDFV                           DATA INIZIALE
178100020912     C**!!!              Z-ADD     SIMFEL        WFLE                           FIL.ELABORATORE
178200040128     c                   Z-Add     lela(xx)      Wfgs
178300020912     C     FNFVVK        SETLL     FNFVV02L
178400040129    5C                   DO        *HIVAL
178500020912     C     FNFVVK        READE     FNFVV02L                               31
178600040129    6C     *IN31         IFEQ      *OFF
178700000000      *
178800000000      * Avendo il f.v. arrivi leggo tutti i f.v. partenze abbinati
178900000000     C                   Z-ADD     FVVNPG        FVANPG                         CATEG.F.V.ARR.
179000000000     C                   Z-ADD     FVVNFV        FVANFA                         NR.F.VIAGGIO
179100020917     c                   Z-Add     FvvFgs        FvaLai
179200040128     c*
179300040128     c*
179400941229     C     FNFVAK        SETLL     FNFVA02L
179500040129     c                   If        %Equal(Fnfva02l)
179600040129     c                   clear                   trul09ds
179700040129     c                   eval      i09mod = 'TFU'
179800040129     c                   eval      i09tfa = fvalai
179900040129     c                   eval      i09dta = ds1ada
180000040129     c                   call      'TRUL09R'
180100040129     c                   parm                    trul09ds
180200040129     c                   eval      SFU = t09
180300040129     c*
180400040129     c                   clear                   trul09ds
180500040129     c                   eval      i09mod = 'TFA'
180600040129     c                   eval      i09tfa = fvalai
180700040129     c                   eval      i09dta = ds1ada
180800040129     c                   call      'TRUL09R'
180900040129     c                   parm                    trul09ds
181000040129     c                   eval      SFA = t09
181100040129     c                   endif
181200040129    7C                   DO        *HIVAL
181300941229     C     FNFVAK        READE     FNFVA02L                               32
181400040129    8C  N32              DO
181500000000      *
181600970606     C* Determino se è un foglio di defluenza
181700970606     C                   MOVE      'N'           WDEFL             1
181800040129    9C     FVATTR        IFNE      *BLANKS
181900970606     C     FVATTR        LOOKUP    TTR                                    99
182000970606     C   99              MOVE      'S'           WDEFL
182100040129    9C                   END
182200000504     C* FILE DI ESTENSIONE
182300000504     C     KFWA          CHAIN     FNFWA01L                           39
182400040129    9C     *IN39         IFEQ      *ON
182500000504     C     FWAATB        ORNE      ' '
182600000504     C                   CLEAR                   FWAFF3
182700000504     C                   CLEAR                   FWAFF4
182800000504     C                   CLEAR                   FWAFL3
182900000504     C                   CLEAR                   FWAFL4
183000040129    9c                   ENDIF
183100070117      * Devo elaborare FNBRV02L solo con quel f.v. per quella linea di scambio
183200000000     C                   Z-ADD     1             $E                3 0
183300040129    9C     FFV($E)       DOWGT     *ZERO
183400000000     C                   MOVE      FVALAI        $COMO1            3            LNA CUI INVIO
183500040205     c     wsololna      ifne      *blanks
183600040205     c     ffv($e)       lookup    lel3                                   98
183700040205     c                   endif
183800040129     c* se sono nel primo giro elaboro la linea del foglio se
183900040129     c* lai è la linea del foglio o il p.o. di scarico
184000040129   10c     wsololna      ifeq      *blanks
184100040129     C     FSP($E)       andeq     $COMO1                                       FIL.SCARICO
184200040129     c     wsololna      oreq      *blanks
184300040129     C     FFV($E)       andeq     $COMO1                                       LINEA ARRIVO
184400040129     c* se sono nel secondo giro elaboro la linea del foglio solo se
184500040129     c* la stessa è il secondo liv. in arrivo di lai presente nella
184600040129     c* schiera delle linee da elaborare
184700040129     c* (nel caso di 018 devo leggere i fogli abbinati da 001 ed elabo-
184800040129     c* rare solo la linea 018)
184900040129     c     wsololna      orne      *blanks
185000040205     c     *in98         andeq     *on
185100000000      *
185200960926      * Cerco  il terminal di arrivo
185300000000     C                   Z-ADD     FVALNP        $LINEP            3 0          LINEA PARTENZA
185400000000     C                   MOVE      FFV($E)       $LINEA            3 0          LINEA ARRIVO
185500000000     C                   EXSR      TROTFA                                       TROVA TFA
185600000000      *
185700000000      * Memorizzo il TFA e la LNA perche' mi servono dopo
185800000000     C                   Z-ADD     $LINEA        $TERAR            3 0          TERMINAL ARRIVO
185900000000     C                   MOVE      FFV($E)       $LNARR            3 0          LINEA ARRIVO
186000000000      *
186100000000      * Se ne TFA ($TERAR) ne LNA ($LINEA) non sono presenti in tab. '£1'
186200000000      * si tratta di tansito; quindi ignoro TFA e sostituisco con SIMFEL
186300000000     C                   MOVE      *BLANKS       $SOLO             1            NO SOLO TERMIN.
186400000000     C                   CLEAR                   $TRANS
186500040128     C***  $TERAR        LOOKUP    L1                                     99     ON=TROVATO
186600040128     C**N99$LNARR        LOOKUP    L1                                     99     ON=TROVATO
186700040128     C**N99              Z-ADD     SIMFEL        $TERAR                         E' TRANSITO
186800040128     C**** *IN99         IFEQ      *OFF
186900040130   11c     $lnarr        ifne      fvalai
187000040130   12c     $terar        ifne      fvalai
187100040128     C***                Z-ADD     SIMFEL        $TERAR                         E' TRANSITO
187200040130     C                   move      fsp($E)       $TERAR                         E' TRANSITO
187300000000     C                   MOVE      'T'           $TRANS                         SI TRANSITO
187400000000      *
187500000000      * Determino se LNA ($LINEA) e' SOLO terminal di arrivo
187600040129  x12C                   ELSE                                                   NO TRANSITO
187700040128     C***  $LNARR        LOOKUP    L1                                     99     ON=TROVATO
187800040129     C     $LNARR        LOOKUP    SFU                                    99     ON=TROVATO
187900000000     C  N99              MOVE      'R'           $SOLO                          SOLO TERMINAL A
188000040129   12c                   endif
188100040129   11C                   ENDIF
188200000000      *
188300000000      * Con il f.v. partenze leggo tutte le spunte
188400941229     C                   Z-ADD     1             SPPNPG                         CATEGORIA
188500941229     C                   Z-ADD     FVALNP        SPPFGS                         LINEA PARTENZA
188600000000     C                   Z-ADD     FVANFV        SPPNFV                         NR.F.VIAGGIO
188700000000     C                   MOVE      FFV($E)       SPPLNA                         LINEA ARRIVO
188800070117     C     FNSPPK        SETLL     FNBRV02L
188900040129   11C                   DO        *HIVAL
189000070117     C     FNSPPK        READE     FNBRV02L                               33
189100040129   12C     *IN33         IFEQ      *OFF
189200000000      *
189300000000      *-------------------------------------------------------------------------
189400000000      *    ?  CONTROLLO I COLLI PARTITI MA NON SPUNTATI IN ARRIVO  ?
189500940607      *
189600940607      * escludo i fogli illuminati perche sono del terminal
189700040129   13C     FVAATR        IFNE      'T'
189800970513     C**
189900970606      * escludo le defluenze
190000970606     C     WDEFL         ANDEQ     'N'
190100000000     C                   EXSR      PARARR                                       PARTITI NON SPU
190200040129   13C                   ENDIF
190300000000      *-------------------------------------------------------------------------
190400000000      *
190500980402      * Proseguo solo se la spunta e' FASULLA (88 89 98 96)
190600980402      * IGNORO SE ESISTE UN'ALTRA SPUNTA CON DATA > VISTO CHE L'HO IGNO
190700980402      *  RATA ANCHE NEL CALCOLO DEI PARTITI NOPN SPUNTATI IN ARRIVO
190800980402     C     SPPNPS        LOOKUP    NPF                                    38
190900040129   13C     *IN38         IFEQ      *ON
191000980402     C     WIGNOR        ANDEQ     ' '
191100000000     C                   MOVE      'S'           $OKSN             1            SI  CONTEGGIO
191200000000      *
191300000000      * leggo il segnacollo
191400000000     C                   Z-ADD     SPPLNP        ARTFLS                         FIL.SEGNACOLLLO
191500000000     C                   Z-ADD     SPPLNA        ARTLNA                         FIL.ARRIVO
191600000000     C                   Z-ADD     SPPNRS        ARTNRS                         NR.SERIE SPED.
191700941230     C                   Z-ADD     SPPNSC        ARTNSC                         NR.SEGNACOLLO
191800040129   14C     $TRANS        IFNE      'T'                                          NO TRANSITO
191900000000      *
192000941229      * Se non e' transito leggo FNART e controllo la data arrivi (ARTDAM)
192100941229     C     FNARTK        CHAIN     FNART27L                           30
192200040129   15C  N30ARTDAM        IFGT      *ZEROS
192300970502     C     ARTDAM        ANDLE     DS1ADA                                       DATA ARRIVO
192400000000     C                   MOVE      'N'           $OKSN                          NO  CONTEGGIO
192500040129   15C                   ENDIF
192600000000      *
192700040129  x14C                   ELSE
192800941229      * Se e' transito leggo FNBTT e controllo la data entrata al trans.(BTTDET)
192900020823     C     FNARTK1       CHAIN     FNBTT37L                           30
193000040129   15C  N30BTTDET        IFGT      0
193100970502     C     BTTDET        ANDLE     DS1ADA                                       DATA ARRIVO
193200000000     C                   MOVE      'N'           $OKSN                          NO  CONTEGGIO
193300040129   15C                   ENDIF
193400040129   14C                   ENDIF
193500000000      *
193600970502      * SE non ho trovato la BOLLA
193700040129   14C     *IN30         IFEQ      *ON
193800000000      *
193900000000      * Se trovo almeno una spunta devo scartare dalle statistiche
194000000000     C                   Z-ADD     SPPLNP        BRVLNP                         FIL.SEGNACOLLO
194100000000     C                   Z-ADD     SPPLNA        BRVLNA                         FIL.ARRIVO
194200000000     C                   Z-ADD     SPPNRS        BRVNRS                         NR.SERIE SPED.
194300000000     C                   Z-ADD     SPPNSC        BRVNSC                         NR.SEGNACOLLO
194400000000      *
194500070117     C     KBRV          CHAIN     FNBRV07L                           30
194600040129   15C     *IN30         DOWEQ     *OFF
194700970502     C     $OKSN         ANDEQ     'S'
194800040128     c* se p.o. gestione spunta è un p.o. dell'area di arrivo del p.o. che
194900040128     c* sto elaborando --> la spunta è da elaborare altrimenti devo continua
195000040128     c* re a leggere
195100040129     c     $trans        ifeq      'T'
195200040130     c     $terar        oreq      fvalai
195300040129     c     brvfgs        lookup    SFA                                    30
195400040129     c                   else
195500040129     c                   clear                   trul09ds
195600040129     c                   eval      i09mod = 'TFA'
195700040129     c                   eval      i09tfa = $terar
195800040129     c                   eval      i09dta = ds1ada
195900040129     c                   call      'TRUL09R'
196000040129     c                   parm                    trul09ds
196100040129     c     brvfgs        lookup    t09                                    30
196200040129     c                   endif
196300970502     C*
196400040129   16c     *in30         ifeq      *on
196500040203     c* se spunta partenza ignoro se fgs non è in £1
196600040203     c     brvnpg        ifeq      1
196700040203     c     brvfgs        lookup    l1                                     30
196800040203     c                   endif
196900040203  16ac     brvnpg        ifne      1
197000040203     c     *in30         oreq      *on
197100970502     C                   CLEAR                   DSLV53
197200000204     C                   Z-ADD     BRVNFV        D53NFV
197300970502     C                   MOVEL     SIMFEL        D53FEL
197400970502     C                   MOVEL     BRVNPG        D53NPG
197500970502     C                   MOVEL     BRVFGS        D53FGS
197600970502     C                   CALL      'FNLV53R'
197700970502     C                   PARM                    DSLV53
197800970502     C*
197900970502     C* SE NON C'E' ERRORE E DATA <= DATA ELABORAZIONE --> E' OK
198000970502     C*   ESISTE LA SPUNTA
198100040129   17C     D53ERR        IFEQ      ' '
198200970502     C     D53DFV        ANDLE     DS1ADA
198300000000     C                   MOVE      'N'           $OKSN                          NO CONTEGGIO
198400040129  x17C                   ELSE
198500070117     C     KBRV          READE     FNBRV07L                               30
198600040129   17C                   ENDIF
198700040203 x16ac                   else
198800070117     C     KBRV          READE     FNBRV07L                               30
198900040203  16ac                   endif
199000040129  x16c                   else
199100070117     C     KBRV          READE     FNBRV07L                               30
199200040129   16c                   endif
199300040129   15C                   ENDDO
199400000000      *
199500040129   14C                   ENDIF                                                  30 ON
199600000000      *
199700000000      * Se SI CONTEGGI ($OKSN='S')  aggiorno la statistica             ' errore
199800040129   14C     $OKSN         IFEQ      'S'
199900000000      *
200000000000      * Come per le bolle rompo quando cambia terminal di arrivo
200100040129   15C     $TERAR        IFNE      $TFAPR                                       PRECEDENTE
200200000000     C     $PRIMO        ANDNE     *ZERO                                        NO PRIMO GIRO
200300000000     C                   EXSR      SCASCH                                       SCARICA SCHIERE
200400000000     C                   EXSR      SCASCT                                       SCARICA SCHIERE
200500000000     C                   EXSR      SCASCR                                       SCARICA SCHIERE
200600000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
200700980708     C                   Z-ADD     0             $B
200800000000     C                   Z-ADD     $TERAR        $TFAPR                         PRECEDENTE
200900040129   15C                   ENDIF
201000000000      *
201100000000      * Cerco l'indice di schiera per il terminal di partenza
201200000000     C                   Z-ADD     1             D
201300000000     C     SPPLNP        LOOKUP    ORG(D)                                 99     ON=TROVATO
201400000000     C   99              Z-ADD     OTP(D)        $LINEA
201500000000     C  N99              MOVEL     SPPLNP        $LINEA
201600000000     C                   EXSR      INDTFP                                       INDICE TFP (B)
201700000000      *
201800040129   15C     $TRANS        IFEQ      'T'                                          'T'=TRANSITO
201900000000     C                   ADD       1             P88T(B)                        TFPT
202000000000     C                   ADD       1             P88D              7 0
202100040129  x15C                   ELSE
202200000000      *
202300040129   16C     $SOLO         IFEQ      'R'                                          SOLO TERMINAL
202400000000     C                   Z-ADD     $LNARR        $LINEA                         LINEA ARRIVO
202500000000     C                   EXSR      INDSOL                                       INDICE SCHIERA
202600000000     C                   ADD       1             P88R(X)                        LNA
202700040129  x16C                   ELSE                                                   NO SOLO TERMIN.
202800000000      *
202900000000      * Cerco l'indice di schiera per la linea di arrivo
203000000000     C                   Z-ADD     SPPLNA        $LINEA
203100000000     C                   EXSR      INDLNA                                       INDICE LNA
203200000000     C                   ADD       1             P88(B)                         TFP
203300000000     C                   ADD       1             P88A(A)                        LNA
203400040129   16C                   ENDIF
203500000000      *
203600040129   15C                   ENDIF
203700000000      *
203800000000      * Solo al primo giro imposto TFA precedente
203900040129   15C     $PRIMO        IFEQ      *ZERO                                        0=PRIMO GIRO
204000000000     C                   Z-ADD     $TERAR        $TFAPR                         PRECEDENTE
204100000000     C                   Z-ADD     1             $PRIMO                         1=GIA' PASSATO
204200000000     C                   Z-ADD     1             $DTAAR
204300040129   15C                   ENDIF
204400040129   14C                   ENDIF                                                  $OKSN='S'
204500000000      *
204600040129   13C                   ENDIF                                                  PIST 88
204700000000      *
204800000000      * solo terminal
204900000000      *
205000040129   13C     $SOLO         IFEQ      'R'                                          SOLO TERMINAL
205100000000      *
205200000000      * Se trovo almeno una spunta devo incrementare il nr. colli
205300000000     C                   Z-ADD     SPPLNP        BRVLNP                         FIL.SEGNACOLLO
205400000000     C                   Z-ADD     SPPLNA        BRVLNA                         FIL.ARRIVO
205500000000     C                   Z-ADD     SPPNRS        BRVNRS                         NR.SERIE SPED.
205600000000     C                   Z-ADD     SPPNSC        BRVNSC                         NR.SEGNACOLLO
205700970502     C*
205800070117     C     KBRV          CHAIN     FNBRV07L                           34
205900040129   15C     *IN34         DOWEQ     *OFF
206000040129     c     brvfgs        lookup    SFA                                    34
206100040129   16c     *in34         ifeq      *on
206200040203     c* se spunta partenza ignoro se fgs non è in £1
206300040203     c     brvnpg        ifeq      1
206400040203     c     brvfgs        lookup    l1                                     34
206500040203     c                   endif
206600040203  16ac     brvnpg        ifne      1
206700040203     c     *in34         oreq      *on
206800970502     C                   CLEAR                   DSLV53
206900000204     C                   Z-ADD     BRVNFV        D53NFV
207000970502     C                   MOVEL     SIMFEL        D53FEL
207100970502     C                   MOVEL     BRVNPG        D53NPG
207200970502     C                   MOVEL     BRVFGS        D53FGS
207300970502     C                   CALL      'FNLV53R'
207400970502     C                   PARM                    DSLV53
207500970502     C*
207600970502     C* SE NON C'E' ERRORE E DATA = DATA ELABORAZIONE --> E' OK
207700970502     C*   ESISTE LA SPUNTA
207800040129   17C     D53ERR        IFEQ      ' '
207900970502     C     D53DFV        ANDEQ     DS1ADA
208000000000     C                   SETON                                        34        C'E' COLLO
208100970502      * Se trovata spunta incremento il numero dei colli
208200970502     C                   Z-ADD     $LNARR        $LINEA                         LINEA ARRIVO
208300970502     C                   EXSR      INDSOL                                       INDICE SCHIERA
208400970502     C                   ADD       1             CLLR(X)                        NR.COLLI
208500970502     C*
208600040129  x17C                   ELSE
208700070117     C     KBRV          READE     FNBRV07L                               34
208800040129   17C                   ENDIF
208900040203 x16ac                   else
209000070117     C     KBRV          READE     FNBRV07L                               34
209100040203  16ac                   endif
209200040129  x16c                   else
209300070117     C     KBRV          READE     FNBRV07L                               34
209400040130   16c                   endif
209500040130   15C                   ENDDO
209600000000      *
209700040129   13C                   ENDIF                                                  *IN33=OFF
209800040130   12C                   ENDIF                                                  FSP,$E=$COMO1
209900040130   11C  N33              ENDDO                                                  *HIVAL
210000000000      *
210100040129   10c                   endif
210200000000     C                   ADD       1             $E
210300040129    9C                   ENDDO                                                  DOW ... $E
210400000000      *
210500040129    8C                   ENDDO
210600040129    7C  N32              ENDDO                                                  *HIVAL
210700000000      *
210800040129    6C                   ENDIF
210900040129    5C  N31              ENDDO                                                  *HIVAL
211000020912
211100040129    4c                   endif
211200040129    3c                   EndDo                                                  L1
211300000000      *
211400000000     C                   Z-ADD     6             WNPG
211500040129    2C                   ENDDO                                                  ... 1 DO 2 ...
211600040128     c                   movea     lel2          lela
211700040128     c                   move      'S'           wsololna
211800040129    1C                   ENDDO                                                  ... 1 DO 2 ...
211900000000      *
212000000000      * Alla fine scarico le ultime schiere ancora in mermoria
212100000000     C                   EXSR      SCASCH                                       SCARICA SCHIERE
212200000000     C                   EXSR      SCASCT                                       SCARICA SCHIERE
212300000000     C                   EXSR      SCASCR                                       SCARICA SCHIERE
212400000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
212500000000      *
212600000000     C                   ENDSR
212700000000      **************************************************************************
212800000000      * Controllo i colli partiti ma non spuntati al terminal di arrivo
212900000000      **************************************************************************
213000000000     C     PARARR        BEGSR
213100000000      *
213200000000      * Cerco fra le spunte partenze, se lo trovo passo oltre
213300970502     C                   MOVEL     'N'           WSPU              1
213400970513     C                   CLEAR                   WIGNOR
213500941229     C                   Z-ADD     1             BRVNPG                         1=PARTENZE
213600941229     C                   Z-ADD     SPPLNP        BRVLNP                         LINEA PARTENZA
213700941229     C                   Z-ADD     SPPLNA        BRVLNA                         LINEA ARRIVO
213800941229     C                   Z-ADD     SPPNRS        BRVNRS                         NR.SERIE SEGNAC
213900941229     C                   Z-ADD     SPPNSC        BRVNSC                         NR.SEGNACOLLO
214000970513     C*
214100970513     C* SE TROVO UN'ALTRA SPUNTA PARTENZA CON DATA > IGNORO QUESTA
214200070117     C     KBRV9         CHAIN     FNBRV09L                           99
214300970513     C*
214400000000     C* E' STATA SPARATA SOLO SE DATA FOGLIO <= A DATA ELABORAZIONE
214500970502     C* ESCLUDO SE SI TRATTA DI SPUNTA LOCALE CONSIDERO SOLO LA DEFLUZ
214600970502    1C     *IN99         DOWEQ     *OFF
214700970513     C**
214800970513     C** NON DEVE ESSERE LA SPUNTA CHE STO GIA' LEGGENDO
214900970513    2C     BRVNFV        IFNE      SPPNFV
215000970513     C     BRVFGS        ORNE      SPPFGS
215100970513     C**
215200970502     C                   CLEAR                   DSLV53
215300000204     C                   Z-ADD     BRVNFV        D53NFV
215400970513     C                   MOVEL     BRVFGS        D53FGS
215500970513     C                   MOVEL     BRVNPG        D53NPG
215600970513     C                   MOVEL     SIMFEL        D53FEL
215700970502     C                   CALL      'FNLV53R'
215800970502     C                   PARM                    DSLV53
215900970502     C*
216000970513    3C     D53ERR        IFEQ      ' '
216100970513     C**
216200970513     C* NON C'E' ERRORE E FOGLIO DI DEFLUENZA: LA DATA DEVE ESSERE
216300970513     C*  <= ALLA DATA ELABORAZIONE
216400040302    4C***  SIMFEL        IFEQ      BRVFLE
216500061120     c***  $terar        ifeq      brvfle
216600061120     c     $terar        ifeq      brvfgs
216700980402     C**
216800980402     C* VERIFICO CHE NON SI TRATTI DI PISTOLA AUTOGENERATA
216900980402    5C     BRVNPS        LOOKUP    NPS                                    98
217000980402     C     *IN98         IFEQ      *OFF
217100970513     C*
217200980402    6C     $TRANS        IFNE      'T'
217300980402    7C     D53DEF        IFEQ      'S'
217400970502     C     D53DFV        ANDLE     DS1ADA
217500970502     C                   MOVEL     'S'           WSPU
217600970502     C                   SETON                                        99
217700980402    7C                   ENDIF
217800980402     C**
217900980402   X6C                   ELSE
218000980402     C                   MOVEL     'S'           WSPU
218100980402     C                   SETON                                        99
218200980402    6C                   ENDIF
218300980402    5C                   ENDIF
218400970513     C**
218500970513   X4C                   ELSE
218600970513     C* IGNORO SE SI C'E' UN'ALTRA SPUNTA CON DATA >
218700970513    5C     D53DFV        IFGT      FVADFV
218800970513     C                   MOVEL     'S'           WIGNOR            1
218900970513     C                   SETON                                        99
219000970513    5C                   ENDIF
219100970513    4C                   ENDIF
219200970513    3C                   ENDIF
219300970513    2C                   ENDIF
219400970513     C**
219500070117     C  N99KBRV9         READE     FNBRV09L                               99
219600970502    1C                   ENDDO
219700970513     C**
219800970513     C* SPUNTA TROVATA  O DA IGNORARE
219900970502    1C     WSPU          IFEQ      'S'
220000970513    1C     WIGNOR        OREQ      'S'
220100970502     C                   GOTO      ENDPAR
220200970502    1C                   ENDIF
220300000000      *
220400000000      * Cerco fra le spunte fogli vari, con categoria 2. Se non lo trovo passo
220500000000     C                   Z-ADD     2             BRVNPG                         2=ARRIVI 1° LVL
220600070117     C     FNBRVK        CHAIN     FNBRV09L                           99
220700000000     C* E' STATA SPARATA SOLO SE DATA FOGLIO = A DATA ELABORAZIONE
220800970502    1C     *IN99         DOWEQ     *OFF
220900040129     c     sfa(1)        ifgt      *zeros
221000040129     c     brvfgs        lookup    sfa                                    99
221100040129     c                   else
221200040129     c* per i diretti ai secondi liv in arrivo
221300040129     c     brvfgs        comp      $lnarr                                 99
221400040129     c                   endif
221500040129     c     *in99         ifeq      *on
221600970502     C                   CLEAR                   DSLV53
221700000204     C                   Z-ADD     BRVNFV        D53NFV
221800970502     C                   MOVEL     BRVNPG        D53NPG
221900970502     C                   MOVEL     BRVFGS        D53FGS
222000970502     C                   MOVEL     SIMFEL        D53FEL
222100970502     C                   MOVEL     'V'           D53TFO
222200970502     C                   CALL      'FNLV53R'
222300970502     C                   PARM                    DSLV53
222400970502     C*
222500970502     C* NON C'E' ERRORE E FOGLIO DI DEFLUENZA: LA DATA DEVE ESSERE
222600970502     C*  <= ALLA DATA ELABORAZIONE
222700970502    2C     D53ERR        IFEQ      ' '
222800970502     C     D53DFV        ANDLE     DS1ADA
222900970502     C                   MOVEL     'S'           WSPU
223000970502     C                   SETON                                        99
223100970502   X2C                   ELSE
223200070117     C     FNBRVK        READE     FNBRV09L                               99
223300970502    2C                   ENDIF
223400040129     c                   else
223500070117     C     FNBRVK        READE     FNBRV09L                               99
223600040129     c                   endif
223700970502    1C                   ENDDO
223800970502     C* SPUNTA TROVATA
223900970502    1C     WSPU          IFEQ      'S'
224000970502     C                   GOTO      ENDPAR
224100970502    1C                   ENDIF
224200000000      *
224300070117      * Se la filiale di scarico (FSP,B) e' di 2° livello cerco (in FNBRV)
224400000000      * anche con categoria 6 (arrivi 2° livelli)
224500040129     C***                MOVE      FSP($E)       $COMO2            3 0
224600040129     C***  $COMO2        LOOKUP    L1                                     99     ON=TROVATO
224700940607     c* oppure se linea di arrivo e' un 2 livello della L1
224800040129     C***                MOVE      FFV($E)       $COMO3            3 0
224900040129     C***  $COMO3        LOOKUP    L1                                     98     ON=TROVATO
225000940607     C*
225100040129    1C***  *IN99         IFEQ      *ON
225200040129     C***  $COMO2        ANDNE     SIMFEL
225300940607     C*
225400040129     C***  *IN98         OREQ      *ON
225500040129     C***  $COMO3        ANDNE     SIMFEL
225600940607     C*
225700000000     C                   Z-ADD     6             BRVNPG                         6=ARRIVI 2° LVL
225800070117     C     FNBRVK        CHAIN     FNBRV09L                           99
225900970502     C* E' STATA SPARATA SOLO SE DATA FOGLIO <= A DATA ELABORAZIONE
226000970502    2C     *IN99         DOWEQ     *OFF
226100040129    3c     sfa(1)        ifgt      *zeros
226200040129     c     brvfgs        lookup    sfa                                    99
226300040129     c                   else
226400040129     c* per i diretti ai secondi liv in arrivo
226500040129     c     brvfgs        comp      $lnarr                                 99
226600040129    3c                   endif
226700040129    3c     *in99         ifeq      *on
226800970502     C                   CLEAR                   DSLV53
226900000204     C                   Z-ADD     BRVNFV        D53NFV
227000970502     C                   MOVEL     BRVNPG        D53NPG
227100970502     C                   MOVEL     BRVFGS        D53FGS
227200970502     C                   MOVEL     SIMFEL        D53FEL
227300970502     C                   MOVEL     'V'           D53TFO
227400970502     C                   CALL      'FNLV53R'
227500970502     C                   PARM                    DSLV53
227600970502     C*
227700970502     C* NON C'E' ERRORE E FOGLIO DI DEFLUENZA: LA DATA DEVE ESSERE
227800970502     C*  <= ALLA DATA ELABORAZIONE
227900970502    3C     D53ERR        IFEQ      ' '
228000970502     C     D53DFV        ANDLE     DS1ADA
228100970502     C                   MOVEL     'S'           WSPU
228200970502     C                   SETON                                        99
228300970502   X3C                   ELSE
228400070117     C     FNBRVK        READE     FNBRV09L                               99
228500970502    3C                   ENDIF
228600040129   x3c                   else
228700070117     C     FNBRVK        READE     FNBRV09L                               99
228800040129     c                   endif
228900970502    2C                   ENDDO
229000970502     C* SPUNTA TROVATA
229100970502    2C     WSPU          IFEQ      'S'
229200970502     C                   GOTO      ENDPAR
229300970502    2C                   ENDIF
229400040129    1C****               ENDIF
229500000000      *
229600000000      * Come per le bolle rompo quando cambia terminal di arrivo
229700000000     C     $TERAR        IFNE      $TFAPR                                       PRECEDENTE
229800000000     C     $PRIMO        ANDNE     *ZERO                                        NO PRIMO GIRO
229900000000     C                   EXSR      SCASCH                                       SCARICA SCHIERE
230000000000     C                   EXSR      SCASCT                                       SCARICA SCHIERE
230100000000     C                   EXSR      SCASCR                                       SCARICA SCHIERE
230200000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
230300980708     C                   Z-ADD     0             $B
230400000000     C                   Z-ADD     $TERAR        $TFAPR                         PRECEDENTE
230500000000     C                   ENDIF
230600000000      *
230700000000      * Cerco l'indice di schiera per il terminal di partenza
230800000000     C                   Z-ADD     1             D
230900000000     C     SPPLNP        LOOKUP    ORG(D)                                 99     ON=TROVATO
231000000000     C   99              Z-ADD     OTP(D)        $LINEA
231100000000     C  N99              Z-ADD     SPPLNP        $LINEA
231200000000     C                   EXSR      INDTFP                                       INDICE TFP (B)
231300000000      *
231400000000     C     $TRANS        IFEQ      'T'                                          'T'=TRANSITO
231500000000     C                   ADD       1             PART(B)                         TFPT
231600000000     C                   ADD       1             PARD              7 0
231700000000     C                   ELSE
231800000000      *
231900000000     C     $SOLO         IFEQ      'R'                                          SOLO TERMINAL
232000000000     C                   Z-ADD     $LNARR        $LINEA                         LINEA ARRIVO
232100000000     C                   EXSR      INDSOL                                       INDICE SCHIERA
232200000000     C                   ADD       1             PARR(X)                        LNA
232300000000     C                   ELSE                                                   NO SOLO TERMIN.
232400940818      *
232500940818      * Cerco l'indice di schiera per la linea di arrivo
232600940818     C                   Z-ADD     SPPLNA        $LINEA
232700940818     C                   EXSR      INDLNA                                       INDICE LNA (A)
232800940818      *
232900000000     C                   ADD       1             PAR(B)                          TFP
233000000000     C                   ADD       1             PARA(A)                         LNA
233100000000     C                   ENDIF
233200000000      *
233300000000     C                   ENDIF
233400000000      *
233500000000      * Solo al primo giro imposto TFA precedente
233600000000     C     $PRIMO        IFEQ      *ZERO                                        0=PRIMO GIRO
233700000000     C                   Z-ADD     $TERAR        $TFAPR                         PRECEDENTE
233800000000     C                   Z-ADD     1             $PRIMO                         1=GIA' PASSATO
233900000000     C                   Z-ADD     1             $DTAAR
234000000000     C                   ENDIF
234100000000      *
234200000000     C     ENDPAR        ENDSR
234300000000      **************************************************************************
234400000000      * La linea passata ($LINEA) viene cercata fra i terminal di partenza
234500000000      * (schiera TFP); se non trovato viene immesso come nuovo terminal.
234600000000      * Infine viene restituito l'indice di schiera (B) ove si trova l'elemento.
234700000000      *
234800000000      * Ricordo che $B e' il contatore della schiera TFP.
234900000000      **************************************************************************
235000000000     C     INDTFP        BEGSR
235100000000      *
235200000000      * Verifico se il TFP appena letto e' nuovo o no
235300000000     C                   Z-ADD     1             B                 3 0
235400000000     C     $TRANS        IFEQ      'T'                                          'T'=TRANSITI
235500000000     C     $LINEA        LOOKUP    TFPT(B)                                99     TERMINAL PART.
235600000000     C  N99              ADD       1             $B                             CONTATORE TFP
235700000000     C  N99              Z-ADD     $B            B
235800000000     C  N99              Z-ADD     $LINEA        TFPT(B)                        NUOVA TFP
235900000000     C                   ELSE
236000000000     C     $LINEA        LOOKUP    TFP(B)                                 99     TERMINAL PART.
236100000000     C  N99              ADD       1             $B                             CONTATORE TFP
236200000000     C  N99              Z-ADD     $B            B
236300000000     C  N99              Z-ADD     $LINEA        TFP(B)                         NUOVA TFP
236400000000     C                   ENDIF
236500000000      *
236600000000     C                   ENDSR
236700000000      **************************************************************************
236800000000      * La linea passata ($LINEA) viene cercata fra le linee di arrivo di cui
236900000000      * solo terminal di arrivo (schiera LNAR); se non trovata viene immessa.
237000000000      * Infine viene restituito l'indice di schiera (X) ove si trova l'elemento.
237100000000      *
237200000000      * Ricordo che $X e' il contatore della schiera LNAR.
237300000000      **************************************************************************
237400000000     C     INDSOL        BEGSR
237500000000      *
237600000000      * Verifico se il LNAR appena letto e' nuovo o no
237700000000     C                   Z-ADD     1             X                 3 0
237800000000     C     $LINEA        LOOKUP    LNAR(X)                                99     ON=TROVATA
237900000000     C  N99              ADD       1             $X                3 0          CONTATORE LNAR
238000000000     C  N99              Z-ADD     $X            X
238100000000     C  N99              Z-ADD     $LINEA        LNAR(X)                        NUOVA LNAR
238200000000      *
238300000000     C                   ENDSR
238400000000      **************************************************************************
238500000000      * La linea passata ($LINEA) viene cercata fra le linee di arrivo
238600000000      * (schiera LNA); se non trovata viene immessa come nuova linea.
238700000000      * Infine viene restituito l'indice di schiera (A) ove si trova l'elemento.
238800000000      *
238900000000      * Ricordo che $A e' il contatore della schiera LNA.
239000000000      **************************************************************************
239100000000     C     INDLNA        BEGSR
239200000000      *
239300000000      * Verifico se la LNA appena letto e' nuova o no
239400000000     C                   Z-ADD     1             A                 3 0
239500000000     C     $LINEA        LOOKUP    LNA(A)                                 99     LINEE ARRIVO
239600000000     C  N99              ADD       1             $A                             CONTATORE LNA
239700000000     C  N99              Z-ADD     $LINEA        LNA($A)                        NUOVA LNA
239800000000     C  N99              Z-ADD     $A            A
239900000000      *
240000000000     C                   ENDSR
240100000000      **************************************************************************
240200000000      * Sono passati in input due campi: linea di partenza ($LINEP) e linea di
240300000000      * arrivo ($LINEA)
240400000000      * Viene restituito il terminal di arrivo (sempre in $LINEA) e pulito il
240500000000      * campo $LINEP.
240600000000      **************************************************************************
240700000000     C     TROTFA        BEGSR
240800000000      *
240900970606     C                   CLEAR                   DSLV55
241000970606     C                   MOVEL     'A'           D55TPT
241100970606     C                   Z-ADD     $LINEA        D55LIN
241200970606     C                   Z-ADD     $LINEP        D55LNP
241300970606     C                   Z-ADD     FVADFV        D55DRF
241400970606     C                   CALL      'FNLV55R'
241500970606     C                   PARM                    DSLV55
241600970606     C*
241700970606     C     D55ERR        IFEQ      *BLANKS
241800970606     C                   Z-ADD     D55TFA        $LINEA
241900970606     C                   END
242000000000      *
242100000000     C                   Z-ADD     *ZERO         $LINEP
242200000000      *
242300000000     C                   ENDSR
242400970605      **************************************************************************
242500970605      * MEMORIZZAZIONE LINEE DI ARRIVO ELABORATE
242600970605      **************************************************************************
242700040130     C***  AGGLNT        BEGSR
242800040130     C***                Z-ADD     1             P
242900040130     C***  P             DOWLE     29
243000040130     C***  LNA(P)        ANDGT     *ZEROS
243100040130     C***  LNA(P)        LOOKUP    LNT                                    98
243200040130     C**N98              Z-ADD     1             PP                2 0
243300040130     C**N98000           LOOKUP    LNT(PP)                                99
243400040130     C**N98
243500040130     C**AN 99              MOVE      LNA(P)        LNT(PP)
243600040130     C**                 ADD       1             P
243700040130     C***                ENDDO
243800970605     c* ripeto per la schiera delle linee di cui sono solo terminal
243900040130     C***                Z-ADD     1             P
244000040130     C***  P             DOWLE     50
244100040130     C***  LNAR(P)       ANDGT     *ZEROS
244200040130     C***  LNAR(P)       LOOKUP    LTT(PP)                                98
244300040130     C**N98              Z-ADD     1             PP                2 0
244400040130     C**N98000           LOOKUP    LTT(PP)                                99
244500040130     C**N98
244600040130     C***AN 99              MOVE      LNAR(P)       LTT(PP)
244700040130     C**                 ADD       1             P
244800040130     C***                ENDDO
244900040130     C***                ENDSR
245000970605      **************************************************************************
245100090929      * SCRITTURA IN FISAR DELLE LINEE DI ARRIVO NON ELABORATE
245200040130      * da quando il report di sede non elabora più per lna ma per tfa
245300040130      * le specifiche seguenti non servono più.
245400970605      **************************************************************************
245500040130     C***  WRTSAR        BEGSR
245600970605     C* Leggo la L1 e la cerco nella schiera delle linee elaborate e
245700090929     c* se non risulta elaborata scrivo record in FISAR (mi serve per
245800970605     C* non dare "dati non pervenuti" sul report di sede)
245900040130     C***                Z-ADD     1             P
246000040130    1C***  P             DOWLE     60
246100040130     C***  l1(p)         ANDGT     *ZEROS
246200970605     C*
246300040130     C***  l1(p)         LOOKUP    LNT                                    98
246400040130    2C***  *IN98         IFEQ      *OFF
246500970605     C*
246600090929     C***                CLEAR                   FISAR000
246700040130     C***                Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
246800040130     C***                MOVE      *ZEROS        SARTFA                         TFA PRECEDENTE
246900040130     C***                Z-ADD     DS1ADA        SARDRE                         DATA
247000040130     C***                Z-ADD     *ZERO         SARTFP                         TERM.PARTENZA
247100040130     C***                Z-ADD     l1(p)         SARLNA                         LINEA DI ARRIVO
247200040130     C***                MOVE      *BLANKS       SARTRE                         TIPO RECORD
247300970605     C* Faccio la setll per sicurezza anche se il record non dovrebbe
247400970605     C* esserci
247500090929     C***  FISARK        SETLL     FISAR01L                               99
247600040130    3C***  *IN99         IFEQ      *OFF
247700970605      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
247800970605      * riempo il "flag di trasmissione" (SARFTR)
247900040130     C***                Z-ADD     *ZERO         SARDTR                         NR.TRASMISSIONE
248000040130    4C***  §5CSAR        IFEQ      'S'
248100040130     C***                MOVE      *BLANK        SARFTR                         FLAG TRASMISS.
248200040130     C***                ELSE
248300040130     C***                MOVE      'T'           SARFTR                         FLAG TRASMISS.
248400040130    4C***                ENDIF
248500090929     C***                WRITE     FISAR000
248600040130    3C***                END
248700040130    2C***                END
248800040130     C***                ADD       1             P
248900970605     C*
249000040130    1C***                ENDDO
249100970605     C*
249200970605     C* con lo stesso criterio elaboro le linee di cui sono solo termi-
249300970605     C* nal non elaborate
249400040130     C***                Z-ADD     1             P
249500040130    1C***  P             DOWLE     500
249600971126     C* scarto se terminal di partenza dell'area
249700040130     C***  ORG(P)        IFGT      0
249800040130    2C***  OTP(P)        ANDNE     SIMFEL
249900040130     C***  OTA(P)        ANDEQ     SIMFEL
250000040130     C***  ORG(P)        LOOKUP    LTT                                    98
250100040130    3C***  *IN98         IFEQ      *OFF
250200970605     C*
250300090929     C***                CLEAR                   FISAR000
250400040130     C***                Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
250500040130     C***                MOVE      *ZEROS        SARTFA                         TFA PRECEDENTE
250600040130     C***                Z-ADD     DS1ADA        SARDRE                         DATA
250700040130     C***                Z-ADD     *ZERO         SARTFP                         TERM.PARTENZA
250800040130     C***                Z-ADD     ORG(P)        SARLNA                         LINEA DI ARRIVO
250900040130     C***                MOVEL     'R'           SARTRE                         TIPO RECORD
251000970605     C* Faccio la setll per sicurezza anche se il record non dovrebbe
251100970605     C* esserci
251200090929     C***  FISARK        SETLL     FISAR01L                               99
251300040130    4C***  *IN99         IFEQ      *OFF
251400970605      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
251500970605      * riempo il "flag di trasmissione" (SARFTR)
251600040130     C***                Z-ADD     *ZERO         SARDTR                         NR.TRASMISSIONE
251700040130    5C***  §5CSAR        IFEQ      'S'
251800040130     C***                MOVE      *BLANK        SARFTR                         FLAG TRASMISS.
251900040130     C***                ELSE
252000040130     C***                MOVE      'T'           SARFTR                         FLAG TRASMISS.
252100040130    5C***                ENDIF
252200090929     C***                WRITE     FISAR000
252300040130    4C***                END
252400040130    3C***                END
252500040130    2C***                END
252600040130     C***                ADD       1             P
252700040130    1C***                ENDDO
252800040130     C***                ENDSR
252900000000      **************************************************************************
253000090929      * Aggiornamento del file statistiche FISAR00F
253100090929     OFISAR003  E            AGGSAR
253200000000     O                       SARCLL
