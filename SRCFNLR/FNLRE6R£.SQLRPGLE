000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      **************************************************************************
000300941228      *  Nome programma:  FNLRE6R (deriva da FLFR63R)
000400000000      *  Descrizione   :  Stampa statistica arrivi.
000500000000      *                   Primo programma di esecuzione: elaborazione dati e
000600180208      *                   scrittura del file statistiche FISAR00F+nuovo TISAR00F
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
004101180208     Ffiars01l  if   e           k disk
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
005601180208     FTISAR01L  UF A E           K DISK
005602180208     FTISAR03L  UF   E           K DISK
005603180208     F                                     RENAME(TISAR000:TISAR003)
005700070117     FFNBRV02L  IF   E           K DISK    USROPN
005800070117     F                                     RENAME(FNBRV000:FISPP000)
005900021218     f                                     prefix(SPP:3)
006000000000     FTABEL00F  IF   E           K DISK
006100021016     fTntbe01l  Uf   e           k Disk
006200040128     FAZCAE02L  IF   E           K DISK
006201180209     FFNBLP01L  if   e           k disk
006300020912
006400020912     d xx              s              3  0                                      contatore
006500000000      *
006600971118     D ORG             S              3  0 DIM(500)                             COD.FIL. DA AZORG
006700971118     D OTP             S              3  0 DIM(500)                             TERM.PAR.DA AZORG
006800100630     D LEL             S              3  0 DIM(60)
006900100630     D LELa            S              3  0 DIM(60)
007000100630     D LEl2            S              3  0 DIM(20)
007100040205     ddslel3           ds                  occurs(20)
007200100630     D LEl3                           3    DIM(20)
007300000000      * TOTALI TERMINAL DI PARTENZA
007400000000     D TFP             S              3  0 DIM(399)                             TERMINAL PARTENZA
007500000000     D CLL             S              7  0 DIM(399)                             TOT COLLI
007600000000     D P88             S              7  0 DIM(399)                             PIST.88 NON ARRIV
007700000000     D DIS             S              7  0 DIM(399)                             DISGUIDI
007800000000     D PAR             S              7  0 DIM(399)                             PARTITI NON SPUNT
007900940607     D F35             S              9  1 DIM(399)                             FINO  35 Q.LI
008000000000     D O35             S              9  1 DIM(399)                             OLTRE 35 Q.LI
008100000000     D PTA             S              9  1 DIM(399)                             PESO TASSABIL
008200000000     D VOL             S              9  4 DIM(399)                             VOLUME
008300090929     D FN              S              7  0 DIM(399)                             FRANCO C+D
008400090929     D FE              S              7  0 DIM(399)                             FRANCO "E"
008500090929     D FH              S              7  0 DIM(399)                             FRANCO "H"
008600090929     D AN              S              7  0 DIM(399)                             ASSEGN C+D
008700090929     D AE              S              7  0 DIM(399)                             ASSEGN "E"
008800090929     D AH              S              7  0 DIM(399)                             ASSEGN "H"
008801180209     D CIF             s              7  0 dim(399)                             Colli Incom.Foglio
008802180209     D CIS             S              7  0 dim(399)                             Colli Incom.Spunta
008806180208     D SDA             s              7  0 dim(399)                             Sped.Distr partita i
008807180208     D SDB             s              7  0 dim(399)                             Sped.Distr partita i
008808180208     D CDA             s              7  0 dim(399)                             ColliDistr partiti i
008810180208     D CDB             s              7  0 dim(399)                             ColliDistr partiti i
008900000620      * TOTALI LINEE DI ARRIVO TER PAR NO POSTE
009000000000     D LNA             S              3  0 DIM(29)                              LINEE PART
009100000000     D CLLA            S              7  0 DIM(29)                              TOT COLLI
009200000000     D P88A            S              7  0 DIM(29)                              PIST.88 NON ARRIV
009300000000     D DISA            S              7  0 DIM(29)                              DISGUIDI
009400000000     D PARA            S              7  0 DIM(29)                              PARTITI NON SPUNT
009500000000     D F35A            S              9  1 DIM(29)                              FINO  35 Q.LI
009600000000     D O35A            S              9  1 DIM(29)                              OLTRE 35 Q.LI
009700000000     D PTAA            S              9  1 DIM(29)                              PESO TASSABIL
009800000000     D VOLA            S              9  4 DIM(29)                              VOLUME
009900090929     D FNA             S              7  0 DIM(29)                              FRANCO C+D
010000090929     D FEA             S              7  0 DIM(29)                              FRANCO "E"
010100090929     D FHA             S              7  0 DIM(29)                              FRANCO "H"
010200090929     D ANA             S              7  0 DIM(29)                              ASSEGN C+D
010300090929     D AEA             S              7  0 DIM(29)                              ASSEGN "E"
010400090929     D AHA             S              7  0 DIM(29)                              ASSEGN "H"
010401180209     D CIFA            s              7  0 dim(399)                             Colli Incom.Foglio
010402180209     D CISA            S              7  0 dim(399)                             Colli Incom.Spunta
010403180208     D SDAA            s              7  0 dim(399)                             Sped.Distr partita i
010404180208     D SDBA            s              7  0 dim(399)                             Sped.Distr partita i
010405180208     D CDAA            s              7  0 dim(399)                             ColliDistr partiti i
010406180208     D CDBA            s              7  0 dim(399)                             ColliDistr partiti i
010500000000      * TOTALI TRANSITI TERMINAL DI PARTENZA
010600000000     D TFPT            S              3  0 DIM(399)                             TERMINAL PARTENZA
010700000000     D CLLT            S              7  0 DIM(399)                             TOT COLLI
010800000000     D P88T            S              7  0 DIM(399)                             PIST.88 NON ARRIV
010900000000     D PART            S              7  0 DIM(399)                             PARTITI NON SPUNT
011000000000     D F35T            S              9  1 DIM(399)                             FINO  35 Q.LI
011100000000     D O35T            S              9  1 DIM(399)                             OLTRE 35 Q.LI
011200000000     D PTAT            S              9  1 DIM(399)                             PESO TASSABIL
011300000000     D VOLT            S              9  4 DIM(399)                             VOLUME
011400090929     D FNT             S              7  0 DIM(399)                             FRANCO C+D
011500090929     D FET             S              7  0 DIM(399)                             FRANCO "E"
011600090929     D FHT             S              7  0 DIM(399)                             FRANCO "H"
011700090929     D ANT             S              7  0 DIM(399)                             ASSEGN C+D
011800090929     D AET             S              7  0 DIM(399)                             ASSEGN "E"
011900090929     D AHT             S              7  0 DIM(399)                             ASSEGN "H"
012000000000      * LINEE DI ARRIVO DI CUI SOLO TERMINAL
012100000000     D LNAR            S              3  0 DIM(50)                              LINEA ARRIVO
012200000000     D CLLR            S              7  0 DIM(50)                              TOT COLLI
012300000000     D P88R            S              7  0 DIM(50)                              PIST.88 NON ARRIV
012400000000     D PARR            S              7  0 DIM(50)                              PARTITI NON SPUNT
012500000000      *
012600970605     D NPS             S              2  0 DIM(101)                             PIST.AUTOMAT.
012700980402     D* CARICO I COD PISTOLA FASULLI DA REIMPOSTARE
012800980402     D NPF             S              2  0 DIM(99)                              NPS CON FSL=F
012900970606     D TTR             S              1    DIM(10)                              TIPTRAINO DEFL
013000021016
013100021016     d kTbeCod         s                   Like(TbeCod)
013200021016     d kTbeKe1         s                   Like(TbeKe1)
013300021218      *
013400021218     d $Volum          s             15  5 inz
013500021218     d $Peso           s             15  5 inz
013600030213     d*** WsiVOL          s              1    inz
013700030213     d*** WsiPES          s              1    inz
013800021220     d Wvolmed         s                   like($Volum) inz
013900021220     d Wpesmed         s                   like($Peso)  inz
013901180209     d sav_sarcll      s                   like(sarcll)
013902180209     d wdisg           s              1
013903180209     d wesblp          s              1
013905180209     d
014000021016
014100950214     D DSLS01        E DS
014200941229      * DS x la schiera delle linee di arrivo (del file FNFVA)
014300000000     D                 DS
014400000000     D  FVALNA                 1      3  0
014500000000     D  FVAFFV                 4    243
014600000000     D  FVAFF2               244    450
014700000504     D  FWAFF3               451    690
014800000504     D  FWAFF4               691    900
014900000504     D*                                     901 903 VUOTO
015000000504     D  FFV                    1    903
015100000504     D                                     DIM(301)                             LINEE FOGLIO -SPP
015200941229      * DS x la schiera delle filiali di scarico (del file FNFVA)
015300000000     D                 DS
015400000000     D  LNA2                   1      3  0
015500000000     D  FVAFLP                 4    243
015600000000     D  FVAFL2               244    450
015700000504     D  FWAFL3               451    690
015800000504     D  FWAFL4               691    900
015900021218     D**VUOTO2               901    903
016000000504     D  FSP                    1    903
016100000504     D                                     DIM(301)                             LINEE SCARICO-SPP
016200941229      * $BOLLA contiene i campi chiave per la chain a FNART01L
016300000000     D                 DS
016400941229     D  $BOLLA                 1     16
016500941229     D  ARTAAS                 1      4  0
016600941229     D  ARTLNP                 5      7  0
016700941229     D  ARTNRS                 8      9  0
016800941229     D  ARTNSP                10     16  0
016900020823     D  ARTFLP                17     19  0
017000020823      * $BOLLT contiene i campi chiave per la chain a FNBTP11L
017100000000     D                 DS
017200941229     D  $BOLLT                 1     16
017300941229     D  BTTAAS                 1      4  0
017400941229     D  BTTLNP                 5      7  0
017500941229     D  BTTNRS                 8      9  0
017600941229     D  BTTNSP                10     16  0
017700020823     D  BTTFLP                17     19  0
017800040923     D WLBDAT          DS                  INZ
017900040923     D  G02DAT                 1      8  0
018000040923     D  G02INV                 9     16  0
018100040923     D  G02ERR                17     17
018200040923     D  G02TGI                18     22  0
018300000619     D OG143         E DS
018301180209     D darsj         e ds
018400000619     D DS3A          E DS
018500970605     D DS7J          E DS
018600970606     D DSTV          E DS
018700000000     D DS3I          E DS
018800941228     D DSQT1         E DS
018900000000     D KPJBA         E DS
019000000000     D CNCR80        E DS
019100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
019200040129     D SFU             S              3  0 DIM(30)                              L6 NO GESTITA
019300040129     D SFA             S              3  0 DIM(30)                              L6 NO GESTITA
019400000000      * DSUL06 serve per call a TRUL06R
019500941230     D DSUL06        E DS                  EXTNAME(TRUL06DS)
019600970502     D  L1                     1     90  0
019700000000     D                                     DIM(30)                              FILIALI GESTITE
019800040127      * Parametri per richiamo a TRUL09R
019900040127     d Trul09ds      e ds
020000040127     d  T09                   19    108  0 dim(30)
020100970502      * DSLV53 serve per call a FNLV53R - REPERISCE DATI FOGLIO
020200970502     D DSLV53        E DS                  EXTNAME(FNLV53DS)
020300970606     D* DS PER DSLV55R - PASSAGGIO PARAMETRI
020400970606     D DSLV55        E DS                  EXTNAME(FNLV55DS)
020500021016
020600021016      * Ds date statistiche
020700021016     d dSdf          e ds
020800090310     d
020900090310     d wrkgetlista     s           4096    varying
021000021016
021100000000      **************************************************************************
021200000000     C     *ENTRY        PLIST
021300000000     C                   PARM                    KPJBA
021400950214     C                   MOVEL     KPJBU         DSLS01
021500000000      *
021600000000     C     TABELK        KLIST                                                  TABEL00F
021700000000     C                   KFLD                    TBLKUT
021800000000     C                   KFLD                    TBLCOD
021900000000     C                   KFLD                    TBLKEY
022000000000     C     KTAB2         KLIST                                                  TABEL00F
022100000000     C                   KFLD                    TBLKUT
022200000000     C                   KFLD                    TBLCOD
022300000000     C     KARB          KLIST
022400000000     C                   KFLD                    VIDLNA            3 0
022500950214     C                   KFLD                    DS1ADA
022600020912     C     FNFVVK        KLIST                                                  FNFVV02L
022700000000     C                   KFLD                    WNPG
022800000000     C                   KFLD                    WDFV
022900020912     c                   Kfld                    Wfgs
023000941229     C     FNFVAK        KLIST                                                  FNFVA02L
023100000000     C                   KFLD                    FVANPG
023200000000     C                   KFLD                    FVANFA
023300020917     c                   Kfld                    FvaLai
023400000504     C     KFWA          KLIST                                                  FNFVA02L
023500000504     C                   KFLD                    FVALNP
023600000504     C                   KFLD                    FVANFV
023700000504     C                   KFLD                    FVALAI
023800070117     C     FNSPPK        KLIST                                                  FNBRV02L
023900941229     C                   KFLD                    SPPNPG
024000000000     C                   KFLD                    SPPNFV
024100941229     C                   KFLD                    SPPFGS
024200000000     C                   KFLD                    SPPLNA
024300070117     C     FNSP1K        KLIST                                                  FNBRV02L
024400950419     C                   KFLD                    SPPNPG
024500950419     C                   KFLD                    SPPNFV
024600950419     C                   KFLD                    SPPFGS
024700950419     C                   KFLD                    SPPLNA
024800950419     C                   KFLD                    SPPLNP
024900941228     C     FNARBK        KLIST                                                  FNARB01L
025000000000     C                   KFLD                    ARTAAS
025100000000     C                   KFLD                    ARTLNP
025200000000     C                   KFLD                    ARTNRS
025300000000     C                   KFLD                    ARTNSP
025400941229     C     FNARTK        KLIST                                                  FNART27L
025500000000     C                   KFLD                    ARTFLS
025600000000     C                   KFLD                    ARTLNA
025700000000     C                   KFLD                    ARTNRS
025800941230     C                   KFLD                    ARTNSC
025900020823     C     FNARTK1       KLIST                                                  FNART27L
026000040130     C***                KFLD                    SIMFEL
026100040130     C                   KFLD                    fvalai
026200020823     C                   KFLD                    ARTFLS
026300020823     C                   KFLD                    ARTLNA
026400020823     C                   KFLD                    ARTNRS
026500020823     C                   KFLD                    ARTNSC
026600020823     C     FLBTPK        KLIST                                                  FNBTP11L
026700020823     C                   KFLD                    BTTFLP
026800020823     C                   KFLD                    BTTAAS
026900941229     C                   KFLD                    BTTLNP
027000941229     C                   KFLD                    BTTNRS
027100941229     C                   KFLD                    BTTNSP
027200090929     C     FISARK        KLIST                                                  FISAR01L
027300000000     C                   KFLD                    SARFLE
027400000000     C                   KFLD                    SARTFA
027500000000     C                   KFLD                    SARTFP
027600000000     C                   KFLD                    SARLNA
027700000000     C                   KFLD                    SARTRE
027800000000     C                   KFLD                    SARDRE
027900090929     C     FLSA3K        KLIST                                                  FISAR03L
028000000000     C                   KFLD                    SARFLE
028100000000     C                   KFLD                    SARTFA
028200000000     C                   KFLD                    SARTRE
028300000000     C                   KFLD                    SARDRE
028400990521     C     FNANMK        KLIST                                                  FNANM04L
028500000000     C                   KFLD                    ANMCAA
028600000000     C                   KFLD                    ANMDAO
028700990521     C     FNAN4K        KLIST                                                  FNANM04L
028800000000     C                   KFLD                    ANMCAA
028900000000     C                   KFLD                    ANMDAO
029000000000     C                   KFLD                    ANMLNA
029100070117     C     FNBRVK        KLIST                                                  FNBRV09L
029200000000     C                   KFLD                    BRVNPG
029300000000     C                   KFLD                    BRVLNP
029400000000     C                   KFLD                    BRVLNA
029500000000     C                   KFLD                    BRVNRS
029600000000     C                   KFLD                    BRVNSC
029700070117     C     KBRV9         KLIST                                                  FNBRV09L
029800970513     C                   KFLD                    BRVNPG
029900970513     C                   KFLD                    BRVLNP
030000970513     C                   KFLD                    BRVLNA
030100970513     C                   KFLD                    BRVNRS
030200970513     C                   KFLD                    BRVNSC
030300070117     C     KBRV          KLIST                                                  FNBRV07L
030400970502     C                   KFLD                    BRVLNP
030500970502     C                   KFLD                    BRVLNA
030600970502     C                   KFLD                    BRVNRS
030700970502     C                   KFLD                    BRVNSC
030800020823     C     KDTA          KLIST
030900020823     C                   KFLD                    SIMFEL            3 0
031000020823     C                   KFLD                    DS1ADA
031100021016
031200021016     c     Ktbe          Klist
031300021016     c                   Kfld                    kTbeCod
031400021016     c                   Kfld                    kTbeKe1
031500040128     C*
031600090929     C     KCAE2         KLIST                                                  FISAR01L
031700040128     C                   KFLD                    KEPA
031800040128     C                   KFLD                    lel(c)
031900040128     C                   MOVEL     'A'           KEPA
032000000000      **************************************************************************
032100000000      *              M A I N      L I N E
032200000000      **************************************************************************
032300000000      *
032400000000      * Imposto il nr. di elementi delle schiere (per i cicli a venire)
032500000000     C                   Z-ADD     399           $MAX              3 0          ELEMENTI SCHIER
032600000000      *
032700000000     C                   OPEN      AZORG01L
032800941228     C                   OPEN      FNARB01L                                     BOLLE
032900941228     C                   OPEN      FNARB29L                                     BOLLE
033000090309     C                   OPEN      FnART27L                                     COLLI
033100020823     C                   OPEN      FNBTP11L                                     BOLLE TRANSITO
033200020823     C                   OPEN      FNBTP19L                                     BOLLE TRANSITO
033300020823     C                   OPEN      FNBTT37L                                     COLLI TRANSITO
033400020823     C                   OPEN      FNBTT39L                                     COLLI TRANSITO
033500070117     C                   OPEN      FNBRV02L                                     SPUNTE
033600941229     C                   OPEN      FNFVA02L
033700990521     C                   OPEN      FNANM04L                                     ANOMALIE
033800040923     c*
033900040923     C* DETERMINO DATA DI ESECUZIONE
034000040923     C                   TIME                    WTIME            14 0
034100040923     c*
034200040923     C                   CLEAR                   WLBDAT
034300040923     C                   MOVE      Wtime         G02DAT
034400040923     C                   CALL      'XSRDA8'
034500040923     C                   PARM                    WLBDAT
034600040923     C                   Z-ADD     G02INV        dateu             8 0
034700000000      *
034800941230      * Riempo alcune schiere
034900000000     C                   EXSR      CARTAB
035000000000      *
035100000000     C                   Z-ADD     *ZERO         $DTAAR            1 0          SALVA DTAARA
035200000000      *
035300000000      * Elaborazione archivi
035400000000     C                   EXSR      LEGGI
035500970605     C*
035600090929     C* Scrittura record in FISAR per linee dell'area o di cui solo
035700970605     C* terminal non elaborate (serve per il report di sede)
035800040130     C***                EXSR      WRTSAR
035900000000      *
036000000000      * Se ho elaborato qualcosa di valido aggiorno la data ultima elaborazione
036100000000     C     $DTAAR        IFGT      *ZERO
036200021016
036300021016     c                   Eval      kTbeCod = 'SDF'
036400021016     c                   Movel(p)  Simfel        kTbeKe1
036500021016     c     Ktbe          Chain     Tntbe01l
036600021016     c                   If        %Found(Tntbe01l)
036700021016     c                   Movel     TbeUni        dSdf
036800021016     c                   If        §SdfArrP = *Zeros or §SdfArrP > Ds1Ada
036900021016     c                   Z-Add     Ds1Ada        §SdfArrP
037000021016     c                   EndIf
037100021016     c                   If        §SdfArrU < Ds1Aal
037200021016     c                   Z-Add     Ds1Aal        §SdfArrU
037300021016     c                   EndIf
037400021016     c                   Movel     dSdf          TbeUni
037500021016     c                   Update    Tntbe000
037600021016     c                   EndIf
037700000000     C                   ENDIF
037800970505     C* CHIUSURA FILE
037900970505     C                   CLEAR                   DSLV53
038000970505     C                   MOVEL     'C'           D53TLA
038100970505     C                   CALL      'FNLV53R'
038200970505     C                   PARM                    DSLV53
038300970606     C* CHIUSURA FILE
038400970606     C                   CLEAR                   DSLV55
038500970606     C                   MOVEL     'C'           D55TLA
038600970606     C                   CALL      'FNLV55R'
038700970606     C                   PARM                    DSLV55
038800100701     C* CHIUSURA FILE
038900100701     C                   CLEAR                   trul09ds
039000100701     C                   MOVEL     'C'           i09TLA
039100100701     C                   CALL      'TRUL09R'
039200100701     C                   PARM                    trul09ds
039300000000      *
039400000000     C                   SETON                                        LR
039500000000      **************************************************************************
039600000000      * LETTURA BOLLE PER CREAZIONE STAMPA
039700000000      **************************************************************************
039800000000     C     LEGGI         BEGSR
039900000000      *
040000000000      * Cancello dall'archivio eventuali record nella stessa data,solo se stampa
040100090929     C     SIMFEL        SETLL     FISAR01L
040200000000     C                   DO        *HIVAL
040300090929     C     SIMFEL        READE     FISAR01L                               31
040400990507     C  N31SARDRE        IFGE      DS1ADA
040500990507     C     SARDRE        ANDLE     DS1AAL
040600090929     C                   DELETE    FISAR000
040700000000     C                   ENDIF
040800000000     C  N31              ENDDO
040900000000      *
040901180208     C     SIMFEL        SETLL     TISAR01L
040902180208     C                   DO        *HIVAL
040903180208     C     SIMFEL        READE     TISAR01L                               31
040904180208     C  N31SARDRE        IFGE      DS1ADA
040905180208     C     SARDRE        ANDLE     DS1AAL
040906180208     C                   DELETE    TISAR000
040907180208     C                   ENDIF
040908180208     C  N31              ENDDO
041000000000      * ----------------?  FASE 1: ELABORAZIONE BOLLE  ?------------------------
041100000000     C                   Z-ADD     1             C                 3 0
041200000000     C                   Z-ADD     *ZERO         $A                3 0          CONTAT. LNA
041300000000     C                   Z-ADD     *ZERO         $B                3 0          CONTAT. TFP
041400000000     C                   MOVE      *BLANKS       $CBLPR                         PRECEDENTE
041500000000     C                   Z-ADD     *ZERO         $TFAPR                         PRECEDENTE
041600000000     C                   Z-ADD     *ZERO         $PRIMO            1 0          PRIMO GIRO
041700000000      *
041800040127      * Eseguo i cicli di lettura su FNARB solo per le linee da elaborare;
041900000000      * evito cosi' di leggere, inutilmente, tutti i record bolle
042000040127     C     lel(c)        DOWNE     0                                            TABELLA '£1'
042100040127     C                   MOVEL     lel(c)        VIDLNA
042200000000      *
042300941228      * N.B.: il file FNARB29L e' ora ordinato anche come terminal di arrivo
042400000000      *       (terzo campo chiave); devo tenere conto del codice TFA poiche'
042500090929      *       dovro' scriverlo nel file statistiche (FISAR00F).
042600000000      *
042700941228      * Utilizzo questa tecnica:  leggo FNARB e riempo le schiere; quando
042800090929      * cambia TFA salvo le schiere nel file statistiche (FISAR) e le pulisco,
042900000000      * quindi continuo le letture ririempiendo le schiere. E cosi' via.
043000970502      *
043100970502     C     KARB          SETLL     FNARB29L
043200970502     C     KARB          READE     FNARB29L                               31
043300970502     C     *IN31         DOWEQ     *OFF
043400000000      *
043500000000     C                   EXSR      ELABB                                        ELABORAZ. BOLLA
043600000000      *
043700970502     C     KARB          READE     FNARB29L                               31
043800970502     C                   ENDDO                                                  *HIVAL
043900000000      *
044000000000     C                   ADD       1             C
044100000000     C                   ENDDO                                                  L1,C <> 0
044200000000      *
044300000000     C                   EXSR      SCASCH                                       SCA.SCHIERE BOL
044400000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
044500941228     C                   CLOSE     FNARB29L                                     FILE BOLLE
044600000000      *
044700000000      * ----------------?  FASE 2:  ELABORAZIONE COLLI  ?-----------------------
044800000000     C                   Z-ADD     1             C
044900000000     C                   MOVE      *BLANKS       $CBLPR                         PRECEDENTE
045000000000     C                   Z-ADD     *ZERO         $TFAPR                         PRECEDENTE
045100000000     C                   Z-ADD     *ZERO         $B                             CONTAT. TFP
045200000000     C                   Z-ADD     *ZERO         $PRIMO                         PRIMO GIRO
045300090310     C                   MOVE      *ZERO         $BOLPR                         PRECEDENTE
045400090310     C                   MOVE      *BLANKS       $OKBOL            1            SE BOLLA OK
045500090309     c* Preparo SQL per leggere i colli arrivati per linea di arrivo
045600090310     c                   exsr      PrepaSQL
045700090310      *?Leggo il file
045800090310     c                   do        *hival
045900090310     C/EXEC SQL
046000090310     C+ FETCH NEXT FROM a1 INTO :artaas, :artlnp, :artnrs, :artnsp, :artlna,
046100180212     C+                         :artflp, :artfls, :artnsc
046200090310     C/END-EXEC
046300090310
046400090310     c                   select
046500090310
046600090310     c                   when      sqlcod = 100
046700090310     c                   leave
046800090310     c
046900090310     c                   when      sqlcod < 0
047000090310     c                   seton                                        H1
047100090310     c                   eval      *InLr = *On
047200090310     c                   return
047300090310
047400090310     c                   other
047500090310      *
047600090310      * Aggancio la testata della bolla (FNARB), se <> dalla precedente
047700090310     C     $BOLLA        IFNE      $BOLPR                                       <> PRECEDENTE
047800090310     C     FNARBK        CHAIN     FNARB01L                           99
047900090310     C   99              MOVE      'N'           $OKBOL                         NO OK BOLLA
048000090310     C  N99              MOVE      'S'           $OKBOL                         SI OK BOLLA
048100090310     C  N99              EXSR      ELABC1                                       CALCOLI BOLLA
048101180209         if not *in99 and arbtsp='D' ;
048102180209           clear wesblp;
048103180209           chain (arbaas:arblnp:arbnrs:arbnsp) fnblp01l;
048104180212           if  %found(fnblp01l);
048105180209             wesblp='S';
048106180209           ENDIF;
048107180209         ENDIF;
048200090310     C                   MOVE      $BOLLA        $BOLPR
048300090310     C                   ENDIF
048400090310      *
048500090310     C     $OKBOL        IFEQ      'S'                                          SE BOLLA OK
048600090310     C                   EXSR      ELABC                                        CALCOLI SU COLL
048700090310     C                   ENDIF
048800090310
048900090310     c                   endsl
049000090310
049100090310     c                   enddo
049200090310
049300090310     C/EXEC SQL
049400090310     C+ CLOSE a1
049500090310     C/END-EXEC
049600090310
049700090929      * Termianti tutti i giri scarico su FISAR le ultime schiere in memoria
049800000000     C                   EXSR      SCASCH                                       SCA.SCHIERE COL
049900000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
050000941228     C                   CLOSE     FNARB01L                                     FILE BOLLE
050100000000      *
050200000000      * ----------------?  FASE 3: ELABORAZIONE TRANSITI  ?---------------------
050300000000      *
050400000000     C                   Z-ADD     *ZERO         $B                             CONTATORE TFP
050500000000     C                   MOVE      *BLANKS       $CBLPR                         PRECEDENTE
050600000406     C                   EXSR      IMPOT                                        SETLL
050700000000      *
050800020823     C     KDTA          READE     FNBTP19L                               31
050900000000     C     *IN31         DOWEQ     '0'
051000000000      *
051100000000     C                   EXSR      ELABT
051200000000      *
051300020823     C     KDTA          READE     FNBTP19L                               31
051400000000     C                   ENDDO                                                  *IN30DOWEQ'0'
051500000000      *
051600090929      * Terimanti tutti i giri scarico su FISAR le ultime schiere in memoria
051700000000     C                   EXSR      SCASCT                                       SCARICA SCHIERE
051800000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
051900020823     C                   CLOSE     FNBTP19L                                     FILE BOLLE TRAN
052000000000      *
052100000000      * ----------------?  FASE 4:  COLLI IN TRANSITO  ?------------------------
052200000000      *
052300000000     C                   MOVE      *ZERO         $BOLPR                         PRECEDENTE
052400000000     C                   MOVE      *BLANKS       $CBLPR                         PRECEDENTE
052500000000     C                   MOVE      *BLANKS       $OKBOL                         SE BOLLA OK
052600980708     C                   Z-ADD     *ZERO         $B                             CONTAT. TFP
052700020823     C     KDTA          SETLL     FNBTT39L
052800020823     C     KDTA          READE     FNBTT39L                               31
052900000000     C     *IN31         DOWEQ     '0'
053000000000      *
053100000000      * Aggancio la testata della bolla (FLBTP), se <> dalla precedente
053200000000     C     $BOLLT        IFNE      $BOLPR                                       <> PRECEDENTE
053300020823     C     FLBTPK        CHAIN     FNBTP11L                           99
053400000000     C   99              MOVE      'N'           $OKBOL                         NO OK BOLLA
053500000000     C  N99              MOVE      'S'           $OKBOL                         SI OK BOLLA
053600000000     C  N99              EXSR      ELACT1
053700000000     C                   MOVE      $BOLLT        $BOLPR                         PRECEDENTE
053800000000     C                   ENDIF
053900000000      *
054000000000     C     $OKBOL        IFEQ      'S'                                          SE BOLLA OK
054100000000     C                   EXSR      ELABCT
054200000000     C                   ENDIF
054300000000      *
054400020823     C     KDTA          READE     FNBTT39L                               31
054500000000     C                   ENDDO
054600000000      *
054700090929      * Terimanti tutti i giri scarico su FISAR le ultime schiere in memoria
054800000000     C                   EXSR      SCASCT                                       SCARICA SCHIERE
054900000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
055000020823     C                   CLOSE     FNBTT39L                                     FILE COLLI TRAN
055100020823     C                   CLOSE     FNBTP11L                                     FILE BOLLE TRAN
055200000000      *
055300000000      * --? FASE 5:  COLLI NON SPUNTATI IN PARTENZA E NON ARRIVATI (PIST.88) ?--
055400000000     C                   EXSR      NONARR                                       NON ARRIVATI
055500020823     C                   CLOSE     FNBTT37L                                     COLLI TRANSITO
055600000000      *
055700000000      * -------------?  FASE 5bis:  LNA DI CUI SOLO TERMINAL  ?-----------------
055800000000      * E' il completamento della fase 5, poiche' i record (della statistica) di
055900000000      * tipo 'R' vengono creati nella fase 5
056000000000      *
056100000000      * Rileggo dalla statistica tutti i record di tipo 'R'=DI CUI SOLO TERMINAL
056200000000      * per verificare se vi sono anche delle anomalie da aggiungervi
056300000000     C                   Z-ADD     SIMFEL        SARFLE
056400000000     C                   Z-ADD     SIMFEL        SARTFA
056500000000     C                   MOVE      'R'           SARTRE                         'R'=SOLO TERMIN.
056600950214     C                   Z-ADD     DS1ADA        SARDRE                         DATA
056700090929     C     FLSA3K        SETLL     FISAR03L
056800000000     C                   DO        *HIVAL
056900090929     C     FLSA3K        READE     FISAR03L                               31
057000000000     C  N31              DO
057100950418      *
057200950418      *? FASE 5bis  -  Parte 1  ·····  INCLUDO LE ANOMALIE ?
057300000000      *
057400000000      * Devo cercare fra le anomalie di tipo "50"
057500000000     C                   Z-ADD     50            ANMCAA                         50=ARRIV.NO PAR
057600950214     C                   Z-ADD     DS1ADA        ANMDAO                         DATA INIZIALE
057700000000     C                   Z-ADD     SARLNA        ANMLNA                         LINEA ARRIVO
057800000000      *
057900990521     C     FNAN4K        SETLL     FNANM04L
058000000000     C                   DO        *HIVAL
058100990521     C     FNAN4K        READE     FNANM04L                               32
058200000000     C  N32              ADD       1             SARCLL
058300990521     C  N32              ENDDO                                                  *HIVAL FNANM04L
058400000000      *
058500000000      * sommo anche le anomalie di tipo "160"
058600000000     C                   Z-ADD     160           ANMCAA                         160=ARR PART AL
058700000000      *
058800990521     C     FNAN4K        SETLL     FNANM04L
058900000000     C                   DO        *HIVAL
059000990521     C     FNAN4K        READE     FNANM04L                               32
059100000000     C  N32              ADD       1             SARCLL
059200990521     C  N32              ENDDO                                                  *HIVAL FNANM04L
059300950418      *
059400950418      *? FASE 5bis  -  Parte 2  ·····  I LOCALI (PUB E MENSE ALLA MODA) ?
059500950418      *
059600950419      * Per la filiale in esame (SARLNA) leggo i fogli (con le relative
059700950419      * spunte) di categ. 2 e 6 con data compresa nel periodo in esame
059800950418      *
059900950419      * Eseguo due volte l'operazione, una per ogni categoria
060000950419     C                   Z-ADD     2             WNPG                           2=ARRIVI
060100180209     C**                 DO        2
060200020912      * devo ciclare per tutti i p.o. dell'area
060300020918     c                   Clear                   xx
060400020912     c                   do        30            xx
060500020912     c                   If        L1(xx) = *Zeros
060600020912     c                   Leave
060700020912     c                   EndIf
060800950419     C                   Z-ADD     DS1ADA        WDFV                           DATA INIZIALE
060900020912     C**!!!              Z-ADD     SIMFEL        WFLE                           FIL.ELABORATORE
061000020912     c                   Z-Add     L1(xx)        Wfgs
061100020912     C     FNFVVK        SETLL     FNFVV02L
061200020912     C     FNFVVK        READE     FNFVV02L                               33
061300950419     C     *IN33         DOWEQ     *OFF
061400950419      *
061500950419      * Leggo le spunte del foglio, ma solo quelle partenti dall'area
061600950419     C                   Z-ADD     FVVNPG        SPPNPG
061700950419     C                   Z-ADD     FVVNFV        SPPNFV
061800950419     C                   Z-ADD     FVVFGS        SPPFGS
061900950419     C                   Z-ADD     SARLNA        SPPLNA                         LINEA ARRIVO
062000950419     C                   Z-ADD     1             $B
062100950419     C     L1($B)        DOWGT     *ZERO
062200950419     C                   Z-ADD     L1($B)        SPPLNP                         LINEA PARTENZA
062300950419      *
062400950419      * Uso £1 per leggere meno spunte possibili
062500070117     C     FNSP1K        SETLL     FNBRV02L
062600070117     C     FNSP1K        READE     FNBRV02L                               32
062700950419     C     *IN32         DOWEQ     *OFF
062800950419     C                   ADD       1             SARCLL
062900070117     C     FNSP1K        READE     FNBRV02L                               32
063000950419     C                   ENDDO
063100950419     C*
063200950419     C                   ADD       1             $B                             ALTRA FIL.
063300950419     C                   ENDDO
063400950419      *
063500020912     C     FNFVVK        READE     FNFVV02L                               33
063600950419     C                   ENDDO
063700020912     c                   EndDo
063800950418      *
063900180209     C***                   Z-ADD     6             WNPG                           6=ARRIVI 2° LVL
064000180209     C***                   ENDDO
064100950418      *?                                                                       ?
064200000000      *
064300000000     C                   EXCEPT    AGGSAR
064301180209         sav_sarcll=sarcll;
064302180209         chain (sarfle:sartfa:sartre:sardre) tisar03l;
064303180209         if %found (tisar03l);
064304180209             sarcll=sav_sarcll;
064305180209             except aggsart;
064306180209         ENDIF;
064400000000     C                   ENDDO
064500090929     C  N31              ENDDO                                                  *HIVAL FISAR03L
064600000000      *
064700000000      * ----------------?  FASE 6:  COLLI IN DISGUIDO  ?------------------------
064800000000      *
064900000000     C                   Z-ADD     *ZERO         $B                             CONTATORE TFP
065000971126     C                   Z-ADD     SIMFEL        $TFAPR                         PRECEDENTE
065100000000     C                   Z-ADD     *ZERO         $PRIMO                         PRIMO GIRO
065200000000      *
065300001127     C                   Z-ADD     56            ANMCAA
065400950214     C                   Z-ADD     DS1ADA        ANMDAO                         DATA INIZIALE
065500990521     C     FNANMK        SETLL     FNANM04L
065600990521     C     FNANMK        READE     FNANM04L                               31
065700970502    1C     *IN31         DOWEQ     *OFF
065800021023      * Deve elaborare solo le anomalie con ANMFLE della L1
065900040129     c***  AnmFle        Lookup    L1                                     99
066000040129      * Deve elaborare solo le anomalie con ANMFLE della lel
066100040129     c     AnmFle        Lookup    Lel                                    99
066200021023     c                   If        Not *In99
066300021023     c                   GoTo      Rileggi
066400021023     c                   EndIf
066500970502     C*
066600970502    2C     ANMAIE        IFEQ      'E'
066700000000      *
066800971126      *
066900970502    3C     $PRIMO        IFEQ      *ZERO                                        0=PRIMO GIRO
067000000000     C                   Z-ADD     1             $PRIMO                         1=GIA' PASSATO
067100000000     C                   Z-ADD     1             $DTAAR
067200970502    3C                   ENDIF
067300000000      *
067400000000      * Cerco l'indice di schiera per il terminal di partenza
067500000000     C                   MOVE      *BLANKS       $TRANS            1            NO TRANSITO
067600970606     C                   Z-ADD     1             D                 3 0
067700000000     C     ANMFLS        LOOKUP    ORG(D)                                 99     ON=TROVATO
067800000000     C   99              Z-ADD     OTP(D)        $LINEA
067900000000     C   99              EXSR      INDTFP                                       INDICE TFP (B)
068000000000      *
068100000000      * Cerco l'indice di schiera per la linea di arrivo
068200971126     C                   Z-ADD     SIMFEL        $LINEA
068300000000     C                   EXSR      INDLNA                                       INDICE LNA (A)
068400000000     C                   ADD       1             DIS(B)                         DEL TFP
068500000000     C                   ADD       1             DISA(A)                        DELLA LNA
068600000620    2C                   ENDIF
068700970502      *
068800021023     c     Rileggi       Tag
068900021023
069000990521     C     FNANMK        READE     FNANM04L                               31
069100970502    1C                   ENDDO
069200000000      *
069300971126     C     $PRIMO        IFGT      *ZEROS
069400000000     C                   EXSR      SCASCH                                       SCARICA SCHIERE
069500000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
069600971126     C                   END
069700000000      *
069800000000     C                   ENDSR
069900090310      **************************************************************************
070000090310      * Prepara SQL per lettura FNART
070100090310      **************************************************************************
070200090310     C     PrepaSQL      BEGSR
070300090310     c                   eval      wrkgetlista =
070400090310     c                             'select artaas, artlnp, artnrs, artnsp,' +
070500180212     c                             ' artlna, artflp, artfls, artnsc, artdfv '+
070600090310     c                             ' from fnart00f  where' +
070700090310     c                             ' artdam = '+
070800090310     c                              %editc(ds1ada:'X') + ' and ' +
070900090310     c                             'artlna  in('
071000090310      * ciclo sulla sk delle linee da elaborare
071100090310     c                   z-add     1             c
071200090310     c                   Do        60            C
071300090310     c                   If        lel(c) = 0
071400090310     c                   Leave
071500090310     c                   EndIf
071600090310     c
071700090310     c                   If        C >1
071800090310     c                   eval      wrkgetlista =
071900090310     c                             %trim(wrkgetlista) + ','
072000090310     c                   EndIf
072100090310     c
072200090310     c                   eval      wrkgetlista =
072300090310     c                             %trim(wrkgetlista) + ' ''' +
072400090310     c                             %editc(lel(C):'X') + ''''
072500090310     c                   EndDo
072600090310     c
072700090310     c                   Eval      wrkgetlista =
072800090310     c                             %trim(wrkgetlista) + ')'
072900090310     c
073000090310      *?Ordinamento
073100090310     c                   eval      wrkgetlista = wrkgetlista +
073200090310     c                             ' order by artlna, artdam '
073300090310      *?Solo lettura
073400090310     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
073500090310     c
073600090310
073700090310     C/EXEC SQL
073800090310     C+ PREPARE s1 FROM :wrkgetlista
073900090310     C/END-EXEC
074000090310     C
074100090310     C/EXEC SQL
074200090310     C+ DECLARE A1 CURSOR FOR S1
074300090310     C/END-EXEC
074400090310     C
074500090310     C/EXEC SQL
074600090310     C+ OPEN a1
074700090310     C/END-EXEC
074800090310
074900090310     C                   ENDSR
075000000000      **************************************************************************
075100000000      * CARICAMENTO TABELLE INIZIALI
075200000000      **************************************************************************
075300000000     C     CARTAB        BEGSR
075400000000      *
075500000000     C                   Z-ADD     1             CODUT
075600000000     C                   CALL      'X§PARUT'
075700000000     C                   PARM                    UT§DSE
075800000000     C                   MOVEL     RAGUT         RSUT             20
075900000000     C                   MOVEL     REC80         CNCR80
076000000000      *
076100941228      * AGGANCIO LA TABELLA QT PER ALCUNI DATI AZIENDALI
076200941228     C                   Z-ADD     1             TBLKUT
076300941228     C                   MOVE      'QT'          TBLCOD
076400941228     C                   MOVEL     '1       '    TBLKEY
076500941228     C     TABELK        CHAIN     TABEL00F                           99
076600941228     C  N99              MOVEL     TBLUNI        DSQT1
076700941228     C   99              CLEAR                   DSQT1
076800000000      *
076900000000      * Prelevo la tabella delle variabili programmi (3I)
077000000000     C                   Z-ADD     1             TBLKUT
077100000000     C                   MOVE      '3I'          TBLCOD
077200000000     C                   MOVEL     '1       '    TBLKEY
077300000000     C     TABELK        CHAIN     TABEL00F                           99
077400000000     C  N99              MOVEL     TBLUNI        DS3I
077500000000     C   99              Z-ADD     20            §3IARR
077600000000      *
077700000000      * Carico una schiera contenente i TFA relativi alle filiali
077800000619     C                   Z-ADD     1             WW
077900000000     C     *LOVAL        SETLL     AZORG01L
078000960926     C                   DO        *HIVAL        $H                3 0
078100000000     C                   READ      AZORG01L                               31
078200000000     C  N31ORGFAG        IFNE      'V'                                          'V'=CENTRO COST
078300940720     C                   Z-ADD     ORGFIL        ORG($H)                        CD.FILIALE
078400970704     C                   CLEAR                   DSLV55
078500970704     C                   Z-ADD     ORGFIL        D55LIN
078600970704     C                   Z-ADD     DS1ADA        D55DRF
078700970704     C                   CALL      'FNLV55R'
078800970704     C                   PARM                    DSLV55
078900970704     C                   Z-ADD     D55TFP        OTP($H)                        TERM.PARTENZ
079000000000     C                   ENDIF
079100000000     C  N31              ENDDO
079200000000      *
079300000000      * Carico la tabella '£1' che contiene le filiali gestite
079400000000     C                   MOVE      KPJBU         $KPJBU                         SALVO KPJBU
079500941230     C                   MOVEL     '£1'          D06COD                         COD.TABELLA
079600020508     c                   movel     simfel        d06key
079700050802     c                   move      'S'           d06key
079800941230     C                   MOVEL     'L'           D06TLA                         CHIUDI CON LR
079900941230     C                   MOVEL     *BLANKS       D06ESC                         COMPRESA CHIAVE
080000050802     C                   MOVEL     ds1ada        D06drf                         COMPRESA CHIAVE
080100000000     C                   MOVE      *BLANKS       KPJBU
080200000000     C                   MOVEL     DSUL06        KPJBU
080300000000     C                   CALL      'TRUL06R'
080400000000     C                   PARM                    KPJBA
080500000000     C                   MOVEL     KPJBU         DSUL06
080600000000     C                   MOVE      $KPJBU        KPJBU                          RIPRIST. KPJBU
080700040127     c* carico schiera dei p.o. da elaborare
080800040127     c                   z-add     1             c
080900040127     c                   clear                   lel
081000040127     c                   clear                   ii                3 0
081100040127    1c     l1(c)         downe     *zeros
081200040127     c                   clear                   trul09ds
081300100630     c                   eval      i09mod = 'DEF'
081400040127     c                   eval      i09tfa = l1(c)
081500040127     c                   eval      i09dta = ds1ada
081600040127     c                   call      'TRUL09R'
081700040127     c                   parm                    trul09ds
081800040127    2c     o09err        ifeq      *blanks
081900040127     c                   z-add     1             ww                3 0
082000040127    3c     t09(ww)       downe     *zeros
082100040127     c     t09(ww)       lookup    lel                                    31
082200040130     c  n31              add       1             ii
082300040127     c  n31              eval      lel(ii) = t09(ww)
082400040127     c                   add       1             ww
082500040127    3c                   enddo
082600040127   x2c                   else
082700040127     c     l1(c)         lookup    lel                                    31
082800040130     c  n31              add       1             ii
082900040127     c  n31              eval      lel(ii) = l1(c)
083000040127    2c                   endif
083100040127     c                   add       1             c
083200040127    1c                   enddo
083300040128     c* carico altra schiera nel caso in cui uno o più p.o. della schiera
083400040128     c* lel sia a sua volta un secondo liv. in arrivo il cui ter-arr apparti
083500040128     c* ene ad altra £1
083600040128     c                   z-add     1             c
083700040129     c                   clear                   lel2
083800040205     c* pulisco ds multipla
083900040205     c                   do        20            ii
084000040205     c     ii            occur     dslel3
084100040205     c                   clear                   lel3
084200040205     c                   enddo
084300040205     c*
084400100630     c**   lel(c)        downe     0
084500100630     c**   kcae2         setll     azcae02l
084600100630     C**   KCAE2         READE     AZCAE02L                               99
084700100630     C*+   *IN99         DOWEQ     *OFF
084800100630     C**   CAEATB        IFEQ      *BLANKS
084900100630     C**   DS1ADA        ANDGE     CAEDDE
085000100630     C**   DS1ADA        ANDLE     CAEDSC
085100100630     C**   CAETFE        LOOKUP    lel                                    99
085200100630     c**   *in99         ifeq      *off
085300100630     c**                 z-add     1             ii
085400100630     c**   caetfe        lookup    lel2(ii)                               99
085500100630     c**   *in99         ifeq      *off
085600100630     c**   000           lookup    lel2(ii)                               99
085700100630     C** 99              z-add     caetfe        lel2(ii)
085800100630     c**                 endif
085900100630     c**   ii            occur     dslel3
086000100630     c**                 z-add     1             yy                3 0
086100100630     c**   '   '         lookup    lel3(yy)                               99
086200100630     c** 99              move      lel(c)        lel3(YY)
086300100630     c*+                 endif
086400100630     C**                 ENDif
086500100630     C**   KCAE2         READE     AZCAE02L                               99
086600100630     C**                 ENDDO
086700100630     c**                 add       1             c
086800100630     c**                 enddo
086900040127     c
087000970605     C**
087100970605     C**  CARICO TABELLA 7J - CODICI PISTOLA (CARICO SOLO QUELLI
087200970605     C*                                       AUTOMATICI)
087300970605     C                   Z-ADD     1             TBLKUT
087400970605     C                   MOVEL     '7J'          TBLCOD
087500970605     C                   Z-ADD     1             X                 3 0
087600980402     C                   Z-ADD     1             Y                 3 0
087700970605     C     KTAB2         SETLL     TABEL
087800970605     C     KTAB2         READE     TABEL                                  99
087900970605     C*
088000970606     C     *IN99         DOWEQ     *OFF
088100970606     C     TBLFLG        IFEQ      ' '
088200970605     C                   MOVEL     TBLUNI        DS7J
088300970605     C     §7JRPS        IFEQ      'A'
088400970605     C                   MOVEL     TBLKEY        NPS(X)
088500970605     C                   ADD       1             X
088600980402     C                   ENDIF
088700980402     C     §7JFSL        IFEQ      'F'
088800980402     C                   MOVEL     TBLKEY        NPF(Y)
088900980402     C                   ADD       1             Y
089000980402     C                   ENDIF
089100980402     C                   ENDIF
089200970605     C*
089300970605     C     KTAB2         READE     TABEL                                  99
089400970606     C                   ENDDO
089500970606     C***
089600970606     C* CARICO I TIPI TRAINO DI DEFLUENZA
089700970606     C***
089800970606     C                   Z-ADD     1             TBLKUT
089900970606     C                   MOVEL     'TV'          TBLCOD
090000970606     C                   Z-ADD     1             X                 3 0
090100970606     C     KTAB2         SETLL     TABEL
090200970606     C     KTAB2         READE     TABEL                                  99
090300970606     C*
090400970606     C     *IN99         DOWEQ     *OFF
090500970606     C     TBLFLG        IFEQ      ' '
090600970606     C                   MOVEL     TBLUNI        DSTV
090700970606     C     §TVDEF        IFEQ      'S'
090800970606     C                   MOVEL     TBLKEY        TTR(X)
090900970606     C                   ADD       1             X
091000970606     C                   ENDIF
091100970606     C                   ENDIF
091200970606     C*
091300970606     C     KTAB2         READE     TABEL                                  99
091400970606     C                   ENDDO
091500000000      *
091600000000     C     *LIKE         DEFINE    KPJBU         $KPJBU                         DI COMODO
091700941230     C     *LIKE         DEFINE    ARBCBO        $CBLPR                         PRECEDENTE
091800000000     C     *LIKE         DEFINE    ARBTFA        $TFAPR                         PRECEDENTE
091900000000     C     *LIKE         DEFINE    $BOLLA        $BOLPR                         PRECEDENTE
092000000000     C     *LIKE         DEFINE    FVVNPG        WNPG
092100000000     C     *LIKE         DEFINE    FVVDFV        WDFV
092200941229     C     *LIKE         DEFINE    FVVFGS        WFGS
092300040128     C     *LIKE         DEFINE    CAEEPA        KEPA
092400000000      *
092500000000     C                   ENDSR
092600000000      **************************************************************************
092700000000      *    IMPOSTO LETTURE LINEE TRANSITI
092800000000      **************************************************************************
092900000000     C     IMPOT         BEGSR
093000000000      *
093100000000      * MI POSIZIONO X > E VEDO SE SONO A FINE FINE
093200000000      *    ALTRIMENTI MI RIPOSIZIONO CON LA NUOVA LINEA LETTA
093300000000      *
093400020823     C     KDTA          SETLL     FNBTP19L
093500000000      *
093600000000     C                   ENDSR
093700000000      **************************************************************************
093800000000      * ELABORAZIONE DATI DELLA BOLLA E DEI PROPRI COLLI
093900000000      **************************************************************************
094000000000     C     ELABB         BEGSR
094100000000      *
094200000000      * Aggancio il tipo bolla dalla tabella 3A per sapere se c/servizio
094300000000      * solo se diverso dal precedente
094400941230     C     ARBCBO        IFNE      $CBLPR
094500000000     C                   Z-ADD     1             TBLKUT
094600000000     C                   MOVE      '3A'          TBLCOD
094700000000     C                   MOVE      *BLANKS       TBLKEY
094800941230     C                   MOVEL     ARBCBO        TBLKEY
094900000000     C     TABELK        CHAIN     TABEL00F                           99
095000000000     C  N99              MOVEL     TBLUNI        DS3A
095100000000     C   99              CLEAR                   DS3A
095200000000     C                   MOVEL     §3ATB1        $ASFR             1            ASSEGN./FRANCO
095300941230     C                   MOVEL     ARBCBO        $CBLPR                         PRECEDENTE
095400000000     C                   ENDIF
095500000000      *
095600000000      * Escludo i recuperi
095700000000     C     §3ARBL        IFNE      'R'                                          'R'=RECUPERI
095800000000      *
095900000000      * Solo al primo giro imposto TFA precedente
096000000000     C     $PRIMO        IFEQ      *ZERO                                        0=PRIMO GIRO
096100000000     C                   Z-ADD     ARBTFA        $TFAPR                         PRECEDENTE
096200000000     C                   Z-ADD     1             $PRIMO                         1=GIA' PASSATO
096300000000     C                   Z-ADD     1             $DTAAR
096400000000     C                   ENDIF
096500000000      *
096600000000      * Se TFA <> dal precedente salvo e pulisco le schiere
096700000000     C     ARBTFA        IFNE      $TFAPR                                       PRECEDENTE
096800000000     C     $PRIMO        ANDNE     *ZERO                                        NO PRIMO GIRO
096900000000     C                   EXSR      SCASCH                                       SCARICA SCHIERE
097000000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
097100980708     C                   Z-ADD     0             $B
097200000000     C                   Z-ADD     ARBTFA        $TFAPR                         PRECEDENTE
097300000000     C                   ENDIF
097400000000     C                   MOVE      *BLANKS       $TRANS                         NO TRANSITO
097500000000      *
097600000000      * Se c/servizio devo trattarlo come filiale a parte (solo per TFP)
097700000000      * Cerco l'indice di schiera per il terminal di partenza
097800000000     C     §3ARBL        IFEQ      'C'                                          'C'=C/SERVIZIO
097900000000     C                   MOVE      *ALL'9'       $LINEA
098000000000     C                   ELSE
098100000619     C* SE ARBLNP E' POSTE TENGO LUI COME TERMINAL DI PARTENZA
098200000619     C                   Z-ADD     1             D
098300000619     C     ARBLNP        LOOKUP    ORG(D)                                 99     ON=TROVATO
098400000619     C   99              Z-ADD     OTP(D)        $LINEA
098500000619     C  N99              Z-ADD     ARBTFP        $LINEA                         999=C/SERVIZIO
098600000619     C                   ENDIF
098700000000     C                   EXSR      INDTFP                                       INDICE TFP (B)
098800000000      *
098900000000      * Cerco l'indice di schiera per la filiale di arrivo
099000000000     C                   Z-ADD     VIDLNA        $LINEA
099100000000     C                   EXSR      INDLNA                                       INDICE LNA (A)
099200000000      *
099300090929      * SPEDIZIONI "E"
099400090929     c                   select
099500090929     C     ARBTSP        wheneq    'E'                                          'E'=ESPRESSI
099600000000      *
099700000000     C     $ASFR         IFEQ      'F'                                          'F'=FRANCO
099800000000     C                   ADD       1             FE(B)                          TFP FRANCHI   E
099900000000     C                   ADD       1             FEA(A)                         LNA    "      E
100000000620     C*
100100000000     C                   ELSE
100200000000     C                   ADD       1             AE(B)                          TFP ASSEGNATI E
100300000000     C                   ADD       1             AEA(A)                         LNA     "     E
100400000620     C                   ENDIF
100500090929
100600090929     C     ARBTSP        wheneq    'H'                                          'H'=h 10 30
100700090929      *
100800090929     C     $ASFR         IFEQ      'F'                                          'F'=FRANCO
100900090929     C                   ADD       1             Fh(B)                          TFP FRANCHI   H
101000090929     C                   ADD       1             FhA(A)                         LNA    "      H
101100090929     C*
101200090929     C                   ELSE
101300090929     C                   ADD       1             Ah(B)                          TFP ASSEGNATI H
101400090929     C                   ADD       1             AhA(A)                         LNA     "     H
101500090929     C                   ENDIF
101600000000      *
101700090929     C                   other                                                  C+D
101800000000      *
101900000000     C     $ASFR         IFEQ      'F'                                          'F'=FRANCO
102000000000     C                   ADD       1             FN(B)                          TFP FRANCHI   N
102100000000     C                   ADD       1             FNA(A)                         LNA    "      N
102200000620     C*
102300000000     C                   ELSE
102400000000     C                   ADD       1             AN(B)                          TFP ASSEGNATI N
102500000000     C                   ADD       1             ANA(A)                         LNA     "     N
102600000620     C                   ENDIF
102700000000      *
102800090929     C                   ENDSL
102801180209     C* Se distribuzione totalizzo distinguendo le partite in giorno "A" dalle altre
102802180209          IF ARBTSP='D' ;
102803180209             chain (arbaas:arblnp:arbnrs:arbnsp) fnblp01l;
102804180209             if %found(fnblp01l);
102805180209             if  blpdrt<>arbdbr  ;
102806180213          //        sdb(b)+=1;
102807180213          //        sdba(a)+=1;
102808180209                else;
102809180209     c*    se partita il giorno 'A' devo verificare che non sia disguidata perchè se
102810180209     c*    disguidata la devo considerare come fosse partita in "B"
102811180209                 wdisg=' ';
102812180209                 if wdisg='S';
102813180213          //        sdb(b)+=1;
102814180213          //        sdba(a)+=1;
102815180209                   else;
102816180213          //        sda(b)+=1;
102817180213          //        sdaa(a)+=1;
102818180209                 ENDIF;
102819180209
102820180209             ENDIF;
102821180209             ENDIF;
102822180209          ENDIF;
102900000000      *
103000000000     C                   ENDIF
103100000000      *
103200000000     C                   ENDSR
103300000000      **************************************************************************
103400000000      * Elaborazione su bolla arrivi
103500000000      * Questa routine faceva parte della ELABC, sono state separate per non
103600000000      * ripetere i calcoli della bolla per ogni collo
103700000000      **************************************************************************
103800000000     C     ELABC1        BEGSR
103900000000      *
104000021218      *
104100021218      * Occhio al peso nella bolla!!     Sulla bolla devo verificare i dati
104200021218      * misurati con la macchina smistacolli (ARBNCP=Numero colli rilevati,
104300021218      * ARBPKC=Peso rilevato)
104400030213      **** Verifico se sono stati pesati   tutti i colli
104500021218sel 1c                   select
104600050111     c                   when      ARBncp = ARBncl
104700021218     c                   z-add     ARBpkc        ARBpkf
104800021218    1c                   when      ARBpkc >= ARBpkf
104900021218     c                   z-add     ARBpkc        ARBpkf
105000021218e   1c                   endsl
105100021220      * Calcolo il PESO   unitario medio per collo da usare se non
105200021220      * tutti i colli sono stati pesati   dalla smistacolli
105300021220     c     ARBpkf        div(H)    ARBncl        Wpesmed
105400000000      *
105500000000      * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
105600000000      * misurati con la macchina smistacolli (ARBNCR=Numero colli rilevati,
105700941230      * ARBVLC=Volume rilevato)
105800030213      **** Verifico se sono stati volumati tutti i colli
105900050111     C     ARBNCR        IFEQ      ARBNCL
106000941230     C                   Z-ADD     ARBVLC        ARBVLF                         VOLUME RILEVATO
106100000000     C                   ELSE
106200941230     C     ARBVLC        IFGE      ARBVLF
106300941230     C                   Z-ADD     ARBVLC        ARBVLF                         VOLUME RILEVATO
106400000000     C                   ENDIF
106500000000     C                   ENDIF
106600021220      * Calcolo il VOLUME unitario medio per collo da usare se non
106700021220      * tutti i colli sono stati pesati   dalla smistacolli
106800021220     c     ARBvlf        div(H)    ARBncl        Wvolmed
106900000000      *
107000000000      * Verifico il tipo bolla con il "precedente"
107100941230     C     ARBCBO        IFNE      $CBLPR
107200000000     C                   Z-ADD     1             TBLKUT
107300000000     C                   MOVEL     '3A'          TBLCOD
107400000000     C                   MOVE      *BLANKS       TBLKEY
107500941230     C                   MOVEL     ARBCBO        TBLKEY
107600000000     C     TABELK        CHAIN     TABEL00F                           99
107700000000     C  N99              MOVEL     TBLUNI        DS3A
107800000000     C   99              CLEAR                   DS3A
107900941230     C                   MOVEL     ARBCBO        $CBLPR                         TIPO PRECEDENTE
108000000000     C                   ENDIF
108100000000      *
108200000000      * CALCOLO PESO TASSABILE (solo sotto i 35 quintali)
108300000000      * N.B.:  Va calcolato sull'intera bolla, indi diviso per il nr.colli
108400941230     C     ARBPKF        IFLE      3500
108500000000      *
108600000000     C     §3ARBL        IFNE      'C'                                          'C'=C/SERVIZIO
108700941230     C                   Z-ADD     ARBPKF        PESTAS
108800941230     C     ARBVLF        IFNE      0
108900941230     C     ARBVLF        MULT      §QTKPM        COMPKT            9 0
109000000000     C     COMPKT        IFGT      PESTAS
109100000000     C                   Z-ADD     COMPKT        PESTAS
109200000000     C                   ENDIF
109300000000     C                   ENDIF
109400000000      *
109500000000      * ARROTONDAMENTO DI 20 KG
109600000000     C                   Z-ADD     0             COMPKT
109700000000     C     PESTAS        DIV       §3IARR        COMPKT
109800000000     C                   MVR                     RESTO
109900000000     C     RESTO         IFGT      0
110000000000     C     COMPKT        MULT      §3IARR        PESTAS
110100000000     C                   ADD       §3IARR        PESTAS
110200000000     C                   ENDIF
110300000000      *
110400000000     C                   DIV       ARBNCL        PESTAS
110500000000      *
110600000000     C                   ENDIF
110700000000     C                   ENDIF
110800000000      *
110900000000     C                   ENDSR
111000000000      **************************************************************************
111100000000      * Elaborazione colli, assegnazione pesi e volumi
111200000000      **************************************************************************
111300000000     C     ELABC         BEGSR
111400000000      *
111500000000      * Se bolla di recupero la salto
111600000000     C     §3ARBL        IFNE      'R'                                          'R'=RECUPERI
111700000000      *
111800000000      * Solo al primo giro imposto TFA precedente
111900000000     C     $PRIMO        IFEQ      *ZERO                                        0=PRIMO GIRO
112000000000     C                   Z-ADD     ARBTFA        $TFAPR                         PRECEDENTE
112100000000     C                   Z-ADD     1             $PRIMO                         1=GIA' PASSATO
112200000000     C                   Z-ADD     1             $DTAAR
112300000000     C                   ENDIF
112400000000      *
112500000000      * Come per le bolle rompo quando cambia terminal di arrivo
112600000000     C     ARBTFA        IFNE      $TFAPR                                       PRECEDENTE
112700000000     C     $PRIMO        ANDNE     *ZERO                                        NO PRIMO GIRO
112800000000     C                   EXSR      SCASCH                                       SCARICA SCHIERE
112900000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
113000980708     C                   Z-ADD     0             $B
113100000000     C                   Z-ADD     ARBTFA        $TFAPR                         PRECEDENTE
113200000000     C                   ENDIF
113300000000     C                   MOVE      *BLANKS       $TRANS                         NO TRANSITO
113400000000      *
113500000000      * Se c/servizio devo trattarlo come filiale a parte (solo per TFP)
113600000000      * Cerco l'indice di schiera per il terminal di partenza
113700000000     C     §3ARBL        IFEQ      'C'                                          'C'=C/SERVIZIO
113800000000     C                   MOVE      *ALL'9'       $LINEA
113900000000     C                   ELSE
114000000619     C* SE ARBLNP E' POSTE TENGO LUI COME TERMINAL DI PARTENZA
114100000619     C                   Z-ADD     1             D
114200000619     C     ARBLNP        LOOKUP    ORG(D)                                 99     ON=TROVATO
114300000619     C   99              Z-ADD     OTP(D)        $LINEA
114400000619     C  N99              Z-ADD     ARBTFP        $LINEA                         999=C/SERVIZIO
114500000000     C                   ENDIF
114600000000     C                   EXSR      INDTFP                                       INDICE TFP (B)
114700000000      *
114800000000      * Cerco l'indice di schiera per la filiale di arrivo
114900000000     C                   Z-ADD     ARTLNA        $LINEA
115000000000     C                   EXSR      INDLNA                                       INDICE LNA (A)
115100000000      *
115200000000     C                   ADD       1             CLL(B)                         TFP NR.COLLI
115300000000     C                   ADD       1             CLLA(A)                        LNA NR.COLLI
115301180209     C* incompatibili
115302180209         chain (artfls:artlna:artnrs:artnsc:'J') fiars01l;
115303180209         if %found(fiars01l);
115304180209            darsj=arsnot;
115305180209      * da spunta
115306180209            if §ARSJTIPO='S';
115307180209               cis(b)+=1;
115308180209               cisa(a)+=1;
115309180209            else;
115310180209      * da foglio
115311180209               cif(b)+=1;
115312180209               cifa(a)+=1;
115313180209            endif;
115314180209         ENDIF;
115400000000      *
115500021218      *
115600021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
115700030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
115800030213      * Peso medio - per "standardizzare" con i pgm. FNLS*
115900030213      * ("metti" che qualche collo NON sia stato pesato in partenza...)
116000021220     c                   z-add     Wpesmed       $Peso
116100000000      *
116200000000      * CALCOLO PESO
116300021218     C     ARBPKF        IFLE      3500
116400000000     C                   ADD(H)    $PESO         F35(B)
116500000000     C                   ADD(H)    $PESO         F35A(A)
116600000000      *
116700000000      * CALCOLO PESO TASSABILE (solo sotto i 35 quintali)
116800000000      * N.B.:  Va calcolato sull'intera bolla, indi diviso per il nr.colli
116900000000     C     §3ARBL        IFNE      'C'                                          'C'=C/SERVIZIO
117000000000     C                   ADD       PESTAS        PTA(B)
117100000000     C                   ADD       PESTAS        PTAA(A)
117200000620     C                   ENDIF
117300000000      *
117400000000     C                   ELSE
117500000000     C                   ADD(H)    $PESO         O35(B)
117600000000     C                   ADD(H)    $PESO         O35A(A)
117700000620     C                   ENDIF
117800021218      *
117900021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
118000030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
118100030213      * Volume medio - per "standardizzare" con i pgm. FNLS*
118200030213      * ("metti" che qualche collo NON sia stato volumato in partenza...)
118300021220     c                   z-add     Wvolmed       $Volum
118400000000      *
118500000000      * CALCOLO VOLUME
118600000000     C     $VOLUM        IFEQ      0
118700000000     C                   Z-ADD     0,0001        $VOLUM
118800000000     C                   ENDIF
118900000000     C                   ADD(H)    $VOLUM        VOL(B)
119000000000     C                   ADD(H)    $VOLUM        VOLA(A)
119001180209     C* Distribuzione
119002180209           if arbtsp = 'D' and wesblp='S';
119003180209              if artdfv<>blpdrt;
119004180213        //        cdb(b)+=1 ;
119005180213        //        cdba(a)+=1;
119007180209              else ;
119008180209                  clear wdisg;
119009180209                  if wdisg='S';
119010180209                  else;
119011180213        //          cda(b)+=1;
119012180213        //          cdaa(a)+=1;
119013180209                  ENDIF;
119014180209              ENDIF;
119015180209           ENDIF;
119100000000      *
119200000000     C                   ENDIF
119300000000      *
119400000000     C                   ENDSR
119500000000      **************************************************************************
119600000000      * ELABORAZIONE DATI DELLA BOLLA TRANSITO E DEI PROPRI COLLI
119700000000      **************************************************************************
119800000000     C     ELABT         BEGSR
119900000619     C*
120000000619     C                   Z-ADD     1             D
120100000619     C     BTPLNP        LOOKUP    ORG(D)                                 99     ON=TROVATO
120200000619     C   99              Z-ADD     OTP(D)        $LINTR            3 0
120300000619     C  N99              Z-ADD     BTPTFP        $LINTR
120400000000      *
120500000000      * SE HO LETTO LINEA PARTENZA NUOVA MEMORIZZO
120600000000     C                   Z-ADD     1             B                 3 0
120700000619     C     $LINTR        LOOKUP    TFPT(B)                                32     TERMINAL PART.
120800000000     C  N32              ADD       1             $B                             CONTATORE TFPT
120900000000     C  N32              Z-ADD     $B            B
121000000703     C  N32              Z-ADD     $LINTR        TFPT(B)                        NUOVA FILIALE
121100000000      *
121200000000      * CHAIN SU TABELLE SOLO SE TIPO BOLLA <> DAL PRECEDENTE
121300941230     C     BTPCBO        IFNE      $CBLPR                                       <> PRECEDENTE
121400000000     C                   Z-ADD     1             TBLKUT
121500000000     C                   MOVE      '3A'          TBLCOD
121600000000     C                   MOVE      *BLANKS       TBLKEY
121700941230     C                   MOVEL     BTPCBO        TBLKEY
121800000000     C     TABELK        CHAIN     TABEL00F                           99
121900000000     C  N99              MOVEL     TBLUNI        DS3A
122000000000     C   99              CLEAR                   DS3A
122100941230     C                   MOVEL     BTPCBO        $CBLPR                         PRECEDENTE
122200000000     C                   ENDIF
122300000000      *
122400000000      * Recuperi esclusi
122500000000     C     §3ARBL        IFNE      'R'
122600000000      *
122700090929      * CALCOLO SPEDIZIONI
122800000000     C                   MOVEL     §3ATB1        $ASFR                          ASSEGN./FRANCO
122900090929     C                   SELECT
123000090929     C     BTPTSP        WHENEQ    'E'                                          'E'=ESPRESSO
123100000000      *
123200000000     C     $ASFR         IFEQ      'F'
123300000000     C                   ADD       1             FET(B)                         FRANCHI     ESP
123400000000     C                   ADD       1             FED               7 0             "        ESP
123500000000     C                   ELSE
123600000000     C                   ADD       1             AET(B)                         ASSEGNATI   ESP
123700000000     C                   ADD       1             AED               7 0             "        ESP
123800000000     C                   END
123900000000      *
124000090929     C     BTPTSP        WHENEQ    'H'                                          'H'=H 10 30
124100090929      *
124200090929     C     $ASFR         IFEQ      'F'
124300090929     C                   ADD       1             FHT(B)                         FRANCHI     "H"
124400090929     C                   ADD       1             FHD               7 0             "        "H"
124500090929     C                   ELSE
124600090929     C                   ADD       1             AHT(B)                         ASSEGNATI   "H"
124700090929     C                   ADD       1             AHD               7 0             "        "H"
124800090929     C                   END
124900090929      *
125000090929     C                   OTHER
125100000000      *
125200000000     C     $ASFR         IFEQ      'F'                                          'F'=FRANCO
125300000000     C                   ADD       1             FNT(B)                         FRANCHI     NOR
125400000000     C                   ADD       1             FND               7 0             "        NOR
125500000000     C                   ELSE
125600000000     C                   ADD       1             ANT(B)                         ASSEGNATI   NOR
125700000000     C                   ADD       1             AND               7 0             "        NOR
125800000000     C                   END
125900000000      *
126000090929     C                   ENDSL
126100000000      *
126200000000     C                   ENDIF                                                  $CBLPR<>'R'
126300000000      *
126400021218     C                   ENDSR
126500000000      **************************************************************************
126600000000      * Elaborazione bolla transiti
126700000000      **************************************************************************
126800000000     C     ELACT1        BEGSR
126900000000      *
127000000000      * Verifico il tipo bolla con il "precedente"
127100941230     C     BTPCBO        IFNE      $CBLPR
127200000000     C                   Z-ADD     1             TBLKUT
127300000000     C                   MOVEL     '3A'          TBLCOD
127400000000     C                   MOVE      *BLANKS       TBLKEY
127500941230     C                   MOVEL     BTPCBO        TBLKEY
127600000000     C     TABELK        CHAIN     TABEL00F                           99
127700000000     C  N99              MOVEL     TBLUNI        DS3A
127800000000     C   99              CLEAR                   DS3A
127900941230     C                   MOVEL     BTPCBO        $CBLPR                         TIPO PRECEDENTE
128000000000     C                   ENDIF
128100021218     *** *
128200021218     ***C     BTPPKB        DIV(H)    BTPNCL        $PESO                          PESO UNITARIO
128300021218      *
128400021218      * Occhio al peso nella bolla!!     Sulla bolla devo verificare i dati
128500021218      * misurati con la macchina smistacolli (BTPNCP=Numero colli rilevati,
128600021218      * BTPPKC=Peso rilevato)
128700030213      **** Verifico se sono stati pesati   tutti i colli
128800030213     c***                clear                   Wsipes
128900021218sel 1c                   select
129000050111     c                   when      BTPncp = BTPncl
129100021218     c                   z-add     BTPpkc        BTPpkb
129200030213     c***                eval      Wsipes =  'S'
129300021218    1c                   when      BTPpkc >= BTPpkb
129400021218     c                   z-add     BTPpkc        BTPpkb
129500021218e   1c                   endsl
129600000000      *
129700000000      * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
129800021218      * misurati con la macchina smistacolli (BTPNCR=Numero colli rilevati,
129900021218      * BTPVLC=Volume rilevato)
130000030213     c***                clear                   Wsivol
130100050111     C     BTPNCR        ifeq      BTPNCL
130200941230     C                   Z-ADD     BTPVLC        BTPVLB                         VOLUME RILEVATO
130300030213     c***                eval      Wsivol =  'S'
130400000000     C                   ELSE
130500941230     C     BTPVLC        IFGE      BTPVLB
130600941230     C                   Z-ADD     BTPVLC        BTPVLB                         VOLUME RILEVATO
130700000000     C                   ENDIF
130800000000     C                   ENDIF
130900000000      *
131000000000      * CALCOLO PESO TASSABILE
131100941230     C     BTPPKB        IFLE      3500
131200000000     C     §3ARBL        IFNE      'C'                                          'C'=C/SERVIZIO
131300000000      *
131400021220     ***C                   Z-ADD     $PESO         PESTAS
131500021220     C                   Z-ADD     BTPPKB        PESTAS
131600941230     C     BTPVLB        IFNE      0
131700941230     C     BTPVLB        MULT      §QTKPM        COMPKT
131800000000     C     COMPKT        IFGT      PESTAS
131900000000     C                   Z-ADD     COMPKT        PESTAS
132000000000     C                   ENDIF
132100000000     C                   ENDIF
132200000000      *
132300000000      * ARROTONDAMENTO DI 20 KG
132400000000     C                   Z-ADD     0             COMPKT
132500000000     C     PESTAS        DIV       §3IARR        COMPKT
132600000000     C                   MVR                     RESTO             3 0
132700000000     C     RESTO         IFGT      0
132800000000     C     COMPKT        MULT      §3IARR        PESTAS            9 1
132900000000     C                   ADD       §3IARR        PESTAS
133000000000     C                   ENDIF
133100000000      *
133200941230     C                   DIV       BTPNCL        PESTAS
133300000000      *
133400000000     C                   ENDIF
133500000000     C                   ENDIF
133600000000      *
133700000000     C                   ENDSR
133800000000      **************************************************************************
133900000000      * Elaborazione colli transiti, assegnazione pesi e volumi
134000000000      **************************************************************************
134100000000     C     ELABCT        BEGSR
134200000000      *
134300000000      * Se bolla di recupero la salto
134400000000     C     §3ARBL        IFNE      'R'                                          'R'=RECUPERI
134500000000      *
134600000000      * Verifico se ho gia' incontrato questo terminal di partenza
134700000703     C*
134800000703     C                   Z-ADD     1             D
134900000703     C     BTPLNP        LOOKUP    ORG(D)                                 99     ON=TROVATO
135000000703     C   99              Z-ADD     OTP(D)        $LINTR            3 0
135100000703     C  N99              Z-ADD     BTPTFP        $LINTR
135200000703      *
135300000000     C                   Z-ADD     1             B
135400000703     C     $LINTR        LOOKUP    TFPT(B)                                32     TERMINAL PART.
135500000000     C  N32              ADD       1             $B                             CONTATORE TFPT
135600000000     C  N32              Z-ADD     $B            B
135700000703     C  N32              Z-ADD     $LINTR        TFPT(B)                        NUOVA FILIALE
135800000000      *
135900000000     C                   ADD       1             CLLT(B)                         TFP NR.COLLI
136000000000     C                   ADD       1             CLLD              7 0          LNA NR.COLLI
136100000000      *
136200021218     ***C     BTTVUC        IFEQ      *ZERO
136300021218     *** *
136400021218     ***C     BTPVLB        DIV(H)    BTPNCL        $VOLUM                         VOLUME UNITARIO
136500021218     *** *
136600021218     ***C                   ELSE
136700021218     ***C                   Z-ADD     BTTVUC        $VOLUM                         DA SMISTACOLLI
136800021218     ***C                   ENDIF
136900021218      *
137000021218      * Calcolo il peso medio
137100030213    1c***                when      Wsipes =  'S' and BTTpuc > 0
137200030213     c***                z-add     BTTpuc        $Peso
137300021218     c     BTPpkb        div(H)    BTPncl        $Peso
137400000000      *
137500000000      * CALCOLO PESO
137600941230     C     BTPPKB        IFLE      3500
137700000000     C                   ADD(H)    $PESO         F35T(B)
137800000000     C                   ADD(H)    $PESO         F35D              9 1
137900000000      *
138000000000      * CALCOLO PESO TASSABILE
138100000000     C     §3ARBL        IFNE      'C'                                          'C'=C/SERVIZIO
138200000000     C                   ADD       PESTAS        PTAT(B)
138300000000     C                   ADD       PESTAS        PTAD              9 1
138400000000     C                   ENDIF
138500000000      *
138600000000     C                   ELSE
138700000000     C                   ADD(H)    $PESO         O35T(B)
138800000000     C                   ADD(H)    $PESO         O35D              9 1
138900000000     C                   ENDIF
139000021218      *
139100021218      * Calcolo il volume medio
139200030213    1c***                when      Wsivol =  'S' and BTTvuc > 0
139300030213     c***                z-add(H)  BTTvuc        $Volum
139400021218     c     BTPvlb        div(H)    BTPncl        $Volum
139500000000      *
139600000000      * CALCOLO VOLUME
139700000000     C     $VOLUM        IFEQ      0
139800000000     C                   Z-ADD     0,0001        $VOLUM
139900000000     C                   ENDIF
140000000000     C                   ADD(H)    $VOLUM        VOLT(B)
140100000000     C                   ADD(H)    $VOLUM        VOLD              9 4
140200000000      *
140300000000     C                   ENDIF
140400000000      *
140500000000     C                   ENDSR
140600000000      **************************************************************************
140700090929      * SCARICO LE SCHIERE NEL FILE DELLE STATISTICHE (FISAR00F)
140800000000      * La routine e' generica e controlla, prima di salvare, se esiste gia'
140900000000      * una precdente registrazione.  Tutte le schiere sono messe in ADD ai
141000000000      * campi del file sempre; sara' compito di chi chiama la routine di
141100000000      * avere piene solo le schiere interessate (es.: nella fase di calcolo dei
141200000000      * disguidi, le schiere piene sono solo DIS e DISA, oltre naturalmente a
141300000000      * TFP e LNA).
141400000000      **************************************************************************
141500000000     C     SCASCH        BEGSR
141600000000      *
141700000000      *?    T E R M I N A L    D I    P A R T E N Z A    ?
141800000000      *
141900000000     C                   Z-ADD     1             P                 4 0
142000000000     C     P             DOWLE     $MAX
142100000000      *
142200000000     C     CLL(P)        IFGT      *ZERO
142300000000     C     P88(P)        ORGT      *ZERO
142400000000     C     DIS(P)        ORGT      *ZERO
142500000000     C     PAR(P)        ORGT      *ZERO
142600000000     C     FN(P)         ORGT      *ZERO
142700000000     C     AN(P)         ORGT      *ZERO
142800000000     C     FE(P)         ORGT      *ZERO
142900000000     C     AE(P)         ORGT      *ZERO
143000090929     C     Fh(P)         ORGT      *ZERO
143100090929     C     Ah(P)         ORGT      *ZERO
143200090929     C                   CLEAR                   FISAR000
143201180208     C                   clear                   tisar000
143300000000     C                   Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
143400000000     C                   Z-ADD     $TFAPR        SARTFA                         TFA PRECEDENTE
143500950214     C                   Z-ADD     DS1ADA        SARDRE                         DATA
143600000000     C                   Z-ADD     TFP(P)        SARTFP                         TERM.PARTENZA
143700000000     C                   Z-ADD     *ZERO         SARLNA                         LINEA DI ARRIVO
143800000000     C     TFP(P)        IFNE      999
143900000000     C                   MOVE      *BLANKS       SARTRE                         TIPO RECORD
144000000000     C                   ELSE
144100000000     C                   MOVE      'C'           SARTRE                         'C'=CONTO SERV.
144200000000     C                   ENDIF
144300090929     C     FISARK        CHAIN     FISAR01L                           99
144301180208     C     FISARK        CHAIN     TISAR01L                           97
144400000000     C                   ADD       CLL(P)        SARCLL                         COLLI
144500000000     C                   ADD       P88(P)        SARP88                         PIST.88 NON ARR
144600000000     C                   ADD       DIS(P)        SARDIS                         DISGUIDI
144700000000     C                   ADD       PAR(P)        SARPAR                         PARTITI NON SPU
144800000000     C                   ADD       PTA(P)        SARPTA                         PESO TASSABILE
144900000000     C                   ADD       F35(P)        SARF35                         FINO A 35 Q.
145000000000     C                   ADD       O35(P)        SARO35                         PESO OLTRE 35 Q
145100000000     C                   ADD       VOL(P)        SARVOL                         VOLUME
145200090929     C                   ADD       FN(P)         SARSNF                         SPED.C+D  FRANC
145300090929     C                   ADD       AN(P)         SARSNA                         SPED.C+D  ASSEG
145400090929     C                   ADD       FE(P)         SARSEF                         SPED."E"  FRANC
145500090929     C                   ADD       AE(P)         SARSEA                         SPED."E"  ASSEG
145600090929     C                   ADD       FH(P)         SARSHF                         SPED."H"  FRANC
145700090929     C                   ADD       AH(P)         SARSHA                         SPED."H"  ASSEG
145701180209     C                   ADD       CIF(p)        SARcif                         colli incomp.foglio
145702180209     C                   ADD       CIS(p)        SARcis                         colli incomp.spunte
145703180209     C                   ADD       SDA(p)        SARSDTA                        sped "D" giorno "A"
145704180209     C                   ADD       SDB(p)        SARSDTB                        Spedi "D" giorno "B"
145705180209     C                   ADD       CDA(p)        SARCDTA                        colli "D" giorno "A"
145706180209     C                   ADD       CDB(p)        SARCDTB                        colli "D" giorno "B"
145800040923     c                   z-add     dateu         sardtr
145900000000      *
146000090929     C   99              WRITE     FISAR000
146100090929     C  N99              UPDATE    FISAR000
146101180208     C   97              WRITE     TISAR000
146102180208     C  N97              UPDATE    TISAR000
146200000000     C                   ENDIF
146300000000     C                   ADD       1             P
146400000000      *
146500000000     C                   ENDDO
146600000000      *
146700000000      *?    L I N E E    D I    A R R I V O    ?
146800000000     C                   Z-ADD     1             P
146900000000     C     P             DOWLE     29
147000000000      *
147100090929     C* LNA CON TFP
147200970605     C     CLLA(P)       IFGT      *ZERO
147300970605     C     P88A(P)       ORGT      *ZERO
147400970605     C     DISA(P)       ORGT      *ZERO
147500970605     C     PARA(P)       ORGT      *ZERO
147600970605     C     FNA(P)        ORGT      *ZERO
147700970605     C     ANA(P)        ORGT      *ZERO
147800970605     C     FEA(P)        ORGT      *ZERO
147900970605     C     AEA(P)        ORGT      *ZERO
148000090929     C     FHA(P)        ORGT      *ZERO
148100090929     C     AHA(P)        ORGT      *ZERO
148200090929     C                   CLEAR                   FISAR000
148201180209     C                   CLEAR                   TISAR000
148300000000     C                   Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
148400000000     C                   Z-ADD     $TFAPR        SARTFA                         TFA PRECEDENTE
148500950214     C                   Z-ADD     DS1ADA        SARDRE                         DATA
148600000000     C                   Z-ADD     *ZERO         SARTFP                         TERM.PARTENZA
148700000000     C                   Z-ADD     LNA(P)        SARLNA                         LINEA DI ARRIVO
148800000000     C                   MOVE      *BLANKS       SARTRE                         TIPO RECORD
148900090929     C     FISARK        CHAIN     FISAR01L                           99
148901180209     C     FISARK        CHAIN     TISAR01L                           97
149000000000     C                   ADD       CLLA(P)       SARCLL                         COLLI
149100000000     C                   ADD       P88A(P)       SARP88                         PIST.88 NON ARR
149200000000     C                   ADD       DISA(P)       SARDIS                         DISGUIDI
149300000000     C                   ADD       PARA(P)       SARPAR                         PARTITI NON SPU
149400000000     C                   ADD       PTAA(P)       SARPTA                         PESO TASSABILE
149500000000     C                   ADD       F35A(P)       SARF35                         FINO A 35 Q.
149600000000     C                   ADD       O35A(P)       SARO35                         PESO OLTRE 35 Q
149700000000     C                   ADD       VOLA(P)       SARVOL                         VOLUME
149800090929     C                   ADD       FNA(P)        SARSNF                         SPED.C+D  FRANC
149900090929     C                   ADD       ANA(P)        SARSNA                         SPED.C+D  ASSEG
150000090929     C                   ADD       FEA(P)        SARSEF                         SPED."E"  FRANC
150100090929     C                   ADD       AEA(P)        SARSEA                         SPED."E"  ASSEG
150200090929     C                   ADD       FHA(P)        SARSHF                         SPED."H"  FRANC
150300090929     C                   ADD       AHA(P)        SARSHA                         SPED."H"  ASSEG
150301180209     C                   ADD       CIFa(p)       SARcif                         colli incomp.foglio
150302180209     C                   ADD       CISa(p)       SARcis                         colli incomp.spunte
150303180209     C                   ADD       SDAa(p)       SARSDTA                        Sped "D" giorno "A"
150304180209     C                   ADD       SDBa(p)       SARSDTB                        Sped "D" giorno "B"
150305180209     C                   ADD       CDAa(p)       SARCDTA                        colli "D" giorno "A"
150306180209     C                   ADD       CDBa(p)       SARCDTB                        colli "D" giorno "B"
150400040923     c                   z-add     dateu         sardtr
150500000000      *
150600090929     C   99              WRITE     FISAR000
150700090929     C  N99              UPDATE    FISAR000
150701180209     C   97              WRITE     TISAR000
150702180209     C  N97              UPDATE    TISAR000
150800000000     C                   ENDIF
150900000000     C                   ADD       1             P
151000000000      *
151100000000     C                   ENDDO
151200000000      *
151300000000     C                   ENDSR
151400000000      **************************************************************************
151500000000      * E' uguale alla routine SCASCH ma rivolta ai TRANSITI, con altra schiere
151600000000      **************************************************************************
151700000000     C     SCASCT        BEGSR
151800000000      *
151900000000      *?    T E R M I N A L    D I    P A R T E N Z A    ?
152000000000      *
152100000000     C                   Z-ADD     1             P
152200000000     C     P             DOWLE     $MAX
152300000000      *
152400000000     C     CLLT(P)       IFGT      *ZERO
152500000000     C     P88T(P)       ORGT      *ZERO
152600000000     C     PART(P)       ORGT      *ZERO
152700000000     C     FNT(P)        ORGT      *ZERO
152800000000     C     ANT(P)        ORGT      *ZERO
152900000000     C     FET(P)        ORGT      *ZERO
153000000000     C     AET(P)        ORGT      *ZERO
153100090929     C     FHT(P)        ORGT      *ZERO
153200090929     C     AHT(P)        ORGT      *ZERO
153300090929     C                   CLEAR                   FISAR000
153301180209     C                   CLEAR                   TISAR000
153400000000     C                   Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
153500000000     C                   Z-ADD     SIMFEL        SARTFA                         TFA
153600950214     C                   Z-ADD     DS1ADA        SARDRE                         DATA
153700000000     C                   Z-ADD     TFPT(P)       SARTFP                         TERM.PARTENZA
153800000000     C                   Z-ADD     *ZERO         SARLNA                         LINEA DI ARRIVO
153900000000     C                   MOVE      'T'           SARTRE                         'T'=TRANSITI
154000090929     C     FISARK        CHAIN     FISAR01L                           99
154001180209     C     FISARK        CHAIN     TISAR01L                           97
154100000000     C                   ADD       CLLT(P)       SARCLL                         COLLI
154200000000     C                   ADD       P88T(P)       SARP88                         PIST.88 NON ARR
154300000000     C                   Z-ADD     *ZERO         SARDIS                         DISGUIDI
154400000000     C                   ADD       PART(P)       SARPAR                         PARTITI NON SPU
154500000000     C                   ADD       PTAT(P)       SARPTA                         PESO TASSABILE
154600000000     C                   ADD       F35T(P)       SARF35                         FINO A 35 Q.
154700000000     C                   ADD       O35T(P)       SARO35                         PESO OLTRE 35 Q
154800000000     C                   ADD       VOLT(P)       SARVOL                         VOLUME
154900090929     C                   ADD       FNT(P)        SARSNF                         SPED.C+D  FRANC
155000090929     C                   ADD       ANT(P)        SARSNA                         SPED.C+D  ASSEG
155100090929     C                   ADD       FET(P)        SARSEF                         SPED."E"  FRANC
155200090929     C                   ADD       AET(P)        SARSEA                         SPED."E"  ASSEG
155300090929     C                   ADD       FHT(P)        SARSHF                         SPED."H"  FRANC
155400090929     C                   ADD       AHT(P)        SARSHA                         SPED."H"  ASSEG
155500040923     c                   z-add     dateu         sardtr
155600000000      *
155700090929     C   99              WRITE     FISAR000
155800090929     C  N99              UPDATE    FISAR000
155801180209     C   97              WRITE     TISAR000
155802180209     C  N97              UPDATE    TISAR000
155900000000     C                   ENDIF
156000000000     C                   ADD       1             P
156100000000      *
156200000000     C                   ENDDO
156300000000      *
156400000000      *?    L I N E E    D I    A R R I V O    ?
156500000000     C     CLLD          IFGT      *ZERO
156600000000     C     P88D          ORGT      *ZERO
156700000000     C     PARD          ORGT      *ZERO
156800000000     C     FND           ORGT      *ZERO
156900000000     C     AND           ORGT      *ZERO
157000000000     C     FED           ORGT      *ZERO
157100000000     C     AED           ORGT      *ZERO
157200090929     C     FHD           ORGT      *ZERO
157300090929     C     AHD           ORGT      *ZERO
157400090929     C                   CLEAR                   FISAR000
157401180209     C                   CLEAR                   TISAR000
157500000000     C                   Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
157600000000     C                   Z-ADD     SIMFEL        SARTFA                         TFA
157700950214     C                   Z-ADD     DS1ADA        SARDRE                         DATA
157800000000     C                   Z-ADD     *ZERO         SARTFP                         TERM.PARTENZA
157900000000     C                   Z-ADD     SIMFEL        SARLNA                         LINEA DI ARRIVO
158000000000     C                   MOVE      'T'           SARTRE                         TIPO RECORD
158100090929     C     FISARK        CHAIN     FISAR01L                           99
158101180209     C     FISARK        CHAIN     TISAR01L                           97
158200000000     C                   ADD       CLLD          SARCLL                         COLLI
158300000000     C                   ADD       P88D          SARP88                         PIST.88 NON ARR
158400000000     C                   Z-ADD     *ZERO         SARDIS                         DISGUIDI
158500000000     C                   ADD       PARD          SARPAR                         PARTITI NON SPU
158600000000     C                   ADD       PTAD          SARPTA                         PESO TASSABILE
158700000000     C                   ADD       F35D          SARF35                         FINO A 35 Q.
158800000000     C                   ADD       O35D          SARO35                         PESO OLTRE 35 Q
158900000000     C                   ADD       VOLD          SARVOL                         VOLUME
159000090929     C                   ADD       FND           SARSNF                         SPED.C+D  FRANC
159100090929     C                   ADD       AND           SARSNA                         SPED.C+D  ASSEG
159200090929     C                   ADD       FED           SARSEF                         SPED."E"  FRANC
159300090929     C                   ADD       AED           SARSEA                         SPED."E"  ASSEG
159400090929     C                   ADD       FHD           SARSHF                         SPED."H"  FRANC
159500090929     C                   ADD       AHD           SARSHA                         SPED."H"  ASSEG
159600040923     c                   z-add     dateu         sardtr
159700000000      *
159800090929     C   99              WRITE     FISAR000
159900090929     C  N99              UPDATE    FISAR000
159901180209     C   97              WRITE     TISAR000
159902180209     C  N97              UPDATE    TISAR000
160000000000     C                   ENDIF
160100000000      *
160200000000     C                   ENDSR
160300000000      **************************************************************************
160400090929      * SCARICO LE SCHIERE NEL FILE DELLE STATISTICHE (FISAR00F)
160500000000      * La routine e' generica e controlla, prima di salvare, se esiste gia'
160600000000      * una precdente registrazione.  Tutte le schiere sono messe in ADD ai
160700000000      * campi del file sempre; sara' compito di chi chiama la routine di
160800000000      * avere piene solo le schiere interessate (es.: nella fase di calcolo
160900000000      * delle LNA di cui solo ..., le schiere piene sono solo CLLR, P88R e PARR
161000000000      * oltre a LNAR)
161100000000      **************************************************************************
161200000000     C     SCASCR        BEGSR
161300000000      *
161400000000      *?    LINEE  DI  ARRIVO  DI  CUI  SOLO  TERMINAL    ?
161500000000      *
161600000000     C                   Z-ADD     1             P
161700000000     C     P             DOWLE     50
161800000000      *
161900000000     C     CLLR(P)       IFGT      *ZERO
162000000000     C     P88R(P)       ORGT      *ZERO
162100000000     C     PARR(P)       ORGT      *ZERO
162200090929     C                   CLEAR                   FISAR000
162201180209     C                   CLEAR                   TISAR000
162300000000     C                   Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
162400000000     C                   Z-ADD     SIMFEL        SARTFA                         TFA
162500950214     C                   Z-ADD     DS1ADA        SARDRE                         DATA
162600000000     C                   Z-ADD     *ZERO         SARTFP                         TERM.PARTENZA
162700000000     C                   Z-ADD     LNAR(P)       SARLNA                         LINEA DI ARRIVO
162800000000     C                   MOVE      'R'           SARTRE                         'R'=SOLO TERM.
162900090929     C     FISARK        CHAIN     FISAR01L                           99
162901180209     C     FISARK        CHAIN     TISAR01L                           97
163000000000     C                   ADD       CLLR(P)       SARCLL                         COLLI
163100000000     C                   ADD       P88R(P)       SARP88                         PIST.88 NON ARR
163200000000     C                   Z-ADD     *ZERO         SARDIS                         DISGUIDI
163300000000     C                   ADD       PARR(P)       SARPAR                         PARTITI NON SPU
163400000000     C                   Z-ADD     *ZERO         SARPTA                         PESO TASSABILE
163500000000     C                   Z-ADD     *ZERO         SARF35                         FINO A 35 Q.
163600000000     C                   Z-ADD     *ZERO         SARO35                         PESO OLTRE 35 Q
163700000000     C                   Z-ADD     *ZERO         SARVOL                         VOLUME
163800090929     C                   Z-ADD     *ZERO         SARSNF
163900090929     C                   Z-ADD     *ZERO         SARSNA
164000090929     C                   Z-ADD     *ZERO         SARSEF
164100090929     C                   Z-ADD     *ZERO         SARSEA
164200090929     C                   Z-ADD     *ZERO         SARSHF
164300090929     C                   Z-ADD     *ZERO         SARSHA
164400040923     c                   z-add     dateu         sardtr
164500000000      *
164600090929     C   99              WRITE     FISAR000
164700090929     C  N99              UPDATE    FISAR000
164701180209     C   97              WRITE     TISAR000
164702180209     C  N97              UPDATE    TISAR000
164800000000     C                   ENDIF
164900000000     C                   ADD       1             P
165000000000      *
165100000000     C                   ENDDO
165200000000      *
165300000000     C                   ENDSR
165400000000      **************************************************************************
165500000000      * Azzeramento delle schiere di elaborazione
165600000000      **************************************************************************
165700000000     C     AZZSCH        BEGSR
165800970605     C*
165900970605     C* PRIMA DI CLEARARE LNA/LNAR MEMORIZZO IN UN'ALTRA SCHIERA LE
166000970605     C* LINEE ELABORATE
166100040130     C***                EXSR      AGGLNT
166200000000      *
166300000704     C                   CLEAR                   $B
166400000000     C                   CLEAR                   TFP                            TERMINAL PARTEN
166500000000     C                   CLEAR                   CLL                            TOT COLLI
166600000000     C                   CLEAR                   P88                            PIST.88 NON ARR
166700000000     C                   CLEAR                   DIS                            DISGUIDI
166800000000     C                   CLEAR                   PAR                            PARTITI NON SPU
166900000000     C                   CLEAR                   F35                            FINE  35 Q.LI
167000000000     C                   CLEAR                   O35                            OLTRE 35 Q.LI
167100000000     C                   CLEAR                   PTA                            PESO TASSABILE
167200000000     C                   CLEAR                   VOL                            VOLUME
167300090929     C                   CLEAR                   FN                             FRANCO C+D
167400090929     C                   CLEAR                   FE                             FRANCO "E"
167500090929     C                   CLEAR                   FH                             FRANCO "H"
167600090929     C                   CLEAR                   AN                             ASSEGN C+D
167700090929     C                   CLEAR                   AE                             ASSEGN "E"
167800090929     C                   CLEAR                   AH                             ASSEGN "H"
167801180209     C                   CLEAR                   CIF                            incomp.Foglio
167802180209     C                   CLEAR                   CIS                            incomp.Spunte
167803180209     C                   CLEAR                   SDA                            Sped "D" giorno "A"
167804180209     C                   CLEAR                   SDB                            Sped "D" giorno "B"
167805180209     C                   CLEAR                   CDA                            colli "D" giorno "A"
167806180209     C                   CLEAR                   CDB                            colli "D" giorno "B"
167900000000      *
168000940720     C                   Z-ADD     *ZERO         $A                             CONTATORE LNA
168100000000     C                   CLEAR                   LNA                            LINEE ARRIVO
168200000000     C                   CLEAR                   CLLA                           TOT COLLI
168300000000     C                   CLEAR                   P88A                           PIST.88 NON ARR
168400000000     C                   CLEAR                   DISA                           DISGUIDI
168500000000     C                   CLEAR                   PARA                           PARTITI NON SPU
168600000000     C                   CLEAR                   F35A                           FINE  35 Q.LI
168700000000     C                   CLEAR                   O35A                           OLTRE 35 Q.LI
168800000000     C                   CLEAR                   PTAA                           PESO TASSABILE
168900000000     C                   CLEAR                   VOLA                           VOLUME
169000090929     C                   CLEAR                   FNA                            FRANCO C+D
169100090929     C                   CLEAR                   FEA                            FRANCO "E"
169200090929     C                   CLEAR                   FHA                            FRANCO "H"
169300090929     C                   CLEAR                   ANA                            ASSEGN C+D
169400090929     C                   CLEAR                   AEA                            ASSEGN "E"
169500090929     C                   CLEAR                   AHA                            ASSEGN "H"
169501180209     C                   CLEAR                   CIFA                           incomp.Foglio
169502180209     C                   CLEAR                   CISA                           incomp.Spunte
169503180209     C                   CLEAR                   SDAA                           Sped "D" giorno "A"
169504180209     C                   CLEAR                   SDBA                           Sped "D" giorno "B"
169505180209     C                   CLEAR                   CDAA                           colli "D" giorno "A"
169506180209     C                   CLEAR                   CDBA                           colli "D" giorno "B"
169600000000      *
169700000000     C                   CLEAR                   TFPT                           TERMINAL PARTEN
169800000000     C                   CLEAR                   CLLT                           TOT COLLI
169900000000     C                   CLEAR                   P88T                           PIST.88 NON ARR
170000000000     C                   CLEAR                   PART                           PARTITI NON SPU
170100000000     C                   CLEAR                   F35T                           FINE  35 Q.LI
170200000000     C                   CLEAR                   O35T                           OLTRE 35 Q.LI
170300000000     C                   CLEAR                   PTAT                           PESO TASSABILE
170400000000     C                   CLEAR                   VOLT                           VOLUME
170500090929     C                   CLEAR                   FNT                            FRANCO C+D
170600090929     C                   CLEAR                   FET                            FRANCO "E"
170700090929     C                   CLEAR                   FHT                            FRANCO "H"
170800090929     C                   CLEAR                   ANT                            ASSEGN C+D
170900090929     C                   CLEAR                   AET                            ASSEGN "E"
171000090929     C                   CLEAR                   AHT                            ASSEGN "H"
171100000000      *
171200000000     C                   CLEAR                   LNAR                           LINEA ARRIVO
171300000000     C                   CLEAR                   CLLR                           TOT COLLI
171400000000     C                   CLEAR                   P88R                           PIST.88 NON ARR
171500000000     C                   CLEAR                   PARR                           PARTITI NON SPU
171600000704     C                   CLEAR                   $X
171700000000      *
171800000000     C                   CLEAR                   CLLD                           TOT COLLI
171900000000     C                   CLEAR                   P88D                           PIST.88 NON ARR
172000000000     C                   CLEAR                   PARD                           PARTITI NON SPU
172100000000     C                   CLEAR                   F35D                           FINE  35 Q.LI
172200000000     C                   CLEAR                   O35D                           OLTRE 35 Q.LI
172300000000     C                   CLEAR                   PTAD                           PESO TASSABILE
172400000000     C                   CLEAR                   VOLD                           VOLUME
172500090929     C                   CLEAR                   FND                            FRANCO C+D
172600090929     C                   CLEAR                   FED                            FRANCO "E"
172700090929     C                   CLEAR                   FHD                            FRANCO "H"
172800090929     C                   CLEAR                   AND                            ASSEGN C+D
172900090929     C                   CLEAR                   AED                            ASSEGN "E"
173000090929     C                   CLEAR                   AHD                            ASSEGN "H"
173100000000      *
173200000000     C                   ENDSR
173300000000      **************************************************************************
173400000000      * Verifico i colli non spuntati in partenza e non arrivati, identificabili
173500000000      * con cod.pistola 88.
173600000000      **************************************************************************
173700000000     C     NONARR        BEGSR
173800000000      *
173900000000     C                   Z-ADD     *ZERO         $B                             CONTATORE TFP
174000000000     C                   Z-ADD     *ZERO         $X                             CONTATORE LNAR
174100000000     C                   Z-ADD     *ZERO         $TFAPR                         PRECEDENTE
174200000000     C                   Z-ADD     *ZERO         $PRIMO                         PRIMO GIRO
174300040128     c* primo giro: elaboro tutte le linee da elaborare --> schiera lel
174400040128     c                   movea     lel           lela
174500040128     c                   clear                   wsololna          1
174600180208    1c                   do        2
174700000000      *
174800040128      * Leggo tutti i fogli arrivi del giorno
174900000000      * Il ciclo di lettura deve essere ripetuto 2 volte (una con tipo 2 ed una
175000180208      * con tipo 6) - non più non esistono più fogli 6
175100000000     C                   Z-ADD     2             WNPG                           2=ARRIVI
175200180208    2C*****              DO        2
175300040128      * devo ciclare per tutti i p.o. da elaborare
175400020918     c                   Clear                   xx
175500040129    3c                   do        60            xx
175600040129    4c                   If        lela(xx) = *Zeros
175700020912     c                   Leave
175800040129    4c                   EndIf
175900040129    4c     wsololna      ifeq      *blanks
176000040129     c     lela(xx)      lookup    l1                                     99
176100040205     c                   else
176200040206     c     xx            occur     dslel3
176300040129    4c                   endif
176400040129     c* Devo leggere i fogli abbinati dalla linea solo se sono nel secondo
176500040129     c* giro (elaborazione fogli di lel2) oppure se sono nel primo giro
176600040129     c* purchè la linea da elaborare sia in £1
176700040129     c* Es.: se sto elaborando per 005,nella schiera lel c'è anche 050 che
176800040129     c* è un po terminal e un pò no ma in questo caso non mi serve andare
176900040129     c* a leggere le spunte dei f.v.p. abbinati da 50 perchè tali dati
177000040129     c* dovranno uscire nelle stampe per terminal di 050 e non di 005
177100040129    4c     wsololna      ifeq      *blanks
177200040129     c     *in99         andeq     *on
177300040129     c     wsololna      orne      *blanks
177400950214     C                   Z-ADD     DS1ADA        WDFV                           DATA INIZIALE
177500020912     C**!!!              Z-ADD     SIMFEL        WFLE                           FIL.ELABORATORE
177600040128     c                   Z-Add     lela(xx)      Wfgs
177700020912     C     FNFVVK        SETLL     FNFVV02L
177800040129    5C                   DO        *HIVAL
177900020912     C     FNFVVK        READE     FNFVV02L                               31
178000040129    6C     *IN31         IFEQ      *OFF
178100000000      *
178200000000      * Avendo il f.v. arrivi leggo tutti i f.v. partenze abbinati
178300000000     C                   Z-ADD     FVVNPG        FVANPG                         CATEG.F.V.ARR.
178400000000     C                   Z-ADD     FVVNFV        FVANFA                         NR.F.VIAGGIO
178500020917     c                   Z-Add     FvvFgs        FvaLai
178600040128     c*
178700040128     c*
178800941229     C     FNFVAK        SETLL     FNFVA02L
178900040129     c                   If        %Equal(Fnfva02l)
179000040129     c                   clear                   trul09ds
179100100630     c                   eval      i09mod = 'DEF'
179200040129     c                   eval      i09tfa = fvalai
179300040129     c                   eval      i09dta = ds1ada
179400040129     c                   call      'TRUL09R'
179500040129     c                   parm                    trul09ds
179600040129     c                   eval      SFU = t09
179700040129     c*
179800040129     c                   clear                   trul09ds
179900040129     c                   eval      i09mod = 'TFA'
180000040129     c                   eval      i09tfa = fvalai
180100040129     c                   eval      i09dta = ds1ada
180200040129     c                   call      'TRUL09R'
180300040129     c                   parm                    trul09ds
180400040129     c                   eval      SFA = t09
180500040129     c                   endif
180600040129    7C                   DO        *HIVAL
180700941229     C     FNFVAK        READE     FNFVA02L                               32
180800040129    8C  N32              DO
180900000000      *
181000970606     C* Determino se è un foglio di defluenza
181100970606     C                   MOVE      'N'           WDEFL             1
181200040129    9C     FVATTR        IFNE      *BLANKS
181300970606     C     FVATTR        LOOKUP    TTR                                    99
181400970606     C   99              MOVE      'S'           WDEFL
181500040129    9C                   END
181600000504     C* FILE DI ESTENSIONE
181700000504     C     KFWA          CHAIN     FNFWA01L                           39
181800040129    9C     *IN39         IFEQ      *ON
181900000504     C     FWAATB        ORNE      ' '
182000000504     C                   CLEAR                   FWAFF3
182100000504     C                   CLEAR                   FWAFF4
182200000504     C                   CLEAR                   FWAFL3
182300000504     C                   CLEAR                   FWAFL4
182400040129    9c                   ENDIF
182500070117      * Devo elaborare FNBRV02L solo con quel f.v. per quella linea di scambio
182600000000     C                   Z-ADD     1             $E                3 0
182700040129    9C     FFV($E)       DOWGT     *ZERO
182800000000     C                   MOVE      FVALAI        $COMO1            3            LNA CUI INVIO
182900040205     c     wsololna      ifne      *blanks
183000040205     c     ffv($e)       lookup    lel3                                   98
183100040205     c                   endif
183200040129     c* se sono nel primo giro elaboro la linea del foglio se
183300040129     c* lai è la linea del foglio o il p.o. di scarico
183400040129   10c     wsololna      ifeq      *blanks
183500040129     C     FSP($E)       andeq     $COMO1                                       FIL.SCARICO
183600040129     c     wsololna      oreq      *blanks
183700040129     C     FFV($E)       andeq     $COMO1                                       LINEA ARRIVO
183800040129     c* se sono nel secondo giro elaboro la linea del foglio solo se
183900040129     c* la stessa è il secondo liv. in arrivo di lai presente nella
184000040129     c* schiera delle linee da elaborare
184100040129     c* (nel caso di 018 devo leggere i fogli abbinati da 001 ed elabo-
184200040129     c* rare solo la linea 018)
184300040129     c     wsololna      orne      *blanks
184400040205     c     *in98         andeq     *on
184500000000      *
184600960926      * Cerco  il terminal di arrivo
184700000000     C                   Z-ADD     FVALNP        $LINEP            3 0          LINEA PARTENZA
184800000000     C                   MOVE      FFV($E)       $LINEA            3 0          LINEA ARRIVO
184900000000     C                   EXSR      TROTFA                                       TROVA TFA
185000000000      *
185100000000      * Memorizzo il TFA e la LNA perche' mi servono dopo
185200000000     C                   Z-ADD     $LINEA        $TERAR            3 0          TERMINAL ARRIVO
185300000000     C                   MOVE      FFV($E)       $LNARR            3 0          LINEA ARRIVO
185400000000      *
185500000000      * Se ne TFA ($TERAR) ne LNA ($LINEA) non sono presenti in tab. '£1'
185600000000      * si tratta di tansito; quindi ignoro TFA e sostituisco con SIMFEL
185700000000     C                   MOVE      *BLANKS       $SOLO             1            NO SOLO TERMIN.
185800000000     C                   CLEAR                   $TRANS
185900040128     C***  $TERAR        LOOKUP    L1                                     99     ON=TROVATO
186000040128     C**N99$LNARR        LOOKUP    L1                                     99     ON=TROVATO
186100040128     C**N99              Z-ADD     SIMFEL        $TERAR                         E' TRANSITO
186200040128     C**** *IN99         IFEQ      *OFF
186300040130   11c     $lnarr        ifne      fvalai
186400040130   12c     $terar        ifne      fvalai
186500040128     C***                Z-ADD     SIMFEL        $TERAR                         E' TRANSITO
186600040130     C                   move      fsp($E)       $TERAR                         E' TRANSITO
186700000000     C                   MOVE      'T'           $TRANS                         SI TRANSITO
186800000000      *
186900000000      * Determino se LNA ($LINEA) e' SOLO terminal di arrivo
187000040129  x12C                   ELSE                                                   NO TRANSITO
187100040128     C***  $LNARR        LOOKUP    L1                                     99     ON=TROVATO
187200040129     C     $LNARR        LOOKUP    SFU                                    99     ON=TROVATO
187300000000     C  N99              MOVE      'R'           $SOLO                          SOLO TERMINAL A
187400040129   12c                   endif
187500040129   11C                   ENDIF
187600000000      *
187700000000      * Con il f.v. partenze leggo tutte le spunte
187800941229     C                   Z-ADD     1             SPPNPG                         CATEGORIA
187900941229     C                   Z-ADD     FVALNP        SPPFGS                         LINEA PARTENZA
188000000000     C                   Z-ADD     FVANFV        SPPNFV                         NR.F.VIAGGIO
188100000000     C                   MOVE      FFV($E)       SPPLNA                         LINEA ARRIVO
188200070117     C     FNSPPK        SETLL     FNBRV02L
188300040129   11C                   DO        *HIVAL
188400070117     C     FNSPPK        READE     FNBRV02L                               33
188500040129   12C     *IN33         IFEQ      *OFF
188600000000      *
188700000000      *-------------------------------------------------------------------------
188800000000      *    ?  CONTROLLO I COLLI PARTITI MA NON SPUNTATI IN ARRIVO  ?
188900940607      *
189000940607      * escludo i fogli illuminati perche sono del terminal
189100040129   13C     FVAATR        IFNE      'T'
189200970513     C**
189300970606      * escludo le defluenze
189400970606     C     WDEFL         ANDEQ     'N'
189500000000     C                   EXSR      PARARR                                       PARTITI NON SPU
189600040129   13C                   ENDIF
189700000000      *-------------------------------------------------------------------------
189800000000      *
189900980402      * Proseguo solo se la spunta e' FASULLA (88 89 98 96)
190000980402      * IGNORO SE ESISTE UN'ALTRA SPUNTA CON DATA > VISTO CHE L'HO IGNO
190100980402      *  RATA ANCHE NEL CALCOLO DEI PARTITI NOPN SPUNTATI IN ARRIVO
190200980402     C     SPPNPS        LOOKUP    NPF                                    38
190300040129   13C     *IN38         IFEQ      *ON
190400980402     C     WIGNOR        ANDEQ     ' '
190500000000     C                   MOVE      'S'           $OKSN             1            SI  CONTEGGIO
190600000000      *
190700000000      * leggo il segnacollo
190800000000     C                   Z-ADD     SPPLNP        ARTFLS                         FIL.SEGNACOLLLO
190900000000     C                   Z-ADD     SPPLNA        ARTLNA                         FIL.ARRIVO
191000000000     C                   Z-ADD     SPPNRS        ARTNRS                         NR.SERIE SPED.
191100941230     C                   Z-ADD     SPPNSC        ARTNSC                         NR.SEGNACOLLO
191200040129   14C     $TRANS        IFNE      'T'                                          NO TRANSITO
191300000000      *
191400941229      * Se non e' transito leggo FNART e controllo la data arrivi (ARTDAM)
191500941229     C     FNARTK        CHAIN     FNART27L                           30
191600040129   15C  N30ARTDAM        IFGT      *ZEROS
191700970502     C     ARTDAM        ANDLE     DS1ADA                                       DATA ARRIVO
191800000000     C                   MOVE      'N'           $OKSN                          NO  CONTEGGIO
191900040129   15C                   ENDIF
192000000000      *
192100040129  x14C                   ELSE
192200941229      * Se e' transito leggo FNBTT e controllo la data entrata al trans.(BTTDET)
192300020823     C     FNARTK1       CHAIN     FNBTT37L                           30
192400040129   15C  N30BTTDET        IFGT      0
192500970502     C     BTTDET        ANDLE     DS1ADA                                       DATA ARRIVO
192600000000     C                   MOVE      'N'           $OKSN                          NO  CONTEGGIO
192700040129   15C                   ENDIF
192800040129   14C                   ENDIF
192900000000      *
193000970502      * SE non ho trovato la BOLLA
193100040129   14C     *IN30         IFEQ      *ON
193200000000      *
193300000000      * Se trovo almeno una spunta devo scartare dalle statistiche
193400000000     C                   Z-ADD     SPPLNP        BRVLNP                         FIL.SEGNACOLLO
193500000000     C                   Z-ADD     SPPLNA        BRVLNA                         FIL.ARRIVO
193600000000     C                   Z-ADD     SPPNRS        BRVNRS                         NR.SERIE SPED.
193700000000     C                   Z-ADD     SPPNSC        BRVNSC                         NR.SEGNACOLLO
193800000000      *
193900070117     C     KBRV          CHAIN     FNBRV07L                           30
194000040129   15C     *IN30         DOWEQ     *OFF
194100970502     C     $OKSN         ANDEQ     'S'
194200040128     c* se p.o. gestione spunta è un p.o. dell'area di arrivo del p.o. che
194300040128     c* sto elaborando --> la spunta è da elaborare altrimenti devo continua
194400040128     c* re a leggere
194500040129     c     $trans        ifeq      'T'
194600040130     c     $terar        oreq      fvalai
194700040129     c     brvfgs        lookup    SFA                                    30
194800040129     c                   else
194900040129     c                   clear                   trul09ds
195000040129     c                   eval      i09mod = 'TFA'
195100040129     c                   eval      i09tfa = $terar
195200040129     c                   eval      i09dta = ds1ada
195300040129     c                   call      'TRUL09R'
195400040129     c                   parm                    trul09ds
195500040129     c     brvfgs        lookup    t09                                    30
195600040129     c                   endif
195700970502     C*
195800040129   16c     *in30         ifeq      *on
195900040203     c* se spunta partenza ignoro se fgs non è in £1
196000040203     c     brvnpg        ifeq      1
196100040203     c     brvfgs        lookup    l1                                     30
196200040203     c                   endif
196300040203  16ac     brvnpg        ifne      1
196400040203     c     *in30         oreq      *on
196500970502     C                   CLEAR                   DSLV53
196600000204     C                   Z-ADD     BRVNFV        D53NFV
196700970502     C                   MOVEL     SIMFEL        D53FEL
196800970502     C                   MOVEL     BRVNPG        D53NPG
196900970502     C                   MOVEL     BRVFGS        D53FGS
197000970502     C                   CALL      'FNLV53R'
197100970502     C                   PARM                    DSLV53
197200970502     C*
197300970502     C* SE NON C'E' ERRORE E DATA <= DATA ELABORAZIONE --> E' OK
197400970502     C*   ESISTE LA SPUNTA
197500040129   17C     D53ERR        IFEQ      ' '
197600970502     C     D53DFV        ANDLE     DS1ADA
197700000000     C                   MOVE      'N'           $OKSN                          NO CONTEGGIO
197800040129  x17C                   ELSE
197900070117     C     KBRV          READE     FNBRV07L                               30
198000040129   17C                   ENDIF
198100040203 x16ac                   else
198200070117     C     KBRV          READE     FNBRV07L                               30
198300040203  16ac                   endif
198400040129  x16c                   else
198500070117     C     KBRV          READE     FNBRV07L                               30
198600040129   16c                   endif
198700040129   15C                   ENDDO
198800000000      *
198900040129   14C                   ENDIF                                                  30 ON
199000000000      *
199100000000      * Se SI CONTEGGI ($OKSN='S')  aggiorno la statistica             ' errore
199200040129   14C     $OKSN         IFEQ      'S'
199300000000      *
199400000000      * Come per le bolle rompo quando cambia terminal di arrivo
199500040129   15C     $TERAR        IFNE      $TFAPR                                       PRECEDENTE
199600000000     C     $PRIMO        ANDNE     *ZERO                                        NO PRIMO GIRO
199700000000     C                   EXSR      SCASCH                                       SCARICA SCHIERE
199800000000     C                   EXSR      SCASCT                                       SCARICA SCHIERE
199900000000     C                   EXSR      SCASCR                                       SCARICA SCHIERE
200000000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
200100980708     C                   Z-ADD     0             $B
200200000000     C                   Z-ADD     $TERAR        $TFAPR                         PRECEDENTE
200300040129   15C                   ENDIF
200400000000      *
200500000000      * Cerco l'indice di schiera per il terminal di partenza
200600000000     C                   Z-ADD     1             D
200700000000     C     SPPLNP        LOOKUP    ORG(D)                                 99     ON=TROVATO
200800000000     C   99              Z-ADD     OTP(D)        $LINEA
200900000000     C  N99              MOVEL     SPPLNP        $LINEA
201000000000     C                   EXSR      INDTFP                                       INDICE TFP (B)
201100000000      *
201200040129   15C     $TRANS        IFEQ      'T'                                          'T'=TRANSITO
201300000000     C                   ADD       1             P88T(B)                        TFPT
201400000000     C                   ADD       1             P88D              7 0
201500040129  x15C                   ELSE
201600000000      *
201700040129   16C     $SOLO         IFEQ      'R'                                          SOLO TERMINAL
201800000000     C                   Z-ADD     $LNARR        $LINEA                         LINEA ARRIVO
201900000000     C                   EXSR      INDSOL                                       INDICE SCHIERA
202000000000     C                   ADD       1             P88R(X)                        LNA
202100040129  x16C                   ELSE                                                   NO SOLO TERMIN.
202200000000      *
202300000000      * Cerco l'indice di schiera per la linea di arrivo
202400000000     C                   Z-ADD     SPPLNA        $LINEA
202500000000     C                   EXSR      INDLNA                                       INDICE LNA
202600000000     C                   ADD       1             P88(B)                         TFP
202700000000     C                   ADD       1             P88A(A)                        LNA
202800040129   16C                   ENDIF
202900000000      *
203000040129   15C                   ENDIF
203100000000      *
203200000000      * Solo al primo giro imposto TFA precedente
203300040129   15C     $PRIMO        IFEQ      *ZERO                                        0=PRIMO GIRO
203400000000     C                   Z-ADD     $TERAR        $TFAPR                         PRECEDENTE
203500000000     C                   Z-ADD     1             $PRIMO                         1=GIA' PASSATO
203600000000     C                   Z-ADD     1             $DTAAR
203700040129   15C                   ENDIF
203800040129   14C                   ENDIF                                                  $OKSN='S'
203900000000      *
204000040129   13C                   ENDIF                                                  PIST 88
204100000000      *
204200000000      * solo terminal
204300000000      *
204400040129   13C     $SOLO         IFEQ      'R'                                          SOLO TERMINAL
204500000000      *
204600000000      * Se trovo almeno una spunta devo incrementare il nr. colli
204700000000     C                   Z-ADD     SPPLNP        BRVLNP                         FIL.SEGNACOLLO
204800000000     C                   Z-ADD     SPPLNA        BRVLNA                         FIL.ARRIVO
204900000000     C                   Z-ADD     SPPNRS        BRVNRS                         NR.SERIE SPED.
205000000000     C                   Z-ADD     SPPNSC        BRVNSC                         NR.SEGNACOLLO
205100970502     C*
205200070117     C     KBRV          CHAIN     FNBRV07L                           34
205300040129   15C     *IN34         DOWEQ     *OFF
205400040129     c     brvfgs        lookup    SFA                                    34
205500040129   16c     *in34         ifeq      *on
205600040203     c* se spunta partenza ignoro se fgs non è in £1
205700040203     c     brvnpg        ifeq      1
205800040203     c     brvfgs        lookup    l1                                     34
205900040203     c                   endif
206000040203  16ac     brvnpg        ifne      1
206100040203     c     *in34         oreq      *on
206200970502     C                   CLEAR                   DSLV53
206300000204     C                   Z-ADD     BRVNFV        D53NFV
206400970502     C                   MOVEL     SIMFEL        D53FEL
206500970502     C                   MOVEL     BRVNPG        D53NPG
206600970502     C                   MOVEL     BRVFGS        D53FGS
206700970502     C                   CALL      'FNLV53R'
206800970502     C                   PARM                    DSLV53
206900970502     C*
207000970502     C* SE NON C'E' ERRORE E DATA = DATA ELABORAZIONE --> E' OK
207100970502     C*   ESISTE LA SPUNTA
207200040129   17C     D53ERR        IFEQ      ' '
207300970502     C     D53DFV        ANDEQ     DS1ADA
207400000000     C                   SETON                                        34        C'E' COLLO
207500970502      * Se trovata spunta incremento il numero dei colli
207600970502     C                   Z-ADD     $LNARR        $LINEA                         LINEA ARRIVO
207700970502     C                   EXSR      INDSOL                                       INDICE SCHIERA
207800970502     C                   ADD       1             CLLR(X)                        NR.COLLI
207900970502     C*
208000040129  x17C                   ELSE
208100070117     C     KBRV          READE     FNBRV07L                               34
208200040129   17C                   ENDIF
208300040203 x16ac                   else
208400070117     C     KBRV          READE     FNBRV07L                               34
208500040203  16ac                   endif
208600040129  x16c                   else
208700070117     C     KBRV          READE     FNBRV07L                               34
208800040130   16c                   endif
208900040130   15C                   ENDDO
209000000000      *
209100040129   13C                   ENDIF                                                  *IN33=OFF
209200040130   12C                   ENDIF                                                  FSP,$E=$COMO1
209300040130   11C  N33              ENDDO                                                  *HIVAL
209400000000      *
209500040129   10c                   endif
209600000000     C                   ADD       1             $E
209700040129    9C                   ENDDO                                                  DOW ... $E
209800000000      *
209900040129    8C                   ENDDO
210000040129    7C  N32              ENDDO                                                  *HIVAL
210100000000      *
210200040129    6C                   ENDIF
210300040129    5C  N31              ENDDO                                                  *HIVAL
210400020912
210500040129    4c                   endif
210600040129    3c                   EndDo                                                  L1
210700000000      *
210800180208     C***                Z-ADD     6             WNPG
210900180208    2C***                ENDDO                                                  ... 1 DO 2 ...
211000040128     c                   movea     lel2          lela
211100040128     c                   move      'S'           wsololna
211200040129    1C                   ENDDO                                                  ... 1 DO 2 ...
211300000000      *
211400000000      * Alla fine scarico le ultime schiere ancora in mermoria
211500000000     C                   EXSR      SCASCH                                       SCARICA SCHIERE
211600000000     C                   EXSR      SCASCT                                       SCARICA SCHIERE
211700000000     C                   EXSR      SCASCR                                       SCARICA SCHIERE
211800000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
211900000000      *
212000000000     C                   ENDSR
212100000000      **************************************************************************
212200000000      * Controllo i colli partiti ma non spuntati al terminal di arrivo
212300000000      **************************************************************************
212400000000     C     PARARR        BEGSR
212500000000      *
212600000000      * Cerco fra le spunte partenze, se lo trovo passo oltre
212700970502     C                   MOVEL     'N'           WSPU              1
212800970513     C                   CLEAR                   WIGNOR
212900941229     C                   Z-ADD     1             BRVNPG                         1=PARTENZE
213000941229     C                   Z-ADD     SPPLNP        BRVLNP                         LINEA PARTENZA
213100941229     C                   Z-ADD     SPPLNA        BRVLNA                         LINEA ARRIVO
213200941229     C                   Z-ADD     SPPNRS        BRVNRS                         NR.SERIE SEGNAC
213300941229     C                   Z-ADD     SPPNSC        BRVNSC                         NR.SEGNACOLLO
213400970513     C*
213500970513     C* SE TROVO UN'ALTRA SPUNTA PARTENZA CON DATA > IGNORO QUESTA
213600070117     C     KBRV9         CHAIN     FNBRV09L                           99
213700970513     C*
213800000000     C* E' STATA SPARATA SOLO SE DATA FOGLIO <= A DATA ELABORAZIONE
213900970502     C* ESCLUDO SE SI TRATTA DI SPUNTA LOCALE CONSIDERO SOLO LA DEFLUZ
214000970502    1C     *IN99         DOWEQ     *OFF
214100970513     C**
214200970513     C** NON DEVE ESSERE LA SPUNTA CHE STO GIA' LEGGENDO
214300970513    2C     BRVNFV        IFNE      SPPNFV
214400970513     C     BRVFGS        ORNE      SPPFGS
214500970513     C**
214600970502     C                   CLEAR                   DSLV53
214700000204     C                   Z-ADD     BRVNFV        D53NFV
214800970513     C                   MOVEL     BRVFGS        D53FGS
214900970513     C                   MOVEL     BRVNPG        D53NPG
215000970513     C                   MOVEL     SIMFEL        D53FEL
215100970502     C                   CALL      'FNLV53R'
215200970502     C                   PARM                    DSLV53
215300970502     C*
215400970513    3C     D53ERR        IFEQ      ' '
215500970513     C**
215600970513     C* NON C'E' ERRORE E FOGLIO DI DEFLUENZA: LA DATA DEVE ESSERE
215700970513     C*  <= ALLA DATA ELABORAZIONE
215800040302    4C***  SIMFEL        IFEQ      BRVFLE
215900061120     c***  $terar        ifeq      brvfle
216000061120     c     $terar        ifeq      brvfgs
216100980402     C**
216200980402     C* VERIFICO CHE NON SI TRATTI DI PISTOLA AUTOGENERATA
216300980402    5C     BRVNPS        LOOKUP    NPS                                    98
216400980402     C     *IN98         IFEQ      *OFF
216500970513     C*
216600980402    6C     $TRANS        IFNE      'T'
216700980402    7C     D53DEF        IFEQ      'S'
216800970502     C     D53DFV        ANDLE     DS1ADA
216900970502     C                   MOVEL     'S'           WSPU
217000970502     C                   SETON                                        99
217100980402    7C                   ENDIF
217200980402     C**
217300980402   X6C                   ELSE
217400980402     C                   MOVEL     'S'           WSPU
217500980402     C                   SETON                                        99
217600980402    6C                   ENDIF
217700980402    5C                   ENDIF
217800970513     C**
217900970513   X4C                   ELSE
218000970513     C* IGNORO SE SI C'E' UN'ALTRA SPUNTA CON DATA >
218100970513    5C     D53DFV        IFGT      FVADFV
218200970513     C                   MOVEL     'S'           WIGNOR            1
218300970513     C                   SETON                                        99
218400970513    5C                   ENDIF
218500970513    4C                   ENDIF
218600970513    3C                   ENDIF
218700970513    2C                   ENDIF
218800970513     C**
218900070117     C  N99KBRV9         READE     FNBRV09L                               99
219000970502    1C                   ENDDO
219100970513     C**
219200970513     C* SPUNTA TROVATA  O DA IGNORARE
219300970502    1C     WSPU          IFEQ      'S'
219400970513    1C     WIGNOR        OREQ      'S'
219500970502     C                   GOTO      ENDPAR
219600970502    1C                   ENDIF
219700000000      *
219800000000      * Cerco fra le spunte fogli vari, con categoria 2. Se non lo trovo passo
219900000000     C                   Z-ADD     2             BRVNPG                         2=ARRIVI 1° LVL
220000070117     C     FNBRVK        CHAIN     FNBRV09L                           99
220100000000     C* E' STATA SPARATA SOLO SE DATA FOGLIO = A DATA ELABORAZIONE
220200970502    1C     *IN99         DOWEQ     *OFF
220300040129     c     sfa(1)        ifgt      *zeros
220400040129     c     brvfgs        lookup    sfa                                    99
220500040129     c                   else
220600040129     c* per i diretti ai secondi liv in arrivo
220700040129     c     brvfgs        comp      $lnarr                                 99
220800040129     c                   endif
220900040129     c     *in99         ifeq      *on
221000970502     C                   CLEAR                   DSLV53
221100000204     C                   Z-ADD     BRVNFV        D53NFV
221200970502     C                   MOVEL     BRVNPG        D53NPG
221300970502     C                   MOVEL     BRVFGS        D53FGS
221400970502     C                   MOVEL     SIMFEL        D53FEL
221500970502     C                   MOVEL     'V'           D53TFO
221600970502     C                   CALL      'FNLV53R'
221700970502     C                   PARM                    DSLV53
221800970502     C*
221900970502     C* NON C'E' ERRORE E FOGLIO DI DEFLUENZA: LA DATA DEVE ESSERE
222000970502     C*  <= ALLA DATA ELABORAZIONE
222100970502    2C     D53ERR        IFEQ      ' '
222200970502     C     D53DFV        ANDLE     DS1ADA
222300970502     C                   MOVEL     'S'           WSPU
222400970502     C                   SETON                                        99
222500970502   X2C                   ELSE
222600070117     C     FNBRVK        READE     FNBRV09L                               99
222700970502    2C                   ENDIF
222800040129     c                   else
222900070117     C     FNBRVK        READE     FNBRV09L                               99
223000040129     c                   endif
223100970502    1C                   ENDDO
223200970502     C* SPUNTA TROVATA
223300970502    1C     WSPU          IFEQ      'S'
223400970502     C                   GOTO      ENDPAR
223500970502    1C                   ENDIF
223600000000      *
223700070117      * Se la filiale di scarico (FSP,B) e' di 2° livello cerco (in FNBRV)
223800000000      * anche con categoria 6 (arrivi 2° livelli)
223900040129     C***                MOVE      FSP($E)       $COMO2            3 0
224000040129     C***  $COMO2        LOOKUP    L1                                     99     ON=TROVATO
224100940607     c* oppure se linea di arrivo e' un 2 livello della L1
224200040129     C***                MOVE      FFV($E)       $COMO3            3 0
224300040129     C***  $COMO3        LOOKUP    L1                                     98     ON=TROVATO
224400940607     C*
224500040129    1C***  *IN99         IFEQ      *ON
224600040129     C***  $COMO2        ANDNE     SIMFEL
224700940607     C*
224800040129     C***  *IN98         OREQ      *ON
224900040129     C***  $COMO3        ANDNE     SIMFEL
225000940607     C*
225100000000     C                   Z-ADD     6             BRVNPG                         6=ARRIVI 2° LVL
225200070117     C     FNBRVK        CHAIN     FNBRV09L                           99
225300970502     C* E' STATA SPARATA SOLO SE DATA FOGLIO <= A DATA ELABORAZIONE
225400970502    2C     *IN99         DOWEQ     *OFF
225500040129    3c     sfa(1)        ifgt      *zeros
225600040129     c     brvfgs        lookup    sfa                                    99
225700040129     c                   else
225800040129     c* per i diretti ai secondi liv in arrivo
225900040129     c     brvfgs        comp      $lnarr                                 99
226000040129    3c                   endif
226100040129    3c     *in99         ifeq      *on
226200970502     C                   CLEAR                   DSLV53
226300000204     C                   Z-ADD     BRVNFV        D53NFV
226400970502     C                   MOVEL     BRVNPG        D53NPG
226500970502     C                   MOVEL     BRVFGS        D53FGS
226600970502     C                   MOVEL     SIMFEL        D53FEL
226700970502     C                   MOVEL     'V'           D53TFO
226800970502     C                   CALL      'FNLV53R'
226900970502     C                   PARM                    DSLV53
227000970502     C*
227100970502     C* NON C'E' ERRORE E FOGLIO DI DEFLUENZA: LA DATA DEVE ESSERE
227200970502     C*  <= ALLA DATA ELABORAZIONE
227300970502    3C     D53ERR        IFEQ      ' '
227400970502     C     D53DFV        ANDLE     DS1ADA
227500970502     C                   MOVEL     'S'           WSPU
227600970502     C                   SETON                                        99
227700970502   X3C                   ELSE
227800070117     C     FNBRVK        READE     FNBRV09L                               99
227900970502    3C                   ENDIF
228000040129   x3c                   else
228100070117     C     FNBRVK        READE     FNBRV09L                               99
228200040129     c                   endif
228300970502    2C                   ENDDO
228400970502     C* SPUNTA TROVATA
228500970502    2C     WSPU          IFEQ      'S'
228600970502     C                   GOTO      ENDPAR
228700970502    2C                   ENDIF
228800040129    1C****               ENDIF
228900000000      *
229000000000      * Come per le bolle rompo quando cambia terminal di arrivo
229100000000     C     $TERAR        IFNE      $TFAPR                                       PRECEDENTE
229200000000     C     $PRIMO        ANDNE     *ZERO                                        NO PRIMO GIRO
229300000000     C                   EXSR      SCASCH                                       SCARICA SCHIERE
229400000000     C                   EXSR      SCASCT                                       SCARICA SCHIERE
229500000000     C                   EXSR      SCASCR                                       SCARICA SCHIERE
229600000000     C                   EXSR      AZZSCH                                       AZZERA SCHIERE
229700980708     C                   Z-ADD     0             $B
229800000000     C                   Z-ADD     $TERAR        $TFAPR                         PRECEDENTE
229900000000     C                   ENDIF
230000000000      *
230100000000      * Cerco l'indice di schiera per il terminal di partenza
230200000000     C                   Z-ADD     1             D
230300000000     C     SPPLNP        LOOKUP    ORG(D)                                 99     ON=TROVATO
230400000000     C   99              Z-ADD     OTP(D)        $LINEA
230500000000     C  N99              Z-ADD     SPPLNP        $LINEA
230600000000     C                   EXSR      INDTFP                                       INDICE TFP (B)
230700000000      *
230800000000     C     $TRANS        IFEQ      'T'                                          'T'=TRANSITO
230900000000     C                   ADD       1             PART(B)                         TFPT
231000000000     C                   ADD       1             PARD              7 0
231100000000     C                   ELSE
231200000000      *
231300000000     C     $SOLO         IFEQ      'R'                                          SOLO TERMINAL
231400000000     C                   Z-ADD     $LNARR        $LINEA                         LINEA ARRIVO
231500000000     C                   EXSR      INDSOL                                       INDICE SCHIERA
231600000000     C                   ADD       1             PARR(X)                        LNA
231700000000     C                   ELSE                                                   NO SOLO TERMIN.
231800940818      *
231900940818      * Cerco l'indice di schiera per la linea di arrivo
232000940818     C                   Z-ADD     SPPLNA        $LINEA
232100940818     C                   EXSR      INDLNA                                       INDICE LNA (A)
232200940818      *
232300000000     C                   ADD       1             PAR(B)                          TFP
232400000000     C                   ADD       1             PARA(A)                         LNA
232500000000     C                   ENDIF
232600000000      *
232700000000     C                   ENDIF
232800000000      *
232900000000      * Solo al primo giro imposto TFA precedente
233000000000     C     $PRIMO        IFEQ      *ZERO                                        0=PRIMO GIRO
233100000000     C                   Z-ADD     $TERAR        $TFAPR                         PRECEDENTE
233200000000     C                   Z-ADD     1             $PRIMO                         1=GIA' PASSATO
233300000000     C                   Z-ADD     1             $DTAAR
233400000000     C                   ENDIF
233500000000      *
233600000000     C     ENDPAR        ENDSR
233700000000      **************************************************************************
233800000000      * La linea passata ($LINEA) viene cercata fra i terminal di partenza
233900000000      * (schiera TFP); se non trovato viene immesso come nuovo terminal.
234000000000      * Infine viene restituito l'indice di schiera (B) ove si trova l'elemento.
234100000000      *
234200000000      * Ricordo che $B e' il contatore della schiera TFP.
234300000000      **************************************************************************
234400000000     C     INDTFP        BEGSR
234500000000      *
234600000000      * Verifico se il TFP appena letto e' nuovo o no
234700000000     C                   Z-ADD     1             B                 3 0
234800000000     C     $TRANS        IFEQ      'T'                                          'T'=TRANSITI
234900000000     C     $LINEA        LOOKUP    TFPT(B)                                99     TERMINAL PART.
235000000000     C  N99              ADD       1             $B                             CONTATORE TFP
235100000000     C  N99              Z-ADD     $B            B
235200000000     C  N99              Z-ADD     $LINEA        TFPT(B)                        NUOVA TFP
235300000000     C                   ELSE
235400000000     C     $LINEA        LOOKUP    TFP(B)                                 99     TERMINAL PART.
235500000000     C  N99              ADD       1             $B                             CONTATORE TFP
235600000000     C  N99              Z-ADD     $B            B
235700000000     C  N99              Z-ADD     $LINEA        TFP(B)                         NUOVA TFP
235800000000     C                   ENDIF
235900000000      *
236000000000     C                   ENDSR
236100000000      **************************************************************************
236200000000      * La linea passata ($LINEA) viene cercata fra le linee di arrivo di cui
236300000000      * solo terminal di arrivo (schiera LNAR); se non trovata viene immessa.
236400000000      * Infine viene restituito l'indice di schiera (X) ove si trova l'elemento.
236500000000      *
236600000000      * Ricordo che $X e' il contatore della schiera LNAR.
236700000000      **************************************************************************
236800000000     C     INDSOL        BEGSR
236900000000      *
237000000000      * Verifico se il LNAR appena letto e' nuovo o no
237100000000     C                   Z-ADD     1             X                 3 0
237200000000     C     $LINEA        LOOKUP    LNAR(X)                                99     ON=TROVATA
237300000000     C  N99              ADD       1             $X                3 0          CONTATORE LNAR
237400000000     C  N99              Z-ADD     $X            X
237500000000     C  N99              Z-ADD     $LINEA        LNAR(X)                        NUOVA LNAR
237600000000      *
237700000000     C                   ENDSR
237800000000      **************************************************************************
237900000000      * La linea passata ($LINEA) viene cercata fra le linee di arrivo
238000000000      * (schiera LNA); se non trovata viene immessa come nuova linea.
238100000000      * Infine viene restituito l'indice di schiera (A) ove si trova l'elemento.
238200000000      *
238300000000      * Ricordo che $A e' il contatore della schiera LNA.
238400000000      **************************************************************************
238500000000     C     INDLNA        BEGSR
238600000000      *
238700000000      * Verifico se la LNA appena letto e' nuova o no
238800000000     C                   Z-ADD     1             A                 3 0
238900000000     C     $LINEA        LOOKUP    LNA(A)                                 99     LINEE ARRIVO
239000000000     C  N99              ADD       1             $A                             CONTATORE LNA
239100000000     C  N99              Z-ADD     $LINEA        LNA($A)                        NUOVA LNA
239200000000     C  N99              Z-ADD     $A            A
239300000000      *
239400000000     C                   ENDSR
239500000000      **************************************************************************
239600000000      * Sono passati in input due campi: linea di partenza ($LINEP) e linea di
239700000000      * arrivo ($LINEA)
239800000000      * Viene restituito il terminal di arrivo (sempre in $LINEA) e pulito il
239900000000      * campo $LINEP.
240000000000      **************************************************************************
240100000000     C     TROTFA        BEGSR
240200000000      *
240300970606     C                   CLEAR                   DSLV55
240400970606     C                   MOVEL     'A'           D55TPT
240500970606     C                   Z-ADD     $LINEA        D55LIN
240600970606     C                   Z-ADD     $LINEP        D55LNP
240700970606     C                   Z-ADD     FVADFV        D55DRF
240800970606     C                   CALL      'FNLV55R'
240900970606     C                   PARM                    DSLV55
241000970606     C*
241100970606     C     D55ERR        IFEQ      *BLANKS
241200970606     C                   Z-ADD     D55TFA        $LINEA
241300970606     C                   END
241400000000      *
241500000000     C                   Z-ADD     *ZERO         $LINEP
241600000000      *
241700000000     C                   ENDSR
241800970605      **************************************************************************
241900970605      * MEMORIZZAZIONE LINEE DI ARRIVO ELABORATE
242000970605      **************************************************************************
242100040130     C***  AGGLNT        BEGSR
242200040130     C***                Z-ADD     1             P
242300040130     C***  P             DOWLE     29
242400040130     C***  LNA(P)        ANDGT     *ZEROS
242500040130     C***  LNA(P)        LOOKUP    LNT                                    98
242600040130     C**N98              Z-ADD     1             PP                2 0
242700040130     C**N98000           LOOKUP    LNT(PP)                                99
242800040130     C**N98
242900040130     C**AN 99              MOVE      LNA(P)        LNT(PP)
243000040130     C**                 ADD       1             P
243100040130     C***                ENDDO
243200970605     c* ripeto per la schiera delle linee di cui sono solo terminal
243300040130     C***                Z-ADD     1             P
243400040130     C***  P             DOWLE     50
243500040130     C***  LNAR(P)       ANDGT     *ZEROS
243600040130     C***  LNAR(P)       LOOKUP    LTT(PP)                                98
243700040130     C**N98              Z-ADD     1             PP                2 0
243800040130     C**N98000           LOOKUP    LTT(PP)                                99
243900040130     C**N98
244000040130     C***AN 99              MOVE      LNAR(P)       LTT(PP)
244100040130     C**                 ADD       1             P
244200040130     C***                ENDDO
244300040130     C***                ENDSR
244400970605      **************************************************************************
244500090929      * SCRITTURA IN FISAR DELLE LINEE DI ARRIVO NON ELABORATE
244600040130      * da quando il report di sede non elabora più per lna ma per tfa
244700040130      * le specifiche seguenti non servono più.
244800970605      **************************************************************************
244900040130     C***  WRTSAR        BEGSR
245000970605     C* Leggo la L1 e la cerco nella schiera delle linee elaborate e
245100090929     c* se non risulta elaborata scrivo record in FISAR (mi serve per
245200970605     C* non dare "dati non pervenuti" sul report di sede)
245300040130     C***                Z-ADD     1             P
245400040130    1C***  P             DOWLE     60
245500040130     C***  l1(p)         ANDGT     *ZEROS
245600970605     C*
245700040130     C***  l1(p)         LOOKUP    LNT                                    98
245800040130    2C***  *IN98         IFEQ      *OFF
245900970605     C*
246000090929     C***                CLEAR                   FISAR000
246100040130     C***                Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
246200040130     C***                MOVE      *ZEROS        SARTFA                         TFA PRECEDENTE
246300040130     C***                Z-ADD     DS1ADA        SARDRE                         DATA
246400040130     C***                Z-ADD     *ZERO         SARTFP                         TERM.PARTENZA
246500040130     C***                Z-ADD     l1(p)         SARLNA                         LINEA DI ARRIVO
246600040130     C***                MOVE      *BLANKS       SARTRE                         TIPO RECORD
246700970605     C* Faccio la setll per sicurezza anche se il record non dovrebbe
246800970605     C* esserci
246900090929     C***  FISARK        SETLL     FISAR01L                               99
247000040130    3C***  *IN99         IFEQ      *OFF
247100970605      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
247200970605      * riempo il "flag di trasmissione" (SARFTR)
247300040130     C***                Z-ADD     *ZERO         SARDTR                         NR.TRASMISSIONE
247400040130    4C***  §5CSAR        IFEQ      'S'
247500040130     C***                MOVE      *BLANK        SARFTR                         FLAG TRASMISS.
247600040130     C***                ELSE
247700040130     C***                MOVE      'T'           SARFTR                         FLAG TRASMISS.
247800040130    4C***                ENDIF
247900090929     C***                WRITE     FISAR000
248000040130    3C***                END
248100040130    2C***                END
248200040130     C***                ADD       1             P
248300970605     C*
248400040130    1C***                ENDDO
248500970605     C*
248600970605     C* con lo stesso criterio elaboro le linee di cui sono solo termi-
248700970605     C* nal non elaborate
248800040130     C***                Z-ADD     1             P
248900040130    1C***  P             DOWLE     500
249000971126     C* scarto se terminal di partenza dell'area
249100040130     C***  ORG(P)        IFGT      0
249200040130    2C***  OTP(P)        ANDNE     SIMFEL
249300040130     C***  OTA(P)        ANDEQ     SIMFEL
249400040130     C***  ORG(P)        LOOKUP    LTT                                    98
249500040130    3C***  *IN98         IFEQ      *OFF
249600970605     C*
249700090929     C***                CLEAR                   FISAR000
249800040130     C***                Z-ADD     SIMFEL        SARFLE                         FILIALE EMITT.
249900040130     C***                MOVE      *ZEROS        SARTFA                         TFA PRECEDENTE
250000040130     C***                Z-ADD     DS1ADA        SARDRE                         DATA
250100040130     C***                Z-ADD     *ZERO         SARTFP                         TERM.PARTENZA
250200040130     C***                Z-ADD     ORG(P)        SARLNA                         LINEA DI ARRIVO
250300040130     C***                MOVEL     'R'           SARTRE                         TIPO RECORD
250400970605     C* Faccio la setll per sicurezza anche se il record non dovrebbe
250500970605     C* esserci
250600090929     C***  FISARK        SETLL     FISAR01L                               99
250700040130    4C***  *IN99         IFEQ      *OFF
250800970605      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
250900970605      * riempo il "flag di trasmissione" (SARFTR)
251000040130     C***                Z-ADD     *ZERO         SARDTR                         NR.TRASMISSIONE
251100040130    5C***  §5CSAR        IFEQ      'S'
251200040130     C***                MOVE      *BLANK        SARFTR                         FLAG TRASMISS.
251300040130     C***                ELSE
251400040130     C***                MOVE      'T'           SARFTR                         FLAG TRASMISS.
251500040130    5C***                ENDIF
251600090929     C***                WRITE     FISAR000
251700040130    4C***                END
251800040130    3C***                END
251900040130    2C***                END
252000040130     C***                ADD       1             P
252100040130    1C***                ENDDO
252200040130     C***                ENDSR
252300000000      **************************************************************************
252400090929      * Aggiornamento del file statistiche FISAR00F
252500090929     OFISAR003  E            AGGSAR
252600000000     O                       SARCLL
252601180209     OTISAR003  E            AGGSART
252602180209     O                       SARCLL
252603180209
