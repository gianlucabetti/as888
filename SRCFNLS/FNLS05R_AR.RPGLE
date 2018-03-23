000100991025     H DECEDIT('0,') DATEDIT(*DMY/)
000200940802     F* FNLS05R *-----------------------------------------------------*
000300900516     F*       INTERROGAZIONE BOLLE - DETTAGLIO BOLLA
000400000000     F*---------------------------------------------------------------*
000500940802     FFNLS05D   CF   E             WORKSTN
000600940810     F                                     SFILE(LS05S08:NRR8)
000700940810     F                                     SFILE(LS05S09:NRR9)
000800951219     F                                     SFILE(LS05S03:NRR3)
000900990119     F                                     SFILE(LS05S11:NRR11)
001000040220     FAZORG01L  IF   E           K DISK
001100940808     FCNIND00F  IF   E           K DISK
001200990119     FFNDFA01L  IF   E           K DISK
001300940804     FFNARB01L  IF   E           K DISK
001400021203     FFIAPD01L  IF   E           K DISK
001500940808     F* FILE STORICO VARIAZIONI BOLLE
001600991020     FFIARBF1C  IF   E           K DISK
001700021218     FFIAR501L  IF   E           K DISK
001800991022     FFIAR601L  IF   E           K DISK
001900991022     FFIAR701L  IF   E           K DISK
002000940802     FFNBLP01L  IF   E           K DISK
002100060216     FFiar401L  IF   E           K DISK
002200051115     FFiar901L  IF   E           K DISK
002300090226     FFiart01L  IF   E           K DISK
002400990809     FFNEVB01L  IF   E           K DISK
002500050406     FtIGCP01L  IF   E           K DISK
002600940804     FFNLBL01L  IF   E           K DISK
002700940804     FFNLBL02L  IF   E           K DISK
002800940804     F                                     RENAME(FNLBL000:FNLBL002)
002900940804     FTABEL00F  IF   E           K DISK
003000011120     FFNCSB01L  IF   E           K DISK
003100040220      *.......... T E L E F O N A T E   /   R E C L A M I
003200040220     fFITGD02L  if   e           k disk
003300040713      * ORM
003400040713     fFnorm01l  if   e           k disk
003500940804      *-------------------------------------------------------------------------
003600040220      *
003700040220      *   S C H I E R E
003800040220      *
003900940906     D SEQ             S             15    DIM(1) CTDATA PERRCD(1)
004000950306     D LBL             S              1    DIM(58) CTDATA PERRCD(58)
004100950306     D LB2             S             58    DIM(1) CTDATA PERRCD(1)
004200941011     D LBD             S             15    DIM(5) CTDATA PERRCD(1)
004300941011     D PRD             S             23    DIM(5) CTDATA PERRCD(1)
004400040220     d TF01            s              1    dim(20) ctdata perrcd(20)
004500021218     D TF02            S              1    DIM(20) CTDATA PERRCD(20)
004600021218     D TF04            S              1    DIM(20) CTDATA PERRCD(20)
004700021218     D TF05            S              1    DIM(20) CTDATA PERRCD(20)
004800021218     D TF07            S              1    DIM(20) CTDATA PERRCD(20)
004900021218     D TF08            S              1    DIM(20) CTDATA PERRCD(20)
005000021218     D TF10            S              1    DIM(20) CTDATA PERRCD(20)
005100021218     D TF11            S              1    DIM(20) CTDATA PERRCD(20)
005200021218     D TF12            S              1    DIM(20) CTDATA PERRCD(20)
005300021218     D TF13            S              1    DIM(20) CTDATA PERRCD(20)
005400021218     D TF14            S              1    DIM(20) CTDATA PERRCD(20)
005500040220     d TF16            s              1    dim(20) ctdata perrcd(20)
005600021218     D TF19            S              1    DIM(20) CTDATA PERRCD(20)
005700021218     D TF20            S              1    DIM(20) CTDATA PERRCD(20)
005800021218     D TF21            S              1    DIM(20) CTDATA PERRCD(20)
005900021218     D TF22            S              1    DIM(20) CTDATA PERRCD(20)
006000021218     D $TF             S              1    DIM(20)
006100021218     D*
006200940802     D CLI             S              7  0 DIM(300)
006300940907     D*
006400940907     D AAN             S              4  0 DIM(30)
006500940907     D LPN             S              3  0 DIM(30)
006600940907     D NRN             S              2  0 DIM(30)
006700940907     D NSN             S              7  0 DIM(30)
006800941021     D CC1             S              1    DIM(99)
006900941021     D CC2             S             20    DIM(99)
007000020329     D CC3             S              1    DIM(99)
007100020329     D CB1             S              1    DIM(99)
007200020329     D CB2             S             20    DIM(99)
007300030121     D RigaTf1         S              1    DIM(62)
007400030121     D RigaTf2         S              1    DIM(62)
007500030121     D RigaTf3         S              1    DIM(62)
007600040220      *
007700040220      *   S T R U T T U R E   D A T I
007800040220      *
007900010220     D*VISUALIZZAZIONE ORM - FIOR07R
008000010220     D Parm01          DS
008100010220     D  ppoe                          3  0
008200010220     D  pnor                          7  0
008300010220     D  pnsr                          2  0
008400010220     D  pnrv                          2  0
008500010220     D  psce                          1
008600010220     D  pfgs                          3  0
008700010220     D  ppor                          3  0
008800010220     D  pdtr                          8  0
008900010220     D  pmdb                         10
009000010220     D  pprb                         10
009100010220     D  pdts                          8  0
009200010220     D  prmp                          1
009300010220     D  pbrc                          1
009400010220     D  pref                          2
009500010220     D  pmio                          1
009600940804     D* DATI DA PGM RICHIAMANTE               - FNLS04R - FNLR36R -
009700940804     D PARAM0          DS
009800940808     D  PA0AAS                 1      4  0
009900940808     D  PA0LNP                 5      7  0
010000940808     D  PA0NRS                 8      9  0
010100940808     D  PA0NSP                10     16  0
010200941109     D  PA0F03                17     17
010300940907     D  PA0FLG                18     18
010400940808     D  PA0A9                 19     19
010500940808     D  PA0B5                 20     20
010600940808     D  PA075                 21     21
010700940808     D  PA098                 22     22
010800940808     D  PA0RSU                23     42
010900940808     D  PA0CML                43     43
011000940808     D  PA0SIM                44     46  0
011100940804      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
011200940808     D  PA0CLI                47     47
011300941213      * PA0GIA indica se il pgm e' richiamato dalle giacenze
011400941213     D  PA0GIA                48     48
011500991022      *
011600090226      * PASSAGGIO DATI A INTERROGAZIONE DETTAGLIO COLLI - FNLR37R -
011700090226     D PARA37          DS
011800090226     D  P37AAS                14     17  0
011900090226     D  P37LNP                18     20  0
012000090226     D  P37NRS                21     22  0
012100090226     D  P37NSP                23     29  0
012200090226     D  P37F03                30     30
012300090226     D  P37FLG                31     31
012400090226     D  P37PGM                32     41
012500090226     d* Denominazione azionda bartolini
012600090226     D  P37RSU                42     61
012700090226      *  P37CLI indica se il pgm e' richiamato da un cliente (='S')
012800090226     D**P37CLI                50     50
012900090226     D  P37DSP                63     70  0
013000090226     D  P37RMN                71     85  0
013100090226     D  P37LNA                86     88  0
013200090226     D  P37dLNA               89    108
013300090226     D  P37RSc               109    143
013400090227     D  P37TBO               153    153
013500090227     D  P37cod               154    160  0
013600991022      *
013700991022      * PASSAGGIO A INTERROGAZIONE SPUNTE               - FNLR04R -
013800941230     D PARAM1          DS                  INZ
013900921013     D  VI1NFV                 1      5  0
014000921013     D  VI1LNP                 6      8  0
014100921013     D  VI1LNA                 9     11  0
014200921013     D  VI1NRS                12     13  0
014300921013     D  VI1NCD                14     20  0
014400921013     D  VI1NCA                21     27  0
014500921013     D  VI1NPS                28     29  0
014600921013     D  VI1NPG                30     30  0
014700921013     D  VI1FLG                31     31
014800921013     D  VI1PGM                32     38
014900941230     D  VI1FGS                39     41  0
015000991022      *
015100991022      * PASSAGGIO A INTERROGAZIONE VARIAZIONI            - FNLR75R -
015200900927     D PARAM2          DS
015300940804     D  PA2AAS                14     17  0
015400940804     D  PA2LNP                18     20  0
015500940804     D  PA2NRS                21     22  0
015600940804     D  PA2NSP                23     29  0
015700991022      *  PREMUTO F3
015800940804     D  PA2F03                30     30
015900991022      *  FLG = "C" CHIUSURA FILES
016000940804     D  PA2FLG                31     31
016100940804     D  PA2PGM                32     41
016200940804     D  PA2DSA                42     61
016300940804     D  PA2DSP                63     70  0
016400940804     D  PA2RMN                71     85  0
016500940804     D  PA2LNA                86     88  0
016600940804     D  PA2LN1                89    108
016700940804     D  PA2RSD               109    143
016800950126     D  PA2GIA               144    144
016900950126     D  PA2FEL               145    147  0
017000950628     D  PA2TBO               148    148
017100991022      *
017200991022      * PASSAGGIO A INTERROGAZ.BOLLE ARRIVI:BOLLA LEGATA - FNLR36R -
017300920326     D PARAM4          DS
017400940808     D  PA4AAS                14     17  0
017500940808     D  PA4LNP                18     20  0
017600940808     D  PA4NRS                21     22  0
017700940808     D  PA4NSP                23     29  0
017800940810     D  PA4F03                30     30
017900940808     D  PA4FLG                31     31
018000991022      *
018100991022      * RICHIAMO PGM DI INTERROGAZIONE EVENTI             - FNLRB5R -
018200940804     D PARAB5          DS
018300940804     D  PB5AAS                14     17  0
018400940804     D  PB5LNP                18     20  0
018500940804     D  PB5NRS                21     22  0
018600940804     D  PB5NSP                23     29  0
018700940804     D  PB5F03                30     30
018800940804     D  PB5FLG                31     31
018900940804     D  PB5PGM                32     41
019000940804     D  PB5DSA                42     61
019100940804     D  PB5DSP                63     70  0
019200940804     D  PB5RMN                71     85  0
019300940804     D  PB5LNA                86     88  0
019400940804     D  PB5LN1                89    108
019500940804     D  PB5RSD               109    143
019600940804     D  PB5DCP               144    151  0
019700940804     D  PB5TMC               152    152
019800040220      *
019900040220     D WLBDAT          DS
020000040220     D  G02DAT                 1      8  0
020100040220     D  G02INV                 9     16  0
020200040220     D  G02ERR                17     17
020300040220     D  G02TGI                18     22  0
020400040220      *
020500040220     D                 DS
020600040220     D  DCTDCA                 1      8  0
020700040220     D  DCTAAC                 1      4  0
020800040220     D  DCTMGC                 5      8  0
020900040220      *
021000040220      *?definizione oggetto reclamo come spedizione?
021100040220     d ds_OggSped      ds                  inz
021200040220     d  BLPlnp                             inz
021300040220     d  BLPnrs                             inz
021400040220     d  BLPnsp                             inz
021500040220     d  BLPaas                             inz
021600040220      *
021700040220      * CAMPO UNICO COMPRENDENTE RAG.SOC.+AGGIUNTIVI 35 CARATTERI
021800040220     D                 DS
021900040220     D  V2CRSD                 1     35
022000040220     D  V2CNOT                36     70
022100040220     D  V2CNOM                 1     70
022200040220      *
022300040220     D TCUDS           DS
022400040220     D  F1                     1      1
022500040220     D  F3                     3      3
022600040220     D  F2                     2      2
022700040220     D  F4                     4      4
022800040220     D  F56                    5      6
022900040220
023000040220     d Status         sds
023100040220     d  SDSprm           *parms
023200040220
023300040220     D KPJBA         E DS
023400040220     d FIDNA1ds      e ds
023500040226      *
023600040226     d FIDNA2ds      e ds                  inz
023700040226     d  IA2mod       e                     inz('M')
023800040226     d  IA2tor       e                     inz('S')
023900991022      *
024000021218     D CnCr80        E DS
024100991022     D ut§dse0f      E DS
024200991022     D  TCU                  398    697    DIM(50)
024300991022     D  KCU                  698    847P 0 DIM(50)   PACKEVEN
024400991022      *
024500991022     D tnsc20ds      E DS
024600950102     D FNLR97        E DS
024700981023     D DSBL4E        E DS
024800060517     D*DSBL4F        E DS
024900060517     D DSBL4I        E DS
025000010220     D DSBL4M        E DS
025100991227     D OG143         E DS
025200930505     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
025300991022     D TNSD99DS      E DS                  INZ
025400900502     D DS3A          E DS
025500051128     D DS1B          E DS
025600991022     D DS$2          E DS
025700991022     D DSCV          E DS
025800020402     D DSCC          E DS
025900940802     D DS3K          E DS
026000951219     D DS7K          E DS
026100991022     D DS7PQRS       E DS
026200991022     D FNLG30DS      E DS
026300991022     D FNLR44DS      E DS
026400991022     D FNLV55DS      E DS
026500051124     D FNLV80DS      E DS
026600991022     D FIDN00DS      E DS
026700991025     d TIBS69DS      E DS                  INZ
026800991025     d DS_cnaco      E DS                  extname(CNACO00F)
026900991025     d DS_cnind      E DS                  extname(CNIND00F)
027000991025     d DS_cnclp      E DS                  extname(CNCLP00F)
027100991025     d DS_fncls      E DS                  extname(FNCLS00F)
027200021218     d FNLRB3Ds      E DS
027300030121     d dTba          e ds
027400031119     d dAr5Gen       e ds
027500030121     d dAr5Ban       e ds
027600030703     d dAr5Bnb       e ds
027700030121     d Tibs02Ds      e ds
027800040714      * ds per il controllo della presenza di CA per la spedizione richiesta
027900040714     d FIDN12DS      e ds
028000040714     d  skKey                 26    305    dim(20)
028100040714     d  skAnn                306    325    dim(20)
028200040714     d  skDca                326    485  0 dim(20)
028300040714     d  skFca                486    545  0 dim(20)
028400040714     d  skDch                546    705  0 dim(20)
028500040714     d  skCch                706    745    dim(20)
028600040714
028700040714     d dsKey           ds
028800040714     d  dsaac                         4  0
028900040714     d  dsfil                         3  0
029000040714     d  dsnca                         7  0
029100040220      *
029200040220      *   C O S T A N T I
029300040220      *
029400971212     D CMANUA          C                   CONST('Inser.MANUALE in ')
029500971212     D CAUTOM          C                   CONST('Attribuito in xxxx')
029600971212     D CSTAMP          C                   CONST(' Stampa XX')
029700040220      *
029800040220      *   V A R I A B I L I
029900040220      *
030000040220     D kpdpdr          S                   LIKE(APDpdr)
030100040220     D kpdtip          S                   LIKE(APDtip)
030200041028     D wdam            S                   LIKE(blpdam)
030300041028     D wdcm            S                   LIKE(blpdcm)
030400041028     D wdce            S                   LIKE(blpdce)
030500041028     D wnrc            S                   LIKE(blpnrc)
030600041028     D wdcp            S                   LIKE(blpdcp)
030700041028     D wcca            S                   LIKE(blpcca)
030800041028     D whmc            S                   LIKE(blphmc)
030900041028     D wtmc            S                   LIKE(blptmc)
031000041028     D wdet            S                   LIKE(blpdet)
031100041028     D wdut            S                   LIKE(blpdut)
031200041028     D wddc            S                   LIKE(blpddc)
031300041028     D wndc            S                   LIKE(blpndc)
031400040220
031500040220     d wtba            s                   Like(§Ar5Tba)
031600040220     d wdestba         s                   Like(V1dTba)
031700991022     D DATA_eur        S               D   DATFMT(*eur)
031800991022     D W0080           S              8  0
031900991025     D W0040           S              4  0
032000991022     D Wsvn            S                   LIKE(AR7svn)
032100991022     D Wvan            S                   LIKE(AR7van)
032200991022     D WDIV            S                   LIKE(AR6div)
032300991022     D KTRC            S                   LIKE(AR6TRC)
032400991022     D $3kinb          S                   LIKE(§3kinb)
032500991022     D $3kctb          S                   LIKE(§3kctb)
032600020225     d lnantw          S                   LIKE(§ogntw)
032700020225     d lnpntw          S                   LIKE(§ogntw)
032800991022     D KAAS            S                   LIKE(BLPaas)
032900991022     D Klnp            S                   LIKE(BLPlnp)
033000991022     D Knrs            S                   LIKE(BLPnrs)
033100991022     D Knsp            S                   LIKE(BLPnsp)
033200991022     D Kcod            S                   LIKE(TBLcod)
033300991022     D Kkey            S                   LIKE(TBLkey)
033400991022     D Kkcc            S                   LIKE(INDkcc)
033500991022     D Kksc            S                   LIKE(INDksc)
033600991022     D Comaan          S                   LIKE(LBLaan)
033700991022     D Wtfp            S                   LIKE(D55tfp)
033800991022     D Wtstdec         S                   LIKE(§CVfdc)
033900991025     D wtibs69r        s              1
034000991022     D W017A           s             17
034100991022     D w0130           s             13  0
034200991022     D w0133           s             13  3
034300991022     D IMPeditato      s             13
034400021218     D CF2413          C                   CONST('F24=AlTasti(1/3)')
034500021218     D CF2423          C                   CONST('F24=AlTasti(2/3)')
034600021218     D CF2433          C                   CONST('F24=AlTasti(3/3)')
034700021218     D CF2412          C                   CONST('F24=AlTasti(1/2)')
034800021218     D CF2422          C                   CONST('F24=AlTasti(2/2)')
034900040220
035000930115     I****************************************************************
035100941028     I* INDICATORI UTILIZZATI
035200930115     I****************************************************************
035300961111     I* 01 - BOLLA RIENTRO
035400961111     I* 02 - BOLLA FRNACO FRONTIERA
035500990118     I* 03 - ABILITA F14 DI INTERROGAZIONE GIACENZA
035600991021     I* 04 - ABILITA F13 DI TASSAZIONE 1a BOLLA
035700960523     I* 05 - DISABILITA F7=ANOMALIE
035800991021     I* 06 - ABILITA F4 DI TASSAZIONE 2a BOLLA
035900930115     I* 07 - ESISTE LEGAME BOLLA ABILITA CMD18
036000970227     I* 08 - SI DTT IN ALTA INTENSITA'
036100971212     I* 09 - IMPORT EXPORT SDI
036200990118     I* 10 - ABILITA F10 DI INTERROGAZ. VARIAIZONI BOLLE
036300991123     I* 11 - EMETTO SCRITTA BOLLO o IVA IN TASSAZIONE 2a BOLLA
036400990118     I* 12 - ABILITA INTERROGAZIONE COMUNICAZIONI ANOMALIA
036500980704     I* 13 - EMETTO RIFERIMENTO PARTNER PARTENZA
036600000216     I* 14 - lna dd
036700000216     I* 15 - ABILITA F23
036800941028     I* 16 - ESISTE PARTICOLARITA' CONTRASSEGNO
036900941028     I* 17 - ESISTE PARTICOLARITA' GIACENZA
037000941028     I* 18 - ESISTE PARTICOLARITA' CONSEGNA
037100941028     I* 19 - ESISTE PARTICOLARITA' VARIE
037200941028     I* 20 - SFLDSP
037300941028     I* 21 - SFLDSPCTL - SFLCLR
037400990118     I* 22 - ABILITA F7, F19 E F20
037500941028     I* 23 - SFLEND
037600990118     I* 24 - ABILITA VISUAL. CONTRASSEGNO
037700990118     I* 25 - ABILITA F24 LATRI TASTI
037800951220     I* 26 - PULIZIA SFL LS05S03
037900040220      * 27   Abilita F1  = Aggiunta in reclamo
038000040220      * 28   Abilita F16 = Gestione reclamo
038100941028     I* 31 - PREMUTO TASTO FUNZIONALE
038200941028     I* 33 - ON = SPEDIZIONE IN FRANCO
038300941028     I* 33 - OFF = SPEDIZIONE IN ASSEGNATO
038400930115     I* 34 - C'E' LEGAME MA NON ABILITA CMD18
038500930115     I* 44 - INDICA A VIDEO SPEDIZIONE GIACENTE APERTA
038600010220     I* 55 - ABILITA F08 (interrogazione orm)
038700021218     I* 77 - NOTE SUPERMERCATI
038800941028     I* 80 - USATO NEL VIDEO LEGAMI
038900941028     I* 81 - USATO NEL VIDEO LEGAMI
039000941028     I* 82 - USATO NEL VIDEO LEGAMI
039100941028     I* 83 - USATO NEL VIDEO LEGAMI
039200941028     I* 93 - USATO NEL VIDEO LEGAMI
039300930505     I* 94 - EMETTO CMD23 PER CALCOLARE E VISUALIZZARE DATA CONS.
039400930505     I*      PREVISTA E IL TOTALE ORE DI RITARDO
039500930701     I* 95 - ESISTONO EVENTI DA INTERROGARE
039600930527     I* 98 - DATA CONSEGNA MERCE >< 0
039700941021     I* 99 - GENERICO, UTILIZZABILE A PIACERE
039800930115     I****************************************************************
039900000000     C     *ENTRY        PLIST
040000000000     C                   PARM                    KPJBA
040100040220     c                   parm                    FIDNA1ds
040200040220      *
040300940804     C                   MOVEL     KPJBU         PARAM0
040400981123     C**
040500981123     C                   Z-ADD     1             CODUT
040600981123     C                   CALL      'X§PARUT'
040700991022     C                   PARM                    ut§dse0f
040800021218     C                   MOVEL     REC80         CNCR80
040900981123     C*--- RICERCA CAPOCONTI
041000981123     C                   DO        50            X                 2 0
041100981123     C                   MOVE      TCU(X)        TCUDS
041200981123     C     F56           CABNE     'CG'          END1
041300981123     C     F4            COMP      '1'                                    21
041400981123     C     F4            COMP      '2'                                    22
041500981123     C     F4            COMP      '3'                                    23
041600981123     C     F4            COMP      '6'                                    27
041700981123     C** 1 CLIENTI   21
041800981123     C** 2 FORNITORI 22
041900981123     C** 3 AGENTI    23
042000981123     C     F3            COMP      '0'                                242425
042100981123     C     F3            COMP      'I'                                    26
042200981123     C** 0 ITALIA   25
042300981123     C** 1 ESTERO   24
042400981123     ** I CAPO CONTO IVA
042500981123     C   21
042600981123     CAN 24              Z-ADD     KCU(X)        KCE               4 0
042700981123     C   21
042800981123     CAN 25              Z-ADD     KCU(X)        KCI               4 0
042900981123     C   22
043000981123     CAN 24              Z-ADD     KCU(X)        KFE               4 0
043100981123     C   22
043200981123     CAN 25              Z-ADD     KCU(X)        KFI               4 0
043300981123     C   23
043400981123     CAN 24              Z-ADD     KCU(X)        KAE               4 0
043500981123     C   23
043600981123     CAN 25              Z-ADD     KCU(X)        KAI               4 0
043700981123     C   26              Z-ADD     KCU(X)        KIVA              4 0
043800981123     C   27              Z-ADD     KCU(X)        KBNA              4 0
043900981123     C     END1          TAG
044000981123     C                   END
044100981123     C                   SETOFF                                       212223
044200981123     C                   SETOFF                                       242526
044300981123     C                   SETOFF                                       27
044400940804     C*
044500990701     C                   MOVEL     PA0RSU        V1CRSU
044600940808     C                   Z-ADD     1             CODUT             1 0
044700981123     C**
044800971202     C* DETERMINO LA DATA DEL GIORNO
044900971202     C                   TIME                    W0140            14 0
045000971202     C                   MOVE      W0140         WDTGIO            8 0
045100971202     C                   Z-ADD     WDTGIO        G02DAT
045200971202     C                   MOVEL     *BLANK        G02ERR
045300971202     C                   CALL      'XSRDA8'
045400971202     C                   PARM                    WLBDAT
045500971202     C                   MOVEL     G02INV        WDATEU            8 0
045600991022      *
045700991022      * CARICO LA TABELLA $2
045800991022     C                   MOVEL     '$2'          KCOD
045900991022     C                   MOVEL(P)  1             KKEY
046000991022     C     KTAB1         CHAIN     TABEL00F                           31
046100991022     C  N31              MOVEL     TBLUNI        DS$2
046200991022      *
046300991022      * CARICO IN SCHIERA LA TABELLA CC
046400941021     C                   Z-ADD     *ZERO         WA
046500941021     C                   CLEAR                   CC1
046600941021     C                   CLEAR                   CC2
046700020329     C                   CLEAR                   CC3
046800941021     C                   MOVEL     'CC'          KCOD
046900941021     C     KTAB2         SETLL     TABEL00F
047000941021     C     KTAB2         READE     TABEL00F                               99
047100941021     C     *IN99         DOWEQ     *OFF
047200941021     C     TBLKEY        ANDNE     *BLANKS
047300941021     C     TBLKEY        ANDNE     *ZERO
047400941021     C                   ADD       1             WA
047500941021     C                   MOVE      TBLKEY        CC1(WA)
047600941021     C                   MOVEL     TBLUNI        CC2(WA)
047700020402     C                   MOVEL(P)  TBLUNI        DSCC
047800020402     C                   MOVE      §CCDFE        CC3(WA)
047900941021     C     KTAB2         READE     TABEL00F                               99
048000941021     C                   ENDDO
048100020329      *
048200020329      * CARICO IN SCHIERA LA TABELLA CB
048300020329     C                   Z-ADD     *ZERO         WA
048400020329     C                   CLEAR                   CB1
048500020329     C                   CLEAR                   CB2
048600020329     C                   MOVEL     'CB'          KCOD
048700020329     C     KTAB2         SETLL     TABEL00F
048800020329     C     KTAB2         READE     TABEL00F                               99
048900020329     C     *IN99         DOWEQ     *OFF
049000020329     C     TBLKEY        ANDNE     *BLANKS
049100020329     C     TBLKEY        ANDNE     *ZERO
049200020329     C                   ADD       1             WA
049300020329     C                   MOVE      TBLKEY        CB1(WA)
049400020329     C                   MOVEL     TBLUNI        CB2(WA)
049500020329     C     KTAB2         READE     TABEL00F                               99
049600020329     C                   ENDDO
049700991022      *
049800991022      * Disabilito F7=Anomalie se in elenco pgm chiamanti c'è fnlr44r
049900991022      * Pa0flg="N" -- > Disabilitare F7
050000991022      * Pa0flg="R" -- > Disabilitare F7 - PGM RICHIAMATO DA FNLR36R
050100991022     c                   IF        PA0FLG = 'N'  or  PA0FLG = 'R'
050200991022     C                   eval      *in05 = *on
050300991022     C                   ENDIF
050400940802      * Testo il flag che mi dice se sono richiamato da un cliente
050500940802     C                   SETON                                        22
050600940804     C     PA0CLI        IFEQ      'S'
050700940802      *
050800940802      * Testo le abilitazioni del cliente
050900940802     C                   EXSR      ABICLI
051000940802     C     $3KINB        CABNE     'S'           FINE
051100940802     C                   SETOFF                                       22
051200940802      *
051300940802     C                   ENDIF
051400921013     C*
051500900511     C* INIZIALIZZO CAMPI ANCHE PER MEMORIZZARE BOLLA CHE NON USO
051600900427     C                   Z-ADD     0             BLPNRS
051700900427     C                   Z-ADD     0             BLPNSP
051800900427     C                   Z-ADD     0             BLPLNA
051900900427     C                   Z-ADD     0             BLPRMN
052000930701     C                   CLEAR                   PARAM1
052100930701     C                   CLEAR                   PARAM2
052200930701     C                   CLEAR                   PARAM4
052300090226     C                   CLEAR                   PARA37
052400940810     C                   CLEAR                   PARAB5
052500991022      *----------------------------------------------------------
052600991022      * CARICO BOLLA
052700900516     C                   EXSR      CARBLP
052800940923     C*
052900940923     C* SE LA BOLLA E' INESISTENTE VADO A FINE PROGRAMMA
053000940923     C     PA0NSP        IFEQ      *ZERO
053100940923     C                   GOTO      FINE
053200940923     C                   ENDIF
053300040220      *
053400040220      *?Abilito F1=Aggiunta in Reclami?
053500040220      * (se richiamato da pgm. FIDNA1R...)
053600040220     c                   eval      *in27  = *off
053700040303     c                   if            SDSprm >  1
053800040308     c                             and IA1ins = ' '
053900040220     c                   eval      *in27  = *on
054000040220     c                   endif
054100040220      *
054200040220      *?Abilito F16=Visualizzazione Reclami?
054300040220      * (se richiamato da pgm. FIDNA1R...)
054400040220     c                   eval      TGDtor  = 'S'
054500040220     c                   eval      TGDogg  = ds_OggSped
054600040220     c     K02TGD02      setll     FITGD000
054700040302     c                   eval      *in28   = (%equal(FITGD02L)   and
054800040303     c                                           (SDSprm =  1
054900040303     c                                        or (SDSprm >  1
055000040303     c                                        and IA1ins <> 'N')))
055100900427     C*
055200931115     C* SOLO SE C'E' CONTRASSEGNO VISUALIZZO IL TASTO CMD22
055300931116     C* VERIFICO NELL'ARCHIVIO SE C'E' CONTRASSEGNO
055400990118     C                   SETOFF                                       2412
055500011120     C     KBLP3         SETLL     FNCSB01L
055600011120     c                   if        %equal(fncsb01l)
055700051115     C     KBLP3         SETLL     Fiar901L
055800051115     c                   if        %equal(fiar901l)
055900011120     c                   eval      *in24 = *on
056000011120     c                   endif
056100011120     c                   endif
056200990118     C* CONTROLLO SE C'E' DANNO
056300990125     C                   EXSR      CTRDAN
056400940322      *
056500940802      * EMETTO IL FORMATO DI RIEPILOGO DELLA BOLLA
056600940802     C                   EXSR      RIEPIL
056700940802      *
056800940802      * CMD 3 - FINE LAVORO TORNO A SUBFILE
056900940808     C     *INKC         IFEQ      *ON
057000940808     C     WF03          ORNE      *BLANKS
057100941109     C                   MOVEL     '1'           PA0F03
057200940808     C                   GOTO      FINE
057300940808     C                   ENDIF
057400940802      *
057500940802      * PER F15 VISUALIZZO TUTTA LA BOLLA, PER INVIO ESCO
057600940802     C  NKP              GOTO      FINE
057700931115     C*
057800931115     C                   SETOFF                                       25
057900940804     C                   MOVEL     'S'           WRIEM
058000021218      *
058100021218      * Riempio il campone in fondo alla videata con i tasti scelti
058200021218     C                   exsr      Ges_TastiFun
058300021218     c                   eval      FD01 = FDxx
058400021218     c                   eval      F24D01 = F24Dxx
058500900427     C     FOR01         TAG
058600940804     C     WRIEM         IFEQ      'S'
058700940804     C                   WRITE     LS05T01
058800940804     C                   MOVEL     *BLANKS       WRIEM
058900940804     C                   ENDIF
059000940804     C*
059100021218      *
059200021218      * Imposto la stringa comandi del 'giro' successivo
059300021218     C                   IF        *inKY = *on and $Loop > 1
059400021218     c                   Exsr      Ges_F24
059500021218     c                   eval      FD01 = FDxx
059600021218     c                   eval      F24D01 = F24Dxx
059700021218     C                   ENDIF
059800021218      *
059900940804     C                   EXFMT     LS05D01
060000940808     C                   MOVEL     *BLANKS       WF03
060100931115      *
060200990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
060300931115     C   KY*IN25         IFEQ      *ON
060400931115     C                   SETOFF                                       25
060500931115     C                   ELSE
060600931115     C                   SETON                                        25
060700931115     C                   ENDIF
060800931115     C   KY              GOTO      FOR01
060900910806     C*
061000900530     C* CMD 3 - FINE LAVORO TORNO A SUBFILE
061100900530     C   KC              GOTO      FINE
061200921013     C*
061300921013     C* CMD12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
061400921013     C   KL              GOTO      FINE
061500911105     C*
061600921013     C* TASTI DI FUNZIONE VARI
061700921013     C                   EXSR      COMAND
061800940808     C*
061900940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
062000940808     C     WF03          IFNE      *BLANKS
062100941109     C                   MOVEL     '1'           PA0F03
062200940808     C                   GOTO      FINE
062300940808     C                   ENDIF
062400940808     C*
062500921013     C   31              GOTO      FOR01
062600921013     C*
062700900427     C* SE TUTTO OK EMETTO FORMATO2
062800931115     C                   SETOFF                                       25
062900940804     C                   MOVEL     'S'           WRIEM
063000021218      *
063100021218      * Riempio il campone in fondo alla videata con i tasti scelti
063200021218     C                   exsr      Ges_TastiFun
063300021218     c                   eval      FD02 = FDxx
063400021218     c                   eval      F24D02 = F24Dxx
063500900427     C     FOR02         TAG
063600940804     C     WRIEM         IFEQ      'S'
063700940804     C                   WRITE     LS05T01
063800940804     C                   MOVEL     *BLANKS       WRIEM
063900940804     C                   ENDIF
064000940804     C*
064100021218      * Imposto la stringa comandi del 'giro' successivo
064200021218     C                   IF        *inKY = *on and $Loop > 1
064300021218     c                   Exsr      Ges_F24
064400021218     c                   eval      FD02 = FDxx
064500021218     c                   eval      F24D02 = F24Dxx
064600021218     C                   ENDIF
064700021218      *
064800940804     C                   EXFMT     LS05D02
064900940808     C                   MOVEL     *BLANKS       WF03
065000931115      *
065100990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
065200931115     C   KY*IN25         IFEQ      *ON
065300931115     C                   SETOFF                                       25
065400931115     C                   ELSE
065500931115     C                   SETON                                        25
065600931115     C                   ENDIF
065700931115     C   KY              GOTO      FOR02
065800910806     C*
065900900427     C* CMD12 - RITORNO VIDEATA 1
066000900427     C   KL              GOTO      FOR01
066100940923     C*
066200940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
066300940923     C                   EXSR      CTRD02
066400940923     C   31              GOTO      FOR02
066500921013     C*
066600921013     C* TASTI DI FUNZIONE VARI
066700921013     C                   EXSR      COMAND
066800940808     C*
066900940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
067000940808     C     WF03          IFNE      *BLANKS
067100941109     C                   MOVEL     '1'           PA0F03
067200940808     C                   GOTO      FINE
067300940808     C                   ENDIF
067400921013     C   31              GOTO      FOR02
067500920429     C*
067600910806     C*
067700930112     C* SE ESISTONO DATI ENTRO IN TERZA VIDEATA
067800931115     C                   SETOFF                                       25
067900940804     C                   MOVEL     'S'           WRIEM
068000021218      *
068100021218      * Riempio il campone in fondo alla videata con i tasti scelti
068200021218     C                   exsr      Ges_TastiFun
068300021218     c                   eval      FD03 = FDxx
068400021218     c                   eval      F24D03 = F24Dxx
068500930112     C     FOR03         TAG
068600940804     C     WRIEM         IFEQ      'S'
068700940804     C                   WRITE     LS05T01
068800940804     C                   MOVEL     *BLANKS       WRIEM
068900940804     C                   ENDIF
069000940804     C*
069100951219     C                   Z-ADD     1             REC3
069200021218      *
069300021218      * Imposto la stringa comandi del 'giro' successivo
069400021218     C                   IF        *inKY = *on and $Loop > 1
069500021218     c                   Exsr      Ges_F24
069600021218     c                   eval      FD03 = FDxx
069700021218     c                   eval      F24D03 = F24Dxx
069800021218     C                   ENDIF
069900951219     C                   WRITE     LS05Z03
070000951219     C                   EXFMT     LS05C03
070100940808     C                   MOVEL     *BLANKS       WF03
070200931115      *
070300990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
070400931115     C   KY*IN25         IFEQ      *ON
070500931115     C                   SETOFF                                       25
070600931115     C                   ELSE
070700931115     C                   SETON                                        25
070800931115     C                   ENDIF
070900931115     C   KY              GOTO      FOR03
071000910806     C*
071100900507     C* CMD12 - RITORNO VIDEATA 2
071200900507     C   KL              GOTO      FOR02
071300930114     C*
071400930114     C* TASTI DI FUNZIONE VARI
071500930114     C                   EXSR      COMAND
071600940808     C*
071700940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
071800940808     C     WF03          IFNE      *BLANKS
071900941109     C                   MOVEL     '1'           PA0F03
072000940808     C                   GOTO      FINE
072100940808     C                   ENDIF
072200930114     C   31              GOTO      FOR03
072300900504     C*
072400000000     C     FINE          TAG
072500971202     C*
072600971202     C                   MOVEL     'C'           D55TLA
072700971202     C                   CALL      'FNLV55R'
072800991022     C                   PARM                    FNLV55DS
072900991025      *
073000991025     C                   IF        Wtibs69r  <>  *BLANKS
073100991025     C                   eval      I69TLA  = 'C'
073200991025     C                   call      'TIBS69R'
073300991025     C                   parm                    TIBS69DS
073400991025     c                   ENDIF
073500040714     C*
073600040714     C                   MOVEL     'C'           I12TLA
073700040714     C                   CALL      'FIDN12R'
073800040714     C                   PARM                    FIDN12DS
073900040714      *
074000991025      *
074100930614     C* REIMPOSTO KPJBU
074200940810     C   KC              MOVEL     '1'           PA0F03
074300940804     C                   MOVEL     PARAM0        KPJBU
074400930614     C*
074500000000     C                   SETON                                        LR
074600940808     C**************************************************************************
074700940808     C*    COMANDI UGUALI NELLE 2 VIDEATE
074800940808     C**************************************************************************
074900921013     C     COMAND        BEGSR
075000931115     C*
075100921013     C                   SETOFF                                       31
075200040220      *
075300040220      *?F1 - Aggiunta bolla in reclamo?
075400040220if  1c                   if        *inKA   = *on
075500040220     c                   clear                   FIDNA1ds
075600040220     c                   eval      IA1ins  = 'S'
075700040220     c                   eval      IA1tor  = 'S'
075800040220     c                   eval      IA1ogg  = ds_OggSped
075900040220     c                   goto      Fine
076000040220     c*****              return
076100040220e   1c                   endif
076200040220      *
076300990118     C* F5 - INTERROGAZIONE EVENTI
076400040220if  1C     *INKE         IFEQ      *ON
076500930526     C                   SETON                                        31
076600941213     C                   CLEAR                   PARAB5
076700940804     C                   MOVEL     PA0AAS        PB5AAS
076800940804     C                   MOVEL     PA0LNP        PB5LNP
076900940804     C                   MOVEL     PA0NRS        PB5NRS
077000940804     C                   MOVEL     PA0NSP        PB5NSP
077100940804     C                   MOVEL     'FNLS05R'     PB5PGM
077200940804     C                   MOVEL     V1CRSU        PB5DSA
077300940808     C                   MOVEL     V1CDSP        PB5DSP
077400940804     C                   MOVEL     V2CRMN        PB5RMN
077500940804     C                   MOVEL     V1CLNA        PB5LNA
077600940804     C                   MOVEL     V1DLNA        PB5LN1
077700940804     C                   MOVEL     V2CRSD        PB5RSD
077800940804     C                   MOVEL     V3CDCP        PB5DCP
077900940804     C                   MOVEL     V3CTMC        PB5TMC
078000940804     C                   MOVEL     PARAB5        KPJBU
078100940804     C                   CALL      'FNLRB5R'
078200930526     C                   PARM                    KPJBA
078300940808     C                   MOVEL     '1'           PA0B5
078400940808     C                   MOVEL     'S'           WRIEM
078500040220e   1C                   ENDIF
078600930526     C*
078700921013     C* CMD 7 - VISUALIZZA ANOMALIE
078800040220if  1C     *INKG         IFEQ      *ON
078900991022     C                   CLEAR                   FNLR44DS
079000960523     C                   MOVEL     PA0AAS        D44AAS
079100960523     C                   MOVEL     PA0LNP        D44FIL
079200960523     C                   MOVEL     PA0NRS        D44NRS
079300960523     C                   MOVEL     PA0NSP        D44NSP
079400960523     C                   MOVEL     'P'           D44TBO
079500960523     C                   MOVE      'I'           D44MOD
079600960523     C                   SETON                                        31
079700991022     C                   MOVEL     FNLR44DS      KPJBU
079800960523     C                   CALL      'FNLR44R'
079900960523     C                   PARM                    KPJBA
080000940808     C                   MOVEL     'S'           WRIEM
080100040220e   1C                   ENDIF
080200010220     C*
080300010220     C* CMD 8 - VISUALIZZA ORM
080400040220if  1C     *INKH         IFEQ      *ON
080500010220
080600010220     C                   clear                   PARM01
080700010220     C                   move      v1cpoe        ppoe
080800010220     C                   move      v1cnor        pnor
080900010220     C                   move      v1cnsr        pnsr
081000010220     C                   move      v1cnrv        pnrv
081100010220     C                   move      '5'           psce
081200010220     C                   move      v1clnp        pfgs
081300010220     C                   movel(p)  PARM01        KPJBU
081400010220
081500010220     C                   call      'FIOR07R'
081600010220     C                   parm                    KPJBA
081700010220     C                   MOVEL     'S'           WRIEM
081800010220     C                   SETON                                        31
081900010220
082000040220e   1C                   ENDIF
082100021218     C*
082200021218     C* F11 - Note supermercati
082300040220if  1C     *INKK         IFEQ      *ON
082400021218     C     *IN77         ANDEQ     *ON
082500021218      * imposto i campi 'bolla' della ds
082600021218     C                   clear                   FnLrb3ds
082700030122     c                   Movel     v1cRsu        Rb3Rsu
082800030121     C                   z-add     SimFel        RB3Fil
082900021218     C                   z-add     V1CAAS        RB3AAS
083000030121     C                   z-add     V1CLP1        RB3LNP
083100021218     C                   z-add     V1CNRS        RB3NRS
083200021218     C                   z-add     V1CNSP        RB3NSP
083300021218     C                   z-add     V1CDSP        RB3DSP
083400021218     C                   z-add     BLPRMN        RB3RMN
083500021218     C                   z-add     V1CLNA        RB3LNA
083600021218     C                   movel     BLPRSD        RB3RSD
083700021218     C                   z-add     V1CPKF        RB3PKF
083800021218     C                   z-add     V1CNCL        RB3NCL
083900021218     C                   z-add     V1CVLF        RB3VLB
084000030703      * bancali - part.varie "B"
084100040220If  2c                   If        %subst(BlpGva:1:1) = 'B'
084200030121     C                   z-add     §Ar5Tba       RB3Tba
084300030121     C                   z-add     V1cNba        RB3Nba
084400030121     C                   z-add     §Ar5Tb2       RB3Tb2
084500030121     C                   z-add     V1cNb2        RB3Nb2
084600030703     c                   Movel     V2cGva        Rb3Gva
084700040220e   2c                   EndIf
084800030703      * bancali - part.varie "O"
084900040220If  2c                   If        %subst(BlpGva:1:1) = 'O'
085000030703     C                   z-add     §Ar5bTba      RB3Tba
085100030703     C                   z-add     V1cNba        RB3Nba
085200030703     C                   z-add     §Ar5bTb2      RB3Tb2
085300030703     C                   z-add     V1cNb2        RB3Nb2
085400030703     c                   Movel     V2cGva        Rb3Gva
085500030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
085600030703     c                   Z-Add     §Ar5bCor      Rb3Cor
085700040220e   2c                   EndIf
085800021218      * richiamo PGM per note supermercati
085900021218     C                   call      'FNLRB3R'
086000030122     c                   Parm                    Kpjba
086100030121     C                   PARM                    Fnlrb3Ds
086200021218      *
086300030703     c                   Eval      *In31 = *On
086400021218     C                   MOVEL     'S'           WRIEM
086500040220e   1C                   ENDIF
086600921013     C*
086700921013     C* CMD14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
086800040220if  1C     *INKN         IFEQ      *ON
086900930701     C     *IN03         ANDEQ     *ON
087000941213     C     PA0GIA        ANDEQ     *BLANK
087100991022     C                   CLEAR                   FNLG30DS
087200941213     C                   MOVEL     'P'           D30POA
087300941213     C                   MOVEL     PA0AAS        D30AAS
087400941213     C                   MOVEL     PA0LNP        D30LNP
087500941213     C                   MOVEL     PA0NRS        D30NRS
087600941213     C                   MOVEL     PA0NSP        D30NSP
087700991022     C                   MOVEL     FNLG30DS      KPJBU
087800930701     C                   SETON                                        31
087900941213     C                   CALL      'FNLG30C'
088000921013     C                   PARM                    KPJBA
088100940808     C                   MOVEL     'S'           WRIEM
088200040220e   1C                   ENDIF
088300040220      *
088400040220      *?F16 - Gestione reclami?
088500040226if  1c                   if        *inKQ   = *on
088600040226     c                   reset                   FIDNA2ds
088700040226if  2c                   if        SDSprm  > 1
088800040226     c                   eval      IA2mod  = 'I'
088900040226e   2c                   endif
089000040226     c                   eval      IA2ogg  = ds_OggSped
089100040226     c                   call      'FIDNA2R'
089200040226     c                   parm                    KPJBA
089300040226     c                   parm                    FIDNA2ds
089400040226     c                   movel     OA2f03        WF03
089500040226     c                   movel     'S'           WRIEM
089600051201     C                   SETON                                        31
089700040220e   1c                   endif
089800921013     C*
089900921013     C* CMD18 - LEGAME BOLLA: VADO IN INTERROG. BOLLE ARRIVI
090000040220if  1C     *INKS         IFEQ      *ON
090100921013     C                   EXSR      CMD18
090200921013     C                   SETON                                        31
090300941222     C                   MOVEL     'S'           WRIEM
090400040220e   1C                   ENDIF
090500921013     C*
090600090226     C* F21 - DETTAGLIO SEGNACOLLI
090700040220if  1C     *INKV         IFEQ      *ON
090800090226     C                   MOVEL     PA0AAS        P37AAS
090900090226     C                   MOVEL     PA0LNP        P37LNP
091000090226     C                   MOVEL     PA0NRS        P37NRS
091100090226     C                   MOVEL     PA0NSP        P37NSP
091200090226     C                   MOVEL     *BLANKS       P37F03
091300090226     C                   MOVEL     *BLANKS       P37FLG
091400090226     C                   MOVEL     *BLANKS       P37PGM
091500090226     C                   MOVEL     'FNLS05R'     P37PGM
091600090226     C                   MOVEL     V1CRSU        P37RSU
091700090226     C*******??          MOVEL     PA0CLI        P37CLI
091800090226     c                   movel     v1cdsp        p37dsp
091900090226     c                   movel     v2crmn        p37rmn
092000090226     c                   movel     v1clna        p37lna
092100090226     c                   movel     v1dlna        p37dlna
092200090226     c                   movel     v2ccdm        p37cod
092300090226     c                   movel     v1crsm        p37rsc
092400090226     c                   movel     'P'           p37tbo
092500090226     C                   MOVEL     PARA37        KPJBU
092600930701     C                   SETON                                        31
092700090226     C                   CALL      'FNLR37R'
092800921013     C                   PARM                    KPJBA
092900090226     C                   MOVEL     KPJBU         PARA37
093000090226     C                   MOVEL     P37F03        WF03
093100940808     C                   MOVEL     'S'           WRIEM
093200040220e   1C                   ENDIF
093300921013     C*
093400990118     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
093500040220do  1C   KJ              DO
093600930526     C                   SETON                                        31
093700940804     C                   MOVEL     PA0AAS        PA2AAS
093800940804     C                   MOVEL     PA0LNP        PA2LNP
093900940804     C                   MOVEL     PA0NRS        PA2NRS
094000940804     C                   MOVEL     PA0NSP        PA2NSP
094100940804     C                   MOVEL     *BLANKS       PA2F03
094200940804     C                   MOVEL     *BLANKS       PA2FLG
094300940804     C                   MOVEL     *BLANKS       PA2PGM
094400940804     C                   MOVEL     'FNLS05R'     PA2PGM
094500940804     C                   MOVEL     V1CRSU        PA2DSA
094600940808     C                   MOVEL     V1CDSP        PA2DSP
094700940804     C                   MOVEL     V2CRMN        PA2RMN
094800940804     C                   MOVEL     V1CLNA        PA2LNA
094900940804     C                   MOVEL     V1DLNA        PA2LN1
095000940804     C                   MOVEL     V2CRSD        PA2RSD
095100950628     C                   MOVEL     'P'           PA2TBO
095200941222     C                   MOVEL     PA0SIM        PA2FEL
095300940804     C                   MOVEL     PARAM2        KPJBU
095400940804     C                   CALL      'FNLR75R'
095500921013     C                   PARM                    KPJBA
095600940808     C                   MOVEL     '1'           PA075
095700940808     C                   MOVEL     'S'           WRIEM
095800040220e   1C                   ENDDO
095900921013     C*
096000990118     C* F19 - INTERROGA SPUNTE
096100040220if  1C     *INKT         IFEQ      *ON
096200930701     C                   SETON                                        31
096300950116     C                   MOVEL     BLPFLS        VI1LNP
096400940804     C                   MOVEL     PA0NRS        VI1NRS
096500941230     C                   MOVEL     9             VI1NPG
096600930701     C                   MOVEL     99            VI1NPS
096700090226     C                   MOVEL     'FNLS05R'     VI1PGM
096800930701     C                   MOVEL     '1'           VI1FLG
096900930701     C                   Z-ADD     BLPLNA        VI1LNA
097000930701     C*
097100040220if  2C     BLPNCD        IFGT      0
097200930701     C                   Z-ADD     BLPNCD        VI1NCD
097300930701     C                   Z-ADD     BLPNCA        VI1NCA
097400040220x   2C                   ELSE
097500930701     C*
097600090226     C     KBLP3         SETLL     FiarT01L
097700090226     C     KBLP3         READE     FiarT01L                               31
097800040220do  3C  N31              DO
097900090226     C                   Z-ADD     arTNSC        VI1NCD
098000090226     C     KBLP3         SETGT     FiarT01L
098100090226     C     KBLP3         READPE    FiarT01L                               31
098200090226     C  N31arTNSC        IFNE      VI1NCD
098300090226     C                   MOVEL     arTNSC        VI1NCA
098400040220     C                   ENDIF
098500040220e   3C                   ENDDO
098600040220e   2C                   ENDIF
098700930701     C*
098800930701     C                   MOVEL     PARAM1        KPJBU
098900940804     C                   CALL      'FNLR04R'
099000921013     C                   PARM                    KPJBA
099100940808     C                   MOVEL     'S'           WRIEM
099200040220e   1C                   ENDIF
099300930505     C*
099400990118     C* F23 - VISUALIZZA DATA CONSEGNA PREVISTA E ORE DI RITARDO
099500040220do  1C   KX              DO
099600930505     C                   SETOFF                                       94
099700930505     C                   SETON                                        31
099800930505     C*
099900930505     C                   MOVEL     ' '           D98TLA
100000930505     C                   MOVEL     'P'           D98TBO
100100930505     C                   MOVEL     BLPAAS        D98AAS
100200930505     C                   MOVEL     BLPLNP        D98LNP
100300930505     C                   MOVEL     BLPNRS        D98NRS
100400930505     C                   MOVEL     BLPNSP        D98NSP
100500930505     C*
100600970925     C                   CALL      'TNSD99R'
100700991022     C                   PARM                    TNSD99DS
100800930505     C*
100900940808     C                   Z-ADD     D98DCE        G02INV
101000940808     C                   Z-ADD     *ZEROS        G02DAT
101100940808     C                   MOVEL     '3'           G02ERR
101200940808     C                   CALL      'XSRDA8'
101300940808     C                   PARM                    WLBDAT
101400940808     C                   Z-ADD     G02DAT        V1CDCE
101500940808     C                   Z-ADD     D98NRC        V1CNRC
101600930526     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
101700930526     C*   CONSEGNA PREVISTA
101800940808     C  N98              Z-ADD     D98HCE        V1CHMC
101900930528     C  N98              Z-ADD     D98HCE        VIDHC2
102000930505     C*
102100940808     C* SE PA098= 1  E' STATA FATTA CALL PGM
102200940808     C                   MOVEL     '1'           PA098
102300040220e   1C                   ENDDO
102400931115     C*
102500990118     C* F20 - VISUALIZZAZIONE AFFIDABILITA'
102600040220do  1C   KU              DO
102700950102     C                   CLEAR                   FNLR97
102800950102     C                   Z-ADD     PA0AAS        R97AAS
102900950102     C                   Z-ADD     PA0LNP        R97LNP
103000950102     C                   Z-ADD     PA0NRS        R97NRS
103100950102     C                   Z-ADD     PA0NSP        R97NSP
103200950102     C                   Z-ADD     V1CDSP        R97DSP
103300950102     C                   Z-ADD     V2CRMN        R97RMN
103400950102     C                   Z-ADD     V1CLNA        R97LNA
103500950102     C                   Z-ADD     V3CDCP        R97DCP
103600950102     C                   MOVEL     V2CRSD        R97RSD
103700090304     C                   MOVEL     V2CRSm        R97RSm
103800090304     c                   movel     v2ccdm        r97ccm
103900950102     C                   MOVEL     V1CRSU        R97RSU
104000950328     C                   MOVEL     V1DLNA        R97DLA
104100950102     C                   MOVEL     PA0CML        R97CML
104200950102     C                   MOVE      'P'           R97AP
104300950102     C   95              MOVE      '1'           R97EVE
104400950102     C                   Z-ADD     V1CDBR        R97DBR
104500950102     C                   Z-ADD     V1CDPC        R97DPC
104600950102     C                   Z-ADD     V1CDUC        R97DUC
104700950328     C                   MOVEL     V3CTMC        R97TMC
104800950102     C                   Z-ADD     V2CCAS        R97CAS
104900950102     C                   MOVEL     V2CVCA        R97VCA
105000931115     C                   MOVE      *BLANKS       KPJBU
105100950102     C                   MOVEL     FNLR97        KPJBU
105200940804     C                   CALL      'FNLR97R'                            31
105300931115     C                   PARM                    KPJBA
105400950102     C                   MOVEL     KPJBU         FNLR97
105500950102     C                   MOVEL     R97F03        WF03
105600931115     C                   SETON                                        31
105700940808     C                   MOVEL     'S'           WRIEM
105800040220e   1C                   ENDDO
105900931115     C*
106000990118     C* F22 - INTERROGAZIONE CONTRASSEGNO, SOLO SE PRESENTE
106100040220if  1C   KWV2CCAS        IFGT      *ZERO
106200991022     C                   CLEAR                   tnsc20ds
106300950125     C                   Z-ADD     1             D20INT
106400950125     C                   Z-ADD     PA0AAS        D20ANN
106500950125     C                   Z-ADD     PA0LNP        D20LNP
106600950125     C                   Z-ADD     PA0NRS        D20NSR
106700950125     C                   Z-ADD     PA0NSP        D20NSP
106800991022     C                   MOVEL     tnsc20ds      KPJBU
106900950125     C                   CALL      'TNSC23R'                            31
107000931115     C                   PARM                    KPJBA
107100931115     C                   SETON                                        31
107200040220     C                   MOVEL     'S'           WRIEM
107300040220e   1C                   ENDIF
107400941021     C*
107500991022     C* ? FORMATO VIDEO:  LS05D08  (TASSAZIONE 1a BOLLA) ?
107600991022     C*  EMETTO SOLO SE VI E' TASSAZIONE
107700040220do  1C   KM              DO
107800941021     C     WRIEM         IFEQ      'S'
107900941021     C                   WRITE     LS05T01
108000941021     C                   MOVEL     *BLANKS       WRIEM
108100941021     C                   ENDIF
108200941021     C                   WRITE     LS05D08
108300941021     C*
108400941021     C                   EXFMT     LS05C08
108500941021     C                   SETON                                        31
108600040220e   1C                   ENDDO
108700991022      *
108800991022      * ? FORMATO VIDEO:  LS05D04  (TASSAZIONE 2a BOLLA) ?
108900040220do  1C   KD              DO
109000991022     C     WRIEM         IFEQ      'S'
109100991022     C                   WRITE     LS05T01
109200991022     C                   MOVEL     *BLANKS       WRIEM
109300991022     C                   ENDIF
109400991022     C                   EXFMT     LS05D04
109500991022     C                   SETON                                        31
109600040220e   1C                   ENDDO
109700991022      **
109800990118     C** F2 - INTERROGAZIONE C.A. SE CE N'E' PIU' DI UNA EMETTO SFL
109900990129     C* Pa0flg="C" -- > Disabilitare F2 PERCHE' RICHIAMATO DA C.A.
110000040220if  1C   KBPA0FLG        IFNE      'C'
110100990129     C                   EXSR      CMD02
110200040220e   1C                   ENDIF
110300931115     C*
110400921013     C                   ENDSR
110500990125     C**************************************************************************
110600990125     C** F2 - INTERROGAZIONE C.A.
110700990125     C**************************************************************************
110800990125     C     CMD02         BEGSR
110900990125     C     CONTCA        IFEQ      1
111000991022     C                   CLEAR                   FIDN00DS
111100990125     C                   Z-ADD     VHCAAC        I00AAC
111200990125     C                   Z-ADD     VHCFIL        I00FIL
111300990125     C                   Z-ADD     VHCNCA        I00NCA
111400990125     C                   Z-ADD     PA0AAS        I00AAS
111500990125     C                   Z-ADD     PA0NRS        I00NRS
111600990125     C                   Z-ADD     PA0NSP        I00NSP
111700990125     C                   Z-ADD     PA0LNP        I00LNP
111800990125     C                   MOVEL     'P'           I00TPB
111900991022     C                   MOVEL     FIDN00DS      KPJBU
112000990125     C                   CALL      'FIDN01R'
112100990125     C                   PARM                    KPJBA
112200000309     C                   PARM                    CALLricors        1
112300990125     C                   SETON                                        31
112400990125     C                   MOVEL     'S'           WRIEM
112500990125     C                   ELSE
112600990125     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
112700990125     C                   SETOFF                                       21
112800990125     C                   WRITE     LS05C11
112900990125     C                   SETON                                        212023
113000990125     C                   CLEAR                   NRR11
113100990125     C**
113200040714     C**""*KBLP3         CHAIN     FNDCT02L                           31
113300040714     C**""**IN31         DOWEQ     *OFF
113400040714     C**""*DCTATB        IFEQ      ' '
113500040714     C**""*              Z-ADD     DCTAAC        V11AAC
113600040714     C**""*              Z-ADD     DCTFIL        V11FIL
113700040714     C**""*              Z-ADD     DCTNCA        V11NCA
113800040714     C**""*              Z-ADD     DCTDCA        G02INV
113900040714     C**""*              Z-ADD     *ZERO         G02DAT
114000040714     C**""*              MOVEL     '3'           G02ERR
114100040714     C**""*              CALL      'XSRDA8'
114200040714     C**""*              PARM                    WLBDAT
114300040714     C**""*              Z-ADD     G02DAT        V11DCA
114400040714     C**""*              Z-ADD     DCTFCA        V11FCA
114500040714     C**""*DCTFCA        CHAIN     FNDFA01L                           31
114600040714     C**N31              MOVEL     DFADES        V11DFA
114700040714     C**""*
114800040714     C**""*              ADD       1             NRR11             4 0
114900040714     C**""*              WRITE     LS05S11
115000040714     C**""*              ENDIF
115100040714     C**""**
115200040714     C**""*KBLP3         READE     FNDCT02L                               31
115300040714     C**""**             ENDDO
115400040714      *
115500040714      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
115600040714      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
115700040714      * legate alla spedizione che passo
115800040714     c                   clear                   fidn12ds
115900040714     c                   eval      i12aas = pa0aas
116000040714     c                   eval      i12lnp = pa0lnp
116100040714     c                   eval      i12nrs = pa0nrs
116200040714     c                   eval      i12nsp = pa0nsp
116300040714     c                   eval      i12fel = 'E'
116400040714     c                   eval      i12fan = 'E'
116500040714      *
116600040714     c                   call      'FIDN12R'
116700040714     c                   parm                    fidn12ds
116800040714      *
116900040714      * se non ci sono errori
117000040714     c                   if        o12err = ' '
117100040714      * se numero di CA trovate maggiore di zero
117200040714     c                   if        o12nca > 0
117300040714     c
117400040714     c                   z-add     *zeros        ii                2 0
117500040714     c     1             do        o12nca        ii
117600040714     c                   movea     skkey(ii)     dskey
117700040714     C                   Z-ADD     dsaac         V11AAC
117800040714     C                   Z-ADD     dsfil         V11FIL
117900040714     C                   Z-ADD     dsnca         V11NCA
118000040714     C                   Z-ADD     skdca(ii)     G02INV
118100040714     C                   Z-ADD     *ZERO         G02DAT
118200040714     C                   MOVEL     '3'           G02ERR
118300040714     C                   CALL      'XSRDA8'
118400040714     C                   PARM                    WLBDAT
118500040714     C                   Z-ADD     G02DAT        V11DCA
118600040714     C                   Z-ADD     skfca(ii)     V11FCA
118700040714     C     V11FCA        CHAIN     FNDFA01L                           31
118800040714     C  N31              MOVEL     DFADES        V11DFA
118900040714     C**
119000040714     C                   ADD       1             NRR11             4 0
119100040714     C                   WRITE     LS05S11
119200040714     C                   ENDDO
119300040714      *
119400040714     c                   endif
119500040714     c                   endif
119600040714      *
119700990125     C* EMETTO FORMATO
119800990125     C     FOR11         TAG
119900990125     C                   Z-ADD     1             NRR11
120000990125     C                   WRITE     LS05T01
120100990125     C                   WRITE     LS05Z11
120200990125     C                   EXFMT     LS05C11
120300990125     C* F12 - RITORNO
120400990125     C     *INKL         IFEQ      *OFF
120500990125     C                   READC     LS05S11                                31
120600990125     C     *IN31         DOWEQ     *OFF
120700990125     C     V11SCE        IFEQ      '5'
120800991022     C                   CLEAR                   FIDN00DS
120900990125     C                   Z-ADD     V11AAC        I00AAC
121000990125     C                   Z-ADD     V11FIL        I00FIL
121100990125     C                   Z-ADD     V11NCA        I00NCA
121200990125     C                   Z-ADD     PA0AAS        I00AAS
121300990125     C                   Z-ADD     PA0NRS        I00NRS
121400990125     C                   Z-ADD     PA0NSP        I00NSP
121500990125     C                   Z-ADD     PA0LNP        I00LNP
121600990125     C                   MOVEL     'P'           I00TPB
121700991022     C                   MOVEL     FIDN00DS      KPJBU
121800990125     C                   CALL      'FIDN01R'
121900990125     C                   PARM                    KPJBA
122000000309     C                   PARM                    CALLricors
122100990125     C**
122200990125     C                   CLEAR                   V11SCE
122300990125     C                   UPDATE    LS05S11
122400990125     C                   ENDIF
122500990125     C**
122600990125     C                   READC     LS05S11                                31
122700990125     C                   ENDDO
122800990125     C                   GOTO      FOR11
122900990125     C                   ENDIF
123000990125     C                   SETON                                        31
123100990125     C                   MOVEL     'S'           WRIEM
123200990125     C*
123300990125     C                   ENDIF
123400990125     C                   ENDSR
123500990125     C**************************************************************************
123600990125     C*    CONTROLLO SE C'E' UNA O PIU' C.A.
123700990125     C**************************************************************************
123800990125     C     CTRDAN        BEGSR
123900990125     C                   CLEAR                   CONTCA
124000040714     C**""*KBLP3         CHAIN     FNDCT02L                           31
124100040714     C**""**IN31         DOWEQ     *OFF
124200040714     C**""*DCTATB        IFEQ      ' '
124300040714     C**""**             SETON                                        12
124400040714     C**""**             ADD       1             CONTCA            3 0
124500040714     C**""**             Z-ADD     DCTAAC        VHCAAC
124600040714     C**""**             Z-ADD     DCTFIL        VHCFIL
124700040714     C**""**             Z-ADD     DCTNCA        VHCNCA
124800040714     C**""**             ENDIF
124900990125     C*
125000040714     C**""*KBLP3         READE     FNDCT02L                               31
125100040714     C**""**             ENDDO
125200040714      *
125300040714      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON
125400040714      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
125500040714      * late e legate alla spedizione che passo
125600040714     c                   clear                   fidn12ds
125700040714     c                   eval      i12aas = pa0aas
125800040714     c                   eval      i12lnp = pa0lnp
125900040714     c                   eval      i12nrs = pa0nrs
126000040714     c                   eval      i12nsp = pa0nsp
126100040714     c                   eval      i12fel = 'E'
126200040714     c                   eval      i12fan = 'E'
126300040714      *
126400040714     c                   call      'FIDN12R'
126500040714     c                   parm                    fidn12ds
126600040714      *
126700040714      * se non ci sono errori
126800040714     c                   if        o12err = ' '
126900040714      * se numero di CA trovate maggiore di zero
127000040714     c                   if        o12nca > 0
127100040714     c                   z-add     O12NCA        CONTCA            3 0
127200040714     C                   SETON                                        12
127300040714     C                   MOVEA     SKKEY(O12NCA) DSKEY
127400040714     C                   Z-ADD     DSAAC         VHCAAC
127500040714     C                   Z-ADD     DSFIL         VHCFIL
127600040714     C                   Z-ADD     DSNCA         VHCNCA
127700040714     C                   ENDIF
127800040714     C                   ENDIF
127900040714      *
128000990125     C                   ENDSR
128100940808     C**************************************************************************
128200971212     C*    CARICO RECORD DA VISUALIZZARE/MANUTENZIONARE
128300940808     C**************************************************************************
128400900511     C     CARBLP        BEGSR
128500940808     C*
128600930112     C*** PULIZIA CAMPI
128700930112     C*
128800930112     C                   MOVEL     ' '           VI2CEI
128900950628     C                   MOVEL     'P'           PA2TBO
129000940804     C                   Z-ADD     0             V3CDCP
129100930526     C*
129200920326     C                   SETOFF                                       440307
129300951011     C                   SETOFF                                       949813
129400021218     C                   SETOFF                                       0977
129500940923     C*
129600940923     C                   MOVEL     PA0AAS        V1CAAS
129700940926     C                   MOVEL     PA0LNP        V1CLNP
129800940923     C                   MOVEL     PA0NRS        V1CNRS
129900940923     C                   MOVE      PA0NSP        V1CNSP
130000940923     C*
130100900511     C* CHAIN
130200010710     C*****              MOVEL     *BLANKS       WSTOR
130300041028     c                   clear                   wesarb            1
130400940802     C     KBLP3         CHAIN     FNBLP01L                           31
130500041028     C     KBLP3         CHAIN     FNarb01L
130600041028     c                   if        %found(fnarb01l)
130700041028     c                   eval      wesarb='S'
130800041028     c                   else
130900041028     c                   eval      wesarb='N'
131000041028     c                   endif
131100940926     C*
131200940928     C* SE NON TROVATA BOLLA, PROVO NELLO STORICO
131300940928     C* E SE NON TROVATA PULISCO I CAMPI CHIAVE E ESCO
131400010713     C     *IN31         IFEQ      *ON
131500010716     C                   MOVEL     *ZERO         PA0AAS
131600010716     C                   MOVEL     *ZERO         PA0LNP
131700010716     C                   MOVEL     *ZERO         PA0NRS
131800010716     C                   MOVEL     *ZERO         PA0NSP
131900010716     C                   GOTO      ENDBLP
132000010713     C                   ENDIF
132100021218     C*
132200040220     C* 77 ON - Esistono note supermercati da interrogare
132300021218     C                   Z-ADD     PA0AAS        KAAS
132400021218     C                   Z-ADD     PA0LNP        KLNP
132500021218     C                   Z-ADD     PA0NRS        KNRS
132600021218     C                   Z-ADD     PA0NSP        KNSP
132700021218     C                   MOVEL     'SCR'         KTRD
132800021218     C     KAR51         CHAIN     FIAR5000                           31
132900021218     C                   If        *in31 = *off and Ar5Atb = ' '
133000021218     C                   seton                                        77
133100021218     C                   else
133200021218     C                   setoff                                       77
133300021218     C                   endif
133400940926     C*
133500900927     C* CONTROLLO SE SPEDIZIONE GIACENTE
133600050406     C     KBLP3         CHAIN     tIGCP01L                           31
133700940923     C*
133800901004     C  N31GCPDCG        IFEQ      0
133900910321     C                   SETON                                        4403
134000910321     C                   ELSE
134100910321     C                   SETON                                        03
134200901004     C                   END
134300940804     C   03PA0CLI        COMP      'S'                                0303
134400900511     C*
134500950214     C                   CLEAR                   V2CFLS
134600950214     C     BLPNCD        IFNE      *ZERO
134700950214     C     BLPFNS        ORNE      *BLANKS
134800940923     C                   MOVEL     BLPFLS        V2CFLS
134900950214     C                   ENDIF
135000940923     C                   MOVEL     PA0NRS        V7CNRS
135100940808     C                   MOVEL     BLPLNP        V1CLP1
135200940805     C                   MOVEL     BLPCBO        V1CCBO
135300940809     C*
135400940809     C                   MOVEL     BLPGGA        V2CGGA
135500940923     C                   MOVEL     *BLANKS       V2IGGA
135600940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
135700940923     C     V2CGGA        COMP      *BLANKS                            1717
135800940809     C*
135900940809     C                   MOVEL     BLPGMA        V2CGMA
136000940923     C                   MOVEL     *BLANKS       V2IGMA
136100940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
136200940923     C     V2CGMA        COMP      *BLANKS                            1818
136300940809     C*
136400960208     C                   MOVEL     BLPGVA        V2CGVA
136500960208     C                   MOVE      ' '           V2CGVA
136600940923     C                   MOVEL     *BLANKS       V2IGVA
136700940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
136800940923     C     V2CGVA        COMP      *BLANKS                            1919
136900940322      *
137000911105     C* NON SEQUENZIALI
137100910912     C     BLPFNS        IFEQ      'S'
137200940808     C                   MOVEL     SEQ(1)        V2CFNS
137300910912     C                   ELSE
137400940808     C                   MOVEL     *BLANKS       V2CFNS
137500910912     C                   END
137600930112     C* CONTROLLO SE ESISTONO VARIAZIONI PER LA BOLLA
137700991020     C     KBLP3         CHAIN     FIARBF1C                           31
137800940804     C     PA0CLI        IFNE      'S'
137900920429     C  N31              SETON                                        10
138000940802     C                   ENDIF
138100910806     C*
138200920409     C* TIPO BOLLA CONTROLLO SE FRANCO
138300940804     C                   MOVEL     '3A'          KCOD
138400940804     C                   MOVEL     *BLANKS       KKEY
138500940805     C                   MOVEL     BLPCBO        KKEY
138600940809     C     KTAB1         CHAIN     TABEL00F                           31
138700940923     C  N31              MOVEL     TBLUNI        DS3A
138800940923     C   31              CLEAR                   DS3A
138900900516     C                   MOVEL     §3ATB1        FLGTB1            1
139000940804     C                   MOVEL     §3ADES        V1DCBO
139100940805     C                   MOVEL     §3ADES        V2DCBO
139200940804     C                   MOVEL     §3ADES        V7DCBO
139300940408     C     FLGTB1        IFEQ      'F'
139400910418     C                   SETON                                        33
139500900511     C                   END
139600900511     C* LINEA ARRIVO E DECODIFICA
139700940804     C                   MOVEL     BLPLNA        V1CLNA
139800020225     C     BLPLNA        CHAIN     AZORG01L
139900020225     c                   if        not %found(azorg01l)
140000020225     C                   MOVEL     *BLANKS       ORGDES
140100020225     c                   endif
140200940804     C                   MOVEL     ORGDES        V1DLNA
140300940804     C                   MOVEL     ORGDES        V2DLNA
140400940810     C                   MOVEL     ORGDES        V9DLNA
140500020225     c                   eval      og143  = ORGde3
140600020225     c                   eval      lnantw = §ogntw
140700971202     C* TERMINAL DI PARTENZA DELLA LINEA DI ARRIVO
140800991022     C                   CLEAR                   FNLV55DS
140900971202     C                   MOVEL     'P'           D55TPT
141000971202     C                   MOVE      BLPLNA        D55LIN
141100971202     C                   Z-ADD     WDATEU        D55DRF
141200971202     C                   CALL      'FNLV55R'
141300991022     C                   PARM                    FNLV55DS
141400971202     C                   MOVE      D55TFP        WTFP
141500940804     C* LINEA PARTENZA E DECODIFICA
141600020225     C     BLPLNP        CHAIN     AZORG01L
141700020225     c                   if        not %found(azorg01l)
141800020225     C                   MOVEL     *BLANKS       ORGDES
141900020225     c                   endif
142000940804     C                   MOVEL     ORGDES        V1DLNP
142100020225     c                   eval      og143  = ORGde3
142200020225     c                   eval      lnpntw = §ogntw
142300920410     C* MITTENTE
142400940804     C                   MOVEL     BLPRSM        V1CRSM
142500940808     C                   MOVEL     BLPRSM        V2CRSM
142600940805     C                   MOVEL     BLPRSM        V7CRSM
142700940804     C                   MOVEL     BLPINM        V1CINM
142800940808     C                   MOVEL     BLPINM        V7CINM
142900940804     C                   MOVEL     BLPPRM        V1CPRM
143000940804     C                   MOVEL     BLPCAM        V1CCAM
143100940804     C                   MOVEL     BLPLOM        V1CLOM
143200940808     C                   MOVEL     BLPLOM        V2CLOM
143300940808     C                   MOVEL     BLPLOM        V7CLOM
143400940808     C                   MOVEL     BLPPRM        V2CPRM
143500940804     C                   MOVEL     BLPNZM        V1CNZM
143600940808     C                   MOVEL     BLPFAP        V1CFAP
143700981023     C                   MOVEL     BLPCTR        V2CCTR
143800060216     C* NUMERO ORM: DA ar4 (Tipo record 'M')
143900950412     C                   MOVEL     BLPAAS        KAAS
144000950412     C                   MOVEL     BLPLNP        KLNP
144100950412     C                   MOVEL     BLPNRS        KNRS
144200950412     C                   MOVEL     BLPNSP        KNSP
144300010220     C                   MOVEL     'M'           KTRC
144400010220     C                   clear                   v1cpoe
144500010220     C                   clear                   v1cnsr
144600010220     C                   clear                   v1cnor
144700010220     C                   clear                   v1cnrv
144800010220     C                   setoff                                       55
144900060216     C     Kar41         CHAIN     Fiar401L                           99
145000010220     C     *in99         ifeq      *off
145100060216     C     ar4not        ifne      *blanks
145200060216     C                   movel     ar4not        dsbl4m
145300010220     C                   move      §b4poe        v1cpoe
145400010220     C                   move      §b4nsr        v1cnsr
145500010220     C                   move      §b4nor        v1cnor
145600010220     C                   move      §b4nrv        v1cnrv
145700040713     c     korm          Setll     Fnorm01l
145800040713     c                   If        %Equal(fnorm01l)
145900010220     C                   seton                                        55
146000040713     c                   EndIf
146100010220     C                   endif
146200010220     C                   endif
146300010220     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
146400060216     C* DA Fiar4 SE RECORD ESISTENTE
146500950412     C                   MOVEL     'P'           KTRC
146600060216     C     Kar41         CHAIN     Fiar401L                           99
146700950412     C     *IN99         IFEQ      *OFF
146800060216     C                   MOVEL     ar4NOT        V1CPIM
146900060216     C                   MOVE      ar4NOT        V2CPID
147000950412     C                   ELSE
147100950412     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI BLPCPI
147200930112     C     BLPCPI        IFNE      *BLANKS
147300940804     C   33              MOVEL     BLPCPI        V1CPIM
147400940804     C  N33              MOVEL     BLPCPI        V2CPID
147500930112     C                   ELSE
147600940808     C                   MOVEL     *BLANKS       V1CPIM
147700940808     C                   MOVEL     *BLANKS       V2CPID
147800930112     C                   END
147900950412     C                   END
148000061108      * codice fiscale mittente e destinatario da fiar4 rcd Q
148100061108     c                   clear                   v1cdfm
148200061108     c                   clear                   v2cdfd
148300061108     c                   eval      ktrc = 'Q'
148400061108     c     kar41         chain     fiar401l
148500061108     c                   if        %found(fiar401l)
148600061108     c                   movel     ar4not        v1cdfm
148700061108     c                   move      ar4not        v2cdfd
148800061108     c                   endif
148900900525     C*
149000940906     C* CERCO LA DESCRIZIONE AGGIUNTIVA PER IL DESTINATARIO
149100940808     C                   MOVEL     BLPAAS        KAAS
149200940808     C                   MOVEL     BLPLNP        KLNP
149300940808     C                   MOVEL     BLPNRS        KNRS
149400940808     C                   MOVEL     BLPNSP        KNSP
149500940906     C                   MOVEL     'D'           KTRC
149600060216     C     Kar41         CHAIN     Fiar401L                           99
149700060216     C  N99              MOVEL     ar4NOT        V2CNOT
149800950125     C   99              MOVEL     *BLANKS       V2CNOT
149900940808     C*
150000971212     C* SOLO PER LE FILIALI ESTERE IMPORT/EXPORT VISUALIZZO RIF PARTNER
150100971212     C                   CLEAR                   V1CEST
150200951011     C                   MOVEL     'E'           KTRC
150300060216     C     Kar41         CHAIN     Fiar401L                           99
150400060216     C  N99              MOVEL     ar4NOT        V2CRPP
150500980704     C  N99              SETON                                        13
150600020225     c   99              if        (lnantw = 'EEX' or lnantw = 'EUP' or
150700020225     c                              lnantw = 'FED') or
150800020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
150900020225     c                              lnpntw = 'FED')
151000980704     C                   SETON                                        13
151100980704     C                   ENDIF
151200951011     C   99              CLEAR                   V2CRPP
151300971212     C* SE E' SDI IMPORTO ANCHE TIPO INSERIMENTO E STAMPA
151400020225     c                   if        not *in99 and (lnpntw = 'EUP' or
151500020225     c                                            lnantw = 'EUP')
151600971212     C**
151700060216     C                   MOVEL     ar4NOT        DSBL4E
151800971212    3C     §B4EMA        IFEQ      'M'
151900971212     C                   MOVEL     CMANUA        W021A            21
152000971212     C                   SETON                                        09
152100971212    4C     §B4EAT        IFEQ      'P'
152200971212     C                   MOVE      'PART'        W021A
152300971212    4C                   ENDIF
152400971212    4C     §B4EAT        IFEQ      'A'
152500971212     C                   MOVE      'ARR.'        W021A
152600971212    4C                   ENDIF
152700971212     C                   MOVE      W021A         V1CEST
152800971212    3C                   ENDIF
152900971212    C**
153000971212    3C     §B4EMA        IFEQ      'A'
153100971212     C                   MOVEL     CAUTOM        W018A            18
153200971212     C                   SETON                                        09
153300971212    4C     §B4EAT        IFEQ      'P'
153400971212     C                   MOVE      'PART'        W018A
153500971212    4C                   ENDIF
153600971212    4C     §B4EAT        IFEQ      'A'
153700971212     C                   MOVE      'ARR.'        W018A
153800971212    4C                   ENDIF
153900971212     C*
154000971212     C                   MOVEL     W018A         V1CEST
154100971212     C                   MOVE      CSTAMP        V1CEST
154200971212     C**
154300971212    4C     §B4EST        IFEQ      'N'
154400971212     C                   MOVE      'NO'          V1CEST
154500971212    4C                   ELSE
154600971212     C                   MOVE      'SI'          V1CEST
154700971212    4C                   ENDIF
154800991022      *
154900971212    3C                   ENDIF
155000971212    2C                   ENDIF
155100991227     c* DPD
155200000216     c                   setoff                                       14
155300000216     c                   clear                   v2crtn
155400000216     c                   clear                   v2czon
155500020225     c                   if        lnantw = 'DPD'
155600991227     c                   seton                                        13
155700060517     C***                MOVEL     'F'           KTRC
155800060517     C                   MOVEL     'I'           KTRC
155900060216     C     Kar41         CHAIN     Fiar401L                           99
156000060517     C**N99              MOVEL     ar4NOT        dsbl4f
156100060517     c** 99              clear                   dsbl4f
156200060517     C  N99              MOVEL     ar4NOT        dsbl4I
156300060517     c   99              clear                   dsbl4I
156400060517     C***                MOVEL     §b4fnp        V2CRPP
156500060517     C                   MOVEL     §b4ipn        V2CRPP
156600060517     c****               if        §b4rtn>*zeros or §b4zon>*zeros
156700060517     c****               movel     §b4rtn        v2crtn
156800060517     c****               movel     §b4zon        v2czon
156900060517     c                   if        §b4ida>*zeros or §b4iza>*zeros
157000060517     c                   movel     §b4ida        v2crtn
157100060517     c                   movel     §b4iza        v2czon
157200000216     c                   seton                                        14
157300000216     c                   setoff                                       09
157400000216     c                   endif
157500050304     c                   else
157600050304     c* dpd import
157700050304     c                   if        lnpntw = 'DPD'
157800060517     c****               clear                   dsbl4f
157900060517     C****               MOVEL     'F'           KTRC
158000060517     c                   clear                   dsbl4i
158100060517     C                   MOVEL     'I'           KTRC
158200060216     C     Kar41         CHAIN     Fiar401L
158300060216     c                   if        %found(fiar401l)
158400050304     c                   seton                                        13
158500060517     C***                MOVEL     ar4NOT        dsbl4f
158600060517     C                   MOVEL     ar4NOT        dsbl4I
158700060517     C***                MOVEL     §b4fnp        V2CRPP
158800060517     C                   MOVEL     §b4ipn        V2CRPP
158900050304     c                   endif
159000050304     c                   endif
159100991227     c                   endif
159200981023     C***
159300981023     C* ASSEGNATO
159400981023     C     *IN33         IFEQ      *OFF
159500981023     C** MITTENTE
159600981023     C     BLPCCM        IFNE      0
159700981023     C                   MOVEL     BLPCCM        V2CCDM
159800981023     C                   MOVEL     BLPCCM        V1CKSC
159900981023     C                   ELSE
160000981023     C                   CLEAR                   V2CCDM
160100981023     C                   CLEAR                   V1CKSC
160200981023     C                   ENDIF
160300981023     C* DESTINATARIO
160400981023     C                   MOVEL     BLPKSC        V2CCDD
160500981023     C                   MOVEL     BLPKSC        V1CCDD
160600991116     c                   eval      KTRC = '1'
160700991116     C     KAR6          CHAIN     FIAR6000                           99
160800991116     C                   IF        *in99 = *OFF and ar6ksc > *zeros
160900991116     C                   MOVEL     ar6ksc        V2CCDD
161000991116     C                   MOVEL     ar6ksc        V1CCDD
161100991116     C                   MOVEL     ar6ctr        V2CCTR
161200991116     c                   ENDIF
161300981023     C                   ELSE
161400981023     C* FRANCO
161500981023     C                   MOVEL     BLPKSC        V1CKSC
161600981023     C                   MOVEL     BLPKSC        V2CCDM
161700981023     C                   CLEAR                   V2CCDD
161800981023     C                   CLEAR                   V1CCDD
161900981023     C                   ENDIF
162000050117     C                   MOVEL     BLPXCO        V2CXCO
162100940808     C*
162200940808     C* SE C'E' IL COD. MITTENTE, RILEVO IL NR.TELEFONICO
162300940809     C                   MOVE      V2CCDM        WQUATT            4 0
162400940809     C     WQUATT        IFNE      8888
162500940809     C     V2CCDM        ANDNE     *ZERO
162600981123     C                   MOVEL     KCI           KKCC
162700940808     C                   MOVEL     V2CCDM        KKSC
162800940808     C     KIND1         CHAIN     CNIND00F                           99
162900940808     C  N99              MOVEL     INDTEL        V2CTEL
163000940808     C   99              MOVEL     *BLANKS       V2CTEL
163100940809     C                   ENDIF
163200940808     C*
163300940808     C                   MOVEL     BLPRSD        V2CRSD
163400940808     C                   MOVEL     BLPRSD        V7CRSD
163500940810     C                   MOVEL     BLPRSD        V9CRSD
163600940808     C                   MOVEL     BLPIND        V2CIND
163700940808     C                   MOVEL     BLPIND        V7CIND
163800940808     C                   MOVEL     BLPPRD        V2CPRD
163900940808     C                   MOVEL     BLPCAD        V2CCAD
164000940808     C                   MOVEL     BLPLOD        V2CLOD
164100940808     C                   MOVEL     BLPLOD        V7CLOD
164200940808     C                   MOVEL     BLPNZD        V2CNZD
164300940804     C                   MOVEL     BLPFIN        V2CFIN
164400940804     C                   MOVEL     BLPGC1        V2CGC1
164500940804     C                   MOVEL     BLPGC2        V2CGC2
164600961111     C                   MOVEL     *BLANKS       V2CFBR
164700961111     C                   MOVEL     *BLANKS       V2CFFR
164800961111     C                   SETOFF                                       0102
164900940804     C     BLPFBR        IFEQ      'S'
165000940804     C                   MOVEL     'SI'          V2CFBR
165100961111     C                   SETON                                        01
165200940804     C                   ENDIF
165300961111     C     BLPFBR        IFEQ      'F'
165400961111     C                   MOVEL     'FR'          V2CFFR
165500961111     C                   SETON                                        02
165600961111     C                   ENDIF
165700970227     C     BLPFST        IFEQ      'S'
165800970131     C     BLPFST        OREQ      'C'
165900970227     C                   MOVEL     'SI DDT'      V1DBAM
166000970227     C                   SETON                                        08
166100970130     C                   ELSE
166200970227     C                   MOVEL     'NO DDT'      V1DBAM
166300970227     C                   SETOFF                                       08
166400970130     C                   ENDIF
166500970227     C*
166600970227     C                   SELECT
166700970227     C     BLPFST        WHENEQ    'C'
166800970227     C     BLPFST        OREQ      'K'
166900970227     C                   MOVEL     'CLIENTE '    V1DFST
167000970227     C     BLPFST        WHENEQ    'S'
167100970227     C     BLPFST        OREQ      'Y'
167200970227     C                   MOVEL     'PARTENZA'    V1DFST
167300970227     C     BLPFST        WHENEQ    'J'
167400970227     C                   MOVEL     'ARRIVO  '    V1DFST
167500970227     C                   OTHER
167600970227     C                   MOVEL     'NO      '    V1DFST
167700970227     C                   ENDSL
167800970227     C*
167900941219     C     BLPFDN        IFEQ      'S'
168000941219     C                   MOVEL     'SI'          V1CFDN
168100941219     C                   ELSE
168200941219     C                   MOVEL     *BLANKS       V1CFDN
168300941219     C                   ENDIF
168400940808     C                   MOVEL     BLPFTM        V1CFTM
168500940804     C                   MOVEL     *BLANKS       V1DTSP
168600940804     C                   MOVEL     '5E'          KCOD
168700940804     C                   MOVEL     *BLANKS       KKEY
168800940808     C                   MOVEL     BLPTSP        KKEY
168900940809     C     KTAB1         CHAIN     TABEL00F                           99
169000940804     C  N99              MOVEL     TBLUNI        V1DTSP
169100941021     C*
169200940805     C                   MOVEL     BLPCTS        V8CCTS
169300940804     C                   MOVEL     BLPQFT        V2CQFT
169400940804     C                   MOVEL     BLPVMD        V2CVMD
169500940804     C                   MOVEL     BLPVAD        V2CVAD
169600940804     C                   MOVEL     BLPIAS        V2CIAS
169700940804     C                   MOVEL     BLPVAS        V2CVAS
169800940808     C                   MOVEL     BLPNAS        V1CNAS
169900940804     C                   MOVEL     BLPNCL        V1CNCL
170000941026     C                   MOVEL     BLPPKF        V1CPKF
170100990909     C                   MOVEL     BLPPKB        V1CPKB
170200940808     C                   MOVEL     BLPVLB        V1CVLB
170300940808     C                   MOVEL     BLPFVB        V1CFVB
170400940808     C                   MOVEL     BLPVLF        V1CVLF
170500940808     C                   MOVEL     BLPFVF        V1CFVF
170600940808     C* SE BLPNCR = 99999  A VIDEO AZZERO NCR E VLC
170700930505     C     BLPNCR        IFEQ      99999
170800940805     C                   MOVEL     *ZEROS        BLPNCR
170900940805     C                   MOVEL     *ZEROS        BLPVLC
171000940808     C                   ENDIF
171100940808     C                   MOVEL     BLPVLC        V1CVLC
171200940808     C                   MOVEL     BLPNCR        V1CNCR
171300990909     C* SE BLPNCP = 99999  A VIDEO AZZERO NCP E PKC
171400990909     C     BLPNCP        IFEQ      99999
171500990909     C                   MOVEL     *ZEROS        BLPNCP
171600990909     C                   MOVEL     *ZEROS        BLPPKC
171700990909     C                   ENDIF
171800990909     C                   MOVEL     BLPPKC        V1CPKC
171900990909     C                   MOVEL     BLPNCP        V1CNCP
172000031119
172100031119      * Referente e Telefono del destinatario
172200031119     c                   Clear                   V2cRef
172300031119     c                   Clear                   V2cTeld
172400031119      * li prendo da Fiar500f record 'GEN'
172500031119     c                   Clear                   dAr5Gen
172600031119     c                   Z-Add     BlpAas        Kaas
172700031119     c                   Z-Add     BlpLNp        Klnp
172800031119     c                   Z-Add     BlpNrs        Knrs
172900031119     c                   Z-Add     BlpNsp        Knsp
173000031119     c                   Eval      Ktrd = 'GEN'
173100031119     c     kAr51         Chain     Fiar501l
173200031119If  1c                   If        %Found(Fiar501l)
173300031119     c                   Movel     Ar5Uni        Dar5Gen
173400031119     c                   Eval      V2cRef = §Ar5Ref
173500031119     c                   Eval      V2cTeld = §Ar5Teld
173600031119    1c                   EndIf
173700040301
173800040301      * Se colli di valore visualizzo "V" nell'apposito campo
173900040301      *  con lo specifico attributo di visualizzazione
174000040301if  1c                   if        §AR5bva = *blanks
174100040301     c                   eval      V2Cbva  = *blanks
174200040301x   1c                   else
174300040301     c                   eval      V2Cbva  = 'V'
174400040301e   1c                   endif
174500031119
174600030121      * Bancali
174700030121     c                   Clear                   V1dTba
174800030121     c                   Clear                   V1dTb2
174900030122     c                   Clear                   V1cNba
175000030122     c                   Clear                   V1cNb2
175100030703      * Record 'BAN'
175200030703If  0c                   If        %subst(BlpGva:1:1) = 'B'
175300030121     c                   Clear                   dAr5Ban
175400030121     c                   Eval      Ktrd = 'BAN'
175500030121     c     kAr51         Chain     Fiar501l
175600030121If  1c                   If        %Found(Fiar501l)
175700030121     c                   Movel     Ar5Uni        Dar5Ban
175800030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
175900030122      * per le note relative al mancato reso dei bancali
176000030122     c                   If        Not *In77 and
176100030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
176200030122     c                   Eval      *In77 = *On
176300030122     c                   EndIf
176400030121     c                   Eval      V1cNba = §Ar5Nba
176500030121     c                   Eval      V1cNb2 = §Ar5Nb2
176600030121If  2c                   If        §Ar5Tba <> *Zeros
176700030703     c                   Eval      wtba = §Ar5Tba
176800030703     c                   ExSr      Sr_DecTba
176900030703     c                   Eval      V1dTba = wdestba
177000030121    2c                   EndIf
177100030121If  2c                   If        §Ar5Tb2 <> *Zeros
177200030703     c                   Eval      wtba = §Ar5Tb2
177300030703     c                   ExSr      Sr_DecTba
177400030703     c                   Eval      V1dTb2 = wdestba
177500030121    2c                   EndIf
177600030121    1c                   EndIf
177700030703    0c                   EndIf
177800030703      * Record 'BNB'  -  Spedizione OVER 25 colli
177900030703If  0c                   If        %subst(BlpGva:1:1) = 'O'
178000030703     c                   Clear                   dAr5Bnb
178100030703     c                   Eval      Ktrd = 'BNB'
178200030703     c     kAr51         Chain     Fiar501l
178300030703If  1c                   If        %Found(Fiar501l)
178400030703     c                   Movel     Ar5Uni        Dar5Bnb
178500030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
178600030703      * per i colli originali in caso di spedizione over 25 colli
178700030703     c  n77              Eval      *In77 = *On
178800030703     c                   Eval      V1cNba = §Ar5bNba
178900030703     c                   Eval      V1cNb2 = §Ar5bNb2
179000030703If  2c                   If        §Ar5bTba <> *Zeros
179100030703     c                   Eval      wtba = §Ar5bTba
179200030703     c                   ExSr      Sr_DecTba
179300030703     c                   Eval      V1dTba = wdestba
179400030703    2c                   EndIf
179500030703If  2c                   If        §Ar5bTb2 <> *Zeros
179600030703     c                   Eval      wtba = §Ar5bTb2
179700030703     c                   ExSr      Sr_DecTba
179800030703     c                   Eval      V1dTb2 = wdestba
179900030703    2c                   EndIf
180000030703    1c                   EndIf
180100030703    0c                   EndIf
180200920409     C*
180300920409     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
180400940804     C                   MOVEL     'TR'          KCOD
180500940804     C                   MOVEL     *BLANKS       KKEY
180600940808     C                   MOVEL     V2CCTR        UNOCTR            1
180700940804     C                   MOVEL     UNOCTR        KKEY
180800940809     C     KTAB1         CHAIN     TABEL00F                           31
180900940804     C                   MOVEL     TBLUNI        V2DCTR
181000910730     C*
181100940808     C* VISUALIZZO V2CCDM SOLO SE LA LNP DEL CODICE = LNP DELLA BOLLA
181200930505     C                   MOVEL     BLPCCM        NUM3              3 0
181300930505     C     BLPLNP        IFEQ      NUM3
181400940808     C                   MOVE      BLPCCM        V2CCDM
181500930505     C                   END
181600930505     C*
181700940804     C                   MOVEL     BLPRMN        V2CRMN
181800940804     C                   MOVEL     BLPRMA        V2CRMA
181900940804     C                   MOVEL     BLPRMO        V2CRMO
182000940804     C                   MOVEL     BLPNCD        V2CNCD
182100940804     C                   MOVEL     BLPNCA        V2CNCA
182200940804     C                   MOVEL     BLPZNC        V2CZNC
182300940804     C                   MOVEL     BLPCTM        V1CCTM
182400051128     C                   MOVEL     '1B'          KCOD
182500051128     C                   MOVEL(P)  blpctm        KKEY
182600051128     C     KTAB1         CHAIN     TABEL00F                           31
182700051128     C  N31              MOVEL     TBLUNI        DS1B
182800051128     c   31              clear                   ds1b
182900051128     c* Vedo se ha il packing list
183000051128     c                   if        §1bf16='S'
183100051128     c                   eval      v1dar8='PACKING LIST'
183200051128     C                   ELSE
183300051128     C                   CLEAR                   V1DAR8
183400051128     C                   ENDIF
183500051104     c
183600051104     c                   select
183700051104     c                   when      blpffd='S'
183800051104     C                   MOVEL     'SI '         V2CFFD
183900051104     c                   when      blpffd='M'
184000051104     C                   MOVEL     'SAB'         V2CFFD
184100051104     C                   other
184200940905     C                   MOVEL     *BLANKS       V2CFFD
184300051104     C                   ENDsl
184400051104     c
184500940808     C                   MOVEL     BLPNFV        V1CNFV
184600940906     C                   MOVEL     BLPNRT        V1CNRT
184700940906     C                   MOVEL     *BLANKS       V1DFPP
184800940906     C     BLPFPP        IFEQ      'M'
184900010220     C                   MOVEL     'MAT'         V1DFPP
185000940906     C                   ELSE
185100010220     C                   MOVEL     'POM'         V1DFPP
185200940906     C                   ENDIF
185300910118     C* GIRO DATA SPEDIZIONE
185400940804     C                   MOVEL     BLPAAS        V1CDSP
185500940804     C                   MOVE      BLPMGS        V1CDSP
185600940804     C                   Z-ADD     V1CDSP        G02INV
185700940804     C                   Z-ADD     *ZEROS        G02DAT
185800940804     C                   MOVEL     '3'           G02ERR
185900940804     C                   CALL      'XSRDA8'
186000940804     C                   PARM                    WLBDAT
186100940804     C                   Z-ADD     G02DAT        V1CDSP
186200940808     C* DATA RITIRO MERCE
186300940804     C                   Z-ADD     BLPDRT        G02INV
186400940804     C                   Z-ADD     *ZEROS        G02DAT
186500940804     C                   MOVEL     '3'           G02ERR
186600940804     C                   CALL      'XSRDA8'
186700940804     C                   PARM                    WLBDAT
186800940804     C                   Z-ADD     G02DAT        V1CDRT
186900900518     C* DATA BORDERO'
187000940804     C                   Z-ADD     BLPDBR        G02INV
187100940804     C                   Z-ADD     *ZEROS        G02DAT
187200940804     C                   MOVEL     '3'           G02ERR
187300940804     C                   CALL      'XSRDA8'
187400940804     C                   PARM                    WLBDAT
187500940804     C                   Z-ADD     G02DAT        V1CDBR
187600940804     C* DATA PARTENZA PRIMO COLLO
187700940804     C                   Z-ADD     BLPDPC        G02INV
187800940804     C                   Z-ADD     *ZEROS        G02DAT
187900940804     C                   MOVEL     '3'           G02ERR
188000940804     C                   CALL      'XSRDA8'
188100940804     C                   PARM                    WLBDAT
188200940804     C                   Z-ADD     G02DAT        V1CDPC
188300940804     C* DATA PARTENZA ULTIMO COLLO
188400940804     C                   Z-ADD     BLPDUC        G02INV
188500940804     C                   Z-ADD     *ZEROS        G02DAT
188600940804     C                   MOVEL     '3'           G02ERR
188700940804     C                   CALL      'XSRDA8'
188800940804     C                   PARM                    WLBDAT
188900940804     C                   Z-ADD     G02DAT        V1CDUC
189000910806     C* DATA SPUNTA ENTRATA
189100940808     C     BLPDSE        IFGT      *ZERO
189200940808     C                   Z-ADD     BLPDSE        G02INV
189300940808     C                   Z-ADD     *ZEROS        G02DAT
189400940808     C                   MOVEL     '3'           G02ERR
189500940808     C                   CALL      'XSRDA8'
189600940808     C                   PARM                    WLBDAT
189700940808     C                   Z-ADD     G02DAT        V1CDSE
189800940808     C                   ENDIF
189900940805     C* CONSEGNA PARTICOLARE 1
190000940805     C                   MOVEL     BLPTC1        V3CTC1
190100940805     C                   MOVEL     *BLANKS       V3DTC1
190200940805     C     V3CTC1        IFNE      *BLANKS
190300940804     C                   MOVEL     '1P'          KCOD
190400940804     C                   MOVEL     *BLANKS       KKEY
190500940805     C                   MOVEL     BLPTC1        KKEY
190600940809     C     KTAB1         CHAIN     TABEL00F                           99
190700940805     C  N99              MOVEL     TBLUNI        V3DTC1
190800940805     C                   ENDIF
190900940805     C* CONSEGNA PARTICOLARE 2
191000940805     C                   MOVE      BLPTC2        V3CTC2
191100940805     C                   MOVEL     *BLANKS       V3DTC2
191200940805     C     V3CTC2        IFNE      *BLANKS
191300940805     C                   MOVEL     '1P'          KCOD
191400940805     C                   MOVEL     *BLANKS       KKEY
191500940805     C                   MOVEL     BLPTC2        KKEY
191600940809     C     KTAB1         CHAIN     TABEL00F                           99
191700940805     C  N99              MOVEL     TBLUNI        V3DTC2
191800940805     C                   ENDIF
191900910118     C* CODICE PADRONCINO
192000940808     C                   MOVEL     BLPPDR        V1CPDR
192100021203     C                   movel     BlpPdr        kpdpdr
192200021203     C                   move      'A'           kpdtip
192300021203     C     kfapd         chain     fiapd01L                           31
192400940808     C  N31              MOVEL     APDRSC        V1DPDR
192500940808     C   31              MOVEL     *BLANKS       V1DPDR
192600910118     C*
192700900511     C                   MOVEL     BLPSOP        VIDSOP
192800930614     C*
192900051124     c* Decodifico consegna richiesta
193000940808     C                   MOVEL     *BLANKS       V2CTCR
193100051124     c                   clear                   v2chcr
193200051124     c                   clear                   v2cdcr
193300051124     c                   if        blpdcr>0 or blphcr>0
193400051124     c                   clear                   fnlv80ds
193500051124     c                   eval      ilv80aas=blpaas
193600051124     c                   eval      ilv80lnp=blplnp
193700051124     c                   eval      ilv80nrs=blpnrs
193800051124     c                   eval      ilv80nsp=blpnsp
193900051124     c                   eval      ilv80mgs=blpmgs
194000051124     c                   eval      ilv80tc1=blptc1
194100051124     c                   eval      ilv80tc2=blptc2
194200051124     c                   eval      ilv80dcr=blpdcr
194300051124     c                   eval      ilv80hcr=blphcr
194400051124     c                   eval      ilv80tcr=blptcr
194500051124     c                   call      'FNLV80R'
194600051124     c                   parm                    kpjba
194700051124     c                   parm                    fnlv80ds
194800051124     c
194900051124     c                   eval      v2cdcr=olv80dcr8
195000051124     c                   eval      v2ctcr=olv80tcrd
195100051124     C                   MOVEL     blpHCR        V2cHCR
195200051124     c                   endif
195300940906     C*-------------------------------------------------------------------------
195400940906     C*
195500940906     C* L E G A M E   B O L L A
195600940906     C                   EXSR      LEGBOL
195700940808     C*-------------------------------------------------------------------------
195800050714     c* Dei seguenti dati,
195900050714     c* Prevalgono quelli di ARB se c'e' e se non presenti legami bolla
196000050714     C* DATA ENTRATA AL TRANSITO
196100050714     c                   if        wesarb='S' and not *in07
196200050714     c                   eval      wdet=arbdet
196300050714     c                   eval      wdut=arbdut
196400050714     c                   else
196500050714     c                   eval      wdet=blpdet
196600050714     c                   eval      wdut=blpdut
196700050714     c                   endif
196800050714     c
196900050714     C     wdet          IFGT      *ZERO
197000050714     C                   Z-ADD     wdet          G02INV
197100050714     C                   Z-ADD     *ZEROS        G02DAT
197200050714     C                   MOVEL     '3'           G02ERR
197300050714     C                   CALL      'XSRDA8'
197400050714     C                   PARM                    WLBDAT
197500050714     C                   Z-ADD     G02DAT        V1CDET
197600050714     C                   ENDIF
197700050714     C* DATA USCITA DAL TRANSITO
197800050714     C     wdut          IFGT      *ZERO
197900050714     C                   Z-ADD     wdut          G02INV
198000050714     C                   Z-ADD     *ZEROS        G02DAT
198100050714     C                   MOVEL     '3'           G02ERR
198200050714     C                   CALL      'XSRDA8'
198300050714     C                   PARM                    WLBDAT
198400050714     C                   Z-ADD     G02DAT        V1CDUT
198500050714     C                   ENDIF
198600050714     c* Mancata consegna
198700050714     c                   if        wesarb='S' and not *in07
198800050714     c                   eval      wtmc=arbtmc
198900050714     c                   else
199000050714     c                   eval      wtmc=blptmc
199100050714     c                   endif
199200050714     c
199300050714     C                   MOVEL     wtmc          V3CTMC
199400050714     C                   MOVEL     *BLANKS       V3DTMC
199500050714     C     wtmc          IFNE      ' '
199600050714     C                   MOVEL     '2A'          KCOD
199700050714     C                   MOVEL     *BLANKS       KKEY
199800050714     C                   MOVEL     wtmc          KKEY
199900050714     C     KTAB1         CHAIN     TABEL00F                           99
200000050714     C  N99              MOVEL     TBLUNI        V3DTMC
200100050714     C                   ENDIF
200200050714     c* Numero e data distinta consegna
200300050714     c                   if        wesarb='S'  and not *in07
200400050714     c                   eval      wndc=arbndc
200500050714     c                   eval      wddc=arbddc
200600050714     c                   else
200700050714     c                   eval      wndc=blpndc
200800050714     c                   eval      wddc=blpddc
200900050714     c                   endif
201000050714     C*
201100050714     C                   MOVEL     wndc          V3CNDC
201200050714     C                   Z-ADD     wddc          G02INV
201300050714     C                   Z-ADD     *ZEROS        G02DAT
201400050714     C                   MOVEL     '3'           G02ERR
201500050714     C                   CALL      'XSRDA8'
201600050714     C                   PARM                    WLBDAT
201700050714     C                   Z-ADD     G02DAT        V3CDDC
201800050714     C* DATA ARRIVO MERCE ALL FILIALE DI ARRIVO
201900050714     c                   clear                   wdam
202000050714     c                   if        wesarb='S' and not *in07
202100050714     c                   eval      wdam=arbdam
202200050714     c                   else
202300050714     c                   eval      wdam=blpdam
202400050714     c                   endif
202500050714     c
202600050714     C     wdam          IFGT      *ZERO
202700050714     C                   Z-ADD     wdam          G02INV
202800050714     C                   Z-ADD     *ZEROS        G02DAT
202900050714     C                   MOVEL     '3'           G02ERR
203000050714     C                   CALL      'XSRDA8'
203100050714     C                   PARM                    WLBDAT
203200050714     C                   Z-ADD     G02DAT        V1CDAM
203300050714     C                   ENDIF
203400050714     c
203500050714     C* DATA CONSEGNA MERCE REALE
203600050714     C                   Z-ADD     *ZERO         V1CDCM
203700050714     c                   if        wesarb='S' and not *in07
203800050714     c                   eval      wdcm=arbdcm
203900050714     c                   eval      wdce=arbdce
204000050714     c                   eval      wnrc=arbnrc
204100050714     c                   eval      wdcp=arbdcp
204200050714     c                   eval      wcca=arbcca
204300050714     c                   eval      whmc=arbhmc
204400050714     c                   else
204500050714     c                   eval      wdcm=blpdcm
204600050714     c                   eval      wdce=blpdce
204700050714     c                   eval      wnrc=blpnrc
204800050714     c                   eval      wdcp=blpdcp
204900050714     c                   eval      wcca=blpcca
205000050714     c                   eval      whmc=blphmc
205100050714     c                   endif
205200050714     c
205300050714     C     wdcm          IFGT      *ZERO
205400050714     C                   SETON                                        98
205500050714     C                   Z-ADD     wdcm          G02INV
205600050714     C                   Z-ADD     *ZEROS        G02DAT
205700050714     C                   MOVEL     '3'           G02ERR
205800050714     C                   CALL      'XSRDA8'
205900050714     C                   PARM                    WLBDAT
206000050714     C                   Z-ADD     G02DAT        V1CDCM
206100050714     C                   MOVEL     whmc          V1CHMC
206200050714     C                   ENDIF
206300050714     C* DATA CONSEGNA PREVISTA ESTERNA E ORE DI RITARDO
206400050714     C                   Z-ADD     0             V1CDCE
206500050714     C                   Z-ADD     0             V1CNRC
206600050714     C     wdce          IFEQ      0
206700050714     C                   SETON                                        94
206800050714     C                   ELSE
206900050714     C                   Z-ADD     wdce          G02INV
207000050714     C                   Z-ADD     *ZEROS        G02DAT
207100050714     C                   MOVEL     '3'           G02ERR
207200050714     C                   CALL      'XSRDA8'
207300050714     C                   PARM                    WLBDAT
207400050714     C                   Z-ADD     G02DAT        V1CDCE
207500050714     C*
207600050714     C                   Z-ADD     wnrc          V1CNRC
207700050714     C                   END
207800050714     C*
207900050714     C* DATA PRIMA CONSEGNA PARZIALE
208000050714     C     wdcp          IFGT      0
208100050714     C                   Z-ADD     wdcp          G02INV
208200050714     C                   Z-ADD     *ZEROS        G02DAT
208300050714     C                   MOVEL     '3'           G02ERR
208400050714     C                   CALL      'XSRDA8'
208500050714     C                   PARM                    WLBDAT
208600050714     C                   Z-ADD     G02DAT        V3CDCP
208700050714     C                   END
208800050714     C* CODICE CONSEGNA ANOMALA
208900050714     C                   MOVEL     wcca          V3CCCA
209000050714     C                   MOVE      *BLANKS       V3DCCA
209100050714     C                   MOVE      *BLANKS       V7DCCA
209200050714     C     wcca          IFNE      ' '
209300050714     C                   MOVEL     '7O'          KCOD
209400050714     C                   MOVEL     *BLANKS       KKEY
209500050714     C                   MOVEL     wcca          KKEY
209600050714     C     KTAB1         CHAIN     TABEL00F                           99
209700050714     C  N99              MOVEL     TBLUNI        V3DCCA
209800050714     C  N99              MOVEL     TBLUNI        V7DCCA
209900050714     C                   END
210000050714     c*******
210100991022      *   SE ESISTE AGGANCIO  2 °   B O L L A
210200991022      *
210300920409     C     §3ATB2        IFNE      '  '
210400991022      *
210500991022     c                   eval      KTRC = '2'
210600991022     C     KAR6          CHAIN     FIAR6000
210700991022      *
210800991022     C                   IF        %FOUND
210900991022     C                   eval        *IN06 = *ON
211000991022     C                   eval      VI2fiv = AR6fiv
211100991022     C                   eval      VI2nft = AR6nft
211200991022     C                   eval      VI2cei = AR6cei
211300991022      *
211400991022     C                   eval      VI2DIV = AR6div
211500991022     C                   eval      WDIV = AR6div
211600991022     C                   exsr      CTR_VALUTA
211700991022      *
211800991027     c                   if        AR6fim = 'P'
211900991027     c                   eval      VI2fim = 'PARTENZA'
212000040116     c                   eval      VI2fi2 = 'Immesse in'
212100991022     c                   endif
212200991022      * Varia G
212300040116     c**!!!              clear                   W0133
212400040116     c**!!!              SELECT
212500040116     c**!!!              WHEN        AR6sv1 = §$2vrg
212600040116     C**!!!              eval      VI2sv1 = AR6sv1
212700040116     c**!!!              z-add     AR6va1        W0133
212800040116     c**!!!              WHEN        AR6sv2 = §$2vrg
212900040116     C**!!!              eval      VI2sv1 = AR6sv2
213000040116     c**!!!              z-add     AR6va2        W0133
213100040116     c**!!!              WHEN        AR6sv3 = §$2vrg
213200040116     C**!!!              eval      VI2sv1 = AR6sv3
213300040116     c**!!!              z-add     AR6va3        W0133
213400040116     c**!!!              ENDSL
213500991022      *
213600040116     c**!!!              if        W0133 > *zeros
213700040116     c**!!!              eval      Wtstdec = §cvfdc
213800040116     c**!!!              exsr      EDTimporti
213900040116     c**!!!              movel     IMPeditato    VI2va1
214000040116     c**!!!              endif
214100991022      * IVA
214200040116     c**!!!              clear                   W0133
214300040116     c**!!!              SELECT
214400040116     c**!!!              WHEN        AR6sv1 = §$2iva
214500040116     c**!!!              z-add     AR6va1        W0133
214600040116     c**!!!              WHEN        AR6sv2 = §$2iva
214700040116     c**!!!              z-add     AR6va2        W0133
214800040116     c**!!!              WHEN        AR6sv3 = §$2iva
214900040116     c**!!!              z-add     AR6va3        W0133
215000040116     c**!!!              ENDSL
215100991022      *
215200040116     c**!!!              IF        W0133 > *zeros
215300040116     c**!!!              eval      *in11 = *off
215400040116     c**!!!              eval      Wtstdec = §cvfdc
215500040116     c**!!!              exsr      EDTimporti
215600040116     c**!!!              movel     IMPeditato    VI2iva
215700040116     c**!!!              ELSE
215800991123      * BOLLO se non ho iva
215900040116     c**!!!              SELECT
216000040116     c**!!!              WHEN        AR6sv1 = §$2bol
216100040116     c**!!!              z-add     AR6va1        W0133
216200040116     c**!!!              WHEN        AR6sv2 = §$2bol
216300040116     c**!!!              z-add     AR6va2        W0133
216400040116     c**!!!              WHEN        AR6sv3 = §$2bol
216500040116     c**!!!              z-add     AR6va3        W0133
216600040116     c**!!!              ENDSL
216700991123      *
216800040116     c**!!!              IF        W0133 > *zeros
216900040116     c**!!!              eval      *in11 = *on
217000040116     c**!!!              eval      Wtstdec = §cvfdc
217100040116     c**!!!              exsr      EDTimporti
217200040116     c**!!!              movel     IMPeditato    VI2iva
217300040116     c**!!!              ENDIF
217400991123      *
217500040116     c**!!!              ENDIF
217600040116
217700040116      * Carico le varie
217800040116     c                   Movel     Ar6Sv1        wsvn
217900040116     c                   Z-add     Ar6Va1        wvan
218000040116     c                   ExSr      Car_Varie1
218100040116
218200040116     c                   Movel     Ar6Sv2        wsvn
218300040116     c                   Z-add     Ar6Va2        wvan
218400040116     c                   ExSr      Car_Varie1
218500040116
218600040116     c                   Movel     Ar6Sv3        wsvn
218700040116     c                   Z-add     Ar6Va3        wvan
218800040116     c                   ExSr      Car_Varie1
218900040116
219000040116     c     Kar6          Setll     Fiar7000
219100040116     c                   Do        *Hival
219200040116     c     Kar6          Reade     Fiar7000
219300040116     c                   If        %Eof(Fiar701l)
219400040116     c                   Leave
219500040116     c                   EndIf
219600040116     c                   Movel     Ar7Svn        wsvn
219700040116     c                   Z-add     Ar7Van        wvan
219800040116     c                   ExSr      Car_Varie1
219900040116     c                   EndDo
220000040116
220100991025      * TOTALE FATTURA
220200991025     c                   clear                   W0133
220300991025     c                   IF          AR6ift > *zeros
220400991025     c                   z-add     AR6ift        W0133
220500991025     c                   eval      Wtstdec = §cvfdc
220600991025     c                   exsr      EDTimporti
220700991025     c                   movel     IMPeditato    VI2ift
220800991025     c                   endif
220900991022      *
221000991022     c                   If         AR6dft > *zeros
221100991022     C     *iso          movel     AR6DFT        DATA_EUR
221200991022     C     *eur          movel     data_eur      W0080
221300991022     C                   eval      VI2dft = %EDITC(W0080:'Y')
221400991022     c                   Endif
221500991022      *  decodifica esenzione IVA
221600991022     C                   if        VI2CEI <> *blanks
221700940808     C                   MOVEL     'EI'          KCOD
221800991022     C                   MOVEL(P)  VI2CEI        KKEY
221900940809     C     KTAB1         CHAIN     TABEL00F                           31
222000991022     C  N31              MOVEL     TBLUNI        VI2DCE
222100940808     C                   ENDIF
222200991022      *
222300991022     c                   eval      VI2ksc = AR6ksc
222400991022     c                   eval      VI2dks = BLPrsd
222500991022     c                   eval      VI2ctr = AR6ctr
222600991022      *
222700991022     C                   eval      KCOD = 'TR'
222800991022     C                   movel     VI2CTR        UNOCTR
222900991022     C                   eval      KKEY = unoctr
223000991022     C     KTAB1         CHAIN     TABEL00F                           31
223100991022     C                   movel     TBLUNI        VI2dct
223200991022      *
223300991022     C                   ENDIF
223400940808     C                   ENDIF
223500951219     C*-------------------------------------------------------------------------
223600951219     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
223700951219     C*
223800951219     C* PULISCO IL SFL
223900951219     C                   SETON                                        26
224000951219     C                   WRITE     LS05C03
224100951219     C                   SETOFF                                       26
224200951219     C*
224300060216     C* CHIAVE PER ACCEDERE AL FILE Fiar401L
224400951219     C                   MOVEL     BLPAAS        KAAS
224500951219     C                   MOVEL     BLPLNP        KLNP
224600951219     C                   MOVEL     BLPNRS        KNRS
224700951219     C                   MOVEL     BLPNSP        KNSP
224800951219     C*
224900951219     C                   Z-ADD     0             NRR3              4 0
225000951219     C                   MOVEL     '7K'          KCOD
225100951219     C*
225200060216     C     Kar42         SETLL     Fiar401L
225300060216     C     Kar42         READE     Fiar401L                               99
225400951219     C     *IN99         DOWEQ     *OFF
225500951219     C*
225600060216     C                   MOVEL(P)  ar4TRC        KKEY
225700951219     C     KTAB1         CHAIN     TABEL                              99
225800951219     C*
225900951219     C     *IN99         IFEQ      *OFF
226000951219     C     TBLFLG        ANDEQ     ' '
226100951219     C                   MOVEL     TBLUNI        DS7K
226200951219     C     §7KVIS        IFEQ      'S'
226300951219     C                   MOVEL     §7KDES        VS3DES
226400060216     C                   MOVEL     ar4NOT        VS3NOT
226500951219     C                   ADD       1             NRR3
226600951219     C                   WRITE     LS05S03
226700951219     C                   ENDIF
226800951219     C                   ENDIF
226900951219     C*
227000060216     C     Kar42         READE     Fiar401L                               99
227100951219     C                   ENDDO
227200951219     C*
227300951219     C* SE NON HO CARICATO NIENTE SCRIVO UGUALMENTE UN RECORD VUOTO
227400951219     C*  NEL SFL PER EVITARE CHE IL PGM DIA ERRORE DI SESSIONE O UNITA'
227500951219     C     NRR3          IFEQ      0
227600951219     C                   ADD       1             NRR3
227700951219     C                   WRITE     LS05S03
227800951219     C                   ENDIF
227900951219     C*
228000940808     C*-------------------------------------------------------------------------
228100930526     C* 95 ON - ESITONO EVENTI DA INTERROGARE
228200940804     C     PA0CLI        IFNE      'S'
228300991022     C     KBLP3         SETLL     FNEVB000                               95
228400940802     C                   ENDIF
228500940805     C*-------------------------------------------------------------------------
228600991022      * PER I DATI SULLA TASSAZIONE, CERCO SU FIAR600F.
228700991022      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SOLO SE IMPOSTATO ALMENO UN CAMPO
228800991022     C                   clear                   SAVNR8
228900991022     C                   eval      *in04 = *off
229000991022     C                   eval      Ktrc = '1'
229100991022     C     KAR6          CHAIN     FIAR601L
229200991022      *
229300991022     C                   IF        %FOUND  and
229400991022     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
229500991022     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
229600991022     C                              AR6sv3 <> *blanks)
229700991022      *
229800991022     C                   eval      *in04 = *on
229900991022      *
230000991025     C                   eval      V8CDIV = AR6div
230100991022     C                   eval      WDIV = AR6div
230200991022     C                   exsr      CTR_VALUTA
230300991022      *
230400991022     c                   IF          ar6por > *zeros
230500991022     c                   z-add     AR6por        W0133
230600991022     c                   eval      Wtstdec = §cvfdc
230700991022     c                   exsr      EDTimporti
230800991022     c                   movel     IMPeditato    V8Cpor
230900991022     c                   ENDIF
231000991022      *
231100991022     c                   IF          ar6imv > *zeros
231200991022     c                   z-add     AR6imv        W0133
231300991022     c                   eval      Wtstdec = §cvfdc
231400991022     c                   exsr      EDTimporti
231500991022     c                   movel     IMPeditato    V8Cimv
231600991022     c                   ENDIF
231700991022      *
231800991022     c                   IF          ar6ift > *zeros
231900991022     c                   z-add     AR6ift        W0133
232000991022     c                   eval      Wtstdec = §cvfdc
232100991022     c                   exsr      EDTimporti
232200991022     c                   movel     IMPeditato    V8Cift
232300991022     c                   ENDIF
232400991022      *
232500991027     c                   IF        AR6fim = 'P'
232600991022     c                   eval      V8Dfim = 'PARTENZA'
232700991022    2C                   ENDIF
232800991022      *
232900991022     C                   MOVEL     AR6CEI        V8CCEI
233000991022      *
233100991022      * SE C'E', DECODIFICO L'ESENZIONE IVA
233200991022     C                   clear                   V8DCEI
233300991022     C                   IF        AR6CEI <>  *BLANKS
233400991022     C                   MOVEL     'EI'          KCOD
233500991022     C                   MOVEL(P)  AR6CEI        KKEY
233600991022     C     KTAB1         CHAIN     TABEL00F                           99
233700991022     C  N99              MOVEL     TBLUNI        V8DCEI
233800991022    2C                   ENDIF
233900991022      *
234000991022      * DATA FATTURA DA GIRARE
234100991022     C                   IF         ar6dft > *zeros
234200991022     C     *iso          movel     AR6DFT        DATA_EUR
234300991022     C     *eur          movel     data_eur      W0080
234400991022     C                   eval      V8Cdft = %EDITC(W0080:'Y')
234500991022     C                   ENDIF
234600991022     C                   MOVEL     ar6NFT        V8CNFT
234700991022     C                   MOVEL     ar6FIV        V8CFIV
234800991025      * CLIENTE E TARIFFA FATTURAZIONE
234900991025     c                   IF        *IN33 = *ON
235000991025     c                   eval      v8cksc = blpksc
235100991025     c                   eval      v8cctr = blpctr
235200991025     c                   eval      v8dksc = blprsm
235300991025     c                   ELSE
235400991025     c                   eval      v8cksc = AR6ksc
235500991025     c                   eval      v8cctr = AR6ctr
235600991025     c                   move      v8cksc        W0040
235700991025     c                   if        W0040 = 9999
235800991027     c                   eval      v8dksc = blprsd
235900991025     c                   else
236000991025     c                   eval      i69kac = v8cksc
236100991025     c                   exsr      CTR_ANAGRA
236200991025     c                   eval      v8dksc = acorag
236300991025     c                   endif
236400991025     c                   ENDIF
236500991025      *   decodifica tariffa
236600991025     C                   eval      KCOD = 'TR'
236700991025     C                   MOVEL     V2CCTR        UNOCTR            1
236800991025     C                   MOVEL(P)  UNOCTR        KKEY
236900991025     C     KTAB1         CHAIN     TABEL00F                           31
237000991025     C                   MOVEL     TBLUNI        V8DCTR
237100991022      *
237200991022      * CERCO LE VARIE
237300991022     C                   eval      *in20 = *off
237400991022     C                   eval      *in21 = *off
237500991022     C                   WRITE     LS05C08
237600991022     C                   eval      *in20 = *on
237700991022     C                   eval      *in21 = *on
237800991022      *
237900991022     c                   clear                   nrr8
238000991022      *
238100991022     C                   MOVEL     ar6SV1        WSVN
238200991022     C                   MOVEL     ar6VA1        WVAN
238300991022     c                   exsr      car_varie
238400991022      *
238500991022     C                   MOVEL     ar6SV2        WSVN
238600991022     C                   MOVEL     ar6VA2        WVAN
238700991022     c                   exsr      car_varie
238800991022      *
238900991022     C                   MOVEL     ar6SV3        WSVN
239000991022     C                   MOVEL     ar6VA3        WVAN
239100991022     c                   exsr      car_varie
239200991022      *
239300991022      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
239400991022     C                   if          ar6SV3 <> *blanks
239500991022     C     KAR6          SETLL     FIAR7000
239600991022     C     KAR6          READE     FIAR7000
239700991022     C                   DOW       %EOF = *OFF
239800991022     C                   MOVEL     AR7SVN        WSVN
239900991022     C                   MOVEL     AR7VAN        WVAN
240000991022     c                   exsr      car_varie
240100991022     C     KAR6          READE     FIAR7000
240200991022     C                   ENDDO
240300991022     C                   endif
240400991022      *
240500991022      * Se non ho caricato record ne carico uno vuoto
240600991022     c                   IF        NRR8 = *ZEROS
240700991022     C                   clear                   V8CDES
240800991022     C                   clear                   V8CVA1
240900991022     C                   clear                   V8CSV1
241000991022     C                   add       1             NRR8
241100991022     C                   WRITE     LS05S08
241200991022     c                   ENDIF
241300991022      *
241400991022     C                   Z-ADD     NRR8          SAVNR8
241500991022     C                   Z-ADD     1             NRR8
241600991022      *
241700991022     C                   ENDIF
241800991022      *-------------------------------------------------------------------------
241900991022      * DECODIFICO IL CODICE TASSAZIONE, SE C'E'
242000991022     C                   clear                   V8DCTS
242100991022     C                   if         V8CCTS <> *blanks
242200991022     C                   MOVEL     'CT'          KCOD
242300991022     C                   MOVEL(P)  V8CCTS        KKEY
242400991022     C     KTAB1         CHAIN     TABEL00F                           99
242500991022     C  N99              MOVEL     TBLUNI        V8DCTS
242600991022     C                   endif
242700940805     C*-------------------------------------------------------------------------
242800940805     C* PRELEVO LE NOTE DAL FILE APPOSITO
242900940805     C                   MOVEL     BLPAAS        KAAS
243000940805     C                   MOVEL     BLPLNP        KLNP
243100940805     C                   MOVEL     BLPNRS        KNRS
243200940805     C                   MOVEL     BLPNSP        KNSP
243300940805     C                   MOVEL     '8'           KTRC
243400060216     C     Kar41         CHAIN     Fiar401L                           99
243500060216     C  N99              MOVEL     ar4NOT        V2CNO1
243600940805     C   99              MOVEL     *BLANKS       V2CNO1
243700940805     C                   MOVEL     '9'           KTRC
243800060216     C     Kar41         CHAIN     Fiar401L                           99
243900060216     C  N99              MOVEL     ar4NOT        V2CNO2
244000940805     C   99              MOVEL     *BLANKS       V2CNO2
244100991022      *
244200991022      *-------------------------------------------------------------------------
244300051115      * IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU Fiar900F
244400991022      * CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
244500940805     C                   MOVEL     *BLANKS       V2CTIC
244600940805     C                   MOVEL     *ZERO         V2CCAS
244700940805     C                   MOVEL     *BLANKS       V2CVCA
244800940805     C                   MOVEL     *BLANKS       V2CGCA
244900940923     C                   MOVEL     *BLANKS       V2IGCA
245000940805     C     §3AFCA        IFGT      *ZERO
245100051115     C     KBLP3         CHAIN     Fiar901L                           99
245200940805     C  N99              DO
245300051115     C                   MOVEL     ar9TIC        V2CTIC
245400051115     C                   MOVEL     ar9CAS        V2CCAS
245500051115     C                   MOVEL     ar9VCA        V2CVCA
245600051115     C                   MOVEL     ar9GCA        V2CGCA
245700940805     C                   ENDDO
245800940805     C                   ENDIF
245900991022      * SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
246000940923     C     V2CGCA        COMP      *BLANKS                            1616
246100021218      *
246200021218      * Riempio il campone in fondo alla videata con i tasti scelti
246300021218     C                   exsr      Ges_TastiFun
246400021218     c                   eval      FD07 = FDxx
246500021218     c                   eval      F24D07 = F24Dxx
246600991022      *
246700940923     C     ENDBLP        ENDSR
246800030703      *-------------------------------------------------------------------------
246900030703      *  Decoditifa tipo bancali
247000030703      *-------------------------------------------------------------------------
247100030703     c     Sr_DecTba     BegSr
247200030703     c                   Clear                   Tibs02Ds
247300030703     c                   Clear                   dTba
247400030703     c                   Movel     'C'           T02Mod
247500030703     c                   Movel     Knsif         T02Sif
247600030703     c                   Movel     'TBA'         T02Cod
247700030703     c                   Movel(p)  wtba          T02Ke1
247800030703     c                   Call      'TIBS02R'
247900030703     c                   Parm                    Kpjba
248000030703     c                   Parm                    Tibs02Ds
248100030703If  3c                   If        T02Err = *Blanks
248200030703     c                   Movel     T02Uni        dTba
248300030703    3c                   EndIf
248400030703     c                   Eval      wdestba = §TbaDesc
248500030703     c                   EndSr
248600021218      *-------------------------------------------------------------------------
248700021218      *  Imposto descrizione tasti funzione
248800021218      *-------------------------------------------------------------------------
248900021218     c     Ges_TastiFun  BEGSR
249000021218      *
249100021218      * per emettere i tasti senza condizionamenti ..
249200021218     c                   setoff                                       25
249300021218      *
249400021218      * Conta i caratteri riempiti dalle RigaTf1
249500021218     c                   z-add     0             $z                3 0
249600021218      *
249700021218      * Conta i caratteri riempiti dalle RigaTf2
249800021218     c                   z-add     0             $k                3 0
249900021218      *
250000021218      * Conta i caratteri riempiti dalle RigaTf3
250100021218     c                   z-add     0             $j                3 0
250200021218      *
250300021218      * Conta la posizione del campone da cui partire per impostare
250400021218      * la descrizione del tasto funzione
250500021218     c                   z-add     0             $y                2 0
250600021218      *
250700021218      * Stringhe che contengono le descrizioni dei tasti funzione
250800021218     c                   reset                   RigaTf1
250900021218     c                   reset                   RigaTf2
251000021218     c                   reset                   RigaTf3
251100021218      *
251200021218      * F13
251300021218     c                   IF        *in04 = *on
251400021218     c                   reset                   $Tf
251500021218     c                   movea     Tf13          $Tf
251600021218     c                   Exsr      Rie_$Tf
251700021218     C                   ENDIF
251800021218      *
251900021218      * F04
252000021218     c                   IF        *in06 = *on
252100021218     c                   reset                   $Tf
252200021218     c                   movea     Tf04          $Tf
252300021218     c                   Exsr      Rie_$Tf
252400021218     C                   ENDIF
252500040220      *
252600040220      *?F16?
252700040220     c                   IF        *in28 = *on
252800040220     c                   reset                   $Tf
252900040220     c                   movea     Tf16          $Tf
253000040220     c                   exsr      Rie_$Tf
253100040220     c                   ENDIF
253200021218      *
253300021218      * F05
253400021218     c                   IF        *in95 = *on
253500021218     c                   reset                   $Tf
253600021218     c                   movea     Tf05          $Tf
253700021218     c                   Exsr      Rie_$Tf
253800021218     C                   ENDIF
253900021218      *
254000021218      * F10
254100021218     c                   IF        *in10 = *on
254200021218     c                   reset                   $Tf
254300021218     c                   movea     Tf10          $Tf
254400021218     c                   Exsr      Rie_$Tf
254500021218     c                   ENDIF
254600021218      *
254700021218      * F14
254800021218     c                   IF        *in03 = *on
254900021218     c                   reset                   $Tf
255000021218     c                   movea     Tf14          $Tf
255100021218     c                   Exsr      Rie_$Tf
255200021218     c                   ENDIF
255300051201      *
255400051201      * F22
255500051201     c                   IF        *in24 = *on
255600051201     c                   reset                   $Tf
255700051201     c                   movea     Tf22          $Tf
255800051201     c                   Exsr      Rie_$Tf
255900051201     c                   ENDIF
256000021218      *
256100021218      * F02
256200021218     c                   IF        *in12 = *on
256300021218     c                   reset                   $Tf
256400021218     c                   movea     Tf02          $Tf
256500021218     c                   Exsr      Rie_$Tf
256600021218     c                   ENDIF
256700021218      *
256800021218      * F11
256900021218     c                   IF        *in77 = *on
257000021218     c                   reset                   $Tf
257100021218     c                   movea     Tf11          $Tf
257200021218     c                   Exsr      Rie_$Tf
257300021218     c                   ENDIF
257400021218      *
257500021218      * F07
257600021218     c                   IF        *in22 = *on and *in05 = *off
257700021218     c                   reset                   $Tf
257800021218     c                   movea     Tf07          $Tf
257900021218     c                   Exsr      Rie_$Tf
258000021218     c                   ENDIF
258100021218      *
258200021218      * F08
258300021218     c                   IF        *in55 = *on
258400021218     c                   reset                   $Tf
258500021218     c                   movea     Tf08          $Tf
258600021218     c                   Exsr      Rie_$Tf
258700021218     c                   ENDIF
258800021218      *
258900021218      * F20
259000021218     c                   IF        *in22 = *off
259100021218     c                   reset                   $Tf
259200021218     c                   movea     Tf20          $Tf
259300021218     c                   Exsr      Rie_$Tf
259400021218     c                   ENDIF
259500021218      *
259600021218      * F21 --> non condizionato
259700021218     c                   IF        *in25 = *off
259800021218     c                   reset                   $Tf
259900021218     c                   movea     Tf21          $Tf
260000021218     c                   Exsr      Rie_$Tf
260100021218     c                   ENDIF
260200040220      *
260300040220      * F19
260400040220     c                   IF        *in22 = *off
260500040220     c                   reset                   $Tf
260600040220     c                   movea     Tf19          $Tf
260700040220     c                   Exsr      Rie_$Tf
260800040220     c                   ENDIF
260900040220      *
261000040220      *?F01?
261100040220     c                   IF        *in27 = *on
261200040220     c                   reset                   $Tf
261300040220     c                   movea     Tf01          $Tf
261400040220     c                   exsr      Rie_$Tf
261500040220     c                   ENDIF
261600021218      *
261700021218      * F12 --> non condizionato
261800021218     c                   IF        *in25 = *off
261900021218     c                   reset                   $Tf
262000021218     c                   movea     Tf12          $Tf
262100021218     c                   Exsr      Rie_$Tf
262200021218     c                   ENDIF
262300021218      *
262400021218      * Pulisco le stringa
262500021218     c                   z-add     1             $h
262600030121     c     1             do        62            $h                2 0
262700021218     C                   if        RigaTf1($h) = '#'
262800021218     c                   movel     *blanks       RigaTf1($h)
262900021218     c                   endif
263000021218     c                   enddo
263100021218     c                   z-add     1             $h
263200030121     c     1             do        62            $h                2 0
263300021218     C                   if        RigaTf2($h) = '#'
263400021218     c                   movel     *blanks       RigaTf2($h)
263500021218     c                   endif
263600021218     c                   enddo
263700021218     c                   z-add     1             $h
263800030121     c     1             do        62            $h                2 0
263900021218     C                   if        RigaTf3($h) = '#'
264000021218     c                   movel     *blanks       RigaTf3($h)
264100021218     c                   endif
264200021218     c                   enddo
264300021218      *
264400021218      * Imposto la descrizione del tasto funzione F24 e segnalo
264500021218      * quante righe ho riempito e quale devo emettere
264600021218     c                   SELECT
264700021218      *
264800021218     c                   when      $k<>*zeros and $Y<>*zeros
264900021218     c                   eval      $loop = 3
265000021218     c                   z-add     3             $loop             1 0
265100021218     C                   movel(p)  CF2413        F24Dxx           16
265200021218      *
265300021218     c                   when      $k<>*zeros and $Y=*zeros
265400021218     c                   z-add     2             $loop             1 0
265500021218     C                   movel(p)  CF2412        F24Dxx
265600021218      *
265700021218     c                   Other
265800021218     c                   z-add     1             $loop             1 0
265900021218     C                   movel     *blanks       F24Dxx
266000021218     c                   ENDSL
266100021218      * Prima riga di tasti funzione
266200030121     c                   movea     RigaTF1       FDxx             62
266300021218     c                   z-add     1             $Rig              1 0
266400021218      *
266500021218     c                   endsr
266600021218      *-------------------------------------------------------------------------
266700021218      *  Riempio le stringhe con i campi funzione compattati
266800021218      *-------------------------------------------------------------------------
266900021218     c     Rie_$Tf       begsr
267000021218      *
267100021218     c                   z-add     1             $x                2 0
267200021218     c     '#'           lookup    $Tf($x)                                31
267300021218     c                   add       $x            $Z
267400030121     c                   IF        $Z <= 63
267500021218     c                   eval      $j = $Z - $x + 1
267600021218     c                   movea     $Tf           RigaTF1($j)
267700021218     c                   ELSE
267800021218     c                   add       $x            $K
267900030121     c                   IF        $K <= 63
268000021218     c                   eval      $j = $K - $x + 1
268100021218     c                   movea     $Tf           RigaTF2($j)
268200021218     c                   ELSE
268300021218     c                   add       $x            $Y
268400030121     c                   IF        $Y <= 63
268500021218     c                   eval      $j = $Y - $x + 1
268600021218     c                   movea     $Tf           RigaTF3($j)
268700021218     c                   ENDIF
268800021218     c                   ENDIF
268900021218     c                   ENDIF
269000021218      *
269100021218     c                   endsr
269200021218      *-------------------------------------------------------------------------
269300021218      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
269400021218      *-------------------------------------------------------------------------
269500021218     c     Ges_F24       begsr
269600021218      *
269700021218     c                   SELECT
269800021218      *
269900021218     c                   WHEN      $Loop = 2 and $Rig =2  or
270000021218     c                             $Loop = 3 and $Rig =3
270100021218     c                   movea     RigaTF1       FDxx
270200021218     c                   z-add     1             $Rig
270300021218      *
270400021218     c                   if        $loop = 2
270500021218     c                   eval      F24Dxx= Cf2412
270600021218     c                   else
270700021218     c                   eval      F24Dxx= Cf2413
270800021218     c                   endif
270900021218      *
271000021218     c                   WHEN      $Loop = 2 and $Rig =1 or
271100021218     c                             $Loop = 3 and $Rig =1
271200021218     c                   movea     RigaTF2       FDxx
271300021218     c                   z-add     2             $Rig
271400021218      *
271500021218     c                   if        $loop = 2
271600021218     c                   eval      F24Dxx= Cf2422
271700021218     c                   else
271800021218     c                   eval      F24Dxx= Cf2423
271900021218     c                   endif
272000021218      *
272100021218     c                   WHEN      $Loop = 3 and $Rig =2
272200021218     c                   movea     RigaTF3       FDxx
272300021218     c                   z-add     3             $Rig
272400021218     c                   eval      F24Dxx= Cf2433
272500021218     c                   ENDSL
272600021218      *
272700021218     c                   endsr
272800991022      *-------------------------------------------------------------------------
272900991022      *  CARICO SUBFILE VARIE
273000991022      *-------------------------------------------------------------------------
273100991022     c     CAR_VARIE     BEGSR
273200991022      *
273300991022     c                   SELECT
273400991022      *
273500991022     c                   WHEN       WSVN = *BLANKS
273600991022      *
273700991022     C                   WHEN       WSVN = §$2iva
273800991022     c                   z-add     Wvan          W0133
273900991022     c                   eval      Wtstdec = §cvfdc
274000991022     c                   exsr      EDTimporti
274100991025     c                   movel     IMPeditato    V8Civa
274200991022      *
274300991022     c                   OTHER
274400991022     c                   z-add     Wvan          W0133
274500991022     c                   eval      Wtstdec = §cvfdc
274600991022     c                   exsr      EDTimporti
274700991022     c                   movel     IMPeditato    V8Cva1
274800991022     c                   movel     WSVN          V8Csv1
274900991022     C                   Z-ADD     1             WA
275000991022     C     V8CSV1        LOOKUP    CC1(WA)                                99
275100020329     C     CC3(WA)       ifeq      'S'
275200020329      *
275300020329      * Verifico se si tratta di una bolla Fedex
275400021119     c                   if        LnaNtw = 'FED'
275500020329     C                   Z-ADD     1             WA
275600020329     C     V8CSV1        LOOKUP    CB1(WA)                                99
275700020329     C   99              MOVEL     CB2(WA)       V8CDES
275800020329     C  N99              clear                   V8CDES
275900020329     C                   else
276000991022     C   99              MOVEL     CC2(WA)       V8CDES
276100991022     C  N99              clear                   V8CDES
276200020329     C                   endif
276300020329     C                   else
276400020329     C   99              MOVEL     CC2(WA)       V8CDES
276500020329     C  N99              clear                   V8CDES
276600020329     C                   endif
276700991022     C                   add       1             NRR8
276800991022     C                   WRITE     LS05S08
276900991022      *
277000991022     c                   ENDSL
277100991022      *
277200991022     c                   ENDSR
277300040116      *-------------------------------------------------------------------------
277400040116      *  Carico le varie della seconda bolla
277500040116      *-------------------------------------------------------------------------
277600040116     c     Car_Varie1    BegSr
277700040116
277800040116s   1c                   Select
277900040116
278000040116     c                   When      wsvn = *Blanks
278100040116      * Iva
278200040116     c                   When      wsvn = §$2iva
278300040116     c                   z-add     wvan          w0133
278400040116     c                   Eval      Wtstdec = §cvfdc
278500040116     c                   ExSr      EDTimporti
278600040116     c                   Movel     IMPeditato    Vi2Iva
278700040116      * Bollo
278800040116     c                   When      wsvn = §$2bol
278900040116     c                   z-add     wvan          w0133
279000040116     c                   Eval      Wtstdec = §cvfdc
279100040116     c                   ExSr      EDTimporti
279200040116     c                   Movel     IMPeditato    Vi2Iva
279300040116      * Varia G - sempre per prima
279400040116     c                   When      wsvn = §$2vrg
279500040116     c                   Eval      Vi2Sv1 = wsvn
279600040116     c                   z-add     wvan          w0133
279700040116     c                   Eval      Wtstdec = §cvfdc
279800040116     c                   ExSr      EDTimporti
279900040116     c                   Movel     IMPeditato    Vi2Va1
280000040116      * Altre varie
280100040116   x1c                   Other
280200040116     c                   z-add     wvan          W0133
280300040116     c                   eval      Wtstdec = §cvfdc
280400040116     c                   exsr      EDTimporti
280500040116if  2c                   If        Vi2Sv2 = *Blanks
280600040116     c                   movel     IMPeditato    Vi2Va2
280700040116     c                   movel     wsvn          Vi2sv2
280800040116   x2c                   Else
280900040116if  3c                   If        Vi2Sv3 = *Blanks
281000040116     c                   movel     IMPeditato    Vi2Va3
281100040116     c                   movel     wsvn          Vi2sv3
281200040116   x3c                   Else
281300040116if  4c                   If        Vi2Sv4 = *Blanks
281400040116     c                   movel     IMPeditato    Vi2Va4
281500040116     c                   movel     wsvn          Vi2sv4
281600041130   x4c                   Else
281700041130if  5c                   If        Vi2Sv5 = *Blanks
281800041130     c                   movel     IMPeditato    Vi2Va5
281900041130     c                   movel     wsvn          Vi2sv5
282000041130   x5c                   Else
282100041130if  6c                   If        Vi2Sv6 = *Blanks
282200041130     c                   movel     IMPeditato    Vi2Va6
282300041130     c                   movel     wsvn          Vi2sv6
282400041130    6c                   EndIf
282500041130    5c                   EndIf
282600040116    4c                   EndIf
282700040116    3c                   EndIf
282800040116    2c                   EndIf
282900040116
283000040116    1c                   EndSl
283100040116
283200040116     c                   EndSr
283300991022      **********************************************************************
283400991022      * AGGANCIO TABELLA VALUTE
283500991022      **********************************************************************
283600991022     C     CTR_VALUTA    BEGSR
283700991022
283800991022     C                   MOVEL     'CV'          KCOD
283900991022     C                   MOVEL(P)  WDIV          KKEY
284000991022     C     KTAB1         CHAIN     TABEL00F                           31
284100991022     C  N31              MOVEL     TBLUNI        DSCV
284200991022
284300991022     C                   ENDSR
284400991022      **********************************************************************
284500991022      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
284600991022      **********************************************************************
284700991022     C     EDTIMPORTI    BEGSR
284800991022
284900991022      * ------------------------------------------------------------------------
285000991022      * W0133      contiene il numero da editare per il video
285100991022      * WTSTDEC    corrisponde a §CVfdc della tabella CV
285200991022      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
285300991022      * ------------------------------------------------------------------------
285400991022      *
285500991022     c                   clear                   IMPeditato
285600991022     c                   clear                   W017A
285700991022      *
285800991022     c                   if        Wtstdec = 'S'
285900991022     C                   eval      W017a = %EDITC(W0133:'2')
286000991022     c                   else
286100991022     c                   z-add     W0133         W0130
286200991022     C                   eval      W017a = %EDITC(W0130:'2')
286300991022     c                   endif
286400991022      *
286500991022     c                   move      W017a         IMPeditato
286600991022      *
286700991022     c                   ENDSR
286800991025      *****************************************************************
286900991025      *  AGGANCIO ANAGRAFICHE
287000991025      *****************************************************************
287100991025     C     CTR_ANAGRA    BEGSR
287200991025      *
287300991025     C                   clear                   DS_cnaco
287400991025     C                   clear                   DS_cnind
287500991025     C                   clear                   DS_cnclp
287600991025     C                   clear                   DS_fncls
287700991025      *
287800991025     C                   CALL      'TIBS69R'
287900991025     C                   PARM                    tibs69DS
288000991025     C                   PARM                    DS_cnaco
288100991025     C                   PARM                    DS_cnind
288200991025     C                   PARM                    DS_cnclp
288300991025     C                   PARM                    DS_fncls
288400991025      *
288500991025     C                   eval      wtibs69r = 'S'
288600991025      * per il 1° giro è inizializzata nelle specifiche "D"
288700991025     C                   clear                   TIBS69DS
288800991025      *
288900991025     C                   ENDSR
289000940805     C**************************************************************************
289100940805     C*    CMD18 VISUALIZZA LEGAMI
289200940805     C**************************************************************************
289300920326     C     CMD18         BEGSR
289400940810     C*
289500940907     C* SAVNR9 > 2 --> GESTIONE SUBFILE
289600950328    1C     SAVNR9        IFGT      2
289700940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
289800940810     C                   MOVEL     'S'           WRIEM
289900940810     C     FORC2         TAG
290000940810     C                   Z-ADD     1             REC9
290100950328    2C     WRIEM         IFEQ      'S'
290200940810     C                   WRITE     LS05T01
290300940810     C                   WRITE     LS05Z09
290400940810     C                   MOVEL     *BLANKS       WRIEM
290500950328    2C                   ENDIF
290600940810     C                   EXFMT     LS05C09
290700940810     C*
290800940810     C* CMD12 - FINE LAVORO
290900940810     C   KL              GOTO      SOTTO
291000940810     C*
291100940810     C                   READC     LS05S09                                93
291200950328    2C     *IN93         DOWEQ     '0'
291300940810     C*
291400950328    3C     V9CSCE        IFEQ      '5'
291500940810     C                   MOVEL     ' '           V9CSCE
291600950103     C*
291700950103     C* RIPRISTINO GLI INDICATORI
291800950103     C                   MOVEL     V9CI80        *IN80
291900950103     C                   MOVEL     V9CI81        *IN81
292000950103     C                   MOVEL     V9CI82        *IN82
292100950103     C                   MOVEL     V9CI83        *IN83
292200940810     C                   UPDATE    LS05S09
292300940810     C*
292400950328    4C     V9CAAN        IFEQ      *ZERO
292500940907     C                   MOVEL     V9CAAP        PA4AAS
292600940907     C                   MOVEL     V9CLPP        PA4LNP
292700940907     C                   MOVEL     V9CNRP        PA4NRS
292800940907     C                   MOVEL     V9CNSP        PA4NSP
292900940907     C                   ELSE
293000940907     C                   MOVEL     V9CAAN        PA4AAS
293100940907     C                   MOVEL     V9CLPN        PA4LNP
293200940907     C                   MOVEL     V9CNRN        PA4NRS
293300940907     C                   MOVEL     V9CNSN        PA4NSP
293400950328    4C                   ENDIF
293500940810     C                   MOVEL     *BLANKS       PA4F03
293600960903     C     PA0FLG        IFEQ      'N'
293700960903     C* N.B.: Non testo anche pa0flg='R' perchè è impossibile che si
293800960903     C*       verifichi questo caso
293900960903     C                   MOVEL     'P'           PA4FLG
294000960903     C                   ELSE
294100940810     C                   MOVEL     '3'           PA4FLG
294200960903     C                   END
294300940810     C                   MOVEL     PARAM4        KPJBU
294400940810     C                   CALL      'FNLR36R'
294500940810     C                   PARM                    KPJBA
294600940810     C                   MOVEL     KPJBU         PARAM4
294700940810     C                   MOVEL     PA4F03        WF03
294800940810     C                   MOVEL     'S'           WRIEM
294900950328    3C                   ENDIF
295000940810     C*
295100940810     C                   READC     LS05S09                                93
295200950328    2C                   ENDDO
295300940908     C*
295400940908     C* RIEMETTO IL FORMATO VIDEO DEI LEGAMI
295500950328    2C     WRIEM         IFEQ      'S'
295600940908     C                   GOTO      FORC2
295700950328    2C                   ENDIF
295800940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
295900940810     C                   ELSE
296000940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
296100940810     C                   MOVEL     COMAAN        PA4AAS
296200940810     C                   MOVEL     COMLPN        PA4LNP
296300940810     C                   MOVEL     COMNRN        PA4NRS
296400940810     C                   MOVEL     COMNSN        PA4NSP
296500940810     C                   MOVEL     *BLANKS       PA4F03
296600960903     C     PA0FLG        IFEQ      'N'
296700960903     C* N.B.: Non testo anche pa0flg='R' perchè è impossibile che si
296800960903     C*       verifichi questo caso
296900960903     C                   MOVEL     'P'           PA4FLG
297000960903     C                   ELSE
297100940810     C                   MOVEL     '3'           PA4FLG
297200960903     C                   END
297300940810     C                   MOVEL     PARAM4        KPJBU
297400940810     C                   CALL      'FNLR36R'
297500940810     C                   PARM                    KPJBA
297600940810     C                   MOVEL     KPJBU         PARAM4
297700940810     C                   MOVEL     PA4F03        WF03
297800940810     C                   MOVEL     'S'           WRIEM
297900940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
298000940810     C                   ENDIF
298100940810     C*
298200940810     C     SOTTO         TAG
298300920326     C                   ENDSR
298400940802      **************************************************************************
298500940802      * VISUALIZZO IL FORMATO VIDEO DI RIEPILOGO DELLA BOLLA
298600940802      **************************************************************************
298700940802     C     RIEPIL        BEGSR
298800940802      *
298900940802     C                   SETOFF                                       25
299000940804     C                   MOVEL     'S'           WRIEM             1
299100021218      *
299200021218      * Riempio il campone in fondo alla videata con i tasti scelti
299300021218     C                   exsr      Ges_TastiFun
299400021218     c                   eval      FD07 = FDxx
299500021218     c                   eval      F24D07 = F24Dxx
299600940802     C     FOR07         TAG
299700940804     C     WRIEM         IFEQ      'S'
299800940804     C                   WRITE     LS05T01
299900940804     C                   MOVEL     *BLANKS       WRIEM
300000940804     C                   ENDIF
300100021218      *
300200021218      * Imposto la stringa comandi del 'giro' successivo
300300021218     C                   IF        *inKY = *on and $Loop > 1
300400021218     c                   Exsr      Ges_F24
300500021218     c                   eval      FD07 = FDxx
300600021218     c                   eval      F24D07 = F24Dxx
300700021218     C                   ENDIF
300800940804     C                   EXFMT     LS05D07
300900940808     C                   MOVEL     *BLANKS       WF03
301000940802      *
301100990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
301200940802     C   KY*IN25         IFEQ      *ON
301300940802     C                   SETOFF                                       25
301400940802     C                   ELSE
301500940802     C                   SETON                                        25
301600940802     C                   ENDIF
301700940802     C   KY              GOTO      FOR07
301800940802      *
301900990118      * F3 - FINE LAVORO TORNO A SUBFILE
302000941109     C   KC              MOVEL     '1'           PA0F03
302100940802     C   KC              GOTO      ENDRIE
302200940802      *
302300940802      * TASTI DI FUNZIONE VARI
302400940802     C                   EXSR      COMAND
302500940808     C*
302600940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
302700940808     C     WF03          IFNE      *BLANKS
302800941109     C                   MOVEL     '1'           PA0F03
302900940808     C                   GOTO      ENDRIE
303000940808     C                   ENDIF
303100950629     C*
303200940802     C   31              GOTO      FOR07
303300940802      *
303400940802     C     ENDRIE        ENDSR
303500940802      **************************************************************************
303600940802      * Nel caso di interrogazione da parte di un cliente, viene verificato se
303700940802      * egli puo' interrogare le bolle e se deve essere conteggiato il traffico
303800940802      * dei byte sulla linea di comunicazione.
303900940802      * Queste informazioni sono contenute nella tabella 3K.
304000940802      *
304100940802      * Inoltre, egli puo' interrogare solo alcuni clienti (quelli a lui
304200940802      * unificati.
304300940802      **************************************************************************
304400940802     C     ABICLI        BEGSR
304500940802      *
304600940802     C                   MOVE      *BLANKS       $3KINB
304700940802     C                   MOVE      *BLANKS       $3KCTB
304800940802      *
304900940802      * Leggo tutta la tabella 3K riempendo una schiera con i cod.clienti
305000940802     C                   Z-ADD     *ZERO         $PRIMA            1 0
305100940802     C                   Z-ADD     *ZERO         $A                3 0
305200940804     C                   MOVE      '3K'          KCOD
305300940809     C     KTAB2         SETLL     TABEL00F
305400940802     C                   DO        *HIVAL
305500940809     C     KTAB2         READE     TABEL00F                               90
305600940802     C  N90TBLFLG        IFEQ      *BLANKS
305700940802     C                   MOVEL     TBLUNI        DS3K
305800940802     C     KNMUS         IFEQ      §3KPRF
305900940802      *
306000940802      * Devo prendere il cod.cliente, che e' la chiave di TABEL
306100940802     C                   MOVEL     TBLKEY        $CODIC            7 0
306200940802     C     $CODIC        LOOKUP    CLI                                    99
306300940802     C  N99$A            IFLT      300
306400940802     C                   ADD       1             $A
306500940802     C                   Z-ADD     $CODIC        CLI($A)
306600940802     C                   ENDIF
306700940802      *
306800940802      * Gia' dalla prima lettura ho i due flag che abilitano il profilo
306900940802     C     $PRIMA        IFEQ      *ZERO
307000940802     C                   MOVE      §3KINB        $3KINB
307100940802     C                   MOVE      §3KCTB        $3KCTB
307200940802     C     §3KINB        COMP      'S'                                9090
307300940802     C                   Z-ADD     1             $PRIMA
307400940802     C                   ENDIF
307500940802      *
307600940802     C                   ENDIF
307700940802     C                   ENDIF
307800940802     C  N90              ENDDO
307900940802      *
308000940802     C                   ENDSR
308100940905     C**************************************************************************
308200940905     C* RIEMPIE IL SFL CON TUTTI I LEGAMI BOLLA
308300940905     C* PARTENDO DALLA BOLLA ORIGINALE, FINO ALL'ULTIMA IN
308400940906     C* ORDINE CRONOLOGICO (ULTIMO MODELLO SWATCH)
308500940905     C**************************************************************************
308600940906     C     LEGBOL        BEGSR
308700940906     C*
308800940906     C* PULISCO IL SFL
308900940906     C                   SETOFF                                       202123
309000940906     C                   WRITE     LS05C09
309100940906     C                   SETON                                        2123
309200940906     C*
309300940906     C                   SETOFF                                       07
309400940906     C                   Z-ADD     *ZERO         SAVNR9            4 0
309500940906     C                   MOVEL     *BLANKS       WFLG1             1
309600940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
309700940905     C*
309800940906     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
309900940906     C     KBLP3         CHAIN     FNLBL01L                           99
310000940906     C   99KBLP3         CHAIN     FNLBL02L                           99
310100940906     C  N99              DO
310200940905     C*
310300940906     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
310400940906     C                   MOVEL     LBLAAO        KAAS
310500940906     C                   MOVEL     LBLLPO        KLNP
310600940906     C                   MOVEL     LBLNRO        KNRS
310700940906     C                   MOVEL     LBLNSO        KNSP
310800940906     C     KLBL3         CHAIN     FNLBL02L                           99
310900950103     C     *IN99         IFEQ      *ON
311000950103     C                   MOVEL     'N'           WORIGI            1
311100950103     C                   ELSE
311200950103     C                   MOVEL     'S'           WORIGI
311300950103     C                   ENDIF
311400940907     C*
311500940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
311600940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
311700950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
311800950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
311900940907     C                   MOVEL     ' '           V9CSCE
312000940907     C                   MOVEL     LBLAAO        V9CAAP
312100940907     C                   MOVEL     LBLLPO        V9CLPP
312200940907     C                   MOVEL     LBLLAO        V9CLAP
312300940907     C                   MOVEL     LBLNRO        V9CNRP
312400940907     C                   MOVEL     LBLNSO        V9CNSP
312500940926     C                   CLEAR                   V9CAAN
312600940926     C                   CLEAR                   V9CLPN
312700940926     C                   CLEAR                   V9CLAN
312800940926     C                   CLEAR                   V9CNRN
312900940926     C                   CLEAR                   V9CNSN
313000940907     C*
313100940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
313200940907     C     LBLAAO        IFEQ      V1CAAS
313300940907     C     LBLLPO        ANDEQ     V1CLNP
313400940907     C     LBLNRO        ANDEQ     V1CNRS
313500940907     C     LBLNSO        ANDEQ     V1CNSP
313600940907     C                   MOVEL     *BLANKS       V9CPRE
313700940907     C                   SETON                                        80
313800940907     C                   SETON                                        81
313900940907     C                   MOVEL     '1'           WFLG1
314000940907     C                   ELSE
314100940907     C                   SETOFF                                       81
314200940907     C*
314300950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
314400950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
314500950320     C                   SETOFF                                       93
314600950320     C     KLBL3         SETLL     FNARB01L                               93
314700950320     C  N93              MOVEA     PRD(1)        V9CPRE
314800950320     C  N93              SETON                                        80
314900950320     C*
315000950320     C* SE RICHIAMATO DAGLI ARRIVI (FNLR36R) NON POSSO
315100950320     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
315200950320     C   93PA0FLG        IFEQ      'A'
315300960903     C     PA0FLG        OREQ      'R'
315400950320     C                   MOVEA     PRD(4)        V9CPRE
315500950320     C                   SETON                                        80
315600950320     C                   ELSE
315700950328     C                   MOVEA     PRD(2)        V9CPRE
315800950320     C                   SETOFF                                       80
315900950320     C                   ENDIF
316000950320     C*
316100950320     C* SE NON TROVATA SU ARB PROVO SU BLP
316200950320     C  N93              DO
316300950320     C                   SETOFF                                       93
316400950320     C     KLBL3         SETLL     FNBLP01L                               93
316500950320     C*
316600950328     C   93              MOVEA     PRD(3)        V9CPRE
316700950428     C   93              SETON                                        80
316800950320     C*
316900950320     C                   ENDDO
317000940907     C*
317100950320     C                   ENDIF
317200940907     C                   SETON                                        83
317300950103     C*
317400950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
317500950103     C                   MOVEL     *IN80         V9CI80
317600950103     C                   MOVEL     *IN81         V9CI81
317700950103     C                   MOVEL     *IN82         V9CI82
317800950103     C                   MOVEL     *IN83         V9CI83
317900950103     C*
318000940907     C                   Z-ADD     1             NRR9
318100940907     C                   WRITE     LS05S09
318200940907     C                   SETON                                        20
318300950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
318400950103     C*
318500950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
318600950103     C*                      2) NON HO TROVATO L'ORIGINALE
318700950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
318800950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
318900950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
319000950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
319100950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
319200950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
319300950103     C* L'ORIGINALE E LA MAMMA.
319400950103     C     WORIGI        IFEQ      'N'
319500940907     C*
319600950103     C                   MOVEL     ' '           V9CSCE
319700950103     C                   CLEAR                   V9CAAP
319800950103     C                   CLEAR                   V9CLPP
319900950103     C                   CLEAR                   V9CLAP
320000950103     C                   CLEAR                   V9CNRP
320100950103     C                   CLEAR                   V9CNSP
320200950103     C                   MOVEL     LBLAAP        V9CAAN
320300950103     C                   MOVEL     LBLLPP        V9CLPN
320400950103     C                   MOVEL     LBLLAP        V9CLAN
320500950103     C                   MOVEL     LBLNRP        V9CNRN
320600950103     C                   MOVEL     LBLNSP        V9CNSN
320700950103     C                   SETOFF                                       8182
320800950103     C*
320900950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
321000950103     C     V9CAAN        IFEQ      V1CAAS
321100950103     C     V9CLPN        ANDEQ     V1CLNP
321200950103     C     V9CNRN        ANDEQ     V1CNRS
321300950103     C     V9CNSN        ANDEQ     V1CNSP
321400950103     C                   MOVEL     *BLANKS       V9CPRE
321500950103     C                   SETON                                        80
321600950103     C                   SETON                                        82
321700950103     C                   ELSE
321800950103     C*
321900950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
322000950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
322100950320     C                   EXSR      CHKBOL
322200950103     C*
322300950103     C                   ENDIF
322400950103     C*
322500950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
322600950103     C                   SETOFF                                       83
322700950103     C*
322800950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
322900950103     C                   MOVEL     *IN80         V9CI80
323000950103     C                   MOVEL     *IN81         V9CI81
323100950103     C                   MOVEL     *IN82         V9CI82
323200950103     C                   MOVEL     *IN83         V9CI83
323300950103     C*
323400950103     C                   ADD       1             NRR9
323500950103     C                   WRITE     LS05S09
323600950103     C*
323700950103     C                   ENDIF
323800940907     C                   SETOFF                                       83  99
323900940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
324000940906     C* CICLO FINO ALL'ULTIMO LEGAME
324100940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
324200940907     C                   Z-ADD     *ZERO         WA                3 0
324300940907     C                   Z-ADD     *ZERO         WB                3 0
324400940907     C                   MOVEA     *ZERO         AAN
324500940906     C     *IN99         DOWEQ     *OFF
324600940905     C*
324700940906     C                   MOVEL     ' '           V9CSCE
324800940907     C                   MOVEL     LBLAAP        V9CAAP
324900940907     C                   MOVEL     LBLLPP        V9CLPP
325000940907     C                   MOVEL     LBLLAP        V9CLAP
325100940907     C                   MOVEL     LBLNRP        V9CNRP
325200940907     C                   MOVEL     LBLNSP        V9CNSP
325300940907     C                   MOVEL     LBLAAN        V9CAAN
325400940907     C                   MOVEL     LBLLPN        V9CLPN
325500940907     C                   MOVEL     LBLLAN        V9CLAN
325600940907     C                   MOVEL     LBLNRN        V9CNRN
325700940907     C                   MOVEL     LBLNSN        V9CNSN
325800940907     C                   SETOFF                                       8182
325900940906     C*
326000950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
326100940907     C     LBLAAP        IFEQ      V1CAAS
326200940907     C     LBLLPP        ANDEQ     V1CLNP
326300940907     C     LBLNRP        ANDEQ     V1CNRS
326400940907     C     LBLNSP        ANDEQ     V1CNSP
326500940907     C                   SETON                                        81
326600940907     C                   ENDIF
326700940907     C*
326800940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
326900940907     C     LBLAAN        IFEQ      V1CAAS
327000940907     C     LBLLPN        ANDEQ     V1CLNP
327100940907     C     LBLNRN        ANDEQ     V1CNRS
327200940907     C     LBLNSN        ANDEQ     V1CNSP
327300940906     C                   MOVEL     *BLANKS       V9CPRE
327400940906     C                   SETON                                        80
327500940907     C                   SETON                                        82
327600940906     C                   ELSE
327700940906     C*
327800950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
327900950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
328000950320     C                   EXSR      CHKBOL
328100940906     C*
328200940906     C                   ENDIF
328300950103     C*
328400950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
328500950103     C                   MOVEL     *IN80         V9CI80
328600950103     C                   MOVEL     *IN81         V9CI81
328700950103     C                   MOVEL     *IN82         V9CI82
328800950103     C                   MOVEL     *IN83         V9CI83
328900940906     C*
329000940906     C* SCRIVO IL RECORD NEL SFL
329100940906     C                   ADD       1             NRR9
329200940906     C                   WRITE     LS05S09
329300940907     C*
329400940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
329500940907     C                   ADD       1             WA
329600940907     C                   MOVEL     LBLAAN        AAN(WA)
329700940907     C                   MOVEL     LBLLPN        LPN(WA)
329800940907     C                   MOVEL     LBLNRN        NRN(WA)
329900940907     C                   MOVEL     LBLNSN        NSN(WA)
330000940907     C*
330100940907     C     KLBL3         READE     FNLBL02L                               99
330200940906     C*
330300940907     C* SE NON HO PIU' BOLLE CON READE
330400940906     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
330500970116     C     WB            DOWLT     WA
330600970116     C     *IN99         ANDEQ     *ON
330700940907     C                   ADD       1             WB
330800940907     C                   MOVEL     AAN(WB)       KAAS
330900940907     C                   MOVEL     LPN(WB)       KLNP
331000940907     C                   MOVEL     NRN(WB)       KNRS
331100940907     C                   MOVEL     NSN(WB)       KNSP
331200940907     C     KLBL3         SETLL     FNLBL02L
331300940907     C     KLBL3         READE     FNLBL02L                               99
331400970116     C                   ENDDO
331500940906     C*
331600940906     C                   ENDDO
331700940906     C                   ENDDO
331800940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
331900940906     C*
332000940906     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
332100940906     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
332200940926     C     NRR9          IFEQ      2
332300940907     C*
332400940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
332500940907     C     2             CHAIN     LS05S09                            99
332600940907     C     V9CAAP        IFEQ      V1CAAS
332700940907     C     V9CLPP        ANDEQ     V1CLNP
332800940907     C     V9CNRP        ANDEQ     V1CNRS
332900940907     C     V9CNSP        ANDEQ     V1CNSP
333000940907     C                   MOVEL     V9CAAN        COMAAN
333100940907     C                   MOVEL     V9CLPN        COMLPN
333200940907     C                   MOVEL     V9CNRN        COMNRN
333300940907     C                   MOVEL     V9CNSN        COMNSN
333400950306     C                   MOVEL     V9CLAN        COMLAN
333500950316     C                   MOVEA     'SEGUEN.'     LBL(14)
333600940907     C                   ELSE
333700940907     C                   MOVEL     V9CAAP        COMAAN
333800940907     C                   MOVEL     V9CLPP        COMLPN
333900940907     C                   MOVEL     V9CNRP        COMNRN
334000940907     C                   MOVEL     V9CNSP        COMNSN
334100950306     C                   MOVEL     V9CLAP        COMLAN
334200950316     C                   MOVEA     'PRECED.'     LBL(14)
334300940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
334400940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
334500940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
334600940926     C     1             CHAIN     LS05S09                            99
334700940926     C* REIMPOSTO NRR9=2 PERCHE' MI SERVE DOPO
334800940926     C                   Z-ADD     2             NRR9
334900940907     C                   ENDIF
335000940907     C*
335100950316     C                   MOVEA     COMLPN        LBL(21)
335200950316     C                   MOVEA     COMNRN        LBL(25)
335300950316     C                   MOVEA     COMNSN        LBL(28)
335400950316     C                   MOVEA     COMLAN        LBL(40)
335500940906     C*
335600940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
335700940907     C                   Z-ADD     1             W                 1 0
335800940907     C     V9CPRE        LOOKUP    PRD(W)                                 99
335900950306     C                   MOVEA     LBD(W)        LBL(44)
336000940907     C     W             COMP      2                                      07
336100940906     C*
336200940906     C                   MOVEA     LBL           LEGAME
336300940906     C                   ELSE
336400940905     C*
336500940926     C     NRR9          IFGT      2
336600940906     C                   MOVEA     LB2           LEGAME
336700940906     C                   SETON                                        07
336800940906     C                   ENDIF
336900940906     C                   ENDIF
337000940906     C*
337100940907     C     NRR9          COMP      1                                    3434
337200940905     C*
337300940906     C                   Z-ADD     NRR9          SAVNR9
337400940906     C                   Z-ADD     1             NRR9
337500940912     C*
337600940912     C* RIAGGANCIO LA BOLLA IN ESAME
337700940912     C     KBLP3         CHAIN     FNBLP01L                           99
337800940905     C*
337900940905     C                   ENDSR
338000950320     C**************************************************************************
338100950320     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
338200950320     C**************************************************************************
338300950320     C     CHKBOL        BEGSR
338400950320     C*
338500950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
338600950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
338700950320     C                   SETOFF                                       93
338800950320     C     KLBL1         SETLL     FNARB01L                               93
338900950320     C  N93              MOVEA     PRD(1)        V9CPRE
339000950320     C  N93              SETON                                        80
339100950320     C*
339200950320     C* SE RICHIAMATO DAGLI ARRIVI (FNLR36R) NON POSSO
339300950320     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
339400950320     C   93PA0FLG        IFEQ      'A'
339500960903     C     PA0FLG        OREQ      'R'
339600950320     C                   MOVEA     PRD(4)        V9CPRE
339700950320     C                   SETON                                        80
339800950320     C                   ELSE
339900950320     C                   MOVEA     PRD(2)        V9CPRE
340000950320     C                   SETOFF                                       80
340100950320     C                   ENDIF
340200950320     C*
340300950320     C* SE NON TROVATA SU ARB PROVO SU BLP
340400950320     C  N93              DO
340500950320     C                   SETOFF                                       93
340600950320     C     KLBL1         SETLL     FNBLP01L                               93
340700950328     C   93              MOVEA     PRD(3)        V9CPRE
340800950428     C   93              SETON                                        80
340900950320     C*
341000950320     C                   ENDDO
341100950320     C*
341200950320     C                   ENDSR
341300940923      **************************************************************************
341400940923      *   CONTROLLI SUL FORMATO VIDEO D02
341500940923      **************************************************************************
341600940923     C     CTRD02        BEGSR
341700940923     C*
341800940923     C                   SETOFF                                       31
341900940923     C*
342000940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
342100940923     C     V2IGCA        IFEQ      '?'
342200991022     C                   CLEAR                   DS7PQRS
342300941003     C                   MOVEL     'S'           DS7RIC
342400941003     C                   MOVEL     'V'           DS7GES
342500941003     C                   MOVEL     V2CGCA        DS7COD
342600991022     C                   MOVEL     DS7PQRS       KPJBU
342700960126     C                   CALL      'TRTB72R'                            31
342800941003     C                   PARM                    KPJBA
342900940923     C                   MOVEL     *BLANKS       V2IGCA
343000940923     C                   SETON                                        31
343100940923     C                   MOVEL     'S'           WRIEM
343200940923     C                   ENDIF
343300940923     C*
343400940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
343500940923     C     V2IGGA        IFEQ      '?'
343600991022     C                   CLEAR                   DS7PQRS
343700941003     C                   MOVEL     'S'           DS7RIC
343800941003     C                   MOVEL     'V'           DS7GES
343900941003     C                   MOVEL     V2CGGA        DS7COD
344000991022     C                   MOVEL     DS7PQRS       KPJBU
344100960126     C                   CALL      'TRTB69R'                            31
344200941003     C                   PARM                    KPJBA
344300940923     C                   SETON                                        31
344400940923     C                   MOVEL     'S'           WRIEM
344500940923     C                   MOVEL     *BLANKS       V2IGGA
344600940923     C                   ENDIF
344700940923     C*
344800940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
344900940923     C     V2IGMA        IFEQ      '?'
345000991022     C                   CLEAR                   DS7PQRS
345100941003     C                   MOVEL     'S'           DS7RIC
345200941003     C                   MOVEL     'V'           DS7GES
345300941003     C                   MOVEL     V2CGMA        DS7COD
345400991022     C                   MOVEL     DS7PQRS       KPJBU
345500960126     C                   CALL      'TRTB70R'                            31
345600941003     C                   PARM                    KPJBA
345700940923     C                   SETON                                        31
345800940923     C                   MOVEL     'S'           WRIEM
345900940923     C                   MOVEL     *BLANKS       V2IGMA
346000940923     C                   ENDIF
346100940923     C*
346200940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
346300940923     C     V2IGVA        IFEQ      '?'
346400991022     C                   CLEAR                   DS7PQRS
346500941003     C                   MOVEL     'S'           DS7RIC
346600941003     C                   MOVEL     'V'           DS7GES
346700941003     C                   MOVEL     V2CGVA        DS7COD
346800991022     C                   MOVEL     DS7PQRS       KPJBU
346900960126     C                   CALL      'TRTB73R'                            31
347000941003     C                   PARM                    KPJBA
347100940923     C                   SETON                                        31
347200940923     C                   MOVEL     'S'           WRIEM
347300941003     C                   MOVEL     *BLANKS       V2IGVA
347400940923     C                   ENDIF
347500940923     C*
347600940923     C                   ENDSR
347700940810      **************************************************************************
347800940810      *   R O U T I N E     I N I Z I A L E
347900940810      **************************************************************************
348000940810     C     *INZSR        BEGSR
348100940810     C*
348200940810     C* ACCESSO   FILE BOLLE VISTA LOGICA 1
348300940810     C     KBLP3         KLIST
348400940810     C                   KFLD                    PA0AAS
348500940810     C                   KFLD                    PA0LNP
348600940810     C                   KFLD                    PA0NRS
348700940810     C                   KFLD                    PA0NSP
348800021218     C*
348900021218     C     KAR51         KLIST
349000021218     C                   KFLD                    KAAS
349100021218     C                   KFLD                    KLNP
349200021218     C                   KFLD                    KNRS
349300021218     C                   KFLD                    KNSP
349400021218     C                   KFLD                    KTRd              3
349500021218     C*
349600991022     C     KAR6          KLIST
349700991022     C                   KFLD                    PA0AAS
349800940808     C                   KFLD                    PA0LNP
349900940808     C                   KFLD                    PA0NRS
350000940808     C                   KFLD                    PA0NSP
350100991022     c                   KFLD                    KTRC
350200940810     C* ACCESSO   FILE LEGAME BOLLA
350300940810     C     KLBL1         KLIST
350400940810     C                   KFLD                    LBLAAN
350500940810     C                   KFLD                    LBLLPN
350600940810     C                   KFLD                    LBLNRN
350700940810     C                   KFLD                    LBLNSN
350800940906     C     KLBL3         KLIST
350900940906     C                   KFLD                    KAAS
351000940906     C                   KFLD                    KLNP
351100940906     C                   KFLD                    KNRS
351200940906     C                   KFLD                    KNSP
351300060216     C     Kar41         KLIST
351400940805     C                   KFLD                    KAAS
351500940805     C                   KFLD                    KLNP
351600940805     C                   KFLD                    KNRS
351700940805     C                   KFLD                    KNSP
351800940805     C                   KFLD                    KTRC
351900060216     C     Kar42         KLIST
352000951219     C                   KFLD                    KAAS
352100951219     C                   KFLD                    KLNP
352200951219     C                   KFLD                    KNRS
352300951219     C                   KFLD                    KNSP
352400940809     C     KTAB1         KLIST
352500940804     C                   KFLD                    CODUT
352600940804     C                   KFLD                    KCOD
352700940804     C                   KFLD                    KKEY
352800940809     C     KTAB2         KLIST
352900940804     C                   KFLD                    CODUT
353000940804     C                   KFLD                    KCOD
353100940808     C     KIND1         KLIST
353200940808     C                   KFLD                    CODUT
353300940808     C                   KFLD                    KKCC
353400940808     C                   KFLD                    KKSC
353500021203     C     Kfapd         klist
353600021203     C                   kfld                    kpdtip
353700021203     C                   kfld                    kpdpdr
353800040220      * File FITGD02L
353900040220     c     K02TGD02      klist
354000040220     c                   kfld                    TGDtor
354100040220     c                   kfld                    TGDogg
354200040713      * Fnorm
354300040713     c     korm          klist
354400040713     c                   kfld                    §b4poe
354500040713     c                   kfld                    §b4nsr
354600040713     c                   kfld                    §b4nor
354700040713     c                   kfld                    §b4nrv
354800940804     C*
354900940810     C                   MOVEL     *ZERO         COMLPN            3
355000940810     C                   MOVEL     *ZERO         COMNRN            2
355100940810     C                   MOVEL     *ZERO         COMNSN            7
355200950306     C                   MOVEL     *ZERO         COMLAN            3
355300940804     C*
355400940810     C                   Z-ADD     *ZERO         NRR8              4 0
355500940810     C                   Z-ADD     *ZERO         NRR9              4 0
355600940906     C                   Z-ADD     *ZERO         SAVNR8            4 0
355700940906     C                   Z-ADD     *ZERO         SAVNR9            4 0
355800940808     C                   MOVEL     *BLANKS       WF03              1
355900940808     C*
356000940804     C                   ENDSR
356100940804      **************************************************************************
356200941011**   SEQ
356300910912NON SEQUENZIALI
356400950316**        12345678901234567890123456789012345678901234567890
356500950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
356600920326**
356700950316LEGATA A PIU' BOLLE                       - F18=Visualizza
356800920326**
356900950316 Non Presente
357000950316 F18=Visualizza
357100940906Vedi in Int.Par
357200940907Vedi in Int.Arr
357300941011Vedi arch.stor.
357400920326**
357500920326Spedizione non Presente
357600941230
357700940907Vedi in Int.Bolle Part.
357800940907Vedi in Int.Bolle Arr.
357900941011Vedi in Archiv. Storico
358000040220** TF                                                                        *
358100040610F1=Agg.Rich.#
358200021218** TF                                                                        *
358300021218F2=C.A.#
358400021218** TF                                                                        *
358500021218F4=Tass2#
358600021218** TF                                                                        *
358700021218F5=Eventi#
358800021218** TF                                                                        *
358900021218F7=Anomalie#
359000021218** TF                                                                        *
359100021218F8=Orm#
359200021218** TF                                                                        *
359300021218F10=Variaz.#
359400021218** TF                                                                        *
359500030121F11=NoteS/B#
359600021218** TF                                                                        *
359700021218F12=Ritorno#
359800021218** TF                                                                        *
359900021218F13=Tassaz.#
360000021218** TF                                                                        *
360100021218F14=Giacenza#
360200040220** TF                                                                        *
360300040610F16=Gest.Rich.#
360400021218** TF                                                                        *
360500021218F19=Spunte#
360600021218** TF                                                                        *
360700021218F20=Affid.#
360800021218** TF                                                                        *
360900021218F21=Dett.Colli#
361000021218** TF                                                                        *
361100051201F22=C/Assegni#
