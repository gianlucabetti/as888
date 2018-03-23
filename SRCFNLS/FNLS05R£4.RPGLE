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
001200040714     F**""**FNDCT02L  IF   E           K DISK
001300990119     FFNDFA01L  IF   E           K DISK
001400940804     FFNARB01L  IF   E           K DISK
001500021203     FFIAPD01L  IF   E           K DISK
001600940808     F* FILE STORICO VARIAZIONI BOLLE
001700131007     FFIARBF2C  IF   E           K DISK
001800021218     FFIAR501L  IF   E           K DISK
001900991022     FFIAR601L  IF   E           K DISK
002000991022     FFIAR701L  IF   E           K DISK
002100940802     FFNBLP01L  IF   E           K DISK
002200060216     FFiar401L  IF   E           K DISK
002300051115     FFiar901L  IF   E           K DISK
002400940802     FFNBLT01L  IF   E           K DISK
002500990809     FFNEVB01L  IF   E           K DISK
002600050406     FtIGCP01L  IF   E           K DISK
002700940804     FFNLBL01L  IF   E           K DISK
002800940804     FFNLBL02L  IF   E           K DISK
002900940804     F                                     RENAME(FNLBL000:FNLBL002)
003000940804     FTABEL00F  IF   E           K DISK
003100011120     FFNCSB01L  IF   E           K DISK
003200040220      *.......... T E L E F O N A T E   /   R E C L A M I
003300040220     fFITGD02L  if   e           k disk
003400040713      * ORM
003500040713     fFnorm01l  if   e           k disk
003600140224     Ffipct02l  IF   E           K DISK
003700940804      *-------------------------------------------------------------------------
003800040220      *
003900040220      *   S C H I E R E
004000040220      *
004100940906     D SEQ             S             15    DIM(1) CTDATA PERRCD(1)
004200950306     D LBL             S              1    DIM(58) CTDATA PERRCD(58)
004300950306     D LB2             S             58    DIM(1) CTDATA PERRCD(1)
004400941011     D LBD             S             15    DIM(5) CTDATA PERRCD(1)
004500941011     D PRD             S             23    DIM(5) CTDATA PERRCD(1)
004600040220     d TF01            s              1    dim(20) ctdata perrcd(20)
004700021218     D TF02            S              1    DIM(20) CTDATA PERRCD(20)
004800021218     D TF04            S              1    DIM(20) CTDATA PERRCD(20)
004900021218     D TF05            S              1    DIM(20) CTDATA PERRCD(20)
005000021218     D TF07            S              1    DIM(20) CTDATA PERRCD(20)
005100021218     D TF08            S              1    DIM(20) CTDATA PERRCD(20)
005200130912     D TF09            S              1    DIM(20) CTDATA PERRCD(20)
005300021218     D TF10            S              1    DIM(20) CTDATA PERRCD(20)
005400021218     D TF11            S              1    DIM(20) CTDATA PERRCD(20)
005500021218     D TF12            S              1    DIM(20) CTDATA PERRCD(20)
005600021218     D TF13            S              1    DIM(20) CTDATA PERRCD(20)
005700021218     D TF14            S              1    DIM(20) CTDATA PERRCD(20)
005800040220     d TF16            s              1    dim(20) ctdata perrcd(20)
005900021218     D TF19            S              1    DIM(20) CTDATA PERRCD(20)
006000021218     D TF20            S              1    DIM(20) CTDATA PERRCD(20)
006100021218     D TF21            S              1    DIM(20) CTDATA PERRCD(20)
006200021218     D TF22            S              1    DIM(20) CTDATA PERRCD(20)
006300021218     D $TF             S              1    DIM(20)
006400021218     D*
006500111020     D**!!!CLI             S              7  0 DIM(300)
006600940907     D*
006700940907     D AAN             S              4  0 DIM(30)
006800940907     D LPN             S              3  0 DIM(30)
006900940907     D NRN             S              2  0 DIM(30)
007000940907     D NSN             S              7  0 DIM(30)
007100941021     D CC1             S              1    DIM(99)
007200941021     D CC2             S             20    DIM(99)
007300020329     D CC3             S              1    DIM(99)
007400020329     D CB1             S              1    DIM(99)
007500020329     D CB2             S             20    DIM(99)
007600030121     D RigaTf1         S              1    DIM(62)
007700030121     D RigaTf2         S              1    DIM(62)
007800030121     D RigaTf3         S              1    DIM(62)
007900040220      *
008000040220      *   S T R U T T U R E   D A T I
008100040220      *
008200010220     D*VISUALIZZAZIONE ORM - FIOR07R
008300010220     D Parm01          DS
008400010220     D  ppoe                          3  0
008500010220     D  pnor                          7  0
008600010220     D  pnsr                          2  0
008700010220     D  pnrv                          2  0
008800010220     D  psce                          1
008900010220     D  pfgs                          3  0
009000010220     D  ppor                          3  0
009100010220     D  pdtr                          8  0
009200010220     D  pmdb                         10
009300010220     D  pprb                         10
009400010220     D  pdts                          8  0
009500010220     D  prmp                          1
009600010220     D  pbrc                          1
009700010220     D  pref                          2
009800010220     D  pmio                          1
009900940804     D* DATI DA PGM RICHIAMANTE               - FNLS04R - FNLR36R -
010000940804     D PARAM0          DS
010100940808     D  PA0AAS                 1      4  0
010200940808     D  PA0LNP                 5      7  0
010300940808     D  PA0NRS                 8      9  0
010400940808     D  PA0NSP                10     16  0
010500941109     D  PA0F03                17     17
010600940907     D  PA0FLG                18     18
010700940808     D  PA0A9                 19     19
010800940808     D  PA0B5                 20     20
010900940808     D  PA075                 21     21
011000940808     D  PA098                 22     22
011100940808     D  PA0RSU                23     42
011200940808     D  PA0CML                43     43
011300940808     D  PA0SIM                44     46  0
011400940804      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
011500111020      * OBSOLETO --> Non ci sono più clienti abilitati al DDM <-- 20/10/2011
011600111020     D**!!!PA0CLI                47     47
011700941213      * PA0GIA indica se il pgm e' richiamato dalle giacenze
011800941213     D  PA0GIA                48     48
011900991022      *
012000991022      * PASSAGGIO DATI A INTERROGAZIONE DETTAGLIO COLLI - FNLS64R -
012100940808     D PARA64          DS
012200940808     D  P64AAS                 1      4  0
012300940808     D  P64LNP                 5      7  0
012400940808     D  P64NRS                 8      9  0
012500940808     D  P64NSP                10     16  0
012600940808     D  P64F03                17     17
012700940808     D  P64FLG                18     18
012800940808     D  P64PGM                19     28
012900940808     D  P64RSU                29     48
013000940808     D  P64CML                49     49
013100991022      *  PA0CLI indica se il pgm e' richiamato da un cliente (='S')
013200111020      * OBSOLETO --> Non ci sono più clienti abilitati al DDM <-- 20/10/2011
013300111020     D**!!!P64CLI                50     50
013400991022      *
013500991022      * PASSAGGIO A INTERROGAZIONE SPUNTE               - FNLR04R -
013600941230     D PARAM1          DS                  INZ
013700921013     D  VI1NFV                 1      5  0
013800921013     D  VI1LNP                 6      8  0
013900921013     D  VI1LNA                 9     11  0
014000921013     D  VI1NRS                12     13  0
014100921013     D  VI1NCD                14     20  0
014200921013     D  VI1NCA                21     27  0
014300921013     D  VI1NPS                28     29  0
014400921013     D  VI1NPG                30     30  0
014500921013     D  VI1FLG                31     31
014600921013     D  VI1PGM                32     38
014700941230     D  VI1FGS                39     41  0
014800101203     d* bolla
014900101203     d  vi1bolp               42     44  0
015000101203     d  vi1bors               45     46  0
015100101203     d  vi1bosp               47     53  0
015200101203     d  vi1boan               54     57  0
015300101203     D  vi1ERR                58     58
015400991022      *
015500991022      * PASSAGGIO A INTERROGAZIONE VARIAZIONI            - FNLR75R -
015600900927     D PARAM2          DS
015700940804     D  PA2AAS                14     17  0
015800940804     D  PA2LNP                18     20  0
015900940804     D  PA2NRS                21     22  0
016000940804     D  PA2NSP                23     29  0
016100991022      *  PREMUTO F3
016200940804     D  PA2F03                30     30
016300991022      *  FLG = "C" CHIUSURA FILES
016400940804     D  PA2FLG                31     31
016500940804     D  PA2PGM                32     41
016600940804     D  PA2DSA                42     61
016700940804     D  PA2DSP                63     70  0
016800940804     D  PA2RMN                71     85  0
016900940804     D  PA2LNA                86     88  0
017000940804     D  PA2LN1                89    108
017100940804     D  PA2RSD               109    143
017200950126     D  PA2GIA               144    144
017300950126     D  PA2FEL               145    147  0
017400950628     D  PA2TBO               148    148
017500991022      *
017600991022      * PASSAGGIO A INTERROGAZ.BOLLE ARRIVI:BOLLA LEGATA - FNLR36R -
017700920326     D PARAM4          DS
017800940808     D  PA4AAS                14     17  0
017900940808     D  PA4LNP                18     20  0
018000940808     D  PA4NRS                21     22  0
018100940808     D  PA4NSP                23     29  0
018200940810     D  PA4F03                30     30
018300940808     D  PA4FLG                31     31
018400991022      *
018500991022      * RICHIAMO PGM DI INTERROGAZIONE EVENTI             - FNLRB5R -
018600940804     D PARAB5          DS
018700940804     D  PB5AAS                14     17  0
018800940804     D  PB5LNP                18     20  0
018900940804     D  PB5NRS                21     22  0
019000940804     D  PB5NSP                23     29  0
019100940804     D  PB5F03                30     30
019200940804     D  PB5FLG                31     31
019300940804     D  PB5PGM                32     41
019400940804     D  PB5DSA                42     61
019500940804     D  PB5DSP                63     70  0
019600940804     D  PB5RMN                71     85  0
019700940804     D  PB5LNA                86     88  0
019800940804     D  PB5LN1                89    108
019900940804     D  PB5RSD               109    143
020000940804     D  PB5DCP               144    151  0
020100940804     D  PB5TMC               152    152
020200040220      *
020300040220     D WLBDAT          DS
020400040220     D  G02DAT                 1      8  0
020500040220     D  G02INV                 9     16  0
020600040220     D  G02ERR                17     17
020700040220     D  G02TGI                18     22  0
020800040220      *
020900040220     D                 DS
021000040220     D  DCTDCA                 1      8  0
021100040220     D  DCTAAC                 1      4  0
021200040220     D  DCTMGC                 5      8  0
021300040220      *
021400040220      *?definizione oggetto reclamo come spedizione?
021500040220     d ds_OggSped      ds                  inz
021600040220     d  BLPlnp                             inz
021700040220     d  BLPnrs                             inz
021800040220     d  BLPnsp                             inz
021900040220     d  BLPaas                             inz
022000040220      *
022100040220      * CAMPO UNICO COMPRENDENTE RAG.SOC.+AGGIUNTIVI 35 CARATTERI
022200040220     D                 DS
022300040220     D  V2CRSD                 1     35
022400040220     D  V2CNOT                36     70
022500040220     D  V2CNOM                 1     70
022600040220      *
022700040220     D TCUDS           DS
022800040220     D  F1                     1      1
022900040220     D  F3                     3      3
023000040220     D  F2                     2      2
023100040220     D  F4                     4      4
023200040220     D  F56                    5      6
023300040220
023400040220     d Status         sds
023500040220     d  SDSprm           *parms
023600040220
023700040220     D KPJBA         E DS
023800040220     d FIDNA1ds      e ds
023900040226      *
024000040226     d FIDNA2ds      e ds                  inz
024100040226     d  IA2mod       e                     inz('M')
024200040226     d  IA2tor       e                     inz('S')
024300991022      *
024400021218     D CnCr80        E DS
024500991022     D ut§dse0f      E DS
024600991022     D  TCU                  398    697    DIM(50)
024700991022     D  KCU                  698    847P 0 DIM(50)   PACKEVEN
024800991022      *
024900991022     D tnsc20ds      E DS
025000950102     D FNLR97        E DS
025100981023     D DSBL4E        E DS
025200060517     D*DSBL4F        E DS
025300060517     D DSBL4I        E DS
025400010220     D DSBL4M        E DS
025500991227     D OG143         E DS
025600930505     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
025700991022     D TNSD99DS      E DS                  INZ
025800900502     D DS3A          E DS
025900130319     D*DS1B          E DS
026000991022     D DS$2          E DS
026100991022     D DSCV          E DS
026200020402     D DSCC          E DS
026300111020     D**!!!DS3K          E DS
026400951219     D DS7K          E DS
026500090608     d ds5e          e ds
026600991022     D DS7PQRS       E DS
026700991022     D FNLG30DS      E DS
026800991022     D FNLR44DS      E DS
026900991022     D FNLV55DS      E DS
027000051124     D FNLV80DS      E DS
027100991022     D FIDN00DS      E DS
027200091216     D trulintds     E DS
027300110601     D fnlv79ds      E DS
027400991025     d TIBS69DS      E DS                  INZ
027500991025     d DS_cnaco      E DS                  extname(CNACO00F)
027600991025     d DS_cnind      E DS                  extname(CNIND00F)
027700991025     d DS_cnclp      E DS                  extname(CNCLP00F)
027800991025     d DS_fncls      E DS                  extname(FNCLS00F)
027900021218     d FNLRB3Ds      E DS
028000030121     d dTba          e ds
028100031119     d dAr5Gen       e ds
028200030121     d dAr5Ban       e ds
028300030703     d dAr5Bnb       e ds
028400130927     d dAr5emd       e ds
028500030121     d Tibs02Ds      e ds
028600110705     d dsbl4a        e ds
028700130920
028800140131     d TRULORSds     e ds
028900140224     d fipctcords    e ds
029000130920
029100040714      * ds per il controllo della presenza di CA per la spedizione richiesta
029200040714     d FIDN12DS      e ds
029300040714     d  skKey                 26    305    dim(20)
029400040714     d  skAnn                306    325    dim(20)
029500040714     d  skDca                326    485  0 dim(20)
029600040714     d  skFca                486    545  0 dim(20)
029700040714     d  skDch                546    705  0 dim(20)
029800040714     d  skCch                706    745    dim(20)
029900040714
030000040714     d dsKey           ds
030100040714     d  dsaac                         4  0
030200040714     d  dsfil                         3  0
030300040714     d  dsnca                         7  0
030400040220      *
030500040220      *   C O S T A N T I
030600040220      *
030700971212     D CMANUA          C                   CONST('Inser.MANUALE in ')
030800971212     D CAUTOM          C                   CONST('Attribuito in xxxx')
030900971212     D CSTAMP          C                   CONST(' Stampa XX')
031000040220      *
031100040220      *   V A R I A B I L I
031200040220      *
031300040220     D kpdpdr          S                   LIKE(APDpdr)
031400040220     D kpdtip          S                   LIKE(APDtip)
031500041028     D wdam            S                   LIKE(blpdam)
031600041028     D wdcm            S                   LIKE(blpdcm)
031700041028     D wdce            S                   LIKE(blpdce)
031800041028     D wnrc            S                   LIKE(blpnrc)
031900041028     D wdcp            S                   LIKE(blpdcp)
032000041028     D wcca            S                   LIKE(blpcca)
032100041028     D whmc            S                   LIKE(blphmc)
032200041028     D wtmc            S                   LIKE(blptmc)
032300041028     D wdet            S                   LIKE(blpdet)
032400041028     D wdut            S                   LIKE(blpdut)
032500041028     D wddc            S                   LIKE(blpddc)
032600041028     D wndc            S                   LIKE(blpndc)
032700040220
032800040220     d wtba            s                   Like(§Ar5Tba)
032900040220     d wdestba         s                   Like(V1dTba)
033000991022     D DATA_eur        S               D   DATFMT(*eur)
033100140131     D wrkoggiiso      s               d
033200991022     D W0080           S              8  0
033300991025     D W0040           S              4  0
033400991022     D Wsvn            S                   LIKE(AR7svn)
033500991022     D Wvan            S                   LIKE(AR7van)
033600991022     D WDIV            S                   LIKE(AR6div)
033700991022     D KTRC            S                   LIKE(AR6TRC)
033800111020     D**!!!$3kinb          S                   LIKE(§3kinb)
033900090603     D** $3kctb          S                   LIKE(§3kctb)
034000020225     d lnantw          S                   LIKE(§ogntw)
034100020225     d lnpntw          S                   LIKE(§ogntw)
034200991022     D KAAS            S                   LIKE(BLPaas)
034300991022     D Klnp            S                   LIKE(BLPlnp)
034400991022     D Knrs            S                   LIKE(BLPnrs)
034500991022     D Knsp            S                   LIKE(BLPnsp)
034600991022     D Kcod            S                   LIKE(TBLcod)
034700991022     D Kkey            S                   LIKE(TBLkey)
034800991022     D Kkcc            S                   LIKE(INDkcc)
034900991022     D Kksc            S                   LIKE(INDksc)
035000991022     D Comaan          S                   LIKE(LBLaan)
035100991022     D Wtfp            S                   LIKE(D55tfp)
035200991022     D Wtstdec         S                   LIKE(§CVfdc)
035300091125     D W_cdi           S                   LIKE(§ar5cdi)
035400991025     D wtibs69r        s              1
035500991022     D W017A           s             17
035600991022     D w0130           s             13  0
035700991022     D w0133           s             13  3
035800991022     D IMPeditato      s             13
035900131007     d wx              s              3  0
036000131007     d wy              s              3  0
036100131007     d wabif9          s              1
036200131007
036300021218     D CF2413          C                   CONST('F24=AlTasti(1/3)')
036400021218     D CF2423          C                   CONST('F24=AlTasti(2/3)')
036500021218     D CF2433          C                   CONST('F24=AlTasti(3/3)')
036600021218     D CF2412          C                   CONST('F24=AlTasti(1/2)')
036700021218     D CF2422          C                   CONST('F24=AlTasti(2/2)')
036800040220
036900930115     I****************************************************************
037000941028     I* INDICATORI UTILIZZATI
037100930115     I****************************************************************
037200961111     I* 01 - BOLLA RIENTRO
037300961111     I* 02 - BOLLA FRNACO FRONTIERA
037400990118     I* 03 - ABILITA F14 DI INTERROGAZIONE GIACENZA
037500991021     I* 04 - ABILITA F13 DI TASSAZIONE 1a BOLLA
037600960523     I* 05 - DISABILITA F7=ANOMALIE
037700991021     I* 06 - ABILITA F4 DI TASSAZIONE 2a BOLLA
037800930115     I* 07 - ESISTE LEGAME BOLLA ABILITA CMD18
037900970227     I* 08 - SI DTT IN ALTA INTENSITA'
038000971212     I* 09 - IMPORT EXPORT SDI
038100990118     I* 10 - ABILITA F10 DI INTERROGAZ. VARIAIZONI BOLLE
038200991123     I* 11 - EMETTO SCRITTA BOLLO o IVA IN TASSAZIONE 2a BOLLA
038300990118     I* 12 - ABILITA INTERROGAZIONE COMUNICAZIONI ANOMALIA
038400980704     I* 13 - EMETTO RIFERIMENTO PARTNER PARTENZA
038500000216     I* 14 - lna dd
038600000216     I* 15 - ABILITA F23
038700941028     I* 16 - ESISTE PARTICOLARITA' CONTRASSEGNO
038800941028     I* 17 - ESISTE PARTICOLARITA' GIACENZA
038900941028     I* 18 - ESISTE PARTICOLARITA' CONSEGNA
039000941028     I* 19 - ESISTE PARTICOLARITA' VARIE
039100941028     I* 20 - SFLDSP
039200941028     I* 21 - SFLDSPCTL - SFLCLR
039300990118     I* 22 - ABILITA F7, F19 E F20
039400941028     I* 23 - SFLEND
039500990118     I* 24 - ABILITA VISUAL. CONTRASSEGNO
039600990118     I* 25 - ABILITA F24 LATRI TASTI
039700951220     I* 26 - PULIZIA SFL LS05S03
039800040220      * 27   Abilita F1  = Aggiunta in reclamo
039900040220      * 28   Abilita F16 = Gestione reclamo
040000941028     I* 31 - PREMUTO TASTO FUNZIONALE
040100941028     I* 33 - ON = SPEDIZIONE IN FRANCO
040200941028     I* 33 - OFF = SPEDIZIONE IN ASSEGNATO
040300930115     I* 34 - C'E' LEGAME MA NON ABILITA CMD18
040400930115     I* 44 - INDICA A VIDEO SPEDIZIONE GIACENTE APERTA
040500010220     I* 55 - ABILITA F08 (interrogazione orm)
040600021218     I* 77 - NOTE SUPERMERCATI
040700941028     I* 80 - USATO NEL VIDEO LEGAMI
040800941028     I* 81 - USATO NEL VIDEO LEGAMI
040900941028     I* 82 - USATO NEL VIDEO LEGAMI
041000941028     I* 83 - USATO NEL VIDEO LEGAMI
041100941028     I* 93 - USATO NEL VIDEO LEGAMI
041200930505     I* 94 - EMETTO CMD23 PER CALCOLARE E VISUALIZZARE DATA CONS.
041300930505     I*      PREVISTA E IL TOTALE ORE DI RITARDO
041400930701     I* 95 - ESISTONO EVENTI DA INTERROGARE
041500930527     I* 98 - DATA CONSEGNA MERCE >< 0
041600941021     I* 99 - GENERICO, UTILIZZABILE A PIACERE
041700930115     I****************************************************************
041800000000     C     *ENTRY        PLIST
041900000000     C                   PARM                    KPJBA
042000040220     c                   parm                    FIDNA1ds
042100040220      *
042200940804     C                   MOVEL     KPJBU         PARAM0
042300981123     C**
042400981123     C                   Z-ADD     1             CODUT
042500981123     C                   CALL      'X§PARUT'
042600991022     C                   PARM                    ut§dse0f
042700021218     C                   MOVEL     REC80         CNCR80
042800981123     C*--- RICERCA CAPOCONTI
042900981123     C                   DO        50            X                 2 0
043000981123     C                   MOVE      TCU(X)        TCUDS
043100981123     C     F56           CABNE     'CG'          END1
043200981123     C     F4            COMP      '1'                                    21
043300981123     C     F4            COMP      '2'                                    22
043400981123     C     F4            COMP      '3'                                    23
043500981123     C     F4            COMP      '6'                                    27
043600981123     C** 1 CLIENTI   21
043700981123     C** 2 FORNITORI 22
043800981123     C** 3 AGENTI    23
043900981123     C     F3            COMP      '0'                                242425
044000981123     C     F3            COMP      'I'                                    26
044100981123     C** 0 ITALIA   25
044200981123     C** 1 ESTERO   24
044300981123     ** I CAPO CONTO IVA
044400981123     C   21
044500981123     CAN 24              Z-ADD     KCU(X)        KCE               4 0
044600981123     C   21
044700981123     CAN 25              Z-ADD     KCU(X)        KCI               4 0
044800981123     C   22
044900981123     CAN 24              Z-ADD     KCU(X)        KFE               4 0
045000981123     C   22
045100981123     CAN 25              Z-ADD     KCU(X)        KFI               4 0
045200981123     C   23
045300981123     CAN 24              Z-ADD     KCU(X)        KAE               4 0
045400981123     C   23
045500981123     CAN 25              Z-ADD     KCU(X)        KAI               4 0
045600981123     C   26              Z-ADD     KCU(X)        KIVA              4 0
045700981123     C   27              Z-ADD     KCU(X)        KBNA              4 0
045800981123     C     END1          TAG
045900981123     C                   END
046000981123     C                   SETOFF                                       212223
046100981123     C                   SETOFF                                       242526
046200981123     C                   SETOFF                                       27
046300940804     C*
046400990701     C                   MOVEL     PA0RSU        V1CRSU
046500940808     C                   Z-ADD     1             CODUT             1 0
046600981123     C**
046700971202     C* DETERMINO LA DATA DEL GIORNO
046800971202     C                   TIME                    W0140            14 0
046900971202     C                   MOVE      W0140         WDTGIO            8 0
047000971202     C                   Z-ADD     WDTGIO        G02DAT
047100971202     C                   MOVEL     *BLANK        G02ERR
047200971202     C                   CALL      'XSRDA8'
047300971202     C                   PARM                    WLBDAT
047400971202     C                   MOVEL     G02INV        WDATEU            8 0
047500991022      *
047600991022      * CARICO LA TABELLA $2
047700991022     C                   MOVEL     '$2'          KCOD
047800991022     C                   MOVEL(P)  1             KKEY
047900991022     C     KTAB1         CHAIN     TABEL00F                           31
048000991022     C  N31              MOVEL     TBLUNI        DS$2
048100991022      *
048200991022      * CARICO IN SCHIERA LA TABELLA CC
048300941021     C                   Z-ADD     *ZERO         WA
048400941021     C                   CLEAR                   CC1
048500941021     C                   CLEAR                   CC2
048600020329     C                   CLEAR                   CC3
048700941021     C                   MOVEL     'CC'          KCOD
048800941021     C     KTAB2         SETLL     TABEL00F
048900941021     C     KTAB2         READE     TABEL00F                               99
049000941021     C     *IN99         DOWEQ     *OFF
049100941021     C     TBLKEY        ANDNE     *BLANKS
049200941021     C     TBLKEY        ANDNE     *ZERO
049300941021     C                   ADD       1             WA
049400941021     C                   MOVE      TBLKEY        CC1(WA)
049500941021     C                   MOVEL     TBLUNI        CC2(WA)
049600020402     C                   MOVEL(P)  TBLUNI        DSCC
049700020402     C                   MOVE      §CCDFE        CC3(WA)
049800941021     C     KTAB2         READE     TABEL00F                               99
049900941021     C                   ENDDO
050000020329      *
050100020329      * CARICO IN SCHIERA LA TABELLA CB
050200020329     C                   Z-ADD     *ZERO         WA
050300020329     C                   CLEAR                   CB1
050400020329     C                   CLEAR                   CB2
050500020329     C                   MOVEL     'CB'          KCOD
050600020329     C     KTAB2         SETLL     TABEL00F
050700020329     C     KTAB2         READE     TABEL00F                               99
050800020329     C     *IN99         DOWEQ     *OFF
050900020329     C     TBLKEY        ANDNE     *BLANKS
051000020329     C     TBLKEY        ANDNE     *ZERO
051100020329     C                   ADD       1             WA
051200020329     C                   MOVE      TBLKEY        CB1(WA)
051300020329     C                   MOVEL     TBLUNI        CB2(WA)
051400020329     C     KTAB2         READE     TABEL00F                               99
051500020329     C                   ENDDO
051600991022      *
051700991022      * Disabilito F7=Anomalie se in elenco pgm chiamanti c'è fnlr44r
051800991022      * Pa0flg="N" -- > Disabilitare F7
051900991022      * Pa0flg="R" -- > Disabilitare F7 - PGM RICHIAMATO DA FNLR36R
052000991022     c                   IF        PA0FLG = 'N'  or  PA0FLG = 'R'
052100991022     C                   eval      *in05 = *on
052200991022     C                   ENDIF
052300940802      * Testo il flag che mi dice se sono richiamato da un cliente
052400940802     C                   SETON                                        22
052500111020
052600140319     c* Tasto funz orari servizi sempre abilitato
052700131007     c                   eval      wabif9='S'
052800921013     C*
052900900511     C* INIZIALIZZO CAMPI ANCHE PER MEMORIZZARE BOLLA CHE NON USO
053000900427     C                   Z-ADD     0             BLPNRS
053100900427     C                   Z-ADD     0             BLPNSP
053200900427     C                   Z-ADD     0             BLPLNA
053300900427     C                   Z-ADD     0             BLPRMN
053400930701     C                   CLEAR                   PARAM1
053500930701     C                   CLEAR                   PARAM2
053600930701     C                   CLEAR                   PARAM4
053700940810     C                   CLEAR                   PARA64
053800940810     C                   CLEAR                   PARAB5
053900991022      *----------------------------------------------------------
054000991022      * CARICO BOLLA
054100900516     C                   EXSR      CARBLP
054200940923     C*
054300940923     C* SE LA BOLLA E' INESISTENTE VADO A FINE PROGRAMMA
054400940923     C     PA0NSP        IFEQ      *ZERO
054500940923     C                   GOTO      FINE
054600940923     C                   ENDIF
054700040220      *
054800040220      *?Abilito F1=Aggiunta in Reclami?
054900040220      * (se richiamato da pgm. FIDNA1R...)
055000040220     c                   eval      *in27  = *off
055100040303     c                   if            SDSprm >  1
055200040308     c                             and IA1ins = ' '
055300040220     c                   eval      *in27  = *on
055400040220     c                   endif
055500040220      *
055600040220      *?Abilito F16=Visualizzazione Reclami?
055700040220      * (se richiamato da pgm. FIDNA1R...)
055800040220     c                   eval      TGDtor  = 'S'
055900040220     c                   eval      TGDogg  = ds_OggSped
056000040220     c     K02TGD02      setll     FITGD000
056100040302     c                   eval      *in28   = (%equal(FITGD02L)   and
056200040303     c                                           (SDSprm =  1
056300040303     c                                        or (SDSprm >  1
056400040303     c                                        and IA1ins <> 'N')))
056500900427     C*
056600931115     C* SOLO SE C'E' CONTRASSEGNO VISUALIZZO IL TASTO CMD22
056700931116     C* VERIFICO NELL'ARCHIVIO SE C'E' CONTRASSEGNO
056800990118     C                   SETOFF                                       2412
056900011120     C     KBLP3         SETLL     FNCSB01L
057000011120     c                   if        %equal(fncsb01l)
057100051115     C     KBLP3         SETLL     Fiar901L
057200051115     c                   if        %equal(fiar901l)
057300011120     c                   eval      *in24 = *on
057400011120     c                   endif
057500011120     c                   endif
057600990118     C* CONTROLLO SE C'E' DANNO
057700990125     C                   EXSR      CTRDAN
057800940322      *
057900940802      * EMETTO IL FORMATO DI RIEPILOGO DELLA BOLLA
058000940802     C                   EXSR      RIEPIL
058100940802      *
058200940802      * CMD 3 - FINE LAVORO TORNO A SUBFILE
058300940808     C     *INKC         IFEQ      *ON
058400940808     C     WF03          ORNE      *BLANKS
058500941109     C                   MOVEL     '1'           PA0F03
058600940808     C                   GOTO      FINE
058700940808     C                   ENDIF
058800940802      *
058900940802      * PER F15 VISUALIZZO TUTTA LA BOLLA, PER INVIO ESCO
059000940802     C  NKP              GOTO      FINE
059100931115     C*
059200931115     C                   SETOFF                                       25
059300940804     C                   MOVEL     'S'           WRIEM
059400021218      *
059500021218      * Riempio il campone in fondo alla videata con i tasti scelti
059600021218     C                   exsr      Ges_TastiFun
059700021218     c                   eval      FD01 = FDxx
059800021218     c                   eval      F24D01 = F24Dxx
059900900427     C     FOR01         TAG
060000940804     C     WRIEM         IFEQ      'S'
060100940804     C                   WRITE     LS05T01
060200940804     C                   MOVEL     *BLANKS       WRIEM
060300940804     C                   ENDIF
060400940804     C*
060500021218      *
060600021218      * Imposto la stringa comandi del 'giro' successivo
060700021218     C                   IF        *inKY = *on and $Loop > 1
060800021218     c                   Exsr      Ges_F24
060900021218     c                   eval      FD01 = FDxx
061000021218     c                   eval      F24D01 = F24Dxx
061100021218     C                   ENDIF
061200021218      *
061300940804     C                   EXFMT     LS05D01
061400940808     C                   MOVEL     *BLANKS       WF03
061500931115      *
061600990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
061700931115     C   KY*IN25         IFEQ      *ON
061800931115     C                   SETOFF                                       25
061900931115     C                   ELSE
062000931115     C                   SETON                                        25
062100931115     C                   ENDIF
062200931115     C   KY              GOTO      FOR01
062300910806     C*
062400900530     C* CMD 3 - FINE LAVORO TORNO A SUBFILE
062500900530     C   KC              GOTO      FINE
062600921013     C*
062700921013     C* CMD12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
062800921013     C   KL              GOTO      FINE
062900911105     C*
063000921013     C* TASTI DI FUNZIONE VARI
063100921013     C                   EXSR      COMAND
063200940808     C*
063300940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
063400940808     C     WF03          IFNE      *BLANKS
063500941109     C                   MOVEL     '1'           PA0F03
063600940808     C                   GOTO      FINE
063700940808     C                   ENDIF
063800940808     C*
063900921013     C   31              GOTO      FOR01
064000921013     C*
064100900427     C* SE TUTTO OK EMETTO FORMATO2
064200931115     C                   SETOFF                                       25
064300940804     C                   MOVEL     'S'           WRIEM
064400021218      *
064500021218      * Riempio il campone in fondo alla videata con i tasti scelti
064600021218     C                   exsr      Ges_TastiFun
064700021218     c                   eval      FD02 = FDxx
064800021218     c                   eval      F24D02 = F24Dxx
064900900427     C     FOR02         TAG
065000940804     C     WRIEM         IFEQ      'S'
065100940804     C                   WRITE     LS05T01
065200940804     C                   MOVEL     *BLANKS       WRIEM
065300940804     C                   ENDIF
065400940804     C*
065500021218      * Imposto la stringa comandi del 'giro' successivo
065600021218     C                   IF        *inKY = *on and $Loop > 1
065700021218     c                   Exsr      Ges_F24
065800021218     c                   eval      FD02 = FDxx
065900021218     c                   eval      F24D02 = F24Dxx
066000021218     C                   ENDIF
066100021218      *
066200940804     C                   EXFMT     LS05D02
066300940808     C                   MOVEL     *BLANKS       WF03
066400931115      *
066500990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
066600931115     C   KY*IN25         IFEQ      *ON
066700931115     C                   SETOFF                                       25
066800931115     C                   ELSE
066900931115     C                   SETON                                        25
067000931115     C                   ENDIF
067100931115     C   KY              GOTO      FOR02
067200910806     C*
067300900427     C* CMD12 - RITORNO VIDEATA 1
067400900427     C   KL              GOTO      FOR01
067500940923     C*
067600940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
067700940923     C                   EXSR      CTRD02
067800940923     C   31              GOTO      FOR02
067900921013     C*
068000921013     C* TASTI DI FUNZIONE VARI
068100921013     C                   EXSR      COMAND
068200940808     C*
068300940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
068400940808     C     WF03          IFNE      *BLANKS
068500941109     C                   MOVEL     '1'           PA0F03
068600940808     C                   GOTO      FINE
068700940808     C                   ENDIF
068800921013     C   31              GOTO      FOR02
068900920429     C*
069000910806     C*
069100930112     C* SE ESISTONO DATI ENTRO IN TERZA VIDEATA
069200931115     C                   SETOFF                                       25
069300940804     C                   MOVEL     'S'           WRIEM
069400021218      *
069500021218      * Riempio il campone in fondo alla videata con i tasti scelti
069600021218     C                   exsr      Ges_TastiFun
069700021218     c                   eval      FD03 = FDxx
069800021218     c                   eval      F24D03 = F24Dxx
069900930112     C     FOR03         TAG
070000940804     C     WRIEM         IFEQ      'S'
070100940804     C                   WRITE     LS05T01
070200940804     C                   MOVEL     *BLANKS       WRIEM
070300940804     C                   ENDIF
070400940804     C*
070500951219     C                   Z-ADD     1             REC3
070600021218      *
070700021218      * Imposto la stringa comandi del 'giro' successivo
070800021218     C                   IF        *inKY = *on and $Loop > 1
070900021218     c                   Exsr      Ges_F24
071000021218     c                   eval      FD03 = FDxx
071100021218     c                   eval      F24D03 = F24Dxx
071200021218     C                   ENDIF
071300951219     C                   WRITE     LS05Z03
071400951219     C                   EXFMT     LS05C03
071500940808     C                   MOVEL     *BLANKS       WF03
071600931115      *
071700990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
071800931115     C   KY*IN25         IFEQ      *ON
071900931115     C                   SETOFF                                       25
072000931115     C                   ELSE
072100931115     C                   SETON                                        25
072200931115     C                   ENDIF
072300931115     C   KY              GOTO      FOR03
072400910806     C*
072500900507     C* CMD12 - RITORNO VIDEATA 2
072600900507     C   KL              GOTO      FOR02
072700930114     C*
072800930114     C* TASTI DI FUNZIONE VARI
072900930114     C                   EXSR      COMAND
073000940808     C*
073100940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
073200940808     C     WF03          IFNE      *BLANKS
073300941109     C                   MOVEL     '1'           PA0F03
073400940808     C                   GOTO      FINE
073500940808     C                   ENDIF
073600930114     C   31              GOTO      FOR03
073700900504     C*
073800000000     C     FINE          TAG
073900971202     C*
074000971202     C                   MOVEL     'C'           D55TLA
074100971202     C                   CALL      'FNLV55R'
074200991022     C                   PARM                    FNLV55DS
074300991025      *
074400991025     C                   IF        Wtibs69r  <>  *BLANKS
074500991025     C                   eval      I69TLA  = 'C'
074600991025     C                   call      'TIBS69R'
074700991025     C                   parm                    TIBS69DS
074800991025     c                   ENDIF
074900040714     C*
075000040714     C                   MOVEL     'C'           I12TLA
075100040714     C                   CALL      'FIDN12R'
075200040714     C                   PARM                    FIDN12DS
075300091216     C*
075400091216     C                   MOVEL     'C'           Iinttla
075500091216     C                   CALL      'TRULINTR'
075600091216     C                   PARM                    trulintds
075700040714      *
075800991025      *
075900930614     C* REIMPOSTO KPJBU
076000940810     C   KC              MOVEL     '1'           PA0F03
076100940804     C                   MOVEL     PARAM0        KPJBU
076200930614     C*
076300000000     C                   SETON                                        LR
076400940808     C**************************************************************************
076500940808     C*    COMANDI UGUALI NELLE 2 VIDEATE
076600940808     C**************************************************************************
076700921013     C     COMAND        BEGSR
076800931115     C*
076900921013     C                   SETOFF                                       31
077000040220      *
077100040220      *?F1 - Aggiunta bolla in reclamo?
077200040220if  1c                   if        *inKA   = *on
077300040220     c                   clear                   FIDNA1ds
077400040220     c                   eval      IA1ins  = 'S'
077500040220     c                   eval      IA1tor  = 'S'
077600040220     c                   eval      IA1ogg  = ds_OggSped
077700040220     c                   goto      Fine
077800040220     c*****              return
077900040220e   1c                   endif
078000040220      *
078100990118     C* F5 - INTERROGAZIONE EVENTI
078200040220if  1C     *INKE         IFEQ      *ON
078300930526     C                   SETON                                        31
078400941213     C                   CLEAR                   PARAB5
078500940804     C                   MOVEL     PA0AAS        PB5AAS
078600940804     C                   MOVEL     PA0LNP        PB5LNP
078700940804     C                   MOVEL     PA0NRS        PB5NRS
078800940804     C                   MOVEL     PA0NSP        PB5NSP
078900940804     C                   MOVEL     'FNLS05R'     PB5PGM
079000940804     C                   MOVEL     V1CRSU        PB5DSA
079100940808     C                   MOVEL     V1CDSP        PB5DSP
079200940804     C                   MOVEL     V2CRMN        PB5RMN
079300940804     C                   MOVEL     V1CLNA        PB5LNA
079400940804     C                   MOVEL     V1DLNA        PB5LN1
079500940804     C                   MOVEL     V2CRSD        PB5RSD
079600940804     C                   MOVEL     V3CDCP        PB5DCP
079700940804     C                   MOVEL     V3CTMC        PB5TMC
079800940804     C                   MOVEL     PARAB5        KPJBU
079900940804     C                   CALL      'FNLRB5R'
080000930526     C                   PARM                    KPJBA
080100940808     C                   MOVEL     '1'           PA0B5
080200940808     C                   MOVEL     'S'           WRIEM
080300040220e   1C                   ENDIF
080400930526     C*
080500921013     C* CMD 7 - VISUALIZZA ANOMALIE
080600040220if  1C     *INKG         IFEQ      *ON
080700991022     C                   CLEAR                   FNLR44DS
080800960523     C                   MOVEL     PA0AAS        D44AAS
080900960523     C                   MOVEL     PA0LNP        D44FIL
081000960523     C                   MOVEL     PA0NRS        D44NRS
081100960523     C                   MOVEL     PA0NSP        D44NSP
081200960523     C                   MOVEL     'P'           D44TBO
081300960523     C                   MOVE      'I'           D44MOD
081400960523     C                   SETON                                        31
081500991022     C                   MOVEL     FNLR44DS      KPJBU
081600960523     C                   CALL      'FNLR44R'
081700960523     C                   PARM                    KPJBA
081800940808     C                   MOVEL     'S'           WRIEM
081900040220e   1C                   ENDIF
082000010220     C*
082100010220     C* CMD 8 - VISUALIZZA ORM
082200040220if  1C     *INKH         IFEQ      *ON
082300010220
082400010220     C                   clear                   PARM01
082500010220     C                   move      v1cpoe        ppoe
082600010220     C                   move      v1cnor        pnor
082700010220     C                   move      v1cnsr        pnsr
082800010220     C                   move      v1cnrv        pnrv
082900010220     C                   move      '5'           psce
083000010220     C                   move      v1clnp        pfgs
083100010220     C                   movel(p)  PARM01        KPJBU
083200010220
083300010220     C                   call      'FIOR07R'
083400010220     C                   parm                    KPJBA
083500010220     C                   MOVEL     'S'           WRIEM
083600010220     C                   SETON                                        31
083700010220
083800040220e   1C                   ENDIF
083900130912c
084000130912     c* F9 - Orari servizi
084100130912     c                   if        *inki=*on
084200131007     c                   if        wabif9='S'
084300130930     c                   exsr      sr_orarisr
084400131007     c                   endif
084500130927     C                   MOVEL     'S'           WRIEM
084600130927     C                   SETON                                        31
084700130912e   1C                   ENDIF
084800021218     C*
084900021218     C* F11 - Note supermercati
085000040220if  1C     *INKK         IFEQ      *ON
085100021218     C     *IN77         ANDEQ     *ON
085200021218      * imposto i campi 'bolla' della ds
085300021218     C                   clear                   FnLrb3ds
085400030122     c                   Movel     v1cRsu        Rb3Rsu
085500030121     C                   z-add     SimFel        RB3Fil
085600021218     C                   z-add     V1CAAS        RB3AAS
085700030121     C                   z-add     V1CLP1        RB3LNP
085800021218     C                   z-add     V1CNRS        RB3NRS
085900021218     C                   z-add     V1CNSP        RB3NSP
086000021218     C                   z-add     V1CDSP        RB3DSP
086100021218     C                   z-add     BLPRMN        RB3RMN
086200021218     C                   z-add     V1CLNA        RB3LNA
086300021218     C                   movel     BLPRSD        RB3RSD
086400021218     C                   z-add     V1CPKF        RB3PKF
086500021218     C                   z-add     V1CNCL        RB3NCL
086600021218     C                   z-add     V1CVLF        RB3VLB
086700030703      * bancali - part.varie "B"
086800040220If  2c                   If        %subst(BlpGva:1:1) = 'B'
086900030121     C                   z-add     §Ar5Tba       RB3Tba
087000030121     C                   z-add     V1cNba        RB3Nba
087100030121     C                   z-add     §Ar5Tb2       RB3Tb2
087200030121     C                   z-add     V1cNb2        RB3Nb2
087300030703     c                   Movel     V2cGva        Rb3Gva
087400040220e   2c                   EndIf
087500030703      * bancali - part.varie "O"
087600040220If  2c                   If        %subst(BlpGva:1:1) = 'O'
087700030703     C                   z-add     §Ar5bTba      RB3Tba
087800030703     C                   z-add     V1cNba        RB3Nba
087900030703     C                   z-add     §Ar5bTb2      RB3Tb2
088000030703     C                   z-add     V1cNb2        RB3Nb2
088100030703     c                   Movel     V2cGva        Rb3Gva
088200030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
088300030703     c                   Z-Add     §Ar5bCor      Rb3Cor
088400040220e   2c                   EndIf
088500021218      * richiamo PGM per note supermercati
088600021218     C                   call      'FNLRB3R'
088700030122     c                   Parm                    Kpjba
088800030121     C                   PARM                    Fnlrb3Ds
088900021218      *
089000030703     c                   Eval      *In31 = *On
089100021218     C                   MOVEL     'S'           WRIEM
089200040220e   1C                   ENDIF
089300921013     C*
089400921013     C* CMD14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
089500040220if  1C     *INKN         IFEQ      *ON
089600930701     C     *IN03         ANDEQ     *ON
089700941213     C     PA0GIA        ANDEQ     *BLANK
089800991022     C                   CLEAR                   FNLG30DS
089900941213     C                   MOVEL     'P'           D30POA
090000941213     C                   MOVEL     PA0AAS        D30AAS
090100941213     C                   MOVEL     PA0LNP        D30LNP
090200941213     C                   MOVEL     PA0NRS        D30NRS
090300941213     C                   MOVEL     PA0NSP        D30NSP
090400991022     C                   MOVEL     FNLG30DS      KPJBU
090500930701     C                   SETON                                        31
090600941213     C                   CALL      'FNLG30C'
090700921013     C                   PARM                    KPJBA
090800940808     C                   MOVEL     'S'           WRIEM
090900040220e   1C                   ENDIF
091000040220      *
091100040220      *?F16 - Gestione reclami?
091200040226if  1c                   if        *inKQ   = *on
091300040226     c                   reset                   FIDNA2ds
091400040226if  2c                   if        SDSprm  > 1
091500040226     c                   eval      IA2mod  = 'I'
091600040226e   2c                   endif
091700040226     c                   eval      IA2ogg  = ds_OggSped
091800040226     c                   call      'FIDNA2R'
091900040226     c                   parm                    KPJBA
092000040226     c                   parm                    FIDNA2ds
092100040226     c                   movel     OA2f03        WF03
092200040226     c                   movel     'S'           WRIEM
092300051201     C                   SETON                                        31
092400040220e   1c                   endif
092500921013     C*
092600921013     C* CMD18 - LEGAME BOLLA: VADO IN INTERROG. BOLLE ARRIVI
092700040220if  1C     *INKS         IFEQ      *ON
092800921013     C                   EXSR      CMD18
092900921013     C                   SETON                                        31
093000941222     C                   MOVEL     'S'           WRIEM
093100040220e   1C                   ENDIF
093200921013     C*
093300921013     C* CMD21 - DETTAGLIO SEGNACOLLI
093400040220if  1C     *INKV         IFEQ      *ON
093500940808     C                   MOVEL     PA0AAS        P64AAS
093600940808     C                   MOVEL     PA0LNP        P64LNP
093700940808     C                   MOVEL     PA0NRS        P64NRS
093800940808     C                   MOVEL     PA0NSP        P64NSP
093900940808     C                   MOVEL     *BLANKS       P64F03
094000940808     C                   MOVEL     *BLANKS       P64FLG
094100940808     C                   MOVEL     *BLANKS       P64PGM
094200940808     C                   MOVEL     'FNLS05R'     P64PGM
094300940808     C                   MOVEL     V1CRSU        P64RSU
094400940808     C                   MOVEL     PA0CML        P64CML
094500111020     C**!!!              MOVEL     PA0CLI        P64CLI
094600940808     C                   MOVEL     PARA64        KPJBU
094700930701     C                   SETON                                        31
094800940804     C                   CALL      'FNLS64R'
094900921013     C                   PARM                    KPJBA
095000940808     C                   MOVEL     KPJBU         PARA64
095100940808     C                   MOVEL     P64F03        WF03
095200940808     C                   MOVEL     'S'           WRIEM
095300040220e   1C                   ENDIF
095400921013     C*
095500990118     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
095600040220do  1C   KJ              DO
095700930526     C                   SETON                                        31
095800940804     C                   MOVEL     PA0AAS        PA2AAS
095900940804     C                   MOVEL     PA0LNP        PA2LNP
096000940804     C                   MOVEL     PA0NRS        PA2NRS
096100940804     C                   MOVEL     PA0NSP        PA2NSP
096200940804     C                   MOVEL     *BLANKS       PA2F03
096300940804     C                   MOVEL     *BLANKS       PA2FLG
096400940804     C                   MOVEL     *BLANKS       PA2PGM
096500940804     C                   MOVEL     'FNLS05R'     PA2PGM
096600940804     C                   MOVEL     V1CRSU        PA2DSA
096700940808     C                   MOVEL     V1CDSP        PA2DSP
096800940804     C                   MOVEL     V2CRMN        PA2RMN
096900940804     C                   MOVEL     V1CLNA        PA2LNA
097000940804     C                   MOVEL     V1DLNA        PA2LN1
097100940804     C                   MOVEL     V2CRSD        PA2RSD
097200950628     C                   MOVEL     'P'           PA2TBO
097300941222     C                   MOVEL     PA0SIM        PA2FEL
097400940804     C                   MOVEL     PARAM2        KPJBU
097500940804     C                   CALL      'FNLR75R'
097600921013     C                   PARM                    KPJBA
097700940808     C                   MOVEL     '1'           PA075
097800940808     C                   MOVEL     'S'           WRIEM
097900040220e   1C                   ENDDO
098000921013     C*
098100990118     C* F19 - INTERROGA SPUNTE
098200040220if  1C     *INKT         IFEQ      *ON
098300930701     C                   SETON                                        31
098400101203     C                   MOVEL     PA0AAS        vi1boan
098500101203     C                   MOVEL     PA0LNP        vi1bolp
098600101203     C                   MOVEL     PA0NRS        vi1bors
098700101203     C                   MOVEL     PA0NSP        vi1bosp
098800950116     C                   MOVEL     BLPFLS        VI1LNP
098900940804     C                   MOVEL     PA0NRS        VI1NRS
099000941230     C                   MOVEL     9             VI1NPG
099100930701     C                   MOVEL     99            VI1NPS
099200101203     C                   MOVEL     'FNLS05R'     VI1PGM
099300930701     C                   MOVEL     '1'           VI1FLG
099400930701     C                   Z-ADD     BLPLNA        VI1LNA
099500930701     C*
099600040220if  2C     BLPNCD        IFGT      0
099700930701     C                   Z-ADD     BLPNCD        VI1NCD
099800930701     C                   Z-ADD     BLPNCA        VI1NCA
099900040220x   2C                   ELSE
100000930701     C*
100100940802     C     KBLP3         SETLL     FNBLT01L
100200940802     C     KBLP3         READE     FNBLT01L                               31
100300040220do  3C  N31              DO
100400940808     C                   Z-ADD     BLTNSC        VI1NCD
100500940802     C     KBLP3         SETGT     FNBLT01L
100600940802     C     KBLP3         READPE    FNBLT01L                               31
100700940808     C  N31BLTNSC        IFNE      VI1NCD
100800940808     C                   MOVEL     BLTNSC        VI1NCA
100900040220     C                   ENDIF
101000040220e   3C                   ENDDO
101100040220e   2C                   ENDIF
101200930701     C*
101300930701     C                   MOVEL     PARAM1        KPJBU
101400940804     C                   CALL      'FNLR04R'
101500921013     C                   PARM                    KPJBA
101600940808     C                   MOVEL     'S'           WRIEM
101700040220e   1C                   ENDIF
101800930505     C*
101900990118     C* F23 - VISUALIZZA DATA CONSEGNA PREVISTA E ORE DI RITARDO
102000040220do  1C   KX              DO
102100930505     C                   SETOFF                                       94
102200930505     C                   SETON                                        31
102300930505     C*
102400930505     C                   MOVEL     ' '           D98TLA
102500930505     C                   MOVEL     'P'           D98TBO
102600930505     C                   MOVEL     BLPAAS        D98AAS
102700930505     C                   MOVEL     BLPLNP        D98LNP
102800930505     C                   MOVEL     BLPNRS        D98NRS
102900930505     C                   MOVEL     BLPNSP        D98NSP
103000930505     C*
103100970925     C                   CALL      'TNSD99R'
103200991022     C                   PARM                    TNSD99DS
103300930505     C*
103400940808     C                   Z-ADD     D98DCE        G02INV
103500940808     C                   Z-ADD     *ZEROS        G02DAT
103600940808     C                   MOVEL     '3'           G02ERR
103700940808     C                   CALL      'XSRDA8'
103800940808     C                   PARM                    WLBDAT
103900940808     C                   Z-ADD     G02DAT        V1CDCE
104000940808     C                   Z-ADD     D98NRC        V1CNRC
104100930526     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
104200930526     C*   CONSEGNA PREVISTA
104300940808     C  N98              Z-ADD     D98HCE        V1CHMC
104400930528     C  N98              Z-ADD     D98HCE        VIDHC2
104500930505     C*
104600940808     C* SE PA098= 1  E' STATA FATTA CALL PGM
104700940808     C                   MOVEL     '1'           PA098
104800040220e   1C                   ENDDO
104900931115     C*
105000990118     C* F20 - VISUALIZZAZIONE AFFIDABILITA'
105100040220do  1C   KU              DO
105200950102     C                   CLEAR                   FNLR97
105300950102     C                   Z-ADD     PA0AAS        R97AAS
105400950102     C                   Z-ADD     PA0LNP        R97LNP
105500950102     C                   Z-ADD     PA0NRS        R97NRS
105600950102     C                   Z-ADD     PA0NSP        R97NSP
105700950102     C                   Z-ADD     V1CDSP        R97DSP
105800950102     C                   Z-ADD     V2CRMN        R97RMN
105900950102     C                   Z-ADD     V1CLNA        R97LNA
106000950102     C                   Z-ADD     V3CDCP        R97DCP
106100950102     C                   MOVEL     V2CRSD        R97RSD
106200950102     C                   MOVEL     V1CRSU        R97RSU
106300950328     C                   MOVEL     V1DLNA        R97DLA
106400950102     C                   MOVEL     PA0CML        R97CML
106500950102     C                   MOVE      'P'           R97AP
106600950102     C   95              MOVE      '1'           R97EVE
106700950102     C                   Z-ADD     V1CDBR        R97DBR
106800950102     C                   Z-ADD     V1CDPC        R97DPC
106900950102     C                   Z-ADD     V1CDUC        R97DUC
107000950328     C                   MOVEL     V3CTMC        R97TMC
107100950102     C                   Z-ADD     V2CCAS        R97CAS
107200950102     C                   MOVEL     V2CVCA        R97VCA
107300931115     C                   MOVE      *BLANKS       KPJBU
107400950102     C                   MOVEL     FNLR97        KPJBU
107500940804     C                   CALL      'FNLR97R'                            31
107600931115     C                   PARM                    KPJBA
107700950102     C                   MOVEL     KPJBU         FNLR97
107800950102     C                   MOVEL     R97F03        WF03
107900931115     C                   SETON                                        31
108000940808     C                   MOVEL     'S'           WRIEM
108100040220e   1C                   ENDDO
108200931115     C*
108300990118     C* F22 - INTERROGAZIONE CONTRASSEGNO, SOLO SE PRESENTE
108400040220if  1C   KWV2CCAS        IFGT      *ZERO
108500991022     C                   CLEAR                   tnsc20ds
108600950125     C                   Z-ADD     1             D20INT
108700950125     C                   Z-ADD     PA0AAS        D20ANN
108800950125     C                   Z-ADD     PA0LNP        D20LNP
108900950125     C                   Z-ADD     PA0NRS        D20NSR
109000950125     C                   Z-ADD     PA0NSP        D20NSP
109100991022     C                   MOVEL     tnsc20ds      KPJBU
109200950125     C                   CALL      'TNSC23R'                            31
109300931115     C                   PARM                    KPJBA
109400931115     C                   SETON                                        31
109500040220     C                   MOVEL     'S'           WRIEM
109600040220e   1C                   ENDIF
109700941021     C*
109800991022     C* ? FORMATO VIDEO:  LS05D08  (TASSAZIONE 1a BOLLA) ?
109900991022     C*  EMETTO SOLO SE VI E' TASSAZIONE
110000040220do  1C   KM              DO
110100941021     C     WRIEM         IFEQ      'S'
110200941021     C                   WRITE     LS05T01
110300941021     C                   MOVEL     *BLANKS       WRIEM
110400941021     C                   ENDIF
110500941021     C                   WRITE     LS05D08
110600941021     C*
110700941021     C                   EXFMT     LS05C08
110800941021     C                   SETON                                        31
110900040220e   1C                   ENDDO
111000991022      *
111100991022      * ? FORMATO VIDEO:  LS05D04  (TASSAZIONE 2a BOLLA) ?
111200040220do  1C   KD              DO
111300991022     C     WRIEM         IFEQ      'S'
111400991022     C                   WRITE     LS05T01
111500991022     C                   MOVEL     *BLANKS       WRIEM
111600991022     C                   ENDIF
111700991022     C                   EXFMT     LS05D04
111800991022     C                   SETON                                        31
111900040220e   1C                   ENDDO
112000991022      **
112100990118     C** F2 - INTERROGAZIONE C.A. SE CE N'E' PIU' DI UNA EMETTO SFL
112200990129     C* Pa0flg="C" -- > Disabilitare F2 PERCHE' RICHIAMATO DA C.A.
112300040220if  1C   KBPA0FLG        IFNE      'C'
112400990129     C                   EXSR      CMD02
112500040220e   1C                   ENDIF
112600931115     C*
112700921013     C                   ENDSR
112800990125     C**************************************************************************
112900990125     C** F2 - INTERROGAZIONE C.A.
113000990125     C**************************************************************************
113100990125     C     CMD02         BEGSR
113200990125     C     CONTCA        IFEQ      1
113300991022     C                   CLEAR                   FIDN00DS
113400990125     C                   Z-ADD     VHCAAC        I00AAC
113500990125     C                   Z-ADD     VHCFIL        I00FIL
113600990125     C                   Z-ADD     VHCNCA        I00NCA
113700990125     C                   Z-ADD     PA0AAS        I00AAS
113800990125     C                   Z-ADD     PA0NRS        I00NRS
113900990125     C                   Z-ADD     PA0NSP        I00NSP
114000990125     C                   Z-ADD     PA0LNP        I00LNP
114100990125     C                   MOVEL     'P'           I00TPB
114200991022     C                   MOVEL     FIDN00DS      KPJBU
114300990125     C                   CALL      'FIDN01R'
114400990125     C                   PARM                    KPJBA
114500000309     C                   PARM                    CALLricors        1
114600990125     C                   SETON                                        31
114700990125     C                   MOVEL     'S'           WRIEM
114800990125     C                   ELSE
114900990125     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
115000990125     C                   SETOFF                                       21
115100990125     C                   WRITE     LS05C11
115200990125     C                   SETON                                        212023
115300990125     C                   CLEAR                   NRR11
115400990125     C**
115500040714     C**""*KBLP3         CHAIN     FNDCT02L                           31
115600040714     C**""**IN31         DOWEQ     *OFF
115700040714     C**""*DCTATB        IFEQ      ' '
115800040714     C**""*              Z-ADD     DCTAAC        V11AAC
115900040714     C**""*              Z-ADD     DCTFIL        V11FIL
116000040714     C**""*              Z-ADD     DCTNCA        V11NCA
116100040714     C**""*              Z-ADD     DCTDCA        G02INV
116200040714     C**""*              Z-ADD     *ZERO         G02DAT
116300040714     C**""*              MOVEL     '3'           G02ERR
116400040714     C**""*              CALL      'XSRDA8'
116500040714     C**""*              PARM                    WLBDAT
116600040714     C**""*              Z-ADD     G02DAT        V11DCA
116700040714     C**""*              Z-ADD     DCTFCA        V11FCA
116800040714     C**""*DCTFCA        CHAIN     FNDFA01L                           31
116900040714     C**N31              MOVEL     DFADES        V11DFA
117000040714     C**""*
117100040714     C**""*              ADD       1             NRR11             4 0
117200040714     C**""*              WRITE     LS05S11
117300040714     C**""*              ENDIF
117400040714     C**""**
117500040714     C**""*KBLP3         READE     FNDCT02L                               31
117600040714     C**""**             ENDDO
117700040714      *
117800040714      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
117900040714      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
118000040714      * legate alla spedizione che passo
118100040714     c                   clear                   fidn12ds
118200040714     c                   eval      i12aas = pa0aas
118300040714     c                   eval      i12lnp = pa0lnp
118400040714     c                   eval      i12nrs = pa0nrs
118500040714     c                   eval      i12nsp = pa0nsp
118600040714     c                   eval      i12fel = 'E'
118700040714     c                   eval      i12fan = 'E'
118800040714      *
118900040714     c                   call      'FIDN12R'
119000040714     c                   parm                    fidn12ds
119100040714      *
119200040714      * se non ci sono errori
119300040714     c                   if        o12err = ' '
119400040714      * se numero di CA trovate maggiore di zero
119500040714     c                   if        o12nca > 0
119600040714     c
119700040714     c                   z-add     *zeros        ii                2 0
119800040714     c     1             do        o12nca        ii
119900040714     c                   movea     skkey(ii)     dskey
120000040714     C                   Z-ADD     dsaac         V11AAC
120100040714     C                   Z-ADD     dsfil         V11FIL
120200040714     C                   Z-ADD     dsnca         V11NCA
120300040714     C                   Z-ADD     skdca(ii)     G02INV
120400040714     C                   Z-ADD     *ZERO         G02DAT
120500040714     C                   MOVEL     '3'           G02ERR
120600040714     C                   CALL      'XSRDA8'
120700040714     C                   PARM                    WLBDAT
120800040714     C                   Z-ADD     G02DAT        V11DCA
120900040714     C                   Z-ADD     skfca(ii)     V11FCA
121000040714     C     V11FCA        CHAIN     FNDFA01L                           31
121100040714     C  N31              MOVEL     DFADES        V11DFA
121200040714     C**
121300040714     C                   ADD       1             NRR11             4 0
121400040714     C                   WRITE     LS05S11
121500040714     C                   ENDDO
121600040714      *
121700040714     c                   endif
121800040714     c                   endif
121900040714      *
122000990125     C* EMETTO FORMATO
122100990125     C     FOR11         TAG
122200990125     C                   Z-ADD     1             NRR11
122300990125     C                   WRITE     LS05T01
122400990125     C                   WRITE     LS05Z11
122500990125     C                   EXFMT     LS05C11
122600990125     C* F12 - RITORNO
122700990125     C     *INKL         IFEQ      *OFF
122800990125     C                   READC     LS05S11                                31
122900990125     C     *IN31         DOWEQ     *OFF
123000990125     C     V11SCE        IFEQ      '5'
123100991022     C                   CLEAR                   FIDN00DS
123200990125     C                   Z-ADD     V11AAC        I00AAC
123300990125     C                   Z-ADD     V11FIL        I00FIL
123400990125     C                   Z-ADD     V11NCA        I00NCA
123500990125     C                   Z-ADD     PA0AAS        I00AAS
123600990125     C                   Z-ADD     PA0NRS        I00NRS
123700990125     C                   Z-ADD     PA0NSP        I00NSP
123800990125     C                   Z-ADD     PA0LNP        I00LNP
123900990125     C                   MOVEL     'P'           I00TPB
124000991022     C                   MOVEL     FIDN00DS      KPJBU
124100990125     C                   CALL      'FIDN01R'
124200990125     C                   PARM                    KPJBA
124300000309     C                   PARM                    CALLricors
124400990125     C**
124500990125     C                   CLEAR                   V11SCE
124600990125     C                   UPDATE    LS05S11
124700990125     C                   ENDIF
124800990125     C**
124900990125     C                   READC     LS05S11                                31
125000990125     C                   ENDDO
125100990125     C                   GOTO      FOR11
125200990125     C                   ENDIF
125300990125     C                   SETON                                        31
125400990125     C                   MOVEL     'S'           WRIEM
125500990125     C*
125600990125     C                   ENDIF
125700990125     C                   ENDSR
125800990125     C**************************************************************************
125900990125     C*    CONTROLLO SE C'E' UNA O PIU' C.A.
126000990125     C**************************************************************************
126100990125     C     CTRDAN        BEGSR
126200990125     C                   CLEAR                   CONTCA
126300040714     C**""*KBLP3         CHAIN     FNDCT02L                           31
126400040714     C**""**IN31         DOWEQ     *OFF
126500040714     C**""*DCTATB        IFEQ      ' '
126600040714     C**""**             SETON                                        12
126700040714     C**""**             ADD       1             CONTCA            3 0
126800040714     C**""**             Z-ADD     DCTAAC        VHCAAC
126900040714     C**""**             Z-ADD     DCTFIL        VHCFIL
127000040714     C**""**             Z-ADD     DCTNCA        VHCNCA
127100040714     C**""**             ENDIF
127200990125     C*
127300040714     C**""*KBLP3         READE     FNDCT02L                               31
127400040714     C**""**             ENDDO
127500040714      *
127600040714      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON
127700040714      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
127800040714      * late e legate alla spedizione che passo
127900040714     c                   clear                   fidn12ds
128000040714     c                   eval      i12aas = pa0aas
128100040714     c                   eval      i12lnp = pa0lnp
128200040714     c                   eval      i12nrs = pa0nrs
128300040714     c                   eval      i12nsp = pa0nsp
128400040714     c                   eval      i12fel = 'E'
128500040714     c                   eval      i12fan = 'E'
128600040714      *
128700040714     c                   call      'FIDN12R'
128800040714     c                   parm                    fidn12ds
128900040714      *
129000040714      * se non ci sono errori
129100040714     c                   if        o12err = ' '
129200040714      * se numero di CA trovate maggiore di zero
129300040714     c                   if        o12nca > 0
129400040714     c                   z-add     O12NCA        CONTCA            3 0
129500040714     C                   SETON                                        12
129600040714     C                   MOVEA     SKKEY(O12NCA) DSKEY
129700040714     C                   Z-ADD     DSAAC         VHCAAC
129800040714     C                   Z-ADD     DSFIL         VHCFIL
129900040714     C                   Z-ADD     DSNCA         VHCNCA
130000040714     C                   ENDIF
130100040714     C                   ENDIF
130200040714      *
130300990125     C                   ENDSR
130400940808     C**************************************************************************
130500971212     C*    CARICO RECORD DA VISUALIZZARE/MANUTENZIONARE
130600940808     C**************************************************************************
130700900511     C     CARBLP        BEGSR
130800940808     C*
130900930112     C*** PULIZIA CAMPI
131000930112     C*
131100930112     C                   MOVEL     ' '           VI2CEI
131200950628     C                   MOVEL     'P'           PA2TBO
131300940804     C                   Z-ADD     0             V3CDCP
131400930526     C*
131500920326     C                   SETOFF                                       440307
131600951011     C                   SETOFF                                       949813
131700021218     C                   SETOFF                                       0977
131800940923     C*
131900940923     C                   MOVEL     PA0AAS        V1CAAS
132000940926     C                   MOVEL     PA0LNP        V1CLNP
132100940923     C                   MOVEL     PA0NRS        V1CNRS
132200940923     C                   MOVE      PA0NSP        V1CNSP
132300940923     C*
132400900511     C* CHAIN
132500041028     c                   clear                   wesarb            1
132600940802     C     KBLP3         CHAIN     FNBLP01L                           31
132700041028     C     KBLP3         CHAIN     FNarb01L
132800041028     c                   if        %found(fnarb01l)
132900041028     c                   eval      wesarb='S'
133000041028     c                   else
133100041028     c                   eval      wesarb='N'
133200041028     c                   endif
133300940926     C*
133400940928     C* SE NON TROVATA BOLLA, PROVO NELLO STORICO
133500940928     C* E SE NON TROVATA PULISCO I CAMPI CHIAVE E ESCO
133600010713     C     *IN31         IFEQ      *ON
133700010716     C                   MOVEL     *ZERO         PA0AAS
133800010716     C                   MOVEL     *ZERO         PA0LNP
133900010716     C                   MOVEL     *ZERO         PA0NRS
134000010716     C                   MOVEL     *ZERO         PA0NSP
134100010716     C                   GOTO      ENDBLP
134200010713     C                   ENDIF
134300021218     C*
134400040220     C* 77 ON - Esistono note supermercati da interrogare
134500021218     C                   Z-ADD     PA0AAS        KAAS
134600021218     C                   Z-ADD     PA0LNP        KLNP
134700021218     C                   Z-ADD     PA0NRS        KNRS
134800021218     C                   Z-ADD     PA0NSP        KNSP
134900021218     C                   MOVEL     'SCR'         KTRD
135000021218     C     KAR51         CHAIN     FIAR5000                           31
135100021218     C                   If        *in31 = *off and Ar5Atb = ' '
135200021218     C                   seton                                        77
135300021218     C                   else
135400021218     C                   setoff                                       77
135500021218     C                   endif
135600940926     C*
135700900927     C* CONTROLLO SE SPEDIZIONE GIACENTE
135800050406     C     KBLP3         CHAIN     tIGCP01L                           31
135900940923     C*
136000901004     C  N31GCPDCG        IFEQ      0
136100910321     C                   SETON                                        4403
136200910321     C                   ELSE
136300910321     C                   SETON                                        03
136400901004     C                   END
136500111020     C**!03PA0CLI        COMP      'S'                                0303
136600900511     C*
136700950214     C                   CLEAR                   V2CFLS
136800950214     C     BLPNCD        IFNE      *ZERO
136900950214     C     BLPFNS        ORNE      *BLANKS
137000940923     C                   MOVEL     BLPFLS        V2CFLS
137100950214     C                   ENDIF
137200940923     C                   MOVEL     PA0NRS        V7CNRS
137300940808     C                   MOVEL     BLPLNP        V1CLP1
137400940805     C                   MOVEL     BLPCBO        V1CCBO
137500940809     C*
137600940809     C                   MOVEL     BLPGGA        V2CGGA
137700940923     C                   MOVEL     *BLANKS       V2IGGA
137800940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
137900940923     C     V2CGGA        COMP      *BLANKS                            1717
138000940809     C*
138100940809     C                   MOVEL     BLPGMA        V2CGMA
138200940923     C                   MOVEL     *BLANKS       V2IGMA
138300940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
138400940923     C     V2CGMA        COMP      *BLANKS                            1818
138500940809     C*
138600960208     C                   MOVEL     BLPGVA        V2CGVA
138700960208     C                   MOVE      ' '           V2CGVA
138800940923     C                   MOVEL     *BLANKS       V2IGVA
138900940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
139000940923     C     V2CGVA        COMP      *BLANKS                            1919
139100940322      *
139200911105     C* NON SEQUENZIALI
139300910912     C     BLPFNS        IFEQ      'S'
139400940808     C                   MOVEL     SEQ(1)        V2CFNS
139500910912     C                   ELSE
139600940808     C                   MOVEL     *BLANKS       V2CFNS
139700910912     C                   END
139800930112     C* CONTROLLO SE ESISTONO VARIAZIONI PER LA BOLLA
139900131007     C     KBLP3         CHAIN     FIARBF2C                           31
140000111020     C**!!!PA0CLI        IFNE      'S'
140100920429     C  N31              SETON                                        10
140200111020     C**!!!              ENDIF
140300910806     C*
140400920409     C* TIPO BOLLA CONTROLLO SE FRANCO
140500940804     C                   MOVEL     '3A'          KCOD
140600940804     C                   MOVEL     *BLANKS       KKEY
140700940805     C                   MOVEL     BLPCBO        KKEY
140800940809     C     KTAB1         CHAIN     TABEL00F                           31
140900940923     C  N31              MOVEL     TBLUNI        DS3A
141000940923     C   31              CLEAR                   DS3A
141100900516     C                   MOVEL     §3ATB1        FLGTB1            1
141200940804     C                   MOVEL     §3ADES        V1DCBO
141300940805     C                   MOVEL     §3ADES        V2DCBO
141400940804     C                   MOVEL     §3ADES        V7DCBO
141500940408     C     FLGTB1        IFEQ      'F'
141600910418     C                   SETON                                        33
141700900511     C                   END
141800900511     C* LINEA ARRIVO E DECODIFICA
141900940804     C                   MOVEL     BLPLNA        V1CLNA
142000020225     C     BLPLNA        CHAIN     AZORG01L
142100020225     c                   if        not %found(azorg01l)
142200020225     C                   MOVEL     *BLANKS       ORGDES
142300020225     c                   endif
142400940804     C                   MOVEL     ORGDES        V1DLNA
142500940804     C                   MOVEL     ORGDES        V2DLNA
142600940810     C                   MOVEL     ORGDES        V9DLNA
142700020225     c                   eval      og143  = ORGde3
142800020225     c                   eval      lnantw = §ogntw
142900971202     C* TERMINAL DI PARTENZA DELLA LINEA DI ARRIVO
143000991022     C                   CLEAR                   FNLV55DS
143100971202     C                   MOVEL     'P'           D55TPT
143200971202     C                   MOVE      BLPLNA        D55LIN
143300971202     C                   Z-ADD     WDATEU        D55DRF
143400971202     C                   CALL      'FNLV55R'
143500991022     C                   PARM                    FNLV55DS
143600971202     C                   MOVE      D55TFP        WTFP
143700940804     C* LINEA PARTENZA E DECODIFICA
143800020225     C     BLPLNP        CHAIN     AZORG01L
143900020225     c                   if        not %found(azorg01l)
144000020225     C                   MOVEL     *BLANKS       ORGDES
144100020225     c                   endif
144200940804     C                   MOVEL     ORGDES        V1DLNP
144300020225     c                   eval      og143  = ORGde3
144400020225     c                   eval      lnpntw = §ogntw
144500920410     C* MITTENTE
144600940804     C                   MOVEL     BLPRSM        V1CRSM
144700940808     C                   MOVEL     BLPRSM        V2CRSM
144800940805     C                   MOVEL     BLPRSM        V7CRSM
144900940804     C                   MOVEL     BLPINM        V1CINM
145000940808     C                   MOVEL     BLPINM        V7CINM
145100940804     C                   MOVEL     BLPPRM        V1CPRM
145200940804     C                   MOVEL     BLPCAM        V1CCAM
145300940804     C                   MOVEL     BLPLOM        V1CLOM
145400940808     C                   MOVEL     BLPLOM        V2CLOM
145500940808     C                   MOVEL     BLPLOM        V7CLOM
145600940808     C                   MOVEL     BLPPRM        V2CPRM
145700940804     C                   MOVEL     BLPNZM        V1CNZM
145800940808     C                   MOVEL     BLPFAP        V1CFAP
145900981023     C                   MOVEL     BLPCTR        V2CCTR
146000060216     C* NUMERO ORM: DA ar4 (Tipo record 'M')
146100950412     C                   MOVEL     BLPAAS        KAAS
146200950412     C                   MOVEL     BLPLNP        KLNP
146300950412     C                   MOVEL     BLPNRS        KNRS
146400950412     C                   MOVEL     BLPNSP        KNSP
146500010220     C                   MOVEL     'M'           KTRC
146600010220     C                   clear                   v1cpoe
146700010220     C                   clear                   v1cnsr
146800010220     C                   clear                   v1cnor
146900010220     C                   clear                   v1cnrv
147000010220     C                   setoff                                       55
147100060216     C     Kar41         CHAIN     Fiar401L                           99
147200010220     C     *in99         ifeq      *off
147300060216     C     ar4not        ifne      *blanks
147400060216     C                   movel     ar4not        dsbl4m
147500010220     C                   move      §b4poe        v1cpoe
147600010220     C                   move      §b4nsr        v1cnsr
147700010220     C                   move      §b4nor        v1cnor
147800010220     C                   move      §b4nrv        v1cnrv
147900040713     c     korm          Setll     Fnorm01l
148000040713     c                   If        %Equal(fnorm01l)
148100010220     C                   seton                                        55
148200040713     c                   EndIf
148300010220     C                   endif
148400010220     C                   endif
148500010220     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
148600060216     C* DA Fiar4 SE RECORD ESISTENTE
148700950412     C                   MOVEL     'P'           KTRC
148800060216     C     Kar41         CHAIN     Fiar401L                           99
148900950412     C     *IN99         IFEQ      *OFF
149000060216     C                   MOVEL     ar4NOT        V1CPIM
149100060216     C                   MOVE      ar4NOT        V2CPID
149200950412     C                   ELSE
149300950412     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI BLPCPI
149400930112     C     BLPCPI        IFNE      *BLANKS
149500940804     C   33              MOVEL     BLPCPI        V1CPIM
149600940804     C  N33              MOVEL     BLPCPI        V2CPID
149700930112     C                   ELSE
149800940808     C                   MOVEL     *BLANKS       V1CPIM
149900940808     C                   MOVEL     *BLANKS       V2CPID
150000930112     C                   END
150100950412     C                   END
150200061108      * codice fiscale mittente e destinatario da fiar4 rcd Q
150300061108     c                   clear                   v1cdfm
150400061108     c                   clear                   v2cdfd
150500061108     c                   eval      ktrc = 'Q'
150600061108     c     kar41         chain     fiar401l
150700061108     c                   if        %found(fiar401l)
150800061108     c                   movel     ar4not        v1cdfm
150900061108     c                   move      ar4not        v2cdfd
151000061108     c                   endif
151100900525     C*
151200940906     C* CERCO LA DESCRIZIONE AGGIUNTIVA PER IL DESTINATARIO
151300940808     C                   MOVEL     BLPAAS        KAAS
151400940808     C                   MOVEL     BLPLNP        KLNP
151500940808     C                   MOVEL     BLPNRS        KNRS
151600940808     C                   MOVEL     BLPNSP        KNSP
151700940906     C                   MOVEL     'D'           KTRC
151800060216     C     Kar41         CHAIN     Fiar401L                           99
151900060216     C  N99              MOVEL     ar4NOT        V2CNOT
152000950125     C   99              MOVEL     *BLANKS       V2CNOT
152100940808     C*
152200971212     C* SOLO PER LE FILIALI ESTERE IMPORT/EXPORT VISUALIZZO RIF PARTNER
152300971212     C                   CLEAR                   V1CEST
152400951011     C                   MOVEL     'E'           KTRC
152500060216     C     Kar41         CHAIN     Fiar401L                           99
152600060216     C  N99              MOVEL     ar4NOT        V2CRPP
152700980704     C  N99              SETON                                        13
152800020225     c   99              if        (lnantw = 'EEX' or lnantw = 'EUP' or
152900020225     c                              lnantw = 'FED') or
153000020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
153100020225     c                              lnpntw = 'FED')
153200980704     C                   SETON                                        13
153300980704     C                   ENDIF
153400951011     C   99              CLEAR                   V2CRPP
153500971212     C* SE E' SDI IMPORTO ANCHE TIPO INSERIMENTO E STAMPA
153600020225     c                   if        not *in99 and (lnpntw = 'EUP' or
153700020225     c                                            lnantw = 'EUP')
153800971212     C**
153900060216     C                   MOVEL     ar4NOT        DSBL4E
154000971212    3C     §B4EMA        IFEQ      'M'
154100971212     C                   MOVEL     CMANUA        W021A            21
154200971212     C                   SETON                                        09
154300971212    4C     §B4EAT        IFEQ      'P'
154400971212     C                   MOVE      'PART'        W021A
154500971212    4C                   ENDIF
154600971212    4C     §B4EAT        IFEQ      'A'
154700971212     C                   MOVE      'ARR.'        W021A
154800971212    4C                   ENDIF
154900971212     C                   MOVE      W021A         V1CEST
155000971212    3C                   ENDIF
155100971212    C**
155200971212    3C     §B4EMA        IFEQ      'A'
155300971212     C                   MOVEL     CAUTOM        W018A            18
155400971212     C                   SETON                                        09
155500971212    4C     §B4EAT        IFEQ      'P'
155600971212     C                   MOVE      'PART'        W018A
155700971212    4C                   ENDIF
155800971212    4C     §B4EAT        IFEQ      'A'
155900971212     C                   MOVE      'ARR.'        W018A
156000971212    4C                   ENDIF
156100971212     C*
156200971212     C                   MOVEL     W018A         V1CEST
156300971212     C                   MOVE      CSTAMP        V1CEST
156400971212     C**
156500971212    4C     §B4EST        IFEQ      'N'
156600971212     C                   MOVE      'NO'          V1CEST
156700971212    4C                   ELSE
156800971212     C                   MOVE      'SI'          V1CEST
156900971212    4C                   ENDIF
157000991022      *
157100971212    3C                   ENDIF
157200971212    2C                   ENDIF
157300991227     c* DPD
157400000216     c                   setoff                                       14
157500000216     c                   clear                   v2crtn
157600000216     c                   clear                   v2czon
157700020225     c                   if        lnantw = 'DPD'
157800991227     c                   seton                                        13
157900060517     C***                MOVEL     'F'           KTRC
158000060517     C                   MOVEL     'I'           KTRC
158100060216     C     Kar41         CHAIN     Fiar401L                           99
158200060517     C**N99              MOVEL     ar4NOT        dsbl4f
158300060517     c** 99              clear                   dsbl4f
158400060517     C  N99              MOVEL     ar4NOT        dsbl4I
158500060517     c   99              clear                   dsbl4I
158600060517     C***                MOVEL     §b4fnp        V2CRPP
158700060517     C                   MOVEL     §b4ipn        V2CRPP
158800060517     c****               if        §b4rtn>*zeros or §b4zon>*zeros
158900060517     c****               movel     §b4rtn        v2crtn
159000060517     c****               movel     §b4zon        v2czon
159100060517     c                   if        §b4ida>*zeros or §b4iza>*zeros
159200060517     c                   movel     §b4ida        v2crtn
159300060517     c                   movel     §b4iza        v2czon
159400000216     c                   seton                                        14
159500000216     c                   setoff                                       09
159600000216     c                   endif
159700050304     c                   else
159800050304     c* dpd import
159900050304     c                   if        lnpntw = 'DPD'
160000060517     c****               clear                   dsbl4f
160100060517     C****               MOVEL     'F'           KTRC
160200060517     c                   clear                   dsbl4i
160300060517     C                   MOVEL     'I'           KTRC
160400060216     C     Kar41         CHAIN     Fiar401L
160500060216     c                   if        %found(fiar401l)
160600050304     c                   seton                                        13
160700060517     C***                MOVEL     ar4NOT        dsbl4f
160800060517     C                   MOVEL     ar4NOT        dsbl4I
160900060517     C***                MOVEL     §b4fnp        V2CRPP
161000060517     C                   MOVEL     §b4ipn        V2CRPP
161100050304     c                   endif
161200050304     c                   endif
161300991227     c                   endif
161400981023     C***
161500981023     C* ASSEGNATO
161600981023     C     *IN33         IFEQ      *OFF
161700981023     C** MITTENTE
161800981023     C     BLPCCM        IFNE      0
161900981023     C                   MOVEL     BLPCCM        V2CCDM
162000981023     C                   MOVEL     BLPCCM        V1CKSC
162100981023     C                   ELSE
162200981023     C                   CLEAR                   V2CCDM
162300981023     C                   CLEAR                   V1CKSC
162400981023     C                   ENDIF
162500981023     C* DESTINATARIO
162600981023     C                   MOVEL     BLPKSC        V2CCDD
162700981023     C                   MOVEL     BLPKSC        V1CCDD
162800991116     c                   eval      KTRC = '1'
162900991116     C     KAR6          CHAIN     FIAR6000                           99
163000991116     C                   IF        *in99 = *OFF and ar6ksc > *zeros
163100991116     C                   MOVEL     ar6ksc        V2CCDD
163200991116     C                   MOVEL     ar6ksc        V1CCDD
163300991116     C                   MOVEL     ar6ctr        V2CCTR
163400991116     c                   ENDIF
163500981023     C                   ELSE
163600981023     C* FRANCO
163700981023     C                   MOVEL     BLPKSC        V1CKSC
163800981023     C                   MOVEL     BLPKSC        V2CCDM
163900981023     C                   CLEAR                   V2CCDD
164000981023     C                   CLEAR                   V1CCDD
164100981023     C                   ENDIF
164200050117     C                   MOVEL     BLPXCO        V2CXCO
164300940808     C*
164400940808     C* SE C'E' IL COD. MITTENTE, RILEVO IL NR.TELEFONICO
164500940809     C                   MOVE      V2CCDM        WQUATT            4 0
164600940809     C     WQUATT        IFNE      8888
164700940809     C     V2CCDM        ANDNE     *ZERO
164800981123     C                   MOVEL     KCI           KKCC
164900940808     C                   MOVEL     V2CCDM        KKSC
165000940808     C     KIND1         CHAIN     CNIND00F                           99
165100940808     C  N99              MOVEL     INDTEL        V2CTEL
165200940808     C   99              MOVEL     *BLANKS       V2CTEL
165300940809     C                   ENDIF
165400940808     C*
165500940808     C                   MOVEL     BLPRSD        V2CRSD
165600940808     C                   MOVEL     BLPRSD        V7CRSD
165700940810     C                   MOVEL     BLPRSD        V9CRSD
165800940808     C                   MOVEL     BLPIND        V2CIND
165900940808     C                   MOVEL     BLPIND        V7CIND
166000940808     C                   MOVEL     BLPPRD        V2CPRD
166100940808     C                   MOVEL     BLPCAD        V2CCAD
166200940808     C                   MOVEL     BLPLOD        V2CLOD
166300940808     C                   MOVEL     BLPLOD        V7CLOD
166400940808     C                   MOVEL     BLPNZD        V2CNZD
166500940804     C                   MOVEL     BLPFIN        V2CFIN
166600940804     C                   MOVEL     BLPGC1        V2CGC1
166700940804     C                   MOVEL     BLPGC2        V2CGC2
166800961111     C                   MOVEL     *BLANKS       V2CFBR
166900961111     C                   MOVEL     *BLANKS       V2CFFR
167000961111     C                   SETOFF                                       0102
167100940804     C     BLPFBR        IFEQ      'S'
167200940804     C                   MOVEL     'SI'          V2CFBR
167300961111     C                   SETON                                        01
167400940804     C                   ENDIF
167500961111     C     BLPFBR        IFEQ      'F'
167600961111     C                   MOVEL     'FR'          V2CFFR
167700961111     C                   SETON                                        02
167800961111     C                   ENDIF
167900970227     C     BLPFST        IFEQ      'S'
168000970131     C     BLPFST        OREQ      'C'
168100970227     C                   MOVEL     'SI DDT'      V1DBAM
168200970227     C                   SETON                                        08
168300970130     C                   ELSE
168400970227     C                   MOVEL     'NO DDT'      V1DBAM
168500970227     C                   SETOFF                                       08
168600970130     C                   ENDIF
168700970227     C*
168800970227     C                   SELECT
168900970227     C     BLPFST        WHENEQ    'C'
169000970227     C     BLPFST        OREQ      'K'
169100970227     C                   MOVEL     'CLIENTE '    V1DFST
169200970227     C     BLPFST        WHENEQ    'S'
169300970227     C     BLPFST        OREQ      'Y'
169400970227     C                   MOVEL     'PARTENZA'    V1DFST
169500970227     C     BLPFST        WHENEQ    'J'
169600970227     C                   MOVEL     'ARRIVO  '    V1DFST
169700970227     C                   OTHER
169800970227     C                   MOVEL     'NO      '    V1DFST
169900970227     C                   ENDSL
170000970227     C*
170100941219     C     BLPFDN        IFEQ      'S'
170200941219     C                   MOVEL     'SI'          V1CFDN
170300941219     C                   ELSE
170400941219     C                   MOVEL     *BLANKS       V1CFDN
170500941219     C                   ENDIF
170600940808     C                   MOVEL     BLPFTM        V1CFTM
170700940804     C                   MOVEL     *BLANKS       V1DTSP
170800940804     C                   MOVEL     '5E'          KCOD
170900940804     C                   MOVEL     *BLANKS       KKEY
171000940808     C                   MOVEL     BLPTSP        KKEY
171100940809     C     KTAB1         CHAIN     TABEL00F                           99
171200090608     C  N99              MOVEL     TBLUNI        ds5e
171300090608     C  N99              MOVEL     §5ed08        V1DTSP
171400941021     C*
171500940805     C                   MOVEL     BLPCTS        V8CCTS
171600940804     C                   MOVEL     BLPQFT        V2CQFT
171700940804     C                   MOVEL     BLPVMD        V2CVMD
171800940804     C                   MOVEL     BLPVAD        V2CVAD
171900940804     C                   MOVEL     BLPIAS        V2CIAS
172000940804     C                   MOVEL     BLPVAS        V2CVAS
172100940808     C                   MOVEL     BLPNAS        V1CNAS
172200940804     C                   MOVEL     BLPNCL        V1CNCL
172300941026     C                   MOVEL     BLPPKF        V1CPKF
172400990909     C                   MOVEL     BLPPKB        V1CPKB
172500940808     C                   MOVEL     BLPVLB        V1CVLB
172600940808     C                   MOVEL     BLPFVB        V1CFVB
172700940808     C                   MOVEL     BLPVLF        V1CVLF
172800940808     C                   MOVEL     BLPFVF        V1CFVF
172900940808     C* SE BLPNCR = 99999  A VIDEO AZZERO NCR E VLC
173000930505     C     BLPNCR        IFEQ      99999
173100940805     C                   MOVEL     *ZEROS        BLPNCR
173200940805     C                   MOVEL     *ZEROS        BLPVLC
173300940808     C                   ENDIF
173400940808     C                   MOVEL     BLPVLC        V1CVLC
173500940808     C                   MOVEL     BLPNCR        V1CNCR
173600990909     C* SE BLPNCP = 99999  A VIDEO AZZERO NCP E PKC
173700990909     C     BLPNCP        IFEQ      99999
173800990909     C                   MOVEL     *ZEROS        BLPNCP
173900990909     C                   MOVEL     *ZEROS        BLPPKC
174000990909     C                   ENDIF
174100990909     C                   MOVEL     BLPPKC        V1CPKC
174200990909     C                   MOVEL     BLPNCP        V1CNCP
174300031119
174400031119      * Referente e Telefono del destinatario
174500031119     c                   Clear                   V2cRef
174600031119     c                   Clear                   V2cTeld
174700110610     c* profilo utente e terminale che ha immesso la bolla
174800110610     c                   Clear                   Vidpru
174900110610     c                   Clear                   Vidnmtd
175000110610     c                   setoff                                       46
175100031119      * li prendo da Fiar500f record 'GEN'
175200031119     c                   Clear                   dAr5Gen
175300091125     c                   clear                   w_cdi
175400031119     c                   Z-Add     BlpAas        Kaas
175500031119     c                   Z-Add     BlpLNp        Klnp
175600031119     c                   Z-Add     BlpNrs        Knrs
175700031119     c                   Z-Add     BlpNsp        Knsp
175800031119     c                   Eval      Ktrd = 'GEN'
175900031119     c     kAr51         Chain     Fiar501l
176000031119If  1c                   If        %Found(Fiar501l)
176100031119     c                   Movel     Ar5Uni        Dar5Gen
176200031119     c                   Eval      V2cRef = §Ar5Ref
176300031119     c                   Eval      V2cTeld = §Ar5Teld
176400091125     c* memorizzo eventuale codice beneficiario assegno
176500091125     c                   eval      w_cdi=§ar5cdi
176600110610      *
176700110610     c                   Eval      Vidpru = §Ar5bpru
176800110610     c                   Eval      Vidnmtd = §Ar5bnmtd
176900110610     c                   if        vidpru<>*blanks
177000110610     c                   seton                                        46
177100110610     c                   endif
177200031119    1c                   EndIf
177300130319     c* Vedo se ha il packing list
177400130320     c                   if        §ar5alS='S'
177500130320     c                   eval      v1dar8='PACKING LIST PDF'
177600130319     C                   ELSE
177700130320     c                   if        §ar5alx='S'
177800130320     c                   eval      v1dar8='PACKING LIST    '
177900130319     c                   else
178000130319     C                   CLEAR                   V1DAR8
178100130319     c                   endif
178200130319     C                   ENDIF
178300110531
178400110531     c* email destinatario
178500110601     c                   setoff                                       45
178600110531     c                   clear                   v2cemW
178700130927     c                   clear                   dar5emd
178800110531      * lo prendo da Fiar500f record 'EMD'
178900110531     c                   Z-Add     BlpAas        Kaas
179000110531     c                   Z-Add     BlpLNp        Klnp
179100110531     c                   Z-Add     BlpNrs        Knrs
179200110531     c                   Z-Add     BlpNsp        Knsp
179300110531     c                   Eval      Ktrd = 'EMD'
179400130927     c*    kAr51         setll     Fiar501l
179500130927     c     kAr51         chain     Fiar501l
179600130927If  1c                   If        %found(Fiar501l)
179700130927     c                   eval      dar5emd=ar5uni
179800130927     c                   if        §ar5tel<>*blanks or §ar5eml<>*blanks
179900110610     c                   eval      v2cemd='SI'
180000110601     c                   seton                                        45
180100130927     c                   endif
180200110531     c                   endif
180300040301
180400040301      * Se colli di valore visualizzo "V" nell'apposito campo
180500040301      *  con lo specifico attributo di visualizzazione
180600040301if  1c                   if        §AR5bva = *blanks
180700040301     c                   eval      V2Cbva  = *blanks
180800040301x   1c                   else
180900040301     c                   eval      V2Cbva  = 'V'
181000040301e   1c                   endif
181100031119
181200030121      * Bancali
181300030121     c                   Clear                   V1dTba
181400030121     c                   Clear                   V1dTb2
181500030122     c                   Clear                   V1cNba
181600030122     c                   Clear                   V1cNb2
181700030703      * Record 'BAN'
181800030703If  0c                   If        %subst(BlpGva:1:1) = 'B'
181900030121     c                   Clear                   dAr5Ban
182000030121     c                   Eval      Ktrd = 'BAN'
182100030121     c     kAr51         Chain     Fiar501l
182200030121If  1c                   If        %Found(Fiar501l)
182300030121     c                   Movel     Ar5Uni        Dar5Ban
182400030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
182500030122      * per le note relative al mancato reso dei bancali
182600030122     c                   If        Not *In77 and
182700030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
182800030122     c                   Eval      *In77 = *On
182900030122     c                   EndIf
183000030121     c                   Eval      V1cNba = §Ar5Nba
183100030121     c                   Eval      V1cNb2 = §Ar5Nb2
183200030121If  2c                   If        §Ar5Tba <> *Zeros
183300030703     c                   Eval      wtba = §Ar5Tba
183400030703     c                   ExSr      Sr_DecTba
183500030703     c                   Eval      V1dTba = wdestba
183600030121    2c                   EndIf
183700030121If  2c                   If        §Ar5Tb2 <> *Zeros
183800030703     c                   Eval      wtba = §Ar5Tb2
183900030703     c                   ExSr      Sr_DecTba
184000030703     c                   Eval      V1dTb2 = wdestba
184100030121    2c                   EndIf
184200030121    1c                   EndIf
184300030703    0c                   EndIf
184400030703      * Record 'BNB'  -  Spedizione OVER 25 colli
184500030703If  0c                   If        %subst(BlpGva:1:1) = 'O'
184600030703     c                   Clear                   dAr5Bnb
184700030703     c                   Eval      Ktrd = 'BNB'
184800030703     c     kAr51         Chain     Fiar501l
184900030703If  1c                   If        %Found(Fiar501l)
185000030703     c                   Movel     Ar5Uni        Dar5Bnb
185100030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
185200030703      * per i colli originali in caso di spedizione over 25 colli
185300030703     c  n77              Eval      *In77 = *On
185400030703     c                   Eval      V1cNba = §Ar5bNba
185500030703     c                   Eval      V1cNb2 = §Ar5bNb2
185600030703If  2c                   If        §Ar5bTba <> *Zeros
185700030703     c                   Eval      wtba = §Ar5bTba
185800030703     c                   ExSr      Sr_DecTba
185900030703     c                   Eval      V1dTba = wdestba
186000030703    2c                   EndIf
186100030703If  2c                   If        §Ar5bTb2 <> *Zeros
186200030703     c                   Eval      wtba = §Ar5bTb2
186300030703     c                   ExSr      Sr_DecTba
186400030703     c                   Eval      V1dTb2 = wdestba
186500030703    2c                   EndIf
186600030703    1c                   EndIf
186700030703    0c                   EndIf
186800920409     C*
186900920409     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
187000940804     C                   MOVEL     'TR'          KCOD
187100940804     C                   MOVEL     *BLANKS       KKEY
187200940808     C                   MOVEL     V2CCTR        UNOCTR            1
187300940804     C                   MOVEL     UNOCTR        KKEY
187400940809     C     KTAB1         CHAIN     TABEL00F                           31
187500940804     C                   MOVEL     TBLUNI        V2DCTR
187600910730     C*
187700940808     C* VISUALIZZO V2CCDM SOLO SE LA LNP DEL CODICE = LNP DELLA BOLLA
187800930505     C                   MOVEL     BLPCCM        NUM3              3 0
187900930505     C     BLPLNP        IFEQ      NUM3
188000940808     C                   MOVE      BLPCCM        V2CCDM
188100930505     C                   END
188200930505     C*
188300940804     C                   MOVEL     BLPRMN        V2CRMN
188400940804     C                   MOVEL     BLPRMA        V2CRMA
188500940804     C                   MOVEL     BLPRMO        V2CRMO
188600940804     C                   MOVEL     BLPNCD        V2CNCD
188700940804     C                   MOVEL     BLPNCA        V2CNCA
188800940804     C                   MOVEL     BLPZNC        V2CZNC
188900940804     C                   MOVEL     BLPCTM        V1CCTM
189000130319     C***                MOVEL     '1B'          KCOD
189100130319     C***                MOVEL(P)  blpctm        KKEY
189200130319     C***  KTAB1         CHAIN     TABEL00F                           31
189300130319     C**N31              MOVEL     TBLUNI        DS1B
189400130319     c** 31              clear                   ds1b
189500051128     c* Vedo se ha il packing list
189600130319     c***                if        §1bf16='S'
189700130319     c***                eval      v1dar8='PACKING LIST'
189800130319     C***                ELSE
189900130319     C***                CLEAR                   V1DAR8
190000130319     C***                ENDIF
190100051104     c
190200051104     c                   select
190300051104     c                   when      blpffd='S'
190400051104     C                   MOVEL     'SI '         V2CFFD
190500051104     c                   when      blpffd='M'
190600051104     C                   MOVEL     'SAB'         V2CFFD
190700051104     C                   other
190800940905     C                   MOVEL     *BLANKS       V2CFFD
190900051104     C                   ENDsl
191000051104     c
191100940808     C                   MOVEL     BLPNFV        V1CNFV
191200940906     C                   MOVEL     BLPNRT        V1CNRT
191300940906     C                   MOVEL     *BLANKS       V1DFPP
191400940906     C     BLPFPP        IFEQ      'M'
191500010220     C                   MOVEL     'MAT'         V1DFPP
191600940906     C                   ELSE
191700010220     C                   MOVEL     'POM'         V1DFPP
191800940906     C                   ENDIF
191900910118     C* GIRO DATA SPEDIZIONE
192000940804     C                   MOVEL     BLPAAS        V1CDSP
192100940804     C                   MOVE      BLPMGS        V1CDSP
192200940804     C                   Z-ADD     V1CDSP        G02INV
192300940804     C                   Z-ADD     *ZEROS        G02DAT
192400940804     C                   MOVEL     '3'           G02ERR
192500940804     C                   CALL      'XSRDA8'
192600940804     C                   PARM                    WLBDAT
192700940804     C                   Z-ADD     G02DAT        V1CDSP
192800940808     C* DATA RITIRO MERCE
192900940804     C                   Z-ADD     BLPDRT        G02INV
193000940804     C                   Z-ADD     *ZEROS        G02DAT
193100940804     C                   MOVEL     '3'           G02ERR
193200940804     C                   CALL      'XSRDA8'
193300940804     C                   PARM                    WLBDAT
193400940804     C                   Z-ADD     G02DAT        V1CDRT
193500900518     C* DATA BORDERO'
193600940804     C                   Z-ADD     BLPDBR        G02INV
193700940804     C                   Z-ADD     *ZEROS        G02DAT
193800940804     C                   MOVEL     '3'           G02ERR
193900940804     C                   CALL      'XSRDA8'
194000940804     C                   PARM                    WLBDAT
194100940804     C                   Z-ADD     G02DAT        V1CDBR
194200940804     C* DATA PARTENZA PRIMO COLLO
194300940804     C                   Z-ADD     BLPDPC        G02INV
194400940804     C                   Z-ADD     *ZEROS        G02DAT
194500940804     C                   MOVEL     '3'           G02ERR
194600940804     C                   CALL      'XSRDA8'
194700940804     C                   PARM                    WLBDAT
194800940804     C                   Z-ADD     G02DAT        V1CDPC
194900940804     C* DATA PARTENZA ULTIMO COLLO
195000940804     C                   Z-ADD     BLPDUC        G02INV
195100940804     C                   Z-ADD     *ZEROS        G02DAT
195200940804     C                   MOVEL     '3'           G02ERR
195300940804     C                   CALL      'XSRDA8'
195400940804     C                   PARM                    WLBDAT
195500940804     C                   Z-ADD     G02DAT        V1CDUC
195600910806     C* DATA SPUNTA ENTRATA
195700940808     C     BLPDSE        IFGT      *ZERO
195800940808     C                   Z-ADD     BLPDSE        G02INV
195900940808     C                   Z-ADD     *ZEROS        G02DAT
196000940808     C                   MOVEL     '3'           G02ERR
196100940808     C                   CALL      'XSRDA8'
196200940808     C                   PARM                    WLBDAT
196300940808     C                   Z-ADD     G02DAT        V1CDSE
196400940808     C                   ENDIF
196500940805     C* CONSEGNA PARTICOLARE 1
196600940805     C                   MOVEL     BLPTC1        V3CTC1
196700940805     C                   MOVEL     *BLANKS       V3DTC1
196800940805     C     V3CTC1        IFNE      *BLANKS
196900940804     C                   MOVEL     '1P'          KCOD
197000940804     C                   MOVEL     *BLANKS       KKEY
197100940805     C                   MOVEL     BLPTC1        KKEY
197200940809     C     KTAB1         CHAIN     TABEL00F                           99
197300940805     C  N99              MOVEL     TBLUNI        V3DTC1
197400940805     C                   ENDIF
197500940805     C* CONSEGNA PARTICOLARE 2
197600940805     C                   MOVE      BLPTC2        V3CTC2
197700940805     C                   MOVEL     *BLANKS       V3DTC2
197800940805     C     V3CTC2        IFNE      *BLANKS
197900940805     C                   MOVEL     '1P'          KCOD
198000940805     C                   MOVEL     *BLANKS       KKEY
198100940805     C                   MOVEL     BLPTC2        KKEY
198200940809     C     KTAB1         CHAIN     TABEL00F                           99
198300940805     C  N99              MOVEL     TBLUNI        V3DTC2
198400940805     C                   ENDIF
198500910118     C* CODICE PADRONCINO
198600940808     C                   MOVEL     BLPPDR        V1CPDR
198700021203     C                   movel     BlpPdr        kpdpdr
198800021203     C                   move      'A'           kpdtip
198900021203     C     kfapd         chain     fiapd01L                           31
199000940808     C  N31              MOVEL     APDRSC        V1DPDR
199100940808     C   31              MOVEL     *BLANKS       V1DPDR
199200910118     C*
199300900511     C                   MOVEL     BLPSOP        VIDSOP
199400930614     C*
199500051124     c* Decodifico consegna richiesta
199600940808     C                   MOVEL     *BLANKS       V2CTCR
199700051124     c                   clear                   v2chcr
199800051124     c                   clear                   v2cdcr
199900051124     c                   if        blpdcr>0 or blphcr>0
200000051124     c                   clear                   fnlv80ds
200100051124     c                   eval      ilv80aas=blpaas
200200051124     c                   eval      ilv80lnp=blplnp
200300051124     c                   eval      ilv80nrs=blpnrs
200400051124     c                   eval      ilv80nsp=blpnsp
200500051124     c                   eval      ilv80mgs=blpmgs
200600051124     c                   eval      ilv80tc1=blptc1
200700051124     c                   eval      ilv80tc2=blptc2
200800051124     c                   eval      ilv80dcr=blpdcr
200900051124     c                   eval      ilv80hcr=blphcr
201000051124     c                   eval      ilv80tcr=blptcr
201100051124     c                   call      'FNLV80R'
201200051124     c                   parm                    kpjba
201300051124     c                   parm                    fnlv80ds
201400051124     c
201500051124     c                   eval      v2cdcr=olv80dcr8
201600051124     c                   eval      v2ctcr=olv80tcrd
201700051124     C                   MOVEL     blpHCR        V2cHCR
201800051124     c                   endif
201900940906     C*-------------------------------------------------------------------------
202000940906     C*
202100940906     C* L E G A M E   B O L L A
202200940906     C                   EXSR      LEGBOL
202300940808     C*-------------------------------------------------------------------------
202400050714     c* Dei seguenti dati,
202500050714     c* Prevalgono quelli di ARB se c'e' e se non presenti legami bolla
202600050714     C* DATA ENTRATA AL TRANSITO
202700050714     c                   if        wesarb='S' and not *in07
202800050714     c                   eval      wdet=arbdet
202900050714     c                   eval      wdut=arbdut
203000050714     c                   else
203100050714     c                   eval      wdet=blpdet
203200050714     c                   eval      wdut=blpdut
203300050714     c                   endif
203400050714     c
203500050714     C     wdet          IFGT      *ZERO
203600050714     C                   Z-ADD     wdet          G02INV
203700050714     C                   Z-ADD     *ZEROS        G02DAT
203800050714     C                   MOVEL     '3'           G02ERR
203900050714     C                   CALL      'XSRDA8'
204000050714     C                   PARM                    WLBDAT
204100050714     C                   Z-ADD     G02DAT        V1CDET
204200050714     C                   ENDIF
204300050714     C* DATA USCITA DAL TRANSITO
204400050714     C     wdut          IFGT      *ZERO
204500050714     C                   Z-ADD     wdut          G02INV
204600050714     C                   Z-ADD     *ZEROS        G02DAT
204700050714     C                   MOVEL     '3'           G02ERR
204800050714     C                   CALL      'XSRDA8'
204900050714     C                   PARM                    WLBDAT
205000050714     C                   Z-ADD     G02DAT        V1CDUT
205100050714     C                   ENDIF
205200050714     c* Mancata consegna
205300050714     c                   if        wesarb='S' and not *in07
205301140729     c                             and savnr9=0
205400050714     c                   eval      wtmc=arbtmc
205500050714     c                   else
205600050714     c                   eval      wtmc=blptmc
205700050714     c                   endif
205800050714     c
205900050714     C                   MOVEL     wtmc          V3CTMC
206000050714     C                   MOVEL     *BLANKS       V3DTMC
206100050714     C     wtmc          IFNE      ' '
206200050714     C                   MOVEL     '2A'          KCOD
206300050714     C                   MOVEL     *BLANKS       KKEY
206400050714     C                   MOVEL     wtmc          KKEY
206500050714     C     KTAB1         CHAIN     TABEL00F                           99
206600050714     C  N99              MOVEL     TBLUNI        V3DTMC
206700050714     C                   ENDIF
206800050714     c* Numero e data distinta consegna
206900050714     c                   if        wesarb='S'  and not *in07
206901140729     c                             and savnr9=0
207000050714     c                   eval      wndc=arbndc
207100050714     c                   eval      wddc=arbddc
207200050714     c                   else
207300050714     c                   eval      wndc=blpndc
207400050714     c                   eval      wddc=blpddc
207500050714     c                   endif
207600050714     C*
207700050714     C                   MOVEL     wndc          V3CNDC
207800050714     C                   Z-ADD     wddc          G02INV
207900050714     C                   Z-ADD     *ZEROS        G02DAT
208000050714     C                   MOVEL     '3'           G02ERR
208100050714     C                   CALL      'XSRDA8'
208200050714     C                   PARM                    WLBDAT
208300050714     C                   Z-ADD     G02DAT        V3CDDC
208400050714     C* DATA ARRIVO MERCE ALL FILIALE DI ARRIVO
208500050714     c                   clear                   wdam
208600050714     c                   if        wesarb='S' and not *in07
208601140729     c                             and savnr9=0
208700050714     c                   eval      wdam=arbdam
208800050714     c                   else
208900050714     c                   eval      wdam=blpdam
209000050714     c                   endif
209100050714     c
209200050714     C     wdam          IFGT      *ZERO
209300050714     C                   Z-ADD     wdam          G02INV
209400050714     C                   Z-ADD     *ZEROS        G02DAT
209500050714     C                   MOVEL     '3'           G02ERR
209600050714     C                   CALL      'XSRDA8'
209700050714     C                   PARM                    WLBDAT
209800050714     C                   Z-ADD     G02DAT        V1CDAM
209900050714     C                   ENDIF
210000050714     c
210100050714     C* DATA CONSEGNA MERCE REALE
210200050714     C                   Z-ADD     *ZERO         V1CDCM
210300050714     c                   if        wesarb='S' and not *in07
210301140729     c                             and savnr9=0
210400050714     c                   eval      wdcm=arbdcm
210500050714     c                   eval      wdce=arbdce
210600050714     c                   eval      wnrc=arbnrc
210700050714     c                   eval      wdcp=arbdcp
210800050714     c                   eval      wcca=arbcca
210900050714     c                   eval      whmc=arbhmc
211000050714     c                   else
211100050714     c                   eval      wdcm=blpdcm
211200050714     c                   eval      wdce=blpdce
211300050714     c                   eval      wnrc=blpnrc
211400050714     c                   eval      wdcp=blpdcp
211500050714     c                   eval      wcca=blpcca
211600050714     c                   eval      whmc=blphmc
211700050714     c                   endif
211800050714     c
211900050714     C     wdcm          IFGT      *ZERO
212000050714     C                   SETON                                        98
212100050714     C                   Z-ADD     wdcm          G02INV
212200050714     C                   Z-ADD     *ZEROS        G02DAT
212300050714     C                   MOVEL     '3'           G02ERR
212400050714     C                   CALL      'XSRDA8'
212500050714     C                   PARM                    WLBDAT
212600050714     C                   Z-ADD     G02DAT        V1CDCM
212700050714     C                   MOVEL     whmc          V1CHMC
212800050714     C                   ENDIF
212900050714     C* DATA CONSEGNA PREVISTA ESTERNA E ORE DI RITARDO
213000050714     C                   Z-ADD     0             V1CDCE
213100050714     C                   Z-ADD     0             V1CNRC
213200050714     C     wdce          IFEQ      0
213300050714     C                   SETON                                        94
213400050714     C                   ELSE
213500050714     C                   Z-ADD     wdce          G02INV
213600050714     C                   Z-ADD     *ZEROS        G02DAT
213700050714     C                   MOVEL     '3'           G02ERR
213800050714     C                   CALL      'XSRDA8'
213900050714     C                   PARM                    WLBDAT
214000050714     C                   Z-ADD     G02DAT        V1CDCE
214100050714     C*
214200050714     C                   Z-ADD     wnrc          V1CNRC
214300050714     C                   END
214400050714     C*
214500050714     C* DATA PRIMA CONSEGNA PARZIALE
214600050714     C     wdcp          IFGT      0
214700050714     C                   Z-ADD     wdcp          G02INV
214800050714     C                   Z-ADD     *ZEROS        G02DAT
214900050714     C                   MOVEL     '3'           G02ERR
215000050714     C                   CALL      'XSRDA8'
215100050714     C                   PARM                    WLBDAT
215200050714     C                   Z-ADD     G02DAT        V3CDCP
215300050714     C                   END
215400050714     C* CODICE CONSEGNA ANOMALA
215500050714     C                   MOVEL     wcca          V3CCCA
215600050714     C                   MOVE      *BLANKS       V3DCCA
215700050714     C                   MOVE      *BLANKS       V7DCCA
215800050714     C     wcca          IFNE      ' '
215900050714     C                   MOVEL     '7O'          KCOD
216000050714     C                   MOVEL     *BLANKS       KKEY
216100050714     C                   MOVEL     wcca          KKEY
216200050714     C     KTAB1         CHAIN     TABEL00F                           99
216300050714     C  N99              MOVEL     TBLUNI        V3DCCA
216400050714     C  N99              MOVEL     TBLUNI        V7DCCA
216500050714     C                   END
216600050714     c*******
216700991022      *   SE ESISTE AGGANCIO  2 °   B O L L A
216800991022      *
216900920409     C     §3ATB2        IFNE      '  '
217000991022      *
217100991022     c                   eval      KTRC = '2'
217200991022     C     KAR6          CHAIN     FIAR6000
217300991022      *
217400991022     C                   IF        %FOUND
217500991022     C                   eval        *IN06 = *ON
217600991022     C                   eval      VI2fiv = AR6fiv
217700991022     C                   eval      VI2nft = AR6nft
217800991022     C                   eval      VI2cei = AR6cei
217900991022      *
218000991022     C                   eval      VI2DIV = AR6div
218100991022     C                   eval      WDIV = AR6div
218200991022     C                   exsr      CTR_VALUTA
218300991022      *
218400991027     c                   if        AR6fim = 'P'
218500991027     c                   eval      VI2fim = 'PARTENZA'
218600040116     c                   eval      VI2fi2 = 'Immesse in'
218700991022     c                   endif
218800991022      * Varia G
218900040116     c**!!!              clear                   W0133
219000040116     c**!!!              SELECT
219100040116     c**!!!              WHEN        AR6sv1 = §$2vrg
219200040116     C**!!!              eval      VI2sv1 = AR6sv1
219300040116     c**!!!              z-add     AR6va1        W0133
219400040116     c**!!!              WHEN        AR6sv2 = §$2vrg
219500040116     C**!!!              eval      VI2sv1 = AR6sv2
219600040116     c**!!!              z-add     AR6va2        W0133
219700040116     c**!!!              WHEN        AR6sv3 = §$2vrg
219800040116     C**!!!              eval      VI2sv1 = AR6sv3
219900040116     c**!!!              z-add     AR6va3        W0133
220000040116     c**!!!              ENDSL
220100991022      *
220200040116     c**!!!              if        W0133 > *zeros
220300040116     c**!!!              eval      Wtstdec = §cvfdc
220400040116     c**!!!              exsr      EDTimporti
220500040116     c**!!!              movel     IMPeditato    VI2va1
220600040116     c**!!!              endif
220700991022      * IVA
220800040116     c**!!!              clear                   W0133
220900040116     c**!!!              SELECT
221000040116     c**!!!              WHEN        AR6sv1 = §$2iva
221100040116     c**!!!              z-add     AR6va1        W0133
221200040116     c**!!!              WHEN        AR6sv2 = §$2iva
221300040116     c**!!!              z-add     AR6va2        W0133
221400040116     c**!!!              WHEN        AR6sv3 = §$2iva
221500040116     c**!!!              z-add     AR6va3        W0133
221600040116     c**!!!              ENDSL
221700991022      *
221800040116     c**!!!              IF        W0133 > *zeros
221900040116     c**!!!              eval      *in11 = *off
222000040116     c**!!!              eval      Wtstdec = §cvfdc
222100040116     c**!!!              exsr      EDTimporti
222200040116     c**!!!              movel     IMPeditato    VI2iva
222300040116     c**!!!              ELSE
222400991123      * BOLLO se non ho iva
222500040116     c**!!!              SELECT
222600040116     c**!!!              WHEN        AR6sv1 = §$2bol
222700040116     c**!!!              z-add     AR6va1        W0133
222800040116     c**!!!              WHEN        AR6sv2 = §$2bol
222900040116     c**!!!              z-add     AR6va2        W0133
223000040116     c**!!!              WHEN        AR6sv3 = §$2bol
223100040116     c**!!!              z-add     AR6va3        W0133
223200040116     c**!!!              ENDSL
223300991123      *
223400040116     c**!!!              IF        W0133 > *zeros
223500040116     c**!!!              eval      *in11 = *on
223600040116     c**!!!              eval      Wtstdec = §cvfdc
223700040116     c**!!!              exsr      EDTimporti
223800040116     c**!!!              movel     IMPeditato    VI2iva
223900040116     c**!!!              ENDIF
224000991123      *
224100040116     c**!!!              ENDIF
224200040116
224300040116      * Carico le varie
224400040116     c                   Movel     Ar6Sv1        wsvn
224500040116     c                   Z-add     Ar6Va1        wvan
224600040116     c                   ExSr      Car_Varie1
224700040116
224800040116     c                   Movel     Ar6Sv2        wsvn
224900040116     c                   Z-add     Ar6Va2        wvan
225000040116     c                   ExSr      Car_Varie1
225100040116
225200040116     c                   Movel     Ar6Sv3        wsvn
225300040116     c                   Z-add     Ar6Va3        wvan
225400040116     c                   ExSr      Car_Varie1
225500040116
225600040116     c     Kar6          Setll     Fiar7000
225700040116     c                   Do        *Hival
225800040116     c     Kar6          Reade     Fiar7000
225900040116     c                   If        %Eof(Fiar701l)
226000040116     c                   Leave
226100040116     c                   EndIf
226200040116     c                   Movel     Ar7Svn        wsvn
226300040116     c                   Z-add     Ar7Van        wvan
226400040116     c                   ExSr      Car_Varie1
226500040116     c                   EndDo
226600040116
226700991025      * TOTALE FATTURA
226800991025     c                   clear                   W0133
226900991025     c                   IF          AR6ift > *zeros
227000991025     c                   z-add     AR6ift        W0133
227100991025     c                   eval      Wtstdec = §cvfdc
227200991025     c                   exsr      EDTimporti
227300991025     c                   movel     IMPeditato    VI2ift
227400991025     c                   endif
227500991022      *
227600991022     c                   If         AR6dft > *zeros
227700991022     C     *iso          movel     AR6DFT        DATA_EUR
227800991022     C     *eur          movel     data_eur      W0080
227900991022     C                   eval      VI2dft = %EDITC(W0080:'Y')
228000991022     c                   Endif
228100991022      *  decodifica esenzione IVA
228200991022     C                   if        VI2CEI <> *blanks
228300940808     C                   MOVEL     'EI'          KCOD
228400991022     C                   MOVEL(P)  VI2CEI        KKEY
228500940809     C     KTAB1         CHAIN     TABEL00F                           31
228600991022     C  N31              MOVEL     TBLUNI        VI2DCE
228700940808     C                   ENDIF
228800991022      *
228900991022     c                   eval      VI2ksc = AR6ksc
229000991022     c                   eval      VI2dks = BLPrsd
229100991022     c                   eval      VI2ctr = AR6ctr
229200991022      *
229300991022     C                   eval      KCOD = 'TR'
229400991022     C                   movel     VI2CTR        UNOCTR
229500991022     C                   eval      KKEY = unoctr
229600991022     C     KTAB1         CHAIN     TABEL00F                           31
229700991022     C                   movel     TBLUNI        VI2dct
229800991022      *
229900991022     C                   ENDIF
230000940808     C                   ENDIF
230100951219     C*-------------------------------------------------------------------------
230200951219     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
230300951219     C*
230400951219     C* PULISCO IL SFL
230500951219     C                   SETON                                        26
230600951219     C                   WRITE     LS05C03
230700951219     C                   SETOFF                                       26
230800951219     C*
230900060216     C* CHIAVE PER ACCEDERE AL FILE Fiar401L
231000951219     C                   MOVEL     BLPAAS        KAAS
231100951219     C                   MOVEL     BLPLNP        KLNP
231200951219     C                   MOVEL     BLPNRS        KNRS
231300951219     C                   MOVEL     BLPNSP        KNSP
231400951219     C*
231500951219     C                   Z-ADD     0             NRR3              4 0
231600951219     C                   MOVEL     '7K'          KCOD
231700951219     C*
231800060216     C     Kar42         SETLL     Fiar401L
231900060216     C     Kar42         READE     Fiar401L                               99
232000951219     C     *IN99         DOWEQ     *OFF
232100951219     C*
232200060216     C                   MOVEL(P)  ar4TRC        KKEY
232300951219     C     KTAB1         CHAIN     TABEL                              99
232400951219     C*
232500951219     C     *IN99         IFEQ      *OFF
232600951219     C     TBLFLG        ANDEQ     ' '
232700951219     C                   MOVEL     TBLUNI        DS7K
232800951219     C     §7KVIS        IFEQ      'S'
232900951219     C                   MOVEL     §7KDES        VS3DES
233000060216     C                   MOVEL     ar4NOT        VS3NOT
233100951219     C                   ADD       1             NRR3
233200951219     C                   WRITE     LS05S03
233300951219     C                   ENDIF
233400951219     C                   ENDIF
233500951219     C*
233600060216     C     Kar42         READE     Fiar401L                               99
233700951219     C                   ENDDO
233800951219     C*
233900951219     C* SE NON HO CARICATO NIENTE SCRIVO UGUALMENTE UN RECORD VUOTO
234000951219     C*  NEL SFL PER EVITARE CHE IL PGM DIA ERRORE DI SESSIONE O UNITA'
234100951219     C     NRR3          IFEQ      0
234200951219     C                   ADD       1             NRR3
234300951219     C                   WRITE     LS05S03
234400951219     C                   ENDIF
234500951219     C*
234600940808     C*-------------------------------------------------------------------------
234700930526     C* 95 ON - ESITONO EVENTI DA INTERROGARE
234800111020     C**!!!PA0CLI        IFNE      'S'
234900991022     C     KBLP3         SETLL     FNEVB000                               95
235000111020     C**!!!              ENDIF
235100940805     C*-------------------------------------------------------------------------
235200110705     C                   clear                   SAVNR8
235300110705     C                   eval      *in04 = *off
235400110705     C                   eval      *in84 = *off
235500110705     c
235600110705     c                   clear                   v8car4ksc
235700110705     c                   clear                   v8dar4ksc
235800110705     c                   clear                   v8car4ctr
235900110705     C* Verifico se presete cod cliente e cod tariffa di tassazione da bollettaz
236000110705     C                   MOVEL     'A'           KTRC
236100110705     C     Kar41         CHAIN     Fiar401L                           99
236200110705     C     *IN99         IFEQ      *OFF
236300110705     c                   movel     ar4not        dsbl4a
236400110705     c                   if        §b4aks>0
236500110705     C                   eval      *in84 = *on
236600110705     C                   eval      *in04 = *on
236700110705     c                   eval      v8car4ksc=§b4aks
236800110705     c                   eval      i69kac = v8car4ksc
236900110705     c                   exsr      CTR_ANAGRA
237000110705     c                   eval      v8dar4ksc = acorag
237100110705     c                   if        §b4act<>*blanks
237200110705     c                   movel     §b4act        v8car4ctr
237300110705     c                   endif
237400110705     c                   endif
237500110705     c                   endif
237600110705     c
237700991022      * PER I DATI SULLA TASSAZIONE, CERCO SU FIAR600F.
237800991022      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SOLO SE IMPOSTATO ALMENO UN CAMPO
237900991022     C                   eval      Ktrc = '1'
238000991022     C     KAR6          CHAIN     FIAR601L
238100110705      * CLIENTE E TARIFFA FATTURAZIONE
238200110705     c                   IF        %found(fiar601l)
238300110705     c
238400110705     c                   IF        *IN33 = *ON
238500110705     c                   eval      v8cksc = blpksc
238600110705     c                   eval      v8cctr = blpctr
238700110705     c                   eval      v8dksc = blprsm
238800110705     c                   ELSE
238900110705     c                   eval      v8cksc = AR6ksc
239000110705     c                   eval      v8cctr = AR6ctr
239100110705     c                   move      v8cksc        W0040
239200110705     c                   if        W0040 = 9999
239300110705     c                   eval      v8dksc = blprsd
239400110705     c                   else
239500110705     c                   eval      i69kac = v8cksc
239600110705     c                   exsr      CTR_ANAGRA
239700110705     c                   eval      v8dksc = acorag
239800110705     c                   endif
239900110705     c                   ENDIF
240000110705     c
240100110705      *   decodifica tariffa
240200110705     C                   eval      KCOD = 'TR'
240300110705     C                   MOVEL     V2CCTR        UNOCTR            1
240400110705     C                   MOVEL(P)  UNOCTR        KKEY
240500110705     C     KTAB1         CHAIN     TABEL00F                           31
240600110705     C                   MOVEL     TBLUNI        V8DCTR
240700110705     c
240800110705     c                   ENDIF
240900991022      *
241000991022     C                   IF        %FOUND  and
241100991022     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
241200991022     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
241300991022     C                              AR6sv3 <> *blanks)
241400991022      *
241500991022     C                   eval      *in04 = *on
241600991022      *
241700991025     C                   eval      V8CDIV = AR6div
241800991022     C                   eval      WDIV = AR6div
241900991022     C                   exsr      CTR_VALUTA
242000991022      *
242100991022     c                   IF          ar6por > *zeros
242200991022     c                   z-add     AR6por        W0133
242300991022     c                   eval      Wtstdec = §cvfdc
242400991022     c                   exsr      EDTimporti
242500991022     c                   movel     IMPeditato    V8Cpor
242600991022     c                   ENDIF
242700991022      *
242800991022     c                   IF          ar6imv > *zeros
242900991022     c                   z-add     AR6imv        W0133
243000991022     c                   eval      Wtstdec = §cvfdc
243100991022     c                   exsr      EDTimporti
243200991022     c                   movel     IMPeditato    V8Cimv
243300991022     c                   ENDIF
243400991022      *
243500991022     c                   IF          ar6ift > *zeros
243600991022     c                   z-add     AR6ift        W0133
243700991022     c                   eval      Wtstdec = §cvfdc
243800991022     c                   exsr      EDTimporti
243900991022     c                   movel     IMPeditato    V8Cift
244000991022     c                   ENDIF
244100991022      *
244200991027     c                   IF        AR6fim = 'P'
244300991022     c                   eval      V8Dfim = 'PARTENZA'
244400991022    2C                   ENDIF
244500991022      *
244600991022     C                   MOVEL     AR6CEI        V8CCEI
244700991022      *
244800991022      * SE C'E', DECODIFICO L'ESENZIONE IVA
244900991022     C                   clear                   V8DCEI
245000991022     C                   IF        AR6CEI <>  *BLANKS
245100991022     C                   MOVEL     'EI'          KCOD
245200991022     C                   MOVEL(P)  AR6CEI        KKEY
245300991022     C     KTAB1         CHAIN     TABEL00F                           99
245400991022     C  N99              MOVEL     TBLUNI        V8DCEI
245500991022    2C                   ENDIF
245600991022      *
245700991022      * DATA FATTURA DA GIRARE
245800991022     C                   IF         ar6dft > *zeros
245900991022     C     *iso          movel     AR6DFT        DATA_EUR
246000991022     C     *eur          movel     data_eur      W0080
246100991022     C                   eval      V8Cdft = %EDITC(W0080:'Y')
246200991022     C                   ENDIF
246300991022     C                   MOVEL     ar6NFT        V8CNFT
246400991022     C                   MOVEL     ar6FIV        V8CFIV
246500991022      *
246600991022      * CERCO LE VARIE
246700991022     C                   eval      *in20 = *off
246800991022     C                   eval      *in21 = *off
246900991022     C                   WRITE     LS05C08
247000991022     C                   eval      *in20 = *on
247100991022     C                   eval      *in21 = *on
247200991022      *
247300991022     c                   clear                   nrr8
247400991022      *
247500991022     C                   MOVEL     ar6SV1        WSVN
247600991022     C                   MOVEL     ar6VA1        WVAN
247700991022     c                   exsr      car_varie
247800991022      *
247900991022     C                   MOVEL     ar6SV2        WSVN
248000991022     C                   MOVEL     ar6VA2        WVAN
248100991022     c                   exsr      car_varie
248200991022      *
248300991022     C                   MOVEL     ar6SV3        WSVN
248400991022     C                   MOVEL     ar6VA3        WVAN
248500991022     c                   exsr      car_varie
248600991022      *
248700991022      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
248800991022     C                   if          ar6SV3 <> *blanks
248900991022     C     KAR6          SETLL     FIAR7000
249000991022     C     KAR6          READE     FIAR7000
249100991022     C                   DOW       %EOF = *OFF
249200991022     C                   MOVEL     AR7SVN        WSVN
249300991022     C                   MOVEL     AR7VAN        WVAN
249400991022     c                   exsr      car_varie
249500991022     C     KAR6          READE     FIAR7000
249600991022     C                   ENDDO
249700991022     C                   endif
249800110705     C                   ENDIF
249900991022      *
250000991022      * Se non ho caricato record ne carico uno vuoto
250100110705     c                   IF        NRR8 = *ZEROS  and *in04
250200991022     C                   clear                   V8CDES
250300991022     C                   clear                   V8CVA1
250400991022     C                   clear                   V8CSV1
250500991022     C                   add       1             NRR8
250600991022     C                   WRITE     LS05S08
250700991022     c                   ENDIF
250800991022      *
250900991022     C                   Z-ADD     NRR8          SAVNR8
251000991022     C                   Z-ADD     1             NRR8
251100991022      *
251200991022      *-------------------------------------------------------------------------
251300991022      * DECODIFICO IL CODICE TASSAZIONE, SE C'E'
251400991022     C                   clear                   V8DCTS
251500991022     C                   if         V8CCTS <> *blanks
251600991022     C                   MOVEL     'CT'          KCOD
251700991022     C                   MOVEL(P)  V8CCTS        KKEY
251800991022     C     KTAB1         CHAIN     TABEL00F                           99
251900991022     C  N99              MOVEL     TBLUNI        V8DCTS
252000991022     C                   endif
252100940805     C*-------------------------------------------------------------------------
252200940805     C* PRELEVO LE NOTE DAL FILE APPOSITO
252300940805     C                   MOVEL     BLPAAS        KAAS
252400940805     C                   MOVEL     BLPLNP        KLNP
252500940805     C                   MOVEL     BLPNRS        KNRS
252600940805     C                   MOVEL     BLPNSP        KNSP
252700940805     C                   MOVEL     '8'           KTRC
252800060216     C     Kar41         CHAIN     Fiar401L                           99
252900060216     C  N99              MOVEL     ar4NOT        V2CNO1
253000940805     C   99              MOVEL     *BLANKS       V2CNO1
253100940805     C                   MOVEL     '9'           KTRC
253200060216     C     Kar41         CHAIN     Fiar401L                           99
253300060216     C  N99              MOVEL     ar4NOT        V2CNO2
253400940805     C   99              MOVEL     *BLANKS       V2CNO2
253500991022      *
253600991022      *-------------------------------------------------------------------------
253700051115      * IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU Fiar900F
253800991022      * CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
253900940805     C                   MOVEL     *BLANKS       V2CTIC
254000940805     C                   MOVEL     *ZERO         V2CCAS
254100940805     C                   MOVEL     *BLANKS       V2CVCA
254200940805     C                   MOVEL     *BLANKS       V2CGCA
254300091214     c                   setoff                                       43
254400940923     C                   MOVEL     *BLANKS       V2IGCA
254500940805     C     §3AFCA        IFGT      *ZERO
254600051115     C     KBLP3         CHAIN     Fiar901L                           99
254700940805     C  N99              DO
254800051115     C                   MOVEL     ar9TIC        V2CTIC
254900051115     C                   MOVEL     ar9CAS        V2CCAS
255000051115     C                   MOVEL     ar9VCA        V2CVCA
255100051115     C                   MOVEL     ar9GCA        V2CGCA
255200091214     c                   seton                                        43
255300940805     C                   ENDDO
255400940805     C                   ENDIF
255500991022      * SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
255600940923     C     V2CGCA        COMP      *BLANKS                            1616
255700021218      *
255800021218      * Riempio il campone in fondo alla videata con i tasti scelti
255900021218     C                   exsr      Ges_TastiFun
256000021218     c                   eval      FD07 = FDxx
256100021218     c                   eval      F24D07 = F24Dxx
256200991022      *
256300940923     C     ENDBLP        ENDSR
256400030703      *-------------------------------------------------------------------------
256500030703      *  Decoditifa tipo bancali
256600030703      *-------------------------------------------------------------------------
256700030703     c     Sr_DecTba     BegSr
256800030703     c                   Clear                   Tibs02Ds
256900030703     c                   Clear                   dTba
257000030703     c                   Movel     'C'           T02Mod
257100030703     c                   Movel     Knsif         T02Sif
257200030703     c                   Movel     'TBA'         T02Cod
257300030703     c                   Movel(p)  wtba          T02Ke1
257400030703     c                   Call      'TIBS02R'
257500030703     c                   Parm                    Kpjba
257600030703     c                   Parm                    Tibs02Ds
257700030703If  3c                   If        T02Err = *Blanks
257800030703     c                   Movel     T02Uni        dTba
257900030703    3c                   EndIf
258000030703     c                   Eval      wdestba = §TbaDesc
258100030703     c                   EndSr
258200021218      *-------------------------------------------------------------------------
258300021218      *  Imposto descrizione tasti funzione
258400021218      *-------------------------------------------------------------------------
258500021218     c     Ges_TastiFun  BEGSR
258600021218      *
258700021218      * per emettere i tasti senza condizionamenti ..
258800021218     c                   setoff                                       25
258900021218      *
259000021218      * Conta i caratteri riempiti dalle RigaTf1
259100021218     c                   z-add     0             $z                3 0
259200021218      *
259300021218      * Conta i caratteri riempiti dalle RigaTf2
259400021218     c                   z-add     0             $k                3 0
259500021218      *
259600021218      * Conta i caratteri riempiti dalle RigaTf3
259700021218     c                   z-add     0             $j                3 0
259800021218      *
259900021218      * Conta la posizione del campone da cui partire per impostare
260000021218      * la descrizione del tasto funzione
260100021218     c                   z-add     0             $y                2 0
260200021218      *
260300021218      * Stringhe che contengono le descrizioni dei tasti funzione
260400021218     c                   reset                   RigaTf1
260500021218     c                   reset                   RigaTf2
260600021218     c                   reset                   RigaTf3
260700021218      *
260800021218      * F13
260900021218     c                   IF        *in04 = *on
261000021218     c                   reset                   $Tf
261100021218     c                   movea     Tf13          $Tf
261200021218     c                   Exsr      Rie_$Tf
261300021218     C                   ENDIF
261400021218      *
261500021218      * F04
261600021218     c                   IF        *in06 = *on
261700021218     c                   reset                   $Tf
261800021218     c                   movea     Tf04          $Tf
261900021218     c                   Exsr      Rie_$Tf
262000021218     C                   ENDIF
262100040220      *
262200040220      *?F16?
262300040220     c                   IF        *in28 = *on
262400040220     c                   reset                   $Tf
262500040220     c                   movea     Tf16          $Tf
262600040220     c                   exsr      Rie_$Tf
262700040220     c                   ENDIF
262800021218      *
262900021218      * F05
263000021218     c                   IF        *in95 = *on
263100021218     c                   reset                   $Tf
263200021218     c                   movea     Tf05          $Tf
263300021218     c                   Exsr      Rie_$Tf
263400021218     C                   ENDIF
263500021218      *
263600021218      * F10
263700021218     c                   IF        *in10 = *on
263800021218     c                   reset                   $Tf
263900021218     c                   movea     Tf10          $Tf
264000021218     c                   Exsr      Rie_$Tf
264100021218     c                   ENDIF
264200021218      *
264300021218      * F14
264400021218     c                   IF        *in03 = *on
264500021218     c                   reset                   $Tf
264600021218     c                   movea     Tf14          $Tf
264700021218     c                   Exsr      Rie_$Tf
264800021218     c                   ENDIF
264900051201      *
265000051201      * F22
265100051201     c                   IF        *in24 = *on
265200051201     c                   reset                   $Tf
265300051201     c                   movea     Tf22          $Tf
265400051201     c                   Exsr      Rie_$Tf
265500051201     c                   ENDIF
265600021218      *
265700021218      * F02
265800021218     c                   IF        *in12 = *on
265900021218     c                   reset                   $Tf
266000021218     c                   movea     Tf02          $Tf
266100021218     c                   Exsr      Rie_$Tf
266200021218     c                   ENDIF
266300021218      *
266400021218      * F11
266500021218     c                   IF        *in77 = *on
266600021218     c                   reset                   $Tf
266700021218     c                   movea     Tf11          $Tf
266800021218     c                   Exsr      Rie_$Tf
266900021218     c                   ENDIF
267000021218      *
267100021218      * F07
267200021218     c                   IF        *in22 = *on and *in05 = *off
267300021218     c                   reset                   $Tf
267400021218     c                   movea     Tf07          $Tf
267500021218     c                   Exsr      Rie_$Tf
267600021218     c                   ENDIF
267700021218      *
267800021218      * F08
267900021218     c                   IF        *in55 = *on
268000021218     c                   reset                   $Tf
268100021218     c                   movea     Tf08          $Tf
268200021218     c                   Exsr      Rie_$Tf
268300021218     c                   ENDIF
268400130912
268500130912      * F09 --> Orari servizi non condizionato
268600131007     c                   IF        *in25 = *off and wabif9='S'
268700130930     c                   reset                   $Tf
268800130930     c                   movea     Tf09          $Tf
268900130930     c                   Exsr      Rie_$Tf
269000130930     c                   ENDIF
269100021218      *
269200021218      * F20
269300021218     c                   IF        *in22 = *off
269400021218     c                   reset                   $Tf
269500021218     c                   movea     Tf20          $Tf
269600021218     c                   Exsr      Rie_$Tf
269700021218     c                   ENDIF
269800021218      *
269900021218      * F21 --> non condizionato
270000021218     c                   IF        *in25 = *off
270100021218     c                   reset                   $Tf
270200021218     c                   movea     Tf21          $Tf
270300021218     c                   Exsr      Rie_$Tf
270400021218     c                   ENDIF
270500040220      *
270600040220      * F19
270700040220     c                   IF        *in22 = *off
270800040220     c                   reset                   $Tf
270900040220     c                   movea     Tf19          $Tf
271000040220     c                   Exsr      Rie_$Tf
271100040220     c                   ENDIF
271200040220      *
271300040220      *?F01?
271400040220     c                   IF        *in27 = *on
271500040220     c                   reset                   $Tf
271600040220     c                   movea     Tf01          $Tf
271700040220     c                   exsr      Rie_$Tf
271800040220     c                   ENDIF
271900021218      *
272000021218      * F12 --> non condizionato
272100021218     c                   IF        *in25 = *off
272200021218     c                   reset                   $Tf
272300021218     c                   movea     Tf12          $Tf
272400021218     c                   Exsr      Rie_$Tf
272500021218     c                   ENDIF
272600021218      *
272700021218      * Pulisco le stringa
272800021218     c                   z-add     1             $h
272900030121     c     1             do        62            $h                2 0
273000021218     C                   if        RigaTf1($h) = '#'
273100021218     c                   movel     *blanks       RigaTf1($h)
273200021218     c                   endif
273300021218     c                   enddo
273400021218     c                   z-add     1             $h
273500030121     c     1             do        62            $h                2 0
273600021218     C                   if        RigaTf2($h) = '#'
273700021218     c                   movel     *blanks       RigaTf2($h)
273800021218     c                   endif
273900021218     c                   enddo
274000021218     c                   z-add     1             $h
274100030121     c     1             do        62            $h                2 0
274200021218     C                   if        RigaTf3($h) = '#'
274300021218     c                   movel     *blanks       RigaTf3($h)
274400021218     c                   endif
274500021218     c                   enddo
274600021218      *
274700021218      * Imposto la descrizione del tasto funzione F24 e segnalo
274800021218      * quante righe ho riempito e quale devo emettere
274900021218     c                   SELECT
275000021218      *
275100021218     c                   when      $k<>*zeros and $Y<>*zeros
275200021218     c                   eval      $loop = 3
275300021218     c                   z-add     3             $loop             1 0
275400021218     C                   movel(p)  CF2413        F24Dxx           16
275500021218      *
275600021218     c                   when      $k<>*zeros and $Y=*zeros
275700021218     c                   z-add     2             $loop             1 0
275800021218     C                   movel(p)  CF2412        F24Dxx
275900021218      *
276000021218     c                   Other
276100021218     c                   z-add     1             $loop             1 0
276200021218     C                   movel     *blanks       F24Dxx
276300021218     c                   ENDSL
276400021218      * Prima riga di tasti funzione
276500030121     c                   movea     RigaTF1       FDxx             62
276600021218     c                   z-add     1             $Rig              1 0
276700021218      *
276800021218     c                   endsr
276900021218      *-------------------------------------------------------------------------
277000021218      *  Riempio le stringhe con i campi funzione compattati
277100021218      *-------------------------------------------------------------------------
277200021218     c     Rie_$Tf       begsr
277300021218      *
277400021218     c                   z-add     1             $x                2 0
277500021218     c     '#'           lookup    $Tf($x)                                31
277600021218     c                   add       $x            $Z
277700030121     c                   IF        $Z <= 63
277800021218     c                   eval      $j = $Z - $x + 1
277900021218     c                   movea     $Tf           RigaTF1($j)
278000021218     c                   ELSE
278100021218     c                   add       $x            $K
278200030121     c                   IF        $K <= 63
278300021218     c                   eval      $j = $K - $x + 1
278400021218     c                   movea     $Tf           RigaTF2($j)
278500021218     c                   ELSE
278600021218     c                   add       $x            $Y
278700030121     c                   IF        $Y <= 63
278800021218     c                   eval      $j = $Y - $x + 1
278900021218     c                   movea     $Tf           RigaTF3($j)
279000021218     c                   ENDIF
279100021218     c                   ENDIF
279200021218     c                   ENDIF
279300021218      *
279400021218     c                   endsr
279500021218      *-------------------------------------------------------------------------
279600021218      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
279700021218      *-------------------------------------------------------------------------
279800021218     c     Ges_F24       begsr
279900021218      *
280000021218     c                   SELECT
280100021218      *
280200021218     c                   WHEN      $Loop = 2 and $Rig =2  or
280300021218     c                             $Loop = 3 and $Rig =3
280400021218     c                   movea     RigaTF1       FDxx
280500021218     c                   z-add     1             $Rig
280600021218      *
280700021218     c                   if        $loop = 2
280800021218     c                   eval      F24Dxx= Cf2412
280900021218     c                   else
281000021218     c                   eval      F24Dxx= Cf2413
281100021218     c                   endif
281200021218      *
281300021218     c                   WHEN      $Loop = 2 and $Rig =1 or
281400021218     c                             $Loop = 3 and $Rig =1
281500021218     c                   movea     RigaTF2       FDxx
281600021218     c                   z-add     2             $Rig
281700021218      *
281800021218     c                   if        $loop = 2
281900021218     c                   eval      F24Dxx= Cf2422
282000021218     c                   else
282100021218     c                   eval      F24Dxx= Cf2423
282200021218     c                   endif
282300021218      *
282400021218     c                   WHEN      $Loop = 3 and $Rig =2
282500021218     c                   movea     RigaTF3       FDxx
282600021218     c                   z-add     3             $Rig
282700021218     c                   eval      F24Dxx= Cf2433
282800021218     c                   ENDSL
282900021218      *
283000021218     c                   endsr
283100991022      *-------------------------------------------------------------------------
283200991022      *  CARICO SUBFILE VARIE
283300991022      *-------------------------------------------------------------------------
283400991022     c     CAR_VARIE     BEGSR
283500991022      *
283600991022     c                   SELECT
283700991022      *
283800991022     c                   WHEN       WSVN = *BLANKS
283900991022      *
284000991022     C                   WHEN       WSVN = §$2iva
284100991022     c                   z-add     Wvan          W0133
284200991022     c                   eval      Wtstdec = §cvfdc
284300991022     c                   exsr      EDTimporti
284400991025     c                   movel     IMPeditato    V8Civa
284500991022      *
284600991022     c                   OTHER
284700991022     c                   z-add     Wvan          W0133
284800991022     c                   eval      Wtstdec = §cvfdc
284900991022     c                   exsr      EDTimporti
285000991022     c                   movel     IMPeditato    V8Cva1
285100991022     c                   movel     WSVN          V8Csv1
285200991022     C                   Z-ADD     1             WA
285300991022     C     V8CSV1        LOOKUP    CC1(WA)                                99
285400020329     C     CC3(WA)       ifeq      'S'
285500020329      *
285600020329      * Verifico se si tratta di una bolla Fedex
285700021119     c                   if        LnaNtw = 'FED'
285800020329     C                   Z-ADD     1             WA
285900020329     C     V8CSV1        LOOKUP    CB1(WA)                                99
286000020329     C   99              MOVEL     CB2(WA)       V8CDES
286100020329     C  N99              clear                   V8CDES
286200020329     C                   else
286300991022     C   99              MOVEL     CC2(WA)       V8CDES
286400991022     C  N99              clear                   V8CDES
286500020329     C                   endif
286600020329     C                   else
286700020329     C   99              MOVEL     CC2(WA)       V8CDES
286800020329     C  N99              clear                   V8CDES
286900020329     C                   endif
287000991022     C                   add       1             NRR8
287100991022     C                   WRITE     LS05S08
287200991022      *
287300991022     c                   ENDSL
287400991022      *
287500991022     c                   ENDSR
287600040116      *-------------------------------------------------------------------------
287700040116      *  Carico le varie della seconda bolla
287800040116      *-------------------------------------------------------------------------
287900040116     c     Car_Varie1    BegSr
288000040116
288100040116s   1c                   Select
288200040116
288300040116     c                   When      wsvn = *Blanks
288400040116      * Iva
288500040116     c                   When      wsvn = §$2iva
288600040116     c                   z-add     wvan          w0133
288700040116     c                   Eval      Wtstdec = §cvfdc
288800040116     c                   ExSr      EDTimporti
288900040116     c                   Movel     IMPeditato    Vi2Iva
289000040116      * Bollo
289100040116     c                   When      wsvn = §$2bol
289200040116     c                   z-add     wvan          w0133
289300040116     c                   Eval      Wtstdec = §cvfdc
289400040116     c                   ExSr      EDTimporti
289500040116     c                   Movel     IMPeditato    Vi2Iva
289600040116      * Varia G - sempre per prima
289700040116     c                   When      wsvn = §$2vrg
289800040116     c                   Eval      Vi2Sv1 = wsvn
289900040116     c                   z-add     wvan          w0133
290000040116     c                   Eval      Wtstdec = §cvfdc
290100040116     c                   ExSr      EDTimporti
290200040116     c                   Movel     IMPeditato    Vi2Va1
290300040116      * Altre varie
290400040116   x1c                   Other
290500040116     c                   z-add     wvan          W0133
290600040116     c                   eval      Wtstdec = §cvfdc
290700040116     c                   exsr      EDTimporti
290800040116if  2c                   If        Vi2Sv2 = *Blanks
290900040116     c                   movel     IMPeditato    Vi2Va2
291000040116     c                   movel     wsvn          Vi2sv2
291100040116   x2c                   Else
291200040116if  3c                   If        Vi2Sv3 = *Blanks
291300040116     c                   movel     IMPeditato    Vi2Va3
291400040116     c                   movel     wsvn          Vi2sv3
291500040116   x3c                   Else
291600040116if  4c                   If        Vi2Sv4 = *Blanks
291700040116     c                   movel     IMPeditato    Vi2Va4
291800040116     c                   movel     wsvn          Vi2sv4
291900041130   x4c                   Else
292000041130if  5c                   If        Vi2Sv5 = *Blanks
292100041130     c                   movel     IMPeditato    Vi2Va5
292200041130     c                   movel     wsvn          Vi2sv5
292300041130   x5c                   Else
292400041130if  6c                   If        Vi2Sv6 = *Blanks
292500041130     c                   movel     IMPeditato    Vi2Va6
292600041130     c                   movel     wsvn          Vi2sv6
292700041130    6c                   EndIf
292800041130    5c                   EndIf
292900040116    4c                   EndIf
293000040116    3c                   EndIf
293100040116    2c                   EndIf
293200040116
293300040116    1c                   EndSl
293400040116
293500040116     c                   EndSr
293600991022      **********************************************************************
293700991022      * AGGANCIO TABELLA VALUTE
293800991022      **********************************************************************
293900991022     C     CTR_VALUTA    BEGSR
294000991022
294100991022     C                   MOVEL     'CV'          KCOD
294200991022     C                   MOVEL(P)  WDIV          KKEY
294300991022     C     KTAB1         CHAIN     TABEL00F                           31
294400991022     C  N31              MOVEL     TBLUNI        DSCV
294500991022
294600991022     C                   ENDSR
294700991022      **********************************************************************
294800991022      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
294900991022      **********************************************************************
295000991022     C     EDTIMPORTI    BEGSR
295100991022
295200991022      * ------------------------------------------------------------------------
295300991022      * W0133      contiene il numero da editare per il video
295400991022      * WTSTDEC    corrisponde a §CVfdc della tabella CV
295500991022      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
295600991022      * ------------------------------------------------------------------------
295700991022      *
295800991022     c                   clear                   IMPeditato
295900991022     c                   clear                   W017A
296000991022      *
296100991022     c                   if        Wtstdec = 'S'
296200991022     C                   eval      W017a = %EDITC(W0133:'2')
296300991022     c                   else
296400991022     c                   z-add     W0133         W0130
296500991022     C                   eval      W017a = %EDITC(W0130:'2')
296600991022     c                   endif
296700991022      *
296800991022     c                   move      W017a         IMPeditato
296900991022      *
297000991022     c                   ENDSR
297100991025      *****************************************************************
297200991025      *  AGGANCIO ANAGRAFICHE
297300991025      *****************************************************************
297400991025     C     CTR_ANAGRA    BEGSR
297500991025      *
297600991025     C                   clear                   DS_cnaco
297700991025     C                   clear                   DS_cnind
297800991025     C                   clear                   DS_cnclp
297900991025     C                   clear                   DS_fncls
298000991025      *
298100991025     C                   CALL      'TIBS69R'
298200991025     C                   PARM                    tibs69DS
298300991025     C                   PARM                    DS_cnaco
298400991025     C                   PARM                    DS_cnind
298500991025     C                   PARM                    DS_cnclp
298600991025     C                   PARM                    DS_fncls
298700991025      *
298800991025     C                   eval      wtibs69r = 'S'
298900991025      * per il 1° giro è inizializzata nelle specifiche "D"
299000991025     C                   clear                   TIBS69DS
299100991025      *
299200991025     C                   ENDSR
299300940805     C**************************************************************************
299400940805     C*    CMD18 VISUALIZZA LEGAMI
299500940805     C**************************************************************************
299600920326     C     CMD18         BEGSR
299700940810     C*
299800940907     C* SAVNR9 > 2 --> GESTIONE SUBFILE
299900950328    1C     SAVNR9        IFGT      2
300000940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
300100940810     C                   MOVEL     'S'           WRIEM
300200940810     C     FORC2         TAG
300300940810     C                   Z-ADD     1             REC9
300400950328    2C     WRIEM         IFEQ      'S'
300500940810     C                   WRITE     LS05T01
300600940810     C                   WRITE     LS05Z09
300700940810     C                   MOVEL     *BLANKS       WRIEM
300800950328    2C                   ENDIF
300900940810     C                   EXFMT     LS05C09
301000940810     C*
301100940810     C* CMD12 - FINE LAVORO
301200940810     C   KL              GOTO      SOTTO
301300940810     C*
301400940810     C                   READC     LS05S09                                93
301500950328    2C     *IN93         DOWEQ     '0'
301600940810     C*
301700950328    3C     V9CSCE        IFEQ      '5'
301800940810     C                   MOVEL     ' '           V9CSCE
301900950103     C*
302000950103     C* RIPRISTINO GLI INDICATORI
302100950103     C                   MOVEL     V9CI80        *IN80
302200950103     C                   MOVEL     V9CI81        *IN81
302300950103     C                   MOVEL     V9CI82        *IN82
302400950103     C                   MOVEL     V9CI83        *IN83
302500940810     C                   UPDATE    LS05S09
302600940810     C*
302700950328    4C     V9CAAN        IFEQ      *ZERO
302800940907     C                   MOVEL     V9CAAP        PA4AAS
302900940907     C                   MOVEL     V9CLPP        PA4LNP
303000940907     C                   MOVEL     V9CNRP        PA4NRS
303100940907     C                   MOVEL     V9CNSP        PA4NSP
303200940907     C                   ELSE
303300940907     C                   MOVEL     V9CAAN        PA4AAS
303400940907     C                   MOVEL     V9CLPN        PA4LNP
303500940907     C                   MOVEL     V9CNRN        PA4NRS
303600940907     C                   MOVEL     V9CNSN        PA4NSP
303700950328    4C                   ENDIF
303800940810     C                   MOVEL     *BLANKS       PA4F03
303900960903     C     PA0FLG        IFEQ      'N'
304000960903     C* N.B.: Non testo anche pa0flg='R' perchè è impossibile che si
304100960903     C*       verifichi questo caso
304200960903     C                   MOVEL     'P'           PA4FLG
304300960903     C                   ELSE
304400940810     C                   MOVEL     '3'           PA4FLG
304500960903     C                   END
304600940810     C                   MOVEL     PARAM4        KPJBU
304700940810     C                   CALL      'FNLR36R'
304800940810     C                   PARM                    KPJBA
304900940810     C                   MOVEL     KPJBU         PARAM4
305000940810     C                   MOVEL     PA4F03        WF03
305100940810     C                   MOVEL     'S'           WRIEM
305200950328    3C                   ENDIF
305300940810     C*
305400940810     C                   READC     LS05S09                                93
305500950328    2C                   ENDDO
305600940908     C*
305700940908     C* RIEMETTO IL FORMATO VIDEO DEI LEGAMI
305800950328    2C     WRIEM         IFEQ      'S'
305900940908     C                   GOTO      FORC2
306000950328    2C                   ENDIF
306100940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
306200940810     C                   ELSE
306300940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
306400940810     C                   MOVEL     COMAAN        PA4AAS
306500940810     C                   MOVEL     COMLPN        PA4LNP
306600940810     C                   MOVEL     COMNRN        PA4NRS
306700940810     C                   MOVEL     COMNSN        PA4NSP
306800940810     C                   MOVEL     *BLANKS       PA4F03
306900960903     C     PA0FLG        IFEQ      'N'
307000960903     C* N.B.: Non testo anche pa0flg='R' perchè è impossibile che si
307100960903     C*       verifichi questo caso
307200960903     C                   MOVEL     'P'           PA4FLG
307300960903     C                   ELSE
307400940810     C                   MOVEL     '3'           PA4FLG
307500960903     C                   END
307600940810     C                   MOVEL     PARAM4        KPJBU
307700940810     C                   CALL      'FNLR36R'
307800940810     C                   PARM                    KPJBA
307900940810     C                   MOVEL     KPJBU         PARAM4
308000940810     C                   MOVEL     PA4F03        WF03
308100940810     C                   MOVEL     'S'           WRIEM
308200940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
308300940810     C                   ENDIF
308400940810     C*
308500940810     C     SOTTO         TAG
308600920326     C                   ENDSR
308700940802      **************************************************************************
308800940802      * VISUALIZZO IL FORMATO VIDEO DI RIEPILOGO DELLA BOLLA
308900940802      **************************************************************************
309000110601     C     RIEPIL        BEGSR
309100940802      *
309200940802     C                   SETOFF                                       25
309300940804     C                   MOVEL     'S'           WRIEM             1
309400021218      *
309500021218      * Riempio il campone in fondo alla videata con i tasti scelti
309600021218     C                   exsr      Ges_TastiFun
309700021218     c                   eval      FD07 = FDxx
309800021218     c                   eval      F24D07 = F24Dxx
309900940802     C     FOR07         TAG
310000940804     C     WRIEM         IFEQ      'S'
310100940804     C                   WRITE     LS05T01
310200940804     C                   MOVEL     *BLANKS       WRIEM
310300940804     C                   ENDIF
310400021218      *
310500021218      * Imposto la stringa comandi del 'giro' successivo
310600021218     C                   IF        *inKY = *on and $Loop > 1
310700021218     c                   Exsr      Ges_F24
310800021218     c                   eval      FD07 = FDxx
310900021218     c                   eval      F24D07 = F24Dxx
311000021218     C                   ENDIF
311100940804     C                   EXFMT     LS05D07
311200940808     C                   MOVEL     *BLANKS       WF03
311300940802      *
311400990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
311500940802     C   KY*IN25         IFEQ      *ON
311600940802     C                   SETOFF                                       25
311700940802     C                   ELSE
311800940802     C                   SETON                                        25
311900940802     C                   ENDIF
312000940802     C   KY              GOTO      FOR07
312100940802      *
312200990118      * F3 - FINE LAVORO TORNO A SUBFILE
312300941109     C   KC              MOVEL     '1'           PA0F03
312400940802     C   KC              GOTO      ENDRIE
312500940802      *
312600940802      * TASTI DI FUNZIONE VARI
312700940802     C                   EXSR      COMAND
312800940808     C*
312900940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
313000940808     C     WF03          IFNE      *BLANKS
313100941109     C                   MOVEL     '1'           PA0F03
313200940808     C                   GOTO      ENDRIE
313300940808     C                   ENDIF
313400091125     c*
313500091125     c* verifico se inserito '?' su beneficiario C/A
313600091125     c                   if        v2cben='?'
313700091125     c                   exsr      sr_bencas
313800091125     C                   SETON                                        31
313900091125     C                   MOVEL     'S'           WRIEM
314000091125     c                   endif
314100110601
314200950629     C*
314300940802     C   31              GOTO      FOR07
314400940802      *
314500940802     C     ENDRIE        ENDSR
314600940802      **************************************************************************
314700940802      * Nel caso di interrogazione da parte di un cliente, viene verificato se
314800940802      * egli puo' interrogare le bolle e se deve essere conteggiato il traffico
314900940802      * dei byte sulla linea di comunicazione.
315000940802      * Queste informazioni sono contenute nella tabella 3K.
315100940802      *
315200940802      * Inoltre, egli puo' interrogare solo alcuni clienti (quelli a lui
315300940802      * unificati.
315400940802      **************************************************************************
315500111020     C**!!!ABICLI        BEGSR
315600940802      *
315700111020     C**!!!              MOVE      *BLANKS       $3KINB
315800090603     C***                MOVE      *BLANKS       $3KCTB
315900940802      *
316000940802      * Leggo tutta la tabella 3K riempendo una schiera con i cod.clienti
316100111020     C**!!!              Z-ADD     *ZERO         $PRIMA            1 0
316200111020     C**!!!              Z-ADD     *ZERO         $A                3 0
316300111020     C**!!!              MOVE      '3K'          KCOD
316400111020     C**!!!KTAB2         SETLL     TABEL00F
316500111020     C**!!!              DO        *HIVAL
316600111020     C**!!!KTAB2         READE     TABEL00F                               90
316700111020     C*!N90TBLFLG        IFEQ      *BLANKS
316800111020     C**!!!              MOVEL     TBLUNI        DS3K
316900111020     C**!!!KNMUS         IFEQ      §3KPRF
317000940802      *
317100940802      * Devo prendere il cod.cliente, che e' la chiave di TABEL
317200111020     C**!!!              MOVEL     TBLKEY        $CODIC            7 0
317300111020     C**!!!$CODIC        LOOKUP    CLI                                    99
317400111020     C*!N99$A            IFLT      300
317500111020     C**!!!              ADD       1             $A
317600111020     C**!!!              Z-ADD     $CODIC        CLI($A)
317700111020     C**!!!              ENDIF
317800940802      *
317900940802      * Gia' dalla prima lettura ho i due flag che abilitano il profilo
318000111020     C**!!!$PRIMA        IFEQ      *ZERO
318100111020     C**!!!              MOVE      §3KINB        $3KINB
318200090603     C***                MOVE      §3KCTB        $3KCTB
318300111020     C**!!!§3KINB        COMP      'S'                                9090
318400111020     C**!!!              Z-ADD     1             $PRIMA
318500111020     C**!!!              ENDIF
318600940802      *
318700111020     C**!!!              ENDIF
318800111020     C**!!!              ENDIF
318900111020     C*!N90              ENDDO
319000940802      *
319100111020     C**!!!              ENDSR
319200940905     C**************************************************************************
319300940905     C* RIEMPIE IL SFL CON TUTTI I LEGAMI BOLLA
319400940905     C* PARTENDO DALLA BOLLA ORIGINALE, FINO ALL'ULTIMA IN
319500940906     C* ORDINE CRONOLOGICO (ULTIMO MODELLO SWATCH)
319600940905     C**************************************************************************
319700940906     C     LEGBOL        BEGSR
319800940906     C*
319900940906     C* PULISCO IL SFL
320000940906     C                   SETOFF                                       202123
320100940906     C                   WRITE     LS05C09
320200940906     C                   SETON                                        2123
320300940906     C*
320400940906     C                   SETOFF                                       07
320500940906     C                   Z-ADD     *ZERO         SAVNR9            4 0
320600940906     C                   MOVEL     *BLANKS       WFLG1             1
320700940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
320800940905     C*
320900940906     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
321000940906     C     KBLP3         CHAIN     FNLBL01L                           99
321100940906     C   99KBLP3         CHAIN     FNLBL02L                           99
321200940906     C  N99              DO
321300940905     C*
321400940906     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
321500940906     C                   MOVEL     LBLAAO        KAAS
321600940906     C                   MOVEL     LBLLPO        KLNP
321700940906     C                   MOVEL     LBLNRO        KNRS
321800940906     C                   MOVEL     LBLNSO        KNSP
321900940906     C     KLBL3         CHAIN     FNLBL02L                           99
322000950103     C     *IN99         IFEQ      *ON
322100950103     C                   MOVEL     'N'           WORIGI            1
322200950103     C                   ELSE
322300950103     C                   MOVEL     'S'           WORIGI
322400950103     C                   ENDIF
322500940907     C*
322600940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
322700940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
322800950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
322900950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
323000940907     C                   MOVEL     ' '           V9CSCE
323100940907     C                   MOVEL     LBLAAO        V9CAAP
323200940907     C                   MOVEL     LBLLPO        V9CLPP
323300940907     C                   MOVEL     LBLLAO        V9CLAP
323400940907     C                   MOVEL     LBLNRO        V9CNRP
323500940907     C                   MOVEL     LBLNSO        V9CNSP
323600940926     C                   CLEAR                   V9CAAN
323700940926     C                   CLEAR                   V9CLPN
323800940926     C                   CLEAR                   V9CLAN
323900940926     C                   CLEAR                   V9CNRN
324000940926     C                   CLEAR                   V9CNSN
324100940907     C*
324200940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
324300940907     C     LBLAAO        IFEQ      V1CAAS
324400940907     C     LBLLPO        ANDEQ     V1CLNP
324500940907     C     LBLNRO        ANDEQ     V1CNRS
324600940907     C     LBLNSO        ANDEQ     V1CNSP
324700940907     C                   MOVEL     *BLANKS       V9CPRE
324800940907     C                   SETON                                        80
324900940907     C                   SETON                                        81
325000940907     C                   MOVEL     '1'           WFLG1
325100940907     C                   ELSE
325200940907     C                   SETOFF                                       81
325300940907     C*
325400950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
325500950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
325600950320     C                   SETOFF                                       93
325700950320     C     KLBL3         SETLL     FNARB01L                               93
325800950320     C  N93              MOVEA     PRD(1)        V9CPRE
325900950320     C  N93              SETON                                        80
326000950320     C*
326100950320     C* SE RICHIAMATO DAGLI ARRIVI (FNLR36R) NON POSSO
326200950320     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
326300950320     C   93PA0FLG        IFEQ      'A'
326400960903     C     PA0FLG        OREQ      'R'
326500950320     C                   MOVEA     PRD(4)        V9CPRE
326600950320     C                   SETON                                        80
326700950320     C                   ELSE
326800950328     C                   MOVEA     PRD(2)        V9CPRE
326900950320     C                   SETOFF                                       80
327000950320     C                   ENDIF
327100950320     C*
327200950320     C* SE NON TROVATA SU ARB PROVO SU BLP
327300950320     C  N93              DO
327400950320     C                   SETOFF                                       93
327500950320     C     KLBL3         SETLL     FNBLP01L                               93
327600950320     C*
327700950328     C   93              MOVEA     PRD(3)        V9CPRE
327800950428     C   93              SETON                                        80
327900950320     C*
328000950320     C                   ENDDO
328100940907     C*
328200950320     C                   ENDIF
328300940907     C                   SETON                                        83
328400950103     C*
328500950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
328600950103     C                   MOVEL     *IN80         V9CI80
328700950103     C                   MOVEL     *IN81         V9CI81
328800950103     C                   MOVEL     *IN82         V9CI82
328900950103     C                   MOVEL     *IN83         V9CI83
329000950103     C*
329100940907     C                   Z-ADD     1             NRR9
329200940907     C                   WRITE     LS05S09
329300940907     C                   SETON                                        20
329400950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
329500950103     C*
329600950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
329700950103     C*                      2) NON HO TROVATO L'ORIGINALE
329800950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
329900950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
330000950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
330100950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
330200950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
330300950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
330400950103     C* L'ORIGINALE E LA MAMMA.
330500950103     C     WORIGI        IFEQ      'N'
330600940907     C*
330700950103     C                   MOVEL     ' '           V9CSCE
330800950103     C                   CLEAR                   V9CAAP
330900950103     C                   CLEAR                   V9CLPP
331000950103     C                   CLEAR                   V9CLAP
331100950103     C                   CLEAR                   V9CNRP
331200950103     C                   CLEAR                   V9CNSP
331300950103     C                   MOVEL     LBLAAP        V9CAAN
331400950103     C                   MOVEL     LBLLPP        V9CLPN
331500950103     C                   MOVEL     LBLLAP        V9CLAN
331600950103     C                   MOVEL     LBLNRP        V9CNRN
331700950103     C                   MOVEL     LBLNSP        V9CNSN
331800950103     C                   SETOFF                                       8182
331900950103     C*
332000950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
332100950103     C     V9CAAN        IFEQ      V1CAAS
332200950103     C     V9CLPN        ANDEQ     V1CLNP
332300950103     C     V9CNRN        ANDEQ     V1CNRS
332400950103     C     V9CNSN        ANDEQ     V1CNSP
332500950103     C                   MOVEL     *BLANKS       V9CPRE
332600950103     C                   SETON                                        80
332700950103     C                   SETON                                        82
332800950103     C                   ELSE
332900950103     C*
333000950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
333100950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
333200950320     C                   EXSR      CHKBOL
333300950103     C*
333400950103     C                   ENDIF
333500950103     C*
333600950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
333700950103     C                   SETOFF                                       83
333800950103     C*
333900950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
334000950103     C                   MOVEL     *IN80         V9CI80
334100950103     C                   MOVEL     *IN81         V9CI81
334200950103     C                   MOVEL     *IN82         V9CI82
334300950103     C                   MOVEL     *IN83         V9CI83
334400950103     C*
334500950103     C                   ADD       1             NRR9
334600950103     C                   WRITE     LS05S09
334700950103     C*
334800950103     C                   ENDIF
334900940907     C                   SETOFF                                       83  99
335000940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
335100940906     C* CICLO FINO ALL'ULTIMO LEGAME
335200940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
335300940907     C                   Z-ADD     *ZERO         WA                3 0
335400940907     C                   Z-ADD     *ZERO         WB                3 0
335500940907     C                   MOVEA     *ZERO         AAN
335600940906     C     *IN99         DOWEQ     *OFF
335700940905     C*
335800940906     C                   MOVEL     ' '           V9CSCE
335900940907     C                   MOVEL     LBLAAP        V9CAAP
336000940907     C                   MOVEL     LBLLPP        V9CLPP
336100940907     C                   MOVEL     LBLLAP        V9CLAP
336200940907     C                   MOVEL     LBLNRP        V9CNRP
336300940907     C                   MOVEL     LBLNSP        V9CNSP
336400940907     C                   MOVEL     LBLAAN        V9CAAN
336500940907     C                   MOVEL     LBLLPN        V9CLPN
336600940907     C                   MOVEL     LBLLAN        V9CLAN
336700940907     C                   MOVEL     LBLNRN        V9CNRN
336800940907     C                   MOVEL     LBLNSN        V9CNSN
336900940907     C                   SETOFF                                       8182
337000940906     C*
337100950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
337200940907     C     LBLAAP        IFEQ      V1CAAS
337300940907     C     LBLLPP        ANDEQ     V1CLNP
337400940907     C     LBLNRP        ANDEQ     V1CNRS
337500940907     C     LBLNSP        ANDEQ     V1CNSP
337600940907     C                   SETON                                        81
337700940907     C                   ENDIF
337800940907     C*
337900940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
338000940907     C     LBLAAN        IFEQ      V1CAAS
338100940907     C     LBLLPN        ANDEQ     V1CLNP
338200940907     C     LBLNRN        ANDEQ     V1CNRS
338300940907     C     LBLNSN        ANDEQ     V1CNSP
338400940906     C                   MOVEL     *BLANKS       V9CPRE
338500940906     C                   SETON                                        80
338600940907     C                   SETON                                        82
338700940906     C                   ELSE
338800940906     C*
338900950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
339000950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
339100950320     C                   EXSR      CHKBOL
339200940906     C*
339300940906     C                   ENDIF
339400950103     C*
339500950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
339600950103     C                   MOVEL     *IN80         V9CI80
339700950103     C                   MOVEL     *IN81         V9CI81
339800950103     C                   MOVEL     *IN82         V9CI82
339900950103     C                   MOVEL     *IN83         V9CI83
340000940906     C*
340100940906     C* SCRIVO IL RECORD NEL SFL
340200940906     C                   ADD       1             NRR9
340300940906     C                   WRITE     LS05S09
340400940907     C*
340500940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
340600940907     C                   ADD       1             WA
340700940907     C                   MOVEL     LBLAAN        AAN(WA)
340800940907     C                   MOVEL     LBLLPN        LPN(WA)
340900940907     C                   MOVEL     LBLNRN        NRN(WA)
341000940907     C                   MOVEL     LBLNSN        NSN(WA)
341100940907     C*
341200940907     C     KLBL3         READE     FNLBL02L                               99
341300940906     C*
341400940907     C* SE NON HO PIU' BOLLE CON READE
341500940906     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
341600970116     C     WB            DOWLT     WA
341700970116     C     *IN99         ANDEQ     *ON
341800940907     C                   ADD       1             WB
341900940907     C                   MOVEL     AAN(WB)       KAAS
342000940907     C                   MOVEL     LPN(WB)       KLNP
342100940907     C                   MOVEL     NRN(WB)       KNRS
342200940907     C                   MOVEL     NSN(WB)       KNSP
342300940907     C     KLBL3         SETLL     FNLBL02L
342400940907     C     KLBL3         READE     FNLBL02L                               99
342500970116     C                   ENDDO
342600940906     C*
342700940906     C                   ENDDO
342800940906     C                   ENDDO
342900940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
343000940906     C*
343100940906     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
343200940906     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
343300940926     C     NRR9          IFEQ      2
343400940907     C*
343500940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
343600940907     C     2             CHAIN     LS05S09                            99
343700940907     C     V9CAAP        IFEQ      V1CAAS
343800940907     C     V9CLPP        ANDEQ     V1CLNP
343900940907     C     V9CNRP        ANDEQ     V1CNRS
344000940907     C     V9CNSP        ANDEQ     V1CNSP
344100940907     C                   MOVEL     V9CAAN        COMAAN
344200940907     C                   MOVEL     V9CLPN        COMLPN
344300940907     C                   MOVEL     V9CNRN        COMNRN
344400940907     C                   MOVEL     V9CNSN        COMNSN
344500950306     C                   MOVEL     V9CLAN        COMLAN
344600950316     C                   MOVEA     'SEGUEN.'     LBL(14)
344700940907     C                   ELSE
344800940907     C                   MOVEL     V9CAAP        COMAAN
344900940907     C                   MOVEL     V9CLPP        COMLPN
345000940907     C                   MOVEL     V9CNRP        COMNRN
345100940907     C                   MOVEL     V9CNSP        COMNSN
345200950306     C                   MOVEL     V9CLAP        COMLAN
345300950316     C                   MOVEA     'PRECED.'     LBL(14)
345400940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
345500940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
345600940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
345700940926     C     1             CHAIN     LS05S09                            99
345800940926     C* REIMPOSTO NRR9=2 PERCHE' MI SERVE DOPO
345900940926     C                   Z-ADD     2             NRR9
346000940907     C                   ENDIF
346100940907     C*
346200950316     C                   MOVEA     COMLPN        LBL(21)
346300950316     C                   MOVEA     COMNRN        LBL(25)
346400950316     C                   MOVEA     COMNSN        LBL(28)
346500950316     C                   MOVEA     COMLAN        LBL(40)
346600940906     C*
346700940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
346800940907     C                   Z-ADD     1             W                 1 0
346900940907     C     V9CPRE        LOOKUP    PRD(W)                                 99
347000950306     C                   MOVEA     LBD(W)        LBL(44)
347100940907     C     W             COMP      2                                      07
347200940906     C*
347300940906     C                   MOVEA     LBL           LEGAME
347400940906     C                   ELSE
347500940905     C*
347600940926     C     NRR9          IFGT      2
347700940906     C                   MOVEA     LB2           LEGAME
347800940906     C                   SETON                                        07
347900940906     C                   ENDIF
348000940906     C                   ENDIF
348100940906     C*
348200940907     C     NRR9          COMP      1                                    3434
348300940905     C*
348400940906     C                   Z-ADD     NRR9          SAVNR9
348500940906     C                   Z-ADD     1             NRR9
348600940912     C*
348700940912     C* RIAGGANCIO LA BOLLA IN ESAME
348800940912     C     KBLP3         CHAIN     FNBLP01L                           99
348900940905     C*
349000940905     C                   ENDSR
349100950320     C**************************************************************************
349200950320     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
349300950320     C**************************************************************************
349400950320     C     CHKBOL        BEGSR
349500950320     C*
349600950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
349700950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
349800950320     C                   SETOFF                                       93
349900950320     C     KLBL1         SETLL     FNARB01L                               93
350000950320     C  N93              MOVEA     PRD(1)        V9CPRE
350100950320     C  N93              SETON                                        80
350200950320     C*
350300950320     C* SE RICHIAMATO DAGLI ARRIVI (FNLR36R) NON POSSO
350400950320     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
350500950320     C   93PA0FLG        IFEQ      'A'
350600960903     C     PA0FLG        OREQ      'R'
350700950320     C                   MOVEA     PRD(4)        V9CPRE
350800950320     C                   SETON                                        80
350900950320     C                   ELSE
351000950320     C                   MOVEA     PRD(2)        V9CPRE
351100950320     C                   SETOFF                                       80
351200950320     C                   ENDIF
351300950320     C*
351400950320     C* SE NON TROVATA SU ARB PROVO SU BLP
351500950320     C  N93              DO
351600950320     C                   SETOFF                                       93
351700950320     C     KLBL1         SETLL     FNBLP01L                               93
351800950328     C   93              MOVEA     PRD(3)        V9CPRE
351900950428     C   93              SETON                                        80
352000950320     C*
352100950320     C                   ENDDO
352200950320     C*
352300950320     C                   ENDSR
352400940923      **************************************************************************
352500940923      *   CONTROLLI SUL FORMATO VIDEO D02
352600940923      **************************************************************************
352700940923     C     CTRD02        BEGSR
352800940923     C*
352900940923     C                   SETOFF                                       31
353000940923     C*
353100110601     c* verifico se inserito '?' su email destinatario
353200110601     c                   if        v2cemw='?'
353300110601     c                   exsr      sr_emdest
353400110601     C                   SETON                                        31
353500110601     C                   MOVEL     'S'           WRIEM
353600110601     c                   if        v2cben='?'
353700110601     c                   write     ls05t01
353800110601     c                   write     ls05d02
353900110601     c                   endif
354000110601     c                   endif
354100091125     c* verifico se inserito '?' su beneficiario C/A
354200091125     c                   if        v2cben='?'
354300091125     c                   exsr      sr_bencas
354400091125     C                   SETON                                        31
354500091125     C                   MOVEL     'S'           WRIEM
354600091125     c                   endif
354700940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
354800940923     C     V2IGCA        IFEQ      '?'
354900991022     C                   CLEAR                   DS7PQRS
355000941003     C                   MOVEL     'S'           DS7RIC
355100941003     C                   MOVEL     'V'           DS7GES
355200941003     C                   MOVEL     V2CGCA        DS7COD
355300991022     C                   MOVEL     DS7PQRS       KPJBU
355400960126     C                   CALL      'TRTB72R'                            31
355500941003     C                   PARM                    KPJBA
355600940923     C                   MOVEL     *BLANKS       V2IGCA
355700940923     C                   SETON                                        31
355800940923     C                   MOVEL     'S'           WRIEM
355900940923     C                   ENDIF
356000940923     C*
356100940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
356200940923     C     V2IGGA        IFEQ      '?'
356300991022     C                   CLEAR                   DS7PQRS
356400941003     C                   MOVEL     'S'           DS7RIC
356500941003     C                   MOVEL     'V'           DS7GES
356600941003     C                   MOVEL     V2CGGA        DS7COD
356700991022     C                   MOVEL     DS7PQRS       KPJBU
356800960126     C                   CALL      'TRTB69R'                            31
356900941003     C                   PARM                    KPJBA
357000940923     C                   SETON                                        31
357100940923     C                   MOVEL     'S'           WRIEM
357200940923     C                   MOVEL     *BLANKS       V2IGGA
357300940923     C                   ENDIF
357400940923     C*
357500940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
357600940923     C     V2IGMA        IFEQ      '?'
357700991022     C                   CLEAR                   DS7PQRS
357800941003     C                   MOVEL     'S'           DS7RIC
357900941003     C                   MOVEL     'V'           DS7GES
358000941003     C                   MOVEL     V2CGMA        DS7COD
358100991022     C                   MOVEL     DS7PQRS       KPJBU
358200960126     C                   CALL      'TRTB70R'                            31
358300941003     C                   PARM                    KPJBA
358400940923     C                   SETON                                        31
358500940923     C                   MOVEL     'S'           WRIEM
358600940923     C                   MOVEL     *BLANKS       V2IGMA
358700940923     C                   ENDIF
358800940923     C*
358900940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
359000940923     C     V2IGVA        IFEQ      '?'
359100991022     C                   CLEAR                   DS7PQRS
359200941003     C                   MOVEL     'S'           DS7RIC
359300941003     C                   MOVEL     'V'           DS7GES
359400941003     C                   MOVEL     V2CGVA        DS7COD
359500991022     C                   MOVEL     DS7PQRS       KPJBU
359600960126     C                   CALL      'TRTB73R'                            31
359700941003     C                   PARM                    KPJBA
359800940923     C                   SETON                                        31
359900940923     C                   MOVEL     'S'           WRIEM
360000941003     C                   MOVEL     *BLANKS       V2IGVA
360100940923     C                   ENDIF
360200940923     C*
360300940923     C                   ENDSR
360400091125      **************************************************************************
360500091125      *   CONTROLLI SUL FORMATO VIDEO D02
360600091125      **************************************************************************
360700091125     C     sr_bencas     BEGSR
360800091125     c                   clear                   v2cben
360900091214     c                   clear                   wdaben
361000091214     c                   clear                   wdasrsc
361100091214     c                   clear                   wdascdi
361200091216     c                   clear                   trulintds
361300091216     c
361400091216     c                   eval      iinttip='B'
361500091216     c                   eval      iinttic=v2ctic
361600091216     c                   eval      iintrscm=v2crsm
361700091216     c                   eval      iintrsco=v2crmo
361800091216     c                   eval      iintCDI =w_cdi
361900091216     c                   call      'TRULINTR'
362000091216     c                   parm                    trulintds
362100091216     c
362200091216     c                   movel     ointrsci      wdasrsc
362300091216     c
362400091216     c                   select
362500091216     c                   when      ointtipi='O'
362600091216     c                   eval      wdaben='MITTENTE  ORIGINALE'
362700091216     c
362800091216     c                   when      ointtipi='R'
362900091216     c                   eval      wdascdi=w_cdi
363000091216     c                   eval      wdaben=' O R D I N A N T E'
363100091216     c
363200091216     c                   when      ointtipi='M'
363300091216     c                   if        v2ccdm>0
363400091216     c                   movel     v2ccdm        wdascdi
363500091216     c                   endif
363600091216     c                   eval      wdaben='  M I T T E N T E'
363700091216     c                   ENDSL
363800091214     c
363900091125     c                   exfmt     ls05wdas
364000091125     c                   endsr
364100110601      **************************************************************************
364200110601      *   emissione window email destinatario
364300110601      **************************************************************************
364400110601     C     sr_emdest     BEGSR
364500110601     c                   clear                   v2cemw
364600110601     c
364700110601     c                   eval      ilv79TLA='L'
364800110601     c                   eval      ilv79Aas=blpaas
364900110601     c                   eval      ilv79Lnp=blplnp
365000110601     c                   eval      ilv79nrs=blpnrs
365100110601     c                   eval      ilv79nsp=blpnsp
365200110601     c                   call      'FNLV79R'
365300110601     c                   parm                    fnlv79ds
365400110601     c
365500110601     c                   endsr
365600130912      **************************************************************************
365700130912      *   window per visualizzare gli orari servizi
365800130912      **************************************************************************
365900130912     C     sr_OrariSr    BEGSR
366000130920
366100140131     c                   clear                   trulorsds
366200140131     c                   eval       IORefil = BLPlna
366300140131     c                   eval       IORecap = BLPcad
366400140131     c                   eval       IOReloc = BLPlod
366500140131     c                   eval       IORenar = BLPnzd
366600140131     c
366700140131     c                   eval       IORetser = 'C'
366800140131     c                   eval       IORetsp = BLPtsp
366900140131     c                   eval       IOREtfp = blptfp
367000140131     c                   eval       IOREtfa = blptfa
367100140131     c                   eval       IOREdti = blpdti
367200140131     c                   eval       IOREhti = blphti
367300140203     c                   if         wesarb='S'
367400140203     c                   eval       IOREdti = arbdti
367500140203     c                   eval       IOREhti = arbhti
367600140203     c                   endif
367700140131     c                   eval       IOREdcr = blpdcr
367800140131     c                   eval       IOREhcr = blphcr
367900140131     c                   eval       IOREtcr = blptcr
368000140131     C*
368100140131     C                   MOVEL     ' '           D98TLA
368200140131     C                   MOVEL     'P'           D98TBO
368300140131     C                   MOVEL     BLPAAS        D98AAS
368400140131     C                   MOVEL     BLPLNP        D98LNP
368500140131     C                   MOVEL     BLPNRS        D98NRS
368600140131     C                   MOVEL     BLPNSP        D98NSP
368700140131     C*
368800140131     C                   CALL      'TNSD99R'
368900140131     C                   PARM                    TNSD99DS
369000140203     c
369100140131     c                   eval       IOREdei = d98dei
369200140131     c                   eval       IOREoei = d98oei
369300140131     c*****              eval       IORedta = BLPaas * 10000 + BLPmgs
369400140203     c
369500140131     c* Data di Riferimento: data consegna se consegnata
369600140131     c                   select
369700140131     c                   when      blpdcm>0
369800140131     c                   eval      ioredta=blpdcm
369900140131     c                   other
370000140131     c* altrimenti data prevista consegna se > = oggi
370100140131     C                   time                    wrkoggiiso
370200140131     C     *iso          move      wrkoggiiso    ioredta
370300140131     c* se < di oggi, imposto oggi
370400140131     c                   if        d98dce>ioredta
370500140131     c                   eval       ioredta=d98dce
370600140131     c                   endif
370700140131     c                   endsl
370800140409
370900140409     c                   eval       IOREDDC=blpddc
371000140409     c                   eval       IORENDC=blpndc
371100140203     c* Se in distinta leggo i dati da FNARB
371200140409     c                   if        wesarb='S'
371300140409     c                   eval       IOREDDC=arbddc
371400140409     c                   eval       IORENDC=arbndc
371500140409     c                   if         arbdcm=0 and ARBfbc = *blanks and
371600140203     c                               ARBddc > 0 and
371700140203     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
371800140203     c                               arbica='R' or arbicc=' ' or arbicc='R')
371900140203     c                   eval       IOREcons = 'S'
372000140224     c* Se in distinta, passo l'orario presunto di consegna
372100140224     c                   move      arbndc        pctndc
372200140224     c                   move      arbifp        pctfgs
372300140224     c                   move      'COR'         ttrd
372400140224     c     kcor          setgt     fipct02l
372500140224     c     kcor          readpe    fipct02l
372600140310     c                   if        not %eof(fipct02l)
372700140224     c                   movel     pctdati       fipctcords
372800140307     c                   eval      ioreorp=§PCTORASTI
372900140224     c                   endif
373000140224     c
373100140203     c                   ENDIF
373200140203     c                   ENDIF
373300140131     C*
373400140325     c                   eval      iorecol=36
373500130920
373600140131     c                   call      'TRULORSR'
373700130912     c                   parm                    kpjba
373800140131     c                   parm                    trulorsds
373900130912     c
374000130912     c                   endsr
374100940810      **************************************************************************
374200940810      *   R O U T I N E     I N I Z I A L E
374300940810      **************************************************************************
374400940810     C     *INZSR        BEGSR
374500940810     C*
374600940810     C* ACCESSO   FILE BOLLE VISTA LOGICA 1
374700940810     C     KBLP3         KLIST
374800940810     C                   KFLD                    PA0AAS
374900940810     C                   KFLD                    PA0LNP
375000940810     C                   KFLD                    PA0NRS
375100940810     C                   KFLD                    PA0NSP
375200021218     C*
375300021218     C     KAR51         KLIST
375400021218     C                   KFLD                    KAAS
375500021218     C                   KFLD                    KLNP
375600021218     C                   KFLD                    KNRS
375700021218     C                   KFLD                    KNSP
375800021218     C                   KFLD                    KTRd              3
375900021218     C*
376000991022     C     KAR6          KLIST
376100991022     C                   KFLD                    PA0AAS
376200940808     C                   KFLD                    PA0LNP
376300940808     C                   KFLD                    PA0NRS
376400940808     C                   KFLD                    PA0NSP
376500991022     c                   KFLD                    KTRC
376600940810     C* ACCESSO   FILE LEGAME BOLLA
376700940810     C     KLBL1         KLIST
376800940810     C                   KFLD                    LBLAAN
376900940810     C                   KFLD                    LBLLPN
377000940810     C                   KFLD                    LBLNRN
377100940810     C                   KFLD                    LBLNSN
377200940906     C     KLBL3         KLIST
377300940906     C                   KFLD                    KAAS
377400940906     C                   KFLD                    KLNP
377500940906     C                   KFLD                    KNRS
377600940906     C                   KFLD                    KNSP
377700060216     C     Kar41         KLIST
377800940805     C                   KFLD                    KAAS
377900940805     C                   KFLD                    KLNP
378000940805     C                   KFLD                    KNRS
378100940805     C                   KFLD                    KNSP
378200940805     C                   KFLD                    KTRC
378300060216     C     Kar42         KLIST
378400951219     C                   KFLD                    KAAS
378500951219     C                   KFLD                    KLNP
378600951219     C                   KFLD                    KNRS
378700951219     C                   KFLD                    KNSP
378800940809     C     KTAB1         KLIST
378900940804     C                   KFLD                    CODUT
379000940804     C                   KFLD                    KCOD
379100940804     C                   KFLD                    KKEY
379200940809     C     KTAB2         KLIST
379300940804     C                   KFLD                    CODUT
379400940804     C                   KFLD                    KCOD
379500940808     C     KIND1         KLIST
379600940808     C                   KFLD                    CODUT
379700940808     C                   KFLD                    KKCC
379800940808     C                   KFLD                    KKSC
379900021203     C     Kfapd         klist
380000021203     C                   kfld                    kpdtip
380100021203     C                   kfld                    kpdpdr
380200040220      * File FITGD02L
380300040220     c     K02TGD02      klist
380400040220     c                   kfld                    TGDtor
380500040220     c                   kfld                    TGDogg
380600040713      * Fnorm
380700040713     c     korm          klist
380800040713     c                   kfld                    §b4poe
380900040713     c                   kfld                    §b4nsr
381000040713     c                   kfld                    §b4nor
381100040713     c                   kfld                    §b4nrv
381200140224     C     Kcor          KLIST
381300140224     C                   KFLD                    pctfgs
381400140224     C                   KFLD                    pctNDC
381500140224     C                   KFLD                    arbpdc
381600140224     C                   KFLD                    TTRD              3
381700140224     C                   KFLD                    arbAAS
381800140224     C                   KFLD                    arbLNP
381900140224     C                   KFLD                    arbNRS
382000140224     C                   KFLD                    arbNSP
382100940804     C*
382200940810     C                   MOVEL     *ZERO         COMLPN            3
382300940810     C                   MOVEL     *ZERO         COMNRN            2
382400940810     C                   MOVEL     *ZERO         COMNSN            7
382500950306     C                   MOVEL     *ZERO         COMLAN            3
382600940804     C*
382700940810     C                   Z-ADD     *ZERO         NRR8              4 0
382800940810     C                   Z-ADD     *ZERO         NRR9              4 0
382900940906     C                   Z-ADD     *ZERO         SAVNR8            4 0
383000940906     C                   Z-ADD     *ZERO         SAVNR9            4 0
383100940808     C                   MOVEL     *BLANKS       WF03              1
383200940808     C*
383300940804     C                   ENDSR
383400940804      **************************************************************************
383500941011**   SEQ
383600910912NON SEQUENZIALI
383700950316**        12345678901234567890123456789012345678901234567890
383800950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
383900920326**
384000950316LEGATA A PIU' BOLLE                       - F18=Visualizza
384100920326**
384200950316 Non Presente
384300950316 F18=Visualizza
384400940906Vedi in Int.Par
384500940907Vedi in Int.Arr
384600941011Vedi arch.stor.
384700920326**
384800920326Spedizione non Presente
384900941230
385000940907Vedi in Int.Bolle Part.
385100940907Vedi in Int.Bolle Arr.
385200941011Vedi in Archiv. Storico
385300040220** TF                                                                        *
385400040610F1=Agg.Rich.#
385500021218** TF                                                                        *
385600021218F2=C.A.#
385700021218** TF                                                                        *
385800021218F4=Tass2#
385900021218** TF                                                                        *
386000021218F5=Eventi#
386100021218** TF                                                                        *
386200021218F7=Anomalie#
386300021218** TF                                                                        *
386400021218F8=Orm#
386500130912** TF                                                                        *
386600130912F9=OrariSr#
386700130912** TF                                                                        *
386800021218F10=Variaz.#
386900021218** TF                                                                        *
387000030121F11=NoteS/B#
387100021218** TF                                                                        *
387200021218F12=Ritorno#
387300021218** TF                                                                        *
387400021218F13=Tassaz.#
387500021218** TF                                                                        *
387600021218F14=Giacenza#
387700040220** TF                                                                        *
387800040610F16=Gest.Rich.#
387900021218** TF                                                                        *
388000021218F19=Spunte#
388100021218** TF                                                                        *
388200021218F20=Affid.#
388300021218** TF                                                                        *
388400021218F21=Dett.Colli#
388500021218** TF                                                                        *
388600051201F22=C/Assegni#
