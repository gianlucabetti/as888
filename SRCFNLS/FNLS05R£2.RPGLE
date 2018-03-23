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
036200131007     D FIL_AB          S              3    DIM(85)
036300131007
036400021218     D CF2413          C                   CONST('F24=AlTasti(1/3)')
036500021218     D CF2423          C                   CONST('F24=AlTasti(2/3)')
036600021218     D CF2433          C                   CONST('F24=AlTasti(3/3)')
036700021218     D CF2412          C                   CONST('F24=AlTasti(1/2)')
036800021218     D CF2422          C                   CONST('F24=AlTasti(2/2)')
036900040220
037000930115     I****************************************************************
037100941028     I* INDICATORI UTILIZZATI
037200930115     I****************************************************************
037300961111     I* 01 - BOLLA RIENTRO
037400961111     I* 02 - BOLLA FRNACO FRONTIERA
037500990118     I* 03 - ABILITA F14 DI INTERROGAZIONE GIACENZA
037600991021     I* 04 - ABILITA F13 DI TASSAZIONE 1a BOLLA
037700960523     I* 05 - DISABILITA F7=ANOMALIE
037800991021     I* 06 - ABILITA F4 DI TASSAZIONE 2a BOLLA
037900930115     I* 07 - ESISTE LEGAME BOLLA ABILITA CMD18
038000970227     I* 08 - SI DTT IN ALTA INTENSITA'
038100971212     I* 09 - IMPORT EXPORT SDI
038200990118     I* 10 - ABILITA F10 DI INTERROGAZ. VARIAIZONI BOLLE
038300991123     I* 11 - EMETTO SCRITTA BOLLO o IVA IN TASSAZIONE 2a BOLLA
038400990118     I* 12 - ABILITA INTERROGAZIONE COMUNICAZIONI ANOMALIA
038500980704     I* 13 - EMETTO RIFERIMENTO PARTNER PARTENZA
038600000216     I* 14 - lna dd
038700000216     I* 15 - ABILITA F23
038800941028     I* 16 - ESISTE PARTICOLARITA' CONTRASSEGNO
038900941028     I* 17 - ESISTE PARTICOLARITA' GIACENZA
039000941028     I* 18 - ESISTE PARTICOLARITA' CONSEGNA
039100941028     I* 19 - ESISTE PARTICOLARITA' VARIE
039200941028     I* 20 - SFLDSP
039300941028     I* 21 - SFLDSPCTL - SFLCLR
039400990118     I* 22 - ABILITA F7, F19 E F20
039500941028     I* 23 - SFLEND
039600990118     I* 24 - ABILITA VISUAL. CONTRASSEGNO
039700990118     I* 25 - ABILITA F24 LATRI TASTI
039800951220     I* 26 - PULIZIA SFL LS05S03
039900040220      * 27   Abilita F1  = Aggiunta in reclamo
040000040220      * 28   Abilita F16 = Gestione reclamo
040100941028     I* 31 - PREMUTO TASTO FUNZIONALE
040200941028     I* 33 - ON = SPEDIZIONE IN FRANCO
040300941028     I* 33 - OFF = SPEDIZIONE IN ASSEGNATO
040400930115     I* 34 - C'E' LEGAME MA NON ABILITA CMD18
040500930115     I* 44 - INDICA A VIDEO SPEDIZIONE GIACENTE APERTA
040600010220     I* 55 - ABILITA F08 (interrogazione orm)
040700021218     I* 77 - NOTE SUPERMERCATI
040800941028     I* 80 - USATO NEL VIDEO LEGAMI
040900941028     I* 81 - USATO NEL VIDEO LEGAMI
041000941028     I* 82 - USATO NEL VIDEO LEGAMI
041100941028     I* 83 - USATO NEL VIDEO LEGAMI
041200941028     I* 93 - USATO NEL VIDEO LEGAMI
041300930505     I* 94 - EMETTO CMD23 PER CALCOLARE E VISUALIZZARE DATA CONS.
041400930505     I*      PREVISTA E IL TOTALE ORE DI RITARDO
041500930701     I* 95 - ESISTONO EVENTI DA INTERROGARE
041600930527     I* 98 - DATA CONSEGNA MERCE >< 0
041700941021     I* 99 - GENERICO, UTILIZZABILE A PIACERE
041800930115     I****************************************************************
041900000000     C     *ENTRY        PLIST
042000000000     C                   PARM                    KPJBA
042100040220     c                   parm                    FIDNA1ds
042200040220      *
042300940804     C                   MOVEL     KPJBU         PARAM0
042400981123     C**
042500981123     C                   Z-ADD     1             CODUT
042600981123     C                   CALL      'X§PARUT'
042700991022     C                   PARM                    ut§dse0f
042800021218     C                   MOVEL     REC80         CNCR80
042900981123     C*--- RICERCA CAPOCONTI
043000981123     C                   DO        50            X                 2 0
043100981123     C                   MOVE      TCU(X)        TCUDS
043200981123     C     F56           CABNE     'CG'          END1
043300981123     C     F4            COMP      '1'                                    21
043400981123     C     F4            COMP      '2'                                    22
043500981123     C     F4            COMP      '3'                                    23
043600981123     C     F4            COMP      '6'                                    27
043700981123     C** 1 CLIENTI   21
043800981123     C** 2 FORNITORI 22
043900981123     C** 3 AGENTI    23
044000981123     C     F3            COMP      '0'                                242425
044100981123     C     F3            COMP      'I'                                    26
044200981123     C** 0 ITALIA   25
044300981123     C** 1 ESTERO   24
044400981123     ** I CAPO CONTO IVA
044500981123     C   21
044600981123     CAN 24              Z-ADD     KCU(X)        KCE               4 0
044700981123     C   21
044800981123     CAN 25              Z-ADD     KCU(X)        KCI               4 0
044900981123     C   22
045000981123     CAN 24              Z-ADD     KCU(X)        KFE               4 0
045100981123     C   22
045200981123     CAN 25              Z-ADD     KCU(X)        KFI               4 0
045300981123     C   23
045400981123     CAN 24              Z-ADD     KCU(X)        KAE               4 0
045500981123     C   23
045600981123     CAN 25              Z-ADD     KCU(X)        KAI               4 0
045700981123     C   26              Z-ADD     KCU(X)        KIVA              4 0
045800981123     C   27              Z-ADD     KCU(X)        KBNA              4 0
045900981123     C     END1          TAG
046000981123     C                   END
046100981123     C                   SETOFF                                       212223
046200981123     C                   SETOFF                                       242526
046300981123     C                   SETOFF                                       27
046400940804     C*
046500990701     C                   MOVEL     PA0RSU        V1CRSU
046600940808     C                   Z-ADD     1             CODUT             1 0
046700981123     C**
046800971202     C* DETERMINO LA DATA DEL GIORNO
046900971202     C                   TIME                    W0140            14 0
047000971202     C                   MOVE      W0140         WDTGIO            8 0
047100971202     C                   Z-ADD     WDTGIO        G02DAT
047200971202     C                   MOVEL     *BLANK        G02ERR
047300971202     C                   CALL      'XSRDA8'
047400971202     C                   PARM                    WLBDAT
047500971202     C                   MOVEL     G02INV        WDATEU            8 0
047600991022      *
047700991022      * CARICO LA TABELLA $2
047800991022     C                   MOVEL     '$2'          KCOD
047900991022     C                   MOVEL(P)  1             KKEY
048000991022     C     KTAB1         CHAIN     TABEL00F                           31
048100991022     C  N31              MOVEL     TBLUNI        DS$2
048200991022      *
048300991022      * CARICO IN SCHIERA LA TABELLA CC
048400941021     C                   Z-ADD     *ZERO         WA
048500941021     C                   CLEAR                   CC1
048600941021     C                   CLEAR                   CC2
048700020329     C                   CLEAR                   CC3
048800941021     C                   MOVEL     'CC'          KCOD
048900941021     C     KTAB2         SETLL     TABEL00F
049000941021     C     KTAB2         READE     TABEL00F                               99
049100941021     C     *IN99         DOWEQ     *OFF
049200941021     C     TBLKEY        ANDNE     *BLANKS
049300941021     C     TBLKEY        ANDNE     *ZERO
049400941021     C                   ADD       1             WA
049500941021     C                   MOVE      TBLKEY        CC1(WA)
049600941021     C                   MOVEL     TBLUNI        CC2(WA)
049700020402     C                   MOVEL(P)  TBLUNI        DSCC
049800020402     C                   MOVE      §CCDFE        CC3(WA)
049900941021     C     KTAB2         READE     TABEL00F                               99
050000941021     C                   ENDDO
050100020329      *
050200020329      * CARICO IN SCHIERA LA TABELLA CB
050300020329     C                   Z-ADD     *ZERO         WA
050400020329     C                   CLEAR                   CB1
050500020329     C                   CLEAR                   CB2
050600020329     C                   MOVEL     'CB'          KCOD
050700020329     C     KTAB2         SETLL     TABEL00F
050800020329     C     KTAB2         READE     TABEL00F                               99
050900020329     C     *IN99         DOWEQ     *OFF
051000020329     C     TBLKEY        ANDNE     *BLANKS
051100020329     C     TBLKEY        ANDNE     *ZERO
051200020329     C                   ADD       1             WA
051300020329     C                   MOVE      TBLKEY        CB1(WA)
051400020329     C                   MOVEL     TBLUNI        CB2(WA)
051500020329     C     KTAB2         READE     TABEL00F                               99
051600020329     C                   ENDDO
051700991022      *
051800991022      * Disabilito F7=Anomalie se in elenco pgm chiamanti c'è fnlr44r
051900991022      * Pa0flg="N" -- > Disabilitare F7
052000991022      * Pa0flg="R" -- > Disabilitare F7 - PGM RICHIAMATO DA FNLR36R
052100991022     c                   IF        PA0FLG = 'N'  or  PA0FLG = 'R'
052200991022     C                   eval      *in05 = *on
052300991022     C                   ENDIF
052400940802      * Testo il flag che mi dice se sono richiamato da un cliente
052500940802     C                   SETON                                        22
052600111020
052700111020     C**!!!PA0CLI        IFEQ      'S'
052800940802      * Testo le abilitazioni del cliente
052900111020     C**!!!              EXSR      ABICLI
053000111020     C**!!!$3KINB        CABNE     'S'           FINE
053100111020     C**!!!              SETOFF                                       22
053200111020     C**!!!              ENDIF
053300131007      * controllo se utente partito con orari servizi
053400131007     c                   clear                   fil_ab
053500131007     c                   Clear                   wabif9
053600131007     c                   Clear                   Tibs02ds
053700131007     c                   Eval      T02Mod = 'C'
053800131007     c                   Eval      T02Sif = knsif
053900131007     c                   Eval      T02Cod = 'VPO'
054000131007     c                   Movel(p)  'DECOFI816'   T02Ke1
054100131007     c                   Call      'TIBS02R'
054200131007     c                   Parm                    kpjba
054300131007     c                   Parm                    Tibs02ds
054400131007if  1c                   If        T02Err = *Blanks
054500131007      /free
054600131007         wx= 1 ;
054700131007         wy= 1 ;
054800131007
054900131007         dow wx< 256   ;
055000131007           fil_ab(wy)= %subst(t02uni:wx:3)  ;
055100131007           wy=wy+ 1   ;
055200131007           wx=wx+ 3   ;
055300131007         enddo  ;
055400131007
055500131007      /end-free
055600131007e   1c                   EndIf
055700131007     c                   move      simpou        w003a             3
055800140224     c                   if        %lookup(w003a:fil_ab)>0 or fil_ab(1)='999'
055900131007     c                   eval      wabif9='S'
056000131007     c                   endif
056100921013     C*
056200900511     C* INIZIALIZZO CAMPI ANCHE PER MEMORIZZARE BOLLA CHE NON USO
056300900427     C                   Z-ADD     0             BLPNRS
056400900427     C                   Z-ADD     0             BLPNSP
056500900427     C                   Z-ADD     0             BLPLNA
056600900427     C                   Z-ADD     0             BLPRMN
056700930701     C                   CLEAR                   PARAM1
056800930701     C                   CLEAR                   PARAM2
056900930701     C                   CLEAR                   PARAM4
057000940810     C                   CLEAR                   PARA64
057100940810     C                   CLEAR                   PARAB5
057200991022      *----------------------------------------------------------
057300991022      * CARICO BOLLA
057400900516     C                   EXSR      CARBLP
057500940923     C*
057600940923     C* SE LA BOLLA E' INESISTENTE VADO A FINE PROGRAMMA
057700940923     C     PA0NSP        IFEQ      *ZERO
057800940923     C                   GOTO      FINE
057900940923     C                   ENDIF
058000040220      *
058100040220      *?Abilito F1=Aggiunta in Reclami?
058200040220      * (se richiamato da pgm. FIDNA1R...)
058300040220     c                   eval      *in27  = *off
058400040303     c                   if            SDSprm >  1
058500040308     c                             and IA1ins = ' '
058600040220     c                   eval      *in27  = *on
058700040220     c                   endif
058800040220      *
058900040220      *?Abilito F16=Visualizzazione Reclami?
059000040220      * (se richiamato da pgm. FIDNA1R...)
059100040220     c                   eval      TGDtor  = 'S'
059200040220     c                   eval      TGDogg  = ds_OggSped
059300040220     c     K02TGD02      setll     FITGD000
059400040302     c                   eval      *in28   = (%equal(FITGD02L)   and
059500040303     c                                           (SDSprm =  1
059600040303     c                                        or (SDSprm >  1
059700040303     c                                        and IA1ins <> 'N')))
059800900427     C*
059900931115     C* SOLO SE C'E' CONTRASSEGNO VISUALIZZO IL TASTO CMD22
060000931116     C* VERIFICO NELL'ARCHIVIO SE C'E' CONTRASSEGNO
060100990118     C                   SETOFF                                       2412
060200011120     C     KBLP3         SETLL     FNCSB01L
060300011120     c                   if        %equal(fncsb01l)
060400051115     C     KBLP3         SETLL     Fiar901L
060500051115     c                   if        %equal(fiar901l)
060600011120     c                   eval      *in24 = *on
060700011120     c                   endif
060800011120     c                   endif
060900990118     C* CONTROLLO SE C'E' DANNO
061000990125     C                   EXSR      CTRDAN
061100940322      *
061200940802      * EMETTO IL FORMATO DI RIEPILOGO DELLA BOLLA
061300940802     C                   EXSR      RIEPIL
061400940802      *
061500940802      * CMD 3 - FINE LAVORO TORNO A SUBFILE
061600940808     C     *INKC         IFEQ      *ON
061700940808     C     WF03          ORNE      *BLANKS
061800941109     C                   MOVEL     '1'           PA0F03
061900940808     C                   GOTO      FINE
062000940808     C                   ENDIF
062100940802      *
062200940802      * PER F15 VISUALIZZO TUTTA LA BOLLA, PER INVIO ESCO
062300940802     C  NKP              GOTO      FINE
062400931115     C*
062500931115     C                   SETOFF                                       25
062600940804     C                   MOVEL     'S'           WRIEM
062700021218      *
062800021218      * Riempio il campone in fondo alla videata con i tasti scelti
062900021218     C                   exsr      Ges_TastiFun
063000021218     c                   eval      FD01 = FDxx
063100021218     c                   eval      F24D01 = F24Dxx
063200900427     C     FOR01         TAG
063300940804     C     WRIEM         IFEQ      'S'
063400940804     C                   WRITE     LS05T01
063500940804     C                   MOVEL     *BLANKS       WRIEM
063600940804     C                   ENDIF
063700940804     C*
063800021218      *
063900021218      * Imposto la stringa comandi del 'giro' successivo
064000021218     C                   IF        *inKY = *on and $Loop > 1
064100021218     c                   Exsr      Ges_F24
064200021218     c                   eval      FD01 = FDxx
064300021218     c                   eval      F24D01 = F24Dxx
064400021218     C                   ENDIF
064500021218      *
064600940804     C                   EXFMT     LS05D01
064700940808     C                   MOVEL     *BLANKS       WF03
064800931115      *
064900990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
065000931115     C   KY*IN25         IFEQ      *ON
065100931115     C                   SETOFF                                       25
065200931115     C                   ELSE
065300931115     C                   SETON                                        25
065400931115     C                   ENDIF
065500931115     C   KY              GOTO      FOR01
065600910806     C*
065700900530     C* CMD 3 - FINE LAVORO TORNO A SUBFILE
065800900530     C   KC              GOTO      FINE
065900921013     C*
066000921013     C* CMD12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
066100921013     C   KL              GOTO      FINE
066200911105     C*
066300921013     C* TASTI DI FUNZIONE VARI
066400921013     C                   EXSR      COMAND
066500940808     C*
066600940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
066700940808     C     WF03          IFNE      *BLANKS
066800941109     C                   MOVEL     '1'           PA0F03
066900940808     C                   GOTO      FINE
067000940808     C                   ENDIF
067100940808     C*
067200921013     C   31              GOTO      FOR01
067300921013     C*
067400900427     C* SE TUTTO OK EMETTO FORMATO2
067500931115     C                   SETOFF                                       25
067600940804     C                   MOVEL     'S'           WRIEM
067700021218      *
067800021218      * Riempio il campone in fondo alla videata con i tasti scelti
067900021218     C                   exsr      Ges_TastiFun
068000021218     c                   eval      FD02 = FDxx
068100021218     c                   eval      F24D02 = F24Dxx
068200900427     C     FOR02         TAG
068300940804     C     WRIEM         IFEQ      'S'
068400940804     C                   WRITE     LS05T01
068500940804     C                   MOVEL     *BLANKS       WRIEM
068600940804     C                   ENDIF
068700940804     C*
068800021218      * Imposto la stringa comandi del 'giro' successivo
068900021218     C                   IF        *inKY = *on and $Loop > 1
069000021218     c                   Exsr      Ges_F24
069100021218     c                   eval      FD02 = FDxx
069200021218     c                   eval      F24D02 = F24Dxx
069300021218     C                   ENDIF
069400021218      *
069500940804     C                   EXFMT     LS05D02
069600940808     C                   MOVEL     *BLANKS       WF03
069700931115      *
069800990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
069900931115     C   KY*IN25         IFEQ      *ON
070000931115     C                   SETOFF                                       25
070100931115     C                   ELSE
070200931115     C                   SETON                                        25
070300931115     C                   ENDIF
070400931115     C   KY              GOTO      FOR02
070500910806     C*
070600900427     C* CMD12 - RITORNO VIDEATA 1
070700900427     C   KL              GOTO      FOR01
070800940923     C*
070900940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
071000940923     C                   EXSR      CTRD02
071100940923     C   31              GOTO      FOR02
071200921013     C*
071300921013     C* TASTI DI FUNZIONE VARI
071400921013     C                   EXSR      COMAND
071500940808     C*
071600940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
071700940808     C     WF03          IFNE      *BLANKS
071800941109     C                   MOVEL     '1'           PA0F03
071900940808     C                   GOTO      FINE
072000940808     C                   ENDIF
072100921013     C   31              GOTO      FOR02
072200920429     C*
072300910806     C*
072400930112     C* SE ESISTONO DATI ENTRO IN TERZA VIDEATA
072500931115     C                   SETOFF                                       25
072600940804     C                   MOVEL     'S'           WRIEM
072700021218      *
072800021218      * Riempio il campone in fondo alla videata con i tasti scelti
072900021218     C                   exsr      Ges_TastiFun
073000021218     c                   eval      FD03 = FDxx
073100021218     c                   eval      F24D03 = F24Dxx
073200930112     C     FOR03         TAG
073300940804     C     WRIEM         IFEQ      'S'
073400940804     C                   WRITE     LS05T01
073500940804     C                   MOVEL     *BLANKS       WRIEM
073600940804     C                   ENDIF
073700940804     C*
073800951219     C                   Z-ADD     1             REC3
073900021218      *
074000021218      * Imposto la stringa comandi del 'giro' successivo
074100021218     C                   IF        *inKY = *on and $Loop > 1
074200021218     c                   Exsr      Ges_F24
074300021218     c                   eval      FD03 = FDxx
074400021218     c                   eval      F24D03 = F24Dxx
074500021218     C                   ENDIF
074600951219     C                   WRITE     LS05Z03
074700951219     C                   EXFMT     LS05C03
074800940808     C                   MOVEL     *BLANKS       WF03
074900931115      *
075000990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
075100931115     C   KY*IN25         IFEQ      *ON
075200931115     C                   SETOFF                                       25
075300931115     C                   ELSE
075400931115     C                   SETON                                        25
075500931115     C                   ENDIF
075600931115     C   KY              GOTO      FOR03
075700910806     C*
075800900507     C* CMD12 - RITORNO VIDEATA 2
075900900507     C   KL              GOTO      FOR02
076000930114     C*
076100930114     C* TASTI DI FUNZIONE VARI
076200930114     C                   EXSR      COMAND
076300940808     C*
076400940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
076500940808     C     WF03          IFNE      *BLANKS
076600941109     C                   MOVEL     '1'           PA0F03
076700940808     C                   GOTO      FINE
076800940808     C                   ENDIF
076900930114     C   31              GOTO      FOR03
077000900504     C*
077100000000     C     FINE          TAG
077200971202     C*
077300971202     C                   MOVEL     'C'           D55TLA
077400971202     C                   CALL      'FNLV55R'
077500991022     C                   PARM                    FNLV55DS
077600991025      *
077700991025     C                   IF        Wtibs69r  <>  *BLANKS
077800991025     C                   eval      I69TLA  = 'C'
077900991025     C                   call      'TIBS69R'
078000991025     C                   parm                    TIBS69DS
078100991025     c                   ENDIF
078200040714     C*
078300040714     C                   MOVEL     'C'           I12TLA
078400040714     C                   CALL      'FIDN12R'
078500040714     C                   PARM                    FIDN12DS
078600091216     C*
078700091216     C                   MOVEL     'C'           Iinttla
078800091216     C                   CALL      'TRULINTR'
078900091216     C                   PARM                    trulintds
079000040714      *
079100991025      *
079200930614     C* REIMPOSTO KPJBU
079300940810     C   KC              MOVEL     '1'           PA0F03
079400940804     C                   MOVEL     PARAM0        KPJBU
079500930614     C*
079600000000     C                   SETON                                        LR
079700940808     C**************************************************************************
079800940808     C*    COMANDI UGUALI NELLE 2 VIDEATE
079900940808     C**************************************************************************
080000921013     C     COMAND        BEGSR
080100931115     C*
080200921013     C                   SETOFF                                       31
080300040220      *
080400040220      *?F1 - Aggiunta bolla in reclamo?
080500040220if  1c                   if        *inKA   = *on
080600040220     c                   clear                   FIDNA1ds
080700040220     c                   eval      IA1ins  = 'S'
080800040220     c                   eval      IA1tor  = 'S'
080900040220     c                   eval      IA1ogg  = ds_OggSped
081000040220     c                   goto      Fine
081100040220     c*****              return
081200040220e   1c                   endif
081300040220      *
081400990118     C* F5 - INTERROGAZIONE EVENTI
081500040220if  1C     *INKE         IFEQ      *ON
081600930526     C                   SETON                                        31
081700941213     C                   CLEAR                   PARAB5
081800940804     C                   MOVEL     PA0AAS        PB5AAS
081900940804     C                   MOVEL     PA0LNP        PB5LNP
082000940804     C                   MOVEL     PA0NRS        PB5NRS
082100940804     C                   MOVEL     PA0NSP        PB5NSP
082200940804     C                   MOVEL     'FNLS05R'     PB5PGM
082300940804     C                   MOVEL     V1CRSU        PB5DSA
082400940808     C                   MOVEL     V1CDSP        PB5DSP
082500940804     C                   MOVEL     V2CRMN        PB5RMN
082600940804     C                   MOVEL     V1CLNA        PB5LNA
082700940804     C                   MOVEL     V1DLNA        PB5LN1
082800940804     C                   MOVEL     V2CRSD        PB5RSD
082900940804     C                   MOVEL     V3CDCP        PB5DCP
083000940804     C                   MOVEL     V3CTMC        PB5TMC
083100940804     C                   MOVEL     PARAB5        KPJBU
083200940804     C                   CALL      'FNLRB5R'
083300930526     C                   PARM                    KPJBA
083400940808     C                   MOVEL     '1'           PA0B5
083500940808     C                   MOVEL     'S'           WRIEM
083600040220e   1C                   ENDIF
083700930526     C*
083800921013     C* CMD 7 - VISUALIZZA ANOMALIE
083900040220if  1C     *INKG         IFEQ      *ON
084000991022     C                   CLEAR                   FNLR44DS
084100960523     C                   MOVEL     PA0AAS        D44AAS
084200960523     C                   MOVEL     PA0LNP        D44FIL
084300960523     C                   MOVEL     PA0NRS        D44NRS
084400960523     C                   MOVEL     PA0NSP        D44NSP
084500960523     C                   MOVEL     'P'           D44TBO
084600960523     C                   MOVE      'I'           D44MOD
084700960523     C                   SETON                                        31
084800991022     C                   MOVEL     FNLR44DS      KPJBU
084900960523     C                   CALL      'FNLR44R'
085000960523     C                   PARM                    KPJBA
085100940808     C                   MOVEL     'S'           WRIEM
085200040220e   1C                   ENDIF
085300010220     C*
085400010220     C* CMD 8 - VISUALIZZA ORM
085500040220if  1C     *INKH         IFEQ      *ON
085600010220
085700010220     C                   clear                   PARM01
085800010220     C                   move      v1cpoe        ppoe
085900010220     C                   move      v1cnor        pnor
086000010220     C                   move      v1cnsr        pnsr
086100010220     C                   move      v1cnrv        pnrv
086200010220     C                   move      '5'           psce
086300010220     C                   move      v1clnp        pfgs
086400010220     C                   movel(p)  PARM01        KPJBU
086500010220
086600010220     C                   call      'FIOR07R'
086700010220     C                   parm                    KPJBA
086800010220     C                   MOVEL     'S'           WRIEM
086900010220     C                   SETON                                        31
087000010220
087100040220e   1C                   ENDIF
087200130912c
087300130912     c* F9 - Orari servizi
087400130912     c                   if        *inki=*on
087500131007     c                   if        wabif9='S'
087600130930     c                   exsr      sr_orarisr
087700131007     c                   endif
087800130927     C                   MOVEL     'S'           WRIEM
087900130927     C                   SETON                                        31
088000130912e   1C                   ENDIF
088100021218     C*
088200021218     C* F11 - Note supermercati
088300040220if  1C     *INKK         IFEQ      *ON
088400021218     C     *IN77         ANDEQ     *ON
088500021218      * imposto i campi 'bolla' della ds
088600021218     C                   clear                   FnLrb3ds
088700030122     c                   Movel     v1cRsu        Rb3Rsu
088800030121     C                   z-add     SimFel        RB3Fil
088900021218     C                   z-add     V1CAAS        RB3AAS
089000030121     C                   z-add     V1CLP1        RB3LNP
089100021218     C                   z-add     V1CNRS        RB3NRS
089200021218     C                   z-add     V1CNSP        RB3NSP
089300021218     C                   z-add     V1CDSP        RB3DSP
089400021218     C                   z-add     BLPRMN        RB3RMN
089500021218     C                   z-add     V1CLNA        RB3LNA
089600021218     C                   movel     BLPRSD        RB3RSD
089700021218     C                   z-add     V1CPKF        RB3PKF
089800021218     C                   z-add     V1CNCL        RB3NCL
089900021218     C                   z-add     V1CVLF        RB3VLB
090000030703      * bancali - part.varie "B"
090100040220If  2c                   If        %subst(BlpGva:1:1) = 'B'
090200030121     C                   z-add     §Ar5Tba       RB3Tba
090300030121     C                   z-add     V1cNba        RB3Nba
090400030121     C                   z-add     §Ar5Tb2       RB3Tb2
090500030121     C                   z-add     V1cNb2        RB3Nb2
090600030703     c                   Movel     V2cGva        Rb3Gva
090700040220e   2c                   EndIf
090800030703      * bancali - part.varie "O"
090900040220If  2c                   If        %subst(BlpGva:1:1) = 'O'
091000030703     C                   z-add     §Ar5bTba      RB3Tba
091100030703     C                   z-add     V1cNba        RB3Nba
091200030703     C                   z-add     §Ar5bTb2      RB3Tb2
091300030703     C                   z-add     V1cNb2        RB3Nb2
091400030703     c                   Movel     V2cGva        Rb3Gva
091500030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
091600030703     c                   Z-Add     §Ar5bCor      Rb3Cor
091700040220e   2c                   EndIf
091800021218      * richiamo PGM per note supermercati
091900021218     C                   call      'FNLRB3R'
092000030122     c                   Parm                    Kpjba
092100030121     C                   PARM                    Fnlrb3Ds
092200021218      *
092300030703     c                   Eval      *In31 = *On
092400021218     C                   MOVEL     'S'           WRIEM
092500040220e   1C                   ENDIF
092600921013     C*
092700921013     C* CMD14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
092800040220if  1C     *INKN         IFEQ      *ON
092900930701     C     *IN03         ANDEQ     *ON
093000941213     C     PA0GIA        ANDEQ     *BLANK
093100991022     C                   CLEAR                   FNLG30DS
093200941213     C                   MOVEL     'P'           D30POA
093300941213     C                   MOVEL     PA0AAS        D30AAS
093400941213     C                   MOVEL     PA0LNP        D30LNP
093500941213     C                   MOVEL     PA0NRS        D30NRS
093600941213     C                   MOVEL     PA0NSP        D30NSP
093700991022     C                   MOVEL     FNLG30DS      KPJBU
093800930701     C                   SETON                                        31
093900941213     C                   CALL      'FNLG30C'
094000921013     C                   PARM                    KPJBA
094100940808     C                   MOVEL     'S'           WRIEM
094200040220e   1C                   ENDIF
094300040220      *
094400040220      *?F16 - Gestione reclami?
094500040226if  1c                   if        *inKQ   = *on
094600040226     c                   reset                   FIDNA2ds
094700040226if  2c                   if        SDSprm  > 1
094800040226     c                   eval      IA2mod  = 'I'
094900040226e   2c                   endif
095000040226     c                   eval      IA2ogg  = ds_OggSped
095100040226     c                   call      'FIDNA2R'
095200040226     c                   parm                    KPJBA
095300040226     c                   parm                    FIDNA2ds
095400040226     c                   movel     OA2f03        WF03
095500040226     c                   movel     'S'           WRIEM
095600051201     C                   SETON                                        31
095700040220e   1c                   endif
095800921013     C*
095900921013     C* CMD18 - LEGAME BOLLA: VADO IN INTERROG. BOLLE ARRIVI
096000040220if  1C     *INKS         IFEQ      *ON
096100921013     C                   EXSR      CMD18
096200921013     C                   SETON                                        31
096300941222     C                   MOVEL     'S'           WRIEM
096400040220e   1C                   ENDIF
096500921013     C*
096600921013     C* CMD21 - DETTAGLIO SEGNACOLLI
096700040220if  1C     *INKV         IFEQ      *ON
096800940808     C                   MOVEL     PA0AAS        P64AAS
096900940808     C                   MOVEL     PA0LNP        P64LNP
097000940808     C                   MOVEL     PA0NRS        P64NRS
097100940808     C                   MOVEL     PA0NSP        P64NSP
097200940808     C                   MOVEL     *BLANKS       P64F03
097300940808     C                   MOVEL     *BLANKS       P64FLG
097400940808     C                   MOVEL     *BLANKS       P64PGM
097500940808     C                   MOVEL     'FNLS05R'     P64PGM
097600940808     C                   MOVEL     V1CRSU        P64RSU
097700940808     C                   MOVEL     PA0CML        P64CML
097800111020     C**!!!              MOVEL     PA0CLI        P64CLI
097900940808     C                   MOVEL     PARA64        KPJBU
098000930701     C                   SETON                                        31
098100940804     C                   CALL      'FNLS64R'
098200921013     C                   PARM                    KPJBA
098300940808     C                   MOVEL     KPJBU         PARA64
098400940808     C                   MOVEL     P64F03        WF03
098500940808     C                   MOVEL     'S'           WRIEM
098600040220e   1C                   ENDIF
098700921013     C*
098800990118     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
098900040220do  1C   KJ              DO
099000930526     C                   SETON                                        31
099100940804     C                   MOVEL     PA0AAS        PA2AAS
099200940804     C                   MOVEL     PA0LNP        PA2LNP
099300940804     C                   MOVEL     PA0NRS        PA2NRS
099400940804     C                   MOVEL     PA0NSP        PA2NSP
099500940804     C                   MOVEL     *BLANKS       PA2F03
099600940804     C                   MOVEL     *BLANKS       PA2FLG
099700940804     C                   MOVEL     *BLANKS       PA2PGM
099800940804     C                   MOVEL     'FNLS05R'     PA2PGM
099900940804     C                   MOVEL     V1CRSU        PA2DSA
100000940808     C                   MOVEL     V1CDSP        PA2DSP
100100940804     C                   MOVEL     V2CRMN        PA2RMN
100200940804     C                   MOVEL     V1CLNA        PA2LNA
100300940804     C                   MOVEL     V1DLNA        PA2LN1
100400940804     C                   MOVEL     V2CRSD        PA2RSD
100500950628     C                   MOVEL     'P'           PA2TBO
100600941222     C                   MOVEL     PA0SIM        PA2FEL
100700940804     C                   MOVEL     PARAM2        KPJBU
100800940804     C                   CALL      'FNLR75R'
100900921013     C                   PARM                    KPJBA
101000940808     C                   MOVEL     '1'           PA075
101100940808     C                   MOVEL     'S'           WRIEM
101200040220e   1C                   ENDDO
101300921013     C*
101400990118     C* F19 - INTERROGA SPUNTE
101500040220if  1C     *INKT         IFEQ      *ON
101600930701     C                   SETON                                        31
101700101203     C                   MOVEL     PA0AAS        vi1boan
101800101203     C                   MOVEL     PA0LNP        vi1bolp
101900101203     C                   MOVEL     PA0NRS        vi1bors
102000101203     C                   MOVEL     PA0NSP        vi1bosp
102100950116     C                   MOVEL     BLPFLS        VI1LNP
102200940804     C                   MOVEL     PA0NRS        VI1NRS
102300941230     C                   MOVEL     9             VI1NPG
102400930701     C                   MOVEL     99            VI1NPS
102500101203     C                   MOVEL     'FNLS05R'     VI1PGM
102600930701     C                   MOVEL     '1'           VI1FLG
102700930701     C                   Z-ADD     BLPLNA        VI1LNA
102800930701     C*
102900040220if  2C     BLPNCD        IFGT      0
103000930701     C                   Z-ADD     BLPNCD        VI1NCD
103100930701     C                   Z-ADD     BLPNCA        VI1NCA
103200040220x   2C                   ELSE
103300930701     C*
103400940802     C     KBLP3         SETLL     FNBLT01L
103500940802     C     KBLP3         READE     FNBLT01L                               31
103600040220do  3C  N31              DO
103700940808     C                   Z-ADD     BLTNSC        VI1NCD
103800940802     C     KBLP3         SETGT     FNBLT01L
103900940802     C     KBLP3         READPE    FNBLT01L                               31
104000940808     C  N31BLTNSC        IFNE      VI1NCD
104100940808     C                   MOVEL     BLTNSC        VI1NCA
104200040220     C                   ENDIF
104300040220e   3C                   ENDDO
104400040220e   2C                   ENDIF
104500930701     C*
104600930701     C                   MOVEL     PARAM1        KPJBU
104700940804     C                   CALL      'FNLR04R'
104800921013     C                   PARM                    KPJBA
104900940808     C                   MOVEL     'S'           WRIEM
105000040220e   1C                   ENDIF
105100930505     C*
105200990118     C* F23 - VISUALIZZA DATA CONSEGNA PREVISTA E ORE DI RITARDO
105300040220do  1C   KX              DO
105400930505     C                   SETOFF                                       94
105500930505     C                   SETON                                        31
105600930505     C*
105700930505     C                   MOVEL     ' '           D98TLA
105800930505     C                   MOVEL     'P'           D98TBO
105900930505     C                   MOVEL     BLPAAS        D98AAS
106000930505     C                   MOVEL     BLPLNP        D98LNP
106100930505     C                   MOVEL     BLPNRS        D98NRS
106200930505     C                   MOVEL     BLPNSP        D98NSP
106300930505     C*
106400970925     C                   CALL      'TNSD99R'
106500991022     C                   PARM                    TNSD99DS
106600930505     C*
106700940808     C                   Z-ADD     D98DCE        G02INV
106800940808     C                   Z-ADD     *ZEROS        G02DAT
106900940808     C                   MOVEL     '3'           G02ERR
107000940808     C                   CALL      'XSRDA8'
107100940808     C                   PARM                    WLBDAT
107200940808     C                   Z-ADD     G02DAT        V1CDCE
107300940808     C                   Z-ADD     D98NRC        V1CNRC
107400930526     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
107500930526     C*   CONSEGNA PREVISTA
107600940808     C  N98              Z-ADD     D98HCE        V1CHMC
107700930528     C  N98              Z-ADD     D98HCE        VIDHC2
107800930505     C*
107900940808     C* SE PA098= 1  E' STATA FATTA CALL PGM
108000940808     C                   MOVEL     '1'           PA098
108100040220e   1C                   ENDDO
108200931115     C*
108300990118     C* F20 - VISUALIZZAZIONE AFFIDABILITA'
108400040220do  1C   KU              DO
108500950102     C                   CLEAR                   FNLR97
108600950102     C                   Z-ADD     PA0AAS        R97AAS
108700950102     C                   Z-ADD     PA0LNP        R97LNP
108800950102     C                   Z-ADD     PA0NRS        R97NRS
108900950102     C                   Z-ADD     PA0NSP        R97NSP
109000950102     C                   Z-ADD     V1CDSP        R97DSP
109100950102     C                   Z-ADD     V2CRMN        R97RMN
109200950102     C                   Z-ADD     V1CLNA        R97LNA
109300950102     C                   Z-ADD     V3CDCP        R97DCP
109400950102     C                   MOVEL     V2CRSD        R97RSD
109500950102     C                   MOVEL     V1CRSU        R97RSU
109600950328     C                   MOVEL     V1DLNA        R97DLA
109700950102     C                   MOVEL     PA0CML        R97CML
109800950102     C                   MOVE      'P'           R97AP
109900950102     C   95              MOVE      '1'           R97EVE
110000950102     C                   Z-ADD     V1CDBR        R97DBR
110100950102     C                   Z-ADD     V1CDPC        R97DPC
110200950102     C                   Z-ADD     V1CDUC        R97DUC
110300950328     C                   MOVEL     V3CTMC        R97TMC
110400950102     C                   Z-ADD     V2CCAS        R97CAS
110500950102     C                   MOVEL     V2CVCA        R97VCA
110600931115     C                   MOVE      *BLANKS       KPJBU
110700950102     C                   MOVEL     FNLR97        KPJBU
110800940804     C                   CALL      'FNLR97R'                            31
110900931115     C                   PARM                    KPJBA
111000950102     C                   MOVEL     KPJBU         FNLR97
111100950102     C                   MOVEL     R97F03        WF03
111200931115     C                   SETON                                        31
111300940808     C                   MOVEL     'S'           WRIEM
111400040220e   1C                   ENDDO
111500931115     C*
111600990118     C* F22 - INTERROGAZIONE CONTRASSEGNO, SOLO SE PRESENTE
111700040220if  1C   KWV2CCAS        IFGT      *ZERO
111800991022     C                   CLEAR                   tnsc20ds
111900950125     C                   Z-ADD     1             D20INT
112000950125     C                   Z-ADD     PA0AAS        D20ANN
112100950125     C                   Z-ADD     PA0LNP        D20LNP
112200950125     C                   Z-ADD     PA0NRS        D20NSR
112300950125     C                   Z-ADD     PA0NSP        D20NSP
112400991022     C                   MOVEL     tnsc20ds      KPJBU
112500950125     C                   CALL      'TNSC23R'                            31
112600931115     C                   PARM                    KPJBA
112700931115     C                   SETON                                        31
112800040220     C                   MOVEL     'S'           WRIEM
112900040220e   1C                   ENDIF
113000941021     C*
113100991022     C* ? FORMATO VIDEO:  LS05D08  (TASSAZIONE 1a BOLLA) ?
113200991022     C*  EMETTO SOLO SE VI E' TASSAZIONE
113300040220do  1C   KM              DO
113400941021     C     WRIEM         IFEQ      'S'
113500941021     C                   WRITE     LS05T01
113600941021     C                   MOVEL     *BLANKS       WRIEM
113700941021     C                   ENDIF
113800941021     C                   WRITE     LS05D08
113900941021     C*
114000941021     C                   EXFMT     LS05C08
114100941021     C                   SETON                                        31
114200040220e   1C                   ENDDO
114300991022      *
114400991022      * ? FORMATO VIDEO:  LS05D04  (TASSAZIONE 2a BOLLA) ?
114500040220do  1C   KD              DO
114600991022     C     WRIEM         IFEQ      'S'
114700991022     C                   WRITE     LS05T01
114800991022     C                   MOVEL     *BLANKS       WRIEM
114900991022     C                   ENDIF
115000991022     C                   EXFMT     LS05D04
115100991022     C                   SETON                                        31
115200040220e   1C                   ENDDO
115300991022      **
115400990118     C** F2 - INTERROGAZIONE C.A. SE CE N'E' PIU' DI UNA EMETTO SFL
115500990129     C* Pa0flg="C" -- > Disabilitare F2 PERCHE' RICHIAMATO DA C.A.
115600040220if  1C   KBPA0FLG        IFNE      'C'
115700990129     C                   EXSR      CMD02
115800040220e   1C                   ENDIF
115900931115     C*
116000921013     C                   ENDSR
116100990125     C**************************************************************************
116200990125     C** F2 - INTERROGAZIONE C.A.
116300990125     C**************************************************************************
116400990125     C     CMD02         BEGSR
116500990125     C     CONTCA        IFEQ      1
116600991022     C                   CLEAR                   FIDN00DS
116700990125     C                   Z-ADD     VHCAAC        I00AAC
116800990125     C                   Z-ADD     VHCFIL        I00FIL
116900990125     C                   Z-ADD     VHCNCA        I00NCA
117000990125     C                   Z-ADD     PA0AAS        I00AAS
117100990125     C                   Z-ADD     PA0NRS        I00NRS
117200990125     C                   Z-ADD     PA0NSP        I00NSP
117300990125     C                   Z-ADD     PA0LNP        I00LNP
117400990125     C                   MOVEL     'P'           I00TPB
117500991022     C                   MOVEL     FIDN00DS      KPJBU
117600990125     C                   CALL      'FIDN01R'
117700990125     C                   PARM                    KPJBA
117800000309     C                   PARM                    CALLricors        1
117900990125     C                   SETON                                        31
118000990125     C                   MOVEL     'S'           WRIEM
118100990125     C                   ELSE
118200990125     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
118300990125     C                   SETOFF                                       21
118400990125     C                   WRITE     LS05C11
118500990125     C                   SETON                                        212023
118600990125     C                   CLEAR                   NRR11
118700990125     C**
118800040714     C**""*KBLP3         CHAIN     FNDCT02L                           31
118900040714     C**""**IN31         DOWEQ     *OFF
119000040714     C**""*DCTATB        IFEQ      ' '
119100040714     C**""*              Z-ADD     DCTAAC        V11AAC
119200040714     C**""*              Z-ADD     DCTFIL        V11FIL
119300040714     C**""*              Z-ADD     DCTNCA        V11NCA
119400040714     C**""*              Z-ADD     DCTDCA        G02INV
119500040714     C**""*              Z-ADD     *ZERO         G02DAT
119600040714     C**""*              MOVEL     '3'           G02ERR
119700040714     C**""*              CALL      'XSRDA8'
119800040714     C**""*              PARM                    WLBDAT
119900040714     C**""*              Z-ADD     G02DAT        V11DCA
120000040714     C**""*              Z-ADD     DCTFCA        V11FCA
120100040714     C**""*DCTFCA        CHAIN     FNDFA01L                           31
120200040714     C**N31              MOVEL     DFADES        V11DFA
120300040714     C**""*
120400040714     C**""*              ADD       1             NRR11             4 0
120500040714     C**""*              WRITE     LS05S11
120600040714     C**""*              ENDIF
120700040714     C**""**
120800040714     C**""*KBLP3         READE     FNDCT02L                               31
120900040714     C**""**             ENDDO
121000040714      *
121100040714      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
121200040714      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
121300040714      * legate alla spedizione che passo
121400040714     c                   clear                   fidn12ds
121500040714     c                   eval      i12aas = pa0aas
121600040714     c                   eval      i12lnp = pa0lnp
121700040714     c                   eval      i12nrs = pa0nrs
121800040714     c                   eval      i12nsp = pa0nsp
121900040714     c                   eval      i12fel = 'E'
122000040714     c                   eval      i12fan = 'E'
122100040714      *
122200040714     c                   call      'FIDN12R'
122300040714     c                   parm                    fidn12ds
122400040714      *
122500040714      * se non ci sono errori
122600040714     c                   if        o12err = ' '
122700040714      * se numero di CA trovate maggiore di zero
122800040714     c                   if        o12nca > 0
122900040714     c
123000040714     c                   z-add     *zeros        ii                2 0
123100040714     c     1             do        o12nca        ii
123200040714     c                   movea     skkey(ii)     dskey
123300040714     C                   Z-ADD     dsaac         V11AAC
123400040714     C                   Z-ADD     dsfil         V11FIL
123500040714     C                   Z-ADD     dsnca         V11NCA
123600040714     C                   Z-ADD     skdca(ii)     G02INV
123700040714     C                   Z-ADD     *ZERO         G02DAT
123800040714     C                   MOVEL     '3'           G02ERR
123900040714     C                   CALL      'XSRDA8'
124000040714     C                   PARM                    WLBDAT
124100040714     C                   Z-ADD     G02DAT        V11DCA
124200040714     C                   Z-ADD     skfca(ii)     V11FCA
124300040714     C     V11FCA        CHAIN     FNDFA01L                           31
124400040714     C  N31              MOVEL     DFADES        V11DFA
124500040714     C**
124600040714     C                   ADD       1             NRR11             4 0
124700040714     C                   WRITE     LS05S11
124800040714     C                   ENDDO
124900040714      *
125000040714     c                   endif
125100040714     c                   endif
125200040714      *
125300990125     C* EMETTO FORMATO
125400990125     C     FOR11         TAG
125500990125     C                   Z-ADD     1             NRR11
125600990125     C                   WRITE     LS05T01
125700990125     C                   WRITE     LS05Z11
125800990125     C                   EXFMT     LS05C11
125900990125     C* F12 - RITORNO
126000990125     C     *INKL         IFEQ      *OFF
126100990125     C                   READC     LS05S11                                31
126200990125     C     *IN31         DOWEQ     *OFF
126300990125     C     V11SCE        IFEQ      '5'
126400991022     C                   CLEAR                   FIDN00DS
126500990125     C                   Z-ADD     V11AAC        I00AAC
126600990125     C                   Z-ADD     V11FIL        I00FIL
126700990125     C                   Z-ADD     V11NCA        I00NCA
126800990125     C                   Z-ADD     PA0AAS        I00AAS
126900990125     C                   Z-ADD     PA0NRS        I00NRS
127000990125     C                   Z-ADD     PA0NSP        I00NSP
127100990125     C                   Z-ADD     PA0LNP        I00LNP
127200990125     C                   MOVEL     'P'           I00TPB
127300991022     C                   MOVEL     FIDN00DS      KPJBU
127400990125     C                   CALL      'FIDN01R'
127500990125     C                   PARM                    KPJBA
127600000309     C                   PARM                    CALLricors
127700990125     C**
127800990125     C                   CLEAR                   V11SCE
127900990125     C                   UPDATE    LS05S11
128000990125     C                   ENDIF
128100990125     C**
128200990125     C                   READC     LS05S11                                31
128300990125     C                   ENDDO
128400990125     C                   GOTO      FOR11
128500990125     C                   ENDIF
128600990125     C                   SETON                                        31
128700990125     C                   MOVEL     'S'           WRIEM
128800990125     C*
128900990125     C                   ENDIF
129000990125     C                   ENDSR
129100990125     C**************************************************************************
129200990125     C*    CONTROLLO SE C'E' UNA O PIU' C.A.
129300990125     C**************************************************************************
129400990125     C     CTRDAN        BEGSR
129500990125     C                   CLEAR                   CONTCA
129600040714     C**""*KBLP3         CHAIN     FNDCT02L                           31
129700040714     C**""**IN31         DOWEQ     *OFF
129800040714     C**""*DCTATB        IFEQ      ' '
129900040714     C**""**             SETON                                        12
130000040714     C**""**             ADD       1             CONTCA            3 0
130100040714     C**""**             Z-ADD     DCTAAC        VHCAAC
130200040714     C**""**             Z-ADD     DCTFIL        VHCFIL
130300040714     C**""**             Z-ADD     DCTNCA        VHCNCA
130400040714     C**""**             ENDIF
130500990125     C*
130600040714     C**""*KBLP3         READE     FNDCT02L                               31
130700040714     C**""**             ENDDO
130800040714      *
130900040714      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON
131000040714      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
131100040714      * late e legate alla spedizione che passo
131200040714     c                   clear                   fidn12ds
131300040714     c                   eval      i12aas = pa0aas
131400040714     c                   eval      i12lnp = pa0lnp
131500040714     c                   eval      i12nrs = pa0nrs
131600040714     c                   eval      i12nsp = pa0nsp
131700040714     c                   eval      i12fel = 'E'
131800040714     c                   eval      i12fan = 'E'
131900040714      *
132000040714     c                   call      'FIDN12R'
132100040714     c                   parm                    fidn12ds
132200040714      *
132300040714      * se non ci sono errori
132400040714     c                   if        o12err = ' '
132500040714      * se numero di CA trovate maggiore di zero
132600040714     c                   if        o12nca > 0
132700040714     c                   z-add     O12NCA        CONTCA            3 0
132800040714     C                   SETON                                        12
132900040714     C                   MOVEA     SKKEY(O12NCA) DSKEY
133000040714     C                   Z-ADD     DSAAC         VHCAAC
133100040714     C                   Z-ADD     DSFIL         VHCFIL
133200040714     C                   Z-ADD     DSNCA         VHCNCA
133300040714     C                   ENDIF
133400040714     C                   ENDIF
133500040714      *
133600990125     C                   ENDSR
133700940808     C**************************************************************************
133800971212     C*    CARICO RECORD DA VISUALIZZARE/MANUTENZIONARE
133900940808     C**************************************************************************
134000900511     C     CARBLP        BEGSR
134100940808     C*
134200930112     C*** PULIZIA CAMPI
134300930112     C*
134400930112     C                   MOVEL     ' '           VI2CEI
134500950628     C                   MOVEL     'P'           PA2TBO
134600940804     C                   Z-ADD     0             V3CDCP
134700930526     C*
134800920326     C                   SETOFF                                       440307
134900951011     C                   SETOFF                                       949813
135000021218     C                   SETOFF                                       0977
135100940923     C*
135200940923     C                   MOVEL     PA0AAS        V1CAAS
135300940926     C                   MOVEL     PA0LNP        V1CLNP
135400940923     C                   MOVEL     PA0NRS        V1CNRS
135500940923     C                   MOVE      PA0NSP        V1CNSP
135600940923     C*
135700900511     C* CHAIN
135800041028     c                   clear                   wesarb            1
135900940802     C     KBLP3         CHAIN     FNBLP01L                           31
136000041028     C     KBLP3         CHAIN     FNarb01L
136100041028     c                   if        %found(fnarb01l)
136200041028     c                   eval      wesarb='S'
136300041028     c                   else
136400041028     c                   eval      wesarb='N'
136500041028     c                   endif
136600940926     C*
136700940928     C* SE NON TROVATA BOLLA, PROVO NELLO STORICO
136800940928     C* E SE NON TROVATA PULISCO I CAMPI CHIAVE E ESCO
136900010713     C     *IN31         IFEQ      *ON
137000010716     C                   MOVEL     *ZERO         PA0AAS
137100010716     C                   MOVEL     *ZERO         PA0LNP
137200010716     C                   MOVEL     *ZERO         PA0NRS
137300010716     C                   MOVEL     *ZERO         PA0NSP
137400010716     C                   GOTO      ENDBLP
137500010713     C                   ENDIF
137600021218     C*
137700040220     C* 77 ON - Esistono note supermercati da interrogare
137800021218     C                   Z-ADD     PA0AAS        KAAS
137900021218     C                   Z-ADD     PA0LNP        KLNP
138000021218     C                   Z-ADD     PA0NRS        KNRS
138100021218     C                   Z-ADD     PA0NSP        KNSP
138200021218     C                   MOVEL     'SCR'         KTRD
138300021218     C     KAR51         CHAIN     FIAR5000                           31
138400021218     C                   If        *in31 = *off and Ar5Atb = ' '
138500021218     C                   seton                                        77
138600021218     C                   else
138700021218     C                   setoff                                       77
138800021218     C                   endif
138900940926     C*
139000900927     C* CONTROLLO SE SPEDIZIONE GIACENTE
139100050406     C     KBLP3         CHAIN     tIGCP01L                           31
139200940923     C*
139300901004     C  N31GCPDCG        IFEQ      0
139400910321     C                   SETON                                        4403
139500910321     C                   ELSE
139600910321     C                   SETON                                        03
139700901004     C                   END
139800111020     C**!03PA0CLI        COMP      'S'                                0303
139900900511     C*
140000950214     C                   CLEAR                   V2CFLS
140100950214     C     BLPNCD        IFNE      *ZERO
140200950214     C     BLPFNS        ORNE      *BLANKS
140300940923     C                   MOVEL     BLPFLS        V2CFLS
140400950214     C                   ENDIF
140500940923     C                   MOVEL     PA0NRS        V7CNRS
140600940808     C                   MOVEL     BLPLNP        V1CLP1
140700940805     C                   MOVEL     BLPCBO        V1CCBO
140800940809     C*
140900940809     C                   MOVEL     BLPGGA        V2CGGA
141000940923     C                   MOVEL     *BLANKS       V2IGGA
141100940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
141200940923     C     V2CGGA        COMP      *BLANKS                            1717
141300940809     C*
141400940809     C                   MOVEL     BLPGMA        V2CGMA
141500940923     C                   MOVEL     *BLANKS       V2IGMA
141600940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
141700940923     C     V2CGMA        COMP      *BLANKS                            1818
141800940809     C*
141900960208     C                   MOVEL     BLPGVA        V2CGVA
142000960208     C                   MOVE      ' '           V2CGVA
142100940923     C                   MOVEL     *BLANKS       V2IGVA
142200940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
142300940923     C     V2CGVA        COMP      *BLANKS                            1919
142400940322      *
142500911105     C* NON SEQUENZIALI
142600910912     C     BLPFNS        IFEQ      'S'
142700940808     C                   MOVEL     SEQ(1)        V2CFNS
142800910912     C                   ELSE
142900940808     C                   MOVEL     *BLANKS       V2CFNS
143000910912     C                   END
143100930112     C* CONTROLLO SE ESISTONO VARIAZIONI PER LA BOLLA
143200131007     C     KBLP3         CHAIN     FIARBF2C                           31
143300111020     C**!!!PA0CLI        IFNE      'S'
143400920429     C  N31              SETON                                        10
143500111020     C**!!!              ENDIF
143600910806     C*
143700920409     C* TIPO BOLLA CONTROLLO SE FRANCO
143800940804     C                   MOVEL     '3A'          KCOD
143900940804     C                   MOVEL     *BLANKS       KKEY
144000940805     C                   MOVEL     BLPCBO        KKEY
144100940809     C     KTAB1         CHAIN     TABEL00F                           31
144200940923     C  N31              MOVEL     TBLUNI        DS3A
144300940923     C   31              CLEAR                   DS3A
144400900516     C                   MOVEL     §3ATB1        FLGTB1            1
144500940804     C                   MOVEL     §3ADES        V1DCBO
144600940805     C                   MOVEL     §3ADES        V2DCBO
144700940804     C                   MOVEL     §3ADES        V7DCBO
144800940408     C     FLGTB1        IFEQ      'F'
144900910418     C                   SETON                                        33
145000900511     C                   END
145100900511     C* LINEA ARRIVO E DECODIFICA
145200940804     C                   MOVEL     BLPLNA        V1CLNA
145300020225     C     BLPLNA        CHAIN     AZORG01L
145400020225     c                   if        not %found(azorg01l)
145500020225     C                   MOVEL     *BLANKS       ORGDES
145600020225     c                   endif
145700940804     C                   MOVEL     ORGDES        V1DLNA
145800940804     C                   MOVEL     ORGDES        V2DLNA
145900940810     C                   MOVEL     ORGDES        V9DLNA
146000020225     c                   eval      og143  = ORGde3
146100020225     c                   eval      lnantw = §ogntw
146200971202     C* TERMINAL DI PARTENZA DELLA LINEA DI ARRIVO
146300991022     C                   CLEAR                   FNLV55DS
146400971202     C                   MOVEL     'P'           D55TPT
146500971202     C                   MOVE      BLPLNA        D55LIN
146600971202     C                   Z-ADD     WDATEU        D55DRF
146700971202     C                   CALL      'FNLV55R'
146800991022     C                   PARM                    FNLV55DS
146900971202     C                   MOVE      D55TFP        WTFP
147000940804     C* LINEA PARTENZA E DECODIFICA
147100020225     C     BLPLNP        CHAIN     AZORG01L
147200020225     c                   if        not %found(azorg01l)
147300020225     C                   MOVEL     *BLANKS       ORGDES
147400020225     c                   endif
147500940804     C                   MOVEL     ORGDES        V1DLNP
147600020225     c                   eval      og143  = ORGde3
147700020225     c                   eval      lnpntw = §ogntw
147800920410     C* MITTENTE
147900940804     C                   MOVEL     BLPRSM        V1CRSM
148000940808     C                   MOVEL     BLPRSM        V2CRSM
148100940805     C                   MOVEL     BLPRSM        V7CRSM
148200940804     C                   MOVEL     BLPINM        V1CINM
148300940808     C                   MOVEL     BLPINM        V7CINM
148400940804     C                   MOVEL     BLPPRM        V1CPRM
148500940804     C                   MOVEL     BLPCAM        V1CCAM
148600940804     C                   MOVEL     BLPLOM        V1CLOM
148700940808     C                   MOVEL     BLPLOM        V2CLOM
148800940808     C                   MOVEL     BLPLOM        V7CLOM
148900940808     C                   MOVEL     BLPPRM        V2CPRM
149000940804     C                   MOVEL     BLPNZM        V1CNZM
149100940808     C                   MOVEL     BLPFAP        V1CFAP
149200981023     C                   MOVEL     BLPCTR        V2CCTR
149300060216     C* NUMERO ORM: DA ar4 (Tipo record 'M')
149400950412     C                   MOVEL     BLPAAS        KAAS
149500950412     C                   MOVEL     BLPLNP        KLNP
149600950412     C                   MOVEL     BLPNRS        KNRS
149700950412     C                   MOVEL     BLPNSP        KNSP
149800010220     C                   MOVEL     'M'           KTRC
149900010220     C                   clear                   v1cpoe
150000010220     C                   clear                   v1cnsr
150100010220     C                   clear                   v1cnor
150200010220     C                   clear                   v1cnrv
150300010220     C                   setoff                                       55
150400060216     C     Kar41         CHAIN     Fiar401L                           99
150500010220     C     *in99         ifeq      *off
150600060216     C     ar4not        ifne      *blanks
150700060216     C                   movel     ar4not        dsbl4m
150800010220     C                   move      §b4poe        v1cpoe
150900010220     C                   move      §b4nsr        v1cnsr
151000010220     C                   move      §b4nor        v1cnor
151100010220     C                   move      §b4nrv        v1cnrv
151200040713     c     korm          Setll     Fnorm01l
151300040713     c                   If        %Equal(fnorm01l)
151400010220     C                   seton                                        55
151500040713     c                   EndIf
151600010220     C                   endif
151700010220     C                   endif
151800010220     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
151900060216     C* DA Fiar4 SE RECORD ESISTENTE
152000950412     C                   MOVEL     'P'           KTRC
152100060216     C     Kar41         CHAIN     Fiar401L                           99
152200950412     C     *IN99         IFEQ      *OFF
152300060216     C                   MOVEL     ar4NOT        V1CPIM
152400060216     C                   MOVE      ar4NOT        V2CPID
152500950412     C                   ELSE
152600950412     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI BLPCPI
152700930112     C     BLPCPI        IFNE      *BLANKS
152800940804     C   33              MOVEL     BLPCPI        V1CPIM
152900940804     C  N33              MOVEL     BLPCPI        V2CPID
153000930112     C                   ELSE
153100940808     C                   MOVEL     *BLANKS       V1CPIM
153200940808     C                   MOVEL     *BLANKS       V2CPID
153300930112     C                   END
153400950412     C                   END
153500061108      * codice fiscale mittente e destinatario da fiar4 rcd Q
153600061108     c                   clear                   v1cdfm
153700061108     c                   clear                   v2cdfd
153800061108     c                   eval      ktrc = 'Q'
153900061108     c     kar41         chain     fiar401l
154000061108     c                   if        %found(fiar401l)
154100061108     c                   movel     ar4not        v1cdfm
154200061108     c                   move      ar4not        v2cdfd
154300061108     c                   endif
154400900525     C*
154500940906     C* CERCO LA DESCRIZIONE AGGIUNTIVA PER IL DESTINATARIO
154600940808     C                   MOVEL     BLPAAS        KAAS
154700940808     C                   MOVEL     BLPLNP        KLNP
154800940808     C                   MOVEL     BLPNRS        KNRS
154900940808     C                   MOVEL     BLPNSP        KNSP
155000940906     C                   MOVEL     'D'           KTRC
155100060216     C     Kar41         CHAIN     Fiar401L                           99
155200060216     C  N99              MOVEL     ar4NOT        V2CNOT
155300950125     C   99              MOVEL     *BLANKS       V2CNOT
155400940808     C*
155500971212     C* SOLO PER LE FILIALI ESTERE IMPORT/EXPORT VISUALIZZO RIF PARTNER
155600971212     C                   CLEAR                   V1CEST
155700951011     C                   MOVEL     'E'           KTRC
155800060216     C     Kar41         CHAIN     Fiar401L                           99
155900060216     C  N99              MOVEL     ar4NOT        V2CRPP
156000980704     C  N99              SETON                                        13
156100020225     c   99              if        (lnantw = 'EEX' or lnantw = 'EUP' or
156200020225     c                              lnantw = 'FED') or
156300020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
156400020225     c                              lnpntw = 'FED')
156500980704     C                   SETON                                        13
156600980704     C                   ENDIF
156700951011     C   99              CLEAR                   V2CRPP
156800971212     C* SE E' SDI IMPORTO ANCHE TIPO INSERIMENTO E STAMPA
156900020225     c                   if        not *in99 and (lnpntw = 'EUP' or
157000020225     c                                            lnantw = 'EUP')
157100971212     C**
157200060216     C                   MOVEL     ar4NOT        DSBL4E
157300971212    3C     §B4EMA        IFEQ      'M'
157400971212     C                   MOVEL     CMANUA        W021A            21
157500971212     C                   SETON                                        09
157600971212    4C     §B4EAT        IFEQ      'P'
157700971212     C                   MOVE      'PART'        W021A
157800971212    4C                   ENDIF
157900971212    4C     §B4EAT        IFEQ      'A'
158000971212     C                   MOVE      'ARR.'        W021A
158100971212    4C                   ENDIF
158200971212     C                   MOVE      W021A         V1CEST
158300971212    3C                   ENDIF
158400971212    C**
158500971212    3C     §B4EMA        IFEQ      'A'
158600971212     C                   MOVEL     CAUTOM        W018A            18
158700971212     C                   SETON                                        09
158800971212    4C     §B4EAT        IFEQ      'P'
158900971212     C                   MOVE      'PART'        W018A
159000971212    4C                   ENDIF
159100971212    4C     §B4EAT        IFEQ      'A'
159200971212     C                   MOVE      'ARR.'        W018A
159300971212    4C                   ENDIF
159400971212     C*
159500971212     C                   MOVEL     W018A         V1CEST
159600971212     C                   MOVE      CSTAMP        V1CEST
159700971212     C**
159800971212    4C     §B4EST        IFEQ      'N'
159900971212     C                   MOVE      'NO'          V1CEST
160000971212    4C                   ELSE
160100971212     C                   MOVE      'SI'          V1CEST
160200971212    4C                   ENDIF
160300991022      *
160400971212    3C                   ENDIF
160500971212    2C                   ENDIF
160600991227     c* DPD
160700000216     c                   setoff                                       14
160800000216     c                   clear                   v2crtn
160900000216     c                   clear                   v2czon
161000020225     c                   if        lnantw = 'DPD'
161100991227     c                   seton                                        13
161200060517     C***                MOVEL     'F'           KTRC
161300060517     C                   MOVEL     'I'           KTRC
161400060216     C     Kar41         CHAIN     Fiar401L                           99
161500060517     C**N99              MOVEL     ar4NOT        dsbl4f
161600060517     c** 99              clear                   dsbl4f
161700060517     C  N99              MOVEL     ar4NOT        dsbl4I
161800060517     c   99              clear                   dsbl4I
161900060517     C***                MOVEL     §b4fnp        V2CRPP
162000060517     C                   MOVEL     §b4ipn        V2CRPP
162100060517     c****               if        §b4rtn>*zeros or §b4zon>*zeros
162200060517     c****               movel     §b4rtn        v2crtn
162300060517     c****               movel     §b4zon        v2czon
162400060517     c                   if        §b4ida>*zeros or §b4iza>*zeros
162500060517     c                   movel     §b4ida        v2crtn
162600060517     c                   movel     §b4iza        v2czon
162700000216     c                   seton                                        14
162800000216     c                   setoff                                       09
162900000216     c                   endif
163000050304     c                   else
163100050304     c* dpd import
163200050304     c                   if        lnpntw = 'DPD'
163300060517     c****               clear                   dsbl4f
163400060517     C****               MOVEL     'F'           KTRC
163500060517     c                   clear                   dsbl4i
163600060517     C                   MOVEL     'I'           KTRC
163700060216     C     Kar41         CHAIN     Fiar401L
163800060216     c                   if        %found(fiar401l)
163900050304     c                   seton                                        13
164000060517     C***                MOVEL     ar4NOT        dsbl4f
164100060517     C                   MOVEL     ar4NOT        dsbl4I
164200060517     C***                MOVEL     §b4fnp        V2CRPP
164300060517     C                   MOVEL     §b4ipn        V2CRPP
164400050304     c                   endif
164500050304     c                   endif
164600991227     c                   endif
164700981023     C***
164800981023     C* ASSEGNATO
164900981023     C     *IN33         IFEQ      *OFF
165000981023     C** MITTENTE
165100981023     C     BLPCCM        IFNE      0
165200981023     C                   MOVEL     BLPCCM        V2CCDM
165300981023     C                   MOVEL     BLPCCM        V1CKSC
165400981023     C                   ELSE
165500981023     C                   CLEAR                   V2CCDM
165600981023     C                   CLEAR                   V1CKSC
165700981023     C                   ENDIF
165800981023     C* DESTINATARIO
165900981023     C                   MOVEL     BLPKSC        V2CCDD
166000981023     C                   MOVEL     BLPKSC        V1CCDD
166100991116     c                   eval      KTRC = '1'
166200991116     C     KAR6          CHAIN     FIAR6000                           99
166300991116     C                   IF        *in99 = *OFF and ar6ksc > *zeros
166400991116     C                   MOVEL     ar6ksc        V2CCDD
166500991116     C                   MOVEL     ar6ksc        V1CCDD
166600991116     C                   MOVEL     ar6ctr        V2CCTR
166700991116     c                   ENDIF
166800981023     C                   ELSE
166900981023     C* FRANCO
167000981023     C                   MOVEL     BLPKSC        V1CKSC
167100981023     C                   MOVEL     BLPKSC        V2CCDM
167200981023     C                   CLEAR                   V2CCDD
167300981023     C                   CLEAR                   V1CCDD
167400981023     C                   ENDIF
167500050117     C                   MOVEL     BLPXCO        V2CXCO
167600940808     C*
167700940808     C* SE C'E' IL COD. MITTENTE, RILEVO IL NR.TELEFONICO
167800940809     C                   MOVE      V2CCDM        WQUATT            4 0
167900940809     C     WQUATT        IFNE      8888
168000940809     C     V2CCDM        ANDNE     *ZERO
168100981123     C                   MOVEL     KCI           KKCC
168200940808     C                   MOVEL     V2CCDM        KKSC
168300940808     C     KIND1         CHAIN     CNIND00F                           99
168400940808     C  N99              MOVEL     INDTEL        V2CTEL
168500940808     C   99              MOVEL     *BLANKS       V2CTEL
168600940809     C                   ENDIF
168700940808     C*
168800940808     C                   MOVEL     BLPRSD        V2CRSD
168900940808     C                   MOVEL     BLPRSD        V7CRSD
169000940810     C                   MOVEL     BLPRSD        V9CRSD
169100940808     C                   MOVEL     BLPIND        V2CIND
169200940808     C                   MOVEL     BLPIND        V7CIND
169300940808     C                   MOVEL     BLPPRD        V2CPRD
169400940808     C                   MOVEL     BLPCAD        V2CCAD
169500940808     C                   MOVEL     BLPLOD        V2CLOD
169600940808     C                   MOVEL     BLPLOD        V7CLOD
169700940808     C                   MOVEL     BLPNZD        V2CNZD
169800940804     C                   MOVEL     BLPFIN        V2CFIN
169900940804     C                   MOVEL     BLPGC1        V2CGC1
170000940804     C                   MOVEL     BLPGC2        V2CGC2
170100961111     C                   MOVEL     *BLANKS       V2CFBR
170200961111     C                   MOVEL     *BLANKS       V2CFFR
170300961111     C                   SETOFF                                       0102
170400940804     C     BLPFBR        IFEQ      'S'
170500940804     C                   MOVEL     'SI'          V2CFBR
170600961111     C                   SETON                                        01
170700940804     C                   ENDIF
170800961111     C     BLPFBR        IFEQ      'F'
170900961111     C                   MOVEL     'FR'          V2CFFR
171000961111     C                   SETON                                        02
171100961111     C                   ENDIF
171200970227     C     BLPFST        IFEQ      'S'
171300970131     C     BLPFST        OREQ      'C'
171400970227     C                   MOVEL     'SI DDT'      V1DBAM
171500970227     C                   SETON                                        08
171600970130     C                   ELSE
171700970227     C                   MOVEL     'NO DDT'      V1DBAM
171800970227     C                   SETOFF                                       08
171900970130     C                   ENDIF
172000970227     C*
172100970227     C                   SELECT
172200970227     C     BLPFST        WHENEQ    'C'
172300970227     C     BLPFST        OREQ      'K'
172400970227     C                   MOVEL     'CLIENTE '    V1DFST
172500970227     C     BLPFST        WHENEQ    'S'
172600970227     C     BLPFST        OREQ      'Y'
172700970227     C                   MOVEL     'PARTENZA'    V1DFST
172800970227     C     BLPFST        WHENEQ    'J'
172900970227     C                   MOVEL     'ARRIVO  '    V1DFST
173000970227     C                   OTHER
173100970227     C                   MOVEL     'NO      '    V1DFST
173200970227     C                   ENDSL
173300970227     C*
173400941219     C     BLPFDN        IFEQ      'S'
173500941219     C                   MOVEL     'SI'          V1CFDN
173600941219     C                   ELSE
173700941219     C                   MOVEL     *BLANKS       V1CFDN
173800941219     C                   ENDIF
173900940808     C                   MOVEL     BLPFTM        V1CFTM
174000940804     C                   MOVEL     *BLANKS       V1DTSP
174100940804     C                   MOVEL     '5E'          KCOD
174200940804     C                   MOVEL     *BLANKS       KKEY
174300940808     C                   MOVEL     BLPTSP        KKEY
174400940809     C     KTAB1         CHAIN     TABEL00F                           99
174500090608     C  N99              MOVEL     TBLUNI        ds5e
174600090608     C  N99              MOVEL     §5ed08        V1DTSP
174700941021     C*
174800940805     C                   MOVEL     BLPCTS        V8CCTS
174900940804     C                   MOVEL     BLPQFT        V2CQFT
175000940804     C                   MOVEL     BLPVMD        V2CVMD
175100940804     C                   MOVEL     BLPVAD        V2CVAD
175200940804     C                   MOVEL     BLPIAS        V2CIAS
175300940804     C                   MOVEL     BLPVAS        V2CVAS
175400940808     C                   MOVEL     BLPNAS        V1CNAS
175500940804     C                   MOVEL     BLPNCL        V1CNCL
175600941026     C                   MOVEL     BLPPKF        V1CPKF
175700990909     C                   MOVEL     BLPPKB        V1CPKB
175800940808     C                   MOVEL     BLPVLB        V1CVLB
175900940808     C                   MOVEL     BLPFVB        V1CFVB
176000940808     C                   MOVEL     BLPVLF        V1CVLF
176100940808     C                   MOVEL     BLPFVF        V1CFVF
176200940808     C* SE BLPNCR = 99999  A VIDEO AZZERO NCR E VLC
176300930505     C     BLPNCR        IFEQ      99999
176400940805     C                   MOVEL     *ZEROS        BLPNCR
176500940805     C                   MOVEL     *ZEROS        BLPVLC
176600940808     C                   ENDIF
176700940808     C                   MOVEL     BLPVLC        V1CVLC
176800940808     C                   MOVEL     BLPNCR        V1CNCR
176900990909     C* SE BLPNCP = 99999  A VIDEO AZZERO NCP E PKC
177000990909     C     BLPNCP        IFEQ      99999
177100990909     C                   MOVEL     *ZEROS        BLPNCP
177200990909     C                   MOVEL     *ZEROS        BLPPKC
177300990909     C                   ENDIF
177400990909     C                   MOVEL     BLPPKC        V1CPKC
177500990909     C                   MOVEL     BLPNCP        V1CNCP
177600031119
177700031119      * Referente e Telefono del destinatario
177800031119     c                   Clear                   V2cRef
177900031119     c                   Clear                   V2cTeld
178000110610     c* profilo utente e terminale che ha immesso la bolla
178100110610     c                   Clear                   Vidpru
178200110610     c                   Clear                   Vidnmtd
178300110610     c                   setoff                                       46
178400031119      * li prendo da Fiar500f record 'GEN'
178500031119     c                   Clear                   dAr5Gen
178600091125     c                   clear                   w_cdi
178700031119     c                   Z-Add     BlpAas        Kaas
178800031119     c                   Z-Add     BlpLNp        Klnp
178900031119     c                   Z-Add     BlpNrs        Knrs
179000031119     c                   Z-Add     BlpNsp        Knsp
179100031119     c                   Eval      Ktrd = 'GEN'
179200031119     c     kAr51         Chain     Fiar501l
179300031119If  1c                   If        %Found(Fiar501l)
179400031119     c                   Movel     Ar5Uni        Dar5Gen
179500031119     c                   Eval      V2cRef = §Ar5Ref
179600031119     c                   Eval      V2cTeld = §Ar5Teld
179700091125     c* memorizzo eventuale codice beneficiario assegno
179800091125     c                   eval      w_cdi=§ar5cdi
179900110610      *
180000110610     c                   Eval      Vidpru = §Ar5bpru
180100110610     c                   Eval      Vidnmtd = §Ar5bnmtd
180200110610     c                   if        vidpru<>*blanks
180300110610     c                   seton                                        46
180400110610     c                   endif
180500031119    1c                   EndIf
180600130319     c* Vedo se ha il packing list
180700130320     c                   if        §ar5alS='S'
180800130320     c                   eval      v1dar8='PACKING LIST PDF'
180900130319     C                   ELSE
181000130320     c                   if        §ar5alx='S'
181100130320     c                   eval      v1dar8='PACKING LIST    '
181200130319     c                   else
181300130319     C                   CLEAR                   V1DAR8
181400130319     c                   endif
181500130319     C                   ENDIF
181600110531
181700110531     c* email destinatario
181800110601     c                   setoff                                       45
181900110531     c                   clear                   v2cemW
182000130927     c                   clear                   dar5emd
182100110531      * lo prendo da Fiar500f record 'EMD'
182200110531     c                   Z-Add     BlpAas        Kaas
182300110531     c                   Z-Add     BlpLNp        Klnp
182400110531     c                   Z-Add     BlpNrs        Knrs
182500110531     c                   Z-Add     BlpNsp        Knsp
182600110531     c                   Eval      Ktrd = 'EMD'
182700130927     c*    kAr51         setll     Fiar501l
182800130927     c     kAr51         chain     Fiar501l
182900130927If  1c                   If        %found(Fiar501l)
183000130927     c                   eval      dar5emd=ar5uni
183100130927     c                   if        §ar5tel<>*blanks or §ar5eml<>*blanks
183200110610     c                   eval      v2cemd='SI'
183300110601     c                   seton                                        45
183400130927     c                   endif
183500110531     c                   endif
183600040301
183700040301      * Se colli di valore visualizzo "V" nell'apposito campo
183800040301      *  con lo specifico attributo di visualizzazione
183900040301if  1c                   if        §AR5bva = *blanks
184000040301     c                   eval      V2Cbva  = *blanks
184100040301x   1c                   else
184200040301     c                   eval      V2Cbva  = 'V'
184300040301e   1c                   endif
184400031119
184500030121      * Bancali
184600030121     c                   Clear                   V1dTba
184700030121     c                   Clear                   V1dTb2
184800030122     c                   Clear                   V1cNba
184900030122     c                   Clear                   V1cNb2
185000030703      * Record 'BAN'
185100030703If  0c                   If        %subst(BlpGva:1:1) = 'B'
185200030121     c                   Clear                   dAr5Ban
185300030121     c                   Eval      Ktrd = 'BAN'
185400030121     c     kAr51         Chain     Fiar501l
185500030121If  1c                   If        %Found(Fiar501l)
185600030121     c                   Movel     Ar5Uni        Dar5Ban
185700030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
185800030122      * per le note relative al mancato reso dei bancali
185900030122     c                   If        Not *In77 and
186000030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
186100030122     c                   Eval      *In77 = *On
186200030122     c                   EndIf
186300030121     c                   Eval      V1cNba = §Ar5Nba
186400030121     c                   Eval      V1cNb2 = §Ar5Nb2
186500030121If  2c                   If        §Ar5Tba <> *Zeros
186600030703     c                   Eval      wtba = §Ar5Tba
186700030703     c                   ExSr      Sr_DecTba
186800030703     c                   Eval      V1dTba = wdestba
186900030121    2c                   EndIf
187000030121If  2c                   If        §Ar5Tb2 <> *Zeros
187100030703     c                   Eval      wtba = §Ar5Tb2
187200030703     c                   ExSr      Sr_DecTba
187300030703     c                   Eval      V1dTb2 = wdestba
187400030121    2c                   EndIf
187500030121    1c                   EndIf
187600030703    0c                   EndIf
187700030703      * Record 'BNB'  -  Spedizione OVER 25 colli
187800030703If  0c                   If        %subst(BlpGva:1:1) = 'O'
187900030703     c                   Clear                   dAr5Bnb
188000030703     c                   Eval      Ktrd = 'BNB'
188100030703     c     kAr51         Chain     Fiar501l
188200030703If  1c                   If        %Found(Fiar501l)
188300030703     c                   Movel     Ar5Uni        Dar5Bnb
188400030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
188500030703      * per i colli originali in caso di spedizione over 25 colli
188600030703     c  n77              Eval      *In77 = *On
188700030703     c                   Eval      V1cNba = §Ar5bNba
188800030703     c                   Eval      V1cNb2 = §Ar5bNb2
188900030703If  2c                   If        §Ar5bTba <> *Zeros
189000030703     c                   Eval      wtba = §Ar5bTba
189100030703     c                   ExSr      Sr_DecTba
189200030703     c                   Eval      V1dTba = wdestba
189300030703    2c                   EndIf
189400030703If  2c                   If        §Ar5bTb2 <> *Zeros
189500030703     c                   Eval      wtba = §Ar5bTb2
189600030703     c                   ExSr      Sr_DecTba
189700030703     c                   Eval      V1dTb2 = wdestba
189800030703    2c                   EndIf
189900030703    1c                   EndIf
190000030703    0c                   EndIf
190100920409     C*
190200920409     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
190300940804     C                   MOVEL     'TR'          KCOD
190400940804     C                   MOVEL     *BLANKS       KKEY
190500940808     C                   MOVEL     V2CCTR        UNOCTR            1
190600940804     C                   MOVEL     UNOCTR        KKEY
190700940809     C     KTAB1         CHAIN     TABEL00F                           31
190800940804     C                   MOVEL     TBLUNI        V2DCTR
190900910730     C*
191000940808     C* VISUALIZZO V2CCDM SOLO SE LA LNP DEL CODICE = LNP DELLA BOLLA
191100930505     C                   MOVEL     BLPCCM        NUM3              3 0
191200930505     C     BLPLNP        IFEQ      NUM3
191300940808     C                   MOVE      BLPCCM        V2CCDM
191400930505     C                   END
191500930505     C*
191600940804     C                   MOVEL     BLPRMN        V2CRMN
191700940804     C                   MOVEL     BLPRMA        V2CRMA
191800940804     C                   MOVEL     BLPRMO        V2CRMO
191900940804     C                   MOVEL     BLPNCD        V2CNCD
192000940804     C                   MOVEL     BLPNCA        V2CNCA
192100940804     C                   MOVEL     BLPZNC        V2CZNC
192200940804     C                   MOVEL     BLPCTM        V1CCTM
192300130319     C***                MOVEL     '1B'          KCOD
192400130319     C***                MOVEL(P)  blpctm        KKEY
192500130319     C***  KTAB1         CHAIN     TABEL00F                           31
192600130319     C**N31              MOVEL     TBLUNI        DS1B
192700130319     c** 31              clear                   ds1b
192800051128     c* Vedo se ha il packing list
192900130319     c***                if        §1bf16='S'
193000130319     c***                eval      v1dar8='PACKING LIST'
193100130319     C***                ELSE
193200130319     C***                CLEAR                   V1DAR8
193300130319     C***                ENDIF
193400051104     c
193500051104     c                   select
193600051104     c                   when      blpffd='S'
193700051104     C                   MOVEL     'SI '         V2CFFD
193800051104     c                   when      blpffd='M'
193900051104     C                   MOVEL     'SAB'         V2CFFD
194000051104     C                   other
194100940905     C                   MOVEL     *BLANKS       V2CFFD
194200051104     C                   ENDsl
194300051104     c
194400940808     C                   MOVEL     BLPNFV        V1CNFV
194500940906     C                   MOVEL     BLPNRT        V1CNRT
194600940906     C                   MOVEL     *BLANKS       V1DFPP
194700940906     C     BLPFPP        IFEQ      'M'
194800010220     C                   MOVEL     'MAT'         V1DFPP
194900940906     C                   ELSE
195000010220     C                   MOVEL     'POM'         V1DFPP
195100940906     C                   ENDIF
195200910118     C* GIRO DATA SPEDIZIONE
195300940804     C                   MOVEL     BLPAAS        V1CDSP
195400940804     C                   MOVE      BLPMGS        V1CDSP
195500940804     C                   Z-ADD     V1CDSP        G02INV
195600940804     C                   Z-ADD     *ZEROS        G02DAT
195700940804     C                   MOVEL     '3'           G02ERR
195800940804     C                   CALL      'XSRDA8'
195900940804     C                   PARM                    WLBDAT
196000940804     C                   Z-ADD     G02DAT        V1CDSP
196100940808     C* DATA RITIRO MERCE
196200940804     C                   Z-ADD     BLPDRT        G02INV
196300940804     C                   Z-ADD     *ZEROS        G02DAT
196400940804     C                   MOVEL     '3'           G02ERR
196500940804     C                   CALL      'XSRDA8'
196600940804     C                   PARM                    WLBDAT
196700940804     C                   Z-ADD     G02DAT        V1CDRT
196800900518     C* DATA BORDERO'
196900940804     C                   Z-ADD     BLPDBR        G02INV
197000940804     C                   Z-ADD     *ZEROS        G02DAT
197100940804     C                   MOVEL     '3'           G02ERR
197200940804     C                   CALL      'XSRDA8'
197300940804     C                   PARM                    WLBDAT
197400940804     C                   Z-ADD     G02DAT        V1CDBR
197500940804     C* DATA PARTENZA PRIMO COLLO
197600940804     C                   Z-ADD     BLPDPC        G02INV
197700940804     C                   Z-ADD     *ZEROS        G02DAT
197800940804     C                   MOVEL     '3'           G02ERR
197900940804     C                   CALL      'XSRDA8'
198000940804     C                   PARM                    WLBDAT
198100940804     C                   Z-ADD     G02DAT        V1CDPC
198200940804     C* DATA PARTENZA ULTIMO COLLO
198300940804     C                   Z-ADD     BLPDUC        G02INV
198400940804     C                   Z-ADD     *ZEROS        G02DAT
198500940804     C                   MOVEL     '3'           G02ERR
198600940804     C                   CALL      'XSRDA8'
198700940804     C                   PARM                    WLBDAT
198800940804     C                   Z-ADD     G02DAT        V1CDUC
198900910806     C* DATA SPUNTA ENTRATA
199000940808     C     BLPDSE        IFGT      *ZERO
199100940808     C                   Z-ADD     BLPDSE        G02INV
199200940808     C                   Z-ADD     *ZEROS        G02DAT
199300940808     C                   MOVEL     '3'           G02ERR
199400940808     C                   CALL      'XSRDA8'
199500940808     C                   PARM                    WLBDAT
199600940808     C                   Z-ADD     G02DAT        V1CDSE
199700940808     C                   ENDIF
199800940805     C* CONSEGNA PARTICOLARE 1
199900940805     C                   MOVEL     BLPTC1        V3CTC1
200000940805     C                   MOVEL     *BLANKS       V3DTC1
200100940805     C     V3CTC1        IFNE      *BLANKS
200200940804     C                   MOVEL     '1P'          KCOD
200300940804     C                   MOVEL     *BLANKS       KKEY
200400940805     C                   MOVEL     BLPTC1        KKEY
200500940809     C     KTAB1         CHAIN     TABEL00F                           99
200600940805     C  N99              MOVEL     TBLUNI        V3DTC1
200700940805     C                   ENDIF
200800940805     C* CONSEGNA PARTICOLARE 2
200900940805     C                   MOVE      BLPTC2        V3CTC2
201000940805     C                   MOVEL     *BLANKS       V3DTC2
201100940805     C     V3CTC2        IFNE      *BLANKS
201200940805     C                   MOVEL     '1P'          KCOD
201300940805     C                   MOVEL     *BLANKS       KKEY
201400940805     C                   MOVEL     BLPTC2        KKEY
201500940809     C     KTAB1         CHAIN     TABEL00F                           99
201600940805     C  N99              MOVEL     TBLUNI        V3DTC2
201700940805     C                   ENDIF
201800910118     C* CODICE PADRONCINO
201900940808     C                   MOVEL     BLPPDR        V1CPDR
202000021203     C                   movel     BlpPdr        kpdpdr
202100021203     C                   move      'A'           kpdtip
202200021203     C     kfapd         chain     fiapd01L                           31
202300940808     C  N31              MOVEL     APDRSC        V1DPDR
202400940808     C   31              MOVEL     *BLANKS       V1DPDR
202500910118     C*
202600900511     C                   MOVEL     BLPSOP        VIDSOP
202700930614     C*
202800051124     c* Decodifico consegna richiesta
202900940808     C                   MOVEL     *BLANKS       V2CTCR
203000051124     c                   clear                   v2chcr
203100051124     c                   clear                   v2cdcr
203200051124     c                   if        blpdcr>0 or blphcr>0
203300051124     c                   clear                   fnlv80ds
203400051124     c                   eval      ilv80aas=blpaas
203500051124     c                   eval      ilv80lnp=blplnp
203600051124     c                   eval      ilv80nrs=blpnrs
203700051124     c                   eval      ilv80nsp=blpnsp
203800051124     c                   eval      ilv80mgs=blpmgs
203900051124     c                   eval      ilv80tc1=blptc1
204000051124     c                   eval      ilv80tc2=blptc2
204100051124     c                   eval      ilv80dcr=blpdcr
204200051124     c                   eval      ilv80hcr=blphcr
204300051124     c                   eval      ilv80tcr=blptcr
204400051124     c                   call      'FNLV80R'
204500051124     c                   parm                    kpjba
204600051124     c                   parm                    fnlv80ds
204700051124     c
204800051124     c                   eval      v2cdcr=olv80dcr8
204900051124     c                   eval      v2ctcr=olv80tcrd
205000051124     C                   MOVEL     blpHCR        V2cHCR
205100051124     c                   endif
205200940906     C*-------------------------------------------------------------------------
205300940906     C*
205400940906     C* L E G A M E   B O L L A
205500940906     C                   EXSR      LEGBOL
205600940808     C*-------------------------------------------------------------------------
205700050714     c* Dei seguenti dati,
205800050714     c* Prevalgono quelli di ARB se c'e' e se non presenti legami bolla
205900050714     C* DATA ENTRATA AL TRANSITO
206000050714     c                   if        wesarb='S' and not *in07
206100050714     c                   eval      wdet=arbdet
206200050714     c                   eval      wdut=arbdut
206300050714     c                   else
206400050714     c                   eval      wdet=blpdet
206500050714     c                   eval      wdut=blpdut
206600050714     c                   endif
206700050714     c
206800050714     C     wdet          IFGT      *ZERO
206900050714     C                   Z-ADD     wdet          G02INV
207000050714     C                   Z-ADD     *ZEROS        G02DAT
207100050714     C                   MOVEL     '3'           G02ERR
207200050714     C                   CALL      'XSRDA8'
207300050714     C                   PARM                    WLBDAT
207400050714     C                   Z-ADD     G02DAT        V1CDET
207500050714     C                   ENDIF
207600050714     C* DATA USCITA DAL TRANSITO
207700050714     C     wdut          IFGT      *ZERO
207800050714     C                   Z-ADD     wdut          G02INV
207900050714     C                   Z-ADD     *ZEROS        G02DAT
208000050714     C                   MOVEL     '3'           G02ERR
208100050714     C                   CALL      'XSRDA8'
208200050714     C                   PARM                    WLBDAT
208300050714     C                   Z-ADD     G02DAT        V1CDUT
208400050714     C                   ENDIF
208500050714     c* Mancata consegna
208600050714     c                   if        wesarb='S' and not *in07
208700050714     c                   eval      wtmc=arbtmc
208800050714     c                   else
208900050714     c                   eval      wtmc=blptmc
209000050714     c                   endif
209100050714     c
209200050714     C                   MOVEL     wtmc          V3CTMC
209300050714     C                   MOVEL     *BLANKS       V3DTMC
209400050714     C     wtmc          IFNE      ' '
209500050714     C                   MOVEL     '2A'          KCOD
209600050714     C                   MOVEL     *BLANKS       KKEY
209700050714     C                   MOVEL     wtmc          KKEY
209800050714     C     KTAB1         CHAIN     TABEL00F                           99
209900050714     C  N99              MOVEL     TBLUNI        V3DTMC
210000050714     C                   ENDIF
210100050714     c* Numero e data distinta consegna
210200050714     c                   if        wesarb='S'  and not *in07
210300050714     c                   eval      wndc=arbndc
210400050714     c                   eval      wddc=arbddc
210500050714     c                   else
210600050714     c                   eval      wndc=blpndc
210700050714     c                   eval      wddc=blpddc
210800050714     c                   endif
210900050714     C*
211000050714     C                   MOVEL     wndc          V3CNDC
211100050714     C                   Z-ADD     wddc          G02INV
211200050714     C                   Z-ADD     *ZEROS        G02DAT
211300050714     C                   MOVEL     '3'           G02ERR
211400050714     C                   CALL      'XSRDA8'
211500050714     C                   PARM                    WLBDAT
211600050714     C                   Z-ADD     G02DAT        V3CDDC
211700050714     C* DATA ARRIVO MERCE ALL FILIALE DI ARRIVO
211800050714     c                   clear                   wdam
211900050714     c                   if        wesarb='S' and not *in07
212000050714     c                   eval      wdam=arbdam
212100050714     c                   else
212200050714     c                   eval      wdam=blpdam
212300050714     c                   endif
212400050714     c
212500050714     C     wdam          IFGT      *ZERO
212600050714     C                   Z-ADD     wdam          G02INV
212700050714     C                   Z-ADD     *ZEROS        G02DAT
212800050714     C                   MOVEL     '3'           G02ERR
212900050714     C                   CALL      'XSRDA8'
213000050714     C                   PARM                    WLBDAT
213100050714     C                   Z-ADD     G02DAT        V1CDAM
213200050714     C                   ENDIF
213300050714     c
213400050714     C* DATA CONSEGNA MERCE REALE
213500050714     C                   Z-ADD     *ZERO         V1CDCM
213600050714     c                   if        wesarb='S' and not *in07
213700050714     c                   eval      wdcm=arbdcm
213800050714     c                   eval      wdce=arbdce
213900050714     c                   eval      wnrc=arbnrc
214000050714     c                   eval      wdcp=arbdcp
214100050714     c                   eval      wcca=arbcca
214200050714     c                   eval      whmc=arbhmc
214300050714     c                   else
214400050714     c                   eval      wdcm=blpdcm
214500050714     c                   eval      wdce=blpdce
214600050714     c                   eval      wnrc=blpnrc
214700050714     c                   eval      wdcp=blpdcp
214800050714     c                   eval      wcca=blpcca
214900050714     c                   eval      whmc=blphmc
215000050714     c                   endif
215100050714     c
215200050714     C     wdcm          IFGT      *ZERO
215300050714     C                   SETON                                        98
215400050714     C                   Z-ADD     wdcm          G02INV
215500050714     C                   Z-ADD     *ZEROS        G02DAT
215600050714     C                   MOVEL     '3'           G02ERR
215700050714     C                   CALL      'XSRDA8'
215800050714     C                   PARM                    WLBDAT
215900050714     C                   Z-ADD     G02DAT        V1CDCM
216000050714     C                   MOVEL     whmc          V1CHMC
216100050714     C                   ENDIF
216200050714     C* DATA CONSEGNA PREVISTA ESTERNA E ORE DI RITARDO
216300050714     C                   Z-ADD     0             V1CDCE
216400050714     C                   Z-ADD     0             V1CNRC
216500050714     C     wdce          IFEQ      0
216600050714     C                   SETON                                        94
216700050714     C                   ELSE
216800050714     C                   Z-ADD     wdce          G02INV
216900050714     C                   Z-ADD     *ZEROS        G02DAT
217000050714     C                   MOVEL     '3'           G02ERR
217100050714     C                   CALL      'XSRDA8'
217200050714     C                   PARM                    WLBDAT
217300050714     C                   Z-ADD     G02DAT        V1CDCE
217400050714     C*
217500050714     C                   Z-ADD     wnrc          V1CNRC
217600050714     C                   END
217700050714     C*
217800050714     C* DATA PRIMA CONSEGNA PARZIALE
217900050714     C     wdcp          IFGT      0
218000050714     C                   Z-ADD     wdcp          G02INV
218100050714     C                   Z-ADD     *ZEROS        G02DAT
218200050714     C                   MOVEL     '3'           G02ERR
218300050714     C                   CALL      'XSRDA8'
218400050714     C                   PARM                    WLBDAT
218500050714     C                   Z-ADD     G02DAT        V3CDCP
218600050714     C                   END
218700050714     C* CODICE CONSEGNA ANOMALA
218800050714     C                   MOVEL     wcca          V3CCCA
218900050714     C                   MOVE      *BLANKS       V3DCCA
219000050714     C                   MOVE      *BLANKS       V7DCCA
219100050714     C     wcca          IFNE      ' '
219200050714     C                   MOVEL     '7O'          KCOD
219300050714     C                   MOVEL     *BLANKS       KKEY
219400050714     C                   MOVEL     wcca          KKEY
219500050714     C     KTAB1         CHAIN     TABEL00F                           99
219600050714     C  N99              MOVEL     TBLUNI        V3DCCA
219700050714     C  N99              MOVEL     TBLUNI        V7DCCA
219800050714     C                   END
219900050714     c*******
220000991022      *   SE ESISTE AGGANCIO  2 °   B O L L A
220100991022      *
220200920409     C     §3ATB2        IFNE      '  '
220300991022      *
220400991022     c                   eval      KTRC = '2'
220500991022     C     KAR6          CHAIN     FIAR6000
220600991022      *
220700991022     C                   IF        %FOUND
220800991022     C                   eval        *IN06 = *ON
220900991022     C                   eval      VI2fiv = AR6fiv
221000991022     C                   eval      VI2nft = AR6nft
221100991022     C                   eval      VI2cei = AR6cei
221200991022      *
221300991022     C                   eval      VI2DIV = AR6div
221400991022     C                   eval      WDIV = AR6div
221500991022     C                   exsr      CTR_VALUTA
221600991022      *
221700991027     c                   if        AR6fim = 'P'
221800991027     c                   eval      VI2fim = 'PARTENZA'
221900040116     c                   eval      VI2fi2 = 'Immesse in'
222000991022     c                   endif
222100991022      * Varia G
222200040116     c**!!!              clear                   W0133
222300040116     c**!!!              SELECT
222400040116     c**!!!              WHEN        AR6sv1 = §$2vrg
222500040116     C**!!!              eval      VI2sv1 = AR6sv1
222600040116     c**!!!              z-add     AR6va1        W0133
222700040116     c**!!!              WHEN        AR6sv2 = §$2vrg
222800040116     C**!!!              eval      VI2sv1 = AR6sv2
222900040116     c**!!!              z-add     AR6va2        W0133
223000040116     c**!!!              WHEN        AR6sv3 = §$2vrg
223100040116     C**!!!              eval      VI2sv1 = AR6sv3
223200040116     c**!!!              z-add     AR6va3        W0133
223300040116     c**!!!              ENDSL
223400991022      *
223500040116     c**!!!              if        W0133 > *zeros
223600040116     c**!!!              eval      Wtstdec = §cvfdc
223700040116     c**!!!              exsr      EDTimporti
223800040116     c**!!!              movel     IMPeditato    VI2va1
223900040116     c**!!!              endif
224000991022      * IVA
224100040116     c**!!!              clear                   W0133
224200040116     c**!!!              SELECT
224300040116     c**!!!              WHEN        AR6sv1 = §$2iva
224400040116     c**!!!              z-add     AR6va1        W0133
224500040116     c**!!!              WHEN        AR6sv2 = §$2iva
224600040116     c**!!!              z-add     AR6va2        W0133
224700040116     c**!!!              WHEN        AR6sv3 = §$2iva
224800040116     c**!!!              z-add     AR6va3        W0133
224900040116     c**!!!              ENDSL
225000991022      *
225100040116     c**!!!              IF        W0133 > *zeros
225200040116     c**!!!              eval      *in11 = *off
225300040116     c**!!!              eval      Wtstdec = §cvfdc
225400040116     c**!!!              exsr      EDTimporti
225500040116     c**!!!              movel     IMPeditato    VI2iva
225600040116     c**!!!              ELSE
225700991123      * BOLLO se non ho iva
225800040116     c**!!!              SELECT
225900040116     c**!!!              WHEN        AR6sv1 = §$2bol
226000040116     c**!!!              z-add     AR6va1        W0133
226100040116     c**!!!              WHEN        AR6sv2 = §$2bol
226200040116     c**!!!              z-add     AR6va2        W0133
226300040116     c**!!!              WHEN        AR6sv3 = §$2bol
226400040116     c**!!!              z-add     AR6va3        W0133
226500040116     c**!!!              ENDSL
226600991123      *
226700040116     c**!!!              IF        W0133 > *zeros
226800040116     c**!!!              eval      *in11 = *on
226900040116     c**!!!              eval      Wtstdec = §cvfdc
227000040116     c**!!!              exsr      EDTimporti
227100040116     c**!!!              movel     IMPeditato    VI2iva
227200040116     c**!!!              ENDIF
227300991123      *
227400040116     c**!!!              ENDIF
227500040116
227600040116      * Carico le varie
227700040116     c                   Movel     Ar6Sv1        wsvn
227800040116     c                   Z-add     Ar6Va1        wvan
227900040116     c                   ExSr      Car_Varie1
228000040116
228100040116     c                   Movel     Ar6Sv2        wsvn
228200040116     c                   Z-add     Ar6Va2        wvan
228300040116     c                   ExSr      Car_Varie1
228400040116
228500040116     c                   Movel     Ar6Sv3        wsvn
228600040116     c                   Z-add     Ar6Va3        wvan
228700040116     c                   ExSr      Car_Varie1
228800040116
228900040116     c     Kar6          Setll     Fiar7000
229000040116     c                   Do        *Hival
229100040116     c     Kar6          Reade     Fiar7000
229200040116     c                   If        %Eof(Fiar701l)
229300040116     c                   Leave
229400040116     c                   EndIf
229500040116     c                   Movel     Ar7Svn        wsvn
229600040116     c                   Z-add     Ar7Van        wvan
229700040116     c                   ExSr      Car_Varie1
229800040116     c                   EndDo
229900040116
230000991025      * TOTALE FATTURA
230100991025     c                   clear                   W0133
230200991025     c                   IF          AR6ift > *zeros
230300991025     c                   z-add     AR6ift        W0133
230400991025     c                   eval      Wtstdec = §cvfdc
230500991025     c                   exsr      EDTimporti
230600991025     c                   movel     IMPeditato    VI2ift
230700991025     c                   endif
230800991022      *
230900991022     c                   If         AR6dft > *zeros
231000991022     C     *iso          movel     AR6DFT        DATA_EUR
231100991022     C     *eur          movel     data_eur      W0080
231200991022     C                   eval      VI2dft = %EDITC(W0080:'Y')
231300991022     c                   Endif
231400991022      *  decodifica esenzione IVA
231500991022     C                   if        VI2CEI <> *blanks
231600940808     C                   MOVEL     'EI'          KCOD
231700991022     C                   MOVEL(P)  VI2CEI        KKEY
231800940809     C     KTAB1         CHAIN     TABEL00F                           31
231900991022     C  N31              MOVEL     TBLUNI        VI2DCE
232000940808     C                   ENDIF
232100991022      *
232200991022     c                   eval      VI2ksc = AR6ksc
232300991022     c                   eval      VI2dks = BLPrsd
232400991022     c                   eval      VI2ctr = AR6ctr
232500991022      *
232600991022     C                   eval      KCOD = 'TR'
232700991022     C                   movel     VI2CTR        UNOCTR
232800991022     C                   eval      KKEY = unoctr
232900991022     C     KTAB1         CHAIN     TABEL00F                           31
233000991022     C                   movel     TBLUNI        VI2dct
233100991022      *
233200991022     C                   ENDIF
233300940808     C                   ENDIF
233400951219     C*-------------------------------------------------------------------------
233500951219     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
233600951219     C*
233700951219     C* PULISCO IL SFL
233800951219     C                   SETON                                        26
233900951219     C                   WRITE     LS05C03
234000951219     C                   SETOFF                                       26
234100951219     C*
234200060216     C* CHIAVE PER ACCEDERE AL FILE Fiar401L
234300951219     C                   MOVEL     BLPAAS        KAAS
234400951219     C                   MOVEL     BLPLNP        KLNP
234500951219     C                   MOVEL     BLPNRS        KNRS
234600951219     C                   MOVEL     BLPNSP        KNSP
234700951219     C*
234800951219     C                   Z-ADD     0             NRR3              4 0
234900951219     C                   MOVEL     '7K'          KCOD
235000951219     C*
235100060216     C     Kar42         SETLL     Fiar401L
235200060216     C     Kar42         READE     Fiar401L                               99
235300951219     C     *IN99         DOWEQ     *OFF
235400951219     C*
235500060216     C                   MOVEL(P)  ar4TRC        KKEY
235600951219     C     KTAB1         CHAIN     TABEL                              99
235700951219     C*
235800951219     C     *IN99         IFEQ      *OFF
235900951219     C     TBLFLG        ANDEQ     ' '
236000951219     C                   MOVEL     TBLUNI        DS7K
236100951219     C     §7KVIS        IFEQ      'S'
236200951219     C                   MOVEL     §7KDES        VS3DES
236300060216     C                   MOVEL     ar4NOT        VS3NOT
236400951219     C                   ADD       1             NRR3
236500951219     C                   WRITE     LS05S03
236600951219     C                   ENDIF
236700951219     C                   ENDIF
236800951219     C*
236900060216     C     Kar42         READE     Fiar401L                               99
237000951219     C                   ENDDO
237100951219     C*
237200951219     C* SE NON HO CARICATO NIENTE SCRIVO UGUALMENTE UN RECORD VUOTO
237300951219     C*  NEL SFL PER EVITARE CHE IL PGM DIA ERRORE DI SESSIONE O UNITA'
237400951219     C     NRR3          IFEQ      0
237500951219     C                   ADD       1             NRR3
237600951219     C                   WRITE     LS05S03
237700951219     C                   ENDIF
237800951219     C*
237900940808     C*-------------------------------------------------------------------------
238000930526     C* 95 ON - ESITONO EVENTI DA INTERROGARE
238100111020     C**!!!PA0CLI        IFNE      'S'
238200991022     C     KBLP3         SETLL     FNEVB000                               95
238300111020     C**!!!              ENDIF
238400940805     C*-------------------------------------------------------------------------
238500110705     C                   clear                   SAVNR8
238600110705     C                   eval      *in04 = *off
238700110705     C                   eval      *in84 = *off
238800110705     c
238900110705     c                   clear                   v8car4ksc
239000110705     c                   clear                   v8dar4ksc
239100110705     c                   clear                   v8car4ctr
239200110705     C* Verifico se presete cod cliente e cod tariffa di tassazione da bollettaz
239300110705     C                   MOVEL     'A'           KTRC
239400110705     C     Kar41         CHAIN     Fiar401L                           99
239500110705     C     *IN99         IFEQ      *OFF
239600110705     c                   movel     ar4not        dsbl4a
239700110705     c                   if        §b4aks>0
239800110705     C                   eval      *in84 = *on
239900110705     C                   eval      *in04 = *on
240000110705     c                   eval      v8car4ksc=§b4aks
240100110705     c                   eval      i69kac = v8car4ksc
240200110705     c                   exsr      CTR_ANAGRA
240300110705     c                   eval      v8dar4ksc = acorag
240400110705     c                   if        §b4act<>*blanks
240500110705     c                   movel     §b4act        v8car4ctr
240600110705     c                   endif
240700110705     c                   endif
240800110705     c                   endif
240900110705     c
241000991022      * PER I DATI SULLA TASSAZIONE, CERCO SU FIAR600F.
241100991022      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SOLO SE IMPOSTATO ALMENO UN CAMPO
241200991022     C                   eval      Ktrc = '1'
241300991022     C     KAR6          CHAIN     FIAR601L
241400110705      * CLIENTE E TARIFFA FATTURAZIONE
241500110705     c                   IF        %found(fiar601l)
241600110705     c
241700110705     c                   IF        *IN33 = *ON
241800110705     c                   eval      v8cksc = blpksc
241900110705     c                   eval      v8cctr = blpctr
242000110705     c                   eval      v8dksc = blprsm
242100110705     c                   ELSE
242200110705     c                   eval      v8cksc = AR6ksc
242300110705     c                   eval      v8cctr = AR6ctr
242400110705     c                   move      v8cksc        W0040
242500110705     c                   if        W0040 = 9999
242600110705     c                   eval      v8dksc = blprsd
242700110705     c                   else
242800110705     c                   eval      i69kac = v8cksc
242900110705     c                   exsr      CTR_ANAGRA
243000110705     c                   eval      v8dksc = acorag
243100110705     c                   endif
243200110705     c                   ENDIF
243300110705     c
243400110705      *   decodifica tariffa
243500110705     C                   eval      KCOD = 'TR'
243600110705     C                   MOVEL     V2CCTR        UNOCTR            1
243700110705     C                   MOVEL(P)  UNOCTR        KKEY
243800110705     C     KTAB1         CHAIN     TABEL00F                           31
243900110705     C                   MOVEL     TBLUNI        V8DCTR
244000110705     c
244100110705     c                   ENDIF
244200991022      *
244300991022     C                   IF        %FOUND  and
244400991022     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
244500991022     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
244600991022     C                              AR6sv3 <> *blanks)
244700991022      *
244800991022     C                   eval      *in04 = *on
244900991022      *
245000991025     C                   eval      V8CDIV = AR6div
245100991022     C                   eval      WDIV = AR6div
245200991022     C                   exsr      CTR_VALUTA
245300991022      *
245400991022     c                   IF          ar6por > *zeros
245500991022     c                   z-add     AR6por        W0133
245600991022     c                   eval      Wtstdec = §cvfdc
245700991022     c                   exsr      EDTimporti
245800991022     c                   movel     IMPeditato    V8Cpor
245900991022     c                   ENDIF
246000991022      *
246100991022     c                   IF          ar6imv > *zeros
246200991022     c                   z-add     AR6imv        W0133
246300991022     c                   eval      Wtstdec = §cvfdc
246400991022     c                   exsr      EDTimporti
246500991022     c                   movel     IMPeditato    V8Cimv
246600991022     c                   ENDIF
246700991022      *
246800991022     c                   IF          ar6ift > *zeros
246900991022     c                   z-add     AR6ift        W0133
247000991022     c                   eval      Wtstdec = §cvfdc
247100991022     c                   exsr      EDTimporti
247200991022     c                   movel     IMPeditato    V8Cift
247300991022     c                   ENDIF
247400991022      *
247500991027     c                   IF        AR6fim = 'P'
247600991022     c                   eval      V8Dfim = 'PARTENZA'
247700991022    2C                   ENDIF
247800991022      *
247900991022     C                   MOVEL     AR6CEI        V8CCEI
248000991022      *
248100991022      * SE C'E', DECODIFICO L'ESENZIONE IVA
248200991022     C                   clear                   V8DCEI
248300991022     C                   IF        AR6CEI <>  *BLANKS
248400991022     C                   MOVEL     'EI'          KCOD
248500991022     C                   MOVEL(P)  AR6CEI        KKEY
248600991022     C     KTAB1         CHAIN     TABEL00F                           99
248700991022     C  N99              MOVEL     TBLUNI        V8DCEI
248800991022    2C                   ENDIF
248900991022      *
249000991022      * DATA FATTURA DA GIRARE
249100991022     C                   IF         ar6dft > *zeros
249200991022     C     *iso          movel     AR6DFT        DATA_EUR
249300991022     C     *eur          movel     data_eur      W0080
249400991022     C                   eval      V8Cdft = %EDITC(W0080:'Y')
249500991022     C                   ENDIF
249600991022     C                   MOVEL     ar6NFT        V8CNFT
249700991022     C                   MOVEL     ar6FIV        V8CFIV
249800991022      *
249900991022      * CERCO LE VARIE
250000991022     C                   eval      *in20 = *off
250100991022     C                   eval      *in21 = *off
250200991022     C                   WRITE     LS05C08
250300991022     C                   eval      *in20 = *on
250400991022     C                   eval      *in21 = *on
250500991022      *
250600991022     c                   clear                   nrr8
250700991022      *
250800991022     C                   MOVEL     ar6SV1        WSVN
250900991022     C                   MOVEL     ar6VA1        WVAN
251000991022     c                   exsr      car_varie
251100991022      *
251200991022     C                   MOVEL     ar6SV2        WSVN
251300991022     C                   MOVEL     ar6VA2        WVAN
251400991022     c                   exsr      car_varie
251500991022      *
251600991022     C                   MOVEL     ar6SV3        WSVN
251700991022     C                   MOVEL     ar6VA3        WVAN
251800991022     c                   exsr      car_varie
251900991022      *
252000991022      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
252100991022     C                   if          ar6SV3 <> *blanks
252200991022     C     KAR6          SETLL     FIAR7000
252300991022     C     KAR6          READE     FIAR7000
252400991022     C                   DOW       %EOF = *OFF
252500991022     C                   MOVEL     AR7SVN        WSVN
252600991022     C                   MOVEL     AR7VAN        WVAN
252700991022     c                   exsr      car_varie
252800991022     C     KAR6          READE     FIAR7000
252900991022     C                   ENDDO
253000991022     C                   endif
253100110705     C                   ENDIF
253200991022      *
253300991022      * Se non ho caricato record ne carico uno vuoto
253400110705     c                   IF        NRR8 = *ZEROS  and *in04
253500991022     C                   clear                   V8CDES
253600991022     C                   clear                   V8CVA1
253700991022     C                   clear                   V8CSV1
253800991022     C                   add       1             NRR8
253900991022     C                   WRITE     LS05S08
254000991022     c                   ENDIF
254100991022      *
254200991022     C                   Z-ADD     NRR8          SAVNR8
254300991022     C                   Z-ADD     1             NRR8
254400991022      *
254500991022      *-------------------------------------------------------------------------
254600991022      * DECODIFICO IL CODICE TASSAZIONE, SE C'E'
254700991022     C                   clear                   V8DCTS
254800991022     C                   if         V8CCTS <> *blanks
254900991022     C                   MOVEL     'CT'          KCOD
255000991022     C                   MOVEL(P)  V8CCTS        KKEY
255100991022     C     KTAB1         CHAIN     TABEL00F                           99
255200991022     C  N99              MOVEL     TBLUNI        V8DCTS
255300991022     C                   endif
255400940805     C*-------------------------------------------------------------------------
255500940805     C* PRELEVO LE NOTE DAL FILE APPOSITO
255600940805     C                   MOVEL     BLPAAS        KAAS
255700940805     C                   MOVEL     BLPLNP        KLNP
255800940805     C                   MOVEL     BLPNRS        KNRS
255900940805     C                   MOVEL     BLPNSP        KNSP
256000940805     C                   MOVEL     '8'           KTRC
256100060216     C     Kar41         CHAIN     Fiar401L                           99
256200060216     C  N99              MOVEL     ar4NOT        V2CNO1
256300940805     C   99              MOVEL     *BLANKS       V2CNO1
256400940805     C                   MOVEL     '9'           KTRC
256500060216     C     Kar41         CHAIN     Fiar401L                           99
256600060216     C  N99              MOVEL     ar4NOT        V2CNO2
256700940805     C   99              MOVEL     *BLANKS       V2CNO2
256800991022      *
256900991022      *-------------------------------------------------------------------------
257000051115      * IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU Fiar900F
257100991022      * CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
257200940805     C                   MOVEL     *BLANKS       V2CTIC
257300940805     C                   MOVEL     *ZERO         V2CCAS
257400940805     C                   MOVEL     *BLANKS       V2CVCA
257500940805     C                   MOVEL     *BLANKS       V2CGCA
257600091214     c                   setoff                                       43
257700940923     C                   MOVEL     *BLANKS       V2IGCA
257800940805     C     §3AFCA        IFGT      *ZERO
257900051115     C     KBLP3         CHAIN     Fiar901L                           99
258000940805     C  N99              DO
258100051115     C                   MOVEL     ar9TIC        V2CTIC
258200051115     C                   MOVEL     ar9CAS        V2CCAS
258300051115     C                   MOVEL     ar9VCA        V2CVCA
258400051115     C                   MOVEL     ar9GCA        V2CGCA
258500091214     c                   seton                                        43
258600940805     C                   ENDDO
258700940805     C                   ENDIF
258800991022      * SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
258900940923     C     V2CGCA        COMP      *BLANKS                            1616
259000021218      *
259100021218      * Riempio il campone in fondo alla videata con i tasti scelti
259200021218     C                   exsr      Ges_TastiFun
259300021218     c                   eval      FD07 = FDxx
259400021218     c                   eval      F24D07 = F24Dxx
259500991022      *
259600940923     C     ENDBLP        ENDSR
259700030703      *-------------------------------------------------------------------------
259800030703      *  Decoditifa tipo bancali
259900030703      *-------------------------------------------------------------------------
260000030703     c     Sr_DecTba     BegSr
260100030703     c                   Clear                   Tibs02Ds
260200030703     c                   Clear                   dTba
260300030703     c                   Movel     'C'           T02Mod
260400030703     c                   Movel     Knsif         T02Sif
260500030703     c                   Movel     'TBA'         T02Cod
260600030703     c                   Movel(p)  wtba          T02Ke1
260700030703     c                   Call      'TIBS02R'
260800030703     c                   Parm                    Kpjba
260900030703     c                   Parm                    Tibs02Ds
261000030703If  3c                   If        T02Err = *Blanks
261100030703     c                   Movel     T02Uni        dTba
261200030703    3c                   EndIf
261300030703     c                   Eval      wdestba = §TbaDesc
261400030703     c                   EndSr
261500021218      *-------------------------------------------------------------------------
261600021218      *  Imposto descrizione tasti funzione
261700021218      *-------------------------------------------------------------------------
261800021218     c     Ges_TastiFun  BEGSR
261900021218      *
262000021218      * per emettere i tasti senza condizionamenti ..
262100021218     c                   setoff                                       25
262200021218      *
262300021218      * Conta i caratteri riempiti dalle RigaTf1
262400021218     c                   z-add     0             $z                3 0
262500021218      *
262600021218      * Conta i caratteri riempiti dalle RigaTf2
262700021218     c                   z-add     0             $k                3 0
262800021218      *
262900021218      * Conta i caratteri riempiti dalle RigaTf3
263000021218     c                   z-add     0             $j                3 0
263100021218      *
263200021218      * Conta la posizione del campone da cui partire per impostare
263300021218      * la descrizione del tasto funzione
263400021218     c                   z-add     0             $y                2 0
263500021218      *
263600021218      * Stringhe che contengono le descrizioni dei tasti funzione
263700021218     c                   reset                   RigaTf1
263800021218     c                   reset                   RigaTf2
263900021218     c                   reset                   RigaTf3
264000021218      *
264100021218      * F13
264200021218     c                   IF        *in04 = *on
264300021218     c                   reset                   $Tf
264400021218     c                   movea     Tf13          $Tf
264500021218     c                   Exsr      Rie_$Tf
264600021218     C                   ENDIF
264700021218      *
264800021218      * F04
264900021218     c                   IF        *in06 = *on
265000021218     c                   reset                   $Tf
265100021218     c                   movea     Tf04          $Tf
265200021218     c                   Exsr      Rie_$Tf
265300021218     C                   ENDIF
265400040220      *
265500040220      *?F16?
265600040220     c                   IF        *in28 = *on
265700040220     c                   reset                   $Tf
265800040220     c                   movea     Tf16          $Tf
265900040220     c                   exsr      Rie_$Tf
266000040220     c                   ENDIF
266100021218      *
266200021218      * F05
266300021218     c                   IF        *in95 = *on
266400021218     c                   reset                   $Tf
266500021218     c                   movea     Tf05          $Tf
266600021218     c                   Exsr      Rie_$Tf
266700021218     C                   ENDIF
266800021218      *
266900021218      * F10
267000021218     c                   IF        *in10 = *on
267100021218     c                   reset                   $Tf
267200021218     c                   movea     Tf10          $Tf
267300021218     c                   Exsr      Rie_$Tf
267400021218     c                   ENDIF
267500021218      *
267600021218      * F14
267700021218     c                   IF        *in03 = *on
267800021218     c                   reset                   $Tf
267900021218     c                   movea     Tf14          $Tf
268000021218     c                   Exsr      Rie_$Tf
268100021218     c                   ENDIF
268200051201      *
268300051201      * F22
268400051201     c                   IF        *in24 = *on
268500051201     c                   reset                   $Tf
268600051201     c                   movea     Tf22          $Tf
268700051201     c                   Exsr      Rie_$Tf
268800051201     c                   ENDIF
268900021218      *
269000021218      * F02
269100021218     c                   IF        *in12 = *on
269200021218     c                   reset                   $Tf
269300021218     c                   movea     Tf02          $Tf
269400021218     c                   Exsr      Rie_$Tf
269500021218     c                   ENDIF
269600021218      *
269700021218      * F11
269800021218     c                   IF        *in77 = *on
269900021218     c                   reset                   $Tf
270000021218     c                   movea     Tf11          $Tf
270100021218     c                   Exsr      Rie_$Tf
270200021218     c                   ENDIF
270300021218      *
270400021218      * F07
270500021218     c                   IF        *in22 = *on and *in05 = *off
270600021218     c                   reset                   $Tf
270700021218     c                   movea     Tf07          $Tf
270800021218     c                   Exsr      Rie_$Tf
270900021218     c                   ENDIF
271000021218      *
271100021218      * F08
271200021218     c                   IF        *in55 = *on
271300021218     c                   reset                   $Tf
271400021218     c                   movea     Tf08          $Tf
271500021218     c                   Exsr      Rie_$Tf
271600021218     c                   ENDIF
271700130912
271800130912      * F09 --> Orari servizi non condizionato
271900131007     c                   IF        *in25 = *off and wabif9='S'
272000130930     c                   reset                   $Tf
272100130930     c                   movea     Tf09          $Tf
272200130930     c                   Exsr      Rie_$Tf
272300130930     c                   ENDIF
272400021218      *
272500021218      * F20
272600021218     c                   IF        *in22 = *off
272700021218     c                   reset                   $Tf
272800021218     c                   movea     Tf20          $Tf
272900021218     c                   Exsr      Rie_$Tf
273000021218     c                   ENDIF
273100021218      *
273200021218      * F21 --> non condizionato
273300021218     c                   IF        *in25 = *off
273400021218     c                   reset                   $Tf
273500021218     c                   movea     Tf21          $Tf
273600021218     c                   Exsr      Rie_$Tf
273700021218     c                   ENDIF
273800040220      *
273900040220      * F19
274000040220     c                   IF        *in22 = *off
274100040220     c                   reset                   $Tf
274200040220     c                   movea     Tf19          $Tf
274300040220     c                   Exsr      Rie_$Tf
274400040220     c                   ENDIF
274500040220      *
274600040220      *?F01?
274700040220     c                   IF        *in27 = *on
274800040220     c                   reset                   $Tf
274900040220     c                   movea     Tf01          $Tf
275000040220     c                   exsr      Rie_$Tf
275100040220     c                   ENDIF
275200021218      *
275300021218      * F12 --> non condizionato
275400021218     c                   IF        *in25 = *off
275500021218     c                   reset                   $Tf
275600021218     c                   movea     Tf12          $Tf
275700021218     c                   Exsr      Rie_$Tf
275800021218     c                   ENDIF
275900021218      *
276000021218      * Pulisco le stringa
276100021218     c                   z-add     1             $h
276200030121     c     1             do        62            $h                2 0
276300021218     C                   if        RigaTf1($h) = '#'
276400021218     c                   movel     *blanks       RigaTf1($h)
276500021218     c                   endif
276600021218     c                   enddo
276700021218     c                   z-add     1             $h
276800030121     c     1             do        62            $h                2 0
276900021218     C                   if        RigaTf2($h) = '#'
277000021218     c                   movel     *blanks       RigaTf2($h)
277100021218     c                   endif
277200021218     c                   enddo
277300021218     c                   z-add     1             $h
277400030121     c     1             do        62            $h                2 0
277500021218     C                   if        RigaTf3($h) = '#'
277600021218     c                   movel     *blanks       RigaTf3($h)
277700021218     c                   endif
277800021218     c                   enddo
277900021218      *
278000021218      * Imposto la descrizione del tasto funzione F24 e segnalo
278100021218      * quante righe ho riempito e quale devo emettere
278200021218     c                   SELECT
278300021218      *
278400021218     c                   when      $k<>*zeros and $Y<>*zeros
278500021218     c                   eval      $loop = 3
278600021218     c                   z-add     3             $loop             1 0
278700021218     C                   movel(p)  CF2413        F24Dxx           16
278800021218      *
278900021218     c                   when      $k<>*zeros and $Y=*zeros
279000021218     c                   z-add     2             $loop             1 0
279100021218     C                   movel(p)  CF2412        F24Dxx
279200021218      *
279300021218     c                   Other
279400021218     c                   z-add     1             $loop             1 0
279500021218     C                   movel     *blanks       F24Dxx
279600021218     c                   ENDSL
279700021218      * Prima riga di tasti funzione
279800030121     c                   movea     RigaTF1       FDxx             62
279900021218     c                   z-add     1             $Rig              1 0
280000021218      *
280100021218     c                   endsr
280200021218      *-------------------------------------------------------------------------
280300021218      *  Riempio le stringhe con i campi funzione compattati
280400021218      *-------------------------------------------------------------------------
280500021218     c     Rie_$Tf       begsr
280600021218      *
280700021218     c                   z-add     1             $x                2 0
280800021218     c     '#'           lookup    $Tf($x)                                31
280900021218     c                   add       $x            $Z
281000030121     c                   IF        $Z <= 63
281100021218     c                   eval      $j = $Z - $x + 1
281200021218     c                   movea     $Tf           RigaTF1($j)
281300021218     c                   ELSE
281400021218     c                   add       $x            $K
281500030121     c                   IF        $K <= 63
281600021218     c                   eval      $j = $K - $x + 1
281700021218     c                   movea     $Tf           RigaTF2($j)
281800021218     c                   ELSE
281900021218     c                   add       $x            $Y
282000030121     c                   IF        $Y <= 63
282100021218     c                   eval      $j = $Y - $x + 1
282200021218     c                   movea     $Tf           RigaTF3($j)
282300021218     c                   ENDIF
282400021218     c                   ENDIF
282500021218     c                   ENDIF
282600021218      *
282700021218     c                   endsr
282800021218      *-------------------------------------------------------------------------
282900021218      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
283000021218      *-------------------------------------------------------------------------
283100021218     c     Ges_F24       begsr
283200021218      *
283300021218     c                   SELECT
283400021218      *
283500021218     c                   WHEN      $Loop = 2 and $Rig =2  or
283600021218     c                             $Loop = 3 and $Rig =3
283700021218     c                   movea     RigaTF1       FDxx
283800021218     c                   z-add     1             $Rig
283900021218      *
284000021218     c                   if        $loop = 2
284100021218     c                   eval      F24Dxx= Cf2412
284200021218     c                   else
284300021218     c                   eval      F24Dxx= Cf2413
284400021218     c                   endif
284500021218      *
284600021218     c                   WHEN      $Loop = 2 and $Rig =1 or
284700021218     c                             $Loop = 3 and $Rig =1
284800021218     c                   movea     RigaTF2       FDxx
284900021218     c                   z-add     2             $Rig
285000021218      *
285100021218     c                   if        $loop = 2
285200021218     c                   eval      F24Dxx= Cf2422
285300021218     c                   else
285400021218     c                   eval      F24Dxx= Cf2423
285500021218     c                   endif
285600021218      *
285700021218     c                   WHEN      $Loop = 3 and $Rig =2
285800021218     c                   movea     RigaTF3       FDxx
285900021218     c                   z-add     3             $Rig
286000021218     c                   eval      F24Dxx= Cf2433
286100021218     c                   ENDSL
286200021218      *
286300021218     c                   endsr
286400991022      *-------------------------------------------------------------------------
286500991022      *  CARICO SUBFILE VARIE
286600991022      *-------------------------------------------------------------------------
286700991022     c     CAR_VARIE     BEGSR
286800991022      *
286900991022     c                   SELECT
287000991022      *
287100991022     c                   WHEN       WSVN = *BLANKS
287200991022      *
287300991022     C                   WHEN       WSVN = §$2iva
287400991022     c                   z-add     Wvan          W0133
287500991022     c                   eval      Wtstdec = §cvfdc
287600991022     c                   exsr      EDTimporti
287700991025     c                   movel     IMPeditato    V8Civa
287800991022      *
287900991022     c                   OTHER
288000991022     c                   z-add     Wvan          W0133
288100991022     c                   eval      Wtstdec = §cvfdc
288200991022     c                   exsr      EDTimporti
288300991022     c                   movel     IMPeditato    V8Cva1
288400991022     c                   movel     WSVN          V8Csv1
288500991022     C                   Z-ADD     1             WA
288600991022     C     V8CSV1        LOOKUP    CC1(WA)                                99
288700020329     C     CC3(WA)       ifeq      'S'
288800020329      *
288900020329      * Verifico se si tratta di una bolla Fedex
289000021119     c                   if        LnaNtw = 'FED'
289100020329     C                   Z-ADD     1             WA
289200020329     C     V8CSV1        LOOKUP    CB1(WA)                                99
289300020329     C   99              MOVEL     CB2(WA)       V8CDES
289400020329     C  N99              clear                   V8CDES
289500020329     C                   else
289600991022     C   99              MOVEL     CC2(WA)       V8CDES
289700991022     C  N99              clear                   V8CDES
289800020329     C                   endif
289900020329     C                   else
290000020329     C   99              MOVEL     CC2(WA)       V8CDES
290100020329     C  N99              clear                   V8CDES
290200020329     C                   endif
290300991022     C                   add       1             NRR8
290400991022     C                   WRITE     LS05S08
290500991022      *
290600991022     c                   ENDSL
290700991022      *
290800991022     c                   ENDSR
290900040116      *-------------------------------------------------------------------------
291000040116      *  Carico le varie della seconda bolla
291100040116      *-------------------------------------------------------------------------
291200040116     c     Car_Varie1    BegSr
291300040116
291400040116s   1c                   Select
291500040116
291600040116     c                   When      wsvn = *Blanks
291700040116      * Iva
291800040116     c                   When      wsvn = §$2iva
291900040116     c                   z-add     wvan          w0133
292000040116     c                   Eval      Wtstdec = §cvfdc
292100040116     c                   ExSr      EDTimporti
292200040116     c                   Movel     IMPeditato    Vi2Iva
292300040116      * Bollo
292400040116     c                   When      wsvn = §$2bol
292500040116     c                   z-add     wvan          w0133
292600040116     c                   Eval      Wtstdec = §cvfdc
292700040116     c                   ExSr      EDTimporti
292800040116     c                   Movel     IMPeditato    Vi2Iva
292900040116      * Varia G - sempre per prima
293000040116     c                   When      wsvn = §$2vrg
293100040116     c                   Eval      Vi2Sv1 = wsvn
293200040116     c                   z-add     wvan          w0133
293300040116     c                   Eval      Wtstdec = §cvfdc
293400040116     c                   ExSr      EDTimporti
293500040116     c                   Movel     IMPeditato    Vi2Va1
293600040116      * Altre varie
293700040116   x1c                   Other
293800040116     c                   z-add     wvan          W0133
293900040116     c                   eval      Wtstdec = §cvfdc
294000040116     c                   exsr      EDTimporti
294100040116if  2c                   If        Vi2Sv2 = *Blanks
294200040116     c                   movel     IMPeditato    Vi2Va2
294300040116     c                   movel     wsvn          Vi2sv2
294400040116   x2c                   Else
294500040116if  3c                   If        Vi2Sv3 = *Blanks
294600040116     c                   movel     IMPeditato    Vi2Va3
294700040116     c                   movel     wsvn          Vi2sv3
294800040116   x3c                   Else
294900040116if  4c                   If        Vi2Sv4 = *Blanks
295000040116     c                   movel     IMPeditato    Vi2Va4
295100040116     c                   movel     wsvn          Vi2sv4
295200041130   x4c                   Else
295300041130if  5c                   If        Vi2Sv5 = *Blanks
295400041130     c                   movel     IMPeditato    Vi2Va5
295500041130     c                   movel     wsvn          Vi2sv5
295600041130   x5c                   Else
295700041130if  6c                   If        Vi2Sv6 = *Blanks
295800041130     c                   movel     IMPeditato    Vi2Va6
295900041130     c                   movel     wsvn          Vi2sv6
296000041130    6c                   EndIf
296100041130    5c                   EndIf
296200040116    4c                   EndIf
296300040116    3c                   EndIf
296400040116    2c                   EndIf
296500040116
296600040116    1c                   EndSl
296700040116
296800040116     c                   EndSr
296900991022      **********************************************************************
297000991022      * AGGANCIO TABELLA VALUTE
297100991022      **********************************************************************
297200991022     C     CTR_VALUTA    BEGSR
297300991022
297400991022     C                   MOVEL     'CV'          KCOD
297500991022     C                   MOVEL(P)  WDIV          KKEY
297600991022     C     KTAB1         CHAIN     TABEL00F                           31
297700991022     C  N31              MOVEL     TBLUNI        DSCV
297800991022
297900991022     C                   ENDSR
298000991022      **********************************************************************
298100991022      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
298200991022      **********************************************************************
298300991022     C     EDTIMPORTI    BEGSR
298400991022
298500991022      * ------------------------------------------------------------------------
298600991022      * W0133      contiene il numero da editare per il video
298700991022      * WTSTDEC    corrisponde a §CVfdc della tabella CV
298800991022      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
298900991022      * ------------------------------------------------------------------------
299000991022      *
299100991022     c                   clear                   IMPeditato
299200991022     c                   clear                   W017A
299300991022      *
299400991022     c                   if        Wtstdec = 'S'
299500991022     C                   eval      W017a = %EDITC(W0133:'2')
299600991022     c                   else
299700991022     c                   z-add     W0133         W0130
299800991022     C                   eval      W017a = %EDITC(W0130:'2')
299900991022     c                   endif
300000991022      *
300100991022     c                   move      W017a         IMPeditato
300200991022      *
300300991022     c                   ENDSR
300400991025      *****************************************************************
300500991025      *  AGGANCIO ANAGRAFICHE
300600991025      *****************************************************************
300700991025     C     CTR_ANAGRA    BEGSR
300800991025      *
300900991025     C                   clear                   DS_cnaco
301000991025     C                   clear                   DS_cnind
301100991025     C                   clear                   DS_cnclp
301200991025     C                   clear                   DS_fncls
301300991025      *
301400991025     C                   CALL      'TIBS69R'
301500991025     C                   PARM                    tibs69DS
301600991025     C                   PARM                    DS_cnaco
301700991025     C                   PARM                    DS_cnind
301800991025     C                   PARM                    DS_cnclp
301900991025     C                   PARM                    DS_fncls
302000991025      *
302100991025     C                   eval      wtibs69r = 'S'
302200991025      * per il 1° giro è inizializzata nelle specifiche "D"
302300991025     C                   clear                   TIBS69DS
302400991025      *
302500991025     C                   ENDSR
302600940805     C**************************************************************************
302700940805     C*    CMD18 VISUALIZZA LEGAMI
302800940805     C**************************************************************************
302900920326     C     CMD18         BEGSR
303000940810     C*
303100940907     C* SAVNR9 > 2 --> GESTIONE SUBFILE
303200950328    1C     SAVNR9        IFGT      2
303300940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
303400940810     C                   MOVEL     'S'           WRIEM
303500940810     C     FORC2         TAG
303600940810     C                   Z-ADD     1             REC9
303700950328    2C     WRIEM         IFEQ      'S'
303800940810     C                   WRITE     LS05T01
303900940810     C                   WRITE     LS05Z09
304000940810     C                   MOVEL     *BLANKS       WRIEM
304100950328    2C                   ENDIF
304200940810     C                   EXFMT     LS05C09
304300940810     C*
304400940810     C* CMD12 - FINE LAVORO
304500940810     C   KL              GOTO      SOTTO
304600940810     C*
304700940810     C                   READC     LS05S09                                93
304800950328    2C     *IN93         DOWEQ     '0'
304900940810     C*
305000950328    3C     V9CSCE        IFEQ      '5'
305100940810     C                   MOVEL     ' '           V9CSCE
305200950103     C*
305300950103     C* RIPRISTINO GLI INDICATORI
305400950103     C                   MOVEL     V9CI80        *IN80
305500950103     C                   MOVEL     V9CI81        *IN81
305600950103     C                   MOVEL     V9CI82        *IN82
305700950103     C                   MOVEL     V9CI83        *IN83
305800940810     C                   UPDATE    LS05S09
305900940810     C*
306000950328    4C     V9CAAN        IFEQ      *ZERO
306100940907     C                   MOVEL     V9CAAP        PA4AAS
306200940907     C                   MOVEL     V9CLPP        PA4LNP
306300940907     C                   MOVEL     V9CNRP        PA4NRS
306400940907     C                   MOVEL     V9CNSP        PA4NSP
306500940907     C                   ELSE
306600940907     C                   MOVEL     V9CAAN        PA4AAS
306700940907     C                   MOVEL     V9CLPN        PA4LNP
306800940907     C                   MOVEL     V9CNRN        PA4NRS
306900940907     C                   MOVEL     V9CNSN        PA4NSP
307000950328    4C                   ENDIF
307100940810     C                   MOVEL     *BLANKS       PA4F03
307200960903     C     PA0FLG        IFEQ      'N'
307300960903     C* N.B.: Non testo anche pa0flg='R' perchè è impossibile che si
307400960903     C*       verifichi questo caso
307500960903     C                   MOVEL     'P'           PA4FLG
307600960903     C                   ELSE
307700940810     C                   MOVEL     '3'           PA4FLG
307800960903     C                   END
307900940810     C                   MOVEL     PARAM4        KPJBU
308000940810     C                   CALL      'FNLR36R'
308100940810     C                   PARM                    KPJBA
308200940810     C                   MOVEL     KPJBU         PARAM4
308300940810     C                   MOVEL     PA4F03        WF03
308400940810     C                   MOVEL     'S'           WRIEM
308500950328    3C                   ENDIF
308600940810     C*
308700940810     C                   READC     LS05S09                                93
308800950328    2C                   ENDDO
308900940908     C*
309000940908     C* RIEMETTO IL FORMATO VIDEO DEI LEGAMI
309100950328    2C     WRIEM         IFEQ      'S'
309200940908     C                   GOTO      FORC2
309300950328    2C                   ENDIF
309400940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
309500940810     C                   ELSE
309600940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
309700940810     C                   MOVEL     COMAAN        PA4AAS
309800940810     C                   MOVEL     COMLPN        PA4LNP
309900940810     C                   MOVEL     COMNRN        PA4NRS
310000940810     C                   MOVEL     COMNSN        PA4NSP
310100940810     C                   MOVEL     *BLANKS       PA4F03
310200960903     C     PA0FLG        IFEQ      'N'
310300960903     C* N.B.: Non testo anche pa0flg='R' perchè è impossibile che si
310400960903     C*       verifichi questo caso
310500960903     C                   MOVEL     'P'           PA4FLG
310600960903     C                   ELSE
310700940810     C                   MOVEL     '3'           PA4FLG
310800960903     C                   END
310900940810     C                   MOVEL     PARAM4        KPJBU
311000940810     C                   CALL      'FNLR36R'
311100940810     C                   PARM                    KPJBA
311200940810     C                   MOVEL     KPJBU         PARAM4
311300940810     C                   MOVEL     PA4F03        WF03
311400940810     C                   MOVEL     'S'           WRIEM
311500940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
311600940810     C                   ENDIF
311700940810     C*
311800940810     C     SOTTO         TAG
311900920326     C                   ENDSR
312000940802      **************************************************************************
312100940802      * VISUALIZZO IL FORMATO VIDEO DI RIEPILOGO DELLA BOLLA
312200940802      **************************************************************************
312300110601     C     RIEPIL        BEGSR
312400940802      *
312500940802     C                   SETOFF                                       25
312600940804     C                   MOVEL     'S'           WRIEM             1
312700021218      *
312800021218      * Riempio il campone in fondo alla videata con i tasti scelti
312900021218     C                   exsr      Ges_TastiFun
313000021218     c                   eval      FD07 = FDxx
313100021218     c                   eval      F24D07 = F24Dxx
313200940802     C     FOR07         TAG
313300940804     C     WRIEM         IFEQ      'S'
313400940804     C                   WRITE     LS05T01
313500940804     C                   MOVEL     *BLANKS       WRIEM
313600940804     C                   ENDIF
313700021218      *
313800021218      * Imposto la stringa comandi del 'giro' successivo
313900021218     C                   IF        *inKY = *on and $Loop > 1
314000021218     c                   Exsr      Ges_F24
314100021218     c                   eval      FD07 = FDxx
314200021218     c                   eval      F24D07 = F24Dxx
314300021218     C                   ENDIF
314400940804     C                   EXFMT     LS05D07
314500940808     C                   MOVEL     *BLANKS       WF03
314600940802      *
314700990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
314800940802     C   KY*IN25         IFEQ      *ON
314900940802     C                   SETOFF                                       25
315000940802     C                   ELSE
315100940802     C                   SETON                                        25
315200940802     C                   ENDIF
315300940802     C   KY              GOTO      FOR07
315400940802      *
315500990118      * F3 - FINE LAVORO TORNO A SUBFILE
315600941109     C   KC              MOVEL     '1'           PA0F03
315700940802     C   KC              GOTO      ENDRIE
315800940802      *
315900940802      * TASTI DI FUNZIONE VARI
316000940802     C                   EXSR      COMAND
316100940808     C*
316200940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
316300940808     C     WF03          IFNE      *BLANKS
316400941109     C                   MOVEL     '1'           PA0F03
316500940808     C                   GOTO      ENDRIE
316600940808     C                   ENDIF
316700091125     c*
316800091125     c* verifico se inserito '?' su beneficiario C/A
316900091125     c                   if        v2cben='?'
317000091125     c                   exsr      sr_bencas
317100091125     C                   SETON                                        31
317200091125     C                   MOVEL     'S'           WRIEM
317300091125     c                   endif
317400110601
317500950629     C*
317600940802     C   31              GOTO      FOR07
317700940802      *
317800940802     C     ENDRIE        ENDSR
317900940802      **************************************************************************
318000940802      * Nel caso di interrogazione da parte di un cliente, viene verificato se
318100940802      * egli puo' interrogare le bolle e se deve essere conteggiato il traffico
318200940802      * dei byte sulla linea di comunicazione.
318300940802      * Queste informazioni sono contenute nella tabella 3K.
318400940802      *
318500940802      * Inoltre, egli puo' interrogare solo alcuni clienti (quelli a lui
318600940802      * unificati.
318700940802      **************************************************************************
318800111020     C**!!!ABICLI        BEGSR
318900940802      *
319000111020     C**!!!              MOVE      *BLANKS       $3KINB
319100090603     C***                MOVE      *BLANKS       $3KCTB
319200940802      *
319300940802      * Leggo tutta la tabella 3K riempendo una schiera con i cod.clienti
319400111020     C**!!!              Z-ADD     *ZERO         $PRIMA            1 0
319500111020     C**!!!              Z-ADD     *ZERO         $A                3 0
319600111020     C**!!!              MOVE      '3K'          KCOD
319700111020     C**!!!KTAB2         SETLL     TABEL00F
319800111020     C**!!!              DO        *HIVAL
319900111020     C**!!!KTAB2         READE     TABEL00F                               90
320000111020     C*!N90TBLFLG        IFEQ      *BLANKS
320100111020     C**!!!              MOVEL     TBLUNI        DS3K
320200111020     C**!!!KNMUS         IFEQ      §3KPRF
320300940802      *
320400940802      * Devo prendere il cod.cliente, che e' la chiave di TABEL
320500111020     C**!!!              MOVEL     TBLKEY        $CODIC            7 0
320600111020     C**!!!$CODIC        LOOKUP    CLI                                    99
320700111020     C*!N99$A            IFLT      300
320800111020     C**!!!              ADD       1             $A
320900111020     C**!!!              Z-ADD     $CODIC        CLI($A)
321000111020     C**!!!              ENDIF
321100940802      *
321200940802      * Gia' dalla prima lettura ho i due flag che abilitano il profilo
321300111020     C**!!!$PRIMA        IFEQ      *ZERO
321400111020     C**!!!              MOVE      §3KINB        $3KINB
321500090603     C***                MOVE      §3KCTB        $3KCTB
321600111020     C**!!!§3KINB        COMP      'S'                                9090
321700111020     C**!!!              Z-ADD     1             $PRIMA
321800111020     C**!!!              ENDIF
321900940802      *
322000111020     C**!!!              ENDIF
322100111020     C**!!!              ENDIF
322200111020     C*!N90              ENDDO
322300940802      *
322400111020     C**!!!              ENDSR
322500940905     C**************************************************************************
322600940905     C* RIEMPIE IL SFL CON TUTTI I LEGAMI BOLLA
322700940905     C* PARTENDO DALLA BOLLA ORIGINALE, FINO ALL'ULTIMA IN
322800940906     C* ORDINE CRONOLOGICO (ULTIMO MODELLO SWATCH)
322900940905     C**************************************************************************
323000940906     C     LEGBOL        BEGSR
323100940906     C*
323200940906     C* PULISCO IL SFL
323300940906     C                   SETOFF                                       202123
323400940906     C                   WRITE     LS05C09
323500940906     C                   SETON                                        2123
323600940906     C*
323700940906     C                   SETOFF                                       07
323800940906     C                   Z-ADD     *ZERO         SAVNR9            4 0
323900940906     C                   MOVEL     *BLANKS       WFLG1             1
324000940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
324100940905     C*
324200940906     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
324300940906     C     KBLP3         CHAIN     FNLBL01L                           99
324400940906     C   99KBLP3         CHAIN     FNLBL02L                           99
324500940906     C  N99              DO
324600940905     C*
324700940906     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
324800940906     C                   MOVEL     LBLAAO        KAAS
324900940906     C                   MOVEL     LBLLPO        KLNP
325000940906     C                   MOVEL     LBLNRO        KNRS
325100940906     C                   MOVEL     LBLNSO        KNSP
325200940906     C     KLBL3         CHAIN     FNLBL02L                           99
325300950103     C     *IN99         IFEQ      *ON
325400950103     C                   MOVEL     'N'           WORIGI            1
325500950103     C                   ELSE
325600950103     C                   MOVEL     'S'           WORIGI
325700950103     C                   ENDIF
325800940907     C*
325900940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
326000940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
326100950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
326200950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
326300940907     C                   MOVEL     ' '           V9CSCE
326400940907     C                   MOVEL     LBLAAO        V9CAAP
326500940907     C                   MOVEL     LBLLPO        V9CLPP
326600940907     C                   MOVEL     LBLLAO        V9CLAP
326700940907     C                   MOVEL     LBLNRO        V9CNRP
326800940907     C                   MOVEL     LBLNSO        V9CNSP
326900940926     C                   CLEAR                   V9CAAN
327000940926     C                   CLEAR                   V9CLPN
327100940926     C                   CLEAR                   V9CLAN
327200940926     C                   CLEAR                   V9CNRN
327300940926     C                   CLEAR                   V9CNSN
327400940907     C*
327500940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
327600940907     C     LBLAAO        IFEQ      V1CAAS
327700940907     C     LBLLPO        ANDEQ     V1CLNP
327800940907     C     LBLNRO        ANDEQ     V1CNRS
327900940907     C     LBLNSO        ANDEQ     V1CNSP
328000940907     C                   MOVEL     *BLANKS       V9CPRE
328100940907     C                   SETON                                        80
328200940907     C                   SETON                                        81
328300940907     C                   MOVEL     '1'           WFLG1
328400940907     C                   ELSE
328500940907     C                   SETOFF                                       81
328600940907     C*
328700950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
328800950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
328900950320     C                   SETOFF                                       93
329000950320     C     KLBL3         SETLL     FNARB01L                               93
329100950320     C  N93              MOVEA     PRD(1)        V9CPRE
329200950320     C  N93              SETON                                        80
329300950320     C*
329400950320     C* SE RICHIAMATO DAGLI ARRIVI (FNLR36R) NON POSSO
329500950320     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
329600950320     C   93PA0FLG        IFEQ      'A'
329700960903     C     PA0FLG        OREQ      'R'
329800950320     C                   MOVEA     PRD(4)        V9CPRE
329900950320     C                   SETON                                        80
330000950320     C                   ELSE
330100950328     C                   MOVEA     PRD(2)        V9CPRE
330200950320     C                   SETOFF                                       80
330300950320     C                   ENDIF
330400950320     C*
330500950320     C* SE NON TROVATA SU ARB PROVO SU BLP
330600950320     C  N93              DO
330700950320     C                   SETOFF                                       93
330800950320     C     KLBL3         SETLL     FNBLP01L                               93
330900950320     C*
331000950328     C   93              MOVEA     PRD(3)        V9CPRE
331100950428     C   93              SETON                                        80
331200950320     C*
331300950320     C                   ENDDO
331400940907     C*
331500950320     C                   ENDIF
331600940907     C                   SETON                                        83
331700950103     C*
331800950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
331900950103     C                   MOVEL     *IN80         V9CI80
332000950103     C                   MOVEL     *IN81         V9CI81
332100950103     C                   MOVEL     *IN82         V9CI82
332200950103     C                   MOVEL     *IN83         V9CI83
332300950103     C*
332400940907     C                   Z-ADD     1             NRR9
332500940907     C                   WRITE     LS05S09
332600940907     C                   SETON                                        20
332700950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
332800950103     C*
332900950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
333000950103     C*                      2) NON HO TROVATO L'ORIGINALE
333100950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
333200950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
333300950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
333400950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
333500950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
333600950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
333700950103     C* L'ORIGINALE E LA MAMMA.
333800950103     C     WORIGI        IFEQ      'N'
333900940907     C*
334000950103     C                   MOVEL     ' '           V9CSCE
334100950103     C                   CLEAR                   V9CAAP
334200950103     C                   CLEAR                   V9CLPP
334300950103     C                   CLEAR                   V9CLAP
334400950103     C                   CLEAR                   V9CNRP
334500950103     C                   CLEAR                   V9CNSP
334600950103     C                   MOVEL     LBLAAP        V9CAAN
334700950103     C                   MOVEL     LBLLPP        V9CLPN
334800950103     C                   MOVEL     LBLLAP        V9CLAN
334900950103     C                   MOVEL     LBLNRP        V9CNRN
335000950103     C                   MOVEL     LBLNSP        V9CNSN
335100950103     C                   SETOFF                                       8182
335200950103     C*
335300950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
335400950103     C     V9CAAN        IFEQ      V1CAAS
335500950103     C     V9CLPN        ANDEQ     V1CLNP
335600950103     C     V9CNRN        ANDEQ     V1CNRS
335700950103     C     V9CNSN        ANDEQ     V1CNSP
335800950103     C                   MOVEL     *BLANKS       V9CPRE
335900950103     C                   SETON                                        80
336000950103     C                   SETON                                        82
336100950103     C                   ELSE
336200950103     C*
336300950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
336400950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
336500950320     C                   EXSR      CHKBOL
336600950103     C*
336700950103     C                   ENDIF
336800950103     C*
336900950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
337000950103     C                   SETOFF                                       83
337100950103     C*
337200950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
337300950103     C                   MOVEL     *IN80         V9CI80
337400950103     C                   MOVEL     *IN81         V9CI81
337500950103     C                   MOVEL     *IN82         V9CI82
337600950103     C                   MOVEL     *IN83         V9CI83
337700950103     C*
337800950103     C                   ADD       1             NRR9
337900950103     C                   WRITE     LS05S09
338000950103     C*
338100950103     C                   ENDIF
338200940907     C                   SETOFF                                       83  99
338300940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
338400940906     C* CICLO FINO ALL'ULTIMO LEGAME
338500940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
338600940907     C                   Z-ADD     *ZERO         WA                3 0
338700940907     C                   Z-ADD     *ZERO         WB                3 0
338800940907     C                   MOVEA     *ZERO         AAN
338900940906     C     *IN99         DOWEQ     *OFF
339000940905     C*
339100940906     C                   MOVEL     ' '           V9CSCE
339200940907     C                   MOVEL     LBLAAP        V9CAAP
339300940907     C                   MOVEL     LBLLPP        V9CLPP
339400940907     C                   MOVEL     LBLLAP        V9CLAP
339500940907     C                   MOVEL     LBLNRP        V9CNRP
339600940907     C                   MOVEL     LBLNSP        V9CNSP
339700940907     C                   MOVEL     LBLAAN        V9CAAN
339800940907     C                   MOVEL     LBLLPN        V9CLPN
339900940907     C                   MOVEL     LBLLAN        V9CLAN
340000940907     C                   MOVEL     LBLNRN        V9CNRN
340100940907     C                   MOVEL     LBLNSN        V9CNSN
340200940907     C                   SETOFF                                       8182
340300940906     C*
340400950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
340500940907     C     LBLAAP        IFEQ      V1CAAS
340600940907     C     LBLLPP        ANDEQ     V1CLNP
340700940907     C     LBLNRP        ANDEQ     V1CNRS
340800940907     C     LBLNSP        ANDEQ     V1CNSP
340900940907     C                   SETON                                        81
341000940907     C                   ENDIF
341100940907     C*
341200940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
341300940907     C     LBLAAN        IFEQ      V1CAAS
341400940907     C     LBLLPN        ANDEQ     V1CLNP
341500940907     C     LBLNRN        ANDEQ     V1CNRS
341600940907     C     LBLNSN        ANDEQ     V1CNSP
341700940906     C                   MOVEL     *BLANKS       V9CPRE
341800940906     C                   SETON                                        80
341900940907     C                   SETON                                        82
342000940906     C                   ELSE
342100940906     C*
342200950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
342300950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
342400950320     C                   EXSR      CHKBOL
342500940906     C*
342600940906     C                   ENDIF
342700950103     C*
342800950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
342900950103     C                   MOVEL     *IN80         V9CI80
343000950103     C                   MOVEL     *IN81         V9CI81
343100950103     C                   MOVEL     *IN82         V9CI82
343200950103     C                   MOVEL     *IN83         V9CI83
343300940906     C*
343400940906     C* SCRIVO IL RECORD NEL SFL
343500940906     C                   ADD       1             NRR9
343600940906     C                   WRITE     LS05S09
343700940907     C*
343800940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
343900940907     C                   ADD       1             WA
344000940907     C                   MOVEL     LBLAAN        AAN(WA)
344100940907     C                   MOVEL     LBLLPN        LPN(WA)
344200940907     C                   MOVEL     LBLNRN        NRN(WA)
344300940907     C                   MOVEL     LBLNSN        NSN(WA)
344400940907     C*
344500940907     C     KLBL3         READE     FNLBL02L                               99
344600940906     C*
344700940907     C* SE NON HO PIU' BOLLE CON READE
344800940906     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
344900970116     C     WB            DOWLT     WA
345000970116     C     *IN99         ANDEQ     *ON
345100940907     C                   ADD       1             WB
345200940907     C                   MOVEL     AAN(WB)       KAAS
345300940907     C                   MOVEL     LPN(WB)       KLNP
345400940907     C                   MOVEL     NRN(WB)       KNRS
345500940907     C                   MOVEL     NSN(WB)       KNSP
345600940907     C     KLBL3         SETLL     FNLBL02L
345700940907     C     KLBL3         READE     FNLBL02L                               99
345800970116     C                   ENDDO
345900940906     C*
346000940906     C                   ENDDO
346100940906     C                   ENDDO
346200940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
346300940906     C*
346400940906     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
346500940906     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
346600940926     C     NRR9          IFEQ      2
346700940907     C*
346800940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
346900940907     C     2             CHAIN     LS05S09                            99
347000940907     C     V9CAAP        IFEQ      V1CAAS
347100940907     C     V9CLPP        ANDEQ     V1CLNP
347200940907     C     V9CNRP        ANDEQ     V1CNRS
347300940907     C     V9CNSP        ANDEQ     V1CNSP
347400940907     C                   MOVEL     V9CAAN        COMAAN
347500940907     C                   MOVEL     V9CLPN        COMLPN
347600940907     C                   MOVEL     V9CNRN        COMNRN
347700940907     C                   MOVEL     V9CNSN        COMNSN
347800950306     C                   MOVEL     V9CLAN        COMLAN
347900950316     C                   MOVEA     'SEGUEN.'     LBL(14)
348000940907     C                   ELSE
348100940907     C                   MOVEL     V9CAAP        COMAAN
348200940907     C                   MOVEL     V9CLPP        COMLPN
348300940907     C                   MOVEL     V9CNRP        COMNRN
348400940907     C                   MOVEL     V9CNSP        COMNSN
348500950306     C                   MOVEL     V9CLAP        COMLAN
348600950316     C                   MOVEA     'PRECED.'     LBL(14)
348700940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
348800940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
348900940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
349000940926     C     1             CHAIN     LS05S09                            99
349100940926     C* REIMPOSTO NRR9=2 PERCHE' MI SERVE DOPO
349200940926     C                   Z-ADD     2             NRR9
349300940907     C                   ENDIF
349400940907     C*
349500950316     C                   MOVEA     COMLPN        LBL(21)
349600950316     C                   MOVEA     COMNRN        LBL(25)
349700950316     C                   MOVEA     COMNSN        LBL(28)
349800950316     C                   MOVEA     COMLAN        LBL(40)
349900940906     C*
350000940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
350100940907     C                   Z-ADD     1             W                 1 0
350200940907     C     V9CPRE        LOOKUP    PRD(W)                                 99
350300950306     C                   MOVEA     LBD(W)        LBL(44)
350400940907     C     W             COMP      2                                      07
350500940906     C*
350600940906     C                   MOVEA     LBL           LEGAME
350700940906     C                   ELSE
350800940905     C*
350900940926     C     NRR9          IFGT      2
351000940906     C                   MOVEA     LB2           LEGAME
351100940906     C                   SETON                                        07
351200940906     C                   ENDIF
351300940906     C                   ENDIF
351400940906     C*
351500940907     C     NRR9          COMP      1                                    3434
351600940905     C*
351700940906     C                   Z-ADD     NRR9          SAVNR9
351800940906     C                   Z-ADD     1             NRR9
351900940912     C*
352000940912     C* RIAGGANCIO LA BOLLA IN ESAME
352100940912     C     KBLP3         CHAIN     FNBLP01L                           99
352200940905     C*
352300940905     C                   ENDSR
352400950320     C**************************************************************************
352500950320     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
352600950320     C**************************************************************************
352700950320     C     CHKBOL        BEGSR
352800950320     C*
352900950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
353000950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
353100950320     C                   SETOFF                                       93
353200950320     C     KLBL1         SETLL     FNARB01L                               93
353300950320     C  N93              MOVEA     PRD(1)        V9CPRE
353400950320     C  N93              SETON                                        80
353500950320     C*
353600950320     C* SE RICHIAMATO DAGLI ARRIVI (FNLR36R) NON POSSO
353700950320     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
353800950320     C   93PA0FLG        IFEQ      'A'
353900960903     C     PA0FLG        OREQ      'R'
354000950320     C                   MOVEA     PRD(4)        V9CPRE
354100950320     C                   SETON                                        80
354200950320     C                   ELSE
354300950320     C                   MOVEA     PRD(2)        V9CPRE
354400950320     C                   SETOFF                                       80
354500950320     C                   ENDIF
354600950320     C*
354700950320     C* SE NON TROVATA SU ARB PROVO SU BLP
354800950320     C  N93              DO
354900950320     C                   SETOFF                                       93
355000950320     C     KLBL1         SETLL     FNBLP01L                               93
355100950328     C   93              MOVEA     PRD(3)        V9CPRE
355200950428     C   93              SETON                                        80
355300950320     C*
355400950320     C                   ENDDO
355500950320     C*
355600950320     C                   ENDSR
355700940923      **************************************************************************
355800940923      *   CONTROLLI SUL FORMATO VIDEO D02
355900940923      **************************************************************************
356000940923     C     CTRD02        BEGSR
356100940923     C*
356200940923     C                   SETOFF                                       31
356300940923     C*
356400110601     c* verifico se inserito '?' su email destinatario
356500110601     c                   if        v2cemw='?'
356600110601     c                   exsr      sr_emdest
356700110601     C                   SETON                                        31
356800110601     C                   MOVEL     'S'           WRIEM
356900110601     c                   if        v2cben='?'
357000110601     c                   write     ls05t01
357100110601     c                   write     ls05d02
357200110601     c                   endif
357300110601     c                   endif
357400091125     c* verifico se inserito '?' su beneficiario C/A
357500091125     c                   if        v2cben='?'
357600091125     c                   exsr      sr_bencas
357700091125     C                   SETON                                        31
357800091125     C                   MOVEL     'S'           WRIEM
357900091125     c                   endif
358000940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
358100940923     C     V2IGCA        IFEQ      '?'
358200991022     C                   CLEAR                   DS7PQRS
358300941003     C                   MOVEL     'S'           DS7RIC
358400941003     C                   MOVEL     'V'           DS7GES
358500941003     C                   MOVEL     V2CGCA        DS7COD
358600991022     C                   MOVEL     DS7PQRS       KPJBU
358700960126     C                   CALL      'TRTB72R'                            31
358800941003     C                   PARM                    KPJBA
358900940923     C                   MOVEL     *BLANKS       V2IGCA
359000940923     C                   SETON                                        31
359100940923     C                   MOVEL     'S'           WRIEM
359200940923     C                   ENDIF
359300940923     C*
359400940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
359500940923     C     V2IGGA        IFEQ      '?'
359600991022     C                   CLEAR                   DS7PQRS
359700941003     C                   MOVEL     'S'           DS7RIC
359800941003     C                   MOVEL     'V'           DS7GES
359900941003     C                   MOVEL     V2CGGA        DS7COD
360000991022     C                   MOVEL     DS7PQRS       KPJBU
360100960126     C                   CALL      'TRTB69R'                            31
360200941003     C                   PARM                    KPJBA
360300940923     C                   SETON                                        31
360400940923     C                   MOVEL     'S'           WRIEM
360500940923     C                   MOVEL     *BLANKS       V2IGGA
360600940923     C                   ENDIF
360700940923     C*
360800940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
360900940923     C     V2IGMA        IFEQ      '?'
361000991022     C                   CLEAR                   DS7PQRS
361100941003     C                   MOVEL     'S'           DS7RIC
361200941003     C                   MOVEL     'V'           DS7GES
361300941003     C                   MOVEL     V2CGMA        DS7COD
361400991022     C                   MOVEL     DS7PQRS       KPJBU
361500960126     C                   CALL      'TRTB70R'                            31
361600941003     C                   PARM                    KPJBA
361700940923     C                   SETON                                        31
361800940923     C                   MOVEL     'S'           WRIEM
361900940923     C                   MOVEL     *BLANKS       V2IGMA
362000940923     C                   ENDIF
362100940923     C*
362200940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
362300940923     C     V2IGVA        IFEQ      '?'
362400991022     C                   CLEAR                   DS7PQRS
362500941003     C                   MOVEL     'S'           DS7RIC
362600941003     C                   MOVEL     'V'           DS7GES
362700941003     C                   MOVEL     V2CGVA        DS7COD
362800991022     C                   MOVEL     DS7PQRS       KPJBU
362900960126     C                   CALL      'TRTB73R'                            31
363000941003     C                   PARM                    KPJBA
363100940923     C                   SETON                                        31
363200940923     C                   MOVEL     'S'           WRIEM
363300941003     C                   MOVEL     *BLANKS       V2IGVA
363400940923     C                   ENDIF
363500940923     C*
363600940923     C                   ENDSR
363700091125      **************************************************************************
363800091125      *   CONTROLLI SUL FORMATO VIDEO D02
363900091125      **************************************************************************
364000091125     C     sr_bencas     BEGSR
364100091125     c                   clear                   v2cben
364200091214     c                   clear                   wdaben
364300091214     c                   clear                   wdasrsc
364400091214     c                   clear                   wdascdi
364500091216     c                   clear                   trulintds
364600091216     c
364700091216     c                   eval      iinttip='B'
364800091216     c                   eval      iinttic=v2ctic
364900091216     c                   eval      iintrscm=v2crsm
365000091216     c                   eval      iintrsco=v2crmo
365100091216     c                   eval      iintCDI =w_cdi
365200091216     c                   call      'TRULINTR'
365300091216     c                   parm                    trulintds
365400091216     c
365500091216     c                   movel     ointrsci      wdasrsc
365600091216     c
365700091216     c                   select
365800091216     c                   when      ointtipi='O'
365900091216     c                   eval      wdaben='MITTENTE  ORIGINALE'
366000091216     c
366100091216     c                   when      ointtipi='R'
366200091216     c                   eval      wdascdi=w_cdi
366300091216     c                   eval      wdaben=' O R D I N A N T E'
366400091216     c
366500091216     c                   when      ointtipi='M'
366600091216     c                   if        v2ccdm>0
366700091216     c                   movel     v2ccdm        wdascdi
366800091216     c                   endif
366900091216     c                   eval      wdaben='  M I T T E N T E'
367000091216     c                   ENDSL
367100091214     c
367200091125     c                   exfmt     ls05wdas
367300091125     c                   endsr
367400110601      **************************************************************************
367500110601      *   emissione window email destinatario
367600110601      **************************************************************************
367700110601     C     sr_emdest     BEGSR
367800110601     c                   clear                   v2cemw
367900110601     c
368000110601     c                   eval      ilv79TLA='L'
368100110601     c                   eval      ilv79Aas=blpaas
368200110601     c                   eval      ilv79Lnp=blplnp
368300110601     c                   eval      ilv79nrs=blpnrs
368400110601     c                   eval      ilv79nsp=blpnsp
368500110601     c                   call      'FNLV79R'
368600110601     c                   parm                    fnlv79ds
368700110601     c
368800110601     c                   endsr
368900130912      **************************************************************************
369000130912      *   window per visualizzare gli orari servizi
369100130912      **************************************************************************
369200130912     C     sr_OrariSr    BEGSR
369300130920
369400140131     c                   clear                   trulorsds
369500140131     c                   eval       IORefil = BLPlna
369600140131     c                   eval       IORecap = BLPcad
369700140131     c                   eval       IOReloc = BLPlod
369800140131     c                   eval       IORenar = BLPnzd
369900140131     c
370000140131     c                   eval       IORetser = 'C'
370100140131     c                   eval       IORetsp = BLPtsp
370200140131     c                   eval       IOREtfp = blptfp
370300140131     c                   eval       IOREtfa = blptfa
370400140131     c                   eval       IOREdti = blpdti
370500140131     c                   eval       IOREhti = blphti
370600140203     c                   if         wesarb='S'
370700140203     c                   eval       IOREdti = arbdti
370800140203     c                   eval       IOREhti = arbhti
370900140203     c                   endif
371000140131     c                   eval       IOREdcr = blpdcr
371100140131     c                   eval       IOREhcr = blphcr
371200140131     c                   eval       IOREtcr = blptcr
371300140131     C*
371400140131     C                   MOVEL     ' '           D98TLA
371500140131     C                   MOVEL     'P'           D98TBO
371600140131     C                   MOVEL     BLPAAS        D98AAS
371700140131     C                   MOVEL     BLPLNP        D98LNP
371800140131     C                   MOVEL     BLPNRS        D98NRS
371900140131     C                   MOVEL     BLPNSP        D98NSP
372000140131     C*
372100140131     C                   CALL      'TNSD99R'
372200140131     C                   PARM                    TNSD99DS
372300140203     c
372400140131     c                   eval       IOREdei = d98dei
372500140131     c                   eval       IOREoei = d98oei
372600140131     c*****              eval       IORedta = BLPaas * 10000 + BLPmgs
372700140203     c
372800140131     c* Data di Riferimento: data consegna se consegnata
372900140131     c                   select
373000140131     c                   when      blpdcm>0
373100140131     c                   eval      ioredta=blpdcm
373200140131     c                   other
373300140131     c* altrimenti data prevista consegna se > = oggi
373400140131     C                   time                    wrkoggiiso
373500140131     C     *iso          move      wrkoggiiso    ioredta
373600140131     c* se < di oggi, imposto oggi
373700140131     c                   if        d98dce>ioredta
373800140131     c                   eval       ioredta=d98dce
373900140131     c                   endif
374000140131     c                   endsl
374100140203     c* Se in distinta leggo i dati da FNARB
374200140203     c                   if        blpdcm=0 and wesarb='S'
374300140203     c                   if         ARBfbc = *blanks and
374400140203     c                               ARBddc > 0 and
374500140203     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
374600140203     c                               arbica='R' or arbicc=' ' or arbicc='R')
374700140203     c                   eval       IOREcons = 'S'
374800140203     c                   eval       IOREDDC=arbddc
374900140307     c                   eval       IORENDC=arbndc
375000140224     c* Se in distinta, passo l'orario presunto di consegna
375100140224     c                   move      arbndc        pctndc
375200140224     c                   move      arbifp        pctfgs
375300140224     c                   move      'COR'         ttrd
375400140224     c     kcor          setgt     fipct02l
375500140224     c     kcor          readpe    fipct02l
375600140310     c                   if        not %eof(fipct02l)
375700140224     c                   movel     pctdati       fipctcords
375800140307     c                   eval      ioreorp=§PCTORASTI
375900140224     c                   endif
376000140224     c
376100140203     c                   ENDIF
376200140203     c                   ENDIF
376300140131     C*
376400130920
376500140131     c                   call      'TRULORSR'
376600130912     c                   parm                    kpjba
376700140131     c                   parm                    trulorsds
376800130912     c
376900130912     c                   endsr
377000940810      **************************************************************************
377100940810      *   R O U T I N E     I N I Z I A L E
377200940810      **************************************************************************
377300940810     C     *INZSR        BEGSR
377400940810     C*
377500940810     C* ACCESSO   FILE BOLLE VISTA LOGICA 1
377600940810     C     KBLP3         KLIST
377700940810     C                   KFLD                    PA0AAS
377800940810     C                   KFLD                    PA0LNP
377900940810     C                   KFLD                    PA0NRS
378000940810     C                   KFLD                    PA0NSP
378100021218     C*
378200021218     C     KAR51         KLIST
378300021218     C                   KFLD                    KAAS
378400021218     C                   KFLD                    KLNP
378500021218     C                   KFLD                    KNRS
378600021218     C                   KFLD                    KNSP
378700021218     C                   KFLD                    KTRd              3
378800021218     C*
378900991022     C     KAR6          KLIST
379000991022     C                   KFLD                    PA0AAS
379100940808     C                   KFLD                    PA0LNP
379200940808     C                   KFLD                    PA0NRS
379300940808     C                   KFLD                    PA0NSP
379400991022     c                   KFLD                    KTRC
379500940810     C* ACCESSO   FILE LEGAME BOLLA
379600940810     C     KLBL1         KLIST
379700940810     C                   KFLD                    LBLAAN
379800940810     C                   KFLD                    LBLLPN
379900940810     C                   KFLD                    LBLNRN
380000940810     C                   KFLD                    LBLNSN
380100940906     C     KLBL3         KLIST
380200940906     C                   KFLD                    KAAS
380300940906     C                   KFLD                    KLNP
380400940906     C                   KFLD                    KNRS
380500940906     C                   KFLD                    KNSP
380600060216     C     Kar41         KLIST
380700940805     C                   KFLD                    KAAS
380800940805     C                   KFLD                    KLNP
380900940805     C                   KFLD                    KNRS
381000940805     C                   KFLD                    KNSP
381100940805     C                   KFLD                    KTRC
381200060216     C     Kar42         KLIST
381300951219     C                   KFLD                    KAAS
381400951219     C                   KFLD                    KLNP
381500951219     C                   KFLD                    KNRS
381600951219     C                   KFLD                    KNSP
381700940809     C     KTAB1         KLIST
381800940804     C                   KFLD                    CODUT
381900940804     C                   KFLD                    KCOD
382000940804     C                   KFLD                    KKEY
382100940809     C     KTAB2         KLIST
382200940804     C                   KFLD                    CODUT
382300940804     C                   KFLD                    KCOD
382400940808     C     KIND1         KLIST
382500940808     C                   KFLD                    CODUT
382600940808     C                   KFLD                    KKCC
382700940808     C                   KFLD                    KKSC
382800021203     C     Kfapd         klist
382900021203     C                   kfld                    kpdtip
383000021203     C                   kfld                    kpdpdr
383100040220      * File FITGD02L
383200040220     c     K02TGD02      klist
383300040220     c                   kfld                    TGDtor
383400040220     c                   kfld                    TGDogg
383500040713      * Fnorm
383600040713     c     korm          klist
383700040713     c                   kfld                    §b4poe
383800040713     c                   kfld                    §b4nsr
383900040713     c                   kfld                    §b4nor
384000040713     c                   kfld                    §b4nrv
384100140224     C     Kcor          KLIST
384200140224     C                   KFLD                    pctfgs
384300140224     C                   KFLD                    pctNDC
384400140224     C                   KFLD                    arbpdc
384500140224     C                   KFLD                    TTRD              3
384600140224     C                   KFLD                    arbAAS
384700140224     C                   KFLD                    arbLNP
384800140224     C                   KFLD                    arbNRS
384900140224     C                   KFLD                    arbNSP
385000940804     C*
385100940810     C                   MOVEL     *ZERO         COMLPN            3
385200940810     C                   MOVEL     *ZERO         COMNRN            2
385300940810     C                   MOVEL     *ZERO         COMNSN            7
385400950306     C                   MOVEL     *ZERO         COMLAN            3
385500940804     C*
385600940810     C                   Z-ADD     *ZERO         NRR8              4 0
385700940810     C                   Z-ADD     *ZERO         NRR9              4 0
385800940906     C                   Z-ADD     *ZERO         SAVNR8            4 0
385900940906     C                   Z-ADD     *ZERO         SAVNR9            4 0
386000940808     C                   MOVEL     *BLANKS       WF03              1
386100940808     C*
386200940804     C                   ENDSR
386300940804      **************************************************************************
386400941011**   SEQ
386500910912NON SEQUENZIALI
386600950316**        12345678901234567890123456789012345678901234567890
386700950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
386800920326**
386900950316LEGATA A PIU' BOLLE                       - F18=Visualizza
387000920326**
387100950316 Non Presente
387200950316 F18=Visualizza
387300940906Vedi in Int.Par
387400940907Vedi in Int.Arr
387500941011Vedi arch.stor.
387600920326**
387700920326Spedizione non Presente
387800941230
387900940907Vedi in Int.Bolle Part.
388000940907Vedi in Int.Bolle Arr.
388100941011Vedi in Archiv. Storico
388200040220** TF                                                                        *
388300040610F1=Agg.Rich.#
388400021218** TF                                                                        *
388500021218F2=C.A.#
388600021218** TF                                                                        *
388700021218F4=Tass2#
388800021218** TF                                                                        *
388900021218F5=Eventi#
389000021218** TF                                                                        *
389100021218F7=Anomalie#
389200021218** TF                                                                        *
389300021218F8=Orm#
389400130912** TF                                                                        *
389500130912F9=OrariSr#
389600130912** TF                                                                        *
389700021218F10=Variaz.#
389800021218** TF                                                                        *
389900030121F11=NoteS/B#
390000021218** TF                                                                        *
390100021218F12=Ritorno#
390200021218** TF                                                                        *
390300021218F13=Tassaz.#
390400021218** TF                                                                        *
390500021218F14=Giacenza#
390600040220** TF                                                                        *
390700040610F16=Gest.Rich.#
390800021218** TF                                                                        *
390900021218F19=Spunte#
391000021218** TF                                                                        *
391100021218F20=Affid.#
391200021218** TF                                                                        *
391300021218F21=Dett.Colli#
391400021218** TF                                                                        *
391500051201F22=C/Assegni#
