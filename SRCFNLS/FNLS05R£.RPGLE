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
025600150728     D diore01       E DS
025700930505     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
025800991022     D TNSD99DS      E DS                  INZ
025900900502     D DS3A          E DS
026000130319     D*DS1B          E DS
026100991022     D DS$2          E DS
026200991022     D DSCV          E DS
026300020402     D DSCC          E DS
026400111020     D**!!!DS3K          E DS
026500951219     D DS7K          E DS
026600090608     d ds5e          e ds
026700991022     D DS7PQRS       E DS
026800991022     D FNLG30DS      E DS
026900991022     D FNLR44DS      E DS
027000991022     D FNLV55DS      E DS
027100051124     D FNLV80DS      E DS
027200991022     D FIDN00DS      E DS
027300091216     D trulintds     E DS
027400110601     D fnlv79ds      E DS
027500991025     d TIBS69DS      E DS                  INZ
027600991025     d DS_cnaco      E DS                  extname(CNACO00F)
027700991025     d DS_cnind      E DS                  extname(CNIND00F)
027800991025     d DS_cnclp      E DS                  extname(CNCLP00F)
027900991025     d DS_fncls      E DS                  extname(FNCLS00F)
028000021218     d FNLRB3Ds      E DS
028100030121     d dTba          e ds
028200031119     d dAr5Gen       e ds
028300030121     d dAr5Ban       e ds
028400030703     d dAr5Bnb       e ds
028500130927     d dAr5emd       e ds
028600030121     d Tibs02Ds      e ds
028700110705     d dsbl4a        e ds
028800130920
028900140131     d TRULORSds     e ds
029000140224     d fipctcords    e ds
029100130920
029200040714      * ds per il controllo della presenza di CA per la spedizione richiesta
029300040714     d FIDN12DS      e ds
029400040714     d  skKey                 26    305    dim(20)
029500040714     d  skAnn                306    325    dim(20)
029600040714     d  skDca                326    485  0 dim(20)
029700040714     d  skFca                486    545  0 dim(20)
029800040714     d  skDch                546    705  0 dim(20)
029900040714     d  skCch                706    745    dim(20)
030000040714
030100040714     d dsKey           ds
030200040714     d  dsaac                         4  0
030300040714     d  dsfil                         3  0
030400040714     d  dsnca                         7  0
030500040220      *
030600040220      *   C O S T A N T I
030700040220      *
030800971212     D CMANUA          C                   CONST('Inser.MANUALE in ')
030900971212     D CAUTOM          C                   CONST('Attribuito in xxxx')
031000971212     D CSTAMP          C                   CONST(' Stampa XX')
031100040220      *
031200040220      *   V A R I A B I L I
031300040220      *
031400040220     D kpdpdr          S                   LIKE(APDpdr)
031500040220     D kpdtip          S                   LIKE(APDtip)
031600041028     D wdam            S                   LIKE(blpdam)
031700041028     D wdcm            S                   LIKE(blpdcm)
031800041028     D wdce            S                   LIKE(blpdce)
031900041028     D wnrc            S                   LIKE(blpnrc)
032000041028     D wdcp            S                   LIKE(blpdcp)
032100041028     D wcca            S                   LIKE(blpcca)
032200041028     D whmc            S                   LIKE(blphmc)
032300041028     D wtmc            S                   LIKE(blptmc)
032400041028     D wdet            S                   LIKE(blpdet)
032500041028     D wdut            S                   LIKE(blpdut)
032600041028     D wddc            S                   LIKE(blpddc)
032700041028     D wndc            S                   LIKE(blpndc)
032800040220
032900040220     d wtba            s                   Like(§Ar5Tba)
033000040220     d wdestba         s                   Like(V1dTba)
033100991022     D DATA_eur        S               D   DATFMT(*eur)
033200140131     D wrkoggiiso      s               d
033300991022     D W0080           S              8  0
033400991025     D W0040           S              4  0
033500991022     D Wsvn            S                   LIKE(AR7svn)
033600991022     D Wvan            S                   LIKE(AR7van)
033700991022     D WDIV            S                   LIKE(AR6div)
033800991022     D KTRC            S                   LIKE(AR6TRC)
033900111020     D**!!!$3kinb          S                   LIKE(§3kinb)
034000090603     D** $3kctb          S                   LIKE(§3kctb)
034100020225     d lnantw          S                   LIKE(§ogntw)
034200020225     d lnpntw          S                   LIKE(§ogntw)
034300991022     D KAAS            S                   LIKE(BLPaas)
034400991022     D Klnp            S                   LIKE(BLPlnp)
034500991022     D Knrs            S                   LIKE(BLPnrs)
034600991022     D Knsp            S                   LIKE(BLPnsp)
034700991022     D Kcod            S                   LIKE(TBLcod)
034800991022     D Kkey            S                   LIKE(TBLkey)
034900991022     D Kkcc            S                   LIKE(INDkcc)
035000991022     D Kksc            S                   LIKE(INDksc)
035100991022     D Comaan          S                   LIKE(LBLaan)
035200991022     D Wtfp            S                   LIKE(D55tfp)
035300991022     D Wtstdec         S                   LIKE(§CVfdc)
035400091125     D W_cdi           S                   LIKE(§ar5cdi)
035500991025     D wtibs69r        s              1
035600991022     D W017A           s             17
035700991022     D w0130           s             13  0
035800991022     D w0133           s             13  3
035900991022     D IMPeditato      s             13
036000131007     d wx              s              3  0
036100131007     d wy              s              3  0
036200131007     d wabif9          s              1
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
052700140319     c* Tasto funz orari servizi sempre abilitato
052800131007     c                   eval      wabif9='S'
052900921013     C*
053000900511     C* INIZIALIZZO CAMPI ANCHE PER MEMORIZZARE BOLLA CHE NON USO
053100900427     C                   Z-ADD     0             BLPNRS
053200900427     C                   Z-ADD     0             BLPNSP
053300900427     C                   Z-ADD     0             BLPLNA
053400900427     C                   Z-ADD     0             BLPRMN
053500930701     C                   CLEAR                   PARAM1
053600930701     C                   CLEAR                   PARAM2
053700930701     C                   CLEAR                   PARAM4
053800940810     C                   CLEAR                   PARA64
053900940810     C                   CLEAR                   PARAB5
054000991022      *----------------------------------------------------------
054100991022      * CARICO BOLLA
054200900516     C                   EXSR      CARBLP
054300940923     C*
054400940923     C* SE LA BOLLA E' INESISTENTE VADO A FINE PROGRAMMA
054500940923     C     PA0NSP        IFEQ      *ZERO
054600940923     C                   GOTO      FINE
054700940923     C                   ENDIF
054800040220      *
054900040220      *?Abilito F1=Aggiunta in Reclami?
055000040220      * (se richiamato da pgm. FIDNA1R...)
055100040220     c                   eval      *in27  = *off
055200040303     c                   if            SDSprm >  1
055300040308     c                             and IA1ins = ' '
055400040220     c                   eval      *in27  = *on
055500040220     c                   endif
055600040220      *
055700040220      *?Abilito F16=Visualizzazione Reclami?
055800040220      * (se richiamato da pgm. FIDNA1R...)
055900040220     c                   eval      TGDtor  = 'S'
056000040220     c                   eval      TGDogg  = ds_OggSped
056100040220     c     K02TGD02      setll     FITGD000
056200040302     c                   eval      *in28   = (%equal(FITGD02L)   and
056300040303     c                                           (SDSprm =  1
056400040303     c                                        or (SDSprm >  1
056500040303     c                                        and IA1ins <> 'N')))
056600900427     C*
056700931115     C* SOLO SE C'E' CONTRASSEGNO VISUALIZZO IL TASTO CMD22
056800931116     C* VERIFICO NELL'ARCHIVIO SE C'E' CONTRASSEGNO
056900990118     C                   SETOFF                                       2412
057000011120     C     KBLP3         SETLL     FNCSB01L
057100011120     c                   if        %equal(fncsb01l)
057200051115     C     KBLP3         SETLL     Fiar901L
057300051115     c                   if        %equal(fiar901l)
057400011120     c                   eval      *in24 = *on
057500011120     c                   endif
057600011120     c                   endif
057700990118     C* CONTROLLO SE C'E' DANNO
057800990125     C                   EXSR      CTRDAN
057900940322      *
058000940802      * EMETTO IL FORMATO DI RIEPILOGO DELLA BOLLA
058100940802     C                   EXSR      RIEPIL
058200940802      *
058300940802      * CMD 3 - FINE LAVORO TORNO A SUBFILE
058400940808     C     *INKC         IFEQ      *ON
058500940808     C     WF03          ORNE      *BLANKS
058600941109     C                   MOVEL     '1'           PA0F03
058700940808     C                   GOTO      FINE
058800940808     C                   ENDIF
058900940802      *
059000940802      * PER F15 VISUALIZZO TUTTA LA BOLLA, PER INVIO ESCO
059100940802     C  NKP              GOTO      FINE
059200931115     C*
059300931115     C                   SETOFF                                       25
059400940804     C                   MOVEL     'S'           WRIEM
059500021218      *
059600021218      * Riempio il campone in fondo alla videata con i tasti scelti
059700021218     C                   exsr      Ges_TastiFun
059800021218     c                   eval      FD01 = FDxx
059900021218     c                   eval      F24D01 = F24Dxx
060000900427     C     FOR01         TAG
060100940804     C     WRIEM         IFEQ      'S'
060200940804     C                   WRITE     LS05T01
060300940804     C                   MOVEL     *BLANKS       WRIEM
060400940804     C                   ENDIF
060500940804     C*
060600021218      *
060700021218      * Imposto la stringa comandi del 'giro' successivo
060800021218     C                   IF        *inKY = *on and $Loop > 1
060900021218     c                   Exsr      Ges_F24
061000021218     c                   eval      FD01 = FDxx
061100021218     c                   eval      F24D01 = F24Dxx
061200021218     C                   ENDIF
061300021218      *
061400940804     C                   EXFMT     LS05D01
061500940808     C                   MOVEL     *BLANKS       WF03
061600931115      *
061700990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
061800931115     C   KY*IN25         IFEQ      *ON
061900931115     C                   SETOFF                                       25
062000931115     C                   ELSE
062100931115     C                   SETON                                        25
062200931115     C                   ENDIF
062300931115     C   KY              GOTO      FOR01
062400910806     C*
062500900530     C* CMD 3 - FINE LAVORO TORNO A SUBFILE
062600900530     C   KC              GOTO      FINE
062700921013     C*
062800921013     C* CMD12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
062900921013     C   KL              GOTO      FINE
063000911105     C*
063100921013     C* TASTI DI FUNZIONE VARI
063200921013     C                   EXSR      COMAND
063300940808     C*
063400940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
063500940808     C     WF03          IFNE      *BLANKS
063600941109     C                   MOVEL     '1'           PA0F03
063700940808     C                   GOTO      FINE
063800940808     C                   ENDIF
063900940808     C*
064000921013     C   31              GOTO      FOR01
064100921013     C*
064200900427     C* SE TUTTO OK EMETTO FORMATO2
064300931115     C                   SETOFF                                       25
064400940804     C                   MOVEL     'S'           WRIEM
064500021218      *
064600021218      * Riempio il campone in fondo alla videata con i tasti scelti
064700021218     C                   exsr      Ges_TastiFun
064800021218     c                   eval      FD02 = FDxx
064900021218     c                   eval      F24D02 = F24Dxx
065000900427     C     FOR02         TAG
065100940804     C     WRIEM         IFEQ      'S'
065200940804     C                   WRITE     LS05T01
065300940804     C                   MOVEL     *BLANKS       WRIEM
065400940804     C                   ENDIF
065500940804     C*
065600021218      * Imposto la stringa comandi del 'giro' successivo
065700021218     C                   IF        *inKY = *on and $Loop > 1
065800021218     c                   Exsr      Ges_F24
065900021218     c                   eval      FD02 = FDxx
066000021218     c                   eval      F24D02 = F24Dxx
066100021218     C                   ENDIF
066200021218      *
066300940804     C                   EXFMT     LS05D02
066400940808     C                   MOVEL     *BLANKS       WF03
066500931115      *
066600990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
066700931115     C   KY*IN25         IFEQ      *ON
066800931115     C                   SETOFF                                       25
066900931115     C                   ELSE
067000931115     C                   SETON                                        25
067100931115     C                   ENDIF
067200931115     C   KY              GOTO      FOR02
067300910806     C*
067400900427     C* CMD12 - RITORNO VIDEATA 1
067500900427     C   KL              GOTO      FOR01
067600940923     C*
067700940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
067800940923     C                   EXSR      CTRD02
067900940923     C   31              GOTO      FOR02
068000921013     C*
068100921013     C* TASTI DI FUNZIONE VARI
068200921013     C                   EXSR      COMAND
068300940808     C*
068400940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
068500940808     C     WF03          IFNE      *BLANKS
068600941109     C                   MOVEL     '1'           PA0F03
068700940808     C                   GOTO      FINE
068800940808     C                   ENDIF
068900921013     C   31              GOTO      FOR02
069000920429     C*
069100910806     C*
069200930112     C* SE ESISTONO DATI ENTRO IN TERZA VIDEATA
069300931115     C                   SETOFF                                       25
069400940804     C                   MOVEL     'S'           WRIEM
069500021218      *
069600021218      * Riempio il campone in fondo alla videata con i tasti scelti
069700021218     C                   exsr      Ges_TastiFun
069800021218     c                   eval      FD03 = FDxx
069900021218     c                   eval      F24D03 = F24Dxx
070000930112     C     FOR03         TAG
070100940804     C     WRIEM         IFEQ      'S'
070200940804     C                   WRITE     LS05T01
070300940804     C                   MOVEL     *BLANKS       WRIEM
070400940804     C                   ENDIF
070500940804     C*
070600951219     C                   Z-ADD     1             REC3
070700021218      *
070800021218      * Imposto la stringa comandi del 'giro' successivo
070900021218     C                   IF        *inKY = *on and $Loop > 1
071000021218     c                   Exsr      Ges_F24
071100021218     c                   eval      FD03 = FDxx
071200021218     c                   eval      F24D03 = F24Dxx
071300021218     C                   ENDIF
071400951219     C                   WRITE     LS05Z03
071500951219     C                   EXFMT     LS05C03
071600940808     C                   MOVEL     *BLANKS       WF03
071700931115      *
071800990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
071900931115     C   KY*IN25         IFEQ      *ON
072000931115     C                   SETOFF                                       25
072100931115     C                   ELSE
072200931115     C                   SETON                                        25
072300931115     C                   ENDIF
072400931115     C   KY              GOTO      FOR03
072500910806     C*
072600900507     C* CMD12 - RITORNO VIDEATA 2
072700900507     C   KL              GOTO      FOR02
072800930114     C*
072900930114     C* TASTI DI FUNZIONE VARI
073000930114     C                   EXSR      COMAND
073100940808     C*
073200940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
073300940808     C     WF03          IFNE      *BLANKS
073400941109     C                   MOVEL     '1'           PA0F03
073500940808     C                   GOTO      FINE
073600940808     C                   ENDIF
073700930114     C   31              GOTO      FOR03
073800900504     C*
073900000000     C     FINE          TAG
074000971202     C*
074100971202     C                   MOVEL     'C'           D55TLA
074200971202     C                   CALL      'FNLV55R'
074300991022     C                   PARM                    FNLV55DS
074400991025      *
074500991025     C                   IF        Wtibs69r  <>  *BLANKS
074600991025     C                   eval      I69TLA  = 'C'
074700991025     C                   call      'TIBS69R'
074800991025     C                   parm                    TIBS69DS
074900991025     c                   ENDIF
075000040714     C*
075100040714     C                   MOVEL     'C'           I12TLA
075200040714     C                   CALL      'FIDN12R'
075300040714     C                   PARM                    FIDN12DS
075400091216     C*
075500091216     C                   MOVEL     'C'           Iinttla
075600091216     C                   CALL      'TRULINTR'
075700091216     C                   PARM                    trulintds
075800040714      *
075900991025      *
076000930614     C* REIMPOSTO KPJBU
076100940810     C   KC              MOVEL     '1'           PA0F03
076200940804     C                   MOVEL     PARAM0        KPJBU
076300930614     C*
076400000000     C                   SETON                                        LR
076500940808     C**************************************************************************
076600940808     C*    COMANDI UGUALI NELLE 2 VIDEATE
076700940808     C**************************************************************************
076800921013     C     COMAND        BEGSR
076900931115     C*
077000921013     C                   SETOFF                                       31
077100040220      *
077200040220      *?F1 - Aggiunta bolla in reclamo?
077300040220if  1c                   if        *inKA   = *on
077400040220     c                   clear                   FIDNA1ds
077500040220     c                   eval      IA1ins  = 'S'
077600040220     c                   eval      IA1tor  = 'S'
077700040220     c                   eval      IA1ogg  = ds_OggSped
077800040220     c                   goto      Fine
077900040220     c*****              return
078000040220e   1c                   endif
078100040220      *
078200990118     C* F5 - INTERROGAZIONE EVENTI
078300040220if  1C     *INKE         IFEQ      *ON
078400930526     C                   SETON                                        31
078500941213     C                   CLEAR                   PARAB5
078600940804     C                   MOVEL     PA0AAS        PB5AAS
078700940804     C                   MOVEL     PA0LNP        PB5LNP
078800940804     C                   MOVEL     PA0NRS        PB5NRS
078900940804     C                   MOVEL     PA0NSP        PB5NSP
079000940804     C                   MOVEL     'FNLS05R'     PB5PGM
079100940804     C                   MOVEL     V1CRSU        PB5DSA
079200940808     C                   MOVEL     V1CDSP        PB5DSP
079300940804     C                   MOVEL     V2CRMN        PB5RMN
079400940804     C                   MOVEL     V1CLNA        PB5LNA
079500940804     C                   MOVEL     V1DLNA        PB5LN1
079600940804     C                   MOVEL     V2CRSD        PB5RSD
079700940804     C                   MOVEL     V3CDCP        PB5DCP
079800940804     C                   MOVEL     V3CTMC        PB5TMC
079900940804     C                   MOVEL     PARAB5        KPJBU
080000940804     C                   CALL      'FNLRB5R'
080100930526     C                   PARM                    KPJBA
080200940808     C                   MOVEL     '1'           PA0B5
080300940808     C                   MOVEL     'S'           WRIEM
080400040220e   1C                   ENDIF
080500930526     C*
080600921013     C* CMD 7 - VISUALIZZA ANOMALIE
080700040220if  1C     *INKG         IFEQ      *ON
080800991022     C                   CLEAR                   FNLR44DS
080900960523     C                   MOVEL     PA0AAS        D44AAS
081000960523     C                   MOVEL     PA0LNP        D44FIL
081100960523     C                   MOVEL     PA0NRS        D44NRS
081200960523     C                   MOVEL     PA0NSP        D44NSP
081300960523     C                   MOVEL     'P'           D44TBO
081400960523     C                   MOVE      'I'           D44MOD
081500960523     C                   SETON                                        31
081600991022     C                   MOVEL     FNLR44DS      KPJBU
081700960523     C                   CALL      'FNLR44R'
081800960523     C                   PARM                    KPJBA
081900940808     C                   MOVEL     'S'           WRIEM
082000040220e   1C                   ENDIF
082100010220     C*
082200010220     C* CMD 8 - VISUALIZZA ORM
082300040220if  1C     *INKH         IFEQ      *ON
082400010220
082500010220     C                   clear                   PARM01
082600010220     C                   move      v1cpoe        ppoe
082700010220     C                   move      v1cnor        pnor
082800010220     C                   move      v1cnsr        pnsr
082900010220     C                   move      v1cnrv        pnrv
083000010220     C                   move      '5'           psce
083100010220     C                   move      v1clnp        pfgs
083200010220     C                   movel(p)  PARM01        KPJBU
083300010220
083400010220     C                   call      'FIOR07R'
083500010220     C                   parm                    KPJBA
083600010220     C                   MOVEL     'S'           WRIEM
083700010220     C                   SETON                                        31
083800010220
083900040220e   1C                   ENDIF
084000130912c
084100130912     c* F9 - Orari servizi
084200130912     c                   if        *inki=*on
084300131007     c                   if        wabif9='S'
084400130930     c                   exsr      sr_orarisr
084500131007     c                   endif
084600130927     C                   MOVEL     'S'           WRIEM
084700130927     C                   SETON                                        31
084800130912e   1C                   ENDIF
084900021218     C*
085000021218     C* F11 - Note supermercati
085100040220if  1C     *INKK         IFEQ      *ON
085200021218     C     *IN77         ANDEQ     *ON
085300021218      * imposto i campi 'bolla' della ds
085400021218     C                   clear                   FnLrb3ds
085500030122     c                   Movel     v1cRsu        Rb3Rsu
085600030121     C                   z-add     SimFel        RB3Fil
085700021218     C                   z-add     V1CAAS        RB3AAS
085800030121     C                   z-add     V1CLP1        RB3LNP
085900021218     C                   z-add     V1CNRS        RB3NRS
086000021218     C                   z-add     V1CNSP        RB3NSP
086100021218     C                   z-add     V1CDSP        RB3DSP
086200021218     C                   z-add     BLPRMN        RB3RMN
086300021218     C                   z-add     V1CLNA        RB3LNA
086400021218     C                   movel     BLPRSD        RB3RSD
086500021218     C                   z-add     V1CPKF        RB3PKF
086600021218     C                   z-add     V1CNCL        RB3NCL
086700021218     C                   z-add     V1CVLF        RB3VLB
086800030703      * bancali - part.varie "B"
086900040220If  2c                   If        %subst(BlpGva:1:1) = 'B'
087000030121     C                   z-add     §Ar5Tba       RB3Tba
087100030121     C                   z-add     V1cNba        RB3Nba
087200030121     C                   z-add     §Ar5Tb2       RB3Tb2
087300030121     C                   z-add     V1cNb2        RB3Nb2
087400030703     c                   Movel     V2cGva        Rb3Gva
087500040220e   2c                   EndIf
087600030703      * bancali - part.varie "O"
087700040220If  2c                   If        %subst(BlpGva:1:1) = 'O'
087800030703     C                   z-add     §Ar5bTba      RB3Tba
087900030703     C                   z-add     V1cNba        RB3Nba
088000030703     C                   z-add     §Ar5bTb2      RB3Tb2
088100030703     C                   z-add     V1cNb2        RB3Nb2
088200030703     c                   Movel     V2cGva        Rb3Gva
088300030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
088400030703     c                   Z-Add     §Ar5bCor      Rb3Cor
088500040220e   2c                   EndIf
088600021218      * richiamo PGM per note supermercati
088700021218     C                   call      'FNLRB3R'
088800030122     c                   Parm                    Kpjba
088900030121     C                   PARM                    Fnlrb3Ds
089000021218      *
089100030703     c                   Eval      *In31 = *On
089200021218     C                   MOVEL     'S'           WRIEM
089300040220e   1C                   ENDIF
089400921013     C*
089500921013     C* CMD14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
089600040220if  1C     *INKN         IFEQ      *ON
089700930701     C     *IN03         ANDEQ     *ON
089800941213     C     PA0GIA        ANDEQ     *BLANK
089900991022     C                   CLEAR                   FNLG30DS
090000941213     C                   MOVEL     'P'           D30POA
090100941213     C                   MOVEL     PA0AAS        D30AAS
090200941213     C                   MOVEL     PA0LNP        D30LNP
090300941213     C                   MOVEL     PA0NRS        D30NRS
090400941213     C                   MOVEL     PA0NSP        D30NSP
090500991022     C                   MOVEL     FNLG30DS      KPJBU
090600930701     C                   SETON                                        31
090700941213     C                   CALL      'FNLG30C'
090800921013     C                   PARM                    KPJBA
090900940808     C                   MOVEL     'S'           WRIEM
091000040220e   1C                   ENDIF
091100040220      *
091200040220      *?F16 - Gestione reclami?
091300040226if  1c                   if        *inKQ   = *on
091400040226     c                   reset                   FIDNA2ds
091500040226if  2c                   if        SDSprm  > 1
091600040226     c                   eval      IA2mod  = 'I'
091700040226e   2c                   endif
091800040226     c                   eval      IA2ogg  = ds_OggSped
091900040226     c                   call      'FIDNA2R'
092000040226     c                   parm                    KPJBA
092100040226     c                   parm                    FIDNA2ds
092200040226     c                   movel     OA2f03        WF03
092300040226     c                   movel     'S'           WRIEM
092400051201     C                   SETON                                        31
092500040220e   1c                   endif
092600921013     C*
092700921013     C* CMD18 - LEGAME BOLLA: VADO IN INTERROG. BOLLE ARRIVI
092800040220if  1C     *INKS         IFEQ      *ON
092900921013     C                   EXSR      CMD18
093000921013     C                   SETON                                        31
093100941222     C                   MOVEL     'S'           WRIEM
093200040220e   1C                   ENDIF
093300921013     C*
093400921013     C* CMD21 - DETTAGLIO SEGNACOLLI
093500040220if  1C     *INKV         IFEQ      *ON
093600940808     C                   MOVEL     PA0AAS        P64AAS
093700940808     C                   MOVEL     PA0LNP        P64LNP
093800940808     C                   MOVEL     PA0NRS        P64NRS
093900940808     C                   MOVEL     PA0NSP        P64NSP
094000940808     C                   MOVEL     *BLANKS       P64F03
094100940808     C                   MOVEL     *BLANKS       P64FLG
094200940808     C                   MOVEL     *BLANKS       P64PGM
094300940808     C                   MOVEL     'FNLS05R'     P64PGM
094400940808     C                   MOVEL     V1CRSU        P64RSU
094500940808     C                   MOVEL     PA0CML        P64CML
094600111020     C**!!!              MOVEL     PA0CLI        P64CLI
094700940808     C                   MOVEL     PARA64        KPJBU
094800930701     C                   SETON                                        31
094900940804     C                   CALL      'FNLS64R'
095000921013     C                   PARM                    KPJBA
095100940808     C                   MOVEL     KPJBU         PARA64
095200940808     C                   MOVEL     P64F03        WF03
095300940808     C                   MOVEL     'S'           WRIEM
095400040220e   1C                   ENDIF
095500921013     C*
095600990118     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
095700040220do  1C   KJ              DO
095800930526     C                   SETON                                        31
095900940804     C                   MOVEL     PA0AAS        PA2AAS
096000940804     C                   MOVEL     PA0LNP        PA2LNP
096100940804     C                   MOVEL     PA0NRS        PA2NRS
096200940804     C                   MOVEL     PA0NSP        PA2NSP
096300940804     C                   MOVEL     *BLANKS       PA2F03
096400940804     C                   MOVEL     *BLANKS       PA2FLG
096500940804     C                   MOVEL     *BLANKS       PA2PGM
096600940804     C                   MOVEL     'FNLS05R'     PA2PGM
096700940804     C                   MOVEL     V1CRSU        PA2DSA
096800940808     C                   MOVEL     V1CDSP        PA2DSP
096900940804     C                   MOVEL     V2CRMN        PA2RMN
097000940804     C                   MOVEL     V1CLNA        PA2LNA
097100940804     C                   MOVEL     V1DLNA        PA2LN1
097200940804     C                   MOVEL     V2CRSD        PA2RSD
097300950628     C                   MOVEL     'P'           PA2TBO
097400941222     C                   MOVEL     PA0SIM        PA2FEL
097500940804     C                   MOVEL     PARAM2        KPJBU
097600940804     C                   CALL      'FNLR75R'
097700921013     C                   PARM                    KPJBA
097800940808     C                   MOVEL     '1'           PA075
097900940808     C                   MOVEL     'S'           WRIEM
098000040220e   1C                   ENDDO
098100921013     C*
098200990118     C* F19 - INTERROGA SPUNTE
098300040220if  1C     *INKT         IFEQ      *ON
098400930701     C                   SETON                                        31
098500101203     C                   MOVEL     PA0AAS        vi1boan
098600101203     C                   MOVEL     PA0LNP        vi1bolp
098700101203     C                   MOVEL     PA0NRS        vi1bors
098800101203     C                   MOVEL     PA0NSP        vi1bosp
098900950116     C                   MOVEL     BLPFLS        VI1LNP
099000940804     C                   MOVEL     PA0NRS        VI1NRS
099100941230     C                   MOVEL     9             VI1NPG
099200930701     C                   MOVEL     99            VI1NPS
099300101203     C                   MOVEL     'FNLS05R'     VI1PGM
099400930701     C                   MOVEL     '1'           VI1FLG
099500930701     C                   Z-ADD     BLPLNA        VI1LNA
099600930701     C*
099700040220if  2C     BLPNCD        IFGT      0
099800930701     C                   Z-ADD     BLPNCD        VI1NCD
099900930701     C                   Z-ADD     BLPNCA        VI1NCA
100000040220x   2C                   ELSE
100100930701     C*
100200940802     C     KBLP3         SETLL     FNBLT01L
100300940802     C     KBLP3         READE     FNBLT01L                               31
100400040220do  3C  N31              DO
100500940808     C                   Z-ADD     BLTNSC        VI1NCD
100600940802     C     KBLP3         SETGT     FNBLT01L
100700940802     C     KBLP3         READPE    FNBLT01L                               31
100800940808     C  N31BLTNSC        IFNE      VI1NCD
100900940808     C                   MOVEL     BLTNSC        VI1NCA
101000040220     C                   ENDIF
101100040220e   3C                   ENDDO
101200040220e   2C                   ENDIF
101300930701     C*
101400930701     C                   MOVEL     PARAM1        KPJBU
101500940804     C                   CALL      'FNLR04R'
101600921013     C                   PARM                    KPJBA
101700940808     C                   MOVEL     'S'           WRIEM
101800040220e   1C                   ENDIF
101900930505     C*
102000990118     C* F23 - VISUALIZZA DATA CONSEGNA PREVISTA E ORE DI RITARDO
102100040220do  1C   KX              DO
102200930505     C                   SETOFF                                       94
102300930505     C                   SETON                                        31
102400930505     C*
102500930505     C                   MOVEL     ' '           D98TLA
102600930505     C                   MOVEL     'P'           D98TBO
102700930505     C                   MOVEL     BLPAAS        D98AAS
102800930505     C                   MOVEL     BLPLNP        D98LNP
102900930505     C                   MOVEL     BLPNRS        D98NRS
103000930505     C                   MOVEL     BLPNSP        D98NSP
103100930505     C*
103200970925     C                   CALL      'TNSD99R'
103300991022     C                   PARM                    TNSD99DS
103400930505     C*
103500940808     C                   Z-ADD     D98DCE        G02INV
103600940808     C                   Z-ADD     *ZEROS        G02DAT
103700940808     C                   MOVEL     '3'           G02ERR
103800940808     C                   CALL      'XSRDA8'
103900940808     C                   PARM                    WLBDAT
104000940808     C                   Z-ADD     G02DAT        V1CDCE
104100940808     C                   Z-ADD     D98NRC        V1CNRC
104200930526     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
104300930526     C*   CONSEGNA PREVISTA
104400940808     C  N98              Z-ADD     D98HCE        V1CHMC
104500930528     C  N98              Z-ADD     D98HCE        VIDHC2
104600930505     C*
104700940808     C* SE PA098= 1  E' STATA FATTA CALL PGM
104800940808     C                   MOVEL     '1'           PA098
104900040220e   1C                   ENDDO
105000931115     C*
105100990118     C* F20 - VISUALIZZAZIONE AFFIDABILITA'
105200040220do  1C   KU              DO
105300950102     C                   CLEAR                   FNLR97
105400950102     C                   Z-ADD     PA0AAS        R97AAS
105500950102     C                   Z-ADD     PA0LNP        R97LNP
105600950102     C                   Z-ADD     PA0NRS        R97NRS
105700950102     C                   Z-ADD     PA0NSP        R97NSP
105800950102     C                   Z-ADD     V1CDSP        R97DSP
105900950102     C                   Z-ADD     V2CRMN        R97RMN
106000950102     C                   Z-ADD     V1CLNA        R97LNA
106100950102     C                   Z-ADD     V3CDCP        R97DCP
106200950102     C                   MOVEL     V2CRSD        R97RSD
106300950102     C                   MOVEL     V1CRSU        R97RSU
106400950328     C                   MOVEL     V1DLNA        R97DLA
106500950102     C                   MOVEL     PA0CML        R97CML
106600950102     C                   MOVE      'P'           R97AP
106700950102     C   95              MOVE      '1'           R97EVE
106800950102     C                   Z-ADD     V1CDBR        R97DBR
106900950102     C                   Z-ADD     V1CDPC        R97DPC
107000950102     C                   Z-ADD     V1CDUC        R97DUC
107100950328     C                   MOVEL     V3CTMC        R97TMC
107200950102     C                   Z-ADD     V2CCAS        R97CAS
107300950102     C                   MOVEL     V2CVCA        R97VCA
107400931115     C                   MOVE      *BLANKS       KPJBU
107500950102     C                   MOVEL     FNLR97        KPJBU
107600940804     C                   CALL      'FNLR97R'                            31
107700931115     C                   PARM                    KPJBA
107800950102     C                   MOVEL     KPJBU         FNLR97
107900950102     C                   MOVEL     R97F03        WF03
108000931115     C                   SETON                                        31
108100940808     C                   MOVEL     'S'           WRIEM
108200040220e   1C                   ENDDO
108300931115     C*
108400990118     C* F22 - INTERROGAZIONE CONTRASSEGNO, SOLO SE PRESENTE
108500040220if  1C   KWV2CCAS        IFGT      *ZERO
108600991022     C                   CLEAR                   tnsc20ds
108700950125     C                   Z-ADD     1             D20INT
108800950125     C                   Z-ADD     PA0AAS        D20ANN
108900950125     C                   Z-ADD     PA0LNP        D20LNP
109000950125     C                   Z-ADD     PA0NRS        D20NSR
109100950125     C                   Z-ADD     PA0NSP        D20NSP
109200991022     C                   MOVEL     tnsc20ds      KPJBU
109300950125     C                   CALL      'TNSC23R'                            31
109400931115     C                   PARM                    KPJBA
109500931115     C                   SETON                                        31
109600040220     C                   MOVEL     'S'           WRIEM
109700040220e   1C                   ENDIF
109800941021     C*
109900991022     C* ? FORMATO VIDEO:  LS05D08  (TASSAZIONE 1a BOLLA) ?
110000991022     C*  EMETTO SOLO SE VI E' TASSAZIONE
110100040220do  1C   KM              DO
110200941021     C     WRIEM         IFEQ      'S'
110300941021     C                   WRITE     LS05T01
110400941021     C                   MOVEL     *BLANKS       WRIEM
110500941021     C                   ENDIF
110600941021     C                   WRITE     LS05D08
110700941021     C*
110800941021     C                   EXFMT     LS05C08
110900941021     C                   SETON                                        31
111000040220e   1C                   ENDDO
111100991022      *
111200991022      * ? FORMATO VIDEO:  LS05D04  (TASSAZIONE 2a BOLLA) ?
111300040220do  1C   KD              DO
111400991022     C     WRIEM         IFEQ      'S'
111500991022     C                   WRITE     LS05T01
111600991022     C                   MOVEL     *BLANKS       WRIEM
111700991022     C                   ENDIF
111800991022     C                   EXFMT     LS05D04
111900991022     C                   SETON                                        31
112000040220e   1C                   ENDDO
112100991022      **
112200990118     C** F2 - INTERROGAZIONE C.A. SE CE N'E' PIU' DI UNA EMETTO SFL
112300990129     C* Pa0flg="C" -- > Disabilitare F2 PERCHE' RICHIAMATO DA C.A.
112400040220if  1C   KBPA0FLG        IFNE      'C'
112500990129     C                   EXSR      CMD02
112600040220e   1C                   ENDIF
112700931115     C*
112800921013     C                   ENDSR
112900990125     C**************************************************************************
113000990125     C** F2 - INTERROGAZIONE C.A.
113100990125     C**************************************************************************
113200990125     C     CMD02         BEGSR
113300990125     C     CONTCA        IFEQ      1
113400991022     C                   CLEAR                   FIDN00DS
113500990125     C                   Z-ADD     VHCAAC        I00AAC
113600990125     C                   Z-ADD     VHCFIL        I00FIL
113700990125     C                   Z-ADD     VHCNCA        I00NCA
113800990125     C                   Z-ADD     PA0AAS        I00AAS
113900990125     C                   Z-ADD     PA0NRS        I00NRS
114000990125     C                   Z-ADD     PA0NSP        I00NSP
114100990125     C                   Z-ADD     PA0LNP        I00LNP
114200990125     C                   MOVEL     'P'           I00TPB
114300991022     C                   MOVEL     FIDN00DS      KPJBU
114400990125     C                   CALL      'FIDN01R'
114500990125     C                   PARM                    KPJBA
114600000309     C                   PARM                    CALLricors        1
114700990125     C                   SETON                                        31
114800990125     C                   MOVEL     'S'           WRIEM
114900990125     C                   ELSE
115000990125     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
115100990125     C                   SETOFF                                       21
115200990125     C                   WRITE     LS05C11
115300990125     C                   SETON                                        212023
115400990125     C                   CLEAR                   NRR11
115500990125     C**
115600040714     C**""*KBLP3         CHAIN     FNDCT02L                           31
115700040714     C**""**IN31         DOWEQ     *OFF
115800040714     C**""*DCTATB        IFEQ      ' '
115900040714     C**""*              Z-ADD     DCTAAC        V11AAC
116000040714     C**""*              Z-ADD     DCTFIL        V11FIL
116100040714     C**""*              Z-ADD     DCTNCA        V11NCA
116200040714     C**""*              Z-ADD     DCTDCA        G02INV
116300040714     C**""*              Z-ADD     *ZERO         G02DAT
116400040714     C**""*              MOVEL     '3'           G02ERR
116500040714     C**""*              CALL      'XSRDA8'
116600040714     C**""*              PARM                    WLBDAT
116700040714     C**""*              Z-ADD     G02DAT        V11DCA
116800040714     C**""*              Z-ADD     DCTFCA        V11FCA
116900040714     C**""*DCTFCA        CHAIN     FNDFA01L                           31
117000040714     C**N31              MOVEL     DFADES        V11DFA
117100040714     C**""*
117200040714     C**""*              ADD       1             NRR11             4 0
117300040714     C**""*              WRITE     LS05S11
117400040714     C**""*              ENDIF
117500040714     C**""**
117600040714     C**""*KBLP3         READE     FNDCT02L                               31
117700040714     C**""**             ENDDO
117800040714      *
117900040714      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
118000040714      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
118100040714      * legate alla spedizione che passo
118200040714     c                   clear                   fidn12ds
118300040714     c                   eval      i12aas = pa0aas
118400040714     c                   eval      i12lnp = pa0lnp
118500040714     c                   eval      i12nrs = pa0nrs
118600040714     c                   eval      i12nsp = pa0nsp
118700040714     c                   eval      i12fel = 'E'
118800040714     c                   eval      i12fan = 'E'
118900040714      *
119000040714     c                   call      'FIDN12R'
119100040714     c                   parm                    fidn12ds
119200040714      *
119300040714      * se non ci sono errori
119400040714     c                   if        o12err = ' '
119500040714      * se numero di CA trovate maggiore di zero
119600040714     c                   if        o12nca > 0
119700040714     c
119800040714     c                   z-add     *zeros        ii                2 0
119900040714     c     1             do        o12nca        ii
120000040714     c                   movea     skkey(ii)     dskey
120100040714     C                   Z-ADD     dsaac         V11AAC
120200040714     C                   Z-ADD     dsfil         V11FIL
120300040714     C                   Z-ADD     dsnca         V11NCA
120400040714     C                   Z-ADD     skdca(ii)     G02INV
120500040714     C                   Z-ADD     *ZERO         G02DAT
120600040714     C                   MOVEL     '3'           G02ERR
120700040714     C                   CALL      'XSRDA8'
120800040714     C                   PARM                    WLBDAT
120900040714     C                   Z-ADD     G02DAT        V11DCA
121000040714     C                   Z-ADD     skfca(ii)     V11FCA
121100040714     C     V11FCA        CHAIN     FNDFA01L                           31
121200040714     C  N31              MOVEL     DFADES        V11DFA
121300040714     C**
121400040714     C                   ADD       1             NRR11             4 0
121500040714     C                   WRITE     LS05S11
121600040714     C                   ENDDO
121700040714      *
121800040714     c                   endif
121900040714     c                   endif
122000040714      *
122100990125     C* EMETTO FORMATO
122200990125     C     FOR11         TAG
122300990125     C                   Z-ADD     1             NRR11
122400990125     C                   WRITE     LS05T01
122500990125     C                   WRITE     LS05Z11
122600990125     C                   EXFMT     LS05C11
122700990125     C* F12 - RITORNO
122800990125     C     *INKL         IFEQ      *OFF
122900990125     C                   READC     LS05S11                                31
123000990125     C     *IN31         DOWEQ     *OFF
123100990125     C     V11SCE        IFEQ      '5'
123200991022     C                   CLEAR                   FIDN00DS
123300990125     C                   Z-ADD     V11AAC        I00AAC
123400990125     C                   Z-ADD     V11FIL        I00FIL
123500990125     C                   Z-ADD     V11NCA        I00NCA
123600990125     C                   Z-ADD     PA0AAS        I00AAS
123700990125     C                   Z-ADD     PA0NRS        I00NRS
123800990125     C                   Z-ADD     PA0NSP        I00NSP
123900990125     C                   Z-ADD     PA0LNP        I00LNP
124000990125     C                   MOVEL     'P'           I00TPB
124100991022     C                   MOVEL     FIDN00DS      KPJBU
124200990125     C                   CALL      'FIDN01R'
124300990125     C                   PARM                    KPJBA
124400000309     C                   PARM                    CALLricors
124500990125     C**
124600990125     C                   CLEAR                   V11SCE
124700990125     C                   UPDATE    LS05S11
124800990125     C                   ENDIF
124900990125     C**
125000990125     C                   READC     LS05S11                                31
125100990125     C                   ENDDO
125200990125     C                   GOTO      FOR11
125300990125     C                   ENDIF
125400990125     C                   SETON                                        31
125500990125     C                   MOVEL     'S'           WRIEM
125600990125     C*
125700990125     C                   ENDIF
125800990125     C                   ENDSR
125900990125     C**************************************************************************
126000990125     C*    CONTROLLO SE C'E' UNA O PIU' C.A.
126100990125     C**************************************************************************
126200990125     C     CTRDAN        BEGSR
126300990125     C                   CLEAR                   CONTCA
126400040714     C**""*KBLP3         CHAIN     FNDCT02L                           31
126500040714     C**""**IN31         DOWEQ     *OFF
126600040714     C**""*DCTATB        IFEQ      ' '
126700040714     C**""**             SETON                                        12
126800040714     C**""**             ADD       1             CONTCA            3 0
126900040714     C**""**             Z-ADD     DCTAAC        VHCAAC
127000040714     C**""**             Z-ADD     DCTFIL        VHCFIL
127100040714     C**""**             Z-ADD     DCTNCA        VHCNCA
127200040714     C**""**             ENDIF
127300990125     C*
127400040714     C**""*KBLP3         READE     FNDCT02L                               31
127500040714     C**""**             ENDDO
127600040714      *
127700040714      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON
127800040714      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
127900040714      * late e legate alla spedizione che passo
128000040714     c                   clear                   fidn12ds
128100040714     c                   eval      i12aas = pa0aas
128200040714     c                   eval      i12lnp = pa0lnp
128300040714     c                   eval      i12nrs = pa0nrs
128400040714     c                   eval      i12nsp = pa0nsp
128500040714     c                   eval      i12fel = 'E'
128600040714     c                   eval      i12fan = 'E'
128700040714      *
128800040714     c                   call      'FIDN12R'
128900040714     c                   parm                    fidn12ds
129000040714      *
129100040714      * se non ci sono errori
129200040714     c                   if        o12err = ' '
129300040714      * se numero di CA trovate maggiore di zero
129400040714     c                   if        o12nca > 0
129500040714     c                   z-add     O12NCA        CONTCA            3 0
129600040714     C                   SETON                                        12
129700040714     C                   MOVEA     SKKEY(O12NCA) DSKEY
129800040714     C                   Z-ADD     DSAAC         VHCAAC
129900040714     C                   Z-ADD     DSFIL         VHCFIL
130000040714     C                   Z-ADD     DSNCA         VHCNCA
130100040714     C                   ENDIF
130200040714     C                   ENDIF
130300040714      *
130400990125     C                   ENDSR
130500940808     C**************************************************************************
130600971212     C*    CARICO RECORD DA VISUALIZZARE/MANUTENZIONARE
130700940808     C**************************************************************************
130800900511     C     CARBLP        BEGSR
130900940808     C*
131000930112     C*** PULIZIA CAMPI
131100930112     C*
131200930112     C                   MOVEL     ' '           VI2CEI
131300950628     C                   MOVEL     'P'           PA2TBO
131400940804     C                   Z-ADD     0             V3CDCP
131500930526     C*
131600920326     C                   SETOFF                                       440307
131700951011     C                   SETOFF                                       949813
131800021218     C                   SETOFF                                       0977
131900940923     C*
132000940923     C                   MOVEL     PA0AAS        V1CAAS
132100940926     C                   MOVEL     PA0LNP        V1CLNP
132200940923     C                   MOVEL     PA0NRS        V1CNRS
132300940923     C                   MOVE      PA0NSP        V1CNSP
132400940923     C*
132500900511     C* CHAIN
132600041028     c                   clear                   wesarb            1
132700940802     C     KBLP3         CHAIN     FNBLP01L                           31
132800041028     C     KBLP3         CHAIN     FNarb01L
132900041028     c                   if        %found(fnarb01l)
133000041028     c                   eval      wesarb='S'
133100041028     c                   else
133200041028     c                   eval      wesarb='N'
133300041028     c                   endif
133400940926     C*
133500940928     C* SE NON TROVATA BOLLA, PROVO NELLO STORICO
133600940928     C* E SE NON TROVATA PULISCO I CAMPI CHIAVE E ESCO
133700010713     C     *IN31         IFEQ      *ON
133800010716     C                   MOVEL     *ZERO         PA0AAS
133900010716     C                   MOVEL     *ZERO         PA0LNP
134000010716     C                   MOVEL     *ZERO         PA0NRS
134100010716     C                   MOVEL     *ZERO         PA0NSP
134200010716     C                   GOTO      ENDBLP
134300010713     C                   ENDIF
134400021218     C*
134500040220     C* 77 ON - Esistono note supermercati da interrogare
134600021218     C                   Z-ADD     PA0AAS        KAAS
134700021218     C                   Z-ADD     PA0LNP        KLNP
134800021218     C                   Z-ADD     PA0NRS        KNRS
134900021218     C                   Z-ADD     PA0NSP        KNSP
135000021218     C                   MOVEL     'SCR'         KTRD
135100021218     C     KAR51         CHAIN     FIAR5000                           31
135200021218     C                   If        *in31 = *off and Ar5Atb = ' '
135300021218     C                   seton                                        77
135400021218     C                   else
135500021218     C                   setoff                                       77
135600021218     C                   endif
135700940926     C*
135800900927     C* CONTROLLO SE SPEDIZIONE GIACENTE
135900050406     C     KBLP3         CHAIN     tIGCP01L                           31
136000940923     C*
136100901004     C  N31GCPDCG        IFEQ      0
136200910321     C                   SETON                                        4403
136300910321     C                   ELSE
136400910321     C                   SETON                                        03
136500901004     C                   END
136600111020     C**!03PA0CLI        COMP      'S'                                0303
136700900511     C*
136800950214     C                   CLEAR                   V2CFLS
136900950214     C     BLPNCD        IFNE      *ZERO
137000950214     C     BLPFNS        ORNE      *BLANKS
137100940923     C                   MOVEL     BLPFLS        V2CFLS
137200950214     C                   ENDIF
137300940923     C                   MOVEL     PA0NRS        V7CNRS
137400940808     C                   MOVEL     BLPLNP        V1CLP1
137500940805     C                   MOVEL     BLPCBO        V1CCBO
137600940809     C*
137700940809     C                   MOVEL     BLPGGA        V2CGGA
137800940923     C                   MOVEL     *BLANKS       V2IGGA
137900940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
138000940923     C     V2CGGA        COMP      *BLANKS                            1717
138100940809     C*
138200940809     C                   MOVEL     BLPGMA        V2CGMA
138300940923     C                   MOVEL     *BLANKS       V2IGMA
138400940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
138500940923     C     V2CGMA        COMP      *BLANKS                            1818
138600940809     C*
138700960208     C                   MOVEL     BLPGVA        V2CGVA
138800960208     C                   MOVE      ' '           V2CGVA
138900940923     C                   MOVEL     *BLANKS       V2IGVA
139000940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
139100940923     C     V2CGVA        COMP      *BLANKS                            1919
139200940322      *
139300911105     C* NON SEQUENZIALI
139400910912     C     BLPFNS        IFEQ      'S'
139500940808     C                   MOVEL     SEQ(1)        V2CFNS
139600910912     C                   ELSE
139700940808     C                   MOVEL     *BLANKS       V2CFNS
139800910912     C                   END
139900930112     C* CONTROLLO SE ESISTONO VARIAZIONI PER LA BOLLA
140000131007     C     KBLP3         CHAIN     FIARBF2C                           31
140100111020     C**!!!PA0CLI        IFNE      'S'
140200920429     C  N31              SETON                                        10
140300111020     C**!!!              ENDIF
140400910806     C*
140500920409     C* TIPO BOLLA CONTROLLO SE FRANCO
140600940804     C                   MOVEL     '3A'          KCOD
140700940804     C                   MOVEL     *BLANKS       KKEY
140800940805     C                   MOVEL     BLPCBO        KKEY
140900940809     C     KTAB1         CHAIN     TABEL00F                           31
141000940923     C  N31              MOVEL     TBLUNI        DS3A
141100940923     C   31              CLEAR                   DS3A
141200900516     C                   MOVEL     §3ATB1        FLGTB1            1
141300940804     C                   MOVEL     §3ADES        V1DCBO
141400940805     C                   MOVEL     §3ADES        V2DCBO
141500940804     C                   MOVEL     §3ADES        V7DCBO
141600940408     C     FLGTB1        IFEQ      'F'
141700910418     C                   SETON                                        33
141800900511     C                   END
141900900511     C* LINEA ARRIVO E DECODIFICA
142000940804     C                   MOVEL     BLPLNA        V1CLNA
142100020225     C     BLPLNA        CHAIN     AZORG01L
142200020225     c                   if        not %found(azorg01l)
142300020225     C                   MOVEL     *BLANKS       ORGDES
142400020225     c                   endif
142500940804     C                   MOVEL     ORGDES        V1DLNA
142600940804     C                   MOVEL     ORGDES        V2DLNA
142700940810     C                   MOVEL     ORGDES        V9DLNA
142800020225     c                   eval      og143  = ORGde3
142900020225     c                   eval      lnantw = §ogntw
143000971202     C* TERMINAL DI PARTENZA DELLA LINEA DI ARRIVO
143100991022     C                   CLEAR                   FNLV55DS
143200971202     C                   MOVEL     'P'           D55TPT
143300971202     C                   MOVE      BLPLNA        D55LIN
143400971202     C                   Z-ADD     WDATEU        D55DRF
143500971202     C                   CALL      'FNLV55R'
143600991022     C                   PARM                    FNLV55DS
143700971202     C                   MOVE      D55TFP        WTFP
143800940804     C* LINEA PARTENZA E DECODIFICA
143900020225     C     BLPLNP        CHAIN     AZORG01L
144000020225     c                   if        not %found(azorg01l)
144100020225     C                   MOVEL     *BLANKS       ORGDES
144200020225     c                   endif
144300940804     C                   MOVEL     ORGDES        V1DLNP
144400020225     c                   eval      og143  = ORGde3
144500020225     c                   eval      lnpntw = §ogntw
144600920410     C* MITTENTE
144700940804     C                   MOVEL     BLPRSM        V1CRSM
144800940808     C                   MOVEL     BLPRSM        V2CRSM
144900940805     C                   MOVEL     BLPRSM        V7CRSM
145000940804     C                   MOVEL     BLPINM        V1CINM
145100940808     C                   MOVEL     BLPINM        V7CINM
145200940804     C                   MOVEL     BLPPRM        V1CPRM
145300940804     C                   MOVEL     BLPCAM        V1CCAM
145400940804     C                   MOVEL     BLPLOM        V1CLOM
145500940808     C                   MOVEL     BLPLOM        V2CLOM
145600940808     C                   MOVEL     BLPLOM        V7CLOM
145700940808     C                   MOVEL     BLPPRM        V2CPRM
145800940804     C                   MOVEL     BLPNZM        V1CNZM
145900940808     C                   MOVEL     BLPFAP        V1CFAP
146000981023     C                   MOVEL     BLPCTR        V2CCTR
146100060216     C* NUMERO ORM: DA ar4 (Tipo record 'M')
146200950412     C                   MOVEL     BLPAAS        KAAS
146300950412     C                   MOVEL     BLPLNP        KLNP
146400950412     C                   MOVEL     BLPNRS        KNRS
146500950412     C                   MOVEL     BLPNSP        KNSP
146600010220     C                   MOVEL     'M'           KTRC
146700010220     C                   clear                   v1cpoe
146800010220     C                   clear                   v1cnsr
146900010220     C                   clear                   v1cnor
147000010220     C                   clear                   v1cnrv
147100010220     C                   setoff                                       55
147200060216     C     Kar41         CHAIN     Fiar401L                           99
147300010220     C     *in99         ifeq      *off
147400060216     C     ar4not        ifne      *blanks
147500060216     C                   movel     ar4not        dsbl4m
147600010220     C                   move      §b4poe        v1cpoe
147700010220     C                   move      §b4nsr        v1cnsr
147800010220     C                   move      §b4nor        v1cnor
147900010220     C                   move      §b4nrv        v1cnrv
148000040713     c     korm          Setll     Fnorm01l
148100040713     c                   If        %Equal(fnorm01l)
148200010220     C                   seton                                        55
148300040713     c                   EndIf
148400010220     C                   endif
148500010220     C                   endif
148600010220     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
148700060216     C* DA Fiar4 SE RECORD ESISTENTE
148800950412     C                   MOVEL     'P'           KTRC
148900060216     C     Kar41         CHAIN     Fiar401L                           99
149000950412     C     *IN99         IFEQ      *OFF
149100060216     C                   MOVEL     ar4NOT        V1CPIM
149200060216     C                   MOVE      ar4NOT        V2CPID
149300950412     C                   ELSE
149400950412     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI BLPCPI
149500930112     C     BLPCPI        IFNE      *BLANKS
149600940804     C   33              MOVEL     BLPCPI        V1CPIM
149700940804     C  N33              MOVEL     BLPCPI        V2CPID
149800930112     C                   ELSE
149900940808     C                   MOVEL     *BLANKS       V1CPIM
150000940808     C                   MOVEL     *BLANKS       V2CPID
150100930112     C                   END
150200950412     C                   END
150300061108      * codice fiscale mittente e destinatario da fiar4 rcd Q
150400061108     c                   clear                   v1cdfm
150500061108     c                   clear                   v2cdfd
150600061108     c                   eval      ktrc = 'Q'
150700061108     c     kar41         chain     fiar401l
150800061108     c                   if        %found(fiar401l)
150900061108     c                   movel     ar4not        v1cdfm
151000061108     c                   move      ar4not        v2cdfd
151100061108     c                   endif
151200900525     C*
151300940906     C* CERCO LA DESCRIZIONE AGGIUNTIVA PER IL DESTINATARIO
151400940808     C                   MOVEL     BLPAAS        KAAS
151500940808     C                   MOVEL     BLPLNP        KLNP
151600940808     C                   MOVEL     BLPNRS        KNRS
151700940808     C                   MOVEL     BLPNSP        KNSP
151800940906     C                   MOVEL     'D'           KTRC
151900060216     C     Kar41         CHAIN     Fiar401L                           99
152000060216     C  N99              MOVEL     ar4NOT        V2CNOT
152100950125     C   99              MOVEL     *BLANKS       V2CNOT
152200940808     C*
152300971212     C* SOLO PER LE FILIALI ESTERE IMPORT/EXPORT VISUALIZZO RIF PARTNER
152400971212     C                   CLEAR                   V1CEST
152500951011     C                   MOVEL     'E'           KTRC
152600060216     C     Kar41         CHAIN     Fiar401L                           99
152700060216     C  N99              MOVEL     ar4NOT        V2CRPP
152800980704     C  N99              SETON                                        13
152900020225     c   99              if        (lnantw = 'EEX' or lnantw = 'EUP' or
153000020225     c                              lnantw = 'FED') or
153100020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
153200020225     c                              lnpntw = 'FED')
153300980704     C                   SETON                                        13
153400980704     C                   ENDIF
153500951011     C   99              CLEAR                   V2CRPP
153600971212     C* SE E' SDI IMPORTO ANCHE TIPO INSERIMENTO E STAMPA
153700020225     c                   if        not *in99 and (lnpntw = 'EUP' or
153800020225     c                                            lnantw = 'EUP')
153900971212     C**
154000060216     C                   MOVEL     ar4NOT        DSBL4E
154100971212    3C     §B4EMA        IFEQ      'M'
154200971212     C                   MOVEL     CMANUA        W021A            21
154300971212     C                   SETON                                        09
154400971212    4C     §B4EAT        IFEQ      'P'
154500971212     C                   MOVE      'PART'        W021A
154600971212    4C                   ENDIF
154700971212    4C     §B4EAT        IFEQ      'A'
154800971212     C                   MOVE      'ARR.'        W021A
154900971212    4C                   ENDIF
155000971212     C                   MOVE      W021A         V1CEST
155100971212    3C                   ENDIF
155200971212    C**
155300971212    3C     §B4EMA        IFEQ      'A'
155400971212     C                   MOVEL     CAUTOM        W018A            18
155500971212     C                   SETON                                        09
155600971212    4C     §B4EAT        IFEQ      'P'
155700971212     C                   MOVE      'PART'        W018A
155800971212    4C                   ENDIF
155900971212    4C     §B4EAT        IFEQ      'A'
156000971212     C                   MOVE      'ARR.'        W018A
156100971212    4C                   ENDIF
156200971212     C*
156300971212     C                   MOVEL     W018A         V1CEST
156400971212     C                   MOVE      CSTAMP        V1CEST
156500971212     C**
156600971212    4C     §B4EST        IFEQ      'N'
156700971212     C                   MOVE      'NO'          V1CEST
156800971212    4C                   ELSE
156900971212     C                   MOVE      'SI'          V1CEST
157000971212    4C                   ENDIF
157100991022      *
157200971212    3C                   ENDIF
157300971212    2C                   ENDIF
157400991227     c* DPD
157500000216     c                   setoff                                       14
157600000216     c                   clear                   v2crtn
157700000216     c                   clear                   v2czon
157800020225     c                   if        lnantw = 'DPD'
157900991227     c                   seton                                        13
158000060517     C***                MOVEL     'F'           KTRC
158100060517     C                   MOVEL     'I'           KTRC
158200060216     C     Kar41         CHAIN     Fiar401L                           99
158300060517     C**N99              MOVEL     ar4NOT        dsbl4f
158400060517     c** 99              clear                   dsbl4f
158500060517     C  N99              MOVEL     ar4NOT        dsbl4I
158600060517     c   99              clear                   dsbl4I
158700060517     C***                MOVEL     §b4fnp        V2CRPP
158800060517     C                   MOVEL     §b4ipn        V2CRPP
158900060517     c****               if        §b4rtn>*zeros or §b4zon>*zeros
159000060517     c****               movel     §b4rtn        v2crtn
159100060517     c****               movel     §b4zon        v2czon
159200060517     c                   if        §b4ida>*zeros or §b4iza>*zeros
159300060517     c                   movel     §b4ida        v2crtn
159400060517     c                   movel     §b4iza        v2czon
159500000216     c                   seton                                        14
159600000216     c                   setoff                                       09
159700000216     c                   endif
159800050304     c                   else
159900050304     c* dpd import
160000050304     c                   if        lnpntw = 'DPD'
160100060517     c****               clear                   dsbl4f
160200060517     C****               MOVEL     'F'           KTRC
160300060517     c                   clear                   dsbl4i
160400060517     C                   MOVEL     'I'           KTRC
160500060216     C     Kar41         CHAIN     Fiar401L
160600060216     c                   if        %found(fiar401l)
160700050304     c                   seton                                        13
160800060517     C***                MOVEL     ar4NOT        dsbl4f
160900060517     C                   MOVEL     ar4NOT        dsbl4I
161000060517     C***                MOVEL     §b4fnp        V2CRPP
161100060517     C                   MOVEL     §b4ipn        V2CRPP
161200050304     c                   endif
161300050304     c                   endif
161400991227     c                   endif
161500981023     C***
161600981023     C* ASSEGNATO
161700981023     C     *IN33         IFEQ      *OFF
161800981023     C** MITTENTE
161900981023     C     BLPCCM        IFNE      0
162000981023     C                   MOVEL     BLPCCM        V2CCDM
162100981023     C                   MOVEL     BLPCCM        V1CKSC
162200981023     C                   ELSE
162300981023     C                   CLEAR                   V2CCDM
162400981023     C                   CLEAR                   V1CKSC
162500981023     C                   ENDIF
162600981023     C* DESTINATARIO
162700981023     C                   MOVEL     BLPKSC        V2CCDD
162800981023     C                   MOVEL     BLPKSC        V1CCDD
162900991116     c                   eval      KTRC = '1'
163000991116     C     KAR6          CHAIN     FIAR6000                           99
163100991116     C                   IF        *in99 = *OFF and ar6ksc > *zeros
163200991116     C                   MOVEL     ar6ksc        V2CCDD
163300991116     C                   MOVEL     ar6ksc        V1CCDD
163400991116     C                   MOVEL     ar6ctr        V2CCTR
163500991116     c                   ENDIF
163600981023     C                   ELSE
163700981023     C* FRANCO
163800981023     C                   MOVEL     BLPKSC        V1CKSC
163900981023     C                   MOVEL     BLPKSC        V2CCDM
164000981023     C                   CLEAR                   V2CCDD
164100981023     C                   CLEAR                   V1CCDD
164200981023     C                   ENDIF
164300050117     C                   MOVEL     BLPXCO        V2CXCO
164400940808     C*
164500940808     C* SE C'E' IL COD. MITTENTE, RILEVO IL NR.TELEFONICO
164600940809     C                   MOVE      V2CCDM        WQUATT            4 0
164700940809     C     WQUATT        IFNE      8888
164800940809     C     V2CCDM        ANDNE     *ZERO
164900981123     C                   MOVEL     KCI           KKCC
165000940808     C                   MOVEL     V2CCDM        KKSC
165100940808     C     KIND1         CHAIN     CNIND00F                           99
165200940808     C  N99              MOVEL     INDTEL        V2CTEL
165300940808     C   99              MOVEL     *BLANKS       V2CTEL
165400940809     C                   ENDIF
165500940808     C*
165600940808     C                   MOVEL     BLPRSD        V2CRSD
165700940808     C                   MOVEL     BLPRSD        V7CRSD
165800940810     C                   MOVEL     BLPRSD        V9CRSD
165900940808     C                   MOVEL     BLPIND        V2CIND
166000940808     C                   MOVEL     BLPIND        V7CIND
166100940808     C                   MOVEL     BLPPRD        V2CPRD
166200940808     C                   MOVEL     BLPCAD        V2CCAD
166300940808     C                   MOVEL     BLPLOD        V2CLOD
166400940808     C                   MOVEL     BLPLOD        V7CLOD
166500940808     C                   MOVEL     BLPNZD        V2CNZD
166600940804     C                   MOVEL     BLPFIN        V2CFIN
166700940804     C                   MOVEL     BLPGC1        V2CGC1
166800940804     C                   MOVEL     BLPGC2        V2CGC2
166900961111     C                   MOVEL     *BLANKS       V2CFBR
167000961111     C                   MOVEL     *BLANKS       V2CFFR
167100961111     C                   SETOFF                                       0102
167200940804     C     BLPFBR        IFEQ      'S'
167300940804     C                   MOVEL     'SI'          V2CFBR
167400961111     C                   SETON                                        01
167500940804     C                   ENDIF
167600961111     C     BLPFBR        IFEQ      'F'
167700961111     C                   MOVEL     'FR'          V2CFFR
167800961111     C                   SETON                                        02
167900961111     C                   ENDIF
168000970227     C     BLPFST        IFEQ      'S'
168100970131     C     BLPFST        OREQ      'C'
168200970227     C                   MOVEL     'SI DDT'      V1DBAM
168300970227     C                   SETON                                        08
168400970130     C                   ELSE
168500970227     C                   MOVEL     'NO DDT'      V1DBAM
168600970227     C                   SETOFF                                       08
168700970130     C                   ENDIF
168800970227     C*
168900970227     C                   SELECT
169000970227     C     BLPFST        WHENEQ    'C'
169100970227     C     BLPFST        OREQ      'K'
169200970227     C                   MOVEL     'CLIENTE '    V1DFST
169300970227     C     BLPFST        WHENEQ    'S'
169400970227     C     BLPFST        OREQ      'Y'
169500970227     C                   MOVEL     'PARTENZA'    V1DFST
169600970227     C     BLPFST        WHENEQ    'J'
169700970227     C                   MOVEL     'ARRIVO  '    V1DFST
169800970227     C                   OTHER
169900970227     C                   MOVEL     'NO      '    V1DFST
170000970227     C                   ENDSL
170100970227     C*
170200941219     C     BLPFDN        IFEQ      'S'
170300941219     C                   MOVEL     'SI'          V1CFDN
170400941219     C                   ELSE
170500941219     C                   MOVEL     *BLANKS       V1CFDN
170600941219     C                   ENDIF
170700940808     C                   MOVEL     BLPFTM        V1CFTM
170800940804     C                   MOVEL     *BLANKS       V1DTSP
170900940804     C                   MOVEL     '5E'          KCOD
171000940804     C                   MOVEL     *BLANKS       KKEY
171100940808     C                   MOVEL     BLPTSP        KKEY
171200940809     C     KTAB1         CHAIN     TABEL00F                           99
171300090608     C  N99              MOVEL     TBLUNI        ds5e
171400090608     C  N99              MOVEL     §5ed08        V1DTSP
171500941021     C*
171600940805     C                   MOVEL     BLPCTS        V8CCTS
171700940804     C                   MOVEL     BLPQFT        V2CQFT
171800940804     C                   MOVEL     BLPVMD        V2CVMD
171900940804     C                   MOVEL     BLPVAD        V2CVAD
172000940804     C                   MOVEL     BLPIAS        V2CIAS
172100940804     C                   MOVEL     BLPVAS        V2CVAS
172200940808     C                   MOVEL     BLPNAS        V1CNAS
172300940804     C                   MOVEL     BLPNCL        V1CNCL
172400941026     C                   MOVEL     BLPPKF        V1CPKF
172500990909     C                   MOVEL     BLPPKB        V1CPKB
172600940808     C                   MOVEL     BLPVLB        V1CVLB
172700940808     C                   MOVEL     BLPFVB        V1CFVB
172800940808     C                   MOVEL     BLPVLF        V1CVLF
172900940808     C                   MOVEL     BLPFVF        V1CFVF
173000940808     C* SE BLPNCR = 99999  A VIDEO AZZERO NCR E VLC
173100930505     C     BLPNCR        IFEQ      99999
173200940805     C                   MOVEL     *ZEROS        BLPNCR
173300940805     C                   MOVEL     *ZEROS        BLPVLC
173400940808     C                   ENDIF
173500940808     C                   MOVEL     BLPVLC        V1CVLC
173600940808     C                   MOVEL     BLPNCR        V1CNCR
173700990909     C* SE BLPNCP = 99999  A VIDEO AZZERO NCP E PKC
173800990909     C     BLPNCP        IFEQ      99999
173900990909     C                   MOVEL     *ZEROS        BLPNCP
174000990909     C                   MOVEL     *ZEROS        BLPPKC
174100990909     C                   ENDIF
174200990909     C                   MOVEL     BLPPKC        V1CPKC
174300990909     C                   MOVEL     BLPNCP        V1CNCP
174400031119
174500031119      * Referente e Telefono del destinatario
174600031119     c                   Clear                   V2cRef
174700031119     c                   Clear                   V2cTeld
174800110610     c* profilo utente e terminale che ha immesso la bolla
174900110610     c                   Clear                   Vidpru
175000110610     c                   Clear                   Vidnmtd
175100110610     c                   setoff                                       46
175200031119      * li prendo da Fiar500f record 'GEN'
175300031119     c                   Clear                   dAr5Gen
175400091125     c                   clear                   w_cdi
175500031119     c                   Z-Add     BlpAas        Kaas
175600031119     c                   Z-Add     BlpLNp        Klnp
175700031119     c                   Z-Add     BlpNrs        Knrs
175800031119     c                   Z-Add     BlpNsp        Knsp
175900031119     c                   Eval      Ktrd = 'GEN'
176000031119     c     kAr51         Chain     Fiar501l
176100031119If  1c                   If        %Found(Fiar501l)
176200031119     c                   Movel     Ar5Uni        Dar5Gen
176300031119     c                   Eval      V2cRef = §Ar5Ref
176400031119     c                   Eval      V2cTeld = §Ar5Teld
176500091125     c* memorizzo eventuale codice beneficiario assegno
176600091125     c                   eval      w_cdi=§ar5cdi
176700110610      *
176800110610     c                   Eval      Vidpru = §Ar5bpru
176900110610     c                   Eval      Vidnmtd = §Ar5bnmtd
177000110610     c                   if        vidpru<>*blanks
177100110610     c                   seton                                        46
177200110610     c                   endif
177300031119    1c                   EndIf
177400130319     c* Vedo se ha il packing list
177500130320     c                   if        §ar5alS='S'
177600130320     c                   eval      v1dar8='PACKING LIST PDF'
177700130319     C                   ELSE
177800130320     c                   if        §ar5alx='S'
177900130320     c                   eval      v1dar8='PACKING LIST    '
178000130319     c                   else
178100130319     C                   CLEAR                   V1DAR8
178200130319     c                   endif
178300130319     C                   ENDIF
178400110531
178500110531     c* email destinatario
178600110601     c                   setoff                                       45
178700110531     c                   clear                   v2cemW
178800130927     c                   clear                   dar5emd
178900110531      * lo prendo da Fiar500f record 'EMD'
179000110531     c                   Z-Add     BlpAas        Kaas
179100110531     c                   Z-Add     BlpLNp        Klnp
179200110531     c                   Z-Add     BlpNrs        Knrs
179300110531     c                   Z-Add     BlpNsp        Knsp
179400110531     c                   Eval      Ktrd = 'EMD'
179500130927     c*    kAr51         setll     Fiar501l
179600130927     c     kAr51         chain     Fiar501l
179700130927If  1c                   If        %found(Fiar501l)
179800130927     c                   eval      dar5emd=ar5uni
179900130927     c                   if        §ar5tel<>*blanks or §ar5eml<>*blanks
180000110610     c                   eval      v2cemd='SI'
180100110601     c                   seton                                        45
180200130927     c                   endif
180300110531     c                   endif
180400040301
180500040301      * Se colli di valore visualizzo "V" nell'apposito campo
180600040301      *  con lo specifico attributo di visualizzazione
180700040301if  1c                   if        §AR5bva = *blanks
180800040301     c                   eval      V2Cbva  = *blanks
180900040301x   1c                   else
181000040301     c                   eval      V2Cbva  = 'V'
181100040301e   1c                   endif
181200031119
181300030121      * Bancali
181400030121     c                   Clear                   V1dTba
181500030121     c                   Clear                   V1dTb2
181600030122     c                   Clear                   V1cNba
181700030122     c                   Clear                   V1cNb2
181800030703      * Record 'BAN'
181900030703If  0c                   If        %subst(BlpGva:1:1) = 'B'
182000030121     c                   Clear                   dAr5Ban
182100030121     c                   Eval      Ktrd = 'BAN'
182200030121     c     kAr51         Chain     Fiar501l
182300030121If  1c                   If        %Found(Fiar501l)
182400030121     c                   Movel     Ar5Uni        Dar5Ban
182500030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
182600030122      * per le note relative al mancato reso dei bancali
182700030122     c                   If        Not *In77 and
182800030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
182900030122     c                   Eval      *In77 = *On
183000030122     c                   EndIf
183100030121     c                   Eval      V1cNba = §Ar5Nba
183200030121     c                   Eval      V1cNb2 = §Ar5Nb2
183300030121If  2c                   If        §Ar5Tba <> *Zeros
183400030703     c                   Eval      wtba = §Ar5Tba
183500030703     c                   ExSr      Sr_DecTba
183600030703     c                   Eval      V1dTba = wdestba
183700030121    2c                   EndIf
183800030121If  2c                   If        §Ar5Tb2 <> *Zeros
183900030703     c                   Eval      wtba = §Ar5Tb2
184000030703     c                   ExSr      Sr_DecTba
184100030703     c                   Eval      V1dTb2 = wdestba
184200030121    2c                   EndIf
184300030121    1c                   EndIf
184400030703    0c                   EndIf
184500030703      * Record 'BNB'  -  Spedizione OVER 25 colli
184600030703If  0c                   If        %subst(BlpGva:1:1) = 'O'
184700030703     c                   Clear                   dAr5Bnb
184800030703     c                   Eval      Ktrd = 'BNB'
184900030703     c     kAr51         Chain     Fiar501l
185000030703If  1c                   If        %Found(Fiar501l)
185100030703     c                   Movel     Ar5Uni        Dar5Bnb
185200030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
185300030703      * per i colli originali in caso di spedizione over 25 colli
185400030703     c  n77              Eval      *In77 = *On
185500030703     c                   Eval      V1cNba = §Ar5bNba
185600030703     c                   Eval      V1cNb2 = §Ar5bNb2
185700030703If  2c                   If        §Ar5bTba <> *Zeros
185800030703     c                   Eval      wtba = §Ar5bTba
185900030703     c                   ExSr      Sr_DecTba
186000030703     c                   Eval      V1dTba = wdestba
186100030703    2c                   EndIf
186200030703If  2c                   If        §Ar5bTb2 <> *Zeros
186300030703     c                   Eval      wtba = §Ar5bTb2
186400030703     c                   ExSr      Sr_DecTba
186500030703     c                   Eval      V1dTb2 = wdestba
186600030703    2c                   EndIf
186700030703    1c                   EndIf
186800030703    0c                   EndIf
186900920409     C*
187000920409     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
187100940804     C                   MOVEL     'TR'          KCOD
187200940804     C                   MOVEL     *BLANKS       KKEY
187300940808     C                   MOVEL     V2CCTR        UNOCTR            1
187400940804     C                   MOVEL     UNOCTR        KKEY
187500940809     C     KTAB1         CHAIN     TABEL00F                           31
187600940804     C                   MOVEL     TBLUNI        V2DCTR
187700910730     C*
187800940808     C* VISUALIZZO V2CCDM SOLO SE LA LNP DEL CODICE = LNP DELLA BOLLA
187900930505     C                   MOVEL     BLPCCM        NUM3              3 0
188000930505     C     BLPLNP        IFEQ      NUM3
188100940808     C                   MOVE      BLPCCM        V2CCDM
188200930505     C                   END
188300930505     C*
188400940804     C                   MOVEL     BLPRMN        V2CRMN
188500940804     C                   MOVEL     BLPRMA        V2CRMA
188600940804     C                   MOVEL     BLPRMO        V2CRMO
188700940804     C                   MOVEL     BLPNCD        V2CNCD
188800940804     C                   MOVEL     BLPNCA        V2CNCA
188900940804     C                   MOVEL     BLPZNC        V2CZNC
189000940804     C                   MOVEL     BLPCTM        V1CCTM
189100130319     C***                MOVEL     '1B'          KCOD
189200130319     C***                MOVEL(P)  blpctm        KKEY
189300130319     C***  KTAB1         CHAIN     TABEL00F                           31
189400130319     C**N31              MOVEL     TBLUNI        DS1B
189500130319     c** 31              clear                   ds1b
189600051128     c* Vedo se ha il packing list
189700130319     c***                if        §1bf16='S'
189800130319     c***                eval      v1dar8='PACKING LIST'
189900130319     C***                ELSE
190000130319     C***                CLEAR                   V1DAR8
190100130319     C***                ENDIF
190200051104     c
190300051104     c                   select
190400051104     c                   when      blpffd='S'
190500051104     C                   MOVEL     'SI '         V2CFFD
190600051104     c                   when      blpffd='M'
190700051104     C                   MOVEL     'SAB'         V2CFFD
190800051104     C                   other
190900940905     C                   MOVEL     *BLANKS       V2CFFD
191000051104     C                   ENDsl
191100051104     c
191200940808     C                   MOVEL     BLPNFV        V1CNFV
191300940906     C                   MOVEL     BLPNRT        V1CNRT
191400940906     C                   MOVEL     *BLANKS       V1DFPP
191500940906     C     BLPFPP        IFEQ      'M'
191600010220     C                   MOVEL     'MAT'         V1DFPP
191700940906     C                   ELSE
191800010220     C                   MOVEL     'POM'         V1DFPP
191900940906     C                   ENDIF
192000910118     C* GIRO DATA SPEDIZIONE
192100940804     C                   MOVEL     BLPAAS        V1CDSP
192200940804     C                   MOVE      BLPMGS        V1CDSP
192300940804     C                   Z-ADD     V1CDSP        G02INV
192400940804     C                   Z-ADD     *ZEROS        G02DAT
192500940804     C                   MOVEL     '3'           G02ERR
192600940804     C                   CALL      'XSRDA8'
192700940804     C                   PARM                    WLBDAT
192800940804     C                   Z-ADD     G02DAT        V1CDSP
192900940808     C* DATA RITIRO MERCE
193000940804     C                   Z-ADD     BLPDRT        G02INV
193100940804     C                   Z-ADD     *ZEROS        G02DAT
193200940804     C                   MOVEL     '3'           G02ERR
193300940804     C                   CALL      'XSRDA8'
193400940804     C                   PARM                    WLBDAT
193500940804     C                   Z-ADD     G02DAT        V1CDRT
193600900518     C* DATA BORDERO'
193700940804     C                   Z-ADD     BLPDBR        G02INV
193800940804     C                   Z-ADD     *ZEROS        G02DAT
193900940804     C                   MOVEL     '3'           G02ERR
194000940804     C                   CALL      'XSRDA8'
194100940804     C                   PARM                    WLBDAT
194200940804     C                   Z-ADD     G02DAT        V1CDBR
194300940804     C* DATA PARTENZA PRIMO COLLO
194400940804     C                   Z-ADD     BLPDPC        G02INV
194500940804     C                   Z-ADD     *ZEROS        G02DAT
194600940804     C                   MOVEL     '3'           G02ERR
194700940804     C                   CALL      'XSRDA8'
194800940804     C                   PARM                    WLBDAT
194900940804     C                   Z-ADD     G02DAT        V1CDPC
195000940804     C* DATA PARTENZA ULTIMO COLLO
195100940804     C                   Z-ADD     BLPDUC        G02INV
195200940804     C                   Z-ADD     *ZEROS        G02DAT
195300940804     C                   MOVEL     '3'           G02ERR
195400940804     C                   CALL      'XSRDA8'
195500940804     C                   PARM                    WLBDAT
195600940804     C                   Z-ADD     G02DAT        V1CDUC
195700910806     C* DATA SPUNTA ENTRATA
195800940808     C     BLPDSE        IFGT      *ZERO
195900940808     C                   Z-ADD     BLPDSE        G02INV
196000940808     C                   Z-ADD     *ZEROS        G02DAT
196100940808     C                   MOVEL     '3'           G02ERR
196200940808     C                   CALL      'XSRDA8'
196300940808     C                   PARM                    WLBDAT
196400940808     C                   Z-ADD     G02DAT        V1CDSE
196500940808     C                   ENDIF
196600940805     C* CONSEGNA PARTICOLARE 1
196700940805     C                   MOVEL     BLPTC1        V3CTC1
196800940805     C                   MOVEL     *BLANKS       V3DTC1
196900940805     C     V3CTC1        IFNE      *BLANKS
197000940804     C                   MOVEL     '1P'          KCOD
197100940804     C                   MOVEL     *BLANKS       KKEY
197200940805     C                   MOVEL     BLPTC1        KKEY
197300940809     C     KTAB1         CHAIN     TABEL00F                           99
197400940805     C  N99              MOVEL     TBLUNI        V3DTC1
197500940805     C                   ENDIF
197600940805     C* CONSEGNA PARTICOLARE 2
197700940805     C                   MOVE      BLPTC2        V3CTC2
197800940805     C                   MOVEL     *BLANKS       V3DTC2
197900940805     C     V3CTC2        IFNE      *BLANKS
198000940805     C                   MOVEL     '1P'          KCOD
198100940805     C                   MOVEL     *BLANKS       KKEY
198200940805     C                   MOVEL     BLPTC2        KKEY
198300940809     C     KTAB1         CHAIN     TABEL00F                           99
198400940805     C  N99              MOVEL     TBLUNI        V3DTC2
198500940805     C                   ENDIF
198600910118     C* CODICE PADRONCINO
198700940808     C                   MOVEL     BLPPDR        V1CPDR
198800021203     C                   movel     BlpPdr        kpdpdr
198900021203     C                   move      'A'           kpdtip
199000021203     C     kfapd         chain     fiapd01L                           31
199100940808     C  N31              MOVEL     APDRSC        V1DPDR
199200940808     C   31              MOVEL     *BLANKS       V1DPDR
199300910118     C*
199400900511     C                   MOVEL     BLPSOP        VIDSOP
199500930614     C*
199600051124     c* Decodifico consegna richiesta
199700940808     C                   MOVEL     *BLANKS       V2CTCR
199800051124     c                   clear                   v2chcr
199900051124     c                   clear                   v2cdcr
200000051124     c                   if        blpdcr>0 or blphcr>0
200100051124     c                   clear                   fnlv80ds
200200051124     c                   eval      ilv80aas=blpaas
200300051124     c                   eval      ilv80lnp=blplnp
200400051124     c                   eval      ilv80nrs=blpnrs
200500051124     c                   eval      ilv80nsp=blpnsp
200600051124     c                   eval      ilv80mgs=blpmgs
200700051124     c                   eval      ilv80tc1=blptc1
200800051124     c                   eval      ilv80tc2=blptc2
200900051124     c                   eval      ilv80dcr=blpdcr
201000051124     c                   eval      ilv80hcr=blphcr
201100051124     c                   eval      ilv80tcr=blptcr
201200051124     c                   call      'FNLV80R'
201300051124     c                   parm                    kpjba
201400051124     c                   parm                    fnlv80ds
201500051124     c
201600051124     c                   eval      v2cdcr=olv80dcr8
201700051124     c                   eval      v2ctcr=olv80tcrd
201800051124     C                   MOVEL     blpHCR        V2cHCR
201900051124     c                   endif
202000940906     C*-------------------------------------------------------------------------
202100940906     C*
202200940906     C* L E G A M E   B O L L A
202300940906     C                   EXSR      LEGBOL
202400940808     C*-------------------------------------------------------------------------
202500050714     c* Dei seguenti dati,
202600050714     c* Prevalgono quelli di ARB se c'e' e se non presenti legami bolla
202700050714     C* DATA ENTRATA AL TRANSITO
202800050714     c                   if        wesarb='S' and not *in07
202900050714     c                   eval      wdet=arbdet
203000050714     c                   eval      wdut=arbdut
203100050714     c                   else
203200050714     c                   eval      wdet=blpdet
203300050714     c                   eval      wdut=blpdut
203400050714     c                   endif
203500050714     c
203600050714     C     wdet          IFGT      *ZERO
203700050714     C                   Z-ADD     wdet          G02INV
203800050714     C                   Z-ADD     *ZEROS        G02DAT
203900050714     C                   MOVEL     '3'           G02ERR
204000050714     C                   CALL      'XSRDA8'
204100050714     C                   PARM                    WLBDAT
204200050714     C                   Z-ADD     G02DAT        V1CDET
204300050714     C                   ENDIF
204400050714     C* DATA USCITA DAL TRANSITO
204500050714     C     wdut          IFGT      *ZERO
204600050714     C                   Z-ADD     wdut          G02INV
204700050714     C                   Z-ADD     *ZEROS        G02DAT
204800050714     C                   MOVEL     '3'           G02ERR
204900050714     C                   CALL      'XSRDA8'
205000050714     C                   PARM                    WLBDAT
205100050714     C                   Z-ADD     G02DAT        V1CDUT
205200050714     C                   ENDIF
205300050714     c* Mancata consegna
205400050714     c                   if        wesarb='S' and not *in07
205500140729     c                             and savnr9=0
205600050714     c                   eval      wtmc=arbtmc
205700050714     c                   else
205800050714     c                   eval      wtmc=blptmc
205900050714     c                   endif
206000050714     c
206100050714     C                   MOVEL     wtmc          V3CTMC
206200050714     C                   MOVEL     *BLANKS       V3DTMC
206300050714     C     wtmc          IFNE      ' '
206400050714     C                   MOVEL     '2A'          KCOD
206500050714     C                   MOVEL     *BLANKS       KKEY
206600050714     C                   MOVEL     wtmc          KKEY
206700050714     C     KTAB1         CHAIN     TABEL00F                           99
206800050714     C  N99              MOVEL     TBLUNI        V3DTMC
206900050714     C                   ENDIF
207000050714     c* Numero e data distinta consegna
207100050714     c                   if        wesarb='S'  and not *in07
207200140729     c                             and savnr9=0
207300050714     c                   eval      wndc=arbndc
207400050714     c                   eval      wddc=arbddc
207500050714     c                   else
207600050714     c                   eval      wndc=blpndc
207700050714     c                   eval      wddc=blpddc
207800050714     c                   endif
207900050714     C*
208000050714     C                   MOVEL     wndc          V3CNDC
208100050714     C                   Z-ADD     wddc          G02INV
208200050714     C                   Z-ADD     *ZEROS        G02DAT
208300050714     C                   MOVEL     '3'           G02ERR
208400050714     C                   CALL      'XSRDA8'
208500050714     C                   PARM                    WLBDAT
208600050714     C                   Z-ADD     G02DAT        V3CDDC
208700050714     C* DATA ARRIVO MERCE ALL FILIALE DI ARRIVO
208800050714     c                   clear                   wdam
208900050714     c                   if        wesarb='S' and not *in07
209000140729     c                             and savnr9=0
209100050714     c                   eval      wdam=arbdam
209200050714     c                   else
209300050714     c                   eval      wdam=blpdam
209400050714     c                   endif
209500050714     c
209600050714     C     wdam          IFGT      *ZERO
209700050714     C                   Z-ADD     wdam          G02INV
209800050714     C                   Z-ADD     *ZEROS        G02DAT
209900050714     C                   MOVEL     '3'           G02ERR
210000050714     C                   CALL      'XSRDA8'
210100050714     C                   PARM                    WLBDAT
210200050714     C                   Z-ADD     G02DAT        V1CDAM
210300050714     C                   ENDIF
210400050714     c
210500050714     C* DATA CONSEGNA MERCE REALE
210600050714     C                   Z-ADD     *ZERO         V1CDCM
210700050714     c                   if        wesarb='S' and not *in07
210800140729     c                             and savnr9=0
210900050714     c                   eval      wdcm=arbdcm
211000050714     c                   eval      wdce=arbdce
211100050714     c                   eval      wnrc=arbnrc
211200050714     c                   eval      wdcp=arbdcp
211300050714     c                   eval      wcca=arbcca
211400050714     c                   eval      whmc=arbhmc
211500050714     c                   else
211600050714     c                   eval      wdcm=blpdcm
211700050714     c                   eval      wdce=blpdce
211800050714     c                   eval      wnrc=blpnrc
211900050714     c                   eval      wdcp=blpdcp
212000050714     c                   eval      wcca=blpcca
212100050714     c                   eval      whmc=blphmc
212200050714     c                   endif
212300050714     c
212400050714     C     wdcm          IFGT      *ZERO
212500050714     C                   SETON                                        98
212600050714     C                   Z-ADD     wdcm          G02INV
212700050714     C                   Z-ADD     *ZEROS        G02DAT
212800050714     C                   MOVEL     '3'           G02ERR
212900050714     C                   CALL      'XSRDA8'
213000050714     C                   PARM                    WLBDAT
213100050714     C                   Z-ADD     G02DAT        V1CDCM
213200050714     C                   MOVEL     whmc          V1CHMC
213300050714     C                   ENDIF
213400050714     C* DATA CONSEGNA PREVISTA ESTERNA E ORE DI RITARDO
213500050714     C                   Z-ADD     0             V1CDCE
213600050714     C                   Z-ADD     0             V1CNRC
213700050714     C     wdce          IFEQ      0
213800050714     C                   SETON                                        94
213900050714     C                   ELSE
214000050714     C                   Z-ADD     wdce          G02INV
214100050714     C                   Z-ADD     *ZEROS        G02DAT
214200050714     C                   MOVEL     '3'           G02ERR
214300050714     C                   CALL      'XSRDA8'
214400050714     C                   PARM                    WLBDAT
214500050714     C                   Z-ADD     G02DAT        V1CDCE
214600050714     C*
214700050714     C                   Z-ADD     wnrc          V1CNRC
214800050714     C                   END
214900050714     C*
215000050714     C* DATA PRIMA CONSEGNA PARZIALE
215100050714     C     wdcp          IFGT      0
215200050714     C                   Z-ADD     wdcp          G02INV
215300050714     C                   Z-ADD     *ZEROS        G02DAT
215400050714     C                   MOVEL     '3'           G02ERR
215500050714     C                   CALL      'XSRDA8'
215600050714     C                   PARM                    WLBDAT
215700050714     C                   Z-ADD     G02DAT        V3CDCP
215800050714     C                   END
215900050714     C* CODICE CONSEGNA ANOMALA
216000050714     C                   MOVEL     wcca          V3CCCA
216100050714     C                   MOVE      *BLANKS       V3DCCA
216200050714     C                   MOVE      *BLANKS       V7DCCA
216300050714     C     wcca          IFNE      ' '
216400050714     C                   MOVEL     '7O'          KCOD
216500050714     C                   MOVEL     *BLANKS       KKEY
216600050714     C                   MOVEL     wcca          KKEY
216700050714     C     KTAB1         CHAIN     TABEL00F                           99
216800050714     C  N99              MOVEL     TBLUNI        V3DCCA
216900050714     C  N99              MOVEL     TBLUNI        V7DCCA
217000050714     C                   END
217100050714     c*******
217200991022      *   SE ESISTE AGGANCIO  2 °   B O L L A
217300991022      *
217400920409     C     §3ATB2        IFNE      '  '
217500991022      *
217600991022     c                   eval      KTRC = '2'
217700991022     C     KAR6          CHAIN     FIAR6000
217800991022      *
217900991022     C                   IF        %FOUND
218000991022     C                   eval        *IN06 = *ON
218100991022     C                   eval      VI2fiv = AR6fiv
218200991022     C                   eval      VI2nft = AR6nft
218300991022     C                   eval      VI2cei = AR6cei
218400991022      *
218500991022     C                   eval      VI2DIV = AR6div
218600991022     C                   eval      WDIV = AR6div
218700991022     C                   exsr      CTR_VALUTA
218800991022      *
218900991027     c                   if        AR6fim = 'P'
219000991027     c                   eval      VI2fim = 'PARTENZA'
219100040116     c                   eval      VI2fi2 = 'Immesse in'
219200991022     c                   endif
219300991022      * Varia G
219400040116     c**!!!              clear                   W0133
219500040116     c**!!!              SELECT
219600040116     c**!!!              WHEN        AR6sv1 = §$2vrg
219700040116     C**!!!              eval      VI2sv1 = AR6sv1
219800040116     c**!!!              z-add     AR6va1        W0133
219900040116     c**!!!              WHEN        AR6sv2 = §$2vrg
220000040116     C**!!!              eval      VI2sv1 = AR6sv2
220100040116     c**!!!              z-add     AR6va2        W0133
220200040116     c**!!!              WHEN        AR6sv3 = §$2vrg
220300040116     C**!!!              eval      VI2sv1 = AR6sv3
220400040116     c**!!!              z-add     AR6va3        W0133
220500040116     c**!!!              ENDSL
220600991022      *
220700040116     c**!!!              if        W0133 > *zeros
220800040116     c**!!!              eval      Wtstdec = §cvfdc
220900040116     c**!!!              exsr      EDTimporti
221000040116     c**!!!              movel     IMPeditato    VI2va1
221100040116     c**!!!              endif
221200991022      * IVA
221300040116     c**!!!              clear                   W0133
221400040116     c**!!!              SELECT
221500040116     c**!!!              WHEN        AR6sv1 = §$2iva
221600040116     c**!!!              z-add     AR6va1        W0133
221700040116     c**!!!              WHEN        AR6sv2 = §$2iva
221800040116     c**!!!              z-add     AR6va2        W0133
221900040116     c**!!!              WHEN        AR6sv3 = §$2iva
222000040116     c**!!!              z-add     AR6va3        W0133
222100040116     c**!!!              ENDSL
222200991022      *
222300040116     c**!!!              IF        W0133 > *zeros
222400040116     c**!!!              eval      *in11 = *off
222500040116     c**!!!              eval      Wtstdec = §cvfdc
222600040116     c**!!!              exsr      EDTimporti
222700040116     c**!!!              movel     IMPeditato    VI2iva
222800040116     c**!!!              ELSE
222900991123      * BOLLO se non ho iva
223000040116     c**!!!              SELECT
223100040116     c**!!!              WHEN        AR6sv1 = §$2bol
223200040116     c**!!!              z-add     AR6va1        W0133
223300040116     c**!!!              WHEN        AR6sv2 = §$2bol
223400040116     c**!!!              z-add     AR6va2        W0133
223500040116     c**!!!              WHEN        AR6sv3 = §$2bol
223600040116     c**!!!              z-add     AR6va3        W0133
223700040116     c**!!!              ENDSL
223800991123      *
223900040116     c**!!!              IF        W0133 > *zeros
224000040116     c**!!!              eval      *in11 = *on
224100040116     c**!!!              eval      Wtstdec = §cvfdc
224200040116     c**!!!              exsr      EDTimporti
224300040116     c**!!!              movel     IMPeditato    VI2iva
224400040116     c**!!!              ENDIF
224500991123      *
224600040116     c**!!!              ENDIF
224700040116
224800040116      * Carico le varie
224900040116     c                   Movel     Ar6Sv1        wsvn
225000040116     c                   Z-add     Ar6Va1        wvan
225100040116     c                   ExSr      Car_Varie1
225200040116
225300040116     c                   Movel     Ar6Sv2        wsvn
225400040116     c                   Z-add     Ar6Va2        wvan
225500040116     c                   ExSr      Car_Varie1
225600040116
225700040116     c                   Movel     Ar6Sv3        wsvn
225800040116     c                   Z-add     Ar6Va3        wvan
225900040116     c                   ExSr      Car_Varie1
226000040116
226100040116     c     Kar6          Setll     Fiar7000
226200040116     c                   Do        *Hival
226300040116     c     Kar6          Reade     Fiar7000
226400040116     c                   If        %Eof(Fiar701l)
226500040116     c                   Leave
226600040116     c                   EndIf
226700040116     c                   Movel     Ar7Svn        wsvn
226800040116     c                   Z-add     Ar7Van        wvan
226900040116     c                   ExSr      Car_Varie1
227000040116     c                   EndDo
227100040116
227200991025      * TOTALE FATTURA
227300991025     c                   clear                   W0133
227400991025     c                   IF          AR6ift > *zeros
227500991025     c                   z-add     AR6ift        W0133
227600991025     c                   eval      Wtstdec = §cvfdc
227700991025     c                   exsr      EDTimporti
227800991025     c                   movel     IMPeditato    VI2ift
227900991025     c                   endif
228000991022      *
228100991022     c                   If         AR6dft > *zeros
228200991022     C     *iso          movel     AR6DFT        DATA_EUR
228300991022     C     *eur          movel     data_eur      W0080
228400991022     C                   eval      VI2dft = %EDITC(W0080:'Y')
228500991022     c                   Endif
228600991022      *  decodifica esenzione IVA
228700991022     C                   if        VI2CEI <> *blanks
228800940808     C                   MOVEL     'EI'          KCOD
228900991022     C                   MOVEL(P)  VI2CEI        KKEY
229000940809     C     KTAB1         CHAIN     TABEL00F                           31
229100991022     C  N31              MOVEL     TBLUNI        VI2DCE
229200940808     C                   ENDIF
229300991022      *
229400991022     c                   eval      VI2ksc = AR6ksc
229500991022     c                   eval      VI2dks = BLPrsd
229600991022     c                   eval      VI2ctr = AR6ctr
229700991022      *
229800991022     C                   eval      KCOD = 'TR'
229900991022     C                   movel     VI2CTR        UNOCTR
230000991022     C                   eval      KKEY = unoctr
230100991022     C     KTAB1         CHAIN     TABEL00F                           31
230200991022     C                   movel     TBLUNI        VI2dct
230300991022      *
230400991022     C                   ENDIF
230500940808     C                   ENDIF
230600951219     C*-------------------------------------------------------------------------
230700951219     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
230800951219     C*
230900951219     C* PULISCO IL SFL
231000951219     C                   SETON                                        26
231100951219     C                   WRITE     LS05C03
231200951219     C                   SETOFF                                       26
231300951219     C*
231400060216     C* CHIAVE PER ACCEDERE AL FILE Fiar401L
231500951219     C                   MOVEL     BLPAAS        KAAS
231600951219     C                   MOVEL     BLPLNP        KLNP
231700951219     C                   MOVEL     BLPNRS        KNRS
231800951219     C                   MOVEL     BLPNSP        KNSP
231900951219     C*
232000951219     C                   Z-ADD     0             NRR3              4 0
232100951219     C                   MOVEL     '7K'          KCOD
232200951219     C*
232300060216     C     Kar42         SETLL     Fiar401L
232400060216     C     Kar42         READE     Fiar401L                               99
232500951219     C     *IN99         DOWEQ     *OFF
232600951219     C*
232700060216     C                   MOVEL(P)  ar4TRC        KKEY
232800951219     C     KTAB1         CHAIN     TABEL                              99
232900951219     C*
233000951219     C     *IN99         IFEQ      *OFF
233100951219     C     TBLFLG        ANDEQ     ' '
233200951219     C                   MOVEL     TBLUNI        DS7K
233300951219     C     §7KVIS        IFEQ      'S'
233400951219     C                   MOVEL     §7KDES        VS3DES
233500060216     C                   MOVEL     ar4NOT        VS3NOT
233600951219     C                   ADD       1             NRR3
233700951219     C                   WRITE     LS05S03
233800951219     C                   ENDIF
233900951219     C                   ENDIF
234000951219     C*
234100060216     C     Kar42         READE     Fiar401L                               99
234200951219     C                   ENDDO
234300951219     C*
234400951219     C* SE NON HO CARICATO NIENTE SCRIVO UGUALMENTE UN RECORD VUOTO
234500951219     C*  NEL SFL PER EVITARE CHE IL PGM DIA ERRORE DI SESSIONE O UNITA'
234600951219     C     NRR3          IFEQ      0
234700951219     C                   ADD       1             NRR3
234800951219     C                   WRITE     LS05S03
234900951219     C                   ENDIF
235000951219     C*
235100940808     C*-------------------------------------------------------------------------
235200930526     C* 95 ON - ESITONO EVENTI DA INTERROGARE
235300111020     C**!!!PA0CLI        IFNE      'S'
235400991022     C     KBLP3         SETLL     FNEVB000                               95
235500111020     C**!!!              ENDIF
235600940805     C*-------------------------------------------------------------------------
235700110705     C                   clear                   SAVNR8
235800110705     C                   eval      *in04 = *off
235900110705     C                   eval      *in84 = *off
236000110705     c
236100110705     c                   clear                   v8car4ksc
236200110705     c                   clear                   v8dar4ksc
236300110705     c                   clear                   v8car4ctr
236400110705     C* Verifico se presete cod cliente e cod tariffa di tassazione da bollettaz
236500110705     C                   MOVEL     'A'           KTRC
236600110705     C     Kar41         CHAIN     Fiar401L                           99
236700110705     C     *IN99         IFEQ      *OFF
236800110705     c                   movel     ar4not        dsbl4a
236900110705     c                   if        §b4aks>0
237000110705     C                   eval      *in84 = *on
237100110705     C                   eval      *in04 = *on
237200110705     c                   eval      v8car4ksc=§b4aks
237300110705     c                   eval      i69kac = v8car4ksc
237400110705     c                   exsr      CTR_ANAGRA
237500110705     c                   eval      v8dar4ksc = acorag
237600110705     c                   if        §b4act<>*blanks
237700110705     c                   movel     §b4act        v8car4ctr
237800110705     c                   endif
237900110705     c                   endif
238000110705     c                   endif
238100110705     c
238200991022      * PER I DATI SULLA TASSAZIONE, CERCO SU FIAR600F.
238300991022      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SOLO SE IMPOSTATO ALMENO UN CAMPO
238400991022     C                   eval      Ktrc = '1'
238500991022     C     KAR6          CHAIN     FIAR601L
238600110705      * CLIENTE E TARIFFA FATTURAZIONE
238700110705     c                   IF        %found(fiar601l)
238800110705     c
238900110705     c                   IF        *IN33 = *ON
239000110705     c                   eval      v8cksc = blpksc
239100110705     c                   eval      v8cctr = blpctr
239200110705     c                   eval      v8dksc = blprsm
239300110705     c                   ELSE
239400110705     c                   eval      v8cksc = AR6ksc
239500110705     c                   eval      v8cctr = AR6ctr
239600110705     c                   move      v8cksc        W0040
239700110705     c                   if        W0040 = 9999
239800110705     c                   eval      v8dksc = blprsd
239900110705     c                   else
240000110705     c                   eval      i69kac = v8cksc
240100110705     c                   exsr      CTR_ANAGRA
240200110705     c                   eval      v8dksc = acorag
240300110705     c                   endif
240400110705     c                   ENDIF
240500110705     c
240600110705      *   decodifica tariffa
240700110705     C                   eval      KCOD = 'TR'
240800110705     C                   MOVEL     V2CCTR        UNOCTR            1
240900110705     C                   MOVEL(P)  UNOCTR        KKEY
241000110705     C     KTAB1         CHAIN     TABEL00F                           31
241100110705     C                   MOVEL     TBLUNI        V8DCTR
241200110705     c
241300110705     c                   ENDIF
241400991022      *
241500991022     C                   IF        %FOUND  and
241600991022     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
241700991022     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
241800991022     C                              AR6sv3 <> *blanks)
241900991022      *
242000991022     C                   eval      *in04 = *on
242100991022      *
242200991025     C                   eval      V8CDIV = AR6div
242300991022     C                   eval      WDIV = AR6div
242400991022     C                   exsr      CTR_VALUTA
242500991022      *
242600991022     c                   IF          ar6por > *zeros
242700991022     c                   z-add     AR6por        W0133
242800991022     c                   eval      Wtstdec = §cvfdc
242900991022     c                   exsr      EDTimporti
243000991022     c                   movel     IMPeditato    V8Cpor
243100991022     c                   ENDIF
243200991022      *
243300991022     c                   IF          ar6imv > *zeros
243400991022     c                   z-add     AR6imv        W0133
243500991022     c                   eval      Wtstdec = §cvfdc
243600991022     c                   exsr      EDTimporti
243700991022     c                   movel     IMPeditato    V8Cimv
243800991022     c                   ENDIF
243900991022      *
244000991022     c                   IF          ar6ift > *zeros
244100991022     c                   z-add     AR6ift        W0133
244200991022     c                   eval      Wtstdec = §cvfdc
244300991022     c                   exsr      EDTimporti
244400991022     c                   movel     IMPeditato    V8Cift
244500991022     c                   ENDIF
244600991022      *
244700991027     c                   IF        AR6fim = 'P'
244800991022     c                   eval      V8Dfim = 'PARTENZA'
244900991022    2C                   ENDIF
245000991022      *
245100991022     C                   MOVEL     AR6CEI        V8CCEI
245200991022      *
245300991022      * SE C'E', DECODIFICO L'ESENZIONE IVA
245400991022     C                   clear                   V8DCEI
245500991022     C                   IF        AR6CEI <>  *BLANKS
245600991022     C                   MOVEL     'EI'          KCOD
245700991022     C                   MOVEL(P)  AR6CEI        KKEY
245800991022     C     KTAB1         CHAIN     TABEL00F                           99
245900991022     C  N99              MOVEL     TBLUNI        V8DCEI
246000991022    2C                   ENDIF
246100991022      *
246200991022      * DATA FATTURA DA GIRARE
246300991022     C                   IF         ar6dft > *zeros
246400991022     C     *iso          movel     AR6DFT        DATA_EUR
246500991022     C     *eur          movel     data_eur      W0080
246600991022     C                   eval      V8Cdft = %EDITC(W0080:'Y')
246700991022     C                   ENDIF
246800991022     C                   MOVEL     ar6NFT        V8CNFT
246900991022     C                   MOVEL     ar6FIV        V8CFIV
247000991022      *
247100991022      * CERCO LE VARIE
247200991022     C                   eval      *in20 = *off
247300991022     C                   eval      *in21 = *off
247400991022     C                   WRITE     LS05C08
247500991022     C                   eval      *in20 = *on
247600991022     C                   eval      *in21 = *on
247700991022      *
247800991022     c                   clear                   nrr8
247900991022      *
248000991022     C                   MOVEL     ar6SV1        WSVN
248100991022     C                   MOVEL     ar6VA1        WVAN
248200991022     c                   exsr      car_varie
248300991022      *
248400991022     C                   MOVEL     ar6SV2        WSVN
248500991022     C                   MOVEL     ar6VA2        WVAN
248600991022     c                   exsr      car_varie
248700991022      *
248800991022     C                   MOVEL     ar6SV3        WSVN
248900991022     C                   MOVEL     ar6VA3        WVAN
249000991022     c                   exsr      car_varie
249100991022      *
249200991022      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
249300991022     C                   if          ar6SV3 <> *blanks
249400991022     C     KAR6          SETLL     FIAR7000
249500991022     C     KAR6          READE     FIAR7000
249600991022     C                   DOW       %EOF = *OFF
249700991022     C                   MOVEL     AR7SVN        WSVN
249800991022     C                   MOVEL     AR7VAN        WVAN
249900991022     c                   exsr      car_varie
250000991022     C     KAR6          READE     FIAR7000
250100991022     C                   ENDDO
250200991022     C                   endif
250300110705     C                   ENDIF
250400991022      *
250500991022      * Se non ho caricato record ne carico uno vuoto
250600110705     c                   IF        NRR8 = *ZEROS  and *in04
250700991022     C                   clear                   V8CDES
250800991022     C                   clear                   V8CVA1
250900991022     C                   clear                   V8CSV1
251000991022     C                   add       1             NRR8
251100991022     C                   WRITE     LS05S08
251200991022     c                   ENDIF
251300991022      *
251400991022     C                   Z-ADD     NRR8          SAVNR8
251500991022     C                   Z-ADD     1             NRR8
251600991022      *
251700991022      *-------------------------------------------------------------------------
251800991022      * DECODIFICO IL CODICE TASSAZIONE, SE C'E'
251900991022     C                   clear                   V8DCTS
252000991022     C                   if         V8CCTS <> *blanks
252100991022     C                   MOVEL     'CT'          KCOD
252200991022     C                   MOVEL(P)  V8CCTS        KKEY
252300991022     C     KTAB1         CHAIN     TABEL00F                           99
252400991022     C  N99              MOVEL     TBLUNI        V8DCTS
252500991022     C                   endif
252600940805     C*-------------------------------------------------------------------------
252700940805     C* PRELEVO LE NOTE DAL FILE APPOSITO
252800940805     C                   MOVEL     BLPAAS        KAAS
252900940805     C                   MOVEL     BLPLNP        KLNP
253000940805     C                   MOVEL     BLPNRS        KNRS
253100940805     C                   MOVEL     BLPNSP        KNSP
253200940805     C                   MOVEL     '8'           KTRC
253300060216     C     Kar41         CHAIN     Fiar401L                           99
253400060216     C  N99              MOVEL     ar4NOT        V2CNO1
253500940805     C   99              MOVEL     *BLANKS       V2CNO1
253600940805     C                   MOVEL     '9'           KTRC
253700060216     C     Kar41         CHAIN     Fiar401L                           99
253800060216     C  N99              MOVEL     ar4NOT        V2CNO2
253900940805     C   99              MOVEL     *BLANKS       V2CNO2
254000991022      *
254100991022      *-------------------------------------------------------------------------
254200051115      * IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU Fiar900F
254300991022      * CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
254400940805     C                   MOVEL     *BLANKS       V2CTIC
254500940805     C                   MOVEL     *ZERO         V2CCAS
254600940805     C                   MOVEL     *BLANKS       V2CVCA
254700940805     C                   MOVEL     *BLANKS       V2CGCA
254800091214     c                   setoff                                       43
254900940923     C                   MOVEL     *BLANKS       V2IGCA
255000940805     C     §3AFCA        IFGT      *ZERO
255100051115     C     KBLP3         CHAIN     Fiar901L                           99
255200940805     C  N99              DO
255300051115     C                   MOVEL     ar9TIC        V2CTIC
255400051115     C                   MOVEL     ar9CAS        V2CCAS
255500051115     C                   MOVEL     ar9VCA        V2CVCA
255600051115     C                   MOVEL     ar9GCA        V2CGCA
255700091214     c                   seton                                        43
255800940805     C                   ENDDO
255900940805     C                   ENDIF
256000991022      * SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
256100940923     C     V2CGCA        COMP      *BLANKS                            1616
256200021218      *
256300021218      * Riempio il campone in fondo alla videata con i tasti scelti
256400021218     C                   exsr      Ges_TastiFun
256500021218     c                   eval      FD07 = FDxx
256600021218     c                   eval      F24D07 = F24Dxx
256700991022      *
256800940923     C     ENDBLP        ENDSR
256900030703      *-------------------------------------------------------------------------
257000030703      *  Decoditifa tipo bancali
257100030703      *-------------------------------------------------------------------------
257200030703     c     Sr_DecTba     BegSr
257300030703     c                   Clear                   Tibs02Ds
257400030703     c                   Clear                   dTba
257500030703     c                   Movel     'C'           T02Mod
257600030703     c                   Movel     Knsif         T02Sif
257700030703     c                   Movel     'TBA'         T02Cod
257800030703     c                   Movel(p)  wtba          T02Ke1
257900030703     c                   Call      'TIBS02R'
258000030703     c                   Parm                    Kpjba
258100030703     c                   Parm                    Tibs02Ds
258200030703If  3c                   If        T02Err = *Blanks
258300030703     c                   Movel     T02Uni        dTba
258400030703    3c                   EndIf
258500030703     c                   Eval      wdestba = §TbaDesc
258600030703     c                   EndSr
258700021218      *-------------------------------------------------------------------------
258800021218      *  Imposto descrizione tasti funzione
258900021218      *-------------------------------------------------------------------------
259000021218     c     Ges_TastiFun  BEGSR
259100021218      *
259200021218      * per emettere i tasti senza condizionamenti ..
259300021218     c                   setoff                                       25
259400021218      *
259500021218      * Conta i caratteri riempiti dalle RigaTf1
259600021218     c                   z-add     0             $z                3 0
259700021218      *
259800021218      * Conta i caratteri riempiti dalle RigaTf2
259900021218     c                   z-add     0             $k                3 0
260000021218      *
260100021218      * Conta i caratteri riempiti dalle RigaTf3
260200021218     c                   z-add     0             $j                3 0
260300021218      *
260400021218      * Conta la posizione del campone da cui partire per impostare
260500021218      * la descrizione del tasto funzione
260600021218     c                   z-add     0             $y                2 0
260700021218      *
260800021218      * Stringhe che contengono le descrizioni dei tasti funzione
260900021218     c                   reset                   RigaTf1
261000021218     c                   reset                   RigaTf2
261100021218     c                   reset                   RigaTf3
261200021218      *
261300021218      * F13
261400021218     c                   IF        *in04 = *on
261500021218     c                   reset                   $Tf
261600021218     c                   movea     Tf13          $Tf
261700021218     c                   Exsr      Rie_$Tf
261800021218     C                   ENDIF
261900021218      *
262000021218      * F04
262100021218     c                   IF        *in06 = *on
262200021218     c                   reset                   $Tf
262300021218     c                   movea     Tf04          $Tf
262400021218     c                   Exsr      Rie_$Tf
262500021218     C                   ENDIF
262600040220      *
262700040220      *?F16?
262800040220     c                   IF        *in28 = *on
262900040220     c                   reset                   $Tf
263000040220     c                   movea     Tf16          $Tf
263100040220     c                   exsr      Rie_$Tf
263200040220     c                   ENDIF
263300021218      *
263400021218      * F05
263500021218     c                   IF        *in95 = *on
263600021218     c                   reset                   $Tf
263700021218     c                   movea     Tf05          $Tf
263800021218     c                   Exsr      Rie_$Tf
263900021218     C                   ENDIF
264000021218      *
264100021218      * F10
264200021218     c                   IF        *in10 = *on
264300021218     c                   reset                   $Tf
264400021218     c                   movea     Tf10          $Tf
264500021218     c                   Exsr      Rie_$Tf
264600021218     c                   ENDIF
264700021218      *
264800021218      * F14
264900021218     c                   IF        *in03 = *on
265000021218     c                   reset                   $Tf
265100021218     c                   movea     Tf14          $Tf
265200021218     c                   Exsr      Rie_$Tf
265300021218     c                   ENDIF
265400051201      *
265500051201      * F22
265600051201     c                   IF        *in24 = *on
265700051201     c                   reset                   $Tf
265800051201     c                   movea     Tf22          $Tf
265900051201     c                   Exsr      Rie_$Tf
266000051201     c                   ENDIF
266100021218      *
266200021218      * F02
266300021218     c                   IF        *in12 = *on
266400021218     c                   reset                   $Tf
266500021218     c                   movea     Tf02          $Tf
266600021218     c                   Exsr      Rie_$Tf
266700021218     c                   ENDIF
266800021218      *
266900021218      * F11
267000021218     c                   IF        *in77 = *on
267100021218     c                   reset                   $Tf
267200021218     c                   movea     Tf11          $Tf
267300021218     c                   Exsr      Rie_$Tf
267400021218     c                   ENDIF
267500021218      *
267600021218      * F07
267700021218     c                   IF        *in22 = *on and *in05 = *off
267800021218     c                   reset                   $Tf
267900021218     c                   movea     Tf07          $Tf
268000021218     c                   Exsr      Rie_$Tf
268100021218     c                   ENDIF
268200021218      *
268300021218      * F08
268400021218     c                   IF        *in55 = *on
268500021218     c                   reset                   $Tf
268600021218     c                   movea     Tf08          $Tf
268700021218     c                   Exsr      Rie_$Tf
268800021218     c                   ENDIF
268900130912
269000130912      * F09 --> Orari servizi non condizionato
269100131007     c                   IF        *in25 = *off and wabif9='S'
269200130930     c                   reset                   $Tf
269300130930     c                   movea     Tf09          $Tf
269400130930     c                   Exsr      Rie_$Tf
269500130930     c                   ENDIF
269600021218      *
269700021218      * F20
269800021218     c                   IF        *in22 = *off
269900021218     c                   reset                   $Tf
270000021218     c                   movea     Tf20          $Tf
270100021218     c                   Exsr      Rie_$Tf
270200021218     c                   ENDIF
270300021218      *
270400021218      * F21 --> non condizionato
270500021218     c                   IF        *in25 = *off
270600021218     c                   reset                   $Tf
270700021218     c                   movea     Tf21          $Tf
270800021218     c                   Exsr      Rie_$Tf
270900021218     c                   ENDIF
271000040220      *
271100040220      * F19
271200040220     c                   IF        *in22 = *off
271300040220     c                   reset                   $Tf
271400040220     c                   movea     Tf19          $Tf
271500040220     c                   Exsr      Rie_$Tf
271600040220     c                   ENDIF
271700040220      *
271800040220      *?F01?
271900040220     c                   IF        *in27 = *on
272000040220     c                   reset                   $Tf
272100040220     c                   movea     Tf01          $Tf
272200040220     c                   exsr      Rie_$Tf
272300040220     c                   ENDIF
272400021218      *
272500021218      * F12 --> non condizionato
272600021218     c                   IF        *in25 = *off
272700021218     c                   reset                   $Tf
272800021218     c                   movea     Tf12          $Tf
272900021218     c                   Exsr      Rie_$Tf
273000021218     c                   ENDIF
273100021218      *
273200021218      * Pulisco le stringa
273300021218     c                   z-add     1             $h
273400030121     c     1             do        62            $h                2 0
273500021218     C                   if        RigaTf1($h) = '#'
273600021218     c                   movel     *blanks       RigaTf1($h)
273700021218     c                   endif
273800021218     c                   enddo
273900021218     c                   z-add     1             $h
274000030121     c     1             do        62            $h                2 0
274100021218     C                   if        RigaTf2($h) = '#'
274200021218     c                   movel     *blanks       RigaTf2($h)
274300021218     c                   endif
274400021218     c                   enddo
274500021218     c                   z-add     1             $h
274600030121     c     1             do        62            $h                2 0
274700021218     C                   if        RigaTf3($h) = '#'
274800021218     c                   movel     *blanks       RigaTf3($h)
274900021218     c                   endif
275000021218     c                   enddo
275100021218      *
275200021218      * Imposto la descrizione del tasto funzione F24 e segnalo
275300021218      * quante righe ho riempito e quale devo emettere
275400021218     c                   SELECT
275500021218      *
275600021218     c                   when      $k<>*zeros and $Y<>*zeros
275700021218     c                   eval      $loop = 3
275800021218     c                   z-add     3             $loop             1 0
275900021218     C                   movel(p)  CF2413        F24Dxx           16
276000021218      *
276100021218     c                   when      $k<>*zeros and $Y=*zeros
276200021218     c                   z-add     2             $loop             1 0
276300021218     C                   movel(p)  CF2412        F24Dxx
276400021218      *
276500021218     c                   Other
276600021218     c                   z-add     1             $loop             1 0
276700021218     C                   movel     *blanks       F24Dxx
276800021218     c                   ENDSL
276900021218      * Prima riga di tasti funzione
277000030121     c                   movea     RigaTF1       FDxx             62
277100021218     c                   z-add     1             $Rig              1 0
277200021218      *
277300021218     c                   endsr
277400021218      *-------------------------------------------------------------------------
277500021218      *  Riempio le stringhe con i campi funzione compattati
277600021218      *-------------------------------------------------------------------------
277700021218     c     Rie_$Tf       begsr
277800021218      *
277900021218     c                   z-add     1             $x                2 0
278000021218     c     '#'           lookup    $Tf($x)                                31
278100021218     c                   add       $x            $Z
278200030121     c                   IF        $Z <= 63
278300021218     c                   eval      $j = $Z - $x + 1
278400021218     c                   movea     $Tf           RigaTF1($j)
278500021218     c                   ELSE
278600021218     c                   add       $x            $K
278700030121     c                   IF        $K <= 63
278800021218     c                   eval      $j = $K - $x + 1
278900021218     c                   movea     $Tf           RigaTF2($j)
279000021218     c                   ELSE
279100021218     c                   add       $x            $Y
279200030121     c                   IF        $Y <= 63
279300021218     c                   eval      $j = $Y - $x + 1
279400021218     c                   movea     $Tf           RigaTF3($j)
279500021218     c                   ENDIF
279600021218     c                   ENDIF
279700021218     c                   ENDIF
279800021218      *
279900021218     c                   endsr
280000021218      *-------------------------------------------------------------------------
280100021218      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
280200021218      *-------------------------------------------------------------------------
280300021218     c     Ges_F24       begsr
280400021218      *
280500021218     c                   SELECT
280600021218      *
280700021218     c                   WHEN      $Loop = 2 and $Rig =2  or
280800021218     c                             $Loop = 3 and $Rig =3
280900021218     c                   movea     RigaTF1       FDxx
281000021218     c                   z-add     1             $Rig
281100021218      *
281200021218     c                   if        $loop = 2
281300021218     c                   eval      F24Dxx= Cf2412
281400021218     c                   else
281500021218     c                   eval      F24Dxx= Cf2413
281600021218     c                   endif
281700021218      *
281800021218     c                   WHEN      $Loop = 2 and $Rig =1 or
281900021218     c                             $Loop = 3 and $Rig =1
282000021218     c                   movea     RigaTF2       FDxx
282100021218     c                   z-add     2             $Rig
282200021218      *
282300021218     c                   if        $loop = 2
282400021218     c                   eval      F24Dxx= Cf2422
282500021218     c                   else
282600021218     c                   eval      F24Dxx= Cf2423
282700021218     c                   endif
282800021218      *
282900021218     c                   WHEN      $Loop = 3 and $Rig =2
283000021218     c                   movea     RigaTF3       FDxx
283100021218     c                   z-add     3             $Rig
283200021218     c                   eval      F24Dxx= Cf2433
283300021218     c                   ENDSL
283400021218      *
283500021218     c                   endsr
283600991022      *-------------------------------------------------------------------------
283700991022      *  CARICO SUBFILE VARIE
283800991022      *-------------------------------------------------------------------------
283900991022     c     CAR_VARIE     BEGSR
284000991022      *
284100991022     c                   SELECT
284200991022      *
284300991022     c                   WHEN       WSVN = *BLANKS
284400991022      *
284500991022     C                   WHEN       WSVN = §$2iva
284600991022     c                   z-add     Wvan          W0133
284700991022     c                   eval      Wtstdec = §cvfdc
284800991022     c                   exsr      EDTimporti
284900991025     c                   movel     IMPeditato    V8Civa
285000991022      *
285100991022     c                   OTHER
285200991022     c                   z-add     Wvan          W0133
285300991022     c                   eval      Wtstdec = §cvfdc
285400991022     c                   exsr      EDTimporti
285500991022     c                   movel     IMPeditato    V8Cva1
285600991022     c                   movel     WSVN          V8Csv1
285700991022     C                   Z-ADD     1             WA
285800991022     C     V8CSV1        LOOKUP    CC1(WA)                                99
285900020329     C     CC3(WA)       ifeq      'S'
286000020329      *
286100020329      * Verifico se si tratta di una bolla Fedex
286200021119     c                   if        LnaNtw = 'FED'
286300020329     C                   Z-ADD     1             WA
286400020329     C     V8CSV1        LOOKUP    CB1(WA)                                99
286500020329     C   99              MOVEL     CB2(WA)       V8CDES
286600020329     C  N99              clear                   V8CDES
286700020329     C                   else
286800991022     C   99              MOVEL     CC2(WA)       V8CDES
286900991022     C  N99              clear                   V8CDES
287000020329     C                   endif
287100020329     C                   else
287200020329     C   99              MOVEL     CC2(WA)       V8CDES
287300020329     C  N99              clear                   V8CDES
287400020329     C                   endif
287500991022     C                   add       1             NRR8
287600991022     C                   WRITE     LS05S08
287700991022      *
287800991022     c                   ENDSL
287900991022      *
288000991022     c                   ENDSR
288100040116      *-------------------------------------------------------------------------
288200040116      *  Carico le varie della seconda bolla
288300040116      *-------------------------------------------------------------------------
288400040116     c     Car_Varie1    BegSr
288500040116
288600040116s   1c                   Select
288700040116
288800040116     c                   When      wsvn = *Blanks
288900040116      * Iva
289000040116     c                   When      wsvn = §$2iva
289100040116     c                   z-add     wvan          w0133
289200040116     c                   Eval      Wtstdec = §cvfdc
289300040116     c                   ExSr      EDTimporti
289400040116     c                   Movel     IMPeditato    Vi2Iva
289500040116      * Bollo
289600040116     c                   When      wsvn = §$2bol
289700040116     c                   z-add     wvan          w0133
289800040116     c                   Eval      Wtstdec = §cvfdc
289900040116     c                   ExSr      EDTimporti
290000040116     c                   Movel     IMPeditato    Vi2Iva
290100040116      * Varia G - sempre per prima
290200040116     c                   When      wsvn = §$2vrg
290300040116     c                   Eval      Vi2Sv1 = wsvn
290400040116     c                   z-add     wvan          w0133
290500040116     c                   Eval      Wtstdec = §cvfdc
290600040116     c                   ExSr      EDTimporti
290700040116     c                   Movel     IMPeditato    Vi2Va1
290800040116      * Altre varie
290900040116   x1c                   Other
291000040116     c                   z-add     wvan          W0133
291100040116     c                   eval      Wtstdec = §cvfdc
291200040116     c                   exsr      EDTimporti
291300040116if  2c                   If        Vi2Sv2 = *Blanks
291400040116     c                   movel     IMPeditato    Vi2Va2
291500040116     c                   movel     wsvn          Vi2sv2
291600040116   x2c                   Else
291700040116if  3c                   If        Vi2Sv3 = *Blanks
291800040116     c                   movel     IMPeditato    Vi2Va3
291900040116     c                   movel     wsvn          Vi2sv3
292000040116   x3c                   Else
292100040116if  4c                   If        Vi2Sv4 = *Blanks
292200040116     c                   movel     IMPeditato    Vi2Va4
292300040116     c                   movel     wsvn          Vi2sv4
292400041130   x4c                   Else
292500041130if  5c                   If        Vi2Sv5 = *Blanks
292600041130     c                   movel     IMPeditato    Vi2Va5
292700041130     c                   movel     wsvn          Vi2sv5
292800041130   x5c                   Else
292900041130if  6c                   If        Vi2Sv6 = *Blanks
293000041130     c                   movel     IMPeditato    Vi2Va6
293100041130     c                   movel     wsvn          Vi2sv6
293200041130    6c                   EndIf
293300041130    5c                   EndIf
293400040116    4c                   EndIf
293500040116    3c                   EndIf
293600040116    2c                   EndIf
293700040116
293800040116    1c                   EndSl
293900040116
294000040116     c                   EndSr
294100991022      **********************************************************************
294200991022      * AGGANCIO TABELLA VALUTE
294300991022      **********************************************************************
294400991022     C     CTR_VALUTA    BEGSR
294500991022
294600991022     C                   MOVEL     'CV'          KCOD
294700991022     C                   MOVEL(P)  WDIV          KKEY
294800991022     C     KTAB1         CHAIN     TABEL00F                           31
294900991022     C  N31              MOVEL     TBLUNI        DSCV
295000991022
295100991022     C                   ENDSR
295200991022      **********************************************************************
295300991022      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
295400991022      **********************************************************************
295500991022     C     EDTIMPORTI    BEGSR
295600991022
295700991022      * ------------------------------------------------------------------------
295800991022      * W0133      contiene il numero da editare per il video
295900991022      * WTSTDEC    corrisponde a §CVfdc della tabella CV
296000991022      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
296100991022      * ------------------------------------------------------------------------
296200991022      *
296300991022     c                   clear                   IMPeditato
296400991022     c                   clear                   W017A
296500991022      *
296600991022     c                   if        Wtstdec = 'S'
296700991022     C                   eval      W017a = %EDITC(W0133:'2')
296800991022     c                   else
296900991022     c                   z-add     W0133         W0130
297000991022     C                   eval      W017a = %EDITC(W0130:'2')
297100991022     c                   endif
297200991022      *
297300991022     c                   move      W017a         IMPeditato
297400991022      *
297500991022     c                   ENDSR
297600991025      *****************************************************************
297700991025      *  AGGANCIO ANAGRAFICHE
297800991025      *****************************************************************
297900991025     C     CTR_ANAGRA    BEGSR
298000991025      *
298100991025     C                   clear                   DS_cnaco
298200991025     C                   clear                   DS_cnind
298300991025     C                   clear                   DS_cnclp
298400991025     C                   clear                   DS_fncls
298500991025      *
298600991025     C                   CALL      'TIBS69R'
298700991025     C                   PARM                    tibs69DS
298800991025     C                   PARM                    DS_cnaco
298900991025     C                   PARM                    DS_cnind
299000991025     C                   PARM                    DS_cnclp
299100991025     C                   PARM                    DS_fncls
299200991025      *
299300991025     C                   eval      wtibs69r = 'S'
299400991025      * per il 1° giro è inizializzata nelle specifiche "D"
299500991025     C                   clear                   TIBS69DS
299600991025      *
299700991025     C                   ENDSR
299800940805     C**************************************************************************
299900940805     C*    CMD18 VISUALIZZA LEGAMI
300000940805     C**************************************************************************
300100920326     C     CMD18         BEGSR
300200940810     C*
300300940907     C* SAVNR9 > 2 --> GESTIONE SUBFILE
300400950328    1C     SAVNR9        IFGT      2
300500940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
300600940810     C                   MOVEL     'S'           WRIEM
300700940810     C     FORC2         TAG
300800940810     C                   Z-ADD     1             REC9
300900950328    2C     WRIEM         IFEQ      'S'
301000940810     C                   WRITE     LS05T01
301100940810     C                   WRITE     LS05Z09
301200940810     C                   MOVEL     *BLANKS       WRIEM
301300950328    2C                   ENDIF
301400940810     C                   EXFMT     LS05C09
301500940810     C*
301600940810     C* CMD12 - FINE LAVORO
301700940810     C   KL              GOTO      SOTTO
301800940810     C*
301900940810     C                   READC     LS05S09                                93
302000950328    2C     *IN93         DOWEQ     '0'
302100940810     C*
302200950328    3C     V9CSCE        IFEQ      '5'
302300940810     C                   MOVEL     ' '           V9CSCE
302400950103     C*
302500950103     C* RIPRISTINO GLI INDICATORI
302600950103     C                   MOVEL     V9CI80        *IN80
302700950103     C                   MOVEL     V9CI81        *IN81
302800950103     C                   MOVEL     V9CI82        *IN82
302900950103     C                   MOVEL     V9CI83        *IN83
303000940810     C                   UPDATE    LS05S09
303100940810     C*
303200950328    4C     V9CAAN        IFEQ      *ZERO
303300940907     C                   MOVEL     V9CAAP        PA4AAS
303400940907     C                   MOVEL     V9CLPP        PA4LNP
303500940907     C                   MOVEL     V9CNRP        PA4NRS
303600940907     C                   MOVEL     V9CNSP        PA4NSP
303700940907     C                   ELSE
303800940907     C                   MOVEL     V9CAAN        PA4AAS
303900940907     C                   MOVEL     V9CLPN        PA4LNP
304000940907     C                   MOVEL     V9CNRN        PA4NRS
304100940907     C                   MOVEL     V9CNSN        PA4NSP
304200950328    4C                   ENDIF
304300940810     C                   MOVEL     *BLANKS       PA4F03
304400960903     C     PA0FLG        IFEQ      'N'
304500960903     C* N.B.: Non testo anche pa0flg='R' perchè è impossibile che si
304600960903     C*       verifichi questo caso
304700960903     C                   MOVEL     'P'           PA4FLG
304800960903     C                   ELSE
304900940810     C                   MOVEL     '3'           PA4FLG
305000960903     C                   END
305100940810     C                   MOVEL     PARAM4        KPJBU
305200940810     C                   CALL      'FNLR36R'
305300940810     C                   PARM                    KPJBA
305400940810     C                   MOVEL     KPJBU         PARAM4
305500940810     C                   MOVEL     PA4F03        WF03
305600940810     C                   MOVEL     'S'           WRIEM
305700950328    3C                   ENDIF
305800940810     C*
305900940810     C                   READC     LS05S09                                93
306000950328    2C                   ENDDO
306100940908     C*
306200940908     C* RIEMETTO IL FORMATO VIDEO DEI LEGAMI
306300950328    2C     WRIEM         IFEQ      'S'
306400940908     C                   GOTO      FORC2
306500950328    2C                   ENDIF
306600940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
306700940810     C                   ELSE
306800940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
306900940810     C                   MOVEL     COMAAN        PA4AAS
307000940810     C                   MOVEL     COMLPN        PA4LNP
307100940810     C                   MOVEL     COMNRN        PA4NRS
307200940810     C                   MOVEL     COMNSN        PA4NSP
307300940810     C                   MOVEL     *BLANKS       PA4F03
307400960903     C     PA0FLG        IFEQ      'N'
307500960903     C* N.B.: Non testo anche pa0flg='R' perchè è impossibile che si
307600960903     C*       verifichi questo caso
307700960903     C                   MOVEL     'P'           PA4FLG
307800960903     C                   ELSE
307900940810     C                   MOVEL     '3'           PA4FLG
308000960903     C                   END
308100940810     C                   MOVEL     PARAM4        KPJBU
308200940810     C                   CALL      'FNLR36R'
308300940810     C                   PARM                    KPJBA
308400940810     C                   MOVEL     KPJBU         PARAM4
308500940810     C                   MOVEL     PA4F03        WF03
308600940810     C                   MOVEL     'S'           WRIEM
308700940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
308800940810     C                   ENDIF
308900940810     C*
309000940810     C     SOTTO         TAG
309100920326     C                   ENDSR
309200940802      **************************************************************************
309300940802      * VISUALIZZO IL FORMATO VIDEO DI RIEPILOGO DELLA BOLLA
309400940802      **************************************************************************
309500110601     C     RIEPIL        BEGSR
309600940802      *
309700940802     C                   SETOFF                                       25
309800940804     C                   MOVEL     'S'           WRIEM             1
309900021218      *
310000021218      * Riempio il campone in fondo alla videata con i tasti scelti
310100021218     C                   exsr      Ges_TastiFun
310200021218     c                   eval      FD07 = FDxx
310300021218     c                   eval      F24D07 = F24Dxx
310400940802     C     FOR07         TAG
310500940804     C     WRIEM         IFEQ      'S'
310600940804     C                   WRITE     LS05T01
310700940804     C                   MOVEL     *BLANKS       WRIEM
310800940804     C                   ENDIF
310900021218      *
311000021218      * Imposto la stringa comandi del 'giro' successivo
311100021218     C                   IF        *inKY = *on and $Loop > 1
311200021218     c                   Exsr      Ges_F24
311300021218     c                   eval      FD07 = FDxx
311400021218     c                   eval      F24D07 = F24Dxx
311500021218     C                   ENDIF
311600940804     C                   EXFMT     LS05D07
311700940808     C                   MOVEL     *BLANKS       WF03
311800940802      *
311900990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
312000940802     C   KY*IN25         IFEQ      *ON
312100940802     C                   SETOFF                                       25
312200940802     C                   ELSE
312300940802     C                   SETON                                        25
312400940802     C                   ENDIF
312500940802     C   KY              GOTO      FOR07
312600940802      *
312700990118      * F3 - FINE LAVORO TORNO A SUBFILE
312800941109     C   KC              MOVEL     '1'           PA0F03
312900940802     C   KC              GOTO      ENDRIE
313000940802      *
313100940802      * TASTI DI FUNZIONE VARI
313200940802     C                   EXSR      COMAND
313300940808     C*
313400940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
313500940808     C     WF03          IFNE      *BLANKS
313600941109     C                   MOVEL     '1'           PA0F03
313700940808     C                   GOTO      ENDRIE
313800940808     C                   ENDIF
313900091125     c*
314000091125     c* verifico se inserito '?' su beneficiario C/A
314100091125     c                   if        v2cben='?'
314200091125     c                   exsr      sr_bencas
314300091125     C                   SETON                                        31
314400091125     C                   MOVEL     'S'           WRIEM
314500091125     c                   endif
314600110601
314700950629     C*
314800940802     C   31              GOTO      FOR07
314900940802      *
315000940802     C     ENDRIE        ENDSR
315100940802      **************************************************************************
315200940802      * Nel caso di interrogazione da parte di un cliente, viene verificato se
315300940802      * egli puo' interrogare le bolle e se deve essere conteggiato il traffico
315400940802      * dei byte sulla linea di comunicazione.
315500940802      * Queste informazioni sono contenute nella tabella 3K.
315600940802      *
315700940802      * Inoltre, egli puo' interrogare solo alcuni clienti (quelli a lui
315800940802      * unificati.
315900940802      **************************************************************************
316000111020     C**!!!ABICLI        BEGSR
316100940802      *
316200111020     C**!!!              MOVE      *BLANKS       $3KINB
316300090603     C***                MOVE      *BLANKS       $3KCTB
316400940802      *
316500940802      * Leggo tutta la tabella 3K riempendo una schiera con i cod.clienti
316600111020     C**!!!              Z-ADD     *ZERO         $PRIMA            1 0
316700111020     C**!!!              Z-ADD     *ZERO         $A                3 0
316800111020     C**!!!              MOVE      '3K'          KCOD
316900111020     C**!!!KTAB2         SETLL     TABEL00F
317000111020     C**!!!              DO        *HIVAL
317100111020     C**!!!KTAB2         READE     TABEL00F                               90
317200111020     C*!N90TBLFLG        IFEQ      *BLANKS
317300111020     C**!!!              MOVEL     TBLUNI        DS3K
317400111020     C**!!!KNMUS         IFEQ      §3KPRF
317500940802      *
317600940802      * Devo prendere il cod.cliente, che e' la chiave di TABEL
317700111020     C**!!!              MOVEL     TBLKEY        $CODIC            7 0
317800111020     C**!!!$CODIC        LOOKUP    CLI                                    99
317900111020     C*!N99$A            IFLT      300
318000111020     C**!!!              ADD       1             $A
318100111020     C**!!!              Z-ADD     $CODIC        CLI($A)
318200111020     C**!!!              ENDIF
318300940802      *
318400940802      * Gia' dalla prima lettura ho i due flag che abilitano il profilo
318500111020     C**!!!$PRIMA        IFEQ      *ZERO
318600111020     C**!!!              MOVE      §3KINB        $3KINB
318700090603     C***                MOVE      §3KCTB        $3KCTB
318800111020     C**!!!§3KINB        COMP      'S'                                9090
318900111020     C**!!!              Z-ADD     1             $PRIMA
319000111020     C**!!!              ENDIF
319100940802      *
319200111020     C**!!!              ENDIF
319300111020     C**!!!              ENDIF
319400111020     C*!N90              ENDDO
319500940802      *
319600111020     C**!!!              ENDSR
319700940905     C**************************************************************************
319800940905     C* RIEMPIE IL SFL CON TUTTI I LEGAMI BOLLA
319900940905     C* PARTENDO DALLA BOLLA ORIGINALE, FINO ALL'ULTIMA IN
320000940906     C* ORDINE CRONOLOGICO (ULTIMO MODELLO SWATCH)
320100940905     C**************************************************************************
320200940906     C     LEGBOL        BEGSR
320300940906     C*
320400940906     C* PULISCO IL SFL
320500940906     C                   SETOFF                                       202123
320600940906     C                   WRITE     LS05C09
320700940906     C                   SETON                                        2123
320800940906     C*
320900940906     C                   SETOFF                                       07
321000940906     C                   Z-ADD     *ZERO         SAVNR9            4 0
321100940906     C                   MOVEL     *BLANKS       WFLG1             1
321200940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
321300940905     C*
321400940906     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
321500940906     C     KBLP3         CHAIN     FNLBL01L                           99
321600940906     C   99KBLP3         CHAIN     FNLBL02L                           99
321700940906     C  N99              DO
321800940905     C*
321900940906     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
322000940906     C                   MOVEL     LBLAAO        KAAS
322100940906     C                   MOVEL     LBLLPO        KLNP
322200940906     C                   MOVEL     LBLNRO        KNRS
322300940906     C                   MOVEL     LBLNSO        KNSP
322400940906     C     KLBL3         CHAIN     FNLBL02L                           99
322500950103     C     *IN99         IFEQ      *ON
322600950103     C                   MOVEL     'N'           WORIGI            1
322700950103     C                   ELSE
322800950103     C                   MOVEL     'S'           WORIGI
322900950103     C                   ENDIF
323000940907     C*
323100940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
323200940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
323300950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
323400950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
323500940907     C                   MOVEL     ' '           V9CSCE
323600940907     C                   MOVEL     LBLAAO        V9CAAP
323700940907     C                   MOVEL     LBLLPO        V9CLPP
323800940907     C                   MOVEL     LBLLAO        V9CLAP
323900940907     C                   MOVEL     LBLNRO        V9CNRP
324000940907     C                   MOVEL     LBLNSO        V9CNSP
324100940926     C                   CLEAR                   V9CAAN
324200940926     C                   CLEAR                   V9CLPN
324300940926     C                   CLEAR                   V9CLAN
324400940926     C                   CLEAR                   V9CNRN
324500940926     C                   CLEAR                   V9CNSN
324600940907     C*
324700940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
324800940907     C     LBLAAO        IFEQ      V1CAAS
324900940907     C     LBLLPO        ANDEQ     V1CLNP
325000940907     C     LBLNRO        ANDEQ     V1CNRS
325100940907     C     LBLNSO        ANDEQ     V1CNSP
325200940907     C                   MOVEL     *BLANKS       V9CPRE
325300940907     C                   SETON                                        80
325400940907     C                   SETON                                        81
325500940907     C                   MOVEL     '1'           WFLG1
325600940907     C                   ELSE
325700940907     C                   SETOFF                                       81
325800940907     C*
325900950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
326000950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
326100950320     C                   SETOFF                                       93
326200950320     C     KLBL3         SETLL     FNARB01L                               93
326300950320     C  N93              MOVEA     PRD(1)        V9CPRE
326400950320     C  N93              SETON                                        80
326500950320     C*
326600950320     C* SE RICHIAMATO DAGLI ARRIVI (FNLR36R) NON POSSO
326700950320     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
326800950320     C   93PA0FLG        IFEQ      'A'
326900960903     C     PA0FLG        OREQ      'R'
327000950320     C                   MOVEA     PRD(4)        V9CPRE
327100950320     C                   SETON                                        80
327200950320     C                   ELSE
327300950328     C                   MOVEA     PRD(2)        V9CPRE
327400950320     C                   SETOFF                                       80
327500950320     C                   ENDIF
327600950320     C*
327700950320     C* SE NON TROVATA SU ARB PROVO SU BLP
327800950320     C  N93              DO
327900950320     C                   SETOFF                                       93
328000950320     C     KLBL3         SETLL     FNBLP01L                               93
328100950320     C*
328200950328     C   93              MOVEA     PRD(3)        V9CPRE
328300950428     C   93              SETON                                        80
328400950320     C*
328500950320     C                   ENDDO
328600940907     C*
328700950320     C                   ENDIF
328800940907     C                   SETON                                        83
328900950103     C*
329000950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
329100950103     C                   MOVEL     *IN80         V9CI80
329200950103     C                   MOVEL     *IN81         V9CI81
329300950103     C                   MOVEL     *IN82         V9CI82
329400950103     C                   MOVEL     *IN83         V9CI83
329500950103     C*
329600940907     C                   Z-ADD     1             NRR9
329700940907     C                   WRITE     LS05S09
329800940907     C                   SETON                                        20
329900950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
330000950103     C*
330100950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
330200950103     C*                      2) NON HO TROVATO L'ORIGINALE
330300950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
330400950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
330500950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
330600950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
330700950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
330800950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
330900950103     C* L'ORIGINALE E LA MAMMA.
331000950103     C     WORIGI        IFEQ      'N'
331100940907     C*
331200950103     C                   MOVEL     ' '           V9CSCE
331300950103     C                   CLEAR                   V9CAAP
331400950103     C                   CLEAR                   V9CLPP
331500950103     C                   CLEAR                   V9CLAP
331600950103     C                   CLEAR                   V9CNRP
331700950103     C                   CLEAR                   V9CNSP
331800950103     C                   MOVEL     LBLAAP        V9CAAN
331900950103     C                   MOVEL     LBLLPP        V9CLPN
332000950103     C                   MOVEL     LBLLAP        V9CLAN
332100950103     C                   MOVEL     LBLNRP        V9CNRN
332200950103     C                   MOVEL     LBLNSP        V9CNSN
332300950103     C                   SETOFF                                       8182
332400950103     C*
332500950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
332600950103     C     V9CAAN        IFEQ      V1CAAS
332700950103     C     V9CLPN        ANDEQ     V1CLNP
332800950103     C     V9CNRN        ANDEQ     V1CNRS
332900950103     C     V9CNSN        ANDEQ     V1CNSP
333000950103     C                   MOVEL     *BLANKS       V9CPRE
333100950103     C                   SETON                                        80
333200950103     C                   SETON                                        82
333300950103     C                   ELSE
333400950103     C*
333500950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
333600950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
333700950320     C                   EXSR      CHKBOL
333800950103     C*
333900950103     C                   ENDIF
334000950103     C*
334100950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
334200950103     C                   SETOFF                                       83
334300950103     C*
334400950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
334500950103     C                   MOVEL     *IN80         V9CI80
334600950103     C                   MOVEL     *IN81         V9CI81
334700950103     C                   MOVEL     *IN82         V9CI82
334800950103     C                   MOVEL     *IN83         V9CI83
334900950103     C*
335000950103     C                   ADD       1             NRR9
335100950103     C                   WRITE     LS05S09
335200950103     C*
335300950103     C                   ENDIF
335400940907     C                   SETOFF                                       83  99
335500940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
335600940906     C* CICLO FINO ALL'ULTIMO LEGAME
335700940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
335800940907     C                   Z-ADD     *ZERO         WA                3 0
335900940907     C                   Z-ADD     *ZERO         WB                3 0
336000940907     C                   MOVEA     *ZERO         AAN
336100940906     C     *IN99         DOWEQ     *OFF
336200940905     C*
336300940906     C                   MOVEL     ' '           V9CSCE
336400940907     C                   MOVEL     LBLAAP        V9CAAP
336500940907     C                   MOVEL     LBLLPP        V9CLPP
336600940907     C                   MOVEL     LBLLAP        V9CLAP
336700940907     C                   MOVEL     LBLNRP        V9CNRP
336800940907     C                   MOVEL     LBLNSP        V9CNSP
336900940907     C                   MOVEL     LBLAAN        V9CAAN
337000940907     C                   MOVEL     LBLLPN        V9CLPN
337100940907     C                   MOVEL     LBLLAN        V9CLAN
337200940907     C                   MOVEL     LBLNRN        V9CNRN
337300940907     C                   MOVEL     LBLNSN        V9CNSN
337400940907     C                   SETOFF                                       8182
337500940906     C*
337600950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
337700940907     C     LBLAAP        IFEQ      V1CAAS
337800940907     C     LBLLPP        ANDEQ     V1CLNP
337900940907     C     LBLNRP        ANDEQ     V1CNRS
338000940907     C     LBLNSP        ANDEQ     V1CNSP
338100940907     C                   SETON                                        81
338200940907     C                   ENDIF
338300940907     C*
338400940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
338500940907     C     LBLAAN        IFEQ      V1CAAS
338600940907     C     LBLLPN        ANDEQ     V1CLNP
338700940907     C     LBLNRN        ANDEQ     V1CNRS
338800940907     C     LBLNSN        ANDEQ     V1CNSP
338900940906     C                   MOVEL     *BLANKS       V9CPRE
339000940906     C                   SETON                                        80
339100940907     C                   SETON                                        82
339200940906     C                   ELSE
339300940906     C*
339400950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
339500950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
339600950320     C                   EXSR      CHKBOL
339700940906     C*
339800940906     C                   ENDIF
339900950103     C*
340000950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
340100950103     C                   MOVEL     *IN80         V9CI80
340200950103     C                   MOVEL     *IN81         V9CI81
340300950103     C                   MOVEL     *IN82         V9CI82
340400950103     C                   MOVEL     *IN83         V9CI83
340500940906     C*
340600940906     C* SCRIVO IL RECORD NEL SFL
340700940906     C                   ADD       1             NRR9
340800940906     C                   WRITE     LS05S09
340900940907     C*
341000940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
341100940907     C                   ADD       1             WA
341200940907     C                   MOVEL     LBLAAN        AAN(WA)
341300940907     C                   MOVEL     LBLLPN        LPN(WA)
341400940907     C                   MOVEL     LBLNRN        NRN(WA)
341500940907     C                   MOVEL     LBLNSN        NSN(WA)
341600940907     C*
341700940907     C     KLBL3         READE     FNLBL02L                               99
341800940906     C*
341900940907     C* SE NON HO PIU' BOLLE CON READE
342000940906     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
342100970116     C     WB            DOWLT     WA
342200970116     C     *IN99         ANDEQ     *ON
342300940907     C                   ADD       1             WB
342400940907     C                   MOVEL     AAN(WB)       KAAS
342500940907     C                   MOVEL     LPN(WB)       KLNP
342600940907     C                   MOVEL     NRN(WB)       KNRS
342700940907     C                   MOVEL     NSN(WB)       KNSP
342800940907     C     KLBL3         SETLL     FNLBL02L
342900940907     C     KLBL3         READE     FNLBL02L                               99
343000970116     C                   ENDDO
343100940906     C*
343200940906     C                   ENDDO
343300940906     C                   ENDDO
343400940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
343500940906     C*
343600940906     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
343700940906     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
343800940926     C     NRR9          IFEQ      2
343900940907     C*
344000940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
344100940907     C     2             CHAIN     LS05S09                            99
344200940907     C     V9CAAP        IFEQ      V1CAAS
344300940907     C     V9CLPP        ANDEQ     V1CLNP
344400940907     C     V9CNRP        ANDEQ     V1CNRS
344500940907     C     V9CNSP        ANDEQ     V1CNSP
344600940907     C                   MOVEL     V9CAAN        COMAAN
344700940907     C                   MOVEL     V9CLPN        COMLPN
344800940907     C                   MOVEL     V9CNRN        COMNRN
344900940907     C                   MOVEL     V9CNSN        COMNSN
345000950306     C                   MOVEL     V9CLAN        COMLAN
345100950316     C                   MOVEA     'SEGUEN.'     LBL(14)
345200940907     C                   ELSE
345300940907     C                   MOVEL     V9CAAP        COMAAN
345400940907     C                   MOVEL     V9CLPP        COMLPN
345500940907     C                   MOVEL     V9CNRP        COMNRN
345600940907     C                   MOVEL     V9CNSP        COMNSN
345700950306     C                   MOVEL     V9CLAP        COMLAN
345800950316     C                   MOVEA     'PRECED.'     LBL(14)
345900940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
346000940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
346100940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
346200940926     C     1             CHAIN     LS05S09                            99
346300940926     C* REIMPOSTO NRR9=2 PERCHE' MI SERVE DOPO
346400940926     C                   Z-ADD     2             NRR9
346500940907     C                   ENDIF
346600940907     C*
346700950316     C                   MOVEA     COMLPN        LBL(21)
346800950316     C                   MOVEA     COMNRN        LBL(25)
346900950316     C                   MOVEA     COMNSN        LBL(28)
347000950316     C                   MOVEA     COMLAN        LBL(40)
347100940906     C*
347200940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
347300940907     C                   Z-ADD     1             W                 1 0
347400940907     C     V9CPRE        LOOKUP    PRD(W)                                 99
347500950306     C                   MOVEA     LBD(W)        LBL(44)
347600940907     C     W             COMP      2                                      07
347700940906     C*
347800940906     C                   MOVEA     LBL           LEGAME
347900940906     C                   ELSE
348000940905     C*
348100940926     C     NRR9          IFGT      2
348200940906     C                   MOVEA     LB2           LEGAME
348300940906     C                   SETON                                        07
348400940906     C                   ENDIF
348500940906     C                   ENDIF
348600940906     C*
348700940907     C     NRR9          COMP      1                                    3434
348800940905     C*
348900940906     C                   Z-ADD     NRR9          SAVNR9
349000940906     C                   Z-ADD     1             NRR9
349100940912     C*
349200940912     C* RIAGGANCIO LA BOLLA IN ESAME
349300940912     C     KBLP3         CHAIN     FNBLP01L                           99
349400940905     C*
349500940905     C                   ENDSR
349600950320     C**************************************************************************
349700950320     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
349800950320     C**************************************************************************
349900950320     C     CHKBOL        BEGSR
350000950320     C*
350100950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
350200950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
350300950320     C                   SETOFF                                       93
350400950320     C     KLBL1         SETLL     FNARB01L                               93
350500950320     C  N93              MOVEA     PRD(1)        V9CPRE
350600950320     C  N93              SETON                                        80
350700950320     C*
350800950320     C* SE RICHIAMATO DAGLI ARRIVI (FNLR36R) NON POSSO
350900950320     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
351000950320     C   93PA0FLG        IFEQ      'A'
351100960903     C     PA0FLG        OREQ      'R'
351200950320     C                   MOVEA     PRD(4)        V9CPRE
351300950320     C                   SETON                                        80
351400950320     C                   ELSE
351500950320     C                   MOVEA     PRD(2)        V9CPRE
351600950320     C                   SETOFF                                       80
351700950320     C                   ENDIF
351800950320     C*
351900950320     C* SE NON TROVATA SU ARB PROVO SU BLP
352000950320     C  N93              DO
352100950320     C                   SETOFF                                       93
352200950320     C     KLBL1         SETLL     FNBLP01L                               93
352300950328     C   93              MOVEA     PRD(3)        V9CPRE
352400950428     C   93              SETON                                        80
352500950320     C*
352600950320     C                   ENDDO
352700950320     C*
352800950320     C                   ENDSR
352900940923      **************************************************************************
353000940923      *   CONTROLLI SUL FORMATO VIDEO D02
353100940923      **************************************************************************
353200940923     C     CTRD02        BEGSR
353300940923     C*
353400940923     C                   SETOFF                                       31
353500940923     C*
353600110601     c* verifico se inserito '?' su email destinatario
353700110601     c                   if        v2cemw='?'
353800110601     c                   exsr      sr_emdest
353900110601     C                   SETON                                        31
354000110601     C                   MOVEL     'S'           WRIEM
354100110601     c                   if        v2cben='?'
354200110601     c                   write     ls05t01
354300110601     c                   write     ls05d02
354400110601     c                   endif
354500110601     c                   endif
354600091125     c* verifico se inserito '?' su beneficiario C/A
354700091125     c                   if        v2cben='?'
354800091125     c                   exsr      sr_bencas
354900091125     C                   SETON                                        31
355000091125     C                   MOVEL     'S'           WRIEM
355100091125     c                   endif
355200940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
355300940923     C     V2IGCA        IFEQ      '?'
355400991022     C                   CLEAR                   DS7PQRS
355500941003     C                   MOVEL     'S'           DS7RIC
355600941003     C                   MOVEL     'V'           DS7GES
355700941003     C                   MOVEL     V2CGCA        DS7COD
355800991022     C                   MOVEL     DS7PQRS       KPJBU
355900960126     C                   CALL      'TRTB72R'                            31
356000941003     C                   PARM                    KPJBA
356100940923     C                   MOVEL     *BLANKS       V2IGCA
356200940923     C                   SETON                                        31
356300940923     C                   MOVEL     'S'           WRIEM
356400940923     C                   ENDIF
356500940923     C*
356600940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
356700940923     C     V2IGGA        IFEQ      '?'
356800991022     C                   CLEAR                   DS7PQRS
356900941003     C                   MOVEL     'S'           DS7RIC
357000941003     C                   MOVEL     'V'           DS7GES
357100941003     C                   MOVEL     V2CGGA        DS7COD
357200991022     C                   MOVEL     DS7PQRS       KPJBU
357300960126     C                   CALL      'TRTB69R'                            31
357400941003     C                   PARM                    KPJBA
357500940923     C                   SETON                                        31
357600940923     C                   MOVEL     'S'           WRIEM
357700940923     C                   MOVEL     *BLANKS       V2IGGA
357800940923     C                   ENDIF
357900940923     C*
358000940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
358100940923     C     V2IGMA        IFEQ      '?'
358200991022     C                   CLEAR                   DS7PQRS
358300941003     C                   MOVEL     'S'           DS7RIC
358400941003     C                   MOVEL     'V'           DS7GES
358500941003     C                   MOVEL     V2CGMA        DS7COD
358600991022     C                   MOVEL     DS7PQRS       KPJBU
358700960126     C                   CALL      'TRTB70R'                            31
358800941003     C                   PARM                    KPJBA
358900940923     C                   SETON                                        31
359000940923     C                   MOVEL     'S'           WRIEM
359100940923     C                   MOVEL     *BLANKS       V2IGMA
359200940923     C                   ENDIF
359300940923     C*
359400940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
359500940923     C     V2IGVA        IFEQ      '?'
359600991022     C                   CLEAR                   DS7PQRS
359700941003     C                   MOVEL     'S'           DS7RIC
359800941003     C                   MOVEL     'V'           DS7GES
359900941003     C                   MOVEL     V2CGVA        DS7COD
360000991022     C                   MOVEL     DS7PQRS       KPJBU
360100960126     C                   CALL      'TRTB73R'                            31
360200941003     C                   PARM                    KPJBA
360300940923     C                   SETON                                        31
360400940923     C                   MOVEL     'S'           WRIEM
360500941003     C                   MOVEL     *BLANKS       V2IGVA
360600940923     C                   ENDIF
360700940923     C*
360800940923     C                   ENDSR
360900091125      **************************************************************************
361000091125      *   CONTROLLI SUL FORMATO VIDEO D02
361100091125      **************************************************************************
361200091125     C     sr_bencas     BEGSR
361300091125     c                   clear                   v2cben
361400091214     c                   clear                   wdaben
361500091214     c                   clear                   wdasrsc
361600091214     c                   clear                   wdascdi
361700091216     c                   clear                   trulintds
361800091216     c
361900091216     c                   eval      iinttip='B'
362000091216     c                   eval      iinttic=v2ctic
362100091216     c                   eval      iintrscm=v2crsm
362200091216     c                   eval      iintrsco=v2crmo
362300091216     c                   eval      iintCDI =w_cdi
362400091216     c                   call      'TRULINTR'
362500091216     c                   parm                    trulintds
362600091216     c
362700091216     c                   movel     ointrsci      wdasrsc
362800091216     c
362900091216     c                   select
363000091216     c                   when      ointtipi='O'
363100091216     c                   eval      wdaben='MITTENTE  ORIGINALE'
363200091216     c
363300091216     c                   when      ointtipi='R'
363400091216     c                   eval      wdascdi=w_cdi
363500091216     c                   eval      wdaben=' O R D I N A N T E'
363600091216     c
363700091216     c                   when      ointtipi='M'
363800091216     c                   if        v2ccdm>0
363900091216     c                   movel     v2ccdm        wdascdi
364000091216     c                   endif
364100091216     c                   eval      wdaben='  M I T T E N T E'
364200091216     c                   ENDSL
364300091214     c
364400091125     c                   exfmt     ls05wdas
364500091125     c                   endsr
364600110601      **************************************************************************
364700110601      *   emissione window email destinatario
364800110601      **************************************************************************
364900110601     C     sr_emdest     BEGSR
365000110601     c                   clear                   v2cemw
365100110601     c
365200110601     c                   eval      ilv79TLA='L'
365300110601     c                   eval      ilv79Aas=blpaas
365400110601     c                   eval      ilv79Lnp=blplnp
365500110601     c                   eval      ilv79nrs=blpnrs
365600110601     c                   eval      ilv79nsp=blpnsp
365700110601     c                   call      'FNLV79R'
365800110601     c                   parm                    fnlv79ds
365900110601     c
366000110601     c                   endsr
366100130912      **************************************************************************
366200130912      *   window per visualizzare gli orari servizi
366300130912      **************************************************************************
366400130912     C     sr_OrariSr    BEGSR
366500130920
366600140131     c                   clear                   trulorsds
366700140131     c                   eval       IORefil = BLPlna
366800140131     c                   eval       IORecap = BLPcad
366900140131     c                   eval       IOReloc = BLPlod
367000140131     c                   eval       IORenar = BLPnzd
367100140131     c
367200140131     c                   eval       IORetser = 'C'
367300140131     c                   eval       IORetsp = BLPtsp
367400140131     c                   eval       IOREtfp = blptfp
367500140131     c                   eval       IOREtfa = blptfa
367600140131     c                   eval       IOREdti = blpdti
367700140131     c                   eval       IOREhti = blphti
367800140203     c                   if         wesarb='S'
367900140203     c                   eval       IOREdti = arbdti
368000140203     c                   eval       IOREhti = arbhti
368100140203     c                   endif
368200140131     c                   eval       IOREdcr = blpdcr
368300140131     c                   eval       IOREhcr = blphcr
368400140131     c                   eval       IOREtcr = blptcr
368500140131     C*
368600140131     C                   MOVEL     ' '           D98TLA
368700140131     C                   MOVEL     'P'           D98TBO
368800140131     C                   MOVEL     BLPAAS        D98AAS
368900140131     C                   MOVEL     BLPLNP        D98LNP
369000140131     C                   MOVEL     BLPNRS        D98NRS
369100140131     C                   MOVEL     BLPNSP        D98NSP
369200140131     C*
369300140131     C                   CALL      'TNSD99R'
369400140131     C                   PARM                    TNSD99DS
369500140203     c
369600140131     c                   eval       IOREdei = d98dei
369700140131     c                   eval       IOREoei = d98oei
369800150728     c                   clear                   diore01
369900150728     c                   movel     d98ttc        §ioretrazC
370000150728     c                   movel     d98tcc        §ioreconsC
370100150728     c                   eval       IOREflo = diore01
370200150728     c
370300140131     c*****              eval       IORedta = BLPaas * 10000 + BLPmgs
370400140203     c
370500140131     c* Data di Riferimento: data consegna se consegnata
370600140131     c                   select
370700140131     c                   when      blpdcm>0
370800140131     c                   eval      ioredta=blpdcm
370900140131     c                   other
371000140131     c* altrimenti data prevista consegna se > = oggi
371100140131     C                   time                    wrkoggiiso
371200140131     C     *iso          move      wrkoggiiso    ioredta
371300140131     c* se < di oggi, imposto oggi
371400140131     c                   if        d98dce>ioredta
371500140131     c                   eval       ioredta=d98dce
371600140131     c                   endif
371700140131     c                   endsl
371800140409
371900140409     c                   eval       IOREDDC=blpddc
372000140409     c                   eval       IORENDC=blpndc
372100140203     c* Se in distinta leggo i dati da FNARB
372200140409     c                   if        wesarb='S'
372300140409     c                   eval       IOREDDC=arbddc
372400140409     c                   eval       IORENDC=arbndc
372500140409     c                   if         arbdcm=0 and ARBfbc = *blanks and
372600140203     c                               ARBddc > 0 and
372700140203     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
372800140203     c                               arbica='R' or arbicc=' ' or arbicc='R')
372900140203     c                   eval       IOREcons = 'S'
373000140224     c* Se in distinta, passo l'orario presunto di consegna
373100140224     c                   move      arbndc        pctndc
373200140224     c                   move      arbifp        pctfgs
373300140224     c                   move      'COR'         ttrd
373400140224     c     kcor          setgt     fipct02l
373500140224     c     kcor          readpe    fipct02l
373600140310     c                   if        not %eof(fipct02l)
373700140224     c                   movel     pctdati       fipctcords
373800140307     c                   eval      ioreorp=§PCTORASTI
373900140224     c                   endif
374000140224     c
374100140203     c                   ENDIF
374200140203     c                   ENDIF
374300140131     C*
374400140325     c                   eval      iorecol=36
374500130920
374600140131     c                   call      'TRULORSR'
374700130912     c                   parm                    kpjba
374800140131     c                   parm                    trulorsds
374900130912     c
375000130912     c                   endsr
375100940810      **************************************************************************
375200940810      *   R O U T I N E     I N I Z I A L E
375300940810      **************************************************************************
375400940810     C     *INZSR        BEGSR
375500940810     C*
375600940810     C* ACCESSO   FILE BOLLE VISTA LOGICA 1
375700940810     C     KBLP3         KLIST
375800940810     C                   KFLD                    PA0AAS
375900940810     C                   KFLD                    PA0LNP
376000940810     C                   KFLD                    PA0NRS
376100940810     C                   KFLD                    PA0NSP
376200021218     C*
376300021218     C     KAR51         KLIST
376400021218     C                   KFLD                    KAAS
376500021218     C                   KFLD                    KLNP
376600021218     C                   KFLD                    KNRS
376700021218     C                   KFLD                    KNSP
376800021218     C                   KFLD                    KTRd              3
376900021218     C*
377000991022     C     KAR6          KLIST
377100991022     C                   KFLD                    PA0AAS
377200940808     C                   KFLD                    PA0LNP
377300940808     C                   KFLD                    PA0NRS
377400940808     C                   KFLD                    PA0NSP
377500991022     c                   KFLD                    KTRC
377600940810     C* ACCESSO   FILE LEGAME BOLLA
377700940810     C     KLBL1         KLIST
377800940810     C                   KFLD                    LBLAAN
377900940810     C                   KFLD                    LBLLPN
378000940810     C                   KFLD                    LBLNRN
378100940810     C                   KFLD                    LBLNSN
378200940906     C     KLBL3         KLIST
378300940906     C                   KFLD                    KAAS
378400940906     C                   KFLD                    KLNP
378500940906     C                   KFLD                    KNRS
378600940906     C                   KFLD                    KNSP
378700060216     C     Kar41         KLIST
378800940805     C                   KFLD                    KAAS
378900940805     C                   KFLD                    KLNP
379000940805     C                   KFLD                    KNRS
379100940805     C                   KFLD                    KNSP
379200940805     C                   KFLD                    KTRC
379300060216     C     Kar42         KLIST
379400951219     C                   KFLD                    KAAS
379500951219     C                   KFLD                    KLNP
379600951219     C                   KFLD                    KNRS
379700951219     C                   KFLD                    KNSP
379800940809     C     KTAB1         KLIST
379900940804     C                   KFLD                    CODUT
380000940804     C                   KFLD                    KCOD
380100940804     C                   KFLD                    KKEY
380200940809     C     KTAB2         KLIST
380300940804     C                   KFLD                    CODUT
380400940804     C                   KFLD                    KCOD
380500940808     C     KIND1         KLIST
380600940808     C                   KFLD                    CODUT
380700940808     C                   KFLD                    KKCC
380800940808     C                   KFLD                    KKSC
380900021203     C     Kfapd         klist
381000021203     C                   kfld                    kpdtip
381100021203     C                   kfld                    kpdpdr
381200040220      * File FITGD02L
381300040220     c     K02TGD02      klist
381400040220     c                   kfld                    TGDtor
381500040220     c                   kfld                    TGDogg
381600040713      * Fnorm
381700040713     c     korm          klist
381800040713     c                   kfld                    §b4poe
381900040713     c                   kfld                    §b4nsr
382000040713     c                   kfld                    §b4nor
382100040713     c                   kfld                    §b4nrv
382200140224     C     Kcor          KLIST
382300140224     C                   KFLD                    pctfgs
382400140224     C                   KFLD                    pctNDC
382500140224     C                   KFLD                    arbpdc
382600140224     C                   KFLD                    TTRD              3
382700140224     C                   KFLD                    arbAAS
382800140224     C                   KFLD                    arbLNP
382900140224     C                   KFLD                    arbNRS
383000140224     C                   KFLD                    arbNSP
383100940804     C*
383200940810     C                   MOVEL     *ZERO         COMLPN            3
383300940810     C                   MOVEL     *ZERO         COMNRN            2
383400940810     C                   MOVEL     *ZERO         COMNSN            7
383500950306     C                   MOVEL     *ZERO         COMLAN            3
383600940804     C*
383700940810     C                   Z-ADD     *ZERO         NRR8              4 0
383800940810     C                   Z-ADD     *ZERO         NRR9              4 0
383900940906     C                   Z-ADD     *ZERO         SAVNR8            4 0
384000940906     C                   Z-ADD     *ZERO         SAVNR9            4 0
384100940808     C                   MOVEL     *BLANKS       WF03              1
384200940808     C*
384300940804     C                   ENDSR
384400940804      **************************************************************************
384500941011**   SEQ
384600910912NON SEQUENZIALI
384700950316**        12345678901234567890123456789012345678901234567890
384800950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
384900920326**
385000950316LEGATA A PIU' BOLLE                       - F18=Visualizza
385100920326**
385200950316 Non Presente
385300950316 F18=Visualizza
385400940906Vedi in Int.Par
385500940907Vedi in Int.Arr
385600941011Vedi arch.stor.
385700920326**
385800920326Spedizione non Presente
385900941230
386000940907Vedi in Int.Bolle Part.
386100940907Vedi in Int.Bolle Arr.
386200941011Vedi in Archiv. Storico
386300040220** TF                                                                        *
386400040610F1=Agg.Rich.#
386500021218** TF                                                                        *
386600021218F2=C.A.#
386700021218** TF                                                                        *
386800021218F4=Tass2#
386900021218** TF                                                                        *
387000021218F5=Eventi#
387100021218** TF                                                                        *
387200021218F7=Anomalie#
387300021218** TF                                                                        *
387400021218F8=Orm#
387500130912** TF                                                                        *
387600130912F9=OrariSr#
387700130912** TF                                                                        *
387800021218F10=Variaz.#
387900021218** TF                                                                        *
388000030121F11=NoteS/B#
388100021218** TF                                                                        *
388200021218F12=Ritorno#
388300021218** TF                                                                        *
388400021218F13=Tassaz.#
388500021218** TF                                                                        *
388600021218F14=Giacenza#
388700040220** TF                                                                        *
388800040610F16=Gest.Rich.#
388900021218** TF                                                                        *
389000021218F19=Spunte#
389100021218** TF                                                                        *
389200021218F20=Affid.#
389300021218** TF                                                                        *
389400021218F21=Dett.Colli#
389500021218** TF                                                                        *
389600051201F22=C/Assegni#
