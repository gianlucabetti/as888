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
003600940804      *-------------------------------------------------------------------------
003700040220      *
003800040220      *   S C H I E R E
003900040220      *
004000940906     D SEQ             S             15    DIM(1) CTDATA PERRCD(1)
004100950306     D LBL             S              1    DIM(58) CTDATA PERRCD(58)
004200950306     D LB2             S             58    DIM(1) CTDATA PERRCD(1)
004300941011     D LBD             S             15    DIM(5) CTDATA PERRCD(1)
004400941011     D PRD             S             23    DIM(5) CTDATA PERRCD(1)
004500040220     d TF01            s              1    dim(20) ctdata perrcd(20)
004600021218     D TF02            S              1    DIM(20) CTDATA PERRCD(20)
004700021218     D TF04            S              1    DIM(20) CTDATA PERRCD(20)
004800021218     D TF05            S              1    DIM(20) CTDATA PERRCD(20)
004900021218     D TF07            S              1    DIM(20) CTDATA PERRCD(20)
005000021218     D TF08            S              1    DIM(20) CTDATA PERRCD(20)
005100130912     D TF09            S              1    DIM(20) CTDATA PERRCD(20)
005200021218     D TF10            S              1    DIM(20) CTDATA PERRCD(20)
005300021218     D TF11            S              1    DIM(20) CTDATA PERRCD(20)
005400021218     D TF12            S              1    DIM(20) CTDATA PERRCD(20)
005500021218     D TF13            S              1    DIM(20) CTDATA PERRCD(20)
005600021218     D TF14            S              1    DIM(20) CTDATA PERRCD(20)
005700040220     d TF16            s              1    dim(20) ctdata perrcd(20)
005800021218     D TF19            S              1    DIM(20) CTDATA PERRCD(20)
005900021218     D TF20            S              1    DIM(20) CTDATA PERRCD(20)
006000021218     D TF21            S              1    DIM(20) CTDATA PERRCD(20)
006100021218     D TF22            S              1    DIM(20) CTDATA PERRCD(20)
006200021218     D $TF             S              1    DIM(20)
006300021218     D*
006400111020     D**!!!CLI             S              7  0 DIM(300)
006500940907     D*
006600940907     D AAN             S              4  0 DIM(30)
006700940907     D LPN             S              3  0 DIM(30)
006800940907     D NRN             S              2  0 DIM(30)
006900940907     D NSN             S              7  0 DIM(30)
007000941021     D CC1             S              1    DIM(99)
007100941021     D CC2             S             20    DIM(99)
007200020329     D CC3             S              1    DIM(99)
007300020329     D CB1             S              1    DIM(99)
007400020329     D CB2             S             20    DIM(99)
007500030121     D RigaTf1         S              1    DIM(62)
007600030121     D RigaTf2         S              1    DIM(62)
007700030121     D RigaTf3         S              1    DIM(62)
007800040220      *
007900040220      *   S T R U T T U R E   D A T I
008000040220      *
008100010220     D*VISUALIZZAZIONE ORM - FIOR07R
008200010220     D Parm01          DS
008300010220     D  ppoe                          3  0
008400010220     D  pnor                          7  0
008500010220     D  pnsr                          2  0
008600010220     D  pnrv                          2  0
008700010220     D  psce                          1
008800010220     D  pfgs                          3  0
008900010220     D  ppor                          3  0
009000010220     D  pdtr                          8  0
009100010220     D  pmdb                         10
009200010220     D  pprb                         10
009300010220     D  pdts                          8  0
009400010220     D  prmp                          1
009500010220     D  pbrc                          1
009600010220     D  pref                          2
009700010220     D  pmio                          1
009800940804     D* DATI DA PGM RICHIAMANTE               - FNLS04R - FNLR36R -
009900940804     D PARAM0          DS
010000940808     D  PA0AAS                 1      4  0
010100940808     D  PA0LNP                 5      7  0
010200940808     D  PA0NRS                 8      9  0
010300940808     D  PA0NSP                10     16  0
010400941109     D  PA0F03                17     17
010500940907     D  PA0FLG                18     18
010600940808     D  PA0A9                 19     19
010700940808     D  PA0B5                 20     20
010800940808     D  PA075                 21     21
010900940808     D  PA098                 22     22
011000940808     D  PA0RSU                23     42
011100940808     D  PA0CML                43     43
011200940808     D  PA0SIM                44     46  0
011300940804      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
011400111020      * OBSOLETO --> Non ci sono più clienti abilitati al DDM <-- 20/10/2011
011500111020     D**!!!PA0CLI                47     47
011600941213      * PA0GIA indica se il pgm e' richiamato dalle giacenze
011700941213     D  PA0GIA                48     48
011800991022      *
011900991022      * PASSAGGIO DATI A INTERROGAZIONE DETTAGLIO COLLI - FNLS64R -
012000940808     D PARA64          DS
012100940808     D  P64AAS                 1      4  0
012200940808     D  P64LNP                 5      7  0
012300940808     D  P64NRS                 8      9  0
012400940808     D  P64NSP                10     16  0
012500940808     D  P64F03                17     17
012600940808     D  P64FLG                18     18
012700940808     D  P64PGM                19     28
012800940808     D  P64RSU                29     48
012900940808     D  P64CML                49     49
013000991022      *  PA0CLI indica se il pgm e' richiamato da un cliente (='S')
013100111020      * OBSOLETO --> Non ci sono più clienti abilitati al DDM <-- 20/10/2011
013200111020     D**!!!P64CLI                50     50
013300991022      *
013400991022      * PASSAGGIO A INTERROGAZIONE SPUNTE               - FNLR04R -
013500941230     D PARAM1          DS                  INZ
013600921013     D  VI1NFV                 1      5  0
013700921013     D  VI1LNP                 6      8  0
013800921013     D  VI1LNA                 9     11  0
013900921013     D  VI1NRS                12     13  0
014000921013     D  VI1NCD                14     20  0
014100921013     D  VI1NCA                21     27  0
014200921013     D  VI1NPS                28     29  0
014300921013     D  VI1NPG                30     30  0
014400921013     D  VI1FLG                31     31
014500921013     D  VI1PGM                32     38
014600941230     D  VI1FGS                39     41  0
014700101203     d* bolla
014800101203     d  vi1bolp               42     44  0
014900101203     d  vi1bors               45     46  0
015000101203     d  vi1bosp               47     53  0
015100101203     d  vi1boan               54     57  0
015200101203     D  vi1ERR                58     58
015300991022      *
015400991022      * PASSAGGIO A INTERROGAZIONE VARIAZIONI            - FNLR75R -
015500900927     D PARAM2          DS
015600940804     D  PA2AAS                14     17  0
015700940804     D  PA2LNP                18     20  0
015800940804     D  PA2NRS                21     22  0
015900940804     D  PA2NSP                23     29  0
016000991022      *  PREMUTO F3
016100940804     D  PA2F03                30     30
016200991022      *  FLG = "C" CHIUSURA FILES
016300940804     D  PA2FLG                31     31
016400940804     D  PA2PGM                32     41
016500940804     D  PA2DSA                42     61
016600940804     D  PA2DSP                63     70  0
016700940804     D  PA2RMN                71     85  0
016800940804     D  PA2LNA                86     88  0
016900940804     D  PA2LN1                89    108
017000940804     D  PA2RSD               109    143
017100950126     D  PA2GIA               144    144
017200950126     D  PA2FEL               145    147  0
017300950628     D  PA2TBO               148    148
017400991022      *
017500991022      * PASSAGGIO A INTERROGAZ.BOLLE ARRIVI:BOLLA LEGATA - FNLR36R -
017600920326     D PARAM4          DS
017700940808     D  PA4AAS                14     17  0
017800940808     D  PA4LNP                18     20  0
017900940808     D  PA4NRS                21     22  0
018000940808     D  PA4NSP                23     29  0
018100940810     D  PA4F03                30     30
018200940808     D  PA4FLG                31     31
018300991022      *
018400991022      * RICHIAMO PGM DI INTERROGAZIONE EVENTI             - FNLRB5R -
018500940804     D PARAB5          DS
018600940804     D  PB5AAS                14     17  0
018700940804     D  PB5LNP                18     20  0
018800940804     D  PB5NRS                21     22  0
018900940804     D  PB5NSP                23     29  0
019000940804     D  PB5F03                30     30
019100940804     D  PB5FLG                31     31
019200940804     D  PB5PGM                32     41
019300940804     D  PB5DSA                42     61
019400940804     D  PB5DSP                63     70  0
019500940804     D  PB5RMN                71     85  0
019600940804     D  PB5LNA                86     88  0
019700940804     D  PB5LN1                89    108
019800940804     D  PB5RSD               109    143
019900940804     D  PB5DCP               144    151  0
020000940804     D  PB5TMC               152    152
020100040220      *
020200040220     D WLBDAT          DS
020300040220     D  G02DAT                 1      8  0
020400040220     D  G02INV                 9     16  0
020500040220     D  G02ERR                17     17
020600040220     D  G02TGI                18     22  0
020700040220      *
020800040220     D                 DS
020900040220     D  DCTDCA                 1      8  0
021000040220     D  DCTAAC                 1      4  0
021100040220     D  DCTMGC                 5      8  0
021200040220      *
021300040220      *?definizione oggetto reclamo come spedizione?
021400040220     d ds_OggSped      ds                  inz
021500040220     d  BLPlnp                             inz
021600040220     d  BLPnrs                             inz
021700040220     d  BLPnsp                             inz
021800040220     d  BLPaas                             inz
021900040220      *
022000040220      * CAMPO UNICO COMPRENDENTE RAG.SOC.+AGGIUNTIVI 35 CARATTERI
022100040220     D                 DS
022200040220     D  V2CRSD                 1     35
022300040220     D  V2CNOT                36     70
022400040220     D  V2CNOM                 1     70
022500040220      *
022600040220     D TCUDS           DS
022700040220     D  F1                     1      1
022800040220     D  F3                     3      3
022900040220     D  F2                     2      2
023000040220     D  F4                     4      4
023100040220     D  F56                    5      6
023200040220
023300040220     d Status         sds
023400040220     d  SDSprm           *parms
023500040220
023600040220     D KPJBA         E DS
023700040220     d FIDNA1ds      e ds
023800040226      *
023900040226     d FIDNA2ds      e ds                  inz
024000040226     d  IA2mod       e                     inz('M')
024100040226     d  IA2tor       e                     inz('S')
024200991022      *
024300021218     D CnCr80        E DS
024400991022     D ut§dse0f      E DS
024500991022     D  TCU                  398    697    DIM(50)
024600991022     D  KCU                  698    847P 0 DIM(50)   PACKEVEN
024700991022      *
024800991022     D tnsc20ds      E DS
024900950102     D FNLR97        E DS
025000981023     D DSBL4E        E DS
025100060517     D*DSBL4F        E DS
025200060517     D DSBL4I        E DS
025300010220     D DSBL4M        E DS
025400991227     D OG143         E DS
025500930505     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
025600991022     D TNSD99DS      E DS                  INZ
025700900502     D DS3A          E DS
025800130319     D*DS1B          E DS
025900991022     D DS$2          E DS
026000991022     D DSCV          E DS
026100020402     D DSCC          E DS
026200111020     D**!!!DS3K          E DS
026300951219     D DS7K          E DS
026400090608     d ds5e          e ds
026500991022     D DS7PQRS       E DS
026600991022     D FNLG30DS      E DS
026700991022     D FNLR44DS      E DS
026800991022     D FNLV55DS      E DS
026900051124     D FNLV80DS      E DS
027000991022     D FIDN00DS      E DS
027100091216     D trulintds     E DS
027200110601     D fnlv79ds      E DS
027300991025     d TIBS69DS      E DS                  INZ
027400991025     d DS_cnaco      E DS                  extname(CNACO00F)
027500991025     d DS_cnind      E DS                  extname(CNIND00F)
027600991025     d DS_cnclp      E DS                  extname(CNCLP00F)
027700991025     d DS_fncls      E DS                  extname(FNCLS00F)
027800021218     d FNLRB3Ds      E DS
027900030121     d dTba          e ds
028000031119     d dAr5Gen       e ds
028100030121     d dAr5Ban       e ds
028200030703     d dAr5Bnb       e ds
028300130927     d dAr5emd       e ds
028400030121     d Tibs02Ds      e ds
028500110705     d dsbl4a        e ds
028600130920
028700140131     d TRULORSds     e ds
028800130920
028900040714      * ds per il controllo della presenza di CA per la spedizione richiesta
029000040714     d FIDN12DS      e ds
029100040714     d  skKey                 26    305    dim(20)
029200040714     d  skAnn                306    325    dim(20)
029300040714     d  skDca                326    485  0 dim(20)
029400040714     d  skFca                486    545  0 dim(20)
029500040714     d  skDch                546    705  0 dim(20)
029600040714     d  skCch                706    745    dim(20)
029700040714
029800040714     d dsKey           ds
029900040714     d  dsaac                         4  0
030000040714     d  dsfil                         3  0
030100040714     d  dsnca                         7  0
030200040220      *
030300040220      *   C O S T A N T I
030400040220      *
030500971212     D CMANUA          C                   CONST('Inser.MANUALE in ')
030600971212     D CAUTOM          C                   CONST('Attribuito in xxxx')
030700971212     D CSTAMP          C                   CONST(' Stampa XX')
030800040220      *
030900040220      *   V A R I A B I L I
031000040220      *
031100040220     D kpdpdr          S                   LIKE(APDpdr)
031200040220     D kpdtip          S                   LIKE(APDtip)
031300041028     D wdam            S                   LIKE(blpdam)
031400041028     D wdcm            S                   LIKE(blpdcm)
031500041028     D wdce            S                   LIKE(blpdce)
031600041028     D wnrc            S                   LIKE(blpnrc)
031700041028     D wdcp            S                   LIKE(blpdcp)
031800041028     D wcca            S                   LIKE(blpcca)
031900041028     D whmc            S                   LIKE(blphmc)
032000041028     D wtmc            S                   LIKE(blptmc)
032100041028     D wdet            S                   LIKE(blpdet)
032200041028     D wdut            S                   LIKE(blpdut)
032300041028     D wddc            S                   LIKE(blpddc)
032400041028     D wndc            S                   LIKE(blpndc)
032500040220
032600040220     d wtba            s                   Like(§Ar5Tba)
032700040220     d wdestba         s                   Like(V1dTba)
032800991022     D DATA_eur        S               D   DATFMT(*eur)
032900140131     D wrkoggiiso      s               d
033000991022     D W0080           S              8  0
033100991025     D W0040           S              4  0
033200991022     D Wsvn            S                   LIKE(AR7svn)
033300991022     D Wvan            S                   LIKE(AR7van)
033400991022     D WDIV            S                   LIKE(AR6div)
033500991022     D KTRC            S                   LIKE(AR6TRC)
033600111020     D**!!!$3kinb          S                   LIKE(§3kinb)
033700090603     D** $3kctb          S                   LIKE(§3kctb)
033800020225     d lnantw          S                   LIKE(§ogntw)
033900020225     d lnpntw          S                   LIKE(§ogntw)
034000991022     D KAAS            S                   LIKE(BLPaas)
034100991022     D Klnp            S                   LIKE(BLPlnp)
034200991022     D Knrs            S                   LIKE(BLPnrs)
034300991022     D Knsp            S                   LIKE(BLPnsp)
034400991022     D Kcod            S                   LIKE(TBLcod)
034500991022     D Kkey            S                   LIKE(TBLkey)
034600991022     D Kkcc            S                   LIKE(INDkcc)
034700991022     D Kksc            S                   LIKE(INDksc)
034800991022     D Comaan          S                   LIKE(LBLaan)
034900991022     D Wtfp            S                   LIKE(D55tfp)
035000991022     D Wtstdec         S                   LIKE(§CVfdc)
035100091125     D W_cdi           S                   LIKE(§ar5cdi)
035200991025     D wtibs69r        s              1
035300991022     D W017A           s             17
035400991022     D w0130           s             13  0
035500991022     D w0133           s             13  3
035600991022     D IMPeditato      s             13
035700131007     d wx              s              3  0
035800131007     d wy              s              3  0
035900131007     d wabif9          s              1
036000131007     D FIL_AB          S              3    DIM(85)
036100131007
036200021218     D CF2413          C                   CONST('F24=AlTasti(1/3)')
036300021218     D CF2423          C                   CONST('F24=AlTasti(2/3)')
036400021218     D CF2433          C                   CONST('F24=AlTasti(3/3)')
036500021218     D CF2412          C                   CONST('F24=AlTasti(1/2)')
036600021218     D CF2422          C                   CONST('F24=AlTasti(2/2)')
036700040220
036800930115     I****************************************************************
036900941028     I* INDICATORI UTILIZZATI
037000930115     I****************************************************************
037100961111     I* 01 - BOLLA RIENTRO
037200961111     I* 02 - BOLLA FRNACO FRONTIERA
037300990118     I* 03 - ABILITA F14 DI INTERROGAZIONE GIACENZA
037400991021     I* 04 - ABILITA F13 DI TASSAZIONE 1a BOLLA
037500960523     I* 05 - DISABILITA F7=ANOMALIE
037600991021     I* 06 - ABILITA F4 DI TASSAZIONE 2a BOLLA
037700930115     I* 07 - ESISTE LEGAME BOLLA ABILITA CMD18
037800970227     I* 08 - SI DTT IN ALTA INTENSITA'
037900971212     I* 09 - IMPORT EXPORT SDI
038000990118     I* 10 - ABILITA F10 DI INTERROGAZ. VARIAIZONI BOLLE
038100991123     I* 11 - EMETTO SCRITTA BOLLO o IVA IN TASSAZIONE 2a BOLLA
038200990118     I* 12 - ABILITA INTERROGAZIONE COMUNICAZIONI ANOMALIA
038300980704     I* 13 - EMETTO RIFERIMENTO PARTNER PARTENZA
038400000216     I* 14 - lna dd
038500000216     I* 15 - ABILITA F23
038600941028     I* 16 - ESISTE PARTICOLARITA' CONTRASSEGNO
038700941028     I* 17 - ESISTE PARTICOLARITA' GIACENZA
038800941028     I* 18 - ESISTE PARTICOLARITA' CONSEGNA
038900941028     I* 19 - ESISTE PARTICOLARITA' VARIE
039000941028     I* 20 - SFLDSP
039100941028     I* 21 - SFLDSPCTL - SFLCLR
039200990118     I* 22 - ABILITA F7, F19 E F20
039300941028     I* 23 - SFLEND
039400990118     I* 24 - ABILITA VISUAL. CONTRASSEGNO
039500990118     I* 25 - ABILITA F24 LATRI TASTI
039600951220     I* 26 - PULIZIA SFL LS05S03
039700040220      * 27   Abilita F1  = Aggiunta in reclamo
039800040220      * 28   Abilita F16 = Gestione reclamo
039900941028     I* 31 - PREMUTO TASTO FUNZIONALE
040000941028     I* 33 - ON = SPEDIZIONE IN FRANCO
040100941028     I* 33 - OFF = SPEDIZIONE IN ASSEGNATO
040200930115     I* 34 - C'E' LEGAME MA NON ABILITA CMD18
040300930115     I* 44 - INDICA A VIDEO SPEDIZIONE GIACENTE APERTA
040400010220     I* 55 - ABILITA F08 (interrogazione orm)
040500021218     I* 77 - NOTE SUPERMERCATI
040600941028     I* 80 - USATO NEL VIDEO LEGAMI
040700941028     I* 81 - USATO NEL VIDEO LEGAMI
040800941028     I* 82 - USATO NEL VIDEO LEGAMI
040900941028     I* 83 - USATO NEL VIDEO LEGAMI
041000941028     I* 93 - USATO NEL VIDEO LEGAMI
041100930505     I* 94 - EMETTO CMD23 PER CALCOLARE E VISUALIZZARE DATA CONS.
041200930505     I*      PREVISTA E IL TOTALE ORE DI RITARDO
041300930701     I* 95 - ESISTONO EVENTI DA INTERROGARE
041400930527     I* 98 - DATA CONSEGNA MERCE >< 0
041500941021     I* 99 - GENERICO, UTILIZZABILE A PIACERE
041600930115     I****************************************************************
041700000000     C     *ENTRY        PLIST
041800000000     C                   PARM                    KPJBA
041900040220     c                   parm                    FIDNA1ds
042000040220      *
042100940804     C                   MOVEL     KPJBU         PARAM0
042200981123     C**
042300981123     C                   Z-ADD     1             CODUT
042400981123     C                   CALL      'X§PARUT'
042500991022     C                   PARM                    ut§dse0f
042600021218     C                   MOVEL     REC80         CNCR80
042700981123     C*--- RICERCA CAPOCONTI
042800981123     C                   DO        50            X                 2 0
042900981123     C                   MOVE      TCU(X)        TCUDS
043000981123     C     F56           CABNE     'CG'          END1
043100981123     C     F4            COMP      '1'                                    21
043200981123     C     F4            COMP      '2'                                    22
043300981123     C     F4            COMP      '3'                                    23
043400981123     C     F4            COMP      '6'                                    27
043500981123     C** 1 CLIENTI   21
043600981123     C** 2 FORNITORI 22
043700981123     C** 3 AGENTI    23
043800981123     C     F3            COMP      '0'                                242425
043900981123     C     F3            COMP      'I'                                    26
044000981123     C** 0 ITALIA   25
044100981123     C** 1 ESTERO   24
044200981123     ** I CAPO CONTO IVA
044300981123     C   21
044400981123     CAN 24              Z-ADD     KCU(X)        KCE               4 0
044500981123     C   21
044600981123     CAN 25              Z-ADD     KCU(X)        KCI               4 0
044700981123     C   22
044800981123     CAN 24              Z-ADD     KCU(X)        KFE               4 0
044900981123     C   22
045000981123     CAN 25              Z-ADD     KCU(X)        KFI               4 0
045100981123     C   23
045200981123     CAN 24              Z-ADD     KCU(X)        KAE               4 0
045300981123     C   23
045400981123     CAN 25              Z-ADD     KCU(X)        KAI               4 0
045500981123     C   26              Z-ADD     KCU(X)        KIVA              4 0
045600981123     C   27              Z-ADD     KCU(X)        KBNA              4 0
045700981123     C     END1          TAG
045800981123     C                   END
045900981123     C                   SETOFF                                       212223
046000981123     C                   SETOFF                                       242526
046100981123     C                   SETOFF                                       27
046200940804     C*
046300990701     C                   MOVEL     PA0RSU        V1CRSU
046400940808     C                   Z-ADD     1             CODUT             1 0
046500981123     C**
046600971202     C* DETERMINO LA DATA DEL GIORNO
046700971202     C                   TIME                    W0140            14 0
046800971202     C                   MOVE      W0140         WDTGIO            8 0
046900971202     C                   Z-ADD     WDTGIO        G02DAT
047000971202     C                   MOVEL     *BLANK        G02ERR
047100971202     C                   CALL      'XSRDA8'
047200971202     C                   PARM                    WLBDAT
047300971202     C                   MOVEL     G02INV        WDATEU            8 0
047400991022      *
047500991022      * CARICO LA TABELLA $2
047600991022     C                   MOVEL     '$2'          KCOD
047700991022     C                   MOVEL(P)  1             KKEY
047800991022     C     KTAB1         CHAIN     TABEL00F                           31
047900991022     C  N31              MOVEL     TBLUNI        DS$2
048000991022      *
048100991022      * CARICO IN SCHIERA LA TABELLA CC
048200941021     C                   Z-ADD     *ZERO         WA
048300941021     C                   CLEAR                   CC1
048400941021     C                   CLEAR                   CC2
048500020329     C                   CLEAR                   CC3
048600941021     C                   MOVEL     'CC'          KCOD
048700941021     C     KTAB2         SETLL     TABEL00F
048800941021     C     KTAB2         READE     TABEL00F                               99
048900941021     C     *IN99         DOWEQ     *OFF
049000941021     C     TBLKEY        ANDNE     *BLANKS
049100941021     C     TBLKEY        ANDNE     *ZERO
049200941021     C                   ADD       1             WA
049300941021     C                   MOVE      TBLKEY        CC1(WA)
049400941021     C                   MOVEL     TBLUNI        CC2(WA)
049500020402     C                   MOVEL(P)  TBLUNI        DSCC
049600020402     C                   MOVE      §CCDFE        CC3(WA)
049700941021     C     KTAB2         READE     TABEL00F                               99
049800941021     C                   ENDDO
049900020329      *
050000020329      * CARICO IN SCHIERA LA TABELLA CB
050100020329     C                   Z-ADD     *ZERO         WA
050200020329     C                   CLEAR                   CB1
050300020329     C                   CLEAR                   CB2
050400020329     C                   MOVEL     'CB'          KCOD
050500020329     C     KTAB2         SETLL     TABEL00F
050600020329     C     KTAB2         READE     TABEL00F                               99
050700020329     C     *IN99         DOWEQ     *OFF
050800020329     C     TBLKEY        ANDNE     *BLANKS
050900020329     C     TBLKEY        ANDNE     *ZERO
051000020329     C                   ADD       1             WA
051100020329     C                   MOVE      TBLKEY        CB1(WA)
051200020329     C                   MOVEL     TBLUNI        CB2(WA)
051300020329     C     KTAB2         READE     TABEL00F                               99
051400020329     C                   ENDDO
051500991022      *
051600991022      * Disabilito F7=Anomalie se in elenco pgm chiamanti c'è fnlr44r
051700991022      * Pa0flg="N" -- > Disabilitare F7
051800991022      * Pa0flg="R" -- > Disabilitare F7 - PGM RICHIAMATO DA FNLR36R
051900991022     c                   IF        PA0FLG = 'N'  or  PA0FLG = 'R'
052000991022     C                   eval      *in05 = *on
052100991022     C                   ENDIF
052200940802      * Testo il flag che mi dice se sono richiamato da un cliente
052300940802     C                   SETON                                        22
052400111020
052500111020     C**!!!PA0CLI        IFEQ      'S'
052600940802      * Testo le abilitazioni del cliente
052700111020     C**!!!              EXSR      ABICLI
052800111020     C**!!!$3KINB        CABNE     'S'           FINE
052900111020     C**!!!              SETOFF                                       22
053000111020     C**!!!              ENDIF
053100131007      * controllo se utente partito con orari servizi
053200131007     c                   clear                   fil_ab
053300131007     c                   Clear                   wabif9
053400131007     c                   Clear                   Tibs02ds
053500131007     c                   Eval      T02Mod = 'C'
053600131007     c                   Eval      T02Sif = knsif
053700131007     c                   Eval      T02Cod = 'VPO'
053800131007     c                   Movel(p)  'DECOFI816'   T02Ke1
053900131007     c                   Call      'TIBS02R'
054000131007     c                   Parm                    kpjba
054100131007     c                   Parm                    Tibs02ds
054200131007if  1c                   If        T02Err = *Blanks
054300131007      /free
054400131007         wx= 1 ;
054500131007         wy= 1 ;
054600131007
054700131007         dow wx< 256   ;
054800131007           fil_ab(wy)= %subst(t02uni:wx:3)  ;
054900131007           wy=wy+ 1   ;
055000131007           wx=wx+ 3   ;
055100131007         enddo  ;
055200131007
055300131007      /end-free
055400131007e   1c                   EndIf
055500131007     c                   move      simpou        w003a             3
055600131007     c                   if        %lookup(w003a:fil_ab)>0
055700131007     c                   eval      wabif9='S'
055800131007     c                   endif
055900921013     C*
056000900511     C* INIZIALIZZO CAMPI ANCHE PER MEMORIZZARE BOLLA CHE NON USO
056100900427     C                   Z-ADD     0             BLPNRS
056200900427     C                   Z-ADD     0             BLPNSP
056300900427     C                   Z-ADD     0             BLPLNA
056400900427     C                   Z-ADD     0             BLPRMN
056500930701     C                   CLEAR                   PARAM1
056600930701     C                   CLEAR                   PARAM2
056700930701     C                   CLEAR                   PARAM4
056800940810     C                   CLEAR                   PARA64
056900940810     C                   CLEAR                   PARAB5
057000991022      *----------------------------------------------------------
057100991022      * CARICO BOLLA
057200900516     C                   EXSR      CARBLP
057300940923     C*
057400940923     C* SE LA BOLLA E' INESISTENTE VADO A FINE PROGRAMMA
057500940923     C     PA0NSP        IFEQ      *ZERO
057600940923     C                   GOTO      FINE
057700940923     C                   ENDIF
057800040220      *
057900040220      *?Abilito F1=Aggiunta in Reclami?
058000040220      * (se richiamato da pgm. FIDNA1R...)
058100040220     c                   eval      *in27  = *off
058200040303     c                   if            SDSprm >  1
058300040308     c                             and IA1ins = ' '
058400040220     c                   eval      *in27  = *on
058500040220     c                   endif
058600040220      *
058700040220      *?Abilito F16=Visualizzazione Reclami?
058800040220      * (se richiamato da pgm. FIDNA1R...)
058900040220     c                   eval      TGDtor  = 'S'
059000040220     c                   eval      TGDogg  = ds_OggSped
059100040220     c     K02TGD02      setll     FITGD000
059200040302     c                   eval      *in28   = (%equal(FITGD02L)   and
059300040303     c                                           (SDSprm =  1
059400040303     c                                        or (SDSprm >  1
059500040303     c                                        and IA1ins <> 'N')))
059600900427     C*
059700931115     C* SOLO SE C'E' CONTRASSEGNO VISUALIZZO IL TASTO CMD22
059800931116     C* VERIFICO NELL'ARCHIVIO SE C'E' CONTRASSEGNO
059900990118     C                   SETOFF                                       2412
060000011120     C     KBLP3         SETLL     FNCSB01L
060100011120     c                   if        %equal(fncsb01l)
060200051115     C     KBLP3         SETLL     Fiar901L
060300051115     c                   if        %equal(fiar901l)
060400011120     c                   eval      *in24 = *on
060500011120     c                   endif
060600011120     c                   endif
060700990118     C* CONTROLLO SE C'E' DANNO
060800990125     C                   EXSR      CTRDAN
060900940322      *
061000940802      * EMETTO IL FORMATO DI RIEPILOGO DELLA BOLLA
061100940802     C                   EXSR      RIEPIL
061200940802      *
061300940802      * CMD 3 - FINE LAVORO TORNO A SUBFILE
061400940808     C     *INKC         IFEQ      *ON
061500940808     C     WF03          ORNE      *BLANKS
061600941109     C                   MOVEL     '1'           PA0F03
061700940808     C                   GOTO      FINE
061800940808     C                   ENDIF
061900940802      *
062000940802      * PER F15 VISUALIZZO TUTTA LA BOLLA, PER INVIO ESCO
062100940802     C  NKP              GOTO      FINE
062200931115     C*
062300931115     C                   SETOFF                                       25
062400940804     C                   MOVEL     'S'           WRIEM
062500021218      *
062600021218      * Riempio il campone in fondo alla videata con i tasti scelti
062700021218     C                   exsr      Ges_TastiFun
062800021218     c                   eval      FD01 = FDxx
062900021218     c                   eval      F24D01 = F24Dxx
063000900427     C     FOR01         TAG
063100940804     C     WRIEM         IFEQ      'S'
063200940804     C                   WRITE     LS05T01
063300940804     C                   MOVEL     *BLANKS       WRIEM
063400940804     C                   ENDIF
063500940804     C*
063600021218      *
063700021218      * Imposto la stringa comandi del 'giro' successivo
063800021218     C                   IF        *inKY = *on and $Loop > 1
063900021218     c                   Exsr      Ges_F24
064000021218     c                   eval      FD01 = FDxx
064100021218     c                   eval      F24D01 = F24Dxx
064200021218     C                   ENDIF
064300021218      *
064400940804     C                   EXFMT     LS05D01
064500940808     C                   MOVEL     *BLANKS       WF03
064600931115      *
064700990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
064800931115     C   KY*IN25         IFEQ      *ON
064900931115     C                   SETOFF                                       25
065000931115     C                   ELSE
065100931115     C                   SETON                                        25
065200931115     C                   ENDIF
065300931115     C   KY              GOTO      FOR01
065400910806     C*
065500900530     C* CMD 3 - FINE LAVORO TORNO A SUBFILE
065600900530     C   KC              GOTO      FINE
065700921013     C*
065800921013     C* CMD12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
065900921013     C   KL              GOTO      FINE
066000911105     C*
066100921013     C* TASTI DI FUNZIONE VARI
066200921013     C                   EXSR      COMAND
066300940808     C*
066400940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
066500940808     C     WF03          IFNE      *BLANKS
066600941109     C                   MOVEL     '1'           PA0F03
066700940808     C                   GOTO      FINE
066800940808     C                   ENDIF
066900940808     C*
067000921013     C   31              GOTO      FOR01
067100921013     C*
067200900427     C* SE TUTTO OK EMETTO FORMATO2
067300931115     C                   SETOFF                                       25
067400940804     C                   MOVEL     'S'           WRIEM
067500021218      *
067600021218      * Riempio il campone in fondo alla videata con i tasti scelti
067700021218     C                   exsr      Ges_TastiFun
067800021218     c                   eval      FD02 = FDxx
067900021218     c                   eval      F24D02 = F24Dxx
068000900427     C     FOR02         TAG
068100940804     C     WRIEM         IFEQ      'S'
068200940804     C                   WRITE     LS05T01
068300940804     C                   MOVEL     *BLANKS       WRIEM
068400940804     C                   ENDIF
068500940804     C*
068600021218      * Imposto la stringa comandi del 'giro' successivo
068700021218     C                   IF        *inKY = *on and $Loop > 1
068800021218     c                   Exsr      Ges_F24
068900021218     c                   eval      FD02 = FDxx
069000021218     c                   eval      F24D02 = F24Dxx
069100021218     C                   ENDIF
069200021218      *
069300940804     C                   EXFMT     LS05D02
069400940808     C                   MOVEL     *BLANKS       WF03
069500931115      *
069600990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
069700931115     C   KY*IN25         IFEQ      *ON
069800931115     C                   SETOFF                                       25
069900931115     C                   ELSE
070000931115     C                   SETON                                        25
070100931115     C                   ENDIF
070200931115     C   KY              GOTO      FOR02
070300910806     C*
070400900427     C* CMD12 - RITORNO VIDEATA 1
070500900427     C   KL              GOTO      FOR01
070600940923     C*
070700940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
070800940923     C                   EXSR      CTRD02
070900940923     C   31              GOTO      FOR02
071000921013     C*
071100921013     C* TASTI DI FUNZIONE VARI
071200921013     C                   EXSR      COMAND
071300940808     C*
071400940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
071500940808     C     WF03          IFNE      *BLANKS
071600941109     C                   MOVEL     '1'           PA0F03
071700940808     C                   GOTO      FINE
071800940808     C                   ENDIF
071900921013     C   31              GOTO      FOR02
072000920429     C*
072100910806     C*
072200930112     C* SE ESISTONO DATI ENTRO IN TERZA VIDEATA
072300931115     C                   SETOFF                                       25
072400940804     C                   MOVEL     'S'           WRIEM
072500021218      *
072600021218      * Riempio il campone in fondo alla videata con i tasti scelti
072700021218     C                   exsr      Ges_TastiFun
072800021218     c                   eval      FD03 = FDxx
072900021218     c                   eval      F24D03 = F24Dxx
073000930112     C     FOR03         TAG
073100940804     C     WRIEM         IFEQ      'S'
073200940804     C                   WRITE     LS05T01
073300940804     C                   MOVEL     *BLANKS       WRIEM
073400940804     C                   ENDIF
073500940804     C*
073600951219     C                   Z-ADD     1             REC3
073700021218      *
073800021218      * Imposto la stringa comandi del 'giro' successivo
073900021218     C                   IF        *inKY = *on and $Loop > 1
074000021218     c                   Exsr      Ges_F24
074100021218     c                   eval      FD03 = FDxx
074200021218     c                   eval      F24D03 = F24Dxx
074300021218     C                   ENDIF
074400951219     C                   WRITE     LS05Z03
074500951219     C                   EXFMT     LS05C03
074600940808     C                   MOVEL     *BLANKS       WF03
074700931115      *
074800990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
074900931115     C   KY*IN25         IFEQ      *ON
075000931115     C                   SETOFF                                       25
075100931115     C                   ELSE
075200931115     C                   SETON                                        25
075300931115     C                   ENDIF
075400931115     C   KY              GOTO      FOR03
075500910806     C*
075600900507     C* CMD12 - RITORNO VIDEATA 2
075700900507     C   KL              GOTO      FOR02
075800930114     C*
075900930114     C* TASTI DI FUNZIONE VARI
076000930114     C                   EXSR      COMAND
076100940808     C*
076200940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
076300940808     C     WF03          IFNE      *BLANKS
076400941109     C                   MOVEL     '1'           PA0F03
076500940808     C                   GOTO      FINE
076600940808     C                   ENDIF
076700930114     C   31              GOTO      FOR03
076800900504     C*
076900000000     C     FINE          TAG
077000971202     C*
077100971202     C                   MOVEL     'C'           D55TLA
077200971202     C                   CALL      'FNLV55R'
077300991022     C                   PARM                    FNLV55DS
077400991025      *
077500991025     C                   IF        Wtibs69r  <>  *BLANKS
077600991025     C                   eval      I69TLA  = 'C'
077700991025     C                   call      'TIBS69R'
077800991025     C                   parm                    TIBS69DS
077900991025     c                   ENDIF
078000040714     C*
078100040714     C                   MOVEL     'C'           I12TLA
078200040714     C                   CALL      'FIDN12R'
078300040714     C                   PARM                    FIDN12DS
078400091216     C*
078500091216     C                   MOVEL     'C'           Iinttla
078600091216     C                   CALL      'TRULINTR'
078700091216     C                   PARM                    trulintds
078800040714      *
078900991025      *
079000930614     C* REIMPOSTO KPJBU
079100940810     C   KC              MOVEL     '1'           PA0F03
079200940804     C                   MOVEL     PARAM0        KPJBU
079300930614     C*
079400000000     C                   SETON                                        LR
079500940808     C**************************************************************************
079600940808     C*    COMANDI UGUALI NELLE 2 VIDEATE
079700940808     C**************************************************************************
079800921013     C     COMAND        BEGSR
079900931115     C*
080000921013     C                   SETOFF                                       31
080100040220      *
080200040220      *?F1 - Aggiunta bolla in reclamo?
080300040220if  1c                   if        *inKA   = *on
080400040220     c                   clear                   FIDNA1ds
080500040220     c                   eval      IA1ins  = 'S'
080600040220     c                   eval      IA1tor  = 'S'
080700040220     c                   eval      IA1ogg  = ds_OggSped
080800040220     c                   goto      Fine
080900040220     c*****              return
081000040220e   1c                   endif
081100040220      *
081200990118     C* F5 - INTERROGAZIONE EVENTI
081300040220if  1C     *INKE         IFEQ      *ON
081400930526     C                   SETON                                        31
081500941213     C                   CLEAR                   PARAB5
081600940804     C                   MOVEL     PA0AAS        PB5AAS
081700940804     C                   MOVEL     PA0LNP        PB5LNP
081800940804     C                   MOVEL     PA0NRS        PB5NRS
081900940804     C                   MOVEL     PA0NSP        PB5NSP
082000940804     C                   MOVEL     'FNLS05R'     PB5PGM
082100940804     C                   MOVEL     V1CRSU        PB5DSA
082200940808     C                   MOVEL     V1CDSP        PB5DSP
082300940804     C                   MOVEL     V2CRMN        PB5RMN
082400940804     C                   MOVEL     V1CLNA        PB5LNA
082500940804     C                   MOVEL     V1DLNA        PB5LN1
082600940804     C                   MOVEL     V2CRSD        PB5RSD
082700940804     C                   MOVEL     V3CDCP        PB5DCP
082800940804     C                   MOVEL     V3CTMC        PB5TMC
082900940804     C                   MOVEL     PARAB5        KPJBU
083000940804     C                   CALL      'FNLRB5R'
083100930526     C                   PARM                    KPJBA
083200940808     C                   MOVEL     '1'           PA0B5
083300940808     C                   MOVEL     'S'           WRIEM
083400040220e   1C                   ENDIF
083500930526     C*
083600921013     C* CMD 7 - VISUALIZZA ANOMALIE
083700040220if  1C     *INKG         IFEQ      *ON
083800991022     C                   CLEAR                   FNLR44DS
083900960523     C                   MOVEL     PA0AAS        D44AAS
084000960523     C                   MOVEL     PA0LNP        D44FIL
084100960523     C                   MOVEL     PA0NRS        D44NRS
084200960523     C                   MOVEL     PA0NSP        D44NSP
084300960523     C                   MOVEL     'P'           D44TBO
084400960523     C                   MOVE      'I'           D44MOD
084500960523     C                   SETON                                        31
084600991022     C                   MOVEL     FNLR44DS      KPJBU
084700960523     C                   CALL      'FNLR44R'
084800960523     C                   PARM                    KPJBA
084900940808     C                   MOVEL     'S'           WRIEM
085000040220e   1C                   ENDIF
085100010220     C*
085200010220     C* CMD 8 - VISUALIZZA ORM
085300040220if  1C     *INKH         IFEQ      *ON
085400010220
085500010220     C                   clear                   PARM01
085600010220     C                   move      v1cpoe        ppoe
085700010220     C                   move      v1cnor        pnor
085800010220     C                   move      v1cnsr        pnsr
085900010220     C                   move      v1cnrv        pnrv
086000010220     C                   move      '5'           psce
086100010220     C                   move      v1clnp        pfgs
086200010220     C                   movel(p)  PARM01        KPJBU
086300010220
086400010220     C                   call      'FIOR07R'
086500010220     C                   parm                    KPJBA
086600010220     C                   MOVEL     'S'           WRIEM
086700010220     C                   SETON                                        31
086800010220
086900040220e   1C                   ENDIF
087000130912c
087100130912     c* F9 - Orari servizi
087200130912     c                   if        *inki=*on
087300131007     c                   if        wabif9='S'
087400130930     c                   exsr      sr_orarisr
087500131007     c                   endif
087600130927     C                   MOVEL     'S'           WRIEM
087700130927     C                   SETON                                        31
087800130912e   1C                   ENDIF
087900021218     C*
088000021218     C* F11 - Note supermercati
088100040220if  1C     *INKK         IFEQ      *ON
088200021218     C     *IN77         ANDEQ     *ON
088300021218      * imposto i campi 'bolla' della ds
088400021218     C                   clear                   FnLrb3ds
088500030122     c                   Movel     v1cRsu        Rb3Rsu
088600030121     C                   z-add     SimFel        RB3Fil
088700021218     C                   z-add     V1CAAS        RB3AAS
088800030121     C                   z-add     V1CLP1        RB3LNP
088900021218     C                   z-add     V1CNRS        RB3NRS
089000021218     C                   z-add     V1CNSP        RB3NSP
089100021218     C                   z-add     V1CDSP        RB3DSP
089200021218     C                   z-add     BLPRMN        RB3RMN
089300021218     C                   z-add     V1CLNA        RB3LNA
089400021218     C                   movel     BLPRSD        RB3RSD
089500021218     C                   z-add     V1CPKF        RB3PKF
089600021218     C                   z-add     V1CNCL        RB3NCL
089700021218     C                   z-add     V1CVLF        RB3VLB
089800030703      * bancali - part.varie "B"
089900040220If  2c                   If        %subst(BlpGva:1:1) = 'B'
090000030121     C                   z-add     §Ar5Tba       RB3Tba
090100030121     C                   z-add     V1cNba        RB3Nba
090200030121     C                   z-add     §Ar5Tb2       RB3Tb2
090300030121     C                   z-add     V1cNb2        RB3Nb2
090400030703     c                   Movel     V2cGva        Rb3Gva
090500040220e   2c                   EndIf
090600030703      * bancali - part.varie "O"
090700040220If  2c                   If        %subst(BlpGva:1:1) = 'O'
090800030703     C                   z-add     §Ar5bTba      RB3Tba
090900030703     C                   z-add     V1cNba        RB3Nba
091000030703     C                   z-add     §Ar5bTb2      RB3Tb2
091100030703     C                   z-add     V1cNb2        RB3Nb2
091200030703     c                   Movel     V2cGva        Rb3Gva
091300030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
091400030703     c                   Z-Add     §Ar5bCor      Rb3Cor
091500040220e   2c                   EndIf
091600021218      * richiamo PGM per note supermercati
091700021218     C                   call      'FNLRB3R'
091800030122     c                   Parm                    Kpjba
091900030121     C                   PARM                    Fnlrb3Ds
092000021218      *
092100030703     c                   Eval      *In31 = *On
092200021218     C                   MOVEL     'S'           WRIEM
092300040220e   1C                   ENDIF
092400921013     C*
092500921013     C* CMD14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
092600040220if  1C     *INKN         IFEQ      *ON
092700930701     C     *IN03         ANDEQ     *ON
092800941213     C     PA0GIA        ANDEQ     *BLANK
092900991022     C                   CLEAR                   FNLG30DS
093000941213     C                   MOVEL     'P'           D30POA
093100941213     C                   MOVEL     PA0AAS        D30AAS
093200941213     C                   MOVEL     PA0LNP        D30LNP
093300941213     C                   MOVEL     PA0NRS        D30NRS
093400941213     C                   MOVEL     PA0NSP        D30NSP
093500991022     C                   MOVEL     FNLG30DS      KPJBU
093600930701     C                   SETON                                        31
093700941213     C                   CALL      'FNLG30C'
093800921013     C                   PARM                    KPJBA
093900940808     C                   MOVEL     'S'           WRIEM
094000040220e   1C                   ENDIF
094100040220      *
094200040220      *?F16 - Gestione reclami?
094300040226if  1c                   if        *inKQ   = *on
094400040226     c                   reset                   FIDNA2ds
094500040226if  2c                   if        SDSprm  > 1
094600040226     c                   eval      IA2mod  = 'I'
094700040226e   2c                   endif
094800040226     c                   eval      IA2ogg  = ds_OggSped
094900040226     c                   call      'FIDNA2R'
095000040226     c                   parm                    KPJBA
095100040226     c                   parm                    FIDNA2ds
095200040226     c                   movel     OA2f03        WF03
095300040226     c                   movel     'S'           WRIEM
095400051201     C                   SETON                                        31
095500040220e   1c                   endif
095600921013     C*
095700921013     C* CMD18 - LEGAME BOLLA: VADO IN INTERROG. BOLLE ARRIVI
095800040220if  1C     *INKS         IFEQ      *ON
095900921013     C                   EXSR      CMD18
096000921013     C                   SETON                                        31
096100941222     C                   MOVEL     'S'           WRIEM
096200040220e   1C                   ENDIF
096300921013     C*
096400921013     C* CMD21 - DETTAGLIO SEGNACOLLI
096500040220if  1C     *INKV         IFEQ      *ON
096600940808     C                   MOVEL     PA0AAS        P64AAS
096700940808     C                   MOVEL     PA0LNP        P64LNP
096800940808     C                   MOVEL     PA0NRS        P64NRS
096900940808     C                   MOVEL     PA0NSP        P64NSP
097000940808     C                   MOVEL     *BLANKS       P64F03
097100940808     C                   MOVEL     *BLANKS       P64FLG
097200940808     C                   MOVEL     *BLANKS       P64PGM
097300940808     C                   MOVEL     'FNLS05R'     P64PGM
097400940808     C                   MOVEL     V1CRSU        P64RSU
097500940808     C                   MOVEL     PA0CML        P64CML
097600111020     C**!!!              MOVEL     PA0CLI        P64CLI
097700940808     C                   MOVEL     PARA64        KPJBU
097800930701     C                   SETON                                        31
097900940804     C                   CALL      'FNLS64R'
098000921013     C                   PARM                    KPJBA
098100940808     C                   MOVEL     KPJBU         PARA64
098200940808     C                   MOVEL     P64F03        WF03
098300940808     C                   MOVEL     'S'           WRIEM
098400040220e   1C                   ENDIF
098500921013     C*
098600990118     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
098700040220do  1C   KJ              DO
098800930526     C                   SETON                                        31
098900940804     C                   MOVEL     PA0AAS        PA2AAS
099000940804     C                   MOVEL     PA0LNP        PA2LNP
099100940804     C                   MOVEL     PA0NRS        PA2NRS
099200940804     C                   MOVEL     PA0NSP        PA2NSP
099300940804     C                   MOVEL     *BLANKS       PA2F03
099400940804     C                   MOVEL     *BLANKS       PA2FLG
099500940804     C                   MOVEL     *BLANKS       PA2PGM
099600940804     C                   MOVEL     'FNLS05R'     PA2PGM
099700940804     C                   MOVEL     V1CRSU        PA2DSA
099800940808     C                   MOVEL     V1CDSP        PA2DSP
099900940804     C                   MOVEL     V2CRMN        PA2RMN
100000940804     C                   MOVEL     V1CLNA        PA2LNA
100100940804     C                   MOVEL     V1DLNA        PA2LN1
100200940804     C                   MOVEL     V2CRSD        PA2RSD
100300950628     C                   MOVEL     'P'           PA2TBO
100400941222     C                   MOVEL     PA0SIM        PA2FEL
100500940804     C                   MOVEL     PARAM2        KPJBU
100600940804     C                   CALL      'FNLR75R'
100700921013     C                   PARM                    KPJBA
100800940808     C                   MOVEL     '1'           PA075
100900940808     C                   MOVEL     'S'           WRIEM
101000040220e   1C                   ENDDO
101100921013     C*
101200990118     C* F19 - INTERROGA SPUNTE
101300040220if  1C     *INKT         IFEQ      *ON
101400930701     C                   SETON                                        31
101500101203     C                   MOVEL     PA0AAS        vi1boan
101600101203     C                   MOVEL     PA0LNP        vi1bolp
101700101203     C                   MOVEL     PA0NRS        vi1bors
101800101203     C                   MOVEL     PA0NSP        vi1bosp
101900950116     C                   MOVEL     BLPFLS        VI1LNP
102000940804     C                   MOVEL     PA0NRS        VI1NRS
102100941230     C                   MOVEL     9             VI1NPG
102200930701     C                   MOVEL     99            VI1NPS
102300101203     C                   MOVEL     'FNLS05R'     VI1PGM
102400930701     C                   MOVEL     '1'           VI1FLG
102500930701     C                   Z-ADD     BLPLNA        VI1LNA
102600930701     C*
102700040220if  2C     BLPNCD        IFGT      0
102800930701     C                   Z-ADD     BLPNCD        VI1NCD
102900930701     C                   Z-ADD     BLPNCA        VI1NCA
103000040220x   2C                   ELSE
103100930701     C*
103200940802     C     KBLP3         SETLL     FNBLT01L
103300940802     C     KBLP3         READE     FNBLT01L                               31
103400040220do  3C  N31              DO
103500940808     C                   Z-ADD     BLTNSC        VI1NCD
103600940802     C     KBLP3         SETGT     FNBLT01L
103700940802     C     KBLP3         READPE    FNBLT01L                               31
103800940808     C  N31BLTNSC        IFNE      VI1NCD
103900940808     C                   MOVEL     BLTNSC        VI1NCA
104000040220     C                   ENDIF
104100040220e   3C                   ENDDO
104200040220e   2C                   ENDIF
104300930701     C*
104400930701     C                   MOVEL     PARAM1        KPJBU
104500940804     C                   CALL      'FNLR04R'
104600921013     C                   PARM                    KPJBA
104700940808     C                   MOVEL     'S'           WRIEM
104800040220e   1C                   ENDIF
104900930505     C*
105000990118     C* F23 - VISUALIZZA DATA CONSEGNA PREVISTA E ORE DI RITARDO
105100040220do  1C   KX              DO
105200930505     C                   SETOFF                                       94
105300930505     C                   SETON                                        31
105400930505     C*
105500930505     C                   MOVEL     ' '           D98TLA
105600930505     C                   MOVEL     'P'           D98TBO
105700930505     C                   MOVEL     BLPAAS        D98AAS
105800930505     C                   MOVEL     BLPLNP        D98LNP
105900930505     C                   MOVEL     BLPNRS        D98NRS
106000930505     C                   MOVEL     BLPNSP        D98NSP
106100930505     C*
106200970925     C                   CALL      'TNSD99R'
106300991022     C                   PARM                    TNSD99DS
106400930505     C*
106500940808     C                   Z-ADD     D98DCE        G02INV
106600940808     C                   Z-ADD     *ZEROS        G02DAT
106700940808     C                   MOVEL     '3'           G02ERR
106800940808     C                   CALL      'XSRDA8'
106900940808     C                   PARM                    WLBDAT
107000940808     C                   Z-ADD     G02DAT        V1CDCE
107100940808     C                   Z-ADD     D98NRC        V1CNRC
107200930526     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
107300930526     C*   CONSEGNA PREVISTA
107400940808     C  N98              Z-ADD     D98HCE        V1CHMC
107500930528     C  N98              Z-ADD     D98HCE        VIDHC2
107600930505     C*
107700940808     C* SE PA098= 1  E' STATA FATTA CALL PGM
107800940808     C                   MOVEL     '1'           PA098
107900040220e   1C                   ENDDO
108000931115     C*
108100990118     C* F20 - VISUALIZZAZIONE AFFIDABILITA'
108200040220do  1C   KU              DO
108300950102     C                   CLEAR                   FNLR97
108400950102     C                   Z-ADD     PA0AAS        R97AAS
108500950102     C                   Z-ADD     PA0LNP        R97LNP
108600950102     C                   Z-ADD     PA0NRS        R97NRS
108700950102     C                   Z-ADD     PA0NSP        R97NSP
108800950102     C                   Z-ADD     V1CDSP        R97DSP
108900950102     C                   Z-ADD     V2CRMN        R97RMN
109000950102     C                   Z-ADD     V1CLNA        R97LNA
109100950102     C                   Z-ADD     V3CDCP        R97DCP
109200950102     C                   MOVEL     V2CRSD        R97RSD
109300950102     C                   MOVEL     V1CRSU        R97RSU
109400950328     C                   MOVEL     V1DLNA        R97DLA
109500950102     C                   MOVEL     PA0CML        R97CML
109600950102     C                   MOVE      'P'           R97AP
109700950102     C   95              MOVE      '1'           R97EVE
109800950102     C                   Z-ADD     V1CDBR        R97DBR
109900950102     C                   Z-ADD     V1CDPC        R97DPC
110000950102     C                   Z-ADD     V1CDUC        R97DUC
110100950328     C                   MOVEL     V3CTMC        R97TMC
110200950102     C                   Z-ADD     V2CCAS        R97CAS
110300950102     C                   MOVEL     V2CVCA        R97VCA
110400931115     C                   MOVE      *BLANKS       KPJBU
110500950102     C                   MOVEL     FNLR97        KPJBU
110600940804     C                   CALL      'FNLR97R'                            31
110700931115     C                   PARM                    KPJBA
110800950102     C                   MOVEL     KPJBU         FNLR97
110900950102     C                   MOVEL     R97F03        WF03
111000931115     C                   SETON                                        31
111100940808     C                   MOVEL     'S'           WRIEM
111200040220e   1C                   ENDDO
111300931115     C*
111400990118     C* F22 - INTERROGAZIONE CONTRASSEGNO, SOLO SE PRESENTE
111500040220if  1C   KWV2CCAS        IFGT      *ZERO
111600991022     C                   CLEAR                   tnsc20ds
111700950125     C                   Z-ADD     1             D20INT
111800950125     C                   Z-ADD     PA0AAS        D20ANN
111900950125     C                   Z-ADD     PA0LNP        D20LNP
112000950125     C                   Z-ADD     PA0NRS        D20NSR
112100950125     C                   Z-ADD     PA0NSP        D20NSP
112200991022     C                   MOVEL     tnsc20ds      KPJBU
112300950125     C                   CALL      'TNSC23R'                            31
112400931115     C                   PARM                    KPJBA
112500931115     C                   SETON                                        31
112600040220     C                   MOVEL     'S'           WRIEM
112700040220e   1C                   ENDIF
112800941021     C*
112900991022     C* ? FORMATO VIDEO:  LS05D08  (TASSAZIONE 1a BOLLA) ?
113000991022     C*  EMETTO SOLO SE VI E' TASSAZIONE
113100040220do  1C   KM              DO
113200941021     C     WRIEM         IFEQ      'S'
113300941021     C                   WRITE     LS05T01
113400941021     C                   MOVEL     *BLANKS       WRIEM
113500941021     C                   ENDIF
113600941021     C                   WRITE     LS05D08
113700941021     C*
113800941021     C                   EXFMT     LS05C08
113900941021     C                   SETON                                        31
114000040220e   1C                   ENDDO
114100991022      *
114200991022      * ? FORMATO VIDEO:  LS05D04  (TASSAZIONE 2a BOLLA) ?
114300040220do  1C   KD              DO
114400991022     C     WRIEM         IFEQ      'S'
114500991022     C                   WRITE     LS05T01
114600991022     C                   MOVEL     *BLANKS       WRIEM
114700991022     C                   ENDIF
114800991022     C                   EXFMT     LS05D04
114900991022     C                   SETON                                        31
115000040220e   1C                   ENDDO
115100991022      **
115200990118     C** F2 - INTERROGAZIONE C.A. SE CE N'E' PIU' DI UNA EMETTO SFL
115300990129     C* Pa0flg="C" -- > Disabilitare F2 PERCHE' RICHIAMATO DA C.A.
115400040220if  1C   KBPA0FLG        IFNE      'C'
115500990129     C                   EXSR      CMD02
115600040220e   1C                   ENDIF
115700931115     C*
115800921013     C                   ENDSR
115900990125     C**************************************************************************
116000990125     C** F2 - INTERROGAZIONE C.A.
116100990125     C**************************************************************************
116200990125     C     CMD02         BEGSR
116300990125     C     CONTCA        IFEQ      1
116400991022     C                   CLEAR                   FIDN00DS
116500990125     C                   Z-ADD     VHCAAC        I00AAC
116600990125     C                   Z-ADD     VHCFIL        I00FIL
116700990125     C                   Z-ADD     VHCNCA        I00NCA
116800990125     C                   Z-ADD     PA0AAS        I00AAS
116900990125     C                   Z-ADD     PA0NRS        I00NRS
117000990125     C                   Z-ADD     PA0NSP        I00NSP
117100990125     C                   Z-ADD     PA0LNP        I00LNP
117200990125     C                   MOVEL     'P'           I00TPB
117300991022     C                   MOVEL     FIDN00DS      KPJBU
117400990125     C                   CALL      'FIDN01R'
117500990125     C                   PARM                    KPJBA
117600000309     C                   PARM                    CALLricors        1
117700990125     C                   SETON                                        31
117800990125     C                   MOVEL     'S'           WRIEM
117900990125     C                   ELSE
118000990125     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
118100990125     C                   SETOFF                                       21
118200990125     C                   WRITE     LS05C11
118300990125     C                   SETON                                        212023
118400990125     C                   CLEAR                   NRR11
118500990125     C**
118600040714     C**""*KBLP3         CHAIN     FNDCT02L                           31
118700040714     C**""**IN31         DOWEQ     *OFF
118800040714     C**""*DCTATB        IFEQ      ' '
118900040714     C**""*              Z-ADD     DCTAAC        V11AAC
119000040714     C**""*              Z-ADD     DCTFIL        V11FIL
119100040714     C**""*              Z-ADD     DCTNCA        V11NCA
119200040714     C**""*              Z-ADD     DCTDCA        G02INV
119300040714     C**""*              Z-ADD     *ZERO         G02DAT
119400040714     C**""*              MOVEL     '3'           G02ERR
119500040714     C**""*              CALL      'XSRDA8'
119600040714     C**""*              PARM                    WLBDAT
119700040714     C**""*              Z-ADD     G02DAT        V11DCA
119800040714     C**""*              Z-ADD     DCTFCA        V11FCA
119900040714     C**""*DCTFCA        CHAIN     FNDFA01L                           31
120000040714     C**N31              MOVEL     DFADES        V11DFA
120100040714     C**""*
120200040714     C**""*              ADD       1             NRR11             4 0
120300040714     C**""*              WRITE     LS05S11
120400040714     C**""*              ENDIF
120500040714     C**""**
120600040714     C**""*KBLP3         READE     FNDCT02L                               31
120700040714     C**""**             ENDDO
120800040714      *
120900040714      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
121000040714      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
121100040714      * legate alla spedizione che passo
121200040714     c                   clear                   fidn12ds
121300040714     c                   eval      i12aas = pa0aas
121400040714     c                   eval      i12lnp = pa0lnp
121500040714     c                   eval      i12nrs = pa0nrs
121600040714     c                   eval      i12nsp = pa0nsp
121700040714     c                   eval      i12fel = 'E'
121800040714     c                   eval      i12fan = 'E'
121900040714      *
122000040714     c                   call      'FIDN12R'
122100040714     c                   parm                    fidn12ds
122200040714      *
122300040714      * se non ci sono errori
122400040714     c                   if        o12err = ' '
122500040714      * se numero di CA trovate maggiore di zero
122600040714     c                   if        o12nca > 0
122700040714     c
122800040714     c                   z-add     *zeros        ii                2 0
122900040714     c     1             do        o12nca        ii
123000040714     c                   movea     skkey(ii)     dskey
123100040714     C                   Z-ADD     dsaac         V11AAC
123200040714     C                   Z-ADD     dsfil         V11FIL
123300040714     C                   Z-ADD     dsnca         V11NCA
123400040714     C                   Z-ADD     skdca(ii)     G02INV
123500040714     C                   Z-ADD     *ZERO         G02DAT
123600040714     C                   MOVEL     '3'           G02ERR
123700040714     C                   CALL      'XSRDA8'
123800040714     C                   PARM                    WLBDAT
123900040714     C                   Z-ADD     G02DAT        V11DCA
124000040714     C                   Z-ADD     skfca(ii)     V11FCA
124100040714     C     V11FCA        CHAIN     FNDFA01L                           31
124200040714     C  N31              MOVEL     DFADES        V11DFA
124300040714     C**
124400040714     C                   ADD       1             NRR11             4 0
124500040714     C                   WRITE     LS05S11
124600040714     C                   ENDDO
124700040714      *
124800040714     c                   endif
124900040714     c                   endif
125000040714      *
125100990125     C* EMETTO FORMATO
125200990125     C     FOR11         TAG
125300990125     C                   Z-ADD     1             NRR11
125400990125     C                   WRITE     LS05T01
125500990125     C                   WRITE     LS05Z11
125600990125     C                   EXFMT     LS05C11
125700990125     C* F12 - RITORNO
125800990125     C     *INKL         IFEQ      *OFF
125900990125     C                   READC     LS05S11                                31
126000990125     C     *IN31         DOWEQ     *OFF
126100990125     C     V11SCE        IFEQ      '5'
126200991022     C                   CLEAR                   FIDN00DS
126300990125     C                   Z-ADD     V11AAC        I00AAC
126400990125     C                   Z-ADD     V11FIL        I00FIL
126500990125     C                   Z-ADD     V11NCA        I00NCA
126600990125     C                   Z-ADD     PA0AAS        I00AAS
126700990125     C                   Z-ADD     PA0NRS        I00NRS
126800990125     C                   Z-ADD     PA0NSP        I00NSP
126900990125     C                   Z-ADD     PA0LNP        I00LNP
127000990125     C                   MOVEL     'P'           I00TPB
127100991022     C                   MOVEL     FIDN00DS      KPJBU
127200990125     C                   CALL      'FIDN01R'
127300990125     C                   PARM                    KPJBA
127400000309     C                   PARM                    CALLricors
127500990125     C**
127600990125     C                   CLEAR                   V11SCE
127700990125     C                   UPDATE    LS05S11
127800990125     C                   ENDIF
127900990125     C**
128000990125     C                   READC     LS05S11                                31
128100990125     C                   ENDDO
128200990125     C                   GOTO      FOR11
128300990125     C                   ENDIF
128400990125     C                   SETON                                        31
128500990125     C                   MOVEL     'S'           WRIEM
128600990125     C*
128700990125     C                   ENDIF
128800990125     C                   ENDSR
128900990125     C**************************************************************************
129000990125     C*    CONTROLLO SE C'E' UNA O PIU' C.A.
129100990125     C**************************************************************************
129200990125     C     CTRDAN        BEGSR
129300990125     C                   CLEAR                   CONTCA
129400040714     C**""*KBLP3         CHAIN     FNDCT02L                           31
129500040714     C**""**IN31         DOWEQ     *OFF
129600040714     C**""*DCTATB        IFEQ      ' '
129700040714     C**""**             SETON                                        12
129800040714     C**""**             ADD       1             CONTCA            3 0
129900040714     C**""**             Z-ADD     DCTAAC        VHCAAC
130000040714     C**""**             Z-ADD     DCTFIL        VHCFIL
130100040714     C**""**             Z-ADD     DCTNCA        VHCNCA
130200040714     C**""**             ENDIF
130300990125     C*
130400040714     C**""*KBLP3         READE     FNDCT02L                               31
130500040714     C**""**             ENDDO
130600040714      *
130700040714      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON
130800040714      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
130900040714      * late e legate alla spedizione che passo
131000040714     c                   clear                   fidn12ds
131100040714     c                   eval      i12aas = pa0aas
131200040714     c                   eval      i12lnp = pa0lnp
131300040714     c                   eval      i12nrs = pa0nrs
131400040714     c                   eval      i12nsp = pa0nsp
131500040714     c                   eval      i12fel = 'E'
131600040714     c                   eval      i12fan = 'E'
131700040714      *
131800040714     c                   call      'FIDN12R'
131900040714     c                   parm                    fidn12ds
132000040714      *
132100040714      * se non ci sono errori
132200040714     c                   if        o12err = ' '
132300040714      * se numero di CA trovate maggiore di zero
132400040714     c                   if        o12nca > 0
132500040714     c                   z-add     O12NCA        CONTCA            3 0
132600040714     C                   SETON                                        12
132700040714     C                   MOVEA     SKKEY(O12NCA) DSKEY
132800040714     C                   Z-ADD     DSAAC         VHCAAC
132900040714     C                   Z-ADD     DSFIL         VHCFIL
133000040714     C                   Z-ADD     DSNCA         VHCNCA
133100040714     C                   ENDIF
133200040714     C                   ENDIF
133300040714      *
133400990125     C                   ENDSR
133500940808     C**************************************************************************
133600971212     C*    CARICO RECORD DA VISUALIZZARE/MANUTENZIONARE
133700940808     C**************************************************************************
133800900511     C     CARBLP        BEGSR
133900940808     C*
134000930112     C*** PULIZIA CAMPI
134100930112     C*
134200930112     C                   MOVEL     ' '           VI2CEI
134300950628     C                   MOVEL     'P'           PA2TBO
134400940804     C                   Z-ADD     0             V3CDCP
134500930526     C*
134600920326     C                   SETOFF                                       440307
134700951011     C                   SETOFF                                       949813
134800021218     C                   SETOFF                                       0977
134900940923     C*
135000940923     C                   MOVEL     PA0AAS        V1CAAS
135100940926     C                   MOVEL     PA0LNP        V1CLNP
135200940923     C                   MOVEL     PA0NRS        V1CNRS
135300940923     C                   MOVE      PA0NSP        V1CNSP
135400940923     C*
135500900511     C* CHAIN
135600041028     c                   clear                   wesarb            1
135700940802     C     KBLP3         CHAIN     FNBLP01L                           31
135800041028     C     KBLP3         CHAIN     FNarb01L
135900041028     c                   if        %found(fnarb01l)
136000041028     c                   eval      wesarb='S'
136100041028     c                   else
136200041028     c                   eval      wesarb='N'
136300041028     c                   endif
136400940926     C*
136500940928     C* SE NON TROVATA BOLLA, PROVO NELLO STORICO
136600940928     C* E SE NON TROVATA PULISCO I CAMPI CHIAVE E ESCO
136700010713     C     *IN31         IFEQ      *ON
136800010716     C                   MOVEL     *ZERO         PA0AAS
136900010716     C                   MOVEL     *ZERO         PA0LNP
137000010716     C                   MOVEL     *ZERO         PA0NRS
137100010716     C                   MOVEL     *ZERO         PA0NSP
137200010716     C                   GOTO      ENDBLP
137300010713     C                   ENDIF
137400021218     C*
137500040220     C* 77 ON - Esistono note supermercati da interrogare
137600021218     C                   Z-ADD     PA0AAS        KAAS
137700021218     C                   Z-ADD     PA0LNP        KLNP
137800021218     C                   Z-ADD     PA0NRS        KNRS
137900021218     C                   Z-ADD     PA0NSP        KNSP
138000021218     C                   MOVEL     'SCR'         KTRD
138100021218     C     KAR51         CHAIN     FIAR5000                           31
138200021218     C                   If        *in31 = *off and Ar5Atb = ' '
138300021218     C                   seton                                        77
138400021218     C                   else
138500021218     C                   setoff                                       77
138600021218     C                   endif
138700940926     C*
138800900927     C* CONTROLLO SE SPEDIZIONE GIACENTE
138900050406     C     KBLP3         CHAIN     tIGCP01L                           31
139000940923     C*
139100901004     C  N31GCPDCG        IFEQ      0
139200910321     C                   SETON                                        4403
139300910321     C                   ELSE
139400910321     C                   SETON                                        03
139500901004     C                   END
139600111020     C**!03PA0CLI        COMP      'S'                                0303
139700900511     C*
139800950214     C                   CLEAR                   V2CFLS
139900950214     C     BLPNCD        IFNE      *ZERO
140000950214     C     BLPFNS        ORNE      *BLANKS
140100940923     C                   MOVEL     BLPFLS        V2CFLS
140200950214     C                   ENDIF
140300940923     C                   MOVEL     PA0NRS        V7CNRS
140400940808     C                   MOVEL     BLPLNP        V1CLP1
140500940805     C                   MOVEL     BLPCBO        V1CCBO
140600940809     C*
140700940809     C                   MOVEL     BLPGGA        V2CGGA
140800940923     C                   MOVEL     *BLANKS       V2IGGA
140900940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
141000940923     C     V2CGGA        COMP      *BLANKS                            1717
141100940809     C*
141200940809     C                   MOVEL     BLPGMA        V2CGMA
141300940923     C                   MOVEL     *BLANKS       V2IGMA
141400940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
141500940923     C     V2CGMA        COMP      *BLANKS                            1818
141600940809     C*
141700960208     C                   MOVEL     BLPGVA        V2CGVA
141800960208     C                   MOVE      ' '           V2CGVA
141900940923     C                   MOVEL     *BLANKS       V2IGVA
142000940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
142100940923     C     V2CGVA        COMP      *BLANKS                            1919
142200940322      *
142300911105     C* NON SEQUENZIALI
142400910912     C     BLPFNS        IFEQ      'S'
142500940808     C                   MOVEL     SEQ(1)        V2CFNS
142600910912     C                   ELSE
142700940808     C                   MOVEL     *BLANKS       V2CFNS
142800910912     C                   END
142900930112     C* CONTROLLO SE ESISTONO VARIAZIONI PER LA BOLLA
143000131007     C     KBLP3         CHAIN     FIARBF2C                           31
143100111020     C**!!!PA0CLI        IFNE      'S'
143200920429     C  N31              SETON                                        10
143300111020     C**!!!              ENDIF
143400910806     C*
143500920409     C* TIPO BOLLA CONTROLLO SE FRANCO
143600940804     C                   MOVEL     '3A'          KCOD
143700940804     C                   MOVEL     *BLANKS       KKEY
143800940805     C                   MOVEL     BLPCBO        KKEY
143900940809     C     KTAB1         CHAIN     TABEL00F                           31
144000940923     C  N31              MOVEL     TBLUNI        DS3A
144100940923     C   31              CLEAR                   DS3A
144200900516     C                   MOVEL     §3ATB1        FLGTB1            1
144300940804     C                   MOVEL     §3ADES        V1DCBO
144400940805     C                   MOVEL     §3ADES        V2DCBO
144500940804     C                   MOVEL     §3ADES        V7DCBO
144600940408     C     FLGTB1        IFEQ      'F'
144700910418     C                   SETON                                        33
144800900511     C                   END
144900900511     C* LINEA ARRIVO E DECODIFICA
145000940804     C                   MOVEL     BLPLNA        V1CLNA
145100020225     C     BLPLNA        CHAIN     AZORG01L
145200020225     c                   if        not %found(azorg01l)
145300020225     C                   MOVEL     *BLANKS       ORGDES
145400020225     c                   endif
145500940804     C                   MOVEL     ORGDES        V1DLNA
145600940804     C                   MOVEL     ORGDES        V2DLNA
145700940810     C                   MOVEL     ORGDES        V9DLNA
145800020225     c                   eval      og143  = ORGde3
145900020225     c                   eval      lnantw = §ogntw
146000971202     C* TERMINAL DI PARTENZA DELLA LINEA DI ARRIVO
146100991022     C                   CLEAR                   FNLV55DS
146200971202     C                   MOVEL     'P'           D55TPT
146300971202     C                   MOVE      BLPLNA        D55LIN
146400971202     C                   Z-ADD     WDATEU        D55DRF
146500971202     C                   CALL      'FNLV55R'
146600991022     C                   PARM                    FNLV55DS
146700971202     C                   MOVE      D55TFP        WTFP
146800940804     C* LINEA PARTENZA E DECODIFICA
146900020225     C     BLPLNP        CHAIN     AZORG01L
147000020225     c                   if        not %found(azorg01l)
147100020225     C                   MOVEL     *BLANKS       ORGDES
147200020225     c                   endif
147300940804     C                   MOVEL     ORGDES        V1DLNP
147400020225     c                   eval      og143  = ORGde3
147500020225     c                   eval      lnpntw = §ogntw
147600920410     C* MITTENTE
147700940804     C                   MOVEL     BLPRSM        V1CRSM
147800940808     C                   MOVEL     BLPRSM        V2CRSM
147900940805     C                   MOVEL     BLPRSM        V7CRSM
148000940804     C                   MOVEL     BLPINM        V1CINM
148100940808     C                   MOVEL     BLPINM        V7CINM
148200940804     C                   MOVEL     BLPPRM        V1CPRM
148300940804     C                   MOVEL     BLPCAM        V1CCAM
148400940804     C                   MOVEL     BLPLOM        V1CLOM
148500940808     C                   MOVEL     BLPLOM        V2CLOM
148600940808     C                   MOVEL     BLPLOM        V7CLOM
148700940808     C                   MOVEL     BLPPRM        V2CPRM
148800940804     C                   MOVEL     BLPNZM        V1CNZM
148900940808     C                   MOVEL     BLPFAP        V1CFAP
149000981023     C                   MOVEL     BLPCTR        V2CCTR
149100060216     C* NUMERO ORM: DA ar4 (Tipo record 'M')
149200950412     C                   MOVEL     BLPAAS        KAAS
149300950412     C                   MOVEL     BLPLNP        KLNP
149400950412     C                   MOVEL     BLPNRS        KNRS
149500950412     C                   MOVEL     BLPNSP        KNSP
149600010220     C                   MOVEL     'M'           KTRC
149700010220     C                   clear                   v1cpoe
149800010220     C                   clear                   v1cnsr
149900010220     C                   clear                   v1cnor
150000010220     C                   clear                   v1cnrv
150100010220     C                   setoff                                       55
150200060216     C     Kar41         CHAIN     Fiar401L                           99
150300010220     C     *in99         ifeq      *off
150400060216     C     ar4not        ifne      *blanks
150500060216     C                   movel     ar4not        dsbl4m
150600010220     C                   move      §b4poe        v1cpoe
150700010220     C                   move      §b4nsr        v1cnsr
150800010220     C                   move      §b4nor        v1cnor
150900010220     C                   move      §b4nrv        v1cnrv
151000040713     c     korm          Setll     Fnorm01l
151100040713     c                   If        %Equal(fnorm01l)
151200010220     C                   seton                                        55
151300040713     c                   EndIf
151400010220     C                   endif
151500010220     C                   endif
151600010220     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
151700060216     C* DA Fiar4 SE RECORD ESISTENTE
151800950412     C                   MOVEL     'P'           KTRC
151900060216     C     Kar41         CHAIN     Fiar401L                           99
152000950412     C     *IN99         IFEQ      *OFF
152100060216     C                   MOVEL     ar4NOT        V1CPIM
152200060216     C                   MOVE      ar4NOT        V2CPID
152300950412     C                   ELSE
152400950412     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI BLPCPI
152500930112     C     BLPCPI        IFNE      *BLANKS
152600940804     C   33              MOVEL     BLPCPI        V1CPIM
152700940804     C  N33              MOVEL     BLPCPI        V2CPID
152800930112     C                   ELSE
152900940808     C                   MOVEL     *BLANKS       V1CPIM
153000940808     C                   MOVEL     *BLANKS       V2CPID
153100930112     C                   END
153200950412     C                   END
153300061108      * codice fiscale mittente e destinatario da fiar4 rcd Q
153400061108     c                   clear                   v1cdfm
153500061108     c                   clear                   v2cdfd
153600061108     c                   eval      ktrc = 'Q'
153700061108     c     kar41         chain     fiar401l
153800061108     c                   if        %found(fiar401l)
153900061108     c                   movel     ar4not        v1cdfm
154000061108     c                   move      ar4not        v2cdfd
154100061108     c                   endif
154200900525     C*
154300940906     C* CERCO LA DESCRIZIONE AGGIUNTIVA PER IL DESTINATARIO
154400940808     C                   MOVEL     BLPAAS        KAAS
154500940808     C                   MOVEL     BLPLNP        KLNP
154600940808     C                   MOVEL     BLPNRS        KNRS
154700940808     C                   MOVEL     BLPNSP        KNSP
154800940906     C                   MOVEL     'D'           KTRC
154900060216     C     Kar41         CHAIN     Fiar401L                           99
155000060216     C  N99              MOVEL     ar4NOT        V2CNOT
155100950125     C   99              MOVEL     *BLANKS       V2CNOT
155200940808     C*
155300971212     C* SOLO PER LE FILIALI ESTERE IMPORT/EXPORT VISUALIZZO RIF PARTNER
155400971212     C                   CLEAR                   V1CEST
155500951011     C                   MOVEL     'E'           KTRC
155600060216     C     Kar41         CHAIN     Fiar401L                           99
155700060216     C  N99              MOVEL     ar4NOT        V2CRPP
155800980704     C  N99              SETON                                        13
155900020225     c   99              if        (lnantw = 'EEX' or lnantw = 'EUP' or
156000020225     c                              lnantw = 'FED') or
156100020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
156200020225     c                              lnpntw = 'FED')
156300980704     C                   SETON                                        13
156400980704     C                   ENDIF
156500951011     C   99              CLEAR                   V2CRPP
156600971212     C* SE E' SDI IMPORTO ANCHE TIPO INSERIMENTO E STAMPA
156700020225     c                   if        not *in99 and (lnpntw = 'EUP' or
156800020225     c                                            lnantw = 'EUP')
156900971212     C**
157000060216     C                   MOVEL     ar4NOT        DSBL4E
157100971212    3C     §B4EMA        IFEQ      'M'
157200971212     C                   MOVEL     CMANUA        W021A            21
157300971212     C                   SETON                                        09
157400971212    4C     §B4EAT        IFEQ      'P'
157500971212     C                   MOVE      'PART'        W021A
157600971212    4C                   ENDIF
157700971212    4C     §B4EAT        IFEQ      'A'
157800971212     C                   MOVE      'ARR.'        W021A
157900971212    4C                   ENDIF
158000971212     C                   MOVE      W021A         V1CEST
158100971212    3C                   ENDIF
158200971212    C**
158300971212    3C     §B4EMA        IFEQ      'A'
158400971212     C                   MOVEL     CAUTOM        W018A            18
158500971212     C                   SETON                                        09
158600971212    4C     §B4EAT        IFEQ      'P'
158700971212     C                   MOVE      'PART'        W018A
158800971212    4C                   ENDIF
158900971212    4C     §B4EAT        IFEQ      'A'
159000971212     C                   MOVE      'ARR.'        W018A
159100971212    4C                   ENDIF
159200971212     C*
159300971212     C                   MOVEL     W018A         V1CEST
159400971212     C                   MOVE      CSTAMP        V1CEST
159500971212     C**
159600971212    4C     §B4EST        IFEQ      'N'
159700971212     C                   MOVE      'NO'          V1CEST
159800971212    4C                   ELSE
159900971212     C                   MOVE      'SI'          V1CEST
160000971212    4C                   ENDIF
160100991022      *
160200971212    3C                   ENDIF
160300971212    2C                   ENDIF
160400991227     c* DPD
160500000216     c                   setoff                                       14
160600000216     c                   clear                   v2crtn
160700000216     c                   clear                   v2czon
160800020225     c                   if        lnantw = 'DPD'
160900991227     c                   seton                                        13
161000060517     C***                MOVEL     'F'           KTRC
161100060517     C                   MOVEL     'I'           KTRC
161200060216     C     Kar41         CHAIN     Fiar401L                           99
161300060517     C**N99              MOVEL     ar4NOT        dsbl4f
161400060517     c** 99              clear                   dsbl4f
161500060517     C  N99              MOVEL     ar4NOT        dsbl4I
161600060517     c   99              clear                   dsbl4I
161700060517     C***                MOVEL     §b4fnp        V2CRPP
161800060517     C                   MOVEL     §b4ipn        V2CRPP
161900060517     c****               if        §b4rtn>*zeros or §b4zon>*zeros
162000060517     c****               movel     §b4rtn        v2crtn
162100060517     c****               movel     §b4zon        v2czon
162200060517     c                   if        §b4ida>*zeros or §b4iza>*zeros
162300060517     c                   movel     §b4ida        v2crtn
162400060517     c                   movel     §b4iza        v2czon
162500000216     c                   seton                                        14
162600000216     c                   setoff                                       09
162700000216     c                   endif
162800050304     c                   else
162900050304     c* dpd import
163000050304     c                   if        lnpntw = 'DPD'
163100060517     c****               clear                   dsbl4f
163200060517     C****               MOVEL     'F'           KTRC
163300060517     c                   clear                   dsbl4i
163400060517     C                   MOVEL     'I'           KTRC
163500060216     C     Kar41         CHAIN     Fiar401L
163600060216     c                   if        %found(fiar401l)
163700050304     c                   seton                                        13
163800060517     C***                MOVEL     ar4NOT        dsbl4f
163900060517     C                   MOVEL     ar4NOT        dsbl4I
164000060517     C***                MOVEL     §b4fnp        V2CRPP
164100060517     C                   MOVEL     §b4ipn        V2CRPP
164200050304     c                   endif
164300050304     c                   endif
164400991227     c                   endif
164500981023     C***
164600981023     C* ASSEGNATO
164700981023     C     *IN33         IFEQ      *OFF
164800981023     C** MITTENTE
164900981023     C     BLPCCM        IFNE      0
165000981023     C                   MOVEL     BLPCCM        V2CCDM
165100981023     C                   MOVEL     BLPCCM        V1CKSC
165200981023     C                   ELSE
165300981023     C                   CLEAR                   V2CCDM
165400981023     C                   CLEAR                   V1CKSC
165500981023     C                   ENDIF
165600981023     C* DESTINATARIO
165700981023     C                   MOVEL     BLPKSC        V2CCDD
165800981023     C                   MOVEL     BLPKSC        V1CCDD
165900991116     c                   eval      KTRC = '1'
166000991116     C     KAR6          CHAIN     FIAR6000                           99
166100991116     C                   IF        *in99 = *OFF and ar6ksc > *zeros
166200991116     C                   MOVEL     ar6ksc        V2CCDD
166300991116     C                   MOVEL     ar6ksc        V1CCDD
166400991116     C                   MOVEL     ar6ctr        V2CCTR
166500991116     c                   ENDIF
166600981023     C                   ELSE
166700981023     C* FRANCO
166800981023     C                   MOVEL     BLPKSC        V1CKSC
166900981023     C                   MOVEL     BLPKSC        V2CCDM
167000981023     C                   CLEAR                   V2CCDD
167100981023     C                   CLEAR                   V1CCDD
167200981023     C                   ENDIF
167300050117     C                   MOVEL     BLPXCO        V2CXCO
167400940808     C*
167500940808     C* SE C'E' IL COD. MITTENTE, RILEVO IL NR.TELEFONICO
167600940809     C                   MOVE      V2CCDM        WQUATT            4 0
167700940809     C     WQUATT        IFNE      8888
167800940809     C     V2CCDM        ANDNE     *ZERO
167900981123     C                   MOVEL     KCI           KKCC
168000940808     C                   MOVEL     V2CCDM        KKSC
168100940808     C     KIND1         CHAIN     CNIND00F                           99
168200940808     C  N99              MOVEL     INDTEL        V2CTEL
168300940808     C   99              MOVEL     *BLANKS       V2CTEL
168400940809     C                   ENDIF
168500940808     C*
168600940808     C                   MOVEL     BLPRSD        V2CRSD
168700940808     C                   MOVEL     BLPRSD        V7CRSD
168800940810     C                   MOVEL     BLPRSD        V9CRSD
168900940808     C                   MOVEL     BLPIND        V2CIND
169000940808     C                   MOVEL     BLPIND        V7CIND
169100940808     C                   MOVEL     BLPPRD        V2CPRD
169200940808     C                   MOVEL     BLPCAD        V2CCAD
169300940808     C                   MOVEL     BLPLOD        V2CLOD
169400940808     C                   MOVEL     BLPLOD        V7CLOD
169500940808     C                   MOVEL     BLPNZD        V2CNZD
169600940804     C                   MOVEL     BLPFIN        V2CFIN
169700940804     C                   MOVEL     BLPGC1        V2CGC1
169800940804     C                   MOVEL     BLPGC2        V2CGC2
169900961111     C                   MOVEL     *BLANKS       V2CFBR
170000961111     C                   MOVEL     *BLANKS       V2CFFR
170100961111     C                   SETOFF                                       0102
170200940804     C     BLPFBR        IFEQ      'S'
170300940804     C                   MOVEL     'SI'          V2CFBR
170400961111     C                   SETON                                        01
170500940804     C                   ENDIF
170600961111     C     BLPFBR        IFEQ      'F'
170700961111     C                   MOVEL     'FR'          V2CFFR
170800961111     C                   SETON                                        02
170900961111     C                   ENDIF
171000970227     C     BLPFST        IFEQ      'S'
171100970131     C     BLPFST        OREQ      'C'
171200970227     C                   MOVEL     'SI DDT'      V1DBAM
171300970227     C                   SETON                                        08
171400970130     C                   ELSE
171500970227     C                   MOVEL     'NO DDT'      V1DBAM
171600970227     C                   SETOFF                                       08
171700970130     C                   ENDIF
171800970227     C*
171900970227     C                   SELECT
172000970227     C     BLPFST        WHENEQ    'C'
172100970227     C     BLPFST        OREQ      'K'
172200970227     C                   MOVEL     'CLIENTE '    V1DFST
172300970227     C     BLPFST        WHENEQ    'S'
172400970227     C     BLPFST        OREQ      'Y'
172500970227     C                   MOVEL     'PARTENZA'    V1DFST
172600970227     C     BLPFST        WHENEQ    'J'
172700970227     C                   MOVEL     'ARRIVO  '    V1DFST
172800970227     C                   OTHER
172900970227     C                   MOVEL     'NO      '    V1DFST
173000970227     C                   ENDSL
173100970227     C*
173200941219     C     BLPFDN        IFEQ      'S'
173300941219     C                   MOVEL     'SI'          V1CFDN
173400941219     C                   ELSE
173500941219     C                   MOVEL     *BLANKS       V1CFDN
173600941219     C                   ENDIF
173700940808     C                   MOVEL     BLPFTM        V1CFTM
173800940804     C                   MOVEL     *BLANKS       V1DTSP
173900940804     C                   MOVEL     '5E'          KCOD
174000940804     C                   MOVEL     *BLANKS       KKEY
174100940808     C                   MOVEL     BLPTSP        KKEY
174200940809     C     KTAB1         CHAIN     TABEL00F                           99
174300090608     C  N99              MOVEL     TBLUNI        ds5e
174400090608     C  N99              MOVEL     §5ed08        V1DTSP
174500941021     C*
174600940805     C                   MOVEL     BLPCTS        V8CCTS
174700940804     C                   MOVEL     BLPQFT        V2CQFT
174800940804     C                   MOVEL     BLPVMD        V2CVMD
174900940804     C                   MOVEL     BLPVAD        V2CVAD
175000940804     C                   MOVEL     BLPIAS        V2CIAS
175100940804     C                   MOVEL     BLPVAS        V2CVAS
175200940808     C                   MOVEL     BLPNAS        V1CNAS
175300940804     C                   MOVEL     BLPNCL        V1CNCL
175400941026     C                   MOVEL     BLPPKF        V1CPKF
175500990909     C                   MOVEL     BLPPKB        V1CPKB
175600940808     C                   MOVEL     BLPVLB        V1CVLB
175700940808     C                   MOVEL     BLPFVB        V1CFVB
175800940808     C                   MOVEL     BLPVLF        V1CVLF
175900940808     C                   MOVEL     BLPFVF        V1CFVF
176000940808     C* SE BLPNCR = 99999  A VIDEO AZZERO NCR E VLC
176100930505     C     BLPNCR        IFEQ      99999
176200940805     C                   MOVEL     *ZEROS        BLPNCR
176300940805     C                   MOVEL     *ZEROS        BLPVLC
176400940808     C                   ENDIF
176500940808     C                   MOVEL     BLPVLC        V1CVLC
176600940808     C                   MOVEL     BLPNCR        V1CNCR
176700990909     C* SE BLPNCP = 99999  A VIDEO AZZERO NCP E PKC
176800990909     C     BLPNCP        IFEQ      99999
176900990909     C                   MOVEL     *ZEROS        BLPNCP
177000990909     C                   MOVEL     *ZEROS        BLPPKC
177100990909     C                   ENDIF
177200990909     C                   MOVEL     BLPPKC        V1CPKC
177300990909     C                   MOVEL     BLPNCP        V1CNCP
177400031119
177500031119      * Referente e Telefono del destinatario
177600031119     c                   Clear                   V2cRef
177700031119     c                   Clear                   V2cTeld
177800110610     c* profilo utente e terminale che ha immesso la bolla
177900110610     c                   Clear                   Vidpru
178000110610     c                   Clear                   Vidnmtd
178100110610     c                   setoff                                       46
178200031119      * li prendo da Fiar500f record 'GEN'
178300031119     c                   Clear                   dAr5Gen
178400091125     c                   clear                   w_cdi
178500031119     c                   Z-Add     BlpAas        Kaas
178600031119     c                   Z-Add     BlpLNp        Klnp
178700031119     c                   Z-Add     BlpNrs        Knrs
178800031119     c                   Z-Add     BlpNsp        Knsp
178900031119     c                   Eval      Ktrd = 'GEN'
179000031119     c     kAr51         Chain     Fiar501l
179100031119If  1c                   If        %Found(Fiar501l)
179200031119     c                   Movel     Ar5Uni        Dar5Gen
179300031119     c                   Eval      V2cRef = §Ar5Ref
179400031119     c                   Eval      V2cTeld = §Ar5Teld
179500091125     c* memorizzo eventuale codice beneficiario assegno
179600091125     c                   eval      w_cdi=§ar5cdi
179700110610      *
179800110610     c                   Eval      Vidpru = §Ar5bpru
179900110610     c                   Eval      Vidnmtd = §Ar5bnmtd
180000110610     c                   if        vidpru<>*blanks
180100110610     c                   seton                                        46
180200110610     c                   endif
180300031119    1c                   EndIf
180400130319     c* Vedo se ha il packing list
180500130320     c                   if        §ar5alS='S'
180600130320     c                   eval      v1dar8='PACKING LIST PDF'
180700130319     C                   ELSE
180800130320     c                   if        §ar5alx='S'
180900130320     c                   eval      v1dar8='PACKING LIST    '
181000130319     c                   else
181100130319     C                   CLEAR                   V1DAR8
181200130319     c                   endif
181300130319     C                   ENDIF
181400110531
181500110531     c* email destinatario
181600110601     c                   setoff                                       45
181700110531     c                   clear                   v2cemW
181800130927     c                   clear                   dar5emd
181900110531      * lo prendo da Fiar500f record 'EMD'
182000110531     c                   Z-Add     BlpAas        Kaas
182100110531     c                   Z-Add     BlpLNp        Klnp
182200110531     c                   Z-Add     BlpNrs        Knrs
182300110531     c                   Z-Add     BlpNsp        Knsp
182400110531     c                   Eval      Ktrd = 'EMD'
182500130927     c*    kAr51         setll     Fiar501l
182600130927     c     kAr51         chain     Fiar501l
182700130927If  1c                   If        %found(Fiar501l)
182800130927     c                   eval      dar5emd=ar5uni
182900130927     c                   if        §ar5tel<>*blanks or §ar5eml<>*blanks
183000110610     c                   eval      v2cemd='SI'
183100110601     c                   seton                                        45
183200130927     c                   endif
183300110531     c                   endif
183400040301
183500040301      * Se colli di valore visualizzo "V" nell'apposito campo
183600040301      *  con lo specifico attributo di visualizzazione
183700040301if  1c                   if        §AR5bva = *blanks
183800040301     c                   eval      V2Cbva  = *blanks
183900040301x   1c                   else
184000040301     c                   eval      V2Cbva  = 'V'
184100040301e   1c                   endif
184200031119
184300030121      * Bancali
184400030121     c                   Clear                   V1dTba
184500030121     c                   Clear                   V1dTb2
184600030122     c                   Clear                   V1cNba
184700030122     c                   Clear                   V1cNb2
184800030703      * Record 'BAN'
184900030703If  0c                   If        %subst(BlpGva:1:1) = 'B'
185000030121     c                   Clear                   dAr5Ban
185100030121     c                   Eval      Ktrd = 'BAN'
185200030121     c     kAr51         Chain     Fiar501l
185300030121If  1c                   If        %Found(Fiar501l)
185400030121     c                   Movel     Ar5Uni        Dar5Ban
185500030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
185600030122      * per le note relative al mancato reso dei bancali
185700030122     c                   If        Not *In77 and
185800030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
185900030122     c                   Eval      *In77 = *On
186000030122     c                   EndIf
186100030121     c                   Eval      V1cNba = §Ar5Nba
186200030121     c                   Eval      V1cNb2 = §Ar5Nb2
186300030121If  2c                   If        §Ar5Tba <> *Zeros
186400030703     c                   Eval      wtba = §Ar5Tba
186500030703     c                   ExSr      Sr_DecTba
186600030703     c                   Eval      V1dTba = wdestba
186700030121    2c                   EndIf
186800030121If  2c                   If        §Ar5Tb2 <> *Zeros
186900030703     c                   Eval      wtba = §Ar5Tb2
187000030703     c                   ExSr      Sr_DecTba
187100030703     c                   Eval      V1dTb2 = wdestba
187200030121    2c                   EndIf
187300030121    1c                   EndIf
187400030703    0c                   EndIf
187500030703      * Record 'BNB'  -  Spedizione OVER 25 colli
187600030703If  0c                   If        %subst(BlpGva:1:1) = 'O'
187700030703     c                   Clear                   dAr5Bnb
187800030703     c                   Eval      Ktrd = 'BNB'
187900030703     c     kAr51         Chain     Fiar501l
188000030703If  1c                   If        %Found(Fiar501l)
188100030703     c                   Movel     Ar5Uni        Dar5Bnb
188200030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
188300030703      * per i colli originali in caso di spedizione over 25 colli
188400030703     c  n77              Eval      *In77 = *On
188500030703     c                   Eval      V1cNba = §Ar5bNba
188600030703     c                   Eval      V1cNb2 = §Ar5bNb2
188700030703If  2c                   If        §Ar5bTba <> *Zeros
188800030703     c                   Eval      wtba = §Ar5bTba
188900030703     c                   ExSr      Sr_DecTba
189000030703     c                   Eval      V1dTba = wdestba
189100030703    2c                   EndIf
189200030703If  2c                   If        §Ar5bTb2 <> *Zeros
189300030703     c                   Eval      wtba = §Ar5bTb2
189400030703     c                   ExSr      Sr_DecTba
189500030703     c                   Eval      V1dTb2 = wdestba
189600030703    2c                   EndIf
189700030703    1c                   EndIf
189800030703    0c                   EndIf
189900920409     C*
190000920409     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
190100940804     C                   MOVEL     'TR'          KCOD
190200940804     C                   MOVEL     *BLANKS       KKEY
190300940808     C                   MOVEL     V2CCTR        UNOCTR            1
190400940804     C                   MOVEL     UNOCTR        KKEY
190500940809     C     KTAB1         CHAIN     TABEL00F                           31
190600940804     C                   MOVEL     TBLUNI        V2DCTR
190700910730     C*
190800940808     C* VISUALIZZO V2CCDM SOLO SE LA LNP DEL CODICE = LNP DELLA BOLLA
190900930505     C                   MOVEL     BLPCCM        NUM3              3 0
191000930505     C     BLPLNP        IFEQ      NUM3
191100940808     C                   MOVE      BLPCCM        V2CCDM
191200930505     C                   END
191300930505     C*
191400940804     C                   MOVEL     BLPRMN        V2CRMN
191500940804     C                   MOVEL     BLPRMA        V2CRMA
191600940804     C                   MOVEL     BLPRMO        V2CRMO
191700940804     C                   MOVEL     BLPNCD        V2CNCD
191800940804     C                   MOVEL     BLPNCA        V2CNCA
191900940804     C                   MOVEL     BLPZNC        V2CZNC
192000940804     C                   MOVEL     BLPCTM        V1CCTM
192100130319     C***                MOVEL     '1B'          KCOD
192200130319     C***                MOVEL(P)  blpctm        KKEY
192300130319     C***  KTAB1         CHAIN     TABEL00F                           31
192400130319     C**N31              MOVEL     TBLUNI        DS1B
192500130319     c** 31              clear                   ds1b
192600051128     c* Vedo se ha il packing list
192700130319     c***                if        §1bf16='S'
192800130319     c***                eval      v1dar8='PACKING LIST'
192900130319     C***                ELSE
193000130319     C***                CLEAR                   V1DAR8
193100130319     C***                ENDIF
193200051104     c
193300051104     c                   select
193400051104     c                   when      blpffd='S'
193500051104     C                   MOVEL     'SI '         V2CFFD
193600051104     c                   when      blpffd='M'
193700051104     C                   MOVEL     'SAB'         V2CFFD
193800051104     C                   other
193900940905     C                   MOVEL     *BLANKS       V2CFFD
194000051104     C                   ENDsl
194100051104     c
194200940808     C                   MOVEL     BLPNFV        V1CNFV
194300940906     C                   MOVEL     BLPNRT        V1CNRT
194400940906     C                   MOVEL     *BLANKS       V1DFPP
194500940906     C     BLPFPP        IFEQ      'M'
194600010220     C                   MOVEL     'MAT'         V1DFPP
194700940906     C                   ELSE
194800010220     C                   MOVEL     'POM'         V1DFPP
194900940906     C                   ENDIF
195000910118     C* GIRO DATA SPEDIZIONE
195100940804     C                   MOVEL     BLPAAS        V1CDSP
195200940804     C                   MOVE      BLPMGS        V1CDSP
195300940804     C                   Z-ADD     V1CDSP        G02INV
195400940804     C                   Z-ADD     *ZEROS        G02DAT
195500940804     C                   MOVEL     '3'           G02ERR
195600940804     C                   CALL      'XSRDA8'
195700940804     C                   PARM                    WLBDAT
195800940804     C                   Z-ADD     G02DAT        V1CDSP
195900940808     C* DATA RITIRO MERCE
196000940804     C                   Z-ADD     BLPDRT        G02INV
196100940804     C                   Z-ADD     *ZEROS        G02DAT
196200940804     C                   MOVEL     '3'           G02ERR
196300940804     C                   CALL      'XSRDA8'
196400940804     C                   PARM                    WLBDAT
196500940804     C                   Z-ADD     G02DAT        V1CDRT
196600900518     C* DATA BORDERO'
196700940804     C                   Z-ADD     BLPDBR        G02INV
196800940804     C                   Z-ADD     *ZEROS        G02DAT
196900940804     C                   MOVEL     '3'           G02ERR
197000940804     C                   CALL      'XSRDA8'
197100940804     C                   PARM                    WLBDAT
197200940804     C                   Z-ADD     G02DAT        V1CDBR
197300940804     C* DATA PARTENZA PRIMO COLLO
197400940804     C                   Z-ADD     BLPDPC        G02INV
197500940804     C                   Z-ADD     *ZEROS        G02DAT
197600940804     C                   MOVEL     '3'           G02ERR
197700940804     C                   CALL      'XSRDA8'
197800940804     C                   PARM                    WLBDAT
197900940804     C                   Z-ADD     G02DAT        V1CDPC
198000940804     C* DATA PARTENZA ULTIMO COLLO
198100940804     C                   Z-ADD     BLPDUC        G02INV
198200940804     C                   Z-ADD     *ZEROS        G02DAT
198300940804     C                   MOVEL     '3'           G02ERR
198400940804     C                   CALL      'XSRDA8'
198500940804     C                   PARM                    WLBDAT
198600940804     C                   Z-ADD     G02DAT        V1CDUC
198700910806     C* DATA SPUNTA ENTRATA
198800940808     C     BLPDSE        IFGT      *ZERO
198900940808     C                   Z-ADD     BLPDSE        G02INV
199000940808     C                   Z-ADD     *ZEROS        G02DAT
199100940808     C                   MOVEL     '3'           G02ERR
199200940808     C                   CALL      'XSRDA8'
199300940808     C                   PARM                    WLBDAT
199400940808     C                   Z-ADD     G02DAT        V1CDSE
199500940808     C                   ENDIF
199600940805     C* CONSEGNA PARTICOLARE 1
199700940805     C                   MOVEL     BLPTC1        V3CTC1
199800940805     C                   MOVEL     *BLANKS       V3DTC1
199900940805     C     V3CTC1        IFNE      *BLANKS
200000940804     C                   MOVEL     '1P'          KCOD
200100940804     C                   MOVEL     *BLANKS       KKEY
200200940805     C                   MOVEL     BLPTC1        KKEY
200300940809     C     KTAB1         CHAIN     TABEL00F                           99
200400940805     C  N99              MOVEL     TBLUNI        V3DTC1
200500940805     C                   ENDIF
200600940805     C* CONSEGNA PARTICOLARE 2
200700940805     C                   MOVE      BLPTC2        V3CTC2
200800940805     C                   MOVEL     *BLANKS       V3DTC2
200900940805     C     V3CTC2        IFNE      *BLANKS
201000940805     C                   MOVEL     '1P'          KCOD
201100940805     C                   MOVEL     *BLANKS       KKEY
201200940805     C                   MOVEL     BLPTC2        KKEY
201300940809     C     KTAB1         CHAIN     TABEL00F                           99
201400940805     C  N99              MOVEL     TBLUNI        V3DTC2
201500940805     C                   ENDIF
201600910118     C* CODICE PADRONCINO
201700940808     C                   MOVEL     BLPPDR        V1CPDR
201800021203     C                   movel     BlpPdr        kpdpdr
201900021203     C                   move      'A'           kpdtip
202000021203     C     kfapd         chain     fiapd01L                           31
202100940808     C  N31              MOVEL     APDRSC        V1DPDR
202200940808     C   31              MOVEL     *BLANKS       V1DPDR
202300910118     C*
202400900511     C                   MOVEL     BLPSOP        VIDSOP
202500930614     C*
202600051124     c* Decodifico consegna richiesta
202700940808     C                   MOVEL     *BLANKS       V2CTCR
202800051124     c                   clear                   v2chcr
202900051124     c                   clear                   v2cdcr
203000051124     c                   if        blpdcr>0 or blphcr>0
203100051124     c                   clear                   fnlv80ds
203200051124     c                   eval      ilv80aas=blpaas
203300051124     c                   eval      ilv80lnp=blplnp
203400051124     c                   eval      ilv80nrs=blpnrs
203500051124     c                   eval      ilv80nsp=blpnsp
203600051124     c                   eval      ilv80mgs=blpmgs
203700051124     c                   eval      ilv80tc1=blptc1
203800051124     c                   eval      ilv80tc2=blptc2
203900051124     c                   eval      ilv80dcr=blpdcr
204000051124     c                   eval      ilv80hcr=blphcr
204100051124     c                   eval      ilv80tcr=blptcr
204200051124     c                   call      'FNLV80R'
204300051124     c                   parm                    kpjba
204400051124     c                   parm                    fnlv80ds
204500051124     c
204600051124     c                   eval      v2cdcr=olv80dcr8
204700051124     c                   eval      v2ctcr=olv80tcrd
204800051124     C                   MOVEL     blpHCR        V2cHCR
204900051124     c                   endif
205000940906     C*-------------------------------------------------------------------------
205100940906     C*
205200940906     C* L E G A M E   B O L L A
205300940906     C                   EXSR      LEGBOL
205400940808     C*-------------------------------------------------------------------------
205500050714     c* Dei seguenti dati,
205600050714     c* Prevalgono quelli di ARB se c'e' e se non presenti legami bolla
205700050714     C* DATA ENTRATA AL TRANSITO
205800050714     c                   if        wesarb='S' and not *in07
205900050714     c                   eval      wdet=arbdet
206000050714     c                   eval      wdut=arbdut
206100050714     c                   else
206200050714     c                   eval      wdet=blpdet
206300050714     c                   eval      wdut=blpdut
206400050714     c                   endif
206500050714     c
206600050714     C     wdet          IFGT      *ZERO
206700050714     C                   Z-ADD     wdet          G02INV
206800050714     C                   Z-ADD     *ZEROS        G02DAT
206900050714     C                   MOVEL     '3'           G02ERR
207000050714     C                   CALL      'XSRDA8'
207100050714     C                   PARM                    WLBDAT
207200050714     C                   Z-ADD     G02DAT        V1CDET
207300050714     C                   ENDIF
207400050714     C* DATA USCITA DAL TRANSITO
207500050714     C     wdut          IFGT      *ZERO
207600050714     C                   Z-ADD     wdut          G02INV
207700050714     C                   Z-ADD     *ZEROS        G02DAT
207800050714     C                   MOVEL     '3'           G02ERR
207900050714     C                   CALL      'XSRDA8'
208000050714     C                   PARM                    WLBDAT
208100050714     C                   Z-ADD     G02DAT        V1CDUT
208200050714     C                   ENDIF
208300050714     c* Mancata consegna
208400050714     c                   if        wesarb='S' and not *in07
208500050714     c                   eval      wtmc=arbtmc
208600050714     c                   else
208700050714     c                   eval      wtmc=blptmc
208800050714     c                   endif
208900050714     c
209000050714     C                   MOVEL     wtmc          V3CTMC
209100050714     C                   MOVEL     *BLANKS       V3DTMC
209200050714     C     wtmc          IFNE      ' '
209300050714     C                   MOVEL     '2A'          KCOD
209400050714     C                   MOVEL     *BLANKS       KKEY
209500050714     C                   MOVEL     wtmc          KKEY
209600050714     C     KTAB1         CHAIN     TABEL00F                           99
209700050714     C  N99              MOVEL     TBLUNI        V3DTMC
209800050714     C                   ENDIF
209900050714     c* Numero e data distinta consegna
210000050714     c                   if        wesarb='S'  and not *in07
210100050714     c                   eval      wndc=arbndc
210200050714     c                   eval      wddc=arbddc
210300050714     c                   else
210400050714     c                   eval      wndc=blpndc
210500050714     c                   eval      wddc=blpddc
210600050714     c                   endif
210700050714     C*
210800050714     C                   MOVEL     wndc          V3CNDC
210900050714     C                   Z-ADD     wddc          G02INV
211000050714     C                   Z-ADD     *ZEROS        G02DAT
211100050714     C                   MOVEL     '3'           G02ERR
211200050714     C                   CALL      'XSRDA8'
211300050714     C                   PARM                    WLBDAT
211400050714     C                   Z-ADD     G02DAT        V3CDDC
211500050714     C* DATA ARRIVO MERCE ALL FILIALE DI ARRIVO
211600050714     c                   clear                   wdam
211700050714     c                   if        wesarb='S' and not *in07
211800050714     c                   eval      wdam=arbdam
211900050714     c                   else
212000050714     c                   eval      wdam=blpdam
212100050714     c                   endif
212200050714     c
212300050714     C     wdam          IFGT      *ZERO
212400050714     C                   Z-ADD     wdam          G02INV
212500050714     C                   Z-ADD     *ZEROS        G02DAT
212600050714     C                   MOVEL     '3'           G02ERR
212700050714     C                   CALL      'XSRDA8'
212800050714     C                   PARM                    WLBDAT
212900050714     C                   Z-ADD     G02DAT        V1CDAM
213000050714     C                   ENDIF
213100050714     c
213200050714     C* DATA CONSEGNA MERCE REALE
213300050714     C                   Z-ADD     *ZERO         V1CDCM
213400050714     c                   if        wesarb='S' and not *in07
213500050714     c                   eval      wdcm=arbdcm
213600050714     c                   eval      wdce=arbdce
213700050714     c                   eval      wnrc=arbnrc
213800050714     c                   eval      wdcp=arbdcp
213900050714     c                   eval      wcca=arbcca
214000050714     c                   eval      whmc=arbhmc
214100050714     c                   else
214200050714     c                   eval      wdcm=blpdcm
214300050714     c                   eval      wdce=blpdce
214400050714     c                   eval      wnrc=blpnrc
214500050714     c                   eval      wdcp=blpdcp
214600050714     c                   eval      wcca=blpcca
214700050714     c                   eval      whmc=blphmc
214800050714     c                   endif
214900050714     c
215000050714     C     wdcm          IFGT      *ZERO
215100050714     C                   SETON                                        98
215200050714     C                   Z-ADD     wdcm          G02INV
215300050714     C                   Z-ADD     *ZEROS        G02DAT
215400050714     C                   MOVEL     '3'           G02ERR
215500050714     C                   CALL      'XSRDA8'
215600050714     C                   PARM                    WLBDAT
215700050714     C                   Z-ADD     G02DAT        V1CDCM
215800050714     C                   MOVEL     whmc          V1CHMC
215900050714     C                   ENDIF
216000050714     C* DATA CONSEGNA PREVISTA ESTERNA E ORE DI RITARDO
216100050714     C                   Z-ADD     0             V1CDCE
216200050714     C                   Z-ADD     0             V1CNRC
216300050714     C     wdce          IFEQ      0
216400050714     C                   SETON                                        94
216500050714     C                   ELSE
216600050714     C                   Z-ADD     wdce          G02INV
216700050714     C                   Z-ADD     *ZEROS        G02DAT
216800050714     C                   MOVEL     '3'           G02ERR
216900050714     C                   CALL      'XSRDA8'
217000050714     C                   PARM                    WLBDAT
217100050714     C                   Z-ADD     G02DAT        V1CDCE
217200050714     C*
217300050714     C                   Z-ADD     wnrc          V1CNRC
217400050714     C                   END
217500050714     C*
217600050714     C* DATA PRIMA CONSEGNA PARZIALE
217700050714     C     wdcp          IFGT      0
217800050714     C                   Z-ADD     wdcp          G02INV
217900050714     C                   Z-ADD     *ZEROS        G02DAT
218000050714     C                   MOVEL     '3'           G02ERR
218100050714     C                   CALL      'XSRDA8'
218200050714     C                   PARM                    WLBDAT
218300050714     C                   Z-ADD     G02DAT        V3CDCP
218400050714     C                   END
218500050714     C* CODICE CONSEGNA ANOMALA
218600050714     C                   MOVEL     wcca          V3CCCA
218700050714     C                   MOVE      *BLANKS       V3DCCA
218800050714     C                   MOVE      *BLANKS       V7DCCA
218900050714     C     wcca          IFNE      ' '
219000050714     C                   MOVEL     '7O'          KCOD
219100050714     C                   MOVEL     *BLANKS       KKEY
219200050714     C                   MOVEL     wcca          KKEY
219300050714     C     KTAB1         CHAIN     TABEL00F                           99
219400050714     C  N99              MOVEL     TBLUNI        V3DCCA
219500050714     C  N99              MOVEL     TBLUNI        V7DCCA
219600050714     C                   END
219700050714     c*******
219800991022      *   SE ESISTE AGGANCIO  2 °   B O L L A
219900991022      *
220000920409     C     §3ATB2        IFNE      '  '
220100991022      *
220200991022     c                   eval      KTRC = '2'
220300991022     C     KAR6          CHAIN     FIAR6000
220400991022      *
220500991022     C                   IF        %FOUND
220600991022     C                   eval        *IN06 = *ON
220700991022     C                   eval      VI2fiv = AR6fiv
220800991022     C                   eval      VI2nft = AR6nft
220900991022     C                   eval      VI2cei = AR6cei
221000991022      *
221100991022     C                   eval      VI2DIV = AR6div
221200991022     C                   eval      WDIV = AR6div
221300991022     C                   exsr      CTR_VALUTA
221400991022      *
221500991027     c                   if        AR6fim = 'P'
221600991027     c                   eval      VI2fim = 'PARTENZA'
221700040116     c                   eval      VI2fi2 = 'Immesse in'
221800991022     c                   endif
221900991022      * Varia G
222000040116     c**!!!              clear                   W0133
222100040116     c**!!!              SELECT
222200040116     c**!!!              WHEN        AR6sv1 = §$2vrg
222300040116     C**!!!              eval      VI2sv1 = AR6sv1
222400040116     c**!!!              z-add     AR6va1        W0133
222500040116     c**!!!              WHEN        AR6sv2 = §$2vrg
222600040116     C**!!!              eval      VI2sv1 = AR6sv2
222700040116     c**!!!              z-add     AR6va2        W0133
222800040116     c**!!!              WHEN        AR6sv3 = §$2vrg
222900040116     C**!!!              eval      VI2sv1 = AR6sv3
223000040116     c**!!!              z-add     AR6va3        W0133
223100040116     c**!!!              ENDSL
223200991022      *
223300040116     c**!!!              if        W0133 > *zeros
223400040116     c**!!!              eval      Wtstdec = §cvfdc
223500040116     c**!!!              exsr      EDTimporti
223600040116     c**!!!              movel     IMPeditato    VI2va1
223700040116     c**!!!              endif
223800991022      * IVA
223900040116     c**!!!              clear                   W0133
224000040116     c**!!!              SELECT
224100040116     c**!!!              WHEN        AR6sv1 = §$2iva
224200040116     c**!!!              z-add     AR6va1        W0133
224300040116     c**!!!              WHEN        AR6sv2 = §$2iva
224400040116     c**!!!              z-add     AR6va2        W0133
224500040116     c**!!!              WHEN        AR6sv3 = §$2iva
224600040116     c**!!!              z-add     AR6va3        W0133
224700040116     c**!!!              ENDSL
224800991022      *
224900040116     c**!!!              IF        W0133 > *zeros
225000040116     c**!!!              eval      *in11 = *off
225100040116     c**!!!              eval      Wtstdec = §cvfdc
225200040116     c**!!!              exsr      EDTimporti
225300040116     c**!!!              movel     IMPeditato    VI2iva
225400040116     c**!!!              ELSE
225500991123      * BOLLO se non ho iva
225600040116     c**!!!              SELECT
225700040116     c**!!!              WHEN        AR6sv1 = §$2bol
225800040116     c**!!!              z-add     AR6va1        W0133
225900040116     c**!!!              WHEN        AR6sv2 = §$2bol
226000040116     c**!!!              z-add     AR6va2        W0133
226100040116     c**!!!              WHEN        AR6sv3 = §$2bol
226200040116     c**!!!              z-add     AR6va3        W0133
226300040116     c**!!!              ENDSL
226400991123      *
226500040116     c**!!!              IF        W0133 > *zeros
226600040116     c**!!!              eval      *in11 = *on
226700040116     c**!!!              eval      Wtstdec = §cvfdc
226800040116     c**!!!              exsr      EDTimporti
226900040116     c**!!!              movel     IMPeditato    VI2iva
227000040116     c**!!!              ENDIF
227100991123      *
227200040116     c**!!!              ENDIF
227300040116
227400040116      * Carico le varie
227500040116     c                   Movel     Ar6Sv1        wsvn
227600040116     c                   Z-add     Ar6Va1        wvan
227700040116     c                   ExSr      Car_Varie1
227800040116
227900040116     c                   Movel     Ar6Sv2        wsvn
228000040116     c                   Z-add     Ar6Va2        wvan
228100040116     c                   ExSr      Car_Varie1
228200040116
228300040116     c                   Movel     Ar6Sv3        wsvn
228400040116     c                   Z-add     Ar6Va3        wvan
228500040116     c                   ExSr      Car_Varie1
228600040116
228700040116     c     Kar6          Setll     Fiar7000
228800040116     c                   Do        *Hival
228900040116     c     Kar6          Reade     Fiar7000
229000040116     c                   If        %Eof(Fiar701l)
229100040116     c                   Leave
229200040116     c                   EndIf
229300040116     c                   Movel     Ar7Svn        wsvn
229400040116     c                   Z-add     Ar7Van        wvan
229500040116     c                   ExSr      Car_Varie1
229600040116     c                   EndDo
229700040116
229800991025      * TOTALE FATTURA
229900991025     c                   clear                   W0133
230000991025     c                   IF          AR6ift > *zeros
230100991025     c                   z-add     AR6ift        W0133
230200991025     c                   eval      Wtstdec = §cvfdc
230300991025     c                   exsr      EDTimporti
230400991025     c                   movel     IMPeditato    VI2ift
230500991025     c                   endif
230600991022      *
230700991022     c                   If         AR6dft > *zeros
230800991022     C     *iso          movel     AR6DFT        DATA_EUR
230900991022     C     *eur          movel     data_eur      W0080
231000991022     C                   eval      VI2dft = %EDITC(W0080:'Y')
231100991022     c                   Endif
231200991022      *  decodifica esenzione IVA
231300991022     C                   if        VI2CEI <> *blanks
231400940808     C                   MOVEL     'EI'          KCOD
231500991022     C                   MOVEL(P)  VI2CEI        KKEY
231600940809     C     KTAB1         CHAIN     TABEL00F                           31
231700991022     C  N31              MOVEL     TBLUNI        VI2DCE
231800940808     C                   ENDIF
231900991022      *
232000991022     c                   eval      VI2ksc = AR6ksc
232100991022     c                   eval      VI2dks = BLPrsd
232200991022     c                   eval      VI2ctr = AR6ctr
232300991022      *
232400991022     C                   eval      KCOD = 'TR'
232500991022     C                   movel     VI2CTR        UNOCTR
232600991022     C                   eval      KKEY = unoctr
232700991022     C     KTAB1         CHAIN     TABEL00F                           31
232800991022     C                   movel     TBLUNI        VI2dct
232900991022      *
233000991022     C                   ENDIF
233100940808     C                   ENDIF
233200951219     C*-------------------------------------------------------------------------
233300951219     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
233400951219     C*
233500951219     C* PULISCO IL SFL
233600951219     C                   SETON                                        26
233700951219     C                   WRITE     LS05C03
233800951219     C                   SETOFF                                       26
233900951219     C*
234000060216     C* CHIAVE PER ACCEDERE AL FILE Fiar401L
234100951219     C                   MOVEL     BLPAAS        KAAS
234200951219     C                   MOVEL     BLPLNP        KLNP
234300951219     C                   MOVEL     BLPNRS        KNRS
234400951219     C                   MOVEL     BLPNSP        KNSP
234500951219     C*
234600951219     C                   Z-ADD     0             NRR3              4 0
234700951219     C                   MOVEL     '7K'          KCOD
234800951219     C*
234900060216     C     Kar42         SETLL     Fiar401L
235000060216     C     Kar42         READE     Fiar401L                               99
235100951219     C     *IN99         DOWEQ     *OFF
235200951219     C*
235300060216     C                   MOVEL(P)  ar4TRC        KKEY
235400951219     C     KTAB1         CHAIN     TABEL                              99
235500951219     C*
235600951219     C     *IN99         IFEQ      *OFF
235700951219     C     TBLFLG        ANDEQ     ' '
235800951219     C                   MOVEL     TBLUNI        DS7K
235900951219     C     §7KVIS        IFEQ      'S'
236000951219     C                   MOVEL     §7KDES        VS3DES
236100060216     C                   MOVEL     ar4NOT        VS3NOT
236200951219     C                   ADD       1             NRR3
236300951219     C                   WRITE     LS05S03
236400951219     C                   ENDIF
236500951219     C                   ENDIF
236600951219     C*
236700060216     C     Kar42         READE     Fiar401L                               99
236800951219     C                   ENDDO
236900951219     C*
237000951219     C* SE NON HO CARICATO NIENTE SCRIVO UGUALMENTE UN RECORD VUOTO
237100951219     C*  NEL SFL PER EVITARE CHE IL PGM DIA ERRORE DI SESSIONE O UNITA'
237200951219     C     NRR3          IFEQ      0
237300951219     C                   ADD       1             NRR3
237400951219     C                   WRITE     LS05S03
237500951219     C                   ENDIF
237600951219     C*
237700940808     C*-------------------------------------------------------------------------
237800930526     C* 95 ON - ESITONO EVENTI DA INTERROGARE
237900111020     C**!!!PA0CLI        IFNE      'S'
238000991022     C     KBLP3         SETLL     FNEVB000                               95
238100111020     C**!!!              ENDIF
238200940805     C*-------------------------------------------------------------------------
238300110705     C                   clear                   SAVNR8
238400110705     C                   eval      *in04 = *off
238500110705     C                   eval      *in84 = *off
238600110705     c
238700110705     c                   clear                   v8car4ksc
238800110705     c                   clear                   v8dar4ksc
238900110705     c                   clear                   v8car4ctr
239000110705     C* Verifico se presete cod cliente e cod tariffa di tassazione da bollettaz
239100110705     C                   MOVEL     'A'           KTRC
239200110705     C     Kar41         CHAIN     Fiar401L                           99
239300110705     C     *IN99         IFEQ      *OFF
239400110705     c                   movel     ar4not        dsbl4a
239500110705     c                   if        §b4aks>0
239600110705     C                   eval      *in84 = *on
239700110705     C                   eval      *in04 = *on
239800110705     c                   eval      v8car4ksc=§b4aks
239900110705     c                   eval      i69kac = v8car4ksc
240000110705     c                   exsr      CTR_ANAGRA
240100110705     c                   eval      v8dar4ksc = acorag
240200110705     c                   if        §b4act<>*blanks
240300110705     c                   movel     §b4act        v8car4ctr
240400110705     c                   endif
240500110705     c                   endif
240600110705     c                   endif
240700110705     c
240800991022      * PER I DATI SULLA TASSAZIONE, CERCO SU FIAR600F.
240900991022      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SOLO SE IMPOSTATO ALMENO UN CAMPO
241000991022     C                   eval      Ktrc = '1'
241100991022     C     KAR6          CHAIN     FIAR601L
241200110705      * CLIENTE E TARIFFA FATTURAZIONE
241300110705     c                   IF        %found(fiar601l)
241400110705     c
241500110705     c                   IF        *IN33 = *ON
241600110705     c                   eval      v8cksc = blpksc
241700110705     c                   eval      v8cctr = blpctr
241800110705     c                   eval      v8dksc = blprsm
241900110705     c                   ELSE
242000110705     c                   eval      v8cksc = AR6ksc
242100110705     c                   eval      v8cctr = AR6ctr
242200110705     c                   move      v8cksc        W0040
242300110705     c                   if        W0040 = 9999
242400110705     c                   eval      v8dksc = blprsd
242500110705     c                   else
242600110705     c                   eval      i69kac = v8cksc
242700110705     c                   exsr      CTR_ANAGRA
242800110705     c                   eval      v8dksc = acorag
242900110705     c                   endif
243000110705     c                   ENDIF
243100110705     c
243200110705      *   decodifica tariffa
243300110705     C                   eval      KCOD = 'TR'
243400110705     C                   MOVEL     V2CCTR        UNOCTR            1
243500110705     C                   MOVEL(P)  UNOCTR        KKEY
243600110705     C     KTAB1         CHAIN     TABEL00F                           31
243700110705     C                   MOVEL     TBLUNI        V8DCTR
243800110705     c
243900110705     c                   ENDIF
244000991022      *
244100991022     C                   IF        %FOUND  and
244200991022     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
244300991022     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
244400991022     C                              AR6sv3 <> *blanks)
244500991022      *
244600991022     C                   eval      *in04 = *on
244700991022      *
244800991025     C                   eval      V8CDIV = AR6div
244900991022     C                   eval      WDIV = AR6div
245000991022     C                   exsr      CTR_VALUTA
245100991022      *
245200991022     c                   IF          ar6por > *zeros
245300991022     c                   z-add     AR6por        W0133
245400991022     c                   eval      Wtstdec = §cvfdc
245500991022     c                   exsr      EDTimporti
245600991022     c                   movel     IMPeditato    V8Cpor
245700991022     c                   ENDIF
245800991022      *
245900991022     c                   IF          ar6imv > *zeros
246000991022     c                   z-add     AR6imv        W0133
246100991022     c                   eval      Wtstdec = §cvfdc
246200991022     c                   exsr      EDTimporti
246300991022     c                   movel     IMPeditato    V8Cimv
246400991022     c                   ENDIF
246500991022      *
246600991022     c                   IF          ar6ift > *zeros
246700991022     c                   z-add     AR6ift        W0133
246800991022     c                   eval      Wtstdec = §cvfdc
246900991022     c                   exsr      EDTimporti
247000991022     c                   movel     IMPeditato    V8Cift
247100991022     c                   ENDIF
247200991022      *
247300991027     c                   IF        AR6fim = 'P'
247400991022     c                   eval      V8Dfim = 'PARTENZA'
247500991022    2C                   ENDIF
247600991022      *
247700991022     C                   MOVEL     AR6CEI        V8CCEI
247800991022      *
247900991022      * SE C'E', DECODIFICO L'ESENZIONE IVA
248000991022     C                   clear                   V8DCEI
248100991022     C                   IF        AR6CEI <>  *BLANKS
248200991022     C                   MOVEL     'EI'          KCOD
248300991022     C                   MOVEL(P)  AR6CEI        KKEY
248400991022     C     KTAB1         CHAIN     TABEL00F                           99
248500991022     C  N99              MOVEL     TBLUNI        V8DCEI
248600991022    2C                   ENDIF
248700991022      *
248800991022      * DATA FATTURA DA GIRARE
248900991022     C                   IF         ar6dft > *zeros
249000991022     C     *iso          movel     AR6DFT        DATA_EUR
249100991022     C     *eur          movel     data_eur      W0080
249200991022     C                   eval      V8Cdft = %EDITC(W0080:'Y')
249300991022     C                   ENDIF
249400991022     C                   MOVEL     ar6NFT        V8CNFT
249500991022     C                   MOVEL     ar6FIV        V8CFIV
249600991022      *
249700991022      * CERCO LE VARIE
249800991022     C                   eval      *in20 = *off
249900991022     C                   eval      *in21 = *off
250000991022     C                   WRITE     LS05C08
250100991022     C                   eval      *in20 = *on
250200991022     C                   eval      *in21 = *on
250300991022      *
250400991022     c                   clear                   nrr8
250500991022      *
250600991022     C                   MOVEL     ar6SV1        WSVN
250700991022     C                   MOVEL     ar6VA1        WVAN
250800991022     c                   exsr      car_varie
250900991022      *
251000991022     C                   MOVEL     ar6SV2        WSVN
251100991022     C                   MOVEL     ar6VA2        WVAN
251200991022     c                   exsr      car_varie
251300991022      *
251400991022     C                   MOVEL     ar6SV3        WSVN
251500991022     C                   MOVEL     ar6VA3        WVAN
251600991022     c                   exsr      car_varie
251700991022      *
251800991022      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
251900991022     C                   if          ar6SV3 <> *blanks
252000991022     C     KAR6          SETLL     FIAR7000
252100991022     C     KAR6          READE     FIAR7000
252200991022     C                   DOW       %EOF = *OFF
252300991022     C                   MOVEL     AR7SVN        WSVN
252400991022     C                   MOVEL     AR7VAN        WVAN
252500991022     c                   exsr      car_varie
252600991022     C     KAR6          READE     FIAR7000
252700991022     C                   ENDDO
252800991022     C                   endif
252900110705     C                   ENDIF
253000991022      *
253100991022      * Se non ho caricato record ne carico uno vuoto
253200110705     c                   IF        NRR8 = *ZEROS  and *in04
253300991022     C                   clear                   V8CDES
253400991022     C                   clear                   V8CVA1
253500991022     C                   clear                   V8CSV1
253600991022     C                   add       1             NRR8
253700991022     C                   WRITE     LS05S08
253800991022     c                   ENDIF
253900991022      *
254000991022     C                   Z-ADD     NRR8          SAVNR8
254100991022     C                   Z-ADD     1             NRR8
254200991022      *
254300991022      *-------------------------------------------------------------------------
254400991022      * DECODIFICO IL CODICE TASSAZIONE, SE C'E'
254500991022     C                   clear                   V8DCTS
254600991022     C                   if         V8CCTS <> *blanks
254700991022     C                   MOVEL     'CT'          KCOD
254800991022     C                   MOVEL(P)  V8CCTS        KKEY
254900991022     C     KTAB1         CHAIN     TABEL00F                           99
255000991022     C  N99              MOVEL     TBLUNI        V8DCTS
255100991022     C                   endif
255200940805     C*-------------------------------------------------------------------------
255300940805     C* PRELEVO LE NOTE DAL FILE APPOSITO
255400940805     C                   MOVEL     BLPAAS        KAAS
255500940805     C                   MOVEL     BLPLNP        KLNP
255600940805     C                   MOVEL     BLPNRS        KNRS
255700940805     C                   MOVEL     BLPNSP        KNSP
255800940805     C                   MOVEL     '8'           KTRC
255900060216     C     Kar41         CHAIN     Fiar401L                           99
256000060216     C  N99              MOVEL     ar4NOT        V2CNO1
256100940805     C   99              MOVEL     *BLANKS       V2CNO1
256200940805     C                   MOVEL     '9'           KTRC
256300060216     C     Kar41         CHAIN     Fiar401L                           99
256400060216     C  N99              MOVEL     ar4NOT        V2CNO2
256500940805     C   99              MOVEL     *BLANKS       V2CNO2
256600991022      *
256700991022      *-------------------------------------------------------------------------
256800051115      * IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU Fiar900F
256900991022      * CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
257000940805     C                   MOVEL     *BLANKS       V2CTIC
257100940805     C                   MOVEL     *ZERO         V2CCAS
257200940805     C                   MOVEL     *BLANKS       V2CVCA
257300940805     C                   MOVEL     *BLANKS       V2CGCA
257400091214     c                   setoff                                       43
257500940923     C                   MOVEL     *BLANKS       V2IGCA
257600940805     C     §3AFCA        IFGT      *ZERO
257700051115     C     KBLP3         CHAIN     Fiar901L                           99
257800940805     C  N99              DO
257900051115     C                   MOVEL     ar9TIC        V2CTIC
258000051115     C                   MOVEL     ar9CAS        V2CCAS
258100051115     C                   MOVEL     ar9VCA        V2CVCA
258200051115     C                   MOVEL     ar9GCA        V2CGCA
258300091214     c                   seton                                        43
258400940805     C                   ENDDO
258500940805     C                   ENDIF
258600991022      * SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
258700940923     C     V2CGCA        COMP      *BLANKS                            1616
258800021218      *
258900021218      * Riempio il campone in fondo alla videata con i tasti scelti
259000021218     C                   exsr      Ges_TastiFun
259100021218     c                   eval      FD07 = FDxx
259200021218     c                   eval      F24D07 = F24Dxx
259300991022      *
259400940923     C     ENDBLP        ENDSR
259500030703      *-------------------------------------------------------------------------
259600030703      *  Decoditifa tipo bancali
259700030703      *-------------------------------------------------------------------------
259800030703     c     Sr_DecTba     BegSr
259900030703     c                   Clear                   Tibs02Ds
260000030703     c                   Clear                   dTba
260100030703     c                   Movel     'C'           T02Mod
260200030703     c                   Movel     Knsif         T02Sif
260300030703     c                   Movel     'TBA'         T02Cod
260400030703     c                   Movel(p)  wtba          T02Ke1
260500030703     c                   Call      'TIBS02R'
260600030703     c                   Parm                    Kpjba
260700030703     c                   Parm                    Tibs02Ds
260800030703If  3c                   If        T02Err = *Blanks
260900030703     c                   Movel     T02Uni        dTba
261000030703    3c                   EndIf
261100030703     c                   Eval      wdestba = §TbaDesc
261200030703     c                   EndSr
261300021218      *-------------------------------------------------------------------------
261400021218      *  Imposto descrizione tasti funzione
261500021218      *-------------------------------------------------------------------------
261600021218     c     Ges_TastiFun  BEGSR
261700021218      *
261800021218      * per emettere i tasti senza condizionamenti ..
261900021218     c                   setoff                                       25
262000021218      *
262100021218      * Conta i caratteri riempiti dalle RigaTf1
262200021218     c                   z-add     0             $z                3 0
262300021218      *
262400021218      * Conta i caratteri riempiti dalle RigaTf2
262500021218     c                   z-add     0             $k                3 0
262600021218      *
262700021218      * Conta i caratteri riempiti dalle RigaTf3
262800021218     c                   z-add     0             $j                3 0
262900021218      *
263000021218      * Conta la posizione del campone da cui partire per impostare
263100021218      * la descrizione del tasto funzione
263200021218     c                   z-add     0             $y                2 0
263300021218      *
263400021218      * Stringhe che contengono le descrizioni dei tasti funzione
263500021218     c                   reset                   RigaTf1
263600021218     c                   reset                   RigaTf2
263700021218     c                   reset                   RigaTf3
263800021218      *
263900021218      * F13
264000021218     c                   IF        *in04 = *on
264100021218     c                   reset                   $Tf
264200021218     c                   movea     Tf13          $Tf
264300021218     c                   Exsr      Rie_$Tf
264400021218     C                   ENDIF
264500021218      *
264600021218      * F04
264700021218     c                   IF        *in06 = *on
264800021218     c                   reset                   $Tf
264900021218     c                   movea     Tf04          $Tf
265000021218     c                   Exsr      Rie_$Tf
265100021218     C                   ENDIF
265200040220      *
265300040220      *?F16?
265400040220     c                   IF        *in28 = *on
265500040220     c                   reset                   $Tf
265600040220     c                   movea     Tf16          $Tf
265700040220     c                   exsr      Rie_$Tf
265800040220     c                   ENDIF
265900021218      *
266000021218      * F05
266100021218     c                   IF        *in95 = *on
266200021218     c                   reset                   $Tf
266300021218     c                   movea     Tf05          $Tf
266400021218     c                   Exsr      Rie_$Tf
266500021218     C                   ENDIF
266600021218      *
266700021218      * F10
266800021218     c                   IF        *in10 = *on
266900021218     c                   reset                   $Tf
267000021218     c                   movea     Tf10          $Tf
267100021218     c                   Exsr      Rie_$Tf
267200021218     c                   ENDIF
267300021218      *
267400021218      * F14
267500021218     c                   IF        *in03 = *on
267600021218     c                   reset                   $Tf
267700021218     c                   movea     Tf14          $Tf
267800021218     c                   Exsr      Rie_$Tf
267900021218     c                   ENDIF
268000051201      *
268100051201      * F22
268200051201     c                   IF        *in24 = *on
268300051201     c                   reset                   $Tf
268400051201     c                   movea     Tf22          $Tf
268500051201     c                   Exsr      Rie_$Tf
268600051201     c                   ENDIF
268700021218      *
268800021218      * F02
268900021218     c                   IF        *in12 = *on
269000021218     c                   reset                   $Tf
269100021218     c                   movea     Tf02          $Tf
269200021218     c                   Exsr      Rie_$Tf
269300021218     c                   ENDIF
269400021218      *
269500021218      * F11
269600021218     c                   IF        *in77 = *on
269700021218     c                   reset                   $Tf
269800021218     c                   movea     Tf11          $Tf
269900021218     c                   Exsr      Rie_$Tf
270000021218     c                   ENDIF
270100021218      *
270200021218      * F07
270300021218     c                   IF        *in22 = *on and *in05 = *off
270400021218     c                   reset                   $Tf
270500021218     c                   movea     Tf07          $Tf
270600021218     c                   Exsr      Rie_$Tf
270700021218     c                   ENDIF
270800021218      *
270900021218      * F08
271000021218     c                   IF        *in55 = *on
271100021218     c                   reset                   $Tf
271200021218     c                   movea     Tf08          $Tf
271300021218     c                   Exsr      Rie_$Tf
271400021218     c                   ENDIF
271500130912
271600130912      * F09 --> Orari servizi non condizionato
271700131007     c                   IF        *in25 = *off and wabif9='S'
271800130930     c                   reset                   $Tf
271900130930     c                   movea     Tf09          $Tf
272000130930     c                   Exsr      Rie_$Tf
272100130930     c                   ENDIF
272200021218      *
272300021218      * F20
272400021218     c                   IF        *in22 = *off
272500021218     c                   reset                   $Tf
272600021218     c                   movea     Tf20          $Tf
272700021218     c                   Exsr      Rie_$Tf
272800021218     c                   ENDIF
272900021218      *
273000021218      * F21 --> non condizionato
273100021218     c                   IF        *in25 = *off
273200021218     c                   reset                   $Tf
273300021218     c                   movea     Tf21          $Tf
273400021218     c                   Exsr      Rie_$Tf
273500021218     c                   ENDIF
273600040220      *
273700040220      * F19
273800040220     c                   IF        *in22 = *off
273900040220     c                   reset                   $Tf
274000040220     c                   movea     Tf19          $Tf
274100040220     c                   Exsr      Rie_$Tf
274200040220     c                   ENDIF
274300040220      *
274400040220      *?F01?
274500040220     c                   IF        *in27 = *on
274600040220     c                   reset                   $Tf
274700040220     c                   movea     Tf01          $Tf
274800040220     c                   exsr      Rie_$Tf
274900040220     c                   ENDIF
275000021218      *
275100021218      * F12 --> non condizionato
275200021218     c                   IF        *in25 = *off
275300021218     c                   reset                   $Tf
275400021218     c                   movea     Tf12          $Tf
275500021218     c                   Exsr      Rie_$Tf
275600021218     c                   ENDIF
275700021218      *
275800021218      * Pulisco le stringa
275900021218     c                   z-add     1             $h
276000030121     c     1             do        62            $h                2 0
276100021218     C                   if        RigaTf1($h) = '#'
276200021218     c                   movel     *blanks       RigaTf1($h)
276300021218     c                   endif
276400021218     c                   enddo
276500021218     c                   z-add     1             $h
276600030121     c     1             do        62            $h                2 0
276700021218     C                   if        RigaTf2($h) = '#'
276800021218     c                   movel     *blanks       RigaTf2($h)
276900021218     c                   endif
277000021218     c                   enddo
277100021218     c                   z-add     1             $h
277200030121     c     1             do        62            $h                2 0
277300021218     C                   if        RigaTf3($h) = '#'
277400021218     c                   movel     *blanks       RigaTf3($h)
277500021218     c                   endif
277600021218     c                   enddo
277700021218      *
277800021218      * Imposto la descrizione del tasto funzione F24 e segnalo
277900021218      * quante righe ho riempito e quale devo emettere
278000021218     c                   SELECT
278100021218      *
278200021218     c                   when      $k<>*zeros and $Y<>*zeros
278300021218     c                   eval      $loop = 3
278400021218     c                   z-add     3             $loop             1 0
278500021218     C                   movel(p)  CF2413        F24Dxx           16
278600021218      *
278700021218     c                   when      $k<>*zeros and $Y=*zeros
278800021218     c                   z-add     2             $loop             1 0
278900021218     C                   movel(p)  CF2412        F24Dxx
279000021218      *
279100021218     c                   Other
279200021218     c                   z-add     1             $loop             1 0
279300021218     C                   movel     *blanks       F24Dxx
279400021218     c                   ENDSL
279500021218      * Prima riga di tasti funzione
279600030121     c                   movea     RigaTF1       FDxx             62
279700021218     c                   z-add     1             $Rig              1 0
279800021218      *
279900021218     c                   endsr
280000021218      *-------------------------------------------------------------------------
280100021218      *  Riempio le stringhe con i campi funzione compattati
280200021218      *-------------------------------------------------------------------------
280300021218     c     Rie_$Tf       begsr
280400021218      *
280500021218     c                   z-add     1             $x                2 0
280600021218     c     '#'           lookup    $Tf($x)                                31
280700021218     c                   add       $x            $Z
280800030121     c                   IF        $Z <= 63
280900021218     c                   eval      $j = $Z - $x + 1
281000021218     c                   movea     $Tf           RigaTF1($j)
281100021218     c                   ELSE
281200021218     c                   add       $x            $K
281300030121     c                   IF        $K <= 63
281400021218     c                   eval      $j = $K - $x + 1
281500021218     c                   movea     $Tf           RigaTF2($j)
281600021218     c                   ELSE
281700021218     c                   add       $x            $Y
281800030121     c                   IF        $Y <= 63
281900021218     c                   eval      $j = $Y - $x + 1
282000021218     c                   movea     $Tf           RigaTF3($j)
282100021218     c                   ENDIF
282200021218     c                   ENDIF
282300021218     c                   ENDIF
282400021218      *
282500021218     c                   endsr
282600021218      *-------------------------------------------------------------------------
282700021218      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
282800021218      *-------------------------------------------------------------------------
282900021218     c     Ges_F24       begsr
283000021218      *
283100021218     c                   SELECT
283200021218      *
283300021218     c                   WHEN      $Loop = 2 and $Rig =2  or
283400021218     c                             $Loop = 3 and $Rig =3
283500021218     c                   movea     RigaTF1       FDxx
283600021218     c                   z-add     1             $Rig
283700021218      *
283800021218     c                   if        $loop = 2
283900021218     c                   eval      F24Dxx= Cf2412
284000021218     c                   else
284100021218     c                   eval      F24Dxx= Cf2413
284200021218     c                   endif
284300021218      *
284400021218     c                   WHEN      $Loop = 2 and $Rig =1 or
284500021218     c                             $Loop = 3 and $Rig =1
284600021218     c                   movea     RigaTF2       FDxx
284700021218     c                   z-add     2             $Rig
284800021218      *
284900021218     c                   if        $loop = 2
285000021218     c                   eval      F24Dxx= Cf2422
285100021218     c                   else
285200021218     c                   eval      F24Dxx= Cf2423
285300021218     c                   endif
285400021218      *
285500021218     c                   WHEN      $Loop = 3 and $Rig =2
285600021218     c                   movea     RigaTF3       FDxx
285700021218     c                   z-add     3             $Rig
285800021218     c                   eval      F24Dxx= Cf2433
285900021218     c                   ENDSL
286000021218      *
286100021218     c                   endsr
286200991022      *-------------------------------------------------------------------------
286300991022      *  CARICO SUBFILE VARIE
286400991022      *-------------------------------------------------------------------------
286500991022     c     CAR_VARIE     BEGSR
286600991022      *
286700991022     c                   SELECT
286800991022      *
286900991022     c                   WHEN       WSVN = *BLANKS
287000991022      *
287100991022     C                   WHEN       WSVN = §$2iva
287200991022     c                   z-add     Wvan          W0133
287300991022     c                   eval      Wtstdec = §cvfdc
287400991022     c                   exsr      EDTimporti
287500991025     c                   movel     IMPeditato    V8Civa
287600991022      *
287700991022     c                   OTHER
287800991022     c                   z-add     Wvan          W0133
287900991022     c                   eval      Wtstdec = §cvfdc
288000991022     c                   exsr      EDTimporti
288100991022     c                   movel     IMPeditato    V8Cva1
288200991022     c                   movel     WSVN          V8Csv1
288300991022     C                   Z-ADD     1             WA
288400991022     C     V8CSV1        LOOKUP    CC1(WA)                                99
288500020329     C     CC3(WA)       ifeq      'S'
288600020329      *
288700020329      * Verifico se si tratta di una bolla Fedex
288800021119     c                   if        LnaNtw = 'FED'
288900020329     C                   Z-ADD     1             WA
289000020329     C     V8CSV1        LOOKUP    CB1(WA)                                99
289100020329     C   99              MOVEL     CB2(WA)       V8CDES
289200020329     C  N99              clear                   V8CDES
289300020329     C                   else
289400991022     C   99              MOVEL     CC2(WA)       V8CDES
289500991022     C  N99              clear                   V8CDES
289600020329     C                   endif
289700020329     C                   else
289800020329     C   99              MOVEL     CC2(WA)       V8CDES
289900020329     C  N99              clear                   V8CDES
290000020329     C                   endif
290100991022     C                   add       1             NRR8
290200991022     C                   WRITE     LS05S08
290300991022      *
290400991022     c                   ENDSL
290500991022      *
290600991022     c                   ENDSR
290700040116      *-------------------------------------------------------------------------
290800040116      *  Carico le varie della seconda bolla
290900040116      *-------------------------------------------------------------------------
291000040116     c     Car_Varie1    BegSr
291100040116
291200040116s   1c                   Select
291300040116
291400040116     c                   When      wsvn = *Blanks
291500040116      * Iva
291600040116     c                   When      wsvn = §$2iva
291700040116     c                   z-add     wvan          w0133
291800040116     c                   Eval      Wtstdec = §cvfdc
291900040116     c                   ExSr      EDTimporti
292000040116     c                   Movel     IMPeditato    Vi2Iva
292100040116      * Bollo
292200040116     c                   When      wsvn = §$2bol
292300040116     c                   z-add     wvan          w0133
292400040116     c                   Eval      Wtstdec = §cvfdc
292500040116     c                   ExSr      EDTimporti
292600040116     c                   Movel     IMPeditato    Vi2Iva
292700040116      * Varia G - sempre per prima
292800040116     c                   When      wsvn = §$2vrg
292900040116     c                   Eval      Vi2Sv1 = wsvn
293000040116     c                   z-add     wvan          w0133
293100040116     c                   Eval      Wtstdec = §cvfdc
293200040116     c                   ExSr      EDTimporti
293300040116     c                   Movel     IMPeditato    Vi2Va1
293400040116      * Altre varie
293500040116   x1c                   Other
293600040116     c                   z-add     wvan          W0133
293700040116     c                   eval      Wtstdec = §cvfdc
293800040116     c                   exsr      EDTimporti
293900040116if  2c                   If        Vi2Sv2 = *Blanks
294000040116     c                   movel     IMPeditato    Vi2Va2
294100040116     c                   movel     wsvn          Vi2sv2
294200040116   x2c                   Else
294300040116if  3c                   If        Vi2Sv3 = *Blanks
294400040116     c                   movel     IMPeditato    Vi2Va3
294500040116     c                   movel     wsvn          Vi2sv3
294600040116   x3c                   Else
294700040116if  4c                   If        Vi2Sv4 = *Blanks
294800040116     c                   movel     IMPeditato    Vi2Va4
294900040116     c                   movel     wsvn          Vi2sv4
295000041130   x4c                   Else
295100041130if  5c                   If        Vi2Sv5 = *Blanks
295200041130     c                   movel     IMPeditato    Vi2Va5
295300041130     c                   movel     wsvn          Vi2sv5
295400041130   x5c                   Else
295500041130if  6c                   If        Vi2Sv6 = *Blanks
295600041130     c                   movel     IMPeditato    Vi2Va6
295700041130     c                   movel     wsvn          Vi2sv6
295800041130    6c                   EndIf
295900041130    5c                   EndIf
296000040116    4c                   EndIf
296100040116    3c                   EndIf
296200040116    2c                   EndIf
296300040116
296400040116    1c                   EndSl
296500040116
296600040116     c                   EndSr
296700991022      **********************************************************************
296800991022      * AGGANCIO TABELLA VALUTE
296900991022      **********************************************************************
297000991022     C     CTR_VALUTA    BEGSR
297100991022
297200991022     C                   MOVEL     'CV'          KCOD
297300991022     C                   MOVEL(P)  WDIV          KKEY
297400991022     C     KTAB1         CHAIN     TABEL00F                           31
297500991022     C  N31              MOVEL     TBLUNI        DSCV
297600991022
297700991022     C                   ENDSR
297800991022      **********************************************************************
297900991022      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
298000991022      **********************************************************************
298100991022     C     EDTIMPORTI    BEGSR
298200991022
298300991022      * ------------------------------------------------------------------------
298400991022      * W0133      contiene il numero da editare per il video
298500991022      * WTSTDEC    corrisponde a §CVfdc della tabella CV
298600991022      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
298700991022      * ------------------------------------------------------------------------
298800991022      *
298900991022     c                   clear                   IMPeditato
299000991022     c                   clear                   W017A
299100991022      *
299200991022     c                   if        Wtstdec = 'S'
299300991022     C                   eval      W017a = %EDITC(W0133:'2')
299400991022     c                   else
299500991022     c                   z-add     W0133         W0130
299600991022     C                   eval      W017a = %EDITC(W0130:'2')
299700991022     c                   endif
299800991022      *
299900991022     c                   move      W017a         IMPeditato
300000991022      *
300100991022     c                   ENDSR
300200991025      *****************************************************************
300300991025      *  AGGANCIO ANAGRAFICHE
300400991025      *****************************************************************
300500991025     C     CTR_ANAGRA    BEGSR
300600991025      *
300700991025     C                   clear                   DS_cnaco
300800991025     C                   clear                   DS_cnind
300900991025     C                   clear                   DS_cnclp
301000991025     C                   clear                   DS_fncls
301100991025      *
301200991025     C                   CALL      'TIBS69R'
301300991025     C                   PARM                    tibs69DS
301400991025     C                   PARM                    DS_cnaco
301500991025     C                   PARM                    DS_cnind
301600991025     C                   PARM                    DS_cnclp
301700991025     C                   PARM                    DS_fncls
301800991025      *
301900991025     C                   eval      wtibs69r = 'S'
302000991025      * per il 1° giro è inizializzata nelle specifiche "D"
302100991025     C                   clear                   TIBS69DS
302200991025      *
302300991025     C                   ENDSR
302400940805     C**************************************************************************
302500940805     C*    CMD18 VISUALIZZA LEGAMI
302600940805     C**************************************************************************
302700920326     C     CMD18         BEGSR
302800940810     C*
302900940907     C* SAVNR9 > 2 --> GESTIONE SUBFILE
303000950328    1C     SAVNR9        IFGT      2
303100940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
303200940810     C                   MOVEL     'S'           WRIEM
303300940810     C     FORC2         TAG
303400940810     C                   Z-ADD     1             REC9
303500950328    2C     WRIEM         IFEQ      'S'
303600940810     C                   WRITE     LS05T01
303700940810     C                   WRITE     LS05Z09
303800940810     C                   MOVEL     *BLANKS       WRIEM
303900950328    2C                   ENDIF
304000940810     C                   EXFMT     LS05C09
304100940810     C*
304200940810     C* CMD12 - FINE LAVORO
304300940810     C   KL              GOTO      SOTTO
304400940810     C*
304500940810     C                   READC     LS05S09                                93
304600950328    2C     *IN93         DOWEQ     '0'
304700940810     C*
304800950328    3C     V9CSCE        IFEQ      '5'
304900940810     C                   MOVEL     ' '           V9CSCE
305000950103     C*
305100950103     C* RIPRISTINO GLI INDICATORI
305200950103     C                   MOVEL     V9CI80        *IN80
305300950103     C                   MOVEL     V9CI81        *IN81
305400950103     C                   MOVEL     V9CI82        *IN82
305500950103     C                   MOVEL     V9CI83        *IN83
305600940810     C                   UPDATE    LS05S09
305700940810     C*
305800950328    4C     V9CAAN        IFEQ      *ZERO
305900940907     C                   MOVEL     V9CAAP        PA4AAS
306000940907     C                   MOVEL     V9CLPP        PA4LNP
306100940907     C                   MOVEL     V9CNRP        PA4NRS
306200940907     C                   MOVEL     V9CNSP        PA4NSP
306300940907     C                   ELSE
306400940907     C                   MOVEL     V9CAAN        PA4AAS
306500940907     C                   MOVEL     V9CLPN        PA4LNP
306600940907     C                   MOVEL     V9CNRN        PA4NRS
306700940907     C                   MOVEL     V9CNSN        PA4NSP
306800950328    4C                   ENDIF
306900940810     C                   MOVEL     *BLANKS       PA4F03
307000960903     C     PA0FLG        IFEQ      'N'
307100960903     C* N.B.: Non testo anche pa0flg='R' perchè è impossibile che si
307200960903     C*       verifichi questo caso
307300960903     C                   MOVEL     'P'           PA4FLG
307400960903     C                   ELSE
307500940810     C                   MOVEL     '3'           PA4FLG
307600960903     C                   END
307700940810     C                   MOVEL     PARAM4        KPJBU
307800940810     C                   CALL      'FNLR36R'
307900940810     C                   PARM                    KPJBA
308000940810     C                   MOVEL     KPJBU         PARAM4
308100940810     C                   MOVEL     PA4F03        WF03
308200940810     C                   MOVEL     'S'           WRIEM
308300950328    3C                   ENDIF
308400940810     C*
308500940810     C                   READC     LS05S09                                93
308600950328    2C                   ENDDO
308700940908     C*
308800940908     C* RIEMETTO IL FORMATO VIDEO DEI LEGAMI
308900950328    2C     WRIEM         IFEQ      'S'
309000940908     C                   GOTO      FORC2
309100950328    2C                   ENDIF
309200940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
309300940810     C                   ELSE
309400940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
309500940810     C                   MOVEL     COMAAN        PA4AAS
309600940810     C                   MOVEL     COMLPN        PA4LNP
309700940810     C                   MOVEL     COMNRN        PA4NRS
309800940810     C                   MOVEL     COMNSN        PA4NSP
309900940810     C                   MOVEL     *BLANKS       PA4F03
310000960903     C     PA0FLG        IFEQ      'N'
310100960903     C* N.B.: Non testo anche pa0flg='R' perchè è impossibile che si
310200960903     C*       verifichi questo caso
310300960903     C                   MOVEL     'P'           PA4FLG
310400960903     C                   ELSE
310500940810     C                   MOVEL     '3'           PA4FLG
310600960903     C                   END
310700940810     C                   MOVEL     PARAM4        KPJBU
310800940810     C                   CALL      'FNLR36R'
310900940810     C                   PARM                    KPJBA
311000940810     C                   MOVEL     KPJBU         PARAM4
311100940810     C                   MOVEL     PA4F03        WF03
311200940810     C                   MOVEL     'S'           WRIEM
311300940810     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
311400940810     C                   ENDIF
311500940810     C*
311600940810     C     SOTTO         TAG
311700920326     C                   ENDSR
311800940802      **************************************************************************
311900940802      * VISUALIZZO IL FORMATO VIDEO DI RIEPILOGO DELLA BOLLA
312000940802      **************************************************************************
312100110601     C     RIEPIL        BEGSR
312200940802      *
312300940802     C                   SETOFF                                       25
312400940804     C                   MOVEL     'S'           WRIEM             1
312500021218      *
312600021218      * Riempio il campone in fondo alla videata con i tasti scelti
312700021218     C                   exsr      Ges_TastiFun
312800021218     c                   eval      FD07 = FDxx
312900021218     c                   eval      F24D07 = F24Dxx
313000940802     C     FOR07         TAG
313100940804     C     WRIEM         IFEQ      'S'
313200940804     C                   WRITE     LS05T01
313300940804     C                   MOVEL     *BLANKS       WRIEM
313400940804     C                   ENDIF
313500021218      *
313600021218      * Imposto la stringa comandi del 'giro' successivo
313700021218     C                   IF        *inKY = *on and $Loop > 1
313800021218     c                   Exsr      Ges_F24
313900021218     c                   eval      FD07 = FDxx
314000021218     c                   eval      F24D07 = F24Dxx
314100021218     C                   ENDIF
314200940804     C                   EXFMT     LS05D07
314300940808     C                   MOVEL     *BLANKS       WF03
314400940802      *
314500990118      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
314600940802     C   KY*IN25         IFEQ      *ON
314700940802     C                   SETOFF                                       25
314800940802     C                   ELSE
314900940802     C                   SETON                                        25
315000940802     C                   ENDIF
315100940802     C   KY              GOTO      FOR07
315200940802      *
315300990118      * F3 - FINE LAVORO TORNO A SUBFILE
315400941109     C   KC              MOVEL     '1'           PA0F03
315500940802     C   KC              GOTO      ENDRIE
315600940802      *
315700940802      * TASTI DI FUNZIONE VARI
315800940802     C                   EXSR      COMAND
315900940808     C*
316000940808     C* CONTROLLO SE E' STATO PREMUTO F3=FINE
316100940808     C     WF03          IFNE      *BLANKS
316200941109     C                   MOVEL     '1'           PA0F03
316300940808     C                   GOTO      ENDRIE
316400940808     C                   ENDIF
316500091125     c*
316600091125     c* verifico se inserito '?' su beneficiario C/A
316700091125     c                   if        v2cben='?'
316800091125     c                   exsr      sr_bencas
316900091125     C                   SETON                                        31
317000091125     C                   MOVEL     'S'           WRIEM
317100091125     c                   endif
317200110601
317300950629     C*
317400940802     C   31              GOTO      FOR07
317500940802      *
317600940802     C     ENDRIE        ENDSR
317700940802      **************************************************************************
317800940802      * Nel caso di interrogazione da parte di un cliente, viene verificato se
317900940802      * egli puo' interrogare le bolle e se deve essere conteggiato il traffico
318000940802      * dei byte sulla linea di comunicazione.
318100940802      * Queste informazioni sono contenute nella tabella 3K.
318200940802      *
318300940802      * Inoltre, egli puo' interrogare solo alcuni clienti (quelli a lui
318400940802      * unificati.
318500940802      **************************************************************************
318600111020     C**!!!ABICLI        BEGSR
318700940802      *
318800111020     C**!!!              MOVE      *BLANKS       $3KINB
318900090603     C***                MOVE      *BLANKS       $3KCTB
319000940802      *
319100940802      * Leggo tutta la tabella 3K riempendo una schiera con i cod.clienti
319200111020     C**!!!              Z-ADD     *ZERO         $PRIMA            1 0
319300111020     C**!!!              Z-ADD     *ZERO         $A                3 0
319400111020     C**!!!              MOVE      '3K'          KCOD
319500111020     C**!!!KTAB2         SETLL     TABEL00F
319600111020     C**!!!              DO        *HIVAL
319700111020     C**!!!KTAB2         READE     TABEL00F                               90
319800111020     C*!N90TBLFLG        IFEQ      *BLANKS
319900111020     C**!!!              MOVEL     TBLUNI        DS3K
320000111020     C**!!!KNMUS         IFEQ      §3KPRF
320100940802      *
320200940802      * Devo prendere il cod.cliente, che e' la chiave di TABEL
320300111020     C**!!!              MOVEL     TBLKEY        $CODIC            7 0
320400111020     C**!!!$CODIC        LOOKUP    CLI                                    99
320500111020     C*!N99$A            IFLT      300
320600111020     C**!!!              ADD       1             $A
320700111020     C**!!!              Z-ADD     $CODIC        CLI($A)
320800111020     C**!!!              ENDIF
320900940802      *
321000940802      * Gia' dalla prima lettura ho i due flag che abilitano il profilo
321100111020     C**!!!$PRIMA        IFEQ      *ZERO
321200111020     C**!!!              MOVE      §3KINB        $3KINB
321300090603     C***                MOVE      §3KCTB        $3KCTB
321400111020     C**!!!§3KINB        COMP      'S'                                9090
321500111020     C**!!!              Z-ADD     1             $PRIMA
321600111020     C**!!!              ENDIF
321700940802      *
321800111020     C**!!!              ENDIF
321900111020     C**!!!              ENDIF
322000111020     C*!N90              ENDDO
322100940802      *
322200111020     C**!!!              ENDSR
322300940905     C**************************************************************************
322400940905     C* RIEMPIE IL SFL CON TUTTI I LEGAMI BOLLA
322500940905     C* PARTENDO DALLA BOLLA ORIGINALE, FINO ALL'ULTIMA IN
322600940906     C* ORDINE CRONOLOGICO (ULTIMO MODELLO SWATCH)
322700940905     C**************************************************************************
322800940906     C     LEGBOL        BEGSR
322900940906     C*
323000940906     C* PULISCO IL SFL
323100940906     C                   SETOFF                                       202123
323200940906     C                   WRITE     LS05C09
323300940906     C                   SETON                                        2123
323400940906     C*
323500940906     C                   SETOFF                                       07
323600940906     C                   Z-ADD     *ZERO         SAVNR9            4 0
323700940906     C                   MOVEL     *BLANKS       WFLG1             1
323800940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
323900940905     C*
324000940906     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
324100940906     C     KBLP3         CHAIN     FNLBL01L                           99
324200940906     C   99KBLP3         CHAIN     FNLBL02L                           99
324300940906     C  N99              DO
324400940905     C*
324500940906     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
324600940906     C                   MOVEL     LBLAAO        KAAS
324700940906     C                   MOVEL     LBLLPO        KLNP
324800940906     C                   MOVEL     LBLNRO        KNRS
324900940906     C                   MOVEL     LBLNSO        KNSP
325000940906     C     KLBL3         CHAIN     FNLBL02L                           99
325100950103     C     *IN99         IFEQ      *ON
325200950103     C                   MOVEL     'N'           WORIGI            1
325300950103     C                   ELSE
325400950103     C                   MOVEL     'S'           WORIGI
325500950103     C                   ENDIF
325600940907     C*
325700940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
325800940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
325900950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
326000950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
326100940907     C                   MOVEL     ' '           V9CSCE
326200940907     C                   MOVEL     LBLAAO        V9CAAP
326300940907     C                   MOVEL     LBLLPO        V9CLPP
326400940907     C                   MOVEL     LBLLAO        V9CLAP
326500940907     C                   MOVEL     LBLNRO        V9CNRP
326600940907     C                   MOVEL     LBLNSO        V9CNSP
326700940926     C                   CLEAR                   V9CAAN
326800940926     C                   CLEAR                   V9CLPN
326900940926     C                   CLEAR                   V9CLAN
327000940926     C                   CLEAR                   V9CNRN
327100940926     C                   CLEAR                   V9CNSN
327200940907     C*
327300940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
327400940907     C     LBLAAO        IFEQ      V1CAAS
327500940907     C     LBLLPO        ANDEQ     V1CLNP
327600940907     C     LBLNRO        ANDEQ     V1CNRS
327700940907     C     LBLNSO        ANDEQ     V1CNSP
327800940907     C                   MOVEL     *BLANKS       V9CPRE
327900940907     C                   SETON                                        80
328000940907     C                   SETON                                        81
328100940907     C                   MOVEL     '1'           WFLG1
328200940907     C                   ELSE
328300940907     C                   SETOFF                                       81
328400940907     C*
328500950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
328600950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
328700950320     C                   SETOFF                                       93
328800950320     C     KLBL3         SETLL     FNARB01L                               93
328900950320     C  N93              MOVEA     PRD(1)        V9CPRE
329000950320     C  N93              SETON                                        80
329100950320     C*
329200950320     C* SE RICHIAMATO DAGLI ARRIVI (FNLR36R) NON POSSO
329300950320     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
329400950320     C   93PA0FLG        IFEQ      'A'
329500960903     C     PA0FLG        OREQ      'R'
329600950320     C                   MOVEA     PRD(4)        V9CPRE
329700950320     C                   SETON                                        80
329800950320     C                   ELSE
329900950328     C                   MOVEA     PRD(2)        V9CPRE
330000950320     C                   SETOFF                                       80
330100950320     C                   ENDIF
330200950320     C*
330300950320     C* SE NON TROVATA SU ARB PROVO SU BLP
330400950320     C  N93              DO
330500950320     C                   SETOFF                                       93
330600950320     C     KLBL3         SETLL     FNBLP01L                               93
330700950320     C*
330800950328     C   93              MOVEA     PRD(3)        V9CPRE
330900950428     C   93              SETON                                        80
331000950320     C*
331100950320     C                   ENDDO
331200940907     C*
331300950320     C                   ENDIF
331400940907     C                   SETON                                        83
331500950103     C*
331600950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
331700950103     C                   MOVEL     *IN80         V9CI80
331800950103     C                   MOVEL     *IN81         V9CI81
331900950103     C                   MOVEL     *IN82         V9CI82
332000950103     C                   MOVEL     *IN83         V9CI83
332100950103     C*
332200940907     C                   Z-ADD     1             NRR9
332300940907     C                   WRITE     LS05S09
332400940907     C                   SETON                                        20
332500950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
332600950103     C*
332700950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
332800950103     C*                      2) NON HO TROVATO L'ORIGINALE
332900950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
333000950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
333100950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
333200950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
333300950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
333400950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
333500950103     C* L'ORIGINALE E LA MAMMA.
333600950103     C     WORIGI        IFEQ      'N'
333700940907     C*
333800950103     C                   MOVEL     ' '           V9CSCE
333900950103     C                   CLEAR                   V9CAAP
334000950103     C                   CLEAR                   V9CLPP
334100950103     C                   CLEAR                   V9CLAP
334200950103     C                   CLEAR                   V9CNRP
334300950103     C                   CLEAR                   V9CNSP
334400950103     C                   MOVEL     LBLAAP        V9CAAN
334500950103     C                   MOVEL     LBLLPP        V9CLPN
334600950103     C                   MOVEL     LBLLAP        V9CLAN
334700950103     C                   MOVEL     LBLNRP        V9CNRN
334800950103     C                   MOVEL     LBLNSP        V9CNSN
334900950103     C                   SETOFF                                       8182
335000950103     C*
335100950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
335200950103     C     V9CAAN        IFEQ      V1CAAS
335300950103     C     V9CLPN        ANDEQ     V1CLNP
335400950103     C     V9CNRN        ANDEQ     V1CNRS
335500950103     C     V9CNSN        ANDEQ     V1CNSP
335600950103     C                   MOVEL     *BLANKS       V9CPRE
335700950103     C                   SETON                                        80
335800950103     C                   SETON                                        82
335900950103     C                   ELSE
336000950103     C*
336100950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
336200950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
336300950320     C                   EXSR      CHKBOL
336400950103     C*
336500950103     C                   ENDIF
336600950103     C*
336700950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
336800950103     C                   SETOFF                                       83
336900950103     C*
337000950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
337100950103     C                   MOVEL     *IN80         V9CI80
337200950103     C                   MOVEL     *IN81         V9CI81
337300950103     C                   MOVEL     *IN82         V9CI82
337400950103     C                   MOVEL     *IN83         V9CI83
337500950103     C*
337600950103     C                   ADD       1             NRR9
337700950103     C                   WRITE     LS05S09
337800950103     C*
337900950103     C                   ENDIF
338000940907     C                   SETOFF                                       83  99
338100940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
338200940906     C* CICLO FINO ALL'ULTIMO LEGAME
338300940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
338400940907     C                   Z-ADD     *ZERO         WA                3 0
338500940907     C                   Z-ADD     *ZERO         WB                3 0
338600940907     C                   MOVEA     *ZERO         AAN
338700940906     C     *IN99         DOWEQ     *OFF
338800940905     C*
338900940906     C                   MOVEL     ' '           V9CSCE
339000940907     C                   MOVEL     LBLAAP        V9CAAP
339100940907     C                   MOVEL     LBLLPP        V9CLPP
339200940907     C                   MOVEL     LBLLAP        V9CLAP
339300940907     C                   MOVEL     LBLNRP        V9CNRP
339400940907     C                   MOVEL     LBLNSP        V9CNSP
339500940907     C                   MOVEL     LBLAAN        V9CAAN
339600940907     C                   MOVEL     LBLLPN        V9CLPN
339700940907     C                   MOVEL     LBLLAN        V9CLAN
339800940907     C                   MOVEL     LBLNRN        V9CNRN
339900940907     C                   MOVEL     LBLNSN        V9CNSN
340000940907     C                   SETOFF                                       8182
340100940906     C*
340200950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
340300940907     C     LBLAAP        IFEQ      V1CAAS
340400940907     C     LBLLPP        ANDEQ     V1CLNP
340500940907     C     LBLNRP        ANDEQ     V1CNRS
340600940907     C     LBLNSP        ANDEQ     V1CNSP
340700940907     C                   SETON                                        81
340800940907     C                   ENDIF
340900940907     C*
341000940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
341100940907     C     LBLAAN        IFEQ      V1CAAS
341200940907     C     LBLLPN        ANDEQ     V1CLNP
341300940907     C     LBLNRN        ANDEQ     V1CNRS
341400940907     C     LBLNSN        ANDEQ     V1CNSP
341500940906     C                   MOVEL     *BLANKS       V9CPRE
341600940906     C                   SETON                                        80
341700940907     C                   SETON                                        82
341800940906     C                   ELSE
341900940906     C*
342000950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
342100950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
342200950320     C                   EXSR      CHKBOL
342300940906     C*
342400940906     C                   ENDIF
342500950103     C*
342600950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
342700950103     C                   MOVEL     *IN80         V9CI80
342800950103     C                   MOVEL     *IN81         V9CI81
342900950103     C                   MOVEL     *IN82         V9CI82
343000950103     C                   MOVEL     *IN83         V9CI83
343100940906     C*
343200940906     C* SCRIVO IL RECORD NEL SFL
343300940906     C                   ADD       1             NRR9
343400940906     C                   WRITE     LS05S09
343500940907     C*
343600940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
343700940907     C                   ADD       1             WA
343800940907     C                   MOVEL     LBLAAN        AAN(WA)
343900940907     C                   MOVEL     LBLLPN        LPN(WA)
344000940907     C                   MOVEL     LBLNRN        NRN(WA)
344100940907     C                   MOVEL     LBLNSN        NSN(WA)
344200940907     C*
344300940907     C     KLBL3         READE     FNLBL02L                               99
344400940906     C*
344500940907     C* SE NON HO PIU' BOLLE CON READE
344600940906     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
344700970116     C     WB            DOWLT     WA
344800970116     C     *IN99         ANDEQ     *ON
344900940907     C                   ADD       1             WB
345000940907     C                   MOVEL     AAN(WB)       KAAS
345100940907     C                   MOVEL     LPN(WB)       KLNP
345200940907     C                   MOVEL     NRN(WB)       KNRS
345300940907     C                   MOVEL     NSN(WB)       KNSP
345400940907     C     KLBL3         SETLL     FNLBL02L
345500940907     C     KLBL3         READE     FNLBL02L                               99
345600970116     C                   ENDDO
345700940906     C*
345800940906     C                   ENDDO
345900940906     C                   ENDDO
346000940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
346100940906     C*
346200940906     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
346300940906     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
346400940926     C     NRR9          IFEQ      2
346500940907     C*
346600940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
346700940907     C     2             CHAIN     LS05S09                            99
346800940907     C     V9CAAP        IFEQ      V1CAAS
346900940907     C     V9CLPP        ANDEQ     V1CLNP
347000940907     C     V9CNRP        ANDEQ     V1CNRS
347100940907     C     V9CNSP        ANDEQ     V1CNSP
347200940907     C                   MOVEL     V9CAAN        COMAAN
347300940907     C                   MOVEL     V9CLPN        COMLPN
347400940907     C                   MOVEL     V9CNRN        COMNRN
347500940907     C                   MOVEL     V9CNSN        COMNSN
347600950306     C                   MOVEL     V9CLAN        COMLAN
347700950316     C                   MOVEA     'SEGUEN.'     LBL(14)
347800940907     C                   ELSE
347900940907     C                   MOVEL     V9CAAP        COMAAN
348000940907     C                   MOVEL     V9CLPP        COMLPN
348100940907     C                   MOVEL     V9CNRP        COMNRN
348200940907     C                   MOVEL     V9CNSP        COMNSN
348300950306     C                   MOVEL     V9CLAP        COMLAN
348400950316     C                   MOVEA     'PRECED.'     LBL(14)
348500940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
348600940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
348700940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
348800940926     C     1             CHAIN     LS05S09                            99
348900940926     C* REIMPOSTO NRR9=2 PERCHE' MI SERVE DOPO
349000940926     C                   Z-ADD     2             NRR9
349100940907     C                   ENDIF
349200940907     C*
349300950316     C                   MOVEA     COMLPN        LBL(21)
349400950316     C                   MOVEA     COMNRN        LBL(25)
349500950316     C                   MOVEA     COMNSN        LBL(28)
349600950316     C                   MOVEA     COMLAN        LBL(40)
349700940906     C*
349800940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
349900940907     C                   Z-ADD     1             W                 1 0
350000940907     C     V9CPRE        LOOKUP    PRD(W)                                 99
350100950306     C                   MOVEA     LBD(W)        LBL(44)
350200940907     C     W             COMP      2                                      07
350300940906     C*
350400940906     C                   MOVEA     LBL           LEGAME
350500940906     C                   ELSE
350600940905     C*
350700940926     C     NRR9          IFGT      2
350800940906     C                   MOVEA     LB2           LEGAME
350900940906     C                   SETON                                        07
351000940906     C                   ENDIF
351100940906     C                   ENDIF
351200940906     C*
351300940907     C     NRR9          COMP      1                                    3434
351400940905     C*
351500940906     C                   Z-ADD     NRR9          SAVNR9
351600940906     C                   Z-ADD     1             NRR9
351700940912     C*
351800940912     C* RIAGGANCIO LA BOLLA IN ESAME
351900940912     C     KBLP3         CHAIN     FNBLP01L                           99
352000940905     C*
352100940905     C                   ENDSR
352200950320     C**************************************************************************
352300950320     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
352400950320     C**************************************************************************
352500950320     C     CHKBOL        BEGSR
352600950320     C*
352700950320     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
352800950320     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
352900950320     C                   SETOFF                                       93
353000950320     C     KLBL1         SETLL     FNARB01L                               93
353100950320     C  N93              MOVEA     PRD(1)        V9CPRE
353200950320     C  N93              SETON                                        80
353300950320     C*
353400950320     C* SE RICHIAMATO DAGLI ARRIVI (FNLR36R) NON POSSO
353500950320     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
353600950320     C   93PA0FLG        IFEQ      'A'
353700960903     C     PA0FLG        OREQ      'R'
353800950320     C                   MOVEA     PRD(4)        V9CPRE
353900950320     C                   SETON                                        80
354000950320     C                   ELSE
354100950320     C                   MOVEA     PRD(2)        V9CPRE
354200950320     C                   SETOFF                                       80
354300950320     C                   ENDIF
354400950320     C*
354500950320     C* SE NON TROVATA SU ARB PROVO SU BLP
354600950320     C  N93              DO
354700950320     C                   SETOFF                                       93
354800950320     C     KLBL1         SETLL     FNBLP01L                               93
354900950328     C   93              MOVEA     PRD(3)        V9CPRE
355000950428     C   93              SETON                                        80
355100950320     C*
355200950320     C                   ENDDO
355300950320     C*
355400950320     C                   ENDSR
355500940923      **************************************************************************
355600940923      *   CONTROLLI SUL FORMATO VIDEO D02
355700940923      **************************************************************************
355800940923     C     CTRD02        BEGSR
355900940923     C*
356000940923     C                   SETOFF                                       31
356100940923     C*
356200110601     c* verifico se inserito '?' su email destinatario
356300110601     c                   if        v2cemw='?'
356400110601     c                   exsr      sr_emdest
356500110601     C                   SETON                                        31
356600110601     C                   MOVEL     'S'           WRIEM
356700110601     c                   if        v2cben='?'
356800110601     c                   write     ls05t01
356900110601     c                   write     ls05d02
357000110601     c                   endif
357100110601     c                   endif
357200091125     c* verifico se inserito '?' su beneficiario C/A
357300091125     c                   if        v2cben='?'
357400091125     c                   exsr      sr_bencas
357500091125     C                   SETON                                        31
357600091125     C                   MOVEL     'S'           WRIEM
357700091125     c                   endif
357800940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
357900940923     C     V2IGCA        IFEQ      '?'
358000991022     C                   CLEAR                   DS7PQRS
358100941003     C                   MOVEL     'S'           DS7RIC
358200941003     C                   MOVEL     'V'           DS7GES
358300941003     C                   MOVEL     V2CGCA        DS7COD
358400991022     C                   MOVEL     DS7PQRS       KPJBU
358500960126     C                   CALL      'TRTB72R'                            31
358600941003     C                   PARM                    KPJBA
358700940923     C                   MOVEL     *BLANKS       V2IGCA
358800940923     C                   SETON                                        31
358900940923     C                   MOVEL     'S'           WRIEM
359000940923     C                   ENDIF
359100940923     C*
359200940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
359300940923     C     V2IGGA        IFEQ      '?'
359400991022     C                   CLEAR                   DS7PQRS
359500941003     C                   MOVEL     'S'           DS7RIC
359600941003     C                   MOVEL     'V'           DS7GES
359700941003     C                   MOVEL     V2CGGA        DS7COD
359800991022     C                   MOVEL     DS7PQRS       KPJBU
359900960126     C                   CALL      'TRTB69R'                            31
360000941003     C                   PARM                    KPJBA
360100940923     C                   SETON                                        31
360200940923     C                   MOVEL     'S'           WRIEM
360300940923     C                   MOVEL     *BLANKS       V2IGGA
360400940923     C                   ENDIF
360500940923     C*
360600940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
360700940923     C     V2IGMA        IFEQ      '?'
360800991022     C                   CLEAR                   DS7PQRS
360900941003     C                   MOVEL     'S'           DS7RIC
361000941003     C                   MOVEL     'V'           DS7GES
361100941003     C                   MOVEL     V2CGMA        DS7COD
361200991022     C                   MOVEL     DS7PQRS       KPJBU
361300960126     C                   CALL      'TRTB70R'                            31
361400941003     C                   PARM                    KPJBA
361500940923     C                   SETON                                        31
361600940923     C                   MOVEL     'S'           WRIEM
361700940923     C                   MOVEL     *BLANKS       V2IGMA
361800940923     C                   ENDIF
361900940923     C*
362000940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
362100940923     C     V2IGVA        IFEQ      '?'
362200991022     C                   CLEAR                   DS7PQRS
362300941003     C                   MOVEL     'S'           DS7RIC
362400941003     C                   MOVEL     'V'           DS7GES
362500941003     C                   MOVEL     V2CGVA        DS7COD
362600991022     C                   MOVEL     DS7PQRS       KPJBU
362700960126     C                   CALL      'TRTB73R'                            31
362800941003     C                   PARM                    KPJBA
362900940923     C                   SETON                                        31
363000940923     C                   MOVEL     'S'           WRIEM
363100941003     C                   MOVEL     *BLANKS       V2IGVA
363200940923     C                   ENDIF
363300940923     C*
363400940923     C                   ENDSR
363500091125      **************************************************************************
363600091125      *   CONTROLLI SUL FORMATO VIDEO D02
363700091125      **************************************************************************
363800091125     C     sr_bencas     BEGSR
363900091125     c                   clear                   v2cben
364000091214     c                   clear                   wdaben
364100091214     c                   clear                   wdasrsc
364200091214     c                   clear                   wdascdi
364300091216     c                   clear                   trulintds
364400091216     c
364500091216     c                   eval      iinttip='B'
364600091216     c                   eval      iinttic=v2ctic
364700091216     c                   eval      iintrscm=v2crsm
364800091216     c                   eval      iintrsco=v2crmo
364900091216     c                   eval      iintCDI =w_cdi
365000091216     c                   call      'TRULINTR'
365100091216     c                   parm                    trulintds
365200091216     c
365300091216     c                   movel     ointrsci      wdasrsc
365400091216     c
365500091216     c                   select
365600091216     c                   when      ointtipi='O'
365700091216     c                   eval      wdaben='MITTENTE  ORIGINALE'
365800091216     c
365900091216     c                   when      ointtipi='R'
366000091216     c                   eval      wdascdi=w_cdi
366100091216     c                   eval      wdaben=' O R D I N A N T E'
366200091216     c
366300091216     c                   when      ointtipi='M'
366400091216     c                   if        v2ccdm>0
366500091216     c                   movel     v2ccdm        wdascdi
366600091216     c                   endif
366700091216     c                   eval      wdaben='  M I T T E N T E'
366800091216     c                   ENDSL
366900091214     c
367000091125     c                   exfmt     ls05wdas
367100091125     c                   endsr
367200110601      **************************************************************************
367300110601      *   emissione window email destinatario
367400110601      **************************************************************************
367500110601     C     sr_emdest     BEGSR
367600110601     c                   clear                   v2cemw
367700110601     c
367800110601     c                   eval      ilv79TLA='L'
367900110601     c                   eval      ilv79Aas=blpaas
368000110601     c                   eval      ilv79Lnp=blplnp
368100110601     c                   eval      ilv79nrs=blpnrs
368200110601     c                   eval      ilv79nsp=blpnsp
368300110601     c                   call      'FNLV79R'
368400110601     c                   parm                    fnlv79ds
368500110601     c
368600110601     c                   endsr
368700130912      **************************************************************************
368800130912      *   window per visualizzare gli orari servizi
368900130912      **************************************************************************
369000130912     C     sr_OrariSr    BEGSR
369100130920
369200140131     c                   clear                   trulorsds
369300140131     c                   eval       IORefil = BLPlna
369400140131     c                   eval       IORecap = BLPcad
369500140131     c                   eval       IOReloc = BLPlod
369600140131     c                   eval       IORenar = BLPnzd
369700140131     c
369800140131     c                   eval       IORetser = 'C'
369900140131     c                   eval       IORetsp = BLPtsp
370000140131     c                   eval       IOREtfp = blptfp
370100140131     c                   eval       IOREtfa = blptfa
370200140131     c                   eval       IOREdti = blpdti
370300140131     c                   eval       IOREhti = blphti
370400140203     c                   if         wesarb='S'
370500140203     c                   eval       IOREdti = arbdti
370600140203     c                   eval       IOREhti = arbhti
370700140203     c                   endif
370800140131     c                   eval       IOREdcr = blpdcr
370900140131     c                   eval       IOREhcr = blphcr
371000140131     c                   eval       IOREtcr = blptcr
371100140131     C*
371200140131     C                   MOVEL     ' '           D98TLA
371300140131     C                   MOVEL     'P'           D98TBO
371400140131     C                   MOVEL     BLPAAS        D98AAS
371500140131     C                   MOVEL     BLPLNP        D98LNP
371600140131     C                   MOVEL     BLPNRS        D98NRS
371700140131     C                   MOVEL     BLPNSP        D98NSP
371800140131     C*
371900140131     C                   CALL      'TNSD99R'
372000140131     C                   PARM                    TNSD99DS
372100140203     c
372200140131     c                   eval       IOREdei = d98dei
372300140131     c                   eval       IOREoei = d98oei
372400140131     c*****              eval       IORedta = BLPaas * 10000 + BLPmgs
372500140203     c
372600140131     c* Data di Riferimento: data consegna se consegnata
372700140131     c                   select
372800140131     c                   when      blpdcm>0
372900140131     c                   eval      ioredta=blpdcm
373000140131     c                   other
373100140131     c* altrimenti data prevista consegna se > = oggi
373200140131     C                   time                    wrkoggiiso
373300140131     C     *iso          move      wrkoggiiso    ioredta
373400140131     c* se < di oggi, imposto oggi
373500140131     c                   if        d98dce>ioredta
373600140131     c                   eval       ioredta=d98dce
373700140131     c                   endif
373800140131     c                   endsl
373900140203     c* Se in distinta leggo i dati da FNARB
374000140203     c                   if        blpdcm=0 and wesarb='S'
374100140203     c                   if         ARBfbc = *blanks and
374200140203     c                               ARBddc > 0 and
374300140203     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
374400140203     c                               arbica='R' or arbicc=' ' or arbicc='R')
374500140203     c                   eval       IOREcons = 'S'
374600140203     c                   eval       IOREDDC=arbddc
374700140203     c                   ENDIF
374800140203     c                   ENDIF
374900140131     C*
375000130920
375100140131     c                   call      'TRULORSR'
375200130912     c                   parm                    kpjba
375300140131     c                   parm                    trulorsds
375400130912     c
375500130912     c                   endsr
375600940810      **************************************************************************
375700940810      *   R O U T I N E     I N I Z I A L E
375800940810      **************************************************************************
375900940810     C     *INZSR        BEGSR
376000940810     C*
376100940810     C* ACCESSO   FILE BOLLE VISTA LOGICA 1
376200940810     C     KBLP3         KLIST
376300940810     C                   KFLD                    PA0AAS
376400940810     C                   KFLD                    PA0LNP
376500940810     C                   KFLD                    PA0NRS
376600940810     C                   KFLD                    PA0NSP
376700021218     C*
376800021218     C     KAR51         KLIST
376900021218     C                   KFLD                    KAAS
377000021218     C                   KFLD                    KLNP
377100021218     C                   KFLD                    KNRS
377200021218     C                   KFLD                    KNSP
377300021218     C                   KFLD                    KTRd              3
377400021218     C*
377500991022     C     KAR6          KLIST
377600991022     C                   KFLD                    PA0AAS
377700940808     C                   KFLD                    PA0LNP
377800940808     C                   KFLD                    PA0NRS
377900940808     C                   KFLD                    PA0NSP
378000991022     c                   KFLD                    KTRC
378100940810     C* ACCESSO   FILE LEGAME BOLLA
378200940810     C     KLBL1         KLIST
378300940810     C                   KFLD                    LBLAAN
378400940810     C                   KFLD                    LBLLPN
378500940810     C                   KFLD                    LBLNRN
378600940810     C                   KFLD                    LBLNSN
378700940906     C     KLBL3         KLIST
378800940906     C                   KFLD                    KAAS
378900940906     C                   KFLD                    KLNP
379000940906     C                   KFLD                    KNRS
379100940906     C                   KFLD                    KNSP
379200060216     C     Kar41         KLIST
379300940805     C                   KFLD                    KAAS
379400940805     C                   KFLD                    KLNP
379500940805     C                   KFLD                    KNRS
379600940805     C                   KFLD                    KNSP
379700940805     C                   KFLD                    KTRC
379800060216     C     Kar42         KLIST
379900951219     C                   KFLD                    KAAS
380000951219     C                   KFLD                    KLNP
380100951219     C                   KFLD                    KNRS
380200951219     C                   KFLD                    KNSP
380300940809     C     KTAB1         KLIST
380400940804     C                   KFLD                    CODUT
380500940804     C                   KFLD                    KCOD
380600940804     C                   KFLD                    KKEY
380700940809     C     KTAB2         KLIST
380800940804     C                   KFLD                    CODUT
380900940804     C                   KFLD                    KCOD
381000940808     C     KIND1         KLIST
381100940808     C                   KFLD                    CODUT
381200940808     C                   KFLD                    KKCC
381300940808     C                   KFLD                    KKSC
381400021203     C     Kfapd         klist
381500021203     C                   kfld                    kpdtip
381600021203     C                   kfld                    kpdpdr
381700040220      * File FITGD02L
381800040220     c     K02TGD02      klist
381900040220     c                   kfld                    TGDtor
382000040220     c                   kfld                    TGDogg
382100040713      * Fnorm
382200040713     c     korm          klist
382300040713     c                   kfld                    §b4poe
382400040713     c                   kfld                    §b4nsr
382500040713     c                   kfld                    §b4nor
382600040713     c                   kfld                    §b4nrv
382700940804     C*
382800940810     C                   MOVEL     *ZERO         COMLPN            3
382900940810     C                   MOVEL     *ZERO         COMNRN            2
383000940810     C                   MOVEL     *ZERO         COMNSN            7
383100950306     C                   MOVEL     *ZERO         COMLAN            3
383200940804     C*
383300940810     C                   Z-ADD     *ZERO         NRR8              4 0
383400940810     C                   Z-ADD     *ZERO         NRR9              4 0
383500940906     C                   Z-ADD     *ZERO         SAVNR8            4 0
383600940906     C                   Z-ADD     *ZERO         SAVNR9            4 0
383700940808     C                   MOVEL     *BLANKS       WF03              1
383800940808     C*
383900940804     C                   ENDSR
384000940804      **************************************************************************
384100941011**   SEQ
384200910912NON SEQUENZIALI
384300950316**        12345678901234567890123456789012345678901234567890
384400950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
384500920326**
384600950316LEGATA A PIU' BOLLE                       - F18=Visualizza
384700920326**
384800950316 Non Presente
384900950316 F18=Visualizza
385000940906Vedi in Int.Par
385100940907Vedi in Int.Arr
385200941011Vedi arch.stor.
385300920326**
385400920326Spedizione non Presente
385500941230
385600940907Vedi in Int.Bolle Part.
385700940907Vedi in Int.Bolle Arr.
385800941011Vedi in Archiv. Storico
385900040220** TF                                                                        *
386000040610F1=Agg.Rich.#
386100021218** TF                                                                        *
386200021218F2=C.A.#
386300021218** TF                                                                        *
386400021218F4=Tass2#
386500021218** TF                                                                        *
386600021218F5=Eventi#
386700021218** TF                                                                        *
386800021218F7=Anomalie#
386900021218** TF                                                                        *
387000021218F8=Orm#
387100130912** TF                                                                        *
387200130912F9=OrariSr#
387300130912** TF                                                                        *
387400021218F10=Variaz.#
387500021218** TF                                                                        *
387600030121F11=NoteS/B#
387700021218** TF                                                                        *
387800021218F12=Ritorno#
387900021218** TF                                                                        *
388000021218F13=Tassaz.#
388100021218** TF                                                                        *
388200021218F14=Giacenza#
388300040220** TF                                                                        *
388400040610F16=Gest.Rich.#
388500021218** TF                                                                        *
388600021218F19=Spunte#
388700021218** TF                                                                        *
388800021218F20=Affid.#
388900021218** TF                                                                        *
389000021218F21=Dett.Colli#
389100021218** TF                                                                        *
389200051201F22=C/Assegni#
