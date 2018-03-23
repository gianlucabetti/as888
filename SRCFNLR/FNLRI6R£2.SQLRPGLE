000100130314     /*PRM dbgview(*source)
000200130314     /*END
000300991105      *------------------------------------------------------------------------*
000400131107      *  Nome programma:  FNlRI6R
000500940712      *  Descrizione   :  Interrogazione bolle arrivi
000600940712      *  Data creazione:  23 OTT 1991
000700940712      *  Data 2° genesi:  12 LUG 1994 (nuovi archivi)
000800131114      *  Data 3° genesi:     NOV 2013
000900940712      *------------------------------------------------------------------------*
001000991105     H DECEDIT('0,') DATEDIT(*DMY/)
001100110929     h dftactgrp(*no) actgrp(*caller)
001200940712      *
001300131107     FFNLRI6D   CF   E             WORKSTN INFDS(CA02DS)
001400131107     F                                     SFILE(LRI6S02:NR2)
001500131107     F                                     SFILE(LRI6S06:NR6)
001600131107     F                                     SFILE(LRI6S08:NR8)
001700131107     F                                     SFILE(LRI6S05:NRR5)
001800131107     F                                     SFILE(LRI6S11:NRR11)
001900070404     FFIARP01L  iF   E           K DISK
002000050405     Ftigcp21l  IF   E           K DISK
002100040218     FAZORG01L  IF   E           K DISK
002200911024     FTABEL00F  IF   E           K DISK
002300021203     FFIAPD01L  IF   E           K DISK
002400940712     F*.......... B O L L E   A R R I V I
002500940712     FFNARB01L  IF   E           K DISK
002600120330     FFNARB05L  IF   E           K DISK    RENAME(FNARB000:FNARB005)
002700991027     FFNARB26L  IF   E           K DISK    RENAME(FNARB000:FNARB026)
002800991027     FFNARB27L  IF   E           K DISK    RENAME(FNARB000:FNARB027)
002900110930     FFNARB32L  IF   E           K DISK    RENAME(FNARB000:FNARB032)
003000110930     FFNARB33L  IF   E           K DISK    RENAME(FNARB000:FNARB033)
003100020910     FFNARB78L  IF   E           K DISK    RENAME(FNARB000:FNARB078)
003200060216     FFNARB54L  IF   E           K DISK    RENAME(FnARB000:FNARB054)
003300060216     FFiAR401L  IF   E           K DISK
003400060216     FFiAR403L  IF   E           K DISK    RENAME(FiAR4000:FiAR4003)
003500021213     FFIAR501L  IF   E           K DISK
003600991026     FFIAR601L  IF   E           K DISK
003700991026     FFIAR701L  IF   E           K DISK
003800991027     FFIARI01L  IF   E           K DISK
003900051114     FFiAR901L  IF   E           K DISK
004000940712     F*.......... D E T T A G L I O   C O L L I
004100940713     FFNART01L  IF   E           K DISK
004200131111     FFNART27L  IF   E           K DISK    RENAME(FNart000:FNart027)
004300131111     FFiarS02L  IF   E           K DISK
004400940712     F*.......... L E G A M E   B O L L A
004500940713     FFNLBL01L  IF   E           K DISK
004600991027     FFNLBL02L  IF   E           K DISK    RENAME(FNLBL000:FNLBL002)
004700940712     F*.......... B O L L E   P A R T E N Z E
004800940713     FFNBLP01L  IF   E           K DISK
004900940712     F*.......... V A R I A Z I O N I   B O L L E
005000131007     FFiARBF2C  IF   E           K DISK
005100990805     FFNEVB01L  IF   E           K DISK
005200950324     F*
005300020906     fFnfvv01l  if   e           k Disk
005400131119     ffidst01l  if   e           k Disk
005500990119     FFNDFA01L  IF   E           K DISK
005600010508     F*.......... O. R. M.
005700060216     FFiAR404L  if   e           k DISK    RENAME(FiAR4000:FiAR4004)
005800040218      *.......... T E L E F O N A T E   /   R E C L A M I
005900040218     fFITGD02L  if   e           k disk
006000030527     F*.......... N U O V E   T A B E L L E
006100030527     FTNTBE01L  IF   E           K DISK
006200131108     F*.......... S P U N T E
006300131108     Ffnbrv01l  IF   E           K DISK
006400131114     F
006500131114     Ffipct02l  IF   E           K DISK
006600040218      *------------------------------------------------------------------------*
006700060130     D $pog            s              3    dim(250)
006800060130     D $DIG            s              1    dim(20)
006900060130     D $ARG            s              3    dim(50)
007000060130     d
007100950307     D LBL             S              1    DIM(58) CTDATA PERRCD(58)
007200950307     D LB2             S             58    DIM(1) CTDATA PERRCD(1)
007300941011     D LBD             S             15    DIM(5) CTDATA PERRCD(1)
007400941011     D PRD             S             23    DIM(5) CTDATA PERRCD(1)
007500131111     D MSG             S             78    DIM(32) CTDATA PERRCD(1)
007600030612      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
007700030612     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
007800030612     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
007900070426     D CMA4f6          S              1    DIM(130) CTDATA PERRCD(65)
008000070426     D CMA5f6          S              1    DIM(130) CTDATA PERRCD(65)
008100030612      *
008200991222     D CNM             S              1    DIM(11) CTDATA PERRCD(11)
008300040218     D TF01            s              1    dim(20) ctdata perrcd(20)
008400070411     D TF01B           s              1    dim(20) ctdata perrcd(20)
008500021216     D TF02            S              1    DIM(20) CTDATA PERRCD(20)
008600021216     D TF04            S              1    DIM(20) CTDATA PERRCD(20)
008700021216     D TF05            S              1    DIM(20) CTDATA PERRCD(20)
008800070420     D TF06            S              1    DIM(20) CTDATA PERRCD(20)
008900021216     D TF07            S              1    DIM(20) CTDATA PERRCD(20)
009000021216     D TF08            S              1    DIM(20) CTDATA PERRCD(20)
009100130830     D TF09            S              1    DIM(20) CTDATA PERRCD(20)
009200021216     D TF10            S              1    DIM(20) CTDATA PERRCD(20)
009300021216     D TF11            S              1    DIM(20) CTDATA PERRCD(20)
009400021217     D TF12            S              1    DIM(20) CTDATA PERRCD(20)
009500021216     D TF13            S              1    DIM(20) CTDATA PERRCD(20)
009600021216     D TF14            S              1    DIM(20) CTDATA PERRCD(20)
009700040218     d TF16            s              1    dim(20) ctdata perrcd(20)
009800070404     d TF17            s              1    dim(20) ctdata perrcd(20)
009900021216     D TF19            S              1    DIM(20) CTDATA PERRCD(20)
010000021216     D TF20            S              1    DIM(20) CTDATA PERRCD(20)
010100021216     D TF21            S              1    DIM(20) CTDATA PERRCD(20)
010200110928     D TF22            S              1    DIM(20) CTDATA PERRCD(20)
010300021217     D $TF             S              1    DIM(20)
010400911024     D*
010500131111     D C1              S              1    DIM(30)
010600941212     D L1              S              3  0 DIM(30)
010700060613     D k11             S              1    DIM(11)
010800920403     D CVB             S              1    DIM(23)
010900921102     D SEG             S              1    DIM(15)
011000930705     D KCA             S              1    DIM(50)
011100940912     D DCA             S             44    DIM(50)
011200940907     D*
011300940907     D AAN             S              4  0 DIM(30)
011400940907     D LPN             S              3  0 DIM(30)
011500940907     D NRN             S              2  0 DIM(30)
011600940907     D NSN             S              7  0 DIM(30)
011700941024     D CC1             S              1    DIM(99)
011800941024     D CC2             S             20    DIM(99)
011900020329     D CC3             S              1    DIM(99)
012000020329     D CB1             S              1    DIM(99)
012100020329     D CB2             S             20    DIM(99)
012200030121     D RigaTf1         S              1    DIM(62)
012300030121     D RigaTf2         S              1    DIM(62)
012400030121     D RigaTf3         S              1    DIM(62)
012500030527     D*
012600060130     D cbodop          S              2    DIM(25)
012700040216     D* PASSAGGIO A INTERROGAZIONE VARIAZIONI           - FNLR75R -
012800940720     D* PASSAGGIO A INTERROGAZIONE DETTAGLIO COLLI      - FNLR37R -
012900940712     D PARAM1          DS
013000940714     D  V2CAAS                14     17  0
013100940712     D  COMLNP                18     20  0
013200940714     D  V2CNRS                21     22  0
013300940714     D  V2CNSP                23     29  0
013400940712     D  PA1F03                30     30
013500940713     D  PA1FLG                31     31
013600940713     D  PA1PGM                32     41
013700940714     D  PA1DSA                42     61
013800940809     D  PA1CML                62     62
013900940720     D  PA1DSP                63     70  0
014000940720     D  PA1RMN                71     85  0
014100940720     D  PA1LNA                86     88  0
014200940802     D  PA1LN1                89    108
014300940802     D  PA1RSD               109    143
014400941213     D* SE RICH DA PGM GIAC. = 1
014500941213     D  PA1GIA               144    144
014600941227     D  PA1FEL               145    147  0
014700950628     D  PA1TBO               148    148
014800110929     d  pa1rma               149    163
014900110929     d  pa1xco               164    164
015000980603     D* PASSAGGIO DATI PER CAMBIO AZIENDA
015100980609     D PARAAZ          DS
015200980603     D  CALPGM               201    210
015300060217     D**PARAZI               211    211
015400060217     D**PARF19               212    212
015500980603     D  PARF3                213    213
015600940802     D* RICHIAMO PGM INTERROGAZ.BOLLE PARTENZE            - FNLS04R -
015700940721     D* RICHIAMO PGM INTERROGAZ.BOLLE TRANSITO            - FNLR68R -
015800920203     D PARAM2          DS
015900940722     D  PA2FLG                31     31
016000940722     D  PA2BOL                62     62
016100940802     D* RICHIAMO PGM INTERROGAZ.BOLLE PART.CON KEY SPECIF - FNLS05R -
016200941214     D PARAM0          DS
016300950316     D  PA0AAS                 1      4  0
016400950316     D  PA0LNP                 5      7  0
016500950316     D  PA0NRS                 8      9  0
016600950316     D  PA0NSP                10     16  0
016700941214     D  PA0F03                17     17
016800941214     D  PA0FLG                18     18
016900941214     D  PA0A9                 19     19
017000941214     D  PA0B5                 20     20
017100941214     D  PA075                 21     21
017200941214     D  PA098                 22     22
017300941214     D  PA0RSU                23     42
017400941214     D  PA0CML                43     43
017500941214     D  PA0SIM                44     46  0
017600941214      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
017700941214     D  PA0CLI                47     47
017800941214      * PA0GIA indica se il pgm e' richiamato dalle giacenze
017900941214     D  PA0GIA                48     48
018000940802     D* RICHIAMO PGM DI INTERROGAZIONE SPUNTE FOGLI VARI  - FNLR04R -
018100941230     D PARA04          DS                  INZ
018200940713     D  PA4NFV                 1      5  0
018300940713     D  PA4LNP                 6      8  0
018400940713     D  PA4LNA                 9     11  0
018500940713     D  PA4NRS                12     13  0
018600940713     D  PA4NCD                14     20  0
018700940713     D  PA4NCA                21     27  0
018800940713     D  PA4NPS                28     29  0
018900940713     D  PA4NPG                30     30  0
019000940713     D  PA4FLG                31     31
019100940713     D  PA4PGM                32     38
019200941230     D  PA4FGS                39     41  0
019300101201     d* bolla
019400101201     d  pa4bolp               42     44  0
019500101201     d  pa4bors               45     46  0
019600101201     d  pa4bosp               47     53  0
019700101201     d  pa4boan               54     57  0
019800101201     D  PA4ERR                58     58
019900990119     D                 DS
020000990119     D  DCTDCA                 1      8  0
020100990119     D  DCTAAC                 1      4  0
020200990119     D  DCTMGC                 5      8  0
020300060518     D**               DS
020400060518     D**§11                    1     11  0 dim(11)
020500060518     D**dpdchd                12     12  0
020600060518     D**dpdbrc                 1     12  0
020700940726     D* RICHIAMO PGM DI INTERROGAZIONE EVENTI             - FNLRB5R -
020800950215     D PARAB5          DS                  INZ
020900940726     D  PB5AAS                14     17  0
021000940726     D  PB5LNP                18     20  0
021100940726     D  PB5NRS                21     22  0
021200940726     D  PB5NSP                23     29  0
021300940726     D  PB5F03                30     30
021400940726     D  PB5FLG                31     31
021500940726     D  PB5PGM                32     41
021600940726     D  PB5DSA                42     61
021700940726     D  PB5DSP                63     70  0
021800940726     D  PB5RMN                71     85  0
021900940726     D  PB5LNA                86     88  0
022000940802     D  PB5LN1                89    108
022100940802     D  PB5RSD               109    143
022200940802     D  PB5DCP               144    151  0
022300940802     D  PB5TMC               152    152
022400940712     D                 DS
022500940712     D  MM                     1      2  0
022600940712     D  GG                     3      4  0
022700940712     D  MMGG                   1      4  0
022800901116     D                 DS
022900940713     D  V1CLNA                 1      3  0
023000940713     D  V1CLP1                 4      6  0
023100940713     D  V1CDTD                 7     14  0
023200940713     D  V1CDTA                15     22  0
023300940712     D  SEL1                   1     22  0
023400901116     D                 DS
023500940713     D  V1CLP2                 1      3  0
023600940713     D  V1CNRS                 4      5  0
023700940713     D  V1CNSP                 6     12  0
023800901116     D  SEL2                   1     12  0
023900940712     D                 DS
024000131111     D  V1CRSD                 1     30
024100131111     D  V1CRSM                31     60
024200131111     d  v1cind                61     90
024300131111     d  v1clod                91    120
024400131111     d  v1cnzd               121    123
024500131111     d  v1cprd               124    125
024600131111     D  SEL3                   1    125
024700010508     D                 DS
024800010508     D  V1Cpoe                 1      3  0
024900010508     D  V1CNsr                 4      5  0
025000010508     D  V1CNor                 6     12  0
025100010508     D  V1CNrv                13     14  0
025200010508     D  SEL4                   1     14  0
025300990119     D                 DS
025400990119     D  V2CTSP                 1      1
025500990119     D  V2CGCA                 2      2
025600990119     D  V2CDAN                 3      3
025700990119     D  V2CFLA                 1      3
025800990119     D                 DS
025900990119     D  V2CCON                 1      6
026000990119     D  V2CFCC                 7      7
026100990119     D  V2CCOB                 1      7
026200131111     D**               DS
026300131111     D*+ARBAAS                 1      4  0
026400131111     D*+ARBMGS                 5      8  0
026500131111     D**ARBDSP                 1      8  0
026600131111     D
026700131111     D                 DS
026800131111     D  v1cbarcode             1     15  0
026900131111     D  b_aas                  1      2  0
027000131111     D  b_lnp                  3      5  0
027100131111     D  b_nrs                  6      7  0
027200131111     D  b_nsp                  8     14  0
027300950428     D                 DS
027400950428     D  V3CLNP                 1      3  0
027500950428     D  V3CNRS                 4      5  0
027600950428     D  V3CNSP                 6     12  0
027700950428     D  V3CDSP                13     20  0
027800950428     D  VIDRMN                21     35  0
027900131121     D  V3CLNA                36     38  0
028000950428     D  D15LNA                39     53
028100950428     D  V32RSD                54     85
028200950428     D  COMC08                 1     85
028300131111     d
028400930705     D WLBDAT          DS
028500940712     D  G02DAT                 1      8  0
028600940712     D  G02INV                 9     16  0
028700940712     D  G02ERR                17     17
028800940712     D  G02TGI                18     22  0
028900900517     D CA02DS          DS
029000900517     D  PRIMA                378    379B 0
029100950125     D                 DS
029200950125     D* CAMPO UNICO COMPRENDENTE RAG.SOC.+AGGIUNTIVI 35 CARATTERI
029300950125     D  V4CRSD                 1     35
029400131120     D  V4CNOT                36     70
029500131120     D  V4CNOM                 1     70
029600040218      *?definizione oggetto reclamo come spedizione?
029700131121     d ds_OggSped      ds                  inz
029800131121     d  wARBlnp                       3s 0 inz
029900131121     d  wARBnrs                       2s 0 inz
030000131121     d  wARBnsp                       7s 0 inz
030100131121     d  wARBaas                       4s 0 inz
030200040218      *
030300040218     d Status         sds
030400040218     d  SDSprm           *parms
030500991027      *
030600040216     D KPJBA         E DS
030700040218     d FIDNA1ds      e ds
030800040225      *
030900040225     d FIDNA2ds      e ds                  inz
031000040225     d  IA2mod       e                     inz('M')
031100040225     d  IA2tor       e                     inz('S')
031200040216      *
031300991029     d YEURCODS      E DS
031400991027     d TIBS69DS      E DS                  INZ
031500021213     d FNLRB3Ds      E DS
031600991027     d DS_cnaco      E DS                  extname(CNACO00F)
031700991027     d DS_cnind      E DS                  extname(CNIND00F)
031800991027     d DS_cnclp      E DS                  extname(CNCLP00F)
031900991027     d DS_fncls      E DS                  extname(FNCLS00F)
032000991027     D DSCV          E DS
032100020402     D DSCC          E DS
032200991027     D DS$2          E DS
032300991027     D DS3A          E DS
032400940909     D DS7O          E DS
032500951220     D DS7K          E DS
032600131108     D DS2A          E DS
032700020627
032800091214     d ds15          e ds
032900020627     d dspr          e ds
033000090604     d ds5e          e ds
033100020627
033200991217     D OG143         E DS
033300070403     D OG148         E DS
033400000615     D DSBL4A        E DS
033500060518     D DSBL4I        E DS
033600991217     D DSBL4E        E DS
033700971219     D DSARBD        E DS                  INZ
033800941014     D DSARBK        E DS                  INZ
033900941014     D DSARBG        E DS                  INZ
034000991011     D DARBT         E DS                  INZ
034100150724     D Diore01       E DS                  INZ
034200150724     d
034300991027     D FIDN00DS      E DS
034400091216     D TRULINTDS     E DS
034500991027     D DS7PQRS       E DS
034600991027     D FNLG30DS      E DS
034700991027     D FNLr44DS      E DS
034800130920
034900131114     d fipctcetds    e ds
035000140224     d fipctcords    e ds
035100140110     D TRULORSDS     E DS                  prefix(S_)
035200131114     D TRULORFDS     E DS
035300130920
035400961121     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
035500991027     D FNLR45DS      E DS
035600950102     D FNLR97        E DS
035700930428     D*
035800930428     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
035900991027     D TNSD99DS      E DS                  INZ
036000941014      *
036100030612     D* DS PER TRUL90R - RICHIESTA STAMPANTI
036200030612     D TRUL90DS      E DS
036300941014      *
036400941212     D* DS PER TRUL06R - CARICAMENTO £X
036500991027     D TRUL06DS      E DS
036600991027     D  LIN                    1     90  0 DIM(30)
036700941212      *
036800941014     D* DS PER FNLR66R - CONTROLLO E VISUALIZZAZIONE CAUSALI VARIAZIONI
036900991027     D FNLR66DS      E DS
037000060224      *
037100060224     D* DS PER FNLv55R - richiamo pgm che cerca i terminal per £6
037200060224     D fnlv55ds      E DS
037300941014      *
037400941014     D* DS PER FNLR48R - MANUTENZIONE BOLLE ARRIVI
037500991027     D FNLR48DS      E DS
037600941014      *
037700941212     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
037800991027     D FNLV50DS      E DS
037900051124     D* DS PER FNLV80R - decodifica cons richiesta
038000051124     D FNLV80DS      E DS
038100131120     D* DS PER FNLRG7R - visulizza altre abilitazioni
038200131120     D fnlrg7ds      E DS
038300070404     D* DS PER FNLRN5R - visulizza note LDV
038400140123     D**** fnlrn5ds      E DS
038500140123     D* DS PER FNLRN6R - visulizza note LDV
038600140123     D fnlrn6ds      E DS
038700070427     D* DS PER FIDG34R - reperisce dati consegna automatica
038800070427     D fidg34ds      E DS
038900070420     D* DS PER           stampa LDV
039000070420     D FNLSB5DS      E DS
039100130308     d FNLSB6ds1     e ds                  inz
039200130319     d  DB6pdf       e                     inz('X')
039300941212      *
039400131119     D DDSTFLR       E DS                  INZ
039500010510     D Dsbl4m        E DS
039600030121     d dTba          e ds
039700031119     d dAr5Gen       e ds
039800030121     d dAr5Ban       e ds
039900030703     d dAr5Bnb       e ds
040000130927     d dAr5emd       e ds
040100030121     d Tibs02Ds      e ds
040200060130     d Azuteds       e ds                  extname(Azute00f)
040300060130     d dDatiute      e ds
040400060130     d dLat          e ds
040500060130     d dute01        e ds
040600060130     D TRUL31DS      E DS
040700060130     D TRUL31DS2     E DS
040800060130     D TIBS34DS      E DS
040900110601     D fnlv79ds      E DS
041000141010      // -?Reperimento Disposizioni di Consegna da WEB
041100141105     d FNLRY09DS2    e ds                  inz
041200040809      * ds per il controllo della presenza di CA per la spedizione richiesta
041300040809     d FIDN12DS      e ds
041400040809     d  skKey                 26    305    dim(20)
041500040809     d  skAnn                306    325    dim(20)
041600040809     d  skDca                326    485  0 dim(20)
041700040809     d  skFca                486    545  0 dim(20)
041800040809     d  skDch                546    705  0 dim(20)
041900040809     d  skCch                706    745    dim(20)
042000040809     d*
042100040809     d dsKey           ds
042200040809     d  dsaac                         4  0
042300040809     d  dsfil                         3  0
042400040809     d  dsnca                         7  0
042500131111
042600131111       // -?File bolle arrivi
042700131111     d FNARBds       e ds                  extname(FNARB00F)
042800131111     d
042900131111     d wEnd            s               n   inz(*off)
043000040809
043100991027      *
043200991027     D W017A           s             17
043300991027     D w0130           s             13  0
043400991027     D w0133           s             13  3
043500991027     D IMPeditato      s             13
043600060130     D savkpjbu        S                   LIKE(kpjbu)
043700060130     D kscfil          S                   LIKE(arblna)
043800060130     D War4aas         S                   LIKE(AR4aas)
043900000228     D War4lnp         S                   LIKE(AR4lnp)
044000000228     D War4nrs         S                   LIKE(AR4nrs)
044100000228     D War4nsp         S                   LIKE(AR4nsp)
044200991027     D Wsvn            S                   LIKE(AR7svn)
044300991027     D Wvan            S                   LIKE(AR7van)
044400991027     D Wdiv            S                   LIKE(AR6div)
044500991029     D Wariimp         S                   LIKE(ARIimp)
044600991029     D Wariipp         S                   LIKE(ARIipp)
044700020906     d wfgs            s                   Like(Fvvfgs)
044800991027     D DATA_eur        S               D   DATFMT(*eur)
044900021203     d kpdpdr          S                   LIKE(APDpdr)
045000021203     d kpdtip          S                   LIKE(APDtip)
045100131120     d kndc6           S                   LIKE(dstnfv)
045200140110     d s_arbdei        S                   LIKE(d98dei)
045300140110     d s_arboei        S                   LIKE(d98oei)
045400150728     d s_arbttc        S                   LIKE(d98ttc)
045500150728     d s_arbtcc        S                   LIKE(d98tcc)
045600131114     D arbrsduni       S             70
045700131114     D arbdsp          S              8  0
045800131111     D W0080           S              8  0
045900991027     D W0040           S              4  0
046000991027     D wtibs69r        s              1
046100991027     D wtstdec         s              1
046200131120     D Wvideo          s              1
046300131209     D WopenC          s              1
046400020225     d lnantw          s                   like(§ogntw)
046500020225     d lnpntw          s                   like(§ogntw)
046600020910     d Wifp            s                   Like(Arbifp) Inz
046700060130     d wabi            s                   like(UteAut)
046800030703
046900030703     d wtba            s                   Like(§Ar5Tba)
047000030703     d wdestba         s                   Like(VidTba)
047100061018     d savnzd          s                   inz  like(v1cnzd)
047200020628
047300091216     D W_cdi           S                   LIKE(§ar5cdi)
047400020628     D ordina          S              1
047500020628     D comando         S              1
047600070420     D wrichtrul90     S              1
047700070403     d Dataeur         s               d   datfmt(*eur)
047800070403     d Datadmy         s               d   datfmt(*dmy)
047900110929     d s_param1        s                   like(param1)
048000110929     d no_f22          s              1    inz(' ')
048100110930     d comrma          s                   like(arbrma)
048200131108     d Wcol_D          s              5  0
048300131108     d Wcol_I          s              5  0
048400131108     d Wcarat          s              3  0
048500131108     d Indx            s              3  0
048600131209     d AP              s              3  0
048700131108     d wx              s              3  0
048800131007     d wy              s              3  0
048900131209     d w060a           s             60
049000131209     d wabif9          s              1
049100141010     d $Fine           s              1    inz(*off)
049200141010       // -?Reperimento Disposizioni di consegna da WEB
049300141010     d fnlry09r        pr                  extpgm('FNLRY09R')
049400141013     d  kpjba                              likeds(kpjba)
049500131111     d
049600131111       // -?Stringa SQL da eseguire
049700131111     d wSQL            s           2048    Varying        inz
049800030527     D*
049900030527     D wrkoggiiso      s               d
050000991027      *
050100930705     D* DEFINIZIONI COSTANTI
050200930705     D*
050300930705     D CNOSEQ          C                   CONST('NON SEQUENZIALI')
050400941014     D CINTER          C                   CONST('* INTERROGAZIONE BOL-
050500941014     D                                     LE ARRIVI *')
050600941014     D CMANUT          C                   CONST('** MANUTENZIONE BOLL-
050700941014     D                                     E ARRIVI **')
050800021216     D CF2413          C                   CONST('F24=AlTasti(1/3)')
050900021216     D CF2423          C                   CONST('F24=AlTasti(2/3)')
051000021216     D CF2433          C                   CONST('F24=AlTasti(3/3)')
051100021216     D CF2412          C                   CONST('F24=AlTasti(1/2)')
051200021216     D CF2422          C                   CONST('F24=AlTasti(2/2)')
051300110705     d
051400110705     D* CAMPO PER DSPATR "NO DISPLAY"
051500110705     D NODI            C                   CONST(X'27')
051600131120     D* CAMPO PER DSPATR "RI"
051700131120     D RI              C                   CONST(X'21')
051800131127     D* CAMPO PER DSPATR "HI"
051900131127     D hi              C                   CONST(X'22')
052000900518     C****************************************************************
052100900518     C*  RIEPILOGO INDICATORI
052200900518     C***************************************************************
052300911023     C* 01    - SELEZIONE PER LINEA ARRIVO
052400911023     C* 02    - SELEZIONE PER LINEA PARTENZA
052500911105     C* 03    - SPEDIZIONE GIACENTE
052600990119     C* 04    - ABILITO TASTO FUNZIONALE F2 - INT C.A.
052700990119     C* 05    - ABILITO TASTO FUNZIONALE F19- SPUNTE
052800070411     c* 06    - on per tassazione 2 bolla emette scritta "ImportoBOLLO"
052900070411     c*       - offper tassazione 2 bolla emette scritta "Importo IVA"
053000911023     C* 07    - PARZIALIZZ. SEGNACOLLO DAL
053100000216     C* 08    - bolla dpd
053200131114     C* 09    - scelta in altre parzializz
053300961111     C* 10    - BOLLA DI RIENTRO
053400911023     C* 11    - PARZIALIZZ. MITTENTE
053500131120     C* 12    -
053600961111     C* 13    - SPEDIZIONE IN CONSEGNA IN EVIDENZA IN SUBFILE
053700911023     C* 14    - PARZIALIZZ. DESTINATARIO
053800940318     C* 15    - DATA DAL UGUALE A DATA AL (SCELTO UN SOLO GIORNO)
053900961112     C* 16    - SI TRATTA DI BOLLA EXPORT
054000070403     C* 17    - p.o. utente e lna bolla hanno GEO attiva
054100911023     C* 18/21 - STATO SPEDIZIONI SELEZIONATO
054200991027     C* 22    - ESISTE ESTENSIONE BOLLA 2 e ABILITO IL TASTO  F4=TASSAZIONE 2
054300950825     C* 23    - SFLEND
054400950825     C* 24    - ABILITO IL TASTO "F13=TASSAZIONE"
054500010508     C* 25    - SELEZIONE PER NUMERO O.R.M.
054600070404     C* 26    - presente FIARP
054700070403     C* 27    - presente blocco consegna ARBFBC<>' '
054800941028     C* 28    - VISUALIZZA MSG ERRORE
054900911129     C* 29    - ESISTE CONTRASSEGNO
055000911023     C* 30/31 - LETTURA COMODO
055100911023     C* 32    - BOLLA LETTA DA SCARTARE
055200911023     C* 33    - BOLLA IN FRANCO
055300911023     C* 34    - LETTURA SUBFILE
055400911023     C* 35    - PULIZIA SUBFILE
055500911129     C* 36    - LETTURE
055600911023     C* 37    - ROLL UP
055700991027     C* 38    - ESISTE TB2 ANCHE SE NON PRESENTE TIPO RECORD '2' IN FIAR600F
055800920203     C* 39    - ABILITAZIONE TASTO DI COMANDO INT. BOLLE PARTENZE
055900131111     C* 40-59 - ERRORI
056000131111     C* 60    - email destiantario
056100131111     C* 61    - PROGRAMMA RICHIAMATO per scelta bolla
056200930407     C* 62    - ALTA INTENSITA' PER SPED BLOCCATE
056300040218     C* 63    - Abilita F1  = Aggiunta in reclamo
056400040218     C* 64    - Abilita F16 = Gestione reclamo
056500091214     c* 65    - Abilita visualizzazione beneficiario C/Ass
056600131127     C* 66    - libero - NON usato
056700960903     C* 67    - ABILITAZIONE TASTO DI COMANDO F7=ANOMALIE
056800131127     C* 68    - pos cursore RMN prima serv x  EMETTERE F23 PER CALCOLARE E VISUALIZZARE DATA CONS.
056900930505     C*         PREVISTA E LE ORE DI RITARDO
057000930526     C* 69    - DATA CONSEGNA MERCE >< 0
057100930510     C* 70    - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
057200990119     C* 71    - ABILITAZIONE TASTO DI COMANDO F8=EVENTI SEGNACOLLI
057300131107     C* 72    - SFLDSPCTL  LRI6S05
057400991027     C* 73    - BOLLA FRANCO FRONTIERA
057500970227     C* 74    - ABILITA EVENTI SEGNACOLLI
057600970227     C* 75    - UTILIZZATO PER NON ESEGUIRE LA SUBROUTINE "LEGBOL"
057700970227     C* 76    - SI DDT IN ALTA INTENSITA'
057800021213     C* 77    - NOTE SUPERMERCATI
057900970227     C* 78    - SFLDSP
058000950825     C* 79    - SFLDSPCTL
058100930510     C* 80    - NEL SFL DEI LEGAMI NON EMETTO IL CAMPO PER LA SELEZIONE
058200941028     C* 81    - USATO NEL VIDEO LEGAMI
058300941028     C* 82    - USATO NEL VIDEO LEGAMI
058400941028     C* 83    - USATO NEL VIDEO LEGAMI
058500970925     C* 84    - PARZIALIZZ. SEGNACOLLO AL
058600971219     C* 85    - VISUALIZZO RIFERIMENTO PARTNER ESTERO
058700111005     c* 86    - Abilitazione F22="PERFECT ORDER"
058800111005     C* 87    - Pgm richiamato in modalità "W" (Perfect Order Wurth)
058900110705     C* 88    -
059000931112     C* 89    - UTILIZZATO PER VISUALIZZARE ALTRI TASTI FUNZIONALI
059100920402     C* 90    - INDICA LA PRESENZA DI UN ERRORE
059200941028     C* 91    - ERRORE QCMDEXC (UTILIZZABILE)
059300940923     C* 92/94 - LEGAME BOLLA
059400930525     C* 95    - ESISTONO EVENTI DA INTERROGARE
059500131111     C* 96    -
059600991222     C* 97    - MANUTENZIONE BOLLE ARRIVI
059700941014     C* 98    - ESISTONO VARIAZIONI BOLLE EFFETTUATE
059800930504     C* 99    - OFF  ESISTE LEGAME BOLLA SEGUENTE
059900900518     C*****************************************************************
060000000000     C     *ENTRY        PLIST
060100000000     C                   PARM                    KPJBA
060200040218     c                   parm                    FIDNA1ds
060300131210     c
060400131210      /free
060500131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
060600131210      /end-free
060700980609     C**
060800940712     C                   MOVEL     KPJBU         PARAM1
060900980609     C                   MOVEL     KPJBU         PARAAZ
061000940713     C*
061100060130     C                   Z-ADD     1             CODUT             1 0
061200060130     c                   Movel     rsut          V1cdsa
061300060130     C                   MOVEL     rsut          PARDUT           20
061400941227     C*
061500941227     C                   MOVEL     SIMFEL        PA1FEL
061600990701     C                   MOVEL     V1CDSA        PA1DSA
061700940713     C*-------------------------------------------------------------------------
061800911024     C                   EXSR      CARTAB
061900131111
062000131111     c                   eval      Wend=*off
062100110930     c*
062200111005     c                   setoff                                       87
062300110930     c                   if        pa1flg='W'
062400110930     c                   eval      comrma=pa1rma
062500111005     c                   eval      *in87=*on
062600110930     c                   endif
062700110930     c* disabilito f22=PerfectOrder se richiamato da sè stesso
062800131107     c                   if        pa1pgm='FNLRI6R'
062900110929     c                   eval      no_f22='N'
063000110929     c                   endif
063100101202     c                   if        pa1pgm=*blanks
063200131107     C                   MOVEL     'FNLRI6R'     PA1PGM
063300101202     c                   endif
063400940713     C                   MOVE      'N'           V1CAFF
063500941215     C                   MOVEL     '? '          V1CCVB
063600941215     C                   CLEAR                   V1DCVB
063700941014     C* TESTATA
063800941014     C                   MOVEL     CINTER        V1CTES
063900131127     c                   clear                   v1clna
064000940317      *
064100940712      * IMPOSTO LA LINEA DI ARRIVO
064200060217     c* Non la imposto se richiamato con key specifica
064300040217if  1C     PA1FLG        IFNE      '1'
064400060217     C     PA1FLG        ANDNE     '3'
064500060217     C     PA1FLG        ANDNE     'C'
064600060217     C     PA1FLG        ANDNE     'N'
064700960903     C     PA1FLG        ANDNE     'P'
064800961121     C     PA1FLG        ANDNE     'E'
064900110929     C     PA1FLG        ANDNE     'W'
065000060217     c* o se vengo da int bolle normale (per assistenza)
065100060217     c     pa1flg        andne     ' '
065200060217     c     pa1flg        andne     '4'
065300060130     c                   z-add     dutpou        v1clna
065400020424     c*
065500040121     C                   MOVE      V1CLNA        NUM3              3 0
065600940712     C     NUM3          CHAIN     AZORG01L                           90
065700940713     C  N90              MOVEL     ORGDES        V1CDEA
065800040217e   1C                   ENDIF
065900930702     C*
066000920703     C                   EXSR      PUL01
066100940809     C                   MOVEL     'S'           WRIEM             1
066200930702     C**
066300930702     C* PGM RICHIAMATO
066400920326     C**
066500940713     C** PA1FLG =   - NON RICHIAMATO
066600920423     C**        = 1 - RICHIAMATO CON   KEY SPECIFICA
066700920423     C**        = 2 - RICHIAMATO SENZA KEY SPECIFICA PER   SELEZ BOLLA
066800920423     C**        = 3 - RICHIAMATO CON   KEY SPECIFICA DA INT-BOLL PART
066900920423     C**        = 4 - RICHIAMATO SENZA KEY SPECIFICA SENZA SELEZ BOLLA
067000990129     C**        = C - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F2=C.A
067100941014     C**        = M - MANUTENZIONE BOLLE ARRIVI
067200960903     C**        = N - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F7=ANOMA
067300960903     C**        = P - RICHIAMATO CON   KEY SPECIFICA DA INT-BOLL PART
067400960903     C**              E DISABILITAZ F7=ANOMALIE
067500961121     C**        = E - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F8=EV.SEGNACOLLI
067600110930     C**        = W - RICHIAMATO SENZA KEY SPECIFICA per "PerfectOrder"
067700960903     C*
067800040218      *?Abilito F1=Aggiunta in Reclami?
067900040218      * (se richiamato da pgm. FIDNA1R...)
068000040220     c                   eval      *in63  = *off
068100040303     c                   if            SDSprm >  1
068200040308     c                             and IA1ins = ' '
068300040220     c                   eval      *in63  = *on
068400060301     c                   clear                   v1clna
068500060301     c                   clear                   v1cdea
068600111003     c                   endif
068700960903     C* ABILITO F7=ANOMALIE SE FLAG DIVERSO DA 'N' E 'P'
068800960903     C     PA1FLG        IFNE      'N'
068900960903     C     PA1FLG        ANDNE     'P'
069000960903     C                   SETON                                        67
069100040217     C                   ENDIF
069200961121     C* ABILITO F8=EVENTI SEGNACOLLI  SE FLAG DIVERSO DA 'E'
069300961121     C     PA1FLG        IFNE      'E'
069400961121     C                   SETON                                        74
069500040217     C                   ENDIF
069600960903     C*
069700040217if  1C     PA1FLG        IFEQ      '1'
069800960903     C     PA1FLG        OREQ      'N'
069900940713     C     PA1FLG        OREQ      '3'
070000960903     C     PA1FLG        OREQ      'P'
070100961121     C     PA1FLG        OREQ      'E'
070200990129     C     PA1FLG        OREQ      'C'
070300920703     C*
070400940714     C                   MOVEL     V2CAAS        V1CAAS
070500940713     C                   MOVEL     COMLNP        V1CLP2
070600940714     C                   MOVEL     V2CNRS        V1CNRS
070700940714     C                   MOVEL     V2CNSP        V1CNSP
070800920703     C* SE NO PASSATA KEY SPECIFICA SI PUO' SELEZIONARE
070900940713     C     PA1FLG        IFEQ      '1'
071000960903     C     PA1FLG        OREQ      'N'
071100961121     C     PA1FLG        OREQ      'E'
071200990129     C     PA1FLG        OREQ      'C'
071300070411     c                   if        v1cnsp=0
071400920703     C                   SETON                                        61
071500040217     C                   ENDIF
071600070411     C                   ENDIF
071700040217x   1C                   ELSE
071800110929     c* Se richiamata con "W" devo saltare tutta la gestione della
071900110929     c* prima videata e devo passare subito al caricamento del sfl
072000110929   1ac     pa1flg        ifne      'W'
072100911213     C*
072200940713     C     PA1FLG        IFEQ      '2'
072300911213     C                   SETON                                        61
072400040217     C                   ENDIF
072500920203     C*
072600980603     C                   SETOFF                                       05
072700040217if  2C     PA1FLG        IFEQ      ' '
072800920203     C                   SETON                                        39
072900040217e   2C                   ENDIF
073000941014     C*
073100000616     C* MANUTENZIONE BOLLE ARRIVI 97=ON
073200040217if  2C     PA1FLG        IFEQ      'M'
073300030612      *
073400941014     C                   SETON                                        97
073500030612     C                   MOVEL     CMANUT        V1CTES
073600070420     c
073700040217e   2C                   ENDIF
073800000703     C*
073900941209     C     INIZIO        TAG
074000131114     C***                EXSR      PUL01
074100131114
074200131114     c* se cursore aperto --> lo chiudo
074300131209     c*****              if        Wend=*on  or *in14 or *in11
074400131209     c                   if        WopenC='1'
074500131114     c                   exsr      closecursor
074600131114     c                   endif
074700911025     C*
074800900509     C     FOR01         TAG
074900941209     C*
075000040217if  2C     WRIEM         IFEQ      'S'
075100131107     C                   WRITE     LRI6T01
075200940720     C                   MOVEL     *BLANKS       WRIEM
075300040217e   2C                   ENDIF
075400131107     C                   EXFMT     LRI6D01
075500940713     C*
075600940713     C                   SETOFF                                       28
075700940715     C                   MOVEL     *BLANKS       WRIEM
075800940713     C*
075900980603     C** F3 - FINE LAVORO
076000040217if  2C     *INKC         IFEQ      '1'
076100980603     C   05              MOVEL     '1'           PARF3
076200980603     C                   GOTO      FINE
076300040217e   2C                   ENDIF
076400131115     C**
076500131115     C** F5 - Altre parzializzazioni
076600131115     c                   if        *inke=*on
076700131115     c                   exsr      AltrePar
076800131115     C                   MOVEL     'S'           WRIEM
076900131115     C                   GOTO      FOR01
077000131115     c                   endif
077100131114     C**
077200980603     C** F7 - INTERROGAZIONE BOLLE PARTENZE
077300040217if  2C     *INKG         IFEQ      *ON
077400930922     C                   EXSR      INTPAR
077500940722     C                   MOVEL     'S'           WRIEM
077600920203     C                   GOTO      FOR01
077700040217e   2C                   ENDIF
077800131114     C**
077900131114     C** F10- pulizia di tutti i campi di scelta
078000131114if  2C     *INKj         IFEQ      *ON
078100131114     C                   EXSR      pul01
078200131114     C                   MOVEL     'S'           WRIEM
078300131114     C                   GOTO      FOR01
078400131114e   2C                   ENDIF
078500930922     C**
078600980603     C** F14- INTERROGAZIONE BOLLE TRANSITO
078700040217do  2C   KN              DO
078800940722     C                   MOVEL     'T'           PA2BOL
078900920423     C                   MOVEL     PARAM2        KPJBU
079000940721     C                   CALL      'FNLR68R'
079100920423     C                   PARM                    KPJBA
079200940722     C                   MOVEL     'S'           WRIEM
079300920423     C                   GOTO      FOR01
079400040217e   2C                   ENDDO
079500911025     C*
079600110929e  1ac                   endif
079700040217e   1C                   ENDIF
079800900509     C* CONTROLLI FORMATO1
079900110930     c     pa1flg        ifne      'W'
080000900509     C                   EXSR      CONTR
080100900509     C   90              GOTO      FOR01
080200110929     c                   endif
080300911025     C**
080400911025     C** P E R   N U M E R O   S P E D I Z I O N E
080500131111     c**  o      B A R C O D E
080600040217if  1C     SEL2          IFNE      0
080700131111     c     v1cbarcode    orgt      0
080800971223     C                   EXSR      SINGOL
080900971223     C     WINIZ         IFEQ      '1'
081000971223     C                   GOTO      INIZIO
081100971223     C                   ENDIF
081200971223     C     WFINE         IFEQ      '1'
081300971223     C                   GOTO      FINE
081400971223     C                   ENDIF
081500040217e   1C                   ENDIF
081600911025     C**
081700911025     C**  P E R   S U B F I L E
081800911023     C                   EXSR      ELAB
081900941014     C*
082000901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
082100040217if  1C     NR2           IFEQ      0
082200940713     C                   MOVEL     MSG(1)        V1CMSG
082300131115     C                   SETON                                        28
082400131115     c                   select
082500131115     c                   when      v1crpe<>*blanks
082600131115     C                   SETON                                        58
082700131115     c                   when      v1crmn>0 or v1crma<>*blanks
082800131127     C                   SETON                                        68
082900131115     c                   when      v1cnot<>*blanks
083000131115     C                   SETON                                        62
083100131115     c                   when      v1cprd<>*blanks
083200131115     C                   SETON                                        50
083300131115     c                   other
083400131115     C                   SETON                                        51
083500131115     c                   endsl
083600900516     C                   GOTO      FOR01
083700040217e   1C                   ENDIF
083800971223     C**
083900040217if  1C     V1CRPE        IFNE      *BLANKS
084000010509     C     NR2           ANDEQ     1
084100010509     c     *in25         oreq      *on
084200971223     C     NR2           ANDEQ     1
084300111011     c     v1crmn        orgt      *zeros
084400111011     C     NR2           ANDEQ     1
084500111011     c     v1crma        orne      *blanks
084600111011     C     NR2           ANDEQ     1
084700131111     c     v1cnot        orne      *blanks
084800131111     C     NR2           ANDEQ     1
084900111011     C                   MOVEL     v2clnp        COMLNP
085000111011     c     karb          chain     fnarb01l
085100971223     C                   EXSR      SINGOL
085200040217     C     WINIZ         IFEQ      '1'
085300971223     C                   GOTO      INIZIO
085400040217     C                   ENDIF
085500040217     C     WFINE         IFEQ      '1'
085600971223     C                   GOTO      FINE
085700040217     C                   ENDIF
085800040217e   1C                   ENDIF
085900930922     C**
086000900518     C     FORCT2        TAG
086100131107     C                   WRITE     LRI6T01
086200131107     C                   WRITE     LRI6Z02
086300900515     C** COMANDI SUBFILE
086400900515     C     FORCT         TAG
086500131107     C                   EXFMT     LRI6C02
086600940923     C                   SETOFF                                       28
086700941109     C                   CLEAR                   PA1F03
086800980603     C* F3 - FINE
086900980603     C     *INKC         IFEQ      '1'
087000980603     C   05              MOVEL     '1'           PARF3
087100980603     C                   GOTO      FINE
087200980603     C                   ENDIF
087300930922     C*
087400980603     C* F12 - RITORNO
087500040217if  1C     *INKL         IFEQ      *ON
087600110930     c     pa1flg        cabeq     'W'           fine
087700940713     C     PA1FLG        IFEQ      '3'
087800960903     C     PA1FLG        OREQ      'P'
087900940713     C     PA1FLG        OREQ      '1'
088000960903     C     PA1FLG        OREQ      'N'
088100961121     C     PA1FLG        OREQ      'E'
088200990129     C     PA1FLG        OREQ      'C'
088300940714     C                   Z-ADD     0             V2CAAS
088400920702     C                   Z-ADD     0             COMLNP
088500940714     C                   Z-ADD     0             V2CNRS
088600940714     C                   Z-ADD     0             V2CNSP
088700930922     C                   ENDIF
088800930922     C                   GOTO      INIZIO
088900040217e   1C                   ENDIF
089000900515     C* ROLLUP
089100040217if  1C     *IN37         IFEQ      *ON
089200940713     C   30              DO
089300940713     C                   MOVEL     MSG(15)       V1CMSG
089400940713     C                   SETON                                        28
089500940713     C                   ENDDO
089600930922     C  N30              EXSR      CARSF
089700940713     C                   Z-ADD     COMOD         REC2
089800930922     C                   GOTO      FORCT
089900040217e   1C                   ENDIF
090000900515     C*
090100900516     C** CONTROLLO SE EFFETTUATA UNA SCELTA
090200940713     C                   Z-ADD     PRIMA         REC2
090300131107     C                   READC     LRI6S02                                34
090400911023     C* VISUALIZZO BOLLA
090500040217do  1C     *IN34         DOWEQ     *OFF
090600940714     C                   MOVEL     *BLANKS       V2CFLG
090700911023     C*
090800040217if  2C     V2CSCE        IFEQ      '5'
090900940714     C                   Z-ADD     V2CLNP        COMLNP
091000940712     C     KARB          CHAIN     FNARB01L                           31
091100930518     C*
091200940714     C                   MOVEL     *BLANKS       V2CSCE
091300040217     C     V2CCON        IFEQ      'IN CON'
091400930518     C                   SETON                                        13
091500040217     C                   END
091600930518     C*
091700040217     C     ARBCCA        IFNE      ' '
091800930518     C     ARBFBC        ORNE      ' '
091900930518     C                   SETON                                        62
092000040217     C                   END
092100941014     C*
092200131107     C                   UPDATE    LRI6S02
092300911023     C*
092400911023     C                   EXSR      BOLLA
092500911023     C                   EXSR      VISUAL
092600911023     C*
092700040217x   2C                   ELSE
092800941014     C*
092900941014     C* 97 ON - MANUTENZIONE BOLLE
093000040217if  3C     V2CSCE        IFEQ      '1'
093100941014     C*
093200040217if  4C     *IN97         IFEQ      *ON
093300060227     c                   movel     v2clna        w003a             3
093400060224     C     w003a         LOOKUP    $pog                                   31
093500060130    2c                   if        not *in31 and d66pocl='S'
093600060130     c     v2ccbo        lookup    cbodop                                 31
093700060130    3c                   if        not *in31
093800060130     c                   movel     v2cksc        kscfil
093900060130   x3c                   else
094000060130     c* Se bolla doppia prendo il ksc dall'AR6
094100060130     C                   Z-ADD     v2cAAS        KAAS
094200060130     C                   Z-ADD     v2cLNP        KLNP
094300060130     C                   Z-ADD     v2cNRS        KNRS
094400060130     C                   Z-ADD     v2cNSP        KNSP
094500060130     C                   MOVEL     '2'           KTRC
094600060130     C     KAR61         CHAIN     FIAR6000                           31
094700060130if  4C                   IF          %FOUND(fiar601l)
094800060130     c                   movel     ar6ksc        kscfil
094900060130     c                   else
095000060130     c                   movel     999           kscfil
095100060130    4c                   endif
095200060130    3c                   endif
095300130314     c*
095400060224     c                   movel     kscfil        w003a
095500060224     c     w003a         lookup    $pog                                   31
095600060130    2c                   endif
095700130314     c*
095800040217if  5C     *IN31         IFEQ      *OFF
095900941212     C                   MOVEL     MSG(19)       V1CMSG
096000941212     C                   SETON                                        599028
096100941212     C                   MOVEL     '2'           V2CFLG
096200130314     c*
096300040217x   5C                   ELSE
096400141010
096500130314     c*
096600991027     C                   CLEAR                   FNLR48DS
096700060224     c* come p.o. gestione prendo il p.o. gestore (da £6) della linea
096800060224     c*  di arrivo
096900060224     c                   clear                   fnlv55ds
097000060224     c                   movel     v2clna        d55lin
097100060224     c                   movel     '6'           d55tpt
097200060224     C                   time                    wrkoggiiso
097300060224     C     *iso          move      wrkoggiiso    d55drf
097400060224     c                   call      'FNLV55R'
097500060224     c                   parm                    fnlv55ds
097600060224     c                   movel     d55tfa        d48fgs
097700130314     c*
097800941014     C                   MOVEL     V2CAAS        D48AAS
097900941014     C                   MOVEL     V2CLNP        D48LNP
098000941014     C                   MOVEL     V2CNRS        D48NRS
098100941014     C                   MOVEL     V2CNSP        D48NSP
098200141010     c* routine per verifica dispo da esaminare
098300141010     c                   exsr      sr_wdwdc
098400141010
098500941014     C                   MOVEL     V2CCBO        D48CBO
098600941014     C                   MOVEL     V1CCVB        D48CVB
098700950616     C                   MOVEL     'V'           D48FFR
098800941227     C                   MOVEL     'A'           D48TBO
098900950607     C                   MOVEL     'S'           D48F12
099000941014     C*
099100991027     C                   MOVEL     FNLR48DS      KPJBU
099200941014     C                   CALL      'FNLR48R'
099300941014     C                   PARM                    KPJBA
099400941212     C                   PARM                    DSARBD
099500941212     C                   PARM                    DSARBK
099600991011     C                   PARM                    DARBT
099700941212     C                   PARM                    DSARBG
099800030612     C                   PARM                    trul90ds
099900941212     C*
100000991027     C                   MOVEL     KPJBU         FNLR48DS
100100941212     C*
100200941014     C* ERRORE IN MANUTENZIONE BOLLE
100300040217     C     D48ERR        IFEQ      '1'
100400941014     C                   MOVEL     D48MSG        V1CMSG
100500941014     C                   SETON                                        592890
100600941014     C                   MOVEL     '2'           V2CFLG
100700040217     C                   ENDIF
100800950114     C*
100900950114     C* SE NON C'E' ERRORE AGGIORNO IL SFL CON I DATI DELLA BOLLA VAR
101000040217     C     *IN90         IFEQ      *OFF
101100950120     C                   Z-ADD     V2CLNP        COMLNP
101200950114     C     KARB          CHAIN     FNARB01L                           31
101300950114     C                   MOVEL     ARBRSM        V2CRSM
101400950114     C                   MOVEL     ARBRSD        V2CRSD
101500131114     c                   if        v1cind=*blanks
101600131114     c                   eval      v2cdest=arblod
101700131114     c                   else
101800131114     c                   eval      v2cdest=arbind
101900131114     c                   endif
102000131114     c
102100040217     C                   ENDIF
102200040217e   5C                   ENDIF
102300941014     C*
102400040217x   4C                   ELSE
102500941014     C*
102600911023     C* SELEZIONATA BOLLA: PASSO KEY ED ESCO DA PGM
102700040217     C     *IN61         IFEQ      *ON
102800940714     C                   Z-ADD     V2CLNP        COMLNP
102900940714     C                   MOVEL     '3'           V2CFLG
103000040220      *?- Richiamato da pgm. FIDNA1R?
103100040220if  6c                   if        SDSprm  > 1
103200131111     c                   eval      wARBlnp  = V2Clnp
103300131111     c                   eval      wARBnrs  = V2Cnrs
103400131111     c                   eval      wARBnsp  = V2Cnsp
103500040220     c                   eval      wARBaas = V2Caas
103600040220     c                   clear                   FIDNA1ds
103700040220     c                   eval      IA1ins  = 'S'
103800040220     c                   eval      IA1tor  = 'S'
103900040220     c                   eval      IA1ogg  = ds_OggSped
104000040220e   6c                   endif
104100040217     C                   ENDIF
104200040217e   4C                   ENDIF
104300040217e   3C                   ENDIF
104400930518     C*
104500040217do  3C  N61              DO
104600940714     C                   MOVEL     *BLANKS       V2CSCE
104700040217     C     V2CCON        IFEQ      'IN CON'
104800930518     C                   SETON                                        13
104900930518     C                   ELSE
105000040217     C     V2CCON        IFNE      *BLANKS
105100940714     C                   TESTN                   V2CCON               31
105200930518     C  N31              SETON                                        62
105300040217     C                   END
105400040217     C                   END
105500131107     C                   UPDATE    LRI6S02
105600040217e   3C                   ENDDO
105700930518     C*
105800040217e   2C                   ENDIF
105900911022     C*
106000941014     C                   SETOFF                                       136259
106100911022     C*
106200911022     C* SE FLAG 2 TORNO AL SUBFILE
106300040217     C     V2CFLG        IFNE      *BLANKS
106400900530     C                   SETON                                        34
106500900530     C                   ELSE
106600131107     C                   READC     LRI6S02                                34
106700040217     C                   ENDIF
106800040217e   1C                   ENDDO
106900941014     C*
107000911023     C* FLG =  3 ==> FILE PGM E PASSO KEY
107100940714     C     V2CFLG        IFEQ      '3'
107200911023     C                   GOTO      FINE
107300911023     C                   ELSE
107400911023     C* FLG >< 2 ==> TORNO A SUBFILE
107500911023     C                   GOTO      FORCT2
107600040217     C                   ENDIF
107700911023     C*
107800000000     C     FINE          TAG
107900930504     C*
108000930702     C* CALL A VUOTO AI PGM  PER CHIUDERE I FILES
108100941214     C     PA098         IFEQ      '1'
108200930504     C                   MOVEL     'C'           D98TLA
108300970925     C                   CALL      'TNSD99R'
108400991027     C                   PARM                    TNSD99DS
108500040217     C                   ENDIF
108600930702     C*
108700941214     C     PA075         IFEQ      '1'
108800940714     C                   MOVEL     'C'           PA1FLG
108900940712     C                   MOVEL     PARAM1        KPJBU
109000940714     C                   CALL      'FNLR75R'                            99
109100930524     C                   PARM                    KPJBA
109200040217     C                   ENDIF
109300930702     C*
109400941214     C     PA0B5         IFEQ      '1'
109500950215     C                   CLEAR                   PARAB5
109600940726     C                   MOVEL     'C'           PB5FLG
109700940726     C                   MOVEL     PARAB5        KPJBU
109800940722     C                   CALL      'FNLRB5R'
109900930525     C                   PARM                    KPJBA
110000040217     C                   ENDIF
110100941014     C*
110200941014     C     *IN97         IFEQ      *ON
110300941014     C                   MOVEL     'C'           D66TLA
110400991027     C                   MOVEL     FNLR66DS      KPJBU
110500941014     C                   CALL      'FNLR66R'
110600941014     C                   PARM                    KPJBA
110700040217     C                   ENDIF
110800991027      *
110900991027     C                   IF        Wtibs69r  <>  *BLANKS
111000991027     C                   eval      I69TLA  = 'C'
111100991027     C                   call      'TIBS69R'
111200991027     C                   parm                    TIBS69DS
111300991027     c                   ENDIF
111400930504     C*
111500941109     C* PER F3=FINE PULISCO CHIAVE BOLLA E IMPOSTO FLAG F3
111600941109     C     *INKC         IFEQ      *ON
111700941109     C                   Z-ADD     *ZERO         V2CAAS
111800941109     C                   Z-ADD     *ZERO         COMLNP
111900941109     C                   Z-ADD     *ZERO         V2CNRS
112000941109     C                   Z-ADD     *ZERO         V2CNSP
112100941109     C                   MOVEL     '1'           PA1F03
112200941109     C                   ENDIF
112300980609     C**
112400941212     C* CHIUDO I PGM
112500941212     C* SOLO PER PGM DI MANUTENZIONE
112600941212     C     *IN97         IFEQ      *ON
112700941212     C                   MOVEL     'C'           D66TLA
112800941212     C                   CALL      'FNLR66R'
112900991027     C                   PARM                    FNLR66DS
113000060224     c
113100060224     c                   clear                   fnlv55ds
113200060224     C                   MOVEL     'C'           D55TLA
113300060227     C                   CALL      'FNLV55R'
113400060224     C                   PARM                    FNLv55DS
113500941212     C*
113600941212     C                   MOVEL     'C'           D48TLA
113700991027     C                   MOVEL     FNLR48DS      KPJBU
113800941212     C                   CALL      'FNLR48R'
113900941212     C                   PARM                    KPJBA
114000941212     C*
114100141105     c                   eval      ilry09tla= 'C'
114200141105     c                   eval      kpjbu=fnlry09ds2
114300141105     c                   call      'FNLRY09R'
114400141105     c                   parm                    kpjba
114500941212     C                   ENDIF
114600980603     C**
114700980609     C                   MOVEL     PARAAZ        KPJBU
114800980624     C                   MOVEL     PARAM1        KPJBU
114900941212     C*
115000040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
115100040810     c                   clear                   fidn12ds
115200040810     c                   eval      i12tla = 'C'
115300040810     c                   call      'FIDN12R'
115400040810     c                   parm                    fidn12ds
115500051128     c                   clear                   fnlv80ds
115600051128     c                   eval      ilv80tla = 'C'
115700051128     c                   call      'FNLV80R'
115800051128     c                   parm                    kpjba
115900051128     c                   parm                    FNLV80DS
116000070427     c                   eval      id34tla = 'C'
116100070427     c                   call      'FIDG34R'
116200070427     c                   parm                    fidg34ds
116300131120     c                   eval      ilrg7tla = 'C'
116400131120     c                   call      'FNLRG7R'
116500131120     c                   parm                    fnlrg7ds
116600091216     c                   eval      iinttla = 'C'
116700091216     c                   call      'TRULINTR'
116800091216     c                   parm                    trulintds
116900141105
117000040810     c*
117100000000     C                   SETON                                        LR
117200941011     C   99
117300941011     CANN99              READ      FNBLP01L                               99
117400940802     C**************************************************************************
117500940802     C*    CARICO TABELLE
117600940802     C**************************************************************************
117700911024     C     CARTAB        BEGSR
117800941212     C***
117900941212     C* CARICO TABELLA FILIALI GESTITE £1
118000941212     C***
118100950122     C                   CLEAR                   L1
118200991027     C                   CLEAR                   TRUL06DS
118300941212     C                   MOVE      '£1'          D06COD
118400941212     C                   MOVEL     SIMFEL        D06KEY
118500941212     C                   MOVEL     'L'           D06TLA
118600991027     C                   MOVEL     TRUL06DS      KPJBU
118700941212     C*
118800941212     C                   CALL      'TRUL06R'
118900941212     C                   PARM                    KPJBA
119000991027     C                   MOVEL     KPJBU         TRUL06DS
119100941212     C                   MOVEA     LIN           L1
119200930705     C*
119300930705     C* CARICO TABELLA 7O
119400930705     C                   MOVEL     '7O'          COD
119500941212     C                   Z-ADD     1             C                 3 0
119600940713     C     KTAB2         SETLL     TABEL
119700940713     C     KTAB2         READE     TABEL                                  30
119800930705     C*
119900930705     C     *IN30         DOWEQ     *OFF
120000930705     C     TBLFLG        IFEQ      ' '
120100930705     C                   MOVEL     TBLKEY        KCA(C)
120200930705     C                   MOVEL     TBLUNI        DCA(C)
120300930705     C                   ADD       1             C
120400930705     C                   END
120500940713     C     KTAB2         READE     TABEL                                  30
120600930705     C                   END
120700941024     C*
120800941024     C* CARICO IN SCHIERA LA TABELLA CC
120900941024     C                   Z-ADD     *ZERO         WA
121000941024     C                   CLEAR                   CC1
121100941024     C                   CLEAR                   CC2
121200020329     C                   CLEAR                   CC3
121300941024     C                   MOVEL     'CC'          COD
121400941024     C     KTAB2         SETLL     TABEL00F
121500941024     C     KTAB2         READE     TABEL00F                               99
121600941024     C     *IN99         DOWEQ     *OFF
121700941024     C     TBLKEY        ANDNE     *BLANKS
121800941024     C     TBLKEY        ANDNE     *ZERO
121900941024     C                   ADD       1             WA
122000941024     C                   MOVE      TBLKEY        CC1(WA)
122100941024     C                   MOVEL     TBLUNI        CC2(WA)
122200020402     C                   MOVEL(P)  TBLUNI        DSCC
122300020402     C                   MOVE      §CCDFE        CC3(WA)
122400941024     C     KTAB2         READE     TABEL00F                               99
122500941024     C                   ENDDO
122600060130     C*
122700060130     C* CARICO IN SCHIERA LA TABELLA 3a per cod bolla doppi
122800060130     C                   Z-ADD     *ZERO         WA
122900060130     C                   CLEAR                   cbodop
123000060130     C                   MOVEL     '3A'          COD
123100060130     C     KTAB2         SETLL     TABEL00F
123200060130     C     KTAB2         READE     TABEL00F                               99
123300060130     C     *IN99         DOWEQ     *OFF
123400060130     c                   movel     tbluni        ds3a
123500060130     c                   if        §3atb2<>*blanks
123600060130     C                   ADD       1             WA
123700060130     C                   MOVEL     TBLKEY        cbodop(wa)
123800060130     c                   endif
123900060130     c
124000060130     C     KTAB2         READE     TABEL00F                               99
124100060130     C                   ENDDO
124200020329      *
124300020329      * CARICO IN SCHIERA LA TABELLA CB
124400020329     C                   Z-ADD     *ZERO         WA
124500020329     C                   CLEAR                   CB1
124600020329     C                   CLEAR                   CB2
124700020329     C                   MOVEL     'CB'          COD
124800020329     C     KTAB2         SETLL     TABEL00F
124900020329     C     KTAB2         READE     TABEL00F                               99
125000020329     C     *IN99         DOWEQ     *OFF
125100020329     C     TBLKEY        ANDNE     *BLANKS
125200020329     C     TBLKEY        ANDNE     *ZERO
125300020329     C                   ADD       1             WA
125400020329     C                   MOVE      TBLKEY        CB1(WA)
125500020329     C                   MOVEL     TBLUNI        CB2(WA)
125600020329     C     KTAB2         READE     TABEL00F                               99
125700020329     C                   ENDDO
125800991027     C*
125900991027     C* CARICO LA TABELLA $2
126000991027     C                   MOVEL     '$2'          COD
126100991027     C                   MOVEL(P)  1             KEY
126200991027     C     KTAB1         CHAIN     TABEL00F
126300991027     C                   IF        %FOUND
126400991027     C                   MOVEL     TBLUNI        DS$2
126500991027     C                   ELSE
126600991027     C                   CLEAR                   DS$2
126700991027     C                   ENDIF
126800940802     C*
126900911024     C                   ENDSR
127000940802     C**************************************************************************
127100940802     C*    PULIZIA CAMPI FORMATO 1
127200940802     C**************************************************************************
127300900514     C     PUL01         BEGSR
127400131114     c
127500131114     C* PULIZIA CAMPI CHE RIMANGONO INVARIATI IL SECONDO GIRO
127600131114     C                   MOVEL     ' '           V1CSSP
127700131127     c                   if        pa1flg='2' or pa1flg='4' or pa1flg='M'
127800131127     c                   else
127900131114     C                   MOVEL     *ZEROS        V1CLNA
128000131127     c                   endif
128100131115     c                   clear                   v1cdea
128200131115     c                   clear                   v1cdep
128300131114     C                   MOVEL     *ZEROS        V1CLP1
128400131114     C                   MOVEL     *ZEROS        V1CDTD
128500131114     C                   MOVEL     *ZEROS        V1CDTA
128600131114     c                   clear                   v1cnzd
128700131114     c                   clear                   v1cprd
128800131128     c                   movel     ' '           v1cxind
128900131114     c                   clear                   v1cind
129000131114     c                   clear                   v1clod
129100131128     c                   movel     ' '           v1cxlod
129200131114     C                   MOVE      *BLANKS       V1CRSD
129300131128     C                   movel     ' '           V1CXRSD
129400131114     C                   MOVE      *BLANKS       V1CRSM
129500131128     C                   movel     ' '           V1CXRSM
129600131114     C                   MOVEL     *ZEROS        NDC
129700131114     C                   MOVEL     *ZEROS        NSS
129800940802     C*
129900060613     C                   CLEAR                   wforz1
130000060613     C                   CLEAR                   V1CRPE
130100010509     C                   CLEAR                   V1Cpoe
130200010509     C                   CLEAR                   V1Cnsr
130300010509     C                   CLEAR                   V1Cnor
130400010509     C                   CLEAR                   V1Cnrv
130500110930     C                   CLEAR                   V1Crmn
130600110930     C                   CLEAR                   V1Crma
130700131115     C                   CLEAR                   V1Cdan
130800940713     C                   MOVEL     *ZEROS        V1CLP2
130900940713     C                   MOVEL     *ZEROS        V1CNRS
131000940713     C                   MOVEL     *ZEROS        V1CNSP
131100000103     C                   z-add     UYEAR         V1CAAS
131200000103     c     uyear         iflt      60
131300000103     c                   movel     20            v1caas
131400000103     c                   else
131500940713     C                   MOVEL     19            V1CAAS
131600000103     c                   endif
131700940713     C                   MOVEL     *BLANKS       V1CTSP
131800131114     C                   MOVEL     *BLANKS       V01DTSP
131900940713     C                   MOVEL     *BLANKS       V1CGIA
132000940713     C                   MOVEL     *ZEROS        V1CPSD
132100940713     C                   MOVEL     99            V1CPSA
132200940713     C                   MOVEL     *ZEROS        V1CSCA
132300940713     C                   MOVEL     *ZEROS        V1CSCD
132400940923     C                   MOVEL     *ZERO         V1CFLS
132500940713     C                   MOVE      'N'           V1CAFF
132600961112     C                   CLEAR                   V1CFBR
132700131111     C                   CLEAR                   V1Cbarcode
132800131111     C                   CLEAR                   V1CNOT
132900040216     c                   clear                   V1Ccbo
133000940713     C                   Z-ADD     0             NR2
133100070420     C****               MOVE      'N'           V1CSNP            1
133200070420     c                   clear                   v1csfd
133300941212     C                   SETOFF                                       09
133400131111     c
133500131111     c* se cursore aperto --> lo chiudo
133600131209     c***                if        Wend=*on  or *in14 or *in11
133700131209     c                   if        WopenC='1'
133800131111     c                   exsr      closecursor
133900131111     c                   endif
134000150223     c* se provengo da gestojne r.a. mi posiziono su brtcode
134100150223     c                   if            SDSprm >  1
134200150223     c                             and IA1ins = ' '
134300150223     c                   seton                                        60
134400150223     c                   endif
134500930705     C*
134600900514     C                   ENDSR
134700940802     C**************************************************************************
134800940802     C*    CONTROLLI FORMATO1
134900940802     C**************************************************************************
135000900509     C     CONTR         BEGSR
135100940317      *
135200940713     C                   MOVE      *BLANKS       V1CDEA
135300940713     C                   MOVE      *BLANKS       V1CDEP
135400110930     c                   movel     v1crma        comrma
135500020709
135600131121     c******             clear                   v2dlna
135700940317      *
135800900515     C                   SETOFF                                       900102
135900970925     C                   SETOFF                                       140784
136000980603     C                   SETOFF                                       03  11
136100040216     C                   SETOFF                                       154046
136200940715     C                   SETOFF                                       414243
136300940715     C                   SETOFF                                       444548
136400940715     C                   SETOFF                                       495152
136500131115     C                   SETOFF                                       552547
136600131111     C                   SETOFF                                       535460
136700131111     c                   setoff                                       575058
136800131127     c                   setoff                                       6862
136900110930     c                   clear                   h1riga
137000110930     c                   clear                   h1colo
137100950121     C*
137200950121     C* SE L'ANNO E' IMMESSO DI DUE CIFRE, METTO LE ALTRE DUE
137300950121     C     V1CAAS        IFLT      100
137400950121     C     V1CAAS        IFGT      60
137500950121     C                   ADD       1900          V1CAAS
137600950121     C                   ELSE
137700950121     C                   ADD       2000          V1CAAS
137800950121     C                   ENDIF
137900950121     C                   ENDIF
138000950121     C*
138100950121     C* SE VARIAZIONE CONTROLLO CAL CAUSALE
138200950121     C   97              DO
138300991027     C                   CLEAR                   FNLR66DS
138400950121     C     '?'           SCAN      V1CCVB                                 90
138500950121     C* RICERCA
138600950121    1C     *IN90         IFEQ      *ON
138700991027     C                   CLEAR                   FNLR66DS
138800950121     C                   MOVEL     'V'           D66GES
138900950121     C                   MOVEL     'A'           D66TBO
139000950121     C*
139100950121     C* SE IMMESSO NUMERO SPEDIZIONE LO VISUALIZZO
139200950121    2C     V1CNSP        IFGT      0
139300950121     C                   MOVEL     V1CNSP        D66NSP
139400950121     C                   MOVEL     V1CAAS        D66AAS
139500950121     C                   MOVEL     V1CLP2        D66LNP
139600950121     C                   MOVEL     V1CNRS        D66NRS
139700950121    2C                   ENDIF
139800950121     C*
139900991027     C                   MOVEL     FNLR66DS      KPJBU
140000950121     C                   CALL      'FNLR66R'
140100950121     C                   PARM                    KPJBA
140200991027     C                   MOVEL     KPJBU         FNLR66DS
140300950121     C                   MOVEL     D66CVB        V1CCVB
140400950121     C                   MOVEL     D66DES        V1DCVB
140500950121     C                   MOVEL     'S'           WRIEM
140600950203     C                   SETON                                        4790
140700950121     C*
140800950121     C                   GOTO      ENDCTR
140900950121   X1C                   ELSE
141000950121     C* CONTROLLO
141100991027     C                   CLEAR                   FNLR66DS
141200950121     C                   MOVEL     'C'           D66GES
141300950121     C                   MOVEL     'A'           D66TBO
141400950121     C                   MOVEL     V1CCVB        D66CVB
141500950121    2C     V1CNSP        IFGT      0
141600950121     C                   MOVEL     V1CNSP        D66NSP
141700950121     C                   MOVEL     V1CAAS        D66AAS
141800950121     C                   MOVEL     V1CLP2        D66LNP
141900950121     C                   MOVEL     V1CNRS        D66NRS
142000950121    2C                   ENDIF
142100950121     C*
142200991027     C                   MOVEL     FNLR66DS      KPJBU
142300950121     C                   CALL      'FNLR66R'
142400950121     C                   PARM                    KPJBA
142500991027     C                   MOVEL     KPJBU         FNLR66DS
142600950121     C                   MOVEL     D66CVB        V1CCVB
142700950121     C                   MOVEL     D66DES        V1DCVB
142800950121     C*
142900950121     C* ERRORE
143000950121    2C     D66MSG        IFNE      *BLANKS
143100950121     C                   MOVEL     D66MSG        V1CMSG
143200950121     C                   SETON                                        284790
143300950121     C                   GOTO      ENDCTR
143400950121     C*
143500950121   X2C                   ELSE
143600950121     C* DECODIFICA
143700950121     C                   MOVEL     D66DES        V1DCVB
143800950121    2C                   ENDIF
143900950121    1C                   ENDIF
144000950121     C                   ENDDO
144100950102     C*
144200131113     C* immettere almeno una selezione
144300901115     C     SEL1          IFEQ      0
144400920911     C     SEL2          ANDEQ     0
144500940317     C     SEL3          ANDEQ     *BLANKS
144600010509     C     SEL4          ANDEQ     *zeros
144700131113     c
144800971223     C     V1CRPE        ANDEQ     *BLANKS
144900110930     c     v1crmn        andeq     *zeros
145000110930     c     v1crma        andeq     *blanks
145100131111     c     v1cnot        andeq     *blanks
145200131111     c     v1cbarcode    andeq     *zeros
145300940713     C                   MOVEL     MSG(3)        V1CMSG
145400940713     C                   SETON                                        412890
145500901115     C                   GOTO      ENDCTR
145600901115     C                   END
145700940318      *
145800901115     C* DATA SPEDIZIONE
145900900514     C* LINEA ARRIVO
146000131113      * per destinatario o mittente sono obbligatori
146100020627      * la nazione e la provincia del destinatario
146200131113    1C     SEL2          IFEQ      0
146300131113    2c     sel4          ifeq      0
146400020225     c                   clear                   lnantw
146500131113    3C     V1CLNA        IFNE      0
146600940713     C                   MOVEL     V1CLNA        NUM3
146700940713     C     NUM3          CHAIN     AZORG01L                           41
146800131113    4C   41              DO
146900940713     C                   MOVEL     MSG(2)        V1CMSG
147000940713     C                   SETON                                        28  90
147100940713     C                   GOTO      ENDCTR
147200131113    4C                   ENDDO
147300940713     C                   MOVEL     ORGDES        V1CDEA
147400131121     C*******            MOVEL     ORGDES        V2DLNA
147500991222     C                   MOVEL     ORGDE3        og143
147600020225     c                   eval      lnantw = §ogntw
147700900514     C                   SETON                                        01
147800131113    3C                   END
147900131113    2C                   END
148000131113    c
148100020626      * selezione per destinatario o mittente
148200020626     c                   if        sel3 <> *blanks
148300020628     c                   exsr      ContrDatiDes
148400020628     c                   if        *in90 = *on                                  ricerca
148500020628     c                   goto      endctr
148600020628     c                   endif
148700020628     c                   if        *in28 = *on                                  errore
148800020628     c                   eval      *in90 = *on
148900020628     c                   goto      endctr
149000020628     c                   endif
149100020626     c                   endif
149200901115     C* LINEA PARTENZA
149300131111     c                   clear                   lnpntw
149400940713     C     V1CLP1        IFNE      0
149500940713     C                   MOVEL     V1CLP1        NUM3
149600940713     C     NUM3          CHAIN     AZORG01L                           42
149700940713     C   42              DO
149800940713     C                   MOVEL     MSG(5)        V1CMSG
149900940713     C                   SETON                                        28  90
150000940713     C                   GOTO      ENDCTR
150100940713     C                   ENDDO
150200940713     C                   MOVEL     ORGDES        V1CDEP
150300131121     C****               MOVEL     ORGDES        V2DLNP
150400000703     C                   movel     orgde3        og143
150500050304     c                   eval      lnpntw = §ogntw
150600901115     C                   SETON                                        02
150700901115     C                   END
150800940413      *
150900971223      * E' OBBLIGATORIA LNA O LNP
151000971223     C     V1CLNA        IFEQ      *ZERO
151100940713     C     V1CLP1        ANDEQ     *ZERO
151200971223     C     V1CRPE        ANDEQ     *BLANKS
151300010509     c     sel4          andeq     0
151400020628     c     sel3          andeq     *blanks
151500110930     c     v1crmn        andeq     *zeros
151600110930     c     v1crma        andeq     *blanks
151700131113     c     v1cnot        andeq     *blanks
151800131114     c     v1cbarcode    andeq     *zeros
151900940713     C                   MOVEL     MSG(4)        V1CMSG
152000940713     C                   SETON                                        412890
152100940712     C                   GOTO      ENDCTR
152200940712     C                   ENDIF
152300940413     C*
152400911023     C* DATA DAL OBBLIGATORIA
152500940713     C     V1CDTD        IFEQ      0
152600971223     C     V1CRPE        ANDEQ     *BLANKS
152700010509     C     sel4          ANDEQ     0
152800110930     C     v1crmn        ANDEQ     0
152900110930     C     v1crma        ANDEQ     *blanks
153000131111     C     v1cnot        ANDEQ     *blanks
153100131114     c     v1cbarcode    andeq     *zeros
153200131113     c
153300131113     c*********          if        v1cssp <> *blanks or
153400131113     c
153500131113     c                   if        (v1crsd = *blanks and v1crsm = *blanks and
153600131111     c                              v1cind = *blanks and v1clod = *blanks and
153700131111     c                             sel3 <> *blanks and *in01) or
153800131111     c                               sel3 = *blanks
153900940713     C                   MOVEL     MSG(8)        V1CMSG
154000940713     C                   SETON                                        285190
154100911023     C                   GOTO      ENDCTR
154200940712     C                   ENDIF
154300911023     C                   END
154400911023     C*
154500940712     C                   Z-ADD     *ZERO         COMDTD
154600940712     C                   Z-ADD     *ZERO         COMAAD
154700940712     C                   Z-ADD     *ZERO         COMMGD
154800940713     C     V1CDTD        IFNE      *ZERO
154900940713     C                   MOVE      V1CDTD        G02DAT
155000930705     C                   MOVEL     *BLANK        G02ERR
155100940713     C                   CALL      'XSRDA8'
155200930705     C                   PARM                    WLBDAT
155300930705     C     G02ERR        IFEQ      '1'
155400940713     C                   MOVEL     MSG(8)        V1CMSG
155500940713     C                   SETON                                        285190
155600930705     C                   GOTO      ENDCTR
155700930705     C                   ELSE
155800940713     C                   Z-ADD     G02DAT        V1CDTD
155900940713     C                   MOVE      G02INV        COMDTD            8 0
156000940713     C                   MOVEL     G02INV        COMAAD            4 0
156100930705     C                   MOVE      G02INV        COMMGD            4 0
156200930705     C                   END
156300940712     C                   ENDIF
156400900515     C* DATA SPEDIZIONE AL
156500940712     C                   Z-ADD     *ZERO         COMDTA
156600940713     C     V1CDTA        IFNE      0
156700940713     C                   MOVE      V1CDTA        G02DAT
156800930705     C                   MOVEL     *BLANK        G02ERR
156900940713     C                   CALL      'XSRDA8'
157000930705     C                   PARM                    WLBDAT
157100930705     C     G02ERR        IFEQ      '1'
157200941215     C                   MOVEL     MSG(8)        V1CMSG
157300940713     C                   SETON                                        285290
157400930705     C                   GOTO      ENDCTR
157500930705     C                   ELSE
157600940713     C                   Z-ADD     G02DAT        V1CDTA
157700940713     C                   MOVE      G02INV        COMDTA            8 0
157800900515     C                   END
157900930705     C*
158000900515     C* DATA SPEDIZIONE AL MAGGIORE DATA SPEDIZIONE AL
158100911023     C     COMDTA        IFLT      COMDTD
158200940713     C                   MOVEL     MSG(10)       V1CMSG
158300940713     C                   SETON                                        285152
158400940713     C                   SETON                                        90
158500900515     C                   GOTO      ENDCTR
158600900515     C                   END
158700940318      *
158800940712     C                   ELSE
158900940712      *
159000940712      * SE NON IMMESSA LA DATA FINALE, LA PONGO UGUALE A QUELLA INIZIALE
159100940712     C                   Z-ADD     COMDTD        COMDTA
159200940318      *
159300900515     C                   END
159400940318      *
159500940712      * VERIFICO SE LE DUE DATE SONO COINCIDENTI
159600131115     c                   if        comdtd>0
159700940712     C     COMDTD        COMP      COMDTA                                 15
159800131115     c                   endif
159900901115     C***
160000911024     C* GIACENTI
160100911024     C* SCELTE INCONGRUENTI
160200940713     C     V1CGIA        IFEQ      'G'
160300940713     C     V1CSSP        IFEQ      '2'
160400940713     C     V1CSSP        OREQ      '3'
160500940713     C                   MOVEL     MSG(14)       V1CMSG
160600940713     C                   SETON                                        285690
160700911024     C                   GOTO      ENDCTR
160800911024     C                   END
160900911024     C                   END
161000901115     C* NUMERO SERIE AL DEVE ESSERE > NUMERO SERIE DAL
161100901115     C*  SE IMMESSO
161200940713     C     V1CPSD        IFGT      V1CPSA
161300940713     C                   MOVEL     MSG(11)       V1CMSG
161400940713     C                   SETON                                        284890
161500901115     C                   GOTO      ENDCTR
161600901115     C                   END
161700901115     C* NUMERO SEGNACOLLO AL DEVE ESSERE > NUMERO SEGNACOLLO DAL
161800940713     C     V1CSCA        IFGT      0
161900940713     C     V1CSCD        ANDGT     V1CSCA
162000940713     C                   MOVEL     MSG(12)       V1CMSG
162100940713     C                   SETON                                        284990
162200901115     C                   GOTO      ENDCTR
162300901115     C                   END
162400901115     C* SE IMMESSO AL IMMETTERE ANCHE DAL
162500940713     C     V1CSCA        IFGT      0
162600940713     C     V1CSCD        ANDEQ     0
162700940713     C                   MOVEL     MSG(13)       V1CMSG
162800940713     C                   SETON                                        284990
162900901115     C                   GOTO      ENDCTR
163000901115     C                   END
163100940713     C     V1CSCD        COMP      0                                  07
163200970925     C     V1CSCA        COMP      0                                  84
163300940923     C*
163400940923     C* LA FILIALE SEGNACOLLO DEVE ESSERE VALIDA, SE IMMESSSA
163500940923     C     V1CFLS        IFNE      *ZERO
163600940923     C     V1CFLS        CHAIN     AZORG01L                           46
163700941014     C     *IN46         IFEQ      *ON
163800941014     C     *IN46         OREQ      *OFF
163900941014     C     ORGFVA        ANDNE     *BLANKS
164000941014     C                   SETON                                        284690
164100941014     C                   MOVEL     MSG(17)       V1CMSG
164200941014     C                   GOTO      ENDCTR
164300940923     C                   ENDIF
164400941014     C                   ENDIF
164500940923     C*
164600901115     C* SE IMMESSA RAGIONE SOCIALE CERCO PRIMO BLANKS
164700940713     C     V1CRSM        IFNE      *BLANKS
164800901115     C                   SETON                                        11
164900940713     C                   MOVEA     V1CRSM        C1
165000131111     C                   Z-ADD     30            XX                2 0
165100930702     C     C1(XX)        DOWEQ     *BLANKS
165200930702     C                   SUB       1             XX
165300901116     C                   END
165400901116     C                   END
165500901115     C* IMMESSO DESTINATARIO: CERCO PRIMO BLANKS
165600940713     C     V1CRSD        IFNE      *BLANKS
165700901115     C                   SETON                                        14
165800940713     C                   MOVEA     V1CRSD        C1
165900131111     C                   Z-ADD     30            ZZ                2 0
166000930702     C     C1(ZZ)        DOWEQ     *BLANKS
166100930702     C                   SUB       1             ZZ
166200901116     C                   END
166300901115     C                   END
166400131113     C* località DESTINATARIO: CERCO PRIMO BLANKS
166500131113     C     V1Clod        IFNE      *BLANKS
166600131113     C                   SETON                                        14
166700131113     C                   MOVEA     V1Clod        C1
166800131113     C                   Z-ADD     30            LL                2 0
166900131113     C     C1(LL)        DOWEQ     *BLANKS
167000131113     C                   SUB       1             LL
167100131113     C                   END
167200131113     C                   END
167300131113     C* Indirizzo DESTINATARIO: CERCO PRIMO BLANKS
167400131113     C     V1Cind        IFNE      *BLANKS
167500131113     C                   SETON                                        14
167600131113     C                   MOVEA     V1Cind        C1
167700131113     C                   Z-ADD     30            DD                2 0
167800131113     C     C1(DD)        DOWEQ     *BLANKS
167900131113     C                   SUB       1             DD
168000131113     C                   END
168100131113     C                   END
168200131113     c
168300131113     c* 14 on anche per indirizzo o località  DESTINAT
168400131113     c                   if        not *in11 and
168500131113     c                              v1clod<>*blanks or v1cind<>*blanks
168600131113     c                   seton                                        14
168700131113     c                   endif
168800131113     c
168900131113      * Nazione provincia destinatario senza ragioni sociali
169000060613      * imposto per destinatario se non è stata impostata la lna  e la linea
169100060613      * partenza
169200060613     c                   if        v1cprd <> *blanks and
169300020628     c                             not *in11 and not *in14 and not *in01
169400060613     c                   eval      *in14 = *on
169500020628     c                   endif
169600060613     c                   if         v1cnzd <> *blanks and v1cprd = *blanks and
169700060613     c                             not *in11 and not *in14 and not *in01
169800060613     c                             and not *in02
169900060613     c                   eval      *in14 = *on
170000060613     c                   endif
170100901115     C                   END
170200901115     C*
170300901115     C** NUMERO SPEDIZIONE
170400901115     C** LINEA PARTENZA
170500140117     C***  SEL2          IFNE      0
170600140117     c                   if        sel2<> 0 and v1cbarcode<=0
170700940713     C                   Z-ADD     V1CLP2        NUM3
170800940713     C     NUM3          CHAIN     AZORG01L                           45
170900940713     C   45              DO
171000940713     C                   MOVEL     MSG(5)        V1CMSG
171100940713     C                   SETON                                        28  90
171200940713     C                   GOTO      ENDCTR
171300940713     C                   ENDDO
171400911023     C* ANNO SPEDIZIONE
171500940713     C     V1CAAS        IFEQ      0
171600940713     C                   MOVEL     MSG(7)        V1CMSG
171700940713     C                   SETON                                        285590
171800911023     C                   GOTO      ENDCTR
171900911023     C                   END
172000940713     C     KLNPX         CHAIN     FNARB01L                           44
172100030527     C*
172200940713     C   44              DO
172300940713     C                   MOVEL     MSG(6)        V1CMSG
172400940713     C                   SETON                                        28  90
172500940713     C                   GOTO      ENDCTR
172600940713     C                   ENDDO
172700901115     C*
172800901115     C                   END
172900131111     c***
173000131113     C* B A R C O D E   L D V
173100131111     c***
173200131111     c                   if        v1cbarcode>0
173300131111     c                   z-add     b_aas         arbaas
173400131111     C     b_aas         IFLT      100
173500131111     C     b_aas         IFGT      60
173600131111     C                   ADD       1900          arbAAS
173700131111     C                   ELSE
173800131111     C                   ADD       2000          arbAAS
173900131111     C                   ENDIF
174000131111     C                   ENDIF
174100131111     c                   z-add     b_lnp         arblnp
174200131111     c                   z-add     b_nrs         arbnrs
174300131111     c                   z-add     b_nsp         arbnsp
174400131114     C     Karb2         CHAIN     FNARB01L
174500131111     C*
174600131111     C                   if        not%found(fnarb01l)
174700131111     C                   MOVEL     MSG(32)       V1CMSG
174800131111     C                   SETON                                        286090
174900131111     C                   GOTO      ENDCTR
175000131111     C                   endif
175100131111     C                   endif
175200010508      ***
175300010508      * N U M E R O   O R M
175400010508      ***
175500010508     C     sel4          ifne      0
175600010508     C     sel2          andeq     0
175700010508      * P.O. EMISSIONE
175800010508     C                   z-add     v1cpoe        num3
175900010508     C     num3          chain     azorg01l                           53
176000010508     C     *in53         ifeq      *on
176100010508     C                   movel     msg(21)       v1cmsg
176200010508     C                   seton                                        28  90
176300010508     C                   goto      endctr
176400010508     C                   endif
176500010508      * N.ORM
176600010510     C                   MOVEL     'M'           WTRC
176700010510     C                   MOVEL(P)  SEL4          WN14             14
176800060216     C     kar44         chain     fiar404l                           54
176900010508     C     *in54         ifeq      *on
177000010508     C                   movel     msg(22)       v1cmsg
177100010508     C                   seton                                        28  90
177200010508     C                   goto      endctr
177300010508     C                   endif
177400010508      *
177500010508     C                   seton                                        25
177600010508      *
177700010508     C                   endif
177800131111     c
177900901115     C*
178000911023     C* TIPO SPEDIZIONE ESPRESSO
178100131114     C                   MOVEL     *BLANKS       V01DTSP
178200940713     C     V1CTSP        IFNE      ' '
178300940715     C     V1CTSP        IFEQ      '?'
178400940715     C                   CALL      'X§TABER'
178500040217     C                   PARM      CodUt         §KUT              1 0
178600040217     C                   PARM      '5E'          COD
178700040217     C                   PARM      *blanks       KEY
178800040217     C                   PARM      *blanks       §DES             30
178900940715     C                   MOVEL     KEY           V1CTSP
179000940715     C                   SETON                                        90
179100940715     C                   MOVEL     'S'           WRIEM
179200940715     C                   ENDIF
179300940715     C*
179400090604     c                   clear                   ds5e
179500940715     C                   MOVEL     '5E'          COD
179600940715     C                   MOVEL     *BLANKS       KEY
179700940715     C                   MOVEL     V1CTSP        KEY
179800940715     C     KTAB1         CHAIN     TABEL00F                           43
179900941014     C     *IN43         IFEQ      *ON
180000941014     C     *IN43         OREQ      *OFF
180100950221     C     TBLFLG        ANDNE     ' '
180200940715     C                   MOVEL     MSG(16)       V1CMSG
180300941014     C                   SETON                                        284390
180400940715     C                   GOTO      ENDCTR
180500941014     C                   ELSE
180600090604     C                   MOVEL     TBLUNI        ds5e
180700131114     C                   MOVEL     §5ed08        V01DTSP
180800991222     C                   ENDIF
180900940715     C                   ENDIF
181000040216      *
181100991222     c*
181200991222     c* Se richiesta linea di arrivo dpd il riferimemnto immesso deve
181300060613     c*  essere composto soltanto da numeri se <=11
181400060613     C*  alfanumerico       se=14
181500050121     c                   clear                   wnodpd            1
181600060613    1c                   if        lnantw = 'DPD' or lnpntw = 'DPD' or
181700050304     c                             lnpntw = *blanks or lnantw = *blanks
181800060613    2c                   if        v1crpe<>*blanks
181900060613    3c                   select
182000060613     c* ok se lungo 14
182100060613     c                   when      %subst(v1crpe:15:1)=' '  and
182200060613     c                             %subst(v1crpe:14:1)<>' '
182300060613     c* ok se lungo 11 ma tutto numerico
182400060613     c                   when      %subst(v1crpe:12:4)=*blanks and
182500060613     c                             %subst(v1crpe:11:1)<>' '
182600060613     C                   MOVEA     V1CRPe        K11
182700060613     C                   Z-ADD     1             YY                2 0
182800060613     c
182900060613    4C                   DO        11            YY
183000060613     C     K11(YY)       LOOKUP    CNM                                    30
183100060613    5C     *IN30         IFEQ      *OFF
183200060613     c* Se non è tutto numerico non è DPD
183300060613     c                   eval      wnodpd = '1'
183400060613     c                   leave
183500060613    5c                   endif
183600060613    4c                   enddo
183700060613   x3c                   other
183800060613     c
183900060613     c* se non è lungo 11 o 14 non è DPD
184000060613     c                   eval      wnodpd = '1'
184100060613    3c                   endsl
184200060613     c
184300060613    3c                   if        wnodpd='1'
184400060613    4c     lnantw        ifeq      'DPD'
184500060613     c     lnpntw        oreq      'DPD'
184600060613     C                   MOVEL     MSG(18)       V1CMSG
184700060613     C                   SETON                                        285890
184800060613     C                   GOTO      ENDCTR
184900060613    4C                   ENDIF
185000060613    3C                   ENDIF
185100060613    2c                   endif
185200061117   x1c                   else
185300061117     c                   eval      wnodpd = '1'
185400060613    1c                   endif
185500941014     C*
185600911023     C** FILE STORICO
185700940713     C     V1CSSP        COMP      '1'                                    18
185800940713     C     V1CSSP        COMP      '2'                                    19
185900940713     C     V1CSSP        COMP      '3'                                    20
186000940713     C     V1CSSP        COMP      ' '                                    21
186100920911     C*
186200940712      * DEVO CONSIDERARE DELLE PRIORITA' PER LE SELEZIONI IMMESSE DALL'UTENTE:
186300010508      *    1°    NUMERO O.R.M.           ( 25 ON)
186400010508      *    2°    MITTENTE                ( 11 ON )
186500010508      *    3°    DESTINATARIO            ( 14 ON )
186600010508      *    4°    LINEA DI PARTENZA       ( 02 ON )
186700010508      *    5°    LINEA DI ARRIVO         ( 01 ON )
186800940712      *
186900010508     C   25              setoff                                       111402
187000010508     C   25              setoff                                       01
187100131113     C   14              SETOFF                                       020111
187200131113     C   11              SETOFF                                       0201
187300940712     C   02              SETOFF                                       01
187400991222     c*se lna dpd e immesso riferimento partner estero, se meno di 12
187500060518     c*  caratteri, calcolo il 12 --> ora non più: il check digit per la
187600060518     c*  ricerca non serve più
187700991222     C                   CLEAR                   krpe
187800050121    1c                   if        v1crpe <> *blanks and (lnantw='DPD'
187900050304     c                             or lnpntw = 'DPD' or wnodpd=' ')
188000060613     C                   movel     V1CRpe        krpe             14
188100991222     C                   ENDIF
188200911023     C*
188300900514     C     ENDCTR        ENDSR
188400020628      *------------------------------------------------------------------------*
188500020628      *    Controlla i dati nazione e provincia destinatario
188600020628      *------------------------------------------------------------------------*
188700020628     c     ContrDatiDes  begsr
188800020628     c                   if        v1cnzd = *blanks and v1cprd = *blanks
188900020628     c                   eval      v1cmsg = msg(23)
189000020628     c                   eval      *in57 = *on
189100020628     c                   eval      *in28 = *on
189200020628     c                   goto      EndContr
189300020628     c                   endif
189400020628      * ricerca tabella nazione
189500020628     c     '?'           scan      v1cnzd                                 90
189600020628     c                   movel     '15'          cod
189700020628     c                   if        *in90
189800020628     c                   call      'TRUL19R'
189900020628     c                   parm                    cod
190000020628     c                   parm                    ordina
190100020628     c                   parm                    key
190200020628     c                   parm                    comando
190300020628     c                   movel     key           v1cnzd
190400020628     c                   eval      *in57 = *on
190500020628     c                   movel     'S'           wriem
190600020628     c                   goto      EndContr
190700020628     c                   endif
190800020628      * controllo nazione destinatario
190900020628     c                   clear                   ds15
191000020628     c                   movel     '15'          cod
191100020628     c                   clear                   key
191200020628     c                   movel     v1cnzd        key
191300020628     c     ktab1         chain     tabel00f
191400020628     c                   if        not %found(tabel00f) or tblflg <> *blanks
191500020628     c                   eval      v1cmsg = msg(24)
191600020628     c                   eval      *in57 = *on
191700020628     c                   eval      *in28 = *on
191800020628     c                   goto      EndContr
191900020628     c                   endif
192000020628     c                   movel     tbluni        ds15
192100060613     c* Se cambiata nazione, clearo forzatura msg se nazine estera
192200060613     c                   if        v1cnzd<>savnzd
192300060613     c                   clear                   wforz1            1
192400060613     c                   eval      savnzd=v1cnzd
192500060613     c                   endif
192600020628      * per nazione italia la provincia è obbligatoria
192700020628     c                   if        §15ita = 'I' and v1cprd = *blanks
192800020628     c                   eval      v1cmsg = msg(25)
192900020628     c                   eval      *in50 = *on
193000020628     c                   eval      *in28 = *on
193100020628     c                   goto      EndContr
193200020628     c                   endif
193300020628      * ricerca tabella provincia
193400020628     c     '?'           scan      v1cprd                                 90
193500020628     c                   movel     'PR'          cod
193600020628     c                   if        *in90
193700020628     c                   call      'TRUL19R'
193800020628     c                   parm                    cod
193900020628     c                   parm                    ordina
194000020628     c                   parm                    key
194100020628     c                   parm                    comando
194200020628     c                   movel     key           v1cprd
194300020628     c                   eval      *in50 = *on
194400020628     c                   movel     'S'           wriem
194500020628     c                   goto      EndContr
194600020628     c                   endif
194700020628      * controllo provincia destinatario
194800020628     c                   if        v1cprd <> *blanks and §15ita = 'I'
194900020628     c                   clear                   dspr
195000020628     c                   movel     'PR'          cod
195100020628     c                   clear                   key
195200020628     c                   movel     v1cprd        key
195300020628     c     ktab1         chain     tabel00f
195400020628     c                   if        not %found(tabel00f) or tblflg <> *blanks
195500020628     c                   eval      v1cmsg = msg(25)
195600020628     c                   eval      *in50 = *on
195700020628     c                   eval      *in28 = *on
195800020628     c                   goto      EndContr
195900020628     c                   endif
196000020628     c                   movel     tbluni        dspr
196100020628      * se nazione italia la provincia deve essere italia
196200020628      * nella tabella ci sono anche le provincie estere e per sapere se ok
196300020628      * faccio il controllo con il codice tassazione per estero
196400020628      * se vuoto vuol dire che è provincia estera
196500020628     c                   if        §15ita = 'I' and §prcte = *blanks
196600020628     c                   eval      v1cmsg = msg(25)
196700020628     c                   eval      *in50 = *on
196800020628     c                   eval      *in28 = *on
196900020628     c                   goto      EndContr
197000020628     c                   endif
197100020628     c                   endif
197200020628
197300020628     c     EndContr      endsr
197400940318      **************************************************************************
197500940318      *    ELABORA BOLLE DA VISUALIZZARE
197600940318      **************************************************************************
197700900515     C     ELAB          BEGSR
197800940318      *
197900131114     C                   SETON                                        35
198000131114     C                   WRITE     LRI6C02
198100131114     C                   SETOFF                                       35
198200131114     C*
198300940713     C                   Z-ADD     1             REC2
198400911024     C                   Z-ADD     1             C
198500131111     c                   eval      wEnd = *off
198600131115     c                   clear                   readFile          3
198700131113     c
198800131113     C                   Z-ADD     COMAAD        $AAS
198900131113     C                   Z-ADD     COMMGD        $MGS
199000131111     c
199100131111    1c                   select
199200010508     c* O.R.M.
199300131111    1c     *in25         wheneq    *on
199400010508     C                   MOVEL     'M'           WTRC
199500010508     C                   MOVEL(P)  SEL4          WN14             14
199600060216     C     Kar44         SETLL     FiAR404L
199700131111     c
199800131111     c***
199900131111     C*  RIFERIMENTO P A R T N E R   E S T E R O
200000131111     c***
200100131111    1C     V1CRPE        whenne    *BLANKS
200200050121     c                   clear                   wgiro             1
200300050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
200400971223     C                   MOVEL(P)  V1CRPE        WN30             30
200500991222     C                   MOVEL     'E'           WTRC
200600991222     c                   else
200700991222     c* D P D
200800991222     C                   MOVEL(P)  krpe          WN30             30
200900060518     C***                MOVEL     'F'           WTRC
201000060518     C                   MOVEL     'I'           WTRC
201100991222     c                   endif
201200991222     c**
201300060216     C     KAR4          SETLL     FiAR403L
201400991222     c**
201500131111     c* C H I   S O N O
201600131111     c**
201700131111    1C     V1CNOT        whenne    *BLANKS
201800131111     C     KarS          SETLL     FIarS02L
201900131111     c* R N N
202000131111    1c     v1crmn        whengt    *zeros
202100110930     c     v1crmn        setll     fnarb33l
202200131115     c                   eval      Readfile='DSP'
202300131111     c* R M A
202400131111    1c     comrma        whenne    *blanks
202500111005     c     comrma        setll     fnarb32l
202600131115     c                   eval      Readfile='DSP'
202700131111     c
202800131113      * RSD/RSM/IND/LOD o solo NAZ/PRV       -----------------------------------
202900110930     c
203000131113     c                   when      *in14  OR *IN11
203100131111     c                   exsr      Presql
203200131111     c                   exsr      Opencursor
203300131111   x1c                   other
203400131111     c
203500971223      *
203600940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1)  -----------------------------------
203700971223    2C   18              DO
203800940713     C     V1CLNA        IFNE      *ZERO
203900940713     C                   Z-ADD     V1CLNA        COMLNA
204000940712     C                   ELSE
204100940712     C                   Z-ADD     L1(1)         COMLNA
204200940712     C                   ENDIF
204300020910     C     K1ARB         SETLL     FNARB78L
204400131115     c                   eval      Readfile='DSP'
204500971223    2C                   ENDDO
204600940318      *
204700940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
204800971223    2C   19              DO
204900940713     C     V1CLNA        IFNE      *ZERO
205000120330     C     V1CLNA        SETLL     FNARB05L
205100940712     C                   ELSE
205200120330     C     *LOVAL        SETLL     FNARB05L
205300940712     C                   ENDIF
205400971223    2C                   ENDDO
205500940318      *
205600940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
205700971223    2C   20              DO
205800940713     C     V1CLNA        IFNE      *ZERO
205900940713     C                   Z-ADD     V1CLNA        COMLNA
206000940712     C                   ELSE
206100940712     C                   Z-ADD     L1(1)         COMLNA
206200940712     C                   ENDIF
206300940713     C     K4ARB         SETLL     FNARB54L
206400131115     c                   eval      Readfile='DCM'
206500971223    2C                   END
206600940318      *
206700940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
206800971223    2C   21              DO
206900131113     c
207000131113     c* per LINEA DI PARTENZA
207100131113    3C     *IN02         IFEQ      *ON
207200950406     C                   SELECT
207300950406     C* Serie = 0 e segnacollo dal diverso da 0 => i segnacolli sono
207400950406     C* sequenziali
207500950406     C     V1CPSD        WHENEQ    *ZEROS
207600950406     C     V1CSCD        ANDNE     *ZEROS
207700950406     C                   Z-ADD     V1CSCD        COMSCD
207800950406     C     K6ARB         SETLL     FNARB27L                               31
207900131115     c                   eval      Readfile='DSP'
208000950406     C     *IN31         IFEQ      *OFF
208100950406     C* Non trovata bolla col numero segnacollo specificato: il
208200950406     C* segnacollo non è quello iniziale della bolla e quindi è sicura-
208300950406     C* mente incluso nella spedizione precedente.
208400950406     C     K6ARB1        READPE    FNARB27L                               31
208500131209     C  n31              Z-ADD     ARBNCD        COMSCD
208600950406     C     K6ARB         SETLL     FNARB27L
208700950406     C                   END
208800131113     c
208900950406     C* Non immesso segnacollo dal oppure serie diversa da zero (segna-
209000950406     C* colli non sequenziali) mi posiziono con segnacollo = 0
209100950406     C                   OTHER
209200950406     C                   MOVE      *ZEROS        COMSCD
209300950405     C     K6ARB         SETLL     FNARB27L
209400131115     c                   eval      Readfile='DSP'
209500950406     C                   ENDSL
209600131113    3C                   END
209700131113     c*
209800131113     c* Per LINEA DI ARRIVO
209900940712     C   01K8ARB         SETLL     FNARB26L
210000131115     c                   eval      Readfile='DSP'
210100131111     c
210200971223    2C                   ENDDO
210300131111    1C                   ENDSL
210400940318      *
210500911023     C                   Z-ADD     0             COMOD
210600911023     C                   EXSR      CARSF
210700940318      *
210800901116     C                   ENDSR
210900940318      **************************************************************************
211000940318      *    CARICO SUBFILE IN AVANTI
211100940318      **************************************************************************
211200900515     C     CARSF         BEGSR
211300940318      *
211400900518     C                   Z-ADD     1             B                 2 0
211500940713     C                   Z-ADD     COMOD         NR2
211600000228     C                   clear                   War4AAS
211700000228     C                   clear                   War4LNP
211800000228     C                   clear                   War4NRS
211900000228     C                   clear                   War4NSP
212000000228      *
212100131113    1C     B             DOWLE     18
212200060216     c                   setoff                                       32
212300131111
212400131113    2c                   select
212500131111     c***
212600131111     c* O R M
212700131111     c***
212800131111     c     *in25         wheneq    *on
212900060216     c     kar44         reade     fiar404l                               30
213000010509     C  N30KARB4         CHAIN     FNARB01L                           32
213100131111     C***
213200131111      * R I F .   p a r t n e r
213300131111     C***
213400131111     C     V1CRPE        whenne    *BLANKS
213500050121     c     leggar4       tag
213600000228      *
213700050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
213800050304     c                             and wgiro <> '2'
213900060216     C     KAR4          READE     FiAR403L                               30
214000991222     c                   else
214100000228      * DPD
214200060216     C     wtrc          READE     FiAR403L                               30
214300000228     C                   If        *in30 = *off
214400060518     c                   movel     ar4n30        dsbl4i
214500000228      *
214600000228     c                   Select
214700000228      * Se non trovo n.parcel e selezionati 0 rcd o più di 1: fine lettura
214800060613     c                   When       §b4ipn <> krpe  and  nr2 <> 1
214900991222     C                   SETON                                            30
215000000228      * Se non trovo n.parcel e è selezionato 1 rcd:  fine lettura e
215100060216      *                                               riposiziono FiAR4
215200060613     c                   When       §b4ipn<> krpe
215300000228     C                   SETON                                            30
215400060216     C     KAR4W         CHAIN     FiAR401L
215500000228      * Se ho trovato il n.parcel ed è il 1o memorizzo key sped
215600000228     c                   When       NR2 = *zeros
215700000228     C                   Z-ADD     AR4AAS        War4AAS
215800000228     C                   Z-ADD     AR4LNP        War4LNP
215900000228     C                   Z-ADD     AR4NRS        War4NRS
216000000228     C                   Z-ADD     AR4NSP        War4NSP
216100000228     c                   Endsl
216200000228     c                   Endif
216300000228      *
216400991222     c                   endif
216500991222     c
216600050304     c                   if        *in30 and wgiro <> '2' and
216700050304     c                             lnpntw <> 'DPD' and lnantw <> 'DPD' and
216800050304     c                             wnodpd = *blanks
216900050121     c                   eval      wgiro = '2'
217000060518     C                   MOVEL     'I'           WTRC
217100050121     C                   MOVEL(P)  KRPe          WN30             30
217200060216     C     Kar4          SETLL     Fiar403l
217300050121     c                   goto      leggar4
217400050121     c                   endif
217500061117     c*
217600061117     c* Se rif.partner può essere DPD non export e immesso rif. lungo 11
217700061117     c* faccio tentaivo di ricerca con rif.truccato
217800061117     c                   if        *in30 and lnantw<>'DPD'and wnodpd=*blanks
217900061117     c                             and %subst(krpe:12:1)=' '
218000061117     C                   eval      krpe='0'+%subst(krpe:1:3)+'99'
218100061117     c                             +%subst(krpe:4:8)
218200061117     C                   MOVEL(P)  KRPe          WN30             30
218300061117     C     Kar4          SETLL     Fiar403L
218400061117     c                   goto      leggar4
218500061117     c                   endif
218600050121     c*
218700971223     C  N30KARB4         CHAIN     FNARB01L                           32
218800131111     c**
218900131111     c* C H I   S O N O
219000131111     c**
219100131111    1C     V1CNOT        whenne    *BLANKS
219200131111     C     KarS          reade     FIarS02L                               30
219300131111     c                   if        not *in30
219400131111     C     KarSXT        CHAIN     FNarT27L                           32
219500131111     C  N32KARTXP        CHAIN     FNarb01L                           32
219600131111    3C     *IN32         IFEQ      *On
219700131111     C                   SETON                                        30
219800131111    3C                   ENDIF
219900131111    2C                   ENDIF
220000110930     c*
220100131111     c* Interrogazione per R M N
220200131111    1C     v1crmn        whengt    *zeros
220300111003     c     v1crmn        reade     fnarb33l                               30
220400940318      *
220500131111     c* Interrogazione per R M A
220600131111    1C     comrma        whenne    *BLANKS
220700111005     c     comrma        reade     fnarb32l                               30
220800131111     c
220900131111      * RSD/RSM/IND/LOD                      -----------------------------------
221000131111     c
221100131113     c                   when      *IN14 OR *IN11
221200131113
221300131111     c                   if        Wend=*off
221400131111     c                   exsr      Readcursor
221500131111     c                   endif
221600131111     c
221700131111     c                   if        Wend=*on
221800131111     c                   seton                                        30
221900131111     c                   endif
222000111005     c*
222100131111     C                   other
222200940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1')  ----------------------------------
222300940318     C   18              DO
222400940713     C     V1CLNA        IFNE      *ZERO
222500020910     C   15K2ARB         READE     FNARB78L                               30
222600020910     C  N15K3ARB         READE     FNARB78L                               30
222700940712      *
222800940712     C                   ELSE
222900940712      *
223000950214     C                   SETON                                            31
223100950214     C                   SETOFF                                           30
223200950214     C     *IN31         DOWEQ     *ON
223300020910     C   15K2ARB         READE     FNARB78L                               31
223400020910     C  N15K3ARB         READE     FNARB78L                               31
223500940318     C   31              ADD       1             C
223600940318     C   31L1(C)         IFNE      0
223700940318     C                   Z-ADD     L1(C)         COMLNA
223800020910     C     K1ARB         SETLL     FNARB78L
223900940318     C                   ELSE
224000950214     C                   SETOFF                                       31
224100940318     C                   SETON                                        30
224200940318     C                   ENDIF
224300940318     C                   ENDDO
224400940318      *
224500940712     C                   ENDIF
224600940318     C                   ENDDO
224700940318      *
224800940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
224900940712     C   19              DO
225000940713     C     V1CLNA        IFNE      *ZERO
225100120330     C     V1CLNA        READE     FNARB05L                               30
225200940712     C                   ELSE
225300120330     C                   READ      FNARB05L                               30
225400940712     C                   ENDIF
225500940712     C                   ENDDO
225600940318      *
225700940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
225800940318     C   20              DO
225900940713     C     V1CLNA        IFNE      *ZERO
226000940712     C   15K5ARB         READE     FNARB54L                               30
226100940713     C  N15V1CLNA        READE     FNARB54L                               30
226200940712      *
226300940712     C                   ELSE
226400940712      *
226500940712     C                   SETON                                            31
226600940712     C     *IN31         DOWEQ     '1'
226700940712     C  N36
226800940712     CAN 15K5ARB         READE     FNARB54L                               31
226900940712     C  N36
227000940712     CANN15COMLNA        READE     FNARB54L                               31
227100060216     C  N31              SETOFF                                       30
227200980120     C   31              ADD       1             C
227300940318     C   31L1(C)         IFNE      0
227400940318     C                   Z-ADD     L1(C)         COMLNA
227500940712     C     K4ARB         SETLL     FNARB54L
227600940318     C                   SETOFF                                       36
227700940318     C                   ELSE
227800940318     C                   SETON                                        30
227900940318     C                   SETOFF                                       31
228000940318     C                   ENDIF
228100940318     C                   ENDDO
228200940318      *
228300940712     C                   ENDIF
228400940318     C                   ENDDO
228500940318      *
228600940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
228700940318     C   21              DO
228800940712     C   01
228900940712     CAN 15K9ARB         READE     FNARB26L                               30
229000940713     C   01
229100940713     CANN15V1CLNA        READE     FNARB26L                               30
229200940712     C   02
229300940712     CAN 15K7ARB         READE     FNARB27L                               30
229400940713     C   02
229500940713     CANN15V1CLP1        READE     FNARB27L                               30
229600131113     c                   ENDDO
229700131113     c                   endsl
229800940318      *
229900060216     c  n32
230000060216     CanN30              EXSR      CONFR
230100900515     C  N30
230200060216     CANN32              EXSR      EMISS
230300900515     C  N30              END
230400131113     c
230500940713     C                   Z-ADD     NR2           COMOD             4 0
230600930518     C                   SETOFF                                       1362
230700131113     c
230800131209     c** 30              if        Wend=*on
230900131209     c   30              if        WopenC='1'
231000131113     c                   exsr      closecursor
231100131113     c                   endif
231200940318      *
231300900515     C                   ENDSR
231400940317      **************************************************************************
231500940317      *    CONFRONTO PER CERCARE BOLLE DA EMETTERE
231600940317      **************************************************************************
231700900515     C     CONFR         BEGSR
231800940317      *
231900911024     C                   SETOFF                                       3236
232000131111     c                   z-add     arbmgs        arbdsp
232100131111     c                   movel     arbaas        arbdsp
232200131111     c
232300110930     c* Se richiamata per Perfect Order devo scartare la bolla se xco diverso da
232400110930     c* quello passato
232500110930     c                   if        pa1flg='W' and arbxco<>pa1xco
232600110930     c                   seton                                        32
232700110930     c                   goto      endcon
232800110930     c                   endif
232900940318      *
233000940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1')  ----------------------------------
233100940712      * DEVO CONTROLLARE LA DATA SPEDIZIONE (ARBDSP)
233200971223    1C   18              DO
233300131115     c                   if        comdtd>0 and *in15
233400131115    2C     ARBDSP        IFNE      COMDTD
233500940713     C     ARBDCM        ANDEQ     *ZERO
233600131115     c
233700131115    3C                   IF        v1clna<>0 and readfile='DSP'
233800940712     C                   SETON                                            30
233900131115     c                   endif
234000131115
234100131115     C                   SETON                                            32
234200911023     C                   GOTO      ENDCON
234300971223    2C                   END
234400131115     c                   endif
234500940318      *
234600131115     c                   if        comdtd>0 and not *in15
234700131115    2C     ARBDSP        IFGT      COMDTA
234800940713     C     ARBDCM        ANDEQ     *ZERO
234900131115     c* si potrebe uscire anche se lettura per riferimenti mittente
235000131115     c*  ma se la linea di arrivo =0 non lo fa ora .... si può sistemare
235100131115     c*  per far leggere meno
235200131115    3C                   IF        v1clna<>0 and readfile='DSP'
235300131115     C                   SETON                                            30
235400131115     c                   endif
235500131115
235600940712     C                   SETON                                            32
235700911023     C                   GOTO      ENDCON
235800971223    2C                   END
235900131115     c                   endif
236000131115     c
236100131115    1C                   END
236200940318      *
236300940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
236400940712      * DEVO CONTROLLARE LA DATA DISTINTA (ARBDDC)
236500971223    1C   19              DO
236600960527     C     ARBFBC        IFNE      *BLANKS
236700960527     C                   SETON                                        32
236800960527     C                   GOTO      ENDCON
236900960527     C                   END
237000960527     C*
237100131115     c                   if        comdtd>0 and *in15
237200131115     C     ARBDDC        IFNE      COMDTD
237300911024     C                   SETON                                            32
237400911024     C                   GOTO      ENDCON
237500911024     C                   END
237600131115     C                   ENDif
237700940318      *
237800131115     c                   if        comdtd>0 and not *in15
237900131115     C     ARBDDC        IFLT      COMDTD
238000940712     C     ARBDDC        ORGT      COMDTA
238100911024     C                   SETON                                            32
238200911024     C                   GOTO      ENDCON
238300911024     C                   END
238400131115     C                   ENDif
238500940318      *
238600940321      * SE NON C'E' NUMERO DISTINTA NON E' IN CONSEGNA
238700911024     C     ARBNDC        IFEQ      0
238800911024     C                   SETON                                            32
238900911024     C                   GOTO      ENDCON
239000911024     C                   END
239100971223    1C                   END
239200940318      *
239300940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
239400940712      *  DEVO CONTROLLARE LA DATA CONSEGNA (ARBDCM)
239500971223    1C   20              DO
239600131115     c                   if        comdtd>0 and *in15
239700131115    2C     ARBDCM        IFNE      COMDTD
239800131115    3C                   IF        v1clna<>0 and readfile='DCM'
239900131115     C                   SETON                                            30
240000131115     c                   endif
240100131115
240200940712     C                   SETON                                            32
240300911024     C                   GOTO      ENDCON
240400971223    2C                   END
240500131115     c                   endif
240600940318      *
240700131115     c                   if        comdtd>0 and not *in15
240800131212     C     ARBDCM        IFLT      COMDTD
240900131212     C                   SETON                                            32
241000131212     C                   GOTO      ENDCON
241100131212     C                   END
241200131212     c
241300131115    2C     ARBDCM        IFGT      COMDTA
241400131115    3C                   IF        v1clna<>0 and readfile='DCM'
241500131115     C                   SETON                                            30
241600131115     c                   endif
241700131115
241800940712     C                   SETON                                            32
241900911024     C                   GOTO      ENDCON
242000971223    2C                   END
242100131115     c                   endif
242200131115     c
242300131115      * SE NON C'E' data consegna  NON E' consegnata
242400131115     C     ARBDCM        IFEQ      0
242500131115     C                   SETON                                            32
242600131115     C                   GOTO      ENDCON
242700131115     C                   ENDIF
242800131115    1C                   END
242900940329      * ------------------------------------------------------------------------
243000940712      * HO SPOSTATO IL CONFRONTO CON MITTENTE E DESTINATARIO PRIMA DEL BLOCCO
243100940712      * CON 21 ON PERCHE' POSSO ACCENDERE IL 30 E ANDARE A FINE CICLO QUANDO
243200940712      * CAMBIA LA RAG.SOCIALE (EVITO DELLE LETTURE)
243300940712      *
243400940712      * OCCHIO!!   Devo usare per primo il confronto che potrebbe portarmi a
243500940712      * fine ciclo: se interrogo per destinatario (14ON) ci deve essere prima il
243600940713      * confronto con V1CRSD, se invece sono per mittente devo fare prima quello
243700940713      * con V1CRSM.
243800060613     C                   MOVE      *BLANKS       $SALTA            1
243900940712      *
244000940712      *??  R A G I O N E    S O C I A L E    D E S T I N A T A R I O  ?
244100131113     c                   if        not *in11 and not *in14
244200131113    1C     V1CRSD        IFNE      *BLANKS
244300131113     c
244400131113    2c     v1cxrsd       ifeq      ' '
244500131113     C     ZZ            SUBST(P)  ARBRSD        COMRAG
244600940712      * Se rag.soc. letta > video riposiziono solo per data con SETGT
244700131113    3C     COMRAG        IFNE      V1CRSD
244800131113     C                   MOVE      'S'           $SALTA
244900131113    3C                   ENDIF
245000131113     c*
245100131113   x2c                   else
245200131114     C                   MOVEL     ARBAAS        KAAS
245300131114     C                   MOVEL     ARBLNP        KLNP
245400131114     C                   MOVEL     ARBNRS        KNRS
245500131114     C                   MOVEL     ARBNSP        KNSP
245600131114     C                   MOVEL     'D'           KTRC
245700131114     C     KAR41         CHAIN     FiAR401L
245800131114     c                   if        %found(fiar401l)
245900131114     c                   eval      arbrsduni=arbrsd+ar4not
246000131114     c                   else
246100131114     c                   eval      arbrsduni=arbrsd
246200131114     c                   endif
246300131114     c
246400131114    3c                   if        %scan(%trimr(v1crsd):arbrsdUNI) = 0
246500131113     C                   MOVE      'S'           $SALTA
246600131113    3c                   endif
246700131113    2C                   ENDIF
246800131113    1C                   ENDIF
246900131113     c
247000131113      *??  I N D I R I Z Z O     D E S T I N A T A R I O  ?
247100131113    1C     V1Cind        IFNE      *BLANKS
247200131113     c
247300131113    2c     v1cxind       ifeq      ' '
247400131113     C     DD            SUBST(P)  ARBind        COMRAG           30
247500131113      * Se rag.soc. letta > video riposiziono solo per data con SETGT
247600131113    3C     COMRAG        IFNE      V1Cind
247700131113     C                   MOVE      'S'           $SALTA
247800131113    3C                   ENDIF
247900131113     c*
248000131113   x2c                   else
248100131113    3c                   if        %scan(%trimr(v1cind):arbind) = 0
248200131113     C                   MOVE      'S'           $SALTA
248300131113    3c                   endif
248400131113    2C                   ENDIF
248500131113    1C                   ENDIF
248600131113      *??  L O C A L I T A'      D E S T I N A T A R I O  ?
248700131113    1C     V1Clod        IFNE      *BLANKS
248800131113     c
248900131113    2c     v1cxlod       ifeq      ' '
249000131113     C     LL            SUBST(P)  ARBlod        COMRAG
249100131113      * Se rag.soc. letta > video riposiziono solo per data con SETGT
249200131113    3C     COMRAG        IFNE      V1Clod
249300131113     C                   MOVE      'S'           $SALTA
249400131113    3C                   ENDIF
249500131113     c*
249600131113   x2c                   else
249700131113    3c                   if        %scan(%trimr(v1clod):arblod) = 0
249800131113     C                   MOVE      'S'           $SALTA
249900131113    3c                   endif
250000131113    2C                   ENDIF
250100131113    1C                   ENDIF
250200940414      *
250300940712      *??  R A G I O N E    S O C I A L E    M I T T E N T E  ?
250400131113    1C     V1CRSM        IFNE      *BLANKS
250500131113    2c     v1cxrsm       ifeq      ' '
250600940712     C     XX            SUBST(P)  ARBRSM        COMRAG
250700940712      * Se rag.soc. letta > video riposiziono solo per data con SETGT
250800131113    3C     COMRAG        IFne      V1CRSM
250900940712     C                   MOVE      'S'           $SALTA
251000131113    3C                   ENDIF
251100131113   x2c                   else
251200131113    3c                   if        %scan(%trimr(v1crsm):arbrsm) = 0
251300131113     C                   MOVE      'S'           $SALTA
251400131113    3c                   endif
251500131113    2C                   ENDIF
251600131113    1C                   ENDIF
251700131113    1C                   ENDIF
251800940712      *
251900131115     c                   if        $SALTA='S'
252000131115     c                   seton                                        32
252100131115     C                   goto      ENDCON
252200131115     c                   endif
252300940318      *
252400940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
252500971223    1C   21              DO
252600940712     C     ARBDSP        IFLT      COMDTD
252700940712     C     COMDTD        ANDNE     *ZERO
252800940712     C                   SETON                                            32
252900911024     C                   GOTO      ENDCON
253000911024     C                   END
253100940318      *
253200940321     C     ARBDSP        IFGT      COMDTA
253300940712     C     COMDTA        ANDNE     *ZERO
253400940712     C                   SETON                                            32
253500131115     c                   if        readfile='DSP'
253600131115     C                   SETON                                            30
253700131115     c                   endif
253800131115     c
253900911024     C                   GOTO      ENDCON
254000131115     C                   ENDif
254100131115     c
254200971223    1C                   ENDDO
254300971223     C**
254400940317      *
254500940317      * PER OGNI SELEZIONE  ----------------------------------------------------
254600940317      *
254700940317      * TESTO LA LINEA DI ARRIVO
254800971223     C     V1CRPE        IFEQ      *BLANKS
254900971223     C     ARBLNA        ANDNE     V1CLNA
255000940713     C     V1CLNA        ANDNE     *ZERO
255100920911     C                   SETON                                            32
255200920911     C                   GOTO      ENDCON
255300971223     C                   ENDIF
255400020628      * testo la nazione/provincia del destinatario
255500060613     c                   if        v1cprd <> *blanks  and
255600020628     c                             (arbnzd <> v1cnzd  or arbprd <> v1cprd)
255700020628     c                   eval      *in32 = *on
255800020628     c                   goto      endcon
255900020628     c                   endif
256000060613     c                   if        (v1cnzd <> *blanks and v1cprd = *blanks) and
256100060613     c                             arbnzd <> v1cnzd
256200060613     c                   eval      *in32 = *on
256300060613     c                   goto      endcon
256400060613     c                   endif
256500020628
256600940317      *
256700940712      * VERIFICO ANCHE LA LNP POICHE' POTREI AVER FATTO ALTRE SELEZIONI PRIMARIE
256800940713     C     ARBLNP        IFNE      V1CLP1
256900940713     C     V1CLP1        ANDNE     *ZERO
257000940712     C                   SETON                                            32
257100940712     C                   GOTO      ENDCON
257200940712     C                   END
257300940923     C* FILIALE SEGNACOLLI
257400940926     C* SE NON VI SONO COLLI, SALTARE QUESTO CONTROLLO
257500940926     C     ARBNCD        IFNE      *ZERO
257600940926     C     ARBNCA        ORNE      *ZERO
257700940926     C     ARBFNS        OREQ      'S'
257800940926     C     ARBFLS        ORNE      *ZERO
257900940923     C     V1CFLS        IFNE      *ZERO
258000940923     C     V1CFLS        ANDNE     ARBFLS
258100940923     C                   SETON                                            32
258200940923     C                   GOTO      ENDCON
258300940926     C                   ENDIF
258400940926     C                   ENDIF
258500961112     C* RESO / FRANCO FRONTIERA
258600961112     C     V1CFBR        IFNE      ' '
258700961112     C                   MOVEL     V1CFBR        WFBR
258800961112     C     V1CFBR        IFEQ      'R'
258900961112     C                   MOVEL     'S'           WFBR
259000961112     C                   ENDIF
259100961112     C     ARBFBR        IFNE      WFBR
259200961112     C                   SETON                                            32
259300961112     C                   GOTO      ENDCON
259400961112     C                   ENDIF
259500961112     C                   ENDIF
259600040216      * Tipo bolla
259700040216     c                   if            V1Ccbo <> *blanks
259800040216     c                             and V1Ccbo <> ARBcbo
259900040216     c                   eval      *in32 = *on
260000040216     c                   goto      EndCon
260100040216     c                   endif
260200900515     C* TIPO SPEDIZIONE
260300940715     C     ARBTSP        IFNE      V1CTSP
260400940715     C     V1CTSP        ANDNE     *BLANKS
260500900515     C                   SETON                                            32
260600900515     C                   GOTO      ENDCON
260700900515     C                   END
260800911023     C* SPEDIZIONI GIACENTI
260900980603     C     V1CGIA        IFEQ      'G'
261000930702     C     ARBFBC        ANDNE     'G'
261100911023     C                   SETON                                            32
261200911023     C                   GOTO      ENDCON
261300911023     C                   END
261400980603     C     V1CGIA        IFEQ      'N'
261500930702     C     ARBFBC        ANDEQ     'G'
261600911023     C                   SETON                                            32
261700911023     C                   GOTO      ENDCON
261800911023     C                   END
261900070420     c*
262000070420     c* SPEDIZIONI IN FERMO DEPOSITO
262100070420     C                   IF        v1csfd='S' and arbffd<>'S'
262200070420     C                   SETON                                            32
262300070420     C                   GOTO      ENDCON
262400070420     C                   END
262500070420     c
262600900518     C* SERIE DAL + AL
262700940713     C     ARBNRS        IFLT      V1CPSD
262800900518     C                   SETON                                            32
262900900518     C                   GOTO      ENDCON
263000900518     C                   END
263100940713     C     ARBNRS        IFGT      V1CPSA
263200900515     C                   SETON                                            32
263300900515     C                   GOTO      ENDCON
263400900515     C                   END
263500900518     C* SEGNACOLLO DAL SOLO
263600930702     C     *IN07         IFEQ      *ON
263700970925     C     *IN84         ANDEQ     *OFF
263800940713     C     KART          SETLL     FNART01L                               31
263900930702     C  N31              SETON                                        32
264000930702     C  N31              GOTO      ENDCON
264100930702     C                   END
264200900515     C* SEGNACOLLO DAL + AL
264300970925     C   84              DO
264400910827     C                   SETON                                        32
264500940713     C     KARB2         SETLL     FNART01L
264600940713     C     KARB2         READE     FNART01L                               31
264700910827     C     *IN31         DOWEQ     '0'
264800940713     C     ARTNSC        IFGE      V1CSCD
264900940713     C     ARTNSC        ANDLE     V1CSCA
265000910827     C                   SETON                                        31
265100910827     C                   SETOFF                                       32
265200910827     C                   END
265300910827     C*
265400940713     C  N31KARB2         READE     FNART01L                               31
265500910827     C                   END
265600910827     C*
265700910827     C   32              GOTO      ENDCON
265800910827     C                   END
265900990119     C*
266000990119     C* NUMERO CA
266100990119    1C     V1CDAN        IFNE      ' '
266200040809     C                   SETON                                        32
266300040809      *
266400040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
266500040809      * legate alla spedizione che passo
266600040809     c                   clear                   fidn12ds
266700040809     c                   eval      i12aas = arbAAS
266800040809     c                   eval      i12lnp = arbLNP
266900040809     c                   eval      i12nrs = arbNRS
267000040809     c                   eval      i12nsp = arbNSP
267100040809     c                   eval      i12fel = 'E'
267200040809     c                   eval      i12fan = 'E'
267300040809     c*
267400040809     c                   call      'FIDN12R'
267500040809     c                   parm                    fidn12ds
267600040809     c*
267700040809     c                   setoff                                       31
267800040809     c*
267900040809     c* se non ci sono errori
268000040809     c                   if        o12err <> ' '
268100040809     c                   seton                                        31
268200040809     c                   else
268300040809     c* se numero di CA trovate maggiore di zero
268400040809     c                   if        o12nca = 0
268500040809     c                   seton                                        31
268600040809     c                   else
268700040809     c                   z-add     1             ii                2 0
268800040809     c                   dow       ii <= o12nca and *in31 = *off
268900040809     C* APERTA
269000040809     C     skdch(ii)     IFEQ      0
269100040809     C     V1CDAN        ANDEQ     'A'
269200040809     C                   SETOFF                                       32
269300040809     C                   SETON                                        31
269400040809     C                   ENDIF
269500040809     C* CHIUSE
269600040809     C     skdch(ii)     IFGT      0
269700040809     C     V1CDAN        ANDEQ     'C'
269800040809     C                   SETOFF                                       32
269900040809     C                   SETON                                        31
270000040809     C                   ENDIF
270100040809     C* CA APERTA O CHIUSA
270200040809     C     V1CDAN        IFEQ      'E'
270300040809     C                   SETOFF                                       32
270400040809     C                   SETON                                        31
270500040809     C                   ENDIF
270600040809     c*
270700040809     c                   add       1             ii
270800040809     c                   enddo
270900040809     c*
271000040809     c                   seton                                        31
271100040809     c*
271200040809     c                   endif
271300040809     c                   endif
271400040809     c**
271500990119    3C                   ENDIF
271600030527     C*
271700940317     C     ENDCON        ENDSR
271800940802     C**************************************************************************
271900940802     C* RICHIAMO PGM DI INTERROGAZIONE PARTENZE
272000940802     C**************************************************************************
272100930922     C     INTPAR        BEGSR
272200940802     C*
272300940722     C                   MOVEL     'A'           PA2FLG
272400930922     C                   MOVEL     PARAM2        KPJBU
272500940722     C                   CALL      'FNLS04R'
272600930922     C                   PARM                    KPJBA
272700940802     C*
272800930922     C                   ENDSR
272900131114     C**************************************************************************
273000131114     C* Atre parzializzazioni
273100131114     C**************************************************************************
273200131114     C     AltrePar      BEGSR
273300131114     c     forw12        TAG
273400131114     c                   exfmt     LRI6W12
273500131114     c
273600131114     c                   setoff                                       285690
273700131114     c                   setoff                                       40
273800131114     c
273900131114     C* GIACENTI
274000131114     C* SCELTE INCONGRUENTI
274100131114     C     V1CGIA        IFEQ      'G'
274200131114     C     V1CSSP        IFEQ      '2'
274300131114     C     V1CSSP        OREQ      '3'
274400131114     C                   MOVEL     MSG(14)       w12MSG
274500131114     C                   SETON                                        285690
274600131114     C                   GOTO      forw12
274700131114     C                   END
274800131114     C                   END
274900131114      * Tipo bolla
275000131114if  1c                   if        V1Ccbo <> *blanks
275100131114     c                   eval      *in31  =  *off
275200131114     c     '?'           scan      V1Ccbo                                 31
275300131114if  2c                   if        *in31 = *on
275400131114     c                   call      'X§TABER'
275500131114     c                   parm      CodUt         §KUT
275600131114     c                   parm      '3A'          COD
275700131114     c                   parm      *blanks       KEY
275800131114     c                   parm      *blanks       §DES
275900131114     c                   movel     KEY           V1Ccbo
276000131114     c                   eval      *in90 = *on
276100131114     c                   goto      forw12
276200131114e   2c                   endif
276300131114      *
276400131114     c                   movel     '3A'          COD
276500131114     c                   movel(p)  V1Ccbo        KEY
276600131114     c     KTAB1         chain     TABEL
276700131114if  2c                   if        NOT %found(TABEL00F)
276800131114     c                             or  TBLflg <> *blanks
276900131114     c                   movel     Msg(9)        w12msg
277000131114     c                   seton                                        284090
277100131114     c                   goto      forw12
277200131114e   2c                   endif
277300131114e   1c                   endif
277400131114     c
277500131114     c                   if        v1cgia<>' ' or v1cdan<>' ' or v1caff<>'N'
277600131114     c                             or v1cfbr<>' ' or v1ccbo<>*blanks
277700131114     c                   seton                                        09
277800131114     c                   else
277900131114     c                   setoff                                       09
278000131114     c                   endif
278100131114     c
278200131114     C*
278300131114     C                   ENDSR
278400940802     C**************************************************************************
278500940802     C* EMISSIONE SUBFILE
278600940802     C**************************************************************************
278700900515     C     EMISS         BEGSR
278800940802     C*
278900930407     C                   SETOFF                                       1362
279000940714     C                   MOVEL     *BLANKS       V2CSCE
279100131121     c**                 if        *in87=*off and not *in02
279200131121     C**                 MOVEL     ARBLNP        V2CLIN
279300131121     c**                 endif
279400131121     c**                 if        *in87 or *in02
279500131121     C**                 MOVEL     ARBLNA        V2CLIN
279600131121     c**                 endif
279700940714     C                   MOVEL     ARBLNP        V2CLNP
279800941212     C                   MOVEL     ARBLNA        V2CLNA
279900911023     C                   MOVEL     ARBMGS        MMGG
280000940714     C                   MOVEL     GG            V2CDSP
280100940714     C                   MOVE      MM            V2CDSP
280200940714     C                   MOVE      ARBAAS        V2CAAS
280300950114     C                   MOVEL     ARBNSP        V2CNSP
280400900516     C*
280500060130     C                   MOVEL     ARBksc        V2Cksc
280600060130     C                   MOVEL     ARBcbo        V2Ccbo
280700060130     C                   MOVEL     ARBRSM        V2CRSM
280800940714     C                   MOVEL     ARBRSD        V2CRSD
280900131114     c                   if        v1cind=*blanks
281000131114     c                   eval      v2cdest=arblod
281100131114     c                   else
281200131114     c                   eval      v2cdest=arbind
281300131114     c                   endif
281400131114     c                   if        arbrma<>*blanks
281500131114     c                   eval      v2crifm=%trim(%editc(arbrmn:'Z')) +'/'+
281600131114     c                                     %trim(arbrma)
281700131114     c                   else
281800131114     c                   eval      v2crifm=%trim(%editc(arbrmn:'Z'))
281900131114     c                   endif
282000131114     C**                 Z-ADD     ARBRMN        V2CRMN
282100131114     C**                 MOVEL     ARBRMA        V2CRMA
282200940714     C                   MOVEL     ARBNRS        V2CNRS
282300940923     C                   MOVEL     ARBFLS        V2CFLS
282400940714     C                   MOVEL     *BLANKS       V2CFLA
282500990119     C* SPEDIZIONE IN DANNO
282600990119     C* SE MI HANNO CHIESTO LE CA CHIUSE VISUALIZZO IL FLAG "C"
282700990119     C* SE MI HANNO CHIESTO LE CA APERTE VISUALIZZO IL FLAG "*"
282800990119     C                   SELECT
282900990119     C     V1CDAN        WHENEQ    'C'
283000990119     C                   MOVEL     'C'           V2CDAN
283100990119     C     V1CDAN        WHENEQ    'A'
283200990119     C                   MOVEL     '*'           V2CDAN
283300990119     C**
283400990119     C                   OTHER
283500040809      *
283600040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
283700040809      * legate alla spedizione che passo
283800040809     c                   clear                   fidn12ds
283900040809     c                   eval      i12aas = arbAAS
284000040809     c                   eval      i12lnp = arbLNP
284100040809     c                   eval      i12nrs = arbNRS
284200040809     c                   eval      i12nsp = arbNSP
284300040809     c                   eval      i12fel = 'E'
284400040809     c                   eval      i12fan = 'E'
284500040809     c*
284600040809     c                   call      'FIDN12R'
284700040809     c                   parm                    fidn12ds
284800040809     c*
284900040809     c                   setoff                                       31
285000040809     c*
285100040809     c* se non ci sono errori
285200040809     c                   if        o12err <> ' '
285300040809     c                   seton                                        31
285400040809     c                   else
285500040809     c* se numero di CA trovate maggiore di zero
285600040809     c                   if        o12nca = 0
285700040809     c                   seton                                        31
285800040809     c                   else
285900040809     c                   z-add     1             ii                2 0
286000040809     c                   dow       ii <= o12nca and *in31 = *off
286100040809     C* SE NE TROVO UNA APERTA, DICO APERTA ALTRIMENTI CHIUSA
286200040809     C     skdch(ii)     IFEQ      0
286300040809     C                   MOVEL     '*'           V2CDAN
286400040809     C                   SETON                                            31
286500040809     C                   ELSE
286600040809     C* CHIUSA MA VADO AVANTI A LEGGERE
286700040809     C                   MOVEL     'C'           V2CDAN
286800040809     C                   ENDIF
286900040809     c*
287000040809     c                   add       1             ii
287100040809     c                   enddo
287200040809     c*
287300040809     c                   seton                                        31
287400040809     c*
287500040809     c                   endif
287600040809     c                   endif
287700990119     C**
287800990119     C                   ENDSL
287900921102     C* SEGNACOLLI
288000930702     C                   CLEAR                   SEG
288100921102     C                   MOVEL     ARBNCD        COMSCN            7
288200921102     C                   MOVEA     COMSCN        SEG(1)
288300921102     C     ARBNCA        IFNE      0
288400921102     C                   MOVEL     ARBNCA        COMSCN
288500921102     C                   MOVEA     COMSCN        SEG(9)
288600921102     C                   END
288700921102     C                   MOVEA     '/'           SEG(8)
288800921102     C* ELIMINO ZERO NON SIGNIFICATIVI
288900921102     C                   Z-ADD     1             Y
289000921102     C     SEG(Y)        DOWEQ     '0'
289100921102     C                   MOVE      ' '           SEG(Y)
289200921102     C                   ADD       1             Y
289300921102     C                   END
289400921102     C*
289500921102     C     ARBNCA        IFNE      0
289600921102     C                   Z-ADD     9             Y                 2 0
289700921102     C     SEG(Y)        DOWEQ     '0'
289800921102     C                   MOVE      ' '           SEG(Y)
289900921102     C                   ADD       1             Y
290000921102     C                   END
290100921102     C                   END
290200940714     C                   MOVEA     SEG           V2CSEG
290300911023     C*
290400911023     C* GIACENZA APERTA
290500941202     C                   Z-ADD     ARBAAS        KAAS
290600940713     C                   Z-ADD     ARBLNP        KLNP
290700940713     C                   Z-ADD     ARBNRS        KNRS
290800940713     C                   Z-ADD     ARBNSP        KNSP
290900050405     C     KGCA1         CHAIN     tigcp21l                           31
291000920410     C  N31GCPATB        IFEQ      ' '
291100920410     C     GCPDCG        IFNE      0
291200990119     C                   MOVE      'C'           V2CGCA
291300911023     C                   ELSE
291400990119     C                   MOVE      '*'           V2CGCA
291500900515     C                   END
291600920410     C                   END
291700990119     C                   MOVEL     ARBTSP        V2CTSP
291800930503     C*
291900911023     C* DATA CONSEGNA
292000940714     C                   MOVEL     *BLANKS       V2CCON
292100940714     C                   MOVEL     *BLANKS       V2CFCC
292200930407     C*
292300930407     C* SE ARBCCA >< 0  METTO DECODIFICA AL POSTO DELLA DATA CONSEGNA
292400930407     C     ARBCCA        IFNE      ' '
292500930705     C                   Z-ADD     1             C
292600930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
292700940909     C   31              MOVEL     DCA(C)        DS7O
292800940909     C*
292900940909     C* NELLA DS7O C'E' SCRITTO SE E' PRIORITARIA L'ANOMALIA O LA DATA
293000940909     C   31§7OVCA        IFEQ      'S'
293100940912     C     §7OVCA        ORNE      'S'
293200940912     C     ARBDCM        ANDEQ     *ZERO
293300940909     C                   MOVEL     §7ODEI        V2CCON
293400940909     C                   SETON                                        62
293500940912     C                   ELSE
293600940909     C*
293700940912     C     ARBDCM        IFNE      *ZERO
293800940909     C                   MOVE      ARBDCM        G02INV
293900940909     C                   Z-ADD     *ZEROS        G02DAT
294000940909     C                   MOVEL     '3'           G02ERR
294100940909     C                   CALL      'XSRDA8'
294200940909     C                   PARM                    WLBDAT
294300940909     C                   MOVEL     G02DAT        V2CCON
294400940909     C                   MOVE      G02DAT        $COMOD            2
294500940909     C                   MOVE      $COMOD        V2CCON
294600940909     C                   ENDIF
294700940912     C                   ENDIF
294800930407     C*
294900930407     C                   ELSE
295000940912     C     ARBDCM        IFNE      *ZERO
295100940712     C                   MOVE      ARBDCM        G02INV
295200940712     C                   Z-ADD     *ZEROS        G02DAT
295300940712     C                   MOVEL     '3'           G02ERR
295400940712     C                   CALL      'XSRDA8'
295500940712     C                   PARM                    WLBDAT
295600940714     C                   MOVEL     G02DAT        V2CCON
295700940713     C                   MOVE      G02DAT        $COMOD            2
295800940714     C                   MOVE      $COMOD        V2CCON
295900940912     C                   ENDIF
296000940912     C                   ENDIF
296100940912     C*
296200940912     C     ARBDCM        IFNE      0
296300930428     C*
296400930506     C* SE ARBDCE = 0  CALL PGM PER CALCOLO DELLA DATA CONSEGNA PREVISTA
296500930506     C*   E DELLE ORE DI RITARDO
296600940712      * SOLO SE RICHIESTA L'AFFIDABILITA'
296700940713     C     V1CAFF        IFEQ      'S'
296800930506     C     ARBDCE        IFEQ      0
296900991027     C                   CLEAR                   TNSD99DS
297000930506     C                   MOVEL     'A'           D98TBO
297100930506     C                   MOVEL     ARBAAS        D98AAS
297200930506     C                   MOVEL     ARBLNP        D98LNP
297300930506     C                   MOVEL     ARBNRS        D98NRS
297400930506     C                   MOVEL     ARBNSP        D98NSP
297500930506     C*
297600970925     C                   CALL      'TNSD99R'
297700991027     C                   PARM                    TNSD99DS
297800930506     C*
297900941214     C* SE PA098 = 1  E' STATA FATTA CALL PGM
298000941214     C                   MOVEL     '1'           PA098
298100930506     C*
298200960201     C* SE C'E' ERRORE VISUALIZZO UNA "R"
298300110301     C     D98ERR        IFNE      '0'
298400960201     C                   MOVEL     'R'           V2CFCC
298500960201     C                   ELSE
298600930506     C* SE D98CRC = S  LA SPEDIZ. E' STAT CONSEGNATA NEI TEMPI PREVISTI
298700930506     C     D98CRC        IFEQ      'S'
298800940714     C                   MOVEL     ' '           V2CFCC
298900930506     C                   ELSE
299000940714     C                   MOVEL     'F'           V2CFCC
299100930506     C                   END
299200960201     C                   END
299300930506     C*
299400930506     C                   ELSE
299500930506     C* ARBDCE > 0
299600940711     C* SE ARBNRC > 0  LA SPEDIZIONE NON E' CONFORME
299700940711     C     ARBNRC        IFGT      0
299800940714     C                   MOVEL     'F'           V2CFCC
299900930504     C                   ELSE
300000940714     C                   MOVEL     ' '           V2CFCC
300100930504     C                   END
300200930506     C                   END
300300940713     C                   ENDIF
300400930402     C*
300500911023     C                   ELSE
300600911023     C* SE NUMERO >< 0 IN CONSEGNA
300700911023     C     ARBNDC        IFNE      0
300800940714     C                   MOVEL     'IN CON'      V2CCON
300900911023     C                   SETON                                        13
301000911023     C                   END
301100911023     C                   END
301200921102     C*
301300940714     C     V2CCON        IFEQ      *BLANKS
301400921102     C     ARBFBC        ANDNE     ' '
301500921102     C                   SETON                                        62
301600921102     C*
301700921102     C     ARBFBC        IFEQ      'B'
301800930504     C     ARBFBC        OREQ      '*'
301900940714     C                   MOVEL     'BLOC.'       V2CCON
302000921102     C                   ELSE
302100921102     C     ARBFBC        IFEQ      'A'
302200940714     C                   MOVEL     'L.AVV'       V2CCON
302300921102     C                   ELSE
302400940714     C                   MOVEL     'GIAC.'       V2CCON
302500921102     C                   END
302600921102     C                   END
302700921102     C                   END
302800911023     C*
302900940714     C                   MOVEL     ARBTMC        V2CTMC
303000900522     C*
303100900518     C                   ADD       1             B
303200940713     C                   ADD       1             NR2
303300131107     C                   WRITE     LRI6S02
303400940720     C*
303500900515     C                   ENDSR
303600940802     C**************************************************************************
303700940802     C* EMISSIONE BOLLA
303800940802     C**************************************************************************
303900911023     C     BOLLA         BEGSR
304000940802     C*
304100070411     C                   SETOFF                                       220616
304200070403     C                   SETOFF                                       170377
304300070404     C                   SETOFF                                       262708
304400131127     C                   SETOFF                                       2933
304500940408     C                   SETOFF                                       687069
304600070524     C                   SETOFF                                       850488
304700131120     C                   SETOFF                                       86
304800991027      *
304900940719     C                   Z-ADD     0             V3CLNA
305000940719     C                   Z-ADD     0             V3CDSP
305100930517     C                   Z-ADD     0             VIDRMN
305200940712     C                   MOVE      *BLANKS       VIDRMA
305300991217     C                   CLEAR                   V3CRPP
305400131120     C                   CLEAR                   atrpda
305500131127     C                   CLEAR                   atrtsp
305600961112     C*
305700911022     C* CONTROLLO SE SPEDIZIONE GIACENTE
305800920219     C* GIACENZA APERTA
305900941202     C                   Z-ADD     ARBAAS        KAAS
306000940713     C                   Z-ADD     ARBLNP        KLNP
306100940713     C                   Z-ADD     ARBNRS        KNRS
306200940713     C                   Z-ADD     ARBNSP        KNSP
306300050405     C     KGCA1         CHAIN     tigcp21l                           31
306400040216if  2C  N31GCPATB        IFEQ      ' '
306500920410     C     GCPDCG        IFNE      0
306600920219     C                   SETON                                        03
306700920219     C                   ELSE
306800070403     C                   SETON                                        0327
306900070412     C                   EVAL      V1DFBC=' GIACENTE '
307000040217     C                   ENDIF
307100040217e   2C                   ENDIF
307200930428     C*
307300021213     C*
307400021213     C* 77 ON - Esistono note supermenrcati d ainterrogare
307500021213     C                   Z-ADD     ARBAAS        KAAS
307600021213     C                   Z-ADD     ARBLNP        KLNP
307700021213     C                   Z-ADD     ARBNRS        KNRS
307800021213     C                   Z-ADD     ARBNSP        KNSP
307900021213     C                   MOVEL     'SCR'         KTRD
308000021213     C     KAR51         CHAIN     FIAR5000                           31
308100021217     C  n31              seton                                        77
308200021217     C   31              setoff                                       77
308300920219     C*
308400920207     C* SE BOLLA CON LA "G" SENZA GIACENZA MECCANIZZATA SEGNALO
308500931116     C     ARBFBC        IFEQ      'G'
308600070412     C                   EVAL      V1DFBC=' GIACENTE '
308700070403     C                   SETON                                        27
308800040217     C                   ENDIF
308900930415     C*
309000940907     C                   MOVEL     ARBAAS        V3CAAS
309100940719     C                   MOVE      ARBNSP        V3CNSP
309200940719     C                   MOVEL     ARBNRS        V3CNRS
309300070404     C                   MOVEL     ARBNRS        V4CNRS
309400131108     C********           MOVEL     ARBNRS        V7CNRS
309500940719     C                   MOVEL     ARBLNP        V3CLNP
309600950508     C                   MOVEL     ARBLNP        V3CLPA
309700131114     c
309800131114     c                   z-add     arbmgs        arbdsp
309900131114     c                   movel     arbaas        arbdsp
310000940908     C*
310100940908     C*  L E G A M I    B O L L A
310200940908     C  N75              EXSR      LEGBOL
310300940908     C*
310400911022     C* NON SEQUENZIALI
310500911023     C     ARBFNS        IFEQ      'S'
310600930705     C                   MOVEL     CNOSEQ        DESFNS
310700911022     C                   ELSE
310800911022     C                   MOVEL     *BLANKS       DESFNS
310900040217     C                   ENDIF
311000930525     C*
311100930525     C* 95 ON - ESITONO EVENTI DA INTERROGARE
311200940713     C                   Z-ADD     ARBAAS        KAAS$
311300940713     C                   Z-ADD     ARBLNP        KLNP
311400940713     C                   Z-ADD     ARBNRS        KNRS
311500940713     C                   Z-ADD     ARBNSP        KNSP
311600990805     C     KEVB1         SETLL     FNEVB01L                               95
311700920402     C*
311800920403     C* 98 ON - ESITONO VARIAZIONI FATTE
311900131007     C     KARB2         SETLL     FiARBF2C                               98
312000990119     C* 04 ON - ESISTONO CA DA VISUALIZZARE
312100990119     C                   CLEAR                   CONTCA
312200040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
312300040809      * legate alla spedizione che passo
312400040809     c                   clear                   fidn12ds
312500040809     c                   eval      i12aas = arbAAS
312600040809     c                   eval      i12lnp = arbLNP
312700040809     c                   eval      i12nrs = arbNRS
312800040809     c                   eval      i12nsp = arbNSP
312900040809     c                   eval      i12fel = 'E'
313000040809     c                   eval      i12fan = 'E'
313100040809     c*
313200040809     c                   call      'FIDN12R'
313300040809     c                   parm                    fidn12ds
313400040809     c*
313500040809     c                   setoff                                       31
313600040809     c*
313700040809     c* se non ci sono errori
313800040809     c                   if        o12err <> ' '
313900040809     c                   seton                                        31
314000040809     c                   else
314100040809     c* se numero di CA trovate maggiore di zero
314200040809     c                   if        o12nca = 0
314300040809     c                   seton                                        31
314400040809     c                   else
314500040809     c                   z-add     1             ii                2 0
314600040809     c                   dow       ii <= o12nca and *in31 = *off
314700040809     c                   movea     skkey(ii)     dskey
314800040809     C                   SETON                                        04
314900040809     C                   ADD       1             CONTCA            3 0
315000040809     C                   Z-ADD     dsaac         VHCAAC
315100040809     C                   Z-ADD     dsfil         VHCFIL
315200040809     C                   Z-ADD     dsnca         VHCNCA
315300040809     c*
315400040809     c                   add       1             ii
315500040809     c                   enddo
315600040809     c*
315700040809     c                   seton                                        31
315800040809     c*
315900040809     c                   endif
316000040809     c                   endif
316100040809     c**
316200070406     c* se Presente FIARP, imposto la cons.richista proposta
316300070404     c                   clear                   v4cdimpr
316400070404     c                   clear                   v4coimpr
316500070404     c                   clear                   v4cprupr
316600070416     c                   clear                   v08dcrpr
316700070404     c                   clear                   v4ctcrpr
316800070404     c                   clear                   v4chcrpr
316900070406     c
317000070406     c* Solo se visualizzabile
317100070406     c     karb2         chain     fiarp01l
317200070406    1c                   if        %found(FIarp01l)
317300070404     c                   seton                                        26
317400070404     C                   MOVEL     arppru        v4cprupr
317500070404     C                   MOVEL     arpoim        v4coimpr
317600070404    2c                   if        arpdim>0
317700070404     c     *iso          movel     arpdim        dataeur
317800070416     c                   movel     dataeur       datadmy
317900070416     c                   movel     datadmy       w0060             6 0
318000070416     c                   movel     w0060         v4cdimpr
318100070404    2c                   endif
318200070404    2c                   if        arpdcr>0
318300070404     c     *iso          movel     arpdcr        dataeur
318400070416     c                   movel     dataeur       v08dcrpr
318500070416     c**                 movel     datadmy       v06dcrpr
318600070404    2c                   endif
318700070404     c                   movel     arphcr        v4chcrpr
318800070404     c                   movel     arptcr        v4ctcrpr
318900070406     c                   if        arptcr=' '
319000070406     c                   eval      v4ctcrpr='IL'
319100070404    1c                   endif
319200070406    1c                   endif
319300070406     c
319400911022     C* TIPO BOLLA CONTROLLO SE FRANCO
319500940719     C                   MOVEL     ARBCBO        V3CCBO
319600911022     C                   MOVEL     '3A'          COD
319700911022     C                   MOVEL     *BLANKS       KEY
319800940712     C                   MOVEL     ARBCBO        KEY
319900940713     C     KTAB1         CHAIN     TABEL                              31
320000911022     C                   MOVEL     TBLUNI        DS3A
320100911022     C                   MOVEL     §3ATB1        FLGTB1            1
320200940719     C                   MOVEL     §3ADES        V3DCBO
320300940719     C                   MOVEL     §3ADES        V5DCBO
320400911030     C* FRANCO
320500040216if  1C     FLGTB1        IFEQ      'F'
320600911022     C                   SETON                                        33
320700940713     C                   MOVE      ARBKSC        V3CKSC
320800940712     C                   MOVE      *BLANKS       VIDKSC
320900040216x   1C                   ELSE
321000911030     C* ASSEGNATO
321100911030     C     ARBCCM        IFNE      0
321200940713     C                   MOVE      ARBCCM        V3CKSC
321300911030     C                   ELSE
321400940713     C                   MOVE      *ZEROS        V3CKSC
321500040217     C                   ENDIF
321600911030     C                   MOVEL     ARBKSC        VIDKSC
321700040217e   1C                   ENDIF
321800070410     c* Numero di telefono del mittente
321900070410     c                   clear                   v3ctelm
322000070410     c                   move      v3cksc        w0040
322100070410     c                   if        w0040<>0000 and w0040<>8888
322200070410     c                             and w0040<>9999
322300070410     c                   eval      i69kin = V3Cksc
322400070410     c                   exsr      CTR_ANAGRA
322500070410     c                   eval      V3ctelm= indtel
322600070410     c                   endif
322700991027     C* 38 ON - ESISTE TB2 ANCHE SENZA TIPO RCD "2" IN FIAR600F
322800920121     C     §3ATB2        COMP      '  '                               3838
322900040218      *
323000040218      *?Controllo se esitono reclami?
323100131111     c                   eval      wARBlnp = ARBlnp
323200131111     c                   eval      wARBnrs = ARBnrs
323300131111     c                   eval      wARBnsp = ARBnsp
323400131111     c                   eval      wARBaas = ARBaas
323500040218     c                   eval      TGDtor  = 'S'
323600040218     c                   eval      TGDogg  = ds_OggSped
323700040218     c     K02TGD02      setll     FITGD000
323800040302     c                   eval      *in64   = (%equal(FITGD02L)   and
323900040303     c                                           (SDSprm =  1
324000040303     c                                        or (SDSprm >  1
324100040303     c                                        and IA1ins <> 'N')))
324200940712     C*
324300940712     C* DECODIFICO  "BOLLA DI RIENTRO"
324400961111     C                   MOVEL     *BLANKS       VIDFBR
324500961111     C                   MOVEL     *BLANKS       VIDFFR
324600961111     C                   SETOFF                                       1073
324700940712     C     ARBFBR        IFEQ      'S'
324800961111     C                   MOVEL     'SI'          VIDFBR
324900961111     C                   SETON                                        10
325000040217     C                   ENDIF
325100961111     C     ARBFBR        IFEQ      'F'
325200961111     C                   MOVEL     'FR'          VIDFFR
325300961111     C                   SETON                                        73
325400040217     C                   ENDIF
325500970130     C*
325600970227     C* FLAG CONSEGNA DDT
325700970130     C                   MOVEL     ARBAAS        KAAS
325800970130     C                   MOVEL     ARBLNP        KLNP
325900970130     C                   MOVEL     ARBNRS        KNRS
326000970130     C                   MOVEL     ARBNSP        KNSP
326100970130     C                   MOVEL     'A'           KTRC
326200970131     C                   CLEAR                   V1DBAM
326300000615     C                   eval      v3csop = ' '
326400060216     C     KAR41         CHAIN     FiAR401L                           31
326500970227     C* C'E' IL RECORD
326600040216if  1C     *IN31         IFEQ      *OFF
326700000615     C                   movel     ar4not        dsbl4a
326800000615     C                   eval      v3csop = §b4aso
326900031027     c                   Move      §b4Abm        wbam              1
327000040216if  2C     WBAM          IFEQ      'S'
327100970131     C     WBAM          OREQ      'C'
327200031027     c     wbam          oreq      'P'
327300031027     c     wbam          oreq      'Q'
327400970227     C                   SETON                                        76
327500970227     C                   MOVEL     'SI DDT'      V1DBAM
327600040216x   2C                   ELSE
327700970227     C                   SETOFF                                       76
327800970227     C                   MOVEL     'NO DDT'      V1DBAM
327900040216e   2C                   ENDIF
328000970227     C**
328100040216sel 2C                   SELECT
328200970227     C     WBAM          WHENEQ    'C'
328300970227     C     WBAM          OREQ      'K'
328400970227     C                   MOVEL     'CLI'         V1DFST
328500970227     C     WBAM          WHENEQ    'J'
328600031027     C     WBAM          oreq      'P'
328700031027     C     WBAM          oreq      'Q'
328800031113     C     WBAM          OREQ      'W'
328900070406     C                   MOVEL     'Arr'         V1DFST
329000970227     C     WBAM          WHENEQ    'S'
329100970227     C     WBAM          OREQ      'Y'
329200070406     C                   MOVEL     'Par'         V1DFST
329300040216x   2C                   OTHER
329400970227     C                   MOVEL     'NO '         V1DFST
329500040216e   2C                   ENDSL
329600970227     C**
329700970227     C* NON  C'E' IL RECORD
329800040216x   1C                   ELSE
329900970227     C                   SETON                                        76
330000970227     C                   MOVEL     'SI DDT'      V1DBAM
330100970227     C                   MOVEL     'PAR'         V1DFST
330200040216e   1C                   ENDIF
330300010510     c* numero O.R.M.
330400010510     C                   MOVEL     'M'           KTRC
330500060216     c     kar41         chain     fiar401l                           31
330600010510     c     *in31         ifeq      *off
330700010510     c                   movel     ar4not        dsbl4m
330800010510     c                   z-add     §b4poe        v3cpoe
330900010510     c                   z-add     §b4nsr        v3cnsr
331000010510     c                   z-add     §b4nor        v3cnor
331100010510     c                   z-add     §b4nrv        v3cnrv
331200010510     c                   else
331300010510     c                   clear                   v3cpoe
331400010510     c                   clear                   v3cnsr
331500010510     c                   clear                   v3cnor
331600010510     c                   clear                   v3cnrv
331700010510     c                   endif
331800130320     c*//* Verifico se ha fiar8
331900130320     C*//                MOVEL     ARBAAS        KAAS
332000130320     C*//                MOVEL     ARBLNP        KLNP
332100130320     C*//                MOVEL     ARBNRS        KNRS
332200130320     C*//                MOVEL     ARBNSP        KNSP
332300130320     c*//  ksped         setll     fiar801l
332400130320     c*//                if        %equal(fiar801l)
332500130320     c*//                eval      v1dAR8='PACKING LIST'
332600130320     C*//                ELSE
332700130320     C*//                CLEAR                   V1DAR8
332800130320     c*//                endif
332900941219     C*
333000941219     C* DECODIFICO  "CONSEGNA A MAGAZZINO"
333100941219     C     ARBFDN        IFEQ      'S'
333200941219     C                   MOVEL     'SI'          V3CFDN
333300941219     C                   ELSE
333400941219     C                   MOVEL     *BLANKS       V3CFDN
333500941219     C                   ENDIF
333600911129     C*
333700920204     C* INCASSO C/A E ASSEGNATO
333800930113     C                   MOVEL     '  '          VIDICC
333900930113     C                   MOVEL     '  '          VIDICA
334000930113     C*
334100930113     C* SE ASSEGNATO O FRANCO CON SECONDA BOLLA
334200930113     C     *IN33         IFEQ      '0'
334300930113     C     *IN33         OREQ      '1'
334400930113     C     *IN38         ANDEQ     '1'
334500930113     C*
334600930330     C     ARBICA        IFNE      ' '
334700941216     C     ARBICA        ANDNE     'R'
334800920204     C                   MOVEL     'SI'          VIDICA
334900040217     C                   ENDIF
335000040217     C                   ENDIF
335100911129     C*
335200911030     C* LINEA PARTENZA DECODIFICA
335300020225     c                   clear                   og143
335400930706     C                   Z-ADD     ARBLNP        NUM3
335500930706     C     NUM3          CHAIN     AZORG01L                           31
335600940719     C  N31              MOVEL     ORGDES        V3DLNP
335700940719     C   31              MOVEL     *BLANKS       V3DLNP
335800020225     c                   eval      og143 = orgde3
335900020225     c                   eval      lnpntw = §ogntw
336000911030     C* LINEA ARRIVO   DECODIFICA
336100991217     c                   clear                   og143
336200070403     C     arblna        CHAIN     AZORG01L                           31
336300940719     C  N31              MOVEL     ORGDES        V3DLNA
336400940719     C   31              MOVEL     *BLANKS       V3DLNA
336500070403     c                   movel     v3dlna        v7dlna
336600991217     C                   movel     orgde3        og143
336700070403     C                   movel     orgde8        og148
336800020304     c                   eval      lnantw = §ogntw
336900070426     c* p.o. gestore di arblna
337000070426     c                   clear                   fnlv55ds
337100070426     c                   movel     arblna        d55lin
337200070426     c                   movel     '6'           d55tpt
337300070426     C                   time                    wrkoggiiso
337400070426     C     *iso          move      wrkoggiiso    d55drf
337500070426     c                   call      'FNLV55R'
337600070426     c                   parm                    fnlv55ds
337700070426     c
337800070426     c                   if        d55tfa<>arblna
337900070426     C     d55tfa        CHAIN     AZORG01L                           31
338000070426     C  n31              movel     orgde8        og148
338100070426     C   31              clear                   og148
338200070426     c                   endif
338300070426     c
338400070403     c
338500070403     c* Abilito visualizzazione nuove funzioni se GEO attiva
338600070426     c* Per p.o. gestione linea di arrivo e p.o. utente
338700070403     c                   if        §oggeot='S' and dutgeot='S'
338800070524     c                   seton                                        1788
338900070406     c                   else
339000070406     c* non visualizzo nemmeno fiarp
339100070406     c                   setoff                                       26
339200070403     c                   endif
339300070524     c
339400070524     c* Vedo se almeno è attivo lo scambio dati con tellus
339500070524     c                   if        not *in88 and §ogtlls<>' ' and dutgeot='S'
339600070524     c                   seton                                        88
339700070524     c                   endif
339800070404     c
339900070404     c* Emetto riga sintetica altre abilitazioni
340000070521     c                   clear                   v5dngd
340100140109     c***                if        *in17
340200070404     c                   clear                   v3cges
340300070404     C                   MOVEL     *BLANKS       V3cgesd
340400070404     c*
340500131120     c                   clear                   fnlrg7ds
340600131120     c                   eval      ilrg7aas=arbaas
340700131120     c                   eval      ilrg7lnp=arblnp
340800131120     c                   eval      ilrg7nrs=arbnrs
340900131120     c                   eval      ilrg7nsp=arbnsp
341000131120     c                   eval      ilrg7lung=58
341100131120     c                   call      'FNLRG7R'
341200131120     c                   parm                    fnlrg7ds
341300131120     c                   movel     olrg7vssin    v3cges
341400140109     c***                endif
341500070410     c
341600070524     c* Reperisco dati della distinta automatica tramite FIDG34R
341700070524     c                   if        *in88
341800070410     c                   clear                   v5ccgi
341900070411     c                   clear                   v5dcgi
342000070410     c                   clear                   v5dtgi
342100070410     c                   clear                   v5dttl
342200070411     c                   clear                   v5clno
342300070427     c                   clear                   v5dlno
342400070427     c                   clear                   v5cargerr
342500070427     c                   clear                   v5dargerr
342600070427     c
342700070427     c                   clear                   fidg34ds
342800070427     c                   eval      id34tip='2'
342900070427     c                   eval      id34aas=arbaas
343000070427     c                   eval      id34lnp=arblnp
343100070427     c                   eval      id34nrs=arbnrs
343200070427     c                   eval      id34nsp=arbnsp
343300070427     c                   call      'FIDG34R'
343400070427     c                   parm                    FIDG34DS
343500070410     c
343600070427     c                   movel     od34cgi       v5ccgi
343700070410    3c                   select
343800070427     c                   when      od34tgi='A'
343900070410     c                   eval      v5dtgi='Automatico'
344000070427     c                   when      od34tgi='M'
344100070410     c                   eval      v5dtgi='Manuale   '
344200070410    3c                   ENDSL
344300070411     c* Decodifica giro
344400070427     c                   eval      v5dcgi=od34cgides
344500070427     c* esito/errore
344600070427     c                   movel     od34err       v5cargerr
344700070427     c                   eval      v5dargerr=od34errdes
344800070427     c* livello normalizzazione
344900070427     c                   movel     od34lno       v5clno
345000070427     c                   eval      v5dlno=od34lnodes
345100070427     c* elaborazione tellus
345200070427     c***                eval      v5dttl=od34ttldes
345300070427     c                   select
345400070427     c                   when      od34ttl='1'
345500070427     c                   eval      v5dttl='SI- CON Giro'
345600070427     c                   when      od34ttl='2'
345700070427     c                   eval      v5dttl='SI-SenzaGiro'
345800070427     c                   endsl
345900070521     c*
346000070521     c* Decodifico se assegnazione automatica o manuale
346100070521     c                   if        arbndc>0 and arbngd<>0
346200070521     c                   eval      v5dngd='MANUALE'
346300070521     c                   endif
346400070521     c                   if        arbndc>0 and arbngd=0
346500070521     c                   eval      v5dngd='AUTOMAT'
346600070521     c                   endif
346700070410     c
346800070410    1c                   endif
346900991217     c
347000060518     c* LNA DPD o lnp dpd: leggo record "I" di ar4
347100000216     c                   setoff                                       08
347200000216     c                   clear                   v3czon
347300000216     c                   clear                   v3crtn
347400040216if  1c                   if        lnantw = 'DPD'
347500991217     c                   seton                                        85
347600991217     C                   MOVEL     ARBAAS        KAAS
347700991217     C                   MOVEL     ARBLNP        KLNP
347800991217     C                   MOVEL     ARBNRS        KNRS
347900991217     C                   MOVEL     ARBNSP        KNSP
348000060518     C                   MOVEL     'I'           KTRC
348100060216     C     KAR41         CHAIN     FiAR401L                           31
348200060518     C  N31              MOVEL     AR4NOT        dsbl4i
348300060518     c   31              clear                   dsbl4i
348400060518     C                   MOVEL(P)  §b4ipn        V3CRPP
348500060518     c                   if        §b4ida<>*blanks or §b4iza>*zeros
348600060518     c                   movel     §b4ida        v3crtn
348700060518     c                   movel     §b4iza        v3czon
348800000216     c                   seton                                        08
348900000216     c                   endif
349000050304     c                   else
349100050304if  1c                   if        lnpntw = 'DPD'
349200050304     C                   MOVEL     ARBAAS        KAAS
349300050304     C                   MOVEL     ARBLNP        KLNP
349400050304     C                   MOVEL     ARBNRS        KNRS
349500050304     C                   MOVEL     ARBNSP        KNSP
349600060518     C                   MOVEL     'I'           KTRC
349700060518     c                   clear                   dsbl4i
349800060216     C     KAR41         CHAIN     FiAR401L
349900060216     c                   if        %found(fiar401l)
350000060518     C**                 MOVEL     AR4NOT        dsbl4f
350100060518     C**                 MOVEL     §b4fnp        V3CRPP
350200060518     C                   MOVEL     AR4NOT        dsbl4i
350300060518     C                   MOVEL     §b4ipn        V3CRPP
350400050304     c                   seton                                        85
350500050304     c                   endif
350600050304     c                   endif
350700040216e   1c                   endif
350800971219     C*
350900971219     C* 16 ON  - SI TRATTA DI BOLLA EXPORT
351000020225     c                   if        lnantw = 'EEX' or lnantw = 'EUP' or
351100060201     c                             lnantw = 'FED' or lnantw='DPD'
351200971219     C                   SETON                                        16
351300971219    1C                   ENDIF
351400940719     C                   MOVEL     V3DLNA        D15LNA
351500920804     C*
351600940719     C                   MOVEL     ARBLNA        V3CLNA
351700940719     C                   MOVEL     ARBRSM        V3CRSM
351800070410     C                   MOVEL     ARBRSM        V4CRSM
351900940713     C                   MOVEL     ARBRSM        V28RSM
352000940719     C                   MOVEL     ARBINM        V3CINM
352100940719     C                   MOVEL     ARBPRM        V3CPRM
352200940719     C                   MOVEL     ARBCAM        V3CCAM
352300940719     C                   MOVEL     ARBLOM        V3CLOM
352400940713     C                   MOVEL     ARBLOM        V25LOM
352500940719     C                   MOVEL     ARBFAP        V3CFAP
352600950421     C                   MOVEL     ARBNZM        V3CNZM
352700131108     c                   if        arbnzm<>*blaNKS
352800131108     C                   MOVEL     arbnzm        v7cprm
352900131108     c                   else
353000131108     C                   MOVEL(P)  arbprm        v7cprm
353100131108     c                   endif
353200131108     c
353300050113     C                   MOVEL     ARBXCO        VIDXCO
353400130221     c                   if        vidxco >*zeros  and no_f22=*blanks
353500110928     c                   seton                                        86
353600110928     c                   endif
353700940719     C                   MOVEL     *BLANKS       V3CPIM
353800940719     C                   MOVEL     *BLANKS       V4CPID
353900061108     c                   clear                   v3cdfm
354000061108     c                   clear                   v4cdfd
354100950411     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
354200060216     C* DA FiAR4 SE RECORD ESISTENTE
354300950411     C                   MOVEL     ARBAAS        KAAS
354400950411     C                   MOVEL     ARBLNP        KLNP
354500950411     C                   MOVEL     ARBNRS        KNRS
354600950411     C                   MOVEL     ARBNSP        KNSP
354700950411     C                   MOVEL     'P'           KTRC
354800060216     C     KAR41         CHAIN     FiAR401L                           31
354900040216if  1C     *IN31         IFEQ      *OFF
355000950411     C                   MOVEL     AR4NOT        V3CPIM
355100950411     C                   MOVE      AR4NOT        V4CPID
355200040216x   1C                   ELSE
355300950411     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI ARBCPI
355400040216if  2C     ARBCPI        IFNE      *BLANKS
355500950411     C  N33              MOVEL     ARBCPI        V4CPID
355600950411     C   33§3ATB2        IFEQ      '  '
355700950411     C                   MOVEL     ARBCPI        V3CPIM
355800950411     C                   ELSE
355900950411     C                   MOVEL     ARBCPI        V4CPID
356000040217     C                   ENDIF
356100040217e   2C                   ENDIF
356200040217e   1C                   ENDIF
356300061108      * valorizzo il codice fiscale del mittente e del destinatario
356400061108      * FIAR4 rcd Q
356500061108     c                   eval      ktrc = 'Q'
356600061108     c     kar41         chain     fiar401l
356700061108if  1c                   if        %found(fiar401l)
356800061108     c                   movel     ar4not        v3cdfm
356900061108     c                   move      ar4not        v4cdfd
357000061108x   1c                   endif
357100980704     C* VISUALIZZO SE C'E' IL RIFERIM PARTNER
357200971219     C                   CLEAR                   V3CEST
357300971219     C                   MOVEL     ARBAAS        KAAS
357400971219     C                   MOVEL     ARBLNP        KLNP
357500971219     C                   MOVEL     ARBNRS        KNRS
357600971219     C                   MOVEL     ARBNSP        KNSP
357700971219     C                   MOVEL     'E'           KTRC
357800060216     C     KAR41         CHAIN     FiAR401L                           31
357900980704     C* VISUALIZZO IN UNA BOLLA ITALIA SE C'E'
358000980704     C* VISUALIZZO SEMPRE IN UNA BOLLA IMPORT/EXPORT
358100971219     C  N31              MOVEL     AR4NOT        V3CRPP
358200980704     C  N31              SETON                                        85
358300980704     C**
358400020225     c                   if        *in31 and
358500020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
358600020225     c                              lnpntw = 'FED') or
358700020225     c                             (lnantw = 'EEX' or lnantw = 'EUP' or
358800020225     c                              lnantw = 'FED')
358900980704     C                   SETON                                        85
359000980704     C                   ENDIF
359100980704     C**
359200940719     C                   MOVEL     ARBRSD        V4CRSD
359300131108     C**********         MOVEL     ARBRSD        V30RSD
359400940713     C                   MOVEL     ARBRSD        V32RSD
359500940719     C                   MOVEL     ARBIND        V4CIND
359600131119     C                   MOVEL     ARBIND        V33IND
359700940719     C                   MOVEL     ARBPRD        V4CPRD
359800940719     C                   MOVEL     ARBCAD        V4CCAD
359900940719     C                   MOVEL     ARBLOD        V4CLOD
360000131119     C*****              MOVEL     ARBLOD        V24LOD
360100940719     C                   MOVEL     ARBFIN        V4CFIN
360200940719     C                   MOVEL     ARBNZD        V4CNZD
360300911022     C                   MOVEL     ARBGC1        VIDGC1
360400911022     C                   MOVEL     ARBGC2        VIDGC2
360500911022     C                   MOVEL     ARBCTR        VIDCTR
360600940719     C                   MOVEL     ARBCTS        V6CCTS
360700131108     c                   if        arbnzd<>*blaNKS
360800131108     C                   MOVEL     arbnzd        v7cprd
360900131108     c                   else
361000131108     C                   MOVEL(P)  arbprd        v7cprd
361100131108     c                   endif
361200940719     C* DECODIFICA CODICE TASSAZIONE
361300940719     C                   MOVEL     'CT'          COD
361400940719     C                   MOVEL     *BLANKS       KEY
361500940719     C                   MOVEL     ARBCTS        KEY
361600940719     C     KTAB1         CHAIN     TABEL00F                           31
361700940719     C                   MOVEL     TBLUNI        V6DCTS
361800911022     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
361900911022     C                   MOVEL     'TR'          COD
362000911022     C                   MOVEL     *BLANKS       KEY
362100911022     C                   MOVEL     VIDCTR        UNOCTR            1
362200911022     C                   MOVEL     UNOCTR        KEY
362300940713     C     KTAB1         CHAIN     TABEL                              31
362400911022     C                   MOVEL     TBLUNI        DEFCTR
362500070404     C*****              MOVEL     ARBFTM        V3CFTM
362600070404     c                   if        arbftm='S'
362700070404     c                   eval      v3dftm='TASSATA CON TARIFFA MITTENTE'
362800070404     c                   endif
362900940711     C* DECODIFICA TIPO SERVIZIO BOLLE
363000090604     c                   clear                   ds5e
363100940711     C                   MOVEL     '5E'          COD
363200940711     C                   MOVEL     *BLANKS       KEY
363300940713     C                   MOVEL     ARBTSP        KEY
363400941024     C     KTAB1         CHAIN     TABEL00F                           31
363500090604     C  N31              MOVEL     TBLUNI        ds5e
363600090604     C  N31              MOVEL     §5ed08        V1DTSP
363700940719     C   31              MOVEL     *BLANKS       V1DTSP
363800131127     c                   if        §5eftc<>' '
363900131127     c                   movel     ri            atrtsp
364000131127     c                   else
364100131127     c                   movel     hi            atrtsp
364200131127     c                   endif
364300940713     C                   MOVEL     ARBVMD        VIDVMD
364400940713     C                   MOVEL     ARBVAD        VIDVAD
364500940713     C                   MOVEL     ARBIAS        VIDIAS
364600940713     C                   MOVEL     ARBVAS        VIDVAS
364700911022     C                   MOVEL     ARBNAS        VIDNAS
364800950214     C                   CLEAR                   VIDFLS
364900950214     C     ARBNCD        IFNE      *ZERO
365000950214     C     ARBFNS        ORNE      *BLANKS
365100940923     C                   MOVEL     ARBFLS        VIDFLS
365200950214     C                   ENDIF
365300950214     C                   Z-ADD     ARBFLS        V3CFLS
365400911022     C                   MOVEL     ARBNCL        VIDNCL
365500070410     C                   MOVEL     ARBPKB        VIDPKb
365600070410     C                   MOVEL     ARBPKF        VIDPKF
365700070410     C                   MOVEL     ARBVLb        VIDVLb
365800070410     C                   MOVEL     ARBFVb        VIDFVb
365900070410     C                   MOVEL     ARBVLF        VIDVLF
366000070410     C                   MOVEL     ARBFVF        VIDFVF
366100930705     C     ARBNCR        IFLT      99998
366200940711     C                   MOVEL     ARBVLC        VIDVLC
366300940711     C                   MOVEL     ARBNCR        VIDNCR
366400921231     C                   ELSE
366500990909     C                   Z-ADD     0             VIDVLC
366600990909     C                   Z-ADD     0             VIDNCR
366700921231     C                   END
366800990909     C     ARBNCP        IFLT      99998
366900990909     C                   MOVEL     ARBPKC        VIDPKC
367000990909     C                   MOVEL     ARBNCP        VIDNCP
367100990909     C                   ELSE
367200990909     C                   Z-ADD     0             VIDPKC
367300990909     C                   Z-ADD     0             VIDNCP
367400990909     C                   END
367500940711     C                   MOVEL     ARBQFT        VIDQFT
367600031119
367700031119      * Referente e Telefono del destinatario
367800031119     c                   Clear                   V4cRef
367900031119     c                   Clear                   V4cTeld
368000091214     c                   clear                   w_cdi
368100031119      * li prendo da Fiar500f record 'GEN'
368200130320     c                   clear                   v1dAR8l
368300130320     c                   clear                   v1dAR8
368400031119     c                   Clear                   dAr5Gen
368500031119     c                   Z-Add     ArbAas        Kaas
368600031119     c                   Z-Add     ArbLNp        Klnp
368700031119     c                   Z-Add     ArbNrs        Knrs
368800031119     c                   Z-Add     ArbNsp        Knsp
368900031119     c                   Eval      Ktrd = 'GEN'
369000031119     c     kAr51         Chain     Fiar501l
369100031119If  1c                   If        %Found(Fiar501l)
369200031119     c                   Movel     Ar5Uni        Dar5Gen
369300031119     c                   Eval      V4cRef = §Ar5Ref
369400031119     c                   Eval      V4cTeld = §Ar5Teld
369500091214     c* memorizzo eventuale codice beneficiario assegno
369600091214     c                   eval      w_cdi=§ar5cdi
369700130320     c* verifico se LDV con packing-list e/o PDF
369800130320sel 2c                   select
369900130320w   2c                   when      §AR5als = 'S'
370000130320     c                   eval      v1dAR8l = 'PACKING LIST PDF'
370100130320     c                   eval      v1dAR8  = 'PACKING PDF '
370200130320w   2c                   when      §AR5alx = 'S'
370300130320     c                   eval      v1dAR8l = 'PACKING LIST'
370400130320     c                   eval      v1dAR8  = 'PACKING LIST'
370500130320e   2c                   endsl
370600031119    1c                   EndIf
370700040301
370800040301      * Se colli di valore visualizzo "V" nell'apposito campo
370900040301      *  con lo specifico attributo di visualizzazione
371000040301if  1c                   if        §AR5bva = *blanks
371100040301     c                   eval      VIDbva  = *blanks
371200040301x   1c                   else
371300040301     c                   eval      VIDbva  = 'V'
371400040301e   1c                   endif
371500110601
371600110610     c* email destinatario
371700110601     c                   setoff                                       60
371800110601     c                   clear                   v4cemw
371900110610     c                   clear                   v4cemd
372000130927     c                   Clear                   dAr5emd
372100110601      * li prendo da Fiar500f record 'EMD'
372200110601     c                   Z-Add     ArbAas        Kaas
372300110601     c                   Z-Add     ArbLNp        Klnp
372400110601     c                   Z-Add     ArbNrs        Knrs
372500110601     c                   Z-Add     ArbNsp        Knsp
372600110601     c                   Eval      Ktrd = 'EMD'
372700130927     c***  kAr51         setll     Fiar501l
372800130927     c     kAr51         chain     Fiar501l
372900130927If  1c                   If        %found(Fiar501l)
373000130927     c                   eval      dar5emd=ar5uni
373100130927     c                   if        §ar5tel<>*blanks or §ar5eml<>*blanks
373200110610     c                   Eval      V4cemd = 'SI'
373300110601     c                   seton                                        60
373400130927     c                   endif
373500110601    1c                   EndIf
373600040301
373700030121      * Bancali
373800030121     c                   Clear                   VidTba
373900030121     c                   Clear                   VidTb2
374000030122     c                   Clear                   VidNba
374100030122     c                   Clear                   VidNb2
374200030703      * Record 'BAN'
374300030703If  0c                   If        %subst(ArbGva:1:1) = 'B'
374400030121     c                   Clear                   dAr5Ban
374500030121     c                   Eval      Ktrd = 'BAN'
374600030121     c     kAr51         Chain     Fiar501l
374700030121If  1c                   If        %Found(Fiar501l)
374800030121     c                   Movel     Ar5Uni        Dar5Ban
374900030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
375000030122      * per le note relative al mancato reso dei bancali
375100030122     c                   If        Not *In77 and
375200030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
375300030122     c                   Eval      *In77 = *On
375400030122     c                   EndIf
375500030121     c                   Eval      VidNba = §Ar5Nba
375600030121     c                   Eval      VidNb2 = §Ar5Nb2
375700030121If  2c                   If        §Ar5Tba <> *Zeros
375800030703     c                   Eval      wtba = §Ar5Tba
375900030703     c                   ExSr      Sr_DecTba
376000030703     c                   Eval      VidTba = wdestba
376100030121    2c                   EndIf
376200030121If  2c                   If        §Ar5Tb2 <> *Zeros
376300030703     c                   Eval      wtba = §Ar5Tb2
376400030703     c                   ExSr      Sr_DecTba
376500030703     c                   Eval      VidTb2 = wdestba
376600030121    2c                   EndIf
376700030121    1c                   EndIf
376800030703    0c                   EndIf
376900030703      * Record 'BNB'  -  Spedizione OVER 25 colli
377000030703If  0c                   If        %subst(ArbGva:1:1) = 'O'
377100030703     c                   Clear                   dAr5Bnb
377200030703     c                   Eval      Ktrd = 'BNB'
377300030703     c     kAr51         Chain     Fiar501l
377400030703If  1c                   If        %Found(Fiar501l)
377500030703     c                   Movel     Ar5Uni        Dar5Bnb
377600030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
377700030703      * per i colli originali in caso di spedizione over 25 colli
377800030703     c  n77              Eval      *In77 = *On
377900030703     c                   Eval      VidNba = §Ar5bNba
378000030703     c                   Eval      VidNb2 = §Ar5bNb2
378100030703If  2c                   If        §Ar5bTba <> *Zeros
378200030703     c                   Eval      wtba = §Ar5bTba
378300030703     c                   ExSr      Sr_DecTba
378400030703     c                   Eval      VidTba = wdestba
378500030703    2c                   EndIf
378600030703If  2c                   If        §Ar5bTb2 <> *Zeros
378700030703     c                   Eval      wtba = §Ar5bTb2
378800030703     c                   ExSr      Sr_DecTba
378900030703     c                   Eval      VidTb2 = wdestba
379000030703    2c                   EndIf
379100030703    1c                   EndIf
379200030703    0c                   EndIf
379300911129     C*
379400131120     C**   ARBACC        IFEQ      'S'
379500131120     C**                 MOVEL     'SI'          V5CACC
379600131120     C**                 ELSE
379700131120     C**                 MOVEL     'NO'          V5CACC
379800131120     C**                 END
379900941215     C     ARBICC        IFNE      ' '
380000941216     C     ARBICC        ANDNE     'R'
380100930218     C                   MOVEL     'SI'          VIDICC
380200930218     C                   END
380300911022     C*
380400911022     C                   MOVEL     ARBRMN        VIDRMN
380500940712     C                   MOVEL     ARBRMA        VIDRMA
380600911022     C                   MOVEL     ARBRMO        VIDRMO
380700911022     C                   MOVEL     ARBNCD        VIDNCD
380800911022     C                   MOVEL     ARBNCA        VIDNCA
380900911022     C                   MOVEL     ARBZNC        VIDZNC
381000051104     c                   select
381100051104     c                   when      arbffd='S'
381200051104     C                   MOVEL     'SI '         VIDFFD
381300051104     c                   when      arbffd='M'
381400051104     C                   MOVEL     'SAB'         VIDFFD
381500051104     C                   other
381600051104     C                   MOVEL     '   '         VIDFFD
381700051104     C                   ENDsl
381800051124     c
381900940719     C                   MOVEL     ARBNFV        V3CNFV
382000051124     c* richiamp pgm per decodifica
382100051124     C                   MOVEL     *BLANKS       VIDTCR
382200051124     c                   clear                   vidhcr
382300051124     c                   clear                   viddcr
382400051124     c                   if        arbdcr>0 or arbhcr>0
382500051124     c                   clear                   fnlv80ds
382600051124     c                   eval      ilv80aas=arbaas
382700051124     c                   eval      ilv80lnp=arblnp
382800051124     c                   eval      ilv80nrs=arbnrs
382900051124     c                   eval      ilv80nsp=arbnsp
383000051124     c                   eval      ilv80mgs=arbmgs
383100051124     c                   eval      ilv80tc1=arbtc1
383200051124     c                   eval      ilv80tc2=arbtc2
383300051124     c                   eval      ilv80dcr=arbdcr
383400051124     c                   eval      ilv80hcr=arbhcr
383500051124     c                   eval      ilv80tcr=arbtcr
383600051124     c                   call      'FNLV80R'
383700051124     c                   parm                    kpjba
383800051124     c                   parm                    fnlv80ds
383900051124     c
384000051124     c                   eval      viddcr=olv80dcr8
384100051124     c                   eval      vidtcr=olv80tcrd
384200911023     C                   MOVEL     ARBHCR        VIDHCR
384300051124     c                   endif
384400051124     c
384500070521     c* Se presente ora consegna, la edito
384600070413     c                   clear                   vidhmc
384700070413     c                   if        arbhmc>0
384800070413     c                   eval      vidhmc=%editw(arbhmc: '0 :  ')
384900070413     C****               MOVE      ARBHMC        VIDHMC
385000070413     c                   endif
385100911023     C                   MOVEL     ARBNTC        VIDNTC
385200911023     C                   MOVEL     ARBTMC        VIDTMC
385300940923     C*
385400940923     C* DECODIFICO IL TIPO MANCATA CONSEGNA
385500940923     C                   MOVEL     *BLANKS       V5DTMC
385600940923     C     ARBTMC        IFNE      *BLANKS
385700940923     C                   MOVEL     '2A'          COD
385800940923     C                   MOVEL     *BLANKS       KEY
385900940923     C                   MOVEL     ARBTMC        KEY
386000940923     C     KTAB1         CHAIN     TABEL00F                           31
386100940923     C  N31              MOVEL     TBLUNI        V5DTMC
386200940923     C                   ENDIF
386300940923     C*
386400911023     C                   MOVEL     ARBMIF        VIDMIF
386500911023     C                   MOVEL     ARBNMI        VIDNMI
386600940923     C*
386700940713     C                   MOVEL     ARBGGA        V4CGGA
386800940923     C                   MOVEL     *BLANKS       V4IGGA
386900940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
387000940923     C     V4CGGA        COMP      *BLANKS                            4242
387100940923     C*
387200940713     C                   MOVEL     ARBGMA        V4CGMA
387300940923     C                   MOVEL     *BLANKS       V4IGMA
387400940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
387500940923     C     V4CGMA        COMP      *BLANKS                            4343
387600940923     C*
387700960208     C                   MOVEL     ARBGVA        V4CGVA
387800960208     C                   MOVE      ' '           V4CGVA
387900940923     C                   MOVEL     *BLANKS       V4IGVA
388000940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
388100940923     C     V4CGVA        COMP      *BLANKS                            4444
388200940923     C*
388300070403     C* FLAG blocco CONSEGNA
388400070403     C                   SELECT
388500070403     c                   WHEN      arbfbc='A'
388600070403     c                   eval      v1dfbc='LascAVVISO'
388700070403     C                   SETON                                        27
388800070403     c                   WHEN      arbfbc='B'
388900070403     c  n17              eval      v1dfbc=' BLOCCATA '
389000070412     c   17              eval      v1dfbc=' RIMPALLO '
389100070412     C                   SETON                                        27
389200070403     c                   WHEN      arbfbc='*'
389300070403     c  n17              eval      v1dfbc=' BLOCCATA '
389400070403     c   17              eval      v1dfbc=' GIACENTE '
389500070403     C                   SETON                                        27
389600070403     C                   ENDSL
389700070403     c
389800070403     C**   ARBFBC        COMP      'A'                                    27
389900070403     C**   ARBFBC        COMP      'B'                                    17
390000070403     C**N17ARBFBC        COMP      '*'                                    17
390100940711     C* TIPO CONSEGNA PARTICOLARE 1
390200940804     C                   MOVEL     *BLANKS       V5DTC1
390300940711     C     ARBTC1        IFNE      ' '
390400911115     C                   MOVEL     '1P'          COD
390500911115     C                   MOVEL     *BLANKS       KEY
390600940711     C                   MOVEL     ARBTC1        KEY
390700940713     C     KTAB1         CHAIN     TABEL                              31
390800940804     C  N31              MOVEL     TBLUNI        V5DTC1
390900940804     C   31              MOVEL     *BLANKS       V5DTC1
391000930113     C                   END
391100940711     C* TIPO CONSEGNA PARTICOLARE 2
391200940804     C                   MOVEL     *BLANKS       V5DTC2
391300940804     C                   MOVEL     *BLANKS       V7DTC2
391400940711     C     ARBTC2        IFNE      ' '
391500940711     C                   MOVEL     '1P'          COD
391600940711     C                   MOVEL     *BLANKS       KEY
391700940711     C                   MOVEL     ARBTC2        KEY
391800940713     C     KTAB1         CHAIN     TABEL                              31
391900940804     C  N31              MOVEL     TBLUNI        V5DTC2
392000940804     C   31              MOVEL     *BLANKS       V5DTC2
392100940804     C  N31              MOVEL     TBLUNI        V7DTC2
392200940804     C   31              MOVEL     *BLANKS       V7DTC2
392300940711     C                   END
392400930113     C* TIPO MANCATA CONSEGNA
392500930113     C                   MOVEL     *BLANKS       DESCCA
392600930113     C     ARBCCA        IFNE      ' '
392700930705     C                   Z-ADD     1             C
392800930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
392900930705     C   31              MOVEL     DCA(C)        ALF12            12
393000930705     C  N31              MOVEL     *BLANKS       ALF12
393100930113     C                   MOVEL     '-'           DESCCA
393200930113     C                   MOVE      ALF12         DESCCA
393300930113     C                   END
393400911022     C* GIRO DATA SPEDIZIONE
393500940712     C                   Z-ADD     ARBDSP        G02INV
393600940712     C                   Z-ADD     *ZEROS        G02DAT
393700940712     C                   MOVEL     '3'           G02ERR
393800940712     C                   CALL      'XSRDA8'
393900940712     C                   PARM                    WLBDAT
394000940719     C                   Z-ADD     G02DAT        V3CDSP
394100070403     c*
394200911022     C* DATA BORDERO'
394300070403     c                   if        arbdbr>0
394400070403     c     *iso          movel     arbdbr        dataeur
394500070403     c                   movel     dataeur       datadmy
394600070404     c                   movel     dataeur       w08dbr            8 0
394700070404     c                   movel     datadmy       v06dbr
394800070403     c                   else
394900070404     c                   clear                   v06dbr
395000070403     c                   endif
395100070404     c
395200070404     c* Prendo da fnblp i dati ritiro merce
395300070403     c     karb2         chain     fnblp01l
395400070403     c                   if        %found(fnblp01l) and blpdrt>0
395500070403     c     *iso          movel     blpdrt        dataeur
395600070403     c                   movel     dataeur       datadmy
395700070404     c                   movel     datadmy       v06drt
395800070404     c                   movel     blpnrt        v7cnrt
395900070406     c                   movel     blpctm        v3cctm
396000070404     c                   if        blpfpp='M'
396100070410     c                   eval      v7dfppr='MAT'
396200070404     c                   else
396300070410     c                   eval      v7dfppr='POM'
396400070404     c                   endif
396500070404     C                   MOVEL     BLPPDR        V7CPDR
396600070404     C                   movel     BlpPdr        kpdpdr
396700070404     C                   move      'A'           kpdtip
396800070404     C     kfapd         chain     fiapd01L
396900070404     c                   if        %found(fiapd01l)
397000070404     C                   MOVEL     APDRSC        V7DPDR
397100070404     c                   else
397200070404     C                   MOVEL     *BLANKS       V7DPDR
397300070404     c                   endif
397400070404     c
397500070404     c
397600070403     c                   else
397700070404     c                   clear                   v06drt
397800070404     c                   clear                   v7cnrt
397900070406     c                   clear                   v3cctm
398000070504     c                   clear                   v7dfppr
398100070404     c                   clear                   v7dpdr
398200070404     c                   clear                   v7cpdr
398300070403     c                   endif
398400070403     c
398500961112     C* SOLO PER LE BOLLE EXPORT VISUALIZZO LA DATA STAMPA MANIFEST
398600070404     C                   CLEAR                   V06DT3
398700070413     c                   if        *in16 and arbdt3>0  and arbdt3<>99999999
398800070404     c     *iso          movel     arbdt3        dataeur
398900070404     c                   movel     dataeur       datadmy
399000070404     c                   movel     datadmy       v06dt3
399100961112     C                   ENDIF
399200940713     C* DATA PARTENZA PRIMO COLLO
399300070404     c                   if        arbdpc>0
399400070404     c     *iso          movel     arbdpc        dataeur
399500070404     c                   movel     dataeur       datadmy
399600070404     c                   movel     datadmy       v06dpc
399700070404     c                   movel     dataeur       w08dpc
399800070404     c                   else
399900070404     c                   clear                   v06dpc
400000070404     c                   clear                   w08dpc            8 0
400100070404     c                   endif
400200940713     C* DATA PARTENZA ULTIMO COLLO
400300070403     C**                 Z-ADD     ARBDUC        G02INV
400400070403     C**                 Z-ADD     *ZEROS        G02DAT
400500070403     C**                 MOVEL     '3'           G02ERR
400600070403     C**                 CALL      'XSRDA8'
400700070403     C**                 PARM                    WLBDAT
400800070403     C**                 Z-ADD     G02DAT        V3CDUC
400900070403     c                   if        arbduc>0
401000070404     c     *iso          movel     arbduc        dataeur
401100070403     c                   movel     dataeur       datadmy
401200070404     c                   movel     datadmy       v06duc
401300070404     c                   movel     dataeur       w08duc
401400070403     c                   else
401500070404     c                   clear                   v06duc
401600070404     c                   clear                   w08duc            8 0
401700070403     c                   endif
401800911022     C* DATA ENTRATA AL TRANSITO
401900070404     c                   if        arbdet>0
402000070404     c     *iso          movel     arbdet        dataeur
402100070404     c                   movel     dataeur       datadmy
402200070404     c                   movel     datadmy       v06det
402300070404     c                   else
402400070404     c                   clear                   v06det
402500070404     c                   endif
402600911022     C* DATA USCITA DAL TRANSITO
402700070404     c                   if        arbdut>0
402800070404     c     *iso          movel     arbdut        dataeur
402900070404     c                   movel     dataeur       datadmy
403000070404     c                   movel     datadmy       v06dut
403100070404     c                   else
403200070404     c                   clear                   v06dut
403300070404     c                   endif
403400930113     C* DATA ARRIVO MERCE
403500070403     c                   if        arbdam>0
403600070403     c     *iso          movel     arbdam        dataeur
403700070403     c                   movel     dataeur       datadmy
403800070404     c                   movel     datadmy       v06dam
403900070403     c                   else
404000070404     c                   clear                   v06dam
404100070403     c                   endif
404200131121
404300930504     C* DATA CONSEGNA MERCE REALE
404400070404     c                   if        arbdcm>0
404500070413     C                   SETON                                        69
404600070404     c     *iso          movel     arbdcm        dataeur
404700070404     c                   movel     dataeur       datadmy
404800070404     c                   movel     dataeur       v08dcm
404900070404     c                   movel     datadmy       v06dcm
405000070404     c                   else
405100070404     c                   clear                   v06dcm
405200070404     c                   clear                   v08dcm
405300070404     c                   endif
405400131121
405500930505     C* DATA CONSEGNA PREVISTA ESTERNA/ORE DI RITARDO
405600070404     C                   Z-ADD     0             v08DCE
405700140113     C                   Z-ADD     0             amgDCE            8 0
405800070404     C                   Z-ADD     0             v06dce
405900940711     C                   Z-ADD     0             VIDNRC
406000131121     C*******            SETON                                        68
406100131121     C                   MOVEL     ' '           D98TLA
406200131121     C                   MOVEL     'A'           D98TBO
406300131121     C                   MOVEL     ARBAAS        D98AAS
406400131121     C                   MOVEL     ARBLNP        D98LNP
406500131121     C                   MOVEL     ARBNRS        D98NRS
406600131121     C                   MOVEL     ARBNSP        D98NSP
406700131121     C*
406800131121     C                   CALL      'TNSD99R'
406900131121     C                   PARM                    TNSD99DS
407000140110     c
407100140110     c* Mi salvo data teorica consegna
407200140110     c                   eval      s_arbdei=d98dei
407300140110     c                   eval      s_arboei=d98oei
407400150728     c
407500150728     c* memorizzo ore trazione è ore consegna cliente da passare al trulorsr
407600150728     c                   eval      s_arbttc=d98ttc
407700150728     c                   eval      s_arbtcc=d98tcc
407800131121     C*
407900140110    1C     ARBDCE        IFEQ      0
408000131121    2c                   if        d98dce>0
408100140113     c                   eval      amgdce=d98dce
408200131121     c     *iso          movel     d98dce        dataeur
408300131121     c                   movel     dataeur       datadmy
408400131121     c                   movel     dataeur       v08dce
408500131121     c                   movel     datadmy       v06dce
408600131121   x2c                   else
408700131121     c                   clear                   v08dce
408800131121     c                   clear                   v06dce
408900131121    2c                   endif
409000131121     c
409100131121     C                   Z-ADD     D98NRC        VIDNRC
409200131121     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
409300131121     C*   CONSEGNA PREVISTA
409400131121     c  n69              eval      vidhmc=%editw(d98hce: '0 :  ')
409500131121     C*
409600131121     C* SE COMODO = 1  E' STATA FATTA CALL PGM
409700131121     C                   MOVEL     '1'           PA098
409800131121   x1C                   ELSE
409900140113     c                   eval      amgdce=arbdce
410000070404     c     *iso          movel     arbdce        dataeur
410100070404     c                   movel     dataeur       datadmy
410200070404     c                   movel     dataeur       v08dce
410300070404     c                   movel     datadmy       v06dce
410400070404     c
410500940711     C                   Z-ADD     ARBNRC        VIDNRC
410600131121    1C                   END
410700131121     c
410800911023     C* DATA MANDATO INTROITO
410900940712     C                   Z-ADD     ARBDMI        G02INV
411000940712     C                   Z-ADD     *ZEROS        G02DAT
411100940712     C                   MOVEL     '3'           G02ERR
411200940712     C                   CALL      'XSRDA8'
411300940712     C                   PARM                    WLBDAT
411400940712     C                   Z-ADD     G02DAT        VIDDMI
411500930113     C* DATA PRIMA CONSEGNA PARZIALE
411600930113     C                   Z-ADD     0             VIDDCP
411700930113     C     ARBDCP        IFGT      0
411800940712     C                   Z-ADD     ARBDCP        G02INV
411900940712     C                   Z-ADD     *ZEROS        G02DAT
412000940712     C                   MOVEL     '3'           G02ERR
412100940712     C                   CALL      'XSRDA8'
412200940712     C                   PARM                    WLBDAT
412300940712     C                   Z-ADD     G02DAT        VIDDCP
412400930113     C                   END
412500911023     C* DATA DISTINTA
412600940712     C                   Z-ADD     ARBDDC        G02INV
412700940712     C                   Z-ADD     *ZEROS        G02DAT
412800940712     C                   MOVEL     '3'           G02ERR
412900940712     C                   CALL      'XSRDA8'
413000940712     C                   PARM                    WLBDAT
413100940712     C                   Z-ADD     G02DAT        VIDDDC
413200911022     C* CODICE PADRONCINO CONSEGNA
413300940712     C                   MOVEL     ARBPDC        VIDPDC
413400940712     C                   MOVEL     *BLANKS       DESPDC
413500940712     C                   MOVEL     *BLANKS       DESPD1
413600940712     C     ARBPDC        IFGT      0
413700021203     C                   movel     arbpdc        kpdpdr
413800021203     C                   move      'A'           kpdtip
413900021203     C     kfapd         chain     fiapd01L                           31
414000940714     C  N31              MOVEL     APDRSC        DESPDC
414100940714     C  N31              MOVEL     APDRSC        DESPD1
414200930705     C                   END
414300950324     C* PRESTAZIONE PADRONCINO MATTINO O POMERIGGIO
414400950324     C                   MOVE      *BLANKS       FVVFPP
414500131120     C***                MOVE      *BLANKS       V5DFPP
414600070404     C                   MOVE      *BLANKS       V7DFPP
414700950327     C                   Z-ADD     ARBNDC        WNDC
414800020906     c                   Move      Arbifp        Wfgs
414900020906     c     Kfvv1         Chain     Fnfvv01l                           31
415000950324     C                   SELECT
415100991027     C                   WHEN      *IN31 = *on
415200991027     C                   WHEN      FVVFPP = 'M'
415300131120     C**                 eval      V5DFPP = 'MATTINO'
415400070504     C                   eval      V7DFPP = 'Mat'
415500991027     C                   WHEN      FVVFPP = 'P'
415600131120     C**                 eval      V5DFPP = 'POMERIGGIO'
415700070504     C                   eval      V7DFPP = 'Pom'
415800950324     C                   ENDSL
415900131119     c* controllo se ha PDA
416000131119     C                   Z-ADD     ARBNDC        kNDC6
416100131120     c                   clear                   vidpda
416200131120     c     Kdst          Chain     Fidst01l
416300131120     c                   if        %found(fidst01l)
416400131119     C                   movel     dstflr        DDStflr
416500131120     c                   if        (§dsttstpda= 'C' or  §dsttstpda= 'E') or
416600131120     c                             (dstpda<>'C' and  dstpda<>'E')
416700131120     c                   eval      vidpda='NO PDA'
416800131120     c                   movel     ri            ATRpda
416900131121     c                   else
417000131121     c                   eval      vidpda='SI PDA'
417100131121     c                   movel     ri            ATRpda
417200131119     c                   endif
417300131119     c                   endif
417400131108     c
417500131108     c* Se bolla in distinta, verifico quanti colli
417600131108     c                   clear                   v7dncl_co
417700131115     c                   clear                   v7dncl_de
417800131108     c                   clear                   wcol_D
417900131108     c                   clear                   wcol_I
418000131108    1c                   if        arbndc>0 and arbddc>0 and arbfbc=' '
418100131108     c* se ritorno all'incasso lo scrivo
418200131108     c
418300131108    2c                   if        arbdcm>0  and (arbicc='R' or arbicc=' ' or
418400131108     c                                  arbica=' ' or arbica='R')
418500131115     c                   eval      v7dncl_de='     IN consegna :'
418600131108     c                   eval      v7dncl_co='PER INCASSO'
418700131108    2c                   endif
418800131108     c
418900131108     c* Se colli da consegnare
419000131108    2c                   if        arbdcm=0
419100131108     C                   Z-ADD     ARBNDC        knfv
419200131108     c                   Move      Arbifp        Wfgs
419300131108     c
419400131108     c     karb2         setll     fnart01l
419500131108     c     karb2         reade     fnart01l
419600131108     c
419700131108    3c                   dow       not %eof(fnart01l)
419800131108     c                   if        artdcm=0
419900131108     c                   add       1             wcol_d
420000131108     c     kbrv          chain     fnbrv01l
420100131108     c                   if        %found(fnbrv01l)
420200131108     c                   add       1             wcol_I
420300131108     c                   endif
420400131108     c                   endif
420500131108     c
420600131108     c
420700131108     c     karb2         reade     fnart01l
420800131108    3c                   enddo
420900131108
421000131108    3c                   if        wcol_d>0
421100131115     c                   eval      v7dncl_de='Colli in consegna:'
421200131108    4c                   if        wcol_i=0
421300131108     c                   eval      v7dncl_co='0/'+%trim(%editc(wcol_d:'Z'))
421400131108     c                   else
421500131108     c                   eval      v7dncl_co=%trim(%editc(wcol_I:'Z')) +'/' +
421600131108     c                                       %trim(%editc(wcol_d:'Z'))
421700131108    4c                   endif
421800131108    3c                   endif
421900131108    c
422000131108    2c                   endif
422100131108    1c                   endif
422200131108     c
422300131108     c* Visualizzo ultimo evento
422400131108     c                   clear                   v7duevb
422500131114     c* prima vedo se c'e' da PDA
422600131114     c                   move      arbndc        pctndc
422700140224     c                   move      arbifp        pctfgs
422800131114     c                   move      'CET'         ttrd
422900131114     c     kcet          setgt     fipct02l
423000131114     c     kcet          readpe    fipct02l
423100140310     c                   if        not %eof(fipct02l)
423200131114     c                   movel     pctdati       fipctcetds
423300131114     c* consegnata
423400131114     c                   if        §pctcmc = ' '
423500131114     c                   MOVE      '  C'         evbcev                         *causale evento
423600131114     c                   else
423700131114     c* mancata consegna
423800131114     c                   movel     §PCTcmc       evbcev
423900131114     c                   endif
424000131114     c                   z-add     §pctdata      evbdev
424100131114     c                   movel     §pctora       evbhev
424200131114
424300131114     c                   exsr      Decodi_EVB
424400131114
424500131114     c                   else
424600131114     c*
424700131108     C                   Z-ADD     ARBAAS        KAAS$
424800131108     C                   Z-ADD     ARBLNP        KLNP
424900131108     C                   Z-ADD     ARBNRS        KNRS
425000131108     C                   Z-ADD     ARBNSP        KNSP
425100131108     C     KEVB1         SETgt     FNEVB01L
425200131108     C     KEVB1         readpe    FNEVB01L
425300131108     c
425400131114    1c                   if        not %eof(fnevb01l)
425500131114     c                   exsr      Decodi_EVB
425600131114    1c                   endif
425700131114    1c                   endif
425800911022     C*
425900911022     C* SE ESISTE AGGANCIO 2° BOLLA
426000991026     C                   Z-ADD     ARBAAS        KAAS
426100991026     C                   Z-ADD     ARBLNP        KLNP
426200991026     C                   Z-ADD     ARBNRS        KNRS
426300991026     C                   Z-ADD     ARBNSP        KNSP
426400991026     C                   MOVEL     '2'           KTRC
426500991027     C     KAR61         CHAIN     FIAR6000                           31
426600911023     C*
426700040216if  1C                   IF          %FOUND
426800991027     C                   eval      *in22 = *on
426900991027     C                   MOVEL     AR6CEI        V9CCEI
427000991027     C                   MOVEL     AR6NFT        V9CNFT
427100991027     C                   MOVEL     AR6FIV        V9CFIV
427200991027      *
427300991027     C                   eval      V9CDIV = AR6div
427400991027     C                   eval      WDIV = AR6div
427500991027     C                   exsr      CTR_VALUTA
427600991027      *
427700001128     C                   clear                   v9dfim
427800001128     C                   clear                   v9dfi2
427900991027     c                   if        AR6fim = 'P'
428000991027     c                   eval      V9Dfim = 'PARTENZA'
428100040126     c                   eval      V9Dfi2 = 'Immesse in'
428200991027     c                   endif
428300991027      * Varia G
428400040130     c                   clear                   W0133
428500040130     c                   clear                   v9csv1
428600040130     c                   clear                   v9cva1
428700040130     c                   clear                   v9csv2
428800040130     c                   clear                   v9cva2
428900040130     c                   clear                   v9csv3
429000040130     c                   clear                   v9cva3
429100040130     c                   clear                   v9csv4
429200040130     c                   clear                   v9cva4
429300041130     c                   clear                   v9csv5
429400041130     c                   clear                   v9cva5
429500041130     c                   clear                   v9csv6
429600041130     c                   clear                   v9cva6
429700040130     c                   clear                   v9civa
429800040130
429900040122      * Carico le varie
430000040122     c                   Movel     Ar6Sv1        wsvn
430100040122     c                   Z-add     Ar6Va1        wvan
430200040122     c                   ExSr      Car_Varie1
430300040122
430400040122     c                   Movel     Ar6Sv2        wsvn
430500040122     c                   Z-add     Ar6Va2        wvan
430600040122     c                   ExSr      Car_Varie1
430700040122
430800040122     c                   Movel     Ar6Sv3        wsvn
430900040122     c                   Z-add     Ar6Va3        wvan
431000040122     c                   ExSr      Car_Varie1
431100040122
431200040122     c     Kar61         Setll     Fiar7000
431300040216do  2c                   Do        *Hival
431400040122     c     Kar61         Reade     Fiar7000
431500040122     c                   If        %Eof(Fiar701l)
431600040122     c                   Leave
431700040122     c                   EndIf
431800040122     c                   Movel     Ar7Svn        wsvn
431900040122     c                   Z-add     Ar7Van        wvan
432000040216     c                   ExSr      Car_Varie1
432100040216e   2c                   EndDo
432200040122
432300991027      * TOTALE FATTURA
432400991027     c                   clear                   W0133
432500991027     c                   IF          AR6ift > *zeros
432600991027     c                   z-add     AR6ift        W0133
432700991027     c                   eval      Wtstdec = §cvfdc
432800991027     c                   exsr      EDTimporti
432900991027     c                   movel     IMPeditato    V9cift
433000001128     C                   else
433100001128     C                   clear                   v9cift
433200991027     c                   endif
433300991027      *
433400991027     c                   If         AR6dft > *zeros
433500991027     C     *iso          movel     AR6DFT        DATA_EUR
433600991027     C     *eur          movel     data_eur      W0080
433700991027     C                   eval      V9cdft = %EDITC(W0080:'Y')
433800001128     C                   else
433900001128     C                   clear                   v9cdft
434000991027     c                   Endif
434100991027      *
434200991027     c                   eval      V9Cksc = AR6ksc
434300991027     c                   eval      V9Cctr = AR6ctr
434400991027     c                   move      v9cksc        W0040
434500991027     c                   If        W0040 = 9999
434600991027     c                   eval      V9Dksc = arbrsd
434700991027     c                   Else
434800991027     c                   eval      i69kac = V9Cksc
434900991027     c                   exsr      CTR_ANAGRA
435000991027     c                   eval      V9Dksc = acorag
435100991027     c                   Endif
435200991027      *
435300991027     C                   eval      COD = 'TR'
435400991027     C                   movel     V9CCTR        UNOCTR            1
435500991027     C                   eval      KEY = unoctr
435600991027     C     KTAB1         CHAIN     TABEL00F                           31
435700991027     C                   movel     TBLUNI        V9DCTR
435800991027      *
435900930114     C* ESENZIONE IVA
436000001128     C                   clear                   v9dcei
436100991111     C     AR6CEI        IFNE      *blanks
436200940721     C                   MOVEL     'EI'          COD
436300991111     C                   MOVEL(P)  AR6CEI        KEY
436400940719     C     KTAB1         CHAIN     TABEL00F                           31
436500991027     C  N31              MOVEL     TBLUNI        V9DCEI
436600940719     C                   ENDIF
436700991027      *
436800040216e   1C                   ENDIF
436900930525     C*
437000991027     C* PER I DATI SULLA TASSAZIONE 1a BOLLA, DEVO CERCARE SU FIAR600F
437100940905     C                   SETOFF                                       24
437200110705     c                   clear                   atrbl4a
437300110705     c
437400110705     c                   clear                   v6car4ksc
437500110705     c                   clear                   v6dar4ksc
437600110705     c                   clear                   v6car4ctr
437700111021     c                   clear                   VIDimv
437800111021     c                   clear                   VIDpor
437900111021     c                   clear                   VIDift
438000111021     c                   clear                   VIDdiv
438100111021     c                   clear                   VIDfim
438200111021     C                   clear                   VIDCEI
438300111021     C                   clear                   VIDNFT
438400111021     C                   clear                   VIDFIV
438500111021     C                   clear                   DESCEI
438600111021     C                   clear                   viddft
438700111021     C                   clear                   vidIVA
438800111021      *
438900111021      * CERCO LE VARIE: LE PRIME 3 SU FIAR600F, EVENTUALI  ALTRE SU FIAR700F
439000111021     C                   eval      *in35 = *on
439100131107     C                   WRITE     LRI6C06
439200111021     C                   eval      *in35 = *off
439300111021      *
439400111021     c                   clear                   NR6
439500111021     c                   clear                   VIDiva
439600110705     c
439700940712     C                   Z-ADD     ARBAAS        KAAS
439800940712     C                   Z-ADD     ARBLNP        KLNP
439900940712     C                   Z-ADD     ARBNRS        KNRS
440000940712     C                   Z-ADD     ARBNSP        KNSP
440100110705     C                   MOVEL     'A'           KTRC
440200110705     C     KAR41         CHAIN     FiAR401L
440300110705if  1C                   IF        not %FOUND
440400110705     c                   eval      atrbl4a=nodi
440500110705     c                   else
440600110705     c                   movel     ar4not        dsbl4a
440700110705     c                   if        §b4aks>0
440800110705     C                   eval      *in24 = *on
440900110705     c                   eval      v6car4ksc=§b4aks
441000110705     c                   eval      i69kac = v6car4ksc
441100110705     c                   exsr      CTR_ANAGRA
441200110705     c                   eval      v6dar4ksc = acorag
441300110705     c                   if        §b4act<>*blanks
441400110705     c                   movel     §b4act        v6car4ctr
441500110705     c                   endif
441600110705     c* non c'e' cod cliente x tassaz.
441700110705     c                   else
441800110705     c                   eval      atrbl4a=nodi
441900110705     c                   endif
442000110705     c                   endif
442100110705     c
442200991026     C                   MOVEL     '1'           KTRC
442300991027     C     KAR61         CHAIN     FIAR6000
442400110705     c
442500110705     c                   if        %found(fiar601l)
442600110705     c
442700110705      * CLIENTE E TARIFFA FATTURAZIONE
442800110705     c                   IF        *IN33 = *ON
442900110705     c                   eval      v6cksc = arbksc
443000110705     c                   eval      v6cctr = arbctr
443100110705     c                   eval      v6dksc = arbrsm
443200110705     c                   ELSE
443300110705     c                   eval      v6cksc = AR6ksc
443400110705     c                   eval      v6cctr = AR6ctr
443500110705     c                   move      v6cksc        W0040
443600110705     c                   if        W0040 = 9999
443700110705     c                   eval      v6dksc = arbrsd
443800110705     c                   else
443900110705     c                   eval      i69kac = v6cksc
444000110705     c                   exsr      CTR_ANAGRA
444100110705     c                   eval      v6dksc = acorag
444200110705     c                   endif
444300110705     c                   ENDIF
444400110705      *   decodifica tariffa
444500110705     C                   eval      COD = 'TR'
444600110705     C                   MOVEL     V6CCTR        UNOCTR            1
444700110705     C                   MOVEL(P)  UNOCTR        KEY
444800110705     C     KTAB1         CHAIN     TABEL00F                           31
444900110705     C                   MOVEL     TBLUNI        V6DCTR
445000110705     c
445100110705     c                   endif
445200991027      *
445300040216if  1C                   IF        %FOUND  and
445400991027     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
445500991027     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
445600991027     C                              AR6sv3 <> *blanks)
445700991027      *
445800991027      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SE PRESENTE ALMENO UNO DEI CAMPI
445900991027     C                   EVAL      *IN24 = *ON
446000991027      *
446100991027     C                   eval      VIDDIV = AR6div
446200991027     C                   eval      WDIV = AR6div
446300991027     C                   exsr      CTR_VALUTA
446400991027      *
446500991027     c                   IF          ar6por > *zeros
446600991027     c                   z-add     AR6por        W0133
446700991027     c                   eval      Wtstdec = §cvfdc
446800991027     c                   exsr      EDTimporti
446900991027     c                   movel     IMPeditato    VIDpor
447000991128     c                   else
447100991128     c                   clear                   VIDpor
447200991027     c                   ENDIF
447300991027      *
447400991027     c                   IF          ar6imv > *zeros
447500991027     c                   z-add     AR6imv        W0133
447600991027     c                   eval      Wtstdec = §cvfdc
447700991027     c                   exsr      EDTimporti
447800991027     c                   movel     IMPeditato    VIDimv
447900991128     c                   else
448000991128     c                   clear                   VIDimv
448100991027     c                   ENDIF
448200991027      *
448300991027     c                   IF          ar6ift > *zeros
448400991027     c                   z-add     AR6ift        W0133
448500991027     c                   eval      Wtstdec = §cvfdc
448600991027     c                   exsr      EDTimporti
448700991027     c                   movel     IMPeditato    VIDift
448800991128     c                   else
448900991128     c                   clear                   VIDift
449000991027     c                   ENDIF
449100991027      *
449200991027     c                   IF        AR6fim = 'P'
449300991027     c                   eval      VIDfim = 'PARTENZA'
449400991128     c                   else
449500991128     c                   clear                   VIDfim
449600991027    2C                   ENDIF
449700991027      *
449800991027     C                   MOVEL     AR6CEI        VIDCEI
449900991027     C                   MOVEL     AR6NFT        VIDNFT
450000991027     C                   MOVEL     AR6FIV        VIDFIV
450100991027      *
450200991027     C                   IF        AR6CEI <>  *BLANKS
450300940808     C                   MOVEL     'EI'          COD
450400991027     C                   MOVEL(P)  AR6CEI        KEY
450500940808     C     KTAB1         CHAIN     TABEL00F                           31
450600940808     C  N31              MOVEL     TBLUNI        DESCEI
450700991128     c                   else
450800991128     c                   clear                   DESCEI
450900940808     C                   ENDIF
451000991027      *
451100991027      * DATA FATTURA DA GIRARE
451200991027     C                   IF         ar6dft > *zeros
451300991027     C     *iso          movel     AR6DFT        DATA_EUR
451400991027     C     *eur          movel     data_eur      W0080
451500991027     C                   eval      VIDdft = %EDITC(W0080:'Y')
451600991128     c                   else
451700991128     c                   clear                   VIDdft
451800991027     C                   ENDIF
451900991027      *
452000991027      *
452100991027     C                   MOVEL     ar6SV1        WSVN
452200991027     C                   MOVEL     ar6VA1        WVAN
452300991027     c                   exsr      car_varie
452400991027      *
452500991027     C                   MOVEL     ar6SV2        WSVN
452600991027     C                   MOVEL     ar6VA2        WVAN
452700991027     c                   exsr      car_varie
452800991027      *
452900991027     C                   MOVEL     ar6SV3        WSVN
453000991027     C                   MOVEL     ar6VA3        WVAN
453100991027     c                   exsr      car_varie
453200991027      *
453300991027      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
453400040216if  2C                   if          ar6SV3 <> *blanks
453500991027     C     KAR61         SETLL     FIAR7000
453600991027     C     KAR61         READE     FIAR7000
453700040216do  3C                   DOW       %EOF = *OFF
453800991027     C                   MOVEL     AR7SVN        WSVN
453900991027     C                   MOVEL     AR7VAN        WVAN
454000991027     c                   exsr      car_varie
454100991027     C     KAR61         READE     FIAR7000
454200040216e   3C                   ENDDO
454300040216e   2C                   endif
454400110705e   1C                   ENDIF
454500991027      *
454600991027      * Se non ho caricato record ne carico uno vuoto
454700110705     c                   IF        NR6 = *ZEROS  and *in24
454800991027     C                   clear                   V6CDE1
454900991027     C                   clear                   V6CVA1
455000991027     C                   clear                   V6CSV1
455100991027     C                   add       1             NR6
455200131107     C                   WRITE     LRI6S06
455300991027     c                   ENDIF
455400991027      *
455500991027      *
455600941206     C* 70 ON - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
455700941206     C     *IN33         IFEQ      '0'
455800941206     C     AR6NFT        ANDGT     0
455900941206     C     *IN22         OREQ      '1'
456000991111     C     AR6NFT        ANDGT     0
456100941206     C                   SETON                                        70
456200941206     C                   END
456300940712     C*
456400940719     C* PRELEVO LE NOTE DAL FILE APPOSITO
456500940719     C                   MOVEL     ARBAAS        KAAS
456600940719     C                   MOVEL     ARBLNP        KLNP
456700940719     C                   MOVEL     ARBNRS        KNRS
456800940719     C                   MOVEL     ARBNSP        KNSP
456900940719     C                   MOVEL     '8'           KTRC
457000060216     C     KAR41         CHAIN     FiAR401L                           31
457100940719     C  N31              MOVEL     AR4NOT        V4CNO1
457200940802     C   31              MOVEL     *BLANKS       V4CNO1
457300940719     C                   MOVEL     '9'           KTRC
457400060216     C     KAR41         CHAIN     FiAR401L                           31
457500940719     C  N31              MOVEL     AR4NOT        V4CNO2
457600940802     C   31              MOVEL     *BLANKS       V4CNO2
457700940719     C*
457800941110     C* ESTENSIONE RAGIONE SOCIALE DESTINATARIO
457900941110     C                   MOVEL     'D'           KTRC
458000060216     C     KAR41         CHAIN     FiAR401L                           31
458100950125     C  N31              MOVEL     AR4NOT        V4CNOT
458200950125     C   31              MOVEL     *BLANKS       V4CNOT
458300971015     C*
458400971015     C* SOLO PER EXPORT DATA DI STAMPA SU BORDERO' ARRIVI
458500070404     C***  *IN16         IFEQ      *ON
458600070404     C***                MOVEL     'A'           KTRC
458700070404     C***  KAR41         CHAIN     FiAR401L                           31
458800070404     C**N31              MOVE      AR4NOT        WDATA             8
458900070404     C***31              MOVEL     *ZEROS        WDATA
459000070404     C***  WDATA         IFGT      *ZEROS
459100070404     C***                MOVEL     WDATA         G02INV
459200070404     C***                Z-ADD     *ZEROS        G02DAT
459300070404     C***                MOVEL     '3'           G02ERR
459400070404     C***                CALL      'XSRDA8'
459500070404     C***                PARM                    WLBDAT
459600070404     C***                Z-ADD     G02DAT        V3CDST
459700070404     C***                ENDIF
459800070404     C***                ENDIF
459900940720     C*
460000051114     C* IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU FiAR900F
460100940720     C* CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
460200940720     C                   MOVEL     *BLANKS       VIDTIC
460300940720     C                   MOVEL     *ZERO         VIDCAS
460400940720     C                   MOVEL     *BLANKS       VIDVCA
460500940720     C                   MOVEL     *BLANKS       V4CGCA
460600940923     C                   MOVEL     *BLANKS       V4IGCA
460700091214     c                   setoff                                       65
460800940720     C     §3AFCA        IFGT      *ZERO
460900051128     C     Ksped         CHAIN     FiAR901L                           31
461000940720     C  N31              DO
461100940720     C                   MOVEL     AR9TIC        VIDTIC
461200940720     C                   MOVEL     AR9CAS        VIDCAS
461300940720     C                   MOVEL     AR9VCA        VIDVCA
461400940720     C                   MOVEL     AR9GCA        V4CGCA
461500091214     c                   seton                                        65
461600940720     C                   ENDDO
461700940720     C                   ENDIF
461800941215     C* ESISTE CONTRASSEGNO: VEDO SE ABILITATO
461900941215     C     VIDCAS        IFNE      0
462000941215     C                   SETON                                        29
462100941215     C                   END
462200940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
462300940923     C     V4CGCA        COMP      *BLANKS                            4141
462400940720     C*
462500940720     C* RIEMPO ANCHE ALCUNI CAMPI PER LE DS DI PASSAGGIO PARAMETRI
462600940720     C                   MOVEL     V3CDSP        PA1DSP
462700940720     C                   MOVEL     ARBRMN        PA1RMN
462800940720     C                   MOVEL     ARBLNA        PA1LNA
462900940802     C                   MOVEL     V3DLNA        PA1LN1
463000940720     C                   MOVEL     ARBRSD        PA1RSD
463100940906     C*-------------------------------------------------------------------------
463200991029      * GESTIONE INCASSO PARZIALE se assegnato o presente estensione bolla
463300991102     C                   clear                   v5cdiv
463400991102     c                   clear                   Wariimp
463500991102     c                   clear                   Wariipp
463600991102     c                   clear                   VIDimp
463700991102     c                   clear                   VIDipp
463800991102     C                   MOVEL     ARBFIP        VIDFIP
463900991102      *
464000991029     c  N33              eval      V5Cdiv = viddiv
464100991029     c   22              eval      V5Cdiv = v9cdiv
464200991029      *
464300040216if  1c                   if        V5CDIV <> *blanks
464400991029      *
464500991029     C                   eval      WDIV = V5Cdiv
464600991029     C                   exsr      CTR_VALUTA
464700991029      *
464800991029     C                   MOVEL     ARBAAS        KAAS
464900991029     C                   MOVEL     ARBLNP        KLNP
465000991029     C                   MOVEL     ARBNRS        KNRS
465100991029     C                   MOVEL     ARBNSP        KNSP
465200991029     C                   MOVEL     'A'           ktrc
465300991029      *
465400991029     c     kar61         setll     fiari000
465500991029     c     kar61         reade     fiari000
465600991029      *
465700040216do  2c                   DOW       not %EOF
465800040216if  3c                   if        v5cdiv = aridiv
465900991029     c                   add       ARIimp        Wariimp
466000991029     c                   add       ARIipp        Wariipp
466100040216x   3c                   else
466200991029      *
466300991029     C                   clear                   yeurcods
466400991029     C                   eval      YECDVI = aridiv
466500991029     C                   eval      YECDVO = V5Cdiv
466600991029     C                   eval      YECIMI = ARIimp
466700011022     C                   move      §CVDEC        YECDCO
466800991029     C                   CALL      'YEURCO'
466900991029     C                   PARM                    YEURcods
467000991029    6C                   if        YECESI = *blanks
467100991029     C                   add       YECIMO        WARIimp
467200991029    6C                   endif
467300991029      *
467400991029     C                   clear                   yeurcods
467500991029     C                   eval      YECDVI = aridiv
467600991029     C                   eval      YECDVO = V5Cdiv
467700991029     C                   eval      YECIMI = ARIipp
467800011022     C                   move      §CVDEC        YECDCO
467900991029     C                   CALL      'YEURCO'
468000991029     C                   PARM                    YEURcods
468100991029    6C                   if        YECESI = *blanks
468200991029     C                   add       YECIMO        WARIipp
468300991029    6C                   endif
468400991029      *
468500040216e   3c                   endif
468600991029      *
468700991029     c     kar61         reade     fiari000
468800040216e   2c                   enddo
468900991029      *
469000991029     c                   IF        WARIipp > *zeros
469100991029     c                   z-add     Wariipp       W0133
469200991029     c                   eval      Wtstdec = §cvfdc
469300991029     c                   exsr      EDTimporti
469400991029     c                   movel     IMPeditato    VIDipp
469500991029     c                   ENDIF
469600991029      *
469700991029     c                   IF        WARIimp > *zeros
469800991029     c                   z-add     Wariimp       W0133
469900991029     c                   eval      Wtstdec = §cvfdc
470000991029     c                   exsr      EDTimporti
470100991029     c                   movel     IMPeditato    VIDimp
470200991029     c                   ENDIF
470300991105      *
470400991105     c                   IF        VIDipp = *blanks  and  VIDimp = *blanks
470500991105     c                   clear                   V5CDIV
470600991105     c                   ENDIF
470700991029      *
470800040216e   1C                   endif
470900991029     C*-------------------------------------------------------------------------
471000951220     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
471100951220     C*
471200951220     C* PULISCO IL SFL
471300960926     C                   SETOFF                                       72
471400131107     C                   WRITE     LRI6C05
471500960926     C                   SETON                                        7172
471600951220     C*
471700060216     C* CHIAVE PER ACCEDERE AL FILE FiAR401L
471800951220     C                   MOVEL     ARBAAS        KAAS
471900951220     C                   MOVEL     ARBLNP        KLNP
472000951220     C                   MOVEL     ARBNRS        KNRS
472100951220     C                   MOVEL     ARBNSP        KNSP
472200951220     C*
472300951220     C                   Z-ADD     0             NRR5              4 0
472400951220     C                   MOVEL     '7K'          COD
472500951220     C*
472600060216     C     KAR42         SETLL     FiAR401L
472700060216     C     KAR42         READE     FiAR401L                               36
472800040216do  1C     *IN36         DOWEQ     *OFF
472900951220     C*
473000951220     C                   MOVEL(P)  AR4TRC        KEY
473100951220     C     KTAB1         CHAIN     TABEL                              36
473200951220     C*
473300951220     C     *IN36         IFEQ      *OFF
473400951220     C     TBLFLG        ANDEQ     ' '
473500951220     C                   MOVEL     TBLUNI        DS7K
473600951220     C     §7KVIS        IFEQ      'S'
473700951220     C                   MOVEL     §7KDES        VS5DES
473800951220     C                   MOVEL     AR4NOT        VS5NOT
473900951220     C                   ADD       1             NRR5
474000131107     C                   WRITE     LRI6S05
474100951220     C                   ENDIF
474200951220     C                   ENDIF
474300951220     C*
474400060216     C     KAR42         READE     FiAR401L                               36
474500040216e   1C                   ENDDO
474600951220     C*
474700960926     C* SE NON HO CARICATO NIENTE NON EMETTO IL SFLDSP PER EVITARE CHE
474800960926     C*  IL PGM DIA ERRORE DI SESSIONE O UNITA'
474900951220     C     NRR5          IFEQ      0
475000991105     C                   clear                   VS5DES
475100991105     C                   clear                   VS5NOT
475200991105     C                   ADD       1             NRR5
475300131107     C                   WRITE     LRI6S05
475400951220     C                   ENDIF
475500951220     C*
475600021216      * Riempio il campone in fondo alla videata con i tasti scelti
475700021216     C                   exsr      Ges_TastiFun
475800021217     c                   eval      FD07 = FDxx
475900021217     c                   eval      F24D07 = F24Dxx
476000911022     C                   ENDSR
476100131114      *-------------------------------------------------------------------------
476200131114      *  Decodifica EVENTO
476300131114      *-------------------------------------------------------------------------
476400131114     c     Decodi_evb    BEGSR
476500131114
476600131114     c                   if        evbcev='  C'
476700131114     c                   eval      §2ades='CONSEGNA'
476800131128     c* aggiungo anche firmatario
476900131128     c                   if        §PCTNOMFIR<>*blaNKS
477000131128     C                   EVAL      §2ADES=%TRIM(§2ADES)+' Firma:' +§PCTNOMFIR
477100131128     c                   endif
477200131128     c
477300131114     C                   else
477400131114     C                   MOVEL     '2A'          COD
477500131114     C                   MOVEL(p)  evbcev        KEY
477600131114     C     KTAB1         CHAIN     TABEL00F
477700131114    3c                   if        not %found(tabel00f)
477800131114     c                   clear                   ds2a
477900131114     c                   else
478000131114     c                   eval      ds2a=tbluni
478100131114    3c                   endif
478200131114    3c                   endif
478300131114     c
478400131114     c                   eval      v7duevb=evbcev+'-'+§2ades
478500131114
478600131114    3c                   if        evbdev>0
478700131114     c     *iso          movel     evbdev        dataeur
478800131114     c                   movel     dataeur       datadmy
478900131114     c                   movel     dataeur       w7cdev            8 0
479000131114
479100131114     c                   eval      indx=%checkr(' ':v7duevb)
479200131114    4c                   if        indx>46
479300131114     c                   eval      indx=46
479400131114     c                   else
479500131114     c                   eval      indx=indx+1
479600131114    4c                   endif
479700131114     c                   eval      wcarat=66-indx
479800131114     c                   eval      %subst(v7duevb:indx:wcarat)=' ' +'il ' +
479900131114     c                             %editw(W7cdev: '  /  /    ')
480000131114     c
480100131114    4c                   if        evbhev>0
480200131114     c                   eval      v7duevb=%trim(v7duevb)+' ' +
480300131114     c                                                 %editw(evbhev: '0 :  ')
480400131114    4c                   endif
480500131114     c
480600131114    3c                   endif
480700131114     c                   ENDSR
480800030703      *-------------------------------------------------------------------------
480900030703      *  Decoditifa tipo bancali
481000030703      *-------------------------------------------------------------------------
481100030703     c     Sr_DecTba     BegSr
481200030703     c                   Clear                   Tibs02Ds
481300030703     c                   Clear                   dTba
481400030703     c                   Movel     'C'           T02Mod
481500030703     c                   Movel     Knsif         T02Sif
481600030703     c                   Movel     'TBA'         T02Cod
481700030703     c                   Movel(p)  wtba          T02Ke1
481800030703     c                   Call      'TIBS02R'
481900030703     c                   Parm                    Kpjba
482000030703     c                   Parm                    Tibs02Ds
482100030703If  3c                   If        T02Err = *Blanks
482200030703     c                   Movel     T02Uni        dTba
482300030703    3c                   EndIf
482400030703     c                   Eval      wdestba = §TbaDesc
482500030703     c                   EndSr
482600021216      *-------------------------------------------------------------------------
482700021216      *  Imposto descrizione tasti funzione
482800021216      *-------------------------------------------------------------------------
482900021216     c     Ges_TastiFun  BEGSR
483000021217      *
483100021217      * per emettere i tasti senza condizionamenti ..
483200021216     c                   setoff                                       89
483300021216      *
483400021216      * Conta i caratteri riempiti dalle RigaTf1
483500021217     c                   z-add     0             $z                3 0
483600021216      *
483700021216      * Conta i caratteri riempiti dalle RigaTf2
483800021217     c                   z-add     0             $k                3 0
483900021216      *
484000021216      * Conta i caratteri riempiti dalle RigaTf3
484100021217     c                   z-add     0             $j                3 0
484200021216      *
484300021216      * Conta la posizione del campone da cui partire per impostare
484400021216      * la descrizione del tasto funzione
484500021216     c                   z-add     0             $y                2 0
484600021216      *
484700021217      * Stringhe che contengono le descrizioni dei tasti funzione
484800021216     c                   reset                   RigaTf1
484900021216     c                   reset                   RigaTf2
485000021216     c                   reset                   RigaTf3
485100070411      *
485200070411      *?F01?--> scelta bolla da assistenza clienti
485300070411     c                   IF        *in63 = *on
485400070411     c                   reset                   $Tf
485500070411     c                   movea     Tf01          $Tf
485600070411     c                   exsr      Rie_$Tf
485700070411     c                   else
485800070411      *?F01?--> scelta bolla generica
485900070411     c                   if        *in61 = *on
486000070411     c                   reset                   $Tf
486100070411     c                   movea     Tf01B         $Tf
486200070411     c                   exsr      Rie_$Tf
486300070411     c                   ENDIF
486400070411     c                   ENDIF
486500021216      *
486600070411      * F13 --> tassazione 1 bolla
486700021216     c                   IF        *in24 = *on
486800021217     c                   reset                   $Tf
486900021217     c                   movea     Tf13          $Tf
487000021217     c                   Exsr      Rie_$Tf
487100021217     C                   ENDIF
487200021216      *
487300070411      * F04 --> tassazione 2 bolla
487400021216     c                   IF        *in22 = *on
487500021217     c                   reset                   $Tf
487600021217     c                   movea     Tf04          $Tf
487700021217     c                   Exsr      Rie_$Tf
487800021217     C                   ENDIF
487900040220      *
488000070411      *?F16 --> assistenza clienti
488100040220     c                   IF        *in64 = *on
488200040220     c                   reset                   $Tf
488300040220     c                   movea     Tf16          $Tf
488400040220     c                   exsr      Rie_$Tf
488500040220     c                   ENDIF
488600021216      *
488700070411      * F05 --> eventi
488800021216     c                   IF        *in95 = *on
488900021217     c                   reset                   $Tf
489000021217     c                   movea     Tf05          $Tf
489100021217     c                   Exsr      Rie_$Tf
489200021217     C                   ENDIF
489300021216      *
489400021216      * F10
489500021217     c                   IF        *in98 = *on
489600021217     c                   reset                   $Tf
489700021217     c                   movea     Tf10          $Tf
489800021217     c                   Exsr      Rie_$Tf
489900021217     c                   ENDIF
490000021216      *
490100021216      * F14
490200021217     c                   IF        *in03 = *on
490300021217     c                   reset                   $Tf
490400021217     c                   movea     Tf14          $Tf
490500021217     c                   Exsr      Rie_$Tf
490600021217     c                   ENDIF
490700021216      *
490800021216      * F02
490900021217     c                   IF        *in04 = *on
491000021217     c                   reset                   $Tf
491100021217     c                   movea     Tf02          $Tf
491200021217     c                   Exsr      Rie_$Tf
491300021217     c                   ENDIF
491400110929      *
491500110930      * F22 --> Perfect Order
491600110929     c                   IF        *in86 = *on
491700110929     c                   reset                   $Tf
491800110929     c                   movea     Tf22          $Tf
491900110929     c                   Exsr      Rie_$Tf
492000110929     C                   ENDIF
492100021216      *
492200021216      * F11
492300021217     c                   IF        *in77 = *on
492400021217     c                   reset                   $Tf
492500021217     c                   movea     Tf11          $Tf
492600021217     c                   Exsr      Rie_$Tf
492700021217     c                   ENDIF
492800070404      *
492900070413      * F17 --> solo se partito com GEO
493000070413     c                   if        *in17
493100070404     c                   reset                   $Tf
493200070404     c                   movea     Tf17          $Tf
493300070404     c                   Exsr      Rie_$Tf
493400070413     c                   endif
493500021216      *
493600021216      * F07
493700021217     c                   IF        *in67 = *on
493800021217     c                   reset                   $Tf
493900021217     c                   movea     Tf07          $Tf
494000021217     c                   Exsr      Rie_$Tf
494100021217     c                   ENDIF
494200021216      *
494300021216      * F08
494400021217     c                   IF        *in74 = *on and *in16 = *on
494500021217     c                   reset                   $Tf
494600021217     c                   movea     Tf08          $Tf
494700021217     c                   Exsr      Rie_$Tf
494800021217     c                   ENDIF
494900130830
495000130830      * F09 --> Orari servizi non condizionato
495100131007     c                   IF        *in89 = *off and wabif9='S'
495200130930     c                   reset                   $Tf
495300130930     c                   movea     Tf09          $Tf
495400130930     c                   Exsr      Rie_$Tf
495500130930     c                   ENDIF
495600021216      *
495700021216      * F20 --> non condizionato
495800021217     c                   IF        *in89 = *off
495900021217     c                   reset                   $Tf
496000021217     c                   movea     Tf20          $Tf
496100021217     c                   Exsr      Rie_$Tf
496200021217     c                   ENDIF
496300021216      *
496400021216      * F21 --> non condizionato
496500021216     c                   IF        *in89 = *off
496600021217     c                   reset                   $Tf
496700021217     c                   movea     Tf21          $Tf
496800021217     c                   Exsr      Rie_$Tf
496900021217     c                   ENDIF
497000021218      *
497100021218      * F19 --> non condizionato
497200021218     c                   IF        *in89 = *off
497300021218     c                   reset                   $Tf
497400021218     c                   movea     Tf19          $Tf
497500021218     c                   Exsr      Rie_$Tf
497600021218     c                   ENDIF
497700070420     c
497800130830      * F06 --> solo se partito con GEO
497900070420     c                   if        *in17
498000070420     c                   reset                   $Tf
498100070420     c                   movea     Tf06          $Tf
498200070420     c                   Exsr      Rie_$Tf
498300070420     c                   ENDIF
498400021217      *
498500021217      * F12 --> non condizionato
498600021217     c                   IF        *in89 = *off
498700021217     c                   reset                   $Tf
498800021217     c                   movea     Tf12          $Tf
498900021217     c                   Exsr      Rie_$Tf
499000021217     c                   ENDIF
499100021216      *
499200021217      * Pulisco le stringa
499300021216     c                   z-add     1             $h
499400030121     c     1             do        62            $h                2 0
499500021216     C                   if        RigaTf1($h) = '#'
499600021216     c                   movel     *blanks       RigaTf1($h)
499700021216     c                   endif
499800021216     c                   enddo
499900021216     c                   z-add     1             $h
500000030121     c     1             do        62            $h                2 0
500100021216     C                   if        RigaTf2($h) = '#'
500200021216     c                   movel     *blanks       RigaTf2($h)
500300021216     c                   endif
500400021216     c                   enddo
500500021216     c                   z-add     1             $h
500600030121     c     1             do        62            $h                2 0
500700021216     C                   if        RigaTf3($h) = '#'
500800021216     c                   movel     *blanks       RigaTf3($h)
500900021216     c                   endif
501000021216     c                   enddo
501100021217      *
501200021216      * Imposto la descrizione del tasto funzione F24 e segnalo
501300021216      * quante righe ho riempito e quale devo emettere
501400021216     c                   SELECT
501500021216      *
501600021216     c                   when      $k<>*zeros and $Y<>*zeros
501700021216     c                   eval      $loop = 3
501800021216     c                   z-add     3             $loop             1 0
501900021217     C                   movel(p)  CF2413        F24Dxx           16
502000021216      *
502100021216     c                   when      $k<>*zeros and $Y=*zeros
502200021216     c                   z-add     2             $loop             1 0
502300021217     C                   movel(p)  CF2412        F24Dxx
502400021216      *
502500021216     c                   Other
502600021216     c                   z-add     1             $loop             1 0
502700021217     C                   movel     *blanks       F24Dxx
502800021216     c                   ENDSL
502900021216      * Prima riga di tasti funzione
503000030121     c                   movea     RigaTF1       FDxx             62
503100021216     c                   z-add     1             $Rig              1 0
503200021216      *
503300021216     c                   endsr
503400021217      *-------------------------------------------------------------------------
503500021217      *  Riempio le stringhe con i campi funzione compattati
503600021217      *-------------------------------------------------------------------------
503700021217     c     Rie_$Tf       begsr
503800021217      *
503900021217     c                   z-add     1             $x                2 0
504000021217     c     '#'           lookup    $Tf($x)                                31
504100021217     c                   add       $x            $Z
504200030121     c                   IF        $Z <= 63
504300021217     c                   eval      $j = $Z - $x + 1
504400021217     c                   movea     $Tf           RigaTF1($j)
504500021217     c                   ELSE
504600021217     c                   add       $x            $K
504700030121     c                   IF        $K <= 63
504800021217     c                   eval      $j = $K - $x + 1
504900021217     c                   movea     $Tf           RigaTF2($j)
505000021217     c                   ELSE
505100021217     c                   add       $x            $Y
505200030121     c                   IF        $Y <= 63
505300021217     c                   eval      $j = $Y - $x + 1
505400021217     c                   movea     $Tf           RigaTF3($j)
505500021217     c                   ENDIF
505600021217     c                   ENDIF
505700021217     c                   ENDIF
505800021217      *
505900021217     c                   endsr
506000021217      *-------------------------------------------------------------------------
506100021217      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
506200021217      *-------------------------------------------------------------------------
506300021217     c     Ges_F24       begsr
506400021217      *
506500021217     c                   SELECT
506600021217      *
506700021217     c                   WHEN      $Loop = 2 and $Rig =2  or
506800021217     c                             $Loop = 3 and $Rig =3
506900021217     c                   movea     RigaTF1       FDxx
507000021217     c                   z-add     1             $Rig
507100021217      *
507200021217     c                   if        $loop = 2
507300021217     c                   eval      F24Dxx= Cf2412
507400021217     c                   else
507500021217     c                   eval      F24Dxx= Cf2413
507600021217     c                   endif
507700021217      *
507800021217     c                   WHEN      $Loop = 2 and $Rig =1 or
507900021217     c                             $Loop = 3 and $Rig =1
508000021217     c                   movea     RigaTF2       FDxx
508100021217     c                   z-add     2             $Rig
508200021217      *
508300021217     c                   if        $loop = 2
508400021217     c                   eval      F24Dxx= Cf2422
508500021217     c                   else
508600021217     c                   eval      F24Dxx= Cf2423
508700021217     c                   endif
508800021217      *
508900021217     c                   WHEN      $Loop = 3 and $Rig =2
509000021217     c                   movea     RigaTF3       FDxx
509100021217     c                   z-add     3             $Rig
509200021217     c                   eval      F24Dxx= Cf2433
509300021217     c                   ENDSL
509400021217      *
509500021217     c                   endsr
509600021217      *-------------------------------------------------------------------------
509700991027      *  CARICO SUBFILE VARIE
509800991027      *-------------------------------------------------------------------------
509900991027     c     CAR_VARIE     BEGSR
510000991128      *
510100991027      *
510200991027     c                   SELECT
510300991027      *
510400991027     c                   WHEN       WSVN = *BLANKS
510500991027      *
510600991027     C                   WHEN       WSVN = §$2iva
510700991027     c                   z-add     Wvan          W0133
510800991027     c                   eval      Wtstdec = §cvfdc
510900991027     c                   exsr      EDTimporti
511000991027     c                   movel     IMPeditato    VIDiva
511100991027      *
511200991027     c                   OTHER
511300991027     c                   z-add     Wvan          W0133
511400991027     c                   eval      Wtstdec = §cvfdc
511500991027     c                   exsr      EDTimporti
511600991027     c                   movel     IMPeditato    V6Cva1
511700991027     c                   movel     WSVN          V6Csv1
511800991027     C                   Z-ADD     1             WA
511900991027     C     V6CSV1        LOOKUP    CC1(WA)                                99
512000020329     C     CC3(WA)       ifeq      'S'
512100020329      *
512200020329      * Verifico se si tratta di una bolla Fedex
512300021119     c                   if        LnaNtw = 'FED'
512400020329     C                   Z-ADD     1             WA
512500020329     C     V6CSV1        LOOKUP    CB1(WA)                                99
512600020329     C   99              MOVEL     CB2(WA)       V6CDE1
512700020329     C  N99              clear                   V6CDE1
512800020329     C                   else
512900991027     C   99              MOVEL     CC2(WA)       V6CDE1
513000991027     C  N99              MOVEL     *BLANKS       V6CDE1
513100020329     C                   endif
513200020329     C                   else
513300020329     C   99              MOVEL     CC2(WA)       V6CDE1
513400020329     C  N99              MOVEL     *BLANKS       V6CDE1
513500020329     C                   endif
513600991027     C                   add       1             NR6
513700131107     C                   WRITE     LRI6S06
513800991027      *
513900991027     c                   ENDSL
514000991027      *
514100991027     c                   ENDSR
514200040122      *-------------------------------------------------------------------------
514300040122      *  Carico le varie della seconda bolla
514400040122      *-------------------------------------------------------------------------
514500040122     c     Car_Varie1    BegSr
514600040122
514700040122s   1c                   Select
514800040122
514900040122     c                   When      wsvn = *Blanks
515000040122      * Iva
515100040122     c                   When      wsvn = §$2iva
515200040122     c                   z-add     wvan          w0133
515300040122     c                   Eval      Wtstdec = §cvfdc
515400040122     c                   ExSr      EDTimporti
515500040122     c                   Movel     IMPeditato    V9cIva
515600070411     c                   setoff                                       06
515700070411      * Bollo
515800070411     c                   When      wsvn = §$2bol
515900070411     c                   z-add     wvan          w0133
516000070411     c                   Eval      Wtstdec = §cvfdc
516100070411     c                   ExSr      EDTimporti
516200070411     c                   Movel     IMPeditato    V9cIva
516300070411     c                   seton                                        06
516400040122      * Varia G - sempre per prima
516500040122     c                   When      wsvn = §$2vrg
516600040122     c                   Eval      V9cSv1 = wsvn
516700040122     c                   z-add     wvan          w0133
516800040122     c                   Eval      Wtstdec = §cvfdc
516900040122     c                   ExSr      EDTimporti
517000040122     c                   Movel     IMPeditato    V9cVa1
517100040122      * Altre varie
517200040122   x1c                   Other
517300040122     c                   z-add     wvan          W0133
517400040122     c                   eval      Wtstdec = §cvfdc
517500040122     c                   exsr      EDTimporti
517600040122if  2c                   If        V9cSv2 = *Blanks
517700040122     c                   movel     IMPeditato    V9cVa2
517800040122     c                   movel     wsvn          V9csv2
517900040122   x2c                   Else
518000040122if  3c                   If        V9cSv3 = *Blanks
518100040122     c                   movel     IMPeditato    V9cVa3
518200040122     c                   movel     wsvn          V9csv3
518300040122   x3c                   Else
518400040122if  4c                   If        V9cSv4 = *Blanks
518500040122     c                   movel     IMPeditato    V9cVa4
518600040122     c                   movel     wsvn          V9csv4
518700041130   x4c                   Else
518800041130if  5c                   If        V9cSv5 = *Blanks
518900041130     c                   movel     IMPeditato    V9cVa5
519000041130     c                   movel     wsvn          V9csv5
519100041130   x5c                   Else
519200041130if  6c                   If        V9cSv6 = *Blanks
519300041130     c                   movel     IMPeditato    V9cVa6
519400041130     c                   movel     wsvn          V9csv6
519500041130    6c                   EndIf
519600041130    5c                   EndIf
519700040122    4c                   EndIf
519800040122    3c                   EndIf
519900040122    2c                   EndIf
520000040122
520100040122    1c                   EndSl
520200040122
520300040122     c                   EndSr
520400991027      *****************************************************************
520500991027      *  AGGANCIO ANAGRAFICHE
520600991027      *****************************************************************
520700991027     C     CTR_ANAGRA    BEGSR
520800991027      *
520900991027     C                   clear                   DS_cnaco
521000991027     C                   clear                   DS_cnind
521100991027     C                   clear                   DS_cnclp
521200991027     C                   clear                   DS_fncls
521300991027      *
521400991027     C                   CALL      'TIBS69R'
521500991027     C                   PARM                    tibs69DS
521600991027     C                   PARM                    DS_cnaco
521700991027     C                   PARM                    DS_cnind
521800991027     C                   PARM                    DS_cnclp
521900991027     C                   PARM                    DS_fncls
522000991027      *
522100991027     C                   eval      wtibs69r = 'S'
522200991027      * per il 1° giro è inizializzata nelle specifiche "D"
522300991027     C                   clear                   TIBS69DS
522400991027      *
522500991027     C                   ENDSR
522600991027      **********************************************************************
522700991027      * AGGANCIO TABELLA VALUTE
522800991027      **********************************************************************
522900991027     C     CTR_VALUTA    BEGSR
523000991027
523100991027     C                   MOVEL     'CV'          COD
523200991027     C                   MOVEL(P)  WDIV          KEY
523300991027     C     KTAB1         CHAIN     TABEL00F                           31
523400991027     C  N31              MOVEL     TBLUNI        DSCV
523500991027
523600991027     C                   ENDSR
523700991027      **********************************************************************
523800991027      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
523900991027      **********************************************************************
524000991027     C     EDTIMPORTI    BEGSR
524100991027
524200991027      * ------------------------------------------------------------------------
524300991027      * W0133      contiene il numero da editare per il video
524400991027      * WTSTDEC    corrisponde a §CVfdc della tabella CV
524500991027      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
524600991027      * ------------------------------------------------------------------------
524700991027      *
524800991027     c                   clear                   IMPeditato
524900991027     c                   clear                   W017A
525000991027      *
525100991027     c                   if        Wtstdec = 'S'
525200991027     C                   eval      W017a = %EDITC(W0133:'2')
525300991027     c                   else
525400991027     c                   z-add     W0133         W0130
525500991027     C                   eval      W017a = %EDITC(W0130:'2')
525600991027     c                   endif
525700991027      *
525800991027     c                   move      W017a         IMPeditato
525900991027      *
526000991027     c                   ENDSR
526100940802     C**************************************************************************
526200940802     C*    LEGAMI BOLLA
526300940802     C**************************************************************************
526400930413     C     LEGBOL        BEGSR
526500940802     C*
526600940912     C* DEVO MEMORIZZARE LA BOLLA POICHE DEVO RIAGGANCIARLA DOPO
526700940912     C                   MOVEL     ARBAAS        WAAS
526800940912     C                   MOVEL     ARBLNP        WLNP
526900940912     C                   MOVEL     ARBNRS        WNRS
527000940912     C                   MOVEL     ARBNSP        WNSP
527100940912     C*
527200940907     C* PULISCO IL SFL
527300950825     C                   SETOFF                                       787923
527400131107     C                   WRITE     LRI6C08
527500950825     C                   SETON                                        7923
527600940923     C                   MOVEL     *BLANKS       LEGAME
527700940907     C*
527800940907     C                   SETOFF                                       94
527900950718     C                   Z-ADD     *ZERO         NR8
528000940907     C                   Z-ADD     *ZERO         SAVNR8            4 0
528100940907     C                   MOVEL     *BLANKS       WFLG1             1
528200940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
528300940907     C*
528400940907     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
528500940907     C     KARB          CHAIN     FNLBL01L                           99
528600940907     C   99KARB          CHAIN     FNLBL02L                           99
528700940907     C  N99              DO
528800940907     C*
528900940907     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
529000940907     C                   MOVEL     LBLAAO        KAAS
529100940907     C                   MOVEL     LBLLPO        KLNP
529200940907     C                   MOVEL     LBLNRO        KNRS
529300940907     C                   MOVEL     LBLNSO        KNSP
529400940907     C     KLBL3         CHAIN     FNLBL02L                           99
529500950103     C     *IN99         IFEQ      *ON
529600950103     C                   MOVEL     'N'           WORIGI            1
529700950103     C                   ELSE
529800950103     C                   MOVEL     'S'           WORIGI
529900950103     C                   ENDIF
530000940907     C*
530100940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
530200940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
530300950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
530400950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
530500940907     C                   MOVEL     ' '           V8CSCE
530600940907     C                   MOVEL     LBLAAO        V8CAAP
530700940907     C                   MOVEL     LBLLPO        V8CLPP
530800940907     C                   MOVEL     LBLLAO        V8CLAP
530900940907     C                   MOVEL     LBLNRO        V8CNRP
531000940907     C                   MOVEL     LBLNSO        V8CNSP
531100940926     C                   CLEAR                   V8CAAN
531200940926     C                   CLEAR                   V8CLPN
531300940926     C                   CLEAR                   V8CLAN
531400940926     C                   CLEAR                   V8CNRN
531500940926     C                   CLEAR                   V8CNSN
531600940907     C*
531700940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
531800940907     C     LBLAAO        IFEQ      V3CAAS
531900940907     C     LBLLPO        ANDEQ     V3CLNP
532000940907     C     LBLNRO        ANDEQ     V3CNRS
532100940907     C     LBLNSO        ANDEQ     V3CNSP
532200940907     C                   MOVEL     *BLANKS       V8CPRE
532300940907     C                   SETON                                        80
532400940907     C                   SETON                                        81
532500940907     C                   MOVEL     '1'           WFLG1
532600940907     C                   ELSE
532700940907     C                   SETOFF                                       81
532800940907     C*
532900950316     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
533000950316     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
533100950316     C                   SETOFF                                       93
533200950316     C     KLBL3         SETLL     FNARB01L                               93
533300950316     C  N93              MOVEA     PRD(1)        V8CPRE
533400950316     C  N93              SETON                                        80
533500950316     C   93              MOVEA     PRD(2)        V8CPRE
533600950316     C   93              SETOFF                                       80
533700950316     C*
533800950316     C* SE NON TROVATA SU ARB PROVO SU BLP
533900950316     C  N93              DO
534000950316     C                   SETOFF                                       93
534100950316     C     KLBL3         SETLL     FNBLP01L                               93
534200010712      *
534300010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
534400010712     c                   if        *in93 = *off
534500010712     C                   MOVEA     PRD(1)        V8CPRE
534600010712     c                   else
534700950316     C* SE RICHIAMATO DALLE PARTENZE (FNLS05R) NON POSSO
534800950316     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
534900010712     C     PA1FLG        IFEQ      '3'
535000960903     C     PA1FLG        OREQ      'N'
535100950316     C                   MOVEA     PRD(3)        V8CPRE
535200950316     C                   SETON                                        80
535300950316     C                   ELSE
535400950316     C                   MOVEA     PRD(2)        V8CPRE
535500950316     C                   SETOFF                                       80
535600950316     C                   ENDIF
535700010712     C                   ENDIF
535800950316     C*
535900950316     C                   ENDDO
536000940907     C*
536100940907     C                   ENDIF
536200940907     C                   SETON                                        83
536300950103     C*
536400950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
536500950103     C                   MOVEL     *IN80         V8CI80
536600950103     C                   MOVEL     *IN81         V8CI81
536700950103     C                   MOVEL     *IN82         V8CI82
536800950103     C                   MOVEL     *IN83         V8CI83
536900950103     C*
537000940907     C                   Z-ADD     1             NR8
537100131107     C                   WRITE     LRI6S08
537200950825     C                   SETON                                        78
537300950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
537400940907     C*
537500950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
537600950103     C*                      2) NON HO TROVATO L'ORIGINALE
537700950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
537800950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
537900950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
538000950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
538100950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
538200950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
538300950103     C* L'ORIGINALE E LA MAMMA.
538400950103     C     WORIGI        IFEQ      'N'
538500950103     C                   MOVEL     *BLANKS       V8CSCE
538600950103     C                   CLEAR                   V8CAAP
538700950103     C                   CLEAR                   V8CLPP
538800950103     C                   CLEAR                   V8CLAP
538900950103     C                   CLEAR                   V8CNRP
539000950103     C                   CLEAR                   V8CNSP
539100950103     C                   MOVEL     LBLAAP        V8CAAN
539200950103     C                   MOVEL     LBLLPP        V8CLPN
539300950103     C                   MOVEL     LBLLAP        V8CLAN
539400950103     C                   MOVEL     LBLNRP        V8CNRN
539500950103     C                   MOVEL     LBLNSP        V8CNSN
539600950103     C                   SETOFF                                       8182
539700950103     C*
539800950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
539900950103     C     V8CAAN        IFEQ      V3CAAS
540000950103     C     V8CLPN        ANDEQ     V3CLNP
540100950103     C     V8CNRN        ANDEQ     V3CNRS
540200950103     C     V8CNSN        ANDEQ     V3CNSP
540300950103     C                   MOVEL     *BLANKS       V8CPRE
540400950103     C                   SETON                                        80
540500950103     C                   SETON                                        82
540600950103     C                   ELSE
540700950103     C*
540800950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
540900950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
541000950316     C                   EXSR      CHKBOL
541100950103     C*
541200950103     C                   ENDIF
541300950103     C*
541400950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
541500950103     C                   SETOFF                                       83
541600950103     C*
541700950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
541800950103     C                   MOVEL     *IN80         V8CI80
541900950103     C                   MOVEL     *IN81         V8CI81
542000950103     C                   MOVEL     *IN82         V8CI82
542100950103     C                   MOVEL     *IN83         V8CI83
542200950103     C*
542300950103     C                   ADD       1             NR8
542400131107     C                   WRITE     LRI6S08
542500950103     C*
542600950103     C                   ENDIF
542700940907     C                   SETOFF                                       83  99
542800940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
542900940907     C* CICLO FINO ALL'ULTIMO LEGAME
543000940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
543100940907     C                   Z-ADD     *ZERO         WA                3 0
543200940907     C                   Z-ADD     *ZERO         WB                3 0
543300940907     C                   MOVEA     *ZERO         AAN
543400940907     C     *IN99         DOWEQ     *OFF
543500940907     C*
543600940907     C                   MOVEL     ' '           V8CSCE
543700940907     C                   MOVEL     LBLAAP        V8CAAP
543800940907     C                   MOVEL     LBLLPP        V8CLPP
543900940907     C                   MOVEL     LBLLAP        V8CLAP
544000940907     C                   MOVEL     LBLNRP        V8CNRP
544100940907     C                   MOVEL     LBLNSP        V8CNSP
544200940907     C                   MOVEL     LBLAAN        V8CAAN
544300940907     C                   MOVEL     LBLLPN        V8CLPN
544400940907     C                   MOVEL     LBLLAN        V8CLAN
544500940907     C                   MOVEL     LBLNRN        V8CNRN
544600940907     C                   MOVEL     LBLNSN        V8CNSN
544700940907     C                   SETOFF                                       8182
544800940907     C*
544900950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
545000940907     C     LBLAAP        IFEQ      V3CAAS
545100940907     C     LBLLPP        ANDEQ     V3CLNP
545200940907     C     LBLNRP        ANDEQ     V3CNRS
545300940907     C     LBLNSP        ANDEQ     V3CNSP
545400940907     C                   SETON                                        81
545500940907     C                   ENDIF
545600940907     C*
545700940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
545800940907     C     LBLAAN        IFEQ      V3CAAS
545900940907     C     LBLLPN        ANDEQ     V3CLNP
546000940907     C     LBLNRN        ANDEQ     V3CNRS
546100940907     C     LBLNSN        ANDEQ     V3CNSP
546200940907     C                   MOVEL     *BLANKS       V8CPRE
546300940907     C                   SETON                                        80
546400940907     C                   SETON                                        82
546500940907     C                   ELSE
546600940907     C*
546700940907     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
546800940907     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
546900950316     C                   EXSR      CHKBOL
547000940907     C*
547100940907     C                   ENDIF
547200950103     C*
547300950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
547400950103     C                   MOVEL     *IN80         V8CI80
547500950103     C                   MOVEL     *IN81         V8CI81
547600950103     C                   MOVEL     *IN82         V8CI82
547700950103     C                   MOVEL     *IN83         V8CI83
547800940907     C*
547900940907     C* SCRIVO IL RECORD NEL SFL
548000940907     C                   ADD       1             NR8
548100131107     C                   WRITE     LRI6S08
548200940907     C*
548300940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
548400940907     C                   ADD       1             WA
548500940907     C                   MOVEL     LBLAAN        AAN(WA)
548600940907     C                   MOVEL     LBLLPN        LPN(WA)
548700940907     C                   MOVEL     LBLNRN        NRN(WA)
548800940907     C                   MOVEL     LBLNSN        NSN(WA)
548900940907     C*
549000940907     C     KLBL3         READE     FNLBL02L                               99
549100940907     C*
549200940907     C* SE NON HO PIU' BOLLE CON READE
549300940907     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
549400970116     C     WB            DOWLT     WA
549500970116     C     *IN99         ANDEQ     *ON
549600940907     C                   ADD       1             WB
549700940907     C                   MOVEL     AAN(WB)       KAAS
549800940907     C                   MOVEL     LPN(WB)       KLNP
549900940907     C                   MOVEL     NRN(WB)       KNRS
550000940907     C                   MOVEL     NSN(WB)       KNSP
550100940907     C     KLBL3         SETLL     FNLBL02L
550200940907     C     KLBL3         READE     FNLBL02L                               99
550300970116     C                   ENDDO
550400940907     C*
550500940907     C                   ENDDO
550600940907     C                   ENDDO
550700940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
550800940907     C*
550900940907     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
551000940907     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
551100940907     C     NR8           IFEQ      2
551200940907     C*
551300940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
551400131107     C     2             CHAIN     LRI6S08                            99
551500940907     C     V8CAAP        IFEQ      V3CAAS
551600940907     C     V8CLPP        ANDEQ     V3CLNP
551700940907     C     V8CNRP        ANDEQ     V3CNRS
551800940907     C     V8CNSP        ANDEQ     V3CNSP
551900940907     C                   MOVEL     V8CAAN        COMAAN
552000940907     C                   MOVEL     V8CLPN        COMLPN
552100940907     C                   MOVEL     V8CNRN        COMNRN
552200940907     C                   MOVEL     V8CNSN        COMNSN
552300950307     C                   MOVEL     V8CLAN        COMLAN
552400950316     C                   MOVEA     'SEGUEN.'     LBL(14)
552500940907     C                   ELSE
552600940907     C                   MOVEL     V8CAAP        COMAAN
552700940907     C                   MOVEL     V8CLPP        COMLPN
552800940907     C                   MOVEL     V8CNRP        COMNRN
552900940907     C                   MOVEL     V8CNSP        COMNSN
553000950307     C                   MOVEL     V8CLAP        COMLAN
553100950316     C                   MOVEA     'PRECED.'     LBL(14)
553200940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
553300940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
553400940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
553500131107     C     1             CHAIN     LRI6S08                            99
553600940926     C* REIMPOSTO NR8=2 PERCHE' MI SERVE DOPO
553700940926     C                   Z-ADD     2             NR8
553800940907     C                   ENDIF
553900940907     C*
554000950316     C                   MOVEA     COMLPN        LBL(21)
554100950316     C                   MOVEA     COMNRN        LBL(25)
554200950316     C                   MOVEA     COMNSN        LBL(28)
554300950316     C                   MOVEA     COMLAN        LBL(40)
554400940907     C*
554500940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
554600940907     C                   Z-ADD     1             W                 1 0
554700940907     C     V8CPRE        LOOKUP    PRD(W)                                 99
554800950307     C                   MOVEA     LBD(W)        LBL(44)
554900940907     C     W             COMP      2                                      94
555000940907     C*
555100940907     C                   MOVEA     LBL           LEGAME
555200940907     C                   ELSE
555300940907     C*
555400940907     C     NR8           IFGT      2
555500940907     C                   MOVEA     LB2           LEGAME
555600940907     C                   SETON                                        94
555700940907     C                   ENDIF
555800940907     C                   ENDIF
555900940907     C*
556000940907     C     NR8           COMP      1                                    3434
556100940907     C*
556200940907     C                   Z-ADD     NR8           SAVNR8
556300940907     C                   Z-ADD     1             NR8
556400940923     C     LEGAME        COMP      *BLANKS                            9292
556500940912     C*
556600940912     C* RIAGGANCIO LA BOLLA IN ESAME
556700940912     C                   MOVEL     WAAS          ARBAAS
556800940912     C                   MOVEL     WLNP          ARBLNP
556900940912     C                   MOVEL     WNRS          ARBNRS
557000940912     C                   MOVEL     WNSP          ARBNSP
557100940912     C     KARB2         CHAIN     FNARB01L                           99
557200930511     C*
557300930413     C                   ENDSR
557400950316     C**************************************************************************
557500950316     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
557600950316     C**************************************************************************
557700950316     C     CHKBOL        BEGSR
557800950316     C*
557900950316     C                   SETOFF                                       93
558000950316     C     KLBL1         SETLL     FNARB01L                               93
558100950316     C  N93              MOVEA     PRD(1)        V8CPRE
558200950316     C  N93              SETON                                        80
558300950316     C   93              MOVEA     PRD(2)        V8CPRE
558400950316     C   93              SETOFF                                       80
558500950316     C*
558600950316     C* SE NON TROVATA SU ARB PROVO SU BLP
558700950316     C  N93              DO
558800950316     C                   SETOFF                                       93
558900950316     C     KLBL1         SETLL     FNBLP01L                               93
559000010712      *
559100010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
559200010712     c                   if        *in93 = *off
559300010712     C                   MOVEA     PRD(1)        V8CPRE
559400010712     c                   else
559500950316     C* SE RICHIAMATO DALLE PARTENZE (FNLS05R) NON POSSO
559600950316     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
559700010712     C     PA1FLG        IFEQ      '3'
559800960903     C     PA1FLG        OREQ      'N'
559900950316     C                   MOVEA     PRD(3)        V8CPRE
560000950316     C                   SETON                                        80
560100950316     C                   ELSE
560200950316     C                   MOVEA     PRD(2)        V8CPRE
560300950316     C                   SETOFF                                       80
560400950316     C                   ENDIF
560500010712     C                   ENDIF
560600950316     C*
560700950316     C                   ENDDO
560800950316     C*
560900950316     C                   ENDSR
561000940802     C**************************************************************************
561100940802     C*    VISUALIZZA BOLLA
561200940802     C**************************************************************************
561300911023     C     VISUAL        BEGSR
561400940321      *
561500940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
561600131108     c     Emriepil      tag
561700940712     C                   EXSR      RIEPIL
561800940712      *
561900131108      * PER F15 PASSO ALLA BOLLA, PER INVIO o F12 RITORNO AL SFL
562000940712     C  NKP              GOTO      ENDVIS
562100931112      *
562200931112     C                   SETOFF                                       89
562300940802     C                   MOVEL     'S'           WRIEM             1
562400021217      *
562500021217      * Riempio il campone in fondo alla videata con i tasti scelti
562600021217     C                   exsr      Ges_TastiFun
562700021217     c                   eval      FD03 = FDxx
562800021217     c                   eval      F24D03 = F24Dxx
562900911023     C     FOR03         TAG
563000940802     C     WRIEM         IFEQ      'S'
563100131107     C                   WRITE     LRI6T01
563200940802     C                   MOVEL     *BLANKS       WRIEM
563300940802     C                   ENDIF
563400021217      *
563500021217      * Imposto la stringa comandi del 'giro' successivo
563600021217     C                   IF        *inKY = *on and $Loop > 1
563700021217     c                   Exsr      Ges_F24
563800021217     c                   eval      FD03 = FDxx
563900021217     c                   eval      F24D03 = F24Dxx
564000021217     C                   ENDIF
564100021217      *
564200131107     C                   EXFMT     LRI6D03
564300070420     c                   setoff                                       28
564400070420     c                   clear                   v1cmsg
564500931112      *
564600131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
564700931112     C   KY*IN89         IFEQ      *ON
564800931112     C                   SETOFF                                       89
564900931112     C                   ELSE
565000931112     C                   SETON                                        89
565100931112     C                   ENDIF
565200931112     C   KY              GOTO      FOR03
565300911023     C*
565400131108     C* F3 - FINE LAVORO TORNO A SUBFILE
565500940714     C   KC              MOVEL     '2'           V2CFLG            1
565600911023     C   KC              GOTO      ENDVIS
565700911023     C*
565800131108     C* F12 - FINE LAVORO tolno a videata di riepilogo
565900131108     C   KL              GOTO      emriepil
566000911023     C*
566100911024     C*
566200930517     C* CMD18 - INTERROGA LEGAME BOLLA
566300930518     C   KS              DO
566400930518     C                   EXSR      CMD18
566500940907     C                   MOVEL     'S'           WRIEM
566600930518     C                   GOTO      FOR03
566700930518     C                   END
566800930517     C*
566900940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
567000920804     C                   EXSR      COMAND
567100940905     C     WCMD          IFEQ      'S'
567200940802     C     WF03          IFEQ      *BLANKS
567300070403     c
567400920804     C                   GOTO      FOR03
567500940802     C                   ELSE
567600940802     C                   MOVEL     '2'           V2CFLG
567700940802     C                   GOTO      ENDVIS
567800940802     C                   ENDIF
567900940905     C                   ENDIF
568000911023     C*
568100911023     C* FORMATO4
568200931112     C                   SETOFF                                       89
568300940802     C                   MOVEL     'S'           WRIEM
568400021217      *
568500021217      * Riempio il campone in fondo alla videata con i tasti scelti
568600021217     C                   exsr      Ges_TastiFun
568700021217     c                   eval      FD04 = FDxx
568800021217     c                   eval      F24D04 = F24Dxx
568900911023     C     FOR04         TAG
569000940802     C     WRIEM         IFEQ      'S'
569100131107     C                   WRITE     LRI6T01
569200940802     C                   MOVEL     *BLANKS       WRIEM
569300940802     C                   ENDIF
569400021217      * Imposto la stringa comandi del 'giro' successivo
569500021217     C                   IF        *inKY = *on and $Loop > 1
569600021217     c                   Exsr      Ges_F24
569700021217     c                   eval      FD04 = FDxx
569800021217     c                   eval      F24D04 = F24Dxx
569900021217     C                   ENDIF
570000131107     C                   EXFMT     LRI6D04
570100070420     c                   setoff                                       28
570200070420     c                   clear                   v1cmsg
570300931112      *
570400131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
570500931112     C   KY*IN89         IFEQ      *ON
570600931112     C                   SETOFF                                       89
570700931112     C                   ELSE
570800931112     C                   SETON                                        89
570900931112     C                   ENDIF
571000931112     C   KY              GOTO      FOR04
571100911023     C*
571200131108     C* F12 - RITORNO VIDEATA 3
571300911023     C   KL              GOTO      FOR03
571400920804     C*
571500131108     C* F18 - INTERROGA LEGAME BOLLA
571600930518     C   KS              DO
571700930518     C                   EXSR      CMD18
571800930518     C                   GOTO      FOR04
571900930518     C                   END
572000940923     C*
572100940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
572200131120     c                   eval      wvideo='4'
572300131120     C                   EXSR      CTR_puIn
572400940923     C     WRIEM         CABEQ     'S'           FOR04
572500930517     C*
572600940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
572700920804     C                   EXSR      COMAND
572800940905     C     WCMD          IFEQ      'S'
572900940802     C     WF03          IFEQ      *BLANKS
573000940802     C                   GOTO      FOR04
573100940802     C                   ELSE
573200940802     C                   MOVEL     '2'           V2CFLG
573300940802     C                   GOTO      ENDVIS
573400940802     C                   ENDIF
573500940905     C                   ENDIF
573600911023     C*
573700911023     C* VIDEATA 5
573800931112     C                   SETOFF                                       89
573900940809     C                   MOVEL     'S'           WRIEM
574000021217      *
574100021217      * Riempio il campone in fondo alla videata con i tasti scelti
574200021217     C                   exsr      Ges_TastiFun
574300021217     c                   eval      FD05 = FDxx
574400021217     c                   eval      F24D05 = F24Dxx
574500911023     C     FOR05         TAG
574600940809     C     WRIEM         IFEQ      'S'
574700131107     C                   WRITE     LRI6T01
574800940809     C                   MOVEL     *BLANKS       WRIEM
574900940809     C                   ENDIF
575000951220     C                   Z-ADD     1             REC5
575100021217      *
575200021217      * Imposto la stringa comandi del 'giro' successivo
575300021217     C                   IF        *inKY = *on and $Loop > 1
575400021217     c                   Exsr      Ges_F24
575500021217     c                   eval      FD05 = FDxx
575600021217     c                   eval      F24D05 = F24Dxx
575700021217     C                   ENDIF
575800131107     C                   WRITE     LRI6Z05
575900131107     C                   EXFMT     LRI6C05
576000070420     c                   setoff                                       28
576100070420     c                   clear                   v1cmsg
576200931112      *
576300131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
576400931112     C   KY*IN89         IFEQ      *ON
576500931112     C                   SETOFF                                       89
576600931112     C                   ELSE
576700931112     C                   SETON                                        89
576800931112     C                   ENDIF
576900931112     C   KY              GOTO      FOR05
577000911023     C*
577100131108     C* F12 - RITORNO VIDEATA 2
577200911023     C   KL              GOTO      FOR04
577300920804     C*
577400940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
577500920804     C                   EXSR      COMAND
577600940905     C     WCMD          IFEQ      'S'
577700940802     C     WF03          IFEQ      *BLANKS
577800940802     C                   GOTO      FOR05
577900940802     C                   ELSE
578000131108     C                   MOVEL     '2'           V2CFLG
578100131108     C                   GOTO      ENDVIS
578200940802     C                   ENDIF
578300070406
578400070406     c                   else
578500070406     c
578600070406     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
578700070406     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
578800070406     c                   if        v3cgesd='?'
578900070406     c                   exsr      VISUALAbi
579000070406     c                   clear                   v3cgesd
579100070406     c                   eval      wriem='S'
579200070406     C                   GOTO      FOR05
579300070406     c                   endif
579400940905     C                   ENDIF
579500131108     c
579600131108     C                   GOTO      emriepil
579700931112      *
579800911023     C     ENDVIS        ENDSR
579900070403     C**************************************************************************
580000070403     C*    VISUALIZZA Abilitazioni
580100070403     C**************************************************************************
580200070403     C     VISUALAbi     BEGSR
580300070403     c*
580400131120     c                   clear                   fnlrg7ds
580500131120     c                   move      v3cdsp        ilrg7aas
580600131120     c                   eval      ilrg7lnp=v3clnp
580700131120     c                   eval      ilrg7nrs=v3cnrs
580800131120     c                   eval      ilrg7nsp=v3cnsp
580900131120     c                   eval      ilrg7vsdet='?'
581000131120     c                   eval      ilrg7lung=58
581100131120     c                   call      'FNLRG7R'
581200131120     c                   parm                    fnlrg7ds
581300070403     c
581400070403     C                   ENDSR
581500940712     C**************************************************************************
581600940802     C*    VISUALIZZA BOLLA PRECEDENTE
581700940802     C**************************************************************************
581800930510     C     VISUA2        BEGSR
581900940321      *
582000940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
582100131108     c     Emriepi2      tag
582200940712     C                   EXSR      RIEPI2
582300940712      *
582400131108      * PER F15 PASSO ALLA BOLLA, PER INVIO RITORNO AL SFL
582500940713     C  NKP              GOTO      ENDV8C
582600931112      *
582700931112     C                   SETOFF                                       89
582800940809     C                   MOVEL     'S'           WRIEM
582900021217      *
583000021217      * Riempio il campone in fondo alla videata con i tasti scelti
583100021217     C                   exsr      Ges_TastiFun
583200070516     c                   eval      FD03 = FDxx
583300070516     c                   eval      F24D03 = F24Dxx
583400930510     C     FOR033        TAG
583500940809     C     WRIEM         IFEQ      'S'
583600131107     C                   WRITE     LRI6T01
583700940809     C                   MOVEL     *BLANKS       WRIEM
583800940809     C                   ENDIF
583900021217      *
584000021217      * Imposto la stringa comandi del 'giro' successivo
584100021217     C                   IF        *inKY = *on and $Loop > 1
584200021217     c                   Exsr      Ges_F24
584300021217     c                   eval      FD03 = FDxx
584400021217     c                   eval      F24D03 = F24Dxx
584500021217     C                   ENDIF
584600131107     C                   EXFMT     LRI6D03
584700070420     c                   setoff                                       28
584800070420     c                   clear                   v1cmsg
584900931112      *
585000131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
585100931112     C   KY*IN89         IFEQ      *ON
585200931112     C                   SETOFF                                       89
585300931112     C                   ELSE
585400931112     C                   SETON                                        89
585500931112     C                   ENDIF
585600931112     C   KY              GOTO      FOR033
585700930510     C*
585800131108     C* F12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
585900930510     C   KC
586000131108     COR KL              GOTO      Emriepi2
586100930510     C*
586200940905     C* CONTROLLO SE PREMUTI TASTI FUNZIONALI E, NEL CASO, RIEMETTO
586300930510     C                   EXSR      COMAND
586400940905     C     WCMD          IFEQ      'S'
586500930510     C                   GOTO      FOR033
586600930510     C                   END
586700931112      *
586800930510     C* FORMATO4
586900931112     C                   SETOFF                                       89
587000940809     C                   MOVEL     'S'           WRIEM
587100021217      *
587200021217      * Riempio il campone in fondo alla videata con i tasti scelti
587300021217     C                   exsr      Ges_TastiFun
587400021217     c                   eval      FD04 = FDxx
587500021217     c                   eval      F24D04 = F24Dxx
587600930510     C     FOR044        TAG
587700940809     C     WRIEM         IFEQ      'S'
587800131107     C                   WRITE     LRI6T01
587900940809     C                   MOVEL     *BLANKS       WRIEM
588000940809     C                   ENDIF
588100021217      *
588200021217      * Imposto la stringa comandi del 'giro' successivo
588300021217     C                   IF        *inKY = *on and $Loop > 1
588400021217     c                   Exsr      Ges_F24
588500021217     c                   eval      FD04 = FDxx
588600021217     c                   eval      F24D04 = F24Dxx
588700021217     C                   ENDIF
588800131107     C                   EXFMT     LRI6D04
588900070420     c                   setoff                                       28
589000070420     c                   clear                   v1cmsg
589100931112      *
589200931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
589300931112     C   KY*IN89         IFEQ      *ON
589400931112     C                   SETOFF                                       89
589500931112     C                   ELSE
589600931112     C                   SETON                                        89
589700931112     C                   ENDIF
589800931112     C   KY              GOTO      FOR044
589900930510     C*
590000930510     C* CMD12 - RITORNO VIDEATA 3
590100930510     C   KL              GOTO      FOR033
590200940923     C*
590300940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
590400131120     c                   eval      wvideo='4'
590500131120     C                   EXSR      CTR_puIn
590600940923     C     WRIEM         CABEQ     'S'           FOR044
590700930510     C*
590800940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
590900930510     C                   EXSR      COMAND
591000940905     C     WCMD          IFEQ      'S'
591100930510     C                   GOTO      FOR044
591200940905     C                   ENDIF
591300931112      *
591400930510     C* VIDEATA 5
591500931112     C                   SETOFF                                       89
591600940809     C                   MOVEL     'S'           WRIEM
591700021217      *
591800021217      * Riempio il campone in fondo alla videata con i tasti scelti
591900021217     C                   exsr      Ges_TastiFun
592000021217     c                   eval      FD05 = FDxx
592100021217     c                   eval      F24D05 = F24Dxx
592200930510     C     FOR055        TAG
592300940809     C     WRIEM         IFEQ      'S'
592400131107     C                   WRITE     LRI6T01
592500940809     C                   MOVEL     *BLANKS       WRIEM
592600940809     C                   ENDIF
592700951220     C                   Z-ADD     1             REC5
592800021217      *
592900021217      * Imposto la stringa comandi del 'giro' successivo
593000021217     C                   IF        *inKY = *on and $Loop > 1
593100021217     c                   Exsr      Ges_F24
593200021217     c                   eval      FD05 = FDxx
593300021217     c                   eval      F24D05 = F24Dxx
593400021217     C                   ENDIF
593500131107     C                   WRITE     LRI6Z05
593600131107     C                   EXFMT     LRI6C05
593700070420     c                   setoff                                       28
593800070420     c                   clear                   v1cmsg
593900931112      *
594000931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
594100931112     C   KY*IN89         IFEQ      *ON
594200931112     C                   SETOFF                                       89
594300931112     C                   ELSE
594400931112     C                   SETON                                        89
594500931112     C                   ENDIF
594600931112     C   KY              GOTO      FOR055
594700930510     C*
594800930510     C* CMD12 - RITORNO VIDEATA 2
594900930510     C   KL              GOTO      FOR044
595000930510     C*
595100940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
595200940905     C                   EXSR      COMAND
595300940905     C     WCMD          IFEQ      'S'
595400930510     C                   GOTO      FOR055
595500930510     C                   END
595600131108
595700131108     C                   GOTO      Emriepi2
595800931112      *
595900940713     C     ENDV8C        ENDSR
596000940802     C**************************************************************************
596100940802     C* TASTI DI FUNZIONE
596200940802     C**************************************************************************
596300920804     C     COMAND        BEGSR
596400940905     C*
596500940905     C* PULISCO IL FLAG CHE INDICA SE PREMUTO UN TASTO FUNZIONE
596600940905     C                   MOVE      *BLANKS       WCMD              1
596700040218      *
596800070411      *?F1 - Aggiunta bolla in reclamo?o scelta bolla generica
596900040218if  1c                   if        *inKA   = *on
597000070423     C                   Z-ADD     arbLNP        COMLNP
597100070411     C                   MOVEL     '3'           V2CFLG
597200070411      *?-
597300070411if  2c                   if        SDSprm  > 1
597400131111     c                   eval      wARBlnp = ARBlnp
597500131111     c                   eval      wARBnrs = ARBnrs
597600131111     c                   eval      wARBnsp = ARBnsp
597700131111     c                   eval      wARBaas = ARBaas
597800040218     c                   clear                   FIDNA1ds
597900040218     c                   eval      IA1ins  = 'S'
598000040218     c                   eval      IA1tor  = 'S'
598100040218     c                   eval      IA1ogg  = ds_OggSped
598200070411    2c                   endif
598300040218     c                   goto      Fine
598400040218e   1c                   endif
598500940802     C*
598600990119     C* F5 - INTERROGAZIONE EVENTI
598700040218do  1C   KE              DO
598800940905     C                   MOVEL     'S'           WCMD
598900940726     C                   MOVEL     V2CAAS        PB5AAS
599000940726     C                   MOVEL     COMLNP        PB5LNP
599100940726     C                   MOVEL     V2CNRS        PB5NRS
599200940726     C                   MOVEL     V2CNSP        PB5NSP
599300940726     C                   MOVEL     *BLANKS       PB5F03
599400940726     C                   MOVEL     *BLANKS       PB5FLG
599500940802     C                   MOVEL     *BLANKS       PB5PGM
599600131107     C                   MOVEL     'FNLRI6R'     PB5PGM
599700940726     C                   MOVEL     V1CDSA        PB5DSA
599800940726     C                   MOVEL     V3CDSP        PB5DSP
599900940726     C                   MOVEL     VIDRMN        PB5RMN
600000940726     C                   MOVEL     V3CLNA        PB5LNA
600100940802     C                   MOVEL     V3DLNA        PB5LN1
600200940726     C                   MOVEL     V4CRSD        PB5RSD
600300940726     C                   MOVEL     VIDDCP        PB5DCP
600400941031     C                   MOVEL     VIDTMC        PB5TMC
600500940726     C                   MOVEL     PARAB5        KPJBU
600600940722     C                   CALL      'FNLRB5R'
600700930525     C                   PARM                    KPJBA
600800941109     C                   MOVEL     KPJBU         PARAB5
600900941109     C                   MOVEL     PB5F03        WF03
601000941214     C                   MOVEL     '1'           PA0B5
601100940725     C                   MOVEL     'S'           WRIEM
601200040218e   1C                   ENDDO
601300070420      *?F6 - Stampa LDV
601400070420if  1c                   if        *inKf = *on
601500070420     C                   MOVEL     'S'           WCMD
601600080201     c* Se assegnato non abilitato non stampo la LdV ma emetto errore
601700080201     c                   if        arbaca=' '
601800080201     C                   MOVEL     MSG(31)       V1CMSG
601900080201     C                   SETON                                        28
602000080201     c                   else
602100070420     c
602200070420     c                   if        wrichtrul90=' '
602300070420     c* La prima volta devo proporre la richiesta stampanti
602400070420     c                   exsr      RICHSTAMPAN
602500070420     C                   MOVEL     'S'           WRIEM
602600070420     c
602700070420     c* Se premuto F3 esco senza fare nulla
602800070420     c                   if        d90f3<>'1' and not *in91
602900070420     c                   eval      wrichtrul90='S'
603000070420     c                   endif
603100070420     c                   if        *in91
603200070420     C                   MOVEL     MSG(30)       V1CMSG
603300070420     C                   SETON                                        28
603400070420     c                   endif
603500070420     c                   endif
603600070420     c
603700070420     c                   if        wrichtrul90='S'
603800070420     C                   CLEAR                   FNLSB5DS
603900070426     c                   movel     'L'           db0tla
604000070420     C                   MOVEL     'R'           DB0RIS
604100070426     C                   MOVEL     'A'           DB0TBO
604200070420     C                   MOVEL     ARBAAS        DB0AAS
604300070420     C                   MOVEL     ARBLNP        DB0LNP
604400070420     C                   MOVEL     ARBNRS        DB0NRS
604500070420     C                   MOVEL     ARBNSP        DB0NSP
604600130319     c                   reset                   fnlsB6ds1
604700070420     C                   CALL      D90PSL
604800070420     C                   PARM                    FNLSB5DS
604900130308     c                   parm                    fnlsB6ds1
605000070420     C                   MOVEL     MSG(29)       V1CMSG
605100070420     C                   SETON                                        28
605200070420     c                   endif
605300080201     c                   endif
605400070420     c                   endif
605500930525     C*
605600990119     C* F7 - ANOMALIE
605700040218do  1C   KG              DO
605800960523     C                   MOVEL     'S'           WCMD
605900960523     C*
606000991027     C                   CLEAR                   FNLr44DS
606100960523     C                   MOVE      V2CAAS        D44AAS
606200960523     C                   MOVE      COMLNP        D44FIL
606300960523     C                   MOVE      V2CNRS        D44NRS
606400960523     C                   MOVE      V2CNSP        D44NSP
606500960523     C                   MOVEL     'A'           D44TBO
606600960523     C                   MOVE      'I'           D44MOD
606700991027     C                   MOVEL     FNLr44DS      KPJBU
606800960523     C                   CALL      'FNLR44R'
606900960523     C                   PARM                    KPJBA
607000960523     C*
607100990119     C                   MOVEL     'S'           WRIEM
607200040218e   1C                   ENDDO
607300920804     C*
607400990119     C* F8  - INTERROGAZIONE EVENTI SEGNACOLLI
607500040218if  1C     *INKH         IFEQ      *ON
607600961121     C                   MOVEL     'S'           WCMD
607700961121     C*
607800991027     C                   CLEAR                   FNLR45DS
607900961121     C                   MOVE      V2CAAS        D45AAS
608000961121     C                   MOVE      COMLNP        D45FIL
608100961121     C                   MOVE      V2CNRS        D45NRS
608200961121     C                   MOVE      V2CNSP        D45NSP
608300991027     C                   MOVEL     FNLR45DS      KPJBU
608400961121     C                   CALL      'FNLR45R'
608500961121     C                   PARM                    KPJBA
608600961121     C*
608700990119     C                   MOVEL     'S'           WRIEM
608800040218e   1C                   ENDIF
608900130830     c
609000130830     c* F9 - Orari servizi
609100131007     c                   if        *inki=*on
609200130830     C                   MOVEL     'S'           WCMD
609300131007     c                   if        wabif9='S'
609400130930     c                   exsr      sr_orarisr
609500131007     c                   endif
609600130830     C                   MOVEL     'S'           WRIEM
609700130830e   1C                   ENDIF
609800021213     C*
609900021213     C* F11 - Note supermercati
610000040218if  1C     *INKK         IFEQ      *ON
610100021213     C     *IN77         ANDEQ     *ON
610200021213      * imposto i campi 'bolla' della ds
610300021217     C                   MOVEL     'S'           WCMD
610400021213     C                   clear                   FnLrb3ds
610500030122     c                   Movel     V1cdsa        Rb3Rsu
610600030121     C                   z-add     SimFel        RB3Fil
610700021216     C                   z-add     V3CAAS        RB3AAS
610800030121     C                   z-add     V3CLNP        RB3LNP
610900021216     C                   z-add     V3CNRS        RB3NRS
611000021216     C                   z-add     V3CNSP        RB3NSP
611100021213     C                   z-add     V3CDSP        RB3DSP
611200021213     C                   z-add     VIDRMN        RB3RMN
611300021213     C                   z-add     V3CLNA        RB3LNA
611400131108     C                   movel     V32RSD        RB3RSD
611500021218     C                   z-add     VIDPKF        RB3PKF
611600021213     C                   z-add     VIDNCL        RB3NCL
611700021213     C                   z-add     VIDVLF        RB3VLB
611800030703      * bancali - part.varie "B"
611900040218If  2c                   If        %subst(ArbGva:1:1) = 'B'
612000030121     C                   z-add     §Ar5Tba       RB3Tba
612100030121     C                   z-add     VidNba        RB3Nba
612200030121     C                   z-add     §Ar5Tb2       RB3Tb2
612300030121     C                   z-add     VidNb2        RB3Nb2
612400030703     c                   Movel     V4cGva        Rb3Gva
612500040218e   2c                   EndIf
612600030703      * bancali - part.varie "O"
612700040218If  2c                   If        %subst(ArbGva:1:1) = 'O'
612800030703     C                   z-add     §Ar5bTba      RB3Tba
612900030703     C                   z-add     VidNba        RB3Nba
613000030703     C                   z-add     §Ar5bTb2      RB3Tb2
613100030703     C                   z-add     VidNb2        RB3Nb2
613200030703     c                   Movel     V4cGva        Rb3Gva
613300030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
613400030703     c                   Z-Add     §Ar5bCor      Rb3Cor
613500040218e   2c                   EndIf
613600021213      * richiamo PGM per note supermercati
613700021213     C                   call      'FNLRB3R'
613800030122     C                   PARM                    Kpjba
613900030121     C                   PARM                    Fnlrb3Ds
614000021213      *
614100021217     C                   MOVEL     'S'           WRIEM
614200040218e   1C                   ENDIF
614300961121     C*
614400990119     C* F14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
614500040218if  1C     *INKN         IFEQ      *ON
614600930624     C     *IN03         ANDEQ     *ON
614700941213     C     PA1GIA        ANDEQ     *BLANK
614800940905     C                   MOVEL     'S'           WCMD
614900991027     C                   CLEAR                   FNLG30DS
615000941213     C                   MOVEL     'A'           D30POA
615100941213     C                   MOVEL     V2CAAS        D30AAS
615200941213     C                   MOVEL     COMLNP        D30LNP
615300941213     C                   MOVEL     V2CNRS        D30NRS
615400941213     C                   MOVEL     V2CNSP        D30NSP
615500991027     C                   MOVEL     FNLG30DS      KPJBU
615600941213     C                   CALL      'FNLG31C'
615700920804     C                   PARM                    KPJBA
615800930624     C*
615900990119     C                   MOVEL     'S'           WRIEM
616000040218e   1C                   ENDIF
616100040218      *
616200040218      *?F16 - Gestione reclami?
616300040218if  1c                   if        *inKQ = *on
616400040225     c                   eval      wARBaas = ARBaas
616500131111     c                   eval      wARBlnp = ARBlnp
616600131111     c                   eval      wARBnrs = ARBnrs
616700131111     c                   eval      wARBnsp = ARBnsp
616800040225     c                   reset                   FIDNA2ds
616900040225if  2c                   if        SDSprm  > 1
617000040225     c                   eval      IA2mod  = 'I'
617100040225e   2c                   endif
617200040225     c                   eval      IA2ogg  = ds_OggSped
617300040225     c                   call      'FIDNA2R'
617400040225     c                   parm                    KPJBA
617500040225     c                   parm                    FIDNA2ds
617600040225     c                   movel     OA2f03        WF03
617700040225     c                   movel     'S'           WRIEM
617800051201     C                   MOVEL     'S'           WCMD
617900040218e   1c                   endif
618000070404      *
618100140311      *?F17 - Note AUT
618200070404if  1c                   if        *inKR = *on
618300140123     c                   clear                   fnlrn6ds
618400140123     c                   eval      in6mod='V'
618500140123     c                   eval      in6aas=v2caas
618600140123     c                   eval      in6lnp=comlnp
618700140123     c                   eval      in6nrs=v2cnrs
618800140123     c                   eval      in6nsp=v2cnsp
618900140123      * se sono una spedizione in consegna passo i dati della distinta del file ARB
619000140123     c                   IF        ARBfbc = *blanks and
619100140123     c                               ARBddc > 0 and
619200140123     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
619300140123     c                               arbica='R' or arbicc=' ' or arbicc='R')
619400140123     c     KSPED         chain     Fnarb01l
619500140123     c                   If        %found(Fnarb01l)
619600140123     c                   eval      in6fgs = arbifp
619700140123     c                   eval      in6nfv = arbndc
619800140123     c                   eval      in6dfv = arbddc
619900140123     c                   eval      in6pdr = arbpdc
620000140123     c                   eval      in6cons = 'S'
620100140123     c                   Endif
620200140123     c                   Endif
620300140123     c                   call      'FNLRN6R'
620400070404     c                   parm                    kpjba
620500140123     c                   parm                    fnlrn6ds
620600070404     c                   movel     'S'           WRIEM
620700070404     C                   MOVEL     'S'           WCMD
620800070404e   1c                   endif
620900920804     C*
621000990119     C* F21 - DETTAGLIO SEGNACOLLI
621100040218do  1C   KV              DO
621200940905     C                   MOVEL     'S'           WCMD
621300950628     C                   MOVEL     'A'           PA1TBO
621400950629     C                   CLEAR                   PA1F03
621500940720     C                   MOVEL     PARAM1        KPJBU
621600940726     C                   CALL      'FNLR37R'
621700920804     C                   PARM                    KPJBA
621800940802     C                   MOVEL     KPJBU         PARAM1
621900940802     C                   MOVEL     PA1F03        WF03
622000990119     C                   MOVEL     'S'           WRIEM
622100040218e   1C                   ENDDO
622200920804     C*
622300990119     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
622400040218do  1C   KJ              DO
622500940905     C                   MOVEL     'S'           WCMD
622600950628     C                   MOVEL     'A'           PA1TBO
622700950629     C                   CLEAR                   PA1F03
622800940712     C                   MOVEL     PARAM1        KPJBU
622900940726     C                   CALL      'FNLR75R'
623000920804     C                   PARM                    KPJBA
623100941214     C                   MOVEL     '1'           PA075
623200940802     C                   MOVEL     KPJBU         PARAM1
623300940802     C                   MOVEL     PA1F03        WF03
623400990119     C                   MOVEL     'S'           WRIEM
623500040218e   1C                   ENDDO
623600930114     C*
623700990119     C* F19 - INTERROGA SPUNTE VARI
623800040218if  1C     *INKT         IFEQ      *ON
623900940905     C                   MOVEL     'S'           WCMD
624000940713     C                   Z-ADD     0             PA4NFV
624100940713     C                   MOVEL     9             PA4NPG
624200940713     C                   MOVEL     99            PA4NPS
624300101202     C                   MOVEL     pa1pgm        PA4PGM
624400101203     C                   MOVEL     '1'           PA4FLG
624500950214     C                   MOVEL     V3CFLS        PA4LNP
624600940719     C                   MOVEL     V3CLNA        PA4LNA
624700940719     C                   MOVEL     V3CNRS        PA4NRS
624800940713     C                   Z-ADD     0             PA4NCD
624900940713     C                   Z-ADD     0             PA4NCA
625000101201     c                   eval      pa4boan=v2caas
625100101201     c                   eval      pa4bolp=comlnp
625200101201     c                   eval      pa4bors=v2cnrs
625300101201     c                   eval      pa4bosp=v2cnsp
625400040218if  2C     VIDNCD        IFGT      0
625500940713     C                   MOVEL     VIDNCD        PA4NCD
625600940713     C                   MOVEL     VIDNCA        PA4NCA
625700101201x   2C**                 ELSE
625800930705     C* CERCO I SEGNACOLLI DAL DETTAGLIO
625900101201     C**   KARB2         SETLL     FNART01L
626000101201     C**   KARB2         READE     FNART01L                               31
626100101201do  3C*+N31              DO
626200101201     C**                 Z-ADD     ARTNSC        PA4NCD
626300101201     C**   KARB2         SETGT     FNART01L
626400101201     C**   KARB2         READPE    FNART01L                               31
626500101201if  4C**N31ARTNSC        IFNE      PA4NCD
626600101201     C**                 MOVEL     ARTNSC        PA4NCA
626700101201e   4C**                 ENDIF
626800101201e   3C**                 ENDDO
626900101201e   2C                   ENDIF
627000930705     C*
627100940802     C                   MOVEL     PARA04        KPJBU
627200940722     C                   CALL      'FNLR04R'
627300920804     C                   PARM                    KPJBA
627400940725     C                   MOVEL     'S'           WRIEM
627500060130     C                   MOVEL     'S'           WCMD
627600040218e   1C                   ENDIF
627700930415     C*
627800990119     C* F23 - VISUALIZZA DATA CONSEGNA PREVISTA ESTERNA E ORE RITARDO
627900131121do  1C** KX              DO
628000131121     C**                 MOVEL     'S'           WCMD
628100131121     C**                 SETOFF                                       68
628200930504     C*
628300131121     C**                 MOVEL     ' '           D98TLA
628400131121     C**                 MOVEL     'A'           D98TBO
628500131121     C**                 MOVEL     ARBAAS        D98AAS
628600131121     C**                 MOVEL     ARBLNP        D98LNP
628700131121     C**                 MOVEL     ARBNRS        D98NRS
628800131121     C**                 MOVEL     ARBNSP        D98NSP
628900930504     C*
629000131121     C**                 CALL      'TNSD99R'
629100131121     C**                 PARM                    TNSD99DS
629200930504     C*
629300131121     c**                 if        d98dce>0
629400131121     c**   *iso          movel     d98dce        dataeur
629500131121     c**                 movel     dataeur       datadmy
629600131121     c**                 movel     dataeur       v08dce
629700131121     c**                 movel     datadmy       v06dce
629800131121     c**                 else
629900131121     c**                 clear                   v08dce
630000131121     c**                 clear                   v06dce
630100131121     c**                 endif
630200070404     c
630300131121     C**                 Z-ADD     D98NRC        VIDNRC
630400930526     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
630500930526     C*   CONSEGNA PREVISTA
630600131121     c**n69              eval      vidhmc=%editw(d98hce: '0 :  ')
630700930504     C*
630800930504     C* SE COMODO = 1  E' STATA FATTA CALL PGM
630900131121     C**                 MOVEL     '1'           PA098
631000131121e   1C**                 ENDDO
631100931112      *
631200990119      * F20 - INTERROGAZIONE AFFIDABILITA'
631300040218do  1C   KU              DO
631400940905     C                   MOVEL     'S'           WCMD
631500950102     C                   CLEAR                   FNLR97
631600950102     C                   MOVEL     V2CAAS        R97AAS
631700950102     C                   MOVEL     COMLNP        R97LNP
631800950102     C                   MOVEL     V2CNRS        R97NRS
631900950102     C                   MOVEL     V2CNSP        R97NSP
632000950102     C                   MOVEL     V3CDSP        R97DSP
632100950102     C                   MOVEL     VIDRMN        R97RMN
632200950102     C                   MOVEL     V3CLNA        R97LNA
632300950102     C                   MOVEL     V3DLNA        R97DLA
632400950102     C                   Z-ADD     VIDDCP        R97DCP
632500950102     C                   MOVEL     V4CRSD        R97RSD
632600950102     C                   MOVEL     V1CDSA        R97RSU
632700040121     C                   MOVEL     *blanks       R97CML
632800950102     C                   MOVEL     'A'           R97AP
632900950102     C   95              MOVEL     'S'           R97EVE
633000070404     C                   Z-ADD     w08DBR        R97DBR
633100070404     C                   Z-ADD     w08DPC        R97DPC
633200070404     C                   Z-ADD     w08DUC        R97DUC
633300950102     C                   MOVEL     VIDTMC        R97TMC
633400950102     C                   Z-ADD     VIDCAS        R97CAS
633500950102     C                   MOVEL     VIDVCA        R97VCA
633600931112     C                   MOVE      *BLANKS       KPJBU
633700950102     C                   MOVEL     FNLR97        KPJBU
633800940722     C                   CALL      'FNLR97R'                            90
633900931112     C                   PARM                    KPJBA
634000950102     C                   MOVEL     KPJBU         FNLR97
634100950102     C                   MOVEL     R97F03        WF03
634200950102     C                   MOVEL     'S'           WRIEM
634300040218e   1C                   ENDDO
634400110930     C* F22 - Perfect Order
634500110929do  1C                   if        *inkw
634600110929     C                   MOVEL     'S'           WCMD
634700110929     c                   movel     param1        s_param1
634800110929     c*
634900110929     c                   MOVEL     'W'           pa1flg
635000131107     c                   MOVEL(p)  'FNLRI6R'     pa1pgm
635100111005     c                   movel(p)  vidrma        pa1rma
635200111005     c                   movel(p)  vidxco        pa1xco
635300110929     c                   movel     paraaz        kpjbu
635400110929     c                   movel     param1        kpjbu
635500131127     c                   call      'FNLR36C'
635600110929     c                   parm                    kpjba
635700110929     c*
635800110929     c                   movel     s_param1      param1
635900110929     C*
636000110929     C                   MOVEL     'S'           WRIEM
636100110929e   1C                   ENDif
636200940905     C*
636300131107     C* ? FORMATO VIDEO:  LRI6D06  (TASSAZIONE 1a BOLLA)   ---   TASTO F13 ?
636400991026     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
636500040218if  1C     *INKM         IFEQ      *ON
636600991027     C                   eval      WCMD = 'S'
636700991027     C                   EVAL      *in89 = *off
636800131107     C                   WRITE     LRI6T01
636900131107     C                   WRITE     LRI6D06
637000991027     C                   eval      NR6 = 1
637100131107     C                   EXFMT     LRI6C06
637200040218e   1C                   ENDIF
637300991027     C*
637400131107     C* ? FORMATO VIDEO:  LRI6D09  (TASSAZIONE 2a BOLLA)   ---   TASTO F04 ?
637500991027     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
637600040218if  1C     *INKD         IFEQ      *ON
637700991027     C                   eval      WCMD = 'S'
637800991027     C                   EVAL      *in89 = *off
637900131107     C                   WRITE     LRI6T01
638000131107     C                   EXFMT     LRI6D09
638100040218e   1C                   ENDIF
638200990119     C*
638300990119     C* F2 - INTERROGAZIONE COMUNICAZIONE ANOMALIA
638400990129     C*      NON RICHIAMO SE RICHIAMATO DA INTERROGAZINE C.A.
638500040218if  1C     *INKB         IFEQ      *ON
638600990129     C     PA1FLG        ANDNE     'C'
638700040218if  2C     CONTCA        IFEQ      1
638800991027     C                   CLEAR                   FIDN00DS
638900990119     C                   Z-ADD     VHCAAC        I00AAC
639000990119     C                   Z-ADD     VHCFIL        I00FIL
639100990119     C                   Z-ADD     VHCNCA        I00NCA
639200990119     C                   Z-ADD     ARBAAS        I00AAS
639300990119     C                   Z-ADD     ARBNRS        I00NRS
639400990119     C                   Z-ADD     ARBNSP        I00NSP
639500990119     C                   Z-ADD     ARBLNP        I00LNP
639600990119     C                   MOVEL     'A'           I00TPB
639700991027     C                   MOVEL     FIDN00DS      KPJBU
639800990119     C                   CALL      'FIDN01R'
639900990119     C                   PARM                    KPJBA
640000000309     C                   PARM                    callricors        1
640100040218x   2C                   ELSE
640200990119     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
640300990119     C                   SETOFF                                       79
640400131107     C                   WRITE     LRI6C11
640500990119     C                   SETON                                        797823
640600990119     C                   CLEAR                   NRR11
640700040809      *
640800040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
640900040809      * legate alla spedizione che passo
641000040809     c                   clear                   fidn12ds
641100040809     c                   eval      i12aas = arbAAS
641200040809     c                   eval      i12lnp = arbLNP
641300040809     c                   eval      i12nrs = arbNRS
641400040809     c                   eval      i12nsp = arbNSP
641500040809     c                   eval      i12fel = 'E'
641600040809     c                   eval      i12fan = 'E'
641700040809     c*
641800040809     c                   call      'FIDN12R'
641900040809     c                   parm                    fidn12ds
642000040809     c*
642100040809     c                   setoff                                       31
642200040809     c*
642300040809     c* se non ci sono errori
642400040809     c                   if        o12err <> ' '
642500040809     c                   seton                                        31
642600040809     c                   else
642700040809     c* se numero di CA trovate maggiore di zero
642800040809     c                   if        o12nca = 0
642900040809     c                   seton                                        31
643000040809     c                   else
643100040809     c                   z-add     1             ii                2 0
643200040809     c                   dow       ii <= o12nca and *in31 = *off
643300040809     c                   movea     skkey(ii)     dskey
643400040809     C                   Z-ADD     dsaac         V11AAC
643500040809     C                   Z-ADD     dsfil         V11FIL
643600040809     C                   Z-ADD     dsnca         V11NCA
643700040809     C                   Z-ADD     skdca(ii)     G02INV
643800040809     C                   Z-ADD     *ZERO         G02DAT
643900040809     C                   MOVEL     '3'           G02ERR
644000040809     C                   CALL      'XSRDA8'
644100040809     C                   PARM                    WLBDAT
644200040809     C                   Z-ADD     G02DAT        V11DCA
644300040809     C                   Z-ADD     skfca(ii)     V11FCA
644400040809     C     skfca(ii)     CHAIN     FNDFA01L
644500040809     C                   IF        %found(FNDFA01L)
644600040809     C                   MOVEL     DFADES        V11DFA
644700040809     C                   ENDIF
644800040809     C**
644900040809     C                   ADD       1             NRR11             4 0
645000131107     C                   WRITE     LRI6S11
645100040809     c*
645200040809     c                   add       1             ii
645300040809     c                   enddo
645400040809     c*
645500040809     c                   seton                                        31
645600040809     c*
645700040809     c                   endif
645800040809     c                   endif
645900040809     c**
646000990119     C* EMETTO FORMATO
646100990119     C     FOR11         TAG
646200990119     C                   Z-ADD     1             NRR11
646300131107     C                   WRITE     LRI6T01
646400131107     C                   WRITE     LRI6Z11
646500131107     C                   EXFMT     LRI6C11
646600990119     C* F12 - RITORNO
646700040218if  3C     *INKL         IFEQ      *OFF
646800131107     C                   READC     LRI6S11                                31
646900040218do  4C     *IN31         DOWEQ     *OFF
647000040218if  5C     V11SCE        IFEQ      '5'
647100991027     C                   CLEAR                   FIDN00DS
647200990119     C                   Z-ADD     V11AAC        I00AAC
647300990119     C                   Z-ADD     V11FIL        I00FIL
647400990119     C                   Z-ADD     V11NCA        I00NCA
647500990119     C                   Z-ADD     ARBAAS        I00AAS
647600990119     C                   Z-ADD     ARBNRS        I00NRS
647700990119     C                   Z-ADD     ARBNSP        I00NSP
647800990119     C                   Z-ADD     ARBLNP        I00LNP
647900990119     C                   MOVEL     'A'           I00TPB
648000991027     C                   MOVEL     FIDN00DS      KPJBU
648100990119     C                   CALL      'FIDN01R'
648200990119     C                   PARM                    KPJBA
648300000309     C                   PARM                    callricors        1
648400990119     C**
648500990119     C                   CLEAR                   V11SCE
648600131107     C                   UPDATE    LRI6S11
648700040218e   5C                   ENDIF
648800990119     C**
648900131107     C                   READC     LRI6S11                                31
649000040218e   4C                   ENDDO
649100990119     C                   GOTO      FOR11
649200040218e   3C                   ENDIF
649300040218e   2C                   ENDIF
649400990119     C*
649500990119     C                   MOVEL     'S'           WRIEM
649600990119     C                   MOVEL     'S'           WCMD
649700040218e   1C                   ENDIF
649800931112      *
649900920804     C                   ENDSR
650000070420     C**************************************************************************
650100070420      * RICHIEDO LE STAMPANTI per stampa LDV
650200070420     C**************************************************************************
650300070420     c     RICHSTAMPAN   BEGSR
650400070420     C                   CLEAR                   TRUL90DS
650500070420      *
650600070420     C                   MOVEL     'S'           d90RSB
650700070420     C                   CALL      'TRUL90R'
650800070420     C                   PARM                    KPJBA
650900070420     C                   PARM                    trul90DS
651000070420      *
651100070420     C* F3 - FINE
651200070420     c                   if        d90f3<>'1'
651300070420      *
651400070420      * OVRPRTF BOLLE A4
651500070426     c                   if        *in97
651600070420     C                   MOVEA     D90PRB4       CMA4(30)
651700070420     C                   MOVEA     D90MDB4       CMA4(52)
651800070426     c                   else
651900070426     C                   MOVEA     D90PRB4       CMA4f6(30)
652000070426     C                   MOVEA     D90MDB4       CMA4f6(52)
652100070426     c                   endif
652200070420     C*
652300111221     C                   Z-ADD     130           LUNG             15 5
652400111221     C                   CLEAR                   COMMAN          130
652500070426     C   97              MOVEA     CMA4          COMMAN
652600070426     C  n97              MOVEA     CMA4f6        COMMAN
652700070420     C                   CALL      'QCMDEXC'                            91
652800070420     C                   PARM                    COMMAN
652900070420     C                   PARM                    LUNG
653000070420      *
653100070420     C                   if        not *in91
653200070420      *
653300070420      * OVRPRTF BOLLE A5
653400070426     c                   if        *in97
653500070420     C                   MOVEA     D90PRB5       CMA5(30)
653600070420     C                   MOVEA     D90MDB5       CMA5(52)
653700070426     c                   else
653800070426     C                   MOVEA     D90PRB5       CMA5f6(30)
653900070426     C                   MOVEA     D90MDB5       CMA5f6(52)
654000070426     c                   endif
654100070420     C*
654200111221     C                   Z-ADD     130           LUNG
654300070420     C                   CLEAR                   COMMAN
654400070426     C   97              MOVEA     CMA5          COMMAN
654500070426     C  n97              MOVEA     CMA5f6        COMMAN
654600070420     C                   CALL      'QCMDEXC'                            91
654700070420     C                   PARM                    COMMAN
654800070420     C                   PARM                    LUNG
654900070420     c                   endif
655000070420     c                   endif
655100070420      *
655200070420     c                   ENDSR
655300940802     C**************************************************************************
655400940802     C*    VISUALIZZA LEGAMI BOLLA
655500940802     C**************************************************************************
655600930413     C     CMD18         BEGSR
655700940802     C*
655800940411     C                   CLEAR                   WCAR
655900940411     C*
656000940411     C* MUOVO I DATI DELLA BOLLA CHE STO VISUALIZZANDO
656100940714     C                   MOVEL     V2CAAS        NUM2              4 0
656200940411     C                   MOVEL     COMLNP        COMLPN
656300940714     C                   MOVEL     V2CNRS        COMNRN
656400940714     C                   MOVEL     V2CNSP        COMNSN
656500950321     C                   MOVEL     V3CLNA        COMLAN
656600950428     C                   MOVEL     COMC08        SAVC08
656700940408     C**
656800940713     C* SAVNR8 > 1 --> GESTIONE SUBFILE
656900940408     C**
657000940907    1C     SAVNR8        IFGT      2
657100940408     C*
657200940809     C                   MOVEL     'S'           WRIEM
657300930413     C     FORC2         TAG
657400940713     C                   Z-ADD     1             REC8
657500940809     C     WRIEM         IFEQ      'S'
657600131107     C                   WRITE     LRI6T01
657700131107     C                   WRITE     LRI6Z08
657800940809     C                   MOVEL     *BLANKS       WRIEM
657900940809     C                   ENDIF
658000950428     C*
658100950428     C                   MOVEL     SAVC08        COMC08
658200131107     C                   EXFMT     LRI6C08
658300930413     C*
658400930413     C* CMD12 - FINE LAVORO
658500930413     C   KL              GOTO      SOTTO
658600930413     C*
658700131107     C                   READC     LRI6S08                                93
658800930510     C   93              GOTO      SOTTO
658900930510     C*
659000940408    2C     *IN93         DOWEQ     '0'
659100930413     C*
659200941230    3C     V8CSCE        IFEQ      '5'
659300940713     C                   MOVEL     ' '           V8CSCE
659400950103     C*
659500950103     C* RIPRISTINO GLI INDICATORI
659600950103     C                   MOVEL     V8CI80        *IN80
659700950103     C                   MOVEL     V8CI81        *IN81
659800950103     C                   MOVEL     V8CI82        *IN82
659900950103     C                   MOVEL     V8CI83        *IN83
660000131107     C                   UPDATE    LRI6S08
660100930504     C*
660200940907     C     V8CAAN        IFEQ      *ZERO
660300940907     C                   MOVEL     V8CAAP        LBLAAN
660400940907     C                   MOVEL     V8CLPP        LBLLPN
660500940907     C                   MOVEL     V8CNRP        LBLNRN
660600940907     C                   MOVEL     V8CNSP        LBLNSN
660700940907     C                   MOVEL     'P'           V8CFPS            1
660800940907     C                   ELSE
660900940907     C                   MOVEL     V8CAAN        LBLAAN
661000940907     C                   MOVEL     V8CLPN        LBLLPN
661100940907     C                   MOVEL     V8CNRN        LBLNRN
661200940907     C                   MOVEL     V8CNSN        LBLNSN
661300940907     C                   MOVEL     'S'           V8CFPS
661400940907     C                   ENDIF
661500940408     C*
661600940408     C* CONTROLLO SE IL LEGAME E' IN PARTENZA O IN ARRIVO
661700940907     C     KLBL1         CHAIN     FNARB01L                           93
661800940408     C*
661900940408    4C     *IN93         IFEQ      *OFF
662000940408     C*
662100940408     C                   EXSR      LEG2
662200940408     C*
662300940408   X4C                   ELSE
662400940408     C*
662500950316     C                   Z-ADD     LBLAAN        PA0AAS
662600950316     C                   Z-ADD     LBLLPN        PA0LNP
662700950316     C                   Z-ADD     LBLNRN        PA0NRS
662800950428     C                   Z-ADD     LBLNSN        PA0NSP
662900060130     C                   MOVEL     rsut          PA0RSU
663000941214     C                   MOVEL     SIMFEL        PA0SIM
663100040121     C                   MOVEL     *blanks       PA0CML
663200960903     C     PA1FLG        IFEQ      'N'
663300960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
663400960903     C*       verifichi questo caso
663500960903     C                   MOVEL     'R'           PA0FLG
663600960903     C                   ELSE
663700941214     C                   MOVEL     'A'           PA0FLG
663800960903     C                   END
663900941214     C                   MOVEL     *BLANKS       PA0CLI
664000950629     C                   CLEAR                   PA0F03
664100941214     C                   MOVEL     *BLANKS       PA0GIA
664200941214     C                   MOVEL     PARAM0        KPJBU
664300940722     C                   CALL      'FNLS05R'
664400930413     C                   PARM                    KPJBA
664500941214     C                   MOVEL     KPJBU         PARAM0
664600941214     C                   MOVEL     PA0F03        WF03
664700940907     C                   MOVEL     'S'           WRIEM
664800940408    4C                   ENDIF
664900940408     C*
665000940408    3C                   ENDIF
665100930413     C*
665200131107     C                   READC     LRI6S08                                93
665300940408    2C                   ENDDO
665400930413     C                   GOTO      FORC2
665500930413     C*
665600930413     C     SOTTO         TAG
665700930413     C*
665800940408    1C                   ELSE
665900940408     C*
666000940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
666100131107     C     2             CHAIN     LRI6S08                            99
666200940907     C     V8CAAP        IFEQ      V3CAAS
666300940907     C     V8CLPP        ANDEQ     V3CLNP
666400940907     C     V8CNRP        ANDEQ     V3CNRS
666500940907     C     V8CNSP        ANDEQ     V3CNSP
666600940907     C                   MOVEL     V8CAAN        ARBAAS
666700940907     C                   MOVEL     V8CLPN        ARBLNP
666800940907     C                   MOVEL     V8CNRN        ARBNRS
666900940907     C                   MOVEL     V8CNSN        ARBNSP
667000940907     C                   MOVEL     'S'           V8CFPS
667100940907     C                   ELSE
667200940907     C                   MOVEL     V8CAAP        ARBAAS
667300940907     C                   MOVEL     V8CLPP        ARBLNP
667400940907     C                   MOVEL     V8CNRP        ARBNRS
667500940907     C                   MOVEL     V8CNSP        ARBNSP
667600940907     C                   MOVEL     'P'           V8CFPS
667700940907     C                   ENDIF
667800940907     C* QUINDI VEDO SE LA TROVO FRA GLI ARRIVI
667900940907     C     KARB2         CHAIN     FNARB01L                           93
668000940408     C*
668100121116    2C     *IN93         IFEQ      *ON
668200950316     C                   Z-ADD     ARBAAS        PA0AAS
668300950316     C                   Z-ADD     ARBLNP        PA0LNP
668400950316     C                   Z-ADD     ARBNRS        PA0NRS
668500950316     C                   Z-ADD     ARBNSP        PA0NSP
668600060130     C                   MOVEL     rsut          PA0RSU
668700941214     C                   MOVEL     SIMFEL        PA0SIM
668800040121     C                   MOVEL     *blanks       PA0CML
668900960903     C     PA1FLG        IFEQ      'N'
669000960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
669100960903     C*       verifichi questo caso
669200960903     C                   MOVEL     'R'           PA0FLG
669300960903     C                   ELSE
669400941214     C                   MOVEL     'A'           PA0FLG
669500960903     C                   END
669600941214     C                   MOVEL     *BLANKS       PA0CLI
669700950629     C                   CLEAR                   PA0F03
669800941214     C                   MOVEL     *BLANKS       PA0GIA
669900941214     C                   MOVEL     PARAM0        KPJBU
670000940722     C                   CALL      'FNLS05R'
670100930413     C                   PARM                    KPJBA
670200941214     C                   MOVEL     KPJBU         PARAM0
670300941214     C                   MOVEL     PA0F03        WF03
670400940907     C                   MOVEL     'S'           WRIEM
670500121116     c* reimposto la key spedizione
670600121116     C                   MOVEL     NUM2          arbAAS
670700121116     C                   MOVEL     COMLPN        arbLNP
670800121116     C                   MOVEL     COMNRN        arbNRS
670900121116     C                   MOVEL     COMNSN        arbNSP
671000930510     C*
671100940408   X2C                   ELSE
671200940408     C*
671300940408     C                   EXSR      LEG2
671400940408     C*
671500940408    2C                   END
671600940408    1C                   END
671700940411     C*
671800940411     C* RICARICO I DATI DELLA BOLLA INIZIALE
671900940411     C     WCAR          IFEQ      'C'
672000940411     C                   SETOFF                                       75
672100940714     C                   MOVEL     NUM2          V2CAAS
672200940411     C                   MOVEL     COMLPN        COMLNP
672300940714     C                   MOVEL     COMNRN        V2CNRS
672400940714     C                   MOVEL     COMNSN        V2CNSP
672500940712     C     KARB          CHAIN     FNARB01L                           93
672600940411     C* RICARICO I DATI
672700940411     C                   EXSR      BOLLA
672800940411     C*
672900940411     C* RIABILITO IL CMD18
673000940411     C                   SETON                                        94
673100940411    1C                   END
673200940802     C*
673300930413     C                   ENDSR
673400940802     C**************************************************************************
673500940802     C*    LEGAMI BOLLA
673600940802     C**************************************************************************
673700940408     C     LEG2          BEGSR
673800940802     C*
673900940411     C                   MOVEL     'C'           WCAR              1
674000940408     C*
674100940408     C* 75 ON - NON RIENTRO NEL CARICAMENTO LEGAME BOLLA
674200940408     C                   SETON                                        75
674300940408     C* 94 OFF- DISABILITO IL CMD18
674400940408     C                   SETOFF                                       94
674500940408     C* CARICO DATI BOLLA
674600940408     C                   EXSR      BOLLA
674700940411     C*
674800940411     C* VISUALIZZA NUOVO LEGAME
674900940408     C* SE LA BOLLA E' UNA PRECEDENTE --> VISUALIZZO LA SEGUENTE
675000940713     C     V8CFPS        IFEQ      'P'
675100950316     C                   MOVEA     'SEGUEN.'     LBL(14)
675200940408     C                   ELSE
675300940408     C* SE LA BOLLA E' UNA SEGUENTE   --> VISUALIZZO LA PRECEDENTE
675400950316     C                   MOVEA     'PRECED.'     LBL(14)
675500940408     C                   ENDIF
675600950316     C                   MOVEA     COMLPN        LBL(21)
675700950316     C                   MOVEA     COMNRN        LBL(25)
675800950316     C                   MOVEA     COMNSN        LBL(28)
675900950316     C                   MOVEA     COMLAN        LBL(40)
676000950307     C                   MOVEA     *BLANKS       LBL(44)
676100940408     C                   MOVEA     LBL           LEGAME
676200940408     C*
676300940411     C* MOUVO I DATI DELLA LEGATA NEI CAMPI DELLA PARAM
676400940714     C                   MOVEL     ARBAAS        V2CAAS
676500940411     C                   MOVEL     ARBLNP        COMLNP
676600940714     C                   MOVEL     ARBNRS        V2CNRS
676700940714     C                   MOVEL     ARBNSP        V2CNSP
676800940408     C*
676900940408     C                   EXSR      VISUA2
677000940408     C*
677100941230     C                   MOVEL     'S'           WRIEM
677200940408     C                   ENDSR
677300940408     C*
677400940712      **************************************************************************
677500131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
677600940408      *     PER BOLLA NORMAL - CMD18 PREMUTO DA VISUAL -
677700091216      *     ricordarsi di aggiornare anche RIEPI2 per ogni modifica fatta qui
677800940712      **************************************************************************
677900940712     C     RIEPIL        BEGSR
678000940712      *
678100940712     C                   SETOFF                                       89
678200940712      *
678300940712      * EMETTO IL FORMATO VIDEO
678400940720     C                   MOVEL     'S'           WRIEM
678500940712     C     FOR07         TAG
678600940720     C     WRIEM         IFEQ      'S'
678700131107     C                   WRITE     LRI6T01
678800940720     C                   MOVEL     *BLANKS       WRIEM
678900940720     C                   ENDIF
679000021217      * Imposto la stringa comandi del 'giro' successivo
679100021217     C                   IF        *inKY = *on and $Loop > 1
679200021217     c                   Exsr      Ges_F24
679300021217     c                   eval      FD07 = FDxx
679400021217     c                   eval      F24D07 = F24Dxx
679500021217     C                   ENDIF
679600131107     C                   EXFMT     LRI6D07
679700070420     c                   setoff                                       28
679800070420     c                   clear                   v1cmsg
679900940712      *
680000131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
680100940712     C   KY*IN89         IFEQ      *ON
680200940712     C                   SETOFF                                       89
680300940712     C                   ELSE
680400940712     C                   SETON                                        89
680500940712     C                   ENDIF
680600940712     C   KY              GOTO      FOR07
680700940712      *
680800131108      * F3 - FINE LAVORO TORNO A SUBFILE
680900940714     C   KC              MOVEL     '2'           V2CFLG            1
681000940712     C   KC              GOTO      ENDRIE
681100940712      *
681200131108      * F18 - INTERROGA LEGAME BOLLA
681300940712     C   KS              DO
681400940712     C                   EXSR      CMD18
681500940712     C                   GOTO      FOR07
681600940712     C                   END
681700940712      *
681800940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
681900940712     C                   EXSR      COMAND
682000940905     C     WCMD          IFEQ      'S'
682100940802     C     WF03          IFEQ      *BLANKS
682200070404     c
682300940802     C                   GOTO      FOR07
682400940802     C                   ELSE
682500940802     C                   MOVEL     '2'           V2CFLG
682600940802     C                   GOTO      ENDRIE
682700070404     C                   ENDIF
682800091215     c
682900070404     c                   else
683000070404     c
683100070404     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
683200070404     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
683300070404     c                   if        v3cgesd='?'
683400131120
683500131120     c**                 if        wriem='S'
683600131120     c**                 eval      wvideo='R'
683700131120     c**                 exsr      Riemett
683800131120     c**                 endif
683900131120
684000070404     c                   exsr      VISUALAbi
684100070404     c                   clear                   v3cgesd
684200070412     c                   eval      wriem='S'
684300070404     c                   endif
684400131120     c
684500131120     c* controlla se immessi dei '?'
684600131120     c                   eval      wvideo='R'
684700131120     c                   exsr      CTR_puIn
684800091215     c
684900091215     c                   if        wriem='S'
685000091215     C                   GOTO      FOR07
685100091215     c                   endif
685200070404     c
685300940712     C                   END
685400940712      *
685500940712     C     ENDRIE        ENDSR
685600940712      **************************************************************************
685700131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
685800940408      *     PER LEGAME BOLLA - CMD18 PREMUTO DA VISUA2 -
685900940712      **************************************************************************
686000940712     C     RIEPI2        BEGSR
686100940712      *
686200940712     C                   SETOFF                                       89
686300940712      *
686400940712      * EMETTO IL FORMATO VIDEO
686500940809     C                   MOVEL     'S'           WRIEM
686600940712     C     FOR77         TAG
686700940809     C     WRIEM         IFEQ      'S'
686800131107     C                   WRITE     LRI6T01
686900940809     C                   MOVEL     *BLANKS       WRIEM
687000940809     C                   ENDIF
687100091216      * Imposto la stringa comandi del 'giro' successivo
687200091216     C                   IF        *inKY = *on and $Loop > 1
687300091216     c                   Exsr      Ges_F24
687400091216     c                   eval      FD07 = FDxx
687500091216     c                   eval      F24D07 = F24Dxx
687600091216     C                   ENDIF
687700091216     c
687800131107     C                   EXFMT     LRI6D07
687900070516     c                   setoff                                       28
688000070516     c                   clear                   v1cmsg
688100940408     C*
688200131108      * F3 - FINE LAVORO TORNO A SUBFILE
688300940714     C   KC              MOVEL     '2'           V2CFLG            1
688400940408     C*
688500940408     C  NKC              DO
688600940712      *
688700131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
688800940712     C   KY*IN89         IFEQ      *ON
688900940712     C                   SETOFF                                       89
689000940712     C                   ELSE
689100940712     C                   SETON                                        89
689200940712     C                   ENDIF
689300940712     C   KY              GOTO      FOR77
689400940712      *
689500940712      *
689600131108      * F18 - INTERROGA LEGAME BOLLA
689700940712     C   KS              DO
689800940712     C                   EXSR      CMD18
689900940712     C                   GOTO      FOR77
690000940712     C                   END
690100940712      *
690200940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
690300940712     C                   EXSR      COMAND
690400940905     C     WCMD          IFEQ      'S'
690500940712     C                   GOTO      FOR77
690600091216     c
690700091216     c                   else
690800091216     c
690900091216     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
691000091216     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
691100091216     c                   if        v3cgesd='?'
691200131120
691300131120     c**                 if        wriem='S'
691400131120     c**                 eval      wvideo='R'
691500131120     c**                 exsr      Riemett
691600131120     c**                 endif
691700131120
691800091216     c                   exsr      VISUALAbi
691900091216     c                   clear                   v3cgesd
692000091216     c                   eval      wriem='S'
692100091216     c                   endif
692200131120
692300131120     c* controlla se immessi dei '?'
692400131120     c                   eval      wvideo='R'
692500131120     c                   exsr      CTR_puIn
692600091216     c
692700091216     c                   if        wriem='S'
692800091216     C                   GOTO      FOR77
692900091216     c                   endif
693000091216     c
693100940712     C                   END
693200091216     C                   END
693300940712      *
693400940712     C                   ENDSR
693500131120      **************************************************************************
693600131120      * riemissione video per nuovo '?'
693700131120      **************************************************************************
693800131120     c**   Riemett       BEGSR
693900131120     c**                 write     LRI6t01
694000131120
694100131120     c**                 select
694200131120     c**                 when      wvideo='4'
694300131120     c**                 write     LRI6d04
694400131120     c**                 when      wvideo='R'
694500131120     c**                 write     LRI6d07
694600131120     c**                 endsl
694700131120
694800131120     c**                 ENDSR
694900940923      **************************************************************************
695000940923      *     CONTROLLI FORMATO VIDEO D04
695100940923      **************************************************************************
695200131120     C     CTR_puIn      BEGSR
695300110601     c*
695400110601     c* verifico se inserito '?' su email destinatario
695500110601     c                   if        v4cemw='?'
695600110601     c                   exsr      sr_emdest
695700110601     C                   MOVEL     'S'           WRIEM
695800110601     c                   endif
695900940923      *
696000091215      * Verifico se inserito '?' per visualizzazione beneficiario
696100091215     c                   if        vidben='?'
696200131120     c**                 if        wriem='S'
696300131120     c**                 exsr      Riemett
696400131120     c**                 endif
696500131120     c
696600091215     c                   exsr      sr_bencas
696700091215     C                   MOVEL     'S'           WRIEM
696800091215     c                   endif
696900131114     c
697000131114     c* Visualizzazione dati filiale
697100131114     c                   if        viddec_f='?'
697200131120     c**                 if        wriem='S'
697300131120     c**                 exsr      Riemett
697400131120     c**                 endif
697500131120
697600131114     c                   clear                   trulorfds
697700131114     c                   eval      iorffil=arblna
697800131114     c                   call      'TRULORFR'
697900131114     c                   parm                    kpjba
698000131114     c                   parm                    trulorfds
698100131114     c
698200131114     c                   clear                   viddec_f
698300131114     C                   MOVEL     'S'           WRIEM
698400131114     c                   endif
698500091215     c
698600940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
698700940923     C     V4IGCA        IFEQ      '?'
698800991027     C                   CLEAR                   DS7PQRS
698900941003     C                   MOVEL     'S'           DS7RIC
699000941003     C                   MOVEL     'V'           DS7GES
699100941003     C                   MOVEL     V4CGCA        DS7COD
699200991027     C                   MOVEL     DS7PQRS       KPJBU
699300960126     C                   CALL      'TRTB72R'
699400941003     C                   PARM                    KPJBA
699500940923     C                   MOVEL     *BLANKS       V4IGCA
699600940923     C                   MOVEL     'S'           WRIEM
699700940923     C                   ENDIF
699800940923     C*
699900940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
700000940923     C     V4IGGA        IFEQ      '?'
700100991027     C                   CLEAR                   DS7PQRS
700200941003     C                   MOVEL     'S'           DS7RIC
700300941003     C                   MOVEL     'V'           DS7GES
700400941003     C                   MOVEL     V4CGGA        DS7COD
700500991027     C                   MOVEL     DS7PQRS       KPJBU
700600960126     C                   CALL      'TRTB69R'
700700941003     C                   PARM                    KPJBA
700800940923     C                   MOVEL     *BLANKS       V4IGGA
700900940923     C                   MOVEL     'S'           WRIEM
701000940923     C                   ENDIF
701100940923     C*
701200940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
701300940923     C     V4IGMA        IFEQ      '?'
701400991027     C                   CLEAR                   DS7PQRS
701500941003     C                   MOVEL     'S'           DS7RIC
701600941003     C                   MOVEL     'V'           DS7GES
701700941003     C                   MOVEL     V4CGMA        DS7COD
701800991027     C                   MOVEL     DS7PQRS       KPJBU
701900960126     C                   CALL      'TRTB70R'
702000941003     C                   PARM                    KPJBA
702100940923     C                   MOVEL     *BLANKS       V4IGMA
702200940923     C                   MOVEL     'S'           WRIEM
702300940923     C                   ENDIF
702400940923     C*
702500940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
702600940923     C     V4IGVA        IFEQ      '?'
702700991027     C                   CLEAR                   DS7PQRS
702800941003     C                   MOVEL     'S'           DS7RIC
702900941003     C                   MOVEL     'V'           DS7GES
703000941003     C                   MOVEL     V4CGVA        DS7COD
703100991027     C                   MOVEL     DS7PQRS       KPJBU
703200960126     C                   CALL      'TRTB73R'
703300941003     C                   PARM                    KPJBA
703400940923     C                   MOVEL     *BLANKS       V4IGVA
703500940923     C                   MOVEL     'S'           WRIEM
703600940923     C                   ENDIF
703700940923      *
703800940923     C                   ENDSR
703900940321      **************************************************************************
704000940713      *     R O U T I N E      I N I Z I A L E
704100940713      **************************************************************************
704200940713     C     *INZSR        BEGSR
704300940713      *
704400940713     C     KARB          KLIST
704500940714     C                   KFLD                    V2CAAS
704600940713     C                   KFLD                    COMLNP            3 0
704700940714     C                   KFLD                    V2CNRS
704800940714     C                   KFLD                    V2CNSP
704900940713     C     KARB2         KLIST
705000940713     C                   KFLD                    ARBAAS
705100940713     C                   KFLD                    ARBLNP
705200940713     C                   KFLD                    ARBNRS
705300940713     C                   KFLD                    ARBNSP
705400971223     C     KARB4         KLIST
705500971223     C                   KFLD                    AR4AAS
705600971223     C                   KFLD                    AR4LNP
705700971223     C                   KFLD                    AR4NRS
705800971223     C                   KFLD                    AR4NSP
705900000228     C     KAR4W         KLIST
706000000228     C                   KFLD                    wAR4AAS
706100000228     C                   KFLD                    wAR4LNP
706200000228     C                   KFLD                    wAR4NRS
706300000228     C                   KFLD                    wAR4NSP
706400940713     C     KLNPX         KLIST
706500940713     C                   KFLD                    V1CAAS
706600940713     C                   KFLD                    V1CLP2
706700940713     C                   KFLD                    V1CNRS
706800940713     C                   KFLD                    V1CNSP
706900940713     C     K1ARB         KLIST
707000020910     c                   Kfld                    Wifp
707100940713     C                   KFLD                    NDC               7 0
707200940713     C                   KFLD                    NSS               3 0
707300940713     C                   KFLD                    COMLNA            3 0
707400940713     C                   KFLD                    COMAAD
707500940713     C                   KFLD                    COMMGD
707600940713     C                   KFLD                    V1CLP1
707700940713     C     K2ARB         KLIST
707800020910     c                   Kfld                    Wifp
707900940713     C                   KFLD                    NDC
708000940713     C                   KFLD                    NSS
708100940713     C                   KFLD                    COMLNA
708200940713     C                   KFLD                    COMAAD
708300940713     C                   KFLD                    COMMGD
708400940713     C     K3ARB         KLIST
708500020910     c                   Kfld                    Wifp
708600940713     C                   KFLD                    NDC
708700940713     C                   KFLD                    NSS
708800940713     C                   KFLD                    COMLNA
708900940713     C     K4ARB         KLIST
709000940713     C                   KFLD                    COMLNA
709100940713     C                   KFLD                    COMDTD
709200940713     C                   KFLD                    V1CLP1
709300940713     C     K5ARB         KLIST
709400940713     C                   KFLD                    COMLNA
709500940713     C                   KFLD                    COMDTD
709600940713     C     K6ARB         KLIST
709700940713     C                   KFLD                    V1CLP1
709800940713     C                   KFLD                    COMAAD
709900940713     C                   KFLD                    COMMGD
710000940713     C                   KFLD                    V1CLNA
710100940713     C                   KFLD                    V1CPSD
710200950405     C                   KFLD                    COMSCD
710300950405     C     K6ARB1        KLIST
710400950405     C                   KFLD                    V1CLP1
710500950405     C                   KFLD                    COMAAD
710600950405     C                   KFLD                    COMMGD
710700950405     C                   KFLD                    V1CLNA
710800950405     C                   KFLD                    V1CPSD
710900940713     C     K7ARB         KLIST
711000940713     C                   KFLD                    V1CLP1
711100940713     C                   KFLD                    COMAAD
711200940713     C                   KFLD                    COMMGD
711300940713     C     K8ARB         KLIST
711400940713     C                   KFLD                    V1CLNA
711500940713     C                   KFLD                    COMAAD
711600940713     C                   KFLD                    COMMGD
711700940713     C                   KFLD                    V1CLP1
711800940713     C                   KFLD                    V1CPSD
711900940713     C                   KFLD                    V1CSCD
712000940713     C     K9ARB         KLIST
712100940713     C                   KFLD                    V1CLNA
712200940713     C                   KFLD                    COMAAD
712300940713     C                   KFLD                    COMMGD
712400020627     c     arb7k         klist
712500020627     c                   kfld                    v1cnzd
712600020627     c                   kfld                    v1cprd
712700131111     C     ARB75K        KLIST
712800020627     c                   kfld                    v1cnzd
712900020627     c                   kfld                    v1cprd
713000131111     C                   KFLD                    $AAS
713100131111     C                   KFLD                    $MGS
713200940713     C                   KFLD                    V1CRSD
713300940713     C     AR43K         KLIST
713400020627     c                   kfld                    v1cnzd
713500020627     c                   kfld                    v1cprd
713600940713     C                   KFLD                    $AAS
713700940713     C                   KFLD                    $MGS
713800940713     C     ARB44K        KLIST
713900020627     c                   kfld                    v1cnzd
714000020627     c                   kfld                    v1cprd
714100940713     C                   KFLD                    $AAS
714200940713     C                   KFLD                    $MGS
714300940713     C                   KFLD                    V1CRSM
714400940713     C     AR44K         KLIST
714500020627     c                   kfld                    v1cnzd
714600020627     c                   kfld                    v1cprd
714700940713     C                   KFLD                    $AAS
714800940713     C                   KFLD                    $MGS
714900971223     C     KAR4          KLIST
715000971223     C                   KFLD                    WTRC
715100971223     C                   KFLD                    WN30
715200010508     C     KAR44         KLIST
715300010508     C                   KFLD                    WTRC
715400010508     C                   KFLD                    WN14
715500940713     C     KART          KLIST
715600940713     C                   KFLD                    ARBAAS
715700940713     C                   KFLD                    ARBLNP
715800940713     C                   KFLD                    ARBNRS
715900940713     C                   KFLD                    ARBNSP
716000940713     C                   KFLD                    V1CSCD
716100940713     C*
716200940713     C     KTAB1         KLIST
716300940713     C                   KFLD                    CODUT
716400940713     C                   KFLD                    COD
716500940713     C                   KFLD                    KEY
716600940713     C     KTAB2         KLIST
716700940713     C                   KFLD                    CODUT
716800940713     C                   KFLD                    COD
716900940713     C*
717000940907     C     KLBL1         KLIST
717100940713     C                   KFLD                    LBLAAN
717200940713     C                   KFLD                    LBLLPN
717300940713     C                   KFLD                    LBLNRN
717400940713     C                   KFLD                    LBLNSN
717500940713     C     KLBL2         KLIST
717600940713     C                   KFLD                    LBLAAP
717700940713     C                   KFLD                    LBLLPP
717800940713     C                   KFLD                    LBLNRP
717900940713     C                   KFLD                    LBLNSP
718000940907     C     KLBL3         KLIST
718100940907     C                   KFLD                    KAAS
718200940907     C                   KFLD                    KLNP
718300940907     C                   KFLD                    KNRS
718400940907     C                   KFLD                    KNSP
718500940713     C*
718600940719     C     KAR41         KLIST
718700940719     C                   KFLD                    KAAS
718800940719     C                   KFLD                    KLNP
718900940719     C                   KFLD                    KNRS
719000940719     C                   KFLD                    KNSP
719100940802     C                   KFLD                    KTRC
719200951220     C     KAR42         KLIST
719300951220     C                   KFLD                    KAAS
719400951220     C                   KFLD                    KLNP
719500951220     C                   KFLD                    KNRS
719600951220     C                   KFLD                    KNSP
719700021213     C*
719800021213     C     KAR51         KLIST
719900021213     C                   KFLD                    KAAS
720000021213     C                   KFLD                    KLNP
720100021213     C                   KFLD                    KNRS
720200021213     C                   KFLD                    KNSP
720300021213     C                   KFLD                    KTRd              3
720400940719     C*
720500991026     C     KAR61         KLIST
720600940713     C                   KFLD                    KAAS
720700940713     C                   KFLD                    KLNP
720800940713     C                   KFLD                    KNRS
720900940713     C                   KFLD                    KNSP
721000991026     C                   KFLD                    KTRC
721100991026     C*
721200051128     C     Ksped         KLIST
721300991026     C                   KFLD                    KAAS
721400991026     C                   KFLD                    KLNP
721500991026     C                   KFLD                    KNRS
721600991026     C                   KFLD                    KNSP
721700940713     C*
721800990715     C     KGCA1         KLIST
721900941202     C                   KFLD                    KAAS
722000940713     C                   KFLD                    KLNP
722100940713     C                   KFLD                    KNRS
722200940713     C                   KFLD                    KNSP
722300940713     C*
722400990805     C     KEVB1         KLIST
722500940713     C                   KFLD                    KAAS$
722600940713     C                   KFLD                    KLNP
722700940713     C                   KFLD                    KNRS
722800940713     C                   KFLD                    KNSP
722900131114     C     Kcet          KLIST
723000140224     C                   KFLD                    pctfgs
723100131114     C                   KFLD                    pctNDC
723200131114     C                   KFLD                    arbpdc
723300131114     C                   KFLD                    TTRD              3
723400131114     C                   KFLD                    arbAAS
723500131114     C                   KFLD                    arbLNP
723600131114     C                   KFLD                    arbNRS
723700131114     C                   KFLD                    arbNSP
723800950324     C*
723900020906     C     KFVV1         KLIST
724000950324     C                   KFLD                    WNPG
724100950327     C                   KFLD                    WNDC
724200020906     c                   Kfld                    Wfgs
724300131119     C     KDST          KLIST
724400131119     C                   KFLD                    WNPG
724500131119     C                   KFLD                    kndc6
724600131119     c                   Kfld                    Wfgs
724700021203
724800021203     C     Kfapd         klist
724900021203     C                   kfld                    kpdtip
725000021203     C                   kfld                    kpdpdr
725100131108     c
725200131108     c     kbrv          klist
725300131108     C                   KFLD                    WNPG
725400131108     C                   KFLD                    knfv
725500131108     c                   Kfld                    Wfgs
725600131108     c                   Kfld                    artfls
725700131108     c                   Kfld                    artlna
725800131108     c                   Kfld                    artnrs
725900131108     c                   Kfld                    artnsc
726000131111     C     KarS          KLIST
726100131111     C                   KFLD                    V1CNOT
726200131111     C                   KFLD                    KKTRC             1
726300131111     C                   MOVE      'C'           KKTRC
726400131111     C     KarSXT        KLIST
726500131111     C                   KFLD                    arSFLS
726600131111     C                   KFLD                    arSLNA
726700131111     C                   KFLD                    arSNRS
726800131111     C                   KFLD                    arSNSC
726900131111     C     KartXP        KLIST
727000131111     C                   KFLD                    arTAAS
727100131111     C                   KFLD                    arTLNP
727200131111     C                   KFLD                    arTNRS
727300131111     C                   KFLD                    arTNSP
727400040218      *
727500040218      * File FITGD02L
727600040218     c     K02TGD02      klist
727700040218     c                   kfld                    TGDtor
727800040218     c                   kfld                    TGDogg
727900030527     C*
728000030527     C* Chiave su TNTBE01L - Parziale
728100030527     C     KTBE01P       KLIST
728200030527     C                   KFLD                    TBECOD
728300030527     C                   KFLD                    TBEKE1
728400940713     C*
728500940713     C*-------------------------------------------------------------------------
728600940713     C*
728700991222     C     *LIKE         DEFINE    ARBAAS        WAAS
728800991027     C     *LIKE         DEFINE    ARBLNP        WLNP
728900991027     C     *LIKE         DEFINE    ARBNRS        WNRS
729000991027     C     *LIKE         DEFINE    ARBNSP        WNSP
729100991027     C     *LIKE         DEFINE    FVVNPG        WNPG
729200991027     C     *LIKE         DEFINE    FVVNFV        WNDC
729300131108     C     *LIKE         DEFINE    brvNFV        knfv
729400991027     C     *LIKE         DEFINE    V1CSCD        COMSCD
729500991027     C     *LIKE         DEFINE    COMC08        SAVC08
729600971219     C     *LIKE         DEFINE    ARBAAS        KAAS
729700990805     C     *LIKE         DEFINE    EVBAAS        KAAS$
729800940713     C     *LIKE         DEFINE    ARBLNP        KLNP
729900940713     C     *LIKE         DEFINE    ARBNRS        KNRS
730000940713     C     *LIKE         DEFINE    ARBNSP        KNSP
730100961112     C     *LIKE         DEFINE    ARBFBR        WFBR
730200940713     C     *LIKE         DEFINE    TBLCOD        COD
730300940713     C     *LIKE         DEFINE    TBLKEY        KEY
730400940713     C     *LIKE         DEFINE    ARBAAS        $AAS
730500940713     C     *LIKE         DEFINE    ARBMGS        $MGS
730600940719     C     *LIKE         DEFINE    AR4TRC        KTRC
730700940907     C     *LIKE         DEFINE    LBLAAN        COMAAN
730800971223     C     *LIKE         DEFINE    AR4TRC        WTRC
730900060130     C*-------------------------------------------------------------------------
731000060130     c
731100060130     c                   movel     kpjbu         savkpjbu
731200060130      *
731300060130     c     *dtaara       define    §azute        azuteds
731400060130     c     *dtaara       define    §datiute      ddatiute
731500060130     c                   in(E)     *dtaara
731600060130    1c                   If        %error  or RSUT = *blanks
731700060130     c                   Clear                   Tibs34ds
731800060130     c                   Call      'TIBS34R'
731900060130     c                   Parm                    Tibs34ds
732000060130     c                   In        *dtaara
732100060130    1c                   EndIf
732200060130
732300060130     c                   Clear                   wabi
732400060130     c                   Clear                   v1cmsg
732500060130     c                   Clear                   dLat
732600060130
732700060130      * Verifica errori e autorità profilo
732800060130s   1c                   Select
732900060130      * se ho errori nei dati utente esco dal pgm
733000060130w   1c                   When      DutErr = 'E'
733100060130     c                   GoTo      Fine
733200060130      * se non c'è l'abilitazione
733300060130      * --> se 1° livello, abilitazioni al terminal
733400060130      *     se 2° livello, abilitazioni al punto operativo
733500060130      *     se sede è impossibile ma se capita mando a fine il pgm
733600060130w   1c                   When      UteAut = *Blanks
733700060130if  2c                   If        DutLpo = '1'
733800060130     c                   Eval      wabi   = 'TP'
733900060130e   2c                   EndIf
734000060130if  2c                   If        DutLpo = '2'
734100060130     c                   Eval      wabi   = 'PO'
734200060130e   2c                   EndIf
734300060130if  2c                   If        DutLpo = 'S'
734400060130     c                   GoTo      Fine
734500060130e   2c                   EndIf
734600060130      * carica le abilitazioni del profilo
734700060130x   1c                   Other
734800060130     c                   Movel     UteFaf        Dute01
734900060131     c                   If        §Utembf <> *Blanks
735000060131     c                   Eval      wabi = §Utembf
735100060130     c                   Else
735200060130     c                   Eval      wabi = UteAut
735300060130     c                   EndIf
735400060130e   1c                   EndSl
735500060130
735600060130      * controllo se ok l'abilitazione dell'utente
735700060130     c                   Clear                   Tibs02ds
735800060130     c                   Eval      T02Mod = 'C'
735900060130     c                   Eval      T02Sif = knsif
736000060130     c                   Eval      T02Cod = 'LAT'
736100060130     c                   Movel(p)  wabi          T02Ke1
736200060130     c                   Call      'TIBS02R'
736300060130     c                   Parm                    kpjba
736400060130     c                   Parm                    Tibs02ds
736500060130if  1c                   If        T02Err = *Blanks
736600060130     c                   Eval      dLat = T02Uni
736700060130e   1c                   EndIf
736800060130      * errore o non abilitato
736900060130if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
737000060130     c                   Seton                                        28
737100060130     C                   movel     msg(26)       v1cmsg
737200060130   x1c                   Else
737300060130      * Reperimento dei P.O. gestibili dall'utente
737400060130     c                   clear                   TRUL31DS
737500060130     c                   clear                   TRUL31DS2
737600060130     c                   eval      I31abi = wabi
737700060130     c                   eval      I31cdi = DUTdis
737800060130     c                   eval      I31car = DUTare
737900060130     c                   eval      I31cpo = DUTpou
738000060130     c                   call      'TRUL31R'
738100060130     c                   parm                    KPJBA
738200060130     c                   parm                    TRUL31DS
738300060130     c                   parm                    TRUL31DS2
738400060130if  2c                   if        O31pog > *zeros
738500060130     c                   movea     O31pog        $pog
738600060130     c                   movea     O31arg        $arg
738700060130     c                   movea     O31dig        $dig
738800060130x   2c                   else
738900060130     c                   Seton                                        28
739000060130     c                   movel     MSG(26)       v1cmsg
739100060130e   2c                   endif
739200060130    1c                   endif
739300060130     c
739400060130     c                   movel     savkpjbu      kpjbu
739500971223     C*
739600971223     C* IMPOSTAZIONE CAMPI FISSI
739700940907     C                   MOVEL     *ZERO         COMLPN            3
739800940907     C                   MOVEL     *ZERO         COMNRN            2
739900940907     C                   MOVEL     *ZERO         COMNSN            7
740000950307     C                   MOVEL     *ZERO         COMLAN            3
740100950324     C                   Z-ADD     4             WNPG
740200940713      *
740300940713     C                   Z-ADD     *ZERO         NR2               4 0
740400940713     C                   Z-ADD     *ZERO         NR6               4 0
740500940713     C                   Z-ADD     *ZERO         NR8               4 0
740600940802     C                   MOVEL     *BLANKS       WF03              1
740700070403     c
740800070403     c* Verifico se profilo utente ha GEO attiva
740900131205     c* Per EDP sempre anche prr RAC999
741000131205     c                   if        %subst(knmus:1:3)='EDP' or
741100131205     c                             %subst(knmus:1:6)='RAC999'
741200070410     c                   eval      dutgeot='S'
741300070410     c                   else
741400070403     c     dutpou        chain     azorg01l
741500070403     c                   if        %found(azorg01l)
741600070403     c                   movel     orgde8        og148
741700070403     c                   else
741800070403     c                   clear                   og148
741900070403     c                   endif
742000070410     c
742100070524     c                   if        §oggeot<>' ' or §ogtlls<>' '
742200070524     c                   movel     'S'           dutgeot           1
742300070410     c                   endif
742400070524     c                   endif
742500131007
742600131007      /free
742700131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
742800131007      /end-free
742900140319     c* Tasto funz orari servizi sempre abilitato
743000140319     c                   eval      wabif9='S'
743100131007
743200940713      *
743300940713     C                   ENDSR
743400971223     C**
743500971223     C* ELABORAZIONE DI SINGOLA BOLLA --------------------------------*
743600971223     C     SINGOL        BEGSR
743700971223     C                   CLEAR                   WINIZ
743800971223     C                   CLEAR                   WFINE
743900971223     C*
744000971223     C* 97 ON - MANUTENZIONE BOLLA
744100060130    1C     *IN97         IFEQ      *ON
744200971223     C*
744300060224     c                   movel     arblna        w003a
744400060224     c     w003a         lookup    $pog                                   31
744500060130     c* Se la causale variazione bolle prevede anche il p.o. cliente
744600131111     c*    controllo anche questo
744700060130    2c                   if        not *in31 and d66pocl='S'
744800060130     c     arbcbo        lookup    cbodop                                 31
744900060130    3c                   if        not *in31
745000060130     c                   movel     arbksc        kscfil
745100060130   x3c                   else
745200060130     c* Se bolla doppia prendo il ksc dall'AR6
745300060130     C                   Z-ADD     ARBAAS        KAAS
745400060130     C                   Z-ADD     ARBLNP        KLNP
745500060130     C                   Z-ADD     ARBNRS        KNRS
745600060130     C                   Z-ADD     ARBNSP        KNSP
745700060130     C                   MOVEL     '2'           KTRC
745800060130     C     KAR61         CHAIN     FIAR6000                           31
745900060130if  4C                   IF          %FOUND(fiar601l)
746000060130     c                   movel     ar6ksc        kscfil
746100060130     c                   else
746200060130     c                   movel     999           kscfil
746300060130    4c                   endif
746400060130    3c                   endif
746500060130     c
746600060224     c                   movel     kscfil        w003a
746700060224     c     w003a         lookup    $pog                                   31
746800060130    2c                   endif
746900060130     c
747000060130     C     *IN31         IFEQ      *OFF
747100971223     C                   MOVEL     MSG(19)       V1CMSG
747200971223     C                   SETON                                        459028
747300971223     C                   GOTO      FOR01
747400971223     C                   ELSE
747500971223     C*
747600991027     C                   CLEAR                   FNLR48DS
747700060224     c* come p.o. gestione prendo il p.o. gestore (da £6) della linea
747800060224     c*  di arrivo
747900060224     c                   clear                   fnlv55ds
748000060224     c                   movel     arblna        d55lin
748100060224     c                   movel     '6'           d55tpt
748200060224     C                   time                    wrkoggiiso
748300060224     C     *iso          move      wrkoggiiso    d55drf
748400060224     c                   call      'FNLV55R'
748500060224     c                   parm                    fnlv55ds
748600060224     c                   movel     d55tfa        d48fgs
748700060224     c
748800131203     c                   select
748900140117     C     v1cbarcode    whengt    0
749000140117     C                   MOVEL     ARbAAS        D48AAS
749100140117     C                   MOVEL     ARbLNP        D48LNP
749200140117     C                   MOVEL     ARbNRS        D48NRS
749300140117     C                   MOVEL     ARbNSP        D48NSP
749400131203     C     SEL2          whenne    0
749500980403     C                   MOVEL     V1CAAS        D48AAS
749600980403     C                   MOVEL     V1CLP2        D48LNP
749700980403     C                   MOVEL     V1CNRS        D48NRS
749800980403     C                   MOVEL     V1CNSP        D48NSP
749900131203     C                   other
750000111011     C*                  MOVEL     AR4AAS        D48AAS
750100111011     C*                  MOVEL     AR4LNP        D48LNP
750200111011     C*                  MOVEL     AR4NRS        D48NRS
750300111011     C*                  MOVEL     AR4NSP        D48NSP
750400111011     C                   MOVEL     v2cAAS        D48AAS
750500111011     C                   MOVEL     v2clnp        D48LNP
750600111011     C                   MOVEL     v2cNRS        D48NRS
750700111011     C                   MOVEL     v2cNSP        D48NSP
750800131203     C                   ENDsl
750900141010     c* routine per verifica dispo da esaminare
751000141010     c                   exsr      sr_wdwdc
751100141010
751200971223     C                   MOVEL     V1CCVB        D48CVB
751300971223     C                   MOVEL     ARBCBO        D48CBO
751400971223     C                   MOVEL     'V'           D48FFR
751500971223     C                   MOVEL     'A'           D48TBO
751600971223     C                   MOVEL     'S'           D48F12
751700991027     C                   MOVEL     FNLR48DS      KPJBU
751800971223     C*
751900971223     C                   CALL      'FNLR48R'
752000971223     C                   PARM                    KPJBA
752100971223     C                   PARM                    DSARBD
752200971223     C                   PARM                    DSARBK
752300991011     C                   PARM                    DARBT
752400971223     C                   PARM                    DSARBG
752500030612     C                   PARM                    trul90ds
752600971223     C*
752700991027     C                   MOVEL     KPJBU         FNLR48DS
752800060307     c                   movel     'S'           wriem
752900971223     C* ERRORE
753000971223     C     D48ERR        IFEQ      '1'
753100971223     C                   MOVEL     D48MSG        V1CMSG
753200971223     C                   SETON                                        284790
753300971223     C                   GOTO      FOR01
753400971223     C                   ENDIF
753500971223     C                   ENDIF
753600971223     C*
753700971223   X2C                   ELSE
753800131111   c
753900971223     C* VISUALIZZAZIONE BOLLA
754000131111     c                   select
754100131111     C     v1cbarcode    whengt    0
754200131111     C                   MOVEL     arbAAS        V2CAAS
754300131111     C                   MOVEL     arblnp        COMLNP
754400131111     C                   MOVEL     arbNRS        V2CNRS
754500131111     C                   MOVEL     arbNSP        V2CNSP
754600140117     C     SEL2          whengt    0
754700140117     C                   MOVEL     V1CAAS        V2CAAS
754800140117     C                   MOVEL     V1CLP2        COMLNP
754900140117     C                   MOVEL     V1CNRS        V2CNRS
755000140117     C                   MOVEL     V1CNSP        V2CNSP
755100131111     C                   ENDSL
755200131111     c
755300971223     C                   EXSR      BOLLA
755400971223     C*
755500971223     C                   EXSR      VISUAL
755600971223    2C                   ENDIF
755700971223     C*
755800971223    2C     PA1FLG        IFNE      '1'
755900971223     C     PA1FLG        ANDNE     'N'
756000971223     C     PA1FLG        ANDNE     '3'
756100971223     C     PA1FLG        ANDNE     'P'
756200971223     C     PA1FLG        ANDNE     'E'
756300990129     C     PA1FLG        ANDNE     'C'
756400971223     C                   MOVEL     '1'           WINIZ             1
756500971223     C                   ELSE
756600971223     C                   MOVEL     '1'           WFINE             1
756700971223    2C                   END
756800971223    2C                   ENDSR
756900110601      **************************************************************************
757000110601      *   emissione window email destinatario
757100110601      **************************************************************************
757200110601     C     sr_emdest     BEGSR
757300110601     c                   clear                   v4cemw
757400110601     c
757500110601     c                   eval      ilv79TLA='L'
757600110601     c                   eval      ilv79Aas=arbaas
757700110601     c                   eval      ilv79Lnp=arblnp
757800110601     c                   eval      ilv79nrs=arbnrs
757900110601     c                   eval      ilv79nsp=arbnsp
758000110601     c                   call      'FNLV79R'
758100110601     c                   parm                    fnlv79ds
758200110601     c
758300110601     c                   endsr
758400091214      **************************************************************************
758500131114      *   Emissione del Beneficiario C/Ass
758600091214      **************************************************************************
758700091214     C     sr_bencas     BEGSR
758800091215     c                   clear                   vidben
758900091214     c                   clear                   wdaben
759000091214     c                   clear                   wdasrsc
759100091214     c                   clear                   wdascdi
759200091216     c                   clear                   trulintds
759300091216     c
759400091216     c                   eval      iinttip='B'
759500091216     c                   eval      iinttic=vidtic
759600091216     c                   eval      iintrscm=v4crsm
759700091216     c                   eval      iintrsco=vidrmo
759800091216     c                   eval      iintCDI =w_cdi
759900091216     c                   call      'TRULINTR'
760000091216     c                   parm                    trulintds
760100091216     c
760200091216     c                   movel     ointrsci      wdasrsc
760300091216     c
760400091216     c                   select
760500091216     c                   when      ointtipi='O'
760600091216     c                   eval      wdaben='MITTENTE  ORIGINALE'
760700091216     c
760800091216     c                   when      ointtipi='R'
760900091216     c                   eval      wdascdi=w_cdi
761000091216     c                   eval      wdaben=' O R D I N A N T E'
761100091216     c
761200091216     c                   when      ointtipi='M'
761300091216     c                   if        v3cksc>0
761400091216     c                   movel     v3cksc        wdascdi
761500091216     c                   endif
761600091216     c                   eval      wdaben='  M I T T E N T E'
761700091216     c                   ENDSL
761800091214     c
761900131107     c                   exfmt     LRI6wdas
762000091214     c                   endsr
762100130830      **************************************************************************
762200130830      *   window per visualizzare gli orari servizi
762300130830      **************************************************************************
762400130830     C     sr_OrariSr    BEGSR
762500130920
762600140110     c                   clear                   trulorSds
762700140110     c                   eval       s_IOREtfp = ARBtfp
762800140110     c                   eval       s_IOREtfa = ARBtfa
762900140110     c                   eval       s_IOREdti = ARBdti
763000140110     c                   eval       s_IOREhti = ARBhti
763100140110     c                   eval       s_IOREdcr = ARBdcr
763200140110     c                   eval       s_IOREhcr = ARBhcr
763300140110     c                   eval       s_IOREtcr = ARBtcr
763400140110     c                   eval       s_IOREDEI = s_ARBdei
763500140110     c                   eval       s_IOREOEI = s_ARBoei
763600140110     c                   eval       s_IOREfil = ARBlna
763700140110     c                   eval       s_IOREcap = ARBcad
763800140110     c                   eval       s_IOREloc = ARBlod
763900140110     c                   eval       s_IOREnar = ARBnzd
764000150724     c                   clear                   diore01
764100150728     c                   movel     s_arbttc      §ioretrazC
764200150728     c                   movel     s_arbtcc      §ioreconsC
764300150724     c                   eval       s_IOREflo = diore01
764400150724     c
764500140113     c* Data di Riferimento: data consegna se consegnata
764600140113     c                   select
764700140113     c                   when      arbdcm>0 and arbica<>' ' and arbica<>'R'
764800140113     c                             and arbicc<>' ' and arbicc<>'R'
764900140113     c                   eval       s_ioredta=arbdcm
765000140113     c* per ritorno all'incasso data del giorno
765100140113     c                   when      arbdcm>0 and arbddc>0
765200140113     C                   time                    wrkoggiiso
765300140113     C     *iso          move      wrkoggiiso    s_ioredta
765400140113     c                   other
765500140113     c* altrimenti data prevista consegna se > = oggi
765600140113     C                   time                    wrkoggiiso
765700140113     C     *iso          move      wrkoggiiso    s_ioredta
765800140113     c* se < di oggi, imposto oggi
765900140113     c                   if        amgdce>s_ioredta
766000140113     c                   eval       s_ioredta=amgdce
766100140113     c                   endif
766200140113     c                   endsl
766300140113     c
766400140110     c                   eval       s_IOREtser = 'C'
766500140409     c                   eval       s_IOREDDC=arbddc
766600140409     c                   eval       s_IOREnDC=arbndc
766700140409     c
766800140110     c                   IF         *in19 or
766900140110     c                              (ARBfbc = *blanks and
767000140110     c                               ARBddc > 0 and
767100140110     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
767200140110     c                               arbica='R' or arbicc=' ' or arbicc='R'))
767300140110     c                   eval       s_IOREcons = 'S'
767400140225     c* Orario presunto ritiro/consegna
767500140225     c                   move      arbndc        pctndc
767600140225     c                   move      arbifp        pctfgs
767700140225     c                   move      'COR'         ttrd
767800140225     c     kcet          setgt     fipct02l
767900140225     c     kcet          readpe    fipct02l
768000140310     c                   if        not %eof(fipct02l)
768100140225     c                   movel     pctdati       fipctcords
768200140307     c                   eval      s_ioreorp=§PCTORASTI
768300140225     c                   endif
768400140225     c
768500140110     c                   ENDIF
768600140225     c
768700140110     c                   eval       s_IOREtsp = ARBtsp
768800130920
768900140325     c                   eval      s_iorecol=36
769000140110     c                   call      'TRULORSR'
769100130920     c                   parm                    kpjba
769200140110     c                   parm                    trulorSds
769300130830     c
769400131111     c                   ENDSR
769500131111      **************************************************************************
769600131111      *   Prepara SQL per rsd ind lod rsm
769700131111      **************************************************************************
769800131111     C     Presql        BEGSR
769900131111     //free
770000131114         if v1cxrsd='X'  ;
770100131114
770200131114         wSQL ='SELECT fnarb00f.*  +
770300131114                   from FNARB00F left outer join FIAR400F on arbaas=ar4aas +
770400131114               and arblnp=ar4lnp and arbnrs=ar4nrs and arbnsp=ar4nsp  +
770500131114                and ar4trc=''D'' where arbnzd='''+ v1cnzd +''' '  ;
770600131114                else  ;
770700131114
770800131114         wSQL ='SELECT * from FNARB00F   where arbnzd='''+ v1cnzd +''' '  ;
770900131114         endif  ;
771000131114
771100131114       if v1cnzd=*blanks   ;
771200131114         wSQL +=' and arbprd=''' + v1cprd + ''' '   ;
771300131111       endif  ;
771400131111
771500131115       if (v1cssp=' ' or v1cssp='1')  ;
771600131115        if comdtd>0 and comdta>comdtd  ;
771700131115           wSQL += ' and (arbaas*10000)+arbmgs>=' + %editc(comdtd:'X')+ ' and +
771800131115                        (arbaas*10000)+arbmgs<=' + %editc(comdta:'X')  ;
771900131115        endif  ;
772000131115        if comdtd>0 and (comdta=0 or comdta=comdtd)      ;
772100131115           wSQL += 'and arbaas=' + %editc(comaad:'X')+ ' and arbmgs=' +
772200131115                                %editc(commgd:'X')  ;
772300131115        endif  ;
772400131115       endif  ;
772500131111
772600131111       // se voglio le "IN CONSEGNA", parzializzo
772700131111       if v1cssp='2'  ;
772800131113           wSQL += ' and (arbdcm=0 or arbica='' '' or arbica=''R'' +
772900131113                     or arbicc='' '' or arbicc=''R'')'  ;
773000131111       endif  ;
773100131114
773200131114       // Se voglio le "DA CONSEGNARE", parzializzo
773300131111       if v1cssp='1'  ;
773400131111           wSQL += ' and arbifp=0 and arbndc=0 and arbnss=0' ;
773500131111       endif  ;
773600131111
773700131114       // Se voglio le "CONSEGNATE", parzializzo
773800131111       if v1cssp='3'  ;
773900131111           wSQL += ' and arbdcm>0' ;
774000131111       endif  ;
774100131111
774200131111       // linea di arrivo
774300131111       if v1clna>0  ;
774400131111           wSQL += ' and arblna=' + %editc(v1clna:'X') ;
774500131111       endif  ;
774600131111
774700131111        if v1crsd<>*blanks  ;
774800131111
774900131111         if v1cxrsd =' '   ;
775000131114           wSQL += ' and arbrsd    like(''';
775100131111         else  ;
775200131114           wSQL += ' and concat(arbrsd, ifnull(ar4not, '' '')) like(''%'  ;
775300131111         endif ;
775400131111
775500131209         w060a=v1crsd  ;
775600131209         exsr APICI  ;
775700131209
775800131209           wSQL += w060a ;
775900131111           wSQL  = %trim(wSQL) + '%'')' ;
776000131111         endif ;
776100131111
776200131111        if v1cIND<>*blanks  ;
776300131111
776400131111         if v1cxind =' '   ;
776500131111           wSQL += ' and arbind like(''';
776600131111         else  ;
776700131113           wSQL += ' and arbind like (''%' ;
776800131111         endif ;
776900131111
777000131209         w060a=v1cind  ;
777100131209         exsr APICI  ;
777200131209           wSQL += w060a ;
777300131111           wSQL  = %trim(wSQL) + '%'')' ;
777400131111         endif ;
777500131111
777600131111        if v1cLOD<>*blanks  ;
777700131111
777800131111         if v1cxlod =' '   ;
777900131111           wSQL += ' and arblod like(''';
778000131111         else  ;
778100131113           wSQL += ' and arblod like (''%' ;
778200131111         endif ;
778300131111
778400131209         w060a=v1clod  ;
778500131209         exsr APICI  ;
778600131209           wSQL += w060a ;
778700131111           wSQL  = %trim(wSQL) + '%'')' ;
778800131111         endif ;
778900131111
779000131111        if v1cRSM<>*blanks  ;
779100131111
779200131111         if v1cxrsm =' '   ;
779300131111           wSQL += ' and arbrsm like(''';
779400131111         else  ;
779500131113           wSQL += ' and arbrsm like (''%' ;
779600131111         endif ;
779700131111
779800131209         w060a=v1crsm  ;
779900131209         exsr APICI  ;
780000131209           wSQL += w060a ;
780100131115           wSQL  = %trim(wSQL) + '%'')' ;
780200131111         endif ;
780300131115
780400131115           wSQL += ' order by arbnzd, arbprd, arbaas, arbmgs'  ;
780500131111
780600131111        ENDSR  ;
780700131111       //--------------------------------------------------------------
780800131111       //?Apertura cursore.                                            ?
780900131111       //--------------------------------------------------------------
781000131111       BEGSR  OpenCursor;
781100131111
781200131111         wEnd = *off;
781300131111
781400131111       //?Dichiarazione cursore?
781500131111         exec sql
781600131111         PREPARE S1 from :wSQL;
781700131209
781800131209         IF sqlcode < 0;
781900131209           wEnd = *on;
782000131209           leavesr        ;
782100131209         ENDIF;
782200131209
782300131111         exec sql
782400131111         DECLARE ARB  cursor for S1;
782500131111
782600131111       //?Apertura del cursore
782700131111         exec sql
782800131111         OPEN ARB;
782900131111
783000131111         IF sqlcode < 0;
783100131111           wEnd = *on;
783200131209           leavesr        ;
783300131111         ENDIF;
783400131209
783500131209           WopenC='1'     ;
783600131209
783700131111
783800131111       ENDSR;
783900131209       //--------------------------------------------------------------
784000131209       //?Raddoppio apici nella stringa                                ?
784100131209       //--------------------------------------------------------------
784200131209       BEGSR  Apici     ;
784300131209       ap=1  ;
784400131209       dow ap <=60  ;
784500131209       if %subst(w060a:ap:1)=''''  ;
784600131209       w060a=%replace('''''':w060a:ap:1)  ;
784700131209       ap= ap   +1       ;
784800131209       endif  ;
784900131209
785000131209       ap= ap   +1       ;
785100131209       enddo  ;
785200131209
785300131209       ENDSR;
785400131111       //--------------------------------------------------------------
785500131111       //?Chiusura cursore.
785600131111       //--------------------------------------------------------------
785700131111       BEGSR CloseCursor;
785800131111
785900131111       //?Chiusura cursore SQL?
786000131111         exec sql
786100131111           close ARB;
786200131209           clear WopenC   ;
786300131209           Wend=*off      ;
786400131111
786500131111       ENDSR;
786600131111       //--------------------------------------------------------------
786700131111       //?Lettura cursore.                                             ?
786800131111       //--------------------------------------------------------------
786900131111       BEGSR  ReadCursor;
787000131111
787100131111         clear  FNARBds;
787200131113         *in30=*off    ;
787300131113         *in32=*off    ;
787400131111
787500131111         exec sql
787600131111         FETCH next from ARB into :FNARBds;
787700131111
787800131111         IF sqlcod = 100 or sqlcod < 0;
787900131111           wEnd = *on;
788000131111         ENDIF;
788100131111
788200131111       ENDSR;
788300141010       //--------------------------------------------------------------
788400141010       //?Window per f19=Dispo di consegna da esaminare
788500141010       //--------------------------------------------------------------
788600141010       BEGSR sr_wdwdc   ;
788700141010          $fine=*off;
788800141105          clear fnlry09ds2;
788900141010          ilry09tch='C';
789000141010          ILRY09AAS=d48aas ;
789100141010          ILRY09LNP=d48LNP;
789200141010          ILRY09NRS=d48NRS ;
789300141010          ILRY09NSP=d48NSP ;
789400141105          kpjbu=fnlry09ds2;
789500141105          fnlry09r(kpjba);
789600141105          fnlry09ds2=kpjbu;
789700141010          if olry09esit='1' ;
789800141010             dow $fine=*off;
789900141010                exfmt lri6wddc;
790000141010                   if *inkt;
790100141105                       clear fnlry09ds2;
790200141010                       ilry09tch='E';
790300141010                       ILRY09AAS=d48aas ;
790400141010                       ILRY09LNP=d48LNP;
790500141010                       ILRY09NRS=d48NRS ;
790600141010                       ILRY09NSP=d48NSP ;
790700141105                       ILRY09DF06='S';
790800141105                       kpjbu=fnlry09ds2;
790900141105                       fnlry09r(kpjba);
791000141010                       $fine=*on;
791100141010                   endif;
791200141010             enddo;
791300141010          endif ;
791400141010
791500141010       ENDSR;
791600131111
791700131111
791800131111
791900131111      /end-free
792000131111
792100940713      **************************************************************************
792200941014**        12345678901234567890123456789012345678901234567890
792300950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
792400930413**
792500950316LEGATA A PIU' BOLLE                       - F18=Visualizza
792600920326**
792700950316 Non Presente
792800950316 F18=Visualizza
792900940907Vedi in Int.Par
793000940907Vedi in Int.Arr
793100941011Vedi Arch.Stor.
793200940907**
793300940907Spedizione non Presente
793400941230
793500940907Vedi in Int.Bolle Part.
793600940907Vedi in Int.Bolle Arr.
793700941011Vedi in Archiv. Storico
793800940713**  MESSAGGI DI ERRORE VIDEO  **************************************************
793900940713Nessuna bolla da visualizzare                                                  1
794000940713Immettere linea di arrivo valida                                               2
794100131114Effettuare ulteriori parzializzazioni                                          3
794200940713Immettere linea di arrivo o linea di partenza                                  4
794300940713Immettere linea di partenza valida                                             5
794400940713Numero spedizione inesistente                                                  6
794500940713Immettere anno spedizione                                                      7
794600940713Immettere data formalmente valida                                              8
794700040216Immettere tipo bolla valido                                                    9
794800940713Immettere data di spedizione iniziale minore o uguale a quella finale         10
794900940713Immettere numero di serie iniziale minore o uguale a quello finale            11
795000940713Immettere numero segnacollo iniziale minore o uguale a quello finale          12
795100940713Immettere numero segnacollo finale, se immesso quello iniziale                13
795200131114Scelta incongruente tra stato spedizioni  e richiesta                         14
795300940713Fine scorrimento                                                              15
795400940715Immettere tipo servizio bolle valido                                          16
795500940923Immettere filiale segnacollo valida                                           17
795600060614Riferimento partner estero errato x bolle DPD:o lungo 11 numerico a 14 alfanum
795700941212Bolla non manutenzionabile: in gestione ad altra filiale                      19
795800991227Il riferimento partner estero per bolle DPD deve avere massimo 12 numeri
795900010508Immettere p.o. di emissione valido                                            21
796000010510Non trovate bolle per l' O.R.M. richiesto                                     22
796100020626Immettere la nazione e la provincia del destinatario                          23
796200020627Nazione destinatario errata                                                   24
796300020627Provincia destinatario errata                                                 25
796400060130Utente non abilitato alla Variazione bolle                                    26
796500060130Utente non abilitato alla variazione bolle del p.o. in gestione               27
796600060613ATTENZIONE!Saranno selez.bolle con Nazione e RagSoc impostate ma PROVINC.vuota28
796700070420Stampa LDV effettuata ...                                                     29
796800070420Stampa LDV al momento non effettuabile: ritentare o telefonare in Sede        30
796900080201Assegnato non tassato: stampa LdV non effettuabile                            31
797000131111non trovata bolla per il barcde digitato                                      32
797100030612**  cmA4
797200030612OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
797300111221 SHARE(*YES) USRDTA('Man.BolA4')
797400030612**  cmA5
797500030612OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
797600111221 SHARE(*YES) USRDTA('Man.BolA5')
797700070426**  cmA4f6
797800111221OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
797900111221USRDTA('Rist.Bolle') SECURE(*YES)
798000070426**  cmA5f6
798100070426OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
798200111221             USRDTA('Rist.Bolle') SECURE(*YES)
798300030612** cnm
798400991222 0123456789
798500040218** TF                                                                        *
798600070420F1=Agg.Rich#                                                                    1
798700070411** TF01B                                                                     *
798800070411F1=SCELTA BOLLA#                                                                1
798900021216** TF                                                                        *
799000021218F2=C.A.#                                                                        2
799100021216** TF                                                                        *
799200021216F4=Tass2#                                                                       4
799300021216** TF                                                                        *
799400021216F5=Eventi#                                                                      5
799500070420** TF                                                                        *
799600070420F6=StampaLDV#                                                                   5
799700021216** TF                                                                        *
799800021217F7=Anomalie#                                                                    7
799900021216** TF                                                                        *
800000021219F8=Discord#                                                                     8
800100130830** TF                                                                        *
800200130830F9=OrariSr#                                                                     9
800300021216** TF                                                                        *
800400070420F10=Variaz#                                                                     10
800500021216** TF                                                                        *
800600030121F11=NoteS/B#                                                                    11
800700021217** TF                                                                        *
800800021217F12=Ritorno#                                                                    11
800900021216** TF                                                                        *
801000070420F13=Tassaz#                                                                     13
801100021217** TF                                                                        *
801200021216F14=Giacenza#                                                                   14
801300040218** TF                                                                        *
801400070420F16=Gest.Rich#                                                                  16
801500070404** TF                                                                        *
801600140311F17=NoteAut#                                                                    16
801700021216** TF                                                                        *
801800021216F19=Spunte#                                                                     19
801900021216** TF                                                                        *
802000070420F20=Affid#                                                                      19
802100021216** TF                                                                        *
802200070420F21=DetColli#                                                                   19
802300110928** TF                                                                        *
802400110930F22=PerfectOrder#                                                               19
