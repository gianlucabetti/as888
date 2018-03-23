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
004300170315     FFiarS03L  IF   E           K DISK
004400940712     F*.......... L E G A M E   B O L L A
004500940713     FFNLBL01L  IF   E           K DISK
004600991027     FFNLBL02L  IF   E           K DISK    RENAME(FNLBL000:FNLBL002)
004700940712     F*.......... B O L L E   P A R T E N Z E
004800160115     FFNBLP01L  IF   E           K DISK    infds(fnblpinf)
004900160115     FFNBLP00F  IF   E             DISK
005000160115     f                                     rename(fnblp000:fnblpfis)
005100160115     f                                     prefix(ARB:3)
005200940712     F*.......... V A R I A Z I O N I   B O L L E
005300131007     FFiARBF2C  IF   E           K DISK
005400990805     FFNEVB01L  IF   E           K DISK
005500950324     F*
005600020906     fFnfvv01l  if   e           k Disk
005700131119     ffidst01l  if   e           k Disk
005800990119     FFNDFA01L  IF   E           K DISK
005900010508     F*.......... O. R. M.
006000060216     FFiAR404L  if   e           k DISK    RENAME(FiAR4000:FiAR4004)
006100040218      *.......... T E L E F O N A T E   /   R E C L A M I
006200160404     fFITGD32L  if   e           k disk
006300030527     F*.......... N U O V E   T A B E L L E
006400030527     FTNTBE01L  IF   E           K DISK
006500131108     F*.......... S P U N T E
006600131108     Ffnbrv01l  IF   E           K DISK
006700131114     F
006800131114     Ffipct02l  IF   E           K DISK
006900040218      *------------------------------------------------------------------------*
007000060130     D $pog            s              3    dim(250)
007100060130     D $DIG            s              1    dim(20)
007200060130     D $ARG            s              3    dim(50)
007300060130     d
007400950307     D LBL             S              1    DIM(58) CTDATA PERRCD(58)
007500950307     D LB2             S             58    DIM(1) CTDATA PERRCD(1)
007600941011     D LBD             S             15    DIM(5) CTDATA PERRCD(1)
007700941011     D PRD             S             23    DIM(5) CTDATA PERRCD(1)
007800131111     D MSG             S             78    DIM(32) CTDATA PERRCD(1)
007900030612      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
008000030612     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
008100030612     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
008200070426     D CMA4f6          S              1    DIM(130) CTDATA PERRCD(65)
008300070426     D CMA5f6          S              1    DIM(130) CTDATA PERRCD(65)
008400030612      *
008500991222     D CNM             S              1    DIM(11) CTDATA PERRCD(11)
008600040218     D TF01            s              1    dim(20) ctdata perrcd(20)
008700070411     D TF01B           s              1    dim(20) ctdata perrcd(20)
008800021216     D TF02            S              1    DIM(20) CTDATA PERRCD(20)
008900021216     D TF04            S              1    DIM(20) CTDATA PERRCD(20)
009000021216     D TF05            S              1    DIM(20) CTDATA PERRCD(20)
009100070420     D TF06            S              1    DIM(20) CTDATA PERRCD(20)
009200021216     D TF07            S              1    DIM(20) CTDATA PERRCD(20)
009300021216     D TF08            S              1    DIM(20) CTDATA PERRCD(20)
009400130830     D TF09            S              1    DIM(20) CTDATA PERRCD(20)
009500021216     D TF10            S              1    DIM(20) CTDATA PERRCD(20)
009600021216     D TF11            S              1    DIM(20) CTDATA PERRCD(20)
009700021217     D TF12            S              1    DIM(20) CTDATA PERRCD(20)
009800021216     D TF13            S              1    DIM(20) CTDATA PERRCD(20)
009900021216     D TF14            S              1    DIM(20) CTDATA PERRCD(20)
010000040218     d TF16            s              1    dim(20) ctdata perrcd(20)
010100070404     d TF17            s              1    dim(20) ctdata perrcd(20)
010200021216     D TF19            S              1    DIM(20) CTDATA PERRCD(20)
010300021216     D TF20            S              1    DIM(20) CTDATA PERRCD(20)
010400021216     D TF21            S              1    DIM(20) CTDATA PERRCD(20)
010500110928     D TF22            S              1    DIM(20) CTDATA PERRCD(20)
010600021217     D $TF             S              1    DIM(20)
010700911024     D*
010800131111     D C1              S              1    DIM(30)
010900941212     D L1              S              3  0 DIM(30)
011000060613     D k11             S              1    DIM(11)
011100920403     D CVB             S              1    DIM(23)
011200921102     D SEG             S              1    DIM(15)
011300930705     D KCA             S              1    DIM(50)
011400940912     D DCA             S             44    DIM(50)
011500940907     D*
011600940907     D AAN             S              4  0 DIM(30)
011700940907     D LPN             S              3  0 DIM(30)
011800940907     D NRN             S              2  0 DIM(30)
011900940907     D NSN             S              7  0 DIM(30)
012000941024     D CC1             S              1    DIM(99)
012100941024     D CC2             S             20    DIM(99)
012200020329     D CC3             S              1    DIM(99)
012300020329     D CB1             S              1    DIM(99)
012400020329     D CB2             S             20    DIM(99)
012500030121     D RigaTf1         S              1    DIM(62)
012600030121     D RigaTf2         S              1    DIM(62)
012700030121     D RigaTf3         S              1    DIM(62)
012800030527     D*
012900060130     D cbodop          S              2    DIM(25)
013000040216     D* PASSAGGIO A INTERROGAZIONE VARIAZIONI           - FNLR75R -
013100940720     D* PASSAGGIO A INTERROGAZIONE DETTAGLIO COLLI      - FNLR37R -
013200940712     D PARAM1          DS
013300940714     D  V2CAAS                14     17  0
013400940712     D  COMLNP                18     20  0
013500940714     D  V2CNRS                21     22  0
013600940714     D  V2CNSP                23     29  0
013700940712     D  PA1F03                30     30
013800940713     D  PA1FLG                31     31
013900940713     D  PA1PGM                32     41
014000940714     D  PA1DSA                42     61
014100940809     D  PA1CML                62     62
014200940720     D  PA1DSP                63     70  0
014300940720     D  PA1RMN                71     85  0
014400940720     D  PA1LNA                86     88  0
014500940802     D  PA1LN1                89    108
014600940802     D  PA1RSD               109    143
014700941213     D* SE RICH DA PGM GIAC. = 1
014800941213     D  PA1GIA               144    144
014900941227     D  PA1FEL               145    147  0
015000950628     D  PA1TBO               148    148
015100110929     d  pa1rma               149    163
015200110929     d  pa1xco               164    164
015300980603     D* PASSAGGIO DATI PER CAMBIO AZIENDA
015400980609     D PARAAZ          DS
015500980603     D  CALPGM               201    210
015600060217     D**PARAZI               211    211
015700060217     D**PARF19               212    212
015800980603     D  PARF3                213    213
015900940802     D* RICHIAMO PGM INTERROGAZ.BOLLE PARTENZE            - FNLS04R -
016000940721     D* RICHIAMO PGM INTERROGAZ.BOLLE TRANSITO            - FNLR68R -
016100920203     D PARAM2          DS
016200940722     D  PA2FLG                31     31
016300940722     D  PA2BOL                62     62
016400940802     D* RICHIAMO PGM INTERROGAZ.BOLLE PART.CON KEY SPECIF - FNLS05R -
016500941214     D PARAM0          DS
016600950316     D  PA0AAS                 1      4  0
016700950316     D  PA0LNP                 5      7  0
016800950316     D  PA0NRS                 8      9  0
016900950316     D  PA0NSP                10     16  0
017000941214     D  PA0F03                17     17
017100941214     D  PA0FLG                18     18
017200941214     D  PA0A9                 19     19
017300941214     D  PA0B5                 20     20
017400941214     D  PA075                 21     21
017500941214     D  PA098                 22     22
017600941214     D  PA0RSU                23     42
017700941214     D  PA0CML                43     43
017800941214     D  PA0SIM                44     46  0
017900941214      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
018000941214     D  PA0CLI                47     47
018100941214      * PA0GIA indica se il pgm e' richiamato dalle giacenze
018200941214     D  PA0GIA                48     48
018300940802     D* RICHIAMO PGM DI INTERROGAZIONE SPUNTE FOGLI VARI  - FNLR04R -
018400941230     D PARA04          DS                  INZ
018500940713     D  PA4NFV                 1      5  0
018600940713     D  PA4LNP                 6      8  0
018700940713     D  PA4LNA                 9     11  0
018800940713     D  PA4NRS                12     13  0
018900940713     D  PA4NCD                14     20  0
019000940713     D  PA4NCA                21     27  0
019100940713     D  PA4NPS                28     29  0
019200940713     D  PA4NPG                30     30  0
019300940713     D  PA4FLG                31     31
019400940713     D  PA4PGM                32     38
019500941230     D  PA4FGS                39     41  0
019600101201     d* bolla
019700101201     d  pa4bolp               42     44  0
019800101201     d  pa4bors               45     46  0
019900101201     d  pa4bosp               47     53  0
020000101201     d  pa4boan               54     57  0
020100101201     D  PA4ERR                58     58
020200990119     D                 DS
020300990119     D  DCTDCA                 1      8  0
020400990119     D  DCTAAC                 1      4  0
020500990119     D  DCTMGC                 5      8  0
020600060518     D**               DS
020700060518     D**§11                    1     11  0 dim(11)
020800060518     D**dpdchd                12     12  0
020900060518     D**dpdbrc                 1     12  0
021000940726     D* RICHIAMO PGM DI INTERROGAZIONE EVENTI             - FNLRB5R -
021100950215     D PARAB5          DS                  INZ
021200940726     D  PB5AAS                14     17  0
021300940726     D  PB5LNP                18     20  0
021400940726     D  PB5NRS                21     22  0
021500940726     D  PB5NSP                23     29  0
021600940726     D  PB5F03                30     30
021700940726     D  PB5FLG                31     31
021800940726     D  PB5PGM                32     41
021900940726     D  PB5DSA                42     61
022000940726     D  PB5DSP                63     70  0
022100940726     D  PB5RMN                71     85  0
022200940726     D  PB5LNA                86     88  0
022300940802     D  PB5LN1                89    108
022400940802     D  PB5RSD               109    143
022500940802     D  PB5DCP               144    151  0
022600940802     D  PB5TMC               152    152
022700940712     D                 DS
022800940712     D  MM                     1      2  0
022900940712     D  GG                     3      4  0
023000940712     D  MMGG                   1      4  0
023100901116     D                 DS
023200940713     D  V1CLNA                 1      3  0
023300940713     D  V1CLP1                 4      6  0
023400940713     D  V1CDTD                 7     14  0
023500940713     D  V1CDTA                15     22  0
023600940712     D  SEL1                   1     22  0
023700901116     D                 DS
023800940713     D  V1CLP2                 1      3  0
023900940713     D  V1CNRS                 4      5  0
024000940713     D  V1CNSP                 6     12  0
024100901116     D  SEL2                   1     12  0
024200940712     D                 DS
024300131111     D  V1CRSD                 1     30
024400131111     D  V1CRSM                31     60
024500131111     d  v1cind                61     90
024600131111     d  v1clod                91    120
024700131111     d  v1cnzd               121    123
024800131111     d  v1cprd               124    125
024900131111     D  SEL3                   1    125
025000010508     D                 DS
025100010508     D  V1Cpoe                 1      3  0
025200010508     D  V1CNsr                 4      5  0
025300010508     D  V1CNor                 6     12  0
025400010508     D  V1CNrv                13     14  0
025500010508     D  SEL4                   1     14  0
025600990119     D                 DS
025700990119     D  V2CTSP                 1      1
025800990119     D  V2CGCA                 2      2
025900990119     D  V2CDAN                 3      3
026000990119     D  V2CFLA                 1      3
026100990119     D                 DS
026200990119     D  V2CCON                 1      6
026300990119     D  V2CFCC                 7      7
026400990119     D  V2CCOB                 1      7
026500131111     D**               DS
026600131111     D*+ARBAAS                 1      4  0
026700131111     D*+ARBMGS                 5      8  0
026800131111     D**ARBDSP                 1      8  0
026900131111     D
027000131111     D                 DS
027100131111     D  v1cbarcode             1     15  0
027200131111     D  b_aas                  1      2  0
027300131111     D  b_lnp                  3      5  0
027400131111     D  b_nrs                  6      7  0
027500131111     D  b_nsp                  8     14  0
027600950428     D                 DS
027700950428     D  V3CLNP                 1      3  0
027800950428     D  V3CNRS                 4      5  0
027900950428     D  V3CNSP                 6     12  0
028000950428     D  V3CDSP                13     20  0
028100950428     D  VIDRMN                21     35  0
028200131121     D  V3CLNA                36     38  0
028300950428     D  D15LNA                39     53
028400950428     D  V32RSD                54     85
028500950428     D  COMC08                 1     85
028600131111     d
028700930705     D WLBDAT          DS
028800940712     D  G02DAT                 1      8  0
028900940712     D  G02INV                 9     16  0
029000940712     D  G02ERR                17     17
029100940712     D  G02TGI                18     22  0
029200900517     D CA02DS          DS
029300900517     D  PRIMA                378    379B 0
029400950125     D                 DS
029500950125     D* CAMPO UNICO COMPRENDENTE RAG.SOC.+AGGIUNTIVI 35 CARATTERI
029600950125     D  V4CRSD                 1     35
029700131120     D  V4CNOT                36     70
029800131120     D  V4CNOM                 1     70
029900040218      *?definizione oggetto reclamo come spedizione?
030000131121     d ds_OggSped      ds                  inz
030100131121     d  wARBlnp                       3s 0 inz
030200131121     d  wARBnrs                       2s 0 inz
030300131121     d  wARBnsp                       7s 0 inz
030400131121     d  wARBaas                       4s 0 inz
030500040218      *
030600040218     d Status         sds
030700040218     d  SDSprm           *parms
030800991027      *
030900040216     D KPJBA         E DS
031000040218     d FIDNA1ds      e ds
031100040225      *
031200040225     d FIDNA2ds      e ds                  inz
031300040225     d  IA2mod       e                     inz('M')
031400040225     d  IA2tor       e                     inz('S')
031500040216      *
031600991029     d YEURCODS      E DS
031700991027     d TIBS69DS      E DS                  INZ
031800021213     d FNLRB3Ds      E DS
031900991027     d DS_cnaco      E DS                  extname(CNACO00F)
032000991027     d DS_cnind      E DS                  extname(CNIND00F)
032100991027     d DS_cnclp      E DS                  extname(CNCLP00F)
032200991027     d DS_fncls      E DS                  extname(FNCLS00F)
032300991027     D DSCV          E DS
032400020402     D DSCC          E DS
032500991027     D DS$2          E DS
032600991027     D DS3A          E DS
032700940909     D DS7O          E DS
032800951220     D DS7K          E DS
032900131108     D DS2A          E DS
033000020627
033100091214     d ds15          e ds
033200020627     d dspr          e ds
033300090604     d ds5e          e ds
033400020627
033500991217     D OG143         E DS
033600070403     D OG148         E DS
033700000615     D DSBL4A        E DS
033800060518     D DSBL4I        E DS
033900991217     D DSBL4E        E DS
034000971219     D DSARBD        E DS                  INZ
034100941014     D DSARBK        E DS                  INZ
034200941014     D DSARBG        E DS                  INZ
034300991011     D DARBT         E DS                  INZ
034400150724     D Diore01       E DS                  INZ
034500150724     d
034600991027     D FIDN00DS      E DS
034700091216     D TRULINTDS     E DS
034800991027     D DS7PQRS       E DS
034900991027     D FNLG30DS      E DS
035000991027     D FNLr44DS      E DS
035100130920
035200131114     d fipctcetds    e ds
035300140224     d fipctcords    e ds
035400140110     D TRULORSDS     E DS                  prefix(S_)
035500131114     D TRULORFDS     E DS
035600130920
035700961121     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
035800991027     D FNLR45DS      E DS
035900950102     D FNLR97        E DS
036000930428     D*
036100930428     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
036200991027     D TNSD99DS      E DS                  INZ
036300941014      *
036400030612     D* DS PER TRUL90R - RICHIESTA STAMPANTI
036500030612     D TRUL90DS      E DS
036600941014      *
036700941212     D* DS PER TRUL06R - CARICAMENTO £X
036800991027     D TRUL06DS      E DS
036900991027     D  LIN                    1     90  0 DIM(30)
037000941212      *
037100941014     D* DS PER FNLR66R - CONTROLLO E VISUALIZZAZIONE CAUSALI VARIAZIONI
037200991027     D FNLR66DS      E DS
037300060224      *
037400060224     D* DS PER FNLv55R - richiamo pgm che cerca i terminal per £6
037500060224     D fnlv55ds      E DS
037600941014      *
037700941014     D* DS PER FNLR48R - MANUTENZIONE BOLLE ARRIVI
037800991027     D FNLR48DS      E DS
037900941014      *
038000941212     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
038100991027     D FNLV50DS      E DS
038200051124     D* DS PER FNLV80R - decodifica cons richiesta
038300051124     D FNLV80DS      E DS
038400131120     D* DS PER FNLRG7R - visulizza altre abilitazioni
038500131120     D fnlrg7ds      E DS
038600070404     D* DS PER FNLRN5R - visulizza note LDV
038700140123     D**** fnlrn5ds      E DS
038800140123     D* DS PER FNLRN6R - visulizza note LDV
038900140123     D fnlrn6ds      E DS
039000070427     D* DS PER FIDG34R - reperisce dati consegna automatica
039100070427     D fidg34ds      E DS
039200070420     D* DS PER           stampa LDV
039300070420     D FNLSB5DS      E DS
039400130308     d FNLSB6ds1     e ds                  inz
039500130319     d  DB6pdf       e                     inz('X')
039600941212      *
039700131119     D DDSTFLR       E DS                  INZ
039800010510     D Dsbl4m        E DS
039900030121     d dTba          e ds
040000031119     d dAr5Gen       e ds
040100030121     d dAr5Ban       e ds
040200030703     d dAr5Bnb       e ds
040300130927     d dAr5emd       e ds
040400030121     d Tibs02Ds      e ds
040500060130     d Azuteds       e ds                  extname(Azute00f)
040600060130     d dDatiute      e ds
040700060130     d dLat          e ds
040800060130     d dute01        e ds
040900060130     D TRUL31DS      E DS
041000060130     D TRUL31DS2     E DS
041100060130     D TIBS34DS      E DS
041200110601     D fnlv79ds      E DS
041300141010      // -?Reperimento Disposizioni di Consegna da WEB
041400141105     d FNLRY09DS2    e ds                  inz
041500040809      * ds per il controllo della presenza di CA per la spedizione richiesta
041600040809     d FIDN12DS      e ds
041700040809     d  skKey                 26    305    dim(20)
041800040809     d  skAnn                306    325    dim(20)
041900040809     d  skDca                326    485  0 dim(20)
042000040809     d  skFca                486    545  0 dim(20)
042100040809     d  skDch                546    705  0 dim(20)
042200040809     d  skCch                706    745    dim(20)
042300040809     d*
042400040809     d dsKey           ds
042500040809     d  dsaac                         4  0
042600040809     d  dsfil                         3  0
042700040809     d  dsnca                         7  0
042800160115
042900160115     D fnblpinf        ds
043000160115     D  blpnrr               397    400i 0
043100131111
043200131111       // -?File bolle arrivi
043300131111     d FNARBds       e ds                  extname(FNARB00F)
043400131111     d
043500131111     d wEnd            s               n   inz(*off)
043600040809
043700991027      *
043800991027     D W017A           s             17
043900991027     D w0130           s             13  0
044000991027     D w0133           s             13  3
044100991027     D IMPeditato      s             13
044200060130     D savkpjbu        S                   LIKE(kpjbu)
044300060130     D kscfil          S                   LIKE(arblna)
044400060130     D War4aas         S                   LIKE(AR4aas)
044500000228     D War4lnp         S                   LIKE(AR4lnp)
044600000228     D War4nrs         S                   LIKE(AR4nrs)
044700000228     D War4nsp         S                   LIKE(AR4nsp)
044800991027     D Wsvn            S                   LIKE(AR7svn)
044900991027     D Wvan            S                   LIKE(AR7van)
045000991027     D Wdiv            S                   LIKE(AR6div)
045100991029     D Wariimp         S                   LIKE(ARIimp)
045200991029     D Wariipp         S                   LIKE(ARIipp)
045300020906     d wfgs            s                   Like(Fvvfgs)
045400991027     D DATA_eur        S               D   DATFMT(*eur)
045500021203     d kpdpdr          S                   LIKE(APDpdr)
045600021203     d kpdtip          S                   LIKE(APDtip)
045700131120     d kndc6           S                   LIKE(dstnfv)
045800140110     d s_arbdei        S                   LIKE(d98dei)
045900140110     d s_arboei        S                   LIKE(d98oei)
046000150728     d s_arbttc        S                   LIKE(d98ttc)
046100150728     d s_arbtcc        S                   LIKE(d98tcc)
046200151019     d s_arbflp        S                   LIKE(d98flp)
046300131114     D arbrsduni       S             70
046400131114     D arbdsp          S              8  0
046500131111     D W0080           S              8  0
046600991027     D W0040           S              4  0
046700991027     D wtibs69r        s              1
046800991027     D wtstdec         s              1
046900131120     D Wvideo          s              1
047000131209     D WopenC          s              1
047100020225     d lnantw          s                   like(§ogntw)
047200020225     d lnpntw          s                   like(§ogntw)
047300020910     d Wifp            s                   Like(Arbifp) Inz
047400060130     d wabi            s                   like(UteAut)
047500030703
047600030703     d wtba            s                   Like(§Ar5Tba)
047700030703     d wdestba         s                   Like(VidTba)
047800061018     d savnzd          s                   inz  like(v1cnzd)
047900020628
048000091216     D W_cdi           S                   LIKE(§ar5cdi)
048100020628     D ordina          S              1
048200020628     D comando         S              1
048300070420     D wrichtrul90     S              1
048400070403     d Dataeur         s               d   datfmt(*eur)
048500070403     d Datadmy         s               d   datfmt(*dmy)
048600110929     d s_param1        s                   like(param1)
048700110929     d no_f22          s              1    inz(' ')
048800110930     d comrma          s                   like(arbrma)
048900131108     d Wcol_D          s              5  0
049000131108     d Wcol_I          s              5  0
049100131108     d Wcarat          s              3  0
049200131108     d Indx            s              3  0
049300131209     d AP              s              3  0
049400131108     d wx              s              3  0
049500131007     d wy              s              3  0
049600131209     d w060a           s             60
049700131209     d wabif9          s              1
049800141010     d $Fine           s              1    inz(*off)
049900141010       // -?Reperimento Disposizioni di consegna da WEB
050000141010     d fnlry09r        pr                  extpgm('FNLRY09R')
050100141013     d  kpjba                              likeds(kpjba)
050200131111     d
050300131111       // -?Stringa SQL da eseguire
050400131111     d wSQL            s           2048    Varying        inz
050500030527     D*
050600030527     D wrkoggiiso      s               d
050700160115     D wchainblp       s              1
050800991027      *
050900930705     D* DEFINIZIONI COSTANTI
051000930705     D*
051100930705     D CNOSEQ          C                   CONST('NON SEQUENZIALI')
051200941014     D CINTER          C                   CONST('* INTERROGAZIONE BOL-
051300941014     D                                     LE ARRIVI *')
051400941014     D CMANUT          C                   CONST('** MANUTENZIONE BOLL-
051500941014     D                                     E ARRIVI **')
051600021216     D CF2413          C                   CONST('F24=AlTasti(1/3)')
051700021216     D CF2423          C                   CONST('F24=AlTasti(2/3)')
051800021216     D CF2433          C                   CONST('F24=AlTasti(3/3)')
051900021216     D CF2412          C                   CONST('F24=AlTasti(1/2)')
052000021216     D CF2422          C                   CONST('F24=AlTasti(2/2)')
052100110705     d
052200110705     D* CAMPO PER DSPATR "NO DISPLAY"
052300110705     D NODI            C                   CONST(X'27')
052400131120     D* CAMPO PER DSPATR "RI"
052500131120     D RI              C                   CONST(X'21')
052600131127     D* CAMPO PER DSPATR "HI"
052700131127     D hi              C                   CONST(X'22')
052800900518     C****************************************************************
052900900518     C*  RIEPILOGO INDICATORI
053000900518     C***************************************************************
053100911023     C* 01    - SELEZIONE PER LINEA ARRIVO
053200911023     C* 02    - SELEZIONE PER LINEA PARTENZA
053300911105     C* 03    - SPEDIZIONE GIACENTE
053400990119     C* 04    - ABILITO TASTO FUNZIONALE F2 - INT C.A.
053500990119     C* 05    - ABILITO TASTO FUNZIONALE F19- SPUNTE
053600070411     c* 06    - on per tassazione 2 bolla emette scritta "ImportoBOLLO"
053700070411     c*       - offper tassazione 2 bolla emette scritta "Importo IVA"
053800911023     C* 07    - PARZIALIZZ. SEGNACOLLO DAL
053900000216     C* 08    - bolla dpd
054000131114     C* 09    - scelta in altre parzializz
054100961111     C* 10    - BOLLA DI RIENTRO
054200911023     C* 11    - PARZIALIZZ. MITTENTE
054300131120     C* 12    -
054400961111     C* 13    - SPEDIZIONE IN CONSEGNA IN EVIDENZA IN SUBFILE
054500911023     C* 14    - PARZIALIZZ. DESTINATARIO
054600940318     C* 15    - DATA DAL UGUALE A DATA AL (SCELTO UN SOLO GIORNO)
054700961112     C* 16    - SI TRATTA DI BOLLA EXPORT
054800070403     C* 17    - p.o. utente e lna bolla hanno GEO attiva
054900911023     C* 18/21 - STATO SPEDIZIONI SELEZIONATO
055000991027     C* 22    - ESISTE ESTENSIONE BOLLA 2 e ABILITO IL TASTO  F4=TASSAZIONE 2
055100950825     C* 23    - SFLEND
055200950825     C* 24    - ABILITO IL TASTO "F13=TASSAZIONE"
055300010508     C* 25    - SELEZIONE PER NUMERO O.R.M.
055400070404     C* 26    - presente FIARP
055500070403     C* 27    - presente blocco consegna ARBFBC<>' '
055600941028     C* 28    - VISUALIZZA MSG ERRORE
055700911129     C* 29    - ESISTE CONTRASSEGNO
055800911023     C* 30/31 - LETTURA COMODO
055900911023     C* 32    - BOLLA LETTA DA SCARTARE
056000911023     C* 33    - BOLLA IN FRANCO
056100911023     C* 34    - LETTURA SUBFILE
056200911023     C* 35    - PULIZIA SUBFILE
056300911129     C* 36    - LETTURE
056400911023     C* 37    - ROLL UP
056500991027     C* 38    - ESISTE TB2 ANCHE SE NON PRESENTE TIPO RECORD '2' IN FIAR600F
056600920203     C* 39    - ABILITAZIONE TASTO DI COMANDO INT. BOLLE PARTENZE
056700131111     C* 40-59 - ERRORI
056800131111     C* 60    - email destiantario
056900131111     C* 61    - PROGRAMMA RICHIAMATO per scelta bolla
057000930407     C* 62    - ALTA INTENSITA' PER SPED BLOCCATE
057100040218     C* 63    - Abilita F1  = Aggiunta in reclamo
057200040218     C* 64    - Abilita F16 = Gestione reclamo
057300091214     c* 65    - Abilita visualizzazione beneficiario C/Ass
057400131127     C* 66    - libero - NON usato
057500960903     C* 67    - ABILITAZIONE TASTO DI COMANDO F7=ANOMALIE
057600131127     C* 68    - pos cursore RMN prima serv x  EMETTERE F23 PER CALCOLARE E VISUALIZZARE DATA CONS.
057700930505     C*         PREVISTA E LE ORE DI RITARDO
057800930526     C* 69    - DATA CONSEGNA MERCE >< 0
057900930510     C* 70    - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
058000990119     C* 71    - ABILITAZIONE TASTO DI COMANDO F8=EVENTI SEGNACOLLI
058100131107     C* 72    - SFLDSPCTL  LRI6S05
058200991027     C* 73    - BOLLA FRANCO FRONTIERA
058300970227     C* 74    - ABILITA EVENTI SEGNACOLLI
058400970227     C* 75    - UTILIZZATO PER NON ESEGUIRE LA SUBROUTINE "LEGBOL"
058500970227     C* 76    - SI DDT IN ALTA INTENSITA'
058600021213     C* 77    - NOTE SUPERMERCATI
058700970227     C* 78    - SFLDSP
058800950825     C* 79    - SFLDSPCTL
058900930510     C* 80    - NEL SFL DEI LEGAMI NON EMETTO IL CAMPO PER LA SELEZIONE
059000941028     C* 81    - USATO NEL VIDEO LEGAMI
059100941028     C* 82    - USATO NEL VIDEO LEGAMI
059200941028     C* 83    - USATO NEL VIDEO LEGAMI
059300970925     C* 84    - PARZIALIZZ. SEGNACOLLO AL
059400971219     C* 85    - VISUALIZZO RIFERIMENTO PARTNER ESTERO
059500111005     c* 86    - Abilitazione F22="PERFECT ORDER"
059600111005     C* 87    - Pgm richiamato in modalità "W" (Perfect Order Wurth)
059700110705     C* 88    -
059800931112     C* 89    - UTILIZZATO PER VISUALIZZARE ALTRI TASTI FUNZIONALI
059900920402     C* 90    - INDICA LA PRESENZA DI UN ERRORE
060000941028     C* 91    - ERRORE QCMDEXC (UTILIZZABILE)
060100940923     C* 92/94 - LEGAME BOLLA
060200930525     C* 95    - ESISTONO EVENTI DA INTERROGARE
060300131111     C* 96    -
060400991222     C* 97    - MANUTENZIONE BOLLE ARRIVI
060500941014     C* 98    - ESISTONO VARIAZIONI BOLLE EFFETTUATE
060600930504     C* 99    - OFF  ESISTE LEGAME BOLLA SEGUENTE
060700900518     C*****************************************************************
060800000000     C     *ENTRY        PLIST
060900000000     C                   PARM                    KPJBA
061000040218     c                   parm                    FIDNA1ds
061100131210     c
061200131210      /free
061300131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
061400131210      /end-free
061500980609     C**
061600940712     C                   MOVEL     KPJBU         PARAM1
061700980609     C                   MOVEL     KPJBU         PARAAZ
061800940713     C*
061900060130     C                   Z-ADD     1             CODUT             1 0
062000060130     c                   Movel     rsut          V1cdsa
062100060130     C                   MOVEL     rsut          PARDUT           20
062200941227     C*
062300941227     C                   MOVEL     SIMFEL        PA1FEL
062400990701     C                   MOVEL     V1CDSA        PA1DSA
062500940713     C*-------------------------------------------------------------------------
062600911024     C                   EXSR      CARTAB
062700131111
062800131111     c                   eval      Wend=*off
062900110930     c*
063000111005     c                   setoff                                       87
063100110930     c                   if        pa1flg='W'
063200110930     c                   eval      comrma=pa1rma
063300111005     c                   eval      *in87=*on
063400110930     c                   endif
063500110930     c* disabilito f22=PerfectOrder se richiamato da sè stesso
063600131107     c                   if        pa1pgm='FNLRI6R'
063700110929     c                   eval      no_f22='N'
063800110929     c                   endif
063900101202     c                   if        pa1pgm=*blanks
064000131107     C                   MOVEL     'FNLRI6R'     PA1PGM
064100101202     c                   endif
064200940713     C                   MOVE      'N'           V1CAFF
064300941215     C                   MOVEL     '? '          V1CCVB
064400941215     C                   CLEAR                   V1DCVB
064500941014     C* TESTATA
064600941014     C                   MOVEL     CINTER        V1CTES
064700131127     c                   clear                   v1clna
064800940317      *
064900940712      * IMPOSTO LA LINEA DI ARRIVO
065000060217     c* Non la imposto se richiamato con key specifica
065100040217if  1C     PA1FLG        IFNE      '1'
065200060217     C     PA1FLG        ANDNE     '3'
065300060217     C     PA1FLG        ANDNE     'C'
065400060217     C     PA1FLG        ANDNE     'N'
065500960903     C     PA1FLG        ANDNE     'P'
065600961121     C     PA1FLG        ANDNE     'E'
065700110929     C     PA1FLG        ANDNE     'W'
065800060217     c* o se vengo da int bolle normale (per assistenza)
065900060217     c     pa1flg        andne     ' '
066000060217     c     pa1flg        andne     '4'
066100060130     c                   z-add     dutpou        v1clna
066200020424     c*
066300040121     C                   MOVE      V1CLNA        NUM3              3 0
066400940712     C     NUM3          CHAIN     AZORG01L                           90
066500940713     C  N90              MOVEL     ORGDES        V1CDEA
066600040217e   1C                   ENDIF
066700930702     C*
066800920703     C                   EXSR      PUL01
066900940809     C                   MOVEL     'S'           WRIEM             1
067000930702     C**
067100930702     C* PGM RICHIAMATO
067200920326     C**
067300940713     C** PA1FLG =   - NON RICHIAMATO
067400920423     C**        = 1 - RICHIAMATO CON   KEY SPECIFICA
067500920423     C**        = 2 - RICHIAMATO SENZA KEY SPECIFICA PER   SELEZ BOLLA
067600920423     C**        = 3 - RICHIAMATO CON   KEY SPECIFICA DA INT-BOLL PART
067700920423     C**        = 4 - RICHIAMATO SENZA KEY SPECIFICA SENZA SELEZ BOLLA
067800990129     C**        = C - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F2=C.A
067900941014     C**        = M - MANUTENZIONE BOLLE ARRIVI
068000960903     C**        = N - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F7=ANOMA
068100960903     C**        = P - RICHIAMATO CON   KEY SPECIFICA DA INT-BOLL PART
068200960903     C**              E DISABILITAZ F7=ANOMALIE
068300961121     C**        = E - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F8=EV.SEGNACOLLI
068400110930     C**        = W - RICHIAMATO SENZA KEY SPECIFICA per "PerfectOrder"
068500960903     C*
068600040218      *?Abilito F1=Aggiunta in Reclami?
068700040218      * (se richiamato da pgm. FIDNA1R...)
068800040220     c                   eval      *in63  = *off
068900040303     c                   if            SDSprm >  1
069000040308     c                             and IA1ins = ' '
069100040220     c                   eval      *in63  = *on
069200060301     c                   clear                   v1clna
069300060301     c                   clear                   v1cdea
069400111003     c                   endif
069500960903     C* ABILITO F7=ANOMALIE SE FLAG DIVERSO DA 'N' E 'P'
069600960903     C     PA1FLG        IFNE      'N'
069700960903     C     PA1FLG        ANDNE     'P'
069800960903     C                   SETON                                        67
069900040217     C                   ENDIF
070000961121     C* ABILITO F8=EVENTI SEGNACOLLI  SE FLAG DIVERSO DA 'E'
070100961121     C     PA1FLG        IFNE      'E'
070200961121     C                   SETON                                        74
070300040217     C                   ENDIF
070400960903     C*
070500040217if  1C     PA1FLG        IFEQ      '1'
070600960903     C     PA1FLG        OREQ      'N'
070700940713     C     PA1FLG        OREQ      '3'
070800960903     C     PA1FLG        OREQ      'P'
070900961121     C     PA1FLG        OREQ      'E'
071000990129     C     PA1FLG        OREQ      'C'
071100920703     C*
071200940714     C                   MOVEL     V2CAAS        V1CAAS
071300940713     C                   MOVEL     COMLNP        V1CLP2
071400940714     C                   MOVEL     V2CNRS        V1CNRS
071500940714     C                   MOVEL     V2CNSP        V1CNSP
071600920703     C* SE NO PASSATA KEY SPECIFICA SI PUO' SELEZIONARE
071700940713     C     PA1FLG        IFEQ      '1'
071800960903     C     PA1FLG        OREQ      'N'
071900961121     C     PA1FLG        OREQ      'E'
072000990129     C     PA1FLG        OREQ      'C'
072100070411     c                   if        v1cnsp=0
072200920703     C                   SETON                                        61
072300040217     C                   ENDIF
072400070411     C                   ENDIF
072500040217x   1C                   ELSE
072600110929     c* Se richiamata con "W" devo saltare tutta la gestione della
072700110929     c* prima videata e devo passare subito al caricamento del sfl
072800110929   1ac     pa1flg        ifne      'W'
072900911213     C*
073000940713     C     PA1FLG        IFEQ      '2'
073100911213     C                   SETON                                        61
073200040217     C                   ENDIF
073300920203     C*
073400980603     C                   SETOFF                                       05
073500040217if  2C     PA1FLG        IFEQ      ' '
073600920203     C                   SETON                                        39
073700040217e   2C                   ENDIF
073800941014     C*
073900000616     C* MANUTENZIONE BOLLE ARRIVI 97=ON
074000040217if  2C     PA1FLG        IFEQ      'M'
074100030612      *
074200941014     C                   SETON                                        97
074300030612     C                   MOVEL     CMANUT        V1CTES
074400070420     c
074500040217e   2C                   ENDIF
074600000703     C*
074700941209     C     INIZIO        TAG
074800131114     C***                EXSR      PUL01
074900131114
075000131114     c* se cursore aperto --> lo chiudo
075100131209     c*****              if        Wend=*on  or *in14 or *in11
075200131209     c                   if        WopenC='1'
075300131114     c                   exsr      closecursor
075400131114     c                   endif
075500911025     C*
075600900509     C     FOR01         TAG
075700941209     C*
075800040217if  2C     WRIEM         IFEQ      'S'
075900131107     C                   WRITE     LRI6T01
076000940720     C                   MOVEL     *BLANKS       WRIEM
076100040217e   2C                   ENDIF
076200131107     C                   EXFMT     LRI6D01
076300940713     C*
076400940713     C                   SETOFF                                       28
076500940715     C                   MOVEL     *BLANKS       WRIEM
076600940713     C*
076700980603     C** F3 - FINE LAVORO
076800040217if  2C     *INKC         IFEQ      '1'
076900980603     C   05              MOVEL     '1'           PARF3
077000980603     C                   GOTO      FINE
077100040217e   2C                   ENDIF
077200131115     C**
077300131115     C** F5 - Altre parzializzazioni
077400131115     c                   if        *inke=*on
077500131115     c                   exsr      AltrePar
077600131115     C                   MOVEL     'S'           WRIEM
077700131115     C                   GOTO      FOR01
077800131115     c                   endif
077900131114     C**
078000980603     C** F7 - INTERROGAZIONE BOLLE PARTENZE
078100040217if  2C     *INKG         IFEQ      *ON
078200930922     C                   EXSR      INTPAR
078300940722     C                   MOVEL     'S'           WRIEM
078400920203     C                   GOTO      FOR01
078500040217e   2C                   ENDIF
078600131114     C**
078700131114     C** F10- pulizia di tutti i campi di scelta
078800131114if  2C     *INKj         IFEQ      *ON
078900131114     C                   EXSR      pul01
079000131114     C                   MOVEL     'S'           WRIEM
079100131114     C                   GOTO      FOR01
079200131114e   2C                   ENDIF
079300930922     C**
079400980603     C** F14- INTERROGAZIONE BOLLE TRANSITO
079500040217do  2C   KN              DO
079600940722     C                   MOVEL     'T'           PA2BOL
079700920423     C                   MOVEL     PARAM2        KPJBU
079800940721     C                   CALL      'FNLR68R'
079900920423     C                   PARM                    KPJBA
080000940722     C                   MOVEL     'S'           WRIEM
080100920423     C                   GOTO      FOR01
080200040217e   2C                   ENDDO
080300911025     C*
080400110929e  1ac                   endif
080500040217e   1C                   ENDIF
080600900509     C* CONTROLLI FORMATO1
080700110930     c     pa1flg        ifne      'W'
080800900509     C                   EXSR      CONTR
080900900509     C   90              GOTO      FOR01
081000110929     c                   endif
081100911025     C**
081200911025     C** P E R   N U M E R O   S P E D I Z I O N E
081300131111     c**  o      B A R C O D E
081400040217if  1C     SEL2          IFNE      0
081500131111     c     v1cbarcode    orgt      0
081600971223     C                   EXSR      SINGOL
081700971223     C     WINIZ         IFEQ      '1'
081800971223     C                   GOTO      INIZIO
081900971223     C                   ENDIF
082000971223     C     WFINE         IFEQ      '1'
082100971223     C                   GOTO      FINE
082200971223     C                   ENDIF
082300040217e   1C                   ENDIF
082400911025     C**
082500911025     C**  P E R   S U B F I L E
082600911023     C                   EXSR      ELAB
082700941014     C*
082800901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
082900040217if  1C     NR2           IFEQ      0
083000940713     C                   MOVEL     MSG(1)        V1CMSG
083100131115     C                   SETON                                        28
083200131115     c                   select
083300131115     c                   when      v1crpe<>*blanks
083400131115     C                   SETON                                        58
083500131115     c                   when      v1crmn>0 or v1crma<>*blanks
083600131127     C                   SETON                                        68
083700131115     c                   when      v1cnot<>*blanks
083800131115     C                   SETON                                        62
083900131115     c                   when      v1cprd<>*blanks
084000131115     C                   SETON                                        50
084100131115     c                   other
084200131115     C                   SETON                                        51
084300131115     c                   endsl
084400900516     C                   GOTO      FOR01
084500040217e   1C                   ENDIF
084600971223     C**
084700040217if  1C     V1CRPE        IFNE      *BLANKS
084800010509     C     NR2           ANDEQ     1
084900010509     c     *in25         oreq      *on
085000971223     C     NR2           ANDEQ     1
085100111011     c     v1crmn        orgt      *zeros
085200111011     C     NR2           ANDEQ     1
085300111011     c     v1crma        orne      *blanks
085400111011     C     NR2           ANDEQ     1
085500131111     c     v1cnot        orne      *blanks
085600131111     C     NR2           ANDEQ     1
085700111011     C                   MOVEL     v2clnp        COMLNP
085800111011     c     karb          chain     fnarb01l
085900971223     C                   EXSR      SINGOL
086000040217     C     WINIZ         IFEQ      '1'
086100971223     C                   GOTO      INIZIO
086200040217     C                   ENDIF
086300040217     C     WFINE         IFEQ      '1'
086400971223     C                   GOTO      FINE
086500040217     C                   ENDIF
086600040217e   1C                   ENDIF
086700930922     C**
086800900518     C     FORCT2        TAG
086900131107     C                   WRITE     LRI6T01
087000131107     C                   WRITE     LRI6Z02
087100900515     C** COMANDI SUBFILE
087200900515     C     FORCT         TAG
087300131107     C                   EXFMT     LRI6C02
087400940923     C                   SETOFF                                       28
087500941109     C                   CLEAR                   PA1F03
087600980603     C* F3 - FINE
087700980603     C     *INKC         IFEQ      '1'
087800980603     C   05              MOVEL     '1'           PARF3
087900980603     C                   GOTO      FINE
088000980603     C                   ENDIF
088100930922     C*
088200980603     C* F12 - RITORNO
088300040217if  1C     *INKL         IFEQ      *ON
088400110930     c     pa1flg        cabeq     'W'           fine
088500940713     C     PA1FLG        IFEQ      '3'
088600960903     C     PA1FLG        OREQ      'P'
088700940713     C     PA1FLG        OREQ      '1'
088800960903     C     PA1FLG        OREQ      'N'
088900961121     C     PA1FLG        OREQ      'E'
089000990129     C     PA1FLG        OREQ      'C'
089100940714     C                   Z-ADD     0             V2CAAS
089200920702     C                   Z-ADD     0             COMLNP
089300940714     C                   Z-ADD     0             V2CNRS
089400940714     C                   Z-ADD     0             V2CNSP
089500930922     C                   ENDIF
089600930922     C                   GOTO      INIZIO
089700040217e   1C                   ENDIF
089800900515     C* ROLLUP
089900040217if  1C     *IN37         IFEQ      *ON
090000940713     C   30              DO
090100940713     C                   MOVEL     MSG(15)       V1CMSG
090200940713     C                   SETON                                        28
090300940713     C                   ENDDO
090400930922     C  N30              EXSR      CARSF
090500940713     C                   Z-ADD     COMOD         REC2
090600930922     C                   GOTO      FORCT
090700040217e   1C                   ENDIF
090800900515     C*
090900900516     C** CONTROLLO SE EFFETTUATA UNA SCELTA
091000940713     C                   Z-ADD     PRIMA         REC2
091100131107     C                   READC     LRI6S02                                34
091200911023     C* VISUALIZZO BOLLA
091300040217do  1C     *IN34         DOWEQ     *OFF
091400940714     C                   MOVEL     *BLANKS       V2CFLG
091500911023     C*
091600040217if  2C     V2CSCE        IFEQ      '5'
091700940714     C                   Z-ADD     V2CLNP        COMLNP
091800940712     C     KARB          CHAIN     FNARB01L                           31
091900930518     C*
092000940714     C                   MOVEL     *BLANKS       V2CSCE
092100040217     C     V2CCON        IFEQ      'IN CON'
092200930518     C                   SETON                                        13
092300040217     C                   END
092400930518     C*
092500040217     C     ARBCCA        IFNE      ' '
092600930518     C     ARBFBC        ORNE      ' '
092700930518     C                   SETON                                        62
092800040217     C                   END
092900941014     C*
093000131107     C                   UPDATE    LRI6S02
093100911023     C*
093200911023     C                   EXSR      BOLLA
093300911023     C                   EXSR      VISUAL
093400911023     C*
093500040217x   2C                   ELSE
093600941014     C*
093700941014     C* 97 ON - MANUTENZIONE BOLLE
093800040217if  3C     V2CSCE        IFEQ      '1'
093900941014     C*
094000040217if  4C     *IN97         IFEQ      *ON
094100060227     c                   movel     v2clna        w003a             3
094200060224     C     w003a         LOOKUP    $pog                                   31
094300060130    2c                   if        not *in31 and d66pocl='S'
094400060130     c     v2ccbo        lookup    cbodop                                 31
094500060130    3c                   if        not *in31
094600060130     c                   movel     v2cksc        kscfil
094700060130   x3c                   else
094800060130     c* Se bolla doppia prendo il ksc dall'AR6
094900060130     C                   Z-ADD     v2cAAS        KAAS
095000060130     C                   Z-ADD     v2cLNP        KLNP
095100060130     C                   Z-ADD     v2cNRS        KNRS
095200060130     C                   Z-ADD     v2cNSP        KNSP
095300060130     C                   MOVEL     '2'           KTRC
095400060130     C     KAR61         CHAIN     FIAR6000                           31
095500060130if  4C                   IF          %FOUND(fiar601l)
095600060130     c                   movel     ar6ksc        kscfil
095700060130     c                   else
095800060130     c                   movel     999           kscfil
095900060130    4c                   endif
096000060130    3c                   endif
096100130314     c*
096200060224     c                   movel     kscfil        w003a
096300060224     c     w003a         lookup    $pog                                   31
096400060130    2c                   endif
096500130314     c*
096600040217if  5C     *IN31         IFEQ      *OFF
096700941212     C                   MOVEL     MSG(19)       V1CMSG
096800941212     C                   SETON                                        599028
096900941212     C                   MOVEL     '2'           V2CFLG
097000130314     c*
097100040217x   5C                   ELSE
097200141010
097300130314     c*
097400991027     C                   CLEAR                   FNLR48DS
097500060224     c* come p.o. gestione prendo il p.o. gestore (da £6) della linea
097600060224     c*  di arrivo
097700060224     c                   clear                   fnlv55ds
097800060224     c                   movel     v2clna        d55lin
097900060224     c                   movel     '6'           d55tpt
098000060224     C                   time                    wrkoggiiso
098100060224     C     *iso          move      wrkoggiiso    d55drf
098200060224     c                   call      'FNLV55R'
098300060224     c                   parm                    fnlv55ds
098400060224     c                   movel     d55tfa        d48fgs
098500130314     c*
098600941014     C                   MOVEL     V2CAAS        D48AAS
098700941014     C                   MOVEL     V2CLNP        D48LNP
098800941014     C                   MOVEL     V2CNRS        D48NRS
098900941014     C                   MOVEL     V2CNSP        D48NSP
099000141010     c* routine per verifica dispo da esaminare
099100141010     c                   exsr      sr_wdwdc
099200141010
099300941014     C                   MOVEL     V2CCBO        D48CBO
099400941014     C                   MOVEL     V1CCVB        D48CVB
099500950616     C                   MOVEL     'V'           D48FFR
099600941227     C                   MOVEL     'A'           D48TBO
099700950607     C                   MOVEL     'S'           D48F12
099800941014     C*
099900991027     C                   MOVEL     FNLR48DS      KPJBU
100000941014     C                   CALL      'FNLR48R'
100100941014     C                   PARM                    KPJBA
100200941212     C                   PARM                    DSARBD
100300941212     C                   PARM                    DSARBK
100400991011     C                   PARM                    DARBT
100500941212     C                   PARM                    DSARBG
100600030612     C                   PARM                    trul90ds
100700941212     C*
100800991027     C                   MOVEL     KPJBU         FNLR48DS
100900941212     C*
101000941014     C* ERRORE IN MANUTENZIONE BOLLE
101100040217     C     D48ERR        IFEQ      '1'
101200941014     C                   MOVEL     D48MSG        V1CMSG
101300941014     C                   SETON                                        592890
101400941014     C                   MOVEL     '2'           V2CFLG
101500040217     C                   ENDIF
101600950114     C*
101700950114     C* SE NON C'E' ERRORE AGGIORNO IL SFL CON I DATI DELLA BOLLA VAR
101800040217     C     *IN90         IFEQ      *OFF
101900950120     C                   Z-ADD     V2CLNP        COMLNP
102000950114     C     KARB          CHAIN     FNARB01L                           31
102100950114     C                   MOVEL     ARBRSM        V2CRSM
102200950114     C                   MOVEL     ARBRSD        V2CRSD
102300131114     c                   if        v1cind=*blanks
102400131114     c                   eval      v2cdest=arblod
102500131114     c                   else
102600131114     c                   eval      v2cdest=arbind
102700131114     c                   endif
102800131114     c
102900040217     C                   ENDIF
103000040217e   5C                   ENDIF
103100941014     C*
103200040217x   4C                   ELSE
103300941014     C*
103400911023     C* SELEZIONATA BOLLA: PASSO KEY ED ESCO DA PGM
103500040217     C     *IN61         IFEQ      *ON
103600940714     C                   Z-ADD     V2CLNP        COMLNP
103700940714     C                   MOVEL     '3'           V2CFLG
103800040220      *?- Richiamato da pgm. FIDNA1R?
103900040220if  6c                   if        SDSprm  > 1
104000131111     c                   eval      wARBlnp  = V2Clnp
104100131111     c                   eval      wARBnrs  = V2Cnrs
104200131111     c                   eval      wARBnsp  = V2Cnsp
104300040220     c                   eval      wARBaas = V2Caas
104400040220     c                   clear                   FIDNA1ds
104500040220     c                   eval      IA1ins  = 'S'
104600040220     c                   eval      IA1tor  = 'S'
104700040220     c                   eval      IA1ogg  = ds_OggSped
104800040220e   6c                   endif
104900040217     C                   ENDIF
105000040217e   4C                   ENDIF
105100040217e   3C                   ENDIF
105200930518     C*
105300040217do  3C  N61              DO
105400940714     C                   MOVEL     *BLANKS       V2CSCE
105500040217     C     V2CCON        IFEQ      'IN CON'
105600930518     C                   SETON                                        13
105700930518     C                   ELSE
105800040217     C     V2CCON        IFNE      *BLANKS
105900940714     C                   TESTN                   V2CCON               31
106000930518     C  N31              SETON                                        62
106100040217     C                   END
106200040217     C                   END
106300131107     C                   UPDATE    LRI6S02
106400040217e   3C                   ENDDO
106500930518     C*
106600040217e   2C                   ENDIF
106700911022     C*
106800941014     C                   SETOFF                                       136259
106900911022     C*
107000911022     C* SE FLAG 2 TORNO AL SUBFILE
107100040217     C     V2CFLG        IFNE      *BLANKS
107200900530     C                   SETON                                        34
107300900530     C                   ELSE
107400131107     C                   READC     LRI6S02                                34
107500040217     C                   ENDIF
107600040217e   1C                   ENDDO
107700941014     C*
107800911023     C* FLG =  3 ==> FILE PGM E PASSO KEY
107900940714     C     V2CFLG        IFEQ      '3'
108000911023     C                   GOTO      FINE
108100911023     C                   ELSE
108200911023     C* FLG >< 2 ==> TORNO A SUBFILE
108300911023     C                   GOTO      FORCT2
108400040217     C                   ENDIF
108500911023     C*
108600000000     C     FINE          TAG
108700930504     C*
108800930702     C* CALL A VUOTO AI PGM  PER CHIUDERE I FILES
108900941214     C     PA098         IFEQ      '1'
109000930504     C                   MOVEL     'C'           D98TLA
109100970925     C                   CALL      'TNSD99R'
109200991027     C                   PARM                    TNSD99DS
109300040217     C                   ENDIF
109400930702     C*
109500941214     C     PA075         IFEQ      '1'
109600940714     C                   MOVEL     'C'           PA1FLG
109700940712     C                   MOVEL     PARAM1        KPJBU
109800940714     C                   CALL      'FNLR75R'                            99
109900930524     C                   PARM                    KPJBA
110000040217     C                   ENDIF
110100930702     C*
110200941214     C     PA0B5         IFEQ      '1'
110300950215     C                   CLEAR                   PARAB5
110400940726     C                   MOVEL     'C'           PB5FLG
110500940726     C                   MOVEL     PARAB5        KPJBU
110600940722     C                   CALL      'FNLRB5R'
110700930525     C                   PARM                    KPJBA
110800040217     C                   ENDIF
110900941014     C*
111000941014     C     *IN97         IFEQ      *ON
111100941014     C                   MOVEL     'C'           D66TLA
111200991027     C                   MOVEL     FNLR66DS      KPJBU
111300941014     C                   CALL      'FNLR66R'
111400941014     C                   PARM                    KPJBA
111500040217     C                   ENDIF
111600991027      *
111700991027     C                   IF        Wtibs69r  <>  *BLANKS
111800991027     C                   eval      I69TLA  = 'C'
111900991027     C                   call      'TIBS69R'
112000991027     C                   parm                    TIBS69DS
112100991027     c                   ENDIF
112200930504     C*
112300941109     C* PER F3=FINE PULISCO CHIAVE BOLLA E IMPOSTO FLAG F3
112400941109     C     *INKC         IFEQ      *ON
112500941109     C                   Z-ADD     *ZERO         V2CAAS
112600941109     C                   Z-ADD     *ZERO         COMLNP
112700941109     C                   Z-ADD     *ZERO         V2CNRS
112800941109     C                   Z-ADD     *ZERO         V2CNSP
112900941109     C                   MOVEL     '1'           PA1F03
113000941109     C                   ENDIF
113100980609     C**
113200941212     C* CHIUDO I PGM
113300941212     C* SOLO PER PGM DI MANUTENZIONE
113400941212     C     *IN97         IFEQ      *ON
113500941212     C                   MOVEL     'C'           D66TLA
113600941212     C                   CALL      'FNLR66R'
113700991027     C                   PARM                    FNLR66DS
113800060224     c
113900060224     c                   clear                   fnlv55ds
114000060224     C                   MOVEL     'C'           D55TLA
114100060227     C                   CALL      'FNLV55R'
114200060224     C                   PARM                    FNLv55DS
114300941212     C*
114400941212     C                   MOVEL     'C'           D48TLA
114500991027     C                   MOVEL     FNLR48DS      KPJBU
114600941212     C                   CALL      'FNLR48R'
114700941212     C                   PARM                    KPJBA
114800941212     C*
114900141105     c                   eval      ilry09tla= 'C'
115000141105     c                   eval      kpjbu=fnlry09ds2
115100141105     c                   call      'FNLRY09R'
115200141105     c                   parm                    kpjba
115300941212     C                   ENDIF
115400980603     C**
115500980609     C                   MOVEL     PARAAZ        KPJBU
115600980624     C                   MOVEL     PARAM1        KPJBU
115700941212     C*
115800040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
115900040810     c                   clear                   fidn12ds
116000040810     c                   eval      i12tla = 'C'
116100040810     c                   call      'FIDN12R'
116200040810     c                   parm                    fidn12ds
116300051128     c                   clear                   fnlv80ds
116400051128     c                   eval      ilv80tla = 'C'
116500051128     c                   call      'FNLV80R'
116600051128     c                   parm                    kpjba
116700051128     c                   parm                    FNLV80DS
116800070427     c                   eval      id34tla = 'C'
116900070427     c                   call      'FIDG34R'
117000070427     c                   parm                    fidg34ds
117100131120     c                   eval      ilrg7tla = 'C'
117200131120     c                   call      'FNLRG7R'
117300131120     c                   parm                    fnlrg7ds
117400091216     c                   eval      iinttla = 'C'
117500091216     c                   call      'TRULINTR'
117600091216     c                   parm                    trulintds
117700141105
117800040810     c*
117900000000     C                   SETON                                        LR
118000941011     C   99
118100941011     CANN99              READ      FNBLP01L                               99
118200940802     C**************************************************************************
118300940802     C*    CARICO TABELLE
118400940802     C**************************************************************************
118500911024     C     CARTAB        BEGSR
118600941212     C***
118700941212     C* CARICO TABELLA FILIALI GESTITE £1
118800941212     C***
118900950122     C                   CLEAR                   L1
119000991027     C                   CLEAR                   TRUL06DS
119100941212     C                   MOVE      '£1'          D06COD
119200941212     C                   MOVEL     SIMFEL        D06KEY
119300941212     C                   MOVEL     'L'           D06TLA
119400991027     C                   MOVEL     TRUL06DS      KPJBU
119500941212     C*
119600941212     C                   CALL      'TRUL06R'
119700941212     C                   PARM                    KPJBA
119800991027     C                   MOVEL     KPJBU         TRUL06DS
119900941212     C                   MOVEA     LIN           L1
120000930705     C*
120100930705     C* CARICO TABELLA 7O
120200930705     C                   MOVEL     '7O'          COD
120300941212     C                   Z-ADD     1             C                 3 0
120400940713     C     KTAB2         SETLL     TABEL
120500940713     C     KTAB2         READE     TABEL                                  30
120600930705     C*
120700930705     C     *IN30         DOWEQ     *OFF
120800930705     C     TBLFLG        IFEQ      ' '
120900930705     C                   MOVEL     TBLKEY        KCA(C)
121000930705     C                   MOVEL     TBLUNI        DCA(C)
121100930705     C                   ADD       1             C
121200930705     C                   END
121300940713     C     KTAB2         READE     TABEL                                  30
121400930705     C                   END
121500941024     C*
121600941024     C* CARICO IN SCHIERA LA TABELLA CC
121700941024     C                   Z-ADD     *ZERO         WA
121800941024     C                   CLEAR                   CC1
121900941024     C                   CLEAR                   CC2
122000020329     C                   CLEAR                   CC3
122100941024     C                   MOVEL     'CC'          COD
122200941024     C     KTAB2         SETLL     TABEL00F
122300941024     C     KTAB2         READE     TABEL00F                               99
122400941024     C     *IN99         DOWEQ     *OFF
122500941024     C     TBLKEY        ANDNE     *BLANKS
122600941024     C     TBLKEY        ANDNE     *ZERO
122700941024     C                   ADD       1             WA
122800941024     C                   MOVE      TBLKEY        CC1(WA)
122900941024     C                   MOVEL     TBLUNI        CC2(WA)
123000020402     C                   MOVEL(P)  TBLUNI        DSCC
123100020402     C                   MOVE      §CCDFE        CC3(WA)
123200941024     C     KTAB2         READE     TABEL00F                               99
123300941024     C                   ENDDO
123400060130     C*
123500060130     C* CARICO IN SCHIERA LA TABELLA 3a per cod bolla doppi
123600060130     C                   Z-ADD     *ZERO         WA
123700060130     C                   CLEAR                   cbodop
123800060130     C                   MOVEL     '3A'          COD
123900060130     C     KTAB2         SETLL     TABEL00F
124000060130     C     KTAB2         READE     TABEL00F                               99
124100060130     C     *IN99         DOWEQ     *OFF
124200060130     c                   movel     tbluni        ds3a
124300060130     c                   if        §3atb2<>*blanks
124400060130     C                   ADD       1             WA
124500060130     C                   MOVEL     TBLKEY        cbodop(wa)
124600060130     c                   endif
124700060130     c
124800060130     C     KTAB2         READE     TABEL00F                               99
124900060130     C                   ENDDO
125000020329      *
125100020329      * CARICO IN SCHIERA LA TABELLA CB
125200020329     C                   Z-ADD     *ZERO         WA
125300020329     C                   CLEAR                   CB1
125400020329     C                   CLEAR                   CB2
125500020329     C                   MOVEL     'CB'          COD
125600020329     C     KTAB2         SETLL     TABEL00F
125700020329     C     KTAB2         READE     TABEL00F                               99
125800020329     C     *IN99         DOWEQ     *OFF
125900020329     C     TBLKEY        ANDNE     *BLANKS
126000020329     C     TBLKEY        ANDNE     *ZERO
126100020329     C                   ADD       1             WA
126200020329     C                   MOVE      TBLKEY        CB1(WA)
126300020329     C                   MOVEL     TBLUNI        CB2(WA)
126400020329     C     KTAB2         READE     TABEL00F                               99
126500020329     C                   ENDDO
126600991027     C*
126700991027     C* CARICO LA TABELLA $2
126800991027     C                   MOVEL     '$2'          COD
126900991027     C                   MOVEL(P)  1             KEY
127000991027     C     KTAB1         CHAIN     TABEL00F
127100991027     C                   IF        %FOUND
127200991027     C                   MOVEL     TBLUNI        DS$2
127300991027     C                   ELSE
127400991027     C                   CLEAR                   DS$2
127500991027     C                   ENDIF
127600940802     C*
127700911024     C                   ENDSR
127800940802     C**************************************************************************
127900940802     C*    PULIZIA CAMPI FORMATO 1
128000940802     C**************************************************************************
128100900514     C     PUL01         BEGSR
128200131114     c
128300131114     C* PULIZIA CAMPI CHE RIMANGONO INVARIATI IL SECONDO GIRO
128400131114     C                   MOVEL     ' '           V1CSSP
128500131127     c                   if        pa1flg='2' or pa1flg='4' or pa1flg='M'
128600131127     c                   else
128700131114     C                   MOVEL     *ZEROS        V1CLNA
128800131127     c                   endif
128900131115     c                   clear                   v1cdea
129000131115     c                   clear                   v1cdep
129100131114     C                   MOVEL     *ZEROS        V1CLP1
129200131114     C                   MOVEL     *ZEROS        V1CDTD
129300131114     C                   MOVEL     *ZEROS        V1CDTA
129400131114     c                   clear                   v1cnzd
129500131114     c                   clear                   v1cprd
129600131128     c                   movel     ' '           v1cxind
129700131114     c                   clear                   v1cind
129800131114     c                   clear                   v1clod
129900131128     c                   movel     ' '           v1cxlod
130000131114     C                   MOVE      *BLANKS       V1CRSD
130100131128     C                   movel     ' '           V1CXRSD
130200131114     C                   MOVE      *BLANKS       V1CRSM
130300131128     C                   movel     ' '           V1CXRSM
130400131114     C                   MOVEL     *ZEROS        NDC
130500131114     C                   MOVEL     *ZEROS        NSS
130600940802     C*
130700060613     C                   CLEAR                   wforz1
130800060613     C                   CLEAR                   V1CRPE
130900010509     C                   CLEAR                   V1Cpoe
131000010509     C                   CLEAR                   V1Cnsr
131100010509     C                   CLEAR                   V1Cnor
131200010509     C                   CLEAR                   V1Cnrv
131300110930     C                   CLEAR                   V1Crmn
131400110930     C                   CLEAR                   V1Crma
131500131115     C                   CLEAR                   V1Cdan
131600940713     C                   MOVEL     *ZEROS        V1CLP2
131700940713     C                   MOVEL     *ZEROS        V1CNRS
131800940713     C                   MOVEL     *ZEROS        V1CNSP
131900000103     C                   z-add     UYEAR         V1CAAS
132000000103     c     uyear         iflt      60
132100000103     c                   movel     20            v1caas
132200000103     c                   else
132300940713     C                   MOVEL     19            V1CAAS
132400000103     c                   endif
132500940713     C                   MOVEL     *BLANKS       V1CTSP
132600131114     C                   MOVEL     *BLANKS       V01DTSP
132700940713     C                   MOVEL     *BLANKS       V1CGIA
132800940713     C                   MOVEL     *ZEROS        V1CPSD
132900940713     C                   MOVEL     99            V1CPSA
133000940713     C                   MOVEL     *ZEROS        V1CSCA
133100940713     C                   MOVEL     *ZEROS        V1CSCD
133200940923     C                   MOVEL     *ZERO         V1CFLS
133300940713     C                   MOVE      'N'           V1CAFF
133400961112     C                   CLEAR                   V1CFBR
133500131111     C                   CLEAR                   V1Cbarcode
133600131111     C                   CLEAR                   V1CNOT
133700040216     c                   clear                   V1Ccbo
133800940713     C                   Z-ADD     0             NR2
133900070420     C****               MOVE      'N'           V1CSNP            1
134000070420     c                   clear                   v1csfd
134100941212     C                   SETOFF                                       09
134200131111     c
134300131111     c* se cursore aperto --> lo chiudo
134400131209     c***                if        Wend=*on  or *in14 or *in11
134500131209     c                   if        WopenC='1'
134600131111     c                   exsr      closecursor
134700131111     c                   endif
134800150223     c* se provengo da gestojne r.a. mi posiziono su brtcode
134900150223     c                   if            SDSprm >  1
135000150223     c                             and IA1ins = ' '
135100150223     c                   seton                                        60
135200150223     c                   endif
135300930705     C*
135400900514     C                   ENDSR
135500940802     C**************************************************************************
135600940802     C*    CONTROLLI FORMATO1
135700940802     C**************************************************************************
135800900509     C     CONTR         BEGSR
135900940317      *
136000940713     C                   MOVE      *BLANKS       V1CDEA
136100940713     C                   MOVE      *BLANKS       V1CDEP
136200110930     c                   movel     v1crma        comrma
136300160115     c                   clear                   wchainblp
136400020709
136500131121     c******             clear                   v2dlna
136600940317      *
136700900515     C                   SETOFF                                       900102
136800970925     C                   SETOFF                                       140784
136900980603     C                   SETOFF                                       03  11
137000040216     C                   SETOFF                                       154046
137100940715     C                   SETOFF                                       414243
137200940715     C                   SETOFF                                       444548
137300940715     C                   SETOFF                                       495152
137400131115     C                   SETOFF                                       552547
137500131111     C                   SETOFF                                       535460
137600131111     c                   setoff                                       575058
137700131127     c                   setoff                                       6862
137800110930     c                   clear                   h1riga
137900110930     c                   clear                   h1colo
138000950121     C*
138100950121     C* SE L'ANNO E' IMMESSO DI DUE CIFRE, METTO LE ALTRE DUE
138200950121     C     V1CAAS        IFLT      100
138300950121     C     V1CAAS        IFGT      60
138400950121     C                   ADD       1900          V1CAAS
138500950121     C                   ELSE
138600950121     C                   ADD       2000          V1CAAS
138700950121     C                   ENDIF
138800950121     C                   ENDIF
138900950121     C*
139000950121     C* SE VARIAZIONE CONTROLLO CAL CAUSALE
139100950121     C   97              DO
139200991027     C                   CLEAR                   FNLR66DS
139300950121     C     '?'           SCAN      V1CCVB                                 90
139400950121     C* RICERCA
139500950121    1C     *IN90         IFEQ      *ON
139600991027     C                   CLEAR                   FNLR66DS
139700950121     C                   MOVEL     'V'           D66GES
139800950121     C                   MOVEL     'A'           D66TBO
139900950121     C*
140000950121     C* SE IMMESSO NUMERO SPEDIZIONE LO VISUALIZZO
140100950121    2C     V1CNSP        IFGT      0
140200950121     C                   MOVEL     V1CNSP        D66NSP
140300950121     C                   MOVEL     V1CAAS        D66AAS
140400950121     C                   MOVEL     V1CLP2        D66LNP
140500950121     C                   MOVEL     V1CNRS        D66NRS
140600950121    2C                   ENDIF
140700950121     C*
140800160210     c                   if        %subst(knmus:1:3)='EDP'
140900160121     c                   eval      d66bnp='S'
141000160210     c                   endif
141100991027     C                   MOVEL     FNLR66DS      KPJBU
141200950121     C                   CALL      'FNLR66R'
141300950121     C                   PARM                    KPJBA
141400991027     C                   MOVEL     KPJBU         FNLR66DS
141500950121     C                   MOVEL     D66CVB        V1CCVB
141600950121     C                   MOVEL     D66DES        V1DCVB
141700950121     C                   MOVEL     'S'           WRIEM
141800950203     C                   SETON                                        4790
141900950121     C*
142000950121     C                   GOTO      ENDCTR
142100950121   X1C                   ELSE
142200950121     C* CONTROLLO
142300991027     C                   CLEAR                   FNLR66DS
142400950121     C                   MOVEL     'C'           D66GES
142500950121     C                   MOVEL     'A'           D66TBO
142600950121     C                   MOVEL     V1CCVB        D66CVB
142700950121    2C     V1CNSP        IFGT      0
142800950121     C                   MOVEL     V1CNSP        D66NSP
142900950121     C                   MOVEL     V1CAAS        D66AAS
143000950121     C                   MOVEL     V1CLP2        D66LNP
143100950121     C                   MOVEL     V1CNRS        D66NRS
143200950121    2C                   ENDIF
143300160210     c                   if        %subst(knmus:1:3)='EDP'
143400160121     c                   eval      d66bnp='S'
143500160210    2C                   ENDIF
143600950121     C*
143700991027     C                   MOVEL     FNLR66DS      KPJBU
143800950121     C                   CALL      'FNLR66R'
143900950121     C                   PARM                    KPJBA
144000991027     C                   MOVEL     KPJBU         FNLR66DS
144100950121     C                   MOVEL     D66CVB        V1CCVB
144200950121     C                   MOVEL     D66DES        V1DCVB
144300950121     C*
144400950121     C* ERRORE
144500950121    2C     D66MSG        IFNE      *BLANKS
144600950121     C                   MOVEL     D66MSG        V1CMSG
144700950121     C                   SETON                                        284790
144800950121     C                   GOTO      ENDCTR
144900950121     C*
145000950121   X2C                   ELSE
145100950121     C* DECODIFICA
145200950121     C                   MOVEL     D66DES        V1DCVB
145300950121    2C                   ENDIF
145400950121    1C                   ENDIF
145500950121     C                   ENDDO
145600950102     C*
145700131113     C* immettere almeno una selezione
145800901115     C     SEL1          IFEQ      0
145900920911     C     SEL2          ANDEQ     0
146000940317     C     SEL3          ANDEQ     *BLANKS
146100010509     C     SEL4          ANDEQ     *zeros
146200131113     c
146300971223     C     V1CRPE        ANDEQ     *BLANKS
146400110930     c     v1crmn        andeq     *zeros
146500110930     c     v1crma        andeq     *blanks
146600131111     c     v1cnot        andeq     *blanks
146700131111     c     v1cbarcode    andeq     *zeros
146800940713     C                   MOVEL     MSG(3)        V1CMSG
146900940713     C                   SETON                                        412890
147000901115     C                   GOTO      ENDCTR
147100901115     C                   END
147200940318      *
147300901115     C* DATA SPEDIZIONE
147400900514     C* LINEA ARRIVO
147500131113      * per destinatario o mittente sono obbligatori
147600020627      * la nazione e la provincia del destinatario
147700131113    1C     SEL2          IFEQ      0
147800131113    2c     sel4          ifeq      0
147900020225     c                   clear                   lnantw
148000131113    3C     V1CLNA        IFNE      0
148100940713     C                   MOVEL     V1CLNA        NUM3
148200940713     C     NUM3          CHAIN     AZORG01L                           41
148300131113    4C   41              DO
148400940713     C                   MOVEL     MSG(2)        V1CMSG
148500940713     C                   SETON                                        28  90
148600940713     C                   GOTO      ENDCTR
148700131113    4C                   ENDDO
148800940713     C                   MOVEL     ORGDES        V1CDEA
148900131121     C*******            MOVEL     ORGDES        V2DLNA
149000991222     C                   MOVEL     ORGDE3        og143
149100020225     c                   eval      lnantw = §ogntw
149200900514     C                   SETON                                        01
149300131113    3C                   END
149400131113    2C                   END
149500131113    c
149600020626      * selezione per destinatario o mittente
149700020626     c                   if        sel3 <> *blanks
149800020628     c                   exsr      ContrDatiDes
149900020628     c                   if        *in90 = *on                                  ricerca
150000020628     c                   goto      endctr
150100020628     c                   endif
150200020628     c                   if        *in28 = *on                                  errore
150300020628     c                   eval      *in90 = *on
150400020628     c                   goto      endctr
150500020628     c                   endif
150600020626     c                   endif
150700901115     C* LINEA PARTENZA
150800131111     c                   clear                   lnpntw
150900940713     C     V1CLP1        IFNE      0
151000940713     C                   MOVEL     V1CLP1        NUM3
151100940713     C     NUM3          CHAIN     AZORG01L                           42
151200940713     C   42              DO
151300940713     C                   MOVEL     MSG(5)        V1CMSG
151400940713     C                   SETON                                        28  90
151500940713     C                   GOTO      ENDCTR
151600940713     C                   ENDDO
151700940713     C                   MOVEL     ORGDES        V1CDEP
151800131121     C****               MOVEL     ORGDES        V2DLNP
151900000703     C                   movel     orgde3        og143
152000050304     c                   eval      lnpntw = §ogntw
152100901115     C                   SETON                                        02
152200901115     C                   END
152300940413      *
152400971223      * E' OBBLIGATORIA LNA O LNP
152500971223     C     V1CLNA        IFEQ      *ZERO
152600940713     C     V1CLP1        ANDEQ     *ZERO
152700971223     C     V1CRPE        ANDEQ     *BLANKS
152800010509     c     sel4          andeq     0
152900020628     c     sel3          andeq     *blanks
153000110930     c     v1crmn        andeq     *zeros
153100110930     c     v1crma        andeq     *blanks
153200131113     c     v1cnot        andeq     *blanks
153300131114     c     v1cbarcode    andeq     *zeros
153400940713     C                   MOVEL     MSG(4)        V1CMSG
153500940713     C                   SETON                                        412890
153600940712     C                   GOTO      ENDCTR
153700940712     C                   ENDIF
153800940413     C*
153900911023     C* DATA DAL OBBLIGATORIA
154000940713     C     V1CDTD        IFEQ      0
154100971223     C     V1CRPE        ANDEQ     *BLANKS
154200010509     C     sel4          ANDEQ     0
154300110930     C     v1crmn        ANDEQ     0
154400110930     C     v1crma        ANDEQ     *blanks
154500131111     C     v1cnot        ANDEQ     *blanks
154600131114     c     v1cbarcode    andeq     *zeros
154700131113     c
154800131113     c*********          if        v1cssp <> *blanks or
154900131113     c
155000131113     c                   if        (v1crsd = *blanks and v1crsm = *blanks and
155100131111     c                              v1cind = *blanks and v1clod = *blanks and
155200131111     c                             sel3 <> *blanks and *in01) or
155300131111     c                               sel3 = *blanks
155400940713     C                   MOVEL     MSG(8)        V1CMSG
155500940713     C                   SETON                                        285190
155600911023     C                   GOTO      ENDCTR
155700940712     C                   ENDIF
155800911023     C                   END
155900911023     C*
156000940712     C                   Z-ADD     *ZERO         COMDTD
156100940712     C                   Z-ADD     *ZERO         COMAAD
156200940712     C                   Z-ADD     *ZERO         COMMGD
156300940713     C     V1CDTD        IFNE      *ZERO
156400940713     C                   MOVE      V1CDTD        G02DAT
156500930705     C                   MOVEL     *BLANK        G02ERR
156600940713     C                   CALL      'XSRDA8'
156700930705     C                   PARM                    WLBDAT
156800930705     C     G02ERR        IFEQ      '1'
156900940713     C                   MOVEL     MSG(8)        V1CMSG
157000940713     C                   SETON                                        285190
157100930705     C                   GOTO      ENDCTR
157200930705     C                   ELSE
157300940713     C                   Z-ADD     G02DAT        V1CDTD
157400940713     C                   MOVE      G02INV        COMDTD            8 0
157500940713     C                   MOVEL     G02INV        COMAAD            4 0
157600930705     C                   MOVE      G02INV        COMMGD            4 0
157700930705     C                   END
157800940712     C                   ENDIF
157900900515     C* DATA SPEDIZIONE AL
158000940712     C                   Z-ADD     *ZERO         COMDTA
158100940713     C     V1CDTA        IFNE      0
158200940713     C                   MOVE      V1CDTA        G02DAT
158300930705     C                   MOVEL     *BLANK        G02ERR
158400940713     C                   CALL      'XSRDA8'
158500930705     C                   PARM                    WLBDAT
158600930705     C     G02ERR        IFEQ      '1'
158700941215     C                   MOVEL     MSG(8)        V1CMSG
158800940713     C                   SETON                                        285290
158900930705     C                   GOTO      ENDCTR
159000930705     C                   ELSE
159100940713     C                   Z-ADD     G02DAT        V1CDTA
159200940713     C                   MOVE      G02INV        COMDTA            8 0
159300900515     C                   END
159400930705     C*
159500900515     C* DATA SPEDIZIONE AL MAGGIORE DATA SPEDIZIONE AL
159600911023     C     COMDTA        IFLT      COMDTD
159700940713     C                   MOVEL     MSG(10)       V1CMSG
159800940713     C                   SETON                                        285152
159900940713     C                   SETON                                        90
160000900515     C                   GOTO      ENDCTR
160100900515     C                   END
160200940318      *
160300940712     C                   ELSE
160400940712      *
160500940712      * SE NON IMMESSA LA DATA FINALE, LA PONGO UGUALE A QUELLA INIZIALE
160600940712     C                   Z-ADD     COMDTD        COMDTA
160700940318      *
160800900515     C                   END
160900940318      *
161000940712      * VERIFICO SE LE DUE DATE SONO COINCIDENTI
161100131115     c                   if        comdtd>0
161200940712     C     COMDTD        COMP      COMDTA                                 15
161300131115     c                   endif
161400901115     C***
161500911024     C* GIACENTI
161600911024     C* SCELTE INCONGRUENTI
161700940713     C     V1CGIA        IFEQ      'G'
161800940713     C     V1CSSP        IFEQ      '2'
161900940713     C     V1CSSP        OREQ      '3'
162000940713     C                   MOVEL     MSG(14)       V1CMSG
162100940713     C                   SETON                                        285690
162200911024     C                   GOTO      ENDCTR
162300911024     C                   END
162400911024     C                   END
162500901115     C* NUMERO SERIE AL DEVE ESSERE > NUMERO SERIE DAL
162600901115     C*  SE IMMESSO
162700940713     C     V1CPSD        IFGT      V1CPSA
162800940713     C                   MOVEL     MSG(11)       V1CMSG
162900940713     C                   SETON                                        284890
163000901115     C                   GOTO      ENDCTR
163100901115     C                   END
163200901115     C* NUMERO SEGNACOLLO AL DEVE ESSERE > NUMERO SEGNACOLLO DAL
163300940713     C     V1CSCA        IFGT      0
163400940713     C     V1CSCD        ANDGT     V1CSCA
163500940713     C                   MOVEL     MSG(12)       V1CMSG
163600940713     C                   SETON                                        284990
163700901115     C                   GOTO      ENDCTR
163800901115     C                   END
163900901115     C* SE IMMESSO AL IMMETTERE ANCHE DAL
164000940713     C     V1CSCA        IFGT      0
164100940713     C     V1CSCD        ANDEQ     0
164200940713     C                   MOVEL     MSG(13)       V1CMSG
164300940713     C                   SETON                                        284990
164400901115     C                   GOTO      ENDCTR
164500901115     C                   END
164600940713     C     V1CSCD        COMP      0                                  07
164700970925     C     V1CSCA        COMP      0                                  84
164800940923     C*
164900940923     C* LA FILIALE SEGNACOLLO DEVE ESSERE VALIDA, SE IMMESSSA
165000940923     C     V1CFLS        IFNE      *ZERO
165100940923     C     V1CFLS        CHAIN     AZORG01L                           46
165200941014     C     *IN46         IFEQ      *ON
165300941014     C     *IN46         OREQ      *OFF
165400941014     C     ORGFVA        ANDNE     *BLANKS
165500941014     C                   SETON                                        284690
165600941014     C                   MOVEL     MSG(17)       V1CMSG
165700941014     C                   GOTO      ENDCTR
165800940923     C                   ENDIF
165900941014     C                   ENDIF
166000940923     C*
166100901115     C* SE IMMESSA RAGIONE SOCIALE CERCO PRIMO BLANKS
166200940713     C     V1CRSM        IFNE      *BLANKS
166300901115     C                   SETON                                        11
166400940713     C                   MOVEA     V1CRSM        C1
166500131111     C                   Z-ADD     30            XX                2 0
166600930702     C     C1(XX)        DOWEQ     *BLANKS
166700930702     C                   SUB       1             XX
166800901116     C                   END
166900901116     C                   END
167000901115     C* IMMESSO DESTINATARIO: CERCO PRIMO BLANKS
167100940713     C     V1CRSD        IFNE      *BLANKS
167200901115     C                   SETON                                        14
167300940713     C                   MOVEA     V1CRSD        C1
167400131111     C                   Z-ADD     30            ZZ                2 0
167500930702     C     C1(ZZ)        DOWEQ     *BLANKS
167600930702     C                   SUB       1             ZZ
167700901116     C                   END
167800901115     C                   END
167900131113     C* località DESTINATARIO: CERCO PRIMO BLANKS
168000131113     C     V1Clod        IFNE      *BLANKS
168100131113     C                   SETON                                        14
168200131113     C                   MOVEA     V1Clod        C1
168300131113     C                   Z-ADD     30            LL                2 0
168400131113     C     C1(LL)        DOWEQ     *BLANKS
168500131113     C                   SUB       1             LL
168600131113     C                   END
168700131113     C                   END
168800131113     C* Indirizzo DESTINATARIO: CERCO PRIMO BLANKS
168900131113     C     V1Cind        IFNE      *BLANKS
169000131113     C                   SETON                                        14
169100131113     C                   MOVEA     V1Cind        C1
169200131113     C                   Z-ADD     30            DD                2 0
169300131113     C     C1(DD)        DOWEQ     *BLANKS
169400131113     C                   SUB       1             DD
169500131113     C                   END
169600131113     C                   END
169700131113     c
169800131113     c* 14 on anche per indirizzo o località  DESTINAT
169900131113     c                   if        not *in11 and
170000131113     c                              v1clod<>*blanks or v1cind<>*blanks
170100131113     c                   seton                                        14
170200131113     c                   endif
170300131113     c
170400131113      * Nazione provincia destinatario senza ragioni sociali
170500060613      * imposto per destinatario se non è stata impostata la lna  e la linea
170600060613      * partenza
170700060613     c                   if        v1cprd <> *blanks and
170800020628     c                             not *in11 and not *in14 and not *in01
170900060613     c                   eval      *in14 = *on
171000020628     c                   endif
171100060613     c                   if         v1cnzd <> *blanks and v1cprd = *blanks and
171200060613     c                             not *in11 and not *in14 and not *in01
171300060613     c                             and not *in02
171400060613     c                   eval      *in14 = *on
171500060613     c                   endif
171600901115     C                   END
171700901115     C*
171800901115     C** NUMERO SPEDIZIONE
171900901115     C** LINEA PARTENZA
172000140117     C***  SEL2          IFNE      0
172100140117     c                   if        sel2<> 0 and v1cbarcode<=0
172200940713     C                   Z-ADD     V1CLP2        NUM3
172300940713     C     NUM3          CHAIN     AZORG01L                           45
172400940713     C   45              DO
172500940713     C                   MOVEL     MSG(5)        V1CMSG
172600940713     C                   SETON                                        28  90
172700940713     C                   GOTO      ENDCTR
172800940713     C                   ENDDO
172900911023     C* ANNO SPEDIZIONE
173000940713     C     V1CAAS        IFEQ      0
173100940713     C                   MOVEL     MSG(7)        V1CMSG
173200940713     C                   SETON                                        285590
173300911023     C                   GOTO      ENDCTR
173400911023     C                   END
173500160115     c                   if        not *in97 or (d66utp=*blanks or d66utp='T')
173600940713     C     KLNPX         CHAIN     FNARB01L                           44
173700160115     c                   endif
173800160121     c                   if        *in97 and (
173900160121     c                             (d66utp='T' and not%found(fnarb01l))
174000160121     c                              or d66utp='P')
174100160115     C     KLNPX         CHAIN     FNBLP01L                           44
174200160115     c                   eval      wchainblp='S'
174300160115     c                   endif
174400030527     C*
174500940713     C   44              DO
174600940713     C                   MOVEL     MSG(6)        V1CMSG
174700940713     C                   SETON                                        28  90
174800940713     C                   GOTO      ENDCTR
174900940713     C                   ENDDO
175000160121     c
175100901115     C*
175200901115     C                   END
175300131111     c***
175400131113     C* B A R C O D E   L D V
175500131111     c***
175600131111     c                   if        v1cbarcode>0
175700131111     c                   z-add     b_aas         arbaas
175800131111     C     b_aas         IFLT      100
175900131111     C     b_aas         IFGT      60
176000131111     C                   ADD       1900          arbAAS
176100131111     C                   ELSE
176200131111     C                   ADD       2000          arbAAS
176300131111     C                   ENDIF
176400131111     C                   ENDIF
176500131111     c                   z-add     b_lnp         arblnp
176600131111     c                   z-add     b_nrs         arbnrs
176700131111     c                   z-add     b_nsp         arbnsp
176800160121     c                   if        not *in97 or (d66utp=*blanks or d66utp='T')
176900160121     C     Karb2         CHAIN     FNARB01L                           44
177000160121     c                   endif
177100160121     c                   if        *in97 and (
177200160121     c                             (d66utp='T' and not%found(fnarb01l))
177300160121     c                              or d66utp='P')
177400160121     C     karb2         CHAIN     FNBLP01L                           44
177500160121     c                   eval      wchainblp='S'
177600160121     c                   endif
177700131111     C*
177800160121     C****               if        not%found(fnarb01l)
177900160121     c                   if        *in44
178000131111     C                   MOVEL     MSG(32)       V1CMSG
178100131111     C                   SETON                                        286090
178200131111     C                   GOTO      ENDCTR
178300131111     C                   endif
178400131111     C                   endif
178500010508      ***
178600010508      * N U M E R O   O R M
178700010508      ***
178800010508     C     sel4          ifne      0
178900010508     C     sel2          andeq     0
179000010508      * P.O. EMISSIONE
179100010508     C                   z-add     v1cpoe        num3
179200010508     C     num3          chain     azorg01l                           53
179300010508     C     *in53         ifeq      *on
179400010508     C                   movel     msg(21)       v1cmsg
179500010508     C                   seton                                        28  90
179600010508     C                   goto      endctr
179700010508     C                   endif
179800010508      * N.ORM
179900010510     C                   MOVEL     'M'           WTRC
180000010510     C                   MOVEL(P)  SEL4          WN14             14
180100060216     C     kar44         chain     fiar404l                           54
180200010508     C     *in54         ifeq      *on
180300010508     C                   movel     msg(22)       v1cmsg
180400010508     C                   seton                                        28  90
180500010508     C                   goto      endctr
180600010508     C                   endif
180700010508      *
180800010508     C                   seton                                        25
180900010508      *
181000010508     C                   endif
181100131111     c
181200901115     C*
181300911023     C* TIPO SPEDIZIONE ESPRESSO
181400131114     C                   MOVEL     *BLANKS       V01DTSP
181500940713     C     V1CTSP        IFNE      ' '
181600940715     C     V1CTSP        IFEQ      '?'
181700940715     C                   CALL      'X§TABER'
181800040217     C                   PARM      CodUt         §KUT              1 0
181900040217     C                   PARM      '5E'          COD
182000040217     C                   PARM      *blanks       KEY
182100040217     C                   PARM      *blanks       §DES             30
182200940715     C                   MOVEL     KEY           V1CTSP
182300940715     C                   SETON                                        90
182400940715     C                   MOVEL     'S'           WRIEM
182500940715     C                   ENDIF
182600940715     C*
182700090604     c                   clear                   ds5e
182800940715     C                   MOVEL     '5E'          COD
182900940715     C                   MOVEL     *BLANKS       KEY
183000940715     C                   MOVEL     V1CTSP        KEY
183100940715     C     KTAB1         CHAIN     TABEL00F                           43
183200941014     C     *IN43         IFEQ      *ON
183300941014     C     *IN43         OREQ      *OFF
183400950221     C     TBLFLG        ANDNE     ' '
183500940715     C                   MOVEL     MSG(16)       V1CMSG
183600941014     C                   SETON                                        284390
183700940715     C                   GOTO      ENDCTR
183800941014     C                   ELSE
183900090604     C                   MOVEL     TBLUNI        ds5e
184000131114     C                   MOVEL     §5ed08        V01DTSP
184100991222     C                   ENDIF
184200940715     C                   ENDIF
184300040216      *
184400991222     c*
184500991222     c* Se richiesta linea di arrivo dpd il riferimemnto immesso deve
184600060613     c*  essere composto soltanto da numeri se <=11
184700060613     C*  alfanumerico       se=14
184800050121     c                   clear                   wnodpd            1
184900060613    1c                   if        lnantw = 'DPD' or lnpntw = 'DPD' or
185000050304     c                             lnpntw = *blanks or lnantw = *blanks
185100060613    2c                   if        v1crpe<>*blanks
185200060613    3c                   select
185300060613     c* ok se lungo 14
185400060613     c                   when      %subst(v1crpe:15:1)=' '  and
185500060613     c                             %subst(v1crpe:14:1)<>' '
185600060613     c* ok se lungo 11 ma tutto numerico
185700060613     c                   when      %subst(v1crpe:12:4)=*blanks and
185800060613     c                             %subst(v1crpe:11:1)<>' '
185900060613     C                   MOVEA     V1CRPe        K11
186000060613     C                   Z-ADD     1             YY                2 0
186100060613     c
186200060613    4C                   DO        11            YY
186300060613     C     K11(YY)       LOOKUP    CNM                                    30
186400060613    5C     *IN30         IFEQ      *OFF
186500060613     c* Se non è tutto numerico non è DPD
186600060613     c                   eval      wnodpd = '1'
186700060613     c                   leave
186800060613    5c                   endif
186900060613    4c                   enddo
187000060613   x3c                   other
187100060613     c
187200060613     c* se non è lungo 11 o 14 non è DPD
187300060613     c                   eval      wnodpd = '1'
187400060613    3c                   endsl
187500060613     c
187600060613    3c                   if        wnodpd='1'
187700060613    4c     lnantw        ifeq      'DPD'
187800060613     c     lnpntw        oreq      'DPD'
187900060613     C                   MOVEL     MSG(18)       V1CMSG
188000060613     C                   SETON                                        285890
188100060613     C                   GOTO      ENDCTR
188200060613    4C                   ENDIF
188300060613    3C                   ENDIF
188400060613    2c                   endif
188500061117   x1c                   else
188600061117     c                   eval      wnodpd = '1'
188700060613    1c                   endif
188800941014     C*
188900911023     C** FILE STORICO
189000940713     C     V1CSSP        COMP      '1'                                    18
189100940713     C     V1CSSP        COMP      '2'                                    19
189200940713     C     V1CSSP        COMP      '3'                                    20
189300940713     C     V1CSSP        COMP      ' '                                    21
189400920911     C*
189500940712      * DEVO CONSIDERARE DELLE PRIORITA' PER LE SELEZIONI IMMESSE DALL'UTENTE:
189600010508      *    1°    NUMERO O.R.M.           ( 25 ON)
189700010508      *    2°    MITTENTE                ( 11 ON )
189800010508      *    3°    DESTINATARIO            ( 14 ON )
189900010508      *    4°    LINEA DI PARTENZA       ( 02 ON )
190000010508      *    5°    LINEA DI ARRIVO         ( 01 ON )
190100940712      *
190200010508     C   25              setoff                                       111402
190300010508     C   25              setoff                                       01
190400131113     C   14              SETOFF                                       020111
190500131113     C   11              SETOFF                                       0201
190600940712     C   02              SETOFF                                       01
190700991222     c*se lna dpd e immesso riferimento partner estero, se meno di 12
190800060518     c*  caratteri, calcolo il 12 --> ora non più: il check digit per la
190900060518     c*  ricerca non serve più
191000991222     C                   CLEAR                   krpe
191100050121    1c                   if        v1crpe <> *blanks and (lnantw='DPD'
191200050304     c                             or lnpntw = 'DPD' or wnodpd=' ')
191300060613     C                   movel     V1CRpe        krpe             14
191400991222     C                   ENDIF
191500911023     C*
191600900514     C     ENDCTR        ENDSR
191700020628      *------------------------------------------------------------------------*
191800020628      *    Controlla i dati nazione e provincia destinatario
191900020628      *------------------------------------------------------------------------*
192000020628     c     ContrDatiDes  begsr
192100020628     c                   if        v1cnzd = *blanks and v1cprd = *blanks
192200020628     c                   eval      v1cmsg = msg(23)
192300020628     c                   eval      *in57 = *on
192400020628     c                   eval      *in28 = *on
192500020628     c                   goto      EndContr
192600020628     c                   endif
192700020628      * ricerca tabella nazione
192800020628     c     '?'           scan      v1cnzd                                 90
192900020628     c                   movel     '15'          cod
193000020628     c                   if        *in90
193100020628     c                   call      'TRUL19R'
193200020628     c                   parm                    cod
193300020628     c                   parm                    ordina
193400020628     c                   parm                    key
193500020628     c                   parm                    comando
193600020628     c                   movel     key           v1cnzd
193700020628     c                   eval      *in57 = *on
193800020628     c                   movel     'S'           wriem
193900020628     c                   goto      EndContr
194000020628     c                   endif
194100020628      * controllo nazione destinatario
194200020628     c                   clear                   ds15
194300020628     c                   movel     '15'          cod
194400020628     c                   clear                   key
194500020628     c                   movel     v1cnzd        key
194600020628     c     ktab1         chain     tabel00f
194700020628     c                   if        not %found(tabel00f) or tblflg <> *blanks
194800020628     c                   eval      v1cmsg = msg(24)
194900020628     c                   eval      *in57 = *on
195000020628     c                   eval      *in28 = *on
195100020628     c                   goto      EndContr
195200020628     c                   endif
195300020628     c                   movel     tbluni        ds15
195400060613     c* Se cambiata nazione, clearo forzatura msg se nazine estera
195500060613     c                   if        v1cnzd<>savnzd
195600060613     c                   clear                   wforz1            1
195700060613     c                   eval      savnzd=v1cnzd
195800060613     c                   endif
195900020628      * per nazione italia la provincia è obbligatoria
196000020628     c                   if        §15ita = 'I' and v1cprd = *blanks
196100020628     c                   eval      v1cmsg = msg(25)
196200020628     c                   eval      *in50 = *on
196300020628     c                   eval      *in28 = *on
196400020628     c                   goto      EndContr
196500020628     c                   endif
196600020628      * ricerca tabella provincia
196700020628     c     '?'           scan      v1cprd                                 90
196800020628     c                   movel     'PR'          cod
196900020628     c                   if        *in90
197000020628     c                   call      'TRUL19R'
197100020628     c                   parm                    cod
197200020628     c                   parm                    ordina
197300020628     c                   parm                    key
197400020628     c                   parm                    comando
197500020628     c                   movel     key           v1cprd
197600020628     c                   eval      *in50 = *on
197700020628     c                   movel     'S'           wriem
197800020628     c                   goto      EndContr
197900020628     c                   endif
198000020628      * controllo provincia destinatario
198100020628     c                   if        v1cprd <> *blanks and §15ita = 'I'
198200020628     c                   clear                   dspr
198300020628     c                   movel     'PR'          cod
198400020628     c                   clear                   key
198500020628     c                   movel     v1cprd        key
198600020628     c     ktab1         chain     tabel00f
198700020628     c                   if        not %found(tabel00f) or tblflg <> *blanks
198800020628     c                   eval      v1cmsg = msg(25)
198900020628     c                   eval      *in50 = *on
199000020628     c                   eval      *in28 = *on
199100020628     c                   goto      EndContr
199200020628     c                   endif
199300020628     c                   movel     tbluni        dspr
199400020628      * se nazione italia la provincia deve essere italia
199500020628      * nella tabella ci sono anche le provincie estere e per sapere se ok
199600020628      * faccio il controllo con il codice tassazione per estero
199700020628      * se vuoto vuol dire che è provincia estera
199800020628     c                   if        §15ita = 'I' and §prcte = *blanks
199900020628     c                   eval      v1cmsg = msg(25)
200000020628     c                   eval      *in50 = *on
200100020628     c                   eval      *in28 = *on
200200020628     c                   goto      EndContr
200300020628     c                   endif
200400020628     c                   endif
200500020628
200600020628     c     EndContr      endsr
200700940318      **************************************************************************
200800940318      *    ELABORA BOLLE DA VISUALIZZARE
200900940318      **************************************************************************
201000900515     C     ELAB          BEGSR
201100940318      *
201200131114     C                   SETON                                        35
201300131114     C                   WRITE     LRI6C02
201400131114     C                   SETOFF                                       35
201500131114     C*
201600940713     C                   Z-ADD     1             REC2
201700911024     C                   Z-ADD     1             C
201800131111     c                   eval      wEnd = *off
201900131115     c                   clear                   readFile          3
202000131113     c
202100131113     C                   Z-ADD     COMAAD        $AAS
202200131113     C                   Z-ADD     COMMGD        $MGS
202300131111     c
202400131111    1c                   select
202500010508     c* O.R.M.
202600131111    1c     *in25         wheneq    *on
202700010508     C                   MOVEL     'M'           WTRC
202800010508     C                   MOVEL(P)  SEL4          WN14             14
202900060216     C     Kar44         SETLL     FiAR404L
203000131111     c
203100131111     c***
203200131111     C*  RIFERIMENTO P A R T N E R   E S T E R O
203300131111     c***
203400131111    1C     V1CRPE        whenne    *BLANKS
203500050121     c                   clear                   wgiro             1
203600050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
203700971223     C                   MOVEL(P)  V1CRPE        WN30             30
203800991222     C                   MOVEL     'E'           WTRC
203900991222     c                   else
204000991222     c* D P D
204100991222     C                   MOVEL(P)  krpe          WN30             30
204200060518     C***                MOVEL     'F'           WTRC
204300060518     C                   MOVEL     'I'           WTRC
204400991222     c                   endif
204500991222     c**
204600060216     C     KAR4          SETLL     FiAR403L
204700991222     c**
204800131111     c* C H I   S O N O
204900131111     c**
205000131111    1C     V1CNOT        whenne    *BLANKS
205100170315     C     KarS          SETLL     FIarS03L
205200131111     c* R N N
205300131111    1c     v1crmn        whengt    *zeros
205400110930     c     v1crmn        setll     fnarb33l
205500131115     c                   eval      Readfile='DSP'
205600131111     c* R M A
205700131111    1c     comrma        whenne    *blanks
205800111005     c     comrma        setll     fnarb32l
205900131115     c                   eval      Readfile='DSP'
206000131111     c
206100131113      * RSD/RSM/IND/LOD o solo NAZ/PRV       -----------------------------------
206200110930     c
206300131113     c                   when      *in14  OR *IN11
206400131111     c                   exsr      Presql
206500131111     c                   exsr      Opencursor
206600131111   x1c                   other
206700131111     c
206800971223      *
206900940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1)  -----------------------------------
207000971223    2C   18              DO
207100940713     C     V1CLNA        IFNE      *ZERO
207200940713     C                   Z-ADD     V1CLNA        COMLNA
207300940712     C                   ELSE
207400940712     C                   Z-ADD     L1(1)         COMLNA
207500940712     C                   ENDIF
207600020910     C     K1ARB         SETLL     FNARB78L
207700131115     c                   eval      Readfile='DSP'
207800971223    2C                   ENDDO
207900940318      *
208000940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
208100971223    2C   19              DO
208200940713     C     V1CLNA        IFNE      *ZERO
208300120330     C     V1CLNA        SETLL     FNARB05L
208400940712     C                   ELSE
208500120330     C     *LOVAL        SETLL     FNARB05L
208600940712     C                   ENDIF
208700971223    2C                   ENDDO
208800940318      *
208900940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
209000971223    2C   20              DO
209100940713     C     V1CLNA        IFNE      *ZERO
209200940713     C                   Z-ADD     V1CLNA        COMLNA
209300940712     C                   ELSE
209400940712     C                   Z-ADD     L1(1)         COMLNA
209500940712     C                   ENDIF
209600940713     C     K4ARB         SETLL     FNARB54L
209700131115     c                   eval      Readfile='DCM'
209800971223    2C                   END
209900940318      *
210000940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
210100971223    2C   21              DO
210200131113     c
210300131113     c* per LINEA DI PARTENZA
210400131113    3C     *IN02         IFEQ      *ON
210500950406     C                   SELECT
210600950406     C* Serie = 0 e segnacollo dal diverso da 0 => i segnacolli sono
210700950406     C* sequenziali
210800950406     C     V1CPSD        WHENEQ    *ZEROS
210900950406     C     V1CSCD        ANDNE     *ZEROS
211000950406     C                   Z-ADD     V1CSCD        COMSCD
211100950406     C     K6ARB         SETLL     FNARB27L                               31
211200131115     c                   eval      Readfile='DSP'
211300950406     C     *IN31         IFEQ      *OFF
211400950406     C* Non trovata bolla col numero segnacollo specificato: il
211500950406     C* segnacollo non è quello iniziale della bolla e quindi è sicura-
211600950406     C* mente incluso nella spedizione precedente.
211700950406     C     K6ARB1        READPE    FNARB27L                               31
211800131209     C  n31              Z-ADD     ARBNCD        COMSCD
211900950406     C     K6ARB         SETLL     FNARB27L
212000950406     C                   END
212100131113     c
212200950406     C* Non immesso segnacollo dal oppure serie diversa da zero (segna-
212300950406     C* colli non sequenziali) mi posiziono con segnacollo = 0
212400950406     C                   OTHER
212500950406     C                   MOVE      *ZEROS        COMSCD
212600950405     C     K6ARB         SETLL     FNARB27L
212700131115     c                   eval      Readfile='DSP'
212800950406     C                   ENDSL
212900131113    3C                   END
213000131113     c*
213100131113     c* Per LINEA DI ARRIVO
213200940712     C   01K8ARB         SETLL     FNARB26L
213300131115     c                   eval      Readfile='DSP'
213400131111     c
213500971223    2C                   ENDDO
213600131111    1C                   ENDSL
213700940318      *
213800911023     C                   Z-ADD     0             COMOD
213900911023     C                   EXSR      CARSF
214000940318      *
214100901116     C                   ENDSR
214200940318      **************************************************************************
214300940318      *    CARICO SUBFILE IN AVANTI
214400940318      **************************************************************************
214500900515     C     CARSF         BEGSR
214600940318      *
214700900518     C                   Z-ADD     1             B                 2 0
214800940713     C                   Z-ADD     COMOD         NR2
214900000228     C                   clear                   War4AAS
215000000228     C                   clear                   War4LNP
215100000228     C                   clear                   War4NRS
215200000228     C                   clear                   War4NSP
215300000228      *
215400131113    1C     B             DOWLE     18
215500060216     c                   setoff                                       32
215600131111
215700131113    2c                   select
215800131111     c***
215900131111     c* O R M
216000131111     c***
216100131111     c     *in25         wheneq    *on
216200060216     c     kar44         reade     fiar404l                               30
216300010509     C  N30KARB4         CHAIN     FNARB01L                           32
216400131111     C***
216500131111      * R I F .   p a r t n e r
216600131111     C***
216700131111     C     V1CRPE        whenne    *BLANKS
216800050121     c     leggar4       tag
216900000228      *
217000050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
217100050304     c                             and wgiro <> '2'
217200060216     C     KAR4          READE     FiAR403L                               30
217300991222     c                   else
217400000228      * DPD
217500060216     C     wtrc          READE     FiAR403L                               30
217600000228     C                   If        *in30 = *off
217700060518     c                   movel     ar4n30        dsbl4i
217800000228      *
217900000228     c                   Select
218000000228      * Se non trovo n.parcel e selezionati 0 rcd o più di 1: fine lettura
218100060613     c                   When       §b4ipn <> krpe  and  nr2 <> 1
218200991222     C                   SETON                                            30
218300000228      * Se non trovo n.parcel e è selezionato 1 rcd:  fine lettura e
218400060216      *                                               riposiziono FiAR4
218500060613     c                   When       §b4ipn<> krpe
218600000228     C                   SETON                                            30
218700060216     C     KAR4W         CHAIN     FiAR401L
218800000228      * Se ho trovato il n.parcel ed è il 1o memorizzo key sped
218900000228     c                   When       NR2 = *zeros
219000000228     C                   Z-ADD     AR4AAS        War4AAS
219100000228     C                   Z-ADD     AR4LNP        War4LNP
219200000228     C                   Z-ADD     AR4NRS        War4NRS
219300000228     C                   Z-ADD     AR4NSP        War4NSP
219400000228     c                   Endsl
219500000228     c                   Endif
219600000228      *
219700991222     c                   endif
219800991222     c
219900050304     c                   if        *in30 and wgiro <> '2' and
220000050304     c                             lnpntw <> 'DPD' and lnantw <> 'DPD' and
220100050304     c                             wnodpd = *blanks
220200050121     c                   eval      wgiro = '2'
220300060518     C                   MOVEL     'I'           WTRC
220400050121     C                   MOVEL(P)  KRPe          WN30             30
220500060216     C     Kar4          SETLL     Fiar403l
220600050121     c                   goto      leggar4
220700050121     c                   endif
220800061117     c*
220900061117     c* Se rif.partner può essere DPD non export e immesso rif. lungo 11
221000061117     c* faccio tentaivo di ricerca con rif.truccato
221100061117     c                   if        *in30 and lnantw<>'DPD'and wnodpd=*blanks
221200061117     c                             and %subst(krpe:12:1)=' '
221300061117     C                   eval      krpe='0'+%subst(krpe:1:3)+'99'
221400061117     c                             +%subst(krpe:4:8)
221500061117     C                   MOVEL(P)  KRPe          WN30             30
221600061117     C     Kar4          SETLL     Fiar403L
221700061117     c                   goto      leggar4
221800061117     c                   endif
221900050121     c*
222000971223     C  N30KARB4         CHAIN     FNARB01L                           32
222100131111     c**
222200131111     c* C H I   S O N O
222300131111     c**
222400131111    1C     V1CNOT        whenne    *BLANKS
222500170315     C     KarS          reade     FIarS03L                               30
222600131111     c                   if        not *in30
222700131111     C     KarSXT        CHAIN     FNarT27L                           32
222800131111     C  N32KARTXP        CHAIN     FNarb01L                           32
222900131111    3C     *IN32         IFEQ      *On
223000131111     C                   SETON                                        30
223100131111    3C                   ENDIF
223200131111    2C                   ENDIF
223300110930     c*
223400131111     c* Interrogazione per R M N
223500131111    1C     v1crmn        whengt    *zeros
223600111003     c     v1crmn        reade     fnarb33l                               30
223700940318      *
223800131111     c* Interrogazione per R M A
223900131111    1C     comrma        whenne    *BLANKS
224000111005     c     comrma        reade     fnarb32l                               30
224100131111     c
224200131111      * RSD/RSM/IND/LOD                      -----------------------------------
224300131111     c
224400131113     c                   when      *IN14 OR *IN11
224500131113
224600131111     c                   if        Wend=*off
224700131111     c                   exsr      Readcursor
224800131111     c                   endif
224900131111     c
225000131111     c                   if        Wend=*on
225100131111     c                   seton                                        30
225200131111     c                   endif
225300111005     c*
225400131111     C                   other
225500940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1')  ----------------------------------
225600940318     C   18              DO
225700940713     C     V1CLNA        IFNE      *ZERO
225800020910     C   15K2ARB         READE     FNARB78L                               30
225900020910     C  N15K3ARB         READE     FNARB78L                               30
226000940712      *
226100940712     C                   ELSE
226200940712      *
226300950214     C                   SETON                                            31
226400950214     C                   SETOFF                                           30
226500950214     C     *IN31         DOWEQ     *ON
226600020910     C   15K2ARB         READE     FNARB78L                               31
226700020910     C  N15K3ARB         READE     FNARB78L                               31
226800940318     C   31              ADD       1             C
226900940318     C   31L1(C)         IFNE      0
227000940318     C                   Z-ADD     L1(C)         COMLNA
227100020910     C     K1ARB         SETLL     FNARB78L
227200940318     C                   ELSE
227300950214     C                   SETOFF                                       31
227400940318     C                   SETON                                        30
227500940318     C                   ENDIF
227600940318     C                   ENDDO
227700940318      *
227800940712     C                   ENDIF
227900940318     C                   ENDDO
228000940318      *
228100940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
228200940712     C   19              DO
228300940713     C     V1CLNA        IFNE      *ZERO
228400120330     C     V1CLNA        READE     FNARB05L                               30
228500940712     C                   ELSE
228600120330     C                   READ      FNARB05L                               30
228700940712     C                   ENDIF
228800940712     C                   ENDDO
228900940318      *
229000940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
229100940318     C   20              DO
229200940713     C     V1CLNA        IFNE      *ZERO
229300940712     C   15K5ARB         READE     FNARB54L                               30
229400940713     C  N15V1CLNA        READE     FNARB54L                               30
229500940712      *
229600940712     C                   ELSE
229700940712      *
229800940712     C                   SETON                                            31
229900940712     C     *IN31         DOWEQ     '1'
230000940712     C  N36
230100940712     CAN 15K5ARB         READE     FNARB54L                               31
230200940712     C  N36
230300940712     CANN15COMLNA        READE     FNARB54L                               31
230400060216     C  N31              SETOFF                                       30
230500980120     C   31              ADD       1             C
230600940318     C   31L1(C)         IFNE      0
230700940318     C                   Z-ADD     L1(C)         COMLNA
230800940712     C     K4ARB         SETLL     FNARB54L
230900940318     C                   SETOFF                                       36
231000940318     C                   ELSE
231100940318     C                   SETON                                        30
231200940318     C                   SETOFF                                       31
231300940318     C                   ENDIF
231400940318     C                   ENDDO
231500940318      *
231600940712     C                   ENDIF
231700940318     C                   ENDDO
231800940318      *
231900940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
232000940318     C   21              DO
232100940712     C   01
232200940712     CAN 15K9ARB         READE     FNARB26L                               30
232300940713     C   01
232400940713     CANN15V1CLNA        READE     FNARB26L                               30
232500940712     C   02
232600940712     CAN 15K7ARB         READE     FNARB27L                               30
232700940713     C   02
232800940713     CANN15V1CLP1        READE     FNARB27L                               30
232900131113     c                   ENDDO
233000131113     c                   endsl
233100940318      *
233200060216     c  n32
233300060216     CanN30              EXSR      CONFR
233400900515     C  N30
233500060216     CANN32              EXSR      EMISS
233600900515     C  N30              END
233700131113     c
233800940713     C                   Z-ADD     NR2           COMOD             4 0
233900930518     C                   SETOFF                                       1362
234000131113     c
234100131209     c** 30              if        Wend=*on
234200131209     c   30              if        WopenC='1'
234300131113     c                   exsr      closecursor
234400131113     c                   endif
234500940318      *
234600900515     C                   ENDSR
234700940317      **************************************************************************
234800940317      *    CONFRONTO PER CERCARE BOLLE DA EMETTERE
234900940317      **************************************************************************
235000900515     C     CONFR         BEGSR
235100940317      *
235200911024     C                   SETOFF                                       3236
235300131111     c                   z-add     arbmgs        arbdsp
235400131111     c                   movel     arbaas        arbdsp
235500131111     c
235600110930     c* Se richiamata per Perfect Order devo scartare la bolla se xco diverso da
235700110930     c* quello passato
235800110930     c                   if        pa1flg='W' and arbxco<>pa1xco
235900110930     c                   seton                                        32
236000110930     c                   goto      endcon
236100110930     c                   endif
236200940318      *
236300940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1')  ----------------------------------
236400940712      * DEVO CONTROLLARE LA DATA SPEDIZIONE (ARBDSP)
236500971223    1C   18              DO
236600131115     c                   if        comdtd>0 and *in15
236700131115    2C     ARBDSP        IFNE      COMDTD
236800940713     C     ARBDCM        ANDEQ     *ZERO
236900131115     c
237000131115    3C                   IF        v1clna<>0 and readfile='DSP'
237100940712     C                   SETON                                            30
237200131115     c                   endif
237300131115
237400131115     C                   SETON                                            32
237500911023     C                   GOTO      ENDCON
237600971223    2C                   END
237700131115     c                   endif
237800940318      *
237900131115     c                   if        comdtd>0 and not *in15
238000131115    2C     ARBDSP        IFGT      COMDTA
238100940713     C     ARBDCM        ANDEQ     *ZERO
238200131115     c* si potrebe uscire anche se lettura per riferimenti mittente
238300131115     c*  ma se la linea di arrivo =0 non lo fa ora .... si può sistemare
238400131115     c*  per far leggere meno
238500131115    3C                   IF        v1clna<>0 and readfile='DSP'
238600131115     C                   SETON                                            30
238700131115     c                   endif
238800131115
238900940712     C                   SETON                                            32
239000911023     C                   GOTO      ENDCON
239100971223    2C                   END
239200131115     c                   endif
239300131115     c
239400131115    1C                   END
239500940318      *
239600940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
239700940712      * DEVO CONTROLLARE LA DATA DISTINTA (ARBDDC)
239800971223    1C   19              DO
239900960527     C     ARBFBC        IFNE      *BLANKS
240000960527     C                   SETON                                        32
240100960527     C                   GOTO      ENDCON
240200960527     C                   END
240300960527     C*
240400131115     c                   if        comdtd>0 and *in15
240500131115     C     ARBDDC        IFNE      COMDTD
240600911024     C                   SETON                                            32
240700911024     C                   GOTO      ENDCON
240800911024     C                   END
240900131115     C                   ENDif
241000940318      *
241100131115     c                   if        comdtd>0 and not *in15
241200131115     C     ARBDDC        IFLT      COMDTD
241300940712     C     ARBDDC        ORGT      COMDTA
241400911024     C                   SETON                                            32
241500911024     C                   GOTO      ENDCON
241600911024     C                   END
241700131115     C                   ENDif
241800940318      *
241900940321      * SE NON C'E' NUMERO DISTINTA NON E' IN CONSEGNA
242000911024     C     ARBNDC        IFEQ      0
242100911024     C                   SETON                                            32
242200911024     C                   GOTO      ENDCON
242300911024     C                   END
242400971223    1C                   END
242500940318      *
242600940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
242700940712      *  DEVO CONTROLLARE LA DATA CONSEGNA (ARBDCM)
242800971223    1C   20              DO
242900131115     c                   if        comdtd>0 and *in15
243000131115    2C     ARBDCM        IFNE      COMDTD
243100131115    3C                   IF        v1clna<>0 and readfile='DCM'
243200131115     C                   SETON                                            30
243300131115     c                   endif
243400131115
243500940712     C                   SETON                                            32
243600911024     C                   GOTO      ENDCON
243700971223    2C                   END
243800131115     c                   endif
243900940318      *
244000131115     c                   if        comdtd>0 and not *in15
244100131212     C     ARBDCM        IFLT      COMDTD
244200131212     C                   SETON                                            32
244300131212     C                   GOTO      ENDCON
244400131212     C                   END
244500131212     c
244600131115    2C     ARBDCM        IFGT      COMDTA
244700131115    3C                   IF        v1clna<>0 and readfile='DCM'
244800131115     C                   SETON                                            30
244900131115     c                   endif
245000131115
245100940712     C                   SETON                                            32
245200911024     C                   GOTO      ENDCON
245300971223    2C                   END
245400131115     c                   endif
245500131115     c
245600131115      * SE NON C'E' data consegna  NON E' consegnata
245700131115     C     ARBDCM        IFEQ      0
245800131115     C                   SETON                                            32
245900131115     C                   GOTO      ENDCON
246000131115     C                   ENDIF
246100131115    1C                   END
246200940329      * ------------------------------------------------------------------------
246300940712      * HO SPOSTATO IL CONFRONTO CON MITTENTE E DESTINATARIO PRIMA DEL BLOCCO
246400940712      * CON 21 ON PERCHE' POSSO ACCENDERE IL 30 E ANDARE A FINE CICLO QUANDO
246500940712      * CAMBIA LA RAG.SOCIALE (EVITO DELLE LETTURE)
246600940712      *
246700940712      * OCCHIO!!   Devo usare per primo il confronto che potrebbe portarmi a
246800940712      * fine ciclo: se interrogo per destinatario (14ON) ci deve essere prima il
246900940713      * confronto con V1CRSD, se invece sono per mittente devo fare prima quello
247000940713      * con V1CRSM.
247100060613     C                   MOVE      *BLANKS       $SALTA            1
247200940712      *
247300940712      *??  R A G I O N E    S O C I A L E    D E S T I N A T A R I O  ?
247400131113     c                   if        not *in11 and not *in14
247500131113    1C     V1CRSD        IFNE      *BLANKS
247600131113     c
247700131113    2c     v1cxrsd       ifeq      ' '
247800131113     C     ZZ            SUBST(P)  ARBRSD        COMRAG
247900940712      * Se rag.soc. letta > video riposiziono solo per data con SETGT
248000131113    3C     COMRAG        IFNE      V1CRSD
248100131113     C                   MOVE      'S'           $SALTA
248200131113    3C                   ENDIF
248300131113     c*
248400131113   x2c                   else
248500131114     C                   MOVEL     ARBAAS        KAAS
248600131114     C                   MOVEL     ARBLNP        KLNP
248700131114     C                   MOVEL     ARBNRS        KNRS
248800131114     C                   MOVEL     ARBNSP        KNSP
248900131114     C                   MOVEL     'D'           KTRC
249000131114     C     KAR41         CHAIN     FiAR401L
249100131114     c                   if        %found(fiar401l)
249200131114     c                   eval      arbrsduni=arbrsd+ar4not
249300131114     c                   else
249400131114     c                   eval      arbrsduni=arbrsd
249500131114     c                   endif
249600131114     c
249700131114    3c                   if        %scan(%trimr(v1crsd):arbrsdUNI) = 0
249800131113     C                   MOVE      'S'           $SALTA
249900131113    3c                   endif
250000131113    2C                   ENDIF
250100131113    1C                   ENDIF
250200131113     c
250300131113      *??  I N D I R I Z Z O     D E S T I N A T A R I O  ?
250400131113    1C     V1Cind        IFNE      *BLANKS
250500131113     c
250600131113    2c     v1cxind       ifeq      ' '
250700131113     C     DD            SUBST(P)  ARBind        COMRAG           30
250800131113      * Se rag.soc. letta > video riposiziono solo per data con SETGT
250900131113    3C     COMRAG        IFNE      V1Cind
251000131113     C                   MOVE      'S'           $SALTA
251100131113    3C                   ENDIF
251200131113     c*
251300131113   x2c                   else
251400131113    3c                   if        %scan(%trimr(v1cind):arbind) = 0
251500131113     C                   MOVE      'S'           $SALTA
251600131113    3c                   endif
251700131113    2C                   ENDIF
251800131113    1C                   ENDIF
251900131113      *??  L O C A L I T A'      D E S T I N A T A R I O  ?
252000131113    1C     V1Clod        IFNE      *BLANKS
252100131113     c
252200131113    2c     v1cxlod       ifeq      ' '
252300131113     C     LL            SUBST(P)  ARBlod        COMRAG
252400131113      * Se rag.soc. letta > video riposiziono solo per data con SETGT
252500131113    3C     COMRAG        IFNE      V1Clod
252600131113     C                   MOVE      'S'           $SALTA
252700131113    3C                   ENDIF
252800131113     c*
252900131113   x2c                   else
253000131113    3c                   if        %scan(%trimr(v1clod):arblod) = 0
253100131113     C                   MOVE      'S'           $SALTA
253200131113    3c                   endif
253300131113    2C                   ENDIF
253400131113    1C                   ENDIF
253500940414      *
253600940712      *??  R A G I O N E    S O C I A L E    M I T T E N T E  ?
253700131113    1C     V1CRSM        IFNE      *BLANKS
253800131113    2c     v1cxrsm       ifeq      ' '
253900940712     C     XX            SUBST(P)  ARBRSM        COMRAG
254000940712      * Se rag.soc. letta > video riposiziono solo per data con SETGT
254100131113    3C     COMRAG        IFne      V1CRSM
254200940712     C                   MOVE      'S'           $SALTA
254300131113    3C                   ENDIF
254400131113   x2c                   else
254500131113    3c                   if        %scan(%trimr(v1crsm):arbrsm) = 0
254600131113     C                   MOVE      'S'           $SALTA
254700131113    3c                   endif
254800131113    2C                   ENDIF
254900131113    1C                   ENDIF
255000131113    1C                   ENDIF
255100940712      *
255200131115     c                   if        $SALTA='S'
255300131115     c                   seton                                        32
255400131115     C                   goto      ENDCON
255500131115     c                   endif
255600940318      *
255700940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
255800971223    1C   21              DO
255900940712     C     ARBDSP        IFLT      COMDTD
256000940712     C     COMDTD        ANDNE     *ZERO
256100940712     C                   SETON                                            32
256200911024     C                   GOTO      ENDCON
256300911024     C                   END
256400940318      *
256500940321     C     ARBDSP        IFGT      COMDTA
256600940712     C     COMDTA        ANDNE     *ZERO
256700940712     C                   SETON                                            32
256800131115     c                   if        readfile='DSP'
256900131115     C                   SETON                                            30
257000131115     c                   endif
257100131115     c
257200911024     C                   GOTO      ENDCON
257300131115     C                   ENDif
257400131115     c
257500971223    1C                   ENDDO
257600971223     C**
257700940317      *
257800940317      * PER OGNI SELEZIONE  ----------------------------------------------------
257900940317      *
258000940317      * TESTO LA LINEA DI ARRIVO
258100971223     C     V1CRPE        IFEQ      *BLANKS
258200971223     C     ARBLNA        ANDNE     V1CLNA
258300940713     C     V1CLNA        ANDNE     *ZERO
258400920911     C                   SETON                                            32
258500920911     C                   GOTO      ENDCON
258600971223     C                   ENDIF
258700020628      * testo la nazione/provincia del destinatario
258800060613     c                   if        v1cprd <> *blanks  and
258900020628     c                             (arbnzd <> v1cnzd  or arbprd <> v1cprd)
259000020628     c                   eval      *in32 = *on
259100020628     c                   goto      endcon
259200020628     c                   endif
259300060613     c                   if        (v1cnzd <> *blanks and v1cprd = *blanks) and
259400060613     c                             arbnzd <> v1cnzd
259500060613     c                   eval      *in32 = *on
259600060613     c                   goto      endcon
259700060613     c                   endif
259800020628
259900940317      *
260000940712      * VERIFICO ANCHE LA LNP POICHE' POTREI AVER FATTO ALTRE SELEZIONI PRIMARIE
260100940713     C     ARBLNP        IFNE      V1CLP1
260200940713     C     V1CLP1        ANDNE     *ZERO
260300940712     C                   SETON                                            32
260400940712     C                   GOTO      ENDCON
260500940712     C                   END
260600940923     C* FILIALE SEGNACOLLI
260700940926     C* SE NON VI SONO COLLI, SALTARE QUESTO CONTROLLO
260800940926     C     ARBNCD        IFNE      *ZERO
260900940926     C     ARBNCA        ORNE      *ZERO
261000940926     C     ARBFNS        OREQ      'S'
261100940926     C     ARBFLS        ORNE      *ZERO
261200940923     C     V1CFLS        IFNE      *ZERO
261300940923     C     V1CFLS        ANDNE     ARBFLS
261400940923     C                   SETON                                            32
261500940923     C                   GOTO      ENDCON
261600940926     C                   ENDIF
261700940926     C                   ENDIF
261800961112     C* RESO / FRANCO FRONTIERA
261900961112     C     V1CFBR        IFNE      ' '
262000961112     C                   MOVEL     V1CFBR        WFBR
262100961112     C     V1CFBR        IFEQ      'R'
262200961112     C                   MOVEL     'S'           WFBR
262300961112     C                   ENDIF
262400961112     C     ARBFBR        IFNE      WFBR
262500961112     C                   SETON                                            32
262600961112     C                   GOTO      ENDCON
262700961112     C                   ENDIF
262800961112     C                   ENDIF
262900040216      * Tipo bolla
263000040216     c                   if            V1Ccbo <> *blanks
263100040216     c                             and V1Ccbo <> ARBcbo
263200040216     c                   eval      *in32 = *on
263300040216     c                   goto      EndCon
263400040216     c                   endif
263500900515     C* TIPO SPEDIZIONE
263600940715     C     ARBTSP        IFNE      V1CTSP
263700940715     C     V1CTSP        ANDNE     *BLANKS
263800900515     C                   SETON                                            32
263900900515     C                   GOTO      ENDCON
264000900515     C                   END
264100911023     C* SPEDIZIONI GIACENTI
264200980603     C     V1CGIA        IFEQ      'G'
264300930702     C     ARBFBC        ANDNE     'G'
264400911023     C                   SETON                                            32
264500911023     C                   GOTO      ENDCON
264600911023     C                   END
264700980603     C     V1CGIA        IFEQ      'N'
264800930702     C     ARBFBC        ANDEQ     'G'
264900911023     C                   SETON                                            32
265000911023     C                   GOTO      ENDCON
265100911023     C                   END
265200070420     c*
265300070420     c* SPEDIZIONI IN FERMO DEPOSITO
265400070420     C                   IF        v1csfd='S' and arbffd<>'S'
265500070420     C                   SETON                                            32
265600070420     C                   GOTO      ENDCON
265700070420     C                   END
265800070420     c
265900900518     C* SERIE DAL + AL
266000940713     C     ARBNRS        IFLT      V1CPSD
266100900518     C                   SETON                                            32
266200900518     C                   GOTO      ENDCON
266300900518     C                   END
266400940713     C     ARBNRS        IFGT      V1CPSA
266500900515     C                   SETON                                            32
266600900515     C                   GOTO      ENDCON
266700900515     C                   END
266800900518     C* SEGNACOLLO DAL SOLO
266900930702     C     *IN07         IFEQ      *ON
267000970925     C     *IN84         ANDEQ     *OFF
267100940713     C     KART          SETLL     FNART01L                               31
267200930702     C  N31              SETON                                        32
267300930702     C  N31              GOTO      ENDCON
267400930702     C                   END
267500900515     C* SEGNACOLLO DAL + AL
267600970925     C   84              DO
267700910827     C                   SETON                                        32
267800940713     C     KARB2         SETLL     FNART01L
267900940713     C     KARB2         READE     FNART01L                               31
268000910827     C     *IN31         DOWEQ     '0'
268100940713     C     ARTNSC        IFGE      V1CSCD
268200940713     C     ARTNSC        ANDLE     V1CSCA
268300910827     C                   SETON                                        31
268400910827     C                   SETOFF                                       32
268500910827     C                   END
268600910827     C*
268700940713     C  N31KARB2         READE     FNART01L                               31
268800910827     C                   END
268900910827     C*
269000910827     C   32              GOTO      ENDCON
269100910827     C                   END
269200990119     C*
269300990119     C* NUMERO CA
269400990119    1C     V1CDAN        IFNE      ' '
269500040809     C                   SETON                                        32
269600040809      *
269700040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
269800040809      * legate alla spedizione che passo
269900040809     c                   clear                   fidn12ds
270000040809     c                   eval      i12aas = arbAAS
270100040809     c                   eval      i12lnp = arbLNP
270200040809     c                   eval      i12nrs = arbNRS
270300040809     c                   eval      i12nsp = arbNSP
270400040809     c                   eval      i12fel = 'E'
270500040809     c                   eval      i12fan = 'E'
270600040809     c*
270700040809     c                   call      'FIDN12R'
270800040809     c                   parm                    fidn12ds
270900040809     c*
271000040809     c                   setoff                                       31
271100040809     c*
271200040809     c* se non ci sono errori
271300040809     c                   if        o12err <> ' '
271400040809     c                   seton                                        31
271500040809     c                   else
271600040809     c* se numero di CA trovate maggiore di zero
271700040809     c                   if        o12nca = 0
271800040809     c                   seton                                        31
271900040809     c                   else
272000040809     c                   z-add     1             ii                2 0
272100040809     c                   dow       ii <= o12nca and *in31 = *off
272200040809     C* APERTA
272300040809     C     skdch(ii)     IFEQ      0
272400040809     C     V1CDAN        ANDEQ     'A'
272500040809     C                   SETOFF                                       32
272600040809     C                   SETON                                        31
272700040809     C                   ENDIF
272800040809     C* CHIUSE
272900040809     C     skdch(ii)     IFGT      0
273000040809     C     V1CDAN        ANDEQ     'C'
273100040809     C                   SETOFF                                       32
273200040809     C                   SETON                                        31
273300040809     C                   ENDIF
273400040809     C* CA APERTA O CHIUSA
273500040809     C     V1CDAN        IFEQ      'E'
273600040809     C                   SETOFF                                       32
273700040809     C                   SETON                                        31
273800040809     C                   ENDIF
273900040809     c*
274000040809     c                   add       1             ii
274100040809     c                   enddo
274200040809     c*
274300040809     c                   seton                                        31
274400040809     c*
274500040809     c                   endif
274600040809     c                   endif
274700040809     c**
274800990119    3C                   ENDIF
274900030527     C*
275000940317     C     ENDCON        ENDSR
275100940802     C**************************************************************************
275200940802     C* RICHIAMO PGM DI INTERROGAZIONE PARTENZE
275300940802     C**************************************************************************
275400930922     C     INTPAR        BEGSR
275500940802     C*
275600940722     C                   MOVEL     'A'           PA2FLG
275700930922     C                   MOVEL     PARAM2        KPJBU
275800940722     C                   CALL      'FNLS04R'
275900930922     C                   PARM                    KPJBA
276000940802     C*
276100930922     C                   ENDSR
276200131114     C**************************************************************************
276300131114     C* Atre parzializzazioni
276400131114     C**************************************************************************
276500131114     C     AltrePar      BEGSR
276600131114     c     forw12        TAG
276700131114     c                   exfmt     LRI6W12
276800131114     c
276900131114     c                   setoff                                       285690
277000131114     c                   setoff                                       40
277100131114     c
277200131114     C* GIACENTI
277300131114     C* SCELTE INCONGRUENTI
277400131114     C     V1CGIA        IFEQ      'G'
277500131114     C     V1CSSP        IFEQ      '2'
277600131114     C     V1CSSP        OREQ      '3'
277700131114     C                   MOVEL     MSG(14)       w12MSG
277800131114     C                   SETON                                        285690
277900131114     C                   GOTO      forw12
278000131114     C                   END
278100131114     C                   END
278200131114      * Tipo bolla
278300131114if  1c                   if        V1Ccbo <> *blanks
278400131114     c                   eval      *in31  =  *off
278500131114     c     '?'           scan      V1Ccbo                                 31
278600131114if  2c                   if        *in31 = *on
278700131114     c                   call      'X§TABER'
278800131114     c                   parm      CodUt         §KUT
278900131114     c                   parm      '3A'          COD
279000131114     c                   parm      *blanks       KEY
279100131114     c                   parm      *blanks       §DES
279200131114     c                   movel     KEY           V1Ccbo
279300131114     c                   eval      *in90 = *on
279400131114     c                   goto      forw12
279500131114e   2c                   endif
279600131114      *
279700131114     c                   movel     '3A'          COD
279800131114     c                   movel(p)  V1Ccbo        KEY
279900131114     c     KTAB1         chain     TABEL
280000131114if  2c                   if        NOT %found(TABEL00F)
280100131114     c                             or  TBLflg <> *blanks
280200131114     c                   movel     Msg(9)        w12msg
280300131114     c                   seton                                        284090
280400131114     c                   goto      forw12
280500131114e   2c                   endif
280600131114e   1c                   endif
280700131114     c
280800131114     c                   if        v1cgia<>' ' or v1cdan<>' ' or v1caff<>'N'
280900131114     c                             or v1cfbr<>' ' or v1ccbo<>*blanks
281000131114     c                   seton                                        09
281100131114     c                   else
281200131114     c                   setoff                                       09
281300131114     c                   endif
281400131114     c
281500131114     C*
281600131114     C                   ENDSR
281700940802     C**************************************************************************
281800940802     C* EMISSIONE SUBFILE
281900940802     C**************************************************************************
282000900515     C     EMISS         BEGSR
282100940802     C*
282200930407     C                   SETOFF                                       1362
282300940714     C                   MOVEL     *BLANKS       V2CSCE
282400131121     c**                 if        *in87=*off and not *in02
282500131121     C**                 MOVEL     ARBLNP        V2CLIN
282600131121     c**                 endif
282700131121     c**                 if        *in87 or *in02
282800131121     C**                 MOVEL     ARBLNA        V2CLIN
282900131121     c**                 endif
283000940714     C                   MOVEL     ARBLNP        V2CLNP
283100941212     C                   MOVEL     ARBLNA        V2CLNA
283200911023     C                   MOVEL     ARBMGS        MMGG
283300940714     C                   MOVEL     GG            V2CDSP
283400940714     C                   MOVE      MM            V2CDSP
283500940714     C                   MOVE      ARBAAS        V2CAAS
283600950114     C                   MOVEL     ARBNSP        V2CNSP
283700900516     C*
283800060130     C                   MOVEL     ARBksc        V2Cksc
283900060130     C                   MOVEL     ARBcbo        V2Ccbo
284000060130     C                   MOVEL     ARBRSM        V2CRSM
284100940714     C                   MOVEL     ARBRSD        V2CRSD
284200131114     c                   if        v1cind=*blanks
284300131114     c                   eval      v2cdest=arblod
284400131114     c                   else
284500131114     c                   eval      v2cdest=arbind
284600131114     c                   endif
284700131114     c                   if        arbrma<>*blanks
284800131114     c                   eval      v2crifm=%trim(%editc(arbrmn:'Z')) +'/'+
284900131114     c                                     %trim(arbrma)
285000131114     c                   else
285100131114     c                   eval      v2crifm=%trim(%editc(arbrmn:'Z'))
285200131114     c                   endif
285300131114     C**                 Z-ADD     ARBRMN        V2CRMN
285400131114     C**                 MOVEL     ARBRMA        V2CRMA
285500940714     C                   MOVEL     ARBNRS        V2CNRS
285600940923     C                   MOVEL     ARBFLS        V2CFLS
285700940714     C                   MOVEL     *BLANKS       V2CFLA
285800990119     C* SPEDIZIONE IN DANNO
285900990119     C* SE MI HANNO CHIESTO LE CA CHIUSE VISUALIZZO IL FLAG "C"
286000990119     C* SE MI HANNO CHIESTO LE CA APERTE VISUALIZZO IL FLAG "*"
286100990119     C                   SELECT
286200990119     C     V1CDAN        WHENEQ    'C'
286300990119     C                   MOVEL     'C'           V2CDAN
286400990119     C     V1CDAN        WHENEQ    'A'
286500990119     C                   MOVEL     '*'           V2CDAN
286600990119     C**
286700990119     C                   OTHER
286800040809      *
286900040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
287000040809      * legate alla spedizione che passo
287100040809     c                   clear                   fidn12ds
287200040809     c                   eval      i12aas = arbAAS
287300040809     c                   eval      i12lnp = arbLNP
287400040809     c                   eval      i12nrs = arbNRS
287500040809     c                   eval      i12nsp = arbNSP
287600040809     c                   eval      i12fel = 'E'
287700040809     c                   eval      i12fan = 'E'
287800040809     c*
287900040809     c                   call      'FIDN12R'
288000040809     c                   parm                    fidn12ds
288100040809     c*
288200040809     c                   setoff                                       31
288300040809     c*
288400040809     c* se non ci sono errori
288500040809     c                   if        o12err <> ' '
288600040809     c                   seton                                        31
288700040809     c                   else
288800040809     c* se numero di CA trovate maggiore di zero
288900040809     c                   if        o12nca = 0
289000040809     c                   seton                                        31
289100040809     c                   else
289200040809     c                   z-add     1             ii                2 0
289300040809     c                   dow       ii <= o12nca and *in31 = *off
289400040809     C* SE NE TROVO UNA APERTA, DICO APERTA ALTRIMENTI CHIUSA
289500040809     C     skdch(ii)     IFEQ      0
289600040809     C                   MOVEL     '*'           V2CDAN
289700040809     C                   SETON                                            31
289800040809     C                   ELSE
289900040809     C* CHIUSA MA VADO AVANTI A LEGGERE
290000040809     C                   MOVEL     'C'           V2CDAN
290100040809     C                   ENDIF
290200040809     c*
290300040809     c                   add       1             ii
290400040809     c                   enddo
290500040809     c*
290600040809     c                   seton                                        31
290700040809     c*
290800040809     c                   endif
290900040809     c                   endif
291000990119     C**
291100990119     C                   ENDSL
291200921102     C* SEGNACOLLI
291300930702     C                   CLEAR                   SEG
291400921102     C                   MOVEL     ARBNCD        COMSCN            7
291500921102     C                   MOVEA     COMSCN        SEG(1)
291600921102     C     ARBNCA        IFNE      0
291700921102     C                   MOVEL     ARBNCA        COMSCN
291800921102     C                   MOVEA     COMSCN        SEG(9)
291900921102     C                   END
292000921102     C                   MOVEA     '/'           SEG(8)
292100921102     C* ELIMINO ZERO NON SIGNIFICATIVI
292200921102     C                   Z-ADD     1             Y
292300921102     C     SEG(Y)        DOWEQ     '0'
292400921102     C                   MOVE      ' '           SEG(Y)
292500921102     C                   ADD       1             Y
292600921102     C                   END
292700921102     C*
292800921102     C     ARBNCA        IFNE      0
292900921102     C                   Z-ADD     9             Y                 2 0
293000921102     C     SEG(Y)        DOWEQ     '0'
293100921102     C                   MOVE      ' '           SEG(Y)
293200921102     C                   ADD       1             Y
293300921102     C                   END
293400921102     C                   END
293500940714     C                   MOVEA     SEG           V2CSEG
293600911023     C*
293700911023     C* GIACENZA APERTA
293800941202     C                   Z-ADD     ARBAAS        KAAS
293900940713     C                   Z-ADD     ARBLNP        KLNP
294000940713     C                   Z-ADD     ARBNRS        KNRS
294100940713     C                   Z-ADD     ARBNSP        KNSP
294200050405     C     KGCA1         CHAIN     tigcp21l                           31
294300920410     C  N31GCPATB        IFEQ      ' '
294400920410     C     GCPDCG        IFNE      0
294500990119     C                   MOVE      'C'           V2CGCA
294600911023     C                   ELSE
294700990119     C                   MOVE      '*'           V2CGCA
294800900515     C                   END
294900920410     C                   END
295000990119     C                   MOVEL     ARBTSP        V2CTSP
295100930503     C*
295200911023     C* DATA CONSEGNA
295300940714     C                   MOVEL     *BLANKS       V2CCON
295400940714     C                   MOVEL     *BLANKS       V2CFCC
295500930407     C*
295600930407     C* SE ARBCCA >< 0  METTO DECODIFICA AL POSTO DELLA DATA CONSEGNA
295700930407     C     ARBCCA        IFNE      ' '
295800930705     C                   Z-ADD     1             C
295900930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
296000940909     C   31              MOVEL     DCA(C)        DS7O
296100940909     C*
296200940909     C* NELLA DS7O C'E' SCRITTO SE E' PRIORITARIA L'ANOMALIA O LA DATA
296300940909     C   31§7OVCA        IFEQ      'S'
296400940912     C     §7OVCA        ORNE      'S'
296500940912     C     ARBDCM        ANDEQ     *ZERO
296600940909     C                   MOVEL     §7ODEI        V2CCON
296700940909     C                   SETON                                        62
296800940912     C                   ELSE
296900940909     C*
297000940912     C     ARBDCM        IFNE      *ZERO
297100940909     C                   MOVE      ARBDCM        G02INV
297200940909     C                   Z-ADD     *ZEROS        G02DAT
297300940909     C                   MOVEL     '3'           G02ERR
297400940909     C                   CALL      'XSRDA8'
297500940909     C                   PARM                    WLBDAT
297600940909     C                   MOVEL     G02DAT        V2CCON
297700940909     C                   MOVE      G02DAT        $COMOD            2
297800940909     C                   MOVE      $COMOD        V2CCON
297900940909     C                   ENDIF
298000940912     C                   ENDIF
298100930407     C*
298200930407     C                   ELSE
298300940912     C     ARBDCM        IFNE      *ZERO
298400940712     C                   MOVE      ARBDCM        G02INV
298500940712     C                   Z-ADD     *ZEROS        G02DAT
298600940712     C                   MOVEL     '3'           G02ERR
298700940712     C                   CALL      'XSRDA8'
298800940712     C                   PARM                    WLBDAT
298900940714     C                   MOVEL     G02DAT        V2CCON
299000940713     C                   MOVE      G02DAT        $COMOD            2
299100940714     C                   MOVE      $COMOD        V2CCON
299200940912     C                   ENDIF
299300940912     C                   ENDIF
299400940912     C*
299500940912     C     ARBDCM        IFNE      0
299600930428     C*
299700930506     C* SE ARBDCE = 0  CALL PGM PER CALCOLO DELLA DATA CONSEGNA PREVISTA
299800930506     C*   E DELLE ORE DI RITARDO
299900940712      * SOLO SE RICHIESTA L'AFFIDABILITA'
300000940713     C     V1CAFF        IFEQ      'S'
300100930506     C     ARBDCE        IFEQ      0
300200991027     C                   CLEAR                   TNSD99DS
300300930506     C                   MOVEL     'A'           D98TBO
300400930506     C                   MOVEL     ARBAAS        D98AAS
300500930506     C                   MOVEL     ARBLNP        D98LNP
300600930506     C                   MOVEL     ARBNRS        D98NRS
300700930506     C                   MOVEL     ARBNSP        D98NSP
300800930506     C*
300900970925     C                   CALL      'TNSD99R'
301000991027     C                   PARM                    TNSD99DS
301100930506     C*
301200941214     C* SE PA098 = 1  E' STATA FATTA CALL PGM
301300941214     C                   MOVEL     '1'           PA098
301400930506     C*
301500960201     C* SE C'E' ERRORE VISUALIZZO UNA "R"
301600110301     C     D98ERR        IFNE      '0'
301700960201     C                   MOVEL     'R'           V2CFCC
301800960201     C                   ELSE
301900930506     C* SE D98CRC = S  LA SPEDIZ. E' STAT CONSEGNATA NEI TEMPI PREVISTI
302000930506     C     D98CRC        IFEQ      'S'
302100940714     C                   MOVEL     ' '           V2CFCC
302200930506     C                   ELSE
302300940714     C                   MOVEL     'F'           V2CFCC
302400930506     C                   END
302500960201     C                   END
302600930506     C*
302700930506     C                   ELSE
302800930506     C* ARBDCE > 0
302900940711     C* SE ARBNRC > 0  LA SPEDIZIONE NON E' CONFORME
303000940711     C     ARBNRC        IFGT      0
303100940714     C                   MOVEL     'F'           V2CFCC
303200930504     C                   ELSE
303300940714     C                   MOVEL     ' '           V2CFCC
303400930504     C                   END
303500930506     C                   END
303600940713     C                   ENDIF
303700930402     C*
303800911023     C                   ELSE
303900911023     C* SE NUMERO >< 0 IN CONSEGNA
304000911023     C     ARBNDC        IFNE      0
304100940714     C                   MOVEL     'IN CON'      V2CCON
304200911023     C                   SETON                                        13
304300911023     C                   END
304400911023     C                   END
304500921102     C*
304600940714     C     V2CCON        IFEQ      *BLANKS
304700921102     C     ARBFBC        ANDNE     ' '
304800921102     C                   SETON                                        62
304900921102     C*
305000921102     C     ARBFBC        IFEQ      'B'
305100930504     C     ARBFBC        OREQ      '*'
305200940714     C                   MOVEL     'BLOC.'       V2CCON
305300921102     C                   ELSE
305400921102     C     ARBFBC        IFEQ      'A'
305500940714     C                   MOVEL     'L.AVV'       V2CCON
305600921102     C                   ELSE
305700940714     C                   MOVEL     'GIAC.'       V2CCON
305800921102     C                   END
305900921102     C                   END
306000921102     C                   END
306100911023     C*
306200940714     C                   MOVEL     ARBTMC        V2CTMC
306300900522     C*
306400900518     C                   ADD       1             B
306500940713     C                   ADD       1             NR2
306600131107     C                   WRITE     LRI6S02
306700940720     C*
306800900515     C                   ENDSR
306900940802     C**************************************************************************
307000940802     C* EMISSIONE BOLLA
307100940802     C**************************************************************************
307200911023     C     BOLLA         BEGSR
307300940802     C*
307400070411     C                   SETOFF                                       220616
307500070403     C                   SETOFF                                       170377
307600070404     C                   SETOFF                                       262708
307700131127     C                   SETOFF                                       2933
307800940408     C                   SETOFF                                       687069
307900070524     C                   SETOFF                                       850488
308000131120     C                   SETOFF                                       86
308100991027      *
308200940719     C                   Z-ADD     0             V3CLNA
308300940719     C                   Z-ADD     0             V3CDSP
308400930517     C                   Z-ADD     0             VIDRMN
308500940712     C                   MOVE      *BLANKS       VIDRMA
308600991217     C                   CLEAR                   V3CRPP
308700131120     C                   CLEAR                   atrpda
308800131127     C                   CLEAR                   atrtsp
308900961112     C*
309000911022     C* CONTROLLO SE SPEDIZIONE GIACENTE
309100920219     C* GIACENZA APERTA
309200941202     C                   Z-ADD     ARBAAS        KAAS
309300940713     C                   Z-ADD     ARBLNP        KLNP
309400940713     C                   Z-ADD     ARBNRS        KNRS
309500940713     C                   Z-ADD     ARBNSP        KNSP
309600050405     C     KGCA1         CHAIN     tigcp21l                           31
309700040216if  2C  N31GCPATB        IFEQ      ' '
309800920410     C     GCPDCG        IFNE      0
309900920219     C                   SETON                                        03
310000920219     C                   ELSE
310100070403     C                   SETON                                        0327
310200070412     C                   EVAL      V1DFBC=' GIACENTE '
310300040217     C                   ENDIF
310400040217e   2C                   ENDIF
310500930428     C*
310600021213     C*
310700021213     C* 77 ON - Esistono note supermenrcati d ainterrogare
310800021213     C                   Z-ADD     ARBAAS        KAAS
310900021213     C                   Z-ADD     ARBLNP        KLNP
311000021213     C                   Z-ADD     ARBNRS        KNRS
311100021213     C                   Z-ADD     ARBNSP        KNSP
311200021213     C                   MOVEL     'SCR'         KTRD
311300021213     C     KAR51         CHAIN     FIAR5000                           31
311400021217     C  n31              seton                                        77
311500021217     C   31              setoff                                       77
311600920219     C*
311700920207     C* SE BOLLA CON LA "G" SENZA GIACENZA MECCANIZZATA SEGNALO
311800931116     C     ARBFBC        IFEQ      'G'
311900070412     C                   EVAL      V1DFBC=' GIACENTE '
312000070403     C                   SETON                                        27
312100040217     C                   ENDIF
312200930415     C*
312300940907     C                   MOVEL     ARBAAS        V3CAAS
312400940719     C                   MOVE      ARBNSP        V3CNSP
312500940719     C                   MOVEL     ARBNRS        V3CNRS
312600070404     C                   MOVEL     ARBNRS        V4CNRS
312700131108     C********           MOVEL     ARBNRS        V7CNRS
312800940719     C                   MOVEL     ARBLNP        V3CLNP
312900950508     C                   MOVEL     ARBLNP        V3CLPA
313000131114     c
313100131114     c                   z-add     arbmgs        arbdsp
313200131114     c                   movel     arbaas        arbdsp
313300940908     C*
313400940908     C*  L E G A M I    B O L L A
313500940908     C  N75              EXSR      LEGBOL
313600940908     C*
313700911022     C* NON SEQUENZIALI
313800911023     C     ARBFNS        IFEQ      'S'
313900930705     C                   MOVEL     CNOSEQ        DESFNS
314000911022     C                   ELSE
314100911022     C                   MOVEL     *BLANKS       DESFNS
314200040217     C                   ENDIF
314300930525     C*
314400930525     C* 95 ON - ESITONO EVENTI DA INTERROGARE
314500940713     C                   Z-ADD     ARBAAS        KAAS$
314600940713     C                   Z-ADD     ARBLNP        KLNP
314700940713     C                   Z-ADD     ARBNRS        KNRS
314800940713     C                   Z-ADD     ARBNSP        KNSP
314900990805     C     KEVB1         SETLL     FNEVB01L                               95
315000920402     C*
315100920403     C* 98 ON - ESITONO VARIAZIONI FATTE
315200131007     C     KARB2         SETLL     FiARBF2C                               98
315300990119     C* 04 ON - ESISTONO CA DA VISUALIZZARE
315400990119     C                   CLEAR                   CONTCA
315500040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
315600040809      * legate alla spedizione che passo
315700040809     c                   clear                   fidn12ds
315800040809     c                   eval      i12aas = arbAAS
315900040809     c                   eval      i12lnp = arbLNP
316000040809     c                   eval      i12nrs = arbNRS
316100040809     c                   eval      i12nsp = arbNSP
316200040809     c                   eval      i12fel = 'E'
316300040809     c                   eval      i12fan = 'E'
316400040809     c*
316500040809     c                   call      'FIDN12R'
316600040809     c                   parm                    fidn12ds
316700040809     c*
316800040809     c                   setoff                                       31
316900040809     c*
317000040809     c* se non ci sono errori
317100040809     c                   if        o12err <> ' '
317200040809     c                   seton                                        31
317300040809     c                   else
317400040809     c* se numero di CA trovate maggiore di zero
317500040809     c                   if        o12nca = 0
317600040809     c                   seton                                        31
317700040809     c                   else
317800040809     c                   z-add     1             ii                2 0
317900040809     c                   dow       ii <= o12nca and *in31 = *off
318000040809     c                   movea     skkey(ii)     dskey
318100040809     C                   SETON                                        04
318200040809     C                   ADD       1             CONTCA            3 0
318300040809     C                   Z-ADD     dsaac         VHCAAC
318400040809     C                   Z-ADD     dsfil         VHCFIL
318500040809     C                   Z-ADD     dsnca         VHCNCA
318600040809     c*
318700040809     c                   add       1             ii
318800040809     c                   enddo
318900040809     c*
319000040809     c                   seton                                        31
319100040809     c*
319200040809     c                   endif
319300040809     c                   endif
319400040809     c**
319500070406     c* se Presente FIARP, imposto la cons.richista proposta
319600070404     c                   clear                   v4cdimpr
319700070404     c                   clear                   v4coimpr
319800070404     c                   clear                   v4cprupr
319900070416     c                   clear                   v08dcrpr
320000070404     c                   clear                   v4ctcrpr
320100070404     c                   clear                   v4chcrpr
320200070406     c
320300070406     c* Solo se visualizzabile
320400070406     c     karb2         chain     fiarp01l
320500070406    1c                   if        %found(FIarp01l)
320600070404     c                   seton                                        26
320700070404     C                   MOVEL     arppru        v4cprupr
320800070404     C                   MOVEL     arpoim        v4coimpr
320900070404    2c                   if        arpdim>0
321000070404     c     *iso          movel     arpdim        dataeur
321100070416     c                   movel     dataeur       datadmy
321200070416     c                   movel     datadmy       w0060             6 0
321300070416     c                   movel     w0060         v4cdimpr
321400070404    2c                   endif
321500070404    2c                   if        arpdcr>0
321600070404     c     *iso          movel     arpdcr        dataeur
321700070416     c                   movel     dataeur       v08dcrpr
321800070416     c**                 movel     datadmy       v06dcrpr
321900070404    2c                   endif
322000070404     c                   movel     arphcr        v4chcrpr
322100070404     c                   movel     arptcr        v4ctcrpr
322200070406     c                   if        arptcr=' '
322300070406     c                   eval      v4ctcrpr='IL'
322400070404    1c                   endif
322500070406    1c                   endif
322600070406     c
322700911022     C* TIPO BOLLA CONTROLLO SE FRANCO
322800940719     C                   MOVEL     ARBCBO        V3CCBO
322900911022     C                   MOVEL     '3A'          COD
323000911022     C                   MOVEL     *BLANKS       KEY
323100940712     C                   MOVEL     ARBCBO        KEY
323200940713     C     KTAB1         CHAIN     TABEL                              31
323300911022     C                   MOVEL     TBLUNI        DS3A
323400911022     C                   MOVEL     §3ATB1        FLGTB1            1
323500940719     C                   MOVEL     §3ADES        V3DCBO
323600940719     C                   MOVEL     §3ADES        V5DCBO
323700911030     C* FRANCO
323800040216if  1C     FLGTB1        IFEQ      'F'
323900911022     C                   SETON                                        33
324000940713     C                   MOVE      ARBKSC        V3CKSC
324100940712     C                   MOVE      *BLANKS       VIDKSC
324200040216x   1C                   ELSE
324300911030     C* ASSEGNATO
324400911030     C     ARBCCM        IFNE      0
324500940713     C                   MOVE      ARBCCM        V3CKSC
324600911030     C                   ELSE
324700940713     C                   MOVE      *ZEROS        V3CKSC
324800040217     C                   ENDIF
324900911030     C                   MOVEL     ARBKSC        VIDKSC
325000040217e   1C                   ENDIF
325100070410     c* Numero di telefono del mittente
325200070410     c                   clear                   v3ctelm
325300070410     c                   move      v3cksc        w0040
325400070410     c                   if        w0040<>0000 and w0040<>8888
325500070410     c                             and w0040<>9999
325600070410     c                   eval      i69kin = V3Cksc
325700070410     c                   exsr      CTR_ANAGRA
325800070410     c                   eval      V3ctelm= indtel
325900070410     c                   endif
326000991027     C* 38 ON - ESISTE TB2 ANCHE SENZA TIPO RCD "2" IN FIAR600F
326100920121     C     §3ATB2        COMP      '  '                               3838
326200040218      *
326300040218      *?Controllo se esitono reclami?
326400131111     c                   eval      wARBlnp = ARBlnp
326500131111     c                   eval      wARBnrs = ARBnrs
326600131111     c                   eval      wARBnsp = ARBnsp
326700131111     c                   eval      wARBaas = ARBaas
326800040218     c                   eval      TGDtor  = 'S'
326900040218     c                   eval      TGDogg  = ds_OggSped
327000040218     c     K02TGD02      setll     FITGD000
327100160404     c                   eval      *in64   = (%equal(FITGD32L)   and
327200040303     c                                           (SDSprm =  1
327300040303     c                                        or (SDSprm >  1
327400040303     c                                        and IA1ins <> 'N')))
327500940712     C*
327600940712     C* DECODIFICO  "BOLLA DI RIENTRO"
327700961111     C                   MOVEL     *BLANKS       VIDFBR
327800961111     C                   MOVEL     *BLANKS       VIDFFR
327900961111     C                   SETOFF                                       1073
328000940712     C     ARBFBR        IFEQ      'S'
328100961111     C                   MOVEL     'SI'          VIDFBR
328200961111     C                   SETON                                        10
328300040217     C                   ENDIF
328400961111     C     ARBFBR        IFEQ      'F'
328500961111     C                   MOVEL     'FR'          VIDFFR
328600961111     C                   SETON                                        73
328700040217     C                   ENDIF
328800970130     C*
328900970227     C* FLAG CONSEGNA DDT
329000970130     C                   MOVEL     ARBAAS        KAAS
329100970130     C                   MOVEL     ARBLNP        KLNP
329200970130     C                   MOVEL     ARBNRS        KNRS
329300970130     C                   MOVEL     ARBNSP        KNSP
329400970130     C                   MOVEL     'A'           KTRC
329500970131     C                   CLEAR                   V1DBAM
329600000615     C                   eval      v3csop = ' '
329700060216     C     KAR41         CHAIN     FiAR401L                           31
329800970227     C* C'E' IL RECORD
329900040216if  1C     *IN31         IFEQ      *OFF
330000000615     C                   movel     ar4not        dsbl4a
330100000615     C                   eval      v3csop = §b4aso
330200031027     c                   Move      §b4Abm        wbam              1
330300040216if  2C     WBAM          IFEQ      'S'
330400970131     C     WBAM          OREQ      'C'
330500031027     c     wbam          oreq      'P'
330600031027     c     wbam          oreq      'Q'
330700970227     C                   SETON                                        76
330800970227     C                   MOVEL     'SI DDT'      V1DBAM
330900040216x   2C                   ELSE
331000970227     C                   SETOFF                                       76
331100970227     C                   MOVEL     'NO DDT'      V1DBAM
331200040216e   2C                   ENDIF
331300970227     C**
331400040216sel 2C                   SELECT
331500970227     C     WBAM          WHENEQ    'C'
331600970227     C     WBAM          OREQ      'K'
331700970227     C                   MOVEL     'CLI'         V1DFST
331800970227     C     WBAM          WHENEQ    'J'
331900031027     C     WBAM          oreq      'P'
332000031027     C     WBAM          oreq      'Q'
332100031113     C     WBAM          OREQ      'W'
332200070406     C                   MOVEL     'Arr'         V1DFST
332300970227     C     WBAM          WHENEQ    'S'
332400970227     C     WBAM          OREQ      'Y'
332500070406     C                   MOVEL     'Par'         V1DFST
332600040216x   2C                   OTHER
332700970227     C                   MOVEL     'NO '         V1DFST
332800040216e   2C                   ENDSL
332900970227     C**
333000970227     C* NON  C'E' IL RECORD
333100040216x   1C                   ELSE
333200970227     C                   SETON                                        76
333300970227     C                   MOVEL     'SI DDT'      V1DBAM
333400970227     C                   MOVEL     'PAR'         V1DFST
333500040216e   1C                   ENDIF
333600010510     c* numero O.R.M.
333700010510     C                   MOVEL     'M'           KTRC
333800060216     c     kar41         chain     fiar401l                           31
333900010510     c     *in31         ifeq      *off
334000010510     c                   movel     ar4not        dsbl4m
334100010510     c                   z-add     §b4poe        v3cpoe
334200010510     c                   z-add     §b4nsr        v3cnsr
334300010510     c                   z-add     §b4nor        v3cnor
334400010510     c                   z-add     §b4nrv        v3cnrv
334500010510     c                   else
334600010510     c                   clear                   v3cpoe
334700010510     c                   clear                   v3cnsr
334800010510     c                   clear                   v3cnor
334900010510     c                   clear                   v3cnrv
335000010510     c                   endif
335100130320     c*//* Verifico se ha fiar8
335200130320     C*//                MOVEL     ARBAAS        KAAS
335300130320     C*//                MOVEL     ARBLNP        KLNP
335400130320     C*//                MOVEL     ARBNRS        KNRS
335500130320     C*//                MOVEL     ARBNSP        KNSP
335600130320     c*//  ksped         setll     fiar801l
335700130320     c*//                if        %equal(fiar801l)
335800130320     c*//                eval      v1dAR8='PACKING LIST'
335900130320     C*//                ELSE
336000130320     C*//                CLEAR                   V1DAR8
336100130320     c*//                endif
336200941219     C*
336300941219     C* DECODIFICO  "CONSEGNA A MAGAZZINO"
336400941219     C     ARBFDN        IFEQ      'S'
336500941219     C                   MOVEL     'SI'          V3CFDN
336600941219     C                   ELSE
336700941219     C                   MOVEL     *BLANKS       V3CFDN
336800941219     C                   ENDIF
336900911129     C*
337000920204     C* INCASSO C/A E ASSEGNATO
337100930113     C                   MOVEL     '  '          VIDICC
337200930113     C                   MOVEL     '  '          VIDICA
337300930113     C*
337400930113     C* SE ASSEGNATO O FRANCO CON SECONDA BOLLA
337500930113     C     *IN33         IFEQ      '0'
337600930113     C     *IN33         OREQ      '1'
337700930113     C     *IN38         ANDEQ     '1'
337800930113     C*
337900930330     C     ARBICA        IFNE      ' '
338000941216     C     ARBICA        ANDNE     'R'
338100920204     C                   MOVEL     'SI'          VIDICA
338200040217     C                   ENDIF
338300040217     C                   ENDIF
338400911129     C*
338500911030     C* LINEA PARTENZA DECODIFICA
338600020225     c                   clear                   og143
338700930706     C                   Z-ADD     ARBLNP        NUM3
338800930706     C     NUM3          CHAIN     AZORG01L                           31
338900940719     C  N31              MOVEL     ORGDES        V3DLNP
339000940719     C   31              MOVEL     *BLANKS       V3DLNP
339100020225     c                   eval      og143 = orgde3
339200020225     c                   eval      lnpntw = §ogntw
339300911030     C* LINEA ARRIVO   DECODIFICA
339400991217     c                   clear                   og143
339500070403     C     arblna        CHAIN     AZORG01L                           31
339600940719     C  N31              MOVEL     ORGDES        V3DLNA
339700940719     C   31              MOVEL     *BLANKS       V3DLNA
339800070403     c                   movel     v3dlna        v7dlna
339900991217     C                   movel     orgde3        og143
340000070403     C                   movel     orgde8        og148
340100020304     c                   eval      lnantw = §ogntw
340200070426     c* p.o. gestore di arblna
340300070426     c                   clear                   fnlv55ds
340400070426     c                   movel     arblna        d55lin
340500070426     c                   movel     '6'           d55tpt
340600070426     C                   time                    wrkoggiiso
340700070426     C     *iso          move      wrkoggiiso    d55drf
340800070426     c                   call      'FNLV55R'
340900070426     c                   parm                    fnlv55ds
341000070426     c
341100070426     c                   if        d55tfa<>arblna
341200070426     C     d55tfa        CHAIN     AZORG01L                           31
341300070426     C  n31              movel     orgde8        og148
341400070426     C   31              clear                   og148
341500070426     c                   endif
341600070426     c
341700070403     c
341800070403     c* Abilito visualizzazione nuove funzioni se GEO attiva
341900070426     c* Per p.o. gestione linea di arrivo e p.o. utente
342000070403     c                   if        §oggeot='S' and dutgeot='S'
342100070524     c                   seton                                        1788
342200070406     c                   else
342300070406     c* non visualizzo nemmeno fiarp
342400070406     c                   setoff                                       26
342500070403     c                   endif
342600070524     c
342700070524     c* Vedo se almeno è attivo lo scambio dati con tellus
342800070524     c                   if        not *in88 and §ogtlls<>' ' and dutgeot='S'
342900070524     c                   seton                                        88
343000070524     c                   endif
343100070404     c
343200070404     c* Emetto riga sintetica altre abilitazioni
343300070521     c                   clear                   v5dngd
343400140109     c***                if        *in17
343500070404     c                   clear                   v3cges
343600070404     C                   MOVEL     *BLANKS       V3cgesd
343700070404     c*
343800131120     c                   clear                   fnlrg7ds
343900131120     c                   eval      ilrg7aas=arbaas
344000131120     c                   eval      ilrg7lnp=arblnp
344100131120     c                   eval      ilrg7nrs=arbnrs
344200131120     c                   eval      ilrg7nsp=arbnsp
344300131120     c                   eval      ilrg7lung=58
344400131120     c                   call      'FNLRG7R'
344500131120     c                   parm                    fnlrg7ds
344600131120     c                   movel     olrg7vssin    v3cges
344700140109     c***                endif
344800070410     c
344900070524     c* Reperisco dati della distinta automatica tramite FIDG34R
345000070524     c                   if        *in88
345100070410     c                   clear                   v5ccgi
345200070411     c                   clear                   v5dcgi
345300070410     c                   clear                   v5dtgi
345400070410     c                   clear                   v5dttl
345500070411     c                   clear                   v5clno
345600070427     c                   clear                   v5dlno
345700070427     c                   clear                   v5cargerr
345800070427     c                   clear                   v5dargerr
345900070427     c
346000070427     c                   clear                   fidg34ds
346100070427     c                   eval      id34tip='2'
346200070427     c                   eval      id34aas=arbaas
346300070427     c                   eval      id34lnp=arblnp
346400070427     c                   eval      id34nrs=arbnrs
346500070427     c                   eval      id34nsp=arbnsp
346600070427     c                   call      'FIDG34R'
346700070427     c                   parm                    FIDG34DS
346800070410     c
346900070427     c                   movel     od34cgi       v5ccgi
347000070410    3c                   select
347100070427     c                   when      od34tgi='A'
347200070410     c                   eval      v5dtgi='Automatico'
347300070427     c                   when      od34tgi='M'
347400070410     c                   eval      v5dtgi='Manuale   '
347500070410    3c                   ENDSL
347600070411     c* Decodifica giro
347700070427     c                   eval      v5dcgi=od34cgides
347800070427     c* esito/errore
347900070427     c                   movel     od34err       v5cargerr
348000070427     c                   eval      v5dargerr=od34errdes
348100070427     c* livello normalizzazione
348200070427     c                   movel     od34lno       v5clno
348300070427     c                   eval      v5dlno=od34lnodes
348400070427     c* elaborazione tellus
348500070427     c***                eval      v5dttl=od34ttldes
348600070427     c                   select
348700070427     c                   when      od34ttl='1'
348800070427     c                   eval      v5dttl='SI- CON Giro'
348900070427     c                   when      od34ttl='2'
349000070427     c                   eval      v5dttl='SI-SenzaGiro'
349100070427     c                   endsl
349200070521     c*
349300070521     c* Decodifico se assegnazione automatica o manuale
349400070521     c                   if        arbndc>0 and arbngd<>0
349500070521     c                   eval      v5dngd='MANUALE'
349600070521     c                   endif
349700070521     c                   if        arbndc>0 and arbngd=0
349800070521     c                   eval      v5dngd='AUTOMAT'
349900070521     c                   endif
350000070410     c
350100070410    1c                   endif
350200991217     c
350300060518     c* LNA DPD o lnp dpd: leggo record "I" di ar4
350400000216     c                   setoff                                       08
350500000216     c                   clear                   v3czon
350600000216     c                   clear                   v3crtn
350700040216if  1c                   if        lnantw = 'DPD'
350800991217     c                   seton                                        85
350900991217     C                   MOVEL     ARBAAS        KAAS
351000991217     C                   MOVEL     ARBLNP        KLNP
351100991217     C                   MOVEL     ARBNRS        KNRS
351200991217     C                   MOVEL     ARBNSP        KNSP
351300060518     C                   MOVEL     'I'           KTRC
351400060216     C     KAR41         CHAIN     FiAR401L                           31
351500060518     C  N31              MOVEL     AR4NOT        dsbl4i
351600060518     c   31              clear                   dsbl4i
351700060518     C                   MOVEL(P)  §b4ipn        V3CRPP
351800060518     c                   if        §b4ida<>*blanks or §b4iza>*zeros
351900060518     c                   movel     §b4ida        v3crtn
352000060518     c                   movel     §b4iza        v3czon
352100000216     c                   seton                                        08
352200000216     c                   endif
352300050304     c                   else
352400050304if  1c                   if        lnpntw = 'DPD'
352500050304     C                   MOVEL     ARBAAS        KAAS
352600050304     C                   MOVEL     ARBLNP        KLNP
352700050304     C                   MOVEL     ARBNRS        KNRS
352800050304     C                   MOVEL     ARBNSP        KNSP
352900060518     C                   MOVEL     'I'           KTRC
353000060518     c                   clear                   dsbl4i
353100060216     C     KAR41         CHAIN     FiAR401L
353200060216     c                   if        %found(fiar401l)
353300060518     C**                 MOVEL     AR4NOT        dsbl4f
353400060518     C**                 MOVEL     §b4fnp        V3CRPP
353500060518     C                   MOVEL     AR4NOT        dsbl4i
353600060518     C                   MOVEL     §b4ipn        V3CRPP
353700050304     c                   seton                                        85
353800050304     c                   endif
353900050304     c                   endif
354000040216e   1c                   endif
354100971219     C*
354200971219     C* 16 ON  - SI TRATTA DI BOLLA EXPORT
354300020225     c                   if        lnantw = 'EEX' or lnantw = 'EUP' or
354400060201     c                             lnantw = 'FED' or lnantw='DPD'
354500971219     C                   SETON                                        16
354600971219    1C                   ENDIF
354700940719     C                   MOVEL     V3DLNA        D15LNA
354800920804     C*
354900940719     C                   MOVEL     ARBLNA        V3CLNA
355000940719     C                   MOVEL     ARBRSM        V3CRSM
355100070410     C                   MOVEL     ARBRSM        V4CRSM
355200940713     C                   MOVEL     ARBRSM        V28RSM
355300940719     C                   MOVEL     ARBINM        V3CINM
355400940719     C                   MOVEL     ARBPRM        V3CPRM
355500940719     C                   MOVEL     ARBCAM        V3CCAM
355600940719     C                   MOVEL     ARBLOM        V3CLOM
355700940713     C                   MOVEL     ARBLOM        V25LOM
355800940719     C                   MOVEL     ARBFAP        V3CFAP
355900950421     C                   MOVEL     ARBNZM        V3CNZM
356000131108     c                   if        arbnzm<>*blaNKS
356100131108     C                   MOVEL     arbnzm        v7cprm
356200131108     c                   else
356300131108     C                   MOVEL(P)  arbprm        v7cprm
356400131108     c                   endif
356500131108     c
356600050113     C                   MOVEL     ARBXCO        VIDXCO
356700130221     c                   if        vidxco >*zeros  and no_f22=*blanks
356800110928     c                   seton                                        86
356900110928     c                   endif
357000940719     C                   MOVEL     *BLANKS       V3CPIM
357100940719     C                   MOVEL     *BLANKS       V4CPID
357200061108     c                   clear                   v3cdfm
357300061108     c                   clear                   v4cdfd
357400950411     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
357500060216     C* DA FiAR4 SE RECORD ESISTENTE
357600950411     C                   MOVEL     ARBAAS        KAAS
357700950411     C                   MOVEL     ARBLNP        KLNP
357800950411     C                   MOVEL     ARBNRS        KNRS
357900950411     C                   MOVEL     ARBNSP        KNSP
358000950411     C                   MOVEL     'P'           KTRC
358100060216     C     KAR41         CHAIN     FiAR401L                           31
358200040216if  1C     *IN31         IFEQ      *OFF
358300950411     C                   MOVEL     AR4NOT        V3CPIM
358400950411     C                   MOVE      AR4NOT        V4CPID
358500040216x   1C                   ELSE
358600950411     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI ARBCPI
358700040216if  2C     ARBCPI        IFNE      *BLANKS
358800950411     C  N33              MOVEL     ARBCPI        V4CPID
358900950411     C   33§3ATB2        IFEQ      '  '
359000950411     C                   MOVEL     ARBCPI        V3CPIM
359100950411     C                   ELSE
359200950411     C                   MOVEL     ARBCPI        V4CPID
359300040217     C                   ENDIF
359400040217e   2C                   ENDIF
359500040217e   1C                   ENDIF
359600061108      * valorizzo il codice fiscale del mittente e del destinatario
359700061108      * FIAR4 rcd Q
359800061108     c                   eval      ktrc = 'Q'
359900061108     c     kar41         chain     fiar401l
360000061108if  1c                   if        %found(fiar401l)
360100061108     c                   movel     ar4not        v3cdfm
360200061108     c                   move      ar4not        v4cdfd
360300061108x   1c                   endif
360400980704     C* VISUALIZZO SE C'E' IL RIFERIM PARTNER
360500971219     C                   CLEAR                   V3CEST
360600971219     C                   MOVEL     ARBAAS        KAAS
360700971219     C                   MOVEL     ARBLNP        KLNP
360800971219     C                   MOVEL     ARBNRS        KNRS
360900971219     C                   MOVEL     ARBNSP        KNSP
361000971219     C                   MOVEL     'E'           KTRC
361100060216     C     KAR41         CHAIN     FiAR401L                           31
361200980704     C* VISUALIZZO IN UNA BOLLA ITALIA SE C'E'
361300980704     C* VISUALIZZO SEMPRE IN UNA BOLLA IMPORT/EXPORT
361400971219     C  N31              MOVEL     AR4NOT        V3CRPP
361500980704     C  N31              SETON                                        85
361600980704     C**
361700020225     c                   if        *in31 and
361800020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
361900020225     c                              lnpntw = 'FED') or
362000020225     c                             (lnantw = 'EEX' or lnantw = 'EUP' or
362100020225     c                              lnantw = 'FED')
362200980704     C                   SETON                                        85
362300980704     C                   ENDIF
362400980704     C**
362500940719     C                   MOVEL     ARBRSD        V4CRSD
362600131108     C**********         MOVEL     ARBRSD        V30RSD
362700940713     C                   MOVEL     ARBRSD        V32RSD
362800940719     C                   MOVEL     ARBIND        V4CIND
362900131119     C                   MOVEL     ARBIND        V33IND
363000940719     C                   MOVEL     ARBPRD        V4CPRD
363100940719     C                   MOVEL     ARBCAD        V4CCAD
363200940719     C                   MOVEL     ARBLOD        V4CLOD
363300131119     C*****              MOVEL     ARBLOD        V24LOD
363400940719     C                   MOVEL     ARBFIN        V4CFIN
363500940719     C                   MOVEL     ARBNZD        V4CNZD
363600911022     C                   MOVEL     ARBGC1        VIDGC1
363700911022     C                   MOVEL     ARBGC2        VIDGC2
363800911022     C                   MOVEL     ARBCTR        VIDCTR
363900940719     C                   MOVEL     ARBCTS        V6CCTS
364000131108     c                   if        arbnzd<>*blaNKS
364100131108     C                   MOVEL     arbnzd        v7cprd
364200131108     c                   else
364300131108     C                   MOVEL(P)  arbprd        v7cprd
364400131108     c                   endif
364500940719     C* DECODIFICA CODICE TASSAZIONE
364600940719     C                   MOVEL     'CT'          COD
364700940719     C                   MOVEL     *BLANKS       KEY
364800940719     C                   MOVEL     ARBCTS        KEY
364900940719     C     KTAB1         CHAIN     TABEL00F                           31
365000940719     C                   MOVEL     TBLUNI        V6DCTS
365100911022     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
365200911022     C                   MOVEL     'TR'          COD
365300911022     C                   MOVEL     *BLANKS       KEY
365400911022     C                   MOVEL     VIDCTR        UNOCTR            1
365500911022     C                   MOVEL     UNOCTR        KEY
365600940713     C     KTAB1         CHAIN     TABEL                              31
365700911022     C                   MOVEL     TBLUNI        DEFCTR
365800070404     C*****              MOVEL     ARBFTM        V3CFTM
365900070404     c                   if        arbftm='S'
366000070404     c                   eval      v3dftm='TASSATA CON TARIFFA MITTENTE'
366100070404     c                   endif
366200940711     C* DECODIFICA TIPO SERVIZIO BOLLE
366300090604     c                   clear                   ds5e
366400940711     C                   MOVEL     '5E'          COD
366500940711     C                   MOVEL     *BLANKS       KEY
366600940713     C                   MOVEL     ARBTSP        KEY
366700941024     C     KTAB1         CHAIN     TABEL00F                           31
366800090604     C  N31              MOVEL     TBLUNI        ds5e
366900090604     C  N31              MOVEL     §5ed08        V1DTSP
367000940719     C   31              MOVEL     *BLANKS       V1DTSP
367100131127     c                   if        §5eftc<>' '
367200131127     c                   movel     ri            atrtsp
367300131127     c                   else
367400131127     c                   movel     hi            atrtsp
367500131127     c                   endif
367600940713     C                   MOVEL     ARBVMD        VIDVMD
367700940713     C                   MOVEL     ARBVAD        VIDVAD
367800940713     C                   MOVEL     ARBIAS        VIDIAS
367900940713     C                   MOVEL     ARBVAS        VIDVAS
368000911022     C                   MOVEL     ARBNAS        VIDNAS
368100950214     C                   CLEAR                   VIDFLS
368200950214     C     ARBNCD        IFNE      *ZERO
368300950214     C     ARBFNS        ORNE      *BLANKS
368400940923     C                   MOVEL     ARBFLS        VIDFLS
368500950214     C                   ENDIF
368600950214     C                   Z-ADD     ARBFLS        V3CFLS
368700911022     C                   MOVEL     ARBNCL        VIDNCL
368800070410     C                   MOVEL     ARBPKB        VIDPKb
368900070410     C                   MOVEL     ARBPKF        VIDPKF
369000070410     C                   MOVEL     ARBVLb        VIDVLb
369100070410     C                   MOVEL     ARBFVb        VIDFVb
369200070410     C                   MOVEL     ARBVLF        VIDVLF
369300070410     C                   MOVEL     ARBFVF        VIDFVF
369400930705     C     ARBNCR        IFLT      99998
369500940711     C                   MOVEL     ARBVLC        VIDVLC
369600940711     C                   MOVEL     ARBNCR        VIDNCR
369700921231     C                   ELSE
369800990909     C                   Z-ADD     0             VIDVLC
369900990909     C                   Z-ADD     0             VIDNCR
370000921231     C                   END
370100990909     C     ARBNCP        IFLT      99998
370200990909     C                   MOVEL     ARBPKC        VIDPKC
370300990909     C                   MOVEL     ARBNCP        VIDNCP
370400990909     C                   ELSE
370500990909     C                   Z-ADD     0             VIDPKC
370600990909     C                   Z-ADD     0             VIDNCP
370700990909     C                   END
370800940711     C                   MOVEL     ARBQFT        VIDQFT
370900031119
371000031119      * Referente e Telefono del destinatario
371100031119     c                   Clear                   V4cRef
371200031119     c                   Clear                   V4cTeld
371300091214     c                   clear                   w_cdi
371400031119      * li prendo da Fiar500f record 'GEN'
371500130320     c                   clear                   v1dAR8l
371600130320     c                   clear                   v1dAR8
371700031119     c                   Clear                   dAr5Gen
371800031119     c                   Z-Add     ArbAas        Kaas
371900031119     c                   Z-Add     ArbLNp        Klnp
372000031119     c                   Z-Add     ArbNrs        Knrs
372100031119     c                   Z-Add     ArbNsp        Knsp
372200031119     c                   Eval      Ktrd = 'GEN'
372300031119     c     kAr51         Chain     Fiar501l
372400031119If  1c                   If        %Found(Fiar501l)
372500031119     c                   Movel     Ar5Uni        Dar5Gen
372600031119     c                   Eval      V4cRef = §Ar5Ref
372700031119     c                   Eval      V4cTeld = §Ar5Teld
372800091214     c* memorizzo eventuale codice beneficiario assegno
372900091214     c                   eval      w_cdi=§ar5cdi
373000130320     c* verifico se LDV con packing-list e/o PDF
373100130320sel 2c                   select
373200130320w   2c                   when      §AR5als = 'S'
373300130320     c                   eval      v1dAR8l = 'PACKING LIST PDF'
373400130320     c                   eval      v1dAR8  = 'PACKING PDF '
373500130320w   2c                   when      §AR5alx = 'S'
373600130320     c                   eval      v1dAR8l = 'PACKING LIST'
373700130320     c                   eval      v1dAR8  = 'PACKING LIST'
373800130320e   2c                   endsl
373900031119    1c                   EndIf
374000040301
374100040301      * Se colli di valore visualizzo "V" nell'apposito campo
374200040301      *  con lo specifico attributo di visualizzazione
374300040301if  1c                   if        §AR5bva = *blanks
374400040301     c                   eval      VIDbva  = *blanks
374500040301x   1c                   else
374600040301     c                   eval      VIDbva  = 'V'
374700040301e   1c                   endif
374800110601
374900110610     c* email destinatario
375000110601     c                   setoff                                       60
375100110601     c                   clear                   v4cemw
375200110610     c                   clear                   v4cemd
375300130927     c                   Clear                   dAr5emd
375400110601      * li prendo da Fiar500f record 'EMD'
375500110601     c                   Z-Add     ArbAas        Kaas
375600110601     c                   Z-Add     ArbLNp        Klnp
375700110601     c                   Z-Add     ArbNrs        Knrs
375800110601     c                   Z-Add     ArbNsp        Knsp
375900110601     c                   Eval      Ktrd = 'EMD'
376000130927     c***  kAr51         setll     Fiar501l
376100130927     c     kAr51         chain     Fiar501l
376200130927If  1c                   If        %found(Fiar501l)
376300130927     c                   eval      dar5emd=ar5uni
376400130927     c                   if        §ar5tel<>*blanks or §ar5eml<>*blanks
376500110610     c                   Eval      V4cemd = 'SI'
376600110601     c                   seton                                        60
376700130927     c                   endif
376800110601    1c                   EndIf
376900040301
377000030121      * Bancali
377100030121     c                   Clear                   VidTba
377200030121     c                   Clear                   VidTb2
377300030122     c                   Clear                   VidNba
377400030122     c                   Clear                   VidNb2
377500030703      * Record 'BAN'
377600030703If  0c                   If        %subst(ArbGva:1:1) = 'B'
377700030121     c                   Clear                   dAr5Ban
377800030121     c                   Eval      Ktrd = 'BAN'
377900030121     c     kAr51         Chain     Fiar501l
378000030121If  1c                   If        %Found(Fiar501l)
378100030121     c                   Movel     Ar5Uni        Dar5Ban
378200030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
378300030122      * per le note relative al mancato reso dei bancali
378400030122     c                   If        Not *In77 and
378500030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
378600030122     c                   Eval      *In77 = *On
378700030122     c                   EndIf
378800030121     c                   Eval      VidNba = §Ar5Nba
378900030121     c                   Eval      VidNb2 = §Ar5Nb2
379000030121If  2c                   If        §Ar5Tba <> *Zeros
379100030703     c                   Eval      wtba = §Ar5Tba
379200030703     c                   ExSr      Sr_DecTba
379300030703     c                   Eval      VidTba = wdestba
379400030121    2c                   EndIf
379500030121If  2c                   If        §Ar5Tb2 <> *Zeros
379600030703     c                   Eval      wtba = §Ar5Tb2
379700030703     c                   ExSr      Sr_DecTba
379800030703     c                   Eval      VidTb2 = wdestba
379900030121    2c                   EndIf
380000030121    1c                   EndIf
380100030703    0c                   EndIf
380200030703      * Record 'BNB'  -  Spedizione OVER 25 colli
380300030703If  0c                   If        %subst(ArbGva:1:1) = 'O'
380400030703     c                   Clear                   dAr5Bnb
380500030703     c                   Eval      Ktrd = 'BNB'
380600030703     c     kAr51         Chain     Fiar501l
380700030703If  1c                   If        %Found(Fiar501l)
380800030703     c                   Movel     Ar5Uni        Dar5Bnb
380900030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
381000030703      * per i colli originali in caso di spedizione over 25 colli
381100030703     c  n77              Eval      *In77 = *On
381200030703     c                   Eval      VidNba = §Ar5bNba
381300030703     c                   Eval      VidNb2 = §Ar5bNb2
381400030703If  2c                   If        §Ar5bTba <> *Zeros
381500030703     c                   Eval      wtba = §Ar5bTba
381600030703     c                   ExSr      Sr_DecTba
381700030703     c                   Eval      VidTba = wdestba
381800030703    2c                   EndIf
381900030703If  2c                   If        §Ar5bTb2 <> *Zeros
382000030703     c                   Eval      wtba = §Ar5bTb2
382100030703     c                   ExSr      Sr_DecTba
382200030703     c                   Eval      VidTb2 = wdestba
382300030703    2c                   EndIf
382400030703    1c                   EndIf
382500030703    0c                   EndIf
382600911129     C*
382700131120     C**   ARBACC        IFEQ      'S'
382800131120     C**                 MOVEL     'SI'          V5CACC
382900131120     C**                 ELSE
383000131120     C**                 MOVEL     'NO'          V5CACC
383100131120     C**                 END
383200941215     C     ARBICC        IFNE      ' '
383300941216     C     ARBICC        ANDNE     'R'
383400930218     C                   MOVEL     'SI'          VIDICC
383500930218     C                   END
383600911022     C*
383700911022     C                   MOVEL     ARBRMN        VIDRMN
383800940712     C                   MOVEL     ARBRMA        VIDRMA
383900911022     C                   MOVEL     ARBRMO        VIDRMO
384000911022     C                   MOVEL     ARBNCD        VIDNCD
384100911022     C                   MOVEL     ARBNCA        VIDNCA
384200911022     C                   MOVEL     ARBZNC        VIDZNC
384300051104     c                   select
384400051104     c                   when      arbffd='S'
384500051104     C                   MOVEL     'SI '         VIDFFD
384600051104     c                   when      arbffd='M'
384700051104     C                   MOVEL     'SAB'         VIDFFD
384800051104     C                   other
384900051104     C                   MOVEL     '   '         VIDFFD
385000051104     C                   ENDsl
385100051124     c
385200940719     C                   MOVEL     ARBNFV        V3CNFV
385300051124     c* richiamp pgm per decodifica
385400051124     C                   MOVEL     *BLANKS       VIDTCR
385500051124     c                   clear                   vidhcr
385600051124     c                   clear                   viddcr
385700051124     c                   if        arbdcr>0 or arbhcr>0
385800051124     c                   clear                   fnlv80ds
385900051124     c                   eval      ilv80aas=arbaas
386000051124     c                   eval      ilv80lnp=arblnp
386100051124     c                   eval      ilv80nrs=arbnrs
386200051124     c                   eval      ilv80nsp=arbnsp
386300051124     c                   eval      ilv80mgs=arbmgs
386400051124     c                   eval      ilv80tc1=arbtc1
386500051124     c                   eval      ilv80tc2=arbtc2
386600051124     c                   eval      ilv80dcr=arbdcr
386700051124     c                   eval      ilv80hcr=arbhcr
386800051124     c                   eval      ilv80tcr=arbtcr
386900051124     c                   call      'FNLV80R'
387000051124     c                   parm                    kpjba
387100051124     c                   parm                    fnlv80ds
387200051124     c
387300051124     c                   eval      viddcr=olv80dcr8
387400051124     c                   eval      vidtcr=olv80tcrd
387500911023     C                   MOVEL     ARBHCR        VIDHCR
387600051124     c                   endif
387700051124     c
387800070521     c* Se presente ora consegna, la edito
387900070413     c                   clear                   vidhmc
388000070413     c                   if        arbhmc>0
388100070413     c                   eval      vidhmc=%editw(arbhmc: '0 :  ')
388200070413     C****               MOVE      ARBHMC        VIDHMC
388300070413     c                   endif
388400911023     C                   MOVEL     ARBNTC        VIDNTC
388500911023     C                   MOVEL     ARBTMC        VIDTMC
388600940923     C*
388700940923     C* DECODIFICO IL TIPO MANCATA CONSEGNA
388800940923     C                   MOVEL     *BLANKS       V5DTMC
388900940923     C     ARBTMC        IFNE      *BLANKS
389000940923     C                   MOVEL     '2A'          COD
389100940923     C                   MOVEL     *BLANKS       KEY
389200940923     C                   MOVEL     ARBTMC        KEY
389300940923     C     KTAB1         CHAIN     TABEL00F                           31
389400940923     C  N31              MOVEL     TBLUNI        V5DTMC
389500940923     C                   ENDIF
389600940923     C*
389700911023     C                   MOVEL     ARBMIF        VIDMIF
389800911023     C                   MOVEL     ARBNMI        VIDNMI
389900940923     C*
390000940713     C                   MOVEL     ARBGGA        V4CGGA
390100940923     C                   MOVEL     *BLANKS       V4IGGA
390200940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
390300940923     C     V4CGGA        COMP      *BLANKS                            4242
390400940923     C*
390500940713     C                   MOVEL     ARBGMA        V4CGMA
390600940923     C                   MOVEL     *BLANKS       V4IGMA
390700940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
390800940923     C     V4CGMA        COMP      *BLANKS                            4343
390900940923     C*
391000960208     C                   MOVEL     ARBGVA        V4CGVA
391100960208     C                   MOVE      ' '           V4CGVA
391200940923     C                   MOVEL     *BLANKS       V4IGVA
391300940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
391400940923     C     V4CGVA        COMP      *BLANKS                            4444
391500940923     C*
391600070403     C* FLAG blocco CONSEGNA
391700070403     C                   SELECT
391800070403     c                   WHEN      arbfbc='A'
391900070403     c                   eval      v1dfbc='LascAVVISO'
392000070403     C                   SETON                                        27
392100070403     c                   WHEN      arbfbc='B'
392200070403     c  n17              eval      v1dfbc=' BLOCCATA '
392300070412     c   17              eval      v1dfbc=' RIMPALLO '
392400070412     C                   SETON                                        27
392500070403     c                   WHEN      arbfbc='*'
392600070403     c  n17              eval      v1dfbc=' BLOCCATA '
392700070403     c   17              eval      v1dfbc=' GIACENTE '
392800070403     C                   SETON                                        27
392900070403     C                   ENDSL
393000070403     c
393100070403     C**   ARBFBC        COMP      'A'                                    27
393200070403     C**   ARBFBC        COMP      'B'                                    17
393300070403     C**N17ARBFBC        COMP      '*'                                    17
393400940711     C* TIPO CONSEGNA PARTICOLARE 1
393500940804     C                   MOVEL     *BLANKS       V5DTC1
393600940711     C     ARBTC1        IFNE      ' '
393700911115     C                   MOVEL     '1P'          COD
393800911115     C                   MOVEL     *BLANKS       KEY
393900940711     C                   MOVEL     ARBTC1        KEY
394000940713     C     KTAB1         CHAIN     TABEL                              31
394100940804     C  N31              MOVEL     TBLUNI        V5DTC1
394200940804     C   31              MOVEL     *BLANKS       V5DTC1
394300930113     C                   END
394400940711     C* TIPO CONSEGNA PARTICOLARE 2
394500940804     C                   MOVEL     *BLANKS       V5DTC2
394600940804     C                   MOVEL     *BLANKS       V7DTC2
394700940711     C     ARBTC2        IFNE      ' '
394800940711     C                   MOVEL     '1P'          COD
394900940711     C                   MOVEL     *BLANKS       KEY
395000940711     C                   MOVEL     ARBTC2        KEY
395100940713     C     KTAB1         CHAIN     TABEL                              31
395200940804     C  N31              MOVEL     TBLUNI        V5DTC2
395300940804     C   31              MOVEL     *BLANKS       V5DTC2
395400940804     C  N31              MOVEL     TBLUNI        V7DTC2
395500940804     C   31              MOVEL     *BLANKS       V7DTC2
395600940711     C                   END
395700930113     C* TIPO MANCATA CONSEGNA
395800930113     C                   MOVEL     *BLANKS       DESCCA
395900930113     C     ARBCCA        IFNE      ' '
396000930705     C                   Z-ADD     1             C
396100930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
396200930705     C   31              MOVEL     DCA(C)        ALF12            12
396300930705     C  N31              MOVEL     *BLANKS       ALF12
396400930113     C                   MOVEL     '-'           DESCCA
396500930113     C                   MOVE      ALF12         DESCCA
396600930113     C                   END
396700911022     C* GIRO DATA SPEDIZIONE
396800940712     C                   Z-ADD     ARBDSP        G02INV
396900940712     C                   Z-ADD     *ZEROS        G02DAT
397000940712     C                   MOVEL     '3'           G02ERR
397100940712     C                   CALL      'XSRDA8'
397200940712     C                   PARM                    WLBDAT
397300940719     C                   Z-ADD     G02DAT        V3CDSP
397400070403     c*
397500911022     C* DATA BORDERO'
397600070403     c                   if        arbdbr>0
397700070403     c     *iso          movel     arbdbr        dataeur
397800070403     c                   movel     dataeur       datadmy
397900070404     c                   movel     dataeur       w08dbr            8 0
398000070404     c                   movel     datadmy       v06dbr
398100070403     c                   else
398200070404     c                   clear                   v06dbr
398300070403     c                   endif
398400070404     c
398500070404     c* Prendo da fnblp i dati ritiro merce
398600070403     c     karb2         chain     fnblp01l
398700070403     c                   if        %found(fnblp01l) and blpdrt>0
398800070403     c     *iso          movel     blpdrt        dataeur
398900070403     c                   movel     dataeur       datadmy
399000070404     c                   movel     datadmy       v06drt
399100070404     c                   movel     blpnrt        v7cnrt
399200070406     c                   movel     blpctm        v3cctm
399300070404     c                   if        blpfpp='M'
399400070410     c                   eval      v7dfppr='MAT'
399500070404     c                   else
399600070410     c                   eval      v7dfppr='POM'
399700070404     c                   endif
399800070404     C                   MOVEL     BLPPDR        V7CPDR
399900070404     C                   movel     BlpPdr        kpdpdr
400000070404     C                   move      'A'           kpdtip
400100070404     C     kfapd         chain     fiapd01L
400200070404     c                   if        %found(fiapd01l)
400300070404     C                   MOVEL     APDRSC        V7DPDR
400400070404     c                   else
400500070404     C                   MOVEL     *BLANKS       V7DPDR
400600070404     c                   endif
400700070404     c
400800070404     c
400900070403     c                   else
401000070404     c                   clear                   v06drt
401100070404     c                   clear                   v7cnrt
401200070406     c                   clear                   v3cctm
401300070504     c                   clear                   v7dfppr
401400070404     c                   clear                   v7dpdr
401500070404     c                   clear                   v7cpdr
401600070403     c                   endif
401700070403     c
401800961112     C* SOLO PER LE BOLLE EXPORT VISUALIZZO LA DATA STAMPA MANIFEST
401900070404     C                   CLEAR                   V06DT3
402000070413     c                   if        *in16 and arbdt3>0  and arbdt3<>99999999
402100070404     c     *iso          movel     arbdt3        dataeur
402200070404     c                   movel     dataeur       datadmy
402300070404     c                   movel     datadmy       v06dt3
402400961112     C                   ENDIF
402500940713     C* DATA PARTENZA PRIMO COLLO
402600070404     c                   if        arbdpc>0
402700070404     c     *iso          movel     arbdpc        dataeur
402800070404     c                   movel     dataeur       datadmy
402900070404     c                   movel     datadmy       v06dpc
403000070404     c                   movel     dataeur       w08dpc
403100070404     c                   else
403200070404     c                   clear                   v06dpc
403300070404     c                   clear                   w08dpc            8 0
403400070404     c                   endif
403500940713     C* DATA PARTENZA ULTIMO COLLO
403600070403     C**                 Z-ADD     ARBDUC        G02INV
403700070403     C**                 Z-ADD     *ZEROS        G02DAT
403800070403     C**                 MOVEL     '3'           G02ERR
403900070403     C**                 CALL      'XSRDA8'
404000070403     C**                 PARM                    WLBDAT
404100070403     C**                 Z-ADD     G02DAT        V3CDUC
404200070403     c                   if        arbduc>0
404300070404     c     *iso          movel     arbduc        dataeur
404400070403     c                   movel     dataeur       datadmy
404500070404     c                   movel     datadmy       v06duc
404600070404     c                   movel     dataeur       w08duc
404700070403     c                   else
404800070404     c                   clear                   v06duc
404900070404     c                   clear                   w08duc            8 0
405000070403     c                   endif
405100911022     C* DATA ENTRATA AL TRANSITO
405200070404     c                   if        arbdet>0
405300070404     c     *iso          movel     arbdet        dataeur
405400070404     c                   movel     dataeur       datadmy
405500070404     c                   movel     datadmy       v06det
405600070404     c                   else
405700070404     c                   clear                   v06det
405800070404     c                   endif
405900911022     C* DATA USCITA DAL TRANSITO
406000070404     c                   if        arbdut>0
406100070404     c     *iso          movel     arbdut        dataeur
406200070404     c                   movel     dataeur       datadmy
406300070404     c                   movel     datadmy       v06dut
406400070404     c                   else
406500070404     c                   clear                   v06dut
406600070404     c                   endif
406700930113     C* DATA ARRIVO MERCE
406800070403     c                   if        arbdam>0
406900070403     c     *iso          movel     arbdam        dataeur
407000070403     c                   movel     dataeur       datadmy
407100070404     c                   movel     datadmy       v06dam
407200070403     c                   else
407300070404     c                   clear                   v06dam
407400070403     c                   endif
407500131121
407600930504     C* DATA CONSEGNA MERCE REALE
407700070404     c                   if        arbdcm>0
407800070413     C                   SETON                                        69
407900070404     c     *iso          movel     arbdcm        dataeur
408000070404     c                   movel     dataeur       datadmy
408100070404     c                   movel     dataeur       v08dcm
408200070404     c                   movel     datadmy       v06dcm
408300070404     c                   else
408400070404     c                   clear                   v06dcm
408500070404     c                   clear                   v08dcm
408600070404     c                   endif
408700131121
408800930505     C* DATA CONSEGNA PREVISTA ESTERNA/ORE DI RITARDO
408900070404     C                   Z-ADD     0             v08DCE
409000140113     C                   Z-ADD     0             amgDCE            8 0
409100070404     C                   Z-ADD     0             v06dce
409200940711     C                   Z-ADD     0             VIDNRC
409300131121     C*******            SETON                                        68
409400131121     C                   MOVEL     ' '           D98TLA
409500131121     C                   MOVEL     'A'           D98TBO
409600131121     C                   MOVEL     ARBAAS        D98AAS
409700131121     C                   MOVEL     ARBLNP        D98LNP
409800131121     C                   MOVEL     ARBNRS        D98NRS
409900131121     C                   MOVEL     ARBNSP        D98NSP
410000131121     C*
410100131121     C                   CALL      'TNSD99R'
410200131121     C                   PARM                    TNSD99DS
410300140110     c
410400140110     c* Mi salvo data teorica consegna
410500140110     c                   eval      s_arbdei=d98dei
410600140110     c                   eval      s_arboei=d98oei
410700150728     c
410800150728     c* memorizzo ore trazione è ore consegna cliente da passare al trulorsr
410900150728     c                   eval      s_arbttc=d98ttc
411000150728     c                   eval      s_arbtcc=d98tcc
411100151019     c                   eval      s_arbflp=d98flp
411200131121     C*
411300140110    1C     ARBDCE        IFEQ      0
411400131121    2c                   if        d98dce>0
411500140113     c                   eval      amgdce=d98dce
411600131121     c     *iso          movel     d98dce        dataeur
411700131121     c                   movel     dataeur       datadmy
411800131121     c                   movel     dataeur       v08dce
411900131121     c                   movel     datadmy       v06dce
412000131121   x2c                   else
412100131121     c                   clear                   v08dce
412200131121     c                   clear                   v06dce
412300131121    2c                   endif
412400131121     c
412500131121     C                   Z-ADD     D98NRC        VIDNRC
412600131121     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
412700131121     C*   CONSEGNA PREVISTA
412800131121     c  n69              eval      vidhmc=%editw(d98hce: '0 :  ')
412900131121     C*
413000131121     C* SE COMODO = 1  E' STATA FATTA CALL PGM
413100131121     C                   MOVEL     '1'           PA098
413200131121   x1C                   ELSE
413300140113     c                   eval      amgdce=arbdce
413400070404     c     *iso          movel     arbdce        dataeur
413500070404     c                   movel     dataeur       datadmy
413600070404     c                   movel     dataeur       v08dce
413700070404     c                   movel     datadmy       v06dce
413800070404     c
413900940711     C                   Z-ADD     ARBNRC        VIDNRC
414000131121    1C                   END
414100131121     c
414200911023     C* DATA MANDATO INTROITO
414300940712     C                   Z-ADD     ARBDMI        G02INV
414400940712     C                   Z-ADD     *ZEROS        G02DAT
414500940712     C                   MOVEL     '3'           G02ERR
414600940712     C                   CALL      'XSRDA8'
414700940712     C                   PARM                    WLBDAT
414800940712     C                   Z-ADD     G02DAT        VIDDMI
414900930113     C* DATA PRIMA CONSEGNA PARZIALE
415000930113     C                   Z-ADD     0             VIDDCP
415100930113     C     ARBDCP        IFGT      0
415200940712     C                   Z-ADD     ARBDCP        G02INV
415300940712     C                   Z-ADD     *ZEROS        G02DAT
415400940712     C                   MOVEL     '3'           G02ERR
415500940712     C                   CALL      'XSRDA8'
415600940712     C                   PARM                    WLBDAT
415700940712     C                   Z-ADD     G02DAT        VIDDCP
415800930113     C                   END
415900911023     C* DATA DISTINTA
416000940712     C                   Z-ADD     ARBDDC        G02INV
416100940712     C                   Z-ADD     *ZEROS        G02DAT
416200940712     C                   MOVEL     '3'           G02ERR
416300940712     C                   CALL      'XSRDA8'
416400940712     C                   PARM                    WLBDAT
416500940712     C                   Z-ADD     G02DAT        VIDDDC
416600911022     C* CODICE PADRONCINO CONSEGNA
416700940712     C                   MOVEL     ARBPDC        VIDPDC
416800940712     C                   MOVEL     *BLANKS       DESPDC
416900940712     C                   MOVEL     *BLANKS       DESPD1
417000940712     C     ARBPDC        IFGT      0
417100021203     C                   movel     arbpdc        kpdpdr
417200021203     C                   move      'A'           kpdtip
417300021203     C     kfapd         chain     fiapd01L                           31
417400940714     C  N31              MOVEL     APDRSC        DESPDC
417500940714     C  N31              MOVEL     APDRSC        DESPD1
417600930705     C                   END
417700950324     C* PRESTAZIONE PADRONCINO MATTINO O POMERIGGIO
417800950324     C                   MOVE      *BLANKS       FVVFPP
417900131120     C***                MOVE      *BLANKS       V5DFPP
418000070404     C                   MOVE      *BLANKS       V7DFPP
418100950327     C                   Z-ADD     ARBNDC        WNDC
418200020906     c                   Move      Arbifp        Wfgs
418300020906     c     Kfvv1         Chain     Fnfvv01l                           31
418400950324     C                   SELECT
418500991027     C                   WHEN      *IN31 = *on
418600991027     C                   WHEN      FVVFPP = 'M'
418700131120     C**                 eval      V5DFPP = 'MATTINO'
418800070504     C                   eval      V7DFPP = 'Mat'
418900991027     C                   WHEN      FVVFPP = 'P'
419000131120     C**                 eval      V5DFPP = 'POMERIGGIO'
419100070504     C                   eval      V7DFPP = 'Pom'
419200950324     C                   ENDSL
419300131119     c* controllo se ha PDA
419400131119     C                   Z-ADD     ARBNDC        kNDC6
419500131120     c                   clear                   vidpda
419600131120     c     Kdst          Chain     Fidst01l
419700131120     c                   if        %found(fidst01l)
419800131119     C                   movel     dstflr        DDStflr
419900131120     c                   if        (§dsttstpda= 'C' or  §dsttstpda= 'E') or
420000131120     c                             (dstpda<>'C' and  dstpda<>'E')
420100131120     c                   eval      vidpda='NO PDA'
420200131120     c                   movel     ri            ATRpda
420300131121     c                   else
420400131121     c                   eval      vidpda='SI PDA'
420500131121     c                   movel     ri            ATRpda
420600131119     c                   endif
420700131119     c                   endif
420800131108     c
420900131108     c* Se bolla in distinta, verifico quanti colli
421000131108     c                   clear                   v7dncl_co
421100131115     c                   clear                   v7dncl_de
421200131108     c                   clear                   wcol_D
421300131108     c                   clear                   wcol_I
421400131108    1c                   if        arbndc>0 and arbddc>0 and arbfbc=' '
421500131108     c* se ritorno all'incasso lo scrivo
421600131108     c
421700131108    2c                   if        arbdcm>0  and (arbicc='R' or arbicc=' ' or
421800131108     c                                  arbica=' ' or arbica='R')
421900131115     c                   eval      v7dncl_de='     IN consegna :'
422000131108     c                   eval      v7dncl_co='PER INCASSO'
422100131108    2c                   endif
422200131108     c
422300131108     c* Se colli da consegnare
422400131108    2c                   if        arbdcm=0
422500131108     C                   Z-ADD     ARBNDC        knfv
422600131108     c                   Move      Arbifp        Wfgs
422700131108     c
422800131108     c     karb2         setll     fnart01l
422900131108     c     karb2         reade     fnart01l
423000131108     c
423100131108    3c                   dow       not %eof(fnart01l)
423200131108     c                   if        artdcm=0
423300131108     c                   add       1             wcol_d
423400131108     c     kbrv          chain     fnbrv01l
423500131108     c                   if        %found(fnbrv01l)
423600131108     c                   add       1             wcol_I
423700131108     c                   endif
423800131108     c                   endif
423900131108     c
424000131108     c
424100131108     c     karb2         reade     fnart01l
424200131108    3c                   enddo
424300131108
424400131108    3c                   if        wcol_d>0
424500131115     c                   eval      v7dncl_de='Colli in consegna:'
424600131108    4c                   if        wcol_i=0
424700131108     c                   eval      v7dncl_co='0/'+%trim(%editc(wcol_d:'Z'))
424800131108     c                   else
424900131108     c                   eval      v7dncl_co=%trim(%editc(wcol_I:'Z')) +'/' +
425000131108     c                                       %trim(%editc(wcol_d:'Z'))
425100131108    4c                   endif
425200131108    3c                   endif
425300131108    c
425400131108    2c                   endif
425500131108    1c                   endif
425600131108     c
425700131108     c* Visualizzo ultimo evento
425800131108     c                   clear                   v7duevb
425900131114     c* prima vedo se c'e' da PDA
426000131114     c                   move      arbndc        pctndc
426100140224     c                   move      arbifp        pctfgs
426200131114     c                   move      'CET'         ttrd
426300131114     c     kcet          setgt     fipct02l
426400131114     c     kcet          readpe    fipct02l
426500140310     c                   if        not %eof(fipct02l)
426600131114     c                   movel     pctdati       fipctcetds
426700131114     c* consegnata
426800131114     c                   if        §pctcmc = ' '
426900131114     c                   MOVE      '  C'         evbcev                         *causale evento
427000131114     c                   else
427100131114     c* mancata consegna
427200131114     c                   movel     §PCTcmc       evbcev
427300131114     c                   endif
427400131114     c                   z-add     §pctdata      evbdev
427500131114     c                   movel     §pctora       evbhev
427600131114
427700131114     c                   exsr      Decodi_EVB
427800131114
427900131114     c                   else
428000131114     c*
428100131108     C                   Z-ADD     ARBAAS        KAAS$
428200131108     C                   Z-ADD     ARBLNP        KLNP
428300131108     C                   Z-ADD     ARBNRS        KNRS
428400131108     C                   Z-ADD     ARBNSP        KNSP
428500131108     C     KEVB1         SETgt     FNEVB01L
428600131108     C     KEVB1         readpe    FNEVB01L
428700131108     c
428800131114    1c                   if        not %eof(fnevb01l)
428900131114     c                   exsr      Decodi_EVB
429000131114    1c                   endif
429100131114    1c                   endif
429200911022     C*
429300911022     C* SE ESISTE AGGANCIO 2° BOLLA
429400991026     C                   Z-ADD     ARBAAS        KAAS
429500991026     C                   Z-ADD     ARBLNP        KLNP
429600991026     C                   Z-ADD     ARBNRS        KNRS
429700991026     C                   Z-ADD     ARBNSP        KNSP
429800991026     C                   MOVEL     '2'           KTRC
429900991027     C     KAR61         CHAIN     FIAR6000                           31
430000911023     C*
430100040216if  1C                   IF          %FOUND
430200991027     C                   eval      *in22 = *on
430300991027     C                   MOVEL     AR6CEI        V9CCEI
430400991027     C                   MOVEL     AR6NFT        V9CNFT
430500991027     C                   MOVEL     AR6FIV        V9CFIV
430600991027      *
430700991027     C                   eval      V9CDIV = AR6div
430800991027     C                   eval      WDIV = AR6div
430900991027     C                   exsr      CTR_VALUTA
431000991027      *
431100001128     C                   clear                   v9dfim
431200001128     C                   clear                   v9dfi2
431300991027     c                   if        AR6fim = 'P'
431400991027     c                   eval      V9Dfim = 'PARTENZA'
431500040126     c                   eval      V9Dfi2 = 'Immesse in'
431600991027     c                   endif
431700991027      * Varia G
431800040130     c                   clear                   W0133
431900040130     c                   clear                   v9csv1
432000040130     c                   clear                   v9cva1
432100040130     c                   clear                   v9csv2
432200040130     c                   clear                   v9cva2
432300040130     c                   clear                   v9csv3
432400040130     c                   clear                   v9cva3
432500040130     c                   clear                   v9csv4
432600040130     c                   clear                   v9cva4
432700041130     c                   clear                   v9csv5
432800041130     c                   clear                   v9cva5
432900041130     c                   clear                   v9csv6
433000041130     c                   clear                   v9cva6
433100040130     c                   clear                   v9civa
433200040130
433300040122      * Carico le varie
433400040122     c                   Movel     Ar6Sv1        wsvn
433500040122     c                   Z-add     Ar6Va1        wvan
433600040122     c                   ExSr      Car_Varie1
433700040122
433800040122     c                   Movel     Ar6Sv2        wsvn
433900040122     c                   Z-add     Ar6Va2        wvan
434000040122     c                   ExSr      Car_Varie1
434100040122
434200040122     c                   Movel     Ar6Sv3        wsvn
434300040122     c                   Z-add     Ar6Va3        wvan
434400040122     c                   ExSr      Car_Varie1
434500040122
434600040122     c     Kar61         Setll     Fiar7000
434700040216do  2c                   Do        *Hival
434800040122     c     Kar61         Reade     Fiar7000
434900040122     c                   If        %Eof(Fiar701l)
435000040122     c                   Leave
435100040122     c                   EndIf
435200040122     c                   Movel     Ar7Svn        wsvn
435300040122     c                   Z-add     Ar7Van        wvan
435400040216     c                   ExSr      Car_Varie1
435500040216e   2c                   EndDo
435600040122
435700991027      * TOTALE FATTURA
435800991027     c                   clear                   W0133
435900991027     c                   IF          AR6ift > *zeros
436000991027     c                   z-add     AR6ift        W0133
436100991027     c                   eval      Wtstdec = §cvfdc
436200991027     c                   exsr      EDTimporti
436300991027     c                   movel     IMPeditato    V9cift
436400001128     C                   else
436500001128     C                   clear                   v9cift
436600991027     c                   endif
436700991027      *
436800991027     c                   If         AR6dft > *zeros
436900991027     C     *iso          movel     AR6DFT        DATA_EUR
437000991027     C     *eur          movel     data_eur      W0080
437100991027     C                   eval      V9cdft = %EDITC(W0080:'Y')
437200001128     C                   else
437300001128     C                   clear                   v9cdft
437400991027     c                   Endif
437500991027      *
437600991027     c                   eval      V9Cksc = AR6ksc
437700991027     c                   eval      V9Cctr = AR6ctr
437800991027     c                   move      v9cksc        W0040
437900991027     c                   If        W0040 = 9999
438000991027     c                   eval      V9Dksc = arbrsd
438100991027     c                   Else
438200991027     c                   eval      i69kac = V9Cksc
438300991027     c                   exsr      CTR_ANAGRA
438400991027     c                   eval      V9Dksc = acorag
438500991027     c                   Endif
438600991027      *
438700991027     C                   eval      COD = 'TR'
438800991027     C                   movel     V9CCTR        UNOCTR            1
438900991027     C                   eval      KEY = unoctr
439000991027     C     KTAB1         CHAIN     TABEL00F                           31
439100991027     C                   movel     TBLUNI        V9DCTR
439200991027      *
439300930114     C* ESENZIONE IVA
439400001128     C                   clear                   v9dcei
439500991111     C     AR6CEI        IFNE      *blanks
439600940721     C                   MOVEL     'EI'          COD
439700991111     C                   MOVEL(P)  AR6CEI        KEY
439800940719     C     KTAB1         CHAIN     TABEL00F                           31
439900991027     C  N31              MOVEL     TBLUNI        V9DCEI
440000940719     C                   ENDIF
440100991027      *
440200040216e   1C                   ENDIF
440300930525     C*
440400991027     C* PER I DATI SULLA TASSAZIONE 1a BOLLA, DEVO CERCARE SU FIAR600F
440500940905     C                   SETOFF                                       24
440600110705     c                   clear                   atrbl4a
440700110705     c
440800110705     c                   clear                   v6car4ksc
440900110705     c                   clear                   v6dar4ksc
441000110705     c                   clear                   v6car4ctr
441100111021     c                   clear                   VIDimv
441200111021     c                   clear                   VIDpor
441300111021     c                   clear                   VIDift
441400111021     c                   clear                   VIDdiv
441500111021     c                   clear                   VIDfim
441600111021     C                   clear                   VIDCEI
441700111021     C                   clear                   VIDNFT
441800111021     C                   clear                   VIDFIV
441900111021     C                   clear                   DESCEI
442000111021     C                   clear                   viddft
442100111021     C                   clear                   vidIVA
442200111021      *
442300111021      * CERCO LE VARIE: LE PRIME 3 SU FIAR600F, EVENTUALI  ALTRE SU FIAR700F
442400111021     C                   eval      *in35 = *on
442500131107     C                   WRITE     LRI6C06
442600111021     C                   eval      *in35 = *off
442700111021      *
442800111021     c                   clear                   NR6
442900111021     c                   clear                   VIDiva
443000110705     c
443100940712     C                   Z-ADD     ARBAAS        KAAS
443200940712     C                   Z-ADD     ARBLNP        KLNP
443300940712     C                   Z-ADD     ARBNRS        KNRS
443400940712     C                   Z-ADD     ARBNSP        KNSP
443500110705     C                   MOVEL     'A'           KTRC
443600110705     C     KAR41         CHAIN     FiAR401L
443700110705if  1C                   IF        not %FOUND
443800110705     c                   eval      atrbl4a=nodi
443900110705     c                   else
444000110705     c                   movel     ar4not        dsbl4a
444100110705     c                   if        §b4aks>0
444200110705     C                   eval      *in24 = *on
444300110705     c                   eval      v6car4ksc=§b4aks
444400110705     c                   eval      i69kac = v6car4ksc
444500110705     c                   exsr      CTR_ANAGRA
444600110705     c                   eval      v6dar4ksc = acorag
444700110705     c                   if        §b4act<>*blanks
444800110705     c                   movel     §b4act        v6car4ctr
444900110705     c                   endif
445000110705     c* non c'e' cod cliente x tassaz.
445100110705     c                   else
445200110705     c                   eval      atrbl4a=nodi
445300110705     c                   endif
445400110705     c                   endif
445500110705     c
445600991026     C                   MOVEL     '1'           KTRC
445700991027     C     KAR61         CHAIN     FIAR6000
445800110705     c
445900110705     c                   if        %found(fiar601l)
446000110705     c
446100110705      * CLIENTE E TARIFFA FATTURAZIONE
446200110705     c                   IF        *IN33 = *ON
446300110705     c                   eval      v6cksc = arbksc
446400110705     c                   eval      v6cctr = arbctr
446500110705     c                   eval      v6dksc = arbrsm
446600110705     c                   ELSE
446700110705     c                   eval      v6cksc = AR6ksc
446800110705     c                   eval      v6cctr = AR6ctr
446900110705     c                   move      v6cksc        W0040
447000110705     c                   if        W0040 = 9999
447100110705     c                   eval      v6dksc = arbrsd
447200110705     c                   else
447300110705     c                   eval      i69kac = v6cksc
447400110705     c                   exsr      CTR_ANAGRA
447500110705     c                   eval      v6dksc = acorag
447600110705     c                   endif
447700110705     c                   ENDIF
447800110705      *   decodifica tariffa
447900110705     C                   eval      COD = 'TR'
448000110705     C                   MOVEL     V6CCTR        UNOCTR            1
448100110705     C                   MOVEL(P)  UNOCTR        KEY
448200110705     C     KTAB1         CHAIN     TABEL00F                           31
448300110705     C                   MOVEL     TBLUNI        V6DCTR
448400110705     c
448500110705     c                   endif
448600991027      *
448700040216if  1C                   IF        %FOUND  and
448800991027     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
448900991027     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
449000991027     C                              AR6sv3 <> *blanks)
449100991027      *
449200991027      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SE PRESENTE ALMENO UNO DEI CAMPI
449300991027     C                   EVAL      *IN24 = *ON
449400991027      *
449500991027     C                   eval      VIDDIV = AR6div
449600991027     C                   eval      WDIV = AR6div
449700991027     C                   exsr      CTR_VALUTA
449800991027      *
449900991027     c                   IF          ar6por > *zeros
450000991027     c                   z-add     AR6por        W0133
450100991027     c                   eval      Wtstdec = §cvfdc
450200991027     c                   exsr      EDTimporti
450300991027     c                   movel     IMPeditato    VIDpor
450400991128     c                   else
450500991128     c                   clear                   VIDpor
450600991027     c                   ENDIF
450700991027      *
450800991027     c                   IF          ar6imv > *zeros
450900991027     c                   z-add     AR6imv        W0133
451000991027     c                   eval      Wtstdec = §cvfdc
451100991027     c                   exsr      EDTimporti
451200991027     c                   movel     IMPeditato    VIDimv
451300991128     c                   else
451400991128     c                   clear                   VIDimv
451500991027     c                   ENDIF
451600991027      *
451700991027     c                   IF          ar6ift > *zeros
451800991027     c                   z-add     AR6ift        W0133
451900991027     c                   eval      Wtstdec = §cvfdc
452000991027     c                   exsr      EDTimporti
452100991027     c                   movel     IMPeditato    VIDift
452200991128     c                   else
452300991128     c                   clear                   VIDift
452400991027     c                   ENDIF
452500991027      *
452600991027     c                   IF        AR6fim = 'P'
452700991027     c                   eval      VIDfim = 'PARTENZA'
452800991128     c                   else
452900991128     c                   clear                   VIDfim
453000991027    2C                   ENDIF
453100991027      *
453200991027     C                   MOVEL     AR6CEI        VIDCEI
453300991027     C                   MOVEL     AR6NFT        VIDNFT
453400991027     C                   MOVEL     AR6FIV        VIDFIV
453500991027      *
453600991027     C                   IF        AR6CEI <>  *BLANKS
453700940808     C                   MOVEL     'EI'          COD
453800991027     C                   MOVEL(P)  AR6CEI        KEY
453900940808     C     KTAB1         CHAIN     TABEL00F                           31
454000940808     C  N31              MOVEL     TBLUNI        DESCEI
454100991128     c                   else
454200991128     c                   clear                   DESCEI
454300940808     C                   ENDIF
454400991027      *
454500991027      * DATA FATTURA DA GIRARE
454600991027     C                   IF         ar6dft > *zeros
454700991027     C     *iso          movel     AR6DFT        DATA_EUR
454800991027     C     *eur          movel     data_eur      W0080
454900991027     C                   eval      VIDdft = %EDITC(W0080:'Y')
455000991128     c                   else
455100991128     c                   clear                   VIDdft
455200991027     C                   ENDIF
455300991027      *
455400991027      *
455500991027     C                   MOVEL     ar6SV1        WSVN
455600991027     C                   MOVEL     ar6VA1        WVAN
455700991027     c                   exsr      car_varie
455800991027      *
455900991027     C                   MOVEL     ar6SV2        WSVN
456000991027     C                   MOVEL     ar6VA2        WVAN
456100991027     c                   exsr      car_varie
456200991027      *
456300991027     C                   MOVEL     ar6SV3        WSVN
456400991027     C                   MOVEL     ar6VA3        WVAN
456500991027     c                   exsr      car_varie
456600991027      *
456700991027      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
456800040216if  2C                   if          ar6SV3 <> *blanks
456900991027     C     KAR61         SETLL     FIAR7000
457000991027     C     KAR61         READE     FIAR7000
457100040216do  3C                   DOW       %EOF = *OFF
457200991027     C                   MOVEL     AR7SVN        WSVN
457300991027     C                   MOVEL     AR7VAN        WVAN
457400991027     c                   exsr      car_varie
457500991027     C     KAR61         READE     FIAR7000
457600040216e   3C                   ENDDO
457700040216e   2C                   endif
457800110705e   1C                   ENDIF
457900991027      *
458000991027      * Se non ho caricato record ne carico uno vuoto
458100110705     c                   IF        NR6 = *ZEROS  and *in24
458200991027     C                   clear                   V6CDE1
458300991027     C                   clear                   V6CVA1
458400991027     C                   clear                   V6CSV1
458500991027     C                   add       1             NR6
458600131107     C                   WRITE     LRI6S06
458700991027     c                   ENDIF
458800991027      *
458900991027      *
459000941206     C* 70 ON - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
459100941206     C     *IN33         IFEQ      '0'
459200941206     C     AR6NFT        ANDGT     0
459300941206     C     *IN22         OREQ      '1'
459400991111     C     AR6NFT        ANDGT     0
459500941206     C                   SETON                                        70
459600941206     C                   END
459700940712     C*
459800940719     C* PRELEVO LE NOTE DAL FILE APPOSITO
459900940719     C                   MOVEL     ARBAAS        KAAS
460000940719     C                   MOVEL     ARBLNP        KLNP
460100940719     C                   MOVEL     ARBNRS        KNRS
460200940719     C                   MOVEL     ARBNSP        KNSP
460300940719     C                   MOVEL     '8'           KTRC
460400060216     C     KAR41         CHAIN     FiAR401L                           31
460500940719     C  N31              MOVEL     AR4NOT        V4CNO1
460600940802     C   31              MOVEL     *BLANKS       V4CNO1
460700940719     C                   MOVEL     '9'           KTRC
460800060216     C     KAR41         CHAIN     FiAR401L                           31
460900940719     C  N31              MOVEL     AR4NOT        V4CNO2
461000940802     C   31              MOVEL     *BLANKS       V4CNO2
461100940719     C*
461200941110     C* ESTENSIONE RAGIONE SOCIALE DESTINATARIO
461300941110     C                   MOVEL     'D'           KTRC
461400060216     C     KAR41         CHAIN     FiAR401L                           31
461500950125     C  N31              MOVEL     AR4NOT        V4CNOT
461600950125     C   31              MOVEL     *BLANKS       V4CNOT
461700971015     C*
461800971015     C* SOLO PER EXPORT DATA DI STAMPA SU BORDERO' ARRIVI
461900070404     C***  *IN16         IFEQ      *ON
462000070404     C***                MOVEL     'A'           KTRC
462100070404     C***  KAR41         CHAIN     FiAR401L                           31
462200070404     C**N31              MOVE      AR4NOT        WDATA             8
462300070404     C***31              MOVEL     *ZEROS        WDATA
462400070404     C***  WDATA         IFGT      *ZEROS
462500070404     C***                MOVEL     WDATA         G02INV
462600070404     C***                Z-ADD     *ZEROS        G02DAT
462700070404     C***                MOVEL     '3'           G02ERR
462800070404     C***                CALL      'XSRDA8'
462900070404     C***                PARM                    WLBDAT
463000070404     C***                Z-ADD     G02DAT        V3CDST
463100070404     C***                ENDIF
463200070404     C***                ENDIF
463300940720     C*
463400051114     C* IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU FiAR900F
463500940720     C* CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
463600940720     C                   MOVEL     *BLANKS       VIDTIC
463700940720     C                   MOVEL     *ZERO         VIDCAS
463800940720     C                   MOVEL     *BLANKS       VIDVCA
463900940720     C                   MOVEL     *BLANKS       V4CGCA
464000940923     C                   MOVEL     *BLANKS       V4IGCA
464100091214     c                   setoff                                       65
464200940720     C     §3AFCA        IFGT      *ZERO
464300051128     C     Ksped         CHAIN     FiAR901L                           31
464400940720     C  N31              DO
464500940720     C                   MOVEL     AR9TIC        VIDTIC
464600940720     C                   MOVEL     AR9CAS        VIDCAS
464700940720     C                   MOVEL     AR9VCA        VIDVCA
464800940720     C                   MOVEL     AR9GCA        V4CGCA
464900091214     c                   seton                                        65
465000940720     C                   ENDDO
465100940720     C                   ENDIF
465200941215     C* ESISTE CONTRASSEGNO: VEDO SE ABILITATO
465300941215     C     VIDCAS        IFNE      0
465400941215     C                   SETON                                        29
465500941215     C                   END
465600940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
465700940923     C     V4CGCA        COMP      *BLANKS                            4141
465800940720     C*
465900940720     C* RIEMPO ANCHE ALCUNI CAMPI PER LE DS DI PASSAGGIO PARAMETRI
466000940720     C                   MOVEL     V3CDSP        PA1DSP
466100940720     C                   MOVEL     ARBRMN        PA1RMN
466200940720     C                   MOVEL     ARBLNA        PA1LNA
466300940802     C                   MOVEL     V3DLNA        PA1LN1
466400940720     C                   MOVEL     ARBRSD        PA1RSD
466500940906     C*-------------------------------------------------------------------------
466600991029      * GESTIONE INCASSO PARZIALE se assegnato o presente estensione bolla
466700991102     C                   clear                   v5cdiv
466800991102     c                   clear                   Wariimp
466900991102     c                   clear                   Wariipp
467000991102     c                   clear                   VIDimp
467100991102     c                   clear                   VIDipp
467200991102     C                   MOVEL     ARBFIP        VIDFIP
467300991102      *
467400991029     c  N33              eval      V5Cdiv = viddiv
467500991029     c   22              eval      V5Cdiv = v9cdiv
467600991029      *
467700040216if  1c                   if        V5CDIV <> *blanks
467800991029      *
467900991029     C                   eval      WDIV = V5Cdiv
468000991029     C                   exsr      CTR_VALUTA
468100991029      *
468200991029     C                   MOVEL     ARBAAS        KAAS
468300991029     C                   MOVEL     ARBLNP        KLNP
468400991029     C                   MOVEL     ARBNRS        KNRS
468500991029     C                   MOVEL     ARBNSP        KNSP
468600991029     C                   MOVEL     'A'           ktrc
468700991029      *
468800991029     c     kar61         setll     fiari000
468900991029     c     kar61         reade     fiari000
469000991029      *
469100040216do  2c                   DOW       not %EOF
469200040216if  3c                   if        v5cdiv = aridiv
469300991029     c                   add       ARIimp        Wariimp
469400991029     c                   add       ARIipp        Wariipp
469500040216x   3c                   else
469600991029      *
469700991029     C                   clear                   yeurcods
469800991029     C                   eval      YECDVI = aridiv
469900991029     C                   eval      YECDVO = V5Cdiv
470000991029     C                   eval      YECIMI = ARIimp
470100011022     C                   move      §CVDEC        YECDCO
470200991029     C                   CALL      'YEURCO'
470300991029     C                   PARM                    YEURcods
470400991029    6C                   if        YECESI = *blanks
470500991029     C                   add       YECIMO        WARIimp
470600991029    6C                   endif
470700991029      *
470800991029     C                   clear                   yeurcods
470900991029     C                   eval      YECDVI = aridiv
471000991029     C                   eval      YECDVO = V5Cdiv
471100991029     C                   eval      YECIMI = ARIipp
471200011022     C                   move      §CVDEC        YECDCO
471300991029     C                   CALL      'YEURCO'
471400991029     C                   PARM                    YEURcods
471500991029    6C                   if        YECESI = *blanks
471600991029     C                   add       YECIMO        WARIipp
471700991029    6C                   endif
471800991029      *
471900040216e   3c                   endif
472000991029      *
472100991029     c     kar61         reade     fiari000
472200040216e   2c                   enddo
472300991029      *
472400991029     c                   IF        WARIipp > *zeros
472500991029     c                   z-add     Wariipp       W0133
472600991029     c                   eval      Wtstdec = §cvfdc
472700991029     c                   exsr      EDTimporti
472800991029     c                   movel     IMPeditato    VIDipp
472900991029     c                   ENDIF
473000991029      *
473100991029     c                   IF        WARIimp > *zeros
473200991029     c                   z-add     Wariimp       W0133
473300991029     c                   eval      Wtstdec = §cvfdc
473400991029     c                   exsr      EDTimporti
473500991029     c                   movel     IMPeditato    VIDimp
473600991029     c                   ENDIF
473700991105      *
473800991105     c                   IF        VIDipp = *blanks  and  VIDimp = *blanks
473900991105     c                   clear                   V5CDIV
474000991105     c                   ENDIF
474100991029      *
474200040216e   1C                   endif
474300991029     C*-------------------------------------------------------------------------
474400951220     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
474500951220     C*
474600951220     C* PULISCO IL SFL
474700960926     C                   SETOFF                                       72
474800131107     C                   WRITE     LRI6C05
474900960926     C                   SETON                                        7172
475000951220     C*
475100060216     C* CHIAVE PER ACCEDERE AL FILE FiAR401L
475200951220     C                   MOVEL     ARBAAS        KAAS
475300951220     C                   MOVEL     ARBLNP        KLNP
475400951220     C                   MOVEL     ARBNRS        KNRS
475500951220     C                   MOVEL     ARBNSP        KNSP
475600951220     C*
475700951220     C                   Z-ADD     0             NRR5              4 0
475800951220     C                   MOVEL     '7K'          COD
475900951220     C*
476000060216     C     KAR42         SETLL     FiAR401L
476100060216     C     KAR42         READE     FiAR401L                               36
476200040216do  1C     *IN36         DOWEQ     *OFF
476300951220     C*
476400951220     C                   MOVEL(P)  AR4TRC        KEY
476500951220     C     KTAB1         CHAIN     TABEL                              36
476600951220     C*
476700951220     C     *IN36         IFEQ      *OFF
476800951220     C     TBLFLG        ANDEQ     ' '
476900951220     C                   MOVEL     TBLUNI        DS7K
477000951220     C     §7KVIS        IFEQ      'S'
477100951220     C                   MOVEL     §7KDES        VS5DES
477200951220     C                   MOVEL     AR4NOT        VS5NOT
477300951220     C                   ADD       1             NRR5
477400131107     C                   WRITE     LRI6S05
477500951220     C                   ENDIF
477600951220     C                   ENDIF
477700951220     C*
477800060216     C     KAR42         READE     FiAR401L                               36
477900040216e   1C                   ENDDO
478000951220     C*
478100960926     C* SE NON HO CARICATO NIENTE NON EMETTO IL SFLDSP PER EVITARE CHE
478200960926     C*  IL PGM DIA ERRORE DI SESSIONE O UNITA'
478300951220     C     NRR5          IFEQ      0
478400991105     C                   clear                   VS5DES
478500991105     C                   clear                   VS5NOT
478600991105     C                   ADD       1             NRR5
478700131107     C                   WRITE     LRI6S05
478800951220     C                   ENDIF
478900951220     C*
479000021216      * Riempio il campone in fondo alla videata con i tasti scelti
479100021216     C                   exsr      Ges_TastiFun
479200021217     c                   eval      FD07 = FDxx
479300021217     c                   eval      F24D07 = F24Dxx
479400911022     C                   ENDSR
479500131114      *-------------------------------------------------------------------------
479600131114      *  Decodifica EVENTO
479700131114      *-------------------------------------------------------------------------
479800131114     c     Decodi_evb    BEGSR
479900131114
480000131114     c                   if        evbcev='  C'
480100131114     c                   eval      §2ades='CONSEGNA'
480200131128     c* aggiungo anche firmatario
480300131128     c                   if        §PCTNOMFIR<>*blaNKS
480400131128     C                   EVAL      §2ADES=%TRIM(§2ADES)+' Firma:' +§PCTNOMFIR
480500131128     c                   endif
480600131128     c
480700131114     C                   else
480800131114     C                   MOVEL     '2A'          COD
480900131114     C                   MOVEL(p)  evbcev        KEY
481000131114     C     KTAB1         CHAIN     TABEL00F
481100131114    3c                   if        not %found(tabel00f)
481200131114     c                   clear                   ds2a
481300131114     c                   else
481400131114     c                   eval      ds2a=tbluni
481500131114    3c                   endif
481600131114    3c                   endif
481700131114     c
481800131114     c                   eval      v7duevb=evbcev+'-'+§2ades
481900131114
482000131114    3c                   if        evbdev>0
482100131114     c     *iso          movel     evbdev        dataeur
482200131114     c                   movel     dataeur       datadmy
482300131114     c                   movel     dataeur       w7cdev            8 0
482400131114
482500131114     c                   eval      indx=%checkr(' ':v7duevb)
482600131114    4c                   if        indx>46
482700131114     c                   eval      indx=46
482800131114     c                   else
482900131114     c                   eval      indx=indx+1
483000131114    4c                   endif
483100131114     c                   eval      wcarat=66-indx
483200131114     c                   eval      %subst(v7duevb:indx:wcarat)=' ' +'il ' +
483300131114     c                             %editw(W7cdev: '  /  /    ')
483400131114     c
483500131114    4c                   if        evbhev>0
483600131114     c                   eval      v7duevb=%trim(v7duevb)+' ' +
483700131114     c                                                 %editw(evbhev: '0 :  ')
483800131114    4c                   endif
483900131114     c
484000131114    3c                   endif
484100131114     c                   ENDSR
484200030703      *-------------------------------------------------------------------------
484300030703      *  Decoditifa tipo bancali
484400030703      *-------------------------------------------------------------------------
484500030703     c     Sr_DecTba     BegSr
484600030703     c                   Clear                   Tibs02Ds
484700030703     c                   Clear                   dTba
484800030703     c                   Movel     'C'           T02Mod
484900030703     c                   Movel     Knsif         T02Sif
485000030703     c                   Movel     'TBA'         T02Cod
485100030703     c                   Movel(p)  wtba          T02Ke1
485200030703     c                   Call      'TIBS02R'
485300030703     c                   Parm                    Kpjba
485400030703     c                   Parm                    Tibs02Ds
485500030703If  3c                   If        T02Err = *Blanks
485600030703     c                   Movel     T02Uni        dTba
485700030703    3c                   EndIf
485800030703     c                   Eval      wdestba = §TbaDesc
485900030703     c                   EndSr
486000021216      *-------------------------------------------------------------------------
486100021216      *  Imposto descrizione tasti funzione
486200021216      *-------------------------------------------------------------------------
486300021216     c     Ges_TastiFun  BEGSR
486400021217      *
486500021217      * per emettere i tasti senza condizionamenti ..
486600021216     c                   setoff                                       89
486700021216      *
486800021216      * Conta i caratteri riempiti dalle RigaTf1
486900021217     c                   z-add     0             $z                3 0
487000021216      *
487100021216      * Conta i caratteri riempiti dalle RigaTf2
487200021217     c                   z-add     0             $k                3 0
487300021216      *
487400021216      * Conta i caratteri riempiti dalle RigaTf3
487500021217     c                   z-add     0             $j                3 0
487600021216      *
487700021216      * Conta la posizione del campone da cui partire per impostare
487800021216      * la descrizione del tasto funzione
487900021216     c                   z-add     0             $y                2 0
488000021216      *
488100021217      * Stringhe che contengono le descrizioni dei tasti funzione
488200021216     c                   reset                   RigaTf1
488300021216     c                   reset                   RigaTf2
488400021216     c                   reset                   RigaTf3
488500070411      *
488600070411      *?F01?--> scelta bolla da assistenza clienti
488700070411     c                   IF        *in63 = *on
488800070411     c                   reset                   $Tf
488900070411     c                   movea     Tf01          $Tf
489000070411     c                   exsr      Rie_$Tf
489100070411     c                   else
489200070411      *?F01?--> scelta bolla generica
489300070411     c                   if        *in61 = *on
489400070411     c                   reset                   $Tf
489500070411     c                   movea     Tf01B         $Tf
489600070411     c                   exsr      Rie_$Tf
489700070411     c                   ENDIF
489800070411     c                   ENDIF
489900021216      *
490000070411      * F13 --> tassazione 1 bolla
490100021216     c                   IF        *in24 = *on
490200021217     c                   reset                   $Tf
490300021217     c                   movea     Tf13          $Tf
490400021217     c                   Exsr      Rie_$Tf
490500021217     C                   ENDIF
490600021216      *
490700070411      * F04 --> tassazione 2 bolla
490800021216     c                   IF        *in22 = *on
490900021217     c                   reset                   $Tf
491000021217     c                   movea     Tf04          $Tf
491100021217     c                   Exsr      Rie_$Tf
491200021217     C                   ENDIF
491300040220      *
491400070411      *?F16 --> assistenza clienti
491500040220     c                   IF        *in64 = *on
491600040220     c                   reset                   $Tf
491700040220     c                   movea     Tf16          $Tf
491800040220     c                   exsr      Rie_$Tf
491900040220     c                   ENDIF
492000021216      *
492100070411      * F05 --> eventi
492200021216     c                   IF        *in95 = *on
492300021217     c                   reset                   $Tf
492400021217     c                   movea     Tf05          $Tf
492500021217     c                   Exsr      Rie_$Tf
492600021217     C                   ENDIF
492700021216      *
492800021216      * F10
492900021217     c                   IF        *in98 = *on
493000021217     c                   reset                   $Tf
493100021217     c                   movea     Tf10          $Tf
493200021217     c                   Exsr      Rie_$Tf
493300021217     c                   ENDIF
493400021216      *
493500021216      * F14
493600021217     c                   IF        *in03 = *on
493700021217     c                   reset                   $Tf
493800021217     c                   movea     Tf14          $Tf
493900021217     c                   Exsr      Rie_$Tf
494000021217     c                   ENDIF
494100021216      *
494200021216      * F02
494300021217     c                   IF        *in04 = *on
494400021217     c                   reset                   $Tf
494500021217     c                   movea     Tf02          $Tf
494600021217     c                   Exsr      Rie_$Tf
494700021217     c                   ENDIF
494800110929      *
494900110930      * F22 --> Perfect Order
495000110929     c                   IF        *in86 = *on
495100110929     c                   reset                   $Tf
495200110929     c                   movea     Tf22          $Tf
495300110929     c                   Exsr      Rie_$Tf
495400110929     C                   ENDIF
495500021216      *
495600021216      * F11
495700021217     c                   IF        *in77 = *on
495800021217     c                   reset                   $Tf
495900021217     c                   movea     Tf11          $Tf
496000021217     c                   Exsr      Rie_$Tf
496100021217     c                   ENDIF
496200070404      *
496300070413      * F17 --> solo se partito com GEO
496400070413     c                   if        *in17
496500070404     c                   reset                   $Tf
496600070404     c                   movea     Tf17          $Tf
496700070404     c                   Exsr      Rie_$Tf
496800070413     c                   endif
496900021216      *
497000021216      * F07
497100021217     c                   IF        *in67 = *on
497200021217     c                   reset                   $Tf
497300021217     c                   movea     Tf07          $Tf
497400021217     c                   Exsr      Rie_$Tf
497500021217     c                   ENDIF
497600021216      *
497700021216      * F08
497800021217     c                   IF        *in74 = *on and *in16 = *on
497900021217     c                   reset                   $Tf
498000021217     c                   movea     Tf08          $Tf
498100021217     c                   Exsr      Rie_$Tf
498200021217     c                   ENDIF
498300130830
498400130830      * F09 --> Orari servizi non condizionato
498500131007     c                   IF        *in89 = *off and wabif9='S'
498600130930     c                   reset                   $Tf
498700130930     c                   movea     Tf09          $Tf
498800130930     c                   Exsr      Rie_$Tf
498900130930     c                   ENDIF
499000021216      *
499100021216      * F20 --> non condizionato
499200021217     c                   IF        *in89 = *off
499300021217     c                   reset                   $Tf
499400021217     c                   movea     Tf20          $Tf
499500021217     c                   Exsr      Rie_$Tf
499600021217     c                   ENDIF
499700021216      *
499800021216      * F21 --> non condizionato
499900021216     c                   IF        *in89 = *off
500000021217     c                   reset                   $Tf
500100021217     c                   movea     Tf21          $Tf
500200021217     c                   Exsr      Rie_$Tf
500300021217     c                   ENDIF
500400021218      *
500500021218      * F19 --> non condizionato
500600021218     c                   IF        *in89 = *off
500700021218     c                   reset                   $Tf
500800021218     c                   movea     Tf19          $Tf
500900021218     c                   Exsr      Rie_$Tf
501000021218     c                   ENDIF
501100070420     c
501200130830      * F06 --> solo se partito con GEO
501300070420     c                   if        *in17
501400070420     c                   reset                   $Tf
501500070420     c                   movea     Tf06          $Tf
501600070420     c                   Exsr      Rie_$Tf
501700070420     c                   ENDIF
501800021217      *
501900021217      * F12 --> non condizionato
502000021217     c                   IF        *in89 = *off
502100021217     c                   reset                   $Tf
502200021217     c                   movea     Tf12          $Tf
502300021217     c                   Exsr      Rie_$Tf
502400021217     c                   ENDIF
502500021216      *
502600021217      * Pulisco le stringa
502700021216     c                   z-add     1             $h
502800030121     c     1             do        62            $h                2 0
502900021216     C                   if        RigaTf1($h) = '#'
503000021216     c                   movel     *blanks       RigaTf1($h)
503100021216     c                   endif
503200021216     c                   enddo
503300021216     c                   z-add     1             $h
503400030121     c     1             do        62            $h                2 0
503500021216     C                   if        RigaTf2($h) = '#'
503600021216     c                   movel     *blanks       RigaTf2($h)
503700021216     c                   endif
503800021216     c                   enddo
503900021216     c                   z-add     1             $h
504000030121     c     1             do        62            $h                2 0
504100021216     C                   if        RigaTf3($h) = '#'
504200021216     c                   movel     *blanks       RigaTf3($h)
504300021216     c                   endif
504400021216     c                   enddo
504500021217      *
504600021216      * Imposto la descrizione del tasto funzione F24 e segnalo
504700021216      * quante righe ho riempito e quale devo emettere
504800021216     c                   SELECT
504900021216      *
505000021216     c                   when      $k<>*zeros and $Y<>*zeros
505100021216     c                   eval      $loop = 3
505200021216     c                   z-add     3             $loop             1 0
505300021217     C                   movel(p)  CF2413        F24Dxx           16
505400021216      *
505500021216     c                   when      $k<>*zeros and $Y=*zeros
505600021216     c                   z-add     2             $loop             1 0
505700021217     C                   movel(p)  CF2412        F24Dxx
505800021216      *
505900021216     c                   Other
506000021216     c                   z-add     1             $loop             1 0
506100021217     C                   movel     *blanks       F24Dxx
506200021216     c                   ENDSL
506300021216      * Prima riga di tasti funzione
506400030121     c                   movea     RigaTF1       FDxx             62
506500021216     c                   z-add     1             $Rig              1 0
506600021216      *
506700021216     c                   endsr
506800021217      *-------------------------------------------------------------------------
506900021217      *  Riempio le stringhe con i campi funzione compattati
507000021217      *-------------------------------------------------------------------------
507100021217     c     Rie_$Tf       begsr
507200021217      *
507300021217     c                   z-add     1             $x                2 0
507400021217     c     '#'           lookup    $Tf($x)                                31
507500021217     c                   add       $x            $Z
507600030121     c                   IF        $Z <= 63
507700021217     c                   eval      $j = $Z - $x + 1
507800021217     c                   movea     $Tf           RigaTF1($j)
507900021217     c                   ELSE
508000021217     c                   add       $x            $K
508100030121     c                   IF        $K <= 63
508200021217     c                   eval      $j = $K - $x + 1
508300021217     c                   movea     $Tf           RigaTF2($j)
508400021217     c                   ELSE
508500021217     c                   add       $x            $Y
508600030121     c                   IF        $Y <= 63
508700021217     c                   eval      $j = $Y - $x + 1
508800021217     c                   movea     $Tf           RigaTF3($j)
508900021217     c                   ENDIF
509000021217     c                   ENDIF
509100021217     c                   ENDIF
509200021217      *
509300021217     c                   endsr
509400021217      *-------------------------------------------------------------------------
509500021217      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
509600021217      *-------------------------------------------------------------------------
509700021217     c     Ges_F24       begsr
509800021217      *
509900021217     c                   SELECT
510000021217      *
510100021217     c                   WHEN      $Loop = 2 and $Rig =2  or
510200021217     c                             $Loop = 3 and $Rig =3
510300021217     c                   movea     RigaTF1       FDxx
510400021217     c                   z-add     1             $Rig
510500021217      *
510600021217     c                   if        $loop = 2
510700021217     c                   eval      F24Dxx= Cf2412
510800021217     c                   else
510900021217     c                   eval      F24Dxx= Cf2413
511000021217     c                   endif
511100021217      *
511200021217     c                   WHEN      $Loop = 2 and $Rig =1 or
511300021217     c                             $Loop = 3 and $Rig =1
511400021217     c                   movea     RigaTF2       FDxx
511500021217     c                   z-add     2             $Rig
511600021217      *
511700021217     c                   if        $loop = 2
511800021217     c                   eval      F24Dxx= Cf2422
511900021217     c                   else
512000021217     c                   eval      F24Dxx= Cf2423
512100021217     c                   endif
512200021217      *
512300021217     c                   WHEN      $Loop = 3 and $Rig =2
512400021217     c                   movea     RigaTF3       FDxx
512500021217     c                   z-add     3             $Rig
512600021217     c                   eval      F24Dxx= Cf2433
512700021217     c                   ENDSL
512800021217      *
512900021217     c                   endsr
513000021217      *-------------------------------------------------------------------------
513100991027      *  CARICO SUBFILE VARIE
513200991027      *-------------------------------------------------------------------------
513300991027     c     CAR_VARIE     BEGSR
513400991128      *
513500991027      *
513600991027     c                   SELECT
513700991027      *
513800991027     c                   WHEN       WSVN = *BLANKS
513900991027      *
514000991027     C                   WHEN       WSVN = §$2iva
514100991027     c                   z-add     Wvan          W0133
514200991027     c                   eval      Wtstdec = §cvfdc
514300991027     c                   exsr      EDTimporti
514400991027     c                   movel     IMPeditato    VIDiva
514500991027      *
514600991027     c                   OTHER
514700991027     c                   z-add     Wvan          W0133
514800991027     c                   eval      Wtstdec = §cvfdc
514900991027     c                   exsr      EDTimporti
515000991027     c                   movel     IMPeditato    V6Cva1
515100991027     c                   movel     WSVN          V6Csv1
515200991027     C                   Z-ADD     1             WA
515300991027     C     V6CSV1        LOOKUP    CC1(WA)                                99
515400020329     C     CC3(WA)       ifeq      'S'
515500020329      *
515600020329      * Verifico se si tratta di una bolla Fedex
515700021119     c                   if        LnaNtw = 'FED'
515800020329     C                   Z-ADD     1             WA
515900020329     C     V6CSV1        LOOKUP    CB1(WA)                                99
516000020329     C   99              MOVEL     CB2(WA)       V6CDE1
516100020329     C  N99              clear                   V6CDE1
516200020329     C                   else
516300991027     C   99              MOVEL     CC2(WA)       V6CDE1
516400991027     C  N99              MOVEL     *BLANKS       V6CDE1
516500020329     C                   endif
516600020329     C                   else
516700020329     C   99              MOVEL     CC2(WA)       V6CDE1
516800020329     C  N99              MOVEL     *BLANKS       V6CDE1
516900020329     C                   endif
517000991027     C                   add       1             NR6
517100131107     C                   WRITE     LRI6S06
517200991027      *
517300991027     c                   ENDSL
517400991027      *
517500991027     c                   ENDSR
517600040122      *-------------------------------------------------------------------------
517700040122      *  Carico le varie della seconda bolla
517800040122      *-------------------------------------------------------------------------
517900040122     c     Car_Varie1    BegSr
518000040122
518100040122s   1c                   Select
518200040122
518300040122     c                   When      wsvn = *Blanks
518400040122      * Iva
518500040122     c                   When      wsvn = §$2iva
518600040122     c                   z-add     wvan          w0133
518700040122     c                   Eval      Wtstdec = §cvfdc
518800040122     c                   ExSr      EDTimporti
518900040122     c                   Movel     IMPeditato    V9cIva
519000070411     c                   setoff                                       06
519100070411      * Bollo
519200070411     c                   When      wsvn = §$2bol
519300070411     c                   z-add     wvan          w0133
519400070411     c                   Eval      Wtstdec = §cvfdc
519500070411     c                   ExSr      EDTimporti
519600070411     c                   Movel     IMPeditato    V9cIva
519700070411     c                   seton                                        06
519800040122      * Varia G - sempre per prima
519900040122     c                   When      wsvn = §$2vrg
520000040122     c                   Eval      V9cSv1 = wsvn
520100040122     c                   z-add     wvan          w0133
520200040122     c                   Eval      Wtstdec = §cvfdc
520300040122     c                   ExSr      EDTimporti
520400040122     c                   Movel     IMPeditato    V9cVa1
520500040122      * Altre varie
520600040122   x1c                   Other
520700040122     c                   z-add     wvan          W0133
520800040122     c                   eval      Wtstdec = §cvfdc
520900040122     c                   exsr      EDTimporti
521000040122if  2c                   If        V9cSv2 = *Blanks
521100040122     c                   movel     IMPeditato    V9cVa2
521200040122     c                   movel     wsvn          V9csv2
521300040122   x2c                   Else
521400040122if  3c                   If        V9cSv3 = *Blanks
521500040122     c                   movel     IMPeditato    V9cVa3
521600040122     c                   movel     wsvn          V9csv3
521700040122   x3c                   Else
521800040122if  4c                   If        V9cSv4 = *Blanks
521900040122     c                   movel     IMPeditato    V9cVa4
522000040122     c                   movel     wsvn          V9csv4
522100041130   x4c                   Else
522200041130if  5c                   If        V9cSv5 = *Blanks
522300041130     c                   movel     IMPeditato    V9cVa5
522400041130     c                   movel     wsvn          V9csv5
522500041130   x5c                   Else
522600041130if  6c                   If        V9cSv6 = *Blanks
522700041130     c                   movel     IMPeditato    V9cVa6
522800041130     c                   movel     wsvn          V9csv6
522900041130    6c                   EndIf
523000041130    5c                   EndIf
523100040122    4c                   EndIf
523200040122    3c                   EndIf
523300040122    2c                   EndIf
523400040122
523500040122    1c                   EndSl
523600040122
523700040122     c                   EndSr
523800991027      *****************************************************************
523900991027      *  AGGANCIO ANAGRAFICHE
524000991027      *****************************************************************
524100991027     C     CTR_ANAGRA    BEGSR
524200991027      *
524300991027     C                   clear                   DS_cnaco
524400991027     C                   clear                   DS_cnind
524500991027     C                   clear                   DS_cnclp
524600991027     C                   clear                   DS_fncls
524700991027      *
524800991027     C                   CALL      'TIBS69R'
524900991027     C                   PARM                    tibs69DS
525000991027     C                   PARM                    DS_cnaco
525100991027     C                   PARM                    DS_cnind
525200991027     C                   PARM                    DS_cnclp
525300991027     C                   PARM                    DS_fncls
525400991027      *
525500991027     C                   eval      wtibs69r = 'S'
525600991027      * per il 1° giro è inizializzata nelle specifiche "D"
525700991027     C                   clear                   TIBS69DS
525800991027      *
525900991027     C                   ENDSR
526000991027      **********************************************************************
526100991027      * AGGANCIO TABELLA VALUTE
526200991027      **********************************************************************
526300991027     C     CTR_VALUTA    BEGSR
526400991027
526500991027     C                   MOVEL     'CV'          COD
526600991027     C                   MOVEL(P)  WDIV          KEY
526700991027     C     KTAB1         CHAIN     TABEL00F                           31
526800991027     C  N31              MOVEL     TBLUNI        DSCV
526900991027
527000991027     C                   ENDSR
527100991027      **********************************************************************
527200991027      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
527300991027      **********************************************************************
527400991027     C     EDTIMPORTI    BEGSR
527500991027
527600991027      * ------------------------------------------------------------------------
527700991027      * W0133      contiene il numero da editare per il video
527800991027      * WTSTDEC    corrisponde a §CVfdc della tabella CV
527900991027      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
528000991027      * ------------------------------------------------------------------------
528100991027      *
528200991027     c                   clear                   IMPeditato
528300991027     c                   clear                   W017A
528400991027      *
528500991027     c                   if        Wtstdec = 'S'
528600991027     C                   eval      W017a = %EDITC(W0133:'2')
528700991027     c                   else
528800991027     c                   z-add     W0133         W0130
528900991027     C                   eval      W017a = %EDITC(W0130:'2')
529000991027     c                   endif
529100991027      *
529200991027     c                   move      W017a         IMPeditato
529300991027      *
529400991027     c                   ENDSR
529500940802     C**************************************************************************
529600940802     C*    LEGAMI BOLLA
529700940802     C**************************************************************************
529800930413     C     LEGBOL        BEGSR
529900940802     C*
530000940912     C* DEVO MEMORIZZARE LA BOLLA POICHE DEVO RIAGGANCIARLA DOPO
530100940912     C                   MOVEL     ARBAAS        WAAS
530200940912     C                   MOVEL     ARBLNP        WLNP
530300940912     C                   MOVEL     ARBNRS        WNRS
530400940912     C                   MOVEL     ARBNSP        WNSP
530500940912     C*
530600940907     C* PULISCO IL SFL
530700950825     C                   SETOFF                                       787923
530800131107     C                   WRITE     LRI6C08
530900950825     C                   SETON                                        7923
531000940923     C                   MOVEL     *BLANKS       LEGAME
531100940907     C*
531200940907     C                   SETOFF                                       94
531300950718     C                   Z-ADD     *ZERO         NR8
531400940907     C                   Z-ADD     *ZERO         SAVNR8            4 0
531500940907     C                   MOVEL     *BLANKS       WFLG1             1
531600940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
531700940907     C*
531800940907     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
531900940907     C     KARB          CHAIN     FNLBL01L                           99
532000940907     C   99KARB          CHAIN     FNLBL02L                           99
532100940907     C  N99              DO
532200940907     C*
532300940907     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
532400940907     C                   MOVEL     LBLAAO        KAAS
532500940907     C                   MOVEL     LBLLPO        KLNP
532600940907     C                   MOVEL     LBLNRO        KNRS
532700940907     C                   MOVEL     LBLNSO        KNSP
532800940907     C     KLBL3         CHAIN     FNLBL02L                           99
532900950103     C     *IN99         IFEQ      *ON
533000950103     C                   MOVEL     'N'           WORIGI            1
533100950103     C                   ELSE
533200950103     C                   MOVEL     'S'           WORIGI
533300950103     C                   ENDIF
533400940907     C*
533500940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
533600940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
533700950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
533800950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
533900940907     C                   MOVEL     ' '           V8CSCE
534000940907     C                   MOVEL     LBLAAO        V8CAAP
534100940907     C                   MOVEL     LBLLPO        V8CLPP
534200940907     C                   MOVEL     LBLLAO        V8CLAP
534300940907     C                   MOVEL     LBLNRO        V8CNRP
534400940907     C                   MOVEL     LBLNSO        V8CNSP
534500940926     C                   CLEAR                   V8CAAN
534600940926     C                   CLEAR                   V8CLPN
534700940926     C                   CLEAR                   V8CLAN
534800940926     C                   CLEAR                   V8CNRN
534900940926     C                   CLEAR                   V8CNSN
535000940907     C*
535100940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
535200940907     C     LBLAAO        IFEQ      V3CAAS
535300940907     C     LBLLPO        ANDEQ     V3CLNP
535400940907     C     LBLNRO        ANDEQ     V3CNRS
535500940907     C     LBLNSO        ANDEQ     V3CNSP
535600940907     C                   MOVEL     *BLANKS       V8CPRE
535700940907     C                   SETON                                        80
535800940907     C                   SETON                                        81
535900940907     C                   MOVEL     '1'           WFLG1
536000940907     C                   ELSE
536100940907     C                   SETOFF                                       81
536200940907     C*
536300950316     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
536400950316     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
536500950316     C                   SETOFF                                       93
536600950316     C     KLBL3         SETLL     FNARB01L                               93
536700950316     C  N93              MOVEA     PRD(1)        V8CPRE
536800950316     C  N93              SETON                                        80
536900950316     C   93              MOVEA     PRD(2)        V8CPRE
537000950316     C   93              SETOFF                                       80
537100950316     C*
537200950316     C* SE NON TROVATA SU ARB PROVO SU BLP
537300950316     C  N93              DO
537400950316     C                   SETOFF                                       93
537500950316     C     KLBL3         SETLL     FNBLP01L                               93
537600010712      *
537700010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
537800010712     c                   if        *in93 = *off
537900010712     C                   MOVEA     PRD(1)        V8CPRE
538000010712     c                   else
538100950316     C* SE RICHIAMATO DALLE PARTENZE (FNLS05R) NON POSSO
538200950316     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
538300010712     C     PA1FLG        IFEQ      '3'
538400960903     C     PA1FLG        OREQ      'N'
538500950316     C                   MOVEA     PRD(3)        V8CPRE
538600950316     C                   SETON                                        80
538700950316     C                   ELSE
538800950316     C                   MOVEA     PRD(2)        V8CPRE
538900950316     C                   SETOFF                                       80
539000950316     C                   ENDIF
539100010712     C                   ENDIF
539200950316     C*
539300950316     C                   ENDDO
539400940907     C*
539500940907     C                   ENDIF
539600940907     C                   SETON                                        83
539700950103     C*
539800950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
539900950103     C                   MOVEL     *IN80         V8CI80
540000950103     C                   MOVEL     *IN81         V8CI81
540100950103     C                   MOVEL     *IN82         V8CI82
540200950103     C                   MOVEL     *IN83         V8CI83
540300950103     C*
540400940907     C                   Z-ADD     1             NR8
540500131107     C                   WRITE     LRI6S08
540600950825     C                   SETON                                        78
540700950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
540800940907     C*
540900950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
541000950103     C*                      2) NON HO TROVATO L'ORIGINALE
541100950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
541200950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
541300950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
541400950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
541500950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
541600950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
541700950103     C* L'ORIGINALE E LA MAMMA.
541800950103     C     WORIGI        IFEQ      'N'
541900950103     C                   MOVEL     *BLANKS       V8CSCE
542000950103     C                   CLEAR                   V8CAAP
542100950103     C                   CLEAR                   V8CLPP
542200950103     C                   CLEAR                   V8CLAP
542300950103     C                   CLEAR                   V8CNRP
542400950103     C                   CLEAR                   V8CNSP
542500950103     C                   MOVEL     LBLAAP        V8CAAN
542600950103     C                   MOVEL     LBLLPP        V8CLPN
542700950103     C                   MOVEL     LBLLAP        V8CLAN
542800950103     C                   MOVEL     LBLNRP        V8CNRN
542900950103     C                   MOVEL     LBLNSP        V8CNSN
543000950103     C                   SETOFF                                       8182
543100950103     C*
543200950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
543300950103     C     V8CAAN        IFEQ      V3CAAS
543400950103     C     V8CLPN        ANDEQ     V3CLNP
543500950103     C     V8CNRN        ANDEQ     V3CNRS
543600950103     C     V8CNSN        ANDEQ     V3CNSP
543700950103     C                   MOVEL     *BLANKS       V8CPRE
543800950103     C                   SETON                                        80
543900950103     C                   SETON                                        82
544000950103     C                   ELSE
544100950103     C*
544200950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
544300950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
544400950316     C                   EXSR      CHKBOL
544500950103     C*
544600950103     C                   ENDIF
544700950103     C*
544800950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
544900950103     C                   SETOFF                                       83
545000950103     C*
545100950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
545200950103     C                   MOVEL     *IN80         V8CI80
545300950103     C                   MOVEL     *IN81         V8CI81
545400950103     C                   MOVEL     *IN82         V8CI82
545500950103     C                   MOVEL     *IN83         V8CI83
545600950103     C*
545700950103     C                   ADD       1             NR8
545800131107     C                   WRITE     LRI6S08
545900950103     C*
546000950103     C                   ENDIF
546100940907     C                   SETOFF                                       83  99
546200940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
546300940907     C* CICLO FINO ALL'ULTIMO LEGAME
546400940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
546500940907     C                   Z-ADD     *ZERO         WA                3 0
546600940907     C                   Z-ADD     *ZERO         WB                3 0
546700940907     C                   MOVEA     *ZERO         AAN
546800940907     C     *IN99         DOWEQ     *OFF
546900940907     C*
547000940907     C                   MOVEL     ' '           V8CSCE
547100940907     C                   MOVEL     LBLAAP        V8CAAP
547200940907     C                   MOVEL     LBLLPP        V8CLPP
547300940907     C                   MOVEL     LBLLAP        V8CLAP
547400940907     C                   MOVEL     LBLNRP        V8CNRP
547500940907     C                   MOVEL     LBLNSP        V8CNSP
547600940907     C                   MOVEL     LBLAAN        V8CAAN
547700940907     C                   MOVEL     LBLLPN        V8CLPN
547800940907     C                   MOVEL     LBLLAN        V8CLAN
547900940907     C                   MOVEL     LBLNRN        V8CNRN
548000940907     C                   MOVEL     LBLNSN        V8CNSN
548100940907     C                   SETOFF                                       8182
548200940907     C*
548300950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
548400940907     C     LBLAAP        IFEQ      V3CAAS
548500940907     C     LBLLPP        ANDEQ     V3CLNP
548600940907     C     LBLNRP        ANDEQ     V3CNRS
548700940907     C     LBLNSP        ANDEQ     V3CNSP
548800940907     C                   SETON                                        81
548900940907     C                   ENDIF
549000940907     C*
549100940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
549200940907     C     LBLAAN        IFEQ      V3CAAS
549300940907     C     LBLLPN        ANDEQ     V3CLNP
549400940907     C     LBLNRN        ANDEQ     V3CNRS
549500940907     C     LBLNSN        ANDEQ     V3CNSP
549600940907     C                   MOVEL     *BLANKS       V8CPRE
549700940907     C                   SETON                                        80
549800940907     C                   SETON                                        82
549900940907     C                   ELSE
550000940907     C*
550100940907     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
550200940907     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
550300950316     C                   EXSR      CHKBOL
550400940907     C*
550500940907     C                   ENDIF
550600950103     C*
550700950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
550800950103     C                   MOVEL     *IN80         V8CI80
550900950103     C                   MOVEL     *IN81         V8CI81
551000950103     C                   MOVEL     *IN82         V8CI82
551100950103     C                   MOVEL     *IN83         V8CI83
551200940907     C*
551300940907     C* SCRIVO IL RECORD NEL SFL
551400940907     C                   ADD       1             NR8
551500131107     C                   WRITE     LRI6S08
551600940907     C*
551700940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
551800940907     C                   ADD       1             WA
551900940907     C                   MOVEL     LBLAAN        AAN(WA)
552000940907     C                   MOVEL     LBLLPN        LPN(WA)
552100940907     C                   MOVEL     LBLNRN        NRN(WA)
552200940907     C                   MOVEL     LBLNSN        NSN(WA)
552300940907     C*
552400940907     C     KLBL3         READE     FNLBL02L                               99
552500940907     C*
552600940907     C* SE NON HO PIU' BOLLE CON READE
552700940907     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
552800970116     C     WB            DOWLT     WA
552900970116     C     *IN99         ANDEQ     *ON
553000940907     C                   ADD       1             WB
553100940907     C                   MOVEL     AAN(WB)       KAAS
553200940907     C                   MOVEL     LPN(WB)       KLNP
553300940907     C                   MOVEL     NRN(WB)       KNRS
553400940907     C                   MOVEL     NSN(WB)       KNSP
553500940907     C     KLBL3         SETLL     FNLBL02L
553600940907     C     KLBL3         READE     FNLBL02L                               99
553700970116     C                   ENDDO
553800940907     C*
553900940907     C                   ENDDO
554000940907     C                   ENDDO
554100940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
554200940907     C*
554300940907     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
554400940907     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
554500940907     C     NR8           IFEQ      2
554600940907     C*
554700940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
554800131107     C     2             CHAIN     LRI6S08                            99
554900940907     C     V8CAAP        IFEQ      V3CAAS
555000940907     C     V8CLPP        ANDEQ     V3CLNP
555100940907     C     V8CNRP        ANDEQ     V3CNRS
555200940907     C     V8CNSP        ANDEQ     V3CNSP
555300940907     C                   MOVEL     V8CAAN        COMAAN
555400940907     C                   MOVEL     V8CLPN        COMLPN
555500940907     C                   MOVEL     V8CNRN        COMNRN
555600940907     C                   MOVEL     V8CNSN        COMNSN
555700950307     C                   MOVEL     V8CLAN        COMLAN
555800950316     C                   MOVEA     'SEGUEN.'     LBL(14)
555900940907     C                   ELSE
556000940907     C                   MOVEL     V8CAAP        COMAAN
556100940907     C                   MOVEL     V8CLPP        COMLPN
556200940907     C                   MOVEL     V8CNRP        COMNRN
556300940907     C                   MOVEL     V8CNSP        COMNSN
556400950307     C                   MOVEL     V8CLAP        COMLAN
556500950316     C                   MOVEA     'PRECED.'     LBL(14)
556600940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
556700940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
556800940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
556900131107     C     1             CHAIN     LRI6S08                            99
557000940926     C* REIMPOSTO NR8=2 PERCHE' MI SERVE DOPO
557100940926     C                   Z-ADD     2             NR8
557200940907     C                   ENDIF
557300940907     C*
557400950316     C                   MOVEA     COMLPN        LBL(21)
557500950316     C                   MOVEA     COMNRN        LBL(25)
557600950316     C                   MOVEA     COMNSN        LBL(28)
557700950316     C                   MOVEA     COMLAN        LBL(40)
557800940907     C*
557900940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
558000940907     C                   Z-ADD     1             W                 1 0
558100940907     C     V8CPRE        LOOKUP    PRD(W)                                 99
558200950307     C                   MOVEA     LBD(W)        LBL(44)
558300940907     C     W             COMP      2                                      94
558400940907     C*
558500940907     C                   MOVEA     LBL           LEGAME
558600940907     C                   ELSE
558700940907     C*
558800940907     C     NR8           IFGT      2
558900940907     C                   MOVEA     LB2           LEGAME
559000940907     C                   SETON                                        94
559100940907     C                   ENDIF
559200940907     C                   ENDIF
559300940907     C*
559400940907     C     NR8           COMP      1                                    3434
559500940907     C*
559600940907     C                   Z-ADD     NR8           SAVNR8
559700940907     C                   Z-ADD     1             NR8
559800940923     C     LEGAME        COMP      *BLANKS                            9292
559900940912     C*
560000940912     C* RIAGGANCIO LA BOLLA IN ESAME
560100940912     C                   MOVEL     WAAS          ARBAAS
560200940912     C                   MOVEL     WLNP          ARBLNP
560300940912     C                   MOVEL     WNRS          ARBNRS
560400940912     C                   MOVEL     WNSP          ARBNSP
560500940912     C     KARB2         CHAIN     FNARB01L                           99
560600930511     C*
560700930413     C                   ENDSR
560800950316     C**************************************************************************
560900950316     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
561000950316     C**************************************************************************
561100950316     C     CHKBOL        BEGSR
561200950316     C*
561300950316     C                   SETOFF                                       93
561400950316     C     KLBL1         SETLL     FNARB01L                               93
561500950316     C  N93              MOVEA     PRD(1)        V8CPRE
561600950316     C  N93              SETON                                        80
561700950316     C   93              MOVEA     PRD(2)        V8CPRE
561800950316     C   93              SETOFF                                       80
561900950316     C*
562000950316     C* SE NON TROVATA SU ARB PROVO SU BLP
562100950316     C  N93              DO
562200950316     C                   SETOFF                                       93
562300950316     C     KLBL1         SETLL     FNBLP01L                               93
562400010712      *
562500010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
562600010712     c                   if        *in93 = *off
562700010712     C                   MOVEA     PRD(1)        V8CPRE
562800010712     c                   else
562900950316     C* SE RICHIAMATO DALLE PARTENZE (FNLS05R) NON POSSO
563000950316     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
563100010712     C     PA1FLG        IFEQ      '3'
563200960903     C     PA1FLG        OREQ      'N'
563300950316     C                   MOVEA     PRD(3)        V8CPRE
563400950316     C                   SETON                                        80
563500950316     C                   ELSE
563600950316     C                   MOVEA     PRD(2)        V8CPRE
563700950316     C                   SETOFF                                       80
563800950316     C                   ENDIF
563900010712     C                   ENDIF
564000950316     C*
564100950316     C                   ENDDO
564200950316     C*
564300950316     C                   ENDSR
564400940802     C**************************************************************************
564500940802     C*    VISUALIZZA BOLLA
564600940802     C**************************************************************************
564700911023     C     VISUAL        BEGSR
564800940321      *
564900940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
565000131108     c     Emriepil      tag
565100940712     C                   EXSR      RIEPIL
565200940712      *
565300131108      * PER F15 PASSO ALLA BOLLA, PER INVIO o F12 RITORNO AL SFL
565400940712     C  NKP              GOTO      ENDVIS
565500931112      *
565600931112     C                   SETOFF                                       89
565700940802     C                   MOVEL     'S'           WRIEM             1
565800021217      *
565900021217      * Riempio il campone in fondo alla videata con i tasti scelti
566000021217     C                   exsr      Ges_TastiFun
566100021217     c                   eval      FD03 = FDxx
566200021217     c                   eval      F24D03 = F24Dxx
566300911023     C     FOR03         TAG
566400940802     C     WRIEM         IFEQ      'S'
566500131107     C                   WRITE     LRI6T01
566600940802     C                   MOVEL     *BLANKS       WRIEM
566700940802     C                   ENDIF
566800021217      *
566900021217      * Imposto la stringa comandi del 'giro' successivo
567000021217     C                   IF        *inKY = *on and $Loop > 1
567100021217     c                   Exsr      Ges_F24
567200021217     c                   eval      FD03 = FDxx
567300021217     c                   eval      F24D03 = F24Dxx
567400021217     C                   ENDIF
567500021217      *
567600131107     C                   EXFMT     LRI6D03
567700070420     c                   setoff                                       28
567800070420     c                   clear                   v1cmsg
567900931112      *
568000131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
568100931112     C   KY*IN89         IFEQ      *ON
568200931112     C                   SETOFF                                       89
568300931112     C                   ELSE
568400931112     C                   SETON                                        89
568500931112     C                   ENDIF
568600931112     C   KY              GOTO      FOR03
568700911023     C*
568800131108     C* F3 - FINE LAVORO TORNO A SUBFILE
568900940714     C   KC              MOVEL     '2'           V2CFLG            1
569000911023     C   KC              GOTO      ENDVIS
569100911023     C*
569200131108     C* F12 - FINE LAVORO tolno a videata di riepilogo
569300131108     C   KL              GOTO      emriepil
569400911023     C*
569500911024     C*
569600930517     C* CMD18 - INTERROGA LEGAME BOLLA
569700930518     C   KS              DO
569800930518     C                   EXSR      CMD18
569900940907     C                   MOVEL     'S'           WRIEM
570000930518     C                   GOTO      FOR03
570100930518     C                   END
570200930517     C*
570300940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
570400920804     C                   EXSR      COMAND
570500940905     C     WCMD          IFEQ      'S'
570600940802     C     WF03          IFEQ      *BLANKS
570700070403     c
570800920804     C                   GOTO      FOR03
570900940802     C                   ELSE
571000940802     C                   MOVEL     '2'           V2CFLG
571100940802     C                   GOTO      ENDVIS
571200940802     C                   ENDIF
571300940905     C                   ENDIF
571400911023     C*
571500911023     C* FORMATO4
571600931112     C                   SETOFF                                       89
571700940802     C                   MOVEL     'S'           WRIEM
571800021217      *
571900021217      * Riempio il campone in fondo alla videata con i tasti scelti
572000021217     C                   exsr      Ges_TastiFun
572100021217     c                   eval      FD04 = FDxx
572200021217     c                   eval      F24D04 = F24Dxx
572300911023     C     FOR04         TAG
572400940802     C     WRIEM         IFEQ      'S'
572500131107     C                   WRITE     LRI6T01
572600940802     C                   MOVEL     *BLANKS       WRIEM
572700940802     C                   ENDIF
572800021217      * Imposto la stringa comandi del 'giro' successivo
572900021217     C                   IF        *inKY = *on and $Loop > 1
573000021217     c                   Exsr      Ges_F24
573100021217     c                   eval      FD04 = FDxx
573200021217     c                   eval      F24D04 = F24Dxx
573300021217     C                   ENDIF
573400131107     C                   EXFMT     LRI6D04
573500070420     c                   setoff                                       28
573600070420     c                   clear                   v1cmsg
573700931112      *
573800131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
573900931112     C   KY*IN89         IFEQ      *ON
574000931112     C                   SETOFF                                       89
574100931112     C                   ELSE
574200931112     C                   SETON                                        89
574300931112     C                   ENDIF
574400931112     C   KY              GOTO      FOR04
574500911023     C*
574600131108     C* F12 - RITORNO VIDEATA 3
574700911023     C   KL              GOTO      FOR03
574800920804     C*
574900131108     C* F18 - INTERROGA LEGAME BOLLA
575000930518     C   KS              DO
575100930518     C                   EXSR      CMD18
575200930518     C                   GOTO      FOR04
575300930518     C                   END
575400940923     C*
575500940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
575600131120     c                   eval      wvideo='4'
575700131120     C                   EXSR      CTR_puIn
575800940923     C     WRIEM         CABEQ     'S'           FOR04
575900930517     C*
576000940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
576100920804     C                   EXSR      COMAND
576200940905     C     WCMD          IFEQ      'S'
576300940802     C     WF03          IFEQ      *BLANKS
576400940802     C                   GOTO      FOR04
576500940802     C                   ELSE
576600940802     C                   MOVEL     '2'           V2CFLG
576700940802     C                   GOTO      ENDVIS
576800940802     C                   ENDIF
576900940905     C                   ENDIF
577000911023     C*
577100911023     C* VIDEATA 5
577200931112     C                   SETOFF                                       89
577300940809     C                   MOVEL     'S'           WRIEM
577400021217      *
577500021217      * Riempio il campone in fondo alla videata con i tasti scelti
577600021217     C                   exsr      Ges_TastiFun
577700021217     c                   eval      FD05 = FDxx
577800021217     c                   eval      F24D05 = F24Dxx
577900911023     C     FOR05         TAG
578000940809     C     WRIEM         IFEQ      'S'
578100131107     C                   WRITE     LRI6T01
578200940809     C                   MOVEL     *BLANKS       WRIEM
578300940809     C                   ENDIF
578400951220     C                   Z-ADD     1             REC5
578500021217      *
578600021217      * Imposto la stringa comandi del 'giro' successivo
578700021217     C                   IF        *inKY = *on and $Loop > 1
578800021217     c                   Exsr      Ges_F24
578900021217     c                   eval      FD05 = FDxx
579000021217     c                   eval      F24D05 = F24Dxx
579100021217     C                   ENDIF
579200131107     C                   WRITE     LRI6Z05
579300131107     C                   EXFMT     LRI6C05
579400070420     c                   setoff                                       28
579500070420     c                   clear                   v1cmsg
579600931112      *
579700131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
579800931112     C   KY*IN89         IFEQ      *ON
579900931112     C                   SETOFF                                       89
580000931112     C                   ELSE
580100931112     C                   SETON                                        89
580200931112     C                   ENDIF
580300931112     C   KY              GOTO      FOR05
580400911023     C*
580500131108     C* F12 - RITORNO VIDEATA 2
580600911023     C   KL              GOTO      FOR04
580700920804     C*
580800940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
580900920804     C                   EXSR      COMAND
581000940905     C     WCMD          IFEQ      'S'
581100940802     C     WF03          IFEQ      *BLANKS
581200940802     C                   GOTO      FOR05
581300940802     C                   ELSE
581400131108     C                   MOVEL     '2'           V2CFLG
581500131108     C                   GOTO      ENDVIS
581600940802     C                   ENDIF
581700070406
581800070406     c                   else
581900070406     c
582000070406     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
582100070406     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
582200070406     c                   if        v3cgesd='?'
582300070406     c                   exsr      VISUALAbi
582400070406     c                   clear                   v3cgesd
582500070406     c                   eval      wriem='S'
582600070406     C                   GOTO      FOR05
582700070406     c                   endif
582800940905     C                   ENDIF
582900131108     c
583000131108     C                   GOTO      emriepil
583100931112      *
583200911023     C     ENDVIS        ENDSR
583300070403     C**************************************************************************
583400070403     C*    VISUALIZZA Abilitazioni
583500070403     C**************************************************************************
583600070403     C     VISUALAbi     BEGSR
583700070403     c*
583800131120     c                   clear                   fnlrg7ds
583900131120     c                   move      v3cdsp        ilrg7aas
584000131120     c                   eval      ilrg7lnp=v3clnp
584100131120     c                   eval      ilrg7nrs=v3cnrs
584200131120     c                   eval      ilrg7nsp=v3cnsp
584300131120     c                   eval      ilrg7vsdet='?'
584400131120     c                   eval      ilrg7lung=58
584500131120     c                   call      'FNLRG7R'
584600131120     c                   parm                    fnlrg7ds
584700070403     c
584800070403     C                   ENDSR
584900940712     C**************************************************************************
585000940802     C*    VISUALIZZA BOLLA PRECEDENTE
585100940802     C**************************************************************************
585200930510     C     VISUA2        BEGSR
585300940321      *
585400940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
585500131108     c     Emriepi2      tag
585600940712     C                   EXSR      RIEPI2
585700940712      *
585800131108      * PER F15 PASSO ALLA BOLLA, PER INVIO RITORNO AL SFL
585900940713     C  NKP              GOTO      ENDV8C
586000931112      *
586100931112     C                   SETOFF                                       89
586200940809     C                   MOVEL     'S'           WRIEM
586300021217      *
586400021217      * Riempio il campone in fondo alla videata con i tasti scelti
586500021217     C                   exsr      Ges_TastiFun
586600070516     c                   eval      FD03 = FDxx
586700070516     c                   eval      F24D03 = F24Dxx
586800930510     C     FOR033        TAG
586900940809     C     WRIEM         IFEQ      'S'
587000131107     C                   WRITE     LRI6T01
587100940809     C                   MOVEL     *BLANKS       WRIEM
587200940809     C                   ENDIF
587300021217      *
587400021217      * Imposto la stringa comandi del 'giro' successivo
587500021217     C                   IF        *inKY = *on and $Loop > 1
587600021217     c                   Exsr      Ges_F24
587700021217     c                   eval      FD03 = FDxx
587800021217     c                   eval      F24D03 = F24Dxx
587900021217     C                   ENDIF
588000131107     C                   EXFMT     LRI6D03
588100070420     c                   setoff                                       28
588200070420     c                   clear                   v1cmsg
588300931112      *
588400131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
588500931112     C   KY*IN89         IFEQ      *ON
588600931112     C                   SETOFF                                       89
588700931112     C                   ELSE
588800931112     C                   SETON                                        89
588900931112     C                   ENDIF
589000931112     C   KY              GOTO      FOR033
589100930510     C*
589200131108     C* F12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
589300930510     C   KC
589400131108     COR KL              GOTO      Emriepi2
589500930510     C*
589600940905     C* CONTROLLO SE PREMUTI TASTI FUNZIONALI E, NEL CASO, RIEMETTO
589700930510     C                   EXSR      COMAND
589800940905     C     WCMD          IFEQ      'S'
589900930510     C                   GOTO      FOR033
590000930510     C                   END
590100931112      *
590200930510     C* FORMATO4
590300931112     C                   SETOFF                                       89
590400940809     C                   MOVEL     'S'           WRIEM
590500021217      *
590600021217      * Riempio il campone in fondo alla videata con i tasti scelti
590700021217     C                   exsr      Ges_TastiFun
590800021217     c                   eval      FD04 = FDxx
590900021217     c                   eval      F24D04 = F24Dxx
591000930510     C     FOR044        TAG
591100940809     C     WRIEM         IFEQ      'S'
591200131107     C                   WRITE     LRI6T01
591300940809     C                   MOVEL     *BLANKS       WRIEM
591400940809     C                   ENDIF
591500021217      *
591600021217      * Imposto la stringa comandi del 'giro' successivo
591700021217     C                   IF        *inKY = *on and $Loop > 1
591800021217     c                   Exsr      Ges_F24
591900021217     c                   eval      FD04 = FDxx
592000021217     c                   eval      F24D04 = F24Dxx
592100021217     C                   ENDIF
592200131107     C                   EXFMT     LRI6D04
592300070420     c                   setoff                                       28
592400070420     c                   clear                   v1cmsg
592500931112      *
592600931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
592700931112     C   KY*IN89         IFEQ      *ON
592800931112     C                   SETOFF                                       89
592900931112     C                   ELSE
593000931112     C                   SETON                                        89
593100931112     C                   ENDIF
593200931112     C   KY              GOTO      FOR044
593300930510     C*
593400930510     C* CMD12 - RITORNO VIDEATA 3
593500930510     C   KL              GOTO      FOR033
593600940923     C*
593700940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
593800131120     c                   eval      wvideo='4'
593900131120     C                   EXSR      CTR_puIn
594000940923     C     WRIEM         CABEQ     'S'           FOR044
594100930510     C*
594200940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
594300930510     C                   EXSR      COMAND
594400940905     C     WCMD          IFEQ      'S'
594500930510     C                   GOTO      FOR044
594600940905     C                   ENDIF
594700931112      *
594800930510     C* VIDEATA 5
594900931112     C                   SETOFF                                       89
595000940809     C                   MOVEL     'S'           WRIEM
595100021217      *
595200021217      * Riempio il campone in fondo alla videata con i tasti scelti
595300021217     C                   exsr      Ges_TastiFun
595400021217     c                   eval      FD05 = FDxx
595500021217     c                   eval      F24D05 = F24Dxx
595600930510     C     FOR055        TAG
595700940809     C     WRIEM         IFEQ      'S'
595800131107     C                   WRITE     LRI6T01
595900940809     C                   MOVEL     *BLANKS       WRIEM
596000940809     C                   ENDIF
596100951220     C                   Z-ADD     1             REC5
596200021217      *
596300021217      * Imposto la stringa comandi del 'giro' successivo
596400021217     C                   IF        *inKY = *on and $Loop > 1
596500021217     c                   Exsr      Ges_F24
596600021217     c                   eval      FD05 = FDxx
596700021217     c                   eval      F24D05 = F24Dxx
596800021217     C                   ENDIF
596900131107     C                   WRITE     LRI6Z05
597000131107     C                   EXFMT     LRI6C05
597100070420     c                   setoff                                       28
597200070420     c                   clear                   v1cmsg
597300931112      *
597400931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
597500931112     C   KY*IN89         IFEQ      *ON
597600931112     C                   SETOFF                                       89
597700931112     C                   ELSE
597800931112     C                   SETON                                        89
597900931112     C                   ENDIF
598000931112     C   KY              GOTO      FOR055
598100930510     C*
598200930510     C* CMD12 - RITORNO VIDEATA 2
598300930510     C   KL              GOTO      FOR044
598400930510     C*
598500940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
598600940905     C                   EXSR      COMAND
598700940905     C     WCMD          IFEQ      'S'
598800930510     C                   GOTO      FOR055
598900930510     C                   END
599000131108
599100131108     C                   GOTO      Emriepi2
599200931112      *
599300940713     C     ENDV8C        ENDSR
599400940802     C**************************************************************************
599500940802     C* TASTI DI FUNZIONE
599600940802     C**************************************************************************
599700920804     C     COMAND        BEGSR
599800940905     C*
599900940905     C* PULISCO IL FLAG CHE INDICA SE PREMUTO UN TASTO FUNZIONE
600000940905     C                   MOVE      *BLANKS       WCMD              1
600100040218      *
600200070411      *?F1 - Aggiunta bolla in reclamo?o scelta bolla generica
600300040218if  1c                   if        *inKA   = *on
600400070423     C                   Z-ADD     arbLNP        COMLNP
600500070411     C                   MOVEL     '3'           V2CFLG
600600070411      *?-
600700070411if  2c                   if        SDSprm  > 1
600800131111     c                   eval      wARBlnp = ARBlnp
600900131111     c                   eval      wARBnrs = ARBnrs
601000131111     c                   eval      wARBnsp = ARBnsp
601100131111     c                   eval      wARBaas = ARBaas
601200040218     c                   clear                   FIDNA1ds
601300040218     c                   eval      IA1ins  = 'S'
601400040218     c                   eval      IA1tor  = 'S'
601500040218     c                   eval      IA1ogg  = ds_OggSped
601600070411    2c                   endif
601700040218     c                   goto      Fine
601800040218e   1c                   endif
601900940802     C*
602000990119     C* F5 - INTERROGAZIONE EVENTI
602100040218do  1C   KE              DO
602200940905     C                   MOVEL     'S'           WCMD
602300940726     C                   MOVEL     V2CAAS        PB5AAS
602400940726     C                   MOVEL     COMLNP        PB5LNP
602500940726     C                   MOVEL     V2CNRS        PB5NRS
602600940726     C                   MOVEL     V2CNSP        PB5NSP
602700940726     C                   MOVEL     *BLANKS       PB5F03
602800940726     C                   MOVEL     *BLANKS       PB5FLG
602900940802     C                   MOVEL     *BLANKS       PB5PGM
603000131107     C                   MOVEL     'FNLRI6R'     PB5PGM
603100940726     C                   MOVEL     V1CDSA        PB5DSA
603200940726     C                   MOVEL     V3CDSP        PB5DSP
603300940726     C                   MOVEL     VIDRMN        PB5RMN
603400940726     C                   MOVEL     V3CLNA        PB5LNA
603500940802     C                   MOVEL     V3DLNA        PB5LN1
603600940726     C                   MOVEL     V4CRSD        PB5RSD
603700940726     C                   MOVEL     VIDDCP        PB5DCP
603800941031     C                   MOVEL     VIDTMC        PB5TMC
603900940726     C                   MOVEL     PARAB5        KPJBU
604000940722     C                   CALL      'FNLRB5R'
604100930525     C                   PARM                    KPJBA
604200941109     C                   MOVEL     KPJBU         PARAB5
604300941109     C                   MOVEL     PB5F03        WF03
604400941214     C                   MOVEL     '1'           PA0B5
604500940725     C                   MOVEL     'S'           WRIEM
604600040218e   1C                   ENDDO
604700070420      *?F6 - Stampa LDV
604800070420if  1c                   if        *inKf = *on
604900070420     C                   MOVEL     'S'           WCMD
605000080201     c* Se assegnato non abilitato non stampo la LdV ma emetto errore
605100080201     c                   if        arbaca=' '
605200080201     C                   MOVEL     MSG(31)       V1CMSG
605300080201     C                   SETON                                        28
605400080201     c                   else
605500070420     c
605600070420     c                   if        wrichtrul90=' '
605700070420     c* La prima volta devo proporre la richiesta stampanti
605800070420     c                   exsr      RICHSTAMPAN
605900070420     C                   MOVEL     'S'           WRIEM
606000070420     c
606100070420     c* Se premuto F3 esco senza fare nulla
606200070420     c                   if        d90f3<>'1' and not *in91
606300070420     c                   eval      wrichtrul90='S'
606400070420     c                   endif
606500070420     c                   if        *in91
606600070420     C                   MOVEL     MSG(30)       V1CMSG
606700070420     C                   SETON                                        28
606800070420     c                   endif
606900070420     c                   endif
607000070420     c
607100070420     c                   if        wrichtrul90='S'
607200070420     C                   CLEAR                   FNLSB5DS
607300070426     c                   movel     'L'           db0tla
607400070420     C                   MOVEL     'R'           DB0RIS
607500070426     C                   MOVEL     'A'           DB0TBO
607600070420     C                   MOVEL     ARBAAS        DB0AAS
607700070420     C                   MOVEL     ARBLNP        DB0LNP
607800070420     C                   MOVEL     ARBNRS        DB0NRS
607900070420     C                   MOVEL     ARBNSP        DB0NSP
608000130319     c                   reset                   fnlsB6ds1
608100070420     C                   CALL      D90PSL
608200070420     C                   PARM                    FNLSB5DS
608300130308     c                   parm                    fnlsB6ds1
608400070420     C                   MOVEL     MSG(29)       V1CMSG
608500070420     C                   SETON                                        28
608600070420     c                   endif
608700080201     c                   endif
608800070420     c                   endif
608900930525     C*
609000990119     C* F7 - ANOMALIE
609100040218do  1C   KG              DO
609200960523     C                   MOVEL     'S'           WCMD
609300960523     C*
609400991027     C                   CLEAR                   FNLr44DS
609500960523     C                   MOVE      V2CAAS        D44AAS
609600960523     C                   MOVE      COMLNP        D44FIL
609700960523     C                   MOVE      V2CNRS        D44NRS
609800960523     C                   MOVE      V2CNSP        D44NSP
609900960523     C                   MOVEL     'A'           D44TBO
610000960523     C                   MOVE      'I'           D44MOD
610100991027     C                   MOVEL     FNLr44DS      KPJBU
610200960523     C                   CALL      'FNLR44R'
610300960523     C                   PARM                    KPJBA
610400960523     C*
610500990119     C                   MOVEL     'S'           WRIEM
610600040218e   1C                   ENDDO
610700920804     C*
610800990119     C* F8  - INTERROGAZIONE EVENTI SEGNACOLLI
610900040218if  1C     *INKH         IFEQ      *ON
611000961121     C                   MOVEL     'S'           WCMD
611100961121     C*
611200991027     C                   CLEAR                   FNLR45DS
611300961121     C                   MOVE      V2CAAS        D45AAS
611400961121     C                   MOVE      COMLNP        D45FIL
611500961121     C                   MOVE      V2CNRS        D45NRS
611600961121     C                   MOVE      V2CNSP        D45NSP
611700991027     C                   MOVEL     FNLR45DS      KPJBU
611800961121     C                   CALL      'FNLR45R'
611900961121     C                   PARM                    KPJBA
612000961121     C*
612100990119     C                   MOVEL     'S'           WRIEM
612200040218e   1C                   ENDIF
612300130830     c
612400130830     c* F9 - Orari servizi
612500131007     c                   if        *inki=*on
612600130830     C                   MOVEL     'S'           WCMD
612700131007     c                   if        wabif9='S'
612800130930     c                   exsr      sr_orarisr
612900131007     c                   endif
613000130830     C                   MOVEL     'S'           WRIEM
613100130830e   1C                   ENDIF
613200021213     C*
613300021213     C* F11 - Note supermercati
613400040218if  1C     *INKK         IFEQ      *ON
613500021213     C     *IN77         ANDEQ     *ON
613600021213      * imposto i campi 'bolla' della ds
613700021217     C                   MOVEL     'S'           WCMD
613800021213     C                   clear                   FnLrb3ds
613900030122     c                   Movel     V1cdsa        Rb3Rsu
614000030121     C                   z-add     SimFel        RB3Fil
614100021216     C                   z-add     V3CAAS        RB3AAS
614200030121     C                   z-add     V3CLNP        RB3LNP
614300021216     C                   z-add     V3CNRS        RB3NRS
614400021216     C                   z-add     V3CNSP        RB3NSP
614500021213     C                   z-add     V3CDSP        RB3DSP
614600021213     C                   z-add     VIDRMN        RB3RMN
614700021213     C                   z-add     V3CLNA        RB3LNA
614800131108     C                   movel     V32RSD        RB3RSD
614900021218     C                   z-add     VIDPKF        RB3PKF
615000021213     C                   z-add     VIDNCL        RB3NCL
615100021213     C                   z-add     VIDVLF        RB3VLB
615200030703      * bancali - part.varie "B"
615300040218If  2c                   If        %subst(ArbGva:1:1) = 'B'
615400030121     C                   z-add     §Ar5Tba       RB3Tba
615500030121     C                   z-add     VidNba        RB3Nba
615600030121     C                   z-add     §Ar5Tb2       RB3Tb2
615700030121     C                   z-add     VidNb2        RB3Nb2
615800030703     c                   Movel     V4cGva        Rb3Gva
615900040218e   2c                   EndIf
616000030703      * bancali - part.varie "O"
616100040218If  2c                   If        %subst(ArbGva:1:1) = 'O'
616200030703     C                   z-add     §Ar5bTba      RB3Tba
616300030703     C                   z-add     VidNba        RB3Nba
616400030703     C                   z-add     §Ar5bTb2      RB3Tb2
616500030703     C                   z-add     VidNb2        RB3Nb2
616600030703     c                   Movel     V4cGva        Rb3Gva
616700030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
616800030703     c                   Z-Add     §Ar5bCor      Rb3Cor
616900040218e   2c                   EndIf
617000021213      * richiamo PGM per note supermercati
617100021213     C                   call      'FNLRB3R'
617200030122     C                   PARM                    Kpjba
617300030121     C                   PARM                    Fnlrb3Ds
617400021213      *
617500021217     C                   MOVEL     'S'           WRIEM
617600040218e   1C                   ENDIF
617700961121     C*
617800990119     C* F14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
617900040218if  1C     *INKN         IFEQ      *ON
618000930624     C     *IN03         ANDEQ     *ON
618100941213     C     PA1GIA        ANDEQ     *BLANK
618200940905     C                   MOVEL     'S'           WCMD
618300991027     C                   CLEAR                   FNLG30DS
618400941213     C                   MOVEL     'A'           D30POA
618500941213     C                   MOVEL     V2CAAS        D30AAS
618600941213     C                   MOVEL     COMLNP        D30LNP
618700941213     C                   MOVEL     V2CNRS        D30NRS
618800941213     C                   MOVEL     V2CNSP        D30NSP
618900991027     C                   MOVEL     FNLG30DS      KPJBU
619000941213     C                   CALL      'FNLG31C'
619100920804     C                   PARM                    KPJBA
619200930624     C*
619300990119     C                   MOVEL     'S'           WRIEM
619400040218e   1C                   ENDIF
619500040218      *
619600040218      *?F16 - Gestione reclami?
619700040218if  1c                   if        *inKQ = *on
619800040225     c                   eval      wARBaas = ARBaas
619900131111     c                   eval      wARBlnp = ARBlnp
620000131111     c                   eval      wARBnrs = ARBnrs
620100131111     c                   eval      wARBnsp = ARBnsp
620200040225     c                   reset                   FIDNA2ds
620300040225if  2c                   if        SDSprm  > 1
620400040225     c                   eval      IA2mod  = 'I'
620500040225e   2c                   endif
620600040225     c                   eval      IA2ogg  = ds_OggSped
620700040225     c                   call      'FIDNA2R'
620800040225     c                   parm                    KPJBA
620900040225     c                   parm                    FIDNA2ds
621000040225     c                   movel     OA2f03        WF03
621100040225     c                   movel     'S'           WRIEM
621200051201     C                   MOVEL     'S'           WCMD
621300040218e   1c                   endif
621400070404      *
621500140311      *?F17 - Note AUT
621600070404if  1c                   if        *inKR = *on
621700140123     c                   clear                   fnlrn6ds
621800140123     c                   eval      in6mod='V'
621900140123     c                   eval      in6aas=v2caas
622000140123     c                   eval      in6lnp=comlnp
622100140123     c                   eval      in6nrs=v2cnrs
622200140123     c                   eval      in6nsp=v2cnsp
622300140123      * se sono una spedizione in consegna passo i dati della distinta del file ARB
622400140123     c                   IF        ARBfbc = *blanks and
622500140123     c                               ARBddc > 0 and
622600140123     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
622700140123     c                               arbica='R' or arbicc=' ' or arbicc='R')
622800140123     c     KSPED         chain     Fnarb01l
622900140123     c                   If        %found(Fnarb01l)
623000140123     c                   eval      in6fgs = arbifp
623100140123     c                   eval      in6nfv = arbndc
623200140123     c                   eval      in6dfv = arbddc
623300140123     c                   eval      in6pdr = arbpdc
623400140123     c                   eval      in6cons = 'S'
623500140123     c                   Endif
623600140123     c                   Endif
623700140123     c                   call      'FNLRN6R'
623800070404     c                   parm                    kpjba
623900140123     c                   parm                    fnlrn6ds
624000070404     c                   movel     'S'           WRIEM
624100070404     C                   MOVEL     'S'           WCMD
624200070404e   1c                   endif
624300920804     C*
624400990119     C* F21 - DETTAGLIO SEGNACOLLI
624500040218do  1C   KV              DO
624600940905     C                   MOVEL     'S'           WCMD
624700950628     C                   MOVEL     'A'           PA1TBO
624800950629     C                   CLEAR                   PA1F03
624900940720     C                   MOVEL     PARAM1        KPJBU
625000940726     C                   CALL      'FNLR37R'
625100920804     C                   PARM                    KPJBA
625200940802     C                   MOVEL     KPJBU         PARAM1
625300940802     C                   MOVEL     PA1F03        WF03
625400990119     C                   MOVEL     'S'           WRIEM
625500040218e   1C                   ENDDO
625600920804     C*
625700990119     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
625800040218do  1C   KJ              DO
625900940905     C                   MOVEL     'S'           WCMD
626000950628     C                   MOVEL     'A'           PA1TBO
626100950629     C                   CLEAR                   PA1F03
626200940712     C                   MOVEL     PARAM1        KPJBU
626300940726     C                   CALL      'FNLR75R'
626400920804     C                   PARM                    KPJBA
626500941214     C                   MOVEL     '1'           PA075
626600940802     C                   MOVEL     KPJBU         PARAM1
626700940802     C                   MOVEL     PA1F03        WF03
626800990119     C                   MOVEL     'S'           WRIEM
626900040218e   1C                   ENDDO
627000930114     C*
627100990119     C* F19 - INTERROGA SPUNTE VARI
627200040218if  1C     *INKT         IFEQ      *ON
627300940905     C                   MOVEL     'S'           WCMD
627400940713     C                   Z-ADD     0             PA4NFV
627500940713     C                   MOVEL     9             PA4NPG
627600940713     C                   MOVEL     99            PA4NPS
627700101202     C                   MOVEL     pa1pgm        PA4PGM
627800101203     C                   MOVEL     '1'           PA4FLG
627900950214     C                   MOVEL     V3CFLS        PA4LNP
628000940719     C                   MOVEL     V3CLNA        PA4LNA
628100940719     C                   MOVEL     V3CNRS        PA4NRS
628200940713     C                   Z-ADD     0             PA4NCD
628300940713     C                   Z-ADD     0             PA4NCA
628400101201     c                   eval      pa4boan=v2caas
628500101201     c                   eval      pa4bolp=comlnp
628600101201     c                   eval      pa4bors=v2cnrs
628700101201     c                   eval      pa4bosp=v2cnsp
628800040218if  2C     VIDNCD        IFGT      0
628900940713     C                   MOVEL     VIDNCD        PA4NCD
629000940713     C                   MOVEL     VIDNCA        PA4NCA
629100101201x   2C**                 ELSE
629200930705     C* CERCO I SEGNACOLLI DAL DETTAGLIO
629300101201     C**   KARB2         SETLL     FNART01L
629400101201     C**   KARB2         READE     FNART01L                               31
629500101201do  3C*+N31              DO
629600101201     C**                 Z-ADD     ARTNSC        PA4NCD
629700101201     C**   KARB2         SETGT     FNART01L
629800101201     C**   KARB2         READPE    FNART01L                               31
629900101201if  4C**N31ARTNSC        IFNE      PA4NCD
630000101201     C**                 MOVEL     ARTNSC        PA4NCA
630100101201e   4C**                 ENDIF
630200101201e   3C**                 ENDDO
630300101201e   2C                   ENDIF
630400930705     C*
630500940802     C                   MOVEL     PARA04        KPJBU
630600940722     C                   CALL      'FNLR04R'
630700920804     C                   PARM                    KPJBA
630800940725     C                   MOVEL     'S'           WRIEM
630900060130     C                   MOVEL     'S'           WCMD
631000040218e   1C                   ENDIF
631100930415     C*
631200990119     C* F23 - VISUALIZZA DATA CONSEGNA PREVISTA ESTERNA E ORE RITARDO
631300131121do  1C** KX              DO
631400131121     C**                 MOVEL     'S'           WCMD
631500131121     C**                 SETOFF                                       68
631600930504     C*
631700131121     C**                 MOVEL     ' '           D98TLA
631800131121     C**                 MOVEL     'A'           D98TBO
631900131121     C**                 MOVEL     ARBAAS        D98AAS
632000131121     C**                 MOVEL     ARBLNP        D98LNP
632100131121     C**                 MOVEL     ARBNRS        D98NRS
632200131121     C**                 MOVEL     ARBNSP        D98NSP
632300930504     C*
632400131121     C**                 CALL      'TNSD99R'
632500131121     C**                 PARM                    TNSD99DS
632600930504     C*
632700131121     c**                 if        d98dce>0
632800131121     c**   *iso          movel     d98dce        dataeur
632900131121     c**                 movel     dataeur       datadmy
633000131121     c**                 movel     dataeur       v08dce
633100131121     c**                 movel     datadmy       v06dce
633200131121     c**                 else
633300131121     c**                 clear                   v08dce
633400131121     c**                 clear                   v06dce
633500131121     c**                 endif
633600070404     c
633700131121     C**                 Z-ADD     D98NRC        VIDNRC
633800930526     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
633900930526     C*   CONSEGNA PREVISTA
634000131121     c**n69              eval      vidhmc=%editw(d98hce: '0 :  ')
634100930504     C*
634200930504     C* SE COMODO = 1  E' STATA FATTA CALL PGM
634300131121     C**                 MOVEL     '1'           PA098
634400131121e   1C**                 ENDDO
634500931112      *
634600990119      * F20 - INTERROGAZIONE AFFIDABILITA'
634700040218do  1C   KU              DO
634800940905     C                   MOVEL     'S'           WCMD
634900950102     C                   CLEAR                   FNLR97
635000950102     C                   MOVEL     V2CAAS        R97AAS
635100950102     C                   MOVEL     COMLNP        R97LNP
635200950102     C                   MOVEL     V2CNRS        R97NRS
635300950102     C                   MOVEL     V2CNSP        R97NSP
635400950102     C                   MOVEL     V3CDSP        R97DSP
635500950102     C                   MOVEL     VIDRMN        R97RMN
635600950102     C                   MOVEL     V3CLNA        R97LNA
635700950102     C                   MOVEL     V3DLNA        R97DLA
635800950102     C                   Z-ADD     VIDDCP        R97DCP
635900950102     C                   MOVEL     V4CRSD        R97RSD
636000950102     C                   MOVEL     V1CDSA        R97RSU
636100040121     C                   MOVEL     *blanks       R97CML
636200950102     C                   MOVEL     'A'           R97AP
636300950102     C   95              MOVEL     'S'           R97EVE
636400070404     C                   Z-ADD     w08DBR        R97DBR
636500070404     C                   Z-ADD     w08DPC        R97DPC
636600070404     C                   Z-ADD     w08DUC        R97DUC
636700950102     C                   MOVEL     VIDTMC        R97TMC
636800950102     C                   Z-ADD     VIDCAS        R97CAS
636900950102     C                   MOVEL     VIDVCA        R97VCA
637000931112     C                   MOVE      *BLANKS       KPJBU
637100950102     C                   MOVEL     FNLR97        KPJBU
637200940722     C                   CALL      'FNLR97R'                            90
637300931112     C                   PARM                    KPJBA
637400950102     C                   MOVEL     KPJBU         FNLR97
637500950102     C                   MOVEL     R97F03        WF03
637600950102     C                   MOVEL     'S'           WRIEM
637700040218e   1C                   ENDDO
637800110930     C* F22 - Perfect Order
637900110929do  1C                   if        *inkw
638000110929     C                   MOVEL     'S'           WCMD
638100110929     c                   movel     param1        s_param1
638200110929     c*
638300110929     c                   MOVEL     'W'           pa1flg
638400131107     c                   MOVEL(p)  'FNLRI6R'     pa1pgm
638500111005     c                   movel(p)  vidrma        pa1rma
638600111005     c                   movel(p)  vidxco        pa1xco
638700110929     c                   movel     paraaz        kpjbu
638800110929     c                   movel     param1        kpjbu
638900131127     c                   call      'FNLR36C'
639000110929     c                   parm                    kpjba
639100110929     c*
639200110929     c                   movel     s_param1      param1
639300110929     C*
639400110929     C                   MOVEL     'S'           WRIEM
639500110929e   1C                   ENDif
639600940905     C*
639700131107     C* ? FORMATO VIDEO:  LRI6D06  (TASSAZIONE 1a BOLLA)   ---   TASTO F13 ?
639800991026     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
639900040218if  1C     *INKM         IFEQ      *ON
640000991027     C                   eval      WCMD = 'S'
640100991027     C                   EVAL      *in89 = *off
640200131107     C                   WRITE     LRI6T01
640300131107     C                   WRITE     LRI6D06
640400991027     C                   eval      NR6 = 1
640500131107     C                   EXFMT     LRI6C06
640600040218e   1C                   ENDIF
640700991027     C*
640800131107     C* ? FORMATO VIDEO:  LRI6D09  (TASSAZIONE 2a BOLLA)   ---   TASTO F04 ?
640900991027     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
641000040218if  1C     *INKD         IFEQ      *ON
641100991027     C                   eval      WCMD = 'S'
641200991027     C                   EVAL      *in89 = *off
641300131107     C                   WRITE     LRI6T01
641400131107     C                   EXFMT     LRI6D09
641500040218e   1C                   ENDIF
641600990119     C*
641700990119     C* F2 - INTERROGAZIONE COMUNICAZIONE ANOMALIA
641800990129     C*      NON RICHIAMO SE RICHIAMATO DA INTERROGAZINE C.A.
641900040218if  1C     *INKB         IFEQ      *ON
642000990129     C     PA1FLG        ANDNE     'C'
642100040218if  2C     CONTCA        IFEQ      1
642200991027     C                   CLEAR                   FIDN00DS
642300990119     C                   Z-ADD     VHCAAC        I00AAC
642400990119     C                   Z-ADD     VHCFIL        I00FIL
642500990119     C                   Z-ADD     VHCNCA        I00NCA
642600990119     C                   Z-ADD     ARBAAS        I00AAS
642700990119     C                   Z-ADD     ARBNRS        I00NRS
642800990119     C                   Z-ADD     ARBNSP        I00NSP
642900990119     C                   Z-ADD     ARBLNP        I00LNP
643000990119     C                   MOVEL     'A'           I00TPB
643100991027     C                   MOVEL     FIDN00DS      KPJBU
643200990119     C                   CALL      'FIDN01R'
643300990119     C                   PARM                    KPJBA
643400000309     C                   PARM                    callricors        1
643500040218x   2C                   ELSE
643600990119     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
643700990119     C                   SETOFF                                       79
643800131107     C                   WRITE     LRI6C11
643900990119     C                   SETON                                        797823
644000990119     C                   CLEAR                   NRR11
644100040809      *
644200040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
644300040809      * legate alla spedizione che passo
644400040809     c                   clear                   fidn12ds
644500040809     c                   eval      i12aas = arbAAS
644600040809     c                   eval      i12lnp = arbLNP
644700040809     c                   eval      i12nrs = arbNRS
644800040809     c                   eval      i12nsp = arbNSP
644900040809     c                   eval      i12fel = 'E'
645000040809     c                   eval      i12fan = 'E'
645100040809     c*
645200040809     c                   call      'FIDN12R'
645300040809     c                   parm                    fidn12ds
645400040809     c*
645500040809     c                   setoff                                       31
645600040809     c*
645700040809     c* se non ci sono errori
645800040809     c                   if        o12err <> ' '
645900040809     c                   seton                                        31
646000040809     c                   else
646100040809     c* se numero di CA trovate maggiore di zero
646200040809     c                   if        o12nca = 0
646300040809     c                   seton                                        31
646400040809     c                   else
646500040809     c                   z-add     1             ii                2 0
646600040809     c                   dow       ii <= o12nca and *in31 = *off
646700040809     c                   movea     skkey(ii)     dskey
646800040809     C                   Z-ADD     dsaac         V11AAC
646900040809     C                   Z-ADD     dsfil         V11FIL
647000040809     C                   Z-ADD     dsnca         V11NCA
647100040809     C                   Z-ADD     skdca(ii)     G02INV
647200040809     C                   Z-ADD     *ZERO         G02DAT
647300040809     C                   MOVEL     '3'           G02ERR
647400040809     C                   CALL      'XSRDA8'
647500040809     C                   PARM                    WLBDAT
647600040809     C                   Z-ADD     G02DAT        V11DCA
647700040809     C                   Z-ADD     skfca(ii)     V11FCA
647800040809     C     skfca(ii)     CHAIN     FNDFA01L
647900040809     C                   IF        %found(FNDFA01L)
648000040809     C                   MOVEL     DFADES        V11DFA
648100040809     C                   ENDIF
648200040809     C**
648300040809     C                   ADD       1             NRR11             4 0
648400131107     C                   WRITE     LRI6S11
648500040809     c*
648600040809     c                   add       1             ii
648700040809     c                   enddo
648800040809     c*
648900040809     c                   seton                                        31
649000040809     c*
649100040809     c                   endif
649200040809     c                   endif
649300040809     c**
649400990119     C* EMETTO FORMATO
649500990119     C     FOR11         TAG
649600990119     C                   Z-ADD     1             NRR11
649700131107     C                   WRITE     LRI6T01
649800131107     C                   WRITE     LRI6Z11
649900131107     C                   EXFMT     LRI6C11
650000990119     C* F12 - RITORNO
650100040218if  3C     *INKL         IFEQ      *OFF
650200131107     C                   READC     LRI6S11                                31
650300040218do  4C     *IN31         DOWEQ     *OFF
650400040218if  5C     V11SCE        IFEQ      '5'
650500991027     C                   CLEAR                   FIDN00DS
650600990119     C                   Z-ADD     V11AAC        I00AAC
650700990119     C                   Z-ADD     V11FIL        I00FIL
650800990119     C                   Z-ADD     V11NCA        I00NCA
650900990119     C                   Z-ADD     ARBAAS        I00AAS
651000990119     C                   Z-ADD     ARBNRS        I00NRS
651100990119     C                   Z-ADD     ARBNSP        I00NSP
651200990119     C                   Z-ADD     ARBLNP        I00LNP
651300990119     C                   MOVEL     'A'           I00TPB
651400991027     C                   MOVEL     FIDN00DS      KPJBU
651500990119     C                   CALL      'FIDN01R'
651600990119     C                   PARM                    KPJBA
651700000309     C                   PARM                    callricors        1
651800990119     C**
651900990119     C                   CLEAR                   V11SCE
652000131107     C                   UPDATE    LRI6S11
652100040218e   5C                   ENDIF
652200990119     C**
652300131107     C                   READC     LRI6S11                                31
652400040218e   4C                   ENDDO
652500990119     C                   GOTO      FOR11
652600040218e   3C                   ENDIF
652700040218e   2C                   ENDIF
652800990119     C*
652900990119     C                   MOVEL     'S'           WRIEM
653000990119     C                   MOVEL     'S'           WCMD
653100040218e   1C                   ENDIF
653200931112      *
653300920804     C                   ENDSR
653400070420     C**************************************************************************
653500070420      * RICHIEDO LE STAMPANTI per stampa LDV
653600070420     C**************************************************************************
653700070420     c     RICHSTAMPAN   BEGSR
653800070420     C                   CLEAR                   TRUL90DS
653900070420      *
654000070420     C                   MOVEL     'S'           d90RSB
654100070420     C                   CALL      'TRUL90R'
654200070420     C                   PARM                    KPJBA
654300070420     C                   PARM                    trul90DS
654400070420      *
654500070420     C* F3 - FINE
654600070420     c                   if        d90f3<>'1'
654700070420      *
654800070420      * OVRPRTF BOLLE A4
654900070426     c                   if        *in97
655000070420     C                   MOVEA     D90PRB4       CMA4(30)
655100070420     C                   MOVEA     D90MDB4       CMA4(52)
655200070426     c                   else
655300070426     C                   MOVEA     D90PRB4       CMA4f6(30)
655400070426     C                   MOVEA     D90MDB4       CMA4f6(52)
655500070426     c                   endif
655600070420     C*
655700111221     C                   Z-ADD     130           LUNG             15 5
655800111221     C                   CLEAR                   COMMAN          130
655900070426     C   97              MOVEA     CMA4          COMMAN
656000070426     C  n97              MOVEA     CMA4f6        COMMAN
656100070420     C                   CALL      'QCMDEXC'                            91
656200070420     C                   PARM                    COMMAN
656300070420     C                   PARM                    LUNG
656400070420      *
656500070420     C                   if        not *in91
656600070420      *
656700070420      * OVRPRTF BOLLE A5
656800070426     c                   if        *in97
656900070420     C                   MOVEA     D90PRB5       CMA5(30)
657000070420     C                   MOVEA     D90MDB5       CMA5(52)
657100070426     c                   else
657200070426     C                   MOVEA     D90PRB5       CMA5f6(30)
657300070426     C                   MOVEA     D90MDB5       CMA5f6(52)
657400070426     c                   endif
657500070420     C*
657600111221     C                   Z-ADD     130           LUNG
657700070420     C                   CLEAR                   COMMAN
657800070426     C   97              MOVEA     CMA5          COMMAN
657900070426     C  n97              MOVEA     CMA5f6        COMMAN
658000070420     C                   CALL      'QCMDEXC'                            91
658100070420     C                   PARM                    COMMAN
658200070420     C                   PARM                    LUNG
658300070420     c                   endif
658400070420     c                   endif
658500070420      *
658600070420     c                   ENDSR
658700940802     C**************************************************************************
658800940802     C*    VISUALIZZA LEGAMI BOLLA
658900940802     C**************************************************************************
659000930413     C     CMD18         BEGSR
659100940802     C*
659200940411     C                   CLEAR                   WCAR
659300940411     C*
659400940411     C* MUOVO I DATI DELLA BOLLA CHE STO VISUALIZZANDO
659500940714     C                   MOVEL     V2CAAS        NUM2              4 0
659600940411     C                   MOVEL     COMLNP        COMLPN
659700940714     C                   MOVEL     V2CNRS        COMNRN
659800940714     C                   MOVEL     V2CNSP        COMNSN
659900950321     C                   MOVEL     V3CLNA        COMLAN
660000950428     C                   MOVEL     COMC08        SAVC08
660100940408     C**
660200940713     C* SAVNR8 > 1 --> GESTIONE SUBFILE
660300940408     C**
660400940907    1C     SAVNR8        IFGT      2
660500940408     C*
660600940809     C                   MOVEL     'S'           WRIEM
660700930413     C     FORC2         TAG
660800940713     C                   Z-ADD     1             REC8
660900940809     C     WRIEM         IFEQ      'S'
661000131107     C                   WRITE     LRI6T01
661100131107     C                   WRITE     LRI6Z08
661200940809     C                   MOVEL     *BLANKS       WRIEM
661300940809     C                   ENDIF
661400950428     C*
661500950428     C                   MOVEL     SAVC08        COMC08
661600131107     C                   EXFMT     LRI6C08
661700930413     C*
661800930413     C* CMD12 - FINE LAVORO
661900930413     C   KL              GOTO      SOTTO
662000930413     C*
662100131107     C                   READC     LRI6S08                                93
662200930510     C   93              GOTO      SOTTO
662300930510     C*
662400940408    2C     *IN93         DOWEQ     '0'
662500930413     C*
662600941230    3C     V8CSCE        IFEQ      '5'
662700940713     C                   MOVEL     ' '           V8CSCE
662800950103     C*
662900950103     C* RIPRISTINO GLI INDICATORI
663000950103     C                   MOVEL     V8CI80        *IN80
663100950103     C                   MOVEL     V8CI81        *IN81
663200950103     C                   MOVEL     V8CI82        *IN82
663300950103     C                   MOVEL     V8CI83        *IN83
663400131107     C                   UPDATE    LRI6S08
663500930504     C*
663600940907     C     V8CAAN        IFEQ      *ZERO
663700940907     C                   MOVEL     V8CAAP        LBLAAN
663800940907     C                   MOVEL     V8CLPP        LBLLPN
663900940907     C                   MOVEL     V8CNRP        LBLNRN
664000940907     C                   MOVEL     V8CNSP        LBLNSN
664100940907     C                   MOVEL     'P'           V8CFPS            1
664200940907     C                   ELSE
664300940907     C                   MOVEL     V8CAAN        LBLAAN
664400940907     C                   MOVEL     V8CLPN        LBLLPN
664500940907     C                   MOVEL     V8CNRN        LBLNRN
664600940907     C                   MOVEL     V8CNSN        LBLNSN
664700940907     C                   MOVEL     'S'           V8CFPS
664800940907     C                   ENDIF
664900940408     C*
665000940408     C* CONTROLLO SE IL LEGAME E' IN PARTENZA O IN ARRIVO
665100940907     C     KLBL1         CHAIN     FNARB01L                           93
665200940408     C*
665300940408    4C     *IN93         IFEQ      *OFF
665400940408     C*
665500940408     C                   EXSR      LEG2
665600940408     C*
665700940408   X4C                   ELSE
665800940408     C*
665900950316     C                   Z-ADD     LBLAAN        PA0AAS
666000950316     C                   Z-ADD     LBLLPN        PA0LNP
666100950316     C                   Z-ADD     LBLNRN        PA0NRS
666200950428     C                   Z-ADD     LBLNSN        PA0NSP
666300060130     C                   MOVEL     rsut          PA0RSU
666400941214     C                   MOVEL     SIMFEL        PA0SIM
666500040121     C                   MOVEL     *blanks       PA0CML
666600960903     C     PA1FLG        IFEQ      'N'
666700960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
666800960903     C*       verifichi questo caso
666900960903     C                   MOVEL     'R'           PA0FLG
667000960903     C                   ELSE
667100941214     C                   MOVEL     'A'           PA0FLG
667200960903     C                   END
667300941214     C                   MOVEL     *BLANKS       PA0CLI
667400950629     C                   CLEAR                   PA0F03
667500941214     C                   MOVEL     *BLANKS       PA0GIA
667600941214     C                   MOVEL     PARAM0        KPJBU
667700940722     C                   CALL      'FNLS05R'
667800930413     C                   PARM                    KPJBA
667900941214     C                   MOVEL     KPJBU         PARAM0
668000941214     C                   MOVEL     PA0F03        WF03
668100940907     C                   MOVEL     'S'           WRIEM
668200940408    4C                   ENDIF
668300940408     C*
668400940408    3C                   ENDIF
668500930413     C*
668600131107     C                   READC     LRI6S08                                93
668700940408    2C                   ENDDO
668800930413     C                   GOTO      FORC2
668900930413     C*
669000930413     C     SOTTO         TAG
669100930413     C*
669200940408    1C                   ELSE
669300940408     C*
669400940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
669500131107     C     2             CHAIN     LRI6S08                            99
669600940907     C     V8CAAP        IFEQ      V3CAAS
669700940907     C     V8CLPP        ANDEQ     V3CLNP
669800940907     C     V8CNRP        ANDEQ     V3CNRS
669900940907     C     V8CNSP        ANDEQ     V3CNSP
670000940907     C                   MOVEL     V8CAAN        ARBAAS
670100940907     C                   MOVEL     V8CLPN        ARBLNP
670200940907     C                   MOVEL     V8CNRN        ARBNRS
670300940907     C                   MOVEL     V8CNSN        ARBNSP
670400940907     C                   MOVEL     'S'           V8CFPS
670500940907     C                   ELSE
670600940907     C                   MOVEL     V8CAAP        ARBAAS
670700940907     C                   MOVEL     V8CLPP        ARBLNP
670800940907     C                   MOVEL     V8CNRP        ARBNRS
670900940907     C                   MOVEL     V8CNSP        ARBNSP
671000940907     C                   MOVEL     'P'           V8CFPS
671100940907     C                   ENDIF
671200940907     C* QUINDI VEDO SE LA TROVO FRA GLI ARRIVI
671300940907     C     KARB2         CHAIN     FNARB01L                           93
671400940408     C*
671500121116    2C     *IN93         IFEQ      *ON
671600950316     C                   Z-ADD     ARBAAS        PA0AAS
671700950316     C                   Z-ADD     ARBLNP        PA0LNP
671800950316     C                   Z-ADD     ARBNRS        PA0NRS
671900950316     C                   Z-ADD     ARBNSP        PA0NSP
672000060130     C                   MOVEL     rsut          PA0RSU
672100941214     C                   MOVEL     SIMFEL        PA0SIM
672200040121     C                   MOVEL     *blanks       PA0CML
672300960903     C     PA1FLG        IFEQ      'N'
672400960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
672500960903     C*       verifichi questo caso
672600960903     C                   MOVEL     'R'           PA0FLG
672700960903     C                   ELSE
672800941214     C                   MOVEL     'A'           PA0FLG
672900960903     C                   END
673000941214     C                   MOVEL     *BLANKS       PA0CLI
673100950629     C                   CLEAR                   PA0F03
673200941214     C                   MOVEL     *BLANKS       PA0GIA
673300941214     C                   MOVEL     PARAM0        KPJBU
673400940722     C                   CALL      'FNLS05R'
673500930413     C                   PARM                    KPJBA
673600941214     C                   MOVEL     KPJBU         PARAM0
673700941214     C                   MOVEL     PA0F03        WF03
673800940907     C                   MOVEL     'S'           WRIEM
673900121116     c* reimposto la key spedizione
674000121116     C                   MOVEL     NUM2          arbAAS
674100121116     C                   MOVEL     COMLPN        arbLNP
674200121116     C                   MOVEL     COMNRN        arbNRS
674300121116     C                   MOVEL     COMNSN        arbNSP
674400930510     C*
674500940408   X2C                   ELSE
674600940408     C*
674700940408     C                   EXSR      LEG2
674800940408     C*
674900940408    2C                   END
675000940408    1C                   END
675100940411     C*
675200940411     C* RICARICO I DATI DELLA BOLLA INIZIALE
675300940411     C     WCAR          IFEQ      'C'
675400940411     C                   SETOFF                                       75
675500940714     C                   MOVEL     NUM2          V2CAAS
675600940411     C                   MOVEL     COMLPN        COMLNP
675700940714     C                   MOVEL     COMNRN        V2CNRS
675800940714     C                   MOVEL     COMNSN        V2CNSP
675900940712     C     KARB          CHAIN     FNARB01L                           93
676000940411     C* RICARICO I DATI
676100940411     C                   EXSR      BOLLA
676200940411     C*
676300940411     C* RIABILITO IL CMD18
676400940411     C                   SETON                                        94
676500940411    1C                   END
676600940802     C*
676700930413     C                   ENDSR
676800940802     C**************************************************************************
676900940802     C*    LEGAMI BOLLA
677000940802     C**************************************************************************
677100940408     C     LEG2          BEGSR
677200940802     C*
677300940411     C                   MOVEL     'C'           WCAR              1
677400940408     C*
677500940408     C* 75 ON - NON RIENTRO NEL CARICAMENTO LEGAME BOLLA
677600940408     C                   SETON                                        75
677700940408     C* 94 OFF- DISABILITO IL CMD18
677800940408     C                   SETOFF                                       94
677900940408     C* CARICO DATI BOLLA
678000940408     C                   EXSR      BOLLA
678100940411     C*
678200940411     C* VISUALIZZA NUOVO LEGAME
678300940408     C* SE LA BOLLA E' UNA PRECEDENTE --> VISUALIZZO LA SEGUENTE
678400940713     C     V8CFPS        IFEQ      'P'
678500950316     C                   MOVEA     'SEGUEN.'     LBL(14)
678600940408     C                   ELSE
678700940408     C* SE LA BOLLA E' UNA SEGUENTE   --> VISUALIZZO LA PRECEDENTE
678800950316     C                   MOVEA     'PRECED.'     LBL(14)
678900940408     C                   ENDIF
679000950316     C                   MOVEA     COMLPN        LBL(21)
679100950316     C                   MOVEA     COMNRN        LBL(25)
679200950316     C                   MOVEA     COMNSN        LBL(28)
679300950316     C                   MOVEA     COMLAN        LBL(40)
679400950307     C                   MOVEA     *BLANKS       LBL(44)
679500940408     C                   MOVEA     LBL           LEGAME
679600940408     C*
679700940411     C* MOUVO I DATI DELLA LEGATA NEI CAMPI DELLA PARAM
679800940714     C                   MOVEL     ARBAAS        V2CAAS
679900940411     C                   MOVEL     ARBLNP        COMLNP
680000940714     C                   MOVEL     ARBNRS        V2CNRS
680100940714     C                   MOVEL     ARBNSP        V2CNSP
680200940408     C*
680300940408     C                   EXSR      VISUA2
680400940408     C*
680500941230     C                   MOVEL     'S'           WRIEM
680600940408     C                   ENDSR
680700940408     C*
680800940712      **************************************************************************
680900131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
681000940408      *     PER BOLLA NORMAL - CMD18 PREMUTO DA VISUAL -
681100091216      *     ricordarsi di aggiornare anche RIEPI2 per ogni modifica fatta qui
681200940712      **************************************************************************
681300940712     C     RIEPIL        BEGSR
681400940712      *
681500940712     C                   SETOFF                                       89
681600940712      *
681700940712      * EMETTO IL FORMATO VIDEO
681800940720     C                   MOVEL     'S'           WRIEM
681900940712     C     FOR07         TAG
682000940720     C     WRIEM         IFEQ      'S'
682100131107     C                   WRITE     LRI6T01
682200940720     C                   MOVEL     *BLANKS       WRIEM
682300940720     C                   ENDIF
682400021217      * Imposto la stringa comandi del 'giro' successivo
682500021217     C                   IF        *inKY = *on and $Loop > 1
682600021217     c                   Exsr      Ges_F24
682700021217     c                   eval      FD07 = FDxx
682800021217     c                   eval      F24D07 = F24Dxx
682900021217     C                   ENDIF
683000131107     C                   EXFMT     LRI6D07
683100070420     c                   setoff                                       28
683200070420     c                   clear                   v1cmsg
683300940712      *
683400131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
683500940712     C   KY*IN89         IFEQ      *ON
683600940712     C                   SETOFF                                       89
683700940712     C                   ELSE
683800940712     C                   SETON                                        89
683900940712     C                   ENDIF
684000940712     C   KY              GOTO      FOR07
684100940712      *
684200131108      * F3 - FINE LAVORO TORNO A SUBFILE
684300940714     C   KC              MOVEL     '2'           V2CFLG            1
684400940712     C   KC              GOTO      ENDRIE
684500940712      *
684600131108      * F18 - INTERROGA LEGAME BOLLA
684700940712     C   KS              DO
684800940712     C                   EXSR      CMD18
684900940712     C                   GOTO      FOR07
685000940712     C                   END
685100940712      *
685200940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
685300940712     C                   EXSR      COMAND
685400940905     C     WCMD          IFEQ      'S'
685500940802     C     WF03          IFEQ      *BLANKS
685600070404     c
685700940802     C                   GOTO      FOR07
685800940802     C                   ELSE
685900940802     C                   MOVEL     '2'           V2CFLG
686000940802     C                   GOTO      ENDRIE
686100070404     C                   ENDIF
686200091215     c
686300070404     c                   else
686400070404     c
686500070404     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
686600070404     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
686700070404     c                   if        v3cgesd='?'
686800131120
686900131120     c**                 if        wriem='S'
687000131120     c**                 eval      wvideo='R'
687100131120     c**                 exsr      Riemett
687200131120     c**                 endif
687300131120
687400070404     c                   exsr      VISUALAbi
687500070404     c                   clear                   v3cgesd
687600070412     c                   eval      wriem='S'
687700070404     c                   endif
687800131120     c
687900131120     c* controlla se immessi dei '?'
688000131120     c                   eval      wvideo='R'
688100131120     c                   exsr      CTR_puIn
688200091215     c
688300091215     c                   if        wriem='S'
688400091215     C                   GOTO      FOR07
688500091215     c                   endif
688600070404     c
688700940712     C                   END
688800940712      *
688900940712     C     ENDRIE        ENDSR
689000940712      **************************************************************************
689100131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
689200940408      *     PER LEGAME BOLLA - CMD18 PREMUTO DA VISUA2 -
689300940712      **************************************************************************
689400940712     C     RIEPI2        BEGSR
689500940712      *
689600940712     C                   SETOFF                                       89
689700940712      *
689800940712      * EMETTO IL FORMATO VIDEO
689900940809     C                   MOVEL     'S'           WRIEM
690000940712     C     FOR77         TAG
690100940809     C     WRIEM         IFEQ      'S'
690200131107     C                   WRITE     LRI6T01
690300940809     C                   MOVEL     *BLANKS       WRIEM
690400940809     C                   ENDIF
690500091216      * Imposto la stringa comandi del 'giro' successivo
690600091216     C                   IF        *inKY = *on and $Loop > 1
690700091216     c                   Exsr      Ges_F24
690800091216     c                   eval      FD07 = FDxx
690900091216     c                   eval      F24D07 = F24Dxx
691000091216     C                   ENDIF
691100091216     c
691200131107     C                   EXFMT     LRI6D07
691300070516     c                   setoff                                       28
691400070516     c                   clear                   v1cmsg
691500940408     C*
691600131108      * F3 - FINE LAVORO TORNO A SUBFILE
691700940714     C   KC              MOVEL     '2'           V2CFLG            1
691800940408     C*
691900940408     C  NKC              DO
692000940712      *
692100131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
692200940712     C   KY*IN89         IFEQ      *ON
692300940712     C                   SETOFF                                       89
692400940712     C                   ELSE
692500940712     C                   SETON                                        89
692600940712     C                   ENDIF
692700940712     C   KY              GOTO      FOR77
692800940712      *
692900940712      *
693000131108      * F18 - INTERROGA LEGAME BOLLA
693100940712     C   KS              DO
693200940712     C                   EXSR      CMD18
693300940712     C                   GOTO      FOR77
693400940712     C                   END
693500940712      *
693600940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
693700940712     C                   EXSR      COMAND
693800940905     C     WCMD          IFEQ      'S'
693900940712     C                   GOTO      FOR77
694000091216     c
694100091216     c                   else
694200091216     c
694300091216     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
694400091216     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
694500091216     c                   if        v3cgesd='?'
694600131120
694700131120     c**                 if        wriem='S'
694800131120     c**                 eval      wvideo='R'
694900131120     c**                 exsr      Riemett
695000131120     c**                 endif
695100131120
695200091216     c                   exsr      VISUALAbi
695300091216     c                   clear                   v3cgesd
695400091216     c                   eval      wriem='S'
695500091216     c                   endif
695600131120
695700131120     c* controlla se immessi dei '?'
695800131120     c                   eval      wvideo='R'
695900131120     c                   exsr      CTR_puIn
696000091216     c
696100091216     c                   if        wriem='S'
696200091216     C                   GOTO      FOR77
696300091216     c                   endif
696400091216     c
696500940712     C                   END
696600091216     C                   END
696700940712      *
696800940712     C                   ENDSR
696900131120      **************************************************************************
697000131120      * riemissione video per nuovo '?'
697100131120      **************************************************************************
697200131120     c**   Riemett       BEGSR
697300131120     c**                 write     LRI6t01
697400131120
697500131120     c**                 select
697600131120     c**                 when      wvideo='4'
697700131120     c**                 write     LRI6d04
697800131120     c**                 when      wvideo='R'
697900131120     c**                 write     LRI6d07
698000131120     c**                 endsl
698100131120
698200131120     c**                 ENDSR
698300940923      **************************************************************************
698400940923      *     CONTROLLI FORMATO VIDEO D04
698500940923      **************************************************************************
698600131120     C     CTR_puIn      BEGSR
698700110601     c*
698800110601     c* verifico se inserito '?' su email destinatario
698900110601     c                   if        v4cemw='?'
699000110601     c                   exsr      sr_emdest
699100110601     C                   MOVEL     'S'           WRIEM
699200110601     c                   endif
699300940923      *
699400091215      * Verifico se inserito '?' per visualizzazione beneficiario
699500091215     c                   if        vidben='?'
699600131120     c**                 if        wriem='S'
699700131120     c**                 exsr      Riemett
699800131120     c**                 endif
699900131120     c
700000091215     c                   exsr      sr_bencas
700100091215     C                   MOVEL     'S'           WRIEM
700200091215     c                   endif
700300131114     c
700400131114     c* Visualizzazione dati filiale
700500131114     c                   if        viddec_f='?'
700600131120     c**                 if        wriem='S'
700700131120     c**                 exsr      Riemett
700800131120     c**                 endif
700900131120
701000131114     c                   clear                   trulorfds
701100131114     c                   eval      iorffil=arblna
701200131114     c                   call      'TRULORFR'
701300131114     c                   parm                    kpjba
701400131114     c                   parm                    trulorfds
701500131114     c
701600131114     c                   clear                   viddec_f
701700131114     C                   MOVEL     'S'           WRIEM
701800131114     c                   endif
701900091215     c
702000940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
702100940923     C     V4IGCA        IFEQ      '?'
702200991027     C                   CLEAR                   DS7PQRS
702300941003     C                   MOVEL     'S'           DS7RIC
702400941003     C                   MOVEL     'V'           DS7GES
702500941003     C                   MOVEL     V4CGCA        DS7COD
702600991027     C                   MOVEL     DS7PQRS       KPJBU
702700960126     C                   CALL      'TRTB72R'
702800941003     C                   PARM                    KPJBA
702900940923     C                   MOVEL     *BLANKS       V4IGCA
703000940923     C                   MOVEL     'S'           WRIEM
703100940923     C                   ENDIF
703200940923     C*
703300940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
703400940923     C     V4IGGA        IFEQ      '?'
703500991027     C                   CLEAR                   DS7PQRS
703600941003     C                   MOVEL     'S'           DS7RIC
703700941003     C                   MOVEL     'V'           DS7GES
703800941003     C                   MOVEL     V4CGGA        DS7COD
703900991027     C                   MOVEL     DS7PQRS       KPJBU
704000960126     C                   CALL      'TRTB69R'
704100941003     C                   PARM                    KPJBA
704200940923     C                   MOVEL     *BLANKS       V4IGGA
704300940923     C                   MOVEL     'S'           WRIEM
704400940923     C                   ENDIF
704500940923     C*
704600940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
704700940923     C     V4IGMA        IFEQ      '?'
704800991027     C                   CLEAR                   DS7PQRS
704900941003     C                   MOVEL     'S'           DS7RIC
705000941003     C                   MOVEL     'V'           DS7GES
705100941003     C                   MOVEL     V4CGMA        DS7COD
705200991027     C                   MOVEL     DS7PQRS       KPJBU
705300960126     C                   CALL      'TRTB70R'
705400941003     C                   PARM                    KPJBA
705500940923     C                   MOVEL     *BLANKS       V4IGMA
705600940923     C                   MOVEL     'S'           WRIEM
705700940923     C                   ENDIF
705800940923     C*
705900940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
706000940923     C     V4IGVA        IFEQ      '?'
706100991027     C                   CLEAR                   DS7PQRS
706200941003     C                   MOVEL     'S'           DS7RIC
706300941003     C                   MOVEL     'V'           DS7GES
706400941003     C                   MOVEL     V4CGVA        DS7COD
706500991027     C                   MOVEL     DS7PQRS       KPJBU
706600960126     C                   CALL      'TRTB73R'
706700941003     C                   PARM                    KPJBA
706800940923     C                   MOVEL     *BLANKS       V4IGVA
706900940923     C                   MOVEL     'S'           WRIEM
707000940923     C                   ENDIF
707100940923      *
707200940923     C                   ENDSR
707300940321      **************************************************************************
707400940713      *     R O U T I N E      I N I Z I A L E
707500940713      **************************************************************************
707600940713     C     *INZSR        BEGSR
707700940713      *
707800940713     C     KARB          KLIST
707900940714     C                   KFLD                    V2CAAS
708000940713     C                   KFLD                    COMLNP            3 0
708100940714     C                   KFLD                    V2CNRS
708200940714     C                   KFLD                    V2CNSP
708300940713     C     KARB2         KLIST
708400940713     C                   KFLD                    ARBAAS
708500940713     C                   KFLD                    ARBLNP
708600940713     C                   KFLD                    ARBNRS
708700940713     C                   KFLD                    ARBNSP
708800971223     C     KARB4         KLIST
708900971223     C                   KFLD                    AR4AAS
709000971223     C                   KFLD                    AR4LNP
709100971223     C                   KFLD                    AR4NRS
709200971223     C                   KFLD                    AR4NSP
709300000228     C     KAR4W         KLIST
709400000228     C                   KFLD                    wAR4AAS
709500000228     C                   KFLD                    wAR4LNP
709600000228     C                   KFLD                    wAR4NRS
709700000228     C                   KFLD                    wAR4NSP
709800940713     C     KLNPX         KLIST
709900940713     C                   KFLD                    V1CAAS
710000940713     C                   KFLD                    V1CLP2
710100940713     C                   KFLD                    V1CNRS
710200940713     C                   KFLD                    V1CNSP
710300940713     C     K1ARB         KLIST
710400020910     c                   Kfld                    Wifp
710500940713     C                   KFLD                    NDC               7 0
710600940713     C                   KFLD                    NSS               3 0
710700940713     C                   KFLD                    COMLNA            3 0
710800940713     C                   KFLD                    COMAAD
710900940713     C                   KFLD                    COMMGD
711000940713     C                   KFLD                    V1CLP1
711100940713     C     K2ARB         KLIST
711200020910     c                   Kfld                    Wifp
711300940713     C                   KFLD                    NDC
711400940713     C                   KFLD                    NSS
711500940713     C                   KFLD                    COMLNA
711600940713     C                   KFLD                    COMAAD
711700940713     C                   KFLD                    COMMGD
711800940713     C     K3ARB         KLIST
711900020910     c                   Kfld                    Wifp
712000940713     C                   KFLD                    NDC
712100940713     C                   KFLD                    NSS
712200940713     C                   KFLD                    COMLNA
712300940713     C     K4ARB         KLIST
712400940713     C                   KFLD                    COMLNA
712500940713     C                   KFLD                    COMDTD
712600940713     C                   KFLD                    V1CLP1
712700940713     C     K5ARB         KLIST
712800940713     C                   KFLD                    COMLNA
712900940713     C                   KFLD                    COMDTD
713000940713     C     K6ARB         KLIST
713100940713     C                   KFLD                    V1CLP1
713200940713     C                   KFLD                    COMAAD
713300940713     C                   KFLD                    COMMGD
713400940713     C                   KFLD                    V1CLNA
713500940713     C                   KFLD                    V1CPSD
713600950405     C                   KFLD                    COMSCD
713700950405     C     K6ARB1        KLIST
713800950405     C                   KFLD                    V1CLP1
713900950405     C                   KFLD                    COMAAD
714000950405     C                   KFLD                    COMMGD
714100950405     C                   KFLD                    V1CLNA
714200950405     C                   KFLD                    V1CPSD
714300940713     C     K7ARB         KLIST
714400940713     C                   KFLD                    V1CLP1
714500940713     C                   KFLD                    COMAAD
714600940713     C                   KFLD                    COMMGD
714700940713     C     K8ARB         KLIST
714800940713     C                   KFLD                    V1CLNA
714900940713     C                   KFLD                    COMAAD
715000940713     C                   KFLD                    COMMGD
715100940713     C                   KFLD                    V1CLP1
715200940713     C                   KFLD                    V1CPSD
715300940713     C                   KFLD                    V1CSCD
715400940713     C     K9ARB         KLIST
715500940713     C                   KFLD                    V1CLNA
715600940713     C                   KFLD                    COMAAD
715700940713     C                   KFLD                    COMMGD
715800020627     c     arb7k         klist
715900020627     c                   kfld                    v1cnzd
716000020627     c                   kfld                    v1cprd
716100131111     C     ARB75K        KLIST
716200020627     c                   kfld                    v1cnzd
716300020627     c                   kfld                    v1cprd
716400131111     C                   KFLD                    $AAS
716500131111     C                   KFLD                    $MGS
716600940713     C                   KFLD                    V1CRSD
716700940713     C     AR43K         KLIST
716800020627     c                   kfld                    v1cnzd
716900020627     c                   kfld                    v1cprd
717000940713     C                   KFLD                    $AAS
717100940713     C                   KFLD                    $MGS
717200940713     C     ARB44K        KLIST
717300020627     c                   kfld                    v1cnzd
717400020627     c                   kfld                    v1cprd
717500940713     C                   KFLD                    $AAS
717600940713     C                   KFLD                    $MGS
717700940713     C                   KFLD                    V1CRSM
717800940713     C     AR44K         KLIST
717900020627     c                   kfld                    v1cnzd
718000020627     c                   kfld                    v1cprd
718100940713     C                   KFLD                    $AAS
718200940713     C                   KFLD                    $MGS
718300971223     C     KAR4          KLIST
718400971223     C                   KFLD                    WTRC
718500971223     C                   KFLD                    WN30
718600010508     C     KAR44         KLIST
718700010508     C                   KFLD                    WTRC
718800010508     C                   KFLD                    WN14
718900940713     C     KART          KLIST
719000940713     C                   KFLD                    ARBAAS
719100940713     C                   KFLD                    ARBLNP
719200940713     C                   KFLD                    ARBNRS
719300940713     C                   KFLD                    ARBNSP
719400940713     C                   KFLD                    V1CSCD
719500940713     C*
719600940713     C     KTAB1         KLIST
719700940713     C                   KFLD                    CODUT
719800940713     C                   KFLD                    COD
719900940713     C                   KFLD                    KEY
720000940713     C     KTAB2         KLIST
720100940713     C                   KFLD                    CODUT
720200940713     C                   KFLD                    COD
720300940713     C*
720400940907     C     KLBL1         KLIST
720500940713     C                   KFLD                    LBLAAN
720600940713     C                   KFLD                    LBLLPN
720700940713     C                   KFLD                    LBLNRN
720800940713     C                   KFLD                    LBLNSN
720900940713     C     KLBL2         KLIST
721000940713     C                   KFLD                    LBLAAP
721100940713     C                   KFLD                    LBLLPP
721200940713     C                   KFLD                    LBLNRP
721300940713     C                   KFLD                    LBLNSP
721400940907     C     KLBL3         KLIST
721500940907     C                   KFLD                    KAAS
721600940907     C                   KFLD                    KLNP
721700940907     C                   KFLD                    KNRS
721800940907     C                   KFLD                    KNSP
721900940713     C*
722000940719     C     KAR41         KLIST
722100940719     C                   KFLD                    KAAS
722200940719     C                   KFLD                    KLNP
722300940719     C                   KFLD                    KNRS
722400940719     C                   KFLD                    KNSP
722500940802     C                   KFLD                    KTRC
722600951220     C     KAR42         KLIST
722700951220     C                   KFLD                    KAAS
722800951220     C                   KFLD                    KLNP
722900951220     C                   KFLD                    KNRS
723000951220     C                   KFLD                    KNSP
723100021213     C*
723200021213     C     KAR51         KLIST
723300021213     C                   KFLD                    KAAS
723400021213     C                   KFLD                    KLNP
723500021213     C                   KFLD                    KNRS
723600021213     C                   KFLD                    KNSP
723700021213     C                   KFLD                    KTRd              3
723800940719     C*
723900991026     C     KAR61         KLIST
724000940713     C                   KFLD                    KAAS
724100940713     C                   KFLD                    KLNP
724200940713     C                   KFLD                    KNRS
724300940713     C                   KFLD                    KNSP
724400991026     C                   KFLD                    KTRC
724500991026     C*
724600051128     C     Ksped         KLIST
724700991026     C                   KFLD                    KAAS
724800991026     C                   KFLD                    KLNP
724900991026     C                   KFLD                    KNRS
725000991026     C                   KFLD                    KNSP
725100940713     C*
725200990715     C     KGCA1         KLIST
725300941202     C                   KFLD                    KAAS
725400940713     C                   KFLD                    KLNP
725500940713     C                   KFLD                    KNRS
725600940713     C                   KFLD                    KNSP
725700940713     C*
725800990805     C     KEVB1         KLIST
725900940713     C                   KFLD                    KAAS$
726000940713     C                   KFLD                    KLNP
726100940713     C                   KFLD                    KNRS
726200940713     C                   KFLD                    KNSP
726300131114     C     Kcet          KLIST
726400140224     C                   KFLD                    pctfgs
726500131114     C                   KFLD                    pctNDC
726600131114     C                   KFLD                    arbpdc
726700131114     C                   KFLD                    TTRD              3
726800131114     C                   KFLD                    arbAAS
726900131114     C                   KFLD                    arbLNP
727000131114     C                   KFLD                    arbNRS
727100131114     C                   KFLD                    arbNSP
727200950324     C*
727300020906     C     KFVV1         KLIST
727400950324     C                   KFLD                    WNPG
727500950327     C                   KFLD                    WNDC
727600020906     c                   Kfld                    Wfgs
727700131119     C     KDST          KLIST
727800131119     C                   KFLD                    WNPG
727900131119     C                   KFLD                    kndc6
728000131119     c                   Kfld                    Wfgs
728100021203
728200021203     C     Kfapd         klist
728300021203     C                   kfld                    kpdtip
728400021203     C                   kfld                    kpdpdr
728500131108     c
728600131108     c     kbrv          klist
728700131108     C                   KFLD                    WNPG
728800131108     C                   KFLD                    knfv
728900131108     c                   Kfld                    Wfgs
729000131108     c                   Kfld                    artfls
729100131108     c                   Kfld                    artlna
729200131108     c                   Kfld                    artnrs
729300131108     c                   Kfld                    artnsc
729400131111     C     KarS          KLIST
729600131111     C                   KFLD                    KKTRC             1
729601170315     C                   KFLD                    V1CNOT
729700131111     C                   MOVE      'C'           KKTRC
729800131111     C     KarSXT        KLIST
729900131111     C                   KFLD                    arSFLS
730000131111     C                   KFLD                    arSLNA
730100131111     C                   KFLD                    arSNRS
730200131111     C                   KFLD                    arSNSC
730300131111     C     KartXP        KLIST
730400131111     C                   KFLD                    arTAAS
730500131111     C                   KFLD                    arTLNP
730600131111     C                   KFLD                    arTNRS
730700131111     C                   KFLD                    arTNSP
730800040218      *
730900160404      * File FITGD32L
731000040218     c     K02TGD02      klist
731100040218     c                   kfld                    TGDtor
731200040218     c                   kfld                    TGDogg
731300030527     C*
731400030527     C* Chiave su TNTBE01L - Parziale
731500030527     C     KTBE01P       KLIST
731600030527     C                   KFLD                    TBECOD
731700030527     C                   KFLD                    TBEKE1
731800940713     C*
731900940713     C*-------------------------------------------------------------------------
732000940713     C*
732100991222     C     *LIKE         DEFINE    ARBAAS        WAAS
732200991027     C     *LIKE         DEFINE    ARBLNP        WLNP
732300991027     C     *LIKE         DEFINE    ARBNRS        WNRS
732400991027     C     *LIKE         DEFINE    ARBNSP        WNSP
732500991027     C     *LIKE         DEFINE    FVVNPG        WNPG
732600991027     C     *LIKE         DEFINE    FVVNFV        WNDC
732700131108     C     *LIKE         DEFINE    brvNFV        knfv
732800991027     C     *LIKE         DEFINE    V1CSCD        COMSCD
732900991027     C     *LIKE         DEFINE    COMC08        SAVC08
733000971219     C     *LIKE         DEFINE    ARBAAS        KAAS
733100990805     C     *LIKE         DEFINE    EVBAAS        KAAS$
733200940713     C     *LIKE         DEFINE    ARBLNP        KLNP
733300940713     C     *LIKE         DEFINE    ARBNRS        KNRS
733400940713     C     *LIKE         DEFINE    ARBNSP        KNSP
733500961112     C     *LIKE         DEFINE    ARBFBR        WFBR
733600940713     C     *LIKE         DEFINE    TBLCOD        COD
733700940713     C     *LIKE         DEFINE    TBLKEY        KEY
733800940713     C     *LIKE         DEFINE    ARBAAS        $AAS
733900940713     C     *LIKE         DEFINE    ARBMGS        $MGS
734000940719     C     *LIKE         DEFINE    AR4TRC        KTRC
734100940907     C     *LIKE         DEFINE    LBLAAN        COMAAN
734200971223     C     *LIKE         DEFINE    AR4TRC        WTRC
734300060130     C*-------------------------------------------------------------------------
734400060130     c
734500060130     c                   movel     kpjbu         savkpjbu
734600060130      *
734700060130     c     *dtaara       define    §azute        azuteds
734800060130     c     *dtaara       define    §datiute      ddatiute
734900060130     c                   in(E)     *dtaara
735000060130    1c                   If        %error  or RSUT = *blanks
735100060130     c                   Clear                   Tibs34ds
735200060130     c                   Call      'TIBS34R'
735300060130     c                   Parm                    Tibs34ds
735400060130     c                   In        *dtaara
735500060130    1c                   EndIf
735600060130
735700060130     c                   Clear                   wabi
735800060130     c                   Clear                   v1cmsg
735900060130     c                   Clear                   dLat
736000060130
736100060130      * Verifica errori e autorità profilo
736200060130s   1c                   Select
736300060130      * se ho errori nei dati utente esco dal pgm
736400060130w   1c                   When      DutErr = 'E'
736500060130     c                   GoTo      Fine
736600060130      * se non c'è l'abilitazione
736700060130      * --> se 1° livello, abilitazioni al terminal
736800060130      *     se 2° livello, abilitazioni al punto operativo
736900060130      *     se sede è impossibile ma se capita mando a fine il pgm
737000060130w   1c                   When      UteAut = *Blanks
737100060130if  2c                   If        DutLpo = '1'
737200060130     c                   Eval      wabi   = 'TP'
737300060130e   2c                   EndIf
737400060130if  2c                   If        DutLpo = '2'
737500060130     c                   Eval      wabi   = 'PO'
737600060130e   2c                   EndIf
737700060130if  2c                   If        DutLpo = 'S'
737800060130     c                   GoTo      Fine
737900060130e   2c                   EndIf
738000060130      * carica le abilitazioni del profilo
738100060130x   1c                   Other
738200060130     c                   Movel     UteFaf        Dute01
738300060131     c                   If        §Utembf <> *Blanks
738400060131     c                   Eval      wabi = §Utembf
738500060130     c                   Else
738600060130     c                   Eval      wabi = UteAut
738700060130     c                   EndIf
738800060130e   1c                   EndSl
738900060130
739000060130      * controllo se ok l'abilitazione dell'utente
739100060130     c                   Clear                   Tibs02ds
739200060130     c                   Eval      T02Mod = 'C'
739300060130     c                   Eval      T02Sif = knsif
739400060130     c                   Eval      T02Cod = 'LAT'
739500060130     c                   Movel(p)  wabi          T02Ke1
739600060130     c                   Call      'TIBS02R'
739700060130     c                   Parm                    kpjba
739800060130     c                   Parm                    Tibs02ds
739900060130if  1c                   If        T02Err = *Blanks
740000060130     c                   Eval      dLat = T02Uni
740100060130e   1c                   EndIf
740200060130      * errore o non abilitato
740300060130if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
740400060130     c                   Seton                                        28
740500060130     C                   movel     msg(26)       v1cmsg
740600060130   x1c                   Else
740700060130      * Reperimento dei P.O. gestibili dall'utente
740800060130     c                   clear                   TRUL31DS
740900060130     c                   clear                   TRUL31DS2
741000060130     c                   eval      I31abi = wabi
741100060130     c                   eval      I31cdi = DUTdis
741200060130     c                   eval      I31car = DUTare
741300060130     c                   eval      I31cpo = DUTpou
741400060130     c                   call      'TRUL31R'
741500060130     c                   parm                    KPJBA
741600060130     c                   parm                    TRUL31DS
741700060130     c                   parm                    TRUL31DS2
741800060130if  2c                   if        O31pog > *zeros
741900060130     c                   movea     O31pog        $pog
742000060130     c                   movea     O31arg        $arg
742100060130     c                   movea     O31dig        $dig
742200060130x   2c                   else
742300060130     c                   Seton                                        28
742400060130     c                   movel     MSG(26)       v1cmsg
742500060130e   2c                   endif
742600060130    1c                   endif
742700060130     c
742800060130     c                   movel     savkpjbu      kpjbu
742900971223     C*
743000971223     C* IMPOSTAZIONE CAMPI FISSI
743100940907     C                   MOVEL     *ZERO         COMLPN            3
743200940907     C                   MOVEL     *ZERO         COMNRN            2
743300940907     C                   MOVEL     *ZERO         COMNSN            7
743400950307     C                   MOVEL     *ZERO         COMLAN            3
743500950324     C                   Z-ADD     4             WNPG
743600940713      *
743700940713     C                   Z-ADD     *ZERO         NR2               4 0
743800940713     C                   Z-ADD     *ZERO         NR6               4 0
743900940713     C                   Z-ADD     *ZERO         NR8               4 0
744000940802     C                   MOVEL     *BLANKS       WF03              1
744100070403     c
744200070403     c* Verifico se profilo utente ha GEO attiva
744300131205     c* Per EDP sempre anche prr RAC999
744400131205     c                   if        %subst(knmus:1:3)='EDP' or
744500131205     c                             %subst(knmus:1:6)='RAC999'
744600070410     c                   eval      dutgeot='S'
744700070410     c                   else
744800070403     c     dutpou        chain     azorg01l
744900070403     c                   if        %found(azorg01l)
745000070403     c                   movel     orgde8        og148
745100070403     c                   else
745200070403     c                   clear                   og148
745300070403     c                   endif
745400070410     c
745500070524     c                   if        §oggeot<>' ' or §ogtlls<>' '
745600070524     c                   movel     'S'           dutgeot           1
745700070410     c                   endif
745800070524     c                   endif
745900131007
746000131007      /free
746100131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
746200131007      /end-free
746300140319     c* Tasto funz orari servizi sempre abilitato
746400140319     c                   eval      wabif9='S'
746500131007
746600940713      *
746700940713     C                   ENDSR
746800160115     C**
746900971223     C* ELABORAZIONE DI SINGOLA BOLLA --------------------------------*
747000971223     C     SINGOL        BEGSR
747100971223     C                   CLEAR                   WINIZ
747200971223     C                   CLEAR                   WFINE
747300971223     C*
747400971223     C* 97 ON - MANUTENZIONE BOLLA
747500060130    1C     *IN97         IFEQ      *ON
747600971223     C*
747700160115     c                   if        wchainblp='S'
747800160115     c     blpnrr        chain     fnblp00f
747900160115     c                   endif
748000060224     c                   movel     arblna        w003a
748100060224     c     w003a         lookup    $pog                                   31
748200060130     c* Se la causale variazione bolle prevede anche il p.o. cliente
748300131111     c*    controllo anche questo
748400060130    2c                   if        not *in31 and d66pocl='S'
748500060130     c     arbcbo        lookup    cbodop                                 31
748600060130    3c                   if        not *in31
748700060130     c                   movel     arbksc        kscfil
748800060130   x3c                   else
748900060130     c* Se bolla doppia prendo il ksc dall'AR6
749000060130     C                   Z-ADD     ARBAAS        KAAS
749100060130     C                   Z-ADD     ARBLNP        KLNP
749200060130     C                   Z-ADD     ARBNRS        KNRS
749300060130     C                   Z-ADD     ARBNSP        KNSP
749400060130     C                   MOVEL     '2'           KTRC
749500060130     C     KAR61         CHAIN     FIAR6000                           31
749600060130if  4C                   IF          %FOUND(fiar601l)
749700060130     c                   movel     ar6ksc        kscfil
749800060130     c                   else
749900060130     c                   movel     999           kscfil
750000060130    4c                   endif
750100060130    3c                   endif
750200060130     c
750300060224     c                   movel     kscfil        w003a
750400060224     c     w003a         lookup    $pog                                   31
750500060130    2c                   endif
750600060130     c
750700060130     C     *IN31         IFEQ      *OFF
750800971223     C                   MOVEL     MSG(19)       V1CMSG
750900971223     C                   SETON                                        459028
751000971223     C                   GOTO      FOR01
751100971223     C                   ELSE
751200971223     C*
751300991027     C                   CLEAR                   FNLR48DS
751400060224     c* come p.o. gestione prendo il p.o. gestore (da £6) della linea
751500060224     c*  di arrivo
751600060224     c                   clear                   fnlv55ds
751700060224     c                   movel     arblna        d55lin
751800060224     c                   movel     '6'           d55tpt
751900060224     C                   time                    wrkoggiiso
752000060224     C     *iso          move      wrkoggiiso    d55drf
752100060224     c                   call      'FNLV55R'
752200060224     c                   parm                    fnlv55ds
752300060224     c                   movel     d55tfa        d48fgs
752400060224     c
752500131203     c                   select
752600140117     C     v1cbarcode    whengt    0
752700140117     C                   MOVEL     ARbAAS        D48AAS
752800140117     C                   MOVEL     ARbLNP        D48LNP
752900140117     C                   MOVEL     ARbNRS        D48NRS
753000140117     C                   MOVEL     ARbNSP        D48NSP
753100131203     C     SEL2          whenne    0
753200980403     C                   MOVEL     V1CAAS        D48AAS
753300980403     C                   MOVEL     V1CLP2        D48LNP
753400980403     C                   MOVEL     V1CNRS        D48NRS
753500980403     C                   MOVEL     V1CNSP        D48NSP
753600131203     C                   other
753700111011     C*                  MOVEL     AR4AAS        D48AAS
753800111011     C*                  MOVEL     AR4LNP        D48LNP
753900111011     C*                  MOVEL     AR4NRS        D48NRS
754000111011     C*                  MOVEL     AR4NSP        D48NSP
754100111011     C                   MOVEL     v2cAAS        D48AAS
754200111011     C                   MOVEL     v2clnp        D48LNP
754300111011     C                   MOVEL     v2cNRS        D48NRS
754400111011     C                   MOVEL     v2cNSP        D48NSP
754500131203     C                   ENDsl
754600141010     c* routine per verifica dispo da esaminare
754700141010     c                   exsr      sr_wdwdc
754800141010
754900971223     C                   MOVEL     V1CCVB        D48CVB
755000971223     C                   MOVEL     ARBCBO        D48CBO
755100971223     C                   MOVEL     'V'           D48FFR
755200971223     C                   MOVEL     'A'           D48TBO
755300971223     C                   MOVEL     'S'           D48F12
755400991027     C                   MOVEL     FNLR48DS      KPJBU
755500971223     C*
755600971223     C                   CALL      'FNLR48R'
755700971223     C                   PARM                    KPJBA
755800971223     C                   PARM                    DSARBD
755900971223     C                   PARM                    DSARBK
756000991011     C                   PARM                    DARBT
756100971223     C                   PARM                    DSARBG
756200030612     C                   PARM                    trul90ds
756300971223     C*
756400991027     C                   MOVEL     KPJBU         FNLR48DS
756500060307     c                   movel     'S'           wriem
756600971223     C* ERRORE
756700971223     C     D48ERR        IFEQ      '1'
756800971223     C                   MOVEL     D48MSG        V1CMSG
756900971223     C                   SETON                                        284790
757000971223     C                   GOTO      FOR01
757100971223     C                   ENDIF
757200971223     C                   ENDIF
757300971223     C*
757400971223   X2C                   ELSE
757500131111   c
757600971223     C* VISUALIZZAZIONE BOLLA
757700131111     c                   select
757800131111     C     v1cbarcode    whengt    0
757900131111     C                   MOVEL     arbAAS        V2CAAS
758000131111     C                   MOVEL     arblnp        COMLNP
758100131111     C                   MOVEL     arbNRS        V2CNRS
758200131111     C                   MOVEL     arbNSP        V2CNSP
758300140117     C     SEL2          whengt    0
758400140117     C                   MOVEL     V1CAAS        V2CAAS
758500140117     C                   MOVEL     V1CLP2        COMLNP
758600140117     C                   MOVEL     V1CNRS        V2CNRS
758700140117     C                   MOVEL     V1CNSP        V2CNSP
758800131111     C                   ENDSL
758900131111     c
759000971223     C                   EXSR      BOLLA
759100971223     C*
759200971223     C                   EXSR      VISUAL
759300971223    2C                   ENDIF
759400971223     C*
759500971223    2C     PA1FLG        IFNE      '1'
759600971223     C     PA1FLG        ANDNE     'N'
759700971223     C     PA1FLG        ANDNE     '3'
759800971223     C     PA1FLG        ANDNE     'P'
759900971223     C     PA1FLG        ANDNE     'E'
760000990129     C     PA1FLG        ANDNE     'C'
760100971223     C                   MOVEL     '1'           WINIZ             1
760200971223     C                   ELSE
760300971223     C                   MOVEL     '1'           WFINE             1
760400971223    2C                   END
760500971223    2C                   ENDSR
760600110601      **************************************************************************
760700110601      *   emissione window email destinatario
760800110601      **************************************************************************
760900110601     C     sr_emdest     BEGSR
761000110601     c                   clear                   v4cemw
761100110601     c
761200110601     c                   eval      ilv79TLA='L'
761300110601     c                   eval      ilv79Aas=arbaas
761400110601     c                   eval      ilv79Lnp=arblnp
761500110601     c                   eval      ilv79nrs=arbnrs
761600110601     c                   eval      ilv79nsp=arbnsp
761700110601     c                   call      'FNLV79R'
761800110601     c                   parm                    fnlv79ds
761900110601     c
762000110601     c                   endsr
762100091214      **************************************************************************
762200131114      *   Emissione del Beneficiario C/Ass
762300091214      **************************************************************************
762400091214     C     sr_bencas     BEGSR
762500091215     c                   clear                   vidben
762600091214     c                   clear                   wdaben
762700091214     c                   clear                   wdasrsc
762800091214     c                   clear                   wdascdi
762900091216     c                   clear                   trulintds
763000091216     c
763100091216     c                   eval      iinttip='B'
763200091216     c                   eval      iinttic=vidtic
763300091216     c                   eval      iintrscm=v4crsm
763400091216     c                   eval      iintrsco=vidrmo
763500091216     c                   eval      iintCDI =w_cdi
763600091216     c                   call      'TRULINTR'
763700091216     c                   parm                    trulintds
763800091216     c
763900091216     c                   movel     ointrsci      wdasrsc
764000091216     c
764100091216     c                   select
764200091216     c                   when      ointtipi='O'
764300091216     c                   eval      wdaben='MITTENTE  ORIGINALE'
764400091216     c
764500091216     c                   when      ointtipi='R'
764600091216     c                   eval      wdascdi=w_cdi
764700091216     c                   eval      wdaben=' O R D I N A N T E'
764800091216     c
764900091216     c                   when      ointtipi='M'
765000091216     c                   if        v3cksc>0
765100091216     c                   movel     v3cksc        wdascdi
765200091216     c                   endif
765300091216     c                   eval      wdaben='  M I T T E N T E'
765400091216     c                   ENDSL
765500091214     c
765600131107     c                   exfmt     LRI6wdas
765700091214     c                   endsr
765800130830      **************************************************************************
765900130830      *   window per visualizzare gli orari servizi
766000130830      **************************************************************************
766100130830     C     sr_OrariSr    BEGSR
766200130920
766300140110     c                   clear                   trulorSds
766400151019     c***                eval       s_IOREtfp = ARBtfp
766500151019     c* se il campo "terminal didisguido" è pieno, passo questo al posto
766600151019     c* del terminal di partenza originale
766700151019     c                   if         s_arbflp>*zeros
766800151019     c                   eval       s_IOREtfp=%INT(s_arbflp)
766900151019     c                   else
767000151019     c                   eval       s_IOREtfp=arbtfp
767100151019     c                   endif
767200140110     c                   eval       s_IOREtfa = ARBtfa
767300140110     c                   eval       s_IOREdti = ARBdti
767400140110     c                   eval       s_IOREhti = ARBhti
767500140110     c                   eval       s_IOREdcr = ARBdcr
767600140110     c                   eval       s_IOREhcr = ARBhcr
767700140110     c                   eval       s_IOREtcr = ARBtcr
767800140110     c                   eval       s_IOREDEI = s_ARBdei
767900140110     c                   eval       s_IOREOEI = s_ARBoei
768000140110     c                   eval       s_IOREfil = ARBlna
768100140110     c                   eval       s_IOREcap = ARBcad
768200140110     c                   eval       s_IOREloc = ARBlod
768300140110     c                   eval       s_IOREnar = ARBnzd
768400150724     c                   clear                   diore01
768500150728     c                   movel     s_arbttc      §ioretrazC
768600150728     c                   movel     s_arbtcc      §ioreconsC
768700150724     c                   eval       s_IOREflo = diore01
768800150724     c
768900140113     c* Data di Riferimento: data consegna se consegnata
769000140113     c                   select
769100140113     c                   when      arbdcm>0 and arbica<>' ' and arbica<>'R'
769200140113     c                             and arbicc<>' ' and arbicc<>'R'
769300140113     c                   eval       s_ioredta=arbdcm
769400140113     c* per ritorno all'incasso data del giorno
769500140113     c                   when      arbdcm>0 and arbddc>0
769600140113     C                   time                    wrkoggiiso
769700140113     C     *iso          move      wrkoggiiso    s_ioredta
769800140113     c                   other
769900140113     c* altrimenti data prevista consegna se > = oggi
770000140113     C                   time                    wrkoggiiso
770100140113     C     *iso          move      wrkoggiiso    s_ioredta
770200140113     c* se < di oggi, imposto oggi
770300140113     c                   if        amgdce>s_ioredta
770400140113     c                   eval       s_ioredta=amgdce
770500140113     c                   endif
770600140113     c                   endsl
770700140113     c
770800140110     c                   eval       s_IOREtser = 'C'
770900140409     c                   eval       s_IOREDDC=arbddc
771000140409     c                   eval       s_IOREnDC=arbndc
771100140409     c
771200140110     c                   IF         *in19 or
771300140110     c                              (ARBfbc = *blanks and
771400140110     c                               ARBddc > 0 and
771500140110     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
771600140110     c                               arbica='R' or arbicc=' ' or arbicc='R'))
771700140110     c                   eval       s_IOREcons = 'S'
771800140225     c* Orario presunto ritiro/consegna
771900140225     c                   move      arbndc        pctndc
772000140225     c                   move      arbifp        pctfgs
772100140225     c                   move      'COR'         ttrd
772200140225     c     kcet          setgt     fipct02l
772300140225     c     kcet          readpe    fipct02l
772400140310     c                   if        not %eof(fipct02l)
772500140225     c                   movel     pctdati       fipctcords
772600140307     c                   eval      s_ioreorp=§PCTORASTI
772700140225     c                   endif
772800140225     c
772900140110     c                   ENDIF
773000140225     c
773100140110     c                   eval       s_IOREtsp = ARBtsp
773200130920
773300140325     c                   eval      s_iorecol=36
773400140110     c                   call      'TRULORSR'
773500130920     c                   parm                    kpjba
773600140110     c                   parm                    trulorSds
773700130830     c
773800131111     c                   ENDSR
773900131111      **************************************************************************
774000131111      *   Prepara SQL per rsd ind lod rsm
774100131111      **************************************************************************
774200131111     C     Presql        BEGSR
774300131111     //free
774400160113      // per ora non lo faccio più  eseguire il join per problemi di prestazione
774500160113      //  la ricerca viene fatta solo sulla ragione sociale di ARB primi 35 caratteri
774600160113      //   if v1cxrsd='X'  ;
774700131114
774800160113      //   wSQL ='SELECT fnarb00f.*  +
774900160113      //             from FNARB00F left outer join FIAR400F on arbaas=ar4aas +
775000160113      //         and arblnp=ar4lnp and arbnrs=ar4nrs and arbnsp=ar4nsp  +
775100160113      //          and ar4trc=''D'' where arbnzd='''+ v1cnzd +''' '  ;
775200160113      //          else  ;
775300131114
775400131114         wSQL ='SELECT * from FNARB00F   where arbnzd='''+ v1cnzd +''' '  ;
775500160113      //   endif  ;
775600131114
775700131114       if v1cnzd=*blanks   ;
775800131114         wSQL +=' and arbprd=''' + v1cprd + ''' '   ;
775900131111       endif  ;
776000131111
776100131115       if (v1cssp=' ' or v1cssp='1')  ;
776200131115        if comdtd>0 and comdta>comdtd  ;
776300131115           wSQL += ' and (arbaas*10000)+arbmgs>=' + %editc(comdtd:'X')+ ' and +
776400131115                        (arbaas*10000)+arbmgs<=' + %editc(comdta:'X')  ;
776500131115        endif  ;
776600131115        if comdtd>0 and (comdta=0 or comdta=comdtd)      ;
776700131115           wSQL += 'and arbaas=' + %editc(comaad:'X')+ ' and arbmgs=' +
776800131115                                %editc(commgd:'X')  ;
776900131115        endif  ;
777000131115       endif  ;
777100131111
777200131111       // se voglio le "IN CONSEGNA", parzializzo
777300131111       if v1cssp='2'  ;
777400131113           wSQL += ' and (arbdcm=0 or arbica='' '' or arbica=''R'' +
777500131113                     or arbicc='' '' or arbicc=''R'')'  ;
777600131111       endif  ;
777700131114
777800131114       // Se voglio le "DA CONSEGNARE", parzializzo
777900131111       if v1cssp='1'  ;
778000131111           wSQL += ' and arbifp=0 and arbndc=0 and arbnss=0' ;
778100131111       endif  ;
778200131111
778300131114       // Se voglio le "CONSEGNATE", parzializzo
778400131111       if v1cssp='3'  ;
778500131111           wSQL += ' and arbdcm>0' ;
778600131111       endif  ;
778700131111
778800131111       // linea di arrivo
778900131111       if v1clna>0  ;
779000131111           wSQL += ' and arblna=' + %editc(v1clna:'X') ;
779100131111       endif  ;
779200131111
779300131111        if v1crsd<>*blanks  ;
779400131111
779500131111         if v1cxrsd =' '   ;
779600131114           wSQL += ' and arbrsd    like(''';
779700131111         else  ;
779800160113           wSQL += ' and arbrsd    like(''%';
779900160113          // non esegue più il joi
780000160113          //  wSQL += ' and concat(arbrsd, ifnull(ar4not, '' '')) like(''%'  ;
780100131111         endif ;
780200131111
780300131209         w060a=v1crsd  ;
780400131209         exsr APICI  ;
780500131209
780600131209           wSQL += w060a ;
780700131111           wSQL  = %trim(wSQL) + '%'')' ;
780800131111         endif ;
780900131111
781000131111        if v1cIND<>*blanks  ;
781100131111
781200131111         if v1cxind =' '   ;
781300131111           wSQL += ' and arbind like(''';
781400131111         else  ;
781500131113           wSQL += ' and arbind like (''%' ;
781600131111         endif ;
781700131111
781800131209         w060a=v1cind  ;
781900131209         exsr APICI  ;
782000131209           wSQL += w060a ;
782100131111           wSQL  = %trim(wSQL) + '%'')' ;
782200131111         endif ;
782300131111
782400131111        if v1cLOD<>*blanks  ;
782500131111
782600131111         if v1cxlod =' '   ;
782700131111           wSQL += ' and arblod like(''';
782800131111         else  ;
782900131113           wSQL += ' and arblod like (''%' ;
783000131111         endif ;
783100131111
783200131209         w060a=v1clod  ;
783300131209         exsr APICI  ;
783400131209           wSQL += w060a ;
783500131111           wSQL  = %trim(wSQL) + '%'')' ;
783600131111         endif ;
783700131111
783800131111        if v1cRSM<>*blanks  ;
783900131111
784000131111         if v1cxrsm =' '   ;
784100131111           wSQL += ' and arbrsm like(''';
784200131111         else  ;
784300131113           wSQL += ' and arbrsm like (''%' ;
784400131111         endif ;
784500131111
784600131209         w060a=v1crsm  ;
784700131209         exsr APICI  ;
784800131209           wSQL += w060a ;
784900131115           wSQL  = %trim(wSQL) + '%'')' ;
785000131111         endif ;
785100131115
785200131115           wSQL += ' order by arbnzd, arbprd, arbaas, arbmgs'  ;
785300131111
785400131111        ENDSR  ;
785500131111       //--------------------------------------------------------------
785600131111       //?Apertura cursore.                                            ?
785700131111       //--------------------------------------------------------------
785800131111       BEGSR  OpenCursor;
785900131111
786000131111         wEnd = *off;
786100131111
786200131111       //?Dichiarazione cursore?
786300131111         exec sql
786400131111         PREPARE S1 from :wSQL;
786500131209
786600131209         IF sqlcode < 0;
786700131209           wEnd = *on;
786800131209           leavesr        ;
786900131209         ENDIF;
787000131209
787100131111         exec sql
787200131111         DECLARE ARB  cursor for S1;
787300131111
787400131111       //?Apertura del cursore
787500131111         exec sql
787600131111         OPEN ARB;
787700131111
787800131111         IF sqlcode < 0;
787900131111           wEnd = *on;
788000131209           leavesr        ;
788100131111         ENDIF;
788200131209
788300131209           WopenC='1'     ;
788400131209
788500131111
788600131111       ENDSR;
788700131209       //--------------------------------------------------------------
788800131209       //?Raddoppio apici nella stringa                                ?
788900131209       //--------------------------------------------------------------
789000131209       BEGSR  Apici     ;
789100131209       ap=1  ;
789200131209       dow ap <=60  ;
789300131209       if %subst(w060a:ap:1)=''''  ;
789400131209       w060a=%replace('''''':w060a:ap:1)  ;
789500131209       ap= ap   +1       ;
789600131209       endif  ;
789700131209
789800131209       ap= ap   +1       ;
789900131209       enddo  ;
790000131209
790100131209       ENDSR;
790200131111       //--------------------------------------------------------------
790300131111       //?Chiusura cursore.
790400131111       //--------------------------------------------------------------
790500131111       BEGSR CloseCursor;
790600131111
790700131111       //?Chiusura cursore SQL?
790800131111         exec sql
790900131111           close ARB;
791000131209           clear WopenC   ;
791100131209           Wend=*off      ;
791200131111
791300131111       ENDSR;
791400131111       //--------------------------------------------------------------
791500131111       //?Lettura cursore.                                             ?
791600131111       //--------------------------------------------------------------
791700131111       BEGSR  ReadCursor;
791800131111
791900131111         clear  FNARBds;
792000131113         *in30=*off    ;
792100131113         *in32=*off    ;
792200131111
792300131111         exec sql
792400131111         FETCH next from ARB into :FNARBds;
792500131111
792600131111         IF sqlcod = 100 or sqlcod < 0;
792700131111           wEnd = *on;
792800131111         ENDIF;
792900131111
793000131111       ENDSR;
793100141010       //--------------------------------------------------------------
793200141010       //?Window per f19=Dispo di consegna da esaminare
793300141010       //--------------------------------------------------------------
793400141010       BEGSR sr_wdwdc   ;
793500141010          $fine=*off;
793600141105          clear fnlry09ds2;
793700141010          ilry09tch='C';
793800141010          ILRY09AAS=d48aas ;
793900141010          ILRY09LNP=d48LNP;
794000141010          ILRY09NRS=d48NRS ;
794100141010          ILRY09NSP=d48NSP ;
794200141105          kpjbu=fnlry09ds2;
794300141105          fnlry09r(kpjba);
794400141105          fnlry09ds2=kpjbu;
794500141010          if olry09esit='1' ;
794600141010             dow $fine=*off;
794700141010                exfmt lri6wddc;
794800141010                   if *inkt;
794900141105                       clear fnlry09ds2;
795000141010                       ilry09tch='E';
795100141010                       ILRY09AAS=d48aas ;
795200141010                       ILRY09LNP=d48LNP;
795300141010                       ILRY09NRS=d48NRS ;
795400141010                       ILRY09NSP=d48NSP ;
795500141105                       ILRY09DF06='S';
795600141105                       kpjbu=fnlry09ds2;
795700141105                       fnlry09r(kpjba);
795800141010                       $fine=*on;
795900141010                   endif;
796000141010             enddo;
796100141010          endif ;
796200141010
796300141010       ENDSR;
796400131111
796500131111
796600131111
796700131111      /end-free
796800131111
796900940713      **************************************************************************
797000941014**        12345678901234567890123456789012345678901234567890
797100950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
797200930413**
797300950316LEGATA A PIU' BOLLE                       - F18=Visualizza
797400920326**
797500950316 Non Presente
797600950316 F18=Visualizza
797700940907Vedi in Int.Par
797800940907Vedi in Int.Arr
797900941011Vedi Arch.Stor.
798000940907**
798100940907Spedizione non Presente
798200941230
798300940907Vedi in Int.Bolle Part.
798400940907Vedi in Int.Bolle Arr.
798500941011Vedi in Archiv. Storico
798600940713**  MESSAGGI DI ERRORE VIDEO  **************************************************
798700940713Nessuna bolla da visualizzare                                                  1
798800940713Immettere linea di arrivo valida                                               2
798900131114Effettuare ulteriori parzializzazioni                                          3
799000940713Immettere linea di arrivo o linea di partenza                                  4
799100940713Immettere linea di partenza valida                                             5
799200940713Numero spedizione inesistente                                                  6
799300940713Immettere anno spedizione                                                      7
799400940713Immettere data formalmente valida                                              8
799500040216Immettere tipo bolla valido                                                    9
799600940713Immettere data di spedizione iniziale minore o uguale a quella finale         10
799700940713Immettere numero di serie iniziale minore o uguale a quello finale            11
799800940713Immettere numero segnacollo iniziale minore o uguale a quello finale          12
799900940713Immettere numero segnacollo finale, se immesso quello iniziale                13
800000131114Scelta incongruente tra stato spedizioni  e richiesta                         14
800100940713Fine scorrimento                                                              15
800200940715Immettere tipo servizio bolle valido                                          16
800300940923Immettere filiale segnacollo valida                                           17
800400060614Riferimento partner estero errato x bolle DPD:o lungo 11 numerico a 14 alfanum
800500941212Bolla non manutenzionabile: in gestione ad altra filiale                      19
800600991227Il riferimento partner estero per bolle DPD deve avere massimo 12 numeri
800700010508Immettere p.o. di emissione valido                                            21
800800010510Non trovate bolle per l' O.R.M. richiesto                                     22
800900020626Immettere la nazione e la provincia del destinatario                          23
801000020627Nazione destinatario errata                                                   24
801100020627Provincia destinatario errata                                                 25
801200060130Utente non abilitato alla Variazione bolle                                    26
801300060130Utente non abilitato alla variazione bolle del p.o. in gestione               27
801400060613ATTENZIONE!Saranno selez.bolle con Nazione e RagSoc impostate ma PROVINC.vuota28
801500070420Stampa LDV effettuata ...                                                     29
801600070420Stampa LDV al momento non effettuabile: ritentare o telefonare in Sede        30
801700080201Assegnato non tassato: stampa LdV non effettuabile                            31
801800131111non trovata bolla per il barcde digitato                                      32
801900030612**  cmA4
802000030612OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
802100111221 SHARE(*YES) USRDTA('Man.BolA4')
802200030612**  cmA5
802300030612OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
802400111221 SHARE(*YES) USRDTA('Man.BolA5')
802500070426**  cmA4f6
802600111221OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
802700111221USRDTA('Rist.Bolle') SECURE(*YES)
802800070426**  cmA5f6
802900070426OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
803000111221             USRDTA('Rist.Bolle') SECURE(*YES)
803100030612** cnm
803200991222 0123456789
803300040218** TF                                                                        *
803400070420F1=Agg.Rich#                                                                    1
803500070411** TF01B                                                                     *
803600070411F1=SCELTA BOLLA#                                                                1
803700021216** TF                                                                        *
803800021218F2=C.A.#                                                                        2
803900021216** TF                                                                        *
804000021216F4=Tass2#                                                                       4
804100021216** TF                                                                        *
804200021216F5=Eventi#                                                                      5
804300070420** TF                                                                        *
804400070420F6=StampaLDV#                                                                   5
804500021216** TF                                                                        *
804600021217F7=Anomalie#                                                                    7
804700021216** TF                                                                        *
804800021219F8=Discord#                                                                     8
804900130830** TF                                                                        *
805000130830F9=OrariSr#                                                                     9
805100021216** TF                                                                        *
805200070420F10=Variaz#                                                                     10
805300021216** TF                                                                        *
805400030121F11=NoteS/B#                                                                    11
805500021217** TF                                                                        *
805600021217F12=Ritorno#                                                                    11
805700021216** TF                                                                        *
805800070420F13=Tassaz#                                                                     13
805900021217** TF                                                                        *
806000021216F14=Giacenza#                                                                   14
806100040218** TF                                                                        *
806200070420F16=Gest.Rich#                                                                  16
806300070404** TF                                                                        *
806400140311F17=NoteAut#                                                                    16
806500021216** TF                                                                        *
806600021216F19=Spunte#                                                                     19
806700021216** TF                                                                        *
806800070420F20=Affid#                                                                      19
806900021216** TF                                                                        *
807000070420F21=DetColli#                                                                   19
807100110928** TF                                                                        *
807200110930F22=PerfectOrder#                                                               19
