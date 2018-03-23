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
045600151019     d s_arbflp        S                   LIKE(d98flp)
045700131114     D arbrsduni       S             70
045800131114     D arbdsp          S              8  0
045900131111     D W0080           S              8  0
046000991027     D W0040           S              4  0
046100991027     D wtibs69r        s              1
046200991027     D wtstdec         s              1
046300131120     D Wvideo          s              1
046400131209     D WopenC          s              1
046500020225     d lnantw          s                   like(§ogntw)
046600020225     d lnpntw          s                   like(§ogntw)
046700020910     d Wifp            s                   Like(Arbifp) Inz
046800060130     d wabi            s                   like(UteAut)
046900030703
047000030703     d wtba            s                   Like(§Ar5Tba)
047100030703     d wdestba         s                   Like(VidTba)
047200061018     d savnzd          s                   inz  like(v1cnzd)
047300020628
047400091216     D W_cdi           S                   LIKE(§ar5cdi)
047500020628     D ordina          S              1
047600020628     D comando         S              1
047700070420     D wrichtrul90     S              1
047800070403     d Dataeur         s               d   datfmt(*eur)
047900070403     d Datadmy         s               d   datfmt(*dmy)
048000110929     d s_param1        s                   like(param1)
048100110929     d no_f22          s              1    inz(' ')
048200110930     d comrma          s                   like(arbrma)
048300131108     d Wcol_D          s              5  0
048400131108     d Wcol_I          s              5  0
048500131108     d Wcarat          s              3  0
048600131108     d Indx            s              3  0
048700131209     d AP              s              3  0
048800131108     d wx              s              3  0
048900131007     d wy              s              3  0
049000131209     d w060a           s             60
049100131209     d wabif9          s              1
049200141010     d $Fine           s              1    inz(*off)
049300141010       // -?Reperimento Disposizioni di consegna da WEB
049400141010     d fnlry09r        pr                  extpgm('FNLRY09R')
049500141013     d  kpjba                              likeds(kpjba)
049600131111     d
049700131111       // -?Stringa SQL da eseguire
049800131111     d wSQL            s           2048    Varying        inz
049900030527     D*
050000030527     D wrkoggiiso      s               d
050100991027      *
050200930705     D* DEFINIZIONI COSTANTI
050300930705     D*
050400930705     D CNOSEQ          C                   CONST('NON SEQUENZIALI')
050500941014     D CINTER          C                   CONST('* INTERROGAZIONE BOL-
050600941014     D                                     LE ARRIVI *')
050700941014     D CMANUT          C                   CONST('** MANUTENZIONE BOLL-
050800941014     D                                     E ARRIVI **')
050900021216     D CF2413          C                   CONST('F24=AlTasti(1/3)')
051000021216     D CF2423          C                   CONST('F24=AlTasti(2/3)')
051100021216     D CF2433          C                   CONST('F24=AlTasti(3/3)')
051200021216     D CF2412          C                   CONST('F24=AlTasti(1/2)')
051300021216     D CF2422          C                   CONST('F24=AlTasti(2/2)')
051400110705     d
051500110705     D* CAMPO PER DSPATR "NO DISPLAY"
051600110705     D NODI            C                   CONST(X'27')
051700131120     D* CAMPO PER DSPATR "RI"
051800131120     D RI              C                   CONST(X'21')
051900131127     D* CAMPO PER DSPATR "HI"
052000131127     D hi              C                   CONST(X'22')
052100900518     C****************************************************************
052200900518     C*  RIEPILOGO INDICATORI
052300900518     C***************************************************************
052400911023     C* 01    - SELEZIONE PER LINEA ARRIVO
052500911023     C* 02    - SELEZIONE PER LINEA PARTENZA
052600911105     C* 03    - SPEDIZIONE GIACENTE
052700990119     C* 04    - ABILITO TASTO FUNZIONALE F2 - INT C.A.
052800990119     C* 05    - ABILITO TASTO FUNZIONALE F19- SPUNTE
052900070411     c* 06    - on per tassazione 2 bolla emette scritta "ImportoBOLLO"
053000070411     c*       - offper tassazione 2 bolla emette scritta "Importo IVA"
053100911023     C* 07    - PARZIALIZZ. SEGNACOLLO DAL
053200000216     C* 08    - bolla dpd
053300131114     C* 09    - scelta in altre parzializz
053400961111     C* 10    - BOLLA DI RIENTRO
053500911023     C* 11    - PARZIALIZZ. MITTENTE
053600131120     C* 12    -
053700961111     C* 13    - SPEDIZIONE IN CONSEGNA IN EVIDENZA IN SUBFILE
053800911023     C* 14    - PARZIALIZZ. DESTINATARIO
053900940318     C* 15    - DATA DAL UGUALE A DATA AL (SCELTO UN SOLO GIORNO)
054000961112     C* 16    - SI TRATTA DI BOLLA EXPORT
054100070403     C* 17    - p.o. utente e lna bolla hanno GEO attiva
054200911023     C* 18/21 - STATO SPEDIZIONI SELEZIONATO
054300991027     C* 22    - ESISTE ESTENSIONE BOLLA 2 e ABILITO IL TASTO  F4=TASSAZIONE 2
054400950825     C* 23    - SFLEND
054500950825     C* 24    - ABILITO IL TASTO "F13=TASSAZIONE"
054600010508     C* 25    - SELEZIONE PER NUMERO O.R.M.
054700070404     C* 26    - presente FIARP
054800070403     C* 27    - presente blocco consegna ARBFBC<>' '
054900941028     C* 28    - VISUALIZZA MSG ERRORE
055000911129     C* 29    - ESISTE CONTRASSEGNO
055100911023     C* 30/31 - LETTURA COMODO
055200911023     C* 32    - BOLLA LETTA DA SCARTARE
055300911023     C* 33    - BOLLA IN FRANCO
055400911023     C* 34    - LETTURA SUBFILE
055500911023     C* 35    - PULIZIA SUBFILE
055600911129     C* 36    - LETTURE
055700911023     C* 37    - ROLL UP
055800991027     C* 38    - ESISTE TB2 ANCHE SE NON PRESENTE TIPO RECORD '2' IN FIAR600F
055900920203     C* 39    - ABILITAZIONE TASTO DI COMANDO INT. BOLLE PARTENZE
056000131111     C* 40-59 - ERRORI
056100131111     C* 60    - email destiantario
056200131111     C* 61    - PROGRAMMA RICHIAMATO per scelta bolla
056300930407     C* 62    - ALTA INTENSITA' PER SPED BLOCCATE
056400040218     C* 63    - Abilita F1  = Aggiunta in reclamo
056500040218     C* 64    - Abilita F16 = Gestione reclamo
056600091214     c* 65    - Abilita visualizzazione beneficiario C/Ass
056700131127     C* 66    - libero - NON usato
056800960903     C* 67    - ABILITAZIONE TASTO DI COMANDO F7=ANOMALIE
056900131127     C* 68    - pos cursore RMN prima serv x  EMETTERE F23 PER CALCOLARE E VISUALIZZARE DATA CONS.
057000930505     C*         PREVISTA E LE ORE DI RITARDO
057100930526     C* 69    - DATA CONSEGNA MERCE >< 0
057200930510     C* 70    - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
057300990119     C* 71    - ABILITAZIONE TASTO DI COMANDO F8=EVENTI SEGNACOLLI
057400131107     C* 72    - SFLDSPCTL  LRI6S05
057500991027     C* 73    - BOLLA FRANCO FRONTIERA
057600970227     C* 74    - ABILITA EVENTI SEGNACOLLI
057700970227     C* 75    - UTILIZZATO PER NON ESEGUIRE LA SUBROUTINE "LEGBOL"
057800970227     C* 76    - SI DDT IN ALTA INTENSITA'
057900021213     C* 77    - NOTE SUPERMERCATI
058000970227     C* 78    - SFLDSP
058100950825     C* 79    - SFLDSPCTL
058200930510     C* 80    - NEL SFL DEI LEGAMI NON EMETTO IL CAMPO PER LA SELEZIONE
058300941028     C* 81    - USATO NEL VIDEO LEGAMI
058400941028     C* 82    - USATO NEL VIDEO LEGAMI
058500941028     C* 83    - USATO NEL VIDEO LEGAMI
058600970925     C* 84    - PARZIALIZZ. SEGNACOLLO AL
058700971219     C* 85    - VISUALIZZO RIFERIMENTO PARTNER ESTERO
058800111005     c* 86    - Abilitazione F22="PERFECT ORDER"
058900111005     C* 87    - Pgm richiamato in modalità "W" (Perfect Order Wurth)
059000110705     C* 88    -
059100931112     C* 89    - UTILIZZATO PER VISUALIZZARE ALTRI TASTI FUNZIONALI
059200920402     C* 90    - INDICA LA PRESENZA DI UN ERRORE
059300941028     C* 91    - ERRORE QCMDEXC (UTILIZZABILE)
059400940923     C* 92/94 - LEGAME BOLLA
059500930525     C* 95    - ESISTONO EVENTI DA INTERROGARE
059600131111     C* 96    -
059700991222     C* 97    - MANUTENZIONE BOLLE ARRIVI
059800941014     C* 98    - ESISTONO VARIAZIONI BOLLE EFFETTUATE
059900930504     C* 99    - OFF  ESISTE LEGAME BOLLA SEGUENTE
060000900518     C*****************************************************************
060100000000     C     *ENTRY        PLIST
060200000000     C                   PARM                    KPJBA
060300040218     c                   parm                    FIDNA1ds
060400131210     c
060500131210      /free
060600131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
060700131210      /end-free
060800980609     C**
060900940712     C                   MOVEL     KPJBU         PARAM1
061000980609     C                   MOVEL     KPJBU         PARAAZ
061100940713     C*
061200060130     C                   Z-ADD     1             CODUT             1 0
061300060130     c                   Movel     rsut          V1cdsa
061400060130     C                   MOVEL     rsut          PARDUT           20
061500941227     C*
061600941227     C                   MOVEL     SIMFEL        PA1FEL
061700990701     C                   MOVEL     V1CDSA        PA1DSA
061800940713     C*-------------------------------------------------------------------------
061900911024     C                   EXSR      CARTAB
062000131111
062100131111     c                   eval      Wend=*off
062200110930     c*
062300111005     c                   setoff                                       87
062400110930     c                   if        pa1flg='W'
062500110930     c                   eval      comrma=pa1rma
062600111005     c                   eval      *in87=*on
062700110930     c                   endif
062800110930     c* disabilito f22=PerfectOrder se richiamato da sè stesso
062900131107     c                   if        pa1pgm='FNLRI6R'
063000110929     c                   eval      no_f22='N'
063100110929     c                   endif
063200101202     c                   if        pa1pgm=*blanks
063300131107     C                   MOVEL     'FNLRI6R'     PA1PGM
063400101202     c                   endif
063500940713     C                   MOVE      'N'           V1CAFF
063600941215     C                   MOVEL     '? '          V1CCVB
063700941215     C                   CLEAR                   V1DCVB
063800941014     C* TESTATA
063900941014     C                   MOVEL     CINTER        V1CTES
064000131127     c                   clear                   v1clna
064100940317      *
064200940712      * IMPOSTO LA LINEA DI ARRIVO
064300060217     c* Non la imposto se richiamato con key specifica
064400040217if  1C     PA1FLG        IFNE      '1'
064500060217     C     PA1FLG        ANDNE     '3'
064600060217     C     PA1FLG        ANDNE     'C'
064700060217     C     PA1FLG        ANDNE     'N'
064800960903     C     PA1FLG        ANDNE     'P'
064900961121     C     PA1FLG        ANDNE     'E'
065000110929     C     PA1FLG        ANDNE     'W'
065100060217     c* o se vengo da int bolle normale (per assistenza)
065200060217     c     pa1flg        andne     ' '
065300060217     c     pa1flg        andne     '4'
065400060130     c                   z-add     dutpou        v1clna
065500020424     c*
065600040121     C                   MOVE      V1CLNA        NUM3              3 0
065700940712     C     NUM3          CHAIN     AZORG01L                           90
065800940713     C  N90              MOVEL     ORGDES        V1CDEA
065900040217e   1C                   ENDIF
066000930702     C*
066100920703     C                   EXSR      PUL01
066200940809     C                   MOVEL     'S'           WRIEM             1
066300930702     C**
066400930702     C* PGM RICHIAMATO
066500920326     C**
066600940713     C** PA1FLG =   - NON RICHIAMATO
066700920423     C**        = 1 - RICHIAMATO CON   KEY SPECIFICA
066800920423     C**        = 2 - RICHIAMATO SENZA KEY SPECIFICA PER   SELEZ BOLLA
066900920423     C**        = 3 - RICHIAMATO CON   KEY SPECIFICA DA INT-BOLL PART
067000920423     C**        = 4 - RICHIAMATO SENZA KEY SPECIFICA SENZA SELEZ BOLLA
067100990129     C**        = C - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F2=C.A
067200941014     C**        = M - MANUTENZIONE BOLLE ARRIVI
067300960903     C**        = N - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F7=ANOMA
067400960903     C**        = P - RICHIAMATO CON   KEY SPECIFICA DA INT-BOLL PART
067500960903     C**              E DISABILITAZ F7=ANOMALIE
067600961121     C**        = E - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F8=EV.SEGNACOLLI
067700110930     C**        = W - RICHIAMATO SENZA KEY SPECIFICA per "PerfectOrder"
067800960903     C*
067900040218      *?Abilito F1=Aggiunta in Reclami?
068000040218      * (se richiamato da pgm. FIDNA1R...)
068100040220     c                   eval      *in63  = *off
068200040303     c                   if            SDSprm >  1
068300040308     c                             and IA1ins = ' '
068400040220     c                   eval      *in63  = *on
068500060301     c                   clear                   v1clna
068600060301     c                   clear                   v1cdea
068700111003     c                   endif
068800960903     C* ABILITO F7=ANOMALIE SE FLAG DIVERSO DA 'N' E 'P'
068900960903     C     PA1FLG        IFNE      'N'
069000960903     C     PA1FLG        ANDNE     'P'
069100960903     C                   SETON                                        67
069200040217     C                   ENDIF
069300961121     C* ABILITO F8=EVENTI SEGNACOLLI  SE FLAG DIVERSO DA 'E'
069400961121     C     PA1FLG        IFNE      'E'
069500961121     C                   SETON                                        74
069600040217     C                   ENDIF
069700960903     C*
069800040217if  1C     PA1FLG        IFEQ      '1'
069900960903     C     PA1FLG        OREQ      'N'
070000940713     C     PA1FLG        OREQ      '3'
070100960903     C     PA1FLG        OREQ      'P'
070200961121     C     PA1FLG        OREQ      'E'
070300990129     C     PA1FLG        OREQ      'C'
070400920703     C*
070500940714     C                   MOVEL     V2CAAS        V1CAAS
070600940713     C                   MOVEL     COMLNP        V1CLP2
070700940714     C                   MOVEL     V2CNRS        V1CNRS
070800940714     C                   MOVEL     V2CNSP        V1CNSP
070900920703     C* SE NO PASSATA KEY SPECIFICA SI PUO' SELEZIONARE
071000940713     C     PA1FLG        IFEQ      '1'
071100960903     C     PA1FLG        OREQ      'N'
071200961121     C     PA1FLG        OREQ      'E'
071300990129     C     PA1FLG        OREQ      'C'
071400070411     c                   if        v1cnsp=0
071500920703     C                   SETON                                        61
071600040217     C                   ENDIF
071700070411     C                   ENDIF
071800040217x   1C                   ELSE
071900110929     c* Se richiamata con "W" devo saltare tutta la gestione della
072000110929     c* prima videata e devo passare subito al caricamento del sfl
072100110929   1ac     pa1flg        ifne      'W'
072200911213     C*
072300940713     C     PA1FLG        IFEQ      '2'
072400911213     C                   SETON                                        61
072500040217     C                   ENDIF
072600920203     C*
072700980603     C                   SETOFF                                       05
072800040217if  2C     PA1FLG        IFEQ      ' '
072900920203     C                   SETON                                        39
073000040217e   2C                   ENDIF
073100941014     C*
073200000616     C* MANUTENZIONE BOLLE ARRIVI 97=ON
073300040217if  2C     PA1FLG        IFEQ      'M'
073400030612      *
073500941014     C                   SETON                                        97
073600030612     C                   MOVEL     CMANUT        V1CTES
073700070420     c
073800040217e   2C                   ENDIF
073900000703     C*
074000941209     C     INIZIO        TAG
074100131114     C***                EXSR      PUL01
074200131114
074300131114     c* se cursore aperto --> lo chiudo
074400131209     c*****              if        Wend=*on  or *in14 or *in11
074500131209     c                   if        WopenC='1'
074600131114     c                   exsr      closecursor
074700131114     c                   endif
074800911025     C*
074900900509     C     FOR01         TAG
075000941209     C*
075100040217if  2C     WRIEM         IFEQ      'S'
075200131107     C                   WRITE     LRI6T01
075300940720     C                   MOVEL     *BLANKS       WRIEM
075400040217e   2C                   ENDIF
075500131107     C                   EXFMT     LRI6D01
075600940713     C*
075700940713     C                   SETOFF                                       28
075800940715     C                   MOVEL     *BLANKS       WRIEM
075900940713     C*
076000980603     C** F3 - FINE LAVORO
076100040217if  2C     *INKC         IFEQ      '1'
076200980603     C   05              MOVEL     '1'           PARF3
076300980603     C                   GOTO      FINE
076400040217e   2C                   ENDIF
076500131115     C**
076600131115     C** F5 - Altre parzializzazioni
076700131115     c                   if        *inke=*on
076800131115     c                   exsr      AltrePar
076900131115     C                   MOVEL     'S'           WRIEM
077000131115     C                   GOTO      FOR01
077100131115     c                   endif
077200131114     C**
077300980603     C** F7 - INTERROGAZIONE BOLLE PARTENZE
077400040217if  2C     *INKG         IFEQ      *ON
077500930922     C                   EXSR      INTPAR
077600940722     C                   MOVEL     'S'           WRIEM
077700920203     C                   GOTO      FOR01
077800040217e   2C                   ENDIF
077900131114     C**
078000131114     C** F10- pulizia di tutti i campi di scelta
078100131114if  2C     *INKj         IFEQ      *ON
078200131114     C                   EXSR      pul01
078300131114     C                   MOVEL     'S'           WRIEM
078400131114     C                   GOTO      FOR01
078500131114e   2C                   ENDIF
078600930922     C**
078700980603     C** F14- INTERROGAZIONE BOLLE TRANSITO
078800040217do  2C   KN              DO
078900940722     C                   MOVEL     'T'           PA2BOL
079000920423     C                   MOVEL     PARAM2        KPJBU
079100940721     C                   CALL      'FNLR68R'
079200920423     C                   PARM                    KPJBA
079300940722     C                   MOVEL     'S'           WRIEM
079400920423     C                   GOTO      FOR01
079500040217e   2C                   ENDDO
079600911025     C*
079700110929e  1ac                   endif
079800040217e   1C                   ENDIF
079900900509     C* CONTROLLI FORMATO1
080000110930     c     pa1flg        ifne      'W'
080100900509     C                   EXSR      CONTR
080200900509     C   90              GOTO      FOR01
080300110929     c                   endif
080400911025     C**
080500911025     C** P E R   N U M E R O   S P E D I Z I O N E
080600131111     c**  o      B A R C O D E
080700040217if  1C     SEL2          IFNE      0
080800131111     c     v1cbarcode    orgt      0
080900971223     C                   EXSR      SINGOL
081000971223     C     WINIZ         IFEQ      '1'
081100971223     C                   GOTO      INIZIO
081200971223     C                   ENDIF
081300971223     C     WFINE         IFEQ      '1'
081400971223     C                   GOTO      FINE
081500971223     C                   ENDIF
081600040217e   1C                   ENDIF
081700911025     C**
081800911025     C**  P E R   S U B F I L E
081900911023     C                   EXSR      ELAB
082000941014     C*
082100901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
082200040217if  1C     NR2           IFEQ      0
082300940713     C                   MOVEL     MSG(1)        V1CMSG
082400131115     C                   SETON                                        28
082500131115     c                   select
082600131115     c                   when      v1crpe<>*blanks
082700131115     C                   SETON                                        58
082800131115     c                   when      v1crmn>0 or v1crma<>*blanks
082900131127     C                   SETON                                        68
083000131115     c                   when      v1cnot<>*blanks
083100131115     C                   SETON                                        62
083200131115     c                   when      v1cprd<>*blanks
083300131115     C                   SETON                                        50
083400131115     c                   other
083500131115     C                   SETON                                        51
083600131115     c                   endsl
083700900516     C                   GOTO      FOR01
083800040217e   1C                   ENDIF
083900971223     C**
084000040217if  1C     V1CRPE        IFNE      *BLANKS
084100010509     C     NR2           ANDEQ     1
084200010509     c     *in25         oreq      *on
084300971223     C     NR2           ANDEQ     1
084400111011     c     v1crmn        orgt      *zeros
084500111011     C     NR2           ANDEQ     1
084600111011     c     v1crma        orne      *blanks
084700111011     C     NR2           ANDEQ     1
084800131111     c     v1cnot        orne      *blanks
084900131111     C     NR2           ANDEQ     1
085000111011     C                   MOVEL     v2clnp        COMLNP
085100111011     c     karb          chain     fnarb01l
085200971223     C                   EXSR      SINGOL
085300040217     C     WINIZ         IFEQ      '1'
085400971223     C                   GOTO      INIZIO
085500040217     C                   ENDIF
085600040217     C     WFINE         IFEQ      '1'
085700971223     C                   GOTO      FINE
085800040217     C                   ENDIF
085900040217e   1C                   ENDIF
086000930922     C**
086100900518     C     FORCT2        TAG
086200131107     C                   WRITE     LRI6T01
086300131107     C                   WRITE     LRI6Z02
086400900515     C** COMANDI SUBFILE
086500900515     C     FORCT         TAG
086600131107     C                   EXFMT     LRI6C02
086700940923     C                   SETOFF                                       28
086800941109     C                   CLEAR                   PA1F03
086900980603     C* F3 - FINE
087000980603     C     *INKC         IFEQ      '1'
087100980603     C   05              MOVEL     '1'           PARF3
087200980603     C                   GOTO      FINE
087300980603     C                   ENDIF
087400930922     C*
087500980603     C* F12 - RITORNO
087600040217if  1C     *INKL         IFEQ      *ON
087700110930     c     pa1flg        cabeq     'W'           fine
087800940713     C     PA1FLG        IFEQ      '3'
087900960903     C     PA1FLG        OREQ      'P'
088000940713     C     PA1FLG        OREQ      '1'
088100960903     C     PA1FLG        OREQ      'N'
088200961121     C     PA1FLG        OREQ      'E'
088300990129     C     PA1FLG        OREQ      'C'
088400940714     C                   Z-ADD     0             V2CAAS
088500920702     C                   Z-ADD     0             COMLNP
088600940714     C                   Z-ADD     0             V2CNRS
088700940714     C                   Z-ADD     0             V2CNSP
088800930922     C                   ENDIF
088900930922     C                   GOTO      INIZIO
089000040217e   1C                   ENDIF
089100900515     C* ROLLUP
089200040217if  1C     *IN37         IFEQ      *ON
089300940713     C   30              DO
089400940713     C                   MOVEL     MSG(15)       V1CMSG
089500940713     C                   SETON                                        28
089600940713     C                   ENDDO
089700930922     C  N30              EXSR      CARSF
089800940713     C                   Z-ADD     COMOD         REC2
089900930922     C                   GOTO      FORCT
090000040217e   1C                   ENDIF
090100900515     C*
090200900516     C** CONTROLLO SE EFFETTUATA UNA SCELTA
090300940713     C                   Z-ADD     PRIMA         REC2
090400131107     C                   READC     LRI6S02                                34
090500911023     C* VISUALIZZO BOLLA
090600040217do  1C     *IN34         DOWEQ     *OFF
090700940714     C                   MOVEL     *BLANKS       V2CFLG
090800911023     C*
090900040217if  2C     V2CSCE        IFEQ      '5'
091000940714     C                   Z-ADD     V2CLNP        COMLNP
091100940712     C     KARB          CHAIN     FNARB01L                           31
091200930518     C*
091300940714     C                   MOVEL     *BLANKS       V2CSCE
091400040217     C     V2CCON        IFEQ      'IN CON'
091500930518     C                   SETON                                        13
091600040217     C                   END
091700930518     C*
091800040217     C     ARBCCA        IFNE      ' '
091900930518     C     ARBFBC        ORNE      ' '
092000930518     C                   SETON                                        62
092100040217     C                   END
092200941014     C*
092300131107     C                   UPDATE    LRI6S02
092400911023     C*
092500911023     C                   EXSR      BOLLA
092600911023     C                   EXSR      VISUAL
092700911023     C*
092800040217x   2C                   ELSE
092900941014     C*
093000941014     C* 97 ON - MANUTENZIONE BOLLE
093100040217if  3C     V2CSCE        IFEQ      '1'
093200941014     C*
093300040217if  4C     *IN97         IFEQ      *ON
093400060227     c                   movel     v2clna        w003a             3
093500060224     C     w003a         LOOKUP    $pog                                   31
093600060130    2c                   if        not *in31 and d66pocl='S'
093700060130     c     v2ccbo        lookup    cbodop                                 31
093800060130    3c                   if        not *in31
093900060130     c                   movel     v2cksc        kscfil
094000060130   x3c                   else
094100060130     c* Se bolla doppia prendo il ksc dall'AR6
094200060130     C                   Z-ADD     v2cAAS        KAAS
094300060130     C                   Z-ADD     v2cLNP        KLNP
094400060130     C                   Z-ADD     v2cNRS        KNRS
094500060130     C                   Z-ADD     v2cNSP        KNSP
094600060130     C                   MOVEL     '2'           KTRC
094700060130     C     KAR61         CHAIN     FIAR6000                           31
094800060130if  4C                   IF          %FOUND(fiar601l)
094900060130     c                   movel     ar6ksc        kscfil
095000060130     c                   else
095100060130     c                   movel     999           kscfil
095200060130    4c                   endif
095300060130    3c                   endif
095400130314     c*
095500060224     c                   movel     kscfil        w003a
095600060224     c     w003a         lookup    $pog                                   31
095700060130    2c                   endif
095800130314     c*
095900040217if  5C     *IN31         IFEQ      *OFF
096000941212     C                   MOVEL     MSG(19)       V1CMSG
096100941212     C                   SETON                                        599028
096200941212     C                   MOVEL     '2'           V2CFLG
096300130314     c*
096400040217x   5C                   ELSE
096500141010
096600130314     c*
096700991027     C                   CLEAR                   FNLR48DS
096800060224     c* come p.o. gestione prendo il p.o. gestore (da £6) della linea
096900060224     c*  di arrivo
097000060224     c                   clear                   fnlv55ds
097100060224     c                   movel     v2clna        d55lin
097200060224     c                   movel     '6'           d55tpt
097300060224     C                   time                    wrkoggiiso
097400060224     C     *iso          move      wrkoggiiso    d55drf
097500060224     c                   call      'FNLV55R'
097600060224     c                   parm                    fnlv55ds
097700060224     c                   movel     d55tfa        d48fgs
097800130314     c*
097900941014     C                   MOVEL     V2CAAS        D48AAS
098000941014     C                   MOVEL     V2CLNP        D48LNP
098100941014     C                   MOVEL     V2CNRS        D48NRS
098200941014     C                   MOVEL     V2CNSP        D48NSP
098300141010     c* routine per verifica dispo da esaminare
098400141010     c                   exsr      sr_wdwdc
098500141010
098600941014     C                   MOVEL     V2CCBO        D48CBO
098700941014     C                   MOVEL     V1CCVB        D48CVB
098800950616     C                   MOVEL     'V'           D48FFR
098900941227     C                   MOVEL     'A'           D48TBO
099000950607     C                   MOVEL     'S'           D48F12
099100941014     C*
099200991027     C                   MOVEL     FNLR48DS      KPJBU
099300941014     C                   CALL      'FNLR48R'
099400941014     C                   PARM                    KPJBA
099500941212     C                   PARM                    DSARBD
099600941212     C                   PARM                    DSARBK
099700991011     C                   PARM                    DARBT
099800941212     C                   PARM                    DSARBG
099900030612     C                   PARM                    trul90ds
100000941212     C*
100100991027     C                   MOVEL     KPJBU         FNLR48DS
100200941212     C*
100300941014     C* ERRORE IN MANUTENZIONE BOLLE
100400040217     C     D48ERR        IFEQ      '1'
100500941014     C                   MOVEL     D48MSG        V1CMSG
100600941014     C                   SETON                                        592890
100700941014     C                   MOVEL     '2'           V2CFLG
100800040217     C                   ENDIF
100900950114     C*
101000950114     C* SE NON C'E' ERRORE AGGIORNO IL SFL CON I DATI DELLA BOLLA VAR
101100040217     C     *IN90         IFEQ      *OFF
101200950120     C                   Z-ADD     V2CLNP        COMLNP
101300950114     C     KARB          CHAIN     FNARB01L                           31
101400950114     C                   MOVEL     ARBRSM        V2CRSM
101500950114     C                   MOVEL     ARBRSD        V2CRSD
101600131114     c                   if        v1cind=*blanks
101700131114     c                   eval      v2cdest=arblod
101800131114     c                   else
101900131114     c                   eval      v2cdest=arbind
102000131114     c                   endif
102100131114     c
102200040217     C                   ENDIF
102300040217e   5C                   ENDIF
102400941014     C*
102500040217x   4C                   ELSE
102600941014     C*
102700911023     C* SELEZIONATA BOLLA: PASSO KEY ED ESCO DA PGM
102800040217     C     *IN61         IFEQ      *ON
102900940714     C                   Z-ADD     V2CLNP        COMLNP
103000940714     C                   MOVEL     '3'           V2CFLG
103100040220      *?- Richiamato da pgm. FIDNA1R?
103200040220if  6c                   if        SDSprm  > 1
103300131111     c                   eval      wARBlnp  = V2Clnp
103400131111     c                   eval      wARBnrs  = V2Cnrs
103500131111     c                   eval      wARBnsp  = V2Cnsp
103600040220     c                   eval      wARBaas = V2Caas
103700040220     c                   clear                   FIDNA1ds
103800040220     c                   eval      IA1ins  = 'S'
103900040220     c                   eval      IA1tor  = 'S'
104000040220     c                   eval      IA1ogg  = ds_OggSped
104100040220e   6c                   endif
104200040217     C                   ENDIF
104300040217e   4C                   ENDIF
104400040217e   3C                   ENDIF
104500930518     C*
104600040217do  3C  N61              DO
104700940714     C                   MOVEL     *BLANKS       V2CSCE
104800040217     C     V2CCON        IFEQ      'IN CON'
104900930518     C                   SETON                                        13
105000930518     C                   ELSE
105100040217     C     V2CCON        IFNE      *BLANKS
105200940714     C                   TESTN                   V2CCON               31
105300930518     C  N31              SETON                                        62
105400040217     C                   END
105500040217     C                   END
105600131107     C                   UPDATE    LRI6S02
105700040217e   3C                   ENDDO
105800930518     C*
105900040217e   2C                   ENDIF
106000911022     C*
106100941014     C                   SETOFF                                       136259
106200911022     C*
106300911022     C* SE FLAG 2 TORNO AL SUBFILE
106400040217     C     V2CFLG        IFNE      *BLANKS
106500900530     C                   SETON                                        34
106600900530     C                   ELSE
106700131107     C                   READC     LRI6S02                                34
106800040217     C                   ENDIF
106900040217e   1C                   ENDDO
107000941014     C*
107100911023     C* FLG =  3 ==> FILE PGM E PASSO KEY
107200940714     C     V2CFLG        IFEQ      '3'
107300911023     C                   GOTO      FINE
107400911023     C                   ELSE
107500911023     C* FLG >< 2 ==> TORNO A SUBFILE
107600911023     C                   GOTO      FORCT2
107700040217     C                   ENDIF
107800911023     C*
107900000000     C     FINE          TAG
108000930504     C*
108100930702     C* CALL A VUOTO AI PGM  PER CHIUDERE I FILES
108200941214     C     PA098         IFEQ      '1'
108300930504     C                   MOVEL     'C'           D98TLA
108400970925     C                   CALL      'TNSD99R'
108500991027     C                   PARM                    TNSD99DS
108600040217     C                   ENDIF
108700930702     C*
108800941214     C     PA075         IFEQ      '1'
108900940714     C                   MOVEL     'C'           PA1FLG
109000940712     C                   MOVEL     PARAM1        KPJBU
109100940714     C                   CALL      'FNLR75R'                            99
109200930524     C                   PARM                    KPJBA
109300040217     C                   ENDIF
109400930702     C*
109500941214     C     PA0B5         IFEQ      '1'
109600950215     C                   CLEAR                   PARAB5
109700940726     C                   MOVEL     'C'           PB5FLG
109800940726     C                   MOVEL     PARAB5        KPJBU
109900940722     C                   CALL      'FNLRB5R'
110000930525     C                   PARM                    KPJBA
110100040217     C                   ENDIF
110200941014     C*
110300941014     C     *IN97         IFEQ      *ON
110400941014     C                   MOVEL     'C'           D66TLA
110500991027     C                   MOVEL     FNLR66DS      KPJBU
110600941014     C                   CALL      'FNLR66R'
110700941014     C                   PARM                    KPJBA
110800040217     C                   ENDIF
110900991027      *
111000991027     C                   IF        Wtibs69r  <>  *BLANKS
111100991027     C                   eval      I69TLA  = 'C'
111200991027     C                   call      'TIBS69R'
111300991027     C                   parm                    TIBS69DS
111400991027     c                   ENDIF
111500930504     C*
111600941109     C* PER F3=FINE PULISCO CHIAVE BOLLA E IMPOSTO FLAG F3
111700941109     C     *INKC         IFEQ      *ON
111800941109     C                   Z-ADD     *ZERO         V2CAAS
111900941109     C                   Z-ADD     *ZERO         COMLNP
112000941109     C                   Z-ADD     *ZERO         V2CNRS
112100941109     C                   Z-ADD     *ZERO         V2CNSP
112200941109     C                   MOVEL     '1'           PA1F03
112300941109     C                   ENDIF
112400980609     C**
112500941212     C* CHIUDO I PGM
112600941212     C* SOLO PER PGM DI MANUTENZIONE
112700941212     C     *IN97         IFEQ      *ON
112800941212     C                   MOVEL     'C'           D66TLA
112900941212     C                   CALL      'FNLR66R'
113000991027     C                   PARM                    FNLR66DS
113100060224     c
113200060224     c                   clear                   fnlv55ds
113300060224     C                   MOVEL     'C'           D55TLA
113400060227     C                   CALL      'FNLV55R'
113500060224     C                   PARM                    FNLv55DS
113600941212     C*
113700941212     C                   MOVEL     'C'           D48TLA
113800991027     C                   MOVEL     FNLR48DS      KPJBU
113900941212     C                   CALL      'FNLR48R'
114000941212     C                   PARM                    KPJBA
114100941212     C*
114200141105     c                   eval      ilry09tla= 'C'
114300141105     c                   eval      kpjbu=fnlry09ds2
114400141105     c                   call      'FNLRY09R'
114500141105     c                   parm                    kpjba
114600941212     C                   ENDIF
114700980603     C**
114800980609     C                   MOVEL     PARAAZ        KPJBU
114900980624     C                   MOVEL     PARAM1        KPJBU
115000941212     C*
115100040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
115200040810     c                   clear                   fidn12ds
115300040810     c                   eval      i12tla = 'C'
115400040810     c                   call      'FIDN12R'
115500040810     c                   parm                    fidn12ds
115600051128     c                   clear                   fnlv80ds
115700051128     c                   eval      ilv80tla = 'C'
115800051128     c                   call      'FNLV80R'
115900051128     c                   parm                    kpjba
116000051128     c                   parm                    FNLV80DS
116100070427     c                   eval      id34tla = 'C'
116200070427     c                   call      'FIDG34R'
116300070427     c                   parm                    fidg34ds
116400131120     c                   eval      ilrg7tla = 'C'
116500131120     c                   call      'FNLRG7R'
116600131120     c                   parm                    fnlrg7ds
116700091216     c                   eval      iinttla = 'C'
116800091216     c                   call      'TRULINTR'
116900091216     c                   parm                    trulintds
117000141105
117100040810     c*
117200000000     C                   SETON                                        LR
117300941011     C   99
117400941011     CANN99              READ      FNBLP01L                               99
117500940802     C**************************************************************************
117600940802     C*    CARICO TABELLE
117700940802     C**************************************************************************
117800911024     C     CARTAB        BEGSR
117900941212     C***
118000941212     C* CARICO TABELLA FILIALI GESTITE £1
118100941212     C***
118200950122     C                   CLEAR                   L1
118300991027     C                   CLEAR                   TRUL06DS
118400941212     C                   MOVE      '£1'          D06COD
118500941212     C                   MOVEL     SIMFEL        D06KEY
118600941212     C                   MOVEL     'L'           D06TLA
118700991027     C                   MOVEL     TRUL06DS      KPJBU
118800941212     C*
118900941212     C                   CALL      'TRUL06R'
119000941212     C                   PARM                    KPJBA
119100991027     C                   MOVEL     KPJBU         TRUL06DS
119200941212     C                   MOVEA     LIN           L1
119300930705     C*
119400930705     C* CARICO TABELLA 7O
119500930705     C                   MOVEL     '7O'          COD
119600941212     C                   Z-ADD     1             C                 3 0
119700940713     C     KTAB2         SETLL     TABEL
119800940713     C     KTAB2         READE     TABEL                                  30
119900930705     C*
120000930705     C     *IN30         DOWEQ     *OFF
120100930705     C     TBLFLG        IFEQ      ' '
120200930705     C                   MOVEL     TBLKEY        KCA(C)
120300930705     C                   MOVEL     TBLUNI        DCA(C)
120400930705     C                   ADD       1             C
120500930705     C                   END
120600940713     C     KTAB2         READE     TABEL                                  30
120700930705     C                   END
120800941024     C*
120900941024     C* CARICO IN SCHIERA LA TABELLA CC
121000941024     C                   Z-ADD     *ZERO         WA
121100941024     C                   CLEAR                   CC1
121200941024     C                   CLEAR                   CC2
121300020329     C                   CLEAR                   CC3
121400941024     C                   MOVEL     'CC'          COD
121500941024     C     KTAB2         SETLL     TABEL00F
121600941024     C     KTAB2         READE     TABEL00F                               99
121700941024     C     *IN99         DOWEQ     *OFF
121800941024     C     TBLKEY        ANDNE     *BLANKS
121900941024     C     TBLKEY        ANDNE     *ZERO
122000941024     C                   ADD       1             WA
122100941024     C                   MOVE      TBLKEY        CC1(WA)
122200941024     C                   MOVEL     TBLUNI        CC2(WA)
122300020402     C                   MOVEL(P)  TBLUNI        DSCC
122400020402     C                   MOVE      §CCDFE        CC3(WA)
122500941024     C     KTAB2         READE     TABEL00F                               99
122600941024     C                   ENDDO
122700060130     C*
122800060130     C* CARICO IN SCHIERA LA TABELLA 3a per cod bolla doppi
122900060130     C                   Z-ADD     *ZERO         WA
123000060130     C                   CLEAR                   cbodop
123100060130     C                   MOVEL     '3A'          COD
123200060130     C     KTAB2         SETLL     TABEL00F
123300060130     C     KTAB2         READE     TABEL00F                               99
123400060130     C     *IN99         DOWEQ     *OFF
123500060130     c                   movel     tbluni        ds3a
123600060130     c                   if        §3atb2<>*blanks
123700060130     C                   ADD       1             WA
123800060130     C                   MOVEL     TBLKEY        cbodop(wa)
123900060130     c                   endif
124000060130     c
124100060130     C     KTAB2         READE     TABEL00F                               99
124200060130     C                   ENDDO
124300020329      *
124400020329      * CARICO IN SCHIERA LA TABELLA CB
124500020329     C                   Z-ADD     *ZERO         WA
124600020329     C                   CLEAR                   CB1
124700020329     C                   CLEAR                   CB2
124800020329     C                   MOVEL     'CB'          COD
124900020329     C     KTAB2         SETLL     TABEL00F
125000020329     C     KTAB2         READE     TABEL00F                               99
125100020329     C     *IN99         DOWEQ     *OFF
125200020329     C     TBLKEY        ANDNE     *BLANKS
125300020329     C     TBLKEY        ANDNE     *ZERO
125400020329     C                   ADD       1             WA
125500020329     C                   MOVE      TBLKEY        CB1(WA)
125600020329     C                   MOVEL     TBLUNI        CB2(WA)
125700020329     C     KTAB2         READE     TABEL00F                               99
125800020329     C                   ENDDO
125900991027     C*
126000991027     C* CARICO LA TABELLA $2
126100991027     C                   MOVEL     '$2'          COD
126200991027     C                   MOVEL(P)  1             KEY
126300991027     C     KTAB1         CHAIN     TABEL00F
126400991027     C                   IF        %FOUND
126500991027     C                   MOVEL     TBLUNI        DS$2
126600991027     C                   ELSE
126700991027     C                   CLEAR                   DS$2
126800991027     C                   ENDIF
126900940802     C*
127000911024     C                   ENDSR
127100940802     C**************************************************************************
127200940802     C*    PULIZIA CAMPI FORMATO 1
127300940802     C**************************************************************************
127400900514     C     PUL01         BEGSR
127500131114     c
127600131114     C* PULIZIA CAMPI CHE RIMANGONO INVARIATI IL SECONDO GIRO
127700131114     C                   MOVEL     ' '           V1CSSP
127800131127     c                   if        pa1flg='2' or pa1flg='4' or pa1flg='M'
127900131127     c                   else
128000131114     C                   MOVEL     *ZEROS        V1CLNA
128100131127     c                   endif
128200131115     c                   clear                   v1cdea
128300131115     c                   clear                   v1cdep
128400131114     C                   MOVEL     *ZEROS        V1CLP1
128500131114     C                   MOVEL     *ZEROS        V1CDTD
128600131114     C                   MOVEL     *ZEROS        V1CDTA
128700131114     c                   clear                   v1cnzd
128800131114     c                   clear                   v1cprd
128900131128     c                   movel     ' '           v1cxind
129000131114     c                   clear                   v1cind
129100131114     c                   clear                   v1clod
129200131128     c                   movel     ' '           v1cxlod
129300131114     C                   MOVE      *BLANKS       V1CRSD
129400131128     C                   movel     ' '           V1CXRSD
129500131114     C                   MOVE      *BLANKS       V1CRSM
129600131128     C                   movel     ' '           V1CXRSM
129700131114     C                   MOVEL     *ZEROS        NDC
129800131114     C                   MOVEL     *ZEROS        NSS
129900940802     C*
130000060613     C                   CLEAR                   wforz1
130100060613     C                   CLEAR                   V1CRPE
130200010509     C                   CLEAR                   V1Cpoe
130300010509     C                   CLEAR                   V1Cnsr
130400010509     C                   CLEAR                   V1Cnor
130500010509     C                   CLEAR                   V1Cnrv
130600110930     C                   CLEAR                   V1Crmn
130700110930     C                   CLEAR                   V1Crma
130800131115     C                   CLEAR                   V1Cdan
130900940713     C                   MOVEL     *ZEROS        V1CLP2
131000940713     C                   MOVEL     *ZEROS        V1CNRS
131100940713     C                   MOVEL     *ZEROS        V1CNSP
131200000103     C                   z-add     UYEAR         V1CAAS
131300000103     c     uyear         iflt      60
131400000103     c                   movel     20            v1caas
131500000103     c                   else
131600940713     C                   MOVEL     19            V1CAAS
131700000103     c                   endif
131800940713     C                   MOVEL     *BLANKS       V1CTSP
131900131114     C                   MOVEL     *BLANKS       V01DTSP
132000940713     C                   MOVEL     *BLANKS       V1CGIA
132100940713     C                   MOVEL     *ZEROS        V1CPSD
132200940713     C                   MOVEL     99            V1CPSA
132300940713     C                   MOVEL     *ZEROS        V1CSCA
132400940713     C                   MOVEL     *ZEROS        V1CSCD
132500940923     C                   MOVEL     *ZERO         V1CFLS
132600940713     C                   MOVE      'N'           V1CAFF
132700961112     C                   CLEAR                   V1CFBR
132800131111     C                   CLEAR                   V1Cbarcode
132900131111     C                   CLEAR                   V1CNOT
133000040216     c                   clear                   V1Ccbo
133100940713     C                   Z-ADD     0             NR2
133200070420     C****               MOVE      'N'           V1CSNP            1
133300070420     c                   clear                   v1csfd
133400941212     C                   SETOFF                                       09
133500131111     c
133600131111     c* se cursore aperto --> lo chiudo
133700131209     c***                if        Wend=*on  or *in14 or *in11
133800131209     c                   if        WopenC='1'
133900131111     c                   exsr      closecursor
134000131111     c                   endif
134100150223     c* se provengo da gestojne r.a. mi posiziono su brtcode
134200150223     c                   if            SDSprm >  1
134300150223     c                             and IA1ins = ' '
134400150223     c                   seton                                        60
134500150223     c                   endif
134600930705     C*
134700900514     C                   ENDSR
134800940802     C**************************************************************************
134900940802     C*    CONTROLLI FORMATO1
135000940802     C**************************************************************************
135100900509     C     CONTR         BEGSR
135200940317      *
135300940713     C                   MOVE      *BLANKS       V1CDEA
135400940713     C                   MOVE      *BLANKS       V1CDEP
135500110930     c                   movel     v1crma        comrma
135600020709
135700131121     c******             clear                   v2dlna
135800940317      *
135900900515     C                   SETOFF                                       900102
136000970925     C                   SETOFF                                       140784
136100980603     C                   SETOFF                                       03  11
136200040216     C                   SETOFF                                       154046
136300940715     C                   SETOFF                                       414243
136400940715     C                   SETOFF                                       444548
136500940715     C                   SETOFF                                       495152
136600131115     C                   SETOFF                                       552547
136700131111     C                   SETOFF                                       535460
136800131111     c                   setoff                                       575058
136900131127     c                   setoff                                       6862
137000110930     c                   clear                   h1riga
137100110930     c                   clear                   h1colo
137200950121     C*
137300950121     C* SE L'ANNO E' IMMESSO DI DUE CIFRE, METTO LE ALTRE DUE
137400950121     C     V1CAAS        IFLT      100
137500950121     C     V1CAAS        IFGT      60
137600950121     C                   ADD       1900          V1CAAS
137700950121     C                   ELSE
137800950121     C                   ADD       2000          V1CAAS
137900950121     C                   ENDIF
138000950121     C                   ENDIF
138100950121     C*
138200950121     C* SE VARIAZIONE CONTROLLO CAL CAUSALE
138300950121     C   97              DO
138400991027     C                   CLEAR                   FNLR66DS
138500950121     C     '?'           SCAN      V1CCVB                                 90
138600950121     C* RICERCA
138700950121    1C     *IN90         IFEQ      *ON
138800991027     C                   CLEAR                   FNLR66DS
138900950121     C                   MOVEL     'V'           D66GES
139000950121     C                   MOVEL     'A'           D66TBO
139100950121     C*
139200950121     C* SE IMMESSO NUMERO SPEDIZIONE LO VISUALIZZO
139300950121    2C     V1CNSP        IFGT      0
139400950121     C                   MOVEL     V1CNSP        D66NSP
139500950121     C                   MOVEL     V1CAAS        D66AAS
139600950121     C                   MOVEL     V1CLP2        D66LNP
139700950121     C                   MOVEL     V1CNRS        D66NRS
139800950121    2C                   ENDIF
139900950121     C*
140000991027     C                   MOVEL     FNLR66DS      KPJBU
140100950121     C                   CALL      'FNLR66R'
140200950121     C                   PARM                    KPJBA
140300991027     C                   MOVEL     KPJBU         FNLR66DS
140400950121     C                   MOVEL     D66CVB        V1CCVB
140500950121     C                   MOVEL     D66DES        V1DCVB
140600950121     C                   MOVEL     'S'           WRIEM
140700950203     C                   SETON                                        4790
140800950121     C*
140900950121     C                   GOTO      ENDCTR
141000950121   X1C                   ELSE
141100950121     C* CONTROLLO
141200991027     C                   CLEAR                   FNLR66DS
141300950121     C                   MOVEL     'C'           D66GES
141400950121     C                   MOVEL     'A'           D66TBO
141500950121     C                   MOVEL     V1CCVB        D66CVB
141600950121    2C     V1CNSP        IFGT      0
141700950121     C                   MOVEL     V1CNSP        D66NSP
141800950121     C                   MOVEL     V1CAAS        D66AAS
141900950121     C                   MOVEL     V1CLP2        D66LNP
142000950121     C                   MOVEL     V1CNRS        D66NRS
142100950121    2C                   ENDIF
142200950121     C*
142300991027     C                   MOVEL     FNLR66DS      KPJBU
142400950121     C                   CALL      'FNLR66R'
142500950121     C                   PARM                    KPJBA
142600991027     C                   MOVEL     KPJBU         FNLR66DS
142700950121     C                   MOVEL     D66CVB        V1CCVB
142800950121     C                   MOVEL     D66DES        V1DCVB
142900950121     C*
143000950121     C* ERRORE
143100950121    2C     D66MSG        IFNE      *BLANKS
143200950121     C                   MOVEL     D66MSG        V1CMSG
143300950121     C                   SETON                                        284790
143400950121     C                   GOTO      ENDCTR
143500950121     C*
143600950121   X2C                   ELSE
143700950121     C* DECODIFICA
143800950121     C                   MOVEL     D66DES        V1DCVB
143900950121    2C                   ENDIF
144000950121    1C                   ENDIF
144100950121     C                   ENDDO
144200950102     C*
144300131113     C* immettere almeno una selezione
144400901115     C     SEL1          IFEQ      0
144500920911     C     SEL2          ANDEQ     0
144600940317     C     SEL3          ANDEQ     *BLANKS
144700010509     C     SEL4          ANDEQ     *zeros
144800131113     c
144900971223     C     V1CRPE        ANDEQ     *BLANKS
145000110930     c     v1crmn        andeq     *zeros
145100110930     c     v1crma        andeq     *blanks
145200131111     c     v1cnot        andeq     *blanks
145300131111     c     v1cbarcode    andeq     *zeros
145400940713     C                   MOVEL     MSG(3)        V1CMSG
145500940713     C                   SETON                                        412890
145600901115     C                   GOTO      ENDCTR
145700901115     C                   END
145800940318      *
145900901115     C* DATA SPEDIZIONE
146000900514     C* LINEA ARRIVO
146100131113      * per destinatario o mittente sono obbligatori
146200020627      * la nazione e la provincia del destinatario
146300131113    1C     SEL2          IFEQ      0
146400131113    2c     sel4          ifeq      0
146500020225     c                   clear                   lnantw
146600131113    3C     V1CLNA        IFNE      0
146700940713     C                   MOVEL     V1CLNA        NUM3
146800940713     C     NUM3          CHAIN     AZORG01L                           41
146900131113    4C   41              DO
147000940713     C                   MOVEL     MSG(2)        V1CMSG
147100940713     C                   SETON                                        28  90
147200940713     C                   GOTO      ENDCTR
147300131113    4C                   ENDDO
147400940713     C                   MOVEL     ORGDES        V1CDEA
147500131121     C*******            MOVEL     ORGDES        V2DLNA
147600991222     C                   MOVEL     ORGDE3        og143
147700020225     c                   eval      lnantw = §ogntw
147800900514     C                   SETON                                        01
147900131113    3C                   END
148000131113    2C                   END
148100131113    c
148200020626      * selezione per destinatario o mittente
148300020626     c                   if        sel3 <> *blanks
148400020628     c                   exsr      ContrDatiDes
148500020628     c                   if        *in90 = *on                                  ricerca
148600020628     c                   goto      endctr
148700020628     c                   endif
148800020628     c                   if        *in28 = *on                                  errore
148900020628     c                   eval      *in90 = *on
149000020628     c                   goto      endctr
149100020628     c                   endif
149200020626     c                   endif
149300901115     C* LINEA PARTENZA
149400131111     c                   clear                   lnpntw
149500940713     C     V1CLP1        IFNE      0
149600940713     C                   MOVEL     V1CLP1        NUM3
149700940713     C     NUM3          CHAIN     AZORG01L                           42
149800940713     C   42              DO
149900940713     C                   MOVEL     MSG(5)        V1CMSG
150000940713     C                   SETON                                        28  90
150100940713     C                   GOTO      ENDCTR
150200940713     C                   ENDDO
150300940713     C                   MOVEL     ORGDES        V1CDEP
150400131121     C****               MOVEL     ORGDES        V2DLNP
150500000703     C                   movel     orgde3        og143
150600050304     c                   eval      lnpntw = §ogntw
150700901115     C                   SETON                                        02
150800901115     C                   END
150900940413      *
151000971223      * E' OBBLIGATORIA LNA O LNP
151100971223     C     V1CLNA        IFEQ      *ZERO
151200940713     C     V1CLP1        ANDEQ     *ZERO
151300971223     C     V1CRPE        ANDEQ     *BLANKS
151400010509     c     sel4          andeq     0
151500020628     c     sel3          andeq     *blanks
151600110930     c     v1crmn        andeq     *zeros
151700110930     c     v1crma        andeq     *blanks
151800131113     c     v1cnot        andeq     *blanks
151900131114     c     v1cbarcode    andeq     *zeros
152000940713     C                   MOVEL     MSG(4)        V1CMSG
152100940713     C                   SETON                                        412890
152200940712     C                   GOTO      ENDCTR
152300940712     C                   ENDIF
152400940413     C*
152500911023     C* DATA DAL OBBLIGATORIA
152600940713     C     V1CDTD        IFEQ      0
152700971223     C     V1CRPE        ANDEQ     *BLANKS
152800010509     C     sel4          ANDEQ     0
152900110930     C     v1crmn        ANDEQ     0
153000110930     C     v1crma        ANDEQ     *blanks
153100131111     C     v1cnot        ANDEQ     *blanks
153200131114     c     v1cbarcode    andeq     *zeros
153300131113     c
153400131113     c*********          if        v1cssp <> *blanks or
153500131113     c
153600131113     c                   if        (v1crsd = *blanks and v1crsm = *blanks and
153700131111     c                              v1cind = *blanks and v1clod = *blanks and
153800131111     c                             sel3 <> *blanks and *in01) or
153900131111     c                               sel3 = *blanks
154000940713     C                   MOVEL     MSG(8)        V1CMSG
154100940713     C                   SETON                                        285190
154200911023     C                   GOTO      ENDCTR
154300940712     C                   ENDIF
154400911023     C                   END
154500911023     C*
154600940712     C                   Z-ADD     *ZERO         COMDTD
154700940712     C                   Z-ADD     *ZERO         COMAAD
154800940712     C                   Z-ADD     *ZERO         COMMGD
154900940713     C     V1CDTD        IFNE      *ZERO
155000940713     C                   MOVE      V1CDTD        G02DAT
155100930705     C                   MOVEL     *BLANK        G02ERR
155200940713     C                   CALL      'XSRDA8'
155300930705     C                   PARM                    WLBDAT
155400930705     C     G02ERR        IFEQ      '1'
155500940713     C                   MOVEL     MSG(8)        V1CMSG
155600940713     C                   SETON                                        285190
155700930705     C                   GOTO      ENDCTR
155800930705     C                   ELSE
155900940713     C                   Z-ADD     G02DAT        V1CDTD
156000940713     C                   MOVE      G02INV        COMDTD            8 0
156100940713     C                   MOVEL     G02INV        COMAAD            4 0
156200930705     C                   MOVE      G02INV        COMMGD            4 0
156300930705     C                   END
156400940712     C                   ENDIF
156500900515     C* DATA SPEDIZIONE AL
156600940712     C                   Z-ADD     *ZERO         COMDTA
156700940713     C     V1CDTA        IFNE      0
156800940713     C                   MOVE      V1CDTA        G02DAT
156900930705     C                   MOVEL     *BLANK        G02ERR
157000940713     C                   CALL      'XSRDA8'
157100930705     C                   PARM                    WLBDAT
157200930705     C     G02ERR        IFEQ      '1'
157300941215     C                   MOVEL     MSG(8)        V1CMSG
157400940713     C                   SETON                                        285290
157500930705     C                   GOTO      ENDCTR
157600930705     C                   ELSE
157700940713     C                   Z-ADD     G02DAT        V1CDTA
157800940713     C                   MOVE      G02INV        COMDTA            8 0
157900900515     C                   END
158000930705     C*
158100900515     C* DATA SPEDIZIONE AL MAGGIORE DATA SPEDIZIONE AL
158200911023     C     COMDTA        IFLT      COMDTD
158300940713     C                   MOVEL     MSG(10)       V1CMSG
158400940713     C                   SETON                                        285152
158500940713     C                   SETON                                        90
158600900515     C                   GOTO      ENDCTR
158700900515     C                   END
158800940318      *
158900940712     C                   ELSE
159000940712      *
159100940712      * SE NON IMMESSA LA DATA FINALE, LA PONGO UGUALE A QUELLA INIZIALE
159200940712     C                   Z-ADD     COMDTD        COMDTA
159300940318      *
159400900515     C                   END
159500940318      *
159600940712      * VERIFICO SE LE DUE DATE SONO COINCIDENTI
159700131115     c                   if        comdtd>0
159800940712     C     COMDTD        COMP      COMDTA                                 15
159900131115     c                   endif
160000901115     C***
160100911024     C* GIACENTI
160200911024     C* SCELTE INCONGRUENTI
160300940713     C     V1CGIA        IFEQ      'G'
160400940713     C     V1CSSP        IFEQ      '2'
160500940713     C     V1CSSP        OREQ      '3'
160600940713     C                   MOVEL     MSG(14)       V1CMSG
160700940713     C                   SETON                                        285690
160800911024     C                   GOTO      ENDCTR
160900911024     C                   END
161000911024     C                   END
161100901115     C* NUMERO SERIE AL DEVE ESSERE > NUMERO SERIE DAL
161200901115     C*  SE IMMESSO
161300940713     C     V1CPSD        IFGT      V1CPSA
161400940713     C                   MOVEL     MSG(11)       V1CMSG
161500940713     C                   SETON                                        284890
161600901115     C                   GOTO      ENDCTR
161700901115     C                   END
161800901115     C* NUMERO SEGNACOLLO AL DEVE ESSERE > NUMERO SEGNACOLLO DAL
161900940713     C     V1CSCA        IFGT      0
162000940713     C     V1CSCD        ANDGT     V1CSCA
162100940713     C                   MOVEL     MSG(12)       V1CMSG
162200940713     C                   SETON                                        284990
162300901115     C                   GOTO      ENDCTR
162400901115     C                   END
162500901115     C* SE IMMESSO AL IMMETTERE ANCHE DAL
162600940713     C     V1CSCA        IFGT      0
162700940713     C     V1CSCD        ANDEQ     0
162800940713     C                   MOVEL     MSG(13)       V1CMSG
162900940713     C                   SETON                                        284990
163000901115     C                   GOTO      ENDCTR
163100901115     C                   END
163200940713     C     V1CSCD        COMP      0                                  07
163300970925     C     V1CSCA        COMP      0                                  84
163400940923     C*
163500940923     C* LA FILIALE SEGNACOLLO DEVE ESSERE VALIDA, SE IMMESSSA
163600940923     C     V1CFLS        IFNE      *ZERO
163700940923     C     V1CFLS        CHAIN     AZORG01L                           46
163800941014     C     *IN46         IFEQ      *ON
163900941014     C     *IN46         OREQ      *OFF
164000941014     C     ORGFVA        ANDNE     *BLANKS
164100941014     C                   SETON                                        284690
164200941014     C                   MOVEL     MSG(17)       V1CMSG
164300941014     C                   GOTO      ENDCTR
164400940923     C                   ENDIF
164500941014     C                   ENDIF
164600940923     C*
164700901115     C* SE IMMESSA RAGIONE SOCIALE CERCO PRIMO BLANKS
164800940713     C     V1CRSM        IFNE      *BLANKS
164900901115     C                   SETON                                        11
165000940713     C                   MOVEA     V1CRSM        C1
165100131111     C                   Z-ADD     30            XX                2 0
165200930702     C     C1(XX)        DOWEQ     *BLANKS
165300930702     C                   SUB       1             XX
165400901116     C                   END
165500901116     C                   END
165600901115     C* IMMESSO DESTINATARIO: CERCO PRIMO BLANKS
165700940713     C     V1CRSD        IFNE      *BLANKS
165800901115     C                   SETON                                        14
165900940713     C                   MOVEA     V1CRSD        C1
166000131111     C                   Z-ADD     30            ZZ                2 0
166100930702     C     C1(ZZ)        DOWEQ     *BLANKS
166200930702     C                   SUB       1             ZZ
166300901116     C                   END
166400901115     C                   END
166500131113     C* località DESTINATARIO: CERCO PRIMO BLANKS
166600131113     C     V1Clod        IFNE      *BLANKS
166700131113     C                   SETON                                        14
166800131113     C                   MOVEA     V1Clod        C1
166900131113     C                   Z-ADD     30            LL                2 0
167000131113     C     C1(LL)        DOWEQ     *BLANKS
167100131113     C                   SUB       1             LL
167200131113     C                   END
167300131113     C                   END
167400131113     C* Indirizzo DESTINATARIO: CERCO PRIMO BLANKS
167500131113     C     V1Cind        IFNE      *BLANKS
167600131113     C                   SETON                                        14
167700131113     C                   MOVEA     V1Cind        C1
167800131113     C                   Z-ADD     30            DD                2 0
167900131113     C     C1(DD)        DOWEQ     *BLANKS
168000131113     C                   SUB       1             DD
168100131113     C                   END
168200131113     C                   END
168300131113     c
168400131113     c* 14 on anche per indirizzo o località  DESTINAT
168500131113     c                   if        not *in11 and
168600131113     c                              v1clod<>*blanks or v1cind<>*blanks
168700131113     c                   seton                                        14
168800131113     c                   endif
168900131113     c
169000131113      * Nazione provincia destinatario senza ragioni sociali
169100060613      * imposto per destinatario se non è stata impostata la lna  e la linea
169200060613      * partenza
169300060613     c                   if        v1cprd <> *blanks and
169400020628     c                             not *in11 and not *in14 and not *in01
169500060613     c                   eval      *in14 = *on
169600020628     c                   endif
169700060613     c                   if         v1cnzd <> *blanks and v1cprd = *blanks and
169800060613     c                             not *in11 and not *in14 and not *in01
169900060613     c                             and not *in02
170000060613     c                   eval      *in14 = *on
170100060613     c                   endif
170200901115     C                   END
170300901115     C*
170400901115     C** NUMERO SPEDIZIONE
170500901115     C** LINEA PARTENZA
170600140117     C***  SEL2          IFNE      0
170700140117     c                   if        sel2<> 0 and v1cbarcode<=0
170800940713     C                   Z-ADD     V1CLP2        NUM3
170900940713     C     NUM3          CHAIN     AZORG01L                           45
171000940713     C   45              DO
171100940713     C                   MOVEL     MSG(5)        V1CMSG
171200940713     C                   SETON                                        28  90
171300940713     C                   GOTO      ENDCTR
171400940713     C                   ENDDO
171500911023     C* ANNO SPEDIZIONE
171600940713     C     V1CAAS        IFEQ      0
171700940713     C                   MOVEL     MSG(7)        V1CMSG
171800940713     C                   SETON                                        285590
171900911023     C                   GOTO      ENDCTR
172000911023     C                   END
172100940713     C     KLNPX         CHAIN     FNARB01L                           44
172200030527     C*
172300940713     C   44              DO
172400940713     C                   MOVEL     MSG(6)        V1CMSG
172500940713     C                   SETON                                        28  90
172600940713     C                   GOTO      ENDCTR
172700940713     C                   ENDDO
172800901115     C*
172900901115     C                   END
173000131111     c***
173100131113     C* B A R C O D E   L D V
173200131111     c***
173300131111     c                   if        v1cbarcode>0
173400131111     c                   z-add     b_aas         arbaas
173500131111     C     b_aas         IFLT      100
173600131111     C     b_aas         IFGT      60
173700131111     C                   ADD       1900          arbAAS
173800131111     C                   ELSE
173900131111     C                   ADD       2000          arbAAS
174000131111     C                   ENDIF
174100131111     C                   ENDIF
174200131111     c                   z-add     b_lnp         arblnp
174300131111     c                   z-add     b_nrs         arbnrs
174400131111     c                   z-add     b_nsp         arbnsp
174500131114     C     Karb2         CHAIN     FNARB01L
174600131111     C*
174700131111     C                   if        not%found(fnarb01l)
174800131111     C                   MOVEL     MSG(32)       V1CMSG
174900131111     C                   SETON                                        286090
175000131111     C                   GOTO      ENDCTR
175100131111     C                   endif
175200131111     C                   endif
175300010508      ***
175400010508      * N U M E R O   O R M
175500010508      ***
175600010508     C     sel4          ifne      0
175700010508     C     sel2          andeq     0
175800010508      * P.O. EMISSIONE
175900010508     C                   z-add     v1cpoe        num3
176000010508     C     num3          chain     azorg01l                           53
176100010508     C     *in53         ifeq      *on
176200010508     C                   movel     msg(21)       v1cmsg
176300010508     C                   seton                                        28  90
176400010508     C                   goto      endctr
176500010508     C                   endif
176600010508      * N.ORM
176700010510     C                   MOVEL     'M'           WTRC
176800010510     C                   MOVEL(P)  SEL4          WN14             14
176900060216     C     kar44         chain     fiar404l                           54
177000010508     C     *in54         ifeq      *on
177100010508     C                   movel     msg(22)       v1cmsg
177200010508     C                   seton                                        28  90
177300010508     C                   goto      endctr
177400010508     C                   endif
177500010508      *
177600010508     C                   seton                                        25
177700010508      *
177800010508     C                   endif
177900131111     c
178000901115     C*
178100911023     C* TIPO SPEDIZIONE ESPRESSO
178200131114     C                   MOVEL     *BLANKS       V01DTSP
178300940713     C     V1CTSP        IFNE      ' '
178400940715     C     V1CTSP        IFEQ      '?'
178500940715     C                   CALL      'X§TABER'
178600040217     C                   PARM      CodUt         §KUT              1 0
178700040217     C                   PARM      '5E'          COD
178800040217     C                   PARM      *blanks       KEY
178900040217     C                   PARM      *blanks       §DES             30
179000940715     C                   MOVEL     KEY           V1CTSP
179100940715     C                   SETON                                        90
179200940715     C                   MOVEL     'S'           WRIEM
179300940715     C                   ENDIF
179400940715     C*
179500090604     c                   clear                   ds5e
179600940715     C                   MOVEL     '5E'          COD
179700940715     C                   MOVEL     *BLANKS       KEY
179800940715     C                   MOVEL     V1CTSP        KEY
179900940715     C     KTAB1         CHAIN     TABEL00F                           43
180000941014     C     *IN43         IFEQ      *ON
180100941014     C     *IN43         OREQ      *OFF
180200950221     C     TBLFLG        ANDNE     ' '
180300940715     C                   MOVEL     MSG(16)       V1CMSG
180400941014     C                   SETON                                        284390
180500940715     C                   GOTO      ENDCTR
180600941014     C                   ELSE
180700090604     C                   MOVEL     TBLUNI        ds5e
180800131114     C                   MOVEL     §5ed08        V01DTSP
180900991222     C                   ENDIF
181000940715     C                   ENDIF
181100040216      *
181200991222     c*
181300991222     c* Se richiesta linea di arrivo dpd il riferimemnto immesso deve
181400060613     c*  essere composto soltanto da numeri se <=11
181500060613     C*  alfanumerico       se=14
181600050121     c                   clear                   wnodpd            1
181700060613    1c                   if        lnantw = 'DPD' or lnpntw = 'DPD' or
181800050304     c                             lnpntw = *blanks or lnantw = *blanks
181900060613    2c                   if        v1crpe<>*blanks
182000060613    3c                   select
182100060613     c* ok se lungo 14
182200060613     c                   when      %subst(v1crpe:15:1)=' '  and
182300060613     c                             %subst(v1crpe:14:1)<>' '
182400060613     c* ok se lungo 11 ma tutto numerico
182500060613     c                   when      %subst(v1crpe:12:4)=*blanks and
182600060613     c                             %subst(v1crpe:11:1)<>' '
182700060613     C                   MOVEA     V1CRPe        K11
182800060613     C                   Z-ADD     1             YY                2 0
182900060613     c
183000060613    4C                   DO        11            YY
183100060613     C     K11(YY)       LOOKUP    CNM                                    30
183200060613    5C     *IN30         IFEQ      *OFF
183300060613     c* Se non è tutto numerico non è DPD
183400060613     c                   eval      wnodpd = '1'
183500060613     c                   leave
183600060613    5c                   endif
183700060613    4c                   enddo
183800060613   x3c                   other
183900060613     c
184000060613     c* se non è lungo 11 o 14 non è DPD
184100060613     c                   eval      wnodpd = '1'
184200060613    3c                   endsl
184300060613     c
184400060613    3c                   if        wnodpd='1'
184500060613    4c     lnantw        ifeq      'DPD'
184600060613     c     lnpntw        oreq      'DPD'
184700060613     C                   MOVEL     MSG(18)       V1CMSG
184800060613     C                   SETON                                        285890
184900060613     C                   GOTO      ENDCTR
185000060613    4C                   ENDIF
185100060613    3C                   ENDIF
185200060613    2c                   endif
185300061117   x1c                   else
185400061117     c                   eval      wnodpd = '1'
185500060613    1c                   endif
185600941014     C*
185700911023     C** FILE STORICO
185800940713     C     V1CSSP        COMP      '1'                                    18
185900940713     C     V1CSSP        COMP      '2'                                    19
186000940713     C     V1CSSP        COMP      '3'                                    20
186100940713     C     V1CSSP        COMP      ' '                                    21
186200920911     C*
186300940712      * DEVO CONSIDERARE DELLE PRIORITA' PER LE SELEZIONI IMMESSE DALL'UTENTE:
186400010508      *    1°    NUMERO O.R.M.           ( 25 ON)
186500010508      *    2°    MITTENTE                ( 11 ON )
186600010508      *    3°    DESTINATARIO            ( 14 ON )
186700010508      *    4°    LINEA DI PARTENZA       ( 02 ON )
186800010508      *    5°    LINEA DI ARRIVO         ( 01 ON )
186900940712      *
187000010508     C   25              setoff                                       111402
187100010508     C   25              setoff                                       01
187200131113     C   14              SETOFF                                       020111
187300131113     C   11              SETOFF                                       0201
187400940712     C   02              SETOFF                                       01
187500991222     c*se lna dpd e immesso riferimento partner estero, se meno di 12
187600060518     c*  caratteri, calcolo il 12 --> ora non più: il check digit per la
187700060518     c*  ricerca non serve più
187800991222     C                   CLEAR                   krpe
187900050121    1c                   if        v1crpe <> *blanks and (lnantw='DPD'
188000050304     c                             or lnpntw = 'DPD' or wnodpd=' ')
188100060613     C                   movel     V1CRpe        krpe             14
188200991222     C                   ENDIF
188300911023     C*
188400900514     C     ENDCTR        ENDSR
188500020628      *------------------------------------------------------------------------*
188600020628      *    Controlla i dati nazione e provincia destinatario
188700020628      *------------------------------------------------------------------------*
188800020628     c     ContrDatiDes  begsr
188900020628     c                   if        v1cnzd = *blanks and v1cprd = *blanks
189000020628     c                   eval      v1cmsg = msg(23)
189100020628     c                   eval      *in57 = *on
189200020628     c                   eval      *in28 = *on
189300020628     c                   goto      EndContr
189400020628     c                   endif
189500020628      * ricerca tabella nazione
189600020628     c     '?'           scan      v1cnzd                                 90
189700020628     c                   movel     '15'          cod
189800020628     c                   if        *in90
189900020628     c                   call      'TRUL19R'
190000020628     c                   parm                    cod
190100020628     c                   parm                    ordina
190200020628     c                   parm                    key
190300020628     c                   parm                    comando
190400020628     c                   movel     key           v1cnzd
190500020628     c                   eval      *in57 = *on
190600020628     c                   movel     'S'           wriem
190700020628     c                   goto      EndContr
190800020628     c                   endif
190900020628      * controllo nazione destinatario
191000020628     c                   clear                   ds15
191100020628     c                   movel     '15'          cod
191200020628     c                   clear                   key
191300020628     c                   movel     v1cnzd        key
191400020628     c     ktab1         chain     tabel00f
191500020628     c                   if        not %found(tabel00f) or tblflg <> *blanks
191600020628     c                   eval      v1cmsg = msg(24)
191700020628     c                   eval      *in57 = *on
191800020628     c                   eval      *in28 = *on
191900020628     c                   goto      EndContr
192000020628     c                   endif
192100020628     c                   movel     tbluni        ds15
192200060613     c* Se cambiata nazione, clearo forzatura msg se nazine estera
192300060613     c                   if        v1cnzd<>savnzd
192400060613     c                   clear                   wforz1            1
192500060613     c                   eval      savnzd=v1cnzd
192600060613     c                   endif
192700020628      * per nazione italia la provincia è obbligatoria
192800020628     c                   if        §15ita = 'I' and v1cprd = *blanks
192900020628     c                   eval      v1cmsg = msg(25)
193000020628     c                   eval      *in50 = *on
193100020628     c                   eval      *in28 = *on
193200020628     c                   goto      EndContr
193300020628     c                   endif
193400020628      * ricerca tabella provincia
193500020628     c     '?'           scan      v1cprd                                 90
193600020628     c                   movel     'PR'          cod
193700020628     c                   if        *in90
193800020628     c                   call      'TRUL19R'
193900020628     c                   parm                    cod
194000020628     c                   parm                    ordina
194100020628     c                   parm                    key
194200020628     c                   parm                    comando
194300020628     c                   movel     key           v1cprd
194400020628     c                   eval      *in50 = *on
194500020628     c                   movel     'S'           wriem
194600020628     c                   goto      EndContr
194700020628     c                   endif
194800020628      * controllo provincia destinatario
194900020628     c                   if        v1cprd <> *blanks and §15ita = 'I'
195000020628     c                   clear                   dspr
195100020628     c                   movel     'PR'          cod
195200020628     c                   clear                   key
195300020628     c                   movel     v1cprd        key
195400020628     c     ktab1         chain     tabel00f
195500020628     c                   if        not %found(tabel00f) or tblflg <> *blanks
195600020628     c                   eval      v1cmsg = msg(25)
195700020628     c                   eval      *in50 = *on
195800020628     c                   eval      *in28 = *on
195900020628     c                   goto      EndContr
196000020628     c                   endif
196100020628     c                   movel     tbluni        dspr
196200020628      * se nazione italia la provincia deve essere italia
196300020628      * nella tabella ci sono anche le provincie estere e per sapere se ok
196400020628      * faccio il controllo con il codice tassazione per estero
196500020628      * se vuoto vuol dire che è provincia estera
196600020628     c                   if        §15ita = 'I' and §prcte = *blanks
196700020628     c                   eval      v1cmsg = msg(25)
196800020628     c                   eval      *in50 = *on
196900020628     c                   eval      *in28 = *on
197000020628     c                   goto      EndContr
197100020628     c                   endif
197200020628     c                   endif
197300020628
197400020628     c     EndContr      endsr
197500940318      **************************************************************************
197600940318      *    ELABORA BOLLE DA VISUALIZZARE
197700940318      **************************************************************************
197800900515     C     ELAB          BEGSR
197900940318      *
198000131114     C                   SETON                                        35
198100131114     C                   WRITE     LRI6C02
198200131114     C                   SETOFF                                       35
198300131114     C*
198400940713     C                   Z-ADD     1             REC2
198500911024     C                   Z-ADD     1             C
198600131111     c                   eval      wEnd = *off
198700131115     c                   clear                   readFile          3
198800131113     c
198900131113     C                   Z-ADD     COMAAD        $AAS
199000131113     C                   Z-ADD     COMMGD        $MGS
199100131111     c
199200131111    1c                   select
199300010508     c* O.R.M.
199400131111    1c     *in25         wheneq    *on
199500010508     C                   MOVEL     'M'           WTRC
199600010508     C                   MOVEL(P)  SEL4          WN14             14
199700060216     C     Kar44         SETLL     FiAR404L
199800131111     c
199900131111     c***
200000131111     C*  RIFERIMENTO P A R T N E R   E S T E R O
200100131111     c***
200200131111    1C     V1CRPE        whenne    *BLANKS
200300050121     c                   clear                   wgiro             1
200400050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
200500971223     C                   MOVEL(P)  V1CRPE        WN30             30
200600991222     C                   MOVEL     'E'           WTRC
200700991222     c                   else
200800991222     c* D P D
200900991222     C                   MOVEL(P)  krpe          WN30             30
201000060518     C***                MOVEL     'F'           WTRC
201100060518     C                   MOVEL     'I'           WTRC
201200991222     c                   endif
201300991222     c**
201400060216     C     KAR4          SETLL     FiAR403L
201500991222     c**
201600131111     c* C H I   S O N O
201700131111     c**
201800131111    1C     V1CNOT        whenne    *BLANKS
201900131111     C     KarS          SETLL     FIarS02L
202000131111     c* R N N
202100131111    1c     v1crmn        whengt    *zeros
202200110930     c     v1crmn        setll     fnarb33l
202300131115     c                   eval      Readfile='DSP'
202400131111     c* R M A
202500131111    1c     comrma        whenne    *blanks
202600111005     c     comrma        setll     fnarb32l
202700131115     c                   eval      Readfile='DSP'
202800131111     c
202900131113      * RSD/RSM/IND/LOD o solo NAZ/PRV       -----------------------------------
203000110930     c
203100131113     c                   when      *in14  OR *IN11
203200131111     c                   exsr      Presql
203300131111     c                   exsr      Opencursor
203400131111   x1c                   other
203500131111     c
203600971223      *
203700940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1)  -----------------------------------
203800971223    2C   18              DO
203900940713     C     V1CLNA        IFNE      *ZERO
204000940713     C                   Z-ADD     V1CLNA        COMLNA
204100940712     C                   ELSE
204200940712     C                   Z-ADD     L1(1)         COMLNA
204300940712     C                   ENDIF
204400020910     C     K1ARB         SETLL     FNARB78L
204500131115     c                   eval      Readfile='DSP'
204600971223    2C                   ENDDO
204700940318      *
204800940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
204900971223    2C   19              DO
205000940713     C     V1CLNA        IFNE      *ZERO
205100120330     C     V1CLNA        SETLL     FNARB05L
205200940712     C                   ELSE
205300120330     C     *LOVAL        SETLL     FNARB05L
205400940712     C                   ENDIF
205500971223    2C                   ENDDO
205600940318      *
205700940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
205800971223    2C   20              DO
205900940713     C     V1CLNA        IFNE      *ZERO
206000940713     C                   Z-ADD     V1CLNA        COMLNA
206100940712     C                   ELSE
206200940712     C                   Z-ADD     L1(1)         COMLNA
206300940712     C                   ENDIF
206400940713     C     K4ARB         SETLL     FNARB54L
206500131115     c                   eval      Readfile='DCM'
206600971223    2C                   END
206700940318      *
206800940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
206900971223    2C   21              DO
207000131113     c
207100131113     c* per LINEA DI PARTENZA
207200131113    3C     *IN02         IFEQ      *ON
207300950406     C                   SELECT
207400950406     C* Serie = 0 e segnacollo dal diverso da 0 => i segnacolli sono
207500950406     C* sequenziali
207600950406     C     V1CPSD        WHENEQ    *ZEROS
207700950406     C     V1CSCD        ANDNE     *ZEROS
207800950406     C                   Z-ADD     V1CSCD        COMSCD
207900950406     C     K6ARB         SETLL     FNARB27L                               31
208000131115     c                   eval      Readfile='DSP'
208100950406     C     *IN31         IFEQ      *OFF
208200950406     C* Non trovata bolla col numero segnacollo specificato: il
208300950406     C* segnacollo non è quello iniziale della bolla e quindi è sicura-
208400950406     C* mente incluso nella spedizione precedente.
208500950406     C     K6ARB1        READPE    FNARB27L                               31
208600131209     C  n31              Z-ADD     ARBNCD        COMSCD
208700950406     C     K6ARB         SETLL     FNARB27L
208800950406     C                   END
208900131113     c
209000950406     C* Non immesso segnacollo dal oppure serie diversa da zero (segna-
209100950406     C* colli non sequenziali) mi posiziono con segnacollo = 0
209200950406     C                   OTHER
209300950406     C                   MOVE      *ZEROS        COMSCD
209400950405     C     K6ARB         SETLL     FNARB27L
209500131115     c                   eval      Readfile='DSP'
209600950406     C                   ENDSL
209700131113    3C                   END
209800131113     c*
209900131113     c* Per LINEA DI ARRIVO
210000940712     C   01K8ARB         SETLL     FNARB26L
210100131115     c                   eval      Readfile='DSP'
210200131111     c
210300971223    2C                   ENDDO
210400131111    1C                   ENDSL
210500940318      *
210600911023     C                   Z-ADD     0             COMOD
210700911023     C                   EXSR      CARSF
210800940318      *
210900901116     C                   ENDSR
211000940318      **************************************************************************
211100940318      *    CARICO SUBFILE IN AVANTI
211200940318      **************************************************************************
211300900515     C     CARSF         BEGSR
211400940318      *
211500900518     C                   Z-ADD     1             B                 2 0
211600940713     C                   Z-ADD     COMOD         NR2
211700000228     C                   clear                   War4AAS
211800000228     C                   clear                   War4LNP
211900000228     C                   clear                   War4NRS
212000000228     C                   clear                   War4NSP
212100000228      *
212200131113    1C     B             DOWLE     18
212300060216     c                   setoff                                       32
212400131111
212500131113    2c                   select
212600131111     c***
212700131111     c* O R M
212800131111     c***
212900131111     c     *in25         wheneq    *on
213000060216     c     kar44         reade     fiar404l                               30
213100010509     C  N30KARB4         CHAIN     FNARB01L                           32
213200131111     C***
213300131111      * R I F .   p a r t n e r
213400131111     C***
213500131111     C     V1CRPE        whenne    *BLANKS
213600050121     c     leggar4       tag
213700000228      *
213800050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
213900050304     c                             and wgiro <> '2'
214000060216     C     KAR4          READE     FiAR403L                               30
214100991222     c                   else
214200000228      * DPD
214300060216     C     wtrc          READE     FiAR403L                               30
214400000228     C                   If        *in30 = *off
214500060518     c                   movel     ar4n30        dsbl4i
214600000228      *
214700000228     c                   Select
214800000228      * Se non trovo n.parcel e selezionati 0 rcd o più di 1: fine lettura
214900060613     c                   When       §b4ipn <> krpe  and  nr2 <> 1
215000991222     C                   SETON                                            30
215100000228      * Se non trovo n.parcel e è selezionato 1 rcd:  fine lettura e
215200060216      *                                               riposiziono FiAR4
215300060613     c                   When       §b4ipn<> krpe
215400000228     C                   SETON                                            30
215500060216     C     KAR4W         CHAIN     FiAR401L
215600000228      * Se ho trovato il n.parcel ed è il 1o memorizzo key sped
215700000228     c                   When       NR2 = *zeros
215800000228     C                   Z-ADD     AR4AAS        War4AAS
215900000228     C                   Z-ADD     AR4LNP        War4LNP
216000000228     C                   Z-ADD     AR4NRS        War4NRS
216100000228     C                   Z-ADD     AR4NSP        War4NSP
216200000228     c                   Endsl
216300000228     c                   Endif
216400000228      *
216500991222     c                   endif
216600991222     c
216700050304     c                   if        *in30 and wgiro <> '2' and
216800050304     c                             lnpntw <> 'DPD' and lnantw <> 'DPD' and
216900050304     c                             wnodpd = *blanks
217000050121     c                   eval      wgiro = '2'
217100060518     C                   MOVEL     'I'           WTRC
217200050121     C                   MOVEL(P)  KRPe          WN30             30
217300060216     C     Kar4          SETLL     Fiar403l
217400050121     c                   goto      leggar4
217500050121     c                   endif
217600061117     c*
217700061117     c* Se rif.partner può essere DPD non export e immesso rif. lungo 11
217800061117     c* faccio tentaivo di ricerca con rif.truccato
217900061117     c                   if        *in30 and lnantw<>'DPD'and wnodpd=*blanks
218000061117     c                             and %subst(krpe:12:1)=' '
218100061117     C                   eval      krpe='0'+%subst(krpe:1:3)+'99'
218200061117     c                             +%subst(krpe:4:8)
218300061117     C                   MOVEL(P)  KRPe          WN30             30
218400061117     C     Kar4          SETLL     Fiar403L
218500061117     c                   goto      leggar4
218600061117     c                   endif
218700050121     c*
218800971223     C  N30KARB4         CHAIN     FNARB01L                           32
218900131111     c**
219000131111     c* C H I   S O N O
219100131111     c**
219200131111    1C     V1CNOT        whenne    *BLANKS
219300131111     C     KarS          reade     FIarS02L                               30
219400131111     c                   if        not *in30
219500131111     C     KarSXT        CHAIN     FNarT27L                           32
219600131111     C  N32KARTXP        CHAIN     FNarb01L                           32
219700131111    3C     *IN32         IFEQ      *On
219800131111     C                   SETON                                        30
219900131111    3C                   ENDIF
220000131111    2C                   ENDIF
220100110930     c*
220200131111     c* Interrogazione per R M N
220300131111    1C     v1crmn        whengt    *zeros
220400111003     c     v1crmn        reade     fnarb33l                               30
220500940318      *
220600131111     c* Interrogazione per R M A
220700131111    1C     comrma        whenne    *BLANKS
220800111005     c     comrma        reade     fnarb32l                               30
220900131111     c
221000131111      * RSD/RSM/IND/LOD                      -----------------------------------
221100131111     c
221200131113     c                   when      *IN14 OR *IN11
221300131113
221400131111     c                   if        Wend=*off
221500131111     c                   exsr      Readcursor
221600131111     c                   endif
221700131111     c
221800131111     c                   if        Wend=*on
221900131111     c                   seton                                        30
222000131111     c                   endif
222100111005     c*
222200131111     C                   other
222300940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1')  ----------------------------------
222400940318     C   18              DO
222500940713     C     V1CLNA        IFNE      *ZERO
222600020910     C   15K2ARB         READE     FNARB78L                               30
222700020910     C  N15K3ARB         READE     FNARB78L                               30
222800940712      *
222900940712     C                   ELSE
223000940712      *
223100950214     C                   SETON                                            31
223200950214     C                   SETOFF                                           30
223300950214     C     *IN31         DOWEQ     *ON
223400020910     C   15K2ARB         READE     FNARB78L                               31
223500020910     C  N15K3ARB         READE     FNARB78L                               31
223600940318     C   31              ADD       1             C
223700940318     C   31L1(C)         IFNE      0
223800940318     C                   Z-ADD     L1(C)         COMLNA
223900020910     C     K1ARB         SETLL     FNARB78L
224000940318     C                   ELSE
224100950214     C                   SETOFF                                       31
224200940318     C                   SETON                                        30
224300940318     C                   ENDIF
224400940318     C                   ENDDO
224500940318      *
224600940712     C                   ENDIF
224700940318     C                   ENDDO
224800940318      *
224900940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
225000940712     C   19              DO
225100940713     C     V1CLNA        IFNE      *ZERO
225200120330     C     V1CLNA        READE     FNARB05L                               30
225300940712     C                   ELSE
225400120330     C                   READ      FNARB05L                               30
225500940712     C                   ENDIF
225600940712     C                   ENDDO
225700940318      *
225800940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
225900940318     C   20              DO
226000940713     C     V1CLNA        IFNE      *ZERO
226100940712     C   15K5ARB         READE     FNARB54L                               30
226200940713     C  N15V1CLNA        READE     FNARB54L                               30
226300940712      *
226400940712     C                   ELSE
226500940712      *
226600940712     C                   SETON                                            31
226700940712     C     *IN31         DOWEQ     '1'
226800940712     C  N36
226900940712     CAN 15K5ARB         READE     FNARB54L                               31
227000940712     C  N36
227100940712     CANN15COMLNA        READE     FNARB54L                               31
227200060216     C  N31              SETOFF                                       30
227300980120     C   31              ADD       1             C
227400940318     C   31L1(C)         IFNE      0
227500940318     C                   Z-ADD     L1(C)         COMLNA
227600940712     C     K4ARB         SETLL     FNARB54L
227700940318     C                   SETOFF                                       36
227800940318     C                   ELSE
227900940318     C                   SETON                                        30
228000940318     C                   SETOFF                                       31
228100940318     C                   ENDIF
228200940318     C                   ENDDO
228300940318      *
228400940712     C                   ENDIF
228500940318     C                   ENDDO
228600940318      *
228700940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
228800940318     C   21              DO
228900940712     C   01
229000940712     CAN 15K9ARB         READE     FNARB26L                               30
229100940713     C   01
229200940713     CANN15V1CLNA        READE     FNARB26L                               30
229300940712     C   02
229400940712     CAN 15K7ARB         READE     FNARB27L                               30
229500940713     C   02
229600940713     CANN15V1CLP1        READE     FNARB27L                               30
229700131113     c                   ENDDO
229800131113     c                   endsl
229900940318      *
230000060216     c  n32
230100060216     CanN30              EXSR      CONFR
230200900515     C  N30
230300060216     CANN32              EXSR      EMISS
230400900515     C  N30              END
230500131113     c
230600940713     C                   Z-ADD     NR2           COMOD             4 0
230700930518     C                   SETOFF                                       1362
230800131113     c
230900131209     c** 30              if        Wend=*on
231000131209     c   30              if        WopenC='1'
231100131113     c                   exsr      closecursor
231200131113     c                   endif
231300940318      *
231400900515     C                   ENDSR
231500940317      **************************************************************************
231600940317      *    CONFRONTO PER CERCARE BOLLE DA EMETTERE
231700940317      **************************************************************************
231800900515     C     CONFR         BEGSR
231900940317      *
232000911024     C                   SETOFF                                       3236
232100131111     c                   z-add     arbmgs        arbdsp
232200131111     c                   movel     arbaas        arbdsp
232300131111     c
232400110930     c* Se richiamata per Perfect Order devo scartare la bolla se xco diverso da
232500110930     c* quello passato
232600110930     c                   if        pa1flg='W' and arbxco<>pa1xco
232700110930     c                   seton                                        32
232800110930     c                   goto      endcon
232900110930     c                   endif
233000940318      *
233100940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1')  ----------------------------------
233200940712      * DEVO CONTROLLARE LA DATA SPEDIZIONE (ARBDSP)
233300971223    1C   18              DO
233400131115     c                   if        comdtd>0 and *in15
233500131115    2C     ARBDSP        IFNE      COMDTD
233600940713     C     ARBDCM        ANDEQ     *ZERO
233700131115     c
233800131115    3C                   IF        v1clna<>0 and readfile='DSP'
233900940712     C                   SETON                                            30
234000131115     c                   endif
234100131115
234200131115     C                   SETON                                            32
234300911023     C                   GOTO      ENDCON
234400971223    2C                   END
234500131115     c                   endif
234600940318      *
234700131115     c                   if        comdtd>0 and not *in15
234800131115    2C     ARBDSP        IFGT      COMDTA
234900940713     C     ARBDCM        ANDEQ     *ZERO
235000131115     c* si potrebe uscire anche se lettura per riferimenti mittente
235100131115     c*  ma se la linea di arrivo =0 non lo fa ora .... si può sistemare
235200131115     c*  per far leggere meno
235300131115    3C                   IF        v1clna<>0 and readfile='DSP'
235400131115     C                   SETON                                            30
235500131115     c                   endif
235600131115
235700940712     C                   SETON                                            32
235800911023     C                   GOTO      ENDCON
235900971223    2C                   END
236000131115     c                   endif
236100131115     c
236200131115    1C                   END
236300940318      *
236400940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
236500940712      * DEVO CONTROLLARE LA DATA DISTINTA (ARBDDC)
236600971223    1C   19              DO
236700960527     C     ARBFBC        IFNE      *BLANKS
236800960527     C                   SETON                                        32
236900960527     C                   GOTO      ENDCON
237000960527     C                   END
237100960527     C*
237200131115     c                   if        comdtd>0 and *in15
237300131115     C     ARBDDC        IFNE      COMDTD
237400911024     C                   SETON                                            32
237500911024     C                   GOTO      ENDCON
237600911024     C                   END
237700131115     C                   ENDif
237800940318      *
237900131115     c                   if        comdtd>0 and not *in15
238000131115     C     ARBDDC        IFLT      COMDTD
238100940712     C     ARBDDC        ORGT      COMDTA
238200911024     C                   SETON                                            32
238300911024     C                   GOTO      ENDCON
238400911024     C                   END
238500131115     C                   ENDif
238600940318      *
238700940321      * SE NON C'E' NUMERO DISTINTA NON E' IN CONSEGNA
238800911024     C     ARBNDC        IFEQ      0
238900911024     C                   SETON                                            32
239000911024     C                   GOTO      ENDCON
239100911024     C                   END
239200971223    1C                   END
239300940318      *
239400940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
239500940712      *  DEVO CONTROLLARE LA DATA CONSEGNA (ARBDCM)
239600971223    1C   20              DO
239700131115     c                   if        comdtd>0 and *in15
239800131115    2C     ARBDCM        IFNE      COMDTD
239900131115    3C                   IF        v1clna<>0 and readfile='DCM'
240000131115     C                   SETON                                            30
240100131115     c                   endif
240200131115
240300940712     C                   SETON                                            32
240400911024     C                   GOTO      ENDCON
240500971223    2C                   END
240600131115     c                   endif
240700940318      *
240800131115     c                   if        comdtd>0 and not *in15
240900131212     C     ARBDCM        IFLT      COMDTD
241000131212     C                   SETON                                            32
241100131212     C                   GOTO      ENDCON
241200131212     C                   END
241300131212     c
241400131115    2C     ARBDCM        IFGT      COMDTA
241500131115    3C                   IF        v1clna<>0 and readfile='DCM'
241600131115     C                   SETON                                            30
241700131115     c                   endif
241800131115
241900940712     C                   SETON                                            32
242000911024     C                   GOTO      ENDCON
242100971223    2C                   END
242200131115     c                   endif
242300131115     c
242400131115      * SE NON C'E' data consegna  NON E' consegnata
242500131115     C     ARBDCM        IFEQ      0
242600131115     C                   SETON                                            32
242700131115     C                   GOTO      ENDCON
242800131115     C                   ENDIF
242900131115    1C                   END
243000940329      * ------------------------------------------------------------------------
243100940712      * HO SPOSTATO IL CONFRONTO CON MITTENTE E DESTINATARIO PRIMA DEL BLOCCO
243200940712      * CON 21 ON PERCHE' POSSO ACCENDERE IL 30 E ANDARE A FINE CICLO QUANDO
243300940712      * CAMBIA LA RAG.SOCIALE (EVITO DELLE LETTURE)
243400940712      *
243500940712      * OCCHIO!!   Devo usare per primo il confronto che potrebbe portarmi a
243600940712      * fine ciclo: se interrogo per destinatario (14ON) ci deve essere prima il
243700940713      * confronto con V1CRSD, se invece sono per mittente devo fare prima quello
243800940713      * con V1CRSM.
243900060613     C                   MOVE      *BLANKS       $SALTA            1
244000940712      *
244100940712      *??  R A G I O N E    S O C I A L E    D E S T I N A T A R I O  ?
244200131113     c                   if        not *in11 and not *in14
244300131113    1C     V1CRSD        IFNE      *BLANKS
244400131113     c
244500131113    2c     v1cxrsd       ifeq      ' '
244600131113     C     ZZ            SUBST(P)  ARBRSD        COMRAG
244700940712      * Se rag.soc. letta > video riposiziono solo per data con SETGT
244800131113    3C     COMRAG        IFNE      V1CRSD
244900131113     C                   MOVE      'S'           $SALTA
245000131113    3C                   ENDIF
245100131113     c*
245200131113   x2c                   else
245300131114     C                   MOVEL     ARBAAS        KAAS
245400131114     C                   MOVEL     ARBLNP        KLNP
245500131114     C                   MOVEL     ARBNRS        KNRS
245600131114     C                   MOVEL     ARBNSP        KNSP
245700131114     C                   MOVEL     'D'           KTRC
245800131114     C     KAR41         CHAIN     FiAR401L
245900131114     c                   if        %found(fiar401l)
246000131114     c                   eval      arbrsduni=arbrsd+ar4not
246100131114     c                   else
246200131114     c                   eval      arbrsduni=arbrsd
246300131114     c                   endif
246400131114     c
246500131114    3c                   if        %scan(%trimr(v1crsd):arbrsdUNI) = 0
246600131113     C                   MOVE      'S'           $SALTA
246700131113    3c                   endif
246800131113    2C                   ENDIF
246900131113    1C                   ENDIF
247000131113     c
247100131113      *??  I N D I R I Z Z O     D E S T I N A T A R I O  ?
247200131113    1C     V1Cind        IFNE      *BLANKS
247300131113     c
247400131113    2c     v1cxind       ifeq      ' '
247500131113     C     DD            SUBST(P)  ARBind        COMRAG           30
247600131113      * Se rag.soc. letta > video riposiziono solo per data con SETGT
247700131113    3C     COMRAG        IFNE      V1Cind
247800131113     C                   MOVE      'S'           $SALTA
247900131113    3C                   ENDIF
248000131113     c*
248100131113   x2c                   else
248200131113    3c                   if        %scan(%trimr(v1cind):arbind) = 0
248300131113     C                   MOVE      'S'           $SALTA
248400131113    3c                   endif
248500131113    2C                   ENDIF
248600131113    1C                   ENDIF
248700131113      *??  L O C A L I T A'      D E S T I N A T A R I O  ?
248800131113    1C     V1Clod        IFNE      *BLANKS
248900131113     c
249000131113    2c     v1cxlod       ifeq      ' '
249100131113     C     LL            SUBST(P)  ARBlod        COMRAG
249200131113      * Se rag.soc. letta > video riposiziono solo per data con SETGT
249300131113    3C     COMRAG        IFNE      V1Clod
249400131113     C                   MOVE      'S'           $SALTA
249500131113    3C                   ENDIF
249600131113     c*
249700131113   x2c                   else
249800131113    3c                   if        %scan(%trimr(v1clod):arblod) = 0
249900131113     C                   MOVE      'S'           $SALTA
250000131113    3c                   endif
250100131113    2C                   ENDIF
250200131113    1C                   ENDIF
250300940414      *
250400940712      *??  R A G I O N E    S O C I A L E    M I T T E N T E  ?
250500131113    1C     V1CRSM        IFNE      *BLANKS
250600131113    2c     v1cxrsm       ifeq      ' '
250700940712     C     XX            SUBST(P)  ARBRSM        COMRAG
250800940712      * Se rag.soc. letta > video riposiziono solo per data con SETGT
250900131113    3C     COMRAG        IFne      V1CRSM
251000940712     C                   MOVE      'S'           $SALTA
251100131113    3C                   ENDIF
251200131113   x2c                   else
251300131113    3c                   if        %scan(%trimr(v1crsm):arbrsm) = 0
251400131113     C                   MOVE      'S'           $SALTA
251500131113    3c                   endif
251600131113    2C                   ENDIF
251700131113    1C                   ENDIF
251800131113    1C                   ENDIF
251900940712      *
252000131115     c                   if        $SALTA='S'
252100131115     c                   seton                                        32
252200131115     C                   goto      ENDCON
252300131115     c                   endif
252400940318      *
252500940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
252600971223    1C   21              DO
252700940712     C     ARBDSP        IFLT      COMDTD
252800940712     C     COMDTD        ANDNE     *ZERO
252900940712     C                   SETON                                            32
253000911024     C                   GOTO      ENDCON
253100911024     C                   END
253200940318      *
253300940321     C     ARBDSP        IFGT      COMDTA
253400940712     C     COMDTA        ANDNE     *ZERO
253500940712     C                   SETON                                            32
253600131115     c                   if        readfile='DSP'
253700131115     C                   SETON                                            30
253800131115     c                   endif
253900131115     c
254000911024     C                   GOTO      ENDCON
254100131115     C                   ENDif
254200131115     c
254300971223    1C                   ENDDO
254400971223     C**
254500940317      *
254600940317      * PER OGNI SELEZIONE  ----------------------------------------------------
254700940317      *
254800940317      * TESTO LA LINEA DI ARRIVO
254900971223     C     V1CRPE        IFEQ      *BLANKS
255000971223     C     ARBLNA        ANDNE     V1CLNA
255100940713     C     V1CLNA        ANDNE     *ZERO
255200920911     C                   SETON                                            32
255300920911     C                   GOTO      ENDCON
255400971223     C                   ENDIF
255500020628      * testo la nazione/provincia del destinatario
255600060613     c                   if        v1cprd <> *blanks  and
255700020628     c                             (arbnzd <> v1cnzd  or arbprd <> v1cprd)
255800020628     c                   eval      *in32 = *on
255900020628     c                   goto      endcon
256000020628     c                   endif
256100060613     c                   if        (v1cnzd <> *blanks and v1cprd = *blanks) and
256200060613     c                             arbnzd <> v1cnzd
256300060613     c                   eval      *in32 = *on
256400060613     c                   goto      endcon
256500060613     c                   endif
256600020628
256700940317      *
256800940712      * VERIFICO ANCHE LA LNP POICHE' POTREI AVER FATTO ALTRE SELEZIONI PRIMARIE
256900940713     C     ARBLNP        IFNE      V1CLP1
257000940713     C     V1CLP1        ANDNE     *ZERO
257100940712     C                   SETON                                            32
257200940712     C                   GOTO      ENDCON
257300940712     C                   END
257400940923     C* FILIALE SEGNACOLLI
257500940926     C* SE NON VI SONO COLLI, SALTARE QUESTO CONTROLLO
257600940926     C     ARBNCD        IFNE      *ZERO
257700940926     C     ARBNCA        ORNE      *ZERO
257800940926     C     ARBFNS        OREQ      'S'
257900940926     C     ARBFLS        ORNE      *ZERO
258000940923     C     V1CFLS        IFNE      *ZERO
258100940923     C     V1CFLS        ANDNE     ARBFLS
258200940923     C                   SETON                                            32
258300940923     C                   GOTO      ENDCON
258400940926     C                   ENDIF
258500940926     C                   ENDIF
258600961112     C* RESO / FRANCO FRONTIERA
258700961112     C     V1CFBR        IFNE      ' '
258800961112     C                   MOVEL     V1CFBR        WFBR
258900961112     C     V1CFBR        IFEQ      'R'
259000961112     C                   MOVEL     'S'           WFBR
259100961112     C                   ENDIF
259200961112     C     ARBFBR        IFNE      WFBR
259300961112     C                   SETON                                            32
259400961112     C                   GOTO      ENDCON
259500961112     C                   ENDIF
259600961112     C                   ENDIF
259700040216      * Tipo bolla
259800040216     c                   if            V1Ccbo <> *blanks
259900040216     c                             and V1Ccbo <> ARBcbo
260000040216     c                   eval      *in32 = *on
260100040216     c                   goto      EndCon
260200040216     c                   endif
260300900515     C* TIPO SPEDIZIONE
260400940715     C     ARBTSP        IFNE      V1CTSP
260500940715     C     V1CTSP        ANDNE     *BLANKS
260600900515     C                   SETON                                            32
260700900515     C                   GOTO      ENDCON
260800900515     C                   END
260900911023     C* SPEDIZIONI GIACENTI
261000980603     C     V1CGIA        IFEQ      'G'
261100930702     C     ARBFBC        ANDNE     'G'
261200911023     C                   SETON                                            32
261300911023     C                   GOTO      ENDCON
261400911023     C                   END
261500980603     C     V1CGIA        IFEQ      'N'
261600930702     C     ARBFBC        ANDEQ     'G'
261700911023     C                   SETON                                            32
261800911023     C                   GOTO      ENDCON
261900911023     C                   END
262000070420     c*
262100070420     c* SPEDIZIONI IN FERMO DEPOSITO
262200070420     C                   IF        v1csfd='S' and arbffd<>'S'
262300070420     C                   SETON                                            32
262400070420     C                   GOTO      ENDCON
262500070420     C                   END
262600070420     c
262700900518     C* SERIE DAL + AL
262800940713     C     ARBNRS        IFLT      V1CPSD
262900900518     C                   SETON                                            32
263000900518     C                   GOTO      ENDCON
263100900518     C                   END
263200940713     C     ARBNRS        IFGT      V1CPSA
263300900515     C                   SETON                                            32
263400900515     C                   GOTO      ENDCON
263500900515     C                   END
263600900518     C* SEGNACOLLO DAL SOLO
263700930702     C     *IN07         IFEQ      *ON
263800970925     C     *IN84         ANDEQ     *OFF
263900940713     C     KART          SETLL     FNART01L                               31
264000930702     C  N31              SETON                                        32
264100930702     C  N31              GOTO      ENDCON
264200930702     C                   END
264300900515     C* SEGNACOLLO DAL + AL
264400970925     C   84              DO
264500910827     C                   SETON                                        32
264600940713     C     KARB2         SETLL     FNART01L
264700940713     C     KARB2         READE     FNART01L                               31
264800910827     C     *IN31         DOWEQ     '0'
264900940713     C     ARTNSC        IFGE      V1CSCD
265000940713     C     ARTNSC        ANDLE     V1CSCA
265100910827     C                   SETON                                        31
265200910827     C                   SETOFF                                       32
265300910827     C                   END
265400910827     C*
265500940713     C  N31KARB2         READE     FNART01L                               31
265600910827     C                   END
265700910827     C*
265800910827     C   32              GOTO      ENDCON
265900910827     C                   END
266000990119     C*
266100990119     C* NUMERO CA
266200990119    1C     V1CDAN        IFNE      ' '
266300040809     C                   SETON                                        32
266400040809      *
266500040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
266600040809      * legate alla spedizione che passo
266700040809     c                   clear                   fidn12ds
266800040809     c                   eval      i12aas = arbAAS
266900040809     c                   eval      i12lnp = arbLNP
267000040809     c                   eval      i12nrs = arbNRS
267100040809     c                   eval      i12nsp = arbNSP
267200040809     c                   eval      i12fel = 'E'
267300040809     c                   eval      i12fan = 'E'
267400040809     c*
267500040809     c                   call      'FIDN12R'
267600040809     c                   parm                    fidn12ds
267700040809     c*
267800040809     c                   setoff                                       31
267900040809     c*
268000040809     c* se non ci sono errori
268100040809     c                   if        o12err <> ' '
268200040809     c                   seton                                        31
268300040809     c                   else
268400040809     c* se numero di CA trovate maggiore di zero
268500040809     c                   if        o12nca = 0
268600040809     c                   seton                                        31
268700040809     c                   else
268800040809     c                   z-add     1             ii                2 0
268900040809     c                   dow       ii <= o12nca and *in31 = *off
269000040809     C* APERTA
269100040809     C     skdch(ii)     IFEQ      0
269200040809     C     V1CDAN        ANDEQ     'A'
269300040809     C                   SETOFF                                       32
269400040809     C                   SETON                                        31
269500040809     C                   ENDIF
269600040809     C* CHIUSE
269700040809     C     skdch(ii)     IFGT      0
269800040809     C     V1CDAN        ANDEQ     'C'
269900040809     C                   SETOFF                                       32
270000040809     C                   SETON                                        31
270100040809     C                   ENDIF
270200040809     C* CA APERTA O CHIUSA
270300040809     C     V1CDAN        IFEQ      'E'
270400040809     C                   SETOFF                                       32
270500040809     C                   SETON                                        31
270600040809     C                   ENDIF
270700040809     c*
270800040809     c                   add       1             ii
270900040809     c                   enddo
271000040809     c*
271100040809     c                   seton                                        31
271200040809     c*
271300040809     c                   endif
271400040809     c                   endif
271500040809     c**
271600990119    3C                   ENDIF
271700030527     C*
271800940317     C     ENDCON        ENDSR
271900940802     C**************************************************************************
272000940802     C* RICHIAMO PGM DI INTERROGAZIONE PARTENZE
272100940802     C**************************************************************************
272200930922     C     INTPAR        BEGSR
272300940802     C*
272400940722     C                   MOVEL     'A'           PA2FLG
272500930922     C                   MOVEL     PARAM2        KPJBU
272600940722     C                   CALL      'FNLS04R'
272700930922     C                   PARM                    KPJBA
272800940802     C*
272900930922     C                   ENDSR
273000131114     C**************************************************************************
273100131114     C* Atre parzializzazioni
273200131114     C**************************************************************************
273300131114     C     AltrePar      BEGSR
273400131114     c     forw12        TAG
273500131114     c                   exfmt     LRI6W12
273600131114     c
273700131114     c                   setoff                                       285690
273800131114     c                   setoff                                       40
273900131114     c
274000131114     C* GIACENTI
274100131114     C* SCELTE INCONGRUENTI
274200131114     C     V1CGIA        IFEQ      'G'
274300131114     C     V1CSSP        IFEQ      '2'
274400131114     C     V1CSSP        OREQ      '3'
274500131114     C                   MOVEL     MSG(14)       w12MSG
274600131114     C                   SETON                                        285690
274700131114     C                   GOTO      forw12
274800131114     C                   END
274900131114     C                   END
275000131114      * Tipo bolla
275100131114if  1c                   if        V1Ccbo <> *blanks
275200131114     c                   eval      *in31  =  *off
275300131114     c     '?'           scan      V1Ccbo                                 31
275400131114if  2c                   if        *in31 = *on
275500131114     c                   call      'X§TABER'
275600131114     c                   parm      CodUt         §KUT
275700131114     c                   parm      '3A'          COD
275800131114     c                   parm      *blanks       KEY
275900131114     c                   parm      *blanks       §DES
276000131114     c                   movel     KEY           V1Ccbo
276100131114     c                   eval      *in90 = *on
276200131114     c                   goto      forw12
276300131114e   2c                   endif
276400131114      *
276500131114     c                   movel     '3A'          COD
276600131114     c                   movel(p)  V1Ccbo        KEY
276700131114     c     KTAB1         chain     TABEL
276800131114if  2c                   if        NOT %found(TABEL00F)
276900131114     c                             or  TBLflg <> *blanks
277000131114     c                   movel     Msg(9)        w12msg
277100131114     c                   seton                                        284090
277200131114     c                   goto      forw12
277300131114e   2c                   endif
277400131114e   1c                   endif
277500131114     c
277600131114     c                   if        v1cgia<>' ' or v1cdan<>' ' or v1caff<>'N'
277700131114     c                             or v1cfbr<>' ' or v1ccbo<>*blanks
277800131114     c                   seton                                        09
277900131114     c                   else
278000131114     c                   setoff                                       09
278100131114     c                   endif
278200131114     c
278300131114     C*
278400131114     C                   ENDSR
278500940802     C**************************************************************************
278600940802     C* EMISSIONE SUBFILE
278700940802     C**************************************************************************
278800900515     C     EMISS         BEGSR
278900940802     C*
279000930407     C                   SETOFF                                       1362
279100940714     C                   MOVEL     *BLANKS       V2CSCE
279200131121     c**                 if        *in87=*off and not *in02
279300131121     C**                 MOVEL     ARBLNP        V2CLIN
279400131121     c**                 endif
279500131121     c**                 if        *in87 or *in02
279600131121     C**                 MOVEL     ARBLNA        V2CLIN
279700131121     c**                 endif
279800940714     C                   MOVEL     ARBLNP        V2CLNP
279900941212     C                   MOVEL     ARBLNA        V2CLNA
280000911023     C                   MOVEL     ARBMGS        MMGG
280100940714     C                   MOVEL     GG            V2CDSP
280200940714     C                   MOVE      MM            V2CDSP
280300940714     C                   MOVE      ARBAAS        V2CAAS
280400950114     C                   MOVEL     ARBNSP        V2CNSP
280500900516     C*
280600060130     C                   MOVEL     ARBksc        V2Cksc
280700060130     C                   MOVEL     ARBcbo        V2Ccbo
280800060130     C                   MOVEL     ARBRSM        V2CRSM
280900940714     C                   MOVEL     ARBRSD        V2CRSD
281000131114     c                   if        v1cind=*blanks
281100131114     c                   eval      v2cdest=arblod
281200131114     c                   else
281300131114     c                   eval      v2cdest=arbind
281400131114     c                   endif
281500131114     c                   if        arbrma<>*blanks
281600131114     c                   eval      v2crifm=%trim(%editc(arbrmn:'Z')) +'/'+
281700131114     c                                     %trim(arbrma)
281800131114     c                   else
281900131114     c                   eval      v2crifm=%trim(%editc(arbrmn:'Z'))
282000131114     c                   endif
282100131114     C**                 Z-ADD     ARBRMN        V2CRMN
282200131114     C**                 MOVEL     ARBRMA        V2CRMA
282300940714     C                   MOVEL     ARBNRS        V2CNRS
282400940923     C                   MOVEL     ARBFLS        V2CFLS
282500940714     C                   MOVEL     *BLANKS       V2CFLA
282600990119     C* SPEDIZIONE IN DANNO
282700990119     C* SE MI HANNO CHIESTO LE CA CHIUSE VISUALIZZO IL FLAG "C"
282800990119     C* SE MI HANNO CHIESTO LE CA APERTE VISUALIZZO IL FLAG "*"
282900990119     C                   SELECT
283000990119     C     V1CDAN        WHENEQ    'C'
283100990119     C                   MOVEL     'C'           V2CDAN
283200990119     C     V1CDAN        WHENEQ    'A'
283300990119     C                   MOVEL     '*'           V2CDAN
283400990119     C**
283500990119     C                   OTHER
283600040809      *
283700040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
283800040809      * legate alla spedizione che passo
283900040809     c                   clear                   fidn12ds
284000040809     c                   eval      i12aas = arbAAS
284100040809     c                   eval      i12lnp = arbLNP
284200040809     c                   eval      i12nrs = arbNRS
284300040809     c                   eval      i12nsp = arbNSP
284400040809     c                   eval      i12fel = 'E'
284500040809     c                   eval      i12fan = 'E'
284600040809     c*
284700040809     c                   call      'FIDN12R'
284800040809     c                   parm                    fidn12ds
284900040809     c*
285000040809     c                   setoff                                       31
285100040809     c*
285200040809     c* se non ci sono errori
285300040809     c                   if        o12err <> ' '
285400040809     c                   seton                                        31
285500040809     c                   else
285600040809     c* se numero di CA trovate maggiore di zero
285700040809     c                   if        o12nca = 0
285800040809     c                   seton                                        31
285900040809     c                   else
286000040809     c                   z-add     1             ii                2 0
286100040809     c                   dow       ii <= o12nca and *in31 = *off
286200040809     C* SE NE TROVO UNA APERTA, DICO APERTA ALTRIMENTI CHIUSA
286300040809     C     skdch(ii)     IFEQ      0
286400040809     C                   MOVEL     '*'           V2CDAN
286500040809     C                   SETON                                            31
286600040809     C                   ELSE
286700040809     C* CHIUSA MA VADO AVANTI A LEGGERE
286800040809     C                   MOVEL     'C'           V2CDAN
286900040809     C                   ENDIF
287000040809     c*
287100040809     c                   add       1             ii
287200040809     c                   enddo
287300040809     c*
287400040809     c                   seton                                        31
287500040809     c*
287600040809     c                   endif
287700040809     c                   endif
287800990119     C**
287900990119     C                   ENDSL
288000921102     C* SEGNACOLLI
288100930702     C                   CLEAR                   SEG
288200921102     C                   MOVEL     ARBNCD        COMSCN            7
288300921102     C                   MOVEA     COMSCN        SEG(1)
288400921102     C     ARBNCA        IFNE      0
288500921102     C                   MOVEL     ARBNCA        COMSCN
288600921102     C                   MOVEA     COMSCN        SEG(9)
288700921102     C                   END
288800921102     C                   MOVEA     '/'           SEG(8)
288900921102     C* ELIMINO ZERO NON SIGNIFICATIVI
289000921102     C                   Z-ADD     1             Y
289100921102     C     SEG(Y)        DOWEQ     '0'
289200921102     C                   MOVE      ' '           SEG(Y)
289300921102     C                   ADD       1             Y
289400921102     C                   END
289500921102     C*
289600921102     C     ARBNCA        IFNE      0
289700921102     C                   Z-ADD     9             Y                 2 0
289800921102     C     SEG(Y)        DOWEQ     '0'
289900921102     C                   MOVE      ' '           SEG(Y)
290000921102     C                   ADD       1             Y
290100921102     C                   END
290200921102     C                   END
290300940714     C                   MOVEA     SEG           V2CSEG
290400911023     C*
290500911023     C* GIACENZA APERTA
290600941202     C                   Z-ADD     ARBAAS        KAAS
290700940713     C                   Z-ADD     ARBLNP        KLNP
290800940713     C                   Z-ADD     ARBNRS        KNRS
290900940713     C                   Z-ADD     ARBNSP        KNSP
291000050405     C     KGCA1         CHAIN     tigcp21l                           31
291100920410     C  N31GCPATB        IFEQ      ' '
291200920410     C     GCPDCG        IFNE      0
291300990119     C                   MOVE      'C'           V2CGCA
291400911023     C                   ELSE
291500990119     C                   MOVE      '*'           V2CGCA
291600900515     C                   END
291700920410     C                   END
291800990119     C                   MOVEL     ARBTSP        V2CTSP
291900930503     C*
292000911023     C* DATA CONSEGNA
292100940714     C                   MOVEL     *BLANKS       V2CCON
292200940714     C                   MOVEL     *BLANKS       V2CFCC
292300930407     C*
292400930407     C* SE ARBCCA >< 0  METTO DECODIFICA AL POSTO DELLA DATA CONSEGNA
292500930407     C     ARBCCA        IFNE      ' '
292600930705     C                   Z-ADD     1             C
292700930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
292800940909     C   31              MOVEL     DCA(C)        DS7O
292900940909     C*
293000940909     C* NELLA DS7O C'E' SCRITTO SE E' PRIORITARIA L'ANOMALIA O LA DATA
293100940909     C   31§7OVCA        IFEQ      'S'
293200940912     C     §7OVCA        ORNE      'S'
293300940912     C     ARBDCM        ANDEQ     *ZERO
293400940909     C                   MOVEL     §7ODEI        V2CCON
293500940909     C                   SETON                                        62
293600940912     C                   ELSE
293700940909     C*
293800940912     C     ARBDCM        IFNE      *ZERO
293900940909     C                   MOVE      ARBDCM        G02INV
294000940909     C                   Z-ADD     *ZEROS        G02DAT
294100940909     C                   MOVEL     '3'           G02ERR
294200940909     C                   CALL      'XSRDA8'
294300940909     C                   PARM                    WLBDAT
294400940909     C                   MOVEL     G02DAT        V2CCON
294500940909     C                   MOVE      G02DAT        $COMOD            2
294600940909     C                   MOVE      $COMOD        V2CCON
294700940909     C                   ENDIF
294800940912     C                   ENDIF
294900930407     C*
295000930407     C                   ELSE
295100940912     C     ARBDCM        IFNE      *ZERO
295200940712     C                   MOVE      ARBDCM        G02INV
295300940712     C                   Z-ADD     *ZEROS        G02DAT
295400940712     C                   MOVEL     '3'           G02ERR
295500940712     C                   CALL      'XSRDA8'
295600940712     C                   PARM                    WLBDAT
295700940714     C                   MOVEL     G02DAT        V2CCON
295800940713     C                   MOVE      G02DAT        $COMOD            2
295900940714     C                   MOVE      $COMOD        V2CCON
296000940912     C                   ENDIF
296100940912     C                   ENDIF
296200940912     C*
296300940912     C     ARBDCM        IFNE      0
296400930428     C*
296500930506     C* SE ARBDCE = 0  CALL PGM PER CALCOLO DELLA DATA CONSEGNA PREVISTA
296600930506     C*   E DELLE ORE DI RITARDO
296700940712      * SOLO SE RICHIESTA L'AFFIDABILITA'
296800940713     C     V1CAFF        IFEQ      'S'
296900930506     C     ARBDCE        IFEQ      0
297000991027     C                   CLEAR                   TNSD99DS
297100930506     C                   MOVEL     'A'           D98TBO
297200930506     C                   MOVEL     ARBAAS        D98AAS
297300930506     C                   MOVEL     ARBLNP        D98LNP
297400930506     C                   MOVEL     ARBNRS        D98NRS
297500930506     C                   MOVEL     ARBNSP        D98NSP
297600930506     C*
297700970925     C                   CALL      'TNSD99R'
297800991027     C                   PARM                    TNSD99DS
297900930506     C*
298000941214     C* SE PA098 = 1  E' STATA FATTA CALL PGM
298100941214     C                   MOVEL     '1'           PA098
298200930506     C*
298300960201     C* SE C'E' ERRORE VISUALIZZO UNA "R"
298400110301     C     D98ERR        IFNE      '0'
298500960201     C                   MOVEL     'R'           V2CFCC
298600960201     C                   ELSE
298700930506     C* SE D98CRC = S  LA SPEDIZ. E' STAT CONSEGNATA NEI TEMPI PREVISTI
298800930506     C     D98CRC        IFEQ      'S'
298900940714     C                   MOVEL     ' '           V2CFCC
299000930506     C                   ELSE
299100940714     C                   MOVEL     'F'           V2CFCC
299200930506     C                   END
299300960201     C                   END
299400930506     C*
299500930506     C                   ELSE
299600930506     C* ARBDCE > 0
299700940711     C* SE ARBNRC > 0  LA SPEDIZIONE NON E' CONFORME
299800940711     C     ARBNRC        IFGT      0
299900940714     C                   MOVEL     'F'           V2CFCC
300000930504     C                   ELSE
300100940714     C                   MOVEL     ' '           V2CFCC
300200930504     C                   END
300300930506     C                   END
300400940713     C                   ENDIF
300500930402     C*
300600911023     C                   ELSE
300700911023     C* SE NUMERO >< 0 IN CONSEGNA
300800911023     C     ARBNDC        IFNE      0
300900940714     C                   MOVEL     'IN CON'      V2CCON
301000911023     C                   SETON                                        13
301100911023     C                   END
301200911023     C                   END
301300921102     C*
301400940714     C     V2CCON        IFEQ      *BLANKS
301500921102     C     ARBFBC        ANDNE     ' '
301600921102     C                   SETON                                        62
301700921102     C*
301800921102     C     ARBFBC        IFEQ      'B'
301900930504     C     ARBFBC        OREQ      '*'
302000940714     C                   MOVEL     'BLOC.'       V2CCON
302100921102     C                   ELSE
302200921102     C     ARBFBC        IFEQ      'A'
302300940714     C                   MOVEL     'L.AVV'       V2CCON
302400921102     C                   ELSE
302500940714     C                   MOVEL     'GIAC.'       V2CCON
302600921102     C                   END
302700921102     C                   END
302800921102     C                   END
302900911023     C*
303000940714     C                   MOVEL     ARBTMC        V2CTMC
303100900522     C*
303200900518     C                   ADD       1             B
303300940713     C                   ADD       1             NR2
303400131107     C                   WRITE     LRI6S02
303500940720     C*
303600900515     C                   ENDSR
303700940802     C**************************************************************************
303800940802     C* EMISSIONE BOLLA
303900940802     C**************************************************************************
304000911023     C     BOLLA         BEGSR
304100940802     C*
304200070411     C                   SETOFF                                       220616
304300070403     C                   SETOFF                                       170377
304400070404     C                   SETOFF                                       262708
304500131127     C                   SETOFF                                       2933
304600940408     C                   SETOFF                                       687069
304700070524     C                   SETOFF                                       850488
304800131120     C                   SETOFF                                       86
304900991027      *
305000940719     C                   Z-ADD     0             V3CLNA
305100940719     C                   Z-ADD     0             V3CDSP
305200930517     C                   Z-ADD     0             VIDRMN
305300940712     C                   MOVE      *BLANKS       VIDRMA
305400991217     C                   CLEAR                   V3CRPP
305500131120     C                   CLEAR                   atrpda
305600131127     C                   CLEAR                   atrtsp
305700961112     C*
305800911022     C* CONTROLLO SE SPEDIZIONE GIACENTE
305900920219     C* GIACENZA APERTA
306000941202     C                   Z-ADD     ARBAAS        KAAS
306100940713     C                   Z-ADD     ARBLNP        KLNP
306200940713     C                   Z-ADD     ARBNRS        KNRS
306300940713     C                   Z-ADD     ARBNSP        KNSP
306400050405     C     KGCA1         CHAIN     tigcp21l                           31
306500040216if  2C  N31GCPATB        IFEQ      ' '
306600920410     C     GCPDCG        IFNE      0
306700920219     C                   SETON                                        03
306800920219     C                   ELSE
306900070403     C                   SETON                                        0327
307000070412     C                   EVAL      V1DFBC=' GIACENTE '
307100040217     C                   ENDIF
307200040217e   2C                   ENDIF
307300930428     C*
307400021213     C*
307500021213     C* 77 ON - Esistono note supermenrcati d ainterrogare
307600021213     C                   Z-ADD     ARBAAS        KAAS
307700021213     C                   Z-ADD     ARBLNP        KLNP
307800021213     C                   Z-ADD     ARBNRS        KNRS
307900021213     C                   Z-ADD     ARBNSP        KNSP
308000021213     C                   MOVEL     'SCR'         KTRD
308100021213     C     KAR51         CHAIN     FIAR5000                           31
308200021217     C  n31              seton                                        77
308300021217     C   31              setoff                                       77
308400920219     C*
308500920207     C* SE BOLLA CON LA "G" SENZA GIACENZA MECCANIZZATA SEGNALO
308600931116     C     ARBFBC        IFEQ      'G'
308700070412     C                   EVAL      V1DFBC=' GIACENTE '
308800070403     C                   SETON                                        27
308900040217     C                   ENDIF
309000930415     C*
309100940907     C                   MOVEL     ARBAAS        V3CAAS
309200940719     C                   MOVE      ARBNSP        V3CNSP
309300940719     C                   MOVEL     ARBNRS        V3CNRS
309400070404     C                   MOVEL     ARBNRS        V4CNRS
309500131108     C********           MOVEL     ARBNRS        V7CNRS
309600940719     C                   MOVEL     ARBLNP        V3CLNP
309700950508     C                   MOVEL     ARBLNP        V3CLPA
309800131114     c
309900131114     c                   z-add     arbmgs        arbdsp
310000131114     c                   movel     arbaas        arbdsp
310100940908     C*
310200940908     C*  L E G A M I    B O L L A
310300940908     C  N75              EXSR      LEGBOL
310400940908     C*
310500911022     C* NON SEQUENZIALI
310600911023     C     ARBFNS        IFEQ      'S'
310700930705     C                   MOVEL     CNOSEQ        DESFNS
310800911022     C                   ELSE
310900911022     C                   MOVEL     *BLANKS       DESFNS
311000040217     C                   ENDIF
311100930525     C*
311200930525     C* 95 ON - ESITONO EVENTI DA INTERROGARE
311300940713     C                   Z-ADD     ARBAAS        KAAS$
311400940713     C                   Z-ADD     ARBLNP        KLNP
311500940713     C                   Z-ADD     ARBNRS        KNRS
311600940713     C                   Z-ADD     ARBNSP        KNSP
311700990805     C     KEVB1         SETLL     FNEVB01L                               95
311800920402     C*
311900920403     C* 98 ON - ESITONO VARIAZIONI FATTE
312000131007     C     KARB2         SETLL     FiARBF2C                               98
312100990119     C* 04 ON - ESISTONO CA DA VISUALIZZARE
312200990119     C                   CLEAR                   CONTCA
312300040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
312400040809      * legate alla spedizione che passo
312500040809     c                   clear                   fidn12ds
312600040809     c                   eval      i12aas = arbAAS
312700040809     c                   eval      i12lnp = arbLNP
312800040809     c                   eval      i12nrs = arbNRS
312900040809     c                   eval      i12nsp = arbNSP
313000040809     c                   eval      i12fel = 'E'
313100040809     c                   eval      i12fan = 'E'
313200040809     c*
313300040809     c                   call      'FIDN12R'
313400040809     c                   parm                    fidn12ds
313500040809     c*
313600040809     c                   setoff                                       31
313700040809     c*
313800040809     c* se non ci sono errori
313900040809     c                   if        o12err <> ' '
314000040809     c                   seton                                        31
314100040809     c                   else
314200040809     c* se numero di CA trovate maggiore di zero
314300040809     c                   if        o12nca = 0
314400040809     c                   seton                                        31
314500040809     c                   else
314600040809     c                   z-add     1             ii                2 0
314700040809     c                   dow       ii <= o12nca and *in31 = *off
314800040809     c                   movea     skkey(ii)     dskey
314900040809     C                   SETON                                        04
315000040809     C                   ADD       1             CONTCA            3 0
315100040809     C                   Z-ADD     dsaac         VHCAAC
315200040809     C                   Z-ADD     dsfil         VHCFIL
315300040809     C                   Z-ADD     dsnca         VHCNCA
315400040809     c*
315500040809     c                   add       1             ii
315600040809     c                   enddo
315700040809     c*
315800040809     c                   seton                                        31
315900040809     c*
316000040809     c                   endif
316100040809     c                   endif
316200040809     c**
316300070406     c* se Presente FIARP, imposto la cons.richista proposta
316400070404     c                   clear                   v4cdimpr
316500070404     c                   clear                   v4coimpr
316600070404     c                   clear                   v4cprupr
316700070416     c                   clear                   v08dcrpr
316800070404     c                   clear                   v4ctcrpr
316900070404     c                   clear                   v4chcrpr
317000070406     c
317100070406     c* Solo se visualizzabile
317200070406     c     karb2         chain     fiarp01l
317300070406    1c                   if        %found(FIarp01l)
317400070404     c                   seton                                        26
317500070404     C                   MOVEL     arppru        v4cprupr
317600070404     C                   MOVEL     arpoim        v4coimpr
317700070404    2c                   if        arpdim>0
317800070404     c     *iso          movel     arpdim        dataeur
317900070416     c                   movel     dataeur       datadmy
318000070416     c                   movel     datadmy       w0060             6 0
318100070416     c                   movel     w0060         v4cdimpr
318200070404    2c                   endif
318300070404    2c                   if        arpdcr>0
318400070404     c     *iso          movel     arpdcr        dataeur
318500070416     c                   movel     dataeur       v08dcrpr
318600070416     c**                 movel     datadmy       v06dcrpr
318700070404    2c                   endif
318800070404     c                   movel     arphcr        v4chcrpr
318900070404     c                   movel     arptcr        v4ctcrpr
319000070406     c                   if        arptcr=' '
319100070406     c                   eval      v4ctcrpr='IL'
319200070404    1c                   endif
319300070406    1c                   endif
319400070406     c
319500911022     C* TIPO BOLLA CONTROLLO SE FRANCO
319600940719     C                   MOVEL     ARBCBO        V3CCBO
319700911022     C                   MOVEL     '3A'          COD
319800911022     C                   MOVEL     *BLANKS       KEY
319900940712     C                   MOVEL     ARBCBO        KEY
320000940713     C     KTAB1         CHAIN     TABEL                              31
320100911022     C                   MOVEL     TBLUNI        DS3A
320200911022     C                   MOVEL     §3ATB1        FLGTB1            1
320300940719     C                   MOVEL     §3ADES        V3DCBO
320400940719     C                   MOVEL     §3ADES        V5DCBO
320500911030     C* FRANCO
320600040216if  1C     FLGTB1        IFEQ      'F'
320700911022     C                   SETON                                        33
320800940713     C                   MOVE      ARBKSC        V3CKSC
320900940712     C                   MOVE      *BLANKS       VIDKSC
321000040216x   1C                   ELSE
321100911030     C* ASSEGNATO
321200911030     C     ARBCCM        IFNE      0
321300940713     C                   MOVE      ARBCCM        V3CKSC
321400911030     C                   ELSE
321500940713     C                   MOVE      *ZEROS        V3CKSC
321600040217     C                   ENDIF
321700911030     C                   MOVEL     ARBKSC        VIDKSC
321800040217e   1C                   ENDIF
321900070410     c* Numero di telefono del mittente
322000070410     c                   clear                   v3ctelm
322100070410     c                   move      v3cksc        w0040
322200070410     c                   if        w0040<>0000 and w0040<>8888
322300070410     c                             and w0040<>9999
322400070410     c                   eval      i69kin = V3Cksc
322500070410     c                   exsr      CTR_ANAGRA
322600070410     c                   eval      V3ctelm= indtel
322700070410     c                   endif
322800991027     C* 38 ON - ESISTE TB2 ANCHE SENZA TIPO RCD "2" IN FIAR600F
322900920121     C     §3ATB2        COMP      '  '                               3838
323000040218      *
323100040218      *?Controllo se esitono reclami?
323200131111     c                   eval      wARBlnp = ARBlnp
323300131111     c                   eval      wARBnrs = ARBnrs
323400131111     c                   eval      wARBnsp = ARBnsp
323500131111     c                   eval      wARBaas = ARBaas
323600040218     c                   eval      TGDtor  = 'S'
323700040218     c                   eval      TGDogg  = ds_OggSped
323800040218     c     K02TGD02      setll     FITGD000
323900040302     c                   eval      *in64   = (%equal(FITGD02L)   and
324000040303     c                                           (SDSprm =  1
324100040303     c                                        or (SDSprm >  1
324200040303     c                                        and IA1ins <> 'N')))
324300940712     C*
324400940712     C* DECODIFICO  "BOLLA DI RIENTRO"
324500961111     C                   MOVEL     *BLANKS       VIDFBR
324600961111     C                   MOVEL     *BLANKS       VIDFFR
324700961111     C                   SETOFF                                       1073
324800940712     C     ARBFBR        IFEQ      'S'
324900961111     C                   MOVEL     'SI'          VIDFBR
325000961111     C                   SETON                                        10
325100040217     C                   ENDIF
325200961111     C     ARBFBR        IFEQ      'F'
325300961111     C                   MOVEL     'FR'          VIDFFR
325400961111     C                   SETON                                        73
325500040217     C                   ENDIF
325600970130     C*
325700970227     C* FLAG CONSEGNA DDT
325800970130     C                   MOVEL     ARBAAS        KAAS
325900970130     C                   MOVEL     ARBLNP        KLNP
326000970130     C                   MOVEL     ARBNRS        KNRS
326100970130     C                   MOVEL     ARBNSP        KNSP
326200970130     C                   MOVEL     'A'           KTRC
326300970131     C                   CLEAR                   V1DBAM
326400000615     C                   eval      v3csop = ' '
326500060216     C     KAR41         CHAIN     FiAR401L                           31
326600970227     C* C'E' IL RECORD
326700040216if  1C     *IN31         IFEQ      *OFF
326800000615     C                   movel     ar4not        dsbl4a
326900000615     C                   eval      v3csop = §b4aso
327000031027     c                   Move      §b4Abm        wbam              1
327100040216if  2C     WBAM          IFEQ      'S'
327200970131     C     WBAM          OREQ      'C'
327300031027     c     wbam          oreq      'P'
327400031027     c     wbam          oreq      'Q'
327500970227     C                   SETON                                        76
327600970227     C                   MOVEL     'SI DDT'      V1DBAM
327700040216x   2C                   ELSE
327800970227     C                   SETOFF                                       76
327900970227     C                   MOVEL     'NO DDT'      V1DBAM
328000040216e   2C                   ENDIF
328100970227     C**
328200040216sel 2C                   SELECT
328300970227     C     WBAM          WHENEQ    'C'
328400970227     C     WBAM          OREQ      'K'
328500970227     C                   MOVEL     'CLI'         V1DFST
328600970227     C     WBAM          WHENEQ    'J'
328700031027     C     WBAM          oreq      'P'
328800031027     C     WBAM          oreq      'Q'
328900031113     C     WBAM          OREQ      'W'
329000070406     C                   MOVEL     'Arr'         V1DFST
329100970227     C     WBAM          WHENEQ    'S'
329200970227     C     WBAM          OREQ      'Y'
329300070406     C                   MOVEL     'Par'         V1DFST
329400040216x   2C                   OTHER
329500970227     C                   MOVEL     'NO '         V1DFST
329600040216e   2C                   ENDSL
329700970227     C**
329800970227     C* NON  C'E' IL RECORD
329900040216x   1C                   ELSE
330000970227     C                   SETON                                        76
330100970227     C                   MOVEL     'SI DDT'      V1DBAM
330200970227     C                   MOVEL     'PAR'         V1DFST
330300040216e   1C                   ENDIF
330400010510     c* numero O.R.M.
330500010510     C                   MOVEL     'M'           KTRC
330600060216     c     kar41         chain     fiar401l                           31
330700010510     c     *in31         ifeq      *off
330800010510     c                   movel     ar4not        dsbl4m
330900010510     c                   z-add     §b4poe        v3cpoe
331000010510     c                   z-add     §b4nsr        v3cnsr
331100010510     c                   z-add     §b4nor        v3cnor
331200010510     c                   z-add     §b4nrv        v3cnrv
331300010510     c                   else
331400010510     c                   clear                   v3cpoe
331500010510     c                   clear                   v3cnsr
331600010510     c                   clear                   v3cnor
331700010510     c                   clear                   v3cnrv
331800010510     c                   endif
331900130320     c*//* Verifico se ha fiar8
332000130320     C*//                MOVEL     ARBAAS        KAAS
332100130320     C*//                MOVEL     ARBLNP        KLNP
332200130320     C*//                MOVEL     ARBNRS        KNRS
332300130320     C*//                MOVEL     ARBNSP        KNSP
332400130320     c*//  ksped         setll     fiar801l
332500130320     c*//                if        %equal(fiar801l)
332600130320     c*//                eval      v1dAR8='PACKING LIST'
332700130320     C*//                ELSE
332800130320     C*//                CLEAR                   V1DAR8
332900130320     c*//                endif
333000941219     C*
333100941219     C* DECODIFICO  "CONSEGNA A MAGAZZINO"
333200941219     C     ARBFDN        IFEQ      'S'
333300941219     C                   MOVEL     'SI'          V3CFDN
333400941219     C                   ELSE
333500941219     C                   MOVEL     *BLANKS       V3CFDN
333600941219     C                   ENDIF
333700911129     C*
333800920204     C* INCASSO C/A E ASSEGNATO
333900930113     C                   MOVEL     '  '          VIDICC
334000930113     C                   MOVEL     '  '          VIDICA
334100930113     C*
334200930113     C* SE ASSEGNATO O FRANCO CON SECONDA BOLLA
334300930113     C     *IN33         IFEQ      '0'
334400930113     C     *IN33         OREQ      '1'
334500930113     C     *IN38         ANDEQ     '1'
334600930113     C*
334700930330     C     ARBICA        IFNE      ' '
334800941216     C     ARBICA        ANDNE     'R'
334900920204     C                   MOVEL     'SI'          VIDICA
335000040217     C                   ENDIF
335100040217     C                   ENDIF
335200911129     C*
335300911030     C* LINEA PARTENZA DECODIFICA
335400020225     c                   clear                   og143
335500930706     C                   Z-ADD     ARBLNP        NUM3
335600930706     C     NUM3          CHAIN     AZORG01L                           31
335700940719     C  N31              MOVEL     ORGDES        V3DLNP
335800940719     C   31              MOVEL     *BLANKS       V3DLNP
335900020225     c                   eval      og143 = orgde3
336000020225     c                   eval      lnpntw = §ogntw
336100911030     C* LINEA ARRIVO   DECODIFICA
336200991217     c                   clear                   og143
336300070403     C     arblna        CHAIN     AZORG01L                           31
336400940719     C  N31              MOVEL     ORGDES        V3DLNA
336500940719     C   31              MOVEL     *BLANKS       V3DLNA
336600070403     c                   movel     v3dlna        v7dlna
336700991217     C                   movel     orgde3        og143
336800070403     C                   movel     orgde8        og148
336900020304     c                   eval      lnantw = §ogntw
337000070426     c* p.o. gestore di arblna
337100070426     c                   clear                   fnlv55ds
337200070426     c                   movel     arblna        d55lin
337300070426     c                   movel     '6'           d55tpt
337400070426     C                   time                    wrkoggiiso
337500070426     C     *iso          move      wrkoggiiso    d55drf
337600070426     c                   call      'FNLV55R'
337700070426     c                   parm                    fnlv55ds
337800070426     c
337900070426     c                   if        d55tfa<>arblna
338000070426     C     d55tfa        CHAIN     AZORG01L                           31
338100070426     C  n31              movel     orgde8        og148
338200070426     C   31              clear                   og148
338300070426     c                   endif
338400070426     c
338500070403     c
338600070403     c* Abilito visualizzazione nuove funzioni se GEO attiva
338700070426     c* Per p.o. gestione linea di arrivo e p.o. utente
338800070403     c                   if        §oggeot='S' and dutgeot='S'
338900070524     c                   seton                                        1788
339000070406     c                   else
339100070406     c* non visualizzo nemmeno fiarp
339200070406     c                   setoff                                       26
339300070403     c                   endif
339400070524     c
339500070524     c* Vedo se almeno è attivo lo scambio dati con tellus
339600070524     c                   if        not *in88 and §ogtlls<>' ' and dutgeot='S'
339700070524     c                   seton                                        88
339800070524     c                   endif
339900070404     c
340000070404     c* Emetto riga sintetica altre abilitazioni
340100070521     c                   clear                   v5dngd
340200140109     c***                if        *in17
340300070404     c                   clear                   v3cges
340400070404     C                   MOVEL     *BLANKS       V3cgesd
340500070404     c*
340600131120     c                   clear                   fnlrg7ds
340700131120     c                   eval      ilrg7aas=arbaas
340800131120     c                   eval      ilrg7lnp=arblnp
340900131120     c                   eval      ilrg7nrs=arbnrs
341000131120     c                   eval      ilrg7nsp=arbnsp
341100131120     c                   eval      ilrg7lung=58
341200131120     c                   call      'FNLRG7R'
341300131120     c                   parm                    fnlrg7ds
341400131120     c                   movel     olrg7vssin    v3cges
341500140109     c***                endif
341600070410     c
341700070524     c* Reperisco dati della distinta automatica tramite FIDG34R
341800070524     c                   if        *in88
341900070410     c                   clear                   v5ccgi
342000070411     c                   clear                   v5dcgi
342100070410     c                   clear                   v5dtgi
342200070410     c                   clear                   v5dttl
342300070411     c                   clear                   v5clno
342400070427     c                   clear                   v5dlno
342500070427     c                   clear                   v5cargerr
342600070427     c                   clear                   v5dargerr
342700070427     c
342800070427     c                   clear                   fidg34ds
342900070427     c                   eval      id34tip='2'
343000070427     c                   eval      id34aas=arbaas
343100070427     c                   eval      id34lnp=arblnp
343200070427     c                   eval      id34nrs=arbnrs
343300070427     c                   eval      id34nsp=arbnsp
343400070427     c                   call      'FIDG34R'
343500070427     c                   parm                    FIDG34DS
343600070410     c
343700070427     c                   movel     od34cgi       v5ccgi
343800070410    3c                   select
343900070427     c                   when      od34tgi='A'
344000070410     c                   eval      v5dtgi='Automatico'
344100070427     c                   when      od34tgi='M'
344200070410     c                   eval      v5dtgi='Manuale   '
344300070410    3c                   ENDSL
344400070411     c* Decodifica giro
344500070427     c                   eval      v5dcgi=od34cgides
344600070427     c* esito/errore
344700070427     c                   movel     od34err       v5cargerr
344800070427     c                   eval      v5dargerr=od34errdes
344900070427     c* livello normalizzazione
345000070427     c                   movel     od34lno       v5clno
345100070427     c                   eval      v5dlno=od34lnodes
345200070427     c* elaborazione tellus
345300070427     c***                eval      v5dttl=od34ttldes
345400070427     c                   select
345500070427     c                   when      od34ttl='1'
345600070427     c                   eval      v5dttl='SI- CON Giro'
345700070427     c                   when      od34ttl='2'
345800070427     c                   eval      v5dttl='SI-SenzaGiro'
345900070427     c                   endsl
346000070521     c*
346100070521     c* Decodifico se assegnazione automatica o manuale
346200070521     c                   if        arbndc>0 and arbngd<>0
346300070521     c                   eval      v5dngd='MANUALE'
346400070521     c                   endif
346500070521     c                   if        arbndc>0 and arbngd=0
346600070521     c                   eval      v5dngd='AUTOMAT'
346700070521     c                   endif
346800070410     c
346900070410    1c                   endif
347000991217     c
347100060518     c* LNA DPD o lnp dpd: leggo record "I" di ar4
347200000216     c                   setoff                                       08
347300000216     c                   clear                   v3czon
347400000216     c                   clear                   v3crtn
347500040216if  1c                   if        lnantw = 'DPD'
347600991217     c                   seton                                        85
347700991217     C                   MOVEL     ARBAAS        KAAS
347800991217     C                   MOVEL     ARBLNP        KLNP
347900991217     C                   MOVEL     ARBNRS        KNRS
348000991217     C                   MOVEL     ARBNSP        KNSP
348100060518     C                   MOVEL     'I'           KTRC
348200060216     C     KAR41         CHAIN     FiAR401L                           31
348300060518     C  N31              MOVEL     AR4NOT        dsbl4i
348400060518     c   31              clear                   dsbl4i
348500060518     C                   MOVEL(P)  §b4ipn        V3CRPP
348600060518     c                   if        §b4ida<>*blanks or §b4iza>*zeros
348700060518     c                   movel     §b4ida        v3crtn
348800060518     c                   movel     §b4iza        v3czon
348900000216     c                   seton                                        08
349000000216     c                   endif
349100050304     c                   else
349200050304if  1c                   if        lnpntw = 'DPD'
349300050304     C                   MOVEL     ARBAAS        KAAS
349400050304     C                   MOVEL     ARBLNP        KLNP
349500050304     C                   MOVEL     ARBNRS        KNRS
349600050304     C                   MOVEL     ARBNSP        KNSP
349700060518     C                   MOVEL     'I'           KTRC
349800060518     c                   clear                   dsbl4i
349900060216     C     KAR41         CHAIN     FiAR401L
350000060216     c                   if        %found(fiar401l)
350100060518     C**                 MOVEL     AR4NOT        dsbl4f
350200060518     C**                 MOVEL     §b4fnp        V3CRPP
350300060518     C                   MOVEL     AR4NOT        dsbl4i
350400060518     C                   MOVEL     §b4ipn        V3CRPP
350500050304     c                   seton                                        85
350600050304     c                   endif
350700050304     c                   endif
350800040216e   1c                   endif
350900971219     C*
351000971219     C* 16 ON  - SI TRATTA DI BOLLA EXPORT
351100020225     c                   if        lnantw = 'EEX' or lnantw = 'EUP' or
351200060201     c                             lnantw = 'FED' or lnantw='DPD'
351300971219     C                   SETON                                        16
351400971219    1C                   ENDIF
351500940719     C                   MOVEL     V3DLNA        D15LNA
351600920804     C*
351700940719     C                   MOVEL     ARBLNA        V3CLNA
351800940719     C                   MOVEL     ARBRSM        V3CRSM
351900070410     C                   MOVEL     ARBRSM        V4CRSM
352000940713     C                   MOVEL     ARBRSM        V28RSM
352100940719     C                   MOVEL     ARBINM        V3CINM
352200940719     C                   MOVEL     ARBPRM        V3CPRM
352300940719     C                   MOVEL     ARBCAM        V3CCAM
352400940719     C                   MOVEL     ARBLOM        V3CLOM
352500940713     C                   MOVEL     ARBLOM        V25LOM
352600940719     C                   MOVEL     ARBFAP        V3CFAP
352700950421     C                   MOVEL     ARBNZM        V3CNZM
352800131108     c                   if        arbnzm<>*blaNKS
352900131108     C                   MOVEL     arbnzm        v7cprm
353000131108     c                   else
353100131108     C                   MOVEL(P)  arbprm        v7cprm
353200131108     c                   endif
353300131108     c
353400050113     C                   MOVEL     ARBXCO        VIDXCO
353500130221     c                   if        vidxco >*zeros  and no_f22=*blanks
353600110928     c                   seton                                        86
353700110928     c                   endif
353800940719     C                   MOVEL     *BLANKS       V3CPIM
353900940719     C                   MOVEL     *BLANKS       V4CPID
354000061108     c                   clear                   v3cdfm
354100061108     c                   clear                   v4cdfd
354200950411     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
354300060216     C* DA FiAR4 SE RECORD ESISTENTE
354400950411     C                   MOVEL     ARBAAS        KAAS
354500950411     C                   MOVEL     ARBLNP        KLNP
354600950411     C                   MOVEL     ARBNRS        KNRS
354700950411     C                   MOVEL     ARBNSP        KNSP
354800950411     C                   MOVEL     'P'           KTRC
354900060216     C     KAR41         CHAIN     FiAR401L                           31
355000040216if  1C     *IN31         IFEQ      *OFF
355100950411     C                   MOVEL     AR4NOT        V3CPIM
355200950411     C                   MOVE      AR4NOT        V4CPID
355300040216x   1C                   ELSE
355400950411     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI ARBCPI
355500040216if  2C     ARBCPI        IFNE      *BLANKS
355600950411     C  N33              MOVEL     ARBCPI        V4CPID
355700950411     C   33§3ATB2        IFEQ      '  '
355800950411     C                   MOVEL     ARBCPI        V3CPIM
355900950411     C                   ELSE
356000950411     C                   MOVEL     ARBCPI        V4CPID
356100040217     C                   ENDIF
356200040217e   2C                   ENDIF
356300040217e   1C                   ENDIF
356400061108      * valorizzo il codice fiscale del mittente e del destinatario
356500061108      * FIAR4 rcd Q
356600061108     c                   eval      ktrc = 'Q'
356700061108     c     kar41         chain     fiar401l
356800061108if  1c                   if        %found(fiar401l)
356900061108     c                   movel     ar4not        v3cdfm
357000061108     c                   move      ar4not        v4cdfd
357100061108x   1c                   endif
357200980704     C* VISUALIZZO SE C'E' IL RIFERIM PARTNER
357300971219     C                   CLEAR                   V3CEST
357400971219     C                   MOVEL     ARBAAS        KAAS
357500971219     C                   MOVEL     ARBLNP        KLNP
357600971219     C                   MOVEL     ARBNRS        KNRS
357700971219     C                   MOVEL     ARBNSP        KNSP
357800971219     C                   MOVEL     'E'           KTRC
357900060216     C     KAR41         CHAIN     FiAR401L                           31
358000980704     C* VISUALIZZO IN UNA BOLLA ITALIA SE C'E'
358100980704     C* VISUALIZZO SEMPRE IN UNA BOLLA IMPORT/EXPORT
358200971219     C  N31              MOVEL     AR4NOT        V3CRPP
358300980704     C  N31              SETON                                        85
358400980704     C**
358500020225     c                   if        *in31 and
358600020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
358700020225     c                              lnpntw = 'FED') or
358800020225     c                             (lnantw = 'EEX' or lnantw = 'EUP' or
358900020225     c                              lnantw = 'FED')
359000980704     C                   SETON                                        85
359100980704     C                   ENDIF
359200980704     C**
359300940719     C                   MOVEL     ARBRSD        V4CRSD
359400131108     C**********         MOVEL     ARBRSD        V30RSD
359500940713     C                   MOVEL     ARBRSD        V32RSD
359600940719     C                   MOVEL     ARBIND        V4CIND
359700131119     C                   MOVEL     ARBIND        V33IND
359800940719     C                   MOVEL     ARBPRD        V4CPRD
359900940719     C                   MOVEL     ARBCAD        V4CCAD
360000940719     C                   MOVEL     ARBLOD        V4CLOD
360100131119     C*****              MOVEL     ARBLOD        V24LOD
360200940719     C                   MOVEL     ARBFIN        V4CFIN
360300940719     C                   MOVEL     ARBNZD        V4CNZD
360400911022     C                   MOVEL     ARBGC1        VIDGC1
360500911022     C                   MOVEL     ARBGC2        VIDGC2
360600911022     C                   MOVEL     ARBCTR        VIDCTR
360700940719     C                   MOVEL     ARBCTS        V6CCTS
360800131108     c                   if        arbnzd<>*blaNKS
360900131108     C                   MOVEL     arbnzd        v7cprd
361000131108     c                   else
361100131108     C                   MOVEL(P)  arbprd        v7cprd
361200131108     c                   endif
361300940719     C* DECODIFICA CODICE TASSAZIONE
361400940719     C                   MOVEL     'CT'          COD
361500940719     C                   MOVEL     *BLANKS       KEY
361600940719     C                   MOVEL     ARBCTS        KEY
361700940719     C     KTAB1         CHAIN     TABEL00F                           31
361800940719     C                   MOVEL     TBLUNI        V6DCTS
361900911022     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
362000911022     C                   MOVEL     'TR'          COD
362100911022     C                   MOVEL     *BLANKS       KEY
362200911022     C                   MOVEL     VIDCTR        UNOCTR            1
362300911022     C                   MOVEL     UNOCTR        KEY
362400940713     C     KTAB1         CHAIN     TABEL                              31
362500911022     C                   MOVEL     TBLUNI        DEFCTR
362600070404     C*****              MOVEL     ARBFTM        V3CFTM
362700070404     c                   if        arbftm='S'
362800070404     c                   eval      v3dftm='TASSATA CON TARIFFA MITTENTE'
362900070404     c                   endif
363000940711     C* DECODIFICA TIPO SERVIZIO BOLLE
363100090604     c                   clear                   ds5e
363200940711     C                   MOVEL     '5E'          COD
363300940711     C                   MOVEL     *BLANKS       KEY
363400940713     C                   MOVEL     ARBTSP        KEY
363500941024     C     KTAB1         CHAIN     TABEL00F                           31
363600090604     C  N31              MOVEL     TBLUNI        ds5e
363700090604     C  N31              MOVEL     §5ed08        V1DTSP
363800940719     C   31              MOVEL     *BLANKS       V1DTSP
363900131127     c                   if        §5eftc<>' '
364000131127     c                   movel     ri            atrtsp
364100131127     c                   else
364200131127     c                   movel     hi            atrtsp
364300131127     c                   endif
364400940713     C                   MOVEL     ARBVMD        VIDVMD
364500940713     C                   MOVEL     ARBVAD        VIDVAD
364600940713     C                   MOVEL     ARBIAS        VIDIAS
364700940713     C                   MOVEL     ARBVAS        VIDVAS
364800911022     C                   MOVEL     ARBNAS        VIDNAS
364900950214     C                   CLEAR                   VIDFLS
365000950214     C     ARBNCD        IFNE      *ZERO
365100950214     C     ARBFNS        ORNE      *BLANKS
365200940923     C                   MOVEL     ARBFLS        VIDFLS
365300950214     C                   ENDIF
365400950214     C                   Z-ADD     ARBFLS        V3CFLS
365500911022     C                   MOVEL     ARBNCL        VIDNCL
365600070410     C                   MOVEL     ARBPKB        VIDPKb
365700070410     C                   MOVEL     ARBPKF        VIDPKF
365800070410     C                   MOVEL     ARBVLb        VIDVLb
365900070410     C                   MOVEL     ARBFVb        VIDFVb
366000070410     C                   MOVEL     ARBVLF        VIDVLF
366100070410     C                   MOVEL     ARBFVF        VIDFVF
366200930705     C     ARBNCR        IFLT      99998
366300940711     C                   MOVEL     ARBVLC        VIDVLC
366400940711     C                   MOVEL     ARBNCR        VIDNCR
366500921231     C                   ELSE
366600990909     C                   Z-ADD     0             VIDVLC
366700990909     C                   Z-ADD     0             VIDNCR
366800921231     C                   END
366900990909     C     ARBNCP        IFLT      99998
367000990909     C                   MOVEL     ARBPKC        VIDPKC
367100990909     C                   MOVEL     ARBNCP        VIDNCP
367200990909     C                   ELSE
367300990909     C                   Z-ADD     0             VIDPKC
367400990909     C                   Z-ADD     0             VIDNCP
367500990909     C                   END
367600940711     C                   MOVEL     ARBQFT        VIDQFT
367700031119
367800031119      * Referente e Telefono del destinatario
367900031119     c                   Clear                   V4cRef
368000031119     c                   Clear                   V4cTeld
368100091214     c                   clear                   w_cdi
368200031119      * li prendo da Fiar500f record 'GEN'
368300130320     c                   clear                   v1dAR8l
368400130320     c                   clear                   v1dAR8
368500031119     c                   Clear                   dAr5Gen
368600031119     c                   Z-Add     ArbAas        Kaas
368700031119     c                   Z-Add     ArbLNp        Klnp
368800031119     c                   Z-Add     ArbNrs        Knrs
368900031119     c                   Z-Add     ArbNsp        Knsp
369000031119     c                   Eval      Ktrd = 'GEN'
369100031119     c     kAr51         Chain     Fiar501l
369200031119If  1c                   If        %Found(Fiar501l)
369300031119     c                   Movel     Ar5Uni        Dar5Gen
369400031119     c                   Eval      V4cRef = §Ar5Ref
369500031119     c                   Eval      V4cTeld = §Ar5Teld
369600091214     c* memorizzo eventuale codice beneficiario assegno
369700091214     c                   eval      w_cdi=§ar5cdi
369800130320     c* verifico se LDV con packing-list e/o PDF
369900130320sel 2c                   select
370000130320w   2c                   when      §AR5als = 'S'
370100130320     c                   eval      v1dAR8l = 'PACKING LIST PDF'
370200130320     c                   eval      v1dAR8  = 'PACKING PDF '
370300130320w   2c                   when      §AR5alx = 'S'
370400130320     c                   eval      v1dAR8l = 'PACKING LIST'
370500130320     c                   eval      v1dAR8  = 'PACKING LIST'
370600130320e   2c                   endsl
370700031119    1c                   EndIf
370800040301
370900040301      * Se colli di valore visualizzo "V" nell'apposito campo
371000040301      *  con lo specifico attributo di visualizzazione
371100040301if  1c                   if        §AR5bva = *blanks
371200040301     c                   eval      VIDbva  = *blanks
371300040301x   1c                   else
371400040301     c                   eval      VIDbva  = 'V'
371500040301e   1c                   endif
371600110601
371700110610     c* email destinatario
371800110601     c                   setoff                                       60
371900110601     c                   clear                   v4cemw
372000110610     c                   clear                   v4cemd
372100130927     c                   Clear                   dAr5emd
372200110601      * li prendo da Fiar500f record 'EMD'
372300110601     c                   Z-Add     ArbAas        Kaas
372400110601     c                   Z-Add     ArbLNp        Klnp
372500110601     c                   Z-Add     ArbNrs        Knrs
372600110601     c                   Z-Add     ArbNsp        Knsp
372700110601     c                   Eval      Ktrd = 'EMD'
372800130927     c***  kAr51         setll     Fiar501l
372900130927     c     kAr51         chain     Fiar501l
373000130927If  1c                   If        %found(Fiar501l)
373100130927     c                   eval      dar5emd=ar5uni
373200130927     c                   if        §ar5tel<>*blanks or §ar5eml<>*blanks
373300110610     c                   Eval      V4cemd = 'SI'
373400110601     c                   seton                                        60
373500130927     c                   endif
373600110601    1c                   EndIf
373700040301
373800030121      * Bancali
373900030121     c                   Clear                   VidTba
374000030121     c                   Clear                   VidTb2
374100030122     c                   Clear                   VidNba
374200030122     c                   Clear                   VidNb2
374300030703      * Record 'BAN'
374400030703If  0c                   If        %subst(ArbGva:1:1) = 'B'
374500030121     c                   Clear                   dAr5Ban
374600030121     c                   Eval      Ktrd = 'BAN'
374700030121     c     kAr51         Chain     Fiar501l
374800030121If  1c                   If        %Found(Fiar501l)
374900030121     c                   Movel     Ar5Uni        Dar5Ban
375000030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
375100030122      * per le note relative al mancato reso dei bancali
375200030122     c                   If        Not *In77 and
375300030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
375400030122     c                   Eval      *In77 = *On
375500030122     c                   EndIf
375600030121     c                   Eval      VidNba = §Ar5Nba
375700030121     c                   Eval      VidNb2 = §Ar5Nb2
375800030121If  2c                   If        §Ar5Tba <> *Zeros
375900030703     c                   Eval      wtba = §Ar5Tba
376000030703     c                   ExSr      Sr_DecTba
376100030703     c                   Eval      VidTba = wdestba
376200030121    2c                   EndIf
376300030121If  2c                   If        §Ar5Tb2 <> *Zeros
376400030703     c                   Eval      wtba = §Ar5Tb2
376500030703     c                   ExSr      Sr_DecTba
376600030703     c                   Eval      VidTb2 = wdestba
376700030121    2c                   EndIf
376800030121    1c                   EndIf
376900030703    0c                   EndIf
377000030703      * Record 'BNB'  -  Spedizione OVER 25 colli
377100030703If  0c                   If        %subst(ArbGva:1:1) = 'O'
377200030703     c                   Clear                   dAr5Bnb
377300030703     c                   Eval      Ktrd = 'BNB'
377400030703     c     kAr51         Chain     Fiar501l
377500030703If  1c                   If        %Found(Fiar501l)
377600030703     c                   Movel     Ar5Uni        Dar5Bnb
377700030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
377800030703      * per i colli originali in caso di spedizione over 25 colli
377900030703     c  n77              Eval      *In77 = *On
378000030703     c                   Eval      VidNba = §Ar5bNba
378100030703     c                   Eval      VidNb2 = §Ar5bNb2
378200030703If  2c                   If        §Ar5bTba <> *Zeros
378300030703     c                   Eval      wtba = §Ar5bTba
378400030703     c                   ExSr      Sr_DecTba
378500030703     c                   Eval      VidTba = wdestba
378600030703    2c                   EndIf
378700030703If  2c                   If        §Ar5bTb2 <> *Zeros
378800030703     c                   Eval      wtba = §Ar5bTb2
378900030703     c                   ExSr      Sr_DecTba
379000030703     c                   Eval      VidTb2 = wdestba
379100030703    2c                   EndIf
379200030703    1c                   EndIf
379300030703    0c                   EndIf
379400911129     C*
379500131120     C**   ARBACC        IFEQ      'S'
379600131120     C**                 MOVEL     'SI'          V5CACC
379700131120     C**                 ELSE
379800131120     C**                 MOVEL     'NO'          V5CACC
379900131120     C**                 END
380000941215     C     ARBICC        IFNE      ' '
380100941216     C     ARBICC        ANDNE     'R'
380200930218     C                   MOVEL     'SI'          VIDICC
380300930218     C                   END
380400911022     C*
380500911022     C                   MOVEL     ARBRMN        VIDRMN
380600940712     C                   MOVEL     ARBRMA        VIDRMA
380700911022     C                   MOVEL     ARBRMO        VIDRMO
380800911022     C                   MOVEL     ARBNCD        VIDNCD
380900911022     C                   MOVEL     ARBNCA        VIDNCA
381000911022     C                   MOVEL     ARBZNC        VIDZNC
381100051104     c                   select
381200051104     c                   when      arbffd='S'
381300051104     C                   MOVEL     'SI '         VIDFFD
381400051104     c                   when      arbffd='M'
381500051104     C                   MOVEL     'SAB'         VIDFFD
381600051104     C                   other
381700051104     C                   MOVEL     '   '         VIDFFD
381800051104     C                   ENDsl
381900051124     c
382000940719     C                   MOVEL     ARBNFV        V3CNFV
382100051124     c* richiamp pgm per decodifica
382200051124     C                   MOVEL     *BLANKS       VIDTCR
382300051124     c                   clear                   vidhcr
382400051124     c                   clear                   viddcr
382500051124     c                   if        arbdcr>0 or arbhcr>0
382600051124     c                   clear                   fnlv80ds
382700051124     c                   eval      ilv80aas=arbaas
382800051124     c                   eval      ilv80lnp=arblnp
382900051124     c                   eval      ilv80nrs=arbnrs
383000051124     c                   eval      ilv80nsp=arbnsp
383100051124     c                   eval      ilv80mgs=arbmgs
383200051124     c                   eval      ilv80tc1=arbtc1
383300051124     c                   eval      ilv80tc2=arbtc2
383400051124     c                   eval      ilv80dcr=arbdcr
383500051124     c                   eval      ilv80hcr=arbhcr
383600051124     c                   eval      ilv80tcr=arbtcr
383700051124     c                   call      'FNLV80R'
383800051124     c                   parm                    kpjba
383900051124     c                   parm                    fnlv80ds
384000051124     c
384100051124     c                   eval      viddcr=olv80dcr8
384200051124     c                   eval      vidtcr=olv80tcrd
384300911023     C                   MOVEL     ARBHCR        VIDHCR
384400051124     c                   endif
384500051124     c
384600070521     c* Se presente ora consegna, la edito
384700070413     c                   clear                   vidhmc
384800070413     c                   if        arbhmc>0
384900070413     c                   eval      vidhmc=%editw(arbhmc: '0 :  ')
385000070413     C****               MOVE      ARBHMC        VIDHMC
385100070413     c                   endif
385200911023     C                   MOVEL     ARBNTC        VIDNTC
385300911023     C                   MOVEL     ARBTMC        VIDTMC
385400940923     C*
385500940923     C* DECODIFICO IL TIPO MANCATA CONSEGNA
385600940923     C                   MOVEL     *BLANKS       V5DTMC
385700940923     C     ARBTMC        IFNE      *BLANKS
385800940923     C                   MOVEL     '2A'          COD
385900940923     C                   MOVEL     *BLANKS       KEY
386000940923     C                   MOVEL     ARBTMC        KEY
386100940923     C     KTAB1         CHAIN     TABEL00F                           31
386200940923     C  N31              MOVEL     TBLUNI        V5DTMC
386300940923     C                   ENDIF
386400940923     C*
386500911023     C                   MOVEL     ARBMIF        VIDMIF
386600911023     C                   MOVEL     ARBNMI        VIDNMI
386700940923     C*
386800940713     C                   MOVEL     ARBGGA        V4CGGA
386900940923     C                   MOVEL     *BLANKS       V4IGGA
387000940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
387100940923     C     V4CGGA        COMP      *BLANKS                            4242
387200940923     C*
387300940713     C                   MOVEL     ARBGMA        V4CGMA
387400940923     C                   MOVEL     *BLANKS       V4IGMA
387500940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
387600940923     C     V4CGMA        COMP      *BLANKS                            4343
387700940923     C*
387800960208     C                   MOVEL     ARBGVA        V4CGVA
387900960208     C                   MOVE      ' '           V4CGVA
388000940923     C                   MOVEL     *BLANKS       V4IGVA
388100940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
388200940923     C     V4CGVA        COMP      *BLANKS                            4444
388300940923     C*
388400070403     C* FLAG blocco CONSEGNA
388500070403     C                   SELECT
388600070403     c                   WHEN      arbfbc='A'
388700070403     c                   eval      v1dfbc='LascAVVISO'
388800070403     C                   SETON                                        27
388900070403     c                   WHEN      arbfbc='B'
389000070403     c  n17              eval      v1dfbc=' BLOCCATA '
389100070412     c   17              eval      v1dfbc=' RIMPALLO '
389200070412     C                   SETON                                        27
389300070403     c                   WHEN      arbfbc='*'
389400070403     c  n17              eval      v1dfbc=' BLOCCATA '
389500070403     c   17              eval      v1dfbc=' GIACENTE '
389600070403     C                   SETON                                        27
389700070403     C                   ENDSL
389800070403     c
389900070403     C**   ARBFBC        COMP      'A'                                    27
390000070403     C**   ARBFBC        COMP      'B'                                    17
390100070403     C**N17ARBFBC        COMP      '*'                                    17
390200940711     C* TIPO CONSEGNA PARTICOLARE 1
390300940804     C                   MOVEL     *BLANKS       V5DTC1
390400940711     C     ARBTC1        IFNE      ' '
390500911115     C                   MOVEL     '1P'          COD
390600911115     C                   MOVEL     *BLANKS       KEY
390700940711     C                   MOVEL     ARBTC1        KEY
390800940713     C     KTAB1         CHAIN     TABEL                              31
390900940804     C  N31              MOVEL     TBLUNI        V5DTC1
391000940804     C   31              MOVEL     *BLANKS       V5DTC1
391100930113     C                   END
391200940711     C* TIPO CONSEGNA PARTICOLARE 2
391300940804     C                   MOVEL     *BLANKS       V5DTC2
391400940804     C                   MOVEL     *BLANKS       V7DTC2
391500940711     C     ARBTC2        IFNE      ' '
391600940711     C                   MOVEL     '1P'          COD
391700940711     C                   MOVEL     *BLANKS       KEY
391800940711     C                   MOVEL     ARBTC2        KEY
391900940713     C     KTAB1         CHAIN     TABEL                              31
392000940804     C  N31              MOVEL     TBLUNI        V5DTC2
392100940804     C   31              MOVEL     *BLANKS       V5DTC2
392200940804     C  N31              MOVEL     TBLUNI        V7DTC2
392300940804     C   31              MOVEL     *BLANKS       V7DTC2
392400940711     C                   END
392500930113     C* TIPO MANCATA CONSEGNA
392600930113     C                   MOVEL     *BLANKS       DESCCA
392700930113     C     ARBCCA        IFNE      ' '
392800930705     C                   Z-ADD     1             C
392900930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
393000930705     C   31              MOVEL     DCA(C)        ALF12            12
393100930705     C  N31              MOVEL     *BLANKS       ALF12
393200930113     C                   MOVEL     '-'           DESCCA
393300930113     C                   MOVE      ALF12         DESCCA
393400930113     C                   END
393500911022     C* GIRO DATA SPEDIZIONE
393600940712     C                   Z-ADD     ARBDSP        G02INV
393700940712     C                   Z-ADD     *ZEROS        G02DAT
393800940712     C                   MOVEL     '3'           G02ERR
393900940712     C                   CALL      'XSRDA8'
394000940712     C                   PARM                    WLBDAT
394100940719     C                   Z-ADD     G02DAT        V3CDSP
394200070403     c*
394300911022     C* DATA BORDERO'
394400070403     c                   if        arbdbr>0
394500070403     c     *iso          movel     arbdbr        dataeur
394600070403     c                   movel     dataeur       datadmy
394700070404     c                   movel     dataeur       w08dbr            8 0
394800070404     c                   movel     datadmy       v06dbr
394900070403     c                   else
395000070404     c                   clear                   v06dbr
395100070403     c                   endif
395200070404     c
395300070404     c* Prendo da fnblp i dati ritiro merce
395400070403     c     karb2         chain     fnblp01l
395500070403     c                   if        %found(fnblp01l) and blpdrt>0
395600070403     c     *iso          movel     blpdrt        dataeur
395700070403     c                   movel     dataeur       datadmy
395800070404     c                   movel     datadmy       v06drt
395900070404     c                   movel     blpnrt        v7cnrt
396000070406     c                   movel     blpctm        v3cctm
396100070404     c                   if        blpfpp='M'
396200070410     c                   eval      v7dfppr='MAT'
396300070404     c                   else
396400070410     c                   eval      v7dfppr='POM'
396500070404     c                   endif
396600070404     C                   MOVEL     BLPPDR        V7CPDR
396700070404     C                   movel     BlpPdr        kpdpdr
396800070404     C                   move      'A'           kpdtip
396900070404     C     kfapd         chain     fiapd01L
397000070404     c                   if        %found(fiapd01l)
397100070404     C                   MOVEL     APDRSC        V7DPDR
397200070404     c                   else
397300070404     C                   MOVEL     *BLANKS       V7DPDR
397400070404     c                   endif
397500070404     c
397600070404     c
397700070403     c                   else
397800070404     c                   clear                   v06drt
397900070404     c                   clear                   v7cnrt
398000070406     c                   clear                   v3cctm
398100070504     c                   clear                   v7dfppr
398200070404     c                   clear                   v7dpdr
398300070404     c                   clear                   v7cpdr
398400070403     c                   endif
398500070403     c
398600961112     C* SOLO PER LE BOLLE EXPORT VISUALIZZO LA DATA STAMPA MANIFEST
398700070404     C                   CLEAR                   V06DT3
398800070413     c                   if        *in16 and arbdt3>0  and arbdt3<>99999999
398900070404     c     *iso          movel     arbdt3        dataeur
399000070404     c                   movel     dataeur       datadmy
399100070404     c                   movel     datadmy       v06dt3
399200961112     C                   ENDIF
399300940713     C* DATA PARTENZA PRIMO COLLO
399400070404     c                   if        arbdpc>0
399500070404     c     *iso          movel     arbdpc        dataeur
399600070404     c                   movel     dataeur       datadmy
399700070404     c                   movel     datadmy       v06dpc
399800070404     c                   movel     dataeur       w08dpc
399900070404     c                   else
400000070404     c                   clear                   v06dpc
400100070404     c                   clear                   w08dpc            8 0
400200070404     c                   endif
400300940713     C* DATA PARTENZA ULTIMO COLLO
400400070403     C**                 Z-ADD     ARBDUC        G02INV
400500070403     C**                 Z-ADD     *ZEROS        G02DAT
400600070403     C**                 MOVEL     '3'           G02ERR
400700070403     C**                 CALL      'XSRDA8'
400800070403     C**                 PARM                    WLBDAT
400900070403     C**                 Z-ADD     G02DAT        V3CDUC
401000070403     c                   if        arbduc>0
401100070404     c     *iso          movel     arbduc        dataeur
401200070403     c                   movel     dataeur       datadmy
401300070404     c                   movel     datadmy       v06duc
401400070404     c                   movel     dataeur       w08duc
401500070403     c                   else
401600070404     c                   clear                   v06duc
401700070404     c                   clear                   w08duc            8 0
401800070403     c                   endif
401900911022     C* DATA ENTRATA AL TRANSITO
402000070404     c                   if        arbdet>0
402100070404     c     *iso          movel     arbdet        dataeur
402200070404     c                   movel     dataeur       datadmy
402300070404     c                   movel     datadmy       v06det
402400070404     c                   else
402500070404     c                   clear                   v06det
402600070404     c                   endif
402700911022     C* DATA USCITA DAL TRANSITO
402800070404     c                   if        arbdut>0
402900070404     c     *iso          movel     arbdut        dataeur
403000070404     c                   movel     dataeur       datadmy
403100070404     c                   movel     datadmy       v06dut
403200070404     c                   else
403300070404     c                   clear                   v06dut
403400070404     c                   endif
403500930113     C* DATA ARRIVO MERCE
403600070403     c                   if        arbdam>0
403700070403     c     *iso          movel     arbdam        dataeur
403800070403     c                   movel     dataeur       datadmy
403900070404     c                   movel     datadmy       v06dam
404000070403     c                   else
404100070404     c                   clear                   v06dam
404200070403     c                   endif
404300131121
404400930504     C* DATA CONSEGNA MERCE REALE
404500070404     c                   if        arbdcm>0
404600070413     C                   SETON                                        69
404700070404     c     *iso          movel     arbdcm        dataeur
404800070404     c                   movel     dataeur       datadmy
404900070404     c                   movel     dataeur       v08dcm
405000070404     c                   movel     datadmy       v06dcm
405100070404     c                   else
405200070404     c                   clear                   v06dcm
405300070404     c                   clear                   v08dcm
405400070404     c                   endif
405500131121
405600930505     C* DATA CONSEGNA PREVISTA ESTERNA/ORE DI RITARDO
405700070404     C                   Z-ADD     0             v08DCE
405800140113     C                   Z-ADD     0             amgDCE            8 0
405900070404     C                   Z-ADD     0             v06dce
406000940711     C                   Z-ADD     0             VIDNRC
406100131121     C*******            SETON                                        68
406200131121     C                   MOVEL     ' '           D98TLA
406300131121     C                   MOVEL     'A'           D98TBO
406400131121     C                   MOVEL     ARBAAS        D98AAS
406500131121     C                   MOVEL     ARBLNP        D98LNP
406600131121     C                   MOVEL     ARBNRS        D98NRS
406700131121     C                   MOVEL     ARBNSP        D98NSP
406800131121     C*
406900131121     C                   CALL      'TNSD99R'
407000131121     C                   PARM                    TNSD99DS
407100140110     c
407200140110     c* Mi salvo data teorica consegna
407300140110     c                   eval      s_arbdei=d98dei
407400140110     c                   eval      s_arboei=d98oei
407500150728     c
407600150728     c* memorizzo ore trazione è ore consegna cliente da passare al trulorsr
407700150728     c                   eval      s_arbttc=d98ttc
407800150728     c                   eval      s_arbtcc=d98tcc
407900151019     c                   eval      s_arbflp=d98flp
408000131121     C*
408100140110    1C     ARBDCE        IFEQ      0
408200131121    2c                   if        d98dce>0
408300140113     c                   eval      amgdce=d98dce
408400131121     c     *iso          movel     d98dce        dataeur
408500131121     c                   movel     dataeur       datadmy
408600131121     c                   movel     dataeur       v08dce
408700131121     c                   movel     datadmy       v06dce
408800131121   x2c                   else
408900131121     c                   clear                   v08dce
409000131121     c                   clear                   v06dce
409100131121    2c                   endif
409200131121     c
409300131121     C                   Z-ADD     D98NRC        VIDNRC
409400131121     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
409500131121     C*   CONSEGNA PREVISTA
409600131121     c  n69              eval      vidhmc=%editw(d98hce: '0 :  ')
409700131121     C*
409800131121     C* SE COMODO = 1  E' STATA FATTA CALL PGM
409900131121     C                   MOVEL     '1'           PA098
410000131121   x1C                   ELSE
410100140113     c                   eval      amgdce=arbdce
410200070404     c     *iso          movel     arbdce        dataeur
410300070404     c                   movel     dataeur       datadmy
410400070404     c                   movel     dataeur       v08dce
410500070404     c                   movel     datadmy       v06dce
410600070404     c
410700940711     C                   Z-ADD     ARBNRC        VIDNRC
410800131121    1C                   END
410900131121     c
411000911023     C* DATA MANDATO INTROITO
411100940712     C                   Z-ADD     ARBDMI        G02INV
411200940712     C                   Z-ADD     *ZEROS        G02DAT
411300940712     C                   MOVEL     '3'           G02ERR
411400940712     C                   CALL      'XSRDA8'
411500940712     C                   PARM                    WLBDAT
411600940712     C                   Z-ADD     G02DAT        VIDDMI
411700930113     C* DATA PRIMA CONSEGNA PARZIALE
411800930113     C                   Z-ADD     0             VIDDCP
411900930113     C     ARBDCP        IFGT      0
412000940712     C                   Z-ADD     ARBDCP        G02INV
412100940712     C                   Z-ADD     *ZEROS        G02DAT
412200940712     C                   MOVEL     '3'           G02ERR
412300940712     C                   CALL      'XSRDA8'
412400940712     C                   PARM                    WLBDAT
412500940712     C                   Z-ADD     G02DAT        VIDDCP
412600930113     C                   END
412700911023     C* DATA DISTINTA
412800940712     C                   Z-ADD     ARBDDC        G02INV
412900940712     C                   Z-ADD     *ZEROS        G02DAT
413000940712     C                   MOVEL     '3'           G02ERR
413100940712     C                   CALL      'XSRDA8'
413200940712     C                   PARM                    WLBDAT
413300940712     C                   Z-ADD     G02DAT        VIDDDC
413400911022     C* CODICE PADRONCINO CONSEGNA
413500940712     C                   MOVEL     ARBPDC        VIDPDC
413600940712     C                   MOVEL     *BLANKS       DESPDC
413700940712     C                   MOVEL     *BLANKS       DESPD1
413800940712     C     ARBPDC        IFGT      0
413900021203     C                   movel     arbpdc        kpdpdr
414000021203     C                   move      'A'           kpdtip
414100021203     C     kfapd         chain     fiapd01L                           31
414200940714     C  N31              MOVEL     APDRSC        DESPDC
414300940714     C  N31              MOVEL     APDRSC        DESPD1
414400930705     C                   END
414500950324     C* PRESTAZIONE PADRONCINO MATTINO O POMERIGGIO
414600950324     C                   MOVE      *BLANKS       FVVFPP
414700131120     C***                MOVE      *BLANKS       V5DFPP
414800070404     C                   MOVE      *BLANKS       V7DFPP
414900950327     C                   Z-ADD     ARBNDC        WNDC
415000020906     c                   Move      Arbifp        Wfgs
415100020906     c     Kfvv1         Chain     Fnfvv01l                           31
415200950324     C                   SELECT
415300991027     C                   WHEN      *IN31 = *on
415400991027     C                   WHEN      FVVFPP = 'M'
415500131120     C**                 eval      V5DFPP = 'MATTINO'
415600070504     C                   eval      V7DFPP = 'Mat'
415700991027     C                   WHEN      FVVFPP = 'P'
415800131120     C**                 eval      V5DFPP = 'POMERIGGIO'
415900070504     C                   eval      V7DFPP = 'Pom'
416000950324     C                   ENDSL
416100131119     c* controllo se ha PDA
416200131119     C                   Z-ADD     ARBNDC        kNDC6
416300131120     c                   clear                   vidpda
416400131120     c     Kdst          Chain     Fidst01l
416500131120     c                   if        %found(fidst01l)
416600131119     C                   movel     dstflr        DDStflr
416700131120     c                   if        (§dsttstpda= 'C' or  §dsttstpda= 'E') or
416800131120     c                             (dstpda<>'C' and  dstpda<>'E')
416900131120     c                   eval      vidpda='NO PDA'
417000131120     c                   movel     ri            ATRpda
417100131121     c                   else
417200131121     c                   eval      vidpda='SI PDA'
417300131121     c                   movel     ri            ATRpda
417400131119     c                   endif
417500131119     c                   endif
417600131108     c
417700131108     c* Se bolla in distinta, verifico quanti colli
417800131108     c                   clear                   v7dncl_co
417900131115     c                   clear                   v7dncl_de
418000131108     c                   clear                   wcol_D
418100131108     c                   clear                   wcol_I
418200131108    1c                   if        arbndc>0 and arbddc>0 and arbfbc=' '
418300131108     c* se ritorno all'incasso lo scrivo
418400131108     c
418500131108    2c                   if        arbdcm>0  and (arbicc='R' or arbicc=' ' or
418600131108     c                                  arbica=' ' or arbica='R')
418700131115     c                   eval      v7dncl_de='     IN consegna :'
418800131108     c                   eval      v7dncl_co='PER INCASSO'
418900131108    2c                   endif
419000131108     c
419100131108     c* Se colli da consegnare
419200131108    2c                   if        arbdcm=0
419300131108     C                   Z-ADD     ARBNDC        knfv
419400131108     c                   Move      Arbifp        Wfgs
419500131108     c
419600131108     c     karb2         setll     fnart01l
419700131108     c     karb2         reade     fnart01l
419800131108     c
419900131108    3c                   dow       not %eof(fnart01l)
420000131108     c                   if        artdcm=0
420100131108     c                   add       1             wcol_d
420200131108     c     kbrv          chain     fnbrv01l
420300131108     c                   if        %found(fnbrv01l)
420400131108     c                   add       1             wcol_I
420500131108     c                   endif
420600131108     c                   endif
420700131108     c
420800131108     c
420900131108     c     karb2         reade     fnart01l
421000131108    3c                   enddo
421100131108
421200131108    3c                   if        wcol_d>0
421300131115     c                   eval      v7dncl_de='Colli in consegna:'
421400131108    4c                   if        wcol_i=0
421500131108     c                   eval      v7dncl_co='0/'+%trim(%editc(wcol_d:'Z'))
421600131108     c                   else
421700131108     c                   eval      v7dncl_co=%trim(%editc(wcol_I:'Z')) +'/' +
421800131108     c                                       %trim(%editc(wcol_d:'Z'))
421900131108    4c                   endif
422000131108    3c                   endif
422100131108    c
422200131108    2c                   endif
422300131108    1c                   endif
422400131108     c
422500131108     c* Visualizzo ultimo evento
422600131108     c                   clear                   v7duevb
422700131114     c* prima vedo se c'e' da PDA
422800131114     c                   move      arbndc        pctndc
422900140224     c                   move      arbifp        pctfgs
423000131114     c                   move      'CET'         ttrd
423100131114     c     kcet          setgt     fipct02l
423200131114     c     kcet          readpe    fipct02l
423300140310     c                   if        not %eof(fipct02l)
423400131114     c                   movel     pctdati       fipctcetds
423500131114     c* consegnata
423600131114     c                   if        §pctcmc = ' '
423700131114     c                   MOVE      '  C'         evbcev                         *causale evento
423800131114     c                   else
423900131114     c* mancata consegna
424000131114     c                   movel     §PCTcmc       evbcev
424100131114     c                   endif
424200131114     c                   z-add     §pctdata      evbdev
424300131114     c                   movel     §pctora       evbhev
424400131114
424500131114     c                   exsr      Decodi_EVB
424600131114
424700131114     c                   else
424800131114     c*
424900131108     C                   Z-ADD     ARBAAS        KAAS$
425000131108     C                   Z-ADD     ARBLNP        KLNP
425100131108     C                   Z-ADD     ARBNRS        KNRS
425200131108     C                   Z-ADD     ARBNSP        KNSP
425300131108     C     KEVB1         SETgt     FNEVB01L
425400131108     C     KEVB1         readpe    FNEVB01L
425500131108     c
425600131114    1c                   if        not %eof(fnevb01l)
425700131114     c                   exsr      Decodi_EVB
425800131114    1c                   endif
425900131114    1c                   endif
426000911022     C*
426100911022     C* SE ESISTE AGGANCIO 2° BOLLA
426200991026     C                   Z-ADD     ARBAAS        KAAS
426300991026     C                   Z-ADD     ARBLNP        KLNP
426400991026     C                   Z-ADD     ARBNRS        KNRS
426500991026     C                   Z-ADD     ARBNSP        KNSP
426600991026     C                   MOVEL     '2'           KTRC
426700991027     C     KAR61         CHAIN     FIAR6000                           31
426800911023     C*
426900040216if  1C                   IF          %FOUND
427000991027     C                   eval      *in22 = *on
427100991027     C                   MOVEL     AR6CEI        V9CCEI
427200991027     C                   MOVEL     AR6NFT        V9CNFT
427300991027     C                   MOVEL     AR6FIV        V9CFIV
427400991027      *
427500991027     C                   eval      V9CDIV = AR6div
427600991027     C                   eval      WDIV = AR6div
427700991027     C                   exsr      CTR_VALUTA
427800991027      *
427900001128     C                   clear                   v9dfim
428000001128     C                   clear                   v9dfi2
428100991027     c                   if        AR6fim = 'P'
428200991027     c                   eval      V9Dfim = 'PARTENZA'
428300040126     c                   eval      V9Dfi2 = 'Immesse in'
428400991027     c                   endif
428500991027      * Varia G
428600040130     c                   clear                   W0133
428700040130     c                   clear                   v9csv1
428800040130     c                   clear                   v9cva1
428900040130     c                   clear                   v9csv2
429000040130     c                   clear                   v9cva2
429100040130     c                   clear                   v9csv3
429200040130     c                   clear                   v9cva3
429300040130     c                   clear                   v9csv4
429400040130     c                   clear                   v9cva4
429500041130     c                   clear                   v9csv5
429600041130     c                   clear                   v9cva5
429700041130     c                   clear                   v9csv6
429800041130     c                   clear                   v9cva6
429900040130     c                   clear                   v9civa
430000040130
430100040122      * Carico le varie
430200040122     c                   Movel     Ar6Sv1        wsvn
430300040122     c                   Z-add     Ar6Va1        wvan
430400040122     c                   ExSr      Car_Varie1
430500040122
430600040122     c                   Movel     Ar6Sv2        wsvn
430700040122     c                   Z-add     Ar6Va2        wvan
430800040122     c                   ExSr      Car_Varie1
430900040122
431000040122     c                   Movel     Ar6Sv3        wsvn
431100040122     c                   Z-add     Ar6Va3        wvan
431200040122     c                   ExSr      Car_Varie1
431300040122
431400040122     c     Kar61         Setll     Fiar7000
431500040216do  2c                   Do        *Hival
431600040122     c     Kar61         Reade     Fiar7000
431700040122     c                   If        %Eof(Fiar701l)
431800040122     c                   Leave
431900040122     c                   EndIf
432000040122     c                   Movel     Ar7Svn        wsvn
432100040122     c                   Z-add     Ar7Van        wvan
432200040216     c                   ExSr      Car_Varie1
432300040216e   2c                   EndDo
432400040122
432500991027      * TOTALE FATTURA
432600991027     c                   clear                   W0133
432700991027     c                   IF          AR6ift > *zeros
432800991027     c                   z-add     AR6ift        W0133
432900991027     c                   eval      Wtstdec = §cvfdc
433000991027     c                   exsr      EDTimporti
433100991027     c                   movel     IMPeditato    V9cift
433200001128     C                   else
433300001128     C                   clear                   v9cift
433400991027     c                   endif
433500991027      *
433600991027     c                   If         AR6dft > *zeros
433700991027     C     *iso          movel     AR6DFT        DATA_EUR
433800991027     C     *eur          movel     data_eur      W0080
433900991027     C                   eval      V9cdft = %EDITC(W0080:'Y')
434000001128     C                   else
434100001128     C                   clear                   v9cdft
434200991027     c                   Endif
434300991027      *
434400991027     c                   eval      V9Cksc = AR6ksc
434500991027     c                   eval      V9Cctr = AR6ctr
434600991027     c                   move      v9cksc        W0040
434700991027     c                   If        W0040 = 9999
434800991027     c                   eval      V9Dksc = arbrsd
434900991027     c                   Else
435000991027     c                   eval      i69kac = V9Cksc
435100991027     c                   exsr      CTR_ANAGRA
435200991027     c                   eval      V9Dksc = acorag
435300991027     c                   Endif
435400991027      *
435500991027     C                   eval      COD = 'TR'
435600991027     C                   movel     V9CCTR        UNOCTR            1
435700991027     C                   eval      KEY = unoctr
435800991027     C     KTAB1         CHAIN     TABEL00F                           31
435900991027     C                   movel     TBLUNI        V9DCTR
436000991027      *
436100930114     C* ESENZIONE IVA
436200001128     C                   clear                   v9dcei
436300991111     C     AR6CEI        IFNE      *blanks
436400940721     C                   MOVEL     'EI'          COD
436500991111     C                   MOVEL(P)  AR6CEI        KEY
436600940719     C     KTAB1         CHAIN     TABEL00F                           31
436700991027     C  N31              MOVEL     TBLUNI        V9DCEI
436800940719     C                   ENDIF
436900991027      *
437000040216e   1C                   ENDIF
437100930525     C*
437200991027     C* PER I DATI SULLA TASSAZIONE 1a BOLLA, DEVO CERCARE SU FIAR600F
437300940905     C                   SETOFF                                       24
437400110705     c                   clear                   atrbl4a
437500110705     c
437600110705     c                   clear                   v6car4ksc
437700110705     c                   clear                   v6dar4ksc
437800110705     c                   clear                   v6car4ctr
437900111021     c                   clear                   VIDimv
438000111021     c                   clear                   VIDpor
438100111021     c                   clear                   VIDift
438200111021     c                   clear                   VIDdiv
438300111021     c                   clear                   VIDfim
438400111021     C                   clear                   VIDCEI
438500111021     C                   clear                   VIDNFT
438600111021     C                   clear                   VIDFIV
438700111021     C                   clear                   DESCEI
438800111021     C                   clear                   viddft
438900111021     C                   clear                   vidIVA
439000111021      *
439100111021      * CERCO LE VARIE: LE PRIME 3 SU FIAR600F, EVENTUALI  ALTRE SU FIAR700F
439200111021     C                   eval      *in35 = *on
439300131107     C                   WRITE     LRI6C06
439400111021     C                   eval      *in35 = *off
439500111021      *
439600111021     c                   clear                   NR6
439700111021     c                   clear                   VIDiva
439800110705     c
439900940712     C                   Z-ADD     ARBAAS        KAAS
440000940712     C                   Z-ADD     ARBLNP        KLNP
440100940712     C                   Z-ADD     ARBNRS        KNRS
440200940712     C                   Z-ADD     ARBNSP        KNSP
440300110705     C                   MOVEL     'A'           KTRC
440400110705     C     KAR41         CHAIN     FiAR401L
440500110705if  1C                   IF        not %FOUND
440600110705     c                   eval      atrbl4a=nodi
440700110705     c                   else
440800110705     c                   movel     ar4not        dsbl4a
440900110705     c                   if        §b4aks>0
441000110705     C                   eval      *in24 = *on
441100110705     c                   eval      v6car4ksc=§b4aks
441200110705     c                   eval      i69kac = v6car4ksc
441300110705     c                   exsr      CTR_ANAGRA
441400110705     c                   eval      v6dar4ksc = acorag
441500110705     c                   if        §b4act<>*blanks
441600110705     c                   movel     §b4act        v6car4ctr
441700110705     c                   endif
441800110705     c* non c'e' cod cliente x tassaz.
441900110705     c                   else
442000110705     c                   eval      atrbl4a=nodi
442100110705     c                   endif
442200110705     c                   endif
442300110705     c
442400991026     C                   MOVEL     '1'           KTRC
442500991027     C     KAR61         CHAIN     FIAR6000
442600110705     c
442700110705     c                   if        %found(fiar601l)
442800110705     c
442900110705      * CLIENTE E TARIFFA FATTURAZIONE
443000110705     c                   IF        *IN33 = *ON
443100110705     c                   eval      v6cksc = arbksc
443200110705     c                   eval      v6cctr = arbctr
443300110705     c                   eval      v6dksc = arbrsm
443400110705     c                   ELSE
443500110705     c                   eval      v6cksc = AR6ksc
443600110705     c                   eval      v6cctr = AR6ctr
443700110705     c                   move      v6cksc        W0040
443800110705     c                   if        W0040 = 9999
443900110705     c                   eval      v6dksc = arbrsd
444000110705     c                   else
444100110705     c                   eval      i69kac = v6cksc
444200110705     c                   exsr      CTR_ANAGRA
444300110705     c                   eval      v6dksc = acorag
444400110705     c                   endif
444500110705     c                   ENDIF
444600110705      *   decodifica tariffa
444700110705     C                   eval      COD = 'TR'
444800110705     C                   MOVEL     V6CCTR        UNOCTR            1
444900110705     C                   MOVEL(P)  UNOCTR        KEY
445000110705     C     KTAB1         CHAIN     TABEL00F                           31
445100110705     C                   MOVEL     TBLUNI        V6DCTR
445200110705     c
445300110705     c                   endif
445400991027      *
445500040216if  1C                   IF        %FOUND  and
445600991027     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
445700991027     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
445800991027     C                              AR6sv3 <> *blanks)
445900991027      *
446000991027      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SE PRESENTE ALMENO UNO DEI CAMPI
446100991027     C                   EVAL      *IN24 = *ON
446200991027      *
446300991027     C                   eval      VIDDIV = AR6div
446400991027     C                   eval      WDIV = AR6div
446500991027     C                   exsr      CTR_VALUTA
446600991027      *
446700991027     c                   IF          ar6por > *zeros
446800991027     c                   z-add     AR6por        W0133
446900991027     c                   eval      Wtstdec = §cvfdc
447000991027     c                   exsr      EDTimporti
447100991027     c                   movel     IMPeditato    VIDpor
447200991128     c                   else
447300991128     c                   clear                   VIDpor
447400991027     c                   ENDIF
447500991027      *
447600991027     c                   IF          ar6imv > *zeros
447700991027     c                   z-add     AR6imv        W0133
447800991027     c                   eval      Wtstdec = §cvfdc
447900991027     c                   exsr      EDTimporti
448000991027     c                   movel     IMPeditato    VIDimv
448100991128     c                   else
448200991128     c                   clear                   VIDimv
448300991027     c                   ENDIF
448400991027      *
448500991027     c                   IF          ar6ift > *zeros
448600991027     c                   z-add     AR6ift        W0133
448700991027     c                   eval      Wtstdec = §cvfdc
448800991027     c                   exsr      EDTimporti
448900991027     c                   movel     IMPeditato    VIDift
449000991128     c                   else
449100991128     c                   clear                   VIDift
449200991027     c                   ENDIF
449300991027      *
449400991027     c                   IF        AR6fim = 'P'
449500991027     c                   eval      VIDfim = 'PARTENZA'
449600991128     c                   else
449700991128     c                   clear                   VIDfim
449800991027    2C                   ENDIF
449900991027      *
450000991027     C                   MOVEL     AR6CEI        VIDCEI
450100991027     C                   MOVEL     AR6NFT        VIDNFT
450200991027     C                   MOVEL     AR6FIV        VIDFIV
450300991027      *
450400991027     C                   IF        AR6CEI <>  *BLANKS
450500940808     C                   MOVEL     'EI'          COD
450600991027     C                   MOVEL(P)  AR6CEI        KEY
450700940808     C     KTAB1         CHAIN     TABEL00F                           31
450800940808     C  N31              MOVEL     TBLUNI        DESCEI
450900991128     c                   else
451000991128     c                   clear                   DESCEI
451100940808     C                   ENDIF
451200991027      *
451300991027      * DATA FATTURA DA GIRARE
451400991027     C                   IF         ar6dft > *zeros
451500991027     C     *iso          movel     AR6DFT        DATA_EUR
451600991027     C     *eur          movel     data_eur      W0080
451700991027     C                   eval      VIDdft = %EDITC(W0080:'Y')
451800991128     c                   else
451900991128     c                   clear                   VIDdft
452000991027     C                   ENDIF
452100991027      *
452200991027      *
452300991027     C                   MOVEL     ar6SV1        WSVN
452400991027     C                   MOVEL     ar6VA1        WVAN
452500991027     c                   exsr      car_varie
452600991027      *
452700991027     C                   MOVEL     ar6SV2        WSVN
452800991027     C                   MOVEL     ar6VA2        WVAN
452900991027     c                   exsr      car_varie
453000991027      *
453100991027     C                   MOVEL     ar6SV3        WSVN
453200991027     C                   MOVEL     ar6VA3        WVAN
453300991027     c                   exsr      car_varie
453400991027      *
453500991027      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
453600040216if  2C                   if          ar6SV3 <> *blanks
453700991027     C     KAR61         SETLL     FIAR7000
453800991027     C     KAR61         READE     FIAR7000
453900040216do  3C                   DOW       %EOF = *OFF
454000991027     C                   MOVEL     AR7SVN        WSVN
454100991027     C                   MOVEL     AR7VAN        WVAN
454200991027     c                   exsr      car_varie
454300991027     C     KAR61         READE     FIAR7000
454400040216e   3C                   ENDDO
454500040216e   2C                   endif
454600110705e   1C                   ENDIF
454700991027      *
454800991027      * Se non ho caricato record ne carico uno vuoto
454900110705     c                   IF        NR6 = *ZEROS  and *in24
455000991027     C                   clear                   V6CDE1
455100991027     C                   clear                   V6CVA1
455200991027     C                   clear                   V6CSV1
455300991027     C                   add       1             NR6
455400131107     C                   WRITE     LRI6S06
455500991027     c                   ENDIF
455600991027      *
455700991027      *
455800941206     C* 70 ON - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
455900941206     C     *IN33         IFEQ      '0'
456000941206     C     AR6NFT        ANDGT     0
456100941206     C     *IN22         OREQ      '1'
456200991111     C     AR6NFT        ANDGT     0
456300941206     C                   SETON                                        70
456400941206     C                   END
456500940712     C*
456600940719     C* PRELEVO LE NOTE DAL FILE APPOSITO
456700940719     C                   MOVEL     ARBAAS        KAAS
456800940719     C                   MOVEL     ARBLNP        KLNP
456900940719     C                   MOVEL     ARBNRS        KNRS
457000940719     C                   MOVEL     ARBNSP        KNSP
457100940719     C                   MOVEL     '8'           KTRC
457200060216     C     KAR41         CHAIN     FiAR401L                           31
457300940719     C  N31              MOVEL     AR4NOT        V4CNO1
457400940802     C   31              MOVEL     *BLANKS       V4CNO1
457500940719     C                   MOVEL     '9'           KTRC
457600060216     C     KAR41         CHAIN     FiAR401L                           31
457700940719     C  N31              MOVEL     AR4NOT        V4CNO2
457800940802     C   31              MOVEL     *BLANKS       V4CNO2
457900940719     C*
458000941110     C* ESTENSIONE RAGIONE SOCIALE DESTINATARIO
458100941110     C                   MOVEL     'D'           KTRC
458200060216     C     KAR41         CHAIN     FiAR401L                           31
458300950125     C  N31              MOVEL     AR4NOT        V4CNOT
458400950125     C   31              MOVEL     *BLANKS       V4CNOT
458500971015     C*
458600971015     C* SOLO PER EXPORT DATA DI STAMPA SU BORDERO' ARRIVI
458700070404     C***  *IN16         IFEQ      *ON
458800070404     C***                MOVEL     'A'           KTRC
458900070404     C***  KAR41         CHAIN     FiAR401L                           31
459000070404     C**N31              MOVE      AR4NOT        WDATA             8
459100070404     C***31              MOVEL     *ZEROS        WDATA
459200070404     C***  WDATA         IFGT      *ZEROS
459300070404     C***                MOVEL     WDATA         G02INV
459400070404     C***                Z-ADD     *ZEROS        G02DAT
459500070404     C***                MOVEL     '3'           G02ERR
459600070404     C***                CALL      'XSRDA8'
459700070404     C***                PARM                    WLBDAT
459800070404     C***                Z-ADD     G02DAT        V3CDST
459900070404     C***                ENDIF
460000070404     C***                ENDIF
460100940720     C*
460200051114     C* IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU FiAR900F
460300940720     C* CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
460400940720     C                   MOVEL     *BLANKS       VIDTIC
460500940720     C                   MOVEL     *ZERO         VIDCAS
460600940720     C                   MOVEL     *BLANKS       VIDVCA
460700940720     C                   MOVEL     *BLANKS       V4CGCA
460800940923     C                   MOVEL     *BLANKS       V4IGCA
460900091214     c                   setoff                                       65
461000940720     C     §3AFCA        IFGT      *ZERO
461100051128     C     Ksped         CHAIN     FiAR901L                           31
461200940720     C  N31              DO
461300940720     C                   MOVEL     AR9TIC        VIDTIC
461400940720     C                   MOVEL     AR9CAS        VIDCAS
461500940720     C                   MOVEL     AR9VCA        VIDVCA
461600940720     C                   MOVEL     AR9GCA        V4CGCA
461700091214     c                   seton                                        65
461800940720     C                   ENDDO
461900940720     C                   ENDIF
462000941215     C* ESISTE CONTRASSEGNO: VEDO SE ABILITATO
462100941215     C     VIDCAS        IFNE      0
462200941215     C                   SETON                                        29
462300941215     C                   END
462400940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
462500940923     C     V4CGCA        COMP      *BLANKS                            4141
462600940720     C*
462700940720     C* RIEMPO ANCHE ALCUNI CAMPI PER LE DS DI PASSAGGIO PARAMETRI
462800940720     C                   MOVEL     V3CDSP        PA1DSP
462900940720     C                   MOVEL     ARBRMN        PA1RMN
463000940720     C                   MOVEL     ARBLNA        PA1LNA
463100940802     C                   MOVEL     V3DLNA        PA1LN1
463200940720     C                   MOVEL     ARBRSD        PA1RSD
463300940906     C*-------------------------------------------------------------------------
463400991029      * GESTIONE INCASSO PARZIALE se assegnato o presente estensione bolla
463500991102     C                   clear                   v5cdiv
463600991102     c                   clear                   Wariimp
463700991102     c                   clear                   Wariipp
463800991102     c                   clear                   VIDimp
463900991102     c                   clear                   VIDipp
464000991102     C                   MOVEL     ARBFIP        VIDFIP
464100991102      *
464200991029     c  N33              eval      V5Cdiv = viddiv
464300991029     c   22              eval      V5Cdiv = v9cdiv
464400991029      *
464500040216if  1c                   if        V5CDIV <> *blanks
464600991029      *
464700991029     C                   eval      WDIV = V5Cdiv
464800991029     C                   exsr      CTR_VALUTA
464900991029      *
465000991029     C                   MOVEL     ARBAAS        KAAS
465100991029     C                   MOVEL     ARBLNP        KLNP
465200991029     C                   MOVEL     ARBNRS        KNRS
465300991029     C                   MOVEL     ARBNSP        KNSP
465400991029     C                   MOVEL     'A'           ktrc
465500991029      *
465600991029     c     kar61         setll     fiari000
465700991029     c     kar61         reade     fiari000
465800991029      *
465900040216do  2c                   DOW       not %EOF
466000040216if  3c                   if        v5cdiv = aridiv
466100991029     c                   add       ARIimp        Wariimp
466200991029     c                   add       ARIipp        Wariipp
466300040216x   3c                   else
466400991029      *
466500991029     C                   clear                   yeurcods
466600991029     C                   eval      YECDVI = aridiv
466700991029     C                   eval      YECDVO = V5Cdiv
466800991029     C                   eval      YECIMI = ARIimp
466900011022     C                   move      §CVDEC        YECDCO
467000991029     C                   CALL      'YEURCO'
467100991029     C                   PARM                    YEURcods
467200991029    6C                   if        YECESI = *blanks
467300991029     C                   add       YECIMO        WARIimp
467400991029    6C                   endif
467500991029      *
467600991029     C                   clear                   yeurcods
467700991029     C                   eval      YECDVI = aridiv
467800991029     C                   eval      YECDVO = V5Cdiv
467900991029     C                   eval      YECIMI = ARIipp
468000011022     C                   move      §CVDEC        YECDCO
468100991029     C                   CALL      'YEURCO'
468200991029     C                   PARM                    YEURcods
468300991029    6C                   if        YECESI = *blanks
468400991029     C                   add       YECIMO        WARIipp
468500991029    6C                   endif
468600991029      *
468700040216e   3c                   endif
468800991029      *
468900991029     c     kar61         reade     fiari000
469000040216e   2c                   enddo
469100991029      *
469200991029     c                   IF        WARIipp > *zeros
469300991029     c                   z-add     Wariipp       W0133
469400991029     c                   eval      Wtstdec = §cvfdc
469500991029     c                   exsr      EDTimporti
469600991029     c                   movel     IMPeditato    VIDipp
469700991029     c                   ENDIF
469800991029      *
469900991029     c                   IF        WARIimp > *zeros
470000991029     c                   z-add     Wariimp       W0133
470100991029     c                   eval      Wtstdec = §cvfdc
470200991029     c                   exsr      EDTimporti
470300991029     c                   movel     IMPeditato    VIDimp
470400991029     c                   ENDIF
470500991105      *
470600991105     c                   IF        VIDipp = *blanks  and  VIDimp = *blanks
470700991105     c                   clear                   V5CDIV
470800991105     c                   ENDIF
470900991029      *
471000040216e   1C                   endif
471100991029     C*-------------------------------------------------------------------------
471200951220     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
471300951220     C*
471400951220     C* PULISCO IL SFL
471500960926     C                   SETOFF                                       72
471600131107     C                   WRITE     LRI6C05
471700960926     C                   SETON                                        7172
471800951220     C*
471900060216     C* CHIAVE PER ACCEDERE AL FILE FiAR401L
472000951220     C                   MOVEL     ARBAAS        KAAS
472100951220     C                   MOVEL     ARBLNP        KLNP
472200951220     C                   MOVEL     ARBNRS        KNRS
472300951220     C                   MOVEL     ARBNSP        KNSP
472400951220     C*
472500951220     C                   Z-ADD     0             NRR5              4 0
472600951220     C                   MOVEL     '7K'          COD
472700951220     C*
472800060216     C     KAR42         SETLL     FiAR401L
472900060216     C     KAR42         READE     FiAR401L                               36
473000040216do  1C     *IN36         DOWEQ     *OFF
473100951220     C*
473200951220     C                   MOVEL(P)  AR4TRC        KEY
473300951220     C     KTAB1         CHAIN     TABEL                              36
473400951220     C*
473500951220     C     *IN36         IFEQ      *OFF
473600951220     C     TBLFLG        ANDEQ     ' '
473700951220     C                   MOVEL     TBLUNI        DS7K
473800951220     C     §7KVIS        IFEQ      'S'
473900951220     C                   MOVEL     §7KDES        VS5DES
474000951220     C                   MOVEL     AR4NOT        VS5NOT
474100951220     C                   ADD       1             NRR5
474200131107     C                   WRITE     LRI6S05
474300951220     C                   ENDIF
474400951220     C                   ENDIF
474500951220     C*
474600060216     C     KAR42         READE     FiAR401L                               36
474700040216e   1C                   ENDDO
474800951220     C*
474900960926     C* SE NON HO CARICATO NIENTE NON EMETTO IL SFLDSP PER EVITARE CHE
475000960926     C*  IL PGM DIA ERRORE DI SESSIONE O UNITA'
475100951220     C     NRR5          IFEQ      0
475200991105     C                   clear                   VS5DES
475300991105     C                   clear                   VS5NOT
475400991105     C                   ADD       1             NRR5
475500131107     C                   WRITE     LRI6S05
475600951220     C                   ENDIF
475700951220     C*
475800021216      * Riempio il campone in fondo alla videata con i tasti scelti
475900021216     C                   exsr      Ges_TastiFun
476000021217     c                   eval      FD07 = FDxx
476100021217     c                   eval      F24D07 = F24Dxx
476200911022     C                   ENDSR
476300131114      *-------------------------------------------------------------------------
476400131114      *  Decodifica EVENTO
476500131114      *-------------------------------------------------------------------------
476600131114     c     Decodi_evb    BEGSR
476700131114
476800131114     c                   if        evbcev='  C'
476900131114     c                   eval      §2ades='CONSEGNA'
477000131128     c* aggiungo anche firmatario
477100131128     c                   if        §PCTNOMFIR<>*blaNKS
477200131128     C                   EVAL      §2ADES=%TRIM(§2ADES)+' Firma:' +§PCTNOMFIR
477300131128     c                   endif
477400131128     c
477500131114     C                   else
477600131114     C                   MOVEL     '2A'          COD
477700131114     C                   MOVEL(p)  evbcev        KEY
477800131114     C     KTAB1         CHAIN     TABEL00F
477900131114    3c                   if        not %found(tabel00f)
478000131114     c                   clear                   ds2a
478100131114     c                   else
478200131114     c                   eval      ds2a=tbluni
478300131114    3c                   endif
478400131114    3c                   endif
478500131114     c
478600131114     c                   eval      v7duevb=evbcev+'-'+§2ades
478700131114
478800131114    3c                   if        evbdev>0
478900131114     c     *iso          movel     evbdev        dataeur
479000131114     c                   movel     dataeur       datadmy
479100131114     c                   movel     dataeur       w7cdev            8 0
479200131114
479300131114     c                   eval      indx=%checkr(' ':v7duevb)
479400131114    4c                   if        indx>46
479500131114     c                   eval      indx=46
479600131114     c                   else
479700131114     c                   eval      indx=indx+1
479800131114    4c                   endif
479900131114     c                   eval      wcarat=66-indx
480000131114     c                   eval      %subst(v7duevb:indx:wcarat)=' ' +'il ' +
480100131114     c                             %editw(W7cdev: '  /  /    ')
480200131114     c
480300131114    4c                   if        evbhev>0
480400131114     c                   eval      v7duevb=%trim(v7duevb)+' ' +
480500131114     c                                                 %editw(evbhev: '0 :  ')
480600131114    4c                   endif
480700131114     c
480800131114    3c                   endif
480900131114     c                   ENDSR
481000030703      *-------------------------------------------------------------------------
481100030703      *  Decoditifa tipo bancali
481200030703      *-------------------------------------------------------------------------
481300030703     c     Sr_DecTba     BegSr
481400030703     c                   Clear                   Tibs02Ds
481500030703     c                   Clear                   dTba
481600030703     c                   Movel     'C'           T02Mod
481700030703     c                   Movel     Knsif         T02Sif
481800030703     c                   Movel     'TBA'         T02Cod
481900030703     c                   Movel(p)  wtba          T02Ke1
482000030703     c                   Call      'TIBS02R'
482100030703     c                   Parm                    Kpjba
482200030703     c                   Parm                    Tibs02Ds
482300030703If  3c                   If        T02Err = *Blanks
482400030703     c                   Movel     T02Uni        dTba
482500030703    3c                   EndIf
482600030703     c                   Eval      wdestba = §TbaDesc
482700030703     c                   EndSr
482800021216      *-------------------------------------------------------------------------
482900021216      *  Imposto descrizione tasti funzione
483000021216      *-------------------------------------------------------------------------
483100021216     c     Ges_TastiFun  BEGSR
483200021217      *
483300021217      * per emettere i tasti senza condizionamenti ..
483400021216     c                   setoff                                       89
483500021216      *
483600021216      * Conta i caratteri riempiti dalle RigaTf1
483700021217     c                   z-add     0             $z                3 0
483800021216      *
483900021216      * Conta i caratteri riempiti dalle RigaTf2
484000021217     c                   z-add     0             $k                3 0
484100021216      *
484200021216      * Conta i caratteri riempiti dalle RigaTf3
484300021217     c                   z-add     0             $j                3 0
484400021216      *
484500021216      * Conta la posizione del campone da cui partire per impostare
484600021216      * la descrizione del tasto funzione
484700021216     c                   z-add     0             $y                2 0
484800021216      *
484900021217      * Stringhe che contengono le descrizioni dei tasti funzione
485000021216     c                   reset                   RigaTf1
485100021216     c                   reset                   RigaTf2
485200021216     c                   reset                   RigaTf3
485300070411      *
485400070411      *?F01?--> scelta bolla da assistenza clienti
485500070411     c                   IF        *in63 = *on
485600070411     c                   reset                   $Tf
485700070411     c                   movea     Tf01          $Tf
485800070411     c                   exsr      Rie_$Tf
485900070411     c                   else
486000070411      *?F01?--> scelta bolla generica
486100070411     c                   if        *in61 = *on
486200070411     c                   reset                   $Tf
486300070411     c                   movea     Tf01B         $Tf
486400070411     c                   exsr      Rie_$Tf
486500070411     c                   ENDIF
486600070411     c                   ENDIF
486700021216      *
486800070411      * F13 --> tassazione 1 bolla
486900021216     c                   IF        *in24 = *on
487000021217     c                   reset                   $Tf
487100021217     c                   movea     Tf13          $Tf
487200021217     c                   Exsr      Rie_$Tf
487300021217     C                   ENDIF
487400021216      *
487500070411      * F04 --> tassazione 2 bolla
487600021216     c                   IF        *in22 = *on
487700021217     c                   reset                   $Tf
487800021217     c                   movea     Tf04          $Tf
487900021217     c                   Exsr      Rie_$Tf
488000021217     C                   ENDIF
488100040220      *
488200070411      *?F16 --> assistenza clienti
488300040220     c                   IF        *in64 = *on
488400040220     c                   reset                   $Tf
488500040220     c                   movea     Tf16          $Tf
488600040220     c                   exsr      Rie_$Tf
488700040220     c                   ENDIF
488800021216      *
488900070411      * F05 --> eventi
489000021216     c                   IF        *in95 = *on
489100021217     c                   reset                   $Tf
489200021217     c                   movea     Tf05          $Tf
489300021217     c                   Exsr      Rie_$Tf
489400021217     C                   ENDIF
489500021216      *
489600021216      * F10
489700021217     c                   IF        *in98 = *on
489800021217     c                   reset                   $Tf
489900021217     c                   movea     Tf10          $Tf
490000021217     c                   Exsr      Rie_$Tf
490100021217     c                   ENDIF
490200021216      *
490300021216      * F14
490400021217     c                   IF        *in03 = *on
490500021217     c                   reset                   $Tf
490600021217     c                   movea     Tf14          $Tf
490700021217     c                   Exsr      Rie_$Tf
490800021217     c                   ENDIF
490900021216      *
491000021216      * F02
491100021217     c                   IF        *in04 = *on
491200021217     c                   reset                   $Tf
491300021217     c                   movea     Tf02          $Tf
491400021217     c                   Exsr      Rie_$Tf
491500021217     c                   ENDIF
491600110929      *
491700110930      * F22 --> Perfect Order
491800110929     c                   IF        *in86 = *on
491900110929     c                   reset                   $Tf
492000110929     c                   movea     Tf22          $Tf
492100110929     c                   Exsr      Rie_$Tf
492200110929     C                   ENDIF
492300021216      *
492400021216      * F11
492500021217     c                   IF        *in77 = *on
492600021217     c                   reset                   $Tf
492700021217     c                   movea     Tf11          $Tf
492800021217     c                   Exsr      Rie_$Tf
492900021217     c                   ENDIF
493000070404      *
493100070413      * F17 --> solo se partito com GEO
493200070413     c                   if        *in17
493300070404     c                   reset                   $Tf
493400070404     c                   movea     Tf17          $Tf
493500070404     c                   Exsr      Rie_$Tf
493600070413     c                   endif
493700021216      *
493800021216      * F07
493900021217     c                   IF        *in67 = *on
494000021217     c                   reset                   $Tf
494100021217     c                   movea     Tf07          $Tf
494200021217     c                   Exsr      Rie_$Tf
494300021217     c                   ENDIF
494400021216      *
494500021216      * F08
494600021217     c                   IF        *in74 = *on and *in16 = *on
494700021217     c                   reset                   $Tf
494800021217     c                   movea     Tf08          $Tf
494900021217     c                   Exsr      Rie_$Tf
495000021217     c                   ENDIF
495100130830
495200130830      * F09 --> Orari servizi non condizionato
495300131007     c                   IF        *in89 = *off and wabif9='S'
495400130930     c                   reset                   $Tf
495500130930     c                   movea     Tf09          $Tf
495600130930     c                   Exsr      Rie_$Tf
495700130930     c                   ENDIF
495800021216      *
495900021216      * F20 --> non condizionato
496000021217     c                   IF        *in89 = *off
496100021217     c                   reset                   $Tf
496200021217     c                   movea     Tf20          $Tf
496300021217     c                   Exsr      Rie_$Tf
496400021217     c                   ENDIF
496500021216      *
496600021216      * F21 --> non condizionato
496700021216     c                   IF        *in89 = *off
496800021217     c                   reset                   $Tf
496900021217     c                   movea     Tf21          $Tf
497000021217     c                   Exsr      Rie_$Tf
497100021217     c                   ENDIF
497200021218      *
497300021218      * F19 --> non condizionato
497400021218     c                   IF        *in89 = *off
497500021218     c                   reset                   $Tf
497600021218     c                   movea     Tf19          $Tf
497700021218     c                   Exsr      Rie_$Tf
497800021218     c                   ENDIF
497900070420     c
498000130830      * F06 --> solo se partito con GEO
498100070420     c                   if        *in17
498200070420     c                   reset                   $Tf
498300070420     c                   movea     Tf06          $Tf
498400070420     c                   Exsr      Rie_$Tf
498500070420     c                   ENDIF
498600021217      *
498700021217      * F12 --> non condizionato
498800021217     c                   IF        *in89 = *off
498900021217     c                   reset                   $Tf
499000021217     c                   movea     Tf12          $Tf
499100021217     c                   Exsr      Rie_$Tf
499200021217     c                   ENDIF
499300021216      *
499400021217      * Pulisco le stringa
499500021216     c                   z-add     1             $h
499600030121     c     1             do        62            $h                2 0
499700021216     C                   if        RigaTf1($h) = '#'
499800021216     c                   movel     *blanks       RigaTf1($h)
499900021216     c                   endif
500000021216     c                   enddo
500100021216     c                   z-add     1             $h
500200030121     c     1             do        62            $h                2 0
500300021216     C                   if        RigaTf2($h) = '#'
500400021216     c                   movel     *blanks       RigaTf2($h)
500500021216     c                   endif
500600021216     c                   enddo
500700021216     c                   z-add     1             $h
500800030121     c     1             do        62            $h                2 0
500900021216     C                   if        RigaTf3($h) = '#'
501000021216     c                   movel     *blanks       RigaTf3($h)
501100021216     c                   endif
501200021216     c                   enddo
501300021217      *
501400021216      * Imposto la descrizione del tasto funzione F24 e segnalo
501500021216      * quante righe ho riempito e quale devo emettere
501600021216     c                   SELECT
501700021216      *
501800021216     c                   when      $k<>*zeros and $Y<>*zeros
501900021216     c                   eval      $loop = 3
502000021216     c                   z-add     3             $loop             1 0
502100021217     C                   movel(p)  CF2413        F24Dxx           16
502200021216      *
502300021216     c                   when      $k<>*zeros and $Y=*zeros
502400021216     c                   z-add     2             $loop             1 0
502500021217     C                   movel(p)  CF2412        F24Dxx
502600021216      *
502700021216     c                   Other
502800021216     c                   z-add     1             $loop             1 0
502900021217     C                   movel     *blanks       F24Dxx
503000021216     c                   ENDSL
503100021216      * Prima riga di tasti funzione
503200030121     c                   movea     RigaTF1       FDxx             62
503300021216     c                   z-add     1             $Rig              1 0
503400021216      *
503500021216     c                   endsr
503600021217      *-------------------------------------------------------------------------
503700021217      *  Riempio le stringhe con i campi funzione compattati
503800021217      *-------------------------------------------------------------------------
503900021217     c     Rie_$Tf       begsr
504000021217      *
504100021217     c                   z-add     1             $x                2 0
504200021217     c     '#'           lookup    $Tf($x)                                31
504300021217     c                   add       $x            $Z
504400030121     c                   IF        $Z <= 63
504500021217     c                   eval      $j = $Z - $x + 1
504600021217     c                   movea     $Tf           RigaTF1($j)
504700021217     c                   ELSE
504800021217     c                   add       $x            $K
504900030121     c                   IF        $K <= 63
505000021217     c                   eval      $j = $K - $x + 1
505100021217     c                   movea     $Tf           RigaTF2($j)
505200021217     c                   ELSE
505300021217     c                   add       $x            $Y
505400030121     c                   IF        $Y <= 63
505500021217     c                   eval      $j = $Y - $x + 1
505600021217     c                   movea     $Tf           RigaTF3($j)
505700021217     c                   ENDIF
505800021217     c                   ENDIF
505900021217     c                   ENDIF
506000021217      *
506100021217     c                   endsr
506200021217      *-------------------------------------------------------------------------
506300021217      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
506400021217      *-------------------------------------------------------------------------
506500021217     c     Ges_F24       begsr
506600021217      *
506700021217     c                   SELECT
506800021217      *
506900021217     c                   WHEN      $Loop = 2 and $Rig =2  or
507000021217     c                             $Loop = 3 and $Rig =3
507100021217     c                   movea     RigaTF1       FDxx
507200021217     c                   z-add     1             $Rig
507300021217      *
507400021217     c                   if        $loop = 2
507500021217     c                   eval      F24Dxx= Cf2412
507600021217     c                   else
507700021217     c                   eval      F24Dxx= Cf2413
507800021217     c                   endif
507900021217      *
508000021217     c                   WHEN      $Loop = 2 and $Rig =1 or
508100021217     c                             $Loop = 3 and $Rig =1
508200021217     c                   movea     RigaTF2       FDxx
508300021217     c                   z-add     2             $Rig
508400021217      *
508500021217     c                   if        $loop = 2
508600021217     c                   eval      F24Dxx= Cf2422
508700021217     c                   else
508800021217     c                   eval      F24Dxx= Cf2423
508900021217     c                   endif
509000021217      *
509100021217     c                   WHEN      $Loop = 3 and $Rig =2
509200021217     c                   movea     RigaTF3       FDxx
509300021217     c                   z-add     3             $Rig
509400021217     c                   eval      F24Dxx= Cf2433
509500021217     c                   ENDSL
509600021217      *
509700021217     c                   endsr
509800021217      *-------------------------------------------------------------------------
509900991027      *  CARICO SUBFILE VARIE
510000991027      *-------------------------------------------------------------------------
510100991027     c     CAR_VARIE     BEGSR
510200991128      *
510300991027      *
510400991027     c                   SELECT
510500991027      *
510600991027     c                   WHEN       WSVN = *BLANKS
510700991027      *
510800991027     C                   WHEN       WSVN = §$2iva
510900991027     c                   z-add     Wvan          W0133
511000991027     c                   eval      Wtstdec = §cvfdc
511100991027     c                   exsr      EDTimporti
511200991027     c                   movel     IMPeditato    VIDiva
511300991027      *
511400991027     c                   OTHER
511500991027     c                   z-add     Wvan          W0133
511600991027     c                   eval      Wtstdec = §cvfdc
511700991027     c                   exsr      EDTimporti
511800991027     c                   movel     IMPeditato    V6Cva1
511900991027     c                   movel     WSVN          V6Csv1
512000991027     C                   Z-ADD     1             WA
512100991027     C     V6CSV1        LOOKUP    CC1(WA)                                99
512200020329     C     CC3(WA)       ifeq      'S'
512300020329      *
512400020329      * Verifico se si tratta di una bolla Fedex
512500021119     c                   if        LnaNtw = 'FED'
512600020329     C                   Z-ADD     1             WA
512700020329     C     V6CSV1        LOOKUP    CB1(WA)                                99
512800020329     C   99              MOVEL     CB2(WA)       V6CDE1
512900020329     C  N99              clear                   V6CDE1
513000020329     C                   else
513100991027     C   99              MOVEL     CC2(WA)       V6CDE1
513200991027     C  N99              MOVEL     *BLANKS       V6CDE1
513300020329     C                   endif
513400020329     C                   else
513500020329     C   99              MOVEL     CC2(WA)       V6CDE1
513600020329     C  N99              MOVEL     *BLANKS       V6CDE1
513700020329     C                   endif
513800991027     C                   add       1             NR6
513900131107     C                   WRITE     LRI6S06
514000991027      *
514100991027     c                   ENDSL
514200991027      *
514300991027     c                   ENDSR
514400040122      *-------------------------------------------------------------------------
514500040122      *  Carico le varie della seconda bolla
514600040122      *-------------------------------------------------------------------------
514700040122     c     Car_Varie1    BegSr
514800040122
514900040122s   1c                   Select
515000040122
515100040122     c                   When      wsvn = *Blanks
515200040122      * Iva
515300040122     c                   When      wsvn = §$2iva
515400040122     c                   z-add     wvan          w0133
515500040122     c                   Eval      Wtstdec = §cvfdc
515600040122     c                   ExSr      EDTimporti
515700040122     c                   Movel     IMPeditato    V9cIva
515800070411     c                   setoff                                       06
515900070411      * Bollo
516000070411     c                   When      wsvn = §$2bol
516100070411     c                   z-add     wvan          w0133
516200070411     c                   Eval      Wtstdec = §cvfdc
516300070411     c                   ExSr      EDTimporti
516400070411     c                   Movel     IMPeditato    V9cIva
516500070411     c                   seton                                        06
516600040122      * Varia G - sempre per prima
516700040122     c                   When      wsvn = §$2vrg
516800040122     c                   Eval      V9cSv1 = wsvn
516900040122     c                   z-add     wvan          w0133
517000040122     c                   Eval      Wtstdec = §cvfdc
517100040122     c                   ExSr      EDTimporti
517200040122     c                   Movel     IMPeditato    V9cVa1
517300040122      * Altre varie
517400040122   x1c                   Other
517500040122     c                   z-add     wvan          W0133
517600040122     c                   eval      Wtstdec = §cvfdc
517700040122     c                   exsr      EDTimporti
517800040122if  2c                   If        V9cSv2 = *Blanks
517900040122     c                   movel     IMPeditato    V9cVa2
518000040122     c                   movel     wsvn          V9csv2
518100040122   x2c                   Else
518200040122if  3c                   If        V9cSv3 = *Blanks
518300040122     c                   movel     IMPeditato    V9cVa3
518400040122     c                   movel     wsvn          V9csv3
518500040122   x3c                   Else
518600040122if  4c                   If        V9cSv4 = *Blanks
518700040122     c                   movel     IMPeditato    V9cVa4
518800040122     c                   movel     wsvn          V9csv4
518900041130   x4c                   Else
519000041130if  5c                   If        V9cSv5 = *Blanks
519100041130     c                   movel     IMPeditato    V9cVa5
519200041130     c                   movel     wsvn          V9csv5
519300041130   x5c                   Else
519400041130if  6c                   If        V9cSv6 = *Blanks
519500041130     c                   movel     IMPeditato    V9cVa6
519600041130     c                   movel     wsvn          V9csv6
519700041130    6c                   EndIf
519800041130    5c                   EndIf
519900040122    4c                   EndIf
520000040122    3c                   EndIf
520100040122    2c                   EndIf
520200040122
520300040122    1c                   EndSl
520400040122
520500040122     c                   EndSr
520600991027      *****************************************************************
520700991027      *  AGGANCIO ANAGRAFICHE
520800991027      *****************************************************************
520900991027     C     CTR_ANAGRA    BEGSR
521000991027      *
521100991027     C                   clear                   DS_cnaco
521200991027     C                   clear                   DS_cnind
521300991027     C                   clear                   DS_cnclp
521400991027     C                   clear                   DS_fncls
521500991027      *
521600991027     C                   CALL      'TIBS69R'
521700991027     C                   PARM                    tibs69DS
521800991027     C                   PARM                    DS_cnaco
521900991027     C                   PARM                    DS_cnind
522000991027     C                   PARM                    DS_cnclp
522100991027     C                   PARM                    DS_fncls
522200991027      *
522300991027     C                   eval      wtibs69r = 'S'
522400991027      * per il 1° giro è inizializzata nelle specifiche "D"
522500991027     C                   clear                   TIBS69DS
522600991027      *
522700991027     C                   ENDSR
522800991027      **********************************************************************
522900991027      * AGGANCIO TABELLA VALUTE
523000991027      **********************************************************************
523100991027     C     CTR_VALUTA    BEGSR
523200991027
523300991027     C                   MOVEL     'CV'          COD
523400991027     C                   MOVEL(P)  WDIV          KEY
523500991027     C     KTAB1         CHAIN     TABEL00F                           31
523600991027     C  N31              MOVEL     TBLUNI        DSCV
523700991027
523800991027     C                   ENDSR
523900991027      **********************************************************************
524000991027      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
524100991027      **********************************************************************
524200991027     C     EDTIMPORTI    BEGSR
524300991027
524400991027      * ------------------------------------------------------------------------
524500991027      * W0133      contiene il numero da editare per il video
524600991027      * WTSTDEC    corrisponde a §CVfdc della tabella CV
524700991027      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
524800991027      * ------------------------------------------------------------------------
524900991027      *
525000991027     c                   clear                   IMPeditato
525100991027     c                   clear                   W017A
525200991027      *
525300991027     c                   if        Wtstdec = 'S'
525400991027     C                   eval      W017a = %EDITC(W0133:'2')
525500991027     c                   else
525600991027     c                   z-add     W0133         W0130
525700991027     C                   eval      W017a = %EDITC(W0130:'2')
525800991027     c                   endif
525900991027      *
526000991027     c                   move      W017a         IMPeditato
526100991027      *
526200991027     c                   ENDSR
526300940802     C**************************************************************************
526400940802     C*    LEGAMI BOLLA
526500940802     C**************************************************************************
526600930413     C     LEGBOL        BEGSR
526700940802     C*
526800940912     C* DEVO MEMORIZZARE LA BOLLA POICHE DEVO RIAGGANCIARLA DOPO
526900940912     C                   MOVEL     ARBAAS        WAAS
527000940912     C                   MOVEL     ARBLNP        WLNP
527100940912     C                   MOVEL     ARBNRS        WNRS
527200940912     C                   MOVEL     ARBNSP        WNSP
527300940912     C*
527400940907     C* PULISCO IL SFL
527500950825     C                   SETOFF                                       787923
527600131107     C                   WRITE     LRI6C08
527700950825     C                   SETON                                        7923
527800940923     C                   MOVEL     *BLANKS       LEGAME
527900940907     C*
528000940907     C                   SETOFF                                       94
528100950718     C                   Z-ADD     *ZERO         NR8
528200940907     C                   Z-ADD     *ZERO         SAVNR8            4 0
528300940907     C                   MOVEL     *BLANKS       WFLG1             1
528400940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
528500940907     C*
528600940907     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
528700940907     C     KARB          CHAIN     FNLBL01L                           99
528800940907     C   99KARB          CHAIN     FNLBL02L                           99
528900940907     C  N99              DO
529000940907     C*
529100940907     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
529200940907     C                   MOVEL     LBLAAO        KAAS
529300940907     C                   MOVEL     LBLLPO        KLNP
529400940907     C                   MOVEL     LBLNRO        KNRS
529500940907     C                   MOVEL     LBLNSO        KNSP
529600940907     C     KLBL3         CHAIN     FNLBL02L                           99
529700950103     C     *IN99         IFEQ      *ON
529800950103     C                   MOVEL     'N'           WORIGI            1
529900950103     C                   ELSE
530000950103     C                   MOVEL     'S'           WORIGI
530100950103     C                   ENDIF
530200940907     C*
530300940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
530400940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
530500950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
530600950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
530700940907     C                   MOVEL     ' '           V8CSCE
530800940907     C                   MOVEL     LBLAAO        V8CAAP
530900940907     C                   MOVEL     LBLLPO        V8CLPP
531000940907     C                   MOVEL     LBLLAO        V8CLAP
531100940907     C                   MOVEL     LBLNRO        V8CNRP
531200940907     C                   MOVEL     LBLNSO        V8CNSP
531300940926     C                   CLEAR                   V8CAAN
531400940926     C                   CLEAR                   V8CLPN
531500940926     C                   CLEAR                   V8CLAN
531600940926     C                   CLEAR                   V8CNRN
531700940926     C                   CLEAR                   V8CNSN
531800940907     C*
531900940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
532000940907     C     LBLAAO        IFEQ      V3CAAS
532100940907     C     LBLLPO        ANDEQ     V3CLNP
532200940907     C     LBLNRO        ANDEQ     V3CNRS
532300940907     C     LBLNSO        ANDEQ     V3CNSP
532400940907     C                   MOVEL     *BLANKS       V8CPRE
532500940907     C                   SETON                                        80
532600940907     C                   SETON                                        81
532700940907     C                   MOVEL     '1'           WFLG1
532800940907     C                   ELSE
532900940907     C                   SETOFF                                       81
533000940907     C*
533100950316     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
533200950316     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
533300950316     C                   SETOFF                                       93
533400950316     C     KLBL3         SETLL     FNARB01L                               93
533500950316     C  N93              MOVEA     PRD(1)        V8CPRE
533600950316     C  N93              SETON                                        80
533700950316     C   93              MOVEA     PRD(2)        V8CPRE
533800950316     C   93              SETOFF                                       80
533900950316     C*
534000950316     C* SE NON TROVATA SU ARB PROVO SU BLP
534100950316     C  N93              DO
534200950316     C                   SETOFF                                       93
534300950316     C     KLBL3         SETLL     FNBLP01L                               93
534400010712      *
534500010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
534600010712     c                   if        *in93 = *off
534700010712     C                   MOVEA     PRD(1)        V8CPRE
534800010712     c                   else
534900950316     C* SE RICHIAMATO DALLE PARTENZE (FNLS05R) NON POSSO
535000950316     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
535100010712     C     PA1FLG        IFEQ      '3'
535200960903     C     PA1FLG        OREQ      'N'
535300950316     C                   MOVEA     PRD(3)        V8CPRE
535400950316     C                   SETON                                        80
535500950316     C                   ELSE
535600950316     C                   MOVEA     PRD(2)        V8CPRE
535700950316     C                   SETOFF                                       80
535800950316     C                   ENDIF
535900010712     C                   ENDIF
536000950316     C*
536100950316     C                   ENDDO
536200940907     C*
536300940907     C                   ENDIF
536400940907     C                   SETON                                        83
536500950103     C*
536600950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
536700950103     C                   MOVEL     *IN80         V8CI80
536800950103     C                   MOVEL     *IN81         V8CI81
536900950103     C                   MOVEL     *IN82         V8CI82
537000950103     C                   MOVEL     *IN83         V8CI83
537100950103     C*
537200940907     C                   Z-ADD     1             NR8
537300131107     C                   WRITE     LRI6S08
537400950825     C                   SETON                                        78
537500950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
537600940907     C*
537700950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
537800950103     C*                      2) NON HO TROVATO L'ORIGINALE
537900950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
538000950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
538100950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
538200950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
538300950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
538400950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
538500950103     C* L'ORIGINALE E LA MAMMA.
538600950103     C     WORIGI        IFEQ      'N'
538700950103     C                   MOVEL     *BLANKS       V8CSCE
538800950103     C                   CLEAR                   V8CAAP
538900950103     C                   CLEAR                   V8CLPP
539000950103     C                   CLEAR                   V8CLAP
539100950103     C                   CLEAR                   V8CNRP
539200950103     C                   CLEAR                   V8CNSP
539300950103     C                   MOVEL     LBLAAP        V8CAAN
539400950103     C                   MOVEL     LBLLPP        V8CLPN
539500950103     C                   MOVEL     LBLLAP        V8CLAN
539600950103     C                   MOVEL     LBLNRP        V8CNRN
539700950103     C                   MOVEL     LBLNSP        V8CNSN
539800950103     C                   SETOFF                                       8182
539900950103     C*
540000950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
540100950103     C     V8CAAN        IFEQ      V3CAAS
540200950103     C     V8CLPN        ANDEQ     V3CLNP
540300950103     C     V8CNRN        ANDEQ     V3CNRS
540400950103     C     V8CNSN        ANDEQ     V3CNSP
540500950103     C                   MOVEL     *BLANKS       V8CPRE
540600950103     C                   SETON                                        80
540700950103     C                   SETON                                        82
540800950103     C                   ELSE
540900950103     C*
541000950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
541100950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
541200950316     C                   EXSR      CHKBOL
541300950103     C*
541400950103     C                   ENDIF
541500950103     C*
541600950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
541700950103     C                   SETOFF                                       83
541800950103     C*
541900950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
542000950103     C                   MOVEL     *IN80         V8CI80
542100950103     C                   MOVEL     *IN81         V8CI81
542200950103     C                   MOVEL     *IN82         V8CI82
542300950103     C                   MOVEL     *IN83         V8CI83
542400950103     C*
542500950103     C                   ADD       1             NR8
542600131107     C                   WRITE     LRI6S08
542700950103     C*
542800950103     C                   ENDIF
542900940907     C                   SETOFF                                       83  99
543000940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
543100940907     C* CICLO FINO ALL'ULTIMO LEGAME
543200940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
543300940907     C                   Z-ADD     *ZERO         WA                3 0
543400940907     C                   Z-ADD     *ZERO         WB                3 0
543500940907     C                   MOVEA     *ZERO         AAN
543600940907     C     *IN99         DOWEQ     *OFF
543700940907     C*
543800940907     C                   MOVEL     ' '           V8CSCE
543900940907     C                   MOVEL     LBLAAP        V8CAAP
544000940907     C                   MOVEL     LBLLPP        V8CLPP
544100940907     C                   MOVEL     LBLLAP        V8CLAP
544200940907     C                   MOVEL     LBLNRP        V8CNRP
544300940907     C                   MOVEL     LBLNSP        V8CNSP
544400940907     C                   MOVEL     LBLAAN        V8CAAN
544500940907     C                   MOVEL     LBLLPN        V8CLPN
544600940907     C                   MOVEL     LBLLAN        V8CLAN
544700940907     C                   MOVEL     LBLNRN        V8CNRN
544800940907     C                   MOVEL     LBLNSN        V8CNSN
544900940907     C                   SETOFF                                       8182
545000940907     C*
545100950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
545200940907     C     LBLAAP        IFEQ      V3CAAS
545300940907     C     LBLLPP        ANDEQ     V3CLNP
545400940907     C     LBLNRP        ANDEQ     V3CNRS
545500940907     C     LBLNSP        ANDEQ     V3CNSP
545600940907     C                   SETON                                        81
545700940907     C                   ENDIF
545800940907     C*
545900940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
546000940907     C     LBLAAN        IFEQ      V3CAAS
546100940907     C     LBLLPN        ANDEQ     V3CLNP
546200940907     C     LBLNRN        ANDEQ     V3CNRS
546300940907     C     LBLNSN        ANDEQ     V3CNSP
546400940907     C                   MOVEL     *BLANKS       V8CPRE
546500940907     C                   SETON                                        80
546600940907     C                   SETON                                        82
546700940907     C                   ELSE
546800940907     C*
546900940907     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
547000940907     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
547100950316     C                   EXSR      CHKBOL
547200940907     C*
547300940907     C                   ENDIF
547400950103     C*
547500950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
547600950103     C                   MOVEL     *IN80         V8CI80
547700950103     C                   MOVEL     *IN81         V8CI81
547800950103     C                   MOVEL     *IN82         V8CI82
547900950103     C                   MOVEL     *IN83         V8CI83
548000940907     C*
548100940907     C* SCRIVO IL RECORD NEL SFL
548200940907     C                   ADD       1             NR8
548300131107     C                   WRITE     LRI6S08
548400940907     C*
548500940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
548600940907     C                   ADD       1             WA
548700940907     C                   MOVEL     LBLAAN        AAN(WA)
548800940907     C                   MOVEL     LBLLPN        LPN(WA)
548900940907     C                   MOVEL     LBLNRN        NRN(WA)
549000940907     C                   MOVEL     LBLNSN        NSN(WA)
549100940907     C*
549200940907     C     KLBL3         READE     FNLBL02L                               99
549300940907     C*
549400940907     C* SE NON HO PIU' BOLLE CON READE
549500940907     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
549600970116     C     WB            DOWLT     WA
549700970116     C     *IN99         ANDEQ     *ON
549800940907     C                   ADD       1             WB
549900940907     C                   MOVEL     AAN(WB)       KAAS
550000940907     C                   MOVEL     LPN(WB)       KLNP
550100940907     C                   MOVEL     NRN(WB)       KNRS
550200940907     C                   MOVEL     NSN(WB)       KNSP
550300940907     C     KLBL3         SETLL     FNLBL02L
550400940907     C     KLBL3         READE     FNLBL02L                               99
550500970116     C                   ENDDO
550600940907     C*
550700940907     C                   ENDDO
550800940907     C                   ENDDO
550900940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
551000940907     C*
551100940907     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
551200940907     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
551300940907     C     NR8           IFEQ      2
551400940907     C*
551500940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
551600131107     C     2             CHAIN     LRI6S08                            99
551700940907     C     V8CAAP        IFEQ      V3CAAS
551800940907     C     V8CLPP        ANDEQ     V3CLNP
551900940907     C     V8CNRP        ANDEQ     V3CNRS
552000940907     C     V8CNSP        ANDEQ     V3CNSP
552100940907     C                   MOVEL     V8CAAN        COMAAN
552200940907     C                   MOVEL     V8CLPN        COMLPN
552300940907     C                   MOVEL     V8CNRN        COMNRN
552400940907     C                   MOVEL     V8CNSN        COMNSN
552500950307     C                   MOVEL     V8CLAN        COMLAN
552600950316     C                   MOVEA     'SEGUEN.'     LBL(14)
552700940907     C                   ELSE
552800940907     C                   MOVEL     V8CAAP        COMAAN
552900940907     C                   MOVEL     V8CLPP        COMLPN
553000940907     C                   MOVEL     V8CNRP        COMNRN
553100940907     C                   MOVEL     V8CNSP        COMNSN
553200950307     C                   MOVEL     V8CLAP        COMLAN
553300950316     C                   MOVEA     'PRECED.'     LBL(14)
553400940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
553500940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
553600940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
553700131107     C     1             CHAIN     LRI6S08                            99
553800940926     C* REIMPOSTO NR8=2 PERCHE' MI SERVE DOPO
553900940926     C                   Z-ADD     2             NR8
554000940907     C                   ENDIF
554100940907     C*
554200950316     C                   MOVEA     COMLPN        LBL(21)
554300950316     C                   MOVEA     COMNRN        LBL(25)
554400950316     C                   MOVEA     COMNSN        LBL(28)
554500950316     C                   MOVEA     COMLAN        LBL(40)
554600940907     C*
554700940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
554800940907     C                   Z-ADD     1             W                 1 0
554900940907     C     V8CPRE        LOOKUP    PRD(W)                                 99
555000950307     C                   MOVEA     LBD(W)        LBL(44)
555100940907     C     W             COMP      2                                      94
555200940907     C*
555300940907     C                   MOVEA     LBL           LEGAME
555400940907     C                   ELSE
555500940907     C*
555600940907     C     NR8           IFGT      2
555700940907     C                   MOVEA     LB2           LEGAME
555800940907     C                   SETON                                        94
555900940907     C                   ENDIF
556000940907     C                   ENDIF
556100940907     C*
556200940907     C     NR8           COMP      1                                    3434
556300940907     C*
556400940907     C                   Z-ADD     NR8           SAVNR8
556500940907     C                   Z-ADD     1             NR8
556600940923     C     LEGAME        COMP      *BLANKS                            9292
556700940912     C*
556800940912     C* RIAGGANCIO LA BOLLA IN ESAME
556900940912     C                   MOVEL     WAAS          ARBAAS
557000940912     C                   MOVEL     WLNP          ARBLNP
557100940912     C                   MOVEL     WNRS          ARBNRS
557200940912     C                   MOVEL     WNSP          ARBNSP
557300940912     C     KARB2         CHAIN     FNARB01L                           99
557400930511     C*
557500930413     C                   ENDSR
557600950316     C**************************************************************************
557700950316     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
557800950316     C**************************************************************************
557900950316     C     CHKBOL        BEGSR
558000950316     C*
558100950316     C                   SETOFF                                       93
558200950316     C     KLBL1         SETLL     FNARB01L                               93
558300950316     C  N93              MOVEA     PRD(1)        V8CPRE
558400950316     C  N93              SETON                                        80
558500950316     C   93              MOVEA     PRD(2)        V8CPRE
558600950316     C   93              SETOFF                                       80
558700950316     C*
558800950316     C* SE NON TROVATA SU ARB PROVO SU BLP
558900950316     C  N93              DO
559000950316     C                   SETOFF                                       93
559100950316     C     KLBL1         SETLL     FNBLP01L                               93
559200010712      *
559300010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
559400010712     c                   if        *in93 = *off
559500010712     C                   MOVEA     PRD(1)        V8CPRE
559600010712     c                   else
559700950316     C* SE RICHIAMATO DALLE PARTENZE (FNLS05R) NON POSSO
559800950316     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
559900010712     C     PA1FLG        IFEQ      '3'
560000960903     C     PA1FLG        OREQ      'N'
560100950316     C                   MOVEA     PRD(3)        V8CPRE
560200950316     C                   SETON                                        80
560300950316     C                   ELSE
560400950316     C                   MOVEA     PRD(2)        V8CPRE
560500950316     C                   SETOFF                                       80
560600950316     C                   ENDIF
560700010712     C                   ENDIF
560800950316     C*
560900950316     C                   ENDDO
561000950316     C*
561100950316     C                   ENDSR
561200940802     C**************************************************************************
561300940802     C*    VISUALIZZA BOLLA
561400940802     C**************************************************************************
561500911023     C     VISUAL        BEGSR
561600940321      *
561700940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
561800131108     c     Emriepil      tag
561900940712     C                   EXSR      RIEPIL
562000940712      *
562100131108      * PER F15 PASSO ALLA BOLLA, PER INVIO o F12 RITORNO AL SFL
562200940712     C  NKP              GOTO      ENDVIS
562300931112      *
562400931112     C                   SETOFF                                       89
562500940802     C                   MOVEL     'S'           WRIEM             1
562600021217      *
562700021217      * Riempio il campone in fondo alla videata con i tasti scelti
562800021217     C                   exsr      Ges_TastiFun
562900021217     c                   eval      FD03 = FDxx
563000021217     c                   eval      F24D03 = F24Dxx
563100911023     C     FOR03         TAG
563200940802     C     WRIEM         IFEQ      'S'
563300131107     C                   WRITE     LRI6T01
563400940802     C                   MOVEL     *BLANKS       WRIEM
563500940802     C                   ENDIF
563600021217      *
563700021217      * Imposto la stringa comandi del 'giro' successivo
563800021217     C                   IF        *inKY = *on and $Loop > 1
563900021217     c                   Exsr      Ges_F24
564000021217     c                   eval      FD03 = FDxx
564100021217     c                   eval      F24D03 = F24Dxx
564200021217     C                   ENDIF
564300021217      *
564400131107     C                   EXFMT     LRI6D03
564500070420     c                   setoff                                       28
564600070420     c                   clear                   v1cmsg
564700931112      *
564800131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
564900931112     C   KY*IN89         IFEQ      *ON
565000931112     C                   SETOFF                                       89
565100931112     C                   ELSE
565200931112     C                   SETON                                        89
565300931112     C                   ENDIF
565400931112     C   KY              GOTO      FOR03
565500911023     C*
565600131108     C* F3 - FINE LAVORO TORNO A SUBFILE
565700940714     C   KC              MOVEL     '2'           V2CFLG            1
565800911023     C   KC              GOTO      ENDVIS
565900911023     C*
566000131108     C* F12 - FINE LAVORO tolno a videata di riepilogo
566100131108     C   KL              GOTO      emriepil
566200911023     C*
566300911024     C*
566400930517     C* CMD18 - INTERROGA LEGAME BOLLA
566500930518     C   KS              DO
566600930518     C                   EXSR      CMD18
566700940907     C                   MOVEL     'S'           WRIEM
566800930518     C                   GOTO      FOR03
566900930518     C                   END
567000930517     C*
567100940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
567200920804     C                   EXSR      COMAND
567300940905     C     WCMD          IFEQ      'S'
567400940802     C     WF03          IFEQ      *BLANKS
567500070403     c
567600920804     C                   GOTO      FOR03
567700940802     C                   ELSE
567800940802     C                   MOVEL     '2'           V2CFLG
567900940802     C                   GOTO      ENDVIS
568000940802     C                   ENDIF
568100940905     C                   ENDIF
568200911023     C*
568300911023     C* FORMATO4
568400931112     C                   SETOFF                                       89
568500940802     C                   MOVEL     'S'           WRIEM
568600021217      *
568700021217      * Riempio il campone in fondo alla videata con i tasti scelti
568800021217     C                   exsr      Ges_TastiFun
568900021217     c                   eval      FD04 = FDxx
569000021217     c                   eval      F24D04 = F24Dxx
569100911023     C     FOR04         TAG
569200940802     C     WRIEM         IFEQ      'S'
569300131107     C                   WRITE     LRI6T01
569400940802     C                   MOVEL     *BLANKS       WRIEM
569500940802     C                   ENDIF
569600021217      * Imposto la stringa comandi del 'giro' successivo
569700021217     C                   IF        *inKY = *on and $Loop > 1
569800021217     c                   Exsr      Ges_F24
569900021217     c                   eval      FD04 = FDxx
570000021217     c                   eval      F24D04 = F24Dxx
570100021217     C                   ENDIF
570200131107     C                   EXFMT     LRI6D04
570300070420     c                   setoff                                       28
570400070420     c                   clear                   v1cmsg
570500931112      *
570600131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
570700931112     C   KY*IN89         IFEQ      *ON
570800931112     C                   SETOFF                                       89
570900931112     C                   ELSE
571000931112     C                   SETON                                        89
571100931112     C                   ENDIF
571200931112     C   KY              GOTO      FOR04
571300911023     C*
571400131108     C* F12 - RITORNO VIDEATA 3
571500911023     C   KL              GOTO      FOR03
571600920804     C*
571700131108     C* F18 - INTERROGA LEGAME BOLLA
571800930518     C   KS              DO
571900930518     C                   EXSR      CMD18
572000930518     C                   GOTO      FOR04
572100930518     C                   END
572200940923     C*
572300940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
572400131120     c                   eval      wvideo='4'
572500131120     C                   EXSR      CTR_puIn
572600940923     C     WRIEM         CABEQ     'S'           FOR04
572700930517     C*
572800940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
572900920804     C                   EXSR      COMAND
573000940905     C     WCMD          IFEQ      'S'
573100940802     C     WF03          IFEQ      *BLANKS
573200940802     C                   GOTO      FOR04
573300940802     C                   ELSE
573400940802     C                   MOVEL     '2'           V2CFLG
573500940802     C                   GOTO      ENDVIS
573600940802     C                   ENDIF
573700940905     C                   ENDIF
573800911023     C*
573900911023     C* VIDEATA 5
574000931112     C                   SETOFF                                       89
574100940809     C                   MOVEL     'S'           WRIEM
574200021217      *
574300021217      * Riempio il campone in fondo alla videata con i tasti scelti
574400021217     C                   exsr      Ges_TastiFun
574500021217     c                   eval      FD05 = FDxx
574600021217     c                   eval      F24D05 = F24Dxx
574700911023     C     FOR05         TAG
574800940809     C     WRIEM         IFEQ      'S'
574900131107     C                   WRITE     LRI6T01
575000940809     C                   MOVEL     *BLANKS       WRIEM
575100940809     C                   ENDIF
575200951220     C                   Z-ADD     1             REC5
575300021217      *
575400021217      * Imposto la stringa comandi del 'giro' successivo
575500021217     C                   IF        *inKY = *on and $Loop > 1
575600021217     c                   Exsr      Ges_F24
575700021217     c                   eval      FD05 = FDxx
575800021217     c                   eval      F24D05 = F24Dxx
575900021217     C                   ENDIF
576000131107     C                   WRITE     LRI6Z05
576100131107     C                   EXFMT     LRI6C05
576200070420     c                   setoff                                       28
576300070420     c                   clear                   v1cmsg
576400931112      *
576500131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
576600931112     C   KY*IN89         IFEQ      *ON
576700931112     C                   SETOFF                                       89
576800931112     C                   ELSE
576900931112     C                   SETON                                        89
577000931112     C                   ENDIF
577100931112     C   KY              GOTO      FOR05
577200911023     C*
577300131108     C* F12 - RITORNO VIDEATA 2
577400911023     C   KL              GOTO      FOR04
577500920804     C*
577600940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
577700920804     C                   EXSR      COMAND
577800940905     C     WCMD          IFEQ      'S'
577900940802     C     WF03          IFEQ      *BLANKS
578000940802     C                   GOTO      FOR05
578100940802     C                   ELSE
578200131108     C                   MOVEL     '2'           V2CFLG
578300131108     C                   GOTO      ENDVIS
578400940802     C                   ENDIF
578500070406
578600070406     c                   else
578700070406     c
578800070406     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
578900070406     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
579000070406     c                   if        v3cgesd='?'
579100070406     c                   exsr      VISUALAbi
579200070406     c                   clear                   v3cgesd
579300070406     c                   eval      wriem='S'
579400070406     C                   GOTO      FOR05
579500070406     c                   endif
579600940905     C                   ENDIF
579700131108     c
579800131108     C                   GOTO      emriepil
579900931112      *
580000911023     C     ENDVIS        ENDSR
580100070403     C**************************************************************************
580200070403     C*    VISUALIZZA Abilitazioni
580300070403     C**************************************************************************
580400070403     C     VISUALAbi     BEGSR
580500070403     c*
580600131120     c                   clear                   fnlrg7ds
580700131120     c                   move      v3cdsp        ilrg7aas
580800131120     c                   eval      ilrg7lnp=v3clnp
580900131120     c                   eval      ilrg7nrs=v3cnrs
581000131120     c                   eval      ilrg7nsp=v3cnsp
581100131120     c                   eval      ilrg7vsdet='?'
581200131120     c                   eval      ilrg7lung=58
581300131120     c                   call      'FNLRG7R'
581400131120     c                   parm                    fnlrg7ds
581500070403     c
581600070403     C                   ENDSR
581700940712     C**************************************************************************
581800940802     C*    VISUALIZZA BOLLA PRECEDENTE
581900940802     C**************************************************************************
582000930510     C     VISUA2        BEGSR
582100940321      *
582200940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
582300131108     c     Emriepi2      tag
582400940712     C                   EXSR      RIEPI2
582500940712      *
582600131108      * PER F15 PASSO ALLA BOLLA, PER INVIO RITORNO AL SFL
582700940713     C  NKP              GOTO      ENDV8C
582800931112      *
582900931112     C                   SETOFF                                       89
583000940809     C                   MOVEL     'S'           WRIEM
583100021217      *
583200021217      * Riempio il campone in fondo alla videata con i tasti scelti
583300021217     C                   exsr      Ges_TastiFun
583400070516     c                   eval      FD03 = FDxx
583500070516     c                   eval      F24D03 = F24Dxx
583600930510     C     FOR033        TAG
583700940809     C     WRIEM         IFEQ      'S'
583800131107     C                   WRITE     LRI6T01
583900940809     C                   MOVEL     *BLANKS       WRIEM
584000940809     C                   ENDIF
584100021217      *
584200021217      * Imposto la stringa comandi del 'giro' successivo
584300021217     C                   IF        *inKY = *on and $Loop > 1
584400021217     c                   Exsr      Ges_F24
584500021217     c                   eval      FD03 = FDxx
584600021217     c                   eval      F24D03 = F24Dxx
584700021217     C                   ENDIF
584800131107     C                   EXFMT     LRI6D03
584900070420     c                   setoff                                       28
585000070420     c                   clear                   v1cmsg
585100931112      *
585200131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
585300931112     C   KY*IN89         IFEQ      *ON
585400931112     C                   SETOFF                                       89
585500931112     C                   ELSE
585600931112     C                   SETON                                        89
585700931112     C                   ENDIF
585800931112     C   KY              GOTO      FOR033
585900930510     C*
586000131108     C* F12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
586100930510     C   KC
586200131108     COR KL              GOTO      Emriepi2
586300930510     C*
586400940905     C* CONTROLLO SE PREMUTI TASTI FUNZIONALI E, NEL CASO, RIEMETTO
586500930510     C                   EXSR      COMAND
586600940905     C     WCMD          IFEQ      'S'
586700930510     C                   GOTO      FOR033
586800930510     C                   END
586900931112      *
587000930510     C* FORMATO4
587100931112     C                   SETOFF                                       89
587200940809     C                   MOVEL     'S'           WRIEM
587300021217      *
587400021217      * Riempio il campone in fondo alla videata con i tasti scelti
587500021217     C                   exsr      Ges_TastiFun
587600021217     c                   eval      FD04 = FDxx
587700021217     c                   eval      F24D04 = F24Dxx
587800930510     C     FOR044        TAG
587900940809     C     WRIEM         IFEQ      'S'
588000131107     C                   WRITE     LRI6T01
588100940809     C                   MOVEL     *BLANKS       WRIEM
588200940809     C                   ENDIF
588300021217      *
588400021217      * Imposto la stringa comandi del 'giro' successivo
588500021217     C                   IF        *inKY = *on and $Loop > 1
588600021217     c                   Exsr      Ges_F24
588700021217     c                   eval      FD04 = FDxx
588800021217     c                   eval      F24D04 = F24Dxx
588900021217     C                   ENDIF
589000131107     C                   EXFMT     LRI6D04
589100070420     c                   setoff                                       28
589200070420     c                   clear                   v1cmsg
589300931112      *
589400931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
589500931112     C   KY*IN89         IFEQ      *ON
589600931112     C                   SETOFF                                       89
589700931112     C                   ELSE
589800931112     C                   SETON                                        89
589900931112     C                   ENDIF
590000931112     C   KY              GOTO      FOR044
590100930510     C*
590200930510     C* CMD12 - RITORNO VIDEATA 3
590300930510     C   KL              GOTO      FOR033
590400940923     C*
590500940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
590600131120     c                   eval      wvideo='4'
590700131120     C                   EXSR      CTR_puIn
590800940923     C     WRIEM         CABEQ     'S'           FOR044
590900930510     C*
591000940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
591100930510     C                   EXSR      COMAND
591200940905     C     WCMD          IFEQ      'S'
591300930510     C                   GOTO      FOR044
591400940905     C                   ENDIF
591500931112      *
591600930510     C* VIDEATA 5
591700931112     C                   SETOFF                                       89
591800940809     C                   MOVEL     'S'           WRIEM
591900021217      *
592000021217      * Riempio il campone in fondo alla videata con i tasti scelti
592100021217     C                   exsr      Ges_TastiFun
592200021217     c                   eval      FD05 = FDxx
592300021217     c                   eval      F24D05 = F24Dxx
592400930510     C     FOR055        TAG
592500940809     C     WRIEM         IFEQ      'S'
592600131107     C                   WRITE     LRI6T01
592700940809     C                   MOVEL     *BLANKS       WRIEM
592800940809     C                   ENDIF
592900951220     C                   Z-ADD     1             REC5
593000021217      *
593100021217      * Imposto la stringa comandi del 'giro' successivo
593200021217     C                   IF        *inKY = *on and $Loop > 1
593300021217     c                   Exsr      Ges_F24
593400021217     c                   eval      FD05 = FDxx
593500021217     c                   eval      F24D05 = F24Dxx
593600021217     C                   ENDIF
593700131107     C                   WRITE     LRI6Z05
593800131107     C                   EXFMT     LRI6C05
593900070420     c                   setoff                                       28
594000070420     c                   clear                   v1cmsg
594100931112      *
594200931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
594300931112     C   KY*IN89         IFEQ      *ON
594400931112     C                   SETOFF                                       89
594500931112     C                   ELSE
594600931112     C                   SETON                                        89
594700931112     C                   ENDIF
594800931112     C   KY              GOTO      FOR055
594900930510     C*
595000930510     C* CMD12 - RITORNO VIDEATA 2
595100930510     C   KL              GOTO      FOR044
595200930510     C*
595300940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
595400940905     C                   EXSR      COMAND
595500940905     C     WCMD          IFEQ      'S'
595600930510     C                   GOTO      FOR055
595700930510     C                   END
595800131108
595900131108     C                   GOTO      Emriepi2
596000931112      *
596100940713     C     ENDV8C        ENDSR
596200940802     C**************************************************************************
596300940802     C* TASTI DI FUNZIONE
596400940802     C**************************************************************************
596500920804     C     COMAND        BEGSR
596600940905     C*
596700940905     C* PULISCO IL FLAG CHE INDICA SE PREMUTO UN TASTO FUNZIONE
596800940905     C                   MOVE      *BLANKS       WCMD              1
596900040218      *
597000070411      *?F1 - Aggiunta bolla in reclamo?o scelta bolla generica
597100040218if  1c                   if        *inKA   = *on
597200070423     C                   Z-ADD     arbLNP        COMLNP
597300070411     C                   MOVEL     '3'           V2CFLG
597400070411      *?-
597500070411if  2c                   if        SDSprm  > 1
597600131111     c                   eval      wARBlnp = ARBlnp
597700131111     c                   eval      wARBnrs = ARBnrs
597800131111     c                   eval      wARBnsp = ARBnsp
597900131111     c                   eval      wARBaas = ARBaas
598000040218     c                   clear                   FIDNA1ds
598100040218     c                   eval      IA1ins  = 'S'
598200040218     c                   eval      IA1tor  = 'S'
598300040218     c                   eval      IA1ogg  = ds_OggSped
598400070411    2c                   endif
598500040218     c                   goto      Fine
598600040218e   1c                   endif
598700940802     C*
598800990119     C* F5 - INTERROGAZIONE EVENTI
598900040218do  1C   KE              DO
599000940905     C                   MOVEL     'S'           WCMD
599100940726     C                   MOVEL     V2CAAS        PB5AAS
599200940726     C                   MOVEL     COMLNP        PB5LNP
599300940726     C                   MOVEL     V2CNRS        PB5NRS
599400940726     C                   MOVEL     V2CNSP        PB5NSP
599500940726     C                   MOVEL     *BLANKS       PB5F03
599600940726     C                   MOVEL     *BLANKS       PB5FLG
599700940802     C                   MOVEL     *BLANKS       PB5PGM
599800131107     C                   MOVEL     'FNLRI6R'     PB5PGM
599900940726     C                   MOVEL     V1CDSA        PB5DSA
600000940726     C                   MOVEL     V3CDSP        PB5DSP
600100940726     C                   MOVEL     VIDRMN        PB5RMN
600200940726     C                   MOVEL     V3CLNA        PB5LNA
600300940802     C                   MOVEL     V3DLNA        PB5LN1
600400940726     C                   MOVEL     V4CRSD        PB5RSD
600500940726     C                   MOVEL     VIDDCP        PB5DCP
600600941031     C                   MOVEL     VIDTMC        PB5TMC
600700940726     C                   MOVEL     PARAB5        KPJBU
600800940722     C                   CALL      'FNLRB5R'
600900930525     C                   PARM                    KPJBA
601000941109     C                   MOVEL     KPJBU         PARAB5
601100941109     C                   MOVEL     PB5F03        WF03
601200941214     C                   MOVEL     '1'           PA0B5
601300940725     C                   MOVEL     'S'           WRIEM
601400040218e   1C                   ENDDO
601500070420      *?F6 - Stampa LDV
601600070420if  1c                   if        *inKf = *on
601700070420     C                   MOVEL     'S'           WCMD
601800080201     c* Se assegnato non abilitato non stampo la LdV ma emetto errore
601900080201     c                   if        arbaca=' '
602000080201     C                   MOVEL     MSG(31)       V1CMSG
602100080201     C                   SETON                                        28
602200080201     c                   else
602300070420     c
602400070420     c                   if        wrichtrul90=' '
602500070420     c* La prima volta devo proporre la richiesta stampanti
602600070420     c                   exsr      RICHSTAMPAN
602700070420     C                   MOVEL     'S'           WRIEM
602800070420     c
602900070420     c* Se premuto F3 esco senza fare nulla
603000070420     c                   if        d90f3<>'1' and not *in91
603100070420     c                   eval      wrichtrul90='S'
603200070420     c                   endif
603300070420     c                   if        *in91
603400070420     C                   MOVEL     MSG(30)       V1CMSG
603500070420     C                   SETON                                        28
603600070420     c                   endif
603700070420     c                   endif
603800070420     c
603900070420     c                   if        wrichtrul90='S'
604000070420     C                   CLEAR                   FNLSB5DS
604100070426     c                   movel     'L'           db0tla
604200070420     C                   MOVEL     'R'           DB0RIS
604300070426     C                   MOVEL     'A'           DB0TBO
604400070420     C                   MOVEL     ARBAAS        DB0AAS
604500070420     C                   MOVEL     ARBLNP        DB0LNP
604600070420     C                   MOVEL     ARBNRS        DB0NRS
604700070420     C                   MOVEL     ARBNSP        DB0NSP
604800130319     c                   reset                   fnlsB6ds1
604900070420     C                   CALL      D90PSL
605000070420     C                   PARM                    FNLSB5DS
605100130308     c                   parm                    fnlsB6ds1
605200070420     C                   MOVEL     MSG(29)       V1CMSG
605300070420     C                   SETON                                        28
605400070420     c                   endif
605500080201     c                   endif
605600070420     c                   endif
605700930525     C*
605800990119     C* F7 - ANOMALIE
605900040218do  1C   KG              DO
606000960523     C                   MOVEL     'S'           WCMD
606100960523     C*
606200991027     C                   CLEAR                   FNLr44DS
606300960523     C                   MOVE      V2CAAS        D44AAS
606400960523     C                   MOVE      COMLNP        D44FIL
606500960523     C                   MOVE      V2CNRS        D44NRS
606600960523     C                   MOVE      V2CNSP        D44NSP
606700960523     C                   MOVEL     'A'           D44TBO
606800960523     C                   MOVE      'I'           D44MOD
606900991027     C                   MOVEL     FNLr44DS      KPJBU
607000960523     C                   CALL      'FNLR44R'
607100960523     C                   PARM                    KPJBA
607200960523     C*
607300990119     C                   MOVEL     'S'           WRIEM
607400040218e   1C                   ENDDO
607500920804     C*
607600990119     C* F8  - INTERROGAZIONE EVENTI SEGNACOLLI
607700040218if  1C     *INKH         IFEQ      *ON
607800961121     C                   MOVEL     'S'           WCMD
607900961121     C*
608000991027     C                   CLEAR                   FNLR45DS
608100961121     C                   MOVE      V2CAAS        D45AAS
608200961121     C                   MOVE      COMLNP        D45FIL
608300961121     C                   MOVE      V2CNRS        D45NRS
608400961121     C                   MOVE      V2CNSP        D45NSP
608500991027     C                   MOVEL     FNLR45DS      KPJBU
608600961121     C                   CALL      'FNLR45R'
608700961121     C                   PARM                    KPJBA
608800961121     C*
608900990119     C                   MOVEL     'S'           WRIEM
609000040218e   1C                   ENDIF
609100130830     c
609200130830     c* F9 - Orari servizi
609300131007     c                   if        *inki=*on
609400130830     C                   MOVEL     'S'           WCMD
609500131007     c                   if        wabif9='S'
609600130930     c                   exsr      sr_orarisr
609700131007     c                   endif
609800130830     C                   MOVEL     'S'           WRIEM
609900130830e   1C                   ENDIF
610000021213     C*
610100021213     C* F11 - Note supermercati
610200040218if  1C     *INKK         IFEQ      *ON
610300021213     C     *IN77         ANDEQ     *ON
610400021213      * imposto i campi 'bolla' della ds
610500021217     C                   MOVEL     'S'           WCMD
610600021213     C                   clear                   FnLrb3ds
610700030122     c                   Movel     V1cdsa        Rb3Rsu
610800030121     C                   z-add     SimFel        RB3Fil
610900021216     C                   z-add     V3CAAS        RB3AAS
611000030121     C                   z-add     V3CLNP        RB3LNP
611100021216     C                   z-add     V3CNRS        RB3NRS
611200021216     C                   z-add     V3CNSP        RB3NSP
611300021213     C                   z-add     V3CDSP        RB3DSP
611400021213     C                   z-add     VIDRMN        RB3RMN
611500021213     C                   z-add     V3CLNA        RB3LNA
611600131108     C                   movel     V32RSD        RB3RSD
611700021218     C                   z-add     VIDPKF        RB3PKF
611800021213     C                   z-add     VIDNCL        RB3NCL
611900021213     C                   z-add     VIDVLF        RB3VLB
612000030703      * bancali - part.varie "B"
612100040218If  2c                   If        %subst(ArbGva:1:1) = 'B'
612200030121     C                   z-add     §Ar5Tba       RB3Tba
612300030121     C                   z-add     VidNba        RB3Nba
612400030121     C                   z-add     §Ar5Tb2       RB3Tb2
612500030121     C                   z-add     VidNb2        RB3Nb2
612600030703     c                   Movel     V4cGva        Rb3Gva
612700040218e   2c                   EndIf
612800030703      * bancali - part.varie "O"
612900040218If  2c                   If        %subst(ArbGva:1:1) = 'O'
613000030703     C                   z-add     §Ar5bTba      RB3Tba
613100030703     C                   z-add     VidNba        RB3Nba
613200030703     C                   z-add     §Ar5bTb2      RB3Tb2
613300030703     C                   z-add     VidNb2        RB3Nb2
613400030703     c                   Movel     V4cGva        Rb3Gva
613500030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
613600030703     c                   Z-Add     §Ar5bCor      Rb3Cor
613700040218e   2c                   EndIf
613800021213      * richiamo PGM per note supermercati
613900021213     C                   call      'FNLRB3R'
614000030122     C                   PARM                    Kpjba
614100030121     C                   PARM                    Fnlrb3Ds
614200021213      *
614300021217     C                   MOVEL     'S'           WRIEM
614400040218e   1C                   ENDIF
614500961121     C*
614600990119     C* F14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
614700040218if  1C     *INKN         IFEQ      *ON
614800930624     C     *IN03         ANDEQ     *ON
614900941213     C     PA1GIA        ANDEQ     *BLANK
615000940905     C                   MOVEL     'S'           WCMD
615100991027     C                   CLEAR                   FNLG30DS
615200941213     C                   MOVEL     'A'           D30POA
615300941213     C                   MOVEL     V2CAAS        D30AAS
615400941213     C                   MOVEL     COMLNP        D30LNP
615500941213     C                   MOVEL     V2CNRS        D30NRS
615600941213     C                   MOVEL     V2CNSP        D30NSP
615700991027     C                   MOVEL     FNLG30DS      KPJBU
615800941213     C                   CALL      'FNLG31C'
615900920804     C                   PARM                    KPJBA
616000930624     C*
616100990119     C                   MOVEL     'S'           WRIEM
616200040218e   1C                   ENDIF
616300040218      *
616400040218      *?F16 - Gestione reclami?
616500040218if  1c                   if        *inKQ = *on
616600040225     c                   eval      wARBaas = ARBaas
616700131111     c                   eval      wARBlnp = ARBlnp
616800131111     c                   eval      wARBnrs = ARBnrs
616900131111     c                   eval      wARBnsp = ARBnsp
617000040225     c                   reset                   FIDNA2ds
617100040225if  2c                   if        SDSprm  > 1
617200040225     c                   eval      IA2mod  = 'I'
617300040225e   2c                   endif
617400040225     c                   eval      IA2ogg  = ds_OggSped
617500040225     c                   call      'FIDNA2R'
617600040225     c                   parm                    KPJBA
617700040225     c                   parm                    FIDNA2ds
617800040225     c                   movel     OA2f03        WF03
617900040225     c                   movel     'S'           WRIEM
618000051201     C                   MOVEL     'S'           WCMD
618100040218e   1c                   endif
618200070404      *
618300140311      *?F17 - Note AUT
618400070404if  1c                   if        *inKR = *on
618500140123     c                   clear                   fnlrn6ds
618600140123     c                   eval      in6mod='V'
618700140123     c                   eval      in6aas=v2caas
618800140123     c                   eval      in6lnp=comlnp
618900140123     c                   eval      in6nrs=v2cnrs
619000140123     c                   eval      in6nsp=v2cnsp
619100140123      * se sono una spedizione in consegna passo i dati della distinta del file ARB
619200140123     c                   IF        ARBfbc = *blanks and
619300140123     c                               ARBddc > 0 and
619400140123     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
619500140123     c                               arbica='R' or arbicc=' ' or arbicc='R')
619600140123     c     KSPED         chain     Fnarb01l
619700140123     c                   If        %found(Fnarb01l)
619800140123     c                   eval      in6fgs = arbifp
619900140123     c                   eval      in6nfv = arbndc
620000140123     c                   eval      in6dfv = arbddc
620100140123     c                   eval      in6pdr = arbpdc
620200140123     c                   eval      in6cons = 'S'
620300140123     c                   Endif
620400140123     c                   Endif
620500140123     c                   call      'FNLRN6R'
620600070404     c                   parm                    kpjba
620700140123     c                   parm                    fnlrn6ds
620800070404     c                   movel     'S'           WRIEM
620900070404     C                   MOVEL     'S'           WCMD
621000070404e   1c                   endif
621100920804     C*
621200990119     C* F21 - DETTAGLIO SEGNACOLLI
621300040218do  1C   KV              DO
621400940905     C                   MOVEL     'S'           WCMD
621500950628     C                   MOVEL     'A'           PA1TBO
621600950629     C                   CLEAR                   PA1F03
621700940720     C                   MOVEL     PARAM1        KPJBU
621800940726     C                   CALL      'FNLR37R'
621900920804     C                   PARM                    KPJBA
622000940802     C                   MOVEL     KPJBU         PARAM1
622100940802     C                   MOVEL     PA1F03        WF03
622200990119     C                   MOVEL     'S'           WRIEM
622300040218e   1C                   ENDDO
622400920804     C*
622500990119     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
622600040218do  1C   KJ              DO
622700940905     C                   MOVEL     'S'           WCMD
622800950628     C                   MOVEL     'A'           PA1TBO
622900950629     C                   CLEAR                   PA1F03
623000940712     C                   MOVEL     PARAM1        KPJBU
623100940726     C                   CALL      'FNLR75R'
623200920804     C                   PARM                    KPJBA
623300941214     C                   MOVEL     '1'           PA075
623400940802     C                   MOVEL     KPJBU         PARAM1
623500940802     C                   MOVEL     PA1F03        WF03
623600990119     C                   MOVEL     'S'           WRIEM
623700040218e   1C                   ENDDO
623800930114     C*
623900990119     C* F19 - INTERROGA SPUNTE VARI
624000040218if  1C     *INKT         IFEQ      *ON
624100940905     C                   MOVEL     'S'           WCMD
624200940713     C                   Z-ADD     0             PA4NFV
624300940713     C                   MOVEL     9             PA4NPG
624400940713     C                   MOVEL     99            PA4NPS
624500101202     C                   MOVEL     pa1pgm        PA4PGM
624600101203     C                   MOVEL     '1'           PA4FLG
624700950214     C                   MOVEL     V3CFLS        PA4LNP
624800940719     C                   MOVEL     V3CLNA        PA4LNA
624900940719     C                   MOVEL     V3CNRS        PA4NRS
625000940713     C                   Z-ADD     0             PA4NCD
625100940713     C                   Z-ADD     0             PA4NCA
625200101201     c                   eval      pa4boan=v2caas
625300101201     c                   eval      pa4bolp=comlnp
625400101201     c                   eval      pa4bors=v2cnrs
625500101201     c                   eval      pa4bosp=v2cnsp
625600040218if  2C     VIDNCD        IFGT      0
625700940713     C                   MOVEL     VIDNCD        PA4NCD
625800940713     C                   MOVEL     VIDNCA        PA4NCA
625900101201x   2C**                 ELSE
626000930705     C* CERCO I SEGNACOLLI DAL DETTAGLIO
626100101201     C**   KARB2         SETLL     FNART01L
626200101201     C**   KARB2         READE     FNART01L                               31
626300101201do  3C*+N31              DO
626400101201     C**                 Z-ADD     ARTNSC        PA4NCD
626500101201     C**   KARB2         SETGT     FNART01L
626600101201     C**   KARB2         READPE    FNART01L                               31
626700101201if  4C**N31ARTNSC        IFNE      PA4NCD
626800101201     C**                 MOVEL     ARTNSC        PA4NCA
626900101201e   4C**                 ENDIF
627000101201e   3C**                 ENDDO
627100101201e   2C                   ENDIF
627200930705     C*
627300940802     C                   MOVEL     PARA04        KPJBU
627400940722     C                   CALL      'FNLR04R'
627500920804     C                   PARM                    KPJBA
627600940725     C                   MOVEL     'S'           WRIEM
627700060130     C                   MOVEL     'S'           WCMD
627800040218e   1C                   ENDIF
627900930415     C*
628000990119     C* F23 - VISUALIZZA DATA CONSEGNA PREVISTA ESTERNA E ORE RITARDO
628100131121do  1C** KX              DO
628200131121     C**                 MOVEL     'S'           WCMD
628300131121     C**                 SETOFF                                       68
628400930504     C*
628500131121     C**                 MOVEL     ' '           D98TLA
628600131121     C**                 MOVEL     'A'           D98TBO
628700131121     C**                 MOVEL     ARBAAS        D98AAS
628800131121     C**                 MOVEL     ARBLNP        D98LNP
628900131121     C**                 MOVEL     ARBNRS        D98NRS
629000131121     C**                 MOVEL     ARBNSP        D98NSP
629100930504     C*
629200131121     C**                 CALL      'TNSD99R'
629300131121     C**                 PARM                    TNSD99DS
629400930504     C*
629500131121     c**                 if        d98dce>0
629600131121     c**   *iso          movel     d98dce        dataeur
629700131121     c**                 movel     dataeur       datadmy
629800131121     c**                 movel     dataeur       v08dce
629900131121     c**                 movel     datadmy       v06dce
630000131121     c**                 else
630100131121     c**                 clear                   v08dce
630200131121     c**                 clear                   v06dce
630300131121     c**                 endif
630400070404     c
630500131121     C**                 Z-ADD     D98NRC        VIDNRC
630600930526     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
630700930526     C*   CONSEGNA PREVISTA
630800131121     c**n69              eval      vidhmc=%editw(d98hce: '0 :  ')
630900930504     C*
631000930504     C* SE COMODO = 1  E' STATA FATTA CALL PGM
631100131121     C**                 MOVEL     '1'           PA098
631200131121e   1C**                 ENDDO
631300931112      *
631400990119      * F20 - INTERROGAZIONE AFFIDABILITA'
631500040218do  1C   KU              DO
631600940905     C                   MOVEL     'S'           WCMD
631700950102     C                   CLEAR                   FNLR97
631800950102     C                   MOVEL     V2CAAS        R97AAS
631900950102     C                   MOVEL     COMLNP        R97LNP
632000950102     C                   MOVEL     V2CNRS        R97NRS
632100950102     C                   MOVEL     V2CNSP        R97NSP
632200950102     C                   MOVEL     V3CDSP        R97DSP
632300950102     C                   MOVEL     VIDRMN        R97RMN
632400950102     C                   MOVEL     V3CLNA        R97LNA
632500950102     C                   MOVEL     V3DLNA        R97DLA
632600950102     C                   Z-ADD     VIDDCP        R97DCP
632700950102     C                   MOVEL     V4CRSD        R97RSD
632800950102     C                   MOVEL     V1CDSA        R97RSU
632900040121     C                   MOVEL     *blanks       R97CML
633000950102     C                   MOVEL     'A'           R97AP
633100950102     C   95              MOVEL     'S'           R97EVE
633200070404     C                   Z-ADD     w08DBR        R97DBR
633300070404     C                   Z-ADD     w08DPC        R97DPC
633400070404     C                   Z-ADD     w08DUC        R97DUC
633500950102     C                   MOVEL     VIDTMC        R97TMC
633600950102     C                   Z-ADD     VIDCAS        R97CAS
633700950102     C                   MOVEL     VIDVCA        R97VCA
633800931112     C                   MOVE      *BLANKS       KPJBU
633900950102     C                   MOVEL     FNLR97        KPJBU
634000940722     C                   CALL      'FNLR97R'                            90
634100931112     C                   PARM                    KPJBA
634200950102     C                   MOVEL     KPJBU         FNLR97
634300950102     C                   MOVEL     R97F03        WF03
634400950102     C                   MOVEL     'S'           WRIEM
634500040218e   1C                   ENDDO
634600110930     C* F22 - Perfect Order
634700110929do  1C                   if        *inkw
634800110929     C                   MOVEL     'S'           WCMD
634900110929     c                   movel     param1        s_param1
635000110929     c*
635100110929     c                   MOVEL     'W'           pa1flg
635200131107     c                   MOVEL(p)  'FNLRI6R'     pa1pgm
635300111005     c                   movel(p)  vidrma        pa1rma
635400111005     c                   movel(p)  vidxco        pa1xco
635500110929     c                   movel     paraaz        kpjbu
635600110929     c                   movel     param1        kpjbu
635700131127     c                   call      'FNLR36C'
635800110929     c                   parm                    kpjba
635900110929     c*
636000110929     c                   movel     s_param1      param1
636100110929     C*
636200110929     C                   MOVEL     'S'           WRIEM
636300110929e   1C                   ENDif
636400940905     C*
636500131107     C* ? FORMATO VIDEO:  LRI6D06  (TASSAZIONE 1a BOLLA)   ---   TASTO F13 ?
636600991026     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
636700040218if  1C     *INKM         IFEQ      *ON
636800991027     C                   eval      WCMD = 'S'
636900991027     C                   EVAL      *in89 = *off
637000131107     C                   WRITE     LRI6T01
637100131107     C                   WRITE     LRI6D06
637200991027     C                   eval      NR6 = 1
637300131107     C                   EXFMT     LRI6C06
637400040218e   1C                   ENDIF
637500991027     C*
637600131107     C* ? FORMATO VIDEO:  LRI6D09  (TASSAZIONE 2a BOLLA)   ---   TASTO F04 ?
637700991027     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
637800040218if  1C     *INKD         IFEQ      *ON
637900991027     C                   eval      WCMD = 'S'
638000991027     C                   EVAL      *in89 = *off
638100131107     C                   WRITE     LRI6T01
638200131107     C                   EXFMT     LRI6D09
638300040218e   1C                   ENDIF
638400990119     C*
638500990119     C* F2 - INTERROGAZIONE COMUNICAZIONE ANOMALIA
638600990129     C*      NON RICHIAMO SE RICHIAMATO DA INTERROGAZINE C.A.
638700040218if  1C     *INKB         IFEQ      *ON
638800990129     C     PA1FLG        ANDNE     'C'
638900040218if  2C     CONTCA        IFEQ      1
639000991027     C                   CLEAR                   FIDN00DS
639100990119     C                   Z-ADD     VHCAAC        I00AAC
639200990119     C                   Z-ADD     VHCFIL        I00FIL
639300990119     C                   Z-ADD     VHCNCA        I00NCA
639400990119     C                   Z-ADD     ARBAAS        I00AAS
639500990119     C                   Z-ADD     ARBNRS        I00NRS
639600990119     C                   Z-ADD     ARBNSP        I00NSP
639700990119     C                   Z-ADD     ARBLNP        I00LNP
639800990119     C                   MOVEL     'A'           I00TPB
639900991027     C                   MOVEL     FIDN00DS      KPJBU
640000990119     C                   CALL      'FIDN01R'
640100990119     C                   PARM                    KPJBA
640200000309     C                   PARM                    callricors        1
640300040218x   2C                   ELSE
640400990119     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
640500990119     C                   SETOFF                                       79
640600131107     C                   WRITE     LRI6C11
640700990119     C                   SETON                                        797823
640800990119     C                   CLEAR                   NRR11
640900040809      *
641000040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
641100040809      * legate alla spedizione che passo
641200040809     c                   clear                   fidn12ds
641300040809     c                   eval      i12aas = arbAAS
641400040809     c                   eval      i12lnp = arbLNP
641500040809     c                   eval      i12nrs = arbNRS
641600040809     c                   eval      i12nsp = arbNSP
641700040809     c                   eval      i12fel = 'E'
641800040809     c                   eval      i12fan = 'E'
641900040809     c*
642000040809     c                   call      'FIDN12R'
642100040809     c                   parm                    fidn12ds
642200040809     c*
642300040809     c                   setoff                                       31
642400040809     c*
642500040809     c* se non ci sono errori
642600040809     c                   if        o12err <> ' '
642700040809     c                   seton                                        31
642800040809     c                   else
642900040809     c* se numero di CA trovate maggiore di zero
643000040809     c                   if        o12nca = 0
643100040809     c                   seton                                        31
643200040809     c                   else
643300040809     c                   z-add     1             ii                2 0
643400040809     c                   dow       ii <= o12nca and *in31 = *off
643500040809     c                   movea     skkey(ii)     dskey
643600040809     C                   Z-ADD     dsaac         V11AAC
643700040809     C                   Z-ADD     dsfil         V11FIL
643800040809     C                   Z-ADD     dsnca         V11NCA
643900040809     C                   Z-ADD     skdca(ii)     G02INV
644000040809     C                   Z-ADD     *ZERO         G02DAT
644100040809     C                   MOVEL     '3'           G02ERR
644200040809     C                   CALL      'XSRDA8'
644300040809     C                   PARM                    WLBDAT
644400040809     C                   Z-ADD     G02DAT        V11DCA
644500040809     C                   Z-ADD     skfca(ii)     V11FCA
644600040809     C     skfca(ii)     CHAIN     FNDFA01L
644700040809     C                   IF        %found(FNDFA01L)
644800040809     C                   MOVEL     DFADES        V11DFA
644900040809     C                   ENDIF
645000040809     C**
645100040809     C                   ADD       1             NRR11             4 0
645200131107     C                   WRITE     LRI6S11
645300040809     c*
645400040809     c                   add       1             ii
645500040809     c                   enddo
645600040809     c*
645700040809     c                   seton                                        31
645800040809     c*
645900040809     c                   endif
646000040809     c                   endif
646100040809     c**
646200990119     C* EMETTO FORMATO
646300990119     C     FOR11         TAG
646400990119     C                   Z-ADD     1             NRR11
646500131107     C                   WRITE     LRI6T01
646600131107     C                   WRITE     LRI6Z11
646700131107     C                   EXFMT     LRI6C11
646800990119     C* F12 - RITORNO
646900040218if  3C     *INKL         IFEQ      *OFF
647000131107     C                   READC     LRI6S11                                31
647100040218do  4C     *IN31         DOWEQ     *OFF
647200040218if  5C     V11SCE        IFEQ      '5'
647300991027     C                   CLEAR                   FIDN00DS
647400990119     C                   Z-ADD     V11AAC        I00AAC
647500990119     C                   Z-ADD     V11FIL        I00FIL
647600990119     C                   Z-ADD     V11NCA        I00NCA
647700990119     C                   Z-ADD     ARBAAS        I00AAS
647800990119     C                   Z-ADD     ARBNRS        I00NRS
647900990119     C                   Z-ADD     ARBNSP        I00NSP
648000990119     C                   Z-ADD     ARBLNP        I00LNP
648100990119     C                   MOVEL     'A'           I00TPB
648200991027     C                   MOVEL     FIDN00DS      KPJBU
648300990119     C                   CALL      'FIDN01R'
648400990119     C                   PARM                    KPJBA
648500000309     C                   PARM                    callricors        1
648600990119     C**
648700990119     C                   CLEAR                   V11SCE
648800131107     C                   UPDATE    LRI6S11
648900040218e   5C                   ENDIF
649000990119     C**
649100131107     C                   READC     LRI6S11                                31
649200040218e   4C                   ENDDO
649300990119     C                   GOTO      FOR11
649400040218e   3C                   ENDIF
649500040218e   2C                   ENDIF
649600990119     C*
649700990119     C                   MOVEL     'S'           WRIEM
649800990119     C                   MOVEL     'S'           WCMD
649900040218e   1C                   ENDIF
650000931112      *
650100920804     C                   ENDSR
650200070420     C**************************************************************************
650300070420      * RICHIEDO LE STAMPANTI per stampa LDV
650400070420     C**************************************************************************
650500070420     c     RICHSTAMPAN   BEGSR
650600070420     C                   CLEAR                   TRUL90DS
650700070420      *
650800070420     C                   MOVEL     'S'           d90RSB
650900070420     C                   CALL      'TRUL90R'
651000070420     C                   PARM                    KPJBA
651100070420     C                   PARM                    trul90DS
651200070420      *
651300070420     C* F3 - FINE
651400070420     c                   if        d90f3<>'1'
651500070420      *
651600070420      * OVRPRTF BOLLE A4
651700070426     c                   if        *in97
651800070420     C                   MOVEA     D90PRB4       CMA4(30)
651900070420     C                   MOVEA     D90MDB4       CMA4(52)
652000070426     c                   else
652100070426     C                   MOVEA     D90PRB4       CMA4f6(30)
652200070426     C                   MOVEA     D90MDB4       CMA4f6(52)
652300070426     c                   endif
652400070420     C*
652500111221     C                   Z-ADD     130           LUNG             15 5
652600111221     C                   CLEAR                   COMMAN          130
652700070426     C   97              MOVEA     CMA4          COMMAN
652800070426     C  n97              MOVEA     CMA4f6        COMMAN
652900070420     C                   CALL      'QCMDEXC'                            91
653000070420     C                   PARM                    COMMAN
653100070420     C                   PARM                    LUNG
653200070420      *
653300070420     C                   if        not *in91
653400070420      *
653500070420      * OVRPRTF BOLLE A5
653600070426     c                   if        *in97
653700070420     C                   MOVEA     D90PRB5       CMA5(30)
653800070420     C                   MOVEA     D90MDB5       CMA5(52)
653900070426     c                   else
654000070426     C                   MOVEA     D90PRB5       CMA5f6(30)
654100070426     C                   MOVEA     D90MDB5       CMA5f6(52)
654200070426     c                   endif
654300070420     C*
654400111221     C                   Z-ADD     130           LUNG
654500070420     C                   CLEAR                   COMMAN
654600070426     C   97              MOVEA     CMA5          COMMAN
654700070426     C  n97              MOVEA     CMA5f6        COMMAN
654800070420     C                   CALL      'QCMDEXC'                            91
654900070420     C                   PARM                    COMMAN
655000070420     C                   PARM                    LUNG
655100070420     c                   endif
655200070420     c                   endif
655300070420      *
655400070420     c                   ENDSR
655500940802     C**************************************************************************
655600940802     C*    VISUALIZZA LEGAMI BOLLA
655700940802     C**************************************************************************
655800930413     C     CMD18         BEGSR
655900940802     C*
656000940411     C                   CLEAR                   WCAR
656100940411     C*
656200940411     C* MUOVO I DATI DELLA BOLLA CHE STO VISUALIZZANDO
656300940714     C                   MOVEL     V2CAAS        NUM2              4 0
656400940411     C                   MOVEL     COMLNP        COMLPN
656500940714     C                   MOVEL     V2CNRS        COMNRN
656600940714     C                   MOVEL     V2CNSP        COMNSN
656700950321     C                   MOVEL     V3CLNA        COMLAN
656800950428     C                   MOVEL     COMC08        SAVC08
656900940408     C**
657000940713     C* SAVNR8 > 1 --> GESTIONE SUBFILE
657100940408     C**
657200940907    1C     SAVNR8        IFGT      2
657300940408     C*
657400940809     C                   MOVEL     'S'           WRIEM
657500930413     C     FORC2         TAG
657600940713     C                   Z-ADD     1             REC8
657700940809     C     WRIEM         IFEQ      'S'
657800131107     C                   WRITE     LRI6T01
657900131107     C                   WRITE     LRI6Z08
658000940809     C                   MOVEL     *BLANKS       WRIEM
658100940809     C                   ENDIF
658200950428     C*
658300950428     C                   MOVEL     SAVC08        COMC08
658400131107     C                   EXFMT     LRI6C08
658500930413     C*
658600930413     C* CMD12 - FINE LAVORO
658700930413     C   KL              GOTO      SOTTO
658800930413     C*
658900131107     C                   READC     LRI6S08                                93
659000930510     C   93              GOTO      SOTTO
659100930510     C*
659200940408    2C     *IN93         DOWEQ     '0'
659300930413     C*
659400941230    3C     V8CSCE        IFEQ      '5'
659500940713     C                   MOVEL     ' '           V8CSCE
659600950103     C*
659700950103     C* RIPRISTINO GLI INDICATORI
659800950103     C                   MOVEL     V8CI80        *IN80
659900950103     C                   MOVEL     V8CI81        *IN81
660000950103     C                   MOVEL     V8CI82        *IN82
660100950103     C                   MOVEL     V8CI83        *IN83
660200131107     C                   UPDATE    LRI6S08
660300930504     C*
660400940907     C     V8CAAN        IFEQ      *ZERO
660500940907     C                   MOVEL     V8CAAP        LBLAAN
660600940907     C                   MOVEL     V8CLPP        LBLLPN
660700940907     C                   MOVEL     V8CNRP        LBLNRN
660800940907     C                   MOVEL     V8CNSP        LBLNSN
660900940907     C                   MOVEL     'P'           V8CFPS            1
661000940907     C                   ELSE
661100940907     C                   MOVEL     V8CAAN        LBLAAN
661200940907     C                   MOVEL     V8CLPN        LBLLPN
661300940907     C                   MOVEL     V8CNRN        LBLNRN
661400940907     C                   MOVEL     V8CNSN        LBLNSN
661500940907     C                   MOVEL     'S'           V8CFPS
661600940907     C                   ENDIF
661700940408     C*
661800940408     C* CONTROLLO SE IL LEGAME E' IN PARTENZA O IN ARRIVO
661900940907     C     KLBL1         CHAIN     FNARB01L                           93
662000940408     C*
662100940408    4C     *IN93         IFEQ      *OFF
662200940408     C*
662300940408     C                   EXSR      LEG2
662400940408     C*
662500940408   X4C                   ELSE
662600940408     C*
662700950316     C                   Z-ADD     LBLAAN        PA0AAS
662800950316     C                   Z-ADD     LBLLPN        PA0LNP
662900950316     C                   Z-ADD     LBLNRN        PA0NRS
663000950428     C                   Z-ADD     LBLNSN        PA0NSP
663100060130     C                   MOVEL     rsut          PA0RSU
663200941214     C                   MOVEL     SIMFEL        PA0SIM
663300040121     C                   MOVEL     *blanks       PA0CML
663400960903     C     PA1FLG        IFEQ      'N'
663500960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
663600960903     C*       verifichi questo caso
663700960903     C                   MOVEL     'R'           PA0FLG
663800960903     C                   ELSE
663900941214     C                   MOVEL     'A'           PA0FLG
664000960903     C                   END
664100941214     C                   MOVEL     *BLANKS       PA0CLI
664200950629     C                   CLEAR                   PA0F03
664300941214     C                   MOVEL     *BLANKS       PA0GIA
664400941214     C                   MOVEL     PARAM0        KPJBU
664500940722     C                   CALL      'FNLS05R'
664600930413     C                   PARM                    KPJBA
664700941214     C                   MOVEL     KPJBU         PARAM0
664800941214     C                   MOVEL     PA0F03        WF03
664900940907     C                   MOVEL     'S'           WRIEM
665000940408    4C                   ENDIF
665100940408     C*
665200940408    3C                   ENDIF
665300930413     C*
665400131107     C                   READC     LRI6S08                                93
665500940408    2C                   ENDDO
665600930413     C                   GOTO      FORC2
665700930413     C*
665800930413     C     SOTTO         TAG
665900930413     C*
666000940408    1C                   ELSE
666100940408     C*
666200940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
666300131107     C     2             CHAIN     LRI6S08                            99
666400940907     C     V8CAAP        IFEQ      V3CAAS
666500940907     C     V8CLPP        ANDEQ     V3CLNP
666600940907     C     V8CNRP        ANDEQ     V3CNRS
666700940907     C     V8CNSP        ANDEQ     V3CNSP
666800940907     C                   MOVEL     V8CAAN        ARBAAS
666900940907     C                   MOVEL     V8CLPN        ARBLNP
667000940907     C                   MOVEL     V8CNRN        ARBNRS
667100940907     C                   MOVEL     V8CNSN        ARBNSP
667200940907     C                   MOVEL     'S'           V8CFPS
667300940907     C                   ELSE
667400940907     C                   MOVEL     V8CAAP        ARBAAS
667500940907     C                   MOVEL     V8CLPP        ARBLNP
667600940907     C                   MOVEL     V8CNRP        ARBNRS
667700940907     C                   MOVEL     V8CNSP        ARBNSP
667800940907     C                   MOVEL     'P'           V8CFPS
667900940907     C                   ENDIF
668000940907     C* QUINDI VEDO SE LA TROVO FRA GLI ARRIVI
668100940907     C     KARB2         CHAIN     FNARB01L                           93
668200940408     C*
668300121116    2C     *IN93         IFEQ      *ON
668400950316     C                   Z-ADD     ARBAAS        PA0AAS
668500950316     C                   Z-ADD     ARBLNP        PA0LNP
668600950316     C                   Z-ADD     ARBNRS        PA0NRS
668700950316     C                   Z-ADD     ARBNSP        PA0NSP
668800060130     C                   MOVEL     rsut          PA0RSU
668900941214     C                   MOVEL     SIMFEL        PA0SIM
669000040121     C                   MOVEL     *blanks       PA0CML
669100960903     C     PA1FLG        IFEQ      'N'
669200960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
669300960903     C*       verifichi questo caso
669400960903     C                   MOVEL     'R'           PA0FLG
669500960903     C                   ELSE
669600941214     C                   MOVEL     'A'           PA0FLG
669700960903     C                   END
669800941214     C                   MOVEL     *BLANKS       PA0CLI
669900950629     C                   CLEAR                   PA0F03
670000941214     C                   MOVEL     *BLANKS       PA0GIA
670100941214     C                   MOVEL     PARAM0        KPJBU
670200940722     C                   CALL      'FNLS05R'
670300930413     C                   PARM                    KPJBA
670400941214     C                   MOVEL     KPJBU         PARAM0
670500941214     C                   MOVEL     PA0F03        WF03
670600940907     C                   MOVEL     'S'           WRIEM
670700121116     c* reimposto la key spedizione
670800121116     C                   MOVEL     NUM2          arbAAS
670900121116     C                   MOVEL     COMLPN        arbLNP
671000121116     C                   MOVEL     COMNRN        arbNRS
671100121116     C                   MOVEL     COMNSN        arbNSP
671200930510     C*
671300940408   X2C                   ELSE
671400940408     C*
671500940408     C                   EXSR      LEG2
671600940408     C*
671700940408    2C                   END
671800940408    1C                   END
671900940411     C*
672000940411     C* RICARICO I DATI DELLA BOLLA INIZIALE
672100940411     C     WCAR          IFEQ      'C'
672200940411     C                   SETOFF                                       75
672300940714     C                   MOVEL     NUM2          V2CAAS
672400940411     C                   MOVEL     COMLPN        COMLNP
672500940714     C                   MOVEL     COMNRN        V2CNRS
672600940714     C                   MOVEL     COMNSN        V2CNSP
672700940712     C     KARB          CHAIN     FNARB01L                           93
672800940411     C* RICARICO I DATI
672900940411     C                   EXSR      BOLLA
673000940411     C*
673100940411     C* RIABILITO IL CMD18
673200940411     C                   SETON                                        94
673300940411    1C                   END
673400940802     C*
673500930413     C                   ENDSR
673600940802     C**************************************************************************
673700940802     C*    LEGAMI BOLLA
673800940802     C**************************************************************************
673900940408     C     LEG2          BEGSR
674000940802     C*
674100940411     C                   MOVEL     'C'           WCAR              1
674200940408     C*
674300940408     C* 75 ON - NON RIENTRO NEL CARICAMENTO LEGAME BOLLA
674400940408     C                   SETON                                        75
674500940408     C* 94 OFF- DISABILITO IL CMD18
674600940408     C                   SETOFF                                       94
674700940408     C* CARICO DATI BOLLA
674800940408     C                   EXSR      BOLLA
674900940411     C*
675000940411     C* VISUALIZZA NUOVO LEGAME
675100940408     C* SE LA BOLLA E' UNA PRECEDENTE --> VISUALIZZO LA SEGUENTE
675200940713     C     V8CFPS        IFEQ      'P'
675300950316     C                   MOVEA     'SEGUEN.'     LBL(14)
675400940408     C                   ELSE
675500940408     C* SE LA BOLLA E' UNA SEGUENTE   --> VISUALIZZO LA PRECEDENTE
675600950316     C                   MOVEA     'PRECED.'     LBL(14)
675700940408     C                   ENDIF
675800950316     C                   MOVEA     COMLPN        LBL(21)
675900950316     C                   MOVEA     COMNRN        LBL(25)
676000950316     C                   MOVEA     COMNSN        LBL(28)
676100950316     C                   MOVEA     COMLAN        LBL(40)
676200950307     C                   MOVEA     *BLANKS       LBL(44)
676300940408     C                   MOVEA     LBL           LEGAME
676400940408     C*
676500940411     C* MOUVO I DATI DELLA LEGATA NEI CAMPI DELLA PARAM
676600940714     C                   MOVEL     ARBAAS        V2CAAS
676700940411     C                   MOVEL     ARBLNP        COMLNP
676800940714     C                   MOVEL     ARBNRS        V2CNRS
676900940714     C                   MOVEL     ARBNSP        V2CNSP
677000940408     C*
677100940408     C                   EXSR      VISUA2
677200940408     C*
677300941230     C                   MOVEL     'S'           WRIEM
677400940408     C                   ENDSR
677500940408     C*
677600940712      **************************************************************************
677700131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
677800940408      *     PER BOLLA NORMAL - CMD18 PREMUTO DA VISUAL -
677900091216      *     ricordarsi di aggiornare anche RIEPI2 per ogni modifica fatta qui
678000940712      **************************************************************************
678100940712     C     RIEPIL        BEGSR
678200940712      *
678300940712     C                   SETOFF                                       89
678400940712      *
678500940712      * EMETTO IL FORMATO VIDEO
678600940720     C                   MOVEL     'S'           WRIEM
678700940712     C     FOR07         TAG
678800940720     C     WRIEM         IFEQ      'S'
678900131107     C                   WRITE     LRI6T01
679000940720     C                   MOVEL     *BLANKS       WRIEM
679100940720     C                   ENDIF
679200021217      * Imposto la stringa comandi del 'giro' successivo
679300021217     C                   IF        *inKY = *on and $Loop > 1
679400021217     c                   Exsr      Ges_F24
679500021217     c                   eval      FD07 = FDxx
679600021217     c                   eval      F24D07 = F24Dxx
679700021217     C                   ENDIF
679800131107     C                   EXFMT     LRI6D07
679900070420     c                   setoff                                       28
680000070420     c                   clear                   v1cmsg
680100940712      *
680200131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
680300940712     C   KY*IN89         IFEQ      *ON
680400940712     C                   SETOFF                                       89
680500940712     C                   ELSE
680600940712     C                   SETON                                        89
680700940712     C                   ENDIF
680800940712     C   KY              GOTO      FOR07
680900940712      *
681000131108      * F3 - FINE LAVORO TORNO A SUBFILE
681100940714     C   KC              MOVEL     '2'           V2CFLG            1
681200940712     C   KC              GOTO      ENDRIE
681300940712      *
681400131108      * F18 - INTERROGA LEGAME BOLLA
681500940712     C   KS              DO
681600940712     C                   EXSR      CMD18
681700940712     C                   GOTO      FOR07
681800940712     C                   END
681900940712      *
682000940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
682100940712     C                   EXSR      COMAND
682200940905     C     WCMD          IFEQ      'S'
682300940802     C     WF03          IFEQ      *BLANKS
682400070404     c
682500940802     C                   GOTO      FOR07
682600940802     C                   ELSE
682700940802     C                   MOVEL     '2'           V2CFLG
682800940802     C                   GOTO      ENDRIE
682900070404     C                   ENDIF
683000091215     c
683100070404     c                   else
683200070404     c
683300070404     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
683400070404     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
683500070404     c                   if        v3cgesd='?'
683600131120
683700131120     c**                 if        wriem='S'
683800131120     c**                 eval      wvideo='R'
683900131120     c**                 exsr      Riemett
684000131120     c**                 endif
684100131120
684200070404     c                   exsr      VISUALAbi
684300070404     c                   clear                   v3cgesd
684400070412     c                   eval      wriem='S'
684500070404     c                   endif
684600131120     c
684700131120     c* controlla se immessi dei '?'
684800131120     c                   eval      wvideo='R'
684900131120     c                   exsr      CTR_puIn
685000091215     c
685100091215     c                   if        wriem='S'
685200091215     C                   GOTO      FOR07
685300091215     c                   endif
685400070404     c
685500940712     C                   END
685600940712      *
685700940712     C     ENDRIE        ENDSR
685800940712      **************************************************************************
685900131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
686000940408      *     PER LEGAME BOLLA - CMD18 PREMUTO DA VISUA2 -
686100940712      **************************************************************************
686200940712     C     RIEPI2        BEGSR
686300940712      *
686400940712     C                   SETOFF                                       89
686500940712      *
686600940712      * EMETTO IL FORMATO VIDEO
686700940809     C                   MOVEL     'S'           WRIEM
686800940712     C     FOR77         TAG
686900940809     C     WRIEM         IFEQ      'S'
687000131107     C                   WRITE     LRI6T01
687100940809     C                   MOVEL     *BLANKS       WRIEM
687200940809     C                   ENDIF
687300091216      * Imposto la stringa comandi del 'giro' successivo
687400091216     C                   IF        *inKY = *on and $Loop > 1
687500091216     c                   Exsr      Ges_F24
687600091216     c                   eval      FD07 = FDxx
687700091216     c                   eval      F24D07 = F24Dxx
687800091216     C                   ENDIF
687900091216     c
688000131107     C                   EXFMT     LRI6D07
688100070516     c                   setoff                                       28
688200070516     c                   clear                   v1cmsg
688300940408     C*
688400131108      * F3 - FINE LAVORO TORNO A SUBFILE
688500940714     C   KC              MOVEL     '2'           V2CFLG            1
688600940408     C*
688700940408     C  NKC              DO
688800940712      *
688900131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
689000940712     C   KY*IN89         IFEQ      *ON
689100940712     C                   SETOFF                                       89
689200940712     C                   ELSE
689300940712     C                   SETON                                        89
689400940712     C                   ENDIF
689500940712     C   KY              GOTO      FOR77
689600940712      *
689700940712      *
689800131108      * F18 - INTERROGA LEGAME BOLLA
689900940712     C   KS              DO
690000940712     C                   EXSR      CMD18
690100940712     C                   GOTO      FOR77
690200940712     C                   END
690300940712      *
690400940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
690500940712     C                   EXSR      COMAND
690600940905     C     WCMD          IFEQ      'S'
690700940712     C                   GOTO      FOR77
690800091216     c
690900091216     c                   else
691000091216     c
691100091216     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
691200091216     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
691300091216     c                   if        v3cgesd='?'
691400131120
691500131120     c**                 if        wriem='S'
691600131120     c**                 eval      wvideo='R'
691700131120     c**                 exsr      Riemett
691800131120     c**                 endif
691900131120
692000091216     c                   exsr      VISUALAbi
692100091216     c                   clear                   v3cgesd
692200091216     c                   eval      wriem='S'
692300091216     c                   endif
692400131120
692500131120     c* controlla se immessi dei '?'
692600131120     c                   eval      wvideo='R'
692700131120     c                   exsr      CTR_puIn
692800091216     c
692900091216     c                   if        wriem='S'
693000091216     C                   GOTO      FOR77
693100091216     c                   endif
693200091216     c
693300940712     C                   END
693400091216     C                   END
693500940712      *
693600940712     C                   ENDSR
693700131120      **************************************************************************
693800131120      * riemissione video per nuovo '?'
693900131120      **************************************************************************
694000131120     c**   Riemett       BEGSR
694100131120     c**                 write     LRI6t01
694200131120
694300131120     c**                 select
694400131120     c**                 when      wvideo='4'
694500131120     c**                 write     LRI6d04
694600131120     c**                 when      wvideo='R'
694700131120     c**                 write     LRI6d07
694800131120     c**                 endsl
694900131120
695000131120     c**                 ENDSR
695100940923      **************************************************************************
695200940923      *     CONTROLLI FORMATO VIDEO D04
695300940923      **************************************************************************
695400131120     C     CTR_puIn      BEGSR
695500110601     c*
695600110601     c* verifico se inserito '?' su email destinatario
695700110601     c                   if        v4cemw='?'
695800110601     c                   exsr      sr_emdest
695900110601     C                   MOVEL     'S'           WRIEM
696000110601     c                   endif
696100940923      *
696200091215      * Verifico se inserito '?' per visualizzazione beneficiario
696300091215     c                   if        vidben='?'
696400131120     c**                 if        wriem='S'
696500131120     c**                 exsr      Riemett
696600131120     c**                 endif
696700131120     c
696800091215     c                   exsr      sr_bencas
696900091215     C                   MOVEL     'S'           WRIEM
697000091215     c                   endif
697100131114     c
697200131114     c* Visualizzazione dati filiale
697300131114     c                   if        viddec_f='?'
697400131120     c**                 if        wriem='S'
697500131120     c**                 exsr      Riemett
697600131120     c**                 endif
697700131120
697800131114     c                   clear                   trulorfds
697900131114     c                   eval      iorffil=arblna
698000131114     c                   call      'TRULORFR'
698100131114     c                   parm                    kpjba
698200131114     c                   parm                    trulorfds
698300131114     c
698400131114     c                   clear                   viddec_f
698500131114     C                   MOVEL     'S'           WRIEM
698600131114     c                   endif
698700091215     c
698800940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
698900940923     C     V4IGCA        IFEQ      '?'
699000991027     C                   CLEAR                   DS7PQRS
699100941003     C                   MOVEL     'S'           DS7RIC
699200941003     C                   MOVEL     'V'           DS7GES
699300941003     C                   MOVEL     V4CGCA        DS7COD
699400991027     C                   MOVEL     DS7PQRS       KPJBU
699500960126     C                   CALL      'TRTB72R'
699600941003     C                   PARM                    KPJBA
699700940923     C                   MOVEL     *BLANKS       V4IGCA
699800940923     C                   MOVEL     'S'           WRIEM
699900940923     C                   ENDIF
700000940923     C*
700100940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
700200940923     C     V4IGGA        IFEQ      '?'
700300991027     C                   CLEAR                   DS7PQRS
700400941003     C                   MOVEL     'S'           DS7RIC
700500941003     C                   MOVEL     'V'           DS7GES
700600941003     C                   MOVEL     V4CGGA        DS7COD
700700991027     C                   MOVEL     DS7PQRS       KPJBU
700800960126     C                   CALL      'TRTB69R'
700900941003     C                   PARM                    KPJBA
701000940923     C                   MOVEL     *BLANKS       V4IGGA
701100940923     C                   MOVEL     'S'           WRIEM
701200940923     C                   ENDIF
701300940923     C*
701400940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
701500940923     C     V4IGMA        IFEQ      '?'
701600991027     C                   CLEAR                   DS7PQRS
701700941003     C                   MOVEL     'S'           DS7RIC
701800941003     C                   MOVEL     'V'           DS7GES
701900941003     C                   MOVEL     V4CGMA        DS7COD
702000991027     C                   MOVEL     DS7PQRS       KPJBU
702100960126     C                   CALL      'TRTB70R'
702200941003     C                   PARM                    KPJBA
702300940923     C                   MOVEL     *BLANKS       V4IGMA
702400940923     C                   MOVEL     'S'           WRIEM
702500940923     C                   ENDIF
702600940923     C*
702700940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
702800940923     C     V4IGVA        IFEQ      '?'
702900991027     C                   CLEAR                   DS7PQRS
703000941003     C                   MOVEL     'S'           DS7RIC
703100941003     C                   MOVEL     'V'           DS7GES
703200941003     C                   MOVEL     V4CGVA        DS7COD
703300991027     C                   MOVEL     DS7PQRS       KPJBU
703400960126     C                   CALL      'TRTB73R'
703500941003     C                   PARM                    KPJBA
703600940923     C                   MOVEL     *BLANKS       V4IGVA
703700940923     C                   MOVEL     'S'           WRIEM
703800940923     C                   ENDIF
703900940923      *
704000940923     C                   ENDSR
704100940321      **************************************************************************
704200940713      *     R O U T I N E      I N I Z I A L E
704300940713      **************************************************************************
704400940713     C     *INZSR        BEGSR
704500940713      *
704600940713     C     KARB          KLIST
704700940714     C                   KFLD                    V2CAAS
704800940713     C                   KFLD                    COMLNP            3 0
704900940714     C                   KFLD                    V2CNRS
705000940714     C                   KFLD                    V2CNSP
705100940713     C     KARB2         KLIST
705200940713     C                   KFLD                    ARBAAS
705300940713     C                   KFLD                    ARBLNP
705400940713     C                   KFLD                    ARBNRS
705500940713     C                   KFLD                    ARBNSP
705600971223     C     KARB4         KLIST
705700971223     C                   KFLD                    AR4AAS
705800971223     C                   KFLD                    AR4LNP
705900971223     C                   KFLD                    AR4NRS
706000971223     C                   KFLD                    AR4NSP
706100000228     C     KAR4W         KLIST
706200000228     C                   KFLD                    wAR4AAS
706300000228     C                   KFLD                    wAR4LNP
706400000228     C                   KFLD                    wAR4NRS
706500000228     C                   KFLD                    wAR4NSP
706600940713     C     KLNPX         KLIST
706700940713     C                   KFLD                    V1CAAS
706800940713     C                   KFLD                    V1CLP2
706900940713     C                   KFLD                    V1CNRS
707000940713     C                   KFLD                    V1CNSP
707100940713     C     K1ARB         KLIST
707200020910     c                   Kfld                    Wifp
707300940713     C                   KFLD                    NDC               7 0
707400940713     C                   KFLD                    NSS               3 0
707500940713     C                   KFLD                    COMLNA            3 0
707600940713     C                   KFLD                    COMAAD
707700940713     C                   KFLD                    COMMGD
707800940713     C                   KFLD                    V1CLP1
707900940713     C     K2ARB         KLIST
708000020910     c                   Kfld                    Wifp
708100940713     C                   KFLD                    NDC
708200940713     C                   KFLD                    NSS
708300940713     C                   KFLD                    COMLNA
708400940713     C                   KFLD                    COMAAD
708500940713     C                   KFLD                    COMMGD
708600940713     C     K3ARB         KLIST
708700020910     c                   Kfld                    Wifp
708800940713     C                   KFLD                    NDC
708900940713     C                   KFLD                    NSS
709000940713     C                   KFLD                    COMLNA
709100940713     C     K4ARB         KLIST
709200940713     C                   KFLD                    COMLNA
709300940713     C                   KFLD                    COMDTD
709400940713     C                   KFLD                    V1CLP1
709500940713     C     K5ARB         KLIST
709600940713     C                   KFLD                    COMLNA
709700940713     C                   KFLD                    COMDTD
709800940713     C     K6ARB         KLIST
709900940713     C                   KFLD                    V1CLP1
710000940713     C                   KFLD                    COMAAD
710100940713     C                   KFLD                    COMMGD
710200940713     C                   KFLD                    V1CLNA
710300940713     C                   KFLD                    V1CPSD
710400950405     C                   KFLD                    COMSCD
710500950405     C     K6ARB1        KLIST
710600950405     C                   KFLD                    V1CLP1
710700950405     C                   KFLD                    COMAAD
710800950405     C                   KFLD                    COMMGD
710900950405     C                   KFLD                    V1CLNA
711000950405     C                   KFLD                    V1CPSD
711100940713     C     K7ARB         KLIST
711200940713     C                   KFLD                    V1CLP1
711300940713     C                   KFLD                    COMAAD
711400940713     C                   KFLD                    COMMGD
711500940713     C     K8ARB         KLIST
711600940713     C                   KFLD                    V1CLNA
711700940713     C                   KFLD                    COMAAD
711800940713     C                   KFLD                    COMMGD
711900940713     C                   KFLD                    V1CLP1
712000940713     C                   KFLD                    V1CPSD
712100940713     C                   KFLD                    V1CSCD
712200940713     C     K9ARB         KLIST
712300940713     C                   KFLD                    V1CLNA
712400940713     C                   KFLD                    COMAAD
712500940713     C                   KFLD                    COMMGD
712600020627     c     arb7k         klist
712700020627     c                   kfld                    v1cnzd
712800020627     c                   kfld                    v1cprd
712900131111     C     ARB75K        KLIST
713000020627     c                   kfld                    v1cnzd
713100020627     c                   kfld                    v1cprd
713200131111     C                   KFLD                    $AAS
713300131111     C                   KFLD                    $MGS
713400940713     C                   KFLD                    V1CRSD
713500940713     C     AR43K         KLIST
713600020627     c                   kfld                    v1cnzd
713700020627     c                   kfld                    v1cprd
713800940713     C                   KFLD                    $AAS
713900940713     C                   KFLD                    $MGS
714000940713     C     ARB44K        KLIST
714100020627     c                   kfld                    v1cnzd
714200020627     c                   kfld                    v1cprd
714300940713     C                   KFLD                    $AAS
714400940713     C                   KFLD                    $MGS
714500940713     C                   KFLD                    V1CRSM
714600940713     C     AR44K         KLIST
714700020627     c                   kfld                    v1cnzd
714800020627     c                   kfld                    v1cprd
714900940713     C                   KFLD                    $AAS
715000940713     C                   KFLD                    $MGS
715100971223     C     KAR4          KLIST
715200971223     C                   KFLD                    WTRC
715300971223     C                   KFLD                    WN30
715400010508     C     KAR44         KLIST
715500010508     C                   KFLD                    WTRC
715600010508     C                   KFLD                    WN14
715700940713     C     KART          KLIST
715800940713     C                   KFLD                    ARBAAS
715900940713     C                   KFLD                    ARBLNP
716000940713     C                   KFLD                    ARBNRS
716100940713     C                   KFLD                    ARBNSP
716200940713     C                   KFLD                    V1CSCD
716300940713     C*
716400940713     C     KTAB1         KLIST
716500940713     C                   KFLD                    CODUT
716600940713     C                   KFLD                    COD
716700940713     C                   KFLD                    KEY
716800940713     C     KTAB2         KLIST
716900940713     C                   KFLD                    CODUT
717000940713     C                   KFLD                    COD
717100940713     C*
717200940907     C     KLBL1         KLIST
717300940713     C                   KFLD                    LBLAAN
717400940713     C                   KFLD                    LBLLPN
717500940713     C                   KFLD                    LBLNRN
717600940713     C                   KFLD                    LBLNSN
717700940713     C     KLBL2         KLIST
717800940713     C                   KFLD                    LBLAAP
717900940713     C                   KFLD                    LBLLPP
718000940713     C                   KFLD                    LBLNRP
718100940713     C                   KFLD                    LBLNSP
718200940907     C     KLBL3         KLIST
718300940907     C                   KFLD                    KAAS
718400940907     C                   KFLD                    KLNP
718500940907     C                   KFLD                    KNRS
718600940907     C                   KFLD                    KNSP
718700940713     C*
718800940719     C     KAR41         KLIST
718900940719     C                   KFLD                    KAAS
719000940719     C                   KFLD                    KLNP
719100940719     C                   KFLD                    KNRS
719200940719     C                   KFLD                    KNSP
719300940802     C                   KFLD                    KTRC
719400951220     C     KAR42         KLIST
719500951220     C                   KFLD                    KAAS
719600951220     C                   KFLD                    KLNP
719700951220     C                   KFLD                    KNRS
719800951220     C                   KFLD                    KNSP
719900021213     C*
720000021213     C     KAR51         KLIST
720100021213     C                   KFLD                    KAAS
720200021213     C                   KFLD                    KLNP
720300021213     C                   KFLD                    KNRS
720400021213     C                   KFLD                    KNSP
720500021213     C                   KFLD                    KTRd              3
720600940719     C*
720700991026     C     KAR61         KLIST
720800940713     C                   KFLD                    KAAS
720900940713     C                   KFLD                    KLNP
721000940713     C                   KFLD                    KNRS
721100940713     C                   KFLD                    KNSP
721200991026     C                   KFLD                    KTRC
721300991026     C*
721400051128     C     Ksped         KLIST
721500991026     C                   KFLD                    KAAS
721600991026     C                   KFLD                    KLNP
721700991026     C                   KFLD                    KNRS
721800991026     C                   KFLD                    KNSP
721900940713     C*
722000990715     C     KGCA1         KLIST
722100941202     C                   KFLD                    KAAS
722200940713     C                   KFLD                    KLNP
722300940713     C                   KFLD                    KNRS
722400940713     C                   KFLD                    KNSP
722500940713     C*
722600990805     C     KEVB1         KLIST
722700940713     C                   KFLD                    KAAS$
722800940713     C                   KFLD                    KLNP
722900940713     C                   KFLD                    KNRS
723000940713     C                   KFLD                    KNSP
723100131114     C     Kcet          KLIST
723200140224     C                   KFLD                    pctfgs
723300131114     C                   KFLD                    pctNDC
723400131114     C                   KFLD                    arbpdc
723500131114     C                   KFLD                    TTRD              3
723600131114     C                   KFLD                    arbAAS
723700131114     C                   KFLD                    arbLNP
723800131114     C                   KFLD                    arbNRS
723900131114     C                   KFLD                    arbNSP
724000950324     C*
724100020906     C     KFVV1         KLIST
724200950324     C                   KFLD                    WNPG
724300950327     C                   KFLD                    WNDC
724400020906     c                   Kfld                    Wfgs
724500131119     C     KDST          KLIST
724600131119     C                   KFLD                    WNPG
724700131119     C                   KFLD                    kndc6
724800131119     c                   Kfld                    Wfgs
724900021203
725000021203     C     Kfapd         klist
725100021203     C                   kfld                    kpdtip
725200021203     C                   kfld                    kpdpdr
725300131108     c
725400131108     c     kbrv          klist
725500131108     C                   KFLD                    WNPG
725600131108     C                   KFLD                    knfv
725700131108     c                   Kfld                    Wfgs
725800131108     c                   Kfld                    artfls
725900131108     c                   Kfld                    artlna
726000131108     c                   Kfld                    artnrs
726100131108     c                   Kfld                    artnsc
726200131111     C     KarS          KLIST
726300131111     C                   KFLD                    V1CNOT
726400131111     C                   KFLD                    KKTRC             1
726500131111     C                   MOVE      'C'           KKTRC
726600131111     C     KarSXT        KLIST
726700131111     C                   KFLD                    arSFLS
726800131111     C                   KFLD                    arSLNA
726900131111     C                   KFLD                    arSNRS
727000131111     C                   KFLD                    arSNSC
727100131111     C     KartXP        KLIST
727200131111     C                   KFLD                    arTAAS
727300131111     C                   KFLD                    arTLNP
727400131111     C                   KFLD                    arTNRS
727500131111     C                   KFLD                    arTNSP
727600040218      *
727700040218      * File FITGD02L
727800040218     c     K02TGD02      klist
727900040218     c                   kfld                    TGDtor
728000040218     c                   kfld                    TGDogg
728100030527     C*
728200030527     C* Chiave su TNTBE01L - Parziale
728300030527     C     KTBE01P       KLIST
728400030527     C                   KFLD                    TBECOD
728500030527     C                   KFLD                    TBEKE1
728600940713     C*
728700940713     C*-------------------------------------------------------------------------
728800940713     C*
728900991222     C     *LIKE         DEFINE    ARBAAS        WAAS
729000991027     C     *LIKE         DEFINE    ARBLNP        WLNP
729100991027     C     *LIKE         DEFINE    ARBNRS        WNRS
729200991027     C     *LIKE         DEFINE    ARBNSP        WNSP
729300991027     C     *LIKE         DEFINE    FVVNPG        WNPG
729400991027     C     *LIKE         DEFINE    FVVNFV        WNDC
729500131108     C     *LIKE         DEFINE    brvNFV        knfv
729600991027     C     *LIKE         DEFINE    V1CSCD        COMSCD
729700991027     C     *LIKE         DEFINE    COMC08        SAVC08
729800971219     C     *LIKE         DEFINE    ARBAAS        KAAS
729900990805     C     *LIKE         DEFINE    EVBAAS        KAAS$
730000940713     C     *LIKE         DEFINE    ARBLNP        KLNP
730100940713     C     *LIKE         DEFINE    ARBNRS        KNRS
730200940713     C     *LIKE         DEFINE    ARBNSP        KNSP
730300961112     C     *LIKE         DEFINE    ARBFBR        WFBR
730400940713     C     *LIKE         DEFINE    TBLCOD        COD
730500940713     C     *LIKE         DEFINE    TBLKEY        KEY
730600940713     C     *LIKE         DEFINE    ARBAAS        $AAS
730700940713     C     *LIKE         DEFINE    ARBMGS        $MGS
730800940719     C     *LIKE         DEFINE    AR4TRC        KTRC
730900940907     C     *LIKE         DEFINE    LBLAAN        COMAAN
731000971223     C     *LIKE         DEFINE    AR4TRC        WTRC
731100060130     C*-------------------------------------------------------------------------
731200060130     c
731300060130     c                   movel     kpjbu         savkpjbu
731400060130      *
731500060130     c     *dtaara       define    §azute        azuteds
731600060130     c     *dtaara       define    §datiute      ddatiute
731700060130     c                   in(E)     *dtaara
731800060130    1c                   If        %error  or RSUT = *blanks
731900060130     c                   Clear                   Tibs34ds
732000060130     c                   Call      'TIBS34R'
732100060130     c                   Parm                    Tibs34ds
732200060130     c                   In        *dtaara
732300060130    1c                   EndIf
732400060130
732500060130     c                   Clear                   wabi
732600060130     c                   Clear                   v1cmsg
732700060130     c                   Clear                   dLat
732800060130
732900060130      * Verifica errori e autorità profilo
733000060130s   1c                   Select
733100060130      * se ho errori nei dati utente esco dal pgm
733200060130w   1c                   When      DutErr = 'E'
733300060130     c                   GoTo      Fine
733400060130      * se non c'è l'abilitazione
733500060130      * --> se 1° livello, abilitazioni al terminal
733600060130      *     se 2° livello, abilitazioni al punto operativo
733700060130      *     se sede è impossibile ma se capita mando a fine il pgm
733800060130w   1c                   When      UteAut = *Blanks
733900060130if  2c                   If        DutLpo = '1'
734000060130     c                   Eval      wabi   = 'TP'
734100060130e   2c                   EndIf
734200060130if  2c                   If        DutLpo = '2'
734300060130     c                   Eval      wabi   = 'PO'
734400060130e   2c                   EndIf
734500060130if  2c                   If        DutLpo = 'S'
734600060130     c                   GoTo      Fine
734700060130e   2c                   EndIf
734800060130      * carica le abilitazioni del profilo
734900060130x   1c                   Other
735000060130     c                   Movel     UteFaf        Dute01
735100060131     c                   If        §Utembf <> *Blanks
735200060131     c                   Eval      wabi = §Utembf
735300060130     c                   Else
735400060130     c                   Eval      wabi = UteAut
735500060130     c                   EndIf
735600060130e   1c                   EndSl
735700060130
735800060130      * controllo se ok l'abilitazione dell'utente
735900060130     c                   Clear                   Tibs02ds
736000060130     c                   Eval      T02Mod = 'C'
736100060130     c                   Eval      T02Sif = knsif
736200060130     c                   Eval      T02Cod = 'LAT'
736300060130     c                   Movel(p)  wabi          T02Ke1
736400060130     c                   Call      'TIBS02R'
736500060130     c                   Parm                    kpjba
736600060130     c                   Parm                    Tibs02ds
736700060130if  1c                   If        T02Err = *Blanks
736800060130     c                   Eval      dLat = T02Uni
736900060130e   1c                   EndIf
737000060130      * errore o non abilitato
737100060130if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
737200060130     c                   Seton                                        28
737300060130     C                   movel     msg(26)       v1cmsg
737400060130   x1c                   Else
737500060130      * Reperimento dei P.O. gestibili dall'utente
737600060130     c                   clear                   TRUL31DS
737700060130     c                   clear                   TRUL31DS2
737800060130     c                   eval      I31abi = wabi
737900060130     c                   eval      I31cdi = DUTdis
738000060130     c                   eval      I31car = DUTare
738100060130     c                   eval      I31cpo = DUTpou
738200060130     c                   call      'TRUL31R'
738300060130     c                   parm                    KPJBA
738400060130     c                   parm                    TRUL31DS
738500060130     c                   parm                    TRUL31DS2
738600060130if  2c                   if        O31pog > *zeros
738700060130     c                   movea     O31pog        $pog
738800060130     c                   movea     O31arg        $arg
738900060130     c                   movea     O31dig        $dig
739000060130x   2c                   else
739100060130     c                   Seton                                        28
739200060130     c                   movel     MSG(26)       v1cmsg
739300060130e   2c                   endif
739400060130    1c                   endif
739500060130     c
739600060130     c                   movel     savkpjbu      kpjbu
739700971223     C*
739800971223     C* IMPOSTAZIONE CAMPI FISSI
739900940907     C                   MOVEL     *ZERO         COMLPN            3
740000940907     C                   MOVEL     *ZERO         COMNRN            2
740100940907     C                   MOVEL     *ZERO         COMNSN            7
740200950307     C                   MOVEL     *ZERO         COMLAN            3
740300950324     C                   Z-ADD     4             WNPG
740400940713      *
740500940713     C                   Z-ADD     *ZERO         NR2               4 0
740600940713     C                   Z-ADD     *ZERO         NR6               4 0
740700940713     C                   Z-ADD     *ZERO         NR8               4 0
740800940802     C                   MOVEL     *BLANKS       WF03              1
740900070403     c
741000070403     c* Verifico se profilo utente ha GEO attiva
741100131205     c* Per EDP sempre anche prr RAC999
741200131205     c                   if        %subst(knmus:1:3)='EDP' or
741300131205     c                             %subst(knmus:1:6)='RAC999'
741400070410     c                   eval      dutgeot='S'
741500070410     c                   else
741600070403     c     dutpou        chain     azorg01l
741700070403     c                   if        %found(azorg01l)
741800070403     c                   movel     orgde8        og148
741900070403     c                   else
742000070403     c                   clear                   og148
742100070403     c                   endif
742200070410     c
742300070524     c                   if        §oggeot<>' ' or §ogtlls<>' '
742400070524     c                   movel     'S'           dutgeot           1
742500070410     c                   endif
742600070524     c                   endif
742700131007
742800131007      /free
742900131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
743000131007      /end-free
743100140319     c* Tasto funz orari servizi sempre abilitato
743200140319     c                   eval      wabif9='S'
743300131007
743400940713      *
743500940713     C                   ENDSR
743600971223     C**
743700971223     C* ELABORAZIONE DI SINGOLA BOLLA --------------------------------*
743800971223     C     SINGOL        BEGSR
743900971223     C                   CLEAR                   WINIZ
744000971223     C                   CLEAR                   WFINE
744100971223     C*
744200971223     C* 97 ON - MANUTENZIONE BOLLA
744300060130    1C     *IN97         IFEQ      *ON
744400971223     C*
744500060224     c                   movel     arblna        w003a
744600060224     c     w003a         lookup    $pog                                   31
744700060130     c* Se la causale variazione bolle prevede anche il p.o. cliente
744800131111     c*    controllo anche questo
744900060130    2c                   if        not *in31 and d66pocl='S'
745000060130     c     arbcbo        lookup    cbodop                                 31
745100060130    3c                   if        not *in31
745200060130     c                   movel     arbksc        kscfil
745300060130   x3c                   else
745400060130     c* Se bolla doppia prendo il ksc dall'AR6
745500060130     C                   Z-ADD     ARBAAS        KAAS
745600060130     C                   Z-ADD     ARBLNP        KLNP
745700060130     C                   Z-ADD     ARBNRS        KNRS
745800060130     C                   Z-ADD     ARBNSP        KNSP
745900060130     C                   MOVEL     '2'           KTRC
746000060130     C     KAR61         CHAIN     FIAR6000                           31
746100060130if  4C                   IF          %FOUND(fiar601l)
746200060130     c                   movel     ar6ksc        kscfil
746300060130     c                   else
746400060130     c                   movel     999           kscfil
746500060130    4c                   endif
746600060130    3c                   endif
746700060130     c
746800060224     c                   movel     kscfil        w003a
746900060224     c     w003a         lookup    $pog                                   31
747000060130    2c                   endif
747100060130     c
747200060130     C     *IN31         IFEQ      *OFF
747300971223     C                   MOVEL     MSG(19)       V1CMSG
747400971223     C                   SETON                                        459028
747500971223     C                   GOTO      FOR01
747600971223     C                   ELSE
747700971223     C*
747800991027     C                   CLEAR                   FNLR48DS
747900060224     c* come p.o. gestione prendo il p.o. gestore (da £6) della linea
748000060224     c*  di arrivo
748100060224     c                   clear                   fnlv55ds
748200060224     c                   movel     arblna        d55lin
748300060224     c                   movel     '6'           d55tpt
748400060224     C                   time                    wrkoggiiso
748500060224     C     *iso          move      wrkoggiiso    d55drf
748600060224     c                   call      'FNLV55R'
748700060224     c                   parm                    fnlv55ds
748800060224     c                   movel     d55tfa        d48fgs
748900060224     c
749000131203     c                   select
749100140117     C     v1cbarcode    whengt    0
749200140117     C                   MOVEL     ARbAAS        D48AAS
749300140117     C                   MOVEL     ARbLNP        D48LNP
749400140117     C                   MOVEL     ARbNRS        D48NRS
749500140117     C                   MOVEL     ARbNSP        D48NSP
749600131203     C     SEL2          whenne    0
749700980403     C                   MOVEL     V1CAAS        D48AAS
749800980403     C                   MOVEL     V1CLP2        D48LNP
749900980403     C                   MOVEL     V1CNRS        D48NRS
750000980403     C                   MOVEL     V1CNSP        D48NSP
750100131203     C                   other
750200111011     C*                  MOVEL     AR4AAS        D48AAS
750300111011     C*                  MOVEL     AR4LNP        D48LNP
750400111011     C*                  MOVEL     AR4NRS        D48NRS
750500111011     C*                  MOVEL     AR4NSP        D48NSP
750600111011     C                   MOVEL     v2cAAS        D48AAS
750700111011     C                   MOVEL     v2clnp        D48LNP
750800111011     C                   MOVEL     v2cNRS        D48NRS
750900111011     C                   MOVEL     v2cNSP        D48NSP
751000131203     C                   ENDsl
751100141010     c* routine per verifica dispo da esaminare
751200141010     c                   exsr      sr_wdwdc
751300141010
751400971223     C                   MOVEL     V1CCVB        D48CVB
751500971223     C                   MOVEL     ARBCBO        D48CBO
751600971223     C                   MOVEL     'V'           D48FFR
751700971223     C                   MOVEL     'A'           D48TBO
751800971223     C                   MOVEL     'S'           D48F12
751900991027     C                   MOVEL     FNLR48DS      KPJBU
752000971223     C*
752100971223     C                   CALL      'FNLR48R'
752200971223     C                   PARM                    KPJBA
752300971223     C                   PARM                    DSARBD
752400971223     C                   PARM                    DSARBK
752500991011     C                   PARM                    DARBT
752600971223     C                   PARM                    DSARBG
752700030612     C                   PARM                    trul90ds
752800971223     C*
752900991027     C                   MOVEL     KPJBU         FNLR48DS
753000060307     c                   movel     'S'           wriem
753100971223     C* ERRORE
753200971223     C     D48ERR        IFEQ      '1'
753300971223     C                   MOVEL     D48MSG        V1CMSG
753400971223     C                   SETON                                        284790
753500971223     C                   GOTO      FOR01
753600971223     C                   ENDIF
753700971223     C                   ENDIF
753800971223     C*
753900971223   X2C                   ELSE
754000131111   c
754100971223     C* VISUALIZZAZIONE BOLLA
754200131111     c                   select
754300131111     C     v1cbarcode    whengt    0
754400131111     C                   MOVEL     arbAAS        V2CAAS
754500131111     C                   MOVEL     arblnp        COMLNP
754600131111     C                   MOVEL     arbNRS        V2CNRS
754700131111     C                   MOVEL     arbNSP        V2CNSP
754800140117     C     SEL2          whengt    0
754900140117     C                   MOVEL     V1CAAS        V2CAAS
755000140117     C                   MOVEL     V1CLP2        COMLNP
755100140117     C                   MOVEL     V1CNRS        V2CNRS
755200140117     C                   MOVEL     V1CNSP        V2CNSP
755300131111     C                   ENDSL
755400131111     c
755500971223     C                   EXSR      BOLLA
755600971223     C*
755700971223     C                   EXSR      VISUAL
755800971223    2C                   ENDIF
755900971223     C*
756000971223    2C     PA1FLG        IFNE      '1'
756100971223     C     PA1FLG        ANDNE     'N'
756200971223     C     PA1FLG        ANDNE     '3'
756300971223     C     PA1FLG        ANDNE     'P'
756400971223     C     PA1FLG        ANDNE     'E'
756500990129     C     PA1FLG        ANDNE     'C'
756600971223     C                   MOVEL     '1'           WINIZ             1
756700971223     C                   ELSE
756800971223     C                   MOVEL     '1'           WFINE             1
756900971223    2C                   END
757000971223    2C                   ENDSR
757100110601      **************************************************************************
757200110601      *   emissione window email destinatario
757300110601      **************************************************************************
757400110601     C     sr_emdest     BEGSR
757500110601     c                   clear                   v4cemw
757600110601     c
757700110601     c                   eval      ilv79TLA='L'
757800110601     c                   eval      ilv79Aas=arbaas
757900110601     c                   eval      ilv79Lnp=arblnp
758000110601     c                   eval      ilv79nrs=arbnrs
758100110601     c                   eval      ilv79nsp=arbnsp
758200110601     c                   call      'FNLV79R'
758300110601     c                   parm                    fnlv79ds
758400110601     c
758500110601     c                   endsr
758600091214      **************************************************************************
758700131114      *   Emissione del Beneficiario C/Ass
758800091214      **************************************************************************
758900091214     C     sr_bencas     BEGSR
759000091215     c                   clear                   vidben
759100091214     c                   clear                   wdaben
759200091214     c                   clear                   wdasrsc
759300091214     c                   clear                   wdascdi
759400091216     c                   clear                   trulintds
759500091216     c
759600091216     c                   eval      iinttip='B'
759700091216     c                   eval      iinttic=vidtic
759800091216     c                   eval      iintrscm=v4crsm
759900091216     c                   eval      iintrsco=vidrmo
760000091216     c                   eval      iintCDI =w_cdi
760100091216     c                   call      'TRULINTR'
760200091216     c                   parm                    trulintds
760300091216     c
760400091216     c                   movel     ointrsci      wdasrsc
760500091216     c
760600091216     c                   select
760700091216     c                   when      ointtipi='O'
760800091216     c                   eval      wdaben='MITTENTE  ORIGINALE'
760900091216     c
761000091216     c                   when      ointtipi='R'
761100091216     c                   eval      wdascdi=w_cdi
761200091216     c                   eval      wdaben=' O R D I N A N T E'
761300091216     c
761400091216     c                   when      ointtipi='M'
761500091216     c                   if        v3cksc>0
761600091216     c                   movel     v3cksc        wdascdi
761700091216     c                   endif
761800091216     c                   eval      wdaben='  M I T T E N T E'
761900091216     c                   ENDSL
762000091214     c
762100131107     c                   exfmt     LRI6wdas
762200091214     c                   endsr
762300130830      **************************************************************************
762400130830      *   window per visualizzare gli orari servizi
762500130830      **************************************************************************
762600130830     C     sr_OrariSr    BEGSR
762700130920
762800140110     c                   clear                   trulorSds
762900151019     c***                eval       s_IOREtfp = ARBtfp
763000151019     c* se il campo "terminal didisguido" è pieno, passo questo al posto
763100151019     c* del terminal di partenza originale
763200151019     c                   if         s_arbflp>*zeros
763300151019     c                   eval       s_IOREtfp=%INT(s_arbflp)
763400151019     c                   else
763500151019     c                   eval       s_IOREtfp=arbtfp
763600151019     c                   endif
763700140110     c                   eval       s_IOREtfa = ARBtfa
763800140110     c                   eval       s_IOREdti = ARBdti
763900140110     c                   eval       s_IOREhti = ARBhti
764000140110     c                   eval       s_IOREdcr = ARBdcr
764100140110     c                   eval       s_IOREhcr = ARBhcr
764200140110     c                   eval       s_IOREtcr = ARBtcr
764300140110     c                   eval       s_IOREDEI = s_ARBdei
764400140110     c                   eval       s_IOREOEI = s_ARBoei
764500140110     c                   eval       s_IOREfil = ARBlna
764600140110     c                   eval       s_IOREcap = ARBcad
764700140110     c                   eval       s_IOREloc = ARBlod
764800140110     c                   eval       s_IOREnar = ARBnzd
764900150724     c                   clear                   diore01
765000150728     c                   movel     s_arbttc      §ioretrazC
765100150728     c                   movel     s_arbtcc      §ioreconsC
765200150724     c                   eval       s_IOREflo = diore01
765300150724     c
765400140113     c* Data di Riferimento: data consegna se consegnata
765500140113     c                   select
765600140113     c                   when      arbdcm>0 and arbica<>' ' and arbica<>'R'
765700140113     c                             and arbicc<>' ' and arbicc<>'R'
765800140113     c                   eval       s_ioredta=arbdcm
765900140113     c* per ritorno all'incasso data del giorno
766000140113     c                   when      arbdcm>0 and arbddc>0
766100140113     C                   time                    wrkoggiiso
766200140113     C     *iso          move      wrkoggiiso    s_ioredta
766300140113     c                   other
766400140113     c* altrimenti data prevista consegna se > = oggi
766500140113     C                   time                    wrkoggiiso
766600140113     C     *iso          move      wrkoggiiso    s_ioredta
766700140113     c* se < di oggi, imposto oggi
766800140113     c                   if        amgdce>s_ioredta
766900140113     c                   eval       s_ioredta=amgdce
767000140113     c                   endif
767100140113     c                   endsl
767200140113     c
767300140110     c                   eval       s_IOREtser = 'C'
767400140409     c                   eval       s_IOREDDC=arbddc
767500140409     c                   eval       s_IOREnDC=arbndc
767600140409     c
767700140110     c                   IF         *in19 or
767800140110     c                              (ARBfbc = *blanks and
767900140110     c                               ARBddc > 0 and
768000140110     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
768100140110     c                               arbica='R' or arbicc=' ' or arbicc='R'))
768200140110     c                   eval       s_IOREcons = 'S'
768300140225     c* Orario presunto ritiro/consegna
768400140225     c                   move      arbndc        pctndc
768500140225     c                   move      arbifp        pctfgs
768600140225     c                   move      'COR'         ttrd
768700140225     c     kcet          setgt     fipct02l
768800140225     c     kcet          readpe    fipct02l
768900140310     c                   if        not %eof(fipct02l)
769000140225     c                   movel     pctdati       fipctcords
769100140307     c                   eval      s_ioreorp=§PCTORASTI
769200140225     c                   endif
769300140225     c
769400140110     c                   ENDIF
769500140225     c
769600140110     c                   eval       s_IOREtsp = ARBtsp
769700130920
769800140325     c                   eval      s_iorecol=36
769900140110     c                   call      'TRULORSR'
770000130920     c                   parm                    kpjba
770100140110     c                   parm                    trulorSds
770200130830     c
770300131111     c                   ENDSR
770400131111      **************************************************************************
770500131111      *   Prepara SQL per rsd ind lod rsm
770600131111      **************************************************************************
770700131111     C     Presql        BEGSR
770800131111     //free
770900160113      // per ora non lo faccio più  eseguire il join per problemi di prestazione
771000160113      //  la ricerca viene fatta solo sulla ragione sociale di ARB primi 35 caratteri
771100160113      //   if v1cxrsd='X'  ;
771200131114
771300160113      //   wSQL ='SELECT fnarb00f.*  +
771400160113      //             from FNARB00F left outer join FIAR400F on arbaas=ar4aas +
771500160113      //         and arblnp=ar4lnp and arbnrs=ar4nrs and arbnsp=ar4nsp  +
771600160113      //          and ar4trc=''D'' where arbnzd='''+ v1cnzd +''' '  ;
771700160113      //          else  ;
771800131114
771900131114         wSQL ='SELECT * from FNARB00F   where arbnzd='''+ v1cnzd +''' '  ;
772000160113      //   endif  ;
772100131114
772200131114       if v1cnzd=*blanks   ;
772300131114         wSQL +=' and arbprd=''' + v1cprd + ''' '   ;
772400131111       endif  ;
772500131111
772600131115       if (v1cssp=' ' or v1cssp='1')  ;
772700131115        if comdtd>0 and comdta>comdtd  ;
772800131115           wSQL += ' and (arbaas*10000)+arbmgs>=' + %editc(comdtd:'X')+ ' and +
772900131115                        (arbaas*10000)+arbmgs<=' + %editc(comdta:'X')  ;
773000131115        endif  ;
773100131115        if comdtd>0 and (comdta=0 or comdta=comdtd)      ;
773200131115           wSQL += 'and arbaas=' + %editc(comaad:'X')+ ' and arbmgs=' +
773300131115                                %editc(commgd:'X')  ;
773400131115        endif  ;
773500131115       endif  ;
773600131111
773700131111       // se voglio le "IN CONSEGNA", parzializzo
773800131111       if v1cssp='2'  ;
773900131113           wSQL += ' and (arbdcm=0 or arbica='' '' or arbica=''R'' +
774000131113                     or arbicc='' '' or arbicc=''R'')'  ;
774100131111       endif  ;
774200131114
774300131114       // Se voglio le "DA CONSEGNARE", parzializzo
774400131111       if v1cssp='1'  ;
774500131111           wSQL += ' and arbifp=0 and arbndc=0 and arbnss=0' ;
774600131111       endif  ;
774700131111
774800131114       // Se voglio le "CONSEGNATE", parzializzo
774900131111       if v1cssp='3'  ;
775000131111           wSQL += ' and arbdcm>0' ;
775100131111       endif  ;
775200131111
775300131111       // linea di arrivo
775400131111       if v1clna>0  ;
775500131111           wSQL += ' and arblna=' + %editc(v1clna:'X') ;
775600131111       endif  ;
775700131111
775800131111        if v1crsd<>*blanks  ;
775900131111
776000131111         if v1cxrsd =' '   ;
776100131114           wSQL += ' and arbrsd    like(''';
776200131111         else  ;
776300160113           wSQL += ' and arbrsd    like(''%';
776400160113          // non esegue più il joi
776500160113          //  wSQL += ' and concat(arbrsd, ifnull(ar4not, '' '')) like(''%'  ;
776600131111         endif ;
776700131111
776800131209         w060a=v1crsd  ;
776900131209         exsr APICI  ;
777000131209
777100131209           wSQL += w060a ;
777200131111           wSQL  = %trim(wSQL) + '%'')' ;
777300131111         endif ;
777400131111
777500131111        if v1cIND<>*blanks  ;
777600131111
777700131111         if v1cxind =' '   ;
777800131111           wSQL += ' and arbind like(''';
777900131111         else  ;
778000131113           wSQL += ' and arbind like (''%' ;
778100131111         endif ;
778200131111
778300131209         w060a=v1cind  ;
778400131209         exsr APICI  ;
778500131209           wSQL += w060a ;
778600131111           wSQL  = %trim(wSQL) + '%'')' ;
778700131111         endif ;
778800131111
778900131111        if v1cLOD<>*blanks  ;
779000131111
779100131111         if v1cxlod =' '   ;
779200131111           wSQL += ' and arblod like(''';
779300131111         else  ;
779400131113           wSQL += ' and arblod like (''%' ;
779500131111         endif ;
779600131111
779700131209         w060a=v1clod  ;
779800131209         exsr APICI  ;
779900131209           wSQL += w060a ;
780000131111           wSQL  = %trim(wSQL) + '%'')' ;
780100131111         endif ;
780200131111
780300131111        if v1cRSM<>*blanks  ;
780400131111
780500131111         if v1cxrsm =' '   ;
780600131111           wSQL += ' and arbrsm like(''';
780700131111         else  ;
780800131113           wSQL += ' and arbrsm like (''%' ;
780900131111         endif ;
781000131111
781100131209         w060a=v1crsm  ;
781200131209         exsr APICI  ;
781300131209           wSQL += w060a ;
781400131115           wSQL  = %trim(wSQL) + '%'')' ;
781500131111         endif ;
781600131115
781700131115           wSQL += ' order by arbnzd, arbprd, arbaas, arbmgs'  ;
781800131111
781900131111        ENDSR  ;
782000131111       //--------------------------------------------------------------
782100131111       //?Apertura cursore.                                            ?
782200131111       //--------------------------------------------------------------
782300131111       BEGSR  OpenCursor;
782400131111
782500131111         wEnd = *off;
782600131111
782700131111       //?Dichiarazione cursore?
782800131111         exec sql
782900131111         PREPARE S1 from :wSQL;
783000131209
783100131209         IF sqlcode < 0;
783200131209           wEnd = *on;
783300131209           leavesr        ;
783400131209         ENDIF;
783500131209
783600131111         exec sql
783700131111         DECLARE ARB  cursor for S1;
783800131111
783900131111       //?Apertura del cursore
784000131111         exec sql
784100131111         OPEN ARB;
784200131111
784300131111         IF sqlcode < 0;
784400131111           wEnd = *on;
784500131209           leavesr        ;
784600131111         ENDIF;
784700131209
784800131209           WopenC='1'     ;
784900131209
785000131111
785100131111       ENDSR;
785200131209       //--------------------------------------------------------------
785300131209       //?Raddoppio apici nella stringa                                ?
785400131209       //--------------------------------------------------------------
785500131209       BEGSR  Apici     ;
785600131209       ap=1  ;
785700131209       dow ap <=60  ;
785800131209       if %subst(w060a:ap:1)=''''  ;
785900131209       w060a=%replace('''''':w060a:ap:1)  ;
786000131209       ap= ap   +1       ;
786100131209       endif  ;
786200131209
786300131209       ap= ap   +1       ;
786400131209       enddo  ;
786500131209
786600131209       ENDSR;
786700131111       //--------------------------------------------------------------
786800131111       //?Chiusura cursore.
786900131111       //--------------------------------------------------------------
787000131111       BEGSR CloseCursor;
787100131111
787200131111       //?Chiusura cursore SQL?
787300131111         exec sql
787400131111           close ARB;
787500131209           clear WopenC   ;
787600131209           Wend=*off      ;
787700131111
787800131111       ENDSR;
787900131111       //--------------------------------------------------------------
788000131111       //?Lettura cursore.                                             ?
788100131111       //--------------------------------------------------------------
788200131111       BEGSR  ReadCursor;
788300131111
788400131111         clear  FNARBds;
788500131113         *in30=*off    ;
788600131113         *in32=*off    ;
788700131111
788800131111         exec sql
788900131111         FETCH next from ARB into :FNARBds;
789000131111
789100131111         IF sqlcod = 100 or sqlcod < 0;
789200131111           wEnd = *on;
789300131111         ENDIF;
789400131111
789500131111       ENDSR;
789600141010       //--------------------------------------------------------------
789700141010       //?Window per f19=Dispo di consegna da esaminare
789800141010       //--------------------------------------------------------------
789900141010       BEGSR sr_wdwdc   ;
790000141010          $fine=*off;
790100141105          clear fnlry09ds2;
790200141010          ilry09tch='C';
790300141010          ILRY09AAS=d48aas ;
790400141010          ILRY09LNP=d48LNP;
790500141010          ILRY09NRS=d48NRS ;
790600141010          ILRY09NSP=d48NSP ;
790700141105          kpjbu=fnlry09ds2;
790800141105          fnlry09r(kpjba);
790900141105          fnlry09ds2=kpjbu;
791000141010          if olry09esit='1' ;
791100141010             dow $fine=*off;
791200141010                exfmt lri6wddc;
791300141010                   if *inkt;
791400141105                       clear fnlry09ds2;
791500141010                       ilry09tch='E';
791600141010                       ILRY09AAS=d48aas ;
791700141010                       ILRY09LNP=d48LNP;
791800141010                       ILRY09NRS=d48NRS ;
791900141010                       ILRY09NSP=d48NSP ;
792000141105                       ILRY09DF06='S';
792100141105                       kpjbu=fnlry09ds2;
792200141105                       fnlry09r(kpjba);
792300141010                       $fine=*on;
792400141010                   endif;
792500141010             enddo;
792600141010          endif ;
792700141010
792800141010       ENDSR;
792900131111
793000131111
793100131111
793200131111      /end-free
793300131111
793400940713      **************************************************************************
793500941014**        12345678901234567890123456789012345678901234567890
793600950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
793700930413**
793800950316LEGATA A PIU' BOLLE                       - F18=Visualizza
793900920326**
794000950316 Non Presente
794100950316 F18=Visualizza
794200940907Vedi in Int.Par
794300940907Vedi in Int.Arr
794400941011Vedi Arch.Stor.
794500940907**
794600940907Spedizione non Presente
794700941230
794800940907Vedi in Int.Bolle Part.
794900940907Vedi in Int.Bolle Arr.
795000941011Vedi in Archiv. Storico
795100940713**  MESSAGGI DI ERRORE VIDEO  **************************************************
795200940713Nessuna bolla da visualizzare                                                  1
795300940713Immettere linea di arrivo valida                                               2
795400131114Effettuare ulteriori parzializzazioni                                          3
795500940713Immettere linea di arrivo o linea di partenza                                  4
795600940713Immettere linea di partenza valida                                             5
795700940713Numero spedizione inesistente                                                  6
795800940713Immettere anno spedizione                                                      7
795900940713Immettere data formalmente valida                                              8
796000040216Immettere tipo bolla valido                                                    9
796100940713Immettere data di spedizione iniziale minore o uguale a quella finale         10
796200940713Immettere numero di serie iniziale minore o uguale a quello finale            11
796300940713Immettere numero segnacollo iniziale minore o uguale a quello finale          12
796400940713Immettere numero segnacollo finale, se immesso quello iniziale                13
796500131114Scelta incongruente tra stato spedizioni  e richiesta                         14
796600940713Fine scorrimento                                                              15
796700940715Immettere tipo servizio bolle valido                                          16
796800940923Immettere filiale segnacollo valida                                           17
796900060614Riferimento partner estero errato x bolle DPD:o lungo 11 numerico a 14 alfanum
797000941212Bolla non manutenzionabile: in gestione ad altra filiale                      19
797100991227Il riferimento partner estero per bolle DPD deve avere massimo 12 numeri
797200010508Immettere p.o. di emissione valido                                            21
797300010510Non trovate bolle per l' O.R.M. richiesto                                     22
797400020626Immettere la nazione e la provincia del destinatario                          23
797500020627Nazione destinatario errata                                                   24
797600020627Provincia destinatario errata                                                 25
797700060130Utente non abilitato alla Variazione bolle                                    26
797800060130Utente non abilitato alla variazione bolle del p.o. in gestione               27
797900060613ATTENZIONE!Saranno selez.bolle con Nazione e RagSoc impostate ma PROVINC.vuota28
798000070420Stampa LDV effettuata ...                                                     29
798100070420Stampa LDV al momento non effettuabile: ritentare o telefonare in Sede        30
798200080201Assegnato non tassato: stampa LdV non effettuabile                            31
798300131111non trovata bolla per il barcde digitato                                      32
798400030612**  cmA4
798500030612OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
798600111221 SHARE(*YES) USRDTA('Man.BolA4')
798700030612**  cmA5
798800030612OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
798900111221 SHARE(*YES) USRDTA('Man.BolA5')
799000070426**  cmA4f6
799100111221OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
799200111221USRDTA('Rist.Bolle') SECURE(*YES)
799300070426**  cmA5f6
799400070426OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
799500111221             USRDTA('Rist.Bolle') SECURE(*YES)
799600030612** cnm
799700991222 0123456789
799800040218** TF                                                                        *
799900070420F1=Agg.Rich#                                                                    1
800000070411** TF01B                                                                     *
800100070411F1=SCELTA BOLLA#                                                                1
800200021216** TF                                                                        *
800300021218F2=C.A.#                                                                        2
800400021216** TF                                                                        *
800500021216F4=Tass2#                                                                       4
800600021216** TF                                                                        *
800700021216F5=Eventi#                                                                      5
800800070420** TF                                                                        *
800900070420F6=StampaLDV#                                                                   5
801000021216** TF                                                                        *
801100021217F7=Anomalie#                                                                    7
801200021216** TF                                                                        *
801300021219F8=Discord#                                                                     8
801400130830** TF                                                                        *
801500130830F9=OrariSr#                                                                     9
801600021216** TF                                                                        *
801700070420F10=Variaz#                                                                     10
801800021216** TF                                                                        *
801900030121F11=NoteS/B#                                                                    11
802000021217** TF                                                                        *
802100021217F12=Ritorno#                                                                    11
802200021216** TF                                                                        *
802300070420F13=Tassaz#                                                                     13
802400021217** TF                                                                        *
802500021216F14=Giacenza#                                                                   14
802600040218** TF                                                                        *
802700070420F16=Gest.Rich#                                                                  16
802800070404** TF                                                                        *
802900140311F17=NoteAut#                                                                    16
803000021216** TF                                                                        *
803100021216F19=Spunte#                                                                     19
803200021216** TF                                                                        *
803300070420F20=Affid#                                                                      19
803400021216** TF                                                                        *
803500070420F21=DetColli#                                                                   19
803600110928** TF                                                                        *
803700110930F22=PerfectOrder#                                                               19
