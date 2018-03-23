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
034100991027     D FIDN00DS      E DS
034200091216     D TRULINTDS     E DS
034300991027     D DS7PQRS       E DS
034400991027     D FNLG30DS      E DS
034500991027     D FNLr44DS      E DS
034600130920
034700131114     d fipctcetds    e ds
034800140224     d fipctcords    e ds
034900140110     D TRULORSDS     E DS                  prefix(S_)
035000131114     D TRULORFDS     E DS
035100130920
035200961121     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
035300991027     D FNLR45DS      E DS
035400950102     D FNLR97        E DS
035500930428     D*
035600930428     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
035700991027     D TNSD99DS      E DS                  INZ
035800941014      *
035900030612     D* DS PER TRUL90R - RICHIESTA STAMPANTI
036000030612     D TRUL90DS      E DS
036100941014      *
036200941212     D* DS PER TRUL06R - CARICAMENTO £X
036300991027     D TRUL06DS      E DS
036400991027     D  LIN                    1     90  0 DIM(30)
036500941212      *
036600941014     D* DS PER FNLR66R - CONTROLLO E VISUALIZZAZIONE CAUSALI VARIAZIONI
036700991027     D FNLR66DS      E DS
036800060224      *
036900060224     D* DS PER FNLv55R - richiamo pgm che cerca i terminal per £6
037000060224     D fnlv55ds      E DS
037100941014      *
037200941014     D* DS PER FNLR48R - MANUTENZIONE BOLLE ARRIVI
037300991027     D FNLR48DS      E DS
037400941014      *
037500941212     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
037600991027     D FNLV50DS      E DS
037700051124     D* DS PER FNLV80R - decodifica cons richiesta
037800051124     D FNLV80DS      E DS
037900131120     D* DS PER FNLRG7R - visulizza altre abilitazioni
038000131120     D fnlrg7ds      E DS
038100070404     D* DS PER FNLRN5R - visulizza note LDV
038200140123     D**** fnlrn5ds      E DS
038300140123     D* DS PER FNLRN6R - visulizza note LDV
038400140123     D fnlrn6ds      E DS
038500070427     D* DS PER FIDG34R - reperisce dati consegna automatica
038600070427     D fidg34ds      E DS
038700070420     D* DS PER           stampa LDV
038800070420     D FNLSB5DS      E DS
038900130308     d FNLSB6ds1     e ds                  inz
039000130319     d  DB6pdf       e                     inz('X')
039100941212      *
039200131119     D DDSTFLR       E DS                  INZ
039300010510     D Dsbl4m        E DS
039400030121     d dTba          e ds
039500031119     d dAr5Gen       e ds
039600030121     d dAr5Ban       e ds
039700030703     d dAr5Bnb       e ds
039800130927     d dAr5emd       e ds
039900030121     d Tibs02Ds      e ds
040000060130     d Azuteds       e ds                  extname(Azute00f)
040100060130     d dDatiute      e ds
040200060130     d dLat          e ds
040300060130     d dute01        e ds
040400060130     D TRUL31DS      E DS
040500060130     D TRUL31DS2     E DS
040600060130     D TIBS34DS      E DS
040700110601     D fnlv79ds      E DS
040701141010      // -?Reperimento Disposizioni di Consegna da WEB
040702141105     d FNLRY09DS2    e ds                  inz
040800040809      * ds per il controllo della presenza di CA per la spedizione richiesta
040900040809     d FIDN12DS      e ds
041000040809     d  skKey                 26    305    dim(20)
041100040809     d  skAnn                306    325    dim(20)
041200040809     d  skDca                326    485  0 dim(20)
041300040809     d  skFca                486    545  0 dim(20)
041400040809     d  skDch                546    705  0 dim(20)
041500040809     d  skCch                706    745    dim(20)
041600040809     d*
041700040809     d dsKey           ds
041800040809     d  dsaac                         4  0
041900040809     d  dsfil                         3  0
042000040809     d  dsnca                         7  0
042100131111
042200131111       // -?File bolle arrivi
042300131111     d FNARBds       e ds                  extname(FNARB00F)
042400131111     d
042500131111     d wEnd            s               n   inz(*off)
042600040809
042700991027      *
042800991027     D W017A           s             17
042900991027     D w0130           s             13  0
043000991027     D w0133           s             13  3
043100991027     D IMPeditato      s             13
043200060130     D savkpjbu        S                   LIKE(kpjbu)
043300060130     D kscfil          S                   LIKE(arblna)
043400060130     D War4aas         S                   LIKE(AR4aas)
043500000228     D War4lnp         S                   LIKE(AR4lnp)
043600000228     D War4nrs         S                   LIKE(AR4nrs)
043700000228     D War4nsp         S                   LIKE(AR4nsp)
043800991027     D Wsvn            S                   LIKE(AR7svn)
043900991027     D Wvan            S                   LIKE(AR7van)
044000991027     D Wdiv            S                   LIKE(AR6div)
044100991029     D Wariimp         S                   LIKE(ARIimp)
044200991029     D Wariipp         S                   LIKE(ARIipp)
044300020906     d wfgs            s                   Like(Fvvfgs)
044400991027     D DATA_eur        S               D   DATFMT(*eur)
044500021203     d kpdpdr          S                   LIKE(APDpdr)
044600021203     d kpdtip          S                   LIKE(APDtip)
044700131120     d kndc6           S                   LIKE(dstnfv)
044800140110     d s_arbdei        S                   LIKE(d98dei)
044900140110     d s_arboei        S                   LIKE(d98oei)
045000131114     D arbrsduni       S             70
045100131114     D arbdsp          S              8  0
045200131111     D W0080           S              8  0
045300991027     D W0040           S              4  0
045400991027     D wtibs69r        s              1
045500991027     D wtstdec         s              1
045600131120     D Wvideo          s              1
045700131209     D WopenC          s              1
045800020225     d lnantw          s                   like(§ogntw)
045900020225     d lnpntw          s                   like(§ogntw)
046000020910     d Wifp            s                   Like(Arbifp) Inz
046100060130     d wabi            s                   like(UteAut)
046200030703
046300030703     d wtba            s                   Like(§Ar5Tba)
046400030703     d wdestba         s                   Like(VidTba)
046500061018     d savnzd          s                   inz  like(v1cnzd)
046600020628
046700091216     D W_cdi           S                   LIKE(§ar5cdi)
046800020628     D ordina          S              1
046900020628     D comando         S              1
047000070420     D wrichtrul90     S              1
047100070403     d Dataeur         s               d   datfmt(*eur)
047200070403     d Datadmy         s               d   datfmt(*dmy)
047300110929     d s_param1        s                   like(param1)
047400110929     d no_f22          s              1    inz(' ')
047500110930     d comrma          s                   like(arbrma)
047600131108     d Wcol_D          s              5  0
047700131108     d Wcol_I          s              5  0
047800131108     d Wcarat          s              3  0
047900131108     d Indx            s              3  0
048000131209     d AP              s              3  0
048100131108     d wx              s              3  0
048200131007     d wy              s              3  0
048300131209     d w060a           s             60
048400131209     d wabif9          s              1
048401141010     d $Fine           s              1    inz(*off)
048402141010       // -?Reperimento Disposizioni di consegna da WEB
048403141010     d fnlry09r        pr                  extpgm('FNLRY09R')
048404141013     d  kpjba                              likeds(kpjba)
048500131111     d
048600131111       // -?Stringa SQL da eseguire
048700131111     d wSQL            s           2048    Varying        inz
048800030527     D*
048900030527     D wrkoggiiso      s               d
049000991027      *
049100930705     D* DEFINIZIONI COSTANTI
049200930705     D*
049300930705     D CNOSEQ          C                   CONST('NON SEQUENZIALI')
049400941014     D CINTER          C                   CONST('* INTERROGAZIONE BOL-
049500941014     D                                     LE ARRIVI *')
049600941014     D CMANUT          C                   CONST('** MANUTENZIONE BOLL-
049700941014     D                                     E ARRIVI **')
049800021216     D CF2413          C                   CONST('F24=AlTasti(1/3)')
049900021216     D CF2423          C                   CONST('F24=AlTasti(2/3)')
050000021216     D CF2433          C                   CONST('F24=AlTasti(3/3)')
050100021216     D CF2412          C                   CONST('F24=AlTasti(1/2)')
050200021216     D CF2422          C                   CONST('F24=AlTasti(2/2)')
050300110705     d
050400110705     D* CAMPO PER DSPATR "NO DISPLAY"
050500110705     D NODI            C                   CONST(X'27')
050600131120     D* CAMPO PER DSPATR "RI"
050700131120     D RI              C                   CONST(X'21')
050800131127     D* CAMPO PER DSPATR "HI"
050900131127     D hi              C                   CONST(X'22')
051000900518     C****************************************************************
051100900518     C*  RIEPILOGO INDICATORI
051200900518     C***************************************************************
051300911023     C* 01    - SELEZIONE PER LINEA ARRIVO
051400911023     C* 02    - SELEZIONE PER LINEA PARTENZA
051500911105     C* 03    - SPEDIZIONE GIACENTE
051600990119     C* 04    - ABILITO TASTO FUNZIONALE F2 - INT C.A.
051700990119     C* 05    - ABILITO TASTO FUNZIONALE F19- SPUNTE
051800070411     c* 06    - on per tassazione 2 bolla emette scritta "ImportoBOLLO"
051900070411     c*       - offper tassazione 2 bolla emette scritta "Importo IVA"
052000911023     C* 07    - PARZIALIZZ. SEGNACOLLO DAL
052100000216     C* 08    - bolla dpd
052200131114     C* 09    - scelta in altre parzializz
052300961111     C* 10    - BOLLA DI RIENTRO
052400911023     C* 11    - PARZIALIZZ. MITTENTE
052500131120     C* 12    -
052600961111     C* 13    - SPEDIZIONE IN CONSEGNA IN EVIDENZA IN SUBFILE
052700911023     C* 14    - PARZIALIZZ. DESTINATARIO
052800940318     C* 15    - DATA DAL UGUALE A DATA AL (SCELTO UN SOLO GIORNO)
052900961112     C* 16    - SI TRATTA DI BOLLA EXPORT
053000070403     C* 17    - p.o. utente e lna bolla hanno GEO attiva
053100911023     C* 18/21 - STATO SPEDIZIONI SELEZIONATO
053200991027     C* 22    - ESISTE ESTENSIONE BOLLA 2 e ABILITO IL TASTO  F4=TASSAZIONE 2
053300950825     C* 23    - SFLEND
053400950825     C* 24    - ABILITO IL TASTO "F13=TASSAZIONE"
053500010508     C* 25    - SELEZIONE PER NUMERO O.R.M.
053600070404     C* 26    - presente FIARP
053700070403     C* 27    - presente blocco consegna ARBFBC<>' '
053800941028     C* 28    - VISUALIZZA MSG ERRORE
053900911129     C* 29    - ESISTE CONTRASSEGNO
054000911023     C* 30/31 - LETTURA COMODO
054100911023     C* 32    - BOLLA LETTA DA SCARTARE
054200911023     C* 33    - BOLLA IN FRANCO
054300911023     C* 34    - LETTURA SUBFILE
054400911023     C* 35    - PULIZIA SUBFILE
054500911129     C* 36    - LETTURE
054600911023     C* 37    - ROLL UP
054700991027     C* 38    - ESISTE TB2 ANCHE SE NON PRESENTE TIPO RECORD '2' IN FIAR600F
054800920203     C* 39    - ABILITAZIONE TASTO DI COMANDO INT. BOLLE PARTENZE
054900131111     C* 40-59 - ERRORI
055000131111     C* 60    - email destiantario
055100131111     C* 61    - PROGRAMMA RICHIAMATO per scelta bolla
055200930407     C* 62    - ALTA INTENSITA' PER SPED BLOCCATE
055300040218     C* 63    - Abilita F1  = Aggiunta in reclamo
055400040218     C* 64    - Abilita F16 = Gestione reclamo
055500091214     c* 65    - Abilita visualizzazione beneficiario C/Ass
055600131127     C* 66    - libero - NON usato
055700960903     C* 67    - ABILITAZIONE TASTO DI COMANDO F7=ANOMALIE
055800131127     C* 68    - pos cursore RMN prima serv x  EMETTERE F23 PER CALCOLARE E VISUALIZZARE DATA CONS.
055900930505     C*         PREVISTA E LE ORE DI RITARDO
056000930526     C* 69    - DATA CONSEGNA MERCE >< 0
056100930510     C* 70    - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
056200990119     C* 71    - ABILITAZIONE TASTO DI COMANDO F8=EVENTI SEGNACOLLI
056300131107     C* 72    - SFLDSPCTL  LRI6S05
056400991027     C* 73    - BOLLA FRANCO FRONTIERA
056500970227     C* 74    - ABILITA EVENTI SEGNACOLLI
056600970227     C* 75    - UTILIZZATO PER NON ESEGUIRE LA SUBROUTINE "LEGBOL"
056700970227     C* 76    - SI DDT IN ALTA INTENSITA'
056800021213     C* 77    - NOTE SUPERMERCATI
056900970227     C* 78    - SFLDSP
057000950825     C* 79    - SFLDSPCTL
057100930510     C* 80    - NEL SFL DEI LEGAMI NON EMETTO IL CAMPO PER LA SELEZIONE
057200941028     C* 81    - USATO NEL VIDEO LEGAMI
057300941028     C* 82    - USATO NEL VIDEO LEGAMI
057400941028     C* 83    - USATO NEL VIDEO LEGAMI
057500970925     C* 84    - PARZIALIZZ. SEGNACOLLO AL
057600971219     C* 85    - VISUALIZZO RIFERIMENTO PARTNER ESTERO
057700111005     c* 86    - Abilitazione F22="PERFECT ORDER"
057800111005     C* 87    - Pgm richiamato in modalità "W" (Perfect Order Wurth)
057900110705     C* 88    -
058000931112     C* 89    - UTILIZZATO PER VISUALIZZARE ALTRI TASTI FUNZIONALI
058100920402     C* 90    - INDICA LA PRESENZA DI UN ERRORE
058200941028     C* 91    - ERRORE QCMDEXC (UTILIZZABILE)
058300940923     C* 92/94 - LEGAME BOLLA
058400930525     C* 95    - ESISTONO EVENTI DA INTERROGARE
058500131111     C* 96    -
058600991222     C* 97    - MANUTENZIONE BOLLE ARRIVI
058700941014     C* 98    - ESISTONO VARIAZIONI BOLLE EFFETTUATE
058800930504     C* 99    - OFF  ESISTE LEGAME BOLLA SEGUENTE
058900900518     C*****************************************************************
059000000000     C     *ENTRY        PLIST
059100000000     C                   PARM                    KPJBA
059200040218     c                   parm                    FIDNA1ds
059300131210     c
059400131210      /free
059500131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
059600131210      /end-free
059700980609     C**
059800940712     C                   MOVEL     KPJBU         PARAM1
059900980609     C                   MOVEL     KPJBU         PARAAZ
060000940713     C*
060100060130     C                   Z-ADD     1             CODUT             1 0
060200060130     c                   Movel     rsut          V1cdsa
060300060130     C                   MOVEL     rsut          PARDUT           20
060400941227     C*
060500941227     C                   MOVEL     SIMFEL        PA1FEL
060600990701     C                   MOVEL     V1CDSA        PA1DSA
060700940713     C*-------------------------------------------------------------------------
060800911024     C                   EXSR      CARTAB
060900131111
061000131111     c                   eval      Wend=*off
061100110930     c*
061200111005     c                   setoff                                       87
061300110930     c                   if        pa1flg='W'
061400110930     c                   eval      comrma=pa1rma
061500111005     c                   eval      *in87=*on
061600110930     c                   endif
061700110930     c* disabilito f22=PerfectOrder se richiamato da sè stesso
061800131107     c                   if        pa1pgm='FNLRI6R'
061900110929     c                   eval      no_f22='N'
062000110929     c                   endif
062100101202     c                   if        pa1pgm=*blanks
062200131107     C                   MOVEL     'FNLRI6R'     PA1PGM
062300101202     c                   endif
062400940713     C                   MOVE      'N'           V1CAFF
062500941215     C                   MOVEL     '? '          V1CCVB
062600941215     C                   CLEAR                   V1DCVB
062700941014     C* TESTATA
062800941014     C                   MOVEL     CINTER        V1CTES
062900131127     c                   clear                   v1clna
063000940317      *
063100940712      * IMPOSTO LA LINEA DI ARRIVO
063200060217     c* Non la imposto se richiamato con key specifica
063300040217if  1C     PA1FLG        IFNE      '1'
063400060217     C     PA1FLG        ANDNE     '3'
063500060217     C     PA1FLG        ANDNE     'C'
063600060217     C     PA1FLG        ANDNE     'N'
063700960903     C     PA1FLG        ANDNE     'P'
063800961121     C     PA1FLG        ANDNE     'E'
063900110929     C     PA1FLG        ANDNE     'W'
064000060217     c* o se vengo da int bolle normale (per assistenza)
064100060217     c     pa1flg        andne     ' '
064200060217     c     pa1flg        andne     '4'
064300060130     c                   z-add     dutpou        v1clna
064400020424     c*
064500040121     C                   MOVE      V1CLNA        NUM3              3 0
064600940712     C     NUM3          CHAIN     AZORG01L                           90
064700940713     C  N90              MOVEL     ORGDES        V1CDEA
064800040217e   1C                   ENDIF
064900930702     C*
065000920703     C                   EXSR      PUL01
065100940809     C                   MOVEL     'S'           WRIEM             1
065200930702     C**
065300930702     C* PGM RICHIAMATO
065400920326     C**
065500940713     C** PA1FLG =   - NON RICHIAMATO
065600920423     C**        = 1 - RICHIAMATO CON   KEY SPECIFICA
065700920423     C**        = 2 - RICHIAMATO SENZA KEY SPECIFICA PER   SELEZ BOLLA
065800920423     C**        = 3 - RICHIAMATO CON   KEY SPECIFICA DA INT-BOLL PART
065900920423     C**        = 4 - RICHIAMATO SENZA KEY SPECIFICA SENZA SELEZ BOLLA
066000990129     C**        = C - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F2=C.A
066100941014     C**        = M - MANUTENZIONE BOLLE ARRIVI
066200960903     C**        = N - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F7=ANOMA
066300960903     C**        = P - RICHIAMATO CON   KEY SPECIFICA DA INT-BOLL PART
066400960903     C**              E DISABILITAZ F7=ANOMALIE
066500961121     C**        = E - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F8=EV.SEGNACOLLI
066600110930     C**        = W - RICHIAMATO SENZA KEY SPECIFICA per "PerfectOrder"
066700960903     C*
066800040218      *?Abilito F1=Aggiunta in Reclami?
066900040218      * (se richiamato da pgm. FIDNA1R...)
067000040220     c                   eval      *in63  = *off
067100040303     c                   if            SDSprm >  1
067200040308     c                             and IA1ins = ' '
067300040220     c                   eval      *in63  = *on
067400060301     c                   clear                   v1clna
067500060301     c                   clear                   v1cdea
067600111003     c                   endif
067700960903     C* ABILITO F7=ANOMALIE SE FLAG DIVERSO DA 'N' E 'P'
067800960903     C     PA1FLG        IFNE      'N'
067900960903     C     PA1FLG        ANDNE     'P'
068000960903     C                   SETON                                        67
068100040217     C                   ENDIF
068200961121     C* ABILITO F8=EVENTI SEGNACOLLI  SE FLAG DIVERSO DA 'E'
068300961121     C     PA1FLG        IFNE      'E'
068400961121     C                   SETON                                        74
068500040217     C                   ENDIF
068600960903     C*
068700040217if  1C     PA1FLG        IFEQ      '1'
068800960903     C     PA1FLG        OREQ      'N'
068900940713     C     PA1FLG        OREQ      '3'
069000960903     C     PA1FLG        OREQ      'P'
069100961121     C     PA1FLG        OREQ      'E'
069200990129     C     PA1FLG        OREQ      'C'
069300920703     C*
069400940714     C                   MOVEL     V2CAAS        V1CAAS
069500940713     C                   MOVEL     COMLNP        V1CLP2
069600940714     C                   MOVEL     V2CNRS        V1CNRS
069700940714     C                   MOVEL     V2CNSP        V1CNSP
069800920703     C* SE NO PASSATA KEY SPECIFICA SI PUO' SELEZIONARE
069900940713     C     PA1FLG        IFEQ      '1'
070000960903     C     PA1FLG        OREQ      'N'
070100961121     C     PA1FLG        OREQ      'E'
070200990129     C     PA1FLG        OREQ      'C'
070300070411     c                   if        v1cnsp=0
070400920703     C                   SETON                                        61
070500040217     C                   ENDIF
070600070411     C                   ENDIF
070700040217x   1C                   ELSE
070800110929     c* Se richiamata con "W" devo saltare tutta la gestione della
070900110929     c* prima videata e devo passare subito al caricamento del sfl
071000110929   1ac     pa1flg        ifne      'W'
071100911213     C*
071200940713     C     PA1FLG        IFEQ      '2'
071300911213     C                   SETON                                        61
071400040217     C                   ENDIF
071500920203     C*
071600980603     C                   SETOFF                                       05
071700040217if  2C     PA1FLG        IFEQ      ' '
071800920203     C                   SETON                                        39
071900040217e   2C                   ENDIF
072000941014     C*
072100000616     C* MANUTENZIONE BOLLE ARRIVI 97=ON
072200040217if  2C     PA1FLG        IFEQ      'M'
072300030612      *
072400941014     C                   SETON                                        97
072500030612     C                   MOVEL     CMANUT        V1CTES
072600070420     c
072700040217e   2C                   ENDIF
072800000703     C*
072900941209     C     INIZIO        TAG
073000131114     C***                EXSR      PUL01
073100131114
073200131114     c* se cursore aperto --> lo chiudo
073300131209     c*****              if        Wend=*on  or *in14 or *in11
073400131209     c                   if        WopenC='1'
073500131114     c                   exsr      closecursor
073600131114     c                   endif
073700911025     C*
073800900509     C     FOR01         TAG
073900941209     C*
074000040217if  2C     WRIEM         IFEQ      'S'
074100131107     C                   WRITE     LRI6T01
074200940720     C                   MOVEL     *BLANKS       WRIEM
074300040217e   2C                   ENDIF
074400131107     C                   EXFMT     LRI6D01
074500940713     C*
074600940713     C                   SETOFF                                       28
074700940715     C                   MOVEL     *BLANKS       WRIEM
074800940713     C*
074900980603     C** F3 - FINE LAVORO
075000040217if  2C     *INKC         IFEQ      '1'
075100980603     C   05              MOVEL     '1'           PARF3
075200980603     C                   GOTO      FINE
075300040217e   2C                   ENDIF
075400131115     C**
075500131115     C** F5 - Altre parzializzazioni
075600131115     c                   if        *inke=*on
075700131115     c                   exsr      AltrePar
075800131115     C                   MOVEL     'S'           WRIEM
075900131115     C                   GOTO      FOR01
076000131115     c                   endif
076100131114     C**
076200980603     C** F7 - INTERROGAZIONE BOLLE PARTENZE
076300040217if  2C     *INKG         IFEQ      *ON
076400930922     C                   EXSR      INTPAR
076500940722     C                   MOVEL     'S'           WRIEM
076600920203     C                   GOTO      FOR01
076700040217e   2C                   ENDIF
076800131114     C**
076900131114     C** F10- pulizia di tutti i campi di scelta
077000131114if  2C     *INKj         IFEQ      *ON
077100131114     C                   EXSR      pul01
077200131114     C                   MOVEL     'S'           WRIEM
077300131114     C                   GOTO      FOR01
077400131114e   2C                   ENDIF
077500930922     C**
077600980603     C** F14- INTERROGAZIONE BOLLE TRANSITO
077700040217do  2C   KN              DO
077800940722     C                   MOVEL     'T'           PA2BOL
077900920423     C                   MOVEL     PARAM2        KPJBU
078000940721     C                   CALL      'FNLR68R'
078100920423     C                   PARM                    KPJBA
078200940722     C                   MOVEL     'S'           WRIEM
078300920423     C                   GOTO      FOR01
078400040217e   2C                   ENDDO
078500911025     C*
078600110929e  1ac                   endif
078700040217e   1C                   ENDIF
078800900509     C* CONTROLLI FORMATO1
078900110930     c     pa1flg        ifne      'W'
079000900509     C                   EXSR      CONTR
079100900509     C   90              GOTO      FOR01
079200110929     c                   endif
079300911025     C**
079400911025     C** P E R   N U M E R O   S P E D I Z I O N E
079500131111     c**  o      B A R C O D E
079600040217if  1C     SEL2          IFNE      0
079700131111     c     v1cbarcode    orgt      0
079800971223     C                   EXSR      SINGOL
079900971223     C     WINIZ         IFEQ      '1'
080000971223     C                   GOTO      INIZIO
080100971223     C                   ENDIF
080200971223     C     WFINE         IFEQ      '1'
080300971223     C                   GOTO      FINE
080400971223     C                   ENDIF
080500040217e   1C                   ENDIF
080600911025     C**
080700911025     C**  P E R   S U B F I L E
080800911023     C                   EXSR      ELAB
080900941014     C*
081000901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
081100040217if  1C     NR2           IFEQ      0
081200940713     C                   MOVEL     MSG(1)        V1CMSG
081300131115     C                   SETON                                        28
081400131115     c                   select
081500131115     c                   when      v1crpe<>*blanks
081600131115     C                   SETON                                        58
081700131115     c                   when      v1crmn>0 or v1crma<>*blanks
081800131127     C                   SETON                                        68
081900131115     c                   when      v1cnot<>*blanks
082000131115     C                   SETON                                        62
082100131115     c                   when      v1cprd<>*blanks
082200131115     C                   SETON                                        50
082300131115     c                   other
082400131115     C                   SETON                                        51
082500131115     c                   endsl
082600900516     C                   GOTO      FOR01
082700040217e   1C                   ENDIF
082800971223     C**
082900040217if  1C     V1CRPE        IFNE      *BLANKS
083000010509     C     NR2           ANDEQ     1
083100010509     c     *in25         oreq      *on
083200971223     C     NR2           ANDEQ     1
083300111011     c     v1crmn        orgt      *zeros
083400111011     C     NR2           ANDEQ     1
083500111011     c     v1crma        orne      *blanks
083600111011     C     NR2           ANDEQ     1
083700131111     c     v1cnot        orne      *blanks
083800131111     C     NR2           ANDEQ     1
083900111011     C                   MOVEL     v2clnp        COMLNP
084000111011     c     karb          chain     fnarb01l
084100971223     C                   EXSR      SINGOL
084200040217     C     WINIZ         IFEQ      '1'
084300971223     C                   GOTO      INIZIO
084400040217     C                   ENDIF
084500040217     C     WFINE         IFEQ      '1'
084600971223     C                   GOTO      FINE
084700040217     C                   ENDIF
084800040217e   1C                   ENDIF
084900930922     C**
085000900518     C     FORCT2        TAG
085100131107     C                   WRITE     LRI6T01
085200131107     C                   WRITE     LRI6Z02
085300900515     C** COMANDI SUBFILE
085400900515     C     FORCT         TAG
085500131107     C                   EXFMT     LRI6C02
085600940923     C                   SETOFF                                       28
085700941109     C                   CLEAR                   PA1F03
085800980603     C* F3 - FINE
085900980603     C     *INKC         IFEQ      '1'
086000980603     C   05              MOVEL     '1'           PARF3
086100980603     C                   GOTO      FINE
086200980603     C                   ENDIF
086300930922     C*
086400980603     C* F12 - RITORNO
086500040217if  1C     *INKL         IFEQ      *ON
086600110930     c     pa1flg        cabeq     'W'           fine
086700940713     C     PA1FLG        IFEQ      '3'
086800960903     C     PA1FLG        OREQ      'P'
086900940713     C     PA1FLG        OREQ      '1'
087000960903     C     PA1FLG        OREQ      'N'
087100961121     C     PA1FLG        OREQ      'E'
087200990129     C     PA1FLG        OREQ      'C'
087300940714     C                   Z-ADD     0             V2CAAS
087400920702     C                   Z-ADD     0             COMLNP
087500940714     C                   Z-ADD     0             V2CNRS
087600940714     C                   Z-ADD     0             V2CNSP
087700930922     C                   ENDIF
087800930922     C                   GOTO      INIZIO
087900040217e   1C                   ENDIF
088000900515     C* ROLLUP
088100040217if  1C     *IN37         IFEQ      *ON
088200940713     C   30              DO
088300940713     C                   MOVEL     MSG(15)       V1CMSG
088400940713     C                   SETON                                        28
088500940713     C                   ENDDO
088600930922     C  N30              EXSR      CARSF
088700940713     C                   Z-ADD     COMOD         REC2
088800930922     C                   GOTO      FORCT
088900040217e   1C                   ENDIF
089000900515     C*
089100900516     C** CONTROLLO SE EFFETTUATA UNA SCELTA
089200940713     C                   Z-ADD     PRIMA         REC2
089300131107     C                   READC     LRI6S02                                34
089400911023     C* VISUALIZZO BOLLA
089500040217do  1C     *IN34         DOWEQ     *OFF
089600940714     C                   MOVEL     *BLANKS       V2CFLG
089700911023     C*
089800040217if  2C     V2CSCE        IFEQ      '5'
089900940714     C                   Z-ADD     V2CLNP        COMLNP
090000940712     C     KARB          CHAIN     FNARB01L                           31
090100930518     C*
090200940714     C                   MOVEL     *BLANKS       V2CSCE
090300040217     C     V2CCON        IFEQ      'IN CON'
090400930518     C                   SETON                                        13
090500040217     C                   END
090600930518     C*
090700040217     C     ARBCCA        IFNE      ' '
090800930518     C     ARBFBC        ORNE      ' '
090900930518     C                   SETON                                        62
091000040217     C                   END
091100941014     C*
091200131107     C                   UPDATE    LRI6S02
091300911023     C*
091400911023     C                   EXSR      BOLLA
091500911023     C                   EXSR      VISUAL
091600911023     C*
091700040217x   2C                   ELSE
091800941014     C*
091900941014     C* 97 ON - MANUTENZIONE BOLLE
092000040217if  3C     V2CSCE        IFEQ      '1'
092100941014     C*
092200040217if  4C     *IN97         IFEQ      *ON
092300060227     c                   movel     v2clna        w003a             3
092400060224     C     w003a         LOOKUP    $pog                                   31
092500060130    2c                   if        not *in31 and d66pocl='S'
092600060130     c     v2ccbo        lookup    cbodop                                 31
092700060130    3c                   if        not *in31
092800060130     c                   movel     v2cksc        kscfil
092900060130   x3c                   else
093000060130     c* Se bolla doppia prendo il ksc dall'AR6
093100060130     C                   Z-ADD     v2cAAS        KAAS
093200060130     C                   Z-ADD     v2cLNP        KLNP
093300060130     C                   Z-ADD     v2cNRS        KNRS
093400060130     C                   Z-ADD     v2cNSP        KNSP
093500060130     C                   MOVEL     '2'           KTRC
093600060130     C     KAR61         CHAIN     FIAR6000                           31
093700060130if  4C                   IF          %FOUND(fiar601l)
093800060130     c                   movel     ar6ksc        kscfil
093900060130     c                   else
094000060130     c                   movel     999           kscfil
094100060130    4c                   endif
094200060130    3c                   endif
094300130314     c*
094400060224     c                   movel     kscfil        w003a
094500060224     c     w003a         lookup    $pog                                   31
094600060130    2c                   endif
094700130314     c*
094800040217if  5C     *IN31         IFEQ      *OFF
094900941212     C                   MOVEL     MSG(19)       V1CMSG
095000941212     C                   SETON                                        599028
095100941212     C                   MOVEL     '2'           V2CFLG
095200130314     c*
095300040217x   5C                   ELSE
095301141010
095400130314     c*
095500991027     C                   CLEAR                   FNLR48DS
095600060224     c* come p.o. gestione prendo il p.o. gestore (da £6) della linea
095700060224     c*  di arrivo
095800060224     c                   clear                   fnlv55ds
095900060224     c                   movel     v2clna        d55lin
096000060224     c                   movel     '6'           d55tpt
096100060224     C                   time                    wrkoggiiso
096200060224     C     *iso          move      wrkoggiiso    d55drf
096300060224     c                   call      'FNLV55R'
096400060224     c                   parm                    fnlv55ds
096500060224     c                   movel     d55tfa        d48fgs
096600130314     c*
096700941014     C                   MOVEL     V2CAAS        D48AAS
096800941014     C                   MOVEL     V2CLNP        D48LNP
096900941014     C                   MOVEL     V2CNRS        D48NRS
097000941014     C                   MOVEL     V2CNSP        D48NSP
097001141010     c* routine per verifica dispo da esaminare
097002141010     c                   exsr      sr_wdwdc
097003141010
097100941014     C                   MOVEL     V2CCBO        D48CBO
097200941014     C                   MOVEL     V1CCVB        D48CVB
097300950616     C                   MOVEL     'V'           D48FFR
097400941227     C                   MOVEL     'A'           D48TBO
097500950607     C                   MOVEL     'S'           D48F12
097600941014     C*
097700991027     C                   MOVEL     FNLR48DS      KPJBU
097800941014     C                   CALL      'FNLR48R'
097900941014     C                   PARM                    KPJBA
098000941212     C                   PARM                    DSARBD
098100941212     C                   PARM                    DSARBK
098200991011     C                   PARM                    DARBT
098300941212     C                   PARM                    DSARBG
098400030612     C                   PARM                    trul90ds
098500941212     C*
098600991027     C                   MOVEL     KPJBU         FNLR48DS
098700941212     C*
098800941014     C* ERRORE IN MANUTENZIONE BOLLE
098900040217     C     D48ERR        IFEQ      '1'
099000941014     C                   MOVEL     D48MSG        V1CMSG
099100941014     C                   SETON                                        592890
099200941014     C                   MOVEL     '2'           V2CFLG
099300040217     C                   ENDIF
099400950114     C*
099500950114     C* SE NON C'E' ERRORE AGGIORNO IL SFL CON I DATI DELLA BOLLA VAR
099600040217     C     *IN90         IFEQ      *OFF
099700950120     C                   Z-ADD     V2CLNP        COMLNP
099800950114     C     KARB          CHAIN     FNARB01L                           31
099900950114     C                   MOVEL     ARBRSM        V2CRSM
100000950114     C                   MOVEL     ARBRSD        V2CRSD
100100131114     c                   if        v1cind=*blanks
100200131114     c                   eval      v2cdest=arblod
100300131114     c                   else
100400131114     c                   eval      v2cdest=arbind
100500131114     c                   endif
100600131114     c
100700040217     C                   ENDIF
100800040217e   5C                   ENDIF
100900941014     C*
101000040217x   4C                   ELSE
101100941014     C*
101200911023     C* SELEZIONATA BOLLA: PASSO KEY ED ESCO DA PGM
101300040217     C     *IN61         IFEQ      *ON
101400940714     C                   Z-ADD     V2CLNP        COMLNP
101500940714     C                   MOVEL     '3'           V2CFLG
101600040220      *?- Richiamato da pgm. FIDNA1R?
101700040220if  6c                   if        SDSprm  > 1
101800131111     c                   eval      wARBlnp  = V2Clnp
101900131111     c                   eval      wARBnrs  = V2Cnrs
102000131111     c                   eval      wARBnsp  = V2Cnsp
102100040220     c                   eval      wARBaas = V2Caas
102200040220     c                   clear                   FIDNA1ds
102300040220     c                   eval      IA1ins  = 'S'
102400040220     c                   eval      IA1tor  = 'S'
102500040220     c                   eval      IA1ogg  = ds_OggSped
102600040220e   6c                   endif
102700040217     C                   ENDIF
102800040217e   4C                   ENDIF
102900040217e   3C                   ENDIF
103000930518     C*
103100040217do  3C  N61              DO
103200940714     C                   MOVEL     *BLANKS       V2CSCE
103300040217     C     V2CCON        IFEQ      'IN CON'
103400930518     C                   SETON                                        13
103500930518     C                   ELSE
103600040217     C     V2CCON        IFNE      *BLANKS
103700940714     C                   TESTN                   V2CCON               31
103800930518     C  N31              SETON                                        62
103900040217     C                   END
104000040217     C                   END
104100131107     C                   UPDATE    LRI6S02
104200040217e   3C                   ENDDO
104300930518     C*
104400040217e   2C                   ENDIF
104500911022     C*
104600941014     C                   SETOFF                                       136259
104700911022     C*
104800911022     C* SE FLAG 2 TORNO AL SUBFILE
104900040217     C     V2CFLG        IFNE      *BLANKS
105000900530     C                   SETON                                        34
105100900530     C                   ELSE
105200131107     C                   READC     LRI6S02                                34
105300040217     C                   ENDIF
105400040217e   1C                   ENDDO
105500941014     C*
105600911023     C* FLG =  3 ==> FILE PGM E PASSO KEY
105700940714     C     V2CFLG        IFEQ      '3'
105800911023     C                   GOTO      FINE
105900911023     C                   ELSE
106000911023     C* FLG >< 2 ==> TORNO A SUBFILE
106100911023     C                   GOTO      FORCT2
106200040217     C                   ENDIF
106300911023     C*
106400000000     C     FINE          TAG
106500930504     C*
106600930702     C* CALL A VUOTO AI PGM  PER CHIUDERE I FILES
106700941214     C     PA098         IFEQ      '1'
106800930504     C                   MOVEL     'C'           D98TLA
106900970925     C                   CALL      'TNSD99R'
107000991027     C                   PARM                    TNSD99DS
107100040217     C                   ENDIF
107200930702     C*
107300941214     C     PA075         IFEQ      '1'
107400940714     C                   MOVEL     'C'           PA1FLG
107500940712     C                   MOVEL     PARAM1        KPJBU
107600940714     C                   CALL      'FNLR75R'                            99
107700930524     C                   PARM                    KPJBA
107800040217     C                   ENDIF
107900930702     C*
108000941214     C     PA0B5         IFEQ      '1'
108100950215     C                   CLEAR                   PARAB5
108200940726     C                   MOVEL     'C'           PB5FLG
108300940726     C                   MOVEL     PARAB5        KPJBU
108400940722     C                   CALL      'FNLRB5R'
108500930525     C                   PARM                    KPJBA
108600040217     C                   ENDIF
108700941014     C*
108800941014     C     *IN97         IFEQ      *ON
108900941014     C                   MOVEL     'C'           D66TLA
109000991027     C                   MOVEL     FNLR66DS      KPJBU
109100941014     C                   CALL      'FNLR66R'
109200941014     C                   PARM                    KPJBA
109300040217     C                   ENDIF
109400991027      *
109500991027     C                   IF        Wtibs69r  <>  *BLANKS
109600991027     C                   eval      I69TLA  = 'C'
109700991027     C                   call      'TIBS69R'
109800991027     C                   parm                    TIBS69DS
109900991027     c                   ENDIF
110000930504     C*
110100941109     C* PER F3=FINE PULISCO CHIAVE BOLLA E IMPOSTO FLAG F3
110200941109     C     *INKC         IFEQ      *ON
110300941109     C                   Z-ADD     *ZERO         V2CAAS
110400941109     C                   Z-ADD     *ZERO         COMLNP
110500941109     C                   Z-ADD     *ZERO         V2CNRS
110600941109     C                   Z-ADD     *ZERO         V2CNSP
110700941109     C                   MOVEL     '1'           PA1F03
110800941109     C                   ENDIF
110900980609     C**
111000941212     C* CHIUDO I PGM
111100941212     C* SOLO PER PGM DI MANUTENZIONE
111200941212     C     *IN97         IFEQ      *ON
111300941212     C                   MOVEL     'C'           D66TLA
111400941212     C                   CALL      'FNLR66R'
111500991027     C                   PARM                    FNLR66DS
111600060224     c
111700060224     c                   clear                   fnlv55ds
111800060224     C                   MOVEL     'C'           D55TLA
111900060227     C                   CALL      'FNLV55R'
112000060224     C                   PARM                    FNLv55DS
112100941212     C*
112200941212     C                   MOVEL     'C'           D48TLA
112300991027     C                   MOVEL     FNLR48DS      KPJBU
112400941212     C                   CALL      'FNLR48R'
112500941212     C                   PARM                    KPJBA
112600941212     C*
112601141105     c                   eval      ilry09tla= 'C'
112602141105     c                   eval      kpjbu=fnlry09ds2
112603141105     c                   call      'FNLRY09R'
112604141105     c                   parm                    kpjba
112700941212     C                   ENDIF
112800980603     C**
112900980609     C                   MOVEL     PARAAZ        KPJBU
113000980624     C                   MOVEL     PARAM1        KPJBU
113100941212     C*
113200040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
113300040810     c                   clear                   fidn12ds
113400040810     c                   eval      i12tla = 'C'
113500040810     c                   call      'FIDN12R'
113600040810     c                   parm                    fidn12ds
113700051128     c                   clear                   fnlv80ds
113800051128     c                   eval      ilv80tla = 'C'
113900051128     c                   call      'FNLV80R'
114000051128     c                   parm                    kpjba
114100051128     c                   parm                    FNLV80DS
114200070427     c                   eval      id34tla = 'C'
114300070427     c                   call      'FIDG34R'
114400070427     c                   parm                    fidg34ds
114500131120     c                   eval      ilrg7tla = 'C'
114600131120     c                   call      'FNLRG7R'
114700131120     c                   parm                    fnlrg7ds
114800091216     c                   eval      iinttla = 'C'
114900091216     c                   call      'TRULINTR'
115000091216     c                   parm                    trulintds
115001141105
115100040810     c*
115200000000     C                   SETON                                        LR
115300941011     C   99
115400941011     CANN99              READ      FNBLP01L                               99
115500940802     C**************************************************************************
115600940802     C*    CARICO TABELLE
115700940802     C**************************************************************************
115800911024     C     CARTAB        BEGSR
115900941212     C***
116000941212     C* CARICO TABELLA FILIALI GESTITE £1
116100941212     C***
116200950122     C                   CLEAR                   L1
116300991027     C                   CLEAR                   TRUL06DS
116400941212     C                   MOVE      '£1'          D06COD
116500941212     C                   MOVEL     SIMFEL        D06KEY
116600941212     C                   MOVEL     'L'           D06TLA
116700991027     C                   MOVEL     TRUL06DS      KPJBU
116800941212     C*
116900941212     C                   CALL      'TRUL06R'
117000941212     C                   PARM                    KPJBA
117100991027     C                   MOVEL     KPJBU         TRUL06DS
117200941212     C                   MOVEA     LIN           L1
117300930705     C*
117400930705     C* CARICO TABELLA 7O
117500930705     C                   MOVEL     '7O'          COD
117600941212     C                   Z-ADD     1             C                 3 0
117700940713     C     KTAB2         SETLL     TABEL
117800940713     C     KTAB2         READE     TABEL                                  30
117900930705     C*
118000930705     C     *IN30         DOWEQ     *OFF
118100930705     C     TBLFLG        IFEQ      ' '
118200930705     C                   MOVEL     TBLKEY        KCA(C)
118300930705     C                   MOVEL     TBLUNI        DCA(C)
118400930705     C                   ADD       1             C
118500930705     C                   END
118600940713     C     KTAB2         READE     TABEL                                  30
118700930705     C                   END
118800941024     C*
118900941024     C* CARICO IN SCHIERA LA TABELLA CC
119000941024     C                   Z-ADD     *ZERO         WA
119100941024     C                   CLEAR                   CC1
119200941024     C                   CLEAR                   CC2
119300020329     C                   CLEAR                   CC3
119400941024     C                   MOVEL     'CC'          COD
119500941024     C     KTAB2         SETLL     TABEL00F
119600941024     C     KTAB2         READE     TABEL00F                               99
119700941024     C     *IN99         DOWEQ     *OFF
119800941024     C     TBLKEY        ANDNE     *BLANKS
119900941024     C     TBLKEY        ANDNE     *ZERO
120000941024     C                   ADD       1             WA
120100941024     C                   MOVE      TBLKEY        CC1(WA)
120200941024     C                   MOVEL     TBLUNI        CC2(WA)
120300020402     C                   MOVEL(P)  TBLUNI        DSCC
120400020402     C                   MOVE      §CCDFE        CC3(WA)
120500941024     C     KTAB2         READE     TABEL00F                               99
120600941024     C                   ENDDO
120700060130     C*
120800060130     C* CARICO IN SCHIERA LA TABELLA 3a per cod bolla doppi
120900060130     C                   Z-ADD     *ZERO         WA
121000060130     C                   CLEAR                   cbodop
121100060130     C                   MOVEL     '3A'          COD
121200060130     C     KTAB2         SETLL     TABEL00F
121300060130     C     KTAB2         READE     TABEL00F                               99
121400060130     C     *IN99         DOWEQ     *OFF
121500060130     c                   movel     tbluni        ds3a
121600060130     c                   if        §3atb2<>*blanks
121700060130     C                   ADD       1             WA
121800060130     C                   MOVEL     TBLKEY        cbodop(wa)
121900060130     c                   endif
122000060130     c
122100060130     C     KTAB2         READE     TABEL00F                               99
122200060130     C                   ENDDO
122300020329      *
122400020329      * CARICO IN SCHIERA LA TABELLA CB
122500020329     C                   Z-ADD     *ZERO         WA
122600020329     C                   CLEAR                   CB1
122700020329     C                   CLEAR                   CB2
122800020329     C                   MOVEL     'CB'          COD
122900020329     C     KTAB2         SETLL     TABEL00F
123000020329     C     KTAB2         READE     TABEL00F                               99
123100020329     C     *IN99         DOWEQ     *OFF
123200020329     C     TBLKEY        ANDNE     *BLANKS
123300020329     C     TBLKEY        ANDNE     *ZERO
123400020329     C                   ADD       1             WA
123500020329     C                   MOVE      TBLKEY        CB1(WA)
123600020329     C                   MOVEL     TBLUNI        CB2(WA)
123700020329     C     KTAB2         READE     TABEL00F                               99
123800020329     C                   ENDDO
123900991027     C*
124000991027     C* CARICO LA TABELLA $2
124100991027     C                   MOVEL     '$2'          COD
124200991027     C                   MOVEL(P)  1             KEY
124300991027     C     KTAB1         CHAIN     TABEL00F
124400991027     C                   IF        %FOUND
124500991027     C                   MOVEL     TBLUNI        DS$2
124600991027     C                   ELSE
124700991027     C                   CLEAR                   DS$2
124800991027     C                   ENDIF
124900940802     C*
125000911024     C                   ENDSR
125100940802     C**************************************************************************
125200940802     C*    PULIZIA CAMPI FORMATO 1
125300940802     C**************************************************************************
125400900514     C     PUL01         BEGSR
125500131114     c
125600131114     C* PULIZIA CAMPI CHE RIMANGONO INVARIATI IL SECONDO GIRO
125700131114     C                   MOVEL     ' '           V1CSSP
125800131127     c                   if        pa1flg='2' or pa1flg='4' or pa1flg='M'
125900131127     c                   else
126000131114     C                   MOVEL     *ZEROS        V1CLNA
126100131127     c                   endif
126200131115     c                   clear                   v1cdea
126300131115     c                   clear                   v1cdep
126400131114     C                   MOVEL     *ZEROS        V1CLP1
126500131114     C                   MOVEL     *ZEROS        V1CDTD
126600131114     C                   MOVEL     *ZEROS        V1CDTA
126700131114     c                   clear                   v1cnzd
126800131114     c                   clear                   v1cprd
126900131128     c                   movel     ' '           v1cxind
127000131114     c                   clear                   v1cind
127100131114     c                   clear                   v1clod
127200131128     c                   movel     ' '           v1cxlod
127300131114     C                   MOVE      *BLANKS       V1CRSD
127400131128     C                   movel     ' '           V1CXRSD
127500131114     C                   MOVE      *BLANKS       V1CRSM
127600131128     C                   movel     ' '           V1CXRSM
127700131114     C                   MOVEL     *ZEROS        NDC
127800131114     C                   MOVEL     *ZEROS        NSS
127900940802     C*
128000060613     C                   CLEAR                   wforz1
128100060613     C                   CLEAR                   V1CRPE
128200010509     C                   CLEAR                   V1Cpoe
128300010509     C                   CLEAR                   V1Cnsr
128400010509     C                   CLEAR                   V1Cnor
128500010509     C                   CLEAR                   V1Cnrv
128600110930     C                   CLEAR                   V1Crmn
128700110930     C                   CLEAR                   V1Crma
128800131115     C                   CLEAR                   V1Cdan
128900940713     C                   MOVEL     *ZEROS        V1CLP2
129000940713     C                   MOVEL     *ZEROS        V1CNRS
129100940713     C                   MOVEL     *ZEROS        V1CNSP
129200000103     C                   z-add     UYEAR         V1CAAS
129300000103     c     uyear         iflt      60
129400000103     c                   movel     20            v1caas
129500000103     c                   else
129600940713     C                   MOVEL     19            V1CAAS
129700000103     c                   endif
129800940713     C                   MOVEL     *BLANKS       V1CTSP
129900131114     C                   MOVEL     *BLANKS       V01DTSP
130000940713     C                   MOVEL     *BLANKS       V1CGIA
130100940713     C                   MOVEL     *ZEROS        V1CPSD
130200940713     C                   MOVEL     99            V1CPSA
130300940713     C                   MOVEL     *ZEROS        V1CSCA
130400940713     C                   MOVEL     *ZEROS        V1CSCD
130500940923     C                   MOVEL     *ZERO         V1CFLS
130600940713     C                   MOVE      'N'           V1CAFF
130700961112     C                   CLEAR                   V1CFBR
130800131111     C                   CLEAR                   V1Cbarcode
130900131111     C                   CLEAR                   V1CNOT
131000040216     c                   clear                   V1Ccbo
131100940713     C                   Z-ADD     0             NR2
131200070420     C****               MOVE      'N'           V1CSNP            1
131300070420     c                   clear                   v1csfd
131400941212     C                   SETOFF                                       09
131500131111     c
131600131111     c* se cursore aperto --> lo chiudo
131700131209     c***                if        Wend=*on  or *in14 or *in11
131800131209     c                   if        WopenC='1'
131900131111     c                   exsr      closecursor
132000131111     c                   endif
132001150223     c* se provengo da gestojne r.a. mi posiziono su brtcode
132002150223     c                   if            SDSprm >  1
132003150223     c                             and IA1ins = ' '
132004150223     c                   seton                                        60
132005150223     c                   endif
132100930705     C*
132200900514     C                   ENDSR
132300940802     C**************************************************************************
132400940802     C*    CONTROLLI FORMATO1
132500940802     C**************************************************************************
132600900509     C     CONTR         BEGSR
132700940317      *
132800940713     C                   MOVE      *BLANKS       V1CDEA
132900940713     C                   MOVE      *BLANKS       V1CDEP
133000110930     c                   movel     v1crma        comrma
133100020709
133200131121     c******             clear                   v2dlna
133300940317      *
133400900515     C                   SETOFF                                       900102
133500970925     C                   SETOFF                                       140784
133600980603     C                   SETOFF                                       03  11
133700040216     C                   SETOFF                                       154046
133800940715     C                   SETOFF                                       414243
133900940715     C                   SETOFF                                       444548
134000940715     C                   SETOFF                                       495152
134100131115     C                   SETOFF                                       552547
134200131111     C                   SETOFF                                       535460
134300131111     c                   setoff                                       575058
134400131127     c                   setoff                                       6862
134500110930     c                   clear                   h1riga
134600110930     c                   clear                   h1colo
134700950121     C*
134800950121     C* SE L'ANNO E' IMMESSO DI DUE CIFRE, METTO LE ALTRE DUE
134900950121     C     V1CAAS        IFLT      100
135000950121     C     V1CAAS        IFGT      60
135100950121     C                   ADD       1900          V1CAAS
135200950121     C                   ELSE
135300950121     C                   ADD       2000          V1CAAS
135400950121     C                   ENDIF
135500950121     C                   ENDIF
135600950121     C*
135700950121     C* SE VARIAZIONE CONTROLLO CAL CAUSALE
135800950121     C   97              DO
135900991027     C                   CLEAR                   FNLR66DS
136000950121     C     '?'           SCAN      V1CCVB                                 90
136100950121     C* RICERCA
136200950121    1C     *IN90         IFEQ      *ON
136300991027     C                   CLEAR                   FNLR66DS
136400950121     C                   MOVEL     'V'           D66GES
136500950121     C                   MOVEL     'A'           D66TBO
136600950121     C*
136700950121     C* SE IMMESSO NUMERO SPEDIZIONE LO VISUALIZZO
136800950121    2C     V1CNSP        IFGT      0
136900950121     C                   MOVEL     V1CNSP        D66NSP
137000950121     C                   MOVEL     V1CAAS        D66AAS
137100950121     C                   MOVEL     V1CLP2        D66LNP
137200950121     C                   MOVEL     V1CNRS        D66NRS
137300950121    2C                   ENDIF
137400950121     C*
137500991027     C                   MOVEL     FNLR66DS      KPJBU
137600950121     C                   CALL      'FNLR66R'
137700950121     C                   PARM                    KPJBA
137800991027     C                   MOVEL     KPJBU         FNLR66DS
137900950121     C                   MOVEL     D66CVB        V1CCVB
138000950121     C                   MOVEL     D66DES        V1DCVB
138100950121     C                   MOVEL     'S'           WRIEM
138200950203     C                   SETON                                        4790
138300950121     C*
138400950121     C                   GOTO      ENDCTR
138500950121   X1C                   ELSE
138600950121     C* CONTROLLO
138700991027     C                   CLEAR                   FNLR66DS
138800950121     C                   MOVEL     'C'           D66GES
138900950121     C                   MOVEL     'A'           D66TBO
139000950121     C                   MOVEL     V1CCVB        D66CVB
139100950121    2C     V1CNSP        IFGT      0
139200950121     C                   MOVEL     V1CNSP        D66NSP
139300950121     C                   MOVEL     V1CAAS        D66AAS
139400950121     C                   MOVEL     V1CLP2        D66LNP
139500950121     C                   MOVEL     V1CNRS        D66NRS
139600950121    2C                   ENDIF
139700950121     C*
139800991027     C                   MOVEL     FNLR66DS      KPJBU
139900950121     C                   CALL      'FNLR66R'
140000950121     C                   PARM                    KPJBA
140100991027     C                   MOVEL     KPJBU         FNLR66DS
140200950121     C                   MOVEL     D66CVB        V1CCVB
140300950121     C                   MOVEL     D66DES        V1DCVB
140400950121     C*
140500950121     C* ERRORE
140600950121    2C     D66MSG        IFNE      *BLANKS
140700950121     C                   MOVEL     D66MSG        V1CMSG
140800950121     C                   SETON                                        284790
140900950121     C                   GOTO      ENDCTR
141000950121     C*
141100950121   X2C                   ELSE
141200950121     C* DECODIFICA
141300950121     C                   MOVEL     D66DES        V1DCVB
141400950121    2C                   ENDIF
141500950121    1C                   ENDIF
141600950121     C                   ENDDO
141700950102     C*
141800131113     C* immettere almeno una selezione
141900901115     C     SEL1          IFEQ      0
142000920911     C     SEL2          ANDEQ     0
142100940317     C     SEL3          ANDEQ     *BLANKS
142200010509     C     SEL4          ANDEQ     *zeros
142300131113     c
142400971223     C     V1CRPE        ANDEQ     *BLANKS
142500110930     c     v1crmn        andeq     *zeros
142600110930     c     v1crma        andeq     *blanks
142700131111     c     v1cnot        andeq     *blanks
142800131111     c     v1cbarcode    andeq     *zeros
142900940713     C                   MOVEL     MSG(3)        V1CMSG
143000940713     C                   SETON                                        412890
143100901115     C                   GOTO      ENDCTR
143200901115     C                   END
143300940318      *
143400901115     C* DATA SPEDIZIONE
143500900514     C* LINEA ARRIVO
143600131113      * per destinatario o mittente sono obbligatori
143700020627      * la nazione e la provincia del destinatario
143800131113    1C     SEL2          IFEQ      0
143900131113    2c     sel4          ifeq      0
144000020225     c                   clear                   lnantw
144100131113    3C     V1CLNA        IFNE      0
144200940713     C                   MOVEL     V1CLNA        NUM3
144300940713     C     NUM3          CHAIN     AZORG01L                           41
144400131113    4C   41              DO
144500940713     C                   MOVEL     MSG(2)        V1CMSG
144600940713     C                   SETON                                        28  90
144700940713     C                   GOTO      ENDCTR
144800131113    4C                   ENDDO
144900940713     C                   MOVEL     ORGDES        V1CDEA
145000131121     C*******            MOVEL     ORGDES        V2DLNA
145100991222     C                   MOVEL     ORGDE3        og143
145200020225     c                   eval      lnantw = §ogntw
145300900514     C                   SETON                                        01
145400131113    3C                   END
145500131113    2C                   END
145600131113    c
145700020626      * selezione per destinatario o mittente
145800020626     c                   if        sel3 <> *blanks
145900020628     c                   exsr      ContrDatiDes
146000020628     c                   if        *in90 = *on                                  ricerca
146100020628     c                   goto      endctr
146200020628     c                   endif
146300020628     c                   if        *in28 = *on                                  errore
146400020628     c                   eval      *in90 = *on
146500020628     c                   goto      endctr
146600020628     c                   endif
146700020626     c                   endif
146800901115     C* LINEA PARTENZA
146900131111     c                   clear                   lnpntw
147000940713     C     V1CLP1        IFNE      0
147100940713     C                   MOVEL     V1CLP1        NUM3
147200940713     C     NUM3          CHAIN     AZORG01L                           42
147300940713     C   42              DO
147400940713     C                   MOVEL     MSG(5)        V1CMSG
147500940713     C                   SETON                                        28  90
147600940713     C                   GOTO      ENDCTR
147700940713     C                   ENDDO
147800940713     C                   MOVEL     ORGDES        V1CDEP
147900131121     C****               MOVEL     ORGDES        V2DLNP
148000000703     C                   movel     orgde3        og143
148100050304     c                   eval      lnpntw = §ogntw
148200901115     C                   SETON                                        02
148300901115     C                   END
148400940413      *
148500971223      * E' OBBLIGATORIA LNA O LNP
148600971223     C     V1CLNA        IFEQ      *ZERO
148700940713     C     V1CLP1        ANDEQ     *ZERO
148800971223     C     V1CRPE        ANDEQ     *BLANKS
148900010509     c     sel4          andeq     0
149000020628     c     sel3          andeq     *blanks
149100110930     c     v1crmn        andeq     *zeros
149200110930     c     v1crma        andeq     *blanks
149300131113     c     v1cnot        andeq     *blanks
149400131114     c     v1cbarcode    andeq     *zeros
149500940713     C                   MOVEL     MSG(4)        V1CMSG
149600940713     C                   SETON                                        412890
149700940712     C                   GOTO      ENDCTR
149800940712     C                   ENDIF
149900940413     C*
150000911023     C* DATA DAL OBBLIGATORIA
150100940713     C     V1CDTD        IFEQ      0
150200971223     C     V1CRPE        ANDEQ     *BLANKS
150300010509     C     sel4          ANDEQ     0
150400110930     C     v1crmn        ANDEQ     0
150500110930     C     v1crma        ANDEQ     *blanks
150600131111     C     v1cnot        ANDEQ     *blanks
150700131114     c     v1cbarcode    andeq     *zeros
150800131113     c
150900131113     c*********          if        v1cssp <> *blanks or
151000131113     c
151100131113     c                   if        (v1crsd = *blanks and v1crsm = *blanks and
151200131111     c                              v1cind = *blanks and v1clod = *blanks and
151300131111     c                             sel3 <> *blanks and *in01) or
151400131111     c                               sel3 = *blanks
151500940713     C                   MOVEL     MSG(8)        V1CMSG
151600940713     C                   SETON                                        285190
151700911023     C                   GOTO      ENDCTR
151800940712     C                   ENDIF
151900911023     C                   END
152000911023     C*
152100940712     C                   Z-ADD     *ZERO         COMDTD
152200940712     C                   Z-ADD     *ZERO         COMAAD
152300940712     C                   Z-ADD     *ZERO         COMMGD
152400940713     C     V1CDTD        IFNE      *ZERO
152500940713     C                   MOVE      V1CDTD        G02DAT
152600930705     C                   MOVEL     *BLANK        G02ERR
152700940713     C                   CALL      'XSRDA8'
152800930705     C                   PARM                    WLBDAT
152900930705     C     G02ERR        IFEQ      '1'
153000940713     C                   MOVEL     MSG(8)        V1CMSG
153100940713     C                   SETON                                        285190
153200930705     C                   GOTO      ENDCTR
153300930705     C                   ELSE
153400940713     C                   Z-ADD     G02DAT        V1CDTD
153500940713     C                   MOVE      G02INV        COMDTD            8 0
153600940713     C                   MOVEL     G02INV        COMAAD            4 0
153700930705     C                   MOVE      G02INV        COMMGD            4 0
153800930705     C                   END
153900940712     C                   ENDIF
154000900515     C* DATA SPEDIZIONE AL
154100940712     C                   Z-ADD     *ZERO         COMDTA
154200940713     C     V1CDTA        IFNE      0
154300940713     C                   MOVE      V1CDTA        G02DAT
154400930705     C                   MOVEL     *BLANK        G02ERR
154500940713     C                   CALL      'XSRDA8'
154600930705     C                   PARM                    WLBDAT
154700930705     C     G02ERR        IFEQ      '1'
154800941215     C                   MOVEL     MSG(8)        V1CMSG
154900940713     C                   SETON                                        285290
155000930705     C                   GOTO      ENDCTR
155100930705     C                   ELSE
155200940713     C                   Z-ADD     G02DAT        V1CDTA
155300940713     C                   MOVE      G02INV        COMDTA            8 0
155400900515     C                   END
155500930705     C*
155600900515     C* DATA SPEDIZIONE AL MAGGIORE DATA SPEDIZIONE AL
155700911023     C     COMDTA        IFLT      COMDTD
155800940713     C                   MOVEL     MSG(10)       V1CMSG
155900940713     C                   SETON                                        285152
156000940713     C                   SETON                                        90
156100900515     C                   GOTO      ENDCTR
156200900515     C                   END
156300940318      *
156400940712     C                   ELSE
156500940712      *
156600940712      * SE NON IMMESSA LA DATA FINALE, LA PONGO UGUALE A QUELLA INIZIALE
156700940712     C                   Z-ADD     COMDTD        COMDTA
156800940318      *
156900900515     C                   END
157000940318      *
157100940712      * VERIFICO SE LE DUE DATE SONO COINCIDENTI
157200131115     c                   if        comdtd>0
157300940712     C     COMDTD        COMP      COMDTA                                 15
157400131115     c                   endif
157500901115     C***
157600911024     C* GIACENTI
157700911024     C* SCELTE INCONGRUENTI
157800940713     C     V1CGIA        IFEQ      'G'
157900940713     C     V1CSSP        IFEQ      '2'
158000940713     C     V1CSSP        OREQ      '3'
158100940713     C                   MOVEL     MSG(14)       V1CMSG
158200940713     C                   SETON                                        285690
158300911024     C                   GOTO      ENDCTR
158400911024     C                   END
158500911024     C                   END
158600901115     C* NUMERO SERIE AL DEVE ESSERE > NUMERO SERIE DAL
158700901115     C*  SE IMMESSO
158800940713     C     V1CPSD        IFGT      V1CPSA
158900940713     C                   MOVEL     MSG(11)       V1CMSG
159000940713     C                   SETON                                        284890
159100901115     C                   GOTO      ENDCTR
159200901115     C                   END
159300901115     C* NUMERO SEGNACOLLO AL DEVE ESSERE > NUMERO SEGNACOLLO DAL
159400940713     C     V1CSCA        IFGT      0
159500940713     C     V1CSCD        ANDGT     V1CSCA
159600940713     C                   MOVEL     MSG(12)       V1CMSG
159700940713     C                   SETON                                        284990
159800901115     C                   GOTO      ENDCTR
159900901115     C                   END
160000901115     C* SE IMMESSO AL IMMETTERE ANCHE DAL
160100940713     C     V1CSCA        IFGT      0
160200940713     C     V1CSCD        ANDEQ     0
160300940713     C                   MOVEL     MSG(13)       V1CMSG
160400940713     C                   SETON                                        284990
160500901115     C                   GOTO      ENDCTR
160600901115     C                   END
160700940713     C     V1CSCD        COMP      0                                  07
160800970925     C     V1CSCA        COMP      0                                  84
160900940923     C*
161000940923     C* LA FILIALE SEGNACOLLO DEVE ESSERE VALIDA, SE IMMESSSA
161100940923     C     V1CFLS        IFNE      *ZERO
161200940923     C     V1CFLS        CHAIN     AZORG01L                           46
161300941014     C     *IN46         IFEQ      *ON
161400941014     C     *IN46         OREQ      *OFF
161500941014     C     ORGFVA        ANDNE     *BLANKS
161600941014     C                   SETON                                        284690
161700941014     C                   MOVEL     MSG(17)       V1CMSG
161800941014     C                   GOTO      ENDCTR
161900940923     C                   ENDIF
162000941014     C                   ENDIF
162100940923     C*
162200901115     C* SE IMMESSA RAGIONE SOCIALE CERCO PRIMO BLANKS
162300940713     C     V1CRSM        IFNE      *BLANKS
162400901115     C                   SETON                                        11
162500940713     C                   MOVEA     V1CRSM        C1
162600131111     C                   Z-ADD     30            XX                2 0
162700930702     C     C1(XX)        DOWEQ     *BLANKS
162800930702     C                   SUB       1             XX
162900901116     C                   END
163000901116     C                   END
163100901115     C* IMMESSO DESTINATARIO: CERCO PRIMO BLANKS
163200940713     C     V1CRSD        IFNE      *BLANKS
163300901115     C                   SETON                                        14
163400940713     C                   MOVEA     V1CRSD        C1
163500131111     C                   Z-ADD     30            ZZ                2 0
163600930702     C     C1(ZZ)        DOWEQ     *BLANKS
163700930702     C                   SUB       1             ZZ
163800901116     C                   END
163900901115     C                   END
164000131113     C* località DESTINATARIO: CERCO PRIMO BLANKS
164100131113     C     V1Clod        IFNE      *BLANKS
164200131113     C                   SETON                                        14
164300131113     C                   MOVEA     V1Clod        C1
164400131113     C                   Z-ADD     30            LL                2 0
164500131113     C     C1(LL)        DOWEQ     *BLANKS
164600131113     C                   SUB       1             LL
164700131113     C                   END
164800131113     C                   END
164900131113     C* Indirizzo DESTINATARIO: CERCO PRIMO BLANKS
165000131113     C     V1Cind        IFNE      *BLANKS
165100131113     C                   SETON                                        14
165200131113     C                   MOVEA     V1Cind        C1
165300131113     C                   Z-ADD     30            DD                2 0
165400131113     C     C1(DD)        DOWEQ     *BLANKS
165500131113     C                   SUB       1             DD
165600131113     C                   END
165700131113     C                   END
165800131113     c
165900131113     c* 14 on anche per indirizzo o località  DESTINAT
166000131113     c                   if        not *in11 and
166100131113     c                              v1clod<>*blanks or v1cind<>*blanks
166200131113     c                   seton                                        14
166300131113     c                   endif
166400131113     c
166500131113      * Nazione provincia destinatario senza ragioni sociali
166600060613      * imposto per destinatario se non è stata impostata la lna  e la linea
166700060613      * partenza
166800060613     c                   if        v1cprd <> *blanks and
166900020628     c                             not *in11 and not *in14 and not *in01
167000060613     c                   eval      *in14 = *on
167100020628     c                   endif
167200060613     c                   if         v1cnzd <> *blanks and v1cprd = *blanks and
167300060613     c                             not *in11 and not *in14 and not *in01
167400060613     c                             and not *in02
167500060613     c                   eval      *in14 = *on
167600060613     c                   endif
167700901115     C                   END
167800901115     C*
167900901115     C** NUMERO SPEDIZIONE
168000901115     C** LINEA PARTENZA
168100140117     C***  SEL2          IFNE      0
168200140117     c                   if        sel2<> 0 and v1cbarcode<=0
168300940713     C                   Z-ADD     V1CLP2        NUM3
168400940713     C     NUM3          CHAIN     AZORG01L                           45
168500940713     C   45              DO
168600940713     C                   MOVEL     MSG(5)        V1CMSG
168700940713     C                   SETON                                        28  90
168800940713     C                   GOTO      ENDCTR
168900940713     C                   ENDDO
169000911023     C* ANNO SPEDIZIONE
169100940713     C     V1CAAS        IFEQ      0
169200940713     C                   MOVEL     MSG(7)        V1CMSG
169300940713     C                   SETON                                        285590
169400911023     C                   GOTO      ENDCTR
169500911023     C                   END
169600940713     C     KLNPX         CHAIN     FNARB01L                           44
169700030527     C*
169800940713     C   44              DO
169900940713     C                   MOVEL     MSG(6)        V1CMSG
170000940713     C                   SETON                                        28  90
170100940713     C                   GOTO      ENDCTR
170200940713     C                   ENDDO
170300901115     C*
170400901115     C                   END
170500131111     c***
170600131113     C* B A R C O D E   L D V
170700131111     c***
170800131111     c                   if        v1cbarcode>0
170900131111     c                   z-add     b_aas         arbaas
171000131111     C     b_aas         IFLT      100
171100131111     C     b_aas         IFGT      60
171200131111     C                   ADD       1900          arbAAS
171300131111     C                   ELSE
171400131111     C                   ADD       2000          arbAAS
171500131111     C                   ENDIF
171600131111     C                   ENDIF
171700131111     c                   z-add     b_lnp         arblnp
171800131111     c                   z-add     b_nrs         arbnrs
171900131111     c                   z-add     b_nsp         arbnsp
172000131114     C     Karb2         CHAIN     FNARB01L
172100131111     C*
172200131111     C                   if        not%found(fnarb01l)
172300131111     C                   MOVEL     MSG(32)       V1CMSG
172400131111     C                   SETON                                        286090
172500131111     C                   GOTO      ENDCTR
172600131111     C                   endif
172700131111     C                   endif
172800010508      ***
172900010508      * N U M E R O   O R M
173000010508      ***
173100010508     C     sel4          ifne      0
173200010508     C     sel2          andeq     0
173300010508      * P.O. EMISSIONE
173400010508     C                   z-add     v1cpoe        num3
173500010508     C     num3          chain     azorg01l                           53
173600010508     C     *in53         ifeq      *on
173700010508     C                   movel     msg(21)       v1cmsg
173800010508     C                   seton                                        28  90
173900010508     C                   goto      endctr
174000010508     C                   endif
174100010508      * N.ORM
174200010510     C                   MOVEL     'M'           WTRC
174300010510     C                   MOVEL(P)  SEL4          WN14             14
174400060216     C     kar44         chain     fiar404l                           54
174500010508     C     *in54         ifeq      *on
174600010508     C                   movel     msg(22)       v1cmsg
174700010508     C                   seton                                        28  90
174800010508     C                   goto      endctr
174900010508     C                   endif
175000010508      *
175100010508     C                   seton                                        25
175200010508      *
175300010508     C                   endif
175400131111     c
175500901115     C*
175600911023     C* TIPO SPEDIZIONE ESPRESSO
175700131114     C                   MOVEL     *BLANKS       V01DTSP
175800940713     C     V1CTSP        IFNE      ' '
175900940715     C     V1CTSP        IFEQ      '?'
176000940715     C                   CALL      'X§TABER'
176100040217     C                   PARM      CodUt         §KUT              1 0
176200040217     C                   PARM      '5E'          COD
176300040217     C                   PARM      *blanks       KEY
176400040217     C                   PARM      *blanks       §DES             30
176500940715     C                   MOVEL     KEY           V1CTSP
176600940715     C                   SETON                                        90
176700940715     C                   MOVEL     'S'           WRIEM
176800940715     C                   ENDIF
176900940715     C*
177000090604     c                   clear                   ds5e
177100940715     C                   MOVEL     '5E'          COD
177200940715     C                   MOVEL     *BLANKS       KEY
177300940715     C                   MOVEL     V1CTSP        KEY
177400940715     C     KTAB1         CHAIN     TABEL00F                           43
177500941014     C     *IN43         IFEQ      *ON
177600941014     C     *IN43         OREQ      *OFF
177700950221     C     TBLFLG        ANDNE     ' '
177800940715     C                   MOVEL     MSG(16)       V1CMSG
177900941014     C                   SETON                                        284390
178000940715     C                   GOTO      ENDCTR
178100941014     C                   ELSE
178200090604     C                   MOVEL     TBLUNI        ds5e
178300131114     C                   MOVEL     §5ed08        V01DTSP
178400991222     C                   ENDIF
178500940715     C                   ENDIF
178600040216      *
178700991222     c*
178800991222     c* Se richiesta linea di arrivo dpd il riferimemnto immesso deve
178900060613     c*  essere composto soltanto da numeri se <=11
179000060613     C*  alfanumerico       se=14
179100050121     c                   clear                   wnodpd            1
179200060613    1c                   if        lnantw = 'DPD' or lnpntw = 'DPD' or
179300050304     c                             lnpntw = *blanks or lnantw = *blanks
179400060613    2c                   if        v1crpe<>*blanks
179500060613    3c                   select
179600060613     c* ok se lungo 14
179700060613     c                   when      %subst(v1crpe:15:1)=' '  and
179800060613     c                             %subst(v1crpe:14:1)<>' '
179900060613     c* ok se lungo 11 ma tutto numerico
180000060613     c                   when      %subst(v1crpe:12:4)=*blanks and
180100060613     c                             %subst(v1crpe:11:1)<>' '
180200060613     C                   MOVEA     V1CRPe        K11
180300060613     C                   Z-ADD     1             YY                2 0
180400060613     c
180500060613    4C                   DO        11            YY
180600060613     C     K11(YY)       LOOKUP    CNM                                    30
180700060613    5C     *IN30         IFEQ      *OFF
180800060613     c* Se non è tutto numerico non è DPD
180900060613     c                   eval      wnodpd = '1'
181000060613     c                   leave
181100060613    5c                   endif
181200060613    4c                   enddo
181300060613   x3c                   other
181400060613     c
181500060613     c* se non è lungo 11 o 14 non è DPD
181600060613     c                   eval      wnodpd = '1'
181700060613    3c                   endsl
181800060613     c
181900060613    3c                   if        wnodpd='1'
182000060613    4c     lnantw        ifeq      'DPD'
182100060613     c     lnpntw        oreq      'DPD'
182200060613     C                   MOVEL     MSG(18)       V1CMSG
182300060613     C                   SETON                                        285890
182400060613     C                   GOTO      ENDCTR
182500060613    4C                   ENDIF
182600060613    3C                   ENDIF
182700060613    2c                   endif
182800061117   x1c                   else
182900061117     c                   eval      wnodpd = '1'
183000060613    1c                   endif
183100941014     C*
183200911023     C** FILE STORICO
183300940713     C     V1CSSP        COMP      '1'                                    18
183400940713     C     V1CSSP        COMP      '2'                                    19
183500940713     C     V1CSSP        COMP      '3'                                    20
183600940713     C     V1CSSP        COMP      ' '                                    21
183700920911     C*
183800940712      * DEVO CONSIDERARE DELLE PRIORITA' PER LE SELEZIONI IMMESSE DALL'UTENTE:
183900010508      *    1°    NUMERO O.R.M.           ( 25 ON)
184000010508      *    2°    MITTENTE                ( 11 ON )
184100010508      *    3°    DESTINATARIO            ( 14 ON )
184200010508      *    4°    LINEA DI PARTENZA       ( 02 ON )
184300010508      *    5°    LINEA DI ARRIVO         ( 01 ON )
184400940712      *
184500010508     C   25              setoff                                       111402
184600010508     C   25              setoff                                       01
184700131113     C   14              SETOFF                                       020111
184800131113     C   11              SETOFF                                       0201
184900940712     C   02              SETOFF                                       01
185000991222     c*se lna dpd e immesso riferimento partner estero, se meno di 12
185100060518     c*  caratteri, calcolo il 12 --> ora non più: il check digit per la
185200060518     c*  ricerca non serve più
185300991222     C                   CLEAR                   krpe
185400050121    1c                   if        v1crpe <> *blanks and (lnantw='DPD'
185500050304     c                             or lnpntw = 'DPD' or wnodpd=' ')
185600060613     C                   movel     V1CRpe        krpe             14
185700991222     C                   ENDIF
185800911023     C*
185900900514     C     ENDCTR        ENDSR
186000020628      *------------------------------------------------------------------------*
186100020628      *    Controlla i dati nazione e provincia destinatario
186200020628      *------------------------------------------------------------------------*
186300020628     c     ContrDatiDes  begsr
186400020628     c                   if        v1cnzd = *blanks and v1cprd = *blanks
186500020628     c                   eval      v1cmsg = msg(23)
186600020628     c                   eval      *in57 = *on
186700020628     c                   eval      *in28 = *on
186800020628     c                   goto      EndContr
186900020628     c                   endif
187000020628      * ricerca tabella nazione
187100020628     c     '?'           scan      v1cnzd                                 90
187200020628     c                   movel     '15'          cod
187300020628     c                   if        *in90
187400020628     c                   call      'TRUL19R'
187500020628     c                   parm                    cod
187600020628     c                   parm                    ordina
187700020628     c                   parm                    key
187800020628     c                   parm                    comando
187900020628     c                   movel     key           v1cnzd
188000020628     c                   eval      *in57 = *on
188100020628     c                   movel     'S'           wriem
188200020628     c                   goto      EndContr
188300020628     c                   endif
188400020628      * controllo nazione destinatario
188500020628     c                   clear                   ds15
188600020628     c                   movel     '15'          cod
188700020628     c                   clear                   key
188800020628     c                   movel     v1cnzd        key
188900020628     c     ktab1         chain     tabel00f
189000020628     c                   if        not %found(tabel00f) or tblflg <> *blanks
189100020628     c                   eval      v1cmsg = msg(24)
189200020628     c                   eval      *in57 = *on
189300020628     c                   eval      *in28 = *on
189400020628     c                   goto      EndContr
189500020628     c                   endif
189600020628     c                   movel     tbluni        ds15
189700060613     c* Se cambiata nazione, clearo forzatura msg se nazine estera
189800060613     c                   if        v1cnzd<>savnzd
189900060613     c                   clear                   wforz1            1
190000060613     c                   eval      savnzd=v1cnzd
190100060613     c                   endif
190200020628      * per nazione italia la provincia è obbligatoria
190300020628     c                   if        §15ita = 'I' and v1cprd = *blanks
190400020628     c                   eval      v1cmsg = msg(25)
190500020628     c                   eval      *in50 = *on
190600020628     c                   eval      *in28 = *on
190700020628     c                   goto      EndContr
190800020628     c                   endif
190900020628      * ricerca tabella provincia
191000020628     c     '?'           scan      v1cprd                                 90
191100020628     c                   movel     'PR'          cod
191200020628     c                   if        *in90
191300020628     c                   call      'TRUL19R'
191400020628     c                   parm                    cod
191500020628     c                   parm                    ordina
191600020628     c                   parm                    key
191700020628     c                   parm                    comando
191800020628     c                   movel     key           v1cprd
191900020628     c                   eval      *in50 = *on
192000020628     c                   movel     'S'           wriem
192100020628     c                   goto      EndContr
192200020628     c                   endif
192300020628      * controllo provincia destinatario
192400020628     c                   if        v1cprd <> *blanks and §15ita = 'I'
192500020628     c                   clear                   dspr
192600020628     c                   movel     'PR'          cod
192700020628     c                   clear                   key
192800020628     c                   movel     v1cprd        key
192900020628     c     ktab1         chain     tabel00f
193000020628     c                   if        not %found(tabel00f) or tblflg <> *blanks
193100020628     c                   eval      v1cmsg = msg(25)
193200020628     c                   eval      *in50 = *on
193300020628     c                   eval      *in28 = *on
193400020628     c                   goto      EndContr
193500020628     c                   endif
193600020628     c                   movel     tbluni        dspr
193700020628      * se nazione italia la provincia deve essere italia
193800020628      * nella tabella ci sono anche le provincie estere e per sapere se ok
193900020628      * faccio il controllo con il codice tassazione per estero
194000020628      * se vuoto vuol dire che è provincia estera
194100020628     c                   if        §15ita = 'I' and §prcte = *blanks
194200020628     c                   eval      v1cmsg = msg(25)
194300020628     c                   eval      *in50 = *on
194400020628     c                   eval      *in28 = *on
194500020628     c                   goto      EndContr
194600020628     c                   endif
194700020628     c                   endif
194800020628
194900020628     c     EndContr      endsr
195000940318      **************************************************************************
195100940318      *    ELABORA BOLLE DA VISUALIZZARE
195200940318      **************************************************************************
195300900515     C     ELAB          BEGSR
195400940318      *
195500131114     C                   SETON                                        35
195600131114     C                   WRITE     LRI6C02
195700131114     C                   SETOFF                                       35
195800131114     C*
195900940713     C                   Z-ADD     1             REC2
196000911024     C                   Z-ADD     1             C
196100131111     c                   eval      wEnd = *off
196200131115     c                   clear                   readFile          3
196300131113     c
196400131113     C                   Z-ADD     COMAAD        $AAS
196500131113     C                   Z-ADD     COMMGD        $MGS
196600131111     c
196700131111    1c                   select
196800010508     c* O.R.M.
196900131111    1c     *in25         wheneq    *on
197000010508     C                   MOVEL     'M'           WTRC
197100010508     C                   MOVEL(P)  SEL4          WN14             14
197200060216     C     Kar44         SETLL     FiAR404L
197300131111     c
197400131111     c***
197500131111     C*  RIFERIMENTO P A R T N E R   E S T E R O
197600131111     c***
197700131111    1C     V1CRPE        whenne    *BLANKS
197800050121     c                   clear                   wgiro             1
197900050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
198000971223     C                   MOVEL(P)  V1CRPE        WN30             30
198100991222     C                   MOVEL     'E'           WTRC
198200991222     c                   else
198300991222     c* D P D
198400991222     C                   MOVEL(P)  krpe          WN30             30
198500060518     C***                MOVEL     'F'           WTRC
198600060518     C                   MOVEL     'I'           WTRC
198700991222     c                   endif
198800991222     c**
198900060216     C     KAR4          SETLL     FiAR403L
199000991222     c**
199100131111     c* C H I   S O N O
199200131111     c**
199300131111    1C     V1CNOT        whenne    *BLANKS
199400131111     C     KarS          SETLL     FIarS02L
199500131111     c* R N N
199600131111    1c     v1crmn        whengt    *zeros
199700110930     c     v1crmn        setll     fnarb33l
199800131115     c                   eval      Readfile='DSP'
199900131111     c* R M A
200000131111    1c     comrma        whenne    *blanks
200100111005     c     comrma        setll     fnarb32l
200200131115     c                   eval      Readfile='DSP'
200300131111     c
200400131113      * RSD/RSM/IND/LOD o solo NAZ/PRV       -----------------------------------
200500110930     c
200600131113     c                   when      *in14  OR *IN11
200700131111     c                   exsr      Presql
200800131111     c                   exsr      Opencursor
200900131111   x1c                   other
201000131111     c
201100971223      *
201200940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1)  -----------------------------------
201300971223    2C   18              DO
201400940713     C     V1CLNA        IFNE      *ZERO
201500940713     C                   Z-ADD     V1CLNA        COMLNA
201600940712     C                   ELSE
201700940712     C                   Z-ADD     L1(1)         COMLNA
201800940712     C                   ENDIF
201900020910     C     K1ARB         SETLL     FNARB78L
202000131115     c                   eval      Readfile='DSP'
202100971223    2C                   ENDDO
202200940318      *
202300940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
202400971223    2C   19              DO
202500940713     C     V1CLNA        IFNE      *ZERO
202600120330     C     V1CLNA        SETLL     FNARB05L
202700940712     C                   ELSE
202800120330     C     *LOVAL        SETLL     FNARB05L
202900940712     C                   ENDIF
203000971223    2C                   ENDDO
203100940318      *
203200940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
203300971223    2C   20              DO
203400940713     C     V1CLNA        IFNE      *ZERO
203500940713     C                   Z-ADD     V1CLNA        COMLNA
203600940712     C                   ELSE
203700940712     C                   Z-ADD     L1(1)         COMLNA
203800940712     C                   ENDIF
203900940713     C     K4ARB         SETLL     FNARB54L
204000131115     c                   eval      Readfile='DCM'
204100971223    2C                   END
204200940318      *
204300940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
204400971223    2C   21              DO
204500131113     c
204600131113     c* per LINEA DI PARTENZA
204700131113    3C     *IN02         IFEQ      *ON
204800950406     C                   SELECT
204900950406     C* Serie = 0 e segnacollo dal diverso da 0 => i segnacolli sono
205000950406     C* sequenziali
205100950406     C     V1CPSD        WHENEQ    *ZEROS
205200950406     C     V1CSCD        ANDNE     *ZEROS
205300950406     C                   Z-ADD     V1CSCD        COMSCD
205400950406     C     K6ARB         SETLL     FNARB27L                               31
205500131115     c                   eval      Readfile='DSP'
205600950406     C     *IN31         IFEQ      *OFF
205700950406     C* Non trovata bolla col numero segnacollo specificato: il
205800950406     C* segnacollo non è quello iniziale della bolla e quindi è sicura-
205900950406     C* mente incluso nella spedizione precedente.
206000950406     C     K6ARB1        READPE    FNARB27L                               31
206100131209     C  n31              Z-ADD     ARBNCD        COMSCD
206200950406     C     K6ARB         SETLL     FNARB27L
206300950406     C                   END
206400131113     c
206500950406     C* Non immesso segnacollo dal oppure serie diversa da zero (segna-
206600950406     C* colli non sequenziali) mi posiziono con segnacollo = 0
206700950406     C                   OTHER
206800950406     C                   MOVE      *ZEROS        COMSCD
206900950405     C     K6ARB         SETLL     FNARB27L
207000131115     c                   eval      Readfile='DSP'
207100950406     C                   ENDSL
207200131113    3C                   END
207300131113     c*
207400131113     c* Per LINEA DI ARRIVO
207500940712     C   01K8ARB         SETLL     FNARB26L
207600131115     c                   eval      Readfile='DSP'
207700131111     c
207800971223    2C                   ENDDO
207900131111    1C                   ENDSL
208000940318      *
208100911023     C                   Z-ADD     0             COMOD
208200911023     C                   EXSR      CARSF
208300940318      *
208400901116     C                   ENDSR
208500940318      **************************************************************************
208600940318      *    CARICO SUBFILE IN AVANTI
208700940318      **************************************************************************
208800900515     C     CARSF         BEGSR
208900940318      *
209000900518     C                   Z-ADD     1             B                 2 0
209100940713     C                   Z-ADD     COMOD         NR2
209200000228     C                   clear                   War4AAS
209300000228     C                   clear                   War4LNP
209400000228     C                   clear                   War4NRS
209500000228     C                   clear                   War4NSP
209600000228      *
209700131113    1C     B             DOWLE     18
209800060216     c                   setoff                                       32
209900131111
210000131113    2c                   select
210100131111     c***
210200131111     c* O R M
210300131111     c***
210400131111     c     *in25         wheneq    *on
210500060216     c     kar44         reade     fiar404l                               30
210600010509     C  N30KARB4         CHAIN     FNARB01L                           32
210700131111     C***
210800131111      * R I F .   p a r t n e r
210900131111     C***
211000131111     C     V1CRPE        whenne    *BLANKS
211100050121     c     leggar4       tag
211200000228      *
211300050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
211400050304     c                             and wgiro <> '2'
211500060216     C     KAR4          READE     FiAR403L                               30
211600991222     c                   else
211700000228      * DPD
211800060216     C     wtrc          READE     FiAR403L                               30
211900000228     C                   If        *in30 = *off
212000060518     c                   movel     ar4n30        dsbl4i
212100000228      *
212200000228     c                   Select
212300000228      * Se non trovo n.parcel e selezionati 0 rcd o più di 1: fine lettura
212400060613     c                   When       §b4ipn <> krpe  and  nr2 <> 1
212500991222     C                   SETON                                            30
212600000228      * Se non trovo n.parcel e è selezionato 1 rcd:  fine lettura e
212700060216      *                                               riposiziono FiAR4
212800060613     c                   When       §b4ipn<> krpe
212900000228     C                   SETON                                            30
213000060216     C     KAR4W         CHAIN     FiAR401L
213100000228      * Se ho trovato il n.parcel ed è il 1o memorizzo key sped
213200000228     c                   When       NR2 = *zeros
213300000228     C                   Z-ADD     AR4AAS        War4AAS
213400000228     C                   Z-ADD     AR4LNP        War4LNP
213500000228     C                   Z-ADD     AR4NRS        War4NRS
213600000228     C                   Z-ADD     AR4NSP        War4NSP
213700000228     c                   Endsl
213800000228     c                   Endif
213900000228      *
214000991222     c                   endif
214100991222     c
214200050304     c                   if        *in30 and wgiro <> '2' and
214300050304     c                             lnpntw <> 'DPD' and lnantw <> 'DPD' and
214400050304     c                             wnodpd = *blanks
214500050121     c                   eval      wgiro = '2'
214600060518     C                   MOVEL     'I'           WTRC
214700050121     C                   MOVEL(P)  KRPe          WN30             30
214800060216     C     Kar4          SETLL     Fiar403l
214900050121     c                   goto      leggar4
215000050121     c                   endif
215100061117     c*
215200061117     c* Se rif.partner può essere DPD non export e immesso rif. lungo 11
215300061117     c* faccio tentaivo di ricerca con rif.truccato
215400061117     c                   if        *in30 and lnantw<>'DPD'and wnodpd=*blanks
215500061117     c                             and %subst(krpe:12:1)=' '
215600061117     C                   eval      krpe='0'+%subst(krpe:1:3)+'99'
215700061117     c                             +%subst(krpe:4:8)
215800061117     C                   MOVEL(P)  KRPe          WN30             30
215900061117     C     Kar4          SETLL     Fiar403L
216000061117     c                   goto      leggar4
216100061117     c                   endif
216200050121     c*
216300971223     C  N30KARB4         CHAIN     FNARB01L                           32
216400131111     c**
216500131111     c* C H I   S O N O
216600131111     c**
216700131111    1C     V1CNOT        whenne    *BLANKS
216800131111     C     KarS          reade     FIarS02L                               30
216900131111     c                   if        not *in30
217000131111     C     KarSXT        CHAIN     FNarT27L                           32
217100131111     C  N32KARTXP        CHAIN     FNarb01L                           32
217200131111    3C     *IN32         IFEQ      *On
217300131111     C                   SETON                                        30
217400131111    3C                   ENDIF
217500131111    2C                   ENDIF
217600110930     c*
217700131111     c* Interrogazione per R M N
217800131111    1C     v1crmn        whengt    *zeros
217900111003     c     v1crmn        reade     fnarb33l                               30
218000940318      *
218100131111     c* Interrogazione per R M A
218200131111    1C     comrma        whenne    *BLANKS
218300111005     c     comrma        reade     fnarb32l                               30
218400131111     c
218500131111      * RSD/RSM/IND/LOD                      -----------------------------------
218600131111     c
218700131113     c                   when      *IN14 OR *IN11
218800131113
218900131111     c                   if        Wend=*off
219000131111     c                   exsr      Readcursor
219100131111     c                   endif
219200131111     c
219300131111     c                   if        Wend=*on
219400131111     c                   seton                                        30
219500131111     c                   endif
219600111005     c*
219700131111     C                   other
219800940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1')  ----------------------------------
219900940318     C   18              DO
220000940713     C     V1CLNA        IFNE      *ZERO
220100020910     C   15K2ARB         READE     FNARB78L                               30
220200020910     C  N15K3ARB         READE     FNARB78L                               30
220300940712      *
220400940712     C                   ELSE
220500940712      *
220600950214     C                   SETON                                            31
220700950214     C                   SETOFF                                           30
220800950214     C     *IN31         DOWEQ     *ON
220900020910     C   15K2ARB         READE     FNARB78L                               31
221000020910     C  N15K3ARB         READE     FNARB78L                               31
221100940318     C   31              ADD       1             C
221200940318     C   31L1(C)         IFNE      0
221300940318     C                   Z-ADD     L1(C)         COMLNA
221400020910     C     K1ARB         SETLL     FNARB78L
221500940318     C                   ELSE
221600950214     C                   SETOFF                                       31
221700940318     C                   SETON                                        30
221800940318     C                   ENDIF
221900940318     C                   ENDDO
222000940318      *
222100940712     C                   ENDIF
222200940318     C                   ENDDO
222300940318      *
222400940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
222500940712     C   19              DO
222600940713     C     V1CLNA        IFNE      *ZERO
222700120330     C     V1CLNA        READE     FNARB05L                               30
222800940712     C                   ELSE
222900120330     C                   READ      FNARB05L                               30
223000940712     C                   ENDIF
223100940712     C                   ENDDO
223200940318      *
223300940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
223400940318     C   20              DO
223500940713     C     V1CLNA        IFNE      *ZERO
223600940712     C   15K5ARB         READE     FNARB54L                               30
223700940713     C  N15V1CLNA        READE     FNARB54L                               30
223800940712      *
223900940712     C                   ELSE
224000940712      *
224100940712     C                   SETON                                            31
224200940712     C     *IN31         DOWEQ     '1'
224300940712     C  N36
224400940712     CAN 15K5ARB         READE     FNARB54L                               31
224500940712     C  N36
224600940712     CANN15COMLNA        READE     FNARB54L                               31
224700060216     C  N31              SETOFF                                       30
224800980120     C   31              ADD       1             C
224900940318     C   31L1(C)         IFNE      0
225000940318     C                   Z-ADD     L1(C)         COMLNA
225100940712     C     K4ARB         SETLL     FNARB54L
225200940318     C                   SETOFF                                       36
225300940318     C                   ELSE
225400940318     C                   SETON                                        30
225500940318     C                   SETOFF                                       31
225600940318     C                   ENDIF
225700940318     C                   ENDDO
225800940318      *
225900940712     C                   ENDIF
226000940318     C                   ENDDO
226100940318      *
226200940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
226300940318     C   21              DO
226400940712     C   01
226500940712     CAN 15K9ARB         READE     FNARB26L                               30
226600940713     C   01
226700940713     CANN15V1CLNA        READE     FNARB26L                               30
226800940712     C   02
226900940712     CAN 15K7ARB         READE     FNARB27L                               30
227000940713     C   02
227100940713     CANN15V1CLP1        READE     FNARB27L                               30
227200131113     c                   ENDDO
227300131113     c                   endsl
227400940318      *
227500060216     c  n32
227600060216     CanN30              EXSR      CONFR
227700900515     C  N30
227800060216     CANN32              EXSR      EMISS
227900900515     C  N30              END
228000131113     c
228100940713     C                   Z-ADD     NR2           COMOD             4 0
228200930518     C                   SETOFF                                       1362
228300131113     c
228400131209     c** 30              if        Wend=*on
228500131209     c   30              if        WopenC='1'
228600131113     c                   exsr      closecursor
228700131113     c                   endif
228800940318      *
228900900515     C                   ENDSR
229000940317      **************************************************************************
229100940317      *    CONFRONTO PER CERCARE BOLLE DA EMETTERE
229200940317      **************************************************************************
229300900515     C     CONFR         BEGSR
229400940317      *
229500911024     C                   SETOFF                                       3236
229600131111     c                   z-add     arbmgs        arbdsp
229700131111     c                   movel     arbaas        arbdsp
229800131111     c
229900110930     c* Se richiamata per Perfect Order devo scartare la bolla se xco diverso da
230000110930     c* quello passato
230100110930     c                   if        pa1flg='W' and arbxco<>pa1xco
230200110930     c                   seton                                        32
230300110930     c                   goto      endcon
230400110930     c                   endif
230500940318      *
230600940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1')  ----------------------------------
230700940712      * DEVO CONTROLLARE LA DATA SPEDIZIONE (ARBDSP)
230800971223    1C   18              DO
230900131115     c                   if        comdtd>0 and *in15
231000131115    2C     ARBDSP        IFNE      COMDTD
231100940713     C     ARBDCM        ANDEQ     *ZERO
231200131115     c
231300131115    3C                   IF        v1clna<>0 and readfile='DSP'
231400940712     C                   SETON                                            30
231500131115     c                   endif
231600131115
231700131115     C                   SETON                                            32
231800911023     C                   GOTO      ENDCON
231900971223    2C                   END
232000131115     c                   endif
232100940318      *
232200131115     c                   if        comdtd>0 and not *in15
232300131115    2C     ARBDSP        IFGT      COMDTA
232400940713     C     ARBDCM        ANDEQ     *ZERO
232500131115     c* si potrebe uscire anche se lettura per riferimenti mittente
232600131115     c*  ma se la linea di arrivo =0 non lo fa ora .... si può sistemare
232700131115     c*  per far leggere meno
232800131115    3C                   IF        v1clna<>0 and readfile='DSP'
232900131115     C                   SETON                                            30
233000131115     c                   endif
233100131115
233200940712     C                   SETON                                            32
233300911023     C                   GOTO      ENDCON
233400971223    2C                   END
233500131115     c                   endif
233600131115     c
233700131115    1C                   END
233800940318      *
233900940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
234000940712      * DEVO CONTROLLARE LA DATA DISTINTA (ARBDDC)
234100971223    1C   19              DO
234200960527     C     ARBFBC        IFNE      *BLANKS
234300960527     C                   SETON                                        32
234400960527     C                   GOTO      ENDCON
234500960527     C                   END
234600960527     C*
234700131115     c                   if        comdtd>0 and *in15
234800131115     C     ARBDDC        IFNE      COMDTD
234900911024     C                   SETON                                            32
235000911024     C                   GOTO      ENDCON
235100911024     C                   END
235200131115     C                   ENDif
235300940318      *
235400131115     c                   if        comdtd>0 and not *in15
235500131115     C     ARBDDC        IFLT      COMDTD
235600940712     C     ARBDDC        ORGT      COMDTA
235700911024     C                   SETON                                            32
235800911024     C                   GOTO      ENDCON
235900911024     C                   END
236000131115     C                   ENDif
236100940318      *
236200940321      * SE NON C'E' NUMERO DISTINTA NON E' IN CONSEGNA
236300911024     C     ARBNDC        IFEQ      0
236400911024     C                   SETON                                            32
236500911024     C                   GOTO      ENDCON
236600911024     C                   END
236700971223    1C                   END
236800940318      *
236900940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
237000940712      *  DEVO CONTROLLARE LA DATA CONSEGNA (ARBDCM)
237100971223    1C   20              DO
237200131115     c                   if        comdtd>0 and *in15
237300131115    2C     ARBDCM        IFNE      COMDTD
237400131115    3C                   IF        v1clna<>0 and readfile='DCM'
237500131115     C                   SETON                                            30
237600131115     c                   endif
237700131115
237800940712     C                   SETON                                            32
237900911024     C                   GOTO      ENDCON
238000971223    2C                   END
238100131115     c                   endif
238200940318      *
238300131115     c                   if        comdtd>0 and not *in15
238400131212     C     ARBDCM        IFLT      COMDTD
238500131212     C                   SETON                                            32
238600131212     C                   GOTO      ENDCON
238700131212     C                   END
238800131212     c
238900131115    2C     ARBDCM        IFGT      COMDTA
239000131115    3C                   IF        v1clna<>0 and readfile='DCM'
239100131115     C                   SETON                                            30
239200131115     c                   endif
239300131115
239400940712     C                   SETON                                            32
239500911024     C                   GOTO      ENDCON
239600971223    2C                   END
239700131115     c                   endif
239800131115     c
239900131115      * SE NON C'E' data consegna  NON E' consegnata
240000131115     C     ARBDCM        IFEQ      0
240100131115     C                   SETON                                            32
240200131115     C                   GOTO      ENDCON
240300131115     C                   ENDIF
240400131115    1C                   END
240500940329      * ------------------------------------------------------------------------
240600940712      * HO SPOSTATO IL CONFRONTO CON MITTENTE E DESTINATARIO PRIMA DEL BLOCCO
240700940712      * CON 21 ON PERCHE' POSSO ACCENDERE IL 30 E ANDARE A FINE CICLO QUANDO
240800940712      * CAMBIA LA RAG.SOCIALE (EVITO DELLE LETTURE)
240900940712      *
241000940712      * OCCHIO!!   Devo usare per primo il confronto che potrebbe portarmi a
241100940712      * fine ciclo: se interrogo per destinatario (14ON) ci deve essere prima il
241200940713      * confronto con V1CRSD, se invece sono per mittente devo fare prima quello
241300940713      * con V1CRSM.
241400060613     C                   MOVE      *BLANKS       $SALTA            1
241500940712      *
241600940712      *??  R A G I O N E    S O C I A L E    D E S T I N A T A R I O  ?
241700131113     c                   if        not *in11 and not *in14
241800131113    1C     V1CRSD        IFNE      *BLANKS
241900131113     c
242000131113    2c     v1cxrsd       ifeq      ' '
242100131113     C     ZZ            SUBST(P)  ARBRSD        COMRAG
242200940712      * Se rag.soc. letta > video riposiziono solo per data con SETGT
242300131113    3C     COMRAG        IFNE      V1CRSD
242400131113     C                   MOVE      'S'           $SALTA
242500131113    3C                   ENDIF
242600131113     c*
242700131113   x2c                   else
242800131114     C                   MOVEL     ARBAAS        KAAS
242900131114     C                   MOVEL     ARBLNP        KLNP
243000131114     C                   MOVEL     ARBNRS        KNRS
243100131114     C                   MOVEL     ARBNSP        KNSP
243200131114     C                   MOVEL     'D'           KTRC
243300131114     C     KAR41         CHAIN     FiAR401L
243400131114     c                   if        %found(fiar401l)
243500131114     c                   eval      arbrsduni=arbrsd+ar4not
243600131114     c                   else
243700131114     c                   eval      arbrsduni=arbrsd
243800131114     c                   endif
243900131114     c
244000131114    3c                   if        %scan(%trimr(v1crsd):arbrsdUNI) = 0
244100131113     C                   MOVE      'S'           $SALTA
244200131113    3c                   endif
244300131113    2C                   ENDIF
244400131113    1C                   ENDIF
244500131113     c
244600131113      *??  I N D I R I Z Z O     D E S T I N A T A R I O  ?
244700131113    1C     V1Cind        IFNE      *BLANKS
244800131113     c
244900131113    2c     v1cxind       ifeq      ' '
245000131113     C     DD            SUBST(P)  ARBind        COMRAG           30
245100131113      * Se rag.soc. letta > video riposiziono solo per data con SETGT
245200131113    3C     COMRAG        IFNE      V1Cind
245300131113     C                   MOVE      'S'           $SALTA
245400131113    3C                   ENDIF
245500131113     c*
245600131113   x2c                   else
245700131113    3c                   if        %scan(%trimr(v1cind):arbind) = 0
245800131113     C                   MOVE      'S'           $SALTA
245900131113    3c                   endif
246000131113    2C                   ENDIF
246100131113    1C                   ENDIF
246200131113      *??  L O C A L I T A'      D E S T I N A T A R I O  ?
246300131113    1C     V1Clod        IFNE      *BLANKS
246400131113     c
246500131113    2c     v1cxlod       ifeq      ' '
246600131113     C     LL            SUBST(P)  ARBlod        COMRAG
246700131113      * Se rag.soc. letta > video riposiziono solo per data con SETGT
246800131113    3C     COMRAG        IFNE      V1Clod
246900131113     C                   MOVE      'S'           $SALTA
247000131113    3C                   ENDIF
247100131113     c*
247200131113   x2c                   else
247300131113    3c                   if        %scan(%trimr(v1clod):arblod) = 0
247400131113     C                   MOVE      'S'           $SALTA
247500131113    3c                   endif
247600131113    2C                   ENDIF
247700131113    1C                   ENDIF
247800940414      *
247900940712      *??  R A G I O N E    S O C I A L E    M I T T E N T E  ?
248000131113    1C     V1CRSM        IFNE      *BLANKS
248100131113    2c     v1cxrsm       ifeq      ' '
248200940712     C     XX            SUBST(P)  ARBRSM        COMRAG
248300940712      * Se rag.soc. letta > video riposiziono solo per data con SETGT
248400131113    3C     COMRAG        IFne      V1CRSM
248500940712     C                   MOVE      'S'           $SALTA
248600131113    3C                   ENDIF
248700131113   x2c                   else
248800131113    3c                   if        %scan(%trimr(v1crsm):arbrsm) = 0
248900131113     C                   MOVE      'S'           $SALTA
249000131113    3c                   endif
249100131113    2C                   ENDIF
249200131113    1C                   ENDIF
249300131113    1C                   ENDIF
249400940712      *
249500131115     c                   if        $SALTA='S'
249600131115     c                   seton                                        32
249700131115     C                   goto      ENDCON
249800131115     c                   endif
249900940318      *
250000940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
250100971223    1C   21              DO
250200940712     C     ARBDSP        IFLT      COMDTD
250300940712     C     COMDTD        ANDNE     *ZERO
250400940712     C                   SETON                                            32
250500911024     C                   GOTO      ENDCON
250600911024     C                   END
250700940318      *
250800940321     C     ARBDSP        IFGT      COMDTA
250900940712     C     COMDTA        ANDNE     *ZERO
251000940712     C                   SETON                                            32
251100131115     c                   if        readfile='DSP'
251200131115     C                   SETON                                            30
251300131115     c                   endif
251400131115     c
251500911024     C                   GOTO      ENDCON
251600131115     C                   ENDif
251700131115     c
251800971223    1C                   ENDDO
251900971223     C**
252000940317      *
252100940317      * PER OGNI SELEZIONE  ----------------------------------------------------
252200940317      *
252300940317      * TESTO LA LINEA DI ARRIVO
252400971223     C     V1CRPE        IFEQ      *BLANKS
252500971223     C     ARBLNA        ANDNE     V1CLNA
252600940713     C     V1CLNA        ANDNE     *ZERO
252700920911     C                   SETON                                            32
252800920911     C                   GOTO      ENDCON
252900971223     C                   ENDIF
253000020628      * testo la nazione/provincia del destinatario
253100060613     c                   if        v1cprd <> *blanks  and
253200020628     c                             (arbnzd <> v1cnzd  or arbprd <> v1cprd)
253300020628     c                   eval      *in32 = *on
253400020628     c                   goto      endcon
253500020628     c                   endif
253600060613     c                   if        (v1cnzd <> *blanks and v1cprd = *blanks) and
253700060613     c                             arbnzd <> v1cnzd
253800060613     c                   eval      *in32 = *on
253900060613     c                   goto      endcon
254000060613     c                   endif
254100020628
254200940317      *
254300940712      * VERIFICO ANCHE LA LNP POICHE' POTREI AVER FATTO ALTRE SELEZIONI PRIMARIE
254400940713     C     ARBLNP        IFNE      V1CLP1
254500940713     C     V1CLP1        ANDNE     *ZERO
254600940712     C                   SETON                                            32
254700940712     C                   GOTO      ENDCON
254800940712     C                   END
254900940923     C* FILIALE SEGNACOLLI
255000940926     C* SE NON VI SONO COLLI, SALTARE QUESTO CONTROLLO
255100940926     C     ARBNCD        IFNE      *ZERO
255200940926     C     ARBNCA        ORNE      *ZERO
255300940926     C     ARBFNS        OREQ      'S'
255400940926     C     ARBFLS        ORNE      *ZERO
255500940923     C     V1CFLS        IFNE      *ZERO
255600940923     C     V1CFLS        ANDNE     ARBFLS
255700940923     C                   SETON                                            32
255800940923     C                   GOTO      ENDCON
255900940926     C                   ENDIF
256000940926     C                   ENDIF
256100961112     C* RESO / FRANCO FRONTIERA
256200961112     C     V1CFBR        IFNE      ' '
256300961112     C                   MOVEL     V1CFBR        WFBR
256400961112     C     V1CFBR        IFEQ      'R'
256500961112     C                   MOVEL     'S'           WFBR
256600961112     C                   ENDIF
256700961112     C     ARBFBR        IFNE      WFBR
256800961112     C                   SETON                                            32
256900961112     C                   GOTO      ENDCON
257000961112     C                   ENDIF
257100961112     C                   ENDIF
257200040216      * Tipo bolla
257300040216     c                   if            V1Ccbo <> *blanks
257400040216     c                             and V1Ccbo <> ARBcbo
257500040216     c                   eval      *in32 = *on
257600040216     c                   goto      EndCon
257700040216     c                   endif
257800900515     C* TIPO SPEDIZIONE
257900940715     C     ARBTSP        IFNE      V1CTSP
258000940715     C     V1CTSP        ANDNE     *BLANKS
258100900515     C                   SETON                                            32
258200900515     C                   GOTO      ENDCON
258300900515     C                   END
258400911023     C* SPEDIZIONI GIACENTI
258500980603     C     V1CGIA        IFEQ      'G'
258600930702     C     ARBFBC        ANDNE     'G'
258700911023     C                   SETON                                            32
258800911023     C                   GOTO      ENDCON
258900911023     C                   END
259000980603     C     V1CGIA        IFEQ      'N'
259100930702     C     ARBFBC        ANDEQ     'G'
259200911023     C                   SETON                                            32
259300911023     C                   GOTO      ENDCON
259400911023     C                   END
259500070420     c*
259600070420     c* SPEDIZIONI IN FERMO DEPOSITO
259700070420     C                   IF        v1csfd='S' and arbffd<>'S'
259800070420     C                   SETON                                            32
259900070420     C                   GOTO      ENDCON
260000070420     C                   END
260100070420     c
260200900518     C* SERIE DAL + AL
260300940713     C     ARBNRS        IFLT      V1CPSD
260400900518     C                   SETON                                            32
260500900518     C                   GOTO      ENDCON
260600900518     C                   END
260700940713     C     ARBNRS        IFGT      V1CPSA
260800900515     C                   SETON                                            32
260900900515     C                   GOTO      ENDCON
261000900515     C                   END
261100900518     C* SEGNACOLLO DAL SOLO
261200930702     C     *IN07         IFEQ      *ON
261300970925     C     *IN84         ANDEQ     *OFF
261400940713     C     KART          SETLL     FNART01L                               31
261500930702     C  N31              SETON                                        32
261600930702     C  N31              GOTO      ENDCON
261700930702     C                   END
261800900515     C* SEGNACOLLO DAL + AL
261900970925     C   84              DO
262000910827     C                   SETON                                        32
262100940713     C     KARB2         SETLL     FNART01L
262200940713     C     KARB2         READE     FNART01L                               31
262300910827     C     *IN31         DOWEQ     '0'
262400940713     C     ARTNSC        IFGE      V1CSCD
262500940713     C     ARTNSC        ANDLE     V1CSCA
262600910827     C                   SETON                                        31
262700910827     C                   SETOFF                                       32
262800910827     C                   END
262900910827     C*
263000940713     C  N31KARB2         READE     FNART01L                               31
263100910827     C                   END
263200910827     C*
263300910827     C   32              GOTO      ENDCON
263400910827     C                   END
263500990119     C*
263600990119     C* NUMERO CA
263700990119    1C     V1CDAN        IFNE      ' '
263800040809     C                   SETON                                        32
263900040809      *
264000040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
264100040809      * legate alla spedizione che passo
264200040809     c                   clear                   fidn12ds
264300040809     c                   eval      i12aas = arbAAS
264400040809     c                   eval      i12lnp = arbLNP
264500040809     c                   eval      i12nrs = arbNRS
264600040809     c                   eval      i12nsp = arbNSP
264700040809     c                   eval      i12fel = 'E'
264800040809     c                   eval      i12fan = 'E'
264900040809     c*
265000040809     c                   call      'FIDN12R'
265100040809     c                   parm                    fidn12ds
265200040809     c*
265300040809     c                   setoff                                       31
265400040809     c*
265500040809     c* se non ci sono errori
265600040809     c                   if        o12err <> ' '
265700040809     c                   seton                                        31
265800040809     c                   else
265900040809     c* se numero di CA trovate maggiore di zero
266000040809     c                   if        o12nca = 0
266100040809     c                   seton                                        31
266200040809     c                   else
266300040809     c                   z-add     1             ii                2 0
266400040809     c                   dow       ii <= o12nca and *in31 = *off
266500040809     C* APERTA
266600040809     C     skdch(ii)     IFEQ      0
266700040809     C     V1CDAN        ANDEQ     'A'
266800040809     C                   SETOFF                                       32
266900040809     C                   SETON                                        31
267000040809     C                   ENDIF
267100040809     C* CHIUSE
267200040809     C     skdch(ii)     IFGT      0
267300040809     C     V1CDAN        ANDEQ     'C'
267400040809     C                   SETOFF                                       32
267500040809     C                   SETON                                        31
267600040809     C                   ENDIF
267700040809     C* CA APERTA O CHIUSA
267800040809     C     V1CDAN        IFEQ      'E'
267900040809     C                   SETOFF                                       32
268000040809     C                   SETON                                        31
268100040809     C                   ENDIF
268200040809     c*
268300040809     c                   add       1             ii
268400040809     c                   enddo
268500040809     c*
268600040809     c                   seton                                        31
268700040809     c*
268800040809     c                   endif
268900040809     c                   endif
269000040809     c**
269100990119    3C                   ENDIF
269200030527     C*
269300940317     C     ENDCON        ENDSR
269400940802     C**************************************************************************
269500940802     C* RICHIAMO PGM DI INTERROGAZIONE PARTENZE
269600940802     C**************************************************************************
269700930922     C     INTPAR        BEGSR
269800940802     C*
269900940722     C                   MOVEL     'A'           PA2FLG
270000930922     C                   MOVEL     PARAM2        KPJBU
270100940722     C                   CALL      'FNLS04R'
270200930922     C                   PARM                    KPJBA
270300940802     C*
270400930922     C                   ENDSR
270500131114     C**************************************************************************
270600131114     C* Atre parzializzazioni
270700131114     C**************************************************************************
270800131114     C     AltrePar      BEGSR
270900131114     c     forw12        TAG
271000131114     c                   exfmt     LRI6W12
271100131114     c
271200131114     c                   setoff                                       285690
271300131114     c                   setoff                                       40
271400131114     c
271500131114     C* GIACENTI
271600131114     C* SCELTE INCONGRUENTI
271700131114     C     V1CGIA        IFEQ      'G'
271800131114     C     V1CSSP        IFEQ      '2'
271900131114     C     V1CSSP        OREQ      '3'
272000131114     C                   MOVEL     MSG(14)       w12MSG
272100131114     C                   SETON                                        285690
272200131114     C                   GOTO      forw12
272300131114     C                   END
272400131114     C                   END
272500131114      * Tipo bolla
272600131114if  1c                   if        V1Ccbo <> *blanks
272700131114     c                   eval      *in31  =  *off
272800131114     c     '?'           scan      V1Ccbo                                 31
272900131114if  2c                   if        *in31 = *on
273000131114     c                   call      'X§TABER'
273100131114     c                   parm      CodUt         §KUT
273200131114     c                   parm      '3A'          COD
273300131114     c                   parm      *blanks       KEY
273400131114     c                   parm      *blanks       §DES
273500131114     c                   movel     KEY           V1Ccbo
273600131114     c                   eval      *in90 = *on
273700131114     c                   goto      forw12
273800131114e   2c                   endif
273900131114      *
274000131114     c                   movel     '3A'          COD
274100131114     c                   movel(p)  V1Ccbo        KEY
274200131114     c     KTAB1         chain     TABEL
274300131114if  2c                   if        NOT %found(TABEL00F)
274400131114     c                             or  TBLflg <> *blanks
274500131114     c                   movel     Msg(9)        w12msg
274600131114     c                   seton                                        284090
274700131114     c                   goto      forw12
274800131114e   2c                   endif
274900131114e   1c                   endif
275000131114     c
275100131114     c                   if        v1cgia<>' ' or v1cdan<>' ' or v1caff<>'N'
275200131114     c                             or v1cfbr<>' ' or v1ccbo<>*blanks
275300131114     c                   seton                                        09
275400131114     c                   else
275500131114     c                   setoff                                       09
275600131114     c                   endif
275700131114     c
275800131114     C*
275900131114     C                   ENDSR
276000940802     C**************************************************************************
276100940802     C* EMISSIONE SUBFILE
276200940802     C**************************************************************************
276300900515     C     EMISS         BEGSR
276400940802     C*
276500930407     C                   SETOFF                                       1362
276600940714     C                   MOVEL     *BLANKS       V2CSCE
276700131121     c**                 if        *in87=*off and not *in02
276800131121     C**                 MOVEL     ARBLNP        V2CLIN
276900131121     c**                 endif
277000131121     c**                 if        *in87 or *in02
277100131121     C**                 MOVEL     ARBLNA        V2CLIN
277200131121     c**                 endif
277300940714     C                   MOVEL     ARBLNP        V2CLNP
277400941212     C                   MOVEL     ARBLNA        V2CLNA
277500911023     C                   MOVEL     ARBMGS        MMGG
277600940714     C                   MOVEL     GG            V2CDSP
277700940714     C                   MOVE      MM            V2CDSP
277800940714     C                   MOVE      ARBAAS        V2CAAS
277900950114     C                   MOVEL     ARBNSP        V2CNSP
278000900516     C*
278100060130     C                   MOVEL     ARBksc        V2Cksc
278200060130     C                   MOVEL     ARBcbo        V2Ccbo
278300060130     C                   MOVEL     ARBRSM        V2CRSM
278400940714     C                   MOVEL     ARBRSD        V2CRSD
278500131114     c                   if        v1cind=*blanks
278600131114     c                   eval      v2cdest=arblod
278700131114     c                   else
278800131114     c                   eval      v2cdest=arbind
278900131114     c                   endif
279000131114     c                   if        arbrma<>*blanks
279100131114     c                   eval      v2crifm=%trim(%editc(arbrmn:'Z')) +'/'+
279200131114     c                                     %trim(arbrma)
279300131114     c                   else
279400131114     c                   eval      v2crifm=%trim(%editc(arbrmn:'Z'))
279500131114     c                   endif
279600131114     C**                 Z-ADD     ARBRMN        V2CRMN
279700131114     C**                 MOVEL     ARBRMA        V2CRMA
279800940714     C                   MOVEL     ARBNRS        V2CNRS
279900940923     C                   MOVEL     ARBFLS        V2CFLS
280000940714     C                   MOVEL     *BLANKS       V2CFLA
280100990119     C* SPEDIZIONE IN DANNO
280200990119     C* SE MI HANNO CHIESTO LE CA CHIUSE VISUALIZZO IL FLAG "C"
280300990119     C* SE MI HANNO CHIESTO LE CA APERTE VISUALIZZO IL FLAG "*"
280400990119     C                   SELECT
280500990119     C     V1CDAN        WHENEQ    'C'
280600990119     C                   MOVEL     'C'           V2CDAN
280700990119     C     V1CDAN        WHENEQ    'A'
280800990119     C                   MOVEL     '*'           V2CDAN
280900990119     C**
281000990119     C                   OTHER
281100040809      *
281200040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
281300040809      * legate alla spedizione che passo
281400040809     c                   clear                   fidn12ds
281500040809     c                   eval      i12aas = arbAAS
281600040809     c                   eval      i12lnp = arbLNP
281700040809     c                   eval      i12nrs = arbNRS
281800040809     c                   eval      i12nsp = arbNSP
281900040809     c                   eval      i12fel = 'E'
282000040809     c                   eval      i12fan = 'E'
282100040809     c*
282200040809     c                   call      'FIDN12R'
282300040809     c                   parm                    fidn12ds
282400040809     c*
282500040809     c                   setoff                                       31
282600040809     c*
282700040809     c* se non ci sono errori
282800040809     c                   if        o12err <> ' '
282900040809     c                   seton                                        31
283000040809     c                   else
283100040809     c* se numero di CA trovate maggiore di zero
283200040809     c                   if        o12nca = 0
283300040809     c                   seton                                        31
283400040809     c                   else
283500040809     c                   z-add     1             ii                2 0
283600040809     c                   dow       ii <= o12nca and *in31 = *off
283700040809     C* SE NE TROVO UNA APERTA, DICO APERTA ALTRIMENTI CHIUSA
283800040809     C     skdch(ii)     IFEQ      0
283900040809     C                   MOVEL     '*'           V2CDAN
284000040809     C                   SETON                                            31
284100040809     C                   ELSE
284200040809     C* CHIUSA MA VADO AVANTI A LEGGERE
284300040809     C                   MOVEL     'C'           V2CDAN
284400040809     C                   ENDIF
284500040809     c*
284600040809     c                   add       1             ii
284700040809     c                   enddo
284800040809     c*
284900040809     c                   seton                                        31
285000040809     c*
285100040809     c                   endif
285200040809     c                   endif
285300990119     C**
285400990119     C                   ENDSL
285500921102     C* SEGNACOLLI
285600930702     C                   CLEAR                   SEG
285700921102     C                   MOVEL     ARBNCD        COMSCN            7
285800921102     C                   MOVEA     COMSCN        SEG(1)
285900921102     C     ARBNCA        IFNE      0
286000921102     C                   MOVEL     ARBNCA        COMSCN
286100921102     C                   MOVEA     COMSCN        SEG(9)
286200921102     C                   END
286300921102     C                   MOVEA     '/'           SEG(8)
286400921102     C* ELIMINO ZERO NON SIGNIFICATIVI
286500921102     C                   Z-ADD     1             Y
286600921102     C     SEG(Y)        DOWEQ     '0'
286700921102     C                   MOVE      ' '           SEG(Y)
286800921102     C                   ADD       1             Y
286900921102     C                   END
287000921102     C*
287100921102     C     ARBNCA        IFNE      0
287200921102     C                   Z-ADD     9             Y                 2 0
287300921102     C     SEG(Y)        DOWEQ     '0'
287400921102     C                   MOVE      ' '           SEG(Y)
287500921102     C                   ADD       1             Y
287600921102     C                   END
287700921102     C                   END
287800940714     C                   MOVEA     SEG           V2CSEG
287900911023     C*
288000911023     C* GIACENZA APERTA
288100941202     C                   Z-ADD     ARBAAS        KAAS
288200940713     C                   Z-ADD     ARBLNP        KLNP
288300940713     C                   Z-ADD     ARBNRS        KNRS
288400940713     C                   Z-ADD     ARBNSP        KNSP
288500050405     C     KGCA1         CHAIN     tigcp21l                           31
288600920410     C  N31GCPATB        IFEQ      ' '
288700920410     C     GCPDCG        IFNE      0
288800990119     C                   MOVE      'C'           V2CGCA
288900911023     C                   ELSE
289000990119     C                   MOVE      '*'           V2CGCA
289100900515     C                   END
289200920410     C                   END
289300990119     C                   MOVEL     ARBTSP        V2CTSP
289400930503     C*
289500911023     C* DATA CONSEGNA
289600940714     C                   MOVEL     *BLANKS       V2CCON
289700940714     C                   MOVEL     *BLANKS       V2CFCC
289800930407     C*
289900930407     C* SE ARBCCA >< 0  METTO DECODIFICA AL POSTO DELLA DATA CONSEGNA
290000930407     C     ARBCCA        IFNE      ' '
290100930705     C                   Z-ADD     1             C
290200930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
290300940909     C   31              MOVEL     DCA(C)        DS7O
290400940909     C*
290500940909     C* NELLA DS7O C'E' SCRITTO SE E' PRIORITARIA L'ANOMALIA O LA DATA
290600940909     C   31§7OVCA        IFEQ      'S'
290700940912     C     §7OVCA        ORNE      'S'
290800940912     C     ARBDCM        ANDEQ     *ZERO
290900940909     C                   MOVEL     §7ODEI        V2CCON
291000940909     C                   SETON                                        62
291100940912     C                   ELSE
291200940909     C*
291300940912     C     ARBDCM        IFNE      *ZERO
291400940909     C                   MOVE      ARBDCM        G02INV
291500940909     C                   Z-ADD     *ZEROS        G02DAT
291600940909     C                   MOVEL     '3'           G02ERR
291700940909     C                   CALL      'XSRDA8'
291800940909     C                   PARM                    WLBDAT
291900940909     C                   MOVEL     G02DAT        V2CCON
292000940909     C                   MOVE      G02DAT        $COMOD            2
292100940909     C                   MOVE      $COMOD        V2CCON
292200940909     C                   ENDIF
292300940912     C                   ENDIF
292400930407     C*
292500930407     C                   ELSE
292600940912     C     ARBDCM        IFNE      *ZERO
292700940712     C                   MOVE      ARBDCM        G02INV
292800940712     C                   Z-ADD     *ZEROS        G02DAT
292900940712     C                   MOVEL     '3'           G02ERR
293000940712     C                   CALL      'XSRDA8'
293100940712     C                   PARM                    WLBDAT
293200940714     C                   MOVEL     G02DAT        V2CCON
293300940713     C                   MOVE      G02DAT        $COMOD            2
293400940714     C                   MOVE      $COMOD        V2CCON
293500940912     C                   ENDIF
293600940912     C                   ENDIF
293700940912     C*
293800940912     C     ARBDCM        IFNE      0
293900930428     C*
294000930506     C* SE ARBDCE = 0  CALL PGM PER CALCOLO DELLA DATA CONSEGNA PREVISTA
294100930506     C*   E DELLE ORE DI RITARDO
294200940712      * SOLO SE RICHIESTA L'AFFIDABILITA'
294300940713     C     V1CAFF        IFEQ      'S'
294400930506     C     ARBDCE        IFEQ      0
294500991027     C                   CLEAR                   TNSD99DS
294600930506     C                   MOVEL     'A'           D98TBO
294700930506     C                   MOVEL     ARBAAS        D98AAS
294800930506     C                   MOVEL     ARBLNP        D98LNP
294900930506     C                   MOVEL     ARBNRS        D98NRS
295000930506     C                   MOVEL     ARBNSP        D98NSP
295100930506     C*
295200970925     C                   CALL      'TNSD99R'
295300991027     C                   PARM                    TNSD99DS
295400930506     C*
295500941214     C* SE PA098 = 1  E' STATA FATTA CALL PGM
295600941214     C                   MOVEL     '1'           PA098
295700930506     C*
295800960201     C* SE C'E' ERRORE VISUALIZZO UNA "R"
295900110301     C     D98ERR        IFNE      '0'
296000960201     C                   MOVEL     'R'           V2CFCC
296100960201     C                   ELSE
296200930506     C* SE D98CRC = S  LA SPEDIZ. E' STAT CONSEGNATA NEI TEMPI PREVISTI
296300930506     C     D98CRC        IFEQ      'S'
296400940714     C                   MOVEL     ' '           V2CFCC
296500930506     C                   ELSE
296600940714     C                   MOVEL     'F'           V2CFCC
296700930506     C                   END
296800960201     C                   END
296900930506     C*
297000930506     C                   ELSE
297100930506     C* ARBDCE > 0
297200940711     C* SE ARBNRC > 0  LA SPEDIZIONE NON E' CONFORME
297300940711     C     ARBNRC        IFGT      0
297400940714     C                   MOVEL     'F'           V2CFCC
297500930504     C                   ELSE
297600940714     C                   MOVEL     ' '           V2CFCC
297700930504     C                   END
297800930506     C                   END
297900940713     C                   ENDIF
298000930402     C*
298100911023     C                   ELSE
298200911023     C* SE NUMERO >< 0 IN CONSEGNA
298300911023     C     ARBNDC        IFNE      0
298400940714     C                   MOVEL     'IN CON'      V2CCON
298500911023     C                   SETON                                        13
298600911023     C                   END
298700911023     C                   END
298800921102     C*
298900940714     C     V2CCON        IFEQ      *BLANKS
299000921102     C     ARBFBC        ANDNE     ' '
299100921102     C                   SETON                                        62
299200921102     C*
299300921102     C     ARBFBC        IFEQ      'B'
299400930504     C     ARBFBC        OREQ      '*'
299500940714     C                   MOVEL     'BLOC.'       V2CCON
299600921102     C                   ELSE
299700921102     C     ARBFBC        IFEQ      'A'
299800940714     C                   MOVEL     'L.AVV'       V2CCON
299900921102     C                   ELSE
300000940714     C                   MOVEL     'GIAC.'       V2CCON
300100921102     C                   END
300200921102     C                   END
300300921102     C                   END
300400911023     C*
300500940714     C                   MOVEL     ARBTMC        V2CTMC
300600900522     C*
300700900518     C                   ADD       1             B
300800940713     C                   ADD       1             NR2
300900131107     C                   WRITE     LRI6S02
301000940720     C*
301100900515     C                   ENDSR
301200940802     C**************************************************************************
301300940802     C* EMISSIONE BOLLA
301400940802     C**************************************************************************
301500911023     C     BOLLA         BEGSR
301600940802     C*
301700070411     C                   SETOFF                                       220616
301800070403     C                   SETOFF                                       170377
301900070404     C                   SETOFF                                       262708
302000131127     C                   SETOFF                                       2933
302100940408     C                   SETOFF                                       687069
302200070524     C                   SETOFF                                       850488
302300131120     C                   SETOFF                                       86
302400991027      *
302500940719     C                   Z-ADD     0             V3CLNA
302600940719     C                   Z-ADD     0             V3CDSP
302700930517     C                   Z-ADD     0             VIDRMN
302800940712     C                   MOVE      *BLANKS       VIDRMA
302900991217     C                   CLEAR                   V3CRPP
303000131120     C                   CLEAR                   atrpda
303100131127     C                   CLEAR                   atrtsp
303200961112     C*
303300911022     C* CONTROLLO SE SPEDIZIONE GIACENTE
303400920219     C* GIACENZA APERTA
303500941202     C                   Z-ADD     ARBAAS        KAAS
303600940713     C                   Z-ADD     ARBLNP        KLNP
303700940713     C                   Z-ADD     ARBNRS        KNRS
303800940713     C                   Z-ADD     ARBNSP        KNSP
303900050405     C     KGCA1         CHAIN     tigcp21l                           31
304000040216if  2C  N31GCPATB        IFEQ      ' '
304100920410     C     GCPDCG        IFNE      0
304200920219     C                   SETON                                        03
304300920219     C                   ELSE
304400070403     C                   SETON                                        0327
304500070412     C                   EVAL      V1DFBC=' GIACENTE '
304600040217     C                   ENDIF
304700040217e   2C                   ENDIF
304800930428     C*
304900021213     C*
305000021213     C* 77 ON - Esistono note supermenrcati d ainterrogare
305100021213     C                   Z-ADD     ARBAAS        KAAS
305200021213     C                   Z-ADD     ARBLNP        KLNP
305300021213     C                   Z-ADD     ARBNRS        KNRS
305400021213     C                   Z-ADD     ARBNSP        KNSP
305500021213     C                   MOVEL     'SCR'         KTRD
305600021213     C     KAR51         CHAIN     FIAR5000                           31
305700021217     C  n31              seton                                        77
305800021217     C   31              setoff                                       77
305900920219     C*
306000920207     C* SE BOLLA CON LA "G" SENZA GIACENZA MECCANIZZATA SEGNALO
306100931116     C     ARBFBC        IFEQ      'G'
306200070412     C                   EVAL      V1DFBC=' GIACENTE '
306300070403     C                   SETON                                        27
306400040217     C                   ENDIF
306500930415     C*
306600940907     C                   MOVEL     ARBAAS        V3CAAS
306700940719     C                   MOVE      ARBNSP        V3CNSP
306800940719     C                   MOVEL     ARBNRS        V3CNRS
306900070404     C                   MOVEL     ARBNRS        V4CNRS
307000131108     C********           MOVEL     ARBNRS        V7CNRS
307100940719     C                   MOVEL     ARBLNP        V3CLNP
307200950508     C                   MOVEL     ARBLNP        V3CLPA
307300131114     c
307400131114     c                   z-add     arbmgs        arbdsp
307500131114     c                   movel     arbaas        arbdsp
307600940908     C*
307700940908     C*  L E G A M I    B O L L A
307800940908     C  N75              EXSR      LEGBOL
307900940908     C*
308000911022     C* NON SEQUENZIALI
308100911023     C     ARBFNS        IFEQ      'S'
308200930705     C                   MOVEL     CNOSEQ        DESFNS
308300911022     C                   ELSE
308400911022     C                   MOVEL     *BLANKS       DESFNS
308500040217     C                   ENDIF
308600930525     C*
308700930525     C* 95 ON - ESITONO EVENTI DA INTERROGARE
308800940713     C                   Z-ADD     ARBAAS        KAAS$
308900940713     C                   Z-ADD     ARBLNP        KLNP
309000940713     C                   Z-ADD     ARBNRS        KNRS
309100940713     C                   Z-ADD     ARBNSP        KNSP
309200990805     C     KEVB1         SETLL     FNEVB01L                               95
309300920402     C*
309400920403     C* 98 ON - ESITONO VARIAZIONI FATTE
309500131007     C     KARB2         SETLL     FiARBF2C                               98
309600990119     C* 04 ON - ESISTONO CA DA VISUALIZZARE
309700990119     C                   CLEAR                   CONTCA
309800040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
309900040809      * legate alla spedizione che passo
310000040809     c                   clear                   fidn12ds
310100040809     c                   eval      i12aas = arbAAS
310200040809     c                   eval      i12lnp = arbLNP
310300040809     c                   eval      i12nrs = arbNRS
310400040809     c                   eval      i12nsp = arbNSP
310500040809     c                   eval      i12fel = 'E'
310600040809     c                   eval      i12fan = 'E'
310700040809     c*
310800040809     c                   call      'FIDN12R'
310900040809     c                   parm                    fidn12ds
311000040809     c*
311100040809     c                   setoff                                       31
311200040809     c*
311300040809     c* se non ci sono errori
311400040809     c                   if        o12err <> ' '
311500040809     c                   seton                                        31
311600040809     c                   else
311700040809     c* se numero di CA trovate maggiore di zero
311800040809     c                   if        o12nca = 0
311900040809     c                   seton                                        31
312000040809     c                   else
312100040809     c                   z-add     1             ii                2 0
312200040809     c                   dow       ii <= o12nca and *in31 = *off
312300040809     c                   movea     skkey(ii)     dskey
312400040809     C                   SETON                                        04
312500040809     C                   ADD       1             CONTCA            3 0
312600040809     C                   Z-ADD     dsaac         VHCAAC
312700040809     C                   Z-ADD     dsfil         VHCFIL
312800040809     C                   Z-ADD     dsnca         VHCNCA
312900040809     c*
313000040809     c                   add       1             ii
313100040809     c                   enddo
313200040809     c*
313300040809     c                   seton                                        31
313400040809     c*
313500040809     c                   endif
313600040809     c                   endif
313700040809     c**
313800070406     c* se Presente FIARP, imposto la cons.richista proposta
313900070404     c                   clear                   v4cdimpr
314000070404     c                   clear                   v4coimpr
314100070404     c                   clear                   v4cprupr
314200070416     c                   clear                   v08dcrpr
314300070404     c                   clear                   v4ctcrpr
314400070404     c                   clear                   v4chcrpr
314500070406     c
314600070406     c* Solo se visualizzabile
314700070406     c     karb2         chain     fiarp01l
314800070406    1c                   if        %found(FIarp01l)
314900070404     c                   seton                                        26
315000070404     C                   MOVEL     arppru        v4cprupr
315100070404     C                   MOVEL     arpoim        v4coimpr
315200070404    2c                   if        arpdim>0
315300070404     c     *iso          movel     arpdim        dataeur
315400070416     c                   movel     dataeur       datadmy
315500070416     c                   movel     datadmy       w0060             6 0
315600070416     c                   movel     w0060         v4cdimpr
315700070404    2c                   endif
315800070404    2c                   if        arpdcr>0
315900070404     c     *iso          movel     arpdcr        dataeur
316000070416     c                   movel     dataeur       v08dcrpr
316100070416     c**                 movel     datadmy       v06dcrpr
316200070404    2c                   endif
316300070404     c                   movel     arphcr        v4chcrpr
316400070404     c                   movel     arptcr        v4ctcrpr
316500070406     c                   if        arptcr=' '
316600070406     c                   eval      v4ctcrpr='IL'
316700070404    1c                   endif
316800070406    1c                   endif
316900070406     c
317000911022     C* TIPO BOLLA CONTROLLO SE FRANCO
317100940719     C                   MOVEL     ARBCBO        V3CCBO
317200911022     C                   MOVEL     '3A'          COD
317300911022     C                   MOVEL     *BLANKS       KEY
317400940712     C                   MOVEL     ARBCBO        KEY
317500940713     C     KTAB1         CHAIN     TABEL                              31
317600911022     C                   MOVEL     TBLUNI        DS3A
317700911022     C                   MOVEL     §3ATB1        FLGTB1            1
317800940719     C                   MOVEL     §3ADES        V3DCBO
317900940719     C                   MOVEL     §3ADES        V5DCBO
318000911030     C* FRANCO
318100040216if  1C     FLGTB1        IFEQ      'F'
318200911022     C                   SETON                                        33
318300940713     C                   MOVE      ARBKSC        V3CKSC
318400940712     C                   MOVE      *BLANKS       VIDKSC
318500040216x   1C                   ELSE
318600911030     C* ASSEGNATO
318700911030     C     ARBCCM        IFNE      0
318800940713     C                   MOVE      ARBCCM        V3CKSC
318900911030     C                   ELSE
319000940713     C                   MOVE      *ZEROS        V3CKSC
319100040217     C                   ENDIF
319200911030     C                   MOVEL     ARBKSC        VIDKSC
319300040217e   1C                   ENDIF
319400070410     c* Numero di telefono del mittente
319500070410     c                   clear                   v3ctelm
319600070410     c                   move      v3cksc        w0040
319700070410     c                   if        w0040<>0000 and w0040<>8888
319800070410     c                             and w0040<>9999
319900070410     c                   eval      i69kin = V3Cksc
320000070410     c                   exsr      CTR_ANAGRA
320100070410     c                   eval      V3ctelm= indtel
320200070410     c                   endif
320300991027     C* 38 ON - ESISTE TB2 ANCHE SENZA TIPO RCD "2" IN FIAR600F
320400920121     C     §3ATB2        COMP      '  '                               3838
320500040218      *
320600040218      *?Controllo se esitono reclami?
320700131111     c                   eval      wARBlnp = ARBlnp
320800131111     c                   eval      wARBnrs = ARBnrs
320900131111     c                   eval      wARBnsp = ARBnsp
321000131111     c                   eval      wARBaas = ARBaas
321100040218     c                   eval      TGDtor  = 'S'
321200040218     c                   eval      TGDogg  = ds_OggSped
321300040218     c     K02TGD02      setll     FITGD000
321400040302     c                   eval      *in64   = (%equal(FITGD02L)   and
321500040303     c                                           (SDSprm =  1
321600040303     c                                        or (SDSprm >  1
321700040303     c                                        and IA1ins <> 'N')))
321800940712     C*
321900940712     C* DECODIFICO  "BOLLA DI RIENTRO"
322000961111     C                   MOVEL     *BLANKS       VIDFBR
322100961111     C                   MOVEL     *BLANKS       VIDFFR
322200961111     C                   SETOFF                                       1073
322300940712     C     ARBFBR        IFEQ      'S'
322400961111     C                   MOVEL     'SI'          VIDFBR
322500961111     C                   SETON                                        10
322600040217     C                   ENDIF
322700961111     C     ARBFBR        IFEQ      'F'
322800961111     C                   MOVEL     'FR'          VIDFFR
322900961111     C                   SETON                                        73
323000040217     C                   ENDIF
323100970130     C*
323200970227     C* FLAG CONSEGNA DDT
323300970130     C                   MOVEL     ARBAAS        KAAS
323400970130     C                   MOVEL     ARBLNP        KLNP
323500970130     C                   MOVEL     ARBNRS        KNRS
323600970130     C                   MOVEL     ARBNSP        KNSP
323700970130     C                   MOVEL     'A'           KTRC
323800970131     C                   CLEAR                   V1DBAM
323900000615     C                   eval      v3csop = ' '
324000060216     C     KAR41         CHAIN     FiAR401L                           31
324100970227     C* C'E' IL RECORD
324200040216if  1C     *IN31         IFEQ      *OFF
324300000615     C                   movel     ar4not        dsbl4a
324400000615     C                   eval      v3csop = §b4aso
324500031027     c                   Move      §b4Abm        wbam              1
324600040216if  2C     WBAM          IFEQ      'S'
324700970131     C     WBAM          OREQ      'C'
324800031027     c     wbam          oreq      'P'
324900031027     c     wbam          oreq      'Q'
325000970227     C                   SETON                                        76
325100970227     C                   MOVEL     'SI DDT'      V1DBAM
325200040216x   2C                   ELSE
325300970227     C                   SETOFF                                       76
325400970227     C                   MOVEL     'NO DDT'      V1DBAM
325500040216e   2C                   ENDIF
325600970227     C**
325700040216sel 2C                   SELECT
325800970227     C     WBAM          WHENEQ    'C'
325900970227     C     WBAM          OREQ      'K'
326000970227     C                   MOVEL     'CLI'         V1DFST
326100970227     C     WBAM          WHENEQ    'J'
326200031027     C     WBAM          oreq      'P'
326300031027     C     WBAM          oreq      'Q'
326400031113     C     WBAM          OREQ      'W'
326500070406     C                   MOVEL     'Arr'         V1DFST
326600970227     C     WBAM          WHENEQ    'S'
326700970227     C     WBAM          OREQ      'Y'
326800070406     C                   MOVEL     'Par'         V1DFST
326900040216x   2C                   OTHER
327000970227     C                   MOVEL     'NO '         V1DFST
327100040216e   2C                   ENDSL
327200970227     C**
327300970227     C* NON  C'E' IL RECORD
327400040216x   1C                   ELSE
327500970227     C                   SETON                                        76
327600970227     C                   MOVEL     'SI DDT'      V1DBAM
327700970227     C                   MOVEL     'PAR'         V1DFST
327800040216e   1C                   ENDIF
327900010510     c* numero O.R.M.
328000010510     C                   MOVEL     'M'           KTRC
328100060216     c     kar41         chain     fiar401l                           31
328200010510     c     *in31         ifeq      *off
328300010510     c                   movel     ar4not        dsbl4m
328400010510     c                   z-add     §b4poe        v3cpoe
328500010510     c                   z-add     §b4nsr        v3cnsr
328600010510     c                   z-add     §b4nor        v3cnor
328700010510     c                   z-add     §b4nrv        v3cnrv
328800010510     c                   else
328900010510     c                   clear                   v3cpoe
329000010510     c                   clear                   v3cnsr
329100010510     c                   clear                   v3cnor
329200010510     c                   clear                   v3cnrv
329300010510     c                   endif
329400130320     c*//* Verifico se ha fiar8
329500130320     C*//                MOVEL     ARBAAS        KAAS
329600130320     C*//                MOVEL     ARBLNP        KLNP
329700130320     C*//                MOVEL     ARBNRS        KNRS
329800130320     C*//                MOVEL     ARBNSP        KNSP
329900130320     c*//  ksped         setll     fiar801l
330000130320     c*//                if        %equal(fiar801l)
330100130320     c*//                eval      v1dAR8='PACKING LIST'
330200130320     C*//                ELSE
330300130320     C*//                CLEAR                   V1DAR8
330400130320     c*//                endif
330500941219     C*
330600941219     C* DECODIFICO  "CONSEGNA A MAGAZZINO"
330700941219     C     ARBFDN        IFEQ      'S'
330800941219     C                   MOVEL     'SI'          V3CFDN
330900941219     C                   ELSE
331000941219     C                   MOVEL     *BLANKS       V3CFDN
331100941219     C                   ENDIF
331200911129     C*
331300920204     C* INCASSO C/A E ASSEGNATO
331400930113     C                   MOVEL     '  '          VIDICC
331500930113     C                   MOVEL     '  '          VIDICA
331600930113     C*
331700930113     C* SE ASSEGNATO O FRANCO CON SECONDA BOLLA
331800930113     C     *IN33         IFEQ      '0'
331900930113     C     *IN33         OREQ      '1'
332000930113     C     *IN38         ANDEQ     '1'
332100930113     C*
332200930330     C     ARBICA        IFNE      ' '
332300941216     C     ARBICA        ANDNE     'R'
332400920204     C                   MOVEL     'SI'          VIDICA
332500040217     C                   ENDIF
332600040217     C                   ENDIF
332700911129     C*
332800911030     C* LINEA PARTENZA DECODIFICA
332900020225     c                   clear                   og143
333000930706     C                   Z-ADD     ARBLNP        NUM3
333100930706     C     NUM3          CHAIN     AZORG01L                           31
333200940719     C  N31              MOVEL     ORGDES        V3DLNP
333300940719     C   31              MOVEL     *BLANKS       V3DLNP
333400020225     c                   eval      og143 = orgde3
333500020225     c                   eval      lnpntw = §ogntw
333600911030     C* LINEA ARRIVO   DECODIFICA
333700991217     c                   clear                   og143
333800070403     C     arblna        CHAIN     AZORG01L                           31
333900940719     C  N31              MOVEL     ORGDES        V3DLNA
334000940719     C   31              MOVEL     *BLANKS       V3DLNA
334100070403     c                   movel     v3dlna        v7dlna
334200991217     C                   movel     orgde3        og143
334300070403     C                   movel     orgde8        og148
334400020304     c                   eval      lnantw = §ogntw
334500070426     c* p.o. gestore di arblna
334600070426     c                   clear                   fnlv55ds
334700070426     c                   movel     arblna        d55lin
334800070426     c                   movel     '6'           d55tpt
334900070426     C                   time                    wrkoggiiso
335000070426     C     *iso          move      wrkoggiiso    d55drf
335100070426     c                   call      'FNLV55R'
335200070426     c                   parm                    fnlv55ds
335300070426     c
335400070426     c                   if        d55tfa<>arblna
335500070426     C     d55tfa        CHAIN     AZORG01L                           31
335600070426     C  n31              movel     orgde8        og148
335700070426     C   31              clear                   og148
335800070426     c                   endif
335900070426     c
336000070403     c
336100070403     c* Abilito visualizzazione nuove funzioni se GEO attiva
336200070426     c* Per p.o. gestione linea di arrivo e p.o. utente
336300070403     c                   if        §oggeot='S' and dutgeot='S'
336400070524     c                   seton                                        1788
336500070406     c                   else
336600070406     c* non visualizzo nemmeno fiarp
336700070406     c                   setoff                                       26
336800070403     c                   endif
336900070524     c
337000070524     c* Vedo se almeno è attivo lo scambio dati con tellus
337100070524     c                   if        not *in88 and §ogtlls<>' ' and dutgeot='S'
337200070524     c                   seton                                        88
337300070524     c                   endif
337400070404     c
337500070404     c* Emetto riga sintetica altre abilitazioni
337600070521     c                   clear                   v5dngd
337700140109     c***                if        *in17
337800070404     c                   clear                   v3cges
337900070404     C                   MOVEL     *BLANKS       V3cgesd
338000070404     c*
338100131120     c                   clear                   fnlrg7ds
338200131120     c                   eval      ilrg7aas=arbaas
338300131120     c                   eval      ilrg7lnp=arblnp
338400131120     c                   eval      ilrg7nrs=arbnrs
338500131120     c                   eval      ilrg7nsp=arbnsp
338600131120     c                   eval      ilrg7lung=58
338700131120     c                   call      'FNLRG7R'
338800131120     c                   parm                    fnlrg7ds
338900131120     c                   movel     olrg7vssin    v3cges
339000140109     c***                endif
339100070410     c
339200070524     c* Reperisco dati della distinta automatica tramite FIDG34R
339300070524     c                   if        *in88
339400070410     c                   clear                   v5ccgi
339500070411     c                   clear                   v5dcgi
339600070410     c                   clear                   v5dtgi
339700070410     c                   clear                   v5dttl
339800070411     c                   clear                   v5clno
339900070427     c                   clear                   v5dlno
340000070427     c                   clear                   v5cargerr
340100070427     c                   clear                   v5dargerr
340200070427     c
340300070427     c                   clear                   fidg34ds
340400070427     c                   eval      id34tip='2'
340500070427     c                   eval      id34aas=arbaas
340600070427     c                   eval      id34lnp=arblnp
340700070427     c                   eval      id34nrs=arbnrs
340800070427     c                   eval      id34nsp=arbnsp
340900070427     c                   call      'FIDG34R'
341000070427     c                   parm                    FIDG34DS
341100070410     c
341200070427     c                   movel     od34cgi       v5ccgi
341300070410    3c                   select
341400070427     c                   when      od34tgi='A'
341500070410     c                   eval      v5dtgi='Automatico'
341600070427     c                   when      od34tgi='M'
341700070410     c                   eval      v5dtgi='Manuale   '
341800070410    3c                   ENDSL
341900070411     c* Decodifica giro
342000070427     c                   eval      v5dcgi=od34cgides
342100070427     c* esito/errore
342200070427     c                   movel     od34err       v5cargerr
342300070427     c                   eval      v5dargerr=od34errdes
342400070427     c* livello normalizzazione
342500070427     c                   movel     od34lno       v5clno
342600070427     c                   eval      v5dlno=od34lnodes
342700070427     c* elaborazione tellus
342800070427     c***                eval      v5dttl=od34ttldes
342900070427     c                   select
343000070427     c                   when      od34ttl='1'
343100070427     c                   eval      v5dttl='SI- CON Giro'
343200070427     c                   when      od34ttl='2'
343300070427     c                   eval      v5dttl='SI-SenzaGiro'
343400070427     c                   endsl
343500070521     c*
343600070521     c* Decodifico se assegnazione automatica o manuale
343700070521     c                   if        arbndc>0 and arbngd<>0
343800070521     c                   eval      v5dngd='MANUALE'
343900070521     c                   endif
344000070521     c                   if        arbndc>0 and arbngd=0
344100070521     c                   eval      v5dngd='AUTOMAT'
344200070521     c                   endif
344300070410     c
344400070410    1c                   endif
344500991217     c
344600060518     c* LNA DPD o lnp dpd: leggo record "I" di ar4
344700000216     c                   setoff                                       08
344800000216     c                   clear                   v3czon
344900000216     c                   clear                   v3crtn
345000040216if  1c                   if        lnantw = 'DPD'
345100991217     c                   seton                                        85
345200991217     C                   MOVEL     ARBAAS        KAAS
345300991217     C                   MOVEL     ARBLNP        KLNP
345400991217     C                   MOVEL     ARBNRS        KNRS
345500991217     C                   MOVEL     ARBNSP        KNSP
345600060518     C                   MOVEL     'I'           KTRC
345700060216     C     KAR41         CHAIN     FiAR401L                           31
345800060518     C  N31              MOVEL     AR4NOT        dsbl4i
345900060518     c   31              clear                   dsbl4i
346000060518     C                   MOVEL(P)  §b4ipn        V3CRPP
346100060518     c                   if        §b4ida<>*blanks or §b4iza>*zeros
346200060518     c                   movel     §b4ida        v3crtn
346300060518     c                   movel     §b4iza        v3czon
346400000216     c                   seton                                        08
346500000216     c                   endif
346600050304     c                   else
346700050304if  1c                   if        lnpntw = 'DPD'
346800050304     C                   MOVEL     ARBAAS        KAAS
346900050304     C                   MOVEL     ARBLNP        KLNP
347000050304     C                   MOVEL     ARBNRS        KNRS
347100050304     C                   MOVEL     ARBNSP        KNSP
347200060518     C                   MOVEL     'I'           KTRC
347300060518     c                   clear                   dsbl4i
347400060216     C     KAR41         CHAIN     FiAR401L
347500060216     c                   if        %found(fiar401l)
347600060518     C**                 MOVEL     AR4NOT        dsbl4f
347700060518     C**                 MOVEL     §b4fnp        V3CRPP
347800060518     C                   MOVEL     AR4NOT        dsbl4i
347900060518     C                   MOVEL     §b4ipn        V3CRPP
348000050304     c                   seton                                        85
348100050304     c                   endif
348200050304     c                   endif
348300040216e   1c                   endif
348400971219     C*
348500971219     C* 16 ON  - SI TRATTA DI BOLLA EXPORT
348600020225     c                   if        lnantw = 'EEX' or lnantw = 'EUP' or
348700060201     c                             lnantw = 'FED' or lnantw='DPD'
348800971219     C                   SETON                                        16
348900971219    1C                   ENDIF
349000940719     C                   MOVEL     V3DLNA        D15LNA
349100920804     C*
349200940719     C                   MOVEL     ARBLNA        V3CLNA
349300940719     C                   MOVEL     ARBRSM        V3CRSM
349400070410     C                   MOVEL     ARBRSM        V4CRSM
349500940713     C                   MOVEL     ARBRSM        V28RSM
349600940719     C                   MOVEL     ARBINM        V3CINM
349700940719     C                   MOVEL     ARBPRM        V3CPRM
349800940719     C                   MOVEL     ARBCAM        V3CCAM
349900940719     C                   MOVEL     ARBLOM        V3CLOM
350000940713     C                   MOVEL     ARBLOM        V25LOM
350100940719     C                   MOVEL     ARBFAP        V3CFAP
350200950421     C                   MOVEL     ARBNZM        V3CNZM
350300131108     c                   if        arbnzm<>*blaNKS
350400131108     C                   MOVEL     arbnzm        v7cprm
350500131108     c                   else
350600131108     C                   MOVEL(P)  arbprm        v7cprm
350700131108     c                   endif
350800131108     c
350900050113     C                   MOVEL     ARBXCO        VIDXCO
351000130221     c                   if        vidxco >*zeros  and no_f22=*blanks
351100110928     c                   seton                                        86
351200110928     c                   endif
351300940719     C                   MOVEL     *BLANKS       V3CPIM
351400940719     C                   MOVEL     *BLANKS       V4CPID
351500061108     c                   clear                   v3cdfm
351600061108     c                   clear                   v4cdfd
351700950411     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
351800060216     C* DA FiAR4 SE RECORD ESISTENTE
351900950411     C                   MOVEL     ARBAAS        KAAS
352000950411     C                   MOVEL     ARBLNP        KLNP
352100950411     C                   MOVEL     ARBNRS        KNRS
352200950411     C                   MOVEL     ARBNSP        KNSP
352300950411     C                   MOVEL     'P'           KTRC
352400060216     C     KAR41         CHAIN     FiAR401L                           31
352500040216if  1C     *IN31         IFEQ      *OFF
352600950411     C                   MOVEL     AR4NOT        V3CPIM
352700950411     C                   MOVE      AR4NOT        V4CPID
352800040216x   1C                   ELSE
352900950411     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI ARBCPI
353000040216if  2C     ARBCPI        IFNE      *BLANKS
353100950411     C  N33              MOVEL     ARBCPI        V4CPID
353200950411     C   33§3ATB2        IFEQ      '  '
353300950411     C                   MOVEL     ARBCPI        V3CPIM
353400950411     C                   ELSE
353500950411     C                   MOVEL     ARBCPI        V4CPID
353600040217     C                   ENDIF
353700040217e   2C                   ENDIF
353800040217e   1C                   ENDIF
353900061108      * valorizzo il codice fiscale del mittente e del destinatario
354000061108      * FIAR4 rcd Q
354100061108     c                   eval      ktrc = 'Q'
354200061108     c     kar41         chain     fiar401l
354300061108if  1c                   if        %found(fiar401l)
354400061108     c                   movel     ar4not        v3cdfm
354500061108     c                   move      ar4not        v4cdfd
354600061108x   1c                   endif
354700980704     C* VISUALIZZO SE C'E' IL RIFERIM PARTNER
354800971219     C                   CLEAR                   V3CEST
354900971219     C                   MOVEL     ARBAAS        KAAS
355000971219     C                   MOVEL     ARBLNP        KLNP
355100971219     C                   MOVEL     ARBNRS        KNRS
355200971219     C                   MOVEL     ARBNSP        KNSP
355300971219     C                   MOVEL     'E'           KTRC
355400060216     C     KAR41         CHAIN     FiAR401L                           31
355500980704     C* VISUALIZZO IN UNA BOLLA ITALIA SE C'E'
355600980704     C* VISUALIZZO SEMPRE IN UNA BOLLA IMPORT/EXPORT
355700971219     C  N31              MOVEL     AR4NOT        V3CRPP
355800980704     C  N31              SETON                                        85
355900980704     C**
356000020225     c                   if        *in31 and
356100020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
356200020225     c                              lnpntw = 'FED') or
356300020225     c                             (lnantw = 'EEX' or lnantw = 'EUP' or
356400020225     c                              lnantw = 'FED')
356500980704     C                   SETON                                        85
356600980704     C                   ENDIF
356700980704     C**
356800940719     C                   MOVEL     ARBRSD        V4CRSD
356900131108     C**********         MOVEL     ARBRSD        V30RSD
357000940713     C                   MOVEL     ARBRSD        V32RSD
357100940719     C                   MOVEL     ARBIND        V4CIND
357200131119     C                   MOVEL     ARBIND        V33IND
357300940719     C                   MOVEL     ARBPRD        V4CPRD
357400940719     C                   MOVEL     ARBCAD        V4CCAD
357500940719     C                   MOVEL     ARBLOD        V4CLOD
357600131119     C*****              MOVEL     ARBLOD        V24LOD
357700940719     C                   MOVEL     ARBFIN        V4CFIN
357800940719     C                   MOVEL     ARBNZD        V4CNZD
357900911022     C                   MOVEL     ARBGC1        VIDGC1
358000911022     C                   MOVEL     ARBGC2        VIDGC2
358100911022     C                   MOVEL     ARBCTR        VIDCTR
358200940719     C                   MOVEL     ARBCTS        V6CCTS
358300131108     c                   if        arbnzd<>*blaNKS
358400131108     C                   MOVEL     arbnzd        v7cprd
358500131108     c                   else
358600131108     C                   MOVEL(P)  arbprd        v7cprd
358700131108     c                   endif
358800940719     C* DECODIFICA CODICE TASSAZIONE
358900940719     C                   MOVEL     'CT'          COD
359000940719     C                   MOVEL     *BLANKS       KEY
359100940719     C                   MOVEL     ARBCTS        KEY
359200940719     C     KTAB1         CHAIN     TABEL00F                           31
359300940719     C                   MOVEL     TBLUNI        V6DCTS
359400911022     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
359500911022     C                   MOVEL     'TR'          COD
359600911022     C                   MOVEL     *BLANKS       KEY
359700911022     C                   MOVEL     VIDCTR        UNOCTR            1
359800911022     C                   MOVEL     UNOCTR        KEY
359900940713     C     KTAB1         CHAIN     TABEL                              31
360000911022     C                   MOVEL     TBLUNI        DEFCTR
360100070404     C*****              MOVEL     ARBFTM        V3CFTM
360200070404     c                   if        arbftm='S'
360300070404     c                   eval      v3dftm='TASSATA CON TARIFFA MITTENTE'
360400070404     c                   endif
360500940711     C* DECODIFICA TIPO SERVIZIO BOLLE
360600090604     c                   clear                   ds5e
360700940711     C                   MOVEL     '5E'          COD
360800940711     C                   MOVEL     *BLANKS       KEY
360900940713     C                   MOVEL     ARBTSP        KEY
361000941024     C     KTAB1         CHAIN     TABEL00F                           31
361100090604     C  N31              MOVEL     TBLUNI        ds5e
361200090604     C  N31              MOVEL     §5ed08        V1DTSP
361300940719     C   31              MOVEL     *BLANKS       V1DTSP
361400131127     c                   if        §5eftc<>' '
361500131127     c                   movel     ri            atrtsp
361600131127     c                   else
361700131127     c                   movel     hi            atrtsp
361800131127     c                   endif
361900940713     C                   MOVEL     ARBVMD        VIDVMD
362000940713     C                   MOVEL     ARBVAD        VIDVAD
362100940713     C                   MOVEL     ARBIAS        VIDIAS
362200940713     C                   MOVEL     ARBVAS        VIDVAS
362300911022     C                   MOVEL     ARBNAS        VIDNAS
362400950214     C                   CLEAR                   VIDFLS
362500950214     C     ARBNCD        IFNE      *ZERO
362600950214     C     ARBFNS        ORNE      *BLANKS
362700940923     C                   MOVEL     ARBFLS        VIDFLS
362800950214     C                   ENDIF
362900950214     C                   Z-ADD     ARBFLS        V3CFLS
363000911022     C                   MOVEL     ARBNCL        VIDNCL
363100070410     C                   MOVEL     ARBPKB        VIDPKb
363200070410     C                   MOVEL     ARBPKF        VIDPKF
363300070410     C                   MOVEL     ARBVLb        VIDVLb
363400070410     C                   MOVEL     ARBFVb        VIDFVb
363500070410     C                   MOVEL     ARBVLF        VIDVLF
363600070410     C                   MOVEL     ARBFVF        VIDFVF
363700930705     C     ARBNCR        IFLT      99998
363800940711     C                   MOVEL     ARBVLC        VIDVLC
363900940711     C                   MOVEL     ARBNCR        VIDNCR
364000921231     C                   ELSE
364100990909     C                   Z-ADD     0             VIDVLC
364200990909     C                   Z-ADD     0             VIDNCR
364300921231     C                   END
364400990909     C     ARBNCP        IFLT      99998
364500990909     C                   MOVEL     ARBPKC        VIDPKC
364600990909     C                   MOVEL     ARBNCP        VIDNCP
364700990909     C                   ELSE
364800990909     C                   Z-ADD     0             VIDPKC
364900990909     C                   Z-ADD     0             VIDNCP
365000990909     C                   END
365100940711     C                   MOVEL     ARBQFT        VIDQFT
365200031119
365300031119      * Referente e Telefono del destinatario
365400031119     c                   Clear                   V4cRef
365500031119     c                   Clear                   V4cTeld
365600091214     c                   clear                   w_cdi
365700031119      * li prendo da Fiar500f record 'GEN'
365800130320     c                   clear                   v1dAR8l
365900130320     c                   clear                   v1dAR8
366000031119     c                   Clear                   dAr5Gen
366100031119     c                   Z-Add     ArbAas        Kaas
366200031119     c                   Z-Add     ArbLNp        Klnp
366300031119     c                   Z-Add     ArbNrs        Knrs
366400031119     c                   Z-Add     ArbNsp        Knsp
366500031119     c                   Eval      Ktrd = 'GEN'
366600031119     c     kAr51         Chain     Fiar501l
366700031119If  1c                   If        %Found(Fiar501l)
366800031119     c                   Movel     Ar5Uni        Dar5Gen
366900031119     c                   Eval      V4cRef = §Ar5Ref
367000031119     c                   Eval      V4cTeld = §Ar5Teld
367100091214     c* memorizzo eventuale codice beneficiario assegno
367200091214     c                   eval      w_cdi=§ar5cdi
367300130320     c* verifico se LDV con packing-list e/o PDF
367400130320sel 2c                   select
367500130320w   2c                   when      §AR5als = 'S'
367600130320     c                   eval      v1dAR8l = 'PACKING LIST PDF'
367700130320     c                   eval      v1dAR8  = 'PACKING PDF '
367800130320w   2c                   when      §AR5alx = 'S'
367900130320     c                   eval      v1dAR8l = 'PACKING LIST'
368000130320     c                   eval      v1dAR8  = 'PACKING LIST'
368100130320e   2c                   endsl
368200031119    1c                   EndIf
368300040301
368400040301      * Se colli di valore visualizzo "V" nell'apposito campo
368500040301      *  con lo specifico attributo di visualizzazione
368600040301if  1c                   if        §AR5bva = *blanks
368700040301     c                   eval      VIDbva  = *blanks
368800040301x   1c                   else
368900040301     c                   eval      VIDbva  = 'V'
369000040301e   1c                   endif
369100110601
369200110610     c* email destinatario
369300110601     c                   setoff                                       60
369400110601     c                   clear                   v4cemw
369500110610     c                   clear                   v4cemd
369600130927     c                   Clear                   dAr5emd
369700110601      * li prendo da Fiar500f record 'EMD'
369800110601     c                   Z-Add     ArbAas        Kaas
369900110601     c                   Z-Add     ArbLNp        Klnp
370000110601     c                   Z-Add     ArbNrs        Knrs
370100110601     c                   Z-Add     ArbNsp        Knsp
370200110601     c                   Eval      Ktrd = 'EMD'
370300130927     c***  kAr51         setll     Fiar501l
370400130927     c     kAr51         chain     Fiar501l
370500130927If  1c                   If        %found(Fiar501l)
370600130927     c                   eval      dar5emd=ar5uni
370700130927     c                   if        §ar5tel<>*blanks or §ar5eml<>*blanks
370800110610     c                   Eval      V4cemd = 'SI'
370900110601     c                   seton                                        60
371000130927     c                   endif
371100110601    1c                   EndIf
371200040301
371300030121      * Bancali
371400030121     c                   Clear                   VidTba
371500030121     c                   Clear                   VidTb2
371600030122     c                   Clear                   VidNba
371700030122     c                   Clear                   VidNb2
371800030703      * Record 'BAN'
371900030703If  0c                   If        %subst(ArbGva:1:1) = 'B'
372000030121     c                   Clear                   dAr5Ban
372100030121     c                   Eval      Ktrd = 'BAN'
372200030121     c     kAr51         Chain     Fiar501l
372300030121If  1c                   If        %Found(Fiar501l)
372400030121     c                   Movel     Ar5Uni        Dar5Ban
372500030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
372600030122      * per le note relative al mancato reso dei bancali
372700030122     c                   If        Not *In77 and
372800030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
372900030122     c                   Eval      *In77 = *On
373000030122     c                   EndIf
373100030121     c                   Eval      VidNba = §Ar5Nba
373200030121     c                   Eval      VidNb2 = §Ar5Nb2
373300030121If  2c                   If        §Ar5Tba <> *Zeros
373400030703     c                   Eval      wtba = §Ar5Tba
373500030703     c                   ExSr      Sr_DecTba
373600030703     c                   Eval      VidTba = wdestba
373700030121    2c                   EndIf
373800030121If  2c                   If        §Ar5Tb2 <> *Zeros
373900030703     c                   Eval      wtba = §Ar5Tb2
374000030703     c                   ExSr      Sr_DecTba
374100030703     c                   Eval      VidTb2 = wdestba
374200030121    2c                   EndIf
374300030121    1c                   EndIf
374400030703    0c                   EndIf
374500030703      * Record 'BNB'  -  Spedizione OVER 25 colli
374600030703If  0c                   If        %subst(ArbGva:1:1) = 'O'
374700030703     c                   Clear                   dAr5Bnb
374800030703     c                   Eval      Ktrd = 'BNB'
374900030703     c     kAr51         Chain     Fiar501l
375000030703If  1c                   If        %Found(Fiar501l)
375100030703     c                   Movel     Ar5Uni        Dar5Bnb
375200030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
375300030703      * per i colli originali in caso di spedizione over 25 colli
375400030703     c  n77              Eval      *In77 = *On
375500030703     c                   Eval      VidNba = §Ar5bNba
375600030703     c                   Eval      VidNb2 = §Ar5bNb2
375700030703If  2c                   If        §Ar5bTba <> *Zeros
375800030703     c                   Eval      wtba = §Ar5bTba
375900030703     c                   ExSr      Sr_DecTba
376000030703     c                   Eval      VidTba = wdestba
376100030703    2c                   EndIf
376200030703If  2c                   If        §Ar5bTb2 <> *Zeros
376300030703     c                   Eval      wtba = §Ar5bTb2
376400030703     c                   ExSr      Sr_DecTba
376500030703     c                   Eval      VidTb2 = wdestba
376600030703    2c                   EndIf
376700030703    1c                   EndIf
376800030703    0c                   EndIf
376900911129     C*
377000131120     C**   ARBACC        IFEQ      'S'
377100131120     C**                 MOVEL     'SI'          V5CACC
377200131120     C**                 ELSE
377300131120     C**                 MOVEL     'NO'          V5CACC
377400131120     C**                 END
377500941215     C     ARBICC        IFNE      ' '
377600941216     C     ARBICC        ANDNE     'R'
377700930218     C                   MOVEL     'SI'          VIDICC
377800930218     C                   END
377900911022     C*
378000911022     C                   MOVEL     ARBRMN        VIDRMN
378100940712     C                   MOVEL     ARBRMA        VIDRMA
378200911022     C                   MOVEL     ARBRMO        VIDRMO
378300911022     C                   MOVEL     ARBNCD        VIDNCD
378400911022     C                   MOVEL     ARBNCA        VIDNCA
378500911022     C                   MOVEL     ARBZNC        VIDZNC
378600051104     c                   select
378700051104     c                   when      arbffd='S'
378800051104     C                   MOVEL     'SI '         VIDFFD
378900051104     c                   when      arbffd='M'
379000051104     C                   MOVEL     'SAB'         VIDFFD
379100051104     C                   other
379200051104     C                   MOVEL     '   '         VIDFFD
379300051104     C                   ENDsl
379400051124     c
379500940719     C                   MOVEL     ARBNFV        V3CNFV
379600051124     c* richiamp pgm per decodifica
379700051124     C                   MOVEL     *BLANKS       VIDTCR
379800051124     c                   clear                   vidhcr
379900051124     c                   clear                   viddcr
380000051124     c                   if        arbdcr>0 or arbhcr>0
380100051124     c                   clear                   fnlv80ds
380200051124     c                   eval      ilv80aas=arbaas
380300051124     c                   eval      ilv80lnp=arblnp
380400051124     c                   eval      ilv80nrs=arbnrs
380500051124     c                   eval      ilv80nsp=arbnsp
380600051124     c                   eval      ilv80mgs=arbmgs
380700051124     c                   eval      ilv80tc1=arbtc1
380800051124     c                   eval      ilv80tc2=arbtc2
380900051124     c                   eval      ilv80dcr=arbdcr
381000051124     c                   eval      ilv80hcr=arbhcr
381100051124     c                   eval      ilv80tcr=arbtcr
381200051124     c                   call      'FNLV80R'
381300051124     c                   parm                    kpjba
381400051124     c                   parm                    fnlv80ds
381500051124     c
381600051124     c                   eval      viddcr=olv80dcr8
381700051124     c                   eval      vidtcr=olv80tcrd
381800911023     C                   MOVEL     ARBHCR        VIDHCR
381900051124     c                   endif
382000051124     c
382100070521     c* Se presente ora consegna, la edito
382200070413     c                   clear                   vidhmc
382300070413     c                   if        arbhmc>0
382400070413     c                   eval      vidhmc=%editw(arbhmc: '0 :  ')
382500070413     C****               MOVE      ARBHMC        VIDHMC
382600070413     c                   endif
382700911023     C                   MOVEL     ARBNTC        VIDNTC
382800911023     C                   MOVEL     ARBTMC        VIDTMC
382900940923     C*
383000940923     C* DECODIFICO IL TIPO MANCATA CONSEGNA
383100940923     C                   MOVEL     *BLANKS       V5DTMC
383200940923     C     ARBTMC        IFNE      *BLANKS
383300940923     C                   MOVEL     '2A'          COD
383400940923     C                   MOVEL     *BLANKS       KEY
383500940923     C                   MOVEL     ARBTMC        KEY
383600940923     C     KTAB1         CHAIN     TABEL00F                           31
383700940923     C  N31              MOVEL     TBLUNI        V5DTMC
383800940923     C                   ENDIF
383900940923     C*
384000911023     C                   MOVEL     ARBMIF        VIDMIF
384100911023     C                   MOVEL     ARBNMI        VIDNMI
384200940923     C*
384300940713     C                   MOVEL     ARBGGA        V4CGGA
384400940923     C                   MOVEL     *BLANKS       V4IGGA
384500940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
384600940923     C     V4CGGA        COMP      *BLANKS                            4242
384700940923     C*
384800940713     C                   MOVEL     ARBGMA        V4CGMA
384900940923     C                   MOVEL     *BLANKS       V4IGMA
385000940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
385100940923     C     V4CGMA        COMP      *BLANKS                            4343
385200940923     C*
385300960208     C                   MOVEL     ARBGVA        V4CGVA
385400960208     C                   MOVE      ' '           V4CGVA
385500940923     C                   MOVEL     *BLANKS       V4IGVA
385600940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
385700940923     C     V4CGVA        COMP      *BLANKS                            4444
385800940923     C*
385900070403     C* FLAG blocco CONSEGNA
386000070403     C                   SELECT
386100070403     c                   WHEN      arbfbc='A'
386200070403     c                   eval      v1dfbc='LascAVVISO'
386300070403     C                   SETON                                        27
386400070403     c                   WHEN      arbfbc='B'
386500070403     c  n17              eval      v1dfbc=' BLOCCATA '
386600070412     c   17              eval      v1dfbc=' RIMPALLO '
386700070412     C                   SETON                                        27
386800070403     c                   WHEN      arbfbc='*'
386900070403     c  n17              eval      v1dfbc=' BLOCCATA '
387000070403     c   17              eval      v1dfbc=' GIACENTE '
387100070403     C                   SETON                                        27
387200070403     C                   ENDSL
387300070403     c
387400070403     C**   ARBFBC        COMP      'A'                                    27
387500070403     C**   ARBFBC        COMP      'B'                                    17
387600070403     C**N17ARBFBC        COMP      '*'                                    17
387700940711     C* TIPO CONSEGNA PARTICOLARE 1
387800940804     C                   MOVEL     *BLANKS       V5DTC1
387900940711     C     ARBTC1        IFNE      ' '
388000911115     C                   MOVEL     '1P'          COD
388100911115     C                   MOVEL     *BLANKS       KEY
388200940711     C                   MOVEL     ARBTC1        KEY
388300940713     C     KTAB1         CHAIN     TABEL                              31
388400940804     C  N31              MOVEL     TBLUNI        V5DTC1
388500940804     C   31              MOVEL     *BLANKS       V5DTC1
388600930113     C                   END
388700940711     C* TIPO CONSEGNA PARTICOLARE 2
388800940804     C                   MOVEL     *BLANKS       V5DTC2
388900940804     C                   MOVEL     *BLANKS       V7DTC2
389000940711     C     ARBTC2        IFNE      ' '
389100940711     C                   MOVEL     '1P'          COD
389200940711     C                   MOVEL     *BLANKS       KEY
389300940711     C                   MOVEL     ARBTC2        KEY
389400940713     C     KTAB1         CHAIN     TABEL                              31
389500940804     C  N31              MOVEL     TBLUNI        V5DTC2
389600940804     C   31              MOVEL     *BLANKS       V5DTC2
389700940804     C  N31              MOVEL     TBLUNI        V7DTC2
389800940804     C   31              MOVEL     *BLANKS       V7DTC2
389900940711     C                   END
390000930113     C* TIPO MANCATA CONSEGNA
390100930113     C                   MOVEL     *BLANKS       DESCCA
390200930113     C     ARBCCA        IFNE      ' '
390300930705     C                   Z-ADD     1             C
390400930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
390500930705     C   31              MOVEL     DCA(C)        ALF12            12
390600930705     C  N31              MOVEL     *BLANKS       ALF12
390700930113     C                   MOVEL     '-'           DESCCA
390800930113     C                   MOVE      ALF12         DESCCA
390900930113     C                   END
391000911022     C* GIRO DATA SPEDIZIONE
391100940712     C                   Z-ADD     ARBDSP        G02INV
391200940712     C                   Z-ADD     *ZEROS        G02DAT
391300940712     C                   MOVEL     '3'           G02ERR
391400940712     C                   CALL      'XSRDA8'
391500940712     C                   PARM                    WLBDAT
391600940719     C                   Z-ADD     G02DAT        V3CDSP
391700070403     c*
391800911022     C* DATA BORDERO'
391900070403     c                   if        arbdbr>0
392000070403     c     *iso          movel     arbdbr        dataeur
392100070403     c                   movel     dataeur       datadmy
392200070404     c                   movel     dataeur       w08dbr            8 0
392300070404     c                   movel     datadmy       v06dbr
392400070403     c                   else
392500070404     c                   clear                   v06dbr
392600070403     c                   endif
392700070404     c
392800070404     c* Prendo da fnblp i dati ritiro merce
392900070403     c     karb2         chain     fnblp01l
393000070403     c                   if        %found(fnblp01l) and blpdrt>0
393100070403     c     *iso          movel     blpdrt        dataeur
393200070403     c                   movel     dataeur       datadmy
393300070404     c                   movel     datadmy       v06drt
393400070404     c                   movel     blpnrt        v7cnrt
393500070406     c                   movel     blpctm        v3cctm
393600070404     c                   if        blpfpp='M'
393700070410     c                   eval      v7dfppr='MAT'
393800070404     c                   else
393900070410     c                   eval      v7dfppr='POM'
394000070404     c                   endif
394100070404     C                   MOVEL     BLPPDR        V7CPDR
394200070404     C                   movel     BlpPdr        kpdpdr
394300070404     C                   move      'A'           kpdtip
394400070404     C     kfapd         chain     fiapd01L
394500070404     c                   if        %found(fiapd01l)
394600070404     C                   MOVEL     APDRSC        V7DPDR
394700070404     c                   else
394800070404     C                   MOVEL     *BLANKS       V7DPDR
394900070404     c                   endif
395000070404     c
395100070404     c
395200070403     c                   else
395300070404     c                   clear                   v06drt
395400070404     c                   clear                   v7cnrt
395500070406     c                   clear                   v3cctm
395600070504     c                   clear                   v7dfppr
395700070404     c                   clear                   v7dpdr
395800070404     c                   clear                   v7cpdr
395900070403     c                   endif
396000070403     c
396100961112     C* SOLO PER LE BOLLE EXPORT VISUALIZZO LA DATA STAMPA MANIFEST
396200070404     C                   CLEAR                   V06DT3
396300070413     c                   if        *in16 and arbdt3>0  and arbdt3<>99999999
396400070404     c     *iso          movel     arbdt3        dataeur
396500070404     c                   movel     dataeur       datadmy
396600070404     c                   movel     datadmy       v06dt3
396700961112     C                   ENDIF
396800940713     C* DATA PARTENZA PRIMO COLLO
396900070404     c                   if        arbdpc>0
397000070404     c     *iso          movel     arbdpc        dataeur
397100070404     c                   movel     dataeur       datadmy
397200070404     c                   movel     datadmy       v06dpc
397300070404     c                   movel     dataeur       w08dpc
397400070404     c                   else
397500070404     c                   clear                   v06dpc
397600070404     c                   clear                   w08dpc            8 0
397700070404     c                   endif
397800940713     C* DATA PARTENZA ULTIMO COLLO
397900070403     C**                 Z-ADD     ARBDUC        G02INV
398000070403     C**                 Z-ADD     *ZEROS        G02DAT
398100070403     C**                 MOVEL     '3'           G02ERR
398200070403     C**                 CALL      'XSRDA8'
398300070403     C**                 PARM                    WLBDAT
398400070403     C**                 Z-ADD     G02DAT        V3CDUC
398500070403     c                   if        arbduc>0
398600070404     c     *iso          movel     arbduc        dataeur
398700070403     c                   movel     dataeur       datadmy
398800070404     c                   movel     datadmy       v06duc
398900070404     c                   movel     dataeur       w08duc
399000070403     c                   else
399100070404     c                   clear                   v06duc
399200070404     c                   clear                   w08duc            8 0
399300070403     c                   endif
399400911022     C* DATA ENTRATA AL TRANSITO
399500070404     c                   if        arbdet>0
399600070404     c     *iso          movel     arbdet        dataeur
399700070404     c                   movel     dataeur       datadmy
399800070404     c                   movel     datadmy       v06det
399900070404     c                   else
400000070404     c                   clear                   v06det
400100070404     c                   endif
400200911022     C* DATA USCITA DAL TRANSITO
400300070404     c                   if        arbdut>0
400400070404     c     *iso          movel     arbdut        dataeur
400500070404     c                   movel     dataeur       datadmy
400600070404     c                   movel     datadmy       v06dut
400700070404     c                   else
400800070404     c                   clear                   v06dut
400900070404     c                   endif
401000930113     C* DATA ARRIVO MERCE
401100070403     c                   if        arbdam>0
401200070403     c     *iso          movel     arbdam        dataeur
401300070403     c                   movel     dataeur       datadmy
401400070404     c                   movel     datadmy       v06dam
401500070403     c                   else
401600070404     c                   clear                   v06dam
401700070403     c                   endif
401800131121
401900930504     C* DATA CONSEGNA MERCE REALE
402000070404     c                   if        arbdcm>0
402100070413     C                   SETON                                        69
402200070404     c     *iso          movel     arbdcm        dataeur
402300070404     c                   movel     dataeur       datadmy
402400070404     c                   movel     dataeur       v08dcm
402500070404     c                   movel     datadmy       v06dcm
402600070404     c                   else
402700070404     c                   clear                   v06dcm
402800070404     c                   clear                   v08dcm
402900070404     c                   endif
403000131121
403100930505     C* DATA CONSEGNA PREVISTA ESTERNA/ORE DI RITARDO
403200070404     C                   Z-ADD     0             v08DCE
403300140113     C                   Z-ADD     0             amgDCE            8 0
403400070404     C                   Z-ADD     0             v06dce
403500940711     C                   Z-ADD     0             VIDNRC
403600131121     C*******            SETON                                        68
403700131121     C                   MOVEL     ' '           D98TLA
403800131121     C                   MOVEL     'A'           D98TBO
403900131121     C                   MOVEL     ARBAAS        D98AAS
404000131121     C                   MOVEL     ARBLNP        D98LNP
404100131121     C                   MOVEL     ARBNRS        D98NRS
404200131121     C                   MOVEL     ARBNSP        D98NSP
404300131121     C*
404400131121     C                   CALL      'TNSD99R'
404500131121     C                   PARM                    TNSD99DS
404600140110     c
404700140110     c* Mi salvo data teorica consegna
404800140110     c                   eval      s_arbdei=d98dei
404900140110     c                   eval      s_arboei=d98oei
405000131121     C*
405100140110    1C     ARBDCE        IFEQ      0
405200131121    2c                   if        d98dce>0
405300140113     c                   eval      amgdce=d98dce
405400131121     c     *iso          movel     d98dce        dataeur
405500131121     c                   movel     dataeur       datadmy
405600131121     c                   movel     dataeur       v08dce
405700131121     c                   movel     datadmy       v06dce
405800131121   x2c                   else
405900131121     c                   clear                   v08dce
406000131121     c                   clear                   v06dce
406100131121    2c                   endif
406200131121     c
406300131121     C                   Z-ADD     D98NRC        VIDNRC
406400131121     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
406500131121     C*   CONSEGNA PREVISTA
406600131121     c  n69              eval      vidhmc=%editw(d98hce: '0 :  ')
406700131121     C*
406800131121     C* SE COMODO = 1  E' STATA FATTA CALL PGM
406900131121     C                   MOVEL     '1'           PA098
407000131121   x1C                   ELSE
407100140113     c                   eval      amgdce=arbdce
407200070404     c     *iso          movel     arbdce        dataeur
407300070404     c                   movel     dataeur       datadmy
407400070404     c                   movel     dataeur       v08dce
407500070404     c                   movel     datadmy       v06dce
407600070404     c
407700940711     C                   Z-ADD     ARBNRC        VIDNRC
407800131121    1C                   END
407900131121     c
408000911023     C* DATA MANDATO INTROITO
408100940712     C                   Z-ADD     ARBDMI        G02INV
408200940712     C                   Z-ADD     *ZEROS        G02DAT
408300940712     C                   MOVEL     '3'           G02ERR
408400940712     C                   CALL      'XSRDA8'
408500940712     C                   PARM                    WLBDAT
408600940712     C                   Z-ADD     G02DAT        VIDDMI
408700930113     C* DATA PRIMA CONSEGNA PARZIALE
408800930113     C                   Z-ADD     0             VIDDCP
408900930113     C     ARBDCP        IFGT      0
409000940712     C                   Z-ADD     ARBDCP        G02INV
409100940712     C                   Z-ADD     *ZEROS        G02DAT
409200940712     C                   MOVEL     '3'           G02ERR
409300940712     C                   CALL      'XSRDA8'
409400940712     C                   PARM                    WLBDAT
409500940712     C                   Z-ADD     G02DAT        VIDDCP
409600930113     C                   END
409700911023     C* DATA DISTINTA
409800940712     C                   Z-ADD     ARBDDC        G02INV
409900940712     C                   Z-ADD     *ZEROS        G02DAT
410000940712     C                   MOVEL     '3'           G02ERR
410100940712     C                   CALL      'XSRDA8'
410200940712     C                   PARM                    WLBDAT
410300940712     C                   Z-ADD     G02DAT        VIDDDC
410400911022     C* CODICE PADRONCINO CONSEGNA
410500940712     C                   MOVEL     ARBPDC        VIDPDC
410600940712     C                   MOVEL     *BLANKS       DESPDC
410700940712     C                   MOVEL     *BLANKS       DESPD1
410800940712     C     ARBPDC        IFGT      0
410900021203     C                   movel     arbpdc        kpdpdr
411000021203     C                   move      'A'           kpdtip
411100021203     C     kfapd         chain     fiapd01L                           31
411200940714     C  N31              MOVEL     APDRSC        DESPDC
411300940714     C  N31              MOVEL     APDRSC        DESPD1
411400930705     C                   END
411500950324     C* PRESTAZIONE PADRONCINO MATTINO O POMERIGGIO
411600950324     C                   MOVE      *BLANKS       FVVFPP
411700131120     C***                MOVE      *BLANKS       V5DFPP
411800070404     C                   MOVE      *BLANKS       V7DFPP
411900950327     C                   Z-ADD     ARBNDC        WNDC
412000020906     c                   Move      Arbifp        Wfgs
412100020906     c     Kfvv1         Chain     Fnfvv01l                           31
412200950324     C                   SELECT
412300991027     C                   WHEN      *IN31 = *on
412400991027     C                   WHEN      FVVFPP = 'M'
412500131120     C**                 eval      V5DFPP = 'MATTINO'
412600070504     C                   eval      V7DFPP = 'Mat'
412700991027     C                   WHEN      FVVFPP = 'P'
412800131120     C**                 eval      V5DFPP = 'POMERIGGIO'
412900070504     C                   eval      V7DFPP = 'Pom'
413000950324     C                   ENDSL
413100131119     c* controllo se ha PDA
413200131119     C                   Z-ADD     ARBNDC        kNDC6
413300131120     c                   clear                   vidpda
413400131120     c     Kdst          Chain     Fidst01l
413500131120     c                   if        %found(fidst01l)
413600131119     C                   movel     dstflr        DDStflr
413700131120     c                   if        (§dsttstpda= 'C' or  §dsttstpda= 'E') or
413800131120     c                             (dstpda<>'C' and  dstpda<>'E')
413900131120     c                   eval      vidpda='NO PDA'
414000131120     c                   movel     ri            ATRpda
414100131121     c                   else
414200131121     c                   eval      vidpda='SI PDA'
414300131121     c                   movel     ri            ATRpda
414400131119     c                   endif
414500131119     c                   endif
414600131108     c
414700131108     c* Se bolla in distinta, verifico quanti colli
414800131108     c                   clear                   v7dncl_co
414900131115     c                   clear                   v7dncl_de
415000131108     c                   clear                   wcol_D
415100131108     c                   clear                   wcol_I
415200131108    1c                   if        arbndc>0 and arbddc>0 and arbfbc=' '
415300131108     c* se ritorno all'incasso lo scrivo
415400131108     c
415500131108    2c                   if        arbdcm>0  and (arbicc='R' or arbicc=' ' or
415600131108     c                                  arbica=' ' or arbica='R')
415700131115     c                   eval      v7dncl_de='     IN consegna :'
415800131108     c                   eval      v7dncl_co='PER INCASSO'
415900131108    2c                   endif
416000131108     c
416100131108     c* Se colli da consegnare
416200131108    2c                   if        arbdcm=0
416300131108     C                   Z-ADD     ARBNDC        knfv
416400131108     c                   Move      Arbifp        Wfgs
416500131108     c
416600131108     c     karb2         setll     fnart01l
416700131108     c     karb2         reade     fnart01l
416800131108     c
416900131108    3c                   dow       not %eof(fnart01l)
417000131108     c                   if        artdcm=0
417100131108     c                   add       1             wcol_d
417200131108     c     kbrv          chain     fnbrv01l
417300131108     c                   if        %found(fnbrv01l)
417400131108     c                   add       1             wcol_I
417500131108     c                   endif
417600131108     c                   endif
417700131108     c
417800131108     c
417900131108     c     karb2         reade     fnart01l
418000131108    3c                   enddo
418100131108
418200131108    3c                   if        wcol_d>0
418300131115     c                   eval      v7dncl_de='Colli in consegna:'
418400131108    4c                   if        wcol_i=0
418500131108     c                   eval      v7dncl_co='0/'+%trim(%editc(wcol_d:'Z'))
418600131108     c                   else
418700131108     c                   eval      v7dncl_co=%trim(%editc(wcol_I:'Z')) +'/' +
418800131108     c                                       %trim(%editc(wcol_d:'Z'))
418900131108    4c                   endif
419000131108    3c                   endif
419100131108    c
419200131108    2c                   endif
419300131108    1c                   endif
419400131108     c
419500131108     c* Visualizzo ultimo evento
419600131108     c                   clear                   v7duevb
419700131114     c* prima vedo se c'e' da PDA
419800131114     c                   move      arbndc        pctndc
419900140224     c                   move      arbifp        pctfgs
420000131114     c                   move      'CET'         ttrd
420100131114     c     kcet          setgt     fipct02l
420200131114     c     kcet          readpe    fipct02l
420300140310     c                   if        not %eof(fipct02l)
420400131114     c                   movel     pctdati       fipctcetds
420500131114     c* consegnata
420600131114     c                   if        §pctcmc = ' '
420700131114     c                   MOVE      '  C'         evbcev                         *causale evento
420800131114     c                   else
420900131114     c* mancata consegna
421000131114     c                   movel     §PCTcmc       evbcev
421100131114     c                   endif
421200131114     c                   z-add     §pctdata      evbdev
421300131114     c                   movel     §pctora       evbhev
421400131114
421500131114     c                   exsr      Decodi_EVB
421600131114
421700131114     c                   else
421800131114     c*
421900131108     C                   Z-ADD     ARBAAS        KAAS$
422000131108     C                   Z-ADD     ARBLNP        KLNP
422100131108     C                   Z-ADD     ARBNRS        KNRS
422200131108     C                   Z-ADD     ARBNSP        KNSP
422300131108     C     KEVB1         SETgt     FNEVB01L
422400131108     C     KEVB1         readpe    FNEVB01L
422500131108     c
422600131114    1c                   if        not %eof(fnevb01l)
422700131114     c                   exsr      Decodi_EVB
422800131114    1c                   endif
422900131114    1c                   endif
423000911022     C*
423100911022     C* SE ESISTE AGGANCIO 2° BOLLA
423200991026     C                   Z-ADD     ARBAAS        KAAS
423300991026     C                   Z-ADD     ARBLNP        KLNP
423400991026     C                   Z-ADD     ARBNRS        KNRS
423500991026     C                   Z-ADD     ARBNSP        KNSP
423600991026     C                   MOVEL     '2'           KTRC
423700991027     C     KAR61         CHAIN     FIAR6000                           31
423800911023     C*
423900040216if  1C                   IF          %FOUND
424000991027     C                   eval      *in22 = *on
424100991027     C                   MOVEL     AR6CEI        V9CCEI
424200991027     C                   MOVEL     AR6NFT        V9CNFT
424300991027     C                   MOVEL     AR6FIV        V9CFIV
424400991027      *
424500991027     C                   eval      V9CDIV = AR6div
424600991027     C                   eval      WDIV = AR6div
424700991027     C                   exsr      CTR_VALUTA
424800991027      *
424900001128     C                   clear                   v9dfim
425000001128     C                   clear                   v9dfi2
425100991027     c                   if        AR6fim = 'P'
425200991027     c                   eval      V9Dfim = 'PARTENZA'
425300040126     c                   eval      V9Dfi2 = 'Immesse in'
425400991027     c                   endif
425500991027      * Varia G
425600040130     c                   clear                   W0133
425700040130     c                   clear                   v9csv1
425800040130     c                   clear                   v9cva1
425900040130     c                   clear                   v9csv2
426000040130     c                   clear                   v9cva2
426100040130     c                   clear                   v9csv3
426200040130     c                   clear                   v9cva3
426300040130     c                   clear                   v9csv4
426400040130     c                   clear                   v9cva4
426500041130     c                   clear                   v9csv5
426600041130     c                   clear                   v9cva5
426700041130     c                   clear                   v9csv6
426800041130     c                   clear                   v9cva6
426900040130     c                   clear                   v9civa
427000040130
427100040122      * Carico le varie
427200040122     c                   Movel     Ar6Sv1        wsvn
427300040122     c                   Z-add     Ar6Va1        wvan
427400040122     c                   ExSr      Car_Varie1
427500040122
427600040122     c                   Movel     Ar6Sv2        wsvn
427700040122     c                   Z-add     Ar6Va2        wvan
427800040122     c                   ExSr      Car_Varie1
427900040122
428000040122     c                   Movel     Ar6Sv3        wsvn
428100040122     c                   Z-add     Ar6Va3        wvan
428200040122     c                   ExSr      Car_Varie1
428300040122
428400040122     c     Kar61         Setll     Fiar7000
428500040216do  2c                   Do        *Hival
428600040122     c     Kar61         Reade     Fiar7000
428700040122     c                   If        %Eof(Fiar701l)
428800040122     c                   Leave
428900040122     c                   EndIf
429000040122     c                   Movel     Ar7Svn        wsvn
429100040122     c                   Z-add     Ar7Van        wvan
429200040216     c                   ExSr      Car_Varie1
429300040216e   2c                   EndDo
429400040122
429500991027      * TOTALE FATTURA
429600991027     c                   clear                   W0133
429700991027     c                   IF          AR6ift > *zeros
429800991027     c                   z-add     AR6ift        W0133
429900991027     c                   eval      Wtstdec = §cvfdc
430000991027     c                   exsr      EDTimporti
430100991027     c                   movel     IMPeditato    V9cift
430200001128     C                   else
430300001128     C                   clear                   v9cift
430400991027     c                   endif
430500991027      *
430600991027     c                   If         AR6dft > *zeros
430700991027     C     *iso          movel     AR6DFT        DATA_EUR
430800991027     C     *eur          movel     data_eur      W0080
430900991027     C                   eval      V9cdft = %EDITC(W0080:'Y')
431000001128     C                   else
431100001128     C                   clear                   v9cdft
431200991027     c                   Endif
431300991027      *
431400991027     c                   eval      V9Cksc = AR6ksc
431500991027     c                   eval      V9Cctr = AR6ctr
431600991027     c                   move      v9cksc        W0040
431700991027     c                   If        W0040 = 9999
431800991027     c                   eval      V9Dksc = arbrsd
431900991027     c                   Else
432000991027     c                   eval      i69kac = V9Cksc
432100991027     c                   exsr      CTR_ANAGRA
432200991027     c                   eval      V9Dksc = acorag
432300991027     c                   Endif
432400991027      *
432500991027     C                   eval      COD = 'TR'
432600991027     C                   movel     V9CCTR        UNOCTR            1
432700991027     C                   eval      KEY = unoctr
432800991027     C     KTAB1         CHAIN     TABEL00F                           31
432900991027     C                   movel     TBLUNI        V9DCTR
433000991027      *
433100930114     C* ESENZIONE IVA
433200001128     C                   clear                   v9dcei
433300991111     C     AR6CEI        IFNE      *blanks
433400940721     C                   MOVEL     'EI'          COD
433500991111     C                   MOVEL(P)  AR6CEI        KEY
433600940719     C     KTAB1         CHAIN     TABEL00F                           31
433700991027     C  N31              MOVEL     TBLUNI        V9DCEI
433800940719     C                   ENDIF
433900991027      *
434000040216e   1C                   ENDIF
434100930525     C*
434200991027     C* PER I DATI SULLA TASSAZIONE 1a BOLLA, DEVO CERCARE SU FIAR600F
434300940905     C                   SETOFF                                       24
434400110705     c                   clear                   atrbl4a
434500110705     c
434600110705     c                   clear                   v6car4ksc
434700110705     c                   clear                   v6dar4ksc
434800110705     c                   clear                   v6car4ctr
434900111021     c                   clear                   VIDimv
435000111021     c                   clear                   VIDpor
435100111021     c                   clear                   VIDift
435200111021     c                   clear                   VIDdiv
435300111021     c                   clear                   VIDfim
435400111021     C                   clear                   VIDCEI
435500111021     C                   clear                   VIDNFT
435600111021     C                   clear                   VIDFIV
435700111021     C                   clear                   DESCEI
435800111021     C                   clear                   viddft
435900111021     C                   clear                   vidIVA
436000111021      *
436100111021      * CERCO LE VARIE: LE PRIME 3 SU FIAR600F, EVENTUALI  ALTRE SU FIAR700F
436200111021     C                   eval      *in35 = *on
436300131107     C                   WRITE     LRI6C06
436400111021     C                   eval      *in35 = *off
436500111021      *
436600111021     c                   clear                   NR6
436700111021     c                   clear                   VIDiva
436800110705     c
436900940712     C                   Z-ADD     ARBAAS        KAAS
437000940712     C                   Z-ADD     ARBLNP        KLNP
437100940712     C                   Z-ADD     ARBNRS        KNRS
437200940712     C                   Z-ADD     ARBNSP        KNSP
437300110705     C                   MOVEL     'A'           KTRC
437400110705     C     KAR41         CHAIN     FiAR401L
437500110705if  1C                   IF        not %FOUND
437600110705     c                   eval      atrbl4a=nodi
437700110705     c                   else
437800110705     c                   movel     ar4not        dsbl4a
437900110705     c                   if        §b4aks>0
438000110705     C                   eval      *in24 = *on
438100110705     c                   eval      v6car4ksc=§b4aks
438200110705     c                   eval      i69kac = v6car4ksc
438300110705     c                   exsr      CTR_ANAGRA
438400110705     c                   eval      v6dar4ksc = acorag
438500110705     c                   if        §b4act<>*blanks
438600110705     c                   movel     §b4act        v6car4ctr
438700110705     c                   endif
438800110705     c* non c'e' cod cliente x tassaz.
438900110705     c                   else
439000110705     c                   eval      atrbl4a=nodi
439100110705     c                   endif
439200110705     c                   endif
439300110705     c
439400991026     C                   MOVEL     '1'           KTRC
439500991027     C     KAR61         CHAIN     FIAR6000
439600110705     c
439700110705     c                   if        %found(fiar601l)
439800110705     c
439900110705      * CLIENTE E TARIFFA FATTURAZIONE
440000110705     c                   IF        *IN33 = *ON
440100110705     c                   eval      v6cksc = arbksc
440200110705     c                   eval      v6cctr = arbctr
440300110705     c                   eval      v6dksc = arbrsm
440400110705     c                   ELSE
440500110705     c                   eval      v6cksc = AR6ksc
440600110705     c                   eval      v6cctr = AR6ctr
440700110705     c                   move      v6cksc        W0040
440800110705     c                   if        W0040 = 9999
440900110705     c                   eval      v6dksc = arbrsd
441000110705     c                   else
441100110705     c                   eval      i69kac = v6cksc
441200110705     c                   exsr      CTR_ANAGRA
441300110705     c                   eval      v6dksc = acorag
441400110705     c                   endif
441500110705     c                   ENDIF
441600110705      *   decodifica tariffa
441700110705     C                   eval      COD = 'TR'
441800110705     C                   MOVEL     V6CCTR        UNOCTR            1
441900110705     C                   MOVEL(P)  UNOCTR        KEY
442000110705     C     KTAB1         CHAIN     TABEL00F                           31
442100110705     C                   MOVEL     TBLUNI        V6DCTR
442200110705     c
442300110705     c                   endif
442400991027      *
442500040216if  1C                   IF        %FOUND  and
442600991027     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
442700991027     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
442800991027     C                              AR6sv3 <> *blanks)
442900991027      *
443000991027      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SE PRESENTE ALMENO UNO DEI CAMPI
443100991027     C                   EVAL      *IN24 = *ON
443200991027      *
443300991027     C                   eval      VIDDIV = AR6div
443400991027     C                   eval      WDIV = AR6div
443500991027     C                   exsr      CTR_VALUTA
443600991027      *
443700991027     c                   IF          ar6por > *zeros
443800991027     c                   z-add     AR6por        W0133
443900991027     c                   eval      Wtstdec = §cvfdc
444000991027     c                   exsr      EDTimporti
444100991027     c                   movel     IMPeditato    VIDpor
444200991128     c                   else
444300991128     c                   clear                   VIDpor
444400991027     c                   ENDIF
444500991027      *
444600991027     c                   IF          ar6imv > *zeros
444700991027     c                   z-add     AR6imv        W0133
444800991027     c                   eval      Wtstdec = §cvfdc
444900991027     c                   exsr      EDTimporti
445000991027     c                   movel     IMPeditato    VIDimv
445100991128     c                   else
445200991128     c                   clear                   VIDimv
445300991027     c                   ENDIF
445400991027      *
445500991027     c                   IF          ar6ift > *zeros
445600991027     c                   z-add     AR6ift        W0133
445700991027     c                   eval      Wtstdec = §cvfdc
445800991027     c                   exsr      EDTimporti
445900991027     c                   movel     IMPeditato    VIDift
446000991128     c                   else
446100991128     c                   clear                   VIDift
446200991027     c                   ENDIF
446300991027      *
446400991027     c                   IF        AR6fim = 'P'
446500991027     c                   eval      VIDfim = 'PARTENZA'
446600991128     c                   else
446700991128     c                   clear                   VIDfim
446800991027    2C                   ENDIF
446900991027      *
447000991027     C                   MOVEL     AR6CEI        VIDCEI
447100991027     C                   MOVEL     AR6NFT        VIDNFT
447200991027     C                   MOVEL     AR6FIV        VIDFIV
447300991027      *
447400991027     C                   IF        AR6CEI <>  *BLANKS
447500940808     C                   MOVEL     'EI'          COD
447600991027     C                   MOVEL(P)  AR6CEI        KEY
447700940808     C     KTAB1         CHAIN     TABEL00F                           31
447800940808     C  N31              MOVEL     TBLUNI        DESCEI
447900991128     c                   else
448000991128     c                   clear                   DESCEI
448100940808     C                   ENDIF
448200991027      *
448300991027      * DATA FATTURA DA GIRARE
448400991027     C                   IF         ar6dft > *zeros
448500991027     C     *iso          movel     AR6DFT        DATA_EUR
448600991027     C     *eur          movel     data_eur      W0080
448700991027     C                   eval      VIDdft = %EDITC(W0080:'Y')
448800991128     c                   else
448900991128     c                   clear                   VIDdft
449000991027     C                   ENDIF
449100991027      *
449200991027      *
449300991027     C                   MOVEL     ar6SV1        WSVN
449400991027     C                   MOVEL     ar6VA1        WVAN
449500991027     c                   exsr      car_varie
449600991027      *
449700991027     C                   MOVEL     ar6SV2        WSVN
449800991027     C                   MOVEL     ar6VA2        WVAN
449900991027     c                   exsr      car_varie
450000991027      *
450100991027     C                   MOVEL     ar6SV3        WSVN
450200991027     C                   MOVEL     ar6VA3        WVAN
450300991027     c                   exsr      car_varie
450400991027      *
450500991027      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
450600040216if  2C                   if          ar6SV3 <> *blanks
450700991027     C     KAR61         SETLL     FIAR7000
450800991027     C     KAR61         READE     FIAR7000
450900040216do  3C                   DOW       %EOF = *OFF
451000991027     C                   MOVEL     AR7SVN        WSVN
451100991027     C                   MOVEL     AR7VAN        WVAN
451200991027     c                   exsr      car_varie
451300991027     C     KAR61         READE     FIAR7000
451400040216e   3C                   ENDDO
451500040216e   2C                   endif
451600110705e   1C                   ENDIF
451700991027      *
451800991027      * Se non ho caricato record ne carico uno vuoto
451900110705     c                   IF        NR6 = *ZEROS  and *in24
452000991027     C                   clear                   V6CDE1
452100991027     C                   clear                   V6CVA1
452200991027     C                   clear                   V6CSV1
452300991027     C                   add       1             NR6
452400131107     C                   WRITE     LRI6S06
452500991027     c                   ENDIF
452600991027      *
452700991027      *
452800941206     C* 70 ON - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
452900941206     C     *IN33         IFEQ      '0'
453000941206     C     AR6NFT        ANDGT     0
453100941206     C     *IN22         OREQ      '1'
453200991111     C     AR6NFT        ANDGT     0
453300941206     C                   SETON                                        70
453400941206     C                   END
453500940712     C*
453600940719     C* PRELEVO LE NOTE DAL FILE APPOSITO
453700940719     C                   MOVEL     ARBAAS        KAAS
453800940719     C                   MOVEL     ARBLNP        KLNP
453900940719     C                   MOVEL     ARBNRS        KNRS
454000940719     C                   MOVEL     ARBNSP        KNSP
454100940719     C                   MOVEL     '8'           KTRC
454200060216     C     KAR41         CHAIN     FiAR401L                           31
454300940719     C  N31              MOVEL     AR4NOT        V4CNO1
454400940802     C   31              MOVEL     *BLANKS       V4CNO1
454500940719     C                   MOVEL     '9'           KTRC
454600060216     C     KAR41         CHAIN     FiAR401L                           31
454700940719     C  N31              MOVEL     AR4NOT        V4CNO2
454800940802     C   31              MOVEL     *BLANKS       V4CNO2
454900940719     C*
455000941110     C* ESTENSIONE RAGIONE SOCIALE DESTINATARIO
455100941110     C                   MOVEL     'D'           KTRC
455200060216     C     KAR41         CHAIN     FiAR401L                           31
455300950125     C  N31              MOVEL     AR4NOT        V4CNOT
455400950125     C   31              MOVEL     *BLANKS       V4CNOT
455500971015     C*
455600971015     C* SOLO PER EXPORT DATA DI STAMPA SU BORDERO' ARRIVI
455700070404     C***  *IN16         IFEQ      *ON
455800070404     C***                MOVEL     'A'           KTRC
455900070404     C***  KAR41         CHAIN     FiAR401L                           31
456000070404     C**N31              MOVE      AR4NOT        WDATA             8
456100070404     C***31              MOVEL     *ZEROS        WDATA
456200070404     C***  WDATA         IFGT      *ZEROS
456300070404     C***                MOVEL     WDATA         G02INV
456400070404     C***                Z-ADD     *ZEROS        G02DAT
456500070404     C***                MOVEL     '3'           G02ERR
456600070404     C***                CALL      'XSRDA8'
456700070404     C***                PARM                    WLBDAT
456800070404     C***                Z-ADD     G02DAT        V3CDST
456900070404     C***                ENDIF
457000070404     C***                ENDIF
457100940720     C*
457200051114     C* IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU FiAR900F
457300940720     C* CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
457400940720     C                   MOVEL     *BLANKS       VIDTIC
457500940720     C                   MOVEL     *ZERO         VIDCAS
457600940720     C                   MOVEL     *BLANKS       VIDVCA
457700940720     C                   MOVEL     *BLANKS       V4CGCA
457800940923     C                   MOVEL     *BLANKS       V4IGCA
457900091214     c                   setoff                                       65
458000940720     C     §3AFCA        IFGT      *ZERO
458100051128     C     Ksped         CHAIN     FiAR901L                           31
458200940720     C  N31              DO
458300940720     C                   MOVEL     AR9TIC        VIDTIC
458400940720     C                   MOVEL     AR9CAS        VIDCAS
458500940720     C                   MOVEL     AR9VCA        VIDVCA
458600940720     C                   MOVEL     AR9GCA        V4CGCA
458700091214     c                   seton                                        65
458800940720     C                   ENDDO
458900940720     C                   ENDIF
459000941215     C* ESISTE CONTRASSEGNO: VEDO SE ABILITATO
459100941215     C     VIDCAS        IFNE      0
459200941215     C                   SETON                                        29
459300941215     C                   END
459400940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
459500940923     C     V4CGCA        COMP      *BLANKS                            4141
459600940720     C*
459700940720     C* RIEMPO ANCHE ALCUNI CAMPI PER LE DS DI PASSAGGIO PARAMETRI
459800940720     C                   MOVEL     V3CDSP        PA1DSP
459900940720     C                   MOVEL     ARBRMN        PA1RMN
460000940720     C                   MOVEL     ARBLNA        PA1LNA
460100940802     C                   MOVEL     V3DLNA        PA1LN1
460200940720     C                   MOVEL     ARBRSD        PA1RSD
460300940906     C*-------------------------------------------------------------------------
460400991029      * GESTIONE INCASSO PARZIALE se assegnato o presente estensione bolla
460500991102     C                   clear                   v5cdiv
460600991102     c                   clear                   Wariimp
460700991102     c                   clear                   Wariipp
460800991102     c                   clear                   VIDimp
460900991102     c                   clear                   VIDipp
461000991102     C                   MOVEL     ARBFIP        VIDFIP
461100991102      *
461200991029     c  N33              eval      V5Cdiv = viddiv
461300991029     c   22              eval      V5Cdiv = v9cdiv
461400991029      *
461500040216if  1c                   if        V5CDIV <> *blanks
461600991029      *
461700991029     C                   eval      WDIV = V5Cdiv
461800991029     C                   exsr      CTR_VALUTA
461900991029      *
462000991029     C                   MOVEL     ARBAAS        KAAS
462100991029     C                   MOVEL     ARBLNP        KLNP
462200991029     C                   MOVEL     ARBNRS        KNRS
462300991029     C                   MOVEL     ARBNSP        KNSP
462400991029     C                   MOVEL     'A'           ktrc
462500991029      *
462600991029     c     kar61         setll     fiari000
462700991029     c     kar61         reade     fiari000
462800991029      *
462900040216do  2c                   DOW       not %EOF
463000040216if  3c                   if        v5cdiv = aridiv
463100991029     c                   add       ARIimp        Wariimp
463200991029     c                   add       ARIipp        Wariipp
463300040216x   3c                   else
463400991029      *
463500991029     C                   clear                   yeurcods
463600991029     C                   eval      YECDVI = aridiv
463700991029     C                   eval      YECDVO = V5Cdiv
463800991029     C                   eval      YECIMI = ARIimp
463900011022     C                   move      §CVDEC        YECDCO
464000991029     C                   CALL      'YEURCO'
464100991029     C                   PARM                    YEURcods
464200991029    6C                   if        YECESI = *blanks
464300991029     C                   add       YECIMO        WARIimp
464400991029    6C                   endif
464500991029      *
464600991029     C                   clear                   yeurcods
464700991029     C                   eval      YECDVI = aridiv
464800991029     C                   eval      YECDVO = V5Cdiv
464900991029     C                   eval      YECIMI = ARIipp
465000011022     C                   move      §CVDEC        YECDCO
465100991029     C                   CALL      'YEURCO'
465200991029     C                   PARM                    YEURcods
465300991029    6C                   if        YECESI = *blanks
465400991029     C                   add       YECIMO        WARIipp
465500991029    6C                   endif
465600991029      *
465700040216e   3c                   endif
465800991029      *
465900991029     c     kar61         reade     fiari000
466000040216e   2c                   enddo
466100991029      *
466200991029     c                   IF        WARIipp > *zeros
466300991029     c                   z-add     Wariipp       W0133
466400991029     c                   eval      Wtstdec = §cvfdc
466500991029     c                   exsr      EDTimporti
466600991029     c                   movel     IMPeditato    VIDipp
466700991029     c                   ENDIF
466800991029      *
466900991029     c                   IF        WARIimp > *zeros
467000991029     c                   z-add     Wariimp       W0133
467100991029     c                   eval      Wtstdec = §cvfdc
467200991029     c                   exsr      EDTimporti
467300991029     c                   movel     IMPeditato    VIDimp
467400991029     c                   ENDIF
467500991105      *
467600991105     c                   IF        VIDipp = *blanks  and  VIDimp = *blanks
467700991105     c                   clear                   V5CDIV
467800991105     c                   ENDIF
467900991029      *
468000040216e   1C                   endif
468100991029     C*-------------------------------------------------------------------------
468200951220     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
468300951220     C*
468400951220     C* PULISCO IL SFL
468500960926     C                   SETOFF                                       72
468600131107     C                   WRITE     LRI6C05
468700960926     C                   SETON                                        7172
468800951220     C*
468900060216     C* CHIAVE PER ACCEDERE AL FILE FiAR401L
469000951220     C                   MOVEL     ARBAAS        KAAS
469100951220     C                   MOVEL     ARBLNP        KLNP
469200951220     C                   MOVEL     ARBNRS        KNRS
469300951220     C                   MOVEL     ARBNSP        KNSP
469400951220     C*
469500951220     C                   Z-ADD     0             NRR5              4 0
469600951220     C                   MOVEL     '7K'          COD
469700951220     C*
469800060216     C     KAR42         SETLL     FiAR401L
469900060216     C     KAR42         READE     FiAR401L                               36
470000040216do  1C     *IN36         DOWEQ     *OFF
470100951220     C*
470200951220     C                   MOVEL(P)  AR4TRC        KEY
470300951220     C     KTAB1         CHAIN     TABEL                              36
470400951220     C*
470500951220     C     *IN36         IFEQ      *OFF
470600951220     C     TBLFLG        ANDEQ     ' '
470700951220     C                   MOVEL     TBLUNI        DS7K
470800951220     C     §7KVIS        IFEQ      'S'
470900951220     C                   MOVEL     §7KDES        VS5DES
471000951220     C                   MOVEL     AR4NOT        VS5NOT
471100951220     C                   ADD       1             NRR5
471200131107     C                   WRITE     LRI6S05
471300951220     C                   ENDIF
471400951220     C                   ENDIF
471500951220     C*
471600060216     C     KAR42         READE     FiAR401L                               36
471700040216e   1C                   ENDDO
471800951220     C*
471900960926     C* SE NON HO CARICATO NIENTE NON EMETTO IL SFLDSP PER EVITARE CHE
472000960926     C*  IL PGM DIA ERRORE DI SESSIONE O UNITA'
472100951220     C     NRR5          IFEQ      0
472200991105     C                   clear                   VS5DES
472300991105     C                   clear                   VS5NOT
472400991105     C                   ADD       1             NRR5
472500131107     C                   WRITE     LRI6S05
472600951220     C                   ENDIF
472700951220     C*
472800021216      * Riempio il campone in fondo alla videata con i tasti scelti
472900021216     C                   exsr      Ges_TastiFun
473000021217     c                   eval      FD07 = FDxx
473100021217     c                   eval      F24D07 = F24Dxx
473200911022     C                   ENDSR
473300131114      *-------------------------------------------------------------------------
473400131114      *  Decodifica EVENTO
473500131114      *-------------------------------------------------------------------------
473600131114     c     Decodi_evb    BEGSR
473700131114
473800131114     c                   if        evbcev='  C'
473900131114     c                   eval      §2ades='CONSEGNA'
474000131128     c* aggiungo anche firmatario
474100131128     c                   if        §PCTNOMFIR<>*blaNKS
474200131128     C                   EVAL      §2ADES=%TRIM(§2ADES)+' Firma:' +§PCTNOMFIR
474300131128     c                   endif
474400131128     c
474500131114     C                   else
474600131114     C                   MOVEL     '2A'          COD
474700131114     C                   MOVEL(p)  evbcev        KEY
474800131114     C     KTAB1         CHAIN     TABEL00F
474900131114    3c                   if        not %found(tabel00f)
475000131114     c                   clear                   ds2a
475100131114     c                   else
475200131114     c                   eval      ds2a=tbluni
475300131114    3c                   endif
475400131114    3c                   endif
475500131114     c
475600131114     c                   eval      v7duevb=evbcev+'-'+§2ades
475700131114
475800131114    3c                   if        evbdev>0
475900131114     c     *iso          movel     evbdev        dataeur
476000131114     c                   movel     dataeur       datadmy
476100131114     c                   movel     dataeur       w7cdev            8 0
476200131114
476300131114     c                   eval      indx=%checkr(' ':v7duevb)
476400131114    4c                   if        indx>46
476500131114     c                   eval      indx=46
476600131114     c                   else
476700131114     c                   eval      indx=indx+1
476800131114    4c                   endif
476900131114     c                   eval      wcarat=66-indx
477000131114     c                   eval      %subst(v7duevb:indx:wcarat)=' ' +'il ' +
477100131114     c                             %editw(W7cdev: '  /  /    ')
477200131114     c
477300131114    4c                   if        evbhev>0
477400131114     c                   eval      v7duevb=%trim(v7duevb)+' ' +
477500131114     c                                                 %editw(evbhev: '0 :  ')
477600131114    4c                   endif
477700131114     c
477800131114    3c                   endif
477900131114     c                   ENDSR
478000030703      *-------------------------------------------------------------------------
478100030703      *  Decoditifa tipo bancali
478200030703      *-------------------------------------------------------------------------
478300030703     c     Sr_DecTba     BegSr
478400030703     c                   Clear                   Tibs02Ds
478500030703     c                   Clear                   dTba
478600030703     c                   Movel     'C'           T02Mod
478700030703     c                   Movel     Knsif         T02Sif
478800030703     c                   Movel     'TBA'         T02Cod
478900030703     c                   Movel(p)  wtba          T02Ke1
479000030703     c                   Call      'TIBS02R'
479100030703     c                   Parm                    Kpjba
479200030703     c                   Parm                    Tibs02Ds
479300030703If  3c                   If        T02Err = *Blanks
479400030703     c                   Movel     T02Uni        dTba
479500030703    3c                   EndIf
479600030703     c                   Eval      wdestba = §TbaDesc
479700030703     c                   EndSr
479800021216      *-------------------------------------------------------------------------
479900021216      *  Imposto descrizione tasti funzione
480000021216      *-------------------------------------------------------------------------
480100021216     c     Ges_TastiFun  BEGSR
480200021217      *
480300021217      * per emettere i tasti senza condizionamenti ..
480400021216     c                   setoff                                       89
480500021216      *
480600021216      * Conta i caratteri riempiti dalle RigaTf1
480700021217     c                   z-add     0             $z                3 0
480800021216      *
480900021216      * Conta i caratteri riempiti dalle RigaTf2
481000021217     c                   z-add     0             $k                3 0
481100021216      *
481200021216      * Conta i caratteri riempiti dalle RigaTf3
481300021217     c                   z-add     0             $j                3 0
481400021216      *
481500021216      * Conta la posizione del campone da cui partire per impostare
481600021216      * la descrizione del tasto funzione
481700021216     c                   z-add     0             $y                2 0
481800021216      *
481900021217      * Stringhe che contengono le descrizioni dei tasti funzione
482000021216     c                   reset                   RigaTf1
482100021216     c                   reset                   RigaTf2
482200021216     c                   reset                   RigaTf3
482300070411      *
482400070411      *?F01?--> scelta bolla da assistenza clienti
482500070411     c                   IF        *in63 = *on
482600070411     c                   reset                   $Tf
482700070411     c                   movea     Tf01          $Tf
482800070411     c                   exsr      Rie_$Tf
482900070411     c                   else
483000070411      *?F01?--> scelta bolla generica
483100070411     c                   if        *in61 = *on
483200070411     c                   reset                   $Tf
483300070411     c                   movea     Tf01B         $Tf
483400070411     c                   exsr      Rie_$Tf
483500070411     c                   ENDIF
483600070411     c                   ENDIF
483700021216      *
483800070411      * F13 --> tassazione 1 bolla
483900021216     c                   IF        *in24 = *on
484000021217     c                   reset                   $Tf
484100021217     c                   movea     Tf13          $Tf
484200021217     c                   Exsr      Rie_$Tf
484300021217     C                   ENDIF
484400021216      *
484500070411      * F04 --> tassazione 2 bolla
484600021216     c                   IF        *in22 = *on
484700021217     c                   reset                   $Tf
484800021217     c                   movea     Tf04          $Tf
484900021217     c                   Exsr      Rie_$Tf
485000021217     C                   ENDIF
485100040220      *
485200070411      *?F16 --> assistenza clienti
485300040220     c                   IF        *in64 = *on
485400040220     c                   reset                   $Tf
485500040220     c                   movea     Tf16          $Tf
485600040220     c                   exsr      Rie_$Tf
485700040220     c                   ENDIF
485800021216      *
485900070411      * F05 --> eventi
486000021216     c                   IF        *in95 = *on
486100021217     c                   reset                   $Tf
486200021217     c                   movea     Tf05          $Tf
486300021217     c                   Exsr      Rie_$Tf
486400021217     C                   ENDIF
486500021216      *
486600021216      * F10
486700021217     c                   IF        *in98 = *on
486800021217     c                   reset                   $Tf
486900021217     c                   movea     Tf10          $Tf
487000021217     c                   Exsr      Rie_$Tf
487100021217     c                   ENDIF
487200021216      *
487300021216      * F14
487400021217     c                   IF        *in03 = *on
487500021217     c                   reset                   $Tf
487600021217     c                   movea     Tf14          $Tf
487700021217     c                   Exsr      Rie_$Tf
487800021217     c                   ENDIF
487900021216      *
488000021216      * F02
488100021217     c                   IF        *in04 = *on
488200021217     c                   reset                   $Tf
488300021217     c                   movea     Tf02          $Tf
488400021217     c                   Exsr      Rie_$Tf
488500021217     c                   ENDIF
488600110929      *
488700110930      * F22 --> Perfect Order
488800110929     c                   IF        *in86 = *on
488900110929     c                   reset                   $Tf
489000110929     c                   movea     Tf22          $Tf
489100110929     c                   Exsr      Rie_$Tf
489200110929     C                   ENDIF
489300021216      *
489400021216      * F11
489500021217     c                   IF        *in77 = *on
489600021217     c                   reset                   $Tf
489700021217     c                   movea     Tf11          $Tf
489800021217     c                   Exsr      Rie_$Tf
489900021217     c                   ENDIF
490000070404      *
490100070413      * F17 --> solo se partito com GEO
490200070413     c                   if        *in17
490300070404     c                   reset                   $Tf
490400070404     c                   movea     Tf17          $Tf
490500070404     c                   Exsr      Rie_$Tf
490600070413     c                   endif
490700021216      *
490800021216      * F07
490900021217     c                   IF        *in67 = *on
491000021217     c                   reset                   $Tf
491100021217     c                   movea     Tf07          $Tf
491200021217     c                   Exsr      Rie_$Tf
491300021217     c                   ENDIF
491400021216      *
491500021216      * F08
491600021217     c                   IF        *in74 = *on and *in16 = *on
491700021217     c                   reset                   $Tf
491800021217     c                   movea     Tf08          $Tf
491900021217     c                   Exsr      Rie_$Tf
492000021217     c                   ENDIF
492100130830
492200130830      * F09 --> Orari servizi non condizionato
492300131007     c                   IF        *in89 = *off and wabif9='S'
492400130930     c                   reset                   $Tf
492500130930     c                   movea     Tf09          $Tf
492600130930     c                   Exsr      Rie_$Tf
492700130930     c                   ENDIF
492800021216      *
492900021216      * F20 --> non condizionato
493000021217     c                   IF        *in89 = *off
493100021217     c                   reset                   $Tf
493200021217     c                   movea     Tf20          $Tf
493300021217     c                   Exsr      Rie_$Tf
493400021217     c                   ENDIF
493500021216      *
493600021216      * F21 --> non condizionato
493700021216     c                   IF        *in89 = *off
493800021217     c                   reset                   $Tf
493900021217     c                   movea     Tf21          $Tf
494000021217     c                   Exsr      Rie_$Tf
494100021217     c                   ENDIF
494200021218      *
494300021218      * F19 --> non condizionato
494400021218     c                   IF        *in89 = *off
494500021218     c                   reset                   $Tf
494600021218     c                   movea     Tf19          $Tf
494700021218     c                   Exsr      Rie_$Tf
494800021218     c                   ENDIF
494900070420     c
495000130830      * F06 --> solo se partito con GEO
495100070420     c                   if        *in17
495200070420     c                   reset                   $Tf
495300070420     c                   movea     Tf06          $Tf
495400070420     c                   Exsr      Rie_$Tf
495500070420     c                   ENDIF
495600021217      *
495700021217      * F12 --> non condizionato
495800021217     c                   IF        *in89 = *off
495900021217     c                   reset                   $Tf
496000021217     c                   movea     Tf12          $Tf
496100021217     c                   Exsr      Rie_$Tf
496200021217     c                   ENDIF
496300021216      *
496400021217      * Pulisco le stringa
496500021216     c                   z-add     1             $h
496600030121     c     1             do        62            $h                2 0
496700021216     C                   if        RigaTf1($h) = '#'
496800021216     c                   movel     *blanks       RigaTf1($h)
496900021216     c                   endif
497000021216     c                   enddo
497100021216     c                   z-add     1             $h
497200030121     c     1             do        62            $h                2 0
497300021216     C                   if        RigaTf2($h) = '#'
497400021216     c                   movel     *blanks       RigaTf2($h)
497500021216     c                   endif
497600021216     c                   enddo
497700021216     c                   z-add     1             $h
497800030121     c     1             do        62            $h                2 0
497900021216     C                   if        RigaTf3($h) = '#'
498000021216     c                   movel     *blanks       RigaTf3($h)
498100021216     c                   endif
498200021216     c                   enddo
498300021217      *
498400021216      * Imposto la descrizione del tasto funzione F24 e segnalo
498500021216      * quante righe ho riempito e quale devo emettere
498600021216     c                   SELECT
498700021216      *
498800021216     c                   when      $k<>*zeros and $Y<>*zeros
498900021216     c                   eval      $loop = 3
499000021216     c                   z-add     3             $loop             1 0
499100021217     C                   movel(p)  CF2413        F24Dxx           16
499200021216      *
499300021216     c                   when      $k<>*zeros and $Y=*zeros
499400021216     c                   z-add     2             $loop             1 0
499500021217     C                   movel(p)  CF2412        F24Dxx
499600021216      *
499700021216     c                   Other
499800021216     c                   z-add     1             $loop             1 0
499900021217     C                   movel     *blanks       F24Dxx
500000021216     c                   ENDSL
500100021216      * Prima riga di tasti funzione
500200030121     c                   movea     RigaTF1       FDxx             62
500300021216     c                   z-add     1             $Rig              1 0
500400021216      *
500500021216     c                   endsr
500600021217      *-------------------------------------------------------------------------
500700021217      *  Riempio le stringhe con i campi funzione compattati
500800021217      *-------------------------------------------------------------------------
500900021217     c     Rie_$Tf       begsr
501000021217      *
501100021217     c                   z-add     1             $x                2 0
501200021217     c     '#'           lookup    $Tf($x)                                31
501300021217     c                   add       $x            $Z
501400030121     c                   IF        $Z <= 63
501500021217     c                   eval      $j = $Z - $x + 1
501600021217     c                   movea     $Tf           RigaTF1($j)
501700021217     c                   ELSE
501800021217     c                   add       $x            $K
501900030121     c                   IF        $K <= 63
502000021217     c                   eval      $j = $K - $x + 1
502100021217     c                   movea     $Tf           RigaTF2($j)
502200021217     c                   ELSE
502300021217     c                   add       $x            $Y
502400030121     c                   IF        $Y <= 63
502500021217     c                   eval      $j = $Y - $x + 1
502600021217     c                   movea     $Tf           RigaTF3($j)
502700021217     c                   ENDIF
502800021217     c                   ENDIF
502900021217     c                   ENDIF
503000021217      *
503100021217     c                   endsr
503200021217      *-------------------------------------------------------------------------
503300021217      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
503400021217      *-------------------------------------------------------------------------
503500021217     c     Ges_F24       begsr
503600021217      *
503700021217     c                   SELECT
503800021217      *
503900021217     c                   WHEN      $Loop = 2 and $Rig =2  or
504000021217     c                             $Loop = 3 and $Rig =3
504100021217     c                   movea     RigaTF1       FDxx
504200021217     c                   z-add     1             $Rig
504300021217      *
504400021217     c                   if        $loop = 2
504500021217     c                   eval      F24Dxx= Cf2412
504600021217     c                   else
504700021217     c                   eval      F24Dxx= Cf2413
504800021217     c                   endif
504900021217      *
505000021217     c                   WHEN      $Loop = 2 and $Rig =1 or
505100021217     c                             $Loop = 3 and $Rig =1
505200021217     c                   movea     RigaTF2       FDxx
505300021217     c                   z-add     2             $Rig
505400021217      *
505500021217     c                   if        $loop = 2
505600021217     c                   eval      F24Dxx= Cf2422
505700021217     c                   else
505800021217     c                   eval      F24Dxx= Cf2423
505900021217     c                   endif
506000021217      *
506100021217     c                   WHEN      $Loop = 3 and $Rig =2
506200021217     c                   movea     RigaTF3       FDxx
506300021217     c                   z-add     3             $Rig
506400021217     c                   eval      F24Dxx= Cf2433
506500021217     c                   ENDSL
506600021217      *
506700021217     c                   endsr
506800021217      *-------------------------------------------------------------------------
506900991027      *  CARICO SUBFILE VARIE
507000991027      *-------------------------------------------------------------------------
507100991027     c     CAR_VARIE     BEGSR
507200991128      *
507300991027      *
507400991027     c                   SELECT
507500991027      *
507600991027     c                   WHEN       WSVN = *BLANKS
507700991027      *
507800991027     C                   WHEN       WSVN = §$2iva
507900991027     c                   z-add     Wvan          W0133
508000991027     c                   eval      Wtstdec = §cvfdc
508100991027     c                   exsr      EDTimporti
508200991027     c                   movel     IMPeditato    VIDiva
508300991027      *
508400991027     c                   OTHER
508500991027     c                   z-add     Wvan          W0133
508600991027     c                   eval      Wtstdec = §cvfdc
508700991027     c                   exsr      EDTimporti
508800991027     c                   movel     IMPeditato    V6Cva1
508900991027     c                   movel     WSVN          V6Csv1
509000991027     C                   Z-ADD     1             WA
509100991027     C     V6CSV1        LOOKUP    CC1(WA)                                99
509200020329     C     CC3(WA)       ifeq      'S'
509300020329      *
509400020329      * Verifico se si tratta di una bolla Fedex
509500021119     c                   if        LnaNtw = 'FED'
509600020329     C                   Z-ADD     1             WA
509700020329     C     V6CSV1        LOOKUP    CB1(WA)                                99
509800020329     C   99              MOVEL     CB2(WA)       V6CDE1
509900020329     C  N99              clear                   V6CDE1
510000020329     C                   else
510100991027     C   99              MOVEL     CC2(WA)       V6CDE1
510200991027     C  N99              MOVEL     *BLANKS       V6CDE1
510300020329     C                   endif
510400020329     C                   else
510500020329     C   99              MOVEL     CC2(WA)       V6CDE1
510600020329     C  N99              MOVEL     *BLANKS       V6CDE1
510700020329     C                   endif
510800991027     C                   add       1             NR6
510900131107     C                   WRITE     LRI6S06
511000991027      *
511100991027     c                   ENDSL
511200991027      *
511300991027     c                   ENDSR
511400040122      *-------------------------------------------------------------------------
511500040122      *  Carico le varie della seconda bolla
511600040122      *-------------------------------------------------------------------------
511700040122     c     Car_Varie1    BegSr
511800040122
511900040122s   1c                   Select
512000040122
512100040122     c                   When      wsvn = *Blanks
512200040122      * Iva
512300040122     c                   When      wsvn = §$2iva
512400040122     c                   z-add     wvan          w0133
512500040122     c                   Eval      Wtstdec = §cvfdc
512600040122     c                   ExSr      EDTimporti
512700040122     c                   Movel     IMPeditato    V9cIva
512800070411     c                   setoff                                       06
512900070411      * Bollo
513000070411     c                   When      wsvn = §$2bol
513100070411     c                   z-add     wvan          w0133
513200070411     c                   Eval      Wtstdec = §cvfdc
513300070411     c                   ExSr      EDTimporti
513400070411     c                   Movel     IMPeditato    V9cIva
513500070411     c                   seton                                        06
513600040122      * Varia G - sempre per prima
513700040122     c                   When      wsvn = §$2vrg
513800040122     c                   Eval      V9cSv1 = wsvn
513900040122     c                   z-add     wvan          w0133
514000040122     c                   Eval      Wtstdec = §cvfdc
514100040122     c                   ExSr      EDTimporti
514200040122     c                   Movel     IMPeditato    V9cVa1
514300040122      * Altre varie
514400040122   x1c                   Other
514500040122     c                   z-add     wvan          W0133
514600040122     c                   eval      Wtstdec = §cvfdc
514700040122     c                   exsr      EDTimporti
514800040122if  2c                   If        V9cSv2 = *Blanks
514900040122     c                   movel     IMPeditato    V9cVa2
515000040122     c                   movel     wsvn          V9csv2
515100040122   x2c                   Else
515200040122if  3c                   If        V9cSv3 = *Blanks
515300040122     c                   movel     IMPeditato    V9cVa3
515400040122     c                   movel     wsvn          V9csv3
515500040122   x3c                   Else
515600040122if  4c                   If        V9cSv4 = *Blanks
515700040122     c                   movel     IMPeditato    V9cVa4
515800040122     c                   movel     wsvn          V9csv4
515900041130   x4c                   Else
516000041130if  5c                   If        V9cSv5 = *Blanks
516100041130     c                   movel     IMPeditato    V9cVa5
516200041130     c                   movel     wsvn          V9csv5
516300041130   x5c                   Else
516400041130if  6c                   If        V9cSv6 = *Blanks
516500041130     c                   movel     IMPeditato    V9cVa6
516600041130     c                   movel     wsvn          V9csv6
516700041130    6c                   EndIf
516800041130    5c                   EndIf
516900040122    4c                   EndIf
517000040122    3c                   EndIf
517100040122    2c                   EndIf
517200040122
517300040122    1c                   EndSl
517400040122
517500040122     c                   EndSr
517600991027      *****************************************************************
517700991027      *  AGGANCIO ANAGRAFICHE
517800991027      *****************************************************************
517900991027     C     CTR_ANAGRA    BEGSR
518000991027      *
518100991027     C                   clear                   DS_cnaco
518200991027     C                   clear                   DS_cnind
518300991027     C                   clear                   DS_cnclp
518400991027     C                   clear                   DS_fncls
518500991027      *
518600991027     C                   CALL      'TIBS69R'
518700991027     C                   PARM                    tibs69DS
518800991027     C                   PARM                    DS_cnaco
518900991027     C                   PARM                    DS_cnind
519000991027     C                   PARM                    DS_cnclp
519100991027     C                   PARM                    DS_fncls
519200991027      *
519300991027     C                   eval      wtibs69r = 'S'
519400991027      * per il 1° giro è inizializzata nelle specifiche "D"
519500991027     C                   clear                   TIBS69DS
519600991027      *
519700991027     C                   ENDSR
519800991027      **********************************************************************
519900991027      * AGGANCIO TABELLA VALUTE
520000991027      **********************************************************************
520100991027     C     CTR_VALUTA    BEGSR
520200991027
520300991027     C                   MOVEL     'CV'          COD
520400991027     C                   MOVEL(P)  WDIV          KEY
520500991027     C     KTAB1         CHAIN     TABEL00F                           31
520600991027     C  N31              MOVEL     TBLUNI        DSCV
520700991027
520800991027     C                   ENDSR
520900991027      **********************************************************************
521000991027      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
521100991027      **********************************************************************
521200991027     C     EDTIMPORTI    BEGSR
521300991027
521400991027      * ------------------------------------------------------------------------
521500991027      * W0133      contiene il numero da editare per il video
521600991027      * WTSTDEC    corrisponde a §CVfdc della tabella CV
521700991027      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
521800991027      * ------------------------------------------------------------------------
521900991027      *
522000991027     c                   clear                   IMPeditato
522100991027     c                   clear                   W017A
522200991027      *
522300991027     c                   if        Wtstdec = 'S'
522400991027     C                   eval      W017a = %EDITC(W0133:'2')
522500991027     c                   else
522600991027     c                   z-add     W0133         W0130
522700991027     C                   eval      W017a = %EDITC(W0130:'2')
522800991027     c                   endif
522900991027      *
523000991027     c                   move      W017a         IMPeditato
523100991027      *
523200991027     c                   ENDSR
523300940802     C**************************************************************************
523400940802     C*    LEGAMI BOLLA
523500940802     C**************************************************************************
523600930413     C     LEGBOL        BEGSR
523700940802     C*
523800940912     C* DEVO MEMORIZZARE LA BOLLA POICHE DEVO RIAGGANCIARLA DOPO
523900940912     C                   MOVEL     ARBAAS        WAAS
524000940912     C                   MOVEL     ARBLNP        WLNP
524100940912     C                   MOVEL     ARBNRS        WNRS
524200940912     C                   MOVEL     ARBNSP        WNSP
524300940912     C*
524400940907     C* PULISCO IL SFL
524500950825     C                   SETOFF                                       787923
524600131107     C                   WRITE     LRI6C08
524700950825     C                   SETON                                        7923
524800940923     C                   MOVEL     *BLANKS       LEGAME
524900940907     C*
525000940907     C                   SETOFF                                       94
525100950718     C                   Z-ADD     *ZERO         NR8
525200940907     C                   Z-ADD     *ZERO         SAVNR8            4 0
525300940907     C                   MOVEL     *BLANKS       WFLG1             1
525400940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
525500940907     C*
525600940907     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
525700940907     C     KARB          CHAIN     FNLBL01L                           99
525800940907     C   99KARB          CHAIN     FNLBL02L                           99
525900940907     C  N99              DO
526000940907     C*
526100940907     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
526200940907     C                   MOVEL     LBLAAO        KAAS
526300940907     C                   MOVEL     LBLLPO        KLNP
526400940907     C                   MOVEL     LBLNRO        KNRS
526500940907     C                   MOVEL     LBLNSO        KNSP
526600940907     C     KLBL3         CHAIN     FNLBL02L                           99
526700950103     C     *IN99         IFEQ      *ON
526800950103     C                   MOVEL     'N'           WORIGI            1
526900950103     C                   ELSE
527000950103     C                   MOVEL     'S'           WORIGI
527100950103     C                   ENDIF
527200940907     C*
527300940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
527400940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
527500950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
527600950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
527700940907     C                   MOVEL     ' '           V8CSCE
527800940907     C                   MOVEL     LBLAAO        V8CAAP
527900940907     C                   MOVEL     LBLLPO        V8CLPP
528000940907     C                   MOVEL     LBLLAO        V8CLAP
528100940907     C                   MOVEL     LBLNRO        V8CNRP
528200940907     C                   MOVEL     LBLNSO        V8CNSP
528300940926     C                   CLEAR                   V8CAAN
528400940926     C                   CLEAR                   V8CLPN
528500940926     C                   CLEAR                   V8CLAN
528600940926     C                   CLEAR                   V8CNRN
528700940926     C                   CLEAR                   V8CNSN
528800940907     C*
528900940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
529000940907     C     LBLAAO        IFEQ      V3CAAS
529100940907     C     LBLLPO        ANDEQ     V3CLNP
529200940907     C     LBLNRO        ANDEQ     V3CNRS
529300940907     C     LBLNSO        ANDEQ     V3CNSP
529400940907     C                   MOVEL     *BLANKS       V8CPRE
529500940907     C                   SETON                                        80
529600940907     C                   SETON                                        81
529700940907     C                   MOVEL     '1'           WFLG1
529800940907     C                   ELSE
529900940907     C                   SETOFF                                       81
530000940907     C*
530100950316     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
530200950316     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
530300950316     C                   SETOFF                                       93
530400950316     C     KLBL3         SETLL     FNARB01L                               93
530500950316     C  N93              MOVEA     PRD(1)        V8CPRE
530600950316     C  N93              SETON                                        80
530700950316     C   93              MOVEA     PRD(2)        V8CPRE
530800950316     C   93              SETOFF                                       80
530900950316     C*
531000950316     C* SE NON TROVATA SU ARB PROVO SU BLP
531100950316     C  N93              DO
531200950316     C                   SETOFF                                       93
531300950316     C     KLBL3         SETLL     FNBLP01L                               93
531400010712      *
531500010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
531600010712     c                   if        *in93 = *off
531700010712     C                   MOVEA     PRD(1)        V8CPRE
531800010712     c                   else
531900950316     C* SE RICHIAMATO DALLE PARTENZE (FNLS05R) NON POSSO
532000950316     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
532100010712     C     PA1FLG        IFEQ      '3'
532200960903     C     PA1FLG        OREQ      'N'
532300950316     C                   MOVEA     PRD(3)        V8CPRE
532400950316     C                   SETON                                        80
532500950316     C                   ELSE
532600950316     C                   MOVEA     PRD(2)        V8CPRE
532700950316     C                   SETOFF                                       80
532800950316     C                   ENDIF
532900010712     C                   ENDIF
533000950316     C*
533100950316     C                   ENDDO
533200940907     C*
533300940907     C                   ENDIF
533400940907     C                   SETON                                        83
533500950103     C*
533600950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
533700950103     C                   MOVEL     *IN80         V8CI80
533800950103     C                   MOVEL     *IN81         V8CI81
533900950103     C                   MOVEL     *IN82         V8CI82
534000950103     C                   MOVEL     *IN83         V8CI83
534100950103     C*
534200940907     C                   Z-ADD     1             NR8
534300131107     C                   WRITE     LRI6S08
534400950825     C                   SETON                                        78
534500950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
534600940907     C*
534700950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
534800950103     C*                      2) NON HO TROVATO L'ORIGINALE
534900950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
535000950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
535100950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
535200950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
535300950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
535400950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
535500950103     C* L'ORIGINALE E LA MAMMA.
535600950103     C     WORIGI        IFEQ      'N'
535700950103     C                   MOVEL     *BLANKS       V8CSCE
535800950103     C                   CLEAR                   V8CAAP
535900950103     C                   CLEAR                   V8CLPP
536000950103     C                   CLEAR                   V8CLAP
536100950103     C                   CLEAR                   V8CNRP
536200950103     C                   CLEAR                   V8CNSP
536300950103     C                   MOVEL     LBLAAP        V8CAAN
536400950103     C                   MOVEL     LBLLPP        V8CLPN
536500950103     C                   MOVEL     LBLLAP        V8CLAN
536600950103     C                   MOVEL     LBLNRP        V8CNRN
536700950103     C                   MOVEL     LBLNSP        V8CNSN
536800950103     C                   SETOFF                                       8182
536900950103     C*
537000950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
537100950103     C     V8CAAN        IFEQ      V3CAAS
537200950103     C     V8CLPN        ANDEQ     V3CLNP
537300950103     C     V8CNRN        ANDEQ     V3CNRS
537400950103     C     V8CNSN        ANDEQ     V3CNSP
537500950103     C                   MOVEL     *BLANKS       V8CPRE
537600950103     C                   SETON                                        80
537700950103     C                   SETON                                        82
537800950103     C                   ELSE
537900950103     C*
538000950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
538100950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
538200950316     C                   EXSR      CHKBOL
538300950103     C*
538400950103     C                   ENDIF
538500950103     C*
538600950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
538700950103     C                   SETOFF                                       83
538800950103     C*
538900950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
539000950103     C                   MOVEL     *IN80         V8CI80
539100950103     C                   MOVEL     *IN81         V8CI81
539200950103     C                   MOVEL     *IN82         V8CI82
539300950103     C                   MOVEL     *IN83         V8CI83
539400950103     C*
539500950103     C                   ADD       1             NR8
539600131107     C                   WRITE     LRI6S08
539700950103     C*
539800950103     C                   ENDIF
539900940907     C                   SETOFF                                       83  99
540000940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
540100940907     C* CICLO FINO ALL'ULTIMO LEGAME
540200940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
540300940907     C                   Z-ADD     *ZERO         WA                3 0
540400940907     C                   Z-ADD     *ZERO         WB                3 0
540500940907     C                   MOVEA     *ZERO         AAN
540600940907     C     *IN99         DOWEQ     *OFF
540700940907     C*
540800940907     C                   MOVEL     ' '           V8CSCE
540900940907     C                   MOVEL     LBLAAP        V8CAAP
541000940907     C                   MOVEL     LBLLPP        V8CLPP
541100940907     C                   MOVEL     LBLLAP        V8CLAP
541200940907     C                   MOVEL     LBLNRP        V8CNRP
541300940907     C                   MOVEL     LBLNSP        V8CNSP
541400940907     C                   MOVEL     LBLAAN        V8CAAN
541500940907     C                   MOVEL     LBLLPN        V8CLPN
541600940907     C                   MOVEL     LBLLAN        V8CLAN
541700940907     C                   MOVEL     LBLNRN        V8CNRN
541800940907     C                   MOVEL     LBLNSN        V8CNSN
541900940907     C                   SETOFF                                       8182
542000940907     C*
542100950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
542200940907     C     LBLAAP        IFEQ      V3CAAS
542300940907     C     LBLLPP        ANDEQ     V3CLNP
542400940907     C     LBLNRP        ANDEQ     V3CNRS
542500940907     C     LBLNSP        ANDEQ     V3CNSP
542600940907     C                   SETON                                        81
542700940907     C                   ENDIF
542800940907     C*
542900940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
543000940907     C     LBLAAN        IFEQ      V3CAAS
543100940907     C     LBLLPN        ANDEQ     V3CLNP
543200940907     C     LBLNRN        ANDEQ     V3CNRS
543300940907     C     LBLNSN        ANDEQ     V3CNSP
543400940907     C                   MOVEL     *BLANKS       V8CPRE
543500940907     C                   SETON                                        80
543600940907     C                   SETON                                        82
543700940907     C                   ELSE
543800940907     C*
543900940907     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
544000940907     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
544100950316     C                   EXSR      CHKBOL
544200940907     C*
544300940907     C                   ENDIF
544400950103     C*
544500950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
544600950103     C                   MOVEL     *IN80         V8CI80
544700950103     C                   MOVEL     *IN81         V8CI81
544800950103     C                   MOVEL     *IN82         V8CI82
544900950103     C                   MOVEL     *IN83         V8CI83
545000940907     C*
545100940907     C* SCRIVO IL RECORD NEL SFL
545200940907     C                   ADD       1             NR8
545300131107     C                   WRITE     LRI6S08
545400940907     C*
545500940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
545600940907     C                   ADD       1             WA
545700940907     C                   MOVEL     LBLAAN        AAN(WA)
545800940907     C                   MOVEL     LBLLPN        LPN(WA)
545900940907     C                   MOVEL     LBLNRN        NRN(WA)
546000940907     C                   MOVEL     LBLNSN        NSN(WA)
546100940907     C*
546200940907     C     KLBL3         READE     FNLBL02L                               99
546300940907     C*
546400940907     C* SE NON HO PIU' BOLLE CON READE
546500940907     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
546600970116     C     WB            DOWLT     WA
546700970116     C     *IN99         ANDEQ     *ON
546800940907     C                   ADD       1             WB
546900940907     C                   MOVEL     AAN(WB)       KAAS
547000940907     C                   MOVEL     LPN(WB)       KLNP
547100940907     C                   MOVEL     NRN(WB)       KNRS
547200940907     C                   MOVEL     NSN(WB)       KNSP
547300940907     C     KLBL3         SETLL     FNLBL02L
547400940907     C     KLBL3         READE     FNLBL02L                               99
547500970116     C                   ENDDO
547600940907     C*
547700940907     C                   ENDDO
547800940907     C                   ENDDO
547900940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
548000940907     C*
548100940907     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
548200940907     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
548300940907     C     NR8           IFEQ      2
548400940907     C*
548500940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
548600131107     C     2             CHAIN     LRI6S08                            99
548700940907     C     V8CAAP        IFEQ      V3CAAS
548800940907     C     V8CLPP        ANDEQ     V3CLNP
548900940907     C     V8CNRP        ANDEQ     V3CNRS
549000940907     C     V8CNSP        ANDEQ     V3CNSP
549100940907     C                   MOVEL     V8CAAN        COMAAN
549200940907     C                   MOVEL     V8CLPN        COMLPN
549300940907     C                   MOVEL     V8CNRN        COMNRN
549400940907     C                   MOVEL     V8CNSN        COMNSN
549500950307     C                   MOVEL     V8CLAN        COMLAN
549600950316     C                   MOVEA     'SEGUEN.'     LBL(14)
549700940907     C                   ELSE
549800940907     C                   MOVEL     V8CAAP        COMAAN
549900940907     C                   MOVEL     V8CLPP        COMLPN
550000940907     C                   MOVEL     V8CNRP        COMNRN
550100940907     C                   MOVEL     V8CNSP        COMNSN
550200950307     C                   MOVEL     V8CLAP        COMLAN
550300950316     C                   MOVEA     'PRECED.'     LBL(14)
550400940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
550500940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
550600940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
550700131107     C     1             CHAIN     LRI6S08                            99
550800940926     C* REIMPOSTO NR8=2 PERCHE' MI SERVE DOPO
550900940926     C                   Z-ADD     2             NR8
551000940907     C                   ENDIF
551100940907     C*
551200950316     C                   MOVEA     COMLPN        LBL(21)
551300950316     C                   MOVEA     COMNRN        LBL(25)
551400950316     C                   MOVEA     COMNSN        LBL(28)
551500950316     C                   MOVEA     COMLAN        LBL(40)
551600940907     C*
551700940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
551800940907     C                   Z-ADD     1             W                 1 0
551900940907     C     V8CPRE        LOOKUP    PRD(W)                                 99
552000950307     C                   MOVEA     LBD(W)        LBL(44)
552100940907     C     W             COMP      2                                      94
552200940907     C*
552300940907     C                   MOVEA     LBL           LEGAME
552400940907     C                   ELSE
552500940907     C*
552600940907     C     NR8           IFGT      2
552700940907     C                   MOVEA     LB2           LEGAME
552800940907     C                   SETON                                        94
552900940907     C                   ENDIF
553000940907     C                   ENDIF
553100940907     C*
553200940907     C     NR8           COMP      1                                    3434
553300940907     C*
553400940907     C                   Z-ADD     NR8           SAVNR8
553500940907     C                   Z-ADD     1             NR8
553600940923     C     LEGAME        COMP      *BLANKS                            9292
553700940912     C*
553800940912     C* RIAGGANCIO LA BOLLA IN ESAME
553900940912     C                   MOVEL     WAAS          ARBAAS
554000940912     C                   MOVEL     WLNP          ARBLNP
554100940912     C                   MOVEL     WNRS          ARBNRS
554200940912     C                   MOVEL     WNSP          ARBNSP
554300940912     C     KARB2         CHAIN     FNARB01L                           99
554400930511     C*
554500930413     C                   ENDSR
554600950316     C**************************************************************************
554700950316     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
554800950316     C**************************************************************************
554900950316     C     CHKBOL        BEGSR
555000950316     C*
555100950316     C                   SETOFF                                       93
555200950316     C     KLBL1         SETLL     FNARB01L                               93
555300950316     C  N93              MOVEA     PRD(1)        V8CPRE
555400950316     C  N93              SETON                                        80
555500950316     C   93              MOVEA     PRD(2)        V8CPRE
555600950316     C   93              SETOFF                                       80
555700950316     C*
555800950316     C* SE NON TROVATA SU ARB PROVO SU BLP
555900950316     C  N93              DO
556000950316     C                   SETOFF                                       93
556100950316     C     KLBL1         SETLL     FNBLP01L                               93
556200010712      *
556300010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
556400010712     c                   if        *in93 = *off
556500010712     C                   MOVEA     PRD(1)        V8CPRE
556600010712     c                   else
556700950316     C* SE RICHIAMATO DALLE PARTENZE (FNLS05R) NON POSSO
556800950316     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
556900010712     C     PA1FLG        IFEQ      '3'
557000960903     C     PA1FLG        OREQ      'N'
557100950316     C                   MOVEA     PRD(3)        V8CPRE
557200950316     C                   SETON                                        80
557300950316     C                   ELSE
557400950316     C                   MOVEA     PRD(2)        V8CPRE
557500950316     C                   SETOFF                                       80
557600950316     C                   ENDIF
557700010712     C                   ENDIF
557800950316     C*
557900950316     C                   ENDDO
558000950316     C*
558100950316     C                   ENDSR
558200940802     C**************************************************************************
558300940802     C*    VISUALIZZA BOLLA
558400940802     C**************************************************************************
558500911023     C     VISUAL        BEGSR
558600940321      *
558700940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
558800131108     c     Emriepil      tag
558900940712     C                   EXSR      RIEPIL
559000940712      *
559100131108      * PER F15 PASSO ALLA BOLLA, PER INVIO o F12 RITORNO AL SFL
559200940712     C  NKP              GOTO      ENDVIS
559300931112      *
559400931112     C                   SETOFF                                       89
559500940802     C                   MOVEL     'S'           WRIEM             1
559600021217      *
559700021217      * Riempio il campone in fondo alla videata con i tasti scelti
559800021217     C                   exsr      Ges_TastiFun
559900021217     c                   eval      FD03 = FDxx
560000021217     c                   eval      F24D03 = F24Dxx
560100911023     C     FOR03         TAG
560200940802     C     WRIEM         IFEQ      'S'
560300131107     C                   WRITE     LRI6T01
560400940802     C                   MOVEL     *BLANKS       WRIEM
560500940802     C                   ENDIF
560600021217      *
560700021217      * Imposto la stringa comandi del 'giro' successivo
560800021217     C                   IF        *inKY = *on and $Loop > 1
560900021217     c                   Exsr      Ges_F24
561000021217     c                   eval      FD03 = FDxx
561100021217     c                   eval      F24D03 = F24Dxx
561200021217     C                   ENDIF
561300021217      *
561400131107     C                   EXFMT     LRI6D03
561500070420     c                   setoff                                       28
561600070420     c                   clear                   v1cmsg
561700931112      *
561800131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
561900931112     C   KY*IN89         IFEQ      *ON
562000931112     C                   SETOFF                                       89
562100931112     C                   ELSE
562200931112     C                   SETON                                        89
562300931112     C                   ENDIF
562400931112     C   KY              GOTO      FOR03
562500911023     C*
562600131108     C* F3 - FINE LAVORO TORNO A SUBFILE
562700940714     C   KC              MOVEL     '2'           V2CFLG            1
562800911023     C   KC              GOTO      ENDVIS
562900911023     C*
563000131108     C* F12 - FINE LAVORO tolno a videata di riepilogo
563100131108     C   KL              GOTO      emriepil
563200911023     C*
563300911024     C*
563400930517     C* CMD18 - INTERROGA LEGAME BOLLA
563500930518     C   KS              DO
563600930518     C                   EXSR      CMD18
563700940907     C                   MOVEL     'S'           WRIEM
563800930518     C                   GOTO      FOR03
563900930518     C                   END
564000930517     C*
564100940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
564200920804     C                   EXSR      COMAND
564300940905     C     WCMD          IFEQ      'S'
564400940802     C     WF03          IFEQ      *BLANKS
564500070403     c
564600920804     C                   GOTO      FOR03
564700940802     C                   ELSE
564800940802     C                   MOVEL     '2'           V2CFLG
564900940802     C                   GOTO      ENDVIS
565000940802     C                   ENDIF
565100940905     C                   ENDIF
565200911023     C*
565300911023     C* FORMATO4
565400931112     C                   SETOFF                                       89
565500940802     C                   MOVEL     'S'           WRIEM
565600021217      *
565700021217      * Riempio il campone in fondo alla videata con i tasti scelti
565800021217     C                   exsr      Ges_TastiFun
565900021217     c                   eval      FD04 = FDxx
566000021217     c                   eval      F24D04 = F24Dxx
566100911023     C     FOR04         TAG
566200940802     C     WRIEM         IFEQ      'S'
566300131107     C                   WRITE     LRI6T01
566400940802     C                   MOVEL     *BLANKS       WRIEM
566500940802     C                   ENDIF
566600021217      * Imposto la stringa comandi del 'giro' successivo
566700021217     C                   IF        *inKY = *on and $Loop > 1
566800021217     c                   Exsr      Ges_F24
566900021217     c                   eval      FD04 = FDxx
567000021217     c                   eval      F24D04 = F24Dxx
567100021217     C                   ENDIF
567200131107     C                   EXFMT     LRI6D04
567300070420     c                   setoff                                       28
567400070420     c                   clear                   v1cmsg
567500931112      *
567600131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
567700931112     C   KY*IN89         IFEQ      *ON
567800931112     C                   SETOFF                                       89
567900931112     C                   ELSE
568000931112     C                   SETON                                        89
568100931112     C                   ENDIF
568200931112     C   KY              GOTO      FOR04
568300911023     C*
568400131108     C* F12 - RITORNO VIDEATA 3
568500911023     C   KL              GOTO      FOR03
568600920804     C*
568700131108     C* F18 - INTERROGA LEGAME BOLLA
568800930518     C   KS              DO
568900930518     C                   EXSR      CMD18
569000930518     C                   GOTO      FOR04
569100930518     C                   END
569200940923     C*
569300940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
569400131120     c                   eval      wvideo='4'
569500131120     C                   EXSR      CTR_puIn
569600940923     C     WRIEM         CABEQ     'S'           FOR04
569700930517     C*
569800940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
569900920804     C                   EXSR      COMAND
570000940905     C     WCMD          IFEQ      'S'
570100940802     C     WF03          IFEQ      *BLANKS
570200940802     C                   GOTO      FOR04
570300940802     C                   ELSE
570400940802     C                   MOVEL     '2'           V2CFLG
570500940802     C                   GOTO      ENDVIS
570600940802     C                   ENDIF
570700940905     C                   ENDIF
570800911023     C*
570900911023     C* VIDEATA 5
571000931112     C                   SETOFF                                       89
571100940809     C                   MOVEL     'S'           WRIEM
571200021217      *
571300021217      * Riempio il campone in fondo alla videata con i tasti scelti
571400021217     C                   exsr      Ges_TastiFun
571500021217     c                   eval      FD05 = FDxx
571600021217     c                   eval      F24D05 = F24Dxx
571700911023     C     FOR05         TAG
571800940809     C     WRIEM         IFEQ      'S'
571900131107     C                   WRITE     LRI6T01
572000940809     C                   MOVEL     *BLANKS       WRIEM
572100940809     C                   ENDIF
572200951220     C                   Z-ADD     1             REC5
572300021217      *
572400021217      * Imposto la stringa comandi del 'giro' successivo
572500021217     C                   IF        *inKY = *on and $Loop > 1
572600021217     c                   Exsr      Ges_F24
572700021217     c                   eval      FD05 = FDxx
572800021217     c                   eval      F24D05 = F24Dxx
572900021217     C                   ENDIF
573000131107     C                   WRITE     LRI6Z05
573100131107     C                   EXFMT     LRI6C05
573200070420     c                   setoff                                       28
573300070420     c                   clear                   v1cmsg
573400931112      *
573500131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
573600931112     C   KY*IN89         IFEQ      *ON
573700931112     C                   SETOFF                                       89
573800931112     C                   ELSE
573900931112     C                   SETON                                        89
574000931112     C                   ENDIF
574100931112     C   KY              GOTO      FOR05
574200911023     C*
574300131108     C* F12 - RITORNO VIDEATA 2
574400911023     C   KL              GOTO      FOR04
574500920804     C*
574600940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
574700920804     C                   EXSR      COMAND
574800940905     C     WCMD          IFEQ      'S'
574900940802     C     WF03          IFEQ      *BLANKS
575000940802     C                   GOTO      FOR05
575100940802     C                   ELSE
575200131108     C                   MOVEL     '2'           V2CFLG
575300131108     C                   GOTO      ENDVIS
575400940802     C                   ENDIF
575500070406
575600070406     c                   else
575700070406     c
575800070406     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
575900070406     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
576000070406     c                   if        v3cgesd='?'
576100070406     c                   exsr      VISUALAbi
576200070406     c                   clear                   v3cgesd
576300070406     c                   eval      wriem='S'
576400070406     C                   GOTO      FOR05
576500070406     c                   endif
576600940905     C                   ENDIF
576700131108     c
576800131108     C                   GOTO      emriepil
576900931112      *
577000911023     C     ENDVIS        ENDSR
577100070403     C**************************************************************************
577200070403     C*    VISUALIZZA Abilitazioni
577300070403     C**************************************************************************
577400070403     C     VISUALAbi     BEGSR
577500070403     c*
577600131120     c                   clear                   fnlrg7ds
577700131120     c                   move      v3cdsp        ilrg7aas
577800131120     c                   eval      ilrg7lnp=v3clnp
577900131120     c                   eval      ilrg7nrs=v3cnrs
578000131120     c                   eval      ilrg7nsp=v3cnsp
578100131120     c                   eval      ilrg7vsdet='?'
578200131120     c                   eval      ilrg7lung=58
578300131120     c                   call      'FNLRG7R'
578400131120     c                   parm                    fnlrg7ds
578500070403     c
578600070403     C                   ENDSR
578700940712     C**************************************************************************
578800940802     C*    VISUALIZZA BOLLA PRECEDENTE
578900940802     C**************************************************************************
579000930510     C     VISUA2        BEGSR
579100940321      *
579200940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
579300131108     c     Emriepi2      tag
579400940712     C                   EXSR      RIEPI2
579500940712      *
579600131108      * PER F15 PASSO ALLA BOLLA, PER INVIO RITORNO AL SFL
579700940713     C  NKP              GOTO      ENDV8C
579800931112      *
579900931112     C                   SETOFF                                       89
580000940809     C                   MOVEL     'S'           WRIEM
580100021217      *
580200021217      * Riempio il campone in fondo alla videata con i tasti scelti
580300021217     C                   exsr      Ges_TastiFun
580400070516     c                   eval      FD03 = FDxx
580500070516     c                   eval      F24D03 = F24Dxx
580600930510     C     FOR033        TAG
580700940809     C     WRIEM         IFEQ      'S'
580800131107     C                   WRITE     LRI6T01
580900940809     C                   MOVEL     *BLANKS       WRIEM
581000940809     C                   ENDIF
581100021217      *
581200021217      * Imposto la stringa comandi del 'giro' successivo
581300021217     C                   IF        *inKY = *on and $Loop > 1
581400021217     c                   Exsr      Ges_F24
581500021217     c                   eval      FD03 = FDxx
581600021217     c                   eval      F24D03 = F24Dxx
581700021217     C                   ENDIF
581800131107     C                   EXFMT     LRI6D03
581900070420     c                   setoff                                       28
582000070420     c                   clear                   v1cmsg
582100931112      *
582200131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
582300931112     C   KY*IN89         IFEQ      *ON
582400931112     C                   SETOFF                                       89
582500931112     C                   ELSE
582600931112     C                   SETON                                        89
582700931112     C                   ENDIF
582800931112     C   KY              GOTO      FOR033
582900930510     C*
583000131108     C* F12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
583100930510     C   KC
583200131108     COR KL              GOTO      Emriepi2
583300930510     C*
583400940905     C* CONTROLLO SE PREMUTI TASTI FUNZIONALI E, NEL CASO, RIEMETTO
583500930510     C                   EXSR      COMAND
583600940905     C     WCMD          IFEQ      'S'
583700930510     C                   GOTO      FOR033
583800930510     C                   END
583900931112      *
584000930510     C* FORMATO4
584100931112     C                   SETOFF                                       89
584200940809     C                   MOVEL     'S'           WRIEM
584300021217      *
584400021217      * Riempio il campone in fondo alla videata con i tasti scelti
584500021217     C                   exsr      Ges_TastiFun
584600021217     c                   eval      FD04 = FDxx
584700021217     c                   eval      F24D04 = F24Dxx
584800930510     C     FOR044        TAG
584900940809     C     WRIEM         IFEQ      'S'
585000131107     C                   WRITE     LRI6T01
585100940809     C                   MOVEL     *BLANKS       WRIEM
585200940809     C                   ENDIF
585300021217      *
585400021217      * Imposto la stringa comandi del 'giro' successivo
585500021217     C                   IF        *inKY = *on and $Loop > 1
585600021217     c                   Exsr      Ges_F24
585700021217     c                   eval      FD04 = FDxx
585800021217     c                   eval      F24D04 = F24Dxx
585900021217     C                   ENDIF
586000131107     C                   EXFMT     LRI6D04
586100070420     c                   setoff                                       28
586200070420     c                   clear                   v1cmsg
586300931112      *
586400931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
586500931112     C   KY*IN89         IFEQ      *ON
586600931112     C                   SETOFF                                       89
586700931112     C                   ELSE
586800931112     C                   SETON                                        89
586900931112     C                   ENDIF
587000931112     C   KY              GOTO      FOR044
587100930510     C*
587200930510     C* CMD12 - RITORNO VIDEATA 3
587300930510     C   KL              GOTO      FOR033
587400940923     C*
587500940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
587600131120     c                   eval      wvideo='4'
587700131120     C                   EXSR      CTR_puIn
587800940923     C     WRIEM         CABEQ     'S'           FOR044
587900930510     C*
588000940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
588100930510     C                   EXSR      COMAND
588200940905     C     WCMD          IFEQ      'S'
588300930510     C                   GOTO      FOR044
588400940905     C                   ENDIF
588500931112      *
588600930510     C* VIDEATA 5
588700931112     C                   SETOFF                                       89
588800940809     C                   MOVEL     'S'           WRIEM
588900021217      *
589000021217      * Riempio il campone in fondo alla videata con i tasti scelti
589100021217     C                   exsr      Ges_TastiFun
589200021217     c                   eval      FD05 = FDxx
589300021217     c                   eval      F24D05 = F24Dxx
589400930510     C     FOR055        TAG
589500940809     C     WRIEM         IFEQ      'S'
589600131107     C                   WRITE     LRI6T01
589700940809     C                   MOVEL     *BLANKS       WRIEM
589800940809     C                   ENDIF
589900951220     C                   Z-ADD     1             REC5
590000021217      *
590100021217      * Imposto la stringa comandi del 'giro' successivo
590200021217     C                   IF        *inKY = *on and $Loop > 1
590300021217     c                   Exsr      Ges_F24
590400021217     c                   eval      FD05 = FDxx
590500021217     c                   eval      F24D05 = F24Dxx
590600021217     C                   ENDIF
590700131107     C                   WRITE     LRI6Z05
590800131107     C                   EXFMT     LRI6C05
590900070420     c                   setoff                                       28
591000070420     c                   clear                   v1cmsg
591100931112      *
591200931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
591300931112     C   KY*IN89         IFEQ      *ON
591400931112     C                   SETOFF                                       89
591500931112     C                   ELSE
591600931112     C                   SETON                                        89
591700931112     C                   ENDIF
591800931112     C   KY              GOTO      FOR055
591900930510     C*
592000930510     C* CMD12 - RITORNO VIDEATA 2
592100930510     C   KL              GOTO      FOR044
592200930510     C*
592300940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
592400940905     C                   EXSR      COMAND
592500940905     C     WCMD          IFEQ      'S'
592600930510     C                   GOTO      FOR055
592700930510     C                   END
592800131108
592900131108     C                   GOTO      Emriepi2
593000931112      *
593100940713     C     ENDV8C        ENDSR
593200940802     C**************************************************************************
593300940802     C* TASTI DI FUNZIONE
593400940802     C**************************************************************************
593500920804     C     COMAND        BEGSR
593600940905     C*
593700940905     C* PULISCO IL FLAG CHE INDICA SE PREMUTO UN TASTO FUNZIONE
593800940905     C                   MOVE      *BLANKS       WCMD              1
593900040218      *
594000070411      *?F1 - Aggiunta bolla in reclamo?o scelta bolla generica
594100040218if  1c                   if        *inKA   = *on
594200070423     C                   Z-ADD     arbLNP        COMLNP
594300070411     C                   MOVEL     '3'           V2CFLG
594400070411      *?-
594500070411if  2c                   if        SDSprm  > 1
594600131111     c                   eval      wARBlnp = ARBlnp
594700131111     c                   eval      wARBnrs = ARBnrs
594800131111     c                   eval      wARBnsp = ARBnsp
594900131111     c                   eval      wARBaas = ARBaas
595000040218     c                   clear                   FIDNA1ds
595100040218     c                   eval      IA1ins  = 'S'
595200040218     c                   eval      IA1tor  = 'S'
595300040218     c                   eval      IA1ogg  = ds_OggSped
595400070411    2c                   endif
595500040218     c                   goto      Fine
595600040218e   1c                   endif
595700940802     C*
595800990119     C* F5 - INTERROGAZIONE EVENTI
595900040218do  1C   KE              DO
596000940905     C                   MOVEL     'S'           WCMD
596100940726     C                   MOVEL     V2CAAS        PB5AAS
596200940726     C                   MOVEL     COMLNP        PB5LNP
596300940726     C                   MOVEL     V2CNRS        PB5NRS
596400940726     C                   MOVEL     V2CNSP        PB5NSP
596500940726     C                   MOVEL     *BLANKS       PB5F03
596600940726     C                   MOVEL     *BLANKS       PB5FLG
596700940802     C                   MOVEL     *BLANKS       PB5PGM
596800131107     C                   MOVEL     'FNLRI6R'     PB5PGM
596900940726     C                   MOVEL     V1CDSA        PB5DSA
597000940726     C                   MOVEL     V3CDSP        PB5DSP
597100940726     C                   MOVEL     VIDRMN        PB5RMN
597200940726     C                   MOVEL     V3CLNA        PB5LNA
597300940802     C                   MOVEL     V3DLNA        PB5LN1
597400940726     C                   MOVEL     V4CRSD        PB5RSD
597500940726     C                   MOVEL     VIDDCP        PB5DCP
597600941031     C                   MOVEL     VIDTMC        PB5TMC
597700940726     C                   MOVEL     PARAB5        KPJBU
597800940722     C                   CALL      'FNLRB5R'
597900930525     C                   PARM                    KPJBA
598000941109     C                   MOVEL     KPJBU         PARAB5
598100941109     C                   MOVEL     PB5F03        WF03
598200941214     C                   MOVEL     '1'           PA0B5
598300940725     C                   MOVEL     'S'           WRIEM
598400040218e   1C                   ENDDO
598500070420      *?F6 - Stampa LDV
598600070420if  1c                   if        *inKf = *on
598700070420     C                   MOVEL     'S'           WCMD
598800080201     c* Se assegnato non abilitato non stampo la LdV ma emetto errore
598900080201     c                   if        arbaca=' '
599000080201     C                   MOVEL     MSG(31)       V1CMSG
599100080201     C                   SETON                                        28
599200080201     c                   else
599300070420     c
599400070420     c                   if        wrichtrul90=' '
599500070420     c* La prima volta devo proporre la richiesta stampanti
599600070420     c                   exsr      RICHSTAMPAN
599700070420     C                   MOVEL     'S'           WRIEM
599800070420     c
599900070420     c* Se premuto F3 esco senza fare nulla
600000070420     c                   if        d90f3<>'1' and not *in91
600100070420     c                   eval      wrichtrul90='S'
600200070420     c                   endif
600300070420     c                   if        *in91
600400070420     C                   MOVEL     MSG(30)       V1CMSG
600500070420     C                   SETON                                        28
600600070420     c                   endif
600700070420     c                   endif
600800070420     c
600900070420     c                   if        wrichtrul90='S'
601000070420     C                   CLEAR                   FNLSB5DS
601100070426     c                   movel     'L'           db0tla
601200070420     C                   MOVEL     'R'           DB0RIS
601300070426     C                   MOVEL     'A'           DB0TBO
601400070420     C                   MOVEL     ARBAAS        DB0AAS
601500070420     C                   MOVEL     ARBLNP        DB0LNP
601600070420     C                   MOVEL     ARBNRS        DB0NRS
601700070420     C                   MOVEL     ARBNSP        DB0NSP
601800130319     c                   reset                   fnlsB6ds1
601900070420     C                   CALL      D90PSL
602000070420     C                   PARM                    FNLSB5DS
602100130308     c                   parm                    fnlsB6ds1
602200070420     C                   MOVEL     MSG(29)       V1CMSG
602300070420     C                   SETON                                        28
602400070420     c                   endif
602500080201     c                   endif
602600070420     c                   endif
602700930525     C*
602800990119     C* F7 - ANOMALIE
602900040218do  1C   KG              DO
603000960523     C                   MOVEL     'S'           WCMD
603100960523     C*
603200991027     C                   CLEAR                   FNLr44DS
603300960523     C                   MOVE      V2CAAS        D44AAS
603400960523     C                   MOVE      COMLNP        D44FIL
603500960523     C                   MOVE      V2CNRS        D44NRS
603600960523     C                   MOVE      V2CNSP        D44NSP
603700960523     C                   MOVEL     'A'           D44TBO
603800960523     C                   MOVE      'I'           D44MOD
603900991027     C                   MOVEL     FNLr44DS      KPJBU
604000960523     C                   CALL      'FNLR44R'
604100960523     C                   PARM                    KPJBA
604200960523     C*
604300990119     C                   MOVEL     'S'           WRIEM
604400040218e   1C                   ENDDO
604500920804     C*
604600990119     C* F8  - INTERROGAZIONE EVENTI SEGNACOLLI
604700040218if  1C     *INKH         IFEQ      *ON
604800961121     C                   MOVEL     'S'           WCMD
604900961121     C*
605000991027     C                   CLEAR                   FNLR45DS
605100961121     C                   MOVE      V2CAAS        D45AAS
605200961121     C                   MOVE      COMLNP        D45FIL
605300961121     C                   MOVE      V2CNRS        D45NRS
605400961121     C                   MOVE      V2CNSP        D45NSP
605500991027     C                   MOVEL     FNLR45DS      KPJBU
605600961121     C                   CALL      'FNLR45R'
605700961121     C                   PARM                    KPJBA
605800961121     C*
605900990119     C                   MOVEL     'S'           WRIEM
606000040218e   1C                   ENDIF
606100130830     c
606200130830     c* F9 - Orari servizi
606300131007     c                   if        *inki=*on
606400130830     C                   MOVEL     'S'           WCMD
606500131007     c                   if        wabif9='S'
606600130930     c                   exsr      sr_orarisr
606700131007     c                   endif
606800130830     C                   MOVEL     'S'           WRIEM
606900130830e   1C                   ENDIF
607000021213     C*
607100021213     C* F11 - Note supermercati
607200040218if  1C     *INKK         IFEQ      *ON
607300021213     C     *IN77         ANDEQ     *ON
607400021213      * imposto i campi 'bolla' della ds
607500021217     C                   MOVEL     'S'           WCMD
607600021213     C                   clear                   FnLrb3ds
607700030122     c                   Movel     V1cdsa        Rb3Rsu
607800030121     C                   z-add     SimFel        RB3Fil
607900021216     C                   z-add     V3CAAS        RB3AAS
608000030121     C                   z-add     V3CLNP        RB3LNP
608100021216     C                   z-add     V3CNRS        RB3NRS
608200021216     C                   z-add     V3CNSP        RB3NSP
608300021213     C                   z-add     V3CDSP        RB3DSP
608400021213     C                   z-add     VIDRMN        RB3RMN
608500021213     C                   z-add     V3CLNA        RB3LNA
608600131108     C                   movel     V32RSD        RB3RSD
608700021218     C                   z-add     VIDPKF        RB3PKF
608800021213     C                   z-add     VIDNCL        RB3NCL
608900021213     C                   z-add     VIDVLF        RB3VLB
609000030703      * bancali - part.varie "B"
609100040218If  2c                   If        %subst(ArbGva:1:1) = 'B'
609200030121     C                   z-add     §Ar5Tba       RB3Tba
609300030121     C                   z-add     VidNba        RB3Nba
609400030121     C                   z-add     §Ar5Tb2       RB3Tb2
609500030121     C                   z-add     VidNb2        RB3Nb2
609600030703     c                   Movel     V4cGva        Rb3Gva
609700040218e   2c                   EndIf
609800030703      * bancali - part.varie "O"
609900040218If  2c                   If        %subst(ArbGva:1:1) = 'O'
610000030703     C                   z-add     §Ar5bTba      RB3Tba
610100030703     C                   z-add     VidNba        RB3Nba
610200030703     C                   z-add     §Ar5bTb2      RB3Tb2
610300030703     C                   z-add     VidNb2        RB3Nb2
610400030703     c                   Movel     V4cGva        Rb3Gva
610500030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
610600030703     c                   Z-Add     §Ar5bCor      Rb3Cor
610700040218e   2c                   EndIf
610800021213      * richiamo PGM per note supermercati
610900021213     C                   call      'FNLRB3R'
611000030122     C                   PARM                    Kpjba
611100030121     C                   PARM                    Fnlrb3Ds
611200021213      *
611300021217     C                   MOVEL     'S'           WRIEM
611400040218e   1C                   ENDIF
611500961121     C*
611600990119     C* F14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
611700040218if  1C     *INKN         IFEQ      *ON
611800930624     C     *IN03         ANDEQ     *ON
611900941213     C     PA1GIA        ANDEQ     *BLANK
612000940905     C                   MOVEL     'S'           WCMD
612100991027     C                   CLEAR                   FNLG30DS
612200941213     C                   MOVEL     'A'           D30POA
612300941213     C                   MOVEL     V2CAAS        D30AAS
612400941213     C                   MOVEL     COMLNP        D30LNP
612500941213     C                   MOVEL     V2CNRS        D30NRS
612600941213     C                   MOVEL     V2CNSP        D30NSP
612700991027     C                   MOVEL     FNLG30DS      KPJBU
612800941213     C                   CALL      'FNLG31C'
612900920804     C                   PARM                    KPJBA
613000930624     C*
613100990119     C                   MOVEL     'S'           WRIEM
613200040218e   1C                   ENDIF
613300040218      *
613400040218      *?F16 - Gestione reclami?
613500040218if  1c                   if        *inKQ = *on
613600040225     c                   eval      wARBaas = ARBaas
613700131111     c                   eval      wARBlnp = ARBlnp
613800131111     c                   eval      wARBnrs = ARBnrs
613900131111     c                   eval      wARBnsp = ARBnsp
614000040225     c                   reset                   FIDNA2ds
614100040225if  2c                   if        SDSprm  > 1
614200040225     c                   eval      IA2mod  = 'I'
614300040225e   2c                   endif
614400040225     c                   eval      IA2ogg  = ds_OggSped
614500040225     c                   call      'FIDNA2R'
614600040225     c                   parm                    KPJBA
614700040225     c                   parm                    FIDNA2ds
614800040225     c                   movel     OA2f03        WF03
614900040225     c                   movel     'S'           WRIEM
615000051201     C                   MOVEL     'S'           WCMD
615100040218e   1c                   endif
615200070404      *
615300140311      *?F17 - Note AUT
615400070404if  1c                   if        *inKR = *on
615500140123     c                   clear                   fnlrn6ds
615600140123     c                   eval      in6mod='V'
615700140123     c                   eval      in6aas=v2caas
615800140123     c                   eval      in6lnp=comlnp
615900140123     c                   eval      in6nrs=v2cnrs
616000140123     c                   eval      in6nsp=v2cnsp
616100140123      * se sono una spedizione in consegna passo i dati della distinta del file ARB
616200140123     c                   IF        ARBfbc = *blanks and
616300140123     c                               ARBddc > 0 and
616400140123     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
616500140123     c                               arbica='R' or arbicc=' ' or arbicc='R')
616600140123     c     KSPED         chain     Fnarb01l
616700140123     c                   If        %found(Fnarb01l)
616800140123     c                   eval      in6fgs = arbifp
616900140123     c                   eval      in6nfv = arbndc
617000140123     c                   eval      in6dfv = arbddc
617100140123     c                   eval      in6pdr = arbpdc
617200140123     c                   eval      in6cons = 'S'
617300140123     c                   Endif
617400140123     c                   Endif
617500140123     c                   call      'FNLRN6R'
617600070404     c                   parm                    kpjba
617700140123     c                   parm                    fnlrn6ds
617800070404     c                   movel     'S'           WRIEM
617900070404     C                   MOVEL     'S'           WCMD
618000070404e   1c                   endif
618100920804     C*
618200990119     C* F21 - DETTAGLIO SEGNACOLLI
618300040218do  1C   KV              DO
618400940905     C                   MOVEL     'S'           WCMD
618500950628     C                   MOVEL     'A'           PA1TBO
618600950629     C                   CLEAR                   PA1F03
618700940720     C                   MOVEL     PARAM1        KPJBU
618800940726     C                   CALL      'FNLR37R'
618900920804     C                   PARM                    KPJBA
619000940802     C                   MOVEL     KPJBU         PARAM1
619100940802     C                   MOVEL     PA1F03        WF03
619200990119     C                   MOVEL     'S'           WRIEM
619300040218e   1C                   ENDDO
619400920804     C*
619500990119     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
619600040218do  1C   KJ              DO
619700940905     C                   MOVEL     'S'           WCMD
619800950628     C                   MOVEL     'A'           PA1TBO
619900950629     C                   CLEAR                   PA1F03
620000940712     C                   MOVEL     PARAM1        KPJBU
620100940726     C                   CALL      'FNLR75R'
620200920804     C                   PARM                    KPJBA
620300941214     C                   MOVEL     '1'           PA075
620400940802     C                   MOVEL     KPJBU         PARAM1
620500940802     C                   MOVEL     PA1F03        WF03
620600990119     C                   MOVEL     'S'           WRIEM
620700040218e   1C                   ENDDO
620800930114     C*
620900990119     C* F19 - INTERROGA SPUNTE VARI
621000040218if  1C     *INKT         IFEQ      *ON
621100940905     C                   MOVEL     'S'           WCMD
621200940713     C                   Z-ADD     0             PA4NFV
621300940713     C                   MOVEL     9             PA4NPG
621400940713     C                   MOVEL     99            PA4NPS
621500101202     C                   MOVEL     pa1pgm        PA4PGM
621600101203     C                   MOVEL     '1'           PA4FLG
621700950214     C                   MOVEL     V3CFLS        PA4LNP
621800940719     C                   MOVEL     V3CLNA        PA4LNA
621900940719     C                   MOVEL     V3CNRS        PA4NRS
622000940713     C                   Z-ADD     0             PA4NCD
622100940713     C                   Z-ADD     0             PA4NCA
622200101201     c                   eval      pa4boan=v2caas
622300101201     c                   eval      pa4bolp=comlnp
622400101201     c                   eval      pa4bors=v2cnrs
622500101201     c                   eval      pa4bosp=v2cnsp
622600040218if  2C     VIDNCD        IFGT      0
622700940713     C                   MOVEL     VIDNCD        PA4NCD
622800940713     C                   MOVEL     VIDNCA        PA4NCA
622900101201x   2C**                 ELSE
623000930705     C* CERCO I SEGNACOLLI DAL DETTAGLIO
623100101201     C**   KARB2         SETLL     FNART01L
623200101201     C**   KARB2         READE     FNART01L                               31
623300101201do  3C*+N31              DO
623400101201     C**                 Z-ADD     ARTNSC        PA4NCD
623500101201     C**   KARB2         SETGT     FNART01L
623600101201     C**   KARB2         READPE    FNART01L                               31
623700101201if  4C**N31ARTNSC        IFNE      PA4NCD
623800101201     C**                 MOVEL     ARTNSC        PA4NCA
623900101201e   4C**                 ENDIF
624000101201e   3C**                 ENDDO
624100101201e   2C                   ENDIF
624200930705     C*
624300940802     C                   MOVEL     PARA04        KPJBU
624400940722     C                   CALL      'FNLR04R'
624500920804     C                   PARM                    KPJBA
624600940725     C                   MOVEL     'S'           WRIEM
624700060130     C                   MOVEL     'S'           WCMD
624800040218e   1C                   ENDIF
624900930415     C*
625000990119     C* F23 - VISUALIZZA DATA CONSEGNA PREVISTA ESTERNA E ORE RITARDO
625100131121do  1C** KX              DO
625200131121     C**                 MOVEL     'S'           WCMD
625300131121     C**                 SETOFF                                       68
625400930504     C*
625500131121     C**                 MOVEL     ' '           D98TLA
625600131121     C**                 MOVEL     'A'           D98TBO
625700131121     C**                 MOVEL     ARBAAS        D98AAS
625800131121     C**                 MOVEL     ARBLNP        D98LNP
625900131121     C**                 MOVEL     ARBNRS        D98NRS
626000131121     C**                 MOVEL     ARBNSP        D98NSP
626100930504     C*
626200131121     C**                 CALL      'TNSD99R'
626300131121     C**                 PARM                    TNSD99DS
626400930504     C*
626500131121     c**                 if        d98dce>0
626600131121     c**   *iso          movel     d98dce        dataeur
626700131121     c**                 movel     dataeur       datadmy
626800131121     c**                 movel     dataeur       v08dce
626900131121     c**                 movel     datadmy       v06dce
627000131121     c**                 else
627100131121     c**                 clear                   v08dce
627200131121     c**                 clear                   v06dce
627300131121     c**                 endif
627400070404     c
627500131121     C**                 Z-ADD     D98NRC        VIDNRC
627600930526     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
627700930526     C*   CONSEGNA PREVISTA
627800131121     c**n69              eval      vidhmc=%editw(d98hce: '0 :  ')
627900930504     C*
628000930504     C* SE COMODO = 1  E' STATA FATTA CALL PGM
628100131121     C**                 MOVEL     '1'           PA098
628200131121e   1C**                 ENDDO
628300931112      *
628400990119      * F20 - INTERROGAZIONE AFFIDABILITA'
628500040218do  1C   KU              DO
628600940905     C                   MOVEL     'S'           WCMD
628700950102     C                   CLEAR                   FNLR97
628800950102     C                   MOVEL     V2CAAS        R97AAS
628900950102     C                   MOVEL     COMLNP        R97LNP
629000950102     C                   MOVEL     V2CNRS        R97NRS
629100950102     C                   MOVEL     V2CNSP        R97NSP
629200950102     C                   MOVEL     V3CDSP        R97DSP
629300950102     C                   MOVEL     VIDRMN        R97RMN
629400950102     C                   MOVEL     V3CLNA        R97LNA
629500950102     C                   MOVEL     V3DLNA        R97DLA
629600950102     C                   Z-ADD     VIDDCP        R97DCP
629700950102     C                   MOVEL     V4CRSD        R97RSD
629800950102     C                   MOVEL     V1CDSA        R97RSU
629900040121     C                   MOVEL     *blanks       R97CML
630000950102     C                   MOVEL     'A'           R97AP
630100950102     C   95              MOVEL     'S'           R97EVE
630200070404     C                   Z-ADD     w08DBR        R97DBR
630300070404     C                   Z-ADD     w08DPC        R97DPC
630400070404     C                   Z-ADD     w08DUC        R97DUC
630500950102     C                   MOVEL     VIDTMC        R97TMC
630600950102     C                   Z-ADD     VIDCAS        R97CAS
630700950102     C                   MOVEL     VIDVCA        R97VCA
630800931112     C                   MOVE      *BLANKS       KPJBU
630900950102     C                   MOVEL     FNLR97        KPJBU
631000940722     C                   CALL      'FNLR97R'                            90
631100931112     C                   PARM                    KPJBA
631200950102     C                   MOVEL     KPJBU         FNLR97
631300950102     C                   MOVEL     R97F03        WF03
631400950102     C                   MOVEL     'S'           WRIEM
631500040218e   1C                   ENDDO
631600110930     C* F22 - Perfect Order
631700110929do  1C                   if        *inkw
631800110929     C                   MOVEL     'S'           WCMD
631900110929     c                   movel     param1        s_param1
632000110929     c*
632100110929     c                   MOVEL     'W'           pa1flg
632200131107     c                   MOVEL(p)  'FNLRI6R'     pa1pgm
632300111005     c                   movel(p)  vidrma        pa1rma
632400111005     c                   movel(p)  vidxco        pa1xco
632500110929     c                   movel     paraaz        kpjbu
632600110929     c                   movel     param1        kpjbu
632700131127     c                   call      'FNLR36C'
632800110929     c                   parm                    kpjba
632900110929     c*
633000110929     c                   movel     s_param1      param1
633100110929     C*
633200110929     C                   MOVEL     'S'           WRIEM
633300110929e   1C                   ENDif
633400940905     C*
633500131107     C* ? FORMATO VIDEO:  LRI6D06  (TASSAZIONE 1a BOLLA)   ---   TASTO F13 ?
633600991026     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
633700040218if  1C     *INKM         IFEQ      *ON
633800991027     C                   eval      WCMD = 'S'
633900991027     C                   EVAL      *in89 = *off
634000131107     C                   WRITE     LRI6T01
634100131107     C                   WRITE     LRI6D06
634200991027     C                   eval      NR6 = 1
634300131107     C                   EXFMT     LRI6C06
634400040218e   1C                   ENDIF
634500991027     C*
634600131107     C* ? FORMATO VIDEO:  LRI6D09  (TASSAZIONE 2a BOLLA)   ---   TASTO F04 ?
634700991027     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
634800040218if  1C     *INKD         IFEQ      *ON
634900991027     C                   eval      WCMD = 'S'
635000991027     C                   EVAL      *in89 = *off
635100131107     C                   WRITE     LRI6T01
635200131107     C                   EXFMT     LRI6D09
635300040218e   1C                   ENDIF
635400990119     C*
635500990119     C* F2 - INTERROGAZIONE COMUNICAZIONE ANOMALIA
635600990129     C*      NON RICHIAMO SE RICHIAMATO DA INTERROGAZINE C.A.
635700040218if  1C     *INKB         IFEQ      *ON
635800990129     C     PA1FLG        ANDNE     'C'
635900040218if  2C     CONTCA        IFEQ      1
636000991027     C                   CLEAR                   FIDN00DS
636100990119     C                   Z-ADD     VHCAAC        I00AAC
636200990119     C                   Z-ADD     VHCFIL        I00FIL
636300990119     C                   Z-ADD     VHCNCA        I00NCA
636400990119     C                   Z-ADD     ARBAAS        I00AAS
636500990119     C                   Z-ADD     ARBNRS        I00NRS
636600990119     C                   Z-ADD     ARBNSP        I00NSP
636700990119     C                   Z-ADD     ARBLNP        I00LNP
636800990119     C                   MOVEL     'A'           I00TPB
636900991027     C                   MOVEL     FIDN00DS      KPJBU
637000990119     C                   CALL      'FIDN01R'
637100990119     C                   PARM                    KPJBA
637200000309     C                   PARM                    callricors        1
637300040218x   2C                   ELSE
637400990119     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
637500990119     C                   SETOFF                                       79
637600131107     C                   WRITE     LRI6C11
637700990119     C                   SETON                                        797823
637800990119     C                   CLEAR                   NRR11
637900040809      *
638000040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
638100040809      * legate alla spedizione che passo
638200040809     c                   clear                   fidn12ds
638300040809     c                   eval      i12aas = arbAAS
638400040809     c                   eval      i12lnp = arbLNP
638500040809     c                   eval      i12nrs = arbNRS
638600040809     c                   eval      i12nsp = arbNSP
638700040809     c                   eval      i12fel = 'E'
638800040809     c                   eval      i12fan = 'E'
638900040809     c*
639000040809     c                   call      'FIDN12R'
639100040809     c                   parm                    fidn12ds
639200040809     c*
639300040809     c                   setoff                                       31
639400040809     c*
639500040809     c* se non ci sono errori
639600040809     c                   if        o12err <> ' '
639700040809     c                   seton                                        31
639800040809     c                   else
639900040809     c* se numero di CA trovate maggiore di zero
640000040809     c                   if        o12nca = 0
640100040809     c                   seton                                        31
640200040809     c                   else
640300040809     c                   z-add     1             ii                2 0
640400040809     c                   dow       ii <= o12nca and *in31 = *off
640500040809     c                   movea     skkey(ii)     dskey
640600040809     C                   Z-ADD     dsaac         V11AAC
640700040809     C                   Z-ADD     dsfil         V11FIL
640800040809     C                   Z-ADD     dsnca         V11NCA
640900040809     C                   Z-ADD     skdca(ii)     G02INV
641000040809     C                   Z-ADD     *ZERO         G02DAT
641100040809     C                   MOVEL     '3'           G02ERR
641200040809     C                   CALL      'XSRDA8'
641300040809     C                   PARM                    WLBDAT
641400040809     C                   Z-ADD     G02DAT        V11DCA
641500040809     C                   Z-ADD     skfca(ii)     V11FCA
641600040809     C     skfca(ii)     CHAIN     FNDFA01L
641700040809     C                   IF        %found(FNDFA01L)
641800040809     C                   MOVEL     DFADES        V11DFA
641900040809     C                   ENDIF
642000040809     C**
642100040809     C                   ADD       1             NRR11             4 0
642200131107     C                   WRITE     LRI6S11
642300040809     c*
642400040809     c                   add       1             ii
642500040809     c                   enddo
642600040809     c*
642700040809     c                   seton                                        31
642800040809     c*
642900040809     c                   endif
643000040809     c                   endif
643100040809     c**
643200990119     C* EMETTO FORMATO
643300990119     C     FOR11         TAG
643400990119     C                   Z-ADD     1             NRR11
643500131107     C                   WRITE     LRI6T01
643600131107     C                   WRITE     LRI6Z11
643700131107     C                   EXFMT     LRI6C11
643800990119     C* F12 - RITORNO
643900040218if  3C     *INKL         IFEQ      *OFF
644000131107     C                   READC     LRI6S11                                31
644100040218do  4C     *IN31         DOWEQ     *OFF
644200040218if  5C     V11SCE        IFEQ      '5'
644300991027     C                   CLEAR                   FIDN00DS
644400990119     C                   Z-ADD     V11AAC        I00AAC
644500990119     C                   Z-ADD     V11FIL        I00FIL
644600990119     C                   Z-ADD     V11NCA        I00NCA
644700990119     C                   Z-ADD     ARBAAS        I00AAS
644800990119     C                   Z-ADD     ARBNRS        I00NRS
644900990119     C                   Z-ADD     ARBNSP        I00NSP
645000990119     C                   Z-ADD     ARBLNP        I00LNP
645100990119     C                   MOVEL     'A'           I00TPB
645200991027     C                   MOVEL     FIDN00DS      KPJBU
645300990119     C                   CALL      'FIDN01R'
645400990119     C                   PARM                    KPJBA
645500000309     C                   PARM                    callricors        1
645600990119     C**
645700990119     C                   CLEAR                   V11SCE
645800131107     C                   UPDATE    LRI6S11
645900040218e   5C                   ENDIF
646000990119     C**
646100131107     C                   READC     LRI6S11                                31
646200040218e   4C                   ENDDO
646300990119     C                   GOTO      FOR11
646400040218e   3C                   ENDIF
646500040218e   2C                   ENDIF
646600990119     C*
646700990119     C                   MOVEL     'S'           WRIEM
646800990119     C                   MOVEL     'S'           WCMD
646900040218e   1C                   ENDIF
647000931112      *
647100920804     C                   ENDSR
647200070420     C**************************************************************************
647300070420      * RICHIEDO LE STAMPANTI per stampa LDV
647400070420     C**************************************************************************
647500070420     c     RICHSTAMPAN   BEGSR
647600070420     C                   CLEAR                   TRUL90DS
647700070420      *
647800070420     C                   MOVEL     'S'           d90RSB
647900070420     C                   CALL      'TRUL90R'
648000070420     C                   PARM                    KPJBA
648100070420     C                   PARM                    trul90DS
648200070420      *
648300070420     C* F3 - FINE
648400070420     c                   if        d90f3<>'1'
648500070420      *
648600070420      * OVRPRTF BOLLE A4
648700070426     c                   if        *in97
648800070420     C                   MOVEA     D90PRB4       CMA4(30)
648900070420     C                   MOVEA     D90MDB4       CMA4(52)
649000070426     c                   else
649100070426     C                   MOVEA     D90PRB4       CMA4f6(30)
649200070426     C                   MOVEA     D90MDB4       CMA4f6(52)
649300070426     c                   endif
649400070420     C*
649500111221     C                   Z-ADD     130           LUNG             15 5
649600111221     C                   CLEAR                   COMMAN          130
649700070426     C   97              MOVEA     CMA4          COMMAN
649800070426     C  n97              MOVEA     CMA4f6        COMMAN
649900070420     C                   CALL      'QCMDEXC'                            91
650000070420     C                   PARM                    COMMAN
650100070420     C                   PARM                    LUNG
650200070420      *
650300070420     C                   if        not *in91
650400070420      *
650500070420      * OVRPRTF BOLLE A5
650600070426     c                   if        *in97
650700070420     C                   MOVEA     D90PRB5       CMA5(30)
650800070420     C                   MOVEA     D90MDB5       CMA5(52)
650900070426     c                   else
651000070426     C                   MOVEA     D90PRB5       CMA5f6(30)
651100070426     C                   MOVEA     D90MDB5       CMA5f6(52)
651200070426     c                   endif
651300070420     C*
651400111221     C                   Z-ADD     130           LUNG
651500070420     C                   CLEAR                   COMMAN
651600070426     C   97              MOVEA     CMA5          COMMAN
651700070426     C  n97              MOVEA     CMA5f6        COMMAN
651800070420     C                   CALL      'QCMDEXC'                            91
651900070420     C                   PARM                    COMMAN
652000070420     C                   PARM                    LUNG
652100070420     c                   endif
652200070420     c                   endif
652300070420      *
652400070420     c                   ENDSR
652500940802     C**************************************************************************
652600940802     C*    VISUALIZZA LEGAMI BOLLA
652700940802     C**************************************************************************
652800930413     C     CMD18         BEGSR
652900940802     C*
653000940411     C                   CLEAR                   WCAR
653100940411     C*
653200940411     C* MUOVO I DATI DELLA BOLLA CHE STO VISUALIZZANDO
653300940714     C                   MOVEL     V2CAAS        NUM2              4 0
653400940411     C                   MOVEL     COMLNP        COMLPN
653500940714     C                   MOVEL     V2CNRS        COMNRN
653600940714     C                   MOVEL     V2CNSP        COMNSN
653700950321     C                   MOVEL     V3CLNA        COMLAN
653800950428     C                   MOVEL     COMC08        SAVC08
653900940408     C**
654000940713     C* SAVNR8 > 1 --> GESTIONE SUBFILE
654100940408     C**
654200940907    1C     SAVNR8        IFGT      2
654300940408     C*
654400940809     C                   MOVEL     'S'           WRIEM
654500930413     C     FORC2         TAG
654600940713     C                   Z-ADD     1             REC8
654700940809     C     WRIEM         IFEQ      'S'
654800131107     C                   WRITE     LRI6T01
654900131107     C                   WRITE     LRI6Z08
655000940809     C                   MOVEL     *BLANKS       WRIEM
655100940809     C                   ENDIF
655200950428     C*
655300950428     C                   MOVEL     SAVC08        COMC08
655400131107     C                   EXFMT     LRI6C08
655500930413     C*
655600930413     C* CMD12 - FINE LAVORO
655700930413     C   KL              GOTO      SOTTO
655800930413     C*
655900131107     C                   READC     LRI6S08                                93
656000930510     C   93              GOTO      SOTTO
656100930510     C*
656200940408    2C     *IN93         DOWEQ     '0'
656300930413     C*
656400941230    3C     V8CSCE        IFEQ      '5'
656500940713     C                   MOVEL     ' '           V8CSCE
656600950103     C*
656700950103     C* RIPRISTINO GLI INDICATORI
656800950103     C                   MOVEL     V8CI80        *IN80
656900950103     C                   MOVEL     V8CI81        *IN81
657000950103     C                   MOVEL     V8CI82        *IN82
657100950103     C                   MOVEL     V8CI83        *IN83
657200131107     C                   UPDATE    LRI6S08
657300930504     C*
657400940907     C     V8CAAN        IFEQ      *ZERO
657500940907     C                   MOVEL     V8CAAP        LBLAAN
657600940907     C                   MOVEL     V8CLPP        LBLLPN
657700940907     C                   MOVEL     V8CNRP        LBLNRN
657800940907     C                   MOVEL     V8CNSP        LBLNSN
657900940907     C                   MOVEL     'P'           V8CFPS            1
658000940907     C                   ELSE
658100940907     C                   MOVEL     V8CAAN        LBLAAN
658200940907     C                   MOVEL     V8CLPN        LBLLPN
658300940907     C                   MOVEL     V8CNRN        LBLNRN
658400940907     C                   MOVEL     V8CNSN        LBLNSN
658500940907     C                   MOVEL     'S'           V8CFPS
658600940907     C                   ENDIF
658700940408     C*
658800940408     C* CONTROLLO SE IL LEGAME E' IN PARTENZA O IN ARRIVO
658900940907     C     KLBL1         CHAIN     FNARB01L                           93
659000940408     C*
659100940408    4C     *IN93         IFEQ      *OFF
659200940408     C*
659300940408     C                   EXSR      LEG2
659400940408     C*
659500940408   X4C                   ELSE
659600940408     C*
659700950316     C                   Z-ADD     LBLAAN        PA0AAS
659800950316     C                   Z-ADD     LBLLPN        PA0LNP
659900950316     C                   Z-ADD     LBLNRN        PA0NRS
660000950428     C                   Z-ADD     LBLNSN        PA0NSP
660100060130     C                   MOVEL     rsut          PA0RSU
660200941214     C                   MOVEL     SIMFEL        PA0SIM
660300040121     C                   MOVEL     *blanks       PA0CML
660400960903     C     PA1FLG        IFEQ      'N'
660500960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
660600960903     C*       verifichi questo caso
660700960903     C                   MOVEL     'R'           PA0FLG
660800960903     C                   ELSE
660900941214     C                   MOVEL     'A'           PA0FLG
661000960903     C                   END
661100941214     C                   MOVEL     *BLANKS       PA0CLI
661200950629     C                   CLEAR                   PA0F03
661300941214     C                   MOVEL     *BLANKS       PA0GIA
661400941214     C                   MOVEL     PARAM0        KPJBU
661500940722     C                   CALL      'FNLS05R'
661600930413     C                   PARM                    KPJBA
661700941214     C                   MOVEL     KPJBU         PARAM0
661800941214     C                   MOVEL     PA0F03        WF03
661900940907     C                   MOVEL     'S'           WRIEM
662000940408    4C                   ENDIF
662100940408     C*
662200940408    3C                   ENDIF
662300930413     C*
662400131107     C                   READC     LRI6S08                                93
662500940408    2C                   ENDDO
662600930413     C                   GOTO      FORC2
662700930413     C*
662800930413     C     SOTTO         TAG
662900930413     C*
663000940408    1C                   ELSE
663100940408     C*
663200940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
663300131107     C     2             CHAIN     LRI6S08                            99
663400940907     C     V8CAAP        IFEQ      V3CAAS
663500940907     C     V8CLPP        ANDEQ     V3CLNP
663600940907     C     V8CNRP        ANDEQ     V3CNRS
663700940907     C     V8CNSP        ANDEQ     V3CNSP
663800940907     C                   MOVEL     V8CAAN        ARBAAS
663900940907     C                   MOVEL     V8CLPN        ARBLNP
664000940907     C                   MOVEL     V8CNRN        ARBNRS
664100940907     C                   MOVEL     V8CNSN        ARBNSP
664200940907     C                   MOVEL     'S'           V8CFPS
664300940907     C                   ELSE
664400940907     C                   MOVEL     V8CAAP        ARBAAS
664500940907     C                   MOVEL     V8CLPP        ARBLNP
664600940907     C                   MOVEL     V8CNRP        ARBNRS
664700940907     C                   MOVEL     V8CNSP        ARBNSP
664800940907     C                   MOVEL     'P'           V8CFPS
664900940907     C                   ENDIF
665000940907     C* QUINDI VEDO SE LA TROVO FRA GLI ARRIVI
665100940907     C     KARB2         CHAIN     FNARB01L                           93
665200940408     C*
665300121116    2C     *IN93         IFEQ      *ON
665400950316     C                   Z-ADD     ARBAAS        PA0AAS
665500950316     C                   Z-ADD     ARBLNP        PA0LNP
665600950316     C                   Z-ADD     ARBNRS        PA0NRS
665700950316     C                   Z-ADD     ARBNSP        PA0NSP
665800060130     C                   MOVEL     rsut          PA0RSU
665900941214     C                   MOVEL     SIMFEL        PA0SIM
666000040121     C                   MOVEL     *blanks       PA0CML
666100960903     C     PA1FLG        IFEQ      'N'
666200960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
666300960903     C*       verifichi questo caso
666400960903     C                   MOVEL     'R'           PA0FLG
666500960903     C                   ELSE
666600941214     C                   MOVEL     'A'           PA0FLG
666700960903     C                   END
666800941214     C                   MOVEL     *BLANKS       PA0CLI
666900950629     C                   CLEAR                   PA0F03
667000941214     C                   MOVEL     *BLANKS       PA0GIA
667100941214     C                   MOVEL     PARAM0        KPJBU
667200940722     C                   CALL      'FNLS05R'
667300930413     C                   PARM                    KPJBA
667400941214     C                   MOVEL     KPJBU         PARAM0
667500941214     C                   MOVEL     PA0F03        WF03
667600940907     C                   MOVEL     'S'           WRIEM
667700121116     c* reimposto la key spedizione
667800121116     C                   MOVEL     NUM2          arbAAS
667900121116     C                   MOVEL     COMLPN        arbLNP
668000121116     C                   MOVEL     COMNRN        arbNRS
668100121116     C                   MOVEL     COMNSN        arbNSP
668200930510     C*
668300940408   X2C                   ELSE
668400940408     C*
668500940408     C                   EXSR      LEG2
668600940408     C*
668700940408    2C                   END
668800940408    1C                   END
668900940411     C*
669000940411     C* RICARICO I DATI DELLA BOLLA INIZIALE
669100940411     C     WCAR          IFEQ      'C'
669200940411     C                   SETOFF                                       75
669300940714     C                   MOVEL     NUM2          V2CAAS
669400940411     C                   MOVEL     COMLPN        COMLNP
669500940714     C                   MOVEL     COMNRN        V2CNRS
669600940714     C                   MOVEL     COMNSN        V2CNSP
669700940712     C     KARB          CHAIN     FNARB01L                           93
669800940411     C* RICARICO I DATI
669900940411     C                   EXSR      BOLLA
670000940411     C*
670100940411     C* RIABILITO IL CMD18
670200940411     C                   SETON                                        94
670300940411    1C                   END
670400940802     C*
670500930413     C                   ENDSR
670600940802     C**************************************************************************
670700940802     C*    LEGAMI BOLLA
670800940802     C**************************************************************************
670900940408     C     LEG2          BEGSR
671000940802     C*
671100940411     C                   MOVEL     'C'           WCAR              1
671200940408     C*
671300940408     C* 75 ON - NON RIENTRO NEL CARICAMENTO LEGAME BOLLA
671400940408     C                   SETON                                        75
671500940408     C* 94 OFF- DISABILITO IL CMD18
671600940408     C                   SETOFF                                       94
671700940408     C* CARICO DATI BOLLA
671800940408     C                   EXSR      BOLLA
671900940411     C*
672000940411     C* VISUALIZZA NUOVO LEGAME
672100940408     C* SE LA BOLLA E' UNA PRECEDENTE --> VISUALIZZO LA SEGUENTE
672200940713     C     V8CFPS        IFEQ      'P'
672300950316     C                   MOVEA     'SEGUEN.'     LBL(14)
672400940408     C                   ELSE
672500940408     C* SE LA BOLLA E' UNA SEGUENTE   --> VISUALIZZO LA PRECEDENTE
672600950316     C                   MOVEA     'PRECED.'     LBL(14)
672700940408     C                   ENDIF
672800950316     C                   MOVEA     COMLPN        LBL(21)
672900950316     C                   MOVEA     COMNRN        LBL(25)
673000950316     C                   MOVEA     COMNSN        LBL(28)
673100950316     C                   MOVEA     COMLAN        LBL(40)
673200950307     C                   MOVEA     *BLANKS       LBL(44)
673300940408     C                   MOVEA     LBL           LEGAME
673400940408     C*
673500940411     C* MOUVO I DATI DELLA LEGATA NEI CAMPI DELLA PARAM
673600940714     C                   MOVEL     ARBAAS        V2CAAS
673700940411     C                   MOVEL     ARBLNP        COMLNP
673800940714     C                   MOVEL     ARBNRS        V2CNRS
673900940714     C                   MOVEL     ARBNSP        V2CNSP
674000940408     C*
674100940408     C                   EXSR      VISUA2
674200940408     C*
674300941230     C                   MOVEL     'S'           WRIEM
674400940408     C                   ENDSR
674500940408     C*
674600940712      **************************************************************************
674700131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
674800940408      *     PER BOLLA NORMAL - CMD18 PREMUTO DA VISUAL -
674900091216      *     ricordarsi di aggiornare anche RIEPI2 per ogni modifica fatta qui
675000940712      **************************************************************************
675100940712     C     RIEPIL        BEGSR
675200940712      *
675300940712     C                   SETOFF                                       89
675400940712      *
675500940712      * EMETTO IL FORMATO VIDEO
675600940720     C                   MOVEL     'S'           WRIEM
675700940712     C     FOR07         TAG
675800940720     C     WRIEM         IFEQ      'S'
675900131107     C                   WRITE     LRI6T01
676000940720     C                   MOVEL     *BLANKS       WRIEM
676100940720     C                   ENDIF
676200021217      * Imposto la stringa comandi del 'giro' successivo
676300021217     C                   IF        *inKY = *on and $Loop > 1
676400021217     c                   Exsr      Ges_F24
676500021217     c                   eval      FD07 = FDxx
676600021217     c                   eval      F24D07 = F24Dxx
676700021217     C                   ENDIF
676800131107     C                   EXFMT     LRI6D07
676900070420     c                   setoff                                       28
677000070420     c                   clear                   v1cmsg
677100940712      *
677200131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
677300940712     C   KY*IN89         IFEQ      *ON
677400940712     C                   SETOFF                                       89
677500940712     C                   ELSE
677600940712     C                   SETON                                        89
677700940712     C                   ENDIF
677800940712     C   KY              GOTO      FOR07
677900940712      *
678000131108      * F3 - FINE LAVORO TORNO A SUBFILE
678100940714     C   KC              MOVEL     '2'           V2CFLG            1
678200940712     C   KC              GOTO      ENDRIE
678300940712      *
678400131108      * F18 - INTERROGA LEGAME BOLLA
678500940712     C   KS              DO
678600940712     C                   EXSR      CMD18
678700940712     C                   GOTO      FOR07
678800940712     C                   END
678900940712      *
679000940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
679100940712     C                   EXSR      COMAND
679200940905     C     WCMD          IFEQ      'S'
679300940802     C     WF03          IFEQ      *BLANKS
679400070404     c
679500940802     C                   GOTO      FOR07
679600940802     C                   ELSE
679700940802     C                   MOVEL     '2'           V2CFLG
679800940802     C                   GOTO      ENDRIE
679900070404     C                   ENDIF
680000091215     c
680100070404     c                   else
680200070404     c
680300070404     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
680400070404     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
680500070404     c                   if        v3cgesd='?'
680600131120
680700131120     c**                 if        wriem='S'
680800131120     c**                 eval      wvideo='R'
680900131120     c**                 exsr      Riemett
681000131120     c**                 endif
681100131120
681200070404     c                   exsr      VISUALAbi
681300070404     c                   clear                   v3cgesd
681400070412     c                   eval      wriem='S'
681500070404     c                   endif
681600131120     c
681700131120     c* controlla se immessi dei '?'
681800131120     c                   eval      wvideo='R'
681900131120     c                   exsr      CTR_puIn
682000091215     c
682100091215     c                   if        wriem='S'
682200091215     C                   GOTO      FOR07
682300091215     c                   endif
682400070404     c
682500940712     C                   END
682600940712      *
682700940712     C     ENDRIE        ENDSR
682800940712      **************************************************************************
682900131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
683000940408      *     PER LEGAME BOLLA - CMD18 PREMUTO DA VISUA2 -
683100940712      **************************************************************************
683200940712     C     RIEPI2        BEGSR
683300940712      *
683400940712     C                   SETOFF                                       89
683500940712      *
683600940712      * EMETTO IL FORMATO VIDEO
683700940809     C                   MOVEL     'S'           WRIEM
683800940712     C     FOR77         TAG
683900940809     C     WRIEM         IFEQ      'S'
684000131107     C                   WRITE     LRI6T01
684100940809     C                   MOVEL     *BLANKS       WRIEM
684200940809     C                   ENDIF
684300091216      * Imposto la stringa comandi del 'giro' successivo
684400091216     C                   IF        *inKY = *on and $Loop > 1
684500091216     c                   Exsr      Ges_F24
684600091216     c                   eval      FD07 = FDxx
684700091216     c                   eval      F24D07 = F24Dxx
684800091216     C                   ENDIF
684900091216     c
685000131107     C                   EXFMT     LRI6D07
685100070516     c                   setoff                                       28
685200070516     c                   clear                   v1cmsg
685300940408     C*
685400131108      * F3 - FINE LAVORO TORNO A SUBFILE
685500940714     C   KC              MOVEL     '2'           V2CFLG            1
685600940408     C*
685700940408     C  NKC              DO
685800940712      *
685900131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
686000940712     C   KY*IN89         IFEQ      *ON
686100940712     C                   SETOFF                                       89
686200940712     C                   ELSE
686300940712     C                   SETON                                        89
686400940712     C                   ENDIF
686500940712     C   KY              GOTO      FOR77
686600940712      *
686700940712      *
686800131108      * F18 - INTERROGA LEGAME BOLLA
686900940712     C   KS              DO
687000940712     C                   EXSR      CMD18
687100940712     C                   GOTO      FOR77
687200940712     C                   END
687300940712      *
687400940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
687500940712     C                   EXSR      COMAND
687600940905     C     WCMD          IFEQ      'S'
687700940712     C                   GOTO      FOR77
687800091216     c
687900091216     c                   else
688000091216     c
688100091216     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
688200091216     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
688300091216     c                   if        v3cgesd='?'
688400131120
688500131120     c**                 if        wriem='S'
688600131120     c**                 eval      wvideo='R'
688700131120     c**                 exsr      Riemett
688800131120     c**                 endif
688900131120
689000091216     c                   exsr      VISUALAbi
689100091216     c                   clear                   v3cgesd
689200091216     c                   eval      wriem='S'
689300091216     c                   endif
689400131120
689500131120     c* controlla se immessi dei '?'
689600131120     c                   eval      wvideo='R'
689700131120     c                   exsr      CTR_puIn
689800091216     c
689900091216     c                   if        wriem='S'
690000091216     C                   GOTO      FOR77
690100091216     c                   endif
690200091216     c
690300940712     C                   END
690400091216     C                   END
690500940712      *
690600940712     C                   ENDSR
690700131120      **************************************************************************
690800131120      * riemissione video per nuovo '?'
690900131120      **************************************************************************
691000131120     c**   Riemett       BEGSR
691100131120     c**                 write     LRI6t01
691200131120
691300131120     c**                 select
691400131120     c**                 when      wvideo='4'
691500131120     c**                 write     LRI6d04
691600131120     c**                 when      wvideo='R'
691700131120     c**                 write     LRI6d07
691800131120     c**                 endsl
691900131120
692000131120     c**                 ENDSR
692100940923      **************************************************************************
692200940923      *     CONTROLLI FORMATO VIDEO D04
692300940923      **************************************************************************
692400131120     C     CTR_puIn      BEGSR
692500110601     c*
692600110601     c* verifico se inserito '?' su email destinatario
692700110601     c                   if        v4cemw='?'
692800110601     c                   exsr      sr_emdest
692900110601     C                   MOVEL     'S'           WRIEM
693000110601     c                   endif
693100940923      *
693200091215      * Verifico se inserito '?' per visualizzazione beneficiario
693300091215     c                   if        vidben='?'
693400131120     c**                 if        wriem='S'
693500131120     c**                 exsr      Riemett
693600131120     c**                 endif
693700131120     c
693800091215     c                   exsr      sr_bencas
693900091215     C                   MOVEL     'S'           WRIEM
694000091215     c                   endif
694100131114     c
694200131114     c* Visualizzazione dati filiale
694300131114     c                   if        viddec_f='?'
694400131120     c**                 if        wriem='S'
694500131120     c**                 exsr      Riemett
694600131120     c**                 endif
694700131120
694800131114     c                   clear                   trulorfds
694900131114     c                   eval      iorffil=arblna
695000131114     c                   call      'TRULORFR'
695100131114     c                   parm                    kpjba
695200131114     c                   parm                    trulorfds
695300131114     c
695400131114     c                   clear                   viddec_f
695500131114     C                   MOVEL     'S'           WRIEM
695600131114     c                   endif
695700091215     c
695800940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
695900940923     C     V4IGCA        IFEQ      '?'
696000991027     C                   CLEAR                   DS7PQRS
696100941003     C                   MOVEL     'S'           DS7RIC
696200941003     C                   MOVEL     'V'           DS7GES
696300941003     C                   MOVEL     V4CGCA        DS7COD
696400991027     C                   MOVEL     DS7PQRS       KPJBU
696500960126     C                   CALL      'TRTB72R'
696600941003     C                   PARM                    KPJBA
696700940923     C                   MOVEL     *BLANKS       V4IGCA
696800940923     C                   MOVEL     'S'           WRIEM
696900940923     C                   ENDIF
697000940923     C*
697100940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
697200940923     C     V4IGGA        IFEQ      '?'
697300991027     C                   CLEAR                   DS7PQRS
697400941003     C                   MOVEL     'S'           DS7RIC
697500941003     C                   MOVEL     'V'           DS7GES
697600941003     C                   MOVEL     V4CGGA        DS7COD
697700991027     C                   MOVEL     DS7PQRS       KPJBU
697800960126     C                   CALL      'TRTB69R'
697900941003     C                   PARM                    KPJBA
698000940923     C                   MOVEL     *BLANKS       V4IGGA
698100940923     C                   MOVEL     'S'           WRIEM
698200940923     C                   ENDIF
698300940923     C*
698400940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
698500940923     C     V4IGMA        IFEQ      '?'
698600991027     C                   CLEAR                   DS7PQRS
698700941003     C                   MOVEL     'S'           DS7RIC
698800941003     C                   MOVEL     'V'           DS7GES
698900941003     C                   MOVEL     V4CGMA        DS7COD
699000991027     C                   MOVEL     DS7PQRS       KPJBU
699100960126     C                   CALL      'TRTB70R'
699200941003     C                   PARM                    KPJBA
699300940923     C                   MOVEL     *BLANKS       V4IGMA
699400940923     C                   MOVEL     'S'           WRIEM
699500940923     C                   ENDIF
699600940923     C*
699700940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
699800940923     C     V4IGVA        IFEQ      '?'
699900991027     C                   CLEAR                   DS7PQRS
700000941003     C                   MOVEL     'S'           DS7RIC
700100941003     C                   MOVEL     'V'           DS7GES
700200941003     C                   MOVEL     V4CGVA        DS7COD
700300991027     C                   MOVEL     DS7PQRS       KPJBU
700400960126     C                   CALL      'TRTB73R'
700500941003     C                   PARM                    KPJBA
700600940923     C                   MOVEL     *BLANKS       V4IGVA
700700940923     C                   MOVEL     'S'           WRIEM
700800940923     C                   ENDIF
700900940923      *
701000940923     C                   ENDSR
701100940321      **************************************************************************
701200940713      *     R O U T I N E      I N I Z I A L E
701300940713      **************************************************************************
701400940713     C     *INZSR        BEGSR
701500940713      *
701600940713     C     KARB          KLIST
701700940714     C                   KFLD                    V2CAAS
701800940713     C                   KFLD                    COMLNP            3 0
701900940714     C                   KFLD                    V2CNRS
702000940714     C                   KFLD                    V2CNSP
702100940713     C     KARB2         KLIST
702200940713     C                   KFLD                    ARBAAS
702300940713     C                   KFLD                    ARBLNP
702400940713     C                   KFLD                    ARBNRS
702500940713     C                   KFLD                    ARBNSP
702600971223     C     KARB4         KLIST
702700971223     C                   KFLD                    AR4AAS
702800971223     C                   KFLD                    AR4LNP
702900971223     C                   KFLD                    AR4NRS
703000971223     C                   KFLD                    AR4NSP
703100000228     C     KAR4W         KLIST
703200000228     C                   KFLD                    wAR4AAS
703300000228     C                   KFLD                    wAR4LNP
703400000228     C                   KFLD                    wAR4NRS
703500000228     C                   KFLD                    wAR4NSP
703600940713     C     KLNPX         KLIST
703700940713     C                   KFLD                    V1CAAS
703800940713     C                   KFLD                    V1CLP2
703900940713     C                   KFLD                    V1CNRS
704000940713     C                   KFLD                    V1CNSP
704100940713     C     K1ARB         KLIST
704200020910     c                   Kfld                    Wifp
704300940713     C                   KFLD                    NDC               7 0
704400940713     C                   KFLD                    NSS               3 0
704500940713     C                   KFLD                    COMLNA            3 0
704600940713     C                   KFLD                    COMAAD
704700940713     C                   KFLD                    COMMGD
704800940713     C                   KFLD                    V1CLP1
704900940713     C     K2ARB         KLIST
705000020910     c                   Kfld                    Wifp
705100940713     C                   KFLD                    NDC
705200940713     C                   KFLD                    NSS
705300940713     C                   KFLD                    COMLNA
705400940713     C                   KFLD                    COMAAD
705500940713     C                   KFLD                    COMMGD
705600940713     C     K3ARB         KLIST
705700020910     c                   Kfld                    Wifp
705800940713     C                   KFLD                    NDC
705900940713     C                   KFLD                    NSS
706000940713     C                   KFLD                    COMLNA
706100940713     C     K4ARB         KLIST
706200940713     C                   KFLD                    COMLNA
706300940713     C                   KFLD                    COMDTD
706400940713     C                   KFLD                    V1CLP1
706500940713     C     K5ARB         KLIST
706600940713     C                   KFLD                    COMLNA
706700940713     C                   KFLD                    COMDTD
706800940713     C     K6ARB         KLIST
706900940713     C                   KFLD                    V1CLP1
707000940713     C                   KFLD                    COMAAD
707100940713     C                   KFLD                    COMMGD
707200940713     C                   KFLD                    V1CLNA
707300940713     C                   KFLD                    V1CPSD
707400950405     C                   KFLD                    COMSCD
707500950405     C     K6ARB1        KLIST
707600950405     C                   KFLD                    V1CLP1
707700950405     C                   KFLD                    COMAAD
707800950405     C                   KFLD                    COMMGD
707900950405     C                   KFLD                    V1CLNA
708000950405     C                   KFLD                    V1CPSD
708100940713     C     K7ARB         KLIST
708200940713     C                   KFLD                    V1CLP1
708300940713     C                   KFLD                    COMAAD
708400940713     C                   KFLD                    COMMGD
708500940713     C     K8ARB         KLIST
708600940713     C                   KFLD                    V1CLNA
708700940713     C                   KFLD                    COMAAD
708800940713     C                   KFLD                    COMMGD
708900940713     C                   KFLD                    V1CLP1
709000940713     C                   KFLD                    V1CPSD
709100940713     C                   KFLD                    V1CSCD
709200940713     C     K9ARB         KLIST
709300940713     C                   KFLD                    V1CLNA
709400940713     C                   KFLD                    COMAAD
709500940713     C                   KFLD                    COMMGD
709600020627     c     arb7k         klist
709700020627     c                   kfld                    v1cnzd
709800020627     c                   kfld                    v1cprd
709900131111     C     ARB75K        KLIST
710000020627     c                   kfld                    v1cnzd
710100020627     c                   kfld                    v1cprd
710200131111     C                   KFLD                    $AAS
710300131111     C                   KFLD                    $MGS
710400940713     C                   KFLD                    V1CRSD
710500940713     C     AR43K         KLIST
710600020627     c                   kfld                    v1cnzd
710700020627     c                   kfld                    v1cprd
710800940713     C                   KFLD                    $AAS
710900940713     C                   KFLD                    $MGS
711000940713     C     ARB44K        KLIST
711100020627     c                   kfld                    v1cnzd
711200020627     c                   kfld                    v1cprd
711300940713     C                   KFLD                    $AAS
711400940713     C                   KFLD                    $MGS
711500940713     C                   KFLD                    V1CRSM
711600940713     C     AR44K         KLIST
711700020627     c                   kfld                    v1cnzd
711800020627     c                   kfld                    v1cprd
711900940713     C                   KFLD                    $AAS
712000940713     C                   KFLD                    $MGS
712100971223     C     KAR4          KLIST
712200971223     C                   KFLD                    WTRC
712300971223     C                   KFLD                    WN30
712400010508     C     KAR44         KLIST
712500010508     C                   KFLD                    WTRC
712600010508     C                   KFLD                    WN14
712700940713     C     KART          KLIST
712800940713     C                   KFLD                    ARBAAS
712900940713     C                   KFLD                    ARBLNP
713000940713     C                   KFLD                    ARBNRS
713100940713     C                   KFLD                    ARBNSP
713200940713     C                   KFLD                    V1CSCD
713300940713     C*
713400940713     C     KTAB1         KLIST
713500940713     C                   KFLD                    CODUT
713600940713     C                   KFLD                    COD
713700940713     C                   KFLD                    KEY
713800940713     C     KTAB2         KLIST
713900940713     C                   KFLD                    CODUT
714000940713     C                   KFLD                    COD
714100940713     C*
714200940907     C     KLBL1         KLIST
714300940713     C                   KFLD                    LBLAAN
714400940713     C                   KFLD                    LBLLPN
714500940713     C                   KFLD                    LBLNRN
714600940713     C                   KFLD                    LBLNSN
714700940713     C     KLBL2         KLIST
714800940713     C                   KFLD                    LBLAAP
714900940713     C                   KFLD                    LBLLPP
715000940713     C                   KFLD                    LBLNRP
715100940713     C                   KFLD                    LBLNSP
715200940907     C     KLBL3         KLIST
715300940907     C                   KFLD                    KAAS
715400940907     C                   KFLD                    KLNP
715500940907     C                   KFLD                    KNRS
715600940907     C                   KFLD                    KNSP
715700940713     C*
715800940719     C     KAR41         KLIST
715900940719     C                   KFLD                    KAAS
716000940719     C                   KFLD                    KLNP
716100940719     C                   KFLD                    KNRS
716200940719     C                   KFLD                    KNSP
716300940802     C                   KFLD                    KTRC
716400951220     C     KAR42         KLIST
716500951220     C                   KFLD                    KAAS
716600951220     C                   KFLD                    KLNP
716700951220     C                   KFLD                    KNRS
716800951220     C                   KFLD                    KNSP
716900021213     C*
717000021213     C     KAR51         KLIST
717100021213     C                   KFLD                    KAAS
717200021213     C                   KFLD                    KLNP
717300021213     C                   KFLD                    KNRS
717400021213     C                   KFLD                    KNSP
717500021213     C                   KFLD                    KTRd              3
717600940719     C*
717700991026     C     KAR61         KLIST
717800940713     C                   KFLD                    KAAS
717900940713     C                   KFLD                    KLNP
718000940713     C                   KFLD                    KNRS
718100940713     C                   KFLD                    KNSP
718200991026     C                   KFLD                    KTRC
718300991026     C*
718400051128     C     Ksped         KLIST
718500991026     C                   KFLD                    KAAS
718600991026     C                   KFLD                    KLNP
718700991026     C                   KFLD                    KNRS
718800991026     C                   KFLD                    KNSP
718900940713     C*
719000990715     C     KGCA1         KLIST
719100941202     C                   KFLD                    KAAS
719200940713     C                   KFLD                    KLNP
719300940713     C                   KFLD                    KNRS
719400940713     C                   KFLD                    KNSP
719500940713     C*
719600990805     C     KEVB1         KLIST
719700940713     C                   KFLD                    KAAS$
719800940713     C                   KFLD                    KLNP
719900940713     C                   KFLD                    KNRS
720000940713     C                   KFLD                    KNSP
720100131114     C     Kcet          KLIST
720200140224     C                   KFLD                    pctfgs
720300131114     C                   KFLD                    pctNDC
720400131114     C                   KFLD                    arbpdc
720500131114     C                   KFLD                    TTRD              3
720600131114     C                   KFLD                    arbAAS
720700131114     C                   KFLD                    arbLNP
720800131114     C                   KFLD                    arbNRS
720900131114     C                   KFLD                    arbNSP
721000950324     C*
721100020906     C     KFVV1         KLIST
721200950324     C                   KFLD                    WNPG
721300950327     C                   KFLD                    WNDC
721400020906     c                   Kfld                    Wfgs
721500131119     C     KDST          KLIST
721600131119     C                   KFLD                    WNPG
721700131119     C                   KFLD                    kndc6
721800131119     c                   Kfld                    Wfgs
721900021203
722000021203     C     Kfapd         klist
722100021203     C                   kfld                    kpdtip
722200021203     C                   kfld                    kpdpdr
722300131108     c
722400131108     c     kbrv          klist
722500131108     C                   KFLD                    WNPG
722600131108     C                   KFLD                    knfv
722700131108     c                   Kfld                    Wfgs
722800131108     c                   Kfld                    artfls
722900131108     c                   Kfld                    artlna
723000131108     c                   Kfld                    artnrs
723100131108     c                   Kfld                    artnsc
723200131111     C     KarS          KLIST
723300131111     C                   KFLD                    V1CNOT
723400131111     C                   KFLD                    KKTRC             1
723500131111     C                   MOVE      'C'           KKTRC
723600131111     C     KarSXT        KLIST
723700131111     C                   KFLD                    arSFLS
723800131111     C                   KFLD                    arSLNA
723900131111     C                   KFLD                    arSNRS
724000131111     C                   KFLD                    arSNSC
724100131111     C     KartXP        KLIST
724200131111     C                   KFLD                    arTAAS
724300131111     C                   KFLD                    arTLNP
724400131111     C                   KFLD                    arTNRS
724500131111     C                   KFLD                    arTNSP
724600040218      *
724700040218      * File FITGD02L
724800040218     c     K02TGD02      klist
724900040218     c                   kfld                    TGDtor
725000040218     c                   kfld                    TGDogg
725100030527     C*
725200030527     C* Chiave su TNTBE01L - Parziale
725300030527     C     KTBE01P       KLIST
725400030527     C                   KFLD                    TBECOD
725500030527     C                   KFLD                    TBEKE1
725600940713     C*
725700940713     C*-------------------------------------------------------------------------
725800940713     C*
725900991222     C     *LIKE         DEFINE    ARBAAS        WAAS
726000991027     C     *LIKE         DEFINE    ARBLNP        WLNP
726100991027     C     *LIKE         DEFINE    ARBNRS        WNRS
726200991027     C     *LIKE         DEFINE    ARBNSP        WNSP
726300991027     C     *LIKE         DEFINE    FVVNPG        WNPG
726400991027     C     *LIKE         DEFINE    FVVNFV        WNDC
726500131108     C     *LIKE         DEFINE    brvNFV        knfv
726600991027     C     *LIKE         DEFINE    V1CSCD        COMSCD
726700991027     C     *LIKE         DEFINE    COMC08        SAVC08
726800971219     C     *LIKE         DEFINE    ARBAAS        KAAS
726900990805     C     *LIKE         DEFINE    EVBAAS        KAAS$
727000940713     C     *LIKE         DEFINE    ARBLNP        KLNP
727100940713     C     *LIKE         DEFINE    ARBNRS        KNRS
727200940713     C     *LIKE         DEFINE    ARBNSP        KNSP
727300961112     C     *LIKE         DEFINE    ARBFBR        WFBR
727400940713     C     *LIKE         DEFINE    TBLCOD        COD
727500940713     C     *LIKE         DEFINE    TBLKEY        KEY
727600940713     C     *LIKE         DEFINE    ARBAAS        $AAS
727700940713     C     *LIKE         DEFINE    ARBMGS        $MGS
727800940719     C     *LIKE         DEFINE    AR4TRC        KTRC
727900940907     C     *LIKE         DEFINE    LBLAAN        COMAAN
728000971223     C     *LIKE         DEFINE    AR4TRC        WTRC
728100060130     C*-------------------------------------------------------------------------
728200060130     c
728300060130     c                   movel     kpjbu         savkpjbu
728400060130      *
728500060130     c     *dtaara       define    §azute        azuteds
728600060130     c     *dtaara       define    §datiute      ddatiute
728700060130     c                   in(E)     *dtaara
728800060130    1c                   If        %error  or RSUT = *blanks
728900060130     c                   Clear                   Tibs34ds
729000060130     c                   Call      'TIBS34R'
729100060130     c                   Parm                    Tibs34ds
729200060130     c                   In        *dtaara
729300060130    1c                   EndIf
729400060130
729500060130     c                   Clear                   wabi
729600060130     c                   Clear                   v1cmsg
729700060130     c                   Clear                   dLat
729800060130
729900060130      * Verifica errori e autorità profilo
730000060130s   1c                   Select
730100060130      * se ho errori nei dati utente esco dal pgm
730200060130w   1c                   When      DutErr = 'E'
730300060130     c                   GoTo      Fine
730400060130      * se non c'è l'abilitazione
730500060130      * --> se 1° livello, abilitazioni al terminal
730600060130      *     se 2° livello, abilitazioni al punto operativo
730700060130      *     se sede è impossibile ma se capita mando a fine il pgm
730800060130w   1c                   When      UteAut = *Blanks
730900060130if  2c                   If        DutLpo = '1'
731000060130     c                   Eval      wabi   = 'TP'
731100060130e   2c                   EndIf
731200060130if  2c                   If        DutLpo = '2'
731300060130     c                   Eval      wabi   = 'PO'
731400060130e   2c                   EndIf
731500060130if  2c                   If        DutLpo = 'S'
731600060130     c                   GoTo      Fine
731700060130e   2c                   EndIf
731800060130      * carica le abilitazioni del profilo
731900060130x   1c                   Other
732000060130     c                   Movel     UteFaf        Dute01
732100060131     c                   If        §Utembf <> *Blanks
732200060131     c                   Eval      wabi = §Utembf
732300060130     c                   Else
732400060130     c                   Eval      wabi = UteAut
732500060130     c                   EndIf
732600060130e   1c                   EndSl
732700060130
732800060130      * controllo se ok l'abilitazione dell'utente
732900060130     c                   Clear                   Tibs02ds
733000060130     c                   Eval      T02Mod = 'C'
733100060130     c                   Eval      T02Sif = knsif
733200060130     c                   Eval      T02Cod = 'LAT'
733300060130     c                   Movel(p)  wabi          T02Ke1
733400060130     c                   Call      'TIBS02R'
733500060130     c                   Parm                    kpjba
733600060130     c                   Parm                    Tibs02ds
733700060130if  1c                   If        T02Err = *Blanks
733800060130     c                   Eval      dLat = T02Uni
733900060130e   1c                   EndIf
734000060130      * errore o non abilitato
734100060130if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
734200060130     c                   Seton                                        28
734300060130     C                   movel     msg(26)       v1cmsg
734400060130   x1c                   Else
734500060130      * Reperimento dei P.O. gestibili dall'utente
734600060130     c                   clear                   TRUL31DS
734700060130     c                   clear                   TRUL31DS2
734800060130     c                   eval      I31abi = wabi
734900060130     c                   eval      I31cdi = DUTdis
735000060130     c                   eval      I31car = DUTare
735100060130     c                   eval      I31cpo = DUTpou
735200060130     c                   call      'TRUL31R'
735300060130     c                   parm                    KPJBA
735400060130     c                   parm                    TRUL31DS
735500060130     c                   parm                    TRUL31DS2
735600060130if  2c                   if        O31pog > *zeros
735700060130     c                   movea     O31pog        $pog
735800060130     c                   movea     O31arg        $arg
735900060130     c                   movea     O31dig        $dig
736000060130x   2c                   else
736100060130     c                   Seton                                        28
736200060130     c                   movel     MSG(26)       v1cmsg
736300060130e   2c                   endif
736400060130    1c                   endif
736500060130     c
736600060130     c                   movel     savkpjbu      kpjbu
736700971223     C*
736800971223     C* IMPOSTAZIONE CAMPI FISSI
736900940907     C                   MOVEL     *ZERO         COMLPN            3
737000940907     C                   MOVEL     *ZERO         COMNRN            2
737100940907     C                   MOVEL     *ZERO         COMNSN            7
737200950307     C                   MOVEL     *ZERO         COMLAN            3
737300950324     C                   Z-ADD     4             WNPG
737400940713      *
737500940713     C                   Z-ADD     *ZERO         NR2               4 0
737600940713     C                   Z-ADD     *ZERO         NR6               4 0
737700940713     C                   Z-ADD     *ZERO         NR8               4 0
737800940802     C                   MOVEL     *BLANKS       WF03              1
737900070403     c
738000070403     c* Verifico se profilo utente ha GEO attiva
738100131205     c* Per EDP sempre anche prr RAC999
738200131205     c                   if        %subst(knmus:1:3)='EDP' or
738300131205     c                             %subst(knmus:1:6)='RAC999'
738400070410     c                   eval      dutgeot='S'
738500070410     c                   else
738600070403     c     dutpou        chain     azorg01l
738700070403     c                   if        %found(azorg01l)
738800070403     c                   movel     orgde8        og148
738900070403     c                   else
739000070403     c                   clear                   og148
739100070403     c                   endif
739200070410     c
739300070524     c                   if        §oggeot<>' ' or §ogtlls<>' '
739400070524     c                   movel     'S'           dutgeot           1
739500070410     c                   endif
739600070524     c                   endif
739700131007
739800131007      /free
739900131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
740000131007      /end-free
740100140319     c* Tasto funz orari servizi sempre abilitato
740200140319     c                   eval      wabif9='S'
740300131007
740400940713      *
740500940713     C                   ENDSR
740600971223     C**
740700971223     C* ELABORAZIONE DI SINGOLA BOLLA --------------------------------*
740800971223     C     SINGOL        BEGSR
740900971223     C                   CLEAR                   WINIZ
741000971223     C                   CLEAR                   WFINE
741100971223     C*
741200971223     C* 97 ON - MANUTENZIONE BOLLA
741300060130    1C     *IN97         IFEQ      *ON
741400971223     C*
741500060224     c                   movel     arblna        w003a
741600060224     c     w003a         lookup    $pog                                   31
741700060130     c* Se la causale variazione bolle prevede anche il p.o. cliente
741800131111     c*    controllo anche questo
741900060130    2c                   if        not *in31 and d66pocl='S'
742000060130     c     arbcbo        lookup    cbodop                                 31
742100060130    3c                   if        not *in31
742200060130     c                   movel     arbksc        kscfil
742300060130   x3c                   else
742400060130     c* Se bolla doppia prendo il ksc dall'AR6
742500060130     C                   Z-ADD     ARBAAS        KAAS
742600060130     C                   Z-ADD     ARBLNP        KLNP
742700060130     C                   Z-ADD     ARBNRS        KNRS
742800060130     C                   Z-ADD     ARBNSP        KNSP
742900060130     C                   MOVEL     '2'           KTRC
743000060130     C     KAR61         CHAIN     FIAR6000                           31
743100060130if  4C                   IF          %FOUND(fiar601l)
743200060130     c                   movel     ar6ksc        kscfil
743300060130     c                   else
743400060130     c                   movel     999           kscfil
743500060130    4c                   endif
743600060130    3c                   endif
743700060130     c
743800060224     c                   movel     kscfil        w003a
743900060224     c     w003a         lookup    $pog                                   31
744000060130    2c                   endif
744100060130     c
744200060130     C     *IN31         IFEQ      *OFF
744300971223     C                   MOVEL     MSG(19)       V1CMSG
744400971223     C                   SETON                                        459028
744500971223     C                   GOTO      FOR01
744600971223     C                   ELSE
744700971223     C*
744800991027     C                   CLEAR                   FNLR48DS
744900060224     c* come p.o. gestione prendo il p.o. gestore (da £6) della linea
745000060224     c*  di arrivo
745100060224     c                   clear                   fnlv55ds
745200060224     c                   movel     arblna        d55lin
745300060224     c                   movel     '6'           d55tpt
745400060224     C                   time                    wrkoggiiso
745500060224     C     *iso          move      wrkoggiiso    d55drf
745600060224     c                   call      'FNLV55R'
745700060224     c                   parm                    fnlv55ds
745800060224     c                   movel     d55tfa        d48fgs
745900060224     c
746000131203     c                   select
746100140117     C     v1cbarcode    whengt    0
746200140117     C                   MOVEL     ARbAAS        D48AAS
746300140117     C                   MOVEL     ARbLNP        D48LNP
746400140117     C                   MOVEL     ARbNRS        D48NRS
746500140117     C                   MOVEL     ARbNSP        D48NSP
746600131203     C     SEL2          whenne    0
746700980403     C                   MOVEL     V1CAAS        D48AAS
746800980403     C                   MOVEL     V1CLP2        D48LNP
746900980403     C                   MOVEL     V1CNRS        D48NRS
747000980403     C                   MOVEL     V1CNSP        D48NSP
747100131203     C                   other
747200111011     C*                  MOVEL     AR4AAS        D48AAS
747300111011     C*                  MOVEL     AR4LNP        D48LNP
747400111011     C*                  MOVEL     AR4NRS        D48NRS
747500111011     C*                  MOVEL     AR4NSP        D48NSP
747600111011     C                   MOVEL     v2cAAS        D48AAS
747700111011     C                   MOVEL     v2clnp        D48LNP
747800111011     C                   MOVEL     v2cNRS        D48NRS
747900111011     C                   MOVEL     v2cNSP        D48NSP
748000131203     C                   ENDsl
748001141010     c* routine per verifica dispo da esaminare
748002141010     c                   exsr      sr_wdwdc
748003141010
748100971223     C                   MOVEL     V1CCVB        D48CVB
748200971223     C                   MOVEL     ARBCBO        D48CBO
748300971223     C                   MOVEL     'V'           D48FFR
748400971223     C                   MOVEL     'A'           D48TBO
748500971223     C                   MOVEL     'S'           D48F12
748600991027     C                   MOVEL     FNLR48DS      KPJBU
748700971223     C*
748800971223     C                   CALL      'FNLR48R'
748900971223     C                   PARM                    KPJBA
749000971223     C                   PARM                    DSARBD
749100971223     C                   PARM                    DSARBK
749200991011     C                   PARM                    DARBT
749300971223     C                   PARM                    DSARBG
749400030612     C                   PARM                    trul90ds
749500971223     C*
749600991027     C                   MOVEL     KPJBU         FNLR48DS
749700060307     c                   movel     'S'           wriem
749800971223     C* ERRORE
749900971223     C     D48ERR        IFEQ      '1'
750000971223     C                   MOVEL     D48MSG        V1CMSG
750100971223     C                   SETON                                        284790
750200971223     C                   GOTO      FOR01
750300971223     C                   ENDIF
750400971223     C                   ENDIF
750500971223     C*
750600971223   X2C                   ELSE
750700131111   c
750800971223     C* VISUALIZZAZIONE BOLLA
750900131111     c                   select
751000131111     C     v1cbarcode    whengt    0
751100131111     C                   MOVEL     arbAAS        V2CAAS
751200131111     C                   MOVEL     arblnp        COMLNP
751300131111     C                   MOVEL     arbNRS        V2CNRS
751400131111     C                   MOVEL     arbNSP        V2CNSP
751500140117     C     SEL2          whengt    0
751600140117     C                   MOVEL     V1CAAS        V2CAAS
751700140117     C                   MOVEL     V1CLP2        COMLNP
751800140117     C                   MOVEL     V1CNRS        V2CNRS
751900140117     C                   MOVEL     V1CNSP        V2CNSP
752000131111     C                   ENDSL
752100131111     c
752200971223     C                   EXSR      BOLLA
752300971223     C*
752400971223     C                   EXSR      VISUAL
752500971223    2C                   ENDIF
752600971223     C*
752700971223    2C     PA1FLG        IFNE      '1'
752800971223     C     PA1FLG        ANDNE     'N'
752900971223     C     PA1FLG        ANDNE     '3'
753000971223     C     PA1FLG        ANDNE     'P'
753100971223     C     PA1FLG        ANDNE     'E'
753200990129     C     PA1FLG        ANDNE     'C'
753300971223     C                   MOVEL     '1'           WINIZ             1
753400971223     C                   ELSE
753500971223     C                   MOVEL     '1'           WFINE             1
753600971223    2C                   END
753700971223    2C                   ENDSR
753800110601      **************************************************************************
753900110601      *   emissione window email destinatario
754000110601      **************************************************************************
754100110601     C     sr_emdest     BEGSR
754200110601     c                   clear                   v4cemw
754300110601     c
754400110601     c                   eval      ilv79TLA='L'
754500110601     c                   eval      ilv79Aas=arbaas
754600110601     c                   eval      ilv79Lnp=arblnp
754700110601     c                   eval      ilv79nrs=arbnrs
754800110601     c                   eval      ilv79nsp=arbnsp
754900110601     c                   call      'FNLV79R'
755000110601     c                   parm                    fnlv79ds
755100110601     c
755200110601     c                   endsr
755300091214      **************************************************************************
755400131114      *   Emissione del Beneficiario C/Ass
755500091214      **************************************************************************
755600091214     C     sr_bencas     BEGSR
755700091215     c                   clear                   vidben
755800091214     c                   clear                   wdaben
755900091214     c                   clear                   wdasrsc
756000091214     c                   clear                   wdascdi
756100091216     c                   clear                   trulintds
756200091216     c
756300091216     c                   eval      iinttip='B'
756400091216     c                   eval      iinttic=vidtic
756500091216     c                   eval      iintrscm=v4crsm
756600091216     c                   eval      iintrsco=vidrmo
756700091216     c                   eval      iintCDI =w_cdi
756800091216     c                   call      'TRULINTR'
756900091216     c                   parm                    trulintds
757000091216     c
757100091216     c                   movel     ointrsci      wdasrsc
757200091216     c
757300091216     c                   select
757400091216     c                   when      ointtipi='O'
757500091216     c                   eval      wdaben='MITTENTE  ORIGINALE'
757600091216     c
757700091216     c                   when      ointtipi='R'
757800091216     c                   eval      wdascdi=w_cdi
757900091216     c                   eval      wdaben=' O R D I N A N T E'
758000091216     c
758100091216     c                   when      ointtipi='M'
758200091216     c                   if        v3cksc>0
758300091216     c                   movel     v3cksc        wdascdi
758400091216     c                   endif
758500091216     c                   eval      wdaben='  M I T T E N T E'
758600091216     c                   ENDSL
758700091214     c
758800131107     c                   exfmt     LRI6wdas
758900091214     c                   endsr
759000130830      **************************************************************************
759100130830      *   window per visualizzare gli orari servizi
759200130830      **************************************************************************
759300130830     C     sr_OrariSr    BEGSR
759400130920
759500140110     c                   clear                   trulorSds
759600140110     c                   eval       s_IOREtfp = ARBtfp
759700140110     c                   eval       s_IOREtfa = ARBtfa
759800140110     c                   eval       s_IOREdti = ARBdti
759900140110     c                   eval       s_IOREhti = ARBhti
760000140110     c                   eval       s_IOREdcr = ARBdcr
760100140110     c                   eval       s_IOREhcr = ARBhcr
760200140110     c                   eval       s_IOREtcr = ARBtcr
760300140110     c                   eval       s_IOREDEI = s_ARBdei
760400140110     c                   eval       s_IOREOEI = s_ARBoei
760500140110     c                   eval       s_IOREfil = ARBlna
760600140110     c                   eval       s_IOREcap = ARBcad
760700140110     c                   eval       s_IOREloc = ARBlod
760800140110     c                   eval       s_IOREnar = ARBnzd
760900140113     c* Data di Riferimento: data consegna se consegnata
761000140113     c                   select
761100140113     c                   when      arbdcm>0 and arbica<>' ' and arbica<>'R'
761200140113     c                             and arbicc<>' ' and arbicc<>'R'
761300140113     c                   eval       s_ioredta=arbdcm
761400140113     c* per ritorno all'incasso data del giorno
761500140113     c                   when      arbdcm>0 and arbddc>0
761600140113     C                   time                    wrkoggiiso
761700140113     C     *iso          move      wrkoggiiso    s_ioredta
761800140113     c                   other
761900140113     c* altrimenti data prevista consegna se > = oggi
762000140113     C                   time                    wrkoggiiso
762100140113     C     *iso          move      wrkoggiiso    s_ioredta
762200140113     c* se < di oggi, imposto oggi
762300140113     c                   if        amgdce>s_ioredta
762400140113     c                   eval       s_ioredta=amgdce
762500140113     c                   endif
762600140113     c                   endsl
762700140113     c
762800140110     c                   eval       s_IOREtser = 'C'
762900140409     c                   eval       s_IOREDDC=arbddc
763000140409     c                   eval       s_IOREnDC=arbndc
763100140409     c
763200140110     c                   IF         *in19 or
763300140110     c                              (ARBfbc = *blanks and
763400140110     c                               ARBddc > 0 and
763500140110     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
763600140110     c                               arbica='R' or arbicc=' ' or arbicc='R'))
763700140110     c                   eval       s_IOREcons = 'S'
763800140225     c* Orario presunto ritiro/consegna
763900140225     c                   move      arbndc        pctndc
764000140225     c                   move      arbifp        pctfgs
764100140225     c                   move      'COR'         ttrd
764200140225     c     kcet          setgt     fipct02l
764300140225     c     kcet          readpe    fipct02l
764400140310     c                   if        not %eof(fipct02l)
764500140225     c                   movel     pctdati       fipctcords
764600140307     c                   eval      s_ioreorp=§PCTORASTI
764700140225     c                   endif
764800140225     c
764900140110     c                   ENDIF
765000140225     c
765100140110     c                   eval       s_IOREtsp = ARBtsp
765200130920
765300140325     c                   eval      s_iorecol=36
765400140110     c                   call      'TRULORSR'
765500130920     c                   parm                    kpjba
765600140110     c                   parm                    trulorSds
765700130830     c
765800131111     c                   ENDSR
765900131111      **************************************************************************
766000131111      *   Prepara SQL per rsd ind lod rsm
766100131111      **************************************************************************
766200131111     C     Presql        BEGSR
766300131111     //free
766400131114         if v1cxrsd='X'  ;
766500131114
766600131114         wSQL ='SELECT fnarb00f.*  +
766700131114                   from FNARB00F left outer join FIAR400F on arbaas=ar4aas +
766800131114               and arblnp=ar4lnp and arbnrs=ar4nrs and arbnsp=ar4nsp  +
766900131114                and ar4trc=''D'' where arbnzd='''+ v1cnzd +''' '  ;
767000131114                else  ;
767100131114
767200131114         wSQL ='SELECT * from FNARB00F   where arbnzd='''+ v1cnzd +''' '  ;
767300131114         endif  ;
767400131114
767500131114       if v1cnzd=*blanks   ;
767600131114         wSQL +=' and arbprd=''' + v1cprd + ''' '   ;
767700131111       endif  ;
767800131111
767900131115       if (v1cssp=' ' or v1cssp='1')  ;
768000131115        if comdtd>0 and comdta>comdtd  ;
768100131115           wSQL += ' and (arbaas*10000)+arbmgs>=' + %editc(comdtd:'X')+ ' and +
768200131115                        (arbaas*10000)+arbmgs<=' + %editc(comdta:'X')  ;
768300131115        endif  ;
768400131115        if comdtd>0 and (comdta=0 or comdta=comdtd)      ;
768500131115           wSQL += 'and arbaas=' + %editc(comaad:'X')+ ' and arbmgs=' +
768600131115                                %editc(commgd:'X')  ;
768700131115        endif  ;
768800131115       endif  ;
768900131111
769000131111       // se voglio le "IN CONSEGNA", parzializzo
769100131111       if v1cssp='2'  ;
769200131113           wSQL += ' and (arbdcm=0 or arbica='' '' or arbica=''R'' +
769300131113                     or arbicc='' '' or arbicc=''R'')'  ;
769400131111       endif  ;
769500131114
769600131114       // Se voglio le "DA CONSEGNARE", parzializzo
769700131111       if v1cssp='1'  ;
769800131111           wSQL += ' and arbifp=0 and arbndc=0 and arbnss=0' ;
769900131111       endif  ;
770000131111
770100131114       // Se voglio le "CONSEGNATE", parzializzo
770200131111       if v1cssp='3'  ;
770300131111           wSQL += ' and arbdcm>0' ;
770400131111       endif  ;
770500131111
770600131111       // linea di arrivo
770700131111       if v1clna>0  ;
770800131111           wSQL += ' and arblna=' + %editc(v1clna:'X') ;
770900131111       endif  ;
771000131111
771100131111        if v1crsd<>*blanks  ;
771200131111
771300131111         if v1cxrsd =' '   ;
771400131114           wSQL += ' and arbrsd    like(''';
771500131111         else  ;
771600131114           wSQL += ' and concat(arbrsd, ifnull(ar4not, '' '')) like(''%'  ;
771700131111         endif ;
771800131111
771900131209         w060a=v1crsd  ;
772000131209         exsr APICI  ;
772100131209
772200131209           wSQL += w060a ;
772300131111           wSQL  = %trim(wSQL) + '%'')' ;
772400131111         endif ;
772500131111
772600131111        if v1cIND<>*blanks  ;
772700131111
772800131111         if v1cxind =' '   ;
772900131111           wSQL += ' and arbind like(''';
773000131111         else  ;
773100131113           wSQL += ' and arbind like (''%' ;
773200131111         endif ;
773300131111
773400131209         w060a=v1cind  ;
773500131209         exsr APICI  ;
773600131209           wSQL += w060a ;
773700131111           wSQL  = %trim(wSQL) + '%'')' ;
773800131111         endif ;
773900131111
774000131111        if v1cLOD<>*blanks  ;
774100131111
774200131111         if v1cxlod =' '   ;
774300131111           wSQL += ' and arblod like(''';
774400131111         else  ;
774500131113           wSQL += ' and arblod like (''%' ;
774600131111         endif ;
774700131111
774800131209         w060a=v1clod  ;
774900131209         exsr APICI  ;
775000131209           wSQL += w060a ;
775100131111           wSQL  = %trim(wSQL) + '%'')' ;
775200131111         endif ;
775300131111
775400131111        if v1cRSM<>*blanks  ;
775500131111
775600131111         if v1cxrsm =' '   ;
775700131111           wSQL += ' and arbrsm like(''';
775800131111         else  ;
775900131113           wSQL += ' and arbrsm like (''%' ;
776000131111         endif ;
776100131111
776200131209         w060a=v1crsm  ;
776300131209         exsr APICI  ;
776400131209           wSQL += w060a ;
776500131115           wSQL  = %trim(wSQL) + '%'')' ;
776600131111         endif ;
776700131115
776800131115           wSQL += ' order by arbnzd, arbprd, arbaas, arbmgs'  ;
776900131111
777000131111        ENDSR  ;
777100131111       //--------------------------------------------------------------
777200131111       //?Apertura cursore.                                            ?
777300131111       //--------------------------------------------------------------
777400131111       BEGSR  OpenCursor;
777500131111
777600131111         wEnd = *off;
777700131111
777800131111       //?Dichiarazione cursore?
777900131111         exec sql
778000131111         PREPARE S1 from :wSQL;
778100131209
778200131209         IF sqlcode < 0;
778300131209           wEnd = *on;
778400131209           leavesr        ;
778500131209         ENDIF;
778600131209
778700131111         exec sql
778800131111         DECLARE ARB  cursor for S1;
778900131111
779000131111       //?Apertura del cursore
779100131111         exec sql
779200131111         OPEN ARB;
779300131111
779400131111         IF sqlcode < 0;
779500131111           wEnd = *on;
779600131209           leavesr        ;
779700131111         ENDIF;
779800131209
779900131209           WopenC='1'     ;
780000131209
780100131111
780200131111       ENDSR;
780300131209       //--------------------------------------------------------------
780400131209       //?Raddoppio apici nella stringa                                ?
780500131209       //--------------------------------------------------------------
780600131209       BEGSR  Apici     ;
780700131209       ap=1  ;
780800131209       dow ap <=60  ;
780900131209       if %subst(w060a:ap:1)=''''  ;
781000131209       w060a=%replace('''''':w060a:ap:1)  ;
781100131209       ap= ap   +1       ;
781200131209       endif  ;
781300131209
781400131209       ap= ap   +1       ;
781500131209       enddo  ;
781600131209
781700131209       ENDSR;
781800131111       //--------------------------------------------------------------
781900131111       //?Chiusura cursore.
782000131111       //--------------------------------------------------------------
782100131111       BEGSR CloseCursor;
782200131111
782300131111       //?Chiusura cursore SQL?
782400131111         exec sql
782500131111           close ARB;
782600131209           clear WopenC   ;
782700131209           Wend=*off      ;
782800131111
782900131111       ENDSR;
783000131111       //--------------------------------------------------------------
783100131111       //?Lettura cursore.                                             ?
783200131111       //--------------------------------------------------------------
783300131111       BEGSR  ReadCursor;
783400131111
783500131111         clear  FNARBds;
783600131113         *in30=*off    ;
783700131113         *in32=*off    ;
783800131111
783900131111         exec sql
784000131111         FETCH next from ARB into :FNARBds;
784100131111
784200131111         IF sqlcod = 100 or sqlcod < 0;
784300131111           wEnd = *on;
784400131111         ENDIF;
784500131111
784600131111       ENDSR;
784605141010       //--------------------------------------------------------------
784606141010       //?Window per f19=Dispo di consegna da esaminare
784607141010       //--------------------------------------------------------------
784608141010       BEGSR sr_wdwdc   ;
784609141010          $fine=*off;
784610141105          clear fnlry09ds2;
784611141010          ilry09tch='C';
784612141010          ILRY09AAS=d48aas ;
784613141010          ILRY09LNP=d48LNP;
784614141010          ILRY09NRS=d48NRS ;
784615141010          ILRY09NSP=d48NSP ;
784616141105          kpjbu=fnlry09ds2;
784617141105          fnlry09r(kpjba);
784618141105          fnlry09ds2=kpjbu;
784619141010          if olry09esit='1' ;
784620141010             dow $fine=*off;
784621141010                exfmt lri6wddc;
784622141010                   if *inkt;
784623141105                       clear fnlry09ds2;
784624141010                       ilry09tch='E';
784625141010                       ILRY09AAS=d48aas ;
784626141010                       ILRY09LNP=d48LNP;
784627141010                       ILRY09NRS=d48NRS ;
784628141010                       ILRY09NSP=d48NSP ;
784629141105                       ILRY09DF06='S';
784630141105                       kpjbu=fnlry09ds2;
784631141105                       fnlry09r(kpjba);
784632141010                       $fine=*on;
784633141010                   endif;
784634141010             enddo;
784635141010          endif ;
784636141010
784637141010       ENDSR;
784700131111
784800131111
784900131111
785000131111      /end-free
785100131111
785200940713      **************************************************************************
785300941014**        12345678901234567890123456789012345678901234567890
785400950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
785500930413**
785600950316LEGATA A PIU' BOLLE                       - F18=Visualizza
785700920326**
785800950316 Non Presente
785900950316 F18=Visualizza
786000940907Vedi in Int.Par
786100940907Vedi in Int.Arr
786200941011Vedi Arch.Stor.
786300940907**
786400940907Spedizione non Presente
786500941230
786600940907Vedi in Int.Bolle Part.
786700940907Vedi in Int.Bolle Arr.
786800941011Vedi in Archiv. Storico
786900940713**  MESSAGGI DI ERRORE VIDEO  **************************************************
787000940713Nessuna bolla da visualizzare                                                  1
787100940713Immettere linea di arrivo valida                                               2
787200131114Effettuare ulteriori parzializzazioni                                          3
787300940713Immettere linea di arrivo o linea di partenza                                  4
787400940713Immettere linea di partenza valida                                             5
787500940713Numero spedizione inesistente                                                  6
787600940713Immettere anno spedizione                                                      7
787700940713Immettere data formalmente valida                                              8
787800040216Immettere tipo bolla valido                                                    9
787900940713Immettere data di spedizione iniziale minore o uguale a quella finale         10
788000940713Immettere numero di serie iniziale minore o uguale a quello finale            11
788100940713Immettere numero segnacollo iniziale minore o uguale a quello finale          12
788200940713Immettere numero segnacollo finale, se immesso quello iniziale                13
788300131114Scelta incongruente tra stato spedizioni  e richiesta                         14
788400940713Fine scorrimento                                                              15
788500940715Immettere tipo servizio bolle valido                                          16
788600940923Immettere filiale segnacollo valida                                           17
788700060614Riferimento partner estero errato x bolle DPD:o lungo 11 numerico a 14 alfanum
788800941212Bolla non manutenzionabile: in gestione ad altra filiale                      19
788900991227Il riferimento partner estero per bolle DPD deve avere massimo 12 numeri
789000010508Immettere p.o. di emissione valido                                            21
789100010510Non trovate bolle per l' O.R.M. richiesto                                     22
789200020626Immettere la nazione e la provincia del destinatario                          23
789300020627Nazione destinatario errata                                                   24
789400020627Provincia destinatario errata                                                 25
789500060130Utente non abilitato alla Variazione bolle                                    26
789600060130Utente non abilitato alla variazione bolle del p.o. in gestione               27
789700060613ATTENZIONE!Saranno selez.bolle con Nazione e RagSoc impostate ma PROVINC.vuota28
789800070420Stampa LDV effettuata ...                                                     29
789900070420Stampa LDV al momento non effettuabile: ritentare o telefonare in Sede        30
790000080201Assegnato non tassato: stampa LdV non effettuabile                            31
790100131111non trovata bolla per il barcde digitato                                      32
790200030612**  cmA4
790300030612OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
790400111221 SHARE(*YES) USRDTA('Man.BolA4')
790500030612**  cmA5
790600030612OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
790700111221 SHARE(*YES) USRDTA('Man.BolA5')
790800070426**  cmA4f6
790900111221OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
791000111221USRDTA('Rist.Bolle') SECURE(*YES)
791100070426**  cmA5f6
791200070426OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
791300111221             USRDTA('Rist.Bolle') SECURE(*YES)
791400030612** cnm
791500991222 0123456789
791600040218** TF                                                                        *
791700070420F1=Agg.Rich#                                                                    1
791800070411** TF01B                                                                     *
791900070411F1=SCELTA BOLLA#                                                                1
792000021216** TF                                                                        *
792100021218F2=C.A.#                                                                        2
792200021216** TF                                                                        *
792300021216F4=Tass2#                                                                       4
792400021216** TF                                                                        *
792500021216F5=Eventi#                                                                      5
792600070420** TF                                                                        *
792700070420F6=StampaLDV#                                                                   5
792800021216** TF                                                                        *
792900021217F7=Anomalie#                                                                    7
793000021216** TF                                                                        *
793100021219F8=Discord#                                                                     8
793200130830** TF                                                                        *
793300130830F9=OrariSr#                                                                     9
793400021216** TF                                                                        *
793500070420F10=Variaz#                                                                     10
793600021216** TF                                                                        *
793700030121F11=NoteS/B#                                                                    11
793800021217** TF                                                                        *
793900021217F12=Ritorno#                                                                    11
794000021216** TF                                                                        *
794100070420F13=Tassaz#                                                                     13
794200021217** TF                                                                        *
794300021216F14=Giacenza#                                                                   14
794400040218** TF                                                                        *
794500070420F16=Gest.Rich#                                                                  16
794600070404** TF                                                                        *
794700140311F17=NoteAut#                                                                    16
794800021216** TF                                                                        *
794900021216F19=Spunte#                                                                     19
795000021216** TF                                                                        *
795100070420F20=Affid#                                                                      19
795200021216** TF                                                                        *
795300070420F21=DetColli#                                                                   19
795400110928** TF                                                                        *
795500110930F22=PerfectOrder#                                                               19
