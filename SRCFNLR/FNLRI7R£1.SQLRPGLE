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
004800160115     FFNBLP01L  IF   E           K DISK    infds(fnblpinf)
004900160115     FFNBLP00F  IF   E             DISK
005000160115     f                                     rename(fnblp000:fnblpfis)
005100160115     f                                     prefix(ARB:3)
005200160316     FFNblt27L  IF   E           K DISK
005300940712     F*.......... V A R I A Z I O N I   B O L L E
005400131007     FFiARBF2C  IF   E           K DISK
005500990805     FFNEVB01L  IF   E           K DISK
005600950324     F*
005700020906     fFnfvv01l  if   e           k Disk
005800131119     ffidst01l  if   e           k Disk
005900990119     FFNDFA01L  IF   E           K DISK
006000010508     F*.......... O. R. M.
006100060216     FFiAR404L  if   e           k DISK    RENAME(FiAR4000:FiAR4004)
006200040218      *.......... T E L E F O N A T E   /   R E C L A M I
006300040218     fFITGD02L  if   e           k disk
006400030527     F*.......... N U O V E   T A B E L L E
006500030527     FTNTBE01L  IF   E           K DISK
006600131108     F*.......... S P U N T E
006700131108     Ffnbrv01l  IF   E           K DISK
006800131114     F
006900131114     Ffipct02l  IF   E           K DISK
007000040218      *------------------------------------------------------------------------*
007100060130     D $pog            s              3    dim(250)
007200060130     D $DIG            s              1    dim(20)
007300060130     D $ARG            s              3    dim(50)
007400060130     d
007500950307     D LBL             S              1    DIM(58) CTDATA PERRCD(58)
007600950307     D LB2             S             58    DIM(1) CTDATA PERRCD(1)
007700941011     D LBD             S             15    DIM(5) CTDATA PERRCD(1)
007800941011     D PRD             S             23    DIM(5) CTDATA PERRCD(1)
007900160316     D MSG             S             78    DIM(40) CTDATA PERRCD(1)
008000030612      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
008100030612     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
008200030612     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
008300070426     D CMA4f6          S              1    DIM(130) CTDATA PERRCD(65)
008400070426     D CMA5f6          S              1    DIM(130) CTDATA PERRCD(65)
008500030612      *
008600991222     D CNM             S              1    DIM(11) CTDATA PERRCD(11)
008700040218     D TF01            s              1    dim(20) ctdata perrcd(20)
008800070411     D TF01B           s              1    dim(20) ctdata perrcd(20)
008900021216     D TF02            S              1    DIM(20) CTDATA PERRCD(20)
009000021216     D TF04            S              1    DIM(20) CTDATA PERRCD(20)
009100021216     D TF05            S              1    DIM(20) CTDATA PERRCD(20)
009200070420     D TF06            S              1    DIM(20) CTDATA PERRCD(20)
009300021216     D TF07            S              1    DIM(20) CTDATA PERRCD(20)
009400021216     D TF08            S              1    DIM(20) CTDATA PERRCD(20)
009500130830     D TF09            S              1    DIM(20) CTDATA PERRCD(20)
009600021216     D TF10            S              1    DIM(20) CTDATA PERRCD(20)
009700021216     D TF11            S              1    DIM(20) CTDATA PERRCD(20)
009800021217     D TF12            S              1    DIM(20) CTDATA PERRCD(20)
009900021216     D TF13            S              1    DIM(20) CTDATA PERRCD(20)
010000021216     D TF14            S              1    DIM(20) CTDATA PERRCD(20)
010100040218     d TF16            s              1    dim(20) ctdata perrcd(20)
010200070404     d TF17            s              1    dim(20) ctdata perrcd(20)
010300021216     D TF19            S              1    DIM(20) CTDATA PERRCD(20)
010400021216     D TF20            S              1    DIM(20) CTDATA PERRCD(20)
010500021216     D TF21            S              1    DIM(20) CTDATA PERRCD(20)
010600110928     D TF22            S              1    DIM(20) CTDATA PERRCD(20)
010700021217     D $TF             S              1    DIM(20)
010800911024     D*
010900131111     D C1              S              1    DIM(30)
011000941212     D L1              S              3  0 DIM(30)
011100060613     D k11             S              1    DIM(11)
011200920403     D CVB             S              1    DIM(23)
011300921102     D SEG             S              1    DIM(15)
011400930705     D KCA             S              1    DIM(50)
011500940912     D DCA             S             44    DIM(50)
011600940907     D*
011700940907     D AAN             S              4  0 DIM(30)
011800940907     D LPN             S              3  0 DIM(30)
011900940907     D NRN             S              2  0 DIM(30)
012000940907     D NSN             S              7  0 DIM(30)
012100941024     D CC1             S              1    DIM(99)
012200941024     D CC2             S             20    DIM(99)
012300020329     D CC3             S              1    DIM(99)
012400020329     D CB1             S              1    DIM(99)
012500020329     D CB2             S             20    DIM(99)
012600030121     D RigaTf1         S              1    DIM(62)
012700030121     D RigaTf2         S              1    DIM(62)
012800030121     D RigaTf3         S              1    DIM(62)
012900030527     D*
013000060130     D cbodop          S              2    DIM(25)
013100040216     D* PASSAGGIO A INTERROGAZIONE VARIAZIONI           - FNLR75R -
013200940720     D* PASSAGGIO A INTERROGAZIONE DETTAGLIO COLLI      - FNLR37R -
013300940712     D PARAM1          DS
013400940714     D  V2CAAS                14     17  0
013500940712     D  COMLNP                18     20  0
013600940714     D  V2CNRS                21     22  0
013700940714     D  V2CNSP                23     29  0
013800940712     D  PA1F03                30     30
013900940713     D  PA1FLG                31     31
014000940713     D  PA1PGM                32     41
014100940714     D  PA1DSA                42     61
014200940809     D  PA1CML                62     62
014300940720     D  PA1DSP                63     70  0
014400940720     D  PA1RMN                71     85  0
014500940720     D  PA1LNA                86     88  0
014600940802     D  PA1LN1                89    108
014700940802     D  PA1RSD               109    143
014800941213     D* SE RICH DA PGM GIAC. = 1
014900941213     D  PA1GIA               144    144
015000941227     D  PA1FEL               145    147  0
015100950628     D  PA1TBO               148    148
015200110929     d  pa1rma               149    163
015300110929     d  pa1xco               164    164
015400980603     D* PASSAGGIO DATI PER CAMBIO AZIENDA
015500980609     D PARAAZ          DS
015600980603     D  CALPGM               201    210
015700060217     D**PARAZI               211    211
015800060217     D**PARF19               212    212
015900980603     D  PARF3                213    213
016000940802     D* RICHIAMO PGM INTERROGAZ.BOLLE PARTENZE            - FNLS04R -
016100940721     D* RICHIAMO PGM INTERROGAZ.BOLLE TRANSITO            - FNLR68R -
016200920203     D PARAM2          DS
016300940722     D  PA2FLG                31     31
016400940722     D  PA2BOL                62     62
016500940802     D* RICHIAMO PGM INTERROGAZ.BOLLE PART.CON KEY SPECIF - FNLS05R -
016600941214     D PARAM0          DS
016700950316     D  PA0AAS                 1      4  0
016800950316     D  PA0LNP                 5      7  0
016900950316     D  PA0NRS                 8      9  0
017000950316     D  PA0NSP                10     16  0
017100941214     D  PA0F03                17     17
017200941214     D  PA0FLG                18     18
017300941214     D  PA0A9                 19     19
017400941214     D  PA0B5                 20     20
017500941214     D  PA075                 21     21
017600941214     D  PA098                 22     22
017700941214     D  PA0RSU                23     42
017800941214     D  PA0CML                43     43
017900941214     D  PA0SIM                44     46  0
018000941214      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
018100941214     D  PA0CLI                47     47
018200941214      * PA0GIA indica se il pgm e' richiamato dalle giacenze
018300941214     D  PA0GIA                48     48
018400940802     D* RICHIAMO PGM DI INTERROGAZIONE SPUNTE FOGLI VARI  - FNLR04R -
018500941230     D PARA04          DS                  INZ
018600940713     D  PA4NFV                 1      5  0
018700940713     D  PA4LNP                 6      8  0
018800940713     D  PA4LNA                 9     11  0
018900940713     D  PA4NRS                12     13  0
019000940713     D  PA4NCD                14     20  0
019100940713     D  PA4NCA                21     27  0
019200940713     D  PA4NPS                28     29  0
019300940713     D  PA4NPG                30     30  0
019400940713     D  PA4FLG                31     31
019500940713     D  PA4PGM                32     38
019600941230     D  PA4FGS                39     41  0
019700101201     d* bolla
019800101201     d  pa4bolp               42     44  0
019900101201     d  pa4bors               45     46  0
020000101201     d  pa4bosp               47     53  0
020100101201     d  pa4boan               54     57  0
020200101201     D  PA4ERR                58     58
020300990119     D                 DS
020400990119     D  DCTDCA                 1      8  0
020500990119     D  DCTAAC                 1      4  0
020600990119     D  DCTMGC                 5      8  0
020700060518     D**               DS
020800060518     D**§11                    1     11  0 dim(11)
020900060518     D**dpdchd                12     12  0
021000060518     D**dpdbrc                 1     12  0
021100940726     D* RICHIAMO PGM DI INTERROGAZIONE EVENTI             - FNLRB5R -
021200950215     D PARAB5          DS                  INZ
021300940726     D  PB5AAS                14     17  0
021400940726     D  PB5LNP                18     20  0
021500940726     D  PB5NRS                21     22  0
021600940726     D  PB5NSP                23     29  0
021700940726     D  PB5F03                30     30
021800940726     D  PB5FLG                31     31
021900940726     D  PB5PGM                32     41
022000940726     D  PB5DSA                42     61
022100940726     D  PB5DSP                63     70  0
022200940726     D  PB5RMN                71     85  0
022300940726     D  PB5LNA                86     88  0
022400940802     D  PB5LN1                89    108
022500940802     D  PB5RSD               109    143
022600940802     D  PB5DCP               144    151  0
022700940802     D  PB5TMC               152    152
022800940712     D                 DS
022900940712     D  MM                     1      2  0
023000940712     D  GG                     3      4  0
023100940712     D  MMGG                   1      4  0
023200901116     D                 DS
023300940713     D  V1CLNA                 1      3  0
023400940713     D  V1CLP1                 4      6  0
023500940713     D  V1CDTD                 7     14  0
023600940713     D  V1CDTA                15     22  0
023700940712     D  SEL1                   1     22  0
023800901116     D                 DS
023900940713     D  V1CLP2                 1      3  0
024000940713     D  V1CNRS                 4      5  0
024100940713     D  V1CNSP                 6     12  0
024200901116     D  SEL2                   1     12  0
024300940712     D                 DS
024400131111     D  V1CRSD                 1     30
024500131111     D  V1CRSM                31     60
024600131111     d  v1cind                61     90
024700131111     d  v1clod                91    120
024800131111     d  v1cnzd               121    123
024900131111     d  v1cprd               124    125
025000131111     D  SEL3                   1    125
025100010508     D                 DS
025200010508     D  V1Cpoe                 1      3  0
025300010508     D  V1CNsr                 4      5  0
025400010508     D  V1CNor                 6     12  0
025500010508     D  V1CNrv                13     14  0
025600010508     D  SEL4                   1     14  0
025700990119     D                 DS
025800990119     D  V2CTSP                 1      1
025900990119     D  V2CGCA                 2      2
026000990119     D  V2CDAN                 3      3
026100990119     D  V2CFLA                 1      3
026200990119     D                 DS
026300990119     D  V2CCON                 1      6
026400990119     D  V2CFCC                 7      7
026500990119     D  V2CCOB                 1      7
026600131111     D**               DS
026700131111     D*+ARBAAS                 1      4  0
026800131111     D*+ARBMGS                 5      8  0
026900131111     D**ARBDSP                 1      8  0
027000131111     D
027100131111     D                 DS
027200131111     D  v1cbarcode             1     15  0
027300131111     D  b_aas                  1      2  0
027400131111     D  b_lnp                  3      5  0
027500131111     D  b_nrs                  6      7  0
027600131111     D  b_nsp                  8     14  0
027700950428     D                 DS
027800950428     D  V3CLNP                 1      3  0
027900950428     D  V3CNRS                 4      5  0
028000950428     D  V3CNSP                 6     12  0
028100950428     D  V3CDSP                13     20  0
028200950428     D  VIDRMN                21     35  0
028300131121     D  V3CLNA                36     38  0
028400950428     D  D15LNA                39     53
028500950428     D  V32RSD                54     85
028600950428     D  COMC08                 1     85
028700131111     d
028800930705     D WLBDAT          DS
028900940712     D  G02DAT                 1      8  0
029000940712     D  G02INV                 9     16  0
029100940712     D  G02ERR                17     17
029200940712     D  G02TGI                18     22  0
029300900517     D CA02DS          DS
029400900517     D  PRIMA                378    379B 0
029500950125     D                 DS
029600950125     D* CAMPO UNICO COMPRENDENTE RAG.SOC.+AGGIUNTIVI 35 CARATTERI
029700950125     D  V4CRSD                 1     35
029800131120     D  V4CNOT                36     70
029900131120     D  V4CNOM                 1     70
030000040218      *?definizione oggetto reclamo come spedizione?
030100131121     d ds_OggSped      ds                  inz
030200131121     d  wARBlnp                       3s 0 inz
030300131121     d  wARBnrs                       2s 0 inz
030400131121     d  wARBnsp                       7s 0 inz
030500131121     d  wARBaas                       4s 0 inz
030600160316     D                 DS
030700160316     D  V1CBCODEB              1     18  0
030800160316     D  wbcodfls               1      3  0
030900160316     D  wbcodlna               4      6  0
031000160316     D  wbcodnrs               7      8  0
031100160316     D  wbcodnsc               9     15  0
031200040218      *
031300040218     d Status         sds
031400040218     d  SDSprm           *parms
031500991027      *
031600040216     D KPJBA         E DS
031700040218     d FIDNA1ds      e ds
031800040225      *
031900040225     d FIDNA2ds      e ds                  inz
032000040225     d  IA2mod       e                     inz('M')
032100040225     d  IA2tor       e                     inz('S')
032200040216      *
032300991029     d YEURCODS      E DS
032400991027     d TIBS69DS      E DS                  INZ
032500021213     d FNLRB3Ds      E DS
032600991027     d DS_cnaco      E DS                  extname(CNACO00F)
032700991027     d DS_cnind      E DS                  extname(CNIND00F)
032800991027     d DS_cnclp      E DS                  extname(CNCLP00F)
032900991027     d DS_fncls      E DS                  extname(FNCLS00F)
033000991027     D DSCV          E DS
033100020402     D DSCC          E DS
033200991027     D DS$2          E DS
033300991027     D DS3A          E DS
033400940909     D DS7O          E DS
033500951220     D DS7K          E DS
033600131108     D DS2A          E DS
033700020627
033800091214     d ds15          e ds
033900020627     d dspr          e ds
034000090604     d ds5e          e ds
034100020627
034200991217     D OG143         E DS
034300070403     D OG148         E DS
034400000615     D DSBL4A        E DS
034500060518     D DSBL4I        E DS
034600991217     D DSBL4E        E DS
034700971219     D DSARBD        E DS                  INZ
034800941014     D DSARBK        E DS                  INZ
034900941014     D DSARBG        E DS                  INZ
035000991011     D DARBT         E DS                  INZ
035100150724     D Diore01       E DS                  INZ
035200150724     d
035300991027     D FIDN00DS      E DS
035400091216     D TRULINTDS     E DS
035500991027     D DS7PQRS       E DS
035600991027     D FNLG30DS      E DS
035700991027     D FNLr44DS      E DS
035800130920
035900131114     d fipctcetds    e ds
036000140224     d fipctcords    e ds
036100140110     D TRULORSDS     E DS                  prefix(S_)
036200131114     D TRULORFDS     E DS
036300130920
036400961121     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
036500991027     D FNLR45DS      E DS
036600950102     D FNLR97        E DS
036700930428     D*
036800930428     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
036900991027     D TNSD99DS      E DS                  INZ
037000941014      *
037100030612     D* DS PER TRUL90R - RICHIESTA STAMPANTI
037200030612     D TRUL90DS      E DS
037300941014      *
037400941212     D* DS PER TRUL06R - CARICAMENTO £X
037500991027     D TRUL06DS      E DS
037600991027     D  LIN                    1     90  0 DIM(30)
037700941212      *
037800941014     D* DS PER FNLR66R - CONTROLLO E VISUALIZZAZIONE CAUSALI VARIAZIONI
037900991027     D FNLR66DS      E DS
038000060224      *
038100060224     D* DS PER FNLv55R - richiamo pgm che cerca i terminal per £6
038200060224     D fnlv55ds      E DS
038300941014      *
038400941014     D* DS PER FNLR48R - MANUTENZIONE BOLLE ARRIVI
038500991027     D FNLR48DS      E DS
038600941014      *
038700941212     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
038800991027     D FNLV50DS      E DS
038900051124     D* DS PER FNLV80R - decodifica cons richiesta
039000051124     D FNLV80DS      E DS
039100131120     D* DS PER FNLRG7R - visulizza altre abilitazioni
039200131120     D fnlrg7ds      E DS
039300070404     D* DS PER FNLRN5R - visulizza note LDV
039400140123     D**** fnlrn5ds      E DS
039500140123     D* DS PER FNLRN6R - visulizza note LDV
039600140123     D fnlrn6ds      E DS
039700070427     D* DS PER FIDG34R - reperisce dati consegna automatica
039800070427     D fidg34ds      E DS
039900070420     D* DS PER           stampa LDV
040000070420     D FNLSB5DS      E DS
040100130308     d FNLSB6ds1     e ds                  inz
040200130319     d  DB6pdf       e                     inz('X')
040300941212      *
040400131119     D DDSTFLR       E DS                  INZ
040500010510     D Dsbl4m        E DS
040600030121     d dTba          e ds
040700031119     d dAr5Gen       e ds
040800030121     d dAr5Ban       e ds
040900030703     d dAr5Bnb       e ds
041000130927     d dAr5emd       e ds
041100030121     d Tibs02Ds      e ds
041200060130     d Azuteds       e ds                  extname(Azute00f)
041300060130     d dDatiute      e ds
041400060130     d dLat          e ds
041500060130     d dute01        e ds
041600060130     D TRUL31DS      E DS
041700060130     D TRUL31DS2     E DS
041800060130     D TIBS34DS      E DS
041900110601     D fnlv79ds      E DS
042000141010      // -?Reperimento Disposizioni di Consegna da WEB
042100141105     d FNLRY09DS2    e ds                  inz
042200040809      * ds per il controllo della presenza di CA per la spedizione richiesta
042300040809     d FIDN12DS      e ds
042400040809     d  skKey                 26    305    dim(20)
042500040809     d  skAnn                306    325    dim(20)
042600040809     d  skDca                326    485  0 dim(20)
042700040809     d  skFca                486    545  0 dim(20)
042800040809     d  skDch                546    705  0 dim(20)
042900040809     d  skCch                706    745    dim(20)
043000040809     d*
043100040809     d dsKey           ds
043200040809     d  dsaac                         4  0
043300040809     d  dsfil                         3  0
043400040809     d  dsnca                         7  0
043500160115
043600160115     D fnblpinf        ds
043700160115     D  blpnrr               397    400i 0
043800131111
043900131111       // -?File bolle arrivi
044000131111     d FNARBds       e ds                  extname(FNARB00F)
044100131111     d
044200131111     d wEnd            s               n   inz(*off)
044300040809
044400991027      *
044500991027     D W017A           s             17
044600991027     D w0130           s             13  0
044700991027     D w0133           s             13  3
044800991027     D IMPeditato      s             13
044900060130     D savkpjbu        S                   LIKE(kpjbu)
045000060130     D kscfil          S                   LIKE(arblna)
045100060130     D War4aas         S                   LIKE(AR4aas)
045200000228     D War4lnp         S                   LIKE(AR4lnp)
045300000228     D War4nrs         S                   LIKE(AR4nrs)
045400000228     D War4nsp         S                   LIKE(AR4nsp)
045500991027     D Wsvn            S                   LIKE(AR7svn)
045600991027     D Wvan            S                   LIKE(AR7van)
045700991027     D Wdiv            S                   LIKE(AR6div)
045800991029     D Wariimp         S                   LIKE(ARIimp)
045900991029     D Wariipp         S                   LIKE(ARIipp)
046000020906     d wfgs            s                   Like(Fvvfgs)
046100991027     D DATA_eur        S               D   DATFMT(*eur)
046200021203     d kpdpdr          S                   LIKE(APDpdr)
046300021203     d kpdtip          S                   LIKE(APDtip)
046400131120     d kndc6           S                   LIKE(dstnfv)
046500140110     d s_arbdei        S                   LIKE(d98dei)
046600140110     d s_arboei        S                   LIKE(d98oei)
046700150728     d s_arbttc        S                   LIKE(d98ttc)
046800150728     d s_arbtcc        S                   LIKE(d98tcc)
046900151019     d s_arbflp        S                   LIKE(d98flp)
047000131114     D arbrsduni       S             70
047100131114     D arbdsp          S              8  0
047200131111     D W0080           S              8  0
047300991027     D W0040           S              4  0
047400991027     D wtibs69r        s              1
047500991027     D wtstdec         s              1
047600131120     D Wvideo          s              1
047700131209     D WopenC          s              1
047800020225     d lnantw          s                   like(§ogntw)
047900020225     d lnpntw          s                   like(§ogntw)
048000020910     d Wifp            s                   Like(Arbifp) Inz
048100060130     d wabi            s                   like(UteAut)
048200030703
048300030703     d wtba            s                   Like(§Ar5Tba)
048400030703     d wdestba         s                   Like(VidTba)
048500061018     d savnzd          s                   inz  like(v1cnzd)
048600020628
048700091216     D W_cdi           S                   LIKE(§ar5cdi)
048800020628     D ordina          S              1
048900020628     D comando         S              1
049000070420     D wrichtrul90     S              1
049100070403     d Dataeur         s               d   datfmt(*eur)
049200070403     d Datadmy         s               d   datfmt(*dmy)
049300110929     d s_param1        s                   like(param1)
049400110929     d no_f22          s              1    inz(' ')
049500110930     d comrma          s                   like(arbrma)
049600131108     d Wcol_D          s              5  0
049700131108     d Wcol_I          s              5  0
049800131108     d Wcarat          s              3  0
049900131108     d Indx            s              3  0
050000131209     d AP              s              3  0
050100131108     d wx              s              3  0
050200131007     d wy              s              3  0
050300131209     d w060a           s             60
050400131209     d wabif9          s              1
050500141010     d $Fine           s              1    inz(*off)
050600160316     d pos             s              2  0
050700141010       // -?Reperimento Disposizioni di consegna da WEB
050800141010     d fnlry09r        pr                  extpgm('FNLRY09R')
050900141013     d  kpjba                              likeds(kpjba)
051000131111     d
051100131111       // -?Stringa SQL da eseguire
051200131111     d wSQL            s           2048    Varying        inz
051300030527     D*
051400030527     D wrkoggiiso      s               d
051500160115     D wchainblp       s              1
051600991027      *
051700930705     D* DEFINIZIONI COSTANTI
051800930705     D*
051900930705     D CNOSEQ          C                   CONST('NON SEQUENZIALI')
052000941014     D CINTER          C                   CONST('* INTERROGAZIONE BOL-
052100941014     D                                     LE ARRIVI *')
052200941014     D CMANUT          C                   CONST('** MANUTENZIONE BOLL-
052300941014     D                                     E ARRIVI **')
052400021216     D CF2413          C                   CONST('F24=AlTasti(1/3)')
052500021216     D CF2423          C                   CONST('F24=AlTasti(2/3)')
052600021216     D CF2433          C                   CONST('F24=AlTasti(3/3)')
052700021216     D CF2412          C                   CONST('F24=AlTasti(1/2)')
052800021216     D CF2422          C                   CONST('F24=AlTasti(2/2)')
052900110705     d
053000110705     D* CAMPO PER DSPATR "NO DISPLAY"
053100110705     D NODI            C                   CONST(X'27')
053200131120     D* CAMPO PER DSPATR "RI"
053300131120     D RI              C                   CONST(X'21')
053400131127     D* CAMPO PER DSPATR "HI"
053500131127     D hi              C                   CONST(X'22')
053600160316
053700160316     D digits          c                   '0123456789'
053800900518     C****************************************************************
053900900518     C*  RIEPILOGO INDICATORI
054000900518     C***************************************************************
054100911023     C* 01    - SELEZIONE PER LINEA ARRIVO
054200911023     C* 02    - SELEZIONE PER LINEA PARTENZA
054300911105     C* 03    - SPEDIZIONE GIACENTE
054400990119     C* 04    - ABILITO TASTO FUNZIONALE F2 - INT C.A.
054500990119     C* 05    - ABILITO TASTO FUNZIONALE F19- SPUNTE
054600070411     c* 06    - on per tassazione 2 bolla emette scritta "ImportoBOLLO"
054700070411     c*       - offper tassazione 2 bolla emette scritta "Importo IVA"
054800911023     C* 07    - PARZIALIZZ. SEGNACOLLO DAL
054900000216     C* 08    - bolla dpd
055000131114     C* 09    - scelta in altre parzializz
055100961111     C* 10    - BOLLA DI RIENTRO
055200911023     C* 11    - PARZIALIZZ. MITTENTE
055300131120     C* 12    -
055400961111     C* 13    - SPEDIZIONE IN CONSEGNA IN EVIDENZA IN SUBFILE
055500911023     C* 14    - PARZIALIZZ. DESTINATARIO
055600940318     C* 15    - DATA DAL UGUALE A DATA AL (SCELTO UN SOLO GIORNO)
055700961112     C* 16    - SI TRATTA DI BOLLA EXPORT
055800070403     C* 17    - p.o. utente e lna bolla hanno GEO attiva
055900911023     C* 18/21 - STATO SPEDIZIONI SELEZIONATO
056000991027     C* 22    - ESISTE ESTENSIONE BOLLA 2 e ABILITO IL TASTO  F4=TASSAZIONE 2
056100950825     C* 23    - SFLEND
056200950825     C* 24    - ABILITO IL TASTO "F13=TASSAZIONE"
056300010508     C* 25    - SELEZIONE PER NUMERO O.R.M.
056400070404     C* 26    - presente FIARP
056500070403     C* 27    - presente blocco consegna ARBFBC<>' '
056600941028     C* 28    - VISUALIZZA MSG ERRORE
056700911129     C* 29    - ESISTE CONTRASSEGNO
056800911023     C* 30/31 - LETTURA COMODO
056900911023     C* 32    - BOLLA LETTA DA SCARTARE
057000911023     C* 33    - BOLLA IN FRANCO
057100911023     C* 34    - LETTURA SUBFILE
057200911023     C* 35    - PULIZIA SUBFILE
057300911129     C* 36    - LETTURE
057400911023     C* 37    - ROLL UP
057500991027     C* 38    - ESISTE TB2 ANCHE SE NON PRESENTE TIPO RECORD '2' IN FIAR600F
057600920203     C* 39    - ABILITAZIONE TASTO DI COMANDO INT. BOLLE PARTENZE
057700131111     C* 40-59 - ERRORI
057800131111     C* 60    - email destiantario
057900131111     C* 61    - PROGRAMMA RICHIAMATO per scelta bolla
058000930407     C* 62    - ALTA INTENSITA' PER SPED BLOCCATE
058100040218     C* 63    - Abilita F1  = Aggiunta in reclamo
058200040218     C* 64    - Abilita F16 = Gestione reclamo
058300091214     c* 65    - Abilita visualizzazione beneficiario C/Ass
058400131127     C* 66    - libero - NON usato
058500960903     C* 67    - ABILITAZIONE TASTO DI COMANDO F7=ANOMALIE
058600131127     C* 68    - pos cursore RMN prima serv x  EMETTERE F23 PER CALCOLARE E VISUALIZZARE DATA CONS.
058700930505     C*         PREVISTA E LE ORE DI RITARDO
058800930526     C* 69    - DATA CONSEGNA MERCE >< 0
058900930510     C* 70    - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
059000990119     C* 71    - ABILITAZIONE TASTO DI COMANDO F8=EVENTI SEGNACOLLI
059100131107     C* 72    - SFLDSPCTL  LRI6S05
059200991027     C* 73    - BOLLA FRANCO FRONTIERA
059300970227     C* 74    - ABILITA EVENTI SEGNACOLLI
059400970227     C* 75    - UTILIZZATO PER NON ESEGUIRE LA SUBROUTINE "LEGBOL"
059500970227     C* 76    - SI DDT IN ALTA INTENSITA'
059600021213     C* 77    - NOTE SUPERMERCATI
059700970227     C* 78    - SFLDSP
059800950825     C* 79    - SFLDSPCTL
059900930510     C* 80    - NEL SFL DEI LEGAMI NON EMETTO IL CAMPO PER LA SELEZIONE
060000941028     C* 81    - USATO NEL VIDEO LEGAMI
060100941028     C* 82    - USATO NEL VIDEO LEGAMI
060200941028     C* 83    - USATO NEL VIDEO LEGAMI
060300970925     C* 84    - PARZIALIZZ. SEGNACOLLO AL
060400971219     C* 85    - VISUALIZZO RIFERIMENTO PARTNER ESTERO
060500111005     c* 86    - Abilitazione F22="PERFECT ORDER"
060600111005     C* 87    - Pgm richiamato in modalità "W" (Perfect Order Wurth)
060700110705     C* 88    -
060800931112     C* 89    - UTILIZZATO PER VISUALIZZARE ALTRI TASTI FUNZIONALI
060900920402     C* 90    - INDICA LA PRESENZA DI UN ERRORE
061000941028     C* 91    - ERRORE QCMDEXC (UTILIZZABILE)
061100940923     C* 92/94 - LEGAME BOLLA
061200930525     C* 95    - ESISTONO EVENTI DA INTERROGARE
061300131111     C* 96    -
061400991222     C* 97    - MANUTENZIONE BOLLE ARRIVI
061500941014     C* 98    - ESISTONO VARIAZIONI BOLLE EFFETTUATE
061600930504     C* 99    - OFF  ESISTE LEGAME BOLLA SEGUENTE
061700900518     C*****************************************************************
061800000000     C     *ENTRY        PLIST
061900000000     C                   PARM                    KPJBA
062000040218     c                   parm                    FIDNA1ds
062100131210     c
062200131210      /free
062300131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
062400131210      /end-free
062500980609     C**
062600940712     C                   MOVEL     KPJBU         PARAM1
062700980609     C                   MOVEL     KPJBU         PARAAZ
062800940713     C*
062900060130     C                   Z-ADD     1             CODUT             1 0
063000060130     c                   Movel     rsut          V1cdsa
063100060130     C                   MOVEL     rsut          PARDUT           20
063200941227     C*
063300941227     C                   MOVEL     SIMFEL        PA1FEL
063400990701     C                   MOVEL     V1CDSA        PA1DSA
063500940713     C*-------------------------------------------------------------------------
063600911024     C                   EXSR      CARTAB
063700131111
063800131111     c                   eval      Wend=*off
063900110930     c*
064000111005     c                   setoff                                       87
064100110930     c                   if        pa1flg='W'
064200110930     c                   eval      comrma=pa1rma
064300111005     c                   eval      *in87=*on
064400110930     c                   endif
064500110930     c* disabilito f22=PerfectOrder se richiamato da sè stesso
064600131107     c                   if        pa1pgm='FNLRI6R'
064700110929     c                   eval      no_f22='N'
064800110929     c                   endif
064900101202     c                   if        pa1pgm=*blanks
065000131107     C                   MOVEL     'FNLRI6R'     PA1PGM
065100101202     c                   endif
065200940713     C                   MOVE      'N'           V1CAFF
065300941215     C                   MOVEL     '? '          V1CCVB
065400941215     C                   CLEAR                   V1DCVB
065500941014     C* TESTATA
065600941014     C                   MOVEL     CINTER        V1CTES
065700131127     c                   clear                   v1clna
065800940317      *
065900940712      * IMPOSTO LA LINEA DI ARRIVO
066000060217     c* Non la imposto se richiamato con key specifica
066100040217if  1C     PA1FLG        IFNE      '1'
066200060217     C     PA1FLG        ANDNE     '3'
066300060217     C     PA1FLG        ANDNE     'C'
066400060217     C     PA1FLG        ANDNE     'N'
066500960903     C     PA1FLG        ANDNE     'P'
066600961121     C     PA1FLG        ANDNE     'E'
066700110929     C     PA1FLG        ANDNE     'W'
066800060217     c* o se vengo da int bolle normale (per assistenza)
066900060217     c     pa1flg        andne     ' '
067000060217     c     pa1flg        andne     '4'
067100060130     c                   z-add     dutpou        v1clna
067200020424     c*
067300040121     C                   MOVE      V1CLNA        NUM3              3 0
067400940712     C     NUM3          CHAIN     AZORG01L                           90
067500940713     C  N90              MOVEL     ORGDES        V1CDEA
067600040217e   1C                   ENDIF
067700930702     C*
067800920703     C                   EXSR      PUL01
067900940809     C                   MOVEL     'S'           WRIEM             1
068000930702     C**
068100930702     C* PGM RICHIAMATO
068200920326     C**
068300940713     C** PA1FLG =   - NON RICHIAMATO
068400920423     C**        = 1 - RICHIAMATO CON   KEY SPECIFICA
068500920423     C**        = 2 - RICHIAMATO SENZA KEY SPECIFICA PER   SELEZ BOLLA
068600920423     C**        = 3 - RICHIAMATO CON   KEY SPECIFICA DA INT-BOLL PART
068700920423     C**        = 4 - RICHIAMATO SENZA KEY SPECIFICA SENZA SELEZ BOLLA
068800990129     C**        = C - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F2=C.A
068900941014     C**        = M - MANUTENZIONE BOLLE ARRIVI
069000960903     C**        = N - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F7=ANOMA
069100960903     C**        = P - RICHIAMATO CON   KEY SPECIFICA DA INT-BOLL PART
069200960903     C**              E DISABILITAZ F7=ANOMALIE
069300961121     C**        = E - RICHIAMATO CON   KEY SPECIFICA E DISABIL.F8=EV.SEGNACOLLI
069400110930     C**        = W - RICHIAMATO SENZA KEY SPECIFICA per "PerfectOrder"
069500960903     C*
069600040218      *?Abilito F1=Aggiunta in Reclami?
069700040218      * (se richiamato da pgm. FIDNA1R...)
069800040220     c                   eval      *in63  = *off
069900040303     c                   if            SDSprm >  1
070000040308     c                             and IA1ins = ' '
070100040220     c                   eval      *in63  = *on
070200060301     c                   clear                   v1clna
070300060301     c                   clear                   v1cdea
070400111003     c                   endif
070500960903     C* ABILITO F7=ANOMALIE SE FLAG DIVERSO DA 'N' E 'P'
070600960903     C     PA1FLG        IFNE      'N'
070700960903     C     PA1FLG        ANDNE     'P'
070800960903     C                   SETON                                        67
070900040217     C                   ENDIF
071000961121     C* ABILITO F8=EVENTI SEGNACOLLI  SE FLAG DIVERSO DA 'E'
071100961121     C     PA1FLG        IFNE      'E'
071200961121     C                   SETON                                        74
071300040217     C                   ENDIF
071400960903     C*
071500040217if  1C     PA1FLG        IFEQ      '1'
071600960903     C     PA1FLG        OREQ      'N'
071700940713     C     PA1FLG        OREQ      '3'
071800960903     C     PA1FLG        OREQ      'P'
071900961121     C     PA1FLG        OREQ      'E'
072000990129     C     PA1FLG        OREQ      'C'
072100920703     C*
072200940714     C                   MOVEL     V2CAAS        V1CAAS
072300940713     C                   MOVEL     COMLNP        V1CLP2
072400940714     C                   MOVEL     V2CNRS        V1CNRS
072500940714     C                   MOVEL     V2CNSP        V1CNSP
072600920703     C* SE NO PASSATA KEY SPECIFICA SI PUO' SELEZIONARE
072700940713     C     PA1FLG        IFEQ      '1'
072800960903     C     PA1FLG        OREQ      'N'
072900961121     C     PA1FLG        OREQ      'E'
073000990129     C     PA1FLG        OREQ      'C'
073100070411     c                   if        v1cnsp=0
073200920703     C                   SETON                                        61
073300040217     C                   ENDIF
073400070411     C                   ENDIF
073500040217x   1C                   ELSE
073600110929     c* Se richiamata con "W" devo saltare tutta la gestione della
073700110929     c* prima videata e devo passare subito al caricamento del sfl
073800110929   1ac     pa1flg        ifne      'W'
073900911213     C*
074000940713     C     PA1FLG        IFEQ      '2'
074100911213     C                   SETON                                        61
074200040217     C                   ENDIF
074300920203     C*
074400980603     C                   SETOFF                                       05
074500040217if  2C     PA1FLG        IFEQ      ' '
074600920203     C                   SETON                                        39
074700040217e   2C                   ENDIF
074800941014     C*
074900000616     C* MANUTENZIONE BOLLE ARRIVI 97=ON
075000040217if  2C     PA1FLG        IFEQ      'M'
075100030612      *
075200941014     C                   SETON                                        97
075300030612     C                   MOVEL     CMANUT        V1CTES
075400070420     c
075500040217e   2C                   ENDIF
075600160316
075700160316     C* MANUTENZIONE BOLLE ARRIVI PER VEDI PACCO DPD 97=ON
075800160316if  2C     PA1FLG        IFEQ      'V'
075900160316      *
076000160316     C                   SETON                                        97
076100160316     C                   MOVEL     CMANUT        V1CTES
076200160316     c
076300160316e   2C                   ENDIF
076400000703     C*
076500941209     C     INIZIO        TAG
076600131114     C***                EXSR      PUL01
076700131114
076800131114     c* se cursore aperto --> lo chiudo
076900131209     c*****              if        Wend=*on  or *in14 or *in11
077000131209     c                   if        WopenC='1'
077100131114     c                   exsr      closecursor
077200131114     c                   endif
077300911025     C*
077400900509     C     FOR01         TAG
077500941209     C*
077600040217if  2C     WRIEM         IFEQ      'S'
077700131107     C                   WRITE     LRI6T01
077800940720     C                   MOVEL     *BLANKS       WRIEM
077900040217e   2C                   ENDIF
078000160316     C                   IF        PA1FLG='V'
078100160316     C                   EXFMT     LRI6D01D
078200160316     C                   ELSE
078300131107     C                   EXFMT     LRI6D01
078400160316     C                   ENDIF
078500940713     C*
078600940713     C                   SETOFF                                       28
078700940715     C                   MOVEL     *BLANKS       WRIEM
078800940713     C*
078900980603     C** F3 - FINE LAVORO
079000040217if  2C     *INKC         IFEQ      '1'
079100980603     C   05              MOVEL     '1'           PARF3
079200980603     C                   GOTO      FINE
079300040217e   2C                   ENDIF
079400131115     C**
079500131115     C** F5 - Altre parzializzazioni
079600131115     c                   if        *inke=*on
079700131115     c                   exsr      AltrePar
079800131115     C                   MOVEL     'S'           WRIEM
079900131115     C                   GOTO      FOR01
080000131115     c                   endif
080100131114     C**
080200980603     C** F7 - INTERROGAZIONE BOLLE PARTENZE
080300040217if  2C     *INKG         IFEQ      *ON
080400930922     C                   EXSR      INTPAR
080500940722     C                   MOVEL     'S'           WRIEM
080600920203     C                   GOTO      FOR01
080700040217e   2C                   ENDIF
080800131114     C**
080900131114     C** F10- pulizia di tutti i campi di scelta
081000131114if  2C     *INKj         IFEQ      *ON
081100131114     C                   EXSR      pul01
081200131114     C                   MOVEL     'S'           WRIEM
081300131114     C                   GOTO      FOR01
081400131114e   2C                   ENDIF
081500930922     C**
081600980603     C** F14- INTERROGAZIONE BOLLE TRANSITO
081700040217do  2C   KN              DO
081800940722     C                   MOVEL     'T'           PA2BOL
081900920423     C                   MOVEL     PARAM2        KPJBU
082000940721     C                   CALL      'FNLR68R'
082100920423     C                   PARM                    KPJBA
082200940722     C                   MOVEL     'S'           WRIEM
082300920423     C                   GOTO      FOR01
082400040217e   2C                   ENDDO
082500911025     C*
082600110929e  1ac                   endif
082700040217e   1C                   ENDIF
082800900509     C* CONTROLLI FORMATO1
082900110930     c     pa1flg        ifne      'W'
083000160316     C     pa1flg        IFEQ      'V'
083100160316     C                   EXSR      sr_CtrVediPDPD
083200160316     c                   else
083300900509     C                   EXSR      CONTR
083400160316     c                   endif
083500900509     C   90              GOTO      FOR01
083600110929     c                   endif
083700911025     C**
083800911025     C** P E R   N U M E R O   S P E D I Z I O N E
083900131111     c**  o      B A R C O D E
084000040217if  1C     SEL2          IFNE      0
084100131111     c     v1cbarcode    orgt      0
084200971223     C                   EXSR      SINGOL
084300971223     C     WINIZ         IFEQ      '1'
084400971223     C                   GOTO      INIZIO
084500971223     C                   ENDIF
084600971223     C     WFINE         IFEQ      '1'
084700971223     C                   GOTO      FINE
084800971223     C                   ENDIF
084900040217e   1C                   ENDIF
085000911025     C**
085100911025     C**  P E R   S U B F I L E
085200911023     C                   EXSR      ELAB
085300941014     C*
085400901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
085500040217if  1C     NR2           IFEQ      0
085600940713     C                   MOVEL     MSG(1)        V1CMSG
085700131115     C                   SETON                                        28
085800131115     c                   select
085900131115     c                   when      v1crpe<>*blanks
086000131115     C                   SETON                                        58
086100131115     c                   when      v1crmn>0 or v1crma<>*blanks
086200131127     C                   SETON                                        68
086300131115     c                   when      v1cnot<>*blanks
086400131115     C                   SETON                                        62
086500131115     c                   when      v1cprd<>*blanks
086600131115     C                   SETON                                        50
086700131115     c                   other
086800131115     C                   SETON                                        51
086900131115     c                   endsl
087000900516     C                   GOTO      FOR01
087100040217e   1C                   ENDIF
087200971223     C**
087300040217if  1C     V1CRPE        IFNE      *BLANKS
087400010509     C     NR2           ANDEQ     1
087500010509     c     *in25         oreq      *on
087600971223     C     NR2           ANDEQ     1
087700111011     c     v1crmn        orgt      *zeros
087800111011     C     NR2           ANDEQ     1
087900111011     c     v1crma        orne      *blanks
088000111011     C     NR2           ANDEQ     1
088100131111     c     v1cnot        orne      *blanks
088200131111     C     NR2           ANDEQ     1
088300111011     C                   MOVEL     v2clnp        COMLNP
088400111011     c     karb          chain     fnarb01l
088500971223     C                   EXSR      SINGOL
088600040217     C     WINIZ         IFEQ      '1'
088700971223     C                   GOTO      INIZIO
088800040217     C                   ENDIF
088900040217     C     WFINE         IFEQ      '1'
089000971223     C                   GOTO      FINE
089100040217     C                   ENDIF
089200040217e   1C                   ENDIF
089300930922     C**
089400900518     C     FORCT2        TAG
089500131107     C                   WRITE     LRI6T01
089600131107     C                   WRITE     LRI6Z02
089700900515     C** COMANDI SUBFILE
089800900515     C     FORCT         TAG
089900131107     C                   EXFMT     LRI6C02
090000940923     C                   SETOFF                                       28
090100941109     C                   CLEAR                   PA1F03
090200980603     C* F3 - FINE
090300980603     C     *INKC         IFEQ      '1'
090400980603     C   05              MOVEL     '1'           PARF3
090500980603     C                   GOTO      FINE
090600980603     C                   ENDIF
090700930922     C*
090800980603     C* F12 - RITORNO
090900040217if  1C     *INKL         IFEQ      *ON
091000110930     c     pa1flg        cabeq     'W'           fine
091100940713     C     PA1FLG        IFEQ      '3'
091200960903     C     PA1FLG        OREQ      'P'
091300940713     C     PA1FLG        OREQ      '1'
091400960903     C     PA1FLG        OREQ      'N'
091500961121     C     PA1FLG        OREQ      'E'
091600990129     C     PA1FLG        OREQ      'C'
091700940714     C                   Z-ADD     0             V2CAAS
091800920702     C                   Z-ADD     0             COMLNP
091900940714     C                   Z-ADD     0             V2CNRS
092000940714     C                   Z-ADD     0             V2CNSP
092100930922     C                   ENDIF
092200930922     C                   GOTO      INIZIO
092300040217e   1C                   ENDIF
092400900515     C* ROLLUP
092500040217if  1C     *IN37         IFEQ      *ON
092600940713     C   30              DO
092700940713     C                   MOVEL     MSG(15)       V1CMSG
092800940713     C                   SETON                                        28
092900940713     C                   ENDDO
093000930922     C  N30              EXSR      CARSF
093100940713     C                   Z-ADD     COMOD         REC2
093200930922     C                   GOTO      FORCT
093300040217e   1C                   ENDIF
093400900515     C*
093500900516     C** CONTROLLO SE EFFETTUATA UNA SCELTA
093600940713     C                   Z-ADD     PRIMA         REC2
093700131107     C                   READC     LRI6S02                                34
093800911023     C* VISUALIZZO BOLLA
093900040217do  1C     *IN34         DOWEQ     *OFF
094000940714     C                   MOVEL     *BLANKS       V2CFLG
094100911023     C*
094200040217if  2C     V2CSCE        IFEQ      '5'
094300940714     C                   Z-ADD     V2CLNP        COMLNP
094400940712     C     KARB          CHAIN     FNARB01L                           31
094500930518     C*
094600940714     C                   MOVEL     *BLANKS       V2CSCE
094700040217     C     V2CCON        IFEQ      'IN CON'
094800930518     C                   SETON                                        13
094900040217     C                   END
095000930518     C*
095100040217     C     ARBCCA        IFNE      ' '
095200930518     C     ARBFBC        ORNE      ' '
095300930518     C                   SETON                                        62
095400040217     C                   END
095500941014     C*
095600131107     C                   UPDATE    LRI6S02
095700911023     C*
095800911023     C                   EXSR      BOLLA
095900911023     C                   EXSR      VISUAL
096000911023     C*
096100040217x   2C                   ELSE
096200941014     C*
096300941014     C* 97 ON - MANUTENZIONE BOLLE
096400040217if  3C     V2CSCE        IFEQ      '1'
096500941014     C*
096600040217if  4C     *IN97         IFEQ      *ON
096700060227     c                   movel     v2clna        w003a             3
096800060224     C     w003a         LOOKUP    $pog                                   31
096900060130    2c                   if        not *in31 and d66pocl='S'
097000060130     c     v2ccbo        lookup    cbodop                                 31
097100060130    3c                   if        not *in31
097200060130     c                   movel     v2cksc        kscfil
097300060130   x3c                   else
097400060130     c* Se bolla doppia prendo il ksc dall'AR6
097500060130     C                   Z-ADD     v2cAAS        KAAS
097600060130     C                   Z-ADD     v2cLNP        KLNP
097700060130     C                   Z-ADD     v2cNRS        KNRS
097800060130     C                   Z-ADD     v2cNSP        KNSP
097900060130     C                   MOVEL     '2'           KTRC
098000060130     C     KAR61         CHAIN     FIAR6000                           31
098100060130if  4C                   IF          %FOUND(fiar601l)
098200060130     c                   movel     ar6ksc        kscfil
098300060130     c                   else
098400060130     c                   movel     999           kscfil
098500060130    4c                   endif
098600060130    3c                   endif
098700130314     c*
098800060224     c                   movel     kscfil        w003a
098900060224     c     w003a         lookup    $pog                                   31
099000060130    2c                   endif
099100130314     c*
099200040217if  5C     *IN31         IFEQ      *OFF
099300941212     C                   MOVEL     MSG(19)       V1CMSG
099400941212     C                   SETON                                        599028
099500941212     C                   MOVEL     '2'           V2CFLG
099600130314     c*
099700040217x   5C                   ELSE
099800141010
099900130314     c*
100000991027     C                   CLEAR                   FNLR48DS
100100060224     c* come p.o. gestione prendo il p.o. gestore (da £6) della linea
100200060224     c*  di arrivo
100300060224     c                   clear                   fnlv55ds
100400060224     c                   movel     v2clna        d55lin
100500060224     c                   movel     '6'           d55tpt
100600060224     C                   time                    wrkoggiiso
100700060224     C     *iso          move      wrkoggiiso    d55drf
100800060224     c                   call      'FNLV55R'
100900060224     c                   parm                    fnlv55ds
101000060224     c                   movel     d55tfa        d48fgs
101100130314     c*
101200941014     C                   MOVEL     V2CAAS        D48AAS
101300941014     C                   MOVEL     V2CLNP        D48LNP
101400941014     C                   MOVEL     V2CNRS        D48NRS
101500941014     C                   MOVEL     V2CNSP        D48NSP
101600141010     c* routine per verifica dispo da esaminare
101700141010     c                   exsr      sr_wdwdc
101800141010
101900941014     C                   MOVEL     V2CCBO        D48CBO
102000941014     C                   MOVEL     V1CCVB        D48CVB
102100950616     C                   MOVEL     'V'           D48FFR
102200941227     C                   MOVEL     'A'           D48TBO
102300950607     C                   MOVEL     'S'           D48F12
102400941014     C*
102500991027     C                   MOVEL     FNLR48DS      KPJBU
102600941014     C                   CALL      'FNLR48R'
102700941014     C                   PARM                    KPJBA
102800941212     C                   PARM                    DSARBD
102900941212     C                   PARM                    DSARBK
103000991011     C                   PARM                    DARBT
103100941212     C                   PARM                    DSARBG
103200030612     C                   PARM                    trul90ds
103300941212     C*
103400991027     C                   MOVEL     KPJBU         FNLR48DS
103500941212     C*
103600941014     C* ERRORE IN MANUTENZIONE BOLLE
103700040217     C     D48ERR        IFEQ      '1'
103800941014     C                   MOVEL     D48MSG        V1CMSG
103900941014     C                   SETON                                        592890
104000941014     C                   MOVEL     '2'           V2CFLG
104100040217     C                   ENDIF
104200950114     C*
104300950114     C* SE NON C'E' ERRORE AGGIORNO IL SFL CON I DATI DELLA BOLLA VAR
104400040217     C     *IN90         IFEQ      *OFF
104500950120     C                   Z-ADD     V2CLNP        COMLNP
104600950114     C     KARB          CHAIN     FNARB01L                           31
104700950114     C                   MOVEL     ARBRSM        V2CRSM
104800950114     C                   MOVEL     ARBRSD        V2CRSD
104900131114     c                   if        v1cind=*blanks
105000131114     c                   eval      v2cdest=arblod
105100131114     c                   else
105200131114     c                   eval      v2cdest=arbind
105300131114     c                   endif
105400131114     c
105500040217     C                   ENDIF
105600040217e   5C                   ENDIF
105700941014     C*
105800040217x   4C                   ELSE
105900941014     C*
106000911023     C* SELEZIONATA BOLLA: PASSO KEY ED ESCO DA PGM
106100040217     C     *IN61         IFEQ      *ON
106200940714     C                   Z-ADD     V2CLNP        COMLNP
106300940714     C                   MOVEL     '3'           V2CFLG
106400040220      *?- Richiamato da pgm. FIDNA1R?
106500040220if  6c                   if        SDSprm  > 1
106600131111     c                   eval      wARBlnp  = V2Clnp
106700131111     c                   eval      wARBnrs  = V2Cnrs
106800131111     c                   eval      wARBnsp  = V2Cnsp
106900040220     c                   eval      wARBaas = V2Caas
107000040220     c                   clear                   FIDNA1ds
107100040220     c                   eval      IA1ins  = 'S'
107200040220     c                   eval      IA1tor  = 'S'
107300040220     c                   eval      IA1ogg  = ds_OggSped
107400040220e   6c                   endif
107500040217     C                   ENDIF
107600040217e   4C                   ENDIF
107700040217e   3C                   ENDIF
107800930518     C*
107900040217do  3C  N61              DO
108000940714     C                   MOVEL     *BLANKS       V2CSCE
108100040217     C     V2CCON        IFEQ      'IN CON'
108200930518     C                   SETON                                        13
108300930518     C                   ELSE
108400040217     C     V2CCON        IFNE      *BLANKS
108500940714     C                   TESTN                   V2CCON               31
108600930518     C  N31              SETON                                        62
108700040217     C                   END
108800040217     C                   END
108900131107     C                   UPDATE    LRI6S02
109000040217e   3C                   ENDDO
109100930518     C*
109200040217e   2C                   ENDIF
109300911022     C*
109400941014     C                   SETOFF                                       136259
109500911022     C*
109600911022     C* SE FLAG 2 TORNO AL SUBFILE
109700040217     C     V2CFLG        IFNE      *BLANKS
109800900530     C                   SETON                                        34
109900900530     C                   ELSE
110000131107     C                   READC     LRI6S02                                34
110100040217     C                   ENDIF
110200040217e   1C                   ENDDO
110300941014     C*
110400911023     C* FLG =  3 ==> FILE PGM E PASSO KEY
110500940714     C     V2CFLG        IFEQ      '3'
110600911023     C                   GOTO      FINE
110700911023     C                   ELSE
110800911023     C* FLG >< 2 ==> TORNO A SUBFILE
110900911023     C                   GOTO      FORCT2
111000040217     C                   ENDIF
111100911023     C*
111200000000     C     FINE          TAG
111300930504     C*
111400930702     C* CALL A VUOTO AI PGM  PER CHIUDERE I FILES
111500941214     C     PA098         IFEQ      '1'
111600930504     C                   MOVEL     'C'           D98TLA
111700970925     C                   CALL      'TNSD99R'
111800991027     C                   PARM                    TNSD99DS
111900040217     C                   ENDIF
112000930702     C*
112100941214     C     PA075         IFEQ      '1'
112200940714     C                   MOVEL     'C'           PA1FLG
112300940712     C                   MOVEL     PARAM1        KPJBU
112400940714     C                   CALL      'FNLR75R'                            99
112500930524     C                   PARM                    KPJBA
112600040217     C                   ENDIF
112700930702     C*
112800941214     C     PA0B5         IFEQ      '1'
112900950215     C                   CLEAR                   PARAB5
113000940726     C                   MOVEL     'C'           PB5FLG
113100940726     C                   MOVEL     PARAB5        KPJBU
113200940722     C                   CALL      'FNLRB5R'
113300930525     C                   PARM                    KPJBA
113400040217     C                   ENDIF
113500941014     C*
113600941014     C     *IN97         IFEQ      *ON
113700941014     C                   MOVEL     'C'           D66TLA
113800991027     C                   MOVEL     FNLR66DS      KPJBU
113900941014     C                   CALL      'FNLR66R'
114000941014     C                   PARM                    KPJBA
114100040217     C                   ENDIF
114200991027      *
114300991027     C                   IF        Wtibs69r  <>  *BLANKS
114400991027     C                   eval      I69TLA  = 'C'
114500991027     C                   call      'TIBS69R'
114600991027     C                   parm                    TIBS69DS
114700991027     c                   ENDIF
114800930504     C*
114900941109     C* PER F3=FINE PULISCO CHIAVE BOLLA E IMPOSTO FLAG F3
115000941109     C     *INKC         IFEQ      *ON
115100941109     C                   Z-ADD     *ZERO         V2CAAS
115200941109     C                   Z-ADD     *ZERO         COMLNP
115300941109     C                   Z-ADD     *ZERO         V2CNRS
115400941109     C                   Z-ADD     *ZERO         V2CNSP
115500941109     C                   MOVEL     '1'           PA1F03
115600941109     C                   ENDIF
115700980609     C**
115800941212     C* CHIUDO I PGM
115900941212     C* SOLO PER PGM DI MANUTENZIONE
116000941212     C     *IN97         IFEQ      *ON
116100941212     C                   MOVEL     'C'           D66TLA
116200941212     C                   CALL      'FNLR66R'
116300991027     C                   PARM                    FNLR66DS
116400060224     c
116500060224     c                   clear                   fnlv55ds
116600060224     C                   MOVEL     'C'           D55TLA
116700060227     C                   CALL      'FNLV55R'
116800060224     C                   PARM                    FNLv55DS
116900941212     C*
117000941212     C                   MOVEL     'C'           D48TLA
117100991027     C                   MOVEL     FNLR48DS      KPJBU
117200941212     C                   CALL      'FNLR48R'
117300941212     C                   PARM                    KPJBA
117400941212     C*
117500141105     c                   eval      ilry09tla= 'C'
117600141105     c                   eval      kpjbu=fnlry09ds2
117700141105     c                   call      'FNLRY09R'
117800141105     c                   parm                    kpjba
117900941212     C                   ENDIF
118000980603     C**
118100980609     C                   MOVEL     PARAAZ        KPJBU
118200980624     C                   MOVEL     PARAM1        KPJBU
118300941212     C*
118400040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
118500040810     c                   clear                   fidn12ds
118600040810     c                   eval      i12tla = 'C'
118700040810     c                   call      'FIDN12R'
118800040810     c                   parm                    fidn12ds
118900051128     c                   clear                   fnlv80ds
119000051128     c                   eval      ilv80tla = 'C'
119100051128     c                   call      'FNLV80R'
119200051128     c                   parm                    kpjba
119300051128     c                   parm                    FNLV80DS
119400070427     c                   eval      id34tla = 'C'
119500070427     c                   call      'FIDG34R'
119600070427     c                   parm                    fidg34ds
119700131120     c                   eval      ilrg7tla = 'C'
119800131120     c                   call      'FNLRG7R'
119900131120     c                   parm                    fnlrg7ds
120000091216     c                   eval      iinttla = 'C'
120100091216     c                   call      'TRULINTR'
120200091216     c                   parm                    trulintds
120300141105
120400040810     c*
120500000000     C                   SETON                                        LR
120600941011     C   99
120700941011     CANN99              READ      FNBLP01L                               99
120800940802     C**************************************************************************
120900940802     C*    CARICO TABELLE
121000940802     C**************************************************************************
121100911024     C     CARTAB        BEGSR
121200941212     C***
121300941212     C* CARICO TABELLA FILIALI GESTITE £1
121400941212     C***
121500950122     C                   CLEAR                   L1
121600991027     C                   CLEAR                   TRUL06DS
121700941212     C                   MOVE      '£1'          D06COD
121800941212     C                   MOVEL     SIMFEL        D06KEY
121900941212     C                   MOVEL     'L'           D06TLA
122000991027     C                   MOVEL     TRUL06DS      KPJBU
122100941212     C*
122200941212     C                   CALL      'TRUL06R'
122300941212     C                   PARM                    KPJBA
122400991027     C                   MOVEL     KPJBU         TRUL06DS
122500941212     C                   MOVEA     LIN           L1
122600930705     C*
122700930705     C* CARICO TABELLA 7O
122800930705     C                   MOVEL     '7O'          COD
122900941212     C                   Z-ADD     1             C                 3 0
123000940713     C     KTAB2         SETLL     TABEL
123100940713     C     KTAB2         READE     TABEL                                  30
123200930705     C*
123300930705     C     *IN30         DOWEQ     *OFF
123400930705     C     TBLFLG        IFEQ      ' '
123500930705     C                   MOVEL     TBLKEY        KCA(C)
123600930705     C                   MOVEL     TBLUNI        DCA(C)
123700930705     C                   ADD       1             C
123800930705     C                   END
123900940713     C     KTAB2         READE     TABEL                                  30
124000930705     C                   END
124100941024     C*
124200941024     C* CARICO IN SCHIERA LA TABELLA CC
124300941024     C                   Z-ADD     *ZERO         WA
124400941024     C                   CLEAR                   CC1
124500941024     C                   CLEAR                   CC2
124600020329     C                   CLEAR                   CC3
124700941024     C                   MOVEL     'CC'          COD
124800941024     C     KTAB2         SETLL     TABEL00F
124900941024     C     KTAB2         READE     TABEL00F                               99
125000941024     C     *IN99         DOWEQ     *OFF
125100941024     C     TBLKEY        ANDNE     *BLANKS
125200941024     C     TBLKEY        ANDNE     *ZERO
125300941024     C                   ADD       1             WA
125400941024     C                   MOVE      TBLKEY        CC1(WA)
125500941024     C                   MOVEL     TBLUNI        CC2(WA)
125600020402     C                   MOVEL(P)  TBLUNI        DSCC
125700020402     C                   MOVE      §CCDFE        CC3(WA)
125800941024     C     KTAB2         READE     TABEL00F                               99
125900941024     C                   ENDDO
126000060130     C*
126100060130     C* CARICO IN SCHIERA LA TABELLA 3a per cod bolla doppi
126200060130     C                   Z-ADD     *ZERO         WA
126300060130     C                   CLEAR                   cbodop
126400060130     C                   MOVEL     '3A'          COD
126500060130     C     KTAB2         SETLL     TABEL00F
126600060130     C     KTAB2         READE     TABEL00F                               99
126700060130     C     *IN99         DOWEQ     *OFF
126800060130     c                   movel     tbluni        ds3a
126900060130     c                   if        §3atb2<>*blanks
127000060130     C                   ADD       1             WA
127100060130     C                   MOVEL     TBLKEY        cbodop(wa)
127200060130     c                   endif
127300060130     c
127400060130     C     KTAB2         READE     TABEL00F                               99
127500060130     C                   ENDDO
127600020329      *
127700020329      * CARICO IN SCHIERA LA TABELLA CB
127800020329     C                   Z-ADD     *ZERO         WA
127900020329     C                   CLEAR                   CB1
128000020329     C                   CLEAR                   CB2
128100020329     C                   MOVEL     'CB'          COD
128200020329     C     KTAB2         SETLL     TABEL00F
128300020329     C     KTAB2         READE     TABEL00F                               99
128400020329     C     *IN99         DOWEQ     *OFF
128500020329     C     TBLKEY        ANDNE     *BLANKS
128600020329     C     TBLKEY        ANDNE     *ZERO
128700020329     C                   ADD       1             WA
128800020329     C                   MOVE      TBLKEY        CB1(WA)
128900020329     C                   MOVEL     TBLUNI        CB2(WA)
129000020329     C     KTAB2         READE     TABEL00F                               99
129100020329     C                   ENDDO
129200991027     C*
129300991027     C* CARICO LA TABELLA $2
129400991027     C                   MOVEL     '$2'          COD
129500991027     C                   MOVEL(P)  1             KEY
129600991027     C     KTAB1         CHAIN     TABEL00F
129700991027     C                   IF        %FOUND
129800991027     C                   MOVEL     TBLUNI        DS$2
129900991027     C                   ELSE
130000991027     C                   CLEAR                   DS$2
130100991027     C                   ENDIF
130200940802     C*
130300911024     C                   ENDSR
130400940802     C**************************************************************************
130500940802     C*    PULIZIA CAMPI FORMATO 1
130600940802     C**************************************************************************
130700900514     C     PUL01         BEGSR
130800131114     c
130900131114     C* PULIZIA CAMPI CHE RIMANGONO INVARIATI IL SECONDO GIRO
131000131114     C                   MOVEL     ' '           V1CSSP
131100131127     c                   if        pa1flg='2' or pa1flg='4' or pa1flg='M'
131200131127     c                   else
131300131114     C                   MOVEL     *ZEROS        V1CLNA
131400131127     c                   endif
131500131115     c                   clear                   v1cdea
131600131115     c                   clear                   v1cdep
131700131114     C                   MOVEL     *ZEROS        V1CLP1
131800131114     C                   MOVEL     *ZEROS        V1CDTD
131900131114     C                   MOVEL     *ZEROS        V1CDTA
132000131114     c                   clear                   v1cnzd
132100131114     c                   clear                   v1cprd
132200131128     c                   movel     ' '           v1cxind
132300131114     c                   clear                   v1cind
132400131114     c                   clear                   v1clod
132500131128     c                   movel     ' '           v1cxlod
132600131114     C                   MOVE      *BLANKS       V1CRSD
132700131128     C                   movel     ' '           V1CXRSD
132800131114     C                   MOVE      *BLANKS       V1CRSM
132900131128     C                   movel     ' '           V1CXRSM
133000131114     C                   MOVEL     *ZEROS        NDC
133100131114     C                   MOVEL     *ZEROS        NSS
133200940802     C*
133300060613     C                   CLEAR                   wforz1
133400060613     C                   CLEAR                   V1CRPE
133500010509     C                   CLEAR                   V1Cpoe
133600010509     C                   CLEAR                   V1Cnsr
133700010509     C                   CLEAR                   V1Cnor
133800010509     C                   CLEAR                   V1Cnrv
133900110930     C                   CLEAR                   V1Crmn
134000110930     C                   CLEAR                   V1Crma
134100131115     C                   CLEAR                   V1Cdan
134200940713     C                   MOVEL     *ZEROS        V1CLP2
134300940713     C                   MOVEL     *ZEROS        V1CNRS
134400940713     C                   MOVEL     *ZEROS        V1CNSP
134500000103     C                   z-add     UYEAR         V1CAAS
134600000103     c     uyear         iflt      60
134700000103     c                   movel     20            v1caas
134800000103     c                   else
134900940713     C                   MOVEL     19            V1CAAS
135000000103     c                   endif
135100940713     C                   MOVEL     *BLANKS       V1CTSP
135200131114     C                   MOVEL     *BLANKS       V01DTSP
135300940713     C                   MOVEL     *BLANKS       V1CGIA
135400940713     C                   MOVEL     *ZEROS        V1CPSD
135500940713     C                   MOVEL     99            V1CPSA
135600940713     C                   MOVEL     *ZEROS        V1CSCA
135700940713     C                   MOVEL     *ZEROS        V1CSCD
135800940923     C                   MOVEL     *ZERO         V1CFLS
135900940713     C                   MOVE      'N'           V1CAFF
136000961112     C                   CLEAR                   V1CFBR
136100131111     C                   CLEAR                   V1Cbarcode
136200131111     C                   CLEAR                   V1CNOT
136300040216     c                   clear                   V1Ccbo
136400940713     C                   Z-ADD     0             NR2
136500070420     C****               MOVE      'N'           V1CSNP            1
136600070420     c                   clear                   v1csfd
136700941212     C                   SETOFF                                       09
136800131111     c
136900131111     c* se cursore aperto --> lo chiudo
137000131209     c***                if        Wend=*on  or *in14 or *in11
137100131209     c                   if        WopenC='1'
137200131111     c                   exsr      closecursor
137300131111     c                   endif
137400150223     c* se provengo da gestojne r.a. mi posiziono su brtcode
137500150223     c                   if            SDSprm >  1
137600150223     c                             and IA1ins = ' '
137700150223     c                   seton                                        60
137800150223     c                   endif
137900930705     C*
138000900514     C                   ENDSR
138100940802     C**************************************************************************
138200940802     C*    CONTROLLI FORMATO1
138300940802     C**************************************************************************
138400900509     C     CONTR         BEGSR
138500940317      *
138600940713     C                   MOVE      *BLANKS       V1CDEA
138700940713     C                   MOVE      *BLANKS       V1CDEP
138800110930     c                   movel     v1crma        comrma
138900160115     c                   clear                   wchainblp
139000020709
139100131121     c******             clear                   v2dlna
139200940317      *
139300900515     C                   SETOFF                                       900102
139400970925     C                   SETOFF                                       140784
139500980603     C                   SETOFF                                       03  11
139600040216     C                   SETOFF                                       154046
139700940715     C                   SETOFF                                       414243
139800940715     C                   SETOFF                                       444548
139900940715     C                   SETOFF                                       495152
140000131115     C                   SETOFF                                       552547
140100131111     C                   SETOFF                                       535460
140200131111     c                   setoff                                       575058
140300131127     c                   setoff                                       6862
140400110930     c                   clear                   h1riga
140500110930     c                   clear                   h1colo
140600950121     C*
140700950121     C* SE L'ANNO E' IMMESSO DI DUE CIFRE, METTO LE ALTRE DUE
140800950121     C     V1CAAS        IFLT      100
140900950121     C     V1CAAS        IFGT      60
141000950121     C                   ADD       1900          V1CAAS
141100950121     C                   ELSE
141200950121     C                   ADD       2000          V1CAAS
141300950121     C                   ENDIF
141400950121     C                   ENDIF
141500950121     C*
141600950121     C* SE VARIAZIONE CONTROLLO CAL CAUSALE
141700950121     C   97              DO
141800991027     C                   CLEAR                   FNLR66DS
141900950121     C     '?'           SCAN      V1CCVB                                 90
142000950121     C* RICERCA
142100950121    1C     *IN90         IFEQ      *ON
142200991027     C                   CLEAR                   FNLR66DS
142300950121     C                   MOVEL     'V'           D66GES
142400950121     C                   MOVEL     'A'           D66TBO
142500950121     C*
142600950121     C* SE IMMESSO NUMERO SPEDIZIONE LO VISUALIZZO
142700950121    2C     V1CNSP        IFGT      0
142800950121     C                   MOVEL     V1CNSP        D66NSP
142900950121     C                   MOVEL     V1CAAS        D66AAS
143000950121     C                   MOVEL     V1CLP2        D66LNP
143100950121     C                   MOVEL     V1CNRS        D66NRS
143200950121    2C                   ENDIF
143300950121     C*
143400160210     c                   if        %subst(knmus:1:3)='EDP'
143500160121     c                   eval      d66bnp='S'
143600160210     c                   endif
143700991027     C                   MOVEL     FNLR66DS      KPJBU
143800950121     C                   CALL      'FNLR66R'
143900950121     C                   PARM                    KPJBA
144000991027     C                   MOVEL     KPJBU         FNLR66DS
144100950121     C                   MOVEL     D66CVB        V1CCVB
144200950121     C                   MOVEL     D66DES        V1DCVB
144300950121     C                   MOVEL     'S'           WRIEM
144400950203     C                   SETON                                        4790
144500950121     C*
144600950121     C                   GOTO      ENDCTR
144700950121   X1C                   ELSE
144800950121     C* CONTROLLO
144900991027     C                   CLEAR                   FNLR66DS
145000950121     C                   MOVEL     'C'           D66GES
145100950121     C                   MOVEL     'A'           D66TBO
145200950121     C                   MOVEL     V1CCVB        D66CVB
145300950121    2C     V1CNSP        IFGT      0
145400950121     C                   MOVEL     V1CNSP        D66NSP
145500950121     C                   MOVEL     V1CAAS        D66AAS
145600950121     C                   MOVEL     V1CLP2        D66LNP
145700950121     C                   MOVEL     V1CNRS        D66NRS
145800950121    2C                   ENDIF
145900160210     c                   if        %subst(knmus:1:3)='EDP'
146000160121     c                   eval      d66bnp='S'
146100160210    2C                   ENDIF
146200950121     C*
146300991027     C                   MOVEL     FNLR66DS      KPJBU
146400950121     C                   CALL      'FNLR66R'
146500950121     C                   PARM                    KPJBA
146600991027     C                   MOVEL     KPJBU         FNLR66DS
146700950121     C                   MOVEL     D66CVB        V1CCVB
146800950121     C                   MOVEL     D66DES        V1DCVB
146900950121     C*
147000950121     C* ERRORE
147100950121    2C     D66MSG        IFNE      *BLANKS
147200950121     C                   MOVEL     D66MSG        V1CMSG
147300950121     C                   SETON                                        284790
147400950121     C                   GOTO      ENDCTR
147500950121     C*
147600950121   X2C                   ELSE
147700950121     C* DECODIFICA
147800950121     C                   MOVEL     D66DES        V1DCVB
147900950121    2C                   ENDIF
148000950121    1C                   ENDIF
148100950121     C                   ENDDO
148200950102     C*
148300131113     C* immettere almeno una selezione
148400901115     C     SEL1          IFEQ      0
148500920911     C     SEL2          ANDEQ     0
148600940317     C     SEL3          ANDEQ     *BLANKS
148700010509     C     SEL4          ANDEQ     *zeros
148800131113     c
148900971223     C     V1CRPE        ANDEQ     *BLANKS
149000110930     c     v1crmn        andeq     *zeros
149100110930     c     v1crma        andeq     *blanks
149200131111     c     v1cnot        andeq     *blanks
149300131111     c     v1cbarcode    andeq     *zeros
149400940713     C                   MOVEL     MSG(3)        V1CMSG
149500940713     C                   SETON                                        412890
149600901115     C                   GOTO      ENDCTR
149700901115     C                   END
149800940318      *
149900901115     C* DATA SPEDIZIONE
150000900514     C* LINEA ARRIVO
150100131113      * per destinatario o mittente sono obbligatori
150200020627      * la nazione e la provincia del destinatario
150300131113    1C     SEL2          IFEQ      0
150400131113    2c     sel4          ifeq      0
150500020225     c                   clear                   lnantw
150600131113    3C     V1CLNA        IFNE      0
150700940713     C                   MOVEL     V1CLNA        NUM3
150800940713     C     NUM3          CHAIN     AZORG01L                           41
150900131113    4C   41              DO
151000940713     C                   MOVEL     MSG(2)        V1CMSG
151100940713     C                   SETON                                        28  90
151200940713     C                   GOTO      ENDCTR
151300131113    4C                   ENDDO
151400940713     C                   MOVEL     ORGDES        V1CDEA
151500131121     C*******            MOVEL     ORGDES        V2DLNA
151600991222     C                   MOVEL     ORGDE3        og143
151700020225     c                   eval      lnantw = §ogntw
151800900514     C                   SETON                                        01
151900131113    3C                   END
152000131113    2C                   END
152100131113    c
152200020626      * selezione per destinatario o mittente
152300020626     c                   if        sel3 <> *blanks
152400020628     c                   exsr      ContrDatiDes
152500020628     c                   if        *in90 = *on                                  ricerca
152600020628     c                   goto      endctr
152700020628     c                   endif
152800020628     c                   if        *in28 = *on                                  errore
152900020628     c                   eval      *in90 = *on
153000020628     c                   goto      endctr
153100020628     c                   endif
153200020626     c                   endif
153300901115     C* LINEA PARTENZA
153400131111     c                   clear                   lnpntw
153500940713     C     V1CLP1        IFNE      0
153600940713     C                   MOVEL     V1CLP1        NUM3
153700940713     C     NUM3          CHAIN     AZORG01L                           42
153800940713     C   42              DO
153900940713     C                   MOVEL     MSG(5)        V1CMSG
154000940713     C                   SETON                                        28  90
154100940713     C                   GOTO      ENDCTR
154200940713     C                   ENDDO
154300940713     C                   MOVEL     ORGDES        V1CDEP
154400131121     C****               MOVEL     ORGDES        V2DLNP
154500000703     C                   movel     orgde3        og143
154600050304     c                   eval      lnpntw = §ogntw
154700901115     C                   SETON                                        02
154800901115     C                   END
154900940413      *
155000971223      * E' OBBLIGATORIA LNA O LNP
155100971223     C     V1CLNA        IFEQ      *ZERO
155200940713     C     V1CLP1        ANDEQ     *ZERO
155300971223     C     V1CRPE        ANDEQ     *BLANKS
155400010509     c     sel4          andeq     0
155500020628     c     sel3          andeq     *blanks
155600110930     c     v1crmn        andeq     *zeros
155700110930     c     v1crma        andeq     *blanks
155800131113     c     v1cnot        andeq     *blanks
155900131114     c     v1cbarcode    andeq     *zeros
156000940713     C                   MOVEL     MSG(4)        V1CMSG
156100940713     C                   SETON                                        412890
156200940712     C                   GOTO      ENDCTR
156300940712     C                   ENDIF
156400940413     C*
156500911023     C* DATA DAL OBBLIGATORIA
156600940713     C     V1CDTD        IFEQ      0
156700971223     C     V1CRPE        ANDEQ     *BLANKS
156800010509     C     sel4          ANDEQ     0
156900110930     C     v1crmn        ANDEQ     0
157000110930     C     v1crma        ANDEQ     *blanks
157100131111     C     v1cnot        ANDEQ     *blanks
157200131114     c     v1cbarcode    andeq     *zeros
157300131113     c
157400131113     c*********          if        v1cssp <> *blanks or
157500131113     c
157600131113     c                   if        (v1crsd = *blanks and v1crsm = *blanks and
157700131111     c                              v1cind = *blanks and v1clod = *blanks and
157800131111     c                             sel3 <> *blanks and *in01) or
157900131111     c                               sel3 = *blanks
158000940713     C                   MOVEL     MSG(8)        V1CMSG
158100940713     C                   SETON                                        285190
158200911023     C                   GOTO      ENDCTR
158300940712     C                   ENDIF
158400911023     C                   END
158500911023     C*
158600940712     C                   Z-ADD     *ZERO         COMDTD
158700940712     C                   Z-ADD     *ZERO         COMAAD
158800940712     C                   Z-ADD     *ZERO         COMMGD
158900940713     C     V1CDTD        IFNE      *ZERO
159000940713     C                   MOVE      V1CDTD        G02DAT
159100930705     C                   MOVEL     *BLANK        G02ERR
159200940713     C                   CALL      'XSRDA8'
159300930705     C                   PARM                    WLBDAT
159400930705     C     G02ERR        IFEQ      '1'
159500940713     C                   MOVEL     MSG(8)        V1CMSG
159600940713     C                   SETON                                        285190
159700930705     C                   GOTO      ENDCTR
159800930705     C                   ELSE
159900940713     C                   Z-ADD     G02DAT        V1CDTD
160000940713     C                   MOVE      G02INV        COMDTD            8 0
160100940713     C                   MOVEL     G02INV        COMAAD            4 0
160200930705     C                   MOVE      G02INV        COMMGD            4 0
160300930705     C                   END
160400940712     C                   ENDIF
160500900515     C* DATA SPEDIZIONE AL
160600940712     C                   Z-ADD     *ZERO         COMDTA
160700940713     C     V1CDTA        IFNE      0
160800940713     C                   MOVE      V1CDTA        G02DAT
160900930705     C                   MOVEL     *BLANK        G02ERR
161000940713     C                   CALL      'XSRDA8'
161100930705     C                   PARM                    WLBDAT
161200930705     C     G02ERR        IFEQ      '1'
161300941215     C                   MOVEL     MSG(8)        V1CMSG
161400940713     C                   SETON                                        285290
161500930705     C                   GOTO      ENDCTR
161600930705     C                   ELSE
161700940713     C                   Z-ADD     G02DAT        V1CDTA
161800940713     C                   MOVE      G02INV        COMDTA            8 0
161900900515     C                   END
162000930705     C*
162100900515     C* DATA SPEDIZIONE AL MAGGIORE DATA SPEDIZIONE AL
162200911023     C     COMDTA        IFLT      COMDTD
162300940713     C                   MOVEL     MSG(10)       V1CMSG
162400940713     C                   SETON                                        285152
162500940713     C                   SETON                                        90
162600900515     C                   GOTO      ENDCTR
162700900515     C                   END
162800940318      *
162900940712     C                   ELSE
163000940712      *
163100940712      * SE NON IMMESSA LA DATA FINALE, LA PONGO UGUALE A QUELLA INIZIALE
163200940712     C                   Z-ADD     COMDTD        COMDTA
163300940318      *
163400900515     C                   END
163500940318      *
163600940712      * VERIFICO SE LE DUE DATE SONO COINCIDENTI
163700131115     c                   if        comdtd>0
163800940712     C     COMDTD        COMP      COMDTA                                 15
163900131115     c                   endif
164000901115     C***
164100911024     C* GIACENTI
164200911024     C* SCELTE INCONGRUENTI
164300940713     C     V1CGIA        IFEQ      'G'
164400940713     C     V1CSSP        IFEQ      '2'
164500940713     C     V1CSSP        OREQ      '3'
164600940713     C                   MOVEL     MSG(14)       V1CMSG
164700940713     C                   SETON                                        285690
164800911024     C                   GOTO      ENDCTR
164900911024     C                   END
165000911024     C                   END
165100901115     C* NUMERO SERIE AL DEVE ESSERE > NUMERO SERIE DAL
165200901115     C*  SE IMMESSO
165300940713     C     V1CPSD        IFGT      V1CPSA
165400940713     C                   MOVEL     MSG(11)       V1CMSG
165500940713     C                   SETON                                        284890
165600901115     C                   GOTO      ENDCTR
165700901115     C                   END
165800901115     C* NUMERO SEGNACOLLO AL DEVE ESSERE > NUMERO SEGNACOLLO DAL
165900940713     C     V1CSCA        IFGT      0
166000940713     C     V1CSCD        ANDGT     V1CSCA
166100940713     C                   MOVEL     MSG(12)       V1CMSG
166200940713     C                   SETON                                        284990
166300901115     C                   GOTO      ENDCTR
166400901115     C                   END
166500901115     C* SE IMMESSO AL IMMETTERE ANCHE DAL
166600940713     C     V1CSCA        IFGT      0
166700940713     C     V1CSCD        ANDEQ     0
166800940713     C                   MOVEL     MSG(13)       V1CMSG
166900940713     C                   SETON                                        284990
167000901115     C                   GOTO      ENDCTR
167100901115     C                   END
167200940713     C     V1CSCD        COMP      0                                  07
167300970925     C     V1CSCA        COMP      0                                  84
167400940923     C*
167500940923     C* LA FILIALE SEGNACOLLO DEVE ESSERE VALIDA, SE IMMESSSA
167600940923     C     V1CFLS        IFNE      *ZERO
167700940923     C     V1CFLS        CHAIN     AZORG01L                           46
167800941014     C     *IN46         IFEQ      *ON
167900941014     C     *IN46         OREQ      *OFF
168000941014     C     ORGFVA        ANDNE     *BLANKS
168100941014     C                   SETON                                        284690
168200941014     C                   MOVEL     MSG(17)       V1CMSG
168300941014     C                   GOTO      ENDCTR
168400940923     C                   ENDIF
168500941014     C                   ENDIF
168600940923     C*
168700901115     C* SE IMMESSA RAGIONE SOCIALE CERCO PRIMO BLANKS
168800940713     C     V1CRSM        IFNE      *BLANKS
168900901115     C                   SETON                                        11
169000940713     C                   MOVEA     V1CRSM        C1
169100131111     C                   Z-ADD     30            XX                2 0
169200930702     C     C1(XX)        DOWEQ     *BLANKS
169300930702     C                   SUB       1             XX
169400901116     C                   END
169500901116     C                   END
169600901115     C* IMMESSO DESTINATARIO: CERCO PRIMO BLANKS
169700940713     C     V1CRSD        IFNE      *BLANKS
169800901115     C                   SETON                                        14
169900940713     C                   MOVEA     V1CRSD        C1
170000131111     C                   Z-ADD     30            ZZ                2 0
170100930702     C     C1(ZZ)        DOWEQ     *BLANKS
170200930702     C                   SUB       1             ZZ
170300901116     C                   END
170400901115     C                   END
170500131113     C* località DESTINATARIO: CERCO PRIMO BLANKS
170600131113     C     V1Clod        IFNE      *BLANKS
170700131113     C                   SETON                                        14
170800131113     C                   MOVEA     V1Clod        C1
170900131113     C                   Z-ADD     30            LL                2 0
171000131113     C     C1(LL)        DOWEQ     *BLANKS
171100131113     C                   SUB       1             LL
171200131113     C                   END
171300131113     C                   END
171400131113     C* Indirizzo DESTINATARIO: CERCO PRIMO BLANKS
171500131113     C     V1Cind        IFNE      *BLANKS
171600131113     C                   SETON                                        14
171700131113     C                   MOVEA     V1Cind        C1
171800131113     C                   Z-ADD     30            DD                2 0
171900131113     C     C1(DD)        DOWEQ     *BLANKS
172000131113     C                   SUB       1             DD
172100131113     C                   END
172200131113     C                   END
172300131113     c
172400131113     c* 14 on anche per indirizzo o località  DESTINAT
172500131113     c                   if        not *in11 and
172600131113     c                              v1clod<>*blanks or v1cind<>*blanks
172700131113     c                   seton                                        14
172800131113     c                   endif
172900131113     c
173000131113      * Nazione provincia destinatario senza ragioni sociali
173100060613      * imposto per destinatario se non è stata impostata la lna  e la linea
173200060613      * partenza
173300060613     c                   if        v1cprd <> *blanks and
173400020628     c                             not *in11 and not *in14 and not *in01
173500060613     c                   eval      *in14 = *on
173600020628     c                   endif
173700060613     c                   if         v1cnzd <> *blanks and v1cprd = *blanks and
173800060613     c                             not *in11 and not *in14 and not *in01
173900060613     c                             and not *in02
174000060613     c                   eval      *in14 = *on
174100060613     c                   endif
174200901115     C                   END
174300901115     C*
174400901115     C** NUMERO SPEDIZIONE
174500901115     C** LINEA PARTENZA
174600140117     C***  SEL2          IFNE      0
174700140117     c                   if        sel2<> 0 and v1cbarcode<=0
174800940713     C                   Z-ADD     V1CLP2        NUM3
174900940713     C     NUM3          CHAIN     AZORG01L                           45
175000940713     C   45              DO
175100940713     C                   MOVEL     MSG(5)        V1CMSG
175200940713     C                   SETON                                        28  90
175300940713     C                   GOTO      ENDCTR
175400940713     C                   ENDDO
175500911023     C* ANNO SPEDIZIONE
175600940713     C     V1CAAS        IFEQ      0
175700940713     C                   MOVEL     MSG(7)        V1CMSG
175800940713     C                   SETON                                        285590
175900911023     C                   GOTO      ENDCTR
176000911023     C                   END
176100160115     c                   if        not *in97 or (d66utp=*blanks or d66utp='T')
176200940713     C     KLNPX         CHAIN     FNARB01L                           44
176300160115     c                   endif
176400160121     c                   if        *in97 and (
176500160121     c                             (d66utp='T' and not%found(fnarb01l))
176600160121     c                              or d66utp='P')
176700160115     C     KLNPX         CHAIN     FNBLP01L                           44
176800160115     c                   eval      wchainblp='S'
176900160115     c                   endif
177000030527     C*
177100940713     C   44              DO
177200940713     C                   MOVEL     MSG(6)        V1CMSG
177300940713     C                   SETON                                        28  90
177400940713     C                   GOTO      ENDCTR
177500940713     C                   ENDDO
177600160121     c
177700901115     C*
177800901115     C                   END
177900131111     c***
178000131113     C* B A R C O D E   L D V
178100131111     c***
178200131111     c                   if        v1cbarcode>0
178300131111     c                   z-add     b_aas         arbaas
178400131111     C     b_aas         IFLT      100
178500131111     C     b_aas         IFGT      60
178600131111     C                   ADD       1900          arbAAS
178700131111     C                   ELSE
178800131111     C                   ADD       2000          arbAAS
178900131111     C                   ENDIF
179000131111     C                   ENDIF
179100131111     c                   z-add     b_lnp         arblnp
179200131111     c                   z-add     b_nrs         arbnrs
179300131111     c                   z-add     b_nsp         arbnsp
179400160121     c                   if        not *in97 or (d66utp=*blanks or d66utp='T')
179500160121     C     Karb2         CHAIN     FNARB01L                           44
179600160121     c                   endif
179700160121     c                   if        *in97 and (
179800160121     c                             (d66utp='T' and not%found(fnarb01l))
179900160121     c                              or d66utp='P')
180000160121     C     karb2         CHAIN     FNBLP01L                           44
180100160121     c                   eval      wchainblp='S'
180200160121     c                   endif
180300131111     C*
180400160121     C****               if        not%found(fnarb01l)
180500160121     c                   if        *in44
180600131111     C                   MOVEL     MSG(32)       V1CMSG
180700131111     C                   SETON                                        286090
180800131111     C                   GOTO      ENDCTR
180900131111     C                   endif
181000131111     C                   endif
181100010508      ***
181200010508      * N U M E R O   O R M
181300010508      ***
181400010508     C     sel4          ifne      0
181500010508     C     sel2          andeq     0
181600010508      * P.O. EMISSIONE
181700010508     C                   z-add     v1cpoe        num3
181800010508     C     num3          chain     azorg01l                           53
181900010508     C     *in53         ifeq      *on
182000010508     C                   movel     msg(21)       v1cmsg
182100010508     C                   seton                                        28  90
182200010508     C                   goto      endctr
182300010508     C                   endif
182400010508      * N.ORM
182500010510     C                   MOVEL     'M'           WTRC
182600010510     C                   MOVEL(P)  SEL4          WN14             14
182700060216     C     kar44         chain     fiar404l                           54
182800010508     C     *in54         ifeq      *on
182900010508     C                   movel     msg(22)       v1cmsg
183000010508     C                   seton                                        28  90
183100010508     C                   goto      endctr
183200010508     C                   endif
183300010508      *
183400010508     C                   seton                                        25
183500010508      *
183600010508     C                   endif
183700131111     c
183800901115     C*
183900911023     C* TIPO SPEDIZIONE ESPRESSO
184000131114     C                   MOVEL     *BLANKS       V01DTSP
184100940713     C     V1CTSP        IFNE      ' '
184200940715     C     V1CTSP        IFEQ      '?'
184300940715     C                   CALL      'X§TABER'
184400040217     C                   PARM      CodUt         §KUT              1 0
184500040217     C                   PARM      '5E'          COD
184600040217     C                   PARM      *blanks       KEY
184700040217     C                   PARM      *blanks       §DES             30
184800940715     C                   MOVEL     KEY           V1CTSP
184900940715     C                   SETON                                        90
185000940715     C                   MOVEL     'S'           WRIEM
185100940715     C                   ENDIF
185200940715     C*
185300090604     c                   clear                   ds5e
185400940715     C                   MOVEL     '5E'          COD
185500940715     C                   MOVEL     *BLANKS       KEY
185600940715     C                   MOVEL     V1CTSP        KEY
185700940715     C     KTAB1         CHAIN     TABEL00F                           43
185800941014     C     *IN43         IFEQ      *ON
185900941014     C     *IN43         OREQ      *OFF
186000950221     C     TBLFLG        ANDNE     ' '
186100940715     C                   MOVEL     MSG(16)       V1CMSG
186200941014     C                   SETON                                        284390
186300940715     C                   GOTO      ENDCTR
186400941014     C                   ELSE
186500090604     C                   MOVEL     TBLUNI        ds5e
186600131114     C                   MOVEL     §5ed08        V01DTSP
186700991222     C                   ENDIF
186800940715     C                   ENDIF
186900040216      *
187000991222     c*
187100991222     c* Se richiesta linea di arrivo dpd il riferimemnto immesso deve
187200060613     c*  essere composto soltanto da numeri se <=11
187300060613     C*  alfanumerico       se=14
187400050121     c                   clear                   wnodpd            1
187500060613    1c                   if        lnantw = 'DPD' or lnpntw = 'DPD' or
187600050304     c                             lnpntw = *blanks or lnantw = *blanks
187700060613    2c                   if        v1crpe<>*blanks
187800060613    3c                   select
187900060613     c* ok se lungo 14
188000060613     c                   when      %subst(v1crpe:15:1)=' '  and
188100060613     c                             %subst(v1crpe:14:1)<>' '
188200060613     c* ok se lungo 11 ma tutto numerico
188300060613     c                   when      %subst(v1crpe:12:4)=*blanks and
188400060613     c                             %subst(v1crpe:11:1)<>' '
188500060613     C                   MOVEA     V1CRPe        K11
188600060613     C                   Z-ADD     1             YY                2 0
188700060613     c
188800060613    4C                   DO        11            YY
188900060613     C     K11(YY)       LOOKUP    CNM                                    30
189000060613    5C     *IN30         IFEQ      *OFF
189100060613     c* Se non è tutto numerico non è DPD
189200060613     c                   eval      wnodpd = '1'
189300060613     c                   leave
189400060613    5c                   endif
189500060613    4c                   enddo
189600060613   x3c                   other
189700060613     c
189800060613     c* se non è lungo 11 o 14 non è DPD
189900060613     c                   eval      wnodpd = '1'
190000060613    3c                   endsl
190100060613     c
190200060613    3c                   if        wnodpd='1'
190300060613    4c     lnantw        ifeq      'DPD'
190400060613     c     lnpntw        oreq      'DPD'
190500060613     C                   MOVEL     MSG(18)       V1CMSG
190600060613     C                   SETON                                        285890
190700060613     C                   GOTO      ENDCTR
190800060613    4C                   ENDIF
190900060613    3C                   ENDIF
191000060613    2c                   endif
191100061117   x1c                   else
191200061117     c                   eval      wnodpd = '1'
191300060613    1c                   endif
191400941014     C*
191500911023     C** FILE STORICO
191600940713     C     V1CSSP        COMP      '1'                                    18
191700940713     C     V1CSSP        COMP      '2'                                    19
191800940713     C     V1CSSP        COMP      '3'                                    20
191900940713     C     V1CSSP        COMP      ' '                                    21
192000920911     C*
192100940712      * DEVO CONSIDERARE DELLE PRIORITA' PER LE SELEZIONI IMMESSE DALL'UTENTE:
192200010508      *    1°    NUMERO O.R.M.           ( 25 ON)
192300010508      *    2°    MITTENTE                ( 11 ON )
192400010508      *    3°    DESTINATARIO            ( 14 ON )
192500010508      *    4°    LINEA DI PARTENZA       ( 02 ON )
192600010508      *    5°    LINEA DI ARRIVO         ( 01 ON )
192700940712      *
192800010508     C   25              setoff                                       111402
192900010508     C   25              setoff                                       01
193000131113     C   14              SETOFF                                       020111
193100131113     C   11              SETOFF                                       0201
193200940712     C   02              SETOFF                                       01
193300991222     c*se lna dpd e immesso riferimento partner estero, se meno di 12
193400060518     c*  caratteri, calcolo il 12 --> ora non più: il check digit per la
193500060518     c*  ricerca non serve più
193600991222     C                   CLEAR                   krpe
193700050121    1c                   if        v1crpe <> *blanks and (lnantw='DPD'
193800050304     c                             or lnpntw = 'DPD' or wnodpd=' ')
193900060613     C                   movel     V1CRpe        krpe             14
194000991222     C                   ENDIF
194100911023     C*
194200900514     C     ENDCTR        ENDSR
194300020628      *------------------------------------------------------------------------*
194400020628      *    Controlla i dati nazione e provincia destinatario
194500020628      *------------------------------------------------------------------------*
194600020628     c     ContrDatiDes  begsr
194700020628     c                   if        v1cnzd = *blanks and v1cprd = *blanks
194800020628     c                   eval      v1cmsg = msg(23)
194900020628     c                   eval      *in57 = *on
195000020628     c                   eval      *in28 = *on
195100020628     c                   goto      EndContr
195200020628     c                   endif
195300020628      * ricerca tabella nazione
195400020628     c     '?'           scan      v1cnzd                                 90
195500020628     c                   movel     '15'          cod
195600020628     c                   if        *in90
195700020628     c                   call      'TRUL19R'
195800020628     c                   parm                    cod
195900020628     c                   parm                    ordina
196000020628     c                   parm                    key
196100020628     c                   parm                    comando
196200020628     c                   movel     key           v1cnzd
196300020628     c                   eval      *in57 = *on
196400020628     c                   movel     'S'           wriem
196500020628     c                   goto      EndContr
196600020628     c                   endif
196700020628      * controllo nazione destinatario
196800020628     c                   clear                   ds15
196900020628     c                   movel     '15'          cod
197000020628     c                   clear                   key
197100020628     c                   movel     v1cnzd        key
197200020628     c     ktab1         chain     tabel00f
197300020628     c                   if        not %found(tabel00f) or tblflg <> *blanks
197400020628     c                   eval      v1cmsg = msg(24)
197500020628     c                   eval      *in57 = *on
197600020628     c                   eval      *in28 = *on
197700020628     c                   goto      EndContr
197800020628     c                   endif
197900020628     c                   movel     tbluni        ds15
198000060613     c* Se cambiata nazione, clearo forzatura msg se nazine estera
198100060613     c                   if        v1cnzd<>savnzd
198200060613     c                   clear                   wforz1            1
198300060613     c                   eval      savnzd=v1cnzd
198400060613     c                   endif
198500020628      * per nazione italia la provincia è obbligatoria
198600020628     c                   if        §15ita = 'I' and v1cprd = *blanks
198700020628     c                   eval      v1cmsg = msg(25)
198800020628     c                   eval      *in50 = *on
198900020628     c                   eval      *in28 = *on
199000020628     c                   goto      EndContr
199100020628     c                   endif
199200020628      * ricerca tabella provincia
199300020628     c     '?'           scan      v1cprd                                 90
199400020628     c                   movel     'PR'          cod
199500020628     c                   if        *in90
199600020628     c                   call      'TRUL19R'
199700020628     c                   parm                    cod
199800020628     c                   parm                    ordina
199900020628     c                   parm                    key
200000020628     c                   parm                    comando
200100020628     c                   movel     key           v1cprd
200200020628     c                   eval      *in50 = *on
200300020628     c                   movel     'S'           wriem
200400020628     c                   goto      EndContr
200500020628     c                   endif
200600020628      * controllo provincia destinatario
200700020628     c                   if        v1cprd <> *blanks and §15ita = 'I'
200800020628     c                   clear                   dspr
200900020628     c                   movel     'PR'          cod
201000020628     c                   clear                   key
201100020628     c                   movel     v1cprd        key
201200020628     c     ktab1         chain     tabel00f
201300020628     c                   if        not %found(tabel00f) or tblflg <> *blanks
201400020628     c                   eval      v1cmsg = msg(25)
201500020628     c                   eval      *in50 = *on
201600020628     c                   eval      *in28 = *on
201700020628     c                   goto      EndContr
201800020628     c                   endif
201900020628     c                   movel     tbluni        dspr
202000020628      * se nazione italia la provincia deve essere italia
202100020628      * nella tabella ci sono anche le provincie estere e per sapere se ok
202200020628      * faccio il controllo con il codice tassazione per estero
202300020628      * se vuoto vuol dire che è provincia estera
202400020628     c                   if        §15ita = 'I' and §prcte = *blanks
202500020628     c                   eval      v1cmsg = msg(25)
202600020628     c                   eval      *in50 = *on
202700020628     c                   eval      *in28 = *on
202800020628     c                   goto      EndContr
202900020628     c                   endif
203000020628     c                   endif
203100020628
203200020628     c     EndContr      endsr
203300940318      **************************************************************************
203400940318      *    ELABORA BOLLE DA VISUALIZZARE
203500940318      **************************************************************************
203600900515     C     ELAB          BEGSR
203700940318      *
203800131114     C                   SETON                                        35
203900131114     C                   WRITE     LRI6C02
204000131114     C                   SETOFF                                       35
204100131114     C*
204200940713     C                   Z-ADD     1             REC2
204300911024     C                   Z-ADD     1             C
204400131111     c                   eval      wEnd = *off
204500131115     c                   clear                   readFile          3
204600131113     c
204700131113     C                   Z-ADD     COMAAD        $AAS
204800131113     C                   Z-ADD     COMMGD        $MGS
204900131111     c
205000131111    1c                   select
205100010508     c* O.R.M.
205200131111    1c     *in25         wheneq    *on
205300010508     C                   MOVEL     'M'           WTRC
205400010508     C                   MOVEL(P)  SEL4          WN14             14
205500060216     C     Kar44         SETLL     FiAR404L
205600131111     c
205700131111     c***
205800131111     C*  RIFERIMENTO P A R T N E R   E S T E R O
205900131111     c***
206000131111    1C     V1CRPE        whenne    *BLANKS
206100050121     c                   clear                   wgiro             1
206200050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
206300971223     C                   MOVEL(P)  V1CRPE        WN30             30
206400991222     C                   MOVEL     'E'           WTRC
206500991222     c                   else
206600991222     c* D P D
206700991222     C                   MOVEL(P)  krpe          WN30             30
206800060518     C***                MOVEL     'F'           WTRC
206900060518     C                   MOVEL     'I'           WTRC
207000991222     c                   endif
207100991222     c**
207200060216     C     KAR4          SETLL     FiAR403L
207300991222     c**
207400131111     c* C H I   S O N O
207500131111     c**
207600131111    1C     V1CNOT        whenne    *BLANKS
207700131111     C     KarS          SETLL     FIarS02L
207800131111     c* R N N
207900131111    1c     v1crmn        whengt    *zeros
208000110930     c     v1crmn        setll     fnarb33l
208100131115     c                   eval      Readfile='DSP'
208200131111     c* R M A
208300131111    1c     comrma        whenne    *blanks
208400111005     c     comrma        setll     fnarb32l
208500131115     c                   eval      Readfile='DSP'
208600131111     c
208700131113      * RSD/RSM/IND/LOD o solo NAZ/PRV       -----------------------------------
208800110930     c
208900131113     c                   when      *in14  OR *IN11
209000131111     c                   exsr      Presql
209100131111     c                   exsr      Opencursor
209200131111   x1c                   other
209300131111     c
209400971223      *
209500940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1)  -----------------------------------
209600971223    2C   18              DO
209700940713     C     V1CLNA        IFNE      *ZERO
209800940713     C                   Z-ADD     V1CLNA        COMLNA
209900940712     C                   ELSE
210000940712     C                   Z-ADD     L1(1)         COMLNA
210100940712     C                   ENDIF
210200020910     C     K1ARB         SETLL     FNARB78L
210300131115     c                   eval      Readfile='DSP'
210400971223    2C                   ENDDO
210500940318      *
210600940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
210700971223    2C   19              DO
210800940713     C     V1CLNA        IFNE      *ZERO
210900120330     C     V1CLNA        SETLL     FNARB05L
211000940712     C                   ELSE
211100120330     C     *LOVAL        SETLL     FNARB05L
211200940712     C                   ENDIF
211300971223    2C                   ENDDO
211400940318      *
211500940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
211600971223    2C   20              DO
211700940713     C     V1CLNA        IFNE      *ZERO
211800940713     C                   Z-ADD     V1CLNA        COMLNA
211900940712     C                   ELSE
212000940712     C                   Z-ADD     L1(1)         COMLNA
212100940712     C                   ENDIF
212200940713     C     K4ARB         SETLL     FNARB54L
212300131115     c                   eval      Readfile='DCM'
212400971223    2C                   END
212500940318      *
212600940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
212700971223    2C   21              DO
212800131113     c
212900131113     c* per LINEA DI PARTENZA
213000131113    3C     *IN02         IFEQ      *ON
213100950406     C                   SELECT
213200950406     C* Serie = 0 e segnacollo dal diverso da 0 => i segnacolli sono
213300950406     C* sequenziali
213400950406     C     V1CPSD        WHENEQ    *ZEROS
213500950406     C     V1CSCD        ANDNE     *ZEROS
213600950406     C                   Z-ADD     V1CSCD        COMSCD
213700950406     C     K6ARB         SETLL     FNARB27L                               31
213800131115     c                   eval      Readfile='DSP'
213900950406     C     *IN31         IFEQ      *OFF
214000950406     C* Non trovata bolla col numero segnacollo specificato: il
214100950406     C* segnacollo non è quello iniziale della bolla e quindi è sicura-
214200950406     C* mente incluso nella spedizione precedente.
214300950406     C     K6ARB1        READPE    FNARB27L                               31
214400131209     C  n31              Z-ADD     ARBNCD        COMSCD
214500950406     C     K6ARB         SETLL     FNARB27L
214600950406     C                   END
214700131113     c
214800950406     C* Non immesso segnacollo dal oppure serie diversa da zero (segna-
214900950406     C* colli non sequenziali) mi posiziono con segnacollo = 0
215000950406     C                   OTHER
215100950406     C                   MOVE      *ZEROS        COMSCD
215200950405     C     K6ARB         SETLL     FNARB27L
215300131115     c                   eval      Readfile='DSP'
215400950406     C                   ENDSL
215500131113    3C                   END
215600131113     c*
215700131113     c* Per LINEA DI ARRIVO
215800940712     C   01K8ARB         SETLL     FNARB26L
215900131115     c                   eval      Readfile='DSP'
216000131111     c
216100971223    2C                   ENDDO
216200131111    1C                   ENDSL
216300940318      *
216400911023     C                   Z-ADD     0             COMOD
216500911023     C                   EXSR      CARSF
216600940318      *
216700901116     C                   ENDSR
216800940318      **************************************************************************
216900940318      *    CARICO SUBFILE IN AVANTI
217000940318      **************************************************************************
217100900515     C     CARSF         BEGSR
217200940318      *
217300900518     C                   Z-ADD     1             B                 2 0
217400940713     C                   Z-ADD     COMOD         NR2
217500000228     C                   clear                   War4AAS
217600000228     C                   clear                   War4LNP
217700000228     C                   clear                   War4NRS
217800000228     C                   clear                   War4NSP
217900000228      *
218000131113    1C     B             DOWLE     18
218100060216     c                   setoff                                       32
218200131111
218300131113    2c                   select
218400131111     c***
218500131111     c* O R M
218600131111     c***
218700131111     c     *in25         wheneq    *on
218800060216     c     kar44         reade     fiar404l                               30
218900010509     C  N30KARB4         CHAIN     FNARB01L                           32
219000131111     C***
219100131111      * R I F .   p a r t n e r
219200131111     C***
219300131111     C     V1CRPE        whenne    *BLANKS
219400050121     c     leggar4       tag
219500000228      *
219600050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
219700050304     c                             and wgiro <> '2'
219800060216     C     KAR4          READE     FiAR403L                               30
219900991222     c                   else
220000000228      * DPD
220100060216     C     wtrc          READE     FiAR403L                               30
220200000228     C                   If        *in30 = *off
220300060518     c                   movel     ar4n30        dsbl4i
220400000228      *
220500000228     c                   Select
220600000228      * Se non trovo n.parcel e selezionati 0 rcd o più di 1: fine lettura
220700060613     c                   When       §b4ipn <> krpe  and  nr2 <> 1
220800991222     C                   SETON                                            30
220900000228      * Se non trovo n.parcel e è selezionato 1 rcd:  fine lettura e
221000060216      *                                               riposiziono FiAR4
221100060613     c                   When       §b4ipn<> krpe
221200000228     C                   SETON                                            30
221300060216     C     KAR4W         CHAIN     FiAR401L
221400000228      * Se ho trovato il n.parcel ed è il 1o memorizzo key sped
221500000228     c                   When       NR2 = *zeros
221600000228     C                   Z-ADD     AR4AAS        War4AAS
221700000228     C                   Z-ADD     AR4LNP        War4LNP
221800000228     C                   Z-ADD     AR4NRS        War4NRS
221900000228     C                   Z-ADD     AR4NSP        War4NSP
222000000228     c                   Endsl
222100000228     c                   Endif
222200000228      *
222300991222     c                   endif
222400991222     c
222500050304     c                   if        *in30 and wgiro <> '2' and
222600050304     c                             lnpntw <> 'DPD' and lnantw <> 'DPD' and
222700050304     c                             wnodpd = *blanks
222800050121     c                   eval      wgiro = '2'
222900060518     C                   MOVEL     'I'           WTRC
223000050121     C                   MOVEL(P)  KRPe          WN30             30
223100060216     C     Kar4          SETLL     Fiar403l
223200050121     c                   goto      leggar4
223300050121     c                   endif
223400061117     c*
223500061117     c* Se rif.partner può essere DPD non export e immesso rif. lungo 11
223600061117     c* faccio tentaivo di ricerca con rif.truccato
223700061117     c                   if        *in30 and lnantw<>'DPD'and wnodpd=*blanks
223800061117     c                             and %subst(krpe:12:1)=' '
223900061117     C                   eval      krpe='0'+%subst(krpe:1:3)+'99'
224000061117     c                             +%subst(krpe:4:8)
224100061117     C                   MOVEL(P)  KRPe          WN30             30
224200061117     C     Kar4          SETLL     Fiar403L
224300061117     c                   goto      leggar4
224400061117     c                   endif
224500050121     c*
224600971223     C  N30KARB4         CHAIN     FNARB01L                           32
224700131111     c**
224800131111     c* C H I   S O N O
224900131111     c**
225000131111    1C     V1CNOT        whenne    *BLANKS
225100131111     C     KarS          reade     FIarS02L                               30
225200131111     c                   if        not *in30
225300131111     C     KarSXT        CHAIN     FNarT27L                           32
225400131111     C  N32KARTXP        CHAIN     FNarb01L                           32
225500131111    3C     *IN32         IFEQ      *On
225600131111     C                   SETON                                        30
225700131111    3C                   ENDIF
225800131111    2C                   ENDIF
225900110930     c*
226000131111     c* Interrogazione per R M N
226100131111    1C     v1crmn        whengt    *zeros
226200111003     c     v1crmn        reade     fnarb33l                               30
226300940318      *
226400131111     c* Interrogazione per R M A
226500131111    1C     comrma        whenne    *BLANKS
226600111005     c     comrma        reade     fnarb32l                               30
226700131111     c
226800131111      * RSD/RSM/IND/LOD                      -----------------------------------
226900131111     c
227000131113     c                   when      *IN14 OR *IN11
227100131113
227200131111     c                   if        Wend=*off
227300131111     c                   exsr      Readcursor
227400131111     c                   endif
227500131111     c
227600131111     c                   if        Wend=*on
227700131111     c                   seton                                        30
227800131111     c                   endif
227900111005     c*
228000131111     C                   other
228100940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1')  ----------------------------------
228200940318     C   18              DO
228300940713     C     V1CLNA        IFNE      *ZERO
228400020910     C   15K2ARB         READE     FNARB78L                               30
228500020910     C  N15K3ARB         READE     FNARB78L                               30
228600940712      *
228700940712     C                   ELSE
228800940712      *
228900950214     C                   SETON                                            31
229000950214     C                   SETOFF                                           30
229100950214     C     *IN31         DOWEQ     *ON
229200020910     C   15K2ARB         READE     FNARB78L                               31
229300020910     C  N15K3ARB         READE     FNARB78L                               31
229400940318     C   31              ADD       1             C
229500940318     C   31L1(C)         IFNE      0
229600940318     C                   Z-ADD     L1(C)         COMLNA
229700020910     C     K1ARB         SETLL     FNARB78L
229800940318     C                   ELSE
229900950214     C                   SETOFF                                       31
230000940318     C                   SETON                                        30
230100940318     C                   ENDIF
230200940318     C                   ENDDO
230300940318      *
230400940712     C                   ENDIF
230500940318     C                   ENDDO
230600940318      *
230700940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
230800940712     C   19              DO
230900940713     C     V1CLNA        IFNE      *ZERO
231000120330     C     V1CLNA        READE     FNARB05L                               30
231100940712     C                   ELSE
231200120330     C                   READ      FNARB05L                               30
231300940712     C                   ENDIF
231400940712     C                   ENDDO
231500940318      *
231600940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
231700940318     C   20              DO
231800940713     C     V1CLNA        IFNE      *ZERO
231900940712     C   15K5ARB         READE     FNARB54L                               30
232000940713     C  N15V1CLNA        READE     FNARB54L                               30
232100940712      *
232200940712     C                   ELSE
232300940712      *
232400940712     C                   SETON                                            31
232500940712     C     *IN31         DOWEQ     '1'
232600940712     C  N36
232700940712     CAN 15K5ARB         READE     FNARB54L                               31
232800940712     C  N36
232900940712     CANN15COMLNA        READE     FNARB54L                               31
233000060216     C  N31              SETOFF                                       30
233100980120     C   31              ADD       1             C
233200940318     C   31L1(C)         IFNE      0
233300940318     C                   Z-ADD     L1(C)         COMLNA
233400940712     C     K4ARB         SETLL     FNARB54L
233500940318     C                   SETOFF                                       36
233600940318     C                   ELSE
233700940318     C                   SETON                                        30
233800940318     C                   SETOFF                                       31
233900940318     C                   ENDIF
234000940318     C                   ENDDO
234100940318      *
234200940712     C                   ENDIF
234300940318     C                   ENDDO
234400940318      *
234500940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
234600940318     C   21              DO
234700940712     C   01
234800940712     CAN 15K9ARB         READE     FNARB26L                               30
234900940713     C   01
235000940713     CANN15V1CLNA        READE     FNARB26L                               30
235100940712     C   02
235200940712     CAN 15K7ARB         READE     FNARB27L                               30
235300940713     C   02
235400940713     CANN15V1CLP1        READE     FNARB27L                               30
235500131113     c                   ENDDO
235600131113     c                   endsl
235700940318      *
235800060216     c  n32
235900060216     CanN30              EXSR      CONFR
236000900515     C  N30
236100060216     CANN32              EXSR      EMISS
236200900515     C  N30              END
236300131113     c
236400940713     C                   Z-ADD     NR2           COMOD             4 0
236500930518     C                   SETOFF                                       1362
236600131113     c
236700131209     c** 30              if        Wend=*on
236800131209     c   30              if        WopenC='1'
236900131113     c                   exsr      closecursor
237000131113     c                   endif
237100940318      *
237200900515     C                   ENDSR
237300940317      **************************************************************************
237400940317      *    CONFRONTO PER CERCARE BOLLE DA EMETTERE
237500940317      **************************************************************************
237600900515     C     CONFR         BEGSR
237700940317      *
237800911024     C                   SETOFF                                       3236
237900131111     c                   z-add     arbmgs        arbdsp
238000131111     c                   movel     arbaas        arbdsp
238100131111     c
238200110930     c* Se richiamata per Perfect Order devo scartare la bolla se xco diverso da
238300110930     c* quello passato
238400110930     c                   if        pa1flg='W' and arbxco<>pa1xco
238500110930     c                   seton                                        32
238600110930     c                   goto      endcon
238700110930     c                   endif
238800940318      *
238900940713      * DA CONSEGNARE  (18 ON)  (V1CSSP='1')  ----------------------------------
239000940712      * DEVO CONTROLLARE LA DATA SPEDIZIONE (ARBDSP)
239100971223    1C   18              DO
239200131115     c                   if        comdtd>0 and *in15
239300131115    2C     ARBDSP        IFNE      COMDTD
239400940713     C     ARBDCM        ANDEQ     *ZERO
239500131115     c
239600131115    3C                   IF        v1clna<>0 and readfile='DSP'
239700940712     C                   SETON                                            30
239800131115     c                   endif
239900131115
240000131115     C                   SETON                                            32
240100911023     C                   GOTO      ENDCON
240200971223    2C                   END
240300131115     c                   endif
240400940318      *
240500131115     c                   if        comdtd>0 and not *in15
240600131115    2C     ARBDSP        IFGT      COMDTA
240700940713     C     ARBDCM        ANDEQ     *ZERO
240800131115     c* si potrebe uscire anche se lettura per riferimenti mittente
240900131115     c*  ma se la linea di arrivo =0 non lo fa ora .... si può sistemare
241000131115     c*  per far leggere meno
241100131115    3C                   IF        v1clna<>0 and readfile='DSP'
241200131115     C                   SETON                                            30
241300131115     c                   endif
241400131115
241500940712     C                   SETON                                            32
241600911023     C                   GOTO      ENDCON
241700971223    2C                   END
241800131115     c                   endif
241900131115     c
242000131115    1C                   END
242100940318      *
242200940713      * IN CONSEGNA  (19 ON)  (V1CSSP='2')  ------------------------------------
242300940712      * DEVO CONTROLLARE LA DATA DISTINTA (ARBDDC)
242400971223    1C   19              DO
242500960527     C     ARBFBC        IFNE      *BLANKS
242600960527     C                   SETON                                        32
242700960527     C                   GOTO      ENDCON
242800960527     C                   END
242900960527     C*
243000131115     c                   if        comdtd>0 and *in15
243100131115     C     ARBDDC        IFNE      COMDTD
243200911024     C                   SETON                                            32
243300911024     C                   GOTO      ENDCON
243400911024     C                   END
243500131115     C                   ENDif
243600940318      *
243700131115     c                   if        comdtd>0 and not *in15
243800131115     C     ARBDDC        IFLT      COMDTD
243900940712     C     ARBDDC        ORGT      COMDTA
244000911024     C                   SETON                                            32
244100911024     C                   GOTO      ENDCON
244200911024     C                   END
244300131115     C                   ENDif
244400940318      *
244500940321      * SE NON C'E' NUMERO DISTINTA NON E' IN CONSEGNA
244600911024     C     ARBNDC        IFEQ      0
244700911024     C                   SETON                                            32
244800911024     C                   GOTO      ENDCON
244900911024     C                   END
245000971223    1C                   END
245100940318      *
245200940713      * CONSEGNATE  (20 ON)  (V1CSSP='3')  -------------------------------------
245300940712      *  DEVO CONTROLLARE LA DATA CONSEGNA (ARBDCM)
245400971223    1C   20              DO
245500131115     c                   if        comdtd>0 and *in15
245600131115    2C     ARBDCM        IFNE      COMDTD
245700131115    3C                   IF        v1clna<>0 and readfile='DCM'
245800131115     C                   SETON                                            30
245900131115     c                   endif
246000131115
246100940712     C                   SETON                                            32
246200911024     C                   GOTO      ENDCON
246300971223    2C                   END
246400131115     c                   endif
246500940318      *
246600131115     c                   if        comdtd>0 and not *in15
246700131212     C     ARBDCM        IFLT      COMDTD
246800131212     C                   SETON                                            32
246900131212     C                   GOTO      ENDCON
247000131212     C                   END
247100131212     c
247200131115    2C     ARBDCM        IFGT      COMDTA
247300131115    3C                   IF        v1clna<>0 and readfile='DCM'
247400131115     C                   SETON                                            30
247500131115     c                   endif
247600131115
247700940712     C                   SETON                                            32
247800911024     C                   GOTO      ENDCON
247900971223    2C                   END
248000131115     c                   endif
248100131115     c
248200131115      * SE NON C'E' data consegna  NON E' consegnata
248300131115     C     ARBDCM        IFEQ      0
248400131115     C                   SETON                                            32
248500131115     C                   GOTO      ENDCON
248600131115     C                   ENDIF
248700131115    1C                   END
248800940329      * ------------------------------------------------------------------------
248900940712      * HO SPOSTATO IL CONFRONTO CON MITTENTE E DESTINATARIO PRIMA DEL BLOCCO
249000940712      * CON 21 ON PERCHE' POSSO ACCENDERE IL 30 E ANDARE A FINE CICLO QUANDO
249100940712      * CAMBIA LA RAG.SOCIALE (EVITO DELLE LETTURE)
249200940712      *
249300940712      * OCCHIO!!   Devo usare per primo il confronto che potrebbe portarmi a
249400940712      * fine ciclo: se interrogo per destinatario (14ON) ci deve essere prima il
249500940713      * confronto con V1CRSD, se invece sono per mittente devo fare prima quello
249600940713      * con V1CRSM.
249700060613     C                   MOVE      *BLANKS       $SALTA            1
249800940712      *
249900940712      *??  R A G I O N E    S O C I A L E    D E S T I N A T A R I O  ?
250000131113     c                   if        not *in11 and not *in14
250100131113    1C     V1CRSD        IFNE      *BLANKS
250200131113     c
250300131113    2c     v1cxrsd       ifeq      ' '
250400131113     C     ZZ            SUBST(P)  ARBRSD        COMRAG
250500940712      * Se rag.soc. letta > video riposiziono solo per data con SETGT
250600131113    3C     COMRAG        IFNE      V1CRSD
250700131113     C                   MOVE      'S'           $SALTA
250800131113    3C                   ENDIF
250900131113     c*
251000131113   x2c                   else
251100131114     C                   MOVEL     ARBAAS        KAAS
251200131114     C                   MOVEL     ARBLNP        KLNP
251300131114     C                   MOVEL     ARBNRS        KNRS
251400131114     C                   MOVEL     ARBNSP        KNSP
251500131114     C                   MOVEL     'D'           KTRC
251600131114     C     KAR41         CHAIN     FiAR401L
251700131114     c                   if        %found(fiar401l)
251800131114     c                   eval      arbrsduni=arbrsd+ar4not
251900131114     c                   else
252000131114     c                   eval      arbrsduni=arbrsd
252100131114     c                   endif
252200131114     c
252300131114    3c                   if        %scan(%trimr(v1crsd):arbrsdUNI) = 0
252400131113     C                   MOVE      'S'           $SALTA
252500131113    3c                   endif
252600131113    2C                   ENDIF
252700131113    1C                   ENDIF
252800131113     c
252900131113      *??  I N D I R I Z Z O     D E S T I N A T A R I O  ?
253000131113    1C     V1Cind        IFNE      *BLANKS
253100131113     c
253200131113    2c     v1cxind       ifeq      ' '
253300131113     C     DD            SUBST(P)  ARBind        COMRAG           30
253400131113      * Se rag.soc. letta > video riposiziono solo per data con SETGT
253500131113    3C     COMRAG        IFNE      V1Cind
253600131113     C                   MOVE      'S'           $SALTA
253700131113    3C                   ENDIF
253800131113     c*
253900131113   x2c                   else
254000131113    3c                   if        %scan(%trimr(v1cind):arbind) = 0
254100131113     C                   MOVE      'S'           $SALTA
254200131113    3c                   endif
254300131113    2C                   ENDIF
254400131113    1C                   ENDIF
254500131113      *??  L O C A L I T A'      D E S T I N A T A R I O  ?
254600131113    1C     V1Clod        IFNE      *BLANKS
254700131113     c
254800131113    2c     v1cxlod       ifeq      ' '
254900131113     C     LL            SUBST(P)  ARBlod        COMRAG
255000131113      * Se rag.soc. letta > video riposiziono solo per data con SETGT
255100131113    3C     COMRAG        IFNE      V1Clod
255200131113     C                   MOVE      'S'           $SALTA
255300131113    3C                   ENDIF
255400131113     c*
255500131113   x2c                   else
255600131113    3c                   if        %scan(%trimr(v1clod):arblod) = 0
255700131113     C                   MOVE      'S'           $SALTA
255800131113    3c                   endif
255900131113    2C                   ENDIF
256000131113    1C                   ENDIF
256100940414      *
256200940712      *??  R A G I O N E    S O C I A L E    M I T T E N T E  ?
256300131113    1C     V1CRSM        IFNE      *BLANKS
256400131113    2c     v1cxrsm       ifeq      ' '
256500940712     C     XX            SUBST(P)  ARBRSM        COMRAG
256600940712      * Se rag.soc. letta > video riposiziono solo per data con SETGT
256700131113    3C     COMRAG        IFne      V1CRSM
256800940712     C                   MOVE      'S'           $SALTA
256900131113    3C                   ENDIF
257000131113   x2c                   else
257100131113    3c                   if        %scan(%trimr(v1crsm):arbrsm) = 0
257200131113     C                   MOVE      'S'           $SALTA
257300131113    3c                   endif
257400131113    2C                   ENDIF
257500131113    1C                   ENDIF
257600131113    1C                   ENDIF
257700940712      *
257800131115     c                   if        $SALTA='S'
257900131115     c                   seton                                        32
258000131115     C                   goto      ENDCON
258100131115     c                   endif
258200940318      *
258300940713      * TUTTE  (21 ON)  (V1CSSP=' ')  ------------------------------------------
258400971223    1C   21              DO
258500940712     C     ARBDSP        IFLT      COMDTD
258600940712     C     COMDTD        ANDNE     *ZERO
258700940712     C                   SETON                                            32
258800911024     C                   GOTO      ENDCON
258900911024     C                   END
259000940318      *
259100940321     C     ARBDSP        IFGT      COMDTA
259200940712     C     COMDTA        ANDNE     *ZERO
259300940712     C                   SETON                                            32
259400131115     c                   if        readfile='DSP'
259500131115     C                   SETON                                            30
259600131115     c                   endif
259700131115     c
259800911024     C                   GOTO      ENDCON
259900131115     C                   ENDif
260000131115     c
260100971223    1C                   ENDDO
260200971223     C**
260300940317      *
260400940317      * PER OGNI SELEZIONE  ----------------------------------------------------
260500940317      *
260600940317      * TESTO LA LINEA DI ARRIVO
260700971223     C     V1CRPE        IFEQ      *BLANKS
260800971223     C     ARBLNA        ANDNE     V1CLNA
260900940713     C     V1CLNA        ANDNE     *ZERO
261000920911     C                   SETON                                            32
261100920911     C                   GOTO      ENDCON
261200971223     C                   ENDIF
261300020628      * testo la nazione/provincia del destinatario
261400060613     c                   if        v1cprd <> *blanks  and
261500020628     c                             (arbnzd <> v1cnzd  or arbprd <> v1cprd)
261600020628     c                   eval      *in32 = *on
261700020628     c                   goto      endcon
261800020628     c                   endif
261900060613     c                   if        (v1cnzd <> *blanks and v1cprd = *blanks) and
262000060613     c                             arbnzd <> v1cnzd
262100060613     c                   eval      *in32 = *on
262200060613     c                   goto      endcon
262300060613     c                   endif
262400020628
262500940317      *
262600940712      * VERIFICO ANCHE LA LNP POICHE' POTREI AVER FATTO ALTRE SELEZIONI PRIMARIE
262700940713     C     ARBLNP        IFNE      V1CLP1
262800940713     C     V1CLP1        ANDNE     *ZERO
262900940712     C                   SETON                                            32
263000940712     C                   GOTO      ENDCON
263100940712     C                   END
263200940923     C* FILIALE SEGNACOLLI
263300940926     C* SE NON VI SONO COLLI, SALTARE QUESTO CONTROLLO
263400940926     C     ARBNCD        IFNE      *ZERO
263500940926     C     ARBNCA        ORNE      *ZERO
263600940926     C     ARBFNS        OREQ      'S'
263700940926     C     ARBFLS        ORNE      *ZERO
263800940923     C     V1CFLS        IFNE      *ZERO
263900940923     C     V1CFLS        ANDNE     ARBFLS
264000940923     C                   SETON                                            32
264100940923     C                   GOTO      ENDCON
264200940926     C                   ENDIF
264300940926     C                   ENDIF
264400961112     C* RESO / FRANCO FRONTIERA
264500961112     C     V1CFBR        IFNE      ' '
264600961112     C                   MOVEL     V1CFBR        WFBR
264700961112     C     V1CFBR        IFEQ      'R'
264800961112     C                   MOVEL     'S'           WFBR
264900961112     C                   ENDIF
265000961112     C     ARBFBR        IFNE      WFBR
265100961112     C                   SETON                                            32
265200961112     C                   GOTO      ENDCON
265300961112     C                   ENDIF
265400961112     C                   ENDIF
265500040216      * Tipo bolla
265600040216     c                   if            V1Ccbo <> *blanks
265700040216     c                             and V1Ccbo <> ARBcbo
265800040216     c                   eval      *in32 = *on
265900040216     c                   goto      EndCon
266000040216     c                   endif
266100900515     C* TIPO SPEDIZIONE
266200940715     C     ARBTSP        IFNE      V1CTSP
266300940715     C     V1CTSP        ANDNE     *BLANKS
266400900515     C                   SETON                                            32
266500900515     C                   GOTO      ENDCON
266600900515     C                   END
266700911023     C* SPEDIZIONI GIACENTI
266800980603     C     V1CGIA        IFEQ      'G'
266900930702     C     ARBFBC        ANDNE     'G'
267000911023     C                   SETON                                            32
267100911023     C                   GOTO      ENDCON
267200911023     C                   END
267300980603     C     V1CGIA        IFEQ      'N'
267400930702     C     ARBFBC        ANDEQ     'G'
267500911023     C                   SETON                                            32
267600911023     C                   GOTO      ENDCON
267700911023     C                   END
267800070420     c*
267900070420     c* SPEDIZIONI IN FERMO DEPOSITO
268000070420     C                   IF        v1csfd='S' and arbffd<>'S'
268100070420     C                   SETON                                            32
268200070420     C                   GOTO      ENDCON
268300070420     C                   END
268400070420     c
268500900518     C* SERIE DAL + AL
268600940713     C     ARBNRS        IFLT      V1CPSD
268700900518     C                   SETON                                            32
268800900518     C                   GOTO      ENDCON
268900900518     C                   END
269000940713     C     ARBNRS        IFGT      V1CPSA
269100900515     C                   SETON                                            32
269200900515     C                   GOTO      ENDCON
269300900515     C                   END
269400900518     C* SEGNACOLLO DAL SOLO
269500930702     C     *IN07         IFEQ      *ON
269600970925     C     *IN84         ANDEQ     *OFF
269700940713     C     KART          SETLL     FNART01L                               31
269800930702     C  N31              SETON                                        32
269900930702     C  N31              GOTO      ENDCON
270000930702     C                   END
270100900515     C* SEGNACOLLO DAL + AL
270200970925     C   84              DO
270300910827     C                   SETON                                        32
270400940713     C     KARB2         SETLL     FNART01L
270500940713     C     KARB2         READE     FNART01L                               31
270600910827     C     *IN31         DOWEQ     '0'
270700940713     C     ARTNSC        IFGE      V1CSCD
270800940713     C     ARTNSC        ANDLE     V1CSCA
270900910827     C                   SETON                                        31
271000910827     C                   SETOFF                                       32
271100910827     C                   END
271200910827     C*
271300940713     C  N31KARB2         READE     FNART01L                               31
271400910827     C                   END
271500910827     C*
271600910827     C   32              GOTO      ENDCON
271700910827     C                   END
271800990119     C*
271900990119     C* NUMERO CA
272000990119    1C     V1CDAN        IFNE      ' '
272100040809     C                   SETON                                        32
272200040809      *
272300040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
272400040809      * legate alla spedizione che passo
272500040809     c                   clear                   fidn12ds
272600040809     c                   eval      i12aas = arbAAS
272700040809     c                   eval      i12lnp = arbLNP
272800040809     c                   eval      i12nrs = arbNRS
272900040809     c                   eval      i12nsp = arbNSP
273000040809     c                   eval      i12fel = 'E'
273100040809     c                   eval      i12fan = 'E'
273200040809     c*
273300040809     c                   call      'FIDN12R'
273400040809     c                   parm                    fidn12ds
273500040809     c*
273600040809     c                   setoff                                       31
273700040809     c*
273800040809     c* se non ci sono errori
273900040809     c                   if        o12err <> ' '
274000040809     c                   seton                                        31
274100040809     c                   else
274200040809     c* se numero di CA trovate maggiore di zero
274300040809     c                   if        o12nca = 0
274400040809     c                   seton                                        31
274500040809     c                   else
274600040809     c                   z-add     1             ii                2 0
274700040809     c                   dow       ii <= o12nca and *in31 = *off
274800040809     C* APERTA
274900040809     C     skdch(ii)     IFEQ      0
275000040809     C     V1CDAN        ANDEQ     'A'
275100040809     C                   SETOFF                                       32
275200040809     C                   SETON                                        31
275300040809     C                   ENDIF
275400040809     C* CHIUSE
275500040809     C     skdch(ii)     IFGT      0
275600040809     C     V1CDAN        ANDEQ     'C'
275700040809     C                   SETOFF                                       32
275800040809     C                   SETON                                        31
275900040809     C                   ENDIF
276000040809     C* CA APERTA O CHIUSA
276100040809     C     V1CDAN        IFEQ      'E'
276200040809     C                   SETOFF                                       32
276300040809     C                   SETON                                        31
276400040809     C                   ENDIF
276500040809     c*
276600040809     c                   add       1             ii
276700040809     c                   enddo
276800040809     c*
276900040809     c                   seton                                        31
277000040809     c*
277100040809     c                   endif
277200040809     c                   endif
277300040809     c**
277400990119    3C                   ENDIF
277500030527     C*
277600940317     C     ENDCON        ENDSR
277700940802     C**************************************************************************
277800940802     C* RICHIAMO PGM DI INTERROGAZIONE PARTENZE
277900940802     C**************************************************************************
278000930922     C     INTPAR        BEGSR
278100940802     C*
278200940722     C                   MOVEL     'A'           PA2FLG
278300930922     C                   MOVEL     PARAM2        KPJBU
278400940722     C                   CALL      'FNLS04R'
278500930922     C                   PARM                    KPJBA
278600940802     C*
278700930922     C                   ENDSR
278800131114     C**************************************************************************
278900131114     C* Atre parzializzazioni
279000131114     C**************************************************************************
279100131114     C     AltrePar      BEGSR
279200131114     c     forw12        TAG
279300131114     c                   exfmt     LRI6W12
279400131114     c
279500131114     c                   setoff                                       285690
279600131114     c                   setoff                                       40
279700131114     c
279800131114     C* GIACENTI
279900131114     C* SCELTE INCONGRUENTI
280000131114     C     V1CGIA        IFEQ      'G'
280100131114     C     V1CSSP        IFEQ      '2'
280200131114     C     V1CSSP        OREQ      '3'
280300131114     C                   MOVEL     MSG(14)       w12MSG
280400131114     C                   SETON                                        285690
280500131114     C                   GOTO      forw12
280600131114     C                   END
280700131114     C                   END
280800131114      * Tipo bolla
280900131114if  1c                   if        V1Ccbo <> *blanks
281000131114     c                   eval      *in31  =  *off
281100131114     c     '?'           scan      V1Ccbo                                 31
281200131114if  2c                   if        *in31 = *on
281300131114     c                   call      'X§TABER'
281400131114     c                   parm      CodUt         §KUT
281500131114     c                   parm      '3A'          COD
281600131114     c                   parm      *blanks       KEY
281700131114     c                   parm      *blanks       §DES
281800131114     c                   movel     KEY           V1Ccbo
281900131114     c                   eval      *in90 = *on
282000131114     c                   goto      forw12
282100131114e   2c                   endif
282200131114      *
282300131114     c                   movel     '3A'          COD
282400131114     c                   movel(p)  V1Ccbo        KEY
282500131114     c     KTAB1         chain     TABEL
282600131114if  2c                   if        NOT %found(TABEL00F)
282700131114     c                             or  TBLflg <> *blanks
282800131114     c                   movel     Msg(9)        w12msg
282900131114     c                   seton                                        284090
283000131114     c                   goto      forw12
283100131114e   2c                   endif
283200131114e   1c                   endif
283300131114     c
283400131114     c                   if        v1cgia<>' ' or v1cdan<>' ' or v1caff<>'N'
283500131114     c                             or v1cfbr<>' ' or v1ccbo<>*blanks
283600131114     c                   seton                                        09
283700131114     c                   else
283800131114     c                   setoff                                       09
283900131114     c                   endif
284000131114     c
284100131114     C*
284200131114     C                   ENDSR
284300940802     C**************************************************************************
284400940802     C* EMISSIONE SUBFILE
284500940802     C**************************************************************************
284600900515     C     EMISS         BEGSR
284700940802     C*
284800930407     C                   SETOFF                                       1362
284900940714     C                   MOVEL     *BLANKS       V2CSCE
285000131121     c**                 if        *in87=*off and not *in02
285100131121     C**                 MOVEL     ARBLNP        V2CLIN
285200131121     c**                 endif
285300131121     c**                 if        *in87 or *in02
285400131121     C**                 MOVEL     ARBLNA        V2CLIN
285500131121     c**                 endif
285600940714     C                   MOVEL     ARBLNP        V2CLNP
285700941212     C                   MOVEL     ARBLNA        V2CLNA
285800911023     C                   MOVEL     ARBMGS        MMGG
285900940714     C                   MOVEL     GG            V2CDSP
286000940714     C                   MOVE      MM            V2CDSP
286100940714     C                   MOVE      ARBAAS        V2CAAS
286200950114     C                   MOVEL     ARBNSP        V2CNSP
286300900516     C*
286400060130     C                   MOVEL     ARBksc        V2Cksc
286500060130     C                   MOVEL     ARBcbo        V2Ccbo
286600060130     C                   MOVEL     ARBRSM        V2CRSM
286700940714     C                   MOVEL     ARBRSD        V2CRSD
286800131114     c                   if        v1cind=*blanks
286900131114     c                   eval      v2cdest=arblod
287000131114     c                   else
287100131114     c                   eval      v2cdest=arbind
287200131114     c                   endif
287300131114     c                   if        arbrma<>*blanks
287400131114     c                   eval      v2crifm=%trim(%editc(arbrmn:'Z')) +'/'+
287500131114     c                                     %trim(arbrma)
287600131114     c                   else
287700131114     c                   eval      v2crifm=%trim(%editc(arbrmn:'Z'))
287800131114     c                   endif
287900131114     C**                 Z-ADD     ARBRMN        V2CRMN
288000131114     C**                 MOVEL     ARBRMA        V2CRMA
288100940714     C                   MOVEL     ARBNRS        V2CNRS
288200940923     C                   MOVEL     ARBFLS        V2CFLS
288300940714     C                   MOVEL     *BLANKS       V2CFLA
288400990119     C* SPEDIZIONE IN DANNO
288500990119     C* SE MI HANNO CHIESTO LE CA CHIUSE VISUALIZZO IL FLAG "C"
288600990119     C* SE MI HANNO CHIESTO LE CA APERTE VISUALIZZO IL FLAG "*"
288700990119     C                   SELECT
288800990119     C     V1CDAN        WHENEQ    'C'
288900990119     C                   MOVEL     'C'           V2CDAN
289000990119     C     V1CDAN        WHENEQ    'A'
289100990119     C                   MOVEL     '*'           V2CDAN
289200990119     C**
289300990119     C                   OTHER
289400040809      *
289500040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
289600040809      * legate alla spedizione che passo
289700040809     c                   clear                   fidn12ds
289800040809     c                   eval      i12aas = arbAAS
289900040809     c                   eval      i12lnp = arbLNP
290000040809     c                   eval      i12nrs = arbNRS
290100040809     c                   eval      i12nsp = arbNSP
290200040809     c                   eval      i12fel = 'E'
290300040809     c                   eval      i12fan = 'E'
290400040809     c*
290500040809     c                   call      'FIDN12R'
290600040809     c                   parm                    fidn12ds
290700040809     c*
290800040809     c                   setoff                                       31
290900040809     c*
291000040809     c* se non ci sono errori
291100040809     c                   if        o12err <> ' '
291200040809     c                   seton                                        31
291300040809     c                   else
291400040809     c* se numero di CA trovate maggiore di zero
291500040809     c                   if        o12nca = 0
291600040809     c                   seton                                        31
291700040809     c                   else
291800040809     c                   z-add     1             ii                2 0
291900040809     c                   dow       ii <= o12nca and *in31 = *off
292000040809     C* SE NE TROVO UNA APERTA, DICO APERTA ALTRIMENTI CHIUSA
292100040809     C     skdch(ii)     IFEQ      0
292200040809     C                   MOVEL     '*'           V2CDAN
292300040809     C                   SETON                                            31
292400040809     C                   ELSE
292500040809     C* CHIUSA MA VADO AVANTI A LEGGERE
292600040809     C                   MOVEL     'C'           V2CDAN
292700040809     C                   ENDIF
292800040809     c*
292900040809     c                   add       1             ii
293000040809     c                   enddo
293100040809     c*
293200040809     c                   seton                                        31
293300040809     c*
293400040809     c                   endif
293500040809     c                   endif
293600990119     C**
293700990119     C                   ENDSL
293800921102     C* SEGNACOLLI
293900930702     C                   CLEAR                   SEG
294000921102     C                   MOVEL     ARBNCD        COMSCN            7
294100921102     C                   MOVEA     COMSCN        SEG(1)
294200921102     C     ARBNCA        IFNE      0
294300921102     C                   MOVEL     ARBNCA        COMSCN
294400921102     C                   MOVEA     COMSCN        SEG(9)
294500921102     C                   END
294600921102     C                   MOVEA     '/'           SEG(8)
294700921102     C* ELIMINO ZERO NON SIGNIFICATIVI
294800921102     C                   Z-ADD     1             Y
294900921102     C     SEG(Y)        DOWEQ     '0'
295000921102     C                   MOVE      ' '           SEG(Y)
295100921102     C                   ADD       1             Y
295200921102     C                   END
295300921102     C*
295400921102     C     ARBNCA        IFNE      0
295500921102     C                   Z-ADD     9             Y                 2 0
295600921102     C     SEG(Y)        DOWEQ     '0'
295700921102     C                   MOVE      ' '           SEG(Y)
295800921102     C                   ADD       1             Y
295900921102     C                   END
296000921102     C                   END
296100940714     C                   MOVEA     SEG           V2CSEG
296200911023     C*
296300911023     C* GIACENZA APERTA
296400941202     C                   Z-ADD     ARBAAS        KAAS
296500940713     C                   Z-ADD     ARBLNP        KLNP
296600940713     C                   Z-ADD     ARBNRS        KNRS
296700940713     C                   Z-ADD     ARBNSP        KNSP
296800050405     C     KGCA1         CHAIN     tigcp21l                           31
296900920410     C  N31GCPATB        IFEQ      ' '
297000920410     C     GCPDCG        IFNE      0
297100990119     C                   MOVE      'C'           V2CGCA
297200911023     C                   ELSE
297300990119     C                   MOVE      '*'           V2CGCA
297400900515     C                   END
297500920410     C                   END
297600990119     C                   MOVEL     ARBTSP        V2CTSP
297700930503     C*
297800911023     C* DATA CONSEGNA
297900940714     C                   MOVEL     *BLANKS       V2CCON
298000940714     C                   MOVEL     *BLANKS       V2CFCC
298100930407     C*
298200930407     C* SE ARBCCA >< 0  METTO DECODIFICA AL POSTO DELLA DATA CONSEGNA
298300930407     C     ARBCCA        IFNE      ' '
298400930705     C                   Z-ADD     1             C
298500930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
298600940909     C   31              MOVEL     DCA(C)        DS7O
298700940909     C*
298800940909     C* NELLA DS7O C'E' SCRITTO SE E' PRIORITARIA L'ANOMALIA O LA DATA
298900940909     C   31§7OVCA        IFEQ      'S'
299000940912     C     §7OVCA        ORNE      'S'
299100940912     C     ARBDCM        ANDEQ     *ZERO
299200940909     C                   MOVEL     §7ODEI        V2CCON
299300940909     C                   SETON                                        62
299400940912     C                   ELSE
299500940909     C*
299600940912     C     ARBDCM        IFNE      *ZERO
299700940909     C                   MOVE      ARBDCM        G02INV
299800940909     C                   Z-ADD     *ZEROS        G02DAT
299900940909     C                   MOVEL     '3'           G02ERR
300000940909     C                   CALL      'XSRDA8'
300100940909     C                   PARM                    WLBDAT
300200940909     C                   MOVEL     G02DAT        V2CCON
300300940909     C                   MOVE      G02DAT        $COMOD            2
300400940909     C                   MOVE      $COMOD        V2CCON
300500940909     C                   ENDIF
300600940912     C                   ENDIF
300700930407     C*
300800930407     C                   ELSE
300900940912     C     ARBDCM        IFNE      *ZERO
301000940712     C                   MOVE      ARBDCM        G02INV
301100940712     C                   Z-ADD     *ZEROS        G02DAT
301200940712     C                   MOVEL     '3'           G02ERR
301300940712     C                   CALL      'XSRDA8'
301400940712     C                   PARM                    WLBDAT
301500940714     C                   MOVEL     G02DAT        V2CCON
301600940713     C                   MOVE      G02DAT        $COMOD            2
301700940714     C                   MOVE      $COMOD        V2CCON
301800940912     C                   ENDIF
301900940912     C                   ENDIF
302000940912     C*
302100940912     C     ARBDCM        IFNE      0
302200930428     C*
302300930506     C* SE ARBDCE = 0  CALL PGM PER CALCOLO DELLA DATA CONSEGNA PREVISTA
302400930506     C*   E DELLE ORE DI RITARDO
302500940712      * SOLO SE RICHIESTA L'AFFIDABILITA'
302600940713     C     V1CAFF        IFEQ      'S'
302700930506     C     ARBDCE        IFEQ      0
302800991027     C                   CLEAR                   TNSD99DS
302900930506     C                   MOVEL     'A'           D98TBO
303000930506     C                   MOVEL     ARBAAS        D98AAS
303100930506     C                   MOVEL     ARBLNP        D98LNP
303200930506     C                   MOVEL     ARBNRS        D98NRS
303300930506     C                   MOVEL     ARBNSP        D98NSP
303400930506     C*
303500970925     C                   CALL      'TNSD99R'
303600991027     C                   PARM                    TNSD99DS
303700930506     C*
303800941214     C* SE PA098 = 1  E' STATA FATTA CALL PGM
303900941214     C                   MOVEL     '1'           PA098
304000930506     C*
304100960201     C* SE C'E' ERRORE VISUALIZZO UNA "R"
304200110301     C     D98ERR        IFNE      '0'
304300960201     C                   MOVEL     'R'           V2CFCC
304400960201     C                   ELSE
304500930506     C* SE D98CRC = S  LA SPEDIZ. E' STAT CONSEGNATA NEI TEMPI PREVISTI
304600930506     C     D98CRC        IFEQ      'S'
304700940714     C                   MOVEL     ' '           V2CFCC
304800930506     C                   ELSE
304900940714     C                   MOVEL     'F'           V2CFCC
305000930506     C                   END
305100960201     C                   END
305200930506     C*
305300930506     C                   ELSE
305400930506     C* ARBDCE > 0
305500940711     C* SE ARBNRC > 0  LA SPEDIZIONE NON E' CONFORME
305600940711     C     ARBNRC        IFGT      0
305700940714     C                   MOVEL     'F'           V2CFCC
305800930504     C                   ELSE
305900940714     C                   MOVEL     ' '           V2CFCC
306000930504     C                   END
306100930506     C                   END
306200940713     C                   ENDIF
306300930402     C*
306400911023     C                   ELSE
306500911023     C* SE NUMERO >< 0 IN CONSEGNA
306600911023     C     ARBNDC        IFNE      0
306700940714     C                   MOVEL     'IN CON'      V2CCON
306800911023     C                   SETON                                        13
306900911023     C                   END
307000911023     C                   END
307100921102     C*
307200940714     C     V2CCON        IFEQ      *BLANKS
307300921102     C     ARBFBC        ANDNE     ' '
307400921102     C                   SETON                                        62
307500921102     C*
307600921102     C     ARBFBC        IFEQ      'B'
307700930504     C     ARBFBC        OREQ      '*'
307800940714     C                   MOVEL     'BLOC.'       V2CCON
307900921102     C                   ELSE
308000921102     C     ARBFBC        IFEQ      'A'
308100940714     C                   MOVEL     'L.AVV'       V2CCON
308200921102     C                   ELSE
308300940714     C                   MOVEL     'GIAC.'       V2CCON
308400921102     C                   END
308500921102     C                   END
308600921102     C                   END
308700911023     C*
308800940714     C                   MOVEL     ARBTMC        V2CTMC
308900900522     C*
309000900518     C                   ADD       1             B
309100940713     C                   ADD       1             NR2
309200131107     C                   WRITE     LRI6S02
309300940720     C*
309400900515     C                   ENDSR
309500940802     C**************************************************************************
309600940802     C* EMISSIONE BOLLA
309700940802     C**************************************************************************
309800911023     C     BOLLA         BEGSR
309900940802     C*
310000070411     C                   SETOFF                                       220616
310100070403     C                   SETOFF                                       170377
310200070404     C                   SETOFF                                       262708
310300131127     C                   SETOFF                                       2933
310400940408     C                   SETOFF                                       687069
310500070524     C                   SETOFF                                       850488
310600131120     C                   SETOFF                                       86
310700991027      *
310800940719     C                   Z-ADD     0             V3CLNA
310900940719     C                   Z-ADD     0             V3CDSP
311000930517     C                   Z-ADD     0             VIDRMN
311100940712     C                   MOVE      *BLANKS       VIDRMA
311200991217     C                   CLEAR                   V3CRPP
311300131120     C                   CLEAR                   atrpda
311400131127     C                   CLEAR                   atrtsp
311500961112     C*
311600911022     C* CONTROLLO SE SPEDIZIONE GIACENTE
311700920219     C* GIACENZA APERTA
311800941202     C                   Z-ADD     ARBAAS        KAAS
311900940713     C                   Z-ADD     ARBLNP        KLNP
312000940713     C                   Z-ADD     ARBNRS        KNRS
312100940713     C                   Z-ADD     ARBNSP        KNSP
312200050405     C     KGCA1         CHAIN     tigcp21l                           31
312300040216if  2C  N31GCPATB        IFEQ      ' '
312400920410     C     GCPDCG        IFNE      0
312500920219     C                   SETON                                        03
312600920219     C                   ELSE
312700070403     C                   SETON                                        0327
312800070412     C                   EVAL      V1DFBC=' GIACENTE '
312900040217     C                   ENDIF
313000040217e   2C                   ENDIF
313100930428     C*
313200021213     C*
313300021213     C* 77 ON - Esistono note supermenrcati d ainterrogare
313400021213     C                   Z-ADD     ARBAAS        KAAS
313500021213     C                   Z-ADD     ARBLNP        KLNP
313600021213     C                   Z-ADD     ARBNRS        KNRS
313700021213     C                   Z-ADD     ARBNSP        KNSP
313800021213     C                   MOVEL     'SCR'         KTRD
313900021213     C     KAR51         CHAIN     FIAR5000                           31
314000021217     C  n31              seton                                        77
314100021217     C   31              setoff                                       77
314200920219     C*
314300920207     C* SE BOLLA CON LA "G" SENZA GIACENZA MECCANIZZATA SEGNALO
314400931116     C     ARBFBC        IFEQ      'G'
314500070412     C                   EVAL      V1DFBC=' GIACENTE '
314600070403     C                   SETON                                        27
314700040217     C                   ENDIF
314800930415     C*
314900940907     C                   MOVEL     ARBAAS        V3CAAS
315000940719     C                   MOVE      ARBNSP        V3CNSP
315100940719     C                   MOVEL     ARBNRS        V3CNRS
315200070404     C                   MOVEL     ARBNRS        V4CNRS
315300131108     C********           MOVEL     ARBNRS        V7CNRS
315400940719     C                   MOVEL     ARBLNP        V3CLNP
315500950508     C                   MOVEL     ARBLNP        V3CLPA
315600131114     c
315700131114     c                   z-add     arbmgs        arbdsp
315800131114     c                   movel     arbaas        arbdsp
315900940908     C*
316000940908     C*  L E G A M I    B O L L A
316100940908     C  N75              EXSR      LEGBOL
316200940908     C*
316300911022     C* NON SEQUENZIALI
316400911023     C     ARBFNS        IFEQ      'S'
316500930705     C                   MOVEL     CNOSEQ        DESFNS
316600911022     C                   ELSE
316700911022     C                   MOVEL     *BLANKS       DESFNS
316800040217     C                   ENDIF
316900930525     C*
317000930525     C* 95 ON - ESITONO EVENTI DA INTERROGARE
317100940713     C                   Z-ADD     ARBAAS        KAAS$
317200940713     C                   Z-ADD     ARBLNP        KLNP
317300940713     C                   Z-ADD     ARBNRS        KNRS
317400940713     C                   Z-ADD     ARBNSP        KNSP
317500990805     C     KEVB1         SETLL     FNEVB01L                               95
317600920402     C*
317700920403     C* 98 ON - ESITONO VARIAZIONI FATTE
317800131007     C     KARB2         SETLL     FiARBF2C                               98
317900990119     C* 04 ON - ESISTONO CA DA VISUALIZZARE
318000990119     C                   CLEAR                   CONTCA
318100040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
318200040809      * legate alla spedizione che passo
318300040809     c                   clear                   fidn12ds
318400040809     c                   eval      i12aas = arbAAS
318500040809     c                   eval      i12lnp = arbLNP
318600040809     c                   eval      i12nrs = arbNRS
318700040809     c                   eval      i12nsp = arbNSP
318800040809     c                   eval      i12fel = 'E'
318900040809     c                   eval      i12fan = 'E'
319000040809     c*
319100040809     c                   call      'FIDN12R'
319200040809     c                   parm                    fidn12ds
319300040809     c*
319400040809     c                   setoff                                       31
319500040809     c*
319600040809     c* se non ci sono errori
319700040809     c                   if        o12err <> ' '
319800040809     c                   seton                                        31
319900040809     c                   else
320000040809     c* se numero di CA trovate maggiore di zero
320100040809     c                   if        o12nca = 0
320200040809     c                   seton                                        31
320300040809     c                   else
320400040809     c                   z-add     1             ii                2 0
320500040809     c                   dow       ii <= o12nca and *in31 = *off
320600040809     c                   movea     skkey(ii)     dskey
320700040809     C                   SETON                                        04
320800040809     C                   ADD       1             CONTCA            3 0
320900040809     C                   Z-ADD     dsaac         VHCAAC
321000040809     C                   Z-ADD     dsfil         VHCFIL
321100040809     C                   Z-ADD     dsnca         VHCNCA
321200040809     c*
321300040809     c                   add       1             ii
321400040809     c                   enddo
321500040809     c*
321600040809     c                   seton                                        31
321700040809     c*
321800040809     c                   endif
321900040809     c                   endif
322000040809     c**
322100070406     c* se Presente FIARP, imposto la cons.richista proposta
322200070404     c                   clear                   v4cdimpr
322300070404     c                   clear                   v4coimpr
322400070404     c                   clear                   v4cprupr
322500070416     c                   clear                   v08dcrpr
322600070404     c                   clear                   v4ctcrpr
322700070404     c                   clear                   v4chcrpr
322800070406     c
322900070406     c* Solo se visualizzabile
323000070406     c     karb2         chain     fiarp01l
323100070406    1c                   if        %found(FIarp01l)
323200070404     c                   seton                                        26
323300070404     C                   MOVEL     arppru        v4cprupr
323400070404     C                   MOVEL     arpoim        v4coimpr
323500070404    2c                   if        arpdim>0
323600070404     c     *iso          movel     arpdim        dataeur
323700070416     c                   movel     dataeur       datadmy
323800070416     c                   movel     datadmy       w0060             6 0
323900070416     c                   movel     w0060         v4cdimpr
324000070404    2c                   endif
324100070404    2c                   if        arpdcr>0
324200070404     c     *iso          movel     arpdcr        dataeur
324300070416     c                   movel     dataeur       v08dcrpr
324400070416     c**                 movel     datadmy       v06dcrpr
324500070404    2c                   endif
324600070404     c                   movel     arphcr        v4chcrpr
324700070404     c                   movel     arptcr        v4ctcrpr
324800070406     c                   if        arptcr=' '
324900070406     c                   eval      v4ctcrpr='IL'
325000070404    1c                   endif
325100070406    1c                   endif
325200070406     c
325300911022     C* TIPO BOLLA CONTROLLO SE FRANCO
325400940719     C                   MOVEL     ARBCBO        V3CCBO
325500911022     C                   MOVEL     '3A'          COD
325600911022     C                   MOVEL     *BLANKS       KEY
325700940712     C                   MOVEL     ARBCBO        KEY
325800940713     C     KTAB1         CHAIN     TABEL                              31
325900911022     C                   MOVEL     TBLUNI        DS3A
326000911022     C                   MOVEL     §3ATB1        FLGTB1            1
326100940719     C                   MOVEL     §3ADES        V3DCBO
326200940719     C                   MOVEL     §3ADES        V5DCBO
326300911030     C* FRANCO
326400040216if  1C     FLGTB1        IFEQ      'F'
326500911022     C                   SETON                                        33
326600940713     C                   MOVE      ARBKSC        V3CKSC
326700940712     C                   MOVE      *BLANKS       VIDKSC
326800040216x   1C                   ELSE
326900911030     C* ASSEGNATO
327000911030     C     ARBCCM        IFNE      0
327100940713     C                   MOVE      ARBCCM        V3CKSC
327200911030     C                   ELSE
327300940713     C                   MOVE      *ZEROS        V3CKSC
327400040217     C                   ENDIF
327500911030     C                   MOVEL     ARBKSC        VIDKSC
327600040217e   1C                   ENDIF
327700070410     c* Numero di telefono del mittente
327800070410     c                   clear                   v3ctelm
327900070410     c                   move      v3cksc        w0040
328000070410     c                   if        w0040<>0000 and w0040<>8888
328100070410     c                             and w0040<>9999
328200070410     c                   eval      i69kin = V3Cksc
328300070410     c                   exsr      CTR_ANAGRA
328400070410     c                   eval      V3ctelm= indtel
328500070410     c                   endif
328600991027     C* 38 ON - ESISTE TB2 ANCHE SENZA TIPO RCD "2" IN FIAR600F
328700920121     C     §3ATB2        COMP      '  '                               3838
328800040218      *
328900040218      *?Controllo se esitono reclami?
329000131111     c                   eval      wARBlnp = ARBlnp
329100131111     c                   eval      wARBnrs = ARBnrs
329200131111     c                   eval      wARBnsp = ARBnsp
329300131111     c                   eval      wARBaas = ARBaas
329400040218     c                   eval      TGDtor  = 'S'
329500040218     c                   eval      TGDogg  = ds_OggSped
329600040218     c     K02TGD02      setll     FITGD000
329700040302     c                   eval      *in64   = (%equal(FITGD02L)   and
329800040303     c                                           (SDSprm =  1
329900040303     c                                        or (SDSprm >  1
330000040303     c                                        and IA1ins <> 'N')))
330100940712     C*
330200940712     C* DECODIFICO  "BOLLA DI RIENTRO"
330300961111     C                   MOVEL     *BLANKS       VIDFBR
330400961111     C                   MOVEL     *BLANKS       VIDFFR
330500961111     C                   SETOFF                                       1073
330600940712     C     ARBFBR        IFEQ      'S'
330700961111     C                   MOVEL     'SI'          VIDFBR
330800961111     C                   SETON                                        10
330900040217     C                   ENDIF
331000961111     C     ARBFBR        IFEQ      'F'
331100961111     C                   MOVEL     'FR'          VIDFFR
331200961111     C                   SETON                                        73
331300040217     C                   ENDIF
331400970130     C*
331500970227     C* FLAG CONSEGNA DDT
331600970130     C                   MOVEL     ARBAAS        KAAS
331700970130     C                   MOVEL     ARBLNP        KLNP
331800970130     C                   MOVEL     ARBNRS        KNRS
331900970130     C                   MOVEL     ARBNSP        KNSP
332000970130     C                   MOVEL     'A'           KTRC
332100970131     C                   CLEAR                   V1DBAM
332200000615     C                   eval      v3csop = ' '
332300060216     C     KAR41         CHAIN     FiAR401L                           31
332400970227     C* C'E' IL RECORD
332500040216if  1C     *IN31         IFEQ      *OFF
332600000615     C                   movel     ar4not        dsbl4a
332700000615     C                   eval      v3csop = §b4aso
332800031027     c                   Move      §b4Abm        wbam              1
332900040216if  2C     WBAM          IFEQ      'S'
333000970131     C     WBAM          OREQ      'C'
333100031027     c     wbam          oreq      'P'
333200031027     c     wbam          oreq      'Q'
333300970227     C                   SETON                                        76
333400970227     C                   MOVEL     'SI DDT'      V1DBAM
333500040216x   2C                   ELSE
333600970227     C                   SETOFF                                       76
333700970227     C                   MOVEL     'NO DDT'      V1DBAM
333800040216e   2C                   ENDIF
333900970227     C**
334000040216sel 2C                   SELECT
334100970227     C     WBAM          WHENEQ    'C'
334200970227     C     WBAM          OREQ      'K'
334300970227     C                   MOVEL     'CLI'         V1DFST
334400970227     C     WBAM          WHENEQ    'J'
334500031027     C     WBAM          oreq      'P'
334600031027     C     WBAM          oreq      'Q'
334700031113     C     WBAM          OREQ      'W'
334800070406     C                   MOVEL     'Arr'         V1DFST
334900970227     C     WBAM          WHENEQ    'S'
335000970227     C     WBAM          OREQ      'Y'
335100070406     C                   MOVEL     'Par'         V1DFST
335200040216x   2C                   OTHER
335300970227     C                   MOVEL     'NO '         V1DFST
335400040216e   2C                   ENDSL
335500970227     C**
335600970227     C* NON  C'E' IL RECORD
335700040216x   1C                   ELSE
335800970227     C                   SETON                                        76
335900970227     C                   MOVEL     'SI DDT'      V1DBAM
336000970227     C                   MOVEL     'PAR'         V1DFST
336100040216e   1C                   ENDIF
336200010510     c* numero O.R.M.
336300010510     C                   MOVEL     'M'           KTRC
336400060216     c     kar41         chain     fiar401l                           31
336500010510     c     *in31         ifeq      *off
336600010510     c                   movel     ar4not        dsbl4m
336700010510     c                   z-add     §b4poe        v3cpoe
336800010510     c                   z-add     §b4nsr        v3cnsr
336900010510     c                   z-add     §b4nor        v3cnor
337000010510     c                   z-add     §b4nrv        v3cnrv
337100010510     c                   else
337200010510     c                   clear                   v3cpoe
337300010510     c                   clear                   v3cnsr
337400010510     c                   clear                   v3cnor
337500010510     c                   clear                   v3cnrv
337600010510     c                   endif
337700130320     c*//* Verifico se ha fiar8
337800130320     C*//                MOVEL     ARBAAS        KAAS
337900130320     C*//                MOVEL     ARBLNP        KLNP
338000130320     C*//                MOVEL     ARBNRS        KNRS
338100130320     C*//                MOVEL     ARBNSP        KNSP
338200130320     c*//  ksped         setll     fiar801l
338300130320     c*//                if        %equal(fiar801l)
338400130320     c*//                eval      v1dAR8='PACKING LIST'
338500130320     C*//                ELSE
338600130320     C*//                CLEAR                   V1DAR8
338700130320     c*//                endif
338800941219     C*
338900941219     C* DECODIFICO  "CONSEGNA A MAGAZZINO"
339000941219     C     ARBFDN        IFEQ      'S'
339100941219     C                   MOVEL     'SI'          V3CFDN
339200941219     C                   ELSE
339300941219     C                   MOVEL     *BLANKS       V3CFDN
339400941219     C                   ENDIF
339500911129     C*
339600920204     C* INCASSO C/A E ASSEGNATO
339700930113     C                   MOVEL     '  '          VIDICC
339800930113     C                   MOVEL     '  '          VIDICA
339900930113     C*
340000930113     C* SE ASSEGNATO O FRANCO CON SECONDA BOLLA
340100930113     C     *IN33         IFEQ      '0'
340200930113     C     *IN33         OREQ      '1'
340300930113     C     *IN38         ANDEQ     '1'
340400930113     C*
340500930330     C     ARBICA        IFNE      ' '
340600941216     C     ARBICA        ANDNE     'R'
340700920204     C                   MOVEL     'SI'          VIDICA
340800040217     C                   ENDIF
340900040217     C                   ENDIF
341000911129     C*
341100911030     C* LINEA PARTENZA DECODIFICA
341200020225     c                   clear                   og143
341300930706     C                   Z-ADD     ARBLNP        NUM3
341400930706     C     NUM3          CHAIN     AZORG01L                           31
341500940719     C  N31              MOVEL     ORGDES        V3DLNP
341600940719     C   31              MOVEL     *BLANKS       V3DLNP
341700020225     c                   eval      og143 = orgde3
341800020225     c                   eval      lnpntw = §ogntw
341900911030     C* LINEA ARRIVO   DECODIFICA
342000991217     c                   clear                   og143
342100070403     C     arblna        CHAIN     AZORG01L                           31
342200940719     C  N31              MOVEL     ORGDES        V3DLNA
342300940719     C   31              MOVEL     *BLANKS       V3DLNA
342400070403     c                   movel     v3dlna        v7dlna
342500991217     C                   movel     orgde3        og143
342600070403     C                   movel     orgde8        og148
342700020304     c                   eval      lnantw = §ogntw
342800070426     c* p.o. gestore di arblna
342900070426     c                   clear                   fnlv55ds
343000070426     c                   movel     arblna        d55lin
343100070426     c                   movel     '6'           d55tpt
343200070426     C                   time                    wrkoggiiso
343300070426     C     *iso          move      wrkoggiiso    d55drf
343400070426     c                   call      'FNLV55R'
343500070426     c                   parm                    fnlv55ds
343600070426     c
343700070426     c                   if        d55tfa<>arblna
343800070426     C     d55tfa        CHAIN     AZORG01L                           31
343900070426     C  n31              movel     orgde8        og148
344000070426     C   31              clear                   og148
344100070426     c                   endif
344200070426     c
344300070403     c
344400070403     c* Abilito visualizzazione nuove funzioni se GEO attiva
344500070426     c* Per p.o. gestione linea di arrivo e p.o. utente
344600070403     c                   if        §oggeot='S' and dutgeot='S'
344700070524     c                   seton                                        1788
344800070406     c                   else
344900070406     c* non visualizzo nemmeno fiarp
345000070406     c                   setoff                                       26
345100070403     c                   endif
345200070524     c
345300070524     c* Vedo se almeno è attivo lo scambio dati con tellus
345400070524     c                   if        not *in88 and §ogtlls<>' ' and dutgeot='S'
345500070524     c                   seton                                        88
345600070524     c                   endif
345700070404     c
345800070404     c* Emetto riga sintetica altre abilitazioni
345900070521     c                   clear                   v5dngd
346000140109     c***                if        *in17
346100070404     c                   clear                   v3cges
346200070404     C                   MOVEL     *BLANKS       V3cgesd
346300070404     c*
346400131120     c                   clear                   fnlrg7ds
346500131120     c                   eval      ilrg7aas=arbaas
346600131120     c                   eval      ilrg7lnp=arblnp
346700131120     c                   eval      ilrg7nrs=arbnrs
346800131120     c                   eval      ilrg7nsp=arbnsp
346900131120     c                   eval      ilrg7lung=58
347000131120     c                   call      'FNLRG7R'
347100131120     c                   parm                    fnlrg7ds
347200131120     c                   movel     olrg7vssin    v3cges
347300140109     c***                endif
347400070410     c
347500070524     c* Reperisco dati della distinta automatica tramite FIDG34R
347600070524     c                   if        *in88
347700070410     c                   clear                   v5ccgi
347800070411     c                   clear                   v5dcgi
347900070410     c                   clear                   v5dtgi
348000070410     c                   clear                   v5dttl
348100070411     c                   clear                   v5clno
348200070427     c                   clear                   v5dlno
348300070427     c                   clear                   v5cargerr
348400070427     c                   clear                   v5dargerr
348500070427     c
348600070427     c                   clear                   fidg34ds
348700070427     c                   eval      id34tip='2'
348800070427     c                   eval      id34aas=arbaas
348900070427     c                   eval      id34lnp=arblnp
349000070427     c                   eval      id34nrs=arbnrs
349100070427     c                   eval      id34nsp=arbnsp
349200070427     c                   call      'FIDG34R'
349300070427     c                   parm                    FIDG34DS
349400070410     c
349500070427     c                   movel     od34cgi       v5ccgi
349600070410    3c                   select
349700070427     c                   when      od34tgi='A'
349800070410     c                   eval      v5dtgi='Automatico'
349900070427     c                   when      od34tgi='M'
350000070410     c                   eval      v5dtgi='Manuale   '
350100070410    3c                   ENDSL
350200070411     c* Decodifica giro
350300070427     c                   eval      v5dcgi=od34cgides
350400070427     c* esito/errore
350500070427     c                   movel     od34err       v5cargerr
350600070427     c                   eval      v5dargerr=od34errdes
350700070427     c* livello normalizzazione
350800070427     c                   movel     od34lno       v5clno
350900070427     c                   eval      v5dlno=od34lnodes
351000070427     c* elaborazione tellus
351100070427     c***                eval      v5dttl=od34ttldes
351200070427     c                   select
351300070427     c                   when      od34ttl='1'
351400070427     c                   eval      v5dttl='SI- CON Giro'
351500070427     c                   when      od34ttl='2'
351600070427     c                   eval      v5dttl='SI-SenzaGiro'
351700070427     c                   endsl
351800070521     c*
351900070521     c* Decodifico se assegnazione automatica o manuale
352000070521     c                   if        arbndc>0 and arbngd<>0
352100070521     c                   eval      v5dngd='MANUALE'
352200070521     c                   endif
352300070521     c                   if        arbndc>0 and arbngd=0
352400070521     c                   eval      v5dngd='AUTOMAT'
352500070521     c                   endif
352600070410     c
352700070410    1c                   endif
352800991217     c
352900060518     c* LNA DPD o lnp dpd: leggo record "I" di ar4
353000000216     c                   setoff                                       08
353100000216     c                   clear                   v3czon
353200000216     c                   clear                   v3crtn
353300040216if  1c                   if        lnantw = 'DPD'
353400991217     c                   seton                                        85
353500991217     C                   MOVEL     ARBAAS        KAAS
353600991217     C                   MOVEL     ARBLNP        KLNP
353700991217     C                   MOVEL     ARBNRS        KNRS
353800991217     C                   MOVEL     ARBNSP        KNSP
353900060518     C                   MOVEL     'I'           KTRC
354000060216     C     KAR41         CHAIN     FiAR401L                           31
354100060518     C  N31              MOVEL     AR4NOT        dsbl4i
354200060518     c   31              clear                   dsbl4i
354300060518     C                   MOVEL(P)  §b4ipn        V3CRPP
354400060518     c                   if        §b4ida<>*blanks or §b4iza>*zeros
354500060518     c                   movel     §b4ida        v3crtn
354600060518     c                   movel     §b4iza        v3czon
354700000216     c                   seton                                        08
354800000216     c                   endif
354900050304     c                   else
355000050304if  1c                   if        lnpntw = 'DPD'
355100050304     C                   MOVEL     ARBAAS        KAAS
355200050304     C                   MOVEL     ARBLNP        KLNP
355300050304     C                   MOVEL     ARBNRS        KNRS
355400050304     C                   MOVEL     ARBNSP        KNSP
355500060518     C                   MOVEL     'I'           KTRC
355600060518     c                   clear                   dsbl4i
355700060216     C     KAR41         CHAIN     FiAR401L
355800060216     c                   if        %found(fiar401l)
355900060518     C**                 MOVEL     AR4NOT        dsbl4f
356000060518     C**                 MOVEL     §b4fnp        V3CRPP
356100060518     C                   MOVEL     AR4NOT        dsbl4i
356200060518     C                   MOVEL     §b4ipn        V3CRPP
356300050304     c                   seton                                        85
356400050304     c                   endif
356500050304     c                   endif
356600040216e   1c                   endif
356700971219     C*
356800971219     C* 16 ON  - SI TRATTA DI BOLLA EXPORT
356900020225     c                   if        lnantw = 'EEX' or lnantw = 'EUP' or
357000060201     c                             lnantw = 'FED' or lnantw='DPD'
357100971219     C                   SETON                                        16
357200971219    1C                   ENDIF
357300940719     C                   MOVEL     V3DLNA        D15LNA
357400920804     C*
357500940719     C                   MOVEL     ARBLNA        V3CLNA
357600940719     C                   MOVEL     ARBRSM        V3CRSM
357700070410     C                   MOVEL     ARBRSM        V4CRSM
357800940713     C                   MOVEL     ARBRSM        V28RSM
357900940719     C                   MOVEL     ARBINM        V3CINM
358000940719     C                   MOVEL     ARBPRM        V3CPRM
358100940719     C                   MOVEL     ARBCAM        V3CCAM
358200940719     C                   MOVEL     ARBLOM        V3CLOM
358300940713     C                   MOVEL     ARBLOM        V25LOM
358400940719     C                   MOVEL     ARBFAP        V3CFAP
358500950421     C                   MOVEL     ARBNZM        V3CNZM
358600131108     c                   if        arbnzm<>*blaNKS
358700131108     C                   MOVEL     arbnzm        v7cprm
358800131108     c                   else
358900131108     C                   MOVEL(P)  arbprm        v7cprm
359000131108     c                   endif
359100131108     c
359200050113     C                   MOVEL     ARBXCO        VIDXCO
359300130221     c                   if        vidxco >*zeros  and no_f22=*blanks
359400110928     c                   seton                                        86
359500110928     c                   endif
359600940719     C                   MOVEL     *BLANKS       V3CPIM
359700940719     C                   MOVEL     *BLANKS       V4CPID
359800061108     c                   clear                   v3cdfm
359900061108     c                   clear                   v4cdfd
360000950411     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
360100060216     C* DA FiAR4 SE RECORD ESISTENTE
360200950411     C                   MOVEL     ARBAAS        KAAS
360300950411     C                   MOVEL     ARBLNP        KLNP
360400950411     C                   MOVEL     ARBNRS        KNRS
360500950411     C                   MOVEL     ARBNSP        KNSP
360600950411     C                   MOVEL     'P'           KTRC
360700060216     C     KAR41         CHAIN     FiAR401L                           31
360800040216if  1C     *IN31         IFEQ      *OFF
360900950411     C                   MOVEL     AR4NOT        V3CPIM
361000950411     C                   MOVE      AR4NOT        V4CPID
361100040216x   1C                   ELSE
361200950411     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI ARBCPI
361300040216if  2C     ARBCPI        IFNE      *BLANKS
361400950411     C  N33              MOVEL     ARBCPI        V4CPID
361500950411     C   33§3ATB2        IFEQ      '  '
361600950411     C                   MOVEL     ARBCPI        V3CPIM
361700950411     C                   ELSE
361800950411     C                   MOVEL     ARBCPI        V4CPID
361900040217     C                   ENDIF
362000040217e   2C                   ENDIF
362100040217e   1C                   ENDIF
362200061108      * valorizzo il codice fiscale del mittente e del destinatario
362300061108      * FIAR4 rcd Q
362400061108     c                   eval      ktrc = 'Q'
362500061108     c     kar41         chain     fiar401l
362600061108if  1c                   if        %found(fiar401l)
362700061108     c                   movel     ar4not        v3cdfm
362800061108     c                   move      ar4not        v4cdfd
362900061108x   1c                   endif
363000980704     C* VISUALIZZO SE C'E' IL RIFERIM PARTNER
363100971219     C                   CLEAR                   V3CEST
363200971219     C                   MOVEL     ARBAAS        KAAS
363300971219     C                   MOVEL     ARBLNP        KLNP
363400971219     C                   MOVEL     ARBNRS        KNRS
363500971219     C                   MOVEL     ARBNSP        KNSP
363600971219     C                   MOVEL     'E'           KTRC
363700060216     C     KAR41         CHAIN     FiAR401L                           31
363800980704     C* VISUALIZZO IN UNA BOLLA ITALIA SE C'E'
363900980704     C* VISUALIZZO SEMPRE IN UNA BOLLA IMPORT/EXPORT
364000971219     C  N31              MOVEL     AR4NOT        V3CRPP
364100980704     C  N31              SETON                                        85
364200980704     C**
364300020225     c                   if        *in31 and
364400020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
364500020225     c                              lnpntw = 'FED') or
364600020225     c                             (lnantw = 'EEX' or lnantw = 'EUP' or
364700020225     c                              lnantw = 'FED')
364800980704     C                   SETON                                        85
364900980704     C                   ENDIF
365000980704     C**
365100940719     C                   MOVEL     ARBRSD        V4CRSD
365200131108     C**********         MOVEL     ARBRSD        V30RSD
365300940713     C                   MOVEL     ARBRSD        V32RSD
365400940719     C                   MOVEL     ARBIND        V4CIND
365500131119     C                   MOVEL     ARBIND        V33IND
365600940719     C                   MOVEL     ARBPRD        V4CPRD
365700940719     C                   MOVEL     ARBCAD        V4CCAD
365800940719     C                   MOVEL     ARBLOD        V4CLOD
365900131119     C*****              MOVEL     ARBLOD        V24LOD
366000940719     C                   MOVEL     ARBFIN        V4CFIN
366100940719     C                   MOVEL     ARBNZD        V4CNZD
366200911022     C                   MOVEL     ARBGC1        VIDGC1
366300911022     C                   MOVEL     ARBGC2        VIDGC2
366400911022     C                   MOVEL     ARBCTR        VIDCTR
366500940719     C                   MOVEL     ARBCTS        V6CCTS
366600131108     c                   if        arbnzd<>*blaNKS
366700131108     C                   MOVEL     arbnzd        v7cprd
366800131108     c                   else
366900131108     C                   MOVEL(P)  arbprd        v7cprd
367000131108     c                   endif
367100940719     C* DECODIFICA CODICE TASSAZIONE
367200940719     C                   MOVEL     'CT'          COD
367300940719     C                   MOVEL     *BLANKS       KEY
367400940719     C                   MOVEL     ARBCTS        KEY
367500940719     C     KTAB1         CHAIN     TABEL00F                           31
367600940719     C                   MOVEL     TBLUNI        V6DCTS
367700911022     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
367800911022     C                   MOVEL     'TR'          COD
367900911022     C                   MOVEL     *BLANKS       KEY
368000911022     C                   MOVEL     VIDCTR        UNOCTR            1
368100911022     C                   MOVEL     UNOCTR        KEY
368200940713     C     KTAB1         CHAIN     TABEL                              31
368300911022     C                   MOVEL     TBLUNI        DEFCTR
368400070404     C*****              MOVEL     ARBFTM        V3CFTM
368500070404     c                   if        arbftm='S'
368600070404     c                   eval      v3dftm='TASSATA CON TARIFFA MITTENTE'
368700070404     c                   endif
368800940711     C* DECODIFICA TIPO SERVIZIO BOLLE
368900090604     c                   clear                   ds5e
369000940711     C                   MOVEL     '5E'          COD
369100940711     C                   MOVEL     *BLANKS       KEY
369200940713     C                   MOVEL     ARBTSP        KEY
369300941024     C     KTAB1         CHAIN     TABEL00F                           31
369400090604     C  N31              MOVEL     TBLUNI        ds5e
369500090604     C  N31              MOVEL     §5ed08        V1DTSP
369600940719     C   31              MOVEL     *BLANKS       V1DTSP
369700131127     c                   if        §5eftc<>' '
369800131127     c                   movel     ri            atrtsp
369900131127     c                   else
370000131127     c                   movel     hi            atrtsp
370100131127     c                   endif
370200940713     C                   MOVEL     ARBVMD        VIDVMD
370300940713     C                   MOVEL     ARBVAD        VIDVAD
370400940713     C                   MOVEL     ARBIAS        VIDIAS
370500940713     C                   MOVEL     ARBVAS        VIDVAS
370600911022     C                   MOVEL     ARBNAS        VIDNAS
370700950214     C                   CLEAR                   VIDFLS
370800950214     C     ARBNCD        IFNE      *ZERO
370900950214     C     ARBFNS        ORNE      *BLANKS
371000940923     C                   MOVEL     ARBFLS        VIDFLS
371100950214     C                   ENDIF
371200950214     C                   Z-ADD     ARBFLS        V3CFLS
371300911022     C                   MOVEL     ARBNCL        VIDNCL
371400070410     C                   MOVEL     ARBPKB        VIDPKb
371500070410     C                   MOVEL     ARBPKF        VIDPKF
371600070410     C                   MOVEL     ARBVLb        VIDVLb
371700070410     C                   MOVEL     ARBFVb        VIDFVb
371800070410     C                   MOVEL     ARBVLF        VIDVLF
371900070410     C                   MOVEL     ARBFVF        VIDFVF
372000930705     C     ARBNCR        IFLT      99998
372100940711     C                   MOVEL     ARBVLC        VIDVLC
372200940711     C                   MOVEL     ARBNCR        VIDNCR
372300921231     C                   ELSE
372400990909     C                   Z-ADD     0             VIDVLC
372500990909     C                   Z-ADD     0             VIDNCR
372600921231     C                   END
372700990909     C     ARBNCP        IFLT      99998
372800990909     C                   MOVEL     ARBPKC        VIDPKC
372900990909     C                   MOVEL     ARBNCP        VIDNCP
373000990909     C                   ELSE
373100990909     C                   Z-ADD     0             VIDPKC
373200990909     C                   Z-ADD     0             VIDNCP
373300990909     C                   END
373400940711     C                   MOVEL     ARBQFT        VIDQFT
373500031119
373600031119      * Referente e Telefono del destinatario
373700031119     c                   Clear                   V4cRef
373800031119     c                   Clear                   V4cTeld
373900091214     c                   clear                   w_cdi
374000031119      * li prendo da Fiar500f record 'GEN'
374100130320     c                   clear                   v1dAR8l
374200130320     c                   clear                   v1dAR8
374300031119     c                   Clear                   dAr5Gen
374400031119     c                   Z-Add     ArbAas        Kaas
374500031119     c                   Z-Add     ArbLNp        Klnp
374600031119     c                   Z-Add     ArbNrs        Knrs
374700031119     c                   Z-Add     ArbNsp        Knsp
374800031119     c                   Eval      Ktrd = 'GEN'
374900031119     c     kAr51         Chain     Fiar501l
375000031119If  1c                   If        %Found(Fiar501l)
375100031119     c                   Movel     Ar5Uni        Dar5Gen
375200031119     c                   Eval      V4cRef = §Ar5Ref
375300031119     c                   Eval      V4cTeld = §Ar5Teld
375400091214     c* memorizzo eventuale codice beneficiario assegno
375500091214     c                   eval      w_cdi=§ar5cdi
375600130320     c* verifico se LDV con packing-list e/o PDF
375700130320sel 2c                   select
375800130320w   2c                   when      §AR5als = 'S'
375900130320     c                   eval      v1dAR8l = 'PACKING LIST PDF'
376000130320     c                   eval      v1dAR8  = 'PACKING PDF '
376100130320w   2c                   when      §AR5alx = 'S'
376200130320     c                   eval      v1dAR8l = 'PACKING LIST'
376300130320     c                   eval      v1dAR8  = 'PACKING LIST'
376400130320e   2c                   endsl
376500031119    1c                   EndIf
376600040301
376700040301      * Se colli di valore visualizzo "V" nell'apposito campo
376800040301      *  con lo specifico attributo di visualizzazione
376900040301if  1c                   if        §AR5bva = *blanks
377000040301     c                   eval      VIDbva  = *blanks
377100040301x   1c                   else
377200040301     c                   eval      VIDbva  = 'V'
377300040301e   1c                   endif
377400110601
377500110610     c* email destinatario
377600110601     c                   setoff                                       60
377700110601     c                   clear                   v4cemw
377800110610     c                   clear                   v4cemd
377900130927     c                   Clear                   dAr5emd
378000110601      * li prendo da Fiar500f record 'EMD'
378100110601     c                   Z-Add     ArbAas        Kaas
378200110601     c                   Z-Add     ArbLNp        Klnp
378300110601     c                   Z-Add     ArbNrs        Knrs
378400110601     c                   Z-Add     ArbNsp        Knsp
378500110601     c                   Eval      Ktrd = 'EMD'
378600130927     c***  kAr51         setll     Fiar501l
378700130927     c     kAr51         chain     Fiar501l
378800130927If  1c                   If        %found(Fiar501l)
378900130927     c                   eval      dar5emd=ar5uni
379000130927     c                   if        §ar5tel<>*blanks or §ar5eml<>*blanks
379100110610     c                   Eval      V4cemd = 'SI'
379200110601     c                   seton                                        60
379300130927     c                   endif
379400110601    1c                   EndIf
379500040301
379600030121      * Bancali
379700030121     c                   Clear                   VidTba
379800030121     c                   Clear                   VidTb2
379900030122     c                   Clear                   VidNba
380000030122     c                   Clear                   VidNb2
380100030703      * Record 'BAN'
380200030703If  0c                   If        %subst(ArbGva:1:1) = 'B'
380300030121     c                   Clear                   dAr5Ban
380400030121     c                   Eval      Ktrd = 'BAN'
380500030121     c     kAr51         Chain     Fiar501l
380600030121If  1c                   If        %Found(Fiar501l)
380700030121     c                   Movel     Ar5Uni        Dar5Ban
380800030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
380900030122      * per le note relative al mancato reso dei bancali
381000030122     c                   If        Not *In77 and
381100030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
381200030122     c                   Eval      *In77 = *On
381300030122     c                   EndIf
381400030121     c                   Eval      VidNba = §Ar5Nba
381500030121     c                   Eval      VidNb2 = §Ar5Nb2
381600030121If  2c                   If        §Ar5Tba <> *Zeros
381700030703     c                   Eval      wtba = §Ar5Tba
381800030703     c                   ExSr      Sr_DecTba
381900030703     c                   Eval      VidTba = wdestba
382000030121    2c                   EndIf
382100030121If  2c                   If        §Ar5Tb2 <> *Zeros
382200030703     c                   Eval      wtba = §Ar5Tb2
382300030703     c                   ExSr      Sr_DecTba
382400030703     c                   Eval      VidTb2 = wdestba
382500030121    2c                   EndIf
382600030121    1c                   EndIf
382700030703    0c                   EndIf
382800030703      * Record 'BNB'  -  Spedizione OVER 25 colli
382900030703If  0c                   If        %subst(ArbGva:1:1) = 'O'
383000030703     c                   Clear                   dAr5Bnb
383100030703     c                   Eval      Ktrd = 'BNB'
383200030703     c     kAr51         Chain     Fiar501l
383300030703If  1c                   If        %Found(Fiar501l)
383400030703     c                   Movel     Ar5Uni        Dar5Bnb
383500030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
383600030703      * per i colli originali in caso di spedizione over 25 colli
383700030703     c  n77              Eval      *In77 = *On
383800030703     c                   Eval      VidNba = §Ar5bNba
383900030703     c                   Eval      VidNb2 = §Ar5bNb2
384000030703If  2c                   If        §Ar5bTba <> *Zeros
384100030703     c                   Eval      wtba = §Ar5bTba
384200030703     c                   ExSr      Sr_DecTba
384300030703     c                   Eval      VidTba = wdestba
384400030703    2c                   EndIf
384500030703If  2c                   If        §Ar5bTb2 <> *Zeros
384600030703     c                   Eval      wtba = §Ar5bTb2
384700030703     c                   ExSr      Sr_DecTba
384800030703     c                   Eval      VidTb2 = wdestba
384900030703    2c                   EndIf
385000030703    1c                   EndIf
385100030703    0c                   EndIf
385200911129     C*
385300131120     C**   ARBACC        IFEQ      'S'
385400131120     C**                 MOVEL     'SI'          V5CACC
385500131120     C**                 ELSE
385600131120     C**                 MOVEL     'NO'          V5CACC
385700131120     C**                 END
385800941215     C     ARBICC        IFNE      ' '
385900941216     C     ARBICC        ANDNE     'R'
386000930218     C                   MOVEL     'SI'          VIDICC
386100930218     C                   END
386200911022     C*
386300911022     C                   MOVEL     ARBRMN        VIDRMN
386400940712     C                   MOVEL     ARBRMA        VIDRMA
386500911022     C                   MOVEL     ARBRMO        VIDRMO
386600911022     C                   MOVEL     ARBNCD        VIDNCD
386700911022     C                   MOVEL     ARBNCA        VIDNCA
386800911022     C                   MOVEL     ARBZNC        VIDZNC
386900051104     c                   select
387000051104     c                   when      arbffd='S'
387100051104     C                   MOVEL     'SI '         VIDFFD
387200051104     c                   when      arbffd='M'
387300051104     C                   MOVEL     'SAB'         VIDFFD
387400051104     C                   other
387500051104     C                   MOVEL     '   '         VIDFFD
387600051104     C                   ENDsl
387700051124     c
387800940719     C                   MOVEL     ARBNFV        V3CNFV
387900051124     c* richiamp pgm per decodifica
388000051124     C                   MOVEL     *BLANKS       VIDTCR
388100051124     c                   clear                   vidhcr
388200051124     c                   clear                   viddcr
388300051124     c                   if        arbdcr>0 or arbhcr>0
388400051124     c                   clear                   fnlv80ds
388500051124     c                   eval      ilv80aas=arbaas
388600051124     c                   eval      ilv80lnp=arblnp
388700051124     c                   eval      ilv80nrs=arbnrs
388800051124     c                   eval      ilv80nsp=arbnsp
388900051124     c                   eval      ilv80mgs=arbmgs
389000051124     c                   eval      ilv80tc1=arbtc1
389100051124     c                   eval      ilv80tc2=arbtc2
389200051124     c                   eval      ilv80dcr=arbdcr
389300051124     c                   eval      ilv80hcr=arbhcr
389400051124     c                   eval      ilv80tcr=arbtcr
389500051124     c                   call      'FNLV80R'
389600051124     c                   parm                    kpjba
389700051124     c                   parm                    fnlv80ds
389800051124     c
389900051124     c                   eval      viddcr=olv80dcr8
390000051124     c                   eval      vidtcr=olv80tcrd
390100911023     C                   MOVEL     ARBHCR        VIDHCR
390200051124     c                   endif
390300051124     c
390400070521     c* Se presente ora consegna, la edito
390500070413     c                   clear                   vidhmc
390600070413     c                   if        arbhmc>0
390700070413     c                   eval      vidhmc=%editw(arbhmc: '0 :  ')
390800070413     C****               MOVE      ARBHMC        VIDHMC
390900070413     c                   endif
391000911023     C                   MOVEL     ARBNTC        VIDNTC
391100911023     C                   MOVEL     ARBTMC        VIDTMC
391200940923     C*
391300940923     C* DECODIFICO IL TIPO MANCATA CONSEGNA
391400940923     C                   MOVEL     *BLANKS       V5DTMC
391500940923     C     ARBTMC        IFNE      *BLANKS
391600940923     C                   MOVEL     '2A'          COD
391700940923     C                   MOVEL     *BLANKS       KEY
391800940923     C                   MOVEL     ARBTMC        KEY
391900940923     C     KTAB1         CHAIN     TABEL00F                           31
392000940923     C  N31              MOVEL     TBLUNI        V5DTMC
392100940923     C                   ENDIF
392200940923     C*
392300911023     C                   MOVEL     ARBMIF        VIDMIF
392400911023     C                   MOVEL     ARBNMI        VIDNMI
392500940923     C*
392600940713     C                   MOVEL     ARBGGA        V4CGGA
392700940923     C                   MOVEL     *BLANKS       V4IGGA
392800940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
392900940923     C     V4CGGA        COMP      *BLANKS                            4242
393000940923     C*
393100940713     C                   MOVEL     ARBGMA        V4CGMA
393200940923     C                   MOVEL     *BLANKS       V4IGMA
393300940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
393400940923     C     V4CGMA        COMP      *BLANKS                            4343
393500940923     C*
393600960208     C                   MOVEL     ARBGVA        V4CGVA
393700960208     C                   MOVE      ' '           V4CGVA
393800940923     C                   MOVEL     *BLANKS       V4IGVA
393900940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
394000940923     C     V4CGVA        COMP      *BLANKS                            4444
394100940923     C*
394200070403     C* FLAG blocco CONSEGNA
394300070403     C                   SELECT
394400070403     c                   WHEN      arbfbc='A'
394500070403     c                   eval      v1dfbc='LascAVVISO'
394600070403     C                   SETON                                        27
394700070403     c                   WHEN      arbfbc='B'
394800070403     c  n17              eval      v1dfbc=' BLOCCATA '
394900070412     c   17              eval      v1dfbc=' RIMPALLO '
395000070412     C                   SETON                                        27
395100070403     c                   WHEN      arbfbc='*'
395200070403     c  n17              eval      v1dfbc=' BLOCCATA '
395300070403     c   17              eval      v1dfbc=' GIACENTE '
395400070403     C                   SETON                                        27
395500070403     C                   ENDSL
395600070403     c
395700070403     C**   ARBFBC        COMP      'A'                                    27
395800070403     C**   ARBFBC        COMP      'B'                                    17
395900070403     C**N17ARBFBC        COMP      '*'                                    17
396000940711     C* TIPO CONSEGNA PARTICOLARE 1
396100940804     C                   MOVEL     *BLANKS       V5DTC1
396200940711     C     ARBTC1        IFNE      ' '
396300911115     C                   MOVEL     '1P'          COD
396400911115     C                   MOVEL     *BLANKS       KEY
396500940711     C                   MOVEL     ARBTC1        KEY
396600940713     C     KTAB1         CHAIN     TABEL                              31
396700940804     C  N31              MOVEL     TBLUNI        V5DTC1
396800940804     C   31              MOVEL     *BLANKS       V5DTC1
396900930113     C                   END
397000940711     C* TIPO CONSEGNA PARTICOLARE 2
397100940804     C                   MOVEL     *BLANKS       V5DTC2
397200940804     C                   MOVEL     *BLANKS       V7DTC2
397300940711     C     ARBTC2        IFNE      ' '
397400940711     C                   MOVEL     '1P'          COD
397500940711     C                   MOVEL     *BLANKS       KEY
397600940711     C                   MOVEL     ARBTC2        KEY
397700940713     C     KTAB1         CHAIN     TABEL                              31
397800940804     C  N31              MOVEL     TBLUNI        V5DTC2
397900940804     C   31              MOVEL     *BLANKS       V5DTC2
398000940804     C  N31              MOVEL     TBLUNI        V7DTC2
398100940804     C   31              MOVEL     *BLANKS       V7DTC2
398200940711     C                   END
398300930113     C* TIPO MANCATA CONSEGNA
398400930113     C                   MOVEL     *BLANKS       DESCCA
398500930113     C     ARBCCA        IFNE      ' '
398600930705     C                   Z-ADD     1             C
398700930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
398800930705     C   31              MOVEL     DCA(C)        ALF12            12
398900930705     C  N31              MOVEL     *BLANKS       ALF12
399000930113     C                   MOVEL     '-'           DESCCA
399100930113     C                   MOVE      ALF12         DESCCA
399200930113     C                   END
399300911022     C* GIRO DATA SPEDIZIONE
399400940712     C                   Z-ADD     ARBDSP        G02INV
399500940712     C                   Z-ADD     *ZEROS        G02DAT
399600940712     C                   MOVEL     '3'           G02ERR
399700940712     C                   CALL      'XSRDA8'
399800940712     C                   PARM                    WLBDAT
399900940719     C                   Z-ADD     G02DAT        V3CDSP
400000070403     c*
400100911022     C* DATA BORDERO'
400200070403     c                   if        arbdbr>0
400300070403     c     *iso          movel     arbdbr        dataeur
400400070403     c                   movel     dataeur       datadmy
400500070404     c                   movel     dataeur       w08dbr            8 0
400600070404     c                   movel     datadmy       v06dbr
400700070403     c                   else
400800070404     c                   clear                   v06dbr
400900070403     c                   endif
401000070404     c
401100070404     c* Prendo da fnblp i dati ritiro merce
401200070403     c     karb2         chain     fnblp01l
401300070403     c                   if        %found(fnblp01l) and blpdrt>0
401400070403     c     *iso          movel     blpdrt        dataeur
401500070403     c                   movel     dataeur       datadmy
401600070404     c                   movel     datadmy       v06drt
401700070404     c                   movel     blpnrt        v7cnrt
401800070406     c                   movel     blpctm        v3cctm
401900070404     c                   if        blpfpp='M'
402000070410     c                   eval      v7dfppr='MAT'
402100070404     c                   else
402200070410     c                   eval      v7dfppr='POM'
402300070404     c                   endif
402400070404     C                   MOVEL     BLPPDR        V7CPDR
402500070404     C                   movel     BlpPdr        kpdpdr
402600070404     C                   move      'A'           kpdtip
402700070404     C     kfapd         chain     fiapd01L
402800070404     c                   if        %found(fiapd01l)
402900070404     C                   MOVEL     APDRSC        V7DPDR
403000070404     c                   else
403100070404     C                   MOVEL     *BLANKS       V7DPDR
403200070404     c                   endif
403300070404     c
403400070404     c
403500070403     c                   else
403600070404     c                   clear                   v06drt
403700070404     c                   clear                   v7cnrt
403800070406     c                   clear                   v3cctm
403900070504     c                   clear                   v7dfppr
404000070404     c                   clear                   v7dpdr
404100070404     c                   clear                   v7cpdr
404200070403     c                   endif
404300070403     c
404400961112     C* SOLO PER LE BOLLE EXPORT VISUALIZZO LA DATA STAMPA MANIFEST
404500070404     C                   CLEAR                   V06DT3
404600070413     c                   if        *in16 and arbdt3>0  and arbdt3<>99999999
404700070404     c     *iso          movel     arbdt3        dataeur
404800070404     c                   movel     dataeur       datadmy
404900070404     c                   movel     datadmy       v06dt3
405000961112     C                   ENDIF
405100940713     C* DATA PARTENZA PRIMO COLLO
405200070404     c                   if        arbdpc>0
405300070404     c     *iso          movel     arbdpc        dataeur
405400070404     c                   movel     dataeur       datadmy
405500070404     c                   movel     datadmy       v06dpc
405600070404     c                   movel     dataeur       w08dpc
405700070404     c                   else
405800070404     c                   clear                   v06dpc
405900070404     c                   clear                   w08dpc            8 0
406000070404     c                   endif
406100940713     C* DATA PARTENZA ULTIMO COLLO
406200070403     C**                 Z-ADD     ARBDUC        G02INV
406300070403     C**                 Z-ADD     *ZEROS        G02DAT
406400070403     C**                 MOVEL     '3'           G02ERR
406500070403     C**                 CALL      'XSRDA8'
406600070403     C**                 PARM                    WLBDAT
406700070403     C**                 Z-ADD     G02DAT        V3CDUC
406800070403     c                   if        arbduc>0
406900070404     c     *iso          movel     arbduc        dataeur
407000070403     c                   movel     dataeur       datadmy
407100070404     c                   movel     datadmy       v06duc
407200070404     c                   movel     dataeur       w08duc
407300070403     c                   else
407400070404     c                   clear                   v06duc
407500070404     c                   clear                   w08duc            8 0
407600070403     c                   endif
407700911022     C* DATA ENTRATA AL TRANSITO
407800070404     c                   if        arbdet>0
407900070404     c     *iso          movel     arbdet        dataeur
408000070404     c                   movel     dataeur       datadmy
408100070404     c                   movel     datadmy       v06det
408200070404     c                   else
408300070404     c                   clear                   v06det
408400070404     c                   endif
408500911022     C* DATA USCITA DAL TRANSITO
408600070404     c                   if        arbdut>0
408700070404     c     *iso          movel     arbdut        dataeur
408800070404     c                   movel     dataeur       datadmy
408900070404     c                   movel     datadmy       v06dut
409000070404     c                   else
409100070404     c                   clear                   v06dut
409200070404     c                   endif
409300930113     C* DATA ARRIVO MERCE
409400070403     c                   if        arbdam>0
409500070403     c     *iso          movel     arbdam        dataeur
409600070403     c                   movel     dataeur       datadmy
409700070404     c                   movel     datadmy       v06dam
409800070403     c                   else
409900070404     c                   clear                   v06dam
410000070403     c                   endif
410100131121
410200930504     C* DATA CONSEGNA MERCE REALE
410300070404     c                   if        arbdcm>0
410400070413     C                   SETON                                        69
410500070404     c     *iso          movel     arbdcm        dataeur
410600070404     c                   movel     dataeur       datadmy
410700070404     c                   movel     dataeur       v08dcm
410800070404     c                   movel     datadmy       v06dcm
410900070404     c                   else
411000070404     c                   clear                   v06dcm
411100070404     c                   clear                   v08dcm
411200070404     c                   endif
411300131121
411400930505     C* DATA CONSEGNA PREVISTA ESTERNA/ORE DI RITARDO
411500070404     C                   Z-ADD     0             v08DCE
411600140113     C                   Z-ADD     0             amgDCE            8 0
411700070404     C                   Z-ADD     0             v06dce
411800940711     C                   Z-ADD     0             VIDNRC
411900131121     C*******            SETON                                        68
412000131121     C                   MOVEL     ' '           D98TLA
412100131121     C                   MOVEL     'A'           D98TBO
412200131121     C                   MOVEL     ARBAAS        D98AAS
412300131121     C                   MOVEL     ARBLNP        D98LNP
412400131121     C                   MOVEL     ARBNRS        D98NRS
412500131121     C                   MOVEL     ARBNSP        D98NSP
412600131121     C*
412700131121     C                   CALL      'TNSD99R'
412800131121     C                   PARM                    TNSD99DS
412900140110     c
413000140110     c* Mi salvo data teorica consegna
413100140110     c                   eval      s_arbdei=d98dei
413200140110     c                   eval      s_arboei=d98oei
413300150728     c
413400150728     c* memorizzo ore trazione è ore consegna cliente da passare al trulorsr
413500150728     c                   eval      s_arbttc=d98ttc
413600150728     c                   eval      s_arbtcc=d98tcc
413700151019     c                   eval      s_arbflp=d98flp
413800131121     C*
413900140110    1C     ARBDCE        IFEQ      0
414000131121    2c                   if        d98dce>0
414100140113     c                   eval      amgdce=d98dce
414200131121     c     *iso          movel     d98dce        dataeur
414300131121     c                   movel     dataeur       datadmy
414400131121     c                   movel     dataeur       v08dce
414500131121     c                   movel     datadmy       v06dce
414600131121   x2c                   else
414700131121     c                   clear                   v08dce
414800131121     c                   clear                   v06dce
414900131121    2c                   endif
415000131121     c
415100131121     C                   Z-ADD     D98NRC        VIDNRC
415200131121     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
415300131121     C*   CONSEGNA PREVISTA
415400131121     c  n69              eval      vidhmc=%editw(d98hce: '0 :  ')
415500131121     C*
415600131121     C* SE COMODO = 1  E' STATA FATTA CALL PGM
415700131121     C                   MOVEL     '1'           PA098
415800131121   x1C                   ELSE
415900140113     c                   eval      amgdce=arbdce
416000070404     c     *iso          movel     arbdce        dataeur
416100070404     c                   movel     dataeur       datadmy
416200070404     c                   movel     dataeur       v08dce
416300070404     c                   movel     datadmy       v06dce
416400070404     c
416500940711     C                   Z-ADD     ARBNRC        VIDNRC
416600131121    1C                   END
416700131121     c
416800911023     C* DATA MANDATO INTROITO
416900940712     C                   Z-ADD     ARBDMI        G02INV
417000940712     C                   Z-ADD     *ZEROS        G02DAT
417100940712     C                   MOVEL     '3'           G02ERR
417200940712     C                   CALL      'XSRDA8'
417300940712     C                   PARM                    WLBDAT
417400940712     C                   Z-ADD     G02DAT        VIDDMI
417500930113     C* DATA PRIMA CONSEGNA PARZIALE
417600930113     C                   Z-ADD     0             VIDDCP
417700930113     C     ARBDCP        IFGT      0
417800940712     C                   Z-ADD     ARBDCP        G02INV
417900940712     C                   Z-ADD     *ZEROS        G02DAT
418000940712     C                   MOVEL     '3'           G02ERR
418100940712     C                   CALL      'XSRDA8'
418200940712     C                   PARM                    WLBDAT
418300940712     C                   Z-ADD     G02DAT        VIDDCP
418400930113     C                   END
418500911023     C* DATA DISTINTA
418600940712     C                   Z-ADD     ARBDDC        G02INV
418700940712     C                   Z-ADD     *ZEROS        G02DAT
418800940712     C                   MOVEL     '3'           G02ERR
418900940712     C                   CALL      'XSRDA8'
419000940712     C                   PARM                    WLBDAT
419100940712     C                   Z-ADD     G02DAT        VIDDDC
419200911022     C* CODICE PADRONCINO CONSEGNA
419300940712     C                   MOVEL     ARBPDC        VIDPDC
419400940712     C                   MOVEL     *BLANKS       DESPDC
419500940712     C                   MOVEL     *BLANKS       DESPD1
419600940712     C     ARBPDC        IFGT      0
419700021203     C                   movel     arbpdc        kpdpdr
419800021203     C                   move      'A'           kpdtip
419900021203     C     kfapd         chain     fiapd01L                           31
420000940714     C  N31              MOVEL     APDRSC        DESPDC
420100940714     C  N31              MOVEL     APDRSC        DESPD1
420200930705     C                   END
420300950324     C* PRESTAZIONE PADRONCINO MATTINO O POMERIGGIO
420400950324     C                   MOVE      *BLANKS       FVVFPP
420500131120     C***                MOVE      *BLANKS       V5DFPP
420600070404     C                   MOVE      *BLANKS       V7DFPP
420700950327     C                   Z-ADD     ARBNDC        WNDC
420800020906     c                   Move      Arbifp        Wfgs
420900020906     c     Kfvv1         Chain     Fnfvv01l                           31
421000950324     C                   SELECT
421100991027     C                   WHEN      *IN31 = *on
421200991027     C                   WHEN      FVVFPP = 'M'
421300131120     C**                 eval      V5DFPP = 'MATTINO'
421400070504     C                   eval      V7DFPP = 'Mat'
421500991027     C                   WHEN      FVVFPP = 'P'
421600131120     C**                 eval      V5DFPP = 'POMERIGGIO'
421700070504     C                   eval      V7DFPP = 'Pom'
421800950324     C                   ENDSL
421900131119     c* controllo se ha PDA
422000131119     C                   Z-ADD     ARBNDC        kNDC6
422100131120     c                   clear                   vidpda
422200131120     c     Kdst          Chain     Fidst01l
422300131120     c                   if        %found(fidst01l)
422400131119     C                   movel     dstflr        DDStflr
422500131120     c                   if        (§dsttstpda= 'C' or  §dsttstpda= 'E') or
422600131120     c                             (dstpda<>'C' and  dstpda<>'E')
422700131120     c                   eval      vidpda='NO PDA'
422800131120     c                   movel     ri            ATRpda
422900131121     c                   else
423000131121     c                   eval      vidpda='SI PDA'
423100131121     c                   movel     ri            ATRpda
423200131119     c                   endif
423300131119     c                   endif
423400131108     c
423500131108     c* Se bolla in distinta, verifico quanti colli
423600131108     c                   clear                   v7dncl_co
423700131115     c                   clear                   v7dncl_de
423800131108     c                   clear                   wcol_D
423900131108     c                   clear                   wcol_I
424000131108    1c                   if        arbndc>0 and arbddc>0 and arbfbc=' '
424100131108     c* se ritorno all'incasso lo scrivo
424200131108     c
424300131108    2c                   if        arbdcm>0  and (arbicc='R' or arbicc=' ' or
424400131108     c                                  arbica=' ' or arbica='R')
424500131115     c                   eval      v7dncl_de='     IN consegna :'
424600131108     c                   eval      v7dncl_co='PER INCASSO'
424700131108    2c                   endif
424800131108     c
424900131108     c* Se colli da consegnare
425000131108    2c                   if        arbdcm=0
425100131108     C                   Z-ADD     ARBNDC        knfv
425200131108     c                   Move      Arbifp        Wfgs
425300131108     c
425400131108     c     karb2         setll     fnart01l
425500131108     c     karb2         reade     fnart01l
425600131108     c
425700131108    3c                   dow       not %eof(fnart01l)
425800131108     c                   if        artdcm=0
425900131108     c                   add       1             wcol_d
426000131108     c     kbrv          chain     fnbrv01l
426100131108     c                   if        %found(fnbrv01l)
426200131108     c                   add       1             wcol_I
426300131108     c                   endif
426400131108     c                   endif
426500131108     c
426600131108     c
426700131108     c     karb2         reade     fnart01l
426800131108    3c                   enddo
426900131108
427000131108    3c                   if        wcol_d>0
427100131115     c                   eval      v7dncl_de='Colli in consegna:'
427200131108    4c                   if        wcol_i=0
427300131108     c                   eval      v7dncl_co='0/'+%trim(%editc(wcol_d:'Z'))
427400131108     c                   else
427500131108     c                   eval      v7dncl_co=%trim(%editc(wcol_I:'Z')) +'/' +
427600131108     c                                       %trim(%editc(wcol_d:'Z'))
427700131108    4c                   endif
427800131108    3c                   endif
427900131108    c
428000131108    2c                   endif
428100131108    1c                   endif
428200131108     c
428300131108     c* Visualizzo ultimo evento
428400131108     c                   clear                   v7duevb
428500131114     c* prima vedo se c'e' da PDA
428600131114     c                   move      arbndc        pctndc
428700140224     c                   move      arbifp        pctfgs
428800131114     c                   move      'CET'         ttrd
428900131114     c     kcet          setgt     fipct02l
429000131114     c     kcet          readpe    fipct02l
429100140310     c                   if        not %eof(fipct02l)
429200131114     c                   movel     pctdati       fipctcetds
429300131114     c* consegnata
429400131114     c                   if        §pctcmc = ' '
429500131114     c                   MOVE      '  C'         evbcev                         *causale evento
429600131114     c                   else
429700131114     c* mancata consegna
429800131114     c                   movel     §PCTcmc       evbcev
429900131114     c                   endif
430000131114     c                   z-add     §pctdata      evbdev
430100131114     c                   movel     §pctora       evbhev
430200131114
430300131114     c                   exsr      Decodi_EVB
430400131114
430500131114     c                   else
430600131114     c*
430700131108     C                   Z-ADD     ARBAAS        KAAS$
430800131108     C                   Z-ADD     ARBLNP        KLNP
430900131108     C                   Z-ADD     ARBNRS        KNRS
431000131108     C                   Z-ADD     ARBNSP        KNSP
431100131108     C     KEVB1         SETgt     FNEVB01L
431200131108     C     KEVB1         readpe    FNEVB01L
431300131108     c
431400131114    1c                   if        not %eof(fnevb01l)
431500131114     c                   exsr      Decodi_EVB
431600131114    1c                   endif
431700131114    1c                   endif
431800911022     C*
431900911022     C* SE ESISTE AGGANCIO 2° BOLLA
432000991026     C                   Z-ADD     ARBAAS        KAAS
432100991026     C                   Z-ADD     ARBLNP        KLNP
432200991026     C                   Z-ADD     ARBNRS        KNRS
432300991026     C                   Z-ADD     ARBNSP        KNSP
432400991026     C                   MOVEL     '2'           KTRC
432500991027     C     KAR61         CHAIN     FIAR6000                           31
432600911023     C*
432700040216if  1C                   IF          %FOUND
432800991027     C                   eval      *in22 = *on
432900991027     C                   MOVEL     AR6CEI        V9CCEI
433000991027     C                   MOVEL     AR6NFT        V9CNFT
433100991027     C                   MOVEL     AR6FIV        V9CFIV
433200991027      *
433300991027     C                   eval      V9CDIV = AR6div
433400991027     C                   eval      WDIV = AR6div
433500991027     C                   exsr      CTR_VALUTA
433600991027      *
433700001128     C                   clear                   v9dfim
433800001128     C                   clear                   v9dfi2
433900991027     c                   if        AR6fim = 'P'
434000991027     c                   eval      V9Dfim = 'PARTENZA'
434100040126     c                   eval      V9Dfi2 = 'Immesse in'
434200991027     c                   endif
434300991027      * Varia G
434400040130     c                   clear                   W0133
434500040130     c                   clear                   v9csv1
434600040130     c                   clear                   v9cva1
434700040130     c                   clear                   v9csv2
434800040130     c                   clear                   v9cva2
434900040130     c                   clear                   v9csv3
435000040130     c                   clear                   v9cva3
435100040130     c                   clear                   v9csv4
435200040130     c                   clear                   v9cva4
435300041130     c                   clear                   v9csv5
435400041130     c                   clear                   v9cva5
435500041130     c                   clear                   v9csv6
435600041130     c                   clear                   v9cva6
435700040130     c                   clear                   v9civa
435800040130
435900040122      * Carico le varie
436000040122     c                   Movel     Ar6Sv1        wsvn
436100040122     c                   Z-add     Ar6Va1        wvan
436200040122     c                   ExSr      Car_Varie1
436300040122
436400040122     c                   Movel     Ar6Sv2        wsvn
436500040122     c                   Z-add     Ar6Va2        wvan
436600040122     c                   ExSr      Car_Varie1
436700040122
436800040122     c                   Movel     Ar6Sv3        wsvn
436900040122     c                   Z-add     Ar6Va3        wvan
437000040122     c                   ExSr      Car_Varie1
437100040122
437200040122     c     Kar61         Setll     Fiar7000
437300040216do  2c                   Do        *Hival
437400040122     c     Kar61         Reade     Fiar7000
437500040122     c                   If        %Eof(Fiar701l)
437600040122     c                   Leave
437700040122     c                   EndIf
437800040122     c                   Movel     Ar7Svn        wsvn
437900040122     c                   Z-add     Ar7Van        wvan
438000040216     c                   ExSr      Car_Varie1
438100040216e   2c                   EndDo
438200040122
438300991027      * TOTALE FATTURA
438400991027     c                   clear                   W0133
438500991027     c                   IF          AR6ift > *zeros
438600991027     c                   z-add     AR6ift        W0133
438700991027     c                   eval      Wtstdec = §cvfdc
438800991027     c                   exsr      EDTimporti
438900991027     c                   movel     IMPeditato    V9cift
439000001128     C                   else
439100001128     C                   clear                   v9cift
439200991027     c                   endif
439300991027      *
439400991027     c                   If         AR6dft > *zeros
439500991027     C     *iso          movel     AR6DFT        DATA_EUR
439600991027     C     *eur          movel     data_eur      W0080
439700991027     C                   eval      V9cdft = %EDITC(W0080:'Y')
439800001128     C                   else
439900001128     C                   clear                   v9cdft
440000991027     c                   Endif
440100991027      *
440200991027     c                   eval      V9Cksc = AR6ksc
440300991027     c                   eval      V9Cctr = AR6ctr
440400991027     c                   move      v9cksc        W0040
440500991027     c                   If        W0040 = 9999
440600991027     c                   eval      V9Dksc = arbrsd
440700991027     c                   Else
440800991027     c                   eval      i69kac = V9Cksc
440900991027     c                   exsr      CTR_ANAGRA
441000991027     c                   eval      V9Dksc = acorag
441100991027     c                   Endif
441200991027      *
441300991027     C                   eval      COD = 'TR'
441400991027     C                   movel     V9CCTR        UNOCTR            1
441500991027     C                   eval      KEY = unoctr
441600991027     C     KTAB1         CHAIN     TABEL00F                           31
441700991027     C                   movel     TBLUNI        V9DCTR
441800991027      *
441900930114     C* ESENZIONE IVA
442000001128     C                   clear                   v9dcei
442100991111     C     AR6CEI        IFNE      *blanks
442200940721     C                   MOVEL     'EI'          COD
442300991111     C                   MOVEL(P)  AR6CEI        KEY
442400940719     C     KTAB1         CHAIN     TABEL00F                           31
442500991027     C  N31              MOVEL     TBLUNI        V9DCEI
442600940719     C                   ENDIF
442700991027      *
442800040216e   1C                   ENDIF
442900930525     C*
443000991027     C* PER I DATI SULLA TASSAZIONE 1a BOLLA, DEVO CERCARE SU FIAR600F
443100940905     C                   SETOFF                                       24
443200110705     c                   clear                   atrbl4a
443300110705     c
443400110705     c                   clear                   v6car4ksc
443500110705     c                   clear                   v6dar4ksc
443600110705     c                   clear                   v6car4ctr
443700111021     c                   clear                   VIDimv
443800111021     c                   clear                   VIDpor
443900111021     c                   clear                   VIDift
444000111021     c                   clear                   VIDdiv
444100111021     c                   clear                   VIDfim
444200111021     C                   clear                   VIDCEI
444300111021     C                   clear                   VIDNFT
444400111021     C                   clear                   VIDFIV
444500111021     C                   clear                   DESCEI
444600111021     C                   clear                   viddft
444700111021     C                   clear                   vidIVA
444800111021      *
444900111021      * CERCO LE VARIE: LE PRIME 3 SU FIAR600F, EVENTUALI  ALTRE SU FIAR700F
445000111021     C                   eval      *in35 = *on
445100131107     C                   WRITE     LRI6C06
445200111021     C                   eval      *in35 = *off
445300111021      *
445400111021     c                   clear                   NR6
445500111021     c                   clear                   VIDiva
445600110705     c
445700940712     C                   Z-ADD     ARBAAS        KAAS
445800940712     C                   Z-ADD     ARBLNP        KLNP
445900940712     C                   Z-ADD     ARBNRS        KNRS
446000940712     C                   Z-ADD     ARBNSP        KNSP
446100110705     C                   MOVEL     'A'           KTRC
446200110705     C     KAR41         CHAIN     FiAR401L
446300110705if  1C                   IF        not %FOUND
446400110705     c                   eval      atrbl4a=nodi
446500110705     c                   else
446600110705     c                   movel     ar4not        dsbl4a
446700110705     c                   if        §b4aks>0
446800110705     C                   eval      *in24 = *on
446900110705     c                   eval      v6car4ksc=§b4aks
447000110705     c                   eval      i69kac = v6car4ksc
447100110705     c                   exsr      CTR_ANAGRA
447200110705     c                   eval      v6dar4ksc = acorag
447300110705     c                   if        §b4act<>*blanks
447400110705     c                   movel     §b4act        v6car4ctr
447500110705     c                   endif
447600110705     c* non c'e' cod cliente x tassaz.
447700110705     c                   else
447800110705     c                   eval      atrbl4a=nodi
447900110705     c                   endif
448000110705     c                   endif
448100110705     c
448200991026     C                   MOVEL     '1'           KTRC
448300991027     C     KAR61         CHAIN     FIAR6000
448400110705     c
448500110705     c                   if        %found(fiar601l)
448600110705     c
448700110705      * CLIENTE E TARIFFA FATTURAZIONE
448800110705     c                   IF        *IN33 = *ON
448900110705     c                   eval      v6cksc = arbksc
449000110705     c                   eval      v6cctr = arbctr
449100110705     c                   eval      v6dksc = arbrsm
449200110705     c                   ELSE
449300110705     c                   eval      v6cksc = AR6ksc
449400110705     c                   eval      v6cctr = AR6ctr
449500110705     c                   move      v6cksc        W0040
449600110705     c                   if        W0040 = 9999
449700110705     c                   eval      v6dksc = arbrsd
449800110705     c                   else
449900110705     c                   eval      i69kac = v6cksc
450000110705     c                   exsr      CTR_ANAGRA
450100110705     c                   eval      v6dksc = acorag
450200110705     c                   endif
450300110705     c                   ENDIF
450400110705      *   decodifica tariffa
450500110705     C                   eval      COD = 'TR'
450600110705     C                   MOVEL     V6CCTR        UNOCTR            1
450700110705     C                   MOVEL(P)  UNOCTR        KEY
450800110705     C     KTAB1         CHAIN     TABEL00F                           31
450900110705     C                   MOVEL     TBLUNI        V6DCTR
451000110705     c
451100110705     c                   endif
451200991027      *
451300040216if  1C                   IF        %FOUND  and
451400991027     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
451500991027     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
451600991027     C                              AR6sv3 <> *blanks)
451700991027      *
451800991027      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SE PRESENTE ALMENO UNO DEI CAMPI
451900991027     C                   EVAL      *IN24 = *ON
452000991027      *
452100991027     C                   eval      VIDDIV = AR6div
452200991027     C                   eval      WDIV = AR6div
452300991027     C                   exsr      CTR_VALUTA
452400991027      *
452500991027     c                   IF          ar6por > *zeros
452600991027     c                   z-add     AR6por        W0133
452700991027     c                   eval      Wtstdec = §cvfdc
452800991027     c                   exsr      EDTimporti
452900991027     c                   movel     IMPeditato    VIDpor
453000991128     c                   else
453100991128     c                   clear                   VIDpor
453200991027     c                   ENDIF
453300991027      *
453400991027     c                   IF          ar6imv > *zeros
453500991027     c                   z-add     AR6imv        W0133
453600991027     c                   eval      Wtstdec = §cvfdc
453700991027     c                   exsr      EDTimporti
453800991027     c                   movel     IMPeditato    VIDimv
453900991128     c                   else
454000991128     c                   clear                   VIDimv
454100991027     c                   ENDIF
454200991027      *
454300991027     c                   IF          ar6ift > *zeros
454400991027     c                   z-add     AR6ift        W0133
454500991027     c                   eval      Wtstdec = §cvfdc
454600991027     c                   exsr      EDTimporti
454700991027     c                   movel     IMPeditato    VIDift
454800991128     c                   else
454900991128     c                   clear                   VIDift
455000991027     c                   ENDIF
455100991027      *
455200991027     c                   IF        AR6fim = 'P'
455300991027     c                   eval      VIDfim = 'PARTENZA'
455400991128     c                   else
455500991128     c                   clear                   VIDfim
455600991027    2C                   ENDIF
455700991027      *
455800991027     C                   MOVEL     AR6CEI        VIDCEI
455900991027     C                   MOVEL     AR6NFT        VIDNFT
456000991027     C                   MOVEL     AR6FIV        VIDFIV
456100991027      *
456200991027     C                   IF        AR6CEI <>  *BLANKS
456300940808     C                   MOVEL     'EI'          COD
456400991027     C                   MOVEL(P)  AR6CEI        KEY
456500940808     C     KTAB1         CHAIN     TABEL00F                           31
456600940808     C  N31              MOVEL     TBLUNI        DESCEI
456700991128     c                   else
456800991128     c                   clear                   DESCEI
456900940808     C                   ENDIF
457000991027      *
457100991027      * DATA FATTURA DA GIRARE
457200991027     C                   IF         ar6dft > *zeros
457300991027     C     *iso          movel     AR6DFT        DATA_EUR
457400991027     C     *eur          movel     data_eur      W0080
457500991027     C                   eval      VIDdft = %EDITC(W0080:'Y')
457600991128     c                   else
457700991128     c                   clear                   VIDdft
457800991027     C                   ENDIF
457900991027      *
458000991027      *
458100991027     C                   MOVEL     ar6SV1        WSVN
458200991027     C                   MOVEL     ar6VA1        WVAN
458300991027     c                   exsr      car_varie
458400991027      *
458500991027     C                   MOVEL     ar6SV2        WSVN
458600991027     C                   MOVEL     ar6VA2        WVAN
458700991027     c                   exsr      car_varie
458800991027      *
458900991027     C                   MOVEL     ar6SV3        WSVN
459000991027     C                   MOVEL     ar6VA3        WVAN
459100991027     c                   exsr      car_varie
459200991027      *
459300991027      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
459400040216if  2C                   if          ar6SV3 <> *blanks
459500991027     C     KAR61         SETLL     FIAR7000
459600991027     C     KAR61         READE     FIAR7000
459700040216do  3C                   DOW       %EOF = *OFF
459800991027     C                   MOVEL     AR7SVN        WSVN
459900991027     C                   MOVEL     AR7VAN        WVAN
460000991027     c                   exsr      car_varie
460100991027     C     KAR61         READE     FIAR7000
460200040216e   3C                   ENDDO
460300040216e   2C                   endif
460400110705e   1C                   ENDIF
460500991027      *
460600991027      * Se non ho caricato record ne carico uno vuoto
460700110705     c                   IF        NR6 = *ZEROS  and *in24
460800991027     C                   clear                   V6CDE1
460900991027     C                   clear                   V6CVA1
461000991027     C                   clear                   V6CSV1
461100991027     C                   add       1             NR6
461200131107     C                   WRITE     LRI6S06
461300991027     c                   ENDIF
461400991027      *
461500991027      *
461600941206     C* 70 ON - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
461700941206     C     *IN33         IFEQ      '0'
461800941206     C     AR6NFT        ANDGT     0
461900941206     C     *IN22         OREQ      '1'
462000991111     C     AR6NFT        ANDGT     0
462100941206     C                   SETON                                        70
462200941206     C                   END
462300940712     C*
462400940719     C* PRELEVO LE NOTE DAL FILE APPOSITO
462500940719     C                   MOVEL     ARBAAS        KAAS
462600940719     C                   MOVEL     ARBLNP        KLNP
462700940719     C                   MOVEL     ARBNRS        KNRS
462800940719     C                   MOVEL     ARBNSP        KNSP
462900940719     C                   MOVEL     '8'           KTRC
463000060216     C     KAR41         CHAIN     FiAR401L                           31
463100940719     C  N31              MOVEL     AR4NOT        V4CNO1
463200940802     C   31              MOVEL     *BLANKS       V4CNO1
463300940719     C                   MOVEL     '9'           KTRC
463400060216     C     KAR41         CHAIN     FiAR401L                           31
463500940719     C  N31              MOVEL     AR4NOT        V4CNO2
463600940802     C   31              MOVEL     *BLANKS       V4CNO2
463700940719     C*
463800941110     C* ESTENSIONE RAGIONE SOCIALE DESTINATARIO
463900941110     C                   MOVEL     'D'           KTRC
464000060216     C     KAR41         CHAIN     FiAR401L                           31
464100950125     C  N31              MOVEL     AR4NOT        V4CNOT
464200950125     C   31              MOVEL     *BLANKS       V4CNOT
464300971015     C*
464400971015     C* SOLO PER EXPORT DATA DI STAMPA SU BORDERO' ARRIVI
464500070404     C***  *IN16         IFEQ      *ON
464600070404     C***                MOVEL     'A'           KTRC
464700070404     C***  KAR41         CHAIN     FiAR401L                           31
464800070404     C**N31              MOVE      AR4NOT        WDATA             8
464900070404     C***31              MOVEL     *ZEROS        WDATA
465000070404     C***  WDATA         IFGT      *ZEROS
465100070404     C***                MOVEL     WDATA         G02INV
465200070404     C***                Z-ADD     *ZEROS        G02DAT
465300070404     C***                MOVEL     '3'           G02ERR
465400070404     C***                CALL      'XSRDA8'
465500070404     C***                PARM                    WLBDAT
465600070404     C***                Z-ADD     G02DAT        V3CDST
465700070404     C***                ENDIF
465800070404     C***                ENDIF
465900940720     C*
466000051114     C* IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU FiAR900F
466100940720     C* CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
466200940720     C                   MOVEL     *BLANKS       VIDTIC
466300940720     C                   MOVEL     *ZERO         VIDCAS
466400940720     C                   MOVEL     *BLANKS       VIDVCA
466500940720     C                   MOVEL     *BLANKS       V4CGCA
466600940923     C                   MOVEL     *BLANKS       V4IGCA
466700091214     c                   setoff                                       65
466800940720     C     §3AFCA        IFGT      *ZERO
466900051128     C     Ksped         CHAIN     FiAR901L                           31
467000940720     C  N31              DO
467100940720     C                   MOVEL     AR9TIC        VIDTIC
467200940720     C                   MOVEL     AR9CAS        VIDCAS
467300940720     C                   MOVEL     AR9VCA        VIDVCA
467400940720     C                   MOVEL     AR9GCA        V4CGCA
467500091214     c                   seton                                        65
467600940720     C                   ENDDO
467700940720     C                   ENDIF
467800941215     C* ESISTE CONTRASSEGNO: VEDO SE ABILITATO
467900941215     C     VIDCAS        IFNE      0
468000941215     C                   SETON                                        29
468100941215     C                   END
468200940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
468300940923     C     V4CGCA        COMP      *BLANKS                            4141
468400940720     C*
468500940720     C* RIEMPO ANCHE ALCUNI CAMPI PER LE DS DI PASSAGGIO PARAMETRI
468600940720     C                   MOVEL     V3CDSP        PA1DSP
468700940720     C                   MOVEL     ARBRMN        PA1RMN
468800940720     C                   MOVEL     ARBLNA        PA1LNA
468900940802     C                   MOVEL     V3DLNA        PA1LN1
469000940720     C                   MOVEL     ARBRSD        PA1RSD
469100940906     C*-------------------------------------------------------------------------
469200991029      * GESTIONE INCASSO PARZIALE se assegnato o presente estensione bolla
469300991102     C                   clear                   v5cdiv
469400991102     c                   clear                   Wariimp
469500991102     c                   clear                   Wariipp
469600991102     c                   clear                   VIDimp
469700991102     c                   clear                   VIDipp
469800991102     C                   MOVEL     ARBFIP        VIDFIP
469900991102      *
470000991029     c  N33              eval      V5Cdiv = viddiv
470100991029     c   22              eval      V5Cdiv = v9cdiv
470200991029      *
470300040216if  1c                   if        V5CDIV <> *blanks
470400991029      *
470500991029     C                   eval      WDIV = V5Cdiv
470600991029     C                   exsr      CTR_VALUTA
470700991029      *
470800991029     C                   MOVEL     ARBAAS        KAAS
470900991029     C                   MOVEL     ARBLNP        KLNP
471000991029     C                   MOVEL     ARBNRS        KNRS
471100991029     C                   MOVEL     ARBNSP        KNSP
471200991029     C                   MOVEL     'A'           ktrc
471300991029      *
471400991029     c     kar61         setll     fiari000
471500991029     c     kar61         reade     fiari000
471600991029      *
471700040216do  2c                   DOW       not %EOF
471800040216if  3c                   if        v5cdiv = aridiv
471900991029     c                   add       ARIimp        Wariimp
472000991029     c                   add       ARIipp        Wariipp
472100040216x   3c                   else
472200991029      *
472300991029     C                   clear                   yeurcods
472400991029     C                   eval      YECDVI = aridiv
472500991029     C                   eval      YECDVO = V5Cdiv
472600991029     C                   eval      YECIMI = ARIimp
472700011022     C                   move      §CVDEC        YECDCO
472800991029     C                   CALL      'YEURCO'
472900991029     C                   PARM                    YEURcods
473000991029    6C                   if        YECESI = *blanks
473100991029     C                   add       YECIMO        WARIimp
473200991029    6C                   endif
473300991029      *
473400991029     C                   clear                   yeurcods
473500991029     C                   eval      YECDVI = aridiv
473600991029     C                   eval      YECDVO = V5Cdiv
473700991029     C                   eval      YECIMI = ARIipp
473800011022     C                   move      §CVDEC        YECDCO
473900991029     C                   CALL      'YEURCO'
474000991029     C                   PARM                    YEURcods
474100991029    6C                   if        YECESI = *blanks
474200991029     C                   add       YECIMO        WARIipp
474300991029    6C                   endif
474400991029      *
474500040216e   3c                   endif
474600991029      *
474700991029     c     kar61         reade     fiari000
474800040216e   2c                   enddo
474900991029      *
475000991029     c                   IF        WARIipp > *zeros
475100991029     c                   z-add     Wariipp       W0133
475200991029     c                   eval      Wtstdec = §cvfdc
475300991029     c                   exsr      EDTimporti
475400991029     c                   movel     IMPeditato    VIDipp
475500991029     c                   ENDIF
475600991029      *
475700991029     c                   IF        WARIimp > *zeros
475800991029     c                   z-add     Wariimp       W0133
475900991029     c                   eval      Wtstdec = §cvfdc
476000991029     c                   exsr      EDTimporti
476100991029     c                   movel     IMPeditato    VIDimp
476200991029     c                   ENDIF
476300991105      *
476400991105     c                   IF        VIDipp = *blanks  and  VIDimp = *blanks
476500991105     c                   clear                   V5CDIV
476600991105     c                   ENDIF
476700991029      *
476800040216e   1C                   endif
476900991029     C*-------------------------------------------------------------------------
477000951220     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
477100951220     C*
477200951220     C* PULISCO IL SFL
477300960926     C                   SETOFF                                       72
477400131107     C                   WRITE     LRI6C05
477500960926     C                   SETON                                        7172
477600951220     C*
477700060216     C* CHIAVE PER ACCEDERE AL FILE FiAR401L
477800951220     C                   MOVEL     ARBAAS        KAAS
477900951220     C                   MOVEL     ARBLNP        KLNP
478000951220     C                   MOVEL     ARBNRS        KNRS
478100951220     C                   MOVEL     ARBNSP        KNSP
478200951220     C*
478300951220     C                   Z-ADD     0             NRR5              4 0
478400951220     C                   MOVEL     '7K'          COD
478500951220     C*
478600060216     C     KAR42         SETLL     FiAR401L
478700060216     C     KAR42         READE     FiAR401L                               36
478800040216do  1C     *IN36         DOWEQ     *OFF
478900951220     C*
479000951220     C                   MOVEL(P)  AR4TRC        KEY
479100951220     C     KTAB1         CHAIN     TABEL                              36
479200951220     C*
479300951220     C     *IN36         IFEQ      *OFF
479400951220     C     TBLFLG        ANDEQ     ' '
479500951220     C                   MOVEL     TBLUNI        DS7K
479600951220     C     §7KVIS        IFEQ      'S'
479700951220     C                   MOVEL     §7KDES        VS5DES
479800951220     C                   MOVEL     AR4NOT        VS5NOT
479900951220     C                   ADD       1             NRR5
480000131107     C                   WRITE     LRI6S05
480100951220     C                   ENDIF
480200951220     C                   ENDIF
480300951220     C*
480400060216     C     KAR42         READE     FiAR401L                               36
480500040216e   1C                   ENDDO
480600951220     C*
480700960926     C* SE NON HO CARICATO NIENTE NON EMETTO IL SFLDSP PER EVITARE CHE
480800960926     C*  IL PGM DIA ERRORE DI SESSIONE O UNITA'
480900951220     C     NRR5          IFEQ      0
481000991105     C                   clear                   VS5DES
481100991105     C                   clear                   VS5NOT
481200991105     C                   ADD       1             NRR5
481300131107     C                   WRITE     LRI6S05
481400951220     C                   ENDIF
481500951220     C*
481600021216      * Riempio il campone in fondo alla videata con i tasti scelti
481700021216     C                   exsr      Ges_TastiFun
481800021217     c                   eval      FD07 = FDxx
481900021217     c                   eval      F24D07 = F24Dxx
482000911022     C                   ENDSR
482100131114      *-------------------------------------------------------------------------
482200131114      *  Decodifica EVENTO
482300131114      *-------------------------------------------------------------------------
482400131114     c     Decodi_evb    BEGSR
482500131114
482600131114     c                   if        evbcev='  C'
482700131114     c                   eval      §2ades='CONSEGNA'
482800131128     c* aggiungo anche firmatario
482900131128     c                   if        §PCTNOMFIR<>*blaNKS
483000131128     C                   EVAL      §2ADES=%TRIM(§2ADES)+' Firma:' +§PCTNOMFIR
483100131128     c                   endif
483200131128     c
483300131114     C                   else
483400131114     C                   MOVEL     '2A'          COD
483500131114     C                   MOVEL(p)  evbcev        KEY
483600131114     C     KTAB1         CHAIN     TABEL00F
483700131114    3c                   if        not %found(tabel00f)
483800131114     c                   clear                   ds2a
483900131114     c                   else
484000131114     c                   eval      ds2a=tbluni
484100131114    3c                   endif
484200131114    3c                   endif
484300131114     c
484400131114     c                   eval      v7duevb=evbcev+'-'+§2ades
484500131114
484600131114    3c                   if        evbdev>0
484700131114     c     *iso          movel     evbdev        dataeur
484800131114     c                   movel     dataeur       datadmy
484900131114     c                   movel     dataeur       w7cdev            8 0
485000131114
485100131114     c                   eval      indx=%checkr(' ':v7duevb)
485200131114    4c                   if        indx>46
485300131114     c                   eval      indx=46
485400131114     c                   else
485500131114     c                   eval      indx=indx+1
485600131114    4c                   endif
485700131114     c                   eval      wcarat=66-indx
485800131114     c                   eval      %subst(v7duevb:indx:wcarat)=' ' +'il ' +
485900131114     c                             %editw(W7cdev: '  /  /    ')
486000131114     c
486100131114    4c                   if        evbhev>0
486200131114     c                   eval      v7duevb=%trim(v7duevb)+' ' +
486300131114     c                                                 %editw(evbhev: '0 :  ')
486400131114    4c                   endif
486500131114     c
486600131114    3c                   endif
486700131114     c                   ENDSR
486800030703      *-------------------------------------------------------------------------
486900030703      *  Decoditifa tipo bancali
487000030703      *-------------------------------------------------------------------------
487100030703     c     Sr_DecTba     BegSr
487200030703     c                   Clear                   Tibs02Ds
487300030703     c                   Clear                   dTba
487400030703     c                   Movel     'C'           T02Mod
487500030703     c                   Movel     Knsif         T02Sif
487600030703     c                   Movel     'TBA'         T02Cod
487700030703     c                   Movel(p)  wtba          T02Ke1
487800030703     c                   Call      'TIBS02R'
487900030703     c                   Parm                    Kpjba
488000030703     c                   Parm                    Tibs02Ds
488100030703If  3c                   If        T02Err = *Blanks
488200030703     c                   Movel     T02Uni        dTba
488300030703    3c                   EndIf
488400030703     c                   Eval      wdestba = §TbaDesc
488500030703     c                   EndSr
488600021216      *-------------------------------------------------------------------------
488700021216      *  Imposto descrizione tasti funzione
488800021216      *-------------------------------------------------------------------------
488900021216     c     Ges_TastiFun  BEGSR
489000021217      *
489100021217      * per emettere i tasti senza condizionamenti ..
489200021216     c                   setoff                                       89
489300021216      *
489400021216      * Conta i caratteri riempiti dalle RigaTf1
489500021217     c                   z-add     0             $z                3 0
489600021216      *
489700021216      * Conta i caratteri riempiti dalle RigaTf2
489800021217     c                   z-add     0             $k                3 0
489900021216      *
490000021216      * Conta i caratteri riempiti dalle RigaTf3
490100021217     c                   z-add     0             $j                3 0
490200021216      *
490300021216      * Conta la posizione del campone da cui partire per impostare
490400021216      * la descrizione del tasto funzione
490500021216     c                   z-add     0             $y                2 0
490600021216      *
490700021217      * Stringhe che contengono le descrizioni dei tasti funzione
490800021216     c                   reset                   RigaTf1
490900021216     c                   reset                   RigaTf2
491000021216     c                   reset                   RigaTf3
491100070411      *
491200070411      *?F01?--> scelta bolla da assistenza clienti
491300070411     c                   IF        *in63 = *on
491400070411     c                   reset                   $Tf
491500070411     c                   movea     Tf01          $Tf
491600070411     c                   exsr      Rie_$Tf
491700070411     c                   else
491800070411      *?F01?--> scelta bolla generica
491900070411     c                   if        *in61 = *on
492000070411     c                   reset                   $Tf
492100070411     c                   movea     Tf01B         $Tf
492200070411     c                   exsr      Rie_$Tf
492300070411     c                   ENDIF
492400070411     c                   ENDIF
492500021216      *
492600070411      * F13 --> tassazione 1 bolla
492700021216     c                   IF        *in24 = *on
492800021217     c                   reset                   $Tf
492900021217     c                   movea     Tf13          $Tf
493000021217     c                   Exsr      Rie_$Tf
493100021217     C                   ENDIF
493200021216      *
493300070411      * F04 --> tassazione 2 bolla
493400021216     c                   IF        *in22 = *on
493500021217     c                   reset                   $Tf
493600021217     c                   movea     Tf04          $Tf
493700021217     c                   Exsr      Rie_$Tf
493800021217     C                   ENDIF
493900040220      *
494000070411      *?F16 --> assistenza clienti
494100040220     c                   IF        *in64 = *on
494200040220     c                   reset                   $Tf
494300040220     c                   movea     Tf16          $Tf
494400040220     c                   exsr      Rie_$Tf
494500040220     c                   ENDIF
494600021216      *
494700070411      * F05 --> eventi
494800021216     c                   IF        *in95 = *on
494900021217     c                   reset                   $Tf
495000021217     c                   movea     Tf05          $Tf
495100021217     c                   Exsr      Rie_$Tf
495200021217     C                   ENDIF
495300021216      *
495400021216      * F10
495500021217     c                   IF        *in98 = *on
495600021217     c                   reset                   $Tf
495700021217     c                   movea     Tf10          $Tf
495800021217     c                   Exsr      Rie_$Tf
495900021217     c                   ENDIF
496000021216      *
496100021216      * F14
496200021217     c                   IF        *in03 = *on
496300021217     c                   reset                   $Tf
496400021217     c                   movea     Tf14          $Tf
496500021217     c                   Exsr      Rie_$Tf
496600021217     c                   ENDIF
496700021216      *
496800021216      * F02
496900021217     c                   IF        *in04 = *on
497000021217     c                   reset                   $Tf
497100021217     c                   movea     Tf02          $Tf
497200021217     c                   Exsr      Rie_$Tf
497300021217     c                   ENDIF
497400110929      *
497500110930      * F22 --> Perfect Order
497600110929     c                   IF        *in86 = *on
497700110929     c                   reset                   $Tf
497800110929     c                   movea     Tf22          $Tf
497900110929     c                   Exsr      Rie_$Tf
498000110929     C                   ENDIF
498100021216      *
498200021216      * F11
498300021217     c                   IF        *in77 = *on
498400021217     c                   reset                   $Tf
498500021217     c                   movea     Tf11          $Tf
498600021217     c                   Exsr      Rie_$Tf
498700021217     c                   ENDIF
498800070404      *
498900070413      * F17 --> solo se partito com GEO
499000070413     c                   if        *in17
499100070404     c                   reset                   $Tf
499200070404     c                   movea     Tf17          $Tf
499300070404     c                   Exsr      Rie_$Tf
499400070413     c                   endif
499500021216      *
499600021216      * F07
499700021217     c                   IF        *in67 = *on
499800021217     c                   reset                   $Tf
499900021217     c                   movea     Tf07          $Tf
500000021217     c                   Exsr      Rie_$Tf
500100021217     c                   ENDIF
500200021216      *
500300021216      * F08
500400021217     c                   IF        *in74 = *on and *in16 = *on
500500021217     c                   reset                   $Tf
500600021217     c                   movea     Tf08          $Tf
500700021217     c                   Exsr      Rie_$Tf
500800021217     c                   ENDIF
500900130830
501000130830      * F09 --> Orari servizi non condizionato
501100131007     c                   IF        *in89 = *off and wabif9='S'
501200130930     c                   reset                   $Tf
501300130930     c                   movea     Tf09          $Tf
501400130930     c                   Exsr      Rie_$Tf
501500130930     c                   ENDIF
501600021216      *
501700021216      * F20 --> non condizionato
501800021217     c                   IF        *in89 = *off
501900021217     c                   reset                   $Tf
502000021217     c                   movea     Tf20          $Tf
502100021217     c                   Exsr      Rie_$Tf
502200021217     c                   ENDIF
502300021216      *
502400021216      * F21 --> non condizionato
502500021216     c                   IF        *in89 = *off
502600021217     c                   reset                   $Tf
502700021217     c                   movea     Tf21          $Tf
502800021217     c                   Exsr      Rie_$Tf
502900021217     c                   ENDIF
503000021218      *
503100021218      * F19 --> non condizionato
503200021218     c                   IF        *in89 = *off
503300021218     c                   reset                   $Tf
503400021218     c                   movea     Tf19          $Tf
503500021218     c                   Exsr      Rie_$Tf
503600021218     c                   ENDIF
503700070420     c
503800130830      * F06 --> solo se partito con GEO
503900070420     c                   if        *in17
504000070420     c                   reset                   $Tf
504100070420     c                   movea     Tf06          $Tf
504200070420     c                   Exsr      Rie_$Tf
504300070420     c                   ENDIF
504400021217      *
504500021217      * F12 --> non condizionato
504600021217     c                   IF        *in89 = *off
504700021217     c                   reset                   $Tf
504800021217     c                   movea     Tf12          $Tf
504900021217     c                   Exsr      Rie_$Tf
505000021217     c                   ENDIF
505100021216      *
505200021217      * Pulisco le stringa
505300021216     c                   z-add     1             $h
505400030121     c     1             do        62            $h                2 0
505500021216     C                   if        RigaTf1($h) = '#'
505600021216     c                   movel     *blanks       RigaTf1($h)
505700021216     c                   endif
505800021216     c                   enddo
505900021216     c                   z-add     1             $h
506000030121     c     1             do        62            $h                2 0
506100021216     C                   if        RigaTf2($h) = '#'
506200021216     c                   movel     *blanks       RigaTf2($h)
506300021216     c                   endif
506400021216     c                   enddo
506500021216     c                   z-add     1             $h
506600030121     c     1             do        62            $h                2 0
506700021216     C                   if        RigaTf3($h) = '#'
506800021216     c                   movel     *blanks       RigaTf3($h)
506900021216     c                   endif
507000021216     c                   enddo
507100021217      *
507200021216      * Imposto la descrizione del tasto funzione F24 e segnalo
507300021216      * quante righe ho riempito e quale devo emettere
507400021216     c                   SELECT
507500021216      *
507600021216     c                   when      $k<>*zeros and $Y<>*zeros
507700021216     c                   eval      $loop = 3
507800021216     c                   z-add     3             $loop             1 0
507900021217     C                   movel(p)  CF2413        F24Dxx           16
508000021216      *
508100021216     c                   when      $k<>*zeros and $Y=*zeros
508200021216     c                   z-add     2             $loop             1 0
508300021217     C                   movel(p)  CF2412        F24Dxx
508400021216      *
508500021216     c                   Other
508600021216     c                   z-add     1             $loop             1 0
508700021217     C                   movel     *blanks       F24Dxx
508800021216     c                   ENDSL
508900021216      * Prima riga di tasti funzione
509000030121     c                   movea     RigaTF1       FDxx             62
509100021216     c                   z-add     1             $Rig              1 0
509200021216      *
509300021216     c                   endsr
509400021217      *-------------------------------------------------------------------------
509500021217      *  Riempio le stringhe con i campi funzione compattati
509600021217      *-------------------------------------------------------------------------
509700021217     c     Rie_$Tf       begsr
509800021217      *
509900021217     c                   z-add     1             $x                2 0
510000021217     c     '#'           lookup    $Tf($x)                                31
510100021217     c                   add       $x            $Z
510200030121     c                   IF        $Z <= 63
510300021217     c                   eval      $j = $Z - $x + 1
510400021217     c                   movea     $Tf           RigaTF1($j)
510500021217     c                   ELSE
510600021217     c                   add       $x            $K
510700030121     c                   IF        $K <= 63
510800021217     c                   eval      $j = $K - $x + 1
510900021217     c                   movea     $Tf           RigaTF2($j)
511000021217     c                   ELSE
511100021217     c                   add       $x            $Y
511200030121     c                   IF        $Y <= 63
511300021217     c                   eval      $j = $Y - $x + 1
511400021217     c                   movea     $Tf           RigaTF3($j)
511500021217     c                   ENDIF
511600021217     c                   ENDIF
511700021217     c                   ENDIF
511800021217      *
511900021217     c                   endsr
512000021217      *-------------------------------------------------------------------------
512100021217      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
512200021217      *-------------------------------------------------------------------------
512300021217     c     Ges_F24       begsr
512400021217      *
512500021217     c                   SELECT
512600021217      *
512700021217     c                   WHEN      $Loop = 2 and $Rig =2  or
512800021217     c                             $Loop = 3 and $Rig =3
512900021217     c                   movea     RigaTF1       FDxx
513000021217     c                   z-add     1             $Rig
513100021217      *
513200021217     c                   if        $loop = 2
513300021217     c                   eval      F24Dxx= Cf2412
513400021217     c                   else
513500021217     c                   eval      F24Dxx= Cf2413
513600021217     c                   endif
513700021217      *
513800021217     c                   WHEN      $Loop = 2 and $Rig =1 or
513900021217     c                             $Loop = 3 and $Rig =1
514000021217     c                   movea     RigaTF2       FDxx
514100021217     c                   z-add     2             $Rig
514200021217      *
514300021217     c                   if        $loop = 2
514400021217     c                   eval      F24Dxx= Cf2422
514500021217     c                   else
514600021217     c                   eval      F24Dxx= Cf2423
514700021217     c                   endif
514800021217      *
514900021217     c                   WHEN      $Loop = 3 and $Rig =2
515000021217     c                   movea     RigaTF3       FDxx
515100021217     c                   z-add     3             $Rig
515200021217     c                   eval      F24Dxx= Cf2433
515300021217     c                   ENDSL
515400021217      *
515500021217     c                   endsr
515600021217      *-------------------------------------------------------------------------
515700991027      *  CARICO SUBFILE VARIE
515800991027      *-------------------------------------------------------------------------
515900991027     c     CAR_VARIE     BEGSR
516000991128      *
516100991027      *
516200991027     c                   SELECT
516300991027      *
516400991027     c                   WHEN       WSVN = *BLANKS
516500991027      *
516600991027     C                   WHEN       WSVN = §$2iva
516700991027     c                   z-add     Wvan          W0133
516800991027     c                   eval      Wtstdec = §cvfdc
516900991027     c                   exsr      EDTimporti
517000991027     c                   movel     IMPeditato    VIDiva
517100991027      *
517200991027     c                   OTHER
517300991027     c                   z-add     Wvan          W0133
517400991027     c                   eval      Wtstdec = §cvfdc
517500991027     c                   exsr      EDTimporti
517600991027     c                   movel     IMPeditato    V6Cva1
517700991027     c                   movel     WSVN          V6Csv1
517800991027     C                   Z-ADD     1             WA
517900991027     C     V6CSV1        LOOKUP    CC1(WA)                                99
518000020329     C     CC3(WA)       ifeq      'S'
518100020329      *
518200020329      * Verifico se si tratta di una bolla Fedex
518300021119     c                   if        LnaNtw = 'FED'
518400020329     C                   Z-ADD     1             WA
518500020329     C     V6CSV1        LOOKUP    CB1(WA)                                99
518600020329     C   99              MOVEL     CB2(WA)       V6CDE1
518700020329     C  N99              clear                   V6CDE1
518800020329     C                   else
518900991027     C   99              MOVEL     CC2(WA)       V6CDE1
519000991027     C  N99              MOVEL     *BLANKS       V6CDE1
519100020329     C                   endif
519200020329     C                   else
519300020329     C   99              MOVEL     CC2(WA)       V6CDE1
519400020329     C  N99              MOVEL     *BLANKS       V6CDE1
519500020329     C                   endif
519600991027     C                   add       1             NR6
519700131107     C                   WRITE     LRI6S06
519800991027      *
519900991027     c                   ENDSL
520000991027      *
520100991027     c                   ENDSR
520200040122      *-------------------------------------------------------------------------
520300040122      *  Carico le varie della seconda bolla
520400040122      *-------------------------------------------------------------------------
520500040122     c     Car_Varie1    BegSr
520600040122
520700040122s   1c                   Select
520800040122
520900040122     c                   When      wsvn = *Blanks
521000040122      * Iva
521100040122     c                   When      wsvn = §$2iva
521200040122     c                   z-add     wvan          w0133
521300040122     c                   Eval      Wtstdec = §cvfdc
521400040122     c                   ExSr      EDTimporti
521500040122     c                   Movel     IMPeditato    V9cIva
521600070411     c                   setoff                                       06
521700070411      * Bollo
521800070411     c                   When      wsvn = §$2bol
521900070411     c                   z-add     wvan          w0133
522000070411     c                   Eval      Wtstdec = §cvfdc
522100070411     c                   ExSr      EDTimporti
522200070411     c                   Movel     IMPeditato    V9cIva
522300070411     c                   seton                                        06
522400040122      * Varia G - sempre per prima
522500040122     c                   When      wsvn = §$2vrg
522600040122     c                   Eval      V9cSv1 = wsvn
522700040122     c                   z-add     wvan          w0133
522800040122     c                   Eval      Wtstdec = §cvfdc
522900040122     c                   ExSr      EDTimporti
523000040122     c                   Movel     IMPeditato    V9cVa1
523100040122      * Altre varie
523200040122   x1c                   Other
523300040122     c                   z-add     wvan          W0133
523400040122     c                   eval      Wtstdec = §cvfdc
523500040122     c                   exsr      EDTimporti
523600040122if  2c                   If        V9cSv2 = *Blanks
523700040122     c                   movel     IMPeditato    V9cVa2
523800040122     c                   movel     wsvn          V9csv2
523900040122   x2c                   Else
524000040122if  3c                   If        V9cSv3 = *Blanks
524100040122     c                   movel     IMPeditato    V9cVa3
524200040122     c                   movel     wsvn          V9csv3
524300040122   x3c                   Else
524400040122if  4c                   If        V9cSv4 = *Blanks
524500040122     c                   movel     IMPeditato    V9cVa4
524600040122     c                   movel     wsvn          V9csv4
524700041130   x4c                   Else
524800041130if  5c                   If        V9cSv5 = *Blanks
524900041130     c                   movel     IMPeditato    V9cVa5
525000041130     c                   movel     wsvn          V9csv5
525100041130   x5c                   Else
525200041130if  6c                   If        V9cSv6 = *Blanks
525300041130     c                   movel     IMPeditato    V9cVa6
525400041130     c                   movel     wsvn          V9csv6
525500041130    6c                   EndIf
525600041130    5c                   EndIf
525700040122    4c                   EndIf
525800040122    3c                   EndIf
525900040122    2c                   EndIf
526000040122
526100040122    1c                   EndSl
526200040122
526300040122     c                   EndSr
526400991027      *****************************************************************
526500991027      *  AGGANCIO ANAGRAFICHE
526600991027      *****************************************************************
526700991027     C     CTR_ANAGRA    BEGSR
526800991027      *
526900991027     C                   clear                   DS_cnaco
527000991027     C                   clear                   DS_cnind
527100991027     C                   clear                   DS_cnclp
527200991027     C                   clear                   DS_fncls
527300991027      *
527400991027     C                   CALL      'TIBS69R'
527500991027     C                   PARM                    tibs69DS
527600991027     C                   PARM                    DS_cnaco
527700991027     C                   PARM                    DS_cnind
527800991027     C                   PARM                    DS_cnclp
527900991027     C                   PARM                    DS_fncls
528000991027      *
528100991027     C                   eval      wtibs69r = 'S'
528200991027      * per il 1° giro è inizializzata nelle specifiche "D"
528300991027     C                   clear                   TIBS69DS
528400991027      *
528500991027     C                   ENDSR
528600991027      **********************************************************************
528700991027      * AGGANCIO TABELLA VALUTE
528800991027      **********************************************************************
528900991027     C     CTR_VALUTA    BEGSR
529000991027
529100991027     C                   MOVEL     'CV'          COD
529200991027     C                   MOVEL(P)  WDIV          KEY
529300991027     C     KTAB1         CHAIN     TABEL00F                           31
529400991027     C  N31              MOVEL     TBLUNI        DSCV
529500991027
529600991027     C                   ENDSR
529700991027      **********************************************************************
529800991027      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
529900991027      **********************************************************************
530000991027     C     EDTIMPORTI    BEGSR
530100991027
530200991027      * ------------------------------------------------------------------------
530300991027      * W0133      contiene il numero da editare per il video
530400991027      * WTSTDEC    corrisponde a §CVfdc della tabella CV
530500991027      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
530600991027      * ------------------------------------------------------------------------
530700991027      *
530800991027     c                   clear                   IMPeditato
530900991027     c                   clear                   W017A
531000991027      *
531100991027     c                   if        Wtstdec = 'S'
531200991027     C                   eval      W017a = %EDITC(W0133:'2')
531300991027     c                   else
531400991027     c                   z-add     W0133         W0130
531500991027     C                   eval      W017a = %EDITC(W0130:'2')
531600991027     c                   endif
531700991027      *
531800991027     c                   move      W017a         IMPeditato
531900991027      *
532000991027     c                   ENDSR
532100940802     C**************************************************************************
532200940802     C*    LEGAMI BOLLA
532300940802     C**************************************************************************
532400930413     C     LEGBOL        BEGSR
532500940802     C*
532600940912     C* DEVO MEMORIZZARE LA BOLLA POICHE DEVO RIAGGANCIARLA DOPO
532700940912     C                   MOVEL     ARBAAS        WAAS
532800940912     C                   MOVEL     ARBLNP        WLNP
532900940912     C                   MOVEL     ARBNRS        WNRS
533000940912     C                   MOVEL     ARBNSP        WNSP
533100940912     C*
533200940907     C* PULISCO IL SFL
533300950825     C                   SETOFF                                       787923
533400131107     C                   WRITE     LRI6C08
533500950825     C                   SETON                                        7923
533600940923     C                   MOVEL     *BLANKS       LEGAME
533700940907     C*
533800940907     C                   SETOFF                                       94
533900950718     C                   Z-ADD     *ZERO         NR8
534000940907     C                   Z-ADD     *ZERO         SAVNR8            4 0
534100940907     C                   MOVEL     *BLANKS       WFLG1             1
534200940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
534300940907     C*
534400940907     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
534500940907     C     KARB          CHAIN     FNLBL01L                           99
534600940907     C   99KARB          CHAIN     FNLBL02L                           99
534700940907     C  N99              DO
534800940907     C*
534900940907     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
535000940907     C                   MOVEL     LBLAAO        KAAS
535100940907     C                   MOVEL     LBLLPO        KLNP
535200940907     C                   MOVEL     LBLNRO        KNRS
535300940907     C                   MOVEL     LBLNSO        KNSP
535400940907     C     KLBL3         CHAIN     FNLBL02L                           99
535500950103     C     *IN99         IFEQ      *ON
535600950103     C                   MOVEL     'N'           WORIGI            1
535700950103     C                   ELSE
535800950103     C                   MOVEL     'S'           WORIGI
535900950103     C                   ENDIF
536000940907     C*
536100940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
536200940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
536300950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
536400950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
536500940907     C                   MOVEL     ' '           V8CSCE
536600940907     C                   MOVEL     LBLAAO        V8CAAP
536700940907     C                   MOVEL     LBLLPO        V8CLPP
536800940907     C                   MOVEL     LBLLAO        V8CLAP
536900940907     C                   MOVEL     LBLNRO        V8CNRP
537000940907     C                   MOVEL     LBLNSO        V8CNSP
537100940926     C                   CLEAR                   V8CAAN
537200940926     C                   CLEAR                   V8CLPN
537300940926     C                   CLEAR                   V8CLAN
537400940926     C                   CLEAR                   V8CNRN
537500940926     C                   CLEAR                   V8CNSN
537600940907     C*
537700940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
537800940907     C     LBLAAO        IFEQ      V3CAAS
537900940907     C     LBLLPO        ANDEQ     V3CLNP
538000940907     C     LBLNRO        ANDEQ     V3CNRS
538100940907     C     LBLNSO        ANDEQ     V3CNSP
538200940907     C                   MOVEL     *BLANKS       V8CPRE
538300940907     C                   SETON                                        80
538400940907     C                   SETON                                        81
538500940907     C                   MOVEL     '1'           WFLG1
538600940907     C                   ELSE
538700940907     C                   SETOFF                                       81
538800940907     C*
538900950316     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
539000950316     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
539100950316     C                   SETOFF                                       93
539200950316     C     KLBL3         SETLL     FNARB01L                               93
539300950316     C  N93              MOVEA     PRD(1)        V8CPRE
539400950316     C  N93              SETON                                        80
539500950316     C   93              MOVEA     PRD(2)        V8CPRE
539600950316     C   93              SETOFF                                       80
539700950316     C*
539800950316     C* SE NON TROVATA SU ARB PROVO SU BLP
539900950316     C  N93              DO
540000950316     C                   SETOFF                                       93
540100950316     C     KLBL3         SETLL     FNBLP01L                               93
540200010712      *
540300010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
540400010712     c                   if        *in93 = *off
540500010712     C                   MOVEA     PRD(1)        V8CPRE
540600010712     c                   else
540700950316     C* SE RICHIAMATO DALLE PARTENZE (FNLS05R) NON POSSO
540800950316     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
540900010712     C     PA1FLG        IFEQ      '3'
541000960903     C     PA1FLG        OREQ      'N'
541100950316     C                   MOVEA     PRD(3)        V8CPRE
541200950316     C                   SETON                                        80
541300950316     C                   ELSE
541400950316     C                   MOVEA     PRD(2)        V8CPRE
541500950316     C                   SETOFF                                       80
541600950316     C                   ENDIF
541700010712     C                   ENDIF
541800950316     C*
541900950316     C                   ENDDO
542000940907     C*
542100940907     C                   ENDIF
542200940907     C                   SETON                                        83
542300950103     C*
542400950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
542500950103     C                   MOVEL     *IN80         V8CI80
542600950103     C                   MOVEL     *IN81         V8CI81
542700950103     C                   MOVEL     *IN82         V8CI82
542800950103     C                   MOVEL     *IN83         V8CI83
542900950103     C*
543000940907     C                   Z-ADD     1             NR8
543100131107     C                   WRITE     LRI6S08
543200950825     C                   SETON                                        78
543300950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
543400940907     C*
543500950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
543600950103     C*                      2) NON HO TROVATO L'ORIGINALE
543700950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
543800950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
543900950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
544000950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
544100950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
544200950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
544300950103     C* L'ORIGINALE E LA MAMMA.
544400950103     C     WORIGI        IFEQ      'N'
544500950103     C                   MOVEL     *BLANKS       V8CSCE
544600950103     C                   CLEAR                   V8CAAP
544700950103     C                   CLEAR                   V8CLPP
544800950103     C                   CLEAR                   V8CLAP
544900950103     C                   CLEAR                   V8CNRP
545000950103     C                   CLEAR                   V8CNSP
545100950103     C                   MOVEL     LBLAAP        V8CAAN
545200950103     C                   MOVEL     LBLLPP        V8CLPN
545300950103     C                   MOVEL     LBLLAP        V8CLAN
545400950103     C                   MOVEL     LBLNRP        V8CNRN
545500950103     C                   MOVEL     LBLNSP        V8CNSN
545600950103     C                   SETOFF                                       8182
545700950103     C*
545800950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
545900950103     C     V8CAAN        IFEQ      V3CAAS
546000950103     C     V8CLPN        ANDEQ     V3CLNP
546100950103     C     V8CNRN        ANDEQ     V3CNRS
546200950103     C     V8CNSN        ANDEQ     V3CNSP
546300950103     C                   MOVEL     *BLANKS       V8CPRE
546400950103     C                   SETON                                        80
546500950103     C                   SETON                                        82
546600950103     C                   ELSE
546700950103     C*
546800950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
546900950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
547000950316     C                   EXSR      CHKBOL
547100950103     C*
547200950103     C                   ENDIF
547300950103     C*
547400950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
547500950103     C                   SETOFF                                       83
547600950103     C*
547700950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
547800950103     C                   MOVEL     *IN80         V8CI80
547900950103     C                   MOVEL     *IN81         V8CI81
548000950103     C                   MOVEL     *IN82         V8CI82
548100950103     C                   MOVEL     *IN83         V8CI83
548200950103     C*
548300950103     C                   ADD       1             NR8
548400131107     C                   WRITE     LRI6S08
548500950103     C*
548600950103     C                   ENDIF
548700940907     C                   SETOFF                                       83  99
548800940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
548900940907     C* CICLO FINO ALL'ULTIMO LEGAME
549000940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
549100940907     C                   Z-ADD     *ZERO         WA                3 0
549200940907     C                   Z-ADD     *ZERO         WB                3 0
549300940907     C                   MOVEA     *ZERO         AAN
549400940907     C     *IN99         DOWEQ     *OFF
549500940907     C*
549600940907     C                   MOVEL     ' '           V8CSCE
549700940907     C                   MOVEL     LBLAAP        V8CAAP
549800940907     C                   MOVEL     LBLLPP        V8CLPP
549900940907     C                   MOVEL     LBLLAP        V8CLAP
550000940907     C                   MOVEL     LBLNRP        V8CNRP
550100940907     C                   MOVEL     LBLNSP        V8CNSP
550200940907     C                   MOVEL     LBLAAN        V8CAAN
550300940907     C                   MOVEL     LBLLPN        V8CLPN
550400940907     C                   MOVEL     LBLLAN        V8CLAN
550500940907     C                   MOVEL     LBLNRN        V8CNRN
550600940907     C                   MOVEL     LBLNSN        V8CNSN
550700940907     C                   SETOFF                                       8182
550800940907     C*
550900950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
551000940907     C     LBLAAP        IFEQ      V3CAAS
551100940907     C     LBLLPP        ANDEQ     V3CLNP
551200940907     C     LBLNRP        ANDEQ     V3CNRS
551300940907     C     LBLNSP        ANDEQ     V3CNSP
551400940907     C                   SETON                                        81
551500940907     C                   ENDIF
551600940907     C*
551700940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
551800940907     C     LBLAAN        IFEQ      V3CAAS
551900940907     C     LBLLPN        ANDEQ     V3CLNP
552000940907     C     LBLNRN        ANDEQ     V3CNRS
552100940907     C     LBLNSN        ANDEQ     V3CNSP
552200940907     C                   MOVEL     *BLANKS       V8CPRE
552300940907     C                   SETON                                        80
552400940907     C                   SETON                                        82
552500940907     C                   ELSE
552600940907     C*
552700940907     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
552800940907     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
552900950316     C                   EXSR      CHKBOL
553000940907     C*
553100940907     C                   ENDIF
553200950103     C*
553300950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
553400950103     C                   MOVEL     *IN80         V8CI80
553500950103     C                   MOVEL     *IN81         V8CI81
553600950103     C                   MOVEL     *IN82         V8CI82
553700950103     C                   MOVEL     *IN83         V8CI83
553800940907     C*
553900940907     C* SCRIVO IL RECORD NEL SFL
554000940907     C                   ADD       1             NR8
554100131107     C                   WRITE     LRI6S08
554200940907     C*
554300940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
554400940907     C                   ADD       1             WA
554500940907     C                   MOVEL     LBLAAN        AAN(WA)
554600940907     C                   MOVEL     LBLLPN        LPN(WA)
554700940907     C                   MOVEL     LBLNRN        NRN(WA)
554800940907     C                   MOVEL     LBLNSN        NSN(WA)
554900940907     C*
555000940907     C     KLBL3         READE     FNLBL02L                               99
555100940907     C*
555200940907     C* SE NON HO PIU' BOLLE CON READE
555300940907     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
555400970116     C     WB            DOWLT     WA
555500970116     C     *IN99         ANDEQ     *ON
555600940907     C                   ADD       1             WB
555700940907     C                   MOVEL     AAN(WB)       KAAS
555800940907     C                   MOVEL     LPN(WB)       KLNP
555900940907     C                   MOVEL     NRN(WB)       KNRS
556000940907     C                   MOVEL     NSN(WB)       KNSP
556100940907     C     KLBL3         SETLL     FNLBL02L
556200940907     C     KLBL3         READE     FNLBL02L                               99
556300970116     C                   ENDDO
556400940907     C*
556500940907     C                   ENDDO
556600940907     C                   ENDDO
556700940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
556800940907     C*
556900940907     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
557000940907     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
557100940907     C     NR8           IFEQ      2
557200940907     C*
557300940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
557400131107     C     2             CHAIN     LRI6S08                            99
557500940907     C     V8CAAP        IFEQ      V3CAAS
557600940907     C     V8CLPP        ANDEQ     V3CLNP
557700940907     C     V8CNRP        ANDEQ     V3CNRS
557800940907     C     V8CNSP        ANDEQ     V3CNSP
557900940907     C                   MOVEL     V8CAAN        COMAAN
558000940907     C                   MOVEL     V8CLPN        COMLPN
558100940907     C                   MOVEL     V8CNRN        COMNRN
558200940907     C                   MOVEL     V8CNSN        COMNSN
558300950307     C                   MOVEL     V8CLAN        COMLAN
558400950316     C                   MOVEA     'SEGUEN.'     LBL(14)
558500940907     C                   ELSE
558600940907     C                   MOVEL     V8CAAP        COMAAN
558700940907     C                   MOVEL     V8CLPP        COMLPN
558800940907     C                   MOVEL     V8CNRP        COMNRN
558900940907     C                   MOVEL     V8CNSP        COMNSN
559000950307     C                   MOVEL     V8CLAP        COMLAN
559100950316     C                   MOVEA     'PRECED.'     LBL(14)
559200940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
559300940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
559400940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
559500131107     C     1             CHAIN     LRI6S08                            99
559600940926     C* REIMPOSTO NR8=2 PERCHE' MI SERVE DOPO
559700940926     C                   Z-ADD     2             NR8
559800940907     C                   ENDIF
559900940907     C*
560000950316     C                   MOVEA     COMLPN        LBL(21)
560100950316     C                   MOVEA     COMNRN        LBL(25)
560200950316     C                   MOVEA     COMNSN        LBL(28)
560300950316     C                   MOVEA     COMLAN        LBL(40)
560400940907     C*
560500940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
560600940907     C                   Z-ADD     1             W                 1 0
560700940907     C     V8CPRE        LOOKUP    PRD(W)                                 99
560800950307     C                   MOVEA     LBD(W)        LBL(44)
560900940907     C     W             COMP      2                                      94
561000940907     C*
561100940907     C                   MOVEA     LBL           LEGAME
561200940907     C                   ELSE
561300940907     C*
561400940907     C     NR8           IFGT      2
561500940907     C                   MOVEA     LB2           LEGAME
561600940907     C                   SETON                                        94
561700940907     C                   ENDIF
561800940907     C                   ENDIF
561900940907     C*
562000940907     C     NR8           COMP      1                                    3434
562100940907     C*
562200940907     C                   Z-ADD     NR8           SAVNR8
562300940907     C                   Z-ADD     1             NR8
562400940923     C     LEGAME        COMP      *BLANKS                            9292
562500940912     C*
562600940912     C* RIAGGANCIO LA BOLLA IN ESAME
562700940912     C                   MOVEL     WAAS          ARBAAS
562800940912     C                   MOVEL     WLNP          ARBLNP
562900940912     C                   MOVEL     WNRS          ARBNRS
563000940912     C                   MOVEL     WNSP          ARBNSP
563100940912     C     KARB2         CHAIN     FNARB01L                           99
563200930511     C*
563300930413     C                   ENDSR
563400950316     C**************************************************************************
563500950316     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
563600950316     C**************************************************************************
563700950316     C     CHKBOL        BEGSR
563800950316     C*
563900950316     C                   SETOFF                                       93
564000950316     C     KLBL1         SETLL     FNARB01L                               93
564100950316     C  N93              MOVEA     PRD(1)        V8CPRE
564200950316     C  N93              SETON                                        80
564300950316     C   93              MOVEA     PRD(2)        V8CPRE
564400950316     C   93              SETOFF                                       80
564500950316     C*
564600950316     C* SE NON TROVATA SU ARB PROVO SU BLP
564700950316     C  N93              DO
564800950316     C                   SETOFF                                       93
564900950316     C     KLBL1         SETLL     FNBLP01L                               93
565000010712      *
565100010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
565200010712     c                   if        *in93 = *off
565300010712     C                   MOVEA     PRD(1)        V8CPRE
565400010712     c                   else
565500950316     C* SE RICHIAMATO DALLE PARTENZE (FNLS05R) NON POSSO
565600950316     C* VISUALIZZARE LA BOLLA, QUINDI NON ABILITO LA SCELTA
565700010712     C     PA1FLG        IFEQ      '3'
565800960903     C     PA1FLG        OREQ      'N'
565900950316     C                   MOVEA     PRD(3)        V8CPRE
566000950316     C                   SETON                                        80
566100950316     C                   ELSE
566200950316     C                   MOVEA     PRD(2)        V8CPRE
566300950316     C                   SETOFF                                       80
566400950316     C                   ENDIF
566500010712     C                   ENDIF
566600950316     C*
566700950316     C                   ENDDO
566800950316     C*
566900950316     C                   ENDSR
567000940802     C**************************************************************************
567100940802     C*    VISUALIZZA BOLLA
567200940802     C**************************************************************************
567300911023     C     VISUAL        BEGSR
567400940321      *
567500940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
567600131108     c     Emriepil      tag
567700940712     C                   EXSR      RIEPIL
567800940712      *
567900131108      * PER F15 PASSO ALLA BOLLA, PER INVIO o F12 RITORNO AL SFL
568000940712     C  NKP              GOTO      ENDVIS
568100931112      *
568200931112     C                   SETOFF                                       89
568300940802     C                   MOVEL     'S'           WRIEM             1
568400021217      *
568500021217      * Riempio il campone in fondo alla videata con i tasti scelti
568600021217     C                   exsr      Ges_TastiFun
568700021217     c                   eval      FD03 = FDxx
568800021217     c                   eval      F24D03 = F24Dxx
568900911023     C     FOR03         TAG
569000940802     C     WRIEM         IFEQ      'S'
569100131107     C                   WRITE     LRI6T01
569200940802     C                   MOVEL     *BLANKS       WRIEM
569300940802     C                   ENDIF
569400021217      *
569500021217      * Imposto la stringa comandi del 'giro' successivo
569600021217     C                   IF        *inKY = *on and $Loop > 1
569700021217     c                   Exsr      Ges_F24
569800021217     c                   eval      FD03 = FDxx
569900021217     c                   eval      F24D03 = F24Dxx
570000021217     C                   ENDIF
570100021217      *
570200131107     C                   EXFMT     LRI6D03
570300070420     c                   setoff                                       28
570400070420     c                   clear                   v1cmsg
570500931112      *
570600131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
570700931112     C   KY*IN89         IFEQ      *ON
570800931112     C                   SETOFF                                       89
570900931112     C                   ELSE
571000931112     C                   SETON                                        89
571100931112     C                   ENDIF
571200931112     C   KY              GOTO      FOR03
571300911023     C*
571400131108     C* F3 - FINE LAVORO TORNO A SUBFILE
571500940714     C   KC              MOVEL     '2'           V2CFLG            1
571600911023     C   KC              GOTO      ENDVIS
571700911023     C*
571800131108     C* F12 - FINE LAVORO tolno a videata di riepilogo
571900131108     C   KL              GOTO      emriepil
572000911023     C*
572100911024     C*
572200930517     C* CMD18 - INTERROGA LEGAME BOLLA
572300930518     C   KS              DO
572400930518     C                   EXSR      CMD18
572500940907     C                   MOVEL     'S'           WRIEM
572600930518     C                   GOTO      FOR03
572700930518     C                   END
572800930517     C*
572900940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
573000920804     C                   EXSR      COMAND
573100940905     C     WCMD          IFEQ      'S'
573200940802     C     WF03          IFEQ      *BLANKS
573300070403     c
573400920804     C                   GOTO      FOR03
573500940802     C                   ELSE
573600940802     C                   MOVEL     '2'           V2CFLG
573700940802     C                   GOTO      ENDVIS
573800940802     C                   ENDIF
573900940905     C                   ENDIF
574000911023     C*
574100911023     C* FORMATO4
574200931112     C                   SETOFF                                       89
574300940802     C                   MOVEL     'S'           WRIEM
574400021217      *
574500021217      * Riempio il campone in fondo alla videata con i tasti scelti
574600021217     C                   exsr      Ges_TastiFun
574700021217     c                   eval      FD04 = FDxx
574800021217     c                   eval      F24D04 = F24Dxx
574900911023     C     FOR04         TAG
575000940802     C     WRIEM         IFEQ      'S'
575100131107     C                   WRITE     LRI6T01
575200940802     C                   MOVEL     *BLANKS       WRIEM
575300940802     C                   ENDIF
575400021217      * Imposto la stringa comandi del 'giro' successivo
575500021217     C                   IF        *inKY = *on and $Loop > 1
575600021217     c                   Exsr      Ges_F24
575700021217     c                   eval      FD04 = FDxx
575800021217     c                   eval      F24D04 = F24Dxx
575900021217     C                   ENDIF
576000131107     C                   EXFMT     LRI6D04
576100070420     c                   setoff                                       28
576200070420     c                   clear                   v1cmsg
576300931112      *
576400131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
576500931112     C   KY*IN89         IFEQ      *ON
576600931112     C                   SETOFF                                       89
576700931112     C                   ELSE
576800931112     C                   SETON                                        89
576900931112     C                   ENDIF
577000931112     C   KY              GOTO      FOR04
577100911023     C*
577200131108     C* F12 - RITORNO VIDEATA 3
577300911023     C   KL              GOTO      FOR03
577400920804     C*
577500131108     C* F18 - INTERROGA LEGAME BOLLA
577600930518     C   KS              DO
577700930518     C                   EXSR      CMD18
577800930518     C                   GOTO      FOR04
577900930518     C                   END
578000940923     C*
578100940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
578200131120     c                   eval      wvideo='4'
578300131120     C                   EXSR      CTR_puIn
578400940923     C     WRIEM         CABEQ     'S'           FOR04
578500930517     C*
578600940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
578700920804     C                   EXSR      COMAND
578800940905     C     WCMD          IFEQ      'S'
578900940802     C     WF03          IFEQ      *BLANKS
579000940802     C                   GOTO      FOR04
579100940802     C                   ELSE
579200940802     C                   MOVEL     '2'           V2CFLG
579300940802     C                   GOTO      ENDVIS
579400940802     C                   ENDIF
579500940905     C                   ENDIF
579600911023     C*
579700911023     C* VIDEATA 5
579800931112     C                   SETOFF                                       89
579900940809     C                   MOVEL     'S'           WRIEM
580000021217      *
580100021217      * Riempio il campone in fondo alla videata con i tasti scelti
580200021217     C                   exsr      Ges_TastiFun
580300021217     c                   eval      FD05 = FDxx
580400021217     c                   eval      F24D05 = F24Dxx
580500911023     C     FOR05         TAG
580600940809     C     WRIEM         IFEQ      'S'
580700131107     C                   WRITE     LRI6T01
580800940809     C                   MOVEL     *BLANKS       WRIEM
580900940809     C                   ENDIF
581000951220     C                   Z-ADD     1             REC5
581100021217      *
581200021217      * Imposto la stringa comandi del 'giro' successivo
581300021217     C                   IF        *inKY = *on and $Loop > 1
581400021217     c                   Exsr      Ges_F24
581500021217     c                   eval      FD05 = FDxx
581600021217     c                   eval      F24D05 = F24Dxx
581700021217     C                   ENDIF
581800131107     C                   WRITE     LRI6Z05
581900131107     C                   EXFMT     LRI6C05
582000070420     c                   setoff                                       28
582100070420     c                   clear                   v1cmsg
582200931112      *
582300131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
582400931112     C   KY*IN89         IFEQ      *ON
582500931112     C                   SETOFF                                       89
582600931112     C                   ELSE
582700931112     C                   SETON                                        89
582800931112     C                   ENDIF
582900931112     C   KY              GOTO      FOR05
583000911023     C*
583100131108     C* F12 - RITORNO VIDEATA 2
583200911023     C   KL              GOTO      FOR04
583300920804     C*
583400940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
583500920804     C                   EXSR      COMAND
583600940905     C     WCMD          IFEQ      'S'
583700940802     C     WF03          IFEQ      *BLANKS
583800940802     C                   GOTO      FOR05
583900940802     C                   ELSE
584000131108     C                   MOVEL     '2'           V2CFLG
584100131108     C                   GOTO      ENDVIS
584200940802     C                   ENDIF
584300070406
584400070406     c                   else
584500070406     c
584600070406     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
584700070406     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
584800070406     c                   if        v3cgesd='?'
584900070406     c                   exsr      VISUALAbi
585000070406     c                   clear                   v3cgesd
585100070406     c                   eval      wriem='S'
585200070406     C                   GOTO      FOR05
585300070406     c                   endif
585400940905     C                   ENDIF
585500131108     c
585600131108     C                   GOTO      emriepil
585700931112      *
585800911023     C     ENDVIS        ENDSR
585900070403     C**************************************************************************
586000070403     C*    VISUALIZZA Abilitazioni
586100070403     C**************************************************************************
586200070403     C     VISUALAbi     BEGSR
586300070403     c*
586400131120     c                   clear                   fnlrg7ds
586500131120     c                   move      v3cdsp        ilrg7aas
586600131120     c                   eval      ilrg7lnp=v3clnp
586700131120     c                   eval      ilrg7nrs=v3cnrs
586800131120     c                   eval      ilrg7nsp=v3cnsp
586900131120     c                   eval      ilrg7vsdet='?'
587000131120     c                   eval      ilrg7lung=58
587100131120     c                   call      'FNLRG7R'
587200131120     c                   parm                    fnlrg7ds
587300070403     c
587400070403     C                   ENDSR
587500940712     C**************************************************************************
587600940802     C*    VISUALIZZA BOLLA PRECEDENTE
587700940802     C**************************************************************************
587800930510     C     VISUA2        BEGSR
587900940321      *
588000940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
588100131108     c     Emriepi2      tag
588200940712     C                   EXSR      RIEPI2
588300940712      *
588400131108      * PER F15 PASSO ALLA BOLLA, PER INVIO RITORNO AL SFL
588500940713     C  NKP              GOTO      ENDV8C
588600931112      *
588700931112     C                   SETOFF                                       89
588800940809     C                   MOVEL     'S'           WRIEM
588900021217      *
589000021217      * Riempio il campone in fondo alla videata con i tasti scelti
589100021217     C                   exsr      Ges_TastiFun
589200070516     c                   eval      FD03 = FDxx
589300070516     c                   eval      F24D03 = F24Dxx
589400930510     C     FOR033        TAG
589500940809     C     WRIEM         IFEQ      'S'
589600131107     C                   WRITE     LRI6T01
589700940809     C                   MOVEL     *BLANKS       WRIEM
589800940809     C                   ENDIF
589900021217      *
590000021217      * Imposto la stringa comandi del 'giro' successivo
590100021217     C                   IF        *inKY = *on and $Loop > 1
590200021217     c                   Exsr      Ges_F24
590300021217     c                   eval      FD03 = FDxx
590400021217     c                   eval      F24D03 = F24Dxx
590500021217     C                   ENDIF
590600131107     C                   EXFMT     LRI6D03
590700070420     c                   setoff                                       28
590800070420     c                   clear                   v1cmsg
590900931112      *
591000131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
591100931112     C   KY*IN89         IFEQ      *ON
591200931112     C                   SETOFF                                       89
591300931112     C                   ELSE
591400931112     C                   SETON                                        89
591500931112     C                   ENDIF
591600931112     C   KY              GOTO      FOR033
591700930510     C*
591800131108     C* F12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
591900930510     C   KC
592000131108     COR KL              GOTO      Emriepi2
592100930510     C*
592200940905     C* CONTROLLO SE PREMUTI TASTI FUNZIONALI E, NEL CASO, RIEMETTO
592300930510     C                   EXSR      COMAND
592400940905     C     WCMD          IFEQ      'S'
592500930510     C                   GOTO      FOR033
592600930510     C                   END
592700931112      *
592800930510     C* FORMATO4
592900931112     C                   SETOFF                                       89
593000940809     C                   MOVEL     'S'           WRIEM
593100021217      *
593200021217      * Riempio il campone in fondo alla videata con i tasti scelti
593300021217     C                   exsr      Ges_TastiFun
593400021217     c                   eval      FD04 = FDxx
593500021217     c                   eval      F24D04 = F24Dxx
593600930510     C     FOR044        TAG
593700940809     C     WRIEM         IFEQ      'S'
593800131107     C                   WRITE     LRI6T01
593900940809     C                   MOVEL     *BLANKS       WRIEM
594000940809     C                   ENDIF
594100021217      *
594200021217      * Imposto la stringa comandi del 'giro' successivo
594300021217     C                   IF        *inKY = *on and $Loop > 1
594400021217     c                   Exsr      Ges_F24
594500021217     c                   eval      FD04 = FDxx
594600021217     c                   eval      F24D04 = F24Dxx
594700021217     C                   ENDIF
594800131107     C                   EXFMT     LRI6D04
594900070420     c                   setoff                                       28
595000070420     c                   clear                   v1cmsg
595100931112      *
595200931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
595300931112     C   KY*IN89         IFEQ      *ON
595400931112     C                   SETOFF                                       89
595500931112     C                   ELSE
595600931112     C                   SETON                                        89
595700931112     C                   ENDIF
595800931112     C   KY              GOTO      FOR044
595900930510     C*
596000930510     C* CMD12 - RITORNO VIDEATA 3
596100930510     C   KL              GOTO      FOR033
596200940923     C*
596300940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
596400131120     c                   eval      wvideo='4'
596500131120     C                   EXSR      CTR_puIn
596600940923     C     WRIEM         CABEQ     'S'           FOR044
596700930510     C*
596800940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
596900930510     C                   EXSR      COMAND
597000940905     C     WCMD          IFEQ      'S'
597100930510     C                   GOTO      FOR044
597200940905     C                   ENDIF
597300931112      *
597400930510     C* VIDEATA 5
597500931112     C                   SETOFF                                       89
597600940809     C                   MOVEL     'S'           WRIEM
597700021217      *
597800021217      * Riempio il campone in fondo alla videata con i tasti scelti
597900021217     C                   exsr      Ges_TastiFun
598000021217     c                   eval      FD05 = FDxx
598100021217     c                   eval      F24D05 = F24Dxx
598200930510     C     FOR055        TAG
598300940809     C     WRIEM         IFEQ      'S'
598400131107     C                   WRITE     LRI6T01
598500940809     C                   MOVEL     *BLANKS       WRIEM
598600940809     C                   ENDIF
598700951220     C                   Z-ADD     1             REC5
598800021217      *
598900021217      * Imposto la stringa comandi del 'giro' successivo
599000021217     C                   IF        *inKY = *on and $Loop > 1
599100021217     c                   Exsr      Ges_F24
599200021217     c                   eval      FD05 = FDxx
599300021217     c                   eval      F24D05 = F24Dxx
599400021217     C                   ENDIF
599500131107     C                   WRITE     LRI6Z05
599600131107     C                   EXFMT     LRI6C05
599700070420     c                   setoff                                       28
599800070420     c                   clear                   v1cmsg
599900931112      *
600000931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
600100931112     C   KY*IN89         IFEQ      *ON
600200931112     C                   SETOFF                                       89
600300931112     C                   ELSE
600400931112     C                   SETON                                        89
600500931112     C                   ENDIF
600600931112     C   KY              GOTO      FOR055
600700930510     C*
600800930510     C* CMD12 - RITORNO VIDEATA 2
600900930510     C   KL              GOTO      FOR044
601000930510     C*
601100940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
601200940905     C                   EXSR      COMAND
601300940905     C     WCMD          IFEQ      'S'
601400930510     C                   GOTO      FOR055
601500930510     C                   END
601600131108
601700131108     C                   GOTO      Emriepi2
601800931112      *
601900940713     C     ENDV8C        ENDSR
602000940802     C**************************************************************************
602100940802     C* TASTI DI FUNZIONE
602200940802     C**************************************************************************
602300920804     C     COMAND        BEGSR
602400940905     C*
602500940905     C* PULISCO IL FLAG CHE INDICA SE PREMUTO UN TASTO FUNZIONE
602600940905     C                   MOVE      *BLANKS       WCMD              1
602700040218      *
602800070411      *?F1 - Aggiunta bolla in reclamo?o scelta bolla generica
602900040218if  1c                   if        *inKA   = *on
603000070423     C                   Z-ADD     arbLNP        COMLNP
603100070411     C                   MOVEL     '3'           V2CFLG
603200070411      *?-
603300070411if  2c                   if        SDSprm  > 1
603400131111     c                   eval      wARBlnp = ARBlnp
603500131111     c                   eval      wARBnrs = ARBnrs
603600131111     c                   eval      wARBnsp = ARBnsp
603700131111     c                   eval      wARBaas = ARBaas
603800040218     c                   clear                   FIDNA1ds
603900040218     c                   eval      IA1ins  = 'S'
604000040218     c                   eval      IA1tor  = 'S'
604100040218     c                   eval      IA1ogg  = ds_OggSped
604200070411    2c                   endif
604300040218     c                   goto      Fine
604400040218e   1c                   endif
604500940802     C*
604600990119     C* F5 - INTERROGAZIONE EVENTI
604700040218do  1C   KE              DO
604800940905     C                   MOVEL     'S'           WCMD
604900940726     C                   MOVEL     V2CAAS        PB5AAS
605000940726     C                   MOVEL     COMLNP        PB5LNP
605100940726     C                   MOVEL     V2CNRS        PB5NRS
605200940726     C                   MOVEL     V2CNSP        PB5NSP
605300940726     C                   MOVEL     *BLANKS       PB5F03
605400940726     C                   MOVEL     *BLANKS       PB5FLG
605500940802     C                   MOVEL     *BLANKS       PB5PGM
605600131107     C                   MOVEL     'FNLRI6R'     PB5PGM
605700940726     C                   MOVEL     V1CDSA        PB5DSA
605800940726     C                   MOVEL     V3CDSP        PB5DSP
605900940726     C                   MOVEL     VIDRMN        PB5RMN
606000940726     C                   MOVEL     V3CLNA        PB5LNA
606100940802     C                   MOVEL     V3DLNA        PB5LN1
606200940726     C                   MOVEL     V4CRSD        PB5RSD
606300940726     C                   MOVEL     VIDDCP        PB5DCP
606400941031     C                   MOVEL     VIDTMC        PB5TMC
606500940726     C                   MOVEL     PARAB5        KPJBU
606600940722     C                   CALL      'FNLRB5R'
606700930525     C                   PARM                    KPJBA
606800941109     C                   MOVEL     KPJBU         PARAB5
606900941109     C                   MOVEL     PB5F03        WF03
607000941214     C                   MOVEL     '1'           PA0B5
607100940725     C                   MOVEL     'S'           WRIEM
607200040218e   1C                   ENDDO
607300070420      *?F6 - Stampa LDV
607400070420if  1c                   if        *inKf = *on
607500070420     C                   MOVEL     'S'           WCMD
607600080201     c* Se assegnato non abilitato non stampo la LdV ma emetto errore
607700080201     c                   if        arbaca=' '
607800080201     C                   MOVEL     MSG(31)       V1CMSG
607900080201     C                   SETON                                        28
608000080201     c                   else
608100070420     c
608200070420     c                   if        wrichtrul90=' '
608300070420     c* La prima volta devo proporre la richiesta stampanti
608400070420     c                   exsr      RICHSTAMPAN
608500070420     C                   MOVEL     'S'           WRIEM
608600070420     c
608700070420     c* Se premuto F3 esco senza fare nulla
608800070420     c                   if        d90f3<>'1' and not *in91
608900070420     c                   eval      wrichtrul90='S'
609000070420     c                   endif
609100070420     c                   if        *in91
609200070420     C                   MOVEL     MSG(30)       V1CMSG
609300070420     C                   SETON                                        28
609400070420     c                   endif
609500070420     c                   endif
609600070420     c
609700070420     c                   if        wrichtrul90='S'
609800070420     C                   CLEAR                   FNLSB5DS
609900070426     c                   movel     'L'           db0tla
610000070420     C                   MOVEL     'R'           DB0RIS
610100070426     C                   MOVEL     'A'           DB0TBO
610200070420     C                   MOVEL     ARBAAS        DB0AAS
610300070420     C                   MOVEL     ARBLNP        DB0LNP
610400070420     C                   MOVEL     ARBNRS        DB0NRS
610500070420     C                   MOVEL     ARBNSP        DB0NSP
610600130319     c                   reset                   fnlsB6ds1
610700070420     C                   CALL      D90PSL
610800070420     C                   PARM                    FNLSB5DS
610900130308     c                   parm                    fnlsB6ds1
611000070420     C                   MOVEL     MSG(29)       V1CMSG
611100070420     C                   SETON                                        28
611200070420     c                   endif
611300080201     c                   endif
611400070420     c                   endif
611500930525     C*
611600990119     C* F7 - ANOMALIE
611700040218do  1C   KG              DO
611800960523     C                   MOVEL     'S'           WCMD
611900960523     C*
612000991027     C                   CLEAR                   FNLr44DS
612100960523     C                   MOVE      V2CAAS        D44AAS
612200960523     C                   MOVE      COMLNP        D44FIL
612300960523     C                   MOVE      V2CNRS        D44NRS
612400960523     C                   MOVE      V2CNSP        D44NSP
612500960523     C                   MOVEL     'A'           D44TBO
612600960523     C                   MOVE      'I'           D44MOD
612700991027     C                   MOVEL     FNLr44DS      KPJBU
612800960523     C                   CALL      'FNLR44R'
612900960523     C                   PARM                    KPJBA
613000960523     C*
613100990119     C                   MOVEL     'S'           WRIEM
613200040218e   1C                   ENDDO
613300920804     C*
613400990119     C* F8  - INTERROGAZIONE EVENTI SEGNACOLLI
613500040218if  1C     *INKH         IFEQ      *ON
613600961121     C                   MOVEL     'S'           WCMD
613700961121     C*
613800991027     C                   CLEAR                   FNLR45DS
613900961121     C                   MOVE      V2CAAS        D45AAS
614000961121     C                   MOVE      COMLNP        D45FIL
614100961121     C                   MOVE      V2CNRS        D45NRS
614200961121     C                   MOVE      V2CNSP        D45NSP
614300991027     C                   MOVEL     FNLR45DS      KPJBU
614400961121     C                   CALL      'FNLR45R'
614500961121     C                   PARM                    KPJBA
614600961121     C*
614700990119     C                   MOVEL     'S'           WRIEM
614800040218e   1C                   ENDIF
614900130830     c
615000130830     c* F9 - Orari servizi
615100131007     c                   if        *inki=*on
615200130830     C                   MOVEL     'S'           WCMD
615300131007     c                   if        wabif9='S'
615400130930     c                   exsr      sr_orarisr
615500131007     c                   endif
615600130830     C                   MOVEL     'S'           WRIEM
615700130830e   1C                   ENDIF
615800021213     C*
615900021213     C* F11 - Note supermercati
616000040218if  1C     *INKK         IFEQ      *ON
616100021213     C     *IN77         ANDEQ     *ON
616200021213      * imposto i campi 'bolla' della ds
616300021217     C                   MOVEL     'S'           WCMD
616400021213     C                   clear                   FnLrb3ds
616500030122     c                   Movel     V1cdsa        Rb3Rsu
616600030121     C                   z-add     SimFel        RB3Fil
616700021216     C                   z-add     V3CAAS        RB3AAS
616800030121     C                   z-add     V3CLNP        RB3LNP
616900021216     C                   z-add     V3CNRS        RB3NRS
617000021216     C                   z-add     V3CNSP        RB3NSP
617100021213     C                   z-add     V3CDSP        RB3DSP
617200021213     C                   z-add     VIDRMN        RB3RMN
617300021213     C                   z-add     V3CLNA        RB3LNA
617400131108     C                   movel     V32RSD        RB3RSD
617500021218     C                   z-add     VIDPKF        RB3PKF
617600021213     C                   z-add     VIDNCL        RB3NCL
617700021213     C                   z-add     VIDVLF        RB3VLB
617800030703      * bancali - part.varie "B"
617900040218If  2c                   If        %subst(ArbGva:1:1) = 'B'
618000030121     C                   z-add     §Ar5Tba       RB3Tba
618100030121     C                   z-add     VidNba        RB3Nba
618200030121     C                   z-add     §Ar5Tb2       RB3Tb2
618300030121     C                   z-add     VidNb2        RB3Nb2
618400030703     c                   Movel     V4cGva        Rb3Gva
618500040218e   2c                   EndIf
618600030703      * bancali - part.varie "O"
618700040218If  2c                   If        %subst(ArbGva:1:1) = 'O'
618800030703     C                   z-add     §Ar5bTba      RB3Tba
618900030703     C                   z-add     VidNba        RB3Nba
619000030703     C                   z-add     §Ar5bTb2      RB3Tb2
619100030703     C                   z-add     VidNb2        RB3Nb2
619200030703     c                   Movel     V4cGva        Rb3Gva
619300030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
619400030703     c                   Z-Add     §Ar5bCor      Rb3Cor
619500040218e   2c                   EndIf
619600021213      * richiamo PGM per note supermercati
619700021213     C                   call      'FNLRB3R'
619800030122     C                   PARM                    Kpjba
619900030121     C                   PARM                    Fnlrb3Ds
620000021213      *
620100021217     C                   MOVEL     'S'           WRIEM
620200040218e   1C                   ENDIF
620300961121     C*
620400990119     C* F14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
620500040218if  1C     *INKN         IFEQ      *ON
620600930624     C     *IN03         ANDEQ     *ON
620700941213     C     PA1GIA        ANDEQ     *BLANK
620800940905     C                   MOVEL     'S'           WCMD
620900991027     C                   CLEAR                   FNLG30DS
621000941213     C                   MOVEL     'A'           D30POA
621100941213     C                   MOVEL     V2CAAS        D30AAS
621200941213     C                   MOVEL     COMLNP        D30LNP
621300941213     C                   MOVEL     V2CNRS        D30NRS
621400941213     C                   MOVEL     V2CNSP        D30NSP
621500991027     C                   MOVEL     FNLG30DS      KPJBU
621600941213     C                   CALL      'FNLG31C'
621700920804     C                   PARM                    KPJBA
621800930624     C*
621900990119     C                   MOVEL     'S'           WRIEM
622000040218e   1C                   ENDIF
622100040218      *
622200040218      *?F16 - Gestione reclami?
622300040218if  1c                   if        *inKQ = *on
622400040225     c                   eval      wARBaas = ARBaas
622500131111     c                   eval      wARBlnp = ARBlnp
622600131111     c                   eval      wARBnrs = ARBnrs
622700131111     c                   eval      wARBnsp = ARBnsp
622800040225     c                   reset                   FIDNA2ds
622900040225if  2c                   if        SDSprm  > 1
623000040225     c                   eval      IA2mod  = 'I'
623100040225e   2c                   endif
623200040225     c                   eval      IA2ogg  = ds_OggSped
623300040225     c                   call      'FIDNA2R'
623400040225     c                   parm                    KPJBA
623500040225     c                   parm                    FIDNA2ds
623600040225     c                   movel     OA2f03        WF03
623700040225     c                   movel     'S'           WRIEM
623800051201     C                   MOVEL     'S'           WCMD
623900040218e   1c                   endif
624000070404      *
624100140311      *?F17 - Note AUT
624200070404if  1c                   if        *inKR = *on
624300140123     c                   clear                   fnlrn6ds
624400140123     c                   eval      in6mod='V'
624500140123     c                   eval      in6aas=v2caas
624600140123     c                   eval      in6lnp=comlnp
624700140123     c                   eval      in6nrs=v2cnrs
624800140123     c                   eval      in6nsp=v2cnsp
624900140123      * se sono una spedizione in consegna passo i dati della distinta del file ARB
625000140123     c                   IF        ARBfbc = *blanks and
625100140123     c                               ARBddc > 0 and
625200140123     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
625300140123     c                               arbica='R' or arbicc=' ' or arbicc='R')
625400140123     c     KSPED         chain     Fnarb01l
625500140123     c                   If        %found(Fnarb01l)
625600140123     c                   eval      in6fgs = arbifp
625700140123     c                   eval      in6nfv = arbndc
625800140123     c                   eval      in6dfv = arbddc
625900140123     c                   eval      in6pdr = arbpdc
626000140123     c                   eval      in6cons = 'S'
626100140123     c                   Endif
626200140123     c                   Endif
626300140123     c                   call      'FNLRN6R'
626400070404     c                   parm                    kpjba
626500140123     c                   parm                    fnlrn6ds
626600070404     c                   movel     'S'           WRIEM
626700070404     C                   MOVEL     'S'           WCMD
626800070404e   1c                   endif
626900920804     C*
627000990119     C* F21 - DETTAGLIO SEGNACOLLI
627100040218do  1C   KV              DO
627200940905     C                   MOVEL     'S'           WCMD
627300950628     C                   MOVEL     'A'           PA1TBO
627400950629     C                   CLEAR                   PA1F03
627500940720     C                   MOVEL     PARAM1        KPJBU
627600940726     C                   CALL      'FNLR37R'
627700920804     C                   PARM                    KPJBA
627800940802     C                   MOVEL     KPJBU         PARAM1
627900940802     C                   MOVEL     PA1F03        WF03
628000990119     C                   MOVEL     'S'           WRIEM
628100040218e   1C                   ENDDO
628200920804     C*
628300990119     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
628400040218do  1C   KJ              DO
628500940905     C                   MOVEL     'S'           WCMD
628600950628     C                   MOVEL     'A'           PA1TBO
628700950629     C                   CLEAR                   PA1F03
628800940712     C                   MOVEL     PARAM1        KPJBU
628900940726     C                   CALL      'FNLR75R'
629000920804     C                   PARM                    KPJBA
629100941214     C                   MOVEL     '1'           PA075
629200940802     C                   MOVEL     KPJBU         PARAM1
629300940802     C                   MOVEL     PA1F03        WF03
629400990119     C                   MOVEL     'S'           WRIEM
629500040218e   1C                   ENDDO
629600930114     C*
629700990119     C* F19 - INTERROGA SPUNTE VARI
629800040218if  1C     *INKT         IFEQ      *ON
629900940905     C                   MOVEL     'S'           WCMD
630000940713     C                   Z-ADD     0             PA4NFV
630100940713     C                   MOVEL     9             PA4NPG
630200940713     C                   MOVEL     99            PA4NPS
630300101202     C                   MOVEL     pa1pgm        PA4PGM
630400101203     C                   MOVEL     '1'           PA4FLG
630500950214     C                   MOVEL     V3CFLS        PA4LNP
630600940719     C                   MOVEL     V3CLNA        PA4LNA
630700940719     C                   MOVEL     V3CNRS        PA4NRS
630800940713     C                   Z-ADD     0             PA4NCD
630900940713     C                   Z-ADD     0             PA4NCA
631000101201     c                   eval      pa4boan=v2caas
631100101201     c                   eval      pa4bolp=comlnp
631200101201     c                   eval      pa4bors=v2cnrs
631300101201     c                   eval      pa4bosp=v2cnsp
631400040218if  2C     VIDNCD        IFGT      0
631500940713     C                   MOVEL     VIDNCD        PA4NCD
631600940713     C                   MOVEL     VIDNCA        PA4NCA
631700101201x   2C**                 ELSE
631800930705     C* CERCO I SEGNACOLLI DAL DETTAGLIO
631900101201     C**   KARB2         SETLL     FNART01L
632000101201     C**   KARB2         READE     FNART01L                               31
632100101201do  3C*+N31              DO
632200101201     C**                 Z-ADD     ARTNSC        PA4NCD
632300101201     C**   KARB2         SETGT     FNART01L
632400101201     C**   KARB2         READPE    FNART01L                               31
632500101201if  4C**N31ARTNSC        IFNE      PA4NCD
632600101201     C**                 MOVEL     ARTNSC        PA4NCA
632700101201e   4C**                 ENDIF
632800101201e   3C**                 ENDDO
632900101201e   2C                   ENDIF
633000930705     C*
633100940802     C                   MOVEL     PARA04        KPJBU
633200940722     C                   CALL      'FNLR04R'
633300920804     C                   PARM                    KPJBA
633400940725     C                   MOVEL     'S'           WRIEM
633500060130     C                   MOVEL     'S'           WCMD
633600040218e   1C                   ENDIF
633700930415     C*
633800990119     C* F23 - VISUALIZZA DATA CONSEGNA PREVISTA ESTERNA E ORE RITARDO
633900131121do  1C** KX              DO
634000131121     C**                 MOVEL     'S'           WCMD
634100131121     C**                 SETOFF                                       68
634200930504     C*
634300131121     C**                 MOVEL     ' '           D98TLA
634400131121     C**                 MOVEL     'A'           D98TBO
634500131121     C**                 MOVEL     ARBAAS        D98AAS
634600131121     C**                 MOVEL     ARBLNP        D98LNP
634700131121     C**                 MOVEL     ARBNRS        D98NRS
634800131121     C**                 MOVEL     ARBNSP        D98NSP
634900930504     C*
635000131121     C**                 CALL      'TNSD99R'
635100131121     C**                 PARM                    TNSD99DS
635200930504     C*
635300131121     c**                 if        d98dce>0
635400131121     c**   *iso          movel     d98dce        dataeur
635500131121     c**                 movel     dataeur       datadmy
635600131121     c**                 movel     dataeur       v08dce
635700131121     c**                 movel     datadmy       v06dce
635800131121     c**                 else
635900131121     c**                 clear                   v08dce
636000131121     c**                 clear                   v06dce
636100131121     c**                 endif
636200070404     c
636300131121     C**                 Z-ADD     D98NRC        VIDNRC
636400930526     C* SE LA SPEDIZIONE NON E' CONSEGNATA VISUALIZZO ANCHE L'ORA
636500930526     C*   CONSEGNA PREVISTA
636600131121     c**n69              eval      vidhmc=%editw(d98hce: '0 :  ')
636700930504     C*
636800930504     C* SE COMODO = 1  E' STATA FATTA CALL PGM
636900131121     C**                 MOVEL     '1'           PA098
637000131121e   1C**                 ENDDO
637100931112      *
637200990119      * F20 - INTERROGAZIONE AFFIDABILITA'
637300040218do  1C   KU              DO
637400940905     C                   MOVEL     'S'           WCMD
637500950102     C                   CLEAR                   FNLR97
637600950102     C                   MOVEL     V2CAAS        R97AAS
637700950102     C                   MOVEL     COMLNP        R97LNP
637800950102     C                   MOVEL     V2CNRS        R97NRS
637900950102     C                   MOVEL     V2CNSP        R97NSP
638000950102     C                   MOVEL     V3CDSP        R97DSP
638100950102     C                   MOVEL     VIDRMN        R97RMN
638200950102     C                   MOVEL     V3CLNA        R97LNA
638300950102     C                   MOVEL     V3DLNA        R97DLA
638400950102     C                   Z-ADD     VIDDCP        R97DCP
638500950102     C                   MOVEL     V4CRSD        R97RSD
638600950102     C                   MOVEL     V1CDSA        R97RSU
638700040121     C                   MOVEL     *blanks       R97CML
638800950102     C                   MOVEL     'A'           R97AP
638900950102     C   95              MOVEL     'S'           R97EVE
639000070404     C                   Z-ADD     w08DBR        R97DBR
639100070404     C                   Z-ADD     w08DPC        R97DPC
639200070404     C                   Z-ADD     w08DUC        R97DUC
639300950102     C                   MOVEL     VIDTMC        R97TMC
639400950102     C                   Z-ADD     VIDCAS        R97CAS
639500950102     C                   MOVEL     VIDVCA        R97VCA
639600931112     C                   MOVE      *BLANKS       KPJBU
639700950102     C                   MOVEL     FNLR97        KPJBU
639800940722     C                   CALL      'FNLR97R'                            90
639900931112     C                   PARM                    KPJBA
640000950102     C                   MOVEL     KPJBU         FNLR97
640100950102     C                   MOVEL     R97F03        WF03
640200950102     C                   MOVEL     'S'           WRIEM
640300040218e   1C                   ENDDO
640400110930     C* F22 - Perfect Order
640500110929do  1C                   if        *inkw
640600110929     C                   MOVEL     'S'           WCMD
640700110929     c                   movel     param1        s_param1
640800110929     c*
640900110929     c                   MOVEL     'W'           pa1flg
641000131107     c                   MOVEL(p)  'FNLRI6R'     pa1pgm
641100111005     c                   movel(p)  vidrma        pa1rma
641200111005     c                   movel(p)  vidxco        pa1xco
641300110929     c                   movel     paraaz        kpjbu
641400110929     c                   movel     param1        kpjbu
641500131127     c                   call      'FNLR36C'
641600110929     c                   parm                    kpjba
641700110929     c*
641800110929     c                   movel     s_param1      param1
641900110929     C*
642000110929     C                   MOVEL     'S'           WRIEM
642100110929e   1C                   ENDif
642200940905     C*
642300131107     C* ? FORMATO VIDEO:  LRI6D06  (TASSAZIONE 1a BOLLA)   ---   TASTO F13 ?
642400991026     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
642500040218if  1C     *INKM         IFEQ      *ON
642600991027     C                   eval      WCMD = 'S'
642700991027     C                   EVAL      *in89 = *off
642800131107     C                   WRITE     LRI6T01
642900131107     C                   WRITE     LRI6D06
643000991027     C                   eval      NR6 = 1
643100131107     C                   EXFMT     LRI6C06
643200040218e   1C                   ENDIF
643300991027     C*
643400131107     C* ? FORMATO VIDEO:  LRI6D09  (TASSAZIONE 2a BOLLA)   ---   TASTO F04 ?
643500991027     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
643600040218if  1C     *INKD         IFEQ      *ON
643700991027     C                   eval      WCMD = 'S'
643800991027     C                   EVAL      *in89 = *off
643900131107     C                   WRITE     LRI6T01
644000131107     C                   EXFMT     LRI6D09
644100040218e   1C                   ENDIF
644200990119     C*
644300990119     C* F2 - INTERROGAZIONE COMUNICAZIONE ANOMALIA
644400990129     C*      NON RICHIAMO SE RICHIAMATO DA INTERROGAZINE C.A.
644500040218if  1C     *INKB         IFEQ      *ON
644600990129     C     PA1FLG        ANDNE     'C'
644700040218if  2C     CONTCA        IFEQ      1
644800991027     C                   CLEAR                   FIDN00DS
644900990119     C                   Z-ADD     VHCAAC        I00AAC
645000990119     C                   Z-ADD     VHCFIL        I00FIL
645100990119     C                   Z-ADD     VHCNCA        I00NCA
645200990119     C                   Z-ADD     ARBAAS        I00AAS
645300990119     C                   Z-ADD     ARBNRS        I00NRS
645400990119     C                   Z-ADD     ARBNSP        I00NSP
645500990119     C                   Z-ADD     ARBLNP        I00LNP
645600990119     C                   MOVEL     'A'           I00TPB
645700991027     C                   MOVEL     FIDN00DS      KPJBU
645800990119     C                   CALL      'FIDN01R'
645900990119     C                   PARM                    KPJBA
646000000309     C                   PARM                    callricors        1
646100040218x   2C                   ELSE
646200990119     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
646300990119     C                   SETOFF                                       79
646400131107     C                   WRITE     LRI6C11
646500990119     C                   SETON                                        797823
646600990119     C                   CLEAR                   NRR11
646700040809      *
646800040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
646900040809      * legate alla spedizione che passo
647000040809     c                   clear                   fidn12ds
647100040809     c                   eval      i12aas = arbAAS
647200040809     c                   eval      i12lnp = arbLNP
647300040809     c                   eval      i12nrs = arbNRS
647400040809     c                   eval      i12nsp = arbNSP
647500040809     c                   eval      i12fel = 'E'
647600040809     c                   eval      i12fan = 'E'
647700040809     c*
647800040809     c                   call      'FIDN12R'
647900040809     c                   parm                    fidn12ds
648000040809     c*
648100040809     c                   setoff                                       31
648200040809     c*
648300040809     c* se non ci sono errori
648400040809     c                   if        o12err <> ' '
648500040809     c                   seton                                        31
648600040809     c                   else
648700040809     c* se numero di CA trovate maggiore di zero
648800040809     c                   if        o12nca = 0
648900040809     c                   seton                                        31
649000040809     c                   else
649100040809     c                   z-add     1             ii                2 0
649200040809     c                   dow       ii <= o12nca and *in31 = *off
649300040809     c                   movea     skkey(ii)     dskey
649400040809     C                   Z-ADD     dsaac         V11AAC
649500040809     C                   Z-ADD     dsfil         V11FIL
649600040809     C                   Z-ADD     dsnca         V11NCA
649700040809     C                   Z-ADD     skdca(ii)     G02INV
649800040809     C                   Z-ADD     *ZERO         G02DAT
649900040809     C                   MOVEL     '3'           G02ERR
650000040809     C                   CALL      'XSRDA8'
650100040809     C                   PARM                    WLBDAT
650200040809     C                   Z-ADD     G02DAT        V11DCA
650300040809     C                   Z-ADD     skfca(ii)     V11FCA
650400040809     C     skfca(ii)     CHAIN     FNDFA01L
650500040809     C                   IF        %found(FNDFA01L)
650600040809     C                   MOVEL     DFADES        V11DFA
650700040809     C                   ENDIF
650800040809     C**
650900040809     C                   ADD       1             NRR11             4 0
651000131107     C                   WRITE     LRI6S11
651100040809     c*
651200040809     c                   add       1             ii
651300040809     c                   enddo
651400040809     c*
651500040809     c                   seton                                        31
651600040809     c*
651700040809     c                   endif
651800040809     c                   endif
651900040809     c**
652000990119     C* EMETTO FORMATO
652100990119     C     FOR11         TAG
652200990119     C                   Z-ADD     1             NRR11
652300131107     C                   WRITE     LRI6T01
652400131107     C                   WRITE     LRI6Z11
652500131107     C                   EXFMT     LRI6C11
652600990119     C* F12 - RITORNO
652700040218if  3C     *INKL         IFEQ      *OFF
652800131107     C                   READC     LRI6S11                                31
652900040218do  4C     *IN31         DOWEQ     *OFF
653000040218if  5C     V11SCE        IFEQ      '5'
653100991027     C                   CLEAR                   FIDN00DS
653200990119     C                   Z-ADD     V11AAC        I00AAC
653300990119     C                   Z-ADD     V11FIL        I00FIL
653400990119     C                   Z-ADD     V11NCA        I00NCA
653500990119     C                   Z-ADD     ARBAAS        I00AAS
653600990119     C                   Z-ADD     ARBNRS        I00NRS
653700990119     C                   Z-ADD     ARBNSP        I00NSP
653800990119     C                   Z-ADD     ARBLNP        I00LNP
653900990119     C                   MOVEL     'A'           I00TPB
654000991027     C                   MOVEL     FIDN00DS      KPJBU
654100990119     C                   CALL      'FIDN01R'
654200990119     C                   PARM                    KPJBA
654300000309     C                   PARM                    callricors        1
654400990119     C**
654500990119     C                   CLEAR                   V11SCE
654600131107     C                   UPDATE    LRI6S11
654700040218e   5C                   ENDIF
654800990119     C**
654900131107     C                   READC     LRI6S11                                31
655000040218e   4C                   ENDDO
655100990119     C                   GOTO      FOR11
655200040218e   3C                   ENDIF
655300040218e   2C                   ENDIF
655400990119     C*
655500990119     C                   MOVEL     'S'           WRIEM
655600990119     C                   MOVEL     'S'           WCMD
655700040218e   1C                   ENDIF
655800931112      *
655900920804     C                   ENDSR
656000070420     C**************************************************************************
656100070420      * RICHIEDO LE STAMPANTI per stampa LDV
656200070420     C**************************************************************************
656300070420     c     RICHSTAMPAN   BEGSR
656400070420     C                   CLEAR                   TRUL90DS
656500070420      *
656600070420     C                   MOVEL     'S'           d90RSB
656700070420     C                   CALL      'TRUL90R'
656800070420     C                   PARM                    KPJBA
656900070420     C                   PARM                    trul90DS
657000070420      *
657100070420     C* F3 - FINE
657200070420     c                   if        d90f3<>'1'
657300070420      *
657400070420      * OVRPRTF BOLLE A4
657500070426     c                   if        *in97
657600070420     C                   MOVEA     D90PRB4       CMA4(30)
657700070420     C                   MOVEA     D90MDB4       CMA4(52)
657800070426     c                   else
657900070426     C                   MOVEA     D90PRB4       CMA4f6(30)
658000070426     C                   MOVEA     D90MDB4       CMA4f6(52)
658100070426     c                   endif
658200070420     C*
658300111221     C                   Z-ADD     130           LUNG             15 5
658400111221     C                   CLEAR                   COMMAN          130
658500070426     C   97              MOVEA     CMA4          COMMAN
658600070426     C  n97              MOVEA     CMA4f6        COMMAN
658700070420     C                   CALL      'QCMDEXC'                            91
658800070420     C                   PARM                    COMMAN
658900070420     C                   PARM                    LUNG
659000070420      *
659100070420     C                   if        not *in91
659200070420      *
659300070420      * OVRPRTF BOLLE A5
659400070426     c                   if        *in97
659500070420     C                   MOVEA     D90PRB5       CMA5(30)
659600070420     C                   MOVEA     D90MDB5       CMA5(52)
659700070426     c                   else
659800070426     C                   MOVEA     D90PRB5       CMA5f6(30)
659900070426     C                   MOVEA     D90MDB5       CMA5f6(52)
660000070426     c                   endif
660100070420     C*
660200111221     C                   Z-ADD     130           LUNG
660300070420     C                   CLEAR                   COMMAN
660400070426     C   97              MOVEA     CMA5          COMMAN
660500070426     C  n97              MOVEA     CMA5f6        COMMAN
660600070420     C                   CALL      'QCMDEXC'                            91
660700070420     C                   PARM                    COMMAN
660800070420     C                   PARM                    LUNG
660900070420     c                   endif
661000070420     c                   endif
661100070420      *
661200070420     c                   ENDSR
661300940802     C**************************************************************************
661400940802     C*    VISUALIZZA LEGAMI BOLLA
661500940802     C**************************************************************************
661600930413     C     CMD18         BEGSR
661700940802     C*
661800940411     C                   CLEAR                   WCAR
661900940411     C*
662000940411     C* MUOVO I DATI DELLA BOLLA CHE STO VISUALIZZANDO
662100940714     C                   MOVEL     V2CAAS        NUM2              4 0
662200940411     C                   MOVEL     COMLNP        COMLPN
662300940714     C                   MOVEL     V2CNRS        COMNRN
662400940714     C                   MOVEL     V2CNSP        COMNSN
662500950321     C                   MOVEL     V3CLNA        COMLAN
662600950428     C                   MOVEL     COMC08        SAVC08
662700940408     C**
662800940713     C* SAVNR8 > 1 --> GESTIONE SUBFILE
662900940408     C**
663000940907    1C     SAVNR8        IFGT      2
663100940408     C*
663200940809     C                   MOVEL     'S'           WRIEM
663300930413     C     FORC2         TAG
663400940713     C                   Z-ADD     1             REC8
663500940809     C     WRIEM         IFEQ      'S'
663600131107     C                   WRITE     LRI6T01
663700131107     C                   WRITE     LRI6Z08
663800940809     C                   MOVEL     *BLANKS       WRIEM
663900940809     C                   ENDIF
664000950428     C*
664100950428     C                   MOVEL     SAVC08        COMC08
664200131107     C                   EXFMT     LRI6C08
664300930413     C*
664400930413     C* CMD12 - FINE LAVORO
664500930413     C   KL              GOTO      SOTTO
664600930413     C*
664700131107     C                   READC     LRI6S08                                93
664800930510     C   93              GOTO      SOTTO
664900930510     C*
665000940408    2C     *IN93         DOWEQ     '0'
665100930413     C*
665200941230    3C     V8CSCE        IFEQ      '5'
665300940713     C                   MOVEL     ' '           V8CSCE
665400950103     C*
665500950103     C* RIPRISTINO GLI INDICATORI
665600950103     C                   MOVEL     V8CI80        *IN80
665700950103     C                   MOVEL     V8CI81        *IN81
665800950103     C                   MOVEL     V8CI82        *IN82
665900950103     C                   MOVEL     V8CI83        *IN83
666000131107     C                   UPDATE    LRI6S08
666100930504     C*
666200940907     C     V8CAAN        IFEQ      *ZERO
666300940907     C                   MOVEL     V8CAAP        LBLAAN
666400940907     C                   MOVEL     V8CLPP        LBLLPN
666500940907     C                   MOVEL     V8CNRP        LBLNRN
666600940907     C                   MOVEL     V8CNSP        LBLNSN
666700940907     C                   MOVEL     'P'           V8CFPS            1
666800940907     C                   ELSE
666900940907     C                   MOVEL     V8CAAN        LBLAAN
667000940907     C                   MOVEL     V8CLPN        LBLLPN
667100940907     C                   MOVEL     V8CNRN        LBLNRN
667200940907     C                   MOVEL     V8CNSN        LBLNSN
667300940907     C                   MOVEL     'S'           V8CFPS
667400940907     C                   ENDIF
667500940408     C*
667600940408     C* CONTROLLO SE IL LEGAME E' IN PARTENZA O IN ARRIVO
667700940907     C     KLBL1         CHAIN     FNARB01L                           93
667800940408     C*
667900940408    4C     *IN93         IFEQ      *OFF
668000940408     C*
668100940408     C                   EXSR      LEG2
668200940408     C*
668300940408   X4C                   ELSE
668400940408     C*
668500950316     C                   Z-ADD     LBLAAN        PA0AAS
668600950316     C                   Z-ADD     LBLLPN        PA0LNP
668700950316     C                   Z-ADD     LBLNRN        PA0NRS
668800950428     C                   Z-ADD     LBLNSN        PA0NSP
668900060130     C                   MOVEL     rsut          PA0RSU
669000941214     C                   MOVEL     SIMFEL        PA0SIM
669100040121     C                   MOVEL     *blanks       PA0CML
669200960903     C     PA1FLG        IFEQ      'N'
669300960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
669400960903     C*       verifichi questo caso
669500960903     C                   MOVEL     'R'           PA0FLG
669600960903     C                   ELSE
669700941214     C                   MOVEL     'A'           PA0FLG
669800960903     C                   END
669900941214     C                   MOVEL     *BLANKS       PA0CLI
670000950629     C                   CLEAR                   PA0F03
670100941214     C                   MOVEL     *BLANKS       PA0GIA
670200941214     C                   MOVEL     PARAM0        KPJBU
670300940722     C                   CALL      'FNLS05R'
670400930413     C                   PARM                    KPJBA
670500941214     C                   MOVEL     KPJBU         PARAM0
670600941214     C                   MOVEL     PA0F03        WF03
670700940907     C                   MOVEL     'S'           WRIEM
670800940408    4C                   ENDIF
670900940408     C*
671000940408    3C                   ENDIF
671100930413     C*
671200131107     C                   READC     LRI6S08                                93
671300940408    2C                   ENDDO
671400930413     C                   GOTO      FORC2
671500930413     C*
671600930413     C     SOTTO         TAG
671700930413     C*
671800940408    1C                   ELSE
671900940408     C*
672000940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
672100131107     C     2             CHAIN     LRI6S08                            99
672200940907     C     V8CAAP        IFEQ      V3CAAS
672300940907     C     V8CLPP        ANDEQ     V3CLNP
672400940907     C     V8CNRP        ANDEQ     V3CNRS
672500940907     C     V8CNSP        ANDEQ     V3CNSP
672600940907     C                   MOVEL     V8CAAN        ARBAAS
672700940907     C                   MOVEL     V8CLPN        ARBLNP
672800940907     C                   MOVEL     V8CNRN        ARBNRS
672900940907     C                   MOVEL     V8CNSN        ARBNSP
673000940907     C                   MOVEL     'S'           V8CFPS
673100940907     C                   ELSE
673200940907     C                   MOVEL     V8CAAP        ARBAAS
673300940907     C                   MOVEL     V8CLPP        ARBLNP
673400940907     C                   MOVEL     V8CNRP        ARBNRS
673500940907     C                   MOVEL     V8CNSP        ARBNSP
673600940907     C                   MOVEL     'P'           V8CFPS
673700940907     C                   ENDIF
673800940907     C* QUINDI VEDO SE LA TROVO FRA GLI ARRIVI
673900940907     C     KARB2         CHAIN     FNARB01L                           93
674000940408     C*
674100121116    2C     *IN93         IFEQ      *ON
674200950316     C                   Z-ADD     ARBAAS        PA0AAS
674300950316     C                   Z-ADD     ARBLNP        PA0LNP
674400950316     C                   Z-ADD     ARBNRS        PA0NRS
674500950316     C                   Z-ADD     ARBNSP        PA0NSP
674600060130     C                   MOVEL     rsut          PA0RSU
674700941214     C                   MOVEL     SIMFEL        PA0SIM
674800040121     C                   MOVEL     *blanks       PA0CML
674900960903     C     PA1FLG        IFEQ      'N'
675000960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
675100960903     C*       verifichi questo caso
675200960903     C                   MOVEL     'R'           PA0FLG
675300960903     C                   ELSE
675400941214     C                   MOVEL     'A'           PA0FLG
675500960903     C                   END
675600941214     C                   MOVEL     *BLANKS       PA0CLI
675700950629     C                   CLEAR                   PA0F03
675800941214     C                   MOVEL     *BLANKS       PA0GIA
675900941214     C                   MOVEL     PARAM0        KPJBU
676000940722     C                   CALL      'FNLS05R'
676100930413     C                   PARM                    KPJBA
676200941214     C                   MOVEL     KPJBU         PARAM0
676300941214     C                   MOVEL     PA0F03        WF03
676400940907     C                   MOVEL     'S'           WRIEM
676500121116     c* reimposto la key spedizione
676600121116     C                   MOVEL     NUM2          arbAAS
676700121116     C                   MOVEL     COMLPN        arbLNP
676800121116     C                   MOVEL     COMNRN        arbNRS
676900121116     C                   MOVEL     COMNSN        arbNSP
677000930510     C*
677100940408   X2C                   ELSE
677200940408     C*
677300940408     C                   EXSR      LEG2
677400940408     C*
677500940408    2C                   END
677600940408    1C                   END
677700940411     C*
677800940411     C* RICARICO I DATI DELLA BOLLA INIZIALE
677900940411     C     WCAR          IFEQ      'C'
678000940411     C                   SETOFF                                       75
678100940714     C                   MOVEL     NUM2          V2CAAS
678200940411     C                   MOVEL     COMLPN        COMLNP
678300940714     C                   MOVEL     COMNRN        V2CNRS
678400940714     C                   MOVEL     COMNSN        V2CNSP
678500940712     C     KARB          CHAIN     FNARB01L                           93
678600940411     C* RICARICO I DATI
678700940411     C                   EXSR      BOLLA
678800940411     C*
678900940411     C* RIABILITO IL CMD18
679000940411     C                   SETON                                        94
679100940411    1C                   END
679200940802     C*
679300930413     C                   ENDSR
679400940802     C**************************************************************************
679500940802     C*    LEGAMI BOLLA
679600940802     C**************************************************************************
679700940408     C     LEG2          BEGSR
679800940802     C*
679900940411     C                   MOVEL     'C'           WCAR              1
680000940408     C*
680100940408     C* 75 ON - NON RIENTRO NEL CARICAMENTO LEGAME BOLLA
680200940408     C                   SETON                                        75
680300940408     C* 94 OFF- DISABILITO IL CMD18
680400940408     C                   SETOFF                                       94
680500940408     C* CARICO DATI BOLLA
680600940408     C                   EXSR      BOLLA
680700940411     C*
680800940411     C* VISUALIZZA NUOVO LEGAME
680900940408     C* SE LA BOLLA E' UNA PRECEDENTE --> VISUALIZZO LA SEGUENTE
681000940713     C     V8CFPS        IFEQ      'P'
681100950316     C                   MOVEA     'SEGUEN.'     LBL(14)
681200940408     C                   ELSE
681300940408     C* SE LA BOLLA E' UNA SEGUENTE   --> VISUALIZZO LA PRECEDENTE
681400950316     C                   MOVEA     'PRECED.'     LBL(14)
681500940408     C                   ENDIF
681600950316     C                   MOVEA     COMLPN        LBL(21)
681700950316     C                   MOVEA     COMNRN        LBL(25)
681800950316     C                   MOVEA     COMNSN        LBL(28)
681900950316     C                   MOVEA     COMLAN        LBL(40)
682000950307     C                   MOVEA     *BLANKS       LBL(44)
682100940408     C                   MOVEA     LBL           LEGAME
682200940408     C*
682300940411     C* MOUVO I DATI DELLA LEGATA NEI CAMPI DELLA PARAM
682400940714     C                   MOVEL     ARBAAS        V2CAAS
682500940411     C                   MOVEL     ARBLNP        COMLNP
682600940714     C                   MOVEL     ARBNRS        V2CNRS
682700940714     C                   MOVEL     ARBNSP        V2CNSP
682800940408     C*
682900940408     C                   EXSR      VISUA2
683000940408     C*
683100941230     C                   MOVEL     'S'           WRIEM
683200940408     C                   ENDSR
683300940408     C*
683400940712      **************************************************************************
683500131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
683600940408      *     PER BOLLA NORMAL - CMD18 PREMUTO DA VISUAL -
683700091216      *     ricordarsi di aggiornare anche RIEPI2 per ogni modifica fatta qui
683800940712      **************************************************************************
683900940712     C     RIEPIL        BEGSR
684000940712      *
684100940712     C                   SETOFF                                       89
684200940712      *
684300940712      * EMETTO IL FORMATO VIDEO
684400940720     C                   MOVEL     'S'           WRIEM
684500940712     C     FOR07         TAG
684600940720     C     WRIEM         IFEQ      'S'
684700131107     C                   WRITE     LRI6T01
684800940720     C                   MOVEL     *BLANKS       WRIEM
684900940720     C                   ENDIF
685000021217      * Imposto la stringa comandi del 'giro' successivo
685100021217     C                   IF        *inKY = *on and $Loop > 1
685200021217     c                   Exsr      Ges_F24
685300021217     c                   eval      FD07 = FDxx
685400021217     c                   eval      F24D07 = F24Dxx
685500021217     C                   ENDIF
685600131107     C                   EXFMT     LRI6D07
685700070420     c                   setoff                                       28
685800070420     c                   clear                   v1cmsg
685900940712      *
686000131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
686100940712     C   KY*IN89         IFEQ      *ON
686200940712     C                   SETOFF                                       89
686300940712     C                   ELSE
686400940712     C                   SETON                                        89
686500940712     C                   ENDIF
686600940712     C   KY              GOTO      FOR07
686700940712      *
686800131108      * F3 - FINE LAVORO TORNO A SUBFILE
686900940714     C   KC              MOVEL     '2'           V2CFLG            1
687000940712     C   KC              GOTO      ENDRIE
687100940712      *
687200131108      * F18 - INTERROGA LEGAME BOLLA
687300940712     C   KS              DO
687400940712     C                   EXSR      CMD18
687500940712     C                   GOTO      FOR07
687600940712     C                   END
687700940712      *
687800940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
687900940712     C                   EXSR      COMAND
688000940905     C     WCMD          IFEQ      'S'
688100940802     C     WF03          IFEQ      *BLANKS
688200070404     c
688300940802     C                   GOTO      FOR07
688400940802     C                   ELSE
688500940802     C                   MOVEL     '2'           V2CFLG
688600940802     C                   GOTO      ENDRIE
688700070404     C                   ENDIF
688800091215     c
688900070404     c                   else
689000070404     c
689100070404     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
689200070404     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
689300070404     c                   if        v3cgesd='?'
689400131120
689500131120     c**                 if        wriem='S'
689600131120     c**                 eval      wvideo='R'
689700131120     c**                 exsr      Riemett
689800131120     c**                 endif
689900131120
690000070404     c                   exsr      VISUALAbi
690100070404     c                   clear                   v3cgesd
690200070412     c                   eval      wriem='S'
690300070404     c                   endif
690400131120     c
690500131120     c* controlla se immessi dei '?'
690600131120     c                   eval      wvideo='R'
690700131120     c                   exsr      CTR_puIn
690800091215     c
690900091215     c                   if        wriem='S'
691000091215     C                   GOTO      FOR07
691100091215     c                   endif
691200070404     c
691300940712     C                   END
691400940712      *
691500940712     C     ENDRIE        ENDSR
691600940712      **************************************************************************
691700131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
691800940408      *     PER LEGAME BOLLA - CMD18 PREMUTO DA VISUA2 -
691900940712      **************************************************************************
692000940712     C     RIEPI2        BEGSR
692100940712      *
692200940712     C                   SETOFF                                       89
692300940712      *
692400940712      * EMETTO IL FORMATO VIDEO
692500940809     C                   MOVEL     'S'           WRIEM
692600940712     C     FOR77         TAG
692700940809     C     WRIEM         IFEQ      'S'
692800131107     C                   WRITE     LRI6T01
692900940809     C                   MOVEL     *BLANKS       WRIEM
693000940809     C                   ENDIF
693100091216      * Imposto la stringa comandi del 'giro' successivo
693200091216     C                   IF        *inKY = *on and $Loop > 1
693300091216     c                   Exsr      Ges_F24
693400091216     c                   eval      FD07 = FDxx
693500091216     c                   eval      F24D07 = F24Dxx
693600091216     C                   ENDIF
693700091216     c
693800131107     C                   EXFMT     LRI6D07
693900070516     c                   setoff                                       28
694000070516     c                   clear                   v1cmsg
694100940408     C*
694200131108      * F3 - FINE LAVORO TORNO A SUBFILE
694300940714     C   KC              MOVEL     '2'           V2CFLG            1
694400940408     C*
694500940408     C  NKC              DO
694600940712      *
694700131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
694800940712     C   KY*IN89         IFEQ      *ON
694900940712     C                   SETOFF                                       89
695000940712     C                   ELSE
695100940712     C                   SETON                                        89
695200940712     C                   ENDIF
695300940712     C   KY              GOTO      FOR77
695400940712      *
695500940712      *
695600131108      * F18 - INTERROGA LEGAME BOLLA
695700940712     C   KS              DO
695800940712     C                   EXSR      CMD18
695900940712     C                   GOTO      FOR77
696000940712     C                   END
696100940712      *
696200940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
696300940712     C                   EXSR      COMAND
696400940905     C     WCMD          IFEQ      'S'
696500940712     C                   GOTO      FOR77
696600091216     c
696700091216     c                   else
696800091216     c
696900091216     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
697000091216     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
697100091216     c                   if        v3cgesd='?'
697200131120
697300131120     c**                 if        wriem='S'
697400131120     c**                 eval      wvideo='R'
697500131120     c**                 exsr      Riemett
697600131120     c**                 endif
697700131120
697800091216     c                   exsr      VISUALAbi
697900091216     c                   clear                   v3cgesd
698000091216     c                   eval      wriem='S'
698100091216     c                   endif
698200131120
698300131120     c* controlla se immessi dei '?'
698400131120     c                   eval      wvideo='R'
698500131120     c                   exsr      CTR_puIn
698600091216     c
698700091216     c                   if        wriem='S'
698800091216     C                   GOTO      FOR77
698900091216     c                   endif
699000091216     c
699100940712     C                   END
699200091216     C                   END
699300940712      *
699400940712     C                   ENDSR
699500131120      **************************************************************************
699600131120      * riemissione video per nuovo '?'
699700131120      **************************************************************************
699800131120     c**   Riemett       BEGSR
699900131120     c**                 write     LRI6t01
700000131120
700100131120     c**                 select
700200131120     c**                 when      wvideo='4'
700300131120     c**                 write     LRI6d04
700400131120     c**                 when      wvideo='R'
700500131120     c**                 write     LRI6d07
700600131120     c**                 endsl
700700131120
700800131120     c**                 ENDSR
700900940923      **************************************************************************
701000940923      *     CONTROLLI FORMATO VIDEO D04
701100940923      **************************************************************************
701200131120     C     CTR_puIn      BEGSR
701300110601     c*
701400110601     c* verifico se inserito '?' su email destinatario
701500110601     c                   if        v4cemw='?'
701600110601     c                   exsr      sr_emdest
701700110601     C                   MOVEL     'S'           WRIEM
701800110601     c                   endif
701900940923      *
702000091215      * Verifico se inserito '?' per visualizzazione beneficiario
702100091215     c                   if        vidben='?'
702200131120     c**                 if        wriem='S'
702300131120     c**                 exsr      Riemett
702400131120     c**                 endif
702500131120     c
702600091215     c                   exsr      sr_bencas
702700091215     C                   MOVEL     'S'           WRIEM
702800091215     c                   endif
702900131114     c
703000131114     c* Visualizzazione dati filiale
703100131114     c                   if        viddec_f='?'
703200131120     c**                 if        wriem='S'
703300131120     c**                 exsr      Riemett
703400131120     c**                 endif
703500131120
703600131114     c                   clear                   trulorfds
703700131114     c                   eval      iorffil=arblna
703800131114     c                   call      'TRULORFR'
703900131114     c                   parm                    kpjba
704000131114     c                   parm                    trulorfds
704100131114     c
704200131114     c                   clear                   viddec_f
704300131114     C                   MOVEL     'S'           WRIEM
704400131114     c                   endif
704500091215     c
704600940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
704700940923     C     V4IGCA        IFEQ      '?'
704800991027     C                   CLEAR                   DS7PQRS
704900941003     C                   MOVEL     'S'           DS7RIC
705000941003     C                   MOVEL     'V'           DS7GES
705100941003     C                   MOVEL     V4CGCA        DS7COD
705200991027     C                   MOVEL     DS7PQRS       KPJBU
705300960126     C                   CALL      'TRTB72R'
705400941003     C                   PARM                    KPJBA
705500940923     C                   MOVEL     *BLANKS       V4IGCA
705600940923     C                   MOVEL     'S'           WRIEM
705700940923     C                   ENDIF
705800940923     C*
705900940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
706000940923     C     V4IGGA        IFEQ      '?'
706100991027     C                   CLEAR                   DS7PQRS
706200941003     C                   MOVEL     'S'           DS7RIC
706300941003     C                   MOVEL     'V'           DS7GES
706400941003     C                   MOVEL     V4CGGA        DS7COD
706500991027     C                   MOVEL     DS7PQRS       KPJBU
706600960126     C                   CALL      'TRTB69R'
706700941003     C                   PARM                    KPJBA
706800940923     C                   MOVEL     *BLANKS       V4IGGA
706900940923     C                   MOVEL     'S'           WRIEM
707000940923     C                   ENDIF
707100940923     C*
707200940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
707300940923     C     V4IGMA        IFEQ      '?'
707400991027     C                   CLEAR                   DS7PQRS
707500941003     C                   MOVEL     'S'           DS7RIC
707600941003     C                   MOVEL     'V'           DS7GES
707700941003     C                   MOVEL     V4CGMA        DS7COD
707800991027     C                   MOVEL     DS7PQRS       KPJBU
707900960126     C                   CALL      'TRTB70R'
708000941003     C                   PARM                    KPJBA
708100940923     C                   MOVEL     *BLANKS       V4IGMA
708200940923     C                   MOVEL     'S'           WRIEM
708300940923     C                   ENDIF
708400940923     C*
708500940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
708600940923     C     V4IGVA        IFEQ      '?'
708700991027     C                   CLEAR                   DS7PQRS
708800941003     C                   MOVEL     'S'           DS7RIC
708900941003     C                   MOVEL     'V'           DS7GES
709000941003     C                   MOVEL     V4CGVA        DS7COD
709100991027     C                   MOVEL     DS7PQRS       KPJBU
709200960126     C                   CALL      'TRTB73R'
709300941003     C                   PARM                    KPJBA
709400940923     C                   MOVEL     *BLANKS       V4IGVA
709500940923     C                   MOVEL     'S'           WRIEM
709600940923     C                   ENDIF
709700940923      *
709800940923     C                   ENDSR
709900940321      **************************************************************************
710000940713      *     R O U T I N E      I N I Z I A L E
710100940713      **************************************************************************
710200940713     C     *INZSR        BEGSR
710300940713      *
710400940713     C     KARB          KLIST
710500940714     C                   KFLD                    V2CAAS
710600940713     C                   KFLD                    COMLNP            3 0
710700940714     C                   KFLD                    V2CNRS
710800940714     C                   KFLD                    V2CNSP
710900940713     C     KARB2         KLIST
711000940713     C                   KFLD                    ARBAAS
711100940713     C                   KFLD                    ARBLNP
711200940713     C                   KFLD                    ARBNRS
711300940713     C                   KFLD                    ARBNSP
711400971223     C     KARB4         KLIST
711500971223     C                   KFLD                    AR4AAS
711600971223     C                   KFLD                    AR4LNP
711700971223     C                   KFLD                    AR4NRS
711800971223     C                   KFLD                    AR4NSP
711900000228     C     KAR4W         KLIST
712000000228     C                   KFLD                    wAR4AAS
712100000228     C                   KFLD                    wAR4LNP
712200000228     C                   KFLD                    wAR4NRS
712300000228     C                   KFLD                    wAR4NSP
712400940713     C     KLNPX         KLIST
712500940713     C                   KFLD                    V1CAAS
712600940713     C                   KFLD                    V1CLP2
712700940713     C                   KFLD                    V1CNRS
712800940713     C                   KFLD                    V1CNSP
712900940713     C     K1ARB         KLIST
713000020910     c                   Kfld                    Wifp
713100940713     C                   KFLD                    NDC               7 0
713200940713     C                   KFLD                    NSS               3 0
713300940713     C                   KFLD                    COMLNA            3 0
713400940713     C                   KFLD                    COMAAD
713500940713     C                   KFLD                    COMMGD
713600940713     C                   KFLD                    V1CLP1
713700940713     C     K2ARB         KLIST
713800020910     c                   Kfld                    Wifp
713900940713     C                   KFLD                    NDC
714000940713     C                   KFLD                    NSS
714100940713     C                   KFLD                    COMLNA
714200940713     C                   KFLD                    COMAAD
714300940713     C                   KFLD                    COMMGD
714400940713     C     K3ARB         KLIST
714500020910     c                   Kfld                    Wifp
714600940713     C                   KFLD                    NDC
714700940713     C                   KFLD                    NSS
714800940713     C                   KFLD                    COMLNA
714900940713     C     K4ARB         KLIST
715000940713     C                   KFLD                    COMLNA
715100940713     C                   KFLD                    COMDTD
715200940713     C                   KFLD                    V1CLP1
715300940713     C     K5ARB         KLIST
715400940713     C                   KFLD                    COMLNA
715500940713     C                   KFLD                    COMDTD
715600940713     C     K6ARB         KLIST
715700940713     C                   KFLD                    V1CLP1
715800940713     C                   KFLD                    COMAAD
715900940713     C                   KFLD                    COMMGD
716000940713     C                   KFLD                    V1CLNA
716100940713     C                   KFLD                    V1CPSD
716200950405     C                   KFLD                    COMSCD
716300950405     C     K6ARB1        KLIST
716400950405     C                   KFLD                    V1CLP1
716500950405     C                   KFLD                    COMAAD
716600950405     C                   KFLD                    COMMGD
716700950405     C                   KFLD                    V1CLNA
716800950405     C                   KFLD                    V1CPSD
716900940713     C     K7ARB         KLIST
717000940713     C                   KFLD                    V1CLP1
717100940713     C                   KFLD                    COMAAD
717200940713     C                   KFLD                    COMMGD
717300940713     C     K8ARB         KLIST
717400940713     C                   KFLD                    V1CLNA
717500940713     C                   KFLD                    COMAAD
717600940713     C                   KFLD                    COMMGD
717700940713     C                   KFLD                    V1CLP1
717800940713     C                   KFLD                    V1CPSD
717900940713     C                   KFLD                    V1CSCD
718000940713     C     K9ARB         KLIST
718100940713     C                   KFLD                    V1CLNA
718200940713     C                   KFLD                    COMAAD
718300940713     C                   KFLD                    COMMGD
718400020627     c     arb7k         klist
718500020627     c                   kfld                    v1cnzd
718600020627     c                   kfld                    v1cprd
718700131111     C     ARB75K        KLIST
718800020627     c                   kfld                    v1cnzd
718900020627     c                   kfld                    v1cprd
719000131111     C                   KFLD                    $AAS
719100131111     C                   KFLD                    $MGS
719200940713     C                   KFLD                    V1CRSD
719300940713     C     AR43K         KLIST
719400020627     c                   kfld                    v1cnzd
719500020627     c                   kfld                    v1cprd
719600940713     C                   KFLD                    $AAS
719700940713     C                   KFLD                    $MGS
719800940713     C     ARB44K        KLIST
719900020627     c                   kfld                    v1cnzd
720000020627     c                   kfld                    v1cprd
720100940713     C                   KFLD                    $AAS
720200940713     C                   KFLD                    $MGS
720300940713     C                   KFLD                    V1CRSM
720400940713     C     AR44K         KLIST
720500020627     c                   kfld                    v1cnzd
720600020627     c                   kfld                    v1cprd
720700940713     C                   KFLD                    $AAS
720800940713     C                   KFLD                    $MGS
720900971223     C     KAR4          KLIST
721000971223     C                   KFLD                    WTRC
721100971223     C                   KFLD                    WN30
721200010508     C     KAR44         KLIST
721300010508     C                   KFLD                    WTRC
721400010508     C                   KFLD                    WN14
721500940713     C     KART          KLIST
721600940713     C                   KFLD                    ARBAAS
721700940713     C                   KFLD                    ARBLNP
721800940713     C                   KFLD                    ARBNRS
721900940713     C                   KFLD                    ARBNSP
722000940713     C                   KFLD                    V1CSCD
722100940713     C*
722200940713     C     KTAB1         KLIST
722300940713     C                   KFLD                    CODUT
722400940713     C                   KFLD                    COD
722500940713     C                   KFLD                    KEY
722600940713     C     KTAB2         KLIST
722700940713     C                   KFLD                    CODUT
722800940713     C                   KFLD                    COD
722900940713     C*
723000940907     C     KLBL1         KLIST
723100940713     C                   KFLD                    LBLAAN
723200940713     C                   KFLD                    LBLLPN
723300940713     C                   KFLD                    LBLNRN
723400940713     C                   KFLD                    LBLNSN
723500940713     C     KLBL2         KLIST
723600940713     C                   KFLD                    LBLAAP
723700940713     C                   KFLD                    LBLLPP
723800940713     C                   KFLD                    LBLNRP
723900940713     C                   KFLD                    LBLNSP
724000940907     C     KLBL3         KLIST
724100940907     C                   KFLD                    KAAS
724200940907     C                   KFLD                    KLNP
724300940907     C                   KFLD                    KNRS
724400940907     C                   KFLD                    KNSP
724500940713     C*
724600940719     C     KAR41         KLIST
724700940719     C                   KFLD                    KAAS
724800940719     C                   KFLD                    KLNP
724900940719     C                   KFLD                    KNRS
725000940719     C                   KFLD                    KNSP
725100940802     C                   KFLD                    KTRC
725200951220     C     KAR42         KLIST
725300951220     C                   KFLD                    KAAS
725400951220     C                   KFLD                    KLNP
725500951220     C                   KFLD                    KNRS
725600951220     C                   KFLD                    KNSP
725700021213     C*
725800021213     C     KAR51         KLIST
725900021213     C                   KFLD                    KAAS
726000021213     C                   KFLD                    KLNP
726100021213     C                   KFLD                    KNRS
726200021213     C                   KFLD                    KNSP
726300021213     C                   KFLD                    KTRd              3
726400940719     C*
726500991026     C     KAR61         KLIST
726600940713     C                   KFLD                    KAAS
726700940713     C                   KFLD                    KLNP
726800940713     C                   KFLD                    KNRS
726900940713     C                   KFLD                    KNSP
727000991026     C                   KFLD                    KTRC
727100991026     C*
727200051128     C     Ksped         KLIST
727300991026     C                   KFLD                    KAAS
727400991026     C                   KFLD                    KLNP
727500991026     C                   KFLD                    KNRS
727600991026     C                   KFLD                    KNSP
727700940713     C*
727800990715     C     KGCA1         KLIST
727900941202     C                   KFLD                    KAAS
728000940713     C                   KFLD                    KLNP
728100940713     C                   KFLD                    KNRS
728200940713     C                   KFLD                    KNSP
728300940713     C*
728400990805     C     KEVB1         KLIST
728500940713     C                   KFLD                    KAAS$
728600940713     C                   KFLD                    KLNP
728700940713     C                   KFLD                    KNRS
728800940713     C                   KFLD                    KNSP
728900131114     C     Kcet          KLIST
729000140224     C                   KFLD                    pctfgs
729100131114     C                   KFLD                    pctNDC
729200131114     C                   KFLD                    arbpdc
729300131114     C                   KFLD                    TTRD              3
729400131114     C                   KFLD                    arbAAS
729500131114     C                   KFLD                    arbLNP
729600131114     C                   KFLD                    arbNRS
729700131114     C                   KFLD                    arbNSP
729800950324     C*
729900020906     C     KFVV1         KLIST
730000950324     C                   KFLD                    WNPG
730100950327     C                   KFLD                    WNDC
730200020906     c                   Kfld                    Wfgs
730300131119     C     KDST          KLIST
730400131119     C                   KFLD                    WNPG
730500131119     C                   KFLD                    kndc6
730600131119     c                   Kfld                    Wfgs
730700021203
730800021203     C     Kfapd         klist
730900021203     C                   kfld                    kpdtip
731000021203     C                   kfld                    kpdpdr
731100131108     c
731200131108     c     kbrv          klist
731300131108     C                   KFLD                    WNPG
731400131108     C                   KFLD                    knfv
731500131108     c                   Kfld                    Wfgs
731600131108     c                   Kfld                    artfls
731700131108     c                   Kfld                    artlna
731800131108     c                   Kfld                    artnrs
731900131108     c                   Kfld                    artnsc
732000131111     C     KarS          KLIST
732100131111     C                   KFLD                    V1CNOT
732200131111     C                   KFLD                    KKTRC             1
732300131111     C                   MOVE      'C'           KKTRC
732400131111     C     KarSXT        KLIST
732500131111     C                   KFLD                    arSFLS
732600131111     C                   KFLD                    arSLNA
732700131111     C                   KFLD                    arSNRS
732800131111     C                   KFLD                    arSNSC
732900131111     C     KartXP        KLIST
733000131111     C                   KFLD                    arTAAS
733100131111     C                   KFLD                    arTLNP
733200131111     C                   KFLD                    arTNRS
733300131111     C                   KFLD                    arTNSP
733400040218      *
733500040218      * File FITGD02L
733600040218     c     K02TGD02      klist
733700040218     c                   kfld                    TGDtor
733800040218     c                   kfld                    TGDogg
733900030527     C*
734000030527     C* Chiave su TNTBE01L - Parziale
734100030527     C     KTBE01P       KLIST
734200030527     C                   KFLD                    TBECOD
734300030527     C                   KFLD                    TBEKE1
734400940713     C*
734500940713     C*-------------------------------------------------------------------------
734600940713     C*
734700991222     C     *LIKE         DEFINE    ARBAAS        WAAS
734800991027     C     *LIKE         DEFINE    ARBLNP        WLNP
734900991027     C     *LIKE         DEFINE    ARBNRS        WNRS
735000991027     C     *LIKE         DEFINE    ARBNSP        WNSP
735100991027     C     *LIKE         DEFINE    FVVNPG        WNPG
735200991027     C     *LIKE         DEFINE    FVVNFV        WNDC
735300131108     C     *LIKE         DEFINE    brvNFV        knfv
735400991027     C     *LIKE         DEFINE    V1CSCD        COMSCD
735500991027     C     *LIKE         DEFINE    COMC08        SAVC08
735600971219     C     *LIKE         DEFINE    ARBAAS        KAAS
735700990805     C     *LIKE         DEFINE    EVBAAS        KAAS$
735800940713     C     *LIKE         DEFINE    ARBLNP        KLNP
735900940713     C     *LIKE         DEFINE    ARBNRS        KNRS
736000940713     C     *LIKE         DEFINE    ARBNSP        KNSP
736100961112     C     *LIKE         DEFINE    ARBFBR        WFBR
736200940713     C     *LIKE         DEFINE    TBLCOD        COD
736300940713     C     *LIKE         DEFINE    TBLKEY        KEY
736400940713     C     *LIKE         DEFINE    ARBAAS        $AAS
736500940713     C     *LIKE         DEFINE    ARBMGS        $MGS
736600940719     C     *LIKE         DEFINE    AR4TRC        KTRC
736700940907     C     *LIKE         DEFINE    LBLAAN        COMAAN
736800971223     C     *LIKE         DEFINE    AR4TRC        WTRC
736900060130     C*-------------------------------------------------------------------------
737000060130     c
737100060130     c                   movel     kpjbu         savkpjbu
737200060130      *
737300060130     c     *dtaara       define    §azute        azuteds
737400060130     c     *dtaara       define    §datiute      ddatiute
737500060130     c                   in(E)     *dtaara
737600060130    1c                   If        %error  or RSUT = *blanks
737700060130     c                   Clear                   Tibs34ds
737800060130     c                   Call      'TIBS34R'
737900060130     c                   Parm                    Tibs34ds
738000060130     c                   In        *dtaara
738100060130    1c                   EndIf
738200060130
738300060130     c                   Clear                   wabi
738400060130     c                   Clear                   v1cmsg
738500060130     c                   Clear                   dLat
738600060130
738700060130      * Verifica errori e autorità profilo
738800060130s   1c                   Select
738900060130      * se ho errori nei dati utente esco dal pgm
739000060130w   1c                   When      DutErr = 'E'
739100060130     c                   GoTo      Fine
739200060130      * se non c'è l'abilitazione
739300060130      * --> se 1° livello, abilitazioni al terminal
739400060130      *     se 2° livello, abilitazioni al punto operativo
739500060130      *     se sede è impossibile ma se capita mando a fine il pgm
739600060130w   1c                   When      UteAut = *Blanks
739700060130if  2c                   If        DutLpo = '1'
739800060130     c                   Eval      wabi   = 'TP'
739900060130e   2c                   EndIf
740000060130if  2c                   If        DutLpo = '2'
740100060130     c                   Eval      wabi   = 'PO'
740200060130e   2c                   EndIf
740300060130if  2c                   If        DutLpo = 'S'
740400060130     c                   GoTo      Fine
740500060130e   2c                   EndIf
740600060130      * carica le abilitazioni del profilo
740700060130x   1c                   Other
740800060130     c                   Movel     UteFaf        Dute01
740900060131     c                   If        §Utembf <> *Blanks
741000060131     c                   Eval      wabi = §Utembf
741100060130     c                   Else
741200060130     c                   Eval      wabi = UteAut
741300060130     c                   EndIf
741400060130e   1c                   EndSl
741500060130
741600060130      * controllo se ok l'abilitazione dell'utente
741700060130     c                   Clear                   Tibs02ds
741800060130     c                   Eval      T02Mod = 'C'
741900060130     c                   Eval      T02Sif = knsif
742000060130     c                   Eval      T02Cod = 'LAT'
742100060130     c                   Movel(p)  wabi          T02Ke1
742200060130     c                   Call      'TIBS02R'
742300060130     c                   Parm                    kpjba
742400060130     c                   Parm                    Tibs02ds
742500060130if  1c                   If        T02Err = *Blanks
742600060130     c                   Eval      dLat = T02Uni
742700060130e   1c                   EndIf
742800060130      * errore o non abilitato
742900060130if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
743000060130     c                   Seton                                        28
743100060130     C                   movel     msg(26)       v1cmsg
743200060130   x1c                   Else
743300060130      * Reperimento dei P.O. gestibili dall'utente
743400060130     c                   clear                   TRUL31DS
743500060130     c                   clear                   TRUL31DS2
743600060130     c                   eval      I31abi = wabi
743700060130     c                   eval      I31cdi = DUTdis
743800060130     c                   eval      I31car = DUTare
743900060130     c                   eval      I31cpo = DUTpou
744000060130     c                   call      'TRUL31R'
744100060130     c                   parm                    KPJBA
744200060130     c                   parm                    TRUL31DS
744300060130     c                   parm                    TRUL31DS2
744400060130if  2c                   if        O31pog > *zeros
744500060130     c                   movea     O31pog        $pog
744600060130     c                   movea     O31arg        $arg
744700060130     c                   movea     O31dig        $dig
744800060130x   2c                   else
744900060130     c                   Seton                                        28
745000060130     c                   movel     MSG(26)       v1cmsg
745100060130e   2c                   endif
745200060130    1c                   endif
745300060130     c
745400060130     c                   movel     savkpjbu      kpjbu
745500971223     C*
745600971223     C* IMPOSTAZIONE CAMPI FISSI
745700940907     C                   MOVEL     *ZERO         COMLPN            3
745800940907     C                   MOVEL     *ZERO         COMNRN            2
745900940907     C                   MOVEL     *ZERO         COMNSN            7
746000950307     C                   MOVEL     *ZERO         COMLAN            3
746100950324     C                   Z-ADD     4             WNPG
746200940713      *
746300940713     C                   Z-ADD     *ZERO         NR2               4 0
746400940713     C                   Z-ADD     *ZERO         NR6               4 0
746500940713     C                   Z-ADD     *ZERO         NR8               4 0
746600940802     C                   MOVEL     *BLANKS       WF03              1
746700070403     c
746800070403     c* Verifico se profilo utente ha GEO attiva
746900131205     c* Per EDP sempre anche prr RAC999
747000131205     c                   if        %subst(knmus:1:3)='EDP' or
747100131205     c                             %subst(knmus:1:6)='RAC999'
747200070410     c                   eval      dutgeot='S'
747300070410     c                   else
747400070403     c     dutpou        chain     azorg01l
747500070403     c                   if        %found(azorg01l)
747600070403     c                   movel     orgde8        og148
747700070403     c                   else
747800070403     c                   clear                   og148
747900070403     c                   endif
748000070410     c
748100070524     c                   if        §oggeot<>' ' or §ogtlls<>' '
748200070524     c                   movel     'S'           dutgeot           1
748300070410     c                   endif
748400070524     c                   endif
748500131007
748600131007      /free
748700131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
748800131007      /end-free
748900140319     c* Tasto funz orari servizi sempre abilitato
749000140319     c                   eval      wabif9='S'
749100131007
749200940713      *
749300940713     C                   ENDSR
749400160115     C**
749500971223     C* ELABORAZIONE DI SINGOLA BOLLA --------------------------------*
749600971223     C     SINGOL        BEGSR
749700971223     C                   CLEAR                   WINIZ
749800971223     C                   CLEAR                   WFINE
749900971223     C*
750000971223     C* 97 ON - MANUTENZIONE BOLLA
750100060130    1C     *IN97         IFEQ      *ON
750200971223     C*
750300160115     c                   if        wchainblp='S'
750400160115     c     blpnrr        chain     fnblp00f
750500160115     c                   endif
750600060224     c                   movel     arblna        w003a
750700060224     c     w003a         lookup    $pog                                   31
750800060130     c* Se la causale variazione bolle prevede anche il p.o. cliente
750900131111     c*    controllo anche questo
751000060130    2c                   if        not *in31 and d66pocl='S'
751100060130     c     arbcbo        lookup    cbodop                                 31
751200060130    3c                   if        not *in31
751300060130     c                   movel     arbksc        kscfil
751400060130   x3c                   else
751500060130     c* Se bolla doppia prendo il ksc dall'AR6
751600060130     C                   Z-ADD     ARBAAS        KAAS
751700060130     C                   Z-ADD     ARBLNP        KLNP
751800060130     C                   Z-ADD     ARBNRS        KNRS
751900060130     C                   Z-ADD     ARBNSP        KNSP
752000060130     C                   MOVEL     '2'           KTRC
752100060130     C     KAR61         CHAIN     FIAR6000                           31
752200060130if  4C                   IF          %FOUND(fiar601l)
752300060130     c                   movel     ar6ksc        kscfil
752400060130     c                   else
752500060130     c                   movel     999           kscfil
752600060130    4c                   endif
752700060130    3c                   endif
752800060130     c
752900060224     c                   movel     kscfil        w003a
753000060224     c     w003a         lookup    $pog                                   31
753100060130    2c                   endif
753200060130     c
753300060130     C     *IN31         IFEQ      *OFF
753400971223     C                   MOVEL     MSG(19)       V1CMSG
753500971223     C                   SETON                                        459028
753600971223     C                   GOTO      FOR01
753700971223     C                   ELSE
753800971223     C*
753900991027     C                   CLEAR                   FNLR48DS
754000060224     c* come p.o. gestione prendo il p.o. gestore (da £6) della linea
754100060224     c*  di arrivo
754200060224     c                   clear                   fnlv55ds
754300060224     c                   movel     arblna        d55lin
754400060224     c                   movel     '6'           d55tpt
754500060224     C                   time                    wrkoggiiso
754600060224     C     *iso          move      wrkoggiiso    d55drf
754700060224     c                   call      'FNLV55R'
754800060224     c                   parm                    fnlv55ds
754900060224     c                   movel     d55tfa        d48fgs
755000060224     c
755100131203     c                   select
755200140117     C     v1cbarcode    whengt    0
755300140117     C                   MOVEL     ARbAAS        D48AAS
755400140117     C                   MOVEL     ARbLNP        D48LNP
755500140117     C                   MOVEL     ARbNRS        D48NRS
755600140117     C                   MOVEL     ARbNSP        D48NSP
755700131203     C     SEL2          whenne    0
755800980403     C                   MOVEL     V1CAAS        D48AAS
755900980403     C                   MOVEL     V1CLP2        D48LNP
756000980403     C                   MOVEL     V1CNRS        D48NRS
756100980403     C                   MOVEL     V1CNSP        D48NSP
756200131203     C                   other
756300111011     C*                  MOVEL     AR4AAS        D48AAS
756400111011     C*                  MOVEL     AR4LNP        D48LNP
756500111011     C*                  MOVEL     AR4NRS        D48NRS
756600111011     C*                  MOVEL     AR4NSP        D48NSP
756700111011     C                   MOVEL     v2cAAS        D48AAS
756800111011     C                   MOVEL     v2clnp        D48LNP
756900111011     C                   MOVEL     v2cNRS        D48NRS
757000111011     C                   MOVEL     v2cNSP        D48NSP
757100131203     C                   ENDsl
757200141010     c* routine per verifica dispo da esaminare
757300141010     c                   exsr      sr_wdwdc
757400141010
757500971223     C                   MOVEL     V1CCVB        D48CVB
757600971223     C                   MOVEL     ARBCBO        D48CBO
757700971223     C                   MOVEL     'V'           D48FFR
757800971223     C                   MOVEL     'A'           D48TBO
757900971223     C                   MOVEL     'S'           D48F12
758000991027     C                   MOVEL     FNLR48DS      KPJBU
758100971223     C*
758200971223     C                   CALL      'FNLR48R'
758300971223     C                   PARM                    KPJBA
758400971223     C                   PARM                    DSARBD
758500971223     C                   PARM                    DSARBK
758600991011     C                   PARM                    DARBT
758700971223     C                   PARM                    DSARBG
758800030612     C                   PARM                    trul90ds
758900971223     C*
759000991027     C                   MOVEL     KPJBU         FNLR48DS
759100060307     c                   movel     'S'           wriem
759200971223     C* ERRORE
759300971223     C     D48ERR        IFEQ      '1'
759400971223     C                   MOVEL     D48MSG        V1CMSG
759500971223     C                   SETON                                        284790
759600971223     C                   GOTO      FOR01
759700971223     C                   ENDIF
759800971223     C                   ENDIF
759900971223     C*
760000971223   X2C                   ELSE
760100131111   c
760200971223     C* VISUALIZZAZIONE BOLLA
760300131111     c                   select
760400131111     C     v1cbarcode    whengt    0
760500131111     C                   MOVEL     arbAAS        V2CAAS
760600131111     C                   MOVEL     arblnp        COMLNP
760700131111     C                   MOVEL     arbNRS        V2CNRS
760800131111     C                   MOVEL     arbNSP        V2CNSP
760900140117     C     SEL2          whengt    0
761000140117     C                   MOVEL     V1CAAS        V2CAAS
761100140117     C                   MOVEL     V1CLP2        COMLNP
761200140117     C                   MOVEL     V1CNRS        V2CNRS
761300140117     C                   MOVEL     V1CNSP        V2CNSP
761400131111     C                   ENDSL
761500131111     c
761600971223     C                   EXSR      BOLLA
761700971223     C*
761800971223     C                   EXSR      VISUAL
761900971223    2C                   ENDIF
762000971223     C*
762100971223    2C     PA1FLG        IFNE      '1'
762200971223     C     PA1FLG        ANDNE     'N'
762300971223     C     PA1FLG        ANDNE     '3'
762400971223     C     PA1FLG        ANDNE     'P'
762500971223     C     PA1FLG        ANDNE     'E'
762600990129     C     PA1FLG        ANDNE     'C'
762700971223     C                   MOVEL     '1'           WINIZ             1
762800971223     C                   ELSE
762900971223     C                   MOVEL     '1'           WFINE             1
763000971223    2C                   END
763100971223    2C                   ENDSR
763200110601      **************************************************************************
763300110601      *   emissione window email destinatario
763400110601      **************************************************************************
763500110601     C     sr_emdest     BEGSR
763600110601     c                   clear                   v4cemw
763700110601     c
763800110601     c                   eval      ilv79TLA='L'
763900110601     c                   eval      ilv79Aas=arbaas
764000110601     c                   eval      ilv79Lnp=arblnp
764100110601     c                   eval      ilv79nrs=arbnrs
764200110601     c                   eval      ilv79nsp=arbnsp
764300110601     c                   call      'FNLV79R'
764400110601     c                   parm                    fnlv79ds
764500110601     c
764600110601     c                   endsr
764700091214      **************************************************************************
764800131114      *   Emissione del Beneficiario C/Ass
764900091214      **************************************************************************
765000091214     C     sr_bencas     BEGSR
765100091215     c                   clear                   vidben
765200091214     c                   clear                   wdaben
765300091214     c                   clear                   wdasrsc
765400091214     c                   clear                   wdascdi
765500091216     c                   clear                   trulintds
765600091216     c
765700091216     c                   eval      iinttip='B'
765800091216     c                   eval      iinttic=vidtic
765900091216     c                   eval      iintrscm=v4crsm
766000091216     c                   eval      iintrsco=vidrmo
766100091216     c                   eval      iintCDI =w_cdi
766200091216     c                   call      'TRULINTR'
766300091216     c                   parm                    trulintds
766400091216     c
766500091216     c                   movel     ointrsci      wdasrsc
766600091216     c
766700091216     c                   select
766800091216     c                   when      ointtipi='O'
766900091216     c                   eval      wdaben='MITTENTE  ORIGINALE'
767000091216     c
767100091216     c                   when      ointtipi='R'
767200091216     c                   eval      wdascdi=w_cdi
767300091216     c                   eval      wdaben=' O R D I N A N T E'
767400091216     c
767500091216     c                   when      ointtipi='M'
767600091216     c                   if        v3cksc>0
767700091216     c                   movel     v3cksc        wdascdi
767800091216     c                   endif
767900091216     c                   eval      wdaben='  M I T T E N T E'
768000091216     c                   ENDSL
768100091214     c
768200131107     c                   exfmt     LRI6wdas
768300091214     c                   endsr
768400130830      **************************************************************************
768500130830      *   window per visualizzare gli orari servizi
768600130830      **************************************************************************
768700130830     C     sr_OrariSr    BEGSR
768800130920
768900140110     c                   clear                   trulorSds
769000151019     c***                eval       s_IOREtfp = ARBtfp
769100151019     c* se il campo "terminal didisguido" è pieno, passo questo al posto
769200151019     c* del terminal di partenza originale
769300151019     c                   if         s_arbflp>*zeros
769400151019     c                   eval       s_IOREtfp=%INT(s_arbflp)
769500151019     c                   else
769600151019     c                   eval       s_IOREtfp=arbtfp
769700151019     c                   endif
769800140110     c                   eval       s_IOREtfa = ARBtfa
769900140110     c                   eval       s_IOREdti = ARBdti
770000140110     c                   eval       s_IOREhti = ARBhti
770100140110     c                   eval       s_IOREdcr = ARBdcr
770200140110     c                   eval       s_IOREhcr = ARBhcr
770300140110     c                   eval       s_IOREtcr = ARBtcr
770400140110     c                   eval       s_IOREDEI = s_ARBdei
770500140110     c                   eval       s_IOREOEI = s_ARBoei
770600140110     c                   eval       s_IOREfil = ARBlna
770700140110     c                   eval       s_IOREcap = ARBcad
770800140110     c                   eval       s_IOREloc = ARBlod
770900140110     c                   eval       s_IOREnar = ARBnzd
771000150724     c                   clear                   diore01
771100150728     c                   movel     s_arbttc      §ioretrazC
771200150728     c                   movel     s_arbtcc      §ioreconsC
771300150724     c                   eval       s_IOREflo = diore01
771400150724     c
771500140113     c* Data di Riferimento: data consegna se consegnata
771600140113     c                   select
771700140113     c                   when      arbdcm>0 and arbica<>' ' and arbica<>'R'
771800140113     c                             and arbicc<>' ' and arbicc<>'R'
771900140113     c                   eval       s_ioredta=arbdcm
772000140113     c* per ritorno all'incasso data del giorno
772100140113     c                   when      arbdcm>0 and arbddc>0
772200140113     C                   time                    wrkoggiiso
772300140113     C     *iso          move      wrkoggiiso    s_ioredta
772400140113     c                   other
772500140113     c* altrimenti data prevista consegna se > = oggi
772600140113     C                   time                    wrkoggiiso
772700140113     C     *iso          move      wrkoggiiso    s_ioredta
772800140113     c* se < di oggi, imposto oggi
772900140113     c                   if        amgdce>s_ioredta
773000140113     c                   eval       s_ioredta=amgdce
773100140113     c                   endif
773200140113     c                   endsl
773300140113     c
773400140110     c                   eval       s_IOREtser = 'C'
773500140409     c                   eval       s_IOREDDC=arbddc
773600140409     c                   eval       s_IOREnDC=arbndc
773700140409     c
773800140110     c                   IF         *in19 or
773900140110     c                              (ARBfbc = *blanks and
774000140110     c                               ARBddc > 0 and
774100140110     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
774200140110     c                               arbica='R' or arbicc=' ' or arbicc='R'))
774300140110     c                   eval       s_IOREcons = 'S'
774400140225     c* Orario presunto ritiro/consegna
774500140225     c                   move      arbndc        pctndc
774600140225     c                   move      arbifp        pctfgs
774700140225     c                   move      'COR'         ttrd
774800140225     c     kcet          setgt     fipct02l
774900140225     c     kcet          readpe    fipct02l
775000140310     c                   if        not %eof(fipct02l)
775100140225     c                   movel     pctdati       fipctcords
775200140307     c                   eval      s_ioreorp=§PCTORASTI
775300140225     c                   endif
775400140225     c
775500140110     c                   ENDIF
775600140225     c
775700140110     c                   eval       s_IOREtsp = ARBtsp
775800130920
775900140325     c                   eval      s_iorecol=36
776000140110     c                   call      'TRULORSR'
776100130920     c                   parm                    kpjba
776200140110     c                   parm                    trulorSds
776300130830     c
776400131111     c                   ENDSR
776500131111      **************************************************************************
776600131111      *   Prepara SQL per rsd ind lod rsm
776700131111      **************************************************************************
776800131111     C     Presql        BEGSR
776900131111     //free
777000160113      // per ora non lo faccio più  eseguire il join per problemi di prestazione
777100160113      //  la ricerca viene fatta solo sulla ragione sociale di ARB primi 35 caratteri
777200160113      //   if v1cxrsd='X'  ;
777300131114
777400160113      //   wSQL ='SELECT fnarb00f.*  +
777500160113      //             from FNARB00F left outer join FIAR400F on arbaas=ar4aas +
777600160113      //         and arblnp=ar4lnp and arbnrs=ar4nrs and arbnsp=ar4nsp  +
777700160113      //          and ar4trc=''D'' where arbnzd='''+ v1cnzd +''' '  ;
777800160113      //          else  ;
777900131114
778000131114         wSQL ='SELECT * from FNARB00F   where arbnzd='''+ v1cnzd +''' '  ;
778100160113      //   endif  ;
778200131114
778300131114       if v1cnzd=*blanks   ;
778400131114         wSQL +=' and arbprd=''' + v1cprd + ''' '   ;
778500131111       endif  ;
778600131111
778700131115       if (v1cssp=' ' or v1cssp='1')  ;
778800131115        if comdtd>0 and comdta>comdtd  ;
778900131115           wSQL += ' and (arbaas*10000)+arbmgs>=' + %editc(comdtd:'X')+ ' and +
779000131115                        (arbaas*10000)+arbmgs<=' + %editc(comdta:'X')  ;
779100131115        endif  ;
779200131115        if comdtd>0 and (comdta=0 or comdta=comdtd)      ;
779300131115           wSQL += 'and arbaas=' + %editc(comaad:'X')+ ' and arbmgs=' +
779400131115                                %editc(commgd:'X')  ;
779500131115        endif  ;
779600131115       endif  ;
779700131111
779800131111       // se voglio le "IN CONSEGNA", parzializzo
779900131111       if v1cssp='2'  ;
780000131113           wSQL += ' and (arbdcm=0 or arbica='' '' or arbica=''R'' +
780100131113                     or arbicc='' '' or arbicc=''R'')'  ;
780200131111       endif  ;
780300131114
780400131114       // Se voglio le "DA CONSEGNARE", parzializzo
780500131111       if v1cssp='1'  ;
780600131111           wSQL += ' and arbifp=0 and arbndc=0 and arbnss=0' ;
780700131111       endif  ;
780800131111
780900131114       // Se voglio le "CONSEGNATE", parzializzo
781000131111       if v1cssp='3'  ;
781100131111           wSQL += ' and arbdcm>0' ;
781200131111       endif  ;
781300131111
781400131111       // linea di arrivo
781500131111       if v1clna>0  ;
781600131111           wSQL += ' and arblna=' + %editc(v1clna:'X') ;
781700131111       endif  ;
781800131111
781900131111        if v1crsd<>*blanks  ;
782000131111
782100131111         if v1cxrsd =' '   ;
782200131114           wSQL += ' and arbrsd    like(''';
782300131111         else  ;
782400160113           wSQL += ' and arbrsd    like(''%';
782500160113          // non esegue più il joi
782600160113          //  wSQL += ' and concat(arbrsd, ifnull(ar4not, '' '')) like(''%'  ;
782700131111         endif ;
782800131111
782900131209         w060a=v1crsd  ;
783000131209         exsr APICI  ;
783100131209
783200131209           wSQL += w060a ;
783300131111           wSQL  = %trim(wSQL) + '%'')' ;
783400131111         endif ;
783500131111
783600131111        if v1cIND<>*blanks  ;
783700131111
783800131111         if v1cxind =' '   ;
783900131111           wSQL += ' and arbind like(''';
784000131111         else  ;
784100131113           wSQL += ' and arbind like (''%' ;
784200131111         endif ;
784300131111
784400131209         w060a=v1cind  ;
784500131209         exsr APICI  ;
784600131209           wSQL += w060a ;
784700131111           wSQL  = %trim(wSQL) + '%'')' ;
784800131111         endif ;
784900131111
785000131111        if v1cLOD<>*blanks  ;
785100131111
785200131111         if v1cxlod =' '   ;
785300131111           wSQL += ' and arblod like(''';
785400131111         else  ;
785500131113           wSQL += ' and arblod like (''%' ;
785600131111         endif ;
785700131111
785800131209         w060a=v1clod  ;
785900131209         exsr APICI  ;
786000131209           wSQL += w060a ;
786100131111           wSQL  = %trim(wSQL) + '%'')' ;
786200131111         endif ;
786300131111
786400131111        if v1cRSM<>*blanks  ;
786500131111
786600131111         if v1cxrsm =' '   ;
786700131111           wSQL += ' and arbrsm like(''';
786800131111         else  ;
786900131113           wSQL += ' and arbrsm like (''%' ;
787000131111         endif ;
787100131111
787200131209         w060a=v1crsm  ;
787300131209         exsr APICI  ;
787400131209           wSQL += w060a ;
787500131115           wSQL  = %trim(wSQL) + '%'')' ;
787600131111         endif ;
787700131115
787800131115           wSQL += ' order by arbnzd, arbprd, arbaas, arbmgs'  ;
787900131111
788000131111        ENDSR  ;
788100131111       //--------------------------------------------------------------
788200131111       //?Apertura cursore.                                            ?
788300131111       //--------------------------------------------------------------
788400131111       BEGSR  OpenCursor;
788500131111
788600131111         wEnd = *off;
788700131111
788800131111       //?Dichiarazione cursore?
788900131111         exec sql
789000131111         PREPARE S1 from :wSQL;
789100131209
789200131209         IF sqlcode < 0;
789300131209           wEnd = *on;
789400131209           leavesr        ;
789500131209         ENDIF;
789600131209
789700131111         exec sql
789800131111         DECLARE ARB  cursor for S1;
789900131111
790000131111       //?Apertura del cursore
790100131111         exec sql
790200131111         OPEN ARB;
790300131111
790400131111         IF sqlcode < 0;
790500131111           wEnd = *on;
790600131209           leavesr        ;
790700131111         ENDIF;
790800131209
790900131209           WopenC='1'     ;
791000131209
791100131111
791200131111       ENDSR;
791300131209       //--------------------------------------------------------------
791400131209       //?Raddoppio apici nella stringa                                ?
791500131209       //--------------------------------------------------------------
791600131209       BEGSR  Apici     ;
791700131209       ap=1  ;
791800131209       dow ap <=60  ;
791900131209       if %subst(w060a:ap:1)=''''  ;
792000131209       w060a=%replace('''''':w060a:ap:1)  ;
792100131209       ap= ap   +1       ;
792200131209       endif  ;
792300131209
792400131209       ap= ap   +1       ;
792500131209       enddo  ;
792600131209
792700131209       ENDSR;
792800131111       //--------------------------------------------------------------
792900131111       //?Chiusura cursore.
793000131111       //--------------------------------------------------------------
793100131111       BEGSR CloseCursor;
793200131111
793300131111       //?Chiusura cursore SQL?
793400131111         exec sql
793500131111           close ARB;
793600131209           clear WopenC   ;
793700131209           Wend=*off      ;
793800131111
793900131111       ENDSR;
794000131111       //--------------------------------------------------------------
794100131111       //?Lettura cursore.                                             ?
794200131111       //--------------------------------------------------------------
794300131111       BEGSR  ReadCursor;
794400131111
794500131111         clear  FNARBds;
794600131113         *in30=*off    ;
794700131113         *in32=*off    ;
794800131111
794900131111         exec sql
795000131111         FETCH next from ARB into :FNARBds;
795100131111
795200131111         IF sqlcod = 100 or sqlcod < 0;
795300131111           wEnd = *on;
795400131111         ENDIF;
795500131111
795600131111       ENDSR;
795700141010       //--------------------------------------------------------------
795800141010       //?Window per f19=Dispo di consegna da esaminare
795900141010       //--------------------------------------------------------------
796000141010       BEGSR sr_wdwdc   ;
796100141010          $fine=*off;
796200141105          clear fnlry09ds2;
796300141010          ilry09tch='C';
796400141010          ILRY09AAS=d48aas ;
796500141010          ILRY09LNP=d48LNP;
796600141010          ILRY09NRS=d48NRS ;
796700141010          ILRY09NSP=d48NSP ;
796800141105          kpjbu=fnlry09ds2;
796900141105          fnlry09r(kpjba);
797000141105          fnlry09ds2=kpjbu;
797100141010          if olry09esit='1' ;
797200141010             dow $fine=*off;
797300141010                exfmt lri6wddc;
797400141010                   if *inkt;
797500141105                       clear fnlry09ds2;
797600141010                       ilry09tch='E';
797700141010                       ILRY09AAS=d48aas ;
797800141010                       ILRY09LNP=d48LNP;
797900141010                       ILRY09NRS=d48NRS ;
798000141010                       ILRY09NSP=d48NSP ;
798100141105                       ILRY09DF06='S';
798200141105                       kpjbu=fnlry09ds2;
798300141105                       fnlry09r(kpjba);
798400141010                       $fine=*on;
798500141010                   endif;
798600141010             enddo;
798700141010          endif ;
798800141010
798900141010       ENDSR;
799000131111
799100131111
799200131111
799300131111      /end-free
799400160315       //--------------------------------------------------------------
799500160315       //?Controllo videata per manutenzione vedi pacco DPD
799600160315       //--------------------------------------------------------------
799700160316       BEGSR sr_CtrVediPDPD ;
799800160316          *in41=*off;
799900160316          *in42=*off;
800000160316          *in43=*off;
800100160316          *in44=*off;
800200160316          *in45=*off;
800300160316          *in46=*off;
800400160316          *in51=*off;
800500160316          *in52=*off;
800600160316          *in55=*off;
800700160316          *in28=*off;
800800160316          *in90=*off;
800900160316       //?Barcode Bartolini  (lettura  con pistola)
801000160316       if V1CBCODEB>0    ;
801100160316          setll (wbcodfls:wbcodlna:wbcodnrs:wbcodnsc) fnart27l    ;
801200160316          if not %equal(fnart27l);
801300160316                *in28=*on;
801400160316                *in90=*on;
801500160316                *in42=*on;
801600160316                vidmsg=err(34);
801700160316                leavesr;
801800160316          endif;
801900160316       endif;
802000160316       //?Segnacollo Bartolini (digitato a mano)
802100160316       if v1cfls>0 or v1clnas>0 or v1cnscs>0;
802200160316       // Filiale segnacollo
802300160316          chain v1cfls azorg01l;
802400160316          if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A') or
802500160316                                      orgfva<>*blanks      ;
802600160316                *in28=*on;
802700160316                *in90=*on;
802800160316                *in43=*on;
802900160316                vidmsg=err(35);
803000160316                leavesr;
803100160316          endif ;
803200160316       // linea di arrivo
803300160316          chain v1clnas azorg01l;
803400160316          if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A') or
803500160316                                      orgfva<>*blanks      ;
803600160316                *in28=*on;
803700160316                *in90=*on;
803800160316                *in44=*on;
803900160316                vidmsg=err(35);
804000160316                leavesr;
804100160316          endif ;
804200160316          setll (v1cfls:v1clnas:v1cnrss:v1cnscs) fnart27l    ;
804300160316          if not %equal(fnart27l);
804400160316                *in28=*on;
804500160316                *in90=*on;
804600160316                *in43=*on;
804700160316                vidmsg=err(34);
804800160316                leavesr;
804900160316          endif;
805000160316       endif;
805100160315       //?Parcel DPD
805200160315          if v1cnrd<>*blanks;
805300160315             if %scan(' ':v1cnrd)>0 ;
805400160316                *in28=*on;
805500160316                *in90=*on;
805600160316                *in41=*on;
805700160316                vidmsg=err(33);
805800160315                leavesr;
805900160315             endif;
806000160315       // Controllo se contiene solo numeri
806100160316       // Possono esserci lettere solo nei primi 4 byte. Dal quinto in poi devono
806200160316       // essere solo numeri
806300160316             pos=%check(digits:v1cnrd);
806400160316             if pos>4;
806500160316                *in28=*on;
806600160316                *in90=*on;
806700160316                *in41=*on;
806800160316                vidmsg=err(33);
806900160316                leavesr;
807000160316             endif;
807100160315          endif;
807200160316       //?Key spedizione
807300160316       if v1clp2>0 or v1cnsp>0;
807400160316          chain v1clp2 azorg01l;
807500160316          if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A') or
807600160316                                      orgfva<>*blanks      ;
807700160316                *in28=*on;
807800160316                *in90=*on;
807900160316                *in45=*on;
808000160316                vidmsg=err(5);
808100160316                leavesr;
808200160316          endif ;
808300160316          if v1caas=0;
808400160316                *in28=*on;
808500160316                *in90=*on;
808600160316                *in55=*on;
808700160316                vidmsg=err(7);
808800160316                leavesr;
808900160316          endif;
809000160316       endif;
809100160316       //?Data spedizione dal/al + lna
809200160316       // Data dal
809300160316          clear comdtd ;
809400160316          clear comdta ;
809500160316       if v1cdtd>0;
809600160316       if %subst(%editc(v1cdtd:'X'):1:2)='00';
809700160316          test(DE) *dmy v1cdtd;
809800160316          if %error;
809900160316             *in90=*on;
810000160316             *in51=*on;
810100160316             *in28=*on;
810200160316             vidmsg=err(8);
810300160316             leavesr;
810400160316          else;
810500160316             dataiso=%date(v1cdtd:*dmy);
810600160316          endif;
810700160316       else;
810800160316          test(DE) *eur vicdtd;
810900160316          if %error;
811000160316             *in90=*on;
811100160316             *in52=*on;
811200160316             *in28=*on;
811300160316             vidmsg=err(8);
811400160316             leavesr;
811500160316          else;
811600160316             dataiso=%date(v1cdtd:*eur);
811700160316          endif;
811800160316       endif;
811900160316          dataeur=dataiso;
812000160316          v1cdtd=%dec(dataeur);
812100160316          comdtd=%dec(dataiso);
812200160316       endif;
812300160316       // Data al
812400160316       if v1cdta>0;
812500160316       if %subst(%editc(v1cdta:'X'):1:2)='00';
812600160316          test(DE) *dmy v1cdta;
812700160316          if %error;
812800160316             *in90=*on;
812900160316             *in52=*on;
813000160316             *in28=*on;
813100160316             vidmsg=err(8);
813200160316             leavesr;
813300160316          else;
813400160316             dataiso=%date(v1cdta:*dmy);
813500160316          endif;
813600160316       else;
813700160316          test(DE) *eur vicdta;
813800160316          if %error;
813900160316             *in90=*on;
814000160316             *in52=*on;
814100160316             *in28=*on;
814200160316             vidmsg=err(8);
814300160316             leavesr;
814400160316          else;
814500160316             dataiso=%date(v1cdta:*eur);
814600160316          endif;
814700160316       endif;
814800160316          dataeur=dataiso;
814900160316          v1cdta=%dec(dataeur);
815000160316          comdta=%dec(dataiso);
815100160316       endif;
815200160316       // Se non immessa data al imposto come data dal
815300160316       if comdta=0;
815400160316          comdta=comdtd;
815500160316       endif ;
815600160316       // errore se data al minore di data dal
815700160316       if comdta<comdtd;
815800160316             *in90=*on;
815900160316             *in52=*on;
816000160316             *in28=*on;
816100160316             vidmsg=err(10);
816200160316             leavesr;
816300160316       endif;
816400160316       //?Linea di arrivo
816500160316       clear v1cdea;
816600160316       if v1clna>0;
816700160316          chain v1clna azorg01l;
816800160316          if not %found(azorg01l)  or (orgfag<>'F' and orgfag<>'A') or
816900160316                                       orgfva<>*blanks      ;
817000160316             *in28=*on;
817100160316             *in46=*on;
817200160316             vidmsg=err(2);
817300160316             leavesr;
817400160316          endif;
817500160316          v1cdea=orgdes;
817600160316       endif;
817700160316       if comdtd>0 and v1clna=0;
817800160316             *in28=*on;
817900160316             *in46=*on;
818000160316             vidmsg=err(2);
818100160316             leavesr;
818200160316       endif ;
818300160315       endsr;
818400131111
818500940713      **************************************************************************
818600941014**        12345678901234567890123456789012345678901234567890
818700950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
818800930413**
818900950316LEGATA A PIU' BOLLE                       - F18=Visualizza
819000920326**
819100950316 Non Presente
819200950316 F18=Visualizza
819300940907Vedi in Int.Par
819400940907Vedi in Int.Arr
819500941011Vedi Arch.Stor.
819600940907**
819700940907Spedizione non Presente
819800941230
819900940907Vedi in Int.Bolle Part.
820000940907Vedi in Int.Bolle Arr.
820100941011Vedi in Archiv. Storico
820200940713**  MESSAGGI DI ERRORE VIDEO  **************************************************
820300940713Nessuna bolla da visualizzare                                                  1
820400940713Immettere linea di arrivo valida                                               2
820500131114Effettuare ulteriori parzializzazioni                                          3
820600940713Immettere linea di arrivo o linea di partenza                                  4
820700940713Immettere linea di partenza valida                                             5
820800940713Numero spedizione inesistente                                                  6
820900940713Immettere anno spedizione                                                      7
821000940713Immettere data formalmente valida                                              8
821100040216Immettere tipo bolla valido                                                    9
821200940713Immettere data di spedizione iniziale minore o uguale a quella finale         10
821300940713Immettere numero di serie iniziale minore o uguale a quello finale            11
821400940713Immettere numero segnacollo iniziale minore o uguale a quello finale          12
821500940713Immettere numero segnacollo finale, se immesso quello iniziale                13
821600131114Scelta incongruente tra stato spedizioni  e richiesta                         14
821700940713Fine scorrimento                                                              15
821800940715Immettere tipo servizio bolle valido                                          16
821900940923Immettere filiale segnacollo valida                                           17
822000060614Riferimento partner estero errato x bolle DPD:o lungo 11 numerico a 14 alfanum
822100941212Bolla non manutenzionabile: in gestione ad altra filiale                      19
822200991227Il riferimento partner estero per bolle DPD deve avere massimo 12 numeri
822300010508Immettere p.o. di emissione valido                                            21
822400010510Non trovate bolle per l' O.R.M. richiesto                                     22
822500020626Immettere la nazione e la provincia del destinatario                          23
822600020627Nazione destinatario errata                                                   24
822700020627Provincia destinatario errata                                                 25
822800060130Utente non abilitato alla Variazione bolle                                    26
822900060130Utente non abilitato alla variazione bolle del p.o. in gestione               27
823000060613ATTENZIONE!Saranno selez.bolle con Nazione e RagSoc impostate ma PROVINC.vuota28
823100070420Stampa LDV effettuata ...                                                     29
823200070420Stampa LDV al momento non effettuabile: ritentare o telefonare in Sede        30
823300080201Assegnato non tassato: stampa LdV non effettuabile                            31
823400131111non trovata bolla per il barcde digitato                                      32
823500160316Numero Parcel DPD non valido                                                  33
823600160316Collo di spedizione non in arrivo                                             34
823700160316Filiale mancante o errata                                                     35
823800160316
823900160316
824000160316
824100160316
824200160316
824300030612**  cmA4
824400030612OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
824500111221 SHARE(*YES) USRDTA('Man.BolA4')
824600030612**  cmA5
824700030612OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
824800111221 SHARE(*YES) USRDTA('Man.BolA5')
824900070426**  cmA4f6
825000111221OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
825100111221USRDTA('Rist.Bolle') SECURE(*YES)
825200070426**  cmA5f6
825300070426OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
825400111221             USRDTA('Rist.Bolle') SECURE(*YES)
825500030612** cnm
825600991222 0123456789
825700040218** TF                                                                        *
825800070420F1=Agg.Rich#                                                                    1
825900070411** TF01B                                                                     *
826000070411F1=SCELTA BOLLA#                                                                1
826100021216** TF                                                                        *
826200021218F2=C.A.#                                                                        2
826300021216** TF                                                                        *
826400021216F4=Tass2#                                                                       4
826500021216** TF                                                                        *
826600021216F5=Eventi#                                                                      5
826700070420** TF                                                                        *
826800070420F6=StampaLDV#                                                                   5
826900021216** TF                                                                        *
827000021217F7=Anomalie#                                                                    7
827100021216** TF                                                                        *
827200021219F8=Discord#                                                                     8
827300130830** TF                                                                        *
827400130830F9=OrariSr#                                                                     9
827500021216** TF                                                                        *
827600070420F10=Variaz#                                                                     10
827700021216** TF                                                                        *
827800030121F11=NoteS/B#                                                                    11
827900021217** TF                                                                        *
828000021217F12=Ritorno#                                                                    11
828100021216** TF                                                                        *
828200070420F13=Tassaz#                                                                     13
828300021217** TF                                                                        *
828400021216F14=Giacenza#                                                                   14
828500040218** TF                                                                        *
828600070420F16=Gest.Rich#                                                                  16
828700070404** TF                                                                        *
828800140311F17=NoteAut#                                                                    16
828900021216** TF                                                                        *
829000021216F19=Spunte#                                                                     19
829100021216** TF                                                                        *
829200070420F20=Affid#                                                                      19
829300021216** TF                                                                        *
829400070420F21=DetColli#                                                                   19
829500110928** TF                                                                        *
829600110930F22=PerfectOrder#                                                               19
