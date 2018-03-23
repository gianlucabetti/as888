000100130314     /*PRM dbgview(*source)
000200130314     /*END
000300991105      *------------------------------------------------------------------------*
000400150312      *  Nome programma:  FNLRU6R1
000500150312      *  Descrizione   :  Interrogazione bolle arrivi  (bolla singola)
000600940712      *------------------------------------------------------------------------*
000700991105     H DECEDIT('0,') DATEDIT(*DMY/)
000800110929     h dftactgrp(*no) actgrp(*caller)
000900940712      *
001000160504     FFNLRU6D1  CF   E             WORKSTN
001100131107     F                                     SFILE(LRI6S06:NR6)
001200131107     F                                     SFILE(LRI6S08:NR8)
001300131107     F                                     SFILE(LRI6S05:NRR5)
001400131107     F                                     SFILE(LRI6S11:NRR11)
001500070404     FFIARP01L  iF   E           K DISK
001600050405     Ftigcp21l  IF   E           K DISK
001700040218     FAZORG01L  IF   E           K DISK
001800911024     FTABEL00F  IF   E           K DISK
001900021203     FFIAPD01L  IF   E           K DISK
002000940712     F*.......... B O L L E   A R R I V I
002100940712     FFNARB01L  IF   E           K DISK
002200060216     FFiAR401L  IF   E           K DISK
002300021213     FFIAR501L  IF   E           K DISK
002400991026     FFIAR601L  IF   E           K DISK
002500991026     FFIAR701L  IF   E           K DISK
002600991027     FFIARI01L  IF   E           K DISK
002700051114     FFiAR901L  IF   E           K DISK
002800940712     F*.......... D E T T A G L I O   C O L L I
002900940713     FFNART01L  IF   E           K DISK
003000940712     F*.......... L E G A M E   B O L L A
003100940713     FFNLBL01L  IF   E           K DISK
003200991027     FFNLBL02L  IF   E           K DISK    RENAME(FNLBL000:FNLBL002)
003300940712     F*.......... B O L L E   P A R T E N Z E
003400150313     FFNBLP01L  IF   E           K DISK    prefix(ARB:3)
003500940712     F*.......... V A R I A Z I O N I   B O L L E
003600131007     FFiARBF2C  IF   E           K DISK
003700990805     FFNEVB01L  IF   E           K DISK
003800950324     F*
003900020906     fFnfvv01l  if   e           k Disk
004000131119     ffidst01l  if   e           k Disk
004100990119     FFNDFA01L  IF   E           K DISK
004200040218      *.......... T E L E F O N A T E   /   R E C L A M I
004300160408     fFITGD32L  if   e           k disk
004400030527     F*.......... N U O V E   T A B E L L E
004500030527     FTNTBE01L  IF   E           K DISK
004600131108     F*.......... S P U N T E
004700131108     Ffnbrv01l  IF   E           K DISK
004800131114     F
004900131114     Ffipct02l  IF   E           K DISK
005000150617     Ffnanm01l  IF   E           K DISK
005100060130     d
005200950307     D LBL             S              1    DIM(58) CTDATA PERRCD(58)
005300950307     D LB2             S             58    DIM(1) CTDATA PERRCD(1)
005400941011     D LBD             S             15    DIM(5) CTDATA PERRCD(1)
005500941011     D PRD             S             23    DIM(5) CTDATA PERRCD(1)
005600160505     D MSG             S             78    DIM(3) CTDATA PERRCD(1)
005700030612      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
005800070426     D CMA4f6          S              1    DIM(130) CTDATA PERRCD(65)
005900070426     D CMA5f6          S              1    DIM(130) CTDATA PERRCD(65)
006000030612      *
006100040218     D TF01            s              1    dim(20) ctdata perrcd(20)
006200070411     D TF01B           s              1    dim(20) ctdata perrcd(20)
006300021216     D TF02            S              1    DIM(20) CTDATA PERRCD(20)
006400021216     D TF04            S              1    DIM(20) CTDATA PERRCD(20)
006500021216     D TF05            S              1    DIM(20) CTDATA PERRCD(20)
006600070420     D TF06            S              1    DIM(20) CTDATA PERRCD(20)
006700021216     D TF07            S              1    DIM(20) CTDATA PERRCD(20)
006800021216     D TF08            S              1    DIM(20) CTDATA PERRCD(20)
006900130830     D TF09            S              1    DIM(20) CTDATA PERRCD(20)
007000021216     D TF10            S              1    DIM(20) CTDATA PERRCD(20)
007100021216     D TF11            S              1    DIM(20) CTDATA PERRCD(20)
007200021217     D TF12            S              1    DIM(20) CTDATA PERRCD(20)
007300021216     D TF13            S              1    DIM(20) CTDATA PERRCD(20)
007400021216     D TF14            S              1    DIM(20) CTDATA PERRCD(20)
007500040218     d TF16            s              1    dim(20) ctdata perrcd(20)
007600070404     d TF17            s              1    dim(20) ctdata perrcd(20)
007700021216     D TF19            S              1    DIM(20) CTDATA PERRCD(20)
007800021216     D TF20            S              1    DIM(20) CTDATA PERRCD(20)
007900021216     D TF21            S              1    DIM(20) CTDATA PERRCD(20)
008000110928     D TF22            S              1    DIM(20) CTDATA PERRCD(20)
008001160628     D TF23            S              1    DIM(20) CTDATA PERRCD(20)
008100021217     D $TF             S              1    DIM(20)
008200911024     D*
008300930705     D KCA             S              1    DIM(50)
008400160610     D DCA             S             57    DIM(50)
008500940907     D*
008600940907     D AAN             S              4  0 DIM(30)
008700940907     D LPN             S              3  0 DIM(30)
008800940907     D NRN             S              2  0 DIM(30)
008900940907     D NSN             S              7  0 DIM(30)
009000941024     D CC1             S              1    DIM(99)
009100941024     D CC2             S             20    DIM(99)
009200020329     D CC3             S              1    DIM(99)
009300020329     D CB1             S              1    DIM(99)
009400020329     D CB2             S             20    DIM(99)
009500030121     D RigaTf1         S              1    DIM(62)
009600030121     D RigaTf2         S              1    DIM(62)
009700030121     D RigaTf3         S              1    DIM(62)
009800030527     D*
009900160505     d* PARAMETRI RICEVUTI IN ENTRATA
010000160606     D PARAMU6ds1      DS
010001160523     d** INPUT
010100940714     D  V2CAAS                14     17  0
010200940712     D  COMLNP                18     20  0
010300940714     D  V2CNRS                21     22  0
010400940714     D  V2CNSP                23     29  0
010401160523     d** OUTPUT
010500160504     D  PARF03                30     30
010501160523     d** INPUT
010600160504     D  PARFLG                31     31
010800160505     D*    SE RICH DA PGM GIAC. = 1
010900160504     D  PARGIA               144    144
010901160523     d** OUTPUT
011000160504     d  parrma               149    163                                         x F22=PerfectOrder
011100160504     d  parxco               164    164                                         x F22=PerfectOrder
011101160523     d** INPUT
011102160606     d*   Tipo lancio: "C" -           CHIUSO CON LR
011103160606     d*                "L" - ELABORO E CHIUDO CON LR
011104160606     d*                " " - ELABORO E CHIUDO IN RETRN
011500160523     d  partla               165    165
011501160523     d  parflo               166    256
011502160606     d*******************************************
011504160527     D PARAMU6ds       DS
011505160527     D  V2CAAS_               14     17  0
011506160527     D  COMLNP_               18     20  0
011507160527     D  V2CNRS_               21     22  0
011508160527     D  V2CNSP_               23     29  0
011510160527     D  PARF03_               30     30
011512160527     D  PARFLG_               31     31
011517160527     D  PARGIA_              144    144
011519160527     d  parrma_              149    163
011520160527     d  parxco_              164    164
011522160527     d  parflo_              165    256
011600160504     D* PASSAGGIO A INTERROGAZIONE VARIAZIONI           - FNLR75R -
011700161019     D* PASSAGGIO A INTERROGAZIONE DETTAGLIO COLLI      - FNLRU38R -
011800160504     D PARAMint        DS
011900160504     D  pa1AAS                14     17  0
012000160504     D  pa1LNP                18     20  0
012100160504     D  pa1NRS                21     22  0
012200160504     D  pa1NSP                23     29  0
012300160504     D  PA1F03                30     30
012400160504     D  PA1FLG                31     31
012500160504     D  PA1PGM                32     41
012600160505     D  PA1DSA                42     61
012700160505     D  PA1DSP                63     70  0
012800160505     D  PA1RMN                71     85  0
012900160505     D  PA1LNA                86     88  0
013000160505     D  PA1LN1                89    108
013100160505     D  PA1RSD               109    143
013200160504     D* SE RICH DA PGM GIAC. = 1
013300160504     D  PA1GIA               144    144
013400160505     D  PA1FEL               145    147  0
013500160505     D  PA1TBO               148    148
013501160916     D  PA1TFP               156    158  0
013502160916     D  PA1TFA               159    161  0
013503160628     D*VISUALIZZAZIONE ORM - FIOR07R
013504160628     D Parm01          DS
013505160628     D  ppoe                          3  0
013506160628     D  pnor                          7  0
013507160628     D  pnsr                          2  0
013508160628     D  pnrv                          2  0
013509160628     D  psce                          1
013510160628     D  pfgs                          3  0
013511160628     D  ppor                          3  0
013512160628     D  pdtr                          8  0
013513160628     D  pmdb                         10
013514160628     D  pprb                         10
013515160628     D  pdts                          8  0
013516160628     D  prmp                          1
013517160628     D  pbrc                          1
013518160628     D  pref                          2
013519160628     D  pmio                          1
013600940802     D* RICHIAMO PGM DI INTERROGAZIONE SPUNTE FOGLI VARI  - FNLR04R -
013700160504     D PARA04          DS                  INZ
013800940713     D  PA4NFV                 1      5  0
013900940713     D  PA4LNP                 6      8  0
014000940713     D  PA4LNA                 9     11  0
014100940713     D  PA4NRS                12     13  0
014200940713     D  PA4NCD                14     20  0
014300940713     D  PA4NCA                21     27  0
014400940713     D  PA4NPS                28     29  0
014500940713     D  PA4NPG                30     30  0
014600940713     D  PA4FLG                31     31
014700940713     D  PA4PGM                32     38
014800941230     D  PA4FGS                39     41  0
014900101201     d* bolla
015000101201     d  pa4bolp               42     44  0
015100101201     d  pa4bors               45     46  0
015200101201     d  pa4bosp               47     53  0
015300101201     d  pa4boan               54     57  0
015400101201     D  PA4ERR                58     58
015500940726     D* RICHIAMO PGM DI INTERROGAZIONE EVENTI             - FNLRB5R -
015600950215     D PARAB5          DS                  INZ
015700940726     D  PB5AAS                14     17  0
015800940726     D  PB5LNP                18     20  0
015900940726     D  PB5NRS                21     22  0
016000940726     D  PB5NSP                23     29  0
016100940726     D  PB5F03                30     30
016200940726     D  PB5FLG                31     31
016300940726     D  PB5PGM                32     41
016400940726     D  PB5DSA                42     61
016500940726     D  PB5DSP                63     70  0
016600940726     D  PB5RMN                71     85  0
016700940726     D  PB5LNA                86     88  0
016800940802     D  PB5LN1                89    108
016900940802     D  PB5RSD               109    143
017000940802     D  PB5DCP               144    151  0
017100940802     D  PB5TMC               152    152
017200131111     D
017300950428     D                 DS
017400950428     D  V3CLNP                 1      3  0
017500950428     D  V3CNRS                 4      5  0
017600950428     D  V3CNSP                 6     12  0
017700950428     D  V3CDSP                13     20  0
017800950428     D  VIDRMN                21     35  0
017900131121     D  V3CLNA                36     38  0
018000950428     D  D15LNA                39     53
018100950428     D  V32RSD                54     85
018200950428     D  COMC08                 1     85
018300131111     d
018400930705     D WLBDAT          DS
018500940712     D  G02DAT                 1      8  0
018600940712     D  G02INV                 9     16  0
018700940712     D  G02ERR                17     17
018800940712     D  G02TGI                18     22  0
018900950125     D                 DS
019000950125     D* CAMPO UNICO COMPRENDENTE RAG.SOC.+AGGIUNTIVI 35 CARATTERI
019100950125     D  V4CRSD                 1     35
019200131120     D  V4CNOT                36     70
019300131120     D  V4CNOM                 1     70
019400040218      *?definizione oggetto reclamo come spedizione?
019500131121     d ds_OggSped      ds                  inz
019600131121     d  wARBlnp                       3s 0 inz
019700131121     d  wARBnrs                       2s 0 inz
019800131121     d  wARBnsp                       7s 0 inz
019900131121     d  wARBaas                       4s 0 inz
020000040218      *
020100040218     d Status         sds
020200040218     d  SDSprm           *parms
020300991027      *
020400040216     D KPJBA         E DS
020500150402     d fnlru6ds      e ds
020600040225      *
020700040225     d FIDNA2ds      e ds                  inz
020800040225     d  IA2mod       e                     inz('M')
020900040225     d  IA2tor       e                     inz('S')
021000040216      *
021100991029     d YEURCODS      E DS
021200991027     d TIBS69DS      E DS                  INZ
021300021213     d FNLRB3Ds      E DS
021400991027     d DS_cnaco      E DS                  extname(CNACO00F)
021500991027     d DS_cnind      E DS                  extname(CNIND00F)
021600991027     d DS_cnclp      E DS                  extname(CNCLP00F)
021700991027     d DS_fncls      E DS                  extname(FNCLS00F)
021701160825     d DS_fnarb      E DS                  extname(FNARB00F)
021800991027     D DSCV          E DS
021900020402     D DSCC          E DS
022000991027     D DS$2          E DS
022100991027     D DS3A          E DS
022200951220     D DS7K          E DS
022201160610     D DS7O          E DS
022300131108     D DS2A          E DS
022400020627
022500090604     d ds5e          e ds
022600020627
022700991217     D OG143         E DS
022800070403     D OG148         E DS
022900000615     D DSBL4A        E DS
023000060518     D DSBL4I        E DS
023100150729     D Diore01       E DS                  INZ
023200150729
023300991027     D FIDN00DS      E DS
023400091216     D TRULINTDS     E DS
023500991027     D DS7PQRS       E DS
023600991027     D FNLG30DS      E DS
023700991027     D FNLr44DS      E DS
023800130920
023900131114     d fipctcetds    e ds
024000140224     d fipctcords    e ds
024100150611     d fnlru3ds      E DS
024200140110     D TRULORSDS     E DS                  prefix(S_)
024300150611     D TRULOR2DS     E DS
024400150611     D TRULOR3DS     E DS
024500131114     D TRULORFDS     E DS
024600130920
024700961121     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
024800991027     D FNLR45DS      E DS
024900950102     D FNLR97        E DS
025000930428     D*
025100930428     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
025200991027     D TNSD99DS      E DS                  INZ
025300941014      *
025400030612     D* DS PER TRUL90R - RICHIESTA STAMPANTI
025500030612     D TRUL90DS      E DS
025600060224      *
025700060224     D* DS PER FNLv55R - richiamo pgm che cerca i terminal per £6
025800060224     D fnlv55ds      E DS
025900941014      *
026000051124     D* DS PER FNLV80R - decodifica cons richiesta
026100051124     D FNLV80DS      E DS
026200131120     D* DS PER FNLRG7R - visulizza altre abilitazioni
026300131120     D fnlrg7ds      E DS
026400140123     D* DS PER FNLRN6R - visulizza note LDV
026500140123     D fnlrn6ds      E DS
026600070427     D* DS PER FIDG34R - reperisce dati consegna automatica
026700070427     D fidg34ds      E DS
026800070420     D* DS PER           stampa LDV
026900070420     D FNLSB5DS      E DS
027000130308     d FNLSB6ds1     e ds                  inz
027100130319     d  DB6pdf       e                     inz('X')
027200941212      *
027300131119     D DDSTFLR       E DS                  INZ
027400010510     D Dsbl4m        E DS
027500030121     d dTba          e ds
027600031119     d dAr5Gen       e ds
027700030121     d dAr5Ban       e ds
027800030703     d dAr5Bnb       e ds
027900130927     d dAr5emd       e ds
028000030121     d Tibs02Ds      e ds
028100060130     d Azuteds       e ds                  extname(Azute00f)
028200060130     d dDatiute      e ds
028300060130     D TIBS34DS      E DS
028400110601     D fnlv79ds      E DS
028500150615     D fnlrj6ds      E DS
028600040809      * ds per il controllo della presenza di CA per la spedizione richiesta
028700040809     d FIDN12DS      e ds
028800040809     d  skKey                 26    305    dim(20)
028900040809     d  skAnn                306    325    dim(20)
029000040809     d  skDca                326    485  0 dim(20)
029100040809     d  skFca                486    545  0 dim(20)
029200040809     d  skDch                546    705  0 dim(20)
029300040809     d  skCch                706    745    dim(20)
029400040809     d*
029500040809     d dsKey           ds
029600040809     d  dsaac                         4  0
029700040809     d  dsfil                         3  0
029800040809     d  dsnca                         7  0
029900040809
030000991027      *
030100991027     D W017A           s             17
030200991027     D w0130           s             13  0
030300991027     D w0133           s             13  3
030400991027     D IMPeditato      s             13
030500991027     D Wsvn            S                   LIKE(AR7svn)
030600991027     D Wvan            S                   LIKE(AR7van)
030700991027     D Wdiv            S                   LIKE(AR6div)
030800991029     D Wariimp         S                   LIKE(ARIimp)
030900991029     D Wariipp         S                   LIKE(ARIipp)
031000020906     d wfgs            s                   Like(Fvvfgs)
031100991027     D DATA_eur        S               D   DATFMT(*eur)
031200021203     d kpdpdr          S                   LIKE(APDpdr)
031300021203     d kpdtip          S                   LIKE(APDtip)
031400131120     d kndc6           S                   LIKE(dstnfv)
031500140110     d s_arbdei        S                   LIKE(d98dei)
031600140110     d s_arboei        S                   LIKE(d98oei)
031700150729     d s_arbttc        S                   LIKE(d98ttc)
031800150729     d s_arbtcc        S                   LIKE(d98tcc)
031900131114     D arbdsp          S              8  0
032000131111     D W0080           S              8  0
032100991027     D W0040           S              4  0
032200991027     D wtibs69r        s              1
032300991027     D wtstdec         s              1
032400131120     D Wvideo          s              1
032500020225     d lnantw          s                   like(§ogntw)
032600020225     d lnpntw          s                   like(§ogntw)
032700030703
032800030703     d wtba            s                   Like(§Ar5Tba)
032900030703     d wdestba         s                   Like(VidTba)
033000020628
033100091216     D W_cdi           S                   LIKE(§ar5cdi)
033200070420     D wrichtrul90     S              1
033300070403     d Dataeur         s               d   datfmt(*eur)
033400070403     d Datadmy         s               d   datfmt(*dmy)
033600110929     d no_f22          s              1    inz(' ')
033700131108     d Wcol_D          s              5  0
033800131108     d Wcol_I          s              5  0
033900131108     d Wcarat          s              3  0
034000131108     d Indx            s              3  0
034100131209     d wabif9          s              1
034200141010     d $Fine           s              1    inz(*off)
034201160610     d win12           s              1    inz(*off)
034300150611     d w_tbo           s              1
034301160610     d w_tbo_sav       s                   like(w_tbo)
034302160913     d blp_arbdcm      s                   like(arbdcm)
034303160913     d blp_arbdcm_sav  s                   like(arbdcm)
034304161014     d blp_arbhmc      s                   like(arbhmc)
034305161014     d blp_arbhmc_sav  s                   like(arbhmc)
034306160614     d w08dcm          s                   like(v08dcm)
034307160614     d w_evbcev        s                   like(evbcev)
034308161108     d wcev            s                   like(evbcev)
034309161107     d w_tcev          s                   like(ILRU3ITCEV)
034400150611     d w_video         s              1
034500150616     d w_ar4_1         s              1
034600150611     d pos             s              3  0
034700150612     d PosA            s              3  0
034800150612     d Lennt1          s              3  0
034900150612     d Lennt1v         s              3  0
035000150617     d kcaa            s                   like(anmcaa) inz(10)
035001160610     d w_ccaleg        s              1
035002161014     d w_orig          s              1
035003161027     d StatoCa         s              1
035100030527     D*
035200030527     D wrkoggiiso      s               d
035300991027      *
035400930705     D* DEFINIZIONI COSTANTI
035500160504     D*
035600930705     D CNOSEQ          C                   CONST('NON SEQUENZIALI')
035700941014     D CINTER          C                   CONST('* INTERROGAZIONE BOL-
035800150409     D                                     LA FILIALE *')
035900021216     D CF2413          C                   CONST('F24=AlTasti(1/3)')
036000021216     D CF2423          C                   CONST('F24=AlTasti(2/3)')
036100021216     D CF2433          C                   CONST('F24=AlTasti(3/3)')
036200021216     D CF2412          C                   CONST('F24=AlTasti(1/2)')
036300021216     D CF2422          C                   CONST('F24=AlTasti(2/2)')
036400150612     D CharOk          C                   const('QWERTYUIOPASDFGHJKLZXCVBNM-
036500150612     D                                     qwertyuiopasdfghjklzxcvbnm-
036600150612     D                                     1234567890.:-_/\()!£$%&+*§-
036700150612     D                                     <>''"èéòàìù')
036800150612     D mi              C                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto inglese
036900150612     D ma              C                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO INGLESE
037000110705     d
037100110705     D* CAMPO PER DSPATR "NO DISPLAY"
037200110705     D NODI            C                   CONST(X'27')
037300131120     D* CAMPO PER DSPATR "RI"
037400131120     D RI              C                   CONST(X'21')
037500131127     D* CAMPO PER DSPATR "HI"
037600131127     D hi              C                   CONST(X'22')
037601161108     D red             C                   CONST(X'28')
037700150618     d c_DspAtrNorm    c                   const(x'20')
037800900518     C****************************************************************
037900900518     C*  RIEPILOGO INDICATORI
038000900518     C***************************************************************
038300911105     C* 03    - SPEDIZIONE GIACENTE
038400990119     C* 04    - ABILITO TASTO FUNZIONALE F2 - INT C.A.
038600070411     c* 06    - on per tassazione 2 bolla emette scritta "ImportoBOLLO"
038700070411     c*       - offper tassazione 2 bolla emette scritta "Importo IVA"
038900000216     C* 08    - bolla dpd
039100961111     C* 10    - BOLLA DI RIENTRO
039300160615     C* 12    - ON=La spedizione è presente solo in BLP
039700961112     C* 16    - SI TRATTA DI BOLLA EXPORT
039800070403     C* 17    - p.o. utente e lna bolla hanno GEO attiva
039801160628     C* 18    - Abilita tasto funzionale F8=ORM
039802161017     c* 19    - Condizionamento RI su data/ora consegna
040000991027     C* 22    - ESISTE ESTENSIONE BOLLA 2 e ABILITO IL TASTO  F4=TASSAZIONE 2
040100950825     C* 23    - SFLEND
040200950825     C* 24    - ABILITO IL TASTO "F13=TASSAZIONE"
040400070404     C* 26    - presente FIARP
040500070403     C* 27    - presente blocco consegna ARBFBC<>' '
040600941028     C* 28    - VISUALIZZA MSG ERRORE
040700911129     C* 29    - ESISTE CONTRASSEGNO
040800911023     C* 30/31 - LETTURA COMODO
041000911023     C* 33    - BOLLA IN FRANCO
041100911023     C* 34    - LETTURA SUBFILE
041200911023     C* 35    - PULIZIA SUBFILE
041300911129     C* 36    - LETTURE
041500991027     C* 38    - ESISTE TB2 ANCHE SE NON PRESENTE TIPO RECORD '2' IN FIAR600F
041700160615     C* 41-59 - ERRORI
041800131111     C* 60    - email destiantario
041900131111     C* 61    - PROGRAMMA RICHIAMATO per scelta bolla
042000150616     C* 62    - Spedizione Da Partire
042100040218     C* 63    - Abilita F1  = Aggiunta in reclamo
042200040218     C* 64    - Abilita F16 = Gestione reclamo
042300091214     c* 65    - Abilita visualizzazione beneficiario C/Ass
042400150616     C* 66    - "Da partire" in RI se arrivato almeno un collo
042500960903     C* 67    - ABILITAZIONE TASTO DI COMANDO F7=ANOMALIE
042600131127     C* 68    - pos cursore RMN prima serv x  EMETTERE F23 PER CALCOLARE E VISUALIZZARE DATA CONS.
042700930505     C*         PREVISTA E LE ORE DI RITARDO
042800930526     C* 69    - DATA CONSEGNA MERCE >< 0
042900930510     C* 70    - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
043000160628     C* 71    - ABILITAZIONE TASTO DI COMANDO F23=EVENTI SEGNACOLLI
043100131107     C* 72    - SFLDSPCTL  LRI6S05
043200991027     C* 73    - BOLLA FRANCO FRONTIERA
043300970227     C* 74    - ABILITA EVENTI SEGNACOLLI
043400970227     C* 75    - UTILIZZATO PER NON ESEGUIRE LA SUBROUTINE "LEGBOL"
043500970227     C* 76    - SI DDT IN ALTA INTENSITA'
043600021213     C* 77    - NOTE SUPERMERCATI
043700970227     C* 78    - SFLDSP
043800950825     C* 79    - SFLDSPCTL
043900930510     C* 80    - NEL SFL DEI LEGAMI NON EMETTO IL CAMPO PER LA SELEZIONE
044000941028     C* 81    - USATO NEL VIDEO LEGAMI
044100941028     C* 82    - USATO NEL VIDEO LEGAMI
044200941028     C* 83    - USATO NEL VIDEO LEGAMI
044300160614     C* 84    - Bolla con figlia e tipo legame cca <> ' '
044400971219     C* 85    - VISUALIZZO RIFERIMENTO PARTNER ESTERO
044500111005     c* 86    - Abilitazione F22="PERFECT ORDER"
044600111005     C* 87    - Pgm richiamato in modalità "W" (Perfect Order Wurth)
044700110705     C* 88    -
044800931112     C* 89    - UTILIZZATO PER VISUALIZZARE ALTRI TASTI FUNZIONALI
044900920402     C* 90    - INDICA LA PRESENZA DI UN ERRORE
045000941028     C* 91    - ERRORE QCMDEXC (UTILIZZABILE)
045100940923     C* 92/94 - LEGAME BOLLA
045200930525     C* 95    - ESISTONO EVENTI DA INTERROGARE
045300160615     C* 96    - ON Data arrivo merce = 0
045400160614     C* 97    - Bolla "Da Partire"
045500941014     C* 98    - ESISTONO VARIAZIONI BOLLE EFFETTUATE
045600930504     C* 99    - OFF  ESISTE LEGAME BOLLA SEGUENTE
045700900518     C*****************************************************************
045800000000     C     *ENTRY        PLIST
045900000000     C                   PARM                    KPJBA
046000150402     c                   parm                    FNLRU6DS
046100131210     c
046200980609     C**
046300160606     C                   MOVEL     KPJBU         PARAMu6ds1
046400940713     C*
046500160505     c                   if        partla<>'C'
046600160505
046700060130     C                   Z-ADD     1             CODUT             1 0
046800060130     c                   Movel     rsut          V1cdsa
046900941227     C*
047000940713     C*-------------------------------------------------------------------------
047100911024     C                   EXSR      CARTAB
047200150410     c                   clear                   v2cflg
047300110930     c*
047400111005     c                   setoff                                       87
047500150312     c* W=Perfect order WURTH: attivo scritta "PERFECT ORDER" a video e
047600150312     c*   disattivo F22=Perfect Order per evitare errore di chiamata ricorsiva
047700160504     c                   if        parflg='W'
047800111005     c                   eval      *in87=*on
047900150312     c                   eval      no_f22='N'
048000110930     c                   endif
048100150312
048500941014     C* TESTATA
048600941014     C                   MOVEL     CINTER        V1CTES
048700131127     c                   clear                   v1clna
048800940317      *
048900930702     C*
049000940809     C                   MOVEL     'S'           WRIEM             1
049100930702     C**
049200930702     C* PGM RICHIAMATO
049300920326     C**
049400160504     C** PARFLG = " " - RICHIAMO "Normale"
049500160504     c*      (  =  1  - Richiamo "Normale" (SOLO PER I VECCHI PGM)  )
049600160504     C**        =  2  - RICHIAMATO PER   SELEZ BOLLA
049700160504     C**        =  C  - DISABIL.F2=C.A
049800160504     C**        =  N  - DISABIL.F7=ANOMA
049900160505     C**        =  P  - RICHIAMATO DA INT-BOLL PART E DISABILITAZ F7=ANOMALIE
050000160628     C**        =  E  - DISABIL.F23=EV.SEGNACOLLI
050100160504     C**        =  W  - RICHIAMATO per "PerfectOrder"
050200960903     C*
050300040218      *?Abilito F1=Aggiunta in Reclami?
050400040218      * (se richiamato da pgm. FIDNA1R...)
050500040220     c                   eval      *in63  = *off
050600150402     c                   if            SDSprm >  1  and ilru6ra='S'
050700150402     c                             and ELRU6ins = ' '
050800040220     c                   eval      *in63  = *on
050900111003     c                   endif
051000960903     C* ABILITO F7=ANOMALIE SE FLAG DIVERSO DA 'N' E 'P'
051100160504     C     PArFLG        IFNE      'N'
051200160504     C     PArFLG        ANDNE     'P'
051300960903     C                   SETON                                        67
051400040217     C                   ENDIF
051500160628     C* ABILITO F23=EVENTI SEGNACOLLI  SE FLAG DIVERSO DA 'E'
051600160504     C     PArFLG        IFNE      'E'
051700961121     C                   SETON                                        74
051800040217     C                   ENDIF
051900150409
052000160504     C     PArFLG        ifeq      '2'
052100150409     C                   SETON                                        61
052200150409     c                   endif
052300960903     C*
052400911025     C**
052401160825     c                   clear                   ds_fnarb
052500150313     c* chaino la spedizione in partenza e in arrivo
052600150611     c                   clear                   w_tbo
052601160913     c                   clear                   blp_arbdcm
052602161014     c                   clear                   blp_arbhmc
052700150313     c     karb          chain     fnblp01l
052800150611     c                   if        %found(fnblp01l)
052900150611     c                   eval      w_tbo='P'
052901160913     c                   eval      blp_arbdcm=arbdcm
052902161014     c                   eval      blp_arbhmc=arbhmc
053000150618     c                   seton                                        12
053100150611     c                   endif
053200150312     c     karb          chain     fnarb01l
053300150611     c                   if        %found(fnarb01l)
053400150611     c                   eval      w_tbo='A'
053500150618     c                   setoff                                       12
053600150611     c                   endif
053700150611     c                   if        w_tbo='P' or w_tbo='A'
053800150312     C** interrogazione bolla
053900150312     C                   EXSR      bolla
054000150312     C                   EXSR      visual
054100911023     C*
054200150313     c                   endif
054300150313     c
054400150313     c     fine          tag
054500150410
054600150410     c                   select
054700150410     c* pa1f03 = 1 --> premuto F3
054800150410     c                   when      v2cflg='2'
054900160504     c                   eval      paRf03='1'
055000150410     c* pa1f03 = S --> Selezionata bolla
055100150410     c                   when      v2cflg='3'
055200160504     c                   eval      paRf03='S'
055300150410     c                   endsl
055400160505
055500160505     c                   endif
055600160505     c
055700930504     C*
055800930504     C*
055900941109     C* PER F3=FINE PULISCO CHIAVE BOLLA E IMPOSTO FLAG F3
056000941109     C     *INKC         IFEQ      *ON
056100941109     C                   Z-ADD     *ZERO         V2CAAS
056200941109     C                   Z-ADD     *ZERO         COMLNP
056300941109     C                   Z-ADD     *ZERO         V2CNRS
056400941109     C                   Z-ADD     *ZERO         V2CNSP
056500160504     C                   MOVEL     '1'           PARF03
056600941109     C                   ENDIF
056700980603     C**
056800160505
056900160505     c* Chiusura con LR
057000160523     c                   if        partla<>' '
057100160505
057200160505     C* CALL A VUOTO AI PGM  PER CHIUDERE I FILES
057300160505     C                   MOVEL     'C'           D98TLA
057400160505     C                   CALL      'TNSD99R'
057500160505     C                   PARM                    TNSD99DS
057600160505     C*
057700160505     C                   MOVEL     'C'           PA1FLG
057800160505     C                   MOVEL     PARAMint      KPJBU
057900160505     C                   CALL      'FNLR75R'                            99
058000160505     C                   PARM                    KPJBA
058100160505     C*
058200160505     C                   CLEAR                   PARAB5
058300160505     C                   MOVEL     'C'           PB5FLG
058400160505     C                   MOVEL     PARAB5        KPJBU
058500160505     C                   CALL      'FNLRB5R'
058600160505     C                   PARM                    KPJBA
058700160505     C*
058800160505      *
058900160505     C                   IF        Wtibs69r  <>  *BLANKS
059000160505     C                   eval      I69TLA  = 'C'
059100160505     C                   call      'TIBS69R'
059200160505     C                   parm                    TIBS69DS
059300160505     c                   ENDIF
059400941212     C*
059500040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
059600040810     c                   clear                   fidn12ds
059700040810     c                   eval      i12tla = 'C'
059800040810     c                   call      'FIDN12R'
059900040810     c                   parm                    fidn12ds
060000051128     c                   clear                   fnlv80ds
060100051128     c                   eval      ilv80tla = 'C'
060200051128     c                   call      'FNLV80R'
060300051128     c                   parm                    kpjba
060400051128     c                   parm                    FNLV80DS
060500070427     c                   eval      id34tla = 'C'
060600070427     c                   call      'FIDG34R'
060700070427     c                   parm                    fidg34ds
060800131120     c                   eval      ilrg7tla = 'C'
060900131120     c                   call      'FNLRG7R'
061000131120     c                   parm                    fnlrg7ds
061100091216     c                   eval      iinttla = 'C'
061200091216     c                   call      'TRULINTR'
061300091216     c                   parm                    trulintds
061400150615     c                   eval      ilrj6tla = 'C'
061500150615     c                   call      'FNLRJ6R'
061600150615     c                   parm                    fnlrj6ds
061700141105
061800040810     c*
061900000000     C                   SETON                                        LR
062000160505     c                   else
062100160505     C                   SETON                                        RT
062200160505     c                   endif
062300160505
062400160606     C                   MOVEL     PARAMU6ds1    KPJBU
062500941011     C   99
062600941011     CANN99              READ      FNBLP01L                               99
062700940802     C**************************************************************************
062800940802     C*    CARICO TABELLE
062900940802     C**************************************************************************
063000911024     C     CARTAB        BEGSR
063100930705     C*
063200930705     C* CARICO TABELLA 7O
063300930705     C                   MOVEL     '7O'          COD
063400941212     C                   Z-ADD     1             C                 3 0
063500940713     C     KTAB2         SETLL     TABEL
063600940713     C     KTAB2         READE     TABEL                                  30
063700930705     C*
063800930705     C     *IN30         DOWEQ     *OFF
063900930705     C     TBLFLG        IFEQ      ' '
064000930705     C                   MOVEL     TBLKEY        KCA(C)
064100930705     C                   MOVEL     TBLUNI        DCA(C)
064200930705     C                   ADD       1             C
064300930705     C                   END
064400940713     C     KTAB2         READE     TABEL                                  30
064500930705     C                   END
064600941024     C*
064700941024     C* CARICO IN SCHIERA LA TABELLA CC
064800941024     C                   Z-ADD     *ZERO         WA
064900941024     C                   CLEAR                   CC1
065000941024     C                   CLEAR                   CC2
065100020329     C                   CLEAR                   CC3
065200941024     C                   MOVEL     'CC'          COD
065300941024     C     KTAB2         SETLL     TABEL00F
065400941024     C     KTAB2         READE     TABEL00F                               99
065500941024     C     *IN99         DOWEQ     *OFF
065600941024     C     TBLKEY        ANDNE     *BLANKS
065700941024     C     TBLKEY        ANDNE     *ZERO
065800941024     C                   ADD       1             WA
065900941024     C                   MOVE      TBLKEY        CC1(WA)
066000941024     C                   MOVEL     TBLUNI        CC2(WA)
066100020402     C                   MOVEL(P)  TBLUNI        DSCC
066200020402     C                   MOVE      §CCDFE        CC3(WA)
066300941024     C     KTAB2         READE     TABEL00F                               99
066400941024     C                   ENDDO
066500060130     C*
066600020329      *
066700020329      * CARICO IN SCHIERA LA TABELLA CB
066800020329     C                   Z-ADD     *ZERO         WA
066900020329     C                   CLEAR                   CB1
067000020329     C                   CLEAR                   CB2
067100020329     C                   MOVEL     'CB'          COD
067200020329     C     KTAB2         SETLL     TABEL00F
067300020329     C     KTAB2         READE     TABEL00F                               99
067400020329     C     *IN99         DOWEQ     *OFF
067500020329     C     TBLKEY        ANDNE     *BLANKS
067600020329     C     TBLKEY        ANDNE     *ZERO
067700020329     C                   ADD       1             WA
067800020329     C                   MOVE      TBLKEY        CB1(WA)
067900020329     C                   MOVEL     TBLUNI        CB2(WA)
068000020329     C     KTAB2         READE     TABEL00F                               99
068100020329     C                   ENDDO
068200991027     C*
068300991027     C* CARICO LA TABELLA $2
068400991027     C                   MOVEL     '$2'          COD
068500991027     C                   MOVEL(P)  1             KEY
068600991027     C     KTAB1         CHAIN     TABEL00F
068700991027     C                   IF        %FOUND
068800991027     C                   MOVEL     TBLUNI        DS$2
068900991027     C                   ELSE
069000991027     C                   CLEAR                   DS$2
069100991027     C                   ENDIF
069200940802     C*
069300911024     C                   ENDSR
069400940802     C**************************************************************************
069500940802     C* EMISSIONE BOLLA
069600940802     C**************************************************************************
069700161014     C     BOLLA         begsr
069800940802     C*
069801160628     c                   setoff                                       18
069900070411     C                   SETOFF                                       220616
070000070403     C                   SETOFF                                       170377
070100070404     C                   SETOFF                                       262708
070200131127     C                   SETOFF                                       2933
070300940408     C                   SETOFF                                       687069
070400070524     C                   SETOFF                                       850488
070500150617     C                   SETOFF                                       866266
070600160614     C                   SETOFF                                       9784
070601161017     C                   SETOFF                                       19
070700991027      *
070800940719     C                   Z-ADD     0             V3CLNA
070900940719     C                   Z-ADD     0             V3CDSP
071000930517     C                   Z-ADD     0             VIDRMN
071100940712     C                   MOVE      *BLANKS       VIDRMA
071200991217     C                   CLEAR                   V3CRPP
071300131120     C                   CLEAR                   atrpda
071301160913     C                   CLEAR                   vidpda
071400131127     C                   CLEAR                   atrtsp
071500160513     C                   CLEAR                   v7ddse
071600160513     C                   CLEAR                   v7ddsed
071601161014     C                   CLEAR                   v7dcons
071602161014     C                   CLEAR                   vidhmc
071700150615     c* ancora da partire
071800150616     c                   if        w_tbo='P' and arbft1=*blanks
071900150616     c                   eval      v1dfbc='Da Partire'
072000150616     c                   seton                                        62
072100150616     c* se "manca record bolla arrivo" metto la scritta in RI
072200150617     c                   if        arbdam>0
072300150617     c                   seton                                        66
072400150617     c                   else
072500150617     C                   Z-ADD     ARBAAS        KAAS
072600150617     C                   Z-ADD     ARBLNP        KLNP
072700150617     C                   Z-ADD     ARBNRS        KNRS
072800150617     C                   Z-ADD     ARBNSP        KNSP
072900150617     c     kanm          setll     fnanm01l                               66
073000150617     c                   endif
073100160513     c* Visualizzo in questo caso la data spunta entrata
073200160513     c                   seton                                        97
073300160513     c                   if        arbdse>0
073400160513     c     *iso          movel     arbdse        dataeur
073500160517     c                   movel     dataeur       w0080
073501160517     c                   eval      v7ddse=%editW(w0080:'  /  /    ')
073503160516     c* astesrisco le seguenti specifiche: per visualizzare se arrivo totale/parziale
073504160516     c* bisognerebbe elaborare sia le spunte del teminal di partenza che quelle della lnp
073505160516     c* insieme.... mentre al momento è possibile farlo con una linea alla volta...
073600160516     c**                 clear                   fnlrj6ds
073700160516     c**                 eval      ILRJ6TRIC='P'
073800160516     c**                 eval      ILRJ6AAS =arbaas
073900160516     c**                 eval      ILRJ6LNP =arblnp
074000160516     c**                 eval      ILRJ6NRS =arbnrs
074100160516     c**                 eval      ILRJ6NSP =arbnsp
074101160516     c**                 eval      ILRJ6lin =arblnp
074200160516     c**                 call      'FNLRJ6R'
074300160516     c**                 parm                    fnlrj6ds
074400160516     c**                 if        olrj6err=*blanks and olrj6fall='S'
074500160516     c**                 eval      V7DDSED='Totale'
074600160516     c**                 else
074700160516     c**                 eval      V7DDSED='Parziale'
074800160516     c**                 endif
074801160517     c                   else
074802160517     c                   eval      v7ddse='NO'
074900160513     c                   endif
075000150615     c                   endif
075100961112     C*
075200911022     C* CONTROLLO SE SPEDIZIONE GIACENTE
075300920219     C* GIACENZA APERTA
075400941202     C                   Z-ADD     ARBAAS        KAAS
075500940713     C                   Z-ADD     ARBLNP        KLNP
075600940713     C                   Z-ADD     ARBNRS        KNRS
075700940713     C                   Z-ADD     ARBNSP        KNSP
075800050405     C     KGCA1         CHAIN     tigcp21l                           31
075900040216if  2C  N31GCPATB        IFEQ      ' '
076000920410     C     GCPDCG        IFNE      0
076100920219     C                   SETON                                        03
076200920219     C                   ELSE
076300070403     C                   SETON                                        0327
076400150616     C                   EVAL      V1DFBC=' GIACENTE '
076500040217     C                   ENDIF
076600040217e   2C                   ENDIF
076700930428     C*
076800021213     C*
076900021213     C* 77 ON - Esistono note supermenrcati d ainterrogare
077000021213     C                   Z-ADD     ARBAAS        KAAS
077100021213     C                   Z-ADD     ARBLNP        KLNP
077200021213     C                   Z-ADD     ARBNRS        KNRS
077300021213     C                   Z-ADD     ARBNSP        KNSP
077400021213     C                   MOVEL     'SCR'         KTRD
077500021213     C     KAR51         CHAIN     FIAR5000                           31
077600021217     C  n31              seton                                        77
077700021217     C   31              setoff                                       77
077800920219     C*
077900920207     C* SE BOLLA CON LA "G" SENZA GIACENZA MECCANIZZATA SEGNALO
078000931116     C     ARBFBC        IFEQ      'G'
078100070412     C                   EVAL      V1DFBC=' GIACENTE '
078200070403     C                   SETON                                        27
078300040217     C                   ENDIF
078400930415     C*
078500940907     C                   MOVEL     ARBAAS        V3CAAS
078600940719     C                   MOVE      ARBNSP        V3CNSP
078700940719     C                   MOVEL     ARBNRS        V3CNRS
078800070404     C                   MOVEL     ARBNRS        V4CNRS
078900131108     C********           MOVEL     ARBNRS        V7CNRS
079000940719     C                   MOVEL     ARBLNP        V3CLNP
079100950508     C                   MOVEL     ARBLNP        V3CLPA
079200131114     c
079300131114     c                   z-add     arbmgs        arbdsp
079400131114     c                   movel     arbaas        arbdsp
079500940908     C*
079600940908     C*  L E G A M I    B O L L A
079700940908     C  N75              EXSR      LEGBOL
079800940908     C*
079900911022     C* NON SEQUENZIALI
080000911023     C     ARBFNS        IFEQ      'S'
080100930705     C                   MOVEL     CNOSEQ        DESFNS
080200911022     C                   ELSE
080300911022     C                   MOVEL     *BLANKS       DESFNS
080400040217     C                   ENDIF
080500930525     C*
080600930525     C* 95 ON - ESITONO EVENTI DA INTERROGARE
080700940713     C                   Z-ADD     ARBAAS        KAAS$
080800940713     C                   Z-ADD     ARBLNP        KLNP
080900940713     C                   Z-ADD     ARBNRS        KNRS
081000940713     C                   Z-ADD     ARBNSP        KNSP
081100990805     C     KEVB1         SETLL     FNEVB01L                               95
081200920402     C*
081300920403     C* 98 ON - ESITONO VARIAZIONI FATTE
081400131007     C     KARB2         SETLL     FiARBF2C                               98
081500990119     C* 04 ON - ESISTONO CA DA VISUALIZZARE
081600990119     C                   CLEAR                   CONTCA
081601161027     C                   CLEAR                   StatoCa
081700040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
081800040809      * legate alla spedizione che passo
081900040809     c                   clear                   fidn12ds
082000040809     c                   eval      i12aas = arbAAS
082100040809     c                   eval      i12lnp = arbLNP
082200040809     c                   eval      i12nrs = arbNRS
082300040809     c                   eval      i12nsp = arbNSP
082400040809     c                   eval      i12fel = 'E'
082500040809     c                   eval      i12fan = 'E'
082600040809     c*
082700040809     c                   call      'FIDN12R'
082800040809     c                   parm                    fidn12ds
082900040809     c*
083000040809     c                   setoff                                       31
083100040809     c*
083200040809     c* se non ci sono errori
083300040809     c                   if        o12err <> ' '
083400040809     c                   seton                                        31
083500040809     c                   else
083600040809     c* se numero di CA trovate maggiore di zero
083700040809     c                   if        o12nca = 0
083800040809     c                   seton                                        31
083900040809     c                   else
084000040809     c                   z-add     1             ii                2 0
084100040809     c                   dow       ii <= o12nca and *in31 = *off
084200040809     c                   movea     skkey(ii)     dskey
084300040809     C                   SETON                                        04
084400040809     C                   ADD       1             CONTCA            3 0
084401161027     c                   if        skdch(ii)>0 and StatoCa<>'A'
084402161027     c                   eval      StatoCa='C'
084403161027     c                   else
084404161027     c                   eval      StatoCa='A'
084405161027     c                   endif
084500040809     C                   Z-ADD     dsaac         VHCAAC
084600040809     C                   Z-ADD     dsfil         VHCFIL
084700040809     C                   Z-ADD     dsnca         VHCNCA
084800040809     c*
084900040809     c                   add       1             ii
085000040809     c                   enddo
085100040809     c*
085200040809     c                   seton                                        31
085300040809     c*
085400040809     c                   endif
085500040809     c                   endif
085600040809     c**
085700070406     c* se Presente FIARP, imposto la cons.richista proposta
085800070404     c                   clear                   v4cdimpr
085900070404     c                   clear                   v4coimpr
086000070404     c                   clear                   v4cprupr
086100070416     c                   clear                   v08dcrpr
086200070404     c                   clear                   v4ctcrpr
086300070404     c                   clear                   v4chcrpr
086400070406     c
086500070406     c* Solo se visualizzabile
086600070406     c     karb2         chain     fiarp01l
086700070406    1c                   if        %found(FIarp01l)
086800070404     c                   seton                                        26
086900070404     C                   MOVEL     arppru        v4cprupr
087000070404     C                   MOVEL     arpoim        v4coimpr
087100070404    2c                   if        arpdim>0
087200070404     c     *iso          movel     arpdim        dataeur
087300070416     c                   movel     dataeur       datadmy
087400070416     c                   movel     datadmy       w0060             6 0
087500070416     c                   movel     w0060         v4cdimpr
087600070404    2c                   endif
087700070404    2c                   if        arpdcr>0
087800070404     c     *iso          movel     arpdcr        dataeur
087900070416     c                   movel     dataeur       v08dcrpr
088000070416     c**                 movel     datadmy       v06dcrpr
088100070404    2c                   endif
088200070404     c                   movel     arphcr        v4chcrpr
088300070404     c                   movel     arptcr        v4ctcrpr
088400070406     c                   if        arptcr=' '
088500070406     c                   eval      v4ctcrpr='IL'
088600070404    1c                   endif
088700070406    1c                   endif
088800070406     c
088900911022     C* TIPO BOLLA CONTROLLO SE FRANCO
089000940719     C                   MOVEL     ARBCBO        V3CCBO
089100911022     C                   MOVEL     '3A'          COD
089200911022     C                   MOVEL     *BLANKS       KEY
089300940712     C                   MOVEL     ARBCBO        KEY
089400940713     C     KTAB1         CHAIN     TABEL                              31
089500911022     C                   MOVEL     TBLUNI        DS3A
089600911022     C                   MOVEL     §3ATB1        FLGTB1            1
089700940719     C                   MOVEL     §3ADES        V3DCBO
089800940719     C                   MOVEL     §3ADES        V5DCBO
089900911030     C* FRANCO
090000040216if  1C     FLGTB1        IFEQ      'F'
090100911022     C                   SETON                                        33
090200940713     C                   MOVE      ARBKSC        V3CKSC
090300940712     C                   MOVE      *BLANKS       VIDKSC
090400040216x   1C                   ELSE
090500911030     C* ASSEGNATO
090600911030     C     ARBCCM        IFNE      0
090700940713     C                   MOVE      ARBCCM        V3CKSC
090800911030     C                   ELSE
090900940713     C                   MOVE      *ZEROS        V3CKSC
091000040217     C                   ENDIF
091100911030     C                   MOVEL     ARBKSC        VIDKSC
091200040217e   1C                   ENDIF
091300070410     c* Numero di telefono del mittente
091400070410     c                   clear                   v3ctelm
091500070410     c                   move      v3cksc        w0040
091600070410     c                   if        w0040<>0000 and w0040<>8888
091700070410     c                             and w0040<>9999
091800070410     c                   eval      i69kin = V3Cksc
091900070410     c                   exsr      CTR_ANAGRA
092000070410     c                   eval      V3ctelm= indtel
092100070410     c                   endif
092200991027     C* 38 ON - ESISTE TB2 ANCHE SENZA TIPO RCD "2" IN FIAR600F
092300920121     C     §3ATB2        COMP      '  '                               3838
092400040218      *
092500040218      *?Controllo se esitono reclami?
092600131111     c                   eval      wARBlnp = ARBlnp
092700131111     c                   eval      wARBnrs = ARBnrs
092800131111     c                   eval      wARBnsp = ARBnsp
092900131111     c                   eval      wARBaas = ARBaas
093000040218     c                   eval      TGDtor  = 'S'
093100040218     c                   eval      TGDogg  = ds_OggSped
093200040218     c     K02TGD02      setll     FITGD000
093300160408     c                   eval      *in64   = (%equal(FITGD32L)   and
093400040303     c                                           (SDSprm =  1
093500040303     c                                        or (SDSprm >  1
093600150402     c                                        and ELRU6INS <> 'N')))
093700940712     C*
093800940712     C* DECODIFICO  "BOLLA DI RIENTRO"
093900961111     C                   MOVEL     *BLANKS       VIDFBR
094000961111     C                   MOVEL     *BLANKS       VIDFFR
094100961111     C                   SETOFF                                       1073
094200940712     C     ARBFBR        IFEQ      'S'
094300961111     C                   MOVEL     'SI'          VIDFBR
094400961111     C                   SETON                                        10
094500040217     C                   ENDIF
094600961111     C     ARBFBR        IFEQ      'F'
094700961111     C                   MOVEL     'FR'          VIDFFR
094800961111     C                   SETON                                        73
094900040217     C                   ENDIF
095000970130     C*
095100970227     C* FLAG CONSEGNA DDT
095200970130     C                   MOVEL     ARBAAS        KAAS
095300970130     C                   MOVEL     ARBLNP        KLNP
095400970130     C                   MOVEL     ARBNRS        KNRS
095500970130     C                   MOVEL     ARBNSP        KNSP
095600970130     C                   MOVEL     'A'           KTRC
095700970131     C                   CLEAR                   V1DBAM
095800000615     C                   eval      v3csop = ' '
095900060216     C     KAR41         CHAIN     FiAR401L                           31
096000970227     C* C'E' IL RECORD
096100040216if  1C     *IN31         IFEQ      *OFF
096200000615     C                   movel     ar4not        dsbl4a
096300000615     C                   eval      v3csop = §b4aso
096400031027     c                   Move      §b4Abm        wbam              1
096500040216if  2C     WBAM          IFEQ      'S'
096600970131     C     WBAM          OREQ      'C'
096700031027     c     wbam          oreq      'P'
096800031027     c     wbam          oreq      'Q'
096900970227     C                   SETON                                        76
097000970227     C                   MOVEL     'SI DDT'      V1DBAM
097100040216x   2C                   ELSE
097200970227     C                   SETOFF                                       76
097300970227     C                   MOVEL     'NO DDT'      V1DBAM
097400040216e   2C                   ENDIF
097500970227     C**
097600150615sel 2C*                  SELECT
097700150615     C*    WBAM          WHENEQ    'C'
097800150615     C*    WBAM          OREQ      'K'
097900150615     C*                  MOVEL     'CLI'         V1DFST
098000150615     C*    WBAM          WHENEQ    'J'
098100150615     C*    WBAM          oreq      'P'
098200150615     C*    WBAM          oreq      'Q'
098300150615     C*    WBAM          OREQ      'W'
098400150615     C*                  MOVEL     'Arr'         V1DFST
098500150615     C*    WBAM          WHENEQ    'S'
098600150615     C*    WBAM          OREQ      'Y'
098700150615     C*                  MOVEL     'Par'         V1DFST
098800150615x   2C*                  OTHER
098900150615     C*                  MOVEL     'NO '         V1DFST
099000150615e   2C*                  ENDSL
099100970227     C**
099200970227     C* NON  C'E' IL RECORD
099300150615x   1C                   ELSE
099301160909     c* se la bolla è da partire prendo il flag da blp
099302160909     c                   if        *in62
099303160909     C     arbFST        IFEQ      'S'
099304160909     C     arbFST        OREQ      'C'
099305160909     C                   MOVEL     'SI DDT'      V1DBAM
099306160909     C                   SETON                                        76
099307160909     C                   ELSE
099308160909     C                   MOVEL     'NO DDT'      V1DBAM
099309160909     C                   SETOFF                                       76
099310160909     C                   ENDIF
099311161019     C                   MOVEL     arbsop        v3csop
099312160909     c                   else
099313161019     c* Attualmente questa situazione non dovrebbe più capitare: se la bolla è in arrivo
099314161019     c* il fiar4 trk 'A' deve sempre esserci
099400150615     C                   SETON                                        76
099500150615     C                   MOVEL     'SI DDT'      V1DBAM
099600150615     C*                  MOVEL     'PAR'         V1DFST
099602160909     c                   endif
099700150615e   1C                   ENDIF
099800010510     c* numero O.R.M.
099900010510     C                   MOVEL     'M'           KTRC
100000060216     c     kar41         chain     fiar401l                           31
100100010510     c     *in31         ifeq      *off
100200010510     c                   movel     ar4not        dsbl4m
100300010510     c                   z-add     §b4poe        v3cpoe
100400010510     c                   z-add     §b4nsr        v3cnsr
100500010510     c                   z-add     §b4nor        v3cnor
100600010510     c                   z-add     §b4nrv        v3cnrv
100601160628     c                   seton                                        18
100700010510     c                   else
100800010510     c                   clear                   v3cpoe
100900010510     c                   clear                   v3cnsr
101000010510     c                   clear                   v3cnor
101100010510     c                   clear                   v3cnrv
101200010510     c                   endif
101300130320     c*//* Verifico se ha fiar8
101400130320     C*//                MOVEL     ARBAAS        KAAS
101500130320     C*//                MOVEL     ARBLNP        KLNP
101600130320     C*//                MOVEL     ARBNRS        KNRS
101700130320     C*//                MOVEL     ARBNSP        KNSP
101800130320     c*//  ksped         setll     fiar801l
101900130320     c*//                if        %equal(fiar801l)
102000130320     c*//                eval      v1dAR8='PACKING LIST'
102100130320     C*//                ELSE
102200130320     C*//                CLEAR                   V1DAR8
102300130320     c*//                endif
102400941219     C*
102500941219     C* DECODIFICO  "CONSEGNA A MAGAZZINO"
102600941219     C     ARBFDN        IFEQ      'S'
102700941219     C                   MOVEL     'SI'          V3CFDN
102800941219     C                   ELSE
102900941219     C                   MOVEL     *BLANKS       V3CFDN
103000941219     C                   ENDIF
103100911129     C*
103200920204     C* INCASSO C/A E ASSEGNATO
103300930113     C                   MOVEL     '  '          VIDICC
103400930113     C                   MOVEL     '  '          VIDICA
103500150615     c                   clear                   v7dicc
103600930113     C*
103700930113     C* SE ASSEGNATO O FRANCO CON SECONDA BOLLA
103800930113     C     *IN33         IFEQ      '0'
103900930113     C     *IN33         OREQ      '1'
104000930113     C     *IN38         ANDEQ     '1'
104100930113     C*
104200150617     c     w_tbo         ifeq      'A'
104300160825     C     ARBICA        andne     ' '
104400941216     C     ARBICA        ANDNE     'R'
104401160825     c     w_tbo         oreq      'P'
104402160825     c     arbdcm        andgt     0
104500920204     C                   MOVEL     'SI'          VIDICA
104600040217     C                   ENDIF
104800040217     C                   ENDIF
104900911129     C*
105000911030     C* LINEA PARTENZA DECODIFICA
105100020225     c                   clear                   og143
105200150313     C                   Z-ADD     ARBLNP        NUM3              3 0
105300930706     C     NUM3          CHAIN     AZORG01L                           31
105400940719     C  N31              MOVEL     ORGDES        V3DLNP
105500940719     C   31              MOVEL     *BLANKS       V3DLNP
105600020225     c                   eval      og143 = orgde3
105700020225     c                   eval      lnpntw = §ogntw
105800911030     C* LINEA ARRIVO   DECODIFICA
105900991217     c                   clear                   og143
105901160517     C                   clear                   orgde5
105902160913     c                   clear                   v7dlnaa
105903160913     c                   clear                   v7dalna
106000070403     C     arblna        CHAIN     AZORG01L                           31
106001160913     C                   movel     orgde3        og143
106002160913     C                   movel     orgde8        og148
106100940719     C  N31              MOVEL     ORGDES        V3DLNA
106200940719     C   31              MOVEL     *BLANKS       V3DLNA
106300070403     c                   movel     v3dlna        v7dlna
106301160913     c                   if        §ogntw<>'EEX' and
106302160913     c                             §ogntw<>'DPD' and §ogntw<>'FED'
106303160913     c                   movel     'a'           v7dalna
106304160517     c                   if        orgde5<>*blANKS
106305160517     c                   movel     orgde5        v7dlnaa
106306160517     c                   else
106307160517     c                   movel     v3dlna        v7dlnaa
106308160517     c                   endif
106309160913     c                   endif
106600020304     c                   eval      lnantw = §ogntw
106700070426     c* p.o. gestore di arblna
106800070426     c                   clear                   fnlv55ds
106900070426     c                   movel     arblna        d55lin
107000070426     c                   movel     '6'           d55tpt
107100070426     C                   time                    wrkoggiiso
107200070426     C     *iso          move      wrkoggiiso    d55drf
107300070426     c                   call      'FNLV55R'
107400070426     c                   parm                    fnlv55ds
107500070426     c
107600070426     c                   if        d55tfa<>arblna
107700070426     C     d55tfa        CHAIN     AZORG01L                           31
107800070426     C  n31              movel     orgde8        og148
107900070426     C   31              clear                   og148
108000070426     c                   endif
108100070426     c
108200070403     c
108300070403     c* Abilito visualizzazione nuove funzioni se GEO attiva
108400070426     c* Per p.o. gestione linea di arrivo e p.o. utente
108500070403     c                   if        §oggeot='S' and dutgeot='S'
108600070524     c                   seton                                        1788
108700070406     c                   else
108800070406     c* non visualizzo nemmeno fiarp
108900070406     c                   setoff                                       26
109000070403     c                   endif
109100070524     c
109200070524     c* Vedo se almeno è attivo lo scambio dati con tellus
109300070524     c                   if        not *in88 and §ogtlls<>' ' and dutgeot='S'
109400070524     c                   seton                                        88
109500070524     c                   endif
109600070404     c
109700070404     c* Emetto riga sintetica altre abilitazioni
109800070521     c                   clear                   v5dngd
109900140109     c***                if        *in17
110000070404     c                   clear                   v3cges
110100070404     C                   MOVEL     *BLANKS       V3cgesd
110200150615     C                   clear                   v7drig1
110300070404     c*
110400131120     c                   clear                   fnlrg7ds
110500131120     c                   eval      ilrg7aas=arbaas
110600131120     c                   eval      ilrg7lnp=arblnp
110700131120     c                   eval      ilrg7nrs=arbnrs
110800131120     c                   eval      ilrg7nsp=arbnsp
110900131120     c                   eval      ilrg7lung=58
111000131120     c                   call      'FNLRG7R'
111100131120     c                   parm                    fnlrg7ds
111200131120     c                   movel     olrg7vssin    v3cges
111300150615     c* richiamo per visualizzazione in videata Globale
111400150615     c                   clear                   fnlrg7ds
111500150615     c                   eval      ilrg7aas=arbaas
111600150615     c                   eval      ilrg7lnp=arblnp
111700150615     c                   eval      ilrg7nrs=arbnrs
111800150615     c                   eval      ilrg7nsp=arbnsp
111900150615     c                   eval      ilrg7lung=10
112000150615     c                   eval      ilrg7nb1='S'
112100150615     c                   eval      ilrg7nb2='A'
112200150615     c                   eval      ilrg7nb3='C'
112300150615     c                   eval      ilrg7nb4='F'
112400150615     c                   eval      ilrg7nb5='M'
112500150615     c                   eval      ilrg7nb6='B'
112600150615     c                   eval      ilrg7nb7='9'
112700150615     c                   eval      ilrg7nb8='L'
112800150615     c                   call      'FNLRG7R'
112900150615     c                   parm                    fnlrg7ds
113000150615     c                   eval      v7drig1=*all'-'
113100150615     c                   if        olrg7abi<>*blanks
113200150616     c                   eval      %subst(v7drig1:66:12)=' ' +
113300150616     c                                             %subst(olrg7vssin:1:10) + '?'
113400150616     c                   setoff                                       55
113500150616     c                   else
113600150616     c                   seton                                        55
113700150615     c                   endif
113800150618     c                   if        arbdcp>0
113900150618     c     *iso          move      arbdcp        datadmy
114000150618     c                   movel     datadmy       w0060
114100161108     c                   eval      %subst(v7drig1:1:22)='ConsParziale'       +
114200161107     c                             hi + %editW(w0060:'  /  /  ') + c_DspAtrNorm
114300150618     c                   endif
114400140109     c***                endif
114500070410     c
114600070524     c* Reperisco dati della distinta automatica tramite FIDG34R
114700070524     c                   if        *in88
114800070410     c                   clear                   v5ccgi
114900070411     c                   clear                   v5dcgi
115000070410     c                   clear                   v5dtgi
115100070410     c                   clear                   v5dttl
115200070411     c                   clear                   v5clno
115300070427     c                   clear                   v5dlno
115400070427     c                   clear                   v5cargerr
115500070427     c                   clear                   v5dargerr
115600070427     c
115700070427     c                   clear                   fidg34ds
115800070427     c                   eval      id34tip='2'
115900070427     c                   eval      id34aas=arbaas
116000070427     c                   eval      id34lnp=arblnp
116100070427     c                   eval      id34nrs=arbnrs
116200070427     c                   eval      id34nsp=arbnsp
116300070427     c                   call      'FIDG34R'
116400070427     c                   parm                    FIDG34DS
116500070410     c
116600070427     c                   movel     od34cgi       v5ccgi
116700070410    3c                   select
116800070427     c                   when      od34tgi='A'
116900070410     c                   eval      v5dtgi='Automatico'
117000070427     c                   when      od34tgi='M'
117100070410     c                   eval      v5dtgi='Manuale   '
117200070410    3c                   ENDSL
117300070411     c* Decodifica giro
117400070427     c                   eval      v5dcgi=od34cgides
117500070427     c* esito/errore
117600070427     c                   movel     od34err       v5cargerr
117700070427     c                   eval      v5dargerr=od34errdes
117800070427     c* livello normalizzazione
117900070427     c                   movel     od34lno       v5clno
118000070427     c                   eval      v5dlno=od34lnodes
118100070427     c* elaborazione tellus
118200070427     c***                eval      v5dttl=od34ttldes
118300070427     c                   select
118400070427     c                   when      od34ttl='1'
118500070427     c                   eval      v5dttl='SI- CON Giro'
118600070427     c                   when      od34ttl='2'
118700070427     c                   eval      v5dttl='SI-SenzaGiro'
118800070427     c                   endsl
118900070521     c*
119000070521     c* Decodifico se assegnazione automatica o manuale
119100070521     c                   if        arbndc>0 and arbngd<>0
119200070521     c                   eval      v5dngd='MANUALE'
119300070521     c                   endif
119400070521     c                   if        arbndc>0 and arbngd=0
119500070521     c                   eval      v5dngd='AUTOMAT'
119600070521     c                   endif
119700070410     c
119800070410    1c                   endif
119900991217     c
120000060518     c* LNA DPD o lnp dpd: leggo record "I" di ar4
120100000216     c                   setoff                                       08
120200000216     c                   clear                   v3czon
120300000216     c                   clear                   v3crtn
120400040216if  1c                   if        lnantw = 'DPD'
120500991217     c                   seton                                        85
120600991217     C                   MOVEL     ARBAAS        KAAS
120700991217     C                   MOVEL     ARBLNP        KLNP
120800991217     C                   MOVEL     ARBNRS        KNRS
120900991217     C                   MOVEL     ARBNSP        KNSP
121000060518     C                   MOVEL     'I'           KTRC
121100060216     C     KAR41         CHAIN     FiAR401L                           31
121200060518     C  N31              MOVEL     AR4NOT        dsbl4i
121300060518     c   31              clear                   dsbl4i
121400060518     C                   MOVEL(P)  §b4ipn        V3CRPP
121500060518     c                   if        §b4ida<>*blanks or §b4iza>*zeros
121600060518     c                   movel     §b4ida        v3crtn
121700060518     c                   movel     §b4iza        v3czon
121800000216     c                   seton                                        08
121900000216     c                   endif
122000050304     c                   else
122100050304if  1c                   if        lnpntw = 'DPD'
122200050304     C                   MOVEL     ARBAAS        KAAS
122300050304     C                   MOVEL     ARBLNP        KLNP
122400050304     C                   MOVEL     ARBNRS        KNRS
122500050304     C                   MOVEL     ARBNSP        KNSP
122600060518     C                   MOVEL     'I'           KTRC
122700060518     c                   clear                   dsbl4i
122800060216     C     KAR41         CHAIN     FiAR401L
122900060216     c                   if        %found(fiar401l)
123000060518     C                   MOVEL     AR4NOT        dsbl4i
123100060518     C                   MOVEL     §b4ipn        V3CRPP
123200050304     c                   seton                                        85
123300050304     c                   endif
123400050304     c                   endif
123500040216e   1c                   endif
123600971219     C*
123700971219     C* 16 ON  - SI TRATTA DI BOLLA EXPORT
123800020225     c                   if        lnantw = 'EEX' or lnantw = 'EUP' or
123900060201     c                             lnantw = 'FED' or lnantw='DPD'
124000971219     C                   SETON                                        16
124100971219    1C                   ENDIF
124200940719     C                   MOVEL     V3DLNA        D15LNA
124300920804     C*
124400940719     C                   MOVEL     ARBLNA        V3CLNA
124500940719     C                   MOVEL     ARBRSM        V3CRSM
124600070410     C                   MOVEL     ARBRSM        V4CRSM
124700940713     C                   MOVEL     ARBRSM        V28RSM
124800940719     C                   MOVEL     ARBINM        V3CINM
124900940719     C                   MOVEL     ARBPRM        V3CPRM
125000940719     C                   MOVEL     ARBCAM        V3CCAM
125100940719     C                   MOVEL     ARBLOM        V3CLOM
125101160912     C                   MOVEL     ARBLOM        V4CLOM
125200940713     C                   MOVEL     ARBLOM        V25LOM
125300940719     C                   MOVEL     ARBFAP        V3CFAP
125400950421     C                   MOVEL     ARBNZM        V3CNZM
125500131108     c                   if        arbnzm<>*blaNKS
125600131108     C                   MOVEL     arbnzm        v7cprm
125700131108     c                   else
125800131108     C                   MOVEL(P)  arbprm        v7cprm
125900131108     c                   endif
126000131108     c
126100050113     C                   MOVEL     ARBXCO        VIDXCO
126200130221     c                   if        vidxco >*zeros  and no_f22=*blanks
126300110928     c                   seton                                        86
126400110928     c                   endif
126500940719     C                   MOVEL     *BLANKS       V3CPIM
126600940719     C                   MOVEL     *BLANKS       V4CPID
126700061108     c                   clear                   v3cdfm
126800061108     c                   clear                   v4cdfd
126900950411     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
127000060216     C* DA FiAR4 SE RECORD ESISTENTE
127100950411     C                   MOVEL     ARBAAS        KAAS
127200950411     C                   MOVEL     ARBLNP        KLNP
127300950411     C                   MOVEL     ARBNRS        KNRS
127400950411     C                   MOVEL     ARBNSP        KNSP
127500950411     C                   MOVEL     'P'           KTRC
127600060216     C     KAR41         CHAIN     FiAR401L                           31
127700040216if  1C     *IN31         IFEQ      *OFF
127800950411     C                   MOVEL     AR4NOT        V3CPIM
127900950411     C                   MOVE      AR4NOT        V4CPID
128000040216x   1C                   ELSE
128100950411     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI ARBCPI
128200040216if  2C     ARBCPI        IFNE      *BLANKS
128300950411     C  N33              MOVEL     ARBCPI        V4CPID
128400950411     C   33§3ATB2        IFEQ      '  '
128500950411     C                   MOVEL     ARBCPI        V3CPIM
128600950411     C                   ELSE
128700950411     C                   MOVEL     ARBCPI        V4CPID
128800040217     C                   ENDIF
128900040217e   2C                   ENDIF
129000040217e   1C                   ENDIF
129100061108      * valorizzo il codice fiscale del mittente e del destinatario
129200061108      * FIAR4 rcd Q
129300061108     c                   eval      ktrc = 'Q'
129400061108     c     kar41         chain     fiar401l
129500061108if  1c                   if        %found(fiar401l)
129600061108     c                   movel     ar4not        v3cdfm
129700061108     c                   move      ar4not        v4cdfd
129800061108x   1c                   endif
129900980704     C* VISUALIZZO SE C'E' IL RIFERIM PARTNER
130000971219     C                   CLEAR                   V3CEST
130100971219     C                   MOVEL     ARBAAS        KAAS
130200971219     C                   MOVEL     ARBLNP        KLNP
130300971219     C                   MOVEL     ARBNRS        KNRS
130400971219     C                   MOVEL     ARBNSP        KNSP
130500971219     C                   MOVEL     'E'           KTRC
130600060216     C     KAR41         CHAIN     FiAR401L                           31
130700980704     C* VISUALIZZO IN UNA BOLLA ITALIA SE C'E'
130800980704     C* VISUALIZZO SEMPRE IN UNA BOLLA IMPORT/EXPORT
130900971219     C  N31              MOVEL     AR4NOT        V3CRPP
131000980704     C  N31              SETON                                        85
131100980704     C**
131200020225     c                   if        *in31 and
131300020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
131400020225     c                              lnpntw = 'FED') or
131500020225     c                             (lnantw = 'EEX' or lnantw = 'EUP' or
131600020225     c                              lnantw = 'FED')
131700980704     C                   SETON                                        85
131800980704     C                   ENDIF
131900980704     C**
132000940719     C                   MOVEL     ARBRSD        V4CRSD
132100131108     C**********         MOVEL     ARBRSD        V30RSD
132200940713     C                   MOVEL     ARBRSD        V32RSD
132300940719     C                   MOVEL     ARBIND        V4CIND
132400131119     C                   MOVEL     ARBIND        V33IND
132500940719     C                   MOVEL     ARBPRD        V4CPRD
132600940719     C                   MOVEL     ARBCAD        V4CCAD
132700940719     C                   MOVEL     ARBLOD        V4CLOD
132800131119     C*****              MOVEL     ARBLOD        V24LOD
132900940719     C                   MOVEL     ARBFIN        V4CFIN
133000940719     C                   MOVEL     ARBNZD        V4CNZD
133100160505
133200160505     c* Giorni di chiusura: Visualizzo la relativa dicitura solo se presenti
133300160505     c                   clear                   v7dggc
133400160505     c                   if        arbgc1<>*blanks or arbgc2<>*blanks
133500160505     c                   eval      v7dggc='GG.Chiusura'
133600160505     c                   endif
133700160505
133800911022     C                   MOVEL     ARBGC1        VIDGC1
133900911022     C                   MOVEL     ARBGC2        VIDGC2
134000911022     C                   MOVEL     ARBCTR        VIDCTR
134100940719     C                   MOVEL     ARBCTS        V6CCTS
134200131108     c                   if        arbnzd<>*blaNKS
134300131108     C                   MOVEL     arbnzd        v7cprd
134400131108     c                   else
134500131108     C                   MOVEL(P)  arbprd        v7cprd
134600131108     c                   endif
134700940719     C* DECODIFICA CODICE TASSAZIONE
134800940719     C                   MOVEL     'CT'          COD
134900940719     C                   MOVEL     *BLANKS       KEY
135000940719     C                   MOVEL     ARBCTS        KEY
135100940719     C     KTAB1         CHAIN     TABEL00F                           31
135200940719     C                   MOVEL     TBLUNI        V6DCTS
135300911022     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
135400911022     C                   MOVEL     'TR'          COD
135500911022     C                   MOVEL     *BLANKS       KEY
135600911022     C                   MOVEL     VIDCTR        UNOCTR            1
135700911022     C                   MOVEL     UNOCTR        KEY
135800940713     C     KTAB1         CHAIN     TABEL                              31
135900911022     C                   MOVEL     TBLUNI        DEFCTR
136000070404     C*****              MOVEL     ARBFTM        V3CFTM
136100150615     c*                  if        arbftm='S'
136200150615     c*                  eval      v3dftm='TASSATA CON TARIFFA MITTENTE'
136300150615     c*                  endif
136400940711     C* DECODIFICA TIPO SERVIZIO BOLLE
136500090604     c                   clear                   ds5e
136600940711     C                   MOVEL     '5E'          COD
136700940711     C                   MOVEL     *BLANKS       KEY
136800940713     C                   MOVEL     ARBTSP        KEY
136900941024     C     KTAB1         CHAIN     TABEL00F                           31
137000090604     C  N31              MOVEL     TBLUNI        ds5e
137100090604     C  N31              MOVEL     §5ed08        V1DTSP
137200940719     C   31              MOVEL     *BLANKS       V1DTSP
137300131127     c                   if        §5eftc<>' '
137400131127     c                   movel     ri            atrtsp
137500160517     c**                 else
137600160517     c**                 movel     hi            atrtsp
137700131127     c                   endif
137800940713     C                   MOVEL     ARBVMD        VIDVMD
137900940713     C                   MOVEL     ARBVAD        VIDVAD
138000940713     C                   MOVEL     ARBIAS        VIDIAS
138100940713     C                   MOVEL     ARBVAS        VIDVAS
138200911022     C                   MOVEL     ARBNAS        VIDNAS
138300950214     C                   CLEAR                   VIDFLS
138400950214     C     ARBNCD        IFNE      *ZERO
138500950214     C     ARBFNS        ORNE      *BLANKS
138600940923     C                   MOVEL     ARBFLS        VIDFLS
138700950214     C                   ENDIF
138800950214     C                   Z-ADD     ARBFLS        V3CFLS
138900911022     C                   MOVEL     ARBNCL        VIDNCL
139000070410     C                   MOVEL     ARBPKB        VIDPKb
139100070410     C                   MOVEL     ARBPKF        VIDPKF
139200070410     C                   MOVEL     ARBVLb        VIDVLb
139300070410     C                   MOVEL     ARBFVb        VIDFVb
139400070410     C                   MOVEL     ARBVLF        VIDVLF
139500070410     C                   MOVEL     ARBFVF        VIDFVF
139600930705     C     ARBNCR        IFLT      99998
139700940711     C                   MOVEL     ARBVLC        VIDVLC
139800940711     C                   MOVEL     ARBNCR        VIDNCR
139900921231     C                   ELSE
140000990909     C                   Z-ADD     0             VIDVLC
140100990909     C                   Z-ADD     0             VIDNCR
140200921231     C                   END
140300990909     C     ARBNCP        IFLT      99998
140400990909     C                   MOVEL     ARBPKC        VIDPKC
140500990909     C                   MOVEL     ARBNCP        VIDNCP
140600990909     C                   ELSE
140700990909     C                   Z-ADD     0             VIDPKC
140800990909     C                   Z-ADD     0             VIDNCP
140900990909     C                   END
141000940711     C                   MOVEL     ARBQFT        VIDQFT
141100031119
141200031119      * Referente e Telefono del destinatario
141300031119     c                   Clear                   V4cRef
141400031119     c                   Clear                   V4cTeld
141500091214     c                   clear                   w_cdi
141600150622     c                   clear                   v03dac
141700150622     c                   clear                   v03orc
141800150622     c                   clear                   v03pru
141900150622     c                   clear                   v03nmtd
142000031119      * li prendo da Fiar500f record 'GEN'
142100130320     c                   clear                   v1dAR8l
142200130320     c                   clear                   v1dAR8
142300031119     c                   Clear                   dAr5Gen
142400031119     c                   Z-Add     ArbAas        Kaas
142500031119     c                   Z-Add     ArbLNp        Klnp
142600031119     c                   Z-Add     ArbNrs        Knrs
142700031119     c                   Z-Add     ArbNsp        Knsp
142800031119     c                   Eval      Ktrd = 'GEN'
142900031119     c     kAr51         Chain     Fiar501l
143000031119If  1c                   If        %Found(Fiar501l)
143100031119     c                   Movel     Ar5Uni        Dar5Gen
143200031119     c                   Eval      V4cRef = §Ar5Ref
143300031119     c                   Eval      V4cTeld = §Ar5Teld
143400091214     c* memorizzo eventuale codice beneficiario assegno
143500091214     c                   eval      w_cdi=§ar5cdi
143600150622     c* Immissione bolla:
143700150622     c* Visualizzo dat/ora creazione del fiar5 record GEN come "immissione"
143800150622     c* perchè in immissione bolle viene creato fiar5 rek "GEN" per memorizzare
143900150622     c* utente/terminale di immissione. Non uso BLPDIM anche perchè non ho l'ora ma solo
144000150622     c* la data su blp
144100150622     c     *iso          movel     ar5dac        datadmy
144200150622     c                   movel     datadmy       v03dac
144300150622     c                   movel     ar5orc        v03orc
144400150622     c* data/ora
144500150622     c* profilo utente e terminale
144600150622     c                   Eval      V03pru = §Ar5bpru
144700150622     c                   Eval      V03nmtd = §Ar5bnmtd
144800130320     c* verifico se LDV con packing-list e/o PDF
144900130320sel 2c                   select
145000130320w   2c                   when      §AR5als = 'S'
145100130320     c                   eval      v1dAR8l = 'PACKING LIST PDF'
145200130320     c                   eval      v1dAR8  = 'PACKING PDF '
145300130320w   2c                   when      §AR5alx = 'S'
145400130320     c                   eval      v1dAR8l = 'PACKING LIST'
145500130320     c                   eval      v1dAR8  = 'PACKING LIST'
145600130320e   2c                   endsl
145700031119    1c                   EndIf
145800040301
145900040301      * Se colli di valore visualizzo "V" nell'apposito campo
146000040301      *  con lo specifico attributo di visualizzazione
146100040301if  1c                   if        §AR5bva = *blanks
146200040301     c                   eval      VIDbva  = *blanks
146300150612     c                   eval      V7Dbva  = *blanks
146400040301x   1c                   else
146500040301     c                   eval      VIDbva  = 'V'
146600150612     c                   eval      V7Dbva  = 'VALORE'
146700040301e   1c                   endif
146800110601
146900110610     c* email destinatario
147000110601     c                   setoff                                       60
147100110601     c                   clear                   v4cemw
147200110610     c                   clear                   v4cemd
147300130927     c                   Clear                   dAr5emd
147400110601      * li prendo da Fiar500f record 'EMD'
147500110601     c                   Z-Add     ArbAas        Kaas
147600110601     c                   Z-Add     ArbLNp        Klnp
147700110601     c                   Z-Add     ArbNrs        Knrs
147800110601     c                   Z-Add     ArbNsp        Knsp
147900110601     c                   Eval      Ktrd = 'EMD'
148000130927     c***  kAr51         setll     Fiar501l
148100130927     c     kAr51         chain     Fiar501l
148200130927If  1c                   If        %found(Fiar501l)
148300130927     c                   eval      dar5emd=ar5uni
148400130927     c                   if        §ar5tel<>*blanks or §ar5eml<>*blanks
148500110610     c                   Eval      V4cemd = 'SI'
148600110601     c                   seton                                        60
148700130927     c                   endif
148800110601    1c                   EndIf
148900040301
149000030121      * Bancali
149100030121     c                   Clear                   VidTba
149200030121     c                   Clear                   VidTb2
149300030122     c                   Clear                   VidNba
149400030122     c                   Clear                   VidNb2
149500030703      * Record 'BAN'
149600030703If  0c                   If        %subst(ArbGva:1:1) = 'B'
149700030121     c                   Clear                   dAr5Ban
149800030121     c                   Eval      Ktrd = 'BAN'
149900030121     c     kAr51         Chain     Fiar501l
150000030121If  1c                   If        %Found(Fiar501l)
150100030121     c                   Movel     Ar5Uni        Dar5Ban
150200030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
150300030122      * per le note relative al mancato reso dei bancali
150400030122     c                   If        Not *In77 and
150500030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
150600030122     c                   Eval      *In77 = *On
150700030122     c                   EndIf
150800030121     c                   Eval      VidNba = §Ar5Nba
150900030121     c                   Eval      VidNb2 = §Ar5Nb2
151000030121If  2c                   If        §Ar5Tba <> *Zeros
151100030703     c                   Eval      wtba = §Ar5Tba
151200030703     c                   ExSr      Sr_DecTba
151300030703     c                   Eval      VidTba = wdestba
151400030121    2c                   EndIf
151500030121If  2c                   If        §Ar5Tb2 <> *Zeros
151600030703     c                   Eval      wtba = §Ar5Tb2
151700030703     c                   ExSr      Sr_DecTba
151800030703     c                   Eval      VidTb2 = wdestba
151900030121    2c                   EndIf
152000030121    1c                   EndIf
152100030703    0c                   EndIf
152200030703      * Record 'BNB'  -  Spedizione OVER 25 colli
152300030703If  0c                   If        %subst(ArbGva:1:1) = 'O'
152400030703     c                   Clear                   dAr5Bnb
152500030703     c                   Eval      Ktrd = 'BNB'
152600030703     c     kAr51         Chain     Fiar501l
152700030703If  1c                   If        %Found(Fiar501l)
152800030703     c                   Movel     Ar5Uni        Dar5Bnb
152900030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
153000030703      * per i colli originali in caso di spedizione over 25 colli
153100030703     c  n77              Eval      *In77 = *On
153200030703     c                   Eval      VidNba = §Ar5bNba
153300030703     c                   Eval      VidNb2 = §Ar5bNb2
153400030703If  2c                   If        §Ar5bTba <> *Zeros
153500030703     c                   Eval      wtba = §Ar5bTba
153600030703     c                   ExSr      Sr_DecTba
153700030703     c                   Eval      VidTba = wdestba
153800030703    2c                   EndIf
153900030703If  2c                   If        §Ar5bTb2 <> *Zeros
154000030703     c                   Eval      wtba = §Ar5bTb2
154100030703     c                   ExSr      Sr_DecTba
154200030703     c                   Eval      VidTb2 = wdestba
154300030703    2c                   EndIf
154400030703    1c                   EndIf
154500030703    0c                   EndIf
154600911129     C*
154700131120     C**   ARBACC        IFEQ      'S'
154800131120     C**                 MOVEL     'SI'          V5CACC
154900131120     C**                 ELSE
155000131120     C**                 MOVEL     'NO'          V5CACC
155100131120     C**                 END
155200160825     c     W_tbo         ifeq      'A'
155300160825     C     ARBICC        andne     ' '
155400941216     C     ARBICC        ANDNE     'R'
155401160825     c     w_tbo         oreq      'P'
155402160825     c     arbdcm        andgt     0
155403160825     c
155500930218     C                   MOVEL     'SI'          VIDICC
155600150615     C                   eval      v7dicc='Incassato'
155700930218     C                   END
155900911022     C*
156000911022     C                   MOVEL     ARBRMN        VIDRMN
156100940712     C                   MOVEL     ARBRMA        VIDRMA
156200911022     C                   MOVEL     ARBRMO        VIDRMO
156300911022     C                   MOVEL     ARBNCD        VIDNCD
156400911022     C                   MOVEL     ARBNCA        VIDNCA
156500911022     C                   MOVEL     ARBZNC        VIDZNC
156600051104     c                   select
156700051104     c                   when      arbffd='S'
156800051104     C                   MOVEL     'SI '         VIDFFD
156801161104     c                   eval      v1dffd='F.DEPOSITO'
156900150615     c***                when      arbffd='M'
157000150615     C***                MOVEL     'SAB'         VIDFFD
157100051104     C                   other
157200051104     C                   MOVEL     '   '         VIDFFD
157201161104     c                   clear                   v1dffd
157300051104     C                   ENDsl
157400051124     c
157500940719     C                   MOVEL     ARBNFV        V3CNFV
157600051124     c* richiamp pgm per decodifica
157700051124     C                   MOVEL     *BLANKS       VIDTCR
157800051124     c                   clear                   vidhcr
157900051124     c                   clear                   viddcr
158000051124     c                   if        arbdcr>0 or arbhcr>0
158100051124     c                   clear                   fnlv80ds
158200051124     c                   eval      ilv80aas=arbaas
158300051124     c                   eval      ilv80lnp=arblnp
158400051124     c                   eval      ilv80nrs=arbnrs
158500051124     c                   eval      ilv80nsp=arbnsp
158600051124     c                   eval      ilv80mgs=arbmgs
158700051124     c                   eval      ilv80tc1=arbtc1
158800051124     c                   eval      ilv80tc2=arbtc2
158900051124     c                   eval      ilv80dcr=arbdcr
159000051124     c                   eval      ilv80hcr=arbhcr
159100051124     c                   eval      ilv80tcr=arbtcr
159200051124     c                   call      'FNLV80R'
159300051124     c                   parm                    kpjba
159400051124     c                   parm                    fnlv80ds
159500051124     c
159600051124     c                   eval      viddcr=olv80dcr8
159700051124     c                   eval      vidtcr=olv80tcrd
159800911023     C                   MOVEL     ARBHCR        VIDHCR
159900051124     c                   endif
160000051124     c
160100070521     c* Se presente ora consegna, la edito
160200161014     c**                 clear                   vidhmc
160300161014     c**                 if        arbhmc>0
160400161014     c**                 eval      vidhmc=%editw(arbhmc: '0 :  ')
160600161014     c**                 endif
160700911023     C                   MOVEL     ARBNTC        VIDNTC
160800911023     C                   MOVEL     ARBTMC        VIDTMC
160900940923     C*
161000940923     C* DECODIFICO IL TIPO MANCATA CONSEGNA
161100940923     C                   MOVEL     *BLANKS       V5DTMC
161200940923     C     ARBTMC        IFNE      *BLANKS
161300940923     C                   MOVEL     '2A'          COD
161400940923     C                   MOVEL     *BLANKS       KEY
161500940923     C                   MOVEL     ARBTMC        KEY
161600940923     C     KTAB1         CHAIN     TABEL00F                           31
161700940923     C  N31              MOVEL     TBLUNI        V5DTMC
161800940923     C                   ENDIF
161900940923     C*
162000911023     C                   MOVEL     ARBMIF        VIDMIF
162100911023     C                   MOVEL     ARBNMI        VIDNMI
162200940923     C*
162300940713     C                   MOVEL     ARBGGA        V4CGGA
162400940923     C                   MOVEL     *BLANKS       V4IGGA
162500940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
162600940923     C     V4CGGA        COMP      *BLANKS                            4242
162700940923     C*
162800940713     C                   MOVEL     ARBGMA        V4CGMA
162900940923     C                   MOVEL     *BLANKS       V4IGMA
163000940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
163100940923     C     V4CGMA        COMP      *BLANKS                            4343
163200940923     C*
163300960208     C                   MOVEL     ARBGVA        V4CGVA
163400960208     C                   MOVE      ' '           V4CGVA
163500940923     C                   MOVEL     *BLANKS       V4IGVA
163600940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
163700940923     C     V4CGVA        COMP      *BLANKS                            4444
163800940923     C*
163900070403     C* FLAG blocco CONSEGNA
164000070403     C                   SELECT
164100070403     c                   WHEN      arbfbc='A'
164200070403     c                   eval      v1dfbc='LascAVVISO'
164300070403     C                   SETON                                        27
164400070403     c                   WHEN      arbfbc='B'
164500070403     c  n17              eval      v1dfbc=' BLOCCATA '
164600070412     c   17              eval      v1dfbc=' RIMPALLO '
164700070412     C                   SETON                                        27
164800070403     c                   WHEN      arbfbc='*'
164900070403     c  n17              eval      v1dfbc=' BLOCCATA '
165000070403     c   17              eval      v1dfbc=' GIACENTE '
165100070403     C                   SETON                                        27
165200070403     C                   ENDSL
165300070403     c
165400070403     C**   ARBFBC        COMP      'A'                                    27
165500070403     C**   ARBFBC        COMP      'B'                                    17
165600070403     C**N17ARBFBC        COMP      '*'                                    17
165700940711     C* TIPO CONSEGNA PARTICOLARE 1
165800940804     C                   MOVEL     *BLANKS       V5DTC1
165900150615     C                   MOVEL     *BLANKS       V7DTC1
166000940711     C     ARBTC1        IFNE      ' '
166100911115     C                   MOVEL     '1P'          COD
166200911115     C                   MOVEL     *BLANKS       KEY
166300940711     C                   MOVEL     ARBTC1        KEY
166400940713     C     KTAB1         CHAIN     TABEL                              31
166500940804     C  N31              MOVEL     TBLUNI        V5DTC1
166600940804     C   31              MOVEL     *BLANKS       V5DTC1
166700150615     C  N31              MOVEL     TBLUNI        V7DTC1
166800150615     C   31              MOVEL     *BLANKS       V7DTC1
166900930113     C                   END
167000940711     C* TIPO CONSEGNA PARTICOLARE 2
167100940804     C                   MOVEL     *BLANKS       V5DTC2
167200940804     C                   MOVEL     *BLANKS       V7DTC2
167300940711     C     ARBTC2        IFNE      ' '
167400940711     C                   MOVEL     '1P'          COD
167500940711     C                   MOVEL     *BLANKS       KEY
167600940711     C                   MOVEL     ARBTC2        KEY
167700940713     C     KTAB1         CHAIN     TABEL                              31
167800940804     C  N31              MOVEL     TBLUNI        V5DTC2
167900940804     C   31              MOVEL     *BLANKS       V5DTC2
168000940804     C  N31              MOVEL     TBLUNI        V7DTC2
168100940804     C   31              MOVEL     *BLANKS       V7DTC2
168200940711     C                   END
168300930113     C* TIPO MANCATA CONSEGNA
168400930113     C                   MOVEL     *BLANKS       DESCCA
168500160505     C                   MOVEL     *BLANKS       DESCCA7
168600160506     C                   clear                   v7dcca
168601160610     C                   clear                   ds7o
168602160610     C                   clear                   w_ccaleg
168603161014     C                   clear                   w_orig
168700930113     C     ARBCCA        IFNE      ' '
168800930705     C                   Z-ADD     1             C
168900930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
169000160505     C   31              MOVEL     DCA(C)        ALF20            20
169100160505     C  N31              MOVEL     *BLANKS       ALF20
169101160610     C   31              MOVEL     dca(c)        ds7o
169200160505     c                   eval      descca ='- ' + %subst(alf20:1:12)
169300160505     c                   eval      descca7=arbcca+'-'+alf20
169400160506     c                   eval      v7dcca='C.Anomala'
169401160610     c                   if        §7otle<>*blanks
169402161014     c     karb2         chain     fnlbl02l
169403161014     c                   if        %found(fnlbl02l)
169404160610     c                   eval      w_ccaleg='S'
169405160614     c                   seton                                        84
169406161014     c                   if        LBLAAO=LBLAAP and
169407161014     c                             LBLLPO=LBLLPP and
169408161014     c                             LBLNRO=LBLNRP and
169409161014     c                             LBLNSO=LBLNSP
169410161014     c                   eval      w_orig  ='S'
169411161014     c                   endif
169414160610     c                   endif
169415160610     c                   endif
169500930113     C                   END
169600911022     C* GIRO DATA SPEDIZIONE
169700940712     C                   Z-ADD     ARBDSP        G02INV
169800940712     C                   Z-ADD     *ZEROS        G02DAT
169900940712     C                   MOVEL     '3'           G02ERR
170000940712     C                   CALL      'XSRDA8'
170100940712     C                   PARM                    WLBDAT
170200940719     C                   Z-ADD     G02DAT        V3CDSP
170300070403     c*
170400911022     C* DATA BORDERO'
170500070403     c                   if        arbdbr>0
170600070403     c     *iso          movel     arbdbr        dataeur
170700070403     c                   movel     dataeur       datadmy
170800070404     c                   movel     dataeur       w08dbr            8 0
170900070404     c                   movel     datadmy       v06dbr
171000070403     c                   else
171100070404     c                   clear                   v06dbr
171200070403     c                   endif
171300070404     c
171400070404     c* Prendo da fnblp i dati ritiro merce
171500150313     c***  karb2         chain     fnblp01l
171600150313     c***                if        %found(fnblp01l) and blpdrt>0
171700150313     c                   if        arbdrt >0
171800150313     c     *iso          movel     arbdrt        dataeur
171900070403     c                   movel     dataeur       datadmy
172000070404     c                   movel     datadmy       v06drt
172100150313     c                   movel     arbnrt        v7cnrt
172200150313     c                   movel     arbctm        v3cctm
172300150313     c                   if        arbfpp='M'
172400070410     c                   eval      v7dfppr='MAT'
172500070404     c                   else
172600070410     c                   eval      v7dfppr='POM'
172700070404     c                   endif
172800150313     C                   MOVEL     arbPDR        V7CPDR
172900150313     C                   movel     arbPdr        kpdpdr
173000070404     C                   move      'A'           kpdtip
173100070404     C     kfapd         chain     fiapd01L
173200070404     c                   if        %found(fiapd01l)
173300070404     C                   MOVEL     APDRSC        V7DPDR
173400070404     c                   else
173500070404     C                   MOVEL     *BLANKS       V7DPDR
173600070404     c                   endif
173700070404     c
173800070404     c
173900070403     c                   else
174000070404     c                   clear                   v06drt
174100070404     c                   clear                   v7cnrt
174200070406     c                   clear                   v3cctm
174300070504     c                   clear                   v7dfppr
174400070404     c                   clear                   v7dpdr
174500070404     c                   clear                   v7cpdr
174600070403     c                   endif
174700070403     c
174800961112     C* SOLO PER LE BOLLE EXPORT VISUALIZZO LA DATA STAMPA MANIFEST
174900070404     C                   CLEAR                   V06DT3
175000070413     c                   if        *in16 and arbdt3>0  and arbdt3<>99999999
175100070404     c     *iso          movel     arbdt3        dataeur
175200070404     c                   movel     dataeur       datadmy
175300070404     c                   movel     datadmy       v06dt3
175400961112     C                   ENDIF
175500940713     C* DATA PARTENZA PRIMO COLLO
175600070404     c                   if        arbdpc>0
175700070404     c     *iso          movel     arbdpc        dataeur
175800070404     c                   movel     dataeur       datadmy
175900070404     c                   movel     datadmy       v06dpc
176000070404     c                   movel     dataeur       w08dpc
176100070404     c                   else
176200070404     c                   clear                   v06dpc
176300070404     c                   clear                   w08dpc            8 0
176400070404     c                   endif
176500940713     C* DATA PARTENZA ULTIMO COLLO
176600070403     C**                 Z-ADD     ARBDUC        G02INV
176700070403     C**                 Z-ADD     *ZEROS        G02DAT
176800070403     C**                 MOVEL     '3'           G02ERR
176900070403     C**                 CALL      'XSRDA8'
177000070403     C**                 PARM                    WLBDAT
177100070403     C**                 Z-ADD     G02DAT        V3CDUC
177200070403     c                   if        arbduc>0
177300070404     c     *iso          movel     arbduc        dataeur
177400070403     c                   movel     dataeur       datadmy
177500070404     c                   movel     datadmy       v06duc
177600070404     c                   movel     dataeur       w08duc
177700070403     c                   else
177800070404     c                   clear                   v06duc
177900070404     c                   clear                   w08duc            8 0
178000070403     c                   endif
178100911022     C* DATA ENTRATA AL TRANSITO
178200070404     c                   if        arbdet>0
178300070404     c     *iso          movel     arbdet        dataeur
178400070404     c                   movel     dataeur       datadmy
178500070404     c                   movel     datadmy       v06det
178600070404     c                   else
178700070404     c                   clear                   v06det
178800070404     c                   endif
178900911022     C* DATA USCITA DAL TRANSITO
179000070404     c                   if        arbdut>0
179100070404     c     *iso          movel     arbdut        dataeur
179200070404     c                   movel     dataeur       datadmy
179300070404     c                   movel     datadmy       v06dut
179400070404     c                   else
179500070404     c                   clear                   v06dut
179600070404     c                   endif
179700930113     C* DATA ARRIVO MERCE
179800070403     c                   if        arbdam>0
179900160506     c                   setoff                                       96
179901160914     c                   clear                   v7ddam
180000070403     c     *iso          movel     arbdam        dataeur
180100070403     c                   movel     dataeur       datadmy
180200070404     c                   movel     datadmy       v06dam
180201160914     c                   if        v7dlnaa<>*blanks
180300150616     c                   clear                   fnlrj6ds
180400150616     c                   eval      ILRJ6TRIC='A'
180500150616     c                   eval      ILRJ6AAS =arbaas
180600150616     c                   eval      ILRJ6LNP =arblnp
180700150616     c                   eval      ILRJ6NRS =arbnrs
180800150616     c                   eval      ILRJ6NSP =arbnsp
180900150616     c                   call      'FNLRJ6R'
181000150616     c                   parm                    fnlrj6ds
181001160517     c* Arrivo alla filiale finale di arrivo:
181002160517     c* Totale
181100150616     c                   if        olrj6err=*blanks and olrj6fall='S'
181200150616     c                   eval      v7ddam='Totale'
181300150616     c                   else
181301160517     c* Parziale
181302160517     c                   if        OLRJ6CDCM+OLRJ6CSPU>0
181303160517     c                   eval      v7ddam='Parziale'
181304160517     c                   else
181305160517     c* Nessuno
181306160517     c                   eval      v7ddam='Nessuno '
181307160517     c                   endif
181500150616     c                   endif
181501160914     c                   endif
181600070403     c                   else
181700160506     c                   seton                                        96
181800070404     c                   clear                   v06dam
181900070403     c                   endif
182000150615
182100150615
182200150615     c
182300131121
182400930504     C* DATA CONSEGNA MERCE REALE
182500160610     c                   if        arbdcm>0  and w_ccaleg=*blanks
182600070413     C                   SETON                                        69
182601161014     c****               eval      v7cdcon='REALE'
182602161017     c                   if        arbcca=*blanks and §3arbl<>'R'
182603161031     c                             and arbcbo<>'M '
182604161014     c                   eval      v7dcons='CONSEGNATA:'
182605161014     c                   eval      v6ddcm ='CONSEGNA'
182606161014     c                   eval      v5ddcm ='REALE '
182607161014     c                   else
182608161014     c                   eval      v7dcons='CHIUSA . .:'
182609161014     c                   eval      v5ddcm ='CHIUSA'
182610161017     c                   eval      v6ddcm ='CHIUSA'
182611161017     c                   seton                                        19
182612161014     c                   endif
182700070404     c     *iso          movel     arbdcm        dataeur
182800070404     c                   movel     dataeur       datadmy
182900070404     c                   movel     dataeur       v08dcm
183000070404     c                   movel     datadmy       v06dcm
183001161014     c                   if        arbhmc>0
183002161014     c                   eval      vidhmc=%editw(arbhmc: '0 :  ')
183004161014     c                   endif
183100070404     c                   else
183200070404     c                   clear                   v06dcm
183300070404     c                   clear                   v08dcm
183301161014     c* solo se originale
183302161014     c                   if        blp_arbdcm>0 and W_orig='S'
183303161018     c                   if        (arbcca<>'2' and arbcca<>'6') or
183304161018     c                             blp_arbdcm<>arbdcm
183305160914     C                   SETON                                        69
183306161014     c                   eval      v7dcons='CONSEGNATA:'
183307161014     c                   eval      v6ddcm ='CONSEGNA'
183308161014     c                   eval      v5ddcm ='REALE  '
183309160913     c     *iso          movel     blp_arbdcm    dataeur
183310160913     c                   movel     dataeur       datadmy
183311160913     c                   movel     dataeur       v08dcm
183312160913     c                   movel     datadmy       v06dcm
183313161014     c                   if        blp_arbhmc>0
183314161014     c                   eval      vidhmc=%editw(blp_arbhmc: '0 :  ')
183315161014     c                   endif
183316161018     c                   endif
183317161014     c                   else
183318161014     c                   if        w_orig<>'S' and arbdcm>0
183319161014     c                   eval      v6ddcm ='CHIUSA'
183320161014     c                   eval      v5ddcm ='CHIUSA'
183321161017     c                   seton                                        19
183322161014     c     *iso          movel     arbdcm        dataeur
183323161014     c                   movel     dataeur       datadmy
183324161014     c                   movel     dataeur       v08dcm
183325161014     c                   movel     datadmy       v06dcm
183326161014     c                   if        arbhmc>0
183327161014     c                   eval      vidhmc=%editw(arbhmc: '0 :  ')
183328161014     c                   endif
183329161014     c                   endif
183330160913     c                   endif
183400070404     c                   endif
183500131121
183600930505     C* DATA CONSEGNA PREVISTA ESTERNA/ORE DI RITARDO
183700070404     C                   Z-ADD     0             v08DCE
183800140113     C                   Z-ADD     0             amgDCE            8 0
183900150615     C*******            Z-ADD     0             v06dce
184000940711     C                   Z-ADD     0             VIDNRC
184100131121     C*******            SETON                                        68
184200131121     C                   MOVEL     ' '           D98TLA
184300150611     C                   MOVEL     w_tbo         D98TBO
184400131121     C                   MOVEL     ARBAAS        D98AAS
184500131121     C                   MOVEL     ARBLNP        D98LNP
184600131121     C                   MOVEL     ARBNRS        D98NRS
184700131121     C                   MOVEL     ARBNSP        D98NSP
184800131121     C*
184900131121     C                   CALL      'TNSD99R'
185000131121     C                   PARM                    TNSD99DS
185100140110     c
185200140110     c* Mi salvo data teorica consegna
185300140110     c                   eval      s_arbdei=d98dei
185400140110     c                   eval      s_arboei=d98oei
185500150729     c
185600150729     c* memorizzo ore trazione è ore consegna cliente da passare al trulorsr
185700150729     c                   eval      s_arbttc=d98ttc
185800150729     c                   eval      s_arbtcc=d98tcc
185900131121     C*
186000140110    1C     ARBDCE        IFEQ      0
186100131121    2c                   if        d98dce>0
186200140113     c                   eval      amgdce=d98dce
186300131121     c     *iso          movel     d98dce        dataeur
186400150615     c****               movel     dataeur       datadmy
186500131121     c                   movel     dataeur       v08dce
186600150615     c****               movel     datadmy       v06dce
186700131121   x2c                   else
186800131121     c                   clear                   v08dce
186900150615     c***                clear                   v06dce
187000131121    2c                   endif
187100131121     c
187200131121     C                   Z-ADD     D98NRC        VIDNRC
187201160523     c*
187500160523     c**n69              eval      vidhmc=%editw(d98hce: '0 :  ')
187600131121     C*
187700131121     C* SE COMODO = 1  E' STATA FATTA CALL PGM
187800131121   x1C                   ELSE
187900140113     c                   eval      amgdce=arbdce
188000070404     c     *iso          movel     arbdce        dataeur
188100150615     c***                movel     dataeur       datadmy
188200070404     c                   movel     dataeur       v08dce
188300150615     c***                movel     datadmy       v06dce
188400070404     c
188500940711     C                   Z-ADD     ARBNRC        VIDNRC
188600131121    1C                   END
188700131121     c
188800911023     C* DATA MANDATO INTROITO
188900940712     C                   Z-ADD     ARBDMI        G02INV
189000940712     C                   Z-ADD     *ZEROS        G02DAT
189100940712     C                   MOVEL     '3'           G02ERR
189200940712     C                   CALL      'XSRDA8'
189300940712     C                   PARM                    WLBDAT
189400940712     C                   Z-ADD     G02DAT        VIDDMI
189500930113     C* DATA PRIMA CONSEGNA PARZIALE
189600930113     C                   Z-ADD     0             VIDDCP
189700150615     C***                Z-ADD     0             VIDDCP6
189800930113     C     ARBDCP        IFGT      0
189900940712     C                   Z-ADD     ARBDCP        G02INV
190000940712     C                   Z-ADD     *ZEROS        G02DAT
190100940712     C                   MOVEL     '3'           G02ERR
190200940712     C                   CALL      'XSRDA8'
190300940712     C                   PARM                    WLBDAT
190400940712     C                   Z-ADD     G02DAT        VIDDCP
190500150615     c***  *iso          move      arbdcp        datadmy
190600150615     c***                movel     datadmy       viddcp6
190700150611     c
190800930113     C                   END
190900160505     c* Visualizzo data distinta e padroncino solo se numero distinta>0
191000160505     c                   clear                   vidddc
191100160505     c                   clear                   v7ddel
191200160505     c                   clear                   vidpdc
191300160505     c                   clear                   v7daut
191400160505     C                   MOVEL     *BLANKS       DESPDC
191500160505     C                   MOVEL     *BLANKS       DESPD1
191600160505     c                   if        arbndc>0
191700160505     C* DATA DISTINTA -
191800940712     C                   Z-ADD     ARBDDC        G02INV
191900940712     C                   Z-ADD     *ZEROS        G02DAT
192000940712     C                   MOVEL     '3'           G02ERR
192100940712     C                   CALL      'XSRDA8'
192200940712     C                   PARM                    WLBDAT
192300940712     C                   Z-ADD     G02DAT        VIDDDC
192400160505     c                   if        vidddc>0
192500160505     c                   eval      v7ddel='del'
192600160505     c                   endif
192700911022     C* CODICE PADRONCINO CONSEGNA
192800940712     C                   MOVEL     ARBPDC        VIDPDC
192900160505     c                   if        vidpdc>0
193000160505     c                   eval      v7daut='Autotr'
193100160505     c                   endif
193200940712     C     ARBPDC        IFGT      0
193300021203     C                   movel     arbpdc        kpdpdr
193400021203     C                   move      'A'           kpdtip
193500021203     C     kfapd         chain     fiapd01L                           31
193600940714     C  N31              MOVEL     APDRSC        DESPDC
193700940714     C  N31              MOVEL     APDRSC        DESPD1
193800930705     C                   END
193900160505     c                   endif
194000950324     C* PRESTAZIONE PADRONCINO MATTINO O POMERIGGIO
194100950324     C                   MOVE      *BLANKS       FVVFPP
194200131120     C***                MOVE      *BLANKS       V5DFPP
194300070404     C                   MOVE      *BLANKS       V7DFPP
194400950327     C                   Z-ADD     ARBNDC        WNDC
194500150617     c                   if        w_tbo='A'
194600020906     c                   Move      Arbifp        Wfgs
194700020906     c     Kfvv1         Chain     Fnfvv01l                           31
194800950324     C                   SELECT
194900991027     C                   WHEN      *IN31 = *on
195000991027     C                   WHEN      FVVFPP = 'M'
195100131120     C**                 eval      V5DFPP = 'MATTINO'
195200070504     C                   eval      V7DFPP = 'Mat'
195300991027     C                   WHEN      FVVFPP = 'P'
195400131120     C**                 eval      V5DFPP = 'POMERIGGIO'
195500070504     C                   eval      V7DFPP = 'Pom'
195600950324     C                   ENDSL
195700131119     c* controllo se ha PDA
195800131119     C                   Z-ADD     ARBNDC        kNDC6
195900131120     c                   clear                   vidpda
196000131120     c     Kdst          Chain     Fidst01l
196100131120     c                   if        %found(fidst01l)
196200131119     C                   movel     dstflr        DDStflr
196300131120     c                   if        (§dsttstpda= 'C' or  §dsttstpda= 'E') or
196400131120     c                             (dstpda<>'C' and  dstpda<>'E')
196500131120     c                   eval      vidpda='NO PDA'
196600150618     c*****              movel     ri            ATRpda
196700131121     c                   else
196800131121     c                   eval      vidpda='SI PDA'
196900150618     c****               movel     ri            ATRpda
197000131119     c                   endif
197100131119     c                   endif
197200150617     c                   endif
197300131108     c
197400131108     c* Se bolla in distinta, verifico quanti colli
197500131108     c                   clear                   v7dncl_co
197600131115     c                   clear                   v7dncl_de
197700131108     c                   clear                   wcol_D
197800131108     c                   clear                   wcol_I
197900131108    1c                   if        arbndc>0 and arbddc>0 and arbfbc=' '
198000150616     c                             and w_tbo='A'
198100131108     c* se ritorno all'incasso lo scrivo
198200131108     c
198300150616    2c                   if        arbdcm>0  and (arbicc='R' or arbicc=' ' or
198400131108     c                                  arbica=' ' or arbica='R')
198401160721     c                             and dstfcf<>'S'
198500131115     c                   eval      v7dncl_de='     IN consegna :'
198600131108     c                   eval      v7dncl_co='PER INCASSO'
198700131108    2c                   endif
198800131108     c
198900131108     c* Se colli da consegnare
199000131108    2c                   if        arbdcm=0
199100131108     C                   Z-ADD     ARBNDC        knfv
199200131108     c                   Move      Arbifp        Wfgs
199300131108     c
199400131108     c     karb2         setll     fnart01l
199500131108     c     karb2         reade     fnart01l
199600131108     c
199700131108    3c                   dow       not %eof(fnart01l)
199800131108     c                   if        artdcm=0
199900131108     c                   add       1             wcol_d
200000131108     c     kbrv          chain     fnbrv01l
200100131108     c                   if        %found(fnbrv01l)
200200131108     c                   add       1             wcol_I
200300131108     c                   endif
200400131108     c                   endif
200500131108     c
200600131108     c
200700131108     c     karb2         reade     fnart01l
200800131108    3c                   enddo
200900131108
201000131108    3c                   if        wcol_d>0
201100131115     c                   eval      v7dncl_de='Colli in consegna:'
201200131108    4c                   if        wcol_i=0
201300131108     c                   eval      v7dncl_co='0/'+%trim(%editc(wcol_d:'Z'))
201400131108     c                   else
201500131108     c                   eval      v7dncl_co=%trim(%editc(wcol_I:'Z')) +'/' +
201600131108     c                                       %trim(%editc(wcol_d:'Z'))
201700131108    4c                   endif
201800131108    3c                   endif
201900131108    c
202000131108    2c                   endif
202100131108    1c                   endif
202200131108     c
202300131108     c* Visualizzo ultimo evento
202400131108     c                   clear                   v7duevb
202500150616     c                   clear                   w_ar4_1
202501160614     c                   clear                   w_evbcev
202502161107     c                   clear                   w_tcev
202503160610     c* Se bolla legata con consegna anomala com ultimo evento devo visualizzare
202504160610     c* data e ora in cui è avvenuto il dirottamento
202505161017    1c                   if        w_ccaleg='S'
202506161017    2c                   if        arbcca='9' or w_tbo='P'
202507160913     c* Per cambio di porto la data/ora coincide con la data/ora di creazione della bolla
202508160913     c* figlia
202509160913     c* idem per resi e dirottamenti che si trovano solo su BLP
202510160913     c* In questi casi la DCM contiene infatti la data di consegna della merce e non la data in
202511160913     c* cui è stato fatto il cambio di porto o dirottamento o reso
202512160913     c     karb2         setll     fnlbl02l
202513160913     c     karb2         reade     fnlbl02l
202514161017    3c                   if        not %eof(fnlbl02l)
202515160913     c                   Z-Add     lblAAN        Kaas
202516160913     c                   Z-Add     lblLPN        Klnp
202517160913     c                   Z-Add     lblNRN        Knrs
202518160913     c                   Z-Add     lblNSN        Knsp
202519160913     c                   Eval      Ktrd = 'GEN'
202520160913     c     kAr51         Chain     Fiar501l
202521161017If  4c                   If        %Found(Fiar501l)
202523160913     c     *iso          movel     ar5dac        dataeur
202524160913     c                   movel     dataeur       w0080
202525160913     c                   movel     ar5orc        w0040
202526160913     c                   eval      v7duevb=%trim(§7odei)+' il ' +
202527160913     c                             %editw(w0080:'  /  /    ')  + ' '
202528160913     c                             + %editw(w0040: '0 :  ')
202529161017    4C                   ENDIF
202530161017    3c                   endif
202531160913     c
202532161017   x2c                   else
202533161017    3c                   if        arbdcm>0
202534160614     c     *iso          movel     arbdcm        dataeur
202535160614     c                   movel     dataeur       w08dcm
202536161017     c**                 if        arbhmc>0
202537161017     c***                eval      vidhmc=%editw(arbhmc: '0 :  ')
202538161017     c***                endif
202539160610     c                   eval      v7duevb=%trim(§7odei)+' il ' +
202540161017     c                             %editw(w08dcm:'  /  /    ')
202541161017    4c                   if        arbhmc>0
202542161017     c                   eval      v7duevb=%trim(v7duevb) + ' ' +
202543161017     c                             %editw(arbhmc: '0 :  ')
202544161017    4c                   endif
202545161017     c***                          %editw(w08dcm:'  /  /    ')  + ' ' + vidhmc
202546161017   x3c                   else
202547160913     c                   eval      v7duevb=%trim(§7odei)
202548161017    3c                   endif
202549161017    2c                   endif
202550161017   x1c                   else
202600131114     c* prima vedo se c'e' da PDA
202700131114     c                   move      arbndc        pctndc
202800140224     c                   move      arbifp        pctfgs
202900131114     c                   move      'CET'         ttrd
203000131114     c     kcet          setgt     fipct02l
203100131114     c     kcet          readpe    fipct02l
203200161017    2c                   if        not %eof(fipct02l)
203300131114     c                   movel     pctdati       fipctcetds
203400131114     c* consegnata
203500161017    3c                   if        §pctcmc = ' '
203600131114     c                   MOVE      '  C'         evbcev                         *causale evento
203700161017   x3c                   else
203800131114     c* mancata consegna
203900131114     c                   movel     §PCTcmc       evbcev
204000161017    3c                   endif
204100131114     c                   z-add     §pctdata      evbdev
204200131114     c                   movel     §pctora       evbhev
204300131114
204400131114     c                   exsr      Decodi_EVB
204401160609     c                   eval      v7duevb=%trim(v7duevb)+' ' +
204402160609     c                                                 'da PDA'
204403161108     c                   eval      wcev=§pctriserv
204404161108     c                   exsr      sr_2a
204405161108     c                   if        §2atip='R'
204406161108     c                   eval      v7duevb=%trim(v7duevb)+  red
204407161108     c                             + %trim(§2ades) + c_DspAtrNorm
204408161108     c                   endif
204414161107     c                   eval      w_tcev='D'
204500131114
204600161017   x2c                   else
204700150616     c* se spedizione consegnata e presente firmatario lo visulizzo
204800150616     c* al posto dell'ultimo evento
204900161017    3c                   if        arbdcm>0
205000150616     C                   MOVEL     ARBAAS        KAAS
205100150616     C                   MOVEL     ARBLNP        KLNP
205200150616     C                   MOVEL     ARBNRS        KNRS
205300150616     C                   MOVEL     ARBNSP        KNSP
205400150616     C                   MOVEL     '1'           KTRC
205500150616     c     kar41         chain     fiar401l
205600161017    4c                   if        %found(fiar401l) and ar4not<>*blanks
205700150616     c                   eval      evbcev='  C'
205800150616     c                   clear                   evbdev
205900150616     c                   clear                   evbhev
206000150616     c                   eval      w_ar4_1='S'
206100150616     c                   exsr      Decodi_EVB
206200161017    4c                   endif
206300161017    3c                   endif
206400131114     c*
206500161108    3c****               if        w_ar4_1=*blanks
206600131108     C                   Z-ADD     ARBAAS        KAAS$
206700131108     C                   Z-ADD     ARBLNP        KLNP
206800131108     C                   Z-ADD     ARBNRS        KNRS
206900131108     C                   Z-ADD     ARBNSP        KNSP
207000131108     C     KEVB1         SETgt     FNEVB01L
207100131108     C     KEVB1         readpe    FNEVB01L
207200131108     c
207300161017    4c                   if        not %eof(fnevb01l)
207301161108    5c                   if        w_ar4_1=*blanks
207400131114     c                   exsr      Decodi_EVB
207401161108   x5c                   else
207402161108     c* Se spedizione consegnata e ultimo evento di riserva visualizzo di fianco alla Firma
207403161108     c                   eval      wcev=evbcev
207404161108     c                   exsr      sr_2a
207413161108     c                   if        §2atip='R'
207414161108     c                   eval      v7duevb=%trim(v7duevb)+  red
207415161108     c                             + %trim(§2ades) + c_DspAtrNorm
207416161108     c                   endif
207417161108    5c                   endif
207500161017    4c                   endif
207600161108    3c****               endif
207700161017    2c                   endif
207702161017    1c                   endif
207800160505     c                   clear                   v7duevd
207900160506     c                   if        v7duevb<>*blanks
208000160505     c                   eval      v7duevd='Ultimo evento'
208100160505     c                   endif
208200911022     C*
208300911022     C* SE ESISTE AGGANCIO 2° BOLLA
208400991026     C                   Z-ADD     ARBAAS        KAAS
208500991026     C                   Z-ADD     ARBLNP        KLNP
208600991026     C                   Z-ADD     ARBNRS        KNRS
208700991026     C                   Z-ADD     ARBNSP        KNSP
208800991026     C                   MOVEL     '2'           KTRC
208900991027     C     KAR61         CHAIN     FIAR6000                           31
209000911023     C*
209100040216if  1C                   IF          %FOUND
209200991027     C                   eval      *in22 = *on
209300991027     C                   MOVEL     AR6CEI        V9CCEI
209400991027     C                   MOVEL     AR6NFT        V9CNFT
209500991027     C                   MOVEL     AR6FIV        V9CFIV
209600991027      *
209700991027     C                   eval      V9CDIV = AR6div
209800991027     C                   eval      WDIV = AR6div
209900991027     C                   exsr      CTR_VALUTA
210000991027      *
210100001128     C                   clear                   v9dfim
210200001128     C                   clear                   v9dfi2
210300991027     c                   if        AR6fim = 'P'
210400991027     c                   eval      V9Dfim = 'PARTENZA'
210500040126     c                   eval      V9Dfi2 = 'Immesse in'
210600991027     c                   endif
210700991027      * Varia G
210800040130     c                   clear                   W0133
210900040130     c                   clear                   v9csv1
211000040130     c                   clear                   v9cva1
211100040130     c                   clear                   v9csv2
211200040130     c                   clear                   v9cva2
211300040130     c                   clear                   v9csv3
211400040130     c                   clear                   v9cva3
211500040130     c                   clear                   v9csv4
211600040130     c                   clear                   v9cva4
211700041130     c                   clear                   v9csv5
211800041130     c                   clear                   v9cva5
211900041130     c                   clear                   v9csv6
212000041130     c                   clear                   v9cva6
212100040130     c                   clear                   v9civa
212200040130
212300040122      * Carico le varie
212400040122     c                   Movel     Ar6Sv1        wsvn
212500040122     c                   Z-add     Ar6Va1        wvan
212600040122     c                   ExSr      Car_Varie1
212700040122
212800040122     c                   Movel     Ar6Sv2        wsvn
212900040122     c                   Z-add     Ar6Va2        wvan
213000040122     c                   ExSr      Car_Varie1
213100040122
213200040122     c                   Movel     Ar6Sv3        wsvn
213300040122     c                   Z-add     Ar6Va3        wvan
213400040122     c                   ExSr      Car_Varie1
213500040122
213600040122     c     Kar61         Setll     Fiar7000
213700040216do  2c                   Do        *Hival
213800040122     c     Kar61         Reade     Fiar7000
213900040122     c                   If        %Eof(Fiar701l)
214000040122     c                   Leave
214100040122     c                   EndIf
214200040122     c                   Movel     Ar7Svn        wsvn
214300040122     c                   Z-add     Ar7Van        wvan
214400040216     c                   ExSr      Car_Varie1
214500040216e   2c                   EndDo
214600040122
214700991027      * TOTALE FATTURA
214800991027     c                   clear                   W0133
214900991027     c                   IF          AR6ift > *zeros
215000991027     c                   z-add     AR6ift        W0133
215100991027     c                   eval      Wtstdec = §cvfdc
215200991027     c                   exsr      EDTimporti
215300991027     c                   movel     IMPeditato    V9cift
215400001128     C                   else
215500001128     C                   clear                   v9cift
215600991027     c                   endif
215700991027      *
215800991027     c                   If         AR6dft > *zeros
215900991027     C     *iso          movel     AR6DFT        DATA_EUR
216000991027     C     *eur          movel     data_eur      W0080
216100991027     C                   eval      V9cdft = %EDITC(W0080:'Y')
216200001128     C                   else
216300001128     C                   clear                   v9cdft
216400991027     c                   Endif
216500991027      *
216600991027     c                   eval      V9Cksc = AR6ksc
216700991027     c                   eval      V9Cctr = AR6ctr
216800991027     c                   move      v9cksc        W0040
216900991027     c                   If        W0040 = 9999
217000991027     c                   eval      V9Dksc = arbrsd
217100991027     c                   Else
217200991027     c                   eval      i69kac = V9Cksc
217300991027     c                   exsr      CTR_ANAGRA
217400991027     c                   eval      V9Dksc = acorag
217500991027     c                   Endif
217600991027      *
217700991027     C                   eval      COD = 'TR'
217800991027     C                   movel     V9CCTR        UNOCTR            1
217900991027     C                   eval      KEY = unoctr
218000991027     C     KTAB1         CHAIN     TABEL00F                           31
218100991027     C                   movel     TBLUNI        V9DCTR
218200991027      *
218300930114     C* ESENZIONE IVA
218400001128     C                   clear                   v9dcei
218500991111     C     AR6CEI        IFNE      *blanks
218600940721     C                   MOVEL     'EI'          COD
218700991111     C                   MOVEL(P)  AR6CEI        KEY
218800940719     C     KTAB1         CHAIN     TABEL00F                           31
218900991027     C  N31              MOVEL     TBLUNI        V9DCEI
219000940719     C                   ENDIF
219100991027      *
219200040216e   1C                   ENDIF
219300150611
219400150611     c* RICERCA E DECODIFICA STATO SPEDIZIONE
219600150611     c                   clear                   v7dst1
219700150611     c                   clear                   v7dst2
219800150616     c                   clear                   v7dst3
219900150616     c                   setoff                                       58
220000160719    1c***                if        not *in69 and w_ccaleg=*blanks
220001160719    1c                   if        w_ccaleg=*blanks
220100150611     c                   clear                   fnlru3ds
220200150611     c                   eval      ILRU3AAS=arbaas
220300150611     c                   eval      ILRU3LNP=arblnp
220400150611     c                   eval      ILRU3NRS=arbnrs
220500150611     c                   eval      ILRU3NSP=arbnsp
220600150611     c                   eval      ILRU3LNA=arblna
220700150611     c                   eval      ILRU3NTC=arbntc
220800150611     c                   eval      ILRU3TCR=arbtcr
220900150611     c                   eval      ILRU3DCR=arbdcr
221000150611     c                   eval      ILRU3HCR=arbhcr
221100160506     c                   if        *in16
221200160506     c                   eval      ILRU3dip=arbdt3
221300160506     c                   endif
221400160614     c                   eval      ILRU3ucev=w_evbcev
221401161107     c                   eval      ILRU3ITCEV=w_tcev
221402160719     c                   eval      ILRU3ica =arbica
221403160719     c                   eval      ILRU3icc =arbicc
221404160825     c                   if        w_tbo='P' and arbdcm>0
221405160825     c                   eval      ILRU3ica ='S'
221406160825     c                   eval      ILRU3icc ='S'
221407160825     c                   endif
221408161027     c                   eval      ilru3ca=StatoCa
221500150611
221600150611     c                   eval      w_video='N'
221700150611     c                   exsr      sr_orarisr
221800150612     c                   call      'FNLRU3R'
221900150611     c                   parm                    kpjba
222000150611     c                   parm                    fnlru3ds
222100150611     c                   parm                    tnsd99ds
222200150611     c                   parm                    trulor2ds
222300150612     c* valorizzazione PRIMA RIGA del video
222400150611     c                   eval      v7dst1=%trim(%xlate(mi:ma:olru3dec1)) + ' ' +
222500150611     c                                          %trim(olru3not1)
222600150612     c* calcolo da che posizione inzizia la parte "note" all'interno della stringa
222700150611     c* che metto a video per poter incolonnare la parte "note" della seconda riga
222800150611     c                   clear                   pos
222900150611     c                   eval      pos=%checkr(' ':olru3dec1)
223000150612     c                   eval      pos+=2
223100150612     c* Verifico se la parte note della prima riga è stata troncata
223200150612
223300150612     c* Lunghezza utile della stringa 1 per il campo not1
223400150612     c                   eval      lennt1v=%len(v7dst1)-pos
223500150612     c* Lunghezza effettiva del campo not1 ricevuto
223600150611     c                   eval      lennt1 =%checkr(' ':olru3not1)
223700150612    2c                   if        lennt1>lennt1v
223800150612     c                   clear                   PosA
223900150612     c                   eval      PosA=%checkr(charok:v7dst1)
224000150612     c                   if        PosA<%len(v7dst1)
224100150612     c* clearo parola troncata per andare a capo nella seconda riga
224200150612     c                   eval      %subst(v7dst1:posA+1)=*blanks
224300150612     c                   endif
224400150612    2c                   endif
224500150611
224600150612     c* valorizzazione SECONDA RIGA del video
224700150612     c                   eval      v7dst2=%trim(olru3dec2)+ ' '
224800150612     c* imposto eventuale a capo della prima riga
224900150612    2c                   if        lennt1>lennt1v
225000150612     c                   eval      %subst(v7dst2:pos)=
225100150612     c                             %trim(%subst(olru3not1:PosA+2-pos))
225200150612    2c                   endif
225300150612    2c                   if        olru3not2<>*blanks
225400150612     c* la seconda riga contiene dati della prima riga --> scrivo le note 2 nella
225500150612     c* terza riga
225600150612    3c                   if        lennt1>lennt1v
225700150612     c*****              eval      v7dst2=%trim(v7dst2)+' ' + %trim(olru3not2)
225800150612     c                   eval      %subst(v7dst3:pos)=
225900150612     c                             %trim(olru3not2)
226000150616     c                   seton                                        58
226100150612
226200150612   x3c                   else
226300150612     c* altrimenti valorizzo la seconda riga con le note2 partendo da pos
226400150612     c* per incolonnarmi con la prima riga
226401161028     c* se la prima riga è vuota le note2 le accodo (caso di sped. consengata con CA)
226402161028     c                   if        v7dst1=*blanks
226403161028     c                   eval      v7dst2=%trim(v7dst2)+' '+%trim(olru3not2)
226405161028     c                   else
226500150612     c                   eval      %subst(v7dst2:pos)=
226600150612     c                             %trim(olru3not2)
226601161028     c                   endif
226700150612     c* Verifico se la parte note della seconda riga è stata troncata
226800150612
226900150612     c* Lunghezza utile della stringa 1 per il campo not1
227000150612     c                   eval      lennt1v=%len(v7dst2)-pos
227100150612     c* Lunghezza effettiva del campo not1 ricevuto
227200150612     c                   eval      lennt1 =%checkr(' ':olru3not2)
227300150612    2c                   if        lennt1>lennt1v
227400150612     c                   clear                   PosA
227500150612     c                   eval      PosA=%checkr(charok:v7dst2)
227600150612     c                   if        PosA<%len(v7dst2)
227700150612     c* clearo parola troncata per andare a capo nella terza   riga
227800150612     c                   eval      %subst(v7dst2:posA+1)=*blanks
227900150612     c                   endif
228000150612    2c                   endif
228100150612     c* valorizzazione TERZA RIGA del video quando vado a capo con la seconda
228200150612     c* imposto eventuale a capo della prima riga
228300150612    2c                   if        lennt1>lennt1v
228400150612     c                   eval      %subst(v7dst3:pos)=
228500150612     c                             %trim(%subst(olru3not2:PosA+2-pos))
228600150616     c                   seton                                        58
228700150612    2c                   endif
228800150612    3c                   endif
228900150612    2c                   endif
228901161027     c                   if        (olru3spc='CON' or olru3spc='CAN')
228902161027     c                             and arbdcm=0
228903160609     c                   eval      *in69=*on
228904160609     c                   eval      v08dcm=w7cdev
228905161017     c                   eval      v5ddcm ='REALE  '
228906161017     c                   eval      v6ddcm ='CONSEGNA'
228907160609     c                   movel     w7cdev        dataeur
228908160609     c                   movel     dataeur       datadmy
228909160609     c                   movel     datadmy       v06dcm
228910160609     c                   eval      vidhmc=%editw(evbhev: '0 :  ')
228911160609     c*****              eval      v7dcca='da PDA'
228912161014     c                   eval      v7dcons='CONSEGNATA:'
228913160609     c                   endif
229000150612    1c                   endif
229100150616     c
229200150616     c                   if        v7dst3=*blanks and
229300150616     c                             ((v7dst1<>*blanks or v7dst2<>*blanks) or
229400160523     c                               *in69)
229500150616     c                   eval      v7dst3=*all'-'
229600150616     c                   endif
229700150616
229800150616     c                   setoff                                       54
229900150616     c                   if        arbffd='S'
230000150616     c                   eval      %subst(v7dst3:68)=' Filiale ?'
230100150616     c                   seton                                        54
230101161031     c                   if        v7dst2=*blanks and *in69
230102161031     c                   eval      v7dst2='IN FERMO DEPOSITO'
230103161031     c                   endif
230200150616     c                   endif
230300930525     C*
230400991027     C* PER I DATI SULLA TASSAZIONE 1a BOLLA, DEVO CERCARE SU FIAR600F
230500940905     C                   SETOFF                                       24
230600110705     c                   clear                   atrbl4a
230700110705     c
230800110705     c                   clear                   v6car4ksc
230900110705     c                   clear                   v6dar4ksc
231000110705     c                   clear                   v6car4ctr
231100111021     c                   clear                   VIDimv
231200111021     c                   clear                   VIDpor
231300111021     c                   clear                   VIDift
231400111021     c                   clear                   VIDdiv
231500111021     c                   clear                   VIDfim
231600111021     C                   clear                   VIDCEI
231700111021     C                   clear                   VIDNFT
231800111021     C                   clear                   VIDFIV
231900111021     C                   clear                   DESCEI
232000111021     C                   clear                   viddft
232100111021     C                   clear                   vidIVA
232200111021      *
232300111021      * CERCO LE VARIE: LE PRIME 3 SU FIAR600F, EVENTUALI  ALTRE SU FIAR700F
232400111021     C                   eval      *in35 = *on
232500131107     C                   WRITE     LRI6C06
232600111021     C                   eval      *in35 = *off
232700111021      *
232800111021     c                   clear                   NR6
232900111021     c                   clear                   VIDiva
233000110705     c
233100940712     C                   Z-ADD     ARBAAS        KAAS
233200940712     C                   Z-ADD     ARBLNP        KLNP
233300940712     C                   Z-ADD     ARBNRS        KNRS
233400940712     C                   Z-ADD     ARBNSP        KNSP
233500110705     C                   MOVEL     'A'           KTRC
233600110705     C     KAR41         CHAIN     FiAR401L
233700110705if  1C                   IF        not %FOUND
233800110705     c                   eval      atrbl4a=nodi
233900110705     c                   else
234000110705     c                   movel     ar4not        dsbl4a
234100110705     c                   if        §b4aks>0
234200110705     C                   eval      *in24 = *on
234300110705     c                   eval      v6car4ksc=§b4aks
234400110705     c                   eval      i69kac = v6car4ksc
234500110705     c                   exsr      CTR_ANAGRA
234600110705     c                   eval      v6dar4ksc = acorag
234700110705     c                   if        §b4act<>*blanks
234800110705     c                   movel     §b4act        v6car4ctr
234900110705     c                   endif
235000110705     c* non c'e' cod cliente x tassaz.
235100110705     c                   else
235200110705     c                   eval      atrbl4a=nodi
235300110705     c                   endif
235400110705     c                   endif
235500110705     c
235600991026     C                   MOVEL     '1'           KTRC
235700991027     C     KAR61         CHAIN     FIAR6000
235800110705     c
235900110705     c                   if        %found(fiar601l)
236000110705     c
236100110705      * CLIENTE E TARIFFA FATTURAZIONE
236200110705     c                   IF        *IN33 = *ON
236300110705     c                   eval      v6cksc = arbksc
236400110705     c                   eval      v6cctr = arbctr
236500110705     c                   eval      v6dksc = arbrsm
236600110705     c                   ELSE
236700110705     c                   eval      v6cksc = AR6ksc
236800110705     c                   eval      v6cctr = AR6ctr
236801160825     c                   if        w_tbo='P'
236802160825     c                   eval      vidksc = AR6ksc
236803160825     c                   eval      vidctr = AR6ctr
236804160825     c                   endif
236900110705     c                   move      v6cksc        W0040
237000110705     c                   if        W0040 = 9999
237100110705     c                   eval      v6dksc = arbrsd
237200110705     c                   else
237300110705     c                   eval      i69kac = v6cksc
237400110705     c                   exsr      CTR_ANAGRA
237500110705     c                   eval      v6dksc = acorag
237600110705     c                   endif
237700110705     c                   ENDIF
237800110705      *   decodifica tariffa
237900110705     C                   eval      COD = 'TR'
238000110705     C                   MOVEL     V6CCTR        UNOCTR            1
238100110705     C                   MOVEL(P)  UNOCTR        KEY
238200110705     C     KTAB1         CHAIN     TABEL00F                           31
238300110705     C                   MOVEL     TBLUNI        V6DCTR
238400110705     c
238500110705     c                   endif
238600991027      *
238700040216if  1C                   IF        %FOUND  and
238800991027     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
238900991027     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
239000991027     C                              AR6sv3 <> *blanks)
239100991027      *
239200991027      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SE PRESENTE ALMENO UNO DEI CAMPI
239300991027     C                   EVAL      *IN24 = *ON
239400991027      *
239500991027     C                   eval      VIDDIV = AR6div
239600991027     C                   eval      WDIV = AR6div
239700991027     C                   exsr      CTR_VALUTA
239800991027      *
239900991027     c                   IF          ar6por > *zeros
240000991027     c                   z-add     AR6por        W0133
240100991027     c                   eval      Wtstdec = §cvfdc
240200991027     c                   exsr      EDTimporti
240300991027     c                   movel     IMPeditato    VIDpor
240400991128     c                   else
240500991128     c                   clear                   VIDpor
240600991027     c                   ENDIF
240700991027      *
240800991027     c                   IF          ar6imv > *zeros
240900991027     c                   z-add     AR6imv        W0133
241000991027     c                   eval      Wtstdec = §cvfdc
241100991027     c                   exsr      EDTimporti
241200991027     c                   movel     IMPeditato    VIDimv
241300991128     c                   else
241400991128     c                   clear                   VIDimv
241500991027     c                   ENDIF
241600991027      *
241700991027     c                   IF          ar6ift > *zeros
241800991027     c                   z-add     AR6ift        W0133
241900991027     c                   eval      Wtstdec = §cvfdc
242000991027     c                   exsr      EDTimporti
242100991027     c                   movel     IMPeditato    VIDift
242200991128     c                   else
242300991128     c                   clear                   VIDift
242400991027     c                   ENDIF
242500991027      *
242600991027     c                   IF        AR6fim = 'P'
242700991027     c                   eval      VIDfim = 'PARTENZA'
242800991128     c                   else
242900991128     c                   clear                   VIDfim
243000991027    2C                   ENDIF
243100991027      *
243200991027     C                   MOVEL     AR6CEI        VIDCEI
243300991027     C                   MOVEL     AR6NFT        VIDNFT
243400991027     C                   MOVEL     AR6FIV        VIDFIV
243500991027      *
243600991027     C                   IF        AR6CEI <>  *BLANKS
243700940808     C                   MOVEL     'EI'          COD
243800991027     C                   MOVEL(P)  AR6CEI        KEY
243900940808     C     KTAB1         CHAIN     TABEL00F                           31
244000940808     C  N31              MOVEL     TBLUNI        DESCEI
244100991128     c                   else
244200991128     c                   clear                   DESCEI
244300940808     C                   ENDIF
244400991027      *
244500991027      * DATA FATTURA DA GIRARE
244600991027     C                   IF         ar6dft > *zeros
244700991027     C     *iso          movel     AR6DFT        DATA_EUR
244800991027     C     *eur          movel     data_eur      W0080
244900991027     C                   eval      VIDdft = %EDITC(W0080:'Y')
245000991128     c                   else
245100991128     c                   clear                   VIDdft
245200991027     C                   ENDIF
245300991027      *
245400991027      *
245500991027     C                   MOVEL     ar6SV1        WSVN
245600991027     C                   MOVEL     ar6VA1        WVAN
245700991027     c                   exsr      car_varie
245800991027      *
245900991027     C                   MOVEL     ar6SV2        WSVN
246000991027     C                   MOVEL     ar6VA2        WVAN
246100991027     c                   exsr      car_varie
246200991027      *
246300991027     C                   MOVEL     ar6SV3        WSVN
246400991027     C                   MOVEL     ar6VA3        WVAN
246500991027     c                   exsr      car_varie
246600991027      *
246700991027      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
246800040216if  2C                   if          ar6SV3 <> *blanks
246900991027     C     KAR61         SETLL     FIAR7000
247000991027     C     KAR61         READE     FIAR7000
247100040216do  3C                   DOW       %EOF = *OFF
247200991027     C                   MOVEL     AR7SVN        WSVN
247300991027     C                   MOVEL     AR7VAN        WVAN
247400991027     c                   exsr      car_varie
247500991027     C     KAR61         READE     FIAR7000
247600040216e   3C                   ENDDO
247700040216e   2C                   endif
247800110705e   1C                   ENDIF
247900991027      *
248000991027      * Se non ho caricato record ne carico uno vuoto
248100110705     c                   IF        NR6 = *ZEROS  and *in24
248200991027     C                   clear                   V6CDE1
248300991027     C                   clear                   V6CVA1
248400991027     C                   clear                   V6CSV1
248500991027     C                   add       1             NR6
248600131107     C                   WRITE     LRI6S06
248700991027     c                   ENDIF
248800991027      *
248900991027      *
249000941206     C* 70 ON - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
249100941206     C     *IN33         IFEQ      '0'
249200941206     C     AR6NFT        ANDGT     0
249300941206     C     *IN22         OREQ      '1'
249400991111     C     AR6NFT        ANDGT     0
249500941206     C                   SETON                                        70
249600941206     C                   END
249700940712     C*
249800940719     C* PRELEVO LE NOTE DAL FILE APPOSITO
249900940719     C                   MOVEL     ARBAAS        KAAS
250000940719     C                   MOVEL     ARBLNP        KLNP
250100940719     C                   MOVEL     ARBNRS        KNRS
250200940719     C                   MOVEL     ARBNSP        KNSP
250300940719     C                   MOVEL     '8'           KTRC
250400060216     C     KAR41         CHAIN     FiAR401L                           31
250500940719     C  N31              MOVEL     AR4NOT        V4CNO1
250600940802     C   31              MOVEL     *BLANKS       V4CNO1
250700940719     C                   MOVEL     '9'           KTRC
250800060216     C     KAR41         CHAIN     FiAR401L                           31
250900940719     C  N31              MOVEL     AR4NOT        V4CNO2
251000940802     C   31              MOVEL     *BLANKS       V4CNO2
251100940719     C*
251200941110     C* ESTENSIONE RAGIONE SOCIALE DESTINATARIO
251300941110     C                   MOVEL     'D'           KTRC
251400060216     C     KAR41         CHAIN     FiAR401L                           31
251500950125     C  N31              MOVEL     AR4NOT        V4CNOT
251600950125     C   31              MOVEL     *BLANKS       V4CNOT
251700971015     C*
251800971015     C* SOLO PER EXPORT DATA DI STAMPA SU BORDERO' ARRIVI
251900070404     C***  *IN16         IFEQ      *ON
252000070404     C***                MOVEL     'A'           KTRC
252100070404     C***  KAR41         CHAIN     FiAR401L                           31
252200070404     C**N31              MOVE      AR4NOT        WDATA             8
252300070404     C***31              MOVEL     *ZEROS        WDATA
252400070404     C***  WDATA         IFGT      *ZEROS
252500070404     C***                MOVEL     WDATA         G02INV
252600070404     C***                Z-ADD     *ZEROS        G02DAT
252700070404     C***                MOVEL     '3'           G02ERR
252800070404     C***                CALL      'XSRDA8'
252900070404     C***                PARM                    WLBDAT
253000070404     C***                Z-ADD     G02DAT        V3CDST
253100070404     C***                ENDIF
253200070404     C***                ENDIF
253300940720     C*
253400051114     C* IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU FiAR900F
253500940720     C* CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
253600940720     C                   MOVEL     *BLANKS       VIDTIC
253700940720     C                   MOVEL     *ZERO         VIDCAS
253800940720     C                   MOVEL     *BLANKS       VIDVCA
253900940720     C                   MOVEL     *BLANKS       V4CGCA
254000940923     C                   MOVEL     *BLANKS       V4IGCA
254100091214     c                   setoff                                       65
254200940720     C     §3AFCA        IFGT      *ZERO
254300051128     C     Ksped         CHAIN     FiAR901L                           31
254400940720     C  N31              DO
254500940720     C                   MOVEL     AR9TIC        VIDTIC
254600940720     C                   MOVEL     AR9CAS        VIDCAS
254700940720     C                   MOVEL     AR9VCA        VIDVCA
254800940720     C                   MOVEL     AR9GCA        V4CGCA
254900091214     c                   seton                                        65
255000940720     C                   ENDDO
255100940720     C                   ENDIF
255200941215     C* ESISTE CONTRASSEGNO: VEDO SE ABILITATO
255300941215     C     VIDCAS        IFNE      0
255400941215     C                   SETON                                        29
255500941215     C                   END
255600940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
255700940923     C     V4CGCA        COMP      *BLANKS                            4141
255800940720     C*
255900940720     C* RIEMPO ANCHE ALCUNI CAMPI PER LE DS DI PASSAGGIO PARAMETRI
256000940720     C                   MOVEL     V3CDSP        PA1DSP
256100940720     C                   MOVEL     ARBRMN        PA1RMN
256200940720     C                   MOVEL     ARBLNA        PA1LNA
256300940802     C                   MOVEL     V3DLNA        PA1LN1
256400940720     C                   MOVEL     ARBRSD        PA1RSD
256500160504     C                   MOVEL     V2CAAS        pa1AAS
256600160504     C                   MOVEL     COMLNP        pa1LNP
256700160504     C                   MOVEL     V2CNRS        pa1NRS
256800160504     C                   MOVEL     V2CNSP        pa1NSP
256900160505     C                   MOVEL     'FNLRU6R1'    pa1pgm
257000160505     C                   MOVEL     RSUT          PA1DSA
257100160505     C                   MOVEL     pargia        PA1gia
257200160505     C                   MOVEL     SIMFEL        PA1FEL
257300940906     C*-------------------------------------------------------------------------
257400991029      * GESTIONE INCASSO PARZIALE se assegnato o presente estensione bolla
257500991102     C                   clear                   v5cdiv
257600991102     c                   clear                   Wariimp
257700991102     c                   clear                   Wariipp
257800991102     c                   clear                   VIDimp
257900991102     c                   clear                   VIDipp
258000991102     C                   MOVEL     ARBFIP        VIDFIP
258100991102      *
258200991029     c  N33              eval      V5Cdiv = viddiv
258300991029     c   22              eval      V5Cdiv = v9cdiv
258400991029      *
258500040216if  1c                   if        V5CDIV <> *blanks
258600991029      *
258700991029     C                   eval      WDIV = V5Cdiv
258800991029     C                   exsr      CTR_VALUTA
258900991029      *
259000991029     C                   MOVEL     ARBAAS        KAAS
259100991029     C                   MOVEL     ARBLNP        KLNP
259200991029     C                   MOVEL     ARBNRS        KNRS
259300991029     C                   MOVEL     ARBNSP        KNSP
259400991029     C                   MOVEL     'A'           ktrc
259500991029      *
259600991029     c     kar61         setll     fiari000
259700991029     c     kar61         reade     fiari000
259800991029      *
259900040216do  2c                   DOW       not %EOF
260000040216if  3c                   if        v5cdiv = aridiv
260100991029     c                   add       ARIimp        Wariimp
260200991029     c                   add       ARIipp        Wariipp
260300040216x   3c                   else
260400991029      *
260500991029     C                   clear                   yeurcods
260600991029     C                   eval      YECDVI = aridiv
260700991029     C                   eval      YECDVO = V5Cdiv
260800991029     C                   eval      YECIMI = ARIimp
260900011022     C                   move      §CVDEC        YECDCO
261000991029     C                   CALL      'YEURCO'
261100991029     C                   PARM                    YEURcods
261200991029    6C                   if        YECESI = *blanks
261300991029     C                   add       YECIMO        WARIimp
261400991029    6C                   endif
261500991029      *
261600991029     C                   clear                   yeurcods
261700991029     C                   eval      YECDVI = aridiv
261800991029     C                   eval      YECDVO = V5Cdiv
261900991029     C                   eval      YECIMI = ARIipp
262000011022     C                   move      §CVDEC        YECDCO
262100991029     C                   CALL      'YEURCO'
262200991029     C                   PARM                    YEURcods
262300991029    6C                   if        YECESI = *blanks
262400991029     C                   add       YECIMO        WARIipp
262500991029    6C                   endif
262600991029      *
262700040216e   3c                   endif
262800991029      *
262900991029     c     kar61         reade     fiari000
263000040216e   2c                   enddo
263100991029      *
263200991029     c                   IF        WARIipp > *zeros
263300991029     c                   z-add     Wariipp       W0133
263400991029     c                   eval      Wtstdec = §cvfdc
263500991029     c                   exsr      EDTimporti
263600991029     c                   movel     IMPeditato    VIDipp
263700991029     c                   ENDIF
263800991029      *
263900991029     c                   IF        WARIimp > *zeros
264000991029     c                   z-add     Wariimp       W0133
264100991029     c                   eval      Wtstdec = §cvfdc
264200991029     c                   exsr      EDTimporti
264300991029     c                   movel     IMPeditato    VIDimp
264400991029     c                   ENDIF
264500991105      *
264600991105     c                   IF        VIDipp = *blanks  and  VIDimp = *blanks
264700991105     c                   clear                   V5CDIV
264800991105     c                   ENDIF
264900991029      *
265000040216e   1C                   endif
265100991029     C*-------------------------------------------------------------------------
265200951220     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
265300951220     C*
265400951220     C* PULISCO IL SFL
265500960926     C                   SETOFF                                       72
265600131107     C                   WRITE     LRI6C05
265700960926     C                   SETON                                        7172
265800951220     C*
265900060216     C* CHIAVE PER ACCEDERE AL FILE FiAR401L
266000951220     C                   MOVEL     ARBAAS        KAAS
266100951220     C                   MOVEL     ARBLNP        KLNP
266200951220     C                   MOVEL     ARBNRS        KNRS
266300951220     C                   MOVEL     ARBNSP        KNSP
266400951220     C*
266500951220     C                   Z-ADD     0             NRR5              4 0
266600951220     C                   MOVEL     '7K'          COD
266700951220     C*
266800060216     C     KAR42         SETLL     FiAR401L
266900060216     C     KAR42         READE     FiAR401L                               36
267000040216do  1C     *IN36         DOWEQ     *OFF
267100951220     C*
267200951220     C                   MOVEL(P)  AR4TRC        KEY
267300951220     C     KTAB1         CHAIN     TABEL                              36
267400951220     C*
267500951220     C     *IN36         IFEQ      *OFF
267600951220     C     TBLFLG        ANDEQ     ' '
267700951220     C                   MOVEL     TBLUNI        DS7K
267800951220     C     §7KVIS        IFEQ      'S'
267900951220     C                   MOVEL     §7KDES        VS5DES
268000951220     C                   MOVEL     AR4NOT        VS5NOT
268100951220     C                   ADD       1             NRR5
268200131107     C                   WRITE     LRI6S05
268300951220     C                   ENDIF
268400951220     C                   ENDIF
268500951220     C*
268600060216     C     KAR42         READE     FiAR401L                               36
268700040216e   1C                   ENDDO
268800951220     C*
268900960926     C* SE NON HO CARICATO NIENTE NON EMETTO IL SFLDSP PER EVITARE CHE
269000960926     C*  IL PGM DIA ERRORE DI SESSIONE O UNITA'
269100951220     C     NRR5          IFEQ      0
269200991105     C                   clear                   VS5DES
269300991105     C                   clear                   VS5NOT
269400991105     C                   ADD       1             NRR5
269500131107     C                   WRITE     LRI6S05
269600951220     C                   ENDIF
269700951220     C*
269800021216      * Riempio il campone in fondo alla videata con i tasti scelti
269900021216     C                   exsr      Ges_TastiFun
270000021217     c                   eval      FD07 = FDxx
270100021217     c                   eval      F24D07 = F24Dxx
270200911022     C                   ENDSR
270300131114      *-------------------------------------------------------------------------
270400131114      *  Decodifica EVENTO
270500131114      *-------------------------------------------------------------------------
270600131114     c     Decodi_evb    BEGSR
270700131114
270800131114     c                   if        evbcev='  C'
270900131114     c                   eval      §2ades='CONSEGNA'
271000131128     c* aggiungo anche firmatario
271100150616     c                   if        w_ar4_1=*blanks
271200131128     c                   if        §PCTNOMFIR<>*blaNKS
271300131128     C                   EVAL      §2ADES=%TRIM(§2ADES)+' Firma:' +§PCTNOMFIR
271400131128     c                   endif
271500150616     c                   else
271600150616     C                   EVAL      §2ADES=%TRIM(§2ADES)+' Firma:' +ar4not
271700150616     c                   endif
271800131128     c
271900131114     C                   else
271901161108     c                   eval      wcev=evbcev
271902161108     c                   exsr      sr_2a
272000161108     C***                MOVEL     '2A'          COD
272100161108     C***                MOVEL(p)  evbcev        KEY
272200161108     C***  KTAB1         CHAIN     TABEL00F
272300161108    3c***                if        not %found(tabel00f)
272400161108     c***                clear                   ds2a
272500161108     c***                else
272600161108     c***                eval      ds2a=tbluni
272700161108    3c***                endif
272800131114    3c                   endif
272900131114     c
272901161108     c                   if        evbcev='  C'
272902161108     c                   eval      v7duevb=§2ades
272903161108     c                   else
273000131114     c                   eval      v7duevb=evbcev+'-'+§2ades
273001161108     c                   endif
273100131114
273200161017    3c                   if        evbdev>0
273300131114     c     *iso          movel     evbdev        dataeur
273400131114     c                   movel     dataeur       datadmy
273500131114     c                   movel     dataeur       w7cdev            8 0
273600131114
273601161017     c* metto data/ora ultimo evento solo se non si tratta di consegna perchè
273602161017     c* in questo caso tali dati sono già visualizzati nella riga "Consegnata"
273603161017     c                   if        evbcev<>'  C'
273700131114     c                   eval      indx=%checkr(' ':v7duevb)
273800131114    4c                   if        indx>46
273900131114     c                   eval      indx=46
274000131114     c                   else
274100131114     c                   eval      indx=indx+1
274200131114    4c                   endif
274300131114     c                   eval      wcarat=66-indx
274400131114     c                   eval      %subst(v7duevb:indx:wcarat)=' ' +'il ' +
274500131114     c                             %editw(W7cdev: '  /  /    ')
274600131114     c
274700131114    4c                   if        evbhev>0
274800131114     c                   eval      v7duevb=%trim(v7duevb)+' ' +
274900131114     c                                                 %editw(evbhev: '0 :  ')
275000131114    4c                   endif
275001161017     c                   endif
275100131114     c
275200131114    3c                   endif
275201160614     c                   eval      w_evbcev=evbcev
275300131114     c                   ENDSR
275301161108      *-------------------------------------------------------------------------
275302161108      *  Decoditifa causale evento
275303161108      *-------------------------------------------------------------------------
275304161108     c     Sr_2a         BegSr
275305161108     C                   MOVEL     '2A'          COD
275306161108     C                   MOVEL(p)  wcev          KEY
275307161108     C     KTAB1         CHAIN     TABEL00F
275308161108    3c                   if        not %found(tabel00f)
275309161108     c                   clear                   ds2a
275310161108     c                   else
275311161108     c                   eval      ds2a=tbluni
275312161108    3c                   endif
275313161108     c                   ENDSR
275400030703      *-------------------------------------------------------------------------
275500030703      *  Decoditifa tipo bancali
275600030703      *-------------------------------------------------------------------------
275700030703     c     Sr_DecTba     BegSr
275800030703     c                   Clear                   Tibs02Ds
275900030703     c                   Clear                   dTba
276000030703     c                   Movel     'C'           T02Mod
276100030703     c                   Movel     Knsif         T02Sif
276200030703     c                   Movel     'TBA'         T02Cod
276300030703     c                   Movel(p)  wtba          T02Ke1
276400030703     c                   Call      'TIBS02R'
276500030703     c                   Parm                    Kpjba
276600030703     c                   Parm                    Tibs02Ds
276700030703If  3c                   If        T02Err = *Blanks
276800030703     c                   Movel     T02Uni        dTba
276900030703    3c                   EndIf
277000030703     c                   Eval      wdestba = §TbaDesc
277100030703     c                   EndSr
277200021216      *-------------------------------------------------------------------------
277300021216      *  Imposto descrizione tasti funzione
277400021216      *-------------------------------------------------------------------------
277500021216     c     Ges_TastiFun  BEGSR
277600021217      *
277700021217      * per emettere i tasti senza condizionamenti ..
277800021216     c                   setoff                                       89
277900021216      *
278000021216      * Conta i caratteri riempiti dalle RigaTf1
278100021217     c                   z-add     0             $z                3 0
278200021216      *
278300021216      * Conta i caratteri riempiti dalle RigaTf2
278400021217     c                   z-add     0             $k                3 0
278500021216      *
278600021216      * Conta i caratteri riempiti dalle RigaTf3
278700021217     c                   z-add     0             $j                3 0
278800021216      *
278900021216      * Conta la posizione del campone da cui partire per impostare
279000021216      * la descrizione del tasto funzione
279100021216     c                   z-add     0             $y                2 0
279200021216      *
279300021217      * Stringhe che contengono le descrizioni dei tasti funzione
279400021216     c                   reset                   RigaTf1
279500021216     c                   reset                   RigaTf2
279600021216     c                   reset                   RigaTf3
279700070411      *
279800070411      *?F01?--> scelta bolla da assistenza clienti
279900070411     c                   IF        *in63 = *on
280000070411     c                   reset                   $Tf
280100070411     c                   movea     Tf01          $Tf
280200070411     c                   exsr      Rie_$Tf
280300070411     c                   else
280400070411      *?F01?--> scelta bolla generica
280500070411     c                   if        *in61 = *on
280600070411     c                   reset                   $Tf
280700070411     c                   movea     Tf01B         $Tf
280800070411     c                   exsr      Rie_$Tf
280900070411     c                   ENDIF
281000070411     c                   ENDIF
281100021216      *
281200070411      * F13 --> tassazione 1 bolla
281300021216     c                   IF        *in24 = *on
281400021217     c                   reset                   $Tf
281500021217     c                   movea     Tf13          $Tf
281600021217     c                   Exsr      Rie_$Tf
281700021217     C                   ENDIF
281800021216      *
281900070411      * F04 --> tassazione 2 bolla
282000021216     c                   IF        *in22 = *on
282100021217     c                   reset                   $Tf
282200021217     c                   movea     Tf04          $Tf
282300021217     c                   Exsr      Rie_$Tf
282400021217     C                   ENDIF
282500040220      *
282600070411      *?F16 --> assistenza clienti
282700040220     c                   IF        *in64 = *on
282800040220     c                   reset                   $Tf
282900040220     c                   movea     Tf16          $Tf
283000040220     c                   exsr      Rie_$Tf
283100040220     c                   ENDIF
283101160628      * F08 --> ORM
283102160628     c                   IF        *in18 = *on
283103160628     c                   reset                   $Tf
283104160628     c                   movea     Tf08          $Tf
283105160628     c                   Exsr      Rie_$Tf
283106160628     C                   ENDIF
283200021216      *
283300070411      * F05 --> eventi
283400021216     c                   IF        *in95 = *on
283500021217     c                   reset                   $Tf
283600021217     c                   movea     Tf05          $Tf
283700021217     c                   Exsr      Rie_$Tf
283800021217     C                   ENDIF
283900021216      *
284000021216      * F10
284100021217     c                   IF        *in98 = *on
284200021217     c                   reset                   $Tf
284300021217     c                   movea     Tf10          $Tf
284400021217     c                   Exsr      Rie_$Tf
284500021217     c                   ENDIF
284600021216      *
284700021216      * F14
284800021217     c                   IF        *in03 = *on
284900021217     c                   reset                   $Tf
285000021217     c                   movea     Tf14          $Tf
285100021217     c                   Exsr      Rie_$Tf
285200021217     c                   ENDIF
285300021216      *
285400021216      * F02
285500021217     c                   IF        *in04 = *on
285600021217     c                   reset                   $Tf
285700021217     c                   movea     Tf02          $Tf
285800021217     c                   Exsr      Rie_$Tf
285900021217     c                   ENDIF
286000110929      *
286100110930      * F22 --> Perfect Order
286200110929     c                   IF        *in86 = *on
286300110929     c                   reset                   $Tf
286400110929     c                   movea     Tf22          $Tf
286500110929     c                   Exsr      Rie_$Tf
286600110929     C                   ENDIF
286700021216      *
286800021216      * F11
286900021217     c                   IF        *in77 = *on
287000021217     c                   reset                   $Tf
287100021217     c                   movea     Tf11          $Tf
287200021217     c                   Exsr      Rie_$Tf
287300021217     c                   ENDIF
287400070404      *
287500070413      * F17 --> solo se partito com GEO
287600150618     c                   if        *in17 and not *in12
287700070404     c                   reset                   $Tf
287800070404     c                   movea     Tf17          $Tf
287900070404     c                   Exsr      Rie_$Tf
288000070413     c                   endif
288100021216      *
288200021216      * F07
288300021217     c                   IF        *in67 = *on
288400021217     c                   reset                   $Tf
288500021217     c                   movea     Tf07          $Tf
288600021217     c                   Exsr      Rie_$Tf
288700021217     c                   ENDIF
288800021216      *
288900160628      * F23= Discordanze estero
289000021217     c                   IF        *in74 = *on and *in16 = *on
289100021217     c                   reset                   $Tf
289200160628     c                   movea     Tf23          $Tf
289300021217     c                   Exsr      Rie_$Tf
289400021217     c                   ENDIF
289500130830
289600130830      * F09 --> Orari servizi non condizionato
289700131007     c                   IF        *in89 = *off and wabif9='S'
289800130930     c                   reset                   $Tf
289900130930     c                   movea     Tf09          $Tf
290000130930     c                   Exsr      Rie_$Tf
290100130930     c                   ENDIF
290200021216      *
290300021216      * F20 --> non condizionato
290400021217     c                   IF        *in89 = *off
290500021217     c                   reset                   $Tf
290600021217     c                   movea     Tf20          $Tf
290700021217     c                   Exsr      Rie_$Tf
290800021217     c                   ENDIF
290900021216      *
291000021216      * F21 --> non condizionato
291100021216     c                   IF        *in89 = *off
291200021217     c                   reset                   $Tf
291300021217     c                   movea     Tf21          $Tf
291400021217     c                   Exsr      Rie_$Tf
291500021217     c                   ENDIF
291600021218      *
291700021218      * F19 --> non condizionato
291800021218     c                   IF        *in89 = *off
291900021218     c                   reset                   $Tf
292000021218     c                   movea     Tf19          $Tf
292100021218     c                   Exsr      Rie_$Tf
292200021218     c                   ENDIF
292300070420     c
292400130830      * F06 --> solo se partito con GEO
292500070420     c                   if        *in17
292600070420     c                   reset                   $Tf
292700070420     c                   movea     Tf06          $Tf
292800070420     c                   Exsr      Rie_$Tf
292900070420     c                   ENDIF
293000021217      *
293100021217      * F12 --> non condizionato
293200021217     c                   IF        *in89 = *off
293300021217     c                   reset                   $Tf
293400021217     c                   movea     Tf12          $Tf
293500021217     c                   Exsr      Rie_$Tf
293600021217     c                   ENDIF
293700021216      *
293800021217      * Pulisco le stringa
293900021216     c                   z-add     1             $h
294000030121     c     1             do        62            $h                2 0
294100021216     C                   if        RigaTf1($h) = '#'
294200021216     c                   movel     *blanks       RigaTf1($h)
294300021216     c                   endif
294400021216     c                   enddo
294500021216     c                   z-add     1             $h
294600030121     c     1             do        62            $h                2 0
294700021216     C                   if        RigaTf2($h) = '#'
294800021216     c                   movel     *blanks       RigaTf2($h)
294900021216     c                   endif
295000021216     c                   enddo
295100021216     c                   z-add     1             $h
295200030121     c     1             do        62            $h                2 0
295300021216     C                   if        RigaTf3($h) = '#'
295400021216     c                   movel     *blanks       RigaTf3($h)
295500021216     c                   endif
295600021216     c                   enddo
295700021217      *
295800021216      * Imposto la descrizione del tasto funzione F24 e segnalo
295900021216      * quante righe ho riempito e quale devo emettere
296000021216     c                   SELECT
296100021216      *
296200021216     c                   when      $k<>*zeros and $Y<>*zeros
296300021216     c                   eval      $loop = 3
296400021216     c                   z-add     3             $loop             1 0
296500021217     C                   movel(p)  CF2413        F24Dxx           16
296600021216      *
296700021216     c                   when      $k<>*zeros and $Y=*zeros
296800021216     c                   z-add     2             $loop             1 0
296900021217     C                   movel(p)  CF2412        F24Dxx
297000021216      *
297100021216     c                   Other
297200021216     c                   z-add     1             $loop             1 0
297300021217     C                   movel     *blanks       F24Dxx
297400021216     c                   ENDSL
297500021216      * Prima riga di tasti funzione
297600030121     c                   movea     RigaTF1       FDxx             62
297700021216     c                   z-add     1             $Rig              1 0
297800021216      *
297900021216     c                   endsr
298000021217      *-------------------------------------------------------------------------
298100021217      *  Riempio le stringhe con i campi funzione compattati
298200021217      *-------------------------------------------------------------------------
298300021217     c     Rie_$Tf       begsr
298400021217      *
298500021217     c                   z-add     1             $x                2 0
298600021217     c     '#'           lookup    $Tf($x)                                31
298700021217     c                   add       $x            $Z
298800030121     c                   IF        $Z <= 63
298900021217     c                   eval      $j = $Z - $x + 1
299000021217     c                   movea     $Tf           RigaTF1($j)
299100021217     c                   ELSE
299200021217     c                   add       $x            $K
299300030121     c                   IF        $K <= 63
299400021217     c                   eval      $j = $K - $x + 1
299500021217     c                   movea     $Tf           RigaTF2($j)
299600021217     c                   ELSE
299700021217     c                   add       $x            $Y
299800030121     c                   IF        $Y <= 63
299900021217     c                   eval      $j = $Y - $x + 1
300000021217     c                   movea     $Tf           RigaTF3($j)
300100021217     c                   ENDIF
300200021217     c                   ENDIF
300300021217     c                   ENDIF
300400021217      *
300500021217     c                   endsr
300600021217      *-------------------------------------------------------------------------
300700021217      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
300800021217      *-------------------------------------------------------------------------
300900021217     c     Ges_F24       begsr
301000021217      *
301100021217     c                   SELECT
301200021217      *
301300021217     c                   WHEN      $Loop = 2 and $Rig =2  or
301400021217     c                             $Loop = 3 and $Rig =3
301500021217     c                   movea     RigaTF1       FDxx
301600021217     c                   z-add     1             $Rig
301700021217      *
301800021217     c                   if        $loop = 2
301900021217     c                   eval      F24Dxx= Cf2412
302000021217     c                   else
302100021217     c                   eval      F24Dxx= Cf2413
302200021217     c                   endif
302300021217      *
302400021217     c                   WHEN      $Loop = 2 and $Rig =1 or
302500021217     c                             $Loop = 3 and $Rig =1
302600021217     c                   movea     RigaTF2       FDxx
302700021217     c                   z-add     2             $Rig
302800021217      *
302900021217     c                   if        $loop = 2
303000021217     c                   eval      F24Dxx= Cf2422
303100021217     c                   else
303200021217     c                   eval      F24Dxx= Cf2423
303300021217     c                   endif
303400021217      *
303500021217     c                   WHEN      $Loop = 3 and $Rig =2
303600021217     c                   movea     RigaTF3       FDxx
303700021217     c                   z-add     3             $Rig
303800021217     c                   eval      F24Dxx= Cf2433
303900021217     c                   ENDSL
304000021217      *
304100021217     c                   endsr
304200021217      *-------------------------------------------------------------------------
304300991027      *  CARICO SUBFILE VARIE
304400991027      *-------------------------------------------------------------------------
304500991027     c     CAR_VARIE     BEGSR
304600991128      *
304700991027      *
304800991027     c                   SELECT
304900991027      *
305000991027     c                   WHEN       WSVN = *BLANKS
305100991027      *
305200991027     C                   WHEN       WSVN = §$2iva
305300991027     c                   z-add     Wvan          W0133
305400991027     c                   eval      Wtstdec = §cvfdc
305500991027     c                   exsr      EDTimporti
305600991027     c                   movel     IMPeditato    VIDiva
305700991027      *
305800991027     c                   OTHER
305900991027     c                   z-add     Wvan          W0133
306000991027     c                   eval      Wtstdec = §cvfdc
306100991027     c                   exsr      EDTimporti
306200991027     c                   movel     IMPeditato    V6Cva1
306300991027     c                   movel     WSVN          V6Csv1
306400991027     C                   Z-ADD     1             WA
306500991027     C     V6CSV1        LOOKUP    CC1(WA)                                99
306600020329     C     CC3(WA)       ifeq      'S'
306700020329      *
306800020329      * Verifico se si tratta di una bolla Fedex
306900021119     c                   if        LnaNtw = 'FED'
307000020329     C                   Z-ADD     1             WA
307100020329     C     V6CSV1        LOOKUP    CB1(WA)                                99
307200020329     C   99              MOVEL     CB2(WA)       V6CDE1
307300020329     C  N99              clear                   V6CDE1
307400020329     C                   else
307500991027     C   99              MOVEL     CC2(WA)       V6CDE1
307600991027     C  N99              MOVEL     *BLANKS       V6CDE1
307700020329     C                   endif
307800020329     C                   else
307900020329     C   99              MOVEL     CC2(WA)       V6CDE1
308000020329     C  N99              MOVEL     *BLANKS       V6CDE1
308100020329     C                   endif
308200991027     C                   add       1             NR6
308300131107     C                   WRITE     LRI6S06
308400991027      *
308500991027     c                   ENDSL
308600991027      *
308700991027     c                   ENDSR
308800040122      *-------------------------------------------------------------------------
308900040122      *  Carico le varie della seconda bolla
309000040122      *-------------------------------------------------------------------------
309100040122     c     Car_Varie1    BegSr
309200040122
309300040122s   1c                   Select
309400040122
309500040122     c                   When      wsvn = *Blanks
309600040122      * Iva
309700040122     c                   When      wsvn = §$2iva
309800040122     c                   z-add     wvan          w0133
309900040122     c                   Eval      Wtstdec = §cvfdc
310000040122     c                   ExSr      EDTimporti
310100040122     c                   Movel     IMPeditato    V9cIva
310200070411     c                   setoff                                       06
310300070411      * Bollo
310400070411     c                   When      wsvn = §$2bol
310500070411     c                   z-add     wvan          w0133
310600070411     c                   Eval      Wtstdec = §cvfdc
310700070411     c                   ExSr      EDTimporti
310800070411     c                   Movel     IMPeditato    V9cIva
310900070411     c                   seton                                        06
311000040122      * Varia G - sempre per prima
311100040122     c                   When      wsvn = §$2vrg
311200040122     c                   Eval      V9cSv1 = wsvn
311300040122     c                   z-add     wvan          w0133
311400040122     c                   Eval      Wtstdec = §cvfdc
311500040122     c                   ExSr      EDTimporti
311600040122     c                   Movel     IMPeditato    V9cVa1
311700040122      * Altre varie
311800040122   x1c                   Other
311900040122     c                   z-add     wvan          W0133
312000040122     c                   eval      Wtstdec = §cvfdc
312100040122     c                   exsr      EDTimporti
312200040122if  2c                   If        V9cSv2 = *Blanks
312300040122     c                   movel     IMPeditato    V9cVa2
312400040122     c                   movel     wsvn          V9csv2
312500040122   x2c                   Else
312600040122if  3c                   If        V9cSv3 = *Blanks
312700040122     c                   movel     IMPeditato    V9cVa3
312800040122     c                   movel     wsvn          V9csv3
312900040122   x3c                   Else
313000040122if  4c                   If        V9cSv4 = *Blanks
313100040122     c                   movel     IMPeditato    V9cVa4
313200040122     c                   movel     wsvn          V9csv4
313300041130   x4c                   Else
313400041130if  5c                   If        V9cSv5 = *Blanks
313500041130     c                   movel     IMPeditato    V9cVa5
313600041130     c                   movel     wsvn          V9csv5
313700041130   x5c                   Else
313800041130if  6c                   If        V9cSv6 = *Blanks
313900041130     c                   movel     IMPeditato    V9cVa6
314000041130     c                   movel     wsvn          V9csv6
314100041130    6c                   EndIf
314200041130    5c                   EndIf
314300040122    4c                   EndIf
314400040122    3c                   EndIf
314500040122    2c                   EndIf
314600040122
314700040122    1c                   EndSl
314800040122
314900040122     c                   EndSr
315000991027      *****************************************************************
315100991027      *  AGGANCIO ANAGRAFICHE
315200991027      *****************************************************************
315300991027     C     CTR_ANAGRA    BEGSR
315400991027      *
315500991027     C                   clear                   DS_cnaco
315600991027     C                   clear                   DS_cnind
315700991027     C                   clear                   DS_cnclp
315800991027     C                   clear                   DS_fncls
315900991027      *
316000991027     C                   CALL      'TIBS69R'
316100991027     C                   PARM                    tibs69DS
316200991027     C                   PARM                    DS_cnaco
316300991027     C                   PARM                    DS_cnind
316400991027     C                   PARM                    DS_cnclp
316500991027     C                   PARM                    DS_fncls
316600991027      *
316700991027     C                   eval      wtibs69r = 'S'
316800991027      * per il 1° giro è inizializzata nelle specifiche "D"
316900991027     C                   clear                   TIBS69DS
317000991027      *
317100991027     C                   ENDSR
317200991027      **********************************************************************
317300991027      * AGGANCIO TABELLA VALUTE
317400991027      **********************************************************************
317500991027     C     CTR_VALUTA    BEGSR
317600991027
317700991027     C                   MOVEL     'CV'          COD
317800991027     C                   MOVEL(P)  WDIV          KEY
317900991027     C     KTAB1         CHAIN     TABEL00F                           31
318000991027     C  N31              MOVEL     TBLUNI        DSCV
318100991027
318200991027     C                   ENDSR
318300991027      **********************************************************************
318400991027      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
318500991027      **********************************************************************
318600991027     C     EDTIMPORTI    BEGSR
318700991027
318800991027      * ------------------------------------------------------------------------
318900991027      * W0133      contiene il numero da editare per il video
319000991027      * WTSTDEC    corrisponde a §CVfdc della tabella CV
319100991027      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
319200991027      * ------------------------------------------------------------------------
319300991027      *
319400991027     c                   clear                   IMPeditato
319500991027     c                   clear                   W017A
319600991027      *
319700991027     c                   if        Wtstdec = 'S'
319800991027     C                   eval      W017a = %EDITC(W0133:'2')
319900991027     c                   else
320000991027     c                   z-add     W0133         W0130
320100991027     C                   eval      W017a = %EDITC(W0130:'2')
320200991027     c                   endif
320300991027      *
320400991027     c                   move      W017a         IMPeditato
320500991027      *
320600991027     c                   ENDSR
320700940802     C**************************************************************************
320800940802     C*    LEGAMI BOLLA
320900940802     C**************************************************************************
321000930413     C     LEGBOL        BEGSR
321100940802     C*
321200940912     C* DEVO MEMORIZZARE LA BOLLA POICHE DEVO RIAGGANCIARLA DOPO
321300940912     C                   MOVEL     ARBAAS        WAAS
321400940912     C                   MOVEL     ARBLNP        WLNP
321500940912     C                   MOVEL     ARBNRS        WNRS
321600940912     C                   MOVEL     ARBNSP        WNSP
321700940912     C*
321800940907     C* PULISCO IL SFL
321900950825     C                   SETOFF                                       787923
322000131107     C                   WRITE     LRI6C08
322100950825     C                   SETON                                        7923
322200940923     C                   MOVEL     *BLANKS       LEGAME
322300940907     C*
322400940907     C                   SETOFF                                       94
322500950718     C                   Z-ADD     *ZERO         NR8
322600940907     C                   Z-ADD     *ZERO         SAVNR8            4 0
322700940907     C                   MOVEL     *BLANKS       WFLG1             1
322800940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
322900940907     C*
323000940907     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
323100940907     C     KARB          CHAIN     FNLBL01L                           99
323200940907     C   99KARB          CHAIN     FNLBL02L                           99
323300940907     C  N99              DO
323400940907     C*
323500940907     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
323600940907     C                   MOVEL     LBLAAO        KAAS
323700940907     C                   MOVEL     LBLLPO        KLNP
323800940907     C                   MOVEL     LBLNRO        KNRS
323900940907     C                   MOVEL     LBLNSO        KNSP
324000940907     C     KLBL3         CHAIN     FNLBL02L                           99
324100950103     C     *IN99         IFEQ      *ON
324200950103     C                   MOVEL     'N'           WORIGI            1
324300950103     C                   ELSE
324400950103     C                   MOVEL     'S'           WORIGI
324500950103     C                   ENDIF
324600940907     C*
324700940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
324800940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
324900950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
325000950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
325100940907     C                   MOVEL     ' '           V8CSCE
325200940907     C                   MOVEL     LBLAAO        V8CAAP
325300940907     C                   MOVEL     LBLLPO        V8CLPP
325400940907     C                   MOVEL     LBLLAO        V8CLAP
325500940907     C                   MOVEL     LBLNRO        V8CNRP
325600940907     C                   MOVEL     LBLNSO        V8CNSP
325700940926     C                   CLEAR                   V8CAAN
325800940926     C                   CLEAR                   V8CLPN
325900940926     C                   CLEAR                   V8CLAN
326000940926     C                   CLEAR                   V8CNRN
326100940926     C                   CLEAR                   V8CNSN
326200940907     C*
326300940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
326400940907     C     LBLAAO        IFEQ      V3CAAS
326500940907     C     LBLLPO        ANDEQ     V3CLNP
326600940907     C     LBLNRO        ANDEQ     V3CNRS
326700940907     C     LBLNSO        ANDEQ     V3CNSP
326800940907     C                   MOVEL     *BLANKS       V8CPRE
326900940907     C                   SETON                                        80
327000940907     C                   SETON                                        81
327100940907     C                   MOVEL     '1'           WFLG1
327200940907     C                   ELSE
327300940907     C                   SETOFF                                       81
327400940907     C*
327500950316     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
327600950316     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
327700950316     C                   SETOFF                                       93
327800950316     C     KLBL3         SETLL     FNARB01L                               93
327900950316     C  N93              MOVEA     PRD(1)        V8CPRE
328000950316     C  N93              SETON                                        80
328100150423     C** 93              MOVEA     PRD(2)        V8CPRE
328200150423     C** 93              SETOFF                                       80
328300950316     C*
328400950316     C* SE NON TROVATA SU ARB PROVO SU BLP
328500950316     C  N93              DO
328600950316     C                   SETOFF                                       93
328700950316     C     KLBL3         SETLL     FNBLP01L                               93
328800010712      *
328900010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
329000010712     c                   if        *in93 = *off
329100010712     C                   MOVEA     PRD(1)        V8CPRE
329200010712     c                   else
329300150423     C                   MOVEA     PRD(2)        V8CPRE
329400150423     C                   SETOFF                                       80
329500010712     C                   ENDIF
329600950316     C*
329700950316     C                   ENDDO
329800940907     C*
329900940907     C                   ENDIF
330000940907     C                   SETON                                        83
330100950103     C*
330200950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
330300950103     C                   MOVEL     *IN80         V8CI80
330400950103     C                   MOVEL     *IN81         V8CI81
330500950103     C                   MOVEL     *IN82         V8CI82
330600950103     C                   MOVEL     *IN83         V8CI83
330700950103     C*
330800940907     C                   Z-ADD     1             NR8
330900131107     C                   WRITE     LRI6S08
331000950825     C                   SETON                                        78
331100950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
331200940907     C*
331300950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
331400950103     C*                      2) NON HO TROVATO L'ORIGINALE
331500950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
331600950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
331700950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
331800950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
331900950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
332000950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
332100950103     C* L'ORIGINALE E LA MAMMA.
332200950103     C     WORIGI        IFEQ      'N'
332300950103     C                   MOVEL     *BLANKS       V8CSCE
332400950103     C                   CLEAR                   V8CAAP
332500950103     C                   CLEAR                   V8CLPP
332600950103     C                   CLEAR                   V8CLAP
332700950103     C                   CLEAR                   V8CNRP
332800950103     C                   CLEAR                   V8CNSP
332900950103     C                   MOVEL     LBLAAP        V8CAAN
333000950103     C                   MOVEL     LBLLPP        V8CLPN
333100950103     C                   MOVEL     LBLLAP        V8CLAN
333200950103     C                   MOVEL     LBLNRP        V8CNRN
333300950103     C                   MOVEL     LBLNSP        V8CNSN
333400950103     C                   SETOFF                                       8182
333500950103     C*
333600950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
333700950103     C     V8CAAN        IFEQ      V3CAAS
333800950103     C     V8CLPN        ANDEQ     V3CLNP
333900950103     C     V8CNRN        ANDEQ     V3CNRS
334000950103     C     V8CNSN        ANDEQ     V3CNSP
334100950103     C                   MOVEL     *BLANKS       V8CPRE
334200950103     C                   SETON                                        80
334300950103     C                   SETON                                        82
334400950103     C                   ELSE
334500950103     C*
334600950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
334700950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
334800950316     C                   EXSR      CHKBOL
334900950103     C*
335000950103     C                   ENDIF
335100950103     C*
335200950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
335300950103     C                   SETOFF                                       83
335400950103     C*
335500950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
335600950103     C                   MOVEL     *IN80         V8CI80
335700950103     C                   MOVEL     *IN81         V8CI81
335800950103     C                   MOVEL     *IN82         V8CI82
335900950103     C                   MOVEL     *IN83         V8CI83
336000950103     C*
336100950103     C                   ADD       1             NR8
336200131107     C                   WRITE     LRI6S08
336300950103     C*
336400950103     C                   ENDIF
336500940907     C                   SETOFF                                       83  99
336600940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
336700940907     C* CICLO FINO ALL'ULTIMO LEGAME
336800940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
336900940907     C                   Z-ADD     *ZERO         WA                3 0
337000940907     C                   Z-ADD     *ZERO         WB                3 0
337100940907     C                   MOVEA     *ZERO         AAN
337200940907     C     *IN99         DOWEQ     *OFF
337300940907     C*
337400940907     C                   MOVEL     ' '           V8CSCE
337500940907     C                   MOVEL     LBLAAP        V8CAAP
337600940907     C                   MOVEL     LBLLPP        V8CLPP
337700940907     C                   MOVEL     LBLLAP        V8CLAP
337800940907     C                   MOVEL     LBLNRP        V8CNRP
337900940907     C                   MOVEL     LBLNSP        V8CNSP
338000940907     C                   MOVEL     LBLAAN        V8CAAN
338100940907     C                   MOVEL     LBLLPN        V8CLPN
338200940907     C                   MOVEL     LBLLAN        V8CLAN
338300940907     C                   MOVEL     LBLNRN        V8CNRN
338400940907     C                   MOVEL     LBLNSN        V8CNSN
338500940907     C                   SETOFF                                       8182
338600940907     C*
338700950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
338800940907     C     LBLAAP        IFEQ      V3CAAS
338900940907     C     LBLLPP        ANDEQ     V3CLNP
339000940907     C     LBLNRP        ANDEQ     V3CNRS
339100940907     C     LBLNSP        ANDEQ     V3CNSP
339200940907     C                   SETON                                        81
339300940907     C                   ENDIF
339400940907     C*
339500940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
339600940907     C     LBLAAN        IFEQ      V3CAAS
339700940907     C     LBLLPN        ANDEQ     V3CLNP
339800940907     C     LBLNRN        ANDEQ     V3CNRS
339900940907     C     LBLNSN        ANDEQ     V3CNSP
340000940907     C                   MOVEL     *BLANKS       V8CPRE
340100940907     C                   SETON                                        80
340200940907     C                   SETON                                        82
340300940907     C                   ELSE
340400940907     C*
340500940907     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
340600940907     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
340700950316     C                   EXSR      CHKBOL
340800940907     C*
340900940907     C                   ENDIF
341000950103     C*
341100950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
341200950103     C                   MOVEL     *IN80         V8CI80
341300950103     C                   MOVEL     *IN81         V8CI81
341400950103     C                   MOVEL     *IN82         V8CI82
341500950103     C                   MOVEL     *IN83         V8CI83
341600940907     C*
341700940907     C* SCRIVO IL RECORD NEL SFL
341800940907     C                   ADD       1             NR8
341900131107     C                   WRITE     LRI6S08
342000940907     C*
342100940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
342200940907     C                   ADD       1             WA
342300940907     C                   MOVEL     LBLAAN        AAN(WA)
342400940907     C                   MOVEL     LBLLPN        LPN(WA)
342500940907     C                   MOVEL     LBLNRN        NRN(WA)
342600940907     C                   MOVEL     LBLNSN        NSN(WA)
342700940907     C*
342800940907     C     KLBL3         READE     FNLBL02L                               99
342900940907     C*
343000940907     C* SE NON HO PIU' BOLLE CON READE
343100940907     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
343200970116     C     WB            DOWLT     WA
343300970116     C     *IN99         ANDEQ     *ON
343400940907     C                   ADD       1             WB
343500940907     C                   MOVEL     AAN(WB)       KAAS
343600940907     C                   MOVEL     LPN(WB)       KLNP
343700940907     C                   MOVEL     NRN(WB)       KNRS
343800940907     C                   MOVEL     NSN(WB)       KNSP
343900940907     C     KLBL3         SETLL     FNLBL02L
344000940907     C     KLBL3         READE     FNLBL02L                               99
344100970116     C                   ENDDO
344200940907     C*
344300940907     C                   ENDDO
344400940907     C                   ENDDO
344500940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
344600940907     C*
344700940907     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
344800940907     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
344900940907     C     NR8           IFEQ      2
345000940907     C*
345100940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
345200131107     C     2             CHAIN     LRI6S08                            99
345300940907     C     V8CAAP        IFEQ      V3CAAS
345400940907     C     V8CLPP        ANDEQ     V3CLNP
345500940907     C     V8CNRP        ANDEQ     V3CNRS
345600940907     C     V8CNSP        ANDEQ     V3CNSP
345700940907     C                   MOVEL     V8CAAN        COMAAN
345800940907     C                   MOVEL     V8CLPN        COMLPN
345900940907     C                   MOVEL     V8CNRN        COMNRN
346000940907     C                   MOVEL     V8CNSN        COMNSN
346100950307     C                   MOVEL     V8CLAN        COMLAN
346200950316     C                   MOVEA     'SEGUEN.'     LBL(14)
346300940907     C                   ELSE
346400940907     C                   MOVEL     V8CAAP        COMAAN
346500940907     C                   MOVEL     V8CLPP        COMLPN
346600940907     C                   MOVEL     V8CNRP        COMNRN
346700940907     C                   MOVEL     V8CNSP        COMNSN
346800950307     C                   MOVEL     V8CLAP        COMLAN
346900950316     C                   MOVEA     'PRECED.'     LBL(14)
347000940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
347100940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
347200940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
347300131107     C     1             CHAIN     LRI6S08                            99
347400940926     C* REIMPOSTO NR8=2 PERCHE' MI SERVE DOPO
347500940926     C                   Z-ADD     2             NR8
347600940907     C                   ENDIF
347700940907     C*
347800950316     C                   MOVEA     COMLPN        LBL(21)
347900950316     C                   MOVEA     COMNRN        LBL(25)
348000950316     C                   MOVEA     COMNSN        LBL(28)
348100950316     C                   MOVEA     COMLAN        LBL(40)
348200940907     C*
348300940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
348400940907     C                   Z-ADD     1             W                 1 0
348500940907     C     V8CPRE        LOOKUP    PRD(W)                                 99
348600950307     C                   MOVEA     LBD(W)        LBL(44)
348700940907     C     W             COMP      2                                      94
348800940907     C*
348900940907     C                   MOVEA     LBL           LEGAME
349000940907     C                   ELSE
349100940907     C*
349200940907     C     NR8           IFGT      2
349300940907     C                   MOVEA     LB2           LEGAME
349400940907     C                   SETON                                        94
349500940907     C                   ENDIF
349600940907     C                   ENDIF
349700940907     C*
349800940907     C     NR8           COMP      1                                    3434
349900940907     C*
350000940907     C                   Z-ADD     NR8           SAVNR8
350100940907     C                   Z-ADD     1             NR8
350200940923     C     LEGAME        COMP      *BLANKS                            9292
350300940912     C*
350400940912     C* RIAGGANCIO LA BOLLA IN ESAME
350500940912     C                   MOVEL     WAAS          ARBAAS
350600940912     C                   MOVEL     WLNP          ARBLNP
350700940912     C                   MOVEL     WNRS          ARBNRS
350800940912     C                   MOVEL     WNSP          ARBNSP
350900150423     c     karb2         chain     fnblp01l
351000150423     C     KARB2         CHAIN     FNARB01L
351100930511     C*
351200930413     C                   ENDSR
351300950316     C**************************************************************************
351400950316     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
351500950316     C**************************************************************************
351600950316     C     CHKBOL        BEGSR
351700950316     C*
351800950316     C                   SETOFF                                       93
351900950316     C     KLBL1         SETLL     FNARB01L                               93
352000950316     C  N93              MOVEA     PRD(1)        V8CPRE
352100950316     C  N93              SETON                                        80
352200950316     C   93              MOVEA     PRD(2)        V8CPRE
352300950316     C   93              SETOFF                                       80
352400950316     C*
352500950316     C* SE NON TROVATA SU ARB PROVO SU BLP
352600950316     C  N93              DO
352700950316     C                   SETOFF                                       93
352800950316     C     KLBL1         SETLL     FNBLP01L                               93
352900010712      *
353000010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
353100010712     c                   if        *in93 = *off
353200010712     C                   MOVEA     PRD(1)        V8CPRE
353300010712     c                   else
353400950316     C                   MOVEA     PRD(2)        V8CPRE
353500950316     C                   SETOFF                                       80
353600010712     C                   ENDIF
353700950316     C*
353800950316     C                   ENDDO
353900950316     C*
354000950316     C                   ENDSR
354100940802     C**************************************************************************
354200940802     C*    VISUALIZZA BOLLA
354300940802     C**************************************************************************
354400911023     C     VISUAL        BEGSR
354500940321      *
354600940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
354700131108     c     Emriepil      tag
354800940712     C                   EXSR      RIEPIL
354900940712      *
355000131108      * PER F15 PASSO ALLA BOLLA, PER INVIO o F12 RITORNO AL SFL
355100940712     C  NKP              GOTO      ENDVIS
355200931112      *
355300931112     C                   SETOFF                                       89
355400940802     C                   MOVEL     'S'           WRIEM             1
355500021217      *
355600021217      * Riempio il campone in fondo alla videata con i tasti scelti
355700021217     C                   exsr      Ges_TastiFun
355800021217     c                   eval      FD03 = FDxx
355900021217     c                   eval      F24D03 = F24Dxx
356000911023     C     FOR03         TAG
356100940802     C     WRIEM         IFEQ      'S'
356200131107     C                   WRITE     LRI6T01
356300940802     C                   MOVEL     *BLANKS       WRIEM
356400940802     C                   ENDIF
356500021217      *
356600021217      * Imposto la stringa comandi del 'giro' successivo
356700021217     C                   IF        *inKY = *on and $Loop > 1
356800021217     c                   Exsr      Ges_F24
356900021217     c                   eval      FD03 = FDxx
357000021217     c                   eval      F24D03 = F24Dxx
357100021217     C                   ENDIF
357200021217      *
357300131107     C                   EXFMT     LRI6D03
357400070420     c                   setoff                                       28
357500070420     c                   clear                   v1cmsg
357600931112      *
357700131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
357800931112     C   KY*IN89         IFEQ      *ON
357900931112     C                   SETOFF                                       89
358000931112     C                   ELSE
358100931112     C                   SETON                                        89
358200931112     C                   ENDIF
358300931112     C   KY              GOTO      FOR03
358400911023     C*
358500131108     C* F3 - FINE LAVORO TORNO A SUBFILE
358600940714     C   KC              MOVEL     '2'           V2CFLG            1
358700911023     C   KC              GOTO      ENDVIS
358800911023     C*
358900131108     C* F12 - FINE LAVORO tolno a videata di riepilogo
359000131108     C   KL              GOTO      emriepil
359100911023     C*
359200911024     C*
359300930517     C* CMD18 - INTERROGA LEGAME BOLLA
359400930518     C   KS              DO
359500930518     C                   EXSR      CMD18
359600940907     C                   MOVEL     'S'           WRIEM
359700930518     C                   GOTO      FOR03
359800930518     C                   END
359900930517     C*
360000940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
360100920804     C                   EXSR      COMAND
360200940905     C     WCMD          IFEQ      'S'
360300940802     C     WF03          IFEQ      *BLANKS
360400070403     c
360500920804     C                   GOTO      FOR03
360600940802     C                   ELSE
360700940802     C                   MOVEL     '2'           V2CFLG
360800940802     C                   GOTO      ENDVIS
360900940802     C                   ENDIF
361000940905     C                   ENDIF
361100911023     C*
361200911023     C* FORMATO4
361300931112     C                   SETOFF                                       89
361400940802     C                   MOVEL     'S'           WRIEM
361500021217      *
361600021217      * Riempio il campone in fondo alla videata con i tasti scelti
361700021217     C                   exsr      Ges_TastiFun
361800021217     c                   eval      FD04 = FDxx
361900021217     c                   eval      F24D04 = F24Dxx
362000911023     C     FOR04         TAG
362100940802     C     WRIEM         IFEQ      'S'
362200131107     C                   WRITE     LRI6T01
362300940802     C                   MOVEL     *BLANKS       WRIEM
362400940802     C                   ENDIF
362500021217      * Imposto la stringa comandi del 'giro' successivo
362600021217     C                   IF        *inKY = *on and $Loop > 1
362700021217     c                   Exsr      Ges_F24
362800021217     c                   eval      FD04 = FDxx
362900021217     c                   eval      F24D04 = F24Dxx
363000021217     C                   ENDIF
363100131107     C                   EXFMT     LRI6D04
363200070420     c                   setoff                                       28
363300070420     c                   clear                   v1cmsg
363400931112      *
363500131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
363600931112     C   KY*IN89         IFEQ      *ON
363700931112     C                   SETOFF                                       89
363800931112     C                   ELSE
363900931112     C                   SETON                                        89
364000931112     C                   ENDIF
364100931112     C   KY              GOTO      FOR04
364200911023     C*
364300131108     C* F12 - RITORNO VIDEATA 3
364400911023     C   KL              GOTO      FOR03
364500920804     C*
364600131108     C* F18 - INTERROGA LEGAME BOLLA
364700930518     C   KS              DO
364800930518     C                   EXSR      CMD18
364900930518     C                   GOTO      FOR04
365000930518     C                   END
365100940923     C*
365200940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
365300131120     c                   eval      wvideo='4'
365400131120     C                   EXSR      CTR_puIn
365500940923     C     WRIEM         CABEQ     'S'           FOR04
365600930517     C*
365700940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
365800920804     C                   EXSR      COMAND
365900940905     C     WCMD          IFEQ      'S'
366000940802     C     WF03          IFEQ      *BLANKS
366100940802     C                   GOTO      FOR04
366200940802     C                   ELSE
366300940802     C                   MOVEL     '2'           V2CFLG
366400940802     C                   GOTO      ENDVIS
366500940802     C                   ENDIF
366600940905     C                   ENDIF
366700911023     C*
366800911023     C* VIDEATA 5
366900931112     C                   SETOFF                                       89
367000940809     C                   MOVEL     'S'           WRIEM
367100021217      *
367200021217      * Riempio il campone in fondo alla videata con i tasti scelti
367300021217     C                   exsr      Ges_TastiFun
367400021217     c                   eval      FD05 = FDxx
367500021217     c                   eval      F24D05 = F24Dxx
367600911023     C     FOR05         TAG
367700940809     C     WRIEM         IFEQ      'S'
367800131107     C                   WRITE     LRI6T01
367900940809     C                   MOVEL     *BLANKS       WRIEM
368000940809     C                   ENDIF
368100951220     C                   Z-ADD     1             REC5
368200021217      *
368300021217      * Imposto la stringa comandi del 'giro' successivo
368400021217     C                   IF        *inKY = *on and $Loop > 1
368500021217     c                   Exsr      Ges_F24
368600021217     c                   eval      FD05 = FDxx
368700021217     c                   eval      F24D05 = F24Dxx
368800021217     C                   ENDIF
368900131107     C                   WRITE     LRI6Z05
369000131107     C                   EXFMT     LRI6C05
369100070420     c                   setoff                                       28
369200070420     c                   clear                   v1cmsg
369300931112      *
369400131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
369500931112     C   KY*IN89         IFEQ      *ON
369600931112     C                   SETOFF                                       89
369700931112     C                   ELSE
369800931112     C                   SETON                                        89
369900931112     C                   ENDIF
370000931112     C   KY              GOTO      FOR05
370100911023     C*
370200131108     C* F12 - RITORNO VIDEATA 2
370300911023     C   KL              GOTO      FOR04
370400920804     C*
370500940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
370600920804     C                   EXSR      COMAND
370700940905     C     WCMD          IFEQ      'S'
370800940802     C     WF03          IFEQ      *BLANKS
370900940802     C                   GOTO      FOR05
371000940802     C                   ELSE
371100131108     C                   MOVEL     '2'           V2CFLG
371200131108     C                   GOTO      ENDVIS
371300940802     C                   ENDIF
371400070406
371500070406     c                   else
371600070406     c
371700070406     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
371800070406     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
371900070406     c                   if        v3cgesd='?'
372000070406     c                   exsr      VISUALAbi
372100070406     c                   clear                   v3cgesd
372200070406     c                   eval      wriem='S'
372300070406     C                   GOTO      FOR05
372400070406     c                   endif
372500940905     C                   ENDIF
372600131108     c
372700131108     C                   GOTO      emriepil
372800931112      *
372900911023     C     ENDVIS        ENDSR
373000070403     C**************************************************************************
373100070403     C*    VISUALIZZA Abilitazioni
373200070403     C**************************************************************************
373300070403     C     VISUALAbi     BEGSR
373400070403     c*
373500131120     c                   clear                   fnlrg7ds
373600131120     c                   move      v3cdsp        ilrg7aas
373700131120     c                   eval      ilrg7lnp=v3clnp
373800131120     c                   eval      ilrg7nrs=v3cnrs
373900131120     c                   eval      ilrg7nsp=v3cnsp
374000131120     c                   eval      ilrg7vsdet='?'
374100131120     c                   eval      ilrg7lung=58
374200131120     c                   call      'FNLRG7R'
374300131120     c                   parm                    fnlrg7ds
374400070403     c
374500070403     C                   ENDSR
374600940712     C**************************************************************************
374700940802     C*    VISUALIZZA BOLLA PRECEDENTE
374800940802     C**************************************************************************
374900930510     C     VISUA2        BEGSR
375000940321      *
375100940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
375200131108     c     Emriepi2      tag
375300940712     C                   EXSR      RIEPI2
375400940712      *
375500131108      * PER F15 PASSO ALLA BOLLA, PER INVIO RITORNO AL SFL
375600940713     C  NKP              GOTO      ENDV8C
375700931112      *
375800931112     C                   SETOFF                                       89
375900940809     C                   MOVEL     'S'           WRIEM
376000021217      *
376100021217      * Riempio il campone in fondo alla videata con i tasti scelti
376200021217     C                   exsr      Ges_TastiFun
376300070516     c                   eval      FD03 = FDxx
376400070516     c                   eval      F24D03 = F24Dxx
376500930510     C     FOR033        TAG
376600940809     C     WRIEM         IFEQ      'S'
376700131107     C                   WRITE     LRI6T01
376800940809     C                   MOVEL     *BLANKS       WRIEM
376900940809     C                   ENDIF
377000021217      *
377100021217      * Imposto la stringa comandi del 'giro' successivo
377200021217     C                   IF        *inKY = *on and $Loop > 1
377300021217     c                   Exsr      Ges_F24
377400021217     c                   eval      FD03 = FDxx
377500021217     c                   eval      F24D03 = F24Dxx
377600021217     C                   ENDIF
377700131107     C                   EXFMT     LRI6D03
377800070420     c                   setoff                                       28
377900070420     c                   clear                   v1cmsg
378000931112      *
378100131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
378200931112     C   KY*IN89         IFEQ      *ON
378300931112     C                   SETOFF                                       89
378400931112     C                   ELSE
378500931112     C                   SETON                                        89
378600931112     C                   ENDIF
378700931112     C   KY              GOTO      FOR033
378800930510     C*
378900131108     C* F12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
379000930510     C   KC
379100131108     COR KL              GOTO      Emriepi2
379200930510     C*
379300940905     C* CONTROLLO SE PREMUTI TASTI FUNZIONALI E, NEL CASO, RIEMETTO
379400930510     C                   EXSR      COMAND
379500940905     C     WCMD          IFEQ      'S'
379600930510     C                   GOTO      FOR033
379700930510     C                   END
379800931112      *
379900930510     C* FORMATO4
380000931112     C                   SETOFF                                       89
380100940809     C                   MOVEL     'S'           WRIEM
380200021217      *
380300021217      * Riempio il campone in fondo alla videata con i tasti scelti
380400021217     C                   exsr      Ges_TastiFun
380500021217     c                   eval      FD04 = FDxx
380600021217     c                   eval      F24D04 = F24Dxx
380700930510     C     FOR044        TAG
380800940809     C     WRIEM         IFEQ      'S'
380900131107     C                   WRITE     LRI6T01
381000940809     C                   MOVEL     *BLANKS       WRIEM
381100940809     C                   ENDIF
381200021217      *
381300021217      * Imposto la stringa comandi del 'giro' successivo
381400021217     C                   IF        *inKY = *on and $Loop > 1
381500021217     c                   Exsr      Ges_F24
381600021217     c                   eval      FD04 = FDxx
381700021217     c                   eval      F24D04 = F24Dxx
381800021217     C                   ENDIF
381900131107     C                   EXFMT     LRI6D04
382000070420     c                   setoff                                       28
382100070420     c                   clear                   v1cmsg
382200931112      *
382300931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
382400931112     C   KY*IN89         IFEQ      *ON
382500931112     C                   SETOFF                                       89
382600931112     C                   ELSE
382700931112     C                   SETON                                        89
382800931112     C                   ENDIF
382900931112     C   KY              GOTO      FOR044
383000930510     C*
383100930510     C* CMD12 - RITORNO VIDEATA 3
383200930510     C   KL              GOTO      FOR033
383300940923     C*
383400940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
383500131120     c                   eval      wvideo='4'
383600131120     C                   EXSR      CTR_puIn
383700940923     C     WRIEM         CABEQ     'S'           FOR044
383800930510     C*
383900940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
384000930510     C                   EXSR      COMAND
384100940905     C     WCMD          IFEQ      'S'
384200930510     C                   GOTO      FOR044
384300940905     C                   ENDIF
384400931112      *
384500930510     C* VIDEATA 5
384600931112     C                   SETOFF                                       89
384700940809     C                   MOVEL     'S'           WRIEM
384800021217      *
384900021217      * Riempio il campone in fondo alla videata con i tasti scelti
385000021217     C                   exsr      Ges_TastiFun
385100021217     c                   eval      FD05 = FDxx
385200021217     c                   eval      F24D05 = F24Dxx
385300930510     C     FOR055        TAG
385400940809     C     WRIEM         IFEQ      'S'
385500131107     C                   WRITE     LRI6T01
385600940809     C                   MOVEL     *BLANKS       WRIEM
385700940809     C                   ENDIF
385800951220     C                   Z-ADD     1             REC5
385900021217      *
386000021217      * Imposto la stringa comandi del 'giro' successivo
386100021217     C                   IF        *inKY = *on and $Loop > 1
386200021217     c                   Exsr      Ges_F24
386300021217     c                   eval      FD05 = FDxx
386400021217     c                   eval      F24D05 = F24Dxx
386500021217     C                   ENDIF
386600131107     C                   WRITE     LRI6Z05
386700131107     C                   EXFMT     LRI6C05
386800070420     c                   setoff                                       28
386900070420     c                   clear                   v1cmsg
387000931112      *
387100931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
387200931112     C   KY*IN89         IFEQ      *ON
387300931112     C                   SETOFF                                       89
387400931112     C                   ELSE
387500931112     C                   SETON                                        89
387600931112     C                   ENDIF
387700931112     C   KY              GOTO      FOR055
387800930510     C*
387900930510     C* CMD12 - RITORNO VIDEATA 2
388000930510     C   KL              GOTO      FOR044
388100930510     C*
388200940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
388300940905     C                   EXSR      COMAND
388400940905     C     WCMD          IFEQ      'S'
388500930510     C                   GOTO      FOR055
388600930510     C                   END
388700131108
388800131108     C                   GOTO      Emriepi2
388900931112      *
389000940713     C     ENDV8C        ENDSR
389100940802     C**************************************************************************
389200940802     C* TASTI DI FUNZIONE
389300940802     C**************************************************************************
389400920804     C     COMAND        BEGSR
389500940905     C*
389600940905     C* PULISCO IL FLAG CHE INDICA SE PREMUTO UN TASTO FUNZIONE
389700940905     C                   MOVE      *BLANKS       WCMD              1
389800040218      *
389900070411      *?F1 - Aggiunta bolla in reclamo?o scelta bolla generica
390000040218if  1c                   if        *inKA   = *on
390100070423     C                   Z-ADD     arbLNP        COMLNP
390200070411     C                   MOVEL     '3'           V2CFLG
390300070411      *?-
390400150402if  2c                   if        SDSprm  > 1 and ilru6ra='S'
390500131111     c                   eval      wARBlnp = ARBlnp
390600131111     c                   eval      wARBnrs = ARBnrs
390700131111     c                   eval      wARBnsp = ARBnsp
390800131111     c                   eval      wARBaas = ARBaas
390900150402     c*??????????        clear                   FIDNA1ds
391000150402     c                   eval      ELRU6INS  = 'S'
391100150402     c                   eval      OLRU6TOR  = 'S'
391200150402     c                   eval      OLRU6OGG  = ds_OggSped
391300070411    2c                   endif
391400040218     c                   goto      Fine
391500040218e   1c                   endif
391600940802     C*
391700990119     C* F5 - INTERROGAZIONE EVENTI
391800040218do  1C   KE              DO
391900940905     C                   MOVEL     'S'           WCMD
392000940726     C                   MOVEL     V2CAAS        PB5AAS
392100940726     C                   MOVEL     COMLNP        PB5LNP
392200940726     C                   MOVEL     V2CNRS        PB5NRS
392300940726     C                   MOVEL     V2CNSP        PB5NSP
392400940726     C                   MOVEL     *BLANKS       PB5F03
392500940726     C                   MOVEL     *BLANKS       PB5FLG
392600940802     C                   MOVEL     *BLANKS       PB5PGM
392700131107     C                   MOVEL     'FNLRI6R'     PB5PGM
392800940726     C                   MOVEL     V1CDSA        PB5DSA
392900940726     C                   MOVEL     V3CDSP        PB5DSP
393000940726     C                   MOVEL     VIDRMN        PB5RMN
393100940726     C                   MOVEL     V3CLNA        PB5LNA
393200940802     C                   MOVEL     V3DLNA        PB5LN1
393300940726     C                   MOVEL     V4CRSD        PB5RSD
393400940726     C                   MOVEL     VIDDCP        PB5DCP
393500941031     C                   MOVEL     VIDTMC        PB5TMC
393600940726     C                   MOVEL     PARAB5        KPJBU
393700940722     C                   CALL      'FNLRB5R'
393800930525     C                   PARM                    KPJBA
393900941109     C                   MOVEL     KPJBU         PARAB5
394000941109     C                   MOVEL     PB5F03        WF03
394100940725     C                   MOVEL     'S'           WRIEM
394200040218e   1C                   ENDDO
394300070420      *?F6 - Stampa LDV
394400070420if  1c                   if        *inKf = *on
394500070420     C                   MOVEL     'S'           WCMD
394600080201     c* Se assegnato non abilitato non stampo la LdV ma emetto errore
394700150617     c                   if        w_tbo='A' and arbaca=' '
394800160505     C                   MOVEL     MSG(3)        V1CMSG
394900080201     C                   SETON                                        28
395000080201     c                   else
395100070420     c
395200070420     c                   if        wrichtrul90=' '
395300070420     c* La prima volta devo proporre la richiesta stampanti
395400070420     c                   exsr      RICHSTAMPAN
395500070420     C                   MOVEL     'S'           WRIEM
395600070420     c
395700070420     c* Se premuto F3 esco senza fare nulla
395800070420     c                   if        d90f3<>'1' and not *in91
395900070420     c                   eval      wrichtrul90='S'
396000070420     c                   endif
396100070420     c                   if        *in91
396200160505     C                   MOVEL     MSG(2)        V1CMSG
396300070420     C                   SETON                                        28
396400070420     c                   endif
396500070420     c                   endif
396600070420     c
396700070420     c                   if        wrichtrul90='S'
396800070420     C                   CLEAR                   FNLSB5DS
396900070426     c                   movel     'L'           db0tla
397000070420     C                   MOVEL     'R'           DB0RIS
397100150617     C                   MOVEL     w_tbo         DB0TBO
397200070420     C                   MOVEL     ARBAAS        DB0AAS
397300070420     C                   MOVEL     ARBLNP        DB0LNP
397400070420     C                   MOVEL     ARBNRS        DB0NRS
397500070420     C                   MOVEL     ARBNSP        DB0NSP
397600130319     c                   reset                   fnlsB6ds1
397700070420     C                   CALL      D90PSL
397800070420     C                   PARM                    FNLSB5DS
397900130308     c                   parm                    fnlsB6ds1
398000160505     C                   MOVEL     MSG(1)        V1CMSG
398100070420     C                   SETON                                        28
398200070420     c                   endif
398300080201     c                   endif
398400070420     c                   endif
398500930525     C*
398600990119     C* F7 - ANOMALIE
398700040218do  1C   KG              DO
398800960523     C                   MOVEL     'S'           WCMD
398900960523     C*
399000991027     C                   CLEAR                   FNLr44DS
399100960523     C                   MOVE      V2CAAS        D44AAS
399200960523     C                   MOVE      COMLNP        D44FIL
399300960523     C                   MOVE      V2CNRS        D44NRS
399400960523     C                   MOVE      V2CNSP        D44NSP
399500150617     C***                MOVEL     'A'           D44TBO
399600150617     C                   MOVEL     w_tbo         D44TBO
399700960523     C                   MOVE      'I'           D44MOD
399800991027     C                   MOVEL     FNLr44DS      KPJBU
399900960523     C                   CALL      'FNLR44R'
400000960523     C                   PARM                    KPJBA
400100960523     C*
400200990119     C                   MOVEL     'S'           WRIEM
400300040218e   1C                   ENDDO
400400920804     C*
400500160628     C* F23 - INTERROGAZIONE EVENTI SEGNACOLLI
400600160628if  1C     *INKX         IFEQ      *ON
400700961121     C                   MOVEL     'S'           WCMD
400800961121     C*
400900991027     C                   CLEAR                   FNLR45DS
401000961121     C                   MOVE      V2CAAS        D45AAS
401100961121     C                   MOVE      COMLNP        D45FIL
401200961121     C                   MOVE      V2CNRS        D45NRS
401300961121     C                   MOVE      V2CNSP        D45NSP
401400991027     C                   MOVEL     FNLR45DS      KPJBU
401500961121     C                   CALL      'FNLR45R'
401600961121     C                   PARM                    KPJBA
401700961121     C*
401800990119     C                   MOVEL     'S'           WRIEM
401900040218e   1C                   ENDIF
401901160628     C* CMD 8 - VISUALIZZA ORM
401902160628if  1C     *INKH         IFEQ      *ON
401903160628     C                   MOVEL     'S'           WCMD
401904160628
401905160628     C                   clear                   PARM01
401906160628     C                   move      v3cpoe        ppoe
401907160628     C                   move      v3cnor        pnor
401908160628     C                   move      v3cnsr        pnsr
401909160628     C                   move      v3cnrv        pnrv
401910160628     C                   move      '5'           psce
401911160628     C                   move      v3clnp        pfgs
401912160628     C                   movel(p)  PARM01        KPJBU
401913160628
401914160628     C                   call      'FIOR07R'
401915160628     C                   parm                    KPJBA
401916160628     C                   MOVEL     'S'           WRIEM
401918160628
401919160628e   1C                   ENDIF
402000130830     c
402100130830     c* F9 - Orari servizi
402200131007     c                   if        *inki=*on
402300130830     C                   MOVEL     'S'           WCMD
402400131007     c                   if        wabif9='S'
402500150611     c                   clear                   W_video
402600130930     c                   exsr      sr_orarisr
402700131007     c                   endif
402800130830     C                   MOVEL     'S'           WRIEM
402900130830e   1C                   ENDIF
403000021213     C*
403100021213     C* F11 - Note supermercati
403200040218if  1C     *INKK         IFEQ      *ON
403300021213     C     *IN77         ANDEQ     *ON
403400021213      * imposto i campi 'bolla' della ds
403500021217     C                   MOVEL     'S'           WCMD
403600021213     C                   clear                   FnLrb3ds
403700030122     c                   Movel     V1cdsa        Rb3Rsu
403800030121     C                   z-add     SimFel        RB3Fil
403900021216     C                   z-add     V3CAAS        RB3AAS
404000030121     C                   z-add     V3CLNP        RB3LNP
404100021216     C                   z-add     V3CNRS        RB3NRS
404200021216     C                   z-add     V3CNSP        RB3NSP
404300021213     C                   z-add     V3CDSP        RB3DSP
404400021213     C                   z-add     VIDRMN        RB3RMN
404500021213     C                   z-add     V3CLNA        RB3LNA
404600131108     C                   movel     V32RSD        RB3RSD
404700021218     C                   z-add     VIDPKF        RB3PKF
404800021213     C                   z-add     VIDNCL        RB3NCL
404900021213     C                   z-add     VIDVLF        RB3VLB
405000030703      * bancali - part.varie "B"
405100040218If  2c                   If        %subst(ArbGva:1:1) = 'B'
405200030121     C                   z-add     §Ar5Tba       RB3Tba
405300030121     C                   z-add     VidNba        RB3Nba
405400030121     C                   z-add     §Ar5Tb2       RB3Tb2
405500030121     C                   z-add     VidNb2        RB3Nb2
405600030703     c                   Movel     V4cGva        Rb3Gva
405700040218e   2c                   EndIf
405800030703      * bancali - part.varie "O"
405900040218If  2c                   If        %subst(ArbGva:1:1) = 'O'
406000030703     C                   z-add     §Ar5bTba      RB3Tba
406100030703     C                   z-add     VidNba        RB3Nba
406200030703     C                   z-add     §Ar5bTb2      RB3Tb2
406300030703     C                   z-add     VidNb2        RB3Nb2
406400030703     c                   Movel     V4cGva        Rb3Gva
406500030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
406600030703     c                   Z-Add     §Ar5bCor      Rb3Cor
406700040218e   2c                   EndIf
406800021213      * richiamo PGM per note supermercati
406900021213     C                   call      'FNLRB3R'
407000030122     C                   PARM                    Kpjba
407100030121     C                   PARM                    Fnlrb3Ds
407200021213      *
407300021217     C                   MOVEL     'S'           WRIEM
407400040218e   1C                   ENDIF
407500961121     C*
407600990119     C* F14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
407700040218if  1C     *INKN         IFEQ      *ON
407800930624     C     *IN03         ANDEQ     *ON
407900160504     C     PARGIA        ANDEQ     *BLANK
408000940905     C                   MOVEL     'S'           WCMD
408100991027     C                   CLEAR                   FNLG30DS
408200941213     C                   MOVEL     'A'           D30POA
408300941213     C                   MOVEL     V2CAAS        D30AAS
408400941213     C                   MOVEL     COMLNP        D30LNP
408500941213     C                   MOVEL     V2CNRS        D30NRS
408600941213     C                   MOVEL     V2CNSP        D30NSP
408700991027     C                   MOVEL     FNLG30DS      KPJBU
408800941213     C                   CALL      'FNLG31C'
408900920804     C                   PARM                    KPJBA
409000930624     C*
409100990119     C                   MOVEL     'S'           WRIEM
409200040218e   1C                   ENDIF
409300040218      *
409400040218      *?F16 - Gestione reclami?
409500040218if  1c                   if        *inKQ = *on
409600040225     c                   eval      wARBaas = ARBaas
409700131111     c                   eval      wARBlnp = ARBlnp
409800131111     c                   eval      wARBnrs = ARBnrs
409900131111     c                   eval      wARBnsp = ARBnsp
410000040225     c                   reset                   FIDNA2ds
410100150402if  2c                   if        SDSprm  > 1  and ilru6ra='S'
410200040225     c                   eval      IA2mod  = 'I'
410300040225e   2c                   endif
410400040225     c                   eval      IA2ogg  = ds_OggSped
410500040225     c                   call      'FIDNA2R'
410600040225     c                   parm                    KPJBA
410700040225     c                   parm                    FIDNA2ds
410800040225     c                   movel     OA2f03        WF03
410900040225     c                   movel     'S'           WRIEM
411000051201     C                   MOVEL     'S'           WCMD
411100040218e   1c                   endif
411200070404      *
411300140311      *?F17 - Note AUT
411301160916      * (Questo tasto funzionale è attivo solo se la bolla è in arrivo. Se è
411302160916      *  solo in partenza non è attivo)
411400070404if  1c                   if        *inKR = *on
411500140123     c                   clear                   fnlrn6ds
411600140123     c                   eval      in6mod='V'
411700140123     c                   eval      in6aas=v2caas
411800140123     c                   eval      in6lnp=comlnp
411900140123     c                   eval      in6nrs=v2cnrs
412000140123     c                   eval      in6nsp=v2cnsp
412100140123      * se sono una spedizione in consegna passo i dati della distinta del file ARB
412200140123     c                   IF        ARBfbc = *blanks and
412300140123     c                               ARBddc > 0 and
412400140123     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
412500140123     c                               arbica='R' or arbicc=' ' or arbicc='R')
412600160916     c***  KSPED         chain     Fnarb01l
412700160916     c****               If        %found(Fnarb01l)
412800140123     c                   eval      in6fgs = arbifp
412900140123     c                   eval      in6nfv = arbndc
413000140123     c                   eval      in6dfv = arbddc
413100140123     c                   eval      in6pdr = arbpdc
413200140123     c                   eval      in6cons = 'S'
413300160916     c***                Endif
413400140123     c                   Endif
413500140123     c                   call      'FNLRN6R'
413600070404     c                   parm                    kpjba
413700140123     c                   parm                    fnlrn6ds
413800070404     c                   movel     'S'           WRIEM
413900070404     C                   MOVEL     'S'           WCMD
414000070404e   1c                   endif
414100920804     C*
414200990119     C* F21 - DETTAGLIO SEGNACOLLI
414300040218do  1C   KV              DO
414400940905     C                   MOVEL     'S'           WCMD
414500950628     C                   MOVEL     'A'           PA1TBO
414600950629     C                   CLEAR                   PA1F03
414700160505     C                   CLEAR                   PA1FlG
414701160610
414702160610     c                   eval      pa1AAS=kaas
414703160610     c                   eval      pa1LNP=klnp
414704160610     c                   eval      pa1NRS=knrs
414705160610     c                   eval      pa1NSP=knsp
414706160916     c                   eval      pa1tfp=arbtfp
414707160916     c                   eval      pa1tfa=arbtfa
414708160610
414800160504     C                   MOVEL     PARAMint      KPJBU
414900161019     C                   CALL      'FNLRU38R'
415000920804     C                   PARM                    KPJBA
415100160504     C                   MOVEL     KPJBU         PARAMint
415200940802     C                   MOVEL     PA1F03        WF03
415300990119     C                   MOVEL     'S'           WRIEM
415400040218e   1C                   ENDDO
415500920804     C*
415600990119     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
415700040218do  1C   KJ              DO
415800940905     C                   MOVEL     'S'           WCMD
415900150617     C***                MOVEL     'A'           PA1TBO
416000150617     C                   MOVEL     w_tbo         PA1TBO
416100950629     C                   CLEAR                   PA1F03
416200160505     C                   CLEAR                   PA1flg
416201160610     c                   eval      pa1AAS=kaas
416202160610     c                   eval      pa1LNP=klnp
416203160610     c                   eval      pa1NRS=knrs
416204160610     c                   eval      pa1NSP=knsp
416300160504     C                   MOVEL     PARAMint      KPJBU
416400940726     C                   CALL      'FNLR75R'
416500920804     C                   PARM                    KPJBA
416600160504     C                   MOVEL     KPJBU         PARAMint
416700940802     C                   MOVEL     PA1F03        WF03
416800990119     C                   MOVEL     'S'           WRIEM
416900040218e   1C                   ENDDO
417000930114     C*
417100990119     C* F19 - INTERROGA SPUNTE VARI
417200040218if  1C     *INKT         IFEQ      *ON
417300940905     C                   MOVEL     'S'           WCMD
417400940713     C                   Z-ADD     0             PA4NFV
417500940713     C                   MOVEL     9             PA4NPG
417600940713     C                   MOVEL     99            PA4NPS
417700160519     C                   MOVEL     'FNLRU6R1'    PA4PGM
417800101203     C                   MOVEL     '1'           PA4FLG
417900950214     C                   MOVEL     V3CFLS        PA4LNP
418000940719     C                   MOVEL     V3CLNA        PA4LNA
418100940719     C                   MOVEL     V3CNRS        PA4NRS
418200940713     C                   Z-ADD     0             PA4NCD
418300940713     C                   Z-ADD     0             PA4NCA
418400101201     c                   eval      pa4boan=v2caas
418500101201     c                   eval      pa4bolp=comlnp
418600101201     c                   eval      pa4bors=v2cnrs
418700101201     c                   eval      pa4bosp=v2cnsp
418800040218if  2C     VIDNCD        IFGT      0
418900940713     C                   MOVEL     VIDNCD        PA4NCD
419000940713     C                   MOVEL     VIDNCA        PA4NCA
419100101201x   2C**                 ELSE
419200930705     C* CERCO I SEGNACOLLI DAL DETTAGLIO
419300101201     C**   KARB2         SETLL     FNART01L
419400101201     C**   KARB2         READE     FNART01L                               31
419500101201do  3C*+N31              DO
419600101201     C**                 Z-ADD     ARTNSC        PA4NCD
419700101201     C**   KARB2         SETGT     FNART01L
419800101201     C**   KARB2         READPE    FNART01L                               31
419900101201if  4C**N31ARTNSC        IFNE      PA4NCD
420000101201     C**                 MOVEL     ARTNSC        PA4NCA
420100101201e   4C**                 ENDIF
420200101201e   3C**                 ENDDO
420300101201e   2C                   ENDIF
420400930705     C*
420500940802     C                   MOVEL     PARA04        KPJBU
420600940722     C                   CALL      'FNLR04R'
420700920804     C                   PARM                    KPJBA
420800940725     C                   MOVEL     'S'           WRIEM
420900060130     C                   MOVEL     'S'           WCMD
421000040218e   1C                   ENDIF
421100930415     C*
421200070404     c
421300931112      *
421400990119      * F20 - INTERROGAZIONE AFFIDABILITA'
421500040218do  1C   KU              DO
421600940905     C                   MOVEL     'S'           WCMD
421700950102     C                   CLEAR                   FNLR97
421800950102     C                   MOVEL     V2CAAS        R97AAS
421900950102     C                   MOVEL     COMLNP        R97LNP
422000950102     C                   MOVEL     V2CNRS        R97NRS
422100950102     C                   MOVEL     V2CNSP        R97NSP
422200950102     C                   MOVEL     V3CDSP        R97DSP
422300950102     C                   MOVEL     VIDRMN        R97RMN
422400950102     C                   MOVEL     V3CLNA        R97LNA
422500950102     C                   MOVEL     V3DLNA        R97DLA
422600950102     C                   Z-ADD     VIDDCP        R97DCP
422700950102     C                   MOVEL     V4CRSD        R97RSD
422800950102     C                   MOVEL     V1CDSA        R97RSU
422900040121     C                   MOVEL     *blanks       R97CML
423000160506     C                   MOVEL     w_tbo         R97AP
423100950102     C   95              MOVEL     'S'           R97EVE
423200070404     C                   Z-ADD     w08DBR        R97DBR
423300070404     C                   Z-ADD     w08DPC        R97DPC
423400070404     C                   Z-ADD     w08DUC        R97DUC
423500950102     C                   MOVEL     VIDTMC        R97TMC
423600950102     C                   Z-ADD     VIDCAS        R97CAS
423700950102     C                   MOVEL     VIDVCA        R97VCA
423800931112     C                   MOVE      *BLANKS       KPJBU
423900950102     C                   MOVEL     FNLR97        KPJBU
424000940722     C                   CALL      'FNLR97R'                            90
424100931112     C                   PARM                    KPJBA
424200950102     C                   MOVEL     KPJBU         FNLR97
424300950102     C                   MOVEL     R97F03        WF03
424400950102     C                   MOVEL     'S'           WRIEM
424500040218e   1C                   ENDDO
424600110930     C* F22 - Perfect Order
424700110929do  1C                   if        *inkw
424800110929     C                   MOVEL     'S'           WCMD
424900160606     c                   movel     paramU6ds1    paramu6ds
425000110929     c*
425100160527     c                   MOVEL     'W'           parflg_
425300160527     c                   movel(p)  vidrma        parrma_
425400160527     c                   movel(p)  vidxco        parxco_
425401160527     c                   clear                   parflo_
425500160527     c                   movel     paramU6ds     kpjbu
425600160519     c                   call      'FNLRU6C'
425700110929     c                   parm                    kpjba
425800110929     c*
426000110929     C*
426100110929     C                   MOVEL     'S'           WRIEM
426200110929e   1C                   ENDif
426300940905     C*
426400131107     C* ? FORMATO VIDEO:  LRI6D06  (TASSAZIONE 1a BOLLA)   ---   TASTO F13 ?
426500991026     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
426600040218if  1C     *INKM         IFEQ      *ON
426700991027     C                   eval      WCMD = 'S'
426800991027     C                   EVAL      *in89 = *off
426900131107     C                   WRITE     LRI6T01
427000131107     C                   WRITE     LRI6D06
427100991027     C                   eval      NR6 = 1
427200131107     C                   EXFMT     LRI6C06
427300040218e   1C                   ENDIF
427400991027     C*
427500131107     C* ? FORMATO VIDEO:  LRI6D09  (TASSAZIONE 2a BOLLA)   ---   TASTO F04 ?
427600991027     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
427700040218if  1C     *INKD         IFEQ      *ON
427800991027     C                   eval      WCMD = 'S'
427900991027     C                   EVAL      *in89 = *off
428000131107     C                   WRITE     LRI6T01
428100131107     C                   EXFMT     LRI6D09
428200040218e   1C                   ENDIF
428300990119     C*
428400990119     C* F2 - INTERROGAZIONE COMUNICAZIONE ANOMALIA
428500990129     C*      NON RICHIAMO SE RICHIAMATO DA INTERROGAZINE C.A.
428600040218if  1C     *INKB         IFEQ      *ON
428700160504     C     PArFLG        ANDNE     'C'
428800040218if  2C     CONTCA        IFEQ      1
428900991027     C                   CLEAR                   FIDN00DS
429000990119     C                   Z-ADD     VHCAAC        I00AAC
429100990119     C                   Z-ADD     VHCFIL        I00FIL
429200990119     C                   Z-ADD     VHCNCA        I00NCA
429300990119     C                   Z-ADD     ARBAAS        I00AAS
429400990119     C                   Z-ADD     ARBNRS        I00NRS
429500990119     C                   Z-ADD     ARBNSP        I00NSP
429600990119     C                   Z-ADD     ARBLNP        I00LNP
429700990119     C                   MOVEL     'A'           I00TPB
429800991027     C                   MOVEL     FIDN00DS      KPJBU
429900990119     C                   CALL      'FIDN01R'
430000990119     C                   PARM                    KPJBA
430100000309     C                   PARM                    callricors        1
430200040218x   2C                   ELSE
430300990119     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
430400990119     C                   SETOFF                                       79
430500131107     C                   WRITE     LRI6C11
430600990119     C                   SETON                                        797823
430700990119     C                   CLEAR                   NRR11
430800040809      *
430900040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
431000040809      * legate alla spedizione che passo
431100040809     c                   clear                   fidn12ds
431200040809     c                   eval      i12aas = arbAAS
431300040809     c                   eval      i12lnp = arbLNP
431400040809     c                   eval      i12nrs = arbNRS
431500040809     c                   eval      i12nsp = arbNSP
431600040809     c                   eval      i12fel = 'E'
431700040809     c                   eval      i12fan = 'E'
431800040809     c*
431900040809     c                   call      'FIDN12R'
432000040809     c                   parm                    fidn12ds
432100040809     c*
432200040809     c                   setoff                                       31
432300040809     c*
432400040809     c* se non ci sono errori
432500040809     c                   if        o12err <> ' '
432600040809     c                   seton                                        31
432700040809     c                   else
432800040809     c* se numero di CA trovate maggiore di zero
432900040809     c                   if        o12nca = 0
433000040809     c                   seton                                        31
433100040809     c                   else
433200040809     c                   z-add     1             ii                2 0
433300040809     c                   dow       ii <= o12nca and *in31 = *off
433400040809     c                   movea     skkey(ii)     dskey
433500040809     C                   Z-ADD     dsaac         V11AAC
433600040809     C                   Z-ADD     dsfil         V11FIL
433700040809     C                   Z-ADD     dsnca         V11NCA
433800040809     C                   Z-ADD     skdca(ii)     G02INV
433900040809     C                   Z-ADD     *ZERO         G02DAT
434000040809     C                   MOVEL     '3'           G02ERR
434100040809     C                   CALL      'XSRDA8'
434200040809     C                   PARM                    WLBDAT
434300040809     C                   Z-ADD     G02DAT        V11DCA
434400040809     C                   Z-ADD     skfca(ii)     V11FCA
434500040809     C     skfca(ii)     CHAIN     FNDFA01L
434600040809     C                   IF        %found(FNDFA01L)
434700040809     C                   MOVEL     DFADES        V11DFA
434800040809     C                   ENDIF
434900040809     C**
435000040809     C                   ADD       1             NRR11             4 0
435100131107     C                   WRITE     LRI6S11
435200040809     c*
435300040809     c                   add       1             ii
435400040809     c                   enddo
435500040809     c*
435600040809     c                   seton                                        31
435700040809     c*
435800040809     c                   endif
435900040809     c                   endif
436000040809     c**
436100990119     C* EMETTO FORMATO
436200990119     C     FOR11         TAG
436300990119     C                   Z-ADD     1             NRR11
436400131107     C                   WRITE     LRI6T01
436500131107     C                   WRITE     LRI6Z11
436600131107     C                   EXFMT     LRI6C11
436700990119     C* F12 - RITORNO
436800040218if  3C     *INKL         IFEQ      *OFF
436900131107     C                   READC     LRI6S11                                31
437000040218do  4C     *IN31         DOWEQ     *OFF
437100040218if  5C     V11SCE        IFEQ      '5'
437200991027     C                   CLEAR                   FIDN00DS
437300990119     C                   Z-ADD     V11AAC        I00AAC
437400990119     C                   Z-ADD     V11FIL        I00FIL
437500990119     C                   Z-ADD     V11NCA        I00NCA
437600990119     C                   Z-ADD     ARBAAS        I00AAS
437700990119     C                   Z-ADD     ARBNRS        I00NRS
437800990119     C                   Z-ADD     ARBNSP        I00NSP
437900990119     C                   Z-ADD     ARBLNP        I00LNP
438000990119     C                   MOVEL     'A'           I00TPB
438100991027     C                   MOVEL     FIDN00DS      KPJBU
438200990119     C                   CALL      'FIDN01R'
438300990119     C                   PARM                    KPJBA
438400000309     C                   PARM                    callricors        1
438500990119     C**
438600990119     C                   CLEAR                   V11SCE
438700131107     C                   UPDATE    LRI6S11
438800040218e   5C                   ENDIF
438900990119     C**
439000131107     C                   READC     LRI6S11                                31
439100040218e   4C                   ENDDO
439200990119     C                   GOTO      FOR11
439300040218e   3C                   ENDIF
439400040218e   2C                   ENDIF
439500990119     C*
439600990119     C                   MOVEL     'S'           WRIEM
439700990119     C                   MOVEL     'S'           WCMD
439800040218e   1C                   ENDIF
439900931112      *
440000920804     C                   ENDSR
440100070420     C**************************************************************************
440200070420      * RICHIEDO LE STAMPANTI per stampa LDV
440300070420     C**************************************************************************
440400070420     c     RICHSTAMPAN   BEGSR
440500070420     C                   CLEAR                   TRUL90DS
440600070420      *
440700070420     C                   MOVEL     'S'           d90RSB
440800070420     C                   CALL      'TRUL90R'
440900070420     C                   PARM                    KPJBA
441000070420     C                   PARM                    trul90DS
441100070420      *
441200070420     C* F3 - FINE
441300070420     c                   if        d90f3<>'1'
441400070420      *
441500070420      * OVRPRTF BOLLE A4
441600070426     C                   MOVEA     D90PRB4       CMA4f6(30)
441700070426     C                   MOVEA     D90MDB4       CMA4f6(52)
441800070420     C*
441900111221     C                   Z-ADD     130           LUNG             15 5
442000111221     C                   CLEAR                   COMMAN          130
442100160503     C                   MOVEA     CMA4f6        COMMAN
442200070420     C                   CALL      'QCMDEXC'                            91
442300070420     C                   PARM                    COMMAN
442400070420     C                   PARM                    LUNG
442500070420      *
442600070420     C                   if        not *in91
442700070420      *
442800070420      * OVRPRTF BOLLE A5
442900070426     C                   MOVEA     D90PRB5       CMA5f6(30)
443000070426     C                   MOVEA     D90MDB5       CMA5f6(52)
443100070420     C*
443200111221     C                   Z-ADD     130           LUNG
443300070420     C                   CLEAR                   COMMAN
443400160503     C                   MOVEA     CMA5f6        COMMAN
443500070420     C                   CALL      'QCMDEXC'                            91
443600070420     C                   PARM                    COMMAN
443700070420     C                   PARM                    LUNG
443800070420     c                   endif
443900070420     c                   endif
444000070420      *
444100070420     c                   ENDSR
444200940802     C**************************************************************************
444300940802     C*    VISUALIZZA LEGAMI BOLLA
444400940802     C**************************************************************************
444500930413     C     CMD18         BEGSR
444600940802     C*
444700940411     C                   CLEAR                   WCAR
444701160610     c                   eval      w_tbo_sav=w_tbo
444702160913     c                   eval      blp_arbdcm_sav=blp_arbdcm
444703161014     c                   eval      blp_arbhmc_sav=blp_arbhmc
444704160610     c                   eval      win12=*in12
444800940411     C*
444900940411     C* MUOVO I DATI DELLA BOLLA CHE STO VISUALIZZANDO
445000940714     C                   MOVEL     V2CAAS        NUM2              4 0
445100940411     C                   MOVEL     COMLNP        COMLPN
445200940714     C                   MOVEL     V2CNRS        COMNRN
445300940714     C                   MOVEL     V2CNSP        COMNSN
445400950321     C                   MOVEL     V3CLNA        COMLAN
445500950428     C                   MOVEL     COMC08        SAVC08
445600940408     C**
445700940713     C* SAVNR8 > 1 --> GESTIONE SUBFILE
445800940408     C**
445900940907    1C     SAVNR8        IFGT      2
446000940408     C*
446100940809     C                   MOVEL     'S'           WRIEM
446200930413     C     FORC2         TAG
446300940713     C                   Z-ADD     1             REC8
446400940809     C     WRIEM         IFEQ      'S'
446500131107     C                   WRITE     LRI6T01
446600131107     C                   WRITE     LRI6Z08
446700940809     C                   MOVEL     *BLANKS       WRIEM
446800940809     C                   ENDIF
446900950428     C*
447000950428     C                   MOVEL     SAVC08        COMC08
447100131107     C                   EXFMT     LRI6C08
447200930413     C*
447300930413     C* CMD12 - FINE LAVORO
447400930413     C   KL              GOTO      SOTTO
447500930413     C*
447600131107     C                   READC     LRI6S08                                93
447700930510     C   93              GOTO      SOTTO
447800930510     C*
447900940408    2C     *IN93         DOWEQ     '0'
448000930413     C*
448100941230    3C     V8CSCE        IFEQ      '5'
448200940713     C                   MOVEL     ' '           V8CSCE
448300950103     C*
448400950103     C* RIPRISTINO GLI INDICATORI
448500950103     C                   MOVEL     V8CI80        *IN80
448600950103     C                   MOVEL     V8CI81        *IN81
448700950103     C                   MOVEL     V8CI82        *IN82
448800950103     C                   MOVEL     V8CI83        *IN83
448900131107     C                   UPDATE    LRI6S08
449000930504     C*
449100940907     C     V8CAAN        IFEQ      *ZERO
449200940907     C                   MOVEL     V8CAAP        LBLAAN
449300940907     C                   MOVEL     V8CLPP        LBLLPN
449400940907     C                   MOVEL     V8CNRP        LBLNRN
449500940907     C                   MOVEL     V8CNSP        LBLNSN
449600940907     C                   MOVEL     'P'           V8CFPS            1
449700940907     C                   ELSE
449800940907     C                   MOVEL     V8CAAN        LBLAAN
449900940907     C                   MOVEL     V8CLPN        LBLLPN
450000940907     C                   MOVEL     V8CNRN        LBLNRN
450100940907     C                   MOVEL     V8CNSN        LBLNSN
450200940907     C                   MOVEL     'S'           V8CFPS
450300940907     C                   ENDIF
450400940408     C*
450500940408     C* CONTROLLO SE IL LEGAME E' IN PARTENZA O IN ARRIVO
450503160610     c                   clear                   w_tbo
450504160913     c                   clear                   blp_arbdcm
450505161014     c                   clear                   blp_arbhmc
450600150423     C     KLBL1         CHAIN     FNblp01L                           93
450601160610     c                   if        %found(fnblp01l)
450602160610     c                   eval      w_tbo='P'
450603160913     c                   eval      blp_arbdcm=arbdcm
450604161014     c                   eval      blp_arbhmc=arbhmc
450605160610     c                   seton                                        12
450606160610     c                   endif
450700940907     C     KLBL1         CHAIN     FNARB01L                           93
450701160610     c                   if        %found(fnarb01l)
450702160610     c                   eval      w_tbo='A'
450703160610     c                   setoff                                       12
450704160610     c                   endif
450800940408     C*
450900150423    4C**** *IN93         IFEQ      *OFF
451000940408     C*
451100940408     C                   EXSR      LEG2
451200940408     C*
451300150423   X4C****               ELSE
451400940408     C*
451500150423     C*                  Z-ADD     LBLAAN        PA0AAS
451600150423     C*                  Z-ADD     LBLLPN        PA0LNP
451700150423     C*                  Z-ADD     LBLNRN        PA0NRS
451800150423     C*                  Z-ADD     LBLNSN        PA0NSP
451900150423     C*                  MOVEL     rsut          PA0RSU
452000150423     C*                  MOVEL     SIMFEL        PA0SIM
452100150423     C*                  MOVEL     *blanks       PA0CML
452200150423     C*    PA1FLG        IFEQ      'N'
452300960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
452400960903     C*       verifichi questo caso
452500150423     C*                  MOVEL     'R'           PA0FLG
452600150423     C*                  ELSE
452700150423     C*                  MOVEL     'A'           PA0FLG
452800150423     C*                  END
452900150423     C*                  MOVEL     *BLANKS       PA0CLI
453000150423     C*                  CLEAR                   PA0F03
453100150423     C*                  MOVEL     *BLANKS       PA0GIA
453200150423     C*                  MOVEL     PARAM0        KPJBU
453300150423     C*                  CALL      'FNLS05R'
453400150423     C*                  PARM                    KPJBA
453500150423     C*                  MOVEL     KPJBU         PARAM0
453600150423     C*                  MOVEL     PA0F03        WF03
453700150423     C*                  MOVEL     'S'           WRIEM
453800150423    4C*                  ENDIF
453900940408     C*
454000940408    3C                   ENDIF
454100930413     C*
454200131107     C                   READC     LRI6S08                                93
454300940408    2C                   ENDDO
454400930413     C                   GOTO      FORC2
454500930413     C*
454600930413     C     SOTTO         TAG
454700930413     C*
454800940408    1C                   ELSE
454900940408     C*
455000940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
455100131107     C     2             CHAIN     LRI6S08                            99
455200940907     C     V8CAAP        IFEQ      V3CAAS
455300940907     C     V8CLPP        ANDEQ     V3CLNP
455400940907     C     V8CNRP        ANDEQ     V3CNRS
455500940907     C     V8CNSP        ANDEQ     V3CNSP
455600940907     C                   MOVEL     V8CAAN        ARBAAS
455700940907     C                   MOVEL     V8CLPN        ARBLNP
455800940907     C                   MOVEL     V8CNRN        ARBNRS
455900940907     C                   MOVEL     V8CNSN        ARBNSP
456000940907     C                   MOVEL     'S'           V8CFPS
456100940907     C                   ELSE
456200940907     C                   MOVEL     V8CAAP        ARBAAS
456300940907     C                   MOVEL     V8CLPP        ARBLNP
456400940907     C                   MOVEL     V8CNRP        ARBNRS
456500940907     C                   MOVEL     V8CNSP        ARBNSP
456600940907     C                   MOVEL     'P'           V8CFPS
456700940907     C                   ENDIF
456800940907     C* QUINDI VEDO SE LA TROVO FRA GLI ARRIVI
456900150423     C     KARB2         CHAIN     FNblp01L                           93
456901160610     c                   if        %found(fnblp01l)
456902160610     c                   eval      w_tbo='P'
456903160913     c                   eval      blp_arbdcm=arbdcm
456904161014     c                   eval      blp_arbhmc=arbhmc
456905160610     c                   seton                                        12
456906160610     c                   endif
457000940907     C     KARB2         CHAIN     FNARB01L                           93
457001160610     c                   if        %found(fnarb01l)
457002160610     c                   eval      w_tbo='A'
457003160610     c                   setoff                                       12
457004160610     c                   endif
457100940408     C*
457200150423    2C*    *IN93         IFEQ      *ON
457300150423     C*                  Z-ADD     ARBAAS        PA0AAS
457400150423     C*                  Z-ADD     ARBLNP        PA0LNP
457500150423     C*                  Z-ADD     ARBNRS        PA0NRS
457600150423     C*                  Z-ADD     ARBNSP        PA0NSP
457700150423     C*                  MOVEL     rsut          PA0RSU
457800150423     C*                  MOVEL     SIMFEL        PA0SIM
457900150423     C*                  MOVEL     *blanks       PA0CML
458000150423     C*    PA1FLG        IFEQ      'N'
458100960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
458200960903     C*       verifichi questo caso
458300150423     C*                  MOVEL     'R'           PA0FLG
458400150423     C*                  ELSE
458500150423     C*                  MOVEL     'A'           PA0FLG
458600150423     C*                  END
458700150423     C*                  MOVEL     *BLANKS       PA0CLI
458800150423     C*                  CLEAR                   PA0F03
458900150423     C*                  MOVEL     *BLANKS       PA0GIA
459000150423     C*                  MOVEL     PARAM0        KPJBU
459100150423     C*                  CALL      'FNLS05R'
459200150423     C*                  PARM                    KPJBA
459300150423     C*                  MOVEL     KPJBU         PARAM0
459400150423     C*                  MOVEL     PA0F03        WF03
459500150423     C*                  MOVEL     'S'           WRIEM
459600121116     c* reimposto la key spedizione
459700150423     C*                  MOVEL     NUM2          arbAAS
459800150423     C*                  MOVEL     COMLPN        arbLNP
459900150423     C*                  MOVEL     COMNRN        arbNRS
460000150423     C*                  MOVEL     COMNSN        arbNSP
460100930510     C*
460200150423   X2C*                  ELSE
460300940408     C*
460400940408     C                   EXSR      LEG2
460500940408     C*
460600150423    2C*                  END
460700940408    1C                   END
460701160610
460702160610     c                   eval      w_tbo=w_tbo_sav
460703160913     c                   eval      blp_arbdcm=blp_arbdcm_sav
460704161014     c                   eval      blp_arbhmc=blp_arbhmc_sav
460705160610     c                   eval      *in12=win12
460800940411     C*
460900940411     C* RICARICO I DATI DELLA BOLLA INIZIALE
461000940411     C     WCAR          IFEQ      'C'
461100940411     C                   SETOFF                                       75
461200940714     C                   MOVEL     NUM2          V2CAAS
461300940411     C                   MOVEL     COMLPN        COMLNP
461400940714     C                   MOVEL     COMNRN        V2CNRS
461500940714     C                   MOVEL     COMNSN        V2CNSP
461501160610     c     karb          chain     fnblp01l
461600940712     C     KARB          CHAIN     FNARB01L                           93
461700940411     C* RICARICO I DATI
461800940411     C                   EXSR      BOLLA
461900940411     C*
462000940411     C* RIABILITO IL CMD18
462100940411     C                   SETON                                        94
462200940411    1C                   END
462300940802     C*
462400930413     C                   ENDSR
462500940802     C**************************************************************************
462600940802     C*    LEGAMI BOLLA
462700940802     C**************************************************************************
462800940408     C     LEG2          BEGSR
462900940802     C*
463000940411     C                   MOVEL     'C'           WCAR              1
463100940408     C*
463200940408     C* 75 ON - NON RIENTRO NEL CARICAMENTO LEGAME BOLLA
463300940408     C                   SETON                                        75
463400940408     C* 94 OFF- DISABILITO IL CMD18
463500940408     C                   SETOFF                                       94
463600940408     C* CARICO DATI BOLLA
463700940408     C                   EXSR      BOLLA
463800940411     C*
463900940411     C* VISUALIZZA NUOVO LEGAME
464000940408     C* SE LA BOLLA E' UNA PRECEDENTE --> VISUALIZZO LA SEGUENTE
464100940713     C     V8CFPS        IFEQ      'P'
464200950316     C                   MOVEA     'SEGUEN.'     LBL(14)
464300940408     C                   ELSE
464400940408     C* SE LA BOLLA E' UNA SEGUENTE   --> VISUALIZZO LA PRECEDENTE
464500950316     C                   MOVEA     'PRECED.'     LBL(14)
464600940408     C                   ENDIF
464700950316     C                   MOVEA     COMLPN        LBL(21)
464800950316     C                   MOVEA     COMNRN        LBL(25)
464900950316     C                   MOVEA     COMNSN        LBL(28)
465000950316     C                   MOVEA     COMLAN        LBL(40)
465100950307     C                   MOVEA     *BLANKS       LBL(44)
465200940408     C                   MOVEA     LBL           LEGAME
465300940408     C*
465400940411     C* MOUVO I DATI DELLA LEGATA NEI CAMPI DELLA PARAM
465500940714     C                   MOVEL     ARBAAS        V2CAAS
465600940411     C                   MOVEL     ARBLNP        COMLNP
465700940714     C                   MOVEL     ARBNRS        V2CNRS
465800940714     C                   MOVEL     ARBNSP        V2CNSP
465900940408     C*
466000940408     C                   EXSR      VISUA2
466100940408     C*
466200941230     C                   MOVEL     'S'           WRIEM
466300940408     C                   ENDSR
466400940408     C*
466500940712      **************************************************************************
466600131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
466700940408      *     PER BOLLA NORMAL - CMD18 PREMUTO DA VISUAL -
466800091216      *     ricordarsi di aggiornare anche RIEPI2 per ogni modifica fatta qui
466900940712      **************************************************************************
467000940712     C     RIEPIL        BEGSR
467100940712      *
467200940712     C                   SETOFF                                       89
467300940712      *
467400940712      * EMETTO IL FORMATO VIDEO
467500940720     C                   MOVEL     'S'           WRIEM
467600940712     C     FOR07         TAG
467700940720     C     WRIEM         IFEQ      'S'
467800131107     C                   WRITE     LRI6T01
467900940720     C                   MOVEL     *BLANKS       WRIEM
468000940720     C                   ENDIF
468100021217      * Imposto la stringa comandi del 'giro' successivo
468200021217     C                   IF        *inKY = *on and $Loop > 1
468300021217     c                   Exsr      Ges_F24
468400021217     c                   eval      FD07 = FDxx
468500021217     c                   eval      F24D07 = F24Dxx
468600021217     C                   ENDIF
468700131107     C                   EXFMT     LRI6D07
468800070420     c                   setoff                                       28
468900070420     c                   clear                   v1cmsg
469000940712      *
469100131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
469200940712     C   KY*IN89         IFEQ      *ON
469300940712     C                   SETOFF                                       89
469400940712     C                   ELSE
469500940712     C                   SETON                                        89
469600940712     C                   ENDIF
469700940712     C   KY              GOTO      FOR07
469800940712      *
469900131108      * F3 - FINE LAVORO TORNO A SUBFILE
470000940714     C   KC              MOVEL     '2'           V2CFLG            1
470100940712     C   KC              GOTO      ENDRIE
470200940712      *
470300131108      * F18 - INTERROGA LEGAME BOLLA
470400940712     C   KS              DO
470500940712     C                   EXSR      CMD18
470600940712     C                   GOTO      FOR07
470700940712     C                   END
470800940712      *
470900940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
471000940712     C                   EXSR      COMAND
471100940905     C     WCMD          IFEQ      'S'
471200940802     C     WF03          IFEQ      *BLANKS
471300070404     c
471400940802     C                   GOTO      FOR07
471500940802     C                   ELSE
471600940802     C                   MOVEL     '2'           V2CFLG
471700940802     C                   GOTO      ENDRIE
471800070404     C                   ENDIF
471900091215     c
472000070404     c                   else
472100070404     c
472200070404     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
472300070404     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
472400070404     c                   if        v3cgesd='?'
472500131120
472600070404     c                   exsr      VISUALAbi
472700070404     c                   clear                   v3cgesd
472800070412     c                   eval      wriem='S'
472900070404     c                   endif
473000131120     c
473100131120     c* controlla se immessi dei '?'
473200131120     c                   eval      wvideo='R'
473300131120     c                   exsr      CTR_puIn
473400091215     c
473500091215     c                   if        wriem='S'
473600091215     C                   GOTO      FOR07
473700091215     c                   endif
473800070404     c
473900940712     C                   END
474000940712      *
474100940712     C     ENDRIE        ENDSR
474200940712      **************************************************************************
474300131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
474400940408      *     PER LEGAME BOLLA - CMD18 PREMUTO DA VISUA2 -
474500940712      **************************************************************************
474600940712     C     RIEPI2        BEGSR
474700940712      *
474800940712     C                   SETOFF                                       89
474900940712      *
475000940712      * EMETTO IL FORMATO VIDEO
475100940809     C                   MOVEL     'S'           WRIEM
475200940712     C     FOR77         TAG
475300940809     C     WRIEM         IFEQ      'S'
475400131107     C                   WRITE     LRI6T01
475500940809     C                   MOVEL     *BLANKS       WRIEM
475600940809     C                   ENDIF
475700091216      * Imposto la stringa comandi del 'giro' successivo
475800091216     C                   IF        *inKY = *on and $Loop > 1
475900091216     c                   Exsr      Ges_F24
476000091216     c                   eval      FD07 = FDxx
476100091216     c                   eval      F24D07 = F24Dxx
476200091216     C                   ENDIF
476300091216     c
476400131107     C                   EXFMT     LRI6D07
476500070516     c                   setoff                                       28
476600070516     c                   clear                   v1cmsg
476700940408     C*
476800131108      * F3 - FINE LAVORO TORNO A SUBFILE
476900940714     C   KC              MOVEL     '2'           V2CFLG            1
477000940408     C*
477100940408     C  NKC              DO
477200940712      *
477300131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
477400940712     C   KY*IN89         IFEQ      *ON
477500940712     C                   SETOFF                                       89
477600940712     C                   ELSE
477700940712     C                   SETON                                        89
477800940712     C                   ENDIF
477900940712     C   KY              GOTO      FOR77
478000940712      *
478100940712      *
478200131108      * F18 - INTERROGA LEGAME BOLLA
478300940712     C   KS              DO
478400940712     C                   EXSR      CMD18
478500940712     C                   GOTO      FOR77
478600940712     C                   END
478700940712      *
478800940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
478900940712     C                   EXSR      COMAND
479000940905     C     WCMD          IFEQ      'S'
479100940712     C                   GOTO      FOR77
479200091216     c
479300091216     c                   else
479400091216     c
479500091216     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
479600091216     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
479700091216     c                   if        v3cgesd='?'
479800131120
479900091216     c                   exsr      VISUALAbi
480000091216     c                   clear                   v3cgesd
480100091216     c                   eval      wriem='S'
480200091216     c                   endif
480300131120
480400131120     c* controlla se immessi dei '?'
480500131120     c                   eval      wvideo='R'
480600131120     c                   exsr      CTR_puIn
480700091216     c
480800091216     c                   if        wriem='S'
480900091216     C                   GOTO      FOR77
481000091216     c                   endif
481100091216     c
481200940712     C                   END
481300091216     C                   END
481400940712      *
481500940712     C                   ENDSR
481600940923      **************************************************************************
481700940923      *     CONTROLLI FORMATO VIDEO D04
481800940923      **************************************************************************
481900131120     C     CTR_puIn      BEGSR
482000110601     c*
482100110601     c* verifico se inserito '?' su email destinatario
482200110601     c                   if        v4cemw='?'
482300110601     c                   exsr      sr_emdest
482400110601     C                   MOVEL     'S'           WRIEM
482500110601     c                   endif
482600940923      *
482700091215      * Verifico se inserito '?' per visualizzazione beneficiario
482800091215     c                   if        vidben='?'
482900131120     c
483000091215     c                   exsr      sr_bencas
483100091215     C                   MOVEL     'S'           WRIEM
483200091215     c                   endif
483300131114     c
483400131114     c* Visualizzazione dati filiale
483500131114     c                   if        viddec_f='?'
483600131120
483700131114     c                   clear                   trulorfds
483800131114     c                   eval      iorffil=arblna
483900131114     c                   call      'TRULORFR'
484000131114     c                   parm                    kpjba
484100131114     c                   parm                    trulorfds
484200131114     c
484300131114     c                   clear                   viddec_f
484400131114     C                   MOVEL     'S'           WRIEM
484500131114     c                   endif
484600091215     c
484700940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
484800940923     C     V4IGCA        IFEQ      '?'
484900991027     C                   CLEAR                   DS7PQRS
485000941003     C                   MOVEL     'S'           DS7RIC
485100941003     C                   MOVEL     'V'           DS7GES
485200941003     C                   MOVEL     V4CGCA        DS7COD
485300991027     C                   MOVEL     DS7PQRS       KPJBU
485400960126     C                   CALL      'TRTB72R'
485500941003     C                   PARM                    KPJBA
485600940923     C                   MOVEL     *BLANKS       V4IGCA
485700940923     C                   MOVEL     'S'           WRIEM
485800940923     C                   ENDIF
485900940923     C*
486000940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
486100940923     C     V4IGGA        IFEQ      '?'
486200991027     C                   CLEAR                   DS7PQRS
486300941003     C                   MOVEL     'S'           DS7RIC
486400941003     C                   MOVEL     'V'           DS7GES
486500941003     C                   MOVEL     V4CGGA        DS7COD
486600991027     C                   MOVEL     DS7PQRS       KPJBU
486700960126     C                   CALL      'TRTB69R'
486800941003     C                   PARM                    KPJBA
486900940923     C                   MOVEL     *BLANKS       V4IGGA
487000940923     C                   MOVEL     'S'           WRIEM
487100940923     C                   ENDIF
487200940923     C*
487300940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
487400940923     C     V4IGMA        IFEQ      '?'
487500991027     C                   CLEAR                   DS7PQRS
487600941003     C                   MOVEL     'S'           DS7RIC
487700941003     C                   MOVEL     'V'           DS7GES
487800941003     C                   MOVEL     V4CGMA        DS7COD
487900991027     C                   MOVEL     DS7PQRS       KPJBU
488000960126     C                   CALL      'TRTB70R'
488100941003     C                   PARM                    KPJBA
488200940923     C                   MOVEL     *BLANKS       V4IGMA
488300940923     C                   MOVEL     'S'           WRIEM
488400940923     C                   ENDIF
488500940923     C*
488600940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
488700940923     C     V4IGVA        IFEQ      '?'
488800991027     C                   CLEAR                   DS7PQRS
488900941003     C                   MOVEL     'S'           DS7RIC
489000941003     C                   MOVEL     'V'           DS7GES
489100941003     C                   MOVEL     V4CGVA        DS7COD
489200991027     C                   MOVEL     DS7PQRS       KPJBU
489300960126     C                   CALL      'TRTB73R'
489400941003     C                   PARM                    KPJBA
489500940923     C                   MOVEL     *BLANKS       V4IGVA
489600940923     C                   MOVEL     'S'           WRIEM
489700940923     C                   ENDIF
489800940923      *
489900940923     C                   ENDSR
490000940321      **************************************************************************
490100940713      *     R O U T I N E      I N I Z I A L E
490200940713      **************************************************************************
490300940713     C     *INZSR        BEGSR
490400940713      *
490500940713     C     KARB          KLIST
490600940714     C                   KFLD                    V2CAAS
490700940713     C                   KFLD                    COMLNP            3 0
490800940714     C                   KFLD                    V2CNRS
490900940714     C                   KFLD                    V2CNSP
491000940713     C     KARB2         KLIST
491100940713     C                   KFLD                    ARBAAS
491200940713     C                   KFLD                    ARBLNP
491300940713     C                   KFLD                    ARBNRS
491400940713     C                   KFLD                    ARBNSP
491500940713     C*
491600940713     C     KTAB1         KLIST
491700940713     C                   KFLD                    CODUT
491800940713     C                   KFLD                    COD
491900940713     C                   KFLD                    KEY
492000940713     C     KTAB2         KLIST
492100940713     C                   KFLD                    CODUT
492200940713     C                   KFLD                    COD
492300940713     C*
492400940907     C     KLBL1         KLIST
492500940713     C                   KFLD                    LBLAAN
492600940713     C                   KFLD                    LBLLPN
492700940713     C                   KFLD                    LBLNRN
492800940713     C                   KFLD                    LBLNSN
492900940907     C     KLBL3         KLIST
493000940907     C                   KFLD                    KAAS
493100940907     C                   KFLD                    KLNP
493200940907     C                   KFLD                    KNRS
493300940907     C                   KFLD                    KNSP
493400940713     C*
493500940719     C     KAR41         KLIST
493600940719     C                   KFLD                    KAAS
493700940719     C                   KFLD                    KLNP
493800940719     C                   KFLD                    KNRS
493900940719     C                   KFLD                    KNSP
494000940802     C                   KFLD                    KTRC
494100951220     C     KAR42         KLIST
494200951220     C                   KFLD                    KAAS
494300951220     C                   KFLD                    KLNP
494400951220     C                   KFLD                    KNRS
494500951220     C                   KFLD                    KNSP
494600021213     C*
494700021213     C     KAR51         KLIST
494800021213     C                   KFLD                    KAAS
494900021213     C                   KFLD                    KLNP
495000021213     C                   KFLD                    KNRS
495100021213     C                   KFLD                    KNSP
495200021213     C                   KFLD                    KTRd              3
495300940719     C*
495400991026     C     KAR61         KLIST
495500940713     C                   KFLD                    KAAS
495600940713     C                   KFLD                    KLNP
495700940713     C                   KFLD                    KNRS
495800940713     C                   KFLD                    KNSP
495900991026     C                   KFLD                    KTRC
496000991026     C*
496100051128     C     Ksped         KLIST
496200991026     C                   KFLD                    KAAS
496300991026     C                   KFLD                    KLNP
496400991026     C                   KFLD                    KNRS
496500991026     C                   KFLD                    KNSP
496600160504     C*
496700990715     C     KGCA1         KLIST
496800941202     C                   KFLD                    KAAS
496900940713     C                   KFLD                    KLNP
497000940713     C                   KFLD                    KNRS
497100940713     C                   KFLD                    KNSP
497200940713     C*
497300990805     C     KEVB1         KLIST
497400940713     C                   KFLD                    KAAS$
497500940713     C                   KFLD                    KLNP
497600940713     C                   KFLD                    KNRS
497700940713     C                   KFLD                    KNSP
497800131114     C     Kcet          KLIST
497900140224     C                   KFLD                    pctfgs
498000131114     C                   KFLD                    pctNDC
498100131114     C                   KFLD                    arbpdc
498200131114     C                   KFLD                    TTRD              3
498300131114     C                   KFLD                    arbAAS
498400131114     C                   KFLD                    arbLNP
498500131114     C                   KFLD                    arbNRS
498600131114     C                   KFLD                    arbNSP
498700950324     C*
498800020906     C     KFVV1         KLIST
498900950324     C                   KFLD                    WNPG
499000950327     C                   KFLD                    WNDC
499100020906     c                   Kfld                    Wfgs
499200131119     C     KDST          KLIST
499300131119     C                   KFLD                    WNPG
499400131119     C                   KFLD                    kndc6
499500131119     c                   Kfld                    Wfgs
499600021203
499700021203     C     Kfapd         klist
499800021203     C                   kfld                    kpdtip
499900021203     C                   kfld                    kpdpdr
500000131108     c
500100131108     c     kbrv          klist
500200131108     C                   KFLD                    WNPG
500300131108     C                   KFLD                    knfv
500400131108     c                   Kfld                    Wfgs
500500131108     c                   Kfld                    artfls
500600131108     c                   Kfld                    artlna
500700131108     c                   Kfld                    artnrs
500800131108     c                   Kfld                    artnsc
500900040218      *
501000160408      * File FITGD32L
501100040218     c     K02TGD02      klist
501200040218     c                   kfld                    TGDtor
501300040218     c                   kfld                    TGDogg
501400030527     C*
501500150617
501600150617     C     KANM          KLIST
501700150617     C                   KFLD                    KAAS
501800150617     C                   KFLD                    KLNP
501900150617     C                   KFLD                    KNRS
502000150617     C                   KFLD                    KNSP
502100150617     C                   KFLD                    KCAA
502200940713     C*
502300940713     C*-------------------------------------------------------------------------
502400940713     C*
502500991222     C     *LIKE         DEFINE    ARBAAS        WAAS
502600991027     C     *LIKE         DEFINE    ARBLNP        WLNP
502700991027     C     *LIKE         DEFINE    ARBNRS        WNRS
502800991027     C     *LIKE         DEFINE    ARBNSP        WNSP
502900991027     C     *LIKE         DEFINE    FVVNPG        WNPG
503000991027     C     *LIKE         DEFINE    FVVNFV        WNDC
503100131108     C     *LIKE         DEFINE    brvNFV        knfv
503200991027     C     *LIKE         DEFINE    COMC08        SAVC08
503300971219     C     *LIKE         DEFINE    ARBAAS        KAAS
503400990805     C     *LIKE         DEFINE    EVBAAS        KAAS$
503500940713     C     *LIKE         DEFINE    ARBLNP        KLNP
503600940713     C     *LIKE         DEFINE    ARBNRS        KNRS
503700940713     C     *LIKE         DEFINE    ARBNSP        KNSP
503800961112     C     *LIKE         DEFINE    ARBFBR        WFBR
503900940713     C     *LIKE         DEFINE    TBLCOD        COD
504000940713     C     *LIKE         DEFINE    TBLKEY        KEY
504100940713     C     *LIKE         DEFINE    ARBAAS        $AAS
504200940713     C     *LIKE         DEFINE    ARBMGS        $MGS
504300940719     C     *LIKE         DEFINE    AR4TRC        KTRC
504400940907     C     *LIKE         DEFINE    LBLAAN        COMAAN
504500971223     C     *LIKE         DEFINE    AR4TRC        WTRC
504600060130     C*-------------------------------------------------------------------------
504700060130     c
504800060130      *
504900060130     c     *dtaara       define    §azute        azuteds
505000060130     c     *dtaara       define    §datiute      ddatiute
505100060130     c                   in(E)     *dtaara
505200060130    1c                   If        %error  or RSUT = *blanks
505300060130     c                   Clear                   Tibs34ds
505400060130     c                   Call      'TIBS34R'
505500060130     c                   Parm                    Tibs34ds
505600060130     c                   In        *dtaara
505700060130    1c                   EndIf
505800060130     c
505900971223     C*
506000971223     C* IMPOSTAZIONE CAMPI FISSI
506100940907     C                   MOVEL     *ZERO         COMLPN            3
506200940907     C                   MOVEL     *ZERO         COMNRN            2
506300940907     C                   MOVEL     *ZERO         COMNSN            7
506400950307     C                   MOVEL     *ZERO         COMLAN            3
506500950324     C                   Z-ADD     4             WNPG
506600940713      *
506700940713     C                   Z-ADD     *ZERO         NR2               4 0
506800940713     C                   Z-ADD     *ZERO         NR6               4 0
506900940713     C                   Z-ADD     *ZERO         NR8               4 0
507000940802     C                   MOVEL     *BLANKS       WF03              1
507100070403     c
507200070403     c* Verifico se profilo utente ha GEO attiva
507300131205     c* Per EDP sempre anche prr RAC999
507400131205     c                   if        %subst(knmus:1:3)='EDP' or
507500131205     c                             %subst(knmus:1:6)='RAC999'
507600070410     c                   eval      dutgeot='S'
507700070410     c                   else
507800070403     c     dutpou        chain     azorg01l
507900070403     c                   if        %found(azorg01l)
508000070403     c                   movel     orgde8        og148
508100070403     c                   else
508200070403     c                   clear                   og148
508300070403     c                   endif
508400070410     c
508500070524     c                   if        §oggeot<>' ' or §ogtlls<>' '
508600070524     c                   movel     'S'           dutgeot           1
508700070410     c                   endif
508800070524     c                   endif
508900131007
509000140319     c* Tasto funz orari servizi sempre abilitato
509100140319     c                   eval      wabif9='S'
509200131007
509300940713      *
509400940713     C                   ENDSR
509500971223     C**
509600110601      **************************************************************************
509700110601      *   emissione window email destinatario
509800110601      **************************************************************************
509900110601     C     sr_emdest     BEGSR
510000110601     c                   clear                   v4cemw
510100110601     c
510200110601     c                   eval      ilv79TLA='L'
510300110601     c                   eval      ilv79Aas=arbaas
510400110601     c                   eval      ilv79Lnp=arblnp
510500110601     c                   eval      ilv79nrs=arbnrs
510600110601     c                   eval      ilv79nsp=arbnsp
510700110601     c                   call      'FNLV79R'
510800110601     c                   parm                    fnlv79ds
510900110601     c
511000110601     c                   endsr
511100091214      **************************************************************************
511200131114      *   Emissione del Beneficiario C/Ass
511300091214      **************************************************************************
511400091214     C     sr_bencas     BEGSR
511500091215     c                   clear                   vidben
511600091214     c                   clear                   wdaben
511700091214     c                   clear                   wdasrsc
511800091214     c                   clear                   wdascdi
511900091216     c                   clear                   trulintds
512000091216     c
512100091216     c                   eval      iinttip='B'
512200091216     c                   eval      iinttic=vidtic
512300091216     c                   eval      iintrscm=v4crsm
512400091216     c                   eval      iintrsco=vidrmo
512500091216     c                   eval      iintCDI =w_cdi
512600091216     c                   call      'TRULINTR'
512700091216     c                   parm                    trulintds
512800091216     c
512900091216     c                   movel     ointrsci      wdasrsc
513000091216     c
513100091216     c                   select
513200091216     c                   when      ointtipi='O'
513300091216     c                   eval      wdaben='MITTENTE  ORIGINALE'
513400091216     c
513500091216     c                   when      ointtipi='R'
513600091216     c                   eval      wdascdi=w_cdi
513700091216     c                   eval      wdaben=' O R D I N A N T E'
513800091216     c
513900091216     c                   when      ointtipi='M'
514000091216     c                   if        v3cksc>0
514100091216     c                   movel     v3cksc        wdascdi
514200091216     c                   endif
514300091216     c                   eval      wdaben='  M I T T E N T E'
514400091216     c                   ENDSL
514500091214     c
514600131107     c                   exfmt     LRI6wdas
514700091214     c                   endsr
514800130830      **************************************************************************
514900130830      *   window per visualizzare gli orari servizi
515000130830      **************************************************************************
515100130830     C     sr_OrariSr    BEGSR
515200130920
515300140110     c                   clear                   trulorSds
515400140110     c                   eval       s_IOREtfp = ARBtfp
515500140110     c                   eval       s_IOREtfa = ARBtfa
515600140110     c                   eval       s_IOREdti = ARBdti
515700140110     c                   eval       s_IOREhti = ARBhti
515800140110     c                   eval       s_IOREdcr = ARBdcr
515900140110     c                   eval       s_IOREhcr = ARBhcr
516000140110     c                   eval       s_IOREtcr = ARBtcr
516100140110     c                   eval       s_IOREDEI = s_ARBdei
516200140110     c                   eval       s_IOREOEI = s_ARBoei
516300140110     c                   eval       s_IOREfil = ARBlna
516400140110     c                   eval       s_IOREcap = ARBcad
516500140110     c                   eval       s_IOREloc = ARBlod
516600140110     c                   eval       s_IOREnar = ARBnzd
516700150729     c                   clear                   diore01
516800150729     c                   movel     s_arbttc      §ioretrazC
516900150729     c                   movel     s_arbtcc      §ioreconsC
517000150729     c                   eval       s_IOREflo = diore01
517100140113     c* Data di Riferimento: data consegna se consegnata
517200140113     c                   select
517300140113     c                   when      arbdcm>0 and arbica<>' ' and arbica<>'R'
517400140113     c                             and arbicc<>' ' and arbicc<>'R'
517500150617     c                             and w_tbo='A' or (w_tbo='P' and arbdcm>0)
517600140113     c                   eval       s_ioredta=arbdcm
517700140113     c* per ritorno all'incasso data del giorno
517800140113     c                   when      arbdcm>0 and arbddc>0
517900140113     C                   time                    wrkoggiiso
518000140113     C     *iso          move      wrkoggiiso    s_ioredta
518100140113     c                   other
518200140113     c* altrimenti data prevista consegna se > = oggi
518300140113     C                   time                    wrkoggiiso
518400140113     C     *iso          move      wrkoggiiso    s_ioredta
518500140113     c* se < di oggi, imposto oggi
518600140113     c                   if        amgdce>s_ioredta
518700140113     c                   eval       s_ioredta=amgdce
518800140113     c                   endif
518900140113     c                   endsl
519000140113     c
519100140110     c                   eval       s_IOREtser = 'C'
519200140409     c                   eval       s_IOREDDC=arbddc
519300140409     c                   eval       s_IOREnDC=arbndc
519400140409     c
519500150617     c****               IF         *in19 or
519600150617     c                   if          w_tbo='A'
519700150316     c                   if          ARBfbc = *blanks and
519800140110     c                               ARBddc > 0 and
519900140110     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
520000150316     c                               arbica='R' or arbicc=' ' or arbicc='R')
520100140110     c                   eval       s_IOREcons = 'S'
520200140225     c* Orario presunto ritiro/consegna
520300140225     c                   move      arbndc        pctndc
520400140225     c                   move      arbifp        pctfgs
520500140225     c                   move      'COR'         ttrd
520600140225     c     kcet          setgt     fipct02l
520700140225     c     kcet          readpe    fipct02l
520800140310     c                   if        not %eof(fipct02l)
520900140225     c                   movel     pctdati       fipctcords
521000140307     c                   eval      s_ioreorp=§PCTORASTI
521100140225     c                   endif
521200140225     c
521300140110     c                   ENDIF
521400150617     c                   ENDIF
521500140225     c
521600140110     c                   eval       s_IOREtsp = ARBtsp
521700130920
521800140325     c                   eval      s_iorecol=36
521900150611     c                   if        W_video='N'
522000150611     c* località normalizzata
522100150611     C                   MOVEL     ARBAAS        KAAS
522200150611     C                   MOVEL     ARBLNP        KLNP
522300150611     C                   MOVEL     ARBNRS        KNRS
522400150611     C                   MOVEL     ARBNSP        KNSP
522500150611     C                   MOVEL     'X'           KTRC
522600150611     c     kar41         chain     fiar401l
522700150611     c                   if        %found(fiar401l)
522800150611     c                   eval      OOR3LOC_N=ar4not
522900150611     c                   else
523000150611     c                   eval      OOR3LOC_N=arblod
523100150611     c                   endif
523200150611     c
523300150611     c                   call      'TRULORSR'
523400150611     c                   parm                    kpjba
523500150611     c                   parm                    trulorSds
523600150611     c                   parm                    trulor2ds
523700150611     c                   parm                    trulor3ds
523800150611     c                   else
523900140110     c                   call      'TRULORSR'
524000130920     c                   parm                    kpjba
524100140110     c                   parm                    trulorSds
524200150611     c                   endif
524300130830     c
524400131111     c                   ENDSR
524500131111
524600940713      **************************************************************************
524700941014**        12345678901234567890123456789012345678901234567890
524800950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
524900930413**
525000950316LEGATA A PIU' BOLLE                       - F18=Visualizza
525100920326**
525200950316 Non Presente
525300950316 F18=Visualizza
525400940907Vedi in Int.Par
525500940907Vedi in Int.Arr
525600941011Vedi Arch.Stor.
525700940907**
525800940907Spedizione non Presente
525900941230
526000940907Vedi in Int.Bolle Part.
526100940907Vedi in Int.Bolle Arr.
526200941011Vedi in Archiv. Storico
526300940713**  MESSAGGI DI ERRORE VIDEO  **************************************************
526400160505Stampa LDV effettuata ...                                                     01
526500160505Stampa LDV al momento non effettuabile: ritentare o telefonare in Sede        02
526600160505Assegnato non tassato: stampa LdV non effettuabile                            03
526700070426**  cmA4f6
526800111221OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
526900111221USRDTA('Rist.Bolle') SECURE(*YES)
527000070426**  cmA5f6
527100070426OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
527200111221             USRDTA('Rist.Bolle') SECURE(*YES)
527300040218** TF                                                                        *
527400070420F1=Agg.Rich#                                                                    1
527500070411** TF01B                                                                     *
527600070411F1=SCELTA BOLLA#                                                                1
527700021216** TF                                                                        *
527800021218F2=C.A.#                                                                        2
527900021216** TF                                                                        *
528000021216F4=Tass2#                                                                       4
528100021216** TF                                                                        *
528200021216F5=Eventi#                                                                      5
528300070420** TF                                                                        *
528400070420F6=StampaLDV#                                                                   5
528500021216** TF                                                                        *
528600021217F7=Anomalie#                                                                    7
528700021216** TF                                                                        *
528800160628F8=Orm#                                                                         8
528900130830** TF                                                                        *
529000130830F9=OrariSr#                                                                     9
529100021216** TF                                                                        *
529200070420F10=Variaz#                                                                     10
529300021216** TF                                                                        *
529400030121F11=NoteS/B#                                                                    11
529500021217** TF                                                                        *
529600021217F12=Ritorno#                                                                    11
529700021216** TF                                                                        *
529800070420F13=Tassaz#                                                                     13
529900021217** TF                                                                        *
530000021216F14=Giacenza#                                                                   14
530100040218** TF                                                                        *
530200070420F16=Gest.Rich#                                                                  16
530300070404** TF                                                                        *
530400140311F17=NoteAut#                                                                    16
530500021216** TF                                                                        *
530600021216F19=Spunte#                                                                     19
530700021216** TF                                                                        *
530800070420F20=Affid#                                                                      19
530900021216** TF                                                                        *
531000070420F21=DetColli#                                                                   19
531100110928** TF                                                                        *
531200110930F22=PerfectOrder#                                                               19
531300160628** TF                                                                        *
531400160628F23=Discord#                                                                    19
