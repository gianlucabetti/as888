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
004701161207     F*.......... O R M
004702161207     Ffnorm01l  IF   E           K DISK
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
008100160628     D TF23            S              1    DIM(20) CTDATA PERRCD(20)
008200021217     D $TF             S              1    DIM(20)
008300911024     D*
008400930705     D KCA             S              1    DIM(50)
008500160610     D DCA             S             57    DIM(50)
008600940907     D*
008700940907     D AAN             S              4  0 DIM(30)
008800940907     D LPN             S              3  0 DIM(30)
008900940907     D NRN             S              2  0 DIM(30)
009000940907     D NSN             S              7  0 DIM(30)
009100941024     D CC1             S              1    DIM(99)
009200941024     D CC2             S             20    DIM(99)
009300020329     D CC3             S              1    DIM(99)
009400020329     D CB1             S              1    DIM(99)
009500020329     D CB2             S             20    DIM(99)
009600030121     D RigaTf1         S              1    DIM(62)
009700030121     D RigaTf2         S              1    DIM(62)
009800030121     D RigaTf3         S              1    DIM(62)
009900030527     D*
010000160505     d* PARAMETRI RICEVUTI IN ENTRATA
010100160606     D PARAMU6ds1      DS
010200160523     d** INPUT
010300940714     D  V2CAAS                14     17  0
010400940712     D  COMLNP                18     20  0
010500940714     D  V2CNRS                21     22  0
010600940714     D  V2CNSP                23     29  0
010700160523     d** OUTPUT
010800160504     D  PARF03                30     30
010900160523     d** INPUT
011000160504     D  PARFLG                31     31
011100160505     D*    SE RICH DA PGM GIAC. = 1
011200160504     D  PARGIA               144    144
011300160523     d** OUTPUT
011400160504     d  parrma               149    163                                         x F22=PerfectOrder
011500160504     d  parxco               164    164                                         x F22=PerfectOrder
011600160523     d** INPUT
011700160606     d*   Tipo lancio: "C" -           CHIUSO CON LR
011800160606     d*                "L" - ELABORO E CHIUDO CON LR
011900160606     d*                " " - ELABORO E CHIUDO IN RETRN
012000160523     d  partla               165    165
012001161124     d** OUTPUT - Codice di errore 1=Sped. non trovata
012100161124     d  parerr               166    166
012101161124     d  parflo               167    256
012200160606     d*******************************************
012300160527     D PARAMU6ds       DS
012400160527     D  V2CAAS_               14     17  0
012500160527     D  COMLNP_               18     20  0
012600160527     D  V2CNRS_               21     22  0
012700160527     D  V2CNSP_               23     29  0
012800160527     D  PARF03_               30     30
012900160527     D  PARFLG_               31     31
013000160527     D  PARGIA_              144    144
013100160527     d  parrma_              149    163
013200160527     d  parxco_              164    164
013300160527     d  parflo_              165    256
013400160504     D* PASSAGGIO A INTERROGAZIONE VARIAZIONI           - FNLR75R -
013500161019     D* PASSAGGIO A INTERROGAZIONE DETTAGLIO COLLI      - FNLRU38R -
013600160504     D PARAMint        DS
013700160504     D  pa1AAS                14     17  0
013800160504     D  pa1LNP                18     20  0
013900160504     D  pa1NRS                21     22  0
014000160504     D  pa1NSP                23     29  0
014100160504     D  PA1F03                30     30
014200160504     D  PA1FLG                31     31
014300160504     D  PA1PGM                32     41
014400160505     D  PA1DSA                42     61
014500160505     D  PA1DSP                63     70  0
014600160505     D  PA1RMN                71     85  0
014700160505     D  PA1LNA                86     88  0
014800160505     D  PA1LN1                89    108
014900160505     D  PA1RSD               109    143
015000160504     D* SE RICH DA PGM GIAC. = 1
015100160504     D  PA1GIA               144    144
015200160505     D  PA1FEL               145    147  0
015300160505     D  PA1TBO               148    148
015400160916     D  PA1TFP               156    158  0
015500160916     D  PA1TFA               159    161  0
015600160628     D*VISUALIZZAZIONE ORM - FIOR07R
015700160628     D Parm01          DS
015800160628     D  ppoe                          3  0
015900160628     D  pnor                          7  0
016000160628     D  pnsr                          2  0
016100160628     D  pnrv                          2  0
016200160628     D  psce                          1
016300160628     D  pfgs                          3  0
016400160628     D  ppor                          3  0
016500160628     D  pdtr                          8  0
016600160628     D  pmdb                         10
016700160628     D  pprb                         10
016800160628     D  pdts                          8  0
016900160628     D  prmp                          1
017000160628     D  pbrc                          1
017100160628     D  pref                          2
017200160628     D  pmio                          1
017300940802     D* RICHIAMO PGM DI INTERROGAZIONE SPUNTE FOGLI VARI  - FNLR04R -
017400160504     D PARA04          DS                  INZ
017500940713     D  PA4NFV                 1      5  0
017600940713     D  PA4LNP                 6      8  0
017700940713     D  PA4LNA                 9     11  0
017800940713     D  PA4NRS                12     13  0
017900940713     D  PA4NCD                14     20  0
018000940713     D  PA4NCA                21     27  0
018100940713     D  PA4NPS                28     29  0
018200940713     D  PA4NPG                30     30  0
018300940713     D  PA4FLG                31     31
018400940713     D  PA4PGM                32     38
018500941230     D  PA4FGS                39     41  0
018600101201     d* bolla
018700101201     d  pa4bolp               42     44  0
018800101201     d  pa4bors               45     46  0
018900101201     d  pa4bosp               47     53  0
019000101201     d  pa4boan               54     57  0
019100101201     D  PA4ERR                58     58
019200940726     D* RICHIAMO PGM DI INTERROGAZIONE EVENTI             - FNLRB5R -
019300950215     D PARAB5          DS                  INZ
019400940726     D  PB5AAS                14     17  0
019500940726     D  PB5LNP                18     20  0
019600940726     D  PB5NRS                21     22  0
019700940726     D  PB5NSP                23     29  0
019800940726     D  PB5F03                30     30
019900940726     D  PB5FLG                31     31
020000940726     D  PB5PGM                32     41
020100940726     D  PB5DSA                42     61
020200940726     D  PB5DSP                63     70  0
020300940726     D  PB5RMN                71     85  0
020400940726     D  PB5LNA                86     88  0
020500940802     D  PB5LN1                89    108
020600940802     D  PB5RSD               109    143
020700940802     D  PB5DCP               144    151  0
020800940802     D  PB5TMC               152    152
020900131111     D
021000950428     D                 DS
021100950428     D  V3CLNP                 1      3  0
021200950428     D  V3CNRS                 4      5  0
021300950428     D  V3CNSP                 6     12  0
021400950428     D  V3CDSP                13     20  0
021500950428     D  VIDRMN                21     35  0
021600131121     D  V3CLNA                36     38  0
021700950428     D  D15LNA                39     53
021800950428     D  V32RSD                54     85
021900950428     D  COMC08                 1     85
022000131111     d
022100930705     D WLBDAT          DS
022200940712     D  G02DAT                 1      8  0
022300940712     D  G02INV                 9     16  0
022400940712     D  G02ERR                17     17
022500940712     D  G02TGI                18     22  0
022600950125     D                 DS
022700950125     D* CAMPO UNICO COMPRENDENTE RAG.SOC.+AGGIUNTIVI 35 CARATTERI
022800950125     D  V4CRSD                 1     35
022900131120     D  V4CNOT                36     70
023000131120     D  V4CNOM                 1     70
023100040218      *?definizione oggetto reclamo come spedizione?
023200131121     d ds_OggSped      ds                  inz
023300131121     d  wARBlnp                       3s 0 inz
023400131121     d  wARBnrs                       2s 0 inz
023500131121     d  wARBnsp                       7s 0 inz
023600131121     d  wARBaas                       4s 0 inz
023700040218      *
023800040218     d Status         sds
023900040218     d  SDSprm           *parms
024000991027      *
024100040216     D KPJBA         E DS
024200150402     d fnlru6ds      e ds
024300040225      *
024400040225     d FIDNA2ds      e ds                  inz
024500040225     d  IA2mod       e                     inz('M')
024600040225     d  IA2tor       e                     inz('S')
024700040216      *
024800991029     d YEURCODS      E DS
024900991027     d TIBS69DS      E DS                  INZ
025000021213     d FNLRB3Ds      E DS
025100991027     d DS_cnaco      E DS                  extname(CNACO00F)
025200991027     d DS_cnind      E DS                  extname(CNIND00F)
025300991027     d DS_cnclp      E DS                  extname(CNCLP00F)
025400991027     d DS_fncls      E DS                  extname(FNCLS00F)
025500160825     d DS_fnarb      E DS                  extname(FNARB00F)
025600991027     D DSCV          E DS
025700020402     D DSCC          E DS
025800991027     D DS$2          E DS
025900991027     D DS3A          E DS
026000951220     D DS7K          E DS
026100160610     D DS7O          E DS
026200131108     D DS2A          E DS
026300020627
026400090604     d ds5e          e ds
026500020627
026600991217     D OG143         E DS
026700070403     D OG148         E DS
026800000615     D DSBL4A        E DS
026900060518     D DSBL4I        E DS
027000150729     D Diore01       E DS                  INZ
027100150729
027200991027     D FIDN00DS      E DS
027300091216     D TRULINTDS     E DS
027400991027     D DS7PQRS       E DS
027500991027     D FNLG30DS      E DS
027600991027     D FNLr44DS      E DS
027700130920
027800131114     d fipctcetds    e ds
027900140224     d fipctcords    e ds
028000150611     d fnlru3ds      E DS
028100140110     D TRULORSDS     E DS                  prefix(S_)
028200150611     D TRULOR2DS     E DS
028300150611     D TRULOR3DS     E DS
028400131114     D TRULORFDS     E DS
028500130920
028600961121     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
028700991027     D FNLR45DS      E DS
028800950102     D FNLR97        E DS
028900930428     D*
029000930428     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
029100991027     D TNSD99DS      E DS                  INZ
029200941014      *
029300030612     D* DS PER TRUL90R - RICHIESTA STAMPANTI
029400030612     D TRUL90DS      E DS
029500060224      *
029600060224     D* DS PER FNLv55R - richiamo pgm che cerca i terminal per £6
029700060224     D fnlv55ds      E DS
029800941014      *
029900051124     D* DS PER FNLV80R - decodifica cons richiesta
030000051124     D FNLV80DS      E DS
030100131120     D* DS PER FNLRG7R - visulizza altre abilitazioni
030200131120     D fnlrg7ds      E DS
030300140123     D* DS PER FNLRN6R - visulizza note LDV
030400140123     D fnlrn6ds      E DS
030500070427     D* DS PER FIDG34R - reperisce dati consegna automatica
030600070427     D fidg34ds      E DS
030700070420     D* DS PER           stampa LDV
030800070420     D FNLSB5DS      E DS
030900130308     d FNLSB6ds1     e ds                  inz
031000130319     d  DB6pdf       e                     inz('X')
031100941212      *
031200131119     D DDSTFLR       E DS                  INZ
031300010510     D Dsbl4m        E DS
031400030121     d dTba          e ds
031500031119     d dAr5Gen       e ds
031600030121     d dAr5Ban       e ds
031700030703     d dAr5Bnb       e ds
031800130927     d dAr5emd       e ds
031900030121     d Tibs02Ds      e ds
032000060130     d Azuteds       e ds                  extname(Azute00f)
032100060130     d dDatiute      e ds
032200060130     D TIBS34DS      E DS
032300110601     D fnlv79ds      E DS
032400150615     D fnlrj6ds      E DS
032500040809      * ds per il controllo della presenza di CA per la spedizione richiesta
032600040809     d FIDN12DS      e ds
032700040809     d  skKey                 26    305    dim(20)
032800040809     d  skAnn                306    325    dim(20)
032900040809     d  skDca                326    485  0 dim(20)
033000040809     d  skFca                486    545  0 dim(20)
033100040809     d  skDch                546    705  0 dim(20)
033200040809     d  skCch                706    745    dim(20)
033300040809     d*
033400040809     d dsKey           ds
033500040809     d  dsaac                         4  0
033600040809     d  dsfil                         3  0
033700040809     d  dsnca                         7  0
033800040809
033900991027      *
034000991027     D W017A           s             17
034100991027     D w0130           s             13  0
034200991027     D w0133           s             13  3
034300991027     D IMPeditato      s             13
034400991027     D Wsvn            S                   LIKE(AR7svn)
034500991027     D Wvan            S                   LIKE(AR7van)
034600991027     D Wdiv            S                   LIKE(AR6div)
034700991029     D Wariimp         S                   LIKE(ARIimp)
034800991029     D Wariipp         S                   LIKE(ARIipp)
034900020906     d wfgs            s                   Like(Fvvfgs)
035000991027     D DATA_eur        S               D   DATFMT(*eur)
035100021203     d kpdpdr          S                   LIKE(APDpdr)
035200021203     d kpdtip          S                   LIKE(APDtip)
035201161122     d apdrsc_w        S                   LIKE(APDrsc)
035300131120     d kndc6           S                   LIKE(dstnfv)
035400140110     d s_arbdei        S                   LIKE(d98dei)
035500140110     d s_arboei        S                   LIKE(d98oei)
035600150729     d s_arbttc        S                   LIKE(d98ttc)
035700150729     d s_arbtcc        S                   LIKE(d98tcc)
035800131114     D arbdsp          S              8  0
035900131111     D W0080           S              8  0
036000991027     D W0040           S              4  0
036100991027     D wtibs69r        s              1
036200991027     D wtstdec         s              1
036300131120     D Wvideo          s              1
036400020225     d lnantw          s                   like(§ogntw)
036500020225     d lnpntw          s                   like(§ogntw)
036600030703
036700030703     d wtba            s                   Like(§Ar5Tba)
036800030703     d wdestba         s                   Like(VidTba)
036900020628
037000091216     D W_cdi           S                   LIKE(§ar5cdi)
037100070420     D wrichtrul90     S              1
037200070403     d Dataeur         s               d   datfmt(*eur)
037300070403     d Datadmy         s               d   datfmt(*dmy)
037400110929     d no_f22          s              1    inz(' ')
037500131108     d Wcol_D          s              5  0
037600131108     d Wcol_I          s              5  0
037700131108     d Wcarat          s              3  0
037800131108     d Indx            s              3  0
037900131209     d wabif9          s              1
038000141010     d $Fine           s              1    inz(*off)
038100160610     d win12           s              1    inz(*off)
038200150611     d w_tbo           s              1
038300160610     d w_tbo_sav       s                   like(w_tbo)
038400160913     d blp_arbdcm      s                   like(arbdcm)
038500160913     d blp_arbdcm_sav  s                   like(arbdcm)
038600161014     d blp_arbhmc      s                   like(arbhmc)
038700161014     d blp_arbhmc_sav  s                   like(arbhmc)
038800160614     d w08dcm          s                   like(v08dcm)
038900160614     d w_evbcev        s                   like(evbcev)
039000161108     d wcev            s                   like(evbcev)
039100161107     d w_tcev          s                   like(ILRU3ITCEV)
039200150611     d w_video         s              1
039300150616     d w_ar4_1         s              1
039400150611     d pos             s              3  0
039500150612     d PosA            s              3  0
039600150612     d Lennt1          s              3  0
039700150612     d Lennt1v         s              3  0
039800150617     d kcaa            s                   like(anmcaa) inz(10)
039900160610     d w_ccaleg        s              1
040000161014     d w_orig          s              1
040100161027     d StatoCa         s              1
040200030527     D*
040300030527     D wrkoggiiso      s               d
040400991027      *
040500930705     D* DEFINIZIONI COSTANTI
040600160504     D*
040700930705     D CNOSEQ          C                   CONST('NON SEQUENZIALI')
040800941014     D CINTER          C                   CONST('* INTERROGAZIONE BOL-
040900150409     D                                     LA FILIALE *')
041000021216     D CF2413          C                   CONST('F24=AlTasti(1/3)')
041100021216     D CF2423          C                   CONST('F24=AlTasti(2/3)')
041200021216     D CF2433          C                   CONST('F24=AlTasti(3/3)')
041300021216     D CF2412          C                   CONST('F24=AlTasti(1/2)')
041400021216     D CF2422          C                   CONST('F24=AlTasti(2/2)')
041500150612     D CharOk          C                   const('QWERTYUIOPASDFGHJKLZXCVBNM-
041600150612     D                                     qwertyuiopasdfghjklzxcvbnm-
041700150612     D                                     1234567890.:-_/\()!£$%&+*§-
041800150612     D                                     <>''"èéòàìù')
041900150612     D mi              C                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto inglese
042000150612     D ma              C                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO INGLESE
042100110705     d
042200110705     D* CAMPO PER DSPATR "NO DISPLAY"
042300110705     D NODI            C                   CONST(X'27')
042400131120     D* CAMPO PER DSPATR "RI"
042500131120     D RI              C                   CONST(X'21')
042600131127     D* CAMPO PER DSPATR "HI"
042700131127     D hi              C                   CONST(X'22')
042800161108     D red             C                   CONST(X'28')
042900150618     d c_DspAtrNorm    c                   const(x'20')
043000900518     C****************************************************************
043100900518     C*  RIEPILOGO INDICATORI
043200900518     C***************************************************************
043300911105     C* 03    - SPEDIZIONE GIACENTE
043400990119     C* 04    - ABILITO TASTO FUNZIONALE F2 - INT C.A.
043500070411     c* 06    - on per tassazione 2 bolla emette scritta "ImportoBOLLO"
043600070411     c*       - offper tassazione 2 bolla emette scritta "Importo IVA"
043700000216     C* 08    - bolla dpd
043800961111     C* 10    - BOLLA DI RIENTRO
043900160615     C* 12    - ON=La spedizione è presente solo in BLP
044000961112     C* 16    - SI TRATTA DI BOLLA EXPORT
044100070403     C* 17    - p.o. utente e lna bolla hanno GEO attiva
044200160628     C* 18    - Abilita tasto funzionale F8=ORM
044300161017     c* 19    - Condizionamento RI su data/ora consegna
044400991027     C* 22    - ESISTE ESTENSIONE BOLLA 2 e ABILITO IL TASTO  F4=TASSAZIONE 2
044500950825     C* 23    - SFLEND
044600950825     C* 24    - ABILITO IL TASTO "F13=TASSAZIONE"
044700070404     C* 26    - presente FIARP
044800070403     C* 27    - presente blocco consegna ARBFBC<>' '
044900941028     C* 28    - VISUALIZZA MSG ERRORE
045000911129     C* 29    - ESISTE CONTRASSEGNO
045100911023     C* 30/31 - LETTURA COMODO
045200911023     C* 33    - BOLLA IN FRANCO
045300911023     C* 34    - LETTURA SUBFILE
045400911023     C* 35    - PULIZIA SUBFILE
045500911129     C* 36    - LETTURE
045600991027     C* 38    - ESISTE TB2 ANCHE SE NON PRESENTE TIPO RECORD '2' IN FIAR600F
045700160615     C* 41-59 - ERRORI
045800131111     C* 60    - email destiantario
045900131111     C* 61    - PROGRAMMA RICHIAMATO per scelta bolla
046000150616     C* 62    - Spedizione Da Partire
046100040218     C* 63    - Abilita F1  = Aggiunta in reclamo
046200040218     C* 64    - Abilita F16 = Gestione reclamo
046300091214     c* 65    - Abilita visualizzazione beneficiario C/Ass
046400150616     C* 66    - "Da partire" in RI se arrivato almeno un collo
046500960903     C* 67    - ABILITAZIONE TASTO DI COMANDO F7=ANOMALIE
046600131127     C* 68    - pos cursore RMN prima serv x  EMETTERE F23 PER CALCOLARE E VISUALIZZARE DATA CONS.
046700930505     C*         PREVISTA E LE ORE DI RITARDO
046800930526     C* 69    - DATA CONSEGNA MERCE >< 0
046900930510     C* 70    - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
047000160628     C* 71    - ABILITAZIONE TASTO DI COMANDO F23=EVENTI SEGNACOLLI
047100131107     C* 72    - SFLDSPCTL  LRI6S05
047200991027     C* 73    - BOLLA FRANCO FRONTIERA
047300970227     C* 74    - ABILITA EVENTI SEGNACOLLI
047400970227     C* 75    - UTILIZZATO PER NON ESEGUIRE LA SUBROUTINE "LEGBOL"
047500970227     C* 76    - SI DDT IN ALTA INTENSITA'
047600021213     C* 77    - NOTE SUPERMERCATI
047700970227     C* 78    - SFLDSP
047800950825     C* 79    - SFLDSPCTL
047900930510     C* 80    - NEL SFL DEI LEGAMI NON EMETTO IL CAMPO PER LA SELEZIONE
048000941028     C* 81    - USATO NEL VIDEO LEGAMI
048100941028     C* 82    - USATO NEL VIDEO LEGAMI
048200941028     C* 83    - USATO NEL VIDEO LEGAMI
048300160614     C* 84    - Bolla con figlia e tipo legame cca <> ' '
048400971219     C* 85    - VISUALIZZO RIFERIMENTO PARTNER ESTERO
048500111005     c* 86    - Abilitazione F22="PERFECT ORDER"
048600111005     C* 87    - Pgm richiamato in modalità "W" (Perfect Order Wurth)
048700110705     C* 88    -
048800931112     C* 89    - UTILIZZATO PER VISUALIZZARE ALTRI TASTI FUNZIONALI
048900920402     C* 90    - INDICA LA PRESENZA DI UN ERRORE
049000941028     C* 91    - ERRORE QCMDEXC (UTILIZZABILE)
049100940923     C* 92/94 - LEGAME BOLLA
049200930525     C* 95    - ESISTONO EVENTI DA INTERROGARE
049300160615     C* 96    - ON Data arrivo merce = 0
049400160614     C* 97    - Bolla "Da Partire"
049500941014     C* 98    - ESISTONO VARIAZIONI BOLLE EFFETTUATE
049600930504     C* 99    - OFF  ESISTE LEGAME BOLLA SEGUENTE
049700900518     C*****************************************************************
049800000000     C     *ENTRY        PLIST
049900000000     C                   PARM                    KPJBA
050000150402     c                   parm                    FNLRU6DS
050100131210     c
050200980609     C**
050300160606     C                   MOVEL     KPJBU         PARAMu6ds1
050301161124     c                   clear                   parerr
050302161124     c                   clear                   parf03
050400940713     C*
050500160505     c                   if        partla<>'C'
050600160505
050700060130     C                   Z-ADD     1             CODUT             1 0
050800060130     c                   Movel     rsut          V1cdsa
050900941227     C*
051000940713     C*-------------------------------------------------------------------------
051100911024     C                   EXSR      CARTAB
051200150410     c                   clear                   v2cflg
051300110930     c*
051400111005     c                   setoff                                       87
051500150312     c* W=Perfect order WURTH: attivo scritta "PERFECT ORDER" a video e
051600150312     c*   disattivo F22=Perfect Order per evitare errore di chiamata ricorsiva
051700160504     c                   if        parflg='W'
051800111005     c                   eval      *in87=*on
051900150312     c                   eval      no_f22='N'
052000110930     c                   endif
052100150312
052200941014     C* TESTATA
052300941014     C                   MOVEL     CINTER        V1CTES
052400131127     c                   clear                   v1clna
052500940317      *
052600930702     C*
052700940809     C                   MOVEL     'S'           WRIEM             1
052800930702     C**
052900930702     C* PGM RICHIAMATO
053000920326     C**
053100160504     C** PARFLG = " " - RICHIAMO "Normale"
053200160504     c*      (  =  1  - Richiamo "Normale" (SOLO PER I VECCHI PGM)  )
053300160504     C**        =  2  - RICHIAMATO PER   SELEZ BOLLA
053400160504     C**        =  C  - DISABIL.F2=C.A
053500160504     C**        =  N  - DISABIL.F7=ANOMA
053600160505     C**        =  P  - RICHIAMATO DA INT-BOLL PART E DISABILITAZ F7=ANOMALIE
053700160628     C**        =  E  - DISABIL.F23=EV.SEGNACOLLI
053800160504     C**        =  W  - RICHIAMATO per "PerfectOrder"
053900960903     C*
054000040218      *?Abilito F1=Aggiunta in Reclami?
054100040218      * (se richiamato da pgm. FIDNA1R...)
054200040220     c                   eval      *in63  = *off
054300150402     c                   if            SDSprm >  1  and ilru6ra='S'
054400150402     c                             and ELRU6ins = ' '
054500040220     c                   eval      *in63  = *on
054600111003     c                   endif
054700960903     C* ABILITO F7=ANOMALIE SE FLAG DIVERSO DA 'N' E 'P'
054800160504     C     PArFLG        IFNE      'N'
054900160504     C     PArFLG        ANDNE     'P'
055000960903     C                   SETON                                        67
055100040217     C                   ENDIF
055200160628     C* ABILITO F23=EVENTI SEGNACOLLI  SE FLAG DIVERSO DA 'E'
055300160504     C     PArFLG        IFNE      'E'
055400961121     C                   SETON                                        74
055500040217     C                   ENDIF
055600150409
055700160504     C     PArFLG        ifeq      '2'
055800150409     C                   SETON                                        61
055900150409     c                   endif
056000960903     C*
056100911025     C**
056200160825     c                   clear                   ds_fnarb
056300150313     c* chaino la spedizione in partenza e in arrivo
056400150611     c                   clear                   w_tbo
056500160913     c                   clear                   blp_arbdcm
056600161014     c                   clear                   blp_arbhmc
056700150313     c     karb          chain     fnblp01l
056800150611     c                   if        %found(fnblp01l)
056900150611     c                   eval      w_tbo='P'
057000160913     c                   eval      blp_arbdcm=arbdcm
057100161014     c                   eval      blp_arbhmc=arbhmc
057200150618     c                   seton                                        12
057300150611     c                   endif
057400150312     c     karb          chain     fnarb01l
057500150611     c                   if        %found(fnarb01l)
057600150611     c                   eval      w_tbo='A'
057700150618     c                   setoff                                       12
057800150611     c                   endif
057900150611     c                   if        w_tbo='P' or w_tbo='A'
058000150312     C** interrogazione bolla
058100150312     C                   EXSR      bolla
058200150312     C                   EXSR      visual
058201161124     c                   else
058202161124     c                   eval      parerr='1'
058300911023     C*
058400150313     c                   endif
058500150313     c
058600150313     c     fine          tag
058700150410
058800150410     c                   select
058900150410     c* pa1f03 = 1 --> premuto F3
059000150410     c                   when      v2cflg='2'
059100160504     c                   eval      paRf03='1'
059200150410     c* pa1f03 = S --> Selezionata bolla
059300150410     c                   when      v2cflg='3'
059400160504     c                   eval      paRf03='S'
059500150410     c                   endsl
059600160505
059700160505     c                   endif
059800160505     c
059900930504     C*
060000930504     C*
060100941109     C* PER F3=FINE PULISCO CHIAVE BOLLA E IMPOSTO FLAG F3
060200941109     C     *INKC         IFEQ      *ON
060300941109     C                   Z-ADD     *ZERO         V2CAAS
060400941109     C                   Z-ADD     *ZERO         COMLNP
060500941109     C                   Z-ADD     *ZERO         V2CNRS
060600941109     C                   Z-ADD     *ZERO         V2CNSP
060700160504     C                   MOVEL     '1'           PARF03
060800941109     C                   ENDIF
060900980603     C**
061000160505
061100160505     c* Chiusura con LR
061200160523     c                   if        partla<>' '
061300160505
061400160505     C* CALL A VUOTO AI PGM  PER CHIUDERE I FILES
061500160505     C                   MOVEL     'C'           D98TLA
061600160505     C                   CALL      'TNSD99R'
061700160505     C                   PARM                    TNSD99DS
061800160505     C*
061900160505     C                   MOVEL     'C'           PA1FLG
062000160505     C                   MOVEL     PARAMint      KPJBU
062100160505     C                   CALL      'FNLR75R'                            99
062200160505     C                   PARM                    KPJBA
062300160505     C*
062400160505     C                   CLEAR                   PARAB5
062500160505     C                   MOVEL     'C'           PB5FLG
062600160505     C                   MOVEL     PARAB5        KPJBU
062700160505     C                   CALL      'FNLRB5R'
062800160505     C                   PARM                    KPJBA
062900160505     C*
063000160505      *
063100160505     C                   IF        Wtibs69r  <>  *BLANKS
063200160505     C                   eval      I69TLA  = 'C'
063300160505     C                   call      'TIBS69R'
063400160505     C                   parm                    TIBS69DS
063500160505     c                   ENDIF
063600941212     C*
063700040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
063800040810     c                   clear                   fidn12ds
063900040810     c                   eval      i12tla = 'C'
064000040810     c                   call      'FIDN12R'
064100040810     c                   parm                    fidn12ds
064200051128     c                   clear                   fnlv80ds
064300051128     c                   eval      ilv80tla = 'C'
064400051128     c                   call      'FNLV80R'
064500051128     c                   parm                    kpjba
064600051128     c                   parm                    FNLV80DS
064700070427     c                   eval      id34tla = 'C'
064800070427     c                   call      'FIDG34R'
064900070427     c                   parm                    fidg34ds
065000131120     c                   eval      ilrg7tla = 'C'
065100131120     c                   call      'FNLRG7R'
065200131120     c                   parm                    fnlrg7ds
065300091216     c                   eval      iinttla = 'C'
065400091216     c                   call      'TRULINTR'
065500091216     c                   parm                    trulintds
065600150615     c                   eval      ilrj6tla = 'C'
065700150615     c                   call      'FNLRJ6R'
065800150615     c                   parm                    fnlrj6ds
065900141105
066000040810     c*
066100000000     C                   SETON                                        LR
066200160505     c                   else
066300160505     C                   SETON                                        RT
066400160505     c                   endif
066500160505
066600160606     C                   MOVEL     PARAMU6ds1    KPJBU
066700941011     C   99
066800941011     CANN99              READ      FNBLP01L                               99
066900940802     C**************************************************************************
067000940802     C*    CARICO TABELLE
067100940802     C**************************************************************************
067200911024     C     CARTAB        BEGSR
067300930705     C*
067400930705     C* CARICO TABELLA 7O
067500930705     C                   MOVEL     '7O'          COD
067600941212     C                   Z-ADD     1             C                 3 0
067700940713     C     KTAB2         SETLL     TABEL
067800940713     C     KTAB2         READE     TABEL                                  30
067900930705     C*
068000930705     C     *IN30         DOWEQ     *OFF
068100930705     C     TBLFLG        IFEQ      ' '
068200930705     C                   MOVEL     TBLKEY        KCA(C)
068300930705     C                   MOVEL     TBLUNI        DCA(C)
068400930705     C                   ADD       1             C
068500930705     C                   END
068600940713     C     KTAB2         READE     TABEL                                  30
068700930705     C                   END
068800941024     C*
068900941024     C* CARICO IN SCHIERA LA TABELLA CC
069000941024     C                   Z-ADD     *ZERO         WA
069100941024     C                   CLEAR                   CC1
069200941024     C                   CLEAR                   CC2
069300020329     C                   CLEAR                   CC3
069400941024     C                   MOVEL     'CC'          COD
069500941024     C     KTAB2         SETLL     TABEL00F
069600941024     C     KTAB2         READE     TABEL00F                               99
069700941024     C     *IN99         DOWEQ     *OFF
069800941024     C     TBLKEY        ANDNE     *BLANKS
069900941024     C     TBLKEY        ANDNE     *ZERO
070000941024     C                   ADD       1             WA
070100941024     C                   MOVE      TBLKEY        CC1(WA)
070200941024     C                   MOVEL     TBLUNI        CC2(WA)
070300020402     C                   MOVEL(P)  TBLUNI        DSCC
070400020402     C                   MOVE      §CCDFE        CC3(WA)
070500941024     C     KTAB2         READE     TABEL00F                               99
070600941024     C                   ENDDO
070700060130     C*
070800020329      *
070900020329      * CARICO IN SCHIERA LA TABELLA CB
071000020329     C                   Z-ADD     *ZERO         WA
071100020329     C                   CLEAR                   CB1
071200020329     C                   CLEAR                   CB2
071300020329     C                   MOVEL     'CB'          COD
071400020329     C     KTAB2         SETLL     TABEL00F
071500020329     C     KTAB2         READE     TABEL00F                               99
071600020329     C     *IN99         DOWEQ     *OFF
071700020329     C     TBLKEY        ANDNE     *BLANKS
071800020329     C     TBLKEY        ANDNE     *ZERO
071900020329     C                   ADD       1             WA
072000020329     C                   MOVE      TBLKEY        CB1(WA)
072100020329     C                   MOVEL     TBLUNI        CB2(WA)
072200020329     C     KTAB2         READE     TABEL00F                               99
072300020329     C                   ENDDO
072400991027     C*
072500991027     C* CARICO LA TABELLA $2
072600991027     C                   MOVEL     '$2'          COD
072700991027     C                   MOVEL(P)  1             KEY
072800991027     C     KTAB1         CHAIN     TABEL00F
072900991027     C                   IF        %FOUND
073000991027     C                   MOVEL     TBLUNI        DS$2
073100991027     C                   ELSE
073200991027     C                   CLEAR                   DS$2
073300991027     C                   ENDIF
073400940802     C*
073500911024     C                   ENDSR
073600940802     C**************************************************************************
073700940802     C* EMISSIONE BOLLA
073800940802     C**************************************************************************
073900161014     C     BOLLA         begsr
074000940802     C*
074100160628     c                   setoff                                       18
074200070411     C                   SETOFF                                       220616
074300070403     C                   SETOFF                                       170377
074400070404     C                   SETOFF                                       262708
074500131127     C                   SETOFF                                       2933
074600940408     C                   SETOFF                                       687069
074700070524     C                   SETOFF                                       850488
074800150617     C                   SETOFF                                       866266
074900160614     C                   SETOFF                                       9784
075000161017     C                   SETOFF                                       19
075100991027      *
075200940719     C                   Z-ADD     0             V3CLNA
075300940719     C                   Z-ADD     0             V3CDSP
075400930517     C                   Z-ADD     0             VIDRMN
075500940712     C                   MOVE      *BLANKS       VIDRMA
075600991217     C                   CLEAR                   V3CRPP
075700131120     C                   CLEAR                   atrpda
075800160913     C                   CLEAR                   vidpda
075900131127     C                   CLEAR                   atrtsp
076000160513     C                   CLEAR                   v7ddse
076100160513     C                   CLEAR                   v7ddsed
076200161014     C                   CLEAR                   v7dcons
076300161014     C                   CLEAR                   vidhmc
076400150615     c* ancora da partire
076500150616     c                   if        w_tbo='P' and arbft1=*blanks
076600150616     c                   eval      v1dfbc='Da Partire'
076700150616     c                   seton                                        62
076800150616     c* se "manca record bolla arrivo" metto la scritta in RI
076900150617     c                   if        arbdam>0
077000150617     c                   seton                                        66
077100150617     c                   else
077200150617     C                   Z-ADD     ARBAAS        KAAS
077300150617     C                   Z-ADD     ARBLNP        KLNP
077400150617     C                   Z-ADD     ARBNRS        KNRS
077500150617     C                   Z-ADD     ARBNSP        KNSP
077600150617     c     kanm          setll     fnanm01l                               66
077700150617     c                   endif
077800160513     c* Visualizzo in questo caso la data spunta entrata
077900160513     c                   seton                                        97
078000160513     c                   if        arbdse>0
078100160513     c     *iso          movel     arbdse        dataeur
078200160517     c                   movel     dataeur       w0080
078300160517     c                   eval      v7ddse=%editW(w0080:'  /  /    ')
078400160516     c* astesrisco le seguenti specifiche: per visualizzare se arrivo totale/parziale
078500160516     c* bisognerebbe elaborare sia le spunte del teminal di partenza che quelle della lnp
078600160516     c* insieme.... mentre al momento è possibile farlo con una linea alla volta...
078700160516     c**                 clear                   fnlrj6ds
078800160516     c**                 eval      ILRJ6TRIC='P'
078900160516     c**                 eval      ILRJ6AAS =arbaas
079000160516     c**                 eval      ILRJ6LNP =arblnp
079100160516     c**                 eval      ILRJ6NRS =arbnrs
079200160516     c**                 eval      ILRJ6NSP =arbnsp
079300160516     c**                 eval      ILRJ6lin =arblnp
079400160516     c**                 call      'FNLRJ6R'
079500160516     c**                 parm                    fnlrj6ds
079600160516     c**                 if        olrj6err=*blanks and olrj6fall='S'
079700160516     c**                 eval      V7DDSED='Totale'
079800160516     c**                 else
079900160516     c**                 eval      V7DDSED='Parziale'
080000160516     c**                 endif
080100160517     c                   else
080200160517     c                   eval      v7ddse='NO'
080300160513     c                   endif
080400150615     c                   endif
080500961112     C*
080600911022     C* CONTROLLO SE SPEDIZIONE GIACENTE
080700920219     C* GIACENZA APERTA
080800941202     C                   Z-ADD     ARBAAS        KAAS
080900940713     C                   Z-ADD     ARBLNP        KLNP
081000940713     C                   Z-ADD     ARBNRS        KNRS
081100940713     C                   Z-ADD     ARBNSP        KNSP
081200050405     C     KGCA1         CHAIN     tigcp21l                           31
081300040216if  2C  N31GCPATB        IFEQ      ' '
081400920410     C     GCPDCG        IFNE      0
081500920219     C                   SETON                                        03
081600920219     C                   ELSE
081700070403     C                   SETON                                        0327
081800150616     C                   EVAL      V1DFBC=' GIACENTE '
081900040217     C                   ENDIF
082000040217e   2C                   ENDIF
082100930428     C*
082200021213     C*
082300021213     C* 77 ON - Esistono note supermenrcati d ainterrogare
082400021213     C                   Z-ADD     ARBAAS        KAAS
082500021213     C                   Z-ADD     ARBLNP        KLNP
082600021213     C                   Z-ADD     ARBNRS        KNRS
082700021213     C                   Z-ADD     ARBNSP        KNSP
082800021213     C                   MOVEL     'SCR'         KTRD
082900021213     C     KAR51         CHAIN     FIAR5000                           31
083000021217     C  n31              seton                                        77
083100021217     C   31              setoff                                       77
083200920219     C*
083300920207     C* SE BOLLA CON LA "G" SENZA GIACENZA MECCANIZZATA SEGNALO
083400931116     C     ARBFBC        IFEQ      'G'
083500070412     C                   EVAL      V1DFBC=' GIACENTE '
083600070403     C                   SETON                                        27
083700040217     C                   ENDIF
083800930415     C*
083900940907     C                   MOVEL     ARBAAS        V3CAAS
084000940719     C                   MOVE      ARBNSP        V3CNSP
084100940719     C                   MOVEL     ARBNRS        V3CNRS
084200070404     C                   MOVEL     ARBNRS        V4CNRS
084300131108     C********           MOVEL     ARBNRS        V7CNRS
084400940719     C                   MOVEL     ARBLNP        V3CLNP
084500950508     C                   MOVEL     ARBLNP        V3CLPA
084600131114     c
084700131114     c                   z-add     arbmgs        arbdsp
084800131114     c                   movel     arbaas        arbdsp
084900940908     C*
085000940908     C*  L E G A M I    B O L L A
085100940908     C  N75              EXSR      LEGBOL
085200940908     C*
085300911022     C* NON SEQUENZIALI
085400911023     C     ARBFNS        IFEQ      'S'
085500930705     C                   MOVEL     CNOSEQ        DESFNS
085600911022     C                   ELSE
085700911022     C                   MOVEL     *BLANKS       DESFNS
085800040217     C                   ENDIF
085900930525     C*
086000930525     C* 95 ON - ESITONO EVENTI DA INTERROGARE
086100940713     C                   Z-ADD     ARBAAS        KAAS$
086200940713     C                   Z-ADD     ARBLNP        KLNP
086300940713     C                   Z-ADD     ARBNRS        KNRS
086400940713     C                   Z-ADD     ARBNSP        KNSP
086500990805     C     KEVB1         SETLL     FNEVB01L                               95
086600920402     C*
086700920403     C* 98 ON - ESITONO VARIAZIONI FATTE
086800131007     C     KARB2         SETLL     FiARBF2C                               98
086900990119     C* 04 ON - ESISTONO CA DA VISUALIZZARE
087000990119     C                   CLEAR                   CONTCA
087100161027     C                   CLEAR                   StatoCa
087200040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
087300040809      * legate alla spedizione che passo
087400040809     c                   clear                   fidn12ds
087500040809     c                   eval      i12aas = arbAAS
087600040809     c                   eval      i12lnp = arbLNP
087700040809     c                   eval      i12nrs = arbNRS
087800040809     c                   eval      i12nsp = arbNSP
087900040809     c                   eval      i12fel = 'E'
088000040809     c                   eval      i12fan = 'E'
088100040809     c*
088200040809     c                   call      'FIDN12R'
088300040809     c                   parm                    fidn12ds
088400040809     c*
088500040809     c                   setoff                                       31
088600040809     c*
088700040809     c* se non ci sono errori
088800040809     c                   if        o12err <> ' '
088900040809     c                   seton                                        31
089000040809     c                   else
089100040809     c* se numero di CA trovate maggiore di zero
089200040809     c                   if        o12nca = 0
089300040809     c                   seton                                        31
089400040809     c                   else
089500040809     c                   z-add     1             ii                2 0
089600040809     c                   dow       ii <= o12nca and *in31 = *off
089700040809     c                   movea     skkey(ii)     dskey
089800040809     C                   SETON                                        04
089900040809     C                   ADD       1             CONTCA            3 0
090000161027     c                   if        skdch(ii)>0 and StatoCa<>'A'
090100161027     c                   eval      StatoCa='C'
090200161027     c                   else
090300161027     c                   eval      StatoCa='A'
090400161027     c                   endif
090500040809     C                   Z-ADD     dsaac         VHCAAC
090600040809     C                   Z-ADD     dsfil         VHCFIL
090700040809     C                   Z-ADD     dsnca         VHCNCA
090800040809     c*
090900040809     c                   add       1             ii
091000040809     c                   enddo
091100040809     c*
091200040809     c                   seton                                        31
091300040809     c*
091400040809     c                   endif
091500040809     c                   endif
091600040809     c**
091700070406     c* se Presente FIARP, imposto la cons.richista proposta
091800070404     c                   clear                   v4cdimpr
091900070404     c                   clear                   v4coimpr
092000070404     c                   clear                   v4cprupr
092100070416     c                   clear                   v08dcrpr
092200070404     c                   clear                   v4ctcrpr
092300070404     c                   clear                   v4chcrpr
092400070406     c
092500070406     c* Solo se visualizzabile
092600070406     c     karb2         chain     fiarp01l
092700070406    1c                   if        %found(FIarp01l)
092800070404     c                   seton                                        26
092900070404     C                   MOVEL     arppru        v4cprupr
093000070404     C                   MOVEL     arpoim        v4coimpr
093100070404    2c                   if        arpdim>0
093200070404     c     *iso          movel     arpdim        dataeur
093300070416     c                   movel     dataeur       datadmy
093400070416     c                   movel     datadmy       w0060             6 0
093500070416     c                   movel     w0060         v4cdimpr
093600070404    2c                   endif
093700070404    2c                   if        arpdcr>0
093800070404     c     *iso          movel     arpdcr        dataeur
093900070416     c                   movel     dataeur       v08dcrpr
094000070416     c**                 movel     datadmy       v06dcrpr
094100070404    2c                   endif
094200070404     c                   movel     arphcr        v4chcrpr
094300070404     c                   movel     arptcr        v4ctcrpr
094400070406     c                   if        arptcr=' '
094500070406     c                   eval      v4ctcrpr='IL'
094600070404    1c                   endif
094700070406    1c                   endif
094800070406     c
094900911022     C* TIPO BOLLA CONTROLLO SE FRANCO
095000940719     C                   MOVEL     ARBCBO        V3CCBO
095100911022     C                   MOVEL     '3A'          COD
095200911022     C                   MOVEL     *BLANKS       KEY
095300940712     C                   MOVEL     ARBCBO        KEY
095400940713     C     KTAB1         CHAIN     TABEL                              31
095500911022     C                   MOVEL     TBLUNI        DS3A
095600911022     C                   MOVEL     §3ATB1        FLGTB1            1
095700940719     C                   MOVEL     §3ADES        V3DCBO
095800940719     C                   MOVEL     §3ADES        V5DCBO
095900911030     C* FRANCO
096000040216if  1C     FLGTB1        IFEQ      'F'
096100911022     C                   SETON                                        33
096200940713     C                   MOVE      ARBKSC        V3CKSC
096300940712     C                   MOVE      *BLANKS       VIDKSC
096400040216x   1C                   ELSE
096500911030     C* ASSEGNATO
096600911030     C     ARBCCM        IFNE      0
096700940713     C                   MOVE      ARBCCM        V3CKSC
096800911030     C                   ELSE
096900940713     C                   MOVE      *ZEROS        V3CKSC
097000040217     C                   ENDIF
097100911030     C                   MOVEL     ARBKSC        VIDKSC
097200040217e   1C                   ENDIF
097300070410     c* Numero di telefono del mittente
097400070410     c                   clear                   v3ctelm
097500070410     c                   move      v3cksc        w0040
097600070410     c                   if        w0040<>0000 and w0040<>8888
097700070410     c                             and w0040<>9999
097800070410     c                   eval      i69kin = V3Cksc
097900070410     c                   exsr      CTR_ANAGRA
098000070410     c                   eval      V3ctelm= indtel
098100070410     c                   endif
098200991027     C* 38 ON - ESISTE TB2 ANCHE SENZA TIPO RCD "2" IN FIAR600F
098300920121     C     §3ATB2        COMP      '  '                               3838
098400040218      *
098500040218      *?Controllo se esitono reclami?
098600131111     c                   eval      wARBlnp = ARBlnp
098700131111     c                   eval      wARBnrs = ARBnrs
098800131111     c                   eval      wARBnsp = ARBnsp
098900131111     c                   eval      wARBaas = ARBaas
099000040218     c                   eval      TGDtor  = 'S'
099100040218     c                   eval      TGDogg  = ds_OggSped
099200040218     c     K02TGD02      setll     FITGD000
099300160408     c                   eval      *in64   = (%equal(FITGD32L)   and
099400040303     c                                           (SDSprm =  1
099500040303     c                                        or (SDSprm >  1
099600150402     c                                        and ELRU6INS <> 'N')))
099700940712     C*
099800940712     C* DECODIFICO  "BOLLA DI RIENTRO"
099900961111     C                   MOVEL     *BLANKS       VIDFBR
100000961111     C                   MOVEL     *BLANKS       VIDFFR
100100961111     C                   SETOFF                                       1073
100200940712     C     ARBFBR        IFEQ      'S'
100300961111     C                   MOVEL     'SI'          VIDFBR
100400961111     C                   SETON                                        10
100500040217     C                   ENDIF
100600961111     C     ARBFBR        IFEQ      'F'
100700961111     C                   MOVEL     'FR'          VIDFFR
100800961111     C                   SETON                                        73
100900040217     C                   ENDIF
101000970130     C*
101100970227     C* FLAG CONSEGNA DDT
101200970130     C                   MOVEL     ARBAAS        KAAS
101300970130     C                   MOVEL     ARBLNP        KLNP
101400970130     C                   MOVEL     ARBNRS        KNRS
101500970130     C                   MOVEL     ARBNSP        KNSP
101600970130     C                   MOVEL     'A'           KTRC
101700970131     C                   CLEAR                   V1DBAM
101800000615     C                   eval      v3csop = ' '
101801170117     c* se la bolla è da partire prendo il flag da blp
101802170117   1ac                   if        *in62
101803170117     C     arbFST        IFEQ      'S'
101804170117     C     arbFST        OREQ      'C'
101805170117     C                   MOVEL     'SI DDT'      V1DBAM
101806170117     C                   SETON                                        76
101807170117     C                   ELSE
101808170117     C                   MOVEL     'NO DDT'      V1DBAM
101809170117     C                   SETOFF                                       76
101810170117     C                   ENDIF
101811170117     C                   MOVEL     arbsop        v3csop
101812170117  x1ac                   else
101813170117     c
101900060216     C     KAR41         CHAIN     FiAR401L                           31
102000970227     C* C'E' IL RECORD
102100040216if  1C     *IN31         IFEQ      *OFF
102200000615     C                   movel     ar4not        dsbl4a
102300000615     C                   eval      v3csop = §b4aso
102400031027     c                   Move      §b4Abm        wbam              1
102500040216if  2C     WBAM          IFEQ      'S'
102600970131     C     WBAM          OREQ      'C'
102700031027     c     wbam          oreq      'P'
102800031027     c     wbam          oreq      'Q'
102900970227     C                   SETON                                        76
103000970227     C                   MOVEL     'SI DDT'      V1DBAM
103100040216x   2C                   ELSE
103200970227     C                   SETOFF                                       76
103300970227     C                   MOVEL     'NO DDT'      V1DBAM
103400040216e   2C                   ENDIF
103500970227     C**
103600150615sel 2C*                  SELECT
103700150615     C*    WBAM          WHENEQ    'C'
103800150615     C*    WBAM          OREQ      'K'
103900150615     C*                  MOVEL     'CLI'         V1DFST
104000150615     C*    WBAM          WHENEQ    'J'
104100150615     C*    WBAM          oreq      'P'
104200150615     C*    WBAM          oreq      'Q'
104300150615     C*    WBAM          OREQ      'W'
104400150615     C*                  MOVEL     'Arr'         V1DFST
104500150615     C*    WBAM          WHENEQ    'S'
104600150615     C*    WBAM          OREQ      'Y'
104700150615     C*                  MOVEL     'Par'         V1DFST
104800150615x   2C*                  OTHER
104900150615     C*                  MOVEL     'NO '         V1DFST
105000150615e   2C*                  ENDSL
105100970227     C**
105200970227     C* NON  C'E' IL RECORD
105300150615x   1C                   ELSE
105400160909     c* se la bolla è da partire prendo il flag da blp
105500170117     c*                  if        *in62
105600170117     C*    arbFST        IFEQ      'S'
105700170117     C*    arbFST        OREQ      'C'
105800170117     C*                  MOVEL     'SI DDT'      V1DBAM
105900170117     C*                  SETON                                        76
106000170117     C*                  ELSE
106100170117     C*                  MOVEL     'NO DDT'      V1DBAM
106200170117     C*                  SETOFF                                       76
106300170117     C*                  ENDIF
106400170117     C*                  MOVEL     arbsop        v3csop
106500170117     c*                  else
106600161019     c* Attualmente questa situazione non dovrebbe più capitare: se la bolla è in arrivo
106700161019     c* il fiar4 trk 'A' deve sempre esserci
106800150615     C                   SETON                                        76
106900150615     C                   MOVEL     'SI DDT'      V1DBAM
107000150615     C*                  MOVEL     'PAR'         V1DFST
107100160909     c                   endif
107200150615e   1C                   ENDIF
107300010510     c* numero O.R.M.
107301161207     c                   clear                   v3cpoe
107302161207     c                   clear                   v3cnsr
107303161207     c                   clear                   v3cnor
107304161207     c                   clear                   v3cnrv
107400010510     C                   MOVEL     'M'           KTRC
107500060216     c     kar41         chain     fiar401l                           31
107600010510     c     *in31         ifeq      *off
107700010510     c                   movel     ar4not        dsbl4m
107800010510     c                   z-add     §b4poe        v3cpoe
107900010510     c                   z-add     §b4nsr        v3cnsr
108000010510     c                   z-add     §b4nor        v3cnor
108100010510     c                   z-add     §b4nrv        v3cnrv
108101161207     c     korm          Setll     Fnorm01l
108102161207     c                   If        %Equal(fnorm01l)
108200160628     c                   seton                                        18
108300161207     c                   endif
108800010510     c                   endif
108900130320     c*//* Verifico se ha fiar8
109000130320     C*//                MOVEL     ARBAAS        KAAS
109100130320     C*//                MOVEL     ARBLNP        KLNP
109200130320     C*//                MOVEL     ARBNRS        KNRS
109300130320     C*//                MOVEL     ARBNSP        KNSP
109400130320     c*//  ksped         setll     fiar801l
109500130320     c*//                if        %equal(fiar801l)
109600130320     c*//                eval      v1dAR8='PACKING LIST'
109700130320     C*//                ELSE
109800130320     C*//                CLEAR                   V1DAR8
109900130320     c*//                endif
110000941219     C*
110100941219     C* DECODIFICO  "CONSEGNA A MAGAZZINO"
110200941219     C     ARBFDN        IFEQ      'S'
110300941219     C                   MOVEL     'SI'          V3CFDN
110400941219     C                   ELSE
110500941219     C                   MOVEL     *BLANKS       V3CFDN
110600941219     C                   ENDIF
110700911129     C*
110800920204     C* INCASSO C/A E ASSEGNATO
110900930113     C                   MOVEL     '  '          VIDICC
111000930113     C                   MOVEL     '  '          VIDICA
111100150615     c                   clear                   v7dicc
111200930113     C*
111300930113     C* SE ASSEGNATO O FRANCO CON SECONDA BOLLA
111400930113     C     *IN33         IFEQ      '0'
111500930113     C     *IN33         OREQ      '1'
111600930113     C     *IN38         ANDEQ     '1'
111700930113     C*
111800150617     c     w_tbo         ifeq      'A'
111900160825     C     ARBICA        andne     ' '
112000941216     C     ARBICA        ANDNE     'R'
112100160825     c     w_tbo         oreq      'P'
112200160825     c     arbdcm        andgt     0
112300920204     C                   MOVEL     'SI'          VIDICA
112400040217     C                   ENDIF
112500040217     C                   ENDIF
112600911129     C*
112700911030     C* LINEA PARTENZA DECODIFICA
112800020225     c                   clear                   og143
112900150313     C                   Z-ADD     ARBLNP        NUM3              3 0
113000930706     C     NUM3          CHAIN     AZORG01L                           31
113100940719     C  N31              MOVEL     ORGDES        V3DLNP
113200940719     C   31              MOVEL     *BLANKS       V3DLNP
113300020225     c                   eval      og143 = orgde3
113400020225     c                   eval      lnpntw = §ogntw
113500911030     C* LINEA ARRIVO   DECODIFICA
113600991217     c                   clear                   og143
113700160517     C                   clear                   orgde5
113800160913     c                   clear                   v7dlnaa
113900160913     c                   clear                   v7dalna
114000070403     C     arblna        CHAIN     AZORG01L                           31
114100160913     C                   movel     orgde3        og143
114200160913     C                   movel     orgde8        og148
114300940719     C  N31              MOVEL     ORGDES        V3DLNA
114400940719     C   31              MOVEL     *BLANKS       V3DLNA
114500070403     c                   movel     v3dlna        v7dlna
114600160913     c                   if        §ogntw<>'EEX' and
114700160913     c                             §ogntw<>'DPD' and §ogntw<>'FED'
114800160913     c                   movel     'a'           v7dalna
114900160517     c                   if        orgde5<>*blANKS
115000160517     c                   movel     orgde5        v7dlnaa
115100160517     c                   else
115200160517     c                   movel     v3dlna        v7dlnaa
115300160517     c                   endif
115400160913     c                   endif
115500020304     c                   eval      lnantw = §ogntw
115600070426     c* p.o. gestore di arblna
115700070426     c                   clear                   fnlv55ds
115800070426     c                   movel     arblna        d55lin
115900070426     c                   movel     '6'           d55tpt
116000070426     C                   time                    wrkoggiiso
116100070426     C     *iso          move      wrkoggiiso    d55drf
116200070426     c                   call      'FNLV55R'
116300070426     c                   parm                    fnlv55ds
116400070426     c
116500070426     c                   if        d55tfa<>arblna
116600070426     C     d55tfa        CHAIN     AZORG01L                           31
116700070426     C  n31              movel     orgde8        og148
116800070426     C   31              clear                   og148
116900070426     c                   endif
117000070426     c
117100070403     c
117200070403     c* Abilito visualizzazione nuove funzioni se GEO attiva
117300070426     c* Per p.o. gestione linea di arrivo e p.o. utente
117400070403     c                   if        §oggeot='S' and dutgeot='S'
117500070524     c                   seton                                        1788
117600070406     c                   else
117700070406     c* non visualizzo nemmeno fiarp
117800070406     c                   setoff                                       26
117900070403     c                   endif
118000070524     c
118100070524     c* Vedo se almeno è attivo lo scambio dati con tellus
118200070524     c                   if        not *in88 and §ogtlls<>' ' and dutgeot='S'
118300070524     c                   seton                                        88
118400070524     c                   endif
118500070404     c
118600070404     c* Emetto riga sintetica altre abilitazioni
118700070521     c                   clear                   v5dngd
118800140109     c***                if        *in17
118900070404     c                   clear                   v3cges
119000070404     C                   MOVEL     *BLANKS       V3cgesd
119100150615     C                   clear                   v7drig1
119200070404     c*
119300131120     c                   clear                   fnlrg7ds
119400131120     c                   eval      ilrg7aas=arbaas
119500131120     c                   eval      ilrg7lnp=arblnp
119600131120     c                   eval      ilrg7nrs=arbnrs
119700131120     c                   eval      ilrg7nsp=arbnsp
119800131120     c                   eval      ilrg7lung=58
119900131120     c                   call      'FNLRG7R'
120000131120     c                   parm                    fnlrg7ds
120100131120     c                   movel     olrg7vssin    v3cges
120200150615     c* richiamo per visualizzazione in videata Globale
120300150615     c                   clear                   fnlrg7ds
120400150615     c                   eval      ilrg7aas=arbaas
120500150615     c                   eval      ilrg7lnp=arblnp
120600150615     c                   eval      ilrg7nrs=arbnrs
120700150615     c                   eval      ilrg7nsp=arbnsp
120800150615     c                   eval      ilrg7lung=10
120900150615     c                   eval      ilrg7nb1='S'
121000150615     c                   eval      ilrg7nb2='A'
121100150615     c                   eval      ilrg7nb3='C'
121200150615     c                   eval      ilrg7nb4='F'
121300150615     c                   eval      ilrg7nb5='M'
121400150615     c                   eval      ilrg7nb6='B'
121500150615     c                   eval      ilrg7nb7='9'
121600150615     c                   eval      ilrg7nb8='L'
121700150615     c                   call      'FNLRG7R'
121800150615     c                   parm                    fnlrg7ds
121900150615     c                   eval      v7drig1=*all'-'
122000150615     c                   if        olrg7abi<>*blanks
122100161123     c***                eval      %subst(v7drig1:66:12)=' ' +
122200161123     c***                                          %subst(olrg7vssin:1:10) + '?'
122201161123     c                   eval      %subst(v7drig1:66:12)=red +
122202161123     c                                             %subst(olrg7vssin:1:10) + '?'
122300150616     c                   setoff                                       55
122400150616     c                   else
122500150616     c                   seton                                        55
122600150615     c                   endif
122700140109     c***                endif
122800070410     c
122900070524     c* Reperisco dati della distinta automatica tramite FIDG34R
123000070524     c                   if        *in88
123100070410     c                   clear                   v5ccgi
123200070411     c                   clear                   v5dcgi
123300070410     c                   clear                   v5dtgi
123400070410     c                   clear                   v5dttl
123500070411     c                   clear                   v5clno
123600070427     c                   clear                   v5dlno
123700070427     c                   clear                   v5cargerr
123800070427     c                   clear                   v5dargerr
123900070427     c
124000070427     c                   clear                   fidg34ds
124100070427     c                   eval      id34tip='2'
124200070427     c                   eval      id34aas=arbaas
124300070427     c                   eval      id34lnp=arblnp
124400070427     c                   eval      id34nrs=arbnrs
124500070427     c                   eval      id34nsp=arbnsp
124600070427     c                   call      'FIDG34R'
124700070427     c                   parm                    FIDG34DS
124800070410     c
124900070427     c                   movel     od34cgi       v5ccgi
125000070410    3c                   select
125100070427     c                   when      od34tgi='A'
125200070410     c                   eval      v5dtgi='Automatico'
125300070427     c                   when      od34tgi='M'
125400070410     c                   eval      v5dtgi='Manuale   '
125500070410    3c                   ENDSL
125600070411     c* Decodifica giro
125700070427     c                   eval      v5dcgi=od34cgides
125800070427     c* esito/errore
125900070427     c                   movel     od34err       v5cargerr
126000070427     c                   eval      v5dargerr=od34errdes
126100070427     c* livello normalizzazione
126200070427     c                   movel     od34lno       v5clno
126300070427     c                   eval      v5dlno=od34lnodes
126400070427     c* elaborazione tellus
126500070427     c***                eval      v5dttl=od34ttldes
126600070427     c                   select
126700070427     c                   when      od34ttl='1'
126800070427     c                   eval      v5dttl='SI- CON Giro'
126900070427     c                   when      od34ttl='2'
127000070427     c                   eval      v5dttl='SI-SenzaGiro'
127100070427     c                   endsl
127200070521     c*
127300070521     c* Decodifico se assegnazione automatica o manuale
127400070521     c                   if        arbndc>0 and arbngd<>0
127500070521     c                   eval      v5dngd='MANUALE'
127600070521     c                   endif
127700070521     c                   if        arbndc>0 and arbngd=0
127800070521     c                   eval      v5dngd='AUTOMAT'
127900070521     c                   endif
128000070410     c
128100070410    1c                   endif
128200991217     c
128300060518     c* LNA DPD o lnp dpd: leggo record "I" di ar4
128400000216     c                   setoff                                       08
128500000216     c                   clear                   v3czon
128600000216     c                   clear                   v3crtn
128700040216if  1c                   if        lnantw = 'DPD'
128800991217     c                   seton                                        85
128900991217     C                   MOVEL     ARBAAS        KAAS
129000991217     C                   MOVEL     ARBLNP        KLNP
129100991217     C                   MOVEL     ARBNRS        KNRS
129200991217     C                   MOVEL     ARBNSP        KNSP
129300060518     C                   MOVEL     'I'           KTRC
129400060216     C     KAR41         CHAIN     FiAR401L                           31
129500060518     C  N31              MOVEL     AR4NOT        dsbl4i
129600060518     c   31              clear                   dsbl4i
129700060518     C                   MOVEL(P)  §b4ipn        V3CRPP
129800060518     c                   if        §b4ida<>*blanks or §b4iza>*zeros
129900060518     c                   movel     §b4ida        v3crtn
130000060518     c                   movel     §b4iza        v3czon
130100000216     c                   seton                                        08
130200000216     c                   endif
130300050304     c                   else
130400050304if  1c                   if        lnpntw = 'DPD'
130500050304     C                   MOVEL     ARBAAS        KAAS
130600050304     C                   MOVEL     ARBLNP        KLNP
130700050304     C                   MOVEL     ARBNRS        KNRS
130800050304     C                   MOVEL     ARBNSP        KNSP
130900060518     C                   MOVEL     'I'           KTRC
131000060518     c                   clear                   dsbl4i
131100060216     C     KAR41         CHAIN     FiAR401L
131200060216     c                   if        %found(fiar401l)
131300060518     C                   MOVEL     AR4NOT        dsbl4i
131400060518     C                   MOVEL     §b4ipn        V3CRPP
131500050304     c                   seton                                        85
131600050304     c                   endif
131700050304     c                   endif
131800040216e   1c                   endif
131900971219     C*
132000971219     C* 16 ON  - SI TRATTA DI BOLLA EXPORT
132100020225     c                   if        lnantw = 'EEX' or lnantw = 'EUP' or
132200060201     c                             lnantw = 'FED' or lnantw='DPD'
132300971219     C                   SETON                                        16
132400971219    1C                   ENDIF
132500940719     C                   MOVEL     V3DLNA        D15LNA
132600920804     C*
132700940719     C                   MOVEL     ARBLNA        V3CLNA
132800940719     C                   MOVEL     ARBRSM        V3CRSM
132900070410     C                   MOVEL     ARBRSM        V4CRSM
133000940713     C                   MOVEL     ARBRSM        V28RSM
133100940719     C                   MOVEL     ARBINM        V3CINM
133200940719     C                   MOVEL     ARBPRM        V3CPRM
133300940719     C                   MOVEL     ARBCAM        V3CCAM
133400940719     C                   MOVEL     ARBLOM        V3CLOM
133500160912     C                   MOVEL     ARBLOM        V4CLOM
133600940713     C                   MOVEL     ARBLOM        V25LOM
133700940719     C                   MOVEL     ARBFAP        V3CFAP
133800950421     C                   MOVEL     ARBNZM        V3CNZM
133900131108     c                   if        arbnzm<>*blaNKS
134000131108     C                   MOVEL     arbnzm        v7cprm
134100131108     c                   else
134200131108     C                   MOVEL(P)  arbprm        v7cprm
134300131108     c                   endif
134400131108     c
134500050113     C                   MOVEL     ARBXCO        VIDXCO
134600130221     c                   if        vidxco >*zeros  and no_f22=*blanks
134700110928     c                   seton                                        86
134800110928     c                   endif
134900940719     C                   MOVEL     *BLANKS       V3CPIM
135000940719     C                   MOVEL     *BLANKS       V4CPID
135100061108     c                   clear                   v3cdfm
135200061108     c                   clear                   v4cdfd
135300950411     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
135400060216     C* DA FiAR4 SE RECORD ESISTENTE
135500950411     C                   MOVEL     ARBAAS        KAAS
135600950411     C                   MOVEL     ARBLNP        KLNP
135700950411     C                   MOVEL     ARBNRS        KNRS
135800950411     C                   MOVEL     ARBNSP        KNSP
135900950411     C                   MOVEL     'P'           KTRC
136000060216     C     KAR41         CHAIN     FiAR401L                           31
136100040216if  1C     *IN31         IFEQ      *OFF
136200950411     C                   MOVEL     AR4NOT        V3CPIM
136300950411     C                   MOVE      AR4NOT        V4CPID
136400040216x   1C                   ELSE
136500950411     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI ARBCPI
136600040216if  2C     ARBCPI        IFNE      *BLANKS
136700950411     C  N33              MOVEL     ARBCPI        V4CPID
136800950411     C   33§3ATB2        IFEQ      '  '
136900950411     C                   MOVEL     ARBCPI        V3CPIM
137000950411     C                   ELSE
137100950411     C                   MOVEL     ARBCPI        V4CPID
137200040217     C                   ENDIF
137300040217e   2C                   ENDIF
137400040217e   1C                   ENDIF
137500061108      * valorizzo il codice fiscale del mittente e del destinatario
137600061108      * FIAR4 rcd Q
137700061108     c                   eval      ktrc = 'Q'
137800061108     c     kar41         chain     fiar401l
137900061108if  1c                   if        %found(fiar401l)
138000061108     c                   movel     ar4not        v3cdfm
138100061108     c                   move      ar4not        v4cdfd
138200061108x   1c                   endif
138300980704     C* VISUALIZZO SE C'E' IL RIFERIM PARTNER
138400971219     C                   CLEAR                   V3CEST
138500971219     C                   MOVEL     ARBAAS        KAAS
138600971219     C                   MOVEL     ARBLNP        KLNP
138700971219     C                   MOVEL     ARBNRS        KNRS
138800971219     C                   MOVEL     ARBNSP        KNSP
138900971219     C                   MOVEL     'E'           KTRC
139000060216     C     KAR41         CHAIN     FiAR401L                           31
139100980704     C* VISUALIZZO IN UNA BOLLA ITALIA SE C'E'
139200980704     C* VISUALIZZO SEMPRE IN UNA BOLLA IMPORT/EXPORT
139300971219     C  N31              MOVEL     AR4NOT        V3CRPP
139400980704     C  N31              SETON                                        85
139500980704     C**
139600020225     c                   if        *in31 and
139700020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
139800020225     c                              lnpntw = 'FED') or
139900020225     c                             (lnantw = 'EEX' or lnantw = 'EUP' or
140000020225     c                              lnantw = 'FED')
140100980704     C                   SETON                                        85
140200980704     C                   ENDIF
140300980704     C**
140400940719     C                   MOVEL     ARBRSD        V4CRSD
140500131108     C**********         MOVEL     ARBRSD        V30RSD
140600940713     C                   MOVEL     ARBRSD        V32RSD
140700940719     C                   MOVEL     ARBIND        V4CIND
140800131119     C                   MOVEL     ARBIND        V33IND
140900940719     C                   MOVEL     ARBPRD        V4CPRD
141000940719     C                   MOVEL     ARBCAD        V4CCAD
141100940719     C                   MOVEL     ARBLOD        V4CLOD
141200131119     C*****              MOVEL     ARBLOD        V24LOD
141300940719     C                   MOVEL     ARBFIN        V4CFIN
141400940719     C                   MOVEL     ARBNZD        V4CNZD
141500160505
141600160505     c* Giorni di chiusura: Visualizzo la relativa dicitura solo se presenti
141700160505     c                   clear                   v7dggc
141800160505     c                   if        arbgc1<>*blanks or arbgc2<>*blanks
141900160505     c                   eval      v7dggc='GG.Chiusura'
142000160505     c                   endif
142100160505
142200911022     C                   MOVEL     ARBGC1        VIDGC1
142300911022     C                   MOVEL     ARBGC2        VIDGC2
142400911022     C                   MOVEL     ARBCTR        VIDCTR
142500940719     C                   MOVEL     ARBCTS        V6CCTS
142600131108     c                   if        arbnzd<>*blaNKS
142700131108     C                   MOVEL     arbnzd        v7cprd
142800131108     c                   else
142900131108     C                   MOVEL(P)  arbprd        v7cprd
143000131108     c                   endif
143100940719     C* DECODIFICA CODICE TASSAZIONE
143200940719     C                   MOVEL     'CT'          COD
143300940719     C                   MOVEL     *BLANKS       KEY
143400940719     C                   MOVEL     ARBCTS        KEY
143500940719     C     KTAB1         CHAIN     TABEL00F                           31
143600940719     C                   MOVEL     TBLUNI        V6DCTS
143700911022     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
143800911022     C                   MOVEL     'TR'          COD
143900911022     C                   MOVEL     *BLANKS       KEY
144000911022     C                   MOVEL     VIDCTR        UNOCTR            1
144100911022     C                   MOVEL     UNOCTR        KEY
144200940713     C     KTAB1         CHAIN     TABEL                              31
144300911022     C                   MOVEL     TBLUNI        DEFCTR
144400070404     C*****              MOVEL     ARBFTM        V3CFTM
144500150615     c*                  if        arbftm='S'
144600150615     c*                  eval      v3dftm='TASSATA CON TARIFFA MITTENTE'
144700150615     c*                  endif
144800940711     C* DECODIFICA TIPO SERVIZIO BOLLE
144900090604     c                   clear                   ds5e
145000940711     C                   MOVEL     '5E'          COD
145100940711     C                   MOVEL     *BLANKS       KEY
145200940713     C                   MOVEL     ARBTSP        KEY
145300941024     C     KTAB1         CHAIN     TABEL00F                           31
145400090604     C  N31              MOVEL     TBLUNI        ds5e
145500090604     C  N31              MOVEL     §5ed08        V1DTSP
145600940719     C   31              MOVEL     *BLANKS       V1DTSP
145700131127     c                   if        §5eftc<>' '
145800131127     c                   movel     ri            atrtsp
145900160517     c**                 else
146000160517     c**                 movel     hi            atrtsp
146100131127     c                   endif
146200940713     C                   MOVEL     ARBVMD        VIDVMD
146300940713     C                   MOVEL     ARBVAD        VIDVAD
146400940713     C                   MOVEL     ARBIAS        VIDIAS
146500940713     C                   MOVEL     ARBVAS        VIDVAS
146600911022     C                   MOVEL     ARBNAS        VIDNAS
146700950214     C                   CLEAR                   VIDFLS
146800950214     C     ARBNCD        IFNE      *ZERO
146900950214     C     ARBFNS        ORNE      *BLANKS
147000940923     C                   MOVEL     ARBFLS        VIDFLS
147100950214     C                   ENDIF
147200950214     C                   Z-ADD     ARBFLS        V3CFLS
147300911022     C                   MOVEL     ARBNCL        VIDNCL
147400070410     C                   MOVEL     ARBPKB        VIDPKb
147500070410     C                   MOVEL     ARBPKF        VIDPKF
147600070410     C                   MOVEL     ARBVLb        VIDVLb
147700070410     C                   MOVEL     ARBFVb        VIDFVb
147800070410     C                   MOVEL     ARBVLF        VIDVLF
147900070410     C                   MOVEL     ARBFVF        VIDFVF
148000930705     C     ARBNCR        IFLT      99998
148100940711     C                   MOVEL     ARBVLC        VIDVLC
148200940711     C                   MOVEL     ARBNCR        VIDNCR
148300921231     C                   ELSE
148400990909     C                   Z-ADD     0             VIDVLC
148500990909     C                   Z-ADD     0             VIDNCR
148600921231     C                   END
148700990909     C     ARBNCP        IFLT      99998
148800990909     C                   MOVEL     ARBPKC        VIDPKC
148900990909     C                   MOVEL     ARBNCP        VIDNCP
149000990909     C                   ELSE
149100990909     C                   Z-ADD     0             VIDPKC
149200990909     C                   Z-ADD     0             VIDNCP
149300990909     C                   END
149400940711     C                   MOVEL     ARBQFT        VIDQFT
149500031119
149600031119      * Referente e Telefono del destinatario
149700031119     c                   Clear                   V4cRef
149800031119     c                   Clear                   V4cTeld
149900091214     c                   clear                   w_cdi
150000150622     c                   clear                   v03dac
150100150622     c                   clear                   v03orc
150200150622     c                   clear                   v03pru
150300150622     c                   clear                   v03nmtd
150400031119      * li prendo da Fiar500f record 'GEN'
150500130320     c                   clear                   v1dAR8l
150600130320     c                   clear                   v1dAR8
150700031119     c                   Clear                   dAr5Gen
150800031119     c                   Z-Add     ArbAas        Kaas
150900031119     c                   Z-Add     ArbLNp        Klnp
151000031119     c                   Z-Add     ArbNrs        Knrs
151100031119     c                   Z-Add     ArbNsp        Knsp
151200031119     c                   Eval      Ktrd = 'GEN'
151300031119     c     kAr51         Chain     Fiar501l
151400031119If  1c                   If        %Found(Fiar501l)
151500031119     c                   Movel     Ar5Uni        Dar5Gen
151600031119     c                   Eval      V4cRef = §Ar5Ref
151700031119     c                   Eval      V4cTeld = §Ar5Teld
151800091214     c* memorizzo eventuale codice beneficiario assegno
151900091214     c                   eval      w_cdi=§ar5cdi
152000150622     c* Immissione bolla:
152100150622     c* Visualizzo dat/ora creazione del fiar5 record GEN come "immissione"
152200150622     c* perchè in immissione bolle viene creato fiar5 rek "GEN" per memorizzare
152300150622     c* utente/terminale di immissione. Non uso BLPDIM anche perchè non ho l'ora ma solo
152400150622     c* la data su blp
152500150622     c     *iso          movel     ar5dac        datadmy
152600150622     c                   movel     datadmy       v03dac
152700150622     c                   movel     ar5orc        v03orc
152800150622     c* data/ora
152900150622     c* profilo utente e terminale
153000150622     c                   Eval      V03pru = §Ar5bpru
153100150622     c                   Eval      V03nmtd = §Ar5bnmtd
153200130320     c* verifico se LDV con packing-list e/o PDF
153300130320sel 2c                   select
153400130320w   2c                   when      §AR5als = 'S'
153500130320     c                   eval      v1dAR8l = 'PACKING LIST PDF'
153600130320     c                   eval      v1dAR8  = 'PACKING PDF '
153700130320w   2c                   when      §AR5alx = 'S'
153800130320     c                   eval      v1dAR8l = 'PACKING LIST'
153900130320     c                   eval      v1dAR8  = 'PACKING LIST'
154000130320e   2c                   endsl
154100031119    1c                   EndIf
154200040301
154300040301      * Se colli di valore visualizzo "V" nell'apposito campo
154400040301      *  con lo specifico attributo di visualizzazione
154500040301if  1c                   if        §AR5bva = *blanks
154600040301     c                   eval      VIDbva  = *blanks
154700150612     c                   eval      V7Dbva  = *blanks
154800040301x   1c                   else
154900040301     c                   eval      VIDbva  = 'V'
155000150612     c                   eval      V7Dbva  = 'VALORE'
155100040301e   1c                   endif
155200110601
155300110610     c* email destinatario
155400110601     c                   setoff                                       60
155500110601     c                   clear                   v4cemw
155600110610     c                   clear                   v4cemd
155700130927     c                   Clear                   dAr5emd
155800110601      * li prendo da Fiar500f record 'EMD'
155900110601     c                   Z-Add     ArbAas        Kaas
156000110601     c                   Z-Add     ArbLNp        Klnp
156100110601     c                   Z-Add     ArbNrs        Knrs
156200110601     c                   Z-Add     ArbNsp        Knsp
156300110601     c                   Eval      Ktrd = 'EMD'
156400130927     c***  kAr51         setll     Fiar501l
156500130927     c     kAr51         chain     Fiar501l
156600130927If  1c                   If        %found(Fiar501l)
156700130927     c                   eval      dar5emd=ar5uni
156800130927     c                   if        §ar5tel<>*blanks or §ar5eml<>*blanks
156900110610     c                   Eval      V4cemd = 'SI'
157000110601     c                   seton                                        60
157100130927     c                   endif
157200110601    1c                   EndIf
157300040301
157400030121      * Bancali
157500030121     c                   Clear                   VidTba
157600030121     c                   Clear                   VidTb2
157700030122     c                   Clear                   VidNba
157800030122     c                   Clear                   VidNb2
157900030703      * Record 'BAN'
158000030703If  0c                   If        %subst(ArbGva:1:1) = 'B'
158100030121     c                   Clear                   dAr5Ban
158200030121     c                   Eval      Ktrd = 'BAN'
158300030121     c     kAr51         Chain     Fiar501l
158400030121If  1c                   If        %Found(Fiar501l)
158500030121     c                   Movel     Ar5Uni        Dar5Ban
158600030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
158700030122      * per le note relative al mancato reso dei bancali
158800030122     c                   If        Not *In77 and
158900030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
159000030122     c                   Eval      *In77 = *On
159100030122     c                   EndIf
159200030121     c                   Eval      VidNba = §Ar5Nba
159300030121     c                   Eval      VidNb2 = §Ar5Nb2
159400030121If  2c                   If        §Ar5Tba <> *Zeros
159500030703     c                   Eval      wtba = §Ar5Tba
159600030703     c                   ExSr      Sr_DecTba
159700030703     c                   Eval      VidTba = wdestba
159800030121    2c                   EndIf
159900030121If  2c                   If        §Ar5Tb2 <> *Zeros
160000030703     c                   Eval      wtba = §Ar5Tb2
160100030703     c                   ExSr      Sr_DecTba
160200030703     c                   Eval      VidTb2 = wdestba
160300030121    2c                   EndIf
160400030121    1c                   EndIf
160500030703    0c                   EndIf
160600030703      * Record 'BNB'  -  Spedizione OVER 25 colli
160700030703If  0c                   If        %subst(ArbGva:1:1) = 'O'
160800030703     c                   Clear                   dAr5Bnb
160900030703     c                   Eval      Ktrd = 'BNB'
161000030703     c     kAr51         Chain     Fiar501l
161100030703If  1c                   If        %Found(Fiar501l)
161200030703     c                   Movel     Ar5Uni        Dar5Bnb
161300030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
161400030703      * per i colli originali in caso di spedizione over 25 colli
161500030703     c  n77              Eval      *In77 = *On
161600030703     c                   Eval      VidNba = §Ar5bNba
161700030703     c                   Eval      VidNb2 = §Ar5bNb2
161800030703If  2c                   If        §Ar5bTba <> *Zeros
161900030703     c                   Eval      wtba = §Ar5bTba
162000030703     c                   ExSr      Sr_DecTba
162100030703     c                   Eval      VidTba = wdestba
162200030703    2c                   EndIf
162300030703If  2c                   If        §Ar5bTb2 <> *Zeros
162400030703     c                   Eval      wtba = §Ar5bTb2
162500030703     c                   ExSr      Sr_DecTba
162600030703     c                   Eval      VidTb2 = wdestba
162700030703    2c                   EndIf
162800030703    1c                   EndIf
162900030703    0c                   EndIf
163000911129     C*
163100131120     C**   ARBACC        IFEQ      'S'
163200131120     C**                 MOVEL     'SI'          V5CACC
163300131120     C**                 ELSE
163400131120     C**                 MOVEL     'NO'          V5CACC
163500131120     C**                 END
163600160825     c     W_tbo         ifeq      'A'
163700160825     C     ARBICC        andne     ' '
163800941216     C     ARBICC        ANDNE     'R'
163900160825     c     w_tbo         oreq      'P'
164000160825     c     arbdcm        andgt     0
164100160825     c
164200930218     C                   MOVEL     'SI'          VIDICC
164300150615     C                   eval      v7dicc='Incassato'
164400930218     C                   END
164500911022     C*
164600911022     C                   MOVEL     ARBRMN        VIDRMN
164700940712     C                   MOVEL     ARBRMA        VIDRMA
164800911022     C                   MOVEL     ARBRMO        VIDRMO
164900911022     C                   MOVEL     ARBNCD        VIDNCD
165000911022     C                   MOVEL     ARBNCA        VIDNCA
165100911022     C                   MOVEL     ARBZNC        VIDZNC
165101161122     c* campo segnacolli dal/al per la videaa di riepilogo
165102161122     c                   clear                   vidncda
165103161122     c* non sequenziali
165104161122     c                   if        arbfns='S'
165105161122     c                   eval      vidncda='Non Sequenziali'
165106161122     c                   else
165107161122     c*     sequenziali
165108161122     c                   if        vidncd>0
165109161122     c                   eval      %subst(vidncda:1:7)=%editc(vidncd:'Z')
165110161122     c                   eval      %subst(vidncda:8:1)='/'
165111161122     c                   if        vidnca>0
165112161122     c                   eval      %subst(vidncda:9:7)=%editc(vidnca:'Z')
165113161122     c                   endif
165114161122     c                   endif
165115161122     c                   endif
165200051104     c                   select
165300051104     c                   when      arbffd='S'
165400051104     C                   MOVEL     'SI '         VIDFFD
165500161104     c                   eval      v1dffd='F.DEPOSITO'
165600150615     c***                when      arbffd='M'
165700150615     C***                MOVEL     'SAB'         VIDFFD
165800051104     C                   other
165900051104     C                   MOVEL     '   '         VIDFFD
166000161104     c                   clear                   v1dffd
166100051104     C                   ENDsl
166200051124     c
166300940719     C                   MOVEL     ARBNFV        V3CNFV
166400051124     c* richiamp pgm per decodifica
166500051124     C                   MOVEL     *BLANKS       VIDTCR
166600051124     c                   clear                   vidhcr
166700051124     c                   clear                   viddcr
166800051124     c                   if        arbdcr>0 or arbhcr>0
166900051124     c                   clear                   fnlv80ds
167000051124     c                   eval      ilv80aas=arbaas
167100051124     c                   eval      ilv80lnp=arblnp
167200051124     c                   eval      ilv80nrs=arbnrs
167300051124     c                   eval      ilv80nsp=arbnsp
167400051124     c                   eval      ilv80mgs=arbmgs
167500051124     c                   eval      ilv80tc1=arbtc1
167600051124     c                   eval      ilv80tc2=arbtc2
167700051124     c                   eval      ilv80dcr=arbdcr
167800051124     c                   eval      ilv80hcr=arbhcr
167900051124     c                   eval      ilv80tcr=arbtcr
168000051124     c                   call      'FNLV80R'
168100051124     c                   parm                    kpjba
168200051124     c                   parm                    fnlv80ds
168300051124     c
168400051124     c                   eval      viddcr=olv80dcr8
168500051124     c                   eval      vidtcr=olv80tcrd
168600911023     C                   MOVEL     ARBHCR        VIDHCR
168700051124     c                   endif
168800051124     c
168900070521     c* Se presente ora consegna, la edito
169000161014     c**                 clear                   vidhmc
169100161014     c**                 if        arbhmc>0
169200161014     c**                 eval      vidhmc=%editw(arbhmc: '0 :  ')
169300161014     c**                 endif
169400911023     C                   MOVEL     ARBNTC        VIDNTC
169500911023     C                   MOVEL     ARBTMC        VIDTMC
169600940923     C*
169700940923     C* DECODIFICO IL TIPO MANCATA CONSEGNA
169800940923     C                   MOVEL     *BLANKS       V5DTMC
169900940923     C     ARBTMC        IFNE      *BLANKS
170000940923     C                   MOVEL     '2A'          COD
170100940923     C                   MOVEL     *BLANKS       KEY
170200940923     C                   MOVEL     ARBTMC        KEY
170300940923     C     KTAB1         CHAIN     TABEL00F                           31
170400940923     C  N31              MOVEL     TBLUNI        V5DTMC
170500940923     C                   ENDIF
170600940923     C*
170700911023     C                   MOVEL     ARBMIF        VIDMIF
170800911023     C                   MOVEL     ARBNMI        VIDNMI
170900940923     C*
171000940713     C                   MOVEL     ARBGGA        V4CGGA
171100940923     C                   MOVEL     *BLANKS       V4IGGA
171200940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
171300940923     C     V4CGGA        COMP      *BLANKS                            4242
171400940923     C*
171500940713     C                   MOVEL     ARBGMA        V4CGMA
171600940923     C                   MOVEL     *BLANKS       V4IGMA
171700940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
171800940923     C     V4CGMA        COMP      *BLANKS                            4343
171900940923     C*
172000960208     C                   MOVEL     ARBGVA        V4CGVA
172100960208     C                   MOVE      ' '           V4CGVA
172200940923     C                   MOVEL     *BLANKS       V4IGVA
172300940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
172400940923     C     V4CGVA        COMP      *BLANKS                            4444
172500940923     C*
172600070403     C* FLAG blocco CONSEGNA
172700070403     C                   SELECT
172800070403     c                   WHEN      arbfbc='A'
172900070403     c                   eval      v1dfbc='LascAVVISO'
173000070403     C                   SETON                                        27
173100070403     c                   WHEN      arbfbc='B'
173200070403     c  n17              eval      v1dfbc=' BLOCCATA '
173300070412     c   17              eval      v1dfbc=' RIMPALLO '
173400070412     C                   SETON                                        27
173500070403     c                   WHEN      arbfbc='*'
173600070403     c  n17              eval      v1dfbc=' BLOCCATA '
173700070403     c   17              eval      v1dfbc=' GIACENTE '
173800070403     C                   SETON                                        27
173900070403     C                   ENDSL
174000070403     c
174100070403     C**   ARBFBC        COMP      'A'                                    27
174200070403     C**   ARBFBC        COMP      'B'                                    17
174300070403     C**N17ARBFBC        COMP      '*'                                    17
174400940711     C* TIPO CONSEGNA PARTICOLARE 1
174500940804     C                   MOVEL     *BLANKS       V5DTC1
174600150615     C                   MOVEL     *BLANKS       V7DTC1
174700940711     C     ARBTC1        IFNE      ' '
174800911115     C                   MOVEL     '1P'          COD
174900911115     C                   MOVEL     *BLANKS       KEY
175000940711     C                   MOVEL     ARBTC1        KEY
175100940713     C     KTAB1         CHAIN     TABEL                              31
175200940804     C  N31              MOVEL     TBLUNI        V5DTC1
175300940804     C   31              MOVEL     *BLANKS       V5DTC1
175400150615     C  N31              MOVEL     TBLUNI        V7DTC1
175500150615     C   31              MOVEL     *BLANKS       V7DTC1
175600930113     C                   END
175700940711     C* TIPO CONSEGNA PARTICOLARE 2
175800940804     C                   MOVEL     *BLANKS       V5DTC2
175900940804     C                   MOVEL     *BLANKS       V7DTC2
176000940711     C     ARBTC2        IFNE      ' '
176100940711     C                   MOVEL     '1P'          COD
176200940711     C                   MOVEL     *BLANKS       KEY
176300940711     C                   MOVEL     ARBTC2        KEY
176400940713     C     KTAB1         CHAIN     TABEL                              31
176500940804     C  N31              MOVEL     TBLUNI        V5DTC2
176600940804     C   31              MOVEL     *BLANKS       V5DTC2
176700940804     C  N31              MOVEL     TBLUNI        V7DTC2
176800940804     C   31              MOVEL     *BLANKS       V7DTC2
176900940711     C                   END
177000930113     C* TIPO MANCATA CONSEGNA
177100930113     C                   MOVEL     *BLANKS       DESCCA
177200160505     C                   MOVEL     *BLANKS       DESCCA7
177300160506     C                   clear                   v7dcca
177400160610     C                   clear                   ds7o
177500160610     C                   clear                   w_ccaleg
177600161014     C                   clear                   w_orig
177700930113     C     ARBCCA        IFNE      ' '
177800930705     C                   Z-ADD     1             C
177900930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
178000160505     C   31              MOVEL     DCA(C)        ALF20            20
178100160505     C  N31              MOVEL     *BLANKS       ALF20
178200160610     C   31              MOVEL     dca(c)        ds7o
178300160505     c                   eval      descca ='- ' + %subst(alf20:1:12)
178400160505     c                   eval      descca7=arbcca+'-'+alf20
178500160506     c                   eval      v7dcca='C.Anomala'
178600160610     c                   if        §7otle<>*blanks
178700161014     c     karb2         chain     fnlbl02l
178800161014     c                   if        %found(fnlbl02l)
178900160610     c                   eval      w_ccaleg='S'
179000160614     c                   seton                                        84
179100161014     c                   if        LBLAAO=LBLAAP and
179200161014     c                             LBLLPO=LBLLPP and
179300161014     c                             LBLNRO=LBLNRP and
179400161014     c                             LBLNSO=LBLNSP
179500161014     c                   eval      w_orig  ='S'
179600161014     c                   endif
179700160610     c                   endif
179800160610     c                   endif
179900930113     C                   END
180000911022     C* GIRO DATA SPEDIZIONE
180100940712     C                   Z-ADD     ARBDSP        G02INV
180200940712     C                   Z-ADD     *ZEROS        G02DAT
180300940712     C                   MOVEL     '3'           G02ERR
180400940712     C                   CALL      'XSRDA8'
180500940712     C                   PARM                    WLBDAT
180600940719     C                   Z-ADD     G02DAT        V3CDSP
180700070403     c*
180800911022     C* DATA BORDERO'
180900070403     c                   if        arbdbr>0
181000070403     c     *iso          movel     arbdbr        dataeur
181100070403     c                   movel     dataeur       datadmy
181200070404     c                   movel     dataeur       w08dbr            8 0
181300070404     c                   movel     datadmy       v06dbr
181400070403     c                   else
181500070404     c                   clear                   v06dbr
181501170117     c                   clear                   w08dbr
181600070403     c                   endif
181700070404     c
181800070404     c* Prendo da fnblp i dati ritiro merce
181900150313     c***  karb2         chain     fnblp01l
182000150313     c***                if        %found(fnblp01l) and blpdrt>0
182100150313     c                   if        arbdrt >0
182200150313     c     *iso          movel     arbdrt        dataeur
182201161207     c                   monitor
182300070403     c                   movel     dataeur       datadmy
182400070404     c                   movel     datadmy       v06drt
182401161207     c                   on-error
182402161207     c                   clear                   v06drt
182403161207     c                   endmon
182500150313     c                   movel     arbnrt        v7cnrt
182600150313     c                   movel     arbctm        v3cctm
182700150313     c                   if        arbfpp='M'
182800070410     c                   eval      v7dfppr='MAT'
182900070404     c                   else
183000070410     c                   eval      v7dfppr='POM'
183100070404     c                   endif
183200150313     C                   MOVEL     arbPDR        V7CPDR
183300150313     C                   movel     arbPdr        kpdpdr
183400070404     C                   move      'A'           kpdtip
183500070404     C     kfapd         chain     fiapd01L
183600070404     c                   if        %found(fiapd01l)
183700070404     C                   MOVEL     APDRSC        V7DPDR
183800070404     c                   else
183900070404     C                   MOVEL     *BLANKS       V7DPDR
184000070404     c                   endif
184100070404     c
184200070404     c
184300070403     c                   else
184400070404     c                   clear                   v06drt
184500070404     c                   clear                   v7cnrt
184600070406     c                   clear                   v3cctm
184700070504     c                   clear                   v7dfppr
184800070404     c                   clear                   v7dpdr
184900070404     c                   clear                   v7cpdr
185000070403     c                   endif
185100070403     c
185200961112     C* SOLO PER LE BOLLE EXPORT VISUALIZZO LA DATA STAMPA MANIFEST
185300070404     C                   CLEAR                   V06DT3
185400070413     c                   if        *in16 and arbdt3>0  and arbdt3<>99999999
185500070404     c     *iso          movel     arbdt3        dataeur
185600070404     c                   movel     dataeur       datadmy
185700070404     c                   movel     datadmy       v06dt3
185800961112     C                   ENDIF
185900940713     C* DATA PARTENZA PRIMO COLLO
186000070404     c                   if        arbdpc>0
186100070404     c     *iso          movel     arbdpc        dataeur
186200070404     c                   movel     dataeur       datadmy
186300070404     c                   movel     datadmy       v06dpc
186400070404     c                   movel     dataeur       w08dpc
186500070404     c                   else
186600070404     c                   clear                   v06dpc
186700070404     c                   clear                   w08dpc            8 0
186800070404     c                   endif
186900940713     C* DATA PARTENZA ULTIMO COLLO
187000070403     C**                 Z-ADD     ARBDUC        G02INV
187100070403     C**                 Z-ADD     *ZEROS        G02DAT
187200070403     C**                 MOVEL     '3'           G02ERR
187300070403     C**                 CALL      'XSRDA8'
187400070403     C**                 PARM                    WLBDAT
187500070403     C**                 Z-ADD     G02DAT        V3CDUC
187600070403     c                   if        arbduc>0
187700070404     c     *iso          movel     arbduc        dataeur
187800070403     c                   movel     dataeur       datadmy
187900070404     c                   movel     datadmy       v06duc
188000070404     c                   movel     dataeur       w08duc
188100070403     c                   else
188200070404     c                   clear                   v06duc
188300070404     c                   clear                   w08duc            8 0
188400070403     c                   endif
188500911022     C* DATA ENTRATA AL TRANSITO
188600070404     c                   if        arbdet>0
188700070404     c     *iso          movel     arbdet        dataeur
188800070404     c                   movel     dataeur       datadmy
188900070404     c                   movel     datadmy       v06det
189000070404     c                   else
189100070404     c                   clear                   v06det
189200070404     c                   endif
189300911022     C* DATA USCITA DAL TRANSITO
189400070404     c                   if        arbdut>0
189500070404     c     *iso          movel     arbdut        dataeur
189600070404     c                   movel     dataeur       datadmy
189700070404     c                   movel     datadmy       v06dut
189800070404     c                   else
189900070404     c                   clear                   v06dut
190000070404     c                   endif
190100930113     C* DATA ARRIVO MERCE
190200070403     c                   if        arbdam>0
190300160506     c                   setoff                                       96
190400160914     c                   clear                   v7ddam
190500070403     c     *iso          movel     arbdam        dataeur
190600070403     c                   movel     dataeur       datadmy
190700070404     c                   movel     datadmy       v06dam
190800160914     c                   if        v7dlnaa<>*blanks
190900150616     c                   clear                   fnlrj6ds
191000150616     c                   eval      ILRJ6TRIC='A'
191100150616     c                   eval      ILRJ6AAS =arbaas
191200150616     c                   eval      ILRJ6LNP =arblnp
191300150616     c                   eval      ILRJ6NRS =arbnrs
191400150616     c                   eval      ILRJ6NSP =arbnsp
191500150616     c                   call      'FNLRJ6R'
191600150616     c                   parm                    fnlrj6ds
191700160517     c* Arrivo alla filiale finale di arrivo:
191800160517     c* Totale
191900150616     c                   if        olrj6err=*blanks and olrj6fall='S'
192000150616     c                   eval      v7ddam='Totale'
192100150616     c                   else
192200160517     c* Parziale
192300160517     c                   if        OLRJ6CDCM+OLRJ6CSPU>0
192400160517     c                   eval      v7ddam='Parziale'
192500160517     c                   else
192600160517     c* Nessuno
192700160517     c                   eval      v7ddam='Nessuno '
192800160517     c                   endif
192900150616     c                   endif
193000161110     c* data cosegna parziale
193100161110     c                   if        arbdcp>0
193200161110     c     *iso          move      arbdcp        datadmy
193300161110     c                   movel     datadmy       w0060
193400161110     c                   eval      %subst(v7drig1:1:22)='ConsParziale'       +
193500161110     c                             hi + %editW(w0060:'  /  /  ') + c_DspAtrNorm
193600161110     c* indico anche quanti colli consegnati sul totale dei colli della spedizione
193700161110     c                   if        arbdcm=0
193800161110     c* spazio di v7drig1 utilizzabile per indicare 'di colli'  --> 8 byte
193900161110     c                   eval      %subst(v7drig1:23:8)= 'di Colli'
194000161110     c* spazio di v7drig1 utilizzabile per indicare colli/colli --> 11 byte
194100161110     c* senza contare i due attributi iniziale (pos 31) e finale
194200161110     c                   eval      %subst(v7drig1:31:13)= hi
194300161110     c                             + %trim(%editc(OLRJ6CDCM:'Z'))
194400161110     c                             + '/'
194500161110     c                             + %trim(%editc(OLRJ6NCL:'Z'))
194600161110     c                             + c_DspAtrNorm
194700161110     c* Determino quanti spazi vuoti rimasti in fondo alla sottostringa del
194800161110     c* campo per poi riempirli con '-'
194900161110     c* Se la sottostringa è stata utilizzata tutta in
195000161110     c* posizione 43 c'è l'attributo di visualizzazione (c_DspAtrNorm)
195100161110     c* e quindi:
195200161110     c* - campo pos non sarà mai = 0
195300161110     c* - se pos = 43 in questo byte non metterò il trattino
195400161110     c                   eval      pos=%checkr(' ':%subst(v7drig1:1:43))
195500161110     c* considero quindi solo quando pos < 43
195600161110     c                   if        pos<43
195700161110     c                   eval      pos+=1
195800161110     c                   eval      %subst(v7drig1:pos:(43-pos+1))=*all'-'
195900161110     c                   endif
196000161110     c                   endif
196100161110     c                   endif
196200160914     c                   endif
196300070403     c                   else
196400160506     c                   seton                                        96
196500070404     c                   clear                   v06dam
196600070403     c                   endif
196700150615
196800150615
196900150615     c
197000131121
197100930504     C* DATA CONSEGNA MERCE REALE
197200160610     c                   if        arbdcm>0  and w_ccaleg=*blanks
197300070413     C                   SETON                                        69
197400161014     c****               eval      v7cdcon='REALE'
197500161017     c                   if        arbcca=*blanks and §3arbl<>'R'
197600161031     c                             and arbcbo<>'M '
197700161014     c                   eval      v7dcons='CONSEGNATA:'
197800161014     c                   eval      v6ddcm ='CONSEGNA'
197900161014     c                   eval      v5ddcm ='REALE '
198000161014     c                   else
198100161014     c                   eval      v7dcons='CHIUSA . .:'
198200161014     c                   eval      v5ddcm ='CHIUSA'
198300161017     c                   eval      v6ddcm ='CHIUSA'
198400161017     c                   seton                                        19
198500161014     c                   endif
198600070404     c     *iso          movel     arbdcm        dataeur
198700070404     c                   movel     dataeur       datadmy
198800070404     c                   movel     dataeur       v08dcm
198900070404     c                   movel     datadmy       v06dcm
199000161014     c                   if        arbhmc>0
199100161014     c                   eval      vidhmc=%editw(arbhmc: '0 :  ')
199200161014     c                   endif
199300070404     c                   else
199400070404     c                   clear                   v06dcm
199500070404     c                   clear                   v08dcm
199501170120     c                   clear                   v6ddcm
199502170120     c                   clear                   v5ddcm
199600161014     c* solo se originale
199700161014     c                   if        blp_arbdcm>0 and W_orig='S'
199800161018     c                   if        (arbcca<>'2' and arbcca<>'6') or
199900161018     c                             blp_arbdcm<>arbdcm
200000160914     C                   SETON                                        69
200100161014     c                   eval      v7dcons='CONSEGNATA:'
200200161014     c                   eval      v6ddcm ='CONSEGNA'
200300161014     c                   eval      v5ddcm ='REALE  '
200400160913     c     *iso          movel     blp_arbdcm    dataeur
200500160913     c                   movel     dataeur       datadmy
200600160913     c                   movel     dataeur       v08dcm
200700160913     c                   movel     datadmy       v06dcm
200800161014     c                   if        blp_arbhmc>0
200900161014     c                   eval      vidhmc=%editw(blp_arbhmc: '0 :  ')
201000161014     c                   endif
201100161018     c                   endif
201200161014     c                   else
201300161014     c                   if        w_orig<>'S' and arbdcm>0
201400161014     c                   eval      v6ddcm ='CHIUSA'
201500161014     c                   eval      v5ddcm ='CHIUSA'
201600161017     c                   seton                                        19
201700161014     c     *iso          movel     arbdcm        dataeur
201800161014     c                   movel     dataeur       datadmy
201900161014     c                   movel     dataeur       v08dcm
202000161014     c                   movel     datadmy       v06dcm
202100161014     c                   if        arbhmc>0
202200161014     c                   eval      vidhmc=%editw(arbhmc: '0 :  ')
202300161014     c                   endif
202400161014     c                   endif
202500160913     c                   endif
202600070404     c                   endif
202700131121
202800930505     C* DATA CONSEGNA PREVISTA ESTERNA/ORE DI RITARDO
202900070404     C                   Z-ADD     0             v08DCE
203000140113     C                   Z-ADD     0             amgDCE            8 0
203100150615     C*******            Z-ADD     0             v06dce
203200940711     C                   Z-ADD     0             VIDNRC
203300131121     C*******            SETON                                        68
203400131121     C                   MOVEL     ' '           D98TLA
203500150611     C                   MOVEL     w_tbo         D98TBO
203600131121     C                   MOVEL     ARBAAS        D98AAS
203700131121     C                   MOVEL     ARBLNP        D98LNP
203800131121     C                   MOVEL     ARBNRS        D98NRS
203900131121     C                   MOVEL     ARBNSP        D98NSP
204000131121     C*
204100131121     C                   CALL      'TNSD99R'
204200131121     C                   PARM                    TNSD99DS
204300140110     c
204400140110     c* Mi salvo data teorica consegna
204500140110     c                   eval      s_arbdei=d98dei
204600140110     c                   eval      s_arboei=d98oei
204700150729     c
204800150729     c* memorizzo ore trazione è ore consegna cliente da passare al trulorsr
204900150729     c                   eval      s_arbttc=d98ttc
205000150729     c                   eval      s_arbtcc=d98tcc
205100131121     C*
205200140110    1C     ARBDCE        IFEQ      0
205300131121    2c                   if        d98dce>0
205400140113     c                   eval      amgdce=d98dce
205500131121     c     *iso          movel     d98dce        dataeur
205600150615     c****               movel     dataeur       datadmy
205700131121     c                   movel     dataeur       v08dce
205800150615     c****               movel     datadmy       v06dce
205900131121   x2c                   else
206000131121     c                   clear                   v08dce
206100150615     c***                clear                   v06dce
206200131121    2c                   endif
206300131121     c
206400131121     C                   Z-ADD     D98NRC        VIDNRC
206500160523     c*
206600160523     c**n69              eval      vidhmc=%editw(d98hce: '0 :  ')
206700131121     C*
206800131121     C* SE COMODO = 1  E' STATA FATTA CALL PGM
206900131121   x1C                   ELSE
207000140113     c                   eval      amgdce=arbdce
207100070404     c     *iso          movel     arbdce        dataeur
207200150615     c***                movel     dataeur       datadmy
207300070404     c                   movel     dataeur       v08dce
207400150615     c***                movel     datadmy       v06dce
207500070404     c
207600940711     C                   Z-ADD     ARBNRC        VIDNRC
207700131121    1C                   END
207800131121     c
207900911023     C* DATA MANDATO INTROITO
208000940712     C                   Z-ADD     ARBDMI        G02INV
208100940712     C                   Z-ADD     *ZEROS        G02DAT
208200940712     C                   MOVEL     '3'           G02ERR
208300940712     C                   CALL      'XSRDA8'
208400940712     C                   PARM                    WLBDAT
208500940712     C                   Z-ADD     G02DAT        VIDDMI
208600930113     C* DATA PRIMA CONSEGNA PARZIALE
208700930113     C                   Z-ADD     0             VIDDCP
208800150615     C***                Z-ADD     0             VIDDCP6
208900930113     C     ARBDCP        IFGT      0
209000940712     C                   Z-ADD     ARBDCP        G02INV
209100940712     C                   Z-ADD     *ZEROS        G02DAT
209200940712     C                   MOVEL     '3'           G02ERR
209300940712     C                   CALL      'XSRDA8'
209400940712     C                   PARM                    WLBDAT
209500940712     C                   Z-ADD     G02DAT        VIDDCP
209600150615     c***  *iso          move      arbdcp        datadmy
209700150615     c***                movel     datadmy       viddcp6
209800150611     c
209900930113     C                   END
209901161122     c* Nella videata di riepilogo,
210000160505     c* Visualizzo data distinta e padroncino solo se numero distinta>0
210100160505     c                   clear                   vidddc
210101161122     c                   clear                   vidddc5
210200160505     c                   clear                   v7ddel
210300160505     c                   clear                   vidpdc
210301161122     c                   clear                   vidpdc5
210400160505     c                   clear                   v7daut
210500160505     C                   MOVEL     *BLANKS       DESPDC
210501161122     C                   MOVEL     *BLANKS       apdrsc_w
210600160505     C                   MOVEL     *BLANKS       DESPD1
210800160505     C* DATA DISTINTA -
210900940712     C                   Z-ADD     ARBDDC        G02INV
211000940712     C                   Z-ADD     *ZEROS        G02DAT
211100940712     C                   MOVEL     '3'           G02ERR
211200940712     C                   CALL      'XSRDA8'
211300940712     C                   PARM                    WLBDAT
211400161122     C                   Z-ADD     G02DAT        VIDDDC5
211401161122     C* CODICE PADRONCINO CONSEGNA
211402161122     C                   MOVEL     ARBPDC        VIDPDC5
211403161122     C     ARBPDC        IFGT      0
211404161122     C                   movel     arbpdc        kpdpdr
211405161122     C                   move      'A'           kpdtip
211406161122     C     kfapd         chain     fiapd01L                           31
211407161122     C  N31              MOVEL     APDRSC        DESPDC
211408161122     C  N31              MOVEL     APDRSC        apdrsc_w
211409161122     C                   END
211410161122     c                   if        arbndc>0
211411161122     c                   eval      vidddc=vidddc5
211500160505     c                   if        vidddc>0
211600160505     c                   eval      v7ddel='del'
211700160505     c                   endif
211800911022     C* CODICE PADRONCINO CONSEGNA
211900940712     C                   MOVEL     ARBPDC        VIDPDC
212000160505     c                   if        vidpdc>0
212100160505     c                   eval      v7daut='Autotr'
212200160505     c                   endif
212201161122     C                   MOVEL     APDRSC_w      DESPD1
212300161122     C*    ARBPDC        IFGT      0
212400161122     C*                  movel     arbpdc        kpdpdr
212500161122     C*                  move      'A'           kpdtip
212600161122     C*    kfapd         chain     fiapd01L                           31
212700161122     C* N31              MOVEL     APDRSC        DESPDC
212800161122     C* N31              MOVEL     APDRSC        DESPD1
212900161122     C*                  END
213000160505     c                   endif
213100950324     C* PRESTAZIONE PADRONCINO MATTINO O POMERIGGIO
213200950324     C                   MOVE      *BLANKS       FVVFPP
213300131120     C***                MOVE      *BLANKS       V5DFPP
213400070404     C                   MOVE      *BLANKS       V7DFPP
213500950327     C                   Z-ADD     ARBNDC        WNDC
213600150617     c                   if        w_tbo='A'
213700020906     c                   Move      Arbifp        Wfgs
213800020906     c     Kfvv1         Chain     Fnfvv01l                           31
213900950324     C                   SELECT
214000991027     C                   WHEN      *IN31 = *on
214100991027     C                   WHEN      FVVFPP = 'M'
214200131120     C**                 eval      V5DFPP = 'MATTINO'
214300070504     C                   eval      V7DFPP = 'Mat'
214400991027     C                   WHEN      FVVFPP = 'P'
214500131120     C**                 eval      V5DFPP = 'POMERIGGIO'
214600070504     C                   eval      V7DFPP = 'Pom'
214700950324     C                   ENDSL
214800131119     c* controllo se ha PDA
214900131119     C                   Z-ADD     ARBNDC        kNDC6
215000131120     c                   clear                   vidpda
215100131120     c     Kdst          Chain     Fidst01l
215200131120     c                   if        %found(fidst01l)
215300131119     C                   movel     dstflr        DDStflr
215400131120     c                   if        (§dsttstpda= 'C' or  §dsttstpda= 'E') or
215500131120     c                             (dstpda<>'C' and  dstpda<>'E')
215600131120     c                   eval      vidpda='NO PDA'
215700150618     c*****              movel     ri            ATRpda
215800131121     c                   else
215900131121     c                   eval      vidpda='SI PDA'
216000150618     c****               movel     ri            ATRpda
216100131119     c                   endif
216200131119     c                   endif
216300150617     c                   endif
216400131108     c
216500131108     c* Se bolla in distinta, verifico quanti colli
216600131108     c                   clear                   v7dncl_co
216700131115     c                   clear                   v7dncl_de
216800131108     c                   clear                   wcol_D
216900131108     c                   clear                   wcol_I
217000131108    1c                   if        arbndc>0 and arbddc>0 and arbfbc=' '
217100150616     c                             and w_tbo='A'
217200131108     c* se ritorno all'incasso lo scrivo
217300131108     c
217400150616    2c                   if        arbdcm>0  and (arbicc='R' or arbicc=' ' or
217500131108     c                                  arbica=' ' or arbica='R')
217600160721     c                             and dstfcf<>'S'
217700131115     c                   eval      v7dncl_de='     IN consegna :'
217800131108     c                   eval      v7dncl_co='PER INCASSO'
217900131108    2c                   endif
218000131108     c
218100131108     c* Se colli da consegnare
218200131108    2c                   if        arbdcm=0
218300131108     C                   Z-ADD     ARBNDC        knfv
218400131108     c                   Move      Arbifp        Wfgs
218500131108     c
218600131108     c     karb2         setll     fnart01l
218700131108     c     karb2         reade     fnart01l
218800131108     c
218900131108    3c                   dow       not %eof(fnart01l)
219000131108     c                   if        artdcm=0
219100131108     c                   add       1             wcol_d
219200131108     c     kbrv          chain     fnbrv01l
219300131108     c                   if        %found(fnbrv01l)
219400131108     c                   add       1             wcol_I
219500131108     c                   endif
219600131108     c                   endif
219700131108     c
219800131108     c
219900131108     c     karb2         reade     fnart01l
220000131108    3c                   enddo
220100131108
220200131108    3c                   if        wcol_d>0
220300131115     c                   eval      v7dncl_de='Colli in consegna:'
220400131108    4c                   if        wcol_i=0
220500131108     c                   eval      v7dncl_co='0/'+%trim(%editc(wcol_d:'Z'))
220600131108     c                   else
220700131108     c                   eval      v7dncl_co=%trim(%editc(wcol_I:'Z')) +'/' +
220800131108     c                                       %trim(%editc(wcol_d:'Z'))
220900131108    4c                   endif
221000131108    3c                   endif
221100131108    c
221200131108    2c                   endif
221300131108    1c                   endif
221400131108     c
221500131108     c* Visualizzo ultimo evento
221600131108     c                   clear                   v7duevb
221700150616     c                   clear                   w_ar4_1
221800160614     c                   clear                   w_evbcev
221900161107     c                   clear                   w_tcev
222000160610     c* Se bolla legata con consegna anomala com ultimo evento devo visualizzare
222100160610     c* data e ora in cui è avvenuto il dirottamento
222200161017    1c                   if        w_ccaleg='S'
222300161017    2c                   if        arbcca='9' or w_tbo='P'
222400160913     c* Per cambio di porto la data/ora coincide con la data/ora di creazione della bolla
222500160913     c* figlia
222600160913     c* idem per resi e dirottamenti che si trovano solo su BLP
222700160913     c* In questi casi la DCM contiene infatti la data di consegna della merce e non la data in
222800160913     c* cui è stato fatto il cambio di porto o dirottamento o reso
222900160913     c     karb2         setll     fnlbl02l
223000160913     c     karb2         reade     fnlbl02l
223100161017    3c                   if        not %eof(fnlbl02l)
223200160913     c                   Z-Add     lblAAN        Kaas
223300160913     c                   Z-Add     lblLPN        Klnp
223400160913     c                   Z-Add     lblNRN        Knrs
223500160913     c                   Z-Add     lblNSN        Knsp
223600160913     c                   Eval      Ktrd = 'GEN'
223700160913     c     kAr51         Chain     Fiar501l
223800161017If  4c                   If        %Found(Fiar501l)
223900160913     c     *iso          movel     ar5dac        dataeur
224000160913     c                   movel     dataeur       w0080
224100160913     c                   movel     ar5orc        w0040
224200160913     c                   eval      v7duevb=%trim(§7odei)+' il ' +
224300160913     c                             %editw(w0080:'  /  /    ')  + ' '
224400160913     c                             + %editw(w0040: '0 :  ')
224500161017    4C                   ENDIF
224600161017    3c                   endif
224700160913     c
224800161017   x2c                   else
224900161017    3c                   if        arbdcm>0
225000160614     c     *iso          movel     arbdcm        dataeur
225100160614     c                   movel     dataeur       w08dcm
225200161017     c**                 if        arbhmc>0
225300161017     c***                eval      vidhmc=%editw(arbhmc: '0 :  ')
225400161017     c***                endif
225500160610     c                   eval      v7duevb=%trim(§7odei)+' il ' +
225600161017     c                             %editw(w08dcm:'  /  /    ')
225700161017    4c                   if        arbhmc>0
225800161017     c                   eval      v7duevb=%trim(v7duevb) + ' ' +
225900161017     c                             %editw(arbhmc: '0 :  ')
226000161017    4c                   endif
226100161017     c***                          %editw(w08dcm:'  /  /    ')  + ' ' + vidhmc
226200161017   x3c                   else
226300160913     c                   eval      v7duevb=%trim(§7odei)
226400161017    3c                   endif
226500161017    2c                   endif
226600161017   x1c                   else
226700131114     c* prima vedo se c'e' da PDA
226800131114     c                   move      arbndc        pctndc
226900140224     c                   move      arbifp        pctfgs
227000131114     c                   move      'CET'         ttrd
227100131114     c     kcet          setgt     fipct02l
227200131114     c     kcet          readpe    fipct02l
227300161017    2c                   if        not %eof(fipct02l)
227400131114     c                   movel     pctdati       fipctcetds
227500131114     c* consegnata
227600161017    3c                   if        §pctcmc = ' '
227700131114     c                   MOVE      '  C'         evbcev                         *causale evento
227800161017   x3c                   else
227900131114     c* mancata consegna
228000131114     c                   movel     §PCTcmc       evbcev
228100161017    3c                   endif
228200131114     c                   z-add     §pctdata      evbdev
228300131114     c                   movel     §pctora       evbhev
228400131114
228500131114     c                   exsr      Decodi_EVB
228600160609     c                   eval      v7duevb=%trim(v7duevb)+' ' +
228700160609     c                                                 'da PDA'
228800161108     c                   eval      wcev=§pctriserv
228900161108     c                   exsr      sr_2a
229000161108     c                   if        §2atip='R'
229100161108     c                   eval      v7duevb=%trim(v7duevb)+  red
229200161108     c                             + %trim(§2ades) + c_DspAtrNorm
229300161108     c                   endif
229400161107     c                   eval      w_tcev='D'
229500131114
229600161017   x2c                   else
229700150616     c* se spedizione consegnata e presente firmatario lo visulizzo
229800150616     c* al posto dell'ultimo evento
229900161017    3c                   if        arbdcm>0
230000150616     C                   MOVEL     ARBAAS        KAAS
230100150616     C                   MOVEL     ARBLNP        KLNP
230200150616     C                   MOVEL     ARBNRS        KNRS
230300150616     C                   MOVEL     ARBNSP        KNSP
230400150616     C                   MOVEL     '1'           KTRC
230500150616     c     kar41         chain     fiar401l
230600161017    4c                   if        %found(fiar401l) and ar4not<>*blanks
230700150616     c                   eval      evbcev='  C'
230800150616     c                   clear                   evbdev
230900150616     c                   clear                   evbhev
231000150616     c                   eval      w_ar4_1='S'
231100150616     c                   exsr      Decodi_EVB
231200161017    4c                   endif
231300161017    3c                   endif
231400131114     c*
231500161108    3c****               if        w_ar4_1=*blanks
231600131108     C                   Z-ADD     ARBAAS        KAAS$
231700131108     C                   Z-ADD     ARBLNP        KLNP
231800131108     C                   Z-ADD     ARBNRS        KNRS
231900131108     C                   Z-ADD     ARBNSP        KNSP
232000131108     C     KEVB1         SETgt     FNEVB01L
232100131108     C     KEVB1         readpe    FNEVB01L
232200131108     c
232300161017    4c                   if        not %eof(fnevb01l)
232400161108    5c                   if        w_ar4_1=*blanks
232500131114     c                   exsr      Decodi_EVB
232600161108   x5c                   else
232700161108     c* Se spedizione consegnata e ultimo evento di riserva visualizzo di fianco alla Firma
232800161108     c                   eval      wcev=evbcev
232900161108     c                   exsr      sr_2a
233000161108     c                   if        §2atip='R'
233100161108     c                   eval      v7duevb=%trim(v7duevb)+  red
233200161108     c                             + %trim(§2ades) + c_DspAtrNorm
233300161108     c                   endif
233400161108    5c                   endif
233500161017    4c                   endif
233600161108    3c****               endif
233700161017    2c                   endif
233800161017    1c                   endif
233900160505     c                   clear                   v7duevd
234000160506     c                   if        v7duevb<>*blanks
234100160505     c                   eval      v7duevd='Ultimo evento'
234200160505     c                   endif
234300911022     C*
234400911022     C* SE ESISTE AGGANCIO 2° BOLLA
234500991026     C                   Z-ADD     ARBAAS        KAAS
234600991026     C                   Z-ADD     ARBLNP        KLNP
234700991026     C                   Z-ADD     ARBNRS        KNRS
234800991026     C                   Z-ADD     ARBNSP        KNSP
234900991026     C                   MOVEL     '2'           KTRC
235000991027     C     KAR61         CHAIN     FIAR6000                           31
235100911023     C*
235200040216if  1C                   IF          %FOUND
235300991027     C                   eval      *in22 = *on
235400991027     C                   MOVEL     AR6CEI        V9CCEI
235500991027     C                   MOVEL     AR6NFT        V9CNFT
235600991027     C                   MOVEL     AR6FIV        V9CFIV
235700991027      *
235800991027     C                   eval      V9CDIV = AR6div
235900991027     C                   eval      WDIV = AR6div
236000991027     C                   exsr      CTR_VALUTA
236100991027      *
236200001128     C                   clear                   v9dfim
236300001128     C                   clear                   v9dfi2
236400991027     c                   if        AR6fim = 'P'
236500991027     c                   eval      V9Dfim = 'PARTENZA'
236600040126     c                   eval      V9Dfi2 = 'Immesse in'
236700991027     c                   endif
236800991027      * Varia G
236900040130     c                   clear                   W0133
237000040130     c                   clear                   v9csv1
237100040130     c                   clear                   v9cva1
237200040130     c                   clear                   v9csv2
237300040130     c                   clear                   v9cva2
237400040130     c                   clear                   v9csv3
237500040130     c                   clear                   v9cva3
237600040130     c                   clear                   v9csv4
237700040130     c                   clear                   v9cva4
237800041130     c                   clear                   v9csv5
237900041130     c                   clear                   v9cva5
238000041130     c                   clear                   v9csv6
238100041130     c                   clear                   v9cva6
238200040130     c                   clear                   v9civa
238300040130
238400040122      * Carico le varie
238500040122     c                   Movel     Ar6Sv1        wsvn
238600040122     c                   Z-add     Ar6Va1        wvan
238700040122     c                   ExSr      Car_Varie1
238800040122
238900040122     c                   Movel     Ar6Sv2        wsvn
239000040122     c                   Z-add     Ar6Va2        wvan
239100040122     c                   ExSr      Car_Varie1
239200040122
239300040122     c                   Movel     Ar6Sv3        wsvn
239400040122     c                   Z-add     Ar6Va3        wvan
239500040122     c                   ExSr      Car_Varie1
239600040122
239700040122     c     Kar61         Setll     Fiar7000
239800040216do  2c                   Do        *Hival
239900040122     c     Kar61         Reade     Fiar7000
240000040122     c                   If        %Eof(Fiar701l)
240100040122     c                   Leave
240200040122     c                   EndIf
240300040122     c                   Movel     Ar7Svn        wsvn
240400040122     c                   Z-add     Ar7Van        wvan
240500040216     c                   ExSr      Car_Varie1
240600040216e   2c                   EndDo
240700040122
240800991027      * TOTALE FATTURA
240900991027     c                   clear                   W0133
241000991027     c                   IF          AR6ift > *zeros
241100991027     c                   z-add     AR6ift        W0133
241200991027     c                   eval      Wtstdec = §cvfdc
241300991027     c                   exsr      EDTimporti
241400991027     c                   movel     IMPeditato    V9cift
241500001128     C                   else
241600001128     C                   clear                   v9cift
241700991027     c                   endif
241800991027      *
241900991027     c                   If         AR6dft > *zeros
242000991027     C     *iso          movel     AR6DFT        DATA_EUR
242100991027     C     *eur          movel     data_eur      W0080
242200991027     C                   eval      V9cdft = %EDITC(W0080:'Y')
242300001128     C                   else
242400001128     C                   clear                   v9cdft
242500991027     c                   Endif
242600991027      *
242700991027     c                   eval      V9Cksc = AR6ksc
242800991027     c                   eval      V9Cctr = AR6ctr
242900991027     c                   move      v9cksc        W0040
243000991027     c                   If        W0040 = 9999
243100991027     c                   eval      V9Dksc = arbrsd
243200991027     c                   Else
243300991027     c                   eval      i69kac = V9Cksc
243400991027     c                   exsr      CTR_ANAGRA
243500991027     c                   eval      V9Dksc = acorag
243600991027     c                   Endif
243700991027      *
243800991027     C                   eval      COD = 'TR'
243900991027     C                   movel     V9CCTR        UNOCTR            1
244000991027     C                   eval      KEY = unoctr
244100991027     C     KTAB1         CHAIN     TABEL00F                           31
244200991027     C                   movel     TBLUNI        V9DCTR
244300991027      *
244400930114     C* ESENZIONE IVA
244500001128     C                   clear                   v9dcei
244600991111     C     AR6CEI        IFNE      *blanks
244700940721     C                   MOVEL     'EI'          COD
244800991111     C                   MOVEL(P)  AR6CEI        KEY
244900940719     C     KTAB1         CHAIN     TABEL00F                           31
245000991027     C  N31              MOVEL     TBLUNI        V9DCEI
245100940719     C                   ENDIF
245200991027      *
245300040216e   1C                   ENDIF
245400150611
245500150611     c* RICERCA E DECODIFICA STATO SPEDIZIONE
245600150611     c                   clear                   v7dst1
245700150611     c                   clear                   v7dst2
245800150616     c                   clear                   v7dst3
245900150616     c                   setoff                                       58
246000160719    1c***                if        not *in69 and w_ccaleg=*blanks
246100160719    1c                   if        w_ccaleg=*blanks
246200150611     c                   clear                   fnlru3ds
246300150611     c                   eval      ILRU3AAS=arbaas
246400150611     c                   eval      ILRU3LNP=arblnp
246500150611     c                   eval      ILRU3NRS=arbnrs
246600150611     c                   eval      ILRU3NSP=arbnsp
246700150611     c                   eval      ILRU3LNA=arblna
246800150611     c                   eval      ILRU3NTC=arbntc
246900150611     c                   eval      ILRU3TCR=arbtcr
247000150611     c                   eval      ILRU3DCR=arbdcr
247100150611     c                   eval      ILRU3HCR=arbhcr
247200160506     c                   if        *in16
247300160506     c                   eval      ILRU3dip=arbdt3
247400160506     c                   endif
247500160614     c                   eval      ILRU3ucev=w_evbcev
247600161107     c                   eval      ILRU3ITCEV=w_tcev
247700160719     c                   eval      ILRU3ica =arbica
247800160719     c                   eval      ILRU3icc =arbicc
247900160825     c                   if        w_tbo='P' and arbdcm>0
248000160825     c                   eval      ILRU3ica ='S'
248100160825     c                   eval      ILRU3icc ='S'
248200160825     c                   endif
248300161027     c                   eval      ilru3ca=StatoCa
248400150611
248500150611     c                   eval      w_video='N'
248600150611     c                   exsr      sr_orarisr
248700150612     c                   call      'FNLRU3R'
248800150611     c                   parm                    kpjba
248900150611     c                   parm                    fnlru3ds
249000150611     c                   parm                    tnsd99ds
249100150611     c                   parm                    trulor2ds
249200150612     c* valorizzazione PRIMA RIGA del video
249300150611     c                   eval      v7dst1=%trim(%xlate(mi:ma:olru3dec1)) + ' ' +
249400150611     c                                          %trim(olru3not1)
249500150612     c* calcolo da che posizione inzizia la parte "note" all'interno della stringa
249600150611     c* che metto a video per poter incolonnare la parte "note" della seconda riga
249700150611     c                   clear                   pos
249800150611     c                   eval      pos=%checkr(' ':olru3dec1)
249900150612     c                   eval      pos+=2
250000150612     c* Verifico se la parte note della prima riga è stata troncata
250100150612
250200150612     c* Lunghezza utile della stringa 1 per il campo not1
250300150612     c                   eval      lennt1v=%len(v7dst1)-pos
250400150612     c* Lunghezza effettiva del campo not1 ricevuto
250500150611     c                   eval      lennt1 =%checkr(' ':olru3not1)
250600150612    2c                   if        lennt1>lennt1v
250700150612     c                   clear                   PosA
250800150612     c                   eval      PosA=%checkr(charok:v7dst1)
250900150612     c                   if        PosA<%len(v7dst1)
251000150612     c* clearo parola troncata per andare a capo nella seconda riga
251100150612     c                   eval      %subst(v7dst1:posA+1)=*blanks
251200150612     c                   endif
251300150612    2c                   endif
251400150611
251500150612     c* valorizzazione SECONDA RIGA del video
251600150612     c                   eval      v7dst2=%trim(olru3dec2)+ ' '
251700150612     c* imposto eventuale a capo della prima riga
251800150612    2c                   if        lennt1>lennt1v
251900150612     c                   eval      %subst(v7dst2:pos)=
252000150612     c                             %trim(%subst(olru3not1:PosA+2-pos))
252100150612    2c                   endif
252200150612    2c                   if        olru3not2<>*blanks
252300150612     c* la seconda riga contiene dati della prima riga --> scrivo le note 2 nella
252400150612     c* terza riga
252500150612    3c                   if        lennt1>lennt1v
252600150612     c*****              eval      v7dst2=%trim(v7dst2)+' ' + %trim(olru3not2)
252700150612     c                   eval      %subst(v7dst3:pos)=
252800150612     c                             %trim(olru3not2)
252900150616     c                   seton                                        58
253000150612
253100150612   x3c                   else
253200150612     c* altrimenti valorizzo la seconda riga con le note2 partendo da pos
253300150612     c* per incolonnarmi con la prima riga
253400161028     c* se la prima riga è vuota le note2 le accodo (caso di sped. consengata con CA)
253500161028     c                   if        v7dst1=*blanks
253600161028     c                   eval      v7dst2=%trim(v7dst2)+' '+%trim(olru3not2)
253700161028     c                   else
253800150612     c                   eval      %subst(v7dst2:pos)=
253900150612     c                             %trim(olru3not2)
254000161028     c                   endif
254100150612     c* Verifico se la parte note della seconda riga è stata troncata
254200150612
254300150612     c* Lunghezza utile della stringa 1 per il campo not1
254400150612     c                   eval      lennt1v=%len(v7dst2)-pos
254500150612     c* Lunghezza effettiva del campo not1 ricevuto
254600150612     c                   eval      lennt1 =%checkr(' ':olru3not2)
254700150612    2c                   if        lennt1>lennt1v
254800150612     c                   clear                   PosA
254900150612     c                   eval      PosA=%checkr(charok:v7dst2)
255000150612     c                   if        PosA<%len(v7dst2)
255100150612     c* clearo parola troncata per andare a capo nella terza   riga
255200150612     c                   eval      %subst(v7dst2:posA+1)=*blanks
255300150612     c                   endif
255400150612    2c                   endif
255500150612     c* valorizzazione TERZA RIGA del video quando vado a capo con la seconda
255600150612     c* imposto eventuale a capo della prima riga
255700150612    2c                   if        lennt1>lennt1v
255800150612     c                   eval      %subst(v7dst3:pos)=
255900150612     c                             %trim(%subst(olru3not2:PosA+2-pos))
256000150616     c                   seton                                        58
256100150612    2c                   endif
256200150612    3c                   endif
256300150612    2c                   endif
256400161027     c                   if        (olru3spc='CON' or olru3spc='CAN')
256500161027     c                             and arbdcm=0
256600160609     c                   eval      *in69=*on
256700160609     c                   eval      v08dcm=w7cdev
256800161017     c                   eval      v5ddcm ='REALE  '
256900161017     c                   eval      v6ddcm ='CONSEGNA'
257000160609     c                   movel     w7cdev        dataeur
257100160609     c                   movel     dataeur       datadmy
257200160609     c                   movel     datadmy       v06dcm
257300160609     c                   eval      vidhmc=%editw(evbhev: '0 :  ')
257400160609     c*****              eval      v7dcca='da PDA'
257500161014     c                   eval      v7dcons='CONSEGNATA:'
257600160609     c                   endif
257700150612    1c                   endif
257800150616     c
257900150616     c                   if        v7dst3=*blanks and
258000150616     c                             ((v7dst1<>*blanks or v7dst2<>*blanks) or
258100160523     c                               *in69)
258200150616     c                   eval      v7dst3=*all'-'
258300150616     c                   endif
258400150616
258500150616     c                   setoff                                       54
258600150616     c                   if        arbffd='S'
258700150616     c                   eval      %subst(v7dst3:68)=' Filiale ?'
258800150616     c                   seton                                        54
258900161031     c                   if        v7dst2=*blanks and *in69
259000161031     c                   eval      v7dst2='IN FERMO DEPOSITO'
259100161031     c                   endif
259200150616     c                   endif
259300930525     C*
259400991027     C* PER I DATI SULLA TASSAZIONE 1a BOLLA, DEVO CERCARE SU FIAR600F
259500940905     C                   SETOFF                                       24
259600110705     c                   clear                   atrbl4a
259700110705     c
259800110705     c                   clear                   v6car4ksc
259900110705     c                   clear                   v6dar4ksc
260000110705     c                   clear                   v6car4ctr
260100111021     c                   clear                   VIDimv
260200111021     c                   clear                   VIDpor
260300111021     c                   clear                   VIDift
260400111021     c                   clear                   VIDdiv
260500111021     c                   clear                   VIDfim
260600111021     C                   clear                   VIDCEI
260700111021     C                   clear                   VIDNFT
260800111021     C                   clear                   VIDFIV
260900111021     C                   clear                   DESCEI
261000111021     C                   clear                   viddft
261100111021     C                   clear                   vidIVA
261200111021      *
261300111021      * CERCO LE VARIE: LE PRIME 3 SU FIAR600F, EVENTUALI  ALTRE SU FIAR700F
261400111021     C                   eval      *in35 = *on
261500131107     C                   WRITE     LRI6C06
261600111021     C                   eval      *in35 = *off
261700111021      *
261800111021     c                   clear                   NR6
261900111021     c                   clear                   VIDiva
262000110705     c
262100940712     C                   Z-ADD     ARBAAS        KAAS
262200940712     C                   Z-ADD     ARBLNP        KLNP
262300940712     C                   Z-ADD     ARBNRS        KNRS
262400940712     C                   Z-ADD     ARBNSP        KNSP
262500110705     C                   MOVEL     'A'           KTRC
262600110705     C     KAR41         CHAIN     FiAR401L
262700110705if  1C                   IF        not %FOUND
262800110705     c                   eval      atrbl4a=nodi
262900110705     c                   else
263000110705     c                   movel     ar4not        dsbl4a
263100110705     c                   if        §b4aks>0
263200110705     C                   eval      *in24 = *on
263300110705     c                   eval      v6car4ksc=§b4aks
263400110705     c                   eval      i69kac = v6car4ksc
263500110705     c                   exsr      CTR_ANAGRA
263600110705     c                   eval      v6dar4ksc = acorag
263700110705     c                   if        §b4act<>*blanks
263800110705     c                   movel     §b4act        v6car4ctr
263900110705     c                   endif
264000110705     c* non c'e' cod cliente x tassaz.
264100110705     c                   else
264200110705     c                   eval      atrbl4a=nodi
264300110705     c                   endif
264400110705     c                   endif
264500110705     c
264600991026     C                   MOVEL     '1'           KTRC
264700991027     C     KAR61         CHAIN     FIAR6000
264800110705     c
264900110705     c                   if        %found(fiar601l)
265000110705     c
265100110705      * CLIENTE E TARIFFA FATTURAZIONE
265200110705     c                   IF        *IN33 = *ON
265300110705     c                   eval      v6cksc = arbksc
265400110705     c                   eval      v6cctr = arbctr
265500110705     c                   eval      v6dksc = arbrsm
265600110705     c                   ELSE
265700110705     c                   eval      v6cksc = AR6ksc
265800110705     c                   eval      v6cctr = AR6ctr
265900160825     c                   if        w_tbo='P'
266000160825     c                   eval      vidksc = AR6ksc
266100160825     c                   eval      vidctr = AR6ctr
266200160825     c                   endif
266300110705     c                   move      v6cksc        W0040
266400110705     c                   if        W0040 = 9999
266500110705     c                   eval      v6dksc = arbrsd
266600110705     c                   else
266700110705     c                   eval      i69kac = v6cksc
266800110705     c                   exsr      CTR_ANAGRA
266900110705     c                   eval      v6dksc = acorag
267000110705     c                   endif
267100110705     c                   ENDIF
267200110705      *   decodifica tariffa
267300110705     C                   eval      COD = 'TR'
267400110705     C                   MOVEL     V6CCTR        UNOCTR            1
267500110705     C                   MOVEL(P)  UNOCTR        KEY
267600110705     C     KTAB1         CHAIN     TABEL00F                           31
267700110705     C                   MOVEL     TBLUNI        V6DCTR
267800110705     c
267900110705     c                   endif
268000991027      *
268100040216if  1C                   IF        %FOUND  and
268200991027     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
268300991027     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
268400991027     C                              AR6sv3 <> *blanks)
268500991027      *
268600991027      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SE PRESENTE ALMENO UNO DEI CAMPI
268700991027     C                   EVAL      *IN24 = *ON
268800991027      *
268900991027     C                   eval      VIDDIV = AR6div
269000991027     C                   eval      WDIV = AR6div
269100991027     C                   exsr      CTR_VALUTA
269200991027      *
269300991027     c                   IF          ar6por > *zeros
269400991027     c                   z-add     AR6por        W0133
269500991027     c                   eval      Wtstdec = §cvfdc
269600991027     c                   exsr      EDTimporti
269700991027     c                   movel     IMPeditato    VIDpor
269800991128     c                   else
269900991128     c                   clear                   VIDpor
270000991027     c                   ENDIF
270100991027      *
270200991027     c                   IF          ar6imv > *zeros
270300991027     c                   z-add     AR6imv        W0133
270400991027     c                   eval      Wtstdec = §cvfdc
270500991027     c                   exsr      EDTimporti
270600991027     c                   movel     IMPeditato    VIDimv
270700991128     c                   else
270800991128     c                   clear                   VIDimv
270900991027     c                   ENDIF
271000991027      *
271100991027     c                   IF          ar6ift > *zeros
271200991027     c                   z-add     AR6ift        W0133
271300991027     c                   eval      Wtstdec = §cvfdc
271400991027     c                   exsr      EDTimporti
271500991027     c                   movel     IMPeditato    VIDift
271600991128     c                   else
271700991128     c                   clear                   VIDift
271800991027     c                   ENDIF
271900991027      *
272000991027     c                   IF        AR6fim = 'P'
272100991027     c                   eval      VIDfim = 'PARTENZA'
272200991128     c                   else
272300991128     c                   clear                   VIDfim
272400991027    2C                   ENDIF
272500991027      *
272600991027     C                   MOVEL     AR6CEI        VIDCEI
272700991027     C                   MOVEL     AR6NFT        VIDNFT
272800991027     C                   MOVEL     AR6FIV        VIDFIV
272900991027      *
273000991027     C                   IF        AR6CEI <>  *BLANKS
273100940808     C                   MOVEL     'EI'          COD
273200991027     C                   MOVEL(P)  AR6CEI        KEY
273300940808     C     KTAB1         CHAIN     TABEL00F                           31
273400940808     C  N31              MOVEL     TBLUNI        DESCEI
273500991128     c                   else
273600991128     c                   clear                   DESCEI
273700940808     C                   ENDIF
273800991027      *
273900991027      * DATA FATTURA DA GIRARE
274000991027     C                   IF         ar6dft > *zeros
274100991027     C     *iso          movel     AR6DFT        DATA_EUR
274200991027     C     *eur          movel     data_eur      W0080
274300991027     C                   eval      VIDdft = %EDITC(W0080:'Y')
274400991128     c                   else
274500991128     c                   clear                   VIDdft
274600991027     C                   ENDIF
274700991027      *
274800991027      *
274900991027     C                   MOVEL     ar6SV1        WSVN
275000991027     C                   MOVEL     ar6VA1        WVAN
275100991027     c                   exsr      car_varie
275200991027      *
275300991027     C                   MOVEL     ar6SV2        WSVN
275400991027     C                   MOVEL     ar6VA2        WVAN
275500991027     c                   exsr      car_varie
275600991027      *
275700991027     C                   MOVEL     ar6SV3        WSVN
275800991027     C                   MOVEL     ar6VA3        WVAN
275900991027     c                   exsr      car_varie
276000991027      *
276100991027      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
276200040216if  2C                   if          ar6SV3 <> *blanks
276300991027     C     KAR61         SETLL     FIAR7000
276400991027     C     KAR61         READE     FIAR7000
276500040216do  3C                   DOW       %EOF = *OFF
276600991027     C                   MOVEL     AR7SVN        WSVN
276700991027     C                   MOVEL     AR7VAN        WVAN
276800991027     c                   exsr      car_varie
276900991027     C     KAR61         READE     FIAR7000
277000040216e   3C                   ENDDO
277100040216e   2C                   endif
277200110705e   1C                   ENDIF
277300991027      *
277400991027      * Se non ho caricato record ne carico uno vuoto
277500110705     c                   IF        NR6 = *ZEROS  and *in24
277600991027     C                   clear                   V6CDE1
277700991027     C                   clear                   V6CVA1
277800991027     C                   clear                   V6CSV1
277900991027     C                   add       1             NR6
278000131107     C                   WRITE     LRI6S06
278100991027     c                   ENDIF
278200991027      *
278300991027      *
278400941206     C* 70 ON - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
278500941206     C     *IN33         IFEQ      '0'
278600941206     C     AR6NFT        ANDGT     0
278700941206     C     *IN22         OREQ      '1'
278800991111     C     AR6NFT        ANDGT     0
278900941206     C                   SETON                                        70
279000941206     C                   END
279100940712     C*
279200940719     C* PRELEVO LE NOTE DAL FILE APPOSITO
279300940719     C                   MOVEL     ARBAAS        KAAS
279400940719     C                   MOVEL     ARBLNP        KLNP
279500940719     C                   MOVEL     ARBNRS        KNRS
279600940719     C                   MOVEL     ARBNSP        KNSP
279700940719     C                   MOVEL     '8'           KTRC
279800060216     C     KAR41         CHAIN     FiAR401L                           31
279900940719     C  N31              MOVEL     AR4NOT        V4CNO1
280000940802     C   31              MOVEL     *BLANKS       V4CNO1
280100940719     C                   MOVEL     '9'           KTRC
280200060216     C     KAR41         CHAIN     FiAR401L                           31
280300940719     C  N31              MOVEL     AR4NOT        V4CNO2
280400940802     C   31              MOVEL     *BLANKS       V4CNO2
280500940719     C*
280600941110     C* ESTENSIONE RAGIONE SOCIALE DESTINATARIO
280700941110     C                   MOVEL     'D'           KTRC
280800060216     C     KAR41         CHAIN     FiAR401L                           31
280900950125     C  N31              MOVEL     AR4NOT        V4CNOT
281000950125     C   31              MOVEL     *BLANKS       V4CNOT
281100971015     C*
281200971015     C* SOLO PER EXPORT DATA DI STAMPA SU BORDERO' ARRIVI
281300070404     C***  *IN16         IFEQ      *ON
281400070404     C***                MOVEL     'A'           KTRC
281500070404     C***  KAR41         CHAIN     FiAR401L                           31
281600070404     C**N31              MOVE      AR4NOT        WDATA             8
281700070404     C***31              MOVEL     *ZEROS        WDATA
281800070404     C***  WDATA         IFGT      *ZEROS
281900070404     C***                MOVEL     WDATA         G02INV
282000070404     C***                Z-ADD     *ZEROS        G02DAT
282100070404     C***                MOVEL     '3'           G02ERR
282200070404     C***                CALL      'XSRDA8'
282300070404     C***                PARM                    WLBDAT
282400070404     C***                Z-ADD     G02DAT        V3CDST
282500070404     C***                ENDIF
282600070404     C***                ENDIF
282700940720     C*
282800051114     C* IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU FiAR900F
282900940720     C* CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
283000940720     C                   MOVEL     *BLANKS       VIDTIC
283100940720     C                   MOVEL     *ZERO         VIDCAS
283200940720     C                   MOVEL     *BLANKS       VIDVCA
283300940720     C                   MOVEL     *BLANKS       V4CGCA
283400940923     C                   MOVEL     *BLANKS       V4IGCA
283500091214     c                   setoff                                       65
283600940720     C     §3AFCA        IFGT      *ZERO
283700051128     C     Ksped         CHAIN     FiAR901L                           31
283800940720     C  N31              DO
283900940720     C                   MOVEL     AR9TIC        VIDTIC
284000940720     C                   MOVEL     AR9CAS        VIDCAS
284100940720     C                   MOVEL     AR9VCA        VIDVCA
284200940720     C                   MOVEL     AR9GCA        V4CGCA
284300091214     c                   seton                                        65
284400940720     C                   ENDDO
284500940720     C                   ENDIF
284600941215     C* ESISTE CONTRASSEGNO: VEDO SE ABILITATO
284700941215     C     VIDCAS        IFNE      0
284800941215     C                   SETON                                        29
284900941215     C                   END
285000940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
285100940923     C     V4CGCA        COMP      *BLANKS                            4141
285200940720     C*
285300940720     C* RIEMPO ANCHE ALCUNI CAMPI PER LE DS DI PASSAGGIO PARAMETRI
285400940720     C                   MOVEL     V3CDSP        PA1DSP
285500940720     C                   MOVEL     ARBRMN        PA1RMN
285600940720     C                   MOVEL     ARBLNA        PA1LNA
285700940802     C                   MOVEL     V3DLNA        PA1LN1
285800940720     C                   MOVEL     ARBRSD        PA1RSD
285900160504     C                   MOVEL     V2CAAS        pa1AAS
286000160504     C                   MOVEL     COMLNP        pa1LNP
286100160504     C                   MOVEL     V2CNRS        pa1NRS
286200160504     C                   MOVEL     V2CNSP        pa1NSP
286300160505     C                   MOVEL     'FNLRU6R1'    pa1pgm
286400160505     C                   MOVEL     RSUT          PA1DSA
286500160505     C                   MOVEL     pargia        PA1gia
286600160505     C                   MOVEL     SIMFEL        PA1FEL
286700940906     C*-------------------------------------------------------------------------
286800991029      * GESTIONE INCASSO PARZIALE se assegnato o presente estensione bolla
286900991102     C                   clear                   v5cdiv
287000991102     c                   clear                   Wariimp
287100991102     c                   clear                   Wariipp
287200991102     c                   clear                   VIDimp
287300991102     c                   clear                   VIDipp
287400991102     C                   MOVEL     ARBFIP        VIDFIP
287500991102      *
287600991029     c  N33              eval      V5Cdiv = viddiv
287700991029     c   22              eval      V5Cdiv = v9cdiv
287800991029      *
287900040216if  1c                   if        V5CDIV <> *blanks
288000991029      *
288100991029     C                   eval      WDIV = V5Cdiv
288200991029     C                   exsr      CTR_VALUTA
288300991029      *
288400991029     C                   MOVEL     ARBAAS        KAAS
288500991029     C                   MOVEL     ARBLNP        KLNP
288600991029     C                   MOVEL     ARBNRS        KNRS
288700991029     C                   MOVEL     ARBNSP        KNSP
288800991029     C                   MOVEL     'A'           ktrc
288900991029      *
289000991029     c     kar61         setll     fiari000
289100991029     c     kar61         reade     fiari000
289200991029      *
289300040216do  2c                   DOW       not %EOF
289400040216if  3c                   if        v5cdiv = aridiv
289500991029     c                   add       ARIimp        Wariimp
289600991029     c                   add       ARIipp        Wariipp
289700040216x   3c                   else
289800991029      *
289900991029     C                   clear                   yeurcods
290000991029     C                   eval      YECDVI = aridiv
290100991029     C                   eval      YECDVO = V5Cdiv
290200991029     C                   eval      YECIMI = ARIimp
290300011022     C                   move      §CVDEC        YECDCO
290400991029     C                   CALL      'YEURCO'
290500991029     C                   PARM                    YEURcods
290600991029    6C                   if        YECESI = *blanks
290700991029     C                   add       YECIMO        WARIimp
290800991029    6C                   endif
290900991029      *
291000991029     C                   clear                   yeurcods
291100991029     C                   eval      YECDVI = aridiv
291200991029     C                   eval      YECDVO = V5Cdiv
291300991029     C                   eval      YECIMI = ARIipp
291400011022     C                   move      §CVDEC        YECDCO
291500991029     C                   CALL      'YEURCO'
291600991029     C                   PARM                    YEURcods
291700991029    6C                   if        YECESI = *blanks
291800991029     C                   add       YECIMO        WARIipp
291900991029    6C                   endif
292000991029      *
292100040216e   3c                   endif
292200991029      *
292300991029     c     kar61         reade     fiari000
292400040216e   2c                   enddo
292500991029      *
292600991029     c                   IF        WARIipp > *zeros
292700991029     c                   z-add     Wariipp       W0133
292800991029     c                   eval      Wtstdec = §cvfdc
292900991029     c                   exsr      EDTimporti
293000991029     c                   movel     IMPeditato    VIDipp
293100991029     c                   ENDIF
293200991029      *
293300991029     c                   IF        WARIimp > *zeros
293400991029     c                   z-add     Wariimp       W0133
293500991029     c                   eval      Wtstdec = §cvfdc
293600991029     c                   exsr      EDTimporti
293700991029     c                   movel     IMPeditato    VIDimp
293800991029     c                   ENDIF
293900991105      *
294000991105     c                   IF        VIDipp = *blanks  and  VIDimp = *blanks
294100991105     c                   clear                   V5CDIV
294200991105     c                   ENDIF
294300991029      *
294400040216e   1C                   endif
294500991029     C*-------------------------------------------------------------------------
294600951220     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
294700951220     C*
294800951220     C* PULISCO IL SFL
294900960926     C                   SETOFF                                       72
295000131107     C                   WRITE     LRI6C05
295100960926     C                   SETON                                        7172
295200951220     C*
295300060216     C* CHIAVE PER ACCEDERE AL FILE FiAR401L
295400951220     C                   MOVEL     ARBAAS        KAAS
295500951220     C                   MOVEL     ARBLNP        KLNP
295600951220     C                   MOVEL     ARBNRS        KNRS
295700951220     C                   MOVEL     ARBNSP        KNSP
295800951220     C*
295900951220     C                   Z-ADD     0             NRR5              4 0
296000951220     C                   MOVEL     '7K'          COD
296100951220     C*
296200060216     C     KAR42         SETLL     FiAR401L
296300060216     C     KAR42         READE     FiAR401L                               36
296400040216do  1C     *IN36         DOWEQ     *OFF
296500951220     C*
296600951220     C                   MOVEL(P)  AR4TRC        KEY
296700951220     C     KTAB1         CHAIN     TABEL                              36
296800951220     C*
296900951220     C     *IN36         IFEQ      *OFF
297000951220     C     TBLFLG        ANDEQ     ' '
297100951220     C                   MOVEL     TBLUNI        DS7K
297200951220     C     §7KVIS        IFEQ      'S'
297300951220     C                   MOVEL     §7KDES        VS5DES
297400951220     C                   MOVEL     AR4NOT        VS5NOT
297500951220     C                   ADD       1             NRR5
297600131107     C                   WRITE     LRI6S05
297700951220     C                   ENDIF
297800951220     C                   ENDIF
297900951220     C*
298000060216     C     KAR42         READE     FiAR401L                               36
298100040216e   1C                   ENDDO
298200951220     C*
298300960926     C* SE NON HO CARICATO NIENTE NON EMETTO IL SFLDSP PER EVITARE CHE
298400960926     C*  IL PGM DIA ERRORE DI SESSIONE O UNITA'
298500951220     C     NRR5          IFEQ      0
298600991105     C                   clear                   VS5DES
298700991105     C                   clear                   VS5NOT
298800991105     C                   ADD       1             NRR5
298900131107     C                   WRITE     LRI6S05
299000951220     C                   ENDIF
299100951220     C*
299200021216      * Riempio il campone in fondo alla videata con i tasti scelti
299300021216     C                   exsr      Ges_TastiFun
299400021217     c                   eval      FD07 = FDxx
299500021217     c                   eval      F24D07 = F24Dxx
299600911022     C                   ENDSR
299700131114      *-------------------------------------------------------------------------
299800131114      *  Decodifica EVENTO
299900131114      *-------------------------------------------------------------------------
300000131114     c     Decodi_evb    BEGSR
300100131114
300200131114     c                   if        evbcev='  C'
300300131114     c                   eval      §2ades='CONSEGNA'
300400131128     c* aggiungo anche firmatario
300500150616     c                   if        w_ar4_1=*blanks
300600131128     c                   if        §PCTNOMFIR<>*blaNKS
300700131128     C                   EVAL      §2ADES=%TRIM(§2ADES)+' Firma:' +§PCTNOMFIR
300800131128     c                   endif
300900150616     c                   else
301000150616     C                   EVAL      §2ADES=%TRIM(§2ADES)+' Firma:' +ar4not
301100150616     c                   endif
301200131128     c
301300131114     C                   else
301400161108     c                   eval      wcev=evbcev
301500161108     c                   exsr      sr_2a
301600161108     C***                MOVEL     '2A'          COD
301700161108     C***                MOVEL(p)  evbcev        KEY
301800161108     C***  KTAB1         CHAIN     TABEL00F
301900161108    3c***                if        not %found(tabel00f)
302000161108     c***                clear                   ds2a
302100161108     c***                else
302200161108     c***                eval      ds2a=tbluni
302300161108    3c***                endif
302400131114    3c                   endif
302500131114     c
302600161108     c                   if        evbcev='  C'
302700161108     c                   eval      v7duevb=§2ades
302800161108     c                   else
302900131114     c                   eval      v7duevb=evbcev+'-'+§2ades
303000161108     c                   endif
303100131114
303200161017    3c                   if        evbdev>0
303300131114     c     *iso          movel     evbdev        dataeur
303400131114     c                   movel     dataeur       datadmy
303500131114     c                   movel     dataeur       w7cdev            8 0
303600131114
303700161017     c* metto data/ora ultimo evento solo se non si tratta di consegna perchè
303800161017     c* in questo caso tali dati sono già visualizzati nella riga "Consegnata"
303900161017     c                   if        evbcev<>'  C'
304000131114     c                   eval      indx=%checkr(' ':v7duevb)
304100131114    4c                   if        indx>46
304200131114     c                   eval      indx=46
304300131114     c                   else
304400131114     c                   eval      indx=indx+1
304500131114    4c                   endif
304600131114     c                   eval      wcarat=66-indx
304700131114     c                   eval      %subst(v7duevb:indx:wcarat)=' ' +'il ' +
304800131114     c                             %editw(W7cdev: '  /  /    ')
304900131114     c
305000131114    4c                   if        evbhev>0
305100131114     c                   eval      v7duevb=%trim(v7duevb)+' ' +
305200131114     c                                                 %editw(evbhev: '0 :  ')
305300131114    4c                   endif
305400161017     c                   endif
305500131114     c
305600131114    3c                   endif
305700160614     c                   eval      w_evbcev=evbcev
305800131114     c                   ENDSR
305900161108      *-------------------------------------------------------------------------
306000161108      *  Decoditifa causale evento
306100161108      *-------------------------------------------------------------------------
306200161108     c     Sr_2a         BegSr
306300161108     C                   MOVEL     '2A'          COD
306400161108     C                   MOVEL(p)  wcev          KEY
306500161108     C     KTAB1         CHAIN     TABEL00F
306600161108    3c                   if        not %found(tabel00f)
306700161108     c                   clear                   ds2a
306800161108     c                   else
306900161108     c                   eval      ds2a=tbluni
307000161108    3c                   endif
307100161108     c                   ENDSR
307200030703      *-------------------------------------------------------------------------
307300030703      *  Decoditifa tipo bancali
307400030703      *-------------------------------------------------------------------------
307500030703     c     Sr_DecTba     BegSr
307600030703     c                   Clear                   Tibs02Ds
307700030703     c                   Clear                   dTba
307800030703     c                   Movel     'C'           T02Mod
307900030703     c                   Movel     Knsif         T02Sif
308000030703     c                   Movel     'TBA'         T02Cod
308100030703     c                   Movel(p)  wtba          T02Ke1
308200030703     c                   Call      'TIBS02R'
308300030703     c                   Parm                    Kpjba
308400030703     c                   Parm                    Tibs02Ds
308500030703If  3c                   If        T02Err = *Blanks
308600030703     c                   Movel     T02Uni        dTba
308700030703    3c                   EndIf
308800030703     c                   Eval      wdestba = §TbaDesc
308900030703     c                   EndSr
309000021216      *-------------------------------------------------------------------------
309100021216      *  Imposto descrizione tasti funzione
309200021216      *-------------------------------------------------------------------------
309300021216     c     Ges_TastiFun  BEGSR
309400021217      *
309500021217      * per emettere i tasti senza condizionamenti ..
309600021216     c                   setoff                                       89
309700021216      *
309800021216      * Conta i caratteri riempiti dalle RigaTf1
309900021217     c                   z-add     0             $z                3 0
310000021216      *
310100021216      * Conta i caratteri riempiti dalle RigaTf2
310200021217     c                   z-add     0             $k                3 0
310300021216      *
310400021216      * Conta i caratteri riempiti dalle RigaTf3
310500021217     c                   z-add     0             $j                3 0
310600021216      *
310700021216      * Conta la posizione del campone da cui partire per impostare
310800021216      * la descrizione del tasto funzione
310900021216     c                   z-add     0             $y                2 0
311000021216      *
311100021217      * Stringhe che contengono le descrizioni dei tasti funzione
311200021216     c                   reset                   RigaTf1
311300021216     c                   reset                   RigaTf2
311400021216     c                   reset                   RigaTf3
311500070411      *
311600070411      *?F01?--> scelta bolla da assistenza clienti
311700070411     c                   IF        *in63 = *on
311800070411     c                   reset                   $Tf
311900070411     c                   movea     Tf01          $Tf
312000070411     c                   exsr      Rie_$Tf
312100070411     c                   else
312200070411      *?F01?--> scelta bolla generica
312300070411     c                   if        *in61 = *on
312400070411     c                   reset                   $Tf
312500070411     c                   movea     Tf01B         $Tf
312600070411     c                   exsr      Rie_$Tf
312700070411     c                   ENDIF
312800070411     c                   ENDIF
312900021216      *
313000070411      * F13 --> tassazione 1 bolla
313100021216     c                   IF        *in24 = *on
313200021217     c                   reset                   $Tf
313300021217     c                   movea     Tf13          $Tf
313400021217     c                   Exsr      Rie_$Tf
313500021217     C                   ENDIF
313600021216      *
313700070411      * F04 --> tassazione 2 bolla
313800021216     c                   IF        *in22 = *on
313900021217     c                   reset                   $Tf
314000021217     c                   movea     Tf04          $Tf
314100021217     c                   Exsr      Rie_$Tf
314200021217     C                   ENDIF
314300040220      *
314400070411      *?F16 --> assistenza clienti
314500040220     c                   IF        *in64 = *on
314600040220     c                   reset                   $Tf
314700040220     c                   movea     Tf16          $Tf
314800040220     c                   exsr      Rie_$Tf
314900040220     c                   ENDIF
315000160628      * F08 --> ORM
315100160628     c                   IF        *in18 = *on
315200160628     c                   reset                   $Tf
315300160628     c                   movea     Tf08          $Tf
315400160628     c                   Exsr      Rie_$Tf
315500160628     C                   ENDIF
315600021216      *
315700070411      * F05 --> eventi
315800021216     c                   IF        *in95 = *on
315900021217     c                   reset                   $Tf
316000021217     c                   movea     Tf05          $Tf
316100021217     c                   Exsr      Rie_$Tf
316200021217     C                   ENDIF
316300021216      *
316400021216      * F10
316500021217     c                   IF        *in98 = *on
316600021217     c                   reset                   $Tf
316700021217     c                   movea     Tf10          $Tf
316800021217     c                   Exsr      Rie_$Tf
316900021217     c                   ENDIF
317000021216      *
317100021216      * F14
317200021217     c                   IF        *in03 = *on
317300021217     c                   reset                   $Tf
317400021217     c                   movea     Tf14          $Tf
317500021217     c                   Exsr      Rie_$Tf
317600021217     c                   ENDIF
317700021216      *
317800021216      * F02
317900021217     c                   IF        *in04 = *on
318000021217     c                   reset                   $Tf
318100021217     c                   movea     Tf02          $Tf
318200021217     c                   Exsr      Rie_$Tf
318300021217     c                   ENDIF
318400110929      *
318500110930      * F22 --> Perfect Order
318600110929     c                   IF        *in86 = *on
318700110929     c                   reset                   $Tf
318800110929     c                   movea     Tf22          $Tf
318900110929     c                   Exsr      Rie_$Tf
319000110929     C                   ENDIF
319100021216      *
319200021216      * F11
319300021217     c                   IF        *in77 = *on
319400021217     c                   reset                   $Tf
319500021217     c                   movea     Tf11          $Tf
319600021217     c                   Exsr      Rie_$Tf
319700021217     c                   ENDIF
319800070404      *
319900070413      * F17 --> solo se partito com GEO
320000161129     c****               if        *in17 and not *in12
320001161129     c                   if        *in17
320100070404     c                   reset                   $Tf
320200070404     c                   movea     Tf17          $Tf
320300070404     c                   Exsr      Rie_$Tf
320400070413     c                   endif
320500021216      *
320600021216      * F07
320700021217     c                   IF        *in67 = *on
320800021217     c                   reset                   $Tf
320900021217     c                   movea     Tf07          $Tf
321000021217     c                   Exsr      Rie_$Tf
321100021217     c                   ENDIF
321200021216      *
321300160628      * F23= Discordanze estero
321400021217     c                   IF        *in74 = *on and *in16 = *on
321500021217     c                   reset                   $Tf
321600160628     c                   movea     Tf23          $Tf
321700021217     c                   Exsr      Rie_$Tf
321800021217     c                   ENDIF
321900130830
322000130830      * F09 --> Orari servizi non condizionato
322100131007     c                   IF        *in89 = *off and wabif9='S'
322200130930     c                   reset                   $Tf
322300130930     c                   movea     Tf09          $Tf
322400130930     c                   Exsr      Rie_$Tf
322500130930     c                   ENDIF
322600021216      *
322700021216      * F20 --> non condizionato
322800021217     c                   IF        *in89 = *off
322900021217     c                   reset                   $Tf
323000021217     c                   movea     Tf20          $Tf
323100021217     c                   Exsr      Rie_$Tf
323200021217     c                   ENDIF
323300021216      *
323400021216      * F21 --> non condizionato
323500021216     c                   IF        *in89 = *off
323600021217     c                   reset                   $Tf
323700021217     c                   movea     Tf21          $Tf
323800021217     c                   Exsr      Rie_$Tf
323900021217     c                   ENDIF
324000021218      *
324100021218      * F19 --> non condizionato
324200021218     c                   IF        *in89 = *off
324300021218     c                   reset                   $Tf
324400021218     c                   movea     Tf19          $Tf
324500021218     c                   Exsr      Rie_$Tf
324600021218     c                   ENDIF
324700070420     c
324800130830      * F06 --> solo se partito con GEO
324900070420     c                   if        *in17
325000070420     c                   reset                   $Tf
325100070420     c                   movea     Tf06          $Tf
325200070420     c                   Exsr      Rie_$Tf
325300070420     c                   ENDIF
325400021217      *
325500021217      * F12 --> non condizionato
325600021217     c                   IF        *in89 = *off
325700021217     c                   reset                   $Tf
325800021217     c                   movea     Tf12          $Tf
325900021217     c                   Exsr      Rie_$Tf
326000021217     c                   ENDIF
326100021216      *
326200021217      * Pulisco le stringa
326300021216     c                   z-add     1             $h
326400030121     c     1             do        62            $h                2 0
326500021216     C                   if        RigaTf1($h) = '#'
326600021216     c                   movel     *blanks       RigaTf1($h)
326700021216     c                   endif
326800021216     c                   enddo
326900021216     c                   z-add     1             $h
327000030121     c     1             do        62            $h                2 0
327100021216     C                   if        RigaTf2($h) = '#'
327200021216     c                   movel     *blanks       RigaTf2($h)
327300021216     c                   endif
327400021216     c                   enddo
327500021216     c                   z-add     1             $h
327600030121     c     1             do        62            $h                2 0
327700021216     C                   if        RigaTf3($h) = '#'
327800021216     c                   movel     *blanks       RigaTf3($h)
327900021216     c                   endif
328000021216     c                   enddo
328100021217      *
328200021216      * Imposto la descrizione del tasto funzione F24 e segnalo
328300021216      * quante righe ho riempito e quale devo emettere
328400021216     c                   SELECT
328500021216      *
328600021216     c                   when      $k<>*zeros and $Y<>*zeros
328700021216     c                   eval      $loop = 3
328800021216     c                   z-add     3             $loop             1 0
328900021217     C                   movel(p)  CF2413        F24Dxx           16
329000021216      *
329100021216     c                   when      $k<>*zeros and $Y=*zeros
329200021216     c                   z-add     2             $loop             1 0
329300021217     C                   movel(p)  CF2412        F24Dxx
329400021216      *
329500021216     c                   Other
329600021216     c                   z-add     1             $loop             1 0
329700021217     C                   movel     *blanks       F24Dxx
329800021216     c                   ENDSL
329900021216      * Prima riga di tasti funzione
330000030121     c                   movea     RigaTF1       FDxx             62
330100021216     c                   z-add     1             $Rig              1 0
330200021216      *
330300021216     c                   endsr
330400021217      *-------------------------------------------------------------------------
330500021217      *  Riempio le stringhe con i campi funzione compattati
330600021217      *-------------------------------------------------------------------------
330700021217     c     Rie_$Tf       begsr
330800021217      *
330900021217     c                   z-add     1             $x                2 0
331000021217     c     '#'           lookup    $Tf($x)                                31
331100021217     c                   add       $x            $Z
331200030121     c                   IF        $Z <= 63
331300021217     c                   eval      $j = $Z - $x + 1
331400021217     c                   movea     $Tf           RigaTF1($j)
331500021217     c                   ELSE
331600021217     c                   add       $x            $K
331700030121     c                   IF        $K <= 63
331800021217     c                   eval      $j = $K - $x + 1
331900021217     c                   movea     $Tf           RigaTF2($j)
332000021217     c                   ELSE
332100021217     c                   add       $x            $Y
332200030121     c                   IF        $Y <= 63
332300021217     c                   eval      $j = $Y - $x + 1
332400021217     c                   movea     $Tf           RigaTF3($j)
332500021217     c                   ENDIF
332600021217     c                   ENDIF
332700021217     c                   ENDIF
332800021217      *
332900021217     c                   endsr
333000021217      *-------------------------------------------------------------------------
333100021217      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
333200021217      *-------------------------------------------------------------------------
333300021217     c     Ges_F24       begsr
333400021217      *
333500021217     c                   SELECT
333600021217      *
333700021217     c                   WHEN      $Loop = 2 and $Rig =2  or
333800021217     c                             $Loop = 3 and $Rig =3
333900021217     c                   movea     RigaTF1       FDxx
334000021217     c                   z-add     1             $Rig
334100021217      *
334200021217     c                   if        $loop = 2
334300021217     c                   eval      F24Dxx= Cf2412
334400021217     c                   else
334500021217     c                   eval      F24Dxx= Cf2413
334600021217     c                   endif
334700021217      *
334800021217     c                   WHEN      $Loop = 2 and $Rig =1 or
334900021217     c                             $Loop = 3 and $Rig =1
335000021217     c                   movea     RigaTF2       FDxx
335100021217     c                   z-add     2             $Rig
335200021217      *
335300021217     c                   if        $loop = 2
335400021217     c                   eval      F24Dxx= Cf2422
335500021217     c                   else
335600021217     c                   eval      F24Dxx= Cf2423
335700021217     c                   endif
335800021217      *
335900021217     c                   WHEN      $Loop = 3 and $Rig =2
336000021217     c                   movea     RigaTF3       FDxx
336100021217     c                   z-add     3             $Rig
336200021217     c                   eval      F24Dxx= Cf2433
336300021217     c                   ENDSL
336400021217      *
336500021217     c                   endsr
336600021217      *-------------------------------------------------------------------------
336700991027      *  CARICO SUBFILE VARIE
336800991027      *-------------------------------------------------------------------------
336900991027     c     CAR_VARIE     BEGSR
337000991128      *
337100991027      *
337200991027     c                   SELECT
337300991027      *
337400991027     c                   WHEN       WSVN = *BLANKS
337500991027      *
337600991027     C                   WHEN       WSVN = §$2iva
337700991027     c                   z-add     Wvan          W0133
337800991027     c                   eval      Wtstdec = §cvfdc
337900991027     c                   exsr      EDTimporti
338000991027     c                   movel     IMPeditato    VIDiva
338100991027      *
338200991027     c                   OTHER
338300991027     c                   z-add     Wvan          W0133
338400991027     c                   eval      Wtstdec = §cvfdc
338500991027     c                   exsr      EDTimporti
338600991027     c                   movel     IMPeditato    V6Cva1
338700991027     c                   movel     WSVN          V6Csv1
338800991027     C                   Z-ADD     1             WA
338900991027     C     V6CSV1        LOOKUP    CC1(WA)                                99
339000020329     C     CC3(WA)       ifeq      'S'
339100020329      *
339200020329      * Verifico se si tratta di una bolla Fedex
339300021119     c                   if        LnaNtw = 'FED'
339400020329     C                   Z-ADD     1             WA
339500020329     C     V6CSV1        LOOKUP    CB1(WA)                                99
339600020329     C   99              MOVEL     CB2(WA)       V6CDE1
339700020329     C  N99              clear                   V6CDE1
339800020329     C                   else
339900991027     C   99              MOVEL     CC2(WA)       V6CDE1
340000991027     C  N99              MOVEL     *BLANKS       V6CDE1
340100020329     C                   endif
340200020329     C                   else
340300020329     C   99              MOVEL     CC2(WA)       V6CDE1
340400020329     C  N99              MOVEL     *BLANKS       V6CDE1
340500020329     C                   endif
340600991027     C                   add       1             NR6
340700131107     C                   WRITE     LRI6S06
340800991027      *
340900991027     c                   ENDSL
341000991027      *
341100991027     c                   ENDSR
341200040122      *-------------------------------------------------------------------------
341300040122      *  Carico le varie della seconda bolla
341400040122      *-------------------------------------------------------------------------
341500040122     c     Car_Varie1    BegSr
341600040122
341700040122s   1c                   Select
341800040122
341900040122     c                   When      wsvn = *Blanks
342000040122      * Iva
342100040122     c                   When      wsvn = §$2iva
342200040122     c                   z-add     wvan          w0133
342300040122     c                   Eval      Wtstdec = §cvfdc
342400040122     c                   ExSr      EDTimporti
342500040122     c                   Movel     IMPeditato    V9cIva
342600070411     c                   setoff                                       06
342700070411      * Bollo
342800070411     c                   When      wsvn = §$2bol
342900070411     c                   z-add     wvan          w0133
343000070411     c                   Eval      Wtstdec = §cvfdc
343100070411     c                   ExSr      EDTimporti
343200070411     c                   Movel     IMPeditato    V9cIva
343300070411     c                   seton                                        06
343400040122      * Varia G - sempre per prima
343500040122     c                   When      wsvn = §$2vrg
343600040122     c                   Eval      V9cSv1 = wsvn
343700040122     c                   z-add     wvan          w0133
343800040122     c                   Eval      Wtstdec = §cvfdc
343900040122     c                   ExSr      EDTimporti
344000040122     c                   Movel     IMPeditato    V9cVa1
344100040122      * Altre varie
344200040122   x1c                   Other
344300040122     c                   z-add     wvan          W0133
344400040122     c                   eval      Wtstdec = §cvfdc
344500040122     c                   exsr      EDTimporti
344600040122if  2c                   If        V9cSv2 = *Blanks
344700040122     c                   movel     IMPeditato    V9cVa2
344800040122     c                   movel     wsvn          V9csv2
344900040122   x2c                   Else
345000040122if  3c                   If        V9cSv3 = *Blanks
345100040122     c                   movel     IMPeditato    V9cVa3
345200040122     c                   movel     wsvn          V9csv3
345300040122   x3c                   Else
345400040122if  4c                   If        V9cSv4 = *Blanks
345500040122     c                   movel     IMPeditato    V9cVa4
345600040122     c                   movel     wsvn          V9csv4
345700041130   x4c                   Else
345800041130if  5c                   If        V9cSv5 = *Blanks
345900041130     c                   movel     IMPeditato    V9cVa5
346000041130     c                   movel     wsvn          V9csv5
346100041130   x5c                   Else
346200041130if  6c                   If        V9cSv6 = *Blanks
346300041130     c                   movel     IMPeditato    V9cVa6
346400041130     c                   movel     wsvn          V9csv6
346500041130    6c                   EndIf
346600041130    5c                   EndIf
346700040122    4c                   EndIf
346800040122    3c                   EndIf
346900040122    2c                   EndIf
347000040122
347100040122    1c                   EndSl
347200040122
347300040122     c                   EndSr
347400991027      *****************************************************************
347500991027      *  AGGANCIO ANAGRAFICHE
347600991027      *****************************************************************
347700991027     C     CTR_ANAGRA    BEGSR
347800991027      *
347900991027     C                   clear                   DS_cnaco
348000991027     C                   clear                   DS_cnind
348100991027     C                   clear                   DS_cnclp
348200991027     C                   clear                   DS_fncls
348300991027      *
348400991027     C                   CALL      'TIBS69R'
348500991027     C                   PARM                    tibs69DS
348600991027     C                   PARM                    DS_cnaco
348700991027     C                   PARM                    DS_cnind
348800991027     C                   PARM                    DS_cnclp
348900991027     C                   PARM                    DS_fncls
349000991027      *
349100991027     C                   eval      wtibs69r = 'S'
349200991027      * per il 1° giro è inizializzata nelle specifiche "D"
349300991027     C                   clear                   TIBS69DS
349400991027      *
349500991027     C                   ENDSR
349600991027      **********************************************************************
349700991027      * AGGANCIO TABELLA VALUTE
349800991027      **********************************************************************
349900991027     C     CTR_VALUTA    BEGSR
350000991027
350100991027     C                   MOVEL     'CV'          COD
350200991027     C                   MOVEL(P)  WDIV          KEY
350300991027     C     KTAB1         CHAIN     TABEL00F                           31
350400991027     C  N31              MOVEL     TBLUNI        DSCV
350500991027
350600991027     C                   ENDSR
350700991027      **********************************************************************
350800991027      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
350900991027      **********************************************************************
351000991027     C     EDTIMPORTI    BEGSR
351100991027
351200991027      * ------------------------------------------------------------------------
351300991027      * W0133      contiene il numero da editare per il video
351400991027      * WTSTDEC    corrisponde a §CVfdc della tabella CV
351500991027      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
351600991027      * ------------------------------------------------------------------------
351700991027      *
351800991027     c                   clear                   IMPeditato
351900991027     c                   clear                   W017A
352000991027      *
352100991027     c                   if        Wtstdec = 'S'
352200991027     C                   eval      W017a = %EDITC(W0133:'2')
352300991027     c                   else
352400991027     c                   z-add     W0133         W0130
352500991027     C                   eval      W017a = %EDITC(W0130:'2')
352600991027     c                   endif
352700991027      *
352800991027     c                   move      W017a         IMPeditato
352900991027      *
353000991027     c                   ENDSR
353100940802     C**************************************************************************
353200940802     C*    LEGAMI BOLLA
353300940802     C**************************************************************************
353400930413     C     LEGBOL        BEGSR
353500940802     C*
353600940912     C* DEVO MEMORIZZARE LA BOLLA POICHE DEVO RIAGGANCIARLA DOPO
353700940912     C                   MOVEL     ARBAAS        WAAS
353800940912     C                   MOVEL     ARBLNP        WLNP
353900940912     C                   MOVEL     ARBNRS        WNRS
354000940912     C                   MOVEL     ARBNSP        WNSP
354100940912     C*
354200940907     C* PULISCO IL SFL
354300950825     C                   SETOFF                                       787923
354400131107     C                   WRITE     LRI6C08
354500950825     C                   SETON                                        7923
354600940923     C                   MOVEL     *BLANKS       LEGAME
354700940907     C*
354800940907     C                   SETOFF                                       94
354900950718     C                   Z-ADD     *ZERO         NR8
355000940907     C                   Z-ADD     *ZERO         SAVNR8            4 0
355100940907     C                   MOVEL     *BLANKS       WFLG1             1
355200940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
355300940907     C*
355400940907     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
355500940907     C     KARB          CHAIN     FNLBL01L                           99
355600940907     C   99KARB          CHAIN     FNLBL02L                           99
355700940907     C  N99              DO
355800940907     C*
355900940907     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
356000940907     C                   MOVEL     LBLAAO        KAAS
356100940907     C                   MOVEL     LBLLPO        KLNP
356200940907     C                   MOVEL     LBLNRO        KNRS
356300940907     C                   MOVEL     LBLNSO        KNSP
356400940907     C     KLBL3         CHAIN     FNLBL02L                           99
356500950103     C     *IN99         IFEQ      *ON
356600950103     C                   MOVEL     'N'           WORIGI            1
356700950103     C                   ELSE
356800950103     C                   MOVEL     'S'           WORIGI
356900950103     C                   ENDIF
357000940907     C*
357100940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
357200940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
357300950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
357400950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
357500940907     C                   MOVEL     ' '           V8CSCE
357600940907     C                   MOVEL     LBLAAO        V8CAAP
357700940907     C                   MOVEL     LBLLPO        V8CLPP
357800940907     C                   MOVEL     LBLLAO        V8CLAP
357900940907     C                   MOVEL     LBLNRO        V8CNRP
358000940907     C                   MOVEL     LBLNSO        V8CNSP
358100940926     C                   CLEAR                   V8CAAN
358200940926     C                   CLEAR                   V8CLPN
358300940926     C                   CLEAR                   V8CLAN
358400940926     C                   CLEAR                   V8CNRN
358500940926     C                   CLEAR                   V8CNSN
358600940907     C*
358700940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
358800940907     C     LBLAAO        IFEQ      V3CAAS
358900940907     C     LBLLPO        ANDEQ     V3CLNP
359000940907     C     LBLNRO        ANDEQ     V3CNRS
359100940907     C     LBLNSO        ANDEQ     V3CNSP
359200940907     C                   MOVEL     *BLANKS       V8CPRE
359300940907     C                   SETON                                        80
359400940907     C                   SETON                                        81
359500940907     C                   MOVEL     '1'           WFLG1
359600940907     C                   ELSE
359700940907     C                   SETOFF                                       81
359800940907     C*
359900950316     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
360000950316     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
360100950316     C                   SETOFF                                       93
360200950316     C     KLBL3         SETLL     FNARB01L                               93
360300950316     C  N93              MOVEA     PRD(1)        V8CPRE
360400950316     C  N93              SETON                                        80
360500150423     C** 93              MOVEA     PRD(2)        V8CPRE
360600150423     C** 93              SETOFF                                       80
360700950316     C*
360800950316     C* SE NON TROVATA SU ARB PROVO SU BLP
360900950316     C  N93              DO
361000950316     C                   SETOFF                                       93
361100950316     C     KLBL3         SETLL     FNBLP01L                               93
361200010712      *
361300010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
361400010712     c                   if        *in93 = *off
361500010712     C                   MOVEA     PRD(1)        V8CPRE
361600010712     c                   else
361700150423     C                   MOVEA     PRD(2)        V8CPRE
361800150423     C                   SETOFF                                       80
361900010712     C                   ENDIF
362000950316     C*
362100950316     C                   ENDDO
362200940907     C*
362300940907     C                   ENDIF
362400940907     C                   SETON                                        83
362500950103     C*
362600950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
362700950103     C                   MOVEL     *IN80         V8CI80
362800950103     C                   MOVEL     *IN81         V8CI81
362900950103     C                   MOVEL     *IN82         V8CI82
363000950103     C                   MOVEL     *IN83         V8CI83
363100950103     C*
363200940907     C                   Z-ADD     1             NR8
363300131107     C                   WRITE     LRI6S08
363400950825     C                   SETON                                        78
363500950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
363600940907     C*
363700950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
363800950103     C*                      2) NON HO TROVATO L'ORIGINALE
363900950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
364000950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
364100950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
364200950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
364300950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
364400950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
364500950103     C* L'ORIGINALE E LA MAMMA.
364600950103     C     WORIGI        IFEQ      'N'
364700950103     C                   MOVEL     *BLANKS       V8CSCE
364800950103     C                   CLEAR                   V8CAAP
364900950103     C                   CLEAR                   V8CLPP
365000950103     C                   CLEAR                   V8CLAP
365100950103     C                   CLEAR                   V8CNRP
365200950103     C                   CLEAR                   V8CNSP
365300950103     C                   MOVEL     LBLAAP        V8CAAN
365400950103     C                   MOVEL     LBLLPP        V8CLPN
365500950103     C                   MOVEL     LBLLAP        V8CLAN
365600950103     C                   MOVEL     LBLNRP        V8CNRN
365700950103     C                   MOVEL     LBLNSP        V8CNSN
365800950103     C                   SETOFF                                       8182
365900950103     C*
366000950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
366100950103     C     V8CAAN        IFEQ      V3CAAS
366200950103     C     V8CLPN        ANDEQ     V3CLNP
366300950103     C     V8CNRN        ANDEQ     V3CNRS
366400950103     C     V8CNSN        ANDEQ     V3CNSP
366500950103     C                   MOVEL     *BLANKS       V8CPRE
366600950103     C                   SETON                                        80
366700950103     C                   SETON                                        82
366800950103     C                   ELSE
366900950103     C*
367000950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
367100950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
367200950316     C                   EXSR      CHKBOL
367300950103     C*
367400950103     C                   ENDIF
367500950103     C*
367600950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
367700950103     C                   SETOFF                                       83
367800950103     C*
367900950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
368000950103     C                   MOVEL     *IN80         V8CI80
368100950103     C                   MOVEL     *IN81         V8CI81
368200950103     C                   MOVEL     *IN82         V8CI82
368300950103     C                   MOVEL     *IN83         V8CI83
368400950103     C*
368500950103     C                   ADD       1             NR8
368600131107     C                   WRITE     LRI6S08
368700950103     C*
368800950103     C                   ENDIF
368900940907     C                   SETOFF                                       83  99
369000940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
369100940907     C* CICLO FINO ALL'ULTIMO LEGAME
369200940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
369300940907     C                   Z-ADD     *ZERO         WA                3 0
369400940907     C                   Z-ADD     *ZERO         WB                3 0
369500940907     C                   MOVEA     *ZERO         AAN
369600940907     C     *IN99         DOWEQ     *OFF
369700940907     C*
369800940907     C                   MOVEL     ' '           V8CSCE
369900940907     C                   MOVEL     LBLAAP        V8CAAP
370000940907     C                   MOVEL     LBLLPP        V8CLPP
370100940907     C                   MOVEL     LBLLAP        V8CLAP
370200940907     C                   MOVEL     LBLNRP        V8CNRP
370300940907     C                   MOVEL     LBLNSP        V8CNSP
370400940907     C                   MOVEL     LBLAAN        V8CAAN
370500940907     C                   MOVEL     LBLLPN        V8CLPN
370600940907     C                   MOVEL     LBLLAN        V8CLAN
370700940907     C                   MOVEL     LBLNRN        V8CNRN
370800940907     C                   MOVEL     LBLNSN        V8CNSN
370900940907     C                   SETOFF                                       8182
371000940907     C*
371100950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
371200940907     C     LBLAAP        IFEQ      V3CAAS
371300940907     C     LBLLPP        ANDEQ     V3CLNP
371400940907     C     LBLNRP        ANDEQ     V3CNRS
371500940907     C     LBLNSP        ANDEQ     V3CNSP
371600940907     C                   SETON                                        81
371700940907     C                   ENDIF
371800940907     C*
371900940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
372000940907     C     LBLAAN        IFEQ      V3CAAS
372100940907     C     LBLLPN        ANDEQ     V3CLNP
372200940907     C     LBLNRN        ANDEQ     V3CNRS
372300940907     C     LBLNSN        ANDEQ     V3CNSP
372400940907     C                   MOVEL     *BLANKS       V8CPRE
372500940907     C                   SETON                                        80
372600940907     C                   SETON                                        82
372700940907     C                   ELSE
372800940907     C*
372900940907     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
373000940907     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
373100950316     C                   EXSR      CHKBOL
373200940907     C*
373300940907     C                   ENDIF
373400950103     C*
373500950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
373600950103     C                   MOVEL     *IN80         V8CI80
373700950103     C                   MOVEL     *IN81         V8CI81
373800950103     C                   MOVEL     *IN82         V8CI82
373900950103     C                   MOVEL     *IN83         V8CI83
374000940907     C*
374100940907     C* SCRIVO IL RECORD NEL SFL
374200940907     C                   ADD       1             NR8
374300131107     C                   WRITE     LRI6S08
374400940907     C*
374500940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
374600940907     C                   ADD       1             WA
374700940907     C                   MOVEL     LBLAAN        AAN(WA)
374800940907     C                   MOVEL     LBLLPN        LPN(WA)
374900940907     C                   MOVEL     LBLNRN        NRN(WA)
375000940907     C                   MOVEL     LBLNSN        NSN(WA)
375100940907     C*
375200940907     C     KLBL3         READE     FNLBL02L                               99
375300940907     C*
375400940907     C* SE NON HO PIU' BOLLE CON READE
375500940907     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
375600970116     C     WB            DOWLT     WA
375700970116     C     *IN99         ANDEQ     *ON
375800940907     C                   ADD       1             WB
375900940907     C                   MOVEL     AAN(WB)       KAAS
376000940907     C                   MOVEL     LPN(WB)       KLNP
376100940907     C                   MOVEL     NRN(WB)       KNRS
376200940907     C                   MOVEL     NSN(WB)       KNSP
376300940907     C     KLBL3         SETLL     FNLBL02L
376400940907     C     KLBL3         READE     FNLBL02L                               99
376500970116     C                   ENDDO
376600940907     C*
376700940907     C                   ENDDO
376800940907     C                   ENDDO
376900940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
377000940907     C*
377100940907     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
377200940907     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
377300940907     C     NR8           IFEQ      2
377400940907     C*
377500940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
377600131107     C     2             CHAIN     LRI6S08                            99
377700940907     C     V8CAAP        IFEQ      V3CAAS
377800940907     C     V8CLPP        ANDEQ     V3CLNP
377900940907     C     V8CNRP        ANDEQ     V3CNRS
378000940907     C     V8CNSP        ANDEQ     V3CNSP
378100940907     C                   MOVEL     V8CAAN        COMAAN
378200940907     C                   MOVEL     V8CLPN        COMLPN
378300940907     C                   MOVEL     V8CNRN        COMNRN
378400940907     C                   MOVEL     V8CNSN        COMNSN
378500950307     C                   MOVEL     V8CLAN        COMLAN
378600950316     C                   MOVEA     'SEGUEN.'     LBL(14)
378700940907     C                   ELSE
378800940907     C                   MOVEL     V8CAAP        COMAAN
378900940907     C                   MOVEL     V8CLPP        COMLPN
379000940907     C                   MOVEL     V8CNRP        COMNRN
379100940907     C                   MOVEL     V8CNSP        COMNSN
379200950307     C                   MOVEL     V8CLAP        COMLAN
379300950316     C                   MOVEA     'PRECED.'     LBL(14)
379400940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
379500940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
379600940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
379700131107     C     1             CHAIN     LRI6S08                            99
379800940926     C* REIMPOSTO NR8=2 PERCHE' MI SERVE DOPO
379900940926     C                   Z-ADD     2             NR8
380000940907     C                   ENDIF
380100940907     C*
380200950316     C                   MOVEA     COMLPN        LBL(21)
380300950316     C                   MOVEA     COMNRN        LBL(25)
380400950316     C                   MOVEA     COMNSN        LBL(28)
380500950316     C                   MOVEA     COMLAN        LBL(40)
380600940907     C*
380700940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
380800940907     C                   Z-ADD     1             W                 1 0
380900940907     C     V8CPRE        LOOKUP    PRD(W)                                 99
381000950307     C                   MOVEA     LBD(W)        LBL(44)
381100940907     C     W             COMP      2                                      94
381200940907     C*
381300940907     C                   MOVEA     LBL           LEGAME
381400940907     C                   ELSE
381500940907     C*
381600940907     C     NR8           IFGT      2
381700940907     C                   MOVEA     LB2           LEGAME
381800940907     C                   SETON                                        94
381900940907     C                   ENDIF
382000940907     C                   ENDIF
382100940907     C*
382200940907     C     NR8           COMP      1                                    3434
382300940907     C*
382400940907     C                   Z-ADD     NR8           SAVNR8
382500940907     C                   Z-ADD     1             NR8
382600940923     C     LEGAME        COMP      *BLANKS                            9292
382700940912     C*
382800940912     C* RIAGGANCIO LA BOLLA IN ESAME
382900940912     C                   MOVEL     WAAS          ARBAAS
383000940912     C                   MOVEL     WLNP          ARBLNP
383100940912     C                   MOVEL     WNRS          ARBNRS
383200940912     C                   MOVEL     WNSP          ARBNSP
383300150423     c     karb2         chain     fnblp01l
383400150423     C     KARB2         CHAIN     FNARB01L
383500930511     C*
383600930413     C                   ENDSR
383700950316     C**************************************************************************
383800950316     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
383900950316     C**************************************************************************
384000950316     C     CHKBOL        BEGSR
384100950316     C*
384200950316     C                   SETOFF                                       93
384300950316     C     KLBL1         SETLL     FNARB01L                               93
384400950316     C  N93              MOVEA     PRD(1)        V8CPRE
384500950316     C  N93              SETON                                        80
384600950316     C   93              MOVEA     PRD(2)        V8CPRE
384700950316     C   93              SETOFF                                       80
384800950316     C*
384900950316     C* SE NON TROVATA SU ARB PROVO SU BLP
385000950316     C  N93              DO
385100950316     C                   SETOFF                                       93
385200950316     C     KLBL1         SETLL     FNBLP01L                               93
385300010712      *
385400010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
385500010712     c                   if        *in93 = *off
385600010712     C                   MOVEA     PRD(1)        V8CPRE
385700010712     c                   else
385800950316     C                   MOVEA     PRD(2)        V8CPRE
385900950316     C                   SETOFF                                       80
386000010712     C                   ENDIF
386100950316     C*
386200950316     C                   ENDDO
386300950316     C*
386400950316     C                   ENDSR
386500940802     C**************************************************************************
386600940802     C*    VISUALIZZA BOLLA
386700940802     C**************************************************************************
386800911023     C     VISUAL        BEGSR
386900940321      *
387000940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
387100131108     c     Emriepil      tag
387200940712     C                   EXSR      RIEPIL
387300940712      *
387400131108      * PER F15 PASSO ALLA BOLLA, PER INVIO o F12 RITORNO AL SFL
387500940712     C  NKP              GOTO      ENDVIS
387600931112      *
387700931112     C                   SETOFF                                       89
387800940802     C                   MOVEL     'S'           WRIEM             1
387900021217      *
388000021217      * Riempio il campone in fondo alla videata con i tasti scelti
388100021217     C                   exsr      Ges_TastiFun
388200021217     c                   eval      FD03 = FDxx
388300021217     c                   eval      F24D03 = F24Dxx
388400911023     C     FOR03         TAG
388500940802     C     WRIEM         IFEQ      'S'
388600131107     C                   WRITE     LRI6T01
388700940802     C                   MOVEL     *BLANKS       WRIEM
388800940802     C                   ENDIF
388900021217      *
389000021217      * Imposto la stringa comandi del 'giro' successivo
389100021217     C                   IF        *inKY = *on and $Loop > 1
389200021217     c                   Exsr      Ges_F24
389300021217     c                   eval      FD03 = FDxx
389400021217     c                   eval      F24D03 = F24Dxx
389500021217     C                   ENDIF
389600021217      *
389700131107     C                   EXFMT     LRI6D03
389800070420     c                   setoff                                       28
389900070420     c                   clear                   v1cmsg
390000931112      *
390100131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
390200931112     C   KY*IN89         IFEQ      *ON
390300931112     C                   SETOFF                                       89
390400931112     C                   ELSE
390500931112     C                   SETON                                        89
390600931112     C                   ENDIF
390700931112     C   KY              GOTO      FOR03
390800911023     C*
390900131108     C* F3 - FINE LAVORO TORNO A SUBFILE
391000940714     C   KC              MOVEL     '2'           V2CFLG            1
391100911023     C   KC              GOTO      ENDVIS
391200911023     C*
391300131108     C* F12 - FINE LAVORO tolno a videata di riepilogo
391400131108     C   KL              GOTO      emriepil
391500911023     C*
391600911024     C*
391700930517     C* CMD18 - INTERROGA LEGAME BOLLA
391800930518     C   KS              DO
391900930518     C                   EXSR      CMD18
392000940907     C                   MOVEL     'S'           WRIEM
392100930518     C                   GOTO      FOR03
392200930518     C                   END
392300930517     C*
392400940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
392500920804     C                   EXSR      COMAND
392600940905     C     WCMD          IFEQ      'S'
392700940802     C     WF03          IFEQ      *BLANKS
392800070403     c
392900920804     C                   GOTO      FOR03
393000940802     C                   ELSE
393100940802     C                   MOVEL     '2'           V2CFLG
393200940802     C                   GOTO      ENDVIS
393300940802     C                   ENDIF
393400940905     C                   ENDIF
393500911023     C*
393600911023     C* FORMATO4
393700931112     C                   SETOFF                                       89
393800940802     C                   MOVEL     'S'           WRIEM
393900021217      *
394000021217      * Riempio il campone in fondo alla videata con i tasti scelti
394100021217     C                   exsr      Ges_TastiFun
394200021217     c                   eval      FD04 = FDxx
394300021217     c                   eval      F24D04 = F24Dxx
394400911023     C     FOR04         TAG
394500940802     C     WRIEM         IFEQ      'S'
394600131107     C                   WRITE     LRI6T01
394700940802     C                   MOVEL     *BLANKS       WRIEM
394800940802     C                   ENDIF
394900021217      * Imposto la stringa comandi del 'giro' successivo
395000021217     C                   IF        *inKY = *on and $Loop > 1
395100021217     c                   Exsr      Ges_F24
395200021217     c                   eval      FD04 = FDxx
395300021217     c                   eval      F24D04 = F24Dxx
395400021217     C                   ENDIF
395500131107     C                   EXFMT     LRI6D04
395600070420     c                   setoff                                       28
395700070420     c                   clear                   v1cmsg
395800931112      *
395900131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
396000931112     C   KY*IN89         IFEQ      *ON
396100931112     C                   SETOFF                                       89
396200931112     C                   ELSE
396300931112     C                   SETON                                        89
396400931112     C                   ENDIF
396500931112     C   KY              GOTO      FOR04
396600911023     C*
396700131108     C* F12 - RITORNO VIDEATA 3
396800911023     C   KL              GOTO      FOR03
396900920804     C*
397000131108     C* F18 - INTERROGA LEGAME BOLLA
397100930518     C   KS              DO
397200930518     C                   EXSR      CMD18
397300930518     C                   GOTO      FOR04
397400930518     C                   END
397500940923     C*
397600940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
397700131120     c                   eval      wvideo='4'
397800131120     C                   EXSR      CTR_puIn
397900940923     C     WRIEM         CABEQ     'S'           FOR04
398000930517     C*
398100940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
398200920804     C                   EXSR      COMAND
398300940905     C     WCMD          IFEQ      'S'
398400940802     C     WF03          IFEQ      *BLANKS
398500940802     C                   GOTO      FOR04
398600940802     C                   ELSE
398700940802     C                   MOVEL     '2'           V2CFLG
398800940802     C                   GOTO      ENDVIS
398900940802     C                   ENDIF
399000940905     C                   ENDIF
399100911023     C*
399200911023     C* VIDEATA 5
399300931112     C                   SETOFF                                       89
399400940809     C                   MOVEL     'S'           WRIEM
399500021217      *
399600021217      * Riempio il campone in fondo alla videata con i tasti scelti
399700021217     C                   exsr      Ges_TastiFun
399800021217     c                   eval      FD05 = FDxx
399900021217     c                   eval      F24D05 = F24Dxx
400000911023     C     FOR05         TAG
400100940809     C     WRIEM         IFEQ      'S'
400200131107     C                   WRITE     LRI6T01
400300940809     C                   MOVEL     *BLANKS       WRIEM
400400940809     C                   ENDIF
400500951220     C                   Z-ADD     1             REC5
400600021217      *
400700021217      * Imposto la stringa comandi del 'giro' successivo
400800021217     C                   IF        *inKY = *on and $Loop > 1
400900021217     c                   Exsr      Ges_F24
401000021217     c                   eval      FD05 = FDxx
401100021217     c                   eval      F24D05 = F24Dxx
401200021217     C                   ENDIF
401300131107     C                   WRITE     LRI6Z05
401400131107     C                   EXFMT     LRI6C05
401500070420     c                   setoff                                       28
401600070420     c                   clear                   v1cmsg
401700931112      *
401800131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
401900931112     C   KY*IN89         IFEQ      *ON
402000931112     C                   SETOFF                                       89
402100931112     C                   ELSE
402200931112     C                   SETON                                        89
402300931112     C                   ENDIF
402400931112     C   KY              GOTO      FOR05
402500911023     C*
402600131108     C* F12 - RITORNO VIDEATA 2
402700911023     C   KL              GOTO      FOR04
402800920804     C*
402900940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
403000920804     C                   EXSR      COMAND
403100940905     C     WCMD          IFEQ      'S'
403200940802     C     WF03          IFEQ      *BLANKS
403300940802     C                   GOTO      FOR05
403400940802     C                   ELSE
403500131108     C                   MOVEL     '2'           V2CFLG
403600131108     C                   GOTO      ENDVIS
403700940802     C                   ENDIF
403800070406
403900070406     c                   else
404000070406     c
404100070406     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
404200070406     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
404300070406     c                   if        v3cgesd='?'
404400070406     c                   exsr      VISUALAbi
404500070406     c                   clear                   v3cgesd
404600070406     c                   eval      wriem='S'
404700070406     C                   GOTO      FOR05
404800070406     c                   endif
404900940905     C                   ENDIF
405000131108     c
405100131108     C                   GOTO      emriepil
405200931112      *
405300911023     C     ENDVIS        ENDSR
405400070403     C**************************************************************************
405500070403     C*    VISUALIZZA Abilitazioni
405600070403     C**************************************************************************
405700070403     C     VISUALAbi     BEGSR
405800070403     c*
405900131120     c                   clear                   fnlrg7ds
406000131120     c                   move      v3cdsp        ilrg7aas
406100131120     c                   eval      ilrg7lnp=v3clnp
406200131120     c                   eval      ilrg7nrs=v3cnrs
406300131120     c                   eval      ilrg7nsp=v3cnsp
406400131120     c                   eval      ilrg7vsdet='?'
406500131120     c                   eval      ilrg7lung=58
406600131120     c                   call      'FNLRG7R'
406700131120     c                   parm                    fnlrg7ds
406800070403     c
406900070403     C                   ENDSR
407000940712     C**************************************************************************
407100940802     C*    VISUALIZZA BOLLA PRECEDENTE
407200940802     C**************************************************************************
407300930510     C     VISUA2        BEGSR
407400940321      *
407500940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
407600131108     c     Emriepi2      tag
407700940712     C                   EXSR      RIEPI2
407800940712      *
407900131108      * PER F15 PASSO ALLA BOLLA, PER INVIO RITORNO AL SFL
408000940713     C  NKP              GOTO      ENDV8C
408100931112      *
408200931112     C                   SETOFF                                       89
408300940809     C                   MOVEL     'S'           WRIEM
408400021217      *
408500021217      * Riempio il campone in fondo alla videata con i tasti scelti
408600021217     C                   exsr      Ges_TastiFun
408700070516     c                   eval      FD03 = FDxx
408800070516     c                   eval      F24D03 = F24Dxx
408900930510     C     FOR033        TAG
409000940809     C     WRIEM         IFEQ      'S'
409100131107     C                   WRITE     LRI6T01
409200940809     C                   MOVEL     *BLANKS       WRIEM
409300940809     C                   ENDIF
409400021217      *
409500021217      * Imposto la stringa comandi del 'giro' successivo
409600021217     C                   IF        *inKY = *on and $Loop > 1
409700021217     c                   Exsr      Ges_F24
409800021217     c                   eval      FD03 = FDxx
409900021217     c                   eval      F24D03 = F24Dxx
410000021217     C                   ENDIF
410100131107     C                   EXFMT     LRI6D03
410200070420     c                   setoff                                       28
410300070420     c                   clear                   v1cmsg
410400931112      *
410500131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
410600931112     C   KY*IN89         IFEQ      *ON
410700931112     C                   SETOFF                                       89
410800931112     C                   ELSE
410900931112     C                   SETON                                        89
411000931112     C                   ENDIF
411100931112     C   KY              GOTO      FOR033
411200930510     C*
411300131108     C* F12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
411400930510     C   KC
411500131108     COR KL              GOTO      Emriepi2
411600930510     C*
411700940905     C* CONTROLLO SE PREMUTI TASTI FUNZIONALI E, NEL CASO, RIEMETTO
411800930510     C                   EXSR      COMAND
411900940905     C     WCMD          IFEQ      'S'
412000930510     C                   GOTO      FOR033
412100930510     C                   END
412200931112      *
412300930510     C* FORMATO4
412400931112     C                   SETOFF                                       89
412500940809     C                   MOVEL     'S'           WRIEM
412600021217      *
412700021217      * Riempio il campone in fondo alla videata con i tasti scelti
412800021217     C                   exsr      Ges_TastiFun
412900021217     c                   eval      FD04 = FDxx
413000021217     c                   eval      F24D04 = F24Dxx
413100930510     C     FOR044        TAG
413200940809     C     WRIEM         IFEQ      'S'
413300131107     C                   WRITE     LRI6T01
413400940809     C                   MOVEL     *BLANKS       WRIEM
413500940809     C                   ENDIF
413600021217      *
413700021217      * Imposto la stringa comandi del 'giro' successivo
413800021217     C                   IF        *inKY = *on and $Loop > 1
413900021217     c                   Exsr      Ges_F24
414000021217     c                   eval      FD04 = FDxx
414100021217     c                   eval      F24D04 = F24Dxx
414200021217     C                   ENDIF
414300131107     C                   EXFMT     LRI6D04
414400070420     c                   setoff                                       28
414500070420     c                   clear                   v1cmsg
414600931112      *
414700931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
414800931112     C   KY*IN89         IFEQ      *ON
414900931112     C                   SETOFF                                       89
415000931112     C                   ELSE
415100931112     C                   SETON                                        89
415200931112     C                   ENDIF
415300931112     C   KY              GOTO      FOR044
415400930510     C*
415500930510     C* CMD12 - RITORNO VIDEATA 3
415600930510     C   KL              GOTO      FOR033
415700940923     C*
415800940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
415900131120     c                   eval      wvideo='4'
416000131120     C                   EXSR      CTR_puIn
416100940923     C     WRIEM         CABEQ     'S'           FOR044
416200930510     C*
416300940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
416400930510     C                   EXSR      COMAND
416500940905     C     WCMD          IFEQ      'S'
416600930510     C                   GOTO      FOR044
416700940905     C                   ENDIF
416800931112      *
416900930510     C* VIDEATA 5
417000931112     C                   SETOFF                                       89
417100940809     C                   MOVEL     'S'           WRIEM
417200021217      *
417300021217      * Riempio il campone in fondo alla videata con i tasti scelti
417400021217     C                   exsr      Ges_TastiFun
417500021217     c                   eval      FD05 = FDxx
417600021217     c                   eval      F24D05 = F24Dxx
417700930510     C     FOR055        TAG
417800940809     C     WRIEM         IFEQ      'S'
417900131107     C                   WRITE     LRI6T01
418000940809     C                   MOVEL     *BLANKS       WRIEM
418100940809     C                   ENDIF
418200951220     C                   Z-ADD     1             REC5
418300021217      *
418400021217      * Imposto la stringa comandi del 'giro' successivo
418500021217     C                   IF        *inKY = *on and $Loop > 1
418600021217     c                   Exsr      Ges_F24
418700021217     c                   eval      FD05 = FDxx
418800021217     c                   eval      F24D05 = F24Dxx
418900021217     C                   ENDIF
419000131107     C                   WRITE     LRI6Z05
419100131107     C                   EXFMT     LRI6C05
419200070420     c                   setoff                                       28
419300070420     c                   clear                   v1cmsg
419400931112      *
419500931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
419600931112     C   KY*IN89         IFEQ      *ON
419700931112     C                   SETOFF                                       89
419800931112     C                   ELSE
419900931112     C                   SETON                                        89
420000931112     C                   ENDIF
420100931112     C   KY              GOTO      FOR055
420200930510     C*
420300930510     C* CMD12 - RITORNO VIDEATA 2
420400930510     C   KL              GOTO      FOR044
420500930510     C*
420600940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
420700940905     C                   EXSR      COMAND
420800940905     C     WCMD          IFEQ      'S'
420900930510     C                   GOTO      FOR055
421000930510     C                   END
421100131108
421200131108     C                   GOTO      Emriepi2
421300931112      *
421400940713     C     ENDV8C        ENDSR
421500940802     C**************************************************************************
421600940802     C* TASTI DI FUNZIONE
421700940802     C**************************************************************************
421800920804     C     COMAND        BEGSR
421900940905     C*
422000940905     C* PULISCO IL FLAG CHE INDICA SE PREMUTO UN TASTO FUNZIONE
422100940905     C                   MOVE      *BLANKS       WCMD              1
422200040218      *
422300070411      *?F1 - Aggiunta bolla in reclamo?o scelta bolla generica
422400040218if  1c                   if        *inKA   = *on
422500070423     C                   Z-ADD     arbLNP        COMLNP
422600070411     C                   MOVEL     '3'           V2CFLG
422700070411      *?-
422800150402if  2c                   if        SDSprm  > 1 and ilru6ra='S'
422900131111     c                   eval      wARBlnp = ARBlnp
423000131111     c                   eval      wARBnrs = ARBnrs
423100131111     c                   eval      wARBnsp = ARBnsp
423200131111     c                   eval      wARBaas = ARBaas
423300150402     c*??????????        clear                   FIDNA1ds
423400150402     c                   eval      ELRU6INS  = 'S'
423500150402     c                   eval      OLRU6TOR  = 'S'
423600150402     c                   eval      OLRU6OGG  = ds_OggSped
423700070411    2c                   endif
423800040218     c                   goto      Fine
423900040218e   1c                   endif
424000940802     C*
424100990119     C* F5 - INTERROGAZIONE EVENTI
424200040218do  1C   KE              DO
424300940905     C                   MOVEL     'S'           WCMD
424400940726     C                   MOVEL     V2CAAS        PB5AAS
424500940726     C                   MOVEL     COMLNP        PB5LNP
424600940726     C                   MOVEL     V2CNRS        PB5NRS
424700940726     C                   MOVEL     V2CNSP        PB5NSP
424800940726     C                   MOVEL     *BLANKS       PB5F03
424900940726     C                   MOVEL     *BLANKS       PB5FLG
425000940802     C                   MOVEL     *BLANKS       PB5PGM
425100131107     C                   MOVEL     'FNLRI6R'     PB5PGM
425200940726     C                   MOVEL     V1CDSA        PB5DSA
425300940726     C                   MOVEL     V3CDSP        PB5DSP
425400940726     C                   MOVEL     VIDRMN        PB5RMN
425500940726     C                   MOVEL     V3CLNA        PB5LNA
425600940802     C                   MOVEL     V3DLNA        PB5LN1
425700940726     C                   MOVEL     V4CRSD        PB5RSD
425800940726     C                   MOVEL     VIDDCP        PB5DCP
425900941031     C                   MOVEL     VIDTMC        PB5TMC
426000940726     C                   MOVEL     PARAB5        KPJBU
426100940722     C                   CALL      'FNLRB5R'
426200930525     C                   PARM                    KPJBA
426300941109     C                   MOVEL     KPJBU         PARAB5
426400941109     C                   MOVEL     PB5F03        WF03
426500940725     C                   MOVEL     'S'           WRIEM
426600040218e   1C                   ENDDO
426700070420      *?F6 - Stampa LDV
426800070420if  1c                   if        *inKf = *on
426900070420     C                   MOVEL     'S'           WCMD
427000080201     c* Se assegnato non abilitato non stampo la LdV ma emetto errore
427100150617     c                   if        w_tbo='A' and arbaca=' '
427200160505     C                   MOVEL     MSG(3)        V1CMSG
427300080201     C                   SETON                                        28
427400080201     c                   else
427500070420     c
427600070420     c                   if        wrichtrul90=' '
427700070420     c* La prima volta devo proporre la richiesta stampanti
427800070420     c                   exsr      RICHSTAMPAN
427900070420     C                   MOVEL     'S'           WRIEM
428000070420     c
428100070420     c* Se premuto F3 esco senza fare nulla
428200070420     c                   if        d90f3<>'1' and not *in91
428300070420     c                   eval      wrichtrul90='S'
428400070420     c                   endif
428500070420     c                   if        *in91
428600160505     C                   MOVEL     MSG(2)        V1CMSG
428700070420     C                   SETON                                        28
428800070420     c                   endif
428900070420     c                   endif
429000070420     c
429100070420     c                   if        wrichtrul90='S'
429200161111      * OVRPRTF BOLLE A4
429300161111     C                   MOVEA     D90PRB4       CMA4f6(30)
429400161111     C                   MOVEA     D90MDB4       CMA4f6(52)
429500161111     C*
429600161111     C                   Z-ADD     130           LUNG             15 5
429700161111     C                   CLEAR                   COMMAN          130
429800161111     C                   MOVEA     CMA4f6        COMMAN
429900161111     C                   CALL      'QCMDEXC'                            91
430000161111     C                   PARM                    COMMAN
430100161111     C                   PARM                    LUNG
430200161111      *
430300161111     C                   if        not *in91
430400161111      *
430500161111      * OVRPRTF BOLLE A5
430600161111     C                   MOVEA     D90PRB5       CMA5f6(30)
430700161111     C                   MOVEA     D90MDB5       CMA5f6(52)
430800161111     C*
430900161111     C                   Z-ADD     130           LUNG
431000161111     C                   CLEAR                   COMMAN
431100161111     C                   MOVEA     CMA5f6        COMMAN
431200161111     C                   CALL      'QCMDEXC'                            91
431300161111     C                   PARM                    COMMAN
431400161111     C                   PARM                    LUNG
431500161111     c                   endif
431600161111      *
431700070420     C                   CLEAR                   FNLSB5DS
431800070426     c                   movel     'L'           db0tla
431900070420     C                   MOVEL     'R'           DB0RIS
432000150617     C                   MOVEL     w_tbo         DB0TBO
432100070420     C                   MOVEL     ARBAAS        DB0AAS
432200070420     C                   MOVEL     ARBLNP        DB0LNP
432300070420     C                   MOVEL     ARBNRS        DB0NRS
432400070420     C                   MOVEL     ARBNSP        DB0NSP
432500130319     c                   reset                   fnlsB6ds1
432600070420     C                   CALL      D90PSL
432700070420     C                   PARM                    FNLSB5DS
432800130308     c                   parm                    fnlsB6ds1
432900160505     C                   MOVEL     MSG(1)        V1CMSG
433000070420     C                   SETON                                        28
433100070420     c                   endif
433200080201     c                   endif
433300070420     c                   endif
433400930525     C*
433500990119     C* F7 - ANOMALIE
433600040218do  1C   KG              DO
433700960523     C                   MOVEL     'S'           WCMD
433800960523     C*
433900991027     C                   CLEAR                   FNLr44DS
434000960523     C                   MOVE      V2CAAS        D44AAS
434100960523     C                   MOVE      COMLNP        D44FIL
434200960523     C                   MOVE      V2CNRS        D44NRS
434300960523     C                   MOVE      V2CNSP        D44NSP
434400150617     C***                MOVEL     'A'           D44TBO
434500150617     C                   MOVEL     w_tbo         D44TBO
434600960523     C                   MOVE      'I'           D44MOD
434700991027     C                   MOVEL     FNLr44DS      KPJBU
434800960523     C                   CALL      'FNLR44R'
434900960523     C                   PARM                    KPJBA
435000960523     C*
435100990119     C                   MOVEL     'S'           WRIEM
435200040218e   1C                   ENDDO
435300920804     C*
435400160628     C* F23 - INTERROGAZIONE EVENTI SEGNACOLLI
435500160628if  1C     *INKX         IFEQ      *ON
435600961121     C                   MOVEL     'S'           WCMD
435700961121     C*
435800991027     C                   CLEAR                   FNLR45DS
435900961121     C                   MOVE      V2CAAS        D45AAS
436000961121     C                   MOVE      COMLNP        D45FIL
436100961121     C                   MOVE      V2CNRS        D45NRS
436200961121     C                   MOVE      V2CNSP        D45NSP
436300991027     C                   MOVEL     FNLR45DS      KPJBU
436400961121     C                   CALL      'FNLR45R'
436500961121     C                   PARM                    KPJBA
436600961121     C*
436700990119     C                   MOVEL     'S'           WRIEM
436800040218e   1C                   ENDIF
436900160628     C* CMD 8 - VISUALIZZA ORM
437000160628if  1C     *INKH         IFEQ      *ON
437100160628     C                   MOVEL     'S'           WCMD
437200160628
437300160628     C                   clear                   PARM01
437400160628     C                   move      v3cpoe        ppoe
437500160628     C                   move      v3cnor        pnor
437600160628     C                   move      v3cnsr        pnsr
437700160628     C                   move      v3cnrv        pnrv
437800160628     C                   move      '5'           psce
437900160628     C                   move      v3clnp        pfgs
438000160628     C                   movel(p)  PARM01        KPJBU
438100160628
438200160628     C                   call      'FIOR07R'
438300160628     C                   parm                    KPJBA
438400160628     C                   MOVEL     'S'           WRIEM
438500160628
438600160628e   1C                   ENDIF
438700130830     c
438800130830     c* F9 - Orari servizi
438900131007     c                   if        *inki=*on
439000130830     C                   MOVEL     'S'           WCMD
439100131007     c                   if        wabif9='S'
439200150611     c                   clear                   W_video
439300130930     c                   exsr      sr_orarisr
439400131007     c                   endif
439500130830     C                   MOVEL     'S'           WRIEM
439600130830e   1C                   ENDIF
439700021213     C*
439800021213     C* F11 - Note supermercati
439900040218if  1C     *INKK         IFEQ      *ON
440000021213     C     *IN77         ANDEQ     *ON
440100021213      * imposto i campi 'bolla' della ds
440200021217     C                   MOVEL     'S'           WCMD
440300021213     C                   clear                   FnLrb3ds
440400030122     c                   Movel     V1cdsa        Rb3Rsu
440500030121     C                   z-add     SimFel        RB3Fil
440600021216     C                   z-add     V3CAAS        RB3AAS
440700030121     C                   z-add     V3CLNP        RB3LNP
440800021216     C                   z-add     V3CNRS        RB3NRS
440900021216     C                   z-add     V3CNSP        RB3NSP
441000021213     C                   z-add     V3CDSP        RB3DSP
441100021213     C                   z-add     VIDRMN        RB3RMN
441200021213     C                   z-add     V3CLNA        RB3LNA
441300131108     C                   movel     V32RSD        RB3RSD
441400021218     C                   z-add     VIDPKF        RB3PKF
441500021213     C                   z-add     VIDNCL        RB3NCL
441600021213     C                   z-add     VIDVLF        RB3VLB
441700030703      * bancali - part.varie "B"
441800040218If  2c                   If        %subst(ArbGva:1:1) = 'B'
441900030121     C                   z-add     §Ar5Tba       RB3Tba
442000030121     C                   z-add     VidNba        RB3Nba
442100030121     C                   z-add     §Ar5Tb2       RB3Tb2
442200030121     C                   z-add     VidNb2        RB3Nb2
442300030703     c                   Movel     V4cGva        Rb3Gva
442400040218e   2c                   EndIf
442500030703      * bancali - part.varie "O"
442600040218If  2c                   If        %subst(ArbGva:1:1) = 'O'
442700030703     C                   z-add     §Ar5bTba      RB3Tba
442800030703     C                   z-add     VidNba        RB3Nba
442900030703     C                   z-add     §Ar5bTb2      RB3Tb2
443000030703     C                   z-add     VidNb2        RB3Nb2
443100030703     c                   Movel     V4cGva        Rb3Gva
443200030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
443300030703     c                   Z-Add     §Ar5bCor      Rb3Cor
443400040218e   2c                   EndIf
443500021213      * richiamo PGM per note supermercati
443600021213     C                   call      'FNLRB3R'
443700030122     C                   PARM                    Kpjba
443800030121     C                   PARM                    Fnlrb3Ds
443900021213      *
444000021217     C                   MOVEL     'S'           WRIEM
444100040218e   1C                   ENDIF
444200961121     C*
444300990119     C* F14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
444400040218if  1C     *INKN         IFEQ      *ON
444500930624     C     *IN03         ANDEQ     *ON
444600160504     C     PARGIA        ANDEQ     *BLANK
444700940905     C                   MOVEL     'S'           WCMD
444800991027     C                   CLEAR                   FNLG30DS
444900941213     C                   MOVEL     'A'           D30POA
445000941213     C                   MOVEL     V2CAAS        D30AAS
445100941213     C                   MOVEL     COMLNP        D30LNP
445200941213     C                   MOVEL     V2CNRS        D30NRS
445300941213     C                   MOVEL     V2CNSP        D30NSP
445400991027     C                   MOVEL     FNLG30DS      KPJBU
445500941213     C                   CALL      'FNLG31C'
445600920804     C                   PARM                    KPJBA
445700930624     C*
445800990119     C                   MOVEL     'S'           WRIEM
445900040218e   1C                   ENDIF
446000040218      *
446100040218      *?F16 - Gestione reclami?
446200040218if  1c                   if        *inKQ = *on
446300040225     c                   eval      wARBaas = ARBaas
446400131111     c                   eval      wARBlnp = ARBlnp
446500131111     c                   eval      wARBnrs = ARBnrs
446600131111     c                   eval      wARBnsp = ARBnsp
446700040225     c                   reset                   FIDNA2ds
446800150402if  2c                   if        SDSprm  > 1  and ilru6ra='S'
446900040225     c                   eval      IA2mod  = 'I'
447000040225e   2c                   endif
447100040225     c                   eval      IA2ogg  = ds_OggSped
447200040225     c                   call      'FIDNA2R'
447300040225     c                   parm                    KPJBA
447400040225     c                   parm                    FIDNA2ds
447500040225     c                   movel     OA2f03        WF03
447600040225     c                   movel     'S'           WRIEM
447700051201     C                   MOVEL     'S'           WCMD
447800040218e   1c                   endif
447900070404      *
448000140311      *?F17 - Note AUT
448100160916      * (Questo tasto funzionale è attivo solo se la bolla è in arrivo. Se è
448200160916      *  solo in partenza non è attivo)
448300070404if  1c                   if        *inKR = *on
448400140123     c                   clear                   fnlrn6ds
448500140123     c                   eval      in6mod='V'
448600140123     c                   eval      in6aas=v2caas
448700140123     c                   eval      in6lnp=comlnp
448800140123     c                   eval      in6nrs=v2cnrs
448900140123     c                   eval      in6nsp=v2cnsp
449000140123      * se sono una spedizione in consegna passo i dati della distinta del file ARB
449100140123     c                   IF        ARBfbc = *blanks and
449200140123     c                               ARBddc > 0 and
449201161129     c                               ARBndc <> *all'8'  and
449202161129     c                               ARBndc <> *all'9'  and
449300140123     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
449400140123     c                               arbica='R' or arbicc=' ' or arbicc='R')
449401161129     c
449500160916     c***  KSPED         chain     Fnarb01l
449600160916     c****               If        %found(Fnarb01l)
449700140123     c                   eval      in6fgs = arbifp
449800140123     c                   eval      in6nfv = arbndc
449900140123     c                   eval      in6dfv = arbddc
450000140123     c                   eval      in6pdr = arbpdc
450100140123     c                   eval      in6cons = 'S'
450200160916     c***                Endif
450300140123     c                   Endif
450400140123     c                   call      'FNLRN6R'
450500070404     c                   parm                    kpjba
450600140123     c                   parm                    fnlrn6ds
450700070404     c                   movel     'S'           WRIEM
450800070404     C                   MOVEL     'S'           WCMD
450900070404e   1c                   endif
451000920804     C*
451100990119     C* F21 - DETTAGLIO SEGNACOLLI
451200040218do  1C   KV              DO
451300940905     C                   MOVEL     'S'           WCMD
451400950628     C                   MOVEL     'A'           PA1TBO
451500950629     C                   CLEAR                   PA1F03
451600160505     C                   CLEAR                   PA1FlG
451700160610
451800160610     c                   eval      pa1AAS=kaas
451900160610     c                   eval      pa1LNP=klnp
452000160610     c                   eval      pa1NRS=knrs
452100160610     c                   eval      pa1NSP=knsp
452200160916     c                   eval      pa1tfp=arbtfp
452300160916     c                   eval      pa1tfa=arbtfa
452400160610
452500160504     C                   MOVEL     PARAMint      KPJBU
452600161019     C                   CALL      'FNLRU38R'
452700920804     C                   PARM                    KPJBA
452800160504     C                   MOVEL     KPJBU         PARAMint
452900940802     C                   MOVEL     PA1F03        WF03
453000990119     C                   MOVEL     'S'           WRIEM
453100040218e   1C                   ENDDO
453200920804     C*
453300990119     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
453400040218do  1C   KJ              DO
453500940905     C                   MOVEL     'S'           WCMD
453600150617     C***                MOVEL     'A'           PA1TBO
453700150617     C                   MOVEL     w_tbo         PA1TBO
453800950629     C                   CLEAR                   PA1F03
453900160505     C                   CLEAR                   PA1flg
454000160610     c                   eval      pa1AAS=kaas
454100160610     c                   eval      pa1LNP=klnp
454200160610     c                   eval      pa1NRS=knrs
454300160610     c                   eval      pa1NSP=knsp
454400160504     C                   MOVEL     PARAMint      KPJBU
454500940726     C                   CALL      'FNLR75R'
454600920804     C                   PARM                    KPJBA
454700160504     C                   MOVEL     KPJBU         PARAMint
454800940802     C                   MOVEL     PA1F03        WF03
454900990119     C                   MOVEL     'S'           WRIEM
455000040218e   1C                   ENDDO
455100930114     C*
455200990119     C* F19 - INTERROGA SPUNTE VARI
455300040218if  1C     *INKT         IFEQ      *ON
455400940905     C                   MOVEL     'S'           WCMD
455500940713     C                   Z-ADD     0             PA4NFV
455600940713     C                   MOVEL     9             PA4NPG
455700940713     C                   MOVEL     99            PA4NPS
455800160519     C                   MOVEL     'FNLRU6R1'    PA4PGM
455900101203     C                   MOVEL     '1'           PA4FLG
456000950214     C                   MOVEL     V3CFLS        PA4LNP
456100940719     C                   MOVEL     V3CLNA        PA4LNA
456200940719     C                   MOVEL     V3CNRS        PA4NRS
456300940713     C                   Z-ADD     0             PA4NCD
456400940713     C                   Z-ADD     0             PA4NCA
456500101201     c                   eval      pa4boan=v2caas
456600101201     c                   eval      pa4bolp=comlnp
456700101201     c                   eval      pa4bors=v2cnrs
456800101201     c                   eval      pa4bosp=v2cnsp
456900040218if  2C     VIDNCD        IFGT      0
457000940713     C                   MOVEL     VIDNCD        PA4NCD
457100940713     C                   MOVEL     VIDNCA        PA4NCA
457200101201x   2C**                 ELSE
457300930705     C* CERCO I SEGNACOLLI DAL DETTAGLIO
457400101201     C**   KARB2         SETLL     FNART01L
457500101201     C**   KARB2         READE     FNART01L                               31
457600101201do  3C*+N31              DO
457700101201     C**                 Z-ADD     ARTNSC        PA4NCD
457800101201     C**   KARB2         SETGT     FNART01L
457900101201     C**   KARB2         READPE    FNART01L                               31
458000101201if  4C**N31ARTNSC        IFNE      PA4NCD
458100101201     C**                 MOVEL     ARTNSC        PA4NCA
458200101201e   4C**                 ENDIF
458300101201e   3C**                 ENDDO
458400101201e   2C                   ENDIF
458500930705     C*
458600940802     C                   MOVEL     PARA04        KPJBU
458700940722     C                   CALL      'FNLR04R'
458800920804     C                   PARM                    KPJBA
458900940725     C                   MOVEL     'S'           WRIEM
459000060130     C                   MOVEL     'S'           WCMD
459100040218e   1C                   ENDIF
459200930415     C*
459300070404     c
459400931112      *
459500990119      * F20 - INTERROGAZIONE AFFIDABILITA'
459600040218do  1C   KU              DO
459700940905     C                   MOVEL     'S'           WCMD
459800950102     C                   CLEAR                   FNLR97
459900950102     C                   MOVEL     V2CAAS        R97AAS
460000950102     C                   MOVEL     COMLNP        R97LNP
460100950102     C                   MOVEL     V2CNRS        R97NRS
460200950102     C                   MOVEL     V2CNSP        R97NSP
460300950102     C                   MOVEL     V3CDSP        R97DSP
460400950102     C                   MOVEL     VIDRMN        R97RMN
460500950102     C                   MOVEL     V3CLNA        R97LNA
460600950102     C                   MOVEL     V3DLNA        R97DLA
460700950102     C                   Z-ADD     VIDDCP        R97DCP
460800950102     C                   MOVEL     V4CRSD        R97RSD
460900950102     C                   MOVEL     V1CDSA        R97RSU
461000040121     C                   MOVEL     *blanks       R97CML
461100160506     C                   MOVEL     w_tbo         R97AP
461200950102     C   95              MOVEL     'S'           R97EVE
461300070404     C                   Z-ADD     w08DBR        R97DBR
461400070404     C                   Z-ADD     w08DPC        R97DPC
461500070404     C                   Z-ADD     w08DUC        R97DUC
461600950102     C                   MOVEL     VIDTMC        R97TMC
461700950102     C                   Z-ADD     VIDCAS        R97CAS
461800950102     C                   MOVEL     VIDVCA        R97VCA
461900931112     C                   MOVE      *BLANKS       KPJBU
462000950102     C                   MOVEL     FNLR97        KPJBU
462100940722     C                   CALL      'FNLR97R'                            90
462200931112     C                   PARM                    KPJBA
462300950102     C                   MOVEL     KPJBU         FNLR97
462400950102     C                   MOVEL     R97F03        WF03
462500950102     C                   MOVEL     'S'           WRIEM
462600040218e   1C                   ENDDO
462700110930     C* F22 - Perfect Order
462800110929do  1C                   if        *inkw
462900110929     C                   MOVEL     'S'           WCMD
463000160606     c                   movel     paramU6ds1    paramu6ds
463100110929     c*
463200160527     c                   MOVEL     'W'           parflg_
463300160527     c                   movel(p)  vidrma        parrma_
463400160527     c                   movel(p)  vidxco        parxco_
463500160527     c                   clear                   parflo_
463600160527     c                   movel     paramU6ds     kpjbu
463700160519     c                   call      'FNLRU6C'
463800110929     c                   parm                    kpjba
463900110929     c*
464000110929     C*
464100110929     C                   MOVEL     'S'           WRIEM
464200110929e   1C                   ENDif
464300940905     C*
464400131107     C* ? FORMATO VIDEO:  LRI6D06  (TASSAZIONE 1a BOLLA)   ---   TASTO F13 ?
464500991026     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
464600040218if  1C     *INKM         IFEQ      *ON
464700991027     C                   eval      WCMD = 'S'
464800991027     C                   EVAL      *in89 = *off
464900131107     C                   WRITE     LRI6T01
465000131107     C                   WRITE     LRI6D06
465100991027     C                   eval      NR6 = 1
465200131107     C                   EXFMT     LRI6C06
465300040218e   1C                   ENDIF
465400991027     C*
465500131107     C* ? FORMATO VIDEO:  LRI6D09  (TASSAZIONE 2a BOLLA)   ---   TASTO F04 ?
465600991027     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
465700040218if  1C     *INKD         IFEQ      *ON
465800991027     C                   eval      WCMD = 'S'
465900991027     C                   EVAL      *in89 = *off
466000131107     C                   WRITE     LRI6T01
466100131107     C                   EXFMT     LRI6D09
466200040218e   1C                   ENDIF
466300990119     C*
466400990119     C* F2 - INTERROGAZIONE COMUNICAZIONE ANOMALIA
466500990129     C*      NON RICHIAMO SE RICHIAMATO DA INTERROGAZINE C.A.
466600040218if  1C     *INKB         IFEQ      *ON
466700160504     C     PArFLG        ANDNE     'C'
466800040218if  2C     CONTCA        IFEQ      1
466900991027     C                   CLEAR                   FIDN00DS
467000990119     C                   Z-ADD     VHCAAC        I00AAC
467100990119     C                   Z-ADD     VHCFIL        I00FIL
467200990119     C                   Z-ADD     VHCNCA        I00NCA
467300990119     C                   Z-ADD     ARBAAS        I00AAS
467400990119     C                   Z-ADD     ARBNRS        I00NRS
467500990119     C                   Z-ADD     ARBNSP        I00NSP
467600990119     C                   Z-ADD     ARBLNP        I00LNP
467700990119     C                   MOVEL     'A'           I00TPB
467800991027     C                   MOVEL     FIDN00DS      KPJBU
467900990119     C                   CALL      'FIDN01R'
468000990119     C                   PARM                    KPJBA
468100000309     C                   PARM                    callricors        1
468200040218x   2C                   ELSE
468300990119     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
468400990119     C                   SETOFF                                       79
468500131107     C                   WRITE     LRI6C11
468600990119     C                   SETON                                        797823
468700990119     C                   CLEAR                   NRR11
468800040809      *
468900040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
469000040809      * legate alla spedizione che passo
469100040809     c                   clear                   fidn12ds
469200040809     c                   eval      i12aas = arbAAS
469300040809     c                   eval      i12lnp = arbLNP
469400040809     c                   eval      i12nrs = arbNRS
469500040809     c                   eval      i12nsp = arbNSP
469600040809     c                   eval      i12fel = 'E'
469700040809     c                   eval      i12fan = 'E'
469800040809     c*
469900040809     c                   call      'FIDN12R'
470000040809     c                   parm                    fidn12ds
470100040809     c*
470200040809     c                   setoff                                       31
470300040809     c*
470400040809     c* se non ci sono errori
470500040809     c                   if        o12err <> ' '
470600040809     c                   seton                                        31
470700040809     c                   else
470800040809     c* se numero di CA trovate maggiore di zero
470900040809     c                   if        o12nca = 0
471000040809     c                   seton                                        31
471100040809     c                   else
471200040809     c                   z-add     1             ii                2 0
471300040809     c                   dow       ii <= o12nca and *in31 = *off
471400040809     c                   movea     skkey(ii)     dskey
471500040809     C                   Z-ADD     dsaac         V11AAC
471600040809     C                   Z-ADD     dsfil         V11FIL
471700040809     C                   Z-ADD     dsnca         V11NCA
471800040809     C                   Z-ADD     skdca(ii)     G02INV
471900040809     C                   Z-ADD     *ZERO         G02DAT
472000040809     C                   MOVEL     '3'           G02ERR
472100040809     C                   CALL      'XSRDA8'
472200040809     C                   PARM                    WLBDAT
472300040809     C                   Z-ADD     G02DAT        V11DCA
472400040809     C                   Z-ADD     skfca(ii)     V11FCA
472500040809     C     skfca(ii)     CHAIN     FNDFA01L
472600040809     C                   IF        %found(FNDFA01L)
472700040809     C                   MOVEL     DFADES        V11DFA
472800040809     C                   ENDIF
472900040809     C**
473000040809     C                   ADD       1             NRR11             4 0
473100131107     C                   WRITE     LRI6S11
473200040809     c*
473300040809     c                   add       1             ii
473400040809     c                   enddo
473500040809     c*
473600040809     c                   seton                                        31
473700040809     c*
473800040809     c                   endif
473900040809     c                   endif
474000040809     c**
474100990119     C* EMETTO FORMATO
474200990119     C     FOR11         TAG
474300990119     C                   Z-ADD     1             NRR11
474400131107     C                   WRITE     LRI6T01
474500131107     C                   WRITE     LRI6Z11
474600131107     C                   EXFMT     LRI6C11
474700990119     C* F12 - RITORNO
474800040218if  3C     *INKL         IFEQ      *OFF
474900131107     C                   READC     LRI6S11                                31
475000040218do  4C     *IN31         DOWEQ     *OFF
475100040218if  5C     V11SCE        IFEQ      '5'
475200991027     C                   CLEAR                   FIDN00DS
475300990119     C                   Z-ADD     V11AAC        I00AAC
475400990119     C                   Z-ADD     V11FIL        I00FIL
475500990119     C                   Z-ADD     V11NCA        I00NCA
475600990119     C                   Z-ADD     ARBAAS        I00AAS
475700990119     C                   Z-ADD     ARBNRS        I00NRS
475800990119     C                   Z-ADD     ARBNSP        I00NSP
475900990119     C                   Z-ADD     ARBLNP        I00LNP
476000990119     C                   MOVEL     'A'           I00TPB
476100991027     C                   MOVEL     FIDN00DS      KPJBU
476200990119     C                   CALL      'FIDN01R'
476300990119     C                   PARM                    KPJBA
476400000309     C                   PARM                    callricors        1
476500990119     C**
476600990119     C                   CLEAR                   V11SCE
476700131107     C                   UPDATE    LRI6S11
476800040218e   5C                   ENDIF
476900990119     C**
477000131107     C                   READC     LRI6S11                                31
477100040218e   4C                   ENDDO
477200990119     C                   GOTO      FOR11
477300040218e   3C                   ENDIF
477400040218e   2C                   ENDIF
477500990119     C*
477600990119     C                   MOVEL     'S'           WRIEM
477700990119     C                   MOVEL     'S'           WCMD
477800040218e   1C                   ENDIF
477900931112      *
478000920804     C                   ENDSR
478100070420     C**************************************************************************
478200070420      * RICHIEDO LE STAMPANTI per stampa LDV
478300070420     C**************************************************************************
478400070420     c     RICHSTAMPAN   BEGSR
478500070420     C                   CLEAR                   TRUL90DS
478600070420      *
478700070420     C                   MOVEL     'S'           d90RSB
478800070420     C                   CALL      'TRUL90R'
478900070420     C                   PARM                    KPJBA
479000070420     C                   PARM                    trul90DS
479100070420      *
479200070420     C* F3 - FINE
479300070420     c                   if        d90f3<>'1'
479400070420      *
479500070420      * OVRPRTF BOLLE A4
479600070426     C                   MOVEA     D90PRB4       CMA4f6(30)
479700070426     C                   MOVEA     D90MDB4       CMA4f6(52)
479800070420     C*
479900111221     C                   Z-ADD     130           LUNG             15 5
480000111221     C                   CLEAR                   COMMAN          130
480100160503     C                   MOVEA     CMA4f6        COMMAN
480200070420     C                   CALL      'QCMDEXC'                            91
480300070420     C                   PARM                    COMMAN
480400070420     C                   PARM                    LUNG
480500070420      *
480600070420     C                   if        not *in91
480700070420      *
480800070420      * OVRPRTF BOLLE A5
480900070426     C                   MOVEA     D90PRB5       CMA5f6(30)
481000070426     C                   MOVEA     D90MDB5       CMA5f6(52)
481100070420     C*
481200111221     C                   Z-ADD     130           LUNG
481300070420     C                   CLEAR                   COMMAN
481400160503     C                   MOVEA     CMA5f6        COMMAN
481500070420     C                   CALL      'QCMDEXC'                            91
481600070420     C                   PARM                    COMMAN
481700070420     C                   PARM                    LUNG
481800070420     c                   endif
481900070420     c                   endif
482000070420      *
482100070420     c                   ENDSR
482200940802     C**************************************************************************
482300940802     C*    VISUALIZZA LEGAMI BOLLA
482400940802     C**************************************************************************
482500930413     C     CMD18         BEGSR
482600940802     C*
482700940411     C                   CLEAR                   WCAR
482800160610     c                   eval      w_tbo_sav=w_tbo
482900160913     c                   eval      blp_arbdcm_sav=blp_arbdcm
483000161014     c                   eval      blp_arbhmc_sav=blp_arbhmc
483100160610     c                   eval      win12=*in12
483200940411     C*
483300940411     C* MUOVO I DATI DELLA BOLLA CHE STO VISUALIZZANDO
483400940714     C                   MOVEL     V2CAAS        NUM2              4 0
483500940411     C                   MOVEL     COMLNP        COMLPN
483600940714     C                   MOVEL     V2CNRS        COMNRN
483700940714     C                   MOVEL     V2CNSP        COMNSN
483800950321     C                   MOVEL     V3CLNA        COMLAN
483900950428     C                   MOVEL     COMC08        SAVC08
484000940408     C**
484100940713     C* SAVNR8 > 1 --> GESTIONE SUBFILE
484200940408     C**
484300940907    1C     SAVNR8        IFGT      2
484400940408     C*
484500940809     C                   MOVEL     'S'           WRIEM
484600930413     C     FORC2         TAG
484700940713     C                   Z-ADD     1             REC8
484800940809     C     WRIEM         IFEQ      'S'
484900131107     C                   WRITE     LRI6T01
485000131107     C                   WRITE     LRI6Z08
485100940809     C                   MOVEL     *BLANKS       WRIEM
485200940809     C                   ENDIF
485300950428     C*
485400950428     C                   MOVEL     SAVC08        COMC08
485500131107     C                   EXFMT     LRI6C08
485600930413     C*
485700930413     C* CMD12 - FINE LAVORO
485800930413     C   KL              GOTO      SOTTO
485900930413     C*
486000131107     C                   READC     LRI6S08                                93
486100930510     C   93              GOTO      SOTTO
486200930510     C*
486300940408    2C     *IN93         DOWEQ     '0'
486400930413     C*
486500941230    3C     V8CSCE        IFEQ      '5'
486600940713     C                   MOVEL     ' '           V8CSCE
486700950103     C*
486800950103     C* RIPRISTINO GLI INDICATORI
486900950103     C                   MOVEL     V8CI80        *IN80
487000950103     C                   MOVEL     V8CI81        *IN81
487100950103     C                   MOVEL     V8CI82        *IN82
487200950103     C                   MOVEL     V8CI83        *IN83
487300131107     C                   UPDATE    LRI6S08
487400930504     C*
487500940907     C     V8CAAN        IFEQ      *ZERO
487600940907     C                   MOVEL     V8CAAP        LBLAAN
487700940907     C                   MOVEL     V8CLPP        LBLLPN
487800940907     C                   MOVEL     V8CNRP        LBLNRN
487900940907     C                   MOVEL     V8CNSP        LBLNSN
488000940907     C                   MOVEL     'P'           V8CFPS            1
488100940907     C                   ELSE
488200940907     C                   MOVEL     V8CAAN        LBLAAN
488300940907     C                   MOVEL     V8CLPN        LBLLPN
488400940907     C                   MOVEL     V8CNRN        LBLNRN
488500940907     C                   MOVEL     V8CNSN        LBLNSN
488600940907     C                   MOVEL     'S'           V8CFPS
488700940907     C                   ENDIF
488800940408     C*
488900940408     C* CONTROLLO SE IL LEGAME E' IN PARTENZA O IN ARRIVO
489000160610     c                   clear                   w_tbo
489100160913     c                   clear                   blp_arbdcm
489200161014     c                   clear                   blp_arbhmc
489300150423     C     KLBL1         CHAIN     FNblp01L                           93
489400160610     c                   if        %found(fnblp01l)
489500160610     c                   eval      w_tbo='P'
489600160913     c                   eval      blp_arbdcm=arbdcm
489700161014     c                   eval      blp_arbhmc=arbhmc
489800160610     c                   seton                                        12
489900160610     c                   endif
490000940907     C     KLBL1         CHAIN     FNARB01L                           93
490100160610     c                   if        %found(fnarb01l)
490200160610     c                   eval      w_tbo='A'
490300160610     c                   setoff                                       12
490400160610     c                   endif
490500940408     C*
490600150423    4C**** *IN93         IFEQ      *OFF
490700940408     C*
490800940408     C                   EXSR      LEG2
490900940408     C*
491000150423   X4C****               ELSE
491100940408     C*
491200150423     C*                  Z-ADD     LBLAAN        PA0AAS
491300150423     C*                  Z-ADD     LBLLPN        PA0LNP
491400150423     C*                  Z-ADD     LBLNRN        PA0NRS
491500150423     C*                  Z-ADD     LBLNSN        PA0NSP
491600150423     C*                  MOVEL     rsut          PA0RSU
491700150423     C*                  MOVEL     SIMFEL        PA0SIM
491800150423     C*                  MOVEL     *blanks       PA0CML
491900150423     C*    PA1FLG        IFEQ      'N'
492000960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
492100960903     C*       verifichi questo caso
492200150423     C*                  MOVEL     'R'           PA0FLG
492300150423     C*                  ELSE
492400150423     C*                  MOVEL     'A'           PA0FLG
492500150423     C*                  END
492600150423     C*                  MOVEL     *BLANKS       PA0CLI
492700150423     C*                  CLEAR                   PA0F03
492800150423     C*                  MOVEL     *BLANKS       PA0GIA
492900150423     C*                  MOVEL     PARAM0        KPJBU
493000150423     C*                  CALL      'FNLS05R'
493100150423     C*                  PARM                    KPJBA
493200150423     C*                  MOVEL     KPJBU         PARAM0
493300150423     C*                  MOVEL     PA0F03        WF03
493400150423     C*                  MOVEL     'S'           WRIEM
493500150423    4C*                  ENDIF
493600940408     C*
493700940408    3C                   ENDIF
493800930413     C*
493900131107     C                   READC     LRI6S08                                93
494000940408    2C                   ENDDO
494100930413     C                   GOTO      FORC2
494200930413     C*
494300930413     C     SOTTO         TAG
494400930413     C*
494500940408    1C                   ELSE
494600940408     C*
494700940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
494800131107     C     2             CHAIN     LRI6S08                            99
494900940907     C     V8CAAP        IFEQ      V3CAAS
495000940907     C     V8CLPP        ANDEQ     V3CLNP
495100940907     C     V8CNRP        ANDEQ     V3CNRS
495200940907     C     V8CNSP        ANDEQ     V3CNSP
495300940907     C                   MOVEL     V8CAAN        ARBAAS
495400940907     C                   MOVEL     V8CLPN        ARBLNP
495500940907     C                   MOVEL     V8CNRN        ARBNRS
495600940907     C                   MOVEL     V8CNSN        ARBNSP
495700940907     C                   MOVEL     'S'           V8CFPS
495800940907     C                   ELSE
495900940907     C                   MOVEL     V8CAAP        ARBAAS
496000940907     C                   MOVEL     V8CLPP        ARBLNP
496100940907     C                   MOVEL     V8CNRP        ARBNRS
496200940907     C                   MOVEL     V8CNSP        ARBNSP
496300940907     C                   MOVEL     'P'           V8CFPS
496400940907     C                   ENDIF
496500940907     C* QUINDI VEDO SE LA TROVO FRA GLI ARRIVI
496600150423     C     KARB2         CHAIN     FNblp01L                           93
496700160610     c                   if        %found(fnblp01l)
496800160610     c                   eval      w_tbo='P'
496900160913     c                   eval      blp_arbdcm=arbdcm
497000161014     c                   eval      blp_arbhmc=arbhmc
497100160610     c                   seton                                        12
497200160610     c                   endif
497300940907     C     KARB2         CHAIN     FNARB01L                           93
497400160610     c                   if        %found(fnarb01l)
497500160610     c                   eval      w_tbo='A'
497600160610     c                   setoff                                       12
497700160610     c                   endif
497800940408     C*
497900150423    2C*    *IN93         IFEQ      *ON
498000150423     C*                  Z-ADD     ARBAAS        PA0AAS
498100150423     C*                  Z-ADD     ARBLNP        PA0LNP
498200150423     C*                  Z-ADD     ARBNRS        PA0NRS
498300150423     C*                  Z-ADD     ARBNSP        PA0NSP
498400150423     C*                  MOVEL     rsut          PA0RSU
498500150423     C*                  MOVEL     SIMFEL        PA0SIM
498600150423     C*                  MOVEL     *blanks       PA0CML
498700150423     C*    PA1FLG        IFEQ      'N'
498800960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
498900960903     C*       verifichi questo caso
499000150423     C*                  MOVEL     'R'           PA0FLG
499100150423     C*                  ELSE
499200150423     C*                  MOVEL     'A'           PA0FLG
499300150423     C*                  END
499400150423     C*                  MOVEL     *BLANKS       PA0CLI
499500150423     C*                  CLEAR                   PA0F03
499600150423     C*                  MOVEL     *BLANKS       PA0GIA
499700150423     C*                  MOVEL     PARAM0        KPJBU
499800150423     C*                  CALL      'FNLS05R'
499900150423     C*                  PARM                    KPJBA
500000150423     C*                  MOVEL     KPJBU         PARAM0
500100150423     C*                  MOVEL     PA0F03        WF03
500200150423     C*                  MOVEL     'S'           WRIEM
500300121116     c* reimposto la key spedizione
500400150423     C*                  MOVEL     NUM2          arbAAS
500500150423     C*                  MOVEL     COMLPN        arbLNP
500600150423     C*                  MOVEL     COMNRN        arbNRS
500700150423     C*                  MOVEL     COMNSN        arbNSP
500800930510     C*
500900150423   X2C*                  ELSE
501000940408     C*
501100940408     C                   EXSR      LEG2
501200940408     C*
501300150423    2C*                  END
501400940408    1C                   END
501500160610
501600160610     c                   eval      w_tbo=w_tbo_sav
501700160913     c                   eval      blp_arbdcm=blp_arbdcm_sav
501800161014     c                   eval      blp_arbhmc=blp_arbhmc_sav
501900160610     c                   eval      *in12=win12
502000940411     C*
502100940411     C* RICARICO I DATI DELLA BOLLA INIZIALE
502200940411     C     WCAR          IFEQ      'C'
502300940411     C                   SETOFF                                       75
502400940714     C                   MOVEL     NUM2          V2CAAS
502500940411     C                   MOVEL     COMLPN        COMLNP
502600940714     C                   MOVEL     COMNRN        V2CNRS
502700940714     C                   MOVEL     COMNSN        V2CNSP
502800160610     c     karb          chain     fnblp01l
502900940712     C     KARB          CHAIN     FNARB01L                           93
503000940411     C* RICARICO I DATI
503100940411     C                   EXSR      BOLLA
503200940411     C*
503300940411     C* RIABILITO IL CMD18
503400940411     C                   SETON                                        94
503500940411    1C                   END
503600940802     C*
503700930413     C                   ENDSR
503800940802     C**************************************************************************
503900940802     C*    LEGAMI BOLLA
504000940802     C**************************************************************************
504100940408     C     LEG2          BEGSR
504200940802     C*
504300940411     C                   MOVEL     'C'           WCAR              1
504400940408     C*
504500940408     C* 75 ON - NON RIENTRO NEL CARICAMENTO LEGAME BOLLA
504600940408     C                   SETON                                        75
504700940408     C* 94 OFF- DISABILITO IL CMD18
504800940408     C                   SETOFF                                       94
504900940408     C* CARICO DATI BOLLA
505000940408     C                   EXSR      BOLLA
505100940411     C*
505200940411     C* VISUALIZZA NUOVO LEGAME
505300940408     C* SE LA BOLLA E' UNA PRECEDENTE --> VISUALIZZO LA SEGUENTE
505400940713     C     V8CFPS        IFEQ      'P'
505500950316     C                   MOVEA     'SEGUEN.'     LBL(14)
505600940408     C                   ELSE
505700940408     C* SE LA BOLLA E' UNA SEGUENTE   --> VISUALIZZO LA PRECEDENTE
505800950316     C                   MOVEA     'PRECED.'     LBL(14)
505900940408     C                   ENDIF
506000950316     C                   MOVEA     COMLPN        LBL(21)
506100950316     C                   MOVEA     COMNRN        LBL(25)
506200950316     C                   MOVEA     COMNSN        LBL(28)
506300950316     C                   MOVEA     COMLAN        LBL(40)
506400950307     C                   MOVEA     *BLANKS       LBL(44)
506500940408     C                   MOVEA     LBL           LEGAME
506600940408     C*
506700940411     C* MOUVO I DATI DELLA LEGATA NEI CAMPI DELLA PARAM
506800940714     C                   MOVEL     ARBAAS        V2CAAS
506900940411     C                   MOVEL     ARBLNP        COMLNP
507000940714     C                   MOVEL     ARBNRS        V2CNRS
507100940714     C                   MOVEL     ARBNSP        V2CNSP
507200940408     C*
507300940408     C                   EXSR      VISUA2
507400940408     C*
507500941230     C                   MOVEL     'S'           WRIEM
507600940408     C                   ENDSR
507700940408     C*
507800940712      **************************************************************************
507900131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
508000940408      *     PER BOLLA NORMAL - CMD18 PREMUTO DA VISUAL -
508100091216      *     ricordarsi di aggiornare anche RIEPI2 per ogni modifica fatta qui
508200940712      **************************************************************************
508300940712     C     RIEPIL        BEGSR
508400940712      *
508500940712     C                   SETOFF                                       89
508600940712      *
508700940712      * EMETTO IL FORMATO VIDEO
508800940720     C                   MOVEL     'S'           WRIEM
508900940712     C     FOR07         TAG
509000940720     C     WRIEM         IFEQ      'S'
509100131107     C                   WRITE     LRI6T01
509200940720     C                   MOVEL     *BLANKS       WRIEM
509300940720     C                   ENDIF
509400021217      * Imposto la stringa comandi del 'giro' successivo
509500021217     C                   IF        *inKY = *on and $Loop > 1
509600021217     c                   Exsr      Ges_F24
509700021217     c                   eval      FD07 = FDxx
509800021217     c                   eval      F24D07 = F24Dxx
509900021217     C                   ENDIF
510000131107     C                   EXFMT     LRI6D07
510100070420     c                   setoff                                       28
510200070420     c                   clear                   v1cmsg
510300940712      *
510400131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
510500940712     C   KY*IN89         IFEQ      *ON
510600940712     C                   SETOFF                                       89
510700940712     C                   ELSE
510800940712     C                   SETON                                        89
510900940712     C                   ENDIF
511000940712     C   KY              GOTO      FOR07
511100940712      *
511200131108      * F3 - FINE LAVORO TORNO A SUBFILE
511300940714     C   KC              MOVEL     '2'           V2CFLG            1
511400940712     C   KC              GOTO      ENDRIE
511500940712      *
511600131108      * F18 - INTERROGA LEGAME BOLLA
511700940712     C   KS              DO
511800940712     C                   EXSR      CMD18
511900940712     C                   GOTO      FOR07
512000940712     C                   END
512100940712      *
512200940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
512300940712     C                   EXSR      COMAND
512400940905     C     WCMD          IFEQ      'S'
512500940802     C     WF03          IFEQ      *BLANKS
512600070404     c
512700940802     C                   GOTO      FOR07
512800940802     C                   ELSE
512900940802     C                   MOVEL     '2'           V2CFLG
513000940802     C                   GOTO      ENDRIE
513100070404     C                   ENDIF
513200091215     c
513300070404     c                   else
513400070404     c
513500070404     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
513600070404     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
513700070404     c                   if        v3cgesd='?'
513800131120
513900070404     c                   exsr      VISUALAbi
514000070404     c                   clear                   v3cgesd
514100070412     c                   eval      wriem='S'
514200070404     c                   endif
514300131120     c
514400131120     c* controlla se immessi dei '?'
514500131120     c                   eval      wvideo='R'
514600131120     c                   exsr      CTR_puIn
514700091215     c
514800091215     c                   if        wriem='S'
514900091215     C                   GOTO      FOR07
515000091215     c                   endif
515100070404     c
515200940712     C                   END
515300940712      *
515400940712     C     ENDRIE        ENDSR
515500940712      **************************************************************************
515600131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
515700940408      *     PER LEGAME BOLLA - CMD18 PREMUTO DA VISUA2 -
515800940712      **************************************************************************
515900940712     C     RIEPI2        BEGSR
516000940712      *
516100940712     C                   SETOFF                                       89
516200940712      *
516300940712      * EMETTO IL FORMATO VIDEO
516400940809     C                   MOVEL     'S'           WRIEM
516500940712     C     FOR77         TAG
516600940809     C     WRIEM         IFEQ      'S'
516700131107     C                   WRITE     LRI6T01
516800940809     C                   MOVEL     *BLANKS       WRIEM
516900940809     C                   ENDIF
517000091216      * Imposto la stringa comandi del 'giro' successivo
517100091216     C                   IF        *inKY = *on and $Loop > 1
517200091216     c                   Exsr      Ges_F24
517300091216     c                   eval      FD07 = FDxx
517400091216     c                   eval      F24D07 = F24Dxx
517500091216     C                   ENDIF
517600091216     c
517700131107     C                   EXFMT     LRI6D07
517800070516     c                   setoff                                       28
517900070516     c                   clear                   v1cmsg
518000940408     C*
518100131108      * F3 - FINE LAVORO TORNO A SUBFILE
518200940714     C   KC              MOVEL     '2'           V2CFLG            1
518300940408     C*
518400940408     C  NKC              DO
518500940712      *
518600131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
518700940712     C   KY*IN89         IFEQ      *ON
518800940712     C                   SETOFF                                       89
518900940712     C                   ELSE
519000940712     C                   SETON                                        89
519100940712     C                   ENDIF
519200940712     C   KY              GOTO      FOR77
519300940712      *
519400940712      *
519500131108      * F18 - INTERROGA LEGAME BOLLA
519600940712     C   KS              DO
519700940712     C                   EXSR      CMD18
519800940712     C                   GOTO      FOR77
519900940712     C                   END
520000940712      *
520100940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
520200940712     C                   EXSR      COMAND
520300940905     C     WCMD          IFEQ      'S'
520400940712     C                   GOTO      FOR77
520500091216     c
520600091216     c                   else
520700091216     c
520800091216     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
520900091216     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
521000091216     c                   if        v3cgesd='?'
521100131120
521200091216     c                   exsr      VISUALAbi
521300091216     c                   clear                   v3cgesd
521400091216     c                   eval      wriem='S'
521500091216     c                   endif
521600131120
521700131120     c* controlla se immessi dei '?'
521800131120     c                   eval      wvideo='R'
521900131120     c                   exsr      CTR_puIn
522000091216     c
522100091216     c                   if        wriem='S'
522200091216     C                   GOTO      FOR77
522300091216     c                   endif
522400091216     c
522500940712     C                   END
522600091216     C                   END
522700940712      *
522800940712     C                   ENDSR
522900940923      **************************************************************************
523000940923      *     CONTROLLI FORMATO VIDEO D04
523100940923      **************************************************************************
523200131120     C     CTR_puIn      BEGSR
523300110601     c*
523400110601     c* verifico se inserito '?' su email destinatario
523500110601     c                   if        v4cemw='?'
523600110601     c                   exsr      sr_emdest
523700110601     C                   MOVEL     'S'           WRIEM
523800110601     c                   endif
523900940923      *
524000091215      * Verifico se inserito '?' per visualizzazione beneficiario
524100091215     c                   if        vidben='?'
524200131120     c
524300091215     c                   exsr      sr_bencas
524400091215     C                   MOVEL     'S'           WRIEM
524500091215     c                   endif
524600131114     c
524700131114     c* Visualizzazione dati filiale
524800131114     c                   if        viddec_f='?'
524900131120
525000131114     c                   clear                   trulorfds
525100131114     c                   eval      iorffil=arblna
525200131114     c                   call      'TRULORFR'
525300131114     c                   parm                    kpjba
525400131114     c                   parm                    trulorfds
525500131114     c
525600131114     c                   clear                   viddec_f
525700131114     C                   MOVEL     'S'           WRIEM
525800131114     c                   endif
525900091215     c
526000940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
526100940923     C     V4IGCA        IFEQ      '?'
526200991027     C                   CLEAR                   DS7PQRS
526300941003     C                   MOVEL     'S'           DS7RIC
526400941003     C                   MOVEL     'V'           DS7GES
526500941003     C                   MOVEL     V4CGCA        DS7COD
526600991027     C                   MOVEL     DS7PQRS       KPJBU
526700960126     C                   CALL      'TRTB72R'
526800941003     C                   PARM                    KPJBA
526900940923     C                   MOVEL     *BLANKS       V4IGCA
527000940923     C                   MOVEL     'S'           WRIEM
527100940923     C                   ENDIF
527200940923     C*
527300940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
527400940923     C     V4IGGA        IFEQ      '?'
527500991027     C                   CLEAR                   DS7PQRS
527600941003     C                   MOVEL     'S'           DS7RIC
527700941003     C                   MOVEL     'V'           DS7GES
527800941003     C                   MOVEL     V4CGGA        DS7COD
527900991027     C                   MOVEL     DS7PQRS       KPJBU
528000960126     C                   CALL      'TRTB69R'
528100941003     C                   PARM                    KPJBA
528200940923     C                   MOVEL     *BLANKS       V4IGGA
528300940923     C                   MOVEL     'S'           WRIEM
528400940923     C                   ENDIF
528500940923     C*
528600940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
528700940923     C     V4IGMA        IFEQ      '?'
528800991027     C                   CLEAR                   DS7PQRS
528900941003     C                   MOVEL     'S'           DS7RIC
529000941003     C                   MOVEL     'V'           DS7GES
529100941003     C                   MOVEL     V4CGMA        DS7COD
529200991027     C                   MOVEL     DS7PQRS       KPJBU
529300960126     C                   CALL      'TRTB70R'
529400941003     C                   PARM                    KPJBA
529500940923     C                   MOVEL     *BLANKS       V4IGMA
529600940923     C                   MOVEL     'S'           WRIEM
529700940923     C                   ENDIF
529800940923     C*
529900940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
530000940923     C     V4IGVA        IFEQ      '?'
530100991027     C                   CLEAR                   DS7PQRS
530200941003     C                   MOVEL     'S'           DS7RIC
530300941003     C                   MOVEL     'V'           DS7GES
530400941003     C                   MOVEL     V4CGVA        DS7COD
530500991027     C                   MOVEL     DS7PQRS       KPJBU
530600960126     C                   CALL      'TRTB73R'
530700941003     C                   PARM                    KPJBA
530800940923     C                   MOVEL     *BLANKS       V4IGVA
530900940923     C                   MOVEL     'S'           WRIEM
531000940923     C                   ENDIF
531100940923      *
531200940923     C                   ENDSR
531300940321      **************************************************************************
531400940713      *     R O U T I N E      I N I Z I A L E
531500940713      **************************************************************************
531600940713     C     *INZSR        BEGSR
531700940713      *
531800940713     C     KARB          KLIST
531900940714     C                   KFLD                    V2CAAS
532000940713     C                   KFLD                    COMLNP            3 0
532100940714     C                   KFLD                    V2CNRS
532200940714     C                   KFLD                    V2CNSP
532300940713     C     KARB2         KLIST
532400940713     C                   KFLD                    ARBAAS
532500940713     C                   KFLD                    ARBLNP
532600940713     C                   KFLD                    ARBNRS
532700940713     C                   KFLD                    ARBNSP
532800940713     C*
532900940713     C     KTAB1         KLIST
533000940713     C                   KFLD                    CODUT
533100940713     C                   KFLD                    COD
533200940713     C                   KFLD                    KEY
533300940713     C     KTAB2         KLIST
533400940713     C                   KFLD                    CODUT
533500940713     C                   KFLD                    COD
533600940713     C*
533700940907     C     KLBL1         KLIST
533800940713     C                   KFLD                    LBLAAN
533900940713     C                   KFLD                    LBLLPN
534000940713     C                   KFLD                    LBLNRN
534100940713     C                   KFLD                    LBLNSN
534200940907     C     KLBL3         KLIST
534300940907     C                   KFLD                    KAAS
534400940907     C                   KFLD                    KLNP
534500940907     C                   KFLD                    KNRS
534600940907     C                   KFLD                    KNSP
534700940713     C*
534800940719     C     KAR41         KLIST
534900940719     C                   KFLD                    KAAS
535000940719     C                   KFLD                    KLNP
535100940719     C                   KFLD                    KNRS
535200940719     C                   KFLD                    KNSP
535300940802     C                   KFLD                    KTRC
535400951220     C     KAR42         KLIST
535500951220     C                   KFLD                    KAAS
535600951220     C                   KFLD                    KLNP
535700951220     C                   KFLD                    KNRS
535800951220     C                   KFLD                    KNSP
535900021213     C*
536000021213     C     KAR51         KLIST
536100021213     C                   KFLD                    KAAS
536200021213     C                   KFLD                    KLNP
536300021213     C                   KFLD                    KNRS
536400021213     C                   KFLD                    KNSP
536500021213     C                   KFLD                    KTRd              3
536600940719     C*
536700991026     C     KAR61         KLIST
536800940713     C                   KFLD                    KAAS
536900940713     C                   KFLD                    KLNP
537000940713     C                   KFLD                    KNRS
537100940713     C                   KFLD                    KNSP
537200991026     C                   KFLD                    KTRC
537300991026     C*
537400051128     C     Ksped         KLIST
537500991026     C                   KFLD                    KAAS
537600991026     C                   KFLD                    KLNP
537700991026     C                   KFLD                    KNRS
537800991026     C                   KFLD                    KNSP
537900160504     C*
538000990715     C     KGCA1         KLIST
538100941202     C                   KFLD                    KAAS
538200940713     C                   KFLD                    KLNP
538300940713     C                   KFLD                    KNRS
538400940713     C                   KFLD                    KNSP
538500940713     C*
538600990805     C     KEVB1         KLIST
538700940713     C                   KFLD                    KAAS$
538800940713     C                   KFLD                    KLNP
538900940713     C                   KFLD                    KNRS
539000940713     C                   KFLD                    KNSP
539100131114     C     Kcet          KLIST
539200140224     C                   KFLD                    pctfgs
539300131114     C                   KFLD                    pctNDC
539400131114     C                   KFLD                    arbpdc
539500131114     C                   KFLD                    TTRD              3
539600131114     C                   KFLD                    arbAAS
539700131114     C                   KFLD                    arbLNP
539800131114     C                   KFLD                    arbNRS
539900131114     C                   KFLD                    arbNSP
540000950324     C*
540100020906     C     KFVV1         KLIST
540200950324     C                   KFLD                    WNPG
540300950327     C                   KFLD                    WNDC
540400020906     c                   Kfld                    Wfgs
540500131119     C     KDST          KLIST
540600131119     C                   KFLD                    WNPG
540700131119     C                   KFLD                    kndc6
540800131119     c                   Kfld                    Wfgs
540900021203
541000021203     C     Kfapd         klist
541100021203     C                   kfld                    kpdtip
541200021203     C                   kfld                    kpdpdr
541300131108     c
541400131108     c     kbrv          klist
541500131108     C                   KFLD                    WNPG
541600131108     C                   KFLD                    knfv
541700131108     c                   Kfld                    Wfgs
541800131108     c                   Kfld                    artfls
541900131108     c                   Kfld                    artlna
542000131108     c                   Kfld                    artnrs
542100131108     c                   Kfld                    artnsc
542200040218      *
542300160408      * File FITGD32L
542400040218     c     K02TGD02      klist
542500040218     c                   kfld                    TGDtor
542600040218     c                   kfld                    TGDogg
542700030527     C*
542800150617
542900150617     C     KANM          KLIST
543000150617     C                   KFLD                    KAAS
543100150617     C                   KFLD                    KLNP
543200150617     C                   KFLD                    KNRS
543300150617     C                   KFLD                    KNSP
543400150617     C                   KFLD                    KCAA
543401161207      * Fnorm
543402161207     c     korm          klist
543403161207     c                   kfld                    §b4poe
543404161207     c                   kfld                    §b4nsr
543405161207     c                   kfld                    §b4nor
543406161207     c                   kfld                    §b4nrv
543500940713     C*
543600940713     C*-------------------------------------------------------------------------
543700940713     C*
543800991222     C     *LIKE         DEFINE    ARBAAS        WAAS
543900991027     C     *LIKE         DEFINE    ARBLNP        WLNP
544000991027     C     *LIKE         DEFINE    ARBNRS        WNRS
544100991027     C     *LIKE         DEFINE    ARBNSP        WNSP
544200991027     C     *LIKE         DEFINE    FVVNPG        WNPG
544300991027     C     *LIKE         DEFINE    FVVNFV        WNDC
544400131108     C     *LIKE         DEFINE    brvNFV        knfv
544500991027     C     *LIKE         DEFINE    COMC08        SAVC08
544600971219     C     *LIKE         DEFINE    ARBAAS        KAAS
544700990805     C     *LIKE         DEFINE    EVBAAS        KAAS$
544800940713     C     *LIKE         DEFINE    ARBLNP        KLNP
544900940713     C     *LIKE         DEFINE    ARBNRS        KNRS
545000940713     C     *LIKE         DEFINE    ARBNSP        KNSP
545100961112     C     *LIKE         DEFINE    ARBFBR        WFBR
545200940713     C     *LIKE         DEFINE    TBLCOD        COD
545300940713     C     *LIKE         DEFINE    TBLKEY        KEY
545400940713     C     *LIKE         DEFINE    ARBAAS        $AAS
545500940713     C     *LIKE         DEFINE    ARBMGS        $MGS
545600940719     C     *LIKE         DEFINE    AR4TRC        KTRC
545700940907     C     *LIKE         DEFINE    LBLAAN        COMAAN
545800971223     C     *LIKE         DEFINE    AR4TRC        WTRC
545900060130     C*-------------------------------------------------------------------------
546000060130     c
546100060130      *
546200060130     c     *dtaara       define    §azute        azuteds
546300060130     c     *dtaara       define    §datiute      ddatiute
546400060130     c                   in(E)     *dtaara
546500060130    1c                   If        %error  or RSUT = *blanks
546600060130     c                   Clear                   Tibs34ds
546700060130     c                   Call      'TIBS34R'
546800060130     c                   Parm                    Tibs34ds
546900060130     c                   In        *dtaara
547000060130    1c                   EndIf
547100060130     c
547200971223     C*
547300971223     C* IMPOSTAZIONE CAMPI FISSI
547400940907     C                   MOVEL     *ZERO         COMLPN            3
547500940907     C                   MOVEL     *ZERO         COMNRN            2
547600940907     C                   MOVEL     *ZERO         COMNSN            7
547700950307     C                   MOVEL     *ZERO         COMLAN            3
547800950324     C                   Z-ADD     4             WNPG
547900940713      *
548000940713     C                   Z-ADD     *ZERO         NR2               4 0
548100940713     C                   Z-ADD     *ZERO         NR6               4 0
548200940713     C                   Z-ADD     *ZERO         NR8               4 0
548300940802     C                   MOVEL     *BLANKS       WF03              1
548400070403     c
548500070403     c* Verifico se profilo utente ha GEO attiva
548600131205     c* Per EDP sempre anche prr RAC999
548700131205     c                   if        %subst(knmus:1:3)='EDP' or
548800131205     c                             %subst(knmus:1:6)='RAC999'
548900070410     c                   eval      dutgeot='S'
549000070410     c                   else
549100070403     c     dutpou        chain     azorg01l
549200070403     c                   if        %found(azorg01l)
549300070403     c                   movel     orgde8        og148
549400070403     c                   else
549500070403     c                   clear                   og148
549600070403     c                   endif
549700070410     c
549800070524     c                   if        §oggeot<>' ' or §ogtlls<>' '
549900070524     c                   movel     'S'           dutgeot           1
550000070410     c                   endif
550100070524     c                   endif
550200131007
550300140319     c* Tasto funz orari servizi sempre abilitato
550400140319     c                   eval      wabif9='S'
550500131007
550600940713      *
550700940713     C                   ENDSR
550800971223     C**
550900110601      **************************************************************************
551000110601      *   emissione window email destinatario
551100110601      **************************************************************************
551200110601     C     sr_emdest     BEGSR
551300110601     c                   clear                   v4cemw
551400110601     c
551500110601     c                   eval      ilv79TLA='L'
551600110601     c                   eval      ilv79Aas=arbaas
551700110601     c                   eval      ilv79Lnp=arblnp
551800110601     c                   eval      ilv79nrs=arbnrs
551900110601     c                   eval      ilv79nsp=arbnsp
552000110601     c                   call      'FNLV79R'
552100110601     c                   parm                    fnlv79ds
552200110601     c
552300110601     c                   endsr
552400091214      **************************************************************************
552500131114      *   Emissione del Beneficiario C/Ass
552600091214      **************************************************************************
552700091214     C     sr_bencas     BEGSR
552800091215     c                   clear                   vidben
552900091214     c                   clear                   wdaben
553000091214     c                   clear                   wdasrsc
553100091214     c                   clear                   wdascdi
553200091216     c                   clear                   trulintds
553300091216     c
553400091216     c                   eval      iinttip='B'
553500091216     c                   eval      iinttic=vidtic
553600091216     c                   eval      iintrscm=v4crsm
553700091216     c                   eval      iintrsco=vidrmo
553800091216     c                   eval      iintCDI =w_cdi
553900091216     c                   call      'TRULINTR'
554000091216     c                   parm                    trulintds
554100091216     c
554200091216     c                   movel     ointrsci      wdasrsc
554300091216     c
554400091216     c                   select
554500091216     c                   when      ointtipi='O'
554600091216     c                   eval      wdaben='MITTENTE  ORIGINALE'
554700091216     c
554800091216     c                   when      ointtipi='R'
554900091216     c                   eval      wdascdi=w_cdi
555000091216     c                   eval      wdaben=' O R D I N A N T E'
555100091216     c
555200091216     c                   when      ointtipi='M'
555300091216     c                   if        v3cksc>0
555400091216     c                   movel     v3cksc        wdascdi
555500091216     c                   endif
555600091216     c                   eval      wdaben='  M I T T E N T E'
555700091216     c                   ENDSL
555800091214     c
555900131107     c                   exfmt     LRI6wdas
556000091214     c                   endsr
556100130830      **************************************************************************
556200130830      *   window per visualizzare gli orari servizi
556300130830      **************************************************************************
556400130830     C     sr_OrariSr    BEGSR
556500130920
556600140110     c                   clear                   trulorSds
556700140110     c                   eval       s_IOREtfp = ARBtfp
556800140110     c                   eval       s_IOREtfa = ARBtfa
556900140110     c                   eval       s_IOREdti = ARBdti
557000140110     c                   eval       s_IOREhti = ARBhti
557100140110     c                   eval       s_IOREdcr = ARBdcr
557200140110     c                   eval       s_IOREhcr = ARBhcr
557300140110     c                   eval       s_IOREtcr = ARBtcr
557400140110     c                   eval       s_IOREDEI = s_ARBdei
557500140110     c                   eval       s_IOREOEI = s_ARBoei
557600140110     c                   eval       s_IOREfil = ARBlna
557700140110     c                   eval       s_IOREcap = ARBcad
557800140110     c                   eval       s_IOREloc = ARBlod
557900140110     c                   eval       s_IOREnar = ARBnzd
558000150729     c                   clear                   diore01
558100150729     c                   movel     s_arbttc      §ioretrazC
558200150729     c                   movel     s_arbtcc      §ioreconsC
558300150729     c                   eval       s_IOREflo = diore01
558400140113     c* Data di Riferimento: data consegna se consegnata
558500140113     c                   select
558600140113     c                   when      arbdcm>0 and arbica<>' ' and arbica<>'R'
558700140113     c                             and arbicc<>' ' and arbicc<>'R'
558800150617     c                             and w_tbo='A' or (w_tbo='P' and arbdcm>0)
558900140113     c                   eval       s_ioredta=arbdcm
559000140113     c* per ritorno all'incasso data del giorno
559100140113     c                   when      arbdcm>0 and arbddc>0
559200140113     C                   time                    wrkoggiiso
559300140113     C     *iso          move      wrkoggiiso    s_ioredta
559400140113     c                   other
559500140113     c* altrimenti data prevista consegna se > = oggi
559600140113     C                   time                    wrkoggiiso
559700140113     C     *iso          move      wrkoggiiso    s_ioredta
559800140113     c* se < di oggi, imposto oggi
559900140113     c                   if        amgdce>s_ioredta
560000140113     c                   eval       s_ioredta=amgdce
560100140113     c                   endif
560200140113     c                   endsl
560300140113     c
560400140110     c                   eval       s_IOREtser = 'C'
560500140409     c                   eval       s_IOREDDC=arbddc
560600140409     c                   eval       s_IOREnDC=arbndc
560700140409     c
560800150617     c****               IF         *in19 or
560900150617     c                   if          w_tbo='A'
561000150316     c                   if          ARBfbc = *blanks and
561100140110     c                               ARBddc > 0 and
561200140110     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
561300150316     c                               arbica='R' or arbicc=' ' or arbicc='R')
561400140110     c                   eval       s_IOREcons = 'S'
561500140225     c* Orario presunto ritiro/consegna
561600140225     c                   move      arbndc        pctndc
561700140225     c                   move      arbifp        pctfgs
561800140225     c                   move      'COR'         ttrd
561900140225     c     kcet          setgt     fipct02l
562000140225     c     kcet          readpe    fipct02l
562100140310     c                   if        not %eof(fipct02l)
562200140225     c                   movel     pctdati       fipctcords
562300140307     c                   eval      s_ioreorp=§PCTORASTI
562400140225     c                   endif
562500140225     c
562600140110     c                   ENDIF
562700150617     c                   ENDIF
562800140225     c
562900140110     c                   eval       s_IOREtsp = ARBtsp
563000130920
563100140325     c                   eval      s_iorecol=36
563200150611     c                   if        W_video='N'
563300150611     c* località normalizzata
563400150611     C                   MOVEL     ARBAAS        KAAS
563500150611     C                   MOVEL     ARBLNP        KLNP
563600150611     C                   MOVEL     ARBNRS        KNRS
563700150611     C                   MOVEL     ARBNSP        KNSP
563800150611     C                   MOVEL     'X'           KTRC
563900150611     c     kar41         chain     fiar401l
564000150611     c                   if        %found(fiar401l)
564100150611     c                   eval      OOR3LOC_N=ar4not
564200150611     c                   else
564300150611     c                   eval      OOR3LOC_N=arblod
564400150611     c                   endif
564500150611     c
564600150611     c                   call      'TRULORSR'
564700150611     c                   parm                    kpjba
564800150611     c                   parm                    trulorSds
564900150611     c                   parm                    trulor2ds
565000150611     c                   parm                    trulor3ds
565100150611     c                   else
565200140110     c                   call      'TRULORSR'
565300130920     c                   parm                    kpjba
565400140110     c                   parm                    trulorSds
565500150611     c                   endif
565600130830     c
565700131111     c                   ENDSR
565800131111
565900940713      **************************************************************************
566000941014**        12345678901234567890123456789012345678901234567890
566100950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
566200930413**
566300950316LEGATA A PIU' BOLLE                       - F18=Visualizza
566400920326**
566500950316 Non Presente
566600950316 F18=Visualizza
566700940907Vedi in Int.Par
566800940907Vedi in Int.Arr
566900941011Vedi Arch.Stor.
567000940907**
567100940907Spedizione non Presente
567200941230
567300940907Vedi in Int.Bolle Part.
567400940907Vedi in Int.Bolle Arr.
567500941011Vedi in Archiv. Storico
567600940713**  MESSAGGI DI ERRORE VIDEO  **************************************************
567700160505Stampa LDV effettuata ...                                                     01
567800160505Stampa LDV al momento non effettuabile: ritentare o telefonare in Sede        02
567900160505Assegnato non tassato: stampa LdV non effettuabile                            03
568000070426**  cmA4f6
568100111221OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
568200111221USRDTA('Rist.Bolle') SECURE(*YES)
568300070426**  cmA5f6
568400070426OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
568500111221             USRDTA('Rist.Bolle') SECURE(*YES)
568600040218** TF                                                                        *
568700070420F1=Agg.Rich#                                                                    1
568800070411** TF01B                                                                     *
568900070411F1=SCELTA BOLLA#                                                                1
569000021216** TF                                                                        *
569100021218F2=C.A.#                                                                        2
569200021216** TF                                                                        *
569300021216F4=Tass2#                                                                       4
569400021216** TF                                                                        *
569500021216F5=Eventi#                                                                      5
569600070420** TF                                                                        *
569700070420F6=StampaLDV#                                                                   5
569800021216** TF                                                                        *
569900021217F7=Anomalie#                                                                    7
570000021216** TF                                                                        *
570100160628F8=Orm#                                                                         8
570200130830** TF                                                                        *
570300130830F9=OrariSr#                                                                     9
570400021216** TF                                                                        *
570500070420F10=Variaz#                                                                     10
570600021216** TF                                                                        *
570700030121F11=NoteS/B#                                                                    11
570800021217** TF                                                                        *
570900021217F12=Ritorno#                                                                    11
571000021216** TF                                                                        *
571100070420F13=Tassaz#                                                                     13
571200021217** TF                                                                        *
571300021216F14=Giacenza#                                                                   14
571400040218** TF                                                                        *
571500070420F16=Gest.Rich#                                                                  16
571600070404** TF                                                                        *
571700140311F17=NoteAut#                                                                    16
571800021216** TF                                                                        *
571900021216F19=Spunte#                                                                     19
572000021216** TF                                                                        *
572100070420F20=Affid#                                                                      19
572200021216** TF                                                                        *
572300070420F21=DetColli#                                                                   19
572400110928** TF                                                                        *
572500110930F22=PerfectOrder#                                                               19
572600160628** TF                                                                        *
572700160628F23=Discord#                                                                    19
