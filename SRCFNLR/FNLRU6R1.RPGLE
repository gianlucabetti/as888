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
002300171023     FFipnd01L  IF   E           K DISK
002400021213     FFIAR501L  IF   E           K DISK
002500991026     FFIAR601L  IF   E           K DISK
002600991026     FFIAR701L  IF   E           K DISK
002700991027     FFIARI01L  IF   E           K DISK
002800051114     FFiAR901L  IF   E           K DISK
002900180124     FFiARs01L  IF   E           K DISK
003000180124     FFiARt01L  IF   E           K DISK    prefix(ARW:3)
003100940712     F*.......... D E T T A G L I O   C O L L I
003200940713     FFNART01L  IF   E           K DISK
003300940712     F*.......... L E G A M E   B O L L A
003400940713     FFNLBL01L  IF   E           K DISK
003500991027     FFNLBL02L  IF   E           K DISK    RENAME(FNLBL000:FNLBL002)
003600940712     F*.......... B O L L E   P A R T E N Z E
003700150313     FFNBLP01L  IF   E           K DISK    prefix(ARB:3)
003800940712     F*.......... V A R I A Z I O N I   B O L L E
003900131007     FFiARBF2C  IF   E           K DISK
004000990805     FFNEVB01L  IF   E           K DISK
004100950324     F*
004200020906     fFnfvv01l  if   e           k Disk
004300131119     ffidst01l  if   e           k Disk
004400990119     FFNDFA01L  IF   E           K DISK
004500040218      *.......... T E L E F O N A T E   /   R E C L A M I
004600160408     fFITGD32L  if   e           k disk
004700030527     F*.......... N U O V E   T A B E L L E
004800030527     FTNTBE01L  IF   E           K DISK
004900131108     F*.......... S P U N T E
005000131108     Ffnbrv01l  IF   E           K DISK
005100161207     F*.......... O R M
005200161207     Ffnorm01l  IF   E           K DISK
005300131114     F
005400131114     Ffipct02l  IF   E           K DISK
005500150617     Ffnanm01l  IF   E           K DISK
005600060130     d
005700950307     D LBL             S              1    DIM(58) CTDATA PERRCD(58)
005800950307     D LB2             S             58    DIM(1) CTDATA PERRCD(1)
005900941011     D LBD             S             15    DIM(5) CTDATA PERRCD(1)
006000941011     D PRD             S             23    DIM(5) CTDATA PERRCD(1)
006100160505     D MSG             S             78    DIM(3) CTDATA PERRCD(1)
006200030612      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
006300070426     D CMA4f6          S              1    DIM(130) CTDATA PERRCD(65)
006400070426     D CMA5f6          S              1    DIM(130) CTDATA PERRCD(65)
006500030612      *
006600040218     D TF01            s              1    dim(20) ctdata perrcd(20)
006700070411     D TF01B           s              1    dim(20) ctdata perrcd(20)
006800021216     D TF02            S              1    DIM(20) CTDATA PERRCD(20)
006900021216     D TF04            S              1    DIM(20) CTDATA PERRCD(20)
007000021216     D TF05            S              1    DIM(20) CTDATA PERRCD(20)
007100070420     D TF06            S              1    DIM(20) CTDATA PERRCD(20)
007200021216     D TF07            S              1    DIM(20) CTDATA PERRCD(20)
007300021216     D TF08            S              1    DIM(20) CTDATA PERRCD(20)
007400130830     D TF09            S              1    DIM(20) CTDATA PERRCD(20)
007500021216     D TF10            S              1    DIM(20) CTDATA PERRCD(20)
007600021216     D TF11            S              1    DIM(20) CTDATA PERRCD(20)
007700021217     D TF12            S              1    DIM(20) CTDATA PERRCD(20)
007800021216     D TF13            S              1    DIM(20) CTDATA PERRCD(20)
007900021216     D TF14            S              1    DIM(20) CTDATA PERRCD(20)
008000040218     d TF16            s              1    dim(20) ctdata perrcd(20)
008100070404     d TF17            s              1    dim(20) ctdata perrcd(20)
008200021216     D TF19            S              1    DIM(20) CTDATA PERRCD(20)
008300021216     D TF20            S              1    DIM(20) CTDATA PERRCD(20)
008400021216     D TF21            S              1    DIM(20) CTDATA PERRCD(20)
008500110928     D TF22            S              1    DIM(20) CTDATA PERRCD(20)
008600160628     D TF23            S              1    DIM(20) CTDATA PERRCD(20)
008700021217     D $TF             S              1    DIM(20)
008800911024     D*
008900930705     D KCA             S              1    DIM(50)
009000160610     D DCA             S             57    DIM(50)
009100940907     D*
009200940907     D AAN             S              4  0 DIM(30)
009300940907     D LPN             S              3  0 DIM(30)
009400940907     D NRN             S              2  0 DIM(30)
009500940907     D NSN             S              7  0 DIM(30)
009600941024     D CC1             S              1    DIM(99)
009700941024     D CC2             S             20    DIM(99)
009800020329     D CC3             S              1    DIM(99)
009900020329     D CB1             S              1    DIM(99)
010000020329     D CB2             S             20    DIM(99)
010100030121     D RigaTf1         S              1    DIM(62)
010200030121     D RigaTf2         S              1    DIM(62)
010300030121     D RigaTf3         S              1    DIM(62)
010400030527     D*
010500160505     d* PARAMETRI RICEVUTI IN ENTRATA
010600160606     D PARAMU6ds1      DS
010700160523     d** INPUT
010800940714     D  V2CAAS                14     17  0
010900940712     D  COMLNP                18     20  0
011000940714     D  V2CNRS                21     22  0
011100940714     D  V2CNSP                23     29  0
011200160523     d** OUTPUT
011300160504     D  PARF03                30     30
011400160523     d** INPUT
011500160504     D  PARFLG                31     31
011600160505     D*    SE RICH DA PGM GIAC. = 1
011700160504     D  PARGIA               144    144
011800160523     d** OUTPUT
011900160504     d  parrma               149    163                                         x F22=PerfectOrder
012000160504     d  parxco               164    164                                         x F22=PerfectOrder
012100160523     d** INPUT
012200160606     d*   Tipo lancio: "C" -           CHIUSO CON LR
012300160606     d*                "L" - ELABORO E CHIUDO CON LR
012400160606     d*                " " - ELABORO E CHIUDO IN RETRN
012500160523     d  partla               165    165
012600161124     d** OUTPUT - Codice di errore 1=Sped. non trovata
012700161124     d  parerr               166    166
012800161124     d  parflo               167    256
012900160606     d*******************************************
013000160527     D PARAMU6ds       DS
013100160527     D  V2CAAS_               14     17  0
013200160527     D  COMLNP_               18     20  0
013300160527     D  V2CNRS_               21     22  0
013400160527     D  V2CNSP_               23     29  0
013500160527     D  PARF03_               30     30
013600160527     D  PARFLG_               31     31
013700160527     D  PARGIA_              144    144
013800160527     d  parrma_              149    163
013900160527     d  parxco_              164    164
014000160527     d  parflo_              165    256
014100160504     D* PASSAGGIO A INTERROGAZIONE VARIAZIONI           - FNLR75R -
014200161019     D* PASSAGGIO A INTERROGAZIONE DETTAGLIO COLLI      - FNLRU38R -
014300160504     D PARAMint        DS
014400160504     D  pa1AAS                14     17  0
014500160504     D  pa1LNP                18     20  0
014600160504     D  pa1NRS                21     22  0
014700160504     D  pa1NSP                23     29  0
014800160504     D  PA1F03                30     30
014900160504     D  PA1FLG                31     31
015000160504     D  PA1PGM                32     41
015100160505     D  PA1DSA                42     61
015200160505     D  PA1DSP                63     70  0
015300160505     D  PA1RMN                71     85  0
015400160505     D  PA1LNA                86     88  0
015500160505     D  PA1LN1                89    108
015600160505     D  PA1RSD               109    143
015700160504     D* SE RICH DA PGM GIAC. = 1
015800160504     D  PA1GIA               144    144
015900160505     D  PA1FEL               145    147  0
016000160505     D  PA1TBO               148    148
016100160916     D  PA1TFP               156    158  0
016200160916     D  PA1TFA               159    161  0
016300160628     D*VISUALIZZAZIONE ORM - FIOR07R
016400160628     D Parm01          DS
016500160628     D  ppoe                          3  0
016600160628     D  pnor                          7  0
016700160628     D  pnsr                          2  0
016800160628     D  pnrv                          2  0
016900160628     D  psce                          1
017000160628     D  pfgs                          3  0
017100160628     D  ppor                          3  0
017200160628     D  pdtr                          8  0
017300160628     D  pmdb                         10
017400160628     D  pprb                         10
017500160628     D  pdts                          8  0
017600160628     D  prmp                          1
017700160628     D  pbrc                          1
017800160628     D  pref                          2
017900160628     D  pmio                          1
018000940802     D* RICHIAMO PGM DI INTERROGAZIONE SPUNTE FOGLI VARI  - FNLR04R -
018100160504     D PARA04          DS                  INZ
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
019900940726     D* RICHIAMO PGM DI INTERROGAZIONE EVENTI             - FNLRB5R -
020000950215     D PARAB5          DS                  INZ
020100940726     D  PB5AAS                14     17  0
020200940726     D  PB5LNP                18     20  0
020300940726     D  PB5NRS                21     22  0
020400940726     D  PB5NSP                23     29  0
020500940726     D  PB5F03                30     30
020600940726     D  PB5FLG                31     31
020700940726     D  PB5PGM                32     41
020800940726     D  PB5DSA                42     61
020900940726     D  PB5DSP                63     70  0
021000940726     D  PB5RMN                71     85  0
021100940726     D  PB5LNA                86     88  0
021200940802     D  PB5LN1                89    108
021300940802     D  PB5RSD               109    143
021400940802     D  PB5DCP               144    151  0
021500940802     D  PB5TMC               152    152
021600131111     D
021700950428     D                 DS
021800950428     D  V3CLNP                 1      3  0
021900950428     D  V3CNRS                 4      5  0
022000950428     D  V3CNSP                 6     12  0
022100950428     D  V3CDSP                13     20  0
022200950428     D  VIDRMN                21     35  0
022300131121     D  V3CLNA                36     38  0
022400950428     D  D15LNA                39     53
022500950428     D  V32RSD                54     85
022600950428     D  COMC08                 1     85
022700131111     d
022800930705     D WLBDAT          DS
022900940712     D  G02DAT                 1      8  0
023000940712     D  G02INV                 9     16  0
023100940712     D  G02ERR                17     17
023200940712     D  G02TGI                18     22  0
023300950125     D                 DS
023400950125     D* CAMPO UNICO COMPRENDENTE RAG.SOC.+AGGIUNTIVI 35 CARATTERI
023500950125     D  V4CRSD                 1     35
023600131120     D  V4CNOT                36     70
023700131120     D  V4CNOM                 1     70
023800040218      *?definizione oggetto reclamo come spedizione?
023900131121     d ds_OggSped      ds                  inz
024000131121     d  wARBlnp                       3s 0 inz
024100131121     d  wARBnrs                       2s 0 inz
024200131121     d  wARBnsp                       7s 0 inz
024300131121     d  wARBaas                       4s 0 inz
024400040218      *
024500040218     d Status         sds
024600040218     d  SDSprm           *parms
024700991027      *
024800040216     D KPJBA         E DS
024900150402     d fnlru6ds      e ds
024901180308      // - Parametri per ricerca Programmi chiamanti
024902180308     d dsStack         ds
024903180308     d  skStack                      10    Dim(100)
025000040225      *
025100040225     d FIDNA2ds      e ds                  inz
025200040225     d  IA2mod       e                     inz('M')
025300040225     d  IA2tor       e                     inz('S')
025400040216      *
025500991029     d YEURCODS      E DS
025600991027     d TIBS69DS      E DS                  INZ
025700021213     d FNLRB3Ds      E DS
025800991027     d DS_cnaco      E DS                  extname(CNACO00F)
025900991027     d DS_cnind      E DS                  extname(CNIND00F)
026000991027     d DS_cnclp      E DS                  extname(CNCLP00F)
026100991027     d DS_fncls      E DS                  extname(FNCLS00F)
026200160825     d DS_fnarb      E DS                  extname(FNARB00F)
026300991027     D DSCV          E DS
026400020402     D DSCC          E DS
026500991027     D DS$2          E DS
026600991027     D DS3A          E DS
026700951220     D DS7K          E DS
026800160610     D DS7O          E DS
026900131108     D DS2A          E DS
027000020627
027100090604     d ds5e          e ds
027200020627
027300991217     D OG143         E DS
027400070403     D OG148         E DS
027500000615     D DSBL4A        E DS
027600171023     D*DSBL4I        E DS
027700171023     D Dpnddp4       E DS
027800150729     D Diore01       E DS                  INZ
027900150729
028000991027     D FIDN00DS      E DS
028100091216     D TRULINTDS     E DS
028200991027     D DS7PQRS       E DS
028300991027     D FNLG30DS      E DS
028400991027     D FNLr44DS      E DS
028500130920
028600131114     d fipctcetds    e ds
028700140224     d fipctcords    e ds
028800150611     d fnlru3ds      E DS
028900140110     D TRULORSDS     E DS                  prefix(S_)
029000150611     D TRULOR2DS     E DS
029100150611     D TRULOR3DS     E DS
029200131114     D TRULORFDS     E DS
029300130920
029400961121     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
029500991027     D FNLR45DS      E DS
029600950102     D FNLR97        E DS
029700930428     D*
029800930428     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
029900991027     D TNSD99DS      E DS                  INZ
030000941014      *
030100030612     D* DS PER TRUL90R - RICHIESTA STAMPANTI
030200030612     D TRUL90DS      E DS
030300060224      *
030400060224     D* DS PER FNLv55R - richiamo pgm che cerca i terminal per £6
030500060224     D fnlv55ds      E DS
030600941014      *
030700051124     D* DS PER FNLV80R - decodifica cons richiesta
030800051124     D FNLV80DS      E DS
030900131120     D* DS PER FNLRG7R - visulizza altre abilitazioni
031000131120     D fnlrg7ds      E DS
031100140123     D* DS PER FNLRN6R - visulizza note LDV
031200140123     D fnlrn6ds      E DS
031300070427     D* DS PER FIDG34R - reperisce dati consegna automatica
031400070427     D fidg34ds      E DS
031500070420     D* DS PER           stampa LDV
031600070420     D FNLSB5DS      E DS
031700130308     d FNLSB6ds1     e ds                  inz
031800130319     d  DB6pdf       e                     inz('X')
031900941212      *
032000131119     D DDSTFLR       E DS                  INZ
032100010510     D Dsbl4m        E DS
032200030121     d dTba          e ds
032300031119     d dAr5Gen       e ds
032400030121     d dAr5Ban       e ds
032500030703     d dAr5Bnb       e ds
032600130927     d dAr5emd       e ds
032700030121     d Tibs02Ds      e ds
032800060130     d Azuteds       e ds                  extname(Azute00f)
032900060130     d dDatiute      e ds
033000060130     D TIBS34DS      E DS
033100110601     D fnlv79ds      E DS
033200150615     D fnlrj6ds      E DS
033300040809      * ds per il controllo della presenza di CA per la spedizione richiesta
033400040809     d FIDN12DS      e ds
033500040809     d  skKey                 26    305    dim(20)
033600040809     d  skAnn                306    325    dim(20)
033700040809     d  skDca                326    485  0 dim(20)
033800040809     d  skFca                486    545  0 dim(20)
033900040809     d  skDch                546    705  0 dim(20)
034000040809     d  skCch                706    745    dim(20)
034100040809     d*
034200040809     d dsKey           ds
034300040809     d  dsaac                         4  0
034400040809     d  dsfil                         3  0
034500040809     d  dsnca                         7  0
034600040809
034700991027      *
034800991027     D W017A           s             17
034900991027     D w0130           s             13  0
035000991027     D w0133           s             13  3
035100991027     D IMPeditato      s             13
035200991027     D Wsvn            S                   LIKE(AR7svn)
035300991027     D Wvan            S                   LIKE(AR7van)
035400991027     D Wdiv            S                   LIKE(AR6div)
035500991029     D Wariimp         S                   LIKE(ARIimp)
035600991029     D Wariipp         S                   LIKE(ARIipp)
035700020906     d wfgs            s                   Like(Fvvfgs)
035800991027     D DATA_eur        S               D   DATFMT(*eur)
035900021203     d kpdpdr          S                   LIKE(APDpdr)
036000021203     d kpdtip          S                   LIKE(APDtip)
036100161122     d apdrsc_w        S                   LIKE(APDrsc)
036200131120     d kndc6           S                   LIKE(dstnfv)
036300140110     d s_arbdei        S                   LIKE(d98dei)
036400140110     d s_arboei        S                   LIKE(d98oei)
036500150729     d s_arbttc        S                   LIKE(d98ttc)
036600150729     d s_arbtcc        S                   LIKE(d98tcc)
036700180124     d karstrc         S                   LIKE(arstrc)
036800131114     D arbdsp          S              8  0
036900131111     D W0080           S              8  0
037000991027     D W0040           S              4  0
037100991027     D wtibs69r        s              1
037200991027     D wtstdec         s              1
037300131120     D Wvideo          s              1
037400020225     d lnantw          s                   like(§ogntw)
037500020225     d lnpntw          s                   like(§ogntw)
037600030703
037700030703     d wtba            s                   Like(§Ar5Tba)
037800030703     d wdestba         s                   Like(VidTba)
037900020628
038000091216     D W_cdi           S                   LIKE(§ar5cdi)
038100070420     D wrichtrul90     S              1
038200070403     d Dataeur         s               d   datfmt(*eur)
038300070403     d Datadmy         s               d   datfmt(*dmy)
038400110929     d no_f22          s              1    inz(' ')
038500131108     d Wcol_D          s              5  0
038600131108     d Wcol_I          s              5  0
038700131108     d Wcarat          s              3  0
038800131108     d Indx            s              3  0
038900131209     d wabif9          s              1
039000141010     d $Fine           s              1    inz(*off)
039100160610     d win12           s              1    inz(*off)
039200150611     d w_tbo           s              1
039300160610     d w_tbo_sav       s                   like(w_tbo)
039400160913     d blp_arbdcm      s                   like(arbdcm)
039500160913     d blp_arbdcm_sav  s                   like(arbdcm)
039600161014     d blp_arbhmc      s                   like(arbhmc)
039700161014     d blp_arbhmc_sav  s                   like(arbhmc)
039800160614     d w08dcm          s                   like(v08dcm)
039900160614     d w_evbcev        s                   like(evbcev)
040000161108     d wcev            s                   like(evbcev)
040100161107     d w_tcev          s                   like(ILRU3ITCEV)
040200150611     d w_video         s              1
040300150616     d w_ar4_1         s              1
040400150611     d pos             s              3  0
040500150612     d PosA            s              3  0
040600150612     d Lennt1          s              3  0
040700150612     d Lennt1v         s              3  0
040800150617     d kcaa            s                   like(anmcaa) inz(10)
040900160610     d w_ccaleg        s              1
041000161014     d w_orig          s              1
041100161027     d StatoCa         s              1
041200030527     D*
041300030527     D wrkoggiiso      s               d
041301180308      // - Campi per procedura esterna
041302180308     d  pStack         s           1000
041303180308     d  pStartPoint    s              3  0
041304180308     d  pEntries       s              3  0
041400991027      *
041500930705     D* DEFINIZIONI COSTANTI
041600160504     D*
041700930705     D CNOSEQ          C                   CONST('NON SEQUENZIALI')
041800941014     D CINTER          C                   CONST('* INTERROGAZIONE BOL-
041900150409     D                                     LA FILIALE *')
042000021216     D CF2413          C                   CONST('F24=AlTasti(1/3)')
042100021216     D CF2423          C                   CONST('F24=AlTasti(2/3)')
042200021216     D CF2433          C                   CONST('F24=AlTasti(3/3)')
042300021216     D CF2412          C                   CONST('F24=AlTasti(1/2)')
042400021216     D CF2422          C                   CONST('F24=AlTasti(2/2)')
042500150612     D CharOk          C                   const('QWERTYUIOPASDFGHJKLZXCVBNM-
042600150612     D                                     qwertyuiopasdfghjklzxcvbnm-
042700150612     D                                     1234567890.:-_/\()!£$%&+*§-
042800150612     D                                     <>''"èéòàìù')
042900150612     D mi              C                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto inglese
043000150612     D ma              C                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO INGLESE
043100110705     d
043200110705     D* CAMPO PER DSPATR "NO DISPLAY"
043300110705     D NODI            C                   CONST(X'27')
043400131120     D* CAMPO PER DSPATR "RI"
043500131120     D RI              C                   CONST(X'21')
043600131127     D* CAMPO PER DSPATR "HI"
043700131127     D hi              C                   CONST(X'22')
043800161108     D red             C                   CONST(X'28')
043900150618     d c_DspAtrNorm    c                   const(x'20')
043901180308       // - Pgm per ricerca Programmi chiamati
043902180308     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
043903180308     d  pStack                     1000
043904180308     d  pStartPoint                   3  0
043905180308     d  pEntries                      3  0
044000900518     C****************************************************************
044100900518     C*  RIEPILOGO INDICATORI
044200900518     C***************************************************************
044300911105     C* 03    - SPEDIZIONE GIACENTE
044400990119     C* 04    - ABILITO TASTO FUNZIONALE F2 - INT C.A.
044500070411     c* 06    - on per tassazione 2 bolla emette scritta "ImportoBOLLO"
044600070411     c*       - offper tassazione 2 bolla emette scritta "Importo IVA"
044700000216     C* 08    - bolla dpd
044800961111     C* 10    - BOLLA DI RIENTRO
044900160615     C* 12    - ON=La spedizione è presente solo in BLP
045000961112     C* 16    - SI TRATTA DI BOLLA EXPORT
045100070403     C* 17    - p.o. utente e lna bolla hanno GEO attiva
045200160628     C* 18    - Abilita tasto funzionale F8=ORM
045300161017     c* 19    - Condizionamento RI su data/ora consegna
045400991027     C* 22    - ESISTE ESTENSIONE BOLLA 2 e ABILITO IL TASTO  F4=TASSAZIONE 2
045500950825     C* 23    - SFLEND
045600950825     C* 24    - ABILITO IL TASTO "F13=TASSAZIONE"
045700070404     C* 26    - presente FIARP
045800070403     C* 27    - presente blocco consegna ARBFBC<>' '
045900941028     C* 28    - VISUALIZZA MSG ERRORE
046000911129     C* 29    - ESISTE CONTRASSEGNO
046100911023     C* 30/31 - LETTURA COMODO
046200911023     C* 33    - BOLLA IN FRANCO
046300911023     C* 34    - LETTURA SUBFILE
046400911023     C* 35    - PULIZIA SUBFILE
046500911129     C* 36    - LETTURE
046600991027     C* 38    - ESISTE TB2 ANCHE SE NON PRESENTE TIPO RECORD '2' IN FIAR600F
046700160615     C* 41-59 - ERRORI
046800131111     C* 60    - email destiantario
046900131111     C* 61    - PROGRAMMA RICHIAMATO per scelta bolla
047000150616     C* 62    - Spedizione Da Partire
047100040218     C* 63    - Abilita F1  = Aggiunta in reclamo
047200040218     C* 64    - Abilita F16 = Gestione reclamo
047300091214     c* 65    - Abilita visualizzazione beneficiario C/Ass
047400150616     C* 66    - "Da partire" in RI se arrivato almeno un collo
047500960903     C* 67    - ABILITAZIONE TASTO DI COMANDO F7=ANOMALIE
047600131127     C* 68    - pos cursore RMN prima serv x  EMETTERE F23 PER CALCOLARE E VISUALIZZARE DATA CONS.
047700930505     C*         PREVISTA E LE ORE DI RITARDO
047800930526     C* 69    - DATA CONSEGNA MERCE >< 0
047900930510     C* 70    - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
048000160628     C* 71    - ABILITAZIONE TASTO DI COMANDO F23=EVENTI SEGNACOLLI
048100131107     C* 72    - SFLDSPCTL  LRI6S05
048200991027     C* 73    - BOLLA FRANCO FRONTIERA
048300970227     C* 74    - ABILITA EVENTI SEGNACOLLI
048400970227     C* 75    - UTILIZZATO PER NON ESEGUIRE LA SUBROUTINE "LEGBOL"
048500970227     C* 76    - SI DDT IN ALTA INTENSITA'
048600021213     C* 77    - NOTE SUPERMERCATI
048700970227     C* 78    - SFLDSP
048800950825     C* 79    - SFLDSPCTL
048900930510     C* 80    - NEL SFL DEI LEGAMI NON EMETTO IL CAMPO PER LA SELEZIONE
049000941028     C* 81    - USATO NEL VIDEO LEGAMI
049100941028     C* 82    - USATO NEL VIDEO LEGAMI
049200941028     C* 83    - USATO NEL VIDEO LEGAMI
049300160614     C* 84    - Bolla con figlia e tipo legame cca <> ' '
049400971219     C* 85    - VISUALIZZO RIFERIMENTO PARTNER ESTERO
049500111005     c* 86    - Abilitazione F22="PERFECT ORDER"
049600111005     C* 87    - Pgm richiamato in modalità "W" (Perfect Order Wurth)
049700110705     C* 88    -
049800931112     C* 89    - UTILIZZATO PER VISUALIZZARE ALTRI TASTI FUNZIONALI
049900920402     C* 90    - INDICA LA PRESENZA DI UN ERRORE
050000941028     C* 91    - ERRORE QCMDEXC (UTILIZZABILE)
050100940923     C* 92/94 - LEGAME BOLLA
050200930525     C* 95    - ESISTONO EVENTI DA INTERROGARE
050300160615     C* 96    - ON Data arrivo merce = 0
050400160614     C* 97    - Bolla "Da Partire"
050500941014     C* 98    - ESISTONO VARIAZIONI BOLLE EFFETTUATE
050600930504     C* 99    - OFF  ESISTE LEGAME BOLLA SEGUENTE
050700900518     C*****************************************************************
050800000000     C     *ENTRY        PLIST
050900000000     C                   PARM                    KPJBA
051000150402     c                   parm                    FNLRU6DS
051100131210     c
051200980609     C**
051300160606     C                   MOVEL     KPJBU         PARAMu6ds1
051400161124     c                   clear                   parerr
051500161124     c                   clear                   parf03
051600940713     C*
051700160505     c                   if        partla<>'C'
051800160505
051900060130     C                   Z-ADD     1             CODUT             1 0
052000060130     c                   Movel     rsut          V1cdsa
052100941227     C*
052200940713     C*-------------------------------------------------------------------------
052300911024     C                   EXSR      CARTAB
052400150410     c                   clear                   v2cflg
052500110930     c*
052600111005     c                   setoff                                       87
052700150312     c* W=Perfect order WURTH: attivo scritta "PERFECT ORDER" a video e
052800150312     c*   disattivo F22=Perfect Order per evitare errore di chiamata ricorsiva
052900160504     c                   if        parflg='W'
053000111005     c                   eval      *in87=*on
053100150312     c                   eval      no_f22='N'
053200110930     c                   endif
053300150312
053400941014     C* TESTATA
053500941014     C                   MOVEL     CINTER        V1CTES
053600131127     c                   clear                   v1clna
053700940317      *
053800930702     C*
053900940809     C                   MOVEL     'S'           WRIEM             1
054000930702     C**
054100930702     C* PGM RICHIAMATO
054200920326     C**
054300160504     C** PARFLG = " " - RICHIAMO "Normale"
054400160504     c*      (  =  1  - Richiamo "Normale" (SOLO PER I VECCHI PGM)  )
054500160504     C**        =  2  - RICHIAMATO PER   SELEZ BOLLA
054600160504     C**        =  C  - DISABIL.F2=C.A
054700160504     C**        =  N  - DISABIL.F7=ANOMA
054800160505     C**        =  P  - RICHIAMATO DA INT-BOLL PART E DISABILITAZ F7=ANOMALIE
054900160628     C**        =  E  - DISABIL.F23=EV.SEGNACOLLI
055000160504     C**        =  W  - RICHIAMATO per "PerfectOrder"
055100960903     C*
055200040218      *?Abilito F1=Aggiunta in Reclami?
055300040218      * (se richiamato da pgm. FIDNA1R...)
055400040220     c                   eval      *in63  = *off
055500150402     c                   if            SDSprm >  1  and ilru6ra='S'
055600150402     c                             and ELRU6ins = ' '
055700040220     c                   eval      *in63  = *on
055800111003     c                   endif
055900960903     C* ABILITO F7=ANOMALIE SE FLAG DIVERSO DA 'N' E 'P'
056000160504     C     PArFLG        IFNE      'N'
056100160504     C     PArFLG        ANDNE     'P'
056200960903     C                   SETON                                        67
056300040217     C                   ENDIF
056400160628     C* ABILITO F23=EVENTI SEGNACOLLI  SE FLAG DIVERSO DA 'E'
056500160504     C     PArFLG        IFNE      'E'
056600961121     C                   SETON                                        74
056700040217     C                   ENDIF
056800150409
056900160504     C     PArFLG        ifeq      '2'
057000150409     C                   SETON                                        61
057100150409     c                   endif
057200960903     C*
057300911025     C**
057400160825     c                   clear                   ds_fnarb
057500150313     c* chaino la spedizione in partenza e in arrivo
057600150611     c                   clear                   w_tbo
057700160913     c                   clear                   blp_arbdcm
057800161014     c                   clear                   blp_arbhmc
057900150313     c     karb          chain     fnblp01l
058000150611     c                   if        %found(fnblp01l)
058100150611     c                   eval      w_tbo='P'
058200160913     c                   eval      blp_arbdcm=arbdcm
058300161014     c                   eval      blp_arbhmc=arbhmc
058400150618     c                   seton                                        12
058500150611     c                   endif
058600150312     c     karb          chain     fnarb01l
058700150611     c                   if        %found(fnarb01l)
058800150611     c                   eval      w_tbo='A'
058900150618     c                   setoff                                       12
059000150611     c                   endif
059100150611     c                   if        w_tbo='P' or w_tbo='A'
059200150312     C** interrogazione bolla
059300150312     C                   EXSR      bolla
059400150312     C                   EXSR      visual
059500161124     c                   else
059600161124     c                   eval      parerr='1'
059700911023     C*
059800150313     c                   endif
059900150313     c
060000150313     c     fine          tag
060100150410
060200150410     c                   select
060300150410     c* pa1f03 = 1 --> premuto F3
060400150410     c                   when      v2cflg='2'
060500160504     c                   eval      paRf03='1'
060600150410     c* pa1f03 = S --> Selezionata bolla
060700150410     c                   when      v2cflg='3'
060800160504     c                   eval      paRf03='S'
060900150410     c                   endsl
061000160505
061100160505     c                   endif
061200160505     c
061300930504     C*
061400930504     C*
061500941109     C* PER F3=FINE PULISCO CHIAVE BOLLA E IMPOSTO FLAG F3
061600941109     C     *INKC         IFEQ      *ON
061700941109     C                   Z-ADD     *ZERO         V2CAAS
061800941109     C                   Z-ADD     *ZERO         COMLNP
061900941109     C                   Z-ADD     *ZERO         V2CNRS
062000941109     C                   Z-ADD     *ZERO         V2CNSP
062100160504     C                   MOVEL     '1'           PARF03
062200941109     C                   ENDIF
062300980603     C**
062400160505
062500160505     c* Chiusura con LR
062600160523     c                   if        partla<>' '
062700160505
062800160505     C* CALL A VUOTO AI PGM  PER CHIUDERE I FILES
062900160505     C                   MOVEL     'C'           D98TLA
063000160505     C                   CALL      'TNSD99R'
063100160505     C                   PARM                    TNSD99DS
063200160505     C*
063300160505     C                   MOVEL     'C'           PA1FLG
063400160505     C                   MOVEL     PARAMint      KPJBU
063500160505     C                   CALL      'FNLR75R'                            99
063600160505     C                   PARM                    KPJBA
063700160505     C*
063800160505     C                   CLEAR                   PARAB5
063900160505     C                   MOVEL     'C'           PB5FLG
064000160505     C                   MOVEL     PARAB5        KPJBU
064100160505     C                   CALL      'FNLRB5R'
064200160505     C                   PARM                    KPJBA
064300160505     C*
064400160505      *
064500160505     C                   IF        Wtibs69r  <>  *BLANKS
064600160505     C                   eval      I69TLA  = 'C'
064700160505     C                   call      'TIBS69R'
064800160505     C                   parm                    TIBS69DS
064900160505     c                   ENDIF
065000941212     C*
065100040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
065200040810     c                   clear                   fidn12ds
065300040810     c                   eval      i12tla = 'C'
065400040810     c                   call      'FIDN12R'
065500040810     c                   parm                    fidn12ds
065600051128     c                   clear                   fnlv80ds
065700051128     c                   eval      ilv80tla = 'C'
065800051128     c                   call      'FNLV80R'
065900051128     c                   parm                    kpjba
066000051128     c                   parm                    FNLV80DS
066100070427     c                   eval      id34tla = 'C'
066200070427     c                   call      'FIDG34R'
066300070427     c                   parm                    fidg34ds
066400131120     c                   eval      ilrg7tla = 'C'
066500131120     c                   call      'FNLRG7R'
066600131120     c                   parm                    fnlrg7ds
066700091216     c                   eval      iinttla = 'C'
066800091216     c                   call      'TRULINTR'
066900091216     c                   parm                    trulintds
067000150615     c                   eval      ilrj6tla = 'C'
067100150615     c                   call      'FNLRJ6R'
067200150615     c                   parm                    fnlrj6ds
067300141105
067400040810     c*
067500000000     C                   SETON                                        LR
067600160505     c                   else
067700160505     C                   SETON                                        RT
067800160505     c                   endif
067900160505
068000160606     C                   MOVEL     PARAMU6ds1    KPJBU
068100941011     C   99
068200941011     CANN99              READ      FNBLP01L                               99
068300940802     C**************************************************************************
068400940802     C*    CARICO TABELLE
068500940802     C**************************************************************************
068600911024     C     CARTAB        BEGSR
068700930705     C*
068800930705     C* CARICO TABELLA 7O
068900930705     C                   MOVEL     '7O'          COD
069000941212     C                   Z-ADD     1             C                 3 0
069100940713     C     KTAB2         SETLL     TABEL
069200940713     C     KTAB2         READE     TABEL                                  30
069300930705     C*
069400930705     C     *IN30         DOWEQ     *OFF
069500930705     C     TBLFLG        IFEQ      ' '
069600930705     C                   MOVEL     TBLKEY        KCA(C)
069700930705     C                   MOVEL     TBLUNI        DCA(C)
069800930705     C                   ADD       1             C
069900930705     C                   END
070000940713     C     KTAB2         READE     TABEL                                  30
070100930705     C                   END
070200941024     C*
070300941024     C* CARICO IN SCHIERA LA TABELLA CC
070400941024     C                   Z-ADD     *ZERO         WA
070500941024     C                   CLEAR                   CC1
070600941024     C                   CLEAR                   CC2
070700020329     C                   CLEAR                   CC3
070800941024     C                   MOVEL     'CC'          COD
070900941024     C     KTAB2         SETLL     TABEL00F
071000941024     C     KTAB2         READE     TABEL00F                               99
071100941024     C     *IN99         DOWEQ     *OFF
071200941024     C     TBLKEY        ANDNE     *BLANKS
071300941024     C     TBLKEY        ANDNE     *ZERO
071400941024     C                   ADD       1             WA
071500941024     C                   MOVE      TBLKEY        CC1(WA)
071600941024     C                   MOVEL     TBLUNI        CC2(WA)
071700020402     C                   MOVEL(P)  TBLUNI        DSCC
071800020402     C                   MOVE      §CCDFE        CC3(WA)
071900941024     C     KTAB2         READE     TABEL00F                               99
072000941024     C                   ENDDO
072100060130     C*
072200020329      *
072300020329      * CARICO IN SCHIERA LA TABELLA CB
072400020329     C                   Z-ADD     *ZERO         WA
072500020329     C                   CLEAR                   CB1
072600020329     C                   CLEAR                   CB2
072700020329     C                   MOVEL     'CB'          COD
072800020329     C     KTAB2         SETLL     TABEL00F
072900020329     C     KTAB2         READE     TABEL00F                               99
073000020329     C     *IN99         DOWEQ     *OFF
073100020329     C     TBLKEY        ANDNE     *BLANKS
073200020329     C     TBLKEY        ANDNE     *ZERO
073300020329     C                   ADD       1             WA
073400020329     C                   MOVE      TBLKEY        CB1(WA)
073500020329     C                   MOVEL     TBLUNI        CB2(WA)
073600020329     C     KTAB2         READE     TABEL00F                               99
073700020329     C                   ENDDO
073800991027     C*
073900991027     C* CARICO LA TABELLA $2
074000991027     C                   MOVEL     '$2'          COD
074100991027     C                   MOVEL(P)  1             KEY
074200991027     C     KTAB1         CHAIN     TABEL00F
074300991027     C                   IF        %FOUND
074400991027     C                   MOVEL     TBLUNI        DS$2
074500991027     C                   ELSE
074600991027     C                   CLEAR                   DS$2
074700991027     C                   ENDIF
074800940802     C*
074900911024     C                   ENDSR
075000940802     C**************************************************************************
075100940802     C* EMISSIONE BOLLA
075200940802     C**************************************************************************
075300161014     C     BOLLA         begsr
075400940802     C*
075500160628     c                   setoff                                       18
075600070411     C                   SETOFF                                       220616
075700070403     C                   SETOFF                                       170377
075800070404     C                   SETOFF                                       262708
075900131127     C                   SETOFF                                       2933
076000940408     C                   SETOFF                                       687069
076100070524     C                   SETOFF                                       850488
076200150617     C                   SETOFF                                       866266
076300160614     C                   SETOFF                                       9784
076400161017     C                   SETOFF                                       19
076500991027      *
076600940719     C                   Z-ADD     0             V3CLNA
076700940719     C                   Z-ADD     0             V3CDSP
076800930517     C                   Z-ADD     0             VIDRMN
076900940712     C                   MOVE      *BLANKS       VIDRMA
077000991217     C                   CLEAR                   V3CRPP
077100131120     C                   CLEAR                   atrpda
077200160913     C                   CLEAR                   vidpda
077300131127     C                   CLEAR                   atrtsp
077400160513     C                   CLEAR                   v7ddse
077500160513     C                   CLEAR                   v7ddsed
077600161014     C                   CLEAR                   v7dcons
077700161014     C                   CLEAR                   vidhmc
077800150615     c* ancora da partire
077900150616     c                   if        w_tbo='P' and arbft1=*blanks
078000150616     c                   eval      v1dfbc='Da Partire'
078100150616     c                   seton                                        62
078200150616     c* se "manca record bolla arrivo" metto la scritta in RI
078300150617     c                   if        arbdam>0
078400150617     c                   seton                                        66
078500150617     c                   else
078600150617     C                   Z-ADD     ARBAAS        KAAS
078700150617     C                   Z-ADD     ARBLNP        KLNP
078800150617     C                   Z-ADD     ARBNRS        KNRS
078900150617     C                   Z-ADD     ARBNSP        KNSP
079000150617     c     kanm          setll     fnanm01l                               66
079100150617     c                   endif
079200160513     c* Visualizzo in questo caso la data spunta entrata
079300160513     c                   seton                                        97
079400160513     c                   if        arbdse>0
079500160513     c     *iso          movel     arbdse        dataeur
079600160517     c                   movel     dataeur       w0080
079700160517     c                   eval      v7ddse=%editW(w0080:'  /  /    ')
079800160516     c* astesrisco le seguenti specifiche: per visualizzare se arrivo totale/parziale
079900160516     c* bisognerebbe elaborare sia le spunte del teminal di partenza che quelle della lnp
080000160516     c* insieme.... mentre al momento è possibile farlo con una linea alla volta...
080100160516     c**                 clear                   fnlrj6ds
080200160516     c**                 eval      ILRJ6TRIC='P'
080300160516     c**                 eval      ILRJ6AAS =arbaas
080400160516     c**                 eval      ILRJ6LNP =arblnp
080500160516     c**                 eval      ILRJ6NRS =arbnrs
080600160516     c**                 eval      ILRJ6NSP =arbnsp
080700160516     c**                 eval      ILRJ6lin =arblnp
080800160516     c**                 call      'FNLRJ6R'
080900160516     c**                 parm                    fnlrj6ds
081000160516     c**                 if        olrj6err=*blanks and olrj6fall='S'
081100160516     c**                 eval      V7DDSED='Totale'
081200160516     c**                 else
081300160516     c**                 eval      V7DDSED='Parziale'
081400160516     c**                 endif
081500160517     c                   else
081600160517     c                   eval      v7ddse='NO'
081700160513     c                   endif
081800150615     c                   endif
081900961112     C*
082000911022     C* CONTROLLO SE SPEDIZIONE GIACENTE
082100920219     C* GIACENZA APERTA
082200941202     C                   Z-ADD     ARBAAS        KAAS
082300940713     C                   Z-ADD     ARBLNP        KLNP
082400940713     C                   Z-ADD     ARBNRS        KNRS
082500940713     C                   Z-ADD     ARBNSP        KNSP
082600050405     C     KGCA1         CHAIN     tigcp21l                           31
082700040216if  2C  N31GCPATB        IFEQ      ' '
082800920410     C     GCPDCG        IFNE      0
082900920219     C                   SETON                                        03
083000920219     C                   ELSE
083100070403     C                   SETON                                        0327
083200150616     C                   EVAL      V1DFBC=' GIACENTE '
083300040217     C                   ENDIF
083400040217e   2C                   ENDIF
083500930428     C*
083600021213     C*
083700021213     C* 77 ON - Esistono note supermenrcati d ainterrogare
083800021213     C                   Z-ADD     ARBAAS        KAAS
083900021213     C                   Z-ADD     ARBLNP        KLNP
084000021213     C                   Z-ADD     ARBNRS        KNRS
084100021213     C                   Z-ADD     ARBNSP        KNSP
084200021213     C                   MOVEL     'SCR'         KTRD
084300021213     C     KAR51         CHAIN     FIAR5000                           31
084400021217     C  n31              seton                                        77
084500021217     C   31              setoff                                       77
084600920219     C*
084700920207     C* SE BOLLA CON LA "G" SENZA GIACENZA MECCANIZZATA SEGNALO
084800931116     C     ARBFBC        IFEQ      'G'
084900070412     C                   EVAL      V1DFBC=' GIACENTE '
085000070403     C                   SETON                                        27
085100040217     C                   ENDIF
085200930415     C*
085300940907     C                   MOVEL     ARBAAS        V3CAAS
085400940719     C                   MOVE      ARBNSP        V3CNSP
085500940719     C                   MOVEL     ARBNRS        V3CNRS
085600070404     C                   MOVEL     ARBNRS        V4CNRS
085700131108     C********           MOVEL     ARBNRS        V7CNRS
085800940719     C                   MOVEL     ARBLNP        V3CLNP
085900950508     C                   MOVEL     ARBLNP        V3CLPA
086000131114     c
086100131114     c                   z-add     arbmgs        arbdsp
086200131114     c                   movel     arbaas        arbdsp
086300940908     C*
086400940908     C*  L E G A M I    B O L L A
086500940908     C  N75              EXSR      LEGBOL
086600940908     C*
086700911022     C* NON SEQUENZIALI
086800911023     C     ARBFNS        IFEQ      'S'
086900930705     C                   MOVEL     CNOSEQ        DESFNS
087000911022     C                   ELSE
087100911022     C                   MOVEL     *BLANKS       DESFNS
087200040217     C                   ENDIF
087300930525     C*
087400930525     C* 95 ON - ESITONO EVENTI DA INTERROGARE
087500940713     C                   Z-ADD     ARBAAS        KAAS$
087600940713     C                   Z-ADD     ARBLNP        KLNP
087700940713     C                   Z-ADD     ARBNRS        KNRS
087800940713     C                   Z-ADD     ARBNSP        KNSP
087900990805     C     KEVB1         SETLL     FNEVB01L                               95
088000920402     C*
088100920403     C* 98 ON - ESITONO VARIAZIONI FATTE
088200131007     C     KARB2         SETLL     FiARBF2C                               98
088300990119     C* 04 ON - ESISTONO CA DA VISUALIZZARE
088400990119     C                   CLEAR                   CONTCA
088500161027     C                   CLEAR                   StatoCa
088600040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
088700040809      * legate alla spedizione che passo
088800040809     c                   clear                   fidn12ds
088900040809     c                   eval      i12aas = arbAAS
089000040809     c                   eval      i12lnp = arbLNP
089100040809     c                   eval      i12nrs = arbNRS
089200040809     c                   eval      i12nsp = arbNSP
089300040809     c                   eval      i12fel = 'E'
089400040809     c                   eval      i12fan = 'E'
089500040809     c*
089600040809     c                   call      'FIDN12R'
089700040809     c                   parm                    fidn12ds
089800040809     c*
089900040809     c                   setoff                                       31
090000040809     c*
090100040809     c* se non ci sono errori
090200040809     c                   if        o12err <> ' '
090300040809     c                   seton                                        31
090400040809     c                   else
090500040809     c* se numero di CA trovate maggiore di zero
090600040809     c                   if        o12nca = 0
090700040809     c                   seton                                        31
090800040809     c                   else
090900040809     c                   z-add     1             ii                2 0
091000040809     c                   dow       ii <= o12nca and *in31 = *off
091100040809     c                   movea     skkey(ii)     dskey
091200040809     C                   SETON                                        04
091300040809     C                   ADD       1             CONTCA            3 0
091400161027     c                   if        skdch(ii)>0 and StatoCa<>'A'
091500161027     c                   eval      StatoCa='C'
091600161027     c                   else
091700161027     c                   eval      StatoCa='A'
091800161027     c                   endif
091900040809     C                   Z-ADD     dsaac         VHCAAC
092000040809     C                   Z-ADD     dsfil         VHCFIL
092100040809     C                   Z-ADD     dsnca         VHCNCA
092200040809     c*
092300040809     c                   add       1             ii
092400040809     c                   enddo
092500040809     c*
092600040809     c                   seton                                        31
092700040809     c*
092800040809     c                   endif
092900040809     c                   endif
093000040809     c**
093100070406     c* se Presente FIARP, imposto la cons.richista proposta
093200070404     c                   clear                   v4cdimpr
093300070404     c                   clear                   v4coimpr
093400070404     c                   clear                   v4cprupr
093500070416     c                   clear                   v08dcrpr
093600070404     c                   clear                   v4ctcrpr
093700070404     c                   clear                   v4chcrpr
093800070406     c
093900070406     c* Solo se visualizzabile
094000070406     c     karb2         chain     fiarp01l
094100070406    1c                   if        %found(FIarp01l)
094200070404     c                   seton                                        26
094300070404     C                   MOVEL     arppru        v4cprupr
094400070404     C                   MOVEL     arpoim        v4coimpr
094500070404    2c                   if        arpdim>0
094600070404     c     *iso          movel     arpdim        dataeur
094700070416     c                   movel     dataeur       datadmy
094800070416     c                   movel     datadmy       w0060             6 0
094900070416     c                   movel     w0060         v4cdimpr
095000070404    2c                   endif
095100070404    2c                   if        arpdcr>0
095200070404     c     *iso          movel     arpdcr        dataeur
095300070416     c                   movel     dataeur       v08dcrpr
095400070416     c**                 movel     datadmy       v06dcrpr
095500070404    2c                   endif
095600070404     c                   movel     arphcr        v4chcrpr
095700070404     c                   movel     arptcr        v4ctcrpr
095800070406     c                   if        arptcr=' '
095900070406     c                   eval      v4ctcrpr='IL'
096000070404    1c                   endif
096100070406    1c                   endif
096200070406     c
096300911022     C* TIPO BOLLA CONTROLLO SE FRANCO
096400940719     C                   MOVEL     ARBCBO        V3CCBO
096500911022     C                   MOVEL     '3A'          COD
096600911022     C                   MOVEL     *BLANKS       KEY
096700940712     C                   MOVEL     ARBCBO        KEY
096800940713     C     KTAB1         CHAIN     TABEL                              31
096900911022     C                   MOVEL     TBLUNI        DS3A
097000911022     C                   MOVEL     §3ATB1        FLGTB1            1
097100940719     C                   MOVEL     §3ADES        V3DCBO
097200940719     C                   MOVEL     §3ADES        V5DCBO
097300911030     C* FRANCO
097400040216if  1C     FLGTB1        IFEQ      'F'
097500911022     C                   SETON                                        33
097600940713     C                   MOVE      ARBKSC        V3CKSC
097700940712     C                   MOVE      *BLANKS       VIDKSC
097800040216x   1C                   ELSE
097900911030     C* ASSEGNATO
098000911030     C     ARBCCM        IFNE      0
098100940713     C                   MOVE      ARBCCM        V3CKSC
098200911030     C                   ELSE
098300940713     C                   MOVE      *ZEROS        V3CKSC
098400040217     C                   ENDIF
098500911030     C                   MOVEL     ARBKSC        VIDKSC
098600040217e   1C                   ENDIF
098700070410     c* Numero di telefono del mittente
098800070410     c                   clear                   v3ctelm
098900070410     c                   move      v3cksc        w0040
099000070410     c                   if        w0040<>0000 and w0040<>8888
099100070410     c                             and w0040<>9999
099200070410     c                   eval      i69kin = V3Cksc
099300070410     c                   exsr      CTR_ANAGRA
099400070410     c                   eval      V3ctelm= indtel
099500070410     c                   endif
099600991027     C* 38 ON - ESISTE TB2 ANCHE SENZA TIPO RCD "2" IN FIAR600F
099700920121     C     §3ATB2        COMP      '  '                               3838
099800040218      *
099900040218      *?Controllo se esitono reclami?
100000131111     c                   eval      wARBlnp = ARBlnp
100100131111     c                   eval      wARBnrs = ARBnrs
100200131111     c                   eval      wARBnsp = ARBnsp
100300131111     c                   eval      wARBaas = ARBaas
100400040218     c                   eval      TGDtor  = 'S'
100500040218     c                   eval      TGDogg  = ds_OggSped
100600040218     c     K02TGD02      setll     FITGD000
100700160408     c                   eval      *in64   = (%equal(FITGD32L)   and
100800040303     c                                           (SDSprm =  1
100900040303     c                                        or (SDSprm >  1
101000150402     c                                        and ELRU6INS <> 'N')))
101100940712     C*
101200940712     C* DECODIFICO  "BOLLA DI RIENTRO"
101300961111     C                   MOVEL     *BLANKS       VIDFBR
101400961111     C                   MOVEL     *BLANKS       VIDFFR
101500961111     C                   SETOFF                                       1073
101600940712     C     ARBFBR        IFEQ      'S'
101700961111     C                   MOVEL     'SI'          VIDFBR
101800961111     C                   SETON                                        10
101900040217     C                   ENDIF
102000961111     C     ARBFBR        IFEQ      'F'
102100961111     C                   MOVEL     'FR'          VIDFFR
102200961111     C                   SETON                                        73
102300040217     C                   ENDIF
102400970130     C*
102500970227     C* FLAG CONSEGNA DDT
102600970130     C                   MOVEL     ARBAAS        KAAS
102700970130     C                   MOVEL     ARBLNP        KLNP
102800970130     C                   MOVEL     ARBNRS        KNRS
102900970130     C                   MOVEL     ARBNSP        KNSP
103000970130     C                   MOVEL     'A'           KTRC
103100970131     C                   CLEAR                   V1DBAM
103200000615     C                   eval      v3csop = ' '
103300170324     c* se la bolla è da partire o chiusa in partenza prendo il flag da blp
103400170324   1ac                   if        *in62 or arbft2='N'
103500170117     C     arbFST        IFEQ      'S'
103600170117     C     arbFST        OREQ      'C'
103700170117     C                   MOVEL     'SI DDT'      V1DBAM
103800170117     C                   SETON                                        76
103900170117     C                   ELSE
104000170117     C                   MOVEL     'NO DDT'      V1DBAM
104100170117     C                   SETOFF                                       76
104200170117     C                   ENDIF
104300170117     C                   MOVEL     arbsop        v3csop
104400170117  x1ac                   else
104500170117     c
104600060216     C     KAR41         CHAIN     FiAR401L                           31
104700970227     C* C'E' IL RECORD
104800040216if  1C     *IN31         IFEQ      *OFF
104900000615     C                   movel     ar4not        dsbl4a
105000000615     C                   eval      v3csop = §b4aso
105100031027     c                   Move      §b4Abm        wbam              1
105200040216if  2C     WBAM          IFEQ      'S'
105300970131     C     WBAM          OREQ      'C'
105400031027     c     wbam          oreq      'P'
105500031027     c     wbam          oreq      'Q'
105600970227     C                   SETON                                        76
105700970227     C                   MOVEL     'SI DDT'      V1DBAM
105800040216x   2C                   ELSE
105900970227     C                   SETOFF                                       76
106000970227     C                   MOVEL     'NO DDT'      V1DBAM
106100040216e   2C                   ENDIF
106200970227     C**
106300150615sel 2C*                  SELECT
106400150615     C*    WBAM          WHENEQ    'C'
106500150615     C*    WBAM          OREQ      'K'
106600150615     C*                  MOVEL     'CLI'         V1DFST
106700150615     C*    WBAM          WHENEQ    'J'
106800150615     C*    WBAM          oreq      'P'
106900150615     C*    WBAM          oreq      'Q'
107000150615     C*    WBAM          OREQ      'W'
107100150615     C*                  MOVEL     'Arr'         V1DFST
107200150615     C*    WBAM          WHENEQ    'S'
107300150615     C*    WBAM          OREQ      'Y'
107400150615     C*                  MOVEL     'Par'         V1DFST
107500150615x   2C*                  OTHER
107600150615     C*                  MOVEL     'NO '         V1DFST
107700150615e   2C*                  ENDSL
107800970227     C**
107900970227     C* NON  C'E' IL RECORD
108000150615x   1C                   ELSE
108100160909     c* se la bolla è da partire prendo il flag da blp
108200170117     c*                  if        *in62
108300170117     C*    arbFST        IFEQ      'S'
108400170117     C*    arbFST        OREQ      'C'
108500170117     C*                  MOVEL     'SI DDT'      V1DBAM
108600170117     C*                  SETON                                        76
108700170117     C*                  ELSE
108800170117     C*                  MOVEL     'NO DDT'      V1DBAM
108900170117     C*                  SETOFF                                       76
109000170117     C*                  ENDIF
109100170117     C*                  MOVEL     arbsop        v3csop
109200170117     c*                  else
109300161019     c* Attualmente questa situazione non dovrebbe più capitare: se la bolla è in arrivo
109400161019     c* il fiar4 trk 'A' deve sempre esserci
109500150615     C                   SETON                                        76
109600150615     C                   MOVEL     'SI DDT'      V1DBAM
109700150615     C*                  MOVEL     'PAR'         V1DFST
109800160909     c                   endif
109900150615e   1C                   ENDIF
110000010510     c* numero O.R.M.
110100161207     c                   clear                   v3cpoe
110200161207     c                   clear                   v3cnsr
110300161207     c                   clear                   v3cnor
110400161207     c                   clear                   v3cnrv
110500010510     C                   MOVEL     'M'           KTRC
110600060216     c     kar41         chain     fiar401l                           31
110700010510     c     *in31         ifeq      *off
110800010510     c                   movel     ar4not        dsbl4m
110900010510     c                   z-add     §b4poe        v3cpoe
111000010510     c                   z-add     §b4nsr        v3cnsr
111100010510     c                   z-add     §b4nor        v3cnor
111200010510     c                   z-add     §b4nrv        v3cnrv
111300161207     c     korm          Setll     Fnorm01l
111400161207     c                   If        %Equal(fnorm01l)
111500160628     c                   seton                                        18
111501180308         XRTVSTKR (pStack:pStartPoint:pEntries);
111502180308         dsStack = pStack;
111503180308         IF  %lookup('FIOR07R   ':skStack) > 0;
111504180308         *In18=*off ;
111505180308         endif      ;
111600161207     c                   endif
111700010510     c                   endif
111800130320     c*//* Verifico se ha fiar8
111900130320     C*//                MOVEL     ARBAAS        KAAS
112000130320     C*//                MOVEL     ARBLNP        KLNP
112100130320     C*//                MOVEL     ARBNRS        KNRS
112200130320     C*//                MOVEL     ARBNSP        KNSP
112300130320     c*//  ksped         setll     fiar801l
112400130320     c*//                if        %equal(fiar801l)
112500130320     c*//                eval      v1dAR8='PACKING LIST'
112600130320     C*//                ELSE
112700130320     C*//                CLEAR                   V1DAR8
112800130320     c*//                endif
112900941219     C*
113000941219     C* DECODIFICO  "CONSEGNA A MAGAZZINO"
113100941219     C     ARBFDN        IFEQ      'S'
113200941219     C                   MOVEL     'SI'          V3CFDN
113300941219     C                   ELSE
113400941219     C                   MOVEL     *BLANKS       V3CFDN
113500941219     C                   ENDIF
113600911129     C*
113700920204     C* INCASSO C/A E ASSEGNATO
113800930113     C                   MOVEL     '  '          VIDICC
113900930113     C                   MOVEL     '  '          VIDICA
114000150615     c                   clear                   v7dicc
114100930113     C*
114200930113     C* SE ASSEGNATO O FRANCO CON SECONDA BOLLA
114300930113     C     *IN33         IFEQ      '0'
114400930113     C     *IN33         OREQ      '1'
114500930113     C     *IN38         ANDEQ     '1'
114600930113     C*
114700150617     c     w_tbo         ifeq      'A'
114800160825     C     ARBICA        andne     ' '
114900941216     C     ARBICA        ANDNE     'R'
115000160825     c     w_tbo         oreq      'P'
115100160825     c     arbdcm        andgt     0
115200920204     C                   MOVEL     'SI'          VIDICA
115300040217     C                   ENDIF
115400040217     C                   ENDIF
115500911129     C*
115600911030     C* LINEA PARTENZA DECODIFICA
115700020225     c                   clear                   og143
115800150313     C                   Z-ADD     ARBLNP        NUM3              3 0
115900930706     C     NUM3          CHAIN     AZORG01L                           31
116000940719     C  N31              MOVEL     ORGDES        V3DLNP
116100940719     C   31              MOVEL     *BLANKS       V3DLNP
116200020225     c                   eval      og143 = orgde3
116300020225     c                   eval      lnpntw = §ogntw
116400911030     C* LINEA ARRIVO   DECODIFICA
116500991217     c                   clear                   og143
116600160517     C                   clear                   orgde5
116700160913     c                   clear                   v7dlnaa
116800160913     c                   clear                   v7dalna
116900070403     C     arblna        CHAIN     AZORG01L                           31
117000160913     C                   movel     orgde3        og143
117100160913     C                   movel     orgde8        og148
117200940719     C  N31              MOVEL     ORGDES        V3DLNA
117300940719     C   31              MOVEL     *BLANKS       V3DLNA
117400070403     c                   movel     v3dlna        v7dlna
117500160913     c                   if        §ogntw<>'EEX' and
117600160913     c                             §ogntw<>'DPD' and §ogntw<>'FED'
117700160913     c                   movel     'a'           v7dalna
117800160517     c                   if        orgde5<>*blANKS
117900160517     c                   movel     orgde5        v7dlnaa
118000160517     c                   else
118100160517     c                   movel     v3dlna        v7dlnaa
118200160517     c                   endif
118300160913     c                   endif
118400020304     c                   eval      lnantw = §ogntw
118500070426     c* p.o. gestore di arblna
118600070426     c                   clear                   fnlv55ds
118700070426     c                   movel     arblna        d55lin
118800070426     c                   movel     '6'           d55tpt
118900070426     C                   time                    wrkoggiiso
119000070426     C     *iso          move      wrkoggiiso    d55drf
119100070426     c                   call      'FNLV55R'
119200070426     c                   parm                    fnlv55ds
119300070426     c
119400070426     c                   if        d55tfa<>arblna
119500070426     C     d55tfa        CHAIN     AZORG01L                           31
119600070426     C  n31              movel     orgde8        og148
119700070426     C   31              clear                   og148
119800070426     c                   endif
119900070426     c
120000070403     c
120100070403     c* Abilito visualizzazione nuove funzioni se GEO attiva
120200070426     c* Per p.o. gestione linea di arrivo e p.o. utente
120300070403     c                   if        §oggeot='S' and dutgeot='S'
120400070524     c                   seton                                        1788
120500070406     c                   else
120600070406     c* non visualizzo nemmeno fiarp
120700070406     c                   setoff                                       26
120800070403     c                   endif
120900070524     c
121000070524     c* Vedo se almeno è attivo lo scambio dati con tellus
121100070524     c                   if        not *in88 and §ogtlls<>' ' and dutgeot='S'
121200070524     c                   seton                                        88
121300070524     c                   endif
121400070404     c
121500070404     c* Emetto riga sintetica altre abilitazioni
121600070521     c                   clear                   v5dngd
121700140109     c***                if        *in17
121800070404     c                   clear                   v3cges
121900070404     C                   MOVEL     *BLANKS       V3cgesd
122000150615     C                   clear                   v7drig1
122100070404     c*
122200131120     c                   clear                   fnlrg7ds
122300131120     c                   eval      ilrg7aas=arbaas
122400131120     c                   eval      ilrg7lnp=arblnp
122500131120     c                   eval      ilrg7nrs=arbnrs
122600131120     c                   eval      ilrg7nsp=arbnsp
122700131120     c                   eval      ilrg7lung=58
122800131120     c                   call      'FNLRG7R'
122900131120     c                   parm                    fnlrg7ds
123000131120     c                   movel     olrg7vssin    v3cges
123100150615     c* richiamo per visualizzazione in videata Globale
123200150615     c                   clear                   fnlrg7ds
123300150615     c                   eval      ilrg7aas=arbaas
123400150615     c                   eval      ilrg7lnp=arblnp
123500150615     c                   eval      ilrg7nrs=arbnrs
123600150615     c                   eval      ilrg7nsp=arbnsp
123700150615     c                   eval      ilrg7lung=10
123800150615     c                   eval      ilrg7nb1='S'
123900150615     c                   eval      ilrg7nb2='A'
124000150615     c                   eval      ilrg7nb3='C'
124100150615     c                   eval      ilrg7nb4='F'
124200150615     c                   eval      ilrg7nb5='M'
124300150615     c                   eval      ilrg7nb6='B'
124400150615     c                   eval      ilrg7nb7='9'
124500150615     c                   eval      ilrg7nb8='L'
124600150615     c                   call      'FNLRG7R'
124700150615     c                   parm                    fnlrg7ds
124800150615     c                   eval      v7drig1=*all'-'
124900150615     c                   if        olrg7abi<>*blanks
125000161123     c***                eval      %subst(v7drig1:66:12)=' ' +
125100161123     c***                                          %subst(olrg7vssin:1:10) + '?'
125200161123     c                   eval      %subst(v7drig1:66:12)=red +
125300161123     c                                             %subst(olrg7vssin:1:10) + '?'
125400150616     c                   setoff                                       55
125500150616     c                   else
125600150616     c                   seton                                        55
125700150615     c                   endif
125800140109     c***                endif
125900070410     c
126000070524     c* Reperisco dati della distinta automatica tramite FIDG34R
126100070524     c                   if        *in88
126200070410     c                   clear                   v5ccgi
126300070411     c                   clear                   v5dcgi
126400070410     c                   clear                   v5dtgi
126500070410     c                   clear                   v5dttl
126600070411     c                   clear                   v5clno
126700070427     c                   clear                   v5dlno
126800070427     c                   clear                   v5cargerr
126900070427     c                   clear                   v5dargerr
127000070427     c
127100070427     c                   clear                   fidg34ds
127200070427     c                   eval      id34tip='2'
127300070427     c                   eval      id34aas=arbaas
127400070427     c                   eval      id34lnp=arblnp
127500070427     c                   eval      id34nrs=arbnrs
127600070427     c                   eval      id34nsp=arbnsp
127700070427     c                   call      'FIDG34R'
127800070427     c                   parm                    FIDG34DS
127900070410     c
128000070427     c                   movel     od34cgi       v5ccgi
128100070410    3c                   select
128200070427     c                   when      od34tgi='A'
128300070410     c                   eval      v5dtgi='Automatico'
128400070427     c                   when      od34tgi='M'
128500070410     c                   eval      v5dtgi='Manuale   '
128600070410    3c                   ENDSL
128700070411     c* Decodifica giro
128800070427     c                   eval      v5dcgi=od34cgides
128900070427     c* esito/errore
129000070427     c                   movel     od34err       v5cargerr
129100070427     c                   eval      v5dargerr=od34errdes
129200070427     c* livello normalizzazione
129300070427     c                   movel     od34lno       v5clno
129400070427     c                   eval      v5dlno=od34lnodes
129500070427     c* elaborazione tellus
129600070427     c***                eval      v5dttl=od34ttldes
129700070427     c                   select
129800070427     c                   when      od34ttl='1'
129900070427     c                   eval      v5dttl='SI- CON Giro'
130000070427     c                   when      od34ttl='2'
130100070427     c                   eval      v5dttl='SI-SenzaGiro'
130200070427     c                   endsl
130300070521     c*
130400070521     c* Decodifico se assegnazione automatica o manuale
130500070521     c                   if        arbndc>0 and arbngd<>0
130600070521     c                   eval      v5dngd='MANUALE'
130700070521     c                   endif
130800070521     c                   if        arbndc>0 and arbngd=0
130900070521     c                   eval      v5dngd='AUTOMAT'
131000070521     c                   endif
131100070410     c
131200070410    1c                   endif
131300991217     c
131400060518     c* LNA DPD o lnp dpd: leggo record "I" di ar4
131500000216     c                   setoff                                       08
131600171023     c*                  clear                   v3czon
131700171023     c*                  clear                   v3crtn
131800171023     c                   clear                   v3cida7
131900171023if  1c                   if        lnantw = 'DPD'  or lnpntw='DPD'
132000991217     c                   seton                                        85
132100991217     C                   MOVEL     ARBAAS        KAAS
132200991217     C                   MOVEL     ARBLNP        KLNP
132300991217     C                   MOVEL     ARBNRS        KNRS
132400991217     C                   MOVEL     ARBNSP        KNSP
132500171023     C*                  MOVEL     'I'           KTRC
132600171023     C*    KAR41         CHAIN     FiAR401L                           31
132700171023     C* N31              MOVEL     AR4NOT        dsbl4i
132800171023     c*  31              clear                   dsbl4i
132900171023     c     ksped         chain     fipnd01l
133000171023     c                   if        %found(fipnd01l)
133100171023     C*                  MOVEL(P)  §b4ipn        V3CRPP
133200171023     c*                  if        §b4ida<>*blanks or §b4iza>*zeros
133300171023     c*                  movel     §b4ida        v3crtn
133400171023     c*                  movel     §b4iza        v3czon
133500171023     c                   eval      v3crpp=pndipn
133600171023     c                   if        pnddpe='DP5'
133700171023     c                   eval      v3cida7=pndida7
133800171023     c                   seton                                        08
133900171023     c                   else
134000171023     c                   if        pnddpe='DP4'
134100171023     c                   eval      dpnddp4=pndetiu
134200171023     c                   eval      v3cida7=§b4ida
134300171023     c                   seton                                        08
134400171023     c                   endif
134500171023     c                   endif
134600171023     c
134700171023     c*                  seton                                        08
134800000216     c                   endif
134900171023     c***                else
135000171023if  1c***                if        lnpntw = 'DPD'
135100171023     C***                MOVEL     ARBAAS        KAAS
135200171023     C***                MOVEL     ARBLNP        KLNP
135300171023     C***                MOVEL     ARBNRS        KNRS
135400171023     C***                MOVEL     ARBNSP        KNSP
135500171023     C*                  MOVEL     'I'           KTRC
135600171023     c*                  clear                   dsbl4i
135700171023     C*    KAR41         CHAIN     FiAR401L
135800171023     c*                  if        %found(fiar401l)
135900171023     C*                  MOVEL     AR4NOT        dsbl4i
136000171023     C*                  MOVEL     §b4ipn        V3CRPP
136100171023     c**                 seton                                        85
136200171023     c**                 endif
136300040216e   1c                   endif
136400971219     C*
136500971219     C* 16 ON  - SI TRATTA DI BOLLA EXPORT
136600020225     c                   if        lnantw = 'EEX' or lnantw = 'EUP' or
136700060201     c                             lnantw = 'FED' or lnantw='DPD'
136800971219     C                   SETON                                        16
136900971219    1C                   ENDIF
137000940719     C                   MOVEL     V3DLNA        D15LNA
137100920804     C*
137200940719     C                   MOVEL     ARBLNA        V3CLNA
137300940719     C                   MOVEL     ARBRSM        V3CRSM
137400070410     C                   MOVEL     ARBRSM        V4CRSM
137500940713     C                   MOVEL     ARBRSM        V28RSM
137600940719     C                   MOVEL     ARBINM        V3CINM
137700940719     C                   MOVEL     ARBPRM        V3CPRM
137800940719     C                   MOVEL     ARBCAM        V3CCAM
137900940719     C                   MOVEL     ARBLOM        V3CLOM
138000160912     C                   MOVEL     ARBLOM        V4CLOM
138100940713     C                   MOVEL     ARBLOM        V25LOM
138200940719     C                   MOVEL     ARBFAP        V3CFAP
138300950421     C                   MOVEL     ARBNZM        V3CNZM
138400131108     c                   if        arbnzm<>*blaNKS
138500131108     C                   MOVEL     arbnzm        v7cprm
138600131108     c                   else
138700131108     C                   MOVEL(P)  arbprm        v7cprm
138800131108     c                   endif
138900131108     c
139000050113     C                   MOVEL     ARBXCO        VIDXCO
139100130221     c                   if        vidxco >*zeros  and no_f22=*blanks
139200110928     c                   seton                                        86
139300110928     c                   endif
139400940719     C                   MOVEL     *BLANKS       V3CPIM
139500940719     C                   MOVEL     *BLANKS       V4CPID
139600061108     c                   clear                   v3cdfm
139700061108     c                   clear                   v4cdfd
139800950411     C* PARTITA IVA MITTENTE E DESTINATARIO: LI VALORIZZO PRENDENDOLI
139900060216     C* DA FiAR4 SE RECORD ESISTENTE
140000950411     C                   MOVEL     ARBAAS        KAAS
140100950411     C                   MOVEL     ARBLNP        KLNP
140200950411     C                   MOVEL     ARBNRS        KNRS
140300950411     C                   MOVEL     ARBNSP        KNSP
140400950411     C                   MOVEL     'P'           KTRC
140500060216     C     KAR41         CHAIN     FiAR401L                           31
140600040216if  1C     *IN31         IFEQ      *OFF
140700950411     C                   MOVEL     AR4NOT        V3CPIM
140800950411     C                   MOVE      AR4NOT        V4CPID
140900040216x   1C                   ELSE
141000950411     C* ALTRIMENTI VALORIZZO IN BASE AL CONTENUTO DI ARBCPI
141100040216if  2C     ARBCPI        IFNE      *BLANKS
141200950411     C  N33              MOVEL     ARBCPI        V4CPID
141300950411     C   33§3ATB2        IFEQ      '  '
141400950411     C                   MOVEL     ARBCPI        V3CPIM
141500950411     C                   ELSE
141600950411     C                   MOVEL     ARBCPI        V4CPID
141700040217     C                   ENDIF
141800040217e   2C                   ENDIF
141900040217e   1C                   ENDIF
142000061108      * valorizzo il codice fiscale del mittente e del destinatario
142100061108      * FIAR4 rcd Q
142200061108     c                   eval      ktrc = 'Q'
142300061108     c     kar41         chain     fiar401l
142400061108if  1c                   if        %found(fiar401l)
142500061108     c                   movel     ar4not        v3cdfm
142600061108     c                   move      ar4not        v4cdfd
142700061108x   1c                   endif
142800980704     C* VISUALIZZO SE C'E' IL RIFERIM PARTNER
142900971219     C                   CLEAR                   V3CEST
143000971219     C                   MOVEL     ARBAAS        KAAS
143100971219     C                   MOVEL     ARBLNP        KLNP
143200971219     C                   MOVEL     ARBNRS        KNRS
143300971219     C                   MOVEL     ARBNSP        KNSP
143400971219     C                   MOVEL     'E'           KTRC
143500060216     C     KAR41         CHAIN     FiAR401L                           31
143600980704     C* VISUALIZZO IN UNA BOLLA ITALIA SE C'E'
143700980704     C* VISUALIZZO SEMPRE IN UNA BOLLA IMPORT/EXPORT
143800971219     C  N31              MOVEL     AR4NOT        V3CRPP
143900980704     C  N31              SETON                                        85
144000980704     C**
144100020225     c                   if        *in31 and
144200020225     c                             (lnpntw = 'EEX' or lnpntw = 'EUP' or
144300020225     c                              lnpntw = 'FED') or
144400020225     c                             (lnantw = 'EEX' or lnantw = 'EUP' or
144500020225     c                              lnantw = 'FED')
144600980704     C                   SETON                                        85
144700980704     C                   ENDIF
144800980704     C**
144900940719     C                   MOVEL     ARBRSD        V4CRSD
145000131108     C**********         MOVEL     ARBRSD        V30RSD
145100940713     C                   MOVEL     ARBRSD        V32RSD
145200940719     C                   MOVEL     ARBIND        V4CIND
145300131119     C                   MOVEL     ARBIND        V33IND
145400940719     C                   MOVEL     ARBPRD        V4CPRD
145500940719     C                   MOVEL     ARBCAD        V4CCAD
145600940719     C                   MOVEL     ARBLOD        V4CLOD
145700131119     C*****              MOVEL     ARBLOD        V24LOD
145800940719     C                   MOVEL     ARBFIN        V4CFIN
145900940719     C                   MOVEL     ARBNZD        V4CNZD
146000160505
146100160505     c* Giorni di chiusura: Visualizzo la relativa dicitura solo se presenti
146200160505     c                   clear                   v7dggc
146300160505     c                   if        arbgc1<>*blanks or arbgc2<>*blanks
146400160505     c                   eval      v7dggc='GG.Chiusura'
146500160505     c                   endif
146600160505
146700911022     C                   MOVEL     ARBGC1        VIDGC1
146800911022     C                   MOVEL     ARBGC2        VIDGC2
146900911022     C                   MOVEL     ARBCTR        VIDCTR
147000940719     C                   MOVEL     ARBCTS        V6CCTS
147100131108     c                   if        arbnzd<>*blaNKS
147200131108     C                   MOVEL     arbnzd        v7cprd
147300131108     c                   else
147400131108     C                   MOVEL(P)  arbprd        v7cprd
147500131108     c                   endif
147600940719     C* DECODIFICA CODICE TASSAZIONE
147700940719     C                   MOVEL     'CT'          COD
147800940719     C                   MOVEL     *BLANKS       KEY
147900940719     C                   MOVEL     ARBCTS        KEY
148000940719     C     KTAB1         CHAIN     TABEL00F                           31
148100940719     C                   MOVEL     TBLUNI        V6DCTS
148200911022     C* DECODIFICA FISSA TARIFFA PREFERENZIALE
148300911022     C                   MOVEL     'TR'          COD
148400911022     C                   MOVEL     *BLANKS       KEY
148500911022     C                   MOVEL     VIDCTR        UNOCTR            1
148600911022     C                   MOVEL     UNOCTR        KEY
148700940713     C     KTAB1         CHAIN     TABEL                              31
148800911022     C                   MOVEL     TBLUNI        DEFCTR
148900070404     C*****              MOVEL     ARBFTM        V3CFTM
149000150615     c*                  if        arbftm='S'
149100150615     c*                  eval      v3dftm='TASSATA CON TARIFFA MITTENTE'
149200150615     c*                  endif
149300940711     C* DECODIFICA TIPO SERVIZIO BOLLE
149400090604     c                   clear                   ds5e
149500940711     C                   MOVEL     '5E'          COD
149600940711     C                   MOVEL     *BLANKS       KEY
149700940713     C                   MOVEL     ARBTSP        KEY
149800941024     C     KTAB1         CHAIN     TABEL00F                           31
149900090604     C  N31              MOVEL     TBLUNI        ds5e
150000090604     C  N31              MOVEL     §5ed08        V1DTSP
150100940719     C   31              MOVEL     *BLANKS       V1DTSP
150200131127     c                   if        §5eftc<>' '
150300131127     c                   movel     ri            atrtsp
150400160517     c**                 else
150500160517     c**                 movel     hi            atrtsp
150600131127     c                   endif
150700940713     C                   MOVEL     ARBVMD        VIDVMD
150800940713     C                   MOVEL     ARBVAD        VIDVAD
150900940713     C                   MOVEL     ARBIAS        VIDIAS
151000940713     C                   MOVEL     ARBVAS        VIDVAS
151100911022     C                   MOVEL     ARBNAS        VIDNAS
151200950214     C                   CLEAR                   VIDFLS
151300950214     C     ARBNCD        IFNE      *ZERO
151400950214     C     ARBFNS        ORNE      *BLANKS
151500940923     C                   MOVEL     ARBFLS        VIDFLS
151600950214     C                   ENDIF
151700950214     C                   Z-ADD     ARBFLS        V3CFLS
151800911022     C                   MOVEL     ARBNCL        VIDNCL
151900070410     C                   MOVEL     ARBPKB        VIDPKb
152000070410     C                   MOVEL     ARBPKF        VIDPKF
152100070410     C                   MOVEL     ARBVLb        VIDVLb
152200070410     C                   MOVEL     ARBFVb        VIDFVb
152300070410     C                   MOVEL     ARBVLF        VIDVLF
152400070410     C                   MOVEL     ARBFVF        VIDFVF
152500930705     C     ARBNCR        IFLT      99998
152600940711     C                   MOVEL     ARBVLC        VIDVLC
152700940711     C                   MOVEL     ARBNCR        VIDNCR
152800921231     C                   ELSE
152900990909     C                   Z-ADD     0             VIDVLC
153000990909     C                   Z-ADD     0             VIDNCR
153100921231     C                   END
153200990909     C     ARBNCP        IFLT      99998
153300990909     C                   MOVEL     ARBPKC        VIDPKC
153400990909     C                   MOVEL     ARBNCP        VIDNCP
153500990909     C                   ELSE
153600990909     C                   Z-ADD     0             VIDPKC
153700990909     C                   Z-ADD     0             VIDNCP
153800990909     C                   END
153900940711     C                   MOVEL     ARBQFT        VIDQFT
154000180124     c
154100180124     c* incompatibile
154200180124     c                   clear                   atrinc
154300180124     c                   clear                   v7dinc
154400180124     c     karb2         setll     fiart01l
154500180124     c     karb2         reade     fiart01l
154600180124     c
154700180124    3c                   dow       not %eof(fiart01l)
154800180124     c* Verifico se esiste record J nel fiars per bolla incompatibile
154900180124     c                   eval      karstrc='J'
155000180124     c     kars          chain     fiars01l
155100180124     c                   if        %found(fiars01l)
155200180124     c                   eval      v7dinc='INC'
155300180124     c                   movel     ri            atrinc
155400180124     c                   leave
155500180124     c                   endif
155600180124     c     karb2         reade     fiart01l
155700180124    3c                   enddo
155800180124
155900031119
156000031119      * Referente e Telefono del destinatario
156100031119     c                   Clear                   V4cRef
156200031119     c                   Clear                   V4cTeld
156300091214     c                   clear                   w_cdi
156400150622     c                   clear                   v03dac
156500150622     c                   clear                   v03orc
156600150622     c                   clear                   v03pru
156700150622     c                   clear                   v03nmtd
156800031119      * li prendo da Fiar500f record 'GEN'
156900130320     c                   clear                   v1dAR8l
157000130320     c                   clear                   v1dAR8
157100031119     c                   Clear                   dAr5Gen
157200031119     c                   Z-Add     ArbAas        Kaas
157300031119     c                   Z-Add     ArbLNp        Klnp
157400031119     c                   Z-Add     ArbNrs        Knrs
157500031119     c                   Z-Add     ArbNsp        Knsp
157600031119     c                   Eval      Ktrd = 'GEN'
157700031119     c     kAr51         Chain     Fiar501l
157800031119If  1c                   If        %Found(Fiar501l)
157900031119     c                   Movel     Ar5Uni        Dar5Gen
158000031119     c                   Eval      V4cRef = §Ar5Ref
158100031119     c                   Eval      V4cTeld = §Ar5Teld
158200091214     c* memorizzo eventuale codice beneficiario assegno
158300091214     c                   eval      w_cdi=§ar5cdi
158400150622     c* Immissione bolla:
158500150622     c* Visualizzo dat/ora creazione del fiar5 record GEN come "immissione"
158600150622     c* perchè in immissione bolle viene creato fiar5 rek "GEN" per memorizzare
158700150622     c* utente/terminale di immissione. Non uso BLPDIM anche perchè non ho l'ora ma solo
158800150622     c* la data su blp
158900150622     c     *iso          movel     ar5dac        datadmy
159000150622     c                   movel     datadmy       v03dac
159100150622     c                   movel     ar5orc        v03orc
159200150622     c* data/ora
159300150622     c* profilo utente e terminale
159400150622     c                   Eval      V03pru = §Ar5bpru
159500150622     c                   Eval      V03nmtd = §Ar5bnmtd
159600130320     c* verifico se LDV con packing-list e/o PDF
159700130320sel 2c                   select
159800130320w   2c                   when      §AR5als = 'S'
159900130320     c                   eval      v1dAR8l = 'PACKING LIST PDF'
160000130320     c                   eval      v1dAR8  = 'PACKING PDF '
160100130320w   2c                   when      §AR5alx = 'S'
160200130320     c                   eval      v1dAR8l = 'PACKING LIST'
160300130320     c                   eval      v1dAR8  = 'PACKING LIST'
160400130320e   2c                   endsl
160500031119    1c                   EndIf
160600040301
160700040301      * Se colli di valore visualizzo "V" nell'apposito campo
160800040301      *  con lo specifico attributo di visualizzazione
160900040301if  1c                   if        §AR5bva = *blanks
161000040301     c                   eval      VIDbva  = *blanks
161100150612     c                   eval      V7Dbva  = *blanks
161200040301x   1c                   else
161300040301     c                   eval      VIDbva  = 'V'
161400150612     c                   eval      V7Dbva  = 'VALORE'
161500040301e   1c                   endif
161600110601
161700110610     c* email destinatario
161800110601     c                   setoff                                       60
161900110601     c                   clear                   v4cemw
162000110610     c                   clear                   v4cemd
162100130927     c                   Clear                   dAr5emd
162200110601      * li prendo da Fiar500f record 'EMD'
162300110601     c                   Z-Add     ArbAas        Kaas
162400110601     c                   Z-Add     ArbLNp        Klnp
162500110601     c                   Z-Add     ArbNrs        Knrs
162600110601     c                   Z-Add     ArbNsp        Knsp
162700110601     c                   Eval      Ktrd = 'EMD'
162800130927     c***  kAr51         setll     Fiar501l
162900130927     c     kAr51         chain     Fiar501l
163000130927If  1c                   If        %found(Fiar501l)
163100130927     c                   eval      dar5emd=ar5uni
163200130927     c                   if        §ar5tel<>*blanks or §ar5eml<>*blanks
163300110610     c                   Eval      V4cemd = 'SI'
163400110601     c                   seton                                        60
163500130927     c                   endif
163600110601    1c                   EndIf
163700040301
163800030121      * Bancali
163900030121     c                   Clear                   VidTba
164000030121     c                   Clear                   VidTb2
164100030122     c                   Clear                   VidNba
164200030122     c                   Clear                   VidNb2
164300030703      * Record 'BAN'
164400030703If  0c                   If        %subst(ArbGva:1:1) = 'B'
164500030121     c                   Clear                   dAr5Ban
164600030121     c                   Eval      Ktrd = 'BAN'
164700030121     c     kAr51         Chain     Fiar501l
164800030121If  1c                   If        %Found(Fiar501l)
164900030121     c                   Movel     Ar5Uni        Dar5Ban
165000030122      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
165100030122      * per le note relative al mancato reso dei bancali
165200030122     c                   If        Not *In77 and
165300030122     c                             (§Ar5Rb1 <> *Zeros or §Ar5Rb1 <> *Zeros)
165400030122     c                   Eval      *In77 = *On
165500030122     c                   EndIf
165600030121     c                   Eval      VidNba = §Ar5Nba
165700030121     c                   Eval      VidNb2 = §Ar5Nb2
165800030121If  2c                   If        §Ar5Tba <> *Zeros
165900030703     c                   Eval      wtba = §Ar5Tba
166000030703     c                   ExSr      Sr_DecTba
166100030703     c                   Eval      VidTba = wdestba
166200030121    2c                   EndIf
166300030121If  2c                   If        §Ar5Tb2 <> *Zeros
166400030703     c                   Eval      wtba = §Ar5Tb2
166500030703     c                   ExSr      Sr_DecTba
166600030703     c                   Eval      VidTb2 = wdestba
166700030121    2c                   EndIf
166800030121    1c                   EndIf
166900030703    0c                   EndIf
167000030703      * Record 'BNB'  -  Spedizione OVER 25 colli
167100030703If  0c                   If        %subst(ArbGva:1:1) = 'O'
167200030703     c                   Clear                   dAr5Bnb
167300030703     c                   Eval      Ktrd = 'BNB'
167400030703     c     kAr51         Chain     Fiar501l
167500030703If  1c                   If        %Found(Fiar501l)
167600030703     c                   Movel     Ar5Uni        Dar5Bnb
167700030703      * Se non è stato attivato il tasto F11 controllo se lo devo attivare ora
167800030703      * per i colli originali in caso di spedizione over 25 colli
167900030703     c  n77              Eval      *In77 = *On
168000030703     c                   Eval      VidNba = §Ar5bNba
168100030703     c                   Eval      VidNb2 = §Ar5bNb2
168200030703If  2c                   If        §Ar5bTba <> *Zeros
168300030703     c                   Eval      wtba = §Ar5bTba
168400030703     c                   ExSr      Sr_DecTba
168500030703     c                   Eval      VidTba = wdestba
168600030703    2c                   EndIf
168700030703If  2c                   If        §Ar5bTb2 <> *Zeros
168800030703     c                   Eval      wtba = §Ar5bTb2
168900030703     c                   ExSr      Sr_DecTba
169000030703     c                   Eval      VidTb2 = wdestba
169100030703    2c                   EndIf
169200030703    1c                   EndIf
169300030703    0c                   EndIf
169400911129     C*
169500131120     C**   ARBACC        IFEQ      'S'
169600131120     C**                 MOVEL     'SI'          V5CACC
169700131120     C**                 ELSE
169800131120     C**                 MOVEL     'NO'          V5CACC
169900131120     C**                 END
170000160825     c     W_tbo         ifeq      'A'
170100160825     C     ARBICC        andne     ' '
170200941216     C     ARBICC        ANDNE     'R'
170300160825     c     w_tbo         oreq      'P'
170400160825     c     arbdcm        andgt     0
170500160825     c
170600930218     C                   MOVEL     'SI'          VIDICC
170700150615     C                   eval      v7dicc='Incassato'
170800930218     C                   END
170900911022     C*
171000911022     C                   MOVEL     ARBRMN        VIDRMN
171100940712     C                   MOVEL     ARBRMA        VIDRMA
171200911022     C                   MOVEL     ARBRMO        VIDRMO
171300911022     C                   MOVEL     ARBNCD        VIDNCD
171400911022     C                   MOVEL     ARBNCA        VIDNCA
171500911022     C                   MOVEL     ARBZNC        VIDZNC
171600161122     c* campo segnacolli dal/al per la videaa di riepilogo
171700161122     c                   clear                   vidncda
171800161122     c* non sequenziali
171900161122     c                   if        arbfns='S'
172000161122     c                   eval      vidncda='Non Sequenziali'
172100161122     c                   else
172200161122     c*     sequenziali
172300161122     c                   if        vidncd>0
172400161122     c                   eval      %subst(vidncda:1:7)=%editc(vidncd:'Z')
172500161122     c                   eval      %subst(vidncda:8:1)='/'
172600161122     c                   if        vidnca>0
172700161122     c                   eval      %subst(vidncda:9:7)=%editc(vidnca:'Z')
172800161122     c                   endif
172900161122     c                   endif
173000161122     c                   endif
173100051104     c                   select
173200051104     c                   when      arbffd='S'
173300051104     C                   MOVEL     'SI '         VIDFFD
173400161104     c                   eval      v1dffd='F.DEPOSITO'
173500150615     c***                when      arbffd='M'
173600150615     C***                MOVEL     'SAB'         VIDFFD
173700051104     C                   other
173800051104     C                   MOVEL     '   '         VIDFFD
173900161104     c                   clear                   v1dffd
174000051104     C                   ENDsl
174100051124     c
174200940719     C                   MOVEL     ARBNFV        V3CNFV
174300051124     c* richiamp pgm per decodifica
174400051124     C                   MOVEL     *BLANKS       VIDTCR
174500051124     c                   clear                   vidhcr
174600051124     c                   clear                   viddcr
174700051124     c                   if        arbdcr>0 or arbhcr>0
174800051124     c                   clear                   fnlv80ds
174900051124     c                   eval      ilv80aas=arbaas
175000051124     c                   eval      ilv80lnp=arblnp
175100051124     c                   eval      ilv80nrs=arbnrs
175200051124     c                   eval      ilv80nsp=arbnsp
175300051124     c                   eval      ilv80mgs=arbmgs
175400051124     c                   eval      ilv80tc1=arbtc1
175500051124     c                   eval      ilv80tc2=arbtc2
175600051124     c                   eval      ilv80dcr=arbdcr
175700051124     c                   eval      ilv80hcr=arbhcr
175800051124     c                   eval      ilv80tcr=arbtcr
175900051124     c                   call      'FNLV80R'
176000051124     c                   parm                    kpjba
176100051124     c                   parm                    fnlv80ds
176200051124     c
176300051124     c                   eval      viddcr=olv80dcr8
176400051124     c                   eval      vidtcr=olv80tcrd
176500911023     C                   MOVEL     ARBHCR        VIDHCR
176600051124     c                   endif
176700051124     c
176800070521     c* Se presente ora consegna, la edito
176900161014     c**                 clear                   vidhmc
177000161014     c**                 if        arbhmc>0
177100161014     c**                 eval      vidhmc=%editw(arbhmc: '0 :  ')
177200161014     c**                 endif
177300911023     C                   MOVEL     ARBNTC        VIDNTC
177400911023     C                   MOVEL     ARBTMC        VIDTMC
177500940923     C*
177600940923     C* DECODIFICO IL TIPO MANCATA CONSEGNA
177700940923     C                   MOVEL     *BLANKS       V5DTMC
177800940923     C     ARBTMC        IFNE      *BLANKS
177900940923     C                   MOVEL     '2A'          COD
178000940923     C                   MOVEL     *BLANKS       KEY
178100940923     C                   MOVEL     ARBTMC        KEY
178200940923     C     KTAB1         CHAIN     TABEL00F                           31
178300940923     C  N31              MOVEL     TBLUNI        V5DTMC
178400940923     C                   ENDIF
178500940923     C*
178600911023     C                   MOVEL     ARBMIF        VIDMIF
178700911023     C                   MOVEL     ARBNMI        VIDNMI
178800940923     C*
178900940713     C                   MOVEL     ARBGGA        V4CGGA
179000940923     C                   MOVEL     *BLANKS       V4IGGA
179100940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
179200940923     C     V4CGGA        COMP      *BLANKS                            4242
179300940923     C*
179400940713     C                   MOVEL     ARBGMA        V4CGMA
179500940923     C                   MOVEL     *BLANKS       V4IGMA
179600940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
179700940923     C     V4CGMA        COMP      *BLANKS                            4343
179800940923     C*
179900960208     C                   MOVEL     ARBGVA        V4CGVA
180000960208     C                   MOVE      ' '           V4CGVA
180100940923     C                   MOVEL     *BLANKS       V4IGVA
180200940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
180300940923     C     V4CGVA        COMP      *BLANKS                            4444
180400940923     C*
180500070403     C* FLAG blocco CONSEGNA
180600070403     C                   SELECT
180700070403     c                   WHEN      arbfbc='A'
180800070403     c                   eval      v1dfbc='LascAVVISO'
180900070403     C                   SETON                                        27
181000070403     c                   WHEN      arbfbc='B'
181100070403     c  n17              eval      v1dfbc=' BLOCCATA '
181200070412     c   17              eval      v1dfbc=' RIMPALLO '
181300070412     C                   SETON                                        27
181400070403     c                   WHEN      arbfbc='*'
181500070403     c  n17              eval      v1dfbc=' BLOCCATA '
181600070403     c   17              eval      v1dfbc=' GIACENTE '
181700070403     C                   SETON                                        27
181800070403     C                   ENDSL
181900070403     c
182000070403     C**   ARBFBC        COMP      'A'                                    27
182100070403     C**   ARBFBC        COMP      'B'                                    17
182200070403     C**N17ARBFBC        COMP      '*'                                    17
182300940711     C* TIPO CONSEGNA PARTICOLARE 1
182400940804     C                   MOVEL     *BLANKS       V5DTC1
182500150615     C                   MOVEL     *BLANKS       V7DTC1
182600940711     C     ARBTC1        IFNE      ' '
182700911115     C                   MOVEL     '1P'          COD
182800911115     C                   MOVEL     *BLANKS       KEY
182900940711     C                   MOVEL     ARBTC1        KEY
183000940713     C     KTAB1         CHAIN     TABEL                              31
183100940804     C  N31              MOVEL     TBLUNI        V5DTC1
183200940804     C   31              MOVEL     *BLANKS       V5DTC1
183300150615     C  N31              MOVEL     TBLUNI        V7DTC1
183400150615     C   31              MOVEL     *BLANKS       V7DTC1
183500930113     C                   END
183600940711     C* TIPO CONSEGNA PARTICOLARE 2
183700940804     C                   MOVEL     *BLANKS       V5DTC2
183800940804     C                   MOVEL     *BLANKS       V7DTC2
183900940711     C     ARBTC2        IFNE      ' '
184000940711     C                   MOVEL     '1P'          COD
184100940711     C                   MOVEL     *BLANKS       KEY
184200940711     C                   MOVEL     ARBTC2        KEY
184300940713     C     KTAB1         CHAIN     TABEL                              31
184400940804     C  N31              MOVEL     TBLUNI        V5DTC2
184500940804     C   31              MOVEL     *BLANKS       V5DTC2
184600940804     C  N31              MOVEL     TBLUNI        V7DTC2
184700940804     C   31              MOVEL     *BLANKS       V7DTC2
184800940711     C                   END
184900930113     C* TIPO MANCATA CONSEGNA
185000930113     C                   MOVEL     *BLANKS       DESCCA
185100160505     C                   MOVEL     *BLANKS       DESCCA7
185200160506     C                   clear                   v7dcca
185300160610     C                   clear                   ds7o
185400160610     C                   clear                   w_ccaleg
185500161014     C                   clear                   w_orig
185600930113     C     ARBCCA        IFNE      ' '
185700930705     C                   Z-ADD     1             C
185800930705     C     ARBCCA        LOOKUP    KCA(C)                                 31
185900160505     C   31              MOVEL     DCA(C)        ALF20            20
186000160505     C  N31              MOVEL     *BLANKS       ALF20
186100160610     C   31              MOVEL     dca(c)        ds7o
186200160505     c                   eval      descca ='- ' + %subst(alf20:1:12)
186300160505     c                   eval      descca7=arbcca+'-'+alf20
186400160506     c                   eval      v7dcca='C.Anomala'
186500160610     c                   if        §7otle<>*blanks
186600161014     c     karb2         chain     fnlbl02l
186700161014     c                   if        %found(fnlbl02l)
186800160610     c                   eval      w_ccaleg='S'
186900160614     c                   seton                                        84
187000161014     c                   if        LBLAAO=LBLAAP and
187100161014     c                             LBLLPO=LBLLPP and
187200161014     c                             LBLNRO=LBLNRP and
187300161014     c                             LBLNSO=LBLNSP
187400161014     c                   eval      w_orig  ='S'
187500161014     c                   endif
187600160610     c                   endif
187700160610     c                   endif
187800930113     C                   END
187900911022     C* GIRO DATA SPEDIZIONE
188000940712     C                   Z-ADD     ARBDSP        G02INV
188100940712     C                   Z-ADD     *ZEROS        G02DAT
188200940712     C                   MOVEL     '3'           G02ERR
188300940712     C                   CALL      'XSRDA8'
188400940712     C                   PARM                    WLBDAT
188500940719     C                   Z-ADD     G02DAT        V3CDSP
188600070403     c*
188700911022     C* DATA BORDERO'
188800070403     c                   if        arbdbr>0
188900070403     c     *iso          movel     arbdbr        dataeur
189000070403     c                   movel     dataeur       datadmy
189100070404     c                   movel     dataeur       w08dbr            8 0
189200070404     c                   movel     datadmy       v06dbr
189300070403     c                   else
189400070404     c                   clear                   v06dbr
189500170117     c                   clear                   w08dbr
189600070403     c                   endif
189700070404     c
189800070404     c* Prendo da fnblp i dati ritiro merce
189900150313     c***  karb2         chain     fnblp01l
190000150313     c***                if        %found(fnblp01l) and blpdrt>0
190100150313     c                   if        arbdrt >0
190200150313     c     *iso          movel     arbdrt        dataeur
190300161207     c                   monitor
190400070403     c                   movel     dataeur       datadmy
190500070404     c                   movel     datadmy       v06drt
190600161207     c                   on-error
190700161207     c                   clear                   v06drt
190800161207     c                   endmon
190900150313     c                   movel     arbnrt        v7cnrt
191000150313     c                   movel     arbctm        v3cctm
191100150313     c                   if        arbfpp='M'
191200070410     c                   eval      v7dfppr='MAT'
191300070404     c                   else
191400070410     c                   eval      v7dfppr='POM'
191500070404     c                   endif
191600150313     C                   MOVEL     arbPDR        V7CPDR
191700150313     C                   movel     arbPdr        kpdpdr
191800070404     C                   move      'A'           kpdtip
191900070404     C     kfapd         chain     fiapd01L
192000070404     c                   if        %found(fiapd01l)
192100070404     C                   MOVEL     APDRSC        V7DPDR
192200070404     c                   else
192300070404     C                   MOVEL     *BLANKS       V7DPDR
192400070404     c                   endif
192500070404     c
192600070404     c
192700070403     c                   else
192800070404     c                   clear                   v06drt
192900070404     c                   clear                   v7cnrt
193000070406     c                   clear                   v3cctm
193100070504     c                   clear                   v7dfppr
193200070404     c                   clear                   v7dpdr
193300070404     c                   clear                   v7cpdr
193400070403     c                   endif
193500070403     c
193600961112     C* SOLO PER LE BOLLE EXPORT VISUALIZZO LA DATA STAMPA MANIFEST
193700070404     C                   CLEAR                   V06DT3
193800171110     c                   if        *in16  and not *in62
193900171110     c                                    and arbdt3>0  and arbdt3<>99999999
194000070404     c     *iso          movel     arbdt3        dataeur
194100070404     c                   movel     dataeur       datadmy
194200070404     c                   movel     datadmy       v06dt3
194300961112     C                   ENDIF
194400940713     C* DATA PARTENZA PRIMO COLLO
194500070404     c                   if        arbdpc>0
194600070404     c     *iso          movel     arbdpc        dataeur
194700070404     c                   movel     dataeur       datadmy
194800070404     c                   movel     datadmy       v06dpc
194900070404     c                   movel     dataeur       w08dpc
195000070404     c                   else
195100070404     c                   clear                   v06dpc
195200070404     c                   clear                   w08dpc            8 0
195300070404     c                   endif
195400940713     C* DATA PARTENZA ULTIMO COLLO
195500070403     C**                 Z-ADD     ARBDUC        G02INV
195600070403     C**                 Z-ADD     *ZEROS        G02DAT
195700070403     C**                 MOVEL     '3'           G02ERR
195800070403     C**                 CALL      'XSRDA8'
195900070403     C**                 PARM                    WLBDAT
196000070403     C**                 Z-ADD     G02DAT        V3CDUC
196100070403     c                   if        arbduc>0
196200070404     c     *iso          movel     arbduc        dataeur
196300070403     c                   movel     dataeur       datadmy
196400070404     c                   movel     datadmy       v06duc
196500070404     c                   movel     dataeur       w08duc
196600070403     c                   else
196700070404     c                   clear                   v06duc
196800070404     c                   clear                   w08duc            8 0
196900070403     c                   endif
197000911022     C* DATA ENTRATA AL TRANSITO
197100070404     c                   if        arbdet>0
197200070404     c     *iso          movel     arbdet        dataeur
197300070404     c                   movel     dataeur       datadmy
197400070404     c                   movel     datadmy       v06det
197500070404     c                   else
197600070404     c                   clear                   v06det
197700070404     c                   endif
197800911022     C* DATA USCITA DAL TRANSITO
197900070404     c                   if        arbdut>0
198000070404     c     *iso          movel     arbdut        dataeur
198100070404     c                   movel     dataeur       datadmy
198200070404     c                   movel     datadmy       v06dut
198300070404     c                   else
198400070404     c                   clear                   v06dut
198500070404     c                   endif
198600930113     C* DATA ARRIVO MERCE
198700070403     c                   if        arbdam>0
198800160506     c                   setoff                                       96
198900160914     c                   clear                   v7ddam
199000070403     c     *iso          movel     arbdam        dataeur
199100070403     c                   movel     dataeur       datadmy
199200070404     c                   movel     datadmy       v06dam
199300160914     c                   if        v7dlnaa<>*blanks
199400150616     c                   clear                   fnlrj6ds
199500150616     c                   eval      ILRJ6TRIC='A'
199600150616     c                   eval      ILRJ6AAS =arbaas
199700150616     c                   eval      ILRJ6LNP =arblnp
199800150616     c                   eval      ILRJ6NRS =arbnrs
199900150616     c                   eval      ILRJ6NSP =arbnsp
200000150616     c                   call      'FNLRJ6R'
200100150616     c                   parm                    fnlrj6ds
200200160517     c* Arrivo alla filiale finale di arrivo:
200300160517     c* Totale
200400150616     c                   if        olrj6err=*blanks and olrj6fall='S'
200500150616     c                   eval      v7ddam='Totale'
200600150616     c                   else
200700160517     c* Parziale
200800160517     c                   if        OLRJ6CDCM+OLRJ6CSPU>0
200900160517     c                   eval      v7ddam='Parziale'
201000160517     c                   else
201100160517     c* Nessuno
201200160517     c                   eval      v7ddam='Nessuno '
201300160517     c                   endif
201400150616     c                   endif
201500161110     c* data cosegna parziale
201600161110     c                   if        arbdcp>0
201700161110     c     *iso          move      arbdcp        datadmy
201800161110     c                   movel     datadmy       w0060
201900161110     c                   eval      %subst(v7drig1:1:22)='ConsParziale'       +
202000161110     c                             hi + %editW(w0060:'  /  /  ') + c_DspAtrNorm
202100161110     c* indico anche quanti colli consegnati sul totale dei colli della spedizione
202200161110     c                   if        arbdcm=0
202300161110     c* spazio di v7drig1 utilizzabile per indicare 'di colli'  --> 8 byte
202400161110     c                   eval      %subst(v7drig1:23:8)= 'di Colli'
202500161110     c* spazio di v7drig1 utilizzabile per indicare colli/colli --> 11 byte
202600161110     c* senza contare i due attributi iniziale (pos 31) e finale
202700161110     c                   eval      %subst(v7drig1:31:13)= hi
202800161110     c                             + %trim(%editc(OLRJ6CDCM:'Z'))
202900161110     c                             + '/'
203000161110     c                             + %trim(%editc(OLRJ6NCL:'Z'))
203100161110     c                             + c_DspAtrNorm
203200161110     c* Determino quanti spazi vuoti rimasti in fondo alla sottostringa del
203300161110     c* campo per poi riempirli con '-'
203400161110     c* Se la sottostringa è stata utilizzata tutta in
203500161110     c* posizione 43 c'è l'attributo di visualizzazione (c_DspAtrNorm)
203600161110     c* e quindi:
203700161110     c* - campo pos non sarà mai = 0
203800161110     c* - se pos = 43 in questo byte non metterò il trattino
203900161110     c                   eval      pos=%checkr(' ':%subst(v7drig1:1:43))
204000161110     c* considero quindi solo quando pos < 43
204100161110     c                   if        pos<43
204200161110     c                   eval      pos+=1
204300161110     c                   eval      %subst(v7drig1:pos:(43-pos+1))=*all'-'
204400161110     c                   endif
204500161110     c                   endif
204600161110     c                   endif
204700160914     c                   endif
204800070403     c                   else
204900160506     c                   seton                                        96
205000070404     c                   clear                   v06dam
205100070403     c                   endif
205200150615
205300150615
205400150615     c
205500131121
205600930504     C* DATA CONSEGNA MERCE REALE
205700160610     c                   if        arbdcm>0  and w_ccaleg=*blanks
205800070413     C                   SETON                                        69
205900161014     c****               eval      v7cdcon='REALE'
206000161017     c                   if        arbcca=*blanks and §3arbl<>'R'
206100161031     c                             and arbcbo<>'M '
206200161014     c                   eval      v7dcons='CONSEGNATA:'
206300161014     c                   eval      v6ddcm ='CONSEGNA'
206400161014     c                   eval      v5ddcm ='REALE '
206500161014     c                   else
206600161014     c                   eval      v7dcons='CHIUSA . .:'
206700161014     c                   eval      v5ddcm ='CHIUSA'
206800161017     c                   eval      v6ddcm ='CHIUSA'
206900161017     c                   seton                                        19
207000161014     c                   endif
207100070404     c     *iso          movel     arbdcm        dataeur
207200070404     c                   movel     dataeur       datadmy
207300070404     c                   movel     dataeur       v08dcm
207400070404     c                   movel     datadmy       v06dcm
207500161014     c                   if        arbhmc>0
207600161014     c                   eval      vidhmc=%editw(arbhmc: '0 :  ')
207700161014     c                   endif
207800070404     c                   else
207900070404     c                   clear                   v06dcm
208000070404     c                   clear                   v08dcm
208100170120     c                   clear                   v6ddcm
208200170120     c                   clear                   v5ddcm
208300161014     c* solo se originale
208400161014     c                   if        blp_arbdcm>0 and W_orig='S'
208500161018     c                   if        (arbcca<>'2' and arbcca<>'6') or
208600161018     c                             blp_arbdcm<>arbdcm
208700160914     C                   SETON                                        69
208800161014     c                   eval      v7dcons='CONSEGNATA:'
208900161014     c                   eval      v6ddcm ='CONSEGNA'
209000161014     c                   eval      v5ddcm ='REALE  '
209100160913     c     *iso          movel     blp_arbdcm    dataeur
209200160913     c                   movel     dataeur       datadmy
209300160913     c                   movel     dataeur       v08dcm
209400160913     c                   movel     datadmy       v06dcm
209500161014     c                   if        blp_arbhmc>0
209600161014     c                   eval      vidhmc=%editw(blp_arbhmc: '0 :  ')
209700161014     c                   endif
209800161018     c                   endif
209900161014     c                   else
210000161014     c                   if        w_orig<>'S' and arbdcm>0
210100161014     c                   eval      v6ddcm ='CHIUSA'
210200161014     c                   eval      v5ddcm ='CHIUSA'
210300161017     c                   seton                                        19
210400161014     c     *iso          movel     arbdcm        dataeur
210500161014     c                   movel     dataeur       datadmy
210600161014     c                   movel     dataeur       v08dcm
210700161014     c                   movel     datadmy       v06dcm
210800161014     c                   if        arbhmc>0
210900161014     c                   eval      vidhmc=%editw(arbhmc: '0 :  ')
211000161014     c                   endif
211100161014     c                   endif
211200160913     c                   endif
211300070404     c                   endif
211400131121
211500930505     C* DATA CONSEGNA PREVISTA ESTERNA/ORE DI RITARDO
211600070404     C                   Z-ADD     0             v08DCE
211700140113     C                   Z-ADD     0             amgDCE            8 0
211800150615     C*******            Z-ADD     0             v06dce
211900940711     C                   Z-ADD     0             VIDNRC
212000131121     C*******            SETON                                        68
212100131121     C                   MOVEL     ' '           D98TLA
212200150611     C                   MOVEL     w_tbo         D98TBO
212300131121     C                   MOVEL     ARBAAS        D98AAS
212400131121     C                   MOVEL     ARBLNP        D98LNP
212500131121     C                   MOVEL     ARBNRS        D98NRS
212600131121     C                   MOVEL     ARBNSP        D98NSP
212700131121     C*
212800131121     C                   CALL      'TNSD99R'
212900131121     C                   PARM                    TNSD99DS
213000140110     c
213100140110     c* Mi salvo data teorica consegna
213200140110     c                   eval      s_arbdei=d98dei
213300140110     c                   eval      s_arboei=d98oei
213400150729     c
213500150729     c* memorizzo ore trazione è ore consegna cliente da passare al trulorsr
213600150729     c                   eval      s_arbttc=d98ttc
213700150729     c                   eval      s_arbtcc=d98tcc
213800131121     C*
213900140110    1C     ARBDCE        IFEQ      0
214000131121    2c                   if        d98dce>0
214100140113     c                   eval      amgdce=d98dce
214200131121     c     *iso          movel     d98dce        dataeur
214300150615     c****               movel     dataeur       datadmy
214400131121     c                   movel     dataeur       v08dce
214500150615     c****               movel     datadmy       v06dce
214600131121   x2c                   else
214700131121     c                   clear                   v08dce
214800150615     c***                clear                   v06dce
214900131121    2c                   endif
215000131121     c
215100131121     C                   Z-ADD     D98NRC        VIDNRC
215200160523     c*
215300160523     c**n69              eval      vidhmc=%editw(d98hce: '0 :  ')
215400131121     C*
215500131121     C* SE COMODO = 1  E' STATA FATTA CALL PGM
215600131121   x1C                   ELSE
215700140113     c                   eval      amgdce=arbdce
215800070404     c     *iso          movel     arbdce        dataeur
215900150615     c***                movel     dataeur       datadmy
216000070404     c                   movel     dataeur       v08dce
216100150615     c***                movel     datadmy       v06dce
216200070404     c
216300940711     C                   Z-ADD     ARBNRC        VIDNRC
216400131121    1C                   END
216500131121     c
216600911023     C* DATA MANDATO INTROITO
216700940712     C                   Z-ADD     ARBDMI        G02INV
216800940712     C                   Z-ADD     *ZEROS        G02DAT
216900940712     C                   MOVEL     '3'           G02ERR
217000940712     C                   CALL      'XSRDA8'
217100940712     C                   PARM                    WLBDAT
217200940712     C                   Z-ADD     G02DAT        VIDDMI
217300930113     C* DATA PRIMA CONSEGNA PARZIALE
217400930113     C                   Z-ADD     0             VIDDCP
217500150615     C***                Z-ADD     0             VIDDCP6
217600930113     C     ARBDCP        IFGT      0
217700940712     C                   Z-ADD     ARBDCP        G02INV
217800940712     C                   Z-ADD     *ZEROS        G02DAT
217900940712     C                   MOVEL     '3'           G02ERR
218000940712     C                   CALL      'XSRDA8'
218100940712     C                   PARM                    WLBDAT
218200940712     C                   Z-ADD     G02DAT        VIDDCP
218300150615     c***  *iso          move      arbdcp        datadmy
218400150615     c***                movel     datadmy       viddcp6
218500150611     c
218600930113     C                   END
218700161122     c* Nella videata di riepilogo,
218800160505     c* Visualizzo data distinta e padroncino solo se numero distinta>0
218900160505     c                   clear                   vidddc
219000161122     c                   clear                   vidddc5
219100160505     c                   clear                   v7ddel
219200160505     c                   clear                   vidpdc
219300161122     c                   clear                   vidpdc5
219400160505     c                   clear                   v7daut
219500160505     C                   MOVEL     *BLANKS       DESPDC
219600161122     C                   MOVEL     *BLANKS       apdrsc_w
219700160505     C                   MOVEL     *BLANKS       DESPD1
219800160505     C* DATA DISTINTA -
219900940712     C                   Z-ADD     ARBDDC        G02INV
220000940712     C                   Z-ADD     *ZEROS        G02DAT
220100940712     C                   MOVEL     '3'           G02ERR
220200940712     C                   CALL      'XSRDA8'
220300940712     C                   PARM                    WLBDAT
220400161122     C                   Z-ADD     G02DAT        VIDDDC5
220500161122     C* CODICE PADRONCINO CONSEGNA
220600161122     C                   MOVEL     ARBPDC        VIDPDC5
220700161122     C     ARBPDC        IFGT      0
220800161122     C                   movel     arbpdc        kpdpdr
220900161122     C                   move      'A'           kpdtip
221000161122     C     kfapd         chain     fiapd01L                           31
221100161122     C  N31              MOVEL     APDRSC        DESPDC
221200161122     C  N31              MOVEL     APDRSC        apdrsc_w
221300161122     C                   END
221400161122     c                   if        arbndc>0
221500161122     c                   eval      vidddc=vidddc5
221600160505     c                   if        vidddc>0
221700160505     c                   eval      v7ddel='del'
221800160505     c                   endif
221900911022     C* CODICE PADRONCINO CONSEGNA
222000940712     C                   MOVEL     ARBPDC        VIDPDC
222100160505     c                   if        vidpdc>0
222200160505     c                   eval      v7daut='Autotr'
222300160505     c                   endif
222400161122     C                   MOVEL     APDRSC_w      DESPD1
222500161122     C*    ARBPDC        IFGT      0
222600161122     C*                  movel     arbpdc        kpdpdr
222700161122     C*                  move      'A'           kpdtip
222800161122     C*    kfapd         chain     fiapd01L                           31
222900161122     C* N31              MOVEL     APDRSC        DESPDC
223000161122     C* N31              MOVEL     APDRSC        DESPD1
223100161122     C*                  END
223200160505     c                   endif
223300950324     C* PRESTAZIONE PADRONCINO MATTINO O POMERIGGIO
223400950324     C                   MOVE      *BLANKS       FVVFPP
223500131120     C***                MOVE      *BLANKS       V5DFPP
223600070404     C                   MOVE      *BLANKS       V7DFPP
223700950327     C                   Z-ADD     ARBNDC        WNDC
223800150617     c                   if        w_tbo='A'
223900020906     c                   Move      Arbifp        Wfgs
224000020906     c     Kfvv1         Chain     Fnfvv01l                           31
224100950324     C                   SELECT
224200991027     C                   WHEN      *IN31 = *on
224300991027     C                   WHEN      FVVFPP = 'M'
224400131120     C**                 eval      V5DFPP = 'MATTINO'
224500070504     C                   eval      V7DFPP = 'Mat'
224600991027     C                   WHEN      FVVFPP = 'P'
224700131120     C**                 eval      V5DFPP = 'POMERIGGIO'
224800070504     C                   eval      V7DFPP = 'Pom'
224900950324     C                   ENDSL
225000131119     c* controllo se ha PDA
225100131119     C                   Z-ADD     ARBNDC        kNDC6
225200131120     c                   clear                   vidpda
225300131120     c     Kdst          Chain     Fidst01l
225400131120     c                   if        %found(fidst01l)
225500131119     C                   movel     dstflr        DDStflr
225600131120     c                   if        (§dsttstpda= 'C' or  §dsttstpda= 'E') or
225700131120     c                             (dstpda<>'C' and  dstpda<>'E')
225800131120     c                   eval      vidpda='NO PDA'
225900150618     c*****              movel     ri            ATRpda
226000131121     c                   else
226100131121     c                   eval      vidpda='SI PDA'
226200150618     c****               movel     ri            ATRpda
226300131119     c                   endif
226400131119     c                   endif
226500150617     c                   endif
226600131108     c
226700131108     c* Se bolla in distinta, verifico quanti colli
226800131108     c                   clear                   v7dncl_co
226900131115     c                   clear                   v7dncl_de
227000131108     c                   clear                   wcol_D
227100131108     c                   clear                   wcol_I
227200131108    1c                   if        arbndc>0 and arbddc>0 and arbfbc=' '
227300150616     c                             and w_tbo='A'
227400131108     c* se ritorno all'incasso lo scrivo
227500131108     c
227600150616    2c                   if        arbdcm>0  and (arbicc='R' or arbicc=' ' or
227700131108     c                                  arbica=' ' or arbica='R')
227800160721     c                             and dstfcf<>'S'
227900131115     c                   eval      v7dncl_de='     IN consegna :'
228000131108     c                   eval      v7dncl_co='PER INCASSO'
228100131108    2c                   endif
228200131108     c
228300131108     c* Se colli da consegnare
228400131108    2c                   if        arbdcm=0
228500131108     C                   Z-ADD     ARBNDC        knfv
228600131108     c                   Move      Arbifp        Wfgs
228700131108     c
228800131108     c     karb2         setll     fnart01l
228900131108     c     karb2         reade     fnart01l
229000131108     c
229100131108    3c                   dow       not %eof(fnart01l)
229200131108     c                   if        artdcm=0
229300131108     c                   add       1             wcol_d
229400131108     c     kbrv          chain     fnbrv01l
229500131108     c                   if        %found(fnbrv01l)
229600131108     c                   add       1             wcol_I
229700131108     c                   endif
229800131108     c                   endif
229900131108     c
230000131108     c
230100131108     c     karb2         reade     fnart01l
230200131108    3c                   enddo
230300131108
230400131108    3c                   if        wcol_d>0
230500131115     c                   eval      v7dncl_de='Colli in consegna:'
230600131108    4c                   if        wcol_i=0
230700131108     c                   eval      v7dncl_co='0/'+%trim(%editc(wcol_d:'Z'))
230800131108     c                   else
230900131108     c                   eval      v7dncl_co=%trim(%editc(wcol_I:'Z')) +'/' +
231000131108     c                                       %trim(%editc(wcol_d:'Z'))
231100131108    4c                   endif
231200131108    3c                   endif
231300131108    c
231400131108    2c                   endif
231500131108    1c                   endif
231600131108     c
231700131108     c* Visualizzo ultimo evento
231800131108     c                   clear                   v7duevb
231900150616     c                   clear                   w_ar4_1
232000160614     c                   clear                   w_evbcev
232100161107     c                   clear                   w_tcev
232200160610     c* Se bolla legata con consegna anomala com ultimo evento devo visualizzare
232300160610     c* data e ora in cui è avvenuto il dirottamento
232400161017    1c                   if        w_ccaleg='S'
232500161017    2c                   if        arbcca='9' or w_tbo='P'
232600160913     c* Per cambio di porto la data/ora coincide con la data/ora di creazione della bolla
232700160913     c* figlia
232800160913     c* idem per resi e dirottamenti che si trovano solo su BLP
232900160913     c* In questi casi la DCM contiene infatti la data di consegna della merce e non la data in
233000160913     c* cui è stato fatto il cambio di porto o dirottamento o reso
233100160913     c     karb2         setll     fnlbl02l
233200160913     c     karb2         reade     fnlbl02l
233300161017    3c                   if        not %eof(fnlbl02l)
233400160913     c                   Z-Add     lblAAN        Kaas
233500160913     c                   Z-Add     lblLPN        Klnp
233600160913     c                   Z-Add     lblNRN        Knrs
233700160913     c                   Z-Add     lblNSN        Knsp
233800160913     c                   Eval      Ktrd = 'GEN'
233900160913     c     kAr51         Chain     Fiar501l
234000161017If  4c                   If        %Found(Fiar501l)
234100160913     c     *iso          movel     ar5dac        dataeur
234200160913     c                   movel     dataeur       w0080
234300160913     c                   movel     ar5orc        w0040
234400160913     c                   eval      v7duevb=%trim(§7odei)+' il ' +
234500160913     c                             %editw(w0080:'  /  /    ')  + ' '
234600160913     c                             + %editw(w0040: '0 :  ')
234700161017    4C                   ENDIF
234800161017    3c                   endif
234900160913     c
235000161017   x2c                   else
235100161017    3c                   if        arbdcm>0
235200160614     c     *iso          movel     arbdcm        dataeur
235300160614     c                   movel     dataeur       w08dcm
235400161017     c**                 if        arbhmc>0
235500161017     c***                eval      vidhmc=%editw(arbhmc: '0 :  ')
235600161017     c***                endif
235700160610     c                   eval      v7duevb=%trim(§7odei)+' il ' +
235800161017     c                             %editw(w08dcm:'  /  /    ')
235900161017    4c                   if        arbhmc>0
236000161017     c                   eval      v7duevb=%trim(v7duevb) + ' ' +
236100161017     c                             %editw(arbhmc: '0 :  ')
236200161017    4c                   endif
236300161017     c***                          %editw(w08dcm:'  /  /    ')  + ' ' + vidhmc
236400161017   x3c                   else
236500160913     c                   eval      v7duevb=%trim(§7odei)
236600161017    3c                   endif
236700161017    2c                   endif
236800161017   x1c                   else
236900131114     c* prima vedo se c'e' da PDA
237000131114     c                   move      arbndc        pctndc
237100140224     c                   move      arbifp        pctfgs
237200131114     c                   move      'CET'         ttrd
237300131114     c     kcet          setgt     fipct02l
237400131114     c     kcet          readpe    fipct02l
237500161017    2c                   if        not %eof(fipct02l)
237600131114     c                   movel     pctdati       fipctcetds
237700131114     c* consegnata
237800161017    3c                   if        §pctcmc = ' '
237900131114     c                   MOVE      '  C'         evbcev                         *causale evento
238000161017   x3c                   else
238100131114     c* mancata consegna
238200131114     c                   movel     §PCTcmc       evbcev
238300161017    3c                   endif
238400131114     c                   z-add     §pctdata      evbdev
238500131114     c                   movel     §pctora       evbhev
238600131114
238700131114     c                   exsr      Decodi_EVB
238800160609     c                   eval      v7duevb=%trim(v7duevb)+' ' +
238900160609     c                                                 'da PDA'
239000161108     c                   eval      wcev=§pctriserv
239100161108     c                   exsr      sr_2a
239200161108     c                   if        §2atip='R'
239300161108     c                   eval      v7duevb=%trim(v7duevb)+  red
239400161108     c                             + %trim(§2ades) + c_DspAtrNorm
239500161108     c                   endif
239600161107     c                   eval      w_tcev='D'
239700131114
239800161017   x2c                   else
239900150616     c* se spedizione consegnata e presente firmatario lo visulizzo
240000150616     c* al posto dell'ultimo evento
240100161017    3c                   if        arbdcm>0
240200150616     C                   MOVEL     ARBAAS        KAAS
240300150616     C                   MOVEL     ARBLNP        KLNP
240400150616     C                   MOVEL     ARBNRS        KNRS
240500150616     C                   MOVEL     ARBNSP        KNSP
240600150616     C                   MOVEL     '1'           KTRC
240700150616     c     kar41         chain     fiar401l
240800161017    4c                   if        %found(fiar401l) and ar4not<>*blanks
240900150616     c                   eval      evbcev='  C'
241000150616     c                   clear                   evbdev
241100150616     c                   clear                   evbhev
241200150616     c                   eval      w_ar4_1='S'
241300150616     c                   exsr      Decodi_EVB
241400161017    4c                   endif
241500161017    3c                   endif
241600131114     c*
241700161108    3c****               if        w_ar4_1=*blanks
241800131108     C                   Z-ADD     ARBAAS        KAAS$
241900131108     C                   Z-ADD     ARBLNP        KLNP
242000131108     C                   Z-ADD     ARBNRS        KNRS
242100131108     C                   Z-ADD     ARBNSP        KNSP
242200131108     C     KEVB1         SETgt     FNEVB01L
242300131108     C     KEVB1         readpe    FNEVB01L
242400131108     c
242500161017    4c                   if        not %eof(fnevb01l)
242600161108    5c                   if        w_ar4_1=*blanks
242700131114     c                   exsr      Decodi_EVB
242800161108   x5c                   else
242900161108     c* Se spedizione consegnata e ultimo evento di riserva visualizzo di fianco alla Firma
243000161108     c                   eval      wcev=evbcev
243100161108     c                   exsr      sr_2a
243200161108     c                   if        §2atip='R'
243300161108     c                   eval      v7duevb=%trim(v7duevb)+  red
243400161108     c                             + %trim(§2ades) + c_DspAtrNorm
243500161108     c                   endif
243600161108    5c                   endif
243700161017    4c                   endif
243800161108    3c****               endif
243900161017    2c                   endif
244000161017    1c                   endif
244100160505     c                   clear                   v7duevd
244200160506     c                   if        v7duevb<>*blanks
244300160505     c                   eval      v7duevd='Ultimo evento'
244400160505     c                   endif
244500911022     C*
244600911022     C* SE ESISTE AGGANCIO 2° BOLLA
244700991026     C                   Z-ADD     ARBAAS        KAAS
244800991026     C                   Z-ADD     ARBLNP        KLNP
244900991026     C                   Z-ADD     ARBNRS        KNRS
245000991026     C                   Z-ADD     ARBNSP        KNSP
245100991026     C                   MOVEL     '2'           KTRC
245200991027     C     KAR61         CHAIN     FIAR6000                           31
245300911023     C*
245400040216if  1C                   IF          %FOUND
245500991027     C                   eval      *in22 = *on
245600991027     C                   MOVEL     AR6CEI        V9CCEI
245700991027     C                   MOVEL     AR6NFT        V9CNFT
245800991027     C                   MOVEL     AR6FIV        V9CFIV
245900991027      *
246000991027     C                   eval      V9CDIV = AR6div
246100991027     C                   eval      WDIV = AR6div
246200991027     C                   exsr      CTR_VALUTA
246300991027      *
246400001128     C                   clear                   v9dfim
246500001128     C                   clear                   v9dfi2
246600991027     c                   if        AR6fim = 'P'
246700991027     c                   eval      V9Dfim = 'PARTENZA'
246800040126     c                   eval      V9Dfi2 = 'Immesse in'
246900991027     c                   endif
247000991027      * Varia G
247100040130     c                   clear                   W0133
247200040130     c                   clear                   v9csv1
247300040130     c                   clear                   v9cva1
247400040130     c                   clear                   v9csv2
247500040130     c                   clear                   v9cva2
247600040130     c                   clear                   v9csv3
247700040130     c                   clear                   v9cva3
247800040130     c                   clear                   v9csv4
247900040130     c                   clear                   v9cva4
248000041130     c                   clear                   v9csv5
248100041130     c                   clear                   v9cva5
248200041130     c                   clear                   v9csv6
248300041130     c                   clear                   v9cva6
248400040130     c                   clear                   v9civa
248500040130
248600040122      * Carico le varie
248700040122     c                   Movel     Ar6Sv1        wsvn
248800040122     c                   Z-add     Ar6Va1        wvan
248900040122     c                   ExSr      Car_Varie1
249000040122
249100040122     c                   Movel     Ar6Sv2        wsvn
249200040122     c                   Z-add     Ar6Va2        wvan
249300040122     c                   ExSr      Car_Varie1
249400040122
249500040122     c                   Movel     Ar6Sv3        wsvn
249600040122     c                   Z-add     Ar6Va3        wvan
249700040122     c                   ExSr      Car_Varie1
249800040122
249900040122     c     Kar61         Setll     Fiar7000
250000040216do  2c                   Do        *Hival
250100040122     c     Kar61         Reade     Fiar7000
250200040122     c                   If        %Eof(Fiar701l)
250300040122     c                   Leave
250400040122     c                   EndIf
250500040122     c                   Movel     Ar7Svn        wsvn
250600040122     c                   Z-add     Ar7Van        wvan
250700040216     c                   ExSr      Car_Varie1
250800040216e   2c                   EndDo
250900040122
251000991027      * TOTALE FATTURA
251100991027     c                   clear                   W0133
251200991027     c                   IF          AR6ift > *zeros
251300991027     c                   z-add     AR6ift        W0133
251400991027     c                   eval      Wtstdec = §cvfdc
251500991027     c                   exsr      EDTimporti
251600991027     c                   movel     IMPeditato    V9cift
251700001128     C                   else
251800001128     C                   clear                   v9cift
251900991027     c                   endif
252000991027      *
252100991027     c                   If         AR6dft > *zeros
252200991027     C     *iso          movel     AR6DFT        DATA_EUR
252300991027     C     *eur          movel     data_eur      W0080
252400991027     C                   eval      V9cdft = %EDITC(W0080:'Y')
252500001128     C                   else
252600001128     C                   clear                   v9cdft
252700991027     c                   Endif
252800991027      *
252900991027     c                   eval      V9Cksc = AR6ksc
253000991027     c                   eval      V9Cctr = AR6ctr
253100991027     c                   move      v9cksc        W0040
253200991027     c                   If        W0040 = 9999
253300991027     c                   eval      V9Dksc = arbrsd
253400991027     c                   Else
253500991027     c                   eval      i69kac = V9Cksc
253600991027     c                   exsr      CTR_ANAGRA
253700991027     c                   eval      V9Dksc = acorag
253800991027     c                   Endif
253900991027      *
254000991027     C                   eval      COD = 'TR'
254100991027     C                   movel     V9CCTR        UNOCTR            1
254200991027     C                   eval      KEY = unoctr
254300991027     C     KTAB1         CHAIN     TABEL00F                           31
254400991027     C                   movel     TBLUNI        V9DCTR
254500991027      *
254600930114     C* ESENZIONE IVA
254700001128     C                   clear                   v9dcei
254800991111     C     AR6CEI        IFNE      *blanks
254900940721     C                   MOVEL     'EI'          COD
255000991111     C                   MOVEL(P)  AR6CEI        KEY
255100940719     C     KTAB1         CHAIN     TABEL00F                           31
255200991027     C  N31              MOVEL     TBLUNI        V9DCEI
255300940719     C                   ENDIF
255400991027      *
255500040216e   1C                   ENDIF
255600150611
255700150611     c* RICERCA E DECODIFICA STATO SPEDIZIONE
255800150611     c                   clear                   v7dst1
255900150611     c                   clear                   v7dst2
256000150616     c                   clear                   v7dst3
256100150616     c                   setoff                                       58
256200160719    1c***                if        not *in69 and w_ccaleg=*blanks
256300160719    1c                   if        w_ccaleg=*blanks
256400150611     c                   clear                   fnlru3ds
256500150611     c                   eval      ILRU3AAS=arbaas
256600150611     c                   eval      ILRU3LNP=arblnp
256700150611     c                   eval      ILRU3NRS=arbnrs
256800150611     c                   eval      ILRU3NSP=arbnsp
256900150611     c                   eval      ILRU3LNA=arblna
257000150611     c                   eval      ILRU3NTC=arbntc
257100150611     c                   eval      ILRU3TCR=arbtcr
257200150611     c                   eval      ILRU3DCR=arbdcr
257300150611     c                   eval      ILRU3HCR=arbhcr
257400171110     c                   if        *in16 and not *in62
257500160506     c                   eval      ILRU3dip=arbdt3
257600160506     c                   endif
257700160614     c                   eval      ILRU3ucev=w_evbcev
257800161107     c                   eval      ILRU3ITCEV=w_tcev
257900160719     c                   eval      ILRU3ica =arbica
258000160719     c                   eval      ILRU3icc =arbicc
258100160825     c                   if        w_tbo='P' and arbdcm>0
258200160825     c                   eval      ILRU3ica ='S'
258300160825     c                   eval      ILRU3icc ='S'
258400160825     c                   endif
258500161027     c                   eval      ilru3ca=StatoCa
258600150611
258700150611     c                   eval      w_video='N'
258800150611     c                   exsr      sr_orarisr
258900150612     c                   call      'FNLRU3R'
259000150611     c                   parm                    kpjba
259100150611     c                   parm                    fnlru3ds
259200150611     c                   parm                    tnsd99ds
259300150611     c                   parm                    trulor2ds
259400150612     c* valorizzazione PRIMA RIGA del video
259500150611     c                   eval      v7dst1=%trim(%xlate(mi:ma:olru3dec1)) + ' ' +
259600150611     c                                          %trim(olru3not1)
259700150612     c* calcolo da che posizione inzizia la parte "note" all'interno della stringa
259800150611     c* che metto a video per poter incolonnare la parte "note" della seconda riga
259900150611     c                   clear                   pos
260000150611     c                   eval      pos=%checkr(' ':olru3dec1)
260100150612     c                   eval      pos+=2
260200150612     c* Verifico se la parte note della prima riga è stata troncata
260300150612
260400150612     c* Lunghezza utile della stringa 1 per il campo not1
260500150612     c                   eval      lennt1v=%len(v7dst1)-pos
260600150612     c* Lunghezza effettiva del campo not1 ricevuto
260700150611     c                   eval      lennt1 =%checkr(' ':olru3not1)
260800150612    2c                   if        lennt1>lennt1v
260900150612     c                   clear                   PosA
261000150612     c                   eval      PosA=%checkr(charok:v7dst1)
261100150612     c                   if        PosA<%len(v7dst1)
261200150612     c* clearo parola troncata per andare a capo nella seconda riga
261300150612     c                   eval      %subst(v7dst1:posA+1)=*blanks
261400150612     c                   endif
261500150612    2c                   endif
261600150611
261700150612     c* valorizzazione SECONDA RIGA del video
261800150612     c                   eval      v7dst2=%trim(olru3dec2)+ ' '
261900150612     c* imposto eventuale a capo della prima riga
262000150612    2c                   if        lennt1>lennt1v
262100150612     c                   eval      %subst(v7dst2:pos)=
262200150612     c                             %trim(%subst(olru3not1:PosA+2-pos))
262300150612    2c                   endif
262400150612    2c                   if        olru3not2<>*blanks
262500150612     c* la seconda riga contiene dati della prima riga --> scrivo le note 2 nella
262600150612     c* terza riga
262700150612    3c                   if        lennt1>lennt1v
262800150612     c*****              eval      v7dst2=%trim(v7dst2)+' ' + %trim(olru3not2)
262900150612     c                   eval      %subst(v7dst3:pos)=
263000150612     c                             %trim(olru3not2)
263100150616     c                   seton                                        58
263200150612
263300150612   x3c                   else
263400150612     c* altrimenti valorizzo la seconda riga con le note2 partendo da pos
263500150612     c* per incolonnarmi con la prima riga
263600161028     c* se la prima riga è vuota le note2 le accodo (caso di sped. consengata con CA)
263700161028     c                   if        v7dst1=*blanks
263800161028     c                   eval      v7dst2=%trim(v7dst2)+' '+%trim(olru3not2)
263900161028     c                   else
264000150612     c                   eval      %subst(v7dst2:pos)=
264100150612     c                             %trim(olru3not2)
264200161028     c                   endif
264300150612     c* Verifico se la parte note della seconda riga è stata troncata
264400150612
264500150612     c* Lunghezza utile della stringa 1 per il campo not1
264600150612     c                   eval      lennt1v=%len(v7dst2)-pos
264700150612     c* Lunghezza effettiva del campo not1 ricevuto
264800150612     c                   eval      lennt1 =%checkr(' ':olru3not2)
264900150612    2c                   if        lennt1>lennt1v
265000150612     c                   clear                   PosA
265100150612     c                   eval      PosA=%checkr(charok:v7dst2)
265200150612     c                   if        PosA<%len(v7dst2)
265300150612     c* clearo parola troncata per andare a capo nella terza   riga
265400150612     c                   eval      %subst(v7dst2:posA+1)=*blanks
265500150612     c                   endif
265600150612    2c                   endif
265700150612     c* valorizzazione TERZA RIGA del video quando vado a capo con la seconda
265800150612     c* imposto eventuale a capo della prima riga
265900150612    2c                   if        lennt1>lennt1v
266000150612     c                   eval      %subst(v7dst3:pos)=
266100150612     c                             %trim(%subst(olru3not2:PosA+2-pos))
266200150616     c                   seton                                        58
266300150612    2c                   endif
266400150612    3c                   endif
266500150612    2c                   endif
266600161027     c                   if        (olru3spc='CON' or olru3spc='CAN')
266700161027     c                             and arbdcm=0
266800160609     c                   eval      *in69=*on
266900160609     c                   eval      v08dcm=w7cdev
267000161017     c                   eval      v5ddcm ='REALE  '
267100161017     c                   eval      v6ddcm ='CONSEGNA'
267200160609     c                   movel     w7cdev        dataeur
267300160609     c                   movel     dataeur       datadmy
267400160609     c                   movel     datadmy       v06dcm
267500160609     c                   eval      vidhmc=%editw(evbhev: '0 :  ')
267600160609     c*****              eval      v7dcca='da PDA'
267700161014     c                   eval      v7dcons='CONSEGNATA:'
267800160609     c                   endif
267900150612    1c                   endif
268000150616     c
268100150616     c                   if        v7dst3=*blanks and
268200150616     c                             ((v7dst1<>*blanks or v7dst2<>*blanks) or
268300160523     c                               *in69)
268400150616     c                   eval      v7dst3=*all'-'
268500150616     c                   endif
268600150616
268700150616     c                   setoff                                       54
268800150616     c                   if        arbffd='S'
268900150616     c                   eval      %subst(v7dst3:68)=' Filiale ?'
269000150616     c                   seton                                        54
269100161031     c                   if        v7dst2=*blanks and *in69
269200161031     c                   eval      v7dst2='IN FERMO DEPOSITO'
269300161031     c                   endif
269400150616     c                   endif
269500930525     C*
269600991027     C* PER I DATI SULLA TASSAZIONE 1a BOLLA, DEVO CERCARE SU FIAR600F
269700940905     C                   SETOFF                                       24
269800110705     c                   clear                   atrbl4a
269900110705     c
270000110705     c                   clear                   v6car4ksc
270100110705     c                   clear                   v6dar4ksc
270200110705     c                   clear                   v6car4ctr
270300111021     c                   clear                   VIDimv
270400111021     c                   clear                   VIDpor
270500111021     c                   clear                   VIDift
270600111021     c                   clear                   VIDdiv
270700111021     c                   clear                   VIDfim
270800111021     C                   clear                   VIDCEI
270900111021     C                   clear                   VIDNFT
271000111021     C                   clear                   VIDFIV
271100111021     C                   clear                   DESCEI
271200111021     C                   clear                   viddft
271300111021     C                   clear                   vidIVA
271400111021      *
271500111021      * CERCO LE VARIE: LE PRIME 3 SU FIAR600F, EVENTUALI  ALTRE SU FIAR700F
271600111021     C                   eval      *in35 = *on
271700131107     C                   WRITE     LRI6C06
271800111021     C                   eval      *in35 = *off
271900111021      *
272000111021     c                   clear                   NR6
272100111021     c                   clear                   VIDiva
272200110705     c
272300940712     C                   Z-ADD     ARBAAS        KAAS
272400940712     C                   Z-ADD     ARBLNP        KLNP
272500940712     C                   Z-ADD     ARBNRS        KNRS
272600940712     C                   Z-ADD     ARBNSP        KNSP
272700110705     C                   MOVEL     'A'           KTRC
272800110705     C     KAR41         CHAIN     FiAR401L
272900110705if  1C                   IF        not %FOUND
273000110705     c                   eval      atrbl4a=nodi
273100110705     c                   else
273200110705     c                   movel     ar4not        dsbl4a
273300110705     c                   if        §b4aks>0
273400110705     C                   eval      *in24 = *on
273500110705     c                   eval      v6car4ksc=§b4aks
273600110705     c                   eval      i69kac = v6car4ksc
273700110705     c                   exsr      CTR_ANAGRA
273800110705     c                   eval      v6dar4ksc = acorag
273900110705     c                   if        §b4act<>*blanks
274000110705     c                   movel     §b4act        v6car4ctr
274100110705     c                   endif
274200110705     c* non c'e' cod cliente x tassaz.
274300110705     c                   else
274400110705     c                   eval      atrbl4a=nodi
274500110705     c                   endif
274600110705     c                   endif
274700110705     c
274800991026     C                   MOVEL     '1'           KTRC
274900991027     C     KAR61         CHAIN     FIAR6000
275000110705     c
275100110705     c                   if        %found(fiar601l)
275200110705     c
275300110705      * CLIENTE E TARIFFA FATTURAZIONE
275400110705     c                   IF        *IN33 = *ON
275500110705     c                   eval      v6cksc = arbksc
275600110705     c                   eval      v6cctr = arbctr
275700110705     c                   eval      v6dksc = arbrsm
275800110705     c                   ELSE
275900110705     c                   eval      v6cksc = AR6ksc
276000110705     c                   eval      v6cctr = AR6ctr
276100160825     c                   if        w_tbo='P'
276200160825     c                   eval      vidksc = AR6ksc
276300160825     c                   eval      vidctr = AR6ctr
276400160825     c                   endif
276500110705     c                   move      v6cksc        W0040
276600110705     c                   if        W0040 = 9999
276700110705     c                   eval      v6dksc = arbrsd
276800110705     c                   else
276900110705     c                   eval      i69kac = v6cksc
277000110705     c                   exsr      CTR_ANAGRA
277100110705     c                   eval      v6dksc = acorag
277200110705     c                   endif
277300110705     c                   ENDIF
277400110705      *   decodifica tariffa
277500110705     C                   eval      COD = 'TR'
277600110705     C                   MOVEL     V6CCTR        UNOCTR            1
277700110705     C                   MOVEL(P)  UNOCTR        KEY
277800110705     C     KTAB1         CHAIN     TABEL00F                           31
277900110705     C                   MOVEL     TBLUNI        V6DCTR
278000110705     c
278100110705     c                   endif
278200991027      *
278300040216if  1C                   IF        %FOUND  and
278400991027     C                             (AR6por > *zeros  or  AR6cei <> *blanks  or
278500991027     C                              AR6sv1 <> *blanks  or  AR6sv2 <> *blanks  or
278600991027     C                              AR6sv3 <> *blanks)
278700991027      *
278800991027      * ACCENDO L'INDICATORE DI F13=TASSAZIONE SE PRESENTE ALMENO UNO DEI CAMPI
278900991027     C                   EVAL      *IN24 = *ON
279000991027      *
279100991027     C                   eval      VIDDIV = AR6div
279200991027     C                   eval      WDIV = AR6div
279300991027     C                   exsr      CTR_VALUTA
279400991027      *
279500991027     c                   IF          ar6por > *zeros
279600991027     c                   z-add     AR6por        W0133
279700991027     c                   eval      Wtstdec = §cvfdc
279800991027     c                   exsr      EDTimporti
279900991027     c                   movel     IMPeditato    VIDpor
280000991128     c                   else
280100991128     c                   clear                   VIDpor
280200991027     c                   ENDIF
280300991027      *
280400991027     c                   IF          ar6imv > *zeros
280500991027     c                   z-add     AR6imv        W0133
280600991027     c                   eval      Wtstdec = §cvfdc
280700991027     c                   exsr      EDTimporti
280800991027     c                   movel     IMPeditato    VIDimv
280900991128     c                   else
281000991128     c                   clear                   VIDimv
281100991027     c                   ENDIF
281200991027      *
281300991027     c                   IF          ar6ift > *zeros
281400991027     c                   z-add     AR6ift        W0133
281500991027     c                   eval      Wtstdec = §cvfdc
281600991027     c                   exsr      EDTimporti
281700991027     c                   movel     IMPeditato    VIDift
281800991128     c                   else
281900991128     c                   clear                   VIDift
282000991027     c                   ENDIF
282100991027      *
282200991027     c                   IF        AR6fim = 'P'
282300991027     c                   eval      VIDfim = 'PARTENZA'
282400991128     c                   else
282500991128     c                   clear                   VIDfim
282600991027    2C                   ENDIF
282700991027      *
282800991027     C                   MOVEL     AR6CEI        VIDCEI
282900991027     C                   MOVEL     AR6NFT        VIDNFT
283000991027     C                   MOVEL     AR6FIV        VIDFIV
283100991027      *
283200991027     C                   IF        AR6CEI <>  *BLANKS
283300940808     C                   MOVEL     'EI'          COD
283400991027     C                   MOVEL(P)  AR6CEI        KEY
283500940808     C     KTAB1         CHAIN     TABEL00F                           31
283600940808     C  N31              MOVEL     TBLUNI        DESCEI
283700991128     c                   else
283800991128     c                   clear                   DESCEI
283900940808     C                   ENDIF
284000991027      *
284100991027      * DATA FATTURA DA GIRARE
284200991027     C                   IF         ar6dft > *zeros
284300991027     C     *iso          movel     AR6DFT        DATA_EUR
284400991027     C     *eur          movel     data_eur      W0080
284500991027     C                   eval      VIDdft = %EDITC(W0080:'Y')
284600991128     c                   else
284700991128     c                   clear                   VIDdft
284800991027     C                   ENDIF
284900991027      *
285000991027      *
285100991027     C                   MOVEL     ar6SV1        WSVN
285200991027     C                   MOVEL     ar6VA1        WVAN
285300991027     c                   exsr      car_varie
285400991027      *
285500991027     C                   MOVEL     ar6SV2        WSVN
285600991027     C                   MOVEL     ar6VA2        WVAN
285700991027     c                   exsr      car_varie
285800991027      *
285900991027     C                   MOVEL     ar6SV3        WSVN
286000991027     C                   MOVEL     ar6VA3        WVAN
286100991027     c                   exsr      car_varie
286200991027      *
286300991027      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE FIAR700F
286400040216if  2C                   if          ar6SV3 <> *blanks
286500991027     C     KAR61         SETLL     FIAR7000
286600991027     C     KAR61         READE     FIAR7000
286700040216do  3C                   DOW       %EOF = *OFF
286800991027     C                   MOVEL     AR7SVN        WSVN
286900991027     C                   MOVEL     AR7VAN        WVAN
287000991027     c                   exsr      car_varie
287100991027     C     KAR61         READE     FIAR7000
287200040216e   3C                   ENDDO
287300040216e   2C                   endif
287400110705e   1C                   ENDIF
287500991027      *
287600991027      * Se non ho caricato record ne carico uno vuoto
287700110705     c                   IF        NR6 = *ZEROS  and *in24
287800991027     C                   clear                   V6CDE1
287900991027     C                   clear                   V6CVA1
288000991027     C                   clear                   V6CSV1
288100991027     C                   add       1             NR6
288200131107     C                   WRITE     LRI6S06
288300991027     c                   ENDIF
288400991027      *
288500991027      *
288600941206     C* 70 ON - EMETTO I DATI DELL'INCASSO FATTURA E INCASSO ASSEGNATO
288700941206     C     *IN33         IFEQ      '0'
288800941206     C     AR6NFT        ANDGT     0
288900941206     C     *IN22         OREQ      '1'
289000991111     C     AR6NFT        ANDGT     0
289100941206     C                   SETON                                        70
289200941206     C                   END
289300940712     C*
289400940719     C* PRELEVO LE NOTE DAL FILE APPOSITO
289500940719     C                   MOVEL     ARBAAS        KAAS
289600940719     C                   MOVEL     ARBLNP        KLNP
289700940719     C                   MOVEL     ARBNRS        KNRS
289800940719     C                   MOVEL     ARBNSP        KNSP
289900940719     C                   MOVEL     '8'           KTRC
290000060216     C     KAR41         CHAIN     FiAR401L                           31
290100940719     C  N31              MOVEL     AR4NOT        V4CNO1
290200940802     C   31              MOVEL     *BLANKS       V4CNO1
290300940719     C                   MOVEL     '9'           KTRC
290400060216     C     KAR41         CHAIN     FiAR401L                           31
290500940719     C  N31              MOVEL     AR4NOT        V4CNO2
290600940802     C   31              MOVEL     *BLANKS       V4CNO2
290700940719     C*
290800941110     C* ESTENSIONE RAGIONE SOCIALE DESTINATARIO
290900941110     C                   MOVEL     'D'           KTRC
291000060216     C     KAR41         CHAIN     FiAR401L                           31
291100950125     C  N31              MOVEL     AR4NOT        V4CNOT
291200950125     C   31              MOVEL     *BLANKS       V4CNOT
291300971015     C*
291400971015     C* SOLO PER EXPORT DATA DI STAMPA SU BORDERO' ARRIVI
291500070404     C***  *IN16         IFEQ      *ON
291600070404     C***                MOVEL     'A'           KTRC
291700070404     C***  KAR41         CHAIN     FiAR401L                           31
291800070404     C**N31              MOVE      AR4NOT        WDATA             8
291900070404     C***31              MOVEL     *ZEROS        WDATA
292000070404     C***  WDATA         IFGT      *ZEROS
292100070404     C***                MOVEL     WDATA         G02INV
292200070404     C***                Z-ADD     *ZEROS        G02DAT
292300070404     C***                MOVEL     '3'           G02ERR
292400070404     C***                CALL      'XSRDA8'
292500070404     C***                PARM                    WLBDAT
292600070404     C***                Z-ADD     G02DAT        V3CDST
292700070404     C***                ENDIF
292800070404     C***                ENDIF
292900940720     C*
293000051114     C* IL CONTRASSEGNO, SE ESISTENTE, LO TROVO SU FiAR900F
293100940720     C* CONTROLLO IL CODICE BOLLA PER SAPERE SE C'E' CONTRASSEGNO
293200940720     C                   MOVEL     *BLANKS       VIDTIC
293300940720     C                   MOVEL     *ZERO         VIDCAS
293400940720     C                   MOVEL     *BLANKS       VIDVCA
293500940720     C                   MOVEL     *BLANKS       V4CGCA
293600940923     C                   MOVEL     *BLANKS       V4IGCA
293700091214     c                   setoff                                       65
293800940720     C     §3AFCA        IFGT      *ZERO
293900051128     C     Ksped         CHAIN     FiAR901L                           31
294000940720     C  N31              DO
294100940720     C                   MOVEL     AR9TIC        VIDTIC
294200940720     C                   MOVEL     AR9CAS        VIDCAS
294300940720     C                   MOVEL     AR9VCA        VIDVCA
294400940720     C                   MOVEL     AR9GCA        V4CGCA
294500091214     c                   seton                                        65
294600940720     C                   ENDDO
294700940720     C                   ENDIF
294800941215     C* ESISTE CONTRASSEGNO: VEDO SE ABILITATO
294900941215     C     VIDCAS        IFNE      0
295000941215     C                   SETON                                        29
295100941215     C                   END
295200940923     C* SE C'E' LA PARTICOLARITA' PERMETTO LA SUA INTERROGAZIONE
295300940923     C     V4CGCA        COMP      *BLANKS                            4141
295400940720     C*
295500940720     C* RIEMPO ANCHE ALCUNI CAMPI PER LE DS DI PASSAGGIO PARAMETRI
295600940720     C                   MOVEL     V3CDSP        PA1DSP
295700940720     C                   MOVEL     ARBRMN        PA1RMN
295800940720     C                   MOVEL     ARBLNA        PA1LNA
295900940802     C                   MOVEL     V3DLNA        PA1LN1
296000940720     C                   MOVEL     ARBRSD        PA1RSD
296100160504     C                   MOVEL     V2CAAS        pa1AAS
296200160504     C                   MOVEL     COMLNP        pa1LNP
296300160504     C                   MOVEL     V2CNRS        pa1NRS
296400160504     C                   MOVEL     V2CNSP        pa1NSP
296500160505     C                   MOVEL     'FNLRU6R1'    pa1pgm
296600160505     C                   MOVEL     RSUT          PA1DSA
296700160505     C                   MOVEL     pargia        PA1gia
296800160505     C                   MOVEL     SIMFEL        PA1FEL
296900940906     C*-------------------------------------------------------------------------
297000991029      * GESTIONE INCASSO PARZIALE se assegnato o presente estensione bolla
297100991102     C                   clear                   v5cdiv
297200991102     c                   clear                   Wariimp
297300991102     c                   clear                   Wariipp
297400991102     c                   clear                   VIDimp
297500991102     c                   clear                   VIDipp
297600991102     C                   MOVEL     ARBFIP        VIDFIP
297700991102      *
297800991029     c  N33              eval      V5Cdiv = viddiv
297900991029     c   22              eval      V5Cdiv = v9cdiv
298000991029      *
298100040216if  1c                   if        V5CDIV <> *blanks
298200991029      *
298300991029     C                   eval      WDIV = V5Cdiv
298400991029     C                   exsr      CTR_VALUTA
298500991029      *
298600991029     C                   MOVEL     ARBAAS        KAAS
298700991029     C                   MOVEL     ARBLNP        KLNP
298800991029     C                   MOVEL     ARBNRS        KNRS
298900991029     C                   MOVEL     ARBNSP        KNSP
299000991029     C                   MOVEL     'A'           ktrc
299100991029      *
299200991029     c     kar61         setll     fiari000
299300991029     c     kar61         reade     fiari000
299400991029      *
299500040216do  2c                   DOW       not %EOF
299600040216if  3c                   if        v5cdiv = aridiv
299700991029     c                   add       ARIimp        Wariimp
299800991029     c                   add       ARIipp        Wariipp
299900040216x   3c                   else
300000991029      *
300100991029     C                   clear                   yeurcods
300200991029     C                   eval      YECDVI = aridiv
300300991029     C                   eval      YECDVO = V5Cdiv
300400991029     C                   eval      YECIMI = ARIimp
300500011022     C                   move      §CVDEC        YECDCO
300600991029     C                   CALL      'YEURCO'
300700991029     C                   PARM                    YEURcods
300800991029    6C                   if        YECESI = *blanks
300900991029     C                   add       YECIMO        WARIimp
301000991029    6C                   endif
301100991029      *
301200991029     C                   clear                   yeurcods
301300991029     C                   eval      YECDVI = aridiv
301400991029     C                   eval      YECDVO = V5Cdiv
301500991029     C                   eval      YECIMI = ARIipp
301600011022     C                   move      §CVDEC        YECDCO
301700991029     C                   CALL      'YEURCO'
301800991029     C                   PARM                    YEURcods
301900991029    6C                   if        YECESI = *blanks
302000991029     C                   add       YECIMO        WARIipp
302100991029    6C                   endif
302200991029      *
302300040216e   3c                   endif
302400991029      *
302500991029     c     kar61         reade     fiari000
302600040216e   2c                   enddo
302700991029      *
302800991029     c                   IF        WARIipp > *zeros
302900991029     c                   z-add     Wariipp       W0133
303000991029     c                   eval      Wtstdec = §cvfdc
303100991029     c                   exsr      EDTimporti
303200991029     c                   movel     IMPeditato    VIDipp
303300991029     c                   ENDIF
303400991029      *
303500991029     c                   IF        WARIimp > *zeros
303600991029     c                   z-add     Wariimp       W0133
303700991029     c                   eval      Wtstdec = §cvfdc
303800991029     c                   exsr      EDTimporti
303900991029     c                   movel     IMPeditato    VIDimp
304000991029     c                   ENDIF
304100991105      *
304200991105     c                   IF        VIDipp = *blanks  and  VIDimp = *blanks
304300991105     c                   clear                   V5CDIV
304400991105     c                   ENDIF
304500991029      *
304600040216e   1C                   endif
304700991029     C*-------------------------------------------------------------------------
304800951220     C* EMETTO NOTE DI CONSEGNA E NOME DEL RITIRANTE DELLA MERCE
304900951220     C*
305000951220     C* PULISCO IL SFL
305100960926     C                   SETOFF                                       72
305200131107     C                   WRITE     LRI6C05
305300960926     C                   SETON                                        7172
305400951220     C*
305500060216     C* CHIAVE PER ACCEDERE AL FILE FiAR401L
305600951220     C                   MOVEL     ARBAAS        KAAS
305700951220     C                   MOVEL     ARBLNP        KLNP
305800951220     C                   MOVEL     ARBNRS        KNRS
305900951220     C                   MOVEL     ARBNSP        KNSP
306000951220     C*
306100951220     C                   Z-ADD     0             NRR5              4 0
306200951220     C                   MOVEL     '7K'          COD
306300951220     C*
306400060216     C     KAR42         SETLL     FiAR401L
306500060216     C     KAR42         READE     FiAR401L                               36
306600040216do  1C     *IN36         DOWEQ     *OFF
306700951220     C*
306800951220     C                   MOVEL(P)  AR4TRC        KEY
306900951220     C     KTAB1         CHAIN     TABEL                              36
307000951220     C*
307100951220     C     *IN36         IFEQ      *OFF
307200951220     C     TBLFLG        ANDEQ     ' '
307300951220     C                   MOVEL     TBLUNI        DS7K
307400951220     C     §7KVIS        IFEQ      'S'
307500951220     C                   MOVEL     §7KDES        VS5DES
307600951220     C                   MOVEL     AR4NOT        VS5NOT
307700951220     C                   ADD       1             NRR5
307800131107     C                   WRITE     LRI6S05
307900951220     C                   ENDIF
308000951220     C                   ENDIF
308100951220     C*
308200060216     C     KAR42         READE     FiAR401L                               36
308300040216e   1C                   ENDDO
308400951220     C*
308500960926     C* SE NON HO CARICATO NIENTE NON EMETTO IL SFLDSP PER EVITARE CHE
308600960926     C*  IL PGM DIA ERRORE DI SESSIONE O UNITA'
308700951220     C     NRR5          IFEQ      0
308800991105     C                   clear                   VS5DES
308900991105     C                   clear                   VS5NOT
309000991105     C                   ADD       1             NRR5
309100131107     C                   WRITE     LRI6S05
309200951220     C                   ENDIF
309300951220     C*
309400021216      * Riempio il campone in fondo alla videata con i tasti scelti
309500021216     C                   exsr      Ges_TastiFun
309600021217     c                   eval      FD07 = FDxx
309700021217     c                   eval      F24D07 = F24Dxx
309800911022     C                   ENDSR
309900131114      *-------------------------------------------------------------------------
310000131114      *  Decodifica EVENTO
310100131114      *-------------------------------------------------------------------------
310200131114     c     Decodi_evb    BEGSR
310300131114
310400131114     c                   if        evbcev='  C'
310500131114     c                   eval      §2ades='CONSEGNA'
310600131128     c* aggiungo anche firmatario
310700150616     c                   if        w_ar4_1=*blanks
310800131128     c                   if        §PCTNOMFIR<>*blaNKS
310900131128     C                   EVAL      §2ADES=%TRIM(§2ADES)+' Firma:' +§PCTNOMFIR
311000131128     c                   endif
311100150616     c                   else
311200150616     C                   EVAL      §2ADES=%TRIM(§2ADES)+' Firma:' +ar4not
311300150616     c                   endif
311400131128     c
311500131114     C                   else
311600161108     c                   eval      wcev=evbcev
311700161108     c                   exsr      sr_2a
311800161108     C***                MOVEL     '2A'          COD
311900161108     C***                MOVEL(p)  evbcev        KEY
312000161108     C***  KTAB1         CHAIN     TABEL00F
312100161108    3c***                if        not %found(tabel00f)
312200161108     c***                clear                   ds2a
312300161108     c***                else
312400161108     c***                eval      ds2a=tbluni
312500161108    3c***                endif
312600131114    3c                   endif
312700131114     c
312800161108     c                   if        evbcev='  C'
312900161108     c                   eval      v7duevb=§2ades
313000161108     c                   else
313100131114     c                   eval      v7duevb=evbcev+'-'+§2ades
313200161108     c                   endif
313300131114
313400161017    3c                   if        evbdev>0
313500131114     c     *iso          movel     evbdev        dataeur
313600131114     c                   movel     dataeur       datadmy
313700131114     c                   movel     dataeur       w7cdev            8 0
313800131114
313900161017     c* metto data/ora ultimo evento solo se non si tratta di consegna perchè
314000161017     c* in questo caso tali dati sono già visualizzati nella riga "Consegnata"
314100161017     c                   if        evbcev<>'  C'
314200131114     c                   eval      indx=%checkr(' ':v7duevb)
314300131114    4c                   if        indx>46
314400131114     c                   eval      indx=46
314500131114     c                   else
314600131114     c                   eval      indx=indx+1
314700131114    4c                   endif
314800131114     c                   eval      wcarat=66-indx
314900131114     c                   eval      %subst(v7duevb:indx:wcarat)=' ' +'il ' +
315000131114     c                             %editw(W7cdev: '  /  /    ')
315100131114     c
315200131114    4c                   if        evbhev>0
315300131114     c                   eval      v7duevb=%trim(v7duevb)+' ' +
315400131114     c                                                 %editw(evbhev: '0 :  ')
315500131114    4c                   endif
315600161017     c                   endif
315700131114     c
315800131114    3c                   endif
315900160614     c                   eval      w_evbcev=evbcev
316000131114     c                   ENDSR
316100161108      *-------------------------------------------------------------------------
316200161108      *  Decoditifa causale evento
316300161108      *-------------------------------------------------------------------------
316400161108     c     Sr_2a         BegSr
316500161108     C                   MOVEL     '2A'          COD
316600161108     C                   MOVEL(p)  wcev          KEY
316700161108     C     KTAB1         CHAIN     TABEL00F
316800161108    3c                   if        not %found(tabel00f)
316900161108     c                   clear                   ds2a
317000161108     c                   else
317100161108     c                   eval      ds2a=tbluni
317200161108    3c                   endif
317300161108     c                   ENDSR
317400030703      *-------------------------------------------------------------------------
317500030703      *  Decoditifa tipo bancali
317600030703      *-------------------------------------------------------------------------
317700030703     c     Sr_DecTba     BegSr
317800030703     c                   Clear                   Tibs02Ds
317900030703     c                   Clear                   dTba
318000030703     c                   Movel     'C'           T02Mod
318100030703     c                   Movel     Knsif         T02Sif
318200030703     c                   Movel     'TBA'         T02Cod
318300030703     c                   Movel(p)  wtba          T02Ke1
318400030703     c                   Call      'TIBS02R'
318500030703     c                   Parm                    Kpjba
318600030703     c                   Parm                    Tibs02Ds
318700030703If  3c                   If        T02Err = *Blanks
318800030703     c                   Movel     T02Uni        dTba
318900030703    3c                   EndIf
319000030703     c                   Eval      wdestba = §TbaDesc
319100030703     c                   EndSr
319200021216      *-------------------------------------------------------------------------
319300021216      *  Imposto descrizione tasti funzione
319400021216      *-------------------------------------------------------------------------
319500021216     c     Ges_TastiFun  BEGSR
319600021217      *
319700021217      * per emettere i tasti senza condizionamenti ..
319800021216     c                   setoff                                       89
319900021216      *
320000021216      * Conta i caratteri riempiti dalle RigaTf1
320100021217     c                   z-add     0             $z                3 0
320200021216      *
320300021216      * Conta i caratteri riempiti dalle RigaTf2
320400021217     c                   z-add     0             $k                3 0
320500021216      *
320600021216      * Conta i caratteri riempiti dalle RigaTf3
320700021217     c                   z-add     0             $j                3 0
320800021216      *
320900021216      * Conta la posizione del campone da cui partire per impostare
321000021216      * la descrizione del tasto funzione
321100021216     c                   z-add     0             $y                2 0
321200021216      *
321300021217      * Stringhe che contengono le descrizioni dei tasti funzione
321400021216     c                   reset                   RigaTf1
321500021216     c                   reset                   RigaTf2
321600021216     c                   reset                   RigaTf3
321700070411      *
321800070411      *?F01?--> scelta bolla da assistenza clienti
321900070411     c                   IF        *in63 = *on
322000070411     c                   reset                   $Tf
322100070411     c                   movea     Tf01          $Tf
322200070411     c                   exsr      Rie_$Tf
322300070411     c                   else
322400070411      *?F01?--> scelta bolla generica
322500070411     c                   if        *in61 = *on
322600070411     c                   reset                   $Tf
322700070411     c                   movea     Tf01B         $Tf
322800070411     c                   exsr      Rie_$Tf
322900070411     c                   ENDIF
323000070411     c                   ENDIF
323100021216      *
323200070411      * F13 --> tassazione 1 bolla
323300021216     c                   IF        *in24 = *on
323400021217     c                   reset                   $Tf
323500021217     c                   movea     Tf13          $Tf
323600021217     c                   Exsr      Rie_$Tf
323700021217     C                   ENDIF
323800021216      *
323900070411      * F04 --> tassazione 2 bolla
324000021216     c                   IF        *in22 = *on
324100021217     c                   reset                   $Tf
324200021217     c                   movea     Tf04          $Tf
324300021217     c                   Exsr      Rie_$Tf
324400021217     C                   ENDIF
324500040220      *
324600070411      *?F16 --> assistenza clienti
324700040220     c                   IF        *in64 = *on
324800040220     c                   reset                   $Tf
324900040220     c                   movea     Tf16          $Tf
325000040220     c                   exsr      Rie_$Tf
325100040220     c                   ENDIF
325200160628      * F08 --> ORM
325300160628     c                   IF        *in18 = *on
325400160628     c                   reset                   $Tf
325500160628     c                   movea     Tf08          $Tf
325600160628     c                   Exsr      Rie_$Tf
325700160628     C                   ENDIF
325800021216      *
325900070411      * F05 --> eventi
326000021216     c                   IF        *in95 = *on
326100021217     c                   reset                   $Tf
326200021217     c                   movea     Tf05          $Tf
326300021217     c                   Exsr      Rie_$Tf
326400021217     C                   ENDIF
326500021216      *
326600021216      * F10
326700021217     c                   IF        *in98 = *on
326800021217     c                   reset                   $Tf
326900021217     c                   movea     Tf10          $Tf
327000021217     c                   Exsr      Rie_$Tf
327100021217     c                   ENDIF
327200021216      *
327300021216      * F14
327400021217     c                   IF        *in03 = *on
327500021217     c                   reset                   $Tf
327600021217     c                   movea     Tf14          $Tf
327700021217     c                   Exsr      Rie_$Tf
327800021217     c                   ENDIF
327900021216      *
328000021216      * F02
328100021217     c                   IF        *in04 = *on
328200021217     c                   reset                   $Tf
328300021217     c                   movea     Tf02          $Tf
328400021217     c                   Exsr      Rie_$Tf
328500021217     c                   ENDIF
328600110929      *
328700110930      * F22 --> Perfect Order
328800110929     c                   IF        *in86 = *on
328900110929     c                   reset                   $Tf
329000110929     c                   movea     Tf22          $Tf
329100110929     c                   Exsr      Rie_$Tf
329200110929     C                   ENDIF
329300021216      *
329400021216      * F11
329500021217     c                   IF        *in77 = *on
329600021217     c                   reset                   $Tf
329700021217     c                   movea     Tf11          $Tf
329800021217     c                   Exsr      Rie_$Tf
329900021217     c                   ENDIF
330000070404      *
330100070413      * F17 --> solo se partito com GEO
330200161129     c****               if        *in17 and not *in12
330300161129     c                   if        *in17
330400070404     c                   reset                   $Tf
330500070404     c                   movea     Tf17          $Tf
330600070404     c                   Exsr      Rie_$Tf
330700070413     c                   endif
330800021216      *
330900021216      * F07
331000021217     c                   IF        *in67 = *on
331100021217     c                   reset                   $Tf
331200021217     c                   movea     Tf07          $Tf
331300021217     c                   Exsr      Rie_$Tf
331400021217     c                   ENDIF
331500021216      *
331600160628      * F23= Discordanze estero
331700021217     c                   IF        *in74 = *on and *in16 = *on
331800021217     c                   reset                   $Tf
331900160628     c                   movea     Tf23          $Tf
332000021217     c                   Exsr      Rie_$Tf
332100021217     c                   ENDIF
332200130830
332300130830      * F09 --> Orari servizi non condizionato
332400131007     c                   IF        *in89 = *off and wabif9='S'
332500130930     c                   reset                   $Tf
332600130930     c                   movea     Tf09          $Tf
332700130930     c                   Exsr      Rie_$Tf
332800130930     c                   ENDIF
332900021216      *
333000021216      * F20 --> non condizionato
333100021217     c                   IF        *in89 = *off
333200021217     c                   reset                   $Tf
333300021217     c                   movea     Tf20          $Tf
333400021217     c                   Exsr      Rie_$Tf
333500021217     c                   ENDIF
333600021216      *
333700021216      * F21 --> non condizionato
333800021216     c                   IF        *in89 = *off
333900021217     c                   reset                   $Tf
334000021217     c                   movea     Tf21          $Tf
334100021217     c                   Exsr      Rie_$Tf
334200021217     c                   ENDIF
334300021218      *
334400021218      * F19 --> non condizionato
334500021218     c                   IF        *in89 = *off
334600021218     c                   reset                   $Tf
334700021218     c                   movea     Tf19          $Tf
334800021218     c                   Exsr      Rie_$Tf
334900021218     c                   ENDIF
335000070420     c
335100130830      * F06 --> solo se partito con GEO
335200070420     c                   if        *in17
335300070420     c                   reset                   $Tf
335400070420     c                   movea     Tf06          $Tf
335500070420     c                   Exsr      Rie_$Tf
335600070420     c                   ENDIF
335700021217      *
335800021217      * F12 --> non condizionato
335900021217     c                   IF        *in89 = *off
336000021217     c                   reset                   $Tf
336100021217     c                   movea     Tf12          $Tf
336200021217     c                   Exsr      Rie_$Tf
336300021217     c                   ENDIF
336400021216      *
336500021217      * Pulisco le stringa
336600021216     c                   z-add     1             $h
336700030121     c     1             do        62            $h                2 0
336800021216     C                   if        RigaTf1($h) = '#'
336900021216     c                   movel     *blanks       RigaTf1($h)
337000021216     c                   endif
337100021216     c                   enddo
337200021216     c                   z-add     1             $h
337300030121     c     1             do        62            $h                2 0
337400021216     C                   if        RigaTf2($h) = '#'
337500021216     c                   movel     *blanks       RigaTf2($h)
337600021216     c                   endif
337700021216     c                   enddo
337800021216     c                   z-add     1             $h
337900030121     c     1             do        62            $h                2 0
338000021216     C                   if        RigaTf3($h) = '#'
338100021216     c                   movel     *blanks       RigaTf3($h)
338200021216     c                   endif
338300021216     c                   enddo
338400021217      *
338500021216      * Imposto la descrizione del tasto funzione F24 e segnalo
338600021216      * quante righe ho riempito e quale devo emettere
338700021216     c                   SELECT
338800021216      *
338900021216     c                   when      $k<>*zeros and $Y<>*zeros
339000021216     c                   eval      $loop = 3
339100021216     c                   z-add     3             $loop             1 0
339200021217     C                   movel(p)  CF2413        F24Dxx           16
339300021216      *
339400021216     c                   when      $k<>*zeros and $Y=*zeros
339500021216     c                   z-add     2             $loop             1 0
339600021217     C                   movel(p)  CF2412        F24Dxx
339700021216      *
339800021216     c                   Other
339900021216     c                   z-add     1             $loop             1 0
340000021217     C                   movel     *blanks       F24Dxx
340100021216     c                   ENDSL
340200021216      * Prima riga di tasti funzione
340300030121     c                   movea     RigaTF1       FDxx             62
340400021216     c                   z-add     1             $Rig              1 0
340500021216      *
340600021216     c                   endsr
340700021217      *-------------------------------------------------------------------------
340800021217      *  Riempio le stringhe con i campi funzione compattati
340900021217      *-------------------------------------------------------------------------
341000021217     c     Rie_$Tf       begsr
341100021217      *
341200021217     c                   z-add     1             $x                2 0
341300021217     c     '#'           lookup    $Tf($x)                                31
341400021217     c                   add       $x            $Z
341500030121     c                   IF        $Z <= 63
341600021217     c                   eval      $j = $Z - $x + 1
341700021217     c                   movea     $Tf           RigaTF1($j)
341800021217     c                   ELSE
341900021217     c                   add       $x            $K
342000030121     c                   IF        $K <= 63
342100021217     c                   eval      $j = $K - $x + 1
342200021217     c                   movea     $Tf           RigaTF2($j)
342300021217     c                   ELSE
342400021217     c                   add       $x            $Y
342500030121     c                   IF        $Y <= 63
342600021217     c                   eval      $j = $Y - $x + 1
342700021217     c                   movea     $Tf           RigaTF3($j)
342800021217     c                   ENDIF
342900021217     c                   ENDIF
343000021217     c                   ENDIF
343100021217      *
343200021217     c                   endsr
343300021217      *-------------------------------------------------------------------------
343400021217      *  Gestisco l'alternarsi delle stringhe con i tasti funzione
343500021217      *-------------------------------------------------------------------------
343600021217     c     Ges_F24       begsr
343700021217      *
343800021217     c                   SELECT
343900021217      *
344000021217     c                   WHEN      $Loop = 2 and $Rig =2  or
344100021217     c                             $Loop = 3 and $Rig =3
344200021217     c                   movea     RigaTF1       FDxx
344300021217     c                   z-add     1             $Rig
344400021217      *
344500021217     c                   if        $loop = 2
344600021217     c                   eval      F24Dxx= Cf2412
344700021217     c                   else
344800021217     c                   eval      F24Dxx= Cf2413
344900021217     c                   endif
345000021217      *
345100021217     c                   WHEN      $Loop = 2 and $Rig =1 or
345200021217     c                             $Loop = 3 and $Rig =1
345300021217     c                   movea     RigaTF2       FDxx
345400021217     c                   z-add     2             $Rig
345500021217      *
345600021217     c                   if        $loop = 2
345700021217     c                   eval      F24Dxx= Cf2422
345800021217     c                   else
345900021217     c                   eval      F24Dxx= Cf2423
346000021217     c                   endif
346100021217      *
346200021217     c                   WHEN      $Loop = 3 and $Rig =2
346300021217     c                   movea     RigaTF3       FDxx
346400021217     c                   z-add     3             $Rig
346500021217     c                   eval      F24Dxx= Cf2433
346600021217     c                   ENDSL
346700021217      *
346800021217     c                   endsr
346900021217      *-------------------------------------------------------------------------
347000991027      *  CARICO SUBFILE VARIE
347100991027      *-------------------------------------------------------------------------
347200991027     c     CAR_VARIE     BEGSR
347300991128      *
347400991027      *
347500991027     c                   SELECT
347600991027      *
347700991027     c                   WHEN       WSVN = *BLANKS
347800991027      *
347900991027     C                   WHEN       WSVN = §$2iva
348000991027     c                   z-add     Wvan          W0133
348100991027     c                   eval      Wtstdec = §cvfdc
348200991027     c                   exsr      EDTimporti
348300991027     c                   movel     IMPeditato    VIDiva
348400991027      *
348500991027     c                   OTHER
348600991027     c                   z-add     Wvan          W0133
348700991027     c                   eval      Wtstdec = §cvfdc
348800991027     c                   exsr      EDTimporti
348900991027     c                   movel     IMPeditato    V6Cva1
349000991027     c                   movel     WSVN          V6Csv1
349100991027     C                   Z-ADD     1             WA
349200991027     C     V6CSV1        LOOKUP    CC1(WA)                                99
349300020329     C     CC3(WA)       ifeq      'S'
349400020329      *
349500020329      * Verifico se si tratta di una bolla Fedex
349600021119     c                   if        LnaNtw = 'FED'
349700020329     C                   Z-ADD     1             WA
349800020329     C     V6CSV1        LOOKUP    CB1(WA)                                99
349900020329     C   99              MOVEL     CB2(WA)       V6CDE1
350000020329     C  N99              clear                   V6CDE1
350100020329     C                   else
350200991027     C   99              MOVEL     CC2(WA)       V6CDE1
350300991027     C  N99              MOVEL     *BLANKS       V6CDE1
350400020329     C                   endif
350500020329     C                   else
350600020329     C   99              MOVEL     CC2(WA)       V6CDE1
350700020329     C  N99              MOVEL     *BLANKS       V6CDE1
350800020329     C                   endif
350900991027     C                   add       1             NR6
351000131107     C                   WRITE     LRI6S06
351100991027      *
351200991027     c                   ENDSL
351300991027      *
351400991027     c                   ENDSR
351500040122      *-------------------------------------------------------------------------
351600040122      *  Carico le varie della seconda bolla
351700040122      *-------------------------------------------------------------------------
351800040122     c     Car_Varie1    BegSr
351900040122
352000040122s   1c                   Select
352100040122
352200040122     c                   When      wsvn = *Blanks
352300040122      * Iva
352400040122     c                   When      wsvn = §$2iva
352500040122     c                   z-add     wvan          w0133
352600040122     c                   Eval      Wtstdec = §cvfdc
352700040122     c                   ExSr      EDTimporti
352800040122     c                   Movel     IMPeditato    V9cIva
352900070411     c                   setoff                                       06
353000070411      * Bollo
353100070411     c                   When      wsvn = §$2bol
353200070411     c                   z-add     wvan          w0133
353300070411     c                   Eval      Wtstdec = §cvfdc
353400070411     c                   ExSr      EDTimporti
353500070411     c                   Movel     IMPeditato    V9cIva
353600070411     c                   seton                                        06
353700040122      * Varia G - sempre per prima
353800040122     c                   When      wsvn = §$2vrg
353900040122     c                   Eval      V9cSv1 = wsvn
354000040122     c                   z-add     wvan          w0133
354100040122     c                   Eval      Wtstdec = §cvfdc
354200040122     c                   ExSr      EDTimporti
354300040122     c                   Movel     IMPeditato    V9cVa1
354400040122      * Altre varie
354500040122   x1c                   Other
354600040122     c                   z-add     wvan          W0133
354700040122     c                   eval      Wtstdec = §cvfdc
354800040122     c                   exsr      EDTimporti
354900040122if  2c                   If        V9cSv2 = *Blanks
355000040122     c                   movel     IMPeditato    V9cVa2
355100040122     c                   movel     wsvn          V9csv2
355200040122   x2c                   Else
355300040122if  3c                   If        V9cSv3 = *Blanks
355400040122     c                   movel     IMPeditato    V9cVa3
355500040122     c                   movel     wsvn          V9csv3
355600040122   x3c                   Else
355700040122if  4c                   If        V9cSv4 = *Blanks
355800040122     c                   movel     IMPeditato    V9cVa4
355900040122     c                   movel     wsvn          V9csv4
356000041130   x4c                   Else
356100041130if  5c                   If        V9cSv5 = *Blanks
356200041130     c                   movel     IMPeditato    V9cVa5
356300041130     c                   movel     wsvn          V9csv5
356400041130   x5c                   Else
356500041130if  6c                   If        V9cSv6 = *Blanks
356600041130     c                   movel     IMPeditato    V9cVa6
356700041130     c                   movel     wsvn          V9csv6
356800041130    6c                   EndIf
356900041130    5c                   EndIf
357000040122    4c                   EndIf
357100040122    3c                   EndIf
357200040122    2c                   EndIf
357300040122
357400040122    1c                   EndSl
357500040122
357600040122     c                   EndSr
357700991027      *****************************************************************
357800991027      *  AGGANCIO ANAGRAFICHE
357900991027      *****************************************************************
358000991027     C     CTR_ANAGRA    BEGSR
358100991027      *
358200991027     C                   clear                   DS_cnaco
358300991027     C                   clear                   DS_cnind
358400991027     C                   clear                   DS_cnclp
358500991027     C                   clear                   DS_fncls
358600991027      *
358700991027     C                   CALL      'TIBS69R'
358800991027     C                   PARM                    tibs69DS
358900991027     C                   PARM                    DS_cnaco
359000991027     C                   PARM                    DS_cnind
359100991027     C                   PARM                    DS_cnclp
359200991027     C                   PARM                    DS_fncls
359300991027      *
359400991027     C                   eval      wtibs69r = 'S'
359500991027      * per il 1° giro è inizializzata nelle specifiche "D"
359600991027     C                   clear                   TIBS69DS
359700991027      *
359800991027     C                   ENDSR
359900991027      **********************************************************************
360000991027      * AGGANCIO TABELLA VALUTE
360100991027      **********************************************************************
360200991027     C     CTR_VALUTA    BEGSR
360300991027
360400991027     C                   MOVEL     'CV'          COD
360500991027     C                   MOVEL(P)  WDIV          KEY
360600991027     C     KTAB1         CHAIN     TABEL00F                           31
360700991027     C  N31              MOVEL     TBLUNI        DSCV
360800991027
360900991027     C                   ENDSR
361000991027      **********************************************************************
361100991027      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
361200991027      **********************************************************************
361300991027     C     EDTIMPORTI    BEGSR
361400991027
361500991027      * ------------------------------------------------------------------------
361600991027      * W0133      contiene il numero da editare per il video
361700991027      * WTSTDEC    corrisponde a §CVfdc della tabella CV
361800991027      * IMPEDITATO ritorna il valore editato lungo 13, tronca cifre oltre 10 mld
361900991027      * ------------------------------------------------------------------------
362000991027      *
362100991027     c                   clear                   IMPeditato
362200991027     c                   clear                   W017A
362300991027      *
362400991027     c                   if        Wtstdec = 'S'
362500991027     C                   eval      W017a = %EDITC(W0133:'2')
362600991027     c                   else
362700991027     c                   z-add     W0133         W0130
362800991027     C                   eval      W017a = %EDITC(W0130:'2')
362900991027     c                   endif
363000991027      *
363100991027     c                   move      W017a         IMPeditato
363200991027      *
363300991027     c                   ENDSR
363400940802     C**************************************************************************
363500940802     C*    LEGAMI BOLLA
363600940802     C**************************************************************************
363700930413     C     LEGBOL        BEGSR
363800940802     C*
363900940912     C* DEVO MEMORIZZARE LA BOLLA POICHE DEVO RIAGGANCIARLA DOPO
364000940912     C                   MOVEL     ARBAAS        WAAS
364100940912     C                   MOVEL     ARBLNP        WLNP
364200940912     C                   MOVEL     ARBNRS        WNRS
364300940912     C                   MOVEL     ARBNSP        WNSP
364400940912     C*
364500940907     C* PULISCO IL SFL
364600950825     C                   SETOFF                                       787923
364700131107     C                   WRITE     LRI6C08
364800950825     C                   SETON                                        7923
364900940923     C                   MOVEL     *BLANKS       LEGAME
365000940907     C*
365100940907     C                   SETOFF                                       94
365200950718     C                   Z-ADD     *ZERO         NR8
365300940907     C                   Z-ADD     *ZERO         SAVNR8            4 0
365400940907     C                   MOVEL     *BLANKS       WFLG1             1
365500940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
365600940907     C*
365700940907     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
365800940907     C     KARB          CHAIN     FNLBL01L                           99
365900940907     C   99KARB          CHAIN     FNLBL02L                           99
366000940907     C  N99              DO
366100940907     C*
366200940907     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
366300940907     C                   MOVEL     LBLAAO        KAAS
366400940907     C                   MOVEL     LBLLPO        KLNP
366500940907     C                   MOVEL     LBLNRO        KNRS
366600940907     C                   MOVEL     LBLNSO        KNSP
366700940907     C     KLBL3         CHAIN     FNLBL02L                           99
366800950103     C     *IN99         IFEQ      *ON
366900950103     C                   MOVEL     'N'           WORIGI            1
367000950103     C                   ELSE
367100950103     C                   MOVEL     'S'           WORIGI
367200950103     C                   ENDIF
367300940907     C*
367400940907     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
367500940907     C* E TESTO SE E' PRESENTE IN ARCHIVIO
367600950102     C* SIA CHE TROVI L'ORIGINALE, SIA CHE NON LA TROVI, DEVO
367700950102     C* INSERIRLA AL PRIMO POSTO DEL SFL
367800940907     C                   MOVEL     ' '           V8CSCE
367900940907     C                   MOVEL     LBLAAO        V8CAAP
368000940907     C                   MOVEL     LBLLPO        V8CLPP
368100940907     C                   MOVEL     LBLLAO        V8CLAP
368200940907     C                   MOVEL     LBLNRO        V8CNRP
368300940907     C                   MOVEL     LBLNSO        V8CNSP
368400940926     C                   CLEAR                   V8CAAN
368500940926     C                   CLEAR                   V8CLPN
368600940926     C                   CLEAR                   V8CLAN
368700940926     C                   CLEAR                   V8CNRN
368800940926     C                   CLEAR                   V8CNSN
368900940907     C*
369000940907     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
369100940907     C     LBLAAO        IFEQ      V3CAAS
369200940907     C     LBLLPO        ANDEQ     V3CLNP
369300940907     C     LBLNRO        ANDEQ     V3CNRS
369400940907     C     LBLNSO        ANDEQ     V3CNSP
369500940907     C                   MOVEL     *BLANKS       V8CPRE
369600940907     C                   SETON                                        80
369700940907     C                   SETON                                        81
369800940907     C                   MOVEL     '1'           WFLG1
369900940907     C                   ELSE
370000940907     C                   SETOFF                                       81
370100940907     C*
370200950316     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
370300950316     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
370400950316     C                   SETOFF                                       93
370500950316     C     KLBL3         SETLL     FNARB01L                               93
370600950316     C  N93              MOVEA     PRD(1)        V8CPRE
370700950316     C  N93              SETON                                        80
370800150423     C** 93              MOVEA     PRD(2)        V8CPRE
370900150423     C** 93              SETOFF                                       80
371000950316     C*
371100950316     C* SE NON TROVATA SU ARB PROVO SU BLP
371200950316     C  N93              DO
371300950316     C                   SETOFF                                       93
371400950316     C     KLBL3         SETLL     FNBLP01L                               93
371500010712      *
371600010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
371700010712     c                   if        *in93 = *off
371800010712     C                   MOVEA     PRD(1)        V8CPRE
371900010712     c                   else
372000150423     C                   MOVEA     PRD(2)        V8CPRE
372100150423     C                   SETOFF                                       80
372200010712     C                   ENDIF
372300950316     C*
372400950316     C                   ENDDO
372500940907     C*
372600940907     C                   ENDIF
372700940907     C                   SETON                                        83
372800950103     C*
372900950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
373000950103     C                   MOVEL     *IN80         V8CI80
373100950103     C                   MOVEL     *IN81         V8CI81
373200950103     C                   MOVEL     *IN82         V8CI82
373300950103     C                   MOVEL     *IN83         V8CI83
373400950103     C*
373500940907     C                   Z-ADD     1             NR8
373600131107     C                   WRITE     LRI6S08
373700950825     C                   SETON                                        78
373800950103     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
373900940907     C*
374000950103     C* HO DUE POSSIBILITA': 1) HO TROVATO L'ORIGINALE
374100950103     C*                      2) NON HO TROVATO L'ORIGINALE
374200950103     C* SE L'HO TROVATA POSSO PROSEGUIRE, ALTRIMENTI DEVO FARE UN
374300950103     C* BALOTTINO: IMPOSTO LA 2° RIGA DI SFL (NELLA 1° C'E' GIA'
374400950103     C* L'ORIGINALE) CON LA BOLLA CHE FIGURA COME MAMMA POSIZIONATA
374500950103     C* A DESTRA (CIOE' COME SE FOSSE "SEGUENTE"), LASCIANDO VUOTI
374600950103     C* I CAMPI SULLA SINISTRA (CAMPI DENOMINATI "PRECEDENTE");
374700950103     C* TUTTO QUESTO PERCHE' MANCANO I LEGAMI CHE STANNO TRA
374800950103     C* L'ORIGINALE E LA MAMMA.
374900950103     C     WORIGI        IFEQ      'N'
375000950103     C                   MOVEL     *BLANKS       V8CSCE
375100950103     C                   CLEAR                   V8CAAP
375200950103     C                   CLEAR                   V8CLPP
375300950103     C                   CLEAR                   V8CLAP
375400950103     C                   CLEAR                   V8CNRP
375500950103     C                   CLEAR                   V8CNSP
375600950103     C                   MOVEL     LBLAAP        V8CAAN
375700950103     C                   MOVEL     LBLLPP        V8CLPN
375800950103     C                   MOVEL     LBLLAP        V8CLAN
375900950103     C                   MOVEL     LBLNRP        V8CNRN
376000950103     C                   MOVEL     LBLNSP        V8CNSN
376100950103     C                   SETOFF                                       8182
376200950103     C*
376300950103     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
376400950103     C     V8CAAN        IFEQ      V3CAAS
376500950103     C     V8CLPN        ANDEQ     V3CLNP
376600950103     C     V8CNRN        ANDEQ     V3CNRS
376700950103     C     V8CNSN        ANDEQ     V3CNSP
376800950103     C                   MOVEL     *BLANKS       V8CPRE
376900950103     C                   SETON                                        80
377000950103     C                   SETON                                        82
377100950103     C                   ELSE
377200950103     C*
377300950103     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
377400950103     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
377500950316     C                   EXSR      CHKBOL
377600950103     C*
377700950103     C                   ENDIF
377800950103     C*
377900950103     C* SCRIVO IL RECORD "TRUCCATO" NEL SFL
378000950103     C                   SETOFF                                       83
378100950103     C*
378200950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
378300950103     C                   MOVEL     *IN80         V8CI80
378400950103     C                   MOVEL     *IN81         V8CI81
378500950103     C                   MOVEL     *IN82         V8CI82
378600950103     C                   MOVEL     *IN83         V8CI83
378700950103     C*
378800950103     C                   ADD       1             NR8
378900131107     C                   WRITE     LRI6S08
379000950103     C*
379100950103     C                   ENDIF
379200940907     C                   SETOFF                                       83  99
379300940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
379400940907     C* CICLO FINO ALL'ULTIMO LEGAME
379500940907     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
379600940907     C                   Z-ADD     *ZERO         WA                3 0
379700940907     C                   Z-ADD     *ZERO         WB                3 0
379800940907     C                   MOVEA     *ZERO         AAN
379900940907     C     *IN99         DOWEQ     *OFF
380000940907     C*
380100940907     C                   MOVEL     ' '           V8CSCE
380200940907     C                   MOVEL     LBLAAP        V8CAAP
380300940907     C                   MOVEL     LBLLPP        V8CLPP
380400940907     C                   MOVEL     LBLLAP        V8CLAP
380500940907     C                   MOVEL     LBLNRP        V8CNRP
380600940907     C                   MOVEL     LBLNSP        V8CNSP
380700940907     C                   MOVEL     LBLAAN        V8CAAN
380800940907     C                   MOVEL     LBLLPN        V8CLPN
380900940907     C                   MOVEL     LBLLAN        V8CLAN
381000940907     C                   MOVEL     LBLNRN        V8CNRN
381100940907     C                   MOVEL     LBLNSN        V8CNSN
381200940907     C                   SETOFF                                       8182
381300940907     C*
381400950103     C* VERIFICO SE LA PRECEDENTE E' QUELLA IN ESAME DALL'UTENTE
381500940907     C     LBLAAP        IFEQ      V3CAAS
381600940907     C     LBLLPP        ANDEQ     V3CLNP
381700940907     C     LBLNRP        ANDEQ     V3CNRS
381800940907     C     LBLNSP        ANDEQ     V3CNSP
381900940907     C                   SETON                                        81
382000940907     C                   ENDIF
382100940907     C*
382200940907     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
382300940907     C     LBLAAN        IFEQ      V3CAAS
382400940907     C     LBLLPN        ANDEQ     V3CLNP
382500940907     C     LBLNRN        ANDEQ     V3CNRS
382600940907     C     LBLNSN        ANDEQ     V3CNSP
382700940907     C                   MOVEL     *BLANKS       V8CPRE
382800940907     C                   SETON                                        80
382900940907     C                   SETON                                        82
383000940907     C                   ELSE
383100940907     C*
383200940907     C* PER ABILITARE O NO IL CAMPO SCELTA DEVO VERIFICARE
383300940907     C* L'ESISTENZA DELLA BOLLA PRIMA IN ARB POI IN BLP
383400950316     C                   EXSR      CHKBOL
383500940907     C*
383600940907     C                   ENDIF
383700950103     C*
383800950103     C* MEMORIZZO GLI INDICATORI NEL RECORD DEL SFL
383900950103     C                   MOVEL     *IN80         V8CI80
384000950103     C                   MOVEL     *IN81         V8CI81
384100950103     C                   MOVEL     *IN82         V8CI82
384200950103     C                   MOVEL     *IN83         V8CI83
384300940907     C*
384400940907     C* SCRIVO IL RECORD NEL SFL
384500940907     C                   ADD       1             NR8
384600131107     C                   WRITE     LRI6S08
384700940907     C*
384800940907     C* AGGIORNO LA SCHIERA CON LE FIGLIE
384900940907     C                   ADD       1             WA
385000940907     C                   MOVEL     LBLAAN        AAN(WA)
385100940907     C                   MOVEL     LBLLPN        LPN(WA)
385200940907     C                   MOVEL     LBLNRN        NRN(WA)
385300940907     C                   MOVEL     LBLNSN        NSN(WA)
385400940907     C*
385500940907     C     KLBL3         READE     FNLBL02L                               99
385600940907     C*
385700940907     C* SE NON HO PIU' BOLLE CON READE
385800940907     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
385900970116     C     WB            DOWLT     WA
386000970116     C     *IN99         ANDEQ     *ON
386100940907     C                   ADD       1             WB
386200940907     C                   MOVEL     AAN(WB)       KAAS
386300940907     C                   MOVEL     LPN(WB)       KLNP
386400940907     C                   MOVEL     NRN(WB)       KNRS
386500940907     C                   MOVEL     NSN(WB)       KNSP
386600940907     C     KLBL3         SETLL     FNLBL02L
386700940907     C     KLBL3         READE     FNLBL02L                               99
386800970116     C                   ENDDO
386900940907     C*
387000940907     C                   ENDDO
387100940907     C                   ENDDO
387200940907     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
387300940907     C*
387400940907     C* SE HO TROVATO UN UNICO LEGAME NON VISUALIZZERO' IL SFL
387500940907     C* MA EMETTO IL CAMPO "LEGAME" IN TUTTE LE VIDEATE BOLLA
387600940907     C     NR8           IFEQ      2
387700940907     C*
387800940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
387900131107     C     2             CHAIN     LRI6S08                            99
388000940907     C     V8CAAP        IFEQ      V3CAAS
388100940907     C     V8CLPP        ANDEQ     V3CLNP
388200940907     C     V8CNRP        ANDEQ     V3CNRS
388300940907     C     V8CNSP        ANDEQ     V3CNSP
388400940907     C                   MOVEL     V8CAAN        COMAAN
388500940907     C                   MOVEL     V8CLPN        COMLPN
388600940907     C                   MOVEL     V8CNRN        COMNRN
388700940907     C                   MOVEL     V8CNSN        COMNSN
388800950307     C                   MOVEL     V8CLAN        COMLAN
388900950316     C                   MOVEA     'SEGUEN.'     LBL(14)
389000940907     C                   ELSE
389100940907     C                   MOVEL     V8CAAP        COMAAN
389200940907     C                   MOVEL     V8CLPP        COMLPN
389300940907     C                   MOVEL     V8CNRP        COMNRN
389400940907     C                   MOVEL     V8CNSP        COMNSN
389500950307     C                   MOVEL     V8CLAP        COMLAN
389600950316     C                   MOVEA     'PRECED.'     LBL(14)
389700940926     C* SE E' LA PRECEDENTE (NEL SFL HO SOLO 2 RIGHE) DEVO USARE
389800940926     C* LA "PRESENZA O MENO" DELLA PRIMA RIGA DEL SFL, POICHE'
389900940926     C* NELLA SECONDA RIGA LA DESCRIZIONE E' VUOTA
390000131107     C     1             CHAIN     LRI6S08                            99
390100940926     C* REIMPOSTO NR8=2 PERCHE' MI SERVE DOPO
390200940926     C                   Z-ADD     2             NR8
390300940907     C                   ENDIF
390400940907     C*
390500950316     C                   MOVEA     COMLPN        LBL(21)
390600950316     C                   MOVEA     COMNRN        LBL(25)
390700950316     C                   MOVEA     COMNSN        LBL(28)
390800950316     C                   MOVEA     COMLAN        LBL(40)
390900940907     C*
391000940907     C* VERIFICO SE IN ARCHIVIO HO ANCORA QUESTA BOLLA
391100940907     C                   Z-ADD     1             W                 1 0
391200940907     C     V8CPRE        LOOKUP    PRD(W)                                 99
391300950307     C                   MOVEA     LBD(W)        LBL(44)
391400940907     C     W             COMP      2                                      94
391500940907     C*
391600940907     C                   MOVEA     LBL           LEGAME
391700940907     C                   ELSE
391800940907     C*
391900940907     C     NR8           IFGT      2
392000940907     C                   MOVEA     LB2           LEGAME
392100940907     C                   SETON                                        94
392200940907     C                   ENDIF
392300940907     C                   ENDIF
392400940907     C*
392500940907     C     NR8           COMP      1                                    3434
392600940907     C*
392700940907     C                   Z-ADD     NR8           SAVNR8
392800940907     C                   Z-ADD     1             NR8
392900940923     C     LEGAME        COMP      *BLANKS                            9292
393000940912     C*
393100940912     C* RIAGGANCIO LA BOLLA IN ESAME
393200940912     C                   MOVEL     WAAS          ARBAAS
393300940912     C                   MOVEL     WLNP          ARBLNP
393400940912     C                   MOVEL     WNRS          ARBNRS
393500940912     C                   MOVEL     WNSP          ARBNSP
393600150423     c     karb2         chain     fnblp01l
393700150423     C     KARB2         CHAIN     FNARB01L
393800930511     C*
393900930413     C                   ENDSR
394000950316     C**************************************************************************
394100950316     C*  VERIFICA L'ESISTENZA DELLA BOLLA IN ARRIVO/PARTENZA
394200950316     C**************************************************************************
394300950316     C     CHKBOL        BEGSR
394400950316     C*
394500950316     C                   SETOFF                                       93
394600950316     C     KLBL1         SETLL     FNARB01L                               93
394700950316     C  N93              MOVEA     PRD(1)        V8CPRE
394800950316     C  N93              SETON                                        80
394900950316     C   93              MOVEA     PRD(2)        V8CPRE
395000950316     C   93              SETOFF                                       80
395100950316     C*
395200950316     C* SE NON TROVATA SU ARB PROVO SU BLP
395300950316     C  N93              DO
395400950316     C                   SETOFF                                       93
395500950316     C     KLBL1         SETLL     FNBLP01L                               93
395600010712      *
395700010712      * Se non trovata (if inserita dopo eliminazione FNSTB)
395800010712     c                   if        *in93 = *off
395900010712     C                   MOVEA     PRD(1)        V8CPRE
396000010712     c                   else
396100950316     C                   MOVEA     PRD(2)        V8CPRE
396200950316     C                   SETOFF                                       80
396300010712     C                   ENDIF
396400950316     C*
396500950316     C                   ENDDO
396600950316     C*
396700950316     C                   ENDSR
396800940802     C**************************************************************************
396900940802     C*    VISUALIZZA BOLLA
397000940802     C**************************************************************************
397100911023     C     VISUAL        BEGSR
397200940321      *
397300940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
397400131108     c     Emriepil      tag
397500940712     C                   EXSR      RIEPIL
397600940712      *
397700131108      * PER F15 PASSO ALLA BOLLA, PER INVIO o F12 RITORNO AL SFL
397800940712     C  NKP              GOTO      ENDVIS
397900931112      *
398000931112     C                   SETOFF                                       89
398100940802     C                   MOVEL     'S'           WRIEM             1
398200021217      *
398300021217      * Riempio il campone in fondo alla videata con i tasti scelti
398400021217     C                   exsr      Ges_TastiFun
398500021217     c                   eval      FD03 = FDxx
398600021217     c                   eval      F24D03 = F24Dxx
398700911023     C     FOR03         TAG
398800940802     C     WRIEM         IFEQ      'S'
398900131107     C                   WRITE     LRI6T01
399000940802     C                   MOVEL     *BLANKS       WRIEM
399100940802     C                   ENDIF
399200021217      *
399300021217      * Imposto la stringa comandi del 'giro' successivo
399400021217     C                   IF        *inKY = *on and $Loop > 1
399500021217     c                   Exsr      Ges_F24
399600021217     c                   eval      FD03 = FDxx
399700021217     c                   eval      F24D03 = F24Dxx
399800021217     C                   ENDIF
399900021217      *
400000131107     C                   EXFMT     LRI6D03
400100070420     c                   setoff                                       28
400200070420     c                   clear                   v1cmsg
400300931112      *
400400131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
400500931112     C   KY*IN89         IFEQ      *ON
400600931112     C                   SETOFF                                       89
400700931112     C                   ELSE
400800931112     C                   SETON                                        89
400900931112     C                   ENDIF
401000931112     C   KY              GOTO      FOR03
401100911023     C*
401200131108     C* F3 - FINE LAVORO TORNO A SUBFILE
401300940714     C   KC              MOVEL     '2'           V2CFLG            1
401400911023     C   KC              GOTO      ENDVIS
401500911023     C*
401600131108     C* F12 - FINE LAVORO tolno a videata di riepilogo
401700131108     C   KL              GOTO      emriepil
401800911023     C*
401900911024     C*
402000930517     C* CMD18 - INTERROGA LEGAME BOLLA
402100930518     C   KS              DO
402200930518     C                   EXSR      CMD18
402300940907     C                   MOVEL     'S'           WRIEM
402400930518     C                   GOTO      FOR03
402500930518     C                   END
402600930517     C*
402700940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
402800920804     C                   EXSR      COMAND
402900940905     C     WCMD          IFEQ      'S'
403000940802     C     WF03          IFEQ      *BLANKS
403100070403     c
403200920804     C                   GOTO      FOR03
403300940802     C                   ELSE
403400940802     C                   MOVEL     '2'           V2CFLG
403500940802     C                   GOTO      ENDVIS
403600940802     C                   ENDIF
403700940905     C                   ENDIF
403800911023     C*
403900911023     C* FORMATO4
404000931112     C                   SETOFF                                       89
404100940802     C                   MOVEL     'S'           WRIEM
404200021217      *
404300021217      * Riempio il campone in fondo alla videata con i tasti scelti
404400021217     C                   exsr      Ges_TastiFun
404500021217     c                   eval      FD04 = FDxx
404600021217     c                   eval      F24D04 = F24Dxx
404700911023     C     FOR04         TAG
404800940802     C     WRIEM         IFEQ      'S'
404900131107     C                   WRITE     LRI6T01
405000940802     C                   MOVEL     *BLANKS       WRIEM
405100940802     C                   ENDIF
405200021217      * Imposto la stringa comandi del 'giro' successivo
405300021217     C                   IF        *inKY = *on and $Loop > 1
405400021217     c                   Exsr      Ges_F24
405500021217     c                   eval      FD04 = FDxx
405600021217     c                   eval      F24D04 = F24Dxx
405700021217     C                   ENDIF
405800131107     C                   EXFMT     LRI6D04
405900070420     c                   setoff                                       28
406000070420     c                   clear                   v1cmsg
406100931112      *
406200131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
406300931112     C   KY*IN89         IFEQ      *ON
406400931112     C                   SETOFF                                       89
406500931112     C                   ELSE
406600931112     C                   SETON                                        89
406700931112     C                   ENDIF
406800931112     C   KY              GOTO      FOR04
406900911023     C*
407000131108     C* F12 - RITORNO VIDEATA 3
407100911023     C   KL              GOTO      FOR03
407200920804     C*
407300131108     C* F18 - INTERROGA LEGAME BOLLA
407400930518     C   KS              DO
407500930518     C                   EXSR      CMD18
407600930518     C                   GOTO      FOR04
407700930518     C                   END
407800940923     C*
407900940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
408000131120     c                   eval      wvideo='4'
408100131120     C                   EXSR      CTR_puIn
408200940923     C     WRIEM         CABEQ     'S'           FOR04
408300930517     C*
408400940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
408500920804     C                   EXSR      COMAND
408600940905     C     WCMD          IFEQ      'S'
408700940802     C     WF03          IFEQ      *BLANKS
408800940802     C                   GOTO      FOR04
408900940802     C                   ELSE
409000940802     C                   MOVEL     '2'           V2CFLG
409100940802     C                   GOTO      ENDVIS
409200940802     C                   ENDIF
409300940905     C                   ENDIF
409400911023     C*
409500911023     C* VIDEATA 5
409600931112     C                   SETOFF                                       89
409700940809     C                   MOVEL     'S'           WRIEM
409800021217      *
409900021217      * Riempio il campone in fondo alla videata con i tasti scelti
410000021217     C                   exsr      Ges_TastiFun
410100021217     c                   eval      FD05 = FDxx
410200021217     c                   eval      F24D05 = F24Dxx
410300911023     C     FOR05         TAG
410400940809     C     WRIEM         IFEQ      'S'
410500131107     C                   WRITE     LRI6T01
410600940809     C                   MOVEL     *BLANKS       WRIEM
410700940809     C                   ENDIF
410800951220     C                   Z-ADD     1             REC5
410900021217      *
411000021217      * Imposto la stringa comandi del 'giro' successivo
411100021217     C                   IF        *inKY = *on and $Loop > 1
411200021217     c                   Exsr      Ges_F24
411300021217     c                   eval      FD05 = FDxx
411400021217     c                   eval      F24D05 = F24Dxx
411500021217     C                   ENDIF
411600131107     C                   WRITE     LRI6Z05
411700131107     C                   EXFMT     LRI6C05
411800070420     c                   setoff                                       28
411900070420     c                   clear                   v1cmsg
412000931112      *
412100131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
412200931112     C   KY*IN89         IFEQ      *ON
412300931112     C                   SETOFF                                       89
412400931112     C                   ELSE
412500931112     C                   SETON                                        89
412600931112     C                   ENDIF
412700931112     C   KY              GOTO      FOR05
412800911023     C*
412900131108     C* F12 - RITORNO VIDEATA 2
413000911023     C   KL              GOTO      FOR04
413100920804     C*
413200940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
413300920804     C                   EXSR      COMAND
413400940905     C     WCMD          IFEQ      'S'
413500940802     C     WF03          IFEQ      *BLANKS
413600940802     C                   GOTO      FOR05
413700940802     C                   ELSE
413800131108     C                   MOVEL     '2'           V2CFLG
413900131108     C                   GOTO      ENDVIS
414000940802     C                   ENDIF
414100070406
414200070406     c                   else
414300070406     c
414400070406     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
414500070406     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
414600070406     c                   if        v3cgesd='?'
414700070406     c                   exsr      VISUALAbi
414800070406     c                   clear                   v3cgesd
414900070406     c                   eval      wriem='S'
415000070406     C                   GOTO      FOR05
415100070406     c                   endif
415200940905     C                   ENDIF
415300131108     c
415400131108     C                   GOTO      emriepil
415500931112      *
415600911023     C     ENDVIS        ENDSR
415700070403     C**************************************************************************
415800070403     C*    VISUALIZZA Abilitazioni
415900070403     C**************************************************************************
416000070403     C     VISUALAbi     BEGSR
416100070403     c*
416200131120     c                   clear                   fnlrg7ds
416300131120     c                   move      v3cdsp        ilrg7aas
416400131120     c                   eval      ilrg7lnp=v3clnp
416500131120     c                   eval      ilrg7nrs=v3cnrs
416600131120     c                   eval      ilrg7nsp=v3cnsp
416700131120     c                   eval      ilrg7vsdet='?'
416800131120     c                   eval      ilrg7lung=58
416900131120     c                   call      'FNLRG7R'
417000131120     c                   parm                    fnlrg7ds
417100070403     c
417200070403     C                   ENDSR
417300940712     C**************************************************************************
417400940802     C*    VISUALIZZA BOLLA PRECEDENTE
417500940802     C**************************************************************************
417600930510     C     VISUA2        BEGSR
417700940321      *
417800940712      * VISUALIZZO IL FORMATO DI RIEPILOGO DELLA BOLLA
417900131108     c     Emriepi2      tag
418000940712     C                   EXSR      RIEPI2
418100940712      *
418200131108      * PER F15 PASSO ALLA BOLLA, PER INVIO RITORNO AL SFL
418300940713     C  NKP              GOTO      ENDV8C
418400931112      *
418500931112     C                   SETOFF                                       89
418600940809     C                   MOVEL     'S'           WRIEM
418700021217      *
418800021217      * Riempio il campone in fondo alla videata con i tasti scelti
418900021217     C                   exsr      Ges_TastiFun
419000070516     c                   eval      FD03 = FDxx
419100070516     c                   eval      F24D03 = F24Dxx
419200930510     C     FOR033        TAG
419300940809     C     WRIEM         IFEQ      'S'
419400131107     C                   WRITE     LRI6T01
419500940809     C                   MOVEL     *BLANKS       WRIEM
419600940809     C                   ENDIF
419700021217      *
419800021217      * Imposto la stringa comandi del 'giro' successivo
419900021217     C                   IF        *inKY = *on and $Loop > 1
420000021217     c                   Exsr      Ges_F24
420100021217     c                   eval      FD03 = FDxx
420200021217     c                   eval      F24D03 = F24Dxx
420300021217     C                   ENDIF
420400131107     C                   EXFMT     LRI6D03
420500070420     c                   setoff                                       28
420600070420     c                   clear                   v1cmsg
420700931112      *
420800131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
420900931112     C   KY*IN89         IFEQ      *ON
421000931112     C                   SETOFF                                       89
421100931112     C                   ELSE
421200931112     C                   SETON                                        89
421300931112     C                   ENDIF
421400931112     C   KY              GOTO      FOR033
421500930510     C*
421600131108     C* F12 - FINE LAVORO CAMBIO BOLLA SE CE NE SONO
421700930510     C   KC
421800131108     COR KL              GOTO      Emriepi2
421900930510     C*
422000940905     C* CONTROLLO SE PREMUTI TASTI FUNZIONALI E, NEL CASO, RIEMETTO
422100930510     C                   EXSR      COMAND
422200940905     C     WCMD          IFEQ      'S'
422300930510     C                   GOTO      FOR033
422400930510     C                   END
422500931112      *
422600930510     C* FORMATO4
422700931112     C                   SETOFF                                       89
422800940809     C                   MOVEL     'S'           WRIEM
422900021217      *
423000021217      * Riempio il campone in fondo alla videata con i tasti scelti
423100021217     C                   exsr      Ges_TastiFun
423200021217     c                   eval      FD04 = FDxx
423300021217     c                   eval      F24D04 = F24Dxx
423400930510     C     FOR044        TAG
423500940809     C     WRIEM         IFEQ      'S'
423600131107     C                   WRITE     LRI6T01
423700940809     C                   MOVEL     *BLANKS       WRIEM
423800940809     C                   ENDIF
423900021217      *
424000021217      * Imposto la stringa comandi del 'giro' successivo
424100021217     C                   IF        *inKY = *on and $Loop > 1
424200021217     c                   Exsr      Ges_F24
424300021217     c                   eval      FD04 = FDxx
424400021217     c                   eval      F24D04 = F24Dxx
424500021217     C                   ENDIF
424600131107     C                   EXFMT     LRI6D04
424700070420     c                   setoff                                       28
424800070420     c                   clear                   v1cmsg
424900931112      *
425000931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
425100931112     C   KY*IN89         IFEQ      *ON
425200931112     C                   SETOFF                                       89
425300931112     C                   ELSE
425400931112     C                   SETON                                        89
425500931112     C                   ENDIF
425600931112     C   KY              GOTO      FOR044
425700930510     C*
425800930510     C* CMD12 - RITORNO VIDEATA 3
425900930510     C   KL              GOTO      FOR033
426000940923     C*
426100940923     C* ESEGUO I CONTROLLI SULLA VIDEATA ( '?' )
426200131120     c                   eval      wvideo='4'
426300131120     C                   EXSR      CTR_puIn
426400940923     C     WRIEM         CABEQ     'S'           FOR044
426500930510     C*
426600940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
426700930510     C                   EXSR      COMAND
426800940905     C     WCMD          IFEQ      'S'
426900930510     C                   GOTO      FOR044
427000940905     C                   ENDIF
427100931112      *
427200930510     C* VIDEATA 5
427300931112     C                   SETOFF                                       89
427400940809     C                   MOVEL     'S'           WRIEM
427500021217      *
427600021217      * Riempio il campone in fondo alla videata con i tasti scelti
427700021217     C                   exsr      Ges_TastiFun
427800021217     c                   eval      FD05 = FDxx
427900021217     c                   eval      F24D05 = F24Dxx
428000930510     C     FOR055        TAG
428100940809     C     WRIEM         IFEQ      'S'
428200131107     C                   WRITE     LRI6T01
428300940809     C                   MOVEL     *BLANKS       WRIEM
428400940809     C                   ENDIF
428500951220     C                   Z-ADD     1             REC5
428600021217      *
428700021217      * Imposto la stringa comandi del 'giro' successivo
428800021217     C                   IF        *inKY = *on and $Loop > 1
428900021217     c                   Exsr      Ges_F24
429000021217     c                   eval      FD05 = FDxx
429100021217     c                   eval      F24D05 = F24Dxx
429200021217     C                   ENDIF
429300131107     C                   WRITE     LRI6Z05
429400131107     C                   EXFMT     LRI6C05
429500070420     c                   setoff                                       28
429600070420     c                   clear                   v1cmsg
429700931112      *
429800931112      * CMD 24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
429900931112     C   KY*IN89         IFEQ      *ON
430000931112     C                   SETOFF                                       89
430100931112     C                   ELSE
430200931112     C                   SETON                                        89
430300931112     C                   ENDIF
430400931112     C   KY              GOTO      FOR055
430500930510     C*
430600930510     C* CMD12 - RITORNO VIDEATA 2
430700930510     C   KL              GOTO      FOR044
430800930510     C*
430900940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
431000940905     C                   EXSR      COMAND
431100940905     C     WCMD          IFEQ      'S'
431200930510     C                   GOTO      FOR055
431300930510     C                   END
431400131108
431500131108     C                   GOTO      Emriepi2
431600931112      *
431700940713     C     ENDV8C        ENDSR
431800940802     C**************************************************************************
431900940802     C* TASTI DI FUNZIONE
432000940802     C**************************************************************************
432100920804     C     COMAND        BEGSR
432200940905     C*
432300940905     C* PULISCO IL FLAG CHE INDICA SE PREMUTO UN TASTO FUNZIONE
432400940905     C                   MOVE      *BLANKS       WCMD              1
432500040218      *
432600070411      *?F1 - Aggiunta bolla in reclamo?o scelta bolla generica
432700040218if  1c                   if        *inKA   = *on
432800070423     C                   Z-ADD     arbLNP        COMLNP
432900070411     C                   MOVEL     '3'           V2CFLG
433000070411      *?-
433100150402if  2c                   if        SDSprm  > 1 and ilru6ra='S'
433200131111     c                   eval      wARBlnp = ARBlnp
433300131111     c                   eval      wARBnrs = ARBnrs
433400131111     c                   eval      wARBnsp = ARBnsp
433500131111     c                   eval      wARBaas = ARBaas
433600150402     c*??????????        clear                   FIDNA1ds
433700150402     c                   eval      ELRU6INS  = 'S'
433800150402     c                   eval      OLRU6TOR  = 'S'
433900150402     c                   eval      OLRU6OGG  = ds_OggSped
434000070411    2c                   endif
434100040218     c                   goto      Fine
434200040218e   1c                   endif
434300940802     C*
434400990119     C* F5 - INTERROGAZIONE EVENTI
434500040218do  1C   KE              DO
434600940905     C                   MOVEL     'S'           WCMD
434700940726     C                   MOVEL     V2CAAS        PB5AAS
434800940726     C                   MOVEL     COMLNP        PB5LNP
434900940726     C                   MOVEL     V2CNRS        PB5NRS
435000940726     C                   MOVEL     V2CNSP        PB5NSP
435100940726     C                   MOVEL     *BLANKS       PB5F03
435200940726     C                   MOVEL     *BLANKS       PB5FLG
435300940802     C                   MOVEL     *BLANKS       PB5PGM
435400131107     C                   MOVEL     'FNLRI6R'     PB5PGM
435500940726     C                   MOVEL     V1CDSA        PB5DSA
435600940726     C                   MOVEL     V3CDSP        PB5DSP
435700940726     C                   MOVEL     VIDRMN        PB5RMN
435800940726     C                   MOVEL     V3CLNA        PB5LNA
435900940802     C                   MOVEL     V3DLNA        PB5LN1
436000940726     C                   MOVEL     V4CRSD        PB5RSD
436100940726     C                   MOVEL     VIDDCP        PB5DCP
436200941031     C                   MOVEL     VIDTMC        PB5TMC
436300940726     C                   MOVEL     PARAB5        KPJBU
436400940722     C                   CALL      'FNLRB5R'
436500930525     C                   PARM                    KPJBA
436600941109     C                   MOVEL     KPJBU         PARAB5
436700941109     C                   MOVEL     PB5F03        WF03
436800940725     C                   MOVEL     'S'           WRIEM
436900040218e   1C                   ENDDO
437000070420      *?F6 - Stampa LDV
437100070420if  1c                   if        *inKf = *on
437200070420     C                   MOVEL     'S'           WCMD
437300080201     c* Se assegnato non abilitato non stampo la LdV ma emetto errore
437400150617     c                   if        w_tbo='A' and arbaca=' '
437500160505     C                   MOVEL     MSG(3)        V1CMSG
437600080201     C                   SETON                                        28
437700080201     c                   else
437800070420     c
437900070420     c                   if        wrichtrul90=' '
438000070420     c* La prima volta devo proporre la richiesta stampanti
438100070420     c                   exsr      RICHSTAMPAN
438200070420     C                   MOVEL     'S'           WRIEM
438300070420     c
438400070420     c* Se premuto F3 esco senza fare nulla
438500070420     c                   if        d90f3<>'1' and not *in91
438600070420     c                   eval      wrichtrul90='S'
438700070420     c                   endif
438800070420     c                   if        *in91
438900160505     C                   MOVEL     MSG(2)        V1CMSG
439000070420     C                   SETON                                        28
439100070420     c                   endif
439200070420     c                   endif
439300070420     c
439400070420     c                   if        wrichtrul90='S'
439500161111      * OVRPRTF BOLLE A4
439600161111     C                   MOVEA     D90PRB4       CMA4f6(30)
439700161111     C                   MOVEA     D90MDB4       CMA4f6(52)
439800161111     C*
439900161111     C                   Z-ADD     130           LUNG             15 5
440000161111     C                   CLEAR                   COMMAN          130
440100161111     C                   MOVEA     CMA4f6        COMMAN
440200161111     C                   CALL      'QCMDEXC'                            91
440300161111     C                   PARM                    COMMAN
440400161111     C                   PARM                    LUNG
440500161111      *
440600161111     C                   if        not *in91
440700161111      *
440800161111      * OVRPRTF BOLLE A5
440900161111     C                   MOVEA     D90PRB5       CMA5f6(30)
441000161111     C                   MOVEA     D90MDB5       CMA5f6(52)
441100161111     C*
441200161111     C                   Z-ADD     130           LUNG
441300161111     C                   CLEAR                   COMMAN
441400161111     C                   MOVEA     CMA5f6        COMMAN
441500161111     C                   CALL      'QCMDEXC'                            91
441600161111     C                   PARM                    COMMAN
441700161111     C                   PARM                    LUNG
441800161111     c                   endif
441900161111      *
442000070420     C                   CLEAR                   FNLSB5DS
442100070426     c                   movel     'L'           db0tla
442200070420     C                   MOVEL     'R'           DB0RIS
442300150617     C                   MOVEL     w_tbo         DB0TBO
442400070420     C                   MOVEL     ARBAAS        DB0AAS
442500070420     C                   MOVEL     ARBLNP        DB0LNP
442600070420     C                   MOVEL     ARBNRS        DB0NRS
442700070420     C                   MOVEL     ARBNSP        DB0NSP
442800130319     c                   reset                   fnlsB6ds1
442900070420     C                   CALL      D90PSL
443000070420     C                   PARM                    FNLSB5DS
443100130308     c                   parm                    fnlsB6ds1
443200160505     C                   MOVEL     MSG(1)        V1CMSG
443300070420     C                   SETON                                        28
443400070420     c                   endif
443500080201     c                   endif
443600070420     c                   endif
443700930525     C*
443800990119     C* F7 - ANOMALIE
443900040218do  1C   KG              DO
444000960523     C                   MOVEL     'S'           WCMD
444100960523     C*
444200991027     C                   CLEAR                   FNLr44DS
444300960523     C                   MOVE      V2CAAS        D44AAS
444400960523     C                   MOVE      COMLNP        D44FIL
444500960523     C                   MOVE      V2CNRS        D44NRS
444600960523     C                   MOVE      V2CNSP        D44NSP
444700150617     C***                MOVEL     'A'           D44TBO
444800150617     C                   MOVEL     w_tbo         D44TBO
444900960523     C                   MOVE      'I'           D44MOD
445000991027     C                   MOVEL     FNLr44DS      KPJBU
445100960523     C                   CALL      'FNLR44R'
445200960523     C                   PARM                    KPJBA
445300960523     C*
445400990119     C                   MOVEL     'S'           WRIEM
445500040218e   1C                   ENDDO
445600920804     C*
445700160628     C* F23 - INTERROGAZIONE EVENTI SEGNACOLLI
445800160628if  1C     *INKX         IFEQ      *ON
445900961121     C                   MOVEL     'S'           WCMD
446000961121     C*
446100991027     C                   CLEAR                   FNLR45DS
446200961121     C                   MOVE      V2CAAS        D45AAS
446300961121     C                   MOVE      COMLNP        D45FIL
446400961121     C                   MOVE      V2CNRS        D45NRS
446500961121     C                   MOVE      V2CNSP        D45NSP
446600991027     C                   MOVEL     FNLR45DS      KPJBU
446700961121     C                   CALL      'FNLR45R'
446800961121     C                   PARM                    KPJBA
446900961121     C*
447000990119     C                   MOVEL     'S'           WRIEM
447100040218e   1C                   ENDIF
447200160628     C* CMD 8 - VISUALIZZA ORM
447300160628if  1C     *INKH         IFEQ      *ON
447400160628     C                   MOVEL     'S'           WCMD
447500160628
447600160628     C                   clear                   PARM01
447700160628     C                   move      v3cpoe        ppoe
447800160628     C                   move      v3cnor        pnor
447900160628     C                   move      v3cnsr        pnsr
448000160628     C                   move      v3cnrv        pnrv
448100160628     C                   move      '5'           psce
448200160628     C                   move      v3clnp        pfgs
448300160628     C                   movel(p)  PARM01        KPJBU
448400160628
448500160628     C                   call      'FIOR07R'
448600160628     C                   parm                    KPJBA
448700160628     C                   MOVEL     'S'           WRIEM
448800160628
448900160628e   1C                   ENDIF
449000130830     c
449100130830     c* F9 - Orari servizi
449200131007     c                   if        *inki=*on
449300130830     C                   MOVEL     'S'           WCMD
449400131007     c                   if        wabif9='S'
449500150611     c                   clear                   W_video
449600130930     c                   exsr      sr_orarisr
449700131007     c                   endif
449800130830     C                   MOVEL     'S'           WRIEM
449900130830e   1C                   ENDIF
450000021213     C*
450100021213     C* F11 - Note supermercati
450200040218if  1C     *INKK         IFEQ      *ON
450300021213     C     *IN77         ANDEQ     *ON
450400021213      * imposto i campi 'bolla' della ds
450500021217     C                   MOVEL     'S'           WCMD
450600021213     C                   clear                   FnLrb3ds
450700030122     c                   Movel     V1cdsa        Rb3Rsu
450800030121     C                   z-add     SimFel        RB3Fil
450900021216     C                   z-add     V3CAAS        RB3AAS
451000030121     C                   z-add     V3CLNP        RB3LNP
451100021216     C                   z-add     V3CNRS        RB3NRS
451200021216     C                   z-add     V3CNSP        RB3NSP
451300021213     C                   z-add     V3CDSP        RB3DSP
451400021213     C                   z-add     VIDRMN        RB3RMN
451500021213     C                   z-add     V3CLNA        RB3LNA
451600131108     C                   movel     V32RSD        RB3RSD
451700021218     C                   z-add     VIDPKF        RB3PKF
451800021213     C                   z-add     VIDNCL        RB3NCL
451900021213     C                   z-add     VIDVLF        RB3VLB
452000030703      * bancali - part.varie "B"
452100040218If  2c                   If        %subst(ArbGva:1:1) = 'B'
452200030121     C                   z-add     §Ar5Tba       RB3Tba
452300030121     C                   z-add     VidNba        RB3Nba
452400030121     C                   z-add     §Ar5Tb2       RB3Tb2
452500030121     C                   z-add     VidNb2        RB3Nb2
452600030703     c                   Movel     V4cGva        Rb3Gva
452700040218e   2c                   EndIf
452800030703      * bancali - part.varie "O"
452900040218If  2c                   If        %subst(ArbGva:1:1) = 'O'
453000030703     C                   z-add     §Ar5bTba      RB3Tba
453100030703     C                   z-add     VidNba        RB3Nba
453200030703     C                   z-add     §Ar5bTb2      RB3Tb2
453300030703     C                   z-add     VidNb2        RB3Nb2
453400030703     c                   Movel     V4cGva        Rb3Gva
453500030703     c                   Z-Add     §Ar5bCsf      Rb3Csf
453600030703     c                   Z-Add     §Ar5bCor      Rb3Cor
453700040218e   2c                   EndIf
453800021213      * richiamo PGM per note supermercati
453900021213     C                   call      'FNLRB3R'
454000030122     C                   PARM                    Kpjba
454100030121     C                   PARM                    Fnlrb3Ds
454200021213      *
454300021217     C                   MOVEL     'S'           WRIEM
454400040218e   1C                   ENDIF
454500961121     C*
454600990119     C* F14 - GIACENTE: VADO IN INTERROGAZIONE GIACENZA
454700040218if  1C     *INKN         IFEQ      *ON
454800930624     C     *IN03         ANDEQ     *ON
454900160504     C     PARGIA        ANDEQ     *BLANK
455000940905     C                   MOVEL     'S'           WCMD
455100991027     C                   CLEAR                   FNLG30DS
455200941213     C                   MOVEL     'A'           D30POA
455300941213     C                   MOVEL     V2CAAS        D30AAS
455400941213     C                   MOVEL     COMLNP        D30LNP
455500941213     C                   MOVEL     V2CNRS        D30NRS
455600941213     C                   MOVEL     V2CNSP        D30NSP
455700991027     C                   MOVEL     FNLG30DS      KPJBU
455800941213     C                   CALL      'FNLG31C'
455900920804     C                   PARM                    KPJBA
456000930624     C*
456100990119     C                   MOVEL     'S'           WRIEM
456200040218e   1C                   ENDIF
456300040218      *
456400040218      *?F16 - Gestione reclami?
456500040218if  1c                   if        *inKQ = *on
456600040225     c                   eval      wARBaas = ARBaas
456700131111     c                   eval      wARBlnp = ARBlnp
456800131111     c                   eval      wARBnrs = ARBnrs
456900131111     c                   eval      wARBnsp = ARBnsp
457000040225     c                   reset                   FIDNA2ds
457100150402if  2c                   if        SDSprm  > 1  and ilru6ra='S'
457200040225     c                   eval      IA2mod  = 'I'
457300040225e   2c                   endif
457400040225     c                   eval      IA2ogg  = ds_OggSped
457500040225     c                   call      'FIDNA2R'
457600040225     c                   parm                    KPJBA
457700040225     c                   parm                    FIDNA2ds
457800040225     c                   movel     OA2f03        WF03
457900040225     c                   movel     'S'           WRIEM
458000051201     C                   MOVEL     'S'           WCMD
458100040218e   1c                   endif
458200070404      *
458300140311      *?F17 - Note AUT
458400160916      * (Questo tasto funzionale è attivo solo se la bolla è in arrivo. Se è
458500160916      *  solo in partenza non è attivo)
458600070404if  1c                   if        *inKR = *on
458700140123     c                   clear                   fnlrn6ds
458800140123     c                   eval      in6mod='V'
458900140123     c                   eval      in6aas=v2caas
459000140123     c                   eval      in6lnp=comlnp
459100140123     c                   eval      in6nrs=v2cnrs
459200140123     c                   eval      in6nsp=v2cnsp
459300140123      * se sono una spedizione in consegna passo i dati della distinta del file ARB
459400140123     c                   IF        ARBfbc = *blanks and
459500140123     c                               ARBddc > 0 and
459600161129     c                               ARBndc <> *all'8'  and
459700161129     c                               ARBndc <> *all'9'  and
459800140123     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
459900140123     c                               arbica='R' or arbicc=' ' or arbicc='R')
460000161129     c
460100160916     c***  KSPED         chain     Fnarb01l
460200160916     c****               If        %found(Fnarb01l)
460300140123     c                   eval      in6fgs = arbifp
460400140123     c                   eval      in6nfv = arbndc
460500140123     c                   eval      in6dfv = arbddc
460600140123     c                   eval      in6pdr = arbpdc
460700140123     c                   eval      in6cons = 'S'
460800160916     c***                Endif
460900140123     c                   Endif
461000140123     c                   call      'FNLRN6R'
461100070404     c                   parm                    kpjba
461200140123     c                   parm                    fnlrn6ds
461300070404     c                   movel     'S'           WRIEM
461400070404     C                   MOVEL     'S'           WCMD
461500070404e   1c                   endif
461600920804     C*
461700990119     C* F21 - DETTAGLIO SEGNACOLLI
461800040218do  1C   KV              DO
461900940905     C                   MOVEL     'S'           WCMD
462000950628     C                   MOVEL     'A'           PA1TBO
462100950629     C                   CLEAR                   PA1F03
462200160505     C                   CLEAR                   PA1FlG
462300160610
462400160610     c                   eval      pa1AAS=kaas
462500160610     c                   eval      pa1LNP=klnp
462600160610     c                   eval      pa1NRS=knrs
462700160610     c                   eval      pa1NSP=knsp
462800160916     c                   eval      pa1tfp=arbtfp
462900160916     c                   eval      pa1tfa=arbtfa
463000160610
463100160504     C                   MOVEL     PARAMint      KPJBU
463200161019     C                   CALL      'FNLRU38R'
463300920804     C                   PARM                    KPJBA
463400160504     C                   MOVEL     KPJBU         PARAMint
463500940802     C                   MOVEL     PA1F03        WF03
463600990119     C                   MOVEL     'S'           WRIEM
463700040218e   1C                   ENDDO
463800920804     C*
463900990119     C* F10 - INTERROGA VARIAZIONI BOLLE EFFETTUATE
464000040218do  1C   KJ              DO
464100940905     C                   MOVEL     'S'           WCMD
464200150617     C***                MOVEL     'A'           PA1TBO
464300150617     C                   MOVEL     w_tbo         PA1TBO
464400950629     C                   CLEAR                   PA1F03
464500160505     C                   CLEAR                   PA1flg
464600160610     c                   eval      pa1AAS=kaas
464700160610     c                   eval      pa1LNP=klnp
464800160610     c                   eval      pa1NRS=knrs
464900160610     c                   eval      pa1NSP=knsp
465000160504     C                   MOVEL     PARAMint      KPJBU
465100940726     C                   CALL      'FNLR75R'
465200920804     C                   PARM                    KPJBA
465300160504     C                   MOVEL     KPJBU         PARAMint
465400940802     C                   MOVEL     PA1F03        WF03
465500990119     C                   MOVEL     'S'           WRIEM
465600040218e   1C                   ENDDO
465700930114     C*
465800990119     C* F19 - INTERROGA SPUNTE VARI
465900040218if  1C     *INKT         IFEQ      *ON
466000940905     C                   MOVEL     'S'           WCMD
466100940713     C                   Z-ADD     0             PA4NFV
466200940713     C                   MOVEL     9             PA4NPG
466300940713     C                   MOVEL     99            PA4NPS
466400160519     C                   MOVEL     'FNLRU6R1'    PA4PGM
466500101203     C                   MOVEL     '1'           PA4FLG
466600950214     C                   MOVEL     V3CFLS        PA4LNP
466700940719     C                   MOVEL     V3CLNA        PA4LNA
466800940719     C                   MOVEL     V3CNRS        PA4NRS
466900940713     C                   Z-ADD     0             PA4NCD
467000940713     C                   Z-ADD     0             PA4NCA
467100101201     c                   eval      pa4boan=v2caas
467200101201     c                   eval      pa4bolp=comlnp
467300101201     c                   eval      pa4bors=v2cnrs
467400101201     c                   eval      pa4bosp=v2cnsp
467500040218if  2C     VIDNCD        IFGT      0
467600940713     C                   MOVEL     VIDNCD        PA4NCD
467700940713     C                   MOVEL     VIDNCA        PA4NCA
467800101201x   2C**                 ELSE
467900930705     C* CERCO I SEGNACOLLI DAL DETTAGLIO
468000101201     C**   KARB2         SETLL     FNART01L
468100101201     C**   KARB2         READE     FNART01L                               31
468200101201do  3C*+N31              DO
468300101201     C**                 Z-ADD     ARTNSC        PA4NCD
468400101201     C**   KARB2         SETGT     FNART01L
468500101201     C**   KARB2         READPE    FNART01L                               31
468600101201if  4C**N31ARTNSC        IFNE      PA4NCD
468700101201     C**                 MOVEL     ARTNSC        PA4NCA
468800101201e   4C**                 ENDIF
468900101201e   3C**                 ENDDO
469000101201e   2C                   ENDIF
469100930705     C*
469200940802     C                   MOVEL     PARA04        KPJBU
469300940722     C                   CALL      'FNLR04R'
469400920804     C                   PARM                    KPJBA
469500940725     C                   MOVEL     'S'           WRIEM
469600060130     C                   MOVEL     'S'           WCMD
469700040218e   1C                   ENDIF
469800930415     C*
469900070404     c
470000931112      *
470100990119      * F20 - INTERROGAZIONE AFFIDABILITA'
470200040218do  1C   KU              DO
470300940905     C                   MOVEL     'S'           WCMD
470400950102     C                   CLEAR                   FNLR97
470500950102     C                   MOVEL     V2CAAS        R97AAS
470600950102     C                   MOVEL     COMLNP        R97LNP
470700950102     C                   MOVEL     V2CNRS        R97NRS
470800950102     C                   MOVEL     V2CNSP        R97NSP
470900950102     C                   MOVEL     V3CDSP        R97DSP
471000950102     C                   MOVEL     VIDRMN        R97RMN
471100950102     C                   MOVEL     V3CLNA        R97LNA
471200950102     C                   MOVEL     V3DLNA        R97DLA
471300950102     C                   Z-ADD     VIDDCP        R97DCP
471400950102     C                   MOVEL     V4CRSD        R97RSD
471500950102     C                   MOVEL     V1CDSA        R97RSU
471600040121     C                   MOVEL     *blanks       R97CML
471700160506     C                   MOVEL     w_tbo         R97AP
471800950102     C   95              MOVEL     'S'           R97EVE
471900070404     C                   Z-ADD     w08DBR        R97DBR
472000070404     C                   Z-ADD     w08DPC        R97DPC
472100070404     C                   Z-ADD     w08DUC        R97DUC
472200950102     C                   MOVEL     VIDTMC        R97TMC
472300950102     C                   Z-ADD     VIDCAS        R97CAS
472400950102     C                   MOVEL     VIDVCA        R97VCA
472500931112     C                   MOVE      *BLANKS       KPJBU
472600950102     C                   MOVEL     FNLR97        KPJBU
472700940722     C                   CALL      'FNLR97R'                            90
472800931112     C                   PARM                    KPJBA
472900950102     C                   MOVEL     KPJBU         FNLR97
473000950102     C                   MOVEL     R97F03        WF03
473100950102     C                   MOVEL     'S'           WRIEM
473200040218e   1C                   ENDDO
473300110930     C* F22 - Perfect Order
473400110929do  1C                   if        *inkw
473500110929     C                   MOVEL     'S'           WCMD
473600160606     c                   movel     paramU6ds1    paramu6ds
473700110929     c*
473800160527     c                   MOVEL     'W'           parflg_
473900160527     c                   movel(p)  vidrma        parrma_
474000160527     c                   movel(p)  vidxco        parxco_
474100160527     c                   clear                   parflo_
474200160527     c                   movel     paramU6ds     kpjbu
474300160519     c                   call      'FNLRU6C'
474400110929     c                   parm                    kpjba
474500110929     c*
474600110929     C*
474700110929     C                   MOVEL     'S'           WRIEM
474800110929e   1C                   ENDif
474900940905     C*
475000131107     C* ? FORMATO VIDEO:  LRI6D06  (TASSAZIONE 1a BOLLA)   ---   TASTO F13 ?
475100991026     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
475200040218if  1C     *INKM         IFEQ      *ON
475300991027     C                   eval      WCMD = 'S'
475400991027     C                   EVAL      *in89 = *off
475500131107     C                   WRITE     LRI6T01
475600131107     C                   WRITE     LRI6D06
475700991027     C                   eval      NR6 = 1
475800131107     C                   EXFMT     LRI6C06
475900040218e   1C                   ENDIF
476000991027     C*
476100131107     C* ? FORMATO VIDEO:  LRI6D09  (TASSAZIONE 2a BOLLA)   ---   TASTO F04 ?
476200991027     C*  EMETTO SOLO SE VI E' TASSAZIONE (SE TROVATO FIAR600F)
476300040218if  1C     *INKD         IFEQ      *ON
476400991027     C                   eval      WCMD = 'S'
476500991027     C                   EVAL      *in89 = *off
476600131107     C                   WRITE     LRI6T01
476700131107     C                   EXFMT     LRI6D09
476800040218e   1C                   ENDIF
476900990119     C*
477000990119     C* F2 - INTERROGAZIONE COMUNICAZIONE ANOMALIA
477100990129     C*      NON RICHIAMO SE RICHIAMATO DA INTERROGAZINE C.A.
477200040218if  1C     *INKB         IFEQ      *ON
477300160504     C     PArFLG        ANDNE     'C'
477400040218if  2C     CONTCA        IFEQ      1
477500991027     C                   CLEAR                   FIDN00DS
477600990119     C                   Z-ADD     VHCAAC        I00AAC
477700990119     C                   Z-ADD     VHCFIL        I00FIL
477800990119     C                   Z-ADD     VHCNCA        I00NCA
477900990119     C                   Z-ADD     ARBAAS        I00AAS
478000990119     C                   Z-ADD     ARBNRS        I00NRS
478100990119     C                   Z-ADD     ARBNSP        I00NSP
478200990119     C                   Z-ADD     ARBLNP        I00LNP
478300990119     C                   MOVEL     'A'           I00TPB
478400991027     C                   MOVEL     FIDN00DS      KPJBU
478500990119     C                   CALL      'FIDN01R'
478600990119     C                   PARM                    KPJBA
478700000309     C                   PARM                    callricors        1
478800040218x   2C                   ELSE
478900990119     C* CARICO LA VIDEATA DELLE ANOMALIE CHE CI SONO
479000990119     C                   SETOFF                                       79
479100131107     C                   WRITE     LRI6C11
479200990119     C                   SETON                                        797823
479300990119     C                   CLEAR                   NRR11
479400040809      *
479500040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
479600040809      * legate alla spedizione che passo
479700040809     c                   clear                   fidn12ds
479800040809     c                   eval      i12aas = arbAAS
479900040809     c                   eval      i12lnp = arbLNP
480000040809     c                   eval      i12nrs = arbNRS
480100040809     c                   eval      i12nsp = arbNSP
480200040809     c                   eval      i12fel = 'E'
480300040809     c                   eval      i12fan = 'E'
480400040809     c*
480500040809     c                   call      'FIDN12R'
480600040809     c                   parm                    fidn12ds
480700040809     c*
480800040809     c                   setoff                                       31
480900040809     c*
481000040809     c* se non ci sono errori
481100040809     c                   if        o12err <> ' '
481200040809     c                   seton                                        31
481300040809     c                   else
481400040809     c* se numero di CA trovate maggiore di zero
481500040809     c                   if        o12nca = 0
481600040809     c                   seton                                        31
481700040809     c                   else
481800040809     c                   z-add     1             ii                2 0
481900040809     c                   dow       ii <= o12nca and *in31 = *off
482000040809     c                   movea     skkey(ii)     dskey
482100040809     C                   Z-ADD     dsaac         V11AAC
482200040809     C                   Z-ADD     dsfil         V11FIL
482300040809     C                   Z-ADD     dsnca         V11NCA
482400040809     C                   Z-ADD     skdca(ii)     G02INV
482500040809     C                   Z-ADD     *ZERO         G02DAT
482600040809     C                   MOVEL     '3'           G02ERR
482700040809     C                   CALL      'XSRDA8'
482800040809     C                   PARM                    WLBDAT
482900040809     C                   Z-ADD     G02DAT        V11DCA
483000040809     C                   Z-ADD     skfca(ii)     V11FCA
483100040809     C     skfca(ii)     CHAIN     FNDFA01L
483200040809     C                   IF        %found(FNDFA01L)
483300040809     C                   MOVEL     DFADES        V11DFA
483400040809     C                   ENDIF
483500040809     C**
483600040809     C                   ADD       1             NRR11             4 0
483700131107     C                   WRITE     LRI6S11
483800040809     c*
483900040809     c                   add       1             ii
484000040809     c                   enddo
484100040809     c*
484200040809     c                   seton                                        31
484300040809     c*
484400040809     c                   endif
484500040809     c                   endif
484600040809     c**
484700990119     C* EMETTO FORMATO
484800990119     C     FOR11         TAG
484900990119     C                   Z-ADD     1             NRR11
485000131107     C                   WRITE     LRI6T01
485100131107     C                   WRITE     LRI6Z11
485200131107     C                   EXFMT     LRI6C11
485300990119     C* F12 - RITORNO
485400040218if  3C     *INKL         IFEQ      *OFF
485500131107     C                   READC     LRI6S11                                31
485600040218do  4C     *IN31         DOWEQ     *OFF
485700040218if  5C     V11SCE        IFEQ      '5'
485800991027     C                   CLEAR                   FIDN00DS
485900990119     C                   Z-ADD     V11AAC        I00AAC
486000990119     C                   Z-ADD     V11FIL        I00FIL
486100990119     C                   Z-ADD     V11NCA        I00NCA
486200990119     C                   Z-ADD     ARBAAS        I00AAS
486300990119     C                   Z-ADD     ARBNRS        I00NRS
486400990119     C                   Z-ADD     ARBNSP        I00NSP
486500990119     C                   Z-ADD     ARBLNP        I00LNP
486600990119     C                   MOVEL     'A'           I00TPB
486700991027     C                   MOVEL     FIDN00DS      KPJBU
486800990119     C                   CALL      'FIDN01R'
486900990119     C                   PARM                    KPJBA
487000000309     C                   PARM                    callricors        1
487100990119     C**
487200990119     C                   CLEAR                   V11SCE
487300131107     C                   UPDATE    LRI6S11
487400040218e   5C                   ENDIF
487500990119     C**
487600131107     C                   READC     LRI6S11                                31
487700040218e   4C                   ENDDO
487800990119     C                   GOTO      FOR11
487900040218e   3C                   ENDIF
488000040218e   2C                   ENDIF
488100990119     C*
488200990119     C                   MOVEL     'S'           WRIEM
488300990119     C                   MOVEL     'S'           WCMD
488400040218e   1C                   ENDIF
488500931112      *
488600920804     C                   ENDSR
488700070420     C**************************************************************************
488800070420      * RICHIEDO LE STAMPANTI per stampa LDV
488900070420     C**************************************************************************
489000070420     c     RICHSTAMPAN   BEGSR
489100070420     C                   CLEAR                   TRUL90DS
489200070420      *
489300070420     C                   MOVEL     'S'           d90RSB
489400070420     C                   CALL      'TRUL90R'
489500070420     C                   PARM                    KPJBA
489600070420     C                   PARM                    trul90DS
489700070420      *
489800070420     C* F3 - FINE
489900070420     c                   if        d90f3<>'1'
490000070420      *
490100070420      * OVRPRTF BOLLE A4
490200070426     C                   MOVEA     D90PRB4       CMA4f6(30)
490300070426     C                   MOVEA     D90MDB4       CMA4f6(52)
490400070420     C*
490500111221     C                   Z-ADD     130           LUNG             15 5
490600111221     C                   CLEAR                   COMMAN          130
490700160503     C                   MOVEA     CMA4f6        COMMAN
490800070420     C                   CALL      'QCMDEXC'                            91
490900070420     C                   PARM                    COMMAN
491000070420     C                   PARM                    LUNG
491100070420      *
491200070420     C                   if        not *in91
491300070420      *
491400070420      * OVRPRTF BOLLE A5
491500070426     C                   MOVEA     D90PRB5       CMA5f6(30)
491600070426     C                   MOVEA     D90MDB5       CMA5f6(52)
491700070420     C*
491800111221     C                   Z-ADD     130           LUNG
491900070420     C                   CLEAR                   COMMAN
492000160503     C                   MOVEA     CMA5f6        COMMAN
492100070420     C                   CALL      'QCMDEXC'                            91
492200070420     C                   PARM                    COMMAN
492300070420     C                   PARM                    LUNG
492400070420     c                   endif
492500070420     c                   endif
492600070420      *
492700070420     c                   ENDSR
492800940802     C**************************************************************************
492900940802     C*    VISUALIZZA LEGAMI BOLLA
493000940802     C**************************************************************************
493100930413     C     CMD18         BEGSR
493200940802     C*
493300940411     C                   CLEAR                   WCAR
493400160610     c                   eval      w_tbo_sav=w_tbo
493500160913     c                   eval      blp_arbdcm_sav=blp_arbdcm
493600161014     c                   eval      blp_arbhmc_sav=blp_arbhmc
493700160610     c                   eval      win12=*in12
493800940411     C*
493900940411     C* MUOVO I DATI DELLA BOLLA CHE STO VISUALIZZANDO
494000940714     C                   MOVEL     V2CAAS        NUM2              4 0
494100940411     C                   MOVEL     COMLNP        COMLPN
494200940714     C                   MOVEL     V2CNRS        COMNRN
494300940714     C                   MOVEL     V2CNSP        COMNSN
494400950321     C                   MOVEL     V3CLNA        COMLAN
494500950428     C                   MOVEL     COMC08        SAVC08
494600940408     C**
494700940713     C* SAVNR8 > 1 --> GESTIONE SUBFILE
494800940408     C**
494900940907    1C     SAVNR8        IFGT      2
495000940408     C*
495100940809     C                   MOVEL     'S'           WRIEM
495200930413     C     FORC2         TAG
495300940713     C                   Z-ADD     1             REC8
495400940809     C     WRIEM         IFEQ      'S'
495500131107     C                   WRITE     LRI6T01
495600131107     C                   WRITE     LRI6Z08
495700940809     C                   MOVEL     *BLANKS       WRIEM
495800940809     C                   ENDIF
495900950428     C*
496000950428     C                   MOVEL     SAVC08        COMC08
496100131107     C                   EXFMT     LRI6C08
496200930413     C*
496300930413     C* CMD12 - FINE LAVORO
496400930413     C   KL              GOTO      SOTTO
496500930413     C*
496600131107     C                   READC     LRI6S08                                93
496700930510     C   93              GOTO      SOTTO
496800930510     C*
496900940408    2C     *IN93         DOWEQ     '0'
497000930413     C*
497100941230    3C     V8CSCE        IFEQ      '5'
497200940713     C                   MOVEL     ' '           V8CSCE
497300950103     C*
497400950103     C* RIPRISTINO GLI INDICATORI
497500950103     C                   MOVEL     V8CI80        *IN80
497600950103     C                   MOVEL     V8CI81        *IN81
497700950103     C                   MOVEL     V8CI82        *IN82
497800950103     C                   MOVEL     V8CI83        *IN83
497900131107     C                   UPDATE    LRI6S08
498000930504     C*
498100940907     C     V8CAAN        IFEQ      *ZERO
498200940907     C                   MOVEL     V8CAAP        LBLAAN
498300940907     C                   MOVEL     V8CLPP        LBLLPN
498400940907     C                   MOVEL     V8CNRP        LBLNRN
498500940907     C                   MOVEL     V8CNSP        LBLNSN
498600940907     C                   MOVEL     'P'           V8CFPS            1
498700940907     C                   ELSE
498800940907     C                   MOVEL     V8CAAN        LBLAAN
498900940907     C                   MOVEL     V8CLPN        LBLLPN
499000940907     C                   MOVEL     V8CNRN        LBLNRN
499100940907     C                   MOVEL     V8CNSN        LBLNSN
499200940907     C                   MOVEL     'S'           V8CFPS
499300940907     C                   ENDIF
499400940408     C*
499500940408     C* CONTROLLO SE IL LEGAME E' IN PARTENZA O IN ARRIVO
499600160610     c                   clear                   w_tbo
499700160913     c                   clear                   blp_arbdcm
499800161014     c                   clear                   blp_arbhmc
499900150423     C     KLBL1         CHAIN     FNblp01L                           93
500000160610     c                   if        %found(fnblp01l)
500100160610     c                   eval      w_tbo='P'
500200160913     c                   eval      blp_arbdcm=arbdcm
500300161014     c                   eval      blp_arbhmc=arbhmc
500400160610     c                   seton                                        12
500500160610     c                   endif
500600940907     C     KLBL1         CHAIN     FNARB01L                           93
500700160610     c                   if        %found(fnarb01l)
500800160610     c                   eval      w_tbo='A'
500900160610     c                   setoff                                       12
501000160610     c                   endif
501100940408     C*
501200150423    4C**** *IN93         IFEQ      *OFF
501300940408     C*
501400940408     C                   EXSR      LEG2
501500940408     C*
501600150423   X4C****               ELSE
501700940408     C*
501800150423     C*                  Z-ADD     LBLAAN        PA0AAS
501900150423     C*                  Z-ADD     LBLLPN        PA0LNP
502000150423     C*                  Z-ADD     LBLNRN        PA0NRS
502100150423     C*                  Z-ADD     LBLNSN        PA0NSP
502200150423     C*                  MOVEL     rsut          PA0RSU
502300150423     C*                  MOVEL     SIMFEL        PA0SIM
502400150423     C*                  MOVEL     *blanks       PA0CML
502500150423     C*    PA1FLG        IFEQ      'N'
502600960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
502700960903     C*       verifichi questo caso
502800150423     C*                  MOVEL     'R'           PA0FLG
502900150423     C*                  ELSE
503000150423     C*                  MOVEL     'A'           PA0FLG
503100150423     C*                  END
503200150423     C*                  MOVEL     *BLANKS       PA0CLI
503300150423     C*                  CLEAR                   PA0F03
503400150423     C*                  MOVEL     *BLANKS       PA0GIA
503500150423     C*                  MOVEL     PARAM0        KPJBU
503600150423     C*                  CALL      'FNLS05R'
503700150423     C*                  PARM                    KPJBA
503800150423     C*                  MOVEL     KPJBU         PARAM0
503900150423     C*                  MOVEL     PA0F03        WF03
504000150423     C*                  MOVEL     'S'           WRIEM
504100150423    4C*                  ENDIF
504200940408     C*
504300940408    3C                   ENDIF
504400930413     C*
504500131107     C                   READC     LRI6S08                                93
504600940408    2C                   ENDDO
504700930413     C                   GOTO      FORC2
504800930413     C*
504900930413     C     SOTTO         TAG
505000930413     C*
505100940408    1C                   ELSE
505200940408     C*
505300940907     C* LEGGO QUALE DELLE DUE BOLLE NON E' QUELLA IN ESAME
505400131107     C     2             CHAIN     LRI6S08                            99
505500940907     C     V8CAAP        IFEQ      V3CAAS
505600940907     C     V8CLPP        ANDEQ     V3CLNP
505700940907     C     V8CNRP        ANDEQ     V3CNRS
505800940907     C     V8CNSP        ANDEQ     V3CNSP
505900940907     C                   MOVEL     V8CAAN        ARBAAS
506000940907     C                   MOVEL     V8CLPN        ARBLNP
506100940907     C                   MOVEL     V8CNRN        ARBNRS
506200940907     C                   MOVEL     V8CNSN        ARBNSP
506300940907     C                   MOVEL     'S'           V8CFPS
506400940907     C                   ELSE
506500940907     C                   MOVEL     V8CAAP        ARBAAS
506600940907     C                   MOVEL     V8CLPP        ARBLNP
506700940907     C                   MOVEL     V8CNRP        ARBNRS
506800940907     C                   MOVEL     V8CNSP        ARBNSP
506900940907     C                   MOVEL     'P'           V8CFPS
507000940907     C                   ENDIF
507100940907     C* QUINDI VEDO SE LA TROVO FRA GLI ARRIVI
507200150423     C     KARB2         CHAIN     FNblp01L                           93
507300160610     c                   if        %found(fnblp01l)
507400160610     c                   eval      w_tbo='P'
507500160913     c                   eval      blp_arbdcm=arbdcm
507600161014     c                   eval      blp_arbhmc=arbhmc
507700160610     c                   seton                                        12
507800160610     c                   endif
507900940907     C     KARB2         CHAIN     FNARB01L                           93
508000160610     c                   if        %found(fnarb01l)
508100160610     c                   eval      w_tbo='A'
508200160610     c                   setoff                                       12
508300160610     c                   endif
508400940408     C*
508500150423    2C*    *IN93         IFEQ      *ON
508600150423     C*                  Z-ADD     ARBAAS        PA0AAS
508700150423     C*                  Z-ADD     ARBLNP        PA0LNP
508800150423     C*                  Z-ADD     ARBNRS        PA0NRS
508900150423     C*                  Z-ADD     ARBNSP        PA0NSP
509000150423     C*                  MOVEL     rsut          PA0RSU
509100150423     C*                  MOVEL     SIMFEL        PA0SIM
509200150423     C*                  MOVEL     *blanks       PA0CML
509300150423     C*    PA1FLG        IFEQ      'N'
509400960903     C* N.B.: Non testo anche pa1flg='P' perchè è impossibile che si
509500960903     C*       verifichi questo caso
509600150423     C*                  MOVEL     'R'           PA0FLG
509700150423     C*                  ELSE
509800150423     C*                  MOVEL     'A'           PA0FLG
509900150423     C*                  END
510000150423     C*                  MOVEL     *BLANKS       PA0CLI
510100150423     C*                  CLEAR                   PA0F03
510200150423     C*                  MOVEL     *BLANKS       PA0GIA
510300150423     C*                  MOVEL     PARAM0        KPJBU
510400150423     C*                  CALL      'FNLS05R'
510500150423     C*                  PARM                    KPJBA
510600150423     C*                  MOVEL     KPJBU         PARAM0
510700150423     C*                  MOVEL     PA0F03        WF03
510800150423     C*                  MOVEL     'S'           WRIEM
510900121116     c* reimposto la key spedizione
511000150423     C*                  MOVEL     NUM2          arbAAS
511100150423     C*                  MOVEL     COMLPN        arbLNP
511200150423     C*                  MOVEL     COMNRN        arbNRS
511300150423     C*                  MOVEL     COMNSN        arbNSP
511400930510     C*
511500150423   X2C*                  ELSE
511600940408     C*
511700940408     C                   EXSR      LEG2
511800940408     C*
511900150423    2C*                  END
512000940408    1C                   END
512100160610
512200160610     c                   eval      w_tbo=w_tbo_sav
512300160913     c                   eval      blp_arbdcm=blp_arbdcm_sav
512400161014     c                   eval      blp_arbhmc=blp_arbhmc_sav
512500160610     c                   eval      *in12=win12
512600940411     C*
512700940411     C* RICARICO I DATI DELLA BOLLA INIZIALE
512800940411     C     WCAR          IFEQ      'C'
512900940411     C                   SETOFF                                       75
513000940714     C                   MOVEL     NUM2          V2CAAS
513100940411     C                   MOVEL     COMLPN        COMLNP
513200940714     C                   MOVEL     COMNRN        V2CNRS
513300940714     C                   MOVEL     COMNSN        V2CNSP
513400160610     c     karb          chain     fnblp01l
513500940712     C     KARB          CHAIN     FNARB01L                           93
513600940411     C* RICARICO I DATI
513700940411     C                   EXSR      BOLLA
513800940411     C*
513900940411     C* RIABILITO IL CMD18
514000940411     C                   SETON                                        94
514100940411    1C                   END
514200940802     C*
514300930413     C                   ENDSR
514400940802     C**************************************************************************
514500940802     C*    LEGAMI BOLLA
514600940802     C**************************************************************************
514700940408     C     LEG2          BEGSR
514800940802     C*
514900940411     C                   MOVEL     'C'           WCAR              1
515000940408     C*
515100940408     C* 75 ON - NON RIENTRO NEL CARICAMENTO LEGAME BOLLA
515200940408     C                   SETON                                        75
515300940408     C* 94 OFF- DISABILITO IL CMD18
515400940408     C                   SETOFF                                       94
515500940408     C* CARICO DATI BOLLA
515600940408     C                   EXSR      BOLLA
515700940411     C*
515800940411     C* VISUALIZZA NUOVO LEGAME
515900940408     C* SE LA BOLLA E' UNA PRECEDENTE --> VISUALIZZO LA SEGUENTE
516000940713     C     V8CFPS        IFEQ      'P'
516100950316     C                   MOVEA     'SEGUEN.'     LBL(14)
516200940408     C                   ELSE
516300940408     C* SE LA BOLLA E' UNA SEGUENTE   --> VISUALIZZO LA PRECEDENTE
516400950316     C                   MOVEA     'PRECED.'     LBL(14)
516500940408     C                   ENDIF
516600950316     C                   MOVEA     COMLPN        LBL(21)
516700950316     C                   MOVEA     COMNRN        LBL(25)
516800950316     C                   MOVEA     COMNSN        LBL(28)
516900950316     C                   MOVEA     COMLAN        LBL(40)
517000950307     C                   MOVEA     *BLANKS       LBL(44)
517100940408     C                   MOVEA     LBL           LEGAME
517200940408     C*
517300940411     C* MOUVO I DATI DELLA LEGATA NEI CAMPI DELLA PARAM
517400940714     C                   MOVEL     ARBAAS        V2CAAS
517500940411     C                   MOVEL     ARBLNP        COMLNP
517600940714     C                   MOVEL     ARBNRS        V2CNRS
517700940714     C                   MOVEL     ARBNSP        V2CNSP
517800940408     C*
517900940408     C                   EXSR      VISUA2
518000940408     C*
518100941230     C                   MOVEL     'S'           WRIEM
518200940408     C                   ENDSR
518300940408     C*
518400940712      **************************************************************************
518500131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
518600940408      *     PER BOLLA NORMAL - CMD18 PREMUTO DA VISUAL -
518700091216      *     ricordarsi di aggiornare anche RIEPI2 per ogni modifica fatta qui
518800940712      **************************************************************************
518900940712     C     RIEPIL        BEGSR
519000940712      *
519100940712     C                   SETOFF                                       89
519200940712      *
519300940712      * EMETTO IL FORMATO VIDEO
519400940720     C                   MOVEL     'S'           WRIEM
519500940712     C     FOR07         TAG
519600940720     C     WRIEM         IFEQ      'S'
519700131107     C                   WRITE     LRI6T01
519800940720     C                   MOVEL     *BLANKS       WRIEM
519900940720     C                   ENDIF
520000021217      * Imposto la stringa comandi del 'giro' successivo
520100021217     C                   IF        *inKY = *on and $Loop > 1
520200021217     c                   Exsr      Ges_F24
520300021217     c                   eval      FD07 = FDxx
520400021217     c                   eval      F24D07 = F24Dxx
520500021217     C                   ENDIF
520600131107     C                   EXFMT     LRI6D07
520700070420     c                   setoff                                       28
520800070420     c                   clear                   v1cmsg
520900940712      *
521000131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
521100940712     C   KY*IN89         IFEQ      *ON
521200940712     C                   SETOFF                                       89
521300940712     C                   ELSE
521400940712     C                   SETON                                        89
521500940712     C                   ENDIF
521600940712     C   KY              GOTO      FOR07
521700940712      *
521800131108      * F3 - FINE LAVORO TORNO A SUBFILE
521900940714     C   KC              MOVEL     '2'           V2CFLG            1
522000940712     C   KC              GOTO      ENDRIE
522100940712      *
522200131108      * F18 - INTERROGA LEGAME BOLLA
522300940712     C   KS              DO
522400940712     C                   EXSR      CMD18
522500940712     C                   GOTO      FOR07
522600940712     C                   END
522700940712      *
522800940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE
522900940712     C                   EXSR      COMAND
523000940905     C     WCMD          IFEQ      'S'
523100940802     C     WF03          IFEQ      *BLANKS
523200070404     c
523300940802     C                   GOTO      FOR07
523400940802     C                   ELSE
523500940802     C                   MOVEL     '2'           V2CFLG
523600940802     C                   GOTO      ENDRIE
523700070404     C                   ENDIF
523800091215     c
523900070404     c                   else
524000070404     c
524100070404     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
524200070404     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
524300070404     c                   if        v3cgesd='?'
524400131120
524500070404     c                   exsr      VISUALAbi
524600070404     c                   clear                   v3cgesd
524700070412     c                   eval      wriem='S'
524800070404     c                   endif
524900131120     c
525000131120     c* controlla se immessi dei '?'
525100131120     c                   eval      wvideo='R'
525200131120     c                   exsr      CTR_puIn
525300091215     c
525400091215     c                   if        wriem='S'
525500091215     C                   GOTO      FOR07
525600091215     c                   endif
525700070404     c
525800940712     C                   END
525900940712      *
526000940712     C     ENDRIE        ENDSR
526100940712      **************************************************************************
526200131107      * Visualizzo il formato video di riepilogo della bolla (LRI6D07)
526300940408      *     PER LEGAME BOLLA - CMD18 PREMUTO DA VISUA2 -
526400940712      **************************************************************************
526500940712     C     RIEPI2        BEGSR
526600940712      *
526700940712     C                   SETOFF                                       89
526800940712      *
526900940712      * EMETTO IL FORMATO VIDEO
527000940809     C                   MOVEL     'S'           WRIEM
527100940712     C     FOR77         TAG
527200940809     C     WRIEM         IFEQ      'S'
527300131107     C                   WRITE     LRI6T01
527400940809     C                   MOVEL     *BLANKS       WRIEM
527500940809     C                   ENDIF
527600091216      * Imposto la stringa comandi del 'giro' successivo
527700091216     C                   IF        *inKY = *on and $Loop > 1
527800091216     c                   Exsr      Ges_F24
527900091216     c                   eval      FD07 = FDxx
528000091216     c                   eval      F24D07 = F24Dxx
528100091216     C                   ENDIF
528200091216     c
528300131107     C                   EXFMT     LRI6D07
528400070516     c                   setoff                                       28
528500070516     c                   clear                   v1cmsg
528600940408     C*
528700131108      * F3 - FINE LAVORO TORNO A SUBFILE
528800940714     C   KC              MOVEL     '2'           V2CFLG            1
528900940408     C*
529000940408     C  NKC              DO
529100940712      *
529200131108      * F24 - VISUALIZZAZIONE ALTRI TASTI FUNZIONALI
529300940712     C   KY*IN89         IFEQ      *ON
529400940712     C                   SETOFF                                       89
529500940712     C                   ELSE
529600940712     C                   SETON                                        89
529700940712     C                   ENDIF
529800940712     C   KY              GOTO      FOR77
529900940712      *
530000940712      *
530100131108      * F18 - INTERROGA LEGAME BOLLA
530200940712     C   KS              DO
530300940712     C                   EXSR      CMD18
530400940712     C                   GOTO      FOR77
530500940712     C                   END
530600940712      *
530700940905     C* VERIFICO SE PREMUTO UN TASTO FUNZIONE E, NEL CASO RIEMETTO
530800940712     C                   EXSR      COMAND
530900940905     C     WCMD          IFEQ      'S'
531000940712     C                   GOTO      FOR77
531100091216     c
531200091216     c                   else
531300091216     c
531400091216     c* Se nessun tasto funzionale premuto, vedo se '?' su altre abilitaz
531500091216     c* se immesso '?' nel dettaglio abilitazioni, richiamo pgm
531600091216     c                   if        v3cgesd='?'
531700131120
531800091216     c                   exsr      VISUALAbi
531900091216     c                   clear                   v3cgesd
532000091216     c                   eval      wriem='S'
532100091216     c                   endif
532200131120
532300131120     c* controlla se immessi dei '?'
532400131120     c                   eval      wvideo='R'
532500131120     c                   exsr      CTR_puIn
532600091216     c
532700091216     c                   if        wriem='S'
532800091216     C                   GOTO      FOR77
532900091216     c                   endif
533000091216     c
533100940712     C                   END
533200091216     C                   END
533300940712      *
533400940712     C                   ENDSR
533500940923      **************************************************************************
533600940923      *     CONTROLLI FORMATO VIDEO D04
533700940923      **************************************************************************
533800131120     C     CTR_puIn      BEGSR
533900110601     c*
534000110601     c* verifico se inserito '?' su email destinatario
534100110601     c                   if        v4cemw='?'
534200110601     c                   exsr      sr_emdest
534300110601     C                   MOVEL     'S'           WRIEM
534400110601     c                   endif
534500940923      *
534600091215      * Verifico se inserito '?' per visualizzazione beneficiario
534700091215     c                   if        vidben='?'
534800131120     c
534900091215     c                   exsr      sr_bencas
535000091215     C                   MOVEL     'S'           WRIEM
535100091215     c                   endif
535200131114     c
535300131114     c* Visualizzazione dati filiale
535400131114     c                   if        viddec_f='?'
535500131120
535600131114     c                   clear                   trulorfds
535700131114     c                   eval      iorffil=arblna
535800131114     c                   call      'TRULORFR'
535900131114     c                   parm                    kpjba
536000131114     c                   parm                    trulorfds
536100131114     c
536200131114     c                   clear                   viddec_f
536300131114     C                   MOVEL     'S'           WRIEM
536400131114     c                   endif
536500091215     c
536600940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' C/ASSEGNO
536700940923     C     V4IGCA        IFEQ      '?'
536800991027     C                   CLEAR                   DS7PQRS
536900941003     C                   MOVEL     'S'           DS7RIC
537000941003     C                   MOVEL     'V'           DS7GES
537100941003     C                   MOVEL     V4CGCA        DS7COD
537200991027     C                   MOVEL     DS7PQRS       KPJBU
537300960126     C                   CALL      'TRTB72R'
537400941003     C                   PARM                    KPJBA
537500940923     C                   MOVEL     *BLANKS       V4IGCA
537600940923     C                   MOVEL     'S'           WRIEM
537700940923     C                   ENDIF
537800940923     C*
537900940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' GIACENZA
538000940923     C     V4IGGA        IFEQ      '?'
538100991027     C                   CLEAR                   DS7PQRS
538200941003     C                   MOVEL     'S'           DS7RIC
538300941003     C                   MOVEL     'V'           DS7GES
538400941003     C                   MOVEL     V4CGGA        DS7COD
538500991027     C                   MOVEL     DS7PQRS       KPJBU
538600960126     C                   CALL      'TRTB69R'
538700941003     C                   PARM                    KPJBA
538800940923     C                   MOVEL     *BLANKS       V4IGGA
538900940923     C                   MOVEL     'S'           WRIEM
539000940923     C                   ENDIF
539100940923     C*
539200940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' CONSEGNA
539300940923     C     V4IGMA        IFEQ      '?'
539400991027     C                   CLEAR                   DS7PQRS
539500941003     C                   MOVEL     'S'           DS7RIC
539600941003     C                   MOVEL     'V'           DS7GES
539700941003     C                   MOVEL     V4CGMA        DS7COD
539800991027     C                   MOVEL     DS7PQRS       KPJBU
539900960126     C                   CALL      'TRTB70R'
540000941003     C                   PARM                    KPJBA
540100940923     C                   MOVEL     *BLANKS       V4IGMA
540200940923     C                   MOVEL     'S'           WRIEM
540300940923     C                   ENDIF
540400940923     C*
540500940923     C* VERIFICO SE INSERITO '?' SU PARTICOLARITA' VARIE
540600940923     C     V4IGVA        IFEQ      '?'
540700991027     C                   CLEAR                   DS7PQRS
540800941003     C                   MOVEL     'S'           DS7RIC
540900941003     C                   MOVEL     'V'           DS7GES
541000941003     C                   MOVEL     V4CGVA        DS7COD
541100991027     C                   MOVEL     DS7PQRS       KPJBU
541200960126     C                   CALL      'TRTB73R'
541300941003     C                   PARM                    KPJBA
541400940923     C                   MOVEL     *BLANKS       V4IGVA
541500940923     C                   MOVEL     'S'           WRIEM
541600940923     C                   ENDIF
541700940923      *
541800940923     C                   ENDSR
541900940321      **************************************************************************
542000940713      *     R O U T I N E      I N I Z I A L E
542100940713      **************************************************************************
542200940713     C     *INZSR        BEGSR
542300940713      *
542400940713     C     KARB          KLIST
542500940714     C                   KFLD                    V2CAAS
542600940713     C                   KFLD                    COMLNP            3 0
542700940714     C                   KFLD                    V2CNRS
542800940714     C                   KFLD                    V2CNSP
542900940713     C     KARB2         KLIST
543000940713     C                   KFLD                    ARBAAS
543100940713     C                   KFLD                    ARBLNP
543200940713     C                   KFLD                    ARBNRS
543300940713     C                   KFLD                    ARBNSP
543400940713     C*
543500940713     C     KTAB1         KLIST
543600940713     C                   KFLD                    CODUT
543700940713     C                   KFLD                    COD
543800940713     C                   KFLD                    KEY
543900940713     C     KTAB2         KLIST
544000940713     C                   KFLD                    CODUT
544100940713     C                   KFLD                    COD
544200940713     C*
544300940907     C     KLBL1         KLIST
544400940713     C                   KFLD                    LBLAAN
544500940713     C                   KFLD                    LBLLPN
544600940713     C                   KFLD                    LBLNRN
544700940713     C                   KFLD                    LBLNSN
544800940907     C     KLBL3         KLIST
544900940907     C                   KFLD                    KAAS
545000940907     C                   KFLD                    KLNP
545100940907     C                   KFLD                    KNRS
545200940907     C                   KFLD                    KNSP
545300940713     C*
545400940719     C     KAR41         KLIST
545500940719     C                   KFLD                    KAAS
545600940719     C                   KFLD                    KLNP
545700940719     C                   KFLD                    KNRS
545800940719     C                   KFLD                    KNSP
545900940802     C                   KFLD                    KTRC
546000951220     C     KAR42         KLIST
546100951220     C                   KFLD                    KAAS
546200951220     C                   KFLD                    KLNP
546300951220     C                   KFLD                    KNRS
546400951220     C                   KFLD                    KNSP
546500021213     C*
546600021213     C     KAR51         KLIST
546700021213     C                   KFLD                    KAAS
546800021213     C                   KFLD                    KLNP
546900021213     C                   KFLD                    KNRS
547000021213     C                   KFLD                    KNSP
547100021213     C                   KFLD                    KTRd              3
547200940719     C*
547300991026     C     KAR61         KLIST
547400940713     C                   KFLD                    KAAS
547500940713     C                   KFLD                    KLNP
547600940713     C                   KFLD                    KNRS
547700940713     C                   KFLD                    KNSP
547800991026     C                   KFLD                    KTRC
547900991026     C*
548000051128     C     Ksped         KLIST
548100991026     C                   KFLD                    KAAS
548200991026     C                   KFLD                    KLNP
548300991026     C                   KFLD                    KNRS
548400991026     C                   KFLD                    KNSP
548500180124     C     Kars          KLIST
548600180124     C                   KFLD                    arwfls
548700180124     C                   KFLD                    arwlna
548800180124     C                   KFLD                    arwnrs
548900180124     C                   KFLD                    arwnsc
549000180124     C                   KFLD                    Karstrc
549100160504     C*
549200990715     C     KGCA1         KLIST
549300941202     C                   KFLD                    KAAS
549400940713     C                   KFLD                    KLNP
549500940713     C                   KFLD                    KNRS
549600940713     C                   KFLD                    KNSP
549700940713     C*
549800990805     C     KEVB1         KLIST
549900940713     C                   KFLD                    KAAS$
550000940713     C                   KFLD                    KLNP
550100940713     C                   KFLD                    KNRS
550200940713     C                   KFLD                    KNSP
550300131114     C     Kcet          KLIST
550400140224     C                   KFLD                    pctfgs
550500131114     C                   KFLD                    pctNDC
550600131114     C                   KFLD                    arbpdc
550700131114     C                   KFLD                    TTRD              3
550800131114     C                   KFLD                    arbAAS
550900131114     C                   KFLD                    arbLNP
551000131114     C                   KFLD                    arbNRS
551100131114     C                   KFLD                    arbNSP
551200950324     C*
551300020906     C     KFVV1         KLIST
551400950324     C                   KFLD                    WNPG
551500950327     C                   KFLD                    WNDC
551600020906     c                   Kfld                    Wfgs
551700131119     C     KDST          KLIST
551800131119     C                   KFLD                    WNPG
551900131119     C                   KFLD                    kndc6
552000131119     c                   Kfld                    Wfgs
552100021203
552200021203     C     Kfapd         klist
552300021203     C                   kfld                    kpdtip
552400021203     C                   kfld                    kpdpdr
552500131108     c
552600131108     c     kbrv          klist
552700131108     C                   KFLD                    WNPG
552800131108     C                   KFLD                    knfv
552900131108     c                   Kfld                    Wfgs
553000131108     c                   Kfld                    artfls
553100131108     c                   Kfld                    artlna
553200131108     c                   Kfld                    artnrs
553300131108     c                   Kfld                    artnsc
553400040218      *
553500160408      * File FITGD32L
553600040218     c     K02TGD02      klist
553700040218     c                   kfld                    TGDtor
553800040218     c                   kfld                    TGDogg
553900030527     C*
554000150617
554100150617     C     KANM          KLIST
554200150617     C                   KFLD                    KAAS
554300150617     C                   KFLD                    KLNP
554400150617     C                   KFLD                    KNRS
554500150617     C                   KFLD                    KNSP
554600150617     C                   KFLD                    KCAA
554700161207      * Fnorm
554800161207     c     korm          klist
554900161207     c                   kfld                    §b4poe
555000161207     c                   kfld                    §b4nsr
555100161207     c                   kfld                    §b4nor
555200161207     c                   kfld                    §b4nrv
555300940713     C*
555400940713     C*-------------------------------------------------------------------------
555500940713     C*
555600991222     C     *LIKE         DEFINE    ARBAAS        WAAS
555700991027     C     *LIKE         DEFINE    ARBLNP        WLNP
555800991027     C     *LIKE         DEFINE    ARBNRS        WNRS
555900991027     C     *LIKE         DEFINE    ARBNSP        WNSP
556000991027     C     *LIKE         DEFINE    FVVNPG        WNPG
556100991027     C     *LIKE         DEFINE    FVVNFV        WNDC
556200131108     C     *LIKE         DEFINE    brvNFV        knfv
556300991027     C     *LIKE         DEFINE    COMC08        SAVC08
556400971219     C     *LIKE         DEFINE    ARBAAS        KAAS
556500990805     C     *LIKE         DEFINE    EVBAAS        KAAS$
556600940713     C     *LIKE         DEFINE    ARBLNP        KLNP
556700940713     C     *LIKE         DEFINE    ARBNRS        KNRS
556800940713     C     *LIKE         DEFINE    ARBNSP        KNSP
556900961112     C     *LIKE         DEFINE    ARBFBR        WFBR
557000940713     C     *LIKE         DEFINE    TBLCOD        COD
557100940713     C     *LIKE         DEFINE    TBLKEY        KEY
557200940713     C     *LIKE         DEFINE    ARBAAS        $AAS
557300940713     C     *LIKE         DEFINE    ARBMGS        $MGS
557400940719     C     *LIKE         DEFINE    AR4TRC        KTRC
557500940907     C     *LIKE         DEFINE    LBLAAN        COMAAN
557600971223     C     *LIKE         DEFINE    AR4TRC        WTRC
557700060130     C*-------------------------------------------------------------------------
557800060130     c
557900060130      *
558000060130     c     *dtaara       define    §azute        azuteds
558100060130     c     *dtaara       define    §datiute      ddatiute
558200060130     c                   in(E)     *dtaara
558300060130    1c                   If        %error  or RSUT = *blanks
558400060130     c                   Clear                   Tibs34ds
558500060130     c                   Call      'TIBS34R'
558600060130     c                   Parm                    Tibs34ds
558700060130     c                   In        *dtaara
558800060130    1c                   EndIf
558900060130     c
559000971223     C*
559100971223     C* IMPOSTAZIONE CAMPI FISSI
559200940907     C                   MOVEL     *ZERO         COMLPN            3
559300940907     C                   MOVEL     *ZERO         COMNRN            2
559400940907     C                   MOVEL     *ZERO         COMNSN            7
559500950307     C                   MOVEL     *ZERO         COMLAN            3
559600950324     C                   Z-ADD     4             WNPG
559700940713      *
559800940713     C                   Z-ADD     *ZERO         NR2               4 0
559900940713     C                   Z-ADD     *ZERO         NR6               4 0
560000940713     C                   Z-ADD     *ZERO         NR8               4 0
560100940802     C                   MOVEL     *BLANKS       WF03              1
560200070403     c
560300070403     c* Verifico se profilo utente ha GEO attiva
560400131205     c* Per EDP sempre anche prr RAC999
560500131205     c                   if        %subst(knmus:1:3)='EDP' or
560600131205     c                             %subst(knmus:1:6)='RAC999'
560700070410     c                   eval      dutgeot='S'
560800070410     c                   else
560900070403     c     dutpou        chain     azorg01l
561000070403     c                   if        %found(azorg01l)
561100070403     c                   movel     orgde8        og148
561200070403     c                   else
561300070403     c                   clear                   og148
561400070403     c                   endif
561500070410     c
561600070524     c                   if        §oggeot<>' ' or §ogtlls<>' '
561700070524     c                   movel     'S'           dutgeot           1
561800070410     c                   endif
561900070524     c                   endif
562000131007
562100140319     c* Tasto funz orari servizi sempre abilitato
562200140319     c                   eval      wabif9='S'
562300131007
562400940713      *
562500940713     C                   ENDSR
562600971223     C**
562700110601      **************************************************************************
562800110601      *   emissione window email destinatario
562900110601      **************************************************************************
563000110601     C     sr_emdest     BEGSR
563100110601     c                   clear                   v4cemw
563200110601     c
563300110601     c                   eval      ilv79TLA='L'
563400110601     c                   eval      ilv79Aas=arbaas
563500110601     c                   eval      ilv79Lnp=arblnp
563600110601     c                   eval      ilv79nrs=arbnrs
563700110601     c                   eval      ilv79nsp=arbnsp
563800110601     c                   call      'FNLV79R'
563900110601     c                   parm                    fnlv79ds
564000110601     c
564100110601     c                   endsr
564200091214      **************************************************************************
564300131114      *   Emissione del Beneficiario C/Ass
564400091214      **************************************************************************
564500091214     C     sr_bencas     BEGSR
564600091215     c                   clear                   vidben
564700091214     c                   clear                   wdaben
564800091214     c                   clear                   wdasrsc
564900091214     c                   clear                   wdascdi
565000091216     c                   clear                   trulintds
565100091216     c
565200091216     c                   eval      iinttip='B'
565300091216     c                   eval      iinttic=vidtic
565400091216     c                   eval      iintrscm=v4crsm
565500091216     c                   eval      iintrsco=vidrmo
565600091216     c                   eval      iintCDI =w_cdi
565700091216     c                   call      'TRULINTR'
565800091216     c                   parm                    trulintds
565900091216     c
566000091216     c                   movel     ointrsci      wdasrsc
566100091216     c
566200091216     c                   select
566300091216     c                   when      ointtipi='O'
566400091216     c                   eval      wdaben='MITTENTE  ORIGINALE'
566500091216     c
566600091216     c                   when      ointtipi='R'
566700091216     c                   eval      wdascdi=w_cdi
566800091216     c                   eval      wdaben=' O R D I N A N T E'
566900091216     c
567000091216     c                   when      ointtipi='M'
567100091216     c                   if        v3cksc>0
567200091216     c                   movel     v3cksc        wdascdi
567300091216     c                   endif
567400091216     c                   eval      wdaben='  M I T T E N T E'
567500091216     c                   ENDSL
567600091214     c
567700131107     c                   exfmt     LRI6wdas
567800091214     c                   endsr
567900130830      **************************************************************************
568000130830      *   window per visualizzare gli orari servizi
568100130830      **************************************************************************
568200130830     C     sr_OrariSr    BEGSR
568300130920
568400140110     c                   clear                   trulorSds
568500140110     c                   eval       s_IOREtfp = ARBtfp
568600140110     c                   eval       s_IOREtfa = ARBtfa
568700140110     c                   eval       s_IOREdti = ARBdti
568800140110     c                   eval       s_IOREhti = ARBhti
568900140110     c                   eval       s_IOREdcr = ARBdcr
569000140110     c                   eval       s_IOREhcr = ARBhcr
569100140110     c                   eval       s_IOREtcr = ARBtcr
569200140110     c                   eval       s_IOREDEI = s_ARBdei
569300140110     c                   eval       s_IOREOEI = s_ARBoei
569400140110     c                   eval       s_IOREfil = ARBlna
569500140110     c                   eval       s_IOREcap = ARBcad
569600140110     c                   eval       s_IOREloc = ARBlod
569700140110     c                   eval       s_IOREnar = ARBnzd
569800150729     c                   clear                   diore01
569900150729     c                   movel     s_arbttc      §ioretrazC
570000150729     c                   movel     s_arbtcc      §ioreconsC
570100150729     c                   eval       s_IOREflo = diore01
570200140113     c* Data di Riferimento: data consegna se consegnata
570300140113     c                   select
570400140113     c                   when      arbdcm>0 and arbica<>' ' and arbica<>'R'
570500140113     c                             and arbicc<>' ' and arbicc<>'R'
570600150617     c                             and w_tbo='A' or (w_tbo='P' and arbdcm>0)
570700140113     c                   eval       s_ioredta=arbdcm
570800140113     c* per ritorno all'incasso data del giorno
570900140113     c                   when      arbdcm>0 and arbddc>0
571000140113     C                   time                    wrkoggiiso
571100140113     C     *iso          move      wrkoggiiso    s_ioredta
571200140113     c                   other
571300140113     c* altrimenti data prevista consegna se > = oggi
571400140113     C                   time                    wrkoggiiso
571500140113     C     *iso          move      wrkoggiiso    s_ioredta
571600140113     c* se < di oggi, imposto oggi
571700140113     c                   if        amgdce>s_ioredta
571800140113     c                   eval       s_ioredta=amgdce
571900140113     c                   endif
572000140113     c                   endsl
572100140113     c
572200140110     c                   eval       s_IOREtser = 'C'
572300140409     c                   eval       s_IOREDDC=arbddc
572400140409     c                   eval       s_IOREnDC=arbndc
572500140409     c
572600150617     c****               IF         *in19 or
572700150617     c                   if          w_tbo='A'
572800150316     c                   if          ARBfbc = *blanks and
572900140110     c                               ARBddc > 0 and
573000140110     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
573100150316     c                               arbica='R' or arbicc=' ' or arbicc='R')
573200140110     c                   eval       s_IOREcons = 'S'
573300140225     c* Orario presunto ritiro/consegna
573400140225     c                   move      arbndc        pctndc
573500140225     c                   move      arbifp        pctfgs
573600140225     c                   move      'COR'         ttrd
573700140225     c     kcet          setgt     fipct02l
573800140225     c     kcet          readpe    fipct02l
573900140310     c                   if        not %eof(fipct02l)
574000140225     c                   movel     pctdati       fipctcords
574100140307     c                   eval      s_ioreorp=§PCTORASTI
574200140225     c                   endif
574300140225     c
574400140110     c                   ENDIF
574500150617     c                   ENDIF
574600140225     c
574700140110     c                   eval       s_IOREtsp = ARBtsp
574800130920
574900140325     c                   eval      s_iorecol=36
575000150611     c                   if        W_video='N'
575100150611     c* località normalizzata
575200150611     C                   MOVEL     ARBAAS        KAAS
575300150611     C                   MOVEL     ARBLNP        KLNP
575400150611     C                   MOVEL     ARBNRS        KNRS
575500150611     C                   MOVEL     ARBNSP        KNSP
575600150611     C                   MOVEL     'X'           KTRC
575700150611     c     kar41         chain     fiar401l
575800150611     c                   if        %found(fiar401l)
575900150611     c                   eval      OOR3LOC_N=ar4not
576000150611     c                   else
576100150611     c                   eval      OOR3LOC_N=arblod
576200150611     c                   endif
576300150611     c
576400150611     c                   call      'TRULORSR'
576500150611     c                   parm                    kpjba
576600150611     c                   parm                    trulorSds
576700150611     c                   parm                    trulor2ds
576800150611     c                   parm                    trulor3ds
576900150611     c                   else
577000140110     c                   call      'TRULORSR'
577100130920     c                   parm                    kpjba
577200140110     c                   parm                    trulorSds
577300150611     c                   endif
577400130830     c
577500131111     c                   ENDSR
577600131111
577700940713      **************************************************************************
577800941014**        12345678901234567890123456789012345678901234567890
577900950316LEGATA BOLLA SEGUEN.XXX/XX/XXXXXXX PER XXX- F18=Visualizza
578000930413**
578100950316LEGATA A PIU' BOLLE                       - F18=Visualizza
578200920326**
578300950316 Non Presente
578400950316 F18=Visualizza
578500940907Vedi in Int.Par
578600940907Vedi in Int.Arr
578700941011Vedi Arch.Stor.
578800940907**
578900940907Spedizione non Presente
579000941230
579100940907Vedi in Int.Bolle Part.
579200940907Vedi in Int.Bolle Arr.
579300941011Vedi in Archiv. Storico
579400940713**  MESSAGGI DI ERRORE VIDEO  **************************************************
579500160505Stampa LDV effettuata ...                                                     01
579600160505Stampa LDV al momento non effettuabile: ritentare o telefonare in Sede        02
579700160505Assegnato non tassato: stampa LdV non effettuabile                            03
579800070426**  cmA4f6
579900111221OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
580000111221USRDTA('Rist.Bolle') SECURE(*YES)
580100070426**  cmA5f6
580200070426OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
580300111221             USRDTA('Rist.Bolle') SECURE(*YES)
580400040218** TF                                                                        *
580500070420F1=Agg.Rich#                                                                    1
580600070411** TF01B                                                                     *
580700070411F1=SCELTA BOLLA#                                                                1
580800021216** TF                                                                        *
580900021218F2=C.A.#                                                                        2
581000021216** TF                                                                        *
581100021216F4=Tass2#                                                                       4
581200021216** TF                                                                        *
581300021216F5=Eventi#                                                                      5
581400070420** TF                                                                        *
581500070420F6=StampaLDV#                                                                   5
581600021216** TF                                                                        *
581700021217F7=Anomalie#                                                                    7
581800021216** TF                                                                        *
581900160628F8=Orm#                                                                         8
582000130830** TF                                                                        *
582100130830F9=OrariSr#                                                                     9
582200021216** TF                                                                        *
582300070420F10=Variaz#                                                                     10
582400021216** TF                                                                        *
582500030121F11=NoteS/B#                                                                    11
582600021217** TF                                                                        *
582700021217F12=Ritorno#                                                                    11
582800021216** TF                                                                        *
582900070420F13=Tassaz#                                                                     13
583000021217** TF                                                                        *
583100021216F14=Giacenza#                                                                   14
583200040218** TF                                                                        *
583300070420F16=Gest.Rich#                                                                  16
583400070404** TF                                                                        *
583500140311F17=NoteAut#                                                                    16
583600021216** TF                                                                        *
583700021216F19=Spunte#                                                                     19
583800021216** TF                                                                        *
583900070420F20=Affid#                                                                      19
584000021216** TF                                                                        *
584100070420F21=DetColli#                                                                   19
584200110928** TF                                                                        *
584300110930F22=PerfectOrder#                                                               19
584400160628** TF                                                                        *
584500160628F23=Discord#                                                                    19
