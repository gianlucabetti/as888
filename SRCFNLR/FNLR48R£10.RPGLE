000100030612     H DECEDIT('0,') DATEDIT(*DMY.)
000110941014     H* FNLR48R *----------------------------------------------------*
000120950208     H*         - MANUTENZIONE BOLLE
000130051118     H*--------------------------------------------------------------*
000140051118      * COMPILARE CON OVRDBF FILE(TITA430C) TOFILE(GAITRAGRPS/TITA430C)
000150060308      *
000160060308      * NOTE RIGUARDANTI l'UTILIZZO DEL PGM FNLV85R(GESTIONE MOTIVO VARIAZ-
000170060308      * IONE BOLLA):
000180060308      * fnlv85r richiamato all'interno di questo pgm per due volte in
000190070321      * due diverse routine:
000200060308      * - in una routine (GES_MTV) viene richiamato per la richiesta e/o il
000210060308      *   controllo della motivazione (emissione di window per la richiesta
000220060308      *   del motivo in base al contenuto di D48FFR)
000230060308      * - nell'altra routine (STO_MTV) viene richiamato per la registrazione
000240060308      *   del motivo variazione nel file FIAR500F.
000250060308      * In entrambi i casi il pgm viene richiamato con tipo lancio = blanks
000260060308      * (chiusura in return anzichè in LR) affinchè il motivo rimanga in
000270060308      * memoria:
000280060308      * - fino al momento della registrazione della variazione su data base
000290060308      *   per variazioni singole
000300060308      *   fino al momento della registrazione dell'ultima variazione su
000310060308      *   data base in caso di variazioni multiple.
000320060308      *   All'interno della stessa chiamata di fnlr48r può infatti accadere
000330060308      *   che una variazione comporti l'esecuzione automatica di un'altra
000340060308      *   variazione e così via...
000350060308      *   Se una o più di queste variazioni richiede il motivo questo viene
000360060308      *   richiesto/controllato solo la prima volta mentre viene memorizzato
000370060308      *   su data base per tutte le variazioni che lo richiedono.
000380000000     H*--------------------------------------------------------------*
000390030612     FFNLR48D   CF   E             WORKSTN USROPN   SFILE(LR48S03:NRR)
000400030612      * BOLLE ARRIVI
000410040913     FFNARB01L  UF   E           K DISK
000420060222     FFiAR401L  UF A E           K DISK
000430051118     FFiAR901L  UF A E           K DISK
000440030612      * BOLLE PARTENZE
000450070320     FFNBLP01L  UF   E           K DISK    prefix(ARB:3)
000460030612      * BOLLE PARTENZE E ARRIVI:TASSAZIONE
000470990930     FFIAR601L  UF A E           K DISK
000480990930     FFIAR701L  UF A E           K DISK
000490130410     F*FIARI01L  IF   E           K DISK
000500160318     FFIARg01L  UF   E           K DISK
000510070308     FFIARP01L  uF a E           K DISK
000520030124      * Estensione bolle
000530030124     fFiar501l  uf a e           k Disk
000540041020     f* Gestione rientri autotrasportatori
000550041020     ffiqdt01l  uf   e           k disk
000560030612      * Conteggi autotrasportatori
000570011126     Ffiftt01L  IF   E           K DISK
000580041008     Ffiftd04L  UF   E           K DISK    RENAME(fiftd000:fiftd004)
000590041008     Ffiftd05L  UF   E           K DISK    RENAME(fiftd000:fiftd005)
000600030612     Ffiftd01L  IF   E           K DISK    RENAME(fiftd000:fiftd001)
000610030612     Ffifst01L  IF   E           K DISK    RENAME(fiftt000:fifst000)
000620041008     Ffifsd04L  UF   E           K DISK    RENAME(fiftd000:fifsd004)
000630041008     Ffifsd05L  UF   E           K DISK    RENAME(fiftd000:fifsd005)
000640030612     Ffifsd01L  IF   E           K DISK    RENAME(fiftd000:fifsd001)
000650030612      *
000660900509     FAZORG01L  IF   E           K DISK
000670910429     FTABEL00F  IF   E           K DISK
000680050331     fTigcp01l  if   e           k Disk
000690050331     FTigcp21L  IF   E           K DISK    RENAME(TIGCP000:TIGCP21)
000700911209     FCNACO00F  IF   E           K DISK
000710941227     FCNIND00F  IF   E           K DISK
000720080701     F****FNDCT01L  IF   E           K DISK
000730990127     FTNTAM01L  IF   E           K DISK
000740990818     FTITPT01L  IF   E           K DISK
000750950208     FFNCLS01L  IF   E           K DISK
000760020911     FFNFVV01L  IF   E           K DISK
000770980105     FFNLBL01L  IF   E           K DISK
000780141022     FFNLBL02L  IF   E           K DISK    rename(fnlbl000:fnlbl02)
000790070309     FFNevb05l  IF   E           K DISK
000800070309     f
000810941014     FFNARBD0F  O  A E             DISK
000820130927     FFNARBN0F  O  A E             DISK
000830990930     FFIARBT0F  O  A E             DISK
000840990930     FFIARBU0F  O  A E             DISK
000850941014     FFNARBK0F  O  A E             DISK
000860941014     FFNARBG0F  O  A E             DISK
000870941227     FFNARBM0F  O  A E             DISK
000880170116     F*FNARBD0T  O  A E             DISK    USROPN  RENAME(FNARBD00:FNARBDTT)
000890170116     F*FIARBT0T  O  A E             DISK    USROPN  RENAME(FIARBT00:FIARBTTT)
000900170116     F*FIARBU0T  O  A E             DISK    USROPN  RENAME(FIARBU00:FIARBUTT)
000910170116     F*FNARBK0T  O  A E             DISK    USROPN  RENAME(FNARBK00:FNARBKTT)
000920170116     F*FNARBM0T  O  A E             DISK    USROPN  RENAME(FNARBM00:FNARBMTT)
000930170116     F*FNARBG0T  O  A E             DISK    USROPN  RENAME(FNARBG00:FNARBGTT)
000940170116     FFNARBD2T  O  A E             DISK    RENAME(FNARBD00:FNARBDTT)
000950170116     FFIARBT2T  O  A E             DISK    RENAME(FIARBT00:FIARBTTT)
000960170116     FFIARBU2T  O  A E             DISK    RENAME(FIARBU00:FIARBUTT)
000970170116     FFNARBK2T  O  A E             DISK    RENAME(FNARBK00:FNARBKTT)
000980170116     FFNARBM2T  O  A E             DISK    RENAME(FNARBM00:FNARBMTT)
000990170116     FFNARBG2T  O  A E             DISK    RENAME(FNARBG00:FNARBGTT)
000991170426     FFNFTA00F  O    E             DISK
001000030722
001010030722     FAZCLN01L  IF   E           K DISK
001020141020     FTIIDC02L  IF A E           K DISK
001030050601      * Bolle sede - Estensione
001040050601     fTita430c  if   e           k Disk    UsrOpn
001050151019     ffiar531c  uf a e           k Disk    UsrOpn infds(infdsar5)
001060151016     f                                     rename(fiar5000:fiar500S)
001070151016     f                                     rename(fiar5p00:fiar5p0S)
001080151019     fTitas30c  if   e           k disk    usropn infds(infdstitas)
001090151016     f                                     prefix(S_)
001100011120
001110160225     d FedD_ksc        s                   like(oc7kscc)
001120160225     d FedD_ctr        s              3
001130160225     d FedM_ksc        s                   like(oc7kscc)
001140160225     d FedM_ctr        s              3
001150160225     d
001160160225     d normlod         s                   like(o95loc)
001170150203     d bollacpi        s                   like(arbcpi)
001180071025     d bollaFGS        s                   like(D48FGS)
001190070319     d sav_arbias      s                   like(vidias)
001200041027     d sav_arbvas      s                   like(vidvas)
001210041027     d sav_arbqft      s                   like(vidqft)
001220041027     d sav_arbvmd      s                   like(vidvmd)
001230041027     d sav_arbvad      s                   like(vidvad)
001240131016     d bol_vidias      s                   like(vidias)
001250131016     d bol_vidvas      s                   like(vidvas)
001260131016     d bol_vidvmd      s                   like(vidvmd)
001270131016     d bol_vidvad      s                   like(vidvad)
001280041027     d sav_arbctr      s                   like(vidctr)
001290020327     d sav_dsftd       s                   like(dsftd)
001300011205     d sav1_vidias     s                   like(vidias)
001310011205     d sav1_vidvmd     s                   like(vidvmd)
001320011205     d sav1_vidcas     s                   like(vidcas)
001330020305     d deseftw         s                   like(§15eft)
001340020305     d desefdw         s                   like(§15efd)
001350020305     d deseffw         s                   like(§15eff)
001360020305     d lnpntw          s                   like(§ogntw)
001370020318     d clintw          s                   like(§ogntw)
001380020305     d lnantw          s                   like(§ogntw)
001390020531     d wdisag          s                   like(ot0dos)
001400070906     d wdisagdest      s                   like(ot0dos)
001410020603     d w48f12          s                   like(d48f12)
001420060301     d w48mct          s                   like(d48mct)
001430020911     d Kfgs            s                   Like(FvvFgs)
001440030612     d kAr5Trd         s                   Like(Ar5Trd)
001450050601     d kta4Trc         s                   Like(ta4trc)
001460021121     d Wctr            s                   Like(VidCtr)
001470040122     d va2va2          s                   Like(Ar6Va1)
001480040122     d va2va3          s                   Like(Ar6Va1)
001490040122     d va2va4          s                   Like(Ar6Va1)
001500041130     d va2va5          s                   Like(Ar6Va1)
001510041130     d va2va6          s                   Like(Ar6Va1)
001520041007     d OLDFFD          s                   Like(Arbffd)
001530041008     d OLDVLC          s                   Like(Arbvlc)
001540050202     d SAVCTR          s                   Like(vidctr)
001550070308     d SAVDCR          s                   Like(comdcr)
001560070308     d SAVTCR          s                   Like(vidtcr)
001570070308     d SAVHCR          s                   Like(vidhcr)
001580070308     d bollaDCR        s                   Like(comdcr)
001590070308     d bollaTCR        s                   Like(vidtcr)
001600070308     d bollaHCR        s                   Like(vidhcr)
001610070308     d bollaFBC        s                   Like(arbfbc)
001620071025     d ar4not_P        s                   Like(ar4not)
001630071025     d sav11pid        s                   Like(v11pid)
001640030612      *
001650030612     d DueCtr          s              2  0
001660070320     d TipoEla         s              1
001670030122     d wI0             s              1
001680030123     d wfpf            s              1
001690070312     d xx              s              3  0
001700090526     d yy              s              3  0
001710030612     d $mbr            s              6
001720030612     d w014a           s             14
001730030612     d wdiffe          s             13  3
001740030612     d w0173           s             17  3
001750030612     d tot_importi     s             17  3
001760100120     d xforzait        s              1
001770100120     d WVariavol       s              1
001780120705     d WVariavolEXP    s              1
001790091106     d Aggiobol        s              1
001800071026     d forzacdf        s              1
001810071025     d forzapid        s              1
001820071025     d forzapid2       s              1
001830071025     d forzaias        s              1
001840080625     d forzaias2       s              1
001850030612     d forzavmd        s              1
001860030612     d forzacas        s              1
001870030709     d flgsf20         s              1
001880030711     d wdataiso        s               d   datfmt(*iso)
001890060201     d wdataLimIas     s               d   datfmt(*iso)
001900030711     d wdataoggi       s               d   datfmt(*iso)
001910030711     d miapsw          s             10
001920030711     d wokpsw          s              1
001930040123     d war72           s              1
001940040122     d contasv         s              2  0
001950040122     d totimv          s                   Like(Ar6Imv)
001960040122     d wesvar1         s              1
001970040122     d wvar3ai         s              1
001980040224     d wffd            s                   Like(ArbFfd)
001990040224     d waggar5         s              1
002000040224     d wCA             s              1
002010040324     d wgiac           s              1
002020040225     d warbdcr         s                   Like(§Ar5gDcr)
002030040316     d wgcfas          s                   Like(GcpFas)
002040040324     d wgcded          s                   Like(GcpDed)
002050040324     d wgcddm          s                   Like(GcpDdm)
002060040608     d wokscr          s              1    inz('0')
002070080416     d wcmitt          s              7
002080080416     d wsflagga_C      s              1
002090110629     d wemd            s              1
002100011205
002110020715     d tbl             s              2    dim(20)                              sk tbl con ass.
002120090526     d ADRs            s              3    dim(50)                              sk tbl con ass.
002130090526     d ADRc            s              3    dim(50)                              sk tbl con ass.
002140170116     D*SOVR            S             48    DIM(1)                               DI COMODO SED
002150170116     D*SALC            S             48    DIM(1)                               DI COMODO SED
002160170116     D*SDLC            S             48    DIM(1)                               DI COMODO SED
002170020805     D PDLC            S             48    DIM(1)                               DI COMODO PAR
002180170116     D*C20             S              1    DIM(48) CTDATA PERRCD(48)
002190170116     D*C21             S              1    DIM(48) CTDATA PERRCD(48)
002200170116     D*C22             S              1    DIM(48) CTDATA PERRCD(48)
002210930507     D GGG             S              1    DIM(8) CTDATA PERRCD(8)              GIORNI CHIUSUR
002220151016     d cmdt            s             60    dim(03) ctdata perrcd(1)
002230170130     D MSG             S             78    DIM(165) CTDATA PERRCD(1)
002240041027     d*cmd             s              1    dim(45) ctdata perrcd(45)            ovrdbf
002250030612
002260990930     D* VARIE DELLA DTASV
002270990930     D* VARIE DELLA DARBT
002280941213     D* VARIE DI COMODO
002290941213     D WSV             S              1    DIM(21)                              SIGLA  VARIE
002300990930     D WVA             S             11  3 DIM(21)                              VALORE VARIE
002310941215     D SKI             S              1    DIM(15)                              COMODO
002320011106     D SKII            S              1    DIM(18)                              COMODO
002330941215     D K78             S              1    DIM(78)                              COMODO
002340990930     D*
002350150119     D SVA             S              1    DIM(100)                             VARIE IN TOT.I
002360070906     d
002370070906     d* Skiere di riesecuzione pgm
002380070906     D kdati           S            250    DIM(10)                              VARIE IN TOT.I
002390070906     D kcvb            S              2    DIM(10)                              VARIE IN TOT.I
002400070907     D ktrc            S              2    DIM(10)                              VARIE IN TOT.I
002410070911     D kffr            S              1    DIM(10)                              VARIE IN TOT.I
002420070911     D kf12            S              1    DIM(10)                              VARIE IN TOT.I
002430150414
002440150414      * filiali abilitate a cons.part "Z"=Expo
002450151119     d*fil_expo        s              3    dim(85)
002460030722
002470030722     D ktfp            S                   LIKE(CLNtfp)
002480030722     D ktfa            S                   LIKE(CLNtfa)
002490030722     D kann            S                   LIKE(CLNann)
002500030722     D kmes            S                   LIKE(CLNmes)
002510050601     d wlib            s             10
002520050601     d commt           s             80
002530050601     d lenght          s             15  5 inz(80)
002540120403     d erra            s              1
002550120403     d errp            s              1
002560120404     d wfatfil         s              2
002570120404     d war6agg         s              1
002580120411     d inx             s              2  0
002590120416     d savcpi_a        s                   like(arbcpi)
002600120418     d savcpi_p        s                   like(arbcpi)
002610120418     d savksc_a        s                   like(arbksc)
002620120418     d savctr_a        s                   like(arbctr)
002630120418     d savksc_p        s                   like(arbksc)
002640120418     d savctr_p        s                   like(arbctr)
002650030722
002660151019     d infdsar5        ds
002670151019     d   recname             261    270
002680151019     d infdstitas      ds
002690151019     d   recnamet            261    270
002700030722     D clnmat          DS
002710030722     D  mat                    1     31    dim(31)
002720030722     D clnpom          DS
002730030722     D  pom                    1     31    dim(31)
002740030722
002750030722     D                 DS
002760030722     D  ds_giorno              7      8  0
002770030722     D  ds_mese                5      6  0
002780030722     D  ds_anno                1      4  0
002790030722     D  ds_data                1      8  0
002800030722
002810941213     D*
002820941213     D* CODICI TARIFFA CON CUI TASSARE GLI ASSEGNATI 9999 - TAB 7W
002830990702     D* TARIFFE PER TASSAZIONE PREPAGATI
002840941222     D*-------------_____------------_______-------------_______-----
002850930505     D                 DS
002860941014     D  ARBAAS                 1      4  0
002870941014     D  ARBMGS                 5      8  0
002880941014     D  ARBDSP                 1      8  0
002890950114     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
002900931210     D PARAM3          DS
002910931210     D  PA3PDR                 1      7  0
002920931210     D  PA3TSR                 8      8
002930931210     D  PA3NDC                 9     15  0
002940931210     D  PA3FCT                16     16
002950931210     D  PA3FVD                17     17
002960931210     D  PA3FVT                18     18
002970950113     D  PA3DDC                19     26  0
002980041122     D  PA3sml               256    256
002990941014     D***
003000941014     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
003010941014     D***
003020941014     D WLBDAT          DS                  INZ
003030941014     D  G02DAT                 1      8  0
003040941014     D  G02INV                 9     16  0
003050941014     D  G02ERR                17     17
003060941014     D  G02TGI                18     22  0
003070941019     D                 DS
003080941019     D  VIDISD                 1      2
003090941019     D  VIDCPD                 3     16
003100941019     D  VIDPID                 1     16
003110071022     D                 DS
003120071022     D  V11ISD                 1      2
003130071022     D  V11CPD                 3     16
003140071022     D  V11PID                 1     16
003150941223     D                 DS
003160941223     D  VIDISM                 1      2
003170941223     D  VIDCPM                 3     16
003180941223     D  VIDPIM                 1     16
003190930210     D                 DS
003200930210     D  EMME                   1      1
003210020805     d  $simfel                2      4  0
003220020805     D  PARMBR                 5      7  0
003230930210     D  MBRFIS                 1     10
003240140130
003250140130      *Schiera per filiali partite con gestione telefonate AUT
003260140130     d Fil816tel       ds           256
003270140130     d  skFil816tel            1    255    dim(85)
003280141029     d TRULVPODS     e ds
003290141029     D  skFilOk               16    765    dim(250)
003300141029
003310141029     D skFilOkdir      s                   like(skFilOk) dim(250) inz
003320140130
003330920211     D DSARBD        E DS
003340930510     D DSARBG        E DS
003350130930     D DSARBN        E DS
003360990930     D DARBT         E DS
003370030612     D  §SV                   44     63    DIM(20)                              SIGLA  VARIE
003380030612     D  §VA                   64    183P 3 DIM(20)                              VALORE VARIE
003390920128     D DSARBK        E DS
003400030612
003410930506     D DSCC          E DS
003420991001     D DS$2          E DS
003430941212     D DSCV          E DS
003440941020     D DSQT1         E DS
003450080416     D DS3K          E DS
003460160915     D DTAS          E DS                  inz
003470060925      ** DS Flag operativi DS DTAS
003480060925     d Dtas01        e ds
003490060925
003500990930     D DTASV         E DS
003510030612     D  VSV                    1     20    DIM(20)                              SIGLA  VARIE
003520030612     D  VVA                   21    140P 3 DIM(20)                              VALORE VARIE
003530030612     D  VES                  141    160    DIM(20)                              ESENZ  VARIE
003540030612                                                                                ESENZ  VARIE
003550030612     D DSBL4W        E DS
003560070320     D DSBL4L        E DS
003570151126     D DSBL4m        E DS
003580160318     D DSBL4i        E DS
003590000623     D DS3IPT        E DS
003600000510     D DS3IDP        E DS
003610000510     D DS1A          E DS
003620930506     D DS1P          E DS
003630060131     D DSSP          E DS
003640941209     D DS15          E DS
003650920117     D DS3A          E DS
003660020715     d dstb          e ds
003670000202     D OG143         E DS
003680070319     D OG146         E DS
003690070306     d Og148         e ds
003700070306     d Og150         e ds
003710000202     D DGEI          E DS
003720991006     D DGED          E DS
003730991028     D DSTA01        E DS
003740930210     D DS7T          E DS
003750021121     d ddfe          e ds
003760030710     d dvpo          e ds
003770071031     d dvpoDECO      e ds
003780160318     d dnsd          e ds
003790050601     d dta4c         e ds
003800151019     D Dar5fat       E DS
003810090604     d ds5e          e ds
003820110906     d FNLVSTDS      e ds
003830130930     d dsemail       e ds
003840130927     d
003850130927     d darbn_ref     e ds
003860130927     d darbn_eml     e ds
003870130930     d darbn_sms     e ds
003880130930     d darbn_not     e ds
003890061107     d
003900151105     d xcfiva1ds     e ds
003910071025     d  ivaeu                  3      4
003920030612      * DS PER YEURCO - EURO CONVERTITORE
003930030612     D YeurcoDS      E DS
003940030612      * DS PER TRUL90R - reperimento stampanti e pgm di stampa
003950130322     D TRUL90DS      E DS
003960030612      * DS PER FNLV13R - CONTROLLO DI UN CAP
003970030612     D Fnlv13DS      E DS
003980030612      * DS PER FNLV14R - CONTROLLO DI INDIRIZZO COMPLETO  SENZA CAP
003990030612     D Fnlv14DS      E DS
004000030612      * DS PER TISI95R - CONTROLLO DI CAP
004010030612     D Tisi95DS      E DS
004020030612      * DS PER TRTB69R - TRTB70R - TRTB72R - TRTB73R - INT E GEST PARTI
004030030612     D DS7pqrs       E DS
004040030612      * DS PER TIBS02R - RICERCA TABELLA SU TNTBE
004050030612     D Tibs02DS      E DS
004060030626      * DS PER STAMPA LDV
004070130321     D*FNLSB5DS      E DS
004080030612      * DS PER FNLV04R - PER CALCOLO TOTALE FATTURA
004090030612     D FNLV04DS      E DS
004100050202      * DS PER FNLV59R - CARICAMENTO E CONTROLLO CODICI TARIFFA
004110050202     D Fnlv59DS      E DS
004120041008      * DS PER FNLV57R - PER aggiornamento files padroncini
004130041008     D FNLV57ds      E DS
004140030612      * DS PER FNLV16R - CONTROLLO TASSAZIONE
004150030612     D Fnlv16DS      E DS
004160040122     d dlv1601       e ds
004170030612      * DS PER FNLV18R - CALCOLO VOLUME AUTOMATICO
004180030612     D Fnlv18DS      E DS
004190060223      * DS PER FNLV19R - SCRITTURA E AGGIORNAMENTO FiAR400F
004200030612     D Fnlv19DS      E DS
004210030612      * DS PER FNLV20R - CALCOLA E SISTENA I VOLUMI SU BOLLA
004220030612     D Fnlv20DS      E DS
004230030612      * DS PER FNLV55R - REPERISCE TERMINAL DI PARTENZA E ARRIVO
004240030612     D Fnlv55DS      E DS
004250050511      * DS PER FNLRa2R - TASSAZIONE PORTI ASSEGNATI
004260050511     D Fnlra2DS      E DS
004270030612      * DS PER FNLR48R - DATI DI ENTRATA
004280030612     D Fnlr48DS      E DS
004290030612      * DS PER FNLR66R - CONTROLLO CAUSALE VARIAZIONE
004300030612     D Fnlr66DS      E DS
004310140130      * DS PER FNLRN6R - note LDV
004320140130     D FnlrN6DS      E DS
004330130321      * DS PER FIDG42R -
004340130321     D Fidg42ds      E DS
004350160318      * DS PER FNLR98R -
004360160318     D Fnlr98ds      E DS
004370141029
004380141029     D fnlry09ds2    E DS
004390150603     d
004400150603     D tnsd99ds      E DS
004410070329     d
004420120413     D TIBS69DS      E DS
004430120413     D ds_cnaco      E DS                  extname(CNACO00F) prefix(BS_)
004440120413     D ds_cnind      E DS                  extname(CNIND00F) prefix(BS_)
004450120413     D ds_cnclp      E DS                  extname(CNCLP00F) prefix(BS_)
004460120413     D ds_fncls      E DS                  extname(FNCLS00F) prefix(BS_)
004470120413
004480070329      * Dati utente
004490060131     d Azuteds       e ds                  extname(Azute00f)
004500060131     d dDatiute      e ds
004510060131     d dute01        e ds
004520060131     D TIBS34DS      E DS
004530060213      * ds x richiamo pgm FNLv85R
004540060213     d Fnlv85ds      e ds
004550060705      * ds x controllo tipo incasso e divisa C/A
004560060705     d TRULTICDS     e ds
004570140515
004580140515     d TRULINTDS     e ds
004590980722      *
004600030612      * DS PER XEDIT - PGM EDITAZIONE
004610011009     D DSEDIT        E DS                  EXTNAME(XEDITDS)
004620011009     D  IEDNUM       E                     EXTFLD(IEDNUMERO)
004630011009     D  IEDNRD       E                     EXTFLD(IEDNRDEC)
004640011009     D  IEDEDT       E                     EXTFLD(IEDEDTCOD)
004650011009     D  IEDLEN       E                     EXTFLD(IEDLENFLD)
004660011009     D  OEDNUM       E                     EXTFLD(OEDNUMAN)
004670011009     D  OEDFLG       E                     EXTFLD(OEDFLGERR)
004680011009     D  OEDCOD       E                     EXTFLD(OEDCODERR)
004690941212      *
004700910429     D KPJBA         E DS
004710930210     D  LIB                   92    101
004720060131     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004730030612     D  DTU                  848    895P 0 DIM(12)    PACKEVEN                  DATE PARAM
004740011205
004750020327     D DSFTD         E DS                  EXTNAME(FIFTD00F)
004760011205     d trul21ds      e ds
004770011205     d trul22ds      e ds
004780011205     d trul23ds      e ds
004790160225     D* DS PER TRUL27R1 - network bolla
004800160225     D trul27ds1     E DS
004810160225     D
004820160225     D trulC7ds      E DS
004830170130     D* DS PER TRUL73R - CONTROLLO LIMITI per tipo servizio
004840170130     D trul73ds      E DS
004850020531     D tisit0ds      E DS
004860131227     D fnlva1ds      E DS
004870041007     d*Trul42Ds      e ds
004880140212     d Trtcp2ds      e ds
004890141021     D TRUL33DS      E DS
004900021129     d Dar5scr       e ds
004910030123     d dAr5Ban       e ds
004920030729     d dAr5Bnb       e ds
004930130924     d dAr5Emd       e ds                  prefix(emd_)
004940031120     d dAr5          e ds
004950031120     d dAr5Gen       e ds
004960040219     d  §Ar5gTcr     e                     Extfld(§Ar5Tcr)
004970040219     d  §Ar5gDcr     e                     Extfld(§Ar5Dcr)
004980040219     d  §Ar5gHcr     e                     Extfld(§Ar5Hcr)
004990040219     d  §Ar5gGc1     e                     Extfld(§Ar5Gc1)
005000040219     d  §Ar5gGc2     e                     Extfld(§Ar5Gc2)
005010040219     d  §Ar5gTc1     e                     Extfld(§Ar5Tc1)
005020040219     d  §Ar5gTc2     e                     Extfld(§Ar5Tc2)
005030070910     d dAr5GenPar    e ds                  extname(dar5gen) prefix(PAR:3)
005040070910     d  PAr5gTcr     e                     Extfld(§Ar5Tcr)
005050070910     d  PAr5gDcr     e                     Extfld(§Ar5Dcr)
005060070910     d  PAr5gHcr     e                     Extfld(§Ar5Hcr)
005070070910     d  PAr5gGc1     e                     Extfld(§Ar5Gc1)
005080070910     d  PAr5gGc2     e                     Extfld(§Ar5Gc2)
005090070910     d  PAr5gTc1     e                     Extfld(§Ar5Tc1)
005100070910     d  PAr5gTc2     e                     Extfld(§Ar5Tc2)
005110030123     d dTba          e ds
005120031028     d dsbl4a        e ds
005130040806
005140070309     d ds2Z          e ds
005150070309     d ds2g2         e ds
005160040806     d ds7q          e ds
005170140408     d ds7r          e ds
005180051109     d dgcpflr       e ds
005190060111     d ddstflo       e ds
005200040809
005210040809      * ds per il controllo della presenza di CA per la spedizione richiesta
005220080701     d****** FIDN12DS      e ds
005230080701     d***  skKey                 26    305    dim(20)
005240080701     d****  skAnn                306    325    dim(20)
005250080701     d****  skDca                326    485  0 dim(20)
005260080701     d****  skFca                486    545  0 dim(20)
005270080701     d****  skDch                546    705  0 dim(20)
005280080701     d****  skCch                706    745    dim(20)
005290040809     d*
005300040809     d dsKey           ds
005310040809     d  dsaac                         4  0
005320040809     d  dsfil                         3  0
005330040809     d  dsnca                         7  0
005340050531     D PARAMT          DS
005350050531     D  PARCA                256    256
005360011205
005370950104     D*
005380950104     D* DEFINIZIONE COSTANTI
005390950104     D*
005400950104     D CPDC            C                   CONST('su Piano dei Conti')
005410950114     D CNOTAR          C                   CONST('NO TARIFFE')
005420061107     D Cdest           C                   CONST('D E S T I N A T A R I O')
005430061107     D CMITT           C                   CONST('--- M I T T E N T E ---')
005440141022     D* CAMPO PER DSPATR "REVERSE IMAGE"        DEI CAMPI A VIDEO
005450141022     D RIMM            C                   CONST(X'21')
005460011205
005470011205     d                sds
005480011205     d  nompgm                 1     10
005490030620
005500030620     d sav_indsin      s                   Like(indsin)
005510041008     d waltrocpi       s                   Like(arbcpi)
005520060223     d wcpifile        s                   Like(arbcpi)
005530041008     d waltrofvf       s                   Like(arbfvf)
005540041008     d waltrovlf       s                   Like(arbvlf)
005550041022     d waltrotc1       s                   Like(arbtc1)
005560041022     d waltrotc2       s                   Like(arbtc2)
005570080422     d waltroft3       s                   Like(arbft3)
005580041027     d newcpi          s                   Like(arbcpi)
005590051114     d wtc1            s                   Like(arbtc1)
005600051114     d wtc2            s                   Like(arbtc2)
005610051220     d memntw          s                   Like(§ogntw)
005620060111     d d_o95lna        s                   like(o95lna)
005630060316     d s_arbrsd        s                   like(arbrsd)
005640060316     d s_arbrd2        s                   like(arbrd2)
005650061107     d wcliie          s              1
005660061107     d wprv            s                   like(arbprd)
005670151105     d wcap            s                   like(arbcad)
005680151105     d wloc            s                   like(arblod)
005690061107     d Esiste          s              1
005700091217     d Wtipoagg        s              3
005710110906     d Lnacons         s              3  0
005720061107     d wnar            s                   Like(arbnzd)
005730130411     d wpda_ndc        s                   Like(arbndc)
005740130411     d wpda_ddc        s                   Like(arbddc)
005750130411     d wpda_dcm        s                   Like(arbdcm)
005760131016     d winf14          s              1
005770131120
005780131120     d sav_ift         s                   Like(ar6ift)
005790140403     d comgf2          s                   Like(o95gf2)
005800140407     d wpk2            s                   Like(§AR5PK2)
005810141020     d wdir            s              1
005820141021     d $Finenum        s               n   inz(*off)
005830141021     d wfgslna         s                   like(d55tfa)
005840141021     D Esito           s              1a
005850141031     d w0090           s              9  0
005860141021     d kprg            s                   like(idcprg)
005870141021     d winf12          s              1
005880141022     d annocur         s              4s 0
005890141029     d w0140i          s             14s 0 inz
005900141203     d savzng          s                   like(vidzng)
005910141212     d wnoindir        s              1
005920150119     d w_noapp         s              1
005930151016     d PesVolpkcn      s                   Like(Taspkcn)
005940151016     d PesVolSpc       s                   Like(TasSpc)
005950151016     d PesVolIMV       s                   Like(Ar6IMV)
005960151019     d Tassatoimv      s                   Like(Ar6IMV)
005970160112     d wfile           s              1
005980160318     d wtogli          s              1
005990170130     d wmsgtsp         s             78
006000170130     d wpes_pre        s                   like(vidpkf)
006010170130     d wvol_pre        s                   like(vidvlf)
006020170131     d wtsp03          s                   like(§5ed03)
006030141021
006040141021     D trul33R         pr                  extpgm('TRUL33R')
006050141021     D  kpjba                              likeds(kpjba)
006060160318     D fnlv19r         pr                  extpgm('FNLV19R')
006070160318     D  kpjba                              likeds(fnlv19ds)
006080160330     D fnlr98c         pr                  extpgm('FNLR98C')
006090160318     D  kpjba                              likeds(kpjba)
006100070515     i*
006110070515     IFNBLP000
006120070515     I              BLPFST                      ARBFSTDDT
006130910430     C****************************************************************
006140910430     C*  RIEPILOGO INDICATORI
006150910430     C***************************************************************
006160920128     C* 01    - VARIAZIONE DESTINATARIO
006170941209     C* 02    - VARIAZIONE PESO / VOLUME
006180930510     C* 03    - PROGRAMMA RICHIAMATO  DSARBG
006190930505     C* 04    - SPEDIZIONE CON GIACENZA APERTA
006200941222     C* 05    - INDICA SE LA 1 BOLLA O LA 2 E' UN ASSEGNATO X P.IVA
006210941222     C* 06    - 1 BOLLA IN FRANCO
006220991006     C* 07    - PROTEZIONE DIVISA PER PREPAGATI E FATTURE IMMEDIATE
006230991012     C* 08    - PREPAGATO
006240941213     C* 09    - SONO STATE MESSE DELLE VARIE NELLA TASSAZIONE
006250941222     C* 10    - ON -BOLLE IN ARRIVO OFF-BOLLE IN PARTENZA
006260941102     C* 11/12 - PROTEGGO LE CONSEGNE PARTICOLARI SE IMMESSE IN PART E
006270941102     C*         NON UTILIZZABILI IN ARRIVO
006280941102     C* 13    - VARIAZIONE TASSAZIONE S.F.
006290911210     C* 14    - VARIAZIONE TASSAZIONE FATTURA
006300941102     C* 15    - RITASSAZIONE
006310950120     C* 16    - PER CAUSALE CP PROTEGGO CAMPI DELLA CONSEGNA RICH/GG CH
006320940614     C* 17 OFF- FILE TASSAZIONE PADRONCINI
006330940614     C* 17 ON - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
006340911211     C* 18    - NON ESISTONO TARIFFE PER IL CODICE DIGITATO
006350920128     C* 19    - PROGRAMMA RICHIAMATO  DSAR2T
006360920128     C* 20    - PROGRAMMA RICHIAMATO  GENERICO
006370990701     C* 21    - CAUSALE VARIAZIONE MODIFICA CONTRASSEGNO
006380990701     C* 22    - UATO PER PROTEZIONE MITTENTE
006390990701     C* 23    - DATI FATTURA NON MODIFICABILI PER PREPAGATO AUTOMAT
006400950113     C* 24    - ESEGUITA LA VARIAZIONE DEL MITTENTE
006410920128     C* 25    - PROGRAMMA RICHIAMATO  DSARBT
006420950607     C* 26    - ABILITA L'ENTER PER L'AGGIORNAMENTO
006430950607     C* 27    - ABILITA L'F12 PER RITORNO AL PGM CHIAMANTE
006440941212     C* 28    - EMETTO VIDMSG
006450950616     C* 29    - PROTEZIONE CAMPI DEL VIDEO
006460980513     C* 30-33 - COMODO
006470920211     C* 34    - PROGRAMMA RICHIAMATO  DSARBD
006480920129     C* 35    - PROGRAMMA RICHIAMATO  DSARBK
006490920129     C* 36    - VARIA TASSAZIONE FRANCHI: SALTO I CONTROLLI SUL COD.CLI
006500920204     C* 37    - VARIATO TOTALE IMPONIBILE: EFFETTUO RISTAMPA FATTURA
006510941014     C* 38    - CREO RECORD IN FNARBM0T
006520941014     C* 39    - CREO RECORD IN FNARBD0T-CAUS.K5 K8- PER AVERE DESTINAT
006530930507     C* 40-67 - ERRORE
006540030729      * 70    - Visualizzo Colli originali
006550991001     C* 71-72 - PROTEGGO CAMPI DESTINATARIO SE 9999 GIA' CONTABILIZZATO
006560990415     C* 73    - SPROTEGGO IL CAMPO DELL'IMPORTO D'ASSICURARE E DEL VALORE
006570990415     C*         MERCE DICHIARATO
006580060201     C* 74    - protezione campi tassazione
006590060201     C* 75    - protezione campi importo da assicurare
006600140213     C* 77    - condizionamento campo "fd da non contattare"
006610131001     C* 81    - della DSARBN  passato campo uni70 - note
006620990415     C* 82    - ERRORE NON BLOCCANTE PER ELIMINAZIONE O AGGIUNTA IMP.ASSICURAZ.
006630131001     C* 83    - della DSARBN  passato campo cel
006640131001     C* 84    - della DSARBN  passato campo ref/tel
006650131001     C* 85    - SFPDSP
006660131001     C* 86    - della DSARBN  passato campo uni70 - email
006670071022     C* 87    - tasto funzionale"F17-Note ldv"attivo
006680070329     C* 88    - PROTEGGO PESO   SE NON VARIABILE
006690941216     C* 89    - PROTEGGO VOLUME SE NON VARIABILE
006700920220     C* 90    - ERRORE GENERICO
006710911213     C* 91    - ERRORE NON POSSIBILE ALLOCARE FILE DI INVIO
006720950407     C* 92    - ERRORE NON BLOCCANTE PER ELIMINAZIONE O AGGIUNTA C/ASS.
006730941215     C* 93    - INDICATORE DI CHANGE SU VALORE MERCE DICH
006740941215     C* 94    - RI SU VALUTA VAL.MERCE SE PRESA IN AUTOMATICO DA NAZ
006750941102     C* 95    - INDICATORE DI CHANGE SU C/ASSEGNO
006760941215     C* 96    - RI SU VALUTA C/ASSEGNO SE PRESA IN AUTOMATICO DA NAZ
006770941215     C* 97    - INDICATORE DI CHANGE SU IMPORTO DA ASSICURARE
006780941215     C* 98    - RI SU VALUTA IMP DA ASSIC SE PRESA IN AUTOMATICO DA NAZ
006790910430     C*****************************************************************
006800000000     C     *ENTRY        PLIST
006810000000     C                   PARM                    KPJBA
006820920211     C                   PARM                    DSARBD
006830920128     C                   PARM                    DSARBK
006840990930     C                   PARM                    DARBT
006850930510     C                   PARM                    DSARBG
006860030612     C                   PARM                    TRUL90DS
006870130930     C                   PARM                    DSARBn
006880050601     c*
006890050110     c*
006900050110     c                   setoff                                       kl
006910030612      *
006920030612      * Il numero dei parametri ricevuti è testato, in caso di modifica
006930030612      *  ricercare la stringa "%PARMS"
006940030612      *
006950030612     C                   MOVEL     KPJBU         Fnlr48DS
006960120426     C*
006970120426     C                   EXSR      OPN_TITA4
006980950104     C* ESEGUO LA VARIAZIONE
006990950104     C                   MOVEL     'E'           WESEG             1
007000020603     c                   movel     d48f12        w48f12
007010060301     c                   movel     d48mct        w48mct
007020950114     C* SE NON  RICHIAMATO IMPOSTO SEMPRE LA FERMATA NELLE VIDEATE
007030950114     C     D48TRC        IFEQ      *BLANKS
007040950609     C                   MOVEL     'V'           D48FFR
007050950114     C                   ENDIF
007060070907     c* skiere di riesecuzione automatica pgm: max 10 volte
007070070907     c                   clear                   kdati
007080070907     c                   clear                   kcvb
007090070907     c                   clear                   ktrc
007100070911     c                   clear                   kffr
007110070911     c                   clear                   kf12
007120070907     c                   clear                   kK                2 0
007130070907     c                   z-add     1             rr                2 0
007140941014     C*
007150910429     C*---------------------------------------------------------------*
007160041006     C* TIPO LANCIO, D48TLA "C" -           CHIUSO CON %R
007170941209     C* TIPO LANCIO, D48TLA "L" - ELABORO E CHIUDO CON LR
007180941209     C* TIPO LANCIO, D48TLA " " - ELABORO E CHIUDO IN RETRN
007190941209     C*
007200941209    1C     D48TLA        IFNE      'C'
007210001213     C*
007220001213    2C     D48FFR        IFNE      'E'
007230141017    2C     D48FFR        andne     'F'
007240001213    3C     OPN48         IFEQ      '0'
007250001213     C                   OPEN      FNLR48D
007260001213     C                   MOVE      '1'           OPN48
007270001213    3C                   ENDIF
007280001213   X2C                   ELSE
007290001213    3C     OPN48         IFEQ      '1'
007300001213     C                   CLOSE     FNLR48D
007310001213     C                   MOVE      '0'           OPN48
007320001213    3C                   ENDIF
007330001213    2C                   ENDIF
007340040224
007350040224     c                   Clear                   wCa
007360040324     c                   Clear                   wgiac
007370941209     C*
007380950104     C* FINTANTO CHE SI VUOLE SI PUO' RIMANRE DENTRO ALLE VARIAZIONI
007390950104   1AC     WESEG         DOWEQ     'E'
007400950104     C                   CLEAR                   WESEG
007410920123     C*
007420950104     C* CONTROLLI BOLLA CON CAUSALE VARIAZIONE
007430900509     C                   EXSR      CONTR
007440941014     C*
007450941014     C   90              GOTO      FINE
007460911209     C*
007470911210     C* CARICO BOLLA
007480120403     c                   if        flgcvr<>'S'
007490911210     C                   EXSR      CARICA
007500131002     c                   if        d48err='1'
007510131002     C                   GOTO      FINE
007520120403     c                   endif
007530131002     c                   endif
007540911209     C**
007550941019     C** VARIAZIONE: DESTINATARIO PESO VOLUME
007560911209     C**
007570941220     C                   SELECT
007580120330     c* Variazioni da manutenzione bolle di sede
007590120330     C     FLGCVR        WHENEQ    'S'
007600120427     c* Questa routine la posso eseguire solo se pgm richiamato
007610120427     c* Hanno fatto una manutenzione con CVB SD/SF da menù in quanto
007620120427     c* SD e SF non erano stati disabilitati per l'utilizzo da bolle par
007630120427     c* e bolle arrivi. In questo modo essendo azzerava peso,volume, mittente..
007640120427     c                   if        d48trc<>*blanks
007650120330     C                   EXSR      gesSD
007660120427     c                   endif
007670120330
007680941220     C     FLGCVR        WHENEQ    'I'
007690920120     C                   EXSR      EMFOR2
007700911210     C**
007710911211     C** VARIAZIONE TASSAZIONE 1° BOLLA
007720911210     C**
007730941220     C     FLGCVR        WHENEQ    'T'
007740920121     C                   EXSR      EMFOR3
007750911211     C**
007760911211     C** VARIAZIONE TASSAZIONE 2° BOLLA
007770911211     C**
007780941220     C     FLGCVR        WHENEQ    '2'
007790941102     C                   EXSR      EMFOR4
007800920116     C**
007810920116     C** VARIAZIONE: CODICE BOLLA CONTRASSEGNO
007820920116     C**
007830941220     C     FLGCVR        WHENEQ    'K'
007840941102     C                   EXSR      EMFOR5
007850920320     C**
007860920320     C** VARIAZIONE CONSEGNA RICHIESTA
007870920320     C**
007880941220     C     FLGCVR        WHENEQ    'C'
007890021129     c     D48Cvb        AndNe     'CS'
007900941102     C                   EXSR      EMFOR7
007910021129     C**
007920021129     C** VARIAZIONE CONSEGNA RICHIESTA SUPERMERCATI
007930021129     C**
007940021129     c     D48Cvb        WhenEq    'CS'
007950021129     c                   ExSr      Emfor71
007960941223     C**
007970941223     C** VARIAZIONE MITTENTE
007980941223     C**
007990941223     C     FLGCVR        WHENEQ    'M'
008000941223     C                   EXSR      EMFOR8
008010961211     C**
008020130927     C** VARIAZIONE DATI MITTENTE
008030961211     C**
008040961211     C     D48CVB        WHENEQ    'RM'
008050961211     C                   EXSR      EMFOR9
008060030123      **
008070030123      ** Variazione mancato ritiro BANCALI
008080030123      **
008090030123     c     D48Cvb        WhenEq    'BR'
008100030123     c                   ExSr      Emfor10
008110961211     C*
008120061107      **
008130061107      ** Variazione CODICE FISCALE
008140061107      **
008150061107     c     D48Cvb        WhenEq    'FI'
008160061107     c                   ExSr      Emfor11
008170130927      **
008180130927      ** Variazione dati destinatario SMS/mail/referente
008190130927      **
008200130927     C     FLGCVR        WHENEQ    'N'
008210130927     c                   ExSr      Emfor12
008220061107     C*
008230941220     C                   ENDSL
008240070907     c
008250070907     c* Verifico se ho delle riesecuzioni da fare
008260070907     c                   if        RR<=10 and kcvb(RR)<>*blanks
008270070907     c                   eval      weseg='E'
008280070907     c                   eval      d48cvb=kcvb(RR)
008290070907     c                   eval      d48trc=ktrc(RR)
008300070911     c                   eval      d48ffr=kffr(RR)
008310070911     c                   eval      w48f12=kf12(RR)
008320070907     c                   select
008330070907     c                   when      d48trc='D'
008340070907     c                   movel     kdati(RR)     dsarbd
008350070907     c                   when      d48trc='G'
008360070907     c                   movel     kdati(RR)     dsarbg
008370070907     c                   when      d48trc='K'
008380070907     c                   movel     kdati(RR)     dsarbK
008390070907     c                   when      d48trc='T'
008400070907     c                   movel     kdati(RR)     darbt
008410070907     c                   when      d48trc='2'
008420070907     c                   movel     kdati(RR)     darbt
008430070907     c                   ENDSL
008440070907     c
008450070907     c                   add       1             RR
008460070907     c                   endif
008470941209     C*
008480950104   1AC                   ENDDO
008490950104    1C                   ENDIF
008500920121     C*
008510000000     C     FINE          TAG
008520941209     C* CHIUSURA PGM
008530060302     C******             MOVEL     Fnlr48DS      KPJBU
008540950114     C*
008550950114     C* PULISCO IL CAMPO CHER AVEVO IMPOSTATO CON LA CONFERMA DEL MITT
008560950114     C*  CHE DEVE RIMANERE IMPOSTATO FINTANTO CHE SI E' DENTRO AL PGM
008570950114     C                   CLEAR                   WRICVL
008580950114     C                   CLEAR                   MITFVF
008590950114     C                   CLEAR                   MITVLF
008600950114     C                   CLEAR                   MITCTR
008610020722      * PULISCO IL CAMPO CHE AVEVO IMPOSTATO PER IL DESTINATARIO
008620020722      *  DISAGIATO
008630020722     C                   CLEAR                   Wdisag
008640060302     c*
008650060302     c                   Clear                   Fnlv85ds
008660060302     c                   Eval      Ilv85Tla = 'C'
008670060302     c                   movel     fnlv85ds      kpjbu
008680060302     c                   Call      'FNLV85R'
008690060302     c                   Parm                    kpjba
008700060302     c*
008710950114     C* CHIUSURA PGM
008720941216    1C     D48TLA        IFEQ      ' '
008730130417     C                   MOVEL     Fnlr48DS      KPJBU
008740941209     C                   RETURN
008750941209     C                   ELSE
008760941209     C*
008770930510     C* SE APERTI FILE CHIUDO
008780001213     C*
008790001213     C     OPN48         IFEQ      '1'
008800001213     C                   CLOSE     FNLR48D
008810001213     C                   ENDIF
008820001213     C*
008830030612     C                   CLEAR                   FNLV04DS
008840941209     C                   MOVEL     'C'           D04TLA
008850990930     C                   CALL      'FNLV04R'
008860030612     C                   PARM                    FNLV04DS
008870990930     C                   PARM                    DTASV
008880970808     C*
008890970808     C                   MOVEL     'C'           I13TLA
008900970808     C                   CALL      'FNLV13R'
008910970808     C                   PARM                    KPJBA
008920030612     C                   PARM                    Fnlv13DS
008930030612     C                   PARM                    Tisi95DS
008940030612     C                   CLEAR                   Tisi95DS
008950971105     C**
008960971105     C                   MOVEL     'C'           I95TLA
008970971105     C                   CALL      'TISI95R'
008980030612     C                   PARM                    Tisi95DS
008990970808     C*
009000970808     C                   MOVEL     'C'           I14TLA
009010970808     C                   CALL      'FNLV14R'
009020970808     C                   PARM                    KPJBA
009030030612     C                   PARM                    Fnlv14DS
009040971114     C*
009050030612     C                   CLEAR                   Fnlv55DS
009060971114     C                   MOVEL     'C'           D55TLA
009070971114     C                   CALL      'FNLV55R'
009080030612     C                   PARM                    Fnlv55DS
009090050202     c
009100050202     C                   CLEAR                   Fnlv59DS
009110050202     C                   MOVEL     'C'           ilv59tla
009120050202     C                   CALL      'FNLV59R'
009130050202     C                   PARM                    Fnlv59DS
009140020305     c*
009150160225     C                   CLEAR                   trul27ds1
009160020305     C                   MOVEL     'C'           I27TLA
009170160225     C                   CALL      'TRUL27R1'
009180160225     C                   PARM                    TRUL27DS1
009190160225     c
009200060705     C                   CLEAR                   trulTICds
009210060705     C                   MOVEL     'C'           IticTLA
009220060705     C                   CALL      'TRULTICR'
009230060705     C                   PARM                    TRULTICDS
009240060705     C                   PARM                    DS1A
009250140515
009260140515     C                   CLEAR                   trulintds
009270140515     C                   MOVEL     'C'           IintTLA
009280140515     C                   CALL      'TRULINTR'
009290140515     C                   PARM                    TRULintds
009300110906     c
009310110906     C                   CLEAR                   fnlvstds
009320110906     C                   MOVEL     'C'           ILVSTTLA
009330110906     C                   CALL      'FNLVSTR'
009340110906     c                   parm                    kpjba
009350110906     C                   PARM                    fnlvstds
009360941220     C*
009370120426     C                   IF        %OPEN(TITA430C)
009380050601     c                   close     tita430c
009390120426     C                   ENDIF
009400151016     C*
009410151016     C                   IF        %OPEN(FIAR531C)
009420151016     c                   close     fiar531c
009430151016     C                   ENDIF
009440151016     C*
009450151016     C                   IF        %OPEN(TITAS30C)
009460151016     c                   close     titas30c
009470151016     C                   ENDIF
009480130321
009490130411     c                   clear                   fidg42ds
009500130411     C                   MOVEL     'C'           co42tla
009510130411     c                   movel     fidg42ds      kpjbu
009520130411     c                   call      'FIDG42R'
009530130411     c                   parm                    kpjba
009540131227
009550131227     c                   clear                   fnlva1ds
009560131227     C                   MOVEL     'C'           ia1tla
009570131227     c                   movel     fnlva1ds      kpjbu
009580131227     c                   call      'FNLVA1R'
009590131227     c                   parm                    kpjba
009600141029
009610141029     c                   eval      ilry09tla= 'C'
009620141029     c                   eval      kpjbu=fnlry09ds2
009630141031     c                   call      'FNLRY09C'
009640141029     c                   parm                    kpjba
009650160318
009660160318     c                   eval      d19tla  =  'C'
009670160318     c                   call      'FNLV19R'
009680160318     c                   parm                    fnlv19ds
009690130321
009700130417     C                   MOVEL     Fnlr48DS      KPJBU
009710041006     C                   SETON                                        LR
009720941209     C                   END
009730941220     C*
009740941209     C* IMPOSTAZIONI INIZIALI -------------------------------------*
009750941209     C     *INZSR        BEGSR
009760941212     C* MI SALVO LA KPJBU
009770941212     C                   MOVEL     KPJBU         W256A           256
009780060131      *
009790060131     c     *dtaara       define    §azute        azuteds
009800060131     c     *dtaara       define    §datiute      ddatiute
009810060131     c                   in(E)     *dtaara
009820060131    1c                   If        %error  or RSUT = *blanks
009830060131     c                   Clear                   Tibs34ds
009840060131     c                   Call      'TIBS34R'
009850060131     c                   Parm                    Tibs34ds
009860060131     c                   In        *dtaara
009870060131    1c                   EndIf
009880060131
009890060131      * se ho errori nei dati utente esco dal pgm
009900060131w   1c                   if        DutErr = 'E'
009910060131     c                   GoTo      Fine
009920060131x   1c                   else
009930060131     c                   Movel     UteFaf        Dute01
009940060131e   1c                   endif
009950941212     C*
009960941209     C                   Z-ADD     1             CODUT
009970060131     C                   CALL      'X§PARUT'
009980060131     C                   PARM                    UT§DSE
009990060131     C                   MOVEL     rsut          VIDDSA
010000000104     C**
010010000104     C                   MOVEL     DTU(10)       W0020             2 0
010020000104     C                   Z-ADD     DTU(10)       WDTU10
010030000104     C     W0020         IFGE      60
010040000104     C                   MOVEL     19            WDTU10
010050000104     C                   ELSE
010060000104     C                   MOVEL     20            WDTU10            8 0
010070000104     C                   ENDIF
010080941014     C*
010090020805
010100020805     c                   eval      $simfel = simfel
010110020805     c                   movel     simfel        $mbr
010120070329     c
010130070329     c* Per EDP note sempre attive, se lna ha geo attiva
010140070329     c                   if        %subst(knmus:1:3)='EDP'
010150070329     c                   eval      dutgeot='S'
010160070329     c                   else
010170070329     c     dutpou        chain     azorg01l
010180070329     c                   if        %found(azorg01l)
010190070329     c                   movel     orgde8        og148
010200070329     c                   else
010210070329     c                   clear                   og148
010220070329     c                   endif
010230070329     c                   movel     §oggeot       dutgeot           1
010240070329     c                   endif
010250941020     C***
010260941020     C* CARICO CAMPI FISSI PER TASSAZIONE/BOLLETTAZIONE
010270941020     C***
010280941020     C                   MOVEL     'QT'          COD
010290941020     C                   MOVEL     '1       '    KEY
010300941212     C                   EXSR      CTABEL
010310941020     C  N30              MOVEL     TBLUNI        DSQT1
010320941020     C   30              CLEAR                   DSQT1
010330941214     C***
010340941214     C* PRENDO DALLA TABELLA DEI VOLUMI IL FLAG DI SOSTITUZIONE PER"Z"
010350941214     C***
010360941214     C                   MOVEL     '7T'          COD
010370941214     C                   MOVEL(P)  'Z'           KEY
010380941220     C                   EXSR      CTABEL
010390941214     C   30              CLEAR                   DS7T
010400941214     C  N30              MOVEL     TBLUNI        DS7T
010410941214     C*
010420970901     C* FLAG DI SOSTITUZIONE
010430941214     C                   MOVEL     §7TAFA        ZAFA              1
010440941214     C                   MOVEL     §7TAAA        ZAAA              1
010450941214     C                   MOVEL     §7TAFN        ZAFN              1
010460941214     C                   MOVEL     §7TAAN        ZAAN              1
010470970418     C***
010480970418     C* PRENDO DALLA TABELLA DEI VOLUMI IL FLAG DI SOSTITUZIONE PER"T"
010490970418     C***
010500970418     C                   MOVEL     '7T'          COD
010510970418     C                   MOVEL(P)  'T'           KEY
010520970418     C                   EXSR      CTABEL
010530970418     C   30              CLEAR                   DS7T
010540970418     C  N30              MOVEL     TBLUNI        DS7T
010550970418     C*
010560970901     C* FLAG DI SOSTITUZIONE
010570970418     C                   MOVEL     §7TAFA        TAFA              1
010580970418     C                   MOVEL     §7TAAA        TAAA              1
010590970418     C                   MOVEL     §7TAFN        TAFN              1
010600970418     C                   MOVEL     §7TAAN        TAAN              1
010610991001     C*
010620991001     C* CARICO DA SIGLE VARIE
010630991001     C                   MOVEL     '$2'          COD
010640991001     C                   MOVEL(P)  '1'           KEY
010650991001     C                   EXSR      CTABEL
010660991001     C  N30              MOVEL     TBLUNI        DS$2
010670991001     C   30              CLEAR                   DS$2
010680990930     C**
010690990930     C* CARICO VARIE CHE FANNO PARTE DEL TOTALE IMPONIBILE
010700990930     C**
010710990930     C                   Z-ADD     0             II                3 0
010720990930     C                   MOVEL     'CC'          COD
010730990930     C     KTAB1         CHAIN     TABEL                              30
010740990930    1C     *IN30         DOWEQ     *OFF
010750990930    2C     TBLFLG        IFEQ      ' '
010760990930     C                   MOVEL     TBLKEY        W005A             5
010770990930    3C     W005A         IFEQ      'VARIE'
010780990930     C                   MOVEL     TBLUNI        DSCC
010790990930     C* SIGLA VARIA
010800990930    4C     §CCFL6        IFEQ      'S'
010810990930     C                   ADD       1             II
010820990930     C                   MOVE      TBLKEY        SVA(II)
010830990930    4C                   ENDIF
010840990930    3C                   ENDIF
010850990930    2C                   ENDIF
010860990930     C**
010870990930     C     KTAB1         READE     TABEL                                  30
010880990930    1C                   ENDDO
010890991006     c*
010900991117     C* REPERIMENTO DIVISA DELLA MONETA DI CONTO
010910030612     C                   CLEAR                   Tibs02DS
010920991117     C                   CLEAR                   DGED
010930991117     C                   MOVEL     'C'           T02MOD
010940991117     C                   MOVEL     KNSIF         T02SIF
010950991117     C                   MOVEL     'GED'         T02COD
010960991117     C                   MOVEL     '1'           T02KE1
010970991117     C                   CALL      'TIBS02R'
010980991117     C                   PARM                    KPJBA
010990030612     C                   PARM                    Tibs02DS
011000991117     C     T02ERR        IFEQ      *BLANKS
011010991117     C                   MOVEL     T02UNI        DGED
011020991117     C                   ENDIF
011030011009     C* REPERIMENTO importi DELLA MONETA DI CONTO
011040030612     C                   CLEAR                   Tibs02DS
011050011009     C                   CLEAR                   DGEI
011060011009     C                   MOVEL     'C'           T02MOD
011070011009     C                   MOVEL     KNSIF         T02SIF
011080011009     C                   MOVEL     'GEI'         T02COD
011090011009     C                   MOVEL     §GEDCN        T02KE1
011100011009     C                   CALL      'TIBS02R'
011110011009     C                   PARM                    KPJBA
011120030612     C                   PARM                    Tibs02DS
011130011009     C     T02ERR        IFEQ      *BLANKS
011140011009     C                   MOVEL     T02UNI        DGEI
011150011120     C                   Z-ADD     §GEDFC        DFCSAV
011160011220     C                   Z-ADD     §geimf        imfsav
011170011009     C                   ENDIF
011180011009      * Tabella CV della moneta di conto
011190011009     C                   CLEAR                   DSCV
011200011009     C                   MOVEL     'CV'          COD
011210011009     C                   MOVEL(P)  §GEDCN        KEY
011220011009     C                   EXSR      CTABEL
011230011009     C     *IN30         IFEQ      *OFF
011240011009     C                   MOVEL     TBLUNI        DSCV
011250011009     C                   MOVEL     §CVDEC        DECSAV
011260011009     C                   ENDIF
011270000510     C***
011280000510     C* CARIO TABELLA VARIABILI DPD E POSTE
011290000510     C***
011300000510     C                   MOVEL     '3I'          COD
011310000510     C                   MOVEL(P)  'POSTE'       KEY
011320000510     C                   EXSR      CTABEL
011330000510     C  N30              MOVEL     TBLUNI        DS3IPT
011340000510     C   30              CLEAR                   DS3IPT
011350000510     C*
011360000510     C                   MOVEL     '3I'          COD
011370000510     C                   MOVEL(P)  'DPD'         KEY
011380000510     C                   EXSR      CTABEL
011390000510     C  N30              MOVEL     TBLUNI        DS3IDP
011400000510     C   30              CLEAR                   DS3IDP
011410020715
011420020715      * carico sk con tipi bolla che prevedono l'assicurazione
011430020715     c                   eval      cod = 'TB'
011440020715     c                   clear                   key
011450020715     c                   clear                   xx
011460020715     c     ktab1         setll     tabel00f
011470020715     c                   do        *hival
011480020715     c     ktab1         reade     tabel00f
011490020715      * fine file
011500020715     c                   if        %eof(tabel00f)
011510020715     c                   leave
011520020715     c                   endif
011530020715      * record annullato
011540020715     c                   if        tblflg <> *blanks
011550020715     c                   iter
011560020715     c                   endif
011570020715     c                   movel     tbluni        dstb
011580020715     c                   if        §tbfas = '1'
011590020715     c                   add       1             xx
011600020715     c                   eval      tbl(xx) = tblkey
011610020715     c                   endif
011620020715     c                   enddo
011630070309
011640070309      * Carico sk con eventi che prevedono modifica della cons.rich
011650070309     c                   eval      cod = '2Z'
011660070309     c                   clear                   key
011670070309     c                   clear                   xx
011680090526     c                   clear                   yy
011690070309     c     ktab1         setll     tabel00f
011700070309     c                   do        *hival
011710070309     c     ktab1         reade     tabel00f
011720070309      * fine file
011730070309     c                   if        %eof(tabel00f)
011740070309     c                   leave
011750070309     c                   endif
011760070309      * record annullato
011770070309     c                   if        tblflg <> *blanks
011780070309     c                   iter
011790070309     c                   endif
011800070309     c                   movel     tbluni        ds2Z
011810090526     c* x pgm non richiamato--> controllo solo gli S-sempre
011820090526     c                   if        §2ZADR='S'
011830070309     c                   add       1             xx
011840090526     c                   eval      ADRs(xx) = tblkey
011850070309     c                   endif
011860090526     c*
011870090526     c* x pgm     richiamato--> controllo sia S-sempre  che
011880090526     c*                         C-richiamati
011890090526     c                   if        §2ZADR<>' '
011900090526     c                   add       1             yy
011910090526     c                   eval      ADRc(yy) = tblkey
011920090526     c                   endif
011930070309     c                   enddo
011940021121
011950021121      * Aggancio tabella default tariffe fedex
011960030612     c                   Clear                   Tibs02DS
011970021121     c                   Movel     'C'           T02Mod
011980021121     c                   Movel     KNSIF         T02Sif
011990021121     c                   Movel     'DFE'         T02Cod
012000021121     c                   Movel     'FED'         T02Ke1
012010021121     c                   Call      'TIBS02R'
012020021121     c                   Parm                    Kpjba
012030030612     c                   Parm                    Tibs02DS
012040021121     c                   If        T02Err = *Blanks
012050021121     c                   Movel     T02Uni        DDfe
012060021121     c                   Else
012070021121     c                   Clear                   DDfe
012080021121     c                   EndIf
012090030710
012100060131      * Aggancio tabella VPO recpord VPO
012110030710     c                   Clear                   Dvpo
012120030710     c                   Clear                   Tibs02ds
012130030710     c                   Eval      T02Mod = 'C'
012140030710     c                   Eval      T02Sif = knsif
012150030710     c                   Eval      T02Cod = 'VPO'
012160030710     c                   Movel(p)  'VPO'         T02Ke1
012170030710     c                   Call      'TIBS02R'
012180030710     c                   Parm                    Kpjba
012190030710     c                   Parm                    Tibs02ds
012200030710     c                   If        T02Err = *Blanks
012210030710     c                   Movel     T02Uni        Dvpo
012220030710     c                   EndIf
012230060131
012240060131      * Aggancio tabella VPO record DECO
012250071031     c                   Clear                   Dvpodeco
012260071031     c                   Clear                   Tibs02ds
012270071031     c                   Eval      T02Mod = 'C'
012280071031     c                   Eval      T02Sif = knsif
012290071031     c                   Eval      T02Cod = 'VPO'
012300071031     c                   Movel(p)  'DECO'        T02Ke1
012310071031     c                   Call      'TIBS02R'
012320071031     c                   Parm                    Kpjba
012330071031     c                   Parm                    Tibs02ds
012340071031     c                   If        T02Err = *Blanks
012350071031     c                   Movel     T02Uni        Dvpodeco
012360071031     c                   EndIf
012370140130
012380140130      * Aggancio tabella VPO record DECOFI816TEL
012390140130     c                   Clear                   Tibs02ds
012400140130     c                   Clear                   fil816tel
012410140130     c                   Eval      T02Mod = 'C'
012420140130     c                   Eval      T02Sif = knsif
012430140130     c                   Eval      T02Cod = 'VPO'
012440140130     c                   Movel(p)  'DECOFI816TEL'T02Ke1
012450140130     c                   Call      'TIBS02R'
012460140130     c                   Parm                    Kpjba
012470140130     c                   Parm                    Tibs02ds
012480140130     c                   If        T02Err = *Blanks
012490140130     c                   Movel     T02Uni        fil816tel
012500140130     c                   EndIf
012510150414
012520150414      * Aggancio tabella "VPO" "EXPO"
012530151119     C*                  CLEAR                   tibs02ds
012540151119     c*                  clear                   fil_expo
012550151119     C*                  MOVEL     'C'           T02MOD
012560151119     C*                  MOVEL     KNSIF         T02SIF
012570151119     C*                  MOVEL     'VPO'         T02COD
012580151119     c*                  movel(P)  'EXPO'        t02ke1
012590151119     C*                  CALL      'TIBS02R'
012600151119     C*                  PARM                    KPJBA
012610151119     C*                  PARM                    tibs02ds
012620151119    2C*                  IF        T02ERR = *BLANKS
012630151119     C*                  MOVEa     T02uni        fil_expo
012640151119    2C*                  ENDIF
012650141029
012660141029      * Aggancio tabella VPO record DECOFI816DIR
012670141029     c                   clear                   trulvpods
012680141029     c                   eval      ivpoke1 = 'DECOFI816DIR'
012690141029     c                   call      'TRULVPOR'
012700141029     c                   parm                    trulvpods
012710141029     c                   eval      skfilokdir=skfilok
012720030710
012730030710      * Aggancio tabella MIA
012740030710     c                   Clear                   miapsw
012750030710     c                   Clear                   Tibs02ds
012760030710     c                   Eval      T02Mod = 'C'
012770030710     c                   Eval      T02Sif = knsif
012780030710     c                   Eval      T02Cod = 'MIA'
012790030710     c                   Movel(p)  'TUTTI'       T02Ke1
012800030710     c                   Call      'TIBS02R'
012810030710     c                   Parm                    Kpjba
012820030710     c                   Parm                    Tibs02ds
012830030710     c                   If        T02Err = *Blanks
012840030710     c                   Movel     T02Uni        miapsw
012850030710     c                   EndIf
012860160318      * Aggancio tabella NSD (per Import DPD "VEDI PACCO")
012870160318     c                   clear                   dnsd
012880160318     c                   Clear                   Tibs02ds
012890160318     c                   Eval      T02Mod = 'C'
012900160318     c                   Eval      T02Sif = knsif
012910160318     c                   Eval      T02Cod = 'NSD'
012920160318     c                   Movel(p)  '1'           T02Ke1
012930160318     c                   Call      'TIBS02R'
012940160318     c                   Parm                    Kpjba
012950160318     c                   Parm                    Tibs02ds
012960160318     c                   If        T02Err = *Blanks
012970160318     c                   Movel     T02Uni        dnsd
012980160318     c                   EndIf
012990031120
013000031120      * Aggancio tabella AR5 per record 'GEN'
013010031120     c                   Clear                   dAr5
013020031120     c                   Clear                   Tibs02ds
013030031120     c                   Eval      T02Mod = 'C'
013040031120     c                   Eval      T02Sif = knsif
013050031120     c                   Eval      T02Cod = 'AR5'
013060031120     c                   Movel(p)  'GEN'         T02Ke1
013070031120     c                   Call      'TIBS02R'
013080031120     c                   Parm                    Kpjba
013090031120     c                   Parm                    Tibs02ds
013100031120     c                   If        T02Err = *Blanks
013110031120     c                   Movel     T02Uni        dAr5
013120031120     c                   EndIf
013130040806      * Tabella 2G
013140040806     c                   Movel     '2G'          cod
013150040902     c                   Movel(p)  '2'           key
013160040806     c                   ExSr      ctabel
013170040902     c  n30              Movel     TblUni        ds2g2
013180040902     c   30              Clear                   ds2g2
013190050901      *
013200941222     C***
013210931119     C* DEFINIZIONE CAMPI
013220931119     C***
013230130411     C**** *LIKE         DEFINE    D66BDI        §7LFIN
013240991112     C     *LIKE         DEFINE    D66FFI        §7LFFI
013250991112     C     *LIKE         DEFINE    DATEU         WDATA
013260011010     C     *LIKE         DEFINE    §CVDEC        COMDEC
013270990702     C     *LIKE         DEFINE    AR6IMV        SAVIMV
013280151016     C     *LIKE         DEFINE    AR6IMV        SAVIMV_fat
013290990702     C     *LIKE         DEFINE    FVVNPG        KNPG
013300170116     C***  *LIKE         DEFINE    SOVR(1)       SOARBT
013310170116     C***  *LIKE         DEFINE    SOVR(1)       SAARBT
013320170116     C***  *LIKE         DEFINE    SOVR(1)       SDARBT
013330941213     C     *LIKE         DEFINE    AR6SV1        WSV1
013340941212     C     *LIKE         DEFINE    AR6VA1        WVA1
013350941212     C     *LIKE         DEFINE    TAMPPA        WPPA
013360941212     C     *LIKE         DEFINE    TAMMPA        WMPA
013370050202     C     *LIKE         DEFINE    ilv59FIE      WFIE
013380941209     C     *LIKE         DEFINE    §15EFT        DESEFT
013390941209     C     *LIKE         DEFINE    TBLKUT        §KUT
013400911211     C     *LIKE         DEFINE    TBLCOD        §COD
013410911211     C     *LIKE         DEFINE    TBLKEY        §KEY
013420941212     C     *LIKE         DEFINE    ARBIAS        VARIAS
013430950426     C     *LIKE         DEFINE    ARBIAS        COMIAS
013440930507     C     *LIKE         DEFINE    ARBCTR        COMCTR
013450941220     C     *LIKE         DEFINE    ARBKSC        KSC2
013460950104     C     *LIKE         DEFINE    ARBFAP        COMFAP
013470950407     C     *LIKE         DEFINE    NEWCBO        SAVCBO
013480950419     C     *LIKE         DEFINE    VIDPID        WPIVA
013490011009      *
013500011120     C     *LIKE         DEFINE    §GEDFC        DFCSAV
013510011220     C     *LIKE         DEFINE    §GEimf        imfsav
013520011009     C     *LIKE         DEFINE    §CVDEC        DECSAV
013530011009      *
013540941220     C* PER I PADRONCINI
013550950113     C     *LIKE         DEFINE    ARBKSC        WKSC
013560950113     C     *LIKE         DEFINE    ARBRSM        WRSM
013570950113     C     *LIKE         DEFINE    ARBRSD        OLDRSD
013580150206     C     *LIKE         DEFINE    ARBlod        OLDlod
013590950113     C     *LIKE         DEFINE    ARBCAD        OLDCAD
013600950113     C     *LIKE         DEFINE    ARBFIN        OLDFIN
013610941209     C     *LIKE         DEFINE    ARBVLF        OLDVLF
013620950114     C     *LIKE         DEFINE    ARBFVF        OLDFVF
013630941209     C     *LIKE         DEFINE    ARBPKF        OLDPKF
013640950113     C     *LIKE         DEFINE    ARBRSM        OLDRSM
013650950113     C     *LIKE         DEFINE    ARBCAM        OLDCAM
013660950113     C     *LIKE         DEFINE    ARBKSC        OLDKSC
013670941220     C*
013680950113     C     *LIKE         DEFINE    ARBVLF        WVOL
013690950113     C     *LIKE         DEFINE    ARBVLF        WSOTV
013700941212     C     *LIKE         DEFINE    ARBVLF        WADDV
013710941215     C     *LIKE         DEFINE    ARBVLF        VARVLF
013720941215     C     *LIKE         DEFINE    ARBPKF        VARPKF
013730950113     C     *LIKE         DEFINE    ARBPKF        WPKG
013740941220     C* PER FAR RIDIGITARE L'IMPORTO
013750941215     C     *LIKE         DEFINE    ARBVAS        VARVAS
013760941215     C     *LIKE         DEFINE    ARBVAD        VARVAD
013770941213     C     *LIKE         DEFINE    AR9VCA        VARVCA
013780950918     C* PER CONTROLLARE SE E' MODIFICATO
013790950918     C     *LIKE         DEFINE    AR9VCA        SAVVCA
013800950918     C     *LIKE         DEFINE    AR9TIC        SAVTIC
013810950918     C     *LIKE         DEFINE    AR9CAS        SAVCAS
013820970808     C     *LIKE         DEFINE    VIDCAD        SAVCAD
013830970808     C     *LIKE         DEFINE    VIDCAD        WCAD
013840970808     C     *LIKE         DEFINE    VIDCAD        WCAD1
013850970808     C     *LIKE         DEFINE    VIDLOD        SAVLOD
013860970808     C     *LIKE         DEFINE    VIDLOD        WLOD
013870970808     C     *LIKE         DEFINE    VIDLOD        WLOD1
013880060111     c     *LIKE         DEFINE    VIDrsd        Wrsd1
013890060111     c     *LIKE         DEFINE    VIDrsd        savrsd
013900061116     c     *LIKE         DEFINE    VIDrsd        savrsdQ
013910060111     c     *LIKE         DEFINE    VIDind        wind1
013920060111     c     *LIKE         DEFINE    VIDind        savind
013930060111     c     *LIKE         DEFINE    VIDprd        wprd1
013940060111     c     *LIKE         DEFINE    VIDprd        savprd
013950060111     c     *LIKE         DEFINE    VIDnzd        wnzd1
013960060111     c     *LIKE         DEFINE    VIDnzd        savnzd
013970970808     C     *LIKE         DEFINE    E14CAP        VARCAD
013980970808     C     *LIKE         DEFINE    E14LOC        VARLOD
013990970808     C     *LIKE         DEFINE    E14CAP        VARCAM
014000970808     C     *LIKE         DEFINE    E14LOC        VARLOM
014010991006     C     *LIKE         DEFINE    AR6POR        VARPOR
014020991006     C     *LIKE         DEFINE    AR6VA1        VA2VA1
014030941220     C* PER IL CONTROLLO
014040941212     C     *LIKE         DEFINE    AR9VCA        WVLT
014050941212     C     *LIKE         DEFINE    AR4NOT        SAVRD2
014060941019     C     *LIKE         DEFINE    AR4TRC        WTRC
014070950424     C     *LIKE         DEFINE    VIDQFT        VARQFT
014080950424     C     *LIKE         DEFINE    VIDKLP        VARKLP
014090950424     C     *LIKE         DEFINE    VIDKSC        VARKSC
014100970418     C     *LIKE         DEFINE    §7TSOZ        WSOSC
014110970418     C     *LIKE         DEFINE    ARBFVF        WFVF
014120970813     C     *LIKE         DEFINE    VIDKSC        MEMKSC
014130991005     C     *LIKE         DEFINE    VIDDIV        DIVGEI
014140991006     C     *LIKE         DEFINE    VIDDIV        WDIV
014150991007     C     *LIKE         DEFINE    AR6DFT        DFT2BO
014160950113     C*
014170950113     C     *LIKE         DEFINE    FTDDDC        WDRT
014180950113     C     *LIKE         DEFINE    FTDPDR        WPDR
014190950113     C     *LIKE         DEFINE    FTDNDC        WNDC
014200950113     C     *LIKE         DEFINE    FTDDDC        WDDC
014210950113     C     *LIKE         DEFINE    FTDTSR        WTSR
014220020327     C     *LIKE         DEFINE    FTDSTP        wstp
014230950114     C* CAMPI COLLEGATI ALLA VARIAZIONE DEL MITTENTE DA STORICIZZARE
014240950114     C     *LIKE         DEFINE    ARBFVF        MITFVF
014250950114     C     *LIKE         DEFINE    ARBVLF        MITVLF
014260950114     C     *LIKE         DEFINE    ARBCTR        MITCTR
014270950601     C* CAMPI PER GESTIONE VARIAZIONI DI BOLLE ESTERE NON LOCALI
014280950601     C     *LIKE         DEFINE    ARBLNA        WEUV
014290950601     C     *LIKE         DEFINE    ARBCVB        PARCVB
014300950601     C     *LIKE         DEFINE    ARBDTV        PARDTV
014310950601     C     *LIKE         DEFINE    ARBORV        PARORV
014320950601     C     *LIKE         DEFINE    ARBAAS        PARAAS
014330950601     C     *LIKE         DEFINE    ARBLNP        PARLNP
014340950601     C     *LIKE         DEFINE    ARBNRS        PARNRS
014350950601     C     *LIKE         DEFINE    ARBNSP        PARNSP
014360950601     C     *LIKE         DEFINE    ARBCVB        PARCVC
014370950601     C     *LIKE         DEFINE    SIMFEL        PARFEV
014380130410     C***  *LIKE         DEFINE    ARITIP        KTIP
014390001204     C     *LIKE         DEFINE    ARBTC1        SAVTC1
014400001204     C     *LIKE         DEFINE    ARBTC1        SAVTC2
014410930507     C*
014420950113     C                   MOVEL     'R'           WTSR
014430911211     C                   MOVEL     'R'           RIS               1
014440930210     C                   MOVEL     'M'           EMME
014450990818     C                   MOVEL     'C '          FTC               2
014460130410     C***                MOVEL     'A'           KTIP
014470931119     C***
014480911211     C* GIRO UDATE
014490931119     C***
014500941014     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + 3
014510941014     C                   TIME                    W0140            14 0
014520941014     C* UDATE IN GGMMAAAA
014530941014     C                   MOVE      W0140         WDTGIO            8 0
014540941014     C*
014550941014     C* UDATE IN AAAAMMGG
014560941014     C                   Z-ADD     WDTGIO        G02DAT
014570941014     C                   MOVEL     *BLANK        G02ERR
014580941014     C                   CALL      'XSRDA8'
014590941014     C                   PARM                    WLBDAT
014600941014     C                   MOVEL     G02INV        DATEU             8 0
014610030710     c     *iso          Move      dateu         wdataoggi
014620970423     C*
014630970423     C                   MOVEL     4             KNPG
014640941014     C**
014650941014     C*  KEY
014660941014     C**
014670941014     C     KARB          KLIST
014680941212     C                   KFLD                    D48AAS
014690941212     C                   KFLD                    D48LNP
014700941212     C                   KFLD                    D48NRS
014710941212     C                   KFLD                    D48NSP
014720941019     C     KARB2         KLIST
014730941212     C                   KFLD                    D48AAS
014740941212     C                   KFLD                    D48LNP
014750941212     C                   KFLD                    D48NRS
014760941212     C                   KFLD                    D48NSP
014770941019     C                   KFLD                    WTRC
014780130410     C***  KARI          KLIST
014790130410     C***                KFLD                    D48AAS
014800130410     C***                KFLD                    D48LNP
014810130410     C***                KFLD                    D48NRS
014820130410     C***                KFLD                    D48NSP
014830130410     C***                KFLD                    KTIP
014840941014     C     KFTT          KLIST
014850941215     C                   KFLD                    FTDPDR
014860941215     C                   KFLD                    FTDTSR
014870941215     C                   KFLD                    FTDNDC
014880950113     C                   KFLD                    FTDDDC
014890950113     C     KFTT2         KLIST
014900950113     C                   KFLD                    WPDR
014910950113     C                   KFLD                    WTSR
014920950113     C                   KFLD                    WNDC
014930950113     C                   KFLD                    WDRT
014940991005     C     KFTD          KLIST
014950950113     C                   KFLD                    WPDR
014960950113     C                   KFLD                    WTSR
014970950113     C                   KFLD                    WNDC
014980950113     C                   KFLD                    WDRT
014990950113     C                   KFLD                    WKSC
015000950113     C                   KFLD                    WRSM
015010020326     C     KFTD5         KLIST
015020020326     C                   KFLD                    WPDR
015030020326     C                   KFLD                    WTSR
015040020326     C                   KFLD                    WNDC
015050020326     C                   KFLD                    WDRT
015060020326     C                   KFLD                    D48AAS
015070020326     C                   KFLD                    D48LNP
015080020326     C                   KFLD                    D48NRS
015090020326     C                   KFLD                    D48NSP
015100941014     C* ACCESSO   TABEL                                    *
015110990930     C     KTAB1         KLIST                                                  *
015120990930     C                   KFLD                    CODUT                          *
015130990930     C                   KFLD                    COD               2            *
015140941014     C     KTAB2         KLIST                                                  *
015150941014     C                   KFLD                    CODUT                          *
015160941014     C                   KFLD                    COD               2            *
015170941014     C                   KFLD                    KEY               8            *
015180941014     C* ACCESSO   CNACO                                    *
015190941014     C     KACO          KLIST                                                  *
015200941014     C                   KFLD                    CODUT                          *
015210060131     C                   KFLD                    dutkci                         *
015220941014     C                   KFLD                    KSC               7 0          *
015230970423     C     KFVV          KLIST                                                  *
015240970423     C                   KFLD                    KNPG                           *
015250970423     C                   KFLD                    W0050                          *
015260020911     C                   KFLD                    Kfgs
015270941014     C* ACCESSO   TFTAM CON PROGRESSIVO                    *
015280941014     C*
015290941014     C     KTAM4         KLIST                                                  *
015300941014     C                   KFLD                    KSC                            *
015310941014     C                   KFLD                    COMCTR                         *
015320941014     C                   KFLD                    COMPRG            3 0          *
015330941014     C     KTAM6         KLIST                                                  *
015340941014     C                   KFLD                    KSC2                           *
015350941014     C                   KFLD                    COMCTR                         *
015360941014     C                   KFLD                    COMPRG                         *
015370941014     C     KTPT          KLIST                                                  *
015380941014     C                   KFLD                    KSC                            *
015390941014     C                   KFLD                    COMCTR                         *
015400941014     C                   KFLD                    COMPRG                         *
015410990818     C                   KFLD                    FTC               2            *
015420030124
015430030124     c     kAr5          Klist
015440030124     c                   Kfld                    D48Aas
015450030124     c                   Kfld                    D48Lnp
015460030124     c                   Kfld                    D48Nrs
015470030124     c                   Kfld                    D48Nsp
015480030124     c                   Kfld                    kAr5Trd
015490030722
015500030722     C     kazcln        klist
015510030722     C                   kfld                    ktfp
015520030722     C                   kfld                    ktfa
015530030722     C                   kfld                    kann
015540030722     C                   kfld                    kmes
015550040809
015560040809     C* CHIAVE SU FNDCT01L da *PGM utilità FIDN12R - Completa
015570080701     C***  KDCT1_C       KLIST
015580080701     C***                KFLD                    dsaac
015590080701     C***                KFLD                    dsfil
015600080701     C***                KFLD                    dsnca
015610041020     C* CHIAVE SU Fiqdt01L
015620041020     C     Kqdt          KLIST
015630041020     C                   KFLD                    kfgs
015640041020     C                   KFLD                    w0060
015650050601     c*
015660050601     C* CHIAVE SU Tita430C
015670050601     C     Kta4          KLIST
015680050601     C                   KFLD                    D48AAS
015690050601     C                   KFLD                    D48LNP
015700050601     C                   KFLD                    D48NRS
015710050601     C                   KFLD                    D48NSP
015720050601     c                   kfld                    kta4trc
015730141029
015740141029     C     Kidc1         KLIST
015750141029     C                   KFLD                    D48AAS
015760141029     C                   KFLD                    D48LNP
015770141029     C                   KFLD                    D48NRS
015780141029     C                   KFLD                    D48NSP
015790141029     C                   KFLD                    w0140I
015800030722
015810941212     C**
015820941212     C                   MOVEL     W256A         KPJBU
015830001213     C*
015840001213     C                   MOVE      '0'           OPN48             1
015850911211     C                   ENDSR
015860120426     C*
015870120426      * APERTURA TITA430C
015880120426     c* se provengo da variazione bolla di sede non ho bisogno del tita4
015890120426     C     OPN_TITA4     BEGSR
015900120426     c                   if        %subst(d48cvb:1:1)<>'S'
015910151016     C                   if        NOT %OPEN(TITA430C)
015920120426      * Se sono in ambiente buono - GAITRAGRU
015930141020     c*****              If        knsif = 'FILTRA201 '
015940141020     c                   If        %subst(knsif:7:1) <> 'P'
015950120426     c                   Eval      wlib = 'GAITRAGRU '
015960120426      *  Se sono in ambiente di prova - GAITRAGRPS
015970120426     c                   Else
015980120426     c                   Eval      wlib = 'GAITRAGRPS'
015990120426     c                   EndIf
016000120426     c*
016010120426     c                   Clear                   commt
016020151016     c                   Movel(p)  cmdt(1)       commt
016030120426     c                   Eval      %Subst(commt:30:10) = wlib
016040120426     c                   Eval      commt =
016050151016     c                             %trim(commt) + '/TITA430C)'
016060120426     c                   Call      'QCMDEXC'
016070120426     c                   Parm                    commt
016080120426     c                   Parm                    lenght
016090151016     c                   Open      tita430c
016100120426     c                   endif
016110151016     c* FIAR531C
016120151016     c                   if        not %open(fiar531c)
016130151016     c                   If        %subst(knsif:7:1) <> 'P'
016140151016     c                   Eval      wlib = 'GAITRAGRU '
016150151016      *  Se sono in ambiente di prova - GAITRAGRPS
016160151016     c                   Else
016170151016     c                   Eval      wlib = 'GAITRAGRPS'
016180151016     c                   EndIf
016190151016     c*
016200151016     c                   Clear                   commt
016210151016     c                   Movel(p)  cmdt(2)       commt
016220151016     c                   Eval      %Subst(commt:30:10) = wlib
016230151016     c                   Eval      commt =
016240151016     c                             %trim(commt) + '/FIAR531C)'
016250151016     c                   Call      'QCMDEXC'
016260151016     c                   Parm                    commt
016270151016     c                   Parm                    lenght
016280151016     c                   Open      fiar531c
016290151016     c                   endif
016300151016     c* TITAS30C
016310151016     C                   if        NOT %OPEN(TITAS30C)
016320151016      * Se sono in ambiente buono - GAITRAGRU
016330151016     c                   If        %subst(knsif:7:1) <> 'P'
016340151016     c                   Eval      wlib = 'GAITRAGRU '
016350151016      *  Se sono in ambiente di prova - GAITRAGRPS
016360151016     c                   Else
016370151016     c                   Eval      wlib = 'GAITRAGRPS'
016380151016     c                   EndIf
016390151016     c*
016400151016     c                   Clear                   commt
016410151016     c                   Movel(p)  cmdt(3)       commt
016420151016     c                   Eval      %Subst(commt:30:10) = wlib
016430151016     c                   Eval      commt =
016440151016     c                             %trim(commt) + '/TITAS30C)'
016450151016     c                   Call      'QCMDEXC'
016460151016     c                   Parm                    commt
016470151016     c                   Parm                    lenght
016480151016     c                   Open      titas30c
016490151016     c                   endif
016500151016     c                   endif
016510120426     C                   ENDSR
016520900509     C*
016530050907      *--- CONTROLLO LA PARTICOLARITA' GIACENZA NEL PERIODO DELLE TRAZIONI RIDOTTE ---*
016540050907     C     Contr7q       BEGSR
016550050901      *
016560050907      * Inizializzo un flag che indica se è consentita la variazione della data cons. richiesta
016570050907      * in periodo d trazioni ridotte ed in presenza d particolarità consegna genericamente "G8"
016580050907     c                   clear                   wDCRG8            1
016590050901      * Determino se variazione di bolla con part. giacenza durante
016600050901      *  periodo delle trazioni ridotte
016610050901     c                   If        dateu >= §2g2dri and
016620050901     c                             dateu <= §2g2drf
016630050901      * posso modificare se arbgga <> *Blanks e se in tabella 7Q ha 'R' come cod. disposizione
016640050901     c                   If        ArbGga <> *Blanks
016650050901     c                   Movel     '7Q'          cod
016660050901     c                   Movel(p)  ArbGga        key
016670050901     c                   ExSr      ctabel
016680050901     c  n30              Movel     TblUni        ds7q
016690050901     c   30              Clear                   ds7q
016700050901     c                   If        §7q4d1 = 'R' or §7q4d2 = 'R' or
016710050901     c                             §7q4d3 = 'R' or §7q4d4 = 'R' or
016720050901     c                             §7q4d5 = 'R'
016730050901     c                   Movel     'S'           wDCRG8
016740050901     c                   EndIF
016750050901     c                   EndIF
016760050901     c                   EndIF
016770050901
016780050901     c                   EndSR
016790080201     c
016800900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
016810900509     C     CONTR         BEGSR
016820030122
016830030122     c     Riprova       Tag
016840030122
016850941014     C                   SETOFF                                       90
016860941014     C                   SETOFF                                       203519
016870941014     C                   SETOFF                                       253403
016880950609     C                   SETOFF                                       262729
016890990415     C                   SETOFF                                       73
016900151222     c                   clear                   wfile
016910941014     C***
016920941014     C* VEDO SE E' UN PROGRAMMA RICHIAMATO
016930941014     C***
016940941019     C     D48TRC        IFNE      *BLANKS
016950941019     C                   SETON                                        20
016960950114     C                   ELSE
016970941019     C                   ENDIF
016980950607     C***
016990950607     C* ABILITO L'ENTER COME TASTO DI AGGIORNAMENTO SE RICHIESTO
017000950607     C***
017010950607     C     D48FFR        IFNE      'V'
017020070309     C     D48FFR        andne     'F'
017030950607     C                   SETON                                        26
017040950607     C                   ENDIF
017050070309     c
017060070309     c* Se richiamo con F  --> mi imposto la E  (ma con 26 off non effettua
017070070309     c*    l'aggiornamento per ENTER)
017080070309     c                   if        d48ffr='F'
017090070309     c                   eval      d48ffr='E'
017100070309     c                   endif
017110070309     c
017120950607     C***
017130950607     C* ABILITO F12 SE RICHIESTO
017140950607     C***
017150020603     C     W48F12        IFEQ      'S'
017160950607     C                   SETON                                        27
017170950607     C                   ENDIF
017180950609     C***
017190950609     C* PROTEGGO TUTTI I CAMPI SE RICHIAMATO DA PGM PER ACQUISIZIONE
017200950609     C* AUTOMATICA VARIAZIONI DA PC ESTERO
017210950609     C***
017220140404     C***  D48EUV        IFEQ      'N'
017230140404     C***                SETON                                        29
017240140404     C***                END
017250941222     C**
017260941222     C* APRO I FILE A SECONDA DEL TIPO BOLLA
017270941222     C**
017280941222     C     D48TBO        IFEQ      'A'
017290941223     C                   SETON                                        10
017300941222     C                   ELSE
017310941223     C                   SETOFF                                       10
017320941222     C                   ENDIF
017330941222     C**
017340930505     C* SPEDIZIONE
017350941222     C**
017360151218     c***                if        d48cvb<>'SD' and d48cvb<>'SF'
017370151218     C***10KARB          CHAIN     FNARB01L                           3031
017380151218     C**N10KARB          CHAIN     FNBLP01L                           3031
017390930505     C*
017400930505     C* NON UTILIZZABILE
017410151218     C***  *IN31         IFEQ      *ON
017420151218     C***                MOVEL     MSG(1)        D48MSG
017430151218     C***                MOVEL     '1'           D48ERR
017440151218     C***                SETON                                        90
017450151218     C***                GOTO      ENDCTR
017460151218     C***                ENDIF
017470941223     C* NON TROVATA SPEDIZIONE
017480151218     C***  *IN30         IFEQ      *ON
017490151218     C***                MOVEL     MSG(61)       D48MSG
017500151218     C***                MOVEL     '1'           D48ERR
017510151218     C***                SETON                                        90
017520151218     C***                GOTO      ENDCTR
017530151218     C***                ENDIF
017540151218     c***                endif
017550080201     C*
017560080201     C* SE PGM RICHIAMATO CONTROLLO SOLO LA CORRETTEZZA DELLA CAUSALE
017570080201     C                   CLEAR                   Fnlr66DS
017580080201     C                   MOVEL     'C'           D66GES
017590080201     C                   MOVEL     D48CVB        D66CVB
017600080201     C  N20              MOVEL     D48AAS        D66AAS
017610080201     C  N20              MOVEL     D48LNP        D66LNP
017620080201     C  N20              MOVEL     D48NRS        D66NRS
017630080201     C  N20              MOVEL     D48NSP        D66NSP
017640080201     C  N20              MOVEL     D48TBO        D66TBO
017650160115     c                   eval      d66bnp='S'
017660080201     C                   MOVEL     Fnlr66DS      KPJBU
017670080201     C                   CALL      'FNLR66R'
017680080201     C                   PARM                    KPJBA
017690080201     C                   MOVEL     KPJBU         Fnlr66DS
017700080201     C*
017710080201     C* TROVATO ERRORE
017720080201     C     D66ERR        IFEQ      '1'
017730080201      * se prima ho variato il destinatario ed ora devo aggiornare le consegne particolari
017740080201      * in caso di errore provo a cambiare la causale di modifica da 'CP' a 'CR'
017750080201     c                   If        wI0 = 'S'
017760080201     c                   Movel     'CR'          D48Cvb
017770080201     c                   Clear                   wI0
017780080201     c                   Goto      Riprova
017790080201     c                   Else
017800080201     C                   MOVEL     D66MSG        D48MSG
017810080201     C                   MOVEL     D66ERR        D48ERR
017820080201     C                   SETON                                        90
017830151218     c** 10              unlock    fnarb01l
017840151218     c**n10              unlock    fnblp01l
017850080201     c
017860080201     C                   GOTO      ENDCTR
017870080201     c                   EndIf
017880080201     C                   ENDIF
017890141112     c* Le variazioni I0 non sono ammesse se presenti dispo di dirottamento
017900141112     c                   if        d48cvb='I0' or d48cvb='I7' or
017910141112     c                             d48cvb='I8' or d48cvb='I9'
017920141112     c                   clear                   fnlry09ds2
017930141112     c                   eval      ILRY09TCH='T'
017940141112     c                   eval      ILRY09TDIS='D'
017950141112     c                   eval      ILRY09AAS=d48aas
017960141112     c                   eval      ILRY09LNP=d48lnp
017970141112     c                   eval      ILRY09NRS=d48nrs
017980141112     c                   eval      ILRY09NSP=d48nsp
017990141112     c                   eval      kpjbu=fnlry09ds2
018000141112     c                   call      'FNLRY09C'
018010141112     c                   parm                    kpjba
018020141112     c                   eval      fnlry09ds2=kpjbu
018030141112     c                   if        OLRY09ESIT='1'
018040141112     C                   MOVEL     msg(156)      D48MSG
018050141112     C                   MOVEL     '1'           D48ERR
018060141112     C                   SETON                                        90
018070151218     c** 10              unlock    fnarb01l
018080151218     c**n10              unlock    fnblp01l
018090141112     c
018100141112     C                   GOTO      ENDCTR
018110141112     c                   endif
018120141112     c                   endif
018130151218     C**
018140151218     C* SPEDIZIONE
018150151218     C**
018160151218     c                   if        d48cvb<>'SD' and d48cvb<>'SF'
018170151221     c* Manutenzione bolle in arrivo
018180151221     c                   if        *in10
018190151221     c*
018200151221     c                   setoff                                       30
018210151221     c                   eval      wfile='A'
018220151221     c                   if        d66utp<>'P'
018230151221     C     KARB          CHAIN     FNARB01L                           3031
018240151221     c                   endif
018250151221     c                   if        (*in30 and d66utp='T') or d66utp='P'
018260151221     C     KARB          CHAIN     FNblp01L                           3031
018270160112     c                   if        not *in31 and not*in30  and ARBft1<>*blanks
018280151221     c                   unlock    fnblp01l
018290151221     C                   MOVEL     MSG(162)      D48MSG
018300151221     C                   MOVEL     '1'           D48ERR
018310151221     C                   SETON                                        90
018320151221     C                   GOTO      ENDCTR
018330151221     c                   endif
018340151221     c                   eval      wfile='P'
018350151221     c                   endif
018360151221     c                   else
018370151221     c* Manutenzione in partenza
018380151222     c                   eval      wfile='P'
018390151221     C     KARB          CHAIN     FNBLP01L                           3031
018400151221     c                   endif
018410151218     C*
018420151218     C* NON UTILIZZABILE
018430151218     C     *IN31         IFEQ      *ON
018440151218     C                   MOVEL     MSG(1)        D48MSG
018450151218     C                   MOVEL     '1'           D48ERR
018460151218     C                   SETON                                        90
018470151218     C                   GOTO      ENDCTR
018480151218     C                   ENDIF
018490151218     C* NON TROVATA SPEDIZIONE
018500151218     C     *IN30         IFEQ      *ON
018510151218     C                   MOVEL     MSG(61)       D48MSG
018520151218     C                   MOVEL     '1'           D48ERR
018530151218     C                   SETON                                        90
018540151218     C                   GOTO      ENDCTR
018550151218     C                   ENDIF
018560151218     c                   endif
018570080201     C* KEY SPEDIZIONE
018580080201     C                   MOVEL     D48CVB        VIDCVR
018590080201     C                   MOVEL     D48CVB        FLGCVR            1
018600080201     C*
018610080201     C* IMPOSTO IL CAMPO §7LFFI CHE MI SERVE PER ALLOCARE I FILE
018620080201     C                   MOVEL     D66FFI        §7LFFI
018630080201     C*
018640080201     C* IMPOSTO IL CAMPO §7LFIN CHE MI SERVE PER PROTEGGERE I CAMPI
018650080201     C* CHE NON DEVO VARIARE
018660130411     C***                MOVEL     D66BDI        §7LFIN
018670941014     C**
018680941014     C**
018690900509     C     ENDCTR        ENDSR
018700910429     C*
018710910429     C*--- CARICO DATI BOLLA -----------------------------------------*
018720910429     C     CARICA        BEGSR
018730920123     C                   SETOFF                                       040521
018740911211     C                   SETOFF                                       131415
018750990930     C                   SETOFF                                       0102
018760941220     C                   SETOFF                                       363789
018770941213     C                   SETOFF                                       111209
018780991001     C                   SETOFF                                       882307
018790941222     C                   SETOFF                                       525306
018800991012     C                   SETOFF                                       167308
018810061107     C                   SETOFF                                       747576
018820131001     C                   SETOFF                                       878681
018830140213     C                   SETOFF                                       838477
018840140408     c                   setoff                                       68
018850011018      *
018860011018     C                   CLEAR                   FLGVRN
018870110906     C                   CLEAR                   lnacons
018880941102     C*
018890950120     C                   CLEAR                   WIN95
018900950120     C                   CLEAR                   WIN97
018910950120     C                   CLEAR                   WIN93
018920131016     C                   CLEAR                   WINF14
018930950120     C                   CLEAR                   VARVCA
018940941215     C                   CLEAR                   VARVAS
018950941215     C                   CLEAR                   VARVAD
018960941215     C                   CLEAR                   VARVLF
018970941215     C                   CLEAR                   VARPKF
018980990702     C                   CLEAR                   SAVIMV
018990151016     C                   CLEAR                   SAVIMV_fat
019000991006     C                   CLEAR                   WDIV
019010991006     C                   CLEAR                   VARPOR
019020991006     C                   CLEAR                   VA2VA1
019030991012     C                   CLEAR                   SAVCBO
019040091124     C                   CLEAR                   WPT1              1
019050941209     C                   MOVEL     0             CIN               1 0
019060941209     C                   MOVEL     0             DIE               1 0
019070941212     C                   MOVEL     0             SEI               1 0
019080941214     C                   MOVEL     0             SET               1 0
019090941223     C                   MOVEL     0             OTT               1 0
019100970808     C                   MOVEL     0             WZV               1 0
019110970808     C                   MOVEL     0             WZ1               1 0
019120970808     C                   MOVEL     0             WZ2               1 0
019130070508     C                   MOVEL     0             WZ3               1 0
019140970808     C                   MOVEL     0             WZVM              1 0
019150970808     C                   MOVEL     0             WZ1M              1 0
019160970808     C                   MOVEL     0             WZ2M              1 0
019170141029     C                   MOVEL     0             Wforzdcr          1 0
019180141112     C                   MOVEL     0             Wforzgiac         1 0
019190990930     C                   MOVEL     ' '           WAR62             1
019200941019     C                   MOVEL     ' '           WAR6              1
019210941209     C                   MOVEL     ' '           WAR7              1
019220040123     c                   Clear                   War72
019230941019     C                   MOVEL     ' '           WAR9              1
019240941019     C                   MOVEL     ' '           WAR4D             1
019250150206     C                   MOVEL     ' '           WAR4X             1
019260061116     C                   MOVEL     ' '           WAR4Q             1
019270000623     C                   MOVEL     ' '           WBL4W             1
019280920319     C                   Z-ADD     0             COMKSC
019290930507     C                   Z-ADD     0             COMCTR
019300930507     C                   Z-ADD     0             COMPRG
019310911211     C                   Z-ADD     0             VI2DFT
019320911211     C                   Z-ADD     0             VIDDFT
019330941209     C                   Z-ADD     0             VARIAS
019340930507     C                   MOVEL     *BLANKS       VARCTR            3
019350941214     C                   CLEAR                   WSOST
019360030710     c                   Clear                   wokpsw
019370030729
019380030729     c                   Clear                   V1cBan
019390030729     c                   Clear                   V1cCor
019400030729     c                   Eval      *In70 = *Off
019410040123     c                   Clear                   ContaSv
019420040123     c                   Clear                   TotImv
019430060223     C                   CLEAR                   V1CCAS
019440060223     C                   CLEAR                   V1CTIC
019450060223     C                   CLEAR                   V1CVCA
019460060223     C                   CLEAR                   SAVCAS
019470060223     C                   CLEAR                   SAVTIC
019480060223     C                   CLEAR                   SAVVCA
019490090914     C                   CLEAR                   SAVrd2
019500150203     C                   CLEAR                   normlod
019510941215     C* PULIZIA DEI FORMATI VIDEO
019520950210     C                   CLEAR                   LR48D02
019530941215     C                   CLEAR                   LR48C03
019540941215     C                   CLEAR                   LR48D04
019550941215     C                   CLEAR                   LR48D05
019560941215     C                   CLEAR                   LR48D07
019570950104     C                   CLEAR                   LR48D08
019580961211     C                   CLEAR                   LR48D09
019590030124     c                   Clear                   Lr48d10
019600950213     C                   CLEAR                   V1CTIC
019610950213     C                   CLEAR                   V1CCAS
019620950213     C                   CLEAR                   V1CVCA
019630950213     C                   CLEAR                   V1CNT1
019640950213     C                   CLEAR                   V1CNT2
019650950213     C                   CLEAR                   V1CTC1
019660950213     C                   CLEAR                   V1DTC1
019670950213     C                   CLEAR                   V1CTC2
019680950213     C                   CLEAR                   V1DTC2
019690030124     c                   Clear                   VidNom
019700030124     c                   Clear                   VidTel
019710030124     c                   Clear                   VidMot
019720030122     c                   Clear                   wI0
019730031120     c                   Clear                   VidRef
019740070508     c                   Clear                   Vidtfd
019750031120     c                   Clear                   VidTeld
019760070306     c                   Clear                   AggioARP
019770070308     c                   Clear                   savdcr
019780070308     c                   Clear                   savtcr
019790070308     c                   Clear                   savhcr
019800070308     c                   Clear                   bollafbc
019810070309     c                   Clear                   Msgforza          3 0
019820150112     c                   Clear                   Msgforzatxt      78
019830070309     c                   clear                   eveADR            1
019840070910     c                   clear                   WesAR5GEN         1
019850130930     c                   clear                   WesAR5EMD         1
019860071025     c                   clear                   ForzaCDF
019870071025     c                   clear                   ForzaPID
019880071025     c                   clear                   ForzaPID2
019890071025     c                   clear                   sav11pid
019900071026     c                   clear                   AggioBol
019910111025     c                   clear                   Dcrindist         1
019920151019     c                   clear                   PesVolPkcn
019930151019     c                   clear                   PesVolSpc
019940151019     c                   clear                   PesVolImv
019950151019     c                   clear                   TassatoImv
019960061129     c  n10              movel     'T'           bollaft2
019970061129     c   10              movel     'R'           bollaft2
019980160330     c                   clear                   wtogli
019990111207     c* ES - Per il momento lo imposto sempre = "S" in quanto l'ufficio qualità non è
020000111207     c*  sicuro di voler fare la modifica di bloccare la manut coonsegna richiesta
020010111207     c*  dalla manutenzione normale
020020111207     c                   eval      Dcrindist='S'
020030941223     C*
020040941223     C     D48TBO        IFEQ      'A'
020050941223     C                   SETON                                        10
020060941223     C                   ELSE
020070941223     C                   SETOFF                                       10
020080941223     C                   ENDIF
020090941223     C*
020100941223     C* CLEARO I FORMATI DELLE BOLLE
020110051118     C                   CLEAR                   FiAR9000
020120990930     C                   CLEAR                   FIAR7000
020130990930     C                   CLEAR                   FIAR6000
020140030124     c                   Clear                   Fiar5000
020150941220     C**
020160941220     C* PGM RICHIAMATO
020170941220     C**
020180941220     C                   SELECT
020190941220     C* PROGRAMMA RICHIAMATO    DSARBD
020200941220     C     D48TRC        WHENEQ    'D'
020210941220     C                   SETON                                        2034
020220941220     C* PROGRAMMA RICHIAMATO    DSARBK
020230941220     C     D48TRC        WHENEQ    'K'
020240950616     C                   SETON                                        203529
020250990930     C* PROGRAMMA RICHIAMATO    DARBT TIPO RECORD 1
020260941220     C     D48TRC        WHENEQ    'T'
020270941220     C                   SETON                                        2025
020280990416     C*
020290941220     C* PROGRAMMA RICHIAMATO    DSARBG
020300941220     C     D48TRC        WHENEQ    'G'
020310941220     C                   SETON                                        2003
020320990930     C* PROGRAMMA RICHIAMATO    DARBT TIPO RECORD 2
020330941220     C     D48TRC        WHENEQ    '2'
020340941220     C                   SETON                                        2019
020350130930     C* PROGRAMMA RICHIAMATO    DARBN TIPO RECORD N + parm
020360130930     C     D48TRC        WHENEQ    'N'
020370130930     c                   if        %parms=7
020380130930     C                   SETON                                        20
020390131008     c                   else
020400131008     C                   MOVEL     MSG(24)       D48MSG
020410131008     C                   MOVEL     '1'           D48ERR
020420131008     c                   leavesr
020430131008     c                   endif
020440131008     c
020450941220     C                   ENDSL
020460950607     C*
020470020603     C     W48F12        IFEQ      'S'
020480950607     C                   SETON                                        27
020490950607     C                   ENDIF
020500950609     C*
020510140404     C***  D48EUV        IFEQ      'N'
020520140404     C***                SETON                                        29
020530140404     C***                END
020540950114     C**
020550950114     C* 24 ON - VARIATO IL MITTENTE
020560950114     C**
020570950114     C     WRICVL        IFEQ      'S'
020580950114     C                   SETON                                        24
020590950114     C                   ENDIF
020600941019     C**
020610941019     C* CARICO I DATI DELLA VIDEATA GENERICA DELLA BOLLA
020620941019     C**
020630941019     C* CHAIN CON CODICE BOLLA
020640941019     C                   MOVEL     '3A'          COD
020650941102     C                   MOVEL(P)  ARBCBO        KEY
020660941212     C                   EXSR      CTABEL
020670941019     C  N30              MOVEL     TBLUNI        DS3A
020680941019     C   30              CLEAR                   DS3A
020690941019     C*
020700941019     C                   MOVEL     §3ATB1        FLGTB1            1
020710941019     C                   MOVEL     §3ADES        V1DCBO                         DESCRIZIONE
020720071116     c* Verifico se il 1°tipo bolla è tassabile o no
020730071116     C                   MOVEL     'TB'          COD
020740071116     C                   MOVEL(P)  §3atb1        KEY
020750071116     C                   EXSR      CTABEL
020760071116     C  N30              MOVEL     TBLUNI        DSTB
020770071116     C   30              CLEAR                   DSTB
020780071116     c                   movel     §tbfcb        oldtb1FCB         1
020781170419
020782170419     c                   clear                   oldtb2FCB
020783170419     c                   if        §3atb2<>*blanks
020784170419     C                   MOVEL     'TB'          COD
020785170419     C                   MOVEL(P)  §3atb2        KEY
020786170419     C                   EXSR      CTABEL
020787170419     C  N30              MOVEL     TBLUNI        DSTB
020788170419     C   30              CLEAR                   DSTB
020789170419     c                   movel     §tbfcb        oldtb2FCB         1
020790170419     c                   endif
020791071116     c
020800070308     c
020810070308     c** Blocco consegna (mi serve per la consegna richiesta)
020820151221     c***10              eval      bollaFBC=arbfbc
020830151221     c                   if        *in10
020840151221     c                   if        wfile='A'
020850151221     c                   eval      bollaFBC=arbfbc
020860151221     c                   else
020870151221     c                   clear                   bollaFBC
020880151221     c                   endif
020890151221     c                   endif
020900070308     c  N10              clear                   bollaFBC
020910941019     C*
020920941019     C* SE FRANCO PROTEGGO P.IVA
020930011017     C     FLGTB1        COMP      'F'                                    05
020940941222     C   05              SETON                                            06
020950941019     C*
020960941019     C* SE ESISTE SECONDA BOLLA PERMETTO L'IMMISSIONE
020970061107     c                   clear                   ksc2bol
020980941212    1C     §3ATB2        IFNE      '  '
020990941019     C   05              SETOFF                                       05
021000941019     C* CHAIN SU 2 BOLLA
021010990930     C                   MOVEL     '2'           WTRC
021020070525     c                   if        flgcvr='2'
021030990930     C     KARB2         CHAIN     FIAR6000                           30
021040070525     c                   else
021050070525     C     KARB2         CHAIN(N)  FIAR6000                           30
021060070525     c                   endif
021070990930     C  N30              MOVEL     'E'           WAR62
021080990930     C  N30              MOVEL     AR6KSC        V1CKSC
021090991007     C  N30              Z-ADD     AR6DFT        DFT2BO
021100941019     C   30              Z-ADD     9999          V1CKSC
021110941019     C   30              MOVEL     ARBLNA        V1CLNA
021120991007     C   30              MOVE      *ZEROS        DFT2BO
021130061107     c                   movel     v1cksc        ksc2bol           7 0
021140040127      * Vedo se esiste anche il Fiar7
021150040127     c     Karb2         Setll     Fiar701l
021160040127     c                   If        %Equal(Fiar701l)
021170040127     c                   Eval      war72 = 'E'
021180040127     c                   EndIf
021190941019     C                   ELSE
021200941019     C                   MOVEL     ARBKSC        V1CKSC
021210941212    1C                   END
021220941019     C**
021230051118     C* SE PREVEDE IL C/A CHAI SU FiAR9
021240941019     C**
021250941212    1C     §3AFCA        IFNE      0
021260041022     c                   if        flgcvr='K'
021270051118     C     KARB          CHAIN     FiAR9000                           30
021280041022     c                   else
021290041022     c* non alloco record se non è variazione di C/A
021300051118     C     KARB          CHAIN(n)  Fiar9000                           30
021310041022     c                   endif
021320041022     c
021330941212    2C     *IN30         IFEQ      *OFF
021340941019     C                   MOVEL     'E'           WAR9              1
021350941019     C                   MOVEL     AR9CAS        V1CCAS
021360941019     C                   MOVEL     AR9TIC        V1CTIC
021370941102     C                   MOVEL     AR9VCA        V1CVCA
021380950918     C                   MOVEL     AR9CAS        SAVCAS
021390950918     C                   MOVEL     AR9TIC        SAVTIC
021400950918     C                   MOVEL     AR9VCA        SAVVCA
021410941212    2C                   ENDIF
021420941212    1C                   ENDIF
021430941019     C**
021440941019     C* VEDO SE CI SONO LE NOTE
021450941019     C**
021460941019     C                   MOVEL     '8'           WTRC
021470060222     C     KARB2         CHAIN(N)  FiAR4000                           30
021480941212    1C     *IN30         IFEQ      *OFF
021490941019     C                   MOVEL     AR4NOT        V1CNT1
021500941019     C*
021510941019     C                   MOVEL     '9'           WTRC
021520060222     C     KARB2         CHAIN(N)  FiAR4000                           30
021530941212    2C     *IN30         IFEQ      *OFF
021540941019     C                   MOVEL     AR4NOT        V1CNT2
021550941212    2C                   ENDIF
021560941212    1C                   ENDIF
021570970409     C* SI NO DDT
021580031028     c                   Clear                   dsbl4a
021590970409     C                   MOVEL     'A'           WTRC
021600151222     c                   if        wfile='A'
021610151222     C     KARB2         CHAIN(N)  FiAR4000                           30
021620151222    2C     *IN30         IFEQ      *OFF
021630031028     C**!!!              MOVEL     AR4NOT        V1CDDT
021640031028     c                   Movel     Ar4not        dsbl4a
021650031028     c                   Move      §b4abm        v1cddt
021660970409     C                   ELSE
021670970409     C                   MOVEL     'S'           V1CDDT
021680970409    2C                   ENDIF
021690151222     c                   else
021700151222     C                   MOVEL     ARBFSTddt     V1CDDT            1
021710151222     c                   endif
021720970409     C*
021730970409     C     V1CDDT        IFEQ      'S'
021740970409     C     V1CDDT        OREQ      'C'
021750031028     c     v1cddt        oreq      'P'
021760031028     c     v1cddt        oreq      'Q'
021770970409     C                   MOVEL     'DDT'         V1DDDT
021780970409     C                   ELSE
021790970409     C                   CLEAR                   V1DDDT
021800970409     C                   ENDIF
021810060222     C* CONTROLLO SE ESISTE RECORD W DI ar4 PER EVENTUALE
021820000623     C*   MODIFICA DI MITTENTE
021830000623     C     *IN10         IFEQ      *OFF
021840000623     C                   MOVEL     'W'           WTRC
021850060222     C     KARB2         CHAIN(N)  Fiar4000                           30
021860000623     C  N30              MOVEL     'S'           WBL4W             1
021870000623     C                   ENDIF
021880941019     C* DESCRIZIONE CAUSALE
021890941019     C                   MOVEL     D66DES        V1DCVR
021900941019     C                   MOVEL     ARBLNP        V1CLNP
021910941212     C                   MOVEL     ARBNRS        V1CNRS
021920941019     C                   MOVEL     ARBNSP        V1CNSP
021930941019     C                   MOVEL     ARBLNA        V1CLNA
021940941019     C                   MOVEL     ARBCBO        V1CCBO
021950941019     C                   MOVEL     ARBRSM        V1CRSM
021960941019     C                   MOVEL     ARBCAM        V1CCAM
021970941019     C                   MOVEL     ARBLOM        V1CLOM
021980941019     C                   MOVEL     ARBPRM        V1CPRM
021990941019     C                   MOVEL     ARBNZM        V1CNZM
022000970828     C                   MOVEL     ARBFAP        V1CFAP
022010941019     C                   MOVEL     ARBRSD        V1CRSD
022020941019     C                   MOVEL     ARBIND        V1CIND
022030941019     C                   MOVEL     ARBCAD        V1CCAD
022040941019     C                   MOVEL     ARBLOD        V1CLOD
022050941019     C                   MOVEL     ARBPRD        V1CPRD
022060941019     C                   MOVEL     ARBNZD        V1CNZD
022070970828     C                   MOVEL     ARBFIN        V1CFIN
022080941019     C                   MOVEL     ARBCTS        V1CCTS
022090941019     C                   MOVEL     ARBTC1        V1CTC1
022100941019     C                   MOVEL     ARBTC2        V1CTC2
022110941027     C                   MOVEL     ARBPKF        V1CPKF
022120941019     C                   MOVEL     ARBVLF        V1CVLF
022130941019     C                   MOVEL     ARBFVF        V1CFVF
022140030729     c                   Z-Add     ArbNcl        V1cNcl
022150141029     c                   clear                   v1dffd
022160141029     c                   if        arbffd='S'
022170141029     c                   eval      v1dffd='FermoDeposito'
022180141029     c                   endif
022190100121     c* Partita iva MITTENTE se bolla singola franco
022200100121     c                   select
022210100121     c                   when      flgtb1='F' and §3atb2=*blanks
022220100121     c                   eval      v1cpim=arbcpi
022230100121     c                   when      flgtb1='A' and §3atb2=*blanks
022240100121     c                   eval      v1cpid=arbcpi
022250100121     c* In arrivo   per bolla  doppia --> partita iva dell'assegnato
022260151222     c                   when      wfile='A'   and §3atb2<>*blanks
022270100121     c                   eval      v1cpid=arbcpi
022280100121     c* In partenza per bolla  doppia --> partita iva del mittente
022290151222     c                   when      wfile='P'   and §3atb2<>*blanks
022300100121     c                   eval      v1cpid=arbcpi
022310100121     c                   ENDSL
022320100121
022330950114     C* SALVO IL CODICE CLIENTE DELLA SPEDIZIONE
022340950114     C                   MOVEL     ARBKSC        SAVKSC            7 0
022350060511     C                   MOVEL     ARBKSC        kscbolla          7 0
022360941102     C* PRENDO LA VALUTA DAL DESTINATARIO E MITTENTE
022370941102     C                   MOVEL     '15'          COD
022380941102     C                   MOVEL(P)  ARBNZD        KEY
022390941212     C                   EXSR      CTABEL
022400941102     C  N30              MOVEL     TBLUNI        DS15
022410941102     C   30              CLEAR                   DS15
022420941102     C                   MOVEL     §15VLT        DESVLT            3
022430941212    1C     §15ITA        IFEQ      'I'
022440941102     C                   MOVEL     'I'           WDESIE            1
022450941102     C                   ELSE
022460941102     C                   MOVEL     'E'           WDESIE            1
022470941212    1C                   ENDIF
022480020305     c                   MOVEL     §15EFT        DESEFTW
022490020305     c                   MOVEL     §15EFD        DESEFDW
022500020305     c                   MOVEL     §15EFF        DESEFFW
022510971230     V* CHAIN SU AZORG CON LINEA PARTENZA
022520971230     C     ARBLNP        CHAIN     AZORG01L                           32
022530941209     C*
022540000223     C                   MOVEL     ORGDE3        OG143
022550020305     C                   MOVEL     §OGNTW        LNPNTW
022560941102     C*
022570941102     C                   MOVEL(P)  ARBNZM        KEY
022580941212     C                   EXSR      CTABEL
022590941102     C  N30              MOVEL     TBLUNI        DS15
022600941102     C   30              CLEAR                   DS15
022610941102     C                   MOVEL     §15VLT        MITVLT            3
022620941212    1C     §15ITA        IFEQ      'I'
022630941102     C                   MOVEL     'I'           WMITIE            1
022640941102     C                   ELSE
022650941102     C                   MOVEL     'E'           WMITIE            1
022660941212    1C                   ENDIF
022670001204     C**
022680051129     C* TERMINAL DI PARTENZA DELLA LNP: VEDO SE HO VDL
022690040123     c                   Clear                   og150
022700970901     C     ARBTFP        CHAIN     AZORG                              32
022710051129     C                   CLEAR                   WVDL
022720970901    1C     *IN32         IFEQ      *OFF
022730051129     C**!!!              TESTN                   DATVDL               31
022740040123     c                   Movel     Orgdf0        og150
022750120613     c                   testn                   §ogvol               31
022760970901     C*
022770040123    2C**!!!*IN31         IFEQ      '1'
022780051129     C**!!!DATVDL        ANDGT     *ZEROS
022790120613     c***!!!             If        §ogspv = 'S'
022800120613     c                   if        *in31 and §ogvol>*zeros
022810051129     C                   MOVEL     'S'           WVDL              1
022820970901    2C                   END
022830970901    1C                   END
022840941019     C**
022850941019     C* TIPO SERVIZIO
022860941019     C**
022870090604     c                   clear                   ds5e
022880941014     C                   MOVEL     '5E'          COD
022890941014     C                   MOVEL(P)  ARBTSP        KEY
022900941212     C                   EXSR      CTABEL
022910090604     C  N30              MOVEL     TBLUNI        ds5e
022920090604     C  N30              MOVEL     §5ed08        V1DTSP
022930170131     C  N30              MOVEL     §5ed03        WTSP03
022940941220     C   30              CLEAR                   V1DTSP
022950170131     C   30              CLEAR                   wtsp03
022960941019     C**
022970910429     C* DATA SPEDIZIONE
022980941019     C**
022990941014     C                   MOVEL     '3'           G02ERR
023000941014     C                   Z-ADD     ARBMGS        G02INV
023010941014     C                   MOVEL     ARBAAS        G02INV
023020941014     C                   CALL      'XSRDA8'
023030941014     C                   PARM                    WLBDAT
023040941014     C*
023050941212     C                   Z-ADD     G02DAT        V1CDSP
023060941019     C**
023070930505     C* LINEA DI ARRIVO
023080941019     C**
023090070306     C     ARBLNA        CHAIN     AZORG                              30
023100941019     C  N30              MOVEL     ORGDES        V1DLNA                          L.ARRIVO
023110941019     C   30              MOVEL     *BLANKS       V1DLNA
023120000202     C                   MOVEL     ORGDE3        OG143
023130020305     C                   MOVEL     §OGNTW        LNANTW
023140020305     c                   select
023150020305     c     lnantw        wheneq    'DPD'
023160020305     c                   movel     desefdw       deseft
023170020305     c     lnantw        wheneq    'FED'
023180020305     c                   movel     deseffw       deseft
023190020305     c                   other
023200020305     c                   movel     deseftw       deseft
023210020305     c                   endsl
023220160225
023230160225     c                   if        lnantw='FED'
023240160225     c                   clear                   trulc7ds
023250160225     c                   eval      ic7tntw='FED'
023260160225     c                   eval      ic7tip='FM'
023270160225     c                   call      'TRULC7R'
023280160225     c                   parm                    trulc7ds
023290160225     c                   eval      fedm_ksc=oc7kscc
023300160225     c                   movel     oc7ctrc       fedm_ctr
023310160225     c
023320160225     c                   clear                   trulc7ds
023330160225     c                   eval      ic7tla='L'
023340160225     c                   eval      ic7tntw='FED'
023350160225     c                   eval      ic7tip='FD'
023360160225     c                   call      'TRULC7R'
023370160225     c                   parm                    trulc7ds
023380160225     c
023390160225     c                   eval      fedd_ksc=oc7kscc
023400160225     c                   movel     oc7ctrc       fedd_ctr
023410160225     c                   endif
023420070906     c*****
023430070906     c* Verifico se destinatario disagiato
023440070906     c*****
023450070906     c                   clear                   wdisagdest
023460070906     c                   clear                   tisit0ds
023470070906     c                   movel     'E'           it0tla
023480070906     c                   movel     arbnzd        it0naz
023490070906     c                   movel     arbprd        it0prv
023500070906     c                   movel     arbcad        it0cap
023510070906     c                   movel     arblod        it0loc
023520070906     c                   movel     arbind        it0ind
023530070906     c                   movel     arbrsd        it0rag
023540070906     c                   call      'TISIT5R'
023550070906     c                   parm                    tisit0ds
023560070906    1c     ot0err        ifeq      *blanks
023570070906    2c                   if        ot0dos = 'A' or ot0dos = 'D' or
023580070906     c                             ot0dos = 'S'
023590070906     c                   movel     ot0dos        wdisagdest
023600070907    3c     wdisagdest    ifeq      'D'
023610070906     c                   movel     'X'           wdisagdest
023620070906    3c                   endif
023630070906    2c                   endif
023640070906   x1c                   else
023650070906     c                   clear                   wdisagdest
023660070906    1c                   endif
023670070306     c
023680070910     c****
023690070910      * Reperisco record GEN nel fiar5
023700070910     c                   Clear                   dAr5Genpar
023710070910     c                   Eval      kAr5Trd = 'GEN'
023720070910     c     kAr5          Chain(n)  Fiar501l
023730070910     c                   If        %Found(Fiar501l)
023740070910     c                   Movel     Ar5Uni        dAr5Genpar
023750070910     c                   eval      wesar5gen='S'
023760070910     c                   endif
023770070319     c****
023780070319     c* FILIALE GESTIONE: VEDO SE GEO ATTIVA
023790070319     c****
023800070319     c                   clear                   FGSgeot
023810070319     c                   clear                   FGSddaSI
023820110906     c****               if        *in10
023830070319     c                   clear                   fnlv55ds
023840070319     C                   MOVEL     '6'           D55TPT
023850070319     C                   MOVEL     ARBLNA        D55LIN
023860070319     C                   MOVEL     DATEU         D55DRF
023870070319     C                   CALL      'FNLV55R'
023880070319     C                   PARM                    Fnlv55DS
023890110906     c                   eval      lnacons=d55tfa
023900070319     c
023910070319     c     d55tfa        CHAIN     AZORG                              30
023920070319     c                   if        not *in30
023930070319     C                   MOVEL     ORGDE8        og148                           L.ARRIVO
023940070319     C                   MOVEL     ORGDE6        og146                           L.ARRIVO
023950070319     C
023960070319     c                   movel     §oggeot       FGSgeot           1
023970070319     C
023980070319    1c                   if        §ogdda>*zeros
023990070319     c                   testn                   §ogdda               30
024000070319     c                   move      §ogdda        w001a             1
024010070319    2c                   if        *in30 and w001a>='0'
024020070319     c* Se la data = 20391231, come se non ci fosse
024030070319    3c                   if        §ogdda<>'20391231'
024040070319     c                   movel     §ogdda        w0080             8 0
024050070426     c* Se <=oggi, attiva procedura distinte automatiche
024060070426    4c                   if        w0080<=dateu
024070070319     c                   movel     'S'           FGSddaSI          1
024080070319    4c                   endif
024090070319    3c                   endif
024100070319    2c                   endif
024110070319    1c                   endif
024120070319     c
024130070319     c                   endif
024140110906     c****               endif
024150151222     c** 10              if        fgsgeot='S'  and dutgeot='S'
024160151222     c                   if        wfile='A' and fgsgeot='S'  and dutgeot='S'
024170140213     c                   seton                                        77
024180140213     c                   endif
024190070329     c* Se p.o. utente o lna hanno GEO attiva, propongo F17 note
024200140130     c* non le propongo se bolla in consegna variazione da inviare a AUT e
024210140130     c* filiale partita con la nuova gestione telefonate AUT
024220151222     c** 10              if        fgsgeot='S'  and dutgeot='S'
024230151222     c                   if        wfile='A' and fgsgeot='S'  and dutgeot='S'
024240140130     c                             and (
024250140130     c* non è in consegna
024260140130     c                             (arbndc=0 or arbdcm>0)
024270140130     c* filiale distinta non partita con nuova gestione telefonate AUT
024280140130     c                             or (%lookup(%subst
024290140130     c                                 (%editc(arbifp:'X'):7:3):skFil816tel)=0
024300140130     c                              and skfil816tel(1)<>'999')
024310140130     c* Causale variazione da non trasmettere a AUT
024320140130     c                             or d66pda=*blanks
024330140130     c                             )
024340070329     c                   seton                                        87
024350070329     c                   endif
024360941019     C**
024370941019     C* CONSEGNE PARTICOLARI
024380941019     C**
024390941019     C                   MOVEL     *BLANKS       V1DTC1
024400941019     C                   MOVEL     *BLANKS       V1DTC2
024410070914     C                   MOVEL     *BLANKS       V7DTC1
024420070914     C                   MOVEL     *BLANKS       V7DTC2
024430941212    1C     ARBTC1        IFNE      ' '
024440930505     C                   MOVEL     '1P'          COD
024450941019     C                   MOVEL(P)  ARBTC1        KEY
024460941212     C                   EXSR      CTABEL
024470941102     C  N30              MOVEL     TBLUNI        DS1P
024480941102     C   30              CLEAR                   DS1P
024490941102     C                   MOVEL     §1PDES        V1DTC1
024500941220     C                   MOVEL     §1PDES        V7DTC1
024510941102     C*
024520941102     C* SE IMMESSA DALLA PARTENZA E NON UTILIZZABILE IN ARRIVO:PROTEGGO
024530941222    2C   10§1PUPR        IFEQ      'S'
024540941213     C     §1PUAR        ANDNE     'S'
024550941102     C                   SETON                                        11
024560941212    2C                   ENDIF
024570941222     C* O VICEVERSA
024580941222    2C  N10§1PUAR        IFEQ      'S'
024590941222     C     §1PUPR        ANDNE     'S'
024600941222     C                   SETON                                        11
024610941222    2C                   ENDIF
024620941102     C*
024630941212    1C                   ENDIF
024640941019     C*
024650941212    1C     ARBTC2        IFNE      ' '
024660941019     C                   MOVEL     '1P'          COD
024670941213     C                   MOVEL(P)  ARBTC2        KEY
024680941212     C                   EXSR      CTABEL
024690941102     C  N30              MOVEL     TBLUNI        DS1P
024700941102     C   30              CLEAR                   DS1P
024710941102     C                   MOVEL     §1PDES        V1DTC2
024720941220     C                   MOVEL     §1PDES        V7DTC2
024730941102     C*
024740941102     C* SE IMMESSA DALLA PARTENZA E NON UTILIZZABILE IN ARRIVO:PROTEGGO
024750941222    2C   10§1PUPR        IFEQ      'S'
024760941213     C     §1PUAR        ANDNE     'S'
024770941102     C                   SETON                                        12
024780941212    2C                   ENDIF
024790941222     C* O VICEVERSA
024800941222    2C  N10§1PUAR        IFEQ      'S'
024810941222     C     §1PUPR        ANDNE     'S'
024820941222     C                   SETON                                        12
024830941222    2C                   ENDIF
024840941222     C*
024850941212    1C                   ENDIF
024860941019     C**
024870941019     C* SPEDIZIONE GIACENTE
024880941019     C**
024890040316     c                   Clear                   wgcfas
024900040324     c                   Clear                   wgcded
024910040324     c                   Clear                   wgcddm
024920051109     c                   Clear                   dgcpflr
024930141113if  1C**   *IN10         IFEQ      *ON
024940141113     c**   d48cvb        oreq      'CS'
024950141113     c**   d48cvb        oreq      'CR'
024960050331     C     KARB          CHAIN     Tigcp21L                           30        GIACENZA
024970941212    1C     *IN30         IFEQ      *OFF
024980050331     C     GCPFAS        ANDLT     40
024990941019     C     GCPATB        ANDEQ     ' '
025000040316     c                   Eval      wgcfas = GcpFas
025010040324     c                   Eval      wgcded = GcpDed
025020040324     c                   Eval      wgcddm = GcpDdm
025030051109     c                   Movel     gcpflr        dgcpflr
025040941019     C                   SETON                                        04
025050941212    1C                   ENDIF
025060040219      * se non ho trovato il rcd di giacenza in arrivo provo in partenza
025070040219      * solo se sto facendo una variazione 'CS' o 'CR'
025080051109      * questo solo per accendere l'indicatore 04 di giacenza aperta, non serve per la
025090051109      * variazione della data consegna richiesta, questa è possibile solo se la giacenza
025100051109      * è aperta in arrivo!!!!
025110141113      * Ora lo 04 mi serve anche per il controllo dell'abilitazione dei dirottamenti sia
025120141113      * in partenza che in arrivo e quinidi lo faccio sempre
025130141113if  2c***                If        *In30 and
025140141113     c***                          (d48cvb = 'CS' or d48cvb = 'CR')
025150141113if  2c                   If        *In30
025160050331     c     kArb          Chain     Tigcp01l
025170050331if  3c                   If        %Found(Tigcp01l) and GcpFas < 50 and
025180040219     c                             GcpAtb = *Blanks
025190040219     c                   Seton                                        04
025200040316     c                   Clear                   wgcfas
025210040324     c                   Clear                   wgcded
025220040324     c                   Clear                   wgcddm
025230040219    3c                   EndIf
025240040219    2c                   EndIf
025250141113    1C*****              ENDIF
025260051129     C* DETERMINO SE IL VOLUME VDL (CHE EVENTUALMENTE SOSTITUIRA' IL
025270970418     C* VOLUME DA FATTURARE) E' PARZIALE O TOTALE
025280970418     C                   CLEAR                   WFVF
025290050111     C     arbncl        IFGT      ARBNCR
025300970418     C                   MOVEL     'Z'           WFVF                           VOLUME PARZ.
025310970418     C                   ELSE
025320970418     C                   MOVEL     'T'           WFVF                           VOLUME TOTAL
025330970418     C                   ENDIF
025340051129      * Determino se il peso VDL è parziale o totale
025350030122     c                   Clear                   VidTpk
025360030123     c                   Clear                   wfpf
025370030122     c                   If        ArbNcp > *Zeros
025380050111     c                   If        arbncl > ArbNcp
025390030122     c                   Eval      VidTpk = 'Par.'                              Peso Parziale
025400030123     c                   Eval      wfpf = 'Z'
025410030122     c                   Else
025420030122     c                   Eval      VidTpk = 'Tot.'                              Peso Totale
025430030123     c                   Eval      wfpf = 'T'
025440030122     c                   EndIf
025450030122     c                   EndIf
025460030729      * Recupero i Bancali
025470030729     c                   If        %Subst(ArbGva:1:1) = 'B'
025480030729     c                   Clear                   dAr5Ban
025490030729     c                   Eval      kAr5Trd = 'BAN'
025500030729     c     kAr5          Chain(N)  Fiar501l
025510030729     c                   If        %Found(Fiar501l)
025520030729     c                   Movel     Ar5Uni        dAr5Ban
025530030729     c     §Ar5Nba       Add       §Ar5Nb2       V1cBan
025540030729     c                   EndIf
025550030729     c                   EndIf
025560030729      * Recupero i Bancali e i colli originali
025570030729     c                   If        %Subst(ArbGva:1:1) = 'O'
025580030729     c                   Clear                   dAr5Bnb
025590030729     c                   Eval      kAr5Trd = 'BNB'
025600030729     c     kAr5          Chain(N)  Fiar501l
025610030729     c                   If        %Found(Fiar501l)
025620030729     c                   Movel     Ar5Uni        dAr5Bnb
025630030729     c     §Ar5bNba      Add       §Ar5bNb2      V1cBan
025640030729     c                   Z-Add     §Ar5bCor      V1cCor
025650030729     c                   Eval      *In70 = *On
025660030729     c                   EndIf
025670030729     c                   EndIf
025680130924      * Memorizzo se presente avviso al destinatario per passarlo alla tassazione
025690110629     c                   clear                   wemd
025700130924     c                   Clear                   dAr5emd
025710110629     c                   Eval      kAr5Trd = 'EMD'
025720130924     c     kAr5          chain(N)  Fiar501l
025730130924     c                   if        %found(fiar501l)
025740130924     c                   Movel     Ar5Uni        dAr5EMD
025750130930     c                   eval      wesar5EMD='S'
025760130924     c                   select
025770130924     c                   when      emd_§ar5mmp='S' and emd_§ar5smp=' '
025780110629     c                   eval      wemd='S'
025790130924     c                   when      emd_§ar5mmp='S' and emd_§ar5smp='S'
025800130924     c                   eval      wemd='E'
025810130924     c                   when      emd_§ar5smp='S'
025820130924     c                   eval      wemd='X'
025830130924     c                   endsl
025840110629     c                   endif
025850061107     C**
025860071022     C* F N A R B D 0 F  record del codice fiscale e partita IVA
025870061107     C**
025880061107    1C     VIDCVR        IFEQ      'FI'
025890071022     c*
025900061129     c* Memorizzo se bolla già trasmessa o meno
025910070528     c*  solo in partenza(in arrivo è per forza trasmessa
025920151222     c  n10              movel     arbft2        bollaft2          1
025930151222     c**                 if        wfile='P'
025940151222     c**                 movel     arbft2        bollaft2          1
025950151222     c***                endif
025960071022     c* Dati per partita IVA
025970071022     c
025980071025     c                   clear                   ar4not_P
025990071022     C                   MOVEL     'P'           WTRC
026000061107     C     KARB2         CHAIN(N)  FiAR4000                           30
026010071025     c  n30              movel     ar4not        ar4not_p
026020071022     C                   MOVEL     'Q'           WTRC
026030071022     C     KARB2         CHAIN(N)  FiAR4000                           30
026040061107     c
026050061107     c* Verifico se bolla singola .Se doppia guardo se sono in partenza
026060061107     c*  o arrivo
026070061107     c* carico il mittente se bolla franco in partenza sempre
026080061107     c*  o franco singola in arrivo
026090061107    2c                   if        (flgtb1='F'  and not *in10) or
026100061107     c                             (flgtb1='F'  and §3atb2=*blanks)
026110061107     c                   eval      v11mide='M'
026120061107     c* Mittente
026130061107     c                   eval      v11decmd=CMITT
026140061107     c                   eval      v11cod=arbksc
026150061107     c                   eval      v11rsc=arbrsm
026160061107     c                   eval      wnar=arbnzm
026170071025     c* codice fiscale
026180061107     c                   if        not *in30 and not *in20
026190061107     c                   movel     ar4not        v11cdfis
026200061107     c                   else
026210061107     c                   clear                   v11cdfis
026220061107     c                   endif
026230071025     c* Partita IVA
026240071107     c****               if        not *in20
026250071025     c                   if        %subst(ar4not_P:1:16)<>*blanks
026260071025     c                   movel     ar4not_P      v11pid
026270071025     c                   else
026280071025     c                   movel     arbcpi        v11pid
026290071025     c                   endif
026300071107     c****               endif
026310071025     c
026320061107     c* In arrivo di fatto non posso inserire cod.fiscale su un franco
026330061107     c*    singolo: proteggo il campo
026340061107    3c                   if        *in10 and §3atb2=*blanks
026350061107     c                   seton                                        76
026360061107    3c                   endif
026370061107    2c                   endif
026380061107     c
026390061107     c* Carico il destinatario in arrivo sempre, in partenza se singola
026400061107    2c                   if        (flgtb1='A') or
026410061107     c                             (flgtb1='F'  and §3atb2<>*blanks and
026420061107     c                             *in10)
026430061107     c                   eval      v11mide='D'
026440061107     c                   eval      v11decmd=Cdest
026450061107     c                   eval      v11rsc=arbrsd
026460061107     c                   eval      wnar=arbnzd
026470061107     c                   if        not *in30 and not *in20
026480061107     c                   move      ar4not        v11cdfis
026490061107     c                   else
026500061107     c                   clear                   v11cdfis
026510061107     c                   endif
026520071025     c* Partita IVA
026530071107     c*****              if        not *in20
026540071025     c                   if        %subst(ar4not_P:20:16)<>*blanks
026550071026     c                   move      ar4not_P      v11pid
026560071025     c                   else
026570071107     c                   movel     arbcpi        v11pid
026580071025     c                   endif
026590071107     c*****              endif
026600071025     c* In partenza proteggo sempre
026610061107     c  n10              seton                                        76
026620061107     c* codice destinatario: dal fiar6 prima o seconda bolla
026630061107    3c                   if        §3atb2<>*blanks
026640061107     c                   eval      v11cod=ksc2bol
026650061107   x3c                   else
026660061107     C                   MOVEL     '1'           WTRC
026670061107     C     KARB2         CHAIN(N)  FIAR6000                           30
026680061107     c   30              eval      v11cod=arbksc
026690061107     c  n30              eval      v11cod=ar6ksc
026700061107    3c                   endif
026710061107    2c                   endif
026720061107     C*
026730061107     C* PGM RICHIAMATO
026740061107     c                   if        *in20
026750071025     c* in CPI c'e' il codice fiscale
026760061107     c                   movel     §bdcpi        v11cdfis
026770071107     c* in RSD c'e' la partita IVA solo dalla data attivazione
026780071203     c***                if        dateu>=§vpopiv
026790071025     c                   movel     §bdrsd        v11PID
026800071203     c***                endif
026810071107     c                   endif
026820071025     c
026830071025     c* controllo Partita IVA se privato
026840071025    2c                   if        v11pid<>*blanks
026850071025     C     'PRIVATO'     SCAN      v11pid        posiz                    30
026860071025     c* solo numeri
026870071025     c* "PRIVATO" senza iso
026880071025    3c     *in30         ifeq      *off
026890071025     c     v11isd        andge     *zeros
026900071025     c     *in30         oreq      *on
026910071025     c     posiz         andlt     3
026920071025     c                   movel     v11pid        w016a            16
026930071025     c                   movel     w016a         v11cpd
026940071025     C                   CLEAR                   V11isd
026950071025    3c                   endif
026960071025    2c                   endif
026970061107     c*
026980061107    1c                   endif
026990941019     C**
027000941027     C* F N A R B D 0 F
027010941019     C**
027020941019    1C     FLGCVR        IFEQ      'I'
027030941019     C*
027040941019    2C     VIDCVR        IFEQ      'I0'
027050140407    2C     VIDCVR        oreQ      'I7'
027060140407    2C     VIDCVR        oreQ      'I8'
027070140407    2C     VIDCVR        oreQ      'I9'
027080941019     C                   SETON                                        01          DESTINATAR
027090941019     C                   ELSE
027100941019     C                   SETON                                        02          PESO VOL
027110941212    2C                   END
027120941025     C*
027130941025     C* 2 RAGIONE SOCIALE DESTINATARIO
027140941212     C                   MOVEL     'D'           WTRC
027150060222     C     KARB2         CHAIN(N)  Fiar4000                           30
027160941212     C     *IN30         IFEQ      *OFF
027170941212     C                   MOVEL     'E'           WAR4D             1
027180941212     C* LO SALVO PER LA STORICIZZAZIONE
027190970520     C                   MOVE      AR4NOT        SAVRD2
027200941212     C                   ENDIF
027210061116     C*
027220061116     C* codice fiscale destinatario
027230061116     C                   MOVEL     'Q'           WTRC
027240061116     C     KARB2         CHAIN(N)  Fiar4000                           30
027250061116     C     *IN30         IFEQ      *OFF
027260061116     C                   MOVEL     'E'           WAR4Q
027270061116     C* Se c'e' il cod fiscale del detinatario lo memorizzo
027280061116     C                   MOVE      AR4NOT        vidcdf
027290061116     c                   if        vidcdf<>*blanks
027300061116     C                   MOVEL     'D'           WAR4Q
027310061116     C                   ENDIF
027320061116     C                   ENDIF
027330150206
027340150206     C* LOCALITA' NORMALIZZATA
027350150206     C                   MOVEL     'X'           WTRC
027360150206     C     KARB2         CHAIN(N)  Fiar4000                           30
027370150206     C     *IN30         IFEQ      *OFF
027380150206     C                   MOVEL     'E'           WAR4X             1
027390150206     C                   ENDIF
027400061116     c
027410051129     C* VISUALIZZO ANCHE IL VOLUME VDL
027420941214     C                   MOVEL     ARBVLC        V1CVLC
027430941214     C                   MOVEL     ARBNCR        V1CNCR
027440031120
027450040123      * Reperisco il referente e il telefono e magazzino giacenza
027460070910     c                   If        wesar5gen='S'
027470070910     c                   Eval      VidRef =  PAr5Ref
027480070910     c                   Eval      VidTeld = PAr5Teld
027490070910     c                   Eval      VidZng =  PAr5Zng
027500031120     c                   EndIf
027510141203     c                   eval      savzng = vidzng
027520941019     C*
027530941019     C* PGM NON RICHIAMATO
027540941019    2C     *IN20         IFEQ      *OFF
027550970520     C                   CLEAR                   VIDRSD
027560941025     C                   MOVEL     ARBRSD        VIDRSD
027570941027     C     WAR4D         IFEQ      'E'
027580061116     C                   MOVE      savrd2        VIDRSD
027590941025     C                   ENDIF
027600941019     C                   MOVEL     ARBIND        VIDIND
027610941019     C                   MOVEL     ARBLOD        VIDLOD
027620941019     C                   MOVEL     ARBPRD        VIDPRD
027630941019     C                   MOVEL     ARBCAD        VIDCAD
027640061115     c
027650140428     c*05 off se --> bolla singola in assegnato
027660140428     c*              bolla doppia
027670071025    3C     *IN05         IFEQ      *OFF
027680140428     c* se bolla doppia in partenza la partita iva che ho caricato non è quella del
027690140428     c* destinatario e quindi la carico solo se sono in arrivo (10 on)
027700151222     c***  *in10         ifeq      *on
027710151222     c     wfile         ifeq      'A'
027720140428     c     §3atb2        oreq      *blanks
027730941220     C                   MOVEL     ARBCPI        VIDPID
027740140428     c                   endif
027750071025     C**   'PRIVATO'     SCAN      vidpid        posiz                    30
027760050922     c* solo numeri
027770050922     c* "PRIVATO" senza iso
027780071025    4c**   *in30         ifeq      *off
027790071025     c**   vidisd        andge     *zeros
027800071025     c**   *in30         oreq      *on
027810071025     c**   posiz         andlt     3
027820071025     C**                 CLEAR                   VIDPID
027830071025     c**                 movel     arbcpi        vidcpd
027840071025    4c**                 endif
027850071025    3C                   ENDIF
027860941220     C*
027870030122     c                   Z-Add     ArbPkc        VidPkc
027880941027     C                   MOVEL     ARBPKF        VIDPKF
027890941027     C                   MOVEL     ARBVLF        VIDVLF
027900941027     C                   MOVEL     ARBFVF        VIDFVF
027910941019     C                   MOVEL     ARBFIN        VIDFIN
027920941019     C                   MOVEL     ARBNZD        VIDNZD
027930941019     C* FERMO DEPOSITO
027940941019    3C     ARBFFD        IFEQ      'S'
027950941019     C                   MOVEL     'SI'          VIDFFD
027960941019     C                   ELSE
027970941019     C                   CLEAR                   VIDFFD
027980941019    3C                   ENDIF
027990941019     C**
028000941019   X2C                   ELSE
028010941019     C**
028020941019     C* PROGRAMMA     RICHIAMATO: PRENDO CAMPI DA DSARBD
028030941019     C                   MOVEL     §BDRSD        VIDRSD
028040970520     C                   MOVE      §BDRD2        VIDRSD
028050941019     C                   MOVEL     §BDIND        VIDIND
028060941019     C                   MOVEL     §BDLOD        VIDLOD
028070941019     C                   MOVEL     §BDPRD        VIDPRD
028080941019     C                   MOVEL     §BDCAD        VIDCAD
028090941019     C                   MOVEL     §BDNZD        VIDNZD
028100060308     c                   movel     §bdref        vidref
028110060308     c                   movel     §bdtel        vidteld
028120061115     c                   clear                   vidcdf
028130071025    3C     *IN05         IFEQ      *OFF
028140950102     C                   MOVEL     §BDCPI        VIDPID
028150071025     C**   'PRIVATO'     SCAN      vidpid        posiz                    30
028160050922     c* solo numeri
028170050922     c* "PRIVATO" senza iso
028180071025    4c**   *in30         ifeq      *off
028190071025     c**   vidisd        andge     *zeros
028200071025     c**   *in30         oreq      *on
028210071025     c**   posiz         andlt     3
028220071025     C**                 CLEAR                   VIDPID
028230071025     c**                 movel     §bdcpi        vidcpd
028240071025    4c**                 endif
028250071025    3C                   ENDIF
028260941019     C                   MOVEL     §BDFIN        VIDFIN
028270941027     C                   MOVEL     §BDPKF        VIDPKF
028280941027     C                   MOVEL     §BDVLF        VIDVLF
028290941209     C                   MOVEL     §BDFVF        VIDFVF
028300941019     C* FERMO DEPOSITO
028310141017     c* §BDFFD= 'S' => si fermo deposito; Si alert
028320141017     c* §BDFFD= 'Y' -->si fermo deposito; no alert
028330141017     c* Serve per le chiamate cieche in quanto il campo a video per la gestione
028340141017     c* del contatto (VIDTFD) non è presente nella dsarbd.
028350941019    3C     §BDFFD        IFEQ      'S'
028360141017    3C     §BDFFD        oreq      'Y'
028370941019     C                   MOVEL     'SI'          VIDFFD
028380941019     C                   ELSE
028390941019     C                   CLEAR                   VIDFFD
028400941019    3C                   ENDIF
028410941216    2C                   ENDIF
028420141017      *
028430141017     c                   if        §bdffd='Y'
028440141017     c                   eval      vidtfd='SI'
028450141017     c                   endif
028460071025     c
028470071025     c* controllo Partita IVA se privato
028480071025    2c                   if        vidpid<>*blanks
028490071025     C     'PRIVATO'     SCAN      vidpid        posiz                    30
028500071025     c* solo numeri
028510071025     c* "PRIVATO" senza iso
028520071025    3c     *in30         ifeq      *off
028530071025     c     vidisd        andge     *zeros
028540071025     c     *in30         oreq      *on
028550071025     c     posiz         andlt     3
028560071025     c                   movel     vidpid        w016a
028570071025     c                   movel     w016a         vidcpd
028580071025     C                   CLEAR                   VIDisd
028590071025    3c                   endif
028600071025    2c                   endif
028610991001     C*
028620991001    2C     *IN01         IFEQ      *ON
028630970808     C* MEMORIZZO CAP/PROVINCIA E LOCALITA' DEL DESTINATARIO
028640970808     C* (MI SERVIRA' NELLA ROUTINE DEI CONTROLLI
028650970808     C*
028660970808     C                   MOVE      VIDCAD        SAVCAD                         CAP
028670970808     C                   MOVE      VIDCAD        VARCAD                          "
028680970808     C                   MOVE      VIDCAD        WCAD                            "
028690970808     C                   MOVE      VIDCAD        WCAD1                           "
028700970808     C                   MOVE      VIDLOD        SAVLOD                         LOCALITA'
028710970808     C                   MOVE      VIDLOD        VARLOD                            "
028720970808     C                   MOVE      VIDLOD        WLOD                              "
028730970808     C                   MOVE      VIDLOD        WLOD1                             "
028740060111     c* memorizzo anche gli altri dati del destinatario
028750060111     c                   move      vidrsd        savrsd
028760061116     c                   move      vidrsd        savrsdQ
028770060111     c                   move      vidrsd        wrsd1
028780060111     c                   move      vidind        savind
028790060111     c                   move      vidind        wind1
028800060111     c                   move      vidprd        savprd
028810060111     c                   move      vidprd        wprd1
028820060111     c                   move      vidnzd        savnzd
028830060111     c                   move      vidnzd        wnzd1
028840970808     C*
028850950419     C* SE P.IVA DEL VIDEO = BLANKS LA PRENDO DALL'ESTENSIONE SE
028860950419     C* ESISTE
028870950419     C     VIDPID        IFEQ      *BLANKS
028880950419     C                   MOVEL     'P'           WTRC
028890060222     C     KARB2         CHAIN(N)  Fiar401L                           30
028900950419     C     *IN30         IFEQ      *OFF
028910950419     C                   MOVE      AR4NOT        VIDPID
028920050922     C     'PRIVATO'     SCAN      vidpid        posiz                    30
028930050922     c* solo numeri
028940050922     c* "PRIVATO" senza iso
028950050922     c     *in30         ifeq      *off
028960050922     c     vidisd        andge     *zeros
028970050922     c     *in30         oreq      *on
028980050922     c     posiz         andlt     3
028990050922     C                   CLEAR                   VIDPID
029000050922     c                   move      ar4not        wpiva
029010050922     C                   MOVEL     WPIVA         VIDCPD
029020050922     c                   endif
029030050922     C**   VIDISD        IFGE      *ZEROS
029040050922     C**                 CLEAR                   VIDPID
029050050922     C**                 MOVE      AR4NOT        WPIVA
029060050922     C**                 MOVEL     WPIVA         VIDCPD
029070050922     C**                 ENDIF
029080950419     C                   END
029090950419     C                   END
029100950419     C*
029110160118     C                   SETOFF                                       717276
029120991001     C* SE ASSEGNATO 9999 GIA' CONTABILIZZATO PROTEGGO DATI
029130991001     C* DESTINATARIO (ANCHE LA P.IVA MA SOLO SE DIVERSA DA BLANKS)
029140991001     C                   MOVE      ARBKSC        CO1KSC            4 0
029150991001    3C     CO1KSC        IFEQ      9999
029160991001     C                   CLEAR                   AR6DFT
029170991001     C                   MOVEL     '1'           WTRC
029180991001     C     KARB2         CHAIN(N)  FIAR6000                           30
029190991001    4C     AR6DFT        IFNE      *ZEROS
029200000104     C     AR6DFT        ANDLE     WDTU10
029210991001     C                   SETON                                        71
029220000104    5C     VIDPID        IFNE      *BLANKS
029230000104     C                   SETON                                        72
029240991001    5C                   END
029250991001    4C                   END
029260991001    3C                   END
029270160118     c* Proteggo la partita iva/cod.fiscale anche se dagli arrivi sto manutenzionando BLP
029280160115     c                   if        *in10 and wfile='P'
029290160118     C                   SETON                                        7276
029300160115     c                   endif
029310991001    2C                   END
029320941019     C*
029330941222     C* PROTEGGO VOLUME SE NON VARIABILE
029340941216     C* SE SI TRATTA DI VARIAZIONE DI PESO, PROTEGGO SEMPRE IL VOLUME
029350941216    2C     D48CVB        IFEQ      'I2'
029360941216     C* PROTEGGO IL PESO
029370941216     C                   SETON                                        88
029380941216     C*
029390941019     C                   MOVEL     '7T'          COD
029400051214     C                   MOVEL(P)  VIDFVF        KEY
029410941212     C                   EXSR      CTABEL
029420941212     C   30              SETON                                        89
029430941222     C     *IN30         IFEQ      *OFF
029440941222     C                   MOVEL     TBLUNI        DS7T
029450091106     c
029460151222     c                   if        *in10
029470091106     c                   eval      wvariavol=§7tvra
029480120705     c                   eval      wvariavolEXP=§7tvraEXP
029490091106     c                   else
029500091106     c                   eval      wvariavol=§7tvrp
029510120705     c                   eval      wvariavolEXP=§7tvrPEXP
029520091106     c                   endif
029530091106     c
029540091106     c* Proteggo il volume in arrivo o in partenza se il flag è ' ' oppure
029550091124     c*  non corrisponde il tipo bolla
029560091106     c                   select
029570091106     c                   when      wvariavol=' '
029580091106     C                   SETON                                        89
029590091106     c                   when      Wvariavol='A' and flgtb1='F'
029600091106     C                   SETON                                        89
029610091106     c                   when      Wvariavol='F' and flgtb1='A'
029620091106     C                   SETON                                        89
029630091106     c                   endsl
029640091106     c
029650120705     C* Se  BOLLA EXPORT,  in aggiunta controllo il relativo flag
029660120705     c                   if        *in89 and (lnantw='FED' or
029670120705     c                                        lnantw='EEX' or lnantw='DPD')
029680120705     c                   select
029690120705     c                   when      WvariavolEXP='3'
029700120705     C                   SETOff                                       89
029710120705     c                   when      WvariavolEXP='A' and flgtb1='A'
029720120705     C                   SETOff                                       89
029730120705     c                   when      Wvariavolexp='F' and flgtb1='F'
029740120705     C                   SETOff                                       89
029750120705     c                   endsl
029760120705     c                   endif
029770091106     c
029780941222    3C                   ENDIF
029790941216     C*
029800941216   X2C                   ELSE
029810941216     C                   SETON                                        89
029820941216    2C                   ENDIF
029830030604
029840030604      * se linea arrivo FedEx e il volume è "T" proteggo sempre
029850030604      * (già da tabella "7T" il volume di tipo "T" non è sostituibile,
029860030604      *  faccio lo stesso la modifica nel caso di variazione della tabella)
029870030604     c                   If        *In02 and LnaNtw = 'FED' and vidfvf = 'T'
029880030604     c                   Eval      *In89 = *On
029890030604     c                   EndIf
029900941019    1C                   ENDIF
029910130930     c
029920130930     C**
029930130930     C* F N A R B N 0 F
029940130930     C**
029950130930    1C     FLGCVR        IFEQ      'N'
029960130930
029970130930      * Reperisco il referente e il telefono e magazzino giacenza
029980130930    2c                   if        not  *in20
029990130930
030000130930    3c                   if        d48cvb='NE'
030010131001     c                   seton                                        868483
030020130930     c                   If        wesar5gen='S'
030030130930     c                   Eval      VidRef =  PAr5Ref
030040130930     c                   Eval      VidTeld = PAr5Teld
030050130930     c                   EndIf
030060130930     c                   if        wesar5emd='S'
030070130930     c                   eval      videml = emd_§ar5eml
030080130930     c                   eval      vidtsms= emd_§ar5tel
030090130930     c                   endif
030100130930    3c                   endif
030110130930
030120130930    3c                   if        d48cvb='NT'
030130131001     c                   seton                                        81
030140130930     c                   eval      vidnt1=v1cnt1
030150130930     c                   eval      vidnt2=v1cnt2
030160130930    3c                   endif
030170130930     c
030180130930   x2c                   else
030190130930    3c                   select
030200130930     c
030210131002     c                   when      §bntrd='NOT'
030220131002     c                   if        §bnu_sn='S'
030230131001     c                   seton                                        81
030240130930     c                   movel     §bnuni70      vidnt1
030250130930     c                   move      §bnuni70      vidnt2
030260131002     c                   else
030270131002     C                   MOVEL     MSG(24)       D48MSG
030280131002     C                   MOVEL     '1'           D48ERR
030290131002     c                   leavesr
030300131002     c                   endif
030310131008     c* se la causale variazione non è NT --> errore
030320131008     c                   if        d48cvb<>'NT'
030330131008     C                   MOVEL     MSG(31)       D48MSG
030340131008     C                   MOVEL     '1'           D48ERR
030350131008     c                   leavesr
030360131008     c                   endif
030370131008
030380130930     c                   other
030390131008
030400131008     c* se la causale variazione non è NE --> errore
030410131008     c                   if        d48cvb<>'NE'
030420131008     C                   MOVEL     MSG(31)       D48MSG
030430131008     C                   MOVEL     '1'           D48ERR
030440131008     c                   leavesr
030450131008     c                   endif
030460130930     c
030470131001     c                   if        §bnu_sn='S'
030480130930     c                   eval      videml = §bnuni70
030490131001     c                   seton                                        86
030500131001     c                   endif
030510131001     c                   if        §bnr_sn='S'
030520131001     c                   eval      vidteld= §bntel
030530130930     c                   eval      vidref = §bnref
030540131001     c                   seton                                        84
030550131001     c                   endif
030560131001     c                   if        §bnc_sn='S'
030570131001     c                   eval      vidtsms= §bncel
030580131001     c                   seton                                        83
030590131001     c                   endif
030600131002     c                   if        *in86=*off and *in84=*off and *in83=*off
030610131002     C                   MOVEL     MSG(24)       D48MSG
030620131002     C                   MOVEL     '1'           D48ERR
030630131002     c                   leavesr
030640131002     c                   endif
030650131001     c
030660130930    3c                   endsl
030670130930     c
030680130930    2c                   endif
030690130930    1c                   endif
030700950104     C**
030710990930     C* F I A R B T 0 F
030720950104     C**
030730100120     c
030740941019    1C     FLGCVR        IFEQ      'T'
030750941019     C                   SELECT
030760941019     C*
030770941102     C     D66RTA        WHENEQ    'S'
030780950616     C                   SETON                                        1529        RITASSAZ
030790941102     C*
030800941102     C     D66BSF        WHENEQ    'S'
030810941019     C                   SETON                                        13          VARIA  SF
030820941019     C*
030830941102     C     D66BFI        WHENEQ    'S'
030840991006     C                   SETON                                        1407        VARIA  FAT
030850941019     C*
030860941019     C                   OTHER
030870941019     C                   SETON                                        36          TASS FRANC
030880941019     C                   ENDSL
030890941102     C* PULIZIA SFL
030900941102     C                   SETON                                        85
030910941215     C                   WRITE     LR48C03
030920941102     C                   SETOFF                                       85
030930941215     C*
030940941215     C                   MOVEL     *BLANKS       DESKSC
030950950104     C   10              MOVEL     ARBRSD        W008A             8
030960950104     C   10              MOVEL     W008A         DESKSC
030970950104     C  N10              MOVEL     ARBRSM        DESKSC
030980131120     c   10              clear                   sav_ift
030990941102     C*
031000941102     C*
031010990930     C* TASSAZIONE IN FIAR6
031020990930     C                   MOVEL     '1'           WTRC
031030990930     C     KARB2         CHAIN     FIAR6000                           30
031040990702    2C     *IN30         IFEQ      *OFF
031050990701     C                   MOVEL     'E'           WAR6
031060990701     C                   MOVEL     AR6NFT        VIDNFT
031070990701     C                   MOVEL     AR6FIV        VIDFIV
031080990702     C                   Z-ADD     AR6IMV        SAVIMV
031090151016     C                   Z-ADD     AR6IMV        SAVIMV_fat
031100131120     c   10              z-add     ar6ift        sav_ift
031110990702     C* PROTEZIONE DATI FATTURA PER PREPAGATI
031120990702    3C  N10VIDNFT        IFLT      §QTNFT
031130990719     C     VIDNFT        ANDGT     0
031140990701     C                   SETON                                        23
031150990702     C                   Z-ADD     AR6IFT        VIDIFT
031160990702    3C                   ENDIF
031170991001     C* PROTEZIONE DIVISA PER PREPAGATI
031180991001    3C  N10VIDNFT        IFGT      0
031190991012     C                   SETON                                        0708
031200991001    3C                   ENDIF
031210991108     C* SE PREPAGATO O P.A. FATTURA GIA'CONTAB. MODIFICO SOLO IMP.ASSIC
031220991008     C* E VALORE MERCE DICHIARATO
031230991108    3C     AR6DFT        IFGT      *ZEROS
031240000104     C     AR6DFT        ANDLE     WDTU10
031250991008     C                   SETON                                        2973
031260991008    3C                   ENDIF
031270991029    2C                   ENDIF
031280991029     C**
031290991029     C* SE ASSEGNATO GIA' INCASSATO, FACCIO MODIFICARE SOLO IMP.ASSIC EARIAZIONE
031300991029     C*  VALORE MERCE DICHIARATO
031310130409    1C*  10FLGCVR        IFEQ      'T'
031320130409    2C*    §7LFIN        IFEQ      'S'
031330130409     C*    ARBICA        ANDNE     ' '
031340130409     C*    ARBICA        ANDNE     'R'
031350991029     C* PROTEGGO I CAMPI CHE NON DEVONO ESSERE MANUTENZIONATI
031360130409     C*                  SETON                                        2973
031370130409    2C*                  END
031380991108     C* SE ICA = 'R' E C'E' INCASSO PARZ<IALE IN ARI, PROTEGGO
031390991108     C*  LO STESS, FACCIO SOLO MODIFICARE IAS
031400130409    2C*    ARBICA        IFEQ      'R'
031410130409     C*    KARI          CHAIN     FIARI01L                           30
031420130409    3C* N30ARIATB        IFEQ      ' '
031430991108     C* PROTEGGO I CAMPI CHE NON DEVONO ESSERE MANUTENZIONATI
031440130409     C*                  SETON                                        2973
031450130409    3C*                  ENDIF
031460130409    2C*                  ENDIF
031470130409    1C*                  ENDIF
031480130410     c* PROTEZIONE CAMPI PER POTER VARIARE SOLO IAS E VMD
031490130410     c* Se bolla già contabilizzata e variabile anche dopo contabilizzazione
031500130410     c* oppure se bolla già incassata e variabile anche dopo incasso (V.di fnlr66r)
031510130410     c                   if        d66bdi='S'
031520130409     C                   SETON                                        2973
031530130409     c                   endif
031540990701     C**
031550950807     C* VEDO SE ESISTE AMCHE L'AR7
031560990930     C     KARB2         SETLL     FIAR7000                               30
031570950807     C   30              MOVEL     'E'           WAR7
031580060201     c
031590060201      * Calcolo data limite mod. importo da assicurare
031600060201     c     *iso          Move      ArbDsp        wdataiso
031610060201     c                   adddur    §vpomia:*d    wdataiso
031620060201      *
031630060201     c                   do        *hival
031640060201      * verifico se è un giorno lavorativo
031650060201     c     *iso          move      wdataiso      ds_data
031660060201     C                   eval      kann = ds_anno
031670060201     C                   eval      kmes = ds_mese
031680060201     C     kazcln        chain     azcln01l
031690060201     C                   if        %found(azcln01l)
031700060201     C                   if        MAT(ds_giorno) =  'F'  or
031710060201     C                             POM(ds_giorno) =  'F'
031720060201      * se non va bene aggiungo 1 giorno
031730060201     c                   adddur    1:*d          wdataiso
031740060201     C                   iter
031750060201     c                   else
031760060201     c                   leave
031770060201     C                   endif
031780060201     c                   else
031790060201     c                   leave
031800060201     C                   endif
031810060201
031820060201     c                   enddo
031830060201
031840060201     c                   eval      wdatalimias=wdataiso
031850080625     c                   clear                   forzaias2
031860080625     c                   clear                   forzaias
031870060201     c
031880060201     c* IAS modificabile solo nei primi 2 gg  tassaz sempre
031890060201     c                   if        §utemtf=' '  and
031900060201     c                             wdataoggi>wdatalimIas
031910060201     c                   seton                                        75
031920060201     c                   endif
031930060201     c* solo IAs ma sempre
031940060201     c                   if        §utemtf='I'
031950060201     c                   seton                                        74
031960060201     c                   endif
031970060201     c* Nulla
031980060201     c                   if        §utemtf='N'
031990060201     c                   seton                                        7475
032000060201     c                   endif
032010991012     C*
032020060201     C*
032030060201     C                   CLEAR                   V3CEV1
032040060201     C                   CLEAR                   V3CSV1
032050060201     C                   CLEAR                   V3CVA1
032060060201     C                   CLEAR                   VARVA1
032070060201     C                   Z-ADD     1             NRR
032080060201     C                   DO        20            NRR
032090060201     C                   WRITE     LR48S03
032100060201     C                   ENDDO
032110920128     C* PROGRAMMA NON RICHIAMATO: DATI DA BOLLA
032120941019    2C     *IN20         IFEQ      *OFF
032130941019     C*
032140941019     C                   Z-ADD     ARBIAS        VIDIAS
032150950426     C                   Z-ADD     ARBIAS        COMIAS
032160941209     C                   MOVEL     ARBVAS        VIDVAS
032170941215     C                   MOVEL     ARBVAS        VARVAS
032180941019     C                   MOVEL     ARBKSC        VIDKLP
032190950424     C                   MOVEL     ARBKSC        VARKLP
032200941019     C                   MOVE      ARBKSC        VIDKSC
032210970822     C                   MOVE      ARBKSC        MEMKSC
032220950424     C                   MOVE      ARBKSC        VARKSC
032230941019     C                   MOVEL     ARBCTR        VIDCTR
032240050202     C                   MOVEL     ARBCTR        SAVCTR
032250060511     C                   MOVEL     ARBCTR        CTRBOLLA          3
032260941215     C                   MOVEL     ARBQFT        VIDQFT
032270950424     C                   MOVEL     ARBQFT        VARQFT
032280941215     C                   MOVEL     ARBVMD        VIDVMD
032290941215     C                   MOVEL     ARBVAD        VIDVAD
032300941215     C                   MOVEL     ARBVAD        VARVAD
032310990930     C* TASSAZIONE IN FIAR6
032320941102    3C     WAR6          IFEQ      'E'
032330990930     C                   MOVEL     AR6DIV        VIDDIV
032340991006     C                   MOVEL     AR6DIV        WDIV
032350941019     C                   MOVEL     AR6POR        VIDPOR
032360991006     C                   MOVEL     AR6POR        VARPOR
032370941019     C                   MOVEL     AR6IMV        VIDIMV
032380941019     C                   MOVEL     AR6CEI        VIDCEI
032390941102     C*
032400941102     C                   Z-ADD     0             NRR
032410941209     C                   MOVEL     AR6SV1        WSV1
032420941209     C                   Z-ADD     AR6VA1        WVA1
032430941102     C                   EXSR      CARVAR
032440941209     C                   MOVEL     AR6SV2        WSV1
032450941209     C                   Z-ADD     AR6VA2        WVA1
032460941102     C                   EXSR      CARVAR
032470941212    4C     AR6SV3        IFNE      ' '
032480941209     C                   MOVEL     AR6SV3        WSV1
032490941209     C                   Z-ADD     AR6VA3        WVA1
032500941102     C                   EXSR      CARVAR
032510941102     C*
032520941102     C* VISTO CHE C'E' LA 3 VARIA VEDO SE CE NE SONO ANCORA
032530990930     C     KARB2         READE(N)  FIAR7000                               30
032540941102    5C     *IN30         DOWEQ     *OFF
032550941209     C                   MOVEL     AR7SVN        WSV1
032560941209     C                   MOVEL     AR7VAN        WVA1
032570941102     C                   EXSR      CARVAR
032580990930     C     KARB2         READE(N)  FIAR7000                               30
032590941102    5C                   ENDDO
032600941102    4C                   ENDIF
032610941102    3C                   ENDIF
032620941019     C*
032630941019   X2C                   ELSE
032640920128     C*
032650990930     C* PROGRAMMA     RICHIAMATO: DATI DA DARBT
032660941025     C                   Z-ADD     §BTIAS        VIDIAS
032670950426     C                   Z-ADD     §BTIAS        COMIAS
032680941019     C                   MOVEL     §BTVAS        VIDVAS
032690941220     C                   MOVEL     §BTVAS        VARVAS
032700941209     C                   MOVEL     §BTQFT        VIDQFT
032710950424     C                   MOVEL     §BTQFT        VARQFT
032720941209     C                   MOVEL     §BTVMD        VIDVMD
032730941209     C                   MOVEL     §BTVAD        VIDVAD
032740941220     C                   MOVEL     §BTVAD        VARVAD
032750920128     C                   MOVEL     §BTKSC        VIDKLP
032760950424     C                   MOVEL     §BTKSC        VARKLP
032770920128     C                   MOVE      §BTKSC        VIDKSC
032780970822     C                   MOVE      §BTKSC        MEMKSC
032790950424     C                   MOVE      §BTKSC        VARKSC
032800920128     C                   MOVEL     §BTKSC        SAVKSC            7 0
032810060511     c                   clear                   kscbolla
032820920128     C                   MOVEL     §BTCTR        VIDCTR
032830050202     C                   MOVEL     §BTCTR        SAVCTR
032840060511     c                   clear                   ctrbolla
032850990930     C                   MOVEL     §BTDIV        VIDDIV
032860991006     C                   MOVEL     §BTDIV        WDIV
032870990930     C                   Z-ADD     §BTPOR        VIDPOR
032880991006     C                   Z-ADD     §BTPOR        VARPOR
032890990930     C                   Z-ADD     §BTIMV        VIDIMV
032900920128     C                   MOVEL     §BTCEI        VIDCEI
032910941102     C* CARICO LE VARIE
032920941102     C                   Z-ADD     0             NRR               5 0
032930941209     C                   DO        20            C                 3 0
032940941209     C                   MOVEL     §SV(C)        WSV1
032950941209     C                   Z-ADD     §VA(C)        WVA1
032960941102     C                   EXSR      CARVAR
032970941209     C                   ENDDO
032980941019    2C                   ENDIF
032990131016
033000131016     c                   z-add     vidias        bol_vidias
033010131016     c                   movel     vidvas        bol_vidvas
033020131016     c                   z-add     vidvmd        bol_vidvmd
033030131016     c                   movel     vidvad        bol_vidvad
033040131016     c
033050941215     C*
033060941215     C* CARICO LE TARIFFE E DEOCIFICO IL CTR CHE C'E'I
033070050202     C                   Z-ADD     SAVKSC        KSC
033080941215     C                   EXSR      CARCTR
033090941019     C*
033100920128     C* DECODIFICA ESENZIONE IVA
033110920128     C                   MOVEL     *BLANKS       DESCEI
033120941019    2C     VIDCEI        IFNE      ' '
033130920128     C                   MOVEL     'EI'          COD
033140920128     C                   MOVEL     *BLANKS       KEY
033150051214     C                   MOVEL(P)  VIDCEI        KEY
033160941212     C                   EXSR      CTABEL
033170920128     C  N30              MOVEL     TBLUNI        DESCEI
033180941019    2C                   END
033190920128     C* GIRO DATA FATTURA
033200941102    1C     WAR6          IFEQ      'E'
033210941102     C     AR6DFT        ANDGT     0
033220941019     C                   Z-ADD     AR6DFT        G02INV
033230941019     C                   MOVEL     '3'           G02ERR
033240941019     C                   CALL      'XSRDA8'
033250941019     C                   PARM                    WLBDAT
033260941019     C*
033270941019     C                   Z-ADD     G02DAT        VIDDFT
033280941019    2C                   ENDIF
033290941019    1C                   ENDIF
033300941019     C**
033310991001     C* F I A R B T 0 F   SECONDA BOLLA
033320941019     C**
033330911211     C     FLGCVR        IFEQ      '2'
033340941102     C                   SELECT
033350941102     C     D66RTA        WHENEQ    'S'
033360950616     C                   SETON                                        1529        RITASSAZ
033370941102     C*
033380941102     C     D66BSF        WHENEQ    'S'
033390941102     C                   SETON                                        13          VARIA  SF
033400941102     C*
033410941102     C     D66BFI        WHENEQ    'S'
033420991006     C                   SETON                                        1407        VARIA  FAT
033430941102     C*
033440941102     C                   ENDSL
033450911211     C*
033460911219     C                   MOVEL     *BLANKS       DESKSC
033470941019     C                   MOVEL     ARBRSD        W008A             8
033480941019     C                   MOVEL     W008A         DESKSC
033490131120     c                   clear                   sav_ift
033500941019     C*
033510941019     C* PGM NON RICHIAMATO
033520941019    2C     *IN20         IFEQ      *OFF
033530920128     C* NON ESISTE ESTENSIONE BOLLA
033540990930    3C     WAR62         IFEQ      ' '
033550911211     C                   MOVEL     ARBLNA        VIDKLP
033560911211     C                   MOVEL     9999          VIDKSC
033570911212     C                   MOVEL     *ZEROS        VIDCTR
033580050202     C                   MOVEL     *ZEROS        SAVCTR
033590060511     c                   clear                   ctrbolla
033600991013     C                   MOVE      *ZEROS        SAVKSC            7 0
033610060511     c                   clear                   kscbolla
033620941019     C*
033630941019   X3C                   ELSE
033640941019     C*
033650131120     c                   z-add     ar6ift        sav_ift
033660990930     C                   MOVEL     AR6DIV        VI2DIV
033670020305     C                   MOVEL     AR6DIV        WDIV
033680990930     C                   MOVEL     AR6SV1        VI2SV1
033690040122     C**!!!              Z-ADD     AR6VA1        VI2VA1
033700040122     C**!!!              Z-ADD     AR6VA1        VA2VA1
033710040123      * carico le varie
033720040122     c                   Movel     Ar6Sv1        wsv1
033730040122     c                   Z-add     Ar6Va1        wva1
033740040122     c                   ExSr      CarVar1
033750040122     c                   Movel     Ar6Sv2        wsv1
033760040122     c                   Z-add     Ar6Va2        wva1
033770040122     c                   ExSr      CarVar1
033780040122     c                   Movel     Ar6Sv3        wsv1
033790040122     c                   Z-add     Ar6Va3        wva1
033800040122     c                   ExSr      CarVar1
033810040122      * Verifico se ci sono altre varie
033820040126     c                   Movel     '2'           wtrc
033830040123     c     Karb2         Setll     Fiar701l
033840040122Do  4c                   Do        *Hival
033850040123     c     Karb2         Reade(n)  Fiar701l
033860040122     c                   If        %Eof(Fiar701l)
033870040122     c                   Leave
033880040122     c                   EndIf
033890040122     c                   Movel     Ar7Svn        wsv1
033900040122     c                   Z-add     Ar7Van        wva1
033910040122     c                   ExSr      CarVar1
033920040122    4c                   EndDo
033930990930     C                   MOVEL     AR6CEI        VI2CEI
033940990930     C                   MOVEL     AR6KSC        VIDKLP
033950990930     C                   MOVE      AR6KSC        VIDKSC
033960991013     C                   MOVEL     AR6KSC        SAVKSC
033970060511     C                   MOVEL     AR6KSC        kscbolla
033980990930     C                   MOVEL     AR6CTR        VIDCTR
033990050202     C                   MOVEL     AR6CTR        SAVCTR
034000060511     C                   MOVEL     AR6CTR        ctrbolla
034010941019    3C                   END
034020941019     C*
034030941019   X2C                   ELSE
034040941019     C*
034050990930     C* PROGRAMMA     RICHIAMATO: PRENDO DATI DA DARBT 2BOLLA
034060990930     C                   MOVEL     §BTDIV        VI2DIV
034070991006     C                   MOVEL     §BTDIV        WDIV
034080040123     C**!!!              MOVEL     §BTSV1        VI2SV1
034090040123     C**!!!              Z-ADD     §BTVA1        VI2VA1
034100040123     C**!!!              Z-ADD     §BTVA1        VA2VA1
034110040123      * carico le varie
034120040123     c                   Do        20            c
034130040123     c                   Movel     §sv(c)        wsv1
034140040123     c                   Z-add     §va(c)        wva1
034150040123     c                   ExSr      CarVar1
034160041130     c                   If        ContaSv = 6
034170040123     c                   Leave
034180040123     c                   EndIf
034190040123     c                   EndDo
034200040123
034210990930     C                   MOVEL     §BTCEI        VI2CEI
034220990930     C                   MOVEL     §BTKSC        VIDKLP
034230990930     C                   MOVE      §BTKSC        VIDKSC
034240991013     C                   MOVE      §BTKSC        SAVKSC
034250060511     c                   clear                   kscbolla
034260990930     C                   MOVEL     §BTCTR        VIDCTR
034270050202     C                   MOVEL     §BTCTR        SAVCTR
034280060511     c                   clear                   ctrbolla
034290941019    2C                   END
034300050202     c
034310050202     C* CARICO LE TARIFFE E DEOCIFICO IL CTR CHE C'E'I
034320050202     C                   Z-ADD     SAVKSC        KSC
034330050202     C                   EXSR      CARCTR
034340920128     C*
034350920128     C                   MOVEL     *BLANKS       DESCEI
034360941019    2C     VI2CEI        IFNE      ' '
034370911211     C                   MOVEL     'EI'          COD
034380911211     C                   MOVEL     *BLANKS       KEY
034390051214     C                   MOVEL(P)  VI2CEI        KEY
034400941212     C                   EXSR      CTABEL
034410911211     C  N30              MOVEL     TBLUNI        DESCEI
034420941019    2C                   END
034430941019     C* GIRO DATA FATTURA
034440990930    1C     WAR62         IFEQ      'E'
034450990930     C     AR6DFT        ANDGT     0
034460990930     C                   Z-ADD     AR6DFT        G02INV
034470941019     C                   MOVEL     '3'           G02ERR
034480941019     C                   CALL      'XSRDA8'
034490941019     C                   PARM                    WLBDAT
034500941019     C*
034510941019     C                   Z-ADD     G02DAT        VI2DFT
034520991007     C                   Z-ADD     AR6FIV        VI2FIV
034530991007     C                   Z-ADD     AR6NFT        VI2NFT
034540941019    2C                   ENDIF
034550911211     C                   END
034560941019     C**
034570050202     C* F N A R B K 0 F
034580941019     C**
034590941019    1C     FLGCVR        IFEQ      'K'
034600941019     C*
034610941019     C* CODICE BOLLA PRECEDENTE
034620941019     C                   MOVEL     ARBCBO        OLDCBO
034630941019     C* SALVO CAMPI DEL CODICE BOLLA CHE MI INTERESSANO
034640941019     C                   MOVEL     §3ATB1        SAVTB1            2
034650941102     C                   MOVEL     §3ARBL        SAVRBL            1
034660941019     C                   MOVEL     §3ATB2        SAVTB2            2
034670941019     C                   MOVEL     §3AFCA        SAVFCA            1 0
034680941019     C                   CLEAR                   DENCBO
034690050414     C*
034700050414     C* VERIFICO SE SI TRATTA DI UN PREPAGATO
034710050414     C                   MOVEL     '1'           WTRC
034720050414     C     KARB2         CHAIN(N)  FIAR6000                           30
034730050414     C  N30              MOVEL     'E'           WAR6
034740050414     C*
034750050414     C
034760920128     C*
034770920128     C* PROGRAMMA NON RICHIAMATO: PRENDO DATI DA BOLLA
034780941019    2C     *IN20         IFEQ      *OFF
034790941019     C*
034800941019    3C     WAR9          IFEQ      'E'
034810941019     C                   MOVEL     AR9TIC        VIDTIC
034820941019     C                   MOVEL     AR9CAS        VIDCAS
034830941019     C                   MOVEL     AR9VCA        VIDVCA
034840941215     C                   MOVEL     AR9VCA        VARVCA
034850941102     C                   MOVEL     AR9GCA        VIDGCA
034860941019    3C                   ENDIF
034870941019     C*
034880941019     C                   MOVEL     '?'           NEWCBO
034890920128     C*
034900941019   X2C                   ELSE
034910920128     C*
034920920128     C* PROGRAMMA     RICHIAMATO: PRENDO DATI DA DSARBK
034930920128     C                   MOVEL     §BKTIC        VIDTIC
034940941019     C                   MOVEL     §BKCAS        VIDCAS
034950941102     C                   MOVEL     §BKGCA        VIDGCA
034960941019     C                   MOVEL     §BKCBN        NEWCBO
034970941220     C                   MOVEL     §BKVCA        VARVCA
034980941220     C                   MOVEL     §BKVCA        VIDVCA
034990950420     C* SE NON PASSATO IMPOSTO QUELLO DELLA BOLLA
035000950420     C     NEWCBO        IFEQ      *BLANKS
035010950420     C                   MOVEL     ARBCBO        NEWCBO
035020950420     C                   ENDIF
035030920128     C*
035040920128     C* DECODIFICA NUOVO CODICE BOLLA
035050920128     C                   MOVEL     '3A'          COD
035060950420     C                   MOVEL(P)  NEWCBO        KEY
035070941212     C                   EXSR      CTABEL
035080920128     C  N30              MOVEL     TBLUNI        DS3A
035090941019     C   30              CLEAR                   DS3A
035100941102     C                   MOVEL     §3ADES        DENCBO
035110941019    2C                   END
035120941019     C*
035130920325     C* VARIAZIONE IMPORTO CONTRASSEGNO
035140941019    2C     VIDCVR        IFEQ      'KA'                                          UFFICIO
035150931118     C     VIDCVR        OREQ      'KB'                                          DOPO CONS
035160931118     C     VIDCVR        OREQ      'KP'                                          PADRONCINO
035170920325     C                   SETON                                        21
035180941102     C                   MOVEL     ARBCBO        NEWCBO
035190941019    2C                   END
035200941019    1C                   END
035210941019     C**
035220070309     C* F N A R B G 0 F
035230941019     C**
035240950120     C     FLGCVR        IFEQ      'C'
035250950120     C* SE E' CAUSALE CP PROTEGGO I CAMPI DELLA CONSEGNA RICHIESTA E
035260950120     C*  GG CHIUSURA
035270950120     C     VIDCVR        IFEQ      'CP'
035280950120     C                   SETON                                        16
035290950120     C                   ENDIF
035300941019     C* PGM NON RICHIAMATO
035310941019     C     *IN20         IFEQ      *OFF
035320920320     C* DATA CONSEGNA RICHIESTA
035330941019     C                   Z-ADD     ARBDCR        G02INV
035340920320     C*
035350920320     C                   Z-ADD     ARBHCR        VIDHCR
035360920320     C*
035370920320     C                   MOVEL     ARBTCR        VIDTCR
035380930510     C                   MOVEL     ARBGC1        VIDGC1
035390930510     C                   MOVEL     ARBGC2        VIDGC2
035400941019     C                   MOVEL     ARBTC1        VIDTC1
035410941102     C                   MOVEL     ARBTC2        VIDTC2
035420111025     c* Se il pgm non è richiamato, verifico se nella DS nel campo tipo §BGTCR
035430111025     c*  c'e' una "S" --> significa che posso aggiornare la consegna richiesta
035440111025     c*  in FIARP se bolla in distinta (solo da gestione telefonate o da richiamo
035450111025     c*  con passaggio dati) In caso contrario non posso farlo e sid eve dare errore
035460111207     c* ES - 07/12/11 -  per il momento asterisco
035470111207     c****               eval      Dcrindist=§BGTCR
035480930510     C*
035490941019   X2C                   ELSE
035500930510     C*
035510941019     C                   Z-ADD     §BGDCR        G02INV
035520930510     C*
035530930510     C                   Z-ADD     §BGHCR        VIDHCR
035540930510     C*
035550930510     C                   MOVEL     §BGTCR        VIDTCR
035560930510     C                   MOVEL     §BGGC1        VIDGC1
035570930510     C                   MOVEL     §BGGC2        VIDGC2
035580941019     C                   MOVEL     §BGTC1        VIDTC1
035590941019     C                   MOVEL     §BGTC2        VIDTC2
035600111025     c                   eval      Dcrindist='S'
035610920320     C                   END
035620941019     C*
035630941019     C* GIRO DATA CONSEGNA RICHIESTA
035640941019     C                   MOVEL     '3'           G02ERR
035650941019     C                   CALL      'XSRDA8'
035660941019     C                   PARM                    WLBDAT
035670941019     C                   Z-ADD     G02DAT        VIDDCR
035680070309     c
035690070309     c* Se lna con GEO attiva, verifico per eventuale modifica consegna ric
035700070309     c*  se c'e' sulla bolla un evento per il quale è inseribile la
035710070319     c*  modifica della data consegna richiesta
035720070319     c                   if        FGSgeot='S'
035730070309     c     karb          setgt     fnevb05l
035740070309     c     karb          readpe    fnevb05l
035750070309     c                   dow       not %eof(fnevb05l) and eveADR=' '
035760090526     c* controllo a seconda che sia o meno richiamato il pgm
035770090526     c  n20evbcev        lookup    ADRs                                   30
035780090526     c   20evbcev        lookup    ADRC                                   30
035790070309     c                   if        *in30
035800070309     c                   eval      eveADR='S'
035810070309     c                   else
035820070309     c     karb          readpe    fnevb05l
035830070309     c                   endif
035840070309     c                   enddo
035850070309     c
035860070309     c                   endif
035870070309     c
035880930510     C                   END
035890020531     c* se modificato destinatario in disagiato o supermercato imposto il
035900020531     c* relativo flag nelle cosegne particolari
035910041015     c* Lo faccio senza testare se arbgva = "U" perchè se mi trovo in
035920041015     c* questa condizione è certo che arbgva è <> da "U"
035930070907     c**   wdisag        ifne      *blanks
035940070907     c**                 select
035950070907     c**   vidtc1        wheneq    *blanks
035960070907     c**                 movel     wdisag        vidtc1
035970070907     c**   vidtc2        wheneq    *blanks
035980070907     c**                 movel     wdisag        vidtc2
035990070907     c**                 endsl
036000070907     c**   vidtc1        ifeq      vidtc2
036010070907     c**                 clear                   vidtc2
036020070907     c**                 endif
036030070907     c**                 endif
036040950104     C**
036050070309     C* F N A R B M 0 F
036060950104     C**
036070950104     C     VIDCVR        IFEQ      'MI'
036080950104     C* PGM NON RICHIAMATO: PER ORA NON ESISTE IL RICHIAMO
036090950104     C* DECODIFICO TIPO TARIFFA
036100950104     C                   MOVEL     ARBCTR        VIDCTR
036110050202     C                   MOVEL     ARBCTR        SAVCTR
036120060511     C                   MOVEL     ARBCTR        ctrbolla
036130050202     C                   CLEAR                   Fnlv59DS
036140050202     C                   MOVEL     VIDCTR        ILV59CTR
036150050202     C                   MOVEL     'S'           ILV59CFO
036160050202     C                   CALL      'FNLV59R'
036170050202     C                   PARM                    Fnlv59DS
036180950104     C*
036190050202     C     OLV59ERR      IFEQ      ' '
036200050202     C                   MOVEL     OLV59DFS      DE2CTR
036210950104     C                   ENDIF
036220950104     C                   MOVEL     ARBKSC        VIDKLP
036230950104     C                   MOVE      ARBKSC        VIDKSC
036240970822     C                   MOVE      ARBKSC        MEMKSC
036250950104     C                   MOVEL     ARBRSM        W008A
036260950104     C                   MOVEL     W008A         DESKSC
036270051220     c                   clear                   memntw
036280051220     c     vidklp        chain     azorg01l
036290051220     c                   if        %found(azorg01l)
036300051220     C                   MOVEL     ORGDE3        OG143
036310051220     C                   MOVEL     §OGNTW        memntw
036320051220     c                   endif
036330160225     c* Se lna fedex --> carico le tariffe di cartello FED
036340160225     c
036350970813     C* SE CODICE 8888 CARICO L'INDIRIZZO COMPLETO
036360950104     C     VIDKSC        IFEQ      8888
036370970813     C                   SETOFF                                       22
036380950104     C                   MOVEL     ARBRSM        VIDRSM
036390950104     C                   MOVEL     ARBINM        VIDINM
036400950104     C                   MOVEL     ARBCAM        VIDCAM
036410950104     C                   MOVEL     ARBLOM        VIDLOM
036420970808     C                   MOVEL     ARBCAM        VARCAM
036430970808     C                   MOVEL     ARBLOM        VARLOM
036440950104     C                   MOVEL     ARBPRM        VIDPRM
036450950104     C                   MOVEL     ARBNZM        VIDNZM
036460950104     C                   MOVEL     ARBFAP        VIDFAP
036470950104     C                   MOVEL     ARBCTR        VIDCTR
036480950104     C                   MOVEL     ARBCPI        VIDPIM
036490050922     C     'PRIVATO'     SCAN      vidpim        posiz             2 0    30
036500050922     c* solo numeri
036510050922     c* "PRIVATO senza iso
036520050922     c     *in30         ifeq      *off
036530050922     c     vidism        andge     *zeros
036540050922     c     *in30         oreq      *on
036550050922     c     posiz         andlt     3
036560050922     c                   clear                   vidpim
036570050922     c                   movel     arbcpi        vidcpm
036580050922     c                   endif
036590970813     C                   ELSE
036600970813     C                   SETON                                        22
036610950104     C                   ENDIF
036620950104     C                   ENDIF
036630961211     C**
036640970110     C* VARIAZIONE DEL RIFERIMENTO MITTENTE ALFABETICO E NATURA MERCE
036650961211     C**
036660961211     C     D48CVB        IFEQ      'RM'
036670970110     C                   MOVEL     ARBRMA        VIDRMA
036680970110     C                   MOVEL     ARBNAS        VIDNAS
036690961211     C                   END
036700950104     C*
036710011018      * Se l'importo d'assicurare è a 0 e la divisa non è impostata
036720011018     C     VIDIAS        IFEQ      *ZEROS
036730011018     C     VIDVAS        ANDEQ     *BLANKS
036740011018      * imposto la divisa della nazione del mittente
036750011018      * se estero
036760011018     C     MITVLT        IFNE      *BLANKS
036770011018     C     WMITIE        ANDEQ     'E'
036780011018     C                   MOVEL     MITVLT        VIDVAS
036790011018     C                   MOVEL     MITVLT        VARVAS
036800011018     C                   ELSE
036810011018      * altrimenti imposto la divisa della moneta di conto
036820011017     C                   MOVEL     §GEDCN        VIDVAS
036830011017     C                   MOVEL     §GEDCN        VARVAS
036840011017     C                   ENDIF
036850011018     C                   ENDIF
036860011018      *
036870011017      * Se la divisa e l'importo del valore merce non sono impostati
036880011018     C     VIDVAD        IFEQ      *BLANKS
036890011018     C     VIDVMD        ANDEQ     *ZEROS
036900011018      * imposto la divisa della nazione del mittente
036910011018      * se estero
036920011018     C     MITVLT        IFNE      *BLANKS
036930011018     C     WMITIE        ANDEQ     'E'
036940011018     C                   MOVEL     MITVLT        VIDVAD
036950011018     C                   MOVEL     MITVLT        VARVAD
036960011018     C                   ELSE
036970011018      * altrimenti imposto la divisa della moneta di conto
036980011017     C                   MOVEL     §GEDCN        VIDVAD
036990011017     C                   MOVEL     §GEDCN        VARVAD
037000011017     C                   ENDIF
037010011018     C                   ENDIF
037020011018      *
037030011017      * Se la divisa e l'importo del contrassegno non sono impostati
037040011018     C     VIDVCA        IFEQ      *BLANKS
037050011018     C     VIDCAS        ANDEQ     *ZEROS
037060011018      * imposto la divisa della nazione del destinatario
037070011018      * se estero
037080011018     C     DESVLT        IFNE      *BLANKS
037090011018     C     WDESIE        ANDEQ     'E'
037100011018     C                   MOVEL     DESVLT        VIDVCA
037110011018     C                   MOVEL     DESVLT        VARVCA
037120011018    2C                   ELSE
037130011018      * altrimenti imposto la divisa della moneta di conto
037140011017     C                   MOVEL     §GEDCN        VIDVCA
037150011017     C                   MOVEL     §GEDCN        VARVCA
037160011017     C                   ENDIF
037170011018     C                   ENDIF
037180030203      *------------
037190030124      * Bancali
037200030711      *  solo x causale 'BR'
037210030711If  1c                   If        D48Cvb = 'BR'
037220030124     c                   Clear                   dAr5Ban
037230030124     c                   Eval      kAr5Trd = 'BAN'
037240030725     c     kAr5          Chain     Fiar501l
037250030711     c                   If        %Found(Fiar501l)
037260030124     c                   Movel     Ar5Uni        dAr5Ban
037270030711     c                   EndIf
037280030124     c                   Eval      VidNba = §Ar5Nba
037290030124     c                   Eval      VidNb2 = §Ar5Nb2
037300030124     c                   Eval      VidRb1 = §Ar5Rb1
037310030124     c                   Eval      VidRb2 = §Ar5Rb2
037320030124     c                   Eval      VidNomb = §Ar5Nomb
037330030124     c                   Eval      VidMotb = §Ar5Motb
037340030124If  2c                   If        §Ar5Tba <> *Zeros
037350030612     c                   Clear                   Tibs02DS
037360030124     c                   Clear                   dTba
037370030124     c                   Movel     'C'           T02Mod
037380030124     c                   Movel     Knsif         T02Sif
037390030124     c                   Movel     'TBA'         T02Cod
037400030124     c                   Movel(p)  §Ar5Tba       T02Ke1
037410030124     c                   Call      'TIBS02R'
037420030124     c                   Parm                    Kpjba
037430030612     c                   Parm                    Tibs02DS
037440030124If  3c                   If        T02Err = *Blanks
037450030124     c                   Movel     T02Uni        dTba
037460030124    3c                   EndIf
037470030124     c                   Eval      VidTba = §TbaDesc
037480030124     c                   Eval      VidTbar = §TbaDesc
037490030124    2c                   EndIf
037500030124If  2c                   If        §Ar5Tb2 <> *Zeros
037510030612     c                   Clear                   Tibs02DS
037520030124     c                   Clear                   dTba
037530030124     c                   Movel     'C'           T02Mod
037540030124     c                   Movel     Knsif         T02Sif
037550030124     c                   Movel     'TBA'         T02Cod
037560030124     c                   Movel(p)  §Ar5Tb2       T02Ke1
037570030124     c                   Call      'TIBS02R'
037580030124     c                   Parm                    Kpjba
037590030612     c                   Parm                    Tibs02DS
037600030124If  3c                   If        T02Err = *Blanks
037610030124     c                   Movel     T02Uni        dTba
037620030124    3c                   EndIf
037630030124     c                   Eval      VidTb2 = §TbaDesc
037640030124     c                   Eval      VidTb2r = §TbaDesc
037650030124    2c                   EndIf
037660030711
037670080805     c* Se pgm richiamato, imposto dati dalla DSARBD
037680080805    2c                   if        *in20
037690080805     c                   Eval      VidRb1 = §bdpkf
037700080805     c                   Eval      VidRb2 = §bdvlf
037710080805    3c                   if        §bdrsd<>*blanks
037720080805     c                   Eval      VidNomb = §bdrsd
037730080805    3c                   endif
037740080805    3c                   if        §bdrd2<>*blanks
037750080805     c                   Eval      VidMotb = §bdrd2
037760080805     c                   else
037770080805     c                   Eval      VidMotb = 'PDA  '
037780080805    3c                   EndIf
037790080805
037800080805    2c                   EndIf
037810080805    1c                   EndIf
037820040608      *------------
037830040608      * Supermercati - telefono e referente
037840040608      *  solo x causale 'CS'
037850040608If  1c                   If        D48Cvb = 'CS'
037860040608     c                   Eval      wokscr = *Off
037870040608     c                   Clear                   dAr5Scr
037880040608     c                   Eval      kAr5Trd = 'SCR'
037890040608     c     kAr5          Setgt     Fiar501l
037900040608do  2c                   Do        *Hival
037910040608     c     kAr5          Readpe(n) Fiar501l
037920040608     c                   If        %Eof(Fiar501l)
037930040608     c                   Leave
037940040608     c                   EndIf
037950040608     c                   Movel     Ar5Uni        dAr5Scr
037960040608     c                   Eval      VidNom = §Ar5Nom
037970040608     c                   Eval      VidTel = §Ar5Tel
037980040608     c                   Eval      wokscr = *On
037990040608     c                   Leave
038000040608e   2c                   EndDo
038010040608      * se non ho trovato il rcd SCR cerco i dati sul rcd GEN
038020040608if  2c                   If        wokscr = *Off
038030070910     c***                Clear                   dAr5Gen
038040070910     c***                Eval      kAr5Trd = 'GEN'
038050070910     c***  kAr5          Chain(n)  Fiar501l
038060070910     c***                If        %Found(Fiar501l)
038070070910     c***                Movel     Ar5Uni        dAr5Gen
038080070910     c***                EndIf
038090070910     c***                Eval      VidNom = §Ar5Ref
038100070910     c***                Eval      VidTel = §Ar5Teld
038110070910     c                   Eval      VidNom = PAr5Ref
038120070910     c                   Eval      VidTel = PAr5Teld
038130040608e   2c                   EndIf
038140040608e   1c                   EndIf
038150911209     C*
038160910429     C                   ENDSR
038170941102     C*
038180941102     C* CARICO LA VARIA IN SFL ---------------------------------------*
038190941102     C     CARVAR        BEGSR
038200941102     C*
038210941102     C     WSV1          IFNE      ' '
038220011018      * Controllo se è una bolla di reso e se ha la varia 'N' 88888888
038230011018     C     ARBFBR        IFEQ      'S'
038240011018     C     WSV1          ANDEQ     'N'
038250011018     C     WVA1          ANDEQ     88888888
038260011018     C                   MOVEL     '1'           FLGVRN            1
038270011018     C                   ENDIF
038280990930     C* CARICO SOLO SE VARIA FA PARTE DEL TOTALE IMPONIBILE
038290990930     C     WSV1          LOOKUP    SVA                                    33
038300990930     C     *IN33         IFEQ      *ON
038310941102     C                   ADD       1             NRR
038320941102     C     NRR           CHAIN     LR48S03                            30
038330941102     C                   MOVEL     WSV1          V3CSV1
038340941102     C                   Z-ADD     WVA1          V3CVA1
038350991006     C                   Z-ADD     WVA1          VARVA1
038360941102     C   30              WRITE     LR48S03
038370941102     C  N30              UPDATE    LR48S03
038380941102     C                   ENDIF
038390990930     C                   ENDIF
038400941102     C                   ENDSR
038410040122
038420040122      *---------------------------------------------------------------*
038430040122      * CARICO LA VARIA DELLA SECONDA BOLLA
038440040122      *---------------------------------------------------------------*
038450040122     c     CarVar1       BegSr
038460040122
038470040122if  1c                   If        wsv1 <> *Blanks
038480040122      * Carico la varia solo se fa parte del totale imponibile
038490040122     c     wsv1          Lookup    sva                                    31
038500040122if  2c                   If        *In31
038510040122     c                   add       1             ContaSv
038520040122     c                   add       wva1          TotImv
038530040122
038540040122s   3c                   Select
038550040122     c                   When      ContaSv = 1
038560040122     c                   Movel     wsv1          Vi2Sv1
038570040122     c                   Z-add     wva1          Vi2Va1
038580040122     c                   Z-add     wva1          Va2Va1
038590040122     c                   When      ContaSv = 2
038600040122     c                   Movel     wsv1          Vi2Sv2
038610040122     c                   Z-add     wva1          Vi2Va2
038620040122     c                   Z-add     wva1          Va2Va2
038630040122     c                   When      ContaSv = 3
038640040122     c                   Movel     wsv1          Vi2Sv3
038650040122     c                   Z-add     wva1          Vi2Va3
038660040122     c                   Z-add     wva1          Va2Va3
038670040122     c                   When      ContaSv = 4
038680040122     c                   Movel     wsv1          Vi2Sv4
038690040122     c                   Z-add     wva1          Vi2Va4
038700040122     c                   Z-add     wva1          Va2Va4
038710041130     c                   When      ContaSv = 5
038720041130     c                   Movel     wsv1          Vi2Sv5
038730041130     c                   Z-add     wva1          Vi2Va5
038740041130     c                   Z-add     wva1          Va2Va5
038750041130     c                   When      ContaSv = 6
038760041130     c                   Movel     wsv1          Vi2Sv6
038770041130     c                   Z-add     wva1          Vi2Va6
038780041130     c                   Z-add     wva1          Va2Va6
038790040122    3c                   EndSl
038800040122
038810040122    2c                   EndIf
038820040122
038830040122    1c                   EndIf
038840040122
038850040122     c                   EndSr
038860910429     C*
038870991001     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
038880991001     C     CARWAR        BEGSR
038890991001     C*
038900991001     C                   CLEAR                   WINL
038910991001     C*
038920991001     C                   CLEAR                   WSV
038930991001     C                   CLEAR                   WVA
038940991001     C                   CLEAR                   C
038950991001    1C                   DO        20            NRR
038960991001     C     NRR           CHAIN     LR48S03                            30
038970991001     C*
038980991001    2C     *IN30         IFEQ      *OFF
038990991001     C     V3CSV1        ANDNE     *BLANKS
039000991001     C                   ADD       1             C
039010991001     C                   MOVEL     V3CSV1        WSV(C)
039020991001     C                   Z-ADD     V3CVA1        WVA(C)
039030991001    2C                   ENDIF
039040991001    1C                   ENDDO
039050991001     C* SE RICHIESTA IL CONTROLLO ESISTENZA DELL'INOLTRO PROCEDO
039060991001     C     WCKINL        IFEQ      '1'
039070991001     C                   Z-ADD     1             II
039080991001     C     §$2FIN        LOOKUP    WSV(II)                                30
039090991001     C   30WVA(II)       IFGT      *ZEROS
039100991001     C                   MOVE      '1'           WINL              1
039110991001     C                   ENDIF
039120991001     C                   ENDIF
039130991001     C*
039140991001     C                   ENDSR
039150040123      *---------------------------------------------------------------*
039160040123      * Carico le varie della seconda bolla in una sk di comodo
039170040123      *---------------------------------------------------------------*
039180040123     c     Carwar1       Begsr
039190040123
039200040123     c                   Clear                   wsv
039210040123     c                   Clear                   wva
039220040123     c                   Clear                   c
039230040123
039240040123     c                   If        vi2sv1 <> *Blanks
039250050401     c                   add       1             c
039260050401     c                   Movel     vi2Sv1        wsv(c)
039270050401     c                   Z-add     vi2va1        wva(c)
039280040123     c                   EndIf
039290040123     c                   If        vi2sv2 <> *Blanks
039300050401     c                   add       1             c
039310050401     c                   Movel     vi2Sv2        wsv(c)
039320050401     c                   Z-add     vi2va2        wva(c)
039330040123     c                   EndIf
039340040123     c                   If        vi2sv3 <> *Blanks
039350050401     c                   add       1             c
039360050401     c                   Movel     vi2Sv3        wsv(c)
039370050401     c                   Z-add     vi2va3        wva(c)
039380040123     c                   EndIf
039390040123     c                   If        vi2sv4 <> *Blanks
039400050401     c                   add       1             c
039410050401     c                   Movel     vi2Sv4        wsv(c)
039420050401     c                   Z-add     vi2va4        wva(c)
039430040123     c                   EndIf
039440041130     c                   If        vi2sv5 <> *Blanks
039450050401     c                   add       1             c
039460050401     c                   Movel     vi2Sv5        wsv(c)
039470050401     c                   Z-add     vi2va5        wva(c)
039480041130     c                   EndIf
039490041130     c                   If        vi2sv6 <> *Blanks
039500050401     c                   add       1             c
039510050401     c                   Movel     vi2Sv6        wsv(c)
039520050401     c                   Z-add     vi2va6        wva(c)
039530041130     c                   EndIf
039540040123
039550040123     c                   EndSr
039560991012     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
039570991012     C     RDTASV        BEGSR
039580991012     C*
039590991012     C                   CLEAR                   C
039600030620     C                   CLEAR                   WVAR3A
039610040123     c                   Clear                   wvar3ai
039620991012    1C                   DO        20            NRR
039630991012     C     NRR           CHAIN     LR48S03                            30
039640991012    2C     *IN30         IFEQ      *OFF
039650991012     C                   ADD       1             C
039660020503     c* se la varia è uguale a quella prevista dal tipo bolla, non richiamo
039670020503     c*  piu' la tassazione
039680040123     c                   if        §3asva<>*blanks and v3csv1=§3asva
039690040123     c                   If        vidimv > *Zeros
039700020503     c                   eval      wtassa='N'
039710030620      * mi salvo l'imnporto della varia
039720030620     c                   eval      itassa = v3cva1
039730040123     c                   EndIf
039740030620      * mi salvo un campo di comodo
039750030620     c                   movel     'S'           Wvar3a            1
039760040123      * memorizzo anche se valorizzata
039770040123     c                   If        v3cva1 > 0
039780040123     c                   Eval      wvar3ai = 'S'
039790040123     c                   EndIf
039800030620      *
039810020503     c                   endif
039820030620      * verifico se esiste la varia "D" non la ricalcolo in caso di fattura immediata
039830030620     c                   if        v3csv1 = 'D'
039840030620     c                   eval      wtassad = 'N'
039850030620     c                   endif
039860030620      *
039870030624      * se esiste la varia "&" con tutti 8888888 non la passo al TNSF20R
039880030624      * ma passo un flag  di richiesta calcolo varia "&"
039890030709     C                   if        v3csv1 = '&' and v3cva1 = 88888888 and
039900030709     C                             flgsf20 = 'S'
039910030624     C                   eval      TASFCAA = 'S'
039920030624     C                   else
039930991012     C                   MOVEL     V3CSV1        VSV(C)
039940991012     C                   MOVEL     V3CVA1        VVA(C)
039950030624     c                   endif
039960030624      *
039970991012    2C                   ENDIF
039980991012    1C                   ENDDO
039990991012     C                   ENDSR
040000920120     C*--- EMISSIONE E GESTIONE FORMATO 2 ----------------------------*
040010170130     C     EMFOR2        BEGSR
040020941102     C*
040030170130     c                   clear                   wpes_pre
040040170130     c                   clear                   wvol_pre
040050170130
040060920120     C     FOR02         TAG
040070941102     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
040080950519    1C     *IN90         IFEQ      *ON
040090950114     C     D48FFR        OREQ      'S'
040100950607     C     D48FFR        OREQ      'V'
040110941214     C                   WRITE     LR48T03
040120941014     C                   EXFMT     LR48D02
040130950519    1C                   END
040140941102     C*
040150941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
040160941102     C                   SETOFF                                       2890
040170941102     C                   SETOFF                                       404142
040180941102     C                   SETOFF                                       434445
040190941102     C                   SETOFF                                       464748
040200061115     C                   SETOFF                                       495051
040210140408     C                   SETOFF                                       5268
040220941220     C* F12 - RITORNO
040230941220    1C     *INKL         IFEQ      *OFF
040240070329     c
040250070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
040260070329     c                   if        *in87 and *inkr
040270070329     c                   exsr      CALLNOTELDV
040280070329     c                   goto      for02
040290070329     c                   endif
040300941216     C*
040310941216     C* SE NON POSSO VARIARE NE IL PESO, NE IL VOLUME, NON CONTROLLO
040320941216     C* E NON AGGIORNO
040330941220    2C     *IN88         IFEQ      *OFF
040340941216     C     *IN89         OREQ      *OFF
040350950114     C     *IN24         OREQ      *ON
040360941216     C*
040370941216     C* CONTROLLO FORMATO
040380920120     C                   EXSR      CONTR2
040390941215     C*
040400941220     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
040410941220    3C     *IN90         IFEQ      *ON
040420941215     C     D48FFR        ANDEQ     'E'
040430941215     C                   MOVEL     VIDMSG        D48MSG
040440941215     C                   MOVEL     '2'           D48ERR
040450941215     C                   GOTO      ENDEM2
040460941220    3C                   ENDIF
040470140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
040480140502     c*   (d48ffr='E' e 26 off)
040490140502     c  n90
040500140502     cann26              if        d48ffr='E'
040510140502     C                   GOTO      ENDEM2
040520140502     c                   endif
040530950607     C  NKF
040540950607     CANN26
040550920120     COR 90              GOTO      FOR02
040560060227     c* richiamo routine di richiesta/controllo motivazione
040570060301     c                   if        d66mtv = 'S' and w48mct = *blanks
040580060227     c                   exsr      ges_mtv
040590060301     c                   if        olv85f12 = '1'
040600060301     c                   if        ilv85wdw = ' '
040610060227     c                   goto      for02
040620060301     c                   else
040630060301     c                   eval      *inkl = '1'
040640060301     C                   GOTO      ENDEM2
040650060227     c                   endif
040660060301     c                   endif
040670060227     C     *IN90         IFEQ      *ON
040680060227     C     D48FFR        ANDEQ     'E'
040690060227     C                   MOVEL     VIDMSG        D48MSG
040700060227     C                   MOVEL     '2'           D48ERR
040710060227     C                   GOTO      ENDEM2
040720060227     C                   ENDIF
040730060227     c                   endif
040740920120     C*
040750141020     c* Se Richiesta modifica di indirizzo tale da variare la LNA emetto window
040760141020     c* per segnalare e richiedere una delle seguenti opzioni:
040770141020     c*  -Forzare la variazione senza variare la lna
040780141020     c*  -Richiedere il dirottamento
040790141111    3c                   if        d48ffr<>'E'  and *in01
040800141029     c                             and (%lookup('999':skFilOkdir)>0 or
040810141029     c                             (%lookup(%editc(arblna:'X'):skFilOkdir)>0 and
040820141029     c                              %lookup(%editc(dutpou:'X'):skFilOkdir)>0) )
040830141020     c                   clear                   wdir
040840141020    4C     d_o95lna      ifne      ARBLNA
040850141022     c* le bolle export non le dirottiamo mai da qui
040860141022     c     lnantw        andne     'DPD'
040870141022     c     lnantw        andne     'EEX'
040880141022     c     lnantw        andne     'FED'
040890141219     c* no bolle di recupero
040900141219     c     §3arbl        andne     'R'
040910141204    5C     SAVCAD        IFNE      VIDCAD
040920141204     C     SAVLOD        ORNE      VIDLOD
040930141204     C     SAVrsd        ORNE      VIDrsd
040940141204     C     SAVind        ORNE      VIDind
040950141204     C     SAVprd        ORNE      VIDprd
040960141204     C     SAVnzd        ORNE      VIDnzd
040970141204     c     vidzng        oreq      savzng
040980141203     c     vidffd        andeq     *blanks
040990141021     c                   exsr      ges_wdw02
041000141021     c                   if        winf12='1'
041010141021     c                   goto      for02
041020141021     c                   endif
041030141020    5C                   END
041040141020    4C                   END
041050141020    4c                   if        wdir='1'
041060141212
041070141212     c* se hanno cambiato il Fermo deposito devo anche eseguire la REGIS2
041080141212     c                   if        arbffd<>%subst(vidffD:1:1)
041090141212     c                   eval      wnoindir='S'
041100141212     c                   exsr      regis2
041110141218     C   91              GOTO      FOR02
041120141212     c                   endif
041130141212
041140141020     c                   exsr      sr_tiidc
041150141212
041160141020   x4c                   else
041170141212     c                   eval      wnoindir=' '
041180141020     C                   EXSR      REGIS2
041190141020    4c                   endif
041200141020   x3c                   else
041210141212     c                   eval      wnoindir=' '
041220930517     C                   EXSR      REGIS2
041230141020    3c                   endif
041240991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
041250991130     C     *IN91         IFEQ      *ON
041260991130     C     D48FFR        ANDEQ     'E'
041270991130     C                   MOVEL     VIDMSG        D48MSG
041280991130     C                   MOVEL     '2'           D48ERR
041290991130     C                   GOTO      ENDEM2
041300991130     C                   ENDIF
041310991130     C   91              GOTO      FOR02
041320941216     C*
041330941220   X2C                   ELSE
041340941216     C*
041350941216     C                   GOTO      FOR02
041360941220    2C                   ENDIF
041370941220     C*
041380950518    1C                   ENDIF
041390150112     c* Se non ho passato msg vincolanti, passo msg di avvertimento
041400150112     c*  se ce ne sono
041410150112     c                   if        msgforzatxt<>*blanks
041420150112     C                   MOVEL     msgforzatxt   D48MSG
041430150112     C                   MOVEL     'Z'           D48ERR
041440150112     c                   clear                   msgforzatxt
041450150112     C                   ENDIF
041460950518     C*
041470950518     C     ENDEM2        TAG
041480950518     C*
041490950518    1C     *INKL         IFEQ      *ON
041500950518     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
041510950519    1C     D48FFR        OREQ      'E'
041520950518     C     *IN90         ANDEQ     *ON
041530000303    1C     D48FFR        OREQ      'E'
041540000303     C     *IN91         ANDEQ     *ON
041550941220     C* F12-RITORNO
041560160119    2C     *IN10         IFEQ      *ON
041570941220     C                   UNLOCK    FNARB01L
041580051118     C                   UNLOCK    FiAR901L
041590160119     c                   endif
041600160119     C****               ELSE
041610941222     C                   UNLOCK    FNBLP01L
041620160119    2C****               END
041630991001     C                   UNLOCK    FIAR601L
041640950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
041650950607     C* flag D48F12 = 1
041660950607     C   KL              MOVEL     '1'           D48F12
041670950519    1C                   END
041680941215     C*
041690950518     C                   ENDSR
041700920121     C*
041710920121     C*--- EMISSIONE E GESTIONE FORMATO 3 ----------------------------*
041720920121     C     EMFOR3        BEGSR
041730990415     C*
041740990415     C                   MOVEL     ' '           COMASS
041750941102     C*
041760920121     C     FOR03         TAG
041770941102     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
041780950114     C     *IN90         IFEQ      *ON
041790941212     C     D48FFR        OREQ      'S'
041800950607     C     D48FFR        OREQ      'V'
041810941215     C                   WRITE     LR48T03
041820941212     C                   WRITE     LR48Z03
041830941212     C                   EXFMT     LR48C03
041840941220    1C                   END
041850941102     C*
041860941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
041870941209     C                   SETOFF                                       289052
041880941102     C                   SETOFF                                       404142
041890991006     C                   SETOFF                                       434445
041900941102     C                   SETOFF                                       464748
041910941209     C                   SETOFF                                       495051
041920941215     C                   SETOFF                                       989453
041930961112     C                   SETOFF                                       545556
041940990415     C                   SETOFF                                       82
041950941220     C*
041960941220     C* PER F12-RITORNO O F21 RITASSAZIONE, DISALLOCO I RECORD
041970941220     C* F12-RITORNO
041980941220    1C     *INKL         IFEQ      *ON
041990941220     C     *INKV         OREQ      *ON
042000941222     C*
042010941222     C     *IN10         IFEQ      *ON
042020941220     C                   UNLOCK    FNARB01L
042030051118     C                   UNLOCK    FiAR901L
042040941222     C                   ELSE
042050941222     C                   UNLOCK    FNBLP01L
042060941220    1C                   ENDIF
042070991005     C                   UNLOCK    FIAR601L
042080950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
042090950607     C* flag D48F12 = 1
042100950607     C   KL              MOVEL     '1'           D48F12
042110941222    1C                   ENDIF
042120941220     C* F12 - RITORNO
042130941220    1C  NKL              DO
042140941220     C*
042150941220     C* F21 - RITASSAZIONE BOLLA
042160941220    2C     *INKV         IFEQ      *ON
042170050511     C                   CLEAR                   Fnlra2DS
042180050511     C                   Z-ADD     ARBAAS        ilra2AAS
042190050511     C                   Z-ADD     ARBLNP        ilra2lNP
042200050511     C                   Z-ADD     ARBNRS        ilra2NRS
042210050511     C                   Z-ADD     ARBNSP        ilra2NSP
042220050511     C                   MOVEL     D48FGS        ilra2FGS
042230050511     C                   MOVEL     D48CVB        ilra2CVB
042240050511     C                   MOVEL     Fnlra2DS      KPJBU
042250050511     C                   CALL      'FNLRA2R'
042260941217     C                   PARM                    KPJBA
042270030718     C                   PARM                    TRUL90DS
042280050804     c* Se mi passa errore, lo passo dal lr48 al lr36
042290050804     c                   movel     kpjbu         fnlra2ds
042300050804     C                   MOVEL     olra2msg      D48MSG
042310050804     C                   MOVEL     olra2err      D48ERR
042320131119     c                   if        olra2nft>0
042330131119     C                   MOVEL     '1'           WTRC
042340131119     c     karb2         chain(n)  fiar6000
042350131119     c                   endif
042360131120     c* trasmetto variazione a PDA:
042370131120     c* se segue fattura --> solo se prima era una fattura immediata
042380131120     c* se fattura immediata --> solo se variato importo della fattura
042390131119     c                   if        *in10 and ((olra2nft=0  and ar6nft>0)
042400131120     c                              or (olra2nft>0 and (ar6ift<>sav_ift
042410131120     c                              or ar6div<>viddiv)))
042420130416     c                   eval      wpda_ndc=arbndc
042430130416     c                   eval      wpda_ddc=arbddc
042440130416     c                   eval      wpda_dcm=arbdcm
042450140120     c                   eval      arbdtv=olra2dtv
042460140120     c                   eval      arborv=olra2orv
042470140120     c                   eval      arbpru=olra2pru
042480140120     c                   eval      arbcvb=d48cvb
042490130416     c                   exsr      sr_pda
042500130416     c                   endif
042501170411     c* se passato da Fattura Immediata a Segue Fattura memorizzo i dati della vecchia fattura
042502170411     c                   if        olra2nft=0 and ar6nft>0
042503170411     c                   exsr      sr_fnfta
042504170411     c                   endif
042510920121     C*
042520941220   X2C                   ELSE
042530920121     C* CONTROLLO FORMATO
042540920121     C                   EXSR      CONTR3
042550941215     C*
042560941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
042570941220    3C     *IN90         IFEQ      *ON
042580941215     C     D48FFR        ANDEQ     'E'
042590941215     C                   MOVEL     VIDMSG        D48MSG
042600941215     C                   MOVEL     '3'           D48ERR
042610941215     C                   GOTO      ENDEM3
042620941220    3C                   ENDIF
042630920320     C*
042640950607     C  NKF
042650950607     CANNKI
042660950607     CANN26
042670990415     COR 82
042680000105     COR KN
042690920320     COR 90              GOTO      FOR03
042700060302     c* richiamo routine di richiesta/controllo motivazione
042710060302     c                   if        d66mtv = 'S' and w48mct = *blanks
042720060302     c                   exsr      ges_mtv
042730060302     c                   if        olv85f12 = '1'
042740060302     c                   if        ilv85wdw = ' '
042750060302     c                   goto      for03
042760060302     c                   else
042770060302     c                   eval      *inkl = '1'
042780060302     C                   GOTO      ENDEM3
042790060302     c                   endif
042800060302     c                   endif
042810060302     C     *IN90         IFEQ      *ON
042820060302     C     D48FFR        ANDEQ     'E'
042830060302     C                   MOVEL     VIDMSG        D48MSG
042840070308     C                   MOVEL     '3'           D48ERR
042850060302     C                   GOTO      ENDEM3
042860060302     C                   ENDIF
042870060302     c                   endif
042880920121     C*
042890920204     C* CMD6 - AGGIORNAMENTO PIU' EVENTUALE STAMPA
042900920204     C                   EXSR      REGIS3
042910991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
042920991130     C     *IN91         IFEQ      *ON
042930991130     C     D48FFR        ANDEQ     'E'
042940991130     C                   MOVEL     VIDMSG        D48MSG
042950991130     C                   MOVEL     '3'           D48ERR
042960991130     C                   GOTO      ENDEM3
042970991130     C                   ENDIF
042980920204     C   91              GOTO      FOR03
042990920204     C*
043000070309     c* ristampo fattura immediata se variata(37 on), se in arrivo
043010070309     c*  (10 on) e se non attiva GEO
043020130321    3C**   *IN37         IFEQ      *ON
043030130321    3C***  *IN10         ANDEQ     *ON
043040130321     c**   FGSddaSI      andeq     ' '
043050130321     C***                CLEAR                   FNLSB5DS
043060130321     C***                MOVEL     'V'           DB0RIS
043070130321     C***                MOVEL     D48TBO        DB0TBO
043080130321     C***                MOVEL     ARBAAS        DB0AAS
043090130321     C***                MOVEL     ARBLNP        DB0LNP
043100130321     C***                MOVEL     ARBNRS        DB0NRS
043110130321     C***                MOVEL     ARBNSP        DB0NSP
043120130321     C***                CALL      D90PSL
043130130321     C***                PARM                    FNLSB5DS
043140130321    3C***                END
043150941212     C*
043160941220    2C                   END
043170941220     C*
043180000303    1C                   END
043190000303     C     ENDEM3        TAG
043200000303     C* SE PGM RICHIAMATO E C'E' ERRORE DISALLOCO
043210000303    1C     D48FFR        IFEQ      'E'
043220000303     C     *IN90         ANDEQ     *ON
043230000303    1C     D48FFR        OREQ      'E'
043240000303     C     *IN91         ANDEQ     *ON
043250000303     C     *IN10         IFEQ      *ON
043260000303     C                   UNLOCK    FNARB01L
043270051118     C                   UNLOCK    FiAR901L
043280000303     C                   ELSE
043290000303     C                   UNLOCK    FNBLP01L
043300000303     C                   ENDIF
043310000303     C                   UNLOCK    FIAR601L
043320000303     C* Comunico al chiamante che è stato premuto F12 valorizzando il
043330000303     C* flag D48F12 = 1
043340000303     C   KL              MOVEL     '1'           D48F12
043350000303    1C                   ENDIF
043360920121     C*
043370000303     C                   ENDSR
043380941102     C*
043390941102     C* EMETTO FORMATO4 ----------------------------------------------*
043400941102     C     EMFOR4        BEGSR
043410941102     C*
043420941102     C     FOR04         TAG
043430941102     C*
043440950114     C     *IN90         IFEQ      *ON
043450941212     C     D48FFR        OREQ      'S'
043460950607     C     D48FFR        OREQ      'V'
043470941102     C                   WRITE     LR48T01
043480941102     C                   EXFMT     LR48D04
043490941102     C                   ENDIF
043500941209     C*
043510941209     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
043520941209     C                   SETOFF                                       289045
043530941209     C                   SETOFF                                       464748
043540991006     C                   SETOFF                                       495051
043550041130     c                   setoff                                       636465
043560041130     c                   setoff                                       6667
043570941220     C*
043580941220     C* PER F12 -RITORNO O F21-RITASSAZIONE SBLOCCO I RECORD
043590941220     C     *INKL         IFEQ      *ON
043600941220     C     *INKV         OREQ      *ON
043610941222     C*
043620941222     C     *IN10         IFEQ      *ON
043630941220     C                   UNLOCK    FNARB01L
043640051118     C                   UNLOCK    FiAR901L
043650941222     C                   ELSE
043660941222     C                   UNLOCK    FNBLP01L
043670941220     C                   ENDIF
043680991005     C                   UNLOCK    FIAR601L
043690950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
043700950607     C* flag D48F12 = 1
043710950607     C   KL              MOVEL     '1'           D48F12
043720941222     C                   ENDIF
043730941220     C* F12 - RITORNO
043740941102     C     *INKL         IFEQ      *OFF
043750050228     C* F21 - RITASSAZIONE BOLLA
043760941220     C     *INKV         IFEQ      *ON
043770050511     C                   CLEAR                   Fnlra2DS
043780050511     C                   Z-ADD     ARBAAS        ilra2AAS
043790050511     C                   Z-ADD     ARBLNP        ilra2LNP
043800050511     C                   Z-ADD     ARBNRS        ilra2NRS
043810050511     C                   Z-ADD     ARBNSP        ilra2NSP
043820050511     C                   MOVEL     D48FGS        ilra2FGS
043830050511     C                   MOVEL     D48CVB        ilra2CVB
043840050511     C                   MOVEL     Fnlra2DS      KPJBU
043850050511     C                   CALL      'FNLRA2R'
043860941102     C                   PARM                    KPJBA
043870030718     C                   PARM                    TRUL90DS
043880050804     c
043890050804     c* Se mi passa errore, lo passo dal lr48 al lr36
043900050804     c                   movel     kpjbu         fnlra2ds
043910050804     C                   MOVEL     olra2msg      D48MSG
043920050804     C                   MOVEL     olra2err      D48ERR
043930131119     c                   if        olra2nft>0
043940131119     C                   MOVEL     '2'           WTRC
043950131119     c     karb2         chain(n)  fiar6000
043960131119     c                   endif
043970131120     c* trasmetto variazione a PDA:
043980131120     c* se segue fattura --> solo se prima era una fattura immediata
043990131120     c* se fattura immediata --> solo se variato importo della fattura
044000131120     c                   if        *in10 and ((olra2nft=0  and ar6nft>0)
044010131120     c                              or (olra2nft>0 and (ar6ift<>sav_ift
044020131120     c                              or ar6div<>viddiv)))
044030130416     c                   eval      wpda_ndc=arbndc
044040130416     c                   eval      wpda_ddc=arbddc
044050130416     c                   eval      wpda_dcm=arbdcm
044060140120     c                   eval      arbdtv=olra2dtv
044070140120     c                   eval      arborv=olra2orv
044080140120     c                   eval      arbpru=olra2pru
044090140120     c                   eval      arbcvb=d48cvb
044100130416     c                   exsr      sr_pda
044110130416     c                   endif
044111170411     c* se passato da Fattura Immediata a Segue Fattura memorizzo i dati della vecchia fattura
044112170411     c                   if        olra2nft=0 and ar6nft>0
044113170411     c                   exsr      sr_fnfta
044114170411     c                   endif
044120941102     C*
044130941220     C                   ELSE
044140941102     C* CONTROLLO FORMATO
044150941102     C                   EXSR      CONTR4
044160941215     C*
044170941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044180941215     C     *IN90         IFEQ      *ON
044190941215     C     D48FFR        ANDEQ     'E'
044200941215     C                   MOVEL     VIDMSG        D48MSG
044210941215     C                   MOVEL     '4'           D48ERR
044220941215     C                   GOTO      ENDEM4
044230941215     C                   ENDIF
044240941102     C*
044250140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
044260140502     c*   (d48ffr='E' e 26 off)
044270140502     c  n90
044280140502     cann26              if        d48ffr='E'
044290140502     C                   GOTO      ENDEM4
044300140502     c                   endif
044310950607     C  NKF
044320950607     CANNKI
044330950607     CANN26
044340941102     COR 90              GOTO      FOR04
044350060302     c* richiamo routine di richiesta/controllo motivazione
044360060302     c                   if        d66mtv = 'S' and w48mct = *blanks
044370060302     c                   exsr      ges_mtv
044380060302     c                   if        olv85f12 = '1'
044390060302     c                   if        ilv85wdw = ' '
044400060302     c                   goto      for04
044410060302     c                   else
044420060302     c                   eval      *inkl = '1'
044430060302     C                   GOTO      ENDEM4
044440060302     c                   endif
044450060302     c                   endif
044460060302     C     *IN90         IFEQ      *ON
044470060302     C     D48FFR        ANDEQ     'E'
044480060302     C                   MOVEL     VIDMSG        D48MSG
044490070308     C                   MOVEL     '4'           D48ERR
044500060302     C                   GOTO      ENDEM4
044510060302     C                   ENDIF
044520060302     c                   endif
044530941102     C*
044540941102     C* CMD6 - AGGIORNAMENTO PIU' EVENTUALE STAMPA
044550941102     C                   EXSR      REGIS4
044560991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044570991130     C     *IN91         IFEQ      *ON
044580991130     C     D48FFR        ANDEQ     'E'
044590991130     C                   MOVEL     VIDMSG        D48MSG
044600991130     C                   MOVEL     '4'           D48ERR
044610991130     C                   GOTO      ENDEM4
044620991130     C                   ENDIF
044630941102     C   91              GOTO      FOR04
044640941102     C                   END
044650941102     C                   END
044660000303     C*
044670000303     C     ENDEM4        TAG
044680000303    1C     D48FFR        IFEQ      'E'
044690000303     C     *IN90         ANDEQ     *ON
044700000303    1C     D48FFR        OREQ      'E'
044710000303     C     *IN91         ANDEQ     *ON
044720000303     C     *IN10         IFEQ      *ON
044730000303     C                   UNLOCK    FNARB01L
044740051118     C                   UNLOCK    FiAR901L
044750000303     C                   ELSE
044760000303     C                   UNLOCK    FNBLP01L
044770000303     C                   ENDIF
044780000303     C                   UNLOCK    FIAR601L
044790000303     C* Comunico al chiamante che è stato premuto F12 valorizzando il
044800000303     C* flag D48F12 = 1
044810000303     C   KL              MOVEL     '1'           D48F12
044820000303    1C                   ENDIF
044830941102     C*
044840000303     C                   ENDSR
044850941102     C*
044860941102     C* EMETTO E GESTISCO FORMATO5 -----------------------------------*
044870941102     C     EMFOR5        BEGSR
044880950407     C*
044890941102     C     FOR05         TAG
044900941102     C*
044910950114     C     *IN90         IFEQ      *ON
044920941212     C     D48FFR        OREQ      'S'
044930950607     C     D48FFR        OREQ      'V'
044940941102     C                   WRITE     LR48T01
044950941102     C                   EXFMT     LR48D05
044960941102     C                   END
044970941102     C*
044980941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
044990941102     C                   SETOFF                                       289096
045000941102     C                   SETOFF                                       404142
045010991001     C                   SETOFF                                       434492
045020070329     C* F12 - RITORNO
045030941220    1C     *INKL         IFEQ      *OFF
045040070329     c
045050070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
045060070329     c                   if        *in87 and *inkr
045070070329     c                   exsr      CALLNOTELDV
045080070329     c                   goto      for05
045090070329     c                   endif
045100941102     C*
045110941102     C* CONTROLLO FORMATO
045120941213     C                   EXSR      CONTR5
045130941215     C*
045140941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045150941220    2C     *IN90         IFEQ      *ON
045160941215     C     D48FFR        ANDEQ     'E'
045170941215     C                   MOVEL     VIDMSG        D48MSG
045180941215     C                   MOVEL     '5'           D48ERR
045190941215     C                   GOTO      ENDEM5
045200941220    2C                   ENDIF
045210140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
045220140502     c*   (d48ffr='E' e 26 off)
045230140502     c  n90
045240140502     cann26              if        d48ffr='E'
045250140502     C                   GOTO      ENDEM5
045260140502     c                   endif
045270140502     c* Se errore forzabile ma pgm richiamato in modalità 'E' o 'F' cioè
045280140502     c* senza emissione di videata ignoro l'errore forzabile e permetto
045290140502     c* l'aggiornamento
045300140502     c   92              if        d48ffr='E'
045310140502     c                   setoff                                       92
045320140502     c                   endif
045330950607     C  NKF
045340950607     CANN26
045350950407     COR 90
045360950407     COR 92              GOTO      FOR05
045370060302     c* richiamo routine di richiesta/controllo motivazione
045380060302     c                   if        d66mtv = 'S' and w48mct = *blanks
045390060302     c                   exsr      ges_mtv
045400060302     c                   if        olv85f12 = '1'
045410060302     c                   if        ilv85wdw = ' '
045420060302     c                   goto      for05
045430060302     c                   else
045440060302     c                   eval      *inkl = '1'
045450060302     C                   GOTO      ENDEM5
045460060302     c                   endif
045470060302     c                   endif
045480060302     C     *IN90         IFEQ      *ON
045490060302     C     D48FFR        ANDEQ     'E'
045500060302     C                   MOVEL     VIDMSG        D48MSG
045510070308     C                   MOVEL     '5'           D48ERR
045520060302     C                   GOTO      ENDEM5
045530060302     C                   ENDIF
045540060302     c                   endif
045550941102     C*
045560941220     C                   EXSR      REGIS5
045570991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045580991130     C     *IN91         IFEQ      *ON
045590991130     C     D48FFR        ANDEQ     'E'
045600991130     C                   MOVEL     VIDMSG        D48MSG
045610991130     C                   MOVEL     '5'           D48ERR
045620991130     C                   GOTO      ENDEM5
045630991130     C                   ENDIF
045640941213     C   91              GOTO      FOR05
045650941220     C*
045660950518     C                   ENDIF
045670941222     C*
045680950518     C     ENDEM5        TAG
045690950518     C* F12-RITORNO  --> DISALLOCO RECROD
045700950518    1C     *INKL         IFEQ      *ON
045710950518     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045720950518     C     D48FFR        OREQ      'E'
045730950518     C     *IN90         ANDEQ     *ON
045740000303     C     D48FFR        OREQ      'E'
045750000303     C     *IN91         ANDEQ     *ON
045760950518     C*
045770950518    2C     *IN10         IFEQ      *ON
045780941220     C                   UNLOCK    FNARB01L
045790941222     C                   ELSE
045800941222     C                   UNLOCK    FNBLP01L
045810950518    2C                   END
045820051118     C                   UNLOCK    Fiar901L
045830991001     C                   UNLOCK    FIAR601L
045840950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
045850950607     C* flag D48F12 = 1
045860950607     C   KL              MOVEL     '1'           D48F12
045870941222    1C                   END
045880941102     C*
045890950518     C                   ENDSR
045900941102     C*
045910941102     C* EMISSIONE E GESTIONE FORMATO 7 ------------------------------*
045920941102     C     EMFOR7        BEGSR
045930941102     C*
045940941102     C     FOR07         TAG
045950941102     C*
045960950114     C     *IN90         IFEQ      *ON
045970941212     C     D48FFR        OREQ      'S'
045980950607     C     D48FFR        OREQ      'V'
045990941102     C                   WRITE     LR48T01
046000941102     C                   EXFMT     LR48D07
046010941102     C                   END
046020941102     C*
046030941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
046040941102     C                   SETOFF                                       2890
046050941102     C                   SETOFF                                       404142
046060941102     C                   SETOFF                                       434445
046070031001     c                   Setoff                                       62
046080070329     C* F12 - RITORNO
046090941102     C     *INKL         IFEQ      *OFF
046100070329     c
046110070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
046120070329     c                   if        *in87 and *inkr
046130070329     c                   exsr      CALLNOTELDV
046140070329     c                   goto      for07
046150070329     c                   endif
046160941102     C*
046170941102     C* CONTROLLO FORMATO
046180941102     C                   EXSR      CONTR7
046190040219      * questi controlli li devo saltare se il richiamo è stato fatto con causale 'CD'
046200040219      * cioè da chiusura distina
046210040224      * o causale 'CA' creata da LR48 in automatico
046220040224     c                   If        Not *In90 and D48Cvb <> 'CD' and
046230170406     c                             d48cvb <> 'CA'
046240040219     C                   ExSr      Contr72
046250040219     c                   EndIf
046260941215     C*
046270941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
046280070309     c*          non se è forzabile solo allafine dopo la REGIS
046290941215     C     *IN90         IFEQ      *ON
046300941215     C     D48FFR        ANDEQ     'E'
046310070330     c**** Msgforza      andeq     0
046320941215     C                   MOVEL     VIDMSG        D48MSG
046330941215     C                   MOVEL     '7'           D48ERR
046340941215     C                   GOTO      ENDEM7
046350941215     C                   ENDIF
046360070330     c
046370070330     c* Se non ho avuto errori e richiamo solo per controlli, esco
046380070330     c*   (d48ffr='E' e 26 off)
046390070329     c  n90
046400070330     cann26              if        d48ffr='E'
046410070329     C                   GOTO      ENDEM7
046420070329     c                   endif
046430950607     C  NKF
046440950607     CANN26
046450941102     COR 90              GOTO      FOR07
046460060301     c* richiamo routine di richiesta/controllo motivazione
046470060301     c                   if        d66mtv = 'S' and w48mct = *blanks
046480060301     c                   exsr      ges_mtv
046490060301     c* premuto f12-Ritorno
046500060301     c                   if        olv85f12 = '1'
046510060301     c                   if        ilv85wdw = ' '
046520060301     c                   goto      for07
046530060301     c                   else
046540060301     c                   eval      *inkl = '1'
046550060301     C                   GOTO      ENDEM7
046560060301     c                   endif
046570060301     c                   endif
046580060301     c* Errore da ritornare a chiamante
046590060301     C     *IN90         IFEQ      *ON
046600060301     C     D48FFR        ANDEQ     'E'
046610060301     C                   MOVEL     VIDMSG        D48MSG
046620060301     C                   MOVEL     '7'           D48ERR
046630060301     C                   GOTO      ENDEM7
046640060301     C                   ENDIF
046650060301     c                   endif
046660941102     C*
046670070307     C* F6 - AGGIORNAMENTO
046680070307     c* Se devo scrivere file proposte, aggiorno solo FIARP
046690070307     c                   if        AggioARP='S'
046700070307     c                   EXSR      REGISARP
046710070307     c                   endif
046720070308     c
046730070308     c* Aggiorno comunque per gg chiusura e consegne particolari, se
046740070308     c*  modificate, in caso di scrittura di FIARP
046750070308     c                   if        AggioARP=' ' or
046760070308     c                             arbgc1<>vidgc1  or arbgc2<>vidgc2 or
046770151230     c                             arbtc1<>vidtc1 or arbtc2<>vidtc2
046780941102     C                   EXSR      REGIS7
046790070309     c                   else
046800160119     C   10              UNLOCK    FIAR601L
046810160119     C   10              UNLOCK    Fnarb01l
046820160119     C   10              UNLOCK    FiAR901L
046830160119     C                   UNLOCK    Fnblp01l
046840070308     c                   endif
046850070308     c
046860991130     C     *IN91         IFEQ      *ON
046870991130     C     D48FFR        ANDEQ     'E'
046880991130     C                   MOVEL     VIDMSG        D48MSG
046890991130     C                   MOVEL     '7'           D48ERR
046900991130     C                   GOTO      ENDEM7
046910991130     C                   ENDIF
046920991130     C   91              GOTO      FOR07
046930950519     C                   ENDIF
046940070309     c
046950070309     c* Se non ho passato msg vincolanti, passo msg di avvertimento
046960070309     c*  se ce ne sono
046970070309     c                   if        msgforza>0
046980070309     c                   z-aDD     MSGFORZA      XX
046990070309     C                   MOVEL     MSG(XX)       D48MSG
047000070309     C                   MOVEL     'Z'           D48ERR
047010070309     c                   clear                   msgforza
047020070309     C                   ENDIF
047030950519     C*
047040950519     C     ENDEM7        TAG
047050070309     C* F12-RITORNO  --> DISALLOCO REcord
047060941222     C*
047070950519    1C     *INKL         IFEQ      *ON
047080950519     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
047090950519     C     D48FFR        OREQ      'E'
047100950519     C     *IN90         ANDEQ     *ON
047110000303     C     D48FFR        OREQ      'E'
047120000303     C     *IN91         ANDEQ     *ON
047130070329     C     D48FFR        OREQ      'E'
047140070329     c     *in26         andeq     *off
047150950519     C*
047160950519    2C     *IN10         IFEQ      *ON
047170941220     C                   UNLOCK    FNARB01L
047180991005     C                   UNLOCK    FIAR601L
047190051118     C                   UNLOCK    FiAR901L
047200160119     c                   endif
047210160119     C***                ELSE
047220941222     C                   UNLOCK    FNBLP01L
047230160119    2C***                END
047240950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
047250950607     C* flag D48F12 = 1
047260950607     C   KL              MOVEL     '1'           D48F12
047270950519    1C                   END
047280941102     C*
047290950519     C                   ENDSR
047300021129      **
047310021129      * EMISSIONE E GESTIONE FORMATO 7/1 Supermercati ---------------*
047320021129     c     Emfor71       BegSr
047330021129
047340021129     c     For071        Tag
047350021129
047360021129     c                   If        *In90 or D48Ffr = 'S' or D48Ffr = 'V'
047370021129     c                   Write     Lr48T01
047380021129     c                   Exfmt     Lr48D071
047390021129     c                   EndIf
047400021129
047410021129      * Spengo idnicatori di posizionamento cursore
047420021129     c                   Setoff                                       2890
047430021129     c                   Setoff                                       404142
047440021129     c                   Setoff                                       434445
047450021129     c                   Setoff                                       575859
047460031001     c                   Setoff                                       62
047470070329      * F12 - RITORNO
047480021129     c                   IF        Not *InKl
047490070329     c
047500070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
047510070329     c                   if        *in87 and *inkr
047520070329     c                   exsr      CALLNOTELDV
047530070329     c                   goto      for071
047540070329     c                   endif
047550021129
047560021129      * CONTROLLO FORMATO
047570021129     c                   ExSr      Contr7
047580021129
047590021129     c  n90              ExSr      Contr71
047600031028     C  n90              ExSr      Contr72
047610070306     c
047620070306     c* Nel caso dei supermercati devo sempre aggiornare la bolla
047630070306     c*  e mai il file di proposta FIARP per sicurezza lo pulisco
047640070306     c                   clear                   aggioarp
047650070308     c                   clear                   savdcr
047660070308     c                   clear                   savTcr
047670070308     c                   clear                   savHcr
047680070306     c
047690040330      * La data non può superare i gg limiti indicati in tab. VPO
047700040330     c                   If        Not *In90
047710040330     c                   Eval      wdataiso = wdataoggi
047720040330     c                   Adddur    §vpodcr:*d    wdataiso
047730040330     c     *iso          move      wdataiso      ds_data
047740040330     c                   Move      §vpodcr       w003a
047750040330     c                   Movea     w003a         ski(13)
047760040330     c                   Z-add     13            i
047770040330     c                   ExSr      abbl
047780040330     c                   Movea     ski(13)       w003a
047790040330     c                   If        ComDcr > ds_data
047800040330     c                   Eval      *In28 = *On
047810040330     c                   Eval      *In40 = *On
047820040330     c                   Eval      *In90 = *On
047830040330     c                   Eval      VidMsg = msg(129)
047840040330     c                   Eval      VidMsg = %replace(w003a:VidMsg:
047850040330     c                                      %scan('___':VidMsg))
047860040330     c                   EndIf
047870040330     c                   EndIf
047880021129
047890021129      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
047900021129     c                   If        *In90 and D48Ffr = 'E'
047910021129     c                   Eval      D48Msg = VidMsg
047920021129     c                   Eval      D48Err = '7'
047930021129     c                   GoTo      EndEm71
047940021129     c                   EndIf
047950140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
047960140502     c*   (d48ffr='E' e 26 off)
047970140502     c  n90
047980140502     cann26              if        d48ffr='E'
047990140502     C                   GOTO      ENDEM71
048000140502     c                   endif
048010021129
048020021129     c  NKF
048030021129     cANN26
048040021129     cOR 90              GoTo      For071
048050060302     c* richiamo routine di richiesta/controllo motivazione
048060060302     c                   if        d66mtv = 'S' and w48mct = *blanks
048070060302     c                   exsr      ges_mtv
048080060302     c                   if        olv85f12 = '1'
048090060302     c                   if        ilv85wdw = ' '
048100060302     c                   goto      for071
048110060302     c                   else
048120060302     c                   eval      *inkl = '1'
048130060302     C                   GOTO      ENDEM71
048140060302     c                   endif
048150060302     c                   endif
048160060302     C     *IN90         IFEQ      *ON
048170060302     C     D48FFR        ANDEQ     'E'
048180060302     C                   MOVEL     VIDMSG        D48MSG
048190070308     C                   MOVEL     '7'           D48ERR
048200060302     C                   GOTO      ENDEM71
048210060302     C                   ENDIF
048220060302     c                   endif
048230021129
048240021129      * CMD6 - AGGIORNAMENTO
048250021129     c                   ExSr      Regis7
048260021129
048270021129     c  n91              ExSr      Regis71
048280021129
048290021129     c                   If        *In91 and D48Ffr = 'E'
048300021129     c                   Eval      D48Msg = VidMsg
048310021129     c                   Eval      D48Err = '7'
048320021129     c                   GoTo      EndEm71
048330021129     c                   EndIf
048340021129
048350021129     c   91              GoTo      For071
048360021129
048370021129     c                   EndIf
048380021129
048390021129     c     EndEm71       Tag
048400021129
048410021129      * F12-RITORNO  --> DISALLOCO RECROD
048420021129
048430021129    1c     *INKL         IFEQ      *ON
048440021129      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048450021129     c     D48FFR        OREQ      'E'
048460021129     c     *IN90         ANDEQ     *ON
048470021129     c     D48FFR        OREQ      'E'
048480021129     c     *IN91         ANDEQ     *ON
048490021129
048500021129    2c     *IN10         IFEQ      *ON
048510021129     c                   UNLOCK    FNARB01L
048520021129     c                   UNLOCK    FIAR601L
048530051118     c                   UNLOCK    FiAR901L
048540021129     c                   ELSE
048550021129     c                   UNLOCK    FNBLP01L
048560021129    2c                   END
048570021129      * Comunico al chiamante che è stato premuto F12 valorizzando il
048580021129      * flag D48F12 = 1
048590021129     c   KL              MOVEL     '1'           D48F12
048600021129    1c                   END
048610021129
048620021129     c                   EndSr
048630941223     C*
048640941223     C* EMISSIONE E GESTIONE FORMATO 8 ------------------------------*
048650941223     C     EMFOR8        BEGSR
048660941223     C*
048670941223     C     FOR08         TAG
048680941223     C*
048690950114     C     *IN90         IFEQ      *ON
048700941223     C     D48FFR        OREQ      'S'
048710950607     C     D48FFR        OREQ      'V'
048720941223     C                   WRITE     LR48T03
048730941223     C                   EXFMT     LR48D08
048740941223     C                   END
048750941223     C*
048760941223     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
048770941223     C                   SETOFF                                       2890
048780941223     C                   SETOFF                                       404142
048790941223     C                   SETOFF                                       434445
048800941223     C                   SETOFF                                       464748
048810941223     C                   SETOFF                                       4950
048820941223     C* CMD12 - RITORNO
048830941223     C     *INKL         IFEQ      *OFF
048840941223     C*
048850941223     C* CONTROLLO FORMATO
048860941223     C                   EXSR      CONTR8
048870941223     C*
048880941223     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048890941223     C     *IN90         IFEQ      *ON
048900941223     C     D48FFR        ANDEQ     'E'
048910941223     C                   MOVEL     VIDMSG        D48MSG
048920941223     C                   MOVEL     '8'           D48ERR
048930941223     C                   GOTO      ENDEM8
048940941223     C                   ENDIF
048950140502
048960140502     c* Non gestita uscita per d48ffr='F' ( n90 and n26 d48ffr='E')
048970140502     c* perchè bisognerebbe verificare bene cosa fare in questo caso
048980140502
048990970110     C  NKF
049000970110     CANNKJ
049010970110     CANN26
049020941223     COR 90              GOTO      FOR08
049030060302     c* richiamo routine di richiesta/controllo motivazione
049040060302     c                   if        d66mtv = 'S' and w48mct = *blanks
049050060302     c                   exsr      ges_mtv
049060060302     c                   if        olv85f12 = '1'
049070060302     c                   if        ilv85wdw = ' '
049080060302     c                   goto      for08
049090060302     c                   else
049100060302     c                   eval      *inkl = '1'
049110060302     C                   GOTO      ENDEM8
049120060302     c                   endif
049130060302     c                   endif
049140060302     C     *IN90         IFEQ      *ON
049150060302     C     D48FFR        ANDEQ     'E'
049160060302     C                   MOVEL     VIDMSG        D48MSG
049170070308     C                   MOVEL     '8'           D48ERR
049180060302     C                   GOTO      ENDEM8
049190060302     C                   ENDIF
049200060302     c                   endif
049210941223     C*
049220970110     C* F6 O F10 - AGGIORNAMENTO
049230941223     C                   EXSR      REGIS8
049240991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049250991130     C     *IN91         IFEQ      *ON
049260991130     C     D48FFR        ANDEQ     'E'
049270991130     C                   MOVEL     VIDMSG        D48MSG
049280991130     C                   MOVEL     '8'           D48ERR
049290991130     C                   GOTO      ENDEM8
049300991130     C                   ENDIF
049310941223     C   91              GOTO      FOR08
049320941223     C*
049330950519     C                   ENDIF
049340150112     c* Se non ho passato msg vincolanti, passo msg di avvertimento
049350150112     c*  se ce ne sono
049360150112     c                   if        msgforzatxt<>*blanks
049370150112     C                   MOVEL     msgforzatxt   D48MSG
049380150112     C                   MOVEL     'Z'           D48ERR
049390150112     c                   clear                   msgforzatxt
049400150112     C                   ENDIF
049410950519     C*
049420950519     C     ENDEM8        TAG
049430950104     C* F12-RITORNO  --> DISALLOCO RECORD
049440950519    1C     *INKL         IFEQ      *ON
049450950519     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049460950519     C     D48FFR        OREQ      'E'
049470950519     C     *IN90         ANDEQ     *ON
049480000303     C     D48FFR        OREQ      'E'
049490000303     C     *IN91         ANDEQ     *ON
049500941223     C*
049510941223     C     *IN10         IFEQ      *ON
049520941223     C                   UNLOCK    FNARB01L
049530991005     C                   UNLOCK    FIAR601L
049540051118     C                   UNLOCK    FiAR901L
049550941223     C                   ELSE
049560941223     C                   UNLOCK    FNBLP01L
049570941223     C                   END
049580950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
049590950607     C* flag D48F12 = 1
049600950607     C   KL              MOVEL     '1'           D48F12
049610941223     C                   END
049620941223     C*
049630950519     C                   ENDSR
049640961211     C* EMISSIONE E GESTIONE FORMATO 9 ------------------------------*
049650961211     C     EMFOR9        BEGSR
049660920120     C*
049670961218     C     FOR09         TAG
049680961218     C*
049690961218     C     *IN90         IFEQ      *ON
049700961218     C     D48FFR        OREQ      'S'
049710961218     C     D48FFR        OREQ      'V'
049720961219     C                   WRITE     LR48T01
049730961218     C                   EXFMT     LR48D09
049740961218     C                   END
049750961218     C*
049760961218     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
049770961218     C                   SETOFF                                       2890
049780970110     C                   SETOFF                                       4041
049790070329     C* F12 - RITORNO
049800961218     C     *INKL         IFEQ      *OFF
049810070329     c
049820070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
049830070329     c                   if        *in87 and *inkr
049840070329     c                   exsr      CALLNOTELDV
049850070329     c                   goto      for09
049860070329     c                   endif
049870070329     C*
049880961218     C*
049890961218     C* CONTROLLO FORMATO
049900961218     C                   EXSR      CONTR9
049910961218     C*
049920961218     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049930961218     C     *IN90         IFEQ      *ON
049940961218     C     D48FFR        ANDEQ     'E'
049950961218     C                   MOVEL     VIDMSG        D48MSG
049960961218     C                   MOVEL     '9'           D48ERR
049970961218     C                   GOTO      ENDEM9
049980961218     C                   ENDIF
049990140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
050000140502     c*   (d48ffr='E' e 26 off)
050010140502     c  n90
050020140502     cann26              if        d48ffr='E'
050030140502     C                   GOTO      ENDEM9
050040140502     c                   endif
050050970110     C  NKF
050060970110     CANN26
050070961218     COR 90              GOTO      FOR09
050080060302     c* richiamo routine di richiesta/controllo motivazione
050090060302     c                   if        d66mtv = 'S' and w48mct = *blanks
050100060302     c                   exsr      ges_mtv
050110060302     c                   if        olv85f12 = '1'
050120060302     c                   if        ilv85wdw = ' '
050130060302     c                   goto      for09
050140060302     c                   else
050150060302     c                   eval      *inkl = '1'
050160060302     C                   GOTO      ENDEM9
050170060302     c                   endif
050180060302     c                   endif
050190060302     C     *IN90         IFEQ      *ON
050200060302     C     D48FFR        ANDEQ     'E'
050210060302     C                   MOVEL     VIDMSG        D48MSG
050220060316     C                   MOVEL     '9'           D48ERR
050230060302     C                   GOTO      ENDEM9
050240060302     C                   ENDIF
050250060302     c                   endif
050260060316     c                   exsr      regis9
050270060316     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050280060316     C     *IN91         IFEQ      *ON
050290060316     C     D48FFR        ANDEQ     'E'
050300060316     C                   MOVEL     VIDMSG        D48MSG
050310060316     C                   MOVEL     '9'           D48ERR
050320060316     C                   GOTO      ENDEM9
050330060316     C                   ENDIF
050340060316     C   91              GOTO      FOR09
050350041028     c
050360961218     C*
050370961218     C                   ENDIF
050380961218     C*
050390961218     C     ENDEM9        TAG
050400961218     C* F12-RITORNO  --> DISALLOCO RECORD
050410961218    1C     *INKL         IFEQ      *ON
050420961218     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050430961218     C     D48FFR        OREQ      'E'
050440961218     C     *IN90         ANDEQ     *ON
050450961218     C*
050460961218     C     *IN10         IFEQ      *ON
050470961218     C                   UNLOCK    FNARB01L
050480991005     C                   UNLOCK    FIAR601L
050490051118     C                   UNLOCK    FiAR901L
050500961218     C                   ELSE
050510961218     C                   UNLOCK    FNBLP01L
050520961218     C                   END
050530961218     C* Comunico al chiamante che è stato premuto F12 valorizzando il
050540961218     C* flag D48F12 = 1
050550961218     C   KL              MOVEL     '1'           D48F12
050560961218     C                   END
050570961211     C*
050580961211     C                   ENDSR
050590030123      **
050600030123      * EMISSIONE E GESTIONE FORMATO 10 Mancato ritiro BANCALI ------*
050610030123     c     Emfor10       BegSr
050620030123
050630030123     c     For10         Tag
050640030123
050650030123     c                   If        *In90 or D48Ffr = 'S' or D48Ffr = 'V'
050660030123     c                   Write     Lr48T01
050670030123     c                   Exfmt     Lr48D10
050680030123     c                   EndIf
050690030123
050700030123      * Spengo indicatori di posizionamento cursore
050710030123     c                   Setoff                                       2890
050720030124     c                   Setoff                                       5759
050730030124     c                   Setoff                                       6061
050740070329      * F12 - RITORNO
050750030123     c                   IF        Not *InKl
050760070329     c
050770070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
050780070329     c                   if        *in87 and *inkr
050790070329     c                   exsr      CALLNOTELDV
050800070329     c                   goto      for10
050810070329     c                   endif
050820030123
050830030123      * CONTROLLO FORMATO
050840030123     c                   ExSr      Contr10
050850030123
050860030123      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050870030123     c                   If        *In90 and D48Ffr = 'E'
050880030123     c                   Eval      D48Msg = VidMsg
050890030123     c                   Eval      D48Err = 'B'
050900030123     c                   GoTo      EndEm10
050910030123     c                   EndIf
050920140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
050930140502     c*   (d48ffr='E' e 26 off)
050940140502     c  n90
050950140502     cann26              if        d48ffr='E'
050960140502     C                   GOTO      ENDEM10
050970140502     c                   endif
050980030123
050990030123     c  NKF
051000030123     cANN26
051010030123     cOR 90              GoTo      For10
051020060302     c* richiamo routine di richiesta/controllo motivazione
051030060302     c                   if        d66mtv = 'S' and w48mct = *blanks
051040060302     c                   exsr      ges_mtv
051050060302     c                   if        olv85f12 = '1'
051060060302     c                   if        ilv85wdw = ' '
051070060302     c                   goto      for10
051080060302     c                   else
051090060302     c                   eval      *inkl = '1'
051100060302     C                   GOTO      ENDEM10
051110060302     c                   endif
051120060302     c                   endif
051130060302     C     *IN90         IFEQ      *ON
051140060302     C     D48FFR        ANDEQ     'E'
051150060302     C                   MOVEL     VIDMSG        D48MSG
051160070308     C                   MOVEL     'B'           D48ERR
051170060302     C                   GOTO      ENDEM10
051180060302     C                   ENDIF
051190060302     c                   endif
051200030123
051210030123      * CMD6 - AGGIORNAMENTO
051220030123     c                   ExSr      Regis10
051230030123
051240030123     c                   If        *In91 and D48Ffr = 'E'
051250030123     c                   Eval      D48Msg = VidMsg
051260030123     c                   Eval      D48Err = 'B'
051270030123     c                   GoTo      EndEm10
051280030123     c                   EndIf
051290030123
051300030123     c   91              GoTo      For10
051310030123
051320030123     c                   EndIf
051330030123
051340030123     c     EndEm10       Tag
051350030123
051360030123      * F12-RITORNO  --> DISALLOCO RECROD
051370030123
051380030123    1c     *INKL         IFEQ      *ON
051390030123      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051400030123     c     D48FFR        OREQ      'E'
051410030123     c     *IN90         ANDEQ     *ON
051420030123     c     D48FFR        OREQ      'E'
051430030123     c     *IN91         ANDEQ     *ON
051440030123
051450030123    2c     *IN10         IFEQ      *ON
051460030123     c                   UNLOCK    FNARB01L
051470030123     c                   UNLOCK    FIAR601L
051480051118     c                   UNLOCK    FiAR901L
051490030124     c                   UNLOCK    Fiar501L
051500030123     c                   ELSE
051510030123     c                   UNLOCK    FNBLP01L
051520030123    2c                   END
051530030123      * Comunico al chiamante che è stato premuto F12 valorizzando il
051540030123      * flag D48F12 = 1
051550030123     c   KL              MOVEL     '1'           D48F12
051560030123    1c                   END
051570030123
051580030123     c                   EndSr
051590061107     C
051600061107     C*--- EMISSIONE E GESTIONE FORMATO 11 ---------------------------*
051610061107     C     EMFOR11       BEGSR
051620061107     C*
051630061107     C     FOR11         TAG
051640061107     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
051650061107    1C     *IN90         IFEQ      *ON
051660061107     C     D48FFR        OREQ      'S'
051670061107     C     D48FFR        OREQ      'V'
051680061107     C                   WRITE     LR48T01
051690061107     C                   EXFMT     LR48D11
051700061107    1C                   END
051710061107     C*
051720061107     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
051730061107     C                   SETOFF                                       2890
051740061107     C                   SETOFF                                       4041
051750061107     C* F12 - RITORNO
051760061107    1C     *INKL         IFEQ      *OFF
051770061107     C*
051780061107     C* CONTROLLO FORMATO
051790061107     C                   EXSR      CONTR11
051800061107     C*
051810061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051820061107    3C     *IN90         IFEQ      *ON
051830061107     C     D48FFR        ANDEQ     'E'
051840061107     C                   MOVEL     VIDMSG        D48MSG
051850061107     C                   MOVEL     'F'           D48ERR
051860061107     C                   GOTO      ENDEM11
051870061107    3C                   ENDIF
051880071025     c
051890071025     c* Se non ho avuto errori e richiamo solo per controlli, esco
051900071025     c*   (d48ffr='E' e 26 off)
051910071025     c  n90
051920071025     cann26              if        d48ffr='E'
051930071025     C                   GOTO      ENDEM11
051940071025     c                   endif
051950061107     C  NKF
051960061107     CANN26
051970061107     COR 90              GOTO      FOR11
051980061107     c* richiamo routine di richiesta/controllo motivazione
051990061107     c                   if        d66mtv = 'S' and w48mct = *blanks
052000061107     c                   exsr      ges_mtv
052010061107     c                   if        olv85f12 = '1'
052020061107     c                   if        ilv85wdw = ' '
052030061107     c                   goto      for11
052040061107     c                   else
052050061107     c                   eval      *inkl = '1'
052060061107     C                   GOTO      ENDEM11
052070061107     c                   endif
052080061107     c                   endif
052090061107     C     *IN90         IFEQ      *ON
052100061107     C     D48FFR        ANDEQ     'E'
052110061107     C                   MOVEL     VIDMSG        D48MSG
052120061107     C                   MOVEL     'F'           D48ERR
052130061107     C                   GOTO      ENDEM11
052140061107     C                   ENDIF
052150061107     c                   endif
052160061107     C*
052170071026     c                   clear                   Aggiobol
052180061107     C                   EXSR      REGIS11
052190061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052200061107     C     *IN91         IFEQ      *ON
052210061107     C     D48FFR        ANDEQ     'E'
052220061107     C                   MOVEL     VIDMSG        D48MSG
052230061107     C                   MOVEL     'F'           D48ERR
052240061107     C                   GOTO      ENDEM11
052250061107     C                   ENDIF
052260061107     C   91              GOTO      FOR11
052270061107     C*
052280061107    1C                   ENDIF
052290061107     C*
052300061107     C     ENDEM11       TAG
052310061107     C*
052320061107    1C     *INKL         IFEQ      *ON
052330061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052340061107    1C     D48FFR        OREQ      'E'
052350061107     C     *IN90         ANDEQ     *ON
052360061107    1C     D48FFR        OREQ      'E'
052370061107     C     *IN91         ANDEQ     *ON
052380061107     C* F12-RITORNO
052390061107    2C     *IN10         IFEQ      *ON
052400061107     C                   UNLOCK    FNARB01L
052410061107     C                   UNLOCK    FiAR901L
052420061107     C                   ELSE
052430061107     C                   UNLOCK    FNBLP01L
052440061107    2C                   END
052450061107     C* Comunico al chiamante che è stato premuto F12 valorizzando il
052460061107     C* flag D48F12 = 1
052470061107     C   KL              MOVEL     '1'           D48F12
052480061107    1C                   END
052490061107     c
052500061107     C                   UNLOCK    Fiar601l
052510061107     C*
052520061107     C                   ENDSR
052530130927     c
052540130927     C*--- EMISSIONE E GESTIONE FORMATO 12  variaz.'"Nx' -------------*
052550130927     C     EMFOR12       BEGSR
052560130927     C*
052570130927     C     FOR12         TAG
052580130927     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
052590130927    1C     *IN90         IFEQ      *ON
052600130927     C     D48FFR        OREQ      'S'
052610130927     C     D48FFR        OREQ      'V'
052620130927     C                   WRITE     LR48T01
052630130927     C                   EXFMT     LR48D12
052640130927    1C                   END
052650130927     C*
052660130927     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
052670130927     C                   SETOFF                                       2890
052680130930     C                   SETOFF                                       41
052690130927     C* F12 - RITORNO
052700130927    1C     *INKL         IFEQ      *OFF
052710130927     c
052720130927     c* F17 - Note LDV   (solo se abilitato geo 87 on)
052730130927     c                   if        *in87 and *inkr
052740130927     c                   exsr      CALLNOTELDV
052750130927     c                   goto      for12
052760130927     c                   endif
052770130927     C*
052780130927     C* CONTROLLO FORMATO
052790130927     C                   EXSR      CONTR12
052800130927     C*
052810130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052820130927    3C     *IN90         IFEQ      *ON
052830130927     C     D48FFR        ANDEQ     'E'
052840130927     C                   MOVEL     VIDMSG        D48MSG
052850130927     C                   MOVEL     'F'           D48ERR
052860130927     C                   GOTO      ENDEM12
052870130927    3C                   ENDIF
052880130927     c
052890130927     c* Se non ho avuto errori e richiamo solo per controlli, esco
052900130927     c*   (d48ffr='E' e 26 off)
052910130927     c  n90
052920130927     cann26              if        d48ffr='E'
052930130927     C                   GOTO      ENDEM12
052940130927     c                   endif
052950130927     C  NKF
052960130927     CANN26
052970130930     COR 90              GOTO      FOR12
052980130927     c* richiamo routine di richiesta/controllo motivazione
052990130927     c                   if        d66mtv = 'S' and w48mct = *blanks
053000130927     c                   exsr      ges_mtv
053010130927     c                   if        olv85f12 = '1'
053020130927     c                   if        ilv85wdw = ' '
053030130927     c                   goto      for12
053040130927     c                   else
053050130927     c                   eval      *inkl = '1'
053060130927     C                   GOTO      ENDEM12
053070130927     c                   endif
053080130927     c                   endif
053090130927     C     *IN90         IFEQ      *ON
053100130927     C     D48FFR        ANDEQ     'E'
053110130927     C                   MOVEL     VIDMSG        D48MSG
053120130927     C                   MOVEL     'F'           D48ERR
053130130927     C                   GOTO      ENDEM12
053140130927     C                   ENDIF
053150130927     c                   endif
053160130927     C*
053170130927     c                   clear                   Aggiobol
053180130927     C                   EXSR      REGIS12
053190130927     c
053200130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
053210130927     C     *IN91         IFEQ      *ON
053220130927     C     D48FFR        ANDEQ     'E'
053230130927     C                   MOVEL     VIDMSG        D48MSG
053240130927     C                   MOVEL     'F'           D48ERR
053250130927     C                   GOTO      ENDEM12
053260130927     C                   ENDIF
053270130927     C   91              GOTO      FOR12
053280130927     C*
053290130927    1C                   ENDIF
053300130927     C*
053310130927     C     ENDEM12       TAG
053320130927     C*
053330130927    1C     *INKL         IFEQ      *ON
053340130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
053350130927    1C     D48FFR        OREQ      'E'
053360130927     C     *IN90         ANDEQ     *ON
053370130927    1C     D48FFR        OREQ      'E'
053380130927     C     *IN91         ANDEQ     *ON
053390130927     C* F12-RITORNO
053400130927    2C     *IN10         IFEQ      *ON
053410130927     C                   UNLOCK    FNARB01L
053420130927     C                   UNLOCK    FiAR901L
053430130927     C                   ELSE
053440130927     C                   UNLOCK    FNBLP01L
053450130927    2C                   END
053460130927     C* Comunico al chiamante che è stato premuto F12 valorizzando il
053470130927     C* flag D48F12 = 1
053480130927     C   KL              MOVEL     '1'           D48F12
053490130927    1C                   END
053500130927     c
053510130927     C                   UNLOCK    Fiar601l
053520130927     C*
053530130927     C                   ENDSR
053540070329     C
053550070329     C*--- richiama pgm per immettere note LDV -----------------------*
053560070329     C     CALLNoteLDV   BEGSR
053570140130     c                   clear                   fnlrn6ds
053580140130     c                   eval      in6mod='I'
053590140130     c                   eval      in6aas=arbaas
053600140130     c                   eval      in6lnp=arblnp
053610140130     c                   eval      in6nrs=arbnrs
053620140130     c                   eval      in6nsp=arbnsp
053630140130     c                   eval      in6pgm='FNLR48R'
053640140130     c                   eval      IN6FGS=arbifp
053650140130     c                   eval      IN6NFV=arbndc
053660140130     c                   eval      IN6DFV=arbddc
053670140130     c                   eval      IN6PDR=arbpdc
053680140130     c                   if        arbdcm=0  and arbndc>0
053690140130     c                   eval      IN6CONS='S'
053700140130     c                   endif
053710140130     c                   call      'FNLRN6R'
053720070329     c                   parm                    kpjba
053730140130     c                   parm                    fnlrn6ds
053740070329     c
053750070329     c                   ENDSR
053760061107     C
053770920120     C*--- CONTROLLI FORMATO2 ----------------------------------------*
053780920120     C     CONTR2        BEGSR
053790941025     C                   SETOFF                                       9028
053800020531     c                   clear                   wdisag
053810030122      * A T T E N Z I O N E!!!
053820030122      * Per la modifica destinatario
053830030122      * Se il nuovo destinatario necessita della consegna particolare
053840030122      * alla fine della REGIS2 viene impostata la causae 'CP' per
053850030122      * poter effettuare una nuova variazione bolla senza uscire dal pgm
053860941020     C***
053870941020     C*** DESTINATARIO  ***
053880941025    1C     *IN01         IFEQ      '1'
053890141112
053900141112     c* messaggio di avviso se pgm non richiamato e bolla con
053910141120     c* giacenza aperta  - Solo se variati dati del destinatario
053920141112     c                   if        *in20=*off and wforzgiac=0
053930141112     c                             and *in04
053940141120    5C     SAVCAD        IFNE      VIDCAD
053950141120     C     SAVLOD        ORNE      VIDLOD
053960141120     C     SAVind        ORNE      VIDind
053970141120     C     SAVprd        ORNE      VIDprd
053980141120     C     SAVnzd        ORNE      VIDnzd
053990141112     c                   eval      wforzgiac=1
054000141112     C                   SETON                                        2890
054010141112     C                   MOVEL     MSG(157)      VIDMSG
054020141112     C                   GOTO      ENDCT2
054030141112     c                   endif
054040141120     c                   endif
054050141112
054060970808     C                   MOVEL     VIDRSD        I14RSC
054070970808     C                   MOVE      VIDRSD        I14RS2
054080970808     C                   MOVEL     VIDIND        I14IND
054090970808     C                   MOVEL     VIDLOD        E14LOC
054100970808     C                   MOVEL     VIDPID        E14PIP
054110970808     C                   MOVEL     VIDPRD        E14PRV
054120970808     C                   MOVEL     VIDNZD        E14NAR
054130970808     C                   MOVEL     VIDCAD        E14CAP
054140970808     C                   MOVEL     VIDFIN        I14ISO
054150970808     C                   MOVEL     'D'           WCLI              1
054160970808     C                   EXSR      CTRIND
054170941020     C*
054180941020     C* CAMPI CHE POSSO AVER IMPOSTATO
054190970808     C                   MOVEL     E14LOC        VIDLOD
054200970808     C                   MOVEL     E14PIP        VIDPID
054210970808     C                   MOVEL     E14PRV        VIDPRD
054220970808     C                   MOVEL     E14NAR        VIDNZD
054230970808     C                   MOVEL     E14CAP        VIDCAD
054240970808     C* ERRORE
054250970808     C   90              GOTO      ENDCT2
054260060111     c* salvo lna desunta da cappario per non sporcarla se destinatario
054270060111     c* che richiede una particolare lna diversa da quella del cappario
054280060111     c                   move      o95lna        d_o95lna
054290020531     c* Verifico se destinatario disagiato
054300020604     c* Per bolle poste non faccio niente
054310060111     c                   clear                   ddstflo
054320020604     c     lnpntw        ifne      'PPT'
054330040702      * Referente e numero di telefono: se campi a video con '#'
054340040702      * sono quelli automatici (dal tisit5) e quindi li pulisco
054350040702     c                   If        %subst(VidRef:1:1) = '#'
054360040702     c                   Clear                   VidRef
054370040702     c                   EndIf
054380040705     c                   If        %subst(VidTeld:1:1) = '#'
054390040702     c                   Clear                   VidTeld
054400040702     c                   EndIf
054410020531     c                   clear                   tisit0ds
054420020531     c                   movel     'E'           it0tla
054430020531     c                   movel     vidnzd        it0naz
054440020531     c                   movel     vidprd        it0prv
054450020531     c                   movel     vidcad        it0cap
054460020531     c                   movel     vidlod        it0loc
054470020531     c                   movel     vidind        it0ind
054480020531     c                   movel     vidrsd        it0rag
054490020531     c                   call      'TISIT5R'
054500020531     c                   parm                    tisit0ds
054510020531     c     ot0err        ifeq      *blanks
054520060113     c                   movel     ot0flo        ddstflo
054530060111   2ac                   if        ot0dos = 'A' or ot0dos = 'D' or
054540060111     c                             ot0dos = 'S'
054550020531     c                   movel     ot0dos        wdisag
054560020531     c     wdisag        ifeq      'D'
054570020531     c                   movel     'X'           wdisag
054580020531     c                   endif
054590060111     c                   endif
054600020531     c                   else
054610020531     c                   clear                   wdisag
054620020531     c                   endif
054630060327     c***                If        wdisag <> *Blanks
054640060327     c***                          and %subst(arbgva:1:1) <> 'U' or
054650060327     c***                          wdisag='X' and %subst(arbgva:1:1)='U'
054660060327     c                   if        (wdisag = 'X') OR
054670060327     c                             (wdisag='S' and %subst(arbGva:1:1)<>'U') OR
054680060327     c                             (Wdisag='A' and %subst(arbGva:1:1)<>'U' and
054690060327     c                             (arbdcr=*zeros or arbtcr <> *blanks))
054700040702     c                   If        VidTeld = *Blanks
054710040702     c                   Eval      VidTeld = ot0tel
054720040702     c                   EndIf
054730040702     c                   If        VidRef = *Blanks
054740040702     c                   Eval      VidRef =  ot0ref
054750040702     c                   EndIf
054760040702     c                   EndIf
054770020604     c                   endif
054780060113     c*
054790060113     c                   if        §dstlna > *zeros
054800060111     c                   move      §dstlna       d_o95lna
054810060111     c                   endif
054820970808     C*
054830970808     C* MODIFICATI CAP E/O LOCALITA':
054840970808     C* 1) AGGIORNO A VIDEO IL FLAG INOLTRO E AVVISO CON MESSAGGIO
054850970808     C* 2) SE VARIA ANCHE LA LNA E CAP E LOCALITA SONO DIVERSI DALLA
054860970808     C*    BOLLA EMMETTO MESSAGGIO FORZABILE MANTENENDO COMUNQUE LA
054870970808     C*    LINEA DI ARRIVO E ZONA DELLA BOLLA
054880970808     C     WCAD          IFNE      VIDCAD
054890970808     C     WLOD          ORNE      VIDLOD
054900970808     C                   MOVE      VIDCAD        WCAD
054910970808     C                   MOVE      VIDLOD        WLOD
054920970808     C* FLAG INOLTRO
054930970808     C     O95ISO        IFNE      VIDFIN
054940970808     C                   MOVE      O95ISO        VIDFIN
054950140507     c* NON avviso se richiamo cieco
054960140507     c                   if        d48ffr<>'E'
054970970808     C                   SETON                                        289049
054980970808     C                   MOVEL     MSG(77)       VIDMSG
054990970808     C                   GOTO      ENDCT2
055000140507     c                   endif
055010970808     C                   END
055020970808     C                   END
055030970828     C* FLAG INOLTRO
055040970828     C     VIDFIN        IFEQ      ' '
055050970828     C                   MOVE      O95ISO        VIDFIN
055060140507     c* NON avviso se richiamo cieco
055070140507     c                   if        d48ffr<>'E'
055080970828     C                   SETON                                        289049
055090970828     C                   MOVEL     MSG(77)       VIDMSG
055100010412     C                   GOTO      ENDCT2
055110140507     c                   endif
055120970828     C                   END
055130010412     C  N29
055140010412     CANN71VIDFIN        IFEQ      'D'
055150010412     C     O95PID        ANDEQ     'N'
055160010412     C                   SETON                                        289049
055170010412     C                   MOVEL     MSG(98)       VIDMSG
055180010412     C                   GOTO      ENDCT2
055190010412     C                   ENDIF
055200970808     C*
055210140507     c* Non faccio il seguente controllo se richiamo "cieco"
055220141029     c                   if        %lookup('999':skFilOkdir)=0 and
055230141029     c                             (%lookup(%editc(arblna:'X'):skFilOkdir)=0
055240141029     c                             or %lookup(%editc(dutpou:'X'):skFilOkdir)=0)
055250141029     c                   if        d48ffr<>'E'
055260141029     C     WCAD1         IFNE      VIDCAD
055270141029     C     WLOD1         ORNE      VIDLOD
055280141029     C     Wrsd1         ORNE      VIDrsd
055290141029     C     Wprd1         ORNE      VIDprd
055300141029     C     Wnzd1         ORNE      VIDnzd
055310141029     C     Wind1         ORNE      VIDind
055320141029     C                   MOVE      VIDCAD        WCAD1
055330141029     C                   MOVE      VIDLOD        WLOD1
055340141029     C                   MOVE      VIDrsd        Wrsd1
055350141029     C                   MOVE      VIDind        Wind1
055360141029     C                   MOVE      VIDprd        Wprd1
055370141029     C                   MOVE      VIDnzd        Wnzd1
055380141029     C     d_o95lna      ifne      ARBLNA
055390141029     C     SAVCAD        IFNE      VIDCAD
055400060111     C***  O95LNA        ANDNE     ARBLNA
055410141029     C     SAVLOD        ORNE      VIDLOD
055420060111     C***  O95LNA        ANDNE     ARBLNA
055430141029     C     SAVrsd        ORNE      VIDrsd
055440141029     C     SAVind        ORNE      VIDind
055450141029     C     SAVprd        ORNE      VIDprd
055460141029     C     SAVnzd        ORNE      VIDnzd
055470141029     C                   SETON                                        9028
055480141029     C                   MOVEL     MSG(76)       VIDMSG
055490141029     C                   GOTO      ENDCT2
055500141029     C                   END
055510141029     C                   END
055520141029     C                   END
055530141029     c                   endif
055540141029     c                   endif
055550941020     C**
055560950522     C* Se P.IVA sprotetta e nazione estera con flag efta la P.IVA è
055570950522     C* obbligatoria
055580950412     C     *IN05         IFEQ      *OFF
055590950412     C     *IN72         ANDEQ     *OFF
055600950522     C     WDESIE        ANDEQ     'E'
055610950522     C     DESEFT        ANDNE     *BLANKS
055620950412     C     VIDPID        ANDEQ     *BLANKS
055630950412     C                   SETON                                        284590
055640950412     C                   MOVEL     MSG(67)       VIDMSG
055650950412     C                   GOTO      ENDCT2
055660950412     C                   END
055670061116     c
055680061116     c* Se modifica la ragione sociale del destinatario, controllare
055690061116     c*  la partita iva
055700140507     c* Non controllo se chiamata pgm richiamato "cieco"
055710140507     c                   if        d48ffr<>'E'
055720061116     c                   if        vidrsd<>savrsdQ and vidcdf<>*blanks
055730061116     C                   SETON                                        285190
055740061116     C                   MOVEL     MSG(134)      VIDMSG
055750061116     c                   movel     vidrsd        savrsdQ
055760061116     C                   GOTO      ENDCT2
055770061116     C                   END
055780140507     c                   endif
055790061116     c
055800061116     c* La memorizzo anche se cdf=*blanks
055810061116     c                   movel     vidrsd        savrsdQ
055820061115     c
055830061115      * Devo richiamare il nuovo driver fatto per controllare il codice
055840061115      * fiscale
055850061115     c                   if        vidcdf<>*blanks
055860151105     c                   clear                   xcfiva1ds
055870061115     c                   clear                   xcfivamod
055880061115     c                   eval      xcfivapar = vidcdf
055890061115     c                   eval      xcfivanar = vidnzd
055900061115     c                   eval      xcfivaprv = vidprd
055910151105     c                   eval      xcfivacap = vidcad
055920151105     c                   eval      xcfivaloc = vidlod
055930151105     c                   eval      xcfivapiva= vidpid
055940151105     c                   call      'XCFIVAR1'
055950151105     c                   parm                    xcfiva1ds
055960061115     c*
055970061115     c                   if        xcfivaflg < *zeros
055980061115     C                   SETON                                        289051
055990080505     C                   MOVEL     xcfivamsg     VIDMSG
056000061115     C                   GOTO      ENDCT2
056010061115     c                   endif
056020061115     c                   endif
056030120508     c* Parita iva $$ non ammessa per fattura di filiale
056040120508     c                   move      v1cksc        w0040
056050120508     c                   if        %scan('$$':vidpid)>0  and w0040=9999
056060120508     C                   SETON                                        284590
056070120508     C                   MOVEL     MSG(149)      VIDMSG
056080120508     C                   GOTO      ENDCT2
056090120508     c                   endif
056100070508     c
056110140408     c
056120070508     c                   movel     vidffd        wffd
056130141029     c* Il fermo deposito non si può inserire se presente dispo di dirottamento da
056140141029     c* eseguire
056150141029     c                   if        vidffd<>*blanks and arbffd<>wffd
056160141029     c                   clear                   fnlry09ds2
056170141029     c                   eval      ILRY09TCH='T'
056180141029     c                   eval      ILRY09TDIS='D'
056190141029     c                   eval      ILRY09AAS=d48aas
056200141029     c                   eval      ILRY09LNP=d48lnp
056210141029     c                   eval      ILRY09NRS=d48nrs
056220141029     c                   eval      ILRY09NSP=d48nsp
056230141029     c                   eval      kpjbu=fnlry09ds2
056240141031     c                   call      'FNLRY09C'
056250141029     c                   parm                    kpjba
056260141029     c                   eval      fnlry09ds2=kpjbu
056270141029     c                   if        OLRY09ESIT='1'
056280141029     C                   SETON                                        286890
056290141029     C                   MOVEL     MSG(155)      VIDMSG
056300141029     C                   GOTO      ENDCT2
056310141029     c                   endif
056320141029     c                   endif
056330140408     c* Il fermo deposito non si può inserire se presente particolarità consegna
056340140408     c* che prevede che il fermo deposito debba essere autorizzato dal mittente
056350140408     c                   if        vidffd<>*blanks and arbffd<>wffd and
056360140408     c                             arbgma<>*blanks
056370140408     c                   Movel     '7R'          cod
056380140408     c                   Movel(p)  ArbGMa        key
056390140408     c                   ExSr      ctabel
056400140408     c  n30              Movel     TblUni        ds7R
056410140408     c   30              Clear                   ds7R
056420140408     C                   IF        §7RFD='S' AND vidcvr<>'I7'
056430140408     C                   SETON                                        289068
056440140408     C                   MOVEL     MSG(153)      VIDMSG
056450140408     C                   GOTO      ENDCT2
056460140408     C                   ENDIF
056470140408     c                   endif
056480140408     c* Da contattare impostabile solo se immesso il fermo deposito
056490140408     c*  solo se GEO attiva
056500141223     c* NON faccio questo controllo se richiamo cieco
056510141223     c                   if        d48ffr<>'E'
056520140213     c   77              if        (arbffd=wffd or  vidffd='  ') and
056530070508     c                             vidtfd='SI'
056540070508     C                   SETON                                        289052
056550070508     C                   MOVEL     MSG(99)       VIDMSG
056560070508     C                   GOTO      ENDCT2
056570070508     c                   endif
056580141223     c                   endif
056590070508     C
056600070508     c* se immesso fermo deposito e c'e' la consegna richiesta avviso
056610140507     c* NON faccio questo controllo se richiamo cieco
056620140507     c                   if        d48ffr<>'E'
056630140213     c   77              if        vidffd='SI' and arbffd<>wffd and
056640070508     c                             vidtfd='  ' and arbdcr>0  AND WZ3=0
056650070508     C                   SETON                                        289052
056660070508     C                   MOVEL     MSG(143)      VIDMSG
056670070508     C                   EVAL      WZ3=1
056680070508     C                   GOTO      ENDCT2
056690140213     c                   endif
056700140507     c                   endif
056710061115     c
056720941025    1C                   ENDIF
056730941019     C*
056740930505     C***
056750941025    1C     *IN02         IFEQ      '1'
056760930505     C* PESO
056770030203      * Se non è protetto
056780030203   2ac                   If        Not *In88
056790930505     C* NON A 0 O MAGGIORE DEL LIMITE POSTO
056800980406     C     VIDPKF        IFEQ      0
056810941020     C                   SETON                                        479028
056820941020     C                   MOVEL     MSG(2)        VIDMSG
056830911209     C                   GOTO      ENDCT2
056840941025    2C                   END
056850000510     C* DPD NO MAGGIORE DEL LIMITE IMPOSTO
056860020318    2C     LNPNTW        IFEQ      'DPD'
056870020318     C     LNANTW        OREQ      'DPD'
056880000510    3C     VIDPKF        IFGT      §3IPKD
056890000510     C                   SETON                                        479028
056900000510     C                   MOVEA     MSG(52)       K78
056910000510     C                   MOVEL     §3IPKD        W006A             6
056920000510     C                   MOVEA     W006A         SKI(10)
056930000510     C                   Z-ADD     10            I                 3 0
056940000510     C                   EXSR      ABBL
056950000510     C                   MOVEA     SKI(10)       K78(53)
056960000510     C                   MOVE      §3IPKD        W001A
056970000510     C                   MOVEL     W001A         K78(60)
056980000510     C                   MOVEA     K78           VIDMSG
056990000510     C                   GOTO      ENDCT2
057000000510    3C                   END
057010000510    2C                   END
057020000510     C**
057030000510     C**
057040000510     C     VARPKF        IFNE      VIDPKF
057050000510     C                   CLEAR                   WPT1
057060000510     C                   ENDIF
057070000510     C*
057080000510     C     VARPKF        IFNE      VIDPKF
057090000510     C     ARBFVF        ANDEQ     'K'
057100000510     C                   CLEAR                   SET
057110000510     C                   ENDIF
057120000510     C                   MOVEL     VIDPKF        VARPKF
057130000510     C**
057140021121
057150021121     c                   Move      ArbCtr        DueCtr
057160021121
057170021121      * Bolla FedEx con tariffa documenti il peso non può essere superiore a
057180021121      * quanto indicato in tabella "DFE" --> msg forzabile
057190140507     c* Non controllo se progamma richiamato in modalità "cieca"
057200140507     c                   if        d48ffr<>'E'
057210021121If  2c                   If        LnaNtw = 'FED' and DueCtr >= §DfeDalD and
057220021121     c                             DueCtr <= §DfeAlD and VidPkf > §DfePkgD
057230021121     c                             and Wpt1 = *Blanks
057240021121     c                   Eval      *In47 = *On
057250021121     c                   Eval      *In28 = *On
057260021121     c                   Eval      *In90 = *On
057270021121      * Preparo il msg
057280021121     c                   Movea     Msg(116)      K78
057290021121     c                   Movel     §DfePkgD      W006a
057300021121     c                   Movea     W006a         Ski(10)
057310021121     c                   Z-Add     10            i
057320021121     c                   ExSr      Abbl
057330021121     c                   Movea     Ski(10)       K78(50)
057340021121     c                   Move      §DfePkgD      W001a
057350021121     c                   Movel     W001a         K78(57)
057360021121     c                   Movea     K78           VidMsg
057370021121     c                   Eval      Wpt1 = '1'
057380021121     c                   GoTo      EndCt2
057390021121    2c                   EndIf
057400140507     c                   endif
057410051129      * Nel caso di peso totalmente rilevato da VDL posso modificare solo se
057420051129      * inserisco il peso uguale a quello VDL
057430030123     c**!!!              If         ArbNcp = ArbNcl and VidPkc <> VidPkf
057440030123     c                   If         wfpf = 'T' and VidPkc <> VidPkf
057450030122     c                   Seton                                        479028
057460030122     c                   Eval      VidMsg = Msg(121)
057470030122     c                   GoTo      EndCt2
057480030122    2c                   EndIf
057490051129      * Nel caso di peso parzialmente rilevato da VDL posso immettere un peso uguale
057500051129      * o superiore a quello VDL
057510030123     c                   If        wfpf = 'Z' and VidPkf < VidPkc
057520030123     c                   Seton                                        479028
057530030123     c                   Eval      VidMsg = Msg(123)
057540030123     c                   GoTo      EndCt2
057550030123    2c                   EndIf
057560030203   2ac                   EndIf
057570021121
057580930505     C* VOLUME
057590091124      * Se non è protetto
057600091124   2ac                   If        Not *In89
057610091124     c
057620980406     C     VIDVLF        IFEQ      0
057630941215     C                   SETON                                        489028
057640941020     C                   MOVEL     MSG(3)        VIDMSG
057650911209     C                   GOTO      ENDCT2
057660941025    2C                   ENDIF
057670941214     C*
057680941027     C* SE IL TIPO VOLUME E' "Z" NON SI PUO' ABBASSARE
057690941027     C     VIDFVF        IFEQ      'Z'
057700941027     C     VIDVLF        ANDLT     ARBVLF
057710941215     C                   SETON                                        489028
057720941027     C                   MOVEL     MSG(5)        VIDMSG
057730941027     C                   GOTO      ENDCT2
057740941027    2C                   ENDIF
057750091124     c*
057760091124     c* Per volume totaole variabile solo come il totale
057770091124     c                   If         ArbNcr = ArbNcl and Vidvlf <> arbvlc
057780091124     c                   Seton                                        489028
057790091124     c                   Eval      VidMsg = Msg(147)
057800091124     c                   GoTo      EndCt2
057810091124    2c                   EndIf
057820091124     c
057830091124      * Nel caso di vol  parzialmente rilevato da VDL posso immettere un vol  uguale
057840091124      * o superiore a quello VDL
057850091124     c                   If         ArbNcr < ArbNcl and Vidvlf < arbvlc
057860091124     c                              and arbncr>0
057870091124     c                   Seton                                        489028
057880091124     c                   Eval      VidMsg = Msg(148)
057890091124     c                   GoTo      EndCt2
057900091124    2c                   EndIf
057910091124   2ac                   EndIf
057920941214     C*
057930051129     C* SE IL TIPO VOLUME NON E' "Z" MA ESISTE UN VOLUME VDL CHE DIVENT
057940941214     C*  SUPERIORE A QUELLO DA FATTURARE DOPO LA VARIAZIONE, AVVISO E
057950941215     C*  MODIFICO
057960941215     C     VARVLF        IFNE      VIDVLF
057970941215     C                   CLEAR                   SET
057980941215     C                   MOVEL     VIDVLF        VARVLF
057990941215     C                   ENDIF
058000941215     C*
058010051129     C*  POI IMPOSTERO NEL VOLUME DA FATTURARE IL VOLUME VDL
058020941214    2C     SET           IFEQ      0
058030941214     C     ARBVLC        ANDGT     0
058040941214     C     VIDFVF        ANDNE     'Z'
058050941214     C     VIDFVF        ANDNE     'T'
058060941214     C*
058070051129    3C     WVDL          IFEQ      'S'
058080970418    4C     WFVF          IFEQ      'Z'
058090941222     C   06              MOVEL     ZAFA          WSOST             1
058100941222     C  N06              MOVEL     ZAAA          WSOST
058110970418   X4C                   ELSE
058120970418     C   06              MOVEL     TAFA          WSOST             1
058130970418     C  N06              MOVEL     TAAA          WSOST
058140970418    4C                   END
058150970418   X3C                   ELSE
058160970418    4C     WFVF          IFEQ      'Z'
058170941222     C   06              MOVEL     ZAFN          WSOST             1
058180941222     C  N06              MOVEL     ZAAN          WSOST
058190970418     C                   ELSE
058200970418     C   06              MOVEL     TAFN          WSOST             1
058210970418     C  N06              MOVEL     TAAN          WSOST
058220970418    4C                   ENDIF
058230970418    3C                   ENDIF
058240941214     C*
058250941214     C* SE VOLUME SAREBBE DA SOSTITUIRE VEDO SE DIVENTA SUPERIORE
058260941214     C* DEL VOLUME DA FATTURARE
058270941214    3C     WSOST         IFEQ      'S'
058280970418     C                   CLEAR                   WSOSC
058290941214     C*
058300941214    4C     VIDVLF        IFLT      ARBVLF
058310941214     C     VIDVLF        ANDLT     ARBVLC
058320970418     C* PRENDO FLAG DI SOSTITUIBILITA' VOLUME REALE
058330970418     C                   MOVEL     '7T'          COD
058340970418     C                   MOVEL(P)  'R'           KEY
058350970418     C                   EXSR      CTABEL
058360970418     C   30              CLEAR                   DS7T
058370970418     C  N30              MOVEL     TBLUNI        DS7T
058380970418     C     WFVF          IFEQ      'Z'
058390970418     C                   MOVEL     §7TSOZ        WSOSC
058400970418     C                   ELSE
058410970418     C                   MOVEL     §7TSOT        WSOSC
058420970418     C                   END
058430970418     C* (SOLO SE IL VOLUME E' SOSTITUIBILE)
058440970418    5C     WSOSC         IFEQ      'S'
058450140507     C                   ADD       1             SET
058460140507     c* non segnalo se pgm richiamato in modalità "cieca"
058470140507     c                   if        d48ffr<>'E'
058480941214     C                   SETON                                        482890
058490941214     C                   MOVEL     MSG(58)       VIDMSG
058500941214     C                   GOTO      ENDCT2
058510140507     c                   endif
058520970418    5C                   ENDIF
058530941214    4C                   ENDIF
058540941214     C*
058550941214     C* SE VOLUME AUTOMATICO E MODIFICATO IL PESO CONTROLLO
058560970418   4AC     *IN88         IFEQ      *OFF
058570941214     C                   Z-ADD     VIDVLF        D20VLU
058580941214     C                   EXSR      VOLAUT
058590970418    4C     D20VLU        IFLT      ARBVLC
058600970418     C                   MOVEL     '7T'          COD
058610970418     C                   MOVEL(P)  ARBFVF        KEY
058620970418     C                   EXSR      CTABEL
058630970418     C   30              CLEAR                   DS7T
058640970418     C  N30              MOVEL     TBLUNI        DS7T
058650051129     C* FLAG DI SOSTITUIBILITA' DA VOL.VDL
058660970418     C     WFVF          IFEQ      'Z'
058670970418     C                   MOVEL     §7TSOZ        WSOSC
058680970418     C                   ELSE
058690970418     C                   MOVEL     §7TSOT        WSOSC
058700970418     C                   END
058710970418    5C     WSOSC         IFEQ      'S'
058720140507     C                   ADD       1             SET
058730140507     c* non segnalo se pgm richiamato in modalità "cieca"
058740140507     c                   if        d48ffr<>'E'
058750941215     C                   SETON                                        472890
058760941214     C                   MOVEL     MSG(58)       VIDMSG
058770941214     C                   GOTO      ENDCT2
058780140507     c                   endif
058790970418    5C                   ENDIF
058800941214    4C                   ENDIF
058810970418   4AC                   ENDIF
058820941214    3C                   ENDIF
058830941214    2C                   ENDIF
058840170130     c* controllo peso e volume con i limiti previsti in tabella "TSP"
058850170130     c* (limiti per tipo servizio)
058860170131     C                   if        not *in88  or not *in89
058870170130     c                   exsr      sr_limtsp
058880170130     c                   if        *in90
058890170130     c                   seton                                        28
058900170130     C                   MOVEL     wmsgtsp       VIDMSG
058910170130     C                   GOTO      ENDCt2
058920170130     c                   endif
058930170130     c                   endif
058940941214     C*
058950941027    1C                   ENDIF
058960910429     C*
058970910429     C     ENDCT2        ENDSR
058980170130     C**************************************************************************
058990170130     C* Controllo limiti (peso e volume) per tipo servizio
059000170130     C**************************************************************************
059010170130     C     sr_limtsp     BEGSR
059020170130     c                   clear                   wmsgtsp
059030170130     c                   clear                   trul73ds
059040170130     c                   eval      i73tsp=arbtsp
059050170130     c                   eval      i73psk=vidpkf
059060170130     c                   eval      i73vmc=vidvlf
059070170130     c                   eval      i73ncl=arbncl
059080170130     c                   call      'TRUL73R'
059090170130     c                   parm                    trul73ds
059100170130     c                   select
059110170130     c                   when      o73err='E'
059120170130     c                   seton                                        90
059130170130     c                   eval      wmsgtsp=msg(163)
059140170130     c                   eval      %subst(wmsgtsp: 40: 8)=v1dtsp
059150170130     c* peso kg bloccante
059160170130     c                   when      O73FKGM='1'  and not *in88
059170170131     c                                          and wfpf <> 'T' and wfpf<>'Z'
059180170130     c                   seton                                        9047
059190170130     c                   eval      wmsgtsp=msg(164)
059200170131     c                   eval      %subst(wmsgtsp: 19: 3)=wtsp03
059210170130     c                   eval      %subst(wmsgtsp: 34: 8)=%editc(o73lkgm:'4')
059220170130     c*volume mc bloccante
059230170131     c                   when      O73FMCM='1'  and not *in89
059240170131     c                                          and arbncr=0
059250170130     c                   seton                                        9048
059260170130     c                   eval      wmsgtsp=msg(165)
059270170131     c                   eval      %subst(wmsgtsp: 19: 3)=wtsp03
059280170130     c                   eval      %subst(wmsgtsp: 36: 6)=%editc(o73lmcm:'4')
059290170130     c* peso kg forzabile
059300170130     c                   when      O73Fkgf='1' and vidpkf<>wpes_pre
059310170130     c                             and not *in88 and d48ffr<>'E'
059320170131     c                             and wfpf <> 'T' and wfpf<>'Z'
059330170130     c                   seton                                        9047
059340170130     c                   eval      wmsgtsp=msg(164)
059350170131     c                   eval      %subst(wmsgtsp: 19: 3)=wtsp03
059360170130     c                   eval      %subst(wmsgtsp: 34: 8)=%editc(o73lkgf:'4')
059370170130     c                   eval      wmsgtsp=%trim(wmsgtsp)+ ' Avvisare ROM.'
059380170130     c                   eval      wmsgtsp=%trim(wmsgtsp)+ ' ENTER per forzare'
059390170130     c                   eval      wpes_pre=vidpkf
059400170130     c* volume mc forzabile
059410170130     c                   when      O73FmcF='1' and vidvlf<>wvol_pre
059420170130     c                             and not *in89 and d48ffr<>'E'
059430170131     c                             and arbncr=0
059440170130     c                   seton                                        9048
059450170130     c                   eval      wmsgtsp=msg(165)
059460170131     c                   eval      %subst(wmsgtsp: 19: 3)=wtsp03
059470170130     c                   eval      %subst(wmsgtsp: 36: 6)=%editc(o73lmcf:'4')
059480170130     c                   eval      wmsgtsp=%trim(wmsgtsp)+ ' Avvisare ROM.'
059490170130     c                   eval      wmsgtsp=%trim(wmsgtsp)+ ' ENTER per forzare'
059500170130     c                   eval      wvol_pre=vidvlf
059510170130     c                   endsl
059520170130     c                   endsr
059530911212     C*
059540911209     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE IX -----------------*
059550911209     C     REGIS2        BEGSR
059560130412     c                   clear                   wpda_ndc
059570130412     c                   clear                   wpda_ddc
059580130412     c                   clear                   wpda_dcm
059590160318     c* flag per togliere da distinta spedizioni import dpd "VEDI PACCO"
059600160318     c                   clear                   wtogli
059610950104     C*  GIRO UDATE
059620950104     C                   TIME                    W0140            14 0
059630950104     C* UDATE IN GGMMAAAA
059640950104     C                   MOVE      W0140         WDTGIO            8 0
059650950104     C*
059660950104     C* UDATE IN AAAAMMGG
059670950104     C                   Z-ADD     WDTGIO        G02DAT
059680950104     C                   MOVEL     *BLANK        G02ERR
059690950104     C                   CALL      'XSRDA8'
059700950104     C                   PARM                    WLBDAT
059710950104     C                   MOVEL     G02INV        DATEU             8 0
059720941213     C                   MOVEL     KNMUS         ARBPRU
059730941221     C* PRENDO I VALORI ASSOLUTI
059740941221     C                   MLLZO     1             VIDPKF
059750941221     C                   MLLZO     1             VIDVLF
059760041007     C* SALVO VOL/PKG/FTC/RSD/CAP/FIN VECCHIO IN FNARB
059770950114     C                   MOVEL     ARBVLF        OLDVLF
059780950114     C                   MOVEL     ARBFVF        OLDFVF
059790950114     C                   MOVEL     ARBPKF        OLDPKF
059800041007     C                   MOVEL     ARBvlc        OLDvlc
059810950114     C                   MOVEL     ARBRSD        OLDRSD
059820150206     C                   MOVEL     ARBLOD        OLDLOD
059830950114     C                   MOVEL     ARBCAD        OLDCAD
059840950114     C                   MOVEL     ARBFIN        OLDFIN
059850041007     C                   MOVEL     ARBFFD        OLDFFD
059860070320     c                   movel     VIDFFD        WFFD
059870151230     c* (Se variazione fatta da Arrivo su bolla ancora da partire non devo inviare)
059880151222     c                   if        not *in10 or wfile='A'
059890920120     C* INVIO PER SEDE
059900170116     C***                MOVEL     'FNARBD0T'    SFILE
059910920120     C* ALLOCO
059920170116     C****               EXSR      ALLOC
059930941027     C* ERRORI IN ALLOCAZIONE
059940170116    1C***  *IN91         IFEQ      *ON
059950170116     C***                GOTO      ENDRG2
059960170116    1C***                ENDIF
059970070515    c
059980070515     c* Se modificato FERMO DEPOSITO, devo allocare record L di ar4
059990070515     c*  per blocco/sblocco telefonata
060000070515     c                   clear                   wesar4L           1
060010070515    1c                   if        Wffd<>oldFFD
060020070515     c                   eval      TipoEla='C'
060030070515     c                   exsr      GestAR4L
060040070515     c
060050070515    2c                   if        *in91
060060170116     c*****              exsr      DLCSED
060070070525     C                   SETON                                        2891
060080070515     C                   GOTO      ENDRG2
060090070515    2c                   endif
060100070515    1c                   endif
060110070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
060120070515     c* iornare direttamente il file
060130070515     c                   clear                   waltrabolla       1
060140151222     C   10KARB          CHAIN     FNBLP01L                           3091
060150151222     C  n10KARB          CHAIN     FNARB01L                           3091
060160070515     c*
060170070515     c                   if        *in91
060180070515     C                   MOVEL     MSG(4)        VIDMSG
060190070515     c*
060200170116     c****               exsr      DLCSED
060210070515     c                   unlock    fiar401l
060220070515     c
060230070525     C                   SETON                                        2891
060240070515     C                   GOTO      ENDRG2
060250070515     c                   else
060260070515     c                   if        *in30
060270070515     c                   eval      waltrabolla='N'
060280070515     c                   else
060290070515     c* Salvo alcuni campi dell'altro file che potrebbero, per tempi
060300070515     c*  di aggiornamento, essere diversi.
060310070515     c                   eval      waltrofvf=arbfvf
060320070515     c                   eval      waltrovlf=arbvlf
060330070515     c                   eval      waltrocpi=arbcpi
060340130411     c* salvo campi di arb che mi servono per invio dati a pda
060350151222     c  n10              eval      wpda_ndc=arbndc
060360151222     c  n10              eval      wpda_ddc=arbddc
060370151222     c  n10              eval      wpda_dcm=arbdcm
060380151222     C   10KARB          CHAIN     FNARB01L                           30
060390151222     C  n10KARB          CHAIN     FNBLP01L                           30
060400070515     c* Alla fine per non sporcarmi i campi del record, richaino il
060410070515     c*  record originale che sto variando
060420151222     c   10              eval      wpda_ndc=arbndc
060430151222     c   10              eval      wpda_ddc=arbddc
060440151222     c   10              eval      wpda_dcm=arbdcm
060450070515     c                   endif
060460070515     c                   endif
060470151222     c                   endif
060480070207     C*
060490070207     C* SE SI TRATTA DI BOLLA FRANCO, SALVO IL CAMPO DELLA PARTITA IVA
060500151222     C*   10
060510151222     C*AN 05              MOVEL     ARBCPI        SAVCPI           16
060520151222     C*   10
060530151222     C*AN 05              MOVEL     *BLANKS       ARBCPI
060540070207     C*
060550151222     C*  N10
060560151222     C*AN 06              MOVEL     ARBCPI        SAVCPI           16
060570151222     C*  N10
060580151222     C*AN 06              MOVEL     *BLANKS       ARBCPI
060590151222     c                   if        (wfile='A' and *in05) or
060600151222     c                             (wfile='P' and *in06)
060610151222     C                   MOVEL     ARBCPI        SAVCPI           16
060620151222     C                   MOVEL     *BLANKS       ARBCPI
060630151222     c                   endif
060640070207     C*
060650061116     c* Se variazione di destinatario chaino anche fiar4 record Q se
060660061116     c*  mi serve
060670141215     c                   if        wnoindir=' '
060680061116     c                   if        *in01 and not *in05
060690061116     c                   if        (vidcdf=*blanks  and war4Q='D') or
060700061116     c                             (vidcdf<>*blanks and war4Q<>' ')
060710061116     C                   MOVEL     'Q'           WTRC
060720061116     C     KARB2         CHAIN     FiAR4000                           3091
060730061116     c*
060740061116     c                   if        *in91
060750061116     C                   MOVEL     MSG(4)        VIDMSG
060760070515     c
060770151223     c                   if        not *in10 or wfile='A'
060780170116     c******             exsr      DLCSED
060790151223     c   10              unlock    fnblp01l
060800151223     c  N10              unlock    fnarb01l
060810151223     c                   endif
060820070525     C                   SETON                                        2891
060830061116     C                   GOTO      ENDRG2
060840061116     c                   endif
060850061116     c                   endif
060860061116     c                   endif
060870141215     c                   endif
060880061116     c
060890061116     c* Verifico se prima c'era
060900911212     C*
060910911209     C                   Z-ADD     DATEU         ARBDTV
060920911209     C                   TIME                    ARBORV
060930911209     C                   MOVEL     VIDCVR        ARBCVB
060940911209     C*
060950060130     C* SE DA STORICIZZARE
060960060130     C**   *IN10         IFEQ      *ON
060970060130    1C**   D66FSA        ANDEQ     'S'
060980060130     C**   *IN10         OREQ      *OFF
060990060130    1C**   D66FSP        ANDEQ     'S'
061000060130    1C     D66FSA        ifeq      'S'
061010941212     C                   MOVEL     SAVRD2        ARBRD2
061020950114     C* SE VENGO DA VARIAZIONE MITTENTE, IL VOLUME LO STORICIZZO CON I
061030950114     C* CAMPI CHE HO SALVATO
061040950114     C     *IN24         IFEQ      *ON
061050950114     C                   MOVEL     MITFVF        ARBFVF
061060950114     C                   MOVEL     MITVLF        ARBVLF
061070950114     C                   MOVEL     MITFVF        ARBFVB
061080950114     C                   MOVEL     MITVLF        ARBVLB
061090950114    1C                   ENDIF
061100011108      *
061110011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
061120011108      * è in partenza 'P' o in arrivo 'A'
061130011108     C     *IN10         IFEQ      *ON
061140160119     c                   if        wfile='P'
061150160119     C                   MOVE      'a'           ARBPRU
061160160119     c                   else
061170011108     C                   MOVE      'A'           ARBPRU
061180160119     c                   endif
061190011108     C                   ENDIF
061200011108     C     *IN10         IFEQ      *OFF
061210011108     C                   MOVE      'P'           ARBPRU
061220011108     C                   ENDIF
061230011108      *
061240941014     C                   WRITE     FNARBD00
061250130927     c
061260130927     c* Memorizzo anche referente e telefono se si tratta di varazione "I0"
061270131009     c                   if        *in01 and
061280131009     c                                  (vidref<>par5ref or vidteld<>par5teld)
061290131204     c                   clear                   darbn_ref
061300130927     c                   eval      §arbnref = par5ref
061310130927     c                   eval      §arbnteld= par5teld
061320131204     c                   eval      arbuni70 = darbn_ref
061330130930     c                   eval      arbtrd='REF'
061340130927     c                   write     FNARBN00
061350130927     c                   endif
061360950114     C*
061370950114     C     *IN24         IFEQ      *ON
061380950114     C                   MOVEL     OLDFVF        ARBFVF
061390950114     C                   MOVEL     OLDVLF        ARBVLF
061400950114     C                   MOVEL     OLDFVF        ARBFVB
061410950114     C                   MOVEL     OLDVLF        ARBVLB
061420950114    1C                   ENDIF
061430950114    1C                   ENDIF
061440060301     c* storicizzazione motivazione
061450060301     c     d66mtv        ifeq      'S'
061460060301     c                   exsr      sto_mtv
061470060301     c                   endif
061480911209     C*
061490941025    1C     *IN01         IFEQ      *ON
061500040224
061510040224      * se in arrivo ho modificato uno dei dati del Fiar5 rcd GEN:
061520040224      * - Fermo deposito
061530040224      * devo memorizzare i dati della bolla prima della modifica
061540070320if  3c   10              If        wffd <> oldFfd
061550041022     c                   ExSr      RegAr5AR
061560040224    3c                   EndIf
061570941025     C*
061580941025     C* SE IMMESSA ANCHE LA 2 RAGIONE SOCIALE
061590141212     c                   if        wnoindir=' '
061600060223     c                   clear                   d19ftr
061610160115     c                   if        *in10 and wfile='P'
061620160115     c                   eval      d19ftr='T'
061630160115     c                   endif
061640941025     C                   MOVEL     ARBAAS        D19AAS
061650941025     C                   MOVEL     ARBLNP        D19LNP
061660941025     C                   MOVEL     ARBNRS        D19NRS
061670941025     C                   MOVEL     ARBNSP        D19NSP
061680941025     C                   MOVEL     'D'           D19TRC
061690970520     C                   MOVE      VIDRSD        D19NT1
061700941025     C                   CLEAR                   D19NT2
061710051114     c
061720941025     C                   CALL      'FNLV19R'
061730030612     C                   PARM                    Fnlv19DS
061740941025     C*
061750941025     C* ERRORE: RECORD NON VINCOLABILE
061760941212    2C     D19ERR        IFEQ      '2'
061770941025     C                   MOVEL     MSG(4)        VIDMSG
061780151223     c                   if        not *in10 or wfile='A'
061790170116     c****               exsr      dlcsed
061800070515     c   10              unlock    fnblp01l
061810070515     c  N10              unlock    fnarb01l
061820070515     c                   unlock    fiar401l
061830151230     c                   endif
061840151223     C                   SETON                                        2891
061850941025     C                   GOTO      ENDRG2
061860941212    2C                   ENDIF
061870141212     c                   endif
061880041006    c
061890950626     C*
061900950626     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
061910950626     C*     NELLA P.IVA
061920950626     C     VIDISD        IFEQ      '$$'
061930950626     C     VIDCPD        ANDEQ     *BLANKS
061940950626     C                   MOVEL     'PRIVATO'     VIDCPD
061950950626     C                   ENDIF
061960941025     C*
061970141212     c                   if        wnoindir=' '
061980941027     C                   MOVEL     VIDRSD        ARBRSD
061990970520     C                   MOVE      D19NT1        ARBRD2
062000911209     C                   MOVEL     VIDIND        ARBIND
062010911209     C                   MOVEL     VIDLOD        ARBLOD
062020911209     C                   MOVEL     VIDCAD        ARBCAD
062030060705     c* se spedizione export DPD diretta in Portogallo devo aggiungere al
062040060705     c* cap immesso tanti 0 a dx quanti ne servono per arrivare ad una
062050060705     c* lunghezza di cap di 7 byte
062060060705     c                   if        lnantw='DPD' and vidnzd = 'P  '
062070060705     c     ' '           checkr(e) arbcad:7      NN                1 0
062080060705     c                   if        %found
062090060705     c                   eval      %subst(arbcad:nn+1:7-nn)=*all'0'
062100060705     c                   endif
062110060705     c                   endif
062120060705     c*
062130911209     C                   MOVEL     VIDPRD        ARBPRD
062140941220     C                   MOVEL     VIDPID        ARBCPI
062150941220     C* LA P.IVA SE HA CODICE ISO VIENE MESSO A SIN ALTRIMENTI SUBITO
062160941220     C*  IL CODICE DELLA P.IVA
062170941220     C     VIDCPD        IFNE      *BLANKS
062180941220     C     VIDISD        ANDEQ     *BLANKS
062190941220     C                   CLEAR                   ARBCPI
062200941220     C                   MOVEL     VIDCPD        ARBCPI
062210941220     C                   ENDIF
062220041027     c                   eval      newcpi=arbcpi
062230041027     c
062240060222     C* SE ESISTE RECORD P.IVA IN FIAR4 LO AGGIORNO
062250950413     C* SOLO SE P.IVA SPROTETTA
062260950413     C     *IN05         IFEQ      *OFF
062270950413     C     *IN72         ANDEQ     *OFF
062280950413     C                   MOVEL     'P'           WTRC
062290060222     C     KARB2         CHAIN     FIAR401L                           30
062300060222     C     *IN30         IFEQ      *OFF
062310060223     c                   move      ar4not        wcpifile
062320060223     c                   if        wcpifile <> arbcpi
062330060223     c                   movel     'T'           ar4ftr
062340060223     c                   z-add     dateu         ar4dtr
062350060223     c                   z-add     dateu         ar4duv
062360060222     C                   MOVE      ARBCPI        AR4NOT
062370060222     C                   UPDATE    FIAR4000
062380060223     c                   else
062390060223     c                   unlock    fiar401l
062400060223     c                   endif
062410060222     C                   END
062420950413     C                   END
062430141215     c                   endif
062440941212     C*
062450931207    2C     VIDFFD        IFEQ      'SI'
062460950222     C                   MOVEL     'C'           ARBFIN
062470930528     C                   MOVEL     'S'           ARBFFD
062480931207   X2C                   ELSE
062490930528     C                   MOVEL     ' '           ARBFFD
062500141212     c                   if        wnoindir=' '
062510930528     C                   MOVEL     VIDFIN        ARBFIN
062520141212     c                   endif
062530931207    2C                   END
062540160318     c* Se spedizione Import dpd richiamo routine per aggiornamenti per
062550160318     c* spedizione "VEDI PACCO"
062560160318     c* (Aggiornamento del flag traduttore su fiar4 rek "I", pulizia del
062570160318     c* codice giro + memorizzaizone di togliere sped da distinta)
062580160318     c                   if        lnpntw='DPD'
062590160318     c                   exsr      sr_VediPDpd
062600160318     c                   endif
062610110905     c* se attiva procedura centro storico e c'e' il giro sulla bolla --> ricalcolo se
062620110905     c*  non è stato forzato a video
062630151230     c                   if        wnoindir=' '  and
062640151230     c                             (not *in10 or wfile='A')
062650110906     c                   if        vidffd=*blanks
062660110906     c                   if        (oldfin<>arbfin and arbfin=o95iso)
062670110906     c                             or oldffd<>vidffd
062680110906     c*****                        or oldfin=arbfin
062690110906     c
062700160318     c     karb          chain(n)  fiarg01l
062710160318     c                   if        %found(fiarg01l) and argcgi<>*blanks
062720160318     c****                         and olvstprop<>'N'
062730110905     c                   clear                   fnlvstds
062740110905     c                   eval      ilvstaas=arbaas
062750110905     c                   eval      ilvstlnp=arblnp
062760110905     c                   eval      ilvstnrs=arbnrs
062770110905     c                   eval      ilvstnsp=arbnsp
062780110905     c                   eval      ilvstmgs=arbmgs
062790110905     c                   eval      ilvstfin=arbfin
062800110905     c                   eval      ilvstcgi=argcgi
062810110906     c                   if        lnacons>0
062820110906     c                   eval      ilvstpoc=lnacons
062830110906     c                   else
062840110906     c                   eval      ilvstpoc=arblna
062850110906     c                   endif
062860110906     c                   if        arbddc>0
062870110906     c                   eval      ilvstdat=arbddc
062880110906     c                   else
062890110906     c                   eval      ilvstdat=dateu
062900110906     c                   endif
062910110905     c                   call      'FNLVSTR'
062920110905     c                   parm                    kpjba
062930110905     c                   parm                    fnlvstds
062940110906     c
062950110906     c* se aggiornato flag inoltro lo memorizzo nella variazione
062960110906     c                   if        OLVSTFINAG='S'
062970110906     c                   eval      arbfin=olvstfin
062980110905     c                   endif
062990110906     c
063000110906     c                   endif
063010110906     c                   endif
063020110906     c                   endif
063030141212     c                   endif
063040110905
063050970808     C* CODICE TASSAZIONE
063060141212     c                   if        wnoindir=' '
063070970808     C                   MOVEL     O95CTS        ARBCTS
063080141212     c                   endif
063090130606
063100031120
063110130930     c                   exsr      AggioREF
063120130930     c
063130931207    1C                   END
063140941025     C**
063150920319     C* RICALCOLO DEL VOLUME
063160941025     C**
063170941025    1C     *IN02         IFEQ      *ON
063180950114     C*
063190950114     C* SE VARIATO MITTENTE PER I PADRONCINI USO QUELLI SALVATI
063200941102     C* PULIZIA DELLA DS PER IMPOSTARE IL VOLUME DA FATTURARE VARIATO
063210030612     C                   CLEAR                   Fnlv20DS
063220941102     C*
063230941025     C* VOLUME REALE
063240041007    2C     VIDVLF        IFNE      oldVLF
063250941102     C                   MOVEL     'R'           D20FVL
063260941102     C                   Z-ADD     VIDVLF        D20VLU
063270930505     C*
063280931207   X2C                   ELSE
063290941025     C*
063300941025     C* VEDO SE DEVO MODIFICARE IL VOLUME DA FATTURARE
063310941214     C                   EXSR      VOLAUT
063320941102    2C                   ENDIF
063330941102     C**
063340941102     C* SE HO MODIFICATO IL VOLUME DA FATTURARE CALL A FNLV20R
063350941102     C*  PER DETERMINARE IL VOLUME BOLLETTATO
063360941102     C**
063370941102    2C     D20VLU        IFGT      0
063380941222     C                   MOVEL     D48TBO        D20TBO
063390941102     C                   MOVEL     'F'           D20TVL
063400041008     C                   MOVEL     arbFVB        D20FVB
063410041008     C                   MOVEL     arbVLB        D20VLB
063420041007     C                   MOVEL     oldFVF        D20FVF
063430041007     C                   MOVEL     oldVLF        D20VLF
063440030612     C                   MOVEL     Fnlv20DS      KPJBU
063450941102     C                   CALL      'FNLV20R'
063460941102     C                   PARM                    KPJBA
063470941102     C*
063480030612     C                   MOVEL     KPJBU         Fnlv20DS
063490941102     C*
063500941102     C                   MOVEL     D20FVB        ARBFVB
063510941102     C                   MOVEL     D20VLB        ARBVLB
063520941102     C                   MOVEL     D20FVF        ARBFVF
063530941102     C                   MOVEL     D20VLF        ARBVLF
063540941102    2C                   ENDIF
063550941214     C*
063560051129     C* SE DEVO SOSTITUIRE COL VOLUME VDL LO FACCIO
063570941214     C     WSOST         IFEQ      'S'
063580970418     C* WSOSC INDICA SE IL TIPO VOLUME DETERMINATO PER LA BOLLA E'
063590051129     C* SOSTITUIBILE DAL VOLUME VDL.
063600970418     C* IN CASO DI VARIAZIONE VOLUME WSOSC SARA' QUELLO DEL TIPO VOLUME
063610970418     C* "R"; IN CASO DI VARIAZIONE DI PESO WSOSC SARA' QUELLO DEL TIPO
063620970418     C* VOLUME DELLA BOLLA
063630041007     C     oldVLC        ANDGT     ARBVLF
063640970418     C     WSOSC         ANDEQ     'S'
063650041007     C                   Z-ADD     oldVLC        ARBVLF
063660970418     C                   MOVE      WFVF          ARBFVF
063670941214     C                   ENDIF
063680920319     C*
063690990628     C  N10              MOVEL     VIDPKF        ARBPKB
063700941102     C                   MOVEL     VIDPKF        ARBPKF
063710041007     c
063720041007    2C     *IN24         IFEQ      *ON
063730041007     C                   MOVEL     MITFVF        OLDFVF
063740041007     C                   MOVEL     MITVLF        OLDVLF
063750041007    2C                   ENDIF
063760941102    1C                   ENDIF
063770941212     C*
063780041007     C* Ora invio solo a sede
063790151223     c                   if        not *in10 or wfile='A'
063800070503     C     D66FFI        ifeq      'S'
063810950227     C*
063820950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
063830950413    2C     §3ABSD        IFEQ      ' '
063840170116     C***                OPEN      FNARBD2T
063850941014     C                   WRITE     FNARBDTT
063860170116     C***                CLOSE     FNARBD2T
063870950227    2C                   END
063880911212     C* DISALLOCO OGGETTO
063890170116     C****               EXSR      DLCSED
063900950208    1C                   END
063910911209     C*
063920950113     C* AGGIORNO FILE TASSAZIONE PADRONCINI
063930950113     C                   EXSR      AGGPDS
063940151223     c                   endif
063950931207     C*
063960941102     C* AGGIORNO
063970151223     C**   10
063980151223     C**AN 05              MOVEL     SAVCPI        ARBCPI
063990151223     C**  N10
064000151223     C**AN 06              MOVEL     SAVCPI        ARBCPI
064010151223     c                   if        (wfile='A' and *in05) or
064020151223     c                             (wfile='P' and *in06)
064030151223     C                   MOVEL     SAVCPI        ARBCPI
064040151223     c                   endif
064050941212     C*
064060151223     c                   if        wfile='A'
064070151223     C                   UPDATE    FNARB000
064080151223     c                   else
064090151223     C                   UPDATE    FNBLP000
064100151223     c                   endif
064110150206     c
064120160318     c* Tolgo  spedizione "VEDI PACCO DPD" da distinta
064130160318     c                   if        wtogli='1'
064140160318     c                   exsr      sr_TogliVDpd
064150160318     c                   endif
064160150206     c* memorizzo la località normalizzata
064170150206     c                   if        vidlod<>normlod OR WAR4X='E'
064180150206     C
064190150211     c                   eval      d19ftr='T'
064200150206     c                   eval      d19agg='E'
064210150203     C                   MOVEL     ARBAAS        D19AAS
064220150203     C                   MOVEL     ARBLNP        D19LNP
064230150203     C                   MOVEL     ARBNRS        D19NRS
064240150203     C                   MOVEL     ARBNSP        D19NSP
064250150203     C                   MOVEL     'X'           D19TRC
064260150206     C                   IF        VIDLOD<>NORMLOD
064270150203     C                   MOVE      normlod       D19NT1
064280150206     C                   ELSE
064290150206     C                   CLEAR                   D19NT1
064300150206     C                   ENDIF
064310150206     C                   CLEAR                   D19NT2
064320150203     c
064330150203     C                   CALL      'FNLV19R'
064340150203     C                   PARM                    Fnlv19DS
064350150206     c                   endif
064360150203     c
064370151230     c* Salto le seguenti istruzioni se
064380151223     c* dall'arrivo sono in manutenzione di una bolla in partenza ancora da
064390151223     c* partire
064400151223     c                   if        not *in10 or wfile='A'
064410070320     c
064420070320     c* Se aggiunto   Fermo Deposito, senza consegna richiesta,
064430070320     c*  imposto il blocco. conconsegna richiesta lo tolgo
064440070320     c* Se tolgo      Fermo Deposito, tolgo il relativo blocco
064450070320     c
064460070320     c* Devo richainare e aggiornare (perchè nel frattempo ho letto
064470070320     c*  altri record fi FIAR4
064480151223
064490070320    1c                   if        Wffd<> oldffd
064500070320     c                   eval      tipoEla='C'
064510070320     c                   exsr      GestAR4L
064520070320     c
064530070320     c* Se allocato pazienza (non aggiorno ma vado avanti
064540070320     c*  fermarmi in questo punto non è bello,lascerebbe la
064550070320     c*  transazione a metà)
064560070320    2c                   if        not *in91
064570070320     c
064580170113     c* Se aggiunto il fermo deposito tolgo eventuale contatto per l'appuntamento
064590170113     c                   if        wffd='S' and
064600170113     c                             (arbtc1='A' or arbtc2='A')
064610170113     c                   eval      §b4lbas=' '
064620170113     c                   endif
064630170113
064640070508    3c****               if        arbdcr=0 and wffd='S'
064650070508     c* imposto il flag telefonata in base a quanto indicato a video
064660170113     c                   if        vidtfd='SI'  or wffd=' '
064670070508     c                   clear                   §b4ltfd
064680070508     c                   else
064690070320     c                   eval      §b4ltfd='S'
064700070508     c                   endif
064710070508     c
064720070508     c****               else
064730070508     c****               clear                   §b4ltfd
064740070508    3c****               endif
064750070320     c                   eval      tipoEla='A'
064760070320     c                   exsr      GestAR4L
064770140212     c* richiamo pgm per eventuale scrittura/cancellazione file spia
064780150603     c* per aggiungere il file spia per la sped, la bolla non deve essere in stati
064790150603     c*  particolari
064800150603     c                   clear                   tnsd99ds
064810150603     c                   clear                   statofd           1
064820150603     c                   if        wffd='S'
064830150603     C                   MOVEL     'L'           D98tla
064840150603     C                   MOVEL     'A'           D98tbo
064850150603     C                   Z-ADD     arbaas        D98aas
064860150603     C                   Z-ADD     arblnp        D98lnp
064870150603     C                   Z-ADD     arbnrs        D98nrs
064880150603     C                   Z-ADD     arbnsp        D98nsp
064890150603     C*
064900150603     C                   call      'TNSD99R'
064910150603     C                   parm                    tnsd99ds
064920150603     c                   if        D98SPCDEE='FFD' or %subst(D98SPCDEE:1:2)='FD'
064930150603     c                   eval      statofd='S'
064940150603     c                   endif
064950150603     c                   endif
064960150603     c
064970150603     c*****              if        wffd=*blanks or vidtfd=*blanks
064980150603     c                   if        wffd=*blanks or (vidtfd=*blanks and
064990150603     c                             statofd='S')
065000140212     c                   clear                   trtcp2ds
065010140213     c                   if        wffd=*blanks
065020140212     c                   eval      t2chiamata='A'
065030140212     c                   endif
065040140212     c                   eval      t2chi= 'F'
065050140212     c                   eval      T2AAS=d48aas
065060140212     c                   eval      T2LNP=d48lnp
065070140212     c                   eval      T2NRS=d48nrs
065080140212     c                   eval      T2NSP=d48nsp
065090140213     c* passo in t2fgs la lna (serve per far funzionare il chiodo nel trtcp2r)
065100140213     c                   eval      T2fgs=arblna
065110140212     c                   call      'TRTCP2R'
065120140212     c                   parm                    trtcp2ds
065130140213     c                   endif
065140070320     c
065150070320    2C                   END
065160070320    1C                   END
065170070320     c
065180041007     c* Per l'altro file sistemo eventuale volume da fatturare e P.I
065190041007     c*  e aggiorno
065200041007     c                   if        waltrabolla=' '
065210041007     c   02              if        waltrofvf='T'  or
065220041007     c                             (waltrofvf='Z' and arbfvf<>'T' aND
065230041007     C                              WALTROVLF> arbvlf)
065240041008     c                   movel     waltrofvf     arbfvf
065250041008     c                   movel     waltrovlf     arbvlf
065260041007     c                   endif
065270041007     c* Se 1°bolla franco, tengo la p.i. presente nel record
065280151223     c                   if        (*in10 and *in06) or
065290151223     c                             (not *in10 and *in05)
065300041007     c                   eval      arbcpi=waltrocpi
065310041027     c                   else
065320041027     c                   eval      arbcpi=NEWCPI
065330041007     c                   endif
065340041007     c
065350041007     c* aggiornamenti di campi mirati
065360041007     C   10              except    arbdblp
065370041007     C  N10              except    arbdarb
065380041007     c                   endif
065390151223
065400151223     c                   endif
065410950221     C*
065420950221     C* DISALLOCO  I FILES CHE NON USO
065430051118     C   10              UNLOCK    FiAR901L
065440991001     C                   UNLOCK    FIAR601L
065450061116     c
065460061116     c* Se si tratta di bolla in assegnato con possibilità di scrivere il
065470061116     c*  cod fiscale, richiamo la REGIS11 per la memorizzazione
065480151230     c                   if        wnoindir=' ' and
065490151230     c                             (not *in10 or wfile='A')
065500061116     c                   if        *in01 and not *in05
065510061116     c                   if        (vidcdf=*blanks  and war4Q='D') or
065520061116     c                             vidcdf<>*blanks
065530061116     c                   eval      v11mide='D'
065540061116     c                   eval      v11cdfis=vidcdf
065550071102     c* Imposto anche il campo della partita iva di nuovo
065560071102     c                   eval      v11pid=vidpid
065570071102     c
065580061116     c                   movel     'FI'          vidcvr
065590071026     c* Non devo aggiornare la bolla perchè l'ho già fatto qui per la
065600071026     c*  partita IVA
065610130412     c                   if        v11pid<>*blanks or
065620130412     c                              %subst(v11cdfis:12:1)<> ' '
065630071026     c                   eval      Aggiobol='N'
065640130402     c                   endif
065650061116     c                   exsr      REGIS11
065660061116     c                   endif
065670061116     c                   endif
065680141215     c                   endif
065690041007     c*
065700141212     c                   if        wnoindir='S'
065710141212     c                   eval      wdisag=wdisagdest
065720141212     c                   endif
065730041007     c* Riaggancio la bolla in modifica, per controlli sul
065740041007     c*  destinatario disagiato e consegna richiesta rispetto a FFD
065750151223     c                   if        wfile='A'
065760151223     C     KARB          CHAIN(n)  FNARB01L                           30
065770151223     c                   else
065780151223     C     KARB          CHAIN(n)  FNBLP01L                           30
065790151223     c                   endif
065800070906     c* Se in destinatario non è più A D o S tolgo se non immesso
065810070906     c*  dalla partenza
065820070911     c                   if        wdisagdest<>wdisag and wdisagdest<>' ' and
065830070910    2c                             (arbtc1=wdisagdest or arbtc2=wdisagdest)
065840070906     c                   exsr      ContrTCP
065850070906     c                   endif
065860040224
065870950602     C*
065880020531     C* SE destinatario disagiato devo rifare il giro per le consegne
065890020531     c* particolari
065900041015     C***  wdisag        IFne      *blanks
065910041015     c***  arbtc1        andne     wdisag
065920041015     c***  arbtc2        andne     wdisag
065930060327     c***                If        wdisag <> *blanks and
065940060327     c***                          %subst(arbGva:1:1) <> 'U' or
065950060327     c***                          wdisag='X' and %subst(arbGva:1:1)='U'
065960060327     c* Per Disagiato ---->sempre
065970060327     c* per Supermercato ->solo se non è consegna ad uffici
065980060327     c* per Appuntamento ->solo se non è consegna ad uffici e non è
065990070917     c*                    prevista data consegna "Tassativa" da partenza
066000070917     c                   movel     arbdcr        w008a
066010070907    1c                   if        (wdisag = 'X') OR
066020060327     c                             (wdisag='S' and %subst(arbGva:1:1)<>'U') OR
066030060327     c                             (Wdisag='A' and %subst(arbGva:1:1)<>'U' and
066040070917     c                             (arbdcr=*zeros or arbtcr <> *blanks or
066050070917     c                              (par5mdp='S' and w008a<>par5gdcr)))
066060070907    2c     arbtc1        ifne      wdisag
066070041015     c     arbtc2        andne     wdisag
066080030122     c                   Eval      wI0 = 'S'
066090020531     C                   MOVEL     'CP'          D48CVB
066100070907     C                   MOVEL     'D'           D48TRC
066110070911     C***                MOVEL     'E'           WESEG
066120070911     c***                clear                   W48f12
066130070907     c***                if        *in20
066140050502     c                   eval      §bgtcr=arbtcr
066150050502     c                   eval      §bghcr=arbhcr
066160050502     c                   eval      §bgdcr=arbdcr
066170050502     c                   eval      §bggc1=arbgc1
066180050502     c                   eval      §bggc2=arbgc2
066190050502     c                   eval      §bgtc1=arbtc1
066200050502     c                   eval      §bgtc2=arbtc2
066210070907     c***                endif
066220070910     c
066230070910     c* Se da eliminare anche la consegna particolare imposto
066240070910     c*  insieme la variazione
066250070910     c                   if        eliminatcp='1'
066260070910     c                   clear                   §bgtc1
066270070911     c* ripristino la vecchia consegna partic.
066280070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
066290070911     c                   eval      §bgtc1=aggtcp
066300070911     c                   endif
066310070910     c                   endif
066320070910     c                   if        eliminatcp='2'
066330070910     c                   clear                   §bgtc2
066340070911     c* ripristino la vecchia consegna partic.
066350070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
066360070911     c                   if        §bgtc1=' '
066370070911     c                   eval      §bgtc1=aggtcp
066380070911     c                   else
066390070911     c                   eval      §bgtc1=aggtcp
066400070911     c                   endif
066410070911     c                   endif
066420070910     c                   endif
066430070911     c
066440070907     c* Se modificato destinatario in disagiato o supermercato imposto il
066450070907     c* relativo flag nelle cosegne particolari
066460070907    3c     wdisag        ifne      *blanks
066470070907     c                   select
066480070907     c     §bgtc1        wheneq    *blanks
066490070907     c                   movel     wdisag        §bgtc1
066500070907     c     §bgtc2        wheneq    *blanks
066510070907     c                   movel     wdisag        §bgtc2
066520070907     c                   endsl
066530070907    4c     §bgtc1        ifeq      §bgtc2
066540070907     c                   clear                   §bgtc2
066550070907    4c                   endif
066560070907    3c                   endif
066570070907     c* Memorizzo in skiera
066580070907    3c                   if        kk<=10
066590070910     c* se devo eliminare una consegna particolare, memorizzo nella
066600070910     c* stessa variazione
066610070910     c                   if        eliminatcp=' '
066620070907     c                   add       1             kk
066630070910     c                   endif
066640070911     c* Passo dati in DS
066650070907     c                   movel     dsarbg        kdati(KK)
066660070907     c                   movel     d48cvb        kcvb(KK)
066670070907     c                   movel     d48trc        ktrc(KK)
066680070911     c* videata si vede o meno in base al richiamo prevedente
066690070911     c                   movel     d48ffr        kffr(KK)
066700070911     c* Disabilito F12
066710070911     c                   movel     ' '           kF12(KK)
066720070907    3c                   endif
066730070907     c
066740070907    2c                   endif
066750070910     c
066760070910   x1c                   else
066770070910     c* Se non devo aggiungere delle consegne particolari,
066780070910      *  se c'è la data consegna richiesta controllo se è ancora valida
066790070910      *  solo se ho fatto una modifica in arrivo
066800070914     c* Solo se non devo aggiungere delle consegnee particolari
066810070914     c*  altrimenti ilk controllo lo faccio nella regis7
066820070914     c                   if        aggtcp=*blanks
066830071109     c
066840071109     c* Imposto il campo WGIAC in base alla giacenza
066850071109if  2c                   If        *In04 and wgcfas = 35
066860071109if  3c                   If        wgcded > *Zeros and arbdcr >= wgcded
066870071109     c                   Eval      wgiac = 'S'
066880071109    3c                   EndIf
066890071109if  3c                   If        wgcded = *Zeros and arbDcr >= wgcddm
066900071109     c                   Eval      wgiac = 'S'
066910071109    3c                   EndIf
066920071109    3c                   EndIf
066930071109     c
066940160114    2c                   If        ArbDcr > *Zeros and *In10  and wfile='A'
066950070910     c                             and wCa = *Blanks and wgiac = *Blanks
066960070910     c                   ExSr      ContrDcr
066970070910    2c                   EndIf
066980070914    2c                   EndIf
066990070907    1c                   endif
067000070907     c
067010070907     c                   clear                   eliminatcp
067020070911     c                   clear                   aggtcp
067030130411
067040130411     c* Richiamo routine per invio variazione a pda
067050160330     c                   if        wfile='A' and wtogli=*blanks
067060130411     c                   exsr      sr_pda
067070160112     c                   endif
067080911209     C*
067090911212     C     ENDRG2        ENDSR
067100130930     c
067110130930     c* Aggiorno referente -----------------------------------------------------*
067120130930     c     AggioREF      BEGSR
067130130930     c
067140130930      * Referente e Telefono e magazzino giacenza
067150130930     c                   Eval      kAr5Trd = 'GEN'
067160130930     c     kAr5          Chain     Fiar501l
067170130930      * Aggiorno Fiar5
067180130930if  2c                   If        %Found(Fiar501l)
067190130930     c                   movel     ar5uni        DAr5Gen
067200130930     c                   Eval      §Ar5Ref = VidRef
067210130930     c                   Eval      §Ar5Teld = VidTeld
067220130930     c
067230130930     c* Solo per causale I0 aggiorno anche mag giacenza e f.deposito
067240130930     c                   if        flgcvr = 'I'
067250130930     c                   Eval      §Ar5Zng = VidZng
067260130930     c
067270130930     c                   if        wffd<>oldffd
067280130930     c                   if        wffd='S'
067290130930     c                   eval      §ar5dfd=%editc(dateu:'X')
067300130930     c                   else
067310130930     c                   clear                   §ar5dfd
067320130930     c                   endif
067330130930     c                   endif
067340130930     c
067350130930     c                   endif
067360130930     c
067370130930     c                   Movel(p)  DAr5Gen       Ar5Uni
067380130930      * se devo trasmettere alla sede sfleggo per la trasmissione
067390130930     c                   If        §Ar5Trse = 'S'
067400130930     c                   Clear                   Ar5Ft2
067410130930     c                   EndIf
067420130930      * sfleggo sempre per la trasmissione al cliente
067430130930     c                   Clear                   Ar5Ft3
067440130930     c                   Update    Fiar5000
067450130930      * Scrivo Fiar5
067460130930   x2c                   Else
067470130930      * se immessi i dati
067480130930      * non mi preoccupo per il fermo deposito: il fiar5 record GEN viene sempre creato
067490130930      * in immissione bolle. Non esiste più il caso di fiar5 record GEN inesistente in fase
067500130930      * di manutenzione bolle
067510130930if  3c                   If        VidRef <> *Blanks or VidTeld <> *Blanks or
067520130930     c                             VidZng <> *Blanks
067530130930     c                   Clear                   Fiar5000
067540130930     c                   Eval      Ar5Aas = ArbAas
067550130930     c                   Eval      Ar5Lnp = ArbLnp
067560130930     c                   Eval      Ar5Nrs = ArbNrs
067570130930     c                   Eval      Ar5Nsp = ArbNsp
067580130930     c                   Eval      Ar5Trd = 'GEN'
067590130930     c                   Move      Dateu         Ar5Dac
067600130930     c                   Movel     W0140         Ar5Orc
067610130930     c                   Clear                   Dar5Gen
067620130930     c                   Eval      §Ar5Ref = VidRef
067630130930     c                   Eval      §Ar5Teld = VidTeld
067640130930     c                   Eval      §Ar5Zng = VidZng
067650130930     c                   Movel(p)  DAr5Gen       Ar5Uni
067660130930     c                   Write     Fiar5000
067670130930    3c                   EndIf
067680130930    2c                   EndIf
067690130930     c                   ENDSR
067700911210     C*
067710911210     C*--- CONTROLLI FORMATO3 ----------------------------------------*
067720911210     C     CONTR3        BEGSR
067730941209     C*
067740920319     C* CONTROLLI COMUNI CON CONTR4
067750920319     C                   EXSR      CTRCOM
067760941209     C   90              GOTO      ENDC03
067770920319     C*
067780920319     C* T A S S A Z I O N E
067790941222     C* CONTROLLO L'IMPORTO DA ASSICURARE SE SONO IN ARRIVO SOLO PER
067800941222     C*  ASSEGNATO, IN PARTENZA SEMPRE
067810941222    1C     *IN36         IFEQ      *OFF
067820941222     C     *IN10         OREQ      *OFF
067830911211     C*
067840011205     C*** I M P O R T O  D A  A S S I C U R A R E  ***
067850080701      **** OBSOLETO DAL 1 GIUGNO 2008 CHE È STATO INSTALLATO LIQUIDAZIONE TANSATTIVA AUTOMATICA ***
067860990127     C*
067870990127     C* IMPOSSIBILE INSERIRE IMPORTO DA ASSICURARE SE BOLLA SI C.A.
067880990127     C*  IN LIQ.TRANSATTIVA
067890080701    2C***  VIDIAS        IFGT      0
067900990127     C* VEDO SE C'E' C.A.
067910040809     C**""*KARB          CHAIN     FNDCT02L                           30
067920040809    3C**""**IN30         DOWEQ     *OFF
067930040809    4C**""*DCTATB        IFEQ      ' '
067940990127     C*
067950040809    5C**""*DCTFPR        IFEQ      'T'
067960040809     C**""*              MOVEL     MSG(82)       VIDMSG
067970040809     C**""*              SETON                                        30
067980040809     C**""*              SETON                                        409028
067990040809    5C**""*              ENDIF
068000040809    4C**""*              ENDIF
068010990127     C**
068020040809     C**""*  N30KARB          READE     FNDCT02L                               30
068030040809    3C**""*              ENDDO
068040040809      *
068050040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
068060040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
068070040809      * legate alla spedizione che passo
068080080701      **** OBSOLETO DAL 1 GIUGNO 2008 CHE È STATO INSTALLATO LIQUIDAZIONE TANSATTIVA AUTOMATICA ***
068090080701      ***    NON FACCIAMO PIUI' IL CONTROLLO ****
068100080701     c*                  clear                   fidn12ds
068110080701     c*                  eval      i12tla = 'L'
068120080701     c*                  eval      i12aas = D48AAS
068130080701     c*                  eval      i12lnp = D48LNP
068140080701     c*                  eval      i12nrs = D48NRS
068150080701     c*                  eval      i12nsp = D48NSP
068160080701     c*                  eval      i12fel = 'E'
068170080701     c*                  eval      i12fan = 'E'
068180040809     c*
068190080701     c*                  call      'FIDN12R'
068200080701     c*                  parm                    fidn12ds
068210040809     c*
068220080701     c*                  setoff                                       30
068230040809     c*
068240040809     c* se non ci sono errori
068250080701    3C*                  if        o12err <> ' '
068260080701     c*                  seton                                        30
068270080701     c*                  else
068280040809     c* se numero di CA trovate maggiore di zero
068290080701    4C*                  if        o12nca = 0
068300080701     c*                  seton                                        30
068310080701     c*                  else
068320080701     c*                  z-add     1             jj                2 0
068330080701    5c*                  dow       jj <= o12nca and *in30 = *off
068340080701     c*                  movea     skkey(jj)     dskey
068350040809     c*
068360040809     c* Aggancio il FNDCT01L x reperire se la CA è in "Liquidazione Transattiva"
068370080701     c*    KDCT1_C       chain     FNDCT01L
068380080701    6c*                  if        %found(FNDCT01L)
068390080701    7C*    DCTFPR        IFEQ      'T'
068400080701     C*                  MOVEL     MSG(82)       VIDMSG
068410080701     C*                  SETON                                        30
068420080701     C*                  SETON                                        409028
068430080701    7C*                  ENDIF
068440080701    6c*                  endif
068450040809     c*
068460080701     c*                  add       1             jj
068470080701    5c*                  enddo
068480040809     c*
068490040809     c                   seton                                        30
068500040809     c*
068510080701    4c*                  endif
068520080701    3c*                  endif
068530990127     C**
068540080701     C** 90              GOTO      ENDC03
068550080701    2C***                ENDIF
068560941215     C*
068570941215     C* SE IL CAMPO = '   ' PRENDO LA DIVISA DALLA NAZIONE DEL MITTENT
068580941215     C* LO EVIDENZIO A VIDEO
068590950120     C* SE VARIATO L'IMPORTO ME LO SALVO
068600950120     C     *IN97         IFEQ      *ON
068610950120     C                   MOVEL     '1'           WIN97             1
068620950120     C                   ENDIF
068630950120     C*
068640941215     C*
068650941215     C                   MOVEL     VIDVAS        WVLT
068660941215     C                   Z-ADD     VIDIAS        W0133            13 3
068670941215     C* CONTROLLO LA DIVISA
068680941215     C                   EXSR      CTRVLT
068690941215     C                   MOVEL     WVLT          VIDVAS
068700941215     C* ERRORE
068710941215     C   90              SETON                                        41
068720941215     C   90              GOTO      ENDC03
068730011009      *
068740011009      * Controllo se la divisa immessa è scaduta o no
068750011009     C     ARBDSP        IFGT      §CVDLI
068760011105     C     VIDIAS        ANDGT     0
068770011102     C                   MOVEL     MSG(102)      VIDMSG
068780011009     C                   SETON                                        419028
068790011009     C                   GOTO      ENDC03
068800011009     C                   ENDIF
068810941215     C*
068820941215     C* SE VARIATA LA VALUTA FACCIO RIDIGITARE L'IMPORTO
068830941222    2C     VIDVAS        IFNE      VARVAS
068840950120     C     WIN97         ANDEQ     *BLANKS
068850941222    3C     VIDVAS        IFNE      *BLANKS
068860941219     C     VIDIAS        ORNE      0
068870941215     C                   MOVEL     MSG(28)       VIDMSG
068880941215     C                   SETON                                        409028
068890941215     C                   GOTO      ENDC03
068900941222    3C                   ENDIF
068910941219    2C                   ENDIF
068920941215     C*
068930941215     C                   MOVEL     VIDVAS        VARVAS
068940950120     C                   CLEAR                   WIN97
068950941215     C*
068960011205
068970011205      * pulisco il flag x la forzatura importo da assicurare forzabile
068980011205     c                   if        vidias <> sav1_vidias
068990011205     c                   clear                   forzaias
069000080625     c                   clear                   forzaias2
069010011205     c                   endif
069020011205      *---------------------
069030011205  2b c                   if        vidias <> 0
069040011205     c                   eval      sav1_vidias = vidias
069050011205      * Controllo l'importo da assicurare tramite il trul22r
069060011205     c                   clear                   trul22ds
069070011205     c                   eval      i22tla = 'L'
069080011205     c                   eval      i22cbo = arbcbo
069090011205     c                   eval      i22tsp = arbtsp
069100011205     c                   eval      i22lnp = arblnp
069110011205     c                   eval      i22nzm = arbnzm
069120011205     c                   eval      i22lna = arblna
069130011205     c                   eval      i22nzd = arbnzd
069140011205     c                   movel     vidklp        i22ksc
069150011205     c                   move      vidksc        i22ksc
069160011205     c                   eval      i22ctr = vidctr
069170011205     c                   eval      i22imp = vidias
069180011205     c                   eval      i22div = vidvas
069190011205     c                   eval      i22ute = knmus
069200011205     c                   eval      i22pgm = nompgm
069210011205     c                   call      'TRUL22R'
069220011205     c                   parm                    trul22ds
069230011205      * se ho tutti i flag impostati non posso immettere l'importo
069240011205      * da assicurare
069250011205  3b c                   if        o22fmn <> *blanks and o22fx1 <> *blanks
069260011205     c                             and o22fx2 <> *blanks
069270011205     c                   eval      vidmsg = msg(79)
069280011205     c                   eval      *in40 = *on
069290011205     c                   eval      *in90 = *on
069300011205     c                   eval      *in28 = *on
069310011206     c                   goto      endc03
069320011205  3e c                   endif
069330011205      * controllo che non sia minore del limite previsto
069340011205  3b c                   if        o22fmn <> *blanks
069350011205     c                   eval      vidmsg = msg(87)
069360011205     c                   eval      *in40 = *on
069370011205     c                   eval      *in90 = *on
069380011205     c                   eval      *in28 = *on
069390011206     c                   goto      endc03
069400011205  3e c                   endif
069410011205      * controllo che non sia maggiore del limite previsto
069420011205      * non forzabile
069430011205  3b c                   if        o22fx2 <> *blanks
069440011205     c                   eval      vidmsg = msg(39)
069450011210     c                   eval      %subst (vidmsg:38:14) = (%editc(o22lx2: '4'))
069460011205     c                   eval      *in40 = *on
069470011205     c                   eval      *in90 = *on
069480011205     c                   eval      *in28 = *on
069490011206     c                   goto      endc03
069500011205  3e c                   endif
069510011205      * controllo che non sia maggiore del limite previsto
069520011205      * forzabile
069530011205  3b c                   if        o22fx1 <> *blanks and forzaias = *blanks
069540011205     c                   eval      forzaias = '1'
069550011205     c                   eval      vidmsg = msg(80)
069560011210     c                   eval      %subst (vidmsg:28:14) = (%editc(o22lx1: '4'))
069570011205     c                   eval      *in40 = *on
069580011205     c                   eval      *in90 = *on
069590011205     c                   eval      *in28 = *on
069600011206     c                   goto      endc03
069610011205  3e c                   endif
069620011205  2e c                   endif
069630011205
069640941209     C*
069650941209     C* SE HA TARIFFE, CONTROLLO
069660941209    2C     WTKS          IFEQ      'S'
069670941209     C     KTAM4         CHAIN     TNTAM000                           30
069680941209    3C     *IN30         IFEQ      *OFF
069690991028     C                   MOVEL     TAMFLO        DSTA01
069700991028     C                   MOVEL     'CV'          COD
069710991028     C                   MOVEL(P)  §TADIV        KEY
069720991028     C                   EXSR      CTABEL
069730991028     C                   MOVEL     TBLUNI        DSCV
069740011010     C                   MOVEL     §CVDEC        COMDEC
069750941209     C*
069760941215     C* PULISCO CAMPI D FORZATURA
069770011010    4C     W0133         IFNE      VARIAS
069780941209     C                   CLEAR                   DIE
069790941215     C                   CLEAR                   CIN
069800941215    4C                   END
069810020715
069820020715      * Non controlla da tariffa se obbligatorio nel caso di tipo bolla non previsto
069830020715     c     §3atb1        Lookup    tbl                                    30
069840020715
069850020715   4Ac                   If        *in30 = *On
069860941209     C*
069870941209     C* SOLO SE E' ASSICURAZIONE ANCHE PER GLI ARRIVI ( <> P )
069880941209     C*   ED E' ESPRESSO IL VALORE MERCE NELLA BOLLA
069890990818     C     KTPT          CHAIN     TITPT000                           30
069900950810    4C     *IN30         IFEQ      *ON
069910950810     C     TPTATB        ORNE      ' '
069920941222     C*
069930950810     C     *IN10         OREQ      *ON
069940941209     C     TPTAPL        ANDNE     'P'
069950941222     C*
069960941222     C     *IN10         OREQ      *OFF
069970941222     C     TPTAPL        ANDNE     'A'
069980941215     C*
069990011010     C                   Z-ADD     W0133         VARIAS
070000941215     C*
070010941215     C* TAMFIS=1 OBBLIGATORIO     FORZABILE --> CON ENTER PRENDO MAX
070020941215     C*                                         IMPORTO DA TARIFFA
070030941215    5C     TAMFIS        IFEQ      '1'
070040941215     C     CIN           ANDNE     1
070050011010     C     W0133         ANDEQ     0
070060941215     C                   MOVEL     MSG(40)       VIDMSG
070070941215     C                   SETON                                        409028
070080941215     C                   ADD       1             CIN
070090941215     C                   GOTO      ENDC03
070100941215    5C                   ENDIF
070110941215     C*
070120941215     C* TAMFIS=3 OBBLIGATORIO NON FORZABILE --> ERRORE SEMPRE
070130941215    5C     TAMFIS        IFEQ      '3'
070140011010     C     W0133         ANDEQ     0
070150941215     C                   MOVEL     MSG(42)       VIDMSG
070160941215     C                   SETON                                        409028
070170941215     C                   GOTO      ENDC03
070180941215    5C                   ENDIF
070190941215     C*
070200941215     C* TAMFIS=2 NON INSERIBILE             --> ERRORE SEMPRE
070210941215    5C     TAMFIS        IFEQ      '2'
070220011010     C     W0133         ANDNE     0
070230941215     C                   MOVEL     MSG(43)       VIDMSG
070240941215     C                   SETON                                        409028
070250941215     C                   GOTO      ENDC03
070260941215    5C                   ENDIF
070270941215     C*
070280941215     C* SE <> 0 CONTROLLO CHE NON SUPERI IL VALORE IN TARIFFA
070290950810    5C  N30TPTATB        IFEQ      ' '
070300011010     C     W0133         ANDGT     0
070310941215     C     DIE           ANDNE     1
070320941215     C     TPTVLM        ANDGT     0
070330941215     C*
070340941209     C* VALORE A COLLO
070350941215    6C     TPTFVM        IFEQ      '1'
070360991028     C     ARBNCL        MULT(H)   TPTVLM        TPTVAL           13 3
070370941215   X6C                   ELSE
070380941215    7C     TPTFVM        IFEQ      '2'
070390991028     C                   Z-ADD     TPTVLM        TPTVAL
070400941215   X7C                   ELSE
070410941212     C     ARBPKF        ADD       0,9           W0060             6 0
070420991028     C     W0060         MULT(H)   TPTVLM        TPTVAL
070430941215    7C                   END
070440941215    6C                   END
070450991028     C*
070460011205     C                   z-add     vidias        vidval           13 3
070470991028    7C     §TADIV        IFNE      VIDVAS
070480030612     C                   CLEAR                   YeurcoDS
070490991028     C                   MOVEL     VIDVAS        YECDVI
070500991028     C                   MOVEL     §TADIV        YECDVO
070510991028     C                   Z-ADD     VIDIAS        YECIMI
070520011010     C                   MOVE      COMDEC        YECDCO
070530991028     C*
070540991028     C                   CALL      'YEURCO'
070550030612     C                   PARM                    YeurcoDS
070560991028     C*
070570991028    8C     YECESI        IFEQ      ' '
070580991028     C                   Z-ADD     YECIMO        VIDVAL
070590991028    8C                   ENDIF
070600991028    7C                   ENDIF
070610941209     C*
070620991028    6C     VIDVAL        IFGT      TPTVAL
070630941209     C                   ADD       1             DIE
070640011205     c                   eval      vidmsg = msg(44)
070650941209     C                   SETON                                        409028
070660941209     C                   GOTO      ENDC03
070670941215    6C                   ENDIF
070680941209     C*
070690941215    5C                   ENDIF
070700941222    4C                   ENDIF
070710020715   4ac                   EndIf
070720941209    3C                   ENDIF
070730941209    2C                   ENDIF
070740030710
070750030710      * calcolo data limite mod. importo da assicurare
070760060201     c**   *iso          Move      ArbDsp        wdataiso
070770060201     c**                 adddur    §vpomia:*d    wdataiso
070780030722      *
070790060201     c**                 do        *hival
070800030722      * verifico se è un giorno lavorativo
070810060201     c**   *iso          move      wdataiso      ds_data
070820060201     C**                 eval      kann = ds_anno
070830060201     C**                 eval      kmes = ds_mese
070840060201     C**   kazcln        chain     azcln01l
070850060201     C**                 if        %found(azcln01l)
070860060201     C**                 if        MAT(ds_giorno) =  'F'  or
070870060201     C**                           POM(ds_giorno) =  'F'
070880030722      * se non va bene aggiungo 1 giorno
070890060201     c**                 adddur    1:*d          wdataiso
070900060201     C**                 iter
070910060201     c**                 else
070920060201     c**                 leave
070930060201     C**                 endif
070940060201     c**                 else
070950060201     c**                 leave
070960060201     C**                 endif
070970030722
070980060201     c**                 enddo
070990030710      * se viene modificato l'importo da assicurare rispetto a quanto
071000030710      * scritto sul file (stessa cosa anche x la divisa)
071010030710      * devo controllare se i gg. limite x la manutenzione sono trascorsi o meno
071020030710      * se sono già trascorsi emetto una window x richiedere la password
071030060201     c**                 If        (VidIas <> ArbIas or
071040060201     c**                           (VidVas <> ArbVas and ArbVas <> *Blanks)) and
071050060201     c**                            wdataoggi > wdataiso
071060060201     c**                           and wokpsw = *Blanks
071070060201     c**                 Exsr      Sr_Gespsw
071080060201     c** 90              GoTo      EndC03
071090060201     c**                 EndIf
071100941209     C***
071110011206     C*** V A L O R E   M E R C E   D I C H I A R A T O ***
071120941209     C* OBBLIGATORIO SE DESTINATARIO ESTERO E C'E' IN TABELLA NAZIONE
071130941209     C* IL FLAG EFTA
071140950120     C* SE HO VARIATO L'IMPORTO LO SALVO
071150950120     C     *IN93         IFEQ      *ON
071160950120     C                   MOVEL     '1'           WIN93             1
071170950120     C                   ENDIF
071180941209     C*
071190941209     C                   MOVEL     VIDVAD        WVLT
071200941209     C                   Z-ADD     VIDVMD        W0133            13 3
071210941209     C* CONTROLLO LA DIVISA
071220941209     C                   EXSR      CTRVLT
071230941209     C                   MOVEL     WVLT          VIDVAD
071240941209     C* ERRORE
071250941209     C   90              SETON                                        44
071260941209     C   90              GOTO      ENDC03
071270011010      *
071280011010      * Controllo se la divisa immessa è scaduta o no
071290011010     C     ARBDSP        IFGT      §CVDLI
071300011105     C     VIDVMD        ANDGT     0
071310011102     C                   MOVEL     MSG(102)      VIDMSG
071320011010     C                   SETON                                        449028
071330011010     C                   GOTO      ENDC03
071340011010     C                   ENDIF
071350941215     C*
071360941215     C* SE HO MODIFICATO LA VALUTA DEVO RIDIGITARE L'IMPORTO
071370941215     C* INDICATORE DI CHANGE 93 ON
071380941215    2C     VARVAD        IFNE      VIDVAD
071390950120     C     WIN93         ANDEQ     *BLANKS
071400941219     C     VIDVAD        IFNE      *BLANKS
071410941219     C     VIDVMD        ORNE      0
071420941215     C                   MOVEL     MSG(28)       VIDMSG
071430941215     C                   SETON                                        439028
071440941215     C                   GOTO      ENDC03
071450941215    2C                   ENDIF
071460941219     C                   ENDIF
071470941215     C*
071480941215     C                   MOVEL     VIDVAD        VARVAD
071490950120     C                   CLEAR                   WIN93
071500011205
071510011205      * pulisco il flag x la forzatura importo da assicurare forzabile
071520011205     c                   if        vidvmd <> sav1_vidvmd
071530011205     c                   clear                   forzavmd
071540011205     c                   endif
071550011205      *---------------------
071560011205     c                   eval      sav1_vidvmd = vidvmd
071570011205      * Controllo il valore merce tramite il trul23r
071580011205     c                   clear                   trul23ds
071590011205     c                   eval      i23tla = 'L'
071600011205     c                   eval      i23cbo = arbcbo
071610011205     c                   eval      i23tsp = arbtsp
071620011205     c                   eval      i23lnp = arblnp
071630011205     c                   eval      i23nzm = arbnzm
071640011205     c                   eval      i23lna = arblna
071650011205     c                   eval      i23nzd = arbnzd
071660011205     c                   movel     vidklp        i23ksc
071670011205     c                   move      vidksc        i23ksc
071680011205     c                   eval      i23ctr = vidctr
071690011206     c                   eval      i23imp = vidvmd
071700011206     c                   eval      i23div = vidvad
071710011205     c                   eval      i23ute = knmus
071720011205     c                   eval      i23pgm = nompgm
071730011205     c                   call      'TRUL23R'
071740011205     c                   parm                    trul23ds
071750011205      * se ho il flag del limite minimo impostato e l'importo è a zero
071760011205      * è obbligatorio
071770011206  2b c                   if        o23fmn <> *blanks and vidvmd = 0
071780011205     c                   eval      vidmsg = msg(45)
071790011205     c                   eval      *in43 = *on
071800011205     c                   eval      *in90 = *on
071810011205     c                   eval      *in28 = *on
071820011205     c                   goto      endc03
071830011206  2e c                   endif
071840011205      * controllo che non sia minore del limite previsto
071850011206  2b c                   if        o23fmn <> *blanks
071860011205     c                   eval      vidmsg = msg(104)
071870011210     c                   eval      %subst (vidmsg:37:14) = (%editc(o23lmn: '4'))
071880011205     c                   eval      *in43 = *on
071890011205     c                   eval      *in90 = *on
071900011205     c                   eval      *in28 = *on
071910011205     c                   goto      endc03
071920011206  2e c                   endif
071930011205      * controllo che non sia maggiore del limite previsto
071940011205      * non forzabile
071950011206  2b c                   if        o23fx2 <> *blanks
071960011205     c                   eval      vidmsg = msg(105)
071970011210     c                   eval      %subst (vidmsg:53:14) = (%editc(o23lx2: '4'))
071980011205     c                   eval      *in43 = *on
071990011205     c                   eval      *in90 = *on
072000011205     c                   eval      *in28 = *on
072010011205     c                   goto      endc03
072020011206  2e c                   endif
072030011205      * controllo che non sia maggiore del limite previsto
072040011205      * forzabile
072050011206  2b c                   if        o23fx1 <> *blanks and forzavmd = *blanks
072060011205     c                   eval      forzavmd = '1'
072070011205     c                   eval      vidmsg = msg(106)
072080011210     c                   eval      %subst (vidmsg:30:14) = (%editc(o23lx1: '4'))
072090011205     c                   eval      *in43 = *on
072100011205     c                   eval      *in90 = *on
072110011205     c                   eval      *in28 = *on
072120011205     c                   goto      endc03
072130011206  2e c                   endif
072140930507     C*
072150930507    1C                   END
072160950424     C*
072170950426     C*** CONTROLLI PER CLIENTE CON TARIFFA A VALORE CON FLAG = '3' ***
072180950426    1C     WTKS          IFEQ      'S'
072190950424     C                   MOVEL     VIDCTR        W001A             1
072200950426    2C     W001A         IFEQ      '4'
072210950424     C     TAMFVD        ANDEQ     '3'
072220950426     C*
072230950426    3C     VIDKLP        IFNE      VARKLP
072240950426     C     VIDKSC        ORNE      VARKSC
072250950426     C     VIDIAS        ORNE      COMIAS
072260950426     C                   SELECT
072270950426     C* FATTURA IMMEDIATA
072280950426     C* Se modificato il cod.cliente o l'importo da assicurare
072290950426     C* è necessario che la q.tà da fatturare e importi tassazione
072300950426     C* vengano azzerati + F14
072310950426     C     *IN14         WHENEQ    *ON
072320991001     C* Carico varie del video in schiera per vedere se c'è l'inoltro
072330991001     C                   MOVEL     '1'           WCKINL            1
072340991001     C                   EXSR      CARWAR
072350950426    4C     VIDQFT        IFNE      *ZEROS
072360950426     C     VIDIMV        ORNE      *ZEROS
072370950426     C     VIDPOR        ORNE      *ZEROS
072380991001     C     WINL          OREQ      '1'
072390950426     C     *INKN         OREQ      *OFF
072400950426     C                   SETON                                        429028
072410950426     C                   MOVEL     MSG(71)       VIDMSG
072420950426     C                   GOTO      ENDC03
072430950426    4C                   END
072440950426     C* SEGUE FATTURA
072450950426     C* Se modificato il cod.cliente o l'importo da assicurare
072460950426     C* è necessario che la q.tà da fatturare e tot imponibile
072470950426     C* vengano azzerati
072480950426     C     *IN13         WHENEQ    *ON
072490950426    4C     VIDQFT        IFNE      *ZEROS
072500950426     C     VIDIMV        ORNE      *ZEROS
072510950426     C                   SETON                                        429028
072520950426     C                   MOVEL     MSG(70)       VIDMSG
072530950426     C                   GOTO      ENDC03
072540950426    4C                   END
072550950426     C                   ENDSL
072560950426     C                   MOVE      VIDKLP        VARKLP
072570950426     C                   MOVE      VIDKSC        VARKSC
072580950426     C                   MOVE      VIDIAS        COMIAS
072590950426     C                   MOVE      *ZEROS        VARQFT
072600950426    3C                   END
072610950426     C*
072620950426     C*** QUANTITA' DA FATTURARE ***
072630950426     C*
072640950426     C* Errore se modificata e diversa da 0
072650950426    3C     VIDQFT        IFNE      VARQFT
072660950426    4C     VIDQFT        IFNE      *ZEROS
072670950426     C                   MOVEL     MSG(68)       VIDMSG
072680950424     C                   SETON                                        429028
072690950424     C                   GOTO      ENDC03
072700950426    4C                   END
072710950426     C* Se azzerata q.tà da fatturare gli
072720950426     C* importi della tassazione devono essere = 0
072730950426     C                   SELECT
072740950426    4C     *IN14         WHENEQ    *ON
072750991001     C* Carico varie del video in schiera per vedere se c'è l'inoltro
072760991001     C                   MOVEL     '1'           WCKINL            1
072770991001     C                   EXSR      CARWAR
072780950426    5C     VIDIMV        IFNE      *ZEROS
072790950426     C     VIDPOR        ORNE      *ZEROS
072800991001     C     WINL          OREQ      '1'
072810950426     C     *INKN         OREQ      *OFF
072820950426     C                   SETON                                        499028
072830950426     C                   MOVEL     MSG(69)       VIDMSG
072840950426     C                   GOTO      ENDC03
072850950426    5C                   END
072860950426    4C     *IN13         WHENEQ    *ON
072870950426    5C     VIDIMV        IFNE      *ZEROS
072880950426     C                   SETON                                        499028
072890950426     C                   MOVEL     MSG(72)       VIDMSG
072900950426     C                   GOTO      ENDC03
072910950426    5C                   END
072920950426    4C                   ENDSL
072930950424     C                   MOVE      VIDQFT        VARQFT
072940950426    3C                   END
072950950424     C*
072960950426    2C                   END
072970950426    1C                   END
072980950424     C*
072990050601     c* Controlli su aggiunte/aumenti di importo da assicurare
073000080625    1c                   If        (VidIas <> ArbIas and vidias > 0 or
073010050804     c                             (VidVas <> ArbVas and vidVas <> *Blanks
073020050804     c                             and Vidias>0))
073030050601     c* verifico se cliente con mandato
073040050601     c                   exsr      tassar
073050050601     c* cliente senza mandato: si può aggiungere/aumentare l'importo solo
073060050601     c* se inviata e-mail
073070050601     c* idem per cliente con mandato se il valore è oltre mandato
073080080625    2c                   if        task88 = 'S'
073090050601     c* vedo se inviata e-mail
073100050609     c                   movel     'c'           kta4trc
073110050601     c                   clear                   dta4c
073120050601     c     kta4          chain     tita430c
073130080625    3c                   if        %found(tita430c)
073140050601     c                   movel     ta4not        dta4c
073150080625    3c                   endif
073160080625    3c                   if        §ta4mail <> 'S'
073170080625    4c                   if        §utemtf=' '
073180050601     c                   seton                                        409028
073190050601     c                   eval      vidmsg=msg(135)
073200050601     c                   goto      endc03
073210080625    4c                   endif
073220080625    4c                   if        (§utemtf='I' or §utemtf='E') and
073230080625     c                             forzaias2=' '
073240080625     c                   eval      forzaias2='1'
073250080625     c                   seton                                        409028
073260080625     c                   eval      vidmsg=msg(146)
073270080625     c                   goto      endc03
073280080625    4c                   endif
073290080625    3c                   endif
073300080625    2c                   endif
073310080625    1c                   endif
073320941221     C*
073330941221     C* F14 - TASSAZIONE
073340991011     C     *INKN         IFEQ      *ON
073350991029     C* Fattura immediata SE NON CONTABILIZZATA
073360991011     C     *IN14         OREQ      *ON
073370991029     C     *IN29         ANDEQ     *OFF
073380991029     c* Prepagati NON CONTABILIZZATA
073390991011     C     *IN10         OREQ      *OFF
073400991011     C     VIDNFT        ANDGT     *ZEROS
073410991029     C     *IN29         ANDEQ     *OFF
073420980513     C                   MOVEL     '1'           WBOL              1
073430011018      * Se è una bolla di reso con varia 'N' 88888888
073440011017      * tasso la varia 'N'
073450011018     C     FLGVRN        IFEQ      '1'
073460011017      * leggo il sbfl delle varie
073470011017     C                   DO        20            NRR
073480011017     C     NRR           CHAIN     LR48S03                            32
073490011017     C     V3CSV1        IFEQ      'N'
073500011017     C     V3CVA1        ANDEQ     88888888
073510011017     C                   EXSR      TASSAN
073520011017     C  N32              UPDATE    LR48S03
073530011017     C                   ENDIF
073540011017     C                   ENDDO
073550011017     C                   ENDIF
073560011017      *
073570151020     c                   if        vidimv<>PesVolImv
073580151016     c                   clear                   PesVolPkcn
073590151016     c                   clear                   PesVolSpc
073600151016     c                   clear                   PesVolImv
073610151020     c                   endif
073620151020     c***                clear                   Tassatoimv
073630941221     C                   EXSR      TASSAZ
073640151019     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
073650151020     c***                if        vidimv>0 and vidimv<>PesVolImv
073660151020     c***                eval      PesVolPkcn=taspkcn
073670151020     c***                eval      PesVolSpc=tasspc
073680151020     c***                eval      PesVolIMV=vidimv
073690151020     c***                endif
073700990702     C* SE SI TRATTA DI UN PREPAGATO DEVO ANCHE VISUALIZZARE IL TOT
073710990702     C*  IMPONIBILE ARROTONDATO
073720990702     C     *IN23         IFEQ      *ON
073730151020     c****               eval      TassatoImv=vidimv
073740990702     C                   Z-ADD     AR6DFT        ARBDFT
073750990702     C                   EXSR      TOTFAT
073760990702     C                   Z-ADD     D04IFT        VIDIFT
073770990702     C                   Z-ADD     D04IMV        VIDIMV
073780990702     C                   Z-ADD     D04POR        VIDPOR
073790991006     C                   Z-ADD     D04POR        VARPOR
073800990702     C                   Z-ADD     D04IMV        SAVIMV
073810990702     C                   ENDIF
073820151020     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
073830151020     c                   if        vidimv>0 and vidimv<>PesVolImv
073840151020     c                   eval      PesVolPkcn=taspkcn
073850151020     c                   eval      PesVolSpc=tasspc
073860151020     c                   eval      PesVolIMV=vidimv
073870151020     c                   endif
073880131016
073890131016     c                   if        *inkn
073900131016     c                   eval      winf14='1'
073910131016     c                   endif
073920991011     C   KN
073930991011     COR 90              GOTO      ENDC03
073940990702    1C                   END
073950991006     C* Se divisa sprotetta significa che manca tariffa: in questo caso
073960991006     C* controllo che la divisa inserita sia = alla divisa della moneta
073970991006     C*  di conto SE CLIENTE 8888 o 9999
073980991006     C*
073990991006     C* Fattura immediata
074000991006     C     *IN14         IFEQ      *ON
074010991006     C* Prepagati
074020991006     C     *IN10         OREQ      *OFF
074030991006     C     VIDNFT        ANDGT     *ZEROS
074040991006     C* Divisa sprotetta
074050991006     C     *IN07         IFEQ      *OFF
074060991006     C* Divisa diversa dalla divisa della moneta di conto
074070991006     C     VIDDIV        ANDNE     §GEDCN
074080991006     C* Cliente 8888 o 9999
074090991006     C     VIDKSC        IFEQ      8888
074100991006     C     VIDKSC        OREQ      9999
074110991006     C                   SETON                                        459028
074120991006     C                   MOVEA     MSG(90)       K78
074130991006     C                   MOVEA     §GEDCN        K78(71)
074140991006     C                   MOVEA     K78           VIDMSG
074150991006     C                   GOTO      ENDC03
074160991006     C                   ENDIF
074170991006     C                   ENDIF
074180991006     C                   ENDIF
074190941102     C**
074200991124     C* SE ABBLENCATA DIVISA ERRORE SE ESISTE ALMENO UN IMPORTO DI
074210991124     C* TASSAZIONE
074220991124    1C     *IN07         IFEQ      *OFF
074230991124     C     VIDDIV        ANDNE     WDIV
074240991124     C     VIDDIV        ANDEQ     *BLANKS
074250991124    2C     VIDPOR        IFGT      *ZEROS
074260991125     C                   SETON                                        459028
074270991124     C                   MOVEL     MSG(94)       VIDMSG
074280991124     C                   GOTO      ENDC03
074290991124   X2C                   ELSE
074300991124    3C                   DO        20            NRR
074310991124     C     NRR           CHAIN     LR48S03                            30
074320991124    4C     V3CVA1        IFGT      *ZEROS
074330991125     C                   SETON                                        459028
074340991124     C                   MOVEL     MSG(94)       VIDMSG
074350991124     C                   GOTO      ENDC03
074360991124    4C                   ENDIF
074370991124    3C                   ENDDO
074380991124    2C                   ENDIF
074390991124    1C                   ENDIF
074400991005     C* PER CONTROLLARE LA TASSAZIONE RICHIAMO PGM FNLV16R
074410991012     C* SE LA DIVISA E' MODIFICATA RICHIAMO L'LV16 SOLO PER IL
074420991012     C* CONTROLLO DELLA DIVISA
074430991013    0C     *IN07         IFEQ      *OFF
074440991013     C     VIDDIV        ANDNE     WDIV
074450991026     C     WDIV          ANDNE     *BLANKS
074460100120     c
074470030612     C                   CLEAR                   Fnlv16DS
074480991012     C                   CLEAR                   DTASV
074490100120     C                   CLEAR                   dlv1601
074500100120
074510991012     C                   MOVEL     'D'           D17FIM
074520991012     C                   MOVE      VIDDIV        D17DIV
074530991012     C     VIDDFT        IFGT      *ZEROS
074540991012     C                   CLEAR                   WLBDAT
074550991012     C                   Z-ADD     VIDDFT        G02DAT
074560991012     C                   MOVEL     *BLANK        G02ERR
074570991012     C                   CALL      'XSRDA8'
074580991012     C                   PARM                    WLBDAT
074590991012     C                   Z-ADD     G02INV        D17DSP
074600991012     C                   ELSE
074610991012     C                   Z-ADD     ARBDSP        D17DSP
074620991012     C                   END
074630991012     C                   CALL      'FNLV16R'
074640030612     C                   PARM                    Fnlv16DS
074650991012     C                   PARM                    DTASV
074660991014     C                   MOVEL     D17DIV        VIDDIV
074670991012   X0C                   ELSE
074680030612     C                   CLEAR                   Fnlv16DS
074690991005     C                   CLEAR                   DTASV
074700941222     C                   MOVEL     D48TBO        D17TBO
074710991006     C   23              MOVEL     'S'           D17PRP
074720941209     C                   MOVEL     KSC           D17KSC
074730991012     C     VIDDFT        IFGT      *ZEROS
074740991012     C                   CLEAR                   WLBDAT
074750991012     C                   Z-ADD     VIDDFT        G02DAT
074760991012     C                   MOVEL     *BLANK        G02ERR
074770991012     C                   CALL      'XSRDA8'
074780991012     C                   PARM                    WLBDAT
074790991012     C                   Z-ADD     G02INV        D17DSP
074800991012     C                   ELSE
074810991012     C                   Z-ADD     ARBDSP        D17DSP
074820991012     C                   END
074830941209     C                   MOVEL     ARBLNP        D17LNP
074840941209     C                   MOVEL     ARBLNA        D17LNA
074850941209     C                   MOVEL     §3ATB1        D17TBL
074860941209     C                   MOVEL     §3ARBL        D17RBL
074870020503     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
074880100120     C                   MOVEL     §3ASVA        §D17sva
074890100120
074900100120     c* Se fattura immediata o prepagato
074910100120     c*                        controllo evetuale esenzione iva
074920100121     c                   if        *in14   or (not *in10 and vidnft>0)
074930100120     c                   Eval      §d17ctriso='S'
074940100120     c                   if        arbcpi<>*blanks
074950100120     C     'PRIVATO'     SCAN      arbcpi        posiz                    30
074960100120     c* solo numeri
074970100120     c                   select
074980100120     c                   when      not *in30 and %subst(arbcpi:1:2)>=*zeros
074990100120     c                   eval      §d17iso='IT'
075000100120     c* "PRIVATO" senza iso
075010100120     c                   when      *in30 and posiz<=3
075020100120     c                   eval      §d17iso='$$'
075030100120     c                   other
075040100120     c                   eval      §d17iso= %subst(arbcpi:1:2)
075050100120     c                   ENDSL
075060100120     c                   endif
075070100120     c                   endif
075080100120     c
075090100120     c                   Movel     Dlv1601       D17Flo
075100100120     c
075110941209     C                   Z-ADD     VIDIAS        D17IAS
075120941209     C                   Z-ADD     VIDQFT        D17QFT
075130941209     C                   Z-ADD     VIDPOR        D17POR
075140991005     C                   MOVEL     VIDDIV        D17DIV
075150991006     C                   MOVEL     WDIV          D17DVP
075160941209     C                   Z-ADD     VIDIMV        D17IMV
075170990702    1C     §3AFCA        IFEQ      1
075180941209     C                   MOVEL     'S'           D17FCA
075190990702    1C                   ENDIF
075200990702    1C* SE C'E' NUMERO E DATA FATTURA RIPASSO IL TOTALE IMPONIBILE
075210950114     C     *IN14         IFEQ      *ON
075220991005     C                   MOVEL     'E'           D17FIM
075230950114     C*
075240990702   X1C                   ELSE
075250950114     C*
075260950114     C                   MOVEL     'C'           D17FIM
075270990702    2C     VIDFIV        IFGT      0
075280950114     C     VIDNFT        ANDGT     0
075290950114     C     VIDDFT        ANDGT     0
075300991005     C                   MOVEL     'E'           D17FIM
075310990702    2C                   ENDIF
075320990702    1C                   ENDIF
075330941209     C                   MOVEL     VIDCEI        D17CEI
075340991029     C* SE PROTETTA LA TASSAZIONE, NON PASSO I DATI DELLA FATTURA
075350941209     C                   MOVEL     VIDFIV        D17FIV
075360941220     C                   MOVEL     'S'           D17FFI
075370941209     C                   MOVEL     VIDNFT        D17NFT
075380941209     C                   Z-ADD     VIDDFT        D17DFG
075390941209     C*
075400991012     C* RICHIAMO ROUTINE PER CARICAMENTO VARIE DELLA DTASV
075410991012     C                   EXSR      RDTASV
075420941209     C*
075430991005     C                   CALL      'FNLV16R'
075440030612     C                   PARM                    Fnlv16DS
075450991005     C                   PARM                    DTASV
075460941102     C*
075470991011     C                   MOVEL     D17DIV        VIDDIV
075480941209     C                   MOVEL     D17CEI        VIDCEI
075490991005     C                   MOVEL     O17DEI        DESCEI
075500991005     C                   MOVEL     O17DEI        V3DCEI
075510991006     C     D17FIM        IFEQ      'E'
075520991014     C     O17ERR        ANDEQ     *BLANKS
075530030620      * OPPURE SE BOLLA CON 1 SOLA VARIA VALORIZZO L'IMPONIBILE ANCHE SE SI TRATTA DI SEGUE FATTURA
075540030620      * SOLO SE LA VARIA ESISTE
075550040123     C**!!!§3ASVA        ORNE      *BLANKS
075560040123     C**!!!WVAR3A        ANDEQ     'S'
075570040123     C**!!!O17ERR        ANDEQ     *BLANKS
075580040123     C**!!!D17FIM        ANDEQ     'C'
075590991005     C                   MOVEL     O17IMV        VIDIMV
075600991006     C                   ENDIF
075610030620      * se bolla con singola varia e la varia non  c'è azzero l'imponibile per i segue fattura
075620030620     C     §3ASVA        IFNE      *BLANKS
075630030620     C     WVAR3A        ANDNE     'S'
075640030620     C     D17FIM        ANDEQ     'C'
075650030620     C                   Z-ADD     *ZEROS        VIDIMV
075660030620     C                   ENDIF
075670030620      ****
075680991029     C                   MOVEL     D17DFG        VIDDFT
075690941209     C* VARIE
075700990702    1C                   DO        20            NRR
075710941215     C     NRR           CHAIN     LR48S03                            30
075720941209     C                   MOVEL     VSV(NRR)      V3CSV1
075730941209     C                   MOVEL     VES(NRR)      V3CEV1
075740941215     C* SE C'E' ERRORE SU VARIA, ACCENDO INDICATORE DI POSIZIONAMENTO
075750991006    2C     O17ERV        IFEQ      NRR
075760941220     C                   SETON                                        5290
075770941215     C                   ELSE
075780941215     C                   SETOFF                                       52
075790990702    2C                   ENDIF
075800941215     C  N30              UPDATE    LR48S03
075810941215     C   30              WRITE     LR48S03
075820990702    1C                   ENDDO
075830991012    0C                   ENDIF
075840941215     C*
075850941215     C                   SETOFF                                       52
075860941102     C*  ERRORE
075870991006    1C     O17ERR        IFNE      *BLANKS
075880991006     C     O17FAS        OREQ      'S'
075890941102     C*
075900991006    2C     O17MSG        IFNE      *BLANKS
075910991006     C                   MOVEL     O17MSG        VIDMSG
075920941209     C                   SETON                                        28
075930941102    2C                   ENDIF
075940990415     C**
075950990415     C* VARIA R  MSG
075960991006    2C     O17ERR        IFEQ      '6'
075970991006     C     O17MSG        ANDEQ     MSG(85)
075980991006     C     O17FAS        OREQ      'S'
075990990416     C*
076000990415     C     COMASS        IFEQ      ' '
076010990415     C* SE MODIFICATO L'IMPORTO D'ASSICURARE E FATTURA GIA' INCASSATA O TASSATA
076020990415    3C     VIDIAS        IFNE      ARBIAS
076030990415     C     VIDVAS        ORNE      ARBVAS
076040990415     C*
076050990415     C* SE GIA' TASSATO FINO ALL'IMPONIBILE E LA DATA FATTURA > 0
076060990415    4C     AR6IMV        IFNE      *ZEROS
076070990415     C                   SETON                                        2882
076080990415     C                   SETOFF                                       90
076090990415     C* ERRORE NON BLOCCANTE:CORREGGERE  VARIA R
076100990419     C                   SELECT
076110990419    5C     ARBIAS        WHENEQ    0
076120990415     C     VIDIAS        ANDGT     0
076130990415     C                   MOVEL     MSG(83)       VIDMSG
076140990419    5C     ARBIAS        WHENGT    0
076150990419     C     VIDIAS        ANDEQ     0
076160990415     C                   MOVEL     MSG(84)       VIDMSG
076170990419     C                   OTHER
076180990419     C                   MOVEL     MSG(86)       VIDMSG
076190990419    5C                   ENDSL
076200990415    4C                   END
076210990415     C                   MOVEL     '1'           COMASS            1
076220990415    3C                   END
076230990415     C                   ELSE
076240990415     C                   SETOFF                                       9028
076250990415     C                   MOVEL     *BLANKS       VIDMSG
076260990415     C                   END
076270990415    2C                   END
076280990415     C* FATTURA
076290941102     C**
076300941102     C* ESENZIONE IVA
076310991006    2C     O17ERR        IFEQ      '1'
076320941209     C                   SETON                                        5190
076330941102    2C                   ENDIF
076340941102     C**
076350941102     C* IMPONIBILE
076360991006     C     O17ERR        IFEQ      '5'
076370941209     C                   SETON                                        5090
076380941102    2C                   ENDIF
076390941222     C* NUMERO FATTURA
076400991006     C     O17ERR        IFEQ      '2'
076410941222     C                   SETON                                        5590
076420941222    2C                   ENDIF
076430941222     C* FILIALE IVA FATTURA
076440991006     C     O17ERR        IFEQ      '3'
076450941222     C                   SETON                                        5490
076460941222    2C                   ENDIF
076470941222     C* DATA FATTURA
076480991006     C     O17ERR        IFEQ      '4'
076490941222     C                   SETON                                        5690
076500941222    2C                   ENDIF
076510991006     C* DIVISA
076520991006     C     O17ERR        IFEQ      '7'
076530991006     C                   SETON                                        4590
076540991006    2C                   ENDIF
076550991012     C* PORTO
076560991012     C     O17ERR        IFEQ      '8'
076570001116     C                   SETON                                        4990
076580991012    2C                   ENDIF
076590941102     C**
076600941220     C                   GOTO      ENDC03
076610941102    1C                   ENDIF
076620040123      * se tipo bolla 'monovaria' e la varia non è esposta o è negata
076630040123      * non posso avere altre varia con importo > 0
076640040123if  1c                   If        §3asva <> *Blanks
076650040123do  2c                   Do        20            nrr
076660040123     c     nrr           Chain     Lr48s03                            30
076670040123     c                   Eval      *In52 = *Off
076680040123if  3c                   If        wvar3a <> *Blanks  and wvar3ai = *Blanks and
076690040123     c                             V3cSv1 <> *Blanks and V3cVa1 > *Zeros
076700040123     c                   Eval      *In28 = *On
076710040123     c                   Eval      *In52 = *On
076720040123     c                   Eval      *In90 = *On
076730040123     c                   Eval      VidMsg = Msg(127)
076740040123     c                   Eval      %subst(VidMsg:75:1) = §3asva
076750040123    3c                   EndIf
076760040123if  3c                   If        wvar3a = *Blanks  and
076770040123     c                             V3cSv1 <> *Blanks and V3cVa1 > *Zeros
076780040123     c                   Eval      *In28 = *On
076790040123     c                   Eval      *In52 = *On
076800040123     c                   Eval      *In90 = *On
076810040123     c                   Eval      VidMsg = Msg(127)
076820040123     c                   Eval      %subst(VidMsg:75:1) = §3asva
076830040123    3c                   EndIf
076840040123     c  n30              Update    Lr48s03
076850040127     c   90              Goto      Endc03
076860040123    2c                   EndDo
076870040123    1c                   EndIf
076880991006     C*
076890991006     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
076900991006    1C  N07VIDDIV        IFNE      WDIV
076910991006     C* CONVERSIONE PORTO
076920991006    2C     VIDPOR        IFEQ      VARPOR
076930991006     C     VIDPOR        ANDGT     *ZEROS
076940991006     C     WDIV          ANDNE     *BLANKS
076950030612     C                   CLEAR                   YeurcoDS
076960991006     C                   MOVEL     WDIV          YECDVI
076970991006     C                   Z-ADD     VIDPOR        YECIMI
076980991006     C                   MOVEL     VIDDIV        YECDVO
076990991006     C     O17FDC        IFEQ      'S'
077000011114     C                   MOVE      '3'           YECDCO
077010991006     C                   ENDIF
077020991006     C                   CALL      'YEURCO'
077030030612     C                   PARM                    YeurcoDS
077040991006     C                   Z-ADD     YECIMO        VIDPOR
077050991012     C                   SETON                                        49
077060991006    2C                   END
077070991006     C* CONVERSIONE VARIE
077080991006    2C                   DO        20            NRR
077090991006     C     NRR           CHAIN     LR48S03                            30
077100991006    3C     V3CVA1        IFEQ      VARVA1
077110991006     C     V3CVA1        ANDGT     *ZEROS
077120991006     C     WDIV          ANDNE     *BLANKS
077130011019      *
077140011019      * Se la bolla non è tassata fino all'imponibile e c'è la varia 'N' 88888888
077150011019      * non devo convertire la varia 'N' 88888888
077160011019     C     VIDIMV        IFEQ      *ZEROS
077170011019     C     FLGVRN        ANDEQ     '1'
077180011019     C     V3CSV1        ANDEQ     'N'
077190011019     C     V3CVA1        ANDEQ     88888888
077200011019     C  N30              UPDATE    LR48S03
077210011019     C                   ITER
077220011019     C                   ENDIF
077230030624      * Se la bolla non è tassata fino all'imponibile e c'è la varia '&' 88888888
077240030624      * non devo convertire la varia '&' 88888888
077250030624     C     VIDIMV        IFEQ      *ZEROS
077260030624     C     V3CSV1        ANDEQ     '&'
077270030624     C     V3CVA1        ANDEQ     88888888
077280030624     C  N30              UPDATE    LR48S03
077290030624     C                   ITER
077300030624     C                   ENDIF
077310011019      *
077320030612     C                   CLEAR                   YeurcoDS
077330991006     C                   MOVEL     WDIV          YECDVI
077340991006     C                   Z-ADD     V3CVA1        YECIMI
077350991006     C                   MOVEL     VIDDIV        YECDVO
077360991006     C     O17FDC        IFEQ      'S'
077370011114     C                   MOVE      '3'           YECDCO
077380991006     C                   ENDIF
077390991006     C                   CALL      'YEURCO'
077400030612     C                   PARM                    YeurcoDS
077410991006     C                   Z-ADD     YECIMO        V3CVA1
077420991012     C                   SETON                                        53
077430991006    3C                   ENDIF
077440991006     C                   Z-ADD     V3CVA1        VARVA1
077450991006     C  N30              UPDATE    LR48S03
077460991006    2C                   ENDDO
077470991006     C* SE TOTALE IMPONIBILE E' IMPOSTATO LO AZZERO E RICHIAMO
077480991006     C* FNLV16R PER RICALCOLARLO IN BASE ALLA NUOVA DIVISA
077490991006     C     VIDIMV        IFGT      *ZEROS
077500991006     C     WDIV          ANDNE     *BLANKS
077510991006     C                   MOVEL     'T'           D17FIM
077520991006     C                   MOVEL     *ZEROS        D17IMV
077530991006     C                   Z-ADD     VIDPOR        D17POR
077540991012     C                   CLEAR                   DTASV
077550991012     C                   EXSR      RDTASV
077560991006     C                   CALL      'FNLV16R'
077570030612     C                   PARM                    Fnlv16DS
077580991006     C                   PARM                    DTASV
077590991006     C                   Z-ADD     O17IMV        VIDIMV
077600991006     C                   ENDIF
077610991006     C                   Z-ADD     VIDPOR        VARPOR
077620991006     C                   MOVEL     VIDDIV        WDIV
077630991012     C* SE VARIATA DIVISA RIEMETTO IN OGNI CASO LA VIDEATA
077640991029     C* PERCHE' L'LV16 DOVRA' CONTROLLARE LA TASSAZIONE
077650991012     C                   SETON                                        90
077660991012     C                   GOTO      ENDC03
077670991006   X1C                   ELSE
077680991006     C* SE NON VARIATA LA DIVISA DEVO COMUNQUE MEMORIZZARE EVENTUALI
077690991006     C* VARIAZIONI EFFETTUATE SUGLI IMPORTI DI TASSAZIONE
077700991006     C                   Z-ADD     VIDPOR        VARPOR
077710991006    2C                   DO        20            NRR
077720991006     C     NRR           CHAIN     LR48S03                            30
077730991006     C                   Z-ADD     V3CVA1        VARVA1
077740991006     C  N30              UPDATE    LR48S03
077750991006    2C                   ENDDO
077760991006    1C                   ENDIF
077770941102     C*
077780941102     C* PER COD 8888 O 9999 CON VARIA "R" OBBLIGATORIO IMPORTO DA ASSIC
077790941209     C* SE C'E' "S" IN D17FAS INDICA QUESTO TIPO DI ERRORE
077800991006    3C     O17FAS        IFEQ      'S'
077810941102     C*
077820941102     C* SE = 0 E FORZO CON ENTER PRENDO VALORE DA TARIFFA
077830990702    2C     SEI           IFNE      1
077840941209     C                   MOVEL     MSG(46)       VIDMSG
077850941209     C                   SETON                                        5290
077860941102     C                   ADD       1             SEI
077870941102     C                   GOTO      ENDC03
077880990702    2C                   ENDIF
077890990702    1C                   ENDIF
077900941222     C*
077910941222     C* IN PARTENZA SE C'ERA PREPAGATO, DEVO LASCIARLO
077920990702    1C     *IN10         IFEQ      *OFF
077930990702    2C     AR6NFT        IFNE      0
077940941222     C     VIDNFT        ANDEQ     0
077950941222     C                   SETON                                        559028
077960941222     C                   MOVEL     MSG(60)       VIDMSG
077970941222     C                   GOTO      ENDC03
077980941222    2C                   ENDIF
077990990702     C*
078000990702     C* SE PREPAGATO INSERIRE TASSAZIONE O F14 PER TASSARE
078010990702     C  NKL
078020990702    2CANNKN*IN23         IFEQ      *ON
078030990702    3C     VIDIMV        IFEQ      0
078040990702     C                   SETON                                        529028
078050990702     C                   MOVEL     MSG(89)       VIDMSG
078060990702     C                   GOTO      ENDC03
078070990702   X3C                   ELSE
078080990702    4C     VIDIMV        IFNE      SAVIMV
078090990702     C                   Z-ADD     AR6DFT        ARBDFT
078100990702     C                   EXSR      TOTFAT
078110990702     C                   Z-ADD     D04IFT        VIDIFT
078120990702     C                   Z-ADD     D04IMV        VIDIMV
078130990702     C                   Z-ADD     D04POR        VIDPOR
078140991006     C                   Z-ADD     D04POR        VARPOR
078150990702     C                   Z-ADD     VIDIMV        SAVIMV
078160990702     C                   SETON                                        90
078170990702    4C                   ENDIF
078180990702    3C                   ENDIF
078190990702    2C                   ENDIF
078200990702    1C                   ENDIF
078210011018      * Se è bolla di reso con varia 'N' 88888888 deve esserci la varia 'N'
078220011017     C                   SETOFF                                       52
078230011018    1C     FLGVRN        IFEQ      '1'
078240011017    2C                   DO        20            NRR
078250011017     C     NRR           CHAIN     LR48S03                            32
078260011017    3C     *IN32         IFEQ      *OFF
078270011018     C                   SETON                                        52
078280011018    4C     V3CSV1        IFEQ      'N'
078290011017     C                   SETOFF                                       52
078300011017     C                   LEAVE
078310011018    4C                   ENDIF
078320011017    3C                   ENDIF
078330011017    2C                   ENDDO
078340011018    2C     *IN52         IFEQ      '1'
078350011017     C     1             CHAIN     LR48S03                            32
078360011017     C  N32              UPDATE    LR48S03
078370011017     C                   MOVEL     MSG(101)      VIDMSG
078380011105     C                   SETON                                        9028
078390011017     C                   GOTO      ENDC03
078400011017    2C                   ENDIF
078410011017    1C                   ENDIF
078420011206      *
078430011206      * Controllo che la somma degli importi immessi non superi il valore
078440011206      * impostato in §GEIMF
078450011206      *      prima controllo il totale imponibile
078460011206  1b c                   if        vidimv <> 0
078470011206     c                   eval      w0173 = vidimv
078480011206      *      controllo se devo convertire il totale imponibile
078490011206  2b c                   if        viddiv <> §gedcn
078500030612     c                   clear                   YeurcoDS
078510011206     c                   eval      yecdvi = viddiv
078520011206     c                   eval      yecdvo = §gedcn
078530011206     c                   eval      yecimi = vidimv
078540011206     c                   move      decsav        yecdco
078550011206     c                   call      'YEURCO'
078560030612     c                   parm                    YeurcoDS
078570011206  3b c                   if        yecesi = ' '
078580011206     c                   eval      w0173 = yecimo
078590011206  3e c                   endif
078600011206  2e c                   endif
078610011206      *      controllo se l'imponibile supera il limite fatturabile
078620011220  2b c                   if        w0173 > imfsav
078630011206     c                   eval      vidmsg = msg(107)
078640011220     c                   eval      %subst (vidmsg:54:14) = (%editc(imfsav: '4'))
078650011206     c                   eval      %subst (vidmsg:69:3) = §gedcn
078660011206     c                   eval      *in50 = *on
078670011206     c                   eval      *in90 = *on
078680011206     c                   eval      *in28 = *on
078690011206     c                   goto      endc03
078700011206  2e c                   endif
078710011206  1x c                   else
078720011206      *      altrimenti controllo tutti gli altri importi
078730011206     c                   eval      tot_importi = vidpor
078740011206      *      leggo le varie
078750011206  2b c                   do        20            nrr
078760011206     c     nrr           chain     lr48s03                            32
078770011206  3b c                   if        *in32 = *off and v3cva1 <> 0
078780011206  4b c                   if        flgvrn = '1' and v3cva1 = 88888888
078790011206     c                             and v3csv1 = 'N'
078800011206     c                   iter
078810011206  4e c                   endif
078820030624  4b c                   if        v3cva1 = 88888888
078830030624     c                             and v3csv1 = '&'
078840030624     c                   iter
078850030624  4e c                   endif
078860011206     c                   eval      tot_importi = tot_importi + v3cva1
078870011206  3e c                   endif
078880011206  2e c                   enddo
078890011206     c                   eval      w0173 = tot_importi
078900011206      *      controllo se devo convertire il totale importi
078910011206  2b c                   if        viddiv <> §gedcn
078920030612     c                   clear                   YeurcoDS
078930011206     c                   eval      yecdvi = viddiv
078940011206     c                   eval      yecdvo = §gedcn
078950011206     c                   eval      yecimi = tot_importi
078960011206     c                   move      decsav        yecdco
078970011206     c                   call      'YEURCO'
078980030612     c                   parm                    YeurcoDS
078990011206  3b c                   if        yecesi = ' '
079000011206     c                   eval      w0173 = yecimo
079010011206  3e c                   endif
079020011206  2e c                   endif
079030011206      *      controllo se l'imponibile supera il limite fatturabile
079040011220  2b c                   if        w0173 > imfsav
079050131016     c                   eval      vidmsg = msg(107)
079060011220     c                   eval      %subst (vidmsg:54:14) = (%editc(imfsav: '4'))
079070011206     c                   eval      %subst (vidmsg:69:3) = §gedcn
079080011206     c                   eval      *in50 = *on
079090011206     c                   eval      *in90 = *on
079100011206     c                   eval      *in28 = *on
079110011206     c                   goto      endc03
079120011206  2e c                   endif
079130011206  1e c                   endif
079140131016     c
079150131016     c* CONTROLLI PER BOLLA CONTABILIZZATA E PGM NON RICHIAMATO (29 ON)
079160131016     c* se f6=conferma
079170131016     c                   if        *in29 and *inkf
079180131016     c* Se eseguito F14=Tassazione --> errore bolla contabilizzata
079190131016     c                   if        winf14='1'
079200131016     c                   eval      vidmsg = msg(151)
079210131016     c                   eval      *in90 = *on
079220131016     c                   eval      *in28 = *on
079230131016     c                   goto      endc03
079240131016     c                   endif
079250131016     c* Se non variato nè ias nè vmd -> errore premere F12
079260131016     c                   if        vidias=bol_vidias and
079270131016     c                             (vidvas=bol_vidvas or vidias=0) and
079280131016     c                             vidvmd=bol_vidvmd and
079290131016     c                             (vidvad=bol_vidvad or vidvmd=0)
079300131016     c                   eval      vidmsg = msg(152)
079310131016     c                   eval      *in90 = *on
079320131016     c                   eval      *in28 = *on
079330131016     c                   goto      endc03
079340131016     c                   endif
079350131016     c                   endif
079360911210     C*
079370941209     C     ENDC03        ENDSR
079380030710      *---------------------------------------------------------------*
079390030710      * Gestione videata password x importo da assicurare             *
079400030710      *---------------------------------------------------------------*
079410060201     c**   Sr_Gespsw     BegSr
079420060201     C**
079430060201     c**                 Clear                   w01psw
079440060201     c**                 Do        *Hival
079450060201     c**                 Exfmt     lr48w01
079460060201     c**                 Eval      *In28 = *Off
079470060201     c**                 If        *InKl
079480060201     c**                 Eval      *In90 = *On
079490060201     c**                 Leave
079500060201     c**                 EndIf
079510060201     c**                 If        w01psw = *Blanks
079520060201     c**                 Eval      w01msg = 'Immettere la password'
079530060201     c**                 Eval      *In28 = *On
079540060201     c**                 Iter
079550060201     c**                 EndIf
079560060201     c**                 If        w01psw <> miapsw
079570060201     c**                 Eval      w01msg = 'Password errata'
079580060201     c**                 Eval      *In28 = *On
079590060201     c**                 Iter
079600060201     c**                 EndIf
079610060201     c**                 Eval      wokpsw = '1'
079620060201     c**                 Leave
079630060201     c**                 EndDo
079640060201     C**
079650060201     c**                 Endsr
079660920319     C*
079670930505     C* CONTROLLO COMUNI  ALL CONTR3 3 CONTR 4 -----------------------*
079680920319     C     CTRCOM        BEGSR
079690920319     C                   SETOFF                                       90
079700030620     C                   CLEAR                   SAV_INDSIN
079710941103     C**
079720920319     C* CODICE CLIENTE
079730941103     C**
079740950104     C*               SALTO CONTROLLO SE SPEDIZIONE IN FRANCO  IN ARRIV
079750950104     C*               LO ESEGUO SE SONO IN PARTENZA
079760941103    1C     *IN36         IFEQ      *OFF
079770941222     C     *IN10         OREQ      *OFF
079780941103     C*
079790941103     C* F7=RICERCA ALFABETICA
079800941103    2C   KG              DO
079810941212     C                   MOVEL     VIDDSA        PARDUT           30
079820920319     C                   MOVEL     *BLANKS       DESCDS           48
079830920319     C                   MOVEL     DESKSC        DESCDS
079840920319     C* PARSTA=9 ESCLUDO ANNULLATI
079850920319     C                   Z-ADD     9             PARSTA
079860060131     C                   Z-ADD     dutkci        CCC               4 0
079870941103    3C     VIDKLP        IFEQ      0
079880000510     C   10              MOVEL     ARBLNA        PARKSM
079890941103     C                   ELSE
079900000510     C                   MOVEL     VIDKLP        PARKSM
079910941103    3C                   ENDIF
079920991112     C                   CLEAR                   PARDIT
079930000510     C                   Z-ADD     1             PARNUM
079940941103     C*
079950000510     C                   CALL      'XALFA3BR'
079960920319     C                   PARM                    PARDUT
079970920319     C                   PARM                    CODUT
079980920319     C                   PARM                    DESCDS
079990920319     C                   PARM                    CCC
080000920319     C                   PARM                    PARSTA            1 0
080010961016     C                   PARM                    PARFLR           90
080020991112     C                   PARM                    PARDIT            3
080030000510     C                   PARM                    PARNUM            2 0
080040000510     C                   PARM                    PARKCM           80
080050000510     C                   PARM                    PARKSM          140
080060000510     C                   PARM                    PARKDM           60
080070941103    3C     PARSTA        IFEQ      -1
080080920319     C*
080090941103    4C     FLGCVR        IFEQ      '2'
080100941212     C*
080110990930     C     WAR62         IFEQ      ' '
080120941212     C                   MOVEL     ARBLNA        VIDKLP
080130941212     C                   MOVEL     9999          VIDKSC
080140941212     C                   ELSE
080150991007     C                   MOVEL     AR6KSC        VIDKLP
080160991007     C                   MOVE      AR6KSC        VIDKSC
080170941212     C                   ENDIF
080180941212     C*
080190941103   X4C                   ELSE
080200920319     C                   MOVEL     ARBKSC        VIDKLP
080210920319     C                   MOVE      ARBKSC        VIDKSC
080220941103    4C                   END
080230920319     C*
080240941103   X3C                   ELSE
080250020305     C                   MOVEL     PARKSM        ACOKSC
080260920319     C                   MOVEL     ACOKSC        VIDKLP
080270920319     C                   MOVE      ACOKSC        VIDKSC
080280970814     C                   CLEAR                   VIDRSM
080290970814     C                   CLEAR                   VIDINM
080300970814     C                   CLEAR                   VIDCAM
080310970814     C                   CLEAR                   VIDLOM
080320970814     C                   CLEAR                   VIDPRM
080330970814     C                   CLEAR                   VIDNZM
080340970814     C                   CLEAR                   VIDPIM
080350941103    3C                   END
080360941103     C*
080370941103     C                   SETON                                        9047
080380941103     C                   GOTO      ENDCOM
080390941212    2C                   ENDDO
080400920319     C* CONTROLLO
080410941103    2C     VIDKLP        IFEQ      0
080420941103     C                   MOVEL     MSG(33)       VIDMSG
080430941103     C                   SETON                                        469028
080440941103     C                   GOTO      ENDCOM
080450941103    2C                   END
080460980722     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE DELLO STESSO S.I. IN
080470991112     C*  CUI SONO  PER LE FATTURE IMMEDIATE
080480040930     C     VIDKLP        CHAIN     AZORG01L                           30
080490040930     c*
080500020725     C     D66BFI        IFEQ      'S'
080510020725     C**N30ORGDIT        IFNE      O50PRA
080520020725     C**                 MOVEL     MSG(81)       VIDMSG
080530020725     C**                 SETON                                        469028
080540020725     C**                 GOTO      ENDCOM
080550020725     C**                 ENDIF
080560020508     c* sempre per le fatture immediate se cliente 9999 il p.o. cliente
080570020508     c* deve essere = alla linea di arrivo della bolla
080580020508     c     vidksc        ifeq      9999
080590020508     c     vidklp        andne     arblna
080600020508     C                   MOVEL     MSG(35)       VIDMSG
080610020508     C                   SETON                                        479028
080620020508     C                   GOTO      ENDCOM
080630020508     c                   endif
080640020306     C                   ENDIF
080650020306     c* se variazione di mittente e cliente 8888 il p.o. cliente deve
080660020306     c* essere = alla linea di partenza bolla
080670020306     c     flgcvr        ifeq      'M'
080680020306     c     vidksc        andeq     8888
080690020306     c     vidklp        andne     arblnp
080700020306     C                   MOVEL     MSG(35)       VIDMSG
080710020306     C                   SETON                                        479028
080720020306     C                   GOTO      ENDCOM
080730020306     c                   endif
080740020508     c* se variazione di bolla in partenza e cliente 9999 errore
080750020508     c     *in10         ifeq      *off
080760020508     c     vidksc        andeq     9999
080770020508     C                   MOVEL     MSG(111)      VIDMSG
080780020508     C                   SETON                                        479028
080790020508     C                   GOTO      ENDCOM
080800020508     c                   endif
080810930507     C*
080820920319     C                   MOVEL     VIDKLP        KSC
080830920319     C                   MOVE      VIDKSC        KSC
080840000510     C  N30              MOVEL     ORGDE3        OG143
080850020318     C  N30              MOVEL     §OGNTW        CLINTW
080860020318     C   30              CLEAR                   CLINTW
080870040930     c* non accetto clienti con ntw = "LOG" o "XXX'
080880040930     c                   if        clintw='LOG'  or clintw='XXX'
080890040930     C                   MOVEL     MSG(130)      VIDMSG
080900040930     C                   SETON                                        469028
080910040930     C                   GOTO      ENDCOM
080920040930     c                   endif
080930051220     c* non è possibile modificare cliente da dpd a non dpd e viceversa
080940051220     c     flgcvr        ifeq      'M'
080950051220     c                   if        (clintw = 'DPD' and clintw <> memntw) or
080960051220     c                             (memntw = 'DPD' and clintw <> memntw)
080970051220     C                   MOVEL     MSG(137)      VIDMSG
080980051220     C                   SETON                                        469028
080990051220     C                   GOTO      ENDCOM
081000051220     c                   endif
081010051220     c                   endif
081020941103     C**
081030920319     C*
081040941103     C     KACO          CHAIN     CNACO000                           30
081050941103    3C  N30ACOFLG        IFNE      ' '
081060920319     C                   SETON                                        30
081070941103    3C                   END
081080941103     C* ERRORE
081090941103    3C     *IN30         IFEQ      *ON
081100941103     C     VIDKSC        OREQ      8888
081110941222     C     *IN10         ANDEQ     *ON
081120941223     C     VIDKSC        OREQ      9999
081130941223     C     *IN36         ANDEQ     *ON
081140941103     C                   MOVEL     MSG(36)       VIDMSG
081150941103     C                   SETON                                        479028
081160941103     C                   GOTO      ENDCOM
081170941103    3C                   ENDIF
081180930507     C*
081190130315    3C**   ACOABL        IFEQ      '8'
081200130315    3C**   ACOABL        OREQ      '*'
081210130315     c
081220130315    3C     ACOABL        ifne      *blanks
081230130418     c     ksc           andne     kscbolla
081240040930     C                   MOVEL     mSG(37)       VIDMSG
081250941103     C                   SETON                                        479028
081260941103     C                   GOTO      ENDCOM
081270941103    3C                   END
081280930507     C*
081290920319     C* SE SEGUE FATTURA NON SI PUO' METTERE COD 9999
081300941103    3C   13VIDKSC        IFEQ      9999
081310941103     C                   MOVEL     MSG(38)       VIDMSG
081320941103     C                   SETON                                        479028
081330941103     C                   GOTO      ENDCOM
081340941103    3C                   END
081350930507     C*
081360970822     C     VIDKSC        IFNE      9999
081370970822     C     VIDKSC        ANDNE     8888
081380970822     C                   MOVEL     ACORAG        DESKSC
081390941103    2C                   END
081400941220     C*
081410941223     C* SALVO ULTIMO CODICE IMPOSTATO
081420941220    2C     COMKSC        IFNE      KSC
081430941220     C                   CLEAR                   CIN
081440941220     C                   CLEAR                   DIE
081450941220     C                   MOVEL     KSC           COMKSC            7 0
081460941220    2C                   END
081470941103    1C                   END
081480030620     C* CERCO IN CNIND IL DIRITTO DI FATTURAZIONE
081490030620     C*
081500030620     C     KACO          CHAIN     CNIND00F                           30
081510030620    3C                   IF        %FOUND(CNIND00F)
081520030620     C                   EVAL      SAV_INDSIN = INDSIN
081530030620     C                   ELSE
081540030620     C                   CLEAR                   SAV_INDSIN
081550030620     C                   ENDIF
081560941103     C**
081570941215     C* CARICO CODICI TARIFFA DEL CLIENTE ED ESEGUO CONTROLLO
081580941103     C**
081590930507     C                   EXSR      CARCTR
081600941209     C*
081610941209     C* SE HO RICERCATO LA TARIFFA, RIEMETTO LA VIDEATA
081620050202    1C     ilv59RIC      IFEQ      'S'
081630941223     C                   SETON                                        9048
081640941209     C                   GOTO      ENDCOM
081650941209    1C                   ENDIF
081660941209     C*
081670941215     C* ATTIVATO IL "?" SUL CODICE TARIFFA
081680050202    1C     olv59ERR      IFEQ      'R'
081690050202     C                   MOVEL     olv59CTR      VIDCTR
081700050202     C                   MOVEL     olv59DFS      DE2CTR
081710941223     C                   SETON                                        9048
081720941209     C                   GOTO      ENDCOM
081730941209    1C                   ENDIF
081740941209     C* ERRORE
081750050202    1C     olv59MSG      IFNE      *BLANKS
081760060606     c* Nella videata della variazione del cod mittente, non do errore
081770060606     c*  se premuto F10 per la modifica del codice tariffa
081780060606     c*  ma disabilito F12
081790060606     c                   if        *inkj and flgcvr='M'
081800060606     c                   clear                   w48f12
081810060606     c                   else
081820050202     C                   MOVEL     olv59MSG      VIDMSG
081830941223     C                   SETON                                        904828
081840941209     C                   GOTO      ENDCOM
081850941209    1C                   ENDIF
081860060606    1C                   ENDIF
081870930507     C*
081880941209     C                   MOVEL     VIDCTR        COMCTR
081890050202     C                   MOVEL     olv59PRG      COMPRG
081900021121
081910160225      * Per bolla FedEx
081920160225If  1c                   If        lnantW = 'FED'
081930160225     c
081940021122      * --> se cliente 8888
081950160225      *     il CTR a video deve essere uno delle tariffe fedex
081960021122If  2c                   If        VidKsc = 8888
081970021122      * ---> e O27Fie = 'M'
081980021122If  3c                   If        WFie =  'M'
081990021122      * la tariffa deve essere quella della cartello
082000160225If  4c                   If        VidCtr <> FedM_ctr
082010021122     c                   Eval      *In48 = *On
082020021122     c                   Eval      *In28 = *On
082030021122     c                   Eval      *In90 = *On
082040021122     c                   Movel     Msg(114)      VidMsg
082050021122     c                   Goto      EndCom
082060021122    4c                   EndIf
082070021122      * ---> e O27Fie non è = 'M'
082080021122   x3c                   Else
082090160225    4c                   if        Vidctr <> FedD_ctr
082100021121     c                   Eval      *In48 = *On
082110021121     c                   Eval      *In90 = *On
082120021121     c                   Eval      *In28 = *On
082130021121     c                   Movel     Msg(114)      VidMsg
082140021121     c                   Goto      EndCom
082150021122    4c                   EndIf
082160021122    3c                   EndIf
082170160225     c
082180021121      * --> se cliente codificato
082190021121   x2c                   Else
082200021121      * ---> e non sono state trovate tariffe
082210021121If  3c                   If        Wtks <> 'S'
082220021121      *      non posso accettare una tariffa documenti se il peso della
082230021121      *      bolla superare il limite previsto in tabella "DFE"
082240021121     c                   Move      Vidctr        DueCtr
082250021121If  4c                   If        ArbPkf > §DfePkgD and
082260021121     c                             DueCtr >= §DfeDalD and DueCtr <= §DfeAlD
082270021121     c                   Eval      *In48 = *On
082280021121     c                   Eval      *In90 = *On
082290021121     c                   Eval      *In28 = *On
082300021121      * Preparo il msg
082310021121     c                   Movea     Msg(115)      K78
082320021121     c                   Movel     §DfePkgD      W006a
082330021121     c                   Movea     W006a         Ski(10)
082340021121     c                   Z-Add     10            i
082350021121     c                   ExSr      Abbl
082360021121     c                   Movea     Ski(10)       K78(59)
082370021121     c                   Move      §DfePkgD      W001a
082380021121     c                   Movel     W001a         K78(66)
082390021121     c                   Movea     K78           VidMsg
082400021121     c                   Goto      EndCom
082410021121    4c                   EndIf
082420021121    3c                   EndIf
082430021121    2c                   EndIf
082440021121    1c                   EndIf
082450920319     C*
082460941103     C     ENDCOM        ENDSR
082470011017     C*
082480011017      *--- TASSAZIONE VARIA 'N' --------------------------------------*
082490011017     C     TASSAN        BEGSR
082500011017      *
082510011017     C                   CLEAR                   DTAS
082520011017     C                   CLEAR                   DTASV
082530011017     C                   MOVEL     VIDKLP        TASKSC
082540011017     C                   MOVE      VIDKSC        TASKSC
082550011017     C                   MOVEL     VIDCTR        TASCTR
082560011017     C                   Z-ADD     ARBPKF        TASPKF
082570160530     C                   Z-ADD     ARBPKb        TASPKb
082580011017     C                   MOVEL     ARBLNP        TASLNP
082590011017     C                   MOVEL     ARBCTS        TASCTS
082600011017     C                   MOVEL     ARBDSP        TASDSP
082610011017     C                   MOVEL     ARBDSP        TASDCT
082620011017     C                   MOVEL     ARBVLF        TASVLF
082630160530     C                   MOVEL     ARBVLc        TASVLc
082640160530     C                   MOVEL     ARBncr        TASncr
082650160530     C                   MOVEL     ARBVLF        TASVLF
082660160530     C                   MOVEL     ARBfvf        TASfvF
082670011017     C                   MOVEL     ARBTSP        TASTSP
082680011017     C     WBOL          IFEQ      '1'
082690011017     C                   MOVEL     §3ATB1        TASTBL
082700011017     C                   ELSE
082710011017     C                   MOVEL     §3ATB2        TASTBL
082720011017     C                   ENDIF
082730011017     C                   MOVEL     ARBLNA        TASLNA
082740011017     C                   MOVEL     ARBNCL        TASNCL
082750160530     C                   MOVEL     ARBNCL        TASNCLb
082760011017     C                   MOVEL     ARBQFT        TASQFT
082770011017     C     WBOL          IFEQ      '1'
082780011017     C     §3AFCA        ANDEQ     1
082790011017     C     WBOL          OREQ      '2'
082800011017     C     §3AFCA        ANDEQ     2
082810011017     C                   MOVEL     AR9CAS        TASCAS
082820011017     C                   MOVEL     AR9VCA        TASVCA
082830011017     C                   MOVEL     AR9TIC        TASTIC
082840011017     C                   ENDIF
082850011017     C                   MOVEL     ARBIAS        TASIAS
082860011017     C                   MOVEL     ARBVAS        TASVAS
082870011017     C                   MOVEL     ARBCAD        TASCAD
082880011017     C                   MOVEL     ARBNZD        TASNZD
082890011017     C                   MOVEL     ARBFIN        TASFIN
082900011017     C                   MOVEL     ARBCAM        TASCAM
082910011017     C                   MOVEL     ARBNZM        TASNZM
082920011017     C                   MOVEL     ARBFAP        TASFAP
082930011017     C                   MOVEL     ARBTC1        TASTC1
082940011017     C                   MOVEL     ARBTC2        TASTC2
082950011017     C     V1CDDT        IFEQ      'C'                                          SI DDT
082960011017     C     V1CDDT        OREQ      'S'
082970031028     c     v1cddt        oreq      'P'
082980031028     c     v1cddt        oreq      'Q'
082990011017     C                   MOVEL     'S'           TASDDT
083000011017     C                   ENDIF
083010060925     c*****              movel     arbfbr        tasflo
083020060925     c                   clear                   dtas01
083030060925     c                   movel     arbfbr        §asfbr
083040060925     c                   movel     arbcca        §ascca
083050060925     c                   eval      tasflo = dtas01
083060011017      **
083070011017      * SE BOLLA EXPORT--> NON PASSO MAI IL FLAG CONSEGNA A MAGAZZINO
083080011017      *                    IN MODO CHE TASSI SEMPRE LA VARIA 'U'
083090020305     C     LNANTW        ifeq      'COR'
083100020305     c     LNANTW        oreq      'MES'
083110020305     c     LNANTW        oreq      'PPT'
083120011017     C                   MOVEL     ARBFDN        TASFDN
083130011017     C                   ENDIF
083140011017      **
083150011017      * CHAIN SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
083160030612     C                   CLEAR                   Tisi95DS
083170011017     C                   MOVEL     ARBLOM        I95LOC
083180011017     C                   MOVEL     ARBCAM        I95CAP
083190011017     C                   MOVEL     ARBNZM        I95NAR
083200011017     C                   MOVEL     '3'           I95TCN
083210011017     C                   MOVEL     ARBDSP        I95DAT
083220011017     C     §3AFCA        IFNE      0
083230011017     C                   MOVEL     'S'           I95FCA
083240011017     C                   ENDIF
083250011017     C                   MOVEL     §3ATB1        I95TPO
083260020305     c     lnpntw        ifeq      'FED'
083270020305     c                   movel     'S'           i95fi1
083280020305     c                   endif
083290011017     C                   CALL      'TISI95R'
083300030612     C                   PARM                    Tisi95DS
083310011017     C     O95ERR        IFEQ      *BLANKS
083320011017     C                   MOVEL     O95CTS        TASMCT
083330011017     C                   ENDIF
083340011017     C                   MOVEL     'S'           WCAL13            1
083350011017      * ELABORO E CHIUDO CON LR IL PGM
083360011017     C                   MOVEL     ' '           TASTLA
083370020930     c* imposto peso vdl e colli rilevati
083380030203     c**!!!              clear                   dtas01
083390030203     c**!!!              z-add     arbncp        §asncp
083400030203     c**!!!              z-add     arbpkc        §aspkc
083410030203     c**!!!              movel     dtas01        tasflo
083420030203     c                   z-add     arbncp        Tasncp
083430030203     c                   z-add     arbpkc        Taspkc
083440030204      * Bancali
083450030205     c                   If        %Subst(ArbGva:1:1) = 'B'
083460030729     c                   Eval      TasBan = V1cBan
083470030205     c                   EndIf
083480030729      * Colli Originali
083490030729     c                   If        %Subst(ArbGva:1:1) = 'O'
083500030729     c                   Eval      TasNcl = V1cCor
083510030729     c                   EndIf
083520050804     c
083530050804     c                   eval      parca  = ' '
083540050804     c                   movel     paramt        kpjbu
083550011017      *
083560011017     C                   CALL      'TNSF20R'
083570011017     C                   PARM                    KPJBA
083580011017     C                   PARM                    DTAS
083590011017     C                   PARM                    DTASV
083600011018      *
083610011018      * Tassazione andata buon fine
083620011018     C     TASERR        IFEQ      *BLANKS
083630011018      *
083640011018      * Controllo la divisa della tassazione con la divisa del video
083650011018      * se non corrispondono devo convertire la varia 'N' nella divisa
083660011018      * del video
083670011018     C     TASDIV        IFNE      VIDDIV
083680011018      * aggancio la tabella CV per sicurezza
083690011018     C                   CLEAR                   DSCV
083700011018     C                   MOVEL     'CV'          COD
083710011018     C                   MOVEL(P)  VIDDIV        KEY
083720011018     C                   EXSR      CTABEL
083730011018     C  N30              MOVEL     TBLUNI        DSCV
083740011018      *
083750030612     C                   CLEAR                   YeurcoDS
083760011018     C                   MOVEL     TASDIV        YECDVI
083770011018     C                   MOVEL     VIDDIV        YECDVO
083780011018     C                   Z-ADD     TASIMV        YECIMI
083790011018     C     §CVFDC        IFEQ      'S'
083800011018     C                   MOVE      '3'           YECDCO
083810011018     C                   ELSE
083820011018     C                   MOVE      '0'           YECDCO
083830011018     C                   ENDIF
083840011018     C                   CALL      'YEURCO'
083850030612     C                   PARM                    YeurcoDS
083860011018     C     YECESI        IFEQ      ' '
083870011018     C                   Z-ADD     YECIMO        TASIMV
083880011018     C                   ELSE
083890011018     C                   Z-ADD     88888888      TASIMV
083900011018     C                   ENDIF
083910011018     C                   ENDIF
083920011017      *
083930011017     C                   Z-ADD     TASIMV        V3CVA1
083940011017     C                   Z-ADD     TASIMV        VARVA1
083950011017     C                   ENDIF
083960011017      *
083970011017     C                   ENDSR
083980910429     C*
083990050531begsrC*--- TASSAZIONE BOLLA ------------------------------------------*
084000910429     C     TASSAZ        BEGSR
084010991005     C                   CLEAR                   DTAS
084020991005     C                   CLEAR                   DTASV
084030020503     C                   CLEAR                   wtassa            1
084040030620     C                   CLEAR                   wtassad           1
084050030620     C                   CLEAR                   itassa           11 3
084060050601     c*
084070050601     c* richiamo routine di impostazione campi "comuni" da passare a
084080050601     c* pgm tnsf20r
084090050601     c                   exsr      SR_rtassa
084100910429     C*
084110941221     C* CARICO LE VARIE
084120941221     C*
084130980513     C     WBOL          IFEQ      '1'
084140030709     c                   eval      flgsf20 = 'S'
084150991012     C                   EXSR      RDTASV
084160030709     c                   eval      flgsf20 = ' '
084170980513     C*
084180980513     C                   ELSE
084190040123      * carico varie seconda bolla
084200040123     C                   MOVEL     'G'           TASSVA
084210040123     c                   Eval      TascVag = 'S'
084220041202     C**!!!VI2VA1        IFGT      0
084230040123     C                   MOVEL     VI2SV1        VSV(1)
084240040123     C                   Z-ADD     VI2VA1        VVA(1)
084250040123     c                   Movel     Vi2Es1        ves(1)
084260041202     C**!!!              ENDIF
084270041202     c**!!!              If        Vi2Va2 > *Zeros
084280040123     c                   Movel     Vi2Sv2        Vsv(2)
084290040123     c                   Z-add     Vi2Va2        Vva(2)
084300040123     c                   Movel     Vi2Es2        ves(2)
084310041202     c**!!!              EndIf
084320041202     c**!!!              If        Vi2Va3 > *Zeros
084330040123     c                   Movel     Vi2Sv3        Vsv(3)
084340040123     c                   Z-add     Vi2Va3        Vva(3)
084350040123     c                   Movel     Vi2Es3        ves(3)
084360041202     c**!!!              EndIf
084370041202     c**!!!              If        Vi2Va4 > *Zeros
084380040123     c                   Movel     Vi2Sv4        Vsv(4)
084390040123     c                   Z-add     Vi2Va4        Vva(4)
084400040123     c                   Movel     Vi2Es4        ves(4)
084410041202     c**!!               EndIf
084420041202     c**!!!              If        Vi2Va5 > *Zeros
084430041130     c                   Movel     Vi2Sv5        Vsv(5)
084440041130     c                   Z-add     Vi2Va5        Vva(5)
084450041130     c                   Movel     Vi2Es5        ves(5)
084460041202     c**!!!              EndIf
084470041202     c**!!!              If        Vi2Va6 > *Zeros
084480041130     c                   Movel     Vi2Sv6        Vsv(6)
084490041130     c                   Z-add     Vi2Va6        Vva(6)
084500041130     c                   Movel     Vi2Es6        ves(6)
084510041202     c**!!!              EndIf
084520040126     c                   xfoot     vva           TotImv
084530040126     c                   Z-add     TotImv        TasImv
084540980608     C                   ENDIF
084550011211
084560011211      * Data fattura se c'è
084570030203     c**!!!              clear                   dtas01
084580020930     c**                 if        ar6dft <> 0
084590020930     c**                 movel     ar6dft        §asdat
084600020930     c**                 endif
084610020503     c**
084620020503     c                   if        wtassa=' '
084630020503     c* se devo tassare solo una varia, da tipo bolla, la imposto
084640020503     c                   if        §3asva<>*blanks
084650020503     c                   eval      tassva=§3asva
084660020503     c                   endif
084670030620     c                   endif
084680030925     c* se diritto di fatturazione uguale a zero non calcolo la varia D
084690030925     c                   if        sav_indsin = 0
084700030925     c                   eval      wtassad = 'N'
084710030925     c                   endif
084720030623      * se tassazione fattura immediata o ritassazione prepagato
084730030623      * passo al TNSF20R la varia "D" e l'importo del diritto
084740030620      * di fatturazione da sommare che prendo da cnind (INDSIN)
084750030623     c                   if        (*in14 and not *in29  and wtassad = ' ')  or
084760030623     c                             (not *in10 and vidnft > 0 and not *in29 and
084770030623     c                             wtassad = ' ')
084780030925      *
084790030620     c                   move      sav_indsin    tasivdf
084800030620     c                   eval      tassvdf = 'D'
084810030620      * se bolla già tassat devo sommare la sola varia "D"
084820030620      * imponibile > 0 oppure imponibile a zero ma singola varia vaolorizzata x bolle speciali
084830040123     c**!!!              if        tasimv > 0 or (tasimv = 0 and itassa > 0)
084840040123     c                   if        tasimv > 0
084850030620     c                   eval      tassva = 'D'
084860030620     c                   endif
084870030620      * se singola varia per bolla speciale non è da calcolare ed è uguale a zero non calcolo la
084880030620      * varia D
084890030620     c                   if        wtassa = 'N' and tasimv = 0 and itassa = 0
084900030620     c                   eval      wtassad = 'N'
084910030620     c                   endif
084920030620
084930030620     c                   endif
084940011211
084950030620      * se non è tassazione fattura immediata non devo calcolare la varia D
084960030623      * o ritassazione prepagato
084970030623     c*                  if        not *in14
084980030623     c                   if        not *in14  or  *in29 or
084990030623     c                             (not *in10 and vidnft = 0)
085000030620     c                   eval      wtassad = 'N'
085010030620     c                   endif
085020030620      * se non devo calcolare la singola varia e neppure la varia D salto la tassazione
085030030620     c                   if        wtassa = 'N' and wtassad = 'N'
085040030620     c                   goto      endtas
085050030620     c                   endif
085060151127     c* Packing list e orm telefonici solo per la prima bolla in quanto la seconda
085070151127     c* è solo per tassare una specifica varia
085080151126     C     WBOL          IFEQ      '1'
085090151126     c* PACKING LIST
085100151126     c                   if        par5als='S' or par5alx='S'
085110151126     c                   eval      tasspl='S'
085120151126     c                   endif
085130151126     c* ADDEBITO ORM TELEFONICI (diritto di chiamata)
085140151126     C                   MOVEL     'M'           WTRC
085150151126     C     KARB2         CHAIN(N)  FiAR401l
085160151126     c                   if        %found(fiar401l)
085170151126     c                   movel     ar4not        dsbl4m
085180151126     c                   if        §b4ado='S'
085190151126     c                   eval      tasprt='S'
085200151126     c                   endif
085210151126     c                   endif
085220151126
085230151126     c                   endif
085240050804     c
085250050804     c                   eval      parca  = ' '
085260050804     c                   movel     paramt        kpjbu
085270910429     C*
085280941209     C                   CALL      'TNSF20R'
085290910429     C                   PARM                    KPJBA
085300991005     C                   PARM                    DTAS
085310991005     C                   PARM                    DTASV
085320910429     C*
085330941212    1C     TASERR        IFNE      *BLANKS
085340980608     C     TASMSG        IFNE      *BLANKS
085350980608     C                   MOVEL(P)  TASMSG        VIDMSG
085360980608     C                   ELSE
085370941212     C                   MOVEL     MSG(55)       VIDMSG
085380980608     C                   ENDIF
085390941209     C                   SETON                                        499028
085400991006     C* SE ERRORI IN TASSAZIONE SPROTEGGO LA DIVISA
085410991006     C                   SETOFF                                       07
085420941212   X1C                   ELSE
085430980513     C     WBOL          IFEQ      '1'
085440950421     C                   MOVEL     TASQFT        VIDQFT
085450950424     C                   MOVEL     TASQFT        VARQFT
085460910429     C                   MOVEL     TASPOR        VIDPOR
085470991006     C                   MOVEL     TASPOR        VARPOR
085480991025     C* SE DIVISA DIVERSA DA PRECEDENTE E PREMUTO F6 ACCENDO IL 90
085490991025     C* PER RIEMETTERE LA VIDEATA IN MODO DA VISUALIZZARE LA NUOVA
085500991025     C* DIVISA
085510991025     C     VIDDIV        IFNE      TASDIV
085520991025     C     *INKF         ANDEQ     *ON
085530991025     C                   SETON                                        90
085540991025     C                   ENDIF
085550991025     C*
085560991005     C                   MOVEL     TASDIV        VIDDIV
085570991006     C                   MOVEL     TASDIV        WDIV
085580991005     C                   Z-ADD     TASIMV        VIDIMV
085590941209     C* CARICO LE VARIE
085600941212    2C                   DO        20            NRR
085610941209     C     NRR           CHAIN     LR48S03                            30
085620941209     C                   MOVEL     VSV(NRR)      V3CSV1
085630941209     C                   Z-ADD     VVA(NRR)      V3CVA1
085640991006     C                   Z-ADD     VVA(NRR)      VARVA1
085650941209     C                   MOVEL     VES(NRR)      V3CEV1
085660941209     C   30              WRITE     LR48S03
085670941209     C  N30              UPDATE    LR48S03
085680941212    2C                   ENDDO
085690941209     C*
085700980513    XC                   ELSE
085710040123     c                   Clear                   ContaSv
085720040123      * carico le varie seconda bolla
085730040123Do  2c                   Do        20            c
085740040123If  3c                   If        Vsv(c) <> §$2Iva and
085750040123     c                             Vsv(c) <> §$2Bol
085760040123     c                   Add       1             ContaSv
085770040123S   4c                   Select
085780040123     c                   When      ContaSv = 1
085790040123     c                   Movel     Vsv(c)        Vi2Sv1
085800040123     c                   Z-add     Vva(c)        Vi2Va1
085810040123     c                   Movel     Ves(c)        Vi2Es1
085820040123     c                   When      ContaSv = 2
085830040123     c                   Movel     Vsv(c)        Vi2Sv2
085840040123     c                   Z-add     Vva(c)        Vi2Va2
085850040123     c                   Movel     Ves(c)        Vi2Es2
085860040123     c                   When      ContaSv = 3
085870040123     c                   Movel     Vsv(c)        Vi2Sv3
085880040123     c                   Z-add     Vva(c)        Vi2Va3
085890040123     c                   Movel     Ves(c)        Vi2Es3
085900040123     c                   When      ContaSv = 4
085910040123     c                   Movel     Vsv(c)        Vi2Sv4
085920040123     c                   Z-add     Vva(c)        Vi2Va4
085930040123     c                   Movel     Ves(c)        Vi2Es4
085940041130     c                   When      ContaSv = 5
085950041130     c                   Movel     Vsv(c)        Vi2Sv5
085960041130     c                   Z-add     Vva(c)        Vi2Va5
085970041130     c                   Movel     Ves(c)        Vi2Es5
085980041130     c                   When      ContaSv = 6
085990041130     c                   Movel     Vsv(c)        Vi2Sv6
086000041130     c                   Z-add     Vva(c)        Vi2Va6
086010041130     c                   Movel     Ves(c)        Vi2Es6
086020040123    4c                   EndSl
086030040123    3c                   EndIf
086040040123    2c                   EndDo
086050040123     C**!!!              Z-ADD     1             C
086060040123     C**!!!TASSVA        LOOKUP    VSV(C)                                 33
086070040123     C*!!33              Z-ADD     VVA(C)        VI2VA1
086080040123     C*!!33              Z-ADD     VVA(C)        VA2VA1
086090040123     C*!!33              MOVEL     VSV(C)        VI2SV1
086100991025     C* SE DIVISA DIVERSA DA PRECEDENTE E PREMUTO F6 ACCENDO IL 90
086110991025     C* PER RIEMETTERE LA VIDEATA IN MODO DA VISUALIZZARE LA NUOVA
086120991025     C* DIVISA
086130040123     C*!!33VI2DIV        IFNE      TASDIV
086140040123     C     VI2DIV        IFNE      TASDIV
086150991025     C     *INKF         ANDEQ     *ON
086160991025     C                   SETON                                        90
086170991025     C                   ENDIF
086180040123     C*!!33              MOVEL     TASDIV        VI2DIV
086190040123     C*!!33              MOVEL     TASDIV        WDIV
086200040123     C                   MOVEL     TASDIV        VI2DIV
086210040123     C                   MOVEL     TASDIV        WDIV
086220040123     C*!!33VES(C)        IFNE      ' '
086230040123     C**!!!VI2CEI        ANDEQ     ' '
086240040123     C**!!!              MOVEL     VES(C)        VI2CEI
086250040123     C**!!!              ENDIF
086260980513     C                   ENDIF
086270020503     C                   ENDIF
086280980513     C*
086290030620    1C*****              ENDIF
086300941209     C*
086310911211     C     ENDTAS        ENDSR
086320050601     C*
086330050601     C*--- Riempimento campi comuni di passaggio a tnsf20r -----------*
086340050601     C     sr_rtassa     BEGSR
086350050601     C* CODIFICATO
086360050601    1C     VIDKSC        IFNE      9999
086370050601    1C     VIDKSC        ANDNE     8888
086380050601     C                   MOVEL     VIDKLP        TASKSC
086390050601     C                   MOVE      VIDKSC        TASKSC
086400050601     C                   MOVEL     VIDCTR        TASCTR
086410050601   X1C                   ELSE
086420050601     C*
086430050601     C* E HO LA DATA FATTURA USO QUELLA, ALTRIMENTI DATEU
086440050601     C     AR6DFT        IFGT      0
086450050601     C                   Z-ADD     AR6DFT        WDATA
086460050601     C                   ELSE
086470050601     C                   Z-ADD     DATEU         WDATA
086480050601     C                   ENDIF
086490050601     C*
086500050601     C* DAL TIPO SERVIZIO PRENDO LA TARIFFA CON CUI TASSARE
086510050601     C*
086520160225     c**                 eval      tasksc  = o27ksc
086530160225     c**                 eval      tasctr  = o27ctr
086540160225     C                   MOVEL     VIDKLP        TASKSC
086550160225     C                   MOVE      VIDKSC        TASKSC
086560160225     C                   MOVEL     VIDCTR        TASCTR
086570050601     C**
086580050601    1C                   ENDIF
086590050601     C*
086600050601     C                   Z-ADD     ARBPKF        TASPKF
086610160530     C                   Z-ADD     ARBPKb        TASPKb
086620050601     C                   MOVEL     ARBLNP        TASLNP
086630050601     C                   MOVEL     ARBCTS        TASCTS
086640050601     C                   MOVEL     ARBDSP        TASDSP
086650050601     C                   MOVEL     ARBDSP        TASDCT
086660050601     C                   MOVEL     ARBVLF        TASVLF
086670160530     C                   MOVEL     ARBfvF        TASfvf
086680160530     c                   z-add     arbvlc        tasvlc
086690160530     c                   z-add     arbncr        tasncr
086700050601     C                   MOVEL     ARBTSP        TASTSP
086710050601     C     WBOL          IFEQ      '1'
086720050601     C                   MOVEL     §3ATB1        TASTBL
086730050601     C                   MOVEL     VIDDIV        TASDIV
086740050601     C                   Z-ADD     VIDIMV        TASIMV
086750050601     C                   ELSE
086760050601     C                   MOVEL     §3ATB2        TASTBL
086770050601     C                   MOVEL     VI2DIV        TASDIV
086780050601     C**!!!              Z-ADD     VI2VA1        TASIMV
086790050601     C                   END
086800050601     C                   MOVEL     ARBLNA        TASLNA
086810050601     C                   MOVEL     ARBNCL        TASNCL
086820160530     C                   MOVEL     ARBNCL        TASNCLB
086830050601     C                   MOVEL     VIDPOR        TASPOR
086840050601     C                   MOVEL     VIDQFT        TASQFT
086850050601     C     WBOL          IFEQ      '1'
086860050601     C     §3AFCA        ANDEQ     1
086870050601     C     WBOL          OREQ      '2'
086880050601     C     §3AFCA        ANDEQ     2
086890050601     C                   MOVEL     AR9CAS        TASCAS
086900050601     C                   MOVEL     AR9VCA        TASVCA
086910050601     C                   MOVEL     AR9TIC        TASTIC
086920050601     C                   ENDIF
086930050601     C                   MOVEL     VIDIAS        TASIAS
086940050601     C                   MOVEL     VIDVAS        TASVAS
086950050601     C                   MOVEL     ARBCAD        TASCAD
086960050601     C                   MOVEL     ARBNZD        TASNZD
086970050601     C                   MOVEL     ARBCAM        TASCAM
086980050601     C                   MOVEL     ARBNZM        TASNZM
086990050601     C                   MOVEL     ARBFIN        TASFIN
087000050601     C                   MOVEL     ARBFAP        TASFAP
087010050601     C                   MOVEL     ARBTC1        TASTC1
087020050601     C                   MOVEL     ARBTC2        TASTC2
087030050601     C     V1CDDT        IFEQ      'S'
087040050601     C     V1CDDT        OREQ      'C'
087050050601     c     v1cddt        oreq      'P'
087060050601     c     v1cddt        oreq      'Q'
087070050601     C                   MOVEL     'S'           TASDDT
087080050601     C                   ENDIF
087090060925     c****               movel     arbfbr        tasflo
087100060925     c                   clear                   dtas01
087110060925     c                   movel     arbfbr        §asfbr
087120060925     c                   movel     arbcca        §ascca
087130110630     c                   movel     wemd          §asemd
087140150108     c* Pin Code
087150150108     c                   if        arbgma<>*blanks
087160150108     c                   Movel     '7R'          cod
087170150108     c                   Movel(p)  ArbGMa        key
087180150108     c                   ExSr      ctabel
087190150108     c  n30              Movel     TblUni        ds7R
087200150108     c   30              Clear                   ds7R
087210150108     c                   if        §7rpincode='S'
087220150108     c                   movel     'S'           §aspin
087230150108     c                   endif
087240150108     c                   endif
087250060925     c                   eval      tasflo = dtas01
087260050601     C* SE BOLLA ARRIVI IN ASSEGNATO EXPORT, NON PASSO LA CONSEGNA A
087270050601     C*  MAGAZZINO PER FAR CALCOLARE SEMPRE LA VARIA "U"
087280050601     C     *IN10         IFEQ      *OFF
087290050601     C     *IN06         OREQ      *ON
087300050601     C     lnantw        oreq      'COR'
087310050601     c     lnantw        oreq      'MES'
087320050601     c     lnantw        oreq      'PPT'
087330050601     C                   MOVEL     ARBFDN        TASFDN
087340050601     C                   ENDIF
087350050601     C**
087360050601     C* CHAI SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
087370050601     C                   CLEAR                   Tisi95DS
087380050601     C                   MOVEL     ARBCAM        I95CAP
087390050601     C                   MOVEL     ARBNZM        I95NAR
087400050601     C                   MOVEL     '3'           I95TCN
087410050601     C                   MOVEL     ARBDSP        I95DAT
087420050601     C                   MOVEL     FLGTB1        I95TPO
087430050601     C     §3AFCA        IFNE      0
087440050601     C                   MOVEL     'S'           I95FCA
087450050601     C                   ENDIF
087460050601     c     lnpntw        ifeq      'FED'
087470050601     c                   movel     'S'           i95fi1
087480050601     c                   endif
087490050601     C**
087500050601     C                   CALL      'TISI95R'
087510050601     C                   PARM                    Tisi95DS
087520050601     C     O95ERR        IFEQ      *BLANKS
087530050601     C                   MOVEL     O95CTS        TASMCT
087540050601     C                   ENDIF
087550050601
087560050601     c                   z-add     arbncp        Tasncp
087570050601     c                   z-add     arbpkc        Taspkc
087580050601      * Bancali
087590050601     c                   If        %Subst(ArbGva:1:1) = 'B'
087600050601     c                   Eval      TasBan = V1cBan
087610050601     c                   EndIf
087620050601      * Colli Originali
087630050601     c                   If        %Subst(ArbGva:1:1) = 'O'
087640050601     c                   Eval      TasNcl = V1cCor
087650050601     c                   EndIf
087660050601     c                   endsr
087670050531     C*--- TASSAZIONE BOLLA ------------------------------------------*
087680050531     C     TASSAR        BEGSR
087690050601     c                   clear                   dtas
087700050601     c                   clear                   dtasv
087710050601     c* richiamo routine di riempimento campi da passare a tnsf20r
087720050601     c                   eval      wbol = '1'
087730050601     c                   exsr      SR_rtassa
087740050531     c                   eval      tassva = 'R'
087750050804     c
087760050531     c                   eval      parca  = 'S'
087770050531     c                   movel     paramt        kpjbu
087780050531      *
087790050531     C                   CALL      'TNSF20R'
087800050531     C                   PARM                    KPJBA
087810050531     C                   PARM                    DTAS
087820050531     C                   PARM                    DTASV
087830050722      *
087840050804      *
087850050722     c                   eval      parca  = ' '
087860050804     c***                movel     paramt        kpjbu
087870050722      *
087880050531     c                   endsr
087890911211     C*
087900011017     C*
087910991006     C* CALCOLO IL TOTALE FATTURA-------------------------------------*
087920990702     C     TOTFAT        BEGSR
087930030612     C                   CLEAR                   FNLV04DS
087940990702     C* AROTONDO IN ARRIVO O IN PARTENZA SE PREPAGATI NUOVI
087950990702    2C     *IN10         IFEQ      *ON
087960990702     C     *IN23         OREQ      *ON
087970991005     C                   MOVEL     VIDDIV        DIVGEI
087980991005     C                   EXSR      RIEGEI
087990991005     C                   MOVEL     §GEAED        D04AED
088000991005     C                   Z-ADD     §GEAVL        D04VAR
088010990702    2C                   ENDIF
088020991005     C                   MOVEL     'T'           D04FIM
088030990702     C                   MOVEL     ARBDFT        D04DFT
088040990702     C                   MOVEL     VIDIMV        D04IMV
088050990702     C                   MOVEL     VIDCEI        D04CEI
088060990702     C                   MOVEL     AR9TIC        D04TIC
088070990702     C                   MOVEL     AR9VCA        D04VCA
088080990702     C                   Z-ADD     AR9CAS        D04CAS
088090990702     C                   MOVEL     '1'           D04SPE
088100990702     C   23              MOVEL     '8'           D04SPE
088110991005     C                   MOVEL     §3ATB1        D04TBL
088120991005     C                   MOVEL     VIDDIV        D04DIV
088130990702     C                   Z-ADD     VIDPOR        D04POR
088140990702     C                   MOVEL     '1'           UNO               1
088150990702     C*
088160991005     C                   CALL      'FNLV04R'
088170030612     C                   PARM                    FNLV04DS
088180991005     C                   PARM                    DTASV
088190990702     C                   ENDSR
088200990702     C*
088210991005     C**************************************************************************
088220991005     C*  REPERIMENTO TABELLA GEI
088230991005     C**************************************************************************
088240991005     C     RIEGEI        BEGSR
088250030612     C                   CLEAR                   Tibs02DS
088260991117     C                   CLEAR                   DGEI
088270991117     C                   MOVEL     'C'           T02MOD
088280991117     C                   MOVEL     KNSIF         T02SIF
088290991117     C                   MOVEL     'GEI'         T02COD
088300991117     C                   MOVEL     DIVGEI        T02KE1
088310991117     C                   CALL      'TIBS02R'
088320991117     C                   PARM                    KPJBA
088330030612     C                   PARM                    Tibs02DS
088340991117     C     T02ERR        IFEQ      *BLANKS
088350991117     C                   MOVEL     T02UNI        DGEI
088360991117     C                   ENDIF
088370991005     C                   ENDSR
088380911211     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE TX -----------------*
088390911211     C     REGIS3        BEGSR
088400041027     C                   SETOFF                                       3709
088410130412     c                   clear                   wpda_ndc
088420130412     c                   clear                   wpda_ddc
088430130412     c                   clear                   wpda_dcm
088440041027      * salvo i campi relativi al valore da assicurare, qtà da fatturare, valore merce
088450041027     c                   eval      sav_arbias = arbias
088460041027     c                   eval      sav_arbvas = arbvas
088470041027     c                   eval      sav_arbqft = arbqft
088480041027     c                   eval      sav_arbvmd = arbvmd
088490041027     c                   eval      sav_arbvad = arbvas
088500041027     c                   movel     arbctr        sav_arbctr
088510950104     C*  GIRO UDATE
088520950104     C                   TIME                    W0140            14 0
088530950104     C* UDATE IN GGMMAAAA
088540950104     C                   MOVE      W0140         WDTGIO            8 0
088550950104     C*
088560950104     C* UDATE IN AAAAMMGG
088570950104     C                   Z-ADD     WDTGIO        G02DAT
088580950104     C                   MOVEL     *BLANK        G02ERR
088590950104     C                   CALL      'XSRDA8'
088600950104     C                   PARM                    WLBDAT
088610950104     C                   MOVEL     G02INV        DATEU             8 0
088620941221     C* PRENDO I VALORI ASSOLUTI
088630941221     C                   MLLZO     1             VIDPOR
088640941221     C                   MLLZO     1             VIDIMV
088650941221     C                   MLLZO     1             VIDIAS
088660941221     C                   MLLZO     1             VIDVMD
088670941221     C                   MLLZO     1             VIDQFT
088680941221     C                   MLLZO     1             VIDKLP
088690941221     C                   MLLZO     1             VIDKSC
088700070515     c
088710070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
088720070515     c* iornare direttamente il file
088730070515     c*  solo se cambiati dati su testa bolla
088740070515     c                   clear                   wsiaggio          1
088750070515  2b c                   if        sav_arbias <> vidias or sav_arbvas<>vidvas
088760070515     c                             or
088770070515     c                             sav_arbvmd <> vidvmd or sav_arbvad<>vidvad
088780070515     c                             or
088790070515     c                             sav_arbqft <> vidqft  or
088800070515     c                             sav_arbctr<>vidctr
088810070515     c                   eval      wsiaggio='S'
088820070515     c                   clear                   waltrabolla       1
088830070515     C   10KARB          CHAIN     FNBLP01L                           3091
088840070515     C  n10KARB          CHAIN     FNARB01L                           3091
088850070515     c*
088860070515     c                   if        *in91
088870070515     C                   SETON                                        28
088880070515     C                   MOVEL     MSG(4)        VIDMSG
088890070515     C                   GOTO      ENDRG3
088900070515     c                   else
088910070515     c                   if        *in30
088920070515     c                   eval      waltrabolla='N'
088930070515     c                   else
088940130411     c  n10              eval      wpda_ndc=arbndc
088950130411     c  n10              eval      wpda_ddc=arbddc
088960130411     c  n10              eval      wpda_dcm=arbdcm
088970070515     C   10KARB          CHAIN     FNARB01L                           30
088980070515     C  N10KARB          CHAIN     FNBLP01L                           30
088990070515     c* Alla fine per non sporcarmi i campi del record, richaino il
089000070515     c*  record originale che sto variando
089010130411     c   10              eval      wpda_ndc=arbndc
089020130411     c   10              eval      wpda_ddc=arbddc
089030130411     c   10              eval      wpda_dcm=arbdcm
089040070515     c                   endif
089050070515     c                   endif
089060070515     c                   endif
089070131120    1c                   if        wsiaggio=*blanks and d66pda<>' '
089080130412     c                   clear                   waltrabolla       1
089090130412    2c                   if        not *in10
089100130412     C     KARB          CHAIN(n)  FNARB01L                           30
089110130412    3c                   if        *in30
089120130412     c                   eval      waltrabolla='N'
089130130412    3c                   endif
089140130412    2c                   endif
089150130412     c                   eval      wpda_ndc=arbndc
089160130412     c                   eval      wpda_ddc=arbddc
089170130412     c                   eval      wpda_dcm=arbdcm
089180130412     C  N10KARB          CHAIN     FNBLP01L                           30
089190130412     c                   endif
089200941213     C*
089210941213     C* INVIO PER SEDE
089220170116     C***                MOVEL     'FIARBT0T'    SFILE
089230941213     C* ALLOCO OGGETTI
089240170116     C***                EXSR      ALLOC
089250070515     c
089260170116     c***                if        *in91
089270170116     C** 10              unlock    FNBLP01L
089280170116     C**N10              unlock    FNARB01L
089290170116     C**                 GOTO      ENDRG3
089300170116     c**                 endif
089310941213     C*
089320991001     C                   MOVEL     *BLANKS       WCKINL            1
089330991001     C                   EXSR      CARWAR
089340151020     c* Mmorizzo il totale imponibilie del video prima degli arrotondamenti
089350151020     c****               if        tassatoimv=0
089360151019     c                   movel     vidimv        Tassatoimv
089370151020     c****               endif
089380991006     C* SOLO SE C'E' NUMERO FATTURA CALCOLO
089390991006     C     VIDDFT        IFGT      *ZEROS
089400000105     C                   MOVEL     O17DFA        ARBDFT
089410991006     C                   EXSR      TOTFAT
089420991006     C     D04BOL        IFGT      *ZEROS
089430991006     C                   ADD       1             C
089440991006     C                   MOVEL     §$2BOL        WSV(C)
089450991006     C                   Z-ADD     D04BOL        WVA(C)
089460991006     C                   ENDIF
089470991006     C     D04IVA        IFGT      *ZEROS
089480991006     C                   ADD       1             C
089490991006     C                   MOVEL     §$2IVA        WSV(C)
089500991006     C                   Z-ADD     D04IVA        WVA(C)
089510991006     C                   ENDIF
089520991006     C                   ENDIF
089530941213     C*
089540941213     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
089550950616    1C     C             IFGT      0
089560941213     C                   SETON                                        09
089570941213     C*
089580941213     C* INVIO PER SEDE
089590941213     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
089600170116    2C***  C             IFGT      3
089610991006     C* MI SALVO I CAMPI DELL'FIARBT
089620170116     C***                MOVEL     SOVR(1)       SOARBT
089630170116     C***                MOVEL     SALC(1)       SAARBT
089640170116     C***                MOVEL     SDLC(1)       SDARBT
089650941213     C*
089660170116     C***                MOVEL     'FIARBU0T'    SFILE
089670941213     C* ALLOCO OGGETTI
089680170116     C***                EXSR      ALLOC
089690070515     c
089700170116     c**                 if        *in91
089710170116     C** 10              unlock    FNBLP01L
089720170116     C**N10              unlock    FNARB01L
089730170116     C**                 MOVEL     SDARBT        SDLC(1)
089740170116     c**                 exsr      DLCSED
089750170116     C**                 SETON                                        2891
089760170116     C**                 GOTO      ENDRG3
089770170116     c**                 endif
089780941213     C*
089790170116    2C***                ENDIF
089800950616    1C                   ENDIF
089810950114     C*
089820950114     C                   MOVEL     KNMUS         ARBPRU
089830950114     C                   Z-ADD     DATEU         ARBDTV
089840950114     C                   TIME                    ARBORV
089850950114     C                   MOVEL     VIDCVR        ARBCVB
089860991011     C                   MOVEL     '1'           ARBTRC
089870991011     C*
089880991011     C                   MOVEL     '1'           AR6TRC
089890930511     C*
089900060130     C* SE DA STORICIZZARE
089910060130    1C**   *IN10         IFEQ      *ON
089920060130     C**   D66FSA        ANDEQ     'S'
089930060130     C**   *IN10         OREQ      *OFF
089940060130     C**   D66FSP        ANDEQ     'S'
089950060130     C     D66FSA        ifeq      'S'
089960941213     C                   EXSR      STOARB
089970950616    1C                   ENDIF
089980060302     c* storicizzazione motivazione
089990060302     c     d66mtv        ifeq      'S'
090000060302     c                   exsr      sto_mtv
090010060302     c                   endif
090020941213     C**
090030991006     C* INVIO VARIAZIONE --> FIARBU0T
090040941213     C**
090050950616    1C     *IN09         IFEQ      *ON
090060941213     C     C             ANDGT     3
090070941213     C                   Z-ADD     4             C
090080941213     C*
090090941213    2C     D66FFI        IFEQ      'S'
090100950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
090110950616    3C     §3ABSD        IFEQ      ' '
090120941213     C                   Z-ADD     4             C
090130170116     C****               OPEN      FIARBU0T
090140941213     C*
090150941213     C* NE SCRIVO FIN TANTO CHE CE NE SONO
090160950616    4C     WSV(C)        DOWNE     ' '
090170941213     C                   MOVEL     WSV(C)        ARBSVN
090180941213     C                   Z-ADD     WVA(C)        ARBVAN
090190991006     C                   WRITE     FIARBUTT
090200941213     C                   ADD       1             C
090210950616    4C                   ENDDO
090220941213     C*
090230170116     C***                CLOSE     FIARBU0T
090240941213     C* DISALLOCO OGGETTO MEMBRO PER SEDE
090250950616    3C                   ENDIF
090260170116     C****               EXSR      DLCSED
090270950208    2C                   ENDIF
090280941213     C* REIMPOSTO I CAMPI
090290170116     C***                MOVEL     SOARBT        SOVR(1)
090300170116     C***                MOVEL     SAARBT        SALC(1)
090310170116     C***                MOVEL     SDARBT        SDLC(1)
090320941213    1C                   ENDIF
090330930506     C*
090340930506     C                   Z-ADD     VIDKSC        ARBKSC
090350930506     C                   MOVEL     VIDKLP        ARBKSC
090360941209     C                   MOVEL     VIDIAS        ARBIAS
090370941209     C                   MOVEL     VIDVAS        ARBVAS
090380011017      * Se l'importo da assicurare è a 0 non devo scrivere la divisa
090390011017     C     VIDIAS        IFEQ      *ZEROS
090400011017     C                   CLEAR                   ARBVAS
090410011017     C                   ENDIF
090420011017      *
090430941209     C                   MOVEL     VIDVMD        ARBVMD
090440941215     C                   MOVEL     VIDVAD        ARBVAD
090450011017      * Se il valore merce è a 0 non devo scrivere la divisa
090460011017     C     VIDVMD        IFEQ      *ZEROS
090470011017     C                   CLEAR                   ARBVAD
090480011017     C                   ENDIF
090490011017      *
090500941209     C                   MOVEL     VIDQFT        ARBQFT
090510930506     C                   MOVEL     VIDCTR        ARBCTR
090520991006     C                   MOVEL     VIDDIV        ARBDIV
090530941213     C                   MOVEL     VIDPOR        ARBPOR
090540941213     C                   MOVEL     WSV(1)        ARBSV1
090550941213     C                   MOVEL     WVA(1)        ARBVA1
090560941213     C                   MOVEL     WSV(2)        ARBSV2
090570941213     C                   MOVEL     WVA(2)        ARBVA2
090580941213     C                   MOVEL     WSV(3)        ARBSV3
090590941213     C                   MOVEL     WVA(3)        ARBVA3
090600991006     C                   Z-ADD     VIDIMV        ARBIMV
090610991006     C                   MOVEL     O17DFA        ARBDFT
090620950104     C                   MOVEL     VIDNFT        ARBNFT
090630950104     C                   MOVEL     VIDFIV        ARBFIV
090640991013     C* SE TOTALE IMPONIBILE ERA IMMESSO DALLA PARTENZA ED E'
090650991013     C* STATO MODIFICATO TOLGO LA 'P'
090660991013     C     AR6FIM        IFEQ      'P'
090670991013     C     ARBIMV        ANDNE     AR6IMV
090680991013     C     *IN10         ANDEQ     *ON
090690991013     C                   MOVE      *BLANKS       ARBFIM
090700991013     C                   ELSE
090710991013     C* ALTRIMENTI CI METTO QUELLO CHE C'ERA
090720991013     C                   MOVE      AR6FIM        ARBFIM
090730991013     C* SE NON ESISTE FIAR6 E SONO IN PARTENZA IMPOSTO 'P'
090740991013     C     WAR6          IFNE      'E'
090750991013     C     *IN10         ANDEQ     *OFF
090760991013     C                   MOVEL     'P'           ARBFIM
090770991013     C                   END
090780991013     C                   ENDIF
090790941209     C*
090800941212     C                   MOVEL     VIDCEI        ARBCEI
090810930506     C*
090820950616    1C     ARBDFT        IFGT      0
090830930505     C*
090840930505     C* SE TOTALE FATTURA NON CORRISPONDE AL PRECEDENTE RISTAMPA FATT
090850941209     C     D04IFT        COMP      AR6IFT                             3737
090860930505     C*
090870941212     C                   MOVE      D04ALI        ARBALI
090880941212     C                   MOVE      D04IFT        ARBIFT
090890941212     C                   MOVE      D04IMV        ARBIMV
090900930514     C*
090910970613    2C                   SELECT
090920941221     C     ARBPOR        WHENGT    0
090930941212     C                   Z-ADD     D04POR        ARBPOR
090940941221     C     ARBVA1        WHENGT    0
090950991007     C     ARBSV1        ANDNE     §$2BOL
090960991007     C     ARBSV1        ANDNE     §$2IVA
090970941213     C                   ADD       D04POR        ARBVA1
090980941209     C     ARBVA2        WHENGT    0
090990991007     C     ARBSV2        ANDNE     §$2BOL
091000991007     C     ARBSV2        ANDNE     §$2IVA
091010941213     C                   ADD       D04POR        ARBVA2
091020941209     C     ARBVA3        WHENGT    0
091030941213     C                   ADD       D04POR        ARBVA3
091040970613    2C                   ENDSL
091050941209     C*
091060950616    1C                   ENDIF
091070930511     C*
091080930511     C                   MOVEL     VIDCVR        ARBCVB
091090911218     C*
091100950616    1C     D66FFI        IFEQ      'S'
091110950227     C*
091120950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
091130950616    2C     §3ABSD        IFEQ      ' '
091140170116     C***                OPEN      FIARBT0T
091150991006     C                   WRITE     FIARBTTT
091160170116     C***                CLOSE     FIARBT0T
091170950616    2C                   END
091180941212     C* DISALLOCO OGGETTO MEMBRO PER SEDE
091190170116     C***                EXSR      DLCSED                                       FNARBT0T
091200950616    1C                   END
091210941213     C**
091220941216     C* A G G I O R N O
091230941213     C**
091240051214     c* Semodificato il cod tariffa, verifico se volume automatico
091250051214     c                   if        not *in10 and vidctr<>sav_arbctr
091260051214     C*
091270051214     C                   MOVEL     '7T'          COD
091280051214     C                   MOVEL(P)  ARBFVF        KEY
091290051214     C                   EXSR      CTABEL
091300051214     C                   MOVEL     TBLUNI        DS7T
091310051214     C     §7TRVL        IFEQ      'A'
091320051214     C     §7TRVL        OREQ      'P'
091330051214     c                   movel     'S'           wricvl
091340051214     c                   seton                                        24
091350051214     c                   endif
091360051214     c                   endif
091370051214     c
091380991006     C                   Z-ADD     ARBKSC        AR6KSC
091390991006     C                   Z-ADD     ARBCTR        AR6CTR
091400941213     C                   MOVE      ARBALI        AR6ALI
091410941213     C                   MOVE      ARBIFT        AR6IFT
091420941213     C                   MOVE      ARBIMV        AR6IMV
091430991006     C                   MOVEL     ARBDIV        AR6DIV
091440941213     C                   MOVE      ARBPOR        AR6POR
091450941213     C                   MOVE      ARBSV1        AR6SV1
091460941213     C                   MOVE      ARBVA1        AR6VA1
091470941213     C                   MOVE      ARBSV2        AR6SV2
091480941213     C                   MOVE      ARBVA2        AR6VA2
091490941213     C                   MOVE      ARBSV3        AR6SV3
091500941213     C                   MOVE      ARBVA3        AR6VA3
091510941213     C                   MOVEL     ARBCEI        AR6CEI
091520950104     C                   MOVEL     ARBDFT        AR6DFT
091530950104     C                   MOVEL     ARBNFT        AR6NFT
091540950104     C                   MOVEL     ARBFIV        AR6FIV
091550991013     C                   MOVEL     ARBFIM        AR6FIM
091560941213     C*
091570991006     C* CANCELLO LE VARIE NELL'FIAR7
091580941221    1C     WAR7          IFEQ      'E'
091590991006     C                   MOVEL     '1'           WTRC
091600991006     C     KARB2         SETLL     FIAR7000
091610991006     C     KARB2         READE     FIAR7000                               30
091620941213     C*
091630941213    2C     *IN30         DOWEQ     *OFF
091640991006     C                   DELETE    FIAR7000
091650991006     C     KARB2         READE     FIAR7000                               30
091660941213    2C                   ENDDO
091670941213    1C                   ENDIF
091680941213     C*
091690941213     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
091700941213    1C     WSV(4)        IFNE      ' '
091710941213     C                   Z-ADD     4             C
091720991006     C                   CLEAR                   FIAR7000
091730941213     C                   MOVEL     ARBAAS        AR7AAS
091740941213     C                   MOVEL     ARBLNP        AR7LNP
091750941213     C                   MOVEL     ARBNRS        AR7NRS
091760941213     C                   MOVEL     ARBNSP        AR7NSP
091770991006     C                   MOVEL     '1'           AR7TRC
091780941213    2C     WSV(C)        DOWNE     ' '
091790941213     C                   MOVEL     WSV(C)        AR7SVN
091800941213     C                   MOVEL     WVA(C)        AR7VAN
091810991006     C                   WRITE     FIAR7000
091820941213     C                   ADD       1             C
091830941213    2C                   ENDDO
091840941213    1C                   ENDIF
091850941213     C**
091860941216     C* SE HO ELIMINATO TUTTA LA TASSAZIONE, PER S.F., DELETO RECORD
091870950616    1C     AR6DFT        IFEQ      0
091880941216     C     AR6SV1        ANDEQ     ' '
091890941216     C     AR6SV2        ANDEQ     ' '
091900941216     C     AR6SV3        ANDEQ     ' '
091910941216     C     AR6POR        ANDEQ     0
091920991116     C     AR6CEI        ANDEQ     ' '
091930941216     C*
091940950616    2C     WAR6          IFEQ      'E'
091950041027     C     *IN10         ifeq      *OFF
091960991006     C                   MOVEL     '1'           WTRC
091970991006     C     KARB2         DELETE    FIAR6000                           30
091980991116     C                   ELSE
091990041027     C* SE variaz, in ARRIVO DEVO UGUALMENTE tenere
092000991116     C* FIAR6000 (SERVE PER LA PROCEDURA DEI DANNI)
092010991116     C                   UPDATE    FIAR6000
092020991116     C                   ENDIF
092030991116   X2C                   ELSE
092040041027     C* SE NON ESISTE RECORD TASSAZIONE scrivo record se var.in arrivo
092050041027     C     *IN10         ifeq      *ON
092060991116     C                   MOVEL     ARBAAS        AR6AAS
092070991116     C                   MOVEL     ARBLNP        AR6LNP
092080991116     C                   MOVEL     ARBNRS        AR6NRS
092090991116     C                   MOVEL     ARBNSP        AR6NSP
092100991116     C                   WRITE     FIAR6000
092110041027    3C                   ENDIF
092120041027    2C                   ENDIF
092130941216     C*
092140941216     C                   ELSE
092150941216     C*
092160950616    2C     WAR6          IFEQ      'E'
092170991006     C                   UPDATE    FIAR6000
092180941209     C                   ELSE
092190941209     C                   MOVEL     ARBAAS        AR6AAS
092200941209     C                   MOVEL     ARBLNP        AR6LNP
092210941209     C                   MOVEL     ARBNRS        AR6NRS
092220941209     C                   MOVEL     ARBNSP        AR6NSP
092230991006     C                   WRITE     FIAR6000
092240950616    2C                   ENDIF
092250950616    1C                   ENDIF
092260941209     C*
092270151016     c* Se tassazione fino all'imponibile, aggiorno peso e volume usati
092280151016     c*  per la fatturazione se inserito o variato totale imponibile
092290151016     c                   if        (ar6imv>0 and ar6imv<>savimv_fat) or
092300151016     c                             ar6imv=0
092310151118     c                   exsr      regpesivol
092320151016     c                   endif
092330950113     C*
092340950113     C* RICALCOLO VOLUME AUTOMATICO E POI VADO IN VIDEATA DEL VOLUME
092350950113     C*  SE VOLUME AUTOMATICO O PRESUNTO PER PRECEDENTE VARIAIZONE
092360950113     C*  MITTENTE
092370950616    1C     *IN24         IFEQ      *ON
092380051214     c* Mi salvo i campi che userò per la storicizzazione
092390051214     C                   MOVEL     sav_arbctr    MITCTR
092400051214     C                   MOVEL     ARBFVF        MITFVF
092410051214     C                   MOVEL     ARBVLF        MITVLF
092420051214     c
092430950113     C                   EXSR      CALVOL
092440070907     C**                 MOVEL     'I2'          D48CVB
092450070907     C**                 MOVEL     'E'           WESEG
092460070907     c                   add       1             kk
092470070907     c                   movel     'I2'          kcvb(kk)
092480070911     c                   movel     'V'           kffr(kk)
092490070911     c                   movel     ' '           kf12(kk)
092500070911     c****               clear                   w48f12
092510950114     C* AGGIORNO GIA' IL VOLUME TANTO PASSO DALLA VIDEATA DEL VOLUME
092520950114     C*  COMUNQUE PER LA TRASMISSIONE
092530950114     C                   MOVEL     D18FVF        ARBFVF
092540950114     C                   MOVEL     D18VLF        ARBVLF
092550070911     C*****              MOVEL     'V'           D48FFR
092560950616     C                   END
092570911211     C*
092580950114     C   10              UPDATE    FNARB000
092590950114     C  N10              UPDATE    FNBLP000
092600950114     C*
092610041027  1b c***                if        wstessogas = 'S' and (d66ffi = 'E' or
092620041027     c***                          d66ffi = 'P') and *in10 = *off
092630041027  2b c***                if        sav_vidias <> vidias or sav_vidvas <> vidvas
092640041027     c***                          or
092650041027     c***                          sav_vidvmd <> vidvmd or sav_vidvad <> vidvad
092660041027     c***                          or
092670041027     c***                          sav_vidqft <> vidqft
092680041027     c***                exsr      Agg_ARB
092690041027  2e c***                endif
092700041027  1e c***                endif
092710041027     c
092720041027     c* Aggiorno altra bolla se c'e' e variati i campi sopra
092730041027     c                   if        wsiaggio='S' and waltrabolla=' '
092740041027     c* non aggiono mai il ksc:
092750041027     c* in  partenza perchè il S.F. va in ar6
092760041027     c*  in arrivo perchè il cod mittente se cambiato viene impostato
092770041027     c*  in regis8
092780041027     C   10              except    arbTblp
092790041027     C  N10              except    arbTarb
092800041027     c                   endif
092810130411
092820130411     c* Richiamo routine per invio variazione a pda
092830130411     c                   exsr      sr_pda
092840911212     C     ENDRG3        ENDSR
092850911211     C*
092860970613     C* SE BOLLA SU DISTINTA CHIUSA NON FACCIO NULLA ALTRIMENTI ------*
092870970613     C*  CHIUDO LA BOLLA
092880970613     C     CHIUBO        BEGSR
092890970613     C                   MOVEL     'S'           WAGG              1
092900970613     C*
092910970613     C* NON AGGIORNO COL MANDATO DI INTROITO SE SULLA BOLLA C'E'
092920970613     C*  UN NUMERO DISTINTA ED E' CHIUSA
092930080516     c                   clear                   fvvfcf
092940970613     C     ARBNDC        IFGT      0
092950020911     c                   Z-Add     ArbIfp        Kfgs
092960970613     C                   Z-ADD     ARBNDC        W0050             5 0
092970020911     C     KFVV          CHAIN     FNFVV01L                           30
092980970613     C     *IN30         IFEQ      *OFF
092990970613     C     FVVFCF        ANDEQ     'S'
093000970613     C                   CLEAR                   WAGG
093010970613     C                   ENDIF
093020970613     C                   ENDIF
093030970613     C*
093040970613    4C     WAGG          IFEQ      'S'
093050970613    5C     ARBNMI        IFEQ      0
093060970613     C                   Z-ADD     8888888       ARBNMI
093070970613     C                   Z-ADD     8888888       ARBNDC
093080970613     C                   Z-ADD     DATEU         ARBDDC
093090080516     c                   else
093100080516     c
093110080516     c                   if        arbndc>0 and fvvfcf<>'S'
093120080516     C                   Z-ADD     8888888       ARBNDC
093130080516     c                   endif
093140080516     c
093150970613    5C                   ENDIF
093160970613    4C                   ENDIF
093170970613     C                   ENDSR
093180970613     C*
093190911211     C*--- CONTROLLI FORMATO4 ----------------------------------------*
093200911211     C     CONTR4        BEGSR
093210040123     C**!!!              MOVEL     ' '           CEISV3
093220920319     C                   EXSR      CTRCOM
093230920319     C   90              GOTO      ENDCT4
093240920319     C*
093250911211     C* T A S S A Z I O N E
093260991011     C     *INKN         IFEQ      *ON
093270991011     c* fattura immediata
093280991011     C     *IN14         OREQ      *ON
093290991011     C                   MOVEL     '2'           WBOL              1
093300991011     C                   EXSR      TASSAZ
093310991011     C   KN
093320991011     COR 90              GOTO      ENDCT4
093330991011     C                   END
093340911211     C*
093350990930     C* OBBLIGATORIO IMPORTO DELLA VARIA
093360040123     C**!!!VI2VA1        IFEQ      0
093370040123     C**!!!              MOVEL     MSG(51)       VIDMSG
093380040123     C**!!!              SETON                                        519028
093390040123     C**!!!              GOTO      ENDCT4
093400040123     C**!!!              END
093410991007     C* Se divisa sprotetta significa che manca tariffa: in questo caso
093420991007     C* controllo che la divisa inserita sia = alla divisa della moneta
093430991007     C*  di conto SE CLIENTE  9999
093440991007     C     *IN14         IFEQ      *ON
093450991007     C     *IN07         ANDEQ     *OFF
093460991007     C     VI2DIV        ANDNE     §GEDCN
093470991007     C     VIDKSC        ANDEQ     9999
093480991007     C                   SETON                                        459028
093490991007     C                   MOVEA     MSG(90)       K78
093500991007     C                   MOVEA     §GEDCN        K78(71)
093510991007     C                   MOVEA     K78           VIDMSG
093520991007     C                   GOTO      ENDCT4
093530991007     C                   ENDIF
093540991006     C* DEMANDO IL RESTO DEI CONTROLLI A PGM FNLV16R
093550100120     C                   CLEAR                   dlv1601
093560100120     C                   CLEAR                   Fnlv16DS
093570991006     C                   CLEAR                   DTASV
093580991006     C                   MOVEL     'A'           D17TBO
093590991006     C                   MOVEL     VIDKLP        D17KSC
093600991006     C                   MOVE      VIDKSC        D17KSC
093610991012     C     DFT2BO        IFGT      0
093620991012     C                   Z-ADD     DFT2BO        D17DSP
093630991012     C                   ELSE
093640991006     C                   Z-ADD     ARBDSP        D17DSP
093650991012     C                   ENDIF
093660991006     C                   MOVEL     ARBLNP        D17LNP
093670991006     C                   MOVEL     ARBLNA        D17LNA
093680100120     C                   MOVEL     §3ATB2        D17TBL
093690991006     C                   MOVEL     §3ARBL        D17RBL
093700020503     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
093710040123     C                   MOVEL     §3ASVA        §d17sva
093720040123      * passo che devo controllare la seconda bolla
093730040123     c                   Eval      §d17Tb2 = 'S'
093740100120
093750100120     c* Se si tratta di ffattura immediata , passo cod iso ddestintario
093760100120    1c                   if        *in14
093770100120     c                   Eval      §d17ctriso='S'
093780100120    2c                   if        arbcpi<>*blanks
093790100120     C     'PRIVATO'     SCAN      arbcpi        posiz                    30
093800100120     c* solo numeri
093810100120     c                   select
093820100120    3c                   when      not *in30 and %subst(arbcpi:1:2)>=*zeros
093830100120     c                   eval      §d17iso='IT'
093840100120     c* "PRIVATO" senza iso
093850100120     c                   when      *in30 and posiz<=3
093860100120     c                   eval      §d17iso='$$'
093870100120     c                   other
093880100120     c                   eval      §d17iso= %subst(arbcpi:1:2)
093890100120    3c                   ENDSL
093900100120    2c                   endif
093910100120    1c                   endif
093920100120     c
093930040123     c                   Movel     dlv1601       D17Flo
093940100120     c
093950991006     C                   MOVEL     VI2DIV        D17DIV
093960991006     C                   MOVEL     WDIV          D17DVP
093970991006    1C     §3AFCA        IFEQ      2
093980991006     C                   MOVEL     'S'           D17FCA
093990991006    1C                   ENDIF
094000991006     C                   MOVEL     'E'           D17FIM
094010991006     C                   MOVEL     VI2CEI        D17CEI
094020991006     C                   MOVEL     VI2SV1        VSV(1)
094030040123     C**!!!              MOVEL     VI2VA1        VVA(1)
094040040123     c                   Z-add     Vi2Va1        vva(1)
094050040123     c                   Movel     Vi2Es1        ves(1)
094060040123     c                   Movel     Vi2Sv2        vsv(2)
094070040123     c                   Movel     Vi2Es2        ves(2)
094080040123     c                   Z-add     Vi2Va2        vva(2)
094090040123     c                   MOVEL     Vi2Sv3        vsv(3)
094100040123     c                   Z-add     Vi2Va3        vva(3)
094110040123     c                   Movel     Vi2Es3        ves(3)
094120040123     c                   MOVEL     Vi2Sv4        vsv(4)
094130040123     c                   Z-add     Vi2Va4        vva(4)
094140040123     c                   Movel     Vi2Es4        ves(4)
094150041130     c                   MOVEL     Vi2Sv5        vsv(5)
094160041130     c                   Z-add     Vi2Va5        vva(5)
094170041130     c                   Movel     Vi2Es5        ves(5)
094180041130     c                   MOVEL     Vi2Sv6        vsv(6)
094190041130     c                   Z-add     Vi2Va6        vva(6)
094200041130     c                   Movel     Vi2Es6        ves(6)
094210991006     C                   CALL      'FNLV16R'
094220030612     C                   PARM                    Fnlv16DS
094230991006     C                   PARM                    DTASV
094240991011     C                   MOVEL     D17DIV        VI2DIV
094250991006     C                   MOVEL     D17CEI        VI2CEI
094260991011     C                   MOVEL     O17DEI        DESCEI
094270040123     C**!!!              MOVEL     VSV(1)        VI2SV1
094280040123     C**!!!              MOVEL     VES(1)        CEISV3            1
094290040123      * Ricarico le varie
094300040123     c                   Movel     Vsv(1)        Vi2Sv1
094310040123     c                   Z-add     Vva(1)        Vi2Va1
094320040123     c                   Movel     Ves(1)        Vi2Es1
094330040123     c                   Movel     Vsv(2)        Vi2Sv2
094340040123     c                   Z-add     Vva(2)        Vi2Va2
094350040123     c                   Movel     Ves(2)        Vi2Es2
094360040123     c                   Movel     Vsv(3)        Vi2Sv3
094370040123     c                   Z-add     Vva(3)        Vi2Va3
094380040123     c                   Movel     Ves(3)        Vi2Es3
094390040123     c                   Movel     Vsv(4)        Vi2Sv4
094400040123     c                   Z-add     Vva(4)        Vi2Va4
094410040123     c                   Movel     Ves(4)        Vi2Es4
094420041130     c                   Movel     Vsv(5)        Vi2Sv5
094430041130     c                   Z-add     Vva(5)        Vi2Va5
094440041130     c                   Movel     Ves(5)        Vi2Es5
094450041130     c                   Movel     Vsv(6)        Vi2Sv6
094460041130     c                   Z-add     Vva(6)        Vi2Va6
094470041130     c                   Movel     Ves(6)        Vi2Es6
094480040123      * Controllo se caricata almeno una varia
094490041130do  1c                   Do        6             c
094500040123     c                   If        vsv(c) <> *Blanks
094510040123     c                   Eval      wesvar1 = 'S'
094520040123     c                   EndIf
094530040123    1c                   EndDo
094540040123
094550991006     C     O17ERR        IFNE      *BLANKS
094560991006     C                   SETON                                        90
094570991006     C     O17MSG        IFNE      *BLANKS
094580991006     C                   MOVEL     O17MSG        VIDMSG
094590991006     C                   SETON                                        28
094600991006     C                   ENDIF
094610991006     C     O17ERR        IFEQ      '6'
094620040123     c                   Select
094630040123     c                   When      O17Erv = 1
094640991006     C                   SETON                                        49
094650040123     c                   When      O17Erv = 2
094660040123     C                   SETON                                        63
094670040123     c                   When      O17Erv = 3
094680040123     C                   SETON                                        64
094690040123     c                   When      O17Erv = 4
094700040123     C                   SETON                                        65
094710041130     c                   When      O17Erv = 5
094720041130     C                   SETON                                        66
094730041130     c                   When      O17Erv = 6
094740041130     C                   SETON                                        67
094750040123     c                   EndSl
094760991006     C                   ENDIF
094770991006     C     O17ERR        IFEQ      '1'
094780991006     C                   SETON                                        50
094790991006     C                   ENDIF
094800991006     C     O17ERR        IFEQ      '7'
094810991006     C                   SETON                                        45
094820991006     C                   ENDIF
094830991006     C   90              GOTO      ENDCT4
094840991006     C                   ENDIF
094850991006     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
094860991006    1C  N07VI2DIV        IFNE      WDIV
094870991006     C* CONVERSIONE VARIA
094880991006    3C     VI2VA1        IFEQ      VA2VA1
094890991006     C     VI2VA1        ANDGT     *ZEROS
094900991006     C     WDIV          ANDNE     *BLANKS
094910030612     C                   CLEAR                   YeurcoDS
094920991006     C                   MOVEL     WDIV          YECDVI
094930991006     C                   Z-ADD     VI2VA1        YECIMI
094940991006     C                   MOVEL     VI2DIV        YECDVO
094950991006     C     O17FDC        IFEQ      'S'
094960011114     C                   MOVE      '3'           YECDCO
094970991006     C                   ELSE
094980991006     C                   CLEAR                   YECDCO
094990991006     C                   ENDIF
095000991006     C                   CALL      'YEURCO'
095010030612     C                   PARM                    YeurcoDS
095020991006     C                   Z-ADD     YECIMO        VI2VA1
095030991012     C                   SETON                                        51
095040991006    3C                   ENDIF
095050040123      * Conversione Varia 2
095060040123    2c                   IF        Vi2Va2 = Va2Va2 and Vi2Va2 > *Zeros and
095070040123     c                             wdiv <> *Blanks
095080040123     c                   Clear                   Yeurcods
095090040123     c                   Movel     wdiv          YECDVI
095100040123     c                   Z-add     Vi2Va2        YECIMI
095110040123     c                   Movel     Vi2Div        YECDVO
095120040123     c                   If        O17Fdc = 'S'
095130040123     c                   Movel     '3'           YECDCO
095140040123     c                   EndIf
095150040123     c                   Call      'YEURCO'
095160040123     c                   Parm                    Yeurcods
095170040123     c                   Z-add     YECIMO        Vi2Va2
095180040123     c                   Eval      *In63 = *On
095190040123    2c                   EndIf
095200040123      * Conversione Varia 3
095210040123    2c                   IF        Vi2Va3 = Va2Va3 and Vi2Va3 > *Zeros and
095220040123     c                             wdiv <> *Blanks
095230040123     c                   Clear                   yeurcods
095240040123     c                   Movel     wdiv          YECDVI
095250040123     c                   Z-add     Vi2Va3        YECIMI
095260040123     c                   Movel     Vi2Div        YECDVO
095270040123     c                   If        O17Fdc = 'S'
095280040123     c                   Movel     '3'           YECDCO
095290040123     c                   EndIf
095300040123     c                   Call      'YEURCO'
095310040123     c                   Parm                    yeurcods
095320040123     c                   Z-add     YECIMO        Vi2Va3
095330040123     c                   Eval      *In64 = *On
095340040123    2c                   EndIf
095350040123      * Conversione Varia 4
095360040123    2c                   IF        Vi2Va4 = Va2Va4 and Vi2Va4 > *Zeros and
095370040123     c                             wdiv <> *Blanks
095380040123     c                   Clear                   yeurcods
095390040123     c                   Movel     wdiv          YECDVI
095400040123     c                   Z-add     Vi2Va4        YECIMI
095410040123     c                   Movel     Vi2Div        YECDVO
095420040123     c                   If        O17Fdc = 'S'
095430040123     c                   Movel     '3'           YECDCO
095440040123     c                   EndIf
095450040123     c                   Call      'YEURCO'
095460040123     c                   Parm                    yeurcods
095470040123     c                   Z-add     YECIMO        Vi2Va4
095480040123     c                   Eval      *In65 = *On
095490040123    2c                   EndIf
095500041130      * Conversione Varia 5
095510041130    2c                   IF        Vi2Va5 = Va2Va5 and Vi2Va5 > *Zeros and
095520041130     c                             wdiv <> *Blanks
095530041130     c                   Clear                   yeurcods
095540041130     c                   Movel     wdiv          YECDVI
095550041130     c                   Z-add     Vi2Va5        YECIMI
095560041130     c                   Movel     Vi2Div        YECDVO
095570041130     c                   If        O17Fdc = 'S'
095580041130     c                   Movel     '3'           YECDCO
095590041130     c                   EndIf
095600041130     c                   Call      'YEURCO'
095610041130     c                   Parm                    yeurcods
095620041130     c                   Z-add     YECIMO        Vi2Va5
095630041130     c                   Eval      *In66 = *On
095640041130    2c                   EndIf
095650041130      * Conversione Varia 6
095660041130    2c                   IF        Vi2Va6 = Va2Va6 and Vi2Va6 > *Zeros and
095670041130     c                             wdiv <> *Blanks
095680041130     c                   Clear                   yeurcods
095690041130     c                   Movel     wdiv          YECDVI
095700041130     c                   Z-add     Vi2Va6        YECIMI
095710041130     c                   Movel     Vi2Div        YECDVO
095720041130     c                   If        O17Fdc = 'S'
095730041130     c                   Movel     '3'           YECDCO
095740041130     c                   EndIf
095750041130     c                   Call      'YEURCO'
095760041130     c                   Parm                    yeurcods
095770041130     c                   Z-add     YECIMO        Vi2Va6
095780041130     c                   Eval      *In67 = *On
095790041130    2c                   EndIf
095800040123     C**!!!              Z-ADD     VI2VA1        VA2VA1
095810040123     C**!!!              Z-ADD     VI2VA1        VVA(1)
095820040123     C**!!!              MOVEL     VI2DIV        WDIV
095830991012     C* RIEMETTO COMUNQUE LA VIDEATA SE HANNO VARIATO LA DIVISA
095840991012     C* (POTREI RIEMETTERLA SOLO SE HO CONVERTITO IMPORTI MA LA
095850991012     C* RIEMETTO COMUNQUE PER FARE COME PER LA PRIMA BOLLA)
095860991012     C                   SETON                                        90
095870040123
095880040123     c                   Z-add     Vi2Va1        Va2Va1
095890040123     c                   Z-add     Vi2Va2        Va2Va2
095900040123     c                   Z-add     Vi2Va3        Va2Va3
095910040123     c                   Z-add     Vi2Va4        Va2Va4
095920041130     c                   Z-add     Vi2Va5        Va2Va5
095930041130     c                   Z-add     Vi2Va6        Va2Va6
095940040123     c                   Z-add     Vi2Va1        Vva(1)
095950040123     c                   Z-add     Vi2Va2        Vva(2)
095960040123     c                   Z-add     Vi2Va3        Vva(3)
095970040123     c                   Z-add     Vi2Va4        Vva(4)
095980041130     c                   Z-add     Vi2Va5        Vva(5)
095990041130     c                   Z-add     Vi2Va6        Vva(6)
096000040123     c                   Movel     Vi2Div        Wdiv
096010040123
096020991012     C                   GOTO      ENDCT4
096030991006   X1C                   ELSE
096040991006     C                   Z-ADD     VI2VA1        VA2VA1
096050040123     c                   Z-add     Vi2Va2        Va2Va2
096060040123     c                   Z-add     Vi2Va3        Va2Va3
096070040123     c                   Z-add     Vi2Va4        Va2Va4
096080041130     c                   Z-add     Vi2Va5        Va2Va5
096090041130     c                   Z-add     Vi2Va6        Va2Va6
096100991006    1C                   ENDIF
096110011206      *
096120011206      * Controllo che la somma degli importi immessi non superi il valore
096130011206      * impostato in §GEIMF
096140040123     c**!!!              eval      w0173 = vi2va1
096150040123     c                   xfoot     vva           w0173
096160011206      *      controllo se devo convertire il totale imponibile
096170011206  1b c                   if        vi2div <> §gedcn
096180030612     c                   clear                   YeurcoDS
096190011206     c                   eval      yecdvi = vi2div
096200011206     c                   eval      yecdvo = §gedcn
096210040123     c**!!!              eval      yecimi = vi2va1
096220040123     c                   eval      yecimi = w0173
096230011206     c                   move      decsav        yecdco
096240011206     c                   call      'YEURCO'
096250030612     c                   parm                    YeurcoDS
096260011206  2b c                   if        yecesi = ' '
096270011206     c                   eval      w0173 = yecimo
096280011206  2e c                   endif
096290011206  1e c                   endif
096300011206      *      controllo se l'imponibile supera il limite fatturabile
096310011220  1b c                   if        w0173 > imfsav
096320011206     c                   eval      vidmsg = msg(110)
096330011220     c                   eval      %subst (vidmsg:45:14) = (%editc(imfsav: '4'))
096340011206     c                   eval      %subst (vidmsg:60:3) = §gedcn
096350040123     c**!!!              eval      *in51 = *on
096360011206     c                   eval      *in90 = *on
096370011206     c                   goto      endct4
096380011206  1e c                   endif
096390040123
096400040123     c                   Z-add     w0173         TotImv
096410920319     C*
096420911211     C     ENDCT4        ENDSR
096430911212     C*
096440911212     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE 2X -----------------*
096450911212     C     REGIS4        BEGSR
096460040130     c                   Setoff                                       3709
096470130412     c                   clear                   wpda_ndc
096480130412     c                   clear                   wpda_ddc
096490130412     c                   clear                   wpda_dcm
096500040130
096510950104     C*  GIRO UDATE
096520950104     C                   TIME                    W0140            14 0
096530950104     C* UDATE IN GGMMAAAA
096540950104     C                   MOVE      W0140         WDTGIO            8 0
096550950104     C*
096560950104     C* UDATE IN AAAAMMGG
096570950104     C                   Z-ADD     WDTGIO        G02DAT
096580950104     C                   MOVEL     *BLANK        G02ERR
096590950104     C                   CALL      'XSRDA8'
096600950104     C                   PARM                    WLBDAT
096610950104     C                   MOVEL     G02INV        DATEU             8 0
096620941221     C* PRENDO I VALORI ASSOLUTI
096630941221     C                   MLLZO     1             VIDKLP
096640941221     C                   MLLZO     1             VIDKSC
096650941221     C                   MLLZO     1             VI2VA1
096660040123     C                   MLLZO     1             VI2VA2
096670040123     C                   MLLZO     1             VI2VA3
096680040123     C                   MLLZO     1             VI2VA4
096690041130     C                   MLLZO     1             VI2VA5
096700041130     C                   MLLZO     1             VI2VA6
096710941221     C*
096720950203     C                   MOVEL     KNMUS         ARBPRU
096730920120     C* INVIO PER SEDE
096740170116     C****               MOVEL     'FIARBT0T'    SFILE             8
096750930210     C* ALLOCO OGGETTI
096760170116     C***                EXSR      ALLOC
096770170116     C***91              GOTO      ENDRG4
096780040123      * carico le varie in una sk di comodo
096790040123     c                   ExSr      CarWar1
096800040123      * se c'è n.fattura calcolo
096810040129If  1c                   If        Dft2bo > *Zeros
096820040129     c                   Movel     Dft2bo        ArbDft
096830040123     C                   MOVEL     VI2DIV        ARBDIV
096840040129     C                   MOVEL     VI2DIV        divgei
096850040123     C                   EXSR      RIEGEI
096860040123     C                   CLEAR                   Fnlv04ds
096870040123     C                   MOVEL     'T'           D04FIM
096880040123     C                   Z-ADD     ArbDft        D04DFT
096890040123     C                   MOVEL     §GEAED        D04AED
096900040123     C                   Z-ADD     §GEAVL        D04VAR
096910040123     c                   Z-add     TotImv        D04Imv
096920040123     C                   MOVEL     VI2CEI        D04CEI
096930040123     C                   MOVEL     AR9TIC        D04TIC
096940040123     C                   MOVEL     AR9VCA        D04VCA
096950040123     C                   Z-ADD     AR9CAS        D04CAS
096960040123     C                   MOVEL     '2'           D04SPE
096970040123     C                   MOVEL     §3ATB2        D04TBL
096980040123     C                   MOVEL     VI2DIV        D04DIV
096990040123     C                   MOVEL     '1'           UNO
097000040123     C*
097010040123     C                   CALL      'FNLV04R'
097020040123     C                   PARM                    Fnlv04ds
097030040123     C                   PARM                    DTASV
097040040123     C*
097050040123     C     D04BOL        IFGT      *ZEROS
097060040123     C                   ADD       1             C
097070040123     C                   MOVEL     §$2BOL        WSV(C)
097080040123     C                   Z-ADD     D04BOL        WVA(C)
097090040123     C                   ENDIF
097100040123     C     D04IVA        IFGT      *ZEROS
097110040123     C                   ADD       1             C
097120040123     C                   MOVEL     §$2IVA        WSV(C)
097130040123     C                   Z-ADD     D04IVA        WVA(C)
097140040123     C                   ENDIF
097150040123    1c                   EndIf
097160040123
097170040123     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
097180040123    1C     C             IFGT      0
097190040123     C                   SETON                                        09
097200040123     C*
097210040123     C* INVIO PER SEDE
097220040123     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
097230170116    2C**   C             IFGT      3
097240040123     C* MI SALVO I CAMPI DELL'FIARBT
097250170116     C***                MOVEL     SOVR(1)       SOARBT
097260170116     C***                MOVEL     SALC(1)       SAARBT
097270170116     C***                MOVEL     SDLC(1)       SDARBT
097280040123     C*
097290170116     C***                MOVEL     'FIARBU0T'    SFILE
097300040123     C* ALLOCO OGGETTI
097310170116     C***                EXSR      ALLOC
097320170116     c***                if        *in91
097330170116     C***                MOVEL     SDARBT        SDLC(1)
097340170116     c***                exsr      DLCSED
097350170116     C***                SETON                                        2891
097360170116     C***                GOTO      ENDRG4
097370170116     c***                endif
097380070515     c
097390170116    2C****               ENDIF
097400040123    1C                   ENDIF
097410911212     C*
097420991007     C                   Z-ADD     DATEU         ARBDTV
097430991007     C                   TIME                    ARBORV
097440991007     C                   MOVEL     VIDCVR        ARBCVB
097450991011     C                   MOVEL     '2'           ARBTRC
097460991011     C                   MOVEL     '2'           AR6TRC
097470991007     C                   CLEAR                   ARBFST
097480991007     C                   CLEAR                   ARBDDS
097490991007     C                   CLEAR                   ARBQFT
097500991007     C                   CLEAR                   ARBVMD
097510991007     C                   CLEAR                   ARBVAD
097520991007     C                   CLEAR                   ARBIAS
097530991007     C                   CLEAR                   ARBVAS
097540911212     C*
097550941212     C* SE DA STORICIZZARE --> STORICIZZO
097560991007     C     WAR62         IFEQ      'E'
097570060130    1C     D66FSA        ANDEQ     'S'
097580060130     C**   *IN10         IFEQ      *ON
097590060130    1C**   D66FSA        ANDEQ     'S'
097600060130     C**   *IN10         OREQ      *OFF
097610060130    1C**   D66FSP        ANDEQ     'S'
097620991007     C                   EXSR      STOARB
097630941212     C                   END
097640060130     C**                 END
097650060302     c* storicizzazione motivazione
097660060302     c     d66mtv        ifeq      'S'
097670060302     c                   exsr      sto_mtv
097680060302     c                   endif
097690040123     C**
097700040123     C* INVIO VARIAZIONE --> FIARBU0T
097710040123     C**
097720040123    1C     *IN09         IFEQ      *ON
097730040123     C     C             ANDGT     3
097740040123     C                   Z-ADD     4             C
097750040123     C*
097760040123    3C     D66FFI        IFEQ      'S'
097770040123     C                   Z-ADD     4             C
097780170116     C****               OPEN      FIARBU0T
097790040123     C*
097800040123     C* NE SCRIVO FIN TANTO CHE CE NE SONO
097810040123    4C     WSV(C)        DOWNE     ' '
097820040123     C                   MOVEL     WSV(C)        ARBSVN
097830040123     C                   Z-ADD     WVA(C)        ARBVAN
097840040123     C                   WRITE     FIARBUTT
097850040123     C                   ADD       1             C
097860040123    4C                   ENDDO
097870040123     C*
097880170116     C****               CLOSE     FIARBU0T
097890040123     C* DISALLOCO OGGETTO MEMBRO PER SEDE
097900170116     C****               EXSR      DLCSED                                       FIARBU0T
097910040123    3C                   ENDIF
097920040123     C* REIMPOSTO I CAMPI
097930170116     C***                MOVEL     SOARBT        SOVR(1)
097940170116     C***                MOVEL     SAARBT        SALC(1)
097950170116     C***                MOVEL     SDARBT        SDLC(1)
097960040123    1C                   ENDIF
097970991011     C*
097980991011     C                   MOVEL     VIDKLP        ARBKSC
097990991007     C                   MOVE      VIDKSC        ARBKSC
098000991007     C                   CLEAR                   ARBPOR
098010991007     C                   MOVEL     VI2DIV        ARBDIV
098020040123     C**!!!              MOVEL     VI2SV1        ARBSV1
098030040123     C**!!!              MOVEL     VI2VA1        ARBVA1
098040040123     C                   MOVEL     WSV(1)        ARBSV1
098050040123     C                   Z-ADD     WVA(1)        ARBVA1
098060040123     C                   MOVEL     WSV(2)        ARBSV2
098070040123     C                   Z-ADD     WVA(2)        ARBVA2
098080040123     C                   MOVEL     WSV(3)        ARBSV3
098090040123     C                   Z-ADD     WVA(3)        ARBVA3
098100991007     C                   MOVEL     VI2CEI        ARBCEI
098110991007     C                   MOVEL     VIDCTR        ARBCTR
098120040123     C**!!!              Z-ADD     VI2VA1        ARBIMV
098130040123     C                   Z-ADD     TotImv        ARBIMV
098140040123     C**!!!              CLEAR                   ARBSV2
098150040123     C**!!!              CLEAR                   ARBVA2
098160040123     C**!!!              CLEAR                   ARBSV3
098170040123     C**!!!              CLEAR                   ARBVA3
098180040129     C**!!!              CLEAR                   ARBALI
098190040129     C**!!!              CLEAR                   ARBIFT
098200991007     C                   Z-ADD     VI2NFT        ARBNFT
098210991007     C                   Z-ADD     VI2FIV        ARBFIV
098220991007     C                   Z-ADD     AR6DFT        ARBDFT
098230991013     C* SE TOTALE IMPONIBILE ERA IMMESSO DALLA PARTENZA ED E'
098240991013     C* STATO MODIFICATO TOLGO LA 'P'
098250991013     C     AR6FIM        IFEQ      'P'
098260991013     C     ARBIMV        ANDNE     AR6IMV
098270991013     C     *IN10         ANDEQ     *ON
098280991013     C                   MOVE      *BLANKS       ARBFIM
098290991013     C                   ELSE
098300991013     C* ALTRIMENTI CI METTO QUELLO CHE C'ERA
098310991013     C                   MOVE      AR6FIM        ARBFIM
098320991013     C* SE NON ESISTE FIAR6 E SONO IN PARTENZA IMPOSTO 'P'
098330991013     C     WAR62         IFNE      'E'
098340991013     C     *IN10         ANDEQ     *OFF
098350991013     C                   MOVEL     'P'           ARBFIM
098360991013     C                   END
098370991013     C                   ENDIF
098380040129
098390040130if  1c                   If        Arbdft > 0
098400040129     C* SE CAMBIATO TOTALE FATTURA RISTAMPO IN AUTOMATICO LA BOLLA
098410040129     C     D04IFT        COMP      AR6IFT                             3737
098420040129
098430040129     C                   MOVE      D04ALI        ARBALI
098440040129     C                   Z-ADD     D04IFT        ARBIFT
098450040130     C                   z-add     D04IMV        ARBIMV
098460051111     c                   select
098470051111     C     ARBVA1        WHENGT    0
098480051111     C     ARBSV1        ANDNE     §$2BOL
098490051111     C     ARBSV1        ANDNE     §$2IVA
098500051111     C                   ADD       D04POR        ARBVA1
098510051111     C     ARBVA2        WHENGT    0
098520051111     C     ARBSV2        ANDNE     §$2BOL
098530051111     C     ARBSV2        ANDNE     §$2IVA
098540051111     C                   ADD       D04POR        ARBVA2
098550051111     C     ARBVA3        WHENGT    0
098560051111     C                   ADD       D04POR        ARBVA3
098570051111    3C                   ENDSL
098580040130     c                   EndIf
098590911212     C*
098600070503     C     D66FFI        IFEQ      'S'
098610170116     C***                OPEN      FIARBT0T
098620991007     C                   WRITE     FIARBTTT
098630170116     C***                CLOSE     FIARBT0T
098640911212     C* DISALLOCO OGGETTO
098650170116     C****               EXSR      DLCSED                                       FIARBT0T
098660911212     C                   END
098670911212     C*
098680911212     C* AGGIORNO
098690991013     C                   Z-ADD     ARBKSC        AR6KSC
098700991013     C                   Z-ADD     ARBCTR        AR6CTR
098710991013     C                   MOVEL     ARBDIV        AR6DIV
098720991013     C                   Z-ADD     ARBPOR        AR6POR
098730991013     C                   MOVEL     ARBSV1        AR6SV1
098740991013     C                   MOVEL     ARBSV2        AR6SV2
098750991013     C                   MOVEL     ARBSV3        AR6SV3
098760991013     C                   Z-ADD     ARBVA1        AR6VA1
098770991013     C                   Z-ADD     ARBVA2        AR6VA2
098780991013     C                   Z-ADD     ARBVA3        AR6VA3
098790991013     C                   Z-ADD     ARBIMV        AR6IMV
098800991013     C                   Z-ADD     ARBALI        AR6ALI
098810991013     C                   MOVEL     ARBCEI        AR6CEI
098820991013     C                   Z-ADD     ARBIFT        AR6IFT
098830991013     C                   Z-ADD     ARBFIV        AR6FIV
098840991013     C                   Z-ADD     ARBNFT        AR6NFT
098850991013     C                   Z-ADD     ARBDFT        AR6DFT
098860991013     C                   MOVEL     ARBFIM        AR6FIM
098870040123     C*
098880040123     C* CANCELLO LE VARIE NELL'FIAR7
098890040123    1C     WAR72         IFEQ      'E'
098900040123     C                   MOVEL     '2'           WTRC
098910040123     C     KARB2         SETLL     FIAR7000
098920040123     C     KARB2         READE     FIAR7000                               30
098930040123     C*
098940040123    2C     *IN30         DOWEQ     *OFF
098950040123     C                   DELETE    FIAR7000
098960040123     C     KARB2         READE     FIAR7000                               30
098970040123    2C                   ENDDO
098980040123    1C                   ENDIF
098990040123     C*
099000040123     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
099010040123    1C     WSV(4)        IFNE      ' '
099020040123     C                   Z-ADD     4             C
099030040123     C                   CLEAR                   FIAR7000
099040040123     C                   MOVEL     ARBAAS        AR7AAS
099050040123     C                   MOVEL     ARBLNP        AR7LNP
099060040123     C                   MOVEL     ARBNRS        AR7NRS
099070040123     C                   MOVEL     ARBNSP        AR7NSP
099080040123     C                   MOVEL     '2'           AR7TRC
099090040123    2C     WSV(C)        DOWNE     ' '
099100040123     C                   MOVEL     WSV(C)        AR7SVN
099110040123     C                   MOVEL     WVA(C)        AR7VAN
099120040123     C                   WRITE     FIAR7000
099130040123     C                   ADD       1             C
099140040123    2C                   ENDDO
099150040123    1C                   ENDIF
099160040123
099170990930     C     WAR62         IFEQ      'E'
099180991007     C                   UPDATE    FIAR6000
099190941212     C                   ELSE
099200991013     C                   CLEAR                   AR6ATB
099210991013     C                   Z-ADD     ARBAAS        AR6AAS
099220991013     C                   Z-ADD     ARBLNP        AR6LNP
099230991013     C                   Z-ADD     ARBNRS        AR6NRS
099240991013     C                   Z-ADD     ARBNSP        AR6NSP
099250991013     C                   MOVEL     '2'           AR6TRC
099260991007     C                   WRITE     FIAR6000
099270941212     C                   ENDIF
099280930211     C*
099290991013     C                   UNLOCK    FNARB01L
099300920204     C* RISTAMPO FATTURA
099310070309     c* ristampo fattura immediata se variata(37 on),
099320070309     c*   e se non attiva GEO
099330130321     C***  *IN37         IFEQ      *ON
099340130321     c***  FGSddaSI      andeq     ' '
099350130321     C***                CLEAR                   FNLSB5DS
099360130321     C***                MOVEL     'V'           DB0RIS
099370130321     C***                MOVEL     D48TBO        DB0TBO
099380130321     C***                MOVEL     ARBAAS        DB0AAS
099390130321     C***                MOVEL     ARBLNP        DB0LNP
099400130321     C***                MOVEL     ARBNRS        DB0NRS
099410130321     C***                MOVEL     ARBNSP        DB0NSP
099420130321     C***                CALL      D90PSL
099430130321     C***                PARM                    FNLSB5DS
099440130321     C***                END
099450130411
099460130411     c                   eval      wpda_ndc=arbndc
099470130411     c                   eval      wpda_ddc=arbddc
099480130411     c                   eval      wpda_dcm=arbdcm
099490130411     c* Richiamo routine per invio variazione a pda
099500130411     c                   exsr      sr_pda
099510911212     C*
099520911212     C     ENDRG4        ENDSR
099530920116     C*
099540920116     C*--- CONTROLLI FORMATO5 ----------------------------------------*
099550920117     C     CONTR5        BEGSR
099560920116     C* CODICE BOLLA NUOVO
099570941102    1C     *IN21         IFEQ      *OFF
099580941102     C*
099590941213     C     '?'           SCAN      NEWCBO                                 90
099600941213    2C     *IN90         IFEQ      *ON
099610941213     C                   MOVEL     CODUT         §KUT
099620920116     C                   MOVEL     '3A'          §COD
099630941102     C                   MOVE      *BLANKS       NEWCBO
099640991011     C                   CLEAR                   §KEY
099650920116     C                   CALL      'X§TABER'
099660920116     C                   PARM                    §KUT
099670920116     C                   PARM                    §COD
099680920116     C                   PARM                    §KEY
099690941209     C                   PARM                    §DES
099700941102     C                   MOVEL     §KEY          NEWCBO
099710941102     C                   MOVEL     §DES          DENCBO
099720920116     C                   SETON                                        90
099730920116     C                   GOTO      ENDCT5
099740941102    2C                   END
099750920116     C*
099760920116     C                   MOVEL     '3A'          COD
099770941102     C                   MOVEL(P)  NEWCBO        KEY
099780941212     C                   EXSR      CTABEL
099790920121     C*
099800941212    2C     *IN30         IFEQ      *ON
099810941102     C                   MOVEL     MSG(16)       VIDMSG
099820941102     C                   SETON                                        409028
099830941102     C                   GOTO      ENDCT5
099840941102    2C                   ENDIF
099850920117     C*
099860920116     C* CODICE NUOVO E VECCHIO NON POSSONO ESSERE UGUALI
099870940614     C* SE PGM RICHIAMATO NON CONTROLLO
099880941212    2C  N20OLDCBO        IFEQ      NEWCBO
099890941102     C                   MOVEL     MSG(17)       VIDMSG
099900941102     C                   SETON                                        409028
099910920116     C                   GOTO      ENDCT5
099920941102    2C                   END
099930920116     C*
099940920116     C                   MOVEL     TBLUNI        DS3A
099950920304     C*
099960941102     C                   MOVEL     §3ADES        DENCBO                          DESCRIZIONE
099970941102     C*
099980920116     C* NON POSSO TRASFORMARE IN BOLLA RECUPERO DA NON RECUPERO E VICEV
099990940322     C* SE RICHIAMATO PERMETTO TUTTO
100000941102    2C     *IN20         IFEQ      *OFF
100010940322     C*
100020941102    3C     §3ARBL        IFEQ      'R'
100030941102     C     SAVRBL        ANDNE     'R'
100040941102     C*
100050941102     C     §3ARBL        ORNE      'R'
100060941102     C     SAVRBL        ANDEQ     'R'
100070941102     C                   MOVEL     MSG(18)       VIDMSG
100080941102     C                   SETON                                        409028
100090920116     C                   GOTO      ENDCT5
100100941102    3C                   END
100110941102    2C                   END
100120920128     C*
100130941102     C* NON POSSO FARE VARIAZIONI DI PORTO
100140941212     C                   MOVEL     §3ATB1        NSCTB1            1
100150941212     C                   MOVEL     SAVTB1        OSCTB1            1
100160920116     C*
100170941102    2C     NSCTB1        IFNE      OSCTB1
100180941102     C                   MOVEL     MSG(19)       VIDMSG
100190941213     C                   SETON                                        409028
100200920128     C                   GOTO      ENDCT5
100210941102    2C                   ENDIF
100220150413     c* non ammesso contrasegno se bolle per EXPO
100230151119     c*                  if        §3afca=1 and (arbtc1='Z' or arbtc2='Z')
100240151119     C*                  MOVEL     MSG(159)      VIDMSG
100250151119     C*                  SETON                                        409028
100260151119     C*                  GOTO      ENDCT5
100270151119     c*                  endif
100280000510     C*
100290000510     C* BOLLE LNP POSTE: SOLO SPEDIZIONI FRANCIO
100300131008     C**   LNPNTW        IFEQ      'PPT'
100310131008     C**   NSCTB1        ANDNE     'F'
100320131008     C**                 MOVEL     MSG(30)       VIDMSG
100330131008     C**                 SETON                                        409028
100340131008     C**                 GOTO      ENDCT5
100350131008     C**                 ENDIF
100360000718     C* BOLLE LNP POSTE CON SERIE : NN POSSO TRASFRMARE IN C/S
100370131008     C**   LNPNTW        IFEQ      'PPT'
100380131008     C**   ARBNRS        ANDGT     0
100390131008     C**   §3ARBL        ANDEQ     'C'
100400131008     C**                 MOVEL     MSG(96)       VIDMSG
100410131008     C**                 SETON                                        409028
100420131008     C**                 GOTO      ENDCT5
100430131008     C**                 ENDIF
100440000925     C* BOLLE LNP POSTE: NON POSSO FARE BOLLE DOPPIE
100450131008     C**   LNPNTW        IFEQ      'PPT'
100460131008     C**   §3ATB2        ANDNE     '  '
100470131008     C**                 MOVEL     MSG(97)       VIDMSG
100480131008     C*+                 SETON                                        409028
100490131008     C**                 GOTO      ENDCT5
100500131008     C**                 ENDIF
100510050414     C* Se prepagato, non posso trasformare in C/S
100520050414     C     §3ARBL        ifeq      'C'
100530050414     c     war6          andeq     'E'
100540050414     c     ar6dft        andgt     0
100550060213     c     d48cvb        andne     'KM'
100560050414     C                   MOVEL     MSG(133)      VIDMSG
100570050414     C                   SETON                                        409028
100580050414     C                   GOTO      ENDCT5
100590050414     C                   ENDIF
100600950407     C*
100610950407     C* MEMORIZZO VARIAZIONE CODICE BOLLA
100620950407     C     NEWCBO        IFNE      SAVCBO
100630950407     C                   MOVE      *BLANKS       COM01A
100640950407     C                   MOVEL     NEWCBO        SAVCBO
100650950407     C                   END
100660941102    1C                   ENDIF
100670920120     C*
100680920121     C* CONTROLLO CONTRASSEGNO
100690941102    1C     §3AFCA        IFEQ      0
100700941213     C     VIDCAS        ANDNE     0
100710941213     C                   MOVEL     MSG(20)       VIDMSG
100720941213     C                   SETON                                        429028
100730941213     C                   GOTO      ENDCT5
100740941213    1C                   ENDIF
100750961204     C* NON POSSO INSERIRE IL C/ASSEGNO PER BOLLE DI RESO
100760961204    1C     §3AFCA        IFGT      0
100770980105     C     ARBFBR        IFEQ      'S'
100780961204     C                   MOVEL     MSG(73)       VIDMSG
100790961204     C                   SETON                                        429028
100800961204     C                   GOTO      ENDCT5
100810961204    1C                   ENDIF
100820980105     C* NON POSSO INSERIRE IL C/A SE BOLLE LEGATA CON LNP=LNA
100830980105     C  N21KARB          CHAIN     FNLBL01L                           30
100840980105    4C  N21*IN30         IFEQ      *OFF
100850980105     C     LBLLAP        ANDEQ     LBLLAN
100860980105     C                   MOVEL     MSG(78)       VIDMSG
100870980105     C                   SETON                                        429028
100880980105     C                   GOTO      ENDCT5
100890980105    1C                   ENDIF
100900980105     C                   ENDIF
100910941102     C*
100920941213     C* SE eliminato importo C/A, elimiare anche tutti i campi relativi
100930011018    1C     VIDCAS        IFEQ      0
100940011018    2C     VIDTIC        IFNE      *BLANKS
100950011018     C     VIDGCA        ORNE      '  '
100960011018     C                   MOVEL     MSG(56)       VIDMSG
100970011018     C                   SETON                                        419028
100980011018     C                   GOTO      ENDCT5
100990011018    2C                   END
101000011018    1C                   END
101010941102     C*
101020931118     C* IMPOSSIBILE ELIMINARE O VARIARE C/A SE GIA' INCASSATO
101030941220     C     SAVFCA        IFNE      0
101040941220     C     ARBICC        ANDNE     ' '
101050941220     C     ARBICC        ANDNE     'R'
101060941220     c*
101070941102    2C     VIDCAS        IFNE      AR9CAS
101080941102     C     VIDTIC        ORNE      AR9TIC
101090941102     C     VIDVCA        ORNE      AR9VCA
101100941102     C     VIDGCA        ORNE      AR9GCA
101110941102     C                   MOVEL     MSG(22)       VIDMSG
101120941102     C                   SETON                                        429028
101130931118     C                   GOTO      ENDCT5
101140941102    2C                   ENDIF
101150941102    1C                   ENDIF
101160920120     C*
101170920204     C* CAUSALE KB NON CONTROLLA ESISTENZA O MENO DEL C/A: OK TUTTO
101180941102    1C     VIDCVR        IFNE      'KB'
101190931227     C*
101200941102     C* SE NON RICHIAMATO NON POSSO USARE CERTI TIPI BOLLA
101210971229     C* NON CONTROLLO PER VARIAZIONE SOLO DI IMPORTO
101220971229     C  N21
101230971229    2CANN20§3AUCB        IFEQ      'N'
101240941102     C                   MOVEL     MSG(16)       VIDMSG
101250941102     C                   SETON                                        409028
101260931227     C                   GOTO      ENDCT5
101270941102    2C                   END
101280920204     C*
101290941102    2C     §3AFCA        IFNE      0
101300941212     C     VIDCAS        ANDEQ     0
101310941102     C                   MOVEL     MSG(21)       VIDMSG
101320941102     C                   SETON                                        429028
101330920120     C                   GOTO      ENDCT5
101340941102    2C                   END
101350941102    1C                   END
101360941213     C**
101370941213     C* SE IMMESSO IMPORTO C/A , CONTROLLO I CAMPI RELATIVI
101380941213     C**
101390941213    1C     VIDCAS        IFGT      0
101400950120     C* SE ACCESO INDICATORE DI VARIAZIONE C/ASS LO SALVO
101410950120     C     *IN95         IFEQ      *ON
101420950120     C                   MOVEL     '1'           WIN95             1
101430950120     C                   ENDIF
101440140515     c                   clear                   trulintds
101450140515     c                   eval      iinttip='B'
101460140515     c                   eval      iinttic=vidtic
101470140515     c                   eval      iintrscm=v1crsm
101480140515     c                   eval      iintrsco=arbrmo
101490140515     c                   eval      IINTAAS=d48aas
101500140515     c                   eval      IINTLNP=d48lnp
101510140515     c                   eval      IINTNRS=d48nrs
101520140515     c                   eval      IINTNSP=d48nsp
101530140515     c                   call      'TRULINTR'
101540140515     c                   parm                    trulintds
101550950120     C*
101560920204     C* CONTROLLO TIPO INCASSO C/A
101570060705     C* richiamo pgm TRULTICR
101580060705     c                   clear                   trulticds
101590060705     c                   clear                   ds1a
101600060705     c                   eval      iticTBO='A'
101610060705     c                   eval      iticlnp=arblnp
101620060705     c                   eval      iticlna=arblna
101630060705     c                   eval      iticlnantw=lnantw
101640060705     c                   eval      iticnzd=arbnzd
101650090707     c                   eval      iticprd=arbprd
101660090707     c                   eval      iticcad=arbcad
101670090707     c                   eval      iticlod=arblod
101680090707     c                   eval      iticind=arbind
101690090707     c                   eval      iticrsd=arbrsd
101700060705     c                   eval      iticdsp=arbdsp
101710130827    3C***  ARBCCM        IFGT      0
101720130827     C***                MOVEL     ARBCCM        iticccm
101730130827     C***                ELSE
101740130827     C***                MOVEL     ARBKSC        iticccm
101750130827    3C***                END
101760130827     c* in iticccm passo il MITTENTE: CCM se assegnato, KSC se franco
101770130827     c                   if        flgtb1='F'
101780130827     c                   z-add     arbksc        iticccm
101790130827     c                   else
101800130827     c                   z-add     arbccm        iticccm
101810130827     c                   if        iticccm=0
101820130827     c                   z-add     8888          iticccm
101830130827     c                   movel     arblnp        iticccm
101840130827     c                   endif
101850130827     c                   endif
101860140515     c* se il beneficiario del c/a è l'ordinante allora passo questo in iticccm
101870140515     c                   if        ointtipi='R'
101880140515     c                   move      ointcdi       iticccm
101890140515     c                   endif
101900060705     c                   eval      itictic=vidtic
101910060705     c                   eval      iticticbol=ar9tic
101920060705     c                   eval      iticdiv=vidvca
101930060705     c                   eval      iticcas=vidcas
101940060705     c                   eval      iticpgm='FNLR48R'
101950060705     c                   call      'TRULTICR'
101960060705     c                   parm                    TRULTICDS
101970060705     c                   parm                    ds1a
101980060705     c
101990060705     c                   if        oticric='T'
102000060705     c                   movel     otictic       vidtic
102010060705     C                   SETON                                        4190
102020060705     C                   GOTO      ENDCT5
102030060705     c                   endif
102040060705     c                   if        oticric='D'
102050060705     c                   movel     oticdiv       vidvca
102060060705     C                   SETON                                        4390
102070060705     C                   GOTO      ENDCT5
102080060705     c                   endif
102090060705     c* Errore tipo incasso
102100091211     c                   if        oticerr='T' or oticerr='C'
102110060705     C                   MOVEL     oticmsg       VIDMSG
102120060705     C                   SETON                                        419028
102130060705     C                   GOTO      ENDCT5
102140060705    2C                   ENDIF
102150060705     c* Errore divisa
102160060705     c                   if        oticerr='D'
102170060705     C                   MOVEL     oticmsg       VIDMSG
102180060705     C                   SETON                                        439028
102190060705     C                   GOTO      ENDCT5
102200060705    2C                   ENDIF
102210011010      *
102220941102     C*
102230941102     C* SE HO MODIFICATO LA VALUTA DEVO RIDIGITARE IL C/A
102240941102     C* INDICATORE DI CHANGE 95 ON
102250941213    2C     VARVCA        IFNE      VIDVCA
102260950120     C     WIN95         ANDEQ     *BLANKS
102270950120     C*
102280941219     C     VIDVCA        IFNE      *BLANKS
102290941219     C     VIDCAS        ORNE      0
102300941102     C                   MOVEL     MSG(28)       VIDMSG
102310941215     C                   SETON                                        429028
102320941102     C                   GOTO      ENDCT5
102330941213    2C                   ENDIF
102340941219    2C                   ENDIF
102350941213     C*
102360011206
102370011206      * pulisco il flag x la forzatura c/assegno forzabile
102380011206     c                   if        vidcas <> sav1_vidcas
102390011206     c                   clear                   forzacas
102400011206     c                   endif
102410011206      *-------------------
102420011206  2b c                   if        vidcas <> 0
102430011206     c                   eval      sav1_vidcas = vidcas
102440011206      * Controllo il c/assegno tramite il trul21r
102450011206     c                   clear                   trul21ds
102460011206     c                   eval      i21tla = 'L'
102470011206     c                   if        *in21 = *off
102480011206     c                   eval      i21cbo = newcbo
102490011206     c                   else
102500011206     c                   eval      i21cbo = arbcbo
102510011206     c                   endif
102520011206     c                   eval      i21tsp = arbtsp
102530011206     c                   eval      i21lnp = arblnp
102540011206     c                   eval      i21nzm = arbnzm
102550011206     c                   eval      i21lna = arblna
102560011206     c                   eval      i21nzd = arbnzd
102570130827     c*****              eval      i21ksc = arbksc
102580011206     c                   movel     arbctr        i21ctr
102590011206     c                   eval      i21tic = vidtic
102600011206     c                   eval      i21imp = vidcas
102610011206     c                   eval      i21div = vidvca
102620011206     c                   eval      i21ute = knmus
102630011206     c                   eval      i21pgm = nompgm
102640130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
102650130827     c                   if        flgtb1='F'
102660130827     c                   z-add     arbksc        i21ksc
102670130827     c                   else
102680130827     c                   z-add     arbccm        i21ksc
102690130827     c                   if        i21ksc=0
102700130827     c                   z-add     8888          i21ksc
102710130827     c                   movel     arblnp        i21ksc
102720130827     c                   endif
102730130827     c                   endif
102740011206     c                   call      'TRUL21R'
102750011206     c                   parm                    trul21ds
102760011206      * controllo che non sia minore del limite previsto
102770011206  3b c                   if        o21fmn <> *blanks
102780011206     c                   eval      vidmsg = msg(108)
102790011210     c                   eval      %subst (vidmsg:31:14) = (%editc(o21lmn: '4'))
102800011206     c                   eval      *in42 = *on
102810011206     c                   eval      *in90 = *on
102820011206     c                   eval      *in28 = *on
102830011206     c                   goto      endct5
102840011206  3e c                   endif
102850011206      * controllo che non sia maggiore del limite previsto
102860011206      * non forzabile
102870011206  3b c                   if        o21fx2 <> *blanks
102880011206     c                   eval      vidmsg = msg(91)
102890011210     c                   eval      %subst (vidmsg:47:14) = (%editc(o21lx2: '4'))
102900011206     c                   eval      *in42 = *on
102910011206     c                   eval      *in90 = *on
102920011206     c                   eval      *in28 = *on
102930011206     c                   goto      endct5
102940011206  3e c                   endif
102950050301      * controllo che non sia maggiore del limite per bancanri bartolini no
102960050301      * abilitati da sede, non forzabile
102970050301  3b c                   if        o21fb <> *blanks
102980050301     c                   eval      vidmsg = msg(132)
102990050301     c                   eval      %subst (vidmsg:26:14) = (%editc(o21lb: '4'))
103000050301     c                   eval      *in42 = *on
103010050301     c                   eval      *in90 = *on
103020050301     c                   eval      *in28 = *on
103030050301     c                   goto      endct5
103040050301  3e c                   endif
103050011206      * controllo che non sia maggiore del limite previsto
103060011206      * forzabile
103070011206  3b c                   if        o21fx1 <> *blanks and forzacas = *blanks
103080011206     c                   eval      forzacas = '1'
103090011206     c                   eval      vidmsg = msg(109)
103100011210     c                   eval      %subst (vidmsg:24:14) = (%editc(o21lx1: '4'))
103110011206     c                   eval      *in42 = *on
103120011206     c                   eval      *in90 = *on
103130011206     c                   eval      *in28 = *on
103140011206     c                   goto      endct5
103150011206  3e c                   endif
103160011206  2e c                   endif
103170011206
103180011120      *
103190011120      * Se bolla giacente e modifica 'KA' o 'KP' l'importo immesso (convertito) deve
103200011120      * essere uguale all'originario
103210011120  2b c                   if        *in04 = *ON  and (vidcvr = 'KA' or
103220011120     c                             vidcvr = 'KP')
103230011120     c                   z-add     vidcas        w0173
103240011120      * Cerco i decimali previsti per la divisa originaria
103250011120     c                   movel     'CV'          cod
103260011120     c                   movel(p)  ar9vca        key
103270011120     c                   exsr      ctabel
103280011120     c  n30              movel     tbluni        dscv
103290011120     c   30              clear                   dscv
103300011120      * Controlla la divisa
103310011120  3b c                   if        ar9vca <> vidvca
103320011120      * Converto il contrassegno inserito a video nella divisa originaria
103330030612     c                   clear                   YeurcoDS
103340011120     c                   movel     vidvca        yecdvi
103350011120     c                   movel     ar9vca        yecdvo
103360011120     c                   z-add     vidcas        yecimi
103370011120     c                   move      §cvdec        yecdco
103380011120     c                   call      'YEURCO'
103390030612     c                   parm                    YeurcoDS
103400011120  4b c                   if        yecesi = ' '
103410011120     c                   z-add     yecimo        w0173
103420011120  4e c                   endif
103430011120  3e c                   endif
103440011120      * Calcolo la differenza tra i 2 contrassegni
103450011120     c                   eval      wdiffe = (ar9cas - w0173)
103460011120     c                   mllzo     1             wdiffe
103470020103      * Converto la differenza nella divisa di conto
103480011120  3b c                   if        ar9vca <> §gedcn
103490030612     c                   clear                   YeurcoDS
103500020103     c                   movel     ar9vca        yecdvi
103510020103     c                   movel     §gedcn        yecdvo
103520011120     c                   z-add     wdiffe        yecimi
103530020103     c                   move      decsav        yecdco
103540011120     c                   call      'YEURCO'
103550030612     c                   parm                    YeurcoDS
103560011120  4b c                   if        yecesi = ' '
103570011120     c                   z-add     yecimo        wdiffe
103580011120  4e c                   endif
103590011120  3e c                   endif
103600020103      *
103610020103      * Se differenza maggiore del valore in tabella ERRORE
103620011120  3b c                   if        wdiffe > dfcsav
103630011120     c                   movel     msg(103)      vidmsg
103640011120     c                   seton                                        429028
103650011120     c                   goto      endct5
103660011120  3e c                   endif
103670011120  2e c                   endif
103680011120
103690991021     C**
103700991021     C                   MOVEL     VIDVCA        VARVCA
103710991021     C                   CLEAR                   WIN95
103720941102     c**
103730941102     C* PARTICOLARITA' C/ASSEGNO
103740941102     c**
103750941213    2C     VIDGCA        IFNE      *BLANKS
103760941102     C     VIDGCR        ORNE      *BLANKS
103770941102     C*
103780941102     C* non posso indicarla se NON IMMESSO C/A
103790941213    3C     VIDCAS        IFEQ      0
103800941102     C     VIDGCA        ANDNE     *BLANKS
103810941102     C                   SETON                                        449028
103820941102     C                   MOVEL     MSG(29)       VIDMSG
103830941102     C                   GOTO      ENDCT5
103840941213    3C                   ENDIF
103850941102     C*
103860941102     C* RICHIAMO IL PRG DI RICERCA
103870941213    3C     VIDGCR        IFEQ      '?'
103880030612     C                   CLEAR                   DS7pqrs
103890941102     C                   MOVEL     'S'           DS7RIC
103900941102     C                   MOVEL     'V'           DS7GES
103910941213    4C     ARBCCM        IFGT      0
103920941102     C                   MOVEL     ARBCCM        DS7KSC
103930941102     C                   ELSE
103940941102     C                   MOVEL     ARBKSC        DS7KSC
103950941213    4C                   ENDIF
103960941102     C                   MOVEL     VIDGCA        DS7COD
103970030612     C                   MOVEL     DS7pqrs       KPJBU
103980941102     C                   CALL      'TRTB72R'
103990941102     C                   PARM                    KPJBA
104000030612     C                   MOVEL     KPJBU         DS7pqrs
104010941102     C*
104020941102     C     VIDGCA        IFEQ      *BLANKS
104030941102     C                   MOVEL     DS7COD        VIDGCA
104040941213    3C                   ENDIF
104050941102     C*
104060941102     C                   CLEAR                   VIDGCR
104070941102     C*
104080941102     C                   SETON                                        90
104090941102     C                   GOTO      ENDCT5
104100941102    2C                   ENDIF
104110941102     C* CONTROLLO
104120030612     C                   CLEAR                   DS7pqrs
104130941102     C                   MOVEL     'S'           DS7RIC
104140941102     C                   MOVEL     'C'           DS7GES
104150941213    2C     ARBCCM        IFGT      0
104160941102     C                   MOVEL     ARBCCM        DS7KSC
104170941102     C                   ELSE
104180941102     C                   MOVEL     ARBKSC        DS7KSC
104190941102     C                   ENDIF
104200941102     C                   MOVEL     VIDGCA        DS7COD
104210030612     C                   MOVEL     DS7pqrs       KPJBU
104220941102     C                   CALL      'TRTB72R'
104230941102     C                   PARM                    KPJBA
104240030612     C                   MOVEL     KPJBU         DS7pqrs
104250941102     C* ERRORE
104260941213    3C     DS7ERR        IFEQ      '1'
104270941102     C                   MOVEL     DS7MSG        VIDMSG
104280941102     C                   SETON                                        449028
104290941102     C                   GOTO      ENDCT5
104300941213    3C                   ENDIF
104310941102     C*
104320941213    2C                   ENDIF
104330941213    1C                   ENDIF
104340011010      *-------------------
104350941220     C*
104360941220     c* SE MODIFICATO IK TIPO BOLLA ED ELIMINATA LA 2 MA GIA' INCASS
104370941220     C*  ERRORE
104380941220     C     §3ATB2        IFEQ      *BLANKS
104390941220     C*
104400941220     C     SAVTB2        ANDNE     *BLANKS
104410941220     C     ARBICA        ANDNE     ' '
104420941220     C     ARBICA        ANDNE     'R'
104430991007     C     DFT2BO        ANDGT     0
104440941220     C                   MOVEL     MSG(59)       VIDMSG
104450941220     C                   SETON                                        409028
104460941220     C                   GOTO      ENDCT5
104470941220     C                   END
104480950407     C* SE CAUSALE VARIAZIONE CODICE BOLLA
104490950407    1C     *IN21         IFEQ      *OFF
104500950407     C     COM01A        ANDEQ     *BLANKS
104510950407     C* E NON TROVATI ERRORI BLOCCANTI
104520950407     C     *IN90         ANDEQ     *OFF
104530950407     C* E ASSEGNATO
104540950407     C     FLGTB1        ANDEQ     'A'
104550950407     C* SE AGGIUNTO O TOLTO IL CONTRASSEGNO
104560950407    2C     SAVFCA        IFEQ      *ZEROS
104570950407     C     §3AFCA        ANDNE     *ZEROS
104580950407     C     SAVFCA        ORNE      *ZEROS
104590950407     C     §3AFCA        ANDEQ     *ZEROS
104600991007     C                   MOVEL     '1'           WTRC
104610991007     C     KARB2         CHAIN(N)  FIAR6000                           30
104620950407    3C     *IN30         IFEQ      *OFF
104630950407     C* SE GIA' TASSATO FINO ALL'IMPONIBILE
104640950407     C     AR6IMV        ANDNE     *ZEROS
104650950407     C                   SETON                                        2892
104660950407     C* ERRORE NON BLOCCANTE:MANCA VARIA G
104670950407    4C     §3AFCA        IFNE      *ZEROS
104680950407     C                   MOVEL     MSG(65)       VIDMSG
104690950407     C                   ELSE
104700950407     C* ERRORE NON BLOCCANTE:C'E' VARIA G
104710950407     C                   MOVEL     MSG(66)       VIDMSG
104720950407    4C                   END
104730950407    3C                   END
104740950407     C*
104750950407    2C                   END
104760950407     C                   MOVE      '1'           COM01A            1
104770950407    1C                   END
104780920116     C*
104790920116     C     ENDCT5        ENDSR
104800920116     C*
104810920116     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE KX -----------------*
104820920129     C     REGIS5        BEGSR
104830991001     C                   SETOFF                                       903839
104840130412     c                   clear                   wpda_ndc
104850130412     c                   clear                   wpda_ddc
104860130412     c                   clear                   wpda_dcm
104870071116     C
104880950104     C*  GIRO UDATE
104890950104     C                   TIME                    W0140            14 0
104900950104     C* UDATE IN GGMMAAAA
104910950104     C                   MOVE      W0140         WDTGIO            8 0
104920950104     C*
104930950104     C* UDATE IN AAAAMMGG
104940950104     C                   Z-ADD     WDTGIO        G02DAT
104950950104     C                   MOVEL     *BLANK        G02ERR
104960950104     C                   CALL      'XSRDA8'
104970950104     C                   PARM                    WLBDAT
104980950104     C                   MOVEL     G02INV        DATEU             8 0
104990941221     C* PRENDO I VALORI ASSOLUTI
105000941221     C                   MLLZO     1             VIDCAS
105010941221     C*
105020941213     C                   MOVEL     KNMUS         ARBPRU
105030941220     C                   MOVEL     ARBCPI        SAVCPI
105040070515     c
105050070515     c* Se modificato il C/A, devo allocarmi record "L" per il
105060070515     c*  blocco telefonata C/A
105070070515     c                   clear                   wesar4L           1
105080070515    1C     vidCAS        IFNE      SAVCAS
105090070515     C     vidTIC        ORNE      SAVTIC
105100070515     C     vidVCA        ORNE      SAVVCA
105110070807     C     vidcas        andgt     0
105120070515     c                   eval      Tipoela='C'
105130070515     c                   exsr      GestAR4L
105140070515    2c                   if        *in91
105150070515     C                   GOTO      ENDRG5
105160070515    2c                   endif
105170070515    1c                   endif
105180070515     c
105190070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
105200070515     c* iornare direttamente il file solo se cambiato tipo bolla
105210070515     c                   clear                   waltrabolla       1
105220130412     c                   setoff                                       3091
105230130412     c                   if        oldcbo<>newcbo
105240070515     C   10KARB          CHAIN     FNBLP01L                           3091
105250070515     C  n10KARB          CHAIN     FNARB01L                           3091
105260130412     c                   else
105270130412     c* questa chain mi serve per l'invio dati a PDA
105280131120     c                   if        d66pda<>' '
105290130412     C  n10KARB          CHAIN(n)  FNARB01L                           30
105300130412     c                   endif
105310130412     c                   endif
105320070515     c*
105330070515     c                   if        *in91
105340070515     C                   SETON                                        28
105350070515     C                   MOVEL     MSG(4)        VIDMSG
105360070515     c                   unlock    fiar401l
105370070515     C                   GOTO      ENDRG5
105380070515     c                   else
105390070515     c                   if        *in30
105400070515     c                   eval      waltrabolla='N'
105410070515     c                   else
105420130411     c  n10              eval      wpda_ndc=arbndc
105430130411     c  n10              eval      wpda_ddc=arbddc
105440130411     c  n10              eval      wpda_dcm=arbdcm
105450130412     c                   if        oldcbo<>newcbo
105460070515     C   10KARB          CHAIN     FNARB01L                           30
105470130412     c                   endif
105480131120     c                   if        oldcbo<>newcbo or d66pda<>' '
105490070515     C  N10KARB          CHAIN     FNBLP01L                           30
105500070515     c* Alla fine per non sporcarmi i campi del record, richaino il
105510070515     c*  record originale che sto variando
105520130411     c   10              eval      wpda_ndc=arbndc
105530130411     c   10              eval      wpda_ddc=arbddc
105540130411     c   10              eval      wpda_dcm=arbdcm
105550130412     c                   endif
105560070515     c                   endif
105570070515     c                   endif
105580130412     c*****              endif
105590920123     C*********
105600920116     C* INVIO PER SEDE
105610170116     C*****              MOVEL     'FNARBK0T'    SFILE
105620920116     C* ALLOCO OGGETTI
105630170116     C***                EXSR      ALLOC
105640170116     c***                if        *in91
105650170116     c***                unlock    fiar401l
105660170116     c** 10              unlock    fnblp01l
105670170116     c**n10              unlock    fnarb01l
105680170116     C**                 GOTO      ENDRG5
105690170116     c****               endif
105700930511     C*
105710941212     C* INVIO SOLO IN SEDE L'INDIRIZZO DEL MITTENTE PER C/A
105720941212     C* INVIO SOLO IN SEDE L'INDIRIZZO DEL DESTINATARIO SE DOPPIA BOLLA
105730930511     C                   MOVEL     'S'           §7LFFI
105740930511     C*
105750930210     C********* PER  C O N T R A S S E G N O
105760170116    1C**   AR9CAS        IFEQ      0
105770170116     C**   VIDCAS        ANDNE     0
105780170116     C**                 SETON                                        38
105790930210     C* INVIO PER SEDE
105800170116     C**                 MOVEL     'FNARBM0T'    SFILE
105810930210     C* ALLOCO OGGETTI
105820170116     C**                 EXSR      ALLOC
105830070515     c
105840170116     c***                if        *in91
105850170116     C**                 MOVEA     'FNARBK0T'    C21(13)
105860170116     C**                 MOVEA     VAR           C21(41)
105870170116     C**                 MOVEL     *BLANKS       COMMAN           80
105880170116     C**                 MOVEA     C21           COMMAN
105890170116     C**                 CALL      'QCMDEXC'                            H1
105900170116     C**                 PARM                    COMMAN
105910170116     C**                 PARM                    LUNG
105920170116     c**                 unlock    fiar401l
105930170116     c** 10              unlock    fnblp01l
105940170116     c**n10              unlock    fnarb01l
105950070515     c
105960170116     C**                 GOTO      ENDRG5
105970170116    1C**                 END
105980170116    1C**                 END
105990950324     C*
106000930210     C********* PER  D O P P I A    B O L L A
106010170116    1C**   SAVTB2        IFEQ      '  '
106020170116     C**   §3ATB2        ANDNE     '  '
106030170116     C**                 SETON                                        39
106040930210     C* INVIO PER SEDE
106050170116     C**                 MOVEL     'FNARBD0T'    SFILE
106060930210     C* ALLOCO OGGETTI
106070170116     C**                 EXSR      ALLOC
106080170116     c**                 if        *in91
106090170116     C**                 MOVEA     'FNARBK0T'    C21(13)
106100170116     C**                 MOVEA     VAR           C21(41)
106110170116     C**                 MOVEL     *BLANKS       COMMAN           80
106120170116     C**                 MOVEA     C21           COMMAN
106130170116     C**                 CALL      'QCMDEXC'                            H1
106140170116     C**                 PARM                    COMMAN
106150170116     C**                 PARM                    LUNG
106160070515     c
106170170116     C**                 MOVEA     'FNARBM0T'    C21(13)
106180170116     C**                 MOVEL     *BLANKS       COMMAN           80
106190170116     C**                 MOVEA     C21           COMMAN
106200170116     C**                 CALL      'QCMDEXC'                            H1
106210170116     C***                PARM                    COMMAN
106220170116     C**                 PARM                    LUNG
106230170116     c**                 unlock    fiar401l
106240170116     c** 10              unlock    fnblp01l
106250170116     c**n10              unlock    fnarb01l
106260170116     C**                 GOTO      ENDRG5
106270170116     C**                 ENDIF
106280170116     C**                 ENDIF
106290041022     C*********
106300041022     C                   Z-ADD     DATEU         ARBDTV
106310060320     C                   TIME                    ARBORV
106320920116     C* STORICIZZAZIONE ALL'ARRIVO
106330060130    1C     D66FSA        ifeq      'S'
106340920116     C                   MOVEL     VIDCVR        ARBCVB
106350941213     C                   MOVEL     OLDCBO        ARBCBP
106360941213     C                   MOVEL     NEWCBO        ARBCBN
106370941213     C                   MOVEL     AR9TIC        ARBTIC
106380941213     C                   MOVEL     AR9CAS        ARBCAS
106390941213     C                   MOVEL     AR9VCA        ARBVCA
106400941213     C                   MOVEL     AR9GCA        ARBGCA
106410011108      *
106420011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
106430011108      * è in partenza 'P' o in arrivo 'A'
106440011108     C     *IN10         IFEQ      *ON
106450011108     C                   MOVE      'A'           ARBPRU
106460011108     C                   ENDIF
106470011108     C     *IN10         IFEQ      *OFF
106480011108     C                   MOVE      'P'           ARBPRU
106490011108     C                   ENDIF
106500011108      *
106510941014     C                   WRITE     FNARBK00
106520930210     C                   END
106530060213     c* storicizzazione motivazione
106540060213     c     d66mtv        ifeq      'S'
106550060301     c                   exsr      sto_mtv
106560060213     c                   endif
106570920116     C*
106580920123     C*********
106590920117     C* I N V I O
106600941102     C                   MOVEL     NEWCBO        ARBCBN
106610941213     C* PER IL FILE DI VARIAZIONE
106620941102     C                   MOVEL     VIDCAS        ARBCAS
106630920116     C                   MOVEL     VIDTIC        ARBTIC
106640941212     C                   MOVEL     VIDGCA        ARBGCA
106650941213     C*
106660941215     C*
106670941212     C                   MOVEL     VIDVCA        ARBVCA
106680011017      * Se l'importo del contrassegno è 0 non devo scrivere la divisa
106690011017     C     VIDCAS        IFEQ      *ZEROS
106700011017     C                   CLEAR                   ARBVCA
106710011017     C                   ENDIF
106720920123     C*********
106730041022     C     D66FFI        ifeq      'S'
106740920117     C                   TIME                    ARBORV
106750920127     C                   MOVEL     VIDCVR        ARBCVB
106760170116     C**                 OPEN      FNARBK0T
106770941014     C                   WRITE     FNARBKTT
106780170116     C**                 CLOSE     FNARBK0T
106790920117     C* DISALLOCO
106800170116     C**                 MOVEA     'FNARBK0T'    C21(13)
106810170116     C**                 MOVEA     VAR           C21(41)
106820170116     C**                 MOVEL     *BLANKS       COMMAN           80
106830170116     C**                 MOVEA     C21           COMMAN
106840170116     C**                 CALL      'QCMDEXC'                            H1
106850170116     C**                 PARM                    COMMAN
106860170116     C**                 PARM                    LUNG
106870920212     C*
106880170116     C** 38              DO
106890170116     C**                 OPEN      FNARBM0T
106900170116     C**                 WRITE     FNARBMTT
106910170116     C**                 CLOSE     FNARBM0T
106920920212     C* DISALLOCO
106930170116     C**                 MOVEA     'FNARBM0T'    C21(13)
106940170116     C**                 MOVEA     C21           COMMAN
106950170116     C**                 CALL      'QCMDEXC'                            H1
106960170116     C**                 PARM                    COMMAN
106970170116     C**                 PARM                    LUNG
106980170116     C**                 END
106990920213     C*
107000170116     C** 39              DO
107010170116     C**                 MOVEL     *BLANKS       ARBCPI
107020170116     C**                 ADD       1             ARBORV
107030170116     C**                 MOVEL     VIDCVR        ARBCVB
107040170116     C**                 MOVEL     NEWCBO        ARBCBO
107050170116     C**                 OPEN      FNARBD0T
107060170116     C**                 WRITE     FNARBDTT
107070170116     C**                 CLOSE     FNARBD0T
107080920213     C* DISALLOCO
107090170116     C**                 MOVEA     'FNARBD0T'    C21(13)
107100170116     C**                 MOVEA     C21           COMMAN
107110170116     C**                 CALL      'QCMDEXC'                            H1
107120170116     C**                 PARM                    COMMAN
107130170116     C**                 PARM                    LUNG
107140170116     C**                 END
107150930210     C                   END
107160920117     C*
107170920117     C* A G G I O R N O   F I L E
107180920117     C*
107190051118     c* se variato imposto la data ultima variazione
107200091217    1c                   if        vidvca<>ar9vca or
107210051118     c                             ar9cas<>vidcas or
107220051118     c                             ar9vca<>vidvca or
107230051118     c                             ar9gca<>vidgca
107240051118     c                   movel     dateu         ar9duv
107250091217    1c                   endif
107260091217     c
107270091217     c* Se variato il tipo incasso C/Ass, lo memorizzo
107280140516     c* non lo memorizzo in caso di annullamento altrimenti su internet viene
107290140516     c* visualizzata sia la variazione che l'annullamento
107300140516    1c                   if        savtic<>vidtic and vidcas>0
107310091217     c                   eval      Wtipoagg='TIC'
107320091217     c                   exsr      RegisAR5GEN
107330091217    1c                   EndIf
107340091217     c
107350091217     c
107360941212     C                   MOVEL     NEWCBO        ARBCBO
107370941212     C                   MOVEL     VIDCAS        AR9CAS
107380941212     C                   MOVEL     VIDTIC        AR9TIC
107390941212     C                   MOVEL     VIDGCA        AR9GCA
107400941212     C                   MOVEL     VIDVCA        AR9VCA
107410011017      * Se l'importo del contrassegno è 0 non devo scrivere la divisa
107420011017     C     VIDCAS        IFEQ      *ZEROS
107430011017     C                   CLEAR                   AR9VCA
107440011017     C                   ENDIF
107450011019      *
107460941220     C                   MOVEL     ARBCPI        SAVCPI
107470920121     C*
107480941220     C* E L I M I N A T A   L A   2   B O L L A
107490941220    1C     SAVTB2        IFNE      '  '
107500920603     C     §3ATB2        ANDEQ     '  '
107510941220     C* PULISCO PARTITA IVA
107520930210     C                   MOVEL     *BLANKS       ARBCPI
107530991001     C* SE C'ERA AR6 SECONDA BOLLA LO DELETO
107540990930     C     WAR62         IFEQ      'E'
107550991008     C                   MOVEL     '2'           WTRC
107551170419     C     KARB2         CHAIN(N)  FIAR601l
107552170419     c                   if        %found(fiar601l) and ar6nft>0
107553170419     c                   exsr      sr_fnfta
107554170419     c                   endif
107560991008     C     KARB2         DELETE    FIAR6000                           30
107570941220     C                   ENDIF
107580941220     C*
107590941220     C* SE GIA' CONSEGNATA DO' PER INCASSATO L'ASSEGNATO
107600000925    2C     ARBDCM        IFGT      0
107610000925    3C     ARBICA        IFEQ      'R'
107620000925     C     ARBICA        OREQ      ' '
107630941220     C                   MOVEL     'Y'           ARBICA
107640000925    4C     ARBICC        IFNE      'R'
107650970613     C                   EXSR      CHIUBO
107660000925    5C     ARBNMI        IFNE      8888888
107670971023     C                   MOVEL     'S'           ARBICA
107680000925    5C                   ENDIF
107690000925    4C                   ENDIF
107700000925    3C                   ENDIF
107710000925    2C                   ENDIF
107720941220     C* ABILITO L'ASSEGNATO
107730941220     C                   MOVEL     'S'           ARBACA
107740941220    1C                   ENDIF
107750941220     C*
107760941220     C* A G G I U N T A   2   B O L L A
107770941220     C     SAVTB2        IFEQ      '  '
107780941220     C     §3ATB2        ANDNE     '  '
107790941220     C* PULISCO PARTITA IVA
107800941220     C                   MOVEL     *BLANKS       ARBCPI
107810941220     C*
107820941220     C                   MOVEL     ' '           ARBACA
107830941220     C                   MOVEL     ' '           ARBICA
107840941220     C* SE GIA' CONSEGNATA SI TRATTA DI UN RITORNO ALL'INCASSO
107850941220     C     ARBDCM        IFGT      0
107860941220     C                   MOVEL     'R'           ARBICA
107870941220     C                   ENDIF
107880941220     C                   ENDIF
107890920120     C*
107900920117     C* ABILITAZIONE CONTRASSEGNO
107910950918     C* SE NON HO VARIATO IL C/A RISPETTO A PRIMA, LASCIO ABILITAZIONE
107920950918     C*  E INCASSO C/A COME ERANO
107930950918    1C     AR9CAS        IFNE      SAVCAS
107940950918     C     AR9TIC        ORNE      SAVTIC
107950950918     C     AR9VCA        ORNE      SAVVCA
107960920120     C                   MOVEL     ' '           ARBACC
107970070320     c                   eval      §b4ltca='S'
107980070320     c
107990941220     C*
108000920117     C* SE NON C'E' PIU' IL C/ASSEGNO ABILITO
108010950918    2C     §3AFCA        IFEQ      0
108020950104     C     AR9CAS        OREQ      0
108030920117     C                   MOVEL     'S'           ARBACC
108040070320     c                   eval      §b4ltca=' '
108050960704     C*
108060960909     C* SE IL CONTRASSEGNO E' UN RITORNO ALL'INCASSO, LO TOLGO
108070960704     C* SE CONSEGNATO, E RITORNO ALL'INCASSO LO DO PER INCASSATO
108080960704    3C     ARBDCM        IFGT      0
108090960704     C     ARBICC        ANDEQ     'R'
108100960704     C                   MOVEL     'Y'           ARBICC
108110970613     C* SE L'ASSEGNATO NON E' UN RITORNO ALL'INCASSO VEDO SE CHIUDERE
108120970613     C* LA BOLLA
108130970613     C     ARBICA        IFNE      'R'
108140970613     C                   EXSR      CHIUBO
108150971023     C     ARBNMI        IFNE      8888888
108160971023     C                   MOVEL     'S'           ARBICC
108170971023     C                   ENDIF
108180970613     C                   ENDIF
108190970613     C                   ENDIF
108200960704     C*
108210950918   X2C                   ELSE
108220941220     C*
108230941220     C* SE LA BOLLA E' GIA' CONSEGNA METTO "R" DI RITORNO ALL'INCASSO
108240950918    3C     ARBDCM        IFGT      0
108250941220     C                   MOVEL     'R'           ARBICC
108260950918   X3C                   ELSE
108270941220     C* ALTRIMENTI PULISCO IL FLAG DI INCASSO C/A
108280941220     C                   CLEAR                   ARBICC
108290950918    3C                   ENDIF
108300011031      *
108310070320    3c                   if        o21fca = *blanks
108320020103     c                   eval      arbacc = 'S'
108330070320     c                   clear                   §b4ltca
108340070320    3c                   endif
108350070320    2C                   END
108360070320     c
108370070320     c* Aggiorno record di FIar4
108380070320     c                   eval      TipoEla='A'
108390070320     c                   exsr      GestAR4L
108400070320    1C                   END
108410071116     c*
108420071116     C* M O D I F I C A T O   S O L O   1 °   T I P O   B O L L A
108430071116    1C     SAVTB2        IFEQ      '  '
108440071116     C     §3ATB2        ANDEQ     '  '
108450071116     C* PRENDI IL TIPO BOLLA DEL NUOVO CODICE
108460071116     C                   MOVEL     'TB'          COD
108470071116     C                   MOVEL(P)  §3atb1        KEY
108480071116     C                   EXSR      CTABEL
108490071116     C  N30              MOVEL     TBLUNI        DSTB
108500071116     C   30              CLEAR                   DSTB
108510071116     c                   movel     §tbfcb        NEWtb1FCB         1
108520071116     c
108530071116     C* Se il nuovo tipo bolla è da non contabilizzare, imposto
108540071116     c*  la "S" di abilitazione Assgnato
108550071116    2c                   if        ARBACA=' ' and NEWtb1FCB='0'
108560071116     c                   eval      arbaca='S'
108570071116    2c                   endif
108580071116     c
108590071116     c* Se il nuovo tipo bolla è da contabilizzare, ma non lo era il
108600071116     c*  vecchio, vedo se devo togliere la "S" per l'abilitazione del C/A
108610071116    2c                   if        NEWtb1FCB='1' and Newtb1FCB<>OLDtb1FCB
108620071116     c                             and arbaca='S'
108630071116     C* TASSAZIONE IN FIAR6
108640071116     C                   MOVEL     '1'           WTRC
108650071116     C     KARB2         CHAIN(N)  FIAR6000
108660071116    3c                   if        (%subst(%editc(arbksc: 'X'):4:4)='9999') and
108670071116     c                             (not %found(FIar601l) or ar6dft=0)
108680071116     c                   eval      arbaca=' '
108690071116    3c                   endif
108700071116    2c                   endif
108701170418     c* se il nuovo tipo bolla non è da contabilizzare e il vecchio lo era
108702170418     c* memorizzo eventuale fattura stornata su file FNFTA00F
108703170418     C* TASSAZIONE IN FIAR6
108704170419     c* solo per bolle in assegnato
108705170418    2c                   if        NEWtb1FCB='0' and Newtb1FCB<>OLDtb1FCB
108706170419     c                             and *in06=*off
108707170418     C                   MOVEL     '1'           WTRC
108708170418     C     KARB2         CHAIN(N)  FIAR6000
108709170418     c                   if        %found(fiar601l) and ar6nft>0
108710170418     c                   exsr      sr_fnfta
108711170418     c                   endif
108712170418     c                   endif
108713071116     c
108720071116    1C                   ENDIF
108721170419
108722170419     C* M O D I F I C A T O   IL  2 °   T I P O   B O L L A
108723170419    1C     SAVTB2        IFne      '  '
108724170419     C     §3ATB2        ANDne     '  '
108725170419     c     savtb2        andne     §3atb2
108726170419     C* PRENDo IL TIPO BOLLA DEL NUOVO CODICE
108727170419     C                   MOVEL     'TB'          COD
108728170419     C                   MOVEL(P)  §3atb2        KEY
108729170419     C                   EXSR      CTABEL
108730170419     C  N30              MOVEL     TBLUNI        DSTB
108731170419     C   30              CLEAR                   DSTB
108732170419     c                   movel     §tbfcb        NEWtb2FCB         1
108733170419     c* se il nuovo tipo bolla non è da contabilizzare e il vecchio lo era
108734170419     c* memorizzo eventuale fattura stornata su file FNFTA00F
108735170419     C* TASSAZIONE IN FIAR6
108736170419     c* solo per bolle in assegnato
108737170419    2c                   if        NEWtb2FCB='0' and Newtb2FCB<>OLDtb2FCB
108739170419     C                   MOVEL     '2'           WTRC
108740170419     C     KARB2         CHAIN(N)  FIAR6000
108741170419     c                   if        %found(fiar601l) and ar6nft>0
108742170419     c                   exsr      sr_fnfta
108743170419     c                   endif
108744170419     c                   endif
108745170419     c                   endif
108746920116     C*
108747941216     C     §3AFCA        IFNE      0
108750950104     C     AR9CAS        ANDGT     0
108760941212     C     WAR9          IFEQ      ' '
108770941212     C                   MOVEL     ARBAAS        AR9AAS
108780941212     C                   MOVEL     ARBLNP        AR9LNP
108790941212     C                   MOVEL     ARBNRS        AR9NRS
108800941212     C                   MOVEL     ARBNSP        AR9NSP
108810051118     c                   movel     dateu         ar9duv
108820051118     C                   WRITE     FiAR9000
108830941212     C                   ELSE
108840051118     C                   UPDATE    FiAR9000
108850941212     C                   ENDIF
108860941216     C*
108870941216     C                   ELSE
108880941216     C* SE MI HANNO CANCELLATO L'AR9 --> DELETO
108890941216     C     WAR9          IFEQ      'E'
108900051118     C     KARB          DELETE    FiAR9000                           30
108910941216     C                   ENDIF
108920941216     C                   ENDIF
108930941212     C*
108940060213     C   10              UPDATE    FNARB000
108950041022     c* Se cambiato codice bolla aggiorno fnblp
108960041022     c                   if        waltrabollA=' ' AND oldcbo<>newcbo
108970041022     c                   eval      arbcbo=newcbo
108980041022     c   10              except    arbkblp
108990041022     c  N10              except    arbkarb
109000041022     c                   endif
109010040224
109020040224      * Se c'è la data consegna richiesta controllo se è ancora valida
109030040224      * solo se ho fatto una modifica in arrivo
109040160114     c                   If        ArbDcr > *Zeros and *In10 and wfile='A'
109050040324     c                             and wCa = *Blanks and wgiac = *Blanks
109060040224     c                   ExSr      ContrDcr
109070040224     c                   EndIf
109080950602     C*
109090130411     c* Richiamo routine per invio variazione a pda
109100130411     c                   exsr      sr_pda
109110950104     C*
109120920117     C     ENDRG5        ENDSR
109130911211     C*
109140911211     C*--- CARICO CODICI TARIFFA -------------------------------------*
109150911211     C     CARCTR        BEGSR
109160020305     c**
109170020305     C* Richiamo trul27r per reperimento WFIE e per reperimento tariffa di
109180020305     C* cartello
109190020305     c**
109200160225     c                   clear                   trul27ds1
109210021121     c                   Eval      I27Pkg = ArbPkf
109220020305     c                   eval      i27tsp = arbtsp
109230020305     c                   eval      i27lna = arblna
109240020305     c                   eval      i27lnp = arblnp
109250020305     C                   eval      i27cli = ksc
109260160225     c                   call      'TRUL27R1'
109270160225     c                   parm                    trul27ds1
109280020305     c                   eval      wfie = o27fie
109290020305     c*
109300941209     C                   MOVEL     *BLANKS       DE2CTR
109310050202     C                   CLEAR                   Fnlv59DS
109320941209     C*
109330050202     C                   MOVEL     D48TBO        ilv59tbo
109340040511      * SE BOLLA MONOVARIA NON PASSO IL TIPO BOLLA
109350040511     C                   IF        §3ASVA <> *BLANK
109360050202     C                   CLEAR                   ilv59tbo
109370050202     c                   eval      ilv59prp='P'
109380040511     C                   ENDIF
109390050202     C                   Z-ADD     ARBDSP        ilv59DSP
109400060131     C                   MOVEL     dutkci        ilv59KCI
109410050202     C                   Z-ADD     KSC           ilv59KSC
109420050202     C                   MOVEL     WFIE          ilv59FIE
109430160225     C                   MOVEL     WFIE          ilv59ta3
109440050202     C                   MOVEL     ARBTSP        ilv59TSP
109450050202     C                   MOVEL     VIDCTR        ilv59CTR
109460941209     C*
109470941209     C* SE NON C'E' RICERCA SU COD.TARIFFA E CAMBIATO CODICE CLIENTE
109480941209     C*  --> RICERCO LA TARIFFA
109490941209     C                   SETOFF                                       32
109500941209     C     '?'           SCAN      VIDCTR                                 32
109510941215     C     *IN32         IFEQ      *OFF
109520941215     C     KSC           ANDNE     SAVKSC
109530050202     C     VIDCTR        ANDEQ     SAVCTR
109540160225     C***                CLEAR                   ilv59CTR
109550941223     C                   CLEAR                   OTT
109560160225     C***                MOVEL     'S'           ilv59RIC
109570941215     C                   ELSE
109580060516     c* Se cod cliente e cod tariffa invariati rispetto alla bolla,
109590060516     c*  accetto codici tariffa bloccati (ma quando immessi validi)
109600060516     c                   if        not *in32 and ksc=kscbolla
109610060511     c                             and ctrbolla=vidctr
109620060516     C                   MOVEL     'B'           ilv59ta2
109630941209     C                   ENDIF
109640060511     C                   ENDIF
109650060516     C                   MOVEL     'S'           ilv59CON
109660050202     C
109670050202     C                   MOVEL     KSC           SAVKSC
109680160303     c* se ho abblancato il codice tariffa, lo faccio reimpostare dal pgm
109690160303     c                   if        ilv59ctr=*blanks
109700160303     C                   MOVEL     'S'           ilv59RIC
109710160303     c                   endif
109720941209     C*
109730050202     C                   CALL      'FNLV59R'
109740050202     C                   PARM                    Fnlv59DS
109750050202     c
109760941209     C*
109770941209     C* NON ESISTONO TARIFFE
109780050202     C                   MOVEL     olv59TKS      WTKS              1
109790050202     C     olv59TKS      IFNE      'S'
109800941209     C                   SETON                                        18
109810911219     C                   SETOFF                                       32
109820950114     C                   MOVEL     CNOTAR        DE2CTR
109830941209     C                   ELSE
109840941209     C*
109850941215     C* SE TARIFFE TROVATE,  DEVO REIMPOSTARE LA TARIFFA A VIDEO
109860050202     C     ilv59RIC      IFEQ      'S'
109870050202     C                   MOVEL     olv59CTR      VIDCTR
109880941209     C                   ENDIF
109890941215     C* DECODIFICA
109900050202     C                   MOVEL     olv59DFS      DE2CTR
109910941209     C                   ENDIF
109920050202     c
109930050202     C                   EVAL      SAVCTR=VIDCTR
109940911211     C*
109950911211     C                   ENDSR
109960911211     C*
109970920320     C*--- CONTROLLI CAMPI CONSEGNA RICHIESTA ------------------------*
109980920320     C     CONTR7        BEGSR
109990920320     C                   SETOFF                                       90
110000941102     c**
110010920320     C* CONSEGNA RICHIESTA
110020941102     C**
110030920320     C                   Z-ADD     0             COMDCR
110040920320     C     VIDDCR        IFNE      0
110050941102     C                   Z-ADD     VIDDCR        G02DAT
110060920320     C                   MOVEL     *BLANK        G02ERR
110070941102     C                   CALL      'XSRDA8'
110080920320     C                   PARM                    WLBDAT
110090941102     C* DATA FORMALMENTE ERRATA
110100920320     C     G02ERR        IFEQ      '1'
110110941102     C                   MOVEL     MSG(6)        VIDMSG
110120941102     C                   SETON                                        409028
110130920320     C                   GOTO      ENDCT7
110140920320     C                   END
110150941102     C*
110160941102     C                   Z-ADD     G02DAT        VIDDCR
110170941102     C*
110180920320     C* DATA CONSEGNA NO < DATA SPEDIZIONE
110190930507     C     G02INV        IFLT      ARBDSP
110200941102     C                   MOVEL     MSG(7)        VIDMSG
110210941102     C                   SETON                                        409028
110220920320     C                   GOTO      ENDCT7
110230930507     C                   END
110240941102     C                   Z-ADD     G02INV        COMDCR            8 0
110250941102     C                   ENDIF
110260070308     c
110270070308      * La data non può superare i gg limiti indicati in tab. VPO
110280070308     c                   If        Not *In90
110290070308     c                   Eval      wdataiso = wdataoggi
110300070308     c                   Adddur    §vpodcr:*d    wdataiso
110310070308     c     *iso          move      wdataiso      ds_data
110320070308     c                   Move      §vpodcr       w003a
110330070308     c                   Movea     w003a         ski(13)
110340070308     c                   Z-add     13            i
110350070308     c                   ExSr      abbl
110360070308     c                   Movea     ski(13)       w003a
110370070308     c                   If        ComDcr > ds_data
110380070308     c                   Eval      *In28 = *On
110390070308     c                   Eval      *In40 = *On
110400070308     c                   Eval      *In90 = *On
110410070308     c                   Eval      VidMsg = msg(129)
110420070308     c                   Eval      VidMsg = %replace(w003a:VidMsg:
110430070308     c                                      %scan('___':VidMsg))
110440070308     C                   GOTO      ENDCT7
110450070308     c                   EndIf
110460070308     c                   EndIf
110470141029     c* Se dcr>0 errore forzabile se presente Fermo Deposito
110480141029     c                   if        d48ffr<>'E'
110490141029     c                   if        comdcr>0 and comdcr<>arbdcr and
110500141029     c                             arbffd<>*blanks and wforzdcr=0
110510141029     C                   MOVEL     MSG(154)      VIDMSG
110520141029     C                   SETON                                        409028
110530141029     C                   EVAL      Wforzdcr=1
110540141029     C                   GOTO      ENDCT7
110550141029     c                   endif
110560141029     c                   endif
110570941102     C**
110580941102     C** ORA CONSEGNA RICHIESTA
110590941102     C**
110600920320     C                   MOVEL     VIDHCR        HM                2 0
110610920320     C     HM            IFLT      0
110620920320     C     HM            ORGT      24
110630941102     C                   MOVEL     MSG(8)        VIDMSG
110640941102     C                   SETON                                        419028
110650920320     C                   GOTO      ENDCT7
110660920320     C                   END
110670941102     C**
110680941102     C** MINUTI CONSEGNA RICHISTA
110690941102     C**
110700920320     C                   MOVE      VIDHCR        HM                2 0
110710920320     C     HM            IFLT      0
110720920320     C     HM            ORGT      59
110730941102     C                   MOVEL     MSG(8)        VIDMSG
110740941102     C                   SETON                                        419028
110750920320     C                   GOTO      ENDCT7
110760920320     C                   END
110770930507     C*
110780941102     C* SE IMMESSO IL TIPO IMMETTERE ANCHE L'ORA O LA DATA
110790930507     C     VIDTCR        IFNE      ' '
110800941102     C     VIDDCR        IFEQ      0
110810941102     C     VIDHCR        ANDEQ     0
110820941102     C                   MOVEL     MSG(9)        VIDMSG
110830941102     C                   SETON                                        409028
110840930507     C                   GOTO      ENDCT7
110850930507     C                   END
110860930507     C                   END
110870070308     c
110880070308     c* Se lna  attivata con GEO, non posso inserire solo tipo e ora:
110890070308     c*  data sempre obbligatoria (se inserito in arrivo, accetto
110900070308     c*  dalla partenza)
110910100419     c* Escludo per le causali di ripristino
110920070319     c                   if        FGSgeot='S' and viddcr=0 and vidhcr>0
110930100419     c                             and d48cvb <> 'CA'
110940070308     c                   if        vidtcr<>arbtcr or vidhcr<>arbhcr or
110950070308     c                             comdcr<>arbdcr
110960070308     C                   MOVEL     MSG(140)      VIDMSG
110970070308     C                   SETON                                        409028
110980070308     C                   GOTO      ENDCT7
110990070308     C                   ENDif
111000070308     C                   ENDif
111010930507     C**
111020941102     C* GIORNI DI CHIUSURA
111030941102     C**
111040930507     C     VIDGC1        IFNE      *BLANKS
111050930507     C                   MOVEL     VIDGC1        ACOM              1
111060930507     C                   MOVE      VIDGC1        BCOM              1
111070941212     C                   Z-ADD     1             B                 2 0
111080930507     C     ACOM          LOOKUP    GGG(B)                                 32
111090941102     C     *IN32         IFEQ      *OFF
111100941102     C                   MOVEL     MSG(10)       VIDMSG
111110941102     C                   SETON                                        429028
111120941102     C                   GOTO      ENDCT7
111130941102     C                   ENDIF
111140930507     C*
111150930507     C     BCOM          IFNE      ' '
111160930507     C     BCOM          ANDNE     'P'
111170930507     C     BCOM          ANDNE     'M'
111180941102     C                   MOVEL     MSG(10)       VIDMSG
111190941102     C                   SETON                                        429028
111200930507     C                   GOTO      ENDCT7
111210930507     C                   END
111220041130      * se modifica fatta in arrivo non posso mettere ' M' o ' P'
111230041130if  2c                   If         *In10 and VidGc1 <> ArbGc1 and
111240041130     c                             (VidGc1 = ' M' or VidGc1 = ' P')
111250041130     c                   Eval      *In28 = *On
111260041130     c                   Eval      *In42 = *On
111270041130     c                   Eval      *In90 = *On
111280041130     c                   Eval      VidMsg = msg(10)
111290041130e   2c                   EndIf
111300930507     C                   END
111310930507     C*
111320930507     C*** 2° GIORNO CHIUSURA ***
111330930507     C     VIDGC2        IFNE      *BLANKS
111340930507     C                   MOVEL     VIDGC2        ACOM              1
111350930507     C                   MOVE      VIDGC2        BCOM              1
111360930507     C                   Z-ADD     1             B
111370930507     C     ACOM          LOOKUP    GGG(B)                                 30
111380941102     C     *IN30         IFEQ      *OFF
111390941102     C                   MOVEL     MSG(10)       VIDMSG
111400941102     C                   SETON                                        439028
111410941102     C                   GOTO      ENDCT7
111420941102     C                   ENDIF
111430930507     C*
111440930507     C     BCOM          IFNE      ' '
111450930507     C     BCOM          ANDNE     'P'
111460930507     C     BCOM          ANDNE     'M'
111470941102     C                   MOVEL     MSG(11)       VIDMSG
111480941102     C                   SETON                                        439028
111490930507     C                   GOTO      ENDCT7
111500930507     C                   END
111510930507     C*
111520930507     C* I GIORNI DI CHIUSURA NON POSSO ESSERE UGUALI
111530930507     C     VIDGC1        IFEQ      VIDGC2
111540941102     C                   MOVEL     MSG(12)       VIDMSG
111550941102     C                   SETON                                        439028
111560930507     C                   GOTO      ENDCT7
111570930507     C                   END
111580930507     C* NON POSSONO ESSERE ' P' ' M'
111590930507     C     VIDGC1        IFEQ      ' M'
111600930507     C     VIDGC2        ANDEQ     ' P'
111610930507     C     VIDGC1        OREQ      ' P'
111620930507     C     VIDGC2        ANDEQ     ' M'
111630941102     C                   MOVEL     MSG(13)       VIDMSG
111640941102     C                   SETON                                        429028
111650930507     C                   GOTO      ENDCT7
111660930507     C                   END
111670041130      * se modifica fatta in arrivo non posso mettere ' M' o ' P'
111680041130if  2c                   If         *In10 and VidGc2 <> ArbGc2 and
111690041130     c                             (VidGc2 = ' M' or VidGc2 = ' P')
111700041130     c                   Eval      *In28 = *On
111710041130     c                   Eval      *In43 = *On
111720041130     c                   Eval      *In90 = *On
111730041130     c                   Eval      VidMsg = msg(10)
111740041130e   2c                   EndIf
111750930507     C                   END
111760941102     C*
111770941102     C* CONSEGNE PARTICOLARi
111780020603     c* Verifico se destinatario disagiato
111790020604     c* per le bolle poste non lo faccio
111800020604     c                   clear                   wdisag
111810020604     c     lnpntw        ifne      'PPT'
111820020603     c                   clear                   tisit0ds
111830020603     c                   movel     'E'           it0tla
111840020603     c                   movel     arbnzd        it0naz
111850020603     c                   movel     arbprd        it0prv
111860020603     c                   movel     arbcad        it0cap
111870020603     c                   movel     arblod        it0loc
111880020603     c                   movel     arbind        it0ind
111890020603     c                   movel     arbrsd        it0rag
111900020603     c                   call      'TISIT5R'
111910020603     c                   parm                    tisit0ds
111920020603     c     ot0err        ifeq      *blanks
111930060203   2ac                   if        ot0dos = 'A' or ot0dos = 'D' or
111940060203     c                             ot0dos = 'S'
111950020603     c                   movel     ot0dos        wdisag
111960020603     c     wdisag        ifeq      'D'
111970020603     c                   movel     'X'           wdisag
111980020603     c                   endif
111990060203     c                   endif
112000020603     c                   else
112010020603     c                   clear                   wdisag
112020020603     c                   endif
112030020604     c                   endif
112040020603     c
112050941102     C*
112060941102     C* 11ON - NON POSO CAMBIARE LA 1 CONSEGNA PARTICOLARE
112070070910     c* Se ho modificato il destinatario è non è stata immessa in
112080070910     c*  partenza, devo per forza toglierla
112090070910     c                   if        *in11=*on and wdisag<>vidtc1 and
112100070910     c                             arbtc1=vidtc1 and par5mdp='S' and
112110070910     c                             par5gtc1<>vidtc1 and par5gtc2<>vidtc1
112120070910     C                   MOVEL     MSG(144)      VIDMSG
112130070910     c                   eval      %subst(vidmsg:27:1) = vidtc1
112140070910     c                   eval      %subst(vidmsg:29:8) = v7dtc1
112150070910     C                   SETON                                        449028
112160070910     C                   GOTO      ENDCT7
112170070910     c                   endif
112180070910     c
112190051116     c****               if        *in11 = *on and wdisag <> *blanks
112200070910     c* se consegna particolare non modificabile ma è stato immesso
112210070910     c* unacons.part.disagiato/supemercat... do' lo stesso la
112220070910     c* possibilita di cambiarla se la combinazione fra le due non è
112230070910     c* ammessa
112240051116     c                   clear                   wcomb
112250070910    0c                   if        *in11=*on and par5mdp='S' and
112260070910     c                             (arbtc1=par5gtc1 or arbtc1=par5gtc2)
112270070910     c
112280070910    1c                   if        vidtc2<>*blanks or wdisag<>*blanks
112290070910     c                             or vidtc1<>arbtc1
112300070910     c*
112310070910     c***                if        *in11 = *on and (vidtc2 <> *blanks or
112320070910     c***                          wdisag <> *blanks or vidtc1 <> arbtc1)
112330051114     c                   move      arbtc1        wtc1
112340051117     c
112350051116     c                   if        wdisag <> *blanks
112360051116     c                   move      wdisag        wtc2
112370051117     c                   exsr      sr_chkcomb
112380051117     c                   endif
112390051117     c                   if        wcomb = *blanks and vidtc1 <> arbtc1
112400051117     c                   move      vidtc1        wtc2
112410051117     c                   exsr      sr_chkcomb
112420051117     c                   endif
112430051117     c                   if        wcomb = *blanks and vidtc2 <> *blanks
112440051117     c                   move      vidtc2        wtc2
112450051117     c                   exsr      sr_chkcomb
112460051117     c                   endif
112470070910    1c                   endif
112480070910     c***
112490070910    0C***  *IN11         IFEQ      *ON
112500070910     c***  wdisag        andeq     *blanks
112510070910    0c***                if        *in11 = *on and wcomb = *blanks
112520070910     c
112530070910    1c                   if        wcomb=*blanks
112540941102     C*
112550070910    2C     VIDTC1        IFNE      ARBTC1
112560941102     C                   MOVEL     MSG(15)       VIDMSG
112570051117     c                   eval      %subst(vidmsg:36:1) = arbtc1
112580941102     C                   SETON                                        449028
112590941102     C                   GOTO      ENDCT7
112600070910    2c                   endif
112610070910    1c                   endif
112620070910    0c                   endif
112630070910     c
112640070910     c***                else
112650070910     C***                MOVEL     '1P'          COD
112660070910     C***                MOVEL(P)  VIDTC1        KEY
112670070910     C***                EXSR      CTABEL
112680070910     C***                MOVEL     TBLUNI        V7DTC1
112690070910    1C***                ENDIF
112700941102     C*
112710070910   X0C***                ELSE
112720941102     C*
112730941102     C                   CLEAR                   V7DTC1
112740941102     C*
112750941102    1C     VIDTC1        IFNE      ' '
112760941102     C* RICERCA
112770941102    2C     VIDTC1        IFEQ      '?'
112780941102     C                   MOVEL     CODUT         §KUT
112790941102     C                   MOVEL     '1P'          §COD
112800941102     C                   MOVE      *BLANKS       VIDTC1
112810991011     C                   CLEAR                   §KEY
112820941102     C                   CALL      'X§TABER'
112830941102     C                   PARM                    §KUT
112840941102     C                   PARM                    §COD
112850941102     C                   PARM                    §KEY
112860941209     C                   PARM                    §DES
112870941102     C                   MOVEL     §KEY          VIDTC1
112880941102     C                   MOVEL     §DES          V7DTC1
112890941102     C                   SETON                                        9044
112900941102     C                   GOTO      ENDCT7
112910941102    2C                   ENDIF
112920941102     C*
112930941102     C                   MOVEL     '1P'          COD
112940941102     C                   MOVEL(P)  VIDTC1        KEY
112950941212     C                   EXSR      CTABEL
112960941102     C*
112970941102    2C     *IN30         IFEQ      *ON
112980941102     C                   MOVEL     MSG(11)       VIDMSG
112990941102     C                   SETON                                        449028
113000941102     C                   GOTO      ENDCT7
113010941102    2C                   ENDIF
113020941102     C*
113030941102     C                   MOVEL     TBLUNI        DS1P
113040941102     C*
113050941102     C* NON UTILIZZABILE IN ARRIVO
113060950104    2C   10§1PUAR        IFNE      'S'
113070020318     C     LNPNTW        ANDNE     'PPT'
113080020318     C     LNPNTW        OREQ      'PPT'
113090000510     C     §1PFPC        ANDNE     'E'
113100000510     C     §1PFPC        ANDNE     'A'
113110040705      * e non è quella presa in automatico
113120051114     c                   If        Vidtc1 <> wdisag and vidtc1 <> arbtc1
113130070914     c* Verifico se era stata messa in partenza ma tolta
113140070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
113150070914     c                             <>vidtc1 and par5gtc2<>vidtc1)
113160941102     C                   MOVEL     MSG(11)       VIDMSG
113170941102     C                   SETON                                        449028
113180941102     C                   GOTO      ENDCT7
113190040705     c                   EndIf
113200070914     c                   EndIf
113210941102    2C                   END
113220060131     c
113230060131     c* SE devo considerare le abilitati, verifico se il profilo
113240060131     c*  può inserire la cons particolare, se non è presa in automatico
113250060131     c   10              If        Vidtc1 <> wdisag and vidtc1 <> arbtc1
113260070914     c                             and  §utemfcp=' '
113270070914     c******                       and dateu>=§vpoacp and  §utemfcp=' '
113280070914     c*
113290070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
113300070914     c                             <>vidtc1 and par5gtc2<>vidtc1)
113310060131     C*
113320060131     C                   MOVEL     'SP'          COD
113330060131     C                   MOVEL(P)  VIDTC1        KEY
113340060131     C                   EXSR      CTABEL
113350060131     C*
113360060131    2C     *IN30         IFEQ      *ON
113370060131     c                   clear                   dssp
113380060131     c                   else
113390060131     c                   movel     tbluni        DSSP
113400060131     c                   endif
113410060131     c
113420060131     c                   if        §spaba='S'
113430060131     C                   MOVEL     MSG(138)      VIDMSG
113440060131     c                   eval      %subst(vidmsg:59:1) = vidtc1
113450060131     C                   SETON                                        449028
113460060131     C                   GOTO      ENDCT7
113470060131     c                   endif
113480060131     c                   endif
113490070914     c
113500070914     c                   endif
113510060131     c
113520950104     C* NON UTILIZZABILE IN PARTENZA
113530950104    2C  N10§1PUPR        IFNE      'S'
113540020318     C     LNPNTW        ANDNE     'PPT'
113550020318     C     LNPNTW        OREQ      'PPT'
113560000510     C     §1PFPC        ANDNE     'E'
113570000510     C     §1PFPC        ANDNE     'P'
113580950104     C                   MOVEL     MSG(11)       VIDMSG
113590950104     C                   SETON                                        449028
113600950104     C                   GOTO      ENDCT7
113610950104    2C                   END
113620000510     C* PER LE POSTE CONTROLLO IL SUO FLAG
113630941102     C*
113640941102     C                   MOVEL     TBLUNI        V7DTC1
113650150414     c* controlli per EXPO
113660151119     c*                  if        vidtc1='Z'
113670151119     c*                  exsr      sr_chkexpo
113680151119     c*                  if        *in90
113690151119     c*                  seton                                        44
113700151119     c*                  goto      endct7
113710151119     c*                  endif
113720151119     c*                  endif
113730941102     C*
113740941102     C* LA 1 E LA 2 NON POSSONO ESSERE UGUALI
113750941102    2C     VIDTC2        IFEQ      VIDTC1
113760941102     C                   MOVEL     MSG(14)       VIDMSG
113770941102     C                   SETON                                        459028
113780941102     C                   GOTO      ENDCT7
113790941102    2C                   END
113800020731     C*
113810941102     C*
113820941102    1C                   END
113830070910    0C******             END
113840941102     C*
113850941102     C* 12ON - NON POSO CAMBIARE LA 2 CONSEGNA PARTICOLARE
113860070910     c* Se ho modificato il destinatario è non è stata immessa in
113870070910     c*  partenza, devo per forza toglierla
113880070910    0c                   if        *in12=*on and wdisag<>vidtc2 and
113890070910     c                             arbtc2=vidtc2 and par5mdp='S' and
113900070910     c                             par5gtc1<>vidtc2 and par5gtc2<>vidtc2
113910070910     C                   MOVEL     MSG(144)      VIDMSG
113920070910     c                   eval      %subst(vidmsg:27:1) = vidtc2
113930070910     c                   eval      %subst(vidmsg:29:8) = v7dtc2
113940070910     C                   SETON                                        459028
113950070910     C                   GOTO      ENDCT7
113960070910    0c                   endif
113970070910     c
113980051114     c* se consegna particolare non modificabile ma è stato immesso
113990051114     c* un destinatrio disagiato/supemercat... do' lo stesso la
114000051114     c* possibilita di cambiarla se la combinazione fra le due non è
114010051114     c* ammessa
114020051116     c                   clear                   wcomb
114030070910    0c                   if        *in12=*on and par5mdp='S' and
114040070910     c                             (arbtc2=par5gtc1 or arbtc2=par5gtc2)
114050070910
114060070910    1c                   if        vidtc1 <> *blanks or
114070070910     c                             wdisag <> *blanks or vidtc2 <> arbtc2
114080070910     c
114090051116     c***                if        *in12 = *on and wdisag <> *blanks
114100070910     c***
114110070910     c***                if        *in12 = *on and (vidtc1 <> *blanks or
114120070910     c***                          wdisag <> *blanks or vidtc2 <> arbtc2)
114130051114     c                   move      arbtc2        wtc1
114140051116     c                   if        wdisag <> *blanks
114150051116     c                   move      wdisag        wtc2
114160051117     c                   exsr      sr_chkcomb
114170051117     c                   endif
114180051117     c                   if        wcomb = *blanks and vidtc2 <> arbtc2
114190051117     c                   move      vidtc2        wtc2
114200051117     c                   exsr      sr_chkcomb
114210051117     c                   endif
114220051117     c                   if        wcomb = *blanks and vidtc1 <> *blanks
114230051117     c                   move      vidtc1        wtc2
114240051117     c                   exsr      sr_chkcomb
114250051117     c                   endif
114260070910    1c                   endif
114270051117     c*
114280070910    0C***  *IN12         IFEQ      *ON
114290070910    0c***                if        *in12 = *on and wcomb = *blanks
114300070910    1c                   if        wcomb = *blanks
114310941102     C*
114320070910    2C     VIDTC2        IFNE      ARBTC2
114330941102     C                   MOVEL     MSG(15)       VIDMSG
114340051117     c                   eval      %subst(vidmsg:36:1) = arbtc2
114350941102     C                   SETON                                        459028
114360941102     C                   GOTO      ENDCT7
114370070910    2c                   endif
114380070910    1c                   endif
114390070910    0c                   endif
114400070910     c
114410070910     c****               else
114420070910     C****               MOVEL     '1P'          COD
114430070910     C****               MOVEL(P)  VIDTC2        KEY
114440070910     C****               EXSR      CTABEL
114450070910     C****               MOVEL     TBLUNI        V7DTC2
114460070910    1C****               ENDIF
114470941102     C*
114480070910   X0C****               ELSE
114490941102     C*
114500941102     C                   CLEAR                   V7DTC2
114510941102     C*
114520941102    1C     VIDTC2        IFNE      ' '
114530941102     C* RICERCA
114540941102    2C     VIDTC2        IFEQ      '?'
114550941102     C                   MOVEL     CODUT         §KUT
114560941102     C                   MOVEL     '1P'          §COD
114570941102     C                   MOVE      *BLANKS       VIDTC2
114580991011     C                   CLEAR                   §KEY
114590941102     C                   CALL      'X§TABER'
114600941102     C                   PARM                    §KUT
114610941102     C                   PARM                    §COD
114620941102     C                   PARM                    §KEY
114630941209     C                   PARM                    §DES
114640941102     C                   MOVEL     §KEY          VIDTC2
114650941102     C                   MOVEL     §DES          V7DTC2
114660941102     C                   SETON                                        9045
114670941102     C                   GOTO      ENDCT7
114680941102    2C                   ENDIF
114690941102     C*
114700941102     C                   MOVEL     '1P'          COD
114710941102     C                   MOVEL(P)  VIDTC2        KEY
114720941212     C                   EXSR      CTABEL
114730941102     C*
114740941102    2C     *IN30         IFEQ      *ON
114750941102     C                   MOVEL     MSG(11)       VIDMSG
114760941102     C                   SETON                                        459028
114770941102     C                   GOTO      ENDCT7
114780941102    2C                   ENDIF
114790941102     C*
114800941102     C                   MOVEL     TBLUNI        DS1P
114810941102     C*
114820941102     C* NON UTILIZZABILE IN ARRIVO
114830950104    2C   10§1PUAR        IFNE      'S'
114840020318     C     LNPNTW        ANDNE     'PPT'
114850020318     C     LNPNTW        OREQ      'PPT'
114860000518     C     §1PFPC        ANDNE     'E'
114870000518     C     §1PFPC        ANDNE     'A'
114880040705      * e non è quella presa in automatico
114890051114     c                   If        Vidtc2 <> wdisag  and vidtc2 <> arbtc2
114900070914     c* Verifico se era stata messa in partenza ma tolta
114910070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
114920070914     c                             <>vidtc2 and par5gtc2<>vidtc2)
114930941102     C                   MOVEL     MSG(11)       VIDMSG
114940941102     C                   SETON                                        459028
114950941102     C                   GOTO      ENDCT7
114960040705     c                   EndIf
114970070914     c                   EndIf
114980941102    2C                   END
114990060131     c
115000060131     c* SE devo considerare le abilitati, verifico se il profilo
115010060131     c*  può inserire la cons particolare, se non è presa in automatico
115020060131     c   10              If        Vidtc2 <> wdisag and vidtc2 <> arbtc2
115030070914     c                             and  §utemfcp=' '
115040070514     c****                         and dateu>=§vpoacp and  §utemfcp=' '
115050070914     c* Verifico se era stata messa in partenza ma tolta
115060070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
115070070914     c                             <>vidtc2 and par5gtc2<>vidtc2)
115080060131     C*
115090060131     C                   MOVEL     'SP'          COD
115100060131     C                   MOVEL(P)  VIDTC2        KEY
115110060131     C                   EXSR      CTABEL
115120060131     C*
115130060131    2C     *IN30         IFEQ      *ON
115140060131     c                   clear                   dssp
115150060131     c                   else
115160060131     c                   movel     tbluni        DSSP
115170060131     c                   endif
115180060131     c
115190060131     c                   if        §spaba='S'
115200060131     C                   MOVEL     MSG(138)      VIDMSG
115210060131     c                   eval      %subst(vidmsg:59:1) = vidtc1
115220060131     C                   SETON                                        459028
115230060131     C                   GOTO      ENDCT7
115240060131     c                   endif
115250060131     c                   endif
115260070914     c                   endif
115270070914     c
115280950104     C* NON UTILIZZABILE IN PARTENZA
115290950104    2C  N10§1PUPR        IFNE      'S'
115300020318     C     LNPNTW        ANDNE     'PPT'
115310020318     C     LNPNTW        OREQ      'PPT'
115320000518     C     §1PFPC        ANDNE     'E'
115330000518     C     §1PFPC        ANDNE     'P'
115340950104     C                   MOVEL     MSG(11)       VIDMSG
115350950104     C                   SETON                                        459028
115360950104     C                   GOTO      ENDCT7
115370950104    2C                   END
115380941102     C*
115390941102     C                   MOVEL     TBLUNI        V7DTC2
115400150414     c* controlli per EXPO
115410151119     c*                  if        vidtc2='Z'
115420151119     c*                  exsr      sr_chkexpo
115430151119     c*                  if        *in90
115440151119     c*                  seton                                        45
115450151119     c*                  goto      endct7
115460151119     c*                  endif
115470151119     c*                  endif
115480941102     C*
115490941102    1C                   END
115500070910    0C****               END
115510060131     c
115520020531     c* Se destinatario disagiato o supermercato errore se non indicato
115530020531     c* in nessuno dei due campi di consegna particolare
115540041015     c*
115550041015     c* Se particolarità varia="U" --> consegna ad uffici devo invece
115560041015     c* dare errore se immesso supermercato o appuntamento
115570041015     c                   if        %subst(arbgva:1:1)='U'
115580041015     c                             and (vidtc1='S' or vidtc1='A' or vidtc2='S'
115590041015     c                             or vidtc2='A')
115600041015     C                   MOVEl     msg(131)      vidmsg
115610041015     C                   SETON                                        449028
115620041015     C                   GOTO      ENDCT7
115630041015     c                   endif
115640150210
115650150210     c* se sono in partenza e immessa la "A" e la data --> errore solo la data immettere
115660150210     c                   if        not *in10 and (vidtc1='A' or vidtc2='A') and
115670150210     c                             comdcr>0
115680150210     c                   if        (par5mdp='S' and par5gtc1<>'A' and par5gtc2
115690150210     c                                                    <> 'A')
115700150210     c                             or (par5mdp=' ' and arbtc1<>'A' and arbtc2
115710150210     c                                                    <> 'A')
115720150210     C                   MOVEl     msg(158)      vidmsg
115730150210     C                   SETON                                        449028
115740150210     C                   GOTO      ENDCT7
115750150210     c                   endif
115760150210     c                   endif
115770150119
115780150119     c* se destinatario particolare che prevede appuntamento controllo se presente
115790150119     c* DCR immessa dalla partenza perchè in questo caso non devo dare errore per la
115800150119     c* mancanza della "A" di appuntamento
115810150119     c                   eval      w_noapp=' '
115820150119     c                   if        wdisag='A'
115830150119     c                   if        (par5mdp='S' and par5gdcr>*zeros
115840150119     c                                          and par5gtcr=' ')
115850150119     c                             or (par5mdp=' ' and arbdcr>0 and arbtcr= ' ')
115860150119     c                   eval      w_noapp='1'
115870150119     c                   endif
115880150119     c                   endif
115890041015     c*
115900041015     C***  wdisag        ifne      *blanks
115910041015     c***  wdisag        andne     vidtc1
115920041015     c***  wdisag        andne     vidtc2
115930150119     c                   if        wdisag <> *blanks and w_noapp=' ' and
115940041015     c                             %subst(arbGva:1:1) <> 'U' or
115950041015     c                             wdisag='X' and %subst(arbGva:1:1)='U'
115960041015     c     wdisag        ifne      vidtc1
115970041015     c     wdisag        andne     vidtc2
115980020603     C                   MOVEa     msg(112)      k78
115990020531     c                   select
116000020531     c     wdisag        wheneq    'X'
116010020531     c                   eval      w014a = 'disagiato     '
116020020531     c     wdisag        wheneq    'S'
116030020531     c                   eval      w014a = '"supermercato"'
116040040705     c     wdisag        wheneq    'A'
116050040705     c                   eval      w014a = '"appuntamento"'
116060020531     c                   endsl
116070020531     C                   MOVEa     w014a         k78(14)
116080020531     C                   MOVEA     K78           VIDMSG
116090020603     C                   SETON                                        449028
116100020531     C                   GOTO      ENDCT7
116110020531     c                   endif
116120041015     c                   endif
116130070907     C* Non è possibile indicare contemporaneamente alcune consegne    i
116140070907     C*  particolari
116150051114     c                   move      vidtc1        wtc1
116160051114     c                   move      vidtc2        wtc2
116170051114     c                   exsr      sr_chkcomb
116180051114     c                   if        wcomb = 'S'
116190051114     C                   movel     MSG(113)      VIDmsg
116200051116     c                   eval      %subst(vidmsg:63:1) = vidtc1
116210051116     c                   eval      %subst(vidmsg:69:1) = vidtc2
116220051114     C                   seton                                        459028
116230051114     C                   goto      ENDCT7
116240051114    2C                   ENDIF
116250941102     C*
116260920320     C     ENDCT7        ENDSR
116270021129      **
116280021129      *--- CONTROLLI CAMPI CONSEGNA RICHIESTA SUPERMERCATI -----------*
116290021129     C     CONTR71       BEGSR
116300021129     C                   SETOFF                                       90
116310021129
116320030123      * Se la spedizione non ha già la consegna particolare 'S'
116330030123     c                   If        ArbTc1 <> 'S' and ArbTc2 <> 'S'
116340030123      * E' obbligatorio inserire la consegna
116350030123     c                   If        (VidTc1 = *Blanks and VidTc2 = *Blanks)
116360030123     c                   Eval      VidMsg = Msg(117)
116370030123     c                   Eval      *In44 = *On
116380030123     c                   Eval      *In28 = *On
116390030123     c                   Eval      *In90 = *On
116400030123     c                   GoTo      EndCt71
116410030123     c                   EndIf
116420030127      * Se ci sono già tutte e 2 almeno una deve essere 'S'
116430030127     c                   If        VidTc1 <> 'S' and VidTc2 <> 'S'
116440030127     c                   Eval      VidMsg = Msg(122)
116450030127     c                   Eval      *In44 = *On
116460030127     c                   Eval      *In28 = *On
116470030127     c                   Eval      *In90 = *On
116480030127     c                   GoTo      EndCt71
116490030127     c                   EndIf
116500030123      * Se la prima consegna c'è già e non è supermercato devo mettere 'S' nella seconda
116510030127     c                   If        ArbTc1 <> *Blanks and VidTc1 <> 'S' and
116520030127     c                             VidTc2 = *Blanks
116530030123     c                   Eval      VidMsg = Msg(117)
116540030123     c                   Eval      *In45 = *On
116550030123     c                   Eval      *In28 = *On
116560030123     c                   Eval      *In90 = *On
116570030123     c                   GoTo      EndCt71
116580030123     c                   EndIf
116590030123      * Se la seconda consegna c'è già e non è supermercato devo mettere 'S' nella prima
116600030127     c                   If        ArbTc2 <> *Blanks and VidTc2 <> 'S' and
116610030127     c                             VidTc1 = *Blanks
116620030123     c                   Eval      VidMsg = Msg(117)
116630030123     c                   Eval      *In45 = *On
116640030123     c                   Eval      *In28 = *On
116650030123     c                   Eval      *In90 = *On
116660030123     c                   GoTo      EndCt71
116670030123     c                   EndIf
116680021129     c                   EndIf
116690021129
116700021129      * Se è stata inserita la consegna particolare 'S' si devono
116710021129      * immettere anche la persona da contattare, il telefono e il motivo
116720021129     c                   If        VidTc1 = 'S' or VidTc2 = 'S'
116730021129      * Nominativo
116740021129     c                   If        VidNom = *Blanks
116750021129     c                   Eval      VidMsg = Msg(118)
116760021129     c                   Eval      *In57 = *On
116770021129     c                   Eval      *In28 = *On
116780021129     c                   Eval      *In90 = *On
116790021129     c                   GoTo      EndCt71
116800021129     c                   EndIf
116810021129      * Telefono
116820021129     c                   If        VidTel = *Blanks
116830021129     c                   Eval      VidMsg = Msg(119)
116840021129     c                   Eval      *In58 = *On
116850021129     c                   Eval      *In28 = *On
116860021129     c                   Eval      *In90 = *On
116870021129     c                   GoTo      EndCt71
116880041007     c**                 Else
116890041007     c**                 Clear                   Trul42Ds
116900041007     c**                 Movel     VidTel        D42Fax
116910041007     c**                 Call      'TRUL42R'
116920041007     c***                Parm                    Trul42Ds
116930041007     c***                If        D42Err = '1'
116940041007     c****               Eval      *In58 = *On
116950041007     c**                 Eval      *In28 = *On
116960041007     c**                 Eval      *In90 = *On
116970041007     c**                 Eval      VidMsg = D42Msg
116980041007     c**                 GoTo      EndCt71
116990041007     c**                 EndIf
117000041007     c                   EndIf
117010021129      * Motivo
117020070406      * non più obbligatorio per evitare che scrivano "bagianate"
117030070406     c**                 If        VidMot = *Blanks
117040070406     c***                Eval      VidMsg = Msg(120)
117050070406     c**                 Eval      *In59 = *On
117060070406     c**                 Eval      *In28 = *On
117070070406     c**                 Eval      *In90 = *On
117080070406     c**                 GoTo      EndCt71
117090070406     c**                 EndIf
117100021129     c                   EndIf
117110021129
117120021129     c     EndCt71       EndSr
117130030930      **
117140030930      *--- CONTROLLO SE POSSIBILE LA MANUTENZIONE DELLA DATA CONSEGNA RICHIESTA ---*
117150030930     c     Contr72       BegSr
117160050907
117170030930     c                   Eval      *In90 = *Off
117180070306     c                   clear                   aggioarp          1
117190050907
117200030930      * Se tipo o data o ora consegna richiesta sono state variate
117210031001If  1c                   If        VidTcr <> ArbTcr or
117220031030     c                             ComDcr <> ArbDcr or
117230030930     c                             VidHcr <> ArbHcr
117240150205     c
117250150205     c* permetto sempre la variazione della cons richiesta se sono in partenza
117260160115     c                   if        not *in10 or wfile='P'
117270150205     c                   Goto      EndCt72
117280150205     c                   EndIf
117290150205     c
117300051109      * Se causale 'CS' e giacenza aperta in fase 30 con appuntamento (sulla giacenza)
117310051109      * posso variare la data
117320051109      * quindi bypasso tutti gli altri controlli
117330051109     c                   If        d48cvb = 'CS' and *In04 and wgcfas = 30 and
117340051109     c                             d§gcpapp = 'A'
117350051109     c                   Eval      wgiac = 'S'
117360051109     c                   Goto      EndCt72
117370051109     c                   EndIf
117380040224      * non posso modificare se la bolla ha giacenza aperta (IN04 ON)
117390050907      * e se la fase non è = a 35 o se la bolla è bloccata come giacenza
117400050907if  2c                   If        *In04 and (wgcfas <> 35 or arbfbc = 'G')
117410070312     c                   Eval      VidMsg = Msg(141)
117420040220     c                   Eval      *In28 = *On
117430040220     c                   Eval      *In90 = *On
117440040220     c                   If        VidTcr <> ArbTcr
117450040220     c                   Eval      *In62 = *On
117460040220     c                   GoTo      EndCt72
117470040220     c                   EndIf
117480040220     c                   If        ComDcr <> ArbDcr
117490040220     c                   Eval      *In40 = *On
117500040220     c                   GoTo      EndCt72
117510040220     c                   EndIf
117520040220     c                   If        VidHcr <> ArbHcr
117530040220     c                   Eval      *In41 = *On
117540040220     c                   GoTo      EndCt72
117550040220     c                   EndIf
117560040220    2c                   EndIf
117570040324      * posso modificare se la bolla ha giacenza aperta (IN04 ON) e la fase è = 35
117580040330      * ma solo se metto la data consegna richiesta maggiore/uguale alla data eseguibilità giacenza
117590040324if  2c                   If        *In04 and wgcfas = 35
117600040330if  3c                   If        wgcded > *Zeros and ComDcr >= wgcded
117610040324     c                   Eval      wgiac = 'S'
117620040324     c                   GoTo      EndCt72
117630040324    3c                   EndIf
117640040330if  3c                   If        wgcded = *Zeros and ComDcr >= wgcddm
117650040324     c                   Eval      wgiac = 'S'
117660040324     c                   GoTo      EndCt72
117670040324    3c                   EndIf
117680040324     c                   Eval      VidMsg = Msg(128)
117690040324     c                   Eval      *In28 = *On
117700040324     c                   Eval      *In90 = *On
117710040324     c                   If        VidTcr <> ArbTcr
117720040324     c                   Eval      *In62 = *On
117730040324     c                   GoTo      EndCt72
117740040324     c                   EndIf
117750040324     c                   If        ComDcr <> ArbDcr
117760040324     c                   Eval      *In40 = *On
117770040324     c                   GoTo      EndCt72
117780040324     c                   EndIf
117790040324     c                   If        VidHcr <> ArbHcr
117800040324     c                   Eval      *In41 = *On
117810040324     c                   GoTo      EndCt72
117820040324     c                   EndIf
117830040324    2c                   EndIf
117840050907      * richiamo il controllo della "7Q"
117850050907     c                   ExSr      Contr7q
117860030930      * posso modificare se
117870031001      *                     Peso o volume da fatturare sono >= a quanto tabellato in VPO
117880031001If  2c                   If        V1cPkf >= §VpoPkg or
117890031001     c                             V1cVlf >= §VpoVlm or
117900031001      *                     Destinatario supermercato o Apputamento
117910031001     c                             VidTc1 = 'S' or VidTc1 = 'A' or
117920031001     c                             VidTc2 = 'S' or VidTc2 = 'A' or
117930150506     c                             VidTc1 = 'Z' or VidTc2 = 'Z' or
117940031001      *                     Lasciato avviso
117950031001     c                             ArbFbc = 'A' or
117960031001      *                     Fermo deposito
117970031114     c                             ArbFfd = 'S' or
117980031114      *                     Tipo bolla "FW" Recapito c/assegni
117990050907     c                             ArbCbo = 'FW' or
118000050907      *                     Periodo trazione ridotte e bolla con particolarità "G8"
118010050907     c                             wDCRG8 = 'S'
118020031001     c                   GoTo      EndCt72
118030031001    2c                   EndIf
118040070320     c
118050070320     c* Se FFD immesso dalla partenza, anche se già tolto, posso
118060070320     c*  manutenzionare la spedizione
118070070920     c***                Clear                   dAr5Gen
118080070920     c***                Eval      kAr5Trd = 'GEN'
118090070920     c***  kAr5          Chain(n)  Fiar501l
118100070920     c***                If        %Found(Fiar501l)
118110070920     c***                Movel     Ar5Uni        dAr5Gen
118120070920     c***                if        §ar5MDP='S'  and §ar5FFD='S'
118130070920     c                   if        Wesar5gen='S' and par5mdp='S' and
118140070920     c                             par5FFD='S'
118150070320     c                   GoTo      EndCt72
118160070320    2c                   EndIf
118170070920    2c***                EndIf
118180070312    c
118190070312     c* Se la bolla non è in distinta, non c'e' blocco giacenza e c'e'
118200070312     c*  un evento che prevede la modifica della cons, richiesta
118210070312     c*  allora posso inserirla ed aggiornare direttamente ARB+fiar5
118220070319     c                   if        arbndc=0 and FGSgeot='S' and
118230070312     c                             arbfbc<>'G' and eveADR='S'
118240070312     c                   GoTo      EndCt72
118250070312    3c                   EndIf
118260070312     c
118270031001      *                     C/assegno non abilitato
118280031001If  2c                   If        Ar9Cas > 0
118290031001     c                   Clear                   Trul21ds
118300031001     c                   Eval      I21Tla = 'L'
118310031001     c                   Eval      I21Cbo = ArbCbo
118320031001     c                   Eval      I21Tsp = ArbTsp
118330031001     c                   Eval      I21Lnp = ArbLnp
118340031001     c                   Eval      I21Nzm = ArbNzm
118350031001     c                   Eval      I21Lna = ArbLna
118360031001     c                   Eval      I21Nzd = ArbNzd
118370130827     c****               Eval      I21Ksc = ArbKsc
118380031001     c                   Movel     ArbCtr        I21Ctr
118390031001     c                   Eval      I21Tic = Ar9Tic
118400031001     c                   Eval      I21Imp = Ar9Cas
118410031001     c                   Eval      I21Div = Ar9Vca
118420031001     c                   Eval      I21Ute = Knmus
118430031001     c                   Eval      I21Pgm = nompgm
118440130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
118450130827     c                   if        flgtb1='F'
118460130827     c                   z-add     arbksc        i21ksc
118470130827     c                   else
118480130827     c                   z-add     arbccm        i21ksc
118490130827     c                   if        i21ksc=0
118500130827     c                   z-add     8888          i21ksc
118510130827     c                   movel     arblnp        i21ksc
118520130827     c                   endif
118530130827     c                   endif
118540031001     c                   Call      'TRUL21R'
118550031001     c                   Parm                    Trul21ds
118560031001If  3c                   If        O21Fca <> *Blanks
118570031001     c                   GoTo      EndCt72
118580031001    3c                   EndIf
118590031001    2c                   EndIf
118600070309     c
118610070309     c* Se la bolla è in distinta, non devo però aggiornare
118620070306     c*  fnarb ma FIARP
118630111025    2c                   if        arbndc>0 and FGSgeot='S'
118640111025     c
118650111025     c* Se posso, scrivo FIARP, altrimenti subiro errore
118660111025    3c                   if        DcrinDist<>'S'
118670111025      * erorre
118680111025     c                   Eval      VidMsg = Msg(126)
118690111025     c                   Eval      *In28 = *On
118700111025     c                   Eval      *In90 = *On
118710111025     c                   Eval      *In40 = *On
118720111025     c                   GoTo      EndCt72
118730111025     c
118740111025   x3c                   else
118750070308     c                   eval      Aggioarp='S'
118760111025    4c                   if        comdcr<>savdcr  or vidtcr<>savtcr or
118770070308     c                             vidhcr<>savhcr
118780070309     c* Se richiamo nascosto, imposto solo il campo msgforza,
118790070309     c*  altrimenti emetto il msg
118800070330     c*  non imposto il campo se devo fare solo i controlli (26 OFF)
118810111025    5c                   if        d48ffr<>'E'
118820070308     c                   seton                                        402890
118830070308     c                   Eval      VidMsg = Msg(139)
118840111025   x5c                   else
118850070330     c
118860070330     c                   if        *in26
118870070330     c                   eval      msgforza=142
118880070309     c                   endif
118890111025    5c                   endif
118900070309     c
118910070308     c                   eval      savdcr=comdcr
118920070308     c                   eval      savtcr=vidtcr
118930070308     c                   eval      savhcr=vidhcr
118940111025    4c                   endif
118950111025    3c                   endif
118960070308     c
118970070306     c                   GoTo      EndCt72
118980111025    2c                   endif
118990070306     c
119000040220      * se arrivo qua vuol dire che non posso modificare
119010140806      * Se variazione da internet permetto la modifica
119020140806     c                   if        d48cvb<>'CI'
119030031001     c                   Eval      VidMsg = Msg(126)
119040031001     c                   Eval      *In28 = *On
119050031001     c                   Eval      *In90 = *On
119060031030     c                   If        VidTcr <> ArbTcr
119070031001     c                   Eval      *In62 = *On
119080031001     c                   GoTo      EndCt72
119090031001     c                   EndIf
119100031030     c                   If        ComDcr <> ArbDcr
119110031001     c                   Eval      *In40 = *On
119120031001     c                   GoTo      EndCt72
119130031001     c                   EndIf
119140031001     c                   If        VidHcr <> ArbHcr
119150031001     c                   Eval      *In41 = *On
119160031001     c                   GoTo      EndCt72
119170031001     c                   EndIf
119180140806     c                   endif
119190031001    1c                   EndIf
119200030930
119210030930     c     EndCt72       EndSr
119220920320     C*
119230920320     C*--- AGGIORNAMENTO FILE ----------------------------------------*
119240920320     C     REGIS7        BEGSR
119250070308     c                   clear                   ModconsR          1
119260130412     c                   clear                   wpda_ndc
119270130412     c                   clear                   wpda_ddc
119280130412     c                   clear                   wpda_dcm
119290070308     C* PRENDO I VALORI ASSOLUTI
119300070308     C                   MLLZO     1             VIDHCR
119310070308     c
119320070308     c* Mi salvo i campi della consegna
119330070308     c*  richiesta della bolla per sucessivi aggiornamenti.
119340070308     c                   eval      bolladcr=arbdcr
119350070308     c                   eval      bollahcr=arbhcr
119360070308     c                   eval      bollatcr=arbtcr
119370070308     c
119380070308     c* Mi salvo anche se modificata, solo se c'e' la DATA consegna impostat
119390070308     c                   if        vidtcr<>arbtcr or comdcr<>arbdcr or
119400070308     c                             arbhcr<>vidhcr  and comdcr>0
119410070308     c                   eval      ModconsR='S'
119420070308     c                   endif
119430070308     c
119440950104     C*  GIRO UDATE
119450950104     C                   TIME                    W0140            14 0
119460950104     C* UDATE IN GGMMAAAA
119470950104     C                   MOVE      W0140         WDTGIO            8 0
119480950104     C* UDATE IN AAAAMMGG
119490950104     C                   Z-ADD     WDTGIO        G02DAT
119500950104     C                   MOVEL     *BLANK        G02ERR
119510950104     C                   CALL      'XSRDA8'
119520950104     C                   PARM                    WLBDAT
119530950104     C                   MOVEL     G02INV        DATEU             8 0
119540941213     C                   MOVEL     KNMUS         ARBPRU
119550930507     C*
119560151230     c                   if        not *in10 or wfile='A'
119570930507     C* INVIO PER SEDE
119580170116     C****               MOVEL     'FNARBG0T'    SFILE
119590930507     C* ALLOCO
119600170116     C***                EXSR      ALLOC
119610170116     C** 91              GOTO      ENDRG7
119620070320     c
119630070320     c* Se impostata la "S" o la "A" devo impostare relativo blocco
119640070320     c*  telefonate in DSBL4L
119650070320     c                   clear                   wesar4L           1
119660070320     c                   clear                   BAS               1
119670070320     c                   clear                   TFD               1
119680070320     c                   if        ((vidtc1='S' or vidtc2='S') and
119690070320     c                              arbtc1<>'S' and arbtc2<>'S')  or
119700070320     c                             ((vidtc1='A' or vidtc2='A') and
119710070320     c                              arbtc1<>'A' and arbtc2<>'A')
119720070320     c                   eval      BAS='S'
119730070320     c                   endif
119740070320     c
119750070320     c* se tolto "A" o "S" devo eliminare il blocco
119760070320     c                   if        vidtc1<>'A' and vidtc1<>'S' and
119770070320     c                              vidtc2<>'A' and vidtc2<>'S' and
119780070320     c                             (arbtc1='S' or arbtc1='A' or
119790070320     c                              arbtc2='S' or arbtc2='A')
119800070320     c                   eval      BAS='N'
119810070320     c                   endif
119820161229     c* se aggiunta consegna richiesta devo eliminare il blocco appuntamento
119830161229     c                   if        bolladcr=0 and comdcr>0 and
119840161229     c                             vidtcr=' ' and
119850161229    1c                             AggioARP<>'S' and
119860161229     c                             (vidtc1='A' or vidtc2='A')
119870161229     c                   eval      BAS='N'
119880161229     c                   endif
119890070320     c* Tolgo blocco telefonata FFD se messa consegna richiesta e
119900070320     c*  presente FFD
119910070329     c****               if        arbffd='S' and viddcr>0
119920070329     c****               eval      TFD='N'
119930070329     c****               endif
119940070320     c                   if        bas<>' ' or TFD<>' '
119950070320     c                   eval      TipoEla='C'
119960070320     c                   exsr      Gestar4L
119970070320    1c                   if        *in91
119980170116     c****               exsr      DLCSED
119990070525     C                   SETON                                        2891
120000070320     C                   GOTO      ENDRG7
120010070320    1c                   endif
120020070320     c                   endif
120030041022     c
120040041022     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
120050041022     c* iornare direttamente il file
120060041022     c                   clear                   waltrabolla       1
120070041022     C   10KARB          CHAIN     FNBLP01L                           3091
120080041022     C  n10KARB          CHAIN     FNARB01L                           3091
120090041022     c*
120100070308    1c                   if        *in91
120110170116     c****               exsr      DLCSED
120120070515     c                   unlock    fiar401l
120130070525     C                   SETON                                        2891
120140041022     C                   MOVEL     MSG(4)        VIDMSG
120150041022     C                   GOTO      ENDRG7
120160070308    1c                   endif
120170070308     c
120180070308     c* Se lna attivata con GEO
120190070308     c* Se ho aggiornato FIARP, e devo aggiornare altri dati,
120200070308     c* nei campi della consegna richeista reimposto quelli della bolla
120210070319    1c                   if        FGSgeot='S'and AggioARP='S'
120220070308     c                   eval      comdcr=Bolladcr
120230070308     c                   eval      vidhcr=bollahcr
120240070308     c                   eval      vidtcr=Bollatcr
120250070308     c                   clear                   ModConsR
120260070308     c                   endif
120270070308     c
120280041022     c                   if        *in30
120290041022     c                   eval      waltrabolla='N'
120300041022     c                   else
120310041022     c* memorizzo campi prima della variazione
120320041022     c                   eval      waltrotc1=arbtc1
120330041022     c                   eval      waltrotc2=arbtc2
120340080422     c   10              eval      waltroft3=arbft3
120350130411     c  n10              eval      wpda_ndc=arbndc
120360130411     c  n10              eval      wpda_ddc=arbddc
120370130411     c  n10              eval      wpda_dcm=arbdcm
120380141223     c  n10              eval      bollaFBC=arbfbc
120390041022     C   10KARB          CHAIN     FNARB01L                           30
120400041022     C  N10KARB          CHAIN     FNBLP01L                           30
120410041022     c* Alla fine per non sporcarmi i campi del record, richaino il
120420041022     c*  record originale che sto variando
120430130411     c                   eval      wpda_ndc=arbndc
120440130411     c                   eval      wpda_ddc=arbddc
120450130411     c                   eval      wpda_dcm=arbdcm
120460041022     c                   endif
120470151230     c                   endif
120480930507     C*
120490930507     C                   Z-ADD     DATEU         ARBDTV
120500930507     C                   TIME                    ARBORV
120510930507     C                   MOVEL     VIDCVR        ARBCVB
120520001204     C*
120530001204     C                   MOVEL     ARBTC1        SAVTC1
120540001204     C                   MOVEL     ARBTC2        SAVTC2
120550080415     c*
120560930507     C*
120570060130     C* SE DA STORICIZZARE
120580060130     C**   *IN10         IFEQ      *ON
120590060130    1C**   D66FSA        ANDEQ     'S'
120600060130     C**   *IN10         OREQ      *OFF
120610060130    1C**   D66FSP        ANDEQ     'S'
120620070308     c
120630060130    1C     D66FSA        ifeq      'S'
120640011108      *
120650011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
120660011108      * è in partenza 'P' o in arrivo 'A'
120670011108     C     *IN10         IFEQ      *ON
120680160119     c                   if        wfile='P'
120690160119     C                   MOVE      'a'           ARBPRU
120700160119     c                   else
120710011108     C                   MOVE      'A'           ARBPRU
120720160119     c                   endif
120730011108     C                   ENDIF
120740011108     C     *IN10         IFEQ      *OFF
120750011108     C                   MOVE      'P'           ARBPRU
120760011108     C                   ENDIF
120770011108      *
120780941014     C                   WRITE     FNARBG00
120790930507     C                   END
120800060301     c* storicizzazione motivazione
120810060301     c     d66mtv        ifeq      'S'
120820060301     c                   exsr      sto_mtv
120830060301     c                   endif
120840040224      * se in arrivo ho modificato uno dei dati del Fiar5 rcd GEN:
120850040224      * - Tipo/Data/Ora consegna richiesta
120860040224      * - Giorni di chiusura
120870040224      * - Consegna particolare
120880040224      * devo memorizzare i dati della bolla prima della modifica
120890150129     c******             If        *In10 and
120900150206     c* dal 06/02 momorizziamo anche se varia la partenza, come se lo facesse l'arrivo
120910150129     c                   if         (VidTcr <> ArbTcr or ComDcr <> ArbDcr or
120920040224     c                               VidHcr <> ArbHcr or VidGc1 <> ArbGc1 or
120930040224     c                               VidGc2 <> ArbGc1 or VidTc1 <> ArbTc1 or
120940170405     c                               VidTc2 <> ArbTc2 )
120950041022     c                   ExSr      RegAr5AR
120960170405     c                   Else
120970170405     c* Se tutto uguale memorizzo i dati di ripristino nel fiar5 rec GEN solo se
120980170405     c* già esistente e causale CI oppure CD
120990170405     c                   if        (d48cvb='CI' or d48cvb='CD')  and
121000170405     c                             wesar5gen='S'
121010170405     c                   ExSr      RegAr5AR
121020170405     c                   EndIf
121030170405     c                   EndIf
121040080415     c*
121050080415     c* Se modifico data consegna richiesta verifico se devo sflaggare per
121060080415     c* clienti
121070080415     c                   eval      wsflagga_C=' '
121080080422     c                   if        comdcr<>arbdcr or
121090080415     c                             vidtcr<>arbtcr
121100080415     c                   clear                   wcmitt
121110080415     c                   if        flgtb1='A'
121120080415     c                   move      arbccm        wcmitt
121130080415     c                   else
121140080415     c                   move      arbksc        wcmitt
121150080415     c                   endif
121160080416     c                   if        wcmitt>'0000000'
121170080416     c                             and %subst(wcmitt:4:4)<>'8888'
121180080415     C                   MOVEL     '3K'          COD
121190080415     C                   MOVEL(P)  wcmitt        KEY
121200080415     C                   EXSR      CTABEL
121210080415     C  N30              MOVEL     TBLUNI        DS3K
121220080415     C   30              CLEAR                   DS3K
121230080415     c                   if        §3kcq8='S'
121240080415     c                   eval      wsflagga_C='S'
121250080415     c                   endif
121260080415     c                   endif
121270080415     c                   endif
121280041022     c
121290041022     c* Se variazione in partenza e modifico  le cons.partic
121300041022     c*  sflaggo per clienti
121310041025     c  n10              if        vidtc1<>arbtc1 or vidtc2<>arbtc2
121320080415     c                             or wsflagga_c='S'
121330041022     c                   clear                   arbft3
121340041022     c                   endif
121350070319     c
121360070319     c* Se impostata la "S" o la "A" devo impostare relativo blocco
121370070319     c*  telefonate in DSBL4L
121380161229     c* idem se aggiunta la dcr devo sbloccare eventuale appuntamento
121390151230     c                   if        not *in10 or wfile='A'
121400070320     c                   if        wesar4L<>' '
121410070320     c                   if        BAS='S'
121420070320     c                   eval      §b4lbas='S'
121430070320     c                   endif
121440070320     c                   if        BAS='N'
121450070320     c                   eval      §b4lbas=' '
121460070320     c                   endif
121470070320     c
121480070320     c                   if        TFD='N'
121490070320     c                   eval      §b4lTFD=' '
121500070320     c                   endif
121510070320     c
121520070320     c                   eval      TipoEla='A'
121530070320     c                   exsr      GestAR4L
121540070320     c                   endif
121550151230     c                   endif
121560070320     c
121570920320     C*
121580920320     C                   MOVEL     COMDCR        ARBDCR
121590920320     C                   MOVEL     VIDHCR        ARBHCR
121600920320     C                   MOVEL     VIDTCR        ARBTCR
121610930507     C                   MOVEL     VIDGC1        ARBGC1
121620930507     C                   MOVEL     VIDGC2        ARBGC2
121630941102     C                   MOVEL     VIDTC1        ARBTC1
121640941102     C                   MOVEL     VIDTC2        ARBTC2
121650070308     c
121660070308     c* Se modificata consegna richiesta in arrivo
121670070308     c*  devo anche pulire il flag blocco consegna se = 'A'
121680141223     c                   if        BollaFBC='A' and ModconsR='S'
121690070308     c                             and comdcr>0
121700070308     c                   clear                   arbfbc
121710070419     c                   clear                   arbcmc
121720070308     c                   endif
121730070920     c
121740070920     c* Se modificata la consegna richiesta e bolla già consegnata
121750070920     c* effettuo il ricalcolo della affidabilità
121760070920     c                   if        ModconsR='S' and arbdcr=0 and arbdcm>0
121770070920     c                   clear                   arbdti
121780070920     c                   clear                   arbhti
121790070920     c                   movel     'S'           arbfbs
121800070920     c                   endif
121810041025     c
121820070319     c* Verifico se devo aggiornare record già esistente in ar5
121830150206     c*  ma solo per le consegne particolari e fermo deposito
121840150206     c  N10              exsr      RegAr5PA
121850941212     C*
121860930507     C* PER SEDE
121870151230     c                   if        not *in10 or wfile='A'
121880070503     C     D66FFI        IFEQ      'S'
121890950227     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
121900950413    2C     §3ABSD        IFEQ      ' '
121910930507     C*
121920170116     C***                OPEN      FNARBG0T
121930941014     C                   WRITE     FNARBGTT
121940170116     C***                CLOSE     FNARBG0T
121950950227     C                   END
121960930507     C* DISALLOCO OGGETTO
121970170116     C****               EXSR      DLCSED                                       FNARBG0T
121980950227     C                   END
121990151230     c                   endif
122000041022     c
122010930507     C*
122020151230     c                   if        wfile='A'
122030151230     C                   UPDATE    FNARB000
122040151230     c                   else
122050151230     C                   UPDATE    FNBLP000
122060151230     c                   endif
122070941220     C*
122080941220     C* DISALLOCO I RECORD CHE NON USO
122090160119     C   10              UNLOCK    FIAR601L
122100041022     c* Aggiorno anche l'altra bolla
122110151230     c                   if        not *in10 or wfile='A'
122120041022     c                   if        waltrabolla=' '
122130080422     c   10              eval      arbft3=waltroft3
122140041025     c* SE aggiorno bopartenza sflaggo per clienti se variate le cons.partic
122150041025     c                   if        *in10 and (waltrotc1<>arbtc1 or
122160080415     c                             waltrotc2<>arbtc2 or
122170080415     c                             wsflagga_c='S')
122180041022     c                   clear                   arbft3
122190041022     c                   endif
122200041022     C   10              except    arbGblp
122210041022     C  N10              except    arbGarb
122220041022     c                   endif
122230151230     c                   endif
122240950602     C*
122250070308     c* Se non è previsto l'aggiornamento di FIARP, vedo se lo devo
122260070308     c*  cancellare (se variata la data consegna richiesta)
122270070319     c                   if        FGSgeot='S'  and  ModconsR='S'
122280070308     c     karb          delete    fiarp01l
122290070308     c                   endif
122300070308     c
122310040224      * Se c'è la data consegna richiesta controllo se è ancora valida
122320040224      * solo se ho fatto una modifica in arrivo
122330150129      * da adesso anche se modifica in partenza
122340150206     c                   If        ArbDcr > *Zeros
122350040324     c                             and wCa = *Blanks and wgiac = *Blanks
122360040224     c                   ExSr      ContrDcr
122370040224     c                   EndIf
122380070308     c
122390130411     C*
122400130411     c* Richiamo routine per invio variazione a pda
122410130411     c                   if        D48Cvb<>'CS'
122420151230     c                             and (not *in10 or wfile='A')
122430130411     c                   exsr      sr_pda
122440130411     c                   endif
122450950104     C*
122460930507     C     ENDRG7        ENDSR
122470021129      **
122480021129      *--- AGGIORNAMENTO FILE FIAR5 ----------------------------------*
122490021129     c     Regis71       BegSr
122500021129
122510021129      * Se è stata indicata la consegna particolare 'S' devo scrivere
122520021129      * il file FIAR5
122530021129     c                   If        VidTc1 = 'S' or VidTc2 = 'S'
122540021129     c                   Clear                   Fiar5000
122550021129     c                   Eval      Ar5Aas = ArbAas
122560021129     c                   Eval      Ar5Lnp = ArbLnp
122570021129     c                   Eval      Ar5Nrs = ArbNrs
122580021129     c                   Eval      Ar5Nsp = ArbNsp
122590021129     c                   Eval      Ar5Trd = 'SCR'
122600021129     c                   Move      Dateu         Ar5Dac
122610021129     c                   Movel     W0140         Ar5Orc
122620021129     c                   Clear                   Dar5Scr
122630021129     c                   Eval      §ar5Cvb = D48Cvb
122640021129     c                   Eval      §ar5Pru = Knmus
122650060131     c                   Eval      §ar5Pou = dutPou
122660021129     c                   Eval      §ar5Tcr = VidTcr
122670021129     c                   Eval      §ar5Dcr = ComDcr
122680021129     c                   Eval      §ar5Hcr = VidHcr
122690021129     c                   Eval      §ar5Gc1 = VidGc1
122700021129     c                   Eval      §ar5Gc2 = VidGc2
122710021129     c                   Eval      §ar5Tc1 = VidTc1
122720021129     c                   Eval      §ar5Tc2 = VidTc2
122730021129     c                   Eval      §ar5Nom = VidNom
122740021129     c                   Eval      §ar5Tel = VidTel
122750021129     c                   Eval      §ar5Mot = VidMot
122760021129     c                   Movel     Dar5Scr       Ar5Uni
122770021129     c                   Write     Fiar5000
122780021129     c                   EndIf
122790040705
122800040705      * Aggiorno anche il record 'GEN' di Fiar5 con il nuovo Telefono e Referente
122810091217     c                   eval      Wtipoagg='REF'
122820091217     c                   exsr      RegisAR5GEN
122830091217     c***
122840091217     c***                Eval      kAr5Trd = 'GEN'
122850091217     c***                Clear                   dAr5Gen
122860091217     c***  kAr5          Chain     Fiar501l
122870040705      * Aggiorno Fiar5
122880091217if  1c***                If        %Found(Fiar501l)
122890091217     c***                Eval      dAr5Gen = Ar5Uni
122900091217     c***                Eval      §Ar5Ref = VidNom
122910091217     c***                Eval      §Ar5Teld = VidTel
122920091217     c***                Movel(p)  dAr5Gen       Ar5Uni
122930040705      * se la modifica è fatta in partenza e devo trasmettere all'arrivo
122940040705      * sfleggo per la trasmissione
122950070306     c***                If        Not *In10 and §Ar5Trar = 'S'
122960070306     c***                Clear                   Ar5Ft1
122970070306     c***                EndIf
122980040705      * se la modifica è fatta in arrivo e devo trasmettere alla partenza
122990040705      * sfleggo per la trasmissione
123000070306     c***                If        *In10 and §Ar5Trpa = 'S'
123010070306     c***                Clear                   Ar5Ft1
123020070306     c***                EndIf
123030040705      * se devo trasmettere alla sede sfleggo per la trasmissione
123040091217     c***                If        §Ar5Trse = 'S'
123050091217     c***                Clear                   Ar5Ft2
123060091217     c***                EndIf
123070040705      * sfleggo sempre per la trasmissione al cliente
123080091217     c***                Clear                   Ar5Ft3
123090091217     c***                Update    Fiar5000
123100040705      * Scrivo Fiar5
123110091217   x1c***                Else
123120040705      * se immessi i dati
123130091217     c***                Clear                   Fiar5000
123140091217     c***                Eval      Ar5Aas = ArbAas
123150091217     c***                Eval      Ar5Lnp = ArbLnp
123160091217     c***                Eval      Ar5Nrs = ArbNrs
123170091217     c***                Eval      Ar5Nsp = ArbNsp
123180091217     c***                Eval      Ar5Trd = 'GEN'
123190091217     c***                Move      Dateu         Ar5Dac
123200091217     c***                Movel     W0140         Ar5Orc
123210091217     c***                Clear                   Dar5Gen
123220091217     c***                Eval      §Ar5Ref = VidNom
123230091217     c***                Eval      §Ar5Teld = VidTel
123240091217     c***                Movel(p)  DAr5Gen       Ar5Uni
123250091217     c***                Write     Fiar5000
123260091217    1c***                EndIf
123270130411     c* Richiamo routine per invio variazione a pda
123280130411     c                   exsr      sr_pda
123290021129
123300021129     c                   EndSr
123310040223
123320041022      *--- CONTROLLO E AGGIORNAMENTO FIAR5 RCD GEN se sono in arrivo--*
123330041022     c     RegAr5AR      BegSr
123340070309     c* Se Attiva GEO e bolla non in distnta e non giacente con
123350070309     c*  un evento che prevede la modifica della cons.richista
123360070309     c*  memorizzo la cons richiesta nei campi di FIAR5
123370070309     c                   clear                   AggioAR5M         1
123380070309     c
123390070319     c                   if        arbndc=0 and FGSgeot='S' and
123400070309     c                             arbfbc<>'G' and eveADR='S'
123410070309     c                   eval      AggioAR5M='S'
123420070309     c                   endif
123430160114     c* Se manutenzione in Arrivo su bolla ancora da partire memorizzo
123440160114     c* di aggiornare i dati del ripristino con quelli del video
123450160114     c                   if        *in10 and wfile = 'P'
123460160114     c                   eval      AggioAR5M='S'
123470160114     c                   endif
123480040223
123490040224     c                   Clear                   waggar5
123500040223     c                   Clear                   dAr5Gen
123510040223     c                   Eval      kAr5Trd = 'GEN'
123520040223     c     kAr5          Chain     Fiar501l
123530040223     c                   If        %Found(Fiar501l)
123540040223     c                   Movel     Ar5Uni        dAr5Gen
123550040223     c                   EndIf
123560040223
123570040223      * Dati non memorizzati
123580040223if  1c                   If        §Ar5Mdp <> 'S'
123590040224     c                   Eval      §Ar5Mdp = 'S'
123600040223     c                   Eval      §Ar5gTcr = ArbTcr
123610040223     c                   Move      ArbDcr        §Ar5gDcr
123620040223     c                   Move      ArbHcr        §Ar5gHcr
123630040223     c                   Eval      §Ar5gGc1 = ArbGc1
123640040223     c                   Eval      §Ar5gGc2 = ArbGc2
123650040223     c                   Eval      §Ar5gTc1 = ArbTc1
123660040223     c                   Eval      §Ar5gTc2 = ArbTc2
123670040223     c                   Eval      §Ar5Ffd  = ArbFfd
123680040223      * Causale 'CD' da chiusura distinta
123690140806      *   oppure
123700140806      * Causale 'CI' da indicazioni da internet
123710040223      * imposto i campi a video
123720140806if  2c                   If        d48cvb = 'CD' or d48cvb='CI' or AggioAR5M='S'
123730040223     c                   Eval      §Ar5Tcrm = VidTcr
123740040223     c                   Move      ComDcr        §Ar5Dcrm
123750040223     c                   Move      VidHcr        §Ar5Hcrm
123760040223      * Altra causale
123770040223      * imposto i campi di arb prima della variazione
123780040223   x2c                   Else
123790040223     c                   Eval      §Ar5Tcrm = ArbTcr
123800040223     c                   Move      ArbDcr        §Ar5Dcrm
123810040223     c                   Move      ArbHcr        §Ar5Hcrm
123820040223    2c                   EndIf
123830040224     c                   Eval      waggar5 = 'S'
123840040223      * Dati memorizzati
123850040223   x1c                   Else
123860140806if  2c                   If        d48cvb = 'CD' or d48cvb='CI' or AggioAR5M='S'
123870040223     c                   Eval      §Ar5Tcrm = VidTcr
123880040223     c                   Move      ComDcr        §Ar5Dcrm
123890040223     c                   Move      VidHcr        §Ar5Hcrm
123900040224     c                   Eval      waggar5 = 'S'
123910040223    2c                   EndIf
123920040223    1c                   EndIf
123930150206     c* se si tratta di una modifica di cons richiesta in partenza lo memorizzo se
123940150206     c*  non l'ho già fatto
123950150206     c                   if        not *in10 and comdcr<>arbdcr and
123960150206     c                             §ar5dcrp<>'P'  and comdcr>0
123970150206     c                   eval      §ar5dcrp= 'P'
123980150206     c                   Eval      waggar5 = 'S'
123990150206    2c                   EndIf
124000150206     c                   if        not *in10 and comdcr<>arbdcr and
124010150206     c                             §ar5dcrp= 'P'  and comdcr=0
124020150206     c                   eval      §ar5dcrp= ' '
124030150206     c                   Eval      waggar5 = 'S'
124040150206    2c                   EndIf
124050150206     c
124060040223
124070040224if  1c                   If        waggar5 = 'S'
124080040224      * aggiorno Fiar5
124090040224if  2c                   If        %Found(Fiar501l)
124100040224     c                   Movel(p)  dAr5Gen       Ar5Uni
124110040224      * se devo trasmettere alla sede sfleggo per la trasmissione
124120040224     c                   If        §Ar5Trse = 'S'
124130040224     c                   Clear                   Ar5Ft2
124140040224     c                   EndIf
124150040224      * sfleggo sempre per la trasmissione al cliente
124160040224     c                   Clear                   Ar5Ft3
124170040223     c                   Update    Fiar5000
124180040224      * scrivo Fiar5
124190040224   x2c                   Else
124200040224     c                   Clear                   Fiar5000
124210040223     c                   Eval      Ar5Aas = ArbAas
124220040223     c                   Eval      Ar5Lnp = ArbLnp
124230040223     c                   Eval      Ar5Nrs = ArbNrs
124240040223     c                   Eval      Ar5Nsp = ArbNsp
124250040224     c                   Eval      Ar5Trd = 'GEN'
124260040223     c                   Move      Dateu         Ar5Dac
124270040223     c                   Movel     W0140         Ar5Orc
124280040224     c                   Movel(p)  dAr5Gen       Ar5Uni
124290041007     c* flaggo sempre il flag di filiale tanto non trasmettiamo più
124300040223     c                   Write     Fiar5000
124310040224    2c                   EndIf
124320040224   x1c                   Else
124330040224     c                   Unlock    Fiar501l
124340040224    1c                   EndIf
124350040223
124360040223     c                   EndSr
124370041022
124380041022      *--- CONTROLLO E AGGIORNAMENTO FIAR5 RCD GEN se sono in partenz-*
124390041022     c     RegAr5PA      BegSr
124400041022     c                   Clear                   dAr5Gen
124410041022     c                   Eval      kAr5Trd = 'GEN'
124420041022     c     kAr5          Chain     Fiar501l
124430041022if  2c                   If        %Found(Fiar501l)
124440041022     c                   Movel     Ar5Uni        dAr5Gen
124450041022      * Flag dati memorizzati
124460041022if  3c                   If        §Ar5Mdp = 'S'
124470041022      * Aggiorno i dati di ripristino solo se uguali a quelli salvati
124480150206if  4c***                If        §Ar5gTcr= §Ar5Tcrm and §Ar5gDcr = §Ar5Dcrm
124490150206     c***                          and §Ar5gHcr = §Ar5Hcrm
124500150206     c***                Eval      §Ar5Tcrm = ArbTcr
124510150206     c***                Move      ArbDcr        §Ar5Dcrm
124520150206     c***                Move      ArbHcr        §Ar5Hcrm
124530150206    4c***                EndIf
124540041022      * Memorizzo i dati della bolla
124550150206     c***                Eval      §Ar5gTcr = ArbTcr
124560150206     c***                Move      ArbDcr        §Ar5gdcr
124570150206     c***                Move      ArbHcr        §Ar5gHcr
124580150206     c***                Eval      §Ar5gGc1 = ArbGc1
124590150206     c***                Eval      §Ar5gGc2 = ArbGc2
124600150206     c* Da 02/15
124610150206     c* come partenza aggiorno soltanto i dati del fermo deposito e dei giorni di chiusura
124620041022     c                   Eval      §Ar5gTc1 = ArbTc1
124630041022     c                   Eval      §Ar5gTc2 = ArbTc2
124640041022     c                   Eval      §Ar5Ffd = arbFfd
124650041022    3c                   EndIf
124660041022     c                   Movel     Dar5Gen       Ar5Uni
124670041022     c                   Update    Fiar5000
124680041022    2c                   EndIf
124690041022     c                   EndSr
124700070906
124710070906      *--- CONTROLLO SE ANCORA VALIDA Le consegne particolari --------*
124720070906     c     ContrTCP      BegSr
124730070910      *
124740070906     c                   clear                   eliminatcp        1
124750070911     c                   clear                   wcomb
124760070911     c                   clear                   aggtcp            1
124770070906     c
124780070910    1c                   if        arbtc1=wdisagdest
124790070910if  2c                   If        (par5Mdp = 'S' and
124800070910     c                             par5gtc1<>arbtc1 and par5gtc2<>arbtc1)
124810070911     c*******                      or   par5Mdp=' '
124820070906     c                   eval      eliminatcp='1'
124830070911     c                   eval      wtc1=arbtc1
124840070907    2c                   endif
124850070910    1c                   endif
124860070910    1c                   if        arbtc2=wdisagdest
124870070910if  2c                   If        (par5Mdp = 'S' and
124880070910     c                             par5gtc1<>arbtc2 and par5gtc2<>arbtc2)
124890070911     c********                     or   par5Mdp=' '
124900070906     c                   eval      eliminatcp='2'
124910070911     c                   eval      wtc1=arbtc2
124920070907    2c                   endif
124930070910    1c                   endif
124940070906     c* Se da eliminare
124950070910    1c                   if        eliminatcp<>' '
124960070911     c* Verifico se per le combinazioni devo ripristinare la cons partic
124970070911     c*  della partenza
124980070911     c                   if        par5mdp='S' and par5gtc1<>' '
124990070911     c                   eval      wtc2=par5gtc1
125000070911     c                   exsr      sr_chkcomb
125010070911     c
125020070911     c                   if        wcomb<>' '
125030070911     c                   eval      aggtcp=par5gtc1
125040070911     c                   endif
125050070912     c                   endif
125060070911     c
125070070911     c                   if        wcomb=' ' and par5gtc2<>' '
125080070911     c                   eval      wtc2=par5gtc1
125090070911     c                   exsr      sr_chkcomb
125100070911     c                   if        wcomb<>' '
125110070911     c                   eval      aggtcp=par5gtc2
125120070911     c                   endif
125130070911     c                   endif
125140070911     c
125150070906     c                   Clear                   dsarbg
125160070906     c                   Eval      d48cvb = 'CA'
125170070906     c                   Eval      weseg = 'E'
125180070911     c*****              Clear                   d48ffr
125190070906     c                   Eval      d48Trc = 'G'
125200070906     c                   Eval      §bgTcr = arbtcr
125210070906     c                   Move      arbdcr        §bgDcr
125220070906     c                   Move      arbhcr        §bgHcr
125230070906     c                   Eval      §bgGc1 = Arbgc1
125240070906     c                   Eval      §bgGc2 = ArbGc2
125250070910    2c                   if        eliminatcp='1'
125260070906     c                   clear                   §bgtc1
125270070906     c                   clear                   arbtc1
125280070906     c                   Eval      §bgtc2 = Arbtc2
125290070911     c* ripristino la vecchia consegna partic.
125300070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
125310070911     c                   eval      §bgtc1=aggtcp
125320070911     c                   endif
125330070910   x2c                   else
125340070906     c                   clear                   §bgtc2
125350070906     c                   clear                   arbtc2
125360070906     c                   Eval      §bgtc1 = Arbtc1
125370070911     c* ripristino la vecchia consegna partic.
125380070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
125390070911     c                   if        §bgtc1=' '
125400070911     c                   eval      §bgtc1=aggtcp
125410070911     c                   else
125420070911     c                   eval      §bgtc1=aggtcp
125430070911     c                   endif
125440070911     c                   endif
125450070910    2c                   endif
125460070907     c
125470070907     c* Memorizzo in skiera rielaborazioni
125480070910    2c                   if        kk<=10
125490070907     c                   add       1             kk
125500070907     c                   movel     dsarbg        kdati(KK)
125510070907     c                   movel     d48cvb        kcvb(KK)
125520070907     c                   movel     d48trc        ktrc(KK)
125530070911     c                   movel     ' '           kffr(KK)
125540070911     c                   movel     ' '           kf12(KK)
125550070910    2c                   endif
125560070910    1c                   endif
125570070906     c
125580070906     c                   EndSr
125590040224
125600040224      *--- CONTROLLO SE ANCORA VALIDA LA DATA CONSEGNA RICHIESTA -----*
125610160114     c     ContrDcr      BegSr
125620040225
125630040225     c                   Move      ArbDcr        warbdcr
125640050907      * richiamo il controllo della "7Q"
125650050907     c                   ExSr      Contr7q
125660040225
125670040225      * Controllo se c'è il fiar5 rcd GEN
125680070906     c                   clear                   dar5gen
125690040225     c                   Eval      kAr5Trd = 'GEN'
125700040225     c     kAr5          Chain(n)  Fiar501l
125710040225     c                   If        %Found(Fiar501l)
125720040225     c                   Movel     Ar5Uni        dAr5Gen
125730040225     c                   EndIf
125740040225      * se ho memorizzato i dati
125750040225if  1c                   If        §Ar5Mdp = 'S' and
125760040225      * e la data è diversa
125770040225     c                             (wArbDcr <> §Ar5Dcrm or
125780040225     c                               ArbTcr <> §Ar5Tcrm)
125790040225      * controllo se la data è valida quando:
125800040224      *                     Peso o volume da fatturare sono >= a quanto tabellato in VPO
125810040225if  2c                   If        ArbPkf >= §VpoPkg or
125820040224     c                             ArbVlf >= §VpoVlm or
125830150506      *                     Destinatario supermercato o Appuntamento
125840040224     c                             ArbTc1 = 'S' or ArbTc1 = 'A' or
125850040224     c                             ArbTc2 = 'S' or ArbTc2 = 'A' or
125860150506     c                             ArbTc1 = 'Z' or ArbTc2 = 'Z' or
125870040312      *                     Lasciato avviso
125880070308     c                             BollaFbc = 'A' or
125890040224      *                     Fermo deposito
125900040224     c                             ArbFfd = 'S' or
125910040224      *                     Tipo bolla "FW" Recapito c/assegni
125920040811     c                             ArbCbo = 'FW'or
125930040811      *                     Periodo trazione ridotte e bolla con particolarità "G8"
125940040811     c                             wDCRG8 = 'S'
125950040225     c                   Goto      EndContr
125960040225    2c                   EndIf
125970040224      *                     C/assegno non abilitato
125980040225if  2c                   If        Ar9Cas > 0
125990040224     c                   Clear                   Trul21ds
126000040224     c                   Eval      I21Tla = 'L'
126010040224     c                   Eval      I21Cbo = ArbCbo
126020040224     c                   Eval      I21Tsp = ArbTsp
126030040224     c                   Eval      I21Lnp = ArbLnp
126040040224     c                   Eval      I21Nzm = ArbNzm
126050040224     c                   Eval      I21Lna = ArbLna
126060040224     c                   Eval      I21Nzd = ArbNzd
126070130827     c*****              Eval      I21Ksc = ArbKsc
126080040224     c                   Movel     ArbCtr        I21Ctr
126090040224     c                   Eval      I21Tic = Ar9Tic
126100040224     c                   Eval      I21Imp = Ar9Cas
126110040224     c                   Eval      I21Div = Ar9Vca
126120040224     c                   Eval      I21Ute = Knmus
126130040224     c                   Eval      I21Pgm = nompgm
126140130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
126150130827     c                   if        flgtb1='F'
126160130827     c                   z-add     arbksc        i21ksc
126170130827     c                   else
126180130827     c                   z-add     arbccm        i21ksc
126190130827     c                   if        i21ksc=0
126200130827     c                   z-add     8888          i21ksc
126210130827     c                   movel     arblnp        i21ksc
126220130827     c                   endif
126230130827     c                   endif
126240040224     c                   Call      'TRUL21R'
126250040224     c                   Parm                    Trul21ds
126260040225if  3c                   If        O21Fca <> *Blanks
126270040225     c                   Goto      EndContr
126280040225    3c                   EndIf
126290040225    2c                   EndIf
126300070320     c
126310070320     c* Se FFD immesso dalla partenza, anche se già tolto, posso
126320070320     c*  manutenzionare la spedizione
126330160114    1c                   if        §ar5MDP='S'  and §ar5FFD='S'
126340070320     c                   GoTo      EndContr
126350070911    1c                   EndIf
126360150206     c* se la cons richiesta non è stata immessa dalla partenza e sono in partenza
126370150206     c*  ripristino
126380160114     c                   if        (not *in10 and  §ar5dcrp= 'P')
126390160114     c                             or (*in10 and wfile='P')
126400150206     c                   GoTo      EndContr
126410150206    1c                   EndIf
126420150206     c
126430040225      * in questo caso devo reimpostare la data consegna richiesta come indicato in Fiar5
126440040225      * facendo una variazione di tipo 'CA'
126450040225      * senza farla vedere a video
126460040225     c                   Clear                   dsarbg
126470040225     c                   Eval      d48cvb = 'CA'
126480040225     c                   Eval      weseg = 'E'
126490070911     c*****              Clear                   d48ffr
126500040225     c                   Eval      d48Trc = 'G'
126510040324     c                   Eval      §bgTcr = §Ar5Tcrm
126520040324     c                   Move      §Ar5Dcrm      §bgDcr
126530040324     c                   Move      §Ar5Hcrm      §bgHcr
126540040315     c                   Eval      §bgGc1 = Arbgc1
126550040315     c                   Eval      §bgGc2 = ArbGc2
126560040315     c                   Eval      §bgTc1 = ArbTc1
126570040315     c                   Eval      §bgTc2 = ArbTc2
126580040225     c                   Eval      wCA = 'S'
126590070906     c* Se da eliminare anche la consegna particolare imposto
126600070906     c*  insieme la variazione
126610070911    1c                   if        eliminatcp='1'
126620070906     c                   clear                   §bgtc1
126630070911     c* ripristino la vecchia consegna partic.
126640070911    2c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
126650070911     c                   eval      §bgtc1=aggtcp
126660070911    2c                   endif
126670070911    1c                   endif
126680070911    1c                   if        eliminatcp='2'
126690070906     c                   clear                   §bgtc2
126700070911    2c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
126710070911    3c                   if        §bgtc1=' '
126720070911     c                   eval      §bgtc1=aggtcp
126730070911     c                   else
126740070911     c                   eval      §bgtc2=aggtcp
126750070911    3c                   endif
126760070911    2c                   endif
126770070911    1c                   endif
126780070906     c
126790070906     c* Memorizzo in skiera rielaborazioni
126800070906     c                   if        kk<=10
126810070907     c* se devo eliminare una consegna particolare, memorizzo nella
126820070907     c* stessa variazione
126830070907     c                   if        eliminatcp=' '
126840070907     c                   add       1             kk
126850070907     c                   endif
126860070906     c                   movel     dsarbg        kdati(KK)
126870070906     c                   movel     d48cvb        kcvb(KK)
126880070907     c                   movel     d48trc        ktrc(KK)
126890070911     c                   movel     ' '           kffr(KK)
126900070911     c                   movel     ' '           kf12(KK)
126910070906     c                   endif
126920070906     c
126930040225    1c                   EndIf
126940040224
126950040225     c     EndContr      EndSr
126960040224
126970070307     C*--- AGGIORNAMENTO FILE proposte FIARP00F ----------------------*
126980070307     C     REGISARP      BEGSR
126990070307     C                   MLLZO     1             VIDHCR
127000070307     c     karb          chain     fiarp01l
127010070307     c                   clear                   fiarp000
127020070307     c                   eval      arpaas=arbaas
127030070307     c                   eval      arplnp=arblnp
127040070307     c                   eval      arpnrs=arbnrs
127050070307     c                   eval      arpnsp=arbnsp
127060070307     c                   eval      arptcr=vidtcr
127070070307     c                   eval      arpdcr=comdcr
127080070307     c                   eval      arphcr=vidhcr
127090070308     C*
127100070308     C                   TIME                    W0140            14 0
127110070308     C* UDATE IN AAAAMMGG
127120070308     C                   MOVE      W0140         WDTGIO            8 0
127130070308     C                   Z-ADD     WDTGIO        G02DAT
127140070307     C                   MOVEL     *BLANK        G02ERR
127150070307     C                   CALL      'XSRDA8'
127160070307     C                   PARM                    WLBDAT
127170070307     C                   MOVEL     G02INV        arpdim            8 0
127180070307     c                   time                    arpoim
127190070308     C                   MOVEL     KNMUS         ARPPRU
127200070307     c                   if        %found(fiarp01l)
127210070307     c                   update    fiarp000
127220070307     c                   else
127230070307     c                   write     fiarp000
127240070307     c                   endif
127250070307     c
127260070307     c                   ENDSR
127270941223     C*
127280941223     C*--- CONTROLLI CAMPI MITTENTE ----------------------------------*
127290941223     C     CONTR8        BEGSR
127300941223     C                   SETOFF                                       9028
127310950104     C                   CLEAR                   VIDFAP
127320941223     C*
127330941223     C* CONTROLLO IL CODICE DEL MITTENTE
127340970813     C                   EXSR      CTRCOM
127350970822     C*
127360970822     C* SE PASSO DA CODIFICATO AD 8888 O VICEVERSA SPROTEGGO O PROTEGGO
127370970822     C* CAMPI DELL'INDIRIZZO
127380970822     C     VIDKSC        IFEQ      8888
127390970822     C     MEMKSC        ANDNE     8888
127400970822     C                   MOVE      VIDKSC        MEMKSC
127410970822     C                   SETOFF                                       22
127420970822     C                   SETON                                        90
127430970822     C                   END
127440970822     C*
127450970822     C     VIDKSC        IFNE      8888
127460970822     C     MEMKSC        ANDEQ     8888
127470970822     C                   MOVE      VIDKSC        MEMKSC
127480970822     C                   SETON                                        2290
127490970822     C                   CLEAR                   VIDRSM
127500970822     C                   CLEAR                   VIDINM
127510970822     C                   CLEAR                   VIDCAM
127520970822     C                   CLEAR                   VIDLOM
127530970822     C                   CLEAR                   VIDPRM
127540970822     C                   CLEAR                   VIDNZM
127550970822     C                   CLEAR                   VIDPIM
127560970822     C                   END
127570950113     C**
127580950113     C     *IN90         IFEQ      *ON
127590950113     C  N48              GOTO      ENDCT8
127600950113     C*
127610950113     C* X IL CODICE TARIFFA DO ERRORE SOLO SE NON E' STATO PREMUTO F10
127620950114     C  NKJ
127630950114     CAN 28              GOTO      ENDCT8
127640950113     C                   ENDIF
127650941223     C*
127660020508     C* SE IMMESSO UN CODICE 8888 OBBLIGATORIO l'indirizzo completo
127670020508    1C     VIDKSC        IFEQ      8888
127680970808     C                   MOVEL     VIDRSM        I14RSC
127690970808     C                   CLEAR                   I14RS2
127700970808     C                   MOVEL     VIDINM        I14IND
127710970808     C                   MOVEL     VIDLOM        E14LOC
127720970808     C                   MOVEL     VIDPIM        E14PIP
127730970808     C                   MOVEL     VIDPRM        E14PRV
127740970808     C                   MOVEL     VIDNZM        E14NAR
127750970808     C                   MOVEL     VIDCAM        E14CAP
127760970808     C                   CLEAR                   I14ISO
127770970808     C                   MOVEL     'M'           WCLI              1
127780970808     C                   EXSR      CTRIND
127790941223     C*
127800941223     C* CAMPI CHE POSSO AVER IMPOSTATO
127810970808     C                   MOVEL     E14LOC        VIDLOM
127820970808     C                   MOVEL     E14PIP        VIDPIM
127830970808     C                   MOVEL     E14PRV        VIDPRM
127840970808     C                   MOVEL     E14NAR        VIDNZM
127850970808     C                   MOVEL     E14CAP        VIDCAM
127860950104     C* SALVO IL FLAG ANTEPORTO
127870970808     C                   MOVEL     O95ISO        COMFAP
127880970808     C                   MOVEL     O95ISO        VIDFAP
127890140403     c* salvo pickup 2
127900140403     C                   MOVEL     O95gf2        comgf2
127910941223     C**
127920970808     C* ERRORE
127930970808     C   90              GOTO      ENDCT8
127940950522     C* P.IVA obbligatoria se nazione estera e c'è flag efta in
127950950522     C* tabella nazione
127960950522     C     WDESIE        IFEQ      'E'
127970950522     C     DESEFT        ANDNE     *BLANKS
127980950412     C     VIDPIM        ANDEQ     *BLANKS
127990950412     C                   MOVEL     MSG(67)       VIDMSG
128000950412     C                   SETON                                        284990
128010950412     C                   GOTO      ENDCT8
128020950412     C                   END
128030941223     c*
128040941223   X1C                   ELSE
128050950104     c*
128060950104     C* VEDO SE ESISTE CNIND
128070950104     C     KACO          CHAIN     CNIND000                           32
128080950104     C*
128090950104     C     *IN32         IFEQ      *ON
128100950104     C                   MOVEL     MSG(63)       VIDMSG
128110950104     C                   SETON                                        479028
128120950104     C                   GOTO      ENDCT8
128130950104     C                   ENDIF
128140950104     C*
128150950104     C* CONTROLLO SUL CAP PROVINCIA NAZIONE
128160970423     C     INDSTA        IFEQ      *BLANKS
128170970423     C     INDPRV        ANDEQ     *BLANKS
128180950104     C                   MOVEL     MSG(64)       VIDMSG
128190950104     C                   SETON                                        479028
128200950104     C                   GOTO      ENDCT8
128210950104     C                   ENDIF
128220950104     C* CAP OBBLIGATORIO : CONTROLLO SE CONGRUENZA CON PROVINCIA
128230030612     C                   CLEAR                   Tisi95DS
128240030612     C                   CLEAR                   Fnlv13DS
128250970811     C                   MOVEL     INDSTA        I95NAR
128260970811     C                   MOVEL     INDCAE        I95CAP
128270970811     C                   MOVEL     INDPRV        I95PRV
128280970811     C                   MOVEL     INDCIT        I95LOC
128290970811     C                   MOVEL     '3'           I95TCN
128300970811     C                   MOVEL     ARBDSP        I95DAT
128310970811     C* ERRORE PER AFFIDABILITA' = 0
128320970811     C                   MOVEL     'S'           I13AF0
128330970811     C* CONTROLLO CONGRUENZA CAP/PROVINCIA
128340970811     C                   MOVEL     'S'           I13CNG
128350970811     C* CONTROLLO NON FORZABILE CITTA' CON VIARIO
128360970811     C                   MOVEL     'S'           I13CNV                         CAP CON VIAR
128370020305     c     lnpntw        ifeq      'FED'
128380020305     C                   movel     'S'           i95fi1
128390020305     c                   endif
128400970811     C                   CALL      'FNLV13R'
128410970811     C                   PARM                    KPJBA
128420030612     C                   PARM                    Fnlv13DS
128430030612     C                   PARM                    Tisi95DS
128440950104     C* ERRORE
128450970811     C     O13ERR        IFNE      *BLANKS
128460970811     C                   MOVEL     O13MSG        VIDMSG
128470950104     C                   MOVE      CPDC          VIDMSG
128480950104     C                   SETON                                        904728
128490950104     C                   GOTO      ENDCT8
128500950104    3C                   ENDIF
128510950104     C* SALVO IL FLAG ANTEPORTO
128520970811     C                   MOVEL     O95ISO        COMFAP
128530950104     C* SOLO IN VISUALIZZAZIONE
128540970811     C                   MOVEL     O95ISO        VIDFAP
128550140403     c* salvo pickup 2
128560140403     C                   MOVEL     O95gf2        comgf2
128570950104    2C                   ENDIF
128580941223     C*
128590941227     C     ENDCT8        ENDSR
128600961218     C*
128610941223     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE MT -----------------*
128620941227     C     REGIS8        BEGSR
128630130412     c                   clear                   wpda_ndc
128640130412     c                   clear                   wpda_ddc
128650130412     c                   clear                   wpda_dcm
128660950113     C                   MOVEL     ARBRSM        OLDRSM
128670950113     C                   MOVEL     ARBKSC        OLDKSC
128680950113     C                   MOVEL     ARBCAM        OLDCAM
128690041027     C                   MOVEL     ARBcpi        savCpi
128700950104     C*  GIRO UDATE
128710950104     C                   TIME                    W0140            14 0
128720950104     C* UDATE IN GGMMAAAA
128730950104     C                   MOVE      W0140         WDTGIO            8 0
128740950104     C*
128750950104     C* UDATE IN AAAAMMGG
128760950104     C                   Z-ADD     WDTGIO        G02DAT
128770950104     C                   MOVEL     *BLANK        G02ERR
128780950104     C                   CALL      'XSRDA8'
128790950104     C                   PARM                    WLBDAT
128800950104     C                   MOVEL     G02INV        DATEU             8 0
128810950104     C*
128820941223     C                   MOVEL     KNMUS         ARBPRU
128830950104     C*
128840950104     C* SALVO ARBCCM DELLA BOLLA PERCHE LO USO
128850950104     C                   MOVEL     ARBCCM        W0070             7 0
128860941223     C*
128870941223     C* INVIO PER SEDE
128880170116     C***                MOVEL     'FNARBM0T'    SFILE
128890941223     C* ALLOCO
128900170116     C***                EXSR      ALLOC
128910941223     C* ERRORI IN ALLOCAZIONE
128920170116    1C***  *IN91         IFEQ      *ON
128930170116     C***                GOTO      ENDRG8
128940170116    1C***                ENDIF
128950041025     c
128960041025     c* alloco record bolla in altro file
128970041025     c                   clear                   waltrabolla       1
128980041025     C   10KARB          CHAIN     FNBLP01L                           3091
128990041025     C  n10KARB          CHAIN     FNARB01L                           3091
129000041025     c*
129010041025     c                   if        *in91
129020041025     C                   MOVEL     MSG(4)        VIDMSG
129030170116     c****               exsr      DLCSED
129040070525     C                   SETON                                        2891
129050041025     C                   GOTO      ENDRG8
129060041025     c                   else
129070041025     c                   if        *in30
129080041025     c                   eval      waltrabolla='N'
129090041025     c                   else
129100041025     c                   eval      waltrocpi=arbcpi
129110041025     c                   eval      waltrofvf=arbfvf
129120041025     c                   eval      waltrovlf=arbvlf
129130130411     c  n10              eval      wpda_ndc=arbndc
129140130411     c  n10              eval      wpda_ddc=arbddc
129150130411     c  n10              eval      wpda_dcm=arbdcm
129160041025     C   10KARB          CHAIN     FNARB01L                           30
129170041025     C  N10KARB          CHAIN     FNBLP01L                           30
129180041025     c* Alla fine per non sporcarmi i campi del record, richaino il
129190041025     c*  record originale che sto variando
129200130411     c   10              eval      wpda_ndc=arbndc
129210130411     c   10              eval      wpda_ddc=arbddc
129220130411     c   10              eval      wpda_dcm=arbdcm
129230041025     c                   endif
129240041025     c                   endif
129250941223     C*
129260941223     C                   Z-ADD     DATEU         ARBDTV
129270941223     C                   TIME                    ARBORV
129280941223     C                   MOVEL     VIDCVR        ARBCVB
129290941223     C*
129300060130     C* SE DA STORICIZZARE
129310060130     C**   *IN10         IFEQ      *ON
129320060130    1C**   D66FSA        ANDEQ     'S'
129330060130     C**   *IN10         OREQ      *OFF
129340060130    1C**   D66FSP        ANDEQ     'S'
129350060130    1C     D66FSA        ifeq      'S'
129360941223     C                   Z-ADD     ARBKSC        ARBCCM
129370011108      *
129380011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
129390011108      * è in partenza 'P' o in arrivo 'A'
129400011108     C     *IN10         IFEQ      *ON
129410011108     C                   MOVE      'A'           ARBPRU
129420011108     C                   ENDIF
129430011108     C     *IN10         IFEQ      *OFF
129440011108     C                   MOVE      'P'           ARBPRU
129450011108     C                   ENDIF
129460011108      *
129470941223     C                   WRITE     FNARBM00
129480941223    1C                   END
129490060302     c* storicizzazione motivazione
129500060302     c     d66mtv        ifeq      'S'
129510060302     c                   exsr      sto_mtv
129520060302     c                   endif
129530941223     C*
129540950104     C                   Z-ADD     VIDKSC        ARBKSC
129550950104     C                   MOVEL     VIDKLP        ARBKSC
129560950104     C                   MOVEL     ARBKSC        ARBCCM
129570970613     C     ARBFDN        IFEQ      ' '
129580970613     C                   MOVEL     COMFAP        ARBFAP
129590970613     C                   ELSE
129600970613     C                   MOVEL     'C'           ARBFAP
129610970613     C                   ENDIF
129620970613     C**
129630020508    1C     VIDKSC        IFEQ      8888
129640941223     C                   MOVEL     VIDRSM        ARBRSM
129650941223     C                   MOVEL     VIDINM        ARBINM
129660941223     C                   MOVEL     VIDLOM        ARBLOM
129670941223     C                   MOVEL     VIDCAM        ARBCAM
129680941223     C                   MOVEL     VIDPRM        ARBPRM
129690941223     C                   MOVEL     VIDNZM        ARBNZM
129700941223     C                   CLEAR                   ARBCPI
129710950626     C*
129720950626     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
129730950626     C*     NELLA P.IVA
129740950626     C     VIDISM        IFEQ      '$$'
129750950626     C     VIDCPM        ANDEQ     *BLANKS
129760950626     C                   MOVEL     'PRIVATO'     VIDCPM
129770950626     C                   ENDIF
129780950626     C*
129790950104    2C     VIDISM        IFEQ      *BLANKS
129800941223     C                   MOVEL     VIDCPM        ARBCPI
129810941223     C                   ELSE
129820941223     C                   MOVEL     VIDPIM        ARBCPI
129830950104    2C                   ENDIF
129840950104     C*
129850950104     C                   ELSE
129860950412     C* PRENDO I DATI DA CNACO E CNIND
129870950104     C                   MOVEL     ACORAG        ARBRSM
129880950104     C                   MOVEL     INDVIA        ARBINM
129890950104     C                   MOVEL     INDCIT        ARBLOM
129900950104     C                   MOVEL     INDCAE        ARBCAM
129910950104     C                   MOVEL     INDPRV        ARBPRM
129920950104     C                   MOVEL     INDSTA        ARBNZM
129930950104     C                   MOVEL     INDIVA        ARBCPI
129940941223     C                   ENDIF
129950041027     c* Mi salvo laa new P.iva
129960041027     c                   eval      newcpi=arbcpi
129970041027     c
129980041025     c
129990060223     C* SE ESISTE RECORD P.IVA IN FIAR4  LO AGGIORNO
130000120413     c                   exsr      up_fiar4P
130010941223     C*
130020941223     C* PER SEDE
130030070503     C     D66FFI        IFEQ      'S'
130040950227     C*
130050950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
130060950413    2C     §3ABSD        IFEQ      ' '
130070170116     C****               OPEN      FNARBM0T
130080941227     C                   WRITE     FNARBMTT
130090170116     C***                CLOSE     FNARBM0T
130100950227    2C                   END
130110941223     C* DISALLOCO OGGETTO
130120170116     C***                EXSR      DLCSED
130130950208    1C                   END
130140941223     C*
130150941223     C* AGGIORNO FILE TASSAZIONE PADRONCINI
130160950113     C                   EXSR      AGGPMT
130170950104     C*  REIMPOSTO IL CODICE CLIENTE MITTENTE
130180950104     C                   MOVEL     W0070         ARBCCM
130190950104     C*
130200950113     C* CONTROLLO SE SI TRATTA DI VOLUME AUTOMATICO
130210950113     C* SE HA VARIATO IL MITTENTE
130220950113     C     ARBKSC        IFNE      OLDKSC
130230950113     C*
130240950113     C                   MOVEL     '7T'          COD
130250051214     C                   MOVEL(P)  ARBFVF        KEY
130260950113     C                   EXSR      CTABEL
130270950113     C                   MOVEL     TBLUNI        DS7T
130280950113     C     §7TRVL        IFEQ      'A'
130290950113     C     §7TRVL        OREQ      'P'
130300950114     C                   MOVEL     'S'           WRICVL            1
130310950113     C                   ENDIF
130320950113     C                   ENDIF
130330950113     C*
130340950114     C* SALVO I CAMPI COLLEGATI AL MITTENTE CHE USERO PER STORICIZZARE
130350950114     C                   MOVEL     ARBCTR        MITCTR
130360950114     C                   MOVEL     ARBFVF        MITFVF
130370950114     C                   MOVEL     ARBVLF        MITVLF
130380140403     c*
130390140403     c* aggiorno fiar5 record gen per pickup2
130400140407     c                   if        comgf2<>'01' and arbnzm=*blanks and
130410140407     c                             arbfdn=*blanks
130420140407     c                   eval      wpk2='S'
130430140407     c                   else
130440140407     c                   clear                   wpk2
130450140407     c                   endif
130460140407     c                   if        (wpk2='S' and par5pk2<>'S') or
130470140407     c                              (wpk2<>'S' and par5pk2='S')
130480140403     c                   eval      Wtipoagg='PIK'
130490140403     c                   exsr      regisar5gen
130500140403     c                   endif
130510950114     C*
130520950113     C* SE PREMUTO F10 DEVO FARE IL GIRO CON IL CODICE TARIFFA
130530950104     C     *INKJ         IFEQ      *ON
130540070907     C***                MOVEL     'TP'          D48CVB
130550070907     C***                MOVEL     'E'           WESEG
130560070907     c                   add       1             kk
130570070907     c                   movel     'TP'          kcvb(kk)
130580070911     c                   movel     d48ffr        kffr(kk)
130590070911     c                   movel     w48f12        kf12(kk)
130600070907     c
130610950113     C                   ELSE
130620950114     C*
130630950114     C* SE HO RIPROPOSTO IL CODICE TARIFFA DEL CLIENTE DEVO PASSARE
130640950114     C*  DALLA VIDEATA DELLA TASSAZIONE
130650950114     C                   MOVEL     ARBCTR        W003A             3
130660950114     C     W003A         IFNE      VIDCTR
130670070907     C***                MOVEL     'TP'          D48CVB
130680070907     C***                MOVEL     'E'           WESEG
130690070911     C****               CLEAR                   D48FFR
130700070907     c                   add       1             kk
130710070907     c                   movel     'TP'          kcvb(kk)
130720070911     c                   movel     ' '           kffr(kk)
130730070911     c                   movel     ' '           kf12(kk)
130740950114     C                   ELSE
130750950114     C*
130760950113     C* RICALCOLO VOLUME AUTOMATICO E POI VADO IN VIDEATA DEL VOLUME
130770950114     C     WRICVL        IFEQ      'S'
130780950113     C                   EXSR      CALVOL
130790950114     C                   MOVEL     D18FVF        ARBFVF
130800950114     C                   MOVEL     D18VLF        ARBVLF
130810070911     C***                MOVEL     'I2'          D48CVB
130820070911     C***                MOVEL     'E'           WESEG
130830070911     c***                clear                   w48f12
130840070907     c                   add       1             kk
130850070907     c                   movel     'I2'          kcvb(kk)
130860070911     c                   movel     d48ffr        Kffr(KK)
130870070911     c                   movel     ' '           Kf12(kk)
130880950104     C                   ENDIF
130890950113     C                   ENDIF
130900950114     C                   ENDIF
130910950114     C* MEMORIZZO IL COD TARIFFA SULLA BOLLA, TANTO SE E' CAMBIATO
130920950114     C*  PASSO PER FORZA DALLA VIDEATA DELLA TASSAZIONE PER POTERLO
130930950114     C*  TRASMETTERE
130940950114     C                   MOVEL     VIDCTR        ARBCTR
130950000623     C**
130960120416     c* Controllo e Gestione eventuale record W in fiar4
130970120416     c                   exsr      up_fiar4W
130980041027     c* In partenza memorizzo se 1°bolla franco anche se doppia
130990041027     c* In arrivo   memorizzo se 1°bolla franco e non    doppia
131000041027     c                   if        (*in10 and not *in05) or
131010041027     c                             (not *in10 and not *in06)
131020041027     c                   movel     savcpi        arbcpi
131030041027     c                   endif
131040950114     C*
131050950114     C   10              UPDATE    FNARB000
131060950114     C  N10              UPDATE    FNBLP000
131070041025     c*
131080041025     c* Aggiorno anche l'altra bolla
131090041025     c                   if        waltrabolla=' '
131100041025     c* Se  devo riaggiornare il volume benen altrimenti tengo quello d
131110041025     c*  della bolla
131120041025     c                   if        wricvl<>'S'
131130041025     c                   eval      arbfvf=waltrofvf
131140041027     c                   eval      arbvlf=waltrovlf
131150041025     c                   endif
131160041027     c
131170041027     c* P.IVA  idem come sopra
131180041027     c                   if        (*in10 and *in06) or
131190041027     c                             (not *in10 and *in05)
131200041027     c                   movel     newcpi        arbcpi
131210041027     c                   else
131220041027     c                   movel     waltrocpi     arbcpi
131230041027     c                   endif
131240041025     c
131250041025     C   10              except    arbMblp
131260041025     C  N10              except    arbMarb
131270041025     c                   endif
131280950114     C*
131290950114     C* DISALLOCO I RECORD CHE NON USO
131300991007     C   10              UNLOCK    FIAR601L
131310130411     C*
131320130411     c* Richiamo routine per invio variazione a pda
131330130411     c                   exsr      sr_pda
131340950602     C*
131350941227     C     ENDRG8        ENDSR
131360950113     C*
131370961219     C*--- CONTROLLI RIFERIMENTO MITTENTE ----------------------------*
131380961219     C     CONTR9        BEGSR
131390961219     C                   SETOFF                                       9028
131400061024     C*
131410061024     C* NATURA MERCE SEMPRE OBBLIGATORIA
131420061024     C     VIDNAS        IFEQ      *BLANKS
131430061024     C                   SETON                                        419028
131440061024     C                   MOVEL     MSG(75)       VIDMSG
131450061024     C                   END
131460961219     C* SE DESTINATARIO ESTERO IL RIFERIMENTO E' OBBLIGATORIO
131470061024     c* Solo se c'è attraversamento dogana (come fa FNLS01R)
131480961219     C     WDESIE        IFEQ      'E'
131490970110     C     VIDRMA        ANDEQ     *BLANKS
131500061024     C     deseft        ANDne     *BLANKS
131510961219     C                   SETON                                        409028
131520961219     C                   MOVEL     MSG(74)       VIDMSG
131530961219     C                   END
131540961219     C*
131550961219     C                   ENDSR
131560060316     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE RM -----------------*
131570060316     C     REGIS9        BEGSR
131580130412     c                   clear                   wpda_ndc
131590130412     c                   clear                   wpda_ddc
131600130412     c                   clear                   wpda_dcm
131610060316     C*  GIRO UDATE
131620060316     C                   TIME                    W0140            14 0
131630060316     C* UDATE IN GGMMAAAA
131640060316     C                   MOVE      W0140         WDTGIO            8 0
131650060316     C*
131660060316     C* UDATE IN AAAAMMGG
131670060316     C                   Z-ADD     WDTGIO        G02DAT
131680060316     C                   MOVEL     *BLANK        G02ERR
131690060316     C                   CALL      'XSRDA8'
131700060316     C                   PARM                    WLBDAT
131710060316     C                   MOVEL     G02INV        DATEU             8 0
131720060316     C*
131730060316     C                   MOVEL     KNMUS         ARBPRU
131740060316     C* INVIO PER SEDE
131750170116     C***                MOVEL     'FNARBD0T'    SFILE
131760060316     C* ALLOCO
131770170116     C***                EXSR      ALLOC
131780060316     C* ERRORI IN ALLOCAZIONE
131790170116    1C***  *IN91         IFEQ      *ON
131800170116     C***                GOTO      ENDRG9
131810170116    1C***                ENDIF
131820060316     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
131830060316     c* iornare direttamente il file
131840060316     c                   clear                   waltrabolla       1
131850060316     C   10KARB          CHAIN     FNBLP01L                           3091
131860060316     C  n10KARB          CHAIN     FNARB01L                           3091
131870060316     c*
131880060316     c                   if        *in91
131890060316     C                   MOVEL     MSG(4)        VIDMSG
131900170116     c****               exsr      DLCSED
131910070525     C                   SETON                                        2891
131920060316     C                   GOTO      ENDRG9
131930060316     c                   else
131940060316     c                   if        *in30
131950060316     c                   eval      waltrabolla='N'
131960060316     c                   else
131970130411     c  n10              eval      wpda_ndc=arbndc
131980130411     c  n10              eval      wpda_ddc=arbddc
131990130411     c  n10              eval      wpda_dcm=arbdcm
132000060316     C   10KARB          CHAIN     FNARB01L                           30
132010060316     C  N10KARB          CHAIN     FNBLP01L                           30
132020060316     c* Alla fine per non sporcarmi i campi del record, richaino il
132030060316     c*  record originale che sto variando
132040130411     c   10              eval      wpda_ndc=arbndc
132050130411     c   10              eval      wpda_ddc=arbddc
132060130411     c   10              eval      wpda_dcm=arbdcm
132070060316     c                   endif
132080060316     c                   endif
132090060316     C                   Z-ADD     DATEU         ARBDTV
132100060320     C                   TIME                    ARBORV
132110060410     C                   MOVEL     VIDCVR        ARBCVB
132120060316     c* STORICIZZAZIONE
132130060316    1C     D66FSA        ifeq      'S'
132140060316      *
132150060316      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
132160060316      * è in partenza 'P' o in arrivo 'A'
132170060316     C     *IN10         IFEQ      *ON
132180060316     C                   MOVE      'A'           ARBPRU
132190060316     C                   ENDIF
132200060316     C     *IN10         IFEQ      *OFF
132210060316     C                   MOVE      'P'           ARBPRU
132220060316     C                   ENDIF
132230060316      *
132240060316     c* nella ragione sociale devo mettere rma e nas ma prima devo
132250060316     c* salvare i campi della ragione sociale
132260060316     c                   movel     arbrsd        s_arbrsd
132270060316     c                   movel     arbrd2        s_arbrd2
132280060316     c                   movel(P)  arbrma        arbrsd
132290060316     c                   movel(P)  arbnas        arbrd2
132300060316     c
132310060316     C                   WRITE     FNARBd00
132320060316     c                   movel     s_arbrsd      arbrsd
132330060316     c                   movel     s_arbrd2      arbrd2
132340060316     C                   END
132350060316     c* storicizzazione motivazione
132360060316     c     d66mtv        ifeq      'S'
132370060316     c                   exsr      sto_mtv
132380060316     c                   endif
132390060316     C* Ora invio solo a sede
132400060316    1C     D66FFI        IFEQ      'S'
132410060316     C*
132420060316     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
132430060316    2C     §3ABSD        IFEQ      ' '
132440170116     C***                OPEN      FNARBD0T
132450060316     c                   movel     arbrsd        s_arbrsd
132460060316     c                   movel     arbrd2        s_arbrd2
132470060316     c                   movel(p)  vidrma        arbrsd
132480060316     c                   movel(p)  vidnas        arbrd2
132490060316     C                   WRITE     FNARBDTT
132500060316     c                   movel     s_arbrsd      arbrsd
132510060316     c                   movel     s_arbrd2      s_arbrd2
132520170116     C****               CLOSE     FNARBD0T
132530060316    2C                   END
132540060316     C* DISALLOCO OGGETTO
132550170116     C***                EXSR      DLCSED
132560060316    1C                   END
132570060316     C*
132580060316     C* F6 - AGGIORNAMENTO
132590060316     C                   MOVE      VIDRMA        ARBRMA
132600060316     C                   MOVE      VIDNAS        ARBNAS
132610060316     C   10              UPDATE    FNARB000
132620060316     C  N10              UPDATE    FNBLP000
132630060316     c
132640060316     c                   if        waltrabolla=' '
132650060316     C                   MOVE      VIDRMA        ARBRMA
132660060316     C                   MOVE      VIDNAS        ARBNAS
132670060316     C   10              except    RMblp
132680060316     C  N10              except    RMarb
132690060316     c                   endif
132700130411     C*
132710130411     c* Richiamo routine per invio variazione a pda
132720130411     c                   exsr      sr_pda
132730060316     c     endrg9        endsr
132740030123      *
132750030123      *--- Controllo campi mancato ritiro BANCALI --------------------*
132760030123     c     Contr10       BegSr
132770030123     c                   Eval      *In90 = *Off
132780030127
132790030127      * Il numero dei bancali non ritirati è obbligatorio
132800030127     c                   If        VidRb1 = *Zeros and VidRb2 = *Zeros
132810030127     c                   Eval      VidMsg = Msg(125)
132820030127     c                   Eval      *In60 = *On
132830030127     c                   Eval      *In28 = *On
132840030127     c                   Eval      *In90 = *On
132850030127     c                   GoTo      EndCt10
132860030127     c                   EndIf
132870030124      * Controllo il n. dei bancali non ritirati
132880030124     c                   If        VidRb1 > VidNba
132890030124     c                   Eval      VidMsg = Msg(124)
132900030124     c                   Eval      *In60 = *On
132910030124     c                   Eval      *In28 = *On
132920030124     c                   Eval      *In90 = *On
132930030124     c                   GoTo      EndCt10
132940030124     c                   EndIf
132950030124     c                   If        VidRb2 > VidNb2
132960030124     c                   Eval      VidMsg = Msg(124)
132970030124     c                   Eval      *In61 = *On
132980030124     c                   Eval      *In28 = *On
132990030124     c                   Eval      *In90 = *On
133000030124     c                   GoTo      EndCt10
133010030124     c                   EndIf
133020030123      * Nominativo
133030030123     c                   If        VidNomb = *Blanks
133040030124     c                   Eval      VidMsg = Msg(118)
133050030124     c                   Eval      *In57 = *On
133060030123     c                   Eval      *In28 = *On
133070030123     c                   Eval      *In90 = *On
133080030123     c                   GoTo      EndCt10
133090030123     c                   EndIf
133100030123      * Motivo
133110030123     c                   If        VidMotb = *Blanks
133120030124     c                   Eval      VidMsg = Msg(120)
133130030124     c                   Eval      *In59 = *On
133140030123     c                   Eval      *In28 = *On
133150030123     c                   Eval      *In90 = *On
133160030123     c                   GoTo      EndCt10
133170030123     c                   EndIf
133180030123
133190030123     c     EndCt10       EndSr
133200030123      **
133210030123      *--- AGGIORNAMENTO FILE FIAR5 ----------------------------------*
133220030123     c     Regis10       BegSr
133230130412
133240130412     c                   clear                   wpda_ndc
133250130412     c                   clear                   wpda_ddc
133260130412     c                   clear                   wpda_dcm
133270030123
133280030124     c                   Eval      §Ar5Prub = Knmus
133290060131     c                   Eval      §Ar5Poub = dutPou
133300030124     c                   Move      Dateu         §Ar5Dav
133310030124     c                   Movel     W0140         §Ar5Orv
133320030124     c                   Eval      §Ar5Rb1 = VidRb1
133330030124     c                   Eval      §Ar5Rb2 = VidRb2
133340030124     c                   Eval      §Ar5Nomb = VidNomb
133350030124     c                   Eval      §Ar5Motb = VidMotb
133360030124     c                   Movel(p)  DAr5Ban       Ar5Uni
133370030124     c                   Clear                   Ar5Ft1
133380030124     c                   Clear                   Ar5Ft2
133390030124     c                   Clear                   Ar5Ft3
133400030124     c                   Update    Fiar5000
133410041020     c*
133420041020     c* aggiornamento file fiqdt00f per gestione rientri
133430041020     c                   z-add     arbifp        kfgs
133440041020     c                   z-add     arbndc        w0060             6 0
133450041020     c     kqdt          chain     fiqdt01l
133460041020     c                   if        %found(fiqdt01l)
133470041020     c                   add       vidrb1        qdtnbnbr
133480041020     c                   add       vidrb2        qdtnbnbr
133490041020     c                   except    aggqdt
133500041020     c                   endif
133510060302     c* storicizzazione motivazione
133520060302     c     d66mtv        ifeq      'S'
133530060320     c                   Move      Dateu         arbdtv
133540060320     c                   movel     w0140         arborv
133550060302     c                   exsr      sto_mtv
133560060302     c                   endif
133570130411     C*
133580130412     c                   clear                   waltrabolla       1
133590130412     c                   if        *in10
133600130412     c                   eval      wpda_ndc=arbndc
133610130412     c                   eval      wpda_ddc=arbddc
133620130412     c                   eval      wpda_dcm=arbdcm
133630130412     c                   unlock    fnarb01l
133640130412     c                   else
133650130412     c                   unlock    fnblp01l
133660131120     c                   if        d66pda<>' '
133670130412     C     KARB          CHAIN(n)  FNARB01L                           30
133680130412     c                   if        *in30
133690130412     c                   eval      waltrabolla='N'
133700130412     c                   else
133710130412     c                   eval      wpda_ndc=arbndc
133720130412     c                   eval      wpda_ddc=arbddc
133730130412     c                   eval      wpda_dcm=arbdcm
133740130412     c                   endif
133750130412     c                   endif
133760130412     c                   endif
133770130412     c*
133780130412     c* Richiamo routine per invio variazione a pda
133790130411     c                   exsr      sr_pda
133800030123
133810030123     c                   EndSr
133820061107     C
133830061107     C*--- CONTROLLI FORMATO 11 --------------------------------------*
133840071025     C     CONTR11       BEGSR
133850061107     C                   SETOFF                                       9028
133860151105     c                   clear                   xcfiva1ds
133870151105     c                   clear                   flgiva            1 0
133880151105     c                   move      v11cod        w0040
133890080505     c
133900061107     c* se non previsto inserimento, ERRORE
133910071025    1c                   if        *in76
133920061107     C                   SETON                                        289040
133930071025    2C                   IF        v11MIDE='M'
133940061107     C                   MOVEL     MSG(92)       VIDMSG
133950061107     C                   ELSE
133960061107     C                   MOVEL     MSG(95)       VIDMSG
133970071025    2c                   endif
133980061107     C                   GOTO      ENDCT11
133990071025    1c                   endif
134000061107     c* flag unico se cliente italia
134010071025    1c                   if        v11mide='M'
134020061107     c                   eval      wcliie=wmitie
134030061107     c                   eval      wprv=arbprm
134040151105     c                   eval      wcap=arbcam
134050151105     c                   eval      wloc=arblom
134060061107     c                   else
134070061107     c                   eval      wcliie=wdesie
134080061107     c                   eval      wprv=arbprd
134090151105     c                   eval      wcap=arbcad
134100151105     c                   eval      wloc=arblod
134110071025    1c                   endif
134120071025     c
134130080505     c* Controllo la Partita IVA
134140071025     C* Se P.IVA sprotetta e nazione estera con flag efta la P.IVA è
134150071025     C* obbligatoria
134160071025     c
134170071025    1c                   if        v11pid=*blanks
134180071025     c
134190071025    2c                   if        wdesie='E' and deseft<>*blanks
134200071025     C                   SETON                                        284190
134210071025     C                   MOVEL     MSG(67)       VIDMSG
134220071025     C                   GOTO      ENDCT11
134230120507     c****               else
134240071031     c* solo se non richiamato nascosto
134250120507    3c*****              if        d48ffr='V' or d48ffr='S'
134260120507    4c****               if        wcliie='I' and   forzapid=' '
134270120507     c*****                        and dateu>=§vpopiv
134280120507     C******             SETON                                        289041
134290120507     C*****              MOVEL     MSG(57)       VIDMSG
134300120507     c******             movel     '1'           forzapid
134310120507     C********           GOTO      ENDCT11
134320120507    4c*********          endif
134330120507    3c********           endif
134340071031     c
134350071025    2c                   endif
134360071025    1C                   ENDif
134370071025     C*
134380151105    1c***                if        v11pid<>*blanks
134390071025     c* Se cambiata pulisco la forzatura
134400071025    2c                   if        v11pid<>sav11pid
134410071025     c                   clear                   forzapid2
134420071025     c                   movel     v11pid        sav11pid
134430071025    2c                   endif
134440071025     c
134450151105     c                   clear                   xcfiva1ds
134460071025     c                   clear                   xcfivamod
134470151105     c                   eval      xcfivafori= 'S'
134480100120     c                   eval      xcfivamod = 'P'
134490071025     c                   eval      xcfivapar = v11pid
134500071025     c                   eval      xcfivanar = wnar
134510071025     c                   eval      xcfivaprv = wprv
134520151105     c                   eval      xcfivacap = wcap
134530151105     c                   eval      xcfivaloc = wloc
134540151105     c                   if        w0040=8888 or w0040=9999
134550151105     c                   eval      xcfivap88='8'
134560151105     c                   endif
134570151105     c                   call      'XCFIVAR1'
134580151105     c                   parm                    xcfiva1ds
134590071025     C*
134600071025      * --> errata forzabile
134610100120    2c                   If        xcfivaflg = -9 and forzaPID2 = ' '
134620071025     c                   seton                                        289041
134630071025     c                   Eval      vidmsg = xcfivamsg
134640071025     c                   Eval      vidmsg = %trim(vidmsg) + ' Enter x forzare'
134650100120     c                   eval      forzaPID2= '1'
134660071025     C                   GOTO      ENDCT11
134670071025    2c                   EndIf
134680071025      * --> errore
134690071025    2c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
134700071025     c                   seton                                        289041
134710071025     c                   eval      vidmsg = xcfivamsg
134720071025     C                   GOTO      ENDCT11
134730071025    2c                   EndIf
134740151105
134750151105     c                   eval      flgiva=xcfivaflg
134760071025     c
134770071025      * se non impostato devo riportare il codice iso della partita iva che
134780071025      * mi viene passato dal pgm di controllo
134790151105    2c                   if        v11pid<>*blanks and v11isd <> ivaeu
134800071025     c                   eval      v11isd = ivaeu
134810071025     c                   seton                                          9041
134820071025     C                   GOTO      ENDCT11
134830071025    2c                   endif
134840151105    1c****               endif
134850080505     c
134860080505      * Devo richiamare il nuovo driver fatto per controllare il codice
134870080505      * fiscale
134880151105     c                   clear                   xcfiva1ds
134890080505     c                   clear                   xcfivamod
134900080505     c                   eval      xcfivapar = v11cdfis
134910080505     c                   eval      xcfivanar = wnar
134920080505     c                   eval      xcfivaprv = wprv
134930151105     c                   eval      xcfivacap = wcap
134940151105     c                   eval      xcfivaloc = wloc
134950151105     c                   eval      xcfivapiva= v11pid
134960151105     c                   if        w0040=8888 or w0040=9999
134970151105     c                   eval      xcfivap88='8'
134980151105     c                   endif
134990151105     c                   call      'XCFIVAR1'
135000151105     c                   parm                    xcfiva1ds
135010080505     c*
135020080505    1c                   if        xcfivaflg < *zeros
135030080505     C                   SETON                                        289040
135040080505     C                   MOVEL     xcfivamsg     VIDMSG
135050080505     C                   GOTO      ENDCT11
135060080505    1c                   endif
135070080505     c
135080080505     c* Obbligatorio se cliente italia partita iva o codice fiscale
135090080505     c* solo se non richiamato nascosto
135100120507    1c***                if        d48ffr='V' or d48ffr='S'
135110120507    2c****               if        wcliie='I' and   v11cdfis=*blanks
135120120507     c****                         and forzacdf=' ' and xcfivaflg<>9
135130120507     C****               SETON                                        289040
135140120507     C*****              MOVEL     MSG(23)       VIDMSG
135150120507     c****               movel     '1'           forzacdf
135160120507     C*****              GOTO      ENDCT11
135170120507    2c*****              endif
135180120507    1c******             endif
135190080505     c
135200080505     c* Se entrambe vuote errore
135210080505    1c                   if        v11cdfis=*blanks and v11pid=*blanks
135220080505     C                   SETON                                        284090
135230080505     C                   MOVEL     MSG(145)      VIDMSG
135240080505     C                   GOTO      ENDCT11
135250080505    1c                   endif
135260120508     c* Se fattura immediata errore se partita iva '$$'
135270120508     c                   if        %scan('$$':v11pid)>0  and wcliie='I'
135280120508     c                   move      v11cod        w0040
135290120508     c                   if        w0040=8888 or w0040=9999
135300120507     C                   SETON                                        284190
135310120507     C                   MOVEL     MSG(149)      VIDMSG
135320120507     C                   GOTO      ENDCT11
135330120508     c                   endif
135340120507     c                   endif
135350120508     c* solo se non richiamato nascosto
135360130315    3c                   if        d48ffr='V' or d48ffr='S'
135370120507     c* Se codice fiscale lungo 11 si richiede la partita IVA  - Errore forzabile
135380120507     c                   if        v11cdfis<>*blanks and v11pid=*blanks and
135390120507     c                             %subst(v11cdfis:12:1)=' ' and wcliie='I'
135400120507     c                             and forzapid=' '
135410120507     C                   SETON                                        289041
135420120507     C                   MOVEL     MSG(57)       VIDMSG
135430120507     c                   movel     '1'           forzapid
135440120507     C                   GOTO      ENDCT11
135450120507     c                   endif
135460130315     c                   endif
135470061107     C*
135480061107     C     ENDCT11       ENDSR
135490130927     c
135500130927     C*--- CONTROLLI FORMATO 12 --------------------------------------*
135510130927     C     CONTR12       BEGSR
135520130927     C                   SETOFF                                       9028
135530130930     c* controllo indirizzo email
135540131001     c                   if        *in86  and videml<>*blanks
135550130930     c                   Clear                   dsemail
135560130930     c                   Eval      §emlindi = videml
135570130930     c                   Call      'XEMAIL'
135580130930     c                   Parm                    dsemail
135590130930if  3c                   If        §emlerro <> '0'
135600130930     C                   SETON                                        289041
135610130930     C                   MOVEL     MSG(150)      VIDMSG
135620130930     C                   GOTO      ENDCT12
135630130930     c                   endif
135640130930
135650130930     c                   endif
135660130930     c
135670130927     C     ENDCT12       ENDSR
135680061107     C*
135690061107     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE FI -----------------*
135700061107     C     REGIS11       BEGSR
135710130412
135720130412     c                   clear                   wpda_ndc
135730130412     c                   clear                   wpda_ddc
135740130412     c                   clear                   wpda_dcm
135750061107     C*  GIRO UDATE
135760061107     C                   TIME                    W0140            14 0
135770061107     C* UDATE IN GGMMAAAA
135780061107     C                   MOVE      W0140         WDTGIO            8 0
135790061107     C*
135800061107     C* UDATE IN AAAAMMGG
135810061107     C                   Z-ADD     WDTGIO        G02DAT
135820061107     C                   MOVEL     *BLANK        G02ERR
135830061107     C                   CALL      'XSRDA8'
135840061107     C                   PARM                    WLBDAT
135850061107     C                   MOVEL     G02INV        DATEU             8 0
135860061107     C                   MOVEL     KNMUS         ARBPRU
135870071025     c*
135880071025     C     V11ISD        IFEQ      '$$'
135890071025     C     V11CPD        ANDEQ     *BLANKS
135900071025     C                   MOVEL     'PRIVATO'     V11CPD
135910071025     C                   ENDIF
135920071025     c
135930071025     c* Salvo a parte la partita iva presente in bolla
135940071025     c                   movel     arbcpi        bollacpi
135950061107     C*
135960061129     c* Se la bolla non è trasmessa non devo allocare il file e non devo
135970061129     c*    creare la variazione
135980170116     c***                if        bollaft2<>' '
135990061107     C* INVIO PER SEDE
136000170116     C***                MOVEL     'FNARBD0T'    SFILE
136010061107     C* ALLOCO
136020170116     C***                EXSR      ALLOC
136030061107     C* ERRORI IN ALLOCAZIONE
136040170116    1C**   *IN91         IFEQ      *ON
136050170116     C***                GOTO      ENDRG11
136060170116    1C***                ENDIF
136070170116    1C***                ENDIF
136080061107     c
136090061107     c* Alloco il record fiar4 del codice fiscale                      agg
136100061107     C                   MOVEL     'Q'           WTRC
136110061107     C     KARB2         CHAIN     FiAR4000                           3091
136120061107     c*
136130061107     c                   if        *in91
136140061107     C                   MOVEL     MSG(4)        VIDMSG
136150170116     c****               exsr      DLCSED
136160070525     C                   SETON                                        2891
136170061107     C                   GOTO      ENDRG11
136180061107     c                   endif
136190061107     c
136200061107     c                   if        *in30
136210061107     c                   eval      esiste='N'
136220061116     c                   clear                   ar4not
136230061107     c                   else
136240061107     c                   eval      esiste='S'
136250061107     c                   endif
136260071026     c*
136270071026     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
136280071026     c* iornare direttamente il file solo se non si tratta di bolla doppia
136290071026     c                   clear                   waltrabolla       1
136300130412     c                   setoff                                       3091
136310130412    1c                   if        §3atb2=*blanks and aggiobol<>'N'
136320071026     C   10KARB          CHAIN     FNBLP01L                           3091
136330071026     C  n10KARB          CHAIN     FNARB01L                           3091
136340130412     c                   else
136350131120     c                   if        d66pda<>' '
136360130412     c                   if        not *in10
136370130412     C     KARB          CHAIN(n)  FNARB01L                           30
136380130412     c                   endif
136390130412     c                   eval      wpda_ndc=arbndc
136400130412     c                   eval      wpda_ddc=arbddc
136410130412     c                   eval      wpda_dcm=arbdcm
136420130412     c                   endif
136430130412    1c                   endif
136440071026     c*
136450071026    2c                   if        *in91
136460071026     C                   MOVEL     MSG(4)        VIDMSG
136470071026     c*
136480170116     c****               exsr      DLCSED
136490071026     c                   unlock    fiar401l
136500071026     c
136510071026     C                   SETON                                        2891
136520071026     C                   GOTO      ENDRG11
136530071026   x2c                   else
136540071026    3c                   if        *in30
136550071026     c                   eval      waltrabolla='N'
136560071026   x3c                   else
136570130412    4c                   if        §3atb2=*blanks and aggiobol<>'N'
136580130411     c  n10              eval      wpda_ndc=arbndc
136590130411     c  n10              eval      wpda_ddc=arbddc
136600130411     c  n10              eval      wpda_dcm=arbdcm
136610071026     C   10KARB          CHAIN     FNARB01L                           30
136620071026     C  N10KARB          CHAIN     FNBLP01L                           30
136630071026     c* Alla fine per non sporcarmi i campi del record, richaino il
136640071026     c*  record originale che sto variando
136650130411     c   10              eval      wpda_ndc=arbndc
136660130411     c   10              eval      wpda_ddc=arbddc
136670130411     c   10              eval      wpda_dcm=arbdcm
136680130412    4c                   endif
136690071026    3c                   endif
136700071026    2c                   endif
136710130412    1c****               endif
136720061107     C*
136730061107     C                   Z-ADD     DATEU         ARBDTV
136740061107     C                   TIME                    ARBORV
136750061107     C                   MOVEL     VIDCVR        ARBCVB
136760061107     c* Memorizzo se si tratta del mittente o destinatario
136770061107     c                   movel     v11mide       arbrd2
136780061107     c
136790061107     c* clearo gli altri campi del file che non mi interessano
136800061107     c                   clear                   arbrsd
136810061107     c                   clear                   arbind
136820061107     c                   clear                   arbcad
136830061107     c                   clear                   arblod
136840061107     c                   clear                   arbprd
136850061107     c                   clear                   arbnzd
136860061107     c                   clear                   arbfin
136870061107     c                   clear                   arbpkb
136880061107     c                   clear                   arbpkf
136890061107     c                   clear                   arbvlb
136900061107     c                   clear                   arbvlf
136910061107     c                   clear                   arbfvb
136920061107     c                   clear                   arbfvf
136930061107     c                   clear                   arbffd
136940061107     c                   clear                   arbfst
136950061107     C*
136960061107     C* SE DA STORICIZZARE
136970061107    1C     D66FSA        ifeq      'S'
136980061107      *
136990061107      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
137000061107      * è in partenza 'P' o in arrivo 'A'
137010061107     C     *IN10         IFEQ      *ON
137020061107     C                   MOVE      'A'           ARBPRU
137030061107     C                   ENDIF
137040061107     C     *IN10         IFEQ      *OFF
137050061107     C                   MOVE      'P'           ARBPRU
137060061107     C                   ENDIF
137070061107     c                   if        v11mide='M'
137080061107     c                   movel     ar4not        arbcpi
137090061107     c                   else
137100061107     c                   move      ar4not        arbcpi
137110061107     c                   endif
137120071025     c* Partita iva
137130071025     c                   movel     bollacpi      arbrsd
137140061107      *
137150061107     C                   WRITE     FNARBD00
137160061107    1C                   ENDIF
137170061107    c
137180061107     c* storicizzazione motivazione
137190061107     c     d66mtv        ifeq      'S'
137200061107     c                   exsr      sto_mtv
137210061107     c                   endif
137220071025     c* Aggiorno campo codice fiscale
137230061107     c                   movel     v11cdfis      arbcpi
137240071025     c* Aggiorno campo per la partita iva
137250071025     C                   MOVEL     V11PID        ARBrsd
137260071025     C* LA P.IVA SE HA CODICE ISO VIENE MESSO A SIN ALTRIMENTI SUBITO
137270071025     C*  IL CODICE DELLA P.IVA
137280071025     C     V11CPD        IFNE      *BLANKS
137290071025     C     V11ISD        ANDEQ     *BLANKS
137300071025     C                   CLEAR                   ARBrsd
137310071025     C                   MOVEL     V11CPD        ARBrsd
137320071025     C                   ENDIF
137330130402     c* Se Cd.Fisc. lungo 11 lo metto anche nella P.Iva se vuota
137340130402     c                   if        v11pid=*blanks and %subst(v11cdfis:12:1)=' '
137350130402     c                   movel     v11cdfis      arbrsd
137360130402     c                   endif
137370071025     c
137380071025     C                   MOVEL     'P'           WTRC
137390071025     C     KARB2         CHAIN     FIAR401L                           30
137400071025    1C     *IN30         IFEQ      *OFF
137410071025    2c                   if        v11mide='M'
137420071025     c                   movel     ar4not        wcpifile
137430071025   x2c                   else
137440071025     c                   move      ar4not        wcpifile
137450071025    2c                   endif
137460071025    2c                   if        wcpifile <> arbrsd
137470071025     c                   movel     'T'           ar4ftr
137480071025     c                   z-add     dateu         ar4dtr
137490071025     c                   z-add     dateu         ar4duv
137500071025    3c                   if        v11mide='M'
137510071025     c                   eval      %subst(ar4not:1:16)=%subst(arbrsd:1 :16)
137520071025     c                   else
137530071025     c                   eval      %subst(ar4not:20:16)=%subst(arbrsd:1:16)
137540071025    3c                   endif
137550071025     c
137560071025     C                   UPDATE    FIAR4000
137570071025   x2c                   else
137580071025     c                   unlock    fiar401l
137590071025    2c                   endif
137600071025    1c                   endif
137610061129     c
137620071025    0c                   if        bollaft2<>' '
137630061107     C* Ora invio solo a sede
137640061107     C     D66FFI        IFEQ      'S'
137650061107     C*
137660061107     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
137670061107    2C     §3ABSD        IFEQ      ' '
137680170116     C***                OPEN      FNARBD0T
137690061107     C                   WRITE     FNARBDTT
137700170116     C***                CLOSE     FNARBD0T
137710061107    2C                   END
137720061107     C* DISALLOCO OGGETTO
137730170116     C**                 EXSR      DLCSED
137740061107    1C                   END
137750071025    0C                   END
137760061107     C*
137770061107     C* AGGIORNO  file: 30 on write del record
137780071025     C                   MOVEL     'Q'           WTRC
137790071025     C     KARB2         CHAIN     FiAR4000                           3091
137800061107     c                   if        Esiste='N'
137810061107     c                   movel     arbaas        ar4aas
137820061107     c                   movel     arblnp        ar4lnp
137830061107     c                   movel     arbnrs        ar4nrs
137840061107     c                   movel     arbnsp        ar4nsp
137850061107     c                   movel     'Q'           ar4trc
137860061107     c                   clear                   ar4not
137870061107     c                   clear                   ar4flo
137880061107     c                   endif
137890061107     c                   if        v11mide='M'
137900061107     c                   movel     v11cdfis      ar4not
137910061107     c                   else
137920061107     c                   move      v11cdfis      ar4not
137930061107     c                   endif
137940061107     c                   movel     'T'           ar4ftr
137950061107     c                   z-add     dateu         ar4dtr
137960061107     c                   z-add     dateu         ar4duv
137970061107     c
137980061116     c                   if        Esiste='N'  and ar4not<>*blanks
137990061107     c                   write     fiar4000
138000061116     c                   endif
138010061116     c                   if        Esiste='S'  and ar4not<>*blanks
138020061107     c                   update    fiar4000
138030061107     c                   endif
138040061116     c                   if        Esiste='S'  and ar4not=*blanks
138050061116     c                   delete    fiar4000
138060061116     c                   endif
138070071025     c
138080071026     c* Aggiorno la bolla per la partita iva solo se non richiamamto da I0
138090071026     c*  per cui ho già aggiornato la partita iova sulla bolla
138100071026     c                   if        Aggiobol<>'N'
138110071025     c                   movel     arbrsd        arbcpi
138120071025     c* aggiornamento mirato per non sporcare degli altri campi
138130071025     c  n10              except    SolocpiP
138140071025     c   10              except    SolocpiA
138150071026     c
138160071026     c* Se 'c'e' l'altra ed è bolla singola la aggiorno
138170071026     c                   if        §3atb2=*blanks and waltrabolla=' '
138180071026     c  n10              except    SolocpiA
138190071026     c   10              except    SolocpiP
138200071026     c                   endif
138210071026     c                   endif
138220061107     C*
138230061107     C* DISALLOCO  I FILES CHE NON USO
138240061107     C   10              UNLOCK    FiAR901L
138250071025     c** 10              unlock    fnarb01l
138260071025     c**N10              unlock    fnblp01l
138270130411     C*
138280130411     c* Richiamo routine per invio variazione a pda
138290130411     c                   exsr      sr_pda
138300061107     C*
138310061107     C     ENDRG11       ENDSR
138320130927     C*
138330130930     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE Nx -----------------*
138340130927     C     REGIS12       BEGSR
138350130927     c
138360130927     C*  GIRO UDATE
138370130927     C                   TIME                    W0140            14 0
138380130927     C* UDATE IN GGMMAAAA
138390130927     C                   MOVE      W0140         WDTGIO            8 0
138400130927     C*
138410130927     C* UDATE IN AAAAMMGG
138420130927     C                   Z-ADD     WDTGIO        G02DAT
138430130927     C                   MOVEL     *BLANK        G02ERR
138440130927     C                   CALL      'XSRDA8'
138450130927     C                   PARM                    WLBDAT
138460130927     C                   MOVEL     G02INV        DATEU             8 0
138470130927     C                   MOVEL     KNMUS         ARBPRU
138480130927     C                   Z-ADD     DATEU         ARBDTV
138490130927     C                   TIME                    ARBORV
138500130927     C                   MOVEL     VIDCVR        ARBCVB
138510131001      *
138520131001      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
138530131001      * è in partenza 'P' o in arrivo 'A'
138540131001     C     *IN10         IFEQ      *ON
138550131001     C                   MOVE      'A'           ARBPRU
138560131001     C                   ENDIF
138570131001     C     *IN10         IFEQ      *OFF
138580131001     C                   MOVE      'P'           ARBPRU
138590131001     C                   ENDIF
138600130927     C*
138610130927     C* SE DA STORICIZZARE
138620130927    1C     D66FSA        ifeq      'S'
138630130930
138640131001     c                   if        *in84 and
138650131001     c                                  (vidref<>par5ref or vidteld<>par5teld)
138660130930     c* Referente e consegna
138670130927     c                   clear                   darbn_ref
138680130927     c                   eval      §arbnref = par5ref
138690130927     c                   eval      §arbnteld= par5teld
138700130927     c                   eval      arbuni70 = darbn_ref
138710130927     c                   eval      arbtrd='REF'
138720130927     c                   write     FNARBN00
138730131001     c                   endif
138740131001     c
138750131001     c                   if        *in83 and emd_§ar5tel<>vidtsms
138760130930     c                   clear                   darbn_sms
138770130930     c                   eval      §arbncel = emd_§ar5tel
138780130930     c                   eval      arbuni70 = darbn_sms
138790130927     c                   eval      arbtrd='SMS'
138800130927     c                   write     FNARBN00
138810131001     c                   endif
138820131001     c
138830131001     c                   if        *in86 and emd_§ar5eml<>videml
138840130927     c                   clear                   darbn_eml
138850130930     c                   eval      §arbneml=emd_§ar5eml
138860130927     c                   eval      arbuni70 = darbn_eml
138870130927     c                   eval      arbtrd='EML'
138880130927     c                   write     FNARBN00
138890131001     c                   endif
138900131001
138910131001     c                   if        *in81 and (vidnt1<>v1cnt1 or vidnt2<>v1cnt2)
138920130930     c                   clear                   darbn_not
138930130930     c                   eval      §arbnnot1=v1cnt1
138940130930     c                   eval      §arbnnot2=v1cnt2
138950130930     c                   eval      arbuni70 = darbn_not
138960130930     c                   eval      arbtrd='NOT'
138970130930     c                   write     FNARBN00
138980130930     c                   endif
138990130930
139000130930     c                   endif
139010130930     c
139020131001     c                   if        *in84
139030130930     c                   exsr      AggioREF
139040131001     c                   endif
139050131001     c
139060130930      * EMAIL e cell sms
139070131001     c                   if        *in83 or *in86
139080130930     c                   Eval      kAr5Trd = 'EMD'
139090130930     c     kAr5          Chain     Fiar501l
139100130930      * Aggiorno Fiar5
139110130930if  2c                   If        %Found(Fiar501l)
139120130930     c                   movel     ar5uni        DAr5EMD
139130131001     c   86              eval      emd_§ar5eml=videml
139140131001     c   83              eval      emd_§ar5tel=vidtsms
139150130930     c                   Movel(p)  DAr5emd       Ar5Uni
139160130930     c                   Update    Fiar5000
139170130930      * Scrivo Fiar5
139180130930   x2c                   Else
139190130930      * se immessi i dati
139200130930      * non mi preoccupo per il fermo deposito: il fiar5 record GEN viene sempre creato
139210130930      * in immissione bolle. Non esiste più il caso di fiar5 record GEN inesistente in fase
139220130930      * di manutenzione bolle
139230130930if  3c                   If        Videml <> *Blanks or Vidtsms <> *Blanks
139240130930     c                   Clear                   Fiar5000
139250130930     c                   Eval      Ar5Aas = ArbAas
139260130930     c                   Eval      Ar5Lnp = ArbLnp
139270130930     c                   Eval      Ar5Nrs = ArbNrs
139280130930     c                   Eval      Ar5Nsp = ArbNsp
139290130930     c                   Eval      Ar5Trd = 'EMD'
139300130930     c                   Move      Dateu         Ar5Dac
139310130930     c                   Movel     W0140         Ar5Orc
139320130930     c                   Clear                   Dar5emd
139330131001     c   86              eval      emd_§ar5eml=videml
139340131001     c   83              eval      emd_§ar5tel=vidtsms
139350130930     c                   Movel(p)  DAr5emd       Ar5Uni
139360160426     c                   Eval      Ar5Ft1 = 'T'
139370160426     c                   Eval      Ar5Dt1 = *All'9'
139380160426     c                   Eval      Ar5Ft2 = 'T'
139390160426     c                   Eval      Ar5Dt2 = *All'9'
139400130930     c                   Write     Fiar5000
139410130930    3c                   EndIf
139420130930    2c                   EndIf
139430130930     c
139440131001   x1c                   endif
139450130930
139460131001     c                   if        *in81
139470130930     c* aggiorno le NOTE
139480130930     c* aggiorno/deleto trk 8 e 9 se variati
139490130930     c                   clear                   fnlv19ds
139500130930     C                   MOVEL     'T'           D19ftr
139510130930     C                   MOVEL     arbAAS        D19AAS
139520130930     C                   MOVEL     arbLNP        D19LNP
139530130930     C                   MOVEL     arbNRS        D19NRS
139540130930     C                   MOVEL     arbNSP        D19NSP
139550130930     C                   MOVEL     vidnt1        D19NT1
139560130930     C                   MOVEL     vidnt2        D19NT2
139570130930    2C     vidnt1        IFNE      *BLANKS
139580130930     C     vidnt2        ORNE      *BLANKS
139590130930     C                   MOVEL     '8'           D19TRC
139600130930     C                   CALL      'FNLV19R'
139610130930     C                   PARM                    fnlv19ds
139620130930    2C                   ENDIF
139630130930     c* Se il pgm mi restituisce la seconda riga vuota la deleto perchè
139640130930     c* prima poteva esistere
139650130930    2C     D19ERR        IFEQ      *BLANKS
139660130930     C     D19NT1        ANDNE     *BLANKS
139670130930     C     D19NT2        ANDEQ     *BLANKS
139680130930     C                   MOVEL     '9'           D19TRC
139690130930     C                   CLEAR                   D19NT1
139700130930     C                   CLEAR                   D19NT2
139710130930     C                   CALL      'FNLV19R'
139720130930     C                   PARM                    fnlv19ds
139730130930   x2C                   else
139740130930     C* Se tornato codice di err richiamo il pgm 2 volte:  una per nota
139750130930    3C     D19ERR        IFNE      *BLANKS
139760130930     C     vidnt1        oreq      *BLANKS
139770130930     C     vidnt2        ANDEQ     *BLANKS
139780130930     C* Nota 1
139790130930     C                   MOVEL     '8'           D19TRC
139800130930     C                   MOVEL     vidnt1        D19NT1
139810130930     C                   CLEAR                   D19NT2
139820130930     C                   CALL      'FNLV19R'
139830130930     C                   PARM                    fnlv19ds
139840130930     C* Nota 2
139850130930     C                   MOVEL     '9'           D19TRC
139860130930     C                   MOVEL     vidnt2        D19NT1
139870130930     C                   CLEAR                   D19NT2
139880130930     C                   CALL      'FNLV19R'
139890130930     C                   PARM                    fnlv19ds
139900130930    3C                   ENDIF
139910130930    2C                   ENDIF
139920130930
139930130930    1c                   EndIf
139940130927     c*
139950131008     c   10              unlock    fnarb01l
139960131008     c  n10              unlock    fnblp01l
139970130927     C     ENDRG12       ENDSR
139980961219     C*
139990950113     C*--- CALCOLO IL VOLUME AUTOMATICO ------------------------------*
140000950113     C     CALVOL        BEGSR
140010030612     C                   CLEAR                   Fnlv18DS
140020950113     C   10              MOVEL     'A'           D18TBO
140030950113     C  N10              MOVEL     'P'           D18TBO
140040950113     C                   Z-ADD     ARBKSC        D18KSC
140050950113     C                   Z-ADD     ARBCTR        D18CTR
140060950113     C                   MOVEL     §3ATB1        D18TBL
140070950113     C                   Z-ADD     ARBAAS        D18AAS
140080950113     C                   Z-ADD     ARBMGS        D18MGS
140090950113     C                   Z-ADD     ARBNCL        D18NCL
140100950113     C                   Z-ADD     ARBPKF        D18PKF
140110020311     c                   movel     arbtsp        d18tsp
140120020311     c                   z-add     arblna        d18lna
140130020311     c                   z-add     arblnp        d18lnp
140140950113     C                   CALL      'FNLV18R'
140150030612     C                   PARM                    Fnlv18DS
140160950113     C*
140170950113     C                   ENDSR
140180911212     C*
140190970808     C* CONTROLLO INDIRIZZO COMPLETO MITT/DESTIN ---------------------*
140200970808     C     CTRIND        BEGSR
140210150203     c                   clear                   normlod
140220970808     C* LE DS SONO DA CLEARARE PRIMA ALTRIMENTI DANNO PROBLEMI IN CTRER
140230030612     C                   CLEAR                   Fnlv13DS
140240030612     C                   CLEAR                   Tisi95DS
140250970808     C*
140260100120     c                   movel     'S'           i14forzait
140270970808     C                   MOVEL     ARBDSP        I14DRI
140280020305     c                   movel     'S'           i14cnz
140290970808     C                   CALL      'FNLV14R'
140300970808     C                   PARM                    KPJBA
140310030612     C                   PARM                    Fnlv14DS
140320970808     C                   MOVEL     O14ERR        WERR              1
140330970808     C                   MOVEL     O14MSG        VIDMSG
140340970808     C                   EXSR      CTRERR
140350970808     C*
140360970808     C     *IN90         IFEQ      *ON
140370970808     C                   GOTO      ENDIND
140380970808     C                   ENDIF
140390970808     C*************
140400970808     C* CONTROLLO CAP
140410970808     C                   MOVEL     '7'           I95TCN
140420970808     C                   MOVEL     E14CAP        I95CAP
140430970808     C                   MOVEL     E14LOC        I95LOC
140440970808     C                   MOVEL     E14PRV        I95PRV
140450970808     C                   MOVEL     E14NAR        I95NAR
140460000623     C                   MOVEL     ARBDSP        I95DAT
140470020318     C     LNPNTW        IFEQ      'PPT'
140480000623     C                   MOVEL     ARBLNP        I95TFP
140490000623     C                   ELSE
140500980602     C                   MOVEL     ARBTFP        I95TFP
140510000623     C                   ENDIF
140520971230     C                   Z-ADD     ARBPKF        I95LKG
140530971230     C                   Z-ADD     ARBVLF        I95LMC
140540971230     C                   MOVEL     ARBFFD        I95FFD
140550971201     C                   MOVEL     FLGTB1        I95TPO
140560971230     C                   MOVEL     ARBTSP        I95TSP
140570980602     C                   MOVEL     'S'           I95FRE
140580971201     C     §3AFCA        IFNE      0
140590971201     C                   MOVEL     'S'           I95FCA
140600971201     C                   ENDIF
140610970808     C                   MOVEL     'S'           I13AF0
140620140902     c                   if        d48ffr<>'E'
140630970808     C                   MOVEL     'S'           I13CNV
140640140807     c                   endif
140650970808     C                   MOVEL     'S'           I13SZV
140660141119     c* in caso di chiamata cieca non è da impostare: non si deve aprire la int località
140670141119     c                   if        d48ffr<>'E'
140680970808     C                   MOVEL     'S'           I13AF1
140690141119     c                   else
140700141119     C                   MOVEL     ' '           I13AF1
140710141119     c                   endif
140720970808     C                   MOVEL     'S'           I13SZ2
140730970808     C*
140740970808     C* SE CAMBIATO CAP PULISCO CAMPI PER FORZATURE
140750970808     C* ** DESTINATARIO **
140760970808     C     WCLI          IFEQ      'D'
140770970808    2C     E14CAP        IFNE      VARCAD
140780970808     C                   CLEAR                   WZV
140790970808     C                   CLEAR                   WZ1
140800970808     C                   CLEAR                   WZ2
140810970808     C                   MOVEL     E14CAP        VARCAD
140820970808    2C                   ENDIF
140830970808     C* MODIFICA LA LOCALITA'
140840970808    2C     E14LOC        IFNE      VARLOD
140850970808     C                   MOVEL     E14LOC        VARLOD
140860970808     C                   CLEAR                   WZ1
140870970808     C                   CLEAR                   WZ2
140880970808    2C                   ENDIF
140890970808     C                   MOVEL     WZV           E13FZV
140900970808     C                   MOVEL     WZ1           E13FZ1
140910970808     C                   MOVEL     WZ2           E13FZ2
140920020305     c     lnantw        ifeq      'FED'
140930020305     c                   movel     'S'           i95fi1
140940020305     c                   endif
140950970808     C                   ELSE
140960970808     C* ** MITTENTE **
140970970808    2C     E14CAP        IFNE      VARCAM
140980970808     C                   CLEAR                   WZVM
140990970808     C                   CLEAR                   WZ1M
141000970808     C                   CLEAR                   WZ2M
141010970808     C                   MOVEL     E14CAP        VARCAM
141020970808    2C                   ENDIF
141030970808     C* MODIFICA LA LOCALITA'
141040970808    2C     E14LOC        IFNE      VARLOM
141050970808     C                   MOVEL     E14LOC        VARLOM
141060970808     C                   CLEAR                   WZ1M
141070970808     C                   CLEAR                   WZ2M
141080970808    2C                   ENDIF
141090020305     c     lnpntw        ifeq      'FED'
141100020305     c                   movel     'S'           i95fi1
141110020305     c                   endif
141120970808     C                   MOVEL     WZVM          E13FZV
141130970808     C                   MOVEL     WZ1M          E13FZ1
141140970808     C                   MOVEL     WZ2M          E13FZ2
141150970808     C                   END
141160970808     C*
141170970808     C**
141180970808     C** CALL
141190970808     C                   CALL      'FNLV13R'
141200970808     C                   PARM                    KPJBA
141210030612     C                   PARM                    Fnlv13DS
141220030612     C                   PARM                    Tisi95DS
141230970808     C*
141240970808     C                   MOVEL     O13ERR        WERR              1
141250970808     C                   MOVEL     O13MSG        VIDMSG
141260970808     C                   EXSR      CTRERR
141270970808     C*
141280970808     C* REIMPOSTO LE FORZATURE
141290970808     C     WCLI          IFEQ      'D'
141300970808     C                   MOVEL     E13FZV        WZV
141310970808     C                   MOVEL     E13FZ1        WZ1
141320970808     C                   MOVEL     E13FZ2        WZ2
141330970808     C                   ELSE
141340970808     C                   MOVEL     E13FZV        WZVM
141350970808     C                   MOVEL     E13FZ1        WZ1M
141360970808     C                   MOVEL     E13FZ2        WZ2M
141370970808     C                   END
141380150203     c* memorizzo la località normalizzata per destinatario
141390150203     c                   if        wcli='D'
141400150203     c                   eval      normlod=o95loc
141410150203     c                   endif
141420970808     C     ENDIND        ENDSR
141430070320     C*
141440070320     C*--- Gestisce record"L"di FIAR4 --------------------------------*
141450070320     C     GestAR4L      BEGSR
141460070320     c* lo faccio solo se GEO attiva
141470070320    0c                   if        FGSGEOT='S'
141480070320     c* Se devo chainare
141490070320     C                   MOVEL     'L'           WTRC
141500070320     c
141510070321    1c                   if        TipoEla='C'
141520070320     c                   clear                   Wesar4L
141530070320     C     KARB2         CHAIN     FiAR4000                           3091
141540070320    2c                   if        *in91
141550070320     C                   SETON                                        28
141560070320     C                   MOVEL     MSG(4)        VIDMSG
141570070320     C                   GOTO      ENDar4L
141580070320    2c                   endif
141590070320     c
141600070320    2c                   if        not *in30
141610070320     c                   movel     'E'           WEsar4L           1
141620070320     c                   movel     ar4not        dsbl4L
141630070320     c                   else
141640070320     c                   movel     'N'           WEsar4L
141650070320     c                   clear                   dsbl4L
141660070320     c                   clear                   fiar4000
141670070320    2c                   endif
141680070320    1c                   endif
141690070320     c
141700070320    1c                   if        tipoela='A'
141710070320     c                   movel     dsbl4l        ar4not
141720070320     c                   movel     dateu         ar4duv
141730070320     c                   movel     dateu         ar4DTR
141740070320     c                   movel     'T'           ar4ftr
141750070320     c
141760070320    3c                   if        wesar4L='N'
141770070320     c                   eval      ar4aas=arbaas
141780070320     c                   eval      ar4lnp=arblnp
141790070320     c                   eval      ar4nrs=arbnrs
141800070320     c                   eval      ar4nsp=arbnsp
141810070320     c                   eval      ar4trc='L'
141820070320     c                   write     fiar4000
141830070320   x3c                   else
141840070320     c                   update    fiar4000
141850070320    3c                   endif
141860070320    1c                   endif
141870070320     c
141880070320    0c                   endif
141890070320     c
141900070320     C     ENDar4L       ENDSR
141910970808     C*
141920970808     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
141930970808     C     CTRERR        BEGSR
141940970808     C* IMPOSTO CAMPI PASSATI
141950970808    1C     O13RON        IFEQ      'S'
141960970808     C                   MOVEL     O95NAR        E14NAR
141970150112     c                   if        d48ffr='E'
141980150112     c                   clear                   werr
141990150112     c                   eval      msgforzatxt=o13msg
142000150112     c                   endif
142010970808    1C                   ENDIF
142020970808    1C     O13ROC        IFEQ      'S'
142030970808     C                   MOVEL     O95CAP        E14CAP
142040150112     c                   if        d48ffr='E'
142050150112     c                   clear                   werr
142060150112     c                   eval      msgforzatxt=o13msg
142070150112     c                   endif
142080970808    1C                   ENDIF
142090970808    1C     O13ROP        IFEQ      'S'
142100970808     C                   MOVEL     O95PRV        E14PRV
142110150112     c                   if        d48ffr='E'
142120150112     c                   clear                   werr
142130150112     c                   eval      msgforzatxt=o13msg
142140150112     c                   endif
142150970808    1C                   ENDIF
142160970808    1C     O13ROL        IFEQ      'S'
142170970808     C                   MOVEL     O95LOC        E14LOC
142180150112     c                   if        d48ffr='E'
142190150112     c                   clear                   werr
142200150112     c                   eval      msgforzatxt=o13msg
142210150112     c                   endif
142220970808    1C                   ENDIF
142230970808     C* ERRORE O RICERCA CAMPO
142240970808    1C     WERR          IFNE      ' '
142250970808     C*
142260970808    2C     VIDMSG        IFNE      *BLANKS
142270970808     C                   SETON                                        28
142280970808    2C                   ENDIF
142290970808     C* POSIZIONAMENTO CURSORE
142300970808    2C                   SELECT
142310970808     C     WERR          WHENEQ    '1'                                          R.SOCIALE
142320970808     C                   SETON                                        40
142330970808     C     WERR          WHENEQ    '2'                                          INDIRIZZO
142340970808     C                   SETON                                        41
142350970808     C     WERR          WHENEQ    '5'                                          CAP
142360970808     C     WCLI          IFEQ      'D'
142370970808     C                   SETON                                        43
142380970808     C                   ELSE
142390970808     C                   SETON                                        42
142400970808     C                   END
142410970808     C     WERR          WHENEQ    '3'                                          LOCALITA
142420970808     C                   SETON                                        43
142430970808     C     WERR          WHENEQ    '4'                                          PROVINCIA
142440970808     C     WCLI          IFEQ      'D'
142450970808     C                   SETON                                        50
142460970808     C                   ELSE
142470970808     C                   SETON                                        45
142480970808     C                   END
142490970808     C     WERR          WHENEQ    '6'                                          NAZIONE
142500970808     C                   SETON                                        44
142510970808     C     WERR          WHENEQ    '7'                                          PARTITA IVA
142520970808     C     WCLI          IFEQ      'D'
142530970808     C                   SETON                                        45
142540970808     C                   ELSE
142550970808     C                   SETON                                        49
142560970808     C                   END
142570970808     C     WERR          WHENEQ    '8'                                          COD.ISO
142580970808     C     WCLI          IFEQ      'D'
142590970808     C                   SETON                                        46
142600970808     C                   ELSE
142610970808     C                   SETON                                        50
142620970808     C                   END
142630970808     C     WERR          WHENEQ    '9'                                          INOLTRO
142640970808     C     WCLI          ANDEQ     'D'
142650970808     C                   SETON                                        49
142660020318     c     werr          wheneq    '0'
142670020318     C     WCLI          IFEQ      'D'
142680020318     C                   SETON                                        50
142690020318     C                   ELSE
142700020318     C                   SETON                                        45
142710020318     C                   END
142720020318     c
142730970808    2C                   ENDSL
142740970808     C*
142750970808     C                   SETON                                        90
142760970808    1C                   ENDIF
142770970808     C                   ENDSR
142780091217     c*
142790091217     c* Aggiornamento per record GEN ddel fiar5 --------------------------------*
142800091217     c     RegisAR5GEN   BEGSR
142810091217     c                   clear                   dar5gen
142820091217     c                   Eval      kAr5Trd = 'GEN'
142830091217     c     kAr5          Chain     Fiar501l
142840091217      * Aggiorno Fiar5
142850091217if  1c                   If        %Found(Fiar501l)
142860091217     c                   movel     ar5uni        DAr5Gen
142870091217     c*
142880091217     c* Aggiorno tipo incasso in arrivo
142890091217    2c                   select
142900091217     c                   when      Wtipoagg='TIC'
142910091217     c                   Eval      §Ar5ticmb=vidtic
142920100119     c                   eval      §ar5flgMB='S'
142930091217     c
142940091217     c                   when      Wtipoagg='REF'
142950091217     c                   Eval      §Ar5Ref = VidNom
142960091217     c                   Eval      §Ar5Teld = VidTel
142970140403     c                   when      Wtipoagg='PIK'
142980140407     c                   Eval      §Ar5pk2 = wpk2
142990091217    2c                   ENDSL
143000091217
143010091217     c                   Movel(p)  DAr5Gen       Ar5Uni
143020091217     c
143030091217      * se devo trasmettere alla sede sfleggo per la trasmissione
143040091217    2c                   If        §Ar5Trse = 'S'
143050091217     c                   Clear                   Ar5Ft2
143060091217    2c                   EndIf
143070091217     c                   Clear                   Ar5Ft3
143080091217     c                   Update    Fiar5000
143090091217      * Scrivo Fiar5
143100091217   x1c                   Else
143110091217      * se immessi i dati
143120091217     c                   Clear                   Fiar5000
143130091217     c                   Eval      Ar5Aas = ArbAas
143140091217     c                   Eval      Ar5Lnp = ArbLnp
143150091217     c                   Eval      Ar5Nrs = ArbNrs
143160091217     c                   Eval      Ar5Nsp = ArbNsp
143170091217     c                   Eval      Ar5Trd = 'GEN'
143180091217     c                   Move      Dateu         Ar5Dac
143190091217     c                   Movel     W0140         Ar5Orc
143200091217     c                   Clear                   Dar5Gen
143210091217     c
143220091217    2c                   select
143230091217     c                   when      Wtipoagg='TIC'
143240091217     c                   Eval      §Ar5ticmb=vidtic
143250100119     c                   eval      §ar5flgMB='S'
143260091217     c
143270091217     c                   when      Wtipoagg='REF'
143280091217     c                   Eval      §Ar5Ref = VidNom
143290091217     c                   Eval      §Ar5Teld = VidTel
143300140403     c                   when      Wtipoagg='PIK'
143310140407     c                   Eval      §Ar5pk2 = wpk2
143320091217    2c                   ENDSL
143330091217
143340091217     c                   Movel(p)  DAr5Gen       Ar5Uni
143350091217    2c                   if        ar5uni<>*blanks
143360091217     c                   Write     Fiar5000
143370091217    2c                   EndIf
143380091217    1c                   EndIf
143390091217     c
143400091217     c                   ENDSR
143410091217     c
143420911212     C*--- ALLOCO OGGETTI --------------------------------------------*
143430170116     C**   ALLOC         BEGSR
143440170116     C**                 MOVE      ')'           VAR               7
143450170116     C**                 MOVE      '))'          VAR2              8
143460170116     C**                 Z-ADD     48            LUNG             15 5
143470920120     C*
143480170116     C**                 MOVEA     SFILE         C20(13)
143490170116     C**                 MOVEA     SFILE         C21(13)
143500170116     C**                 MOVEA     SFILE         C22(13)
143510170116     C**                 MOVEA     C22           SOVR(1)
143520170116     C**                 MOVEA     C20           SALC(1)
143530170116     C**                 MOVEA     C21           SDLC(1)
143540920120     C*
143550041027     C* solo PER SEDE
143560170116     C**   §7LFFI        IFEQ      'S'
143570911212     C* CHECK OBJ E ADDPFM
143580170116     C**                 Z-ADD     046           PARMBR            3 0
143590170116     c**                 move      046           $mbr
143600170116     C**                 MOVEL     SFILE         FISICO
143610170116     C**                 CALL      'TRUL50C'
143620170116     C**                 PARM                    FISICO           10
143630170116     C**                 PARM                    MBRFIS           10
143640170116     C**                 PARM                    LIB              10
143650170116     C**                 PARM                    LOGICO           10
143660170116     C**                 PARM                    MBRLOG           10
143670170116     C**                 PARM                    ULFLG             1
143680911212     C** OVRDBF
143690170116     c**                 movel     $mbr          var
143700170116     C**                 MOVE      VAR           SOVR
143710170116     C**                 MOVEL     *BLANKS       COMMAN           80
143720170116     C**                 MOVEA     SOVR(1)       COMMAN
143730170116     C**                 CALL      'QCMDEXC'
143740170116     C**                 PARM                    COMMAN
143750170116     C**                 PARM                    LUNG
143760911212     C* ALLOCO OGGETTO
143770170116     c**                 movel     $mbr          var2
143780170116     C**                 MOVE      VAR2          SALC
143790170116     C**                 MOVEL     *BLANKS       COMMAN           80
143800170116     C**                 MOVEA     SALC(1)       COMMAN
143810170116     C**                 CALL      'QCMDEXC'                            91
143820170116     C**                 PARM                    COMMAN
143830170116     C**                 PARM                    LUNG
143840911212     C*
143850911212     C* 90 ON - MEMBRO ALLOCATO MESSAGGIO DI ATTENDERE
143860170116     C** 91              GOTO      ENDALC
143870170116     C**                 END
143880941212     C*
143890170116     C**   ENDALC        TAG
143900941212     C* 91 ON - ERRORE
143910941212     C*
143920170116     C**   *IN91         IFEQ      *ON
143930170116     C**                 SETON                                        28
143940170116     C**                 MOVEa     MSG(136)      k78
143950170116     C**                 MOVEa     SFILE         k78(68)
143960170116     C**                 MOVEa     k78           vidmsg
143970170116     C**                 ENDIF
143980941212     C*
143990170116     C**                 ENDSR
144000920831     C*
144010940321     C*--- AGGIORNO FILE TASSAZIONE PADRONCINI -----------------------*
144020950113     C     AGGPDS        BEGSR
144030041008     C                   MOVEL     ' '           UNO               1
144040041008     c* richiamo pgm fnlv57ds
144050041008     c                   clear                   fnlv57ds
144060041008     C* 01 ON  - VARIAZIONE DESTINATARIO
144070041008    1c     *in01         ifeq      *on
144080041008    2C     ARBRSD        IFNE      OLDRSD
144090041008     C     ARBCAD        ORNE      OLDCAD
144100041008     C     ARBFIN        ORNE      OLDFIN
144110041008     c                   eval      uno='1'
144120041008    2c                   endif
144130041008    1c                   endif
144140041008     c
144150041008    1c                   if        *in02
144160041008    2c     ARBVLF        IFNE      OLDVLF
144170041008     C     ARBPKF        orne      OLDPKF
144180041008     c                   eval      uno='1'
144190041008    2c                   endif
144200041008    1c                   endif
144210041008     c*
144220041008    1c                   if        uno='1'
144230041008     c                   eval      i57aas=arbaas
144240041008     c                   eval      i57lnp=arblnp
144250041008     c                   eval      i57nrs=arbnrs
144260041008     c                   eval      i57nsp=arbnsp
144270041008     c
144280041008     c                   select
144290041008     c* Se presenti entrambe le bolle
144300041008     c                   when      waltrabolla=' '
144310070320     c                   eval      i57drt=arbdrt
144320070320     c                   eval      i57nrt=arbnrt
144330070320     c                   eval      i57fpp=arbfpp
144340070320     c                   eval      i57prt=arbpdr
144350070320     c                   eval      i57pco=arbpdc
144360041008     c* modifica in arrivo non presente fnblp
144370041008     c                   when      *in10 and waltrabolla='N'
144380041008     c                   eval      i57pdr=arbpdc
144390041008     c                   eval      i57tbo='A'
144400041008     c* modifica in partenza non presente fnarb
144410041008     c                   when      not *in10 and waltrabolla='N'
144420070320     c                   eval      i57pdr=arbpdr
144430041008     c                   eval      i57tbo='P'
144440070320     c                   eval      i57drt=arbdrt
144450070320     c                   eval      i57nrt=arbnrt
144460070320     c                   eval      i57fpp=arbfpp
144470041008     c                   endsl
144480041008     c
144490041008     c                   eval      i57ncl=arbncl
144500041011     c   02              eval      i57tvl='F'
144510041008     c* volume vecchio
144520041008     c                   eval      i57vlc=arbvlc
144530041008     c                   eval      i57ncr=arbncr
144540041008     c                   eval      i57vlf=oldvlf
144550041008     c* volume nuovo
144560041008     c                   eval      i57vcn=arbvlc
144570041008     c                   eval      i57ncn=arbncp
144580041008     c                   eval      i57vfn=arbvlf
144590041008     c* peso vecchio
144600041008     c                   eval      i57pkc=arbpkc
144610041008     c                   eval      i57ncp=arbncp
144620041008     c                   eval      i57pkf=oldpkf
144630041008     c* peso nuovo
144640041008     c                   eval      i57pcn=arbpkc
144650041008     c                   eval      i57npn=arbncp
144660041008     c                   eval      i57pfn=arbpkf
144670041008     c* destinatario vecchio
144680041008     c                   eval      i57rsd=oldrsd
144690041008     c                   eval      i57cad=oldcad
144700041008     c                   eval      i57fin=oldfin
144710041008     c* destinatario nuovo
144720041008     c                   eval      i57rdn=arbrsd
144730041008     c                   eval      i57cdn=arbcad
144740041008     c                   eval      i57fdn=arbfin
144750041008     c
144760041008     c                   call      'FNLV57R'
144770041008     c                   parm                    kpjba
144780041008     c                   parm                    fnlv57ds
144790041008     c                   endif
144800940321     C*
144810940321     C                   ENDSR
144820950113     C*
144830950113     C* AGGIORNA PADRONCINI PER IL MITTENTE --------------------------*
144840950113     C     AGGPMT        BEGSR
144850950113     C*
144860950113    1C     ARBKSC        IFNE      OLDKSC
144870950113     C     ARBRSM        ORNE      OLDRSM
144880950113     C     ARBCAM        ORNE      OLDCAM
144890950113     C*
144900950113     C* CONTROLLO CON QUALE VOLUME HO AGGIORNATO LA BOLLA
144910950113     C                   CLEAR                   WVOL
144920950113     C*
144930950113     C* VOLUME PARZIALE "Z"
144940950113     C*
144950050111     C     ARBNCR        ifne      ARBNCL
144960950113     C*
144970950113    3C     ARBVLC        IFGE      ARBVLF
144980950113     C                   Z-ADD     ARBVLC        WVOL
144990950113   X3C                   ELSE
145000950113     C                   Z-ADD     ARBVLF        WVOL
145010950113    3C                   ENDIF
145020950113     C* VOLUME TOTALE "T"
145030950113   X2C                   ELSE
145040950113     C                   Z-ADD     ARBVLC        WVOL
145050950113    2C                   ENDIF
145060950113     C****
145070950113     C** ESEGUO L'AGGIORNAMENTO DEI FILE PADRONCINI 2 VOLTE:
145080011126     C**  - LA PRIMA   VOLTA PER I FILE fiftt00F/fiftd00F
145090011126     C**  - LA SECONDA VOLTA PER I FILE fifst00F/fifsd00F - SIMULAZIONE
145100950113     C****
145110950113     C* 17 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
145120950113     C                   SETOFF                                       17
145130950113     C*
145140070320     C                   MOVE      arbPDR        WPDR
145150070320     C                   Z-ADD     arbDRT        WDRT
145160950113     C* NUMERO DISTINTA
145170070320    2C     arbFPP        IFEQ      'M'
145180950113     C                   Z-ADD     1000          WNDC
145190950113     C                   ELSE
145200950113     C                   Z-ADD     5000          WNDC
145210950113    2C                   ENDIF
145220070320     C                   ADD       arbNRT        WNDC
145230950113     C*
145240950113     C*
145250950113    2C                   DO        2
145260011126     C   17KFTT2         CHAIN     fifst000                           30
145270011126     C  N17KFTT2         CHAIN     fiftt000                           30
145280950113     C*
145290950113    3C     *IN30         IFEQ      *OFF
145300950113     C*
145310950113     C* SE GIA' CONFERMATA NON AGGIORNO NIENTE
145320950113    4C     FTTFVL        IFNE      'C'
145330020326     c*
145340020326     c  n17kftd5         chain     fiftd005                           30
145350020326     c   17kftd5         chain     fifsd005                           30
145360020326     c     *in30         ifeq      *off
145370020326     c                   z-add     arbpkf        ftdpkl
145380020326     c                   z-add     wvol          ftdvlu
145390020326     c                   movel     arbrsm        ftdrsc
145400020326     c* dò per scontato che la bolla sia un porto franco
145410020326     c                   z-add     arbksc        ftdksc
145420020326     c                   movel     arbcam        ftdcap
145430020326     c                   movel     arbfap        ftdfin
145440020326     c* se è stato modificato codice cliente o ragione sociale devo
145450020326     c* aggiornare anche il numero di stop
145460020327    1C     ARBKSC        IFNE      OLDKSC
145470020327     C     ARBRSM        ORNE      OLDRSM
145480020327     c                   exsr      aggstp
145490020327     c                   endif
145500020327     c  n17              update    fiftd005
145510020327     c   17              update    fifsd005
145520020326     c                   endif
145530950113     C*
145540950113     C* SE VALORIZZATA SVALORIZZO
145550950113     C                   EXSR      SVAFTT
145560950113     C*
145570950113    4C                   ENDIF
145580950113    3C                   ENDIF
145590950113     C*
145600950113     C* 17 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
145610950113     C                   SETON                                        17
145620950113    2C                   ENDDO
145630950113     C*
145640950113    1C                   ENDIF
145650950113     C*
145660950113     C                   ENDSR
145670020327     c*
145680020327     c* AGGSTP - Aggiornamento numero stop a cambio di mittente ------*
145690020327     C     AGGSTP        BEGSR
145700020327     c                   clear                   wstp
145710020327     C* ESCLUDENDO FRANCHI E ASSEGNATI VARI,
145720020327     C*   CHAINO FNCLS00F PER PRENDERE IL NUMERO DEGLI STOP
145730020327     C                   Z-ADD     ftdksc        w0040             4 0
145740020327    5C     w0040         IFNE      *ALL'8'
145750020327     C     w0040         ANDNE     *ALL'9'
145760020327     C     ftdksc        CHAIN     FNCLS000                           31
145770020327     C  N31              z-add     CLSSTP        wstp
145780020327    5C                   ENDIF
145790020327     c     wstp          ifeq      *zeros
145800020327     c* prima di fare la chain mi devo salvare i dati del record di fiftd005
145810020327     c                   movel     dsftd         sav_dsftd
145820020327     C                   MOVEL     ARBKSC        WKSC
145830020327     C                   MOVEL     ARBRSM        WRSM
145840020327     C  N17KFTD          CHAIN(N)  fiftd004                           31
145850020327     C   17KFTD          CHAIN(N)  fifsd004                           31
145860020327     c  n31              z-add     ftdstp        wstp
145870020327     c* Se il numero stop da attribuire è ancora = a 0 devo cercare
145880020327     c* nella distinta il primo numero disponibile
145890020327     c     wstp          ifeq      *zeros
145900020327     C  N17KFTT2         setgt     fiftd001
145910020327     C   17KFTT2         setgt     fifsd001
145920020327     C  N17KFTT2         readpe    fiftd001
145930020327     C   17KFTT2         readpe    fifsd001
145940020327     c                   if        not %eof
145950020327     c                   eval      wstp = (ftdstp+1)
145960020417     c     wstp          iflt      1000
145970020417     c                   z-add     1000          wstp
145980020417     c                   endif
145990020417     c                   else
146000020417     c* qui non dovrebbe mai entrare ma lo gestisco comunque anche
146010020417     c* per unifomrità con fnlsa2r
146020020417     c                   z-add     1000          wstp
146030020327     c                   endif
146040020327     c                   endif
146050020327     c*
146060020327     c* ripristino i dati del record salvato
146070020327     c                   movel     sav_dsftd     dsftd
146080020327     c                   endif
146090020327     c                   z-add     wstp          ftdstp
146100020327     C                   ENDSR
146110020326     c*
146120950113     C* SVALORIZZA UNA DISTINTA --------------------------------------*
146130950113     C     SVAFTT        BEGSR
146140950113     C* SOLO SE VALORIZZATA
146150950113    6C     FTTFVL        IFEQ      'V'
146160950113     C                   MOVEL     FTDPDR        PA3PDR                         *COD. PADRONCINO
146170950113     C                   MOVEL     FTDTSR        PA3TSR                         *TIPO PRESTAZIONE
146180950113     C                   Z-ADD     FTDNDC        PA3NDC                         *NUMERO DISTINTA
146190950113     C                   Z-ADD     FTDDDC        PA3DDC                         *DATA   DISTINTA
146200950113     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
146210950113     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
146220950113     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
146230041122     C  n17              MOVEL     ' '           PA3sml                         *FLG VAL.MANUAL.TEST
146240041122     C   17              MOVEL     'S'           PA3sml                         *FLG VAL.MANUAL.TEST
146250950113     C                   MOVEL     PARAM3        KPJBU
146260011129     C  N17              CALL      'FICNE2R'
146270950113     C                   PARM                    KPJBA
146280011129     C   17              CALL      'FICNE9C'
146290950113     C                   PARM                    KPJBA
146300950113    6C                   ENDIF
146310950113     C                   ENDSR
146320941102     C*
146330941102     C* CONTROLLO DIVISA ---------------------------------------------*
146340941102     C     CTRVLT        BEGSR
146350011011      *
146360011011      * La divisa è obbligatoria se è inserito un importo
146370011011     C     W0133         IFGT      *ZEROS
146380011011     C     WVLT          ANDEQ     *BLANKS
146390011011     C                   MOVEL     MSG(100)      VIDMSG
146400011011     C                   SETON                                        9028
146410011011     C                   GOTO      ENDVLT
146420011011     C                   ENDIF
146430941102     C*
146440941102     C                   MOVEL     'CV'          COD
146450941102     C* RICERCA
146460941102     C     '?'           SCAN      WVLT                                   90
146470941102     C     *IN90         IFEQ      *ON
146480991011     C                   CLEAR                   §KEY
146490941102     C                   CALL      'X§TABER'
146500941102     C                   PARM                    CODUT
146510941102     C                   PARM                    COD
146520941102     C                   PARM                    §KEY
146530991011     C                   PARM                    §DES             25
146540941102     C                   MOVEL     §KEY          WVLT
146550941102     C                   GOTO      ENDVLT
146560941102     C                   ENDIF
146570941102     C*
146580941102     C* CONTROLLO DIVISA
146590941102     C                   MOVEL(P)  WVLT          KEY
146600941213     C                   EXSR      CTABEL
146610941102     C* ERRORE
146620941102    1C     *IN30         IFEQ      *ON
146630941102     C                   MOVEL     MSG(26)       VIDMSG
146640941102     C                   SETON                                        9028
146650941102     C                   GOTO      ENDVLT
146660941102    1C                   ENDIF
146670941102     C                   MOVEL     TBLUNI        DSCV
146680941102     C*
146690941102     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
146700941102    1C     §CVFDC        IFNE      'S'
146710941102     C                   MOVE      W0133         W0033             3 3
146720941102    2C     W0033         IFNE      0
146730941102     C                   MOVEL     MSG(27)       VIDMSG
146740941102     C                   SETON                                        9028
146750941102     C                   GOTO      ENDVLT
146760941102    2C                   ENDIF
146770941102    1C                   ENDIF
146780011010      *
146790011010      * Controllo il numero di decimali immessi
146800011011    1C     §CVFDC        IFEQ      'S'
146810011011     C     W0133         ANDNE     *ZEROS
146820011011      *
146830011011    2C                   SELECT
146840011011     C     §CVDEC        WHENEQ    3
146850011011     C     §CVDEC        WHENEQ    2
146860011011     C                   MOVE      W0133         W0010             1 0
146870011011    3C     W0010         IFNE      *ZEROS
146880011011     C                   MOVEA     MSG(93)       K78
146890011011     C                   MOVE      §CVDEC        WCVDEC            2
146900011011     C                   MOVEA     WCVDEC        K78(29)
146910011011     C                   MOVEA     K78           VIDMSG
146920011011     C                   SETON                                        9028
146930011011    3C                   ENDIF
146940011011     C     §CVDEC        WHENEQ    1
146950011011     C                   MOVE      W0133         W0020             2 0
146960011011    3C     W0020         IFNE      *ZEROS
146970011011     C                   MOVEA     MSG(93)       K78
146980011011     C                   MOVE      §CVDEC        WCVDEC            2
146990011011     C                   MOVEA     WCVDEC        K78(29)
147000011011     C                   MOVEA     K78           VIDMSG
147010011011     C                   SETON                                        9028
147020011011    3C                   ENDIF
147030011011     C     §CVDEC        WHENEQ    0
147040011011    2C                   ENDSL
147050011011      *
147060011011    1C                   ENDIF
147070941102     C*
147080941102     C     ENDVLT        ENDSR
147090941212     c*
147100941212     C*--- CHAIN TABEL00F --------------------------------------------*
147110941212     C     CTABEL        BEGSR
147120941212     C     KTAB2         CHAIN     TABEL                              30
147130941212     C  N30TBLFLG        IFNE      ' '
147140941212     C                   SETON                                        30
147150941212     C                   END
147160941212     C                   ENDSR
147170941212     C*---------------------------------------------------------------
147180941212     C* DISALLOCO OGGETTO MEMBRO PER LA SEDE
147190941212     C*---------------------------------------------------------------
147200170116     C**   DLCSED        BEGSR
147210170116     c**                 move      046           $mbr
147220170116     c**                 movel     $mbr          var2
147230170116     C**                 MOVE      VAR2          SDLC(1)
147240170116     C**                 MOVEL     *BLANKS       COMMAN           80
147250170116     C**                 MOVEA     SDLC(1)       COMMAN
147260170116     C**                 CALL      'QCMDEXC'                            91
147270170116     C**                 PARM                    COMMAN
147280170116     C**                 PARM                    LUNG
147290170116     C**                 ENDSR
147300941213     C*---------------------------------------------------------------
147310941213     C* STORICIZZO LA TASSAZIONE
147320941213     C*---------------------------------------------------------------
147330941213     C     STOARB        BEGSR
147340991006     C* PER LA PRIMA BOLLA KSC E CTR LI PRENDO SEMPRE DALLA BOLLA
147350991006     C* PRINCIPALE PERCHE' SONO SICURA DI AVERLA
147360991006     C* PER LA SECONDA BOLLA LI DEVO PRENDERE DA AR6 PERCHE'
147370991006     C* QUELLI DELLA BOLLA PRINCIPALE SI RIFERISCONO ALLA PRIMA BOLLA
147380991006     C     AR6TRC        IFEQ      '2'
147390991006     C                   Z-ADD     AR6KSC        ARBKSC
147400991006     C                   Z-ADD     AR6CTR        ARBCTR
147410991006     C                   ENDIF
147420991006     C                   MOVEL     AR6DIV        ARBDIV
147430941213     C                   MOVEL     AR6POR        ARBPOR
147440941213     C                   MOVEL     AR6SV1        ARBSV1
147450941213     C                   MOVEL     AR6VA1        ARBVA1
147460941213     C                   MOVEL     AR6SV2        ARBSV2
147470941213     C                   MOVEL     AR6VA2        ARBVA2
147480941213     C                   MOVEL     AR6SV3        ARBSV3
147490941213     C                   MOVEL     AR6VA3        ARBVA3
147500941213     C                   MOVEL     AR6IMV        ARBIMV
147510941213     C                   MOVEL     AR6ALI        ARBALI
147520941213     C                   MOVEL     AR6CEI        ARBCEI
147530941213     C                   MOVEL     AR6IFT        ARBIFT
147540941213     C                   MOVEL     AR6DFT        ARBDFT
147550941213     C                   MOVEL     AR6NFT        ARBNFT
147560941213     C                   MOVEL     AR6FIV        ARBFIV
147570950114     C* SE VENGO DA VARIAIZONE MITTENTE COME CODICE TARIFFA CI METTO
147580950114     C*  QUELLO SALVATO
147590950114     C   24              MOVEL     MITCTR        ARBCTR
147600991013     C                   MOVEL     AR6FIM        ARBFIM
147610011108      *
147620011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
147630011108      * è in partenza 'P' o in arrivo 'A'
147640011108     C     *IN10         IFEQ      *ON
147650011108     C                   MOVE      'A'           ARBPRU
147660011108     C                   ENDIF
147670011108     C     *IN10         IFEQ      *OFF
147680011108     C                   MOVE      'P'           ARBPRU
147690011108     C                   ENDIF
147700011108      *
147710950114     C*
147720991006     C                   WRITE     FIARBT00
147730950114     C*
147740991006     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
147750941221    1C     WAR7          IFEQ      'E'
147760991006    1C     ARBTRC        ANDEQ     '1'
147770991006     C                   MOVEL     '1'           WTRC
147780991006     C     KARB2         SETLL     FIAR7000
147790991006     C     KARB2         READE(N)  FIAR7000                               30
147800941213     C*
147810941213    2C     *IN30         DOWEQ     *OFF
147820941213     C                   MOVEL     AR7SVN        ARBSVN
147830941213     C                   MOVEL     AR7VAN        ARBVAN
147840991006     C                   WRITE     FIARBU00
147850941213     C*
147860991006     C     KARB2         READE(N)  FIAR7000                               30
147870941213    2C                   ENDDO
147880941213     C                   ENDIF
147890041130      *-------
147900041130      * Seconda bolla
147910041130     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
147920041130    1C     WAR72         IFEQ      'E'
147930041130    1C     ARBTRC        ANDEQ     '2'
147940041130     C                   MOVEL     '2'           WTRC
147950041130     C     KARB2         SETLL     FIAR7000
147960041130     C     KARB2         READE(N)  FIAR7000                               30
147970041130     C*
147980041130    2C     *IN30         DOWEQ     *OFF
147990041130     C                   MOVEL     AR7SVN        ARBSVN
148000041130     C                   MOVEL     AR7VAN        ARBVAN
148010041130     C                   WRITE     FIARBU00
148020041130     C*
148030041130     C     KARB2         READE(N)  FIAR7000                               30
148040041130    2C                   ENDDO
148050041130     C                   ENDIF
148060041130      *-------
148070941213     C*
148080941213     C                   ENDSR
148090941214     C*----------------------------------------------------------------
148100941214     C* RICALCOLO IL VOLUME AUTOMATICO
148110941214     C*----------------------------------------------------------------
148120941214     C     VOLAUT        BEGSR
148130941214     C*
148140941214     C* VOLUME AUTOMATICO A PESO: PESO MODIFICATO
148150941214    3C     VIDPKF        IFNE      ARBPKF
148160941214     C     ARBFVF        ANDEQ     'K'
148170941214     C*
148180120702     C     ARBPKF        DIV(H)    ARBVLF        CLIKPM            9 1
148190941214     C     VIDPKF        DIV(H)    CLIKPM        D20VLU
148200941214     C*
148210941214     C     ARBVLB        IFEQ      0
148220941214     C                   Z-ADD     0,001         D20VLU
148230941214     C                   END
148240941214     C                   MOVEL     ARBFVF        D20FVL
148250941214     C*
148260941214    3C                   END
148270941214     C                   ENDSR
148280941215     C**************************************************************************
148290941215     C*  ABBLANCA ZERO NON SIGNIFICATIVI
148300941215     C**************************************************************************
148310941215     C     ABBL          BEGSR
148320941215     C*
148330941215     C                   SETOFF                                       99
148340941215     C     SKI(I)        DOWEQ     '0'
148350941215     C                   MOVEL     ' '           SKI(I)
148360941215     C                   ADD       1             I
148370941215     C     I             COMP      15                                     99
148380941215     C  N99              END
148390941215     C*
148400941215     C                   ENDSR
148410051114     C**************************************************************************
148420051114     C*  ABBLANCA ZERO NON SIGNIFICATIVI
148430051114     C**************************************************************************
148440051114     C     SR_chkcomb    BEGSR
148450051114     c                   clear                   wcomb             1
148460051114     C* Non è possibile indicare contemporaneamente consegne particolari
148470051117     C*  'S' ed 'X' / 'S' ed 'A' / "S" e "P" / "X" e "P"
148480051114    2C                   IF           (wTC1 = 'X'  and  wTC2 = 'S')
148490051114     C                             or (wTC1 = 'S'  and  wTC2 = 'X')
148500051114     C                             or (wTC1 = 'A'  and  wTC2 = 'S')
148510051114     C                             or (wTC1 = 'S'  and  wTC2 = 'A')
148520051114     C                             or (wTC1 = 'S'  and  wTC2 = 'P')
148530051114     C                             or (wTC1 = 'P'  and  wTC2 = 'S')
148540051117     C***                          or (wTC1 = 'X'  and  wTC2 = 'A')
148550051117     C***                          or (wTC1 = 'A'  and  wTC2 = 'X')
148560051114     C                             or (wTC1 = 'X'  and  wTC2 = 'P')
148570051114     C                             or (wTC1 = 'P'  and  wTC2 = 'X')
148580051114     c                   eval      wcomb = 'S'
148590051114     c                   endif
148600051114     C                   ENDSR
148610060227     C**************************************************************************
148620060301     C*  Richiamo pgm di richiesta/controllo motivazione variazione
148630060227     C**************************************************************************
148640060227     C     ges_mtv       BEGSR
148650060227     c                   clear                   fnlv85ds
148660060227     c                   movel     'V'           ilv85tel
148670060227     c                   if        d48ffr = 'S' or d48ffr = 'V'
148680060227     c                   clear                   ilv85wdw
148690060227     c                   else
148700060301     c                   if        d48ffr = *blanks
148710060301     c                   movel     'E'           ilv85wdw
148720060301     c                   else
148730060227     c                   movel     'N'           ilv85wdw
148740060301     c                   endif
148750060227     c                   endif
148760060227     c                   movel     d48cvb        ilv85cvb
148770060227     c                   if        d48mtv <> *blanks
148780060227     c                   movel     d48mtv        ilv85mtv
148790060227     c                   movel     d48not        ilv85nt1
148800060227     c                   move      d48not        ilv85nt2
148810060227     c                   endif
148820060227     c                   movel     fnlv85ds      kpjbu
148830060302     c                   call      'FNLV85R'
148840060227     c                   parm                    kpjba
148850060227     c                   movel     kpjbu         fnlv85ds
148860060227     c                   if        olv85err = '1'
148870060227     c                   seton                                        90
148880060227     C                   eval      vidmsg='Motivazione variazione mancante o er-
148890060227     c                             rata'
148900060227     c                   endif
148910060301     c                   if        olv85f12 = ' ' and olv85err = ' '
148920060301     c                   movel     'N'           w48mct
148930060301     c                   endif
148940060227     c                   endsr
148950060301     C**************************************************************************
148960060301     C*  Richiamo pgm per storicizzazione motivazione variazione
148970060301     C**************************************************************************
148980060301     C     sto_mtv       BEGSR
148990060303     c                   clear                   fnlv85ds
149000060301     c                   movel     ' '           ilv85tla
149010060301     c                   movel     'R'           ilv85tel
149020060301     c                   movel     d48cvb        ilv85cvb
149030060301     c                   z-add     arbdtv        ilv85dac
149040060301     c                   z-add     arborv        ilv85orc
149050060301     c                   z-add     arbaas        ilv85aas
149060060301     c                   z-add     arblnp        ilv85lnp
149070060301     c                   z-add     arbnrs        ilv85nrs
149080060301     c                   z-add     arbnsp        ilv85nsp
149090060303     c                   if        d48mtv <> *blanks and
149100060303     c                             (d48ffr = 'E' or d48mct = 'N')
149110060303     c                   movel     d48mtv        ilv85mtv
149120060303     c                   movel     d48not        ilv85nt1
149130060303     c                   move      d48not        ilv85nt2
149140060303     c                   endif
149150060301     c                   movel     fnlv85ds      kpjbu
149160060301     c                   call      'FNLV85R'
149170060301     c                   parm                    kpjba
149180060301     c                   endsr
149190120330     C**************************************************************************
149200120330     C*  AGGIORNAMENTO DA MANUTENZIONE BOLLE DI SEDE (CAUS.VAR.BOLLA "SD")
149210120330     C**************************************************************************
149220120330     C     GESSD         BEGSR
149230120330     c                   clear                   errp
149240120330     c                   clear                   erra
149250120419     c                   clear                   savcpi_p
149260120419     c                   clear                   savksc_p
149270120419     c                   clear                   savctr_p
149280120419     c                   clear                   savcpi_a
149290120419     c                   clear                   savksc_a
149300120419     c                   clear                   savctr_a
149310120330     c* chaino le bolle in partenza e in arrivo
149320120330     C     KARB          CHAIN(e)  FNBLP01L
149330120402    1c                   if        %error
149340120330     c                   eval      errp='A'
149350120402   x1c                   else
149360120402    2c                   if        not %found(fnblp01l)
149370120330     c                   eval      errp='N'
149380120418     c                   else
149390120418     c                   eval      savcpi_p=arbcpi
149400120418     c                   eval      savksc_p=arbksc
149410120418     c                   eval      savctr_p=arbctr
149420120402    2c                   endif
149430120402    1c                   endif
149440120330     C     KARB          CHAIN(e)  FNARB01L
149450120402    1c                   if        %error
149460120330     c                   eval      erra='A'
149470120402   x1c                   else
149480120402    2c                   if        not %found(fnarb01l)
149490120330     c                   eval      erra='N'
149500120413     c                   else
149510120413     c                   eval      savcpi_a=arbcpi
149520120418     c                   eval      savksc_a=arbksc
149530120418     c                   eval      savctr_a=arbctr
149540120402    2c                   endif
149550120402    1c                   endif
149560120402     c* Una delle due bolle allocate --> restituisco messaggio di avviso
149570120402    1c                   if        errp='A' or erra='A'
149580120402     c                   eval      d48err='A'
149590120402     c                   eval      d48msg='Bolla di filiale allocata da altro l+
149600120418     c                             avoro:NON AGGIORNATA in'
149610120418    2c                   if        errp='A'
149620120418     c                   eval      d48msg=%trim(d48msg)+' Partenza'
149630120418    3c                   if        erra='A'
149640120418     c                   eval      d48msg=%trim(d48msg)+' e'
149650120418    3c                   endif
149660120418    2c                   endif
149670120418    2c                   if        erra='A'
149680120418     c                   eval      d48msg=%trim(d48msg)+' Arrivo'
149690120418    2c                   endif
149700120402    1c                   endif
149710120330     c* se entrambe le bolle allocate/inesistenti vado a fine
149720120402    1c                   if        errp<>*blanks and erra<>*blanks
149730120330     c                   leavesr
149740120402    1c                   endif
149750120404     c*Variazione di PESO
149760120404    1c                   if        §bdpkf<>arbpkf
149770120402     c                   z-add     §bdpkf        arbpkf
149780120404    1c                   endif
149790120404     c*Variazione di VOLUME: il volume, in sede, può essere variato solo da
149800120404     c* profilo di sede. Siccome in questi casi la variazione avviene solitamente
149810120404     c* quando in filiale la bolla non c'è più sostituisco sempre
149820120404    1c                   if        §bdvlf<>arbvlf or §bdfvf<>arbfvf
149830120404     c                   z-add     §bdvlf        arbvlf
149840120411     c                   movel     §bdfvf        arbfvf
149850120404    1c                   endif
149860120529     c* IMOPORTO DA ASSICURARE
149870120529     c                   if        §btias<>arbias or §btvas<>arbvas
149880120529     c                   z-add     §btias        arbias
149890120529     c                   movel     §btvas        arbvas
149900120529     c                   endif
149910120404     c* Variazione TASSAZIONE: vengono caricate solo se c'è corrispodenza
149920120404     c* fra sede e filiale al livello di tipo di fattura: Segue Fattura o
149930120404     c* Fattura Immediata
149940120411     c* Chaino fiar6 per vedere se S.F. o  F.I.:
149950120404     c                   clear                   wfatfil
149960120404     c                   clear                   ar6ksc
149970120411     c                   clear                   war6
149980120416     c                   clear                   fiar6000
149990120416     C                   MOVEL     §bttrc        WTRC
150000120404     c     karb2         chain     fiar601l
150010120411    1c                   if        %found(fiar601l) and ar6nft= 0 or
150020120411     c                             not %found(fiar601l)
150030120404     c                   eval      wfatfil='SF'
150040120411     c                   if        not %found(fiar601l)
150050120411     c                   eval      war6='N'
150060120411     c                   endif
150070120404     c                   else
150080120404     c                   eval      wfatfil='SD'
150090120404    1c                   endif
150100120404    1c                   if        d48cvb=wfatfil
150110120404     c
150120120404     c* . . . . . . . QUANTITA' DA FATTURARE
150130120411    2c                   if        §btqft<>arbqft
150140120411     c                   z-add     §btqft        arbqft
150150120404    2c                   endif
150160120404     c* . . . . . . . CLIENTE DI FATTURAZIONE e COD.TARIFFA
150170120419     C*
150180120404     C                   MOVEL     '3A'          COD
150190120404     C                   MOVEL(P)  ARBCBO        KEY
150200120404     C                   EXSR      CTABEL
150210120404     C  N30              MOVEL     TBLUNI        DS3A
150220120404     C   30              CLEAR                   DS3A
150230120413     c* assegnato
150240120404    2c                   if        §bttrc='2' or %subst(§3atb1:1:1)='A'
150250120404    3c                   if        §btksc<>ar6ksc
150260120404     c                   eval      ar6ksc=§btksc
150270120404    3c                   endif
150280120404    3c                   if        §btctr<>ar6ctr
150290120404     c                   eval      ar6ctr=§btctr
150300120404    3c                   endif
150310120418     c* Se assegnato no seconda bolla devo aggiornare su arb anche il ksc/ctr
150320120418    2c                   if        %subst(§3atb1:1:1)='A'
150330120418    3c                   if        §btksc<>savksc_a
150340120418     c                   eval      savksc_a=§btksc
150350120418    3c                   endif
150360120418    3c                   if        §btctr<>savctr_a
150370120418     c                   eval      savctr_a=§btctr
150380120418    3c                   endif
150390120418     c                   endif
150400120404   x2c                   else
150410120413     c* franco
150420120413     c*  se non codificato non faccio niente
150430120413     c                   z-add     §btksc        w0040
150440120418    3c                   if        w0040<>8888 and §btksc<>arbksc
150450120413     c                   exsr      gessd_ksc
150460120411    3c                   endif
150470120411    3c                   if        §btctr<>arbctr
150480120411     c                   eval      arbctr=§btctr
150490120411    3c                   endif
150500120404    2c                   endif
150510120404     c* . . . . . . . CAMPI DI TASSAZIONE
150520120404     c* . . . . Divisa
150530120404     c                   if        §btdiv<>ar6div
150540120404     c                   eval      ar6div=§btdiv
150550120404     c                   endif
150560120404     c* . . . . Porto
150570120404     c                   if        §btpor<>ar6por
150580120404     c                   eval      ar6por=§btpor
150590120404     c                   endif
150600120411     c* . . . . Imponibile IVA
150610120411     c                   if        §btimv<>ar6imv
150620120411     c                   eval      ar6imv=§btimv
150630120411     c                   endif
150640120411     c* . . . . Causale esenzione
150650120411     c                   if        §btcei<>ar6cei
150660120411     c                   eval      ar6cei=§btcei
150670120411     c                   endif
150680120404     c* . . . . Varie - In §SV e §VA ci sono le varie ricevute dal chiamante
150690120404     c*                 IN WSV e WVA carico le varie presenti sul fiar6/7
150700120411     C                   MOVEL     §sv(1)        ar6sv1
150710120411     C                   Z-ADD     §va(1)        ar6va1
150720120411     C                   MOVEL     §sv(2)        ar6sv2
150730120411     C                   Z-ADD     §va(2)        ar6va2
150740120411     C                   MOVEL     §sv(3)        ar6sv3
150750120411     C                   Z-ADD     §va(3)        ar6va3
150760120411     c* cancello varie su fiar7 se presenti
150770120419     C                   MOVEL     §bttrc        WTRC
150780120411     c     karb2         setll     fiar7000
150790120411     c     karb2         reade     fiar7000
150800120411     c                   dow       not %eof(fiar701l)
150810120411     C                   DELETE    FIAR7000
150820120411     c     karb2         reade     fiar7000
150830120411     c                   enddo
150840120411     c* se presenti più di 3 verie le memorizzo nell'fiar7
150850120411    2c                   if        §sv(4)<>' '
150860120411     c                   z-add     4             inx
150870120411     c                   clear                   AR7ATB
150880120411     c                   z-add     d48aas        AR7AAS
150890120411     c                   z-add     d48lnp        AR7LNP
150900120411     c                   z-add     d48nrs        AR7NRS
150910120411     c                   z-add     d48nsp        AR7NSP
150920120419     c                   movel     §bttrc        AR7TRC
150930120411     c                   dow       §sv(inx)<>*blanks or inx>20
150940120411     C                   MOVEL     §sv(inx)      ar7svn
150950120411     C                   z-add     §va(inx)      ar7van
150960120411     C                   write     fIAR7000
150970120411     c                   add       1             inx
150980120411     c                   enddo
150990120411    2C                   endif
151000120416     c* Aggiornamento FIAR6
151010120416     c* se record non esiste lo creo solo se è pieno almeno uno dei campi
151020120416    2c                   if        war6='N'
151030120416    3c                   if        ar6ksc>0 or ar6por>0 or ar6sv1<>*blanks
151040120416     c                             or ar6sv2<>*blanks or ar6sv3<>*blanks
151050120416     c                             or ar6cei<>*blanks
151060120411     c                   eval      AR6AAS=d48aas
151070120411     c                   eval      AR6LNP=d48lnp
151080120411     c                   eval      AR6NRS=d48nrs
151090120411     c                   eval      AR6NSP=d48nsp
151100120411     c                   eval      AR6TRC=§bttrc
151110120411
151120120411     c                   write     fiar6000
151130120416    3c                   endif
151140120417     c                   else
151150120416    3c                   if        ar6ksc>0 or ar6por>0 or ar6sv1<>*blanks
151160120416     c                             or ar6sv2<>*blanks or ar6sv3<>*blanks
151170120416     c                             or ar6cei<>*blanks
151180120404     c                   except    ar6sd
151190120416     c                   else
151200120416     c                   delete    fiar6000
151210120416     c                   endif
151220120416    2c                   endif
151230120404   x1c                   else
151240120404     c                   unlock    fiar601l
151250120404    1c                   endif
151260120404     c* aggiornamento file
151270120418     c* BLP
151280120404    1c                   if        errp=*blanks
151290120418     c                   eval      arbcpi=savcpi_p
151300120418     c* solo se assegnato rimetto il ksc/ctr che c'era
151310120418     c                   if        %subst(§3atb1:1:1)='A'
151320120418     c                   eval      arbksc=savksc_p
151330120418     c                   eval      arbctr=savctr_p
151340120418     c                   endif
151350120404     c                   except    arbsdblp
151360120404    1c                   endif
151370120418     c* ARB
151380120404    1c                   if        erra=*blanks
151390120416     c* P.iva DEL MITTENTE SU ARB LA AGGIORNO SOLO SE FRANCO NO DOPPIA BOLLA
151400120419     c* in caso contrario tengo la P.IVA che c'era
151410120416     c                   eval      arbcpi=savcpi_a
151420120418     c*
151430120418     c                   if        %subst(§3atb1:1:1)='A'
151440120418     c                   eval      arbksc=savksc_a
151450120418     c                   eval      arbctr=savctr_a
151460120418     c                   endif
151470120404     c                   except    arbsdarb
151480120404    1c                   endif
151490120330     c
151500120330     C                   ENDSR
151510120413     C**************************************************************************
151520120413     C*  AGGIORNAMENTi derivanti da variazioni in sede del ksc di bolla FRANCO
151530120413     C**************************************************************************
151540120413     C     GESSD_ksc     BEGSR
151550120416     C* Controllo se esiste record W di fiar4 per eventuale modifica di mittente
151560120416     C                   MOVEL     'W'           WTRC
151570120416     c                   clear                   wbl4w
151580120416     C     KARB2         CHAIN(N)  Fiar401l
151590120416     c                   if        %found(fiar401l)
151600120416     C                   MOVEL     'S'           WBL4W             1
151610120416     c                   endif
151620120416     c                   z-add     arbksc        oldksc
151630120416     c*
151640120413     c                   eval      arbksc=§btksc
151650120413     c* recupero dati del cliente da anagrafica
151660120413     C                   clear                   TIBS69DS
151670120413     C                   z-add     §btksc        I69kac
151680120413     C                   z-add     §btksc        I69kin
151690120413     C                   call      'TIBS69R'
151700120413     C                   parm                    tibs69DS
151710120413     C                   parm                    ds_cnaco
151720120413     C                   parm                    ds_cnind
151730120413     C                   parm                    ds_cnclp
151740120413     C                   parm                    ds_fncls
151750120413     c                   eval      arbrsm=bs_acorag
151760120413     c                   eval      arbinm=bs_indvia
151770120413     c                   eval      arbLOM=bs_indcit
151780120413     c                   eval      arbCAM=bs_indcae
151790120413     c                   eval      arbPRM=bs_indprv
151800120413     c                   eval      arbNZM=bs_indsta
151810120418     c* P.IVA: non aggiorno arbcpi perchè comune per blp e arb e non va bene:
151820120418     c* se bolla doppia su blp c'è la p.iva del mittente e su arb la p.iva del dest
151830120413     c* dell'aggiornamento di FNARB
151840120419     c                   if        %subst(§3atb1:1:1)='F' and §3atb2=*blanks
151850120419     c                   eval      savcpi_a=bs_indiva
151860120419     c                   endif
151870120419     c                   eval      savcpi_p=bs_indiva
151880120416     c                   exsr      up_fiar4P
151890120416     c                   exsr      up_fiar4w
151900120416     c*
151910120413     c                   endsr
151920120413     C**************************************************************************
151930120413     C* AGGIORNAMENTO RECORD P.IVA IN FIAR4  se esiste
151940120413     C**************************************************************************
151950120413     C     up_fiar4P     BEGSR
151960120413     C                   MOVEL     'P'           WTRC
151970120413     C     KARB2         CHAIN     FiAR401L                           30
151980120413     C     *IN30         IFEQ      *OFF
151990120413     c                   movel     ar4not        wcpifile
152000120413     c                   if        wcpifile <> arbcpi
152010120413     c                   movel     'T'           ar4ftr
152020120413     c                   z-add     dateu         ar4duv
152030120413     c                   z-add     dateu         ar4dtr
152040120413     C                   MOVEl     ARBCPI        AR4NOT
152050120413     C                   UPDATE    FiAR4000
152060120413     c                   else
152070120413     c                   unlock    fiar401l
152080120413     c                   endif
152090120413     C                   END
152100120413     c                   endsr
152110120416     C**************************************************************************
152120120416     C* Gestione record fiar4 record W per DATA TRASMISSIONE VAB
152130120416     C**************************************************************************
152140120416     C     up_fiar4W     BEGSR
152150120416     C* CONTROLLO SE C'E' NUOVO CODICE O VARIATO PER AGGIORNARE O ELIM
152160120416     C*  O SCRIVERE RECORD W IN ar4
152170120416     C                   MOVEL     '3Q'          COD
152180120416     C                   MOVEL(P)  ARBKSC        KEY
152190120416     C     KTAB2         CHAIN     TABEL00F                           30
152200120416     C  N30TBLFLG        IFNE      ' '
152210120416     C                   SETON                                        30
152220120416     C                   ENDIF
152230120416     C*
152240120416     C* C'E' 3Q DEVO CREARE O VARIARE
152250120416    1C  N10*IN30         IFEQ      *OFF
152260120416    2C     WBL4W         IFEQ      ' '
152270120416    3C     ARBKSC        orNE      OLDKSC
152280120416     C                   CLEAR                   DSBL4W
152290120416     C                   CLEAR                   fnlv19ds
152300120416     C                   MOVEL     DSBL4W        d19nt1
152310120416     C                   MOVEL     ARBAAS        d19AAS
152320120416     C                   MOVEL     ARBLNP        d19LNP
152330120416     C                   MOVEL     ARBNRS        d19NRS
152340120416     C                   MOVEL     ARBNSP        d19NSP
152350120416     C                   MOVEL     'W'           d19TRC
152360120416     C                   MOVEL     'T'           d19ftr
152370120416     C                   call      'FNLV19R'
152380120416     c                   parm                    fnlv19ds
152390120416    2C                   ENDIF
152400120416     C*
152410120416   X1C                   ELSE
152420120416     C* SE NO CI DEVE ESERE E PRIMA C'ERA LO DELETO
152430120416    2C     WBL4W         IFEQ      'S'
152440120416     c                   clear                   fnlv19ds
152450120416     C                   clear                   d19nt1
152460120416     c                   clear                   d19nt2
152470120416     C                   MOVEL     arbAAS        d19AAS
152480120416     C                   MOVEL     arbLNP        d19LNP
152490120416     C                   MOVEL     arbNRS        d19NRS
152500120416     C                   MOVEL     arbNSP        d19NSP
152510120416     C                   MOVEL     'W'           d19TRC
152520120416     c                   movel     'T'           d19ftr
152530120416     C                   CALL      'FNLV19R'
152540120416     C                   PARM                    fnlv19ds
152550120416    2C                   ENDIF
152560120416    1C                   ENDIF
152570120416     c                   endsr
152580130321     C**************************************************************************
152590130411     C* Invio Variazione bolla a PDA
152600130321     C**************************************************************************
152610130411     C     sr_pda        BEGSR
152620140908     c* Solo se la chiamata non è cieca:
152630140908     c                   if        d48ffr<>'E'
152640141020     c****               if        d66pda<>' ' and (*in10 or waltrabolla=' ')
152650141020     c                   if        d66pda<>' ' and *in10
152660131227     c                             and wpda_ndc>0 and wpda_dcm=0
152670131227     c* richiamo pgm per verificare che ci sia stata un'effettiva variazione
152680131227     c                   clear                   fnlva1ds
152690131227     c                   eval      IA1AAS=arbaas
152700131227     c                   eval      IA1LNP=arblnp
152710131227     c                   eval      IA1NRS=arbnrs
152720131227     c                   eval      IA1NSP=arbnsp
152730131227     c                   eval      IA1DTV=arbdtv
152740131227     c                   eval      IA1ORV=arborv
152750131227     c                   eval      IA1CVB=arbcvb
152760131227     c                   eval      IA1PRU=arbpru
152770131227     c                   movel     fnlva1ds      kpjbu
152780131227     c                   call      'FNLVA1R'
152790131227     c                   parm                    kpjba
152800131227     c                   movel     kpjbu         fnlva1ds
152810131227     c*
152820131227     c                   if        OA1ESITO='V'
152830140130     c* se filiale partita con nuova gestione telefonate richiamo pgm gestione
152840140130     c* note che si preoccuperà anche del richiamo a fidg42r
152850140130     c                   if        *in87=*off
152860140130     c                   clear                   fnlrn6ds
152870140130     c                   eval      in6mod='I'
152880140130     c                   eval      in6aas=arbaas
152890140130     c                   eval      in6lnp=arblnp
152900140130     c                   eval      in6nrs=arbnrs
152910140130     c                   eval      in6nsp=arbnsp
152920140130     c                   eval      in6pgm='FNLR48R'
152930140130     c                   eval      IN6FGS=arbifp
152940140130     c                   eval      IN6NFV=wpda_ndc
152950140130     c                   eval      IN6DFV=wpda_ddc
152960140130     c                   eval      IN6PDR=arbpdc
152970140130     c                   eval      IN6VAR=d66pda
152980140130     c                   eval      IN6CONS='S'
152990140130     c                   call      'FNLRN6R'
153000140130     C                   parm                    kpjba
153010140130     C                   parm                    fnlrn6ds
153020140130     c                   else
153030140130     c* altrimenti richiamo direttemente fidg42r
153040131227     c
153050130411     c                   z-add     arbifp        w0030             3 0
153060130411     c     w0030         chain     azorg01l
153070130411     c                   if        %found(azorg01l)
153080130411     c                   eval      og148=orgde8
153090131227     c                   if        §ogpdacon<>' '
153100130411     c                   clear                   fidg42ds
153110130411     c                   eval      CO42FGS=arbifp
153120130411     c                   eval      CO42NDC=wpda_ndc
153130130411     c                   eval      CO42DDC=wpda_ddc
153140130411     c                   eval      CO42AAS=arbaas
153150130411     c                   eval      CO42LNP=arblnp
153160130411     c                   eval      CO42NRS=arbnrs
153170130411     c                   eval      CO42NSP=arbnsp
153180130411     c                   eval      CO42CMT='N'
153190130411     c                   movel     fidg42ds      kpjbu
153200130411     c                   call      'FIDG42R'
153210130411     c                   parm                    kpjba
153220130411     c                   endif
153230130411     c                   endif
153240140130     c                   endif
153250131227     c                   endif
153260130411     c                   endif
153270140908     c                   endif
153280130411     c                   endsr
153290141021     C**************************************************************************
153300141021     C* Gestione finestra per richiesta dirottamento
153310141021     C**************************************************************************
153320141021     C     ges_wdw02     BEGSR
153330141021     c                   clear                   winf12
153340141022     c                   eval      atrf21=rimm
153350141125     c                   setoff                                       782879
153360141125     c* iN partenza non si può mai variare senza il dirottamento --> disabilito l'F8
153370141125     c                   if        not *in10
153380141125     c                   seton                                        79
153390141125     c                   endif
153400141022     c                   eval      w02txt='La Linea di Arrivo desunta è diversa-
153410141022     c                              dalla Linea di Arrivo della bolla'
153420141029     c                   exsr      sr_NoAbiDir
153430141021    6c                   do        *hival
153440141021     c                   exfmt     lr48w02
153450141113     c  n78              setoff                                       28
153460141113     c  n78              clear                   w02msg
153470141021     c                   select
153480141021     c* F21=Richiesta dirottamento
153490141021     c                   when      *inkv
153500141021     c                   eval      wdir='1'
153510141021     c* F12=Ritorno
153520141021     c                   when      *inkl
153530141021     c                   eval      winf12='1'
153540141021     c                   endsl
153550141106     c                   if        *inkv or *inkh or *inkl
153560141021     c                   leave
153570141021     c                   endif
153580141021    6c                   enddo
153590141113     c                   setoff                                       7828
153600141021     c                   endsr
153610141029     C**************************************************************************
153620141029     C* Verifica condizioni che disabilitano F21=Dirottamento
153630141029     C**************************************************************************
153640141029     C     sr_NoAbiDir   BEGSR
153650141029
153660141029     c*******************************************
153670141029     c* Casi di disabilitazione del Dirottamento:
153680141029     c*******************************************
153690141029     c* - BOLLA LEGATA
153700141029     c* Se la bolla è legata il dirottamento può essere richiesto solo sull'ultima figlia
153710141029     c     karb          chain     fnlbl02l
153720141029     c                   if        %found(fnlbl02l)
153730141029     c* disattivo l'f21 ed emetto messaggio per avvisare che il dirottamento deve essere
153740141029     c* fatto sulla figlia
153750141029     c                   seton                                        7828
153760141029     c                   eval      w02msg ='Bolla legata:il dirottamento può -
153770141029     c                             essere richiesto solo sulla figlia'
153780141029     c                   clear                   atrf21
153790141029     c                   leavesr
153800141029     c                   endif
153810141029
153820141029     c* - PRESENZA DISPO DA ESAMINARE
153830141029     c**   kidc1         setll     tiidc01l
153840141029     c***                if        %equal(tiidc01l)
153850141029     c                   clear                   fnlry09ds2
153860141029     c                   eval      ILRY09TCH='T'
153870141029     c                   eval      ILRY09AAS=d48aas
153880141029     c                   eval      ILRY09LNP=d48lnp
153890141029     c                   eval      ILRY09NRS=d48nrs
153900141029     c                   eval      ILRY09NSP=d48nsp
153910141029     c                   eval      kpjbu=fnlry09ds2
153920141031     c                   call      'FNLRY09C'
153930141029     c                   parm                    kpjba
153940141029     c                   eval      fnlry09ds2=kpjbu
153950141029     c                   if        OLRY09ESIT='1'
153960141029     c                   seton                                        7828
153970141029     c                   eval      w02msg ='Presenti Disposizioni di Consegna-
153980141029     c                              da elaborare: non è possibile dirottare'
153990141029     c                   clear                   atrf21
154000141029     c                   leavesr
154010141029     c                   endif
154020141029     c
154030141029     c* - PRESENZA FERMO DEPOSITO
154040141029     c                   if        vidffd<>*blanks
154050141029     c                   seton                                        7828
154060141125     c                   if        not *in79
154070141125     c                   eval      w02msg='Presente Fermo Deposito: F8 per non -
154080141029     c                             dirottare, F12 per correggere'
154090141125     c                   else
154100141125     c                   eval      w02msg='Presente Fermo Deposito: F12 per cor-
154110141125     c                             reggere'
154120141125     c                   endif
154130141029     c                   clear                   atrf21
154140141029     c                   endif
154150141112     c* - PRESENZA giacenza aperta
154160141112     c                   if        *in04
154170141112     c                   seton                                        7828
154180141112     c                   eval      w02msg='Presente Giacenza aperta: non è poss-
154190141112     c                             ibile dirottare'
154200141112     c                   clear                   atrf21
154210141112     c                   endif
154220141029     c                   endsr
154230150414     C**************************************************************************
154240150414     C* controlli per cons. particolare "Z"=EXPO
154250150414     C**************************************************************************
154260151119     C*    sr_chkexpo    BEGSR
154270150414     c* Se Expo errore se bolla con contrassegno
154280151119     c*                  if        §3afca=1
154290151119     C*                  MOVEL     MSG(159)      VIDMSG
154300151119     C*                  SETON                                        9028
154310151119     C*                  leavesr
154320151119     c*                  endif
154330150414     c* se expo errore se non è franco
154340151119     c*                  if        %subst(§3atb1:1:1)<>'F'
154350151119     C*                  MOVEL     MSG(160)      VIDMSG
154360151119     C*                  SETON                                        9028
154370151119     C*                  leavesr
154380151119     c*                  endif
154390150414     c* errore se lna non è in tabella
154400151119     c*                  move      v1clna        w003a
154410151119     c*                  if        %lookup(w003a:fil_expo)=0
154420151119     C*                  MOVEL     MSG(161)      VIDMSG
154430151119     c*                  eval      vidmsg=%trim(vidmsg) + ' ' + w003a
154440151119     C*                  SETON                                        9028
154450151119     C*                  leavesr
154460151119     c*                  endif
154470151119     c*                  endsr
154480151016     C*--- Registro peso e volume usati perfatturaz. in FIAR5 --------*
154490151016     C     REGPesivol    BEGSR
154500151016     c                   clear                   dar5fat
154510151016     c                   Eval      kar5trd='FAT'
154520151016     c     kar5          Chain     Fiar531c
154530151016     c                   If        %Found(Fiar531c)
154540151016     c                   Movel     ar5uni        dar5fat
154550151016     c                   endif
154560151016     c* Se tassata
154570151016    1c                   if        ar6imv>0
154580151016     c* Volume
154581170420     c                   movel     arbvlf        §ar5vltas
154582170420     c                   movel     arbfvf        §ar5fvtas
154590170420     c                   if        tasvlt>0  and
154591170420     c                             PesVolIMV=Tassatoimv
154600160530     c                   z-add     tasvlt        §ar5vltas
154610160530     c                   movel     tasfvt        §ar5fvtas
154620170420     c*                  else
154630170420     c*                  movel     arbvlf        §ar5vltas
154640170420     c*                  movel     arbfvf        §ar5fvtas
154650160530     c                   endif
154660151016     c* Peso
154670151016     c                   movel     arbpkf        §ar5pktas
154680151016     c                   movel     'R'           §ar5fptas
154690151016     c
154700151016     c* Se il totale imponibile aggiornato senza arrotondamento
154710151016     c*  è = a quello memorizzato al momento del richiamo al
154720151016     c*   TNSF20R--> memorizzo se usato peso VDL (-tara)
154730151016    2c                   if        PesVolIMV=Tassatoimv  and
154740151016     c                             PesVolspc='S'
154750151016     c                   movel     PesVolPkcn    §ar5pktas
154760151016    3c                   if        arbncp=arbncl
154770151016     c                   movel     'T'           §ar5fptas
154780151016     c                   else
154790151016     c                   movel     'Z'           §ar5fptas
154800151016    3c                   endif
154810151016     c
154820151016    2c                   endif
154830151016     c                   eval      §AR5DATAS=%dec(%date())
154840151016     c                   eval      §AR5HMTAS=%dec(%time())
154850151016     c                   eval      §AR5PRTAS=knmus
154860151016     c                   eval      §AR5NJTAS=knmtd
154870151016     c*
154880151016    1c                   endif
154890151016     c
154900151016      * aggiorno Fiar5 record "FAT"
154910151016if  1c                   If        %Found(Fiar531c)
154920151016     c* Se ho eliminato la tassazione  deleto
154930151016     c                   if        ar6imv=0
154940151016     c                   if        recname='FIAR5000'
154950151016     c                   delete    fiar500s
154960151016     c                   else
154970151016     c                   delete    fiar5p0s
154980151016     c                   endif
154990151016     c                   else
155000151016     c                   Movel(p)  dar5fat       Ar5uni
155010151016     c                   eval      ar5dac=%dec(%date())
155020151016     c                   eval      ar5orc=%dec(%time())
155030151016     c                   if        recname='FIAR5000'
155040151016     c                   Update    Fiar500s
155050151016     c                   else
155060151016     c                   Update    Fiar5p0s
155070151016     c                   endif
155080151016     c                   endif
155090151016      * scrivo   Fiar5 record "FAT"
155100151016   x1c                   Else
155110151016    2c                   if        ar6imv>0
155120151016     c                   Clear                   Fiar500s
155130151016     c                   Clear                   Fiar5p0s
155140151016     c                   Eval      Ar5Aas = ArbAas
155150151016     c                   Eval      Ar5Lnp = ArbLnp
155160151016     c                   Eval      Ar5Nrs = ArbNrs
155170151016     c                   Eval      Ar5Nsp = ArbNsp
155180151016     c                   Eval      Ar5Trd = 'FAT'
155190151016     c                   Movel(p)  dar5fat       Ar5uni
155200151016     c                   eval      ar5dac=%dec(%date())
155210151016     c                   eval      ar5orc=%dec(%time())
155220151016     c                   Eval      AR5FT1='T'
155230151016     c                   Eval      AR5DT1=99999999
155240151016     c                   Eval      AR5FT2='T'
155250151016     c                   Eval      AR5DT2=99999999
155260151016     c                   Eval      AR5FT3='T'
155270151016     c                   Eval      AR5DT3=99999999
155280151016     c* Verifico dove si trova TITAS per la write di fiar5
155290151016     c     karb          chain     titaS30c
155300160104     c                   if        %found(titas30c)
155310151019     c                   if        recnamet='TITAS000'
155320151016     c                   Write     Fiar500s
155330151016     c                   else
155340151016     c                   Write     Fiar5p0s
155350151016     c                   endif
155360160104     c                   else
155370160104     c                   Write     Fiar500s
155380160104     c                   endif
155390151016    2c                   EndIf
155400151016    1c                   EndIf
155410151016     c
155420151016     c                   ENDSR
155430141020     C**************************************************************************
155440141020     C* Scrittura TIIDC per Caricamento Disposizione di dirottamento
155450141020     C**************************************************************************
155460141020     C     sr_tiidc      BEGSR
155470141020      /free
155480141020       clear tiidc000;
155490141020       exsr repnum;
155500141020       if esito = *blanks;
155510141020          IDCPRG=kprg                         ;
155520151201       // Se progressivo non reperito lo lascio a 0:
155530151201       // per le dispo di reso e dirottamento non abbiamo la necessità di
155540151201       // avere il progressivo dal momento che per una stessa spedizione
155550151201       // non è ammessa la presenza di + dispo se una di queste è dirottamento o reso
155560151201       // Abbiamo quindi preferito scrivere a 0 il progressivo piuttosto che perdere la
155570151201       // dispo. Questo discorso non val per le dispo che riceviamo da web
155580151201       //else;
155590151201       //   leavesr;
155600141020       endif;
155610141020       IDCINSDATA   = %int( %subst( %char( %dec( %timestamp() ) )
155620141020                                    : 1 : 14 ) );
155630141020       //IDCtimestn   = ;
155640141020       IDCMODACC='B'                       ;
155650141020       //IDCIP = ;
155660141020       //IDCIDCLI = ;
155670141023       if *in10;
155680141023          IDCTIPOCLI= 'D' ;
155690141023       else;
155700141023          IDCTIPOCLI= 'M' ;
155710141023       endif;
155720141020       IDCWAAS   = arbaas;
155730141020       IDCWLNP   = arblnp;
155740141020       IDCWNRS   = arbnrs;
155750141020       IDCWNSP   = arbnsp;
155760141020       IDCAAS    = arbaas;
155770141020       IDCLNP    = arblnp;
155780141020       IDCNRS    = arbnrs;
155790141020       IDCNSP    = arbnsp;
155800141021       IDCFGS    = lnacons;
155810141020       IDCTIPODIS = '3';
155820141020       //IDCMAILAVV     ;
155830141020       //IDCTELAVV  ;
155840141020       //IDCALERT
155850141021          IDCRSD   = vidrsd;
155860141021          IDCIND   = vidind;
155870141021          IDCCAD   = vidcad;
155880141021          IDCLOD   = vidlod;
155890141021          IDCPRD   = vidprd;
155900141021          IDCNAZ   = vidnzd;
155910141020       // bolla da dirottare
155920141021          IDCNEWLNA=d_o95lna;
155930141021          IDCFLA   ='S';
155940141020       IDCELAFLG='S';
155950141022       IDCELAUTE=knmus;
155960141023       idcref=Vidref;
155970141023       idcteld=vidteld;
155980141020
155990141020         write tiidc000;
156000141020       endsr;
156010141020        //-----------------------------------------------------------------------*
156020141020        // Reperimento progressivo richiesta da numeratore 650
156030141020        //-----------------------------------------------------------------------*
156040141020        begsr RepNum;
156050141020
156060141022          annocur  = %subdt(%date():*years);
156070141020          clear esito;
156080141020          $finenum=*off;
156090141020          clear trul33ds;
156100141020
156110141020          I33TLa = 'L';
156120141020          I33Ope = *ZEROS;
156130141030          I33CNu = 651;
156140141020          I33Num = 1;
156150141020
156160141020          KPJBU = TRUL33DS;
156170141020
156180141020          dow $finenum=*off   ;
156190141020             Trul33R(kpjba);
156200141020
156210141020             TRUL33DS = KPJBU;
156220141020
156230141020             if O33Err <> *zeros;
156240141020               Esito = '2';
156250141020               $finenum=*on;
156260141020             else;
156270141020               w0090=o33nri;
156280141020               kprg=%dec(%subst(%editc(annocur:'X'):3:2):2:0)*1000000000+w0090;
156290141020               setll kprg tiidc02l;
156300141020        // Se numero già presente nel file TIIDC ne cerco un altro
156310141020               if not %equal;
156320141020                  $finenum=*on;
156330141020               endif;
156340141020             endif;
156350141020          enddo;
156360141020
156370141020          endsr;
156380160318        //-----------------------------------------------------------------------*
156390160318        // Aggiornamenti specifici per "VEDI PACCO DPD"
156400160318        //-----------------------------------------------------------------------*
156410160318        begsr sr_VediPDpd ;
156420160318        // Per prima cosa chaino fiar4 record "I" per verificare se si tratta di
156430160318        // "VEDI PACCO"
156440160318             chain(n) (arbaas:arblnp:arbnrs:arbnsp:'I') fiar401l ;
156450160318             if %found(fiar401l);
156460160318                dsbl4i=ar4not;
156470160318              if §b4trd='1';
156480160318        // Se "VEDI PACCO" con giro forzato come da tabella NSD:
156490160318                 chain (arbaas:arblnp:arbnrs:arbnsp) fiarg01l;
156500160318                 if %found(fiarg01l) and argcgi=§nsdcgi;
156510160318        // Memorizzo che, appena aggiornata la bolla, dovrò richiamare il pgm per togliere
156520160318        // la sped. dalla distinta
156530160318        // (Solo se manutenzione fatta in arrivo)
156540160318                  if *in10 and arbndc>0 ;
156550160318                    wtogli='1'      ;
156560160318                  endif;
156570160318        // Cancello il giro forzato
156580160318                    clear argcgi;
156590160318                    clear argtgi;
156600160318                    update fiarg000;
156610160318                 else ;
156620160318                    unlock fiarg01l ;
156630160318                 endif;
156640160318        // Aggiorno il flag Traduttore da 1 a 2
156650160318                 clear  fnlv19ds ;
156660160318                 D19AGG='E';
156670160318                 D19FTR='T';
156680160318                 D19AAS=arbaas;
156690160318                 D19LNP=arblnp;
156700160318                 D19NRS=arbnrs;
156710160318                 D19NSP=arbnsp;
156720160318                 D19TRC='I';
156730160318                 if *in10;
156740160318                    §b4trd='2';
156750160318                 else;
156760160318                    §b4trd='3';
156770160318                 endif;
156780160318                 D19NT1=dsbl4i;
156790160318                 fnlv19r(fnlv19ds);
156800160318              endif;
156810160318             endif;
156820160318        endsr;
156830160318        //-----------------------------------------------------------------------*
156840160318        // Toglie da distinta spedizione "VEDI PACCO DPD"
156850160318        //-----------------------------------------------------------------------*
156860160318        begsr sr_TogliVDpd ;
156870160318                    clear  fnlr98ds;
156880160318                    I98AAS    = arbaas;
156890160318                    I98LNP    = arblnp;
156900160318                    I98NRS    = arbnrs;
156910160318                    I98NSP    = arbnsp;
156920160318                    I98VIDEO  = 'N';
156930160318                    I98npg    = 4  ;
156940160318                    I98fgs    = arbifp ;
156950160318                    I98ndc    = arbndc ;
156960160331                    I98ALCDST = ' ' ;
156970160331                    I98CTLDST = ' ' ;
156980160330                    I98STRCMT = '  ';
156990160318                    kpjbu = fnlr98ds ;
157000160330                    fnlr98c(kpjba);
157010160401        //          if o98esito='1';
157020160401        //              msgforzatxt=O98MSGERR ;
157030160401        //          endif;
157040160318        endsr;
157050141020      /end-free
157051170411        //-----------------------------------------------------------------------*
157052170411        // Memorizzazione dati fattura annullata/stornata
157053170411        //-----------------------------------------------------------------------*
157054170411        begsr sr_Fnfta     ;
157055170411        FTAAAS = ar6aas    ;
157056170411        FTALNP = ar6lnp    ;
157057170411        FTANRS = ar6nrs    ;
157058170411        FTANSP = ar6nsp    ;
157059170411        FTAMGS = arbmgs    ;
157060170411        FTADIV = ar6div    ;
157061170411        FTAALI = ar6ali    ;
157062170411        FTACEI = ar6cei    ;
157063170411        FTAIFT = ar6ift    ;
157064170411        FTAFIV = ar6fiv    ;
157065170411        FTANFT = ar6nft    ;
157066170411        FTADFT = ar6dft    ;
157067170411
157068170411        write  fnfta000    ;
157069170411        endsr;
157070170411
157071940321     C*---------------------------------------------------------------*
157072041007     Ofnblp000  E            ARBDblp
157080041007     O                       arbrsd
157090041007     O                       arbind
157100041007     O                       arbcad
157110041007     O                       arblod
157120041007     O                       arbprd
157130041007     O                       arbnzd
157140041007     O                       arbfin
157150041007     O                       arbcpi
157160041007     O                       arbpkb
157170041007     O                       arbpkf
157180041007     O                       arbvlb
157190041007     O                       arbfvb
157200041007     O                       arbvlf
157210041007     O                       arbfvf
157220041007     O                       arbffd
157230041007     O                       arbcts
157240041022     Ofnblp000  E            ARBKblp
157250041022     O                       arbcbo
157260041022     Ofnblp000  E            ARBGblp
157270041022     O                       arbdcr
157280041022     O                       arbhcr
157290041022     O                       arbtcr
157300041022     O                       arbgc1
157310041022     O                       arbgc2
157320041022     O                       arbtc1
157330041022     O                       arbtc2
157340041022     O                       arbft3
157350041025     Ofnblp000  E            ARBMblp
157360041025     O                       arbksc
157370041025     O                       arbrsm
157380041025     O                       arbinm
157390041025     O                       arbcam
157400041025     O                       arblom
157410041025     O                       arbprm
157420041025     O                       arbnzm
157430041025     O                       arbfap
157440041025     O                       arbcpi
157450041025     O                       arbCTR
157460041025     O                       arbfvf
157470041025     O                       arbvlf
157480041027     Ofnblp000  E            ARBTblp
157490041027     O                       arbvas
157500041027     O                       arbias
157510041027     O                       arbvmd
157520041027     O                       arbvad
157530041027     O                       arbqft
157540041027     O                       arbctr
157550041028     Ofnblp000  E            RMblp
157560041028     O                       arbrma
157570041028     O                       arbnas
157580071025     Ofnblp000  E            SoloCPIP
157590071025     O                       arbcpi
157600120404     Ofnblp000  E            ARBsdblp
157610120404     O                       arbpkf
157620120404     O                       arbvlf
157630120404     O                       arbfvf
157640120416     O                       arbksc
157650120416     O                       arbctr
157660120416     O                       arbrsm
157670120416     O                       arbinm
157680120416     O                       arbLOM
157690120416     O                       arbCAM
157700120416     O                       arbPRM
157710120416     O                       arbNZM
157720120416     O                       arbcpi
157730120531     o                       arbias
157740120531     o                       arbvas
157750041007     Ofnarb000  E            ARBDarb
157760041025     O                       arbrsd
157770041007     O                       arbind
157780041007     O                       arbcad
157790041007     O                       arblod
157800041007     O                       arbprd
157810041007     O                       arbnzd
157820041007     O                       arbfin
157830041007     O                       arbcpi
157840041007     O                       arbpkb
157850041007     O                       arbpkf
157860041007     O                       arbvlb
157870041007     O                       arbfvb
157880041007     O                       arbvlf
157890041007     O                       arbfvf
157900041007     O                       arbffd
157910041007     O                       arbcts
157920041022     Ofnarb000  E            ARBKarb
157930041022     O                       arbcbo
157940041025     Ofnarb000  E            ARBGarb
157950041022     O                       arbdcr
157960041022     O                       arbhcr
157970041022     O                       arbtcr
157980041022     O                       arbgc1
157990041022     O                       arbgc2
158000041022     O                       arbtc1
158010041022     O                       arbtc2
158020141223     O                       arbfbc
158030141223     O                       arbcmc
158040041025     Ofnarb000  E            ARBMarb
158050041025     O                       arbksc
158060041025     O                       arbrsm
158070041025     O                       arbinm
158080041025     O                       arbcam
158090041025     O                       arblom
158100041025     O                       arbprm
158110041025     O                       arbnzm
158120041025     O                       arbfap
158130041025     O                       arbcpi
158140041025     O                       arbCTR
158150041025     O                       arbfvf
158160041025     O                       arbvlf
158170041027     Ofnarb000  E            ARBTarb
158180041027     O                       arbvas
158190041027     O                       arbias
158200041027     O                       arbvmd
158210041027     O                       arbvad
158220041027     O                       arbqft
158230041027     O                       arbctr
158240041028     Ofnarb000  E            RMarb
158250041028     O                       arbrma
158260041028     O                       arbnas
158270071025     Ofnarb000  E            SoloCPIA
158280071025     O                       arbcpi
158290120404     Ofnarb000  E            ARBsdarb
158300120404     O                       arbpkf
158310120404     O                       arbvlf
158320120404     O                       arbfvf
158330120416     O                       arbksc
158340120416     O                       arbctr
158350120416     O                       arbrsm
158360120416     O                       arbinm
158370120416     O                       arbLOM
158380120416     O                       arbCAM
158390120416     O                       arbPRM
158400120416     O                       arbNZM
158410120416     O                       arbcpi
158420120531     o                       arbias
158430120531     o                       arbvas
158440120411     ofiar6000  e            AR6SD
158450120411     o                       AR6KSC
158460120411     o                       AR6CTR
158470120411     o                       AR6DIV
158480120411     o                       AR6POR
158490120411     o                       AR6SV1
158500120411     o                       AR6VA1
158510120411     o                       AR6SV2
158520120411     o                       AR6VA2
158530120411     o                       AR6SV3
158540120411     o                       AR6VA3
158550120411     o                       ar6imv
158560120411     o                       ar6cei
158570041020     Ofiqdt000  E            AGGqdt
158580041020     O                       qdtnbnbr
158590930507**         GGG
1586009803091234567
158610050601** cmdt
158620050601OVRDBF FILE(TITA430C) TOFILE(
158630151016OVRDBF FILE(FIAR531C) TOFILE(
158640151016OVRDBF FILE(TITAS30C) TOFILE(
158650941014**
158660941102Spedizione al momento non disponibile: gia' utilizzata da un altro utente     01
158670980406Peso mancante                                                                 02
158680980406Volume mancante                                                               03
158690051129Variazione non eseguibile: bolla allocata da altro utente. Attendi e riprova
158700051129Impossibile inserire un volume inferiore a quello parzialmente rilevato da VDL05
158710941102Data formalmente errata                                                       06
158720941102Data consegna richiesta non puo' essere inferiore della data spedizione       07
158730941102Ora richiesta formalmente errata                                              08
158740941102Se immesso tipo immettere anche ora o data consegna richiesta                 09
158750941102Giorno di chiusura errato                                                     10
158760000510Consegna particolare errata o non utilizzabile                                11
158770941102I giorni di chiusura non possono essere uguali
158780941102Il destinatario non puo' essere sempre chiuso!!!?!!
158790941102Le consegne particolari non possono essere uguali
158800051117Impossibile modificare Cons.Part. " " :immessa da partenza no gestib.in arriv 15
158810941102Codice Bolla errato o non utilizzabile
158820941102Codice bolla esistente e nuovo non possono essere uguali
158830941102Impossibile trasformare una bolla da non recupero in recupero o viceversa
158840941213Impossibile effettuare cambi di porto: utilizzare l'apposito programma
158850941213Il codice bolla non ammette il C/Assegno                                      20
158860941102Il codice bolla vuole il C/Assegno: inserire almeno l'importo
158870941102Impossibile variare i campi relativi al C/Assegno perche' gia' incassato
158880071025Codice Fiscale obbligatorio per cliente ITALIA.  Enter per forzare            23
158890131002Mancano i dati passati per il tipo di aggiornamento richiesto                 24
158900941102Se immessa la valuta immettere anche l'importo o eliminare entrambi           25
158910941102Valuta errata
158920000510Non ammessi importi con decimali per la valuta indicata                       27
158930000510Se modificata la divisa, ridigitare il relativo importo                       28
158940941102Impossibile immettere le particolarita' C/Assegno per bolla senza C/A
158950131008$$Sono ammesse solo spedizioni in  FRANCO  per le  POSTE                        30
158960131008Causale variazione errata per il tipo di dati passati                         31
158970000510Spedizione POSTE : peso superiore a xxxxxx,x kg.  Enter per forzare!          32
158980941103Filiale Cliente inesistente
158990941103Per FATTURA IMMEDIATA impossibile specificare codice di altra filiale
159000941103Codice non ammesso se cliente di altra filiale                                35
159010941103Codice cliente errato o inesistente
159020941103Codice cliente non utilizzabile: bloccato in Piano dei Conti
159030941103Se Segue Fattura impossibile indicare il codice 9999
159040011210Importo da Ass. > del limite imposto xxxxxxxxxx,xxx EUR. TEL.IN SEDE X AUTOR. 39
159050941209Importo da Assicurare = 0 : ENTER per prendere il massimo importo da Tariffa  40
159060941103Impossibile eseguire la tassazione: cliente non dell'area
159070941209Importo da Assicurare obbligatorio, come indicato nella tariffa del cliente   42
159080011205Importo da Assicurare da non immettere, come indicato nella tariffa del cli.  43
159090011205Importo da Assicurare SUPERIORE al mandato esposto in tariffa:ENTER X FORZARE 44
159100011205Valore merce dichiarato obbligatorio                                          45
159110941209Immessa Varia "R" per 8888 o 9999 :obbligatorio l'imp.Assicur.Se 0 da tariffa 46
159120941209Codice cliente di altra filiale: F9 per forzare l'immissione
159130941209Sigla varia inesistente                                                       48
159140941209Varia "G" non ammessa                                                         49
159150941209Varia "G" obbligatoria                                                        50
159160941209Importo varia obbligatorio                                                    51
159170000510Spedizione D.P.D. : non ammesso un peso superiore a xxxxxx,x kg.              52
159180990702Non e' stata trovata la tariffa per la tassazione                             53
159190941212Provvigione a zero: non indicata la percentuale in tariffa                    54
159200941212Errore in tassazione                                                          55
159210011018Se eliminato importo C/A eliminare anche il Tipo Incasso e Particolar.        56
159220071025Partita IVA  obbligatoria per cliente ITALIA.  Enter per forzare              57
159230051129Nuovo volume da FATT. inferiore al volume parziale VDL: sara' sostituito      58
159240950420Non e' possibile modificare il tipo bolla: assegnato gia' incassato           59
159250941222Non e' possibile eliminare i dati della fattura se era un PREPAGATO           60
159260941223Non trovata spedizione                                                        61
159270941223Immesso codice cliente: l'indirizzo sara' ignorato                            62
159280950406Anagrafica mittente non completa in Piano dei Conti                           63
159290950406Manca la provincia in anagrafica mittente: inserirla!!!                       64
159300950407Aggiunto contrassegno: aggiungere anche varia G. Enter per forzatura          65
159310950407Eliminato contrassegno: eliminare anche varia G. Enter per forzatura          66
159320950522Partita IVA obbligatoria se bolla diretta all'estero con attraversam. dogana  67
159330950424Q.ta'da fatturare non inseribile per tariffa a valore:assunto importo da assic68
159340991012Azzerata q.tà da fattur.:azzerare anche gli importi di tassazione             69
159350950426Variato cliente o importo da assic.:azzerare q.tà da fattur. e tot.imponibile.70
159360991012Variato cliente o importo da assic.:azzerare q.tà da fattur. e tassazione     71
159370950426Azzerata q.tà da fattur.e tariffa a valore:azzerare anche il tot. imponibile  72
159380961204Impossibile inserire il C/Assegno in bolle di RESO
159390061024Rif. Mittente obbligatorio se destinatario estero con attraversamento dogana  74
159400970110Natura merce obbligatoria                                                     75
159410970826ATTENZIONE!LArrivo desunta, diversa da bolla ma non verra'variata.ENTER forza
159420970808ATTENZIONE! Inoltro modificato. Enter per continuare                          77
159430980309Impossibile inserire il C/Assegno in questa bolla legata                      78
159440011205Importo da Assicurare non ammesso                                             79
159450011210Importo da Assicurare > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER PER FORZARE  80
159460991112Impossibile fare fattura immediata a cliente di altro network                 81
159470990127Impossibile inserire importo da assicurare:bolla su C.A.in Liquidaz.Transatt. 82
159480990419Aggiunto Imp.Assicur.: aggiungere anche varia R. ENTER per forzare            83
159490990419Eliminato Imp.Assicur.: eliminare anche varia R. ENTER per forzare            84
159500120525Cli 8888/9999:se ImportoAssicurare e TotImponibile InserireVaria"R"
159510990419Modificato Imp.Assicur: controllare varia R . ENTER per forzare               86
159520060221Importo da assicurare INFERIORE a quanto previsto dalla legge                 87
159530990701Per i Prepagati, Numero fattura deve essere maggiore di XXXXXX                88
159540990702Prepagato: premere F14 per tassare o inserire tassazione fino all'imponibile  89
159550991006Per codice cliente 8888 o 9999 senza tariffa è obbligatoria la divisa xxx     90
159560011210Importo C/Assegno SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR              91
159570061107Aggiornamento effettuabile soltanto in PARTENZA su spedizioni in Prepagato
159580011010Ammessi importi con massimo __ decimali per la divisa indicata                93
159590991124Divisa obbligatoria se immesso almeno un importo di tassazione                94
159600061107Aggiornamento effettuabile soltanto in ARRIVO su spedizioni Fattura Immediata 95
159610131008$$Bolla cliente POSTE: non si puo' trasformare in C/Servizio                    96
159620131008$$Bolla cliente POSTE: spese C/A sempre a carico del mittente                   97
159630010412Tipo inoltro "D"=Disagiato non ammesso per questo cap/località                98
159640070508Impostare tipo CONTATTO solo se immesso il Fermo Deposito                     99
159650011011Divisa obbligatoria                                                           00
159660011017Manca la varia 'N'                                                            01
159670011102Divisa scaduta                                                                02
159680011120Spedizione giacente: importo C/A non valido                                   03
159690011210Valore merce dichiarato INFERIORE a xxxxxxxxxx,xxx EUR                        04
159700011210Valore merce dichiarato SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR        05
159710011210Valore merce dichiarato > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE  06
159720011206Gli importi inseriti SUPERANO il limite fatturabile, xxxxxxxxxx,xxx xxx       07
159730011210Importo C/Assegno INFERIORE a xxxxxxxxxx,xxx EUR                              08
159740011210Importo C/Assegno > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE        09
159750011206Importo varia SUPERA il limite fatturabile, xxxxxxxxxx,xxx xxx                10
159760020508Discordanza tra codice bolla e codice cliente mittente
159770020531Destinatario               : immettere la relativa consegna particolare       12
159780051116Non è possibile indicare contemporaneamente cons.particolari 'S' e 'X'        13
159790021121Immettere codice tariffa della relativa cartello FedEx                        14
159800021121Non ammessa tariffa FedEx Documenti: peso superiore a Kg. xxxxxx,x            15
159810021121ATTENZIONE! Bolla con Tar.FedEx Doc.e peso > Kg. xxxxxx,x: ENTER X FORZARE    16
159820021129Immettere consegna particolare "S"                                            17
159830030124Immettere il nominativo                                                       18
159840021129Immettere il n. di telefono della persona contattata                          19
159850030124Immettere il motivo                                                           20
159860051129Peso rilevato totalmente da VDL.  Si può inserire solo il peso = a peso VDL   21
159870030123Almeno una delle consegne particolari deve essere "S"                         22
159880051129Impossibile inserire un peso inferiore a quello parzialmente rilevato da VDL  23
159890030124Il n. dei bancali non ritirati non può superare il n. dei bancali bollettati  24
159900030124Immettere il n. dei bancali non ritirati                                      25
159910031001Non è possibile modificare la consegna richiesta                              26
159920040123Spedizione "monovaria". Non inserire altre varie se non immessa la varia " "  27
159930040330La data consegna richiesta deve essere inferiore alla eseguibilità disp.giac. 28
159940040330Data consegna richiesta superiore a ___ giorni                                29
159950131008Codice cliente o linea cliente non utilizzabile                               30
159960041015Prevista consegna ad Uffici:consegna per Appuntam. o a Supermerc. non ammessa 31
159970110512C/A  SUPERIORE al limite xxxxxxxxxx,xxx EUR non ammesso per bancari BRT       32
159980050414Impossibile trasformare prepagato in C/S                                      33
159990061116Modificata ragione sociale DESTINATARIO: controllare correttezza Cod.FIScale  34
160000050603Non ammessi inserimenti di importo da assicurare: bolla non coperta da mandato35
160010141110Variazione non effettuabile:salvataggio in corso. Riprovare (File "        ") 36
160020051220Non è possibile cambiare il mittente da DPD a non DPD e viceversa             37
160030060131Utente non abilitato ad inserire la consegna particolare " "                  38
160040070312Modifica di DataConsegnaRichiesta POSTICIPATA in chiusura distinta:ENTERavanza
160050070308Data consegna richiesta OBBLIGATORIA insieme al Tipo e/o all'Ora
160060070312Consegna Richiesta non modificabile: giacenza aperta o sped.con blocco "G"    41
160070070330Modifica di DataConsegnaRichiesta POSTICIPATA in chiusura distinta            42
160080070508Impostato Fermo Dep.DA CONTATTARE ma presente ConsegnaRich.in bolla:ENTR forza 43
160090070910Togliere Cons.Particolare X-xxxxxxxx:non immessa in partenza e non da Destinat44
160100071025Immettere codice fiscale o partita IVA o uscire con F12                       45
160110091124Inserimento/aumento ImpAssic x bolla non coperta da mandato:inviare MAIL.Forza46
160120091124Volume rilevato totalmente da VDL. Si può inserire solo il volume = al VDL    47
160130091124Impossibile inserire un volume inferiore a quello parzialmente rilevato da VDL48
160140120508P.Iva "$$" non ammessa per fatture emesse in filiale                          49
160150130930Indirizzo e-mail formalmente errato                                           50
160160131016Eseguita tassazione su bolla contabilizzata:non ammesso F6. F12 per uscire    51
160170131016Non effettuate variazioni: non ammesso F6=Conferma. Premere F12 per uscire    52
160180140408Il Fermo Deposito deve essere autorizzato dal Mittente                        53
160190141029Immessa Data Cons. Richiesta ma presente Fermo Deposito in bolla: ENTER forza 54
160200160204Presenti Dispo di Dirott./Reso ancora da elaborare: Fermo Deposito non ammesso55
160210160204Presente Dispo di Dirott./Reso ancora da elaborare: variazione non ammessa    56
160220141112Presente Giacenza aperta: Enter per Forzare                                   57
160230150210Consegna part. per Appuntamento non valida: immessa data consegna "Tassativa" 58
160240150413Spedizione per EXPO: non ammesso il Contrassegno                              59
160250150414Spedizione per EXPO possibile solo per bolle Franco                           60
160260150414Spedizione per EXPO non ammessa per la linea di arrivo                        61
160270151221Spedizione non manutenzionabile per questa Causale Variazione                 62
160280170130Manca tabella Limiti per Tipo Servizio          - TELEFONARE IN SEDE          63
160290170130ATTENZIONE! Sped."   " di peso > xxxxxx,x Kg. non ammessa                     64
160300170130ATTENZIONE! Sped."   " di volume > xx,xxx mc. non ammessa                     65
