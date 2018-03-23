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
001000170426     FFNFTA00F  O    E             DISK
001010030722
001020030722     FAZCLN01L  IF   E           K DISK
001030141020     FTIIDC02L  IF A E           K DISK
001040050601      * Bolle sede - Estensione
001050050601     fTita430c  if   e           k Disk    UsrOpn
001060151019     ffiar531c  uf a e           k Disk    UsrOpn infds(infdsar5)
001070151016     f                                     rename(fiar5000:fiar500S)
001080151016     f                                     rename(fiar5p00:fiar5p0S)
001090151019     fTitas30c  if   e           k disk    usropn infds(infdstitas)
001100151016     f                                     prefix(S_)
001110011120
001120160225     d FedD_ksc        s                   like(oc7kscc)
001130160225     d FedD_ctr        s              3
001140160225     d FedM_ksc        s                   like(oc7kscc)
001150160225     d FedM_ctr        s              3
001160160225     d
001170160225     d normlod         s                   like(o95loc)
001180150203     d bollacpi        s                   like(arbcpi)
001190071025     d bollaFGS        s                   like(D48FGS)
001200070319     d sav_arbias      s                   like(vidias)
001210041027     d sav_arbvas      s                   like(vidvas)
001220041027     d sav_arbqft      s                   like(vidqft)
001230041027     d sav_arbvmd      s                   like(vidvmd)
001240041027     d sav_arbvad      s                   like(vidvad)
001250131016     d bol_vidias      s                   like(vidias)
001260131016     d bol_vidvas      s                   like(vidvas)
001270131016     d bol_vidvmd      s                   like(vidvmd)
001280131016     d bol_vidvad      s                   like(vidvad)
001290041027     d sav_arbctr      s                   like(vidctr)
001300020327     d sav_dsftd       s                   like(dsftd)
001310011205     d sav1_vidias     s                   like(vidias)
001320011205     d sav1_vidvmd     s                   like(vidvmd)
001330011205     d sav1_vidcas     s                   like(vidcas)
001340020305     d deseftw         s                   like(§15eft)
001350020305     d desefdw         s                   like(§15efd)
001360020305     d deseffw         s                   like(§15eff)
001370020305     d lnpntw          s                   like(§ogntw)
001380020318     d clintw          s                   like(§ogntw)
001390020305     d lnantw          s                   like(§ogntw)
001400020531     d wdisag          s                   like(ot0dos)
001410070906     d wdisagdest      s                   like(ot0dos)
001420020603     d w48f12          s                   like(d48f12)
001430060301     d w48mct          s                   like(d48mct)
001440020911     d Kfgs            s                   Like(FvvFgs)
001450030612     d kAr5Trd         s                   Like(Ar5Trd)
001460050601     d kta4Trc         s                   Like(ta4trc)
001470021121     d Wctr            s                   Like(VidCtr)
001480040122     d va2va2          s                   Like(Ar6Va1)
001490040122     d va2va3          s                   Like(Ar6Va1)
001500040122     d va2va4          s                   Like(Ar6Va1)
001510041130     d va2va5          s                   Like(Ar6Va1)
001520041130     d va2va6          s                   Like(Ar6Va1)
001530041007     d OLDFFD          s                   Like(Arbffd)
001540041008     d OLDVLC          s                   Like(Arbvlc)
001550050202     d SAVCTR          s                   Like(vidctr)
001560070308     d SAVDCR          s                   Like(comdcr)
001570070308     d SAVTCR          s                   Like(vidtcr)
001580070308     d SAVHCR          s                   Like(vidhcr)
001590070308     d bollaDCR        s                   Like(comdcr)
001600070308     d bollaTCR        s                   Like(vidtcr)
001610070308     d bollaHCR        s                   Like(vidhcr)
001620070308     d bollaFBC        s                   Like(arbfbc)
001630071025     d ar4not_P        s                   Like(ar4not)
001640071025     d sav11pid        s                   Like(v11pid)
001650030612      *
001660030612     d DueCtr          s              2  0
001670070320     d TipoEla         s              1
001680030122     d wI0             s              1
001690030123     d wfpf            s              1
001700070312     d xx              s              3  0
001710090526     d yy              s              3  0
001720030612     d $mbr            s              6
001730030612     d w014a           s             14
001740030612     d wdiffe          s             13  3
001750030612     d w0173           s             17  3
001760030612     d tot_importi     s             17  3
001770100120     d xforzait        s              1
001780100120     d WVariavol       s              1
001790120705     d WVariavolEXP    s              1
001800091106     d Aggiobol        s              1
001810071026     d forzacdf        s              1
001820071025     d forzapid        s              1
001830071025     d forzapid2       s              1
001840071025     d forzaias        s              1
001850080625     d forzaias2       s              1
001860030612     d forzavmd        s              1
001870030612     d forzacas        s              1
001880030709     d flgsf20         s              1
001890030711     d wdataiso        s               d   datfmt(*iso)
001900060201     d wdataLimIas     s               d   datfmt(*iso)
001910030711     d wdataoggi       s               d   datfmt(*iso)
001920030711     d miapsw          s             10
001930030711     d wokpsw          s              1
001940040123     d war72           s              1
001950040122     d contasv         s              2  0
001960040122     d totimv          s                   Like(Ar6Imv)
001970040122     d wesvar1         s              1
001980040122     d wvar3ai         s              1
001990040224     d wffd            s                   Like(ArbFfd)
002000040224     d waggar5         s              1
002010040224     d wCA             s              1
002020040324     d wgiac           s              1
002030040225     d warbdcr         s                   Like(§Ar5gDcr)
002040040316     d wgcfas          s                   Like(GcpFas)
002050040324     d wgcded          s                   Like(GcpDed)
002060040324     d wgcddm          s                   Like(GcpDdm)
002070040608     d wokscr          s              1    inz('0')
002080080416     d wcmitt          s              7
002090080416     d wsflagga_C      s              1
002100110629     d wemd            s              1
002110011205
002120020715     d tbl             s              2    dim(20)                              sk tbl con ass.
002130090526     d ADRs            s              3    dim(50)                              sk tbl con ass.
002140090526     d ADRc            s              3    dim(50)                              sk tbl con ass.
002150170116     D*SOVR            S             48    DIM(1)                               DI COMODO SED
002160170116     D*SALC            S             48    DIM(1)                               DI COMODO SED
002170170116     D*SDLC            S             48    DIM(1)                               DI COMODO SED
002180020805     D PDLC            S             48    DIM(1)                               DI COMODO PAR
002190170116     D*C20             S              1    DIM(48) CTDATA PERRCD(48)
002200170116     D*C21             S              1    DIM(48) CTDATA PERRCD(48)
002210170116     D*C22             S              1    DIM(48) CTDATA PERRCD(48)
002220930507     D GGG             S              1    DIM(8) CTDATA PERRCD(8)              GIORNI CHIUSUR
002230151016     d cmdt            s             60    dim(03) ctdata perrcd(1)
002240170130     D MSG             S             78    DIM(165) CTDATA PERRCD(1)
002250041027     d*cmd             s              1    dim(45) ctdata perrcd(45)            ovrdbf
002260030612
002270990930     D* VARIE DELLA DTASV
002280990930     D* VARIE DELLA DARBT
002290941213     D* VARIE DI COMODO
002300941213     D WSV             S              1    DIM(21)                              SIGLA  VARIE
002310990930     D WVA             S             11  3 DIM(21)                              VALORE VARIE
002320941215     D SKI             S              1    DIM(15)                              COMODO
002330011106     D SKII            S              1    DIM(18)                              COMODO
002340941215     D K78             S              1    DIM(78)                              COMODO
002350990930     D*
002360150119     D SVA             S              1    DIM(100)                             VARIE IN TOT.I
002370070906     d
002380070906     d* Skiere di riesecuzione pgm
002390070906     D kdati           S            250    DIM(10)                              VARIE IN TOT.I
002400070906     D kcvb            S              2    DIM(10)                              VARIE IN TOT.I
002410070907     D ktrc            S              2    DIM(10)                              VARIE IN TOT.I
002420070911     D kffr            S              1    DIM(10)                              VARIE IN TOT.I
002430070911     D kf12            S              1    DIM(10)                              VARIE IN TOT.I
002440150414
002450150414      * filiali abilitate a cons.part "Z"=Expo
002460151119     d*fil_expo        s              3    dim(85)
002470030722
002480030722     D ktfp            S                   LIKE(CLNtfp)
002490030722     D ktfa            S                   LIKE(CLNtfa)
002500030722     D kann            S                   LIKE(CLNann)
002510030722     D kmes            S                   LIKE(CLNmes)
002520050601     d wlib            s             10
002530050601     d commt           s             80
002540050601     d lenght          s             15  5 inz(80)
002550120403     d erra            s              1
002560120403     d errp            s              1
002570120404     d wfatfil         s              2
002580120404     d war6agg         s              1
002590120411     d inx             s              2  0
002600120416     d savcpi_a        s                   like(arbcpi)
002610120418     d savcpi_p        s                   like(arbcpi)
002620120418     d savksc_a        s                   like(arbksc)
002630120418     d savctr_a        s                   like(arbctr)
002640120418     d savksc_p        s                   like(arbksc)
002650120418     d savctr_p        s                   like(arbctr)
002660030722
002670151019     d infdsar5        ds
002680151019     d   recname             261    270
002690151019     d infdstitas      ds
002700151019     d   recnamet            261    270
002710030722     D clnmat          DS
002720030722     D  mat                    1     31    dim(31)
002730030722     D clnpom          DS
002740030722     D  pom                    1     31    dim(31)
002750030722
002760030722     D                 DS
002770030722     D  ds_giorno              7      8  0
002780030722     D  ds_mese                5      6  0
002790030722     D  ds_anno                1      4  0
002800030722     D  ds_data                1      8  0
002810030722
002820941213     D*
002830941213     D* CODICI TARIFFA CON CUI TASSARE GLI ASSEGNATI 9999 - TAB 7W
002840990702     D* TARIFFE PER TASSAZIONE PREPAGATI
002850941222     D*-------------_____------------_______-------------_______-----
002860930505     D                 DS
002870941014     D  ARBAAS                 1      4  0
002880941014     D  ARBMGS                 5      8  0
002890941014     D  ARBDSP                 1      8  0
002900950114     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
002910931210     D PARAM3          DS
002920931210     D  PA3PDR                 1      7  0
002930931210     D  PA3TSR                 8      8
002940931210     D  PA3NDC                 9     15  0
002950931210     D  PA3FCT                16     16
002960931210     D  PA3FVD                17     17
002970931210     D  PA3FVT                18     18
002980950113     D  PA3DDC                19     26  0
002990041122     D  PA3sml               256    256
003000941014     D***
003010941014     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
003020941014     D***
003030941014     D WLBDAT          DS                  INZ
003040941014     D  G02DAT                 1      8  0
003050941014     D  G02INV                 9     16  0
003060941014     D  G02ERR                17     17
003070941014     D  G02TGI                18     22  0
003080941019     D                 DS
003090941019     D  VIDISD                 1      2
003100941019     D  VIDCPD                 3     16
003110941019     D  VIDPID                 1     16
003120071022     D                 DS
003130071022     D  V11ISD                 1      2
003140071022     D  V11CPD                 3     16
003150071022     D  V11PID                 1     16
003160941223     D                 DS
003170941223     D  VIDISM                 1      2
003180941223     D  VIDCPM                 3     16
003190941223     D  VIDPIM                 1     16
003200930210     D                 DS
003210930210     D  EMME                   1      1
003220020805     d  $simfel                2      4  0
003230020805     D  PARMBR                 5      7  0
003240930210     D  MBRFIS                 1     10
003250140130
003260140130      *Schiera per filiali partite con gestione telefonate AUT
003270140130     d Fil816tel       ds           256
003280140130     d  skFil816tel            1    255    dim(85)
003290141029     d TRULVPODS     e ds
003300141029     D  skFilOk               16    765    dim(250)
003310141029
003320141029     D skFilOkdir      s                   like(skFilOk) dim(250) inz
003330140130
003340920211     D DSARBD        E DS
003350930510     D DSARBG        E DS
003360130930     D DSARBN        E DS
003370990930     D DARBT         E DS
003380030612     D  §SV                   44     63    DIM(20)                              SIGLA  VARIE
003390030612     D  §VA                   64    183P 3 DIM(20)                              VALORE VARIE
003400920128     D DSARBK        E DS
003410030612
003420930506     D DSCC          E DS
003430991001     D DS$2          E DS
003440941212     D DSCV          E DS
003450941020     D DSQT1         E DS
003460080416     D DS3K          E DS
003470160915     D DTAS          E DS                  inz
003480060925      ** DS Flag operativi DS DTAS
003490060925     d Dtas01        e ds
003500060925
003510990930     D DTASV         E DS
003520030612     D  VSV                    1     20    DIM(20)                              SIGLA  VARIE
003530030612     D  VVA                   21    140P 3 DIM(20)                              VALORE VARIE
003540030612     D  VES                  141    160    DIM(20)                              ESENZ  VARIE
003550030612                                                                                ESENZ  VARIE
003560030612     D DSBL4W        E DS
003570070320     D DSBL4L        E DS
003580151126     D DSBL4m        E DS
003590160318     D DSBL4i        E DS
003600000623     D DS3IPT        E DS
003610000510     D DS3IDP        E DS
003620000510     D DS1A          E DS
003630930506     D DS1P          E DS
003640060131     D DSSP          E DS
003650941209     D DS15          E DS
003660920117     D DS3A          E DS
003670020715     d dstb          e ds
003680000202     D OG143         E DS
003690070319     D OG146         E DS
003700070306     d Og148         e ds
003710070306     d Og150         e ds
003720000202     D DGEI          E DS
003730991006     D DGED          E DS
003740991028     D DSTA01        E DS
003750930210     D DS7T          E DS
003760021121     d ddfe          e ds
003770030710     d dvpo          e ds
003780071031     d dvpoDECO      e ds
003790160318     d dnsd          e ds
003800050601     d dta4c         e ds
003810151019     D Dar5fat       E DS
003820090604     d ds5e          e ds
003830110906     d FNLVSTDS      e ds
003840130930     d dsemail       e ds
003850130927     d
003860130927     d darbn_ref     e ds
003870130927     d darbn_eml     e ds
003880130930     d darbn_sms     e ds
003890130930     d darbn_not     e ds
003900061107     d
003910151105     d xcfiva1ds     e ds
003920071025     d  ivaeu                  3      4
003930030612      * DS PER YEURCO - EURO CONVERTITORE
003940030612     D YeurcoDS      E DS
003950030612      * DS PER TRUL90R - reperimento stampanti e pgm di stampa
003960130322     D TRUL90DS      E DS
003970030612      * DS PER FNLV13R - CONTROLLO DI UN CAP
003980030612     D Fnlv13DS      E DS
003990030612      * DS PER FNLV14R - CONTROLLO DI INDIRIZZO COMPLETO  SENZA CAP
004000030612     D Fnlv14DS      E DS
004010030612      * DS PER TISI95R - CONTROLLO DI CAP
004020030612     D Tisi95DS      E DS
004030030612      * DS PER TRTB69R - TRTB70R - TRTB72R - TRTB73R - INT E GEST PARTI
004040030612     D DS7pqrs       E DS
004050030612      * DS PER TIBS02R - RICERCA TABELLA SU TNTBE
004060030612     D Tibs02DS      E DS
004070030626      * DS PER STAMPA LDV
004080130321     D*FNLSB5DS      E DS
004090030612      * DS PER FNLV04R - PER CALCOLO TOTALE FATTURA
004100030612     D FNLV04DS      E DS
004110050202      * DS PER FNLV59R - CARICAMENTO E CONTROLLO CODICI TARIFFA
004120050202     D Fnlv59DS      E DS
004130041008      * DS PER FNLV57R - PER aggiornamento files padroncini
004140041008     D FNLV57ds      E DS
004150030612      * DS PER FNLV16R - CONTROLLO TASSAZIONE
004160030612     D Fnlv16DS      E DS
004170040122     d dlv1601       e ds
004180030612      * DS PER FNLV18R - CALCOLO VOLUME AUTOMATICO
004190030612     D Fnlv18DS      E DS
004200060223      * DS PER FNLV19R - SCRITTURA E AGGIORNAMENTO FiAR400F
004210030612     D Fnlv19DS      E DS
004220030612      * DS PER FNLV20R - CALCOLA E SISTENA I VOLUMI SU BOLLA
004230030612     D Fnlv20DS      E DS
004240030612      * DS PER FNLV55R - REPERISCE TERMINAL DI PARTENZA E ARRIVO
004250030612     D Fnlv55DS      E DS
004260050511      * DS PER FNLRa2R - TASSAZIONE PORTI ASSEGNATI
004270050511     D Fnlra2DS      E DS
004280030612      * DS PER FNLR48R - DATI DI ENTRATA
004290030612     D Fnlr48DS      E DS
004300030612      * DS PER FNLR66R - CONTROLLO CAUSALE VARIAZIONE
004310030612     D Fnlr66DS      E DS
004320140130      * DS PER FNLRN6R - note LDV
004330140130     D FnlrN6DS      E DS
004340130321      * DS PER FIDG42R -
004350130321     D Fidg42ds      E DS
004360160318      * DS PER FNLR98R -
004370160318     D Fnlr98ds      E DS
004380141029
004390141029     D fnlry09ds2    E DS
004400150603     d
004410150603     D tnsd99ds      E DS
004420070329     d
004430120413     D TIBS69DS      E DS
004440120413     D ds_cnaco      E DS                  extname(CNACO00F) prefix(BS_)
004450120413     D ds_cnind      E DS                  extname(CNIND00F) prefix(BS_)
004460120413     D ds_cnclp      E DS                  extname(CNCLP00F) prefix(BS_)
004470120413     D ds_fncls      E DS                  extname(FNCLS00F) prefix(BS_)
004480120413
004490070329      * Dati utente
004500060131     d Azuteds       e ds                  extname(Azute00f)
004510060131     d dDatiute      e ds
004520060131     d dute01        e ds
004530060131     D TIBS34DS      E DS
004540060213      * ds x richiamo pgm FNLv85R
004550060213     d Fnlv85ds      e ds
004560060705      * ds x controllo tipo incasso e divisa C/A
004570060705     d TRULTICDS     e ds
004580140515
004590140515     d TRULINTDS     e ds
004600980722      *
004610030612      * DS PER XEDIT - PGM EDITAZIONE
004620011009     D DSEDIT        E DS                  EXTNAME(XEDITDS)
004630011009     D  IEDNUM       E                     EXTFLD(IEDNUMERO)
004640011009     D  IEDNRD       E                     EXTFLD(IEDNRDEC)
004650011009     D  IEDEDT       E                     EXTFLD(IEDEDTCOD)
004660011009     D  IEDLEN       E                     EXTFLD(IEDLENFLD)
004670011009     D  OEDNUM       E                     EXTFLD(OEDNUMAN)
004680011009     D  OEDFLG       E                     EXTFLD(OEDFLGERR)
004690011009     D  OEDCOD       E                     EXTFLD(OEDCODERR)
004700941212      *
004710910429     D KPJBA         E DS
004720930210     D  LIB                   92    101
004730060131     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004740030612     D  DTU                  848    895P 0 DIM(12)    PACKEVEN                  DATE PARAM
004750011205
004760020327     D DSFTD         E DS                  EXTNAME(FIFTD00F)
004770011205     d trul21ds      e ds
004780011205     d trul22ds      e ds
004790011205     d trul23ds      e ds
004800160225     D* DS PER TRUL27R1 - network bolla
004810160225     D trul27ds1     E DS
004820160225     D
004830160225     D trulC7ds      E DS
004840170130     D* DS PER TRUL73R - CONTROLLO LIMITI per tipo servizio
004850170130     D trul73ds      E DS
004860020531     D tisit0ds      E DS
004870131227     D fnlva1ds      E DS
004880041007     d*Trul42Ds      e ds
004890140212     d Trtcp2ds      e ds
004900141021     D TRUL33DS      E DS
004910021129     d Dar5scr       e ds
004920030123     d dAr5Ban       e ds
004930030729     d dAr5Bnb       e ds
004940130924     d dAr5Emd       e ds                  prefix(emd_)
004950031120     d dAr5          e ds
004960031120     d dAr5Gen       e ds
004970040219     d  §Ar5gTcr     e                     Extfld(§Ar5Tcr)
004980040219     d  §Ar5gDcr     e                     Extfld(§Ar5Dcr)
004990040219     d  §Ar5gHcr     e                     Extfld(§Ar5Hcr)
005000040219     d  §Ar5gGc1     e                     Extfld(§Ar5Gc1)
005010040219     d  §Ar5gGc2     e                     Extfld(§Ar5Gc2)
005020040219     d  §Ar5gTc1     e                     Extfld(§Ar5Tc1)
005030040219     d  §Ar5gTc2     e                     Extfld(§Ar5Tc2)
005040070910     d dAr5GenPar    e ds                  extname(dar5gen) prefix(PAR:3)
005050070910     d  PAr5gTcr     e                     Extfld(§Ar5Tcr)
005060070910     d  PAr5gDcr     e                     Extfld(§Ar5Dcr)
005070070910     d  PAr5gHcr     e                     Extfld(§Ar5Hcr)
005080070910     d  PAr5gGc1     e                     Extfld(§Ar5Gc1)
005090070910     d  PAr5gGc2     e                     Extfld(§Ar5Gc2)
005100070910     d  PAr5gTc1     e                     Extfld(§Ar5Tc1)
005110070910     d  PAr5gTc2     e                     Extfld(§Ar5Tc2)
005120030123     d dTba          e ds
005130031028     d dsbl4a        e ds
005140040806
005150070309     d ds2Z          e ds
005160070309     d ds2g2         e ds
005170040806     d ds7q          e ds
005180140408     d ds7r          e ds
005190051109     d dgcpflr       e ds
005200060111     d ddstflo       e ds
005210040809
005220040809      * ds per il controllo della presenza di CA per la spedizione richiesta
005230080701     d****** FIDN12DS      e ds
005240080701     d***  skKey                 26    305    dim(20)
005250080701     d****  skAnn                306    325    dim(20)
005260080701     d****  skDca                326    485  0 dim(20)
005270080701     d****  skFca                486    545  0 dim(20)
005280080701     d****  skDch                546    705  0 dim(20)
005290080701     d****  skCch                706    745    dim(20)
005300040809     d*
005310040809     d dsKey           ds
005320040809     d  dsaac                         4  0
005330040809     d  dsfil                         3  0
005340040809     d  dsnca                         7  0
005350050531     D PARAMT          DS
005360050531     D  PARCA                256    256
005370011205
005380950104     D*
005390950104     D* DEFINIZIONE COSTANTI
005400950104     D*
005410950104     D CPDC            C                   CONST('su Piano dei Conti')
005420950114     D CNOTAR          C                   CONST('NO TARIFFE')
005430061107     D Cdest           C                   CONST('D E S T I N A T A R I O')
005440061107     D CMITT           C                   CONST('--- M I T T E N T E ---')
005450141022     D* CAMPO PER DSPATR "REVERSE IMAGE"        DEI CAMPI A VIDEO
005460141022     D RIMM            C                   CONST(X'21')
005470011205
005480011205     d                sds
005490011205     d  nompgm                 1     10
005500030620
005510030620     d sav_indsin      s                   Like(indsin)
005520041008     d waltrocpi       s                   Like(arbcpi)
005530060223     d wcpifile        s                   Like(arbcpi)
005540041008     d waltrofvf       s                   Like(arbfvf)
005550041008     d waltrovlf       s                   Like(arbvlf)
005560041022     d waltrotc1       s                   Like(arbtc1)
005570041022     d waltrotc2       s                   Like(arbtc2)
005580080422     d waltroft3       s                   Like(arbft3)
005590041027     d newcpi          s                   Like(arbcpi)
005600051114     d wtc1            s                   Like(arbtc1)
005610051114     d wtc2            s                   Like(arbtc2)
005620051220     d memntw          s                   Like(§ogntw)
005630060111     d d_o95lna        s                   like(o95lna)
005640060316     d s_arbrsd        s                   like(arbrsd)
005650060316     d s_arbrd2        s                   like(arbrd2)
005660061107     d wcliie          s              1
005670061107     d wprv            s                   like(arbprd)
005680151105     d wcap            s                   like(arbcad)
005690151105     d wloc            s                   like(arblod)
005700061107     d Esiste          s              1
005710091217     d Wtipoagg        s              3
005720110906     d Lnacons         s              3  0
005730061107     d wnar            s                   Like(arbnzd)
005740130411     d wpda_ndc        s                   Like(arbndc)
005750130411     d wpda_ddc        s                   Like(arbddc)
005760130411     d wpda_dcm        s                   Like(arbdcm)
005770131016     d winf14          s              1
005780131120
005790131120     d sav_ift         s                   Like(ar6ift)
005800140403     d comgf2          s                   Like(o95gf2)
005810140407     d wpk2            s                   Like(§AR5PK2)
005820141020     d wdir            s              1
005830141021     d $Finenum        s               n   inz(*off)
005840141021     d wfgslna         s                   like(d55tfa)
005850141021     D Esito           s              1a
005860141031     d w0090           s              9  0
005870141021     d kprg            s                   like(idcprg)
005880141021     d winf12          s              1
005890141022     d annocur         s              4s 0
005900141029     d w0140i          s             14s 0 inz
005910141203     d savzng          s                   like(vidzng)
005920141212     d wnoindir        s              1
005930150119     d w_noapp         s              1
005940151016     d PesVolpkcn      s                   Like(Taspkcn)
005950151016     d PesVolSpc       s                   Like(TasSpc)
005960151016     d PesVolIMV       s                   Like(Ar6IMV)
005970151019     d Tassatoimv      s                   Like(Ar6IMV)
005980160112     d wfile           s              1
005990160318     d wtogli          s              1
006000170130     d wmsgtsp         s             78
006010170130     d wpes_pre        s                   like(vidpkf)
006020170130     d wvol_pre        s                   like(vidvlf)
006030170131     d wtsp03          s                   like(§5ed03)
006040141021
006050141021     D trul33R         pr                  extpgm('TRUL33R')
006060141021     D  kpjba                              likeds(kpjba)
006070160318     D fnlv19r         pr                  extpgm('FNLV19R')
006080160318     D  kpjba                              likeds(fnlv19ds)
006090160330     D fnlr98c         pr                  extpgm('FNLR98C')
006100160318     D  kpjba                              likeds(kpjba)
006110070515     i*
006120070515     IFNBLP000
006130070515     I              BLPFST                      ARBFSTDDT
006140910430     C****************************************************************
006150910430     C*  RIEPILOGO INDICATORI
006160910430     C***************************************************************
006170920128     C* 01    - VARIAZIONE DESTINATARIO
006180941209     C* 02    - VARIAZIONE PESO / VOLUME
006190930510     C* 03    - PROGRAMMA RICHIAMATO  DSARBG
006200930505     C* 04    - SPEDIZIONE CON GIACENZA APERTA
006210941222     C* 05    - INDICA SE LA 1 BOLLA O LA 2 E' UN ASSEGNATO X P.IVA
006220941222     C* 06    - 1 BOLLA IN FRANCO
006230991006     C* 07    - PROTEZIONE DIVISA PER PREPAGATI E FATTURE IMMEDIATE
006240991012     C* 08    - PREPAGATO
006250941213     C* 09    - SONO STATE MESSE DELLE VARIE NELLA TASSAZIONE
006260941222     C* 10    - ON -BOLLE IN ARRIVO OFF-BOLLE IN PARTENZA
006270941102     C* 11/12 - PROTEGGO LE CONSEGNE PARTICOLARI SE IMMESSE IN PART E
006280941102     C*         NON UTILIZZABILI IN ARRIVO
006290941102     C* 13    - VARIAZIONE TASSAZIONE S.F.
006300911210     C* 14    - VARIAZIONE TASSAZIONE FATTURA
006310941102     C* 15    - RITASSAZIONE
006320950120     C* 16    - PER CAUSALE CP PROTEGGO CAMPI DELLA CONSEGNA RICH/GG CH
006330940614     C* 17 OFF- FILE TASSAZIONE PADRONCINI
006340940614     C* 17 ON - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
006350911211     C* 18    - NON ESISTONO TARIFFE PER IL CODICE DIGITATO
006360920128     C* 19    - PROGRAMMA RICHIAMATO  DSAR2T
006370920128     C* 20    - PROGRAMMA RICHIAMATO  GENERICO
006380990701     C* 21    - CAUSALE VARIAZIONE MODIFICA CONTRASSEGNO
006390990701     C* 22    - UATO PER PROTEZIONE MITTENTE
006400990701     C* 23    - DATI FATTURA NON MODIFICABILI PER PREPAGATO AUTOMAT
006410950113     C* 24    - ESEGUITA LA VARIAZIONE DEL MITTENTE
006420920128     C* 25    - PROGRAMMA RICHIAMATO  DSARBT
006430950607     C* 26    - ABILITA L'ENTER PER L'AGGIORNAMENTO
006440950607     C* 27    - ABILITA L'F12 PER RITORNO AL PGM CHIAMANTE
006450941212     C* 28    - EMETTO VIDMSG
006460950616     C* 29    - PROTEZIONE CAMPI DEL VIDEO
006470980513     C* 30-33 - COMODO
006480920211     C* 34    - PROGRAMMA RICHIAMATO  DSARBD
006490920129     C* 35    - PROGRAMMA RICHIAMATO  DSARBK
006500920129     C* 36    - VARIA TASSAZIONE FRANCHI: SALTO I CONTROLLI SUL COD.CLI
006510920204     C* 37    - VARIATO TOTALE IMPONIBILE: EFFETTUO RISTAMPA FATTURA
006520941014     C* 38    - CREO RECORD IN FNARBM0T
006530941014     C* 39    - CREO RECORD IN FNARBD0T-CAUS.K5 K8- PER AVERE DESTINAT
006540930507     C* 40-67 - ERRORE
006550030729      * 70    - Visualizzo Colli originali
006560991001     C* 71-72 - PROTEGGO CAMPI DESTINATARIO SE 9999 GIA' CONTABILIZZATO
006570990415     C* 73    - SPROTEGGO IL CAMPO DELL'IMPORTO D'ASSICURARE E DEL VALORE
006580990415     C*         MERCE DICHIARATO
006590060201     C* 74    - protezione campi tassazione
006600060201     C* 75    - protezione campi importo da assicurare
006610140213     C* 77    - condizionamento campo "fd da non contattare"
006620131001     C* 81    - della DSARBN  passato campo uni70 - note
006630990415     C* 82    - ERRORE NON BLOCCANTE PER ELIMINAZIONE O AGGIUNTA IMP.ASSICURAZ.
006640131001     C* 83    - della DSARBN  passato campo cel
006650131001     C* 84    - della DSARBN  passato campo ref/tel
006660131001     C* 85    - SFPDSP
006670131001     C* 86    - della DSARBN  passato campo uni70 - email
006680071022     C* 87    - tasto funzionale"F17-Note ldv"attivo
006690070329     C* 88    - PROTEGGO PESO   SE NON VARIABILE
006700941216     C* 89    - PROTEGGO VOLUME SE NON VARIABILE
006710920220     C* 90    - ERRORE GENERICO
006720911213     C* 91    - ERRORE NON POSSIBILE ALLOCARE FILE DI INVIO
006730950407     C* 92    - ERRORE NON BLOCCANTE PER ELIMINAZIONE O AGGIUNTA C/ASS.
006740941215     C* 93    - INDICATORE DI CHANGE SU VALORE MERCE DICH
006750941215     C* 94    - RI SU VALUTA VAL.MERCE SE PRESA IN AUTOMATICO DA NAZ
006760941102     C* 95    - INDICATORE DI CHANGE SU C/ASSEGNO
006770941215     C* 96    - RI SU VALUTA C/ASSEGNO SE PRESA IN AUTOMATICO DA NAZ
006780941215     C* 97    - INDICATORE DI CHANGE SU IMPORTO DA ASSICURARE
006790941215     C* 98    - RI SU VALUTA IMP DA ASSIC SE PRESA IN AUTOMATICO DA NAZ
006800910430     C*****************************************************************
006810000000     C     *ENTRY        PLIST
006820000000     C                   PARM                    KPJBA
006830920211     C                   PARM                    DSARBD
006840920128     C                   PARM                    DSARBK
006850990930     C                   PARM                    DARBT
006860930510     C                   PARM                    DSARBG
006870030612     C                   PARM                    TRUL90DS
006880130930     C                   PARM                    DSARBn
006890050601     c*
006900050110     c*
006910050110     c                   setoff                                       kl
006920030612      *
006930030612      * Il numero dei parametri ricevuti è testato, in caso di modifica
006940030612      *  ricercare la stringa "%PARMS"
006950030612      *
006960030612     C                   MOVEL     KPJBU         Fnlr48DS
006970120426     C*
006980120426     C                   EXSR      OPN_TITA4
006990950104     C* ESEGUO LA VARIAZIONE
007000950104     C                   MOVEL     'E'           WESEG             1
007010020603     c                   movel     d48f12        w48f12
007020060301     c                   movel     d48mct        w48mct
007030950114     C* SE NON  RICHIAMATO IMPOSTO SEMPRE LA FERMATA NELLE VIDEATE
007040950114     C     D48TRC        IFEQ      *BLANKS
007050950609     C                   MOVEL     'V'           D48FFR
007060950114     C                   ENDIF
007070070907     c* skiere di riesecuzione automatica pgm: max 10 volte
007080070907     c                   clear                   kdati
007090070907     c                   clear                   kcvb
007100070907     c                   clear                   ktrc
007110070911     c                   clear                   kffr
007120070911     c                   clear                   kf12
007130070907     c                   clear                   kK                2 0
007140070907     c                   z-add     1             rr                2 0
007150941014     C*
007160910429     C*---------------------------------------------------------------*
007170041006     C* TIPO LANCIO, D48TLA "C" -           CHIUSO CON %R
007180941209     C* TIPO LANCIO, D48TLA "L" - ELABORO E CHIUDO CON LR
007190941209     C* TIPO LANCIO, D48TLA " " - ELABORO E CHIUDO IN RETRN
007200941209     C*
007210941209    1C     D48TLA        IFNE      'C'
007220001213     C*
007230001213    2C     D48FFR        IFNE      'E'
007240141017    2C     D48FFR        andne     'F'
007250001213    3C     OPN48         IFEQ      '0'
007260001213     C                   OPEN      FNLR48D
007270001213     C                   MOVE      '1'           OPN48
007280001213    3C                   ENDIF
007290001213   X2C                   ELSE
007300001213    3C     OPN48         IFEQ      '1'
007310001213     C                   CLOSE     FNLR48D
007320001213     C                   MOVE      '0'           OPN48
007330001213    3C                   ENDIF
007340001213    2C                   ENDIF
007350040224
007360040224     c                   Clear                   wCa
007370040324     c                   Clear                   wgiac
007380941209     C*
007390950104     C* FINTANTO CHE SI VUOLE SI PUO' RIMANRE DENTRO ALLE VARIAZIONI
007400950104   1AC     WESEG         DOWEQ     'E'
007410950104     C                   CLEAR                   WESEG
007420920123     C*
007430950104     C* CONTROLLI BOLLA CON CAUSALE VARIAZIONE
007440900509     C                   EXSR      CONTR
007450941014     C*
007460941014     C   90              GOTO      FINE
007470911209     C*
007480911210     C* CARICO BOLLA
007490120403     c                   if        flgcvr<>'S'
007500911210     C                   EXSR      CARICA
007510131002     c                   if        d48err='1'
007520131002     C                   GOTO      FINE
007530120403     c                   endif
007540131002     c                   endif
007550911209     C**
007560941019     C** VARIAZIONE: DESTINATARIO PESO VOLUME
007570911209     C**
007580941220     C                   SELECT
007590120330     c* Variazioni da manutenzione bolle di sede
007600120330     C     FLGCVR        WHENEQ    'S'
007610120427     c* Questa routine la posso eseguire solo se pgm richiamato
007620120427     c* Hanno fatto una manutenzione con CVB SD/SF da menù in quanto
007630120427     c* SD e SF non erano stati disabilitati per l'utilizzo da bolle par
007640120427     c* e bolle arrivi. In questo modo essendo azzerava peso,volume, mittente..
007650120427     c                   if        d48trc<>*blanks
007660120330     C                   EXSR      gesSD
007670120427     c                   endif
007680120330
007690941220     C     FLGCVR        WHENEQ    'I'
007700920120     C                   EXSR      EMFOR2
007710911210     C**
007720911211     C** VARIAZIONE TASSAZIONE 1° BOLLA
007730911210     C**
007740941220     C     FLGCVR        WHENEQ    'T'
007750920121     C                   EXSR      EMFOR3
007760911211     C**
007770911211     C** VARIAZIONE TASSAZIONE 2° BOLLA
007780911211     C**
007790941220     C     FLGCVR        WHENEQ    '2'
007800941102     C                   EXSR      EMFOR4
007810920116     C**
007820920116     C** VARIAZIONE: CODICE BOLLA CONTRASSEGNO
007830920116     C**
007840941220     C     FLGCVR        WHENEQ    'K'
007850941102     C                   EXSR      EMFOR5
007860920320     C**
007870920320     C** VARIAZIONE CONSEGNA RICHIESTA
007880920320     C**
007890941220     C     FLGCVR        WHENEQ    'C'
007900021129     c     D48Cvb        AndNe     'CS'
007910941102     C                   EXSR      EMFOR7
007920021129     C**
007930021129     C** VARIAZIONE CONSEGNA RICHIESTA SUPERMERCATI
007940021129     C**
007950021129     c     D48Cvb        WhenEq    'CS'
007960021129     c                   ExSr      Emfor71
007970941223     C**
007980941223     C** VARIAZIONE MITTENTE
007990941223     C**
008000941223     C     FLGCVR        WHENEQ    'M'
008010941223     C                   EXSR      EMFOR8
008020961211     C**
008030130927     C** VARIAZIONE DATI MITTENTE
008040961211     C**
008050961211     C     D48CVB        WHENEQ    'RM'
008060961211     C                   EXSR      EMFOR9
008070030123      **
008080030123      ** Variazione mancato ritiro BANCALI
008090030123      **
008100030123     c     D48Cvb        WhenEq    'BR'
008110030123     c                   ExSr      Emfor10
008120961211     C*
008130061107      **
008140061107      ** Variazione CODICE FISCALE
008150061107      **
008160061107     c     D48Cvb        WhenEq    'FI'
008170061107     c                   ExSr      Emfor11
008180130927      **
008190130927      ** Variazione dati destinatario SMS/mail/referente
008200130927      **
008210130927     C     FLGCVR        WHENEQ    'N'
008220130927     c                   ExSr      Emfor12
008230061107     C*
008240941220     C                   ENDSL
008250070907     c
008260070907     c* Verifico se ho delle riesecuzioni da fare
008270070907     c                   if        RR<=10 and kcvb(RR)<>*blanks
008280070907     c                   eval      weseg='E'
008290070907     c                   eval      d48cvb=kcvb(RR)
008300070907     c                   eval      d48trc=ktrc(RR)
008310070911     c                   eval      d48ffr=kffr(RR)
008320070911     c                   eval      w48f12=kf12(RR)
008330070907     c                   select
008340070907     c                   when      d48trc='D'
008350070907     c                   movel     kdati(RR)     dsarbd
008360070907     c                   when      d48trc='G'
008370070907     c                   movel     kdati(RR)     dsarbg
008380070907     c                   when      d48trc='K'
008390070907     c                   movel     kdati(RR)     dsarbK
008400070907     c                   when      d48trc='T'
008410070907     c                   movel     kdati(RR)     darbt
008420070907     c                   when      d48trc='2'
008430070907     c                   movel     kdati(RR)     darbt
008440070907     c                   ENDSL
008450070907     c
008460070907     c                   add       1             RR
008470070907     c                   endif
008480941209     C*
008490950104   1AC                   ENDDO
008500950104    1C                   ENDIF
008510920121     C*
008520000000     C     FINE          TAG
008530941209     C* CHIUSURA PGM
008540060302     C******             MOVEL     Fnlr48DS      KPJBU
008550950114     C*
008560950114     C* PULISCO IL CAMPO CHER AVEVO IMPOSTATO CON LA CONFERMA DEL MITT
008570950114     C*  CHE DEVE RIMANERE IMPOSTATO FINTANTO CHE SI E' DENTRO AL PGM
008580950114     C                   CLEAR                   WRICVL
008590950114     C                   CLEAR                   MITFVF
008600950114     C                   CLEAR                   MITVLF
008610950114     C                   CLEAR                   MITCTR
008620020722      * PULISCO IL CAMPO CHE AVEVO IMPOSTATO PER IL DESTINATARIO
008630020722      *  DISAGIATO
008640020722     C                   CLEAR                   Wdisag
008650060302     c*
008660060302     c                   Clear                   Fnlv85ds
008670060302     c                   Eval      Ilv85Tla = 'C'
008680060302     c                   movel     fnlv85ds      kpjbu
008690060302     c                   Call      'FNLV85R'
008700060302     c                   Parm                    kpjba
008710060302     c*
008720950114     C* CHIUSURA PGM
008730941216    1C     D48TLA        IFEQ      ' '
008740130417     C                   MOVEL     Fnlr48DS      KPJBU
008750941209     C                   RETURN
008760941209     C                   ELSE
008770941209     C*
008780930510     C* SE APERTI FILE CHIUDO
008790001213     C*
008800001213     C     OPN48         IFEQ      '1'
008810001213     C                   CLOSE     FNLR48D
008820001213     C                   ENDIF
008830001213     C*
008840030612     C                   CLEAR                   FNLV04DS
008850941209     C                   MOVEL     'C'           D04TLA
008860990930     C                   CALL      'FNLV04R'
008870030612     C                   PARM                    FNLV04DS
008880990930     C                   PARM                    DTASV
008890970808     C*
008900970808     C                   MOVEL     'C'           I13TLA
008910970808     C                   CALL      'FNLV13R'
008920970808     C                   PARM                    KPJBA
008930030612     C                   PARM                    Fnlv13DS
008940030612     C                   PARM                    Tisi95DS
008950030612     C                   CLEAR                   Tisi95DS
008960971105     C**
008970971105     C                   MOVEL     'C'           I95TLA
008980971105     C                   CALL      'TISI95R'
008990030612     C                   PARM                    Tisi95DS
009000970808     C*
009010970808     C                   MOVEL     'C'           I14TLA
009020970808     C                   CALL      'FNLV14R'
009030970808     C                   PARM                    KPJBA
009040030612     C                   PARM                    Fnlv14DS
009050971114     C*
009060030612     C                   CLEAR                   Fnlv55DS
009070971114     C                   MOVEL     'C'           D55TLA
009080971114     C                   CALL      'FNLV55R'
009090030612     C                   PARM                    Fnlv55DS
009100050202     c
009110050202     C                   CLEAR                   Fnlv59DS
009120050202     C                   MOVEL     'C'           ilv59tla
009130050202     C                   CALL      'FNLV59R'
009140050202     C                   PARM                    Fnlv59DS
009150020305     c*
009160160225     C                   CLEAR                   trul27ds1
009170020305     C                   MOVEL     'C'           I27TLA
009180160225     C                   CALL      'TRUL27R1'
009190160225     C                   PARM                    TRUL27DS1
009200160225     c
009210060705     C                   CLEAR                   trulTICds
009220060705     C                   MOVEL     'C'           IticTLA
009230060705     C                   CALL      'TRULTICR'
009240060705     C                   PARM                    TRULTICDS
009250060705     C                   PARM                    DS1A
009260140515
009270140515     C                   CLEAR                   trulintds
009280140515     C                   MOVEL     'C'           IintTLA
009290140515     C                   CALL      'TRULINTR'
009300140515     C                   PARM                    TRULintds
009310110906     c
009320110906     C                   CLEAR                   fnlvstds
009330110906     C                   MOVEL     'C'           ILVSTTLA
009340110906     C                   CALL      'FNLVSTR'
009350110906     c                   parm                    kpjba
009360110906     C                   PARM                    fnlvstds
009370941220     C*
009380120426     C                   IF        %OPEN(TITA430C)
009390050601     c                   close     tita430c
009400120426     C                   ENDIF
009410151016     C*
009420151016     C                   IF        %OPEN(FIAR531C)
009430151016     c                   close     fiar531c
009440151016     C                   ENDIF
009450151016     C*
009460151016     C                   IF        %OPEN(TITAS30C)
009470151016     c                   close     titas30c
009480151016     C                   ENDIF
009490130321
009500130411     c                   clear                   fidg42ds
009510130411     C                   MOVEL     'C'           co42tla
009520130411     c                   movel     fidg42ds      kpjbu
009530130411     c                   call      'FIDG42R'
009540130411     c                   parm                    kpjba
009550131227
009560131227     c                   clear                   fnlva1ds
009570131227     C                   MOVEL     'C'           ia1tla
009580131227     c                   movel     fnlva1ds      kpjbu
009590131227     c                   call      'FNLVA1R'
009600131227     c                   parm                    kpjba
009610141029
009620141029     c                   eval      ilry09tla= 'C'
009630141029     c                   eval      kpjbu=fnlry09ds2
009640141031     c                   call      'FNLRY09C'
009650141029     c                   parm                    kpjba
009660160318
009670160318     c                   eval      d19tla  =  'C'
009680160318     c                   call      'FNLV19R'
009690160318     c                   parm                    fnlv19ds
009700130321
009710130417     C                   MOVEL     Fnlr48DS      KPJBU
009720041006     C                   SETON                                        LR
009730941209     C                   END
009740941220     C*
009750941209     C* IMPOSTAZIONI INIZIALI -------------------------------------*
009760941209     C     *INZSR        BEGSR
009770941212     C* MI SALVO LA KPJBU
009780941212     C                   MOVEL     KPJBU         W256A           256
009790060131      *
009800060131     c     *dtaara       define    §azute        azuteds
009810060131     c     *dtaara       define    §datiute      ddatiute
009820060131     c                   in(E)     *dtaara
009830060131    1c                   If        %error  or RSUT = *blanks
009840060131     c                   Clear                   Tibs34ds
009850060131     c                   Call      'TIBS34R'
009860060131     c                   Parm                    Tibs34ds
009870060131     c                   In        *dtaara
009880060131    1c                   EndIf
009890060131
009900060131      * se ho errori nei dati utente esco dal pgm
009910060131w   1c                   if        DutErr = 'E'
009920060131     c                   GoTo      Fine
009930060131x   1c                   else
009940060131     c                   Movel     UteFaf        Dute01
009950060131e   1c                   endif
009960941212     C*
009970941209     C                   Z-ADD     1             CODUT
009980060131     C                   CALL      'X§PARUT'
009990060131     C                   PARM                    UT§DSE
010000060131     C                   MOVEL     rsut          VIDDSA
010010000104     C**
010020000104     C                   MOVEL     DTU(10)       W0020             2 0
010030000104     C                   Z-ADD     DTU(10)       WDTU10
010040000104     C     W0020         IFGE      60
010050000104     C                   MOVEL     19            WDTU10
010060000104     C                   ELSE
010070000104     C                   MOVEL     20            WDTU10            8 0
010080000104     C                   ENDIF
010090941014     C*
010100020805
010110020805     c                   eval      $simfel = simfel
010120020805     c                   movel     simfel        $mbr
010130070329     c
010140070329     c* Per EDP note sempre attive, se lna ha geo attiva
010150070329     c                   if        %subst(knmus:1:3)='EDP'
010160070329     c                   eval      dutgeot='S'
010170070329     c                   else
010180070329     c     dutpou        chain     azorg01l
010190070329     c                   if        %found(azorg01l)
010200070329     c                   movel     orgde8        og148
010210070329     c                   else
010220070329     c                   clear                   og148
010230070329     c                   endif
010240070329     c                   movel     §oggeot       dutgeot           1
010250070329     c                   endif
010260941020     C***
010270941020     C* CARICO CAMPI FISSI PER TASSAZIONE/BOLLETTAZIONE
010280941020     C***
010290941020     C                   MOVEL     'QT'          COD
010300941020     C                   MOVEL     '1       '    KEY
010310941212     C                   EXSR      CTABEL
010320941020     C  N30              MOVEL     TBLUNI        DSQT1
010330941020     C   30              CLEAR                   DSQT1
010340941214     C***
010350941214     C* PRENDO DALLA TABELLA DEI VOLUMI IL FLAG DI SOSTITUZIONE PER"Z"
010360941214     C***
010370941214     C                   MOVEL     '7T'          COD
010380941214     C                   MOVEL(P)  'Z'           KEY
010390941220     C                   EXSR      CTABEL
010400941214     C   30              CLEAR                   DS7T
010410941214     C  N30              MOVEL     TBLUNI        DS7T
010420941214     C*
010430970901     C* FLAG DI SOSTITUZIONE
010440941214     C                   MOVEL     §7TAFA        ZAFA              1
010450941214     C                   MOVEL     §7TAAA        ZAAA              1
010460941214     C                   MOVEL     §7TAFN        ZAFN              1
010470941214     C                   MOVEL     §7TAAN        ZAAN              1
010480970418     C***
010490970418     C* PRENDO DALLA TABELLA DEI VOLUMI IL FLAG DI SOSTITUZIONE PER"T"
010500970418     C***
010510970418     C                   MOVEL     '7T'          COD
010520970418     C                   MOVEL(P)  'T'           KEY
010530970418     C                   EXSR      CTABEL
010540970418     C   30              CLEAR                   DS7T
010550970418     C  N30              MOVEL     TBLUNI        DS7T
010560970418     C*
010570970901     C* FLAG DI SOSTITUZIONE
010580970418     C                   MOVEL     §7TAFA        TAFA              1
010590970418     C                   MOVEL     §7TAAA        TAAA              1
010600970418     C                   MOVEL     §7TAFN        TAFN              1
010610970418     C                   MOVEL     §7TAAN        TAAN              1
010620991001     C*
010630991001     C* CARICO DA SIGLE VARIE
010640991001     C                   MOVEL     '$2'          COD
010650991001     C                   MOVEL(P)  '1'           KEY
010660991001     C                   EXSR      CTABEL
010670991001     C  N30              MOVEL     TBLUNI        DS$2
010680991001     C   30              CLEAR                   DS$2
010690990930     C**
010700990930     C* CARICO VARIE CHE FANNO PARTE DEL TOTALE IMPONIBILE
010710990930     C**
010720990930     C                   Z-ADD     0             II                3 0
010730990930     C                   MOVEL     'CC'          COD
010740990930     C     KTAB1         CHAIN     TABEL                              30
010750990930    1C     *IN30         DOWEQ     *OFF
010760990930    2C     TBLFLG        IFEQ      ' '
010770990930     C                   MOVEL     TBLKEY        W005A             5
010780990930    3C     W005A         IFEQ      'VARIE'
010790990930     C                   MOVEL     TBLUNI        DSCC
010800990930     C* SIGLA VARIA
010810990930    4C     §CCFL6        IFEQ      'S'
010820990930     C                   ADD       1             II
010830990930     C                   MOVE      TBLKEY        SVA(II)
010840990930    4C                   ENDIF
010850990930    3C                   ENDIF
010860990930    2C                   ENDIF
010870990930     C**
010880990930     C     KTAB1         READE     TABEL                                  30
010890990930    1C                   ENDDO
010900991006     c*
010910991117     C* REPERIMENTO DIVISA DELLA MONETA DI CONTO
010920030612     C                   CLEAR                   Tibs02DS
010930991117     C                   CLEAR                   DGED
010940991117     C                   MOVEL     'C'           T02MOD
010950991117     C                   MOVEL     KNSIF         T02SIF
010960991117     C                   MOVEL     'GED'         T02COD
010970991117     C                   MOVEL     '1'           T02KE1
010980991117     C                   CALL      'TIBS02R'
010990991117     C                   PARM                    KPJBA
011000030612     C                   PARM                    Tibs02DS
011010991117     C     T02ERR        IFEQ      *BLANKS
011020991117     C                   MOVEL     T02UNI        DGED
011030991117     C                   ENDIF
011040011009     C* REPERIMENTO importi DELLA MONETA DI CONTO
011050030612     C                   CLEAR                   Tibs02DS
011060011009     C                   CLEAR                   DGEI
011070011009     C                   MOVEL     'C'           T02MOD
011080011009     C                   MOVEL     KNSIF         T02SIF
011090011009     C                   MOVEL     'GEI'         T02COD
011100011009     C                   MOVEL     §GEDCN        T02KE1
011110011009     C                   CALL      'TIBS02R'
011120011009     C                   PARM                    KPJBA
011130030612     C                   PARM                    Tibs02DS
011140011009     C     T02ERR        IFEQ      *BLANKS
011150011009     C                   MOVEL     T02UNI        DGEI
011160011120     C                   Z-ADD     §GEDFC        DFCSAV
011170011220     C                   Z-ADD     §geimf        imfsav
011180011009     C                   ENDIF
011190011009      * Tabella CV della moneta di conto
011200011009     C                   CLEAR                   DSCV
011210011009     C                   MOVEL     'CV'          COD
011220011009     C                   MOVEL(P)  §GEDCN        KEY
011230011009     C                   EXSR      CTABEL
011240011009     C     *IN30         IFEQ      *OFF
011250011009     C                   MOVEL     TBLUNI        DSCV
011260011009     C                   MOVEL     §CVDEC        DECSAV
011270011009     C                   ENDIF
011280000510     C***
011290000510     C* CARIO TABELLA VARIABILI DPD E POSTE
011300000510     C***
011310000510     C                   MOVEL     '3I'          COD
011320000510     C                   MOVEL(P)  'POSTE'       KEY
011330000510     C                   EXSR      CTABEL
011340000510     C  N30              MOVEL     TBLUNI        DS3IPT
011350000510     C   30              CLEAR                   DS3IPT
011360000510     C*
011370000510     C                   MOVEL     '3I'          COD
011380000510     C                   MOVEL(P)  'DPD'         KEY
011390000510     C                   EXSR      CTABEL
011400000510     C  N30              MOVEL     TBLUNI        DS3IDP
011410000510     C   30              CLEAR                   DS3IDP
011420020715
011430020715      * carico sk con tipi bolla che prevedono l'assicurazione
011440020715     c                   eval      cod = 'TB'
011450020715     c                   clear                   key
011460020715     c                   clear                   xx
011470020715     c     ktab1         setll     tabel00f
011480020715     c                   do        *hival
011490020715     c     ktab1         reade     tabel00f
011500020715      * fine file
011510020715     c                   if        %eof(tabel00f)
011520020715     c                   leave
011530020715     c                   endif
011540020715      * record annullato
011550020715     c                   if        tblflg <> *blanks
011560020715     c                   iter
011570020715     c                   endif
011580020715     c                   movel     tbluni        dstb
011590020715     c                   if        §tbfas = '1'
011600020715     c                   add       1             xx
011610020715     c                   eval      tbl(xx) = tblkey
011620020715     c                   endif
011630020715     c                   enddo
011640070309
011650070309      * Carico sk con eventi che prevedono modifica della cons.rich
011660070309     c                   eval      cod = '2Z'
011670070309     c                   clear                   key
011680070309     c                   clear                   xx
011690090526     c                   clear                   yy
011700070309     c     ktab1         setll     tabel00f
011710070309     c                   do        *hival
011720070309     c     ktab1         reade     tabel00f
011730070309      * fine file
011740070309     c                   if        %eof(tabel00f)
011750070309     c                   leave
011760070309     c                   endif
011770070309      * record annullato
011780070309     c                   if        tblflg <> *blanks
011790070309     c                   iter
011800070309     c                   endif
011810070309     c                   movel     tbluni        ds2Z
011820090526     c* x pgm non richiamato--> controllo solo gli S-sempre
011830090526     c                   if        §2ZADR='S'
011840070309     c                   add       1             xx
011850090526     c                   eval      ADRs(xx) = tblkey
011860070309     c                   endif
011870090526     c*
011880090526     c* x pgm     richiamato--> controllo sia S-sempre  che
011890090526     c*                         C-richiamati
011900090526     c                   if        §2ZADR<>' '
011910090526     c                   add       1             yy
011920090526     c                   eval      ADRc(yy) = tblkey
011930090526     c                   endif
011940070309     c                   enddo
011950021121
011960021121      * Aggancio tabella default tariffe fedex
011970030612     c                   Clear                   Tibs02DS
011980021121     c                   Movel     'C'           T02Mod
011990021121     c                   Movel     KNSIF         T02Sif
012000021121     c                   Movel     'DFE'         T02Cod
012010021121     c                   Movel     'FED'         T02Ke1
012020021121     c                   Call      'TIBS02R'
012030021121     c                   Parm                    Kpjba
012040030612     c                   Parm                    Tibs02DS
012050021121     c                   If        T02Err = *Blanks
012060021121     c                   Movel     T02Uni        DDfe
012070021121     c                   Else
012080021121     c                   Clear                   DDfe
012090021121     c                   EndIf
012100030710
012110060131      * Aggancio tabella VPO recpord VPO
012120030710     c                   Clear                   Dvpo
012130030710     c                   Clear                   Tibs02ds
012140030710     c                   Eval      T02Mod = 'C'
012150030710     c                   Eval      T02Sif = knsif
012160030710     c                   Eval      T02Cod = 'VPO'
012170030710     c                   Movel(p)  'VPO'         T02Ke1
012180030710     c                   Call      'TIBS02R'
012190030710     c                   Parm                    Kpjba
012200030710     c                   Parm                    Tibs02ds
012210030710     c                   If        T02Err = *Blanks
012220030710     c                   Movel     T02Uni        Dvpo
012230030710     c                   EndIf
012240060131
012250060131      * Aggancio tabella VPO record DECO
012260071031     c                   Clear                   Dvpodeco
012270071031     c                   Clear                   Tibs02ds
012280071031     c                   Eval      T02Mod = 'C'
012290071031     c                   Eval      T02Sif = knsif
012300071031     c                   Eval      T02Cod = 'VPO'
012310071031     c                   Movel(p)  'DECO'        T02Ke1
012320071031     c                   Call      'TIBS02R'
012330071031     c                   Parm                    Kpjba
012340071031     c                   Parm                    Tibs02ds
012350071031     c                   If        T02Err = *Blanks
012360071031     c                   Movel     T02Uni        Dvpodeco
012370071031     c                   EndIf
012380140130
012390140130      * Aggancio tabella VPO record DECOFI816TEL
012400140130     c                   Clear                   Tibs02ds
012410140130     c                   Clear                   fil816tel
012420140130     c                   Eval      T02Mod = 'C'
012430140130     c                   Eval      T02Sif = knsif
012440140130     c                   Eval      T02Cod = 'VPO'
012450140130     c                   Movel(p)  'DECOFI816TEL'T02Ke1
012460140130     c                   Call      'TIBS02R'
012470140130     c                   Parm                    Kpjba
012480140130     c                   Parm                    Tibs02ds
012490140130     c                   If        T02Err = *Blanks
012500140130     c                   Movel     T02Uni        fil816tel
012510140130     c                   EndIf
012520150414
012530150414      * Aggancio tabella "VPO" "EXPO"
012540151119     C*                  CLEAR                   tibs02ds
012550151119     c*                  clear                   fil_expo
012560151119     C*                  MOVEL     'C'           T02MOD
012570151119     C*                  MOVEL     KNSIF         T02SIF
012580151119     C*                  MOVEL     'VPO'         T02COD
012590151119     c*                  movel(P)  'EXPO'        t02ke1
012600151119     C*                  CALL      'TIBS02R'
012610151119     C*                  PARM                    KPJBA
012620151119     C*                  PARM                    tibs02ds
012630151119    2C*                  IF        T02ERR = *BLANKS
012640151119     C*                  MOVEa     T02uni        fil_expo
012650151119    2C*                  ENDIF
012660141029
012670141029      * Aggancio tabella VPO record DECOFI816DIR
012680141029     c                   clear                   trulvpods
012690141029     c                   eval      ivpoke1 = 'DECOFI816DIR'
012700141029     c                   call      'TRULVPOR'
012710141029     c                   parm                    trulvpods
012720141029     c                   eval      skfilokdir=skfilok
012730030710
012740030710      * Aggancio tabella MIA
012750030710     c                   Clear                   miapsw
012760030710     c                   Clear                   Tibs02ds
012770030710     c                   Eval      T02Mod = 'C'
012780030710     c                   Eval      T02Sif = knsif
012790030710     c                   Eval      T02Cod = 'MIA'
012800030710     c                   Movel(p)  'TUTTI'       T02Ke1
012810030710     c                   Call      'TIBS02R'
012820030710     c                   Parm                    Kpjba
012830030710     c                   Parm                    Tibs02ds
012840030710     c                   If        T02Err = *Blanks
012850030710     c                   Movel     T02Uni        miapsw
012860030710     c                   EndIf
012870160318      * Aggancio tabella NSD (per Import DPD "VEDI PACCO")
012880160318     c                   clear                   dnsd
012890160318     c                   Clear                   Tibs02ds
012900160318     c                   Eval      T02Mod = 'C'
012910160318     c                   Eval      T02Sif = knsif
012920160318     c                   Eval      T02Cod = 'NSD'
012930160318     c                   Movel(p)  '1'           T02Ke1
012940160318     c                   Call      'TIBS02R'
012950160318     c                   Parm                    Kpjba
012960160318     c                   Parm                    Tibs02ds
012970160318     c                   If        T02Err = *Blanks
012980160318     c                   Movel     T02Uni        dnsd
012990160318     c                   EndIf
013000031120
013010031120      * Aggancio tabella AR5 per record 'GEN'
013020031120     c                   Clear                   dAr5
013030031120     c                   Clear                   Tibs02ds
013040031120     c                   Eval      T02Mod = 'C'
013050031120     c                   Eval      T02Sif = knsif
013060031120     c                   Eval      T02Cod = 'AR5'
013070031120     c                   Movel(p)  'GEN'         T02Ke1
013080031120     c                   Call      'TIBS02R'
013090031120     c                   Parm                    Kpjba
013100031120     c                   Parm                    Tibs02ds
013110031120     c                   If        T02Err = *Blanks
013120031120     c                   Movel     T02Uni        dAr5
013130031120     c                   EndIf
013140040806      * Tabella 2G
013150040806     c                   Movel     '2G'          cod
013160040902     c                   Movel(p)  '2'           key
013170040806     c                   ExSr      ctabel
013180040902     c  n30              Movel     TblUni        ds2g2
013190040902     c   30              Clear                   ds2g2
013200050901      *
013210941222     C***
013220931119     C* DEFINIZIONE CAMPI
013230931119     C***
013240130411     C**** *LIKE         DEFINE    D66BDI        §7LFIN
013250991112     C     *LIKE         DEFINE    D66FFI        §7LFFI
013260991112     C     *LIKE         DEFINE    DATEU         WDATA
013270011010     C     *LIKE         DEFINE    §CVDEC        COMDEC
013280990702     C     *LIKE         DEFINE    AR6IMV        SAVIMV
013290151016     C     *LIKE         DEFINE    AR6IMV        SAVIMV_fat
013300990702     C     *LIKE         DEFINE    FVVNPG        KNPG
013310170116     C***  *LIKE         DEFINE    SOVR(1)       SOARBT
013320170116     C***  *LIKE         DEFINE    SOVR(1)       SAARBT
013330170116     C***  *LIKE         DEFINE    SOVR(1)       SDARBT
013340941213     C     *LIKE         DEFINE    AR6SV1        WSV1
013350941212     C     *LIKE         DEFINE    AR6VA1        WVA1
013360941212     C     *LIKE         DEFINE    TAMPPA        WPPA
013370941212     C     *LIKE         DEFINE    TAMMPA        WMPA
013380050202     C     *LIKE         DEFINE    ilv59FIE      WFIE
013390941209     C     *LIKE         DEFINE    §15EFT        DESEFT
013400941209     C     *LIKE         DEFINE    TBLKUT        §KUT
013410911211     C     *LIKE         DEFINE    TBLCOD        §COD
013420911211     C     *LIKE         DEFINE    TBLKEY        §KEY
013430941212     C     *LIKE         DEFINE    ARBIAS        VARIAS
013440950426     C     *LIKE         DEFINE    ARBIAS        COMIAS
013450930507     C     *LIKE         DEFINE    ARBCTR        COMCTR
013460941220     C     *LIKE         DEFINE    ARBKSC        KSC2
013470950104     C     *LIKE         DEFINE    ARBFAP        COMFAP
013480950407     C     *LIKE         DEFINE    NEWCBO        SAVCBO
013490950419     C     *LIKE         DEFINE    VIDPID        WPIVA
013500011009      *
013510011120     C     *LIKE         DEFINE    §GEDFC        DFCSAV
013520011220     C     *LIKE         DEFINE    §GEimf        imfsav
013530011009     C     *LIKE         DEFINE    §CVDEC        DECSAV
013540011009      *
013550941220     C* PER I PADRONCINI
013560950113     C     *LIKE         DEFINE    ARBKSC        WKSC
013570950113     C     *LIKE         DEFINE    ARBRSM        WRSM
013580950113     C     *LIKE         DEFINE    ARBRSD        OLDRSD
013590150206     C     *LIKE         DEFINE    ARBlod        OLDlod
013600950113     C     *LIKE         DEFINE    ARBCAD        OLDCAD
013610950113     C     *LIKE         DEFINE    ARBFIN        OLDFIN
013620941209     C     *LIKE         DEFINE    ARBVLF        OLDVLF
013630950114     C     *LIKE         DEFINE    ARBFVF        OLDFVF
013640941209     C     *LIKE         DEFINE    ARBPKF        OLDPKF
013650950113     C     *LIKE         DEFINE    ARBRSM        OLDRSM
013660950113     C     *LIKE         DEFINE    ARBCAM        OLDCAM
013670950113     C     *LIKE         DEFINE    ARBKSC        OLDKSC
013680941220     C*
013690950113     C     *LIKE         DEFINE    ARBVLF        WVOL
013700950113     C     *LIKE         DEFINE    ARBVLF        WSOTV
013710941212     C     *LIKE         DEFINE    ARBVLF        WADDV
013720941215     C     *LIKE         DEFINE    ARBVLF        VARVLF
013730941215     C     *LIKE         DEFINE    ARBPKF        VARPKF
013740950113     C     *LIKE         DEFINE    ARBPKF        WPKG
013750941220     C* PER FAR RIDIGITARE L'IMPORTO
013760941215     C     *LIKE         DEFINE    ARBVAS        VARVAS
013770941215     C     *LIKE         DEFINE    ARBVAD        VARVAD
013780941213     C     *LIKE         DEFINE    AR9VCA        VARVCA
013790950918     C* PER CONTROLLARE SE E' MODIFICATO
013800950918     C     *LIKE         DEFINE    AR9VCA        SAVVCA
013810950918     C     *LIKE         DEFINE    AR9TIC        SAVTIC
013820950918     C     *LIKE         DEFINE    AR9CAS        SAVCAS
013830970808     C     *LIKE         DEFINE    VIDCAD        SAVCAD
013840970808     C     *LIKE         DEFINE    VIDCAD        WCAD
013850970808     C     *LIKE         DEFINE    VIDCAD        WCAD1
013860970808     C     *LIKE         DEFINE    VIDLOD        SAVLOD
013870970808     C     *LIKE         DEFINE    VIDLOD        WLOD
013880970808     C     *LIKE         DEFINE    VIDLOD        WLOD1
013890060111     c     *LIKE         DEFINE    VIDrsd        Wrsd1
013900060111     c     *LIKE         DEFINE    VIDrsd        savrsd
013910061116     c     *LIKE         DEFINE    VIDrsd        savrsdQ
013920060111     c     *LIKE         DEFINE    VIDind        wind1
013930060111     c     *LIKE         DEFINE    VIDind        savind
013940060111     c     *LIKE         DEFINE    VIDprd        wprd1
013950060111     c     *LIKE         DEFINE    VIDprd        savprd
013960060111     c     *LIKE         DEFINE    VIDnzd        wnzd1
013970060111     c     *LIKE         DEFINE    VIDnzd        savnzd
013980970808     C     *LIKE         DEFINE    E14CAP        VARCAD
013990970808     C     *LIKE         DEFINE    E14LOC        VARLOD
014000970808     C     *LIKE         DEFINE    E14CAP        VARCAM
014010970808     C     *LIKE         DEFINE    E14LOC        VARLOM
014020991006     C     *LIKE         DEFINE    AR6POR        VARPOR
014030991006     C     *LIKE         DEFINE    AR6VA1        VA2VA1
014040941220     C* PER IL CONTROLLO
014050941212     C     *LIKE         DEFINE    AR9VCA        WVLT
014060941212     C     *LIKE         DEFINE    AR4NOT        SAVRD2
014070941019     C     *LIKE         DEFINE    AR4TRC        WTRC
014080950424     C     *LIKE         DEFINE    VIDQFT        VARQFT
014090950424     C     *LIKE         DEFINE    VIDKLP        VARKLP
014100950424     C     *LIKE         DEFINE    VIDKSC        VARKSC
014110970418     C     *LIKE         DEFINE    §7TSOZ        WSOSC
014120970418     C     *LIKE         DEFINE    ARBFVF        WFVF
014130970813     C     *LIKE         DEFINE    VIDKSC        MEMKSC
014140991005     C     *LIKE         DEFINE    VIDDIV        DIVGEI
014150991006     C     *LIKE         DEFINE    VIDDIV        WDIV
014160991007     C     *LIKE         DEFINE    AR6DFT        DFT2BO
014170950113     C*
014180950113     C     *LIKE         DEFINE    FTDDDC        WDRT
014190950113     C     *LIKE         DEFINE    FTDPDR        WPDR
014200950113     C     *LIKE         DEFINE    FTDNDC        WNDC
014210950113     C     *LIKE         DEFINE    FTDDDC        WDDC
014220950113     C     *LIKE         DEFINE    FTDTSR        WTSR
014230020327     C     *LIKE         DEFINE    FTDSTP        wstp
014240950114     C* CAMPI COLLEGATI ALLA VARIAZIONE DEL MITTENTE DA STORICIZZARE
014250950114     C     *LIKE         DEFINE    ARBFVF        MITFVF
014260950114     C     *LIKE         DEFINE    ARBVLF        MITVLF
014270950114     C     *LIKE         DEFINE    ARBCTR        MITCTR
014280950601     C* CAMPI PER GESTIONE VARIAZIONI DI BOLLE ESTERE NON LOCALI
014290950601     C     *LIKE         DEFINE    ARBLNA        WEUV
014300950601     C     *LIKE         DEFINE    ARBCVB        PARCVB
014310950601     C     *LIKE         DEFINE    ARBDTV        PARDTV
014320950601     C     *LIKE         DEFINE    ARBORV        PARORV
014330950601     C     *LIKE         DEFINE    ARBAAS        PARAAS
014340950601     C     *LIKE         DEFINE    ARBLNP        PARLNP
014350950601     C     *LIKE         DEFINE    ARBNRS        PARNRS
014360950601     C     *LIKE         DEFINE    ARBNSP        PARNSP
014370950601     C     *LIKE         DEFINE    ARBCVB        PARCVC
014380950601     C     *LIKE         DEFINE    SIMFEL        PARFEV
014390130410     C***  *LIKE         DEFINE    ARITIP        KTIP
014400001204     C     *LIKE         DEFINE    ARBTC1        SAVTC1
014410001204     C     *LIKE         DEFINE    ARBTC1        SAVTC2
014420930507     C*
014430950113     C                   MOVEL     'R'           WTSR
014440911211     C                   MOVEL     'R'           RIS               1
014450930210     C                   MOVEL     'M'           EMME
014460990818     C                   MOVEL     'C '          FTC               2
014470130410     C***                MOVEL     'A'           KTIP
014480931119     C***
014490911211     C* GIRO UDATE
014500931119     C***
014510941014     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + 3
014520941014     C                   TIME                    W0140            14 0
014530941014     C* UDATE IN GGMMAAAA
014540941014     C                   MOVE      W0140         WDTGIO            8 0
014550941014     C*
014560941014     C* UDATE IN AAAAMMGG
014570941014     C                   Z-ADD     WDTGIO        G02DAT
014580941014     C                   MOVEL     *BLANK        G02ERR
014590941014     C                   CALL      'XSRDA8'
014600941014     C                   PARM                    WLBDAT
014610941014     C                   MOVEL     G02INV        DATEU             8 0
014620030710     c     *iso          Move      dateu         wdataoggi
014630970423     C*
014640970423     C                   MOVEL     4             KNPG
014650941014     C**
014660941014     C*  KEY
014670941014     C**
014680941014     C     KARB          KLIST
014690941212     C                   KFLD                    D48AAS
014700941212     C                   KFLD                    D48LNP
014710941212     C                   KFLD                    D48NRS
014720941212     C                   KFLD                    D48NSP
014730941019     C     KARB2         KLIST
014740941212     C                   KFLD                    D48AAS
014750941212     C                   KFLD                    D48LNP
014760941212     C                   KFLD                    D48NRS
014770941212     C                   KFLD                    D48NSP
014780941019     C                   KFLD                    WTRC
014790130410     C***  KARI          KLIST
014800130410     C***                KFLD                    D48AAS
014810130410     C***                KFLD                    D48LNP
014820130410     C***                KFLD                    D48NRS
014830130410     C***                KFLD                    D48NSP
014840130410     C***                KFLD                    KTIP
014850941014     C     KFTT          KLIST
014860941215     C                   KFLD                    FTDPDR
014870941215     C                   KFLD                    FTDTSR
014880941215     C                   KFLD                    FTDNDC
014890950113     C                   KFLD                    FTDDDC
014900950113     C     KFTT2         KLIST
014910950113     C                   KFLD                    WPDR
014920950113     C                   KFLD                    WTSR
014930950113     C                   KFLD                    WNDC
014940950113     C                   KFLD                    WDRT
014950991005     C     KFTD          KLIST
014960950113     C                   KFLD                    WPDR
014970950113     C                   KFLD                    WTSR
014980950113     C                   KFLD                    WNDC
014990950113     C                   KFLD                    WDRT
015000950113     C                   KFLD                    WKSC
015010950113     C                   KFLD                    WRSM
015020020326     C     KFTD5         KLIST
015030020326     C                   KFLD                    WPDR
015040020326     C                   KFLD                    WTSR
015050020326     C                   KFLD                    WNDC
015060020326     C                   KFLD                    WDRT
015070020326     C                   KFLD                    D48AAS
015080020326     C                   KFLD                    D48LNP
015090020326     C                   KFLD                    D48NRS
015100020326     C                   KFLD                    D48NSP
015110941014     C* ACCESSO   TABEL                                    *
015120990930     C     KTAB1         KLIST                                                  *
015130990930     C                   KFLD                    CODUT                          *
015140990930     C                   KFLD                    COD               2            *
015150941014     C     KTAB2         KLIST                                                  *
015160941014     C                   KFLD                    CODUT                          *
015170941014     C                   KFLD                    COD               2            *
015180941014     C                   KFLD                    KEY               8            *
015190941014     C* ACCESSO   CNACO                                    *
015200941014     C     KACO          KLIST                                                  *
015210941014     C                   KFLD                    CODUT                          *
015220060131     C                   KFLD                    dutkci                         *
015230941014     C                   KFLD                    KSC               7 0          *
015240970423     C     KFVV          KLIST                                                  *
015250970423     C                   KFLD                    KNPG                           *
015260970423     C                   KFLD                    W0050                          *
015270020911     C                   KFLD                    Kfgs
015280941014     C* ACCESSO   TFTAM CON PROGRESSIVO                    *
015290941014     C*
015300941014     C     KTAM4         KLIST                                                  *
015310941014     C                   KFLD                    KSC                            *
015320941014     C                   KFLD                    COMCTR                         *
015330941014     C                   KFLD                    COMPRG            3 0          *
015340941014     C     KTAM6         KLIST                                                  *
015350941014     C                   KFLD                    KSC2                           *
015360941014     C                   KFLD                    COMCTR                         *
015370941014     C                   KFLD                    COMPRG                         *
015380941014     C     KTPT          KLIST                                                  *
015390941014     C                   KFLD                    KSC                            *
015400941014     C                   KFLD                    COMCTR                         *
015410941014     C                   KFLD                    COMPRG                         *
015420990818     C                   KFLD                    FTC               2            *
015430030124
015440030124     c     kAr5          Klist
015450030124     c                   Kfld                    D48Aas
015460030124     c                   Kfld                    D48Lnp
015470030124     c                   Kfld                    D48Nrs
015480030124     c                   Kfld                    D48Nsp
015490030124     c                   Kfld                    kAr5Trd
015500030722
015510030722     C     kazcln        klist
015520030722     C                   kfld                    ktfp
015530030722     C                   kfld                    ktfa
015540030722     C                   kfld                    kann
015550030722     C                   kfld                    kmes
015560040809
015570040809     C* CHIAVE SU FNDCT01L da *PGM utilità FIDN12R - Completa
015580080701     C***  KDCT1_C       KLIST
015590080701     C***                KFLD                    dsaac
015600080701     C***                KFLD                    dsfil
015610080701     C***                KFLD                    dsnca
015620041020     C* CHIAVE SU Fiqdt01L
015630041020     C     Kqdt          KLIST
015640041020     C                   KFLD                    kfgs
015650041020     C                   KFLD                    w0060
015660170627     C                   KFLD                    arbddc
015670050601     c*
015680050601     C* CHIAVE SU Tita430C
015690050601     C     Kta4          KLIST
015700050601     C                   KFLD                    D48AAS
015710050601     C                   KFLD                    D48LNP
015720050601     C                   KFLD                    D48NRS
015730050601     C                   KFLD                    D48NSP
015740050601     c                   kfld                    kta4trc
015750141029
015760141029     C     Kidc1         KLIST
015770141029     C                   KFLD                    D48AAS
015780141029     C                   KFLD                    D48LNP
015790141029     C                   KFLD                    D48NRS
015800141029     C                   KFLD                    D48NSP
015810141029     C                   KFLD                    w0140I
015820030722
015830941212     C**
015840941212     C                   MOVEL     W256A         KPJBU
015850001213     C*
015860001213     C                   MOVE      '0'           OPN48             1
015870911211     C                   ENDSR
015880120426     C*
015890120426      * APERTURA TITA430C
015900120426     c* se provengo da variazione bolla di sede non ho bisogno del tita4
015910120426     C     OPN_TITA4     BEGSR
015920120426     c                   if        %subst(d48cvb:1:1)<>'S'
015930151016     C                   if        NOT %OPEN(TITA430C)
015940120426      * Se sono in ambiente buono - GAITRAGRU
015950141020     c*****              If        knsif = 'FILTRA201 '
015960141020     c                   If        %subst(knsif:7:1) <> 'P'
015970120426     c                   Eval      wlib = 'GAITRAGRU '
015980120426      *  Se sono in ambiente di prova - GAITRAGRPS
015990120426     c                   Else
016000120426     c                   Eval      wlib = 'GAITRAGRPS'
016010120426     c                   EndIf
016020120426     c*
016030120426     c                   Clear                   commt
016040151016     c                   Movel(p)  cmdt(1)       commt
016050120426     c                   Eval      %Subst(commt:30:10) = wlib
016060120426     c                   Eval      commt =
016070151016     c                             %trim(commt) + '/TITA430C)'
016080120426     c                   Call      'QCMDEXC'
016090120426     c                   Parm                    commt
016100120426     c                   Parm                    lenght
016110151016     c                   Open      tita430c
016120120426     c                   endif
016130151016     c* FIAR531C
016140151016     c                   if        not %open(fiar531c)
016150151016     c                   If        %subst(knsif:7:1) <> 'P'
016160151016     c                   Eval      wlib = 'GAITRAGRU '
016170151016      *  Se sono in ambiente di prova - GAITRAGRPS
016180151016     c                   Else
016190151016     c                   Eval      wlib = 'GAITRAGRPS'
016200151016     c                   EndIf
016210151016     c*
016220151016     c                   Clear                   commt
016230151016     c                   Movel(p)  cmdt(2)       commt
016240151016     c                   Eval      %Subst(commt:30:10) = wlib
016250151016     c                   Eval      commt =
016260151016     c                             %trim(commt) + '/FIAR531C)'
016270151016     c                   Call      'QCMDEXC'
016280151016     c                   Parm                    commt
016290151016     c                   Parm                    lenght
016300151016     c                   Open      fiar531c
016310151016     c                   endif
016320151016     c* TITAS30C
016330151016     C                   if        NOT %OPEN(TITAS30C)
016340151016      * Se sono in ambiente buono - GAITRAGRU
016350151016     c                   If        %subst(knsif:7:1) <> 'P'
016360151016     c                   Eval      wlib = 'GAITRAGRU '
016370151016      *  Se sono in ambiente di prova - GAITRAGRPS
016380151016     c                   Else
016390151016     c                   Eval      wlib = 'GAITRAGRPS'
016400151016     c                   EndIf
016410151016     c*
016420151016     c                   Clear                   commt
016430151016     c                   Movel(p)  cmdt(3)       commt
016440151016     c                   Eval      %Subst(commt:30:10) = wlib
016450151016     c                   Eval      commt =
016460151016     c                             %trim(commt) + '/TITAS30C)'
016470151016     c                   Call      'QCMDEXC'
016480151016     c                   Parm                    commt
016490151016     c                   Parm                    lenght
016500151016     c                   Open      titas30c
016510151016     c                   endif
016520151016     c                   endif
016530120426     C                   ENDSR
016540900509     C*
016550050907      *--- CONTROLLO LA PARTICOLARITA' GIACENZA NEL PERIODO DELLE TRAZIONI RIDOTTE ---*
016560050907     C     Contr7q       BEGSR
016570050901      *
016580050907      * Inizializzo un flag che indica se è consentita la variazione della data cons. richiesta
016590050907      * in periodo d trazioni ridotte ed in presenza d particolarità consegna genericamente "G8"
016600050907     c                   clear                   wDCRG8            1
016610050901      * Determino se variazione di bolla con part. giacenza durante
016620050901      *  periodo delle trazioni ridotte
016630050901     c                   If        dateu >= §2g2dri and
016640050901     c                             dateu <= §2g2drf
016650050901      * posso modificare se arbgga <> *Blanks e se in tabella 7Q ha 'R' come cod. disposizione
016660050901     c                   If        ArbGga <> *Blanks
016670050901     c                   Movel     '7Q'          cod
016680050901     c                   Movel(p)  ArbGga        key
016690050901     c                   ExSr      ctabel
016700050901     c  n30              Movel     TblUni        ds7q
016710050901     c   30              Clear                   ds7q
016720050901     c                   If        §7q4d1 = 'R' or §7q4d2 = 'R' or
016730050901     c                             §7q4d3 = 'R' or §7q4d4 = 'R' or
016740050901     c                             §7q4d5 = 'R'
016750050901     c                   Movel     'S'           wDCRG8
016760050901     c                   EndIF
016770050901     c                   EndIF
016780050901     c                   EndIF
016790050901
016800050901     c                   EndSR
016810080201     c
016820900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
016830900509     C     CONTR         BEGSR
016840030122
016850030122     c     Riprova       Tag
016860030122
016870941014     C                   SETOFF                                       90
016880941014     C                   SETOFF                                       203519
016890941014     C                   SETOFF                                       253403
016900950609     C                   SETOFF                                       262729
016910990415     C                   SETOFF                                       73
016920151222     c                   clear                   wfile
016930941014     C***
016940941014     C* VEDO SE E' UN PROGRAMMA RICHIAMATO
016950941014     C***
016960941019     C     D48TRC        IFNE      *BLANKS
016970941019     C                   SETON                                        20
016980950114     C                   ELSE
016990941019     C                   ENDIF
017000950607     C***
017010950607     C* ABILITO L'ENTER COME TASTO DI AGGIORNAMENTO SE RICHIESTO
017020950607     C***
017030950607     C     D48FFR        IFNE      'V'
017040070309     C     D48FFR        andne     'F'
017050950607     C                   SETON                                        26
017060950607     C                   ENDIF
017070070309     c
017080070309     c* Se richiamo con F  --> mi imposto la E  (ma con 26 off non effettua
017090070309     c*    l'aggiornamento per ENTER)
017100070309     c                   if        d48ffr='F'
017110070309     c                   eval      d48ffr='E'
017120070309     c                   endif
017130070309     c
017140950607     C***
017150950607     C* ABILITO F12 SE RICHIESTO
017160950607     C***
017170020603     C     W48F12        IFEQ      'S'
017180950607     C                   SETON                                        27
017190950607     C                   ENDIF
017200950609     C***
017210950609     C* PROTEGGO TUTTI I CAMPI SE RICHIAMATO DA PGM PER ACQUISIZIONE
017220950609     C* AUTOMATICA VARIAZIONI DA PC ESTERO
017230950609     C***
017240140404     C***  D48EUV        IFEQ      'N'
017250140404     C***                SETON                                        29
017260140404     C***                END
017270941222     C**
017280941222     C* APRO I FILE A SECONDA DEL TIPO BOLLA
017290941222     C**
017300941222     C     D48TBO        IFEQ      'A'
017310941223     C                   SETON                                        10
017320941222     C                   ELSE
017330941223     C                   SETOFF                                       10
017340941222     C                   ENDIF
017350941222     C**
017360930505     C* SPEDIZIONE
017370941222     C**
017380151218     c***                if        d48cvb<>'SD' and d48cvb<>'SF'
017390151218     C***10KARB          CHAIN     FNARB01L                           3031
017400151218     C**N10KARB          CHAIN     FNBLP01L                           3031
017410930505     C*
017420930505     C* NON UTILIZZABILE
017430151218     C***  *IN31         IFEQ      *ON
017440151218     C***                MOVEL     MSG(1)        D48MSG
017450151218     C***                MOVEL     '1'           D48ERR
017460151218     C***                SETON                                        90
017470151218     C***                GOTO      ENDCTR
017480151218     C***                ENDIF
017490941223     C* NON TROVATA SPEDIZIONE
017500151218     C***  *IN30         IFEQ      *ON
017510151218     C***                MOVEL     MSG(61)       D48MSG
017520151218     C***                MOVEL     '1'           D48ERR
017530151218     C***                SETON                                        90
017540151218     C***                GOTO      ENDCTR
017550151218     C***                ENDIF
017560151218     c***                endif
017570080201     C*
017580080201     C* SE PGM RICHIAMATO CONTROLLO SOLO LA CORRETTEZZA DELLA CAUSALE
017590080201     C                   CLEAR                   Fnlr66DS
017600080201     C                   MOVEL     'C'           D66GES
017610080201     C                   MOVEL     D48CVB        D66CVB
017620080201     C  N20              MOVEL     D48AAS        D66AAS
017630080201     C  N20              MOVEL     D48LNP        D66LNP
017640080201     C  N20              MOVEL     D48NRS        D66NRS
017650080201     C  N20              MOVEL     D48NSP        D66NSP
017660080201     C  N20              MOVEL     D48TBO        D66TBO
017670160115     c                   eval      d66bnp='S'
017680080201     C                   MOVEL     Fnlr66DS      KPJBU
017690080201     C                   CALL      'FNLR66R'
017700080201     C                   PARM                    KPJBA
017710080201     C                   MOVEL     KPJBU         Fnlr66DS
017720080201     C*
017730080201     C* TROVATO ERRORE
017740080201     C     D66ERR        IFEQ      '1'
017750080201      * se prima ho variato il destinatario ed ora devo aggiornare le consegne particolari
017760080201      * in caso di errore provo a cambiare la causale di modifica da 'CP' a 'CR'
017770080201     c                   If        wI0 = 'S'
017780080201     c                   Movel     'CR'          D48Cvb
017790080201     c                   Clear                   wI0
017800080201     c                   Goto      Riprova
017810080201     c                   Else
017820080201     C                   MOVEL     D66MSG        D48MSG
017830080201     C                   MOVEL     D66ERR        D48ERR
017840080201     C                   SETON                                        90
017850151218     c** 10              unlock    fnarb01l
017860151218     c**n10              unlock    fnblp01l
017870080201     c
017880080201     C                   GOTO      ENDCTR
017890080201     c                   EndIf
017900080201     C                   ENDIF
017910141112     c* Le variazioni I0 non sono ammesse se presenti dispo di dirottamento
017920141112     c                   if        d48cvb='I0' or d48cvb='I7' or
017930141112     c                             d48cvb='I8' or d48cvb='I9'
017940141112     c                   clear                   fnlry09ds2
017950141112     c                   eval      ILRY09TCH='T'
017960141112     c                   eval      ILRY09TDIS='D'
017970141112     c                   eval      ILRY09AAS=d48aas
017980141112     c                   eval      ILRY09LNP=d48lnp
017990141112     c                   eval      ILRY09NRS=d48nrs
018000141112     c                   eval      ILRY09NSP=d48nsp
018010141112     c                   eval      kpjbu=fnlry09ds2
018020141112     c                   call      'FNLRY09C'
018030141112     c                   parm                    kpjba
018040141112     c                   eval      fnlry09ds2=kpjbu
018050141112     c                   if        OLRY09ESIT='1'
018060141112     C                   MOVEL     msg(156)      D48MSG
018070141112     C                   MOVEL     '1'           D48ERR
018080141112     C                   SETON                                        90
018090151218     c** 10              unlock    fnarb01l
018100151218     c**n10              unlock    fnblp01l
018110141112     c
018120141112     C                   GOTO      ENDCTR
018130141112     c                   endif
018140141112     c                   endif
018150151218     C**
018160151218     C* SPEDIZIONE
018170151218     C**
018180151218     c                   if        d48cvb<>'SD' and d48cvb<>'SF'
018190151221     c* Manutenzione bolle in arrivo
018200151221     c                   if        *in10
018210151221     c*
018220151221     c                   setoff                                       30
018230151221     c                   eval      wfile='A'
018240151221     c                   if        d66utp<>'P'
018250151221     C     KARB          CHAIN     FNARB01L                           3031
018260151221     c                   endif
018270151221     c                   if        (*in30 and d66utp='T') or d66utp='P'
018280151221     C     KARB          CHAIN     FNblp01L                           3031
018290160112     c                   if        not *in31 and not*in30  and ARBft1<>*blanks
018300151221     c                   unlock    fnblp01l
018310151221     C                   MOVEL     MSG(162)      D48MSG
018320151221     C                   MOVEL     '1'           D48ERR
018330151221     C                   SETON                                        90
018340151221     C                   GOTO      ENDCTR
018350151221     c                   endif
018360151221     c                   eval      wfile='P'
018370151221     c                   endif
018380151221     c                   else
018390151221     c* Manutenzione in partenza
018400151222     c                   eval      wfile='P'
018410151221     C     KARB          CHAIN     FNBLP01L                           3031
018420151221     c                   endif
018430151218     C*
018440151218     C* NON UTILIZZABILE
018450151218     C     *IN31         IFEQ      *ON
018460151218     C                   MOVEL     MSG(1)        D48MSG
018470151218     C                   MOVEL     '1'           D48ERR
018480151218     C                   SETON                                        90
018490151218     C                   GOTO      ENDCTR
018500151218     C                   ENDIF
018510151218     C* NON TROVATA SPEDIZIONE
018520151218     C     *IN30         IFEQ      *ON
018530151218     C                   MOVEL     MSG(61)       D48MSG
018540151218     C                   MOVEL     '1'           D48ERR
018550151218     C                   SETON                                        90
018560151218     C                   GOTO      ENDCTR
018570151218     C                   ENDIF
018580151218     c                   endif
018590080201     C* KEY SPEDIZIONE
018600080201     C                   MOVEL     D48CVB        VIDCVR
018610080201     C                   MOVEL     D48CVB        FLGCVR            1
018620080201     C*
018630080201     C* IMPOSTO IL CAMPO §7LFFI CHE MI SERVE PER ALLOCARE I FILE
018640080201     C                   MOVEL     D66FFI        §7LFFI
018650080201     C*
018660080201     C* IMPOSTO IL CAMPO §7LFIN CHE MI SERVE PER PROTEGGERE I CAMPI
018670080201     C* CHE NON DEVO VARIARE
018680130411     C***                MOVEL     D66BDI        §7LFIN
018690941014     C**
018700941014     C**
018710900509     C     ENDCTR        ENDSR
018720910429     C*
018730910429     C*--- CARICO DATI BOLLA -----------------------------------------*
018740910429     C     CARICA        BEGSR
018750920123     C                   SETOFF                                       040521
018760911211     C                   SETOFF                                       131415
018770990930     C                   SETOFF                                       0102
018780941220     C                   SETOFF                                       363789
018790941213     C                   SETOFF                                       111209
018800991001     C                   SETOFF                                       882307
018810941222     C                   SETOFF                                       525306
018820991012     C                   SETOFF                                       167308
018830061107     C                   SETOFF                                       747576
018840131001     C                   SETOFF                                       878681
018850140213     C                   SETOFF                                       838477
018860140408     c                   setoff                                       68
018870011018      *
018880011018     C                   CLEAR                   FLGVRN
018890110906     C                   CLEAR                   lnacons
018900941102     C*
018910950120     C                   CLEAR                   WIN95
018920950120     C                   CLEAR                   WIN97
018930950120     C                   CLEAR                   WIN93
018940131016     C                   CLEAR                   WINF14
018950950120     C                   CLEAR                   VARVCA
018960941215     C                   CLEAR                   VARVAS
018970941215     C                   CLEAR                   VARVAD
018980941215     C                   CLEAR                   VARVLF
018990941215     C                   CLEAR                   VARPKF
019000990702     C                   CLEAR                   SAVIMV
019010151016     C                   CLEAR                   SAVIMV_fat
019020991006     C                   CLEAR                   WDIV
019030991006     C                   CLEAR                   VARPOR
019040991006     C                   CLEAR                   VA2VA1
019050991012     C                   CLEAR                   SAVCBO
019060091124     C                   CLEAR                   WPT1              1
019070941209     C                   MOVEL     0             CIN               1 0
019080941209     C                   MOVEL     0             DIE               1 0
019090941212     C                   MOVEL     0             SEI               1 0
019100941214     C                   MOVEL     0             SET               1 0
019110941223     C                   MOVEL     0             OTT               1 0
019120970808     C                   MOVEL     0             WZV               1 0
019130970808     C                   MOVEL     0             WZ1               1 0
019140970808     C                   MOVEL     0             WZ2               1 0
019150070508     C                   MOVEL     0             WZ3               1 0
019160970808     C                   MOVEL     0             WZVM              1 0
019170970808     C                   MOVEL     0             WZ1M              1 0
019180970808     C                   MOVEL     0             WZ2M              1 0
019190141029     C                   MOVEL     0             Wforzdcr          1 0
019200141112     C                   MOVEL     0             Wforzgiac         1 0
019210990930     C                   MOVEL     ' '           WAR62             1
019220941019     C                   MOVEL     ' '           WAR6              1
019230941209     C                   MOVEL     ' '           WAR7              1
019240040123     c                   Clear                   War72
019250941019     C                   MOVEL     ' '           WAR9              1
019260941019     C                   MOVEL     ' '           WAR4D             1
019270150206     C                   MOVEL     ' '           WAR4X             1
019280061116     C                   MOVEL     ' '           WAR4Q             1
019290000623     C                   MOVEL     ' '           WBL4W             1
019300920319     C                   Z-ADD     0             COMKSC
019310930507     C                   Z-ADD     0             COMCTR
019320930507     C                   Z-ADD     0             COMPRG
019330911211     C                   Z-ADD     0             VI2DFT
019340911211     C                   Z-ADD     0             VIDDFT
019350941209     C                   Z-ADD     0             VARIAS
019360930507     C                   MOVEL     *BLANKS       VARCTR            3
019370941214     C                   CLEAR                   WSOST
019380030710     c                   Clear                   wokpsw
019390030729
019400030729     c                   Clear                   V1cBan
019410030729     c                   Clear                   V1cCor
019420030729     c                   Eval      *In70 = *Off
019430040123     c                   Clear                   ContaSv
019440040123     c                   Clear                   TotImv
019450060223     C                   CLEAR                   V1CCAS
019460060223     C                   CLEAR                   V1CTIC
019470060223     C                   CLEAR                   V1CVCA
019480060223     C                   CLEAR                   SAVCAS
019490060223     C                   CLEAR                   SAVTIC
019500060223     C                   CLEAR                   SAVVCA
019510090914     C                   CLEAR                   SAVrd2
019520150203     C                   CLEAR                   normlod
019530941215     C* PULIZIA DEI FORMATI VIDEO
019540950210     C                   CLEAR                   LR48D02
019550941215     C                   CLEAR                   LR48C03
019560941215     C                   CLEAR                   LR48D04
019570941215     C                   CLEAR                   LR48D05
019580941215     C                   CLEAR                   LR48D07
019590950104     C                   CLEAR                   LR48D08
019600961211     C                   CLEAR                   LR48D09
019610030124     c                   Clear                   Lr48d10
019620950213     C                   CLEAR                   V1CTIC
019630950213     C                   CLEAR                   V1CCAS
019640950213     C                   CLEAR                   V1CVCA
019650950213     C                   CLEAR                   V1CNT1
019660950213     C                   CLEAR                   V1CNT2
019670950213     C                   CLEAR                   V1CTC1
019680950213     C                   CLEAR                   V1DTC1
019690950213     C                   CLEAR                   V1CTC2
019700950213     C                   CLEAR                   V1DTC2
019710030124     c                   Clear                   VidNom
019720030124     c                   Clear                   VidTel
019730030124     c                   Clear                   VidMot
019740030122     c                   Clear                   wI0
019750031120     c                   Clear                   VidRef
019760070508     c                   Clear                   Vidtfd
019770031120     c                   Clear                   VidTeld
019780070306     c                   Clear                   AggioARP
019790070308     c                   Clear                   savdcr
019800070308     c                   Clear                   savtcr
019810070308     c                   Clear                   savhcr
019820070308     c                   Clear                   bollafbc
019830070309     c                   Clear                   Msgforza          3 0
019840150112     c                   Clear                   Msgforzatxt      78
019850070309     c                   clear                   eveADR            1
019860070910     c                   clear                   WesAR5GEN         1
019870130930     c                   clear                   WesAR5EMD         1
019880071025     c                   clear                   ForzaCDF
019890071025     c                   clear                   ForzaPID
019900071025     c                   clear                   ForzaPID2
019910071025     c                   clear                   sav11pid
019920071026     c                   clear                   AggioBol
019930111025     c                   clear                   Dcrindist         1
019940151019     c                   clear                   PesVolPkcn
019950151019     c                   clear                   PesVolSpc
019960151019     c                   clear                   PesVolImv
019970151019     c                   clear                   TassatoImv
019980061129     c  n10              movel     'T'           bollaft2
019990061129     c   10              movel     'R'           bollaft2
020000160330     c                   clear                   wtogli
020010111207     c* ES - Per il momento lo imposto sempre = "S" in quanto l'ufficio qualità non è
020020111207     c*  sicuro di voler fare la modifica di bloccare la manut coonsegna richiesta
020030111207     c*  dalla manutenzione normale
020040111207     c                   eval      Dcrindist='S'
020050941223     C*
020060941223     C     D48TBO        IFEQ      'A'
020070941223     C                   SETON                                        10
020080941223     C                   ELSE
020090941223     C                   SETOFF                                       10
020100941223     C                   ENDIF
020110941223     C*
020120941223     C* CLEARO I FORMATI DELLE BOLLE
020130051118     C                   CLEAR                   FiAR9000
020140990930     C                   CLEAR                   FIAR7000
020150990930     C                   CLEAR                   FIAR6000
020160030124     c                   Clear                   Fiar5000
020170941220     C**
020180941220     C* PGM RICHIAMATO
020190941220     C**
020200941220     C                   SELECT
020210941220     C* PROGRAMMA RICHIAMATO    DSARBD
020220941220     C     D48TRC        WHENEQ    'D'
020230941220     C                   SETON                                        2034
020240941220     C* PROGRAMMA RICHIAMATO    DSARBK
020250941220     C     D48TRC        WHENEQ    'K'
020260950616     C                   SETON                                        203529
020270990930     C* PROGRAMMA RICHIAMATO    DARBT TIPO RECORD 1
020280941220     C     D48TRC        WHENEQ    'T'
020290941220     C                   SETON                                        2025
020300990416     C*
020310941220     C* PROGRAMMA RICHIAMATO    DSARBG
020320941220     C     D48TRC        WHENEQ    'G'
020330941220     C                   SETON                                        2003
020340990930     C* PROGRAMMA RICHIAMATO    DARBT TIPO RECORD 2
020350941220     C     D48TRC        WHENEQ    '2'
020360941220     C                   SETON                                        2019
020370130930     C* PROGRAMMA RICHIAMATO    DARBN TIPO RECORD N + parm
020380130930     C     D48TRC        WHENEQ    'N'
020390130930     c                   if        %parms=7
020400130930     C                   SETON                                        20
020410131008     c                   else
020420131008     C                   MOVEL     MSG(24)       D48MSG
020430131008     C                   MOVEL     '1'           D48ERR
020440131008     c                   leavesr
020450131008     c                   endif
020460131008     c
020470941220     C                   ENDSL
020480950607     C*
020490020603     C     W48F12        IFEQ      'S'
020500950607     C                   SETON                                        27
020510950607     C                   ENDIF
020520950609     C*
020530140404     C***  D48EUV        IFEQ      'N'
020540140404     C***                SETON                                        29
020550140404     C***                END
020560950114     C**
020570950114     C* 24 ON - VARIATO IL MITTENTE
020580950114     C**
020590950114     C     WRICVL        IFEQ      'S'
020600950114     C                   SETON                                        24
020610950114     C                   ENDIF
020620941019     C**
020630941019     C* CARICO I DATI DELLA VIDEATA GENERICA DELLA BOLLA
020640941019     C**
020650941019     C* CHAIN CON CODICE BOLLA
020660941019     C                   MOVEL     '3A'          COD
020670941102     C                   MOVEL(P)  ARBCBO        KEY
020680941212     C                   EXSR      CTABEL
020690941019     C  N30              MOVEL     TBLUNI        DS3A
020700941019     C   30              CLEAR                   DS3A
020710941019     C*
020720941019     C                   MOVEL     §3ATB1        FLGTB1            1
020730941019     C                   MOVEL     §3ADES        V1DCBO                         DESCRIZIONE
020740071116     c* Verifico se il 1°tipo bolla è tassabile o no
020750071116     C                   MOVEL     'TB'          COD
020760071116     C                   MOVEL(P)  §3atb1        KEY
020770071116     C                   EXSR      CTABEL
020780071116     C  N30              MOVEL     TBLUNI        DSTB
020790071116     C   30              CLEAR                   DSTB
020800071116     c                   movel     §tbfcb        oldtb1FCB         1
020810170419
020820170419     c                   clear                   oldtb2FCB
020830170419     c                   if        §3atb2<>*blanks
020840170419     C                   MOVEL     'TB'          COD
020850170419     C                   MOVEL(P)  §3atb2        KEY
020860170419     C                   EXSR      CTABEL
020870170419     C  N30              MOVEL     TBLUNI        DSTB
020880170419     C   30              CLEAR                   DSTB
020890170419     c                   movel     §tbfcb        oldtb2FCB         1
020900170419     c                   endif
020910071116     c
020920070308     c
020930070308     c** Blocco consegna (mi serve per la consegna richiesta)
020940151221     c***10              eval      bollaFBC=arbfbc
020950151221     c                   if        *in10
020960151221     c                   if        wfile='A'
020970151221     c                   eval      bollaFBC=arbfbc
020980151221     c                   else
020990151221     c                   clear                   bollaFBC
021000151221     c                   endif
021010151221     c                   endif
021020070308     c  N10              clear                   bollaFBC
021030941019     C*
021040941019     C* SE FRANCO PROTEGGO P.IVA
021050011017     C     FLGTB1        COMP      'F'                                    05
021060941222     C   05              SETON                                            06
021070941019     C*
021080941019     C* SE ESISTE SECONDA BOLLA PERMETTO L'IMMISSIONE
021090061107     c                   clear                   ksc2bol
021100941212    1C     §3ATB2        IFNE      '  '
021110941019     C   05              SETOFF                                       05
021120941019     C* CHAIN SU 2 BOLLA
021130990930     C                   MOVEL     '2'           WTRC
021140070525     c                   if        flgcvr='2'
021150990930     C     KARB2         CHAIN     FIAR6000                           30
021160070525     c                   else
021170070525     C     KARB2         CHAIN(N)  FIAR6000                           30
021180070525     c                   endif
021190990930     C  N30              MOVEL     'E'           WAR62
021200990930     C  N30              MOVEL     AR6KSC        V1CKSC
021210991007     C  N30              Z-ADD     AR6DFT        DFT2BO
021220941019     C   30              Z-ADD     9999          V1CKSC
021230941019     C   30              MOVEL     ARBLNA        V1CLNA
021240991007     C   30              MOVE      *ZEROS        DFT2BO
021250061107     c                   movel     v1cksc        ksc2bol           7 0
021260040127      * Vedo se esiste anche il Fiar7
021270040127     c     Karb2         Setll     Fiar701l
021280040127     c                   If        %Equal(Fiar701l)
021290040127     c                   Eval      war72 = 'E'
021300040127     c                   EndIf
021310941019     C                   ELSE
021320941019     C                   MOVEL     ARBKSC        V1CKSC
021330941212    1C                   END
021340941019     C**
021350051118     C* SE PREVEDE IL C/A CHAI SU FiAR9
021360941019     C**
021370941212    1C     §3AFCA        IFNE      0
021380041022     c                   if        flgcvr='K'
021390051118     C     KARB          CHAIN     FiAR9000                           30
021400041022     c                   else
021410041022     c* non alloco record se non è variazione di C/A
021420051118     C     KARB          CHAIN(n)  Fiar9000                           30
021430041022     c                   endif
021440041022     c
021450941212    2C     *IN30         IFEQ      *OFF
021460941019     C                   MOVEL     'E'           WAR9              1
021470941019     C                   MOVEL     AR9CAS        V1CCAS
021480941019     C                   MOVEL     AR9TIC        V1CTIC
021490941102     C                   MOVEL     AR9VCA        V1CVCA
021500950918     C                   MOVEL     AR9CAS        SAVCAS
021510950918     C                   MOVEL     AR9TIC        SAVTIC
021520950918     C                   MOVEL     AR9VCA        SAVVCA
021530941212    2C                   ENDIF
021540941212    1C                   ENDIF
021550941019     C**
021560941019     C* VEDO SE CI SONO LE NOTE
021570941019     C**
021580941019     C                   MOVEL     '8'           WTRC
021590060222     C     KARB2         CHAIN(N)  FiAR4000                           30
021600941212    1C     *IN30         IFEQ      *OFF
021610941019     C                   MOVEL     AR4NOT        V1CNT1
021620941019     C*
021630941019     C                   MOVEL     '9'           WTRC
021640060222     C     KARB2         CHAIN(N)  FiAR4000                           30
021650941212    2C     *IN30         IFEQ      *OFF
021660941019     C                   MOVEL     AR4NOT        V1CNT2
021670941212    2C                   ENDIF
021680941212    1C                   ENDIF
021690970409     C* SI NO DDT
021700031028     c                   Clear                   dsbl4a
021710970409     C                   MOVEL     'A'           WTRC
021720151222     c                   if        wfile='A'
021730151222     C     KARB2         CHAIN(N)  FiAR4000                           30
021740151222    2C     *IN30         IFEQ      *OFF
021750031028     C**!!!              MOVEL     AR4NOT        V1CDDT
021760031028     c                   Movel     Ar4not        dsbl4a
021770031028     c                   Move      §b4abm        v1cddt
021780970409     C                   ELSE
021790970409     C                   MOVEL     'S'           V1CDDT
021800970409    2C                   ENDIF
021810151222     c                   else
021820151222     C                   MOVEL     ARBFSTddt     V1CDDT            1
021830151222     c                   endif
021840970409     C*
021850970409     C     V1CDDT        IFEQ      'S'
021860970409     C     V1CDDT        OREQ      'C'
021870031028     c     v1cddt        oreq      'P'
021880031028     c     v1cddt        oreq      'Q'
021890970409     C                   MOVEL     'DDT'         V1DDDT
021900970409     C                   ELSE
021910970409     C                   CLEAR                   V1DDDT
021920970409     C                   ENDIF
021930060222     C* CONTROLLO SE ESISTE RECORD W DI ar4 PER EVENTUALE
021940000623     C*   MODIFICA DI MITTENTE
021950000623     C     *IN10         IFEQ      *OFF
021960000623     C                   MOVEL     'W'           WTRC
021970060222     C     KARB2         CHAIN(N)  Fiar4000                           30
021980000623     C  N30              MOVEL     'S'           WBL4W             1
021990000623     C                   ENDIF
022000941019     C* DESCRIZIONE CAUSALE
022010941019     C                   MOVEL     D66DES        V1DCVR
022020941019     C                   MOVEL     ARBLNP        V1CLNP
022030941212     C                   MOVEL     ARBNRS        V1CNRS
022040941019     C                   MOVEL     ARBNSP        V1CNSP
022050941019     C                   MOVEL     ARBLNA        V1CLNA
022060941019     C                   MOVEL     ARBCBO        V1CCBO
022070941019     C                   MOVEL     ARBRSM        V1CRSM
022080941019     C                   MOVEL     ARBCAM        V1CCAM
022090941019     C                   MOVEL     ARBLOM        V1CLOM
022100941019     C                   MOVEL     ARBPRM        V1CPRM
022110941019     C                   MOVEL     ARBNZM        V1CNZM
022120970828     C                   MOVEL     ARBFAP        V1CFAP
022130941019     C                   MOVEL     ARBRSD        V1CRSD
022140941019     C                   MOVEL     ARBIND        V1CIND
022150941019     C                   MOVEL     ARBCAD        V1CCAD
022160941019     C                   MOVEL     ARBLOD        V1CLOD
022170941019     C                   MOVEL     ARBPRD        V1CPRD
022180941019     C                   MOVEL     ARBNZD        V1CNZD
022190970828     C                   MOVEL     ARBFIN        V1CFIN
022200941019     C                   MOVEL     ARBCTS        V1CCTS
022210941019     C                   MOVEL     ARBTC1        V1CTC1
022220941019     C                   MOVEL     ARBTC2        V1CTC2
022230941027     C                   MOVEL     ARBPKF        V1CPKF
022240941019     C                   MOVEL     ARBVLF        V1CVLF
022250941019     C                   MOVEL     ARBFVF        V1CFVF
022260030729     c                   Z-Add     ArbNcl        V1cNcl
022270141029     c                   clear                   v1dffd
022280141029     c                   if        arbffd='S'
022290141029     c                   eval      v1dffd='FermoDeposito'
022300141029     c                   endif
022310100121     c* Partita iva MITTENTE se bolla singola franco
022320100121     c                   select
022330100121     c                   when      flgtb1='F' and §3atb2=*blanks
022340100121     c                   eval      v1cpim=arbcpi
022350100121     c                   when      flgtb1='A' and §3atb2=*blanks
022360100121     c                   eval      v1cpid=arbcpi
022370100121     c* In arrivo   per bolla  doppia --> partita iva dell'assegnato
022380151222     c                   when      wfile='A'   and §3atb2<>*blanks
022390100121     c                   eval      v1cpid=arbcpi
022400100121     c* In partenza per bolla  doppia --> partita iva del mittente
022410151222     c                   when      wfile='P'   and §3atb2<>*blanks
022420100121     c                   eval      v1cpid=arbcpi
022430100121     c                   ENDSL
022440100121
022450950114     C* SALVO IL CODICE CLIENTE DELLA SPEDIZIONE
022460950114     C                   MOVEL     ARBKSC        SAVKSC            7 0
022470060511     C                   MOVEL     ARBKSC        kscbolla          7 0
022480941102     C* PRENDO LA VALUTA DAL DESTINATARIO E MITTENTE
022490941102     C                   MOVEL     '15'          COD
022500941102     C                   MOVEL(P)  ARBNZD        KEY
022510941212     C                   EXSR      CTABEL
022520941102     C  N30              MOVEL     TBLUNI        DS15
022530941102     C   30              CLEAR                   DS15
022540941102     C                   MOVEL     §15VLT        DESVLT            3
022550941212    1C     §15ITA        IFEQ      'I'
022560941102     C                   MOVEL     'I'           WDESIE            1
022570941102     C                   ELSE
022580941102     C                   MOVEL     'E'           WDESIE            1
022590941212    1C                   ENDIF
022600020305     c                   MOVEL     §15EFT        DESEFTW
022610020305     c                   MOVEL     §15EFD        DESEFDW
022620020305     c                   MOVEL     §15EFF        DESEFFW
022630971230     V* CHAIN SU AZORG CON LINEA PARTENZA
022640971230     C     ARBLNP        CHAIN     AZORG01L                           32
022650941209     C*
022660000223     C                   MOVEL     ORGDE3        OG143
022670020305     C                   MOVEL     §OGNTW        LNPNTW
022680941102     C*
022690941102     C                   MOVEL(P)  ARBNZM        KEY
022700941212     C                   EXSR      CTABEL
022710941102     C  N30              MOVEL     TBLUNI        DS15
022720941102     C   30              CLEAR                   DS15
022730941102     C                   MOVEL     §15VLT        MITVLT            3
022740941212    1C     §15ITA        IFEQ      'I'
022750941102     C                   MOVEL     'I'           WMITIE            1
022760941102     C                   ELSE
022770941102     C                   MOVEL     'E'           WMITIE            1
022780941212    1C                   ENDIF
022790001204     C**
022800051129     C* TERMINAL DI PARTENZA DELLA LNP: VEDO SE HO VDL
022810040123     c                   Clear                   og150
022820970901     C     ARBTFP        CHAIN     AZORG                              32
022830051129     C                   CLEAR                   WVDL
022840970901    1C     *IN32         IFEQ      *OFF
022850051129     C**!!!              TESTN                   DATVDL               31
022860040123     c                   Movel     Orgdf0        og150
022870120613     c                   testn                   §ogvol               31
022880970901     C*
022890040123    2C**!!!*IN31         IFEQ      '1'
022900051129     C**!!!DATVDL        ANDGT     *ZEROS
022910120613     c***!!!             If        §ogspv = 'S'
022920120613     c                   if        *in31 and §ogvol>*zeros
022930051129     C                   MOVEL     'S'           WVDL              1
022940970901    2C                   END
022950970901    1C                   END
022960941019     C**
022970941019     C* TIPO SERVIZIO
022980941019     C**
022990090604     c                   clear                   ds5e
023000941014     C                   MOVEL     '5E'          COD
023010941014     C                   MOVEL(P)  ARBTSP        KEY
023020941212     C                   EXSR      CTABEL
023030090604     C  N30              MOVEL     TBLUNI        ds5e
023040090604     C  N30              MOVEL     §5ed08        V1DTSP
023050170131     C  N30              MOVEL     §5ed03        WTSP03
023060941220     C   30              CLEAR                   V1DTSP
023070170131     C   30              CLEAR                   wtsp03
023080941019     C**
023090910429     C* DATA SPEDIZIONE
023100941019     C**
023110941014     C                   MOVEL     '3'           G02ERR
023120941014     C                   Z-ADD     ARBMGS        G02INV
023130941014     C                   MOVEL     ARBAAS        G02INV
023140941014     C                   CALL      'XSRDA8'
023150941014     C                   PARM                    WLBDAT
023160941014     C*
023170941212     C                   Z-ADD     G02DAT        V1CDSP
023180941019     C**
023190930505     C* LINEA DI ARRIVO
023200941019     C**
023210070306     C     ARBLNA        CHAIN     AZORG                              30
023220941019     C  N30              MOVEL     ORGDES        V1DLNA                          L.ARRIVO
023230941019     C   30              MOVEL     *BLANKS       V1DLNA
023240000202     C                   MOVEL     ORGDE3        OG143
023250020305     C                   MOVEL     §OGNTW        LNANTW
023260020305     c                   select
023270020305     c     lnantw        wheneq    'DPD'
023280020305     c                   movel     desefdw       deseft
023290020305     c     lnantw        wheneq    'FED'
023300020305     c                   movel     deseffw       deseft
023310020305     c                   other
023320020305     c                   movel     deseftw       deseft
023330020305     c                   endsl
023340160225
023350160225     c                   if        lnantw='FED'
023360160225     c                   clear                   trulc7ds
023370160225     c                   eval      ic7tntw='FED'
023380160225     c                   eval      ic7tip='FM'
023390160225     c                   call      'TRULC7R'
023400160225     c                   parm                    trulc7ds
023410160225     c                   eval      fedm_ksc=oc7kscc
023420160225     c                   movel     oc7ctrc       fedm_ctr
023430160225     c
023440160225     c                   clear                   trulc7ds
023450160225     c                   eval      ic7tla='L'
023460160225     c                   eval      ic7tntw='FED'
023470160225     c                   eval      ic7tip='FD'
023480160225     c                   call      'TRULC7R'
023490160225     c                   parm                    trulc7ds
023500160225     c
023510160225     c                   eval      fedd_ksc=oc7kscc
023520160225     c                   movel     oc7ctrc       fedd_ctr
023530160225     c                   endif
023540070906     c*****
023550070906     c* Verifico se destinatario disagiato
023560070906     c*****
023570070906     c                   clear                   wdisagdest
023580070906     c                   clear                   tisit0ds
023590070906     c                   movel     'E'           it0tla
023600070906     c                   movel     arbnzd        it0naz
023610070906     c                   movel     arbprd        it0prv
023620070906     c                   movel     arbcad        it0cap
023630070906     c                   movel     arblod        it0loc
023640070906     c                   movel     arbind        it0ind
023650070906     c                   movel     arbrsd        it0rag
023660070906     c                   call      'TISIT5R'
023670070906     c                   parm                    tisit0ds
023680070906    1c     ot0err        ifeq      *blanks
023690070906    2c                   if        ot0dos = 'A' or ot0dos = 'D' or
023700070906     c                             ot0dos = 'S'
023710070906     c                   movel     ot0dos        wdisagdest
023720070907    3c     wdisagdest    ifeq      'D'
023730070906     c                   movel     'X'           wdisagdest
023740070906    3c                   endif
023750070906    2c                   endif
023760070906   x1c                   else
023770070906     c                   clear                   wdisagdest
023780070906    1c                   endif
023790070306     c
023800070910     c****
023810070910      * Reperisco record GEN nel fiar5
023820070910     c                   Clear                   dAr5Genpar
023830070910     c                   Eval      kAr5Trd = 'GEN'
023840070910     c     kAr5          Chain(n)  Fiar501l
023850070910     c                   If        %Found(Fiar501l)
023860070910     c                   Movel     Ar5Uni        dAr5Genpar
023870070910     c                   eval      wesar5gen='S'
023880070910     c                   endif
023890070319     c****
023900070319     c* FILIALE GESTIONE: VEDO SE GEO ATTIVA
023910070319     c****
023920070319     c                   clear                   FGSgeot
023930070319     c                   clear                   FGSddaSI
023940110906     c****               if        *in10
023950070319     c                   clear                   fnlv55ds
023960070319     C                   MOVEL     '6'           D55TPT
023970070319     C                   MOVEL     ARBLNA        D55LIN
023980070319     C                   MOVEL     DATEU         D55DRF
023990070319     C                   CALL      'FNLV55R'
024000070319     C                   PARM                    Fnlv55DS
024010110906     c                   eval      lnacons=d55tfa
024020070319     c
024030070319     c     d55tfa        CHAIN     AZORG                              30
024040070319     c                   if        not *in30
024050070319     C                   MOVEL     ORGDE8        og148                           L.ARRIVO
024060070319     C                   MOVEL     ORGDE6        og146                           L.ARRIVO
024070070319     C
024080070319     c                   movel     §oggeot       FGSgeot           1
024090070319     C
024100070319    1c                   if        §ogdda>*zeros
024110070319     c                   testn                   §ogdda               30
024120070319     c                   move      §ogdda        w001a             1
024130070319    2c                   if        *in30 and w001a>='0'
024140070319     c* Se la data = 20391231, come se non ci fosse
024150070319    3c                   if        §ogdda<>'20391231'
024160070319     c                   movel     §ogdda        w0080             8 0
024170070426     c* Se <=oggi, attiva procedura distinte automatiche
024180070426    4c                   if        w0080<=dateu
024190070319     c                   movel     'S'           FGSddaSI          1
024200070319    4c                   endif
024210070319    3c                   endif
024220070319    2c                   endif
024230070319    1c                   endif
024240070319     c
024250070319     c                   endif
024260110906     c****               endif
024270151222     c** 10              if        fgsgeot='S'  and dutgeot='S'
024280151222     c                   if        wfile='A' and fgsgeot='S'  and dutgeot='S'
024290140213     c                   seton                                        77
024300140213     c                   endif
024310070329     c* Se p.o. utente o lna hanno GEO attiva, propongo F17 note
024320140130     c* non le propongo se bolla in consegna variazione da inviare a AUT e
024330140130     c* filiale partita con la nuova gestione telefonate AUT
024340151222     c** 10              if        fgsgeot='S'  and dutgeot='S'
024350151222     c                   if        wfile='A' and fgsgeot='S'  and dutgeot='S'
024360140130     c                             and (
024370140130     c* non è in consegna
024380140130     c                             (arbndc=0 or arbdcm>0)
024390140130     c* filiale distinta non partita con nuova gestione telefonate AUT
024400140130     c                             or (%lookup(%subst
024410140130     c                                 (%editc(arbifp:'X'):7:3):skFil816tel)=0
024420140130     c                              and skfil816tel(1)<>'999')
024430140130     c* Causale variazione da non trasmettere a AUT
024440140130     c                             or d66pda=*blanks
024450140130     c                             )
024460070329     c                   seton                                        87
024470070329     c                   endif
024480941019     C**
024490941019     C* CONSEGNE PARTICOLARI
024500941019     C**
024510941019     C                   MOVEL     *BLANKS       V1DTC1
024520941019     C                   MOVEL     *BLANKS       V1DTC2
024530070914     C                   MOVEL     *BLANKS       V7DTC1
024540070914     C                   MOVEL     *BLANKS       V7DTC2
024550941212    1C     ARBTC1        IFNE      ' '
024560930505     C                   MOVEL     '1P'          COD
024570941019     C                   MOVEL(P)  ARBTC1        KEY
024580941212     C                   EXSR      CTABEL
024590941102     C  N30              MOVEL     TBLUNI        DS1P
024600941102     C   30              CLEAR                   DS1P
024610941102     C                   MOVEL     §1PDES        V1DTC1
024620941220     C                   MOVEL     §1PDES        V7DTC1
024630941102     C*
024640941102     C* SE IMMESSA DALLA PARTENZA E NON UTILIZZABILE IN ARRIVO:PROTEGGO
024650941222    2C   10§1PUPR        IFEQ      'S'
024660941213     C     §1PUAR        ANDNE     'S'
024670941102     C                   SETON                                        11
024680941212    2C                   ENDIF
024690941222     C* O VICEVERSA
024700941222    2C  N10§1PUAR        IFEQ      'S'
024710941222     C     §1PUPR        ANDNE     'S'
024720941222     C                   SETON                                        11
024730941222    2C                   ENDIF
024740941102     C*
024750941212    1C                   ENDIF
024760941019     C*
024770941212    1C     ARBTC2        IFNE      ' '
024780941019     C                   MOVEL     '1P'          COD
024790941213     C                   MOVEL(P)  ARBTC2        KEY
024800941212     C                   EXSR      CTABEL
024810941102     C  N30              MOVEL     TBLUNI        DS1P
024820941102     C   30              CLEAR                   DS1P
024830941102     C                   MOVEL     §1PDES        V1DTC2
024840941220     C                   MOVEL     §1PDES        V7DTC2
024850941102     C*
024860941102     C* SE IMMESSA DALLA PARTENZA E NON UTILIZZABILE IN ARRIVO:PROTEGGO
024870941222    2C   10§1PUPR        IFEQ      'S'
024880941213     C     §1PUAR        ANDNE     'S'
024890941102     C                   SETON                                        12
024900941212    2C                   ENDIF
024910941222     C* O VICEVERSA
024920941222    2C  N10§1PUAR        IFEQ      'S'
024930941222     C     §1PUPR        ANDNE     'S'
024940941222     C                   SETON                                        12
024950941222    2C                   ENDIF
024960941222     C*
024970941212    1C                   ENDIF
024980941019     C**
024990941019     C* SPEDIZIONE GIACENTE
025000941019     C**
025010040316     c                   Clear                   wgcfas
025020040324     c                   Clear                   wgcded
025030040324     c                   Clear                   wgcddm
025040051109     c                   Clear                   dgcpflr
025050141113if  1C**   *IN10         IFEQ      *ON
025060141113     c**   d48cvb        oreq      'CS'
025070141113     c**   d48cvb        oreq      'CR'
025080050331     C     KARB          CHAIN     Tigcp21L                           30        GIACENZA
025090941212    1C     *IN30         IFEQ      *OFF
025100050331     C     GCPFAS        ANDLT     40
025110941019     C     GCPATB        ANDEQ     ' '
025120040316     c                   Eval      wgcfas = GcpFas
025130040324     c                   Eval      wgcded = GcpDed
025140040324     c                   Eval      wgcddm = GcpDdm
025150051109     c                   Movel     gcpflr        dgcpflr
025160941019     C                   SETON                                        04
025170941212    1C                   ENDIF
025180040219      * se non ho trovato il rcd di giacenza in arrivo provo in partenza
025190040219      * solo se sto facendo una variazione 'CS' o 'CR'
025200051109      * questo solo per accendere l'indicatore 04 di giacenza aperta, non serve per la
025210051109      * variazione della data consegna richiesta, questa è possibile solo se la giacenza
025220051109      * è aperta in arrivo!!!!
025230141113      * Ora lo 04 mi serve anche per il controllo dell'abilitazione dei dirottamenti sia
025240141113      * in partenza che in arrivo e quinidi lo faccio sempre
025250141113if  2c***                If        *In30 and
025260141113     c***                          (d48cvb = 'CS' or d48cvb = 'CR')
025270141113if  2c                   If        *In30
025280050331     c     kArb          Chain     Tigcp01l
025290050331if  3c                   If        %Found(Tigcp01l) and GcpFas < 50 and
025300040219     c                             GcpAtb = *Blanks
025310040219     c                   Seton                                        04
025320040316     c                   Clear                   wgcfas
025330040324     c                   Clear                   wgcded
025340040324     c                   Clear                   wgcddm
025350040219    3c                   EndIf
025360040219    2c                   EndIf
025370141113    1C*****              ENDIF
025380051129     C* DETERMINO SE IL VOLUME VDL (CHE EVENTUALMENTE SOSTITUIRA' IL
025390970418     C* VOLUME DA FATTURARE) E' PARZIALE O TOTALE
025400970418     C                   CLEAR                   WFVF
025410050111     C     arbncl        IFGT      ARBNCR
025420970418     C                   MOVEL     'Z'           WFVF                           VOLUME PARZ.
025430970418     C                   ELSE
025440970418     C                   MOVEL     'T'           WFVF                           VOLUME TOTAL
025450970418     C                   ENDIF
025460051129      * Determino se il peso VDL è parziale o totale
025470030122     c                   Clear                   VidTpk
025480030123     c                   Clear                   wfpf
025490030122     c                   If        ArbNcp > *Zeros
025500050111     c                   If        arbncl > ArbNcp
025510030122     c                   Eval      VidTpk = 'Par.'                              Peso Parziale
025520030123     c                   Eval      wfpf = 'Z'
025530030122     c                   Else
025540030122     c                   Eval      VidTpk = 'Tot.'                              Peso Totale
025550030123     c                   Eval      wfpf = 'T'
025560030122     c                   EndIf
025570030122     c                   EndIf
025580030729      * Recupero i Bancali
025590030729     c                   If        %Subst(ArbGva:1:1) = 'B'
025600030729     c                   Clear                   dAr5Ban
025610030729     c                   Eval      kAr5Trd = 'BAN'
025620030729     c     kAr5          Chain(N)  Fiar501l
025630030729     c                   If        %Found(Fiar501l)
025640030729     c                   Movel     Ar5Uni        dAr5Ban
025650030729     c     §Ar5Nba       Add       §Ar5Nb2       V1cBan
025660030729     c                   EndIf
025670030729     c                   EndIf
025680030729      * Recupero i Bancali e i colli originali
025690030729     c                   If        %Subst(ArbGva:1:1) = 'O'
025700030729     c                   Clear                   dAr5Bnb
025710030729     c                   Eval      kAr5Trd = 'BNB'
025720030729     c     kAr5          Chain(N)  Fiar501l
025730030729     c                   If        %Found(Fiar501l)
025740030729     c                   Movel     Ar5Uni        dAr5Bnb
025750030729     c     §Ar5bNba      Add       §Ar5bNb2      V1cBan
025760030729     c                   Z-Add     §Ar5bCor      V1cCor
025770030729     c                   Eval      *In70 = *On
025780030729     c                   EndIf
025790030729     c                   EndIf
025800130924      * Memorizzo se presente avviso al destinatario per passarlo alla tassazione
025810110629     c                   clear                   wemd
025820130924     c                   Clear                   dAr5emd
025830110629     c                   Eval      kAr5Trd = 'EMD'
025840130924     c     kAr5          chain(N)  Fiar501l
025850130924     c                   if        %found(fiar501l)
025860130924     c                   Movel     Ar5Uni        dAr5EMD
025870130930     c                   eval      wesar5EMD='S'
025880130924     c                   select
025890130924     c                   when      emd_§ar5mmp='S' and emd_§ar5smp=' '
025900110629     c                   eval      wemd='S'
025910130924     c                   when      emd_§ar5mmp='S' and emd_§ar5smp='S'
025920130924     c                   eval      wemd='E'
025930130924     c                   when      emd_§ar5smp='S'
025940130924     c                   eval      wemd='X'
025950130924     c                   endsl
025960110629     c                   endif
025970061107     C**
025980071022     C* F N A R B D 0 F  record del codice fiscale e partita IVA
025990061107     C**
026000061107    1C     VIDCVR        IFEQ      'FI'
026010071022     c*
026020061129     c* Memorizzo se bolla già trasmessa o meno
026030070528     c*  solo in partenza(in arrivo è per forza trasmessa
026040151222     c  n10              movel     arbft2        bollaft2          1
026050151222     c**                 if        wfile='P'
026060151222     c**                 movel     arbft2        bollaft2          1
026070151222     c***                endif
026080071022     c* Dati per partita IVA
026090071022     c
026100071025     c                   clear                   ar4not_P
026110071022     C                   MOVEL     'P'           WTRC
026120061107     C     KARB2         CHAIN(N)  FiAR4000                           30
026130071025     c  n30              movel     ar4not        ar4not_p
026140071022     C                   MOVEL     'Q'           WTRC
026150071022     C     KARB2         CHAIN(N)  FiAR4000                           30
026160061107     c
026170061107     c* Verifico se bolla singola .Se doppia guardo se sono in partenza
026180061107     c*  o arrivo
026190061107     c* carico il mittente se bolla franco in partenza sempre
026200061107     c*  o franco singola in arrivo
026210061107    2c                   if        (flgtb1='F'  and not *in10) or
026220061107     c                             (flgtb1='F'  and §3atb2=*blanks)
026230061107     c                   eval      v11mide='M'
026240061107     c* Mittente
026250061107     c                   eval      v11decmd=CMITT
026260061107     c                   eval      v11cod=arbksc
026270061107     c                   eval      v11rsc=arbrsm
026280061107     c                   eval      wnar=arbnzm
026290071025     c* codice fiscale
026300061107     c                   if        not *in30 and not *in20
026310061107     c                   movel     ar4not        v11cdfis
026320061107     c                   else
026330061107     c                   clear                   v11cdfis
026340061107     c                   endif
026350071025     c* Partita IVA
026360071107     c****               if        not *in20
026370071025     c                   if        %subst(ar4not_P:1:16)<>*blanks
026380071025     c                   movel     ar4not_P      v11pid
026390071025     c                   else
026400071025     c                   movel     arbcpi        v11pid
026410071025     c                   endif
026420071107     c****               endif
026430071025     c
026440061107     c* In arrivo di fatto non posso inserire cod.fiscale su un franco
026450061107     c*    singolo: proteggo il campo
026460061107    3c                   if        *in10 and §3atb2=*blanks
026470061107     c                   seton                                        76
026480061107    3c                   endif
026490061107    2c                   endif
026500061107     c
026510061107     c* Carico il destinatario in arrivo sempre, in partenza se singola
026520061107    2c                   if        (flgtb1='A') or
026530061107     c                             (flgtb1='F'  and §3atb2<>*blanks and
026540061107     c                             *in10)
026550061107     c                   eval      v11mide='D'
026560061107     c                   eval      v11decmd=Cdest
026570061107     c                   eval      v11rsc=arbrsd
026580061107     c                   eval      wnar=arbnzd
026590061107     c                   if        not *in30 and not *in20
026600061107     c                   move      ar4not        v11cdfis
026610061107     c                   else
026620061107     c                   clear                   v11cdfis
026630061107     c                   endif
026640071025     c* Partita IVA
026650071107     c*****              if        not *in20
026660071025     c                   if        %subst(ar4not_P:20:16)<>*blanks
026670071026     c                   move      ar4not_P      v11pid
026680071025     c                   else
026690071107     c                   movel     arbcpi        v11pid
026700071025     c                   endif
026710071107     c*****              endif
026720071025     c* In partenza proteggo sempre
026730061107     c  n10              seton                                        76
026740061107     c* codice destinatario: dal fiar6 prima o seconda bolla
026750061107    3c                   if        §3atb2<>*blanks
026760061107     c                   eval      v11cod=ksc2bol
026770061107   x3c                   else
026780061107     C                   MOVEL     '1'           WTRC
026790061107     C     KARB2         CHAIN(N)  FIAR6000                           30
026800061107     c   30              eval      v11cod=arbksc
026810061107     c  n30              eval      v11cod=ar6ksc
026820061107    3c                   endif
026830061107    2c                   endif
026840061107     C*
026850061107     C* PGM RICHIAMATO
026860061107     c                   if        *in20
026870071025     c* in CPI c'e' il codice fiscale
026880061107     c                   movel     §bdcpi        v11cdfis
026890071107     c* in RSD c'e' la partita IVA solo dalla data attivazione
026900071203     c***                if        dateu>=§vpopiv
026910071025     c                   movel     §bdrsd        v11PID
026920071203     c***                endif
026930071107     c                   endif
026940071025     c
026950071025     c* controllo Partita IVA se privato
026960071025    2c                   if        v11pid<>*blanks
026970071025     C     'PRIVATO'     SCAN      v11pid        posiz                    30
026980071025     c* solo numeri
026990071025     c* "PRIVATO" senza iso
027000071025    3c     *in30         ifeq      *off
027010071025     c     v11isd        andge     *zeros
027020071025     c     *in30         oreq      *on
027030071025     c     posiz         andlt     3
027040071025     c                   movel     v11pid        w016a            16
027050071025     c                   movel     w016a         v11cpd
027060071025     C                   CLEAR                   V11isd
027070071025    3c                   endif
027080071025    2c                   endif
027090061107     c*
027100061107    1c                   endif
027110941019     C**
027120941027     C* F N A R B D 0 F
027130941019     C**
027140941019    1C     FLGCVR        IFEQ      'I'
027150941019     C*
027160941019    2C     VIDCVR        IFEQ      'I0'
027170140407    2C     VIDCVR        oreQ      'I7'
027180140407    2C     VIDCVR        oreQ      'I8'
027190140407    2C     VIDCVR        oreQ      'I9'
027200941019     C                   SETON                                        01          DESTINATAR
027210941019     C                   ELSE
027220941019     C                   SETON                                        02          PESO VOL
027230941212    2C                   END
027240941025     C*
027250941025     C* 2 RAGIONE SOCIALE DESTINATARIO
027260941212     C                   MOVEL     'D'           WTRC
027270060222     C     KARB2         CHAIN(N)  Fiar4000                           30
027280941212     C     *IN30         IFEQ      *OFF
027290941212     C                   MOVEL     'E'           WAR4D             1
027300941212     C* LO SALVO PER LA STORICIZZAZIONE
027310970520     C                   MOVE      AR4NOT        SAVRD2
027320941212     C                   ENDIF
027330061116     C*
027340061116     C* codice fiscale destinatario
027350061116     C                   MOVEL     'Q'           WTRC
027360061116     C     KARB2         CHAIN(N)  Fiar4000                           30
027370061116     C     *IN30         IFEQ      *OFF
027380061116     C                   MOVEL     'E'           WAR4Q
027390061116     C* Se c'e' il cod fiscale del detinatario lo memorizzo
027400061116     C                   MOVE      AR4NOT        vidcdf
027410061116     c                   if        vidcdf<>*blanks
027420061116     C                   MOVEL     'D'           WAR4Q
027430061116     C                   ENDIF
027440061116     C                   ENDIF
027450150206
027460150206     C* LOCALITA' NORMALIZZATA
027470150206     C                   MOVEL     'X'           WTRC
027480150206     C     KARB2         CHAIN(N)  Fiar4000                           30
027490150206     C     *IN30         IFEQ      *OFF
027500150206     C                   MOVEL     'E'           WAR4X             1
027510150206     C                   ENDIF
027520061116     c
027530051129     C* VISUALIZZO ANCHE IL VOLUME VDL
027540941214     C                   MOVEL     ARBVLC        V1CVLC
027550941214     C                   MOVEL     ARBNCR        V1CNCR
027560031120
027570040123      * Reperisco il referente e il telefono e magazzino giacenza
027580070910     c                   If        wesar5gen='S'
027590070910     c                   Eval      VidRef =  PAr5Ref
027600070910     c                   Eval      VidTeld = PAr5Teld
027610070910     c                   Eval      VidZng =  PAr5Zng
027620031120     c                   EndIf
027630141203     c                   eval      savzng = vidzng
027640941019     C*
027650941019     C* PGM NON RICHIAMATO
027660941019    2C     *IN20         IFEQ      *OFF
027670970520     C                   CLEAR                   VIDRSD
027680941025     C                   MOVEL     ARBRSD        VIDRSD
027690941027     C     WAR4D         IFEQ      'E'
027700061116     C                   MOVE      savrd2        VIDRSD
027710941025     C                   ENDIF
027720941019     C                   MOVEL     ARBIND        VIDIND
027730941019     C                   MOVEL     ARBLOD        VIDLOD
027740941019     C                   MOVEL     ARBPRD        VIDPRD
027750941019     C                   MOVEL     ARBCAD        VIDCAD
027760061115     c
027770140428     c*05 off se --> bolla singola in assegnato
027780140428     c*              bolla doppia
027790071025    3C     *IN05         IFEQ      *OFF
027800140428     c* se bolla doppia in partenza la partita iva che ho caricato non è quella del
027810140428     c* destinatario e quindi la carico solo se sono in arrivo (10 on)
027820151222     c***  *in10         ifeq      *on
027830151222     c     wfile         ifeq      'A'
027840140428     c     §3atb2        oreq      *blanks
027850941220     C                   MOVEL     ARBCPI        VIDPID
027860140428     c                   endif
027870071025     C**   'PRIVATO'     SCAN      vidpid        posiz                    30
027880050922     c* solo numeri
027890050922     c* "PRIVATO" senza iso
027900071025    4c**   *in30         ifeq      *off
027910071025     c**   vidisd        andge     *zeros
027920071025     c**   *in30         oreq      *on
027930071025     c**   posiz         andlt     3
027940071025     C**                 CLEAR                   VIDPID
027950071025     c**                 movel     arbcpi        vidcpd
027960071025    4c**                 endif
027970071025    3C                   ENDIF
027980941220     C*
027990030122     c                   Z-Add     ArbPkc        VidPkc
028000941027     C                   MOVEL     ARBPKF        VIDPKF
028010941027     C                   MOVEL     ARBVLF        VIDVLF
028020941027     C                   MOVEL     ARBFVF        VIDFVF
028030941019     C                   MOVEL     ARBFIN        VIDFIN
028040941019     C                   MOVEL     ARBNZD        VIDNZD
028050941019     C* FERMO DEPOSITO
028060941019    3C     ARBFFD        IFEQ      'S'
028070941019     C                   MOVEL     'SI'          VIDFFD
028080941019     C                   ELSE
028090941019     C                   CLEAR                   VIDFFD
028100941019    3C                   ENDIF
028110941019     C**
028120941019   X2C                   ELSE
028130941019     C**
028140941019     C* PROGRAMMA     RICHIAMATO: PRENDO CAMPI DA DSARBD
028150941019     C                   MOVEL     §BDRSD        VIDRSD
028160970520     C                   MOVE      §BDRD2        VIDRSD
028170941019     C                   MOVEL     §BDIND        VIDIND
028180941019     C                   MOVEL     §BDLOD        VIDLOD
028190941019     C                   MOVEL     §BDPRD        VIDPRD
028200941019     C                   MOVEL     §BDCAD        VIDCAD
028210941019     C                   MOVEL     §BDNZD        VIDNZD
028220060308     c                   movel     §bdref        vidref
028230060308     c                   movel     §bdtel        vidteld
028240061115     c                   clear                   vidcdf
028250071025    3C     *IN05         IFEQ      *OFF
028260950102     C                   MOVEL     §BDCPI        VIDPID
028270071025     C**   'PRIVATO'     SCAN      vidpid        posiz                    30
028280050922     c* solo numeri
028290050922     c* "PRIVATO" senza iso
028300071025    4c**   *in30         ifeq      *off
028310071025     c**   vidisd        andge     *zeros
028320071025     c**   *in30         oreq      *on
028330071025     c**   posiz         andlt     3
028340071025     C**                 CLEAR                   VIDPID
028350071025     c**                 movel     §bdcpi        vidcpd
028360071025    4c**                 endif
028370071025    3C                   ENDIF
028380941019     C                   MOVEL     §BDFIN        VIDFIN
028390941027     C                   MOVEL     §BDPKF        VIDPKF
028400941027     C                   MOVEL     §BDVLF        VIDVLF
028410941209     C                   MOVEL     §BDFVF        VIDFVF
028420941019     C* FERMO DEPOSITO
028430141017     c* §BDFFD= 'S' => si fermo deposito; Si alert
028440141017     c* §BDFFD= 'Y' -->si fermo deposito; no alert
028450141017     c* Serve per le chiamate cieche in quanto il campo a video per la gestione
028460141017     c* del contatto (VIDTFD) non è presente nella dsarbd.
028470941019    3C     §BDFFD        IFEQ      'S'
028480141017    3C     §BDFFD        oreq      'Y'
028490941019     C                   MOVEL     'SI'          VIDFFD
028500941019     C                   ELSE
028510941019     C                   CLEAR                   VIDFFD
028520941019    3C                   ENDIF
028530941216    2C                   ENDIF
028540141017      *
028550141017     c                   if        §bdffd='Y'
028560141017     c                   eval      vidtfd='SI'
028570141017     c                   endif
028580071025     c
028590071025     c* controllo Partita IVA se privato
028600071025    2c                   if        vidpid<>*blanks
028610071025     C     'PRIVATO'     SCAN      vidpid        posiz                    30
028620071025     c* solo numeri
028630071025     c* "PRIVATO" senza iso
028640071025    3c     *in30         ifeq      *off
028650071025     c     vidisd        andge     *zeros
028660071025     c     *in30         oreq      *on
028670071025     c     posiz         andlt     3
028680071025     c                   movel     vidpid        w016a
028690071025     c                   movel     w016a         vidcpd
028700071025     C                   CLEAR                   VIDisd
028710071025    3c                   endif
028720071025    2c                   endif
028730991001     C*
028740991001    2C     *IN01         IFEQ      *ON
028750970808     C* MEMORIZZO CAP/PROVINCIA E LOCALITA' DEL DESTINATARIO
028760970808     C* (MI SERVIRA' NELLA ROUTINE DEI CONTROLLI
028770970808     C*
028780970808     C                   MOVE      VIDCAD        SAVCAD                         CAP
028790970808     C                   MOVE      VIDCAD        VARCAD                          "
028800970808     C                   MOVE      VIDCAD        WCAD                            "
028810970808     C                   MOVE      VIDCAD        WCAD1                           "
028820970808     C                   MOVE      VIDLOD        SAVLOD                         LOCALITA'
028830970808     C                   MOVE      VIDLOD        VARLOD                            "
028840970808     C                   MOVE      VIDLOD        WLOD                              "
028850970808     C                   MOVE      VIDLOD        WLOD1                             "
028860060111     c* memorizzo anche gli altri dati del destinatario
028870060111     c                   move      vidrsd        savrsd
028880061116     c                   move      vidrsd        savrsdQ
028890060111     c                   move      vidrsd        wrsd1
028900060111     c                   move      vidind        savind
028910060111     c                   move      vidind        wind1
028920060111     c                   move      vidprd        savprd
028930060111     c                   move      vidprd        wprd1
028940060111     c                   move      vidnzd        savnzd
028950060111     c                   move      vidnzd        wnzd1
028960970808     C*
028970950419     C* SE P.IVA DEL VIDEO = BLANKS LA PRENDO DALL'ESTENSIONE SE
028980950419     C* ESISTE
028990950419     C     VIDPID        IFEQ      *BLANKS
029000950419     C                   MOVEL     'P'           WTRC
029010060222     C     KARB2         CHAIN(N)  Fiar401L                           30
029020950419     C     *IN30         IFEQ      *OFF
029030950419     C                   MOVE      AR4NOT        VIDPID
029040050922     C     'PRIVATO'     SCAN      vidpid        posiz                    30
029050050922     c* solo numeri
029060050922     c* "PRIVATO" senza iso
029070050922     c     *in30         ifeq      *off
029080050922     c     vidisd        andge     *zeros
029090050922     c     *in30         oreq      *on
029100050922     c     posiz         andlt     3
029110050922     C                   CLEAR                   VIDPID
029120050922     c                   move      ar4not        wpiva
029130050922     C                   MOVEL     WPIVA         VIDCPD
029140050922     c                   endif
029150050922     C**   VIDISD        IFGE      *ZEROS
029160050922     C**                 CLEAR                   VIDPID
029170050922     C**                 MOVE      AR4NOT        WPIVA
029180050922     C**                 MOVEL     WPIVA         VIDCPD
029190050922     C**                 ENDIF
029200950419     C                   END
029210950419     C                   END
029220950419     C*
029230160118     C                   SETOFF                                       717276
029240991001     C* SE ASSEGNATO 9999 GIA' CONTABILIZZATO PROTEGGO DATI
029250991001     C* DESTINATARIO (ANCHE LA P.IVA MA SOLO SE DIVERSA DA BLANKS)
029260991001     C                   MOVE      ARBKSC        CO1KSC            4 0
029270991001    3C     CO1KSC        IFEQ      9999
029280991001     C                   CLEAR                   AR6DFT
029290991001     C                   MOVEL     '1'           WTRC
029300991001     C     KARB2         CHAIN(N)  FIAR6000                           30
029310991001    4C     AR6DFT        IFNE      *ZEROS
029320000104     C     AR6DFT        ANDLE     WDTU10
029330991001     C                   SETON                                        71
029340000104    5C     VIDPID        IFNE      *BLANKS
029350000104     C                   SETON                                        72
029360991001    5C                   END
029370991001    4C                   END
029380991001    3C                   END
029390160118     c* Proteggo la partita iva/cod.fiscale anche se dagli arrivi sto manutenzionando BLP
029400160115     c                   if        *in10 and wfile='P'
029410160118     C                   SETON                                        7276
029420160115     c                   endif
029430991001    2C                   END
029440941019     C*
029450941222     C* PROTEGGO VOLUME SE NON VARIABILE
029460941216     C* SE SI TRATTA DI VARIAZIONE DI PESO, PROTEGGO SEMPRE IL VOLUME
029470941216    2C     D48CVB        IFEQ      'I2'
029480941216     C* PROTEGGO IL PESO
029490941216     C                   SETON                                        88
029500941216     C*
029510941019     C                   MOVEL     '7T'          COD
029520051214     C                   MOVEL(P)  VIDFVF        KEY
029530941212     C                   EXSR      CTABEL
029540941212     C   30              SETON                                        89
029550941222     C     *IN30         IFEQ      *OFF
029560941222     C                   MOVEL     TBLUNI        DS7T
029570091106     c
029580151222     c                   if        *in10
029590091106     c                   eval      wvariavol=§7tvra
029600120705     c                   eval      wvariavolEXP=§7tvraEXP
029610091106     c                   else
029620091106     c                   eval      wvariavol=§7tvrp
029630120705     c                   eval      wvariavolEXP=§7tvrPEXP
029640091106     c                   endif
029650091106     c
029660091106     c* Proteggo il volume in arrivo o in partenza se il flag è ' ' oppure
029670091124     c*  non corrisponde il tipo bolla
029680091106     c                   select
029690091106     c                   when      wvariavol=' '
029700091106     C                   SETON                                        89
029710091106     c                   when      Wvariavol='A' and flgtb1='F'
029720091106     C                   SETON                                        89
029730091106     c                   when      Wvariavol='F' and flgtb1='A'
029740091106     C                   SETON                                        89
029750091106     c                   endsl
029760091106     c
029770120705     C* Se  BOLLA EXPORT,  in aggiunta controllo il relativo flag
029780120705     c                   if        *in89 and (lnantw='FED' or
029790120705     c                                        lnantw='EEX' or lnantw='DPD')
029800120705     c                   select
029810120705     c                   when      WvariavolEXP='3'
029820120705     C                   SETOff                                       89
029830120705     c                   when      WvariavolEXP='A' and flgtb1='A'
029840120705     C                   SETOff                                       89
029850120705     c                   when      Wvariavolexp='F' and flgtb1='F'
029860120705     C                   SETOff                                       89
029870120705     c                   endsl
029880120705     c                   endif
029890091106     c
029900941222    3C                   ENDIF
029910941216     C*
029920941216   X2C                   ELSE
029930941216     C                   SETON                                        89
029940941216    2C                   ENDIF
029950030604
029960030604      * se linea arrivo FedEx e il volume è "T" proteggo sempre
029970030604      * (già da tabella "7T" il volume di tipo "T" non è sostituibile,
029980030604      *  faccio lo stesso la modifica nel caso di variazione della tabella)
029990030604     c                   If        *In02 and LnaNtw = 'FED' and vidfvf = 'T'
030000030604     c                   Eval      *In89 = *On
030010030604     c                   EndIf
030020941019    1C                   ENDIF
030030130930     c
030040130930     C**
030050130930     C* F N A R B N 0 F
030060130930     C**
030070130930    1C     FLGCVR        IFEQ      'N'
030080130930
030090130930      * Reperisco il referente e il telefono e magazzino giacenza
030100130930    2c                   if        not  *in20
030110130930
030120130930    3c                   if        d48cvb='NE'
030130131001     c                   seton                                        868483
030140130930     c                   If        wesar5gen='S'
030150130930     c                   Eval      VidRef =  PAr5Ref
030160130930     c                   Eval      VidTeld = PAr5Teld
030170130930     c                   EndIf
030180130930     c                   if        wesar5emd='S'
030190130930     c                   eval      videml = emd_§ar5eml
030200130930     c                   eval      vidtsms= emd_§ar5tel
030210130930     c                   endif
030220130930    3c                   endif
030230130930
030240130930    3c                   if        d48cvb='NT'
030250131001     c                   seton                                        81
030260130930     c                   eval      vidnt1=v1cnt1
030270130930     c                   eval      vidnt2=v1cnt2
030280130930    3c                   endif
030290130930     c
030300130930   x2c                   else
030310130930    3c                   select
030320130930     c
030330131002     c                   when      §bntrd='NOT'
030340131002     c                   if        §bnu_sn='S'
030350131001     c                   seton                                        81
030360130930     c                   movel     §bnuni70      vidnt1
030370130930     c                   move      §bnuni70      vidnt2
030380131002     c                   else
030390131002     C                   MOVEL     MSG(24)       D48MSG
030400131002     C                   MOVEL     '1'           D48ERR
030410131002     c                   leavesr
030420131002     c                   endif
030430131008     c* se la causale variazione non è NT --> errore
030440131008     c                   if        d48cvb<>'NT'
030450131008     C                   MOVEL     MSG(31)       D48MSG
030460131008     C                   MOVEL     '1'           D48ERR
030470131008     c                   leavesr
030480131008     c                   endif
030490131008
030500130930     c                   other
030510131008
030520131008     c* se la causale variazione non è NE --> errore
030530131008     c                   if        d48cvb<>'NE'
030540131008     C                   MOVEL     MSG(31)       D48MSG
030550131008     C                   MOVEL     '1'           D48ERR
030560131008     c                   leavesr
030570131008     c                   endif
030580130930     c
030590131001     c                   if        §bnu_sn='S'
030600130930     c                   eval      videml = §bnuni70
030610131001     c                   seton                                        86
030620131001     c                   endif
030630131001     c                   if        §bnr_sn='S'
030640131001     c                   eval      vidteld= §bntel
030650130930     c                   eval      vidref = §bnref
030660131001     c                   seton                                        84
030670131001     c                   endif
030680131001     c                   if        §bnc_sn='S'
030690131001     c                   eval      vidtsms= §bncel
030700131001     c                   seton                                        83
030710131001     c                   endif
030720131002     c                   if        *in86=*off and *in84=*off and *in83=*off
030730131002     C                   MOVEL     MSG(24)       D48MSG
030740131002     C                   MOVEL     '1'           D48ERR
030750131002     c                   leavesr
030760131002     c                   endif
030770131001     c
030780130930    3c                   endsl
030790130930     c
030800130930    2c                   endif
030810130930    1c                   endif
030820950104     C**
030830990930     C* F I A R B T 0 F
030840950104     C**
030850100120     c
030860941019    1C     FLGCVR        IFEQ      'T'
030870941019     C                   SELECT
030880941019     C*
030890941102     C     D66RTA        WHENEQ    'S'
030900950616     C                   SETON                                        1529        RITASSAZ
030910941102     C*
030920941102     C     D66BSF        WHENEQ    'S'
030930941019     C                   SETON                                        13          VARIA  SF
030940941019     C*
030950941102     C     D66BFI        WHENEQ    'S'
030960991006     C                   SETON                                        1407        VARIA  FAT
030970941019     C*
030980941019     C                   OTHER
030990941019     C                   SETON                                        36          TASS FRANC
031000941019     C                   ENDSL
031010941102     C* PULIZIA SFL
031020941102     C                   SETON                                        85
031030941215     C                   WRITE     LR48C03
031040941102     C                   SETOFF                                       85
031050941215     C*
031060941215     C                   MOVEL     *BLANKS       DESKSC
031070950104     C   10              MOVEL     ARBRSD        W008A             8
031080950104     C   10              MOVEL     W008A         DESKSC
031090950104     C  N10              MOVEL     ARBRSM        DESKSC
031100131120     c   10              clear                   sav_ift
031110941102     C*
031120941102     C*
031130990930     C* TASSAZIONE IN FIAR6
031140990930     C                   MOVEL     '1'           WTRC
031150990930     C     KARB2         CHAIN     FIAR6000                           30
031160990702    2C     *IN30         IFEQ      *OFF
031170990701     C                   MOVEL     'E'           WAR6
031180990701     C                   MOVEL     AR6NFT        VIDNFT
031190990701     C                   MOVEL     AR6FIV        VIDFIV
031200990702     C                   Z-ADD     AR6IMV        SAVIMV
031210151016     C                   Z-ADD     AR6IMV        SAVIMV_fat
031220131120     c   10              z-add     ar6ift        sav_ift
031230990702     C* PROTEZIONE DATI FATTURA PER PREPAGATI
031240990702    3C  N10VIDNFT        IFLT      §QTNFT
031250990719     C     VIDNFT        ANDGT     0
031260990701     C                   SETON                                        23
031270990702     C                   Z-ADD     AR6IFT        VIDIFT
031280990702    3C                   ENDIF
031290991001     C* PROTEZIONE DIVISA PER PREPAGATI
031300991001    3C  N10VIDNFT        IFGT      0
031310991012     C                   SETON                                        0708
031320991001    3C                   ENDIF
031330991108     C* SE PREPAGATO O P.A. FATTURA GIA'CONTAB. MODIFICO SOLO IMP.ASSIC
031340991008     C* E VALORE MERCE DICHIARATO
031350991108    3C     AR6DFT        IFGT      *ZEROS
031360000104     C     AR6DFT        ANDLE     WDTU10
031370991008     C                   SETON                                        2973
031380991008    3C                   ENDIF
031390991029    2C                   ENDIF
031400991029     C**
031410991029     C* SE ASSEGNATO GIA' INCASSATO, FACCIO MODIFICARE SOLO IMP.ASSIC EARIAZIONE
031420991029     C*  VALORE MERCE DICHIARATO
031430130409    1C*  10FLGCVR        IFEQ      'T'
031440130409    2C*    §7LFIN        IFEQ      'S'
031450130409     C*    ARBICA        ANDNE     ' '
031460130409     C*    ARBICA        ANDNE     'R'
031470991029     C* PROTEGGO I CAMPI CHE NON DEVONO ESSERE MANUTENZIONATI
031480130409     C*                  SETON                                        2973
031490130409    2C*                  END
031500991108     C* SE ICA = 'R' E C'E' INCASSO PARZ<IALE IN ARI, PROTEGGO
031510991108     C*  LO STESS, FACCIO SOLO MODIFICARE IAS
031520130409    2C*    ARBICA        IFEQ      'R'
031530130409     C*    KARI          CHAIN     FIARI01L                           30
031540130409    3C* N30ARIATB        IFEQ      ' '
031550991108     C* PROTEGGO I CAMPI CHE NON DEVONO ESSERE MANUTENZIONATI
031560130409     C*                  SETON                                        2973
031570130409    3C*                  ENDIF
031580130409    2C*                  ENDIF
031590130409    1C*                  ENDIF
031600130410     c* PROTEZIONE CAMPI PER POTER VARIARE SOLO IAS E VMD
031610130410     c* Se bolla già contabilizzata e variabile anche dopo contabilizzazione
031620130410     c* oppure se bolla già incassata e variabile anche dopo incasso (V.di fnlr66r)
031630130410     c                   if        d66bdi='S'
031640130409     C                   SETON                                        2973
031650130409     c                   endif
031660990701     C**
031670950807     C* VEDO SE ESISTE AMCHE L'AR7
031680990930     C     KARB2         SETLL     FIAR7000                               30
031690950807     C   30              MOVEL     'E'           WAR7
031700060201     c
031710060201      * Calcolo data limite mod. importo da assicurare
031720060201     c     *iso          Move      ArbDsp        wdataiso
031730060201     c                   adddur    §vpomia:*d    wdataiso
031740060201      *
031750060201     c                   do        *hival
031760060201      * verifico se è un giorno lavorativo
031770060201     c     *iso          move      wdataiso      ds_data
031780060201     C                   eval      kann = ds_anno
031790060201     C                   eval      kmes = ds_mese
031800060201     C     kazcln        chain     azcln01l
031810060201     C                   if        %found(azcln01l)
031820060201     C                   if        MAT(ds_giorno) =  'F'  or
031830060201     C                             POM(ds_giorno) =  'F'
031840060201      * se non va bene aggiungo 1 giorno
031850060201     c                   adddur    1:*d          wdataiso
031860060201     C                   iter
031870060201     c                   else
031880060201     c                   leave
031890060201     C                   endif
031900060201     c                   else
031910060201     c                   leave
031920060201     C                   endif
031930060201
031940060201     c                   enddo
031950060201
031960060201     c                   eval      wdatalimias=wdataiso
031970080625     c                   clear                   forzaias2
031980080625     c                   clear                   forzaias
031990060201     c
032000060201     c* IAS modificabile solo nei primi 2 gg  tassaz sempre
032010060201     c                   if        §utemtf=' '  and
032020060201     c                             wdataoggi>wdatalimIas
032030060201     c                   seton                                        75
032040060201     c                   endif
032050060201     c* solo IAs ma sempre
032060060201     c                   if        §utemtf='I'
032070060201     c                   seton                                        74
032080060201     c                   endif
032090060201     c* Nulla
032100060201     c                   if        §utemtf='N'
032110060201     c                   seton                                        7475
032120060201     c                   endif
032130991012     C*
032140060201     C*
032150060201     C                   CLEAR                   V3CEV1
032160060201     C                   CLEAR                   V3CSV1
032170060201     C                   CLEAR                   V3CVA1
032180060201     C                   CLEAR                   VARVA1
032190060201     C                   Z-ADD     1             NRR
032200060201     C                   DO        20            NRR
032210060201     C                   WRITE     LR48S03
032220060201     C                   ENDDO
032230920128     C* PROGRAMMA NON RICHIAMATO: DATI DA BOLLA
032240941019    2C     *IN20         IFEQ      *OFF
032250941019     C*
032260941019     C                   Z-ADD     ARBIAS        VIDIAS
032270950426     C                   Z-ADD     ARBIAS        COMIAS
032280941209     C                   MOVEL     ARBVAS        VIDVAS
032290941215     C                   MOVEL     ARBVAS        VARVAS
032300941019     C                   MOVEL     ARBKSC        VIDKLP
032310950424     C                   MOVEL     ARBKSC        VARKLP
032320941019     C                   MOVE      ARBKSC        VIDKSC
032330970822     C                   MOVE      ARBKSC        MEMKSC
032340950424     C                   MOVE      ARBKSC        VARKSC
032350941019     C                   MOVEL     ARBCTR        VIDCTR
032360050202     C                   MOVEL     ARBCTR        SAVCTR
032370060511     C                   MOVEL     ARBCTR        CTRBOLLA          3
032380941215     C                   MOVEL     ARBQFT        VIDQFT
032390950424     C                   MOVEL     ARBQFT        VARQFT
032400941215     C                   MOVEL     ARBVMD        VIDVMD
032410941215     C                   MOVEL     ARBVAD        VIDVAD
032420941215     C                   MOVEL     ARBVAD        VARVAD
032430990930     C* TASSAZIONE IN FIAR6
032440941102    3C     WAR6          IFEQ      'E'
032450990930     C                   MOVEL     AR6DIV        VIDDIV
032460991006     C                   MOVEL     AR6DIV        WDIV
032470941019     C                   MOVEL     AR6POR        VIDPOR
032480991006     C                   MOVEL     AR6POR        VARPOR
032490941019     C                   MOVEL     AR6IMV        VIDIMV
032500941019     C                   MOVEL     AR6CEI        VIDCEI
032510941102     C*
032520941102     C                   Z-ADD     0             NRR
032530941209     C                   MOVEL     AR6SV1        WSV1
032540941209     C                   Z-ADD     AR6VA1        WVA1
032550941102     C                   EXSR      CARVAR
032560941209     C                   MOVEL     AR6SV2        WSV1
032570941209     C                   Z-ADD     AR6VA2        WVA1
032580941102     C                   EXSR      CARVAR
032590941212    4C     AR6SV3        IFNE      ' '
032600941209     C                   MOVEL     AR6SV3        WSV1
032610941209     C                   Z-ADD     AR6VA3        WVA1
032620941102     C                   EXSR      CARVAR
032630941102     C*
032640941102     C* VISTO CHE C'E' LA 3 VARIA VEDO SE CE NE SONO ANCORA
032650990930     C     KARB2         READE(N)  FIAR7000                               30
032660941102    5C     *IN30         DOWEQ     *OFF
032670941209     C                   MOVEL     AR7SVN        WSV1
032680941209     C                   MOVEL     AR7VAN        WVA1
032690941102     C                   EXSR      CARVAR
032700990930     C     KARB2         READE(N)  FIAR7000                               30
032710941102    5C                   ENDDO
032720941102    4C                   ENDIF
032730941102    3C                   ENDIF
032740941019     C*
032750941019   X2C                   ELSE
032760920128     C*
032770990930     C* PROGRAMMA     RICHIAMATO: DATI DA DARBT
032780941025     C                   Z-ADD     §BTIAS        VIDIAS
032790950426     C                   Z-ADD     §BTIAS        COMIAS
032800941019     C                   MOVEL     §BTVAS        VIDVAS
032810941220     C                   MOVEL     §BTVAS        VARVAS
032820941209     C                   MOVEL     §BTQFT        VIDQFT
032830950424     C                   MOVEL     §BTQFT        VARQFT
032840941209     C                   MOVEL     §BTVMD        VIDVMD
032850941209     C                   MOVEL     §BTVAD        VIDVAD
032860941220     C                   MOVEL     §BTVAD        VARVAD
032870920128     C                   MOVEL     §BTKSC        VIDKLP
032880950424     C                   MOVEL     §BTKSC        VARKLP
032890920128     C                   MOVE      §BTKSC        VIDKSC
032900970822     C                   MOVE      §BTKSC        MEMKSC
032910950424     C                   MOVE      §BTKSC        VARKSC
032920920128     C                   MOVEL     §BTKSC        SAVKSC            7 0
032930060511     c                   clear                   kscbolla
032940920128     C                   MOVEL     §BTCTR        VIDCTR
032950050202     C                   MOVEL     §BTCTR        SAVCTR
032960060511     c                   clear                   ctrbolla
032970990930     C                   MOVEL     §BTDIV        VIDDIV
032980991006     C                   MOVEL     §BTDIV        WDIV
032990990930     C                   Z-ADD     §BTPOR        VIDPOR
033000991006     C                   Z-ADD     §BTPOR        VARPOR
033010990930     C                   Z-ADD     §BTIMV        VIDIMV
033020920128     C                   MOVEL     §BTCEI        VIDCEI
033030941102     C* CARICO LE VARIE
033040941102     C                   Z-ADD     0             NRR               5 0
033050941209     C                   DO        20            C                 3 0
033060941209     C                   MOVEL     §SV(C)        WSV1
033070941209     C                   Z-ADD     §VA(C)        WVA1
033080941102     C                   EXSR      CARVAR
033090941209     C                   ENDDO
033100941019    2C                   ENDIF
033110131016
033120131016     c                   z-add     vidias        bol_vidias
033130131016     c                   movel     vidvas        bol_vidvas
033140131016     c                   z-add     vidvmd        bol_vidvmd
033150131016     c                   movel     vidvad        bol_vidvad
033160131016     c
033170941215     C*
033180941215     C* CARICO LE TARIFFE E DEOCIFICO IL CTR CHE C'E'I
033190050202     C                   Z-ADD     SAVKSC        KSC
033200941215     C                   EXSR      CARCTR
033210941019     C*
033220920128     C* DECODIFICA ESENZIONE IVA
033230920128     C                   MOVEL     *BLANKS       DESCEI
033240941019    2C     VIDCEI        IFNE      ' '
033250920128     C                   MOVEL     'EI'          COD
033260920128     C                   MOVEL     *BLANKS       KEY
033270051214     C                   MOVEL(P)  VIDCEI        KEY
033280941212     C                   EXSR      CTABEL
033290920128     C  N30              MOVEL     TBLUNI        DESCEI
033300941019    2C                   END
033310920128     C* GIRO DATA FATTURA
033320941102    1C     WAR6          IFEQ      'E'
033330941102     C     AR6DFT        ANDGT     0
033340941019     C                   Z-ADD     AR6DFT        G02INV
033350941019     C                   MOVEL     '3'           G02ERR
033360941019     C                   CALL      'XSRDA8'
033370941019     C                   PARM                    WLBDAT
033380941019     C*
033390941019     C                   Z-ADD     G02DAT        VIDDFT
033400941019    2C                   ENDIF
033410941019    1C                   ENDIF
033420941019     C**
033430991001     C* F I A R B T 0 F   SECONDA BOLLA
033440941019     C**
033450911211     C     FLGCVR        IFEQ      '2'
033460941102     C                   SELECT
033470941102     C     D66RTA        WHENEQ    'S'
033480950616     C                   SETON                                        1529        RITASSAZ
033490941102     C*
033500941102     C     D66BSF        WHENEQ    'S'
033510941102     C                   SETON                                        13          VARIA  SF
033520941102     C*
033530941102     C     D66BFI        WHENEQ    'S'
033540991006     C                   SETON                                        1407        VARIA  FAT
033550941102     C*
033560941102     C                   ENDSL
033570911211     C*
033580911219     C                   MOVEL     *BLANKS       DESKSC
033590941019     C                   MOVEL     ARBRSD        W008A             8
033600941019     C                   MOVEL     W008A         DESKSC
033610131120     c                   clear                   sav_ift
033620941019     C*
033630941019     C* PGM NON RICHIAMATO
033640941019    2C     *IN20         IFEQ      *OFF
033650920128     C* NON ESISTE ESTENSIONE BOLLA
033660990930    3C     WAR62         IFEQ      ' '
033670911211     C                   MOVEL     ARBLNA        VIDKLP
033680911211     C                   MOVEL     9999          VIDKSC
033690911212     C                   MOVEL     *ZEROS        VIDCTR
033700050202     C                   MOVEL     *ZEROS        SAVCTR
033710060511     c                   clear                   ctrbolla
033720991013     C                   MOVE      *ZEROS        SAVKSC            7 0
033730060511     c                   clear                   kscbolla
033740941019     C*
033750941019   X3C                   ELSE
033760941019     C*
033770131120     c                   z-add     ar6ift        sav_ift
033780990930     C                   MOVEL     AR6DIV        VI2DIV
033790020305     C                   MOVEL     AR6DIV        WDIV
033800990930     C                   MOVEL     AR6SV1        VI2SV1
033810040122     C**!!!              Z-ADD     AR6VA1        VI2VA1
033820040122     C**!!!              Z-ADD     AR6VA1        VA2VA1
033830040123      * carico le varie
033840040122     c                   Movel     Ar6Sv1        wsv1
033850040122     c                   Z-add     Ar6Va1        wva1
033860040122     c                   ExSr      CarVar1
033870040122     c                   Movel     Ar6Sv2        wsv1
033880040122     c                   Z-add     Ar6Va2        wva1
033890040122     c                   ExSr      CarVar1
033900040122     c                   Movel     Ar6Sv3        wsv1
033910040122     c                   Z-add     Ar6Va3        wva1
033920040122     c                   ExSr      CarVar1
033930040122      * Verifico se ci sono altre varie
033940040126     c                   Movel     '2'           wtrc
033950040123     c     Karb2         Setll     Fiar701l
033960040122Do  4c                   Do        *Hival
033970040123     c     Karb2         Reade(n)  Fiar701l
033980040122     c                   If        %Eof(Fiar701l)
033990040122     c                   Leave
034000040122     c                   EndIf
034010040122     c                   Movel     Ar7Svn        wsv1
034020040122     c                   Z-add     Ar7Van        wva1
034030040122     c                   ExSr      CarVar1
034040040122    4c                   EndDo
034050990930     C                   MOVEL     AR6CEI        VI2CEI
034060990930     C                   MOVEL     AR6KSC        VIDKLP
034070990930     C                   MOVE      AR6KSC        VIDKSC
034080991013     C                   MOVEL     AR6KSC        SAVKSC
034090060511     C                   MOVEL     AR6KSC        kscbolla
034100990930     C                   MOVEL     AR6CTR        VIDCTR
034110050202     C                   MOVEL     AR6CTR        SAVCTR
034120060511     C                   MOVEL     AR6CTR        ctrbolla
034130941019    3C                   END
034140941019     C*
034150941019   X2C                   ELSE
034160941019     C*
034170990930     C* PROGRAMMA     RICHIAMATO: PRENDO DATI DA DARBT 2BOLLA
034180990930     C                   MOVEL     §BTDIV        VI2DIV
034190991006     C                   MOVEL     §BTDIV        WDIV
034200040123     C**!!!              MOVEL     §BTSV1        VI2SV1
034210040123     C**!!!              Z-ADD     §BTVA1        VI2VA1
034220040123     C**!!!              Z-ADD     §BTVA1        VA2VA1
034230040123      * carico le varie
034240040123     c                   Do        20            c
034250040123     c                   Movel     §sv(c)        wsv1
034260040123     c                   Z-add     §va(c)        wva1
034270040123     c                   ExSr      CarVar1
034280041130     c                   If        ContaSv = 6
034290040123     c                   Leave
034300040123     c                   EndIf
034310040123     c                   EndDo
034320040123
034330990930     C                   MOVEL     §BTCEI        VI2CEI
034340990930     C                   MOVEL     §BTKSC        VIDKLP
034350990930     C                   MOVE      §BTKSC        VIDKSC
034360991013     C                   MOVE      §BTKSC        SAVKSC
034370060511     c                   clear                   kscbolla
034380990930     C                   MOVEL     §BTCTR        VIDCTR
034390050202     C                   MOVEL     §BTCTR        SAVCTR
034400060511     c                   clear                   ctrbolla
034410941019    2C                   END
034420050202     c
034430050202     C* CARICO LE TARIFFE E DEOCIFICO IL CTR CHE C'E'I
034440050202     C                   Z-ADD     SAVKSC        KSC
034450050202     C                   EXSR      CARCTR
034460920128     C*
034470920128     C                   MOVEL     *BLANKS       DESCEI
034480941019    2C     VI2CEI        IFNE      ' '
034490911211     C                   MOVEL     'EI'          COD
034500911211     C                   MOVEL     *BLANKS       KEY
034510051214     C                   MOVEL(P)  VI2CEI        KEY
034520941212     C                   EXSR      CTABEL
034530911211     C  N30              MOVEL     TBLUNI        DESCEI
034540941019    2C                   END
034550941019     C* GIRO DATA FATTURA
034560990930    1C     WAR62         IFEQ      'E'
034570990930     C     AR6DFT        ANDGT     0
034580990930     C                   Z-ADD     AR6DFT        G02INV
034590941019     C                   MOVEL     '3'           G02ERR
034600941019     C                   CALL      'XSRDA8'
034610941019     C                   PARM                    WLBDAT
034620941019     C*
034630941019     C                   Z-ADD     G02DAT        VI2DFT
034640991007     C                   Z-ADD     AR6FIV        VI2FIV
034650991007     C                   Z-ADD     AR6NFT        VI2NFT
034660941019    2C                   ENDIF
034670911211     C                   END
034680941019     C**
034690050202     C* F N A R B K 0 F
034700941019     C**
034710941019    1C     FLGCVR        IFEQ      'K'
034720941019     C*
034730941019     C* CODICE BOLLA PRECEDENTE
034740941019     C                   MOVEL     ARBCBO        OLDCBO
034750941019     C* SALVO CAMPI DEL CODICE BOLLA CHE MI INTERESSANO
034760941019     C                   MOVEL     §3ATB1        SAVTB1            2
034770941102     C                   MOVEL     §3ARBL        SAVRBL            1
034780941019     C                   MOVEL     §3ATB2        SAVTB2            2
034790941019     C                   MOVEL     §3AFCA        SAVFCA            1 0
034800941019     C                   CLEAR                   DENCBO
034810050414     C*
034820050414     C* VERIFICO SE SI TRATTA DI UN PREPAGATO
034830050414     C                   MOVEL     '1'           WTRC
034840050414     C     KARB2         CHAIN(N)  FIAR6000                           30
034850050414     C  N30              MOVEL     'E'           WAR6
034860050414     C*
034870050414     C
034880920128     C*
034890920128     C* PROGRAMMA NON RICHIAMATO: PRENDO DATI DA BOLLA
034900941019    2C     *IN20         IFEQ      *OFF
034910941019     C*
034920941019    3C     WAR9          IFEQ      'E'
034930941019     C                   MOVEL     AR9TIC        VIDTIC
034940941019     C                   MOVEL     AR9CAS        VIDCAS
034950941019     C                   MOVEL     AR9VCA        VIDVCA
034960941215     C                   MOVEL     AR9VCA        VARVCA
034970941102     C                   MOVEL     AR9GCA        VIDGCA
034980941019    3C                   ENDIF
034990941019     C*
035000941019     C                   MOVEL     '?'           NEWCBO
035010920128     C*
035020941019   X2C                   ELSE
035030920128     C*
035040920128     C* PROGRAMMA     RICHIAMATO: PRENDO DATI DA DSARBK
035050920128     C                   MOVEL     §BKTIC        VIDTIC
035060941019     C                   MOVEL     §BKCAS        VIDCAS
035070941102     C                   MOVEL     §BKGCA        VIDGCA
035080941019     C                   MOVEL     §BKCBN        NEWCBO
035090941220     C                   MOVEL     §BKVCA        VARVCA
035100941220     C                   MOVEL     §BKVCA        VIDVCA
035110950420     C* SE NON PASSATO IMPOSTO QUELLO DELLA BOLLA
035120950420     C     NEWCBO        IFEQ      *BLANKS
035130950420     C                   MOVEL     ARBCBO        NEWCBO
035140950420     C                   ENDIF
035150920128     C*
035160920128     C* DECODIFICA NUOVO CODICE BOLLA
035170920128     C                   MOVEL     '3A'          COD
035180950420     C                   MOVEL(P)  NEWCBO        KEY
035190941212     C                   EXSR      CTABEL
035200920128     C  N30              MOVEL     TBLUNI        DS3A
035210941019     C   30              CLEAR                   DS3A
035220941102     C                   MOVEL     §3ADES        DENCBO
035230941019    2C                   END
035240941019     C*
035250920325     C* VARIAZIONE IMPORTO CONTRASSEGNO
035260941019    2C     VIDCVR        IFEQ      'KA'                                          UFFICIO
035270931118     C     VIDCVR        OREQ      'KB'                                          DOPO CONS
035280931118     C     VIDCVR        OREQ      'KP'                                          PADRONCINO
035290920325     C                   SETON                                        21
035300941102     C                   MOVEL     ARBCBO        NEWCBO
035310941019    2C                   END
035320941019    1C                   END
035330941019     C**
035340070309     C* F N A R B G 0 F
035350941019     C**
035360950120     C     FLGCVR        IFEQ      'C'
035370950120     C* SE E' CAUSALE CP PROTEGGO I CAMPI DELLA CONSEGNA RICHIESTA E
035380950120     C*  GG CHIUSURA
035390950120     C     VIDCVR        IFEQ      'CP'
035400950120     C                   SETON                                        16
035410950120     C                   ENDIF
035420941019     C* PGM NON RICHIAMATO
035430941019     C     *IN20         IFEQ      *OFF
035440920320     C* DATA CONSEGNA RICHIESTA
035450941019     C                   Z-ADD     ARBDCR        G02INV
035460920320     C*
035470920320     C                   Z-ADD     ARBHCR        VIDHCR
035480920320     C*
035490920320     C                   MOVEL     ARBTCR        VIDTCR
035500930510     C                   MOVEL     ARBGC1        VIDGC1
035510930510     C                   MOVEL     ARBGC2        VIDGC2
035520941019     C                   MOVEL     ARBTC1        VIDTC1
035530941102     C                   MOVEL     ARBTC2        VIDTC2
035540111025     c* Se il pgm non è richiamato, verifico se nella DS nel campo tipo §BGTCR
035550111025     c*  c'e' una "S" --> significa che posso aggiornare la consegna richiesta
035560111025     c*  in FIARP se bolla in distinta (solo da gestione telefonate o da richiamo
035570111025     c*  con passaggio dati) In caso contrario non posso farlo e sid eve dare errore
035580111207     c* ES - 07/12/11 -  per il momento asterisco
035590111207     c****               eval      Dcrindist=§BGTCR
035600930510     C*
035610941019   X2C                   ELSE
035620930510     C*
035630941019     C                   Z-ADD     §BGDCR        G02INV
035640930510     C*
035650930510     C                   Z-ADD     §BGHCR        VIDHCR
035660930510     C*
035670930510     C                   MOVEL     §BGTCR        VIDTCR
035680930510     C                   MOVEL     §BGGC1        VIDGC1
035690930510     C                   MOVEL     §BGGC2        VIDGC2
035700941019     C                   MOVEL     §BGTC1        VIDTC1
035710941019     C                   MOVEL     §BGTC2        VIDTC2
035720111025     c                   eval      Dcrindist='S'
035730920320     C                   END
035740941019     C*
035750941019     C* GIRO DATA CONSEGNA RICHIESTA
035760941019     C                   MOVEL     '3'           G02ERR
035770941019     C                   CALL      'XSRDA8'
035780941019     C                   PARM                    WLBDAT
035790941019     C                   Z-ADD     G02DAT        VIDDCR
035800070309     c
035810070309     c* Se lna con GEO attiva, verifico per eventuale modifica consegna ric
035820070309     c*  se c'e' sulla bolla un evento per il quale è inseribile la
035830070319     c*  modifica della data consegna richiesta
035840070319     c                   if        FGSgeot='S'
035850070309     c     karb          setgt     fnevb05l
035860070309     c     karb          readpe    fnevb05l
035870070309     c                   dow       not %eof(fnevb05l) and eveADR=' '
035880090526     c* controllo a seconda che sia o meno richiamato il pgm
035890090526     c  n20evbcev        lookup    ADRs                                   30
035900090526     c   20evbcev        lookup    ADRC                                   30
035910070309     c                   if        *in30
035920070309     c                   eval      eveADR='S'
035930070309     c                   else
035940070309     c     karb          readpe    fnevb05l
035950070309     c                   endif
035960070309     c                   enddo
035970070309     c
035980070309     c                   endif
035990070309     c
036000930510     C                   END
036010020531     c* se modificato destinatario in disagiato o supermercato imposto il
036020020531     c* relativo flag nelle cosegne particolari
036030041015     c* Lo faccio senza testare se arbgva = "U" perchè se mi trovo in
036040041015     c* questa condizione è certo che arbgva è <> da "U"
036050070907     c**   wdisag        ifne      *blanks
036060070907     c**                 select
036070070907     c**   vidtc1        wheneq    *blanks
036080070907     c**                 movel     wdisag        vidtc1
036090070907     c**   vidtc2        wheneq    *blanks
036100070907     c**                 movel     wdisag        vidtc2
036110070907     c**                 endsl
036120070907     c**   vidtc1        ifeq      vidtc2
036130070907     c**                 clear                   vidtc2
036140070907     c**                 endif
036150070907     c**                 endif
036160950104     C**
036170070309     C* F N A R B M 0 F
036180950104     C**
036190950104     C     VIDCVR        IFEQ      'MI'
036200950104     C* PGM NON RICHIAMATO: PER ORA NON ESISTE IL RICHIAMO
036210950104     C* DECODIFICO TIPO TARIFFA
036220950104     C                   MOVEL     ARBCTR        VIDCTR
036230050202     C                   MOVEL     ARBCTR        SAVCTR
036240060511     C                   MOVEL     ARBCTR        ctrbolla
036250050202     C                   CLEAR                   Fnlv59DS
036260050202     C                   MOVEL     VIDCTR        ILV59CTR
036270050202     C                   MOVEL     'S'           ILV59CFO
036280050202     C                   CALL      'FNLV59R'
036290050202     C                   PARM                    Fnlv59DS
036300950104     C*
036310050202     C     OLV59ERR      IFEQ      ' '
036320050202     C                   MOVEL     OLV59DFS      DE2CTR
036330950104     C                   ENDIF
036340950104     C                   MOVEL     ARBKSC        VIDKLP
036350950104     C                   MOVE      ARBKSC        VIDKSC
036360970822     C                   MOVE      ARBKSC        MEMKSC
036370950104     C                   MOVEL     ARBRSM        W008A
036380950104     C                   MOVEL     W008A         DESKSC
036390051220     c                   clear                   memntw
036400051220     c     vidklp        chain     azorg01l
036410051220     c                   if        %found(azorg01l)
036420051220     C                   MOVEL     ORGDE3        OG143
036430051220     C                   MOVEL     §OGNTW        memntw
036440051220     c                   endif
036450160225     c* Se lna fedex --> carico le tariffe di cartello FED
036460160225     c
036470970813     C* SE CODICE 8888 CARICO L'INDIRIZZO COMPLETO
036480950104     C     VIDKSC        IFEQ      8888
036490970813     C                   SETOFF                                       22
036500950104     C                   MOVEL     ARBRSM        VIDRSM
036510950104     C                   MOVEL     ARBINM        VIDINM
036520950104     C                   MOVEL     ARBCAM        VIDCAM
036530950104     C                   MOVEL     ARBLOM        VIDLOM
036540970808     C                   MOVEL     ARBCAM        VARCAM
036550970808     C                   MOVEL     ARBLOM        VARLOM
036560950104     C                   MOVEL     ARBPRM        VIDPRM
036570950104     C                   MOVEL     ARBNZM        VIDNZM
036580950104     C                   MOVEL     ARBFAP        VIDFAP
036590950104     C                   MOVEL     ARBCTR        VIDCTR
036600950104     C                   MOVEL     ARBCPI        VIDPIM
036610050922     C     'PRIVATO'     SCAN      vidpim        posiz             2 0    30
036620050922     c* solo numeri
036630050922     c* "PRIVATO senza iso
036640050922     c     *in30         ifeq      *off
036650050922     c     vidism        andge     *zeros
036660050922     c     *in30         oreq      *on
036670050922     c     posiz         andlt     3
036680050922     c                   clear                   vidpim
036690050922     c                   movel     arbcpi        vidcpm
036700050922     c                   endif
036710970813     C                   ELSE
036720970813     C                   SETON                                        22
036730950104     C                   ENDIF
036740950104     C                   ENDIF
036750961211     C**
036760970110     C* VARIAZIONE DEL RIFERIMENTO MITTENTE ALFABETICO E NATURA MERCE
036770961211     C**
036780961211     C     D48CVB        IFEQ      'RM'
036790970110     C                   MOVEL     ARBRMA        VIDRMA
036800970110     C                   MOVEL     ARBNAS        VIDNAS
036810961211     C                   END
036820950104     C*
036830011018      * Se l'importo d'assicurare è a 0 e la divisa non è impostata
036840011018     C     VIDIAS        IFEQ      *ZEROS
036850011018     C     VIDVAS        ANDEQ     *BLANKS
036860011018      * imposto la divisa della nazione del mittente
036870011018      * se estero
036880011018     C     MITVLT        IFNE      *BLANKS
036890011018     C     WMITIE        ANDEQ     'E'
036900011018     C                   MOVEL     MITVLT        VIDVAS
036910011018     C                   MOVEL     MITVLT        VARVAS
036920011018     C                   ELSE
036930011018      * altrimenti imposto la divisa della moneta di conto
036940011017     C                   MOVEL     §GEDCN        VIDVAS
036950011017     C                   MOVEL     §GEDCN        VARVAS
036960011017     C                   ENDIF
036970011018     C                   ENDIF
036980011018      *
036990011017      * Se la divisa e l'importo del valore merce non sono impostati
037000011018     C     VIDVAD        IFEQ      *BLANKS
037010011018     C     VIDVMD        ANDEQ     *ZEROS
037020011018      * imposto la divisa della nazione del mittente
037030011018      * se estero
037040011018     C     MITVLT        IFNE      *BLANKS
037050011018     C     WMITIE        ANDEQ     'E'
037060011018     C                   MOVEL     MITVLT        VIDVAD
037070011018     C                   MOVEL     MITVLT        VARVAD
037080011018     C                   ELSE
037090011018      * altrimenti imposto la divisa della moneta di conto
037100011017     C                   MOVEL     §GEDCN        VIDVAD
037110011017     C                   MOVEL     §GEDCN        VARVAD
037120011017     C                   ENDIF
037130011018     C                   ENDIF
037140011018      *
037150011017      * Se la divisa e l'importo del contrassegno non sono impostati
037160011018     C     VIDVCA        IFEQ      *BLANKS
037170011018     C     VIDCAS        ANDEQ     *ZEROS
037180011018      * imposto la divisa della nazione del destinatario
037190011018      * se estero
037200011018     C     DESVLT        IFNE      *BLANKS
037210011018     C     WDESIE        ANDEQ     'E'
037220011018     C                   MOVEL     DESVLT        VIDVCA
037230011018     C                   MOVEL     DESVLT        VARVCA
037240011018    2C                   ELSE
037250011018      * altrimenti imposto la divisa della moneta di conto
037260011017     C                   MOVEL     §GEDCN        VIDVCA
037270011017     C                   MOVEL     §GEDCN        VARVCA
037280011017     C                   ENDIF
037290011018     C                   ENDIF
037300030203      *------------
037310030124      * Bancali
037320030711      *  solo x causale 'BR'
037330030711If  1c                   If        D48Cvb = 'BR'
037340030124     c                   Clear                   dAr5Ban
037350030124     c                   Eval      kAr5Trd = 'BAN'
037360030725     c     kAr5          Chain     Fiar501l
037370030711     c                   If        %Found(Fiar501l)
037380030124     c                   Movel     Ar5Uni        dAr5Ban
037390030711     c                   EndIf
037400030124     c                   Eval      VidNba = §Ar5Nba
037410030124     c                   Eval      VidNb2 = §Ar5Nb2
037420030124     c                   Eval      VidRb1 = §Ar5Rb1
037430030124     c                   Eval      VidRb2 = §Ar5Rb2
037440030124     c                   Eval      VidNomb = §Ar5Nomb
037450030124     c                   Eval      VidMotb = §Ar5Motb
037460030124If  2c                   If        §Ar5Tba <> *Zeros
037470030612     c                   Clear                   Tibs02DS
037480030124     c                   Clear                   dTba
037490030124     c                   Movel     'C'           T02Mod
037500030124     c                   Movel     Knsif         T02Sif
037510030124     c                   Movel     'TBA'         T02Cod
037520030124     c                   Movel(p)  §Ar5Tba       T02Ke1
037530030124     c                   Call      'TIBS02R'
037540030124     c                   Parm                    Kpjba
037550030612     c                   Parm                    Tibs02DS
037560030124If  3c                   If        T02Err = *Blanks
037570030124     c                   Movel     T02Uni        dTba
037580030124    3c                   EndIf
037590030124     c                   Eval      VidTba = §TbaDesc
037600030124     c                   Eval      VidTbar = §TbaDesc
037610030124    2c                   EndIf
037620030124If  2c                   If        §Ar5Tb2 <> *Zeros
037630030612     c                   Clear                   Tibs02DS
037640030124     c                   Clear                   dTba
037650030124     c                   Movel     'C'           T02Mod
037660030124     c                   Movel     Knsif         T02Sif
037670030124     c                   Movel     'TBA'         T02Cod
037680030124     c                   Movel(p)  §Ar5Tb2       T02Ke1
037690030124     c                   Call      'TIBS02R'
037700030124     c                   Parm                    Kpjba
037710030612     c                   Parm                    Tibs02DS
037720030124If  3c                   If        T02Err = *Blanks
037730030124     c                   Movel     T02Uni        dTba
037740030124    3c                   EndIf
037750030124     c                   Eval      VidTb2 = §TbaDesc
037760030124     c                   Eval      VidTb2r = §TbaDesc
037770030124    2c                   EndIf
037780030711
037790080805     c* Se pgm richiamato, imposto dati dalla DSARBD
037800080805    2c                   if        *in20
037810080805     c                   Eval      VidRb1 = §bdpkf
037820080805     c                   Eval      VidRb2 = §bdvlf
037830080805    3c                   if        §bdrsd<>*blanks
037840080805     c                   Eval      VidNomb = §bdrsd
037850080805    3c                   endif
037860080805    3c                   if        §bdrd2<>*blanks
037870080805     c                   Eval      VidMotb = §bdrd2
037880080805     c                   else
037890080805     c                   Eval      VidMotb = 'PDA  '
037900080805    3c                   EndIf
037910080805
037920080805    2c                   EndIf
037930080805    1c                   EndIf
037940040608      *------------
037950040608      * Supermercati - telefono e referente
037960040608      *  solo x causale 'CS'
037970040608If  1c                   If        D48Cvb = 'CS'
037980040608     c                   Eval      wokscr = *Off
037990040608     c                   Clear                   dAr5Scr
038000040608     c                   Eval      kAr5Trd = 'SCR'
038010040608     c     kAr5          Setgt     Fiar501l
038020040608do  2c                   Do        *Hival
038030040608     c     kAr5          Readpe(n) Fiar501l
038040040608     c                   If        %Eof(Fiar501l)
038050040608     c                   Leave
038060040608     c                   EndIf
038070040608     c                   Movel     Ar5Uni        dAr5Scr
038080040608     c                   Eval      VidNom = §Ar5Nom
038090040608     c                   Eval      VidTel = §Ar5Tel
038100040608     c                   Eval      wokscr = *On
038110040608     c                   Leave
038120040608e   2c                   EndDo
038130040608      * se non ho trovato il rcd SCR cerco i dati sul rcd GEN
038140040608if  2c                   If        wokscr = *Off
038150070910     c***                Clear                   dAr5Gen
038160070910     c***                Eval      kAr5Trd = 'GEN'
038170070910     c***  kAr5          Chain(n)  Fiar501l
038180070910     c***                If        %Found(Fiar501l)
038190070910     c***                Movel     Ar5Uni        dAr5Gen
038200070910     c***                EndIf
038210070910     c***                Eval      VidNom = §Ar5Ref
038220070910     c***                Eval      VidTel = §Ar5Teld
038230070910     c                   Eval      VidNom = PAr5Ref
038240070910     c                   Eval      VidTel = PAr5Teld
038250040608e   2c                   EndIf
038260040608e   1c                   EndIf
038270911209     C*
038280910429     C                   ENDSR
038290941102     C*
038300941102     C* CARICO LA VARIA IN SFL ---------------------------------------*
038310941102     C     CARVAR        BEGSR
038320941102     C*
038330941102     C     WSV1          IFNE      ' '
038340011018      * Controllo se è una bolla di reso e se ha la varia 'N' 88888888
038350011018     C     ARBFBR        IFEQ      'S'
038360011018     C     WSV1          ANDEQ     'N'
038370011018     C     WVA1          ANDEQ     88888888
038380011018     C                   MOVEL     '1'           FLGVRN            1
038390011018     C                   ENDIF
038400990930     C* CARICO SOLO SE VARIA FA PARTE DEL TOTALE IMPONIBILE
038410990930     C     WSV1          LOOKUP    SVA                                    33
038420990930     C     *IN33         IFEQ      *ON
038430941102     C                   ADD       1             NRR
038440941102     C     NRR           CHAIN     LR48S03                            30
038450941102     C                   MOVEL     WSV1          V3CSV1
038460941102     C                   Z-ADD     WVA1          V3CVA1
038470991006     C                   Z-ADD     WVA1          VARVA1
038480941102     C   30              WRITE     LR48S03
038490941102     C  N30              UPDATE    LR48S03
038500941102     C                   ENDIF
038510990930     C                   ENDIF
038520941102     C                   ENDSR
038530040122
038540040122      *---------------------------------------------------------------*
038550040122      * CARICO LA VARIA DELLA SECONDA BOLLA
038560040122      *---------------------------------------------------------------*
038570040122     c     CarVar1       BegSr
038580040122
038590040122if  1c                   If        wsv1 <> *Blanks
038600040122      * Carico la varia solo se fa parte del totale imponibile
038610040122     c     wsv1          Lookup    sva                                    31
038620040122if  2c                   If        *In31
038630040122     c                   add       1             ContaSv
038640040122     c                   add       wva1          TotImv
038650040122
038660040122s   3c                   Select
038670040122     c                   When      ContaSv = 1
038680040122     c                   Movel     wsv1          Vi2Sv1
038690040122     c                   Z-add     wva1          Vi2Va1
038700040122     c                   Z-add     wva1          Va2Va1
038710040122     c                   When      ContaSv = 2
038720040122     c                   Movel     wsv1          Vi2Sv2
038730040122     c                   Z-add     wva1          Vi2Va2
038740040122     c                   Z-add     wva1          Va2Va2
038750040122     c                   When      ContaSv = 3
038760040122     c                   Movel     wsv1          Vi2Sv3
038770040122     c                   Z-add     wva1          Vi2Va3
038780040122     c                   Z-add     wva1          Va2Va3
038790040122     c                   When      ContaSv = 4
038800040122     c                   Movel     wsv1          Vi2Sv4
038810040122     c                   Z-add     wva1          Vi2Va4
038820040122     c                   Z-add     wva1          Va2Va4
038830041130     c                   When      ContaSv = 5
038840041130     c                   Movel     wsv1          Vi2Sv5
038850041130     c                   Z-add     wva1          Vi2Va5
038860041130     c                   Z-add     wva1          Va2Va5
038870041130     c                   When      ContaSv = 6
038880041130     c                   Movel     wsv1          Vi2Sv6
038890041130     c                   Z-add     wva1          Vi2Va6
038900041130     c                   Z-add     wva1          Va2Va6
038910040122    3c                   EndSl
038920040122
038930040122    2c                   EndIf
038940040122
038950040122    1c                   EndIf
038960040122
038970040122     c                   EndSr
038980910429     C*
038990991001     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
039000991001     C     CARWAR        BEGSR
039010991001     C*
039020991001     C                   CLEAR                   WINL
039030991001     C*
039040991001     C                   CLEAR                   WSV
039050991001     C                   CLEAR                   WVA
039060991001     C                   CLEAR                   C
039070991001    1C                   DO        20            NRR
039080991001     C     NRR           CHAIN     LR48S03                            30
039090991001     C*
039100991001    2C     *IN30         IFEQ      *OFF
039110991001     C     V3CSV1        ANDNE     *BLANKS
039120991001     C                   ADD       1             C
039130991001     C                   MOVEL     V3CSV1        WSV(C)
039140991001     C                   Z-ADD     V3CVA1        WVA(C)
039150991001    2C                   ENDIF
039160991001    1C                   ENDDO
039170991001     C* SE RICHIESTA IL CONTROLLO ESISTENZA DELL'INOLTRO PROCEDO
039180991001     C     WCKINL        IFEQ      '1'
039190991001     C                   Z-ADD     1             II
039200991001     C     §$2FIN        LOOKUP    WSV(II)                                30
039210991001     C   30WVA(II)       IFGT      *ZEROS
039220991001     C                   MOVE      '1'           WINL              1
039230991001     C                   ENDIF
039240991001     C                   ENDIF
039250991001     C*
039260991001     C                   ENDSR
039270040123      *---------------------------------------------------------------*
039280040123      * Carico le varie della seconda bolla in una sk di comodo
039290040123      *---------------------------------------------------------------*
039300040123     c     Carwar1       Begsr
039310040123
039320040123     c                   Clear                   wsv
039330040123     c                   Clear                   wva
039340040123     c                   Clear                   c
039350040123
039360040123     c                   If        vi2sv1 <> *Blanks
039370050401     c                   add       1             c
039380050401     c                   Movel     vi2Sv1        wsv(c)
039390050401     c                   Z-add     vi2va1        wva(c)
039400040123     c                   EndIf
039410040123     c                   If        vi2sv2 <> *Blanks
039420050401     c                   add       1             c
039430050401     c                   Movel     vi2Sv2        wsv(c)
039440050401     c                   Z-add     vi2va2        wva(c)
039450040123     c                   EndIf
039460040123     c                   If        vi2sv3 <> *Blanks
039470050401     c                   add       1             c
039480050401     c                   Movel     vi2Sv3        wsv(c)
039490050401     c                   Z-add     vi2va3        wva(c)
039500040123     c                   EndIf
039510040123     c                   If        vi2sv4 <> *Blanks
039520050401     c                   add       1             c
039530050401     c                   Movel     vi2Sv4        wsv(c)
039540050401     c                   Z-add     vi2va4        wva(c)
039550040123     c                   EndIf
039560041130     c                   If        vi2sv5 <> *Blanks
039570050401     c                   add       1             c
039580050401     c                   Movel     vi2Sv5        wsv(c)
039590050401     c                   Z-add     vi2va5        wva(c)
039600041130     c                   EndIf
039610041130     c                   If        vi2sv6 <> *Blanks
039620050401     c                   add       1             c
039630050401     c                   Movel     vi2Sv6        wsv(c)
039640050401     c                   Z-add     vi2va6        wva(c)
039650041130     c                   EndIf
039660040123
039670040123     c                   EndSr
039680991012     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
039690991012     C     RDTASV        BEGSR
039700991012     C*
039710991012     C                   CLEAR                   C
039720030620     C                   CLEAR                   WVAR3A
039730040123     c                   Clear                   wvar3ai
039740991012    1C                   DO        20            NRR
039750991012     C     NRR           CHAIN     LR48S03                            30
039760991012    2C     *IN30         IFEQ      *OFF
039770991012     C                   ADD       1             C
039780020503     c* se la varia è uguale a quella prevista dal tipo bolla, non richiamo
039790020503     c*  piu' la tassazione
039800040123     c                   if        §3asva<>*blanks and v3csv1=§3asva
039810040123     c                   If        vidimv > *Zeros
039820020503     c                   eval      wtassa='N'
039830030620      * mi salvo l'imnporto della varia
039840030620     c                   eval      itassa = v3cva1
039850040123     c                   EndIf
039860030620      * mi salvo un campo di comodo
039870030620     c                   movel     'S'           Wvar3a            1
039880040123      * memorizzo anche se valorizzata
039890040123     c                   If        v3cva1 > 0
039900040123     c                   Eval      wvar3ai = 'S'
039910040123     c                   EndIf
039920030620      *
039930020503     c                   endif
039940030620      * verifico se esiste la varia "D" non la ricalcolo in caso di fattura immediata
039950030620     c                   if        v3csv1 = 'D'
039960030620     c                   eval      wtassad = 'N'
039970030620     c                   endif
039980030620      *
039990030624      * se esiste la varia "&" con tutti 8888888 non la passo al TNSF20R
040000030624      * ma passo un flag  di richiesta calcolo varia "&"
040010030709     C                   if        v3csv1 = '&' and v3cva1 = 88888888 and
040020030709     C                             flgsf20 = 'S'
040030030624     C                   eval      TASFCAA = 'S'
040040030624     C                   else
040050991012     C                   MOVEL     V3CSV1        VSV(C)
040060991012     C                   MOVEL     V3CVA1        VVA(C)
040070030624     c                   endif
040080030624      *
040090991012    2C                   ENDIF
040100991012    1C                   ENDDO
040110991012     C                   ENDSR
040120920120     C*--- EMISSIONE E GESTIONE FORMATO 2 ----------------------------*
040130170130     C     EMFOR2        BEGSR
040140941102     C*
040150170130     c                   clear                   wpes_pre
040160170130     c                   clear                   wvol_pre
040170170130
040180920120     C     FOR02         TAG
040190941102     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
040200950519    1C     *IN90         IFEQ      *ON
040210950114     C     D48FFR        OREQ      'S'
040220950607     C     D48FFR        OREQ      'V'
040230941214     C                   WRITE     LR48T03
040240941014     C                   EXFMT     LR48D02
040250950519    1C                   END
040260941102     C*
040270941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
040280941102     C                   SETOFF                                       2890
040290941102     C                   SETOFF                                       404142
040300941102     C                   SETOFF                                       434445
040310941102     C                   SETOFF                                       464748
040320061115     C                   SETOFF                                       495051
040330140408     C                   SETOFF                                       5268
040340941220     C* F12 - RITORNO
040350941220    1C     *INKL         IFEQ      *OFF
040360070329     c
040370070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
040380070329     c                   if        *in87 and *inkr
040390070329     c                   exsr      CALLNOTELDV
040400070329     c                   goto      for02
040410070329     c                   endif
040420941216     C*
040430941216     C* SE NON POSSO VARIARE NE IL PESO, NE IL VOLUME, NON CONTROLLO
040440941216     C* E NON AGGIORNO
040450941220    2C     *IN88         IFEQ      *OFF
040460941216     C     *IN89         OREQ      *OFF
040470950114     C     *IN24         OREQ      *ON
040480941216     C*
040490941216     C* CONTROLLO FORMATO
040500920120     C                   EXSR      CONTR2
040510941215     C*
040520941220     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
040530941220    3C     *IN90         IFEQ      *ON
040540941215     C     D48FFR        ANDEQ     'E'
040550941215     C                   MOVEL     VIDMSG        D48MSG
040560941215     C                   MOVEL     '2'           D48ERR
040570941215     C                   GOTO      ENDEM2
040580941220    3C                   ENDIF
040590140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
040600140502     c*   (d48ffr='E' e 26 off)
040610140502     c  n90
040620140502     cann26              if        d48ffr='E'
040630140502     C                   GOTO      ENDEM2
040640140502     c                   endif
040650950607     C  NKF
040660950607     CANN26
040670920120     COR 90              GOTO      FOR02
040680060227     c* richiamo routine di richiesta/controllo motivazione
040690060301     c                   if        d66mtv = 'S' and w48mct = *blanks
040700060227     c                   exsr      ges_mtv
040710060301     c                   if        olv85f12 = '1'
040720060301     c                   if        ilv85wdw = ' '
040730060227     c                   goto      for02
040740060301     c                   else
040750060301     c                   eval      *inkl = '1'
040760060301     C                   GOTO      ENDEM2
040770060227     c                   endif
040780060301     c                   endif
040790060227     C     *IN90         IFEQ      *ON
040800060227     C     D48FFR        ANDEQ     'E'
040810060227     C                   MOVEL     VIDMSG        D48MSG
040820060227     C                   MOVEL     '2'           D48ERR
040830060227     C                   GOTO      ENDEM2
040840060227     C                   ENDIF
040850060227     c                   endif
040860920120     C*
040870141020     c* Se Richiesta modifica di indirizzo tale da variare la LNA emetto window
040880141020     c* per segnalare e richiedere una delle seguenti opzioni:
040890141020     c*  -Forzare la variazione senza variare la lna
040900141020     c*  -Richiedere il dirottamento
040910141111    3c                   if        d48ffr<>'E'  and *in01
040920141029     c                             and (%lookup('999':skFilOkdir)>0 or
040930141029     c                             (%lookup(%editc(arblna:'X'):skFilOkdir)>0 and
040940141029     c                              %lookup(%editc(dutpou:'X'):skFilOkdir)>0) )
040950141020     c                   clear                   wdir
040960141020    4C     d_o95lna      ifne      ARBLNA
040970141022     c* le bolle export non le dirottiamo mai da qui
040980141022     c     lnantw        andne     'DPD'
040990141022     c     lnantw        andne     'EEX'
041000141022     c     lnantw        andne     'FED'
041010141219     c* no bolle di recupero
041020141219     c     §3arbl        andne     'R'
041030141204    5C     SAVCAD        IFNE      VIDCAD
041040141204     C     SAVLOD        ORNE      VIDLOD
041050141204     C     SAVrsd        ORNE      VIDrsd
041060141204     C     SAVind        ORNE      VIDind
041070141204     C     SAVprd        ORNE      VIDprd
041080141204     C     SAVnzd        ORNE      VIDnzd
041090141204     c     vidzng        oreq      savzng
041100141203     c     vidffd        andeq     *blanks
041110141021     c                   exsr      ges_wdw02
041120141021     c                   if        winf12='1'
041130141021     c                   goto      for02
041140141021     c                   endif
041150141020    5C                   END
041160141020    4C                   END
041170141020    4c                   if        wdir='1'
041180141212
041190141212     c* se hanno cambiato il Fermo deposito devo anche eseguire la REGIS2
041200141212     c                   if        arbffd<>%subst(vidffD:1:1)
041210141212     c                   eval      wnoindir='S'
041220141212     c                   exsr      regis2
041230141218     C   91              GOTO      FOR02
041240141212     c                   endif
041250141212
041260141020     c                   exsr      sr_tiidc
041270141212
041280141020   x4c                   else
041290141212     c                   eval      wnoindir=' '
041300141020     C                   EXSR      REGIS2
041310141020    4c                   endif
041320141020   x3c                   else
041330141212     c                   eval      wnoindir=' '
041340930517     C                   EXSR      REGIS2
041350141020    3c                   endif
041360991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
041370991130     C     *IN91         IFEQ      *ON
041380991130     C     D48FFR        ANDEQ     'E'
041390991130     C                   MOVEL     VIDMSG        D48MSG
041400991130     C                   MOVEL     '2'           D48ERR
041410991130     C                   GOTO      ENDEM2
041420991130     C                   ENDIF
041430991130     C   91              GOTO      FOR02
041440941216     C*
041450941220   X2C                   ELSE
041460941216     C*
041470941216     C                   GOTO      FOR02
041480941220    2C                   ENDIF
041490941220     C*
041500950518    1C                   ENDIF
041510150112     c* Se non ho passato msg vincolanti, passo msg di avvertimento
041520150112     c*  se ce ne sono
041530150112     c                   if        msgforzatxt<>*blanks
041540150112     C                   MOVEL     msgforzatxt   D48MSG
041550150112     C                   MOVEL     'Z'           D48ERR
041560150112     c                   clear                   msgforzatxt
041570150112     C                   ENDIF
041580950518     C*
041590950518     C     ENDEM2        TAG
041600950518     C*
041610950518    1C     *INKL         IFEQ      *ON
041620950518     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
041630950519    1C     D48FFR        OREQ      'E'
041640950518     C     *IN90         ANDEQ     *ON
041650000303    1C     D48FFR        OREQ      'E'
041660000303     C     *IN91         ANDEQ     *ON
041670941220     C* F12-RITORNO
041680160119    2C     *IN10         IFEQ      *ON
041690941220     C                   UNLOCK    FNARB01L
041700051118     C                   UNLOCK    FiAR901L
041710160119     c                   endif
041720160119     C****               ELSE
041730941222     C                   UNLOCK    FNBLP01L
041740160119    2C****               END
041750991001     C                   UNLOCK    FIAR601L
041760950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
041770950607     C* flag D48F12 = 1
041780950607     C   KL              MOVEL     '1'           D48F12
041790950519    1C                   END
041800941215     C*
041810950518     C                   ENDSR
041820920121     C*
041830920121     C*--- EMISSIONE E GESTIONE FORMATO 3 ----------------------------*
041840920121     C     EMFOR3        BEGSR
041850990415     C*
041860990415     C                   MOVEL     ' '           COMASS
041870941102     C*
041880920121     C     FOR03         TAG
041890941102     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
041900950114     C     *IN90         IFEQ      *ON
041910941212     C     D48FFR        OREQ      'S'
041920950607     C     D48FFR        OREQ      'V'
041930941215     C                   WRITE     LR48T03
041940941212     C                   WRITE     LR48Z03
041950941212     C                   EXFMT     LR48C03
041960941220    1C                   END
041970941102     C*
041980941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
041990941209     C                   SETOFF                                       289052
042000941102     C                   SETOFF                                       404142
042010991006     C                   SETOFF                                       434445
042020941102     C                   SETOFF                                       464748
042030941209     C                   SETOFF                                       495051
042040941215     C                   SETOFF                                       989453
042050961112     C                   SETOFF                                       545556
042060990415     C                   SETOFF                                       82
042070941220     C*
042080941220     C* PER F12-RITORNO O F21 RITASSAZIONE, DISALLOCO I RECORD
042090941220     C* F12-RITORNO
042100941220    1C     *INKL         IFEQ      *ON
042110941220     C     *INKV         OREQ      *ON
042120941222     C*
042130941222     C     *IN10         IFEQ      *ON
042140941220     C                   UNLOCK    FNARB01L
042150051118     C                   UNLOCK    FiAR901L
042160941222     C                   ELSE
042170941222     C                   UNLOCK    FNBLP01L
042180941220    1C                   ENDIF
042190991005     C                   UNLOCK    FIAR601L
042200950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
042210950607     C* flag D48F12 = 1
042220950607     C   KL              MOVEL     '1'           D48F12
042230941222    1C                   ENDIF
042240941220     C* F12 - RITORNO
042250941220    1C  NKL              DO
042260941220     C*
042270941220     C* F21 - RITASSAZIONE BOLLA
042280941220    2C     *INKV         IFEQ      *ON
042290050511     C                   CLEAR                   Fnlra2DS
042300050511     C                   Z-ADD     ARBAAS        ilra2AAS
042310050511     C                   Z-ADD     ARBLNP        ilra2lNP
042320050511     C                   Z-ADD     ARBNRS        ilra2NRS
042330050511     C                   Z-ADD     ARBNSP        ilra2NSP
042340050511     C                   MOVEL     D48FGS        ilra2FGS
042350050511     C                   MOVEL     D48CVB        ilra2CVB
042360050511     C                   MOVEL     Fnlra2DS      KPJBU
042370050511     C                   CALL      'FNLRA2R'
042380941217     C                   PARM                    KPJBA
042390030718     C                   PARM                    TRUL90DS
042400050804     c* Se mi passa errore, lo passo dal lr48 al lr36
042410050804     c                   movel     kpjbu         fnlra2ds
042420050804     C                   MOVEL     olra2msg      D48MSG
042430050804     C                   MOVEL     olra2err      D48ERR
042440170505     c                   if        olra2err=*blanks
042450131119     c                   if        olra2nft>0
042460131119     C                   MOVEL     '1'           WTRC
042470131119     c     karb2         chain(n)  fiar6000
042480131119     c                   endif
042490131120     c* trasmetto variazione a PDA:
042500131120     c* se segue fattura --> solo se prima era una fattura immediata
042510131120     c* se fattura immediata --> solo se variato importo della fattura
042520131119     c                   if        *in10 and ((olra2nft=0  and ar6nft>0)
042530131120     c                              or (olra2nft>0 and (ar6ift<>sav_ift
042540131120     c                              or ar6div<>viddiv)))
042550130416     c                   eval      wpda_ndc=arbndc
042560130416     c                   eval      wpda_ddc=arbddc
042570130416     c                   eval      wpda_dcm=arbdcm
042580140120     c                   eval      arbdtv=olra2dtv
042590140120     c                   eval      arborv=olra2orv
042600140120     c                   eval      arbpru=olra2pru
042610140120     c                   eval      arbcvb=d48cvb
042620130416     c                   exsr      sr_pda
042630130416     c                   endif
042640170411     c* se passato da Fattura Immediata a Segue Fattura memorizzo i dati della vecchia fattura
042650170411     c                   if        olra2nft=0 and ar6nft>0
042660170411     c                   exsr      sr_fnfta
042670170411     c                   endif
042680170505     c                   endif
042690920121     C*
042700941220   X2C                   ELSE
042710920121     C* CONTROLLO FORMATO
042720920121     C                   EXSR      CONTR3
042730941215     C*
042740941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
042750941220    3C     *IN90         IFEQ      *ON
042760941215     C     D48FFR        ANDEQ     'E'
042770941215     C                   MOVEL     VIDMSG        D48MSG
042780941215     C                   MOVEL     '3'           D48ERR
042790941215     C                   GOTO      ENDEM3
042800941220    3C                   ENDIF
042810920320     C*
042820950607     C  NKF
042830950607     CANNKI
042840950607     CANN26
042850990415     COR 82
042860000105     COR KN
042870920320     COR 90              GOTO      FOR03
042880060302     c* richiamo routine di richiesta/controllo motivazione
042890060302     c                   if        d66mtv = 'S' and w48mct = *blanks
042900060302     c                   exsr      ges_mtv
042910060302     c                   if        olv85f12 = '1'
042920060302     c                   if        ilv85wdw = ' '
042930060302     c                   goto      for03
042940060302     c                   else
042950060302     c                   eval      *inkl = '1'
042960060302     C                   GOTO      ENDEM3
042970060302     c                   endif
042980060302     c                   endif
042990060302     C     *IN90         IFEQ      *ON
043000060302     C     D48FFR        ANDEQ     'E'
043010060302     C                   MOVEL     VIDMSG        D48MSG
043020070308     C                   MOVEL     '3'           D48ERR
043030060302     C                   GOTO      ENDEM3
043040060302     C                   ENDIF
043050060302     c                   endif
043060920121     C*
043070920204     C* CMD6 - AGGIORNAMENTO PIU' EVENTUALE STAMPA
043080920204     C                   EXSR      REGIS3
043090991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
043100991130     C     *IN91         IFEQ      *ON
043110991130     C     D48FFR        ANDEQ     'E'
043120991130     C                   MOVEL     VIDMSG        D48MSG
043130991130     C                   MOVEL     '3'           D48ERR
043140991130     C                   GOTO      ENDEM3
043150991130     C                   ENDIF
043160920204     C   91              GOTO      FOR03
043170920204     C*
043180070309     c* ristampo fattura immediata se variata(37 on), se in arrivo
043190070309     c*  (10 on) e se non attiva GEO
043200130321    3C**   *IN37         IFEQ      *ON
043210130321    3C***  *IN10         ANDEQ     *ON
043220130321     c**   FGSddaSI      andeq     ' '
043230130321     C***                CLEAR                   FNLSB5DS
043240130321     C***                MOVEL     'V'           DB0RIS
043250130321     C***                MOVEL     D48TBO        DB0TBO
043260130321     C***                MOVEL     ARBAAS        DB0AAS
043270130321     C***                MOVEL     ARBLNP        DB0LNP
043280130321     C***                MOVEL     ARBNRS        DB0NRS
043290130321     C***                MOVEL     ARBNSP        DB0NSP
043300130321     C***                CALL      D90PSL
043310130321     C***                PARM                    FNLSB5DS
043320130321    3C***                END
043330941212     C*
043340941220    2C                   END
043350941220     C*
043360000303    1C                   END
043370000303     C     ENDEM3        TAG
043380000303     C* SE PGM RICHIAMATO E C'E' ERRORE DISALLOCO
043390000303    1C     D48FFR        IFEQ      'E'
043400000303     C     *IN90         ANDEQ     *ON
043410000303    1C     D48FFR        OREQ      'E'
043420000303     C     *IN91         ANDEQ     *ON
043430000303     C     *IN10         IFEQ      *ON
043440000303     C                   UNLOCK    FNARB01L
043450051118     C                   UNLOCK    FiAR901L
043460000303     C                   ELSE
043470000303     C                   UNLOCK    FNBLP01L
043480000303     C                   ENDIF
043490000303     C                   UNLOCK    FIAR601L
043500000303     C* Comunico al chiamante che è stato premuto F12 valorizzando il
043510000303     C* flag D48F12 = 1
043520000303     C   KL              MOVEL     '1'           D48F12
043530000303    1C                   ENDIF
043540920121     C*
043550000303     C                   ENDSR
043560941102     C*
043570941102     C* EMETTO FORMATO4 ----------------------------------------------*
043580941102     C     EMFOR4        BEGSR
043590941102     C*
043600941102     C     FOR04         TAG
043610941102     C*
043620950114     C     *IN90         IFEQ      *ON
043630941212     C     D48FFR        OREQ      'S'
043640950607     C     D48FFR        OREQ      'V'
043650941102     C                   WRITE     LR48T01
043660941102     C                   EXFMT     LR48D04
043670941102     C                   ENDIF
043680941209     C*
043690941209     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
043700941209     C                   SETOFF                                       289045
043710941209     C                   SETOFF                                       464748
043720991006     C                   SETOFF                                       495051
043730041130     c                   setoff                                       636465
043740041130     c                   setoff                                       6667
043750941220     C*
043760941220     C* PER F12 -RITORNO O F21-RITASSAZIONE SBLOCCO I RECORD
043770941220     C     *INKL         IFEQ      *ON
043780941220     C     *INKV         OREQ      *ON
043790941222     C*
043800941222     C     *IN10         IFEQ      *ON
043810941220     C                   UNLOCK    FNARB01L
043820051118     C                   UNLOCK    FiAR901L
043830941222     C                   ELSE
043840941222     C                   UNLOCK    FNBLP01L
043850941220     C                   ENDIF
043860991005     C                   UNLOCK    FIAR601L
043870950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
043880950607     C* flag D48F12 = 1
043890950607     C   KL              MOVEL     '1'           D48F12
043900941222     C                   ENDIF
043910941220     C* F12 - RITORNO
043920941102     C     *INKL         IFEQ      *OFF
043930050228     C* F21 - RITASSAZIONE BOLLA
043940941220     C     *INKV         IFEQ      *ON
043950050511     C                   CLEAR                   Fnlra2DS
043960050511     C                   Z-ADD     ARBAAS        ilra2AAS
043970050511     C                   Z-ADD     ARBLNP        ilra2LNP
043980050511     C                   Z-ADD     ARBNRS        ilra2NRS
043990050511     C                   Z-ADD     ARBNSP        ilra2NSP
044000050511     C                   MOVEL     D48FGS        ilra2FGS
044010050511     C                   MOVEL     D48CVB        ilra2CVB
044020050511     C                   MOVEL     Fnlra2DS      KPJBU
044030050511     C                   CALL      'FNLRA2R'
044040941102     C                   PARM                    KPJBA
044050030718     C                   PARM                    TRUL90DS
044060050804     c
044070050804     c* Se mi passa errore, lo passo dal lr48 al lr36
044080050804     c                   movel     kpjbu         fnlra2ds
044090050804     C                   MOVEL     olra2msg      D48MSG
044100050804     C                   MOVEL     olra2err      D48ERR
044110170505     c                   if        olra2err=*blanks
044120131119     c                   if        olra2nft>0
044130131119     C                   MOVEL     '2'           WTRC
044140131119     c     karb2         chain(n)  fiar6000
044150131119     c                   endif
044160131120     c* trasmetto variazione a PDA:
044170131120     c* se segue fattura --> solo se prima era una fattura immediata
044180131120     c* se fattura immediata --> solo se variato importo della fattura
044190131120     c                   if        *in10 and ((olra2nft=0  and ar6nft>0)
044200131120     c                              or (olra2nft>0 and (ar6ift<>sav_ift
044210131120     c                              or ar6div<>viddiv)))
044220130416     c                   eval      wpda_ndc=arbndc
044230130416     c                   eval      wpda_ddc=arbddc
044240130416     c                   eval      wpda_dcm=arbdcm
044250140120     c                   eval      arbdtv=olra2dtv
044260140120     c                   eval      arborv=olra2orv
044270140120     c                   eval      arbpru=olra2pru
044280140120     c                   eval      arbcvb=d48cvb
044290130416     c                   exsr      sr_pda
044300130416     c                   endif
044310170411     c* se passato da Fattura Immediata a Segue Fattura memorizzo i dati della vecchia fattura
044320170411     c                   if        olra2nft=0 and ar6nft>0
044330170411     c                   exsr      sr_fnfta
044340170411     c                   endif
044350170505     c                   endif
044360941102     C*
044370941220     C                   ELSE
044380941102     C* CONTROLLO FORMATO
044390941102     C                   EXSR      CONTR4
044400941215     C*
044410941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044420941215     C     *IN90         IFEQ      *ON
044430941215     C     D48FFR        ANDEQ     'E'
044440941215     C                   MOVEL     VIDMSG        D48MSG
044450941215     C                   MOVEL     '4'           D48ERR
044460941215     C                   GOTO      ENDEM4
044470941215     C                   ENDIF
044480941102     C*
044490140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
044500140502     c*   (d48ffr='E' e 26 off)
044510140502     c  n90
044520140502     cann26              if        d48ffr='E'
044530140502     C                   GOTO      ENDEM4
044540140502     c                   endif
044550950607     C  NKF
044560950607     CANNKI
044570950607     CANN26
044580941102     COR 90              GOTO      FOR04
044590060302     c* richiamo routine di richiesta/controllo motivazione
044600060302     c                   if        d66mtv = 'S' and w48mct = *blanks
044610060302     c                   exsr      ges_mtv
044620060302     c                   if        olv85f12 = '1'
044630060302     c                   if        ilv85wdw = ' '
044640060302     c                   goto      for04
044650060302     c                   else
044660060302     c                   eval      *inkl = '1'
044670060302     C                   GOTO      ENDEM4
044680060302     c                   endif
044690060302     c                   endif
044700060302     C     *IN90         IFEQ      *ON
044710060302     C     D48FFR        ANDEQ     'E'
044720060302     C                   MOVEL     VIDMSG        D48MSG
044730070308     C                   MOVEL     '4'           D48ERR
044740060302     C                   GOTO      ENDEM4
044750060302     C                   ENDIF
044760060302     c                   endif
044770941102     C*
044780941102     C* CMD6 - AGGIORNAMENTO PIU' EVENTUALE STAMPA
044790941102     C                   EXSR      REGIS4
044800991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044810991130     C     *IN91         IFEQ      *ON
044820991130     C     D48FFR        ANDEQ     'E'
044830991130     C                   MOVEL     VIDMSG        D48MSG
044840991130     C                   MOVEL     '4'           D48ERR
044850991130     C                   GOTO      ENDEM4
044860991130     C                   ENDIF
044870941102     C   91              GOTO      FOR04
044880941102     C                   END
044890941102     C                   END
044900000303     C*
044910000303     C     ENDEM4        TAG
044920000303    1C     D48FFR        IFEQ      'E'
044930000303     C     *IN90         ANDEQ     *ON
044940000303    1C     D48FFR        OREQ      'E'
044950000303     C     *IN91         ANDEQ     *ON
044960000303     C     *IN10         IFEQ      *ON
044970000303     C                   UNLOCK    FNARB01L
044980051118     C                   UNLOCK    FiAR901L
044990000303     C                   ELSE
045000000303     C                   UNLOCK    FNBLP01L
045010000303     C                   ENDIF
045020000303     C                   UNLOCK    FIAR601L
045030000303     C* Comunico al chiamante che è stato premuto F12 valorizzando il
045040000303     C* flag D48F12 = 1
045050000303     C   KL              MOVEL     '1'           D48F12
045060000303    1C                   ENDIF
045070941102     C*
045080000303     C                   ENDSR
045090941102     C*
045100941102     C* EMETTO E GESTISCO FORMATO5 -----------------------------------*
045110941102     C     EMFOR5        BEGSR
045120950407     C*
045130941102     C     FOR05         TAG
045140941102     C*
045150950114     C     *IN90         IFEQ      *ON
045160941212     C     D48FFR        OREQ      'S'
045170950607     C     D48FFR        OREQ      'V'
045180941102     C                   WRITE     LR48T01
045190941102     C                   EXFMT     LR48D05
045200941102     C                   END
045210941102     C*
045220941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
045230941102     C                   SETOFF                                       289096
045240941102     C                   SETOFF                                       404142
045250991001     C                   SETOFF                                       434492
045260070329     C* F12 - RITORNO
045270941220    1C     *INKL         IFEQ      *OFF
045280070329     c
045290070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
045300070329     c                   if        *in87 and *inkr
045310070329     c                   exsr      CALLNOTELDV
045320070329     c                   goto      for05
045330070329     c                   endif
045340941102     C*
045350941102     C* CONTROLLO FORMATO
045360941213     C                   EXSR      CONTR5
045370941215     C*
045380941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045390941220    2C     *IN90         IFEQ      *ON
045400941215     C     D48FFR        ANDEQ     'E'
045410941215     C                   MOVEL     VIDMSG        D48MSG
045420941215     C                   MOVEL     '5'           D48ERR
045430941215     C                   GOTO      ENDEM5
045440941220    2C                   ENDIF
045450140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
045460140502     c*   (d48ffr='E' e 26 off)
045470140502     c  n90
045480140502     cann26              if        d48ffr='E'
045490140502     C                   GOTO      ENDEM5
045500140502     c                   endif
045510140502     c* Se errore forzabile ma pgm richiamato in modalità 'E' o 'F' cioè
045520140502     c* senza emissione di videata ignoro l'errore forzabile e permetto
045530140502     c* l'aggiornamento
045540140502     c   92              if        d48ffr='E'
045550140502     c                   setoff                                       92
045560140502     c                   endif
045570950607     C  NKF
045580950607     CANN26
045590950407     COR 90
045600950407     COR 92              GOTO      FOR05
045610060302     c* richiamo routine di richiesta/controllo motivazione
045620060302     c                   if        d66mtv = 'S' and w48mct = *blanks
045630060302     c                   exsr      ges_mtv
045640060302     c                   if        olv85f12 = '1'
045650060302     c                   if        ilv85wdw = ' '
045660060302     c                   goto      for05
045670060302     c                   else
045680060302     c                   eval      *inkl = '1'
045690060302     C                   GOTO      ENDEM5
045700060302     c                   endif
045710060302     c                   endif
045720060302     C     *IN90         IFEQ      *ON
045730060302     C     D48FFR        ANDEQ     'E'
045740060302     C                   MOVEL     VIDMSG        D48MSG
045750070308     C                   MOVEL     '5'           D48ERR
045760060302     C                   GOTO      ENDEM5
045770060302     C                   ENDIF
045780060302     c                   endif
045790941102     C*
045800941220     C                   EXSR      REGIS5
045810991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045820991130     C     *IN91         IFEQ      *ON
045830991130     C     D48FFR        ANDEQ     'E'
045840991130     C                   MOVEL     VIDMSG        D48MSG
045850991130     C                   MOVEL     '5'           D48ERR
045860991130     C                   GOTO      ENDEM5
045870991130     C                   ENDIF
045880941213     C   91              GOTO      FOR05
045890941220     C*
045900950518     C                   ENDIF
045910941222     C*
045920950518     C     ENDEM5        TAG
045930950518     C* F12-RITORNO  --> DISALLOCO RECROD
045940950518    1C     *INKL         IFEQ      *ON
045950950518     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045960950518     C     D48FFR        OREQ      'E'
045970950518     C     *IN90         ANDEQ     *ON
045980000303     C     D48FFR        OREQ      'E'
045990000303     C     *IN91         ANDEQ     *ON
046000950518     C*
046010950518    2C     *IN10         IFEQ      *ON
046020941220     C                   UNLOCK    FNARB01L
046030941222     C                   ELSE
046040941222     C                   UNLOCK    FNBLP01L
046050950518    2C                   END
046060051118     C                   UNLOCK    Fiar901L
046070991001     C                   UNLOCK    FIAR601L
046080950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
046090950607     C* flag D48F12 = 1
046100950607     C   KL              MOVEL     '1'           D48F12
046110941222    1C                   END
046120941102     C*
046130950518     C                   ENDSR
046140941102     C*
046150941102     C* EMISSIONE E GESTIONE FORMATO 7 ------------------------------*
046160941102     C     EMFOR7        BEGSR
046170941102     C*
046180941102     C     FOR07         TAG
046190941102     C*
046200950114     C     *IN90         IFEQ      *ON
046210941212     C     D48FFR        OREQ      'S'
046220950607     C     D48FFR        OREQ      'V'
046230941102     C                   WRITE     LR48T01
046240941102     C                   EXFMT     LR48D07
046250941102     C                   END
046260941102     C*
046270941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
046280941102     C                   SETOFF                                       2890
046290941102     C                   SETOFF                                       404142
046300941102     C                   SETOFF                                       434445
046310031001     c                   Setoff                                       62
046320070329     C* F12 - RITORNO
046330941102     C     *INKL         IFEQ      *OFF
046340070329     c
046350070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
046360070329     c                   if        *in87 and *inkr
046370070329     c                   exsr      CALLNOTELDV
046380070329     c                   goto      for07
046390070329     c                   endif
046400941102     C*
046410941102     C* CONTROLLO FORMATO
046420941102     C                   EXSR      CONTR7
046430040219      * questi controlli li devo saltare se il richiamo è stato fatto con causale 'CD'
046440040219      * cioè da chiusura distina
046450040224      * o causale 'CA' creata da LR48 in automatico
046460040224     c                   If        Not *In90 and D48Cvb <> 'CD' and
046470170406     c                             d48cvb <> 'CA'
046480040219     C                   ExSr      Contr72
046490040219     c                   EndIf
046500941215     C*
046510941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
046520070309     c*          non se è forzabile solo allafine dopo la REGIS
046530941215     C     *IN90         IFEQ      *ON
046540941215     C     D48FFR        ANDEQ     'E'
046550070330     c**** Msgforza      andeq     0
046560941215     C                   MOVEL     VIDMSG        D48MSG
046570941215     C                   MOVEL     '7'           D48ERR
046580941215     C                   GOTO      ENDEM7
046590941215     C                   ENDIF
046600070330     c
046610070330     c* Se non ho avuto errori e richiamo solo per controlli, esco
046620070330     c*   (d48ffr='E' e 26 off)
046630070329     c  n90
046640070330     cann26              if        d48ffr='E'
046650070329     C                   GOTO      ENDEM7
046660070329     c                   endif
046670950607     C  NKF
046680950607     CANN26
046690941102     COR 90              GOTO      FOR07
046700060301     c* richiamo routine di richiesta/controllo motivazione
046710060301     c                   if        d66mtv = 'S' and w48mct = *blanks
046720060301     c                   exsr      ges_mtv
046730060301     c* premuto f12-Ritorno
046740060301     c                   if        olv85f12 = '1'
046750060301     c                   if        ilv85wdw = ' '
046760060301     c                   goto      for07
046770060301     c                   else
046780060301     c                   eval      *inkl = '1'
046790060301     C                   GOTO      ENDEM7
046800060301     c                   endif
046810060301     c                   endif
046820060301     c* Errore da ritornare a chiamante
046830060301     C     *IN90         IFEQ      *ON
046840060301     C     D48FFR        ANDEQ     'E'
046850060301     C                   MOVEL     VIDMSG        D48MSG
046860060301     C                   MOVEL     '7'           D48ERR
046870060301     C                   GOTO      ENDEM7
046880060301     C                   ENDIF
046890060301     c                   endif
046900941102     C*
046910070307     C* F6 - AGGIORNAMENTO
046920070307     c* Se devo scrivere file proposte, aggiorno solo FIARP
046930070307     c                   if        AggioARP='S'
046940070307     c                   EXSR      REGISARP
046950070307     c                   endif
046960070308     c
046970070308     c* Aggiorno comunque per gg chiusura e consegne particolari, se
046980070308     c*  modificate, in caso di scrittura di FIARP
046990070308     c                   if        AggioARP=' ' or
047000070308     c                             arbgc1<>vidgc1  or arbgc2<>vidgc2 or
047010151230     c                             arbtc1<>vidtc1 or arbtc2<>vidtc2
047020941102     C                   EXSR      REGIS7
047030070309     c                   else
047040160119     C   10              UNLOCK    FIAR601L
047050160119     C   10              UNLOCK    Fnarb01l
047060160119     C   10              UNLOCK    FiAR901L
047070160119     C                   UNLOCK    Fnblp01l
047080070308     c                   endif
047090070308     c
047100991130     C     *IN91         IFEQ      *ON
047110991130     C     D48FFR        ANDEQ     'E'
047120991130     C                   MOVEL     VIDMSG        D48MSG
047130991130     C                   MOVEL     '7'           D48ERR
047140991130     C                   GOTO      ENDEM7
047150991130     C                   ENDIF
047160991130     C   91              GOTO      FOR07
047170950519     C                   ENDIF
047180070309     c
047190070309     c* Se non ho passato msg vincolanti, passo msg di avvertimento
047200070309     c*  se ce ne sono
047210070309     c                   if        msgforza>0
047220070309     c                   z-aDD     MSGFORZA      XX
047230070309     C                   MOVEL     MSG(XX)       D48MSG
047240070309     C                   MOVEL     'Z'           D48ERR
047250070309     c                   clear                   msgforza
047260070309     C                   ENDIF
047270950519     C*
047280950519     C     ENDEM7        TAG
047290070309     C* F12-RITORNO  --> DISALLOCO REcord
047300941222     C*
047310950519    1C     *INKL         IFEQ      *ON
047320950519     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
047330950519     C     D48FFR        OREQ      'E'
047340950519     C     *IN90         ANDEQ     *ON
047350000303     C     D48FFR        OREQ      'E'
047360000303     C     *IN91         ANDEQ     *ON
047370070329     C     D48FFR        OREQ      'E'
047380070329     c     *in26         andeq     *off
047390950519     C*
047400950519    2C     *IN10         IFEQ      *ON
047410941220     C                   UNLOCK    FNARB01L
047420991005     C                   UNLOCK    FIAR601L
047430051118     C                   UNLOCK    FiAR901L
047440160119     c                   endif
047450160119     C***                ELSE
047460941222     C                   UNLOCK    FNBLP01L
047470160119    2C***                END
047480950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
047490950607     C* flag D48F12 = 1
047500950607     C   KL              MOVEL     '1'           D48F12
047510950519    1C                   END
047520941102     C*
047530950519     C                   ENDSR
047540021129      **
047550021129      * EMISSIONE E GESTIONE FORMATO 7/1 Supermercati ---------------*
047560021129     c     Emfor71       BegSr
047570021129
047580021129     c     For071        Tag
047590021129
047600021129     c                   If        *In90 or D48Ffr = 'S' or D48Ffr = 'V'
047610021129     c                   Write     Lr48T01
047620021129     c                   Exfmt     Lr48D071
047630021129     c                   EndIf
047640021129
047650021129      * Spengo idnicatori di posizionamento cursore
047660021129     c                   Setoff                                       2890
047670021129     c                   Setoff                                       404142
047680021129     c                   Setoff                                       434445
047690021129     c                   Setoff                                       575859
047700031001     c                   Setoff                                       62
047710070329      * F12 - RITORNO
047720021129     c                   IF        Not *InKl
047730070329     c
047740070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
047750070329     c                   if        *in87 and *inkr
047760070329     c                   exsr      CALLNOTELDV
047770070329     c                   goto      for071
047780070329     c                   endif
047790021129
047800021129      * CONTROLLO FORMATO
047810021129     c                   ExSr      Contr7
047820021129
047830021129     c  n90              ExSr      Contr71
047840031028     C  n90              ExSr      Contr72
047850070306     c
047860070306     c* Nel caso dei supermercati devo sempre aggiornare la bolla
047870070306     c*  e mai il file di proposta FIARP per sicurezza lo pulisco
047880070306     c                   clear                   aggioarp
047890070308     c                   clear                   savdcr
047900070308     c                   clear                   savTcr
047910070308     c                   clear                   savHcr
047920070306     c
047930040330      * La data non può superare i gg limiti indicati in tab. VPO
047940040330     c                   If        Not *In90
047950040330     c                   Eval      wdataiso = wdataoggi
047960040330     c                   Adddur    §vpodcr:*d    wdataiso
047970040330     c     *iso          move      wdataiso      ds_data
047980040330     c                   Move      §vpodcr       w003a
047990040330     c                   Movea     w003a         ski(13)
048000040330     c                   Z-add     13            i
048010040330     c                   ExSr      abbl
048020040330     c                   Movea     ski(13)       w003a
048030040330     c                   If        ComDcr > ds_data
048040040330     c                   Eval      *In28 = *On
048050040330     c                   Eval      *In40 = *On
048060040330     c                   Eval      *In90 = *On
048070040330     c                   Eval      VidMsg = msg(129)
048080040330     c                   Eval      VidMsg = %replace(w003a:VidMsg:
048090040330     c                                      %scan('___':VidMsg))
048100040330     c                   EndIf
048110040330     c                   EndIf
048120021129
048130021129      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048140021129     c                   If        *In90 and D48Ffr = 'E'
048150021129     c                   Eval      D48Msg = VidMsg
048160021129     c                   Eval      D48Err = '7'
048170021129     c                   GoTo      EndEm71
048180021129     c                   EndIf
048190140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
048200140502     c*   (d48ffr='E' e 26 off)
048210140502     c  n90
048220140502     cann26              if        d48ffr='E'
048230140502     C                   GOTO      ENDEM71
048240140502     c                   endif
048250021129
048260021129     c  NKF
048270021129     cANN26
048280021129     cOR 90              GoTo      For071
048290060302     c* richiamo routine di richiesta/controllo motivazione
048300060302     c                   if        d66mtv = 'S' and w48mct = *blanks
048310060302     c                   exsr      ges_mtv
048320060302     c                   if        olv85f12 = '1'
048330060302     c                   if        ilv85wdw = ' '
048340060302     c                   goto      for071
048350060302     c                   else
048360060302     c                   eval      *inkl = '1'
048370060302     C                   GOTO      ENDEM71
048380060302     c                   endif
048390060302     c                   endif
048400060302     C     *IN90         IFEQ      *ON
048410060302     C     D48FFR        ANDEQ     'E'
048420060302     C                   MOVEL     VIDMSG        D48MSG
048430070308     C                   MOVEL     '7'           D48ERR
048440060302     C                   GOTO      ENDEM71
048450060302     C                   ENDIF
048460060302     c                   endif
048470021129
048480021129      * CMD6 - AGGIORNAMENTO
048490021129     c                   ExSr      Regis7
048500021129
048510021129     c  n91              ExSr      Regis71
048520021129
048530021129     c                   If        *In91 and D48Ffr = 'E'
048540021129     c                   Eval      D48Msg = VidMsg
048550021129     c                   Eval      D48Err = '7'
048560021129     c                   GoTo      EndEm71
048570021129     c                   EndIf
048580021129
048590021129     c   91              GoTo      For071
048600021129
048610021129     c                   EndIf
048620021129
048630021129     c     EndEm71       Tag
048640021129
048650021129      * F12-RITORNO  --> DISALLOCO RECROD
048660021129
048670021129    1c     *INKL         IFEQ      *ON
048680021129      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048690021129     c     D48FFR        OREQ      'E'
048700021129     c     *IN90         ANDEQ     *ON
048710021129     c     D48FFR        OREQ      'E'
048720021129     c     *IN91         ANDEQ     *ON
048730021129
048740021129    2c     *IN10         IFEQ      *ON
048750021129     c                   UNLOCK    FNARB01L
048760021129     c                   UNLOCK    FIAR601L
048770051118     c                   UNLOCK    FiAR901L
048780021129     c                   ELSE
048790021129     c                   UNLOCK    FNBLP01L
048800021129    2c                   END
048810021129      * Comunico al chiamante che è stato premuto F12 valorizzando il
048820021129      * flag D48F12 = 1
048830021129     c   KL              MOVEL     '1'           D48F12
048840021129    1c                   END
048850021129
048860021129     c                   EndSr
048870941223     C*
048880941223     C* EMISSIONE E GESTIONE FORMATO 8 ------------------------------*
048890941223     C     EMFOR8        BEGSR
048900941223     C*
048910941223     C     FOR08         TAG
048920941223     C*
048930950114     C     *IN90         IFEQ      *ON
048940941223     C     D48FFR        OREQ      'S'
048950950607     C     D48FFR        OREQ      'V'
048960941223     C                   WRITE     LR48T03
048970941223     C                   EXFMT     LR48D08
048980941223     C                   END
048990941223     C*
049000941223     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
049010941223     C                   SETOFF                                       2890
049020941223     C                   SETOFF                                       404142
049030941223     C                   SETOFF                                       434445
049040941223     C                   SETOFF                                       464748
049050941223     C                   SETOFF                                       4950
049060941223     C* CMD12 - RITORNO
049070941223     C     *INKL         IFEQ      *OFF
049080941223     C*
049090941223     C* CONTROLLO FORMATO
049100941223     C                   EXSR      CONTR8
049110941223     C*
049120941223     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049130941223     C     *IN90         IFEQ      *ON
049140941223     C     D48FFR        ANDEQ     'E'
049150941223     C                   MOVEL     VIDMSG        D48MSG
049160941223     C                   MOVEL     '8'           D48ERR
049170941223     C                   GOTO      ENDEM8
049180941223     C                   ENDIF
049190140502
049200140502     c* Non gestita uscita per d48ffr='F' ( n90 and n26 d48ffr='E')
049210140502     c* perchè bisognerebbe verificare bene cosa fare in questo caso
049220140502
049230970110     C  NKF
049240970110     CANNKJ
049250970110     CANN26
049260941223     COR 90              GOTO      FOR08
049270060302     c* richiamo routine di richiesta/controllo motivazione
049280060302     c                   if        d66mtv = 'S' and w48mct = *blanks
049290060302     c                   exsr      ges_mtv
049300060302     c                   if        olv85f12 = '1'
049310060302     c                   if        ilv85wdw = ' '
049320060302     c                   goto      for08
049330060302     c                   else
049340060302     c                   eval      *inkl = '1'
049350060302     C                   GOTO      ENDEM8
049360060302     c                   endif
049370060302     c                   endif
049380060302     C     *IN90         IFEQ      *ON
049390060302     C     D48FFR        ANDEQ     'E'
049400060302     C                   MOVEL     VIDMSG        D48MSG
049410070308     C                   MOVEL     '8'           D48ERR
049420060302     C                   GOTO      ENDEM8
049430060302     C                   ENDIF
049440060302     c                   endif
049450941223     C*
049460970110     C* F6 O F10 - AGGIORNAMENTO
049470941223     C                   EXSR      REGIS8
049480991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049490991130     C     *IN91         IFEQ      *ON
049500991130     C     D48FFR        ANDEQ     'E'
049510991130     C                   MOVEL     VIDMSG        D48MSG
049520991130     C                   MOVEL     '8'           D48ERR
049530991130     C                   GOTO      ENDEM8
049540991130     C                   ENDIF
049550941223     C   91              GOTO      FOR08
049560941223     C*
049570950519     C                   ENDIF
049580150112     c* Se non ho passato msg vincolanti, passo msg di avvertimento
049590150112     c*  se ce ne sono
049600150112     c                   if        msgforzatxt<>*blanks
049610150112     C                   MOVEL     msgforzatxt   D48MSG
049620150112     C                   MOVEL     'Z'           D48ERR
049630150112     c                   clear                   msgforzatxt
049640150112     C                   ENDIF
049650950519     C*
049660950519     C     ENDEM8        TAG
049670950104     C* F12-RITORNO  --> DISALLOCO RECORD
049680950519    1C     *INKL         IFEQ      *ON
049690950519     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049700950519     C     D48FFR        OREQ      'E'
049710950519     C     *IN90         ANDEQ     *ON
049720000303     C     D48FFR        OREQ      'E'
049730000303     C     *IN91         ANDEQ     *ON
049740941223     C*
049750941223     C     *IN10         IFEQ      *ON
049760941223     C                   UNLOCK    FNARB01L
049770991005     C                   UNLOCK    FIAR601L
049780051118     C                   UNLOCK    FiAR901L
049790941223     C                   ELSE
049800941223     C                   UNLOCK    FNBLP01L
049810941223     C                   END
049820950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
049830950607     C* flag D48F12 = 1
049840950607     C   KL              MOVEL     '1'           D48F12
049850941223     C                   END
049860941223     C*
049870950519     C                   ENDSR
049880961211     C* EMISSIONE E GESTIONE FORMATO 9 ------------------------------*
049890961211     C     EMFOR9        BEGSR
049900920120     C*
049910961218     C     FOR09         TAG
049920961218     C*
049930961218     C     *IN90         IFEQ      *ON
049940961218     C     D48FFR        OREQ      'S'
049950961218     C     D48FFR        OREQ      'V'
049960961219     C                   WRITE     LR48T01
049970961218     C                   EXFMT     LR48D09
049980961218     C                   END
049990961218     C*
050000961218     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
050010961218     C                   SETOFF                                       2890
050020970110     C                   SETOFF                                       4041
050030070329     C* F12 - RITORNO
050040961218     C     *INKL         IFEQ      *OFF
050050070329     c
050060070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
050070070329     c                   if        *in87 and *inkr
050080070329     c                   exsr      CALLNOTELDV
050090070329     c                   goto      for09
050100070329     c                   endif
050110070329     C*
050120961218     C*
050130961218     C* CONTROLLO FORMATO
050140961218     C                   EXSR      CONTR9
050150961218     C*
050160961218     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050170961218     C     *IN90         IFEQ      *ON
050180961218     C     D48FFR        ANDEQ     'E'
050190961218     C                   MOVEL     VIDMSG        D48MSG
050200961218     C                   MOVEL     '9'           D48ERR
050210961218     C                   GOTO      ENDEM9
050220961218     C                   ENDIF
050230140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
050240140502     c*   (d48ffr='E' e 26 off)
050250140502     c  n90
050260140502     cann26              if        d48ffr='E'
050270140502     C                   GOTO      ENDEM9
050280140502     c                   endif
050290970110     C  NKF
050300970110     CANN26
050310961218     COR 90              GOTO      FOR09
050320060302     c* richiamo routine di richiesta/controllo motivazione
050330060302     c                   if        d66mtv = 'S' and w48mct = *blanks
050340060302     c                   exsr      ges_mtv
050350060302     c                   if        olv85f12 = '1'
050360060302     c                   if        ilv85wdw = ' '
050370060302     c                   goto      for09
050380060302     c                   else
050390060302     c                   eval      *inkl = '1'
050400060302     C                   GOTO      ENDEM9
050410060302     c                   endif
050420060302     c                   endif
050430060302     C     *IN90         IFEQ      *ON
050440060302     C     D48FFR        ANDEQ     'E'
050450060302     C                   MOVEL     VIDMSG        D48MSG
050460060316     C                   MOVEL     '9'           D48ERR
050470060302     C                   GOTO      ENDEM9
050480060302     C                   ENDIF
050490060302     c                   endif
050500060316     c                   exsr      regis9
050510060316     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050520060316     C     *IN91         IFEQ      *ON
050530060316     C     D48FFR        ANDEQ     'E'
050540060316     C                   MOVEL     VIDMSG        D48MSG
050550060316     C                   MOVEL     '9'           D48ERR
050560060316     C                   GOTO      ENDEM9
050570060316     C                   ENDIF
050580060316     C   91              GOTO      FOR09
050590041028     c
050600961218     C*
050610961218     C                   ENDIF
050620961218     C*
050630961218     C     ENDEM9        TAG
050640961218     C* F12-RITORNO  --> DISALLOCO RECORD
050650961218    1C     *INKL         IFEQ      *ON
050660961218     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050670961218     C     D48FFR        OREQ      'E'
050680961218     C     *IN90         ANDEQ     *ON
050690961218     C*
050700961218     C     *IN10         IFEQ      *ON
050710961218     C                   UNLOCK    FNARB01L
050720991005     C                   UNLOCK    FIAR601L
050730051118     C                   UNLOCK    FiAR901L
050740961218     C                   ELSE
050750961218     C                   UNLOCK    FNBLP01L
050760961218     C                   END
050770961218     C* Comunico al chiamante che è stato premuto F12 valorizzando il
050780961218     C* flag D48F12 = 1
050790961218     C   KL              MOVEL     '1'           D48F12
050800961218     C                   END
050810961211     C*
050820961211     C                   ENDSR
050830030123      **
050840030123      * EMISSIONE E GESTIONE FORMATO 10 Mancato ritiro BANCALI ------*
050850030123     c     Emfor10       BegSr
050860030123
050870030123     c     For10         Tag
050880030123
050890030123     c                   If        *In90 or D48Ffr = 'S' or D48Ffr = 'V'
050900030123     c                   Write     Lr48T01
050910030123     c                   Exfmt     Lr48D10
050920030123     c                   EndIf
050930030123
050940030123      * Spengo indicatori di posizionamento cursore
050950030123     c                   Setoff                                       2890
050960030124     c                   Setoff                                       5759
050970030124     c                   Setoff                                       6061
050980070329      * F12 - RITORNO
050990030123     c                   IF        Not *InKl
051000070329     c
051010070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
051020070329     c                   if        *in87 and *inkr
051030070329     c                   exsr      CALLNOTELDV
051040070329     c                   goto      for10
051050070329     c                   endif
051060030123
051070030123      * CONTROLLO FORMATO
051080030123     c                   ExSr      Contr10
051090030123
051100030123      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051110030123     c                   If        *In90 and D48Ffr = 'E'
051120030123     c                   Eval      D48Msg = VidMsg
051130030123     c                   Eval      D48Err = 'B'
051140030123     c                   GoTo      EndEm10
051150030123     c                   EndIf
051160140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
051170140502     c*   (d48ffr='E' e 26 off)
051180140502     c  n90
051190140502     cann26              if        d48ffr='E'
051200140502     C                   GOTO      ENDEM10
051210140502     c                   endif
051220030123
051230030123     c  NKF
051240030123     cANN26
051250030123     cOR 90              GoTo      For10
051260060302     c* richiamo routine di richiesta/controllo motivazione
051270060302     c                   if        d66mtv = 'S' and w48mct = *blanks
051280060302     c                   exsr      ges_mtv
051290060302     c                   if        olv85f12 = '1'
051300060302     c                   if        ilv85wdw = ' '
051310060302     c                   goto      for10
051320060302     c                   else
051330060302     c                   eval      *inkl = '1'
051340060302     C                   GOTO      ENDEM10
051350060302     c                   endif
051360060302     c                   endif
051370060302     C     *IN90         IFEQ      *ON
051380060302     C     D48FFR        ANDEQ     'E'
051390060302     C                   MOVEL     VIDMSG        D48MSG
051400070308     C                   MOVEL     'B'           D48ERR
051410060302     C                   GOTO      ENDEM10
051420060302     C                   ENDIF
051430060302     c                   endif
051440030123
051450030123      * CMD6 - AGGIORNAMENTO
051460030123     c                   ExSr      Regis10
051470030123
051480030123     c                   If        *In91 and D48Ffr = 'E'
051490030123     c                   Eval      D48Msg = VidMsg
051500030123     c                   Eval      D48Err = 'B'
051510030123     c                   GoTo      EndEm10
051520030123     c                   EndIf
051530030123
051540030123     c   91              GoTo      For10
051550030123
051560030123     c                   EndIf
051570030123
051580030123     c     EndEm10       Tag
051590030123
051600030123      * F12-RITORNO  --> DISALLOCO RECROD
051610030123
051620030123    1c     *INKL         IFEQ      *ON
051630030123      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051640030123     c     D48FFR        OREQ      'E'
051650030123     c     *IN90         ANDEQ     *ON
051660030123     c     D48FFR        OREQ      'E'
051670030123     c     *IN91         ANDEQ     *ON
051680030123
051690030123    2c     *IN10         IFEQ      *ON
051700030123     c                   UNLOCK    FNARB01L
051710030123     c                   UNLOCK    FIAR601L
051720051118     c                   UNLOCK    FiAR901L
051730030124     c                   UNLOCK    Fiar501L
051740030123     c                   ELSE
051750030123     c                   UNLOCK    FNBLP01L
051760030123    2c                   END
051770030123      * Comunico al chiamante che è stato premuto F12 valorizzando il
051780030123      * flag D48F12 = 1
051790030123     c   KL              MOVEL     '1'           D48F12
051800030123    1c                   END
051810030123
051820030123     c                   EndSr
051830061107     C
051840061107     C*--- EMISSIONE E GESTIONE FORMATO 11 ---------------------------*
051850061107     C     EMFOR11       BEGSR
051860061107     C*
051870061107     C     FOR11         TAG
051880061107     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
051890061107    1C     *IN90         IFEQ      *ON
051900061107     C     D48FFR        OREQ      'S'
051910061107     C     D48FFR        OREQ      'V'
051920061107     C                   WRITE     LR48T01
051930061107     C                   EXFMT     LR48D11
051940061107    1C                   END
051950061107     C*
051960061107     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
051970061107     C                   SETOFF                                       2890
051980061107     C                   SETOFF                                       4041
051990061107     C* F12 - RITORNO
052000061107    1C     *INKL         IFEQ      *OFF
052010061107     C*
052020061107     C* CONTROLLO FORMATO
052030061107     C                   EXSR      CONTR11
052040061107     C*
052050061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052060061107    3C     *IN90         IFEQ      *ON
052070061107     C     D48FFR        ANDEQ     'E'
052080061107     C                   MOVEL     VIDMSG        D48MSG
052090061107     C                   MOVEL     'F'           D48ERR
052100061107     C                   GOTO      ENDEM11
052110061107    3C                   ENDIF
052120071025     c
052130071025     c* Se non ho avuto errori e richiamo solo per controlli, esco
052140071025     c*   (d48ffr='E' e 26 off)
052150071025     c  n90
052160071025     cann26              if        d48ffr='E'
052170071025     C                   GOTO      ENDEM11
052180071025     c                   endif
052190061107     C  NKF
052200061107     CANN26
052210061107     COR 90              GOTO      FOR11
052220061107     c* richiamo routine di richiesta/controllo motivazione
052230061107     c                   if        d66mtv = 'S' and w48mct = *blanks
052240061107     c                   exsr      ges_mtv
052250061107     c                   if        olv85f12 = '1'
052260061107     c                   if        ilv85wdw = ' '
052270061107     c                   goto      for11
052280061107     c                   else
052290061107     c                   eval      *inkl = '1'
052300061107     C                   GOTO      ENDEM11
052310061107     c                   endif
052320061107     c                   endif
052330061107     C     *IN90         IFEQ      *ON
052340061107     C     D48FFR        ANDEQ     'E'
052350061107     C                   MOVEL     VIDMSG        D48MSG
052360061107     C                   MOVEL     'F'           D48ERR
052370061107     C                   GOTO      ENDEM11
052380061107     C                   ENDIF
052390061107     c                   endif
052400061107     C*
052410071026     c                   clear                   Aggiobol
052420061107     C                   EXSR      REGIS11
052430061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052440061107     C     *IN91         IFEQ      *ON
052450061107     C     D48FFR        ANDEQ     'E'
052460061107     C                   MOVEL     VIDMSG        D48MSG
052470061107     C                   MOVEL     'F'           D48ERR
052480061107     C                   GOTO      ENDEM11
052490061107     C                   ENDIF
052500061107     C   91              GOTO      FOR11
052510061107     C*
052520061107    1C                   ENDIF
052530061107     C*
052540061107     C     ENDEM11       TAG
052550061107     C*
052560061107    1C     *INKL         IFEQ      *ON
052570061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052580061107    1C     D48FFR        OREQ      'E'
052590061107     C     *IN90         ANDEQ     *ON
052600061107    1C     D48FFR        OREQ      'E'
052610061107     C     *IN91         ANDEQ     *ON
052620061107     C* F12-RITORNO
052630061107    2C     *IN10         IFEQ      *ON
052640061107     C                   UNLOCK    FNARB01L
052650061107     C                   UNLOCK    FiAR901L
052660061107     C                   ELSE
052670061107     C                   UNLOCK    FNBLP01L
052680061107    2C                   END
052690061107     C* Comunico al chiamante che è stato premuto F12 valorizzando il
052700061107     C* flag D48F12 = 1
052710061107     C   KL              MOVEL     '1'           D48F12
052720061107    1C                   END
052730061107     c
052740061107     C                   UNLOCK    Fiar601l
052750061107     C*
052760061107     C                   ENDSR
052770130927     c
052780130927     C*--- EMISSIONE E GESTIONE FORMATO 12  variaz.'"Nx' -------------*
052790130927     C     EMFOR12       BEGSR
052800130927     C*
052810130927     C     FOR12         TAG
052820130927     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
052830130927    1C     *IN90         IFEQ      *ON
052840130927     C     D48FFR        OREQ      'S'
052850130927     C     D48FFR        OREQ      'V'
052860130927     C                   WRITE     LR48T01
052870130927     C                   EXFMT     LR48D12
052880130927    1C                   END
052890130927     C*
052900130927     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
052910130927     C                   SETOFF                                       2890
052920130930     C                   SETOFF                                       41
052930130927     C* F12 - RITORNO
052940130927    1C     *INKL         IFEQ      *OFF
052950130927     c
052960130927     c* F17 - Note LDV   (solo se abilitato geo 87 on)
052970130927     c                   if        *in87 and *inkr
052980130927     c                   exsr      CALLNOTELDV
052990130927     c                   goto      for12
053000130927     c                   endif
053010130927     C*
053020130927     C* CONTROLLO FORMATO
053030130927     C                   EXSR      CONTR12
053040130927     C*
053050130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
053060130927    3C     *IN90         IFEQ      *ON
053070130927     C     D48FFR        ANDEQ     'E'
053080130927     C                   MOVEL     VIDMSG        D48MSG
053090130927     C                   MOVEL     'F'           D48ERR
053100130927     C                   GOTO      ENDEM12
053110130927    3C                   ENDIF
053120130927     c
053130130927     c* Se non ho avuto errori e richiamo solo per controlli, esco
053140130927     c*   (d48ffr='E' e 26 off)
053150130927     c  n90
053160130927     cann26              if        d48ffr='E'
053170130927     C                   GOTO      ENDEM12
053180130927     c                   endif
053190130927     C  NKF
053200130927     CANN26
053210130930     COR 90              GOTO      FOR12
053220130927     c* richiamo routine di richiesta/controllo motivazione
053230130927     c                   if        d66mtv = 'S' and w48mct = *blanks
053240130927     c                   exsr      ges_mtv
053250130927     c                   if        olv85f12 = '1'
053260130927     c                   if        ilv85wdw = ' '
053270130927     c                   goto      for12
053280130927     c                   else
053290130927     c                   eval      *inkl = '1'
053300130927     C                   GOTO      ENDEM12
053310130927     c                   endif
053320130927     c                   endif
053330130927     C     *IN90         IFEQ      *ON
053340130927     C     D48FFR        ANDEQ     'E'
053350130927     C                   MOVEL     VIDMSG        D48MSG
053360130927     C                   MOVEL     'F'           D48ERR
053370130927     C                   GOTO      ENDEM12
053380130927     C                   ENDIF
053390130927     c                   endif
053400130927     C*
053410130927     c                   clear                   Aggiobol
053420130927     C                   EXSR      REGIS12
053430130927     c
053440130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
053450130927     C     *IN91         IFEQ      *ON
053460130927     C     D48FFR        ANDEQ     'E'
053470130927     C                   MOVEL     VIDMSG        D48MSG
053480130927     C                   MOVEL     'F'           D48ERR
053490130927     C                   GOTO      ENDEM12
053500130927     C                   ENDIF
053510130927     C   91              GOTO      FOR12
053520130927     C*
053530130927    1C                   ENDIF
053540130927     C*
053550130927     C     ENDEM12       TAG
053560130927     C*
053570130927    1C     *INKL         IFEQ      *ON
053580130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
053590130927    1C     D48FFR        OREQ      'E'
053600130927     C     *IN90         ANDEQ     *ON
053610130927    1C     D48FFR        OREQ      'E'
053620130927     C     *IN91         ANDEQ     *ON
053630130927     C* F12-RITORNO
053640130927    2C     *IN10         IFEQ      *ON
053650130927     C                   UNLOCK    FNARB01L
053660130927     C                   UNLOCK    FiAR901L
053670130927     C                   ELSE
053680130927     C                   UNLOCK    FNBLP01L
053690130927    2C                   END
053700130927     C* Comunico al chiamante che è stato premuto F12 valorizzando il
053710130927     C* flag D48F12 = 1
053720130927     C   KL              MOVEL     '1'           D48F12
053730130927    1C                   END
053740130927     c
053750130927     C                   UNLOCK    Fiar601l
053760130927     C*
053770130927     C                   ENDSR
053780070329     C
053790070329     C*--- richiama pgm per immettere note LDV -----------------------*
053800070329     C     CALLNoteLDV   BEGSR
053810140130     c                   clear                   fnlrn6ds
053820140130     c                   eval      in6mod='I'
053830140130     c                   eval      in6aas=arbaas
053840140130     c                   eval      in6lnp=arblnp
053850140130     c                   eval      in6nrs=arbnrs
053860140130     c                   eval      in6nsp=arbnsp
053870140130     c                   eval      in6pgm='FNLR48R'
053880140130     c                   eval      IN6FGS=arbifp
053890140130     c                   eval      IN6NFV=arbndc
053900140130     c                   eval      IN6DFV=arbddc
053910140130     c                   eval      IN6PDR=arbpdc
053920140130     c                   if        arbdcm=0  and arbndc>0
053930140130     c                   eval      IN6CONS='S'
053940140130     c                   endif
053950140130     c                   call      'FNLRN6R'
053960070329     c                   parm                    kpjba
053970140130     c                   parm                    fnlrn6ds
053980070329     c
053990070329     c                   ENDSR
054000061107     C
054010920120     C*--- CONTROLLI FORMATO2 ----------------------------------------*
054020920120     C     CONTR2        BEGSR
054030941025     C                   SETOFF                                       9028
054040020531     c                   clear                   wdisag
054050030122      * A T T E N Z I O N E!!!
054060030122      * Per la modifica destinatario
054070030122      * Se il nuovo destinatario necessita della consegna particolare
054080030122      * alla fine della REGIS2 viene impostata la causae 'CP' per
054090030122      * poter effettuare una nuova variazione bolla senza uscire dal pgm
054100941020     C***
054110941020     C*** DESTINATARIO  ***
054120941025    1C     *IN01         IFEQ      '1'
054130141112
054140141112     c* messaggio di avviso se pgm non richiamato e bolla con
054150141120     c* giacenza aperta  - Solo se variati dati del destinatario
054160141112     c                   if        *in20=*off and wforzgiac=0
054170141112     c                             and *in04
054180141120    5C     SAVCAD        IFNE      VIDCAD
054190141120     C     SAVLOD        ORNE      VIDLOD
054200141120     C     SAVind        ORNE      VIDind
054210141120     C     SAVprd        ORNE      VIDprd
054220141120     C     SAVnzd        ORNE      VIDnzd
054230141112     c                   eval      wforzgiac=1
054240141112     C                   SETON                                        2890
054250141112     C                   MOVEL     MSG(157)      VIDMSG
054260141112     C                   GOTO      ENDCT2
054270141112     c                   endif
054280141120     c                   endif
054290141112
054300970808     C                   MOVEL     VIDRSD        I14RSC
054310970808     C                   MOVE      VIDRSD        I14RS2
054320970808     C                   MOVEL     VIDIND        I14IND
054330970808     C                   MOVEL     VIDLOD        E14LOC
054340970808     C                   MOVEL     VIDPID        E14PIP
054350970808     C                   MOVEL     VIDPRD        E14PRV
054360970808     C                   MOVEL     VIDNZD        E14NAR
054370970808     C                   MOVEL     VIDCAD        E14CAP
054380970808     C                   MOVEL     VIDFIN        I14ISO
054390970808     C                   MOVEL     'D'           WCLI              1
054400970808     C                   EXSR      CTRIND
054410941020     C*
054420941020     C* CAMPI CHE POSSO AVER IMPOSTATO
054430970808     C                   MOVEL     E14LOC        VIDLOD
054440970808     C                   MOVEL     E14PIP        VIDPID
054450970808     C                   MOVEL     E14PRV        VIDPRD
054460970808     C                   MOVEL     E14NAR        VIDNZD
054470970808     C                   MOVEL     E14CAP        VIDCAD
054480970808     C* ERRORE
054490970808     C   90              GOTO      ENDCT2
054500060111     c* salvo lna desunta da cappario per non sporcarla se destinatario
054510060111     c* che richiede una particolare lna diversa da quella del cappario
054520060111     c                   move      o95lna        d_o95lna
054530020531     c* Verifico se destinatario disagiato
054540020604     c* Per bolle poste non faccio niente
054550060111     c                   clear                   ddstflo
054560020604     c     lnpntw        ifne      'PPT'
054570040702      * Referente e numero di telefono: se campi a video con '#'
054580040702      * sono quelli automatici (dal tisit5) e quindi li pulisco
054590040702     c                   If        %subst(VidRef:1:1) = '#'
054600040702     c                   Clear                   VidRef
054610040702     c                   EndIf
054620040705     c                   If        %subst(VidTeld:1:1) = '#'
054630040702     c                   Clear                   VidTeld
054640040702     c                   EndIf
054650020531     c                   clear                   tisit0ds
054660020531     c                   movel     'E'           it0tla
054670020531     c                   movel     vidnzd        it0naz
054680020531     c                   movel     vidprd        it0prv
054690020531     c                   movel     vidcad        it0cap
054700020531     c                   movel     vidlod        it0loc
054710020531     c                   movel     vidind        it0ind
054720020531     c                   movel     vidrsd        it0rag
054730020531     c                   call      'TISIT5R'
054740020531     c                   parm                    tisit0ds
054750020531     c     ot0err        ifeq      *blanks
054760060113     c                   movel     ot0flo        ddstflo
054770060111   2ac                   if        ot0dos = 'A' or ot0dos = 'D' or
054780060111     c                             ot0dos = 'S'
054790020531     c                   movel     ot0dos        wdisag
054800020531     c     wdisag        ifeq      'D'
054810020531     c                   movel     'X'           wdisag
054820020531     c                   endif
054830060111     c                   endif
054840020531     c                   else
054850020531     c                   clear                   wdisag
054860020531     c                   endif
054870060327     c***                If        wdisag <> *Blanks
054880060327     c***                          and %subst(arbgva:1:1) <> 'U' or
054890060327     c***                          wdisag='X' and %subst(arbgva:1:1)='U'
054900060327     c                   if        (wdisag = 'X') OR
054910060327     c                             (wdisag='S' and %subst(arbGva:1:1)<>'U') OR
054920060327     c                             (Wdisag='A' and %subst(arbGva:1:1)<>'U' and
054930060327     c                             (arbdcr=*zeros or arbtcr <> *blanks))
054940040702     c                   If        VidTeld = *Blanks
054950040702     c                   Eval      VidTeld = ot0tel
054960040702     c                   EndIf
054970040702     c                   If        VidRef = *Blanks
054980040702     c                   Eval      VidRef =  ot0ref
054990040702     c                   EndIf
055000040702     c                   EndIf
055010020604     c                   endif
055020060113     c*
055030060113     c                   if        §dstlna > *zeros
055040060111     c                   move      §dstlna       d_o95lna
055050060111     c                   endif
055060970808     C*
055070970808     C* MODIFICATI CAP E/O LOCALITA':
055080970808     C* 1) AGGIORNO A VIDEO IL FLAG INOLTRO E AVVISO CON MESSAGGIO
055090970808     C* 2) SE VARIA ANCHE LA LNA E CAP E LOCALITA SONO DIVERSI DALLA
055100970808     C*    BOLLA EMMETTO MESSAGGIO FORZABILE MANTENENDO COMUNQUE LA
055110970808     C*    LINEA DI ARRIVO E ZONA DELLA BOLLA
055120970808     C     WCAD          IFNE      VIDCAD
055130970808     C     WLOD          ORNE      VIDLOD
055140970808     C                   MOVE      VIDCAD        WCAD
055150970808     C                   MOVE      VIDLOD        WLOD
055160970808     C* FLAG INOLTRO
055170970808     C     O95ISO        IFNE      VIDFIN
055180970808     C                   MOVE      O95ISO        VIDFIN
055190140507     c* NON avviso se richiamo cieco
055200140507     c                   if        d48ffr<>'E'
055210970808     C                   SETON                                        289049
055220970808     C                   MOVEL     MSG(77)       VIDMSG
055230970808     C                   GOTO      ENDCT2
055240140507     c                   endif
055250970808     C                   END
055260970808     C                   END
055270970828     C* FLAG INOLTRO
055280970828     C     VIDFIN        IFEQ      ' '
055290970828     C                   MOVE      O95ISO        VIDFIN
055300140507     c* NON avviso se richiamo cieco
055310140507     c                   if        d48ffr<>'E'
055320970828     C                   SETON                                        289049
055330970828     C                   MOVEL     MSG(77)       VIDMSG
055340010412     C                   GOTO      ENDCT2
055350140507     c                   endif
055360970828     C                   END
055370010412     C  N29
055380010412     CANN71VIDFIN        IFEQ      'D'
055390010412     C     O95PID        ANDEQ     'N'
055400010412     C                   SETON                                        289049
055410010412     C                   MOVEL     MSG(98)       VIDMSG
055420010412     C                   GOTO      ENDCT2
055430010412     C                   ENDIF
055440970808     C*
055450140507     c* Non faccio il seguente controllo se richiamo "cieco"
055460141029     c                   if        %lookup('999':skFilOkdir)=0 and
055470141029     c                             (%lookup(%editc(arblna:'X'):skFilOkdir)=0
055480141029     c                             or %lookup(%editc(dutpou:'X'):skFilOkdir)=0)
055490141029     c                   if        d48ffr<>'E'
055500141029     C     WCAD1         IFNE      VIDCAD
055510141029     C     WLOD1         ORNE      VIDLOD
055520141029     C     Wrsd1         ORNE      VIDrsd
055530141029     C     Wprd1         ORNE      VIDprd
055540141029     C     Wnzd1         ORNE      VIDnzd
055550141029     C     Wind1         ORNE      VIDind
055560141029     C                   MOVE      VIDCAD        WCAD1
055570141029     C                   MOVE      VIDLOD        WLOD1
055580141029     C                   MOVE      VIDrsd        Wrsd1
055590141029     C                   MOVE      VIDind        Wind1
055600141029     C                   MOVE      VIDprd        Wprd1
055610141029     C                   MOVE      VIDnzd        Wnzd1
055620141029     C     d_o95lna      ifne      ARBLNA
055630141029     C     SAVCAD        IFNE      VIDCAD
055640060111     C***  O95LNA        ANDNE     ARBLNA
055650141029     C     SAVLOD        ORNE      VIDLOD
055660060111     C***  O95LNA        ANDNE     ARBLNA
055670141029     C     SAVrsd        ORNE      VIDrsd
055680141029     C     SAVind        ORNE      VIDind
055690141029     C     SAVprd        ORNE      VIDprd
055700141029     C     SAVnzd        ORNE      VIDnzd
055710141029     C                   SETON                                        9028
055720141029     C                   MOVEL     MSG(76)       VIDMSG
055730141029     C                   GOTO      ENDCT2
055740141029     C                   END
055750141029     C                   END
055760141029     C                   END
055770141029     c                   endif
055780141029     c                   endif
055790941020     C**
055800950522     C* Se P.IVA sprotetta e nazione estera con flag efta la P.IVA è
055810950522     C* obbligatoria
055820950412     C     *IN05         IFEQ      *OFF
055830950412     C     *IN72         ANDEQ     *OFF
055840950522     C     WDESIE        ANDEQ     'E'
055850950522     C     DESEFT        ANDNE     *BLANKS
055860950412     C     VIDPID        ANDEQ     *BLANKS
055870950412     C                   SETON                                        284590
055880950412     C                   MOVEL     MSG(67)       VIDMSG
055890950412     C                   GOTO      ENDCT2
055900950412     C                   END
055910061116     c
055920061116     c* Se modifica la ragione sociale del destinatario, controllare
055930061116     c*  la partita iva
055940140507     c* Non controllo se chiamata pgm richiamato "cieco"
055950140507     c                   if        d48ffr<>'E'
055960061116     c                   if        vidrsd<>savrsdQ and vidcdf<>*blanks
055970061116     C                   SETON                                        285190
055980061116     C                   MOVEL     MSG(134)      VIDMSG
055990061116     c                   movel     vidrsd        savrsdQ
056000061116     C                   GOTO      ENDCT2
056010061116     C                   END
056020140507     c                   endif
056030061116     c
056040061116     c* La memorizzo anche se cdf=*blanks
056050061116     c                   movel     vidrsd        savrsdQ
056060061115     c
056070061115      * Devo richiamare il nuovo driver fatto per controllare il codice
056080061115      * fiscale
056090061115     c                   if        vidcdf<>*blanks
056100151105     c                   clear                   xcfiva1ds
056110061115     c                   clear                   xcfivamod
056120061115     c                   eval      xcfivapar = vidcdf
056130061115     c                   eval      xcfivanar = vidnzd
056140061115     c                   eval      xcfivaprv = vidprd
056150151105     c                   eval      xcfivacap = vidcad
056160151105     c                   eval      xcfivaloc = vidlod
056170151105     c                   eval      xcfivapiva= vidpid
056180151105     c                   call      'XCFIVAR1'
056190151105     c                   parm                    xcfiva1ds
056200061115     c*
056210061115     c                   if        xcfivaflg < *zeros
056220061115     C                   SETON                                        289051
056230080505     C                   MOVEL     xcfivamsg     VIDMSG
056240061115     C                   GOTO      ENDCT2
056250061115     c                   endif
056260061115     c                   endif
056270120508     c* Parita iva $$ non ammessa per fattura di filiale
056280120508     c                   move      v1cksc        w0040
056290120508     c                   if        %scan('$$':vidpid)>0  and w0040=9999
056300120508     C                   SETON                                        284590
056310120508     C                   MOVEL     MSG(149)      VIDMSG
056320120508     C                   GOTO      ENDCT2
056330120508     c                   endif
056340070508     c
056350140408     c
056360070508     c                   movel     vidffd        wffd
056370141029     c* Il fermo deposito non si può inserire se presente dispo di dirottamento da
056380141029     c* eseguire
056390141029     c                   if        vidffd<>*blanks and arbffd<>wffd
056400141029     c                   clear                   fnlry09ds2
056410141029     c                   eval      ILRY09TCH='T'
056420141029     c                   eval      ILRY09TDIS='D'
056430141029     c                   eval      ILRY09AAS=d48aas
056440141029     c                   eval      ILRY09LNP=d48lnp
056450141029     c                   eval      ILRY09NRS=d48nrs
056460141029     c                   eval      ILRY09NSP=d48nsp
056470141029     c                   eval      kpjbu=fnlry09ds2
056480141031     c                   call      'FNLRY09C'
056490141029     c                   parm                    kpjba
056500141029     c                   eval      fnlry09ds2=kpjbu
056510141029     c                   if        OLRY09ESIT='1'
056520141029     C                   SETON                                        286890
056530141029     C                   MOVEL     MSG(155)      VIDMSG
056540141029     C                   GOTO      ENDCT2
056550141029     c                   endif
056560141029     c                   endif
056570140408     c* Il fermo deposito non si può inserire se presente particolarità consegna
056580140408     c* che prevede che il fermo deposito debba essere autorizzato dal mittente
056590140408     c                   if        vidffd<>*blanks and arbffd<>wffd and
056600140408     c                             arbgma<>*blanks
056610140408     c                   Movel     '7R'          cod
056620140408     c                   Movel(p)  ArbGMa        key
056630140408     c                   ExSr      ctabel
056640140408     c  n30              Movel     TblUni        ds7R
056650140408     c   30              Clear                   ds7R
056660140408     C                   IF        §7RFD='S' AND vidcvr<>'I7'
056670140408     C                   SETON                                        289068
056680140408     C                   MOVEL     MSG(153)      VIDMSG
056690140408     C                   GOTO      ENDCT2
056700140408     C                   ENDIF
056710140408     c                   endif
056720140408     c* Da contattare impostabile solo se immesso il fermo deposito
056730140408     c*  solo se GEO attiva
056740141223     c* NON faccio questo controllo se richiamo cieco
056750141223     c                   if        d48ffr<>'E'
056760140213     c   77              if        (arbffd=wffd or  vidffd='  ') and
056770070508     c                             vidtfd='SI'
056780070508     C                   SETON                                        289052
056790070508     C                   MOVEL     MSG(99)       VIDMSG
056800070508     C                   GOTO      ENDCT2
056810070508     c                   endif
056820141223     c                   endif
056830070508     C
056840070508     c* se immesso fermo deposito e c'e' la consegna richiesta avviso
056850140507     c* NON faccio questo controllo se richiamo cieco
056860140507     c                   if        d48ffr<>'E'
056870140213     c   77              if        vidffd='SI' and arbffd<>wffd and
056880070508     c                             vidtfd='  ' and arbdcr>0  AND WZ3=0
056890070508     C                   SETON                                        289052
056900070508     C                   MOVEL     MSG(143)      VIDMSG
056910070508     C                   EVAL      WZ3=1
056920070508     C                   GOTO      ENDCT2
056930140213     c                   endif
056940140507     c                   endif
056950061115     c
056960941025    1C                   ENDIF
056970941019     C*
056980930505     C***
056990941025    1C     *IN02         IFEQ      '1'
057000930505     C* PESO
057010030203      * Se non è protetto
057020030203   2ac                   If        Not *In88
057030930505     C* NON A 0 O MAGGIORE DEL LIMITE POSTO
057040980406     C     VIDPKF        IFEQ      0
057050941020     C                   SETON                                        479028
057060941020     C                   MOVEL     MSG(2)        VIDMSG
057070911209     C                   GOTO      ENDCT2
057080941025    2C                   END
057090000510     C* DPD NO MAGGIORE DEL LIMITE IMPOSTO
057100020318    2C     LNPNTW        IFEQ      'DPD'
057110020318     C     LNANTW        OREQ      'DPD'
057120000510    3C     VIDPKF        IFGT      §3IPKD
057130000510     C                   SETON                                        479028
057140000510     C                   MOVEA     MSG(52)       K78
057150000510     C                   MOVEL     §3IPKD        W006A             6
057160000510     C                   MOVEA     W006A         SKI(10)
057170000510     C                   Z-ADD     10            I                 3 0
057180000510     C                   EXSR      ABBL
057190000510     C                   MOVEA     SKI(10)       K78(53)
057200000510     C                   MOVE      §3IPKD        W001A
057210000510     C                   MOVEL     W001A         K78(60)
057220000510     C                   MOVEA     K78           VIDMSG
057230000510     C                   GOTO      ENDCT2
057240000510    3C                   END
057250000510    2C                   END
057260000510     C**
057270000510     C**
057280000510     C     VARPKF        IFNE      VIDPKF
057290000510     C                   CLEAR                   WPT1
057300000510     C                   ENDIF
057310000510     C*
057320000510     C     VARPKF        IFNE      VIDPKF
057330000510     C     ARBFVF        ANDEQ     'K'
057340000510     C                   CLEAR                   SET
057350000510     C                   ENDIF
057360000510     C                   MOVEL     VIDPKF        VARPKF
057370000510     C**
057380021121
057390021121     c                   Move      ArbCtr        DueCtr
057400021121
057410021121      * Bolla FedEx con tariffa documenti il peso non può essere superiore a
057420021121      * quanto indicato in tabella "DFE" --> msg forzabile
057430140507     c* Non controllo se progamma richiamato in modalità "cieca"
057440140507     c                   if        d48ffr<>'E'
057450021121If  2c                   If        LnaNtw = 'FED' and DueCtr >= §DfeDalD and
057460021121     c                             DueCtr <= §DfeAlD and VidPkf > §DfePkgD
057470021121     c                             and Wpt1 = *Blanks
057480021121     c                   Eval      *In47 = *On
057490021121     c                   Eval      *In28 = *On
057500021121     c                   Eval      *In90 = *On
057510021121      * Preparo il msg
057520021121     c                   Movea     Msg(116)      K78
057530021121     c                   Movel     §DfePkgD      W006a
057540021121     c                   Movea     W006a         Ski(10)
057550021121     c                   Z-Add     10            i
057560021121     c                   ExSr      Abbl
057570021121     c                   Movea     Ski(10)       K78(50)
057580021121     c                   Move      §DfePkgD      W001a
057590021121     c                   Movel     W001a         K78(57)
057600021121     c                   Movea     K78           VidMsg
057610021121     c                   Eval      Wpt1 = '1'
057620021121     c                   GoTo      EndCt2
057630021121    2c                   EndIf
057640140507     c                   endif
057650051129      * Nel caso di peso totalmente rilevato da VDL posso modificare solo se
057660051129      * inserisco il peso uguale a quello VDL
057670030123     c**!!!              If         ArbNcp = ArbNcl and VidPkc <> VidPkf
057680030123     c                   If         wfpf = 'T' and VidPkc <> VidPkf
057690030122     c                   Seton                                        479028
057700030122     c                   Eval      VidMsg = Msg(121)
057710030122     c                   GoTo      EndCt2
057720030122    2c                   EndIf
057730051129      * Nel caso di peso parzialmente rilevato da VDL posso immettere un peso uguale
057740051129      * o superiore a quello VDL
057750030123     c                   If        wfpf = 'Z' and VidPkf < VidPkc
057760030123     c                   Seton                                        479028
057770030123     c                   Eval      VidMsg = Msg(123)
057780030123     c                   GoTo      EndCt2
057790030123    2c                   EndIf
057800030203   2ac                   EndIf
057810021121
057820930505     C* VOLUME
057830091124      * Se non è protetto
057840091124   2ac                   If        Not *In89
057850091124     c
057860980406     C     VIDVLF        IFEQ      0
057870941215     C                   SETON                                        489028
057880941020     C                   MOVEL     MSG(3)        VIDMSG
057890911209     C                   GOTO      ENDCT2
057900941025    2C                   ENDIF
057910941214     C*
057920941027     C* SE IL TIPO VOLUME E' "Z" NON SI PUO' ABBASSARE
057930941027     C     VIDFVF        IFEQ      'Z'
057940941027     C     VIDVLF        ANDLT     ARBVLF
057950941215     C                   SETON                                        489028
057960941027     C                   MOVEL     MSG(5)        VIDMSG
057970941027     C                   GOTO      ENDCT2
057980941027    2C                   ENDIF
057990091124     c*
058000091124     c* Per volume totaole variabile solo come il totale
058010091124     c                   If         ArbNcr = ArbNcl and Vidvlf <> arbvlc
058020091124     c                   Seton                                        489028
058030091124     c                   Eval      VidMsg = Msg(147)
058040091124     c                   GoTo      EndCt2
058050091124    2c                   EndIf
058060091124     c
058070091124      * Nel caso di vol  parzialmente rilevato da VDL posso immettere un vol  uguale
058080091124      * o superiore a quello VDL
058090091124     c                   If         ArbNcr < ArbNcl and Vidvlf < arbvlc
058100091124     c                              and arbncr>0
058110091124     c                   Seton                                        489028
058120091124     c                   Eval      VidMsg = Msg(148)
058130091124     c                   GoTo      EndCt2
058140091124    2c                   EndIf
058150091124   2ac                   EndIf
058160941214     C*
058170051129     C* SE IL TIPO VOLUME NON E' "Z" MA ESISTE UN VOLUME VDL CHE DIVENT
058180941214     C*  SUPERIORE A QUELLO DA FATTURARE DOPO LA VARIAZIONE, AVVISO E
058190941215     C*  MODIFICO
058200941215     C     VARVLF        IFNE      VIDVLF
058210941215     C                   CLEAR                   SET
058220941215     C                   MOVEL     VIDVLF        VARVLF
058230941215     C                   ENDIF
058240941215     C*
058250051129     C*  POI IMPOSTERO NEL VOLUME DA FATTURARE IL VOLUME VDL
058260941214    2C     SET           IFEQ      0
058270941214     C     ARBVLC        ANDGT     0
058280941214     C     VIDFVF        ANDNE     'Z'
058290941214     C     VIDFVF        ANDNE     'T'
058300941214     C*
058310051129    3C     WVDL          IFEQ      'S'
058320970418    4C     WFVF          IFEQ      'Z'
058330941222     C   06              MOVEL     ZAFA          WSOST             1
058340941222     C  N06              MOVEL     ZAAA          WSOST
058350970418   X4C                   ELSE
058360970418     C   06              MOVEL     TAFA          WSOST             1
058370970418     C  N06              MOVEL     TAAA          WSOST
058380970418    4C                   END
058390970418   X3C                   ELSE
058400970418    4C     WFVF          IFEQ      'Z'
058410941222     C   06              MOVEL     ZAFN          WSOST             1
058420941222     C  N06              MOVEL     ZAAN          WSOST
058430970418     C                   ELSE
058440970418     C   06              MOVEL     TAFN          WSOST             1
058450970418     C  N06              MOVEL     TAAN          WSOST
058460970418    4C                   ENDIF
058470970418    3C                   ENDIF
058480941214     C*
058490941214     C* SE VOLUME SAREBBE DA SOSTITUIRE VEDO SE DIVENTA SUPERIORE
058500941214     C* DEL VOLUME DA FATTURARE
058510941214    3C     WSOST         IFEQ      'S'
058520970418     C                   CLEAR                   WSOSC
058530941214     C*
058540941214    4C     VIDVLF        IFLT      ARBVLF
058550941214     C     VIDVLF        ANDLT     ARBVLC
058560970418     C* PRENDO FLAG DI SOSTITUIBILITA' VOLUME REALE
058570970418     C                   MOVEL     '7T'          COD
058580970418     C                   MOVEL(P)  'R'           KEY
058590970418     C                   EXSR      CTABEL
058600970418     C   30              CLEAR                   DS7T
058610970418     C  N30              MOVEL     TBLUNI        DS7T
058620970418     C     WFVF          IFEQ      'Z'
058630970418     C                   MOVEL     §7TSOZ        WSOSC
058640970418     C                   ELSE
058650970418     C                   MOVEL     §7TSOT        WSOSC
058660970418     C                   END
058670970418     C* (SOLO SE IL VOLUME E' SOSTITUIBILE)
058680970418    5C     WSOSC         IFEQ      'S'
058690140507     C                   ADD       1             SET
058700140507     c* non segnalo se pgm richiamato in modalità "cieca"
058710140507     c                   if        d48ffr<>'E'
058720941214     C                   SETON                                        482890
058730941214     C                   MOVEL     MSG(58)       VIDMSG
058740941214     C                   GOTO      ENDCT2
058750140507     c                   endif
058760970418    5C                   ENDIF
058770941214    4C                   ENDIF
058780941214     C*
058790941214     C* SE VOLUME AUTOMATICO E MODIFICATO IL PESO CONTROLLO
058800970418   4AC     *IN88         IFEQ      *OFF
058810941214     C                   Z-ADD     VIDVLF        D20VLU
058820941214     C                   EXSR      VOLAUT
058830970418    4C     D20VLU        IFLT      ARBVLC
058840970418     C                   MOVEL     '7T'          COD
058850970418     C                   MOVEL(P)  ARBFVF        KEY
058860970418     C                   EXSR      CTABEL
058870970418     C   30              CLEAR                   DS7T
058880970418     C  N30              MOVEL     TBLUNI        DS7T
058890051129     C* FLAG DI SOSTITUIBILITA' DA VOL.VDL
058900970418     C     WFVF          IFEQ      'Z'
058910970418     C                   MOVEL     §7TSOZ        WSOSC
058920970418     C                   ELSE
058930970418     C                   MOVEL     §7TSOT        WSOSC
058940970418     C                   END
058950970418    5C     WSOSC         IFEQ      'S'
058960140507     C                   ADD       1             SET
058970140507     c* non segnalo se pgm richiamato in modalità "cieca"
058980140507     c                   if        d48ffr<>'E'
058990941215     C                   SETON                                        472890
059000941214     C                   MOVEL     MSG(58)       VIDMSG
059010941214     C                   GOTO      ENDCT2
059020140507     c                   endif
059030970418    5C                   ENDIF
059040941214    4C                   ENDIF
059050970418   4AC                   ENDIF
059060941214    3C                   ENDIF
059070941214    2C                   ENDIF
059080170130     c* controllo peso e volume con i limiti previsti in tabella "TSP"
059090170130     c* (limiti per tipo servizio)
059100170131     C                   if        not *in88  or not *in89
059110170130     c                   exsr      sr_limtsp
059120170130     c                   if        *in90
059130170130     c                   seton                                        28
059140170130     C                   MOVEL     wmsgtsp       VIDMSG
059150170130     C                   GOTO      ENDCt2
059160170130     c                   endif
059170170130     c                   endif
059180941214     C*
059190941027    1C                   ENDIF
059200910429     C*
059210910429     C     ENDCT2        ENDSR
059220170130     C**************************************************************************
059230170130     C* Controllo limiti (peso e volume) per tipo servizio
059240170130     C**************************************************************************
059250170130     C     sr_limtsp     BEGSR
059260170130     c                   clear                   wmsgtsp
059270170130     c                   clear                   trul73ds
059280170130     c                   eval      i73tsp=arbtsp
059290170130     c                   eval      i73psk=vidpkf
059300170130     c                   eval      i73vmc=vidvlf
059310170130     c                   eval      i73ncl=arbncl
059320170130     c                   call      'TRUL73R'
059330170130     c                   parm                    trul73ds
059340170130     c                   select
059350170130     c                   when      o73err='E'
059360170130     c                   seton                                        90
059370170130     c                   eval      wmsgtsp=msg(163)
059380170130     c                   eval      %subst(wmsgtsp: 40: 8)=v1dtsp
059390170130     c* peso kg bloccante
059400170130     c                   when      O73FKGM='1'  and not *in88
059410170131     c                                          and wfpf <> 'T' and wfpf<>'Z'
059420170130     c                   seton                                        9047
059430170130     c                   eval      wmsgtsp=msg(164)
059440170131     c                   eval      %subst(wmsgtsp: 19: 3)=wtsp03
059450170130     c                   eval      %subst(wmsgtsp: 34: 8)=%editc(o73lkgm:'4')
059460170130     c*volume mc bloccante
059470170131     c                   when      O73FMCM='1'  and not *in89
059480170131     c                                          and arbncr=0
059490170130     c                   seton                                        9048
059500170130     c                   eval      wmsgtsp=msg(165)
059510170131     c                   eval      %subst(wmsgtsp: 19: 3)=wtsp03
059520170130     c                   eval      %subst(wmsgtsp: 36: 6)=%editc(o73lmcm:'4')
059530170130     c* peso kg forzabile
059540170130     c                   when      O73Fkgf='1' and vidpkf<>wpes_pre
059550170130     c                             and not *in88 and d48ffr<>'E'
059560170131     c                             and wfpf <> 'T' and wfpf<>'Z'
059570170130     c                   seton                                        9047
059580170130     c                   eval      wmsgtsp=msg(164)
059590170131     c                   eval      %subst(wmsgtsp: 19: 3)=wtsp03
059600170130     c                   eval      %subst(wmsgtsp: 34: 8)=%editc(o73lkgf:'4')
059610170130     c                   eval      wmsgtsp=%trim(wmsgtsp)+ ' Avvisare ROM.'
059620170130     c                   eval      wmsgtsp=%trim(wmsgtsp)+ ' ENTER per forzare'
059630170130     c                   eval      wpes_pre=vidpkf
059640170130     c* volume mc forzabile
059650170130     c                   when      O73FmcF='1' and vidvlf<>wvol_pre
059660170130     c                             and not *in89 and d48ffr<>'E'
059670170131     c                             and arbncr=0
059680170130     c                   seton                                        9048
059690170130     c                   eval      wmsgtsp=msg(165)
059700170131     c                   eval      %subst(wmsgtsp: 19: 3)=wtsp03
059710170130     c                   eval      %subst(wmsgtsp: 36: 6)=%editc(o73lmcf:'4')
059720170130     c                   eval      wmsgtsp=%trim(wmsgtsp)+ ' Avvisare ROM.'
059730170130     c                   eval      wmsgtsp=%trim(wmsgtsp)+ ' ENTER per forzare'
059740170130     c                   eval      wvol_pre=vidvlf
059750170130     c                   endsl
059760170130     c                   endsr
059770911212     C*
059780911209     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE IX -----------------*
059790911209     C     REGIS2        BEGSR
059800130412     c                   clear                   wpda_ndc
059810130412     c                   clear                   wpda_ddc
059820130412     c                   clear                   wpda_dcm
059830160318     c* flag per togliere da distinta spedizioni import dpd "VEDI PACCO"
059840160318     c                   clear                   wtogli
059850950104     C*  GIRO UDATE
059860950104     C                   TIME                    W0140            14 0
059870950104     C* UDATE IN GGMMAAAA
059880950104     C                   MOVE      W0140         WDTGIO            8 0
059890950104     C*
059900950104     C* UDATE IN AAAAMMGG
059910950104     C                   Z-ADD     WDTGIO        G02DAT
059920950104     C                   MOVEL     *BLANK        G02ERR
059930950104     C                   CALL      'XSRDA8'
059940950104     C                   PARM                    WLBDAT
059950950104     C                   MOVEL     G02INV        DATEU             8 0
059960941213     C                   MOVEL     KNMUS         ARBPRU
059970941221     C* PRENDO I VALORI ASSOLUTI
059980941221     C                   MLLZO     1             VIDPKF
059990941221     C                   MLLZO     1             VIDVLF
060000041007     C* SALVO VOL/PKG/FTC/RSD/CAP/FIN VECCHIO IN FNARB
060010950114     C                   MOVEL     ARBVLF        OLDVLF
060020950114     C                   MOVEL     ARBFVF        OLDFVF
060030950114     C                   MOVEL     ARBPKF        OLDPKF
060040041007     C                   MOVEL     ARBvlc        OLDvlc
060050950114     C                   MOVEL     ARBRSD        OLDRSD
060060150206     C                   MOVEL     ARBLOD        OLDLOD
060070950114     C                   MOVEL     ARBCAD        OLDCAD
060080950114     C                   MOVEL     ARBFIN        OLDFIN
060090041007     C                   MOVEL     ARBFFD        OLDFFD
060100070320     c                   movel     VIDFFD        WFFD
060110151230     c* (Se variazione fatta da Arrivo su bolla ancora da partire non devo inviare)
060120151222     c                   if        not *in10 or wfile='A'
060130920120     C* INVIO PER SEDE
060140170116     C***                MOVEL     'FNARBD0T'    SFILE
060150920120     C* ALLOCO
060160170116     C****               EXSR      ALLOC
060170941027     C* ERRORI IN ALLOCAZIONE
060180170116    1C***  *IN91         IFEQ      *ON
060190170116     C***                GOTO      ENDRG2
060200170116    1C***                ENDIF
060210070515    c
060220070515     c* Se modificato FERMO DEPOSITO, devo allocare record L di ar4
060230070515     c*  per blocco/sblocco telefonata
060240070515     c                   clear                   wesar4L           1
060250070515    1c                   if        Wffd<>oldFFD
060260070515     c                   eval      TipoEla='C'
060270070515     c                   exsr      GestAR4L
060280070515     c
060290070515    2c                   if        *in91
060300170116     c*****              exsr      DLCSED
060310070525     C                   SETON                                        2891
060320070515     C                   GOTO      ENDRG2
060330070515    2c                   endif
060340070515    1c                   endif
060350070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
060360070515     c* iornare direttamente il file
060370070515     c                   clear                   waltrabolla       1
060380151222     C   10KARB          CHAIN     FNBLP01L                           3091
060390151222     C  n10KARB          CHAIN     FNARB01L                           3091
060400070515     c*
060410070515     c                   if        *in91
060420070515     C                   MOVEL     MSG(4)        VIDMSG
060430070515     c*
060440170116     c****               exsr      DLCSED
060450070515     c                   unlock    fiar401l
060460070515     c
060470070525     C                   SETON                                        2891
060480070515     C                   GOTO      ENDRG2
060490070515     c                   else
060500070515     c                   if        *in30
060510070515     c                   eval      waltrabolla='N'
060520070515     c                   else
060530070515     c* Salvo alcuni campi dell'altro file che potrebbero, per tempi
060540070515     c*  di aggiornamento, essere diversi.
060550070515     c                   eval      waltrofvf=arbfvf
060560070515     c                   eval      waltrovlf=arbvlf
060570070515     c                   eval      waltrocpi=arbcpi
060580130411     c* salvo campi di arb che mi servono per invio dati a pda
060590151222     c  n10              eval      wpda_ndc=arbndc
060600151222     c  n10              eval      wpda_ddc=arbddc
060610151222     c  n10              eval      wpda_dcm=arbdcm
060620151222     C   10KARB          CHAIN     FNARB01L                           30
060630151222     C  n10KARB          CHAIN     FNBLP01L                           30
060640070515     c* Alla fine per non sporcarmi i campi del record, richaino il
060650070515     c*  record originale che sto variando
060660151222     c   10              eval      wpda_ndc=arbndc
060670151222     c   10              eval      wpda_ddc=arbddc
060680151222     c   10              eval      wpda_dcm=arbdcm
060690070515     c                   endif
060700070515     c                   endif
060710151222     c                   endif
060720070207     C*
060730070207     C* SE SI TRATTA DI BOLLA FRANCO, SALVO IL CAMPO DELLA PARTITA IVA
060740151222     C*   10
060750151222     C*AN 05              MOVEL     ARBCPI        SAVCPI           16
060760151222     C*   10
060770151222     C*AN 05              MOVEL     *BLANKS       ARBCPI
060780070207     C*
060790151222     C*  N10
060800151222     C*AN 06              MOVEL     ARBCPI        SAVCPI           16
060810151222     C*  N10
060820151222     C*AN 06              MOVEL     *BLANKS       ARBCPI
060830151222     c                   if        (wfile='A' and *in05) or
060840151222     c                             (wfile='P' and *in06)
060850151222     C                   MOVEL     ARBCPI        SAVCPI           16
060860151222     C                   MOVEL     *BLANKS       ARBCPI
060870151222     c                   endif
060880070207     C*
060890061116     c* Se variazione di destinatario chaino anche fiar4 record Q se
060900061116     c*  mi serve
060910141215     c                   if        wnoindir=' '
060920061116     c                   if        *in01 and not *in05
060930061116     c                   if        (vidcdf=*blanks  and war4Q='D') or
060940061116     c                             (vidcdf<>*blanks and war4Q<>' ')
060950061116     C                   MOVEL     'Q'           WTRC
060960061116     C     KARB2         CHAIN     FiAR4000                           3091
060970061116     c*
060980061116     c                   if        *in91
060990061116     C                   MOVEL     MSG(4)        VIDMSG
061000070515     c
061010151223     c                   if        not *in10 or wfile='A'
061020170116     c******             exsr      DLCSED
061030151223     c   10              unlock    fnblp01l
061040151223     c  N10              unlock    fnarb01l
061050151223     c                   endif
061060070525     C                   SETON                                        2891
061070061116     C                   GOTO      ENDRG2
061080061116     c                   endif
061090061116     c                   endif
061100061116     c                   endif
061110141215     c                   endif
061120061116     c
061130061116     c* Verifico se prima c'era
061140911212     C*
061150911209     C                   Z-ADD     DATEU         ARBDTV
061160911209     C                   TIME                    ARBORV
061170911209     C                   MOVEL     VIDCVR        ARBCVB
061180911209     C*
061190060130     C* SE DA STORICIZZARE
061200060130     C**   *IN10         IFEQ      *ON
061210060130    1C**   D66FSA        ANDEQ     'S'
061220060130     C**   *IN10         OREQ      *OFF
061230060130    1C**   D66FSP        ANDEQ     'S'
061240060130    1C     D66FSA        ifeq      'S'
061250941212     C                   MOVEL     SAVRD2        ARBRD2
061260950114     C* SE VENGO DA VARIAZIONE MITTENTE, IL VOLUME LO STORICIZZO CON I
061270950114     C* CAMPI CHE HO SALVATO
061280950114     C     *IN24         IFEQ      *ON
061290950114     C                   MOVEL     MITFVF        ARBFVF
061300950114     C                   MOVEL     MITVLF        ARBVLF
061310950114     C                   MOVEL     MITFVF        ARBFVB
061320950114     C                   MOVEL     MITVLF        ARBVLB
061330950114    1C                   ENDIF
061340011108      *
061350011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
061360011108      * è in partenza 'P' o in arrivo 'A'
061370011108     C     *IN10         IFEQ      *ON
061380160119     c                   if        wfile='P'
061390160119     C                   MOVE      'a'           ARBPRU
061400160119     c                   else
061410011108     C                   MOVE      'A'           ARBPRU
061420160119     c                   endif
061430011108     C                   ENDIF
061440011108     C     *IN10         IFEQ      *OFF
061450011108     C                   MOVE      'P'           ARBPRU
061460011108     C                   ENDIF
061470011108      *
061480941014     C                   WRITE     FNARBD00
061490130927     c
061500130927     c* Memorizzo anche referente e telefono se si tratta di varazione "I0"
061510131009     c                   if        *in01 and
061520131009     c                                  (vidref<>par5ref or vidteld<>par5teld)
061530131204     c                   clear                   darbn_ref
061540130927     c                   eval      §arbnref = par5ref
061550130927     c                   eval      §arbnteld= par5teld
061560131204     c                   eval      arbuni70 = darbn_ref
061570130930     c                   eval      arbtrd='REF'
061580130927     c                   write     FNARBN00
061590130927     c                   endif
061600950114     C*
061610950114     C     *IN24         IFEQ      *ON
061620950114     C                   MOVEL     OLDFVF        ARBFVF
061630950114     C                   MOVEL     OLDVLF        ARBVLF
061640950114     C                   MOVEL     OLDFVF        ARBFVB
061650950114     C                   MOVEL     OLDVLF        ARBVLB
061660950114    1C                   ENDIF
061670950114    1C                   ENDIF
061680060301     c* storicizzazione motivazione
061690060301     c     d66mtv        ifeq      'S'
061700060301     c                   exsr      sto_mtv
061710060301     c                   endif
061720911209     C*
061730941025    1C     *IN01         IFEQ      *ON
061740040224
061750040224      * se in arrivo ho modificato uno dei dati del Fiar5 rcd GEN:
061760040224      * - Fermo deposito
061770040224      * devo memorizzare i dati della bolla prima della modifica
061780070320if  3c   10              If        wffd <> oldFfd
061790041022     c                   ExSr      RegAr5AR
061800040224    3c                   EndIf
061810941025     C*
061820941025     C* SE IMMESSA ANCHE LA 2 RAGIONE SOCIALE
061830141212     c                   if        wnoindir=' '
061840060223     c                   clear                   d19ftr
061850160115     c                   if        *in10 and wfile='P'
061860160115     c                   eval      d19ftr='T'
061870160115     c                   endif
061880941025     C                   MOVEL     ARBAAS        D19AAS
061890941025     C                   MOVEL     ARBLNP        D19LNP
061900941025     C                   MOVEL     ARBNRS        D19NRS
061910941025     C                   MOVEL     ARBNSP        D19NSP
061920941025     C                   MOVEL     'D'           D19TRC
061930970520     C                   MOVE      VIDRSD        D19NT1
061940941025     C                   CLEAR                   D19NT2
061950051114     c
061960941025     C                   CALL      'FNLV19R'
061970030612     C                   PARM                    Fnlv19DS
061980941025     C*
061990941025     C* ERRORE: RECORD NON VINCOLABILE
062000941212    2C     D19ERR        IFEQ      '2'
062010941025     C                   MOVEL     MSG(4)        VIDMSG
062020151223     c                   if        not *in10 or wfile='A'
062030170116     c****               exsr      dlcsed
062040070515     c   10              unlock    fnblp01l
062050070515     c  N10              unlock    fnarb01l
062060070515     c                   unlock    fiar401l
062070151230     c                   endif
062080151223     C                   SETON                                        2891
062090941025     C                   GOTO      ENDRG2
062100941212    2C                   ENDIF
062110141212     c                   endif
062120041006    c
062130950626     C*
062140950626     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
062150950626     C*     NELLA P.IVA
062160950626     C     VIDISD        IFEQ      '$$'
062170950626     C     VIDCPD        ANDEQ     *BLANKS
062180950626     C                   MOVEL     'PRIVATO'     VIDCPD
062190950626     C                   ENDIF
062200941025     C*
062210141212     c                   if        wnoindir=' '
062220941027     C                   MOVEL     VIDRSD        ARBRSD
062230970520     C                   MOVE      D19NT1        ARBRD2
062240911209     C                   MOVEL     VIDIND        ARBIND
062250911209     C                   MOVEL     VIDLOD        ARBLOD
062260911209     C                   MOVEL     VIDCAD        ARBCAD
062270060705     c* se spedizione export DPD diretta in Portogallo devo aggiungere al
062280060705     c* cap immesso tanti 0 a dx quanti ne servono per arrivare ad una
062290060705     c* lunghezza di cap di 7 byte
062300060705     c                   if        lnantw='DPD' and vidnzd = 'P  '
062310060705     c     ' '           checkr(e) arbcad:7      NN                1 0
062320060705     c                   if        %found
062330060705     c                   eval      %subst(arbcad:nn+1:7-nn)=*all'0'
062340060705     c                   endif
062350060705     c                   endif
062360060705     c*
062370911209     C                   MOVEL     VIDPRD        ARBPRD
062380941220     C                   MOVEL     VIDPID        ARBCPI
062390941220     C* LA P.IVA SE HA CODICE ISO VIENE MESSO A SIN ALTRIMENTI SUBITO
062400941220     C*  IL CODICE DELLA P.IVA
062410941220     C     VIDCPD        IFNE      *BLANKS
062420941220     C     VIDISD        ANDEQ     *BLANKS
062430941220     C                   CLEAR                   ARBCPI
062440941220     C                   MOVEL     VIDCPD        ARBCPI
062450941220     C                   ENDIF
062460041027     c                   eval      newcpi=arbcpi
062470041027     c
062480060222     C* SE ESISTE RECORD P.IVA IN FIAR4 LO AGGIORNO
062490950413     C* SOLO SE P.IVA SPROTETTA
062500950413     C     *IN05         IFEQ      *OFF
062510950413     C     *IN72         ANDEQ     *OFF
062520950413     C                   MOVEL     'P'           WTRC
062530060222     C     KARB2         CHAIN     FIAR401L                           30
062540060222     C     *IN30         IFEQ      *OFF
062550060223     c                   move      ar4not        wcpifile
062560060223     c                   if        wcpifile <> arbcpi
062570060223     c                   movel     'T'           ar4ftr
062580060223     c                   z-add     dateu         ar4dtr
062590060223     c                   z-add     dateu         ar4duv
062600060222     C                   MOVE      ARBCPI        AR4NOT
062610060222     C                   UPDATE    FIAR4000
062620060223     c                   else
062630060223     c                   unlock    fiar401l
062640060223     c                   endif
062650060222     C                   END
062660950413     C                   END
062670141215     c                   endif
062680941212     C*
062690931207    2C     VIDFFD        IFEQ      'SI'
062700950222     C                   MOVEL     'C'           ARBFIN
062710930528     C                   MOVEL     'S'           ARBFFD
062720931207   X2C                   ELSE
062730930528     C                   MOVEL     ' '           ARBFFD
062740141212     c                   if        wnoindir=' '
062750930528     C                   MOVEL     VIDFIN        ARBFIN
062760141212     c                   endif
062770931207    2C                   END
062780160318     c* Se spedizione Import dpd richiamo routine per aggiornamenti per
062790160318     c* spedizione "VEDI PACCO"
062800160318     c* (Aggiornamento del flag traduttore su fiar4 rek "I", pulizia del
062810160318     c* codice giro + memorizzaizone di togliere sped da distinta)
062820160318     c                   if        lnpntw='DPD'
062830160318     c                   exsr      sr_VediPDpd
062840160318     c                   endif
062850110905     c* se attiva procedura centro storico e c'e' il giro sulla bolla --> ricalcolo se
062860110905     c*  non è stato forzato a video
062870151230     c                   if        wnoindir=' '  and
062880151230     c                             (not *in10 or wfile='A')
062890110906     c                   if        vidffd=*blanks
062900110906     c                   if        (oldfin<>arbfin and arbfin=o95iso)
062910110906     c                             or oldffd<>vidffd
062920110906     c*****                        or oldfin=arbfin
062930110906     c
062940160318     c     karb          chain(n)  fiarg01l
062950160318     c                   if        %found(fiarg01l) and argcgi<>*blanks
062960160318     c****                         and olvstprop<>'N'
062970110905     c                   clear                   fnlvstds
062980110905     c                   eval      ilvstaas=arbaas
062990110905     c                   eval      ilvstlnp=arblnp
063000110905     c                   eval      ilvstnrs=arbnrs
063010110905     c                   eval      ilvstnsp=arbnsp
063020110905     c                   eval      ilvstmgs=arbmgs
063030110905     c                   eval      ilvstfin=arbfin
063040110905     c                   eval      ilvstcgi=argcgi
063050110906     c                   if        lnacons>0
063060110906     c                   eval      ilvstpoc=lnacons
063070110906     c                   else
063080110906     c                   eval      ilvstpoc=arblna
063090110906     c                   endif
063100110906     c                   if        arbddc>0
063110110906     c                   eval      ilvstdat=arbddc
063120110906     c                   else
063130110906     c                   eval      ilvstdat=dateu
063140110906     c                   endif
063150110905     c                   call      'FNLVSTR'
063160110905     c                   parm                    kpjba
063170110905     c                   parm                    fnlvstds
063180110906     c
063190110906     c* se aggiornato flag inoltro lo memorizzo nella variazione
063200110906     c                   if        OLVSTFINAG='S'
063210110906     c                   eval      arbfin=olvstfin
063220110905     c                   endif
063230110906     c
063240110906     c                   endif
063250110906     c                   endif
063260110906     c                   endif
063270141212     c                   endif
063280110905
063290970808     C* CODICE TASSAZIONE
063300141212     c                   if        wnoindir=' '
063310970808     C                   MOVEL     O95CTS        ARBCTS
063320141212     c                   endif
063330130606
063340031120
063350130930     c                   exsr      AggioREF
063360130930     c
063370931207    1C                   END
063380941025     C**
063390920319     C* RICALCOLO DEL VOLUME
063400941025     C**
063410941025    1C     *IN02         IFEQ      *ON
063420950114     C*
063430950114     C* SE VARIATO MITTENTE PER I PADRONCINI USO QUELLI SALVATI
063440941102     C* PULIZIA DELLA DS PER IMPOSTARE IL VOLUME DA FATTURARE VARIATO
063450030612     C                   CLEAR                   Fnlv20DS
063460941102     C*
063470941025     C* VOLUME REALE
063480041007    2C     VIDVLF        IFNE      oldVLF
063490941102     C                   MOVEL     'R'           D20FVL
063500941102     C                   Z-ADD     VIDVLF        D20VLU
063510930505     C*
063520931207   X2C                   ELSE
063530941025     C*
063540941025     C* VEDO SE DEVO MODIFICARE IL VOLUME DA FATTURARE
063550941214     C                   EXSR      VOLAUT
063560941102    2C                   ENDIF
063570941102     C**
063580941102     C* SE HO MODIFICATO IL VOLUME DA FATTURARE CALL A FNLV20R
063590941102     C*  PER DETERMINARE IL VOLUME BOLLETTATO
063600941102     C**
063610941102    2C     D20VLU        IFGT      0
063620941222     C                   MOVEL     D48TBO        D20TBO
063630941102     C                   MOVEL     'F'           D20TVL
063640041008     C                   MOVEL     arbFVB        D20FVB
063650041008     C                   MOVEL     arbVLB        D20VLB
063660041007     C                   MOVEL     oldFVF        D20FVF
063670041007     C                   MOVEL     oldVLF        D20VLF
063680030612     C                   MOVEL     Fnlv20DS      KPJBU
063690941102     C                   CALL      'FNLV20R'
063700941102     C                   PARM                    KPJBA
063710941102     C*
063720030612     C                   MOVEL     KPJBU         Fnlv20DS
063730941102     C*
063740941102     C                   MOVEL     D20FVB        ARBFVB
063750941102     C                   MOVEL     D20VLB        ARBVLB
063760941102     C                   MOVEL     D20FVF        ARBFVF
063770941102     C                   MOVEL     D20VLF        ARBVLF
063780941102    2C                   ENDIF
063790941214     C*
063800051129     C* SE DEVO SOSTITUIRE COL VOLUME VDL LO FACCIO
063810941214     C     WSOST         IFEQ      'S'
063820970418     C* WSOSC INDICA SE IL TIPO VOLUME DETERMINATO PER LA BOLLA E'
063830051129     C* SOSTITUIBILE DAL VOLUME VDL.
063840970418     C* IN CASO DI VARIAZIONE VOLUME WSOSC SARA' QUELLO DEL TIPO VOLUME
063850970418     C* "R"; IN CASO DI VARIAZIONE DI PESO WSOSC SARA' QUELLO DEL TIPO
063860970418     C* VOLUME DELLA BOLLA
063870041007     C     oldVLC        ANDGT     ARBVLF
063880970418     C     WSOSC         ANDEQ     'S'
063890041007     C                   Z-ADD     oldVLC        ARBVLF
063900970418     C                   MOVE      WFVF          ARBFVF
063910941214     C                   ENDIF
063920920319     C*
063930990628     C  N10              MOVEL     VIDPKF        ARBPKB
063940941102     C                   MOVEL     VIDPKF        ARBPKF
063950041007     c
063960041007    2C     *IN24         IFEQ      *ON
063970041007     C                   MOVEL     MITFVF        OLDFVF
063980041007     C                   MOVEL     MITVLF        OLDVLF
063990041007    2C                   ENDIF
064000941102    1C                   ENDIF
064010941212     C*
064020041007     C* Ora invio solo a sede
064030151223     c                   if        not *in10 or wfile='A'
064040070503     C     D66FFI        ifeq      'S'
064050950227     C*
064060950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
064070950413    2C     §3ABSD        IFEQ      ' '
064080170116     C***                OPEN      FNARBD2T
064090941014     C                   WRITE     FNARBDTT
064100170116     C***                CLOSE     FNARBD2T
064110950227    2C                   END
064120911212     C* DISALLOCO OGGETTO
064130170116     C****               EXSR      DLCSED
064140950208    1C                   END
064150911209     C*
064160950113     C* AGGIORNO FILE TASSAZIONE PADRONCINI
064170950113     C                   EXSR      AGGPDS
064180151223     c                   endif
064190931207     C*
064200941102     C* AGGIORNO
064210151223     C**   10
064220151223     C**AN 05              MOVEL     SAVCPI        ARBCPI
064230151223     C**  N10
064240151223     C**AN 06              MOVEL     SAVCPI        ARBCPI
064250151223     c                   if        (wfile='A' and *in05) or
064260151223     c                             (wfile='P' and *in06)
064270151223     C                   MOVEL     SAVCPI        ARBCPI
064280151223     c                   endif
064290941212     C*
064300151223     c                   if        wfile='A'
064310151223     C                   UPDATE    FNARB000
064320151223     c                   else
064330151223     C                   UPDATE    FNBLP000
064340151223     c                   endif
064350150206     c
064360160318     c* Tolgo  spedizione "VEDI PACCO DPD" da distinta
064370160318     c                   if        wtogli='1'
064380160318     c                   exsr      sr_TogliVDpd
064390160318     c                   endif
064400150206     c* memorizzo la località normalizzata
064410150206     c                   if        vidlod<>normlod OR WAR4X='E'
064420150206     C
064430150211     c                   eval      d19ftr='T'
064440150206     c                   eval      d19agg='E'
064450150203     C                   MOVEL     ARBAAS        D19AAS
064460150203     C                   MOVEL     ARBLNP        D19LNP
064470150203     C                   MOVEL     ARBNRS        D19NRS
064480150203     C                   MOVEL     ARBNSP        D19NSP
064490150203     C                   MOVEL     'X'           D19TRC
064500150206     C                   IF        VIDLOD<>NORMLOD
064510150203     C                   MOVE      normlod       D19NT1
064520150206     C                   ELSE
064530150206     C                   CLEAR                   D19NT1
064540150206     C                   ENDIF
064550150206     C                   CLEAR                   D19NT2
064560150203     c
064570150203     C                   CALL      'FNLV19R'
064580150203     C                   PARM                    Fnlv19DS
064590150206     c                   endif
064600150203     c
064610151230     c* Salto le seguenti istruzioni se
064620151223     c* dall'arrivo sono in manutenzione di una bolla in partenza ancora da
064630151223     c* partire
064640151223     c                   if        not *in10 or wfile='A'
064650070320     c
064660070320     c* Se aggiunto   Fermo Deposito, senza consegna richiesta,
064670070320     c*  imposto il blocco. conconsegna richiesta lo tolgo
064680070320     c* Se tolgo      Fermo Deposito, tolgo il relativo blocco
064690070320     c
064700070320     c* Devo richainare e aggiornare (perchè nel frattempo ho letto
064710070320     c*  altri record fi FIAR4
064720151223
064730070320    1c                   if        Wffd<> oldffd
064740070320     c                   eval      tipoEla='C'
064750070320     c                   exsr      GestAR4L
064760070320     c
064770070320     c* Se allocato pazienza (non aggiorno ma vado avanti
064780070320     c*  fermarmi in questo punto non è bello,lascerebbe la
064790070320     c*  transazione a metà)
064800070320    2c                   if        not *in91
064810070320     c
064820170113     c* Se aggiunto il fermo deposito tolgo eventuale contatto per l'appuntamento
064830170113     c                   if        wffd='S' and
064840170113     c                             (arbtc1='A' or arbtc2='A')
064850170113     c                   eval      §b4lbas=' '
064860170113     c                   endif
064870170113
064880070508    3c****               if        arbdcr=0 and wffd='S'
064890070508     c* imposto il flag telefonata in base a quanto indicato a video
064900170113     c                   if        vidtfd='SI'  or wffd=' '
064910070508     c                   clear                   §b4ltfd
064920070508     c                   else
064930070320     c                   eval      §b4ltfd='S'
064940070508     c                   endif
064950070508     c
064960070508     c****               else
064970070508     c****               clear                   §b4ltfd
064980070508    3c****               endif
064990070320     c                   eval      tipoEla='A'
065000070320     c                   exsr      GestAR4L
065010140212     c* richiamo pgm per eventuale scrittura/cancellazione file spia
065020150603     c* per aggiungere il file spia per la sped, la bolla non deve essere in stati
065030150603     c*  particolari
065040150603     c                   clear                   tnsd99ds
065050150603     c                   clear                   statofd           1
065060150603     c                   if        wffd='S'
065070150603     C                   MOVEL     'L'           D98tla
065080150603     C                   MOVEL     'A'           D98tbo
065090150603     C                   Z-ADD     arbaas        D98aas
065100150603     C                   Z-ADD     arblnp        D98lnp
065110150603     C                   Z-ADD     arbnrs        D98nrs
065120150603     C                   Z-ADD     arbnsp        D98nsp
065130150603     C*
065140150603     C                   call      'TNSD99R'
065150150603     C                   parm                    tnsd99ds
065160150603     c                   if        D98SPCDEE='FFD' or %subst(D98SPCDEE:1:2)='FD'
065170150603     c                   eval      statofd='S'
065180150603     c                   endif
065190150603     c                   endif
065200150603     c
065210150603     c*****              if        wffd=*blanks or vidtfd=*blanks
065220150603     c                   if        wffd=*blanks or (vidtfd=*blanks and
065230150603     c                             statofd='S')
065240140212     c                   clear                   trtcp2ds
065250140213     c                   if        wffd=*blanks
065260140212     c                   eval      t2chiamata='A'
065270140212     c                   endif
065280140212     c                   eval      t2chi= 'F'
065290140212     c                   eval      T2AAS=d48aas
065300140212     c                   eval      T2LNP=d48lnp
065310140212     c                   eval      T2NRS=d48nrs
065320140212     c                   eval      T2NSP=d48nsp
065330140213     c* passo in t2fgs la lna (serve per far funzionare il chiodo nel trtcp2r)
065340140213     c                   eval      T2fgs=arblna
065350140212     c                   call      'TRTCP2R'
065360140212     c                   parm                    trtcp2ds
065370140213     c                   endif
065380070320     c
065390070320    2C                   END
065400070320    1C                   END
065410070320     c
065420041007     c* Per l'altro file sistemo eventuale volume da fatturare e P.I
065430041007     c*  e aggiorno
065440041007     c                   if        waltrabolla=' '
065450041007     c   02              if        waltrofvf='T'  or
065460041007     c                             (waltrofvf='Z' and arbfvf<>'T' aND
065470041007     C                              WALTROVLF> arbvlf)
065480041008     c                   movel     waltrofvf     arbfvf
065490041008     c                   movel     waltrovlf     arbvlf
065500041007     c                   endif
065510041007     c* Se 1°bolla franco, tengo la p.i. presente nel record
065520151223     c                   if        (*in10 and *in06) or
065530151223     c                             (not *in10 and *in05)
065540041007     c                   eval      arbcpi=waltrocpi
065550041027     c                   else
065560041027     c                   eval      arbcpi=NEWCPI
065570041007     c                   endif
065580041007     c
065590041007     c* aggiornamenti di campi mirati
065600041007     C   10              except    arbdblp
065610041007     C  N10              except    arbdarb
065620041007     c                   endif
065630151223
065640151223     c                   endif
065650950221     C*
065660950221     C* DISALLOCO  I FILES CHE NON USO
065670051118     C   10              UNLOCK    FiAR901L
065680991001     C                   UNLOCK    FIAR601L
065690061116     c
065700061116     c* Se si tratta di bolla in assegnato con possibilità di scrivere il
065710061116     c*  cod fiscale, richiamo la REGIS11 per la memorizzazione
065720151230     c                   if        wnoindir=' ' and
065730151230     c                             (not *in10 or wfile='A')
065740061116     c                   if        *in01 and not *in05
065750061116     c                   if        (vidcdf=*blanks  and war4Q='D') or
065760061116     c                             vidcdf<>*blanks
065770061116     c                   eval      v11mide='D'
065780061116     c                   eval      v11cdfis=vidcdf
065790071102     c* Imposto anche il campo della partita iva di nuovo
065800071102     c                   eval      v11pid=vidpid
065810071102     c
065820061116     c                   movel     'FI'          vidcvr
065830071026     c* Non devo aggiornare la bolla perchè l'ho già fatto qui per la
065840071026     c*  partita IVA
065850130412     c                   if        v11pid<>*blanks or
065860130412     c                              %subst(v11cdfis:12:1)<> ' '
065870071026     c                   eval      Aggiobol='N'
065880130402     c                   endif
065890061116     c                   exsr      REGIS11
065900061116     c                   endif
065910061116     c                   endif
065920141215     c                   endif
065930041007     c*
065940141212     c                   if        wnoindir='S'
065950141212     c                   eval      wdisag=wdisagdest
065960141212     c                   endif
065970041007     c* Riaggancio la bolla in modifica, per controlli sul
065980041007     c*  destinatario disagiato e consegna richiesta rispetto a FFD
065990151223     c                   if        wfile='A'
066000151223     C     KARB          CHAIN(n)  FNARB01L                           30
066010151223     c                   else
066020151223     C     KARB          CHAIN(n)  FNBLP01L                           30
066030151223     c                   endif
066040070906     c* Se in destinatario non è più A D o S tolgo se non immesso
066050070906     c*  dalla partenza
066060070911     c                   if        wdisagdest<>wdisag and wdisagdest<>' ' and
066070070910    2c                             (arbtc1=wdisagdest or arbtc2=wdisagdest)
066080070906     c                   exsr      ContrTCP
066090070906     c                   endif
066100040224
066110950602     C*
066120020531     C* SE destinatario disagiato devo rifare il giro per le consegne
066130020531     c* particolari
066140041015     C***  wdisag        IFne      *blanks
066150041015     c***  arbtc1        andne     wdisag
066160041015     c***  arbtc2        andne     wdisag
066170060327     c***                If        wdisag <> *blanks and
066180060327     c***                          %subst(arbGva:1:1) <> 'U' or
066190060327     c***                          wdisag='X' and %subst(arbGva:1:1)='U'
066200060327     c* Per Disagiato ---->sempre
066210060327     c* per Supermercato ->solo se non è consegna ad uffici
066220060327     c* per Appuntamento ->solo se non è consegna ad uffici e non è
066230070917     c*                    prevista data consegna "Tassativa" da partenza
066240070917     c                   movel     arbdcr        w008a
066250070907    1c                   if        (wdisag = 'X') OR
066260060327     c                             (wdisag='S' and %subst(arbGva:1:1)<>'U') OR
066270060327     c                             (Wdisag='A' and %subst(arbGva:1:1)<>'U' and
066280070917     c                             (arbdcr=*zeros or arbtcr <> *blanks or
066290070917     c                              (par5mdp='S' and w008a<>par5gdcr)))
066300070907    2c     arbtc1        ifne      wdisag
066310041015     c     arbtc2        andne     wdisag
066320030122     c                   Eval      wI0 = 'S'
066330020531     C                   MOVEL     'CP'          D48CVB
066340070907     C                   MOVEL     'D'           D48TRC
066350070911     C***                MOVEL     'E'           WESEG
066360070911     c***                clear                   W48f12
066370070907     c***                if        *in20
066380050502     c                   eval      §bgtcr=arbtcr
066390050502     c                   eval      §bghcr=arbhcr
066400050502     c                   eval      §bgdcr=arbdcr
066410050502     c                   eval      §bggc1=arbgc1
066420050502     c                   eval      §bggc2=arbgc2
066430050502     c                   eval      §bgtc1=arbtc1
066440050502     c                   eval      §bgtc2=arbtc2
066450070907     c***                endif
066460070910     c
066470070910     c* Se da eliminare anche la consegna particolare imposto
066480070910     c*  insieme la variazione
066490070910     c                   if        eliminatcp='1'
066500070910     c                   clear                   §bgtc1
066510070911     c* ripristino la vecchia consegna partic.
066520070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
066530070911     c                   eval      §bgtc1=aggtcp
066540070911     c                   endif
066550070910     c                   endif
066560070910     c                   if        eliminatcp='2'
066570070910     c                   clear                   §bgtc2
066580070911     c* ripristino la vecchia consegna partic.
066590070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
066600070911     c                   if        §bgtc1=' '
066610070911     c                   eval      §bgtc1=aggtcp
066620070911     c                   else
066630070911     c                   eval      §bgtc1=aggtcp
066640070911     c                   endif
066650070911     c                   endif
066660070910     c                   endif
066670070911     c
066680070907     c* Se modificato destinatario in disagiato o supermercato imposto il
066690070907     c* relativo flag nelle cosegne particolari
066700070907    3c     wdisag        ifne      *blanks
066710070907     c                   select
066720070907     c     §bgtc1        wheneq    *blanks
066730070907     c                   movel     wdisag        §bgtc1
066740070907     c     §bgtc2        wheneq    *blanks
066750070907     c                   movel     wdisag        §bgtc2
066760070907     c                   endsl
066770070907    4c     §bgtc1        ifeq      §bgtc2
066780070907     c                   clear                   §bgtc2
066790070907    4c                   endif
066800070907    3c                   endif
066810070907     c* Memorizzo in skiera
066820070907    3c                   if        kk<=10
066830070910     c* se devo eliminare una consegna particolare, memorizzo nella
066840070910     c* stessa variazione
066850070910     c                   if        eliminatcp=' '
066860070907     c                   add       1             kk
066870070910     c                   endif
066880070911     c* Passo dati in DS
066890070907     c                   movel     dsarbg        kdati(KK)
066900070907     c                   movel     d48cvb        kcvb(KK)
066910070907     c                   movel     d48trc        ktrc(KK)
066920070911     c* videata si vede o meno in base al richiamo prevedente
066930070911     c                   movel     d48ffr        kffr(KK)
066940070911     c* Disabilito F12
066950070911     c                   movel     ' '           kF12(KK)
066960070907    3c                   endif
066970070907     c
066980070907    2c                   endif
066990070910     c
067000070910   x1c                   else
067010070910     c* Se non devo aggiungere delle consegne particolari,
067020070910      *  se c'è la data consegna richiesta controllo se è ancora valida
067030070910      *  solo se ho fatto una modifica in arrivo
067040070914     c* Solo se non devo aggiungere delle consegnee particolari
067050070914     c*  altrimenti ilk controllo lo faccio nella regis7
067060070914     c                   if        aggtcp=*blanks
067070071109     c
067080071109     c* Imposto il campo WGIAC in base alla giacenza
067090071109if  2c                   If        *In04 and wgcfas = 35
067100071109if  3c                   If        wgcded > *Zeros and arbdcr >= wgcded
067110071109     c                   Eval      wgiac = 'S'
067120071109    3c                   EndIf
067130071109if  3c                   If        wgcded = *Zeros and arbDcr >= wgcddm
067140071109     c                   Eval      wgiac = 'S'
067150071109    3c                   EndIf
067160071109    3c                   EndIf
067170071109     c
067180160114    2c                   If        ArbDcr > *Zeros and *In10  and wfile='A'
067190070910     c                             and wCa = *Blanks and wgiac = *Blanks
067200070910     c                   ExSr      ContrDcr
067210070910    2c                   EndIf
067220070914    2c                   EndIf
067230070907    1c                   endif
067240070907     c
067250070907     c                   clear                   eliminatcp
067260070911     c                   clear                   aggtcp
067270130411
067280130411     c* Richiamo routine per invio variazione a pda
067290160330     c                   if        wfile='A' and wtogli=*blanks
067300130411     c                   exsr      sr_pda
067310160112     c                   endif
067320911209     C*
067330911212     C     ENDRG2        ENDSR
067340130930     c
067350130930     c* Aggiorno referente -----------------------------------------------------*
067360130930     c     AggioREF      BEGSR
067370130930     c
067380130930      * Referente e Telefono e magazzino giacenza
067390130930     c                   Eval      kAr5Trd = 'GEN'
067400130930     c     kAr5          Chain     Fiar501l
067410130930      * Aggiorno Fiar5
067420130930if  2c                   If        %Found(Fiar501l)
067430130930     c                   movel     ar5uni        DAr5Gen
067440130930     c                   Eval      §Ar5Ref = VidRef
067450130930     c                   Eval      §Ar5Teld = VidTeld
067460130930     c
067470130930     c* Solo per causale I0 aggiorno anche mag giacenza e f.deposito
067480130930     c                   if        flgcvr = 'I'
067490130930     c                   Eval      §Ar5Zng = VidZng
067500130930     c
067510130930     c                   if        wffd<>oldffd
067520130930     c                   if        wffd='S'
067530130930     c                   eval      §ar5dfd=%editc(dateu:'X')
067540130930     c                   else
067550130930     c                   clear                   §ar5dfd
067560130930     c                   endif
067570130930     c                   endif
067580130930     c
067590130930     c                   endif
067600130930     c
067610130930     c                   Movel(p)  DAr5Gen       Ar5Uni
067620130930      * se devo trasmettere alla sede sfleggo per la trasmissione
067630130930     c                   If        §Ar5Trse = 'S'
067640130930     c                   Clear                   Ar5Ft2
067650130930     c                   EndIf
067660130930      * sfleggo sempre per la trasmissione al cliente
067670130930     c                   Clear                   Ar5Ft3
067680130930     c                   Update    Fiar5000
067690130930      * Scrivo Fiar5
067700130930   x2c                   Else
067710130930      * se immessi i dati
067720130930      * non mi preoccupo per il fermo deposito: il fiar5 record GEN viene sempre creato
067730130930      * in immissione bolle. Non esiste più il caso di fiar5 record GEN inesistente in fase
067740130930      * di manutenzione bolle
067750130930if  3c                   If        VidRef <> *Blanks or VidTeld <> *Blanks or
067760130930     c                             VidZng <> *Blanks
067770130930     c                   Clear                   Fiar5000
067780130930     c                   Eval      Ar5Aas = ArbAas
067790130930     c                   Eval      Ar5Lnp = ArbLnp
067800130930     c                   Eval      Ar5Nrs = ArbNrs
067810130930     c                   Eval      Ar5Nsp = ArbNsp
067820130930     c                   Eval      Ar5Trd = 'GEN'
067830130930     c                   Move      Dateu         Ar5Dac
067840130930     c                   Movel     W0140         Ar5Orc
067850130930     c                   Clear                   Dar5Gen
067860130930     c                   Eval      §Ar5Ref = VidRef
067870130930     c                   Eval      §Ar5Teld = VidTeld
067880130930     c                   Eval      §Ar5Zng = VidZng
067890130930     c                   Movel(p)  DAr5Gen       Ar5Uni
067900130930     c                   Write     Fiar5000
067910130930    3c                   EndIf
067920130930    2c                   EndIf
067930130930     c                   ENDSR
067940911210     C*
067950911210     C*--- CONTROLLI FORMATO3 ----------------------------------------*
067960911210     C     CONTR3        BEGSR
067970941209     C*
067980920319     C* CONTROLLI COMUNI CON CONTR4
067990920319     C                   EXSR      CTRCOM
068000941209     C   90              GOTO      ENDC03
068010920319     C*
068020920319     C* T A S S A Z I O N E
068030941222     C* CONTROLLO L'IMPORTO DA ASSICURARE SE SONO IN ARRIVO SOLO PER
068040941222     C*  ASSEGNATO, IN PARTENZA SEMPRE
068050941222    1C     *IN36         IFEQ      *OFF
068060941222     C     *IN10         OREQ      *OFF
068070911211     C*
068080011205     C*** I M P O R T O  D A  A S S I C U R A R E  ***
068090080701      **** OBSOLETO DAL 1 GIUGNO 2008 CHE È STATO INSTALLATO LIQUIDAZIONE TANSATTIVA AUTOMATICA ***
068100990127     C*
068110990127     C* IMPOSSIBILE INSERIRE IMPORTO DA ASSICURARE SE BOLLA SI C.A.
068120990127     C*  IN LIQ.TRANSATTIVA
068130080701    2C***  VIDIAS        IFGT      0
068140990127     C* VEDO SE C'E' C.A.
068150040809     C**""*KARB          CHAIN     FNDCT02L                           30
068160040809    3C**""**IN30         DOWEQ     *OFF
068170040809    4C**""*DCTATB        IFEQ      ' '
068180990127     C*
068190040809    5C**""*DCTFPR        IFEQ      'T'
068200040809     C**""*              MOVEL     MSG(82)       VIDMSG
068210040809     C**""*              SETON                                        30
068220040809     C**""*              SETON                                        409028
068230040809    5C**""*              ENDIF
068240040809    4C**""*              ENDIF
068250990127     C**
068260040809     C**""*  N30KARB          READE     FNDCT02L                               30
068270040809    3C**""*              ENDDO
068280040809      *
068290040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
068300040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
068310040809      * legate alla spedizione che passo
068320080701      **** OBSOLETO DAL 1 GIUGNO 2008 CHE È STATO INSTALLATO LIQUIDAZIONE TANSATTIVA AUTOMATICA ***
068330080701      ***    NON FACCIAMO PIUI' IL CONTROLLO ****
068340080701     c*                  clear                   fidn12ds
068350080701     c*                  eval      i12tla = 'L'
068360080701     c*                  eval      i12aas = D48AAS
068370080701     c*                  eval      i12lnp = D48LNP
068380080701     c*                  eval      i12nrs = D48NRS
068390080701     c*                  eval      i12nsp = D48NSP
068400080701     c*                  eval      i12fel = 'E'
068410080701     c*                  eval      i12fan = 'E'
068420040809     c*
068430080701     c*                  call      'FIDN12R'
068440080701     c*                  parm                    fidn12ds
068450040809     c*
068460080701     c*                  setoff                                       30
068470040809     c*
068480040809     c* se non ci sono errori
068490080701    3C*                  if        o12err <> ' '
068500080701     c*                  seton                                        30
068510080701     c*                  else
068520040809     c* se numero di CA trovate maggiore di zero
068530080701    4C*                  if        o12nca = 0
068540080701     c*                  seton                                        30
068550080701     c*                  else
068560080701     c*                  z-add     1             jj                2 0
068570080701    5c*                  dow       jj <= o12nca and *in30 = *off
068580080701     c*                  movea     skkey(jj)     dskey
068590040809     c*
068600040809     c* Aggancio il FNDCT01L x reperire se la CA è in "Liquidazione Transattiva"
068610080701     c*    KDCT1_C       chain     FNDCT01L
068620080701    6c*                  if        %found(FNDCT01L)
068630080701    7C*    DCTFPR        IFEQ      'T'
068640080701     C*                  MOVEL     MSG(82)       VIDMSG
068650080701     C*                  SETON                                        30
068660080701     C*                  SETON                                        409028
068670080701    7C*                  ENDIF
068680080701    6c*                  endif
068690040809     c*
068700080701     c*                  add       1             jj
068710080701    5c*                  enddo
068720040809     c*
068730040809     c                   seton                                        30
068740040809     c*
068750080701    4c*                  endif
068760080701    3c*                  endif
068770990127     C**
068780080701     C** 90              GOTO      ENDC03
068790080701    2C***                ENDIF
068800941215     C*
068810941215     C* SE IL CAMPO = '   ' PRENDO LA DIVISA DALLA NAZIONE DEL MITTENT
068820941215     C* LO EVIDENZIO A VIDEO
068830950120     C* SE VARIATO L'IMPORTO ME LO SALVO
068840950120     C     *IN97         IFEQ      *ON
068850950120     C                   MOVEL     '1'           WIN97             1
068860950120     C                   ENDIF
068870950120     C*
068880941215     C*
068890941215     C                   MOVEL     VIDVAS        WVLT
068900941215     C                   Z-ADD     VIDIAS        W0133            13 3
068910941215     C* CONTROLLO LA DIVISA
068920941215     C                   EXSR      CTRVLT
068930941215     C                   MOVEL     WVLT          VIDVAS
068940941215     C* ERRORE
068950941215     C   90              SETON                                        41
068960941215     C   90              GOTO      ENDC03
068970011009      *
068980011009      * Controllo se la divisa immessa è scaduta o no
068990011009     C     ARBDSP        IFGT      §CVDLI
069000011105     C     VIDIAS        ANDGT     0
069010011102     C                   MOVEL     MSG(102)      VIDMSG
069020011009     C                   SETON                                        419028
069030011009     C                   GOTO      ENDC03
069040011009     C                   ENDIF
069050941215     C*
069060941215     C* SE VARIATA LA VALUTA FACCIO RIDIGITARE L'IMPORTO
069070941222    2C     VIDVAS        IFNE      VARVAS
069080950120     C     WIN97         ANDEQ     *BLANKS
069090941222    3C     VIDVAS        IFNE      *BLANKS
069100941219     C     VIDIAS        ORNE      0
069110941215     C                   MOVEL     MSG(28)       VIDMSG
069120941215     C                   SETON                                        409028
069130941215     C                   GOTO      ENDC03
069140941222    3C                   ENDIF
069150941219    2C                   ENDIF
069160941215     C*
069170941215     C                   MOVEL     VIDVAS        VARVAS
069180950120     C                   CLEAR                   WIN97
069190941215     C*
069200011205
069210011205      * pulisco il flag x la forzatura importo da assicurare forzabile
069220011205     c                   if        vidias <> sav1_vidias
069230011205     c                   clear                   forzaias
069240080625     c                   clear                   forzaias2
069250011205     c                   endif
069260011205      *---------------------
069270011205  2b c                   if        vidias <> 0
069280011205     c                   eval      sav1_vidias = vidias
069290011205      * Controllo l'importo da assicurare tramite il trul22r
069300011205     c                   clear                   trul22ds
069310011205     c                   eval      i22tla = 'L'
069320011205     c                   eval      i22cbo = arbcbo
069330011205     c                   eval      i22tsp = arbtsp
069340011205     c                   eval      i22lnp = arblnp
069350011205     c                   eval      i22nzm = arbnzm
069360011205     c                   eval      i22lna = arblna
069370011205     c                   eval      i22nzd = arbnzd
069380011205     c                   movel     vidklp        i22ksc
069390011205     c                   move      vidksc        i22ksc
069400011205     c                   eval      i22ctr = vidctr
069410011205     c                   eval      i22imp = vidias
069420011205     c                   eval      i22div = vidvas
069430011205     c                   eval      i22ute = knmus
069440011205     c                   eval      i22pgm = nompgm
069450011205     c                   call      'TRUL22R'
069460011205     c                   parm                    trul22ds
069470011205      * se ho tutti i flag impostati non posso immettere l'importo
069480011205      * da assicurare
069490011205  3b c                   if        o22fmn <> *blanks and o22fx1 <> *blanks
069500011205     c                             and o22fx2 <> *blanks
069510011205     c                   eval      vidmsg = msg(79)
069520011205     c                   eval      *in40 = *on
069530011205     c                   eval      *in90 = *on
069540011205     c                   eval      *in28 = *on
069550011206     c                   goto      endc03
069560011205  3e c                   endif
069570011205      * controllo che non sia minore del limite previsto
069580011205  3b c                   if        o22fmn <> *blanks
069590011205     c                   eval      vidmsg = msg(87)
069600011205     c                   eval      *in40 = *on
069610011205     c                   eval      *in90 = *on
069620011205     c                   eval      *in28 = *on
069630011206     c                   goto      endc03
069640011205  3e c                   endif
069650011205      * controllo che non sia maggiore del limite previsto
069660011205      * non forzabile
069670011205  3b c                   if        o22fx2 <> *blanks
069680011205     c                   eval      vidmsg = msg(39)
069690011210     c                   eval      %subst (vidmsg:38:14) = (%editc(o22lx2: '4'))
069700011205     c                   eval      *in40 = *on
069710011205     c                   eval      *in90 = *on
069720011205     c                   eval      *in28 = *on
069730011206     c                   goto      endc03
069740011205  3e c                   endif
069750011205      * controllo che non sia maggiore del limite previsto
069760011205      * forzabile
069770011205  3b c                   if        o22fx1 <> *blanks and forzaias = *blanks
069780011205     c                   eval      forzaias = '1'
069790011205     c                   eval      vidmsg = msg(80)
069800011210     c                   eval      %subst (vidmsg:28:14) = (%editc(o22lx1: '4'))
069810011205     c                   eval      *in40 = *on
069820011205     c                   eval      *in90 = *on
069830011205     c                   eval      *in28 = *on
069840011206     c                   goto      endc03
069850011205  3e c                   endif
069860011205  2e c                   endif
069870011205
069880941209     C*
069890941209     C* SE HA TARIFFE, CONTROLLO
069900941209    2C     WTKS          IFEQ      'S'
069910941209     C     KTAM4         CHAIN     TNTAM000                           30
069920941209    3C     *IN30         IFEQ      *OFF
069930991028     C                   MOVEL     TAMFLO        DSTA01
069940991028     C                   MOVEL     'CV'          COD
069950991028     C                   MOVEL(P)  §TADIV        KEY
069960991028     C                   EXSR      CTABEL
069970991028     C                   MOVEL     TBLUNI        DSCV
069980011010     C                   MOVEL     §CVDEC        COMDEC
069990941209     C*
070000941215     C* PULISCO CAMPI D FORZATURA
070010011010    4C     W0133         IFNE      VARIAS
070020941209     C                   CLEAR                   DIE
070030941215     C                   CLEAR                   CIN
070040941215    4C                   END
070050020715
070060020715      * Non controlla da tariffa se obbligatorio nel caso di tipo bolla non previsto
070070020715     c     §3atb1        Lookup    tbl                                    30
070080020715
070090020715   4Ac                   If        *in30 = *On
070100941209     C*
070110941209     C* SOLO SE E' ASSICURAZIONE ANCHE PER GLI ARRIVI ( <> P )
070120941209     C*   ED E' ESPRESSO IL VALORE MERCE NELLA BOLLA
070130990818     C     KTPT          CHAIN     TITPT000                           30
070140950810    4C     *IN30         IFEQ      *ON
070150950810     C     TPTATB        ORNE      ' '
070160941222     C*
070170950810     C     *IN10         OREQ      *ON
070180941209     C     TPTAPL        ANDNE     'P'
070190941222     C*
070200941222     C     *IN10         OREQ      *OFF
070210941222     C     TPTAPL        ANDNE     'A'
070220941215     C*
070230011010     C                   Z-ADD     W0133         VARIAS
070240941215     C*
070250941215     C* TAMFIS=1 OBBLIGATORIO     FORZABILE --> CON ENTER PRENDO MAX
070260941215     C*                                         IMPORTO DA TARIFFA
070270941215    5C     TAMFIS        IFEQ      '1'
070280941215     C     CIN           ANDNE     1
070290011010     C     W0133         ANDEQ     0
070300941215     C                   MOVEL     MSG(40)       VIDMSG
070310941215     C                   SETON                                        409028
070320941215     C                   ADD       1             CIN
070330941215     C                   GOTO      ENDC03
070340941215    5C                   ENDIF
070350941215     C*
070360941215     C* TAMFIS=3 OBBLIGATORIO NON FORZABILE --> ERRORE SEMPRE
070370941215    5C     TAMFIS        IFEQ      '3'
070380011010     C     W0133         ANDEQ     0
070390941215     C                   MOVEL     MSG(42)       VIDMSG
070400941215     C                   SETON                                        409028
070410941215     C                   GOTO      ENDC03
070420941215    5C                   ENDIF
070430941215     C*
070440941215     C* TAMFIS=2 NON INSERIBILE             --> ERRORE SEMPRE
070450941215    5C     TAMFIS        IFEQ      '2'
070460011010     C     W0133         ANDNE     0
070470941215     C                   MOVEL     MSG(43)       VIDMSG
070480941215     C                   SETON                                        409028
070490941215     C                   GOTO      ENDC03
070500941215    5C                   ENDIF
070510941215     C*
070520941215     C* SE <> 0 CONTROLLO CHE NON SUPERI IL VALORE IN TARIFFA
070530950810    5C  N30TPTATB        IFEQ      ' '
070540011010     C     W0133         ANDGT     0
070550941215     C     DIE           ANDNE     1
070560941215     C     TPTVLM        ANDGT     0
070570941215     C*
070580941209     C* VALORE A COLLO
070590941215    6C     TPTFVM        IFEQ      '1'
070600991028     C     ARBNCL        MULT(H)   TPTVLM        TPTVAL           13 3
070610941215   X6C                   ELSE
070620941215    7C     TPTFVM        IFEQ      '2'
070630991028     C                   Z-ADD     TPTVLM        TPTVAL
070640941215   X7C                   ELSE
070650941212     C     ARBPKF        ADD       0,9           W0060             6 0
070660991028     C     W0060         MULT(H)   TPTVLM        TPTVAL
070670941215    7C                   END
070680941215    6C                   END
070690991028     C*
070700011205     C                   z-add     vidias        vidval           13 3
070710991028    7C     §TADIV        IFNE      VIDVAS
070720030612     C                   CLEAR                   YeurcoDS
070730991028     C                   MOVEL     VIDVAS        YECDVI
070740991028     C                   MOVEL     §TADIV        YECDVO
070750991028     C                   Z-ADD     VIDIAS        YECIMI
070760011010     C                   MOVE      COMDEC        YECDCO
070770991028     C*
070780991028     C                   CALL      'YEURCO'
070790030612     C                   PARM                    YeurcoDS
070800991028     C*
070810991028    8C     YECESI        IFEQ      ' '
070820991028     C                   Z-ADD     YECIMO        VIDVAL
070830991028    8C                   ENDIF
070840991028    7C                   ENDIF
070850941209     C*
070860991028    6C     VIDVAL        IFGT      TPTVAL
070870941209     C                   ADD       1             DIE
070880011205     c                   eval      vidmsg = msg(44)
070890941209     C                   SETON                                        409028
070900941209     C                   GOTO      ENDC03
070910941215    6C                   ENDIF
070920941209     C*
070930941215    5C                   ENDIF
070940941222    4C                   ENDIF
070950020715   4ac                   EndIf
070960941209    3C                   ENDIF
070970941209    2C                   ENDIF
070980030710
070990030710      * calcolo data limite mod. importo da assicurare
071000060201     c**   *iso          Move      ArbDsp        wdataiso
071010060201     c**                 adddur    §vpomia:*d    wdataiso
071020030722      *
071030060201     c**                 do        *hival
071040030722      * verifico se è un giorno lavorativo
071050060201     c**   *iso          move      wdataiso      ds_data
071060060201     C**                 eval      kann = ds_anno
071070060201     C**                 eval      kmes = ds_mese
071080060201     C**   kazcln        chain     azcln01l
071090060201     C**                 if        %found(azcln01l)
071100060201     C**                 if        MAT(ds_giorno) =  'F'  or
071110060201     C**                           POM(ds_giorno) =  'F'
071120030722      * se non va bene aggiungo 1 giorno
071130060201     c**                 adddur    1:*d          wdataiso
071140060201     C**                 iter
071150060201     c**                 else
071160060201     c**                 leave
071170060201     C**                 endif
071180060201     c**                 else
071190060201     c**                 leave
071200060201     C**                 endif
071210030722
071220060201     c**                 enddo
071230030710      * se viene modificato l'importo da assicurare rispetto a quanto
071240030710      * scritto sul file (stessa cosa anche x la divisa)
071250030710      * devo controllare se i gg. limite x la manutenzione sono trascorsi o meno
071260030710      * se sono già trascorsi emetto una window x richiedere la password
071270060201     c**                 If        (VidIas <> ArbIas or
071280060201     c**                           (VidVas <> ArbVas and ArbVas <> *Blanks)) and
071290060201     c**                            wdataoggi > wdataiso
071300060201     c**                           and wokpsw = *Blanks
071310060201     c**                 Exsr      Sr_Gespsw
071320060201     c** 90              GoTo      EndC03
071330060201     c**                 EndIf
071340941209     C***
071350011206     C*** V A L O R E   M E R C E   D I C H I A R A T O ***
071360941209     C* OBBLIGATORIO SE DESTINATARIO ESTERO E C'E' IN TABELLA NAZIONE
071370941209     C* IL FLAG EFTA
071380950120     C* SE HO VARIATO L'IMPORTO LO SALVO
071390950120     C     *IN93         IFEQ      *ON
071400950120     C                   MOVEL     '1'           WIN93             1
071410950120     C                   ENDIF
071420941209     C*
071430941209     C                   MOVEL     VIDVAD        WVLT
071440941209     C                   Z-ADD     VIDVMD        W0133            13 3
071450941209     C* CONTROLLO LA DIVISA
071460941209     C                   EXSR      CTRVLT
071470941209     C                   MOVEL     WVLT          VIDVAD
071480941209     C* ERRORE
071490941209     C   90              SETON                                        44
071500941209     C   90              GOTO      ENDC03
071510011010      *
071520011010      * Controllo se la divisa immessa è scaduta o no
071530011010     C     ARBDSP        IFGT      §CVDLI
071540011105     C     VIDVMD        ANDGT     0
071550011102     C                   MOVEL     MSG(102)      VIDMSG
071560011010     C                   SETON                                        449028
071570011010     C                   GOTO      ENDC03
071580011010     C                   ENDIF
071590941215     C*
071600941215     C* SE HO MODIFICATO LA VALUTA DEVO RIDIGITARE L'IMPORTO
071610941215     C* INDICATORE DI CHANGE 93 ON
071620941215    2C     VARVAD        IFNE      VIDVAD
071630950120     C     WIN93         ANDEQ     *BLANKS
071640941219     C     VIDVAD        IFNE      *BLANKS
071650941219     C     VIDVMD        ORNE      0
071660941215     C                   MOVEL     MSG(28)       VIDMSG
071670941215     C                   SETON                                        439028
071680941215     C                   GOTO      ENDC03
071690941215    2C                   ENDIF
071700941219     C                   ENDIF
071710941215     C*
071720941215     C                   MOVEL     VIDVAD        VARVAD
071730950120     C                   CLEAR                   WIN93
071740011205
071750011205      * pulisco il flag x la forzatura importo da assicurare forzabile
071760011205     c                   if        vidvmd <> sav1_vidvmd
071770011205     c                   clear                   forzavmd
071780011205     c                   endif
071790011205      *---------------------
071800011205     c                   eval      sav1_vidvmd = vidvmd
071810011205      * Controllo il valore merce tramite il trul23r
071820011205     c                   clear                   trul23ds
071830011205     c                   eval      i23tla = 'L'
071840011205     c                   eval      i23cbo = arbcbo
071850011205     c                   eval      i23tsp = arbtsp
071860011205     c                   eval      i23lnp = arblnp
071870011205     c                   eval      i23nzm = arbnzm
071880011205     c                   eval      i23lna = arblna
071890011205     c                   eval      i23nzd = arbnzd
071900011205     c                   movel     vidklp        i23ksc
071910011205     c                   move      vidksc        i23ksc
071920011205     c                   eval      i23ctr = vidctr
071930011206     c                   eval      i23imp = vidvmd
071940011206     c                   eval      i23div = vidvad
071950011205     c                   eval      i23ute = knmus
071960011205     c                   eval      i23pgm = nompgm
071970011205     c                   call      'TRUL23R'
071980011205     c                   parm                    trul23ds
071990011205      * se ho il flag del limite minimo impostato e l'importo è a zero
072000011205      * è obbligatorio
072010011206  2b c                   if        o23fmn <> *blanks and vidvmd = 0
072020011205     c                   eval      vidmsg = msg(45)
072030011205     c                   eval      *in43 = *on
072040011205     c                   eval      *in90 = *on
072050011205     c                   eval      *in28 = *on
072060011205     c                   goto      endc03
072070011206  2e c                   endif
072080011205      * controllo che non sia minore del limite previsto
072090011206  2b c                   if        o23fmn <> *blanks
072100011205     c                   eval      vidmsg = msg(104)
072110011210     c                   eval      %subst (vidmsg:37:14) = (%editc(o23lmn: '4'))
072120011205     c                   eval      *in43 = *on
072130011205     c                   eval      *in90 = *on
072140011205     c                   eval      *in28 = *on
072150011205     c                   goto      endc03
072160011206  2e c                   endif
072170011205      * controllo che non sia maggiore del limite previsto
072180011205      * non forzabile
072190011206  2b c                   if        o23fx2 <> *blanks
072200011205     c                   eval      vidmsg = msg(105)
072210011210     c                   eval      %subst (vidmsg:53:14) = (%editc(o23lx2: '4'))
072220011205     c                   eval      *in43 = *on
072230011205     c                   eval      *in90 = *on
072240011205     c                   eval      *in28 = *on
072250011205     c                   goto      endc03
072260011206  2e c                   endif
072270011205      * controllo che non sia maggiore del limite previsto
072280011205      * forzabile
072290011206  2b c                   if        o23fx1 <> *blanks and forzavmd = *blanks
072300011205     c                   eval      forzavmd = '1'
072310011205     c                   eval      vidmsg = msg(106)
072320011210     c                   eval      %subst (vidmsg:30:14) = (%editc(o23lx1: '4'))
072330011205     c                   eval      *in43 = *on
072340011205     c                   eval      *in90 = *on
072350011205     c                   eval      *in28 = *on
072360011205     c                   goto      endc03
072370011206  2e c                   endif
072380930507     C*
072390930507    1C                   END
072400950424     C*
072410950426     C*** CONTROLLI PER CLIENTE CON TARIFFA A VALORE CON FLAG = '3' ***
072420950426    1C     WTKS          IFEQ      'S'
072430950424     C                   MOVEL     VIDCTR        W001A             1
072440950426    2C     W001A         IFEQ      '4'
072450950424     C     TAMFVD        ANDEQ     '3'
072460950426     C*
072470950426    3C     VIDKLP        IFNE      VARKLP
072480950426     C     VIDKSC        ORNE      VARKSC
072490950426     C     VIDIAS        ORNE      COMIAS
072500950426     C                   SELECT
072510950426     C* FATTURA IMMEDIATA
072520950426     C* Se modificato il cod.cliente o l'importo da assicurare
072530950426     C* è necessario che la q.tà da fatturare e importi tassazione
072540950426     C* vengano azzerati + F14
072550950426     C     *IN14         WHENEQ    *ON
072560991001     C* Carico varie del video in schiera per vedere se c'è l'inoltro
072570991001     C                   MOVEL     '1'           WCKINL            1
072580991001     C                   EXSR      CARWAR
072590950426    4C     VIDQFT        IFNE      *ZEROS
072600950426     C     VIDIMV        ORNE      *ZEROS
072610950426     C     VIDPOR        ORNE      *ZEROS
072620991001     C     WINL          OREQ      '1'
072630950426     C     *INKN         OREQ      *OFF
072640950426     C                   SETON                                        429028
072650950426     C                   MOVEL     MSG(71)       VIDMSG
072660950426     C                   GOTO      ENDC03
072670950426    4C                   END
072680950426     C* SEGUE FATTURA
072690950426     C* Se modificato il cod.cliente o l'importo da assicurare
072700950426     C* è necessario che la q.tà da fatturare e tot imponibile
072710950426     C* vengano azzerati
072720950426     C     *IN13         WHENEQ    *ON
072730950426    4C     VIDQFT        IFNE      *ZEROS
072740950426     C     VIDIMV        ORNE      *ZEROS
072750950426     C                   SETON                                        429028
072760950426     C                   MOVEL     MSG(70)       VIDMSG
072770950426     C                   GOTO      ENDC03
072780950426    4C                   END
072790950426     C                   ENDSL
072800950426     C                   MOVE      VIDKLP        VARKLP
072810950426     C                   MOVE      VIDKSC        VARKSC
072820950426     C                   MOVE      VIDIAS        COMIAS
072830950426     C                   MOVE      *ZEROS        VARQFT
072840950426    3C                   END
072850950426     C*
072860950426     C*** QUANTITA' DA FATTURARE ***
072870950426     C*
072880950426     C* Errore se modificata e diversa da 0
072890950426    3C     VIDQFT        IFNE      VARQFT
072900950426    4C     VIDQFT        IFNE      *ZEROS
072910950426     C                   MOVEL     MSG(68)       VIDMSG
072920950424     C                   SETON                                        429028
072930950424     C                   GOTO      ENDC03
072940950426    4C                   END
072950950426     C* Se azzerata q.tà da fatturare gli
072960950426     C* importi della tassazione devono essere = 0
072970950426     C                   SELECT
072980950426    4C     *IN14         WHENEQ    *ON
072990991001     C* Carico varie del video in schiera per vedere se c'è l'inoltro
073000991001     C                   MOVEL     '1'           WCKINL            1
073010991001     C                   EXSR      CARWAR
073020950426    5C     VIDIMV        IFNE      *ZEROS
073030950426     C     VIDPOR        ORNE      *ZEROS
073040991001     C     WINL          OREQ      '1'
073050950426     C     *INKN         OREQ      *OFF
073060950426     C                   SETON                                        499028
073070950426     C                   MOVEL     MSG(69)       VIDMSG
073080950426     C                   GOTO      ENDC03
073090950426    5C                   END
073100950426    4C     *IN13         WHENEQ    *ON
073110950426    5C     VIDIMV        IFNE      *ZEROS
073120950426     C                   SETON                                        499028
073130950426     C                   MOVEL     MSG(72)       VIDMSG
073140950426     C                   GOTO      ENDC03
073150950426    5C                   END
073160950426    4C                   ENDSL
073170950424     C                   MOVE      VIDQFT        VARQFT
073180950426    3C                   END
073190950424     C*
073200950426    2C                   END
073210950426    1C                   END
073220950424     C*
073230050601     c* Controlli su aggiunte/aumenti di importo da assicurare
073240080625    1c                   If        (VidIas <> ArbIas and vidias > 0 or
073250050804     c                             (VidVas <> ArbVas and vidVas <> *Blanks
073260050804     c                             and Vidias>0))
073270050601     c* verifico se cliente con mandato
073280050601     c                   exsr      tassar
073290050601     c* cliente senza mandato: si può aggiungere/aumentare l'importo solo
073300050601     c* se inviata e-mail
073310050601     c* idem per cliente con mandato se il valore è oltre mandato
073320080625    2c                   if        task88 = 'S'
073330050601     c* vedo se inviata e-mail
073340050609     c                   movel     'c'           kta4trc
073350050601     c                   clear                   dta4c
073360050601     c     kta4          chain     tita430c
073370080625    3c                   if        %found(tita430c)
073380050601     c                   movel     ta4not        dta4c
073390080625    3c                   endif
073400080625    3c                   if        §ta4mail <> 'S'
073410080625    4c                   if        §utemtf=' '
073420050601     c                   seton                                        409028
073430050601     c                   eval      vidmsg=msg(135)
073440050601     c                   goto      endc03
073450080625    4c                   endif
073460080625    4c                   if        (§utemtf='I' or §utemtf='E') and
073470080625     c                             forzaias2=' '
073480080625     c                   eval      forzaias2='1'
073490080625     c                   seton                                        409028
073500080625     c                   eval      vidmsg=msg(146)
073510080625     c                   goto      endc03
073520080625    4c                   endif
073530080625    3c                   endif
073540080625    2c                   endif
073550080625    1c                   endif
073560941221     C*
073570941221     C* F14 - TASSAZIONE
073580991011     C     *INKN         IFEQ      *ON
073590991029     C* Fattura immediata SE NON CONTABILIZZATA
073600991011     C     *IN14         OREQ      *ON
073610991029     C     *IN29         ANDEQ     *OFF
073620991029     c* Prepagati NON CONTABILIZZATA
073630991011     C     *IN10         OREQ      *OFF
073640991011     C     VIDNFT        ANDGT     *ZEROS
073650991029     C     *IN29         ANDEQ     *OFF
073660980513     C                   MOVEL     '1'           WBOL              1
073670011018      * Se è una bolla di reso con varia 'N' 88888888
073680011017      * tasso la varia 'N'
073690011018     C     FLGVRN        IFEQ      '1'
073700011017      * leggo il sbfl delle varie
073710011017     C                   DO        20            NRR
073720011017     C     NRR           CHAIN     LR48S03                            32
073730011017     C     V3CSV1        IFEQ      'N'
073740011017     C     V3CVA1        ANDEQ     88888888
073750011017     C                   EXSR      TASSAN
073760011017     C  N32              UPDATE    LR48S03
073770011017     C                   ENDIF
073780011017     C                   ENDDO
073790011017     C                   ENDIF
073800011017      *
073810151020     c                   if        vidimv<>PesVolImv
073820151016     c                   clear                   PesVolPkcn
073830151016     c                   clear                   PesVolSpc
073840151016     c                   clear                   PesVolImv
073850151020     c                   endif
073860151020     c***                clear                   Tassatoimv
073870941221     C                   EXSR      TASSAZ
073880151019     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
073890151020     c***                if        vidimv>0 and vidimv<>PesVolImv
073900151020     c***                eval      PesVolPkcn=taspkcn
073910151020     c***                eval      PesVolSpc=tasspc
073920151020     c***                eval      PesVolIMV=vidimv
073930151020     c***                endif
073940990702     C* SE SI TRATTA DI UN PREPAGATO DEVO ANCHE VISUALIZZARE IL TOT
073950990702     C*  IMPONIBILE ARROTONDATO
073960990702     C     *IN23         IFEQ      *ON
073970151020     c****               eval      TassatoImv=vidimv
073980990702     C                   Z-ADD     AR6DFT        ARBDFT
073990990702     C                   EXSR      TOTFAT
074000990702     C                   Z-ADD     D04IFT        VIDIFT
074010990702     C                   Z-ADD     D04IMV        VIDIMV
074020990702     C                   Z-ADD     D04POR        VIDPOR
074030991006     C                   Z-ADD     D04POR        VARPOR
074040990702     C                   Z-ADD     D04IMV        SAVIMV
074050990702     C                   ENDIF
074060151020     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
074070151020     c                   if        vidimv>0 and vidimv<>PesVolImv
074080151020     c                   eval      PesVolPkcn=taspkcn
074090151020     c                   eval      PesVolSpc=tasspc
074100151020     c                   eval      PesVolIMV=vidimv
074110151020     c                   endif
074120131016
074130131016     c                   if        *inkn
074140131016     c                   eval      winf14='1'
074150131016     c                   endif
074160991011     C   KN
074170991011     COR 90              GOTO      ENDC03
074180990702    1C                   END
074190991006     C* Se divisa sprotetta significa che manca tariffa: in questo caso
074200991006     C* controllo che la divisa inserita sia = alla divisa della moneta
074210991006     C*  di conto SE CLIENTE 8888 o 9999
074220991006     C*
074230991006     C* Fattura immediata
074240991006     C     *IN14         IFEQ      *ON
074250991006     C* Prepagati
074260991006     C     *IN10         OREQ      *OFF
074270991006     C     VIDNFT        ANDGT     *ZEROS
074280991006     C* Divisa sprotetta
074290991006     C     *IN07         IFEQ      *OFF
074300991006     C* Divisa diversa dalla divisa della moneta di conto
074310991006     C     VIDDIV        ANDNE     §GEDCN
074320991006     C* Cliente 8888 o 9999
074330991006     C     VIDKSC        IFEQ      8888
074340991006     C     VIDKSC        OREQ      9999
074350991006     C                   SETON                                        459028
074360991006     C                   MOVEA     MSG(90)       K78
074370991006     C                   MOVEA     §GEDCN        K78(71)
074380991006     C                   MOVEA     K78           VIDMSG
074390991006     C                   GOTO      ENDC03
074400991006     C                   ENDIF
074410991006     C                   ENDIF
074420991006     C                   ENDIF
074430941102     C**
074440991124     C* SE ABBLENCATA DIVISA ERRORE SE ESISTE ALMENO UN IMPORTO DI
074450991124     C* TASSAZIONE
074460991124    1C     *IN07         IFEQ      *OFF
074470991124     C     VIDDIV        ANDNE     WDIV
074480991124     C     VIDDIV        ANDEQ     *BLANKS
074490991124    2C     VIDPOR        IFGT      *ZEROS
074500991125     C                   SETON                                        459028
074510991124     C                   MOVEL     MSG(94)       VIDMSG
074520991124     C                   GOTO      ENDC03
074530991124   X2C                   ELSE
074540991124    3C                   DO        20            NRR
074550991124     C     NRR           CHAIN     LR48S03                            30
074560991124    4C     V3CVA1        IFGT      *ZEROS
074570991125     C                   SETON                                        459028
074580991124     C                   MOVEL     MSG(94)       VIDMSG
074590991124     C                   GOTO      ENDC03
074600991124    4C                   ENDIF
074610991124    3C                   ENDDO
074620991124    2C                   ENDIF
074630991124    1C                   ENDIF
074640991005     C* PER CONTROLLARE LA TASSAZIONE RICHIAMO PGM FNLV16R
074650991012     C* SE LA DIVISA E' MODIFICATA RICHIAMO L'LV16 SOLO PER IL
074660991012     C* CONTROLLO DELLA DIVISA
074670991013    0C     *IN07         IFEQ      *OFF
074680991013     C     VIDDIV        ANDNE     WDIV
074690991026     C     WDIV          ANDNE     *BLANKS
074700100120     c
074710030612     C                   CLEAR                   Fnlv16DS
074720991012     C                   CLEAR                   DTASV
074730100120     C                   CLEAR                   dlv1601
074740100120
074750991012     C                   MOVEL     'D'           D17FIM
074760991012     C                   MOVE      VIDDIV        D17DIV
074770991012     C     VIDDFT        IFGT      *ZEROS
074780991012     C                   CLEAR                   WLBDAT
074790991012     C                   Z-ADD     VIDDFT        G02DAT
074800991012     C                   MOVEL     *BLANK        G02ERR
074810991012     C                   CALL      'XSRDA8'
074820991012     C                   PARM                    WLBDAT
074830991012     C                   Z-ADD     G02INV        D17DSP
074840991012     C                   ELSE
074850991012     C                   Z-ADD     ARBDSP        D17DSP
074860991012     C                   END
074870991012     C                   CALL      'FNLV16R'
074880030612     C                   PARM                    Fnlv16DS
074890991012     C                   PARM                    DTASV
074900991014     C                   MOVEL     D17DIV        VIDDIV
074910991012   X0C                   ELSE
074920030612     C                   CLEAR                   Fnlv16DS
074930991005     C                   CLEAR                   DTASV
074940941222     C                   MOVEL     D48TBO        D17TBO
074950991006     C   23              MOVEL     'S'           D17PRP
074960941209     C                   MOVEL     KSC           D17KSC
074970991012     C     VIDDFT        IFGT      *ZEROS
074980991012     C                   CLEAR                   WLBDAT
074990991012     C                   Z-ADD     VIDDFT        G02DAT
075000991012     C                   MOVEL     *BLANK        G02ERR
075010991012     C                   CALL      'XSRDA8'
075020991012     C                   PARM                    WLBDAT
075030991012     C                   Z-ADD     G02INV        D17DSP
075040991012     C                   ELSE
075050991012     C                   Z-ADD     ARBDSP        D17DSP
075060991012     C                   END
075070941209     C                   MOVEL     ARBLNP        D17LNP
075080941209     C                   MOVEL     ARBLNA        D17LNA
075090941209     C                   MOVEL     §3ATB1        D17TBL
075100941209     C                   MOVEL     §3ARBL        D17RBL
075110020503     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
075120100120     C                   MOVEL     §3ASVA        §D17sva
075130100120
075140100120     c* Se fattura immediata o prepagato
075150100120     c*                        controllo evetuale esenzione iva
075160100121     c                   if        *in14   or (not *in10 and vidnft>0)
075170100120     c                   Eval      §d17ctriso='S'
075180100120     c                   if        arbcpi<>*blanks
075190100120     C     'PRIVATO'     SCAN      arbcpi        posiz                    30
075200100120     c* solo numeri
075210100120     c                   select
075220100120     c                   when      not *in30 and %subst(arbcpi:1:2)>=*zeros
075230100120     c                   eval      §d17iso='IT'
075240100120     c* "PRIVATO" senza iso
075250100120     c                   when      *in30 and posiz<=3
075260100120     c                   eval      §d17iso='$$'
075270100120     c                   other
075280100120     c                   eval      §d17iso= %subst(arbcpi:1:2)
075290100120     c                   ENDSL
075300100120     c                   endif
075310100120     c                   endif
075320100120     c
075330100120     c                   Movel     Dlv1601       D17Flo
075340100120     c
075350941209     C                   Z-ADD     VIDIAS        D17IAS
075360941209     C                   Z-ADD     VIDQFT        D17QFT
075370941209     C                   Z-ADD     VIDPOR        D17POR
075380991005     C                   MOVEL     VIDDIV        D17DIV
075390991006     C                   MOVEL     WDIV          D17DVP
075400941209     C                   Z-ADD     VIDIMV        D17IMV
075410990702    1C     §3AFCA        IFEQ      1
075420941209     C                   MOVEL     'S'           D17FCA
075430990702    1C                   ENDIF
075440990702    1C* SE C'E' NUMERO E DATA FATTURA RIPASSO IL TOTALE IMPONIBILE
075450950114     C     *IN14         IFEQ      *ON
075460991005     C                   MOVEL     'E'           D17FIM
075470950114     C*
075480990702   X1C                   ELSE
075490950114     C*
075500950114     C                   MOVEL     'C'           D17FIM
075510990702    2C     VIDFIV        IFGT      0
075520950114     C     VIDNFT        ANDGT     0
075530950114     C     VIDDFT        ANDGT     0
075540991005     C                   MOVEL     'E'           D17FIM
075550990702    2C                   ENDIF
075560990702    1C                   ENDIF
075570941209     C                   MOVEL     VIDCEI        D17CEI
075580991029     C* SE PROTETTA LA TASSAZIONE, NON PASSO I DATI DELLA FATTURA
075590941209     C                   MOVEL     VIDFIV        D17FIV
075600941220     C                   MOVEL     'S'           D17FFI
075610941209     C                   MOVEL     VIDNFT        D17NFT
075620941209     C                   Z-ADD     VIDDFT        D17DFG
075630941209     C*
075640991012     C* RICHIAMO ROUTINE PER CARICAMENTO VARIE DELLA DTASV
075650991012     C                   EXSR      RDTASV
075660941209     C*
075670991005     C                   CALL      'FNLV16R'
075680030612     C                   PARM                    Fnlv16DS
075690991005     C                   PARM                    DTASV
075700941102     C*
075710991011     C                   MOVEL     D17DIV        VIDDIV
075720941209     C                   MOVEL     D17CEI        VIDCEI
075730991005     C                   MOVEL     O17DEI        DESCEI
075740991005     C                   MOVEL     O17DEI        V3DCEI
075750991006     C     D17FIM        IFEQ      'E'
075760991014     C     O17ERR        ANDEQ     *BLANKS
075770030620      * OPPURE SE BOLLA CON 1 SOLA VARIA VALORIZZO L'IMPONIBILE ANCHE SE SI TRATTA DI SEGUE FATTURA
075780030620      * SOLO SE LA VARIA ESISTE
075790040123     C**!!!§3ASVA        ORNE      *BLANKS
075800040123     C**!!!WVAR3A        ANDEQ     'S'
075810040123     C**!!!O17ERR        ANDEQ     *BLANKS
075820040123     C**!!!D17FIM        ANDEQ     'C'
075830991005     C                   MOVEL     O17IMV        VIDIMV
075840991006     C                   ENDIF
075850030620      * se bolla con singola varia e la varia non  c'è azzero l'imponibile per i segue fattura
075860030620     C     §3ASVA        IFNE      *BLANKS
075870030620     C     WVAR3A        ANDNE     'S'
075880030620     C     D17FIM        ANDEQ     'C'
075890030620     C                   Z-ADD     *ZEROS        VIDIMV
075900030620     C                   ENDIF
075910030620      ****
075920991029     C                   MOVEL     D17DFG        VIDDFT
075930941209     C* VARIE
075940990702    1C                   DO        20            NRR
075950941215     C     NRR           CHAIN     LR48S03                            30
075960941209     C                   MOVEL     VSV(NRR)      V3CSV1
075970941209     C                   MOVEL     VES(NRR)      V3CEV1
075980941215     C* SE C'E' ERRORE SU VARIA, ACCENDO INDICATORE DI POSIZIONAMENTO
075990991006    2C     O17ERV        IFEQ      NRR
076000941220     C                   SETON                                        5290
076010941215     C                   ELSE
076020941215     C                   SETOFF                                       52
076030990702    2C                   ENDIF
076040941215     C  N30              UPDATE    LR48S03
076050941215     C   30              WRITE     LR48S03
076060990702    1C                   ENDDO
076070991012    0C                   ENDIF
076080941215     C*
076090941215     C                   SETOFF                                       52
076100941102     C*  ERRORE
076110991006    1C     O17ERR        IFNE      *BLANKS
076120991006     C     O17FAS        OREQ      'S'
076130941102     C*
076140991006    2C     O17MSG        IFNE      *BLANKS
076150991006     C                   MOVEL     O17MSG        VIDMSG
076160941209     C                   SETON                                        28
076170941102    2C                   ENDIF
076180990415     C**
076190990415     C* VARIA R  MSG
076200991006    2C     O17ERR        IFEQ      '6'
076210991006     C     O17MSG        ANDEQ     MSG(85)
076220991006     C     O17FAS        OREQ      'S'
076230990416     C*
076240990415     C     COMASS        IFEQ      ' '
076250990415     C* SE MODIFICATO L'IMPORTO D'ASSICURARE E FATTURA GIA' INCASSATA O TASSATA
076260990415    3C     VIDIAS        IFNE      ARBIAS
076270990415     C     VIDVAS        ORNE      ARBVAS
076280990415     C*
076290990415     C* SE GIA' TASSATO FINO ALL'IMPONIBILE E LA DATA FATTURA > 0
076300990415    4C     AR6IMV        IFNE      *ZEROS
076310990415     C                   SETON                                        2882
076320990415     C                   SETOFF                                       90
076330990415     C* ERRORE NON BLOCCANTE:CORREGGERE  VARIA R
076340990419     C                   SELECT
076350990419    5C     ARBIAS        WHENEQ    0
076360990415     C     VIDIAS        ANDGT     0
076370990415     C                   MOVEL     MSG(83)       VIDMSG
076380990419    5C     ARBIAS        WHENGT    0
076390990419     C     VIDIAS        ANDEQ     0
076400990415     C                   MOVEL     MSG(84)       VIDMSG
076410990419     C                   OTHER
076420990419     C                   MOVEL     MSG(86)       VIDMSG
076430990419    5C                   ENDSL
076440990415    4C                   END
076450990415     C                   MOVEL     '1'           COMASS            1
076460990415    3C                   END
076470990415     C                   ELSE
076480990415     C                   SETOFF                                       9028
076490990415     C                   MOVEL     *BLANKS       VIDMSG
076500990415     C                   END
076510990415    2C                   END
076520990415     C* FATTURA
076530941102     C**
076540941102     C* ESENZIONE IVA
076550991006    2C     O17ERR        IFEQ      '1'
076560941209     C                   SETON                                        5190
076570941102    2C                   ENDIF
076580941102     C**
076590941102     C* IMPONIBILE
076600991006     C     O17ERR        IFEQ      '5'
076610941209     C                   SETON                                        5090
076620941102    2C                   ENDIF
076630941222     C* NUMERO FATTURA
076640991006     C     O17ERR        IFEQ      '2'
076650941222     C                   SETON                                        5590
076660941222    2C                   ENDIF
076670941222     C* FILIALE IVA FATTURA
076680991006     C     O17ERR        IFEQ      '3'
076690941222     C                   SETON                                        5490
076700941222    2C                   ENDIF
076710941222     C* DATA FATTURA
076720991006     C     O17ERR        IFEQ      '4'
076730941222     C                   SETON                                        5690
076740941222    2C                   ENDIF
076750991006     C* DIVISA
076760991006     C     O17ERR        IFEQ      '7'
076770991006     C                   SETON                                        4590
076780991006    2C                   ENDIF
076790991012     C* PORTO
076800991012     C     O17ERR        IFEQ      '8'
076810001116     C                   SETON                                        4990
076820991012    2C                   ENDIF
076830941102     C**
076840941220     C                   GOTO      ENDC03
076850941102    1C                   ENDIF
076860040123      * se tipo bolla 'monovaria' e la varia non è esposta o è negata
076870040123      * non posso avere altre varia con importo > 0
076880040123if  1c                   If        §3asva <> *Blanks
076890040123do  2c                   Do        20            nrr
076900040123     c     nrr           Chain     Lr48s03                            30
076910040123     c                   Eval      *In52 = *Off
076920040123if  3c                   If        wvar3a <> *Blanks  and wvar3ai = *Blanks and
076930040123     c                             V3cSv1 <> *Blanks and V3cVa1 > *Zeros
076940040123     c                   Eval      *In28 = *On
076950040123     c                   Eval      *In52 = *On
076960040123     c                   Eval      *In90 = *On
076970040123     c                   Eval      VidMsg = Msg(127)
076980040123     c                   Eval      %subst(VidMsg:75:1) = §3asva
076990040123    3c                   EndIf
077000040123if  3c                   If        wvar3a = *Blanks  and
077010040123     c                             V3cSv1 <> *Blanks and V3cVa1 > *Zeros
077020040123     c                   Eval      *In28 = *On
077030040123     c                   Eval      *In52 = *On
077040040123     c                   Eval      *In90 = *On
077050040123     c                   Eval      VidMsg = Msg(127)
077060040123     c                   Eval      %subst(VidMsg:75:1) = §3asva
077070040123    3c                   EndIf
077080040123     c  n30              Update    Lr48s03
077090040127     c   90              Goto      Endc03
077100040123    2c                   EndDo
077110040123    1c                   EndIf
077120991006     C*
077130991006     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
077140991006    1C  N07VIDDIV        IFNE      WDIV
077150991006     C* CONVERSIONE PORTO
077160991006    2C     VIDPOR        IFEQ      VARPOR
077170991006     C     VIDPOR        ANDGT     *ZEROS
077180991006     C     WDIV          ANDNE     *BLANKS
077190030612     C                   CLEAR                   YeurcoDS
077200991006     C                   MOVEL     WDIV          YECDVI
077210991006     C                   Z-ADD     VIDPOR        YECIMI
077220991006     C                   MOVEL     VIDDIV        YECDVO
077230991006     C     O17FDC        IFEQ      'S'
077240011114     C                   MOVE      '3'           YECDCO
077250991006     C                   ENDIF
077260991006     C                   CALL      'YEURCO'
077270030612     C                   PARM                    YeurcoDS
077280991006     C                   Z-ADD     YECIMO        VIDPOR
077290991012     C                   SETON                                        49
077300991006    2C                   END
077310991006     C* CONVERSIONE VARIE
077320991006    2C                   DO        20            NRR
077330991006     C     NRR           CHAIN     LR48S03                            30
077340991006    3C     V3CVA1        IFEQ      VARVA1
077350991006     C     V3CVA1        ANDGT     *ZEROS
077360991006     C     WDIV          ANDNE     *BLANKS
077370011019      *
077380011019      * Se la bolla non è tassata fino all'imponibile e c'è la varia 'N' 88888888
077390011019      * non devo convertire la varia 'N' 88888888
077400011019     C     VIDIMV        IFEQ      *ZEROS
077410011019     C     FLGVRN        ANDEQ     '1'
077420011019     C     V3CSV1        ANDEQ     'N'
077430011019     C     V3CVA1        ANDEQ     88888888
077440011019     C  N30              UPDATE    LR48S03
077450011019     C                   ITER
077460011019     C                   ENDIF
077470030624      * Se la bolla non è tassata fino all'imponibile e c'è la varia '&' 88888888
077480030624      * non devo convertire la varia '&' 88888888
077490030624     C     VIDIMV        IFEQ      *ZEROS
077500030624     C     V3CSV1        ANDEQ     '&'
077510030624     C     V3CVA1        ANDEQ     88888888
077520030624     C  N30              UPDATE    LR48S03
077530030624     C                   ITER
077540030624     C                   ENDIF
077550011019      *
077560030612     C                   CLEAR                   YeurcoDS
077570991006     C                   MOVEL     WDIV          YECDVI
077580991006     C                   Z-ADD     V3CVA1        YECIMI
077590991006     C                   MOVEL     VIDDIV        YECDVO
077600991006     C     O17FDC        IFEQ      'S'
077610011114     C                   MOVE      '3'           YECDCO
077620991006     C                   ENDIF
077630991006     C                   CALL      'YEURCO'
077640030612     C                   PARM                    YeurcoDS
077650991006     C                   Z-ADD     YECIMO        V3CVA1
077660991012     C                   SETON                                        53
077670991006    3C                   ENDIF
077680991006     C                   Z-ADD     V3CVA1        VARVA1
077690991006     C  N30              UPDATE    LR48S03
077700991006    2C                   ENDDO
077710991006     C* SE TOTALE IMPONIBILE E' IMPOSTATO LO AZZERO E RICHIAMO
077720991006     C* FNLV16R PER RICALCOLARLO IN BASE ALLA NUOVA DIVISA
077730991006     C     VIDIMV        IFGT      *ZEROS
077740991006     C     WDIV          ANDNE     *BLANKS
077750991006     C                   MOVEL     'T'           D17FIM
077760991006     C                   MOVEL     *ZEROS        D17IMV
077770991006     C                   Z-ADD     VIDPOR        D17POR
077780991012     C                   CLEAR                   DTASV
077790991012     C                   EXSR      RDTASV
077800991006     C                   CALL      'FNLV16R'
077810030612     C                   PARM                    Fnlv16DS
077820991006     C                   PARM                    DTASV
077830991006     C                   Z-ADD     O17IMV        VIDIMV
077840991006     C                   ENDIF
077850991006     C                   Z-ADD     VIDPOR        VARPOR
077860991006     C                   MOVEL     VIDDIV        WDIV
077870991012     C* SE VARIATA DIVISA RIEMETTO IN OGNI CASO LA VIDEATA
077880991029     C* PERCHE' L'LV16 DOVRA' CONTROLLARE LA TASSAZIONE
077890991012     C                   SETON                                        90
077900991012     C                   GOTO      ENDC03
077910991006   X1C                   ELSE
077920991006     C* SE NON VARIATA LA DIVISA DEVO COMUNQUE MEMORIZZARE EVENTUALI
077930991006     C* VARIAZIONI EFFETTUATE SUGLI IMPORTI DI TASSAZIONE
077940991006     C                   Z-ADD     VIDPOR        VARPOR
077950991006    2C                   DO        20            NRR
077960991006     C     NRR           CHAIN     LR48S03                            30
077970991006     C                   Z-ADD     V3CVA1        VARVA1
077980991006     C  N30              UPDATE    LR48S03
077990991006    2C                   ENDDO
078000991006    1C                   ENDIF
078010941102     C*
078020941102     C* PER COD 8888 O 9999 CON VARIA "R" OBBLIGATORIO IMPORTO DA ASSIC
078030941209     C* SE C'E' "S" IN D17FAS INDICA QUESTO TIPO DI ERRORE
078040991006    3C     O17FAS        IFEQ      'S'
078050941102     C*
078060941102     C* SE = 0 E FORZO CON ENTER PRENDO VALORE DA TARIFFA
078070990702    2C     SEI           IFNE      1
078080941209     C                   MOVEL     MSG(46)       VIDMSG
078090941209     C                   SETON                                        5290
078100941102     C                   ADD       1             SEI
078110941102     C                   GOTO      ENDC03
078120990702    2C                   ENDIF
078130990702    1C                   ENDIF
078140941222     C*
078150941222     C* IN PARTENZA SE C'ERA PREPAGATO, DEVO LASCIARLO
078160990702    1C     *IN10         IFEQ      *OFF
078170990702    2C     AR6NFT        IFNE      0
078180941222     C     VIDNFT        ANDEQ     0
078190941222     C                   SETON                                        559028
078200941222     C                   MOVEL     MSG(60)       VIDMSG
078210941222     C                   GOTO      ENDC03
078220941222    2C                   ENDIF
078230990702     C*
078240990702     C* SE PREPAGATO INSERIRE TASSAZIONE O F14 PER TASSARE
078250990702     C  NKL
078260990702    2CANNKN*IN23         IFEQ      *ON
078270990702    3C     VIDIMV        IFEQ      0
078280990702     C                   SETON                                        529028
078290990702     C                   MOVEL     MSG(89)       VIDMSG
078300990702     C                   GOTO      ENDC03
078310990702   X3C                   ELSE
078320990702    4C     VIDIMV        IFNE      SAVIMV
078330990702     C                   Z-ADD     AR6DFT        ARBDFT
078340990702     C                   EXSR      TOTFAT
078350990702     C                   Z-ADD     D04IFT        VIDIFT
078360990702     C                   Z-ADD     D04IMV        VIDIMV
078370990702     C                   Z-ADD     D04POR        VIDPOR
078380991006     C                   Z-ADD     D04POR        VARPOR
078390990702     C                   Z-ADD     VIDIMV        SAVIMV
078400990702     C                   SETON                                        90
078410990702    4C                   ENDIF
078420990702    3C                   ENDIF
078430990702    2C                   ENDIF
078440990702    1C                   ENDIF
078450011018      * Se è bolla di reso con varia 'N' 88888888 deve esserci la varia 'N'
078460011017     C                   SETOFF                                       52
078470011018    1C     FLGVRN        IFEQ      '1'
078480011017    2C                   DO        20            NRR
078490011017     C     NRR           CHAIN     LR48S03                            32
078500011017    3C     *IN32         IFEQ      *OFF
078510011018     C                   SETON                                        52
078520011018    4C     V3CSV1        IFEQ      'N'
078530011017     C                   SETOFF                                       52
078540011017     C                   LEAVE
078550011018    4C                   ENDIF
078560011017    3C                   ENDIF
078570011017    2C                   ENDDO
078580011018    2C     *IN52         IFEQ      '1'
078590011017     C     1             CHAIN     LR48S03                            32
078600011017     C  N32              UPDATE    LR48S03
078610011017     C                   MOVEL     MSG(101)      VIDMSG
078620011105     C                   SETON                                        9028
078630011017     C                   GOTO      ENDC03
078640011017    2C                   ENDIF
078650011017    1C                   ENDIF
078660011206      *
078670011206      * Controllo che la somma degli importi immessi non superi il valore
078680011206      * impostato in §GEIMF
078690011206      *      prima controllo il totale imponibile
078700011206  1b c                   if        vidimv <> 0
078710011206     c                   eval      w0173 = vidimv
078720011206      *      controllo se devo convertire il totale imponibile
078730011206  2b c                   if        viddiv <> §gedcn
078740030612     c                   clear                   YeurcoDS
078750011206     c                   eval      yecdvi = viddiv
078760011206     c                   eval      yecdvo = §gedcn
078770011206     c                   eval      yecimi = vidimv
078780011206     c                   move      decsav        yecdco
078790011206     c                   call      'YEURCO'
078800030612     c                   parm                    YeurcoDS
078810011206  3b c                   if        yecesi = ' '
078820011206     c                   eval      w0173 = yecimo
078830011206  3e c                   endif
078840011206  2e c                   endif
078850011206      *      controllo se l'imponibile supera il limite fatturabile
078860011220  2b c                   if        w0173 > imfsav
078870011206     c                   eval      vidmsg = msg(107)
078880011220     c                   eval      %subst (vidmsg:54:14) = (%editc(imfsav: '4'))
078890011206     c                   eval      %subst (vidmsg:69:3) = §gedcn
078900011206     c                   eval      *in50 = *on
078910011206     c                   eval      *in90 = *on
078920011206     c                   eval      *in28 = *on
078930011206     c                   goto      endc03
078940011206  2e c                   endif
078950011206  1x c                   else
078960011206      *      altrimenti controllo tutti gli altri importi
078970011206     c                   eval      tot_importi = vidpor
078980011206      *      leggo le varie
078990011206  2b c                   do        20            nrr
079000011206     c     nrr           chain     lr48s03                            32
079010011206  3b c                   if        *in32 = *off and v3cva1 <> 0
079020011206  4b c                   if        flgvrn = '1' and v3cva1 = 88888888
079030011206     c                             and v3csv1 = 'N'
079040011206     c                   iter
079050011206  4e c                   endif
079060030624  4b c                   if        v3cva1 = 88888888
079070030624     c                             and v3csv1 = '&'
079080030624     c                   iter
079090030624  4e c                   endif
079100011206     c                   eval      tot_importi = tot_importi + v3cva1
079110011206  3e c                   endif
079120011206  2e c                   enddo
079130011206     c                   eval      w0173 = tot_importi
079140011206      *      controllo se devo convertire il totale importi
079150011206  2b c                   if        viddiv <> §gedcn
079160030612     c                   clear                   YeurcoDS
079170011206     c                   eval      yecdvi = viddiv
079180011206     c                   eval      yecdvo = §gedcn
079190011206     c                   eval      yecimi = tot_importi
079200011206     c                   move      decsav        yecdco
079210011206     c                   call      'YEURCO'
079220030612     c                   parm                    YeurcoDS
079230011206  3b c                   if        yecesi = ' '
079240011206     c                   eval      w0173 = yecimo
079250011206  3e c                   endif
079260011206  2e c                   endif
079270011206      *      controllo se l'imponibile supera il limite fatturabile
079280011220  2b c                   if        w0173 > imfsav
079290131016     c                   eval      vidmsg = msg(107)
079300011220     c                   eval      %subst (vidmsg:54:14) = (%editc(imfsav: '4'))
079310011206     c                   eval      %subst (vidmsg:69:3) = §gedcn
079320011206     c                   eval      *in50 = *on
079330011206     c                   eval      *in90 = *on
079340011206     c                   eval      *in28 = *on
079350011206     c                   goto      endc03
079360011206  2e c                   endif
079370011206  1e c                   endif
079380131016     c
079390131016     c* CONTROLLI PER BOLLA CONTABILIZZATA E PGM NON RICHIAMATO (29 ON)
079400131016     c* se f6=conferma
079410131016     c                   if        *in29 and *inkf
079420131016     c* Se eseguito F14=Tassazione --> errore bolla contabilizzata
079430131016     c                   if        winf14='1'
079440131016     c                   eval      vidmsg = msg(151)
079450131016     c                   eval      *in90 = *on
079460131016     c                   eval      *in28 = *on
079470131016     c                   goto      endc03
079480131016     c                   endif
079490131016     c* Se non variato nè ias nè vmd -> errore premere F12
079500131016     c                   if        vidias=bol_vidias and
079510131016     c                             (vidvas=bol_vidvas or vidias=0) and
079520131016     c                             vidvmd=bol_vidvmd and
079530131016     c                             (vidvad=bol_vidvad or vidvmd=0)
079540131016     c                   eval      vidmsg = msg(152)
079550131016     c                   eval      *in90 = *on
079560131016     c                   eval      *in28 = *on
079570131016     c                   goto      endc03
079580131016     c                   endif
079590131016     c                   endif
079600911210     C*
079610941209     C     ENDC03        ENDSR
079620030710      *---------------------------------------------------------------*
079630030710      * Gestione videata password x importo da assicurare             *
079640030710      *---------------------------------------------------------------*
079650060201     c**   Sr_Gespsw     BegSr
079660060201     C**
079670060201     c**                 Clear                   w01psw
079680060201     c**                 Do        *Hival
079690060201     c**                 Exfmt     lr48w01
079700060201     c**                 Eval      *In28 = *Off
079710060201     c**                 If        *InKl
079720060201     c**                 Eval      *In90 = *On
079730060201     c**                 Leave
079740060201     c**                 EndIf
079750060201     c**                 If        w01psw = *Blanks
079760060201     c**                 Eval      w01msg = 'Immettere la password'
079770060201     c**                 Eval      *In28 = *On
079780060201     c**                 Iter
079790060201     c**                 EndIf
079800060201     c**                 If        w01psw <> miapsw
079810060201     c**                 Eval      w01msg = 'Password errata'
079820060201     c**                 Eval      *In28 = *On
079830060201     c**                 Iter
079840060201     c**                 EndIf
079850060201     c**                 Eval      wokpsw = '1'
079860060201     c**                 Leave
079870060201     c**                 EndDo
079880060201     C**
079890060201     c**                 Endsr
079900920319     C*
079910930505     C* CONTROLLO COMUNI  ALL CONTR3 3 CONTR 4 -----------------------*
079920920319     C     CTRCOM        BEGSR
079930920319     C                   SETOFF                                       90
079940030620     C                   CLEAR                   SAV_INDSIN
079950941103     C**
079960920319     C* CODICE CLIENTE
079970941103     C**
079980950104     C*               SALTO CONTROLLO SE SPEDIZIONE IN FRANCO  IN ARRIV
079990950104     C*               LO ESEGUO SE SONO IN PARTENZA
080000941103    1C     *IN36         IFEQ      *OFF
080010941222     C     *IN10         OREQ      *OFF
080020941103     C*
080030941103     C* F7=RICERCA ALFABETICA
080040941103    2C   KG              DO
080050941212     C                   MOVEL     VIDDSA        PARDUT           30
080060920319     C                   MOVEL     *BLANKS       DESCDS           48
080070920319     C                   MOVEL     DESKSC        DESCDS
080080920319     C* PARSTA=9 ESCLUDO ANNULLATI
080090920319     C                   Z-ADD     9             PARSTA
080100060131     C                   Z-ADD     dutkci        CCC               4 0
080110941103    3C     VIDKLP        IFEQ      0
080120000510     C   10              MOVEL     ARBLNA        PARKSM
080130941103     C                   ELSE
080140000510     C                   MOVEL     VIDKLP        PARKSM
080150941103    3C                   ENDIF
080160991112     C                   CLEAR                   PARDIT
080170000510     C                   Z-ADD     1             PARNUM
080180941103     C*
080190000510     C                   CALL      'XALFA3BR'
080200920319     C                   PARM                    PARDUT
080210920319     C                   PARM                    CODUT
080220920319     C                   PARM                    DESCDS
080230920319     C                   PARM                    CCC
080240920319     C                   PARM                    PARSTA            1 0
080250961016     C                   PARM                    PARFLR           90
080260991112     C                   PARM                    PARDIT            3
080270000510     C                   PARM                    PARNUM            2 0
080280000510     C                   PARM                    PARKCM           80
080290000510     C                   PARM                    PARKSM          140
080300000510     C                   PARM                    PARKDM           60
080310941103    3C     PARSTA        IFEQ      -1
080320920319     C*
080330941103    4C     FLGCVR        IFEQ      '2'
080340941212     C*
080350990930     C     WAR62         IFEQ      ' '
080360941212     C                   MOVEL     ARBLNA        VIDKLP
080370941212     C                   MOVEL     9999          VIDKSC
080380941212     C                   ELSE
080390991007     C                   MOVEL     AR6KSC        VIDKLP
080400991007     C                   MOVE      AR6KSC        VIDKSC
080410941212     C                   ENDIF
080420941212     C*
080430941103   X4C                   ELSE
080440920319     C                   MOVEL     ARBKSC        VIDKLP
080450920319     C                   MOVE      ARBKSC        VIDKSC
080460941103    4C                   END
080470920319     C*
080480941103   X3C                   ELSE
080490020305     C                   MOVEL     PARKSM        ACOKSC
080500920319     C                   MOVEL     ACOKSC        VIDKLP
080510920319     C                   MOVE      ACOKSC        VIDKSC
080520970814     C                   CLEAR                   VIDRSM
080530970814     C                   CLEAR                   VIDINM
080540970814     C                   CLEAR                   VIDCAM
080550970814     C                   CLEAR                   VIDLOM
080560970814     C                   CLEAR                   VIDPRM
080570970814     C                   CLEAR                   VIDNZM
080580970814     C                   CLEAR                   VIDPIM
080590941103    3C                   END
080600941103     C*
080610941103     C                   SETON                                        9047
080620941103     C                   GOTO      ENDCOM
080630941212    2C                   ENDDO
080640920319     C* CONTROLLO
080650941103    2C     VIDKLP        IFEQ      0
080660941103     C                   MOVEL     MSG(33)       VIDMSG
080670941103     C                   SETON                                        469028
080680941103     C                   GOTO      ENDCOM
080690941103    2C                   END
080700980722     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE DELLO STESSO S.I. IN
080710991112     C*  CUI SONO  PER LE FATTURE IMMEDIATE
080720040930     C     VIDKLP        CHAIN     AZORG01L                           30
080730040930     c*
080740020725     C     D66BFI        IFEQ      'S'
080750020725     C**N30ORGDIT        IFNE      O50PRA
080760020725     C**                 MOVEL     MSG(81)       VIDMSG
080770020725     C**                 SETON                                        469028
080780020725     C**                 GOTO      ENDCOM
080790020725     C**                 ENDIF
080800020508     c* sempre per le fatture immediate se cliente 9999 il p.o. cliente
080810020508     c* deve essere = alla linea di arrivo della bolla
080820020508     c     vidksc        ifeq      9999
080830020508     c     vidklp        andne     arblna
080840020508     C                   MOVEL     MSG(35)       VIDMSG
080850020508     C                   SETON                                        479028
080860020508     C                   GOTO      ENDCOM
080870020508     c                   endif
080880020306     C                   ENDIF
080890020306     c* se variazione di mittente e cliente 8888 il p.o. cliente deve
080900020306     c* essere = alla linea di partenza bolla
080910020306     c     flgcvr        ifeq      'M'
080920020306     c     vidksc        andeq     8888
080930020306     c     vidklp        andne     arblnp
080940020306     C                   MOVEL     MSG(35)       VIDMSG
080950020306     C                   SETON                                        479028
080960020306     C                   GOTO      ENDCOM
080970020306     c                   endif
080980020508     c* se variazione di bolla in partenza e cliente 9999 errore
080990020508     c     *in10         ifeq      *off
081000020508     c     vidksc        andeq     9999
081010020508     C                   MOVEL     MSG(111)      VIDMSG
081020020508     C                   SETON                                        479028
081030020508     C                   GOTO      ENDCOM
081040020508     c                   endif
081050930507     C*
081060920319     C                   MOVEL     VIDKLP        KSC
081070920319     C                   MOVE      VIDKSC        KSC
081080000510     C  N30              MOVEL     ORGDE3        OG143
081090020318     C  N30              MOVEL     §OGNTW        CLINTW
081100020318     C   30              CLEAR                   CLINTW
081110040930     c* non accetto clienti con ntw = "LOG" o "XXX'
081120040930     c                   if        clintw='LOG'  or clintw='XXX'
081130040930     C                   MOVEL     MSG(130)      VIDMSG
081140040930     C                   SETON                                        469028
081150040930     C                   GOTO      ENDCOM
081160040930     c                   endif
081170051220     c* non è possibile modificare cliente da dpd a non dpd e viceversa
081180051220     c     flgcvr        ifeq      'M'
081190051220     c                   if        (clintw = 'DPD' and clintw <> memntw) or
081200051220     c                             (memntw = 'DPD' and clintw <> memntw)
081210051220     C                   MOVEL     MSG(137)      VIDMSG
081220051220     C                   SETON                                        469028
081230051220     C                   GOTO      ENDCOM
081240051220     c                   endif
081250051220     c                   endif
081260941103     C**
081270920319     C*
081280941103     C     KACO          CHAIN     CNACO000                           30
081290941103    3C  N30ACOFLG        IFNE      ' '
081300920319     C                   SETON                                        30
081310941103    3C                   END
081320941103     C* ERRORE
081330941103    3C     *IN30         IFEQ      *ON
081340941103     C     VIDKSC        OREQ      8888
081350941222     C     *IN10         ANDEQ     *ON
081360941223     C     VIDKSC        OREQ      9999
081370941223     C     *IN36         ANDEQ     *ON
081380941103     C                   MOVEL     MSG(36)       VIDMSG
081390941103     C                   SETON                                        479028
081400941103     C                   GOTO      ENDCOM
081410941103    3C                   ENDIF
081420930507     C*
081430130315    3C**   ACOABL        IFEQ      '8'
081440130315    3C**   ACOABL        OREQ      '*'
081450130315     c
081460130315    3C     ACOABL        ifne      *blanks
081470130418     c     ksc           andne     kscbolla
081480040930     C                   MOVEL     mSG(37)       VIDMSG
081490941103     C                   SETON                                        479028
081500941103     C                   GOTO      ENDCOM
081510941103    3C                   END
081520930507     C*
081530920319     C* SE SEGUE FATTURA NON SI PUO' METTERE COD 9999
081540941103    3C   13VIDKSC        IFEQ      9999
081550941103     C                   MOVEL     MSG(38)       VIDMSG
081560941103     C                   SETON                                        479028
081570941103     C                   GOTO      ENDCOM
081580941103    3C                   END
081590930507     C*
081600970822     C     VIDKSC        IFNE      9999
081610970822     C     VIDKSC        ANDNE     8888
081620970822     C                   MOVEL     ACORAG        DESKSC
081630941103    2C                   END
081640941220     C*
081650941223     C* SALVO ULTIMO CODICE IMPOSTATO
081660941220    2C     COMKSC        IFNE      KSC
081670941220     C                   CLEAR                   CIN
081680941220     C                   CLEAR                   DIE
081690941220     C                   MOVEL     KSC           COMKSC            7 0
081700941220    2C                   END
081710941103    1C                   END
081720030620     C* CERCO IN CNIND IL DIRITTO DI FATTURAZIONE
081730030620     C*
081740030620     C     KACO          CHAIN     CNIND00F                           30
081750030620    3C                   IF        %FOUND(CNIND00F)
081760030620     C                   EVAL      SAV_INDSIN = INDSIN
081770030620     C                   ELSE
081780030620     C                   CLEAR                   SAV_INDSIN
081790030620     C                   ENDIF
081800941103     C**
081810941215     C* CARICO CODICI TARIFFA DEL CLIENTE ED ESEGUO CONTROLLO
081820941103     C**
081830930507     C                   EXSR      CARCTR
081840941209     C*
081850941209     C* SE HO RICERCATO LA TARIFFA, RIEMETTO LA VIDEATA
081860050202    1C     ilv59RIC      IFEQ      'S'
081870941223     C                   SETON                                        9048
081880941209     C                   GOTO      ENDCOM
081890941209    1C                   ENDIF
081900941209     C*
081910941215     C* ATTIVATO IL "?" SUL CODICE TARIFFA
081920050202    1C     olv59ERR      IFEQ      'R'
081930050202     C                   MOVEL     olv59CTR      VIDCTR
081940050202     C                   MOVEL     olv59DFS      DE2CTR
081950941223     C                   SETON                                        9048
081960941209     C                   GOTO      ENDCOM
081970941209    1C                   ENDIF
081980941209     C* ERRORE
081990050202    1C     olv59MSG      IFNE      *BLANKS
082000060606     c* Nella videata della variazione del cod mittente, non do errore
082010060606     c*  se premuto F10 per la modifica del codice tariffa
082020060606     c*  ma disabilito F12
082030060606     c                   if        *inkj and flgcvr='M'
082040060606     c                   clear                   w48f12
082050060606     c                   else
082060050202     C                   MOVEL     olv59MSG      VIDMSG
082070941223     C                   SETON                                        904828
082080941209     C                   GOTO      ENDCOM
082090941209    1C                   ENDIF
082100060606    1C                   ENDIF
082110930507     C*
082120941209     C                   MOVEL     VIDCTR        COMCTR
082130050202     C                   MOVEL     olv59PRG      COMPRG
082140021121
082150160225      * Per bolla FedEx
082160160225If  1c                   If        lnantW = 'FED'
082170160225     c
082180021122      * --> se cliente 8888
082190160225      *     il CTR a video deve essere uno delle tariffe fedex
082200021122If  2c                   If        VidKsc = 8888
082210021122      * ---> e O27Fie = 'M'
082220021122If  3c                   If        WFie =  'M'
082230021122      * la tariffa deve essere quella della cartello
082240160225If  4c                   If        VidCtr <> FedM_ctr
082250021122     c                   Eval      *In48 = *On
082260021122     c                   Eval      *In28 = *On
082270021122     c                   Eval      *In90 = *On
082280021122     c                   Movel     Msg(114)      VidMsg
082290021122     c                   Goto      EndCom
082300021122    4c                   EndIf
082310021122      * ---> e O27Fie non è = 'M'
082320021122   x3c                   Else
082330160225    4c                   if        Vidctr <> FedD_ctr
082340021121     c                   Eval      *In48 = *On
082350021121     c                   Eval      *In90 = *On
082360021121     c                   Eval      *In28 = *On
082370021121     c                   Movel     Msg(114)      VidMsg
082380021121     c                   Goto      EndCom
082390021122    4c                   EndIf
082400021122    3c                   EndIf
082410160225     c
082420021121      * --> se cliente codificato
082430021121   x2c                   Else
082440021121      * ---> e non sono state trovate tariffe
082450021121If  3c                   If        Wtks <> 'S'
082460021121      *      non posso accettare una tariffa documenti se il peso della
082470021121      *      bolla superare il limite previsto in tabella "DFE"
082480021121     c                   Move      Vidctr        DueCtr
082490021121If  4c                   If        ArbPkf > §DfePkgD and
082500021121     c                             DueCtr >= §DfeDalD and DueCtr <= §DfeAlD
082510021121     c                   Eval      *In48 = *On
082520021121     c                   Eval      *In90 = *On
082530021121     c                   Eval      *In28 = *On
082540021121      * Preparo il msg
082550021121     c                   Movea     Msg(115)      K78
082560021121     c                   Movel     §DfePkgD      W006a
082570021121     c                   Movea     W006a         Ski(10)
082580021121     c                   Z-Add     10            i
082590021121     c                   ExSr      Abbl
082600021121     c                   Movea     Ski(10)       K78(59)
082610021121     c                   Move      §DfePkgD      W001a
082620021121     c                   Movel     W001a         K78(66)
082630021121     c                   Movea     K78           VidMsg
082640021121     c                   Goto      EndCom
082650021121    4c                   EndIf
082660021121    3c                   EndIf
082670021121    2c                   EndIf
082680021121    1c                   EndIf
082690920319     C*
082700941103     C     ENDCOM        ENDSR
082710011017     C*
082720011017      *--- TASSAZIONE VARIA 'N' --------------------------------------*
082730011017     C     TASSAN        BEGSR
082740011017      *
082750011017     C                   CLEAR                   DTAS
082760011017     C                   CLEAR                   DTASV
082770011017     C                   MOVEL     VIDKLP        TASKSC
082780011017     C                   MOVE      VIDKSC        TASKSC
082790011017     C                   MOVEL     VIDCTR        TASCTR
082800011017     C                   Z-ADD     ARBPKF        TASPKF
082810160530     C                   Z-ADD     ARBPKb        TASPKb
082820011017     C                   MOVEL     ARBLNP        TASLNP
082830011017     C                   MOVEL     ARBCTS        TASCTS
082840011017     C                   MOVEL     ARBDSP        TASDSP
082850011017     C                   MOVEL     ARBDSP        TASDCT
082860011017     C                   MOVEL     ARBVLF        TASVLF
082870160530     C                   MOVEL     ARBVLc        TASVLc
082880160530     C                   MOVEL     ARBncr        TASncr
082890160530     C                   MOVEL     ARBVLF        TASVLF
082900160530     C                   MOVEL     ARBfvf        TASfvF
082910011017     C                   MOVEL     ARBTSP        TASTSP
082920011017     C     WBOL          IFEQ      '1'
082930011017     C                   MOVEL     §3ATB1        TASTBL
082940011017     C                   ELSE
082950011017     C                   MOVEL     §3ATB2        TASTBL
082960011017     C                   ENDIF
082970011017     C                   MOVEL     ARBLNA        TASLNA
082980011017     C                   MOVEL     ARBNCL        TASNCL
082990160530     C                   MOVEL     ARBNCL        TASNCLb
083000011017     C                   MOVEL     ARBQFT        TASQFT
083010011017     C     WBOL          IFEQ      '1'
083020011017     C     §3AFCA        ANDEQ     1
083030011017     C     WBOL          OREQ      '2'
083040011017     C     §3AFCA        ANDEQ     2
083050011017     C                   MOVEL     AR9CAS        TASCAS
083060011017     C                   MOVEL     AR9VCA        TASVCA
083070011017     C                   MOVEL     AR9TIC        TASTIC
083080011017     C                   ENDIF
083090011017     C                   MOVEL     ARBIAS        TASIAS
083100011017     C                   MOVEL     ARBVAS        TASVAS
083110011017     C                   MOVEL     ARBCAD        TASCAD
083120011017     C                   MOVEL     ARBNZD        TASNZD
083130011017     C                   MOVEL     ARBFIN        TASFIN
083140011017     C                   MOVEL     ARBCAM        TASCAM
083150011017     C                   MOVEL     ARBNZM        TASNZM
083160011017     C                   MOVEL     ARBFAP        TASFAP
083170011017     C                   MOVEL     ARBTC1        TASTC1
083180011017     C                   MOVEL     ARBTC2        TASTC2
083190011017     C     V1CDDT        IFEQ      'C'                                          SI DDT
083200011017     C     V1CDDT        OREQ      'S'
083210031028     c     v1cddt        oreq      'P'
083220031028     c     v1cddt        oreq      'Q'
083230011017     C                   MOVEL     'S'           TASDDT
083240011017     C                   ENDIF
083250060925     c*****              movel     arbfbr        tasflo
083260060925     c                   clear                   dtas01
083270060925     c                   movel     arbfbr        §asfbr
083280060925     c                   movel     arbcca        §ascca
083290060925     c                   eval      tasflo = dtas01
083300011017      **
083310011017      * SE BOLLA EXPORT--> NON PASSO MAI IL FLAG CONSEGNA A MAGAZZINO
083320011017      *                    IN MODO CHE TASSI SEMPRE LA VARIA 'U'
083330020305     C     LNANTW        ifeq      'COR'
083340020305     c     LNANTW        oreq      'MES'
083350020305     c     LNANTW        oreq      'PPT'
083360011017     C                   MOVEL     ARBFDN        TASFDN
083370011017     C                   ENDIF
083380011017      **
083390011017      * CHAIN SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
083400030612     C                   CLEAR                   Tisi95DS
083410011017     C                   MOVEL     ARBLOM        I95LOC
083420011017     C                   MOVEL     ARBCAM        I95CAP
083430011017     C                   MOVEL     ARBNZM        I95NAR
083440011017     C                   MOVEL     '3'           I95TCN
083450011017     C                   MOVEL     ARBDSP        I95DAT
083460011017     C     §3AFCA        IFNE      0
083470011017     C                   MOVEL     'S'           I95FCA
083480011017     C                   ENDIF
083490011017     C                   MOVEL     §3ATB1        I95TPO
083500020305     c     lnpntw        ifeq      'FED'
083510020305     c                   movel     'S'           i95fi1
083520020305     c                   endif
083530011017     C                   CALL      'TISI95R'
083540030612     C                   PARM                    Tisi95DS
083550011017     C     O95ERR        IFEQ      *BLANKS
083560011017     C                   MOVEL     O95CTS        TASMCT
083570011017     C                   ENDIF
083580011017     C                   MOVEL     'S'           WCAL13            1
083590011017      * ELABORO E CHIUDO CON LR IL PGM
083600011017     C                   MOVEL     ' '           TASTLA
083610020930     c* imposto peso vdl e colli rilevati
083620030203     c**!!!              clear                   dtas01
083630030203     c**!!!              z-add     arbncp        §asncp
083640030203     c**!!!              z-add     arbpkc        §aspkc
083650030203     c**!!!              movel     dtas01        tasflo
083660030203     c                   z-add     arbncp        Tasncp
083670030203     c                   z-add     arbpkc        Taspkc
083680030204      * Bancali
083690030205     c                   If        %Subst(ArbGva:1:1) = 'B'
083700030729     c                   Eval      TasBan = V1cBan
083710030205     c                   EndIf
083720030729      * Colli Originali
083730030729     c                   If        %Subst(ArbGva:1:1) = 'O'
083740030729     c                   Eval      TasNcl = V1cCor
083750030729     c                   EndIf
083760050804     c
083770050804     c                   eval      parca  = ' '
083780050804     c                   movel     paramt        kpjbu
083790011017      *
083800011017     C                   CALL      'TNSF20R'
083810011017     C                   PARM                    KPJBA
083820011017     C                   PARM                    DTAS
083830011017     C                   PARM                    DTASV
083840011018      *
083850011018      * Tassazione andata buon fine
083860011018     C     TASERR        IFEQ      *BLANKS
083870011018      *
083880011018      * Controllo la divisa della tassazione con la divisa del video
083890011018      * se non corrispondono devo convertire la varia 'N' nella divisa
083900011018      * del video
083910011018     C     TASDIV        IFNE      VIDDIV
083920011018      * aggancio la tabella CV per sicurezza
083930011018     C                   CLEAR                   DSCV
083940011018     C                   MOVEL     'CV'          COD
083950011018     C                   MOVEL(P)  VIDDIV        KEY
083960011018     C                   EXSR      CTABEL
083970011018     C  N30              MOVEL     TBLUNI        DSCV
083980011018      *
083990030612     C                   CLEAR                   YeurcoDS
084000011018     C                   MOVEL     TASDIV        YECDVI
084010011018     C                   MOVEL     VIDDIV        YECDVO
084020011018     C                   Z-ADD     TASIMV        YECIMI
084030011018     C     §CVFDC        IFEQ      'S'
084040011018     C                   MOVE      '3'           YECDCO
084050011018     C                   ELSE
084060011018     C                   MOVE      '0'           YECDCO
084070011018     C                   ENDIF
084080011018     C                   CALL      'YEURCO'
084090030612     C                   PARM                    YeurcoDS
084100011018     C     YECESI        IFEQ      ' '
084110011018     C                   Z-ADD     YECIMO        TASIMV
084120011018     C                   ELSE
084130011018     C                   Z-ADD     88888888      TASIMV
084140011018     C                   ENDIF
084150011018     C                   ENDIF
084160011017      *
084170011017     C                   Z-ADD     TASIMV        V3CVA1
084180011017     C                   Z-ADD     TASIMV        VARVA1
084190011017     C                   ENDIF
084200011017      *
084210011017     C                   ENDSR
084220910429     C*
084230050531begsrC*--- TASSAZIONE BOLLA ------------------------------------------*
084240910429     C     TASSAZ        BEGSR
084250991005     C                   CLEAR                   DTAS
084260991005     C                   CLEAR                   DTASV
084270020503     C                   CLEAR                   wtassa            1
084280030620     C                   CLEAR                   wtassad           1
084290030620     C                   CLEAR                   itassa           11 3
084300050601     c*
084310050601     c* richiamo routine di impostazione campi "comuni" da passare a
084320050601     c* pgm tnsf20r
084330050601     c                   exsr      SR_rtassa
084340910429     C*
084350941221     C* CARICO LE VARIE
084360941221     C*
084370980513     C     WBOL          IFEQ      '1'
084380030709     c                   eval      flgsf20 = 'S'
084390991012     C                   EXSR      RDTASV
084400030709     c                   eval      flgsf20 = ' '
084410980513     C*
084420980513     C                   ELSE
084430040123      * carico varie seconda bolla
084440040123     C                   MOVEL     'G'           TASSVA
084450040123     c                   Eval      TascVag = 'S'
084460041202     C**!!!VI2VA1        IFGT      0
084470040123     C                   MOVEL     VI2SV1        VSV(1)
084480040123     C                   Z-ADD     VI2VA1        VVA(1)
084490040123     c                   Movel     Vi2Es1        ves(1)
084500041202     C**!!!              ENDIF
084510041202     c**!!!              If        Vi2Va2 > *Zeros
084520040123     c                   Movel     Vi2Sv2        Vsv(2)
084530040123     c                   Z-add     Vi2Va2        Vva(2)
084540040123     c                   Movel     Vi2Es2        ves(2)
084550041202     c**!!!              EndIf
084560041202     c**!!!              If        Vi2Va3 > *Zeros
084570040123     c                   Movel     Vi2Sv3        Vsv(3)
084580040123     c                   Z-add     Vi2Va3        Vva(3)
084590040123     c                   Movel     Vi2Es3        ves(3)
084600041202     c**!!!              EndIf
084610041202     c**!!!              If        Vi2Va4 > *Zeros
084620040123     c                   Movel     Vi2Sv4        Vsv(4)
084630040123     c                   Z-add     Vi2Va4        Vva(4)
084640040123     c                   Movel     Vi2Es4        ves(4)
084650041202     c**!!               EndIf
084660041202     c**!!!              If        Vi2Va5 > *Zeros
084670041130     c                   Movel     Vi2Sv5        Vsv(5)
084680041130     c                   Z-add     Vi2Va5        Vva(5)
084690041130     c                   Movel     Vi2Es5        ves(5)
084700041202     c**!!!              EndIf
084710041202     c**!!!              If        Vi2Va6 > *Zeros
084720041130     c                   Movel     Vi2Sv6        Vsv(6)
084730041130     c                   Z-add     Vi2Va6        Vva(6)
084740041130     c                   Movel     Vi2Es6        ves(6)
084750041202     c**!!!              EndIf
084760040126     c                   xfoot     vva           TotImv
084770040126     c                   Z-add     TotImv        TasImv
084780980608     C                   ENDIF
084790011211
084800011211      * Data fattura se c'è
084810030203     c**!!!              clear                   dtas01
084820020930     c**                 if        ar6dft <> 0
084830020930     c**                 movel     ar6dft        §asdat
084840020930     c**                 endif
084850020503     c**
084860020503     c                   if        wtassa=' '
084870020503     c* se devo tassare solo una varia, da tipo bolla, la imposto
084880020503     c                   if        §3asva<>*blanks
084890020503     c                   eval      tassva=§3asva
084900020503     c                   endif
084910030620     c                   endif
084920030925     c* se diritto di fatturazione uguale a zero non calcolo la varia D
084930030925     c                   if        sav_indsin = 0
084940030925     c                   eval      wtassad = 'N'
084950030925     c                   endif
084960030623      * se tassazione fattura immediata o ritassazione prepagato
084970030623      * passo al TNSF20R la varia "D" e l'importo del diritto
084980030620      * di fatturazione da sommare che prendo da cnind (INDSIN)
084990030623     c                   if        (*in14 and not *in29  and wtassad = ' ')  or
085000030623     c                             (not *in10 and vidnft > 0 and not *in29 and
085010030623     c                             wtassad = ' ')
085020030925      *
085030030620     c                   move      sav_indsin    tasivdf
085040030620     c                   eval      tassvdf = 'D'
085050030620      * se bolla già tassat devo sommare la sola varia "D"
085060030620      * imponibile > 0 oppure imponibile a zero ma singola varia vaolorizzata x bolle speciali
085070040123     c**!!!              if        tasimv > 0 or (tasimv = 0 and itassa > 0)
085080040123     c                   if        tasimv > 0
085090030620     c                   eval      tassva = 'D'
085100030620     c                   endif
085110030620      * se singola varia per bolla speciale non è da calcolare ed è uguale a zero non calcolo la
085120030620      * varia D
085130030620     c                   if        wtassa = 'N' and tasimv = 0 and itassa = 0
085140030620     c                   eval      wtassad = 'N'
085150030620     c                   endif
085160030620
085170030620     c                   endif
085180011211
085190030620      * se non è tassazione fattura immediata non devo calcolare la varia D
085200030623      * o ritassazione prepagato
085210030623     c*                  if        not *in14
085220030623     c                   if        not *in14  or  *in29 or
085230030623     c                             (not *in10 and vidnft = 0)
085240030620     c                   eval      wtassad = 'N'
085250030620     c                   endif
085260030620      * se non devo calcolare la singola varia e neppure la varia D salto la tassazione
085270030620     c                   if        wtassa = 'N' and wtassad = 'N'
085280030620     c                   goto      endtas
085290030620     c                   endif
085300151127     c* Packing list e orm telefonici solo per la prima bolla in quanto la seconda
085310151127     c* è solo per tassare una specifica varia
085320151126     C     WBOL          IFEQ      '1'
085330151126     c* PACKING LIST
085340151126     c                   if        par5als='S' or par5alx='S'
085350151126     c                   eval      tasspl='S'
085360151126     c                   endif
085370151126     c* ADDEBITO ORM TELEFONICI (diritto di chiamata)
085380151126     C                   MOVEL     'M'           WTRC
085390151126     C     KARB2         CHAIN(N)  FiAR401l
085400151126     c                   if        %found(fiar401l)
085410151126     c                   movel     ar4not        dsbl4m
085420151126     c                   if        §b4ado='S'
085430151126     c                   eval      tasprt='S'
085440151126     c                   endif
085450151126     c                   endif
085460151126
085470151126     c                   endif
085480050804     c
085490050804     c                   eval      parca  = ' '
085500050804     c                   movel     paramt        kpjbu
085510910429     C*
085520941209     C                   CALL      'TNSF20R'
085530910429     C                   PARM                    KPJBA
085540991005     C                   PARM                    DTAS
085550991005     C                   PARM                    DTASV
085560910429     C*
085570941212    1C     TASERR        IFNE      *BLANKS
085580980608     C     TASMSG        IFNE      *BLANKS
085590980608     C                   MOVEL(P)  TASMSG        VIDMSG
085600980608     C                   ELSE
085610941212     C                   MOVEL     MSG(55)       VIDMSG
085620980608     C                   ENDIF
085630941209     C                   SETON                                        499028
085640991006     C* SE ERRORI IN TASSAZIONE SPROTEGGO LA DIVISA
085650991006     C                   SETOFF                                       07
085660941212   X1C                   ELSE
085670980513     C     WBOL          IFEQ      '1'
085680950421     C                   MOVEL     TASQFT        VIDQFT
085690950424     C                   MOVEL     TASQFT        VARQFT
085700910429     C                   MOVEL     TASPOR        VIDPOR
085710991006     C                   MOVEL     TASPOR        VARPOR
085720991025     C* SE DIVISA DIVERSA DA PRECEDENTE E PREMUTO F6 ACCENDO IL 90
085730991025     C* PER RIEMETTERE LA VIDEATA IN MODO DA VISUALIZZARE LA NUOVA
085740991025     C* DIVISA
085750991025     C     VIDDIV        IFNE      TASDIV
085760991025     C     *INKF         ANDEQ     *ON
085770991025     C                   SETON                                        90
085780991025     C                   ENDIF
085790991025     C*
085800991005     C                   MOVEL     TASDIV        VIDDIV
085810991006     C                   MOVEL     TASDIV        WDIV
085820991005     C                   Z-ADD     TASIMV        VIDIMV
085830941209     C* CARICO LE VARIE
085840941212    2C                   DO        20            NRR
085850941209     C     NRR           CHAIN     LR48S03                            30
085860941209     C                   MOVEL     VSV(NRR)      V3CSV1
085870941209     C                   Z-ADD     VVA(NRR)      V3CVA1
085880991006     C                   Z-ADD     VVA(NRR)      VARVA1
085890941209     C                   MOVEL     VES(NRR)      V3CEV1
085900941209     C   30              WRITE     LR48S03
085910941209     C  N30              UPDATE    LR48S03
085920941212    2C                   ENDDO
085930941209     C*
085940980513    XC                   ELSE
085950040123     c                   Clear                   ContaSv
085960040123      * carico le varie seconda bolla
085970040123Do  2c                   Do        20            c
085980040123If  3c                   If        Vsv(c) <> §$2Iva and
085990040123     c                             Vsv(c) <> §$2Bol
086000040123     c                   Add       1             ContaSv
086010040123S   4c                   Select
086020040123     c                   When      ContaSv = 1
086030040123     c                   Movel     Vsv(c)        Vi2Sv1
086040040123     c                   Z-add     Vva(c)        Vi2Va1
086050040123     c                   Movel     Ves(c)        Vi2Es1
086060040123     c                   When      ContaSv = 2
086070040123     c                   Movel     Vsv(c)        Vi2Sv2
086080040123     c                   Z-add     Vva(c)        Vi2Va2
086090040123     c                   Movel     Ves(c)        Vi2Es2
086100040123     c                   When      ContaSv = 3
086110040123     c                   Movel     Vsv(c)        Vi2Sv3
086120040123     c                   Z-add     Vva(c)        Vi2Va3
086130040123     c                   Movel     Ves(c)        Vi2Es3
086140040123     c                   When      ContaSv = 4
086150040123     c                   Movel     Vsv(c)        Vi2Sv4
086160040123     c                   Z-add     Vva(c)        Vi2Va4
086170040123     c                   Movel     Ves(c)        Vi2Es4
086180041130     c                   When      ContaSv = 5
086190041130     c                   Movel     Vsv(c)        Vi2Sv5
086200041130     c                   Z-add     Vva(c)        Vi2Va5
086210041130     c                   Movel     Ves(c)        Vi2Es5
086220041130     c                   When      ContaSv = 6
086230041130     c                   Movel     Vsv(c)        Vi2Sv6
086240041130     c                   Z-add     Vva(c)        Vi2Va6
086250041130     c                   Movel     Ves(c)        Vi2Es6
086260040123    4c                   EndSl
086270040123    3c                   EndIf
086280040123    2c                   EndDo
086290040123     C**!!!              Z-ADD     1             C
086300040123     C**!!!TASSVA        LOOKUP    VSV(C)                                 33
086310040123     C*!!33              Z-ADD     VVA(C)        VI2VA1
086320040123     C*!!33              Z-ADD     VVA(C)        VA2VA1
086330040123     C*!!33              MOVEL     VSV(C)        VI2SV1
086340991025     C* SE DIVISA DIVERSA DA PRECEDENTE E PREMUTO F6 ACCENDO IL 90
086350991025     C* PER RIEMETTERE LA VIDEATA IN MODO DA VISUALIZZARE LA NUOVA
086360991025     C* DIVISA
086370040123     C*!!33VI2DIV        IFNE      TASDIV
086380040123     C     VI2DIV        IFNE      TASDIV
086390991025     C     *INKF         ANDEQ     *ON
086400991025     C                   SETON                                        90
086410991025     C                   ENDIF
086420040123     C*!!33              MOVEL     TASDIV        VI2DIV
086430040123     C*!!33              MOVEL     TASDIV        WDIV
086440040123     C                   MOVEL     TASDIV        VI2DIV
086450040123     C                   MOVEL     TASDIV        WDIV
086460040123     C*!!33VES(C)        IFNE      ' '
086470040123     C**!!!VI2CEI        ANDEQ     ' '
086480040123     C**!!!              MOVEL     VES(C)        VI2CEI
086490040123     C**!!!              ENDIF
086500980513     C                   ENDIF
086510020503     C                   ENDIF
086520980513     C*
086530030620    1C*****              ENDIF
086540941209     C*
086550911211     C     ENDTAS        ENDSR
086560050601     C*
086570050601     C*--- Riempimento campi comuni di passaggio a tnsf20r -----------*
086580050601     C     sr_rtassa     BEGSR
086590050601     C* CODIFICATO
086600050601    1C     VIDKSC        IFNE      9999
086610050601    1C     VIDKSC        ANDNE     8888
086620050601     C                   MOVEL     VIDKLP        TASKSC
086630050601     C                   MOVE      VIDKSC        TASKSC
086640050601     C                   MOVEL     VIDCTR        TASCTR
086650050601   X1C                   ELSE
086660050601     C*
086670050601     C* E HO LA DATA FATTURA USO QUELLA, ALTRIMENTI DATEU
086680050601     C     AR6DFT        IFGT      0
086690050601     C                   Z-ADD     AR6DFT        WDATA
086700050601     C                   ELSE
086710050601     C                   Z-ADD     DATEU         WDATA
086720050601     C                   ENDIF
086730050601     C*
086740050601     C* DAL TIPO SERVIZIO PRENDO LA TARIFFA CON CUI TASSARE
086750050601     C*
086760160225     c**                 eval      tasksc  = o27ksc
086770160225     c**                 eval      tasctr  = o27ctr
086780160225     C                   MOVEL     VIDKLP        TASKSC
086790160225     C                   MOVE      VIDKSC        TASKSC
086800160225     C                   MOVEL     VIDCTR        TASCTR
086810050601     C**
086820050601    1C                   ENDIF
086830050601     C*
086840050601     C                   Z-ADD     ARBPKF        TASPKF
086850160530     C                   Z-ADD     ARBPKb        TASPKb
086860050601     C                   MOVEL     ARBLNP        TASLNP
086870050601     C                   MOVEL     ARBCTS        TASCTS
086880050601     C                   MOVEL     ARBDSP        TASDSP
086890050601     C                   MOVEL     ARBDSP        TASDCT
086900050601     C                   MOVEL     ARBVLF        TASVLF
086910160530     C                   MOVEL     ARBfvF        TASfvf
086920160530     c                   z-add     arbvlc        tasvlc
086930160530     c                   z-add     arbncr        tasncr
086940050601     C                   MOVEL     ARBTSP        TASTSP
086950050601     C     WBOL          IFEQ      '1'
086960050601     C                   MOVEL     §3ATB1        TASTBL
086970050601     C                   MOVEL     VIDDIV        TASDIV
086980050601     C                   Z-ADD     VIDIMV        TASIMV
086990050601     C                   ELSE
087000050601     C                   MOVEL     §3ATB2        TASTBL
087010050601     C                   MOVEL     VI2DIV        TASDIV
087020050601     C**!!!              Z-ADD     VI2VA1        TASIMV
087030050601     C                   END
087040050601     C                   MOVEL     ARBLNA        TASLNA
087050050601     C                   MOVEL     ARBNCL        TASNCL
087060160530     C                   MOVEL     ARBNCL        TASNCLB
087070050601     C                   MOVEL     VIDPOR        TASPOR
087080050601     C                   MOVEL     VIDQFT        TASQFT
087090050601     C     WBOL          IFEQ      '1'
087100050601     C     §3AFCA        ANDEQ     1
087110050601     C     WBOL          OREQ      '2'
087120050601     C     §3AFCA        ANDEQ     2
087130050601     C                   MOVEL     AR9CAS        TASCAS
087140050601     C                   MOVEL     AR9VCA        TASVCA
087150050601     C                   MOVEL     AR9TIC        TASTIC
087160050601     C                   ENDIF
087170050601     C                   MOVEL     VIDIAS        TASIAS
087180050601     C                   MOVEL     VIDVAS        TASVAS
087190050601     C                   MOVEL     ARBCAD        TASCAD
087200050601     C                   MOVEL     ARBNZD        TASNZD
087210050601     C                   MOVEL     ARBCAM        TASCAM
087220050601     C                   MOVEL     ARBNZM        TASNZM
087230050601     C                   MOVEL     ARBFIN        TASFIN
087240050601     C                   MOVEL     ARBFAP        TASFAP
087250050601     C                   MOVEL     ARBTC1        TASTC1
087260050601     C                   MOVEL     ARBTC2        TASTC2
087270050601     C     V1CDDT        IFEQ      'S'
087280050601     C     V1CDDT        OREQ      'C'
087290050601     c     v1cddt        oreq      'P'
087300050601     c     v1cddt        oreq      'Q'
087310050601     C                   MOVEL     'S'           TASDDT
087320050601     C                   ENDIF
087330060925     c****               movel     arbfbr        tasflo
087340060925     c                   clear                   dtas01
087350060925     c                   movel     arbfbr        §asfbr
087360060925     c                   movel     arbcca        §ascca
087370110630     c                   movel     wemd          §asemd
087380150108     c* Pin Code
087390150108     c                   if        arbgma<>*blanks
087400150108     c                   Movel     '7R'          cod
087410150108     c                   Movel(p)  ArbGMa        key
087420150108     c                   ExSr      ctabel
087430150108     c  n30              Movel     TblUni        ds7R
087440150108     c   30              Clear                   ds7R
087450150108     c                   if        §7rpincode='S'
087460150108     c                   movel     'S'           §aspin
087470150108     c                   endif
087480150108     c                   endif
087490060925     c                   eval      tasflo = dtas01
087500050601     C* SE BOLLA ARRIVI IN ASSEGNATO EXPORT, NON PASSO LA CONSEGNA A
087510050601     C*  MAGAZZINO PER FAR CALCOLARE SEMPRE LA VARIA "U"
087520050601     C     *IN10         IFEQ      *OFF
087530050601     C     *IN06         OREQ      *ON
087540050601     C     lnantw        oreq      'COR'
087550050601     c     lnantw        oreq      'MES'
087560050601     c     lnantw        oreq      'PPT'
087570050601     C                   MOVEL     ARBFDN        TASFDN
087580050601     C                   ENDIF
087590050601     C**
087600050601     C* CHAI SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
087610050601     C                   CLEAR                   Tisi95DS
087620050601     C                   MOVEL     ARBCAM        I95CAP
087630050601     C                   MOVEL     ARBNZM        I95NAR
087640050601     C                   MOVEL     '3'           I95TCN
087650050601     C                   MOVEL     ARBDSP        I95DAT
087660050601     C                   MOVEL     FLGTB1        I95TPO
087670050601     C     §3AFCA        IFNE      0
087680050601     C                   MOVEL     'S'           I95FCA
087690050601     C                   ENDIF
087700050601     c     lnpntw        ifeq      'FED'
087710050601     c                   movel     'S'           i95fi1
087720050601     c                   endif
087730050601     C**
087740050601     C                   CALL      'TISI95R'
087750050601     C                   PARM                    Tisi95DS
087760050601     C     O95ERR        IFEQ      *BLANKS
087770050601     C                   MOVEL     O95CTS        TASMCT
087780050601     C                   ENDIF
087790050601
087800050601     c                   z-add     arbncp        Tasncp
087810050601     c                   z-add     arbpkc        Taspkc
087820050601      * Bancali
087830050601     c                   If        %Subst(ArbGva:1:1) = 'B'
087840050601     c                   Eval      TasBan = V1cBan
087850050601     c                   EndIf
087860050601      * Colli Originali
087870050601     c                   If        %Subst(ArbGva:1:1) = 'O'
087880050601     c                   Eval      TasNcl = V1cCor
087890050601     c                   EndIf
087900050601     c                   endsr
087910050531     C*--- TASSAZIONE BOLLA ------------------------------------------*
087920050531     C     TASSAR        BEGSR
087930050601     c                   clear                   dtas
087940050601     c                   clear                   dtasv
087950050601     c* richiamo routine di riempimento campi da passare a tnsf20r
087960050601     c                   eval      wbol = '1'
087970050601     c                   exsr      SR_rtassa
087980050531     c                   eval      tassva = 'R'
087990050804     c
088000050531     c                   eval      parca  = 'S'
088010050531     c                   movel     paramt        kpjbu
088020050531      *
088030050531     C                   CALL      'TNSF20R'
088040050531     C                   PARM                    KPJBA
088050050531     C                   PARM                    DTAS
088060050531     C                   PARM                    DTASV
088070050722      *
088080050804      *
088090050722     c                   eval      parca  = ' '
088100050804     c***                movel     paramt        kpjbu
088110050722      *
088120050531     c                   endsr
088130911211     C*
088140011017     C*
088150991006     C* CALCOLO IL TOTALE FATTURA-------------------------------------*
088160990702     C     TOTFAT        BEGSR
088170030612     C                   CLEAR                   FNLV04DS
088180990702     C* AROTONDO IN ARRIVO O IN PARTENZA SE PREPAGATI NUOVI
088190990702    2C     *IN10         IFEQ      *ON
088200990702     C     *IN23         OREQ      *ON
088210991005     C                   MOVEL     VIDDIV        DIVGEI
088220991005     C                   EXSR      RIEGEI
088230991005     C                   MOVEL     §GEAED        D04AED
088240991005     C                   Z-ADD     §GEAVL        D04VAR
088250990702    2C                   ENDIF
088260991005     C                   MOVEL     'T'           D04FIM
088270990702     C                   MOVEL     ARBDFT        D04DFT
088280990702     C                   MOVEL     VIDIMV        D04IMV
088290990702     C                   MOVEL     VIDCEI        D04CEI
088300990702     C                   MOVEL     AR9TIC        D04TIC
088310990702     C                   MOVEL     AR9VCA        D04VCA
088320990702     C                   Z-ADD     AR9CAS        D04CAS
088330990702     C                   MOVEL     '1'           D04SPE
088340990702     C   23              MOVEL     '8'           D04SPE
088350991005     C                   MOVEL     §3ATB1        D04TBL
088360991005     C                   MOVEL     VIDDIV        D04DIV
088370990702     C                   Z-ADD     VIDPOR        D04POR
088380990702     C                   MOVEL     '1'           UNO               1
088390990702     C*
088400991005     C                   CALL      'FNLV04R'
088410030612     C                   PARM                    FNLV04DS
088420991005     C                   PARM                    DTASV
088430990702     C                   ENDSR
088440990702     C*
088450991005     C**************************************************************************
088460991005     C*  REPERIMENTO TABELLA GEI
088470991005     C**************************************************************************
088480991005     C     RIEGEI        BEGSR
088490030612     C                   CLEAR                   Tibs02DS
088500991117     C                   CLEAR                   DGEI
088510991117     C                   MOVEL     'C'           T02MOD
088520991117     C                   MOVEL     KNSIF         T02SIF
088530991117     C                   MOVEL     'GEI'         T02COD
088540991117     C                   MOVEL     DIVGEI        T02KE1
088550991117     C                   CALL      'TIBS02R'
088560991117     C                   PARM                    KPJBA
088570030612     C                   PARM                    Tibs02DS
088580991117     C     T02ERR        IFEQ      *BLANKS
088590991117     C                   MOVEL     T02UNI        DGEI
088600991117     C                   ENDIF
088610991005     C                   ENDSR
088620911211     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE TX -----------------*
088630911211     C     REGIS3        BEGSR
088640041027     C                   SETOFF                                       3709
088650130412     c                   clear                   wpda_ndc
088660130412     c                   clear                   wpda_ddc
088670130412     c                   clear                   wpda_dcm
088680041027      * salvo i campi relativi al valore da assicurare, qtà da fatturare, valore merce
088690041027     c                   eval      sav_arbias = arbias
088700041027     c                   eval      sav_arbvas = arbvas
088710041027     c                   eval      sav_arbqft = arbqft
088720041027     c                   eval      sav_arbvmd = arbvmd
088730041027     c                   eval      sav_arbvad = arbvas
088740041027     c                   movel     arbctr        sav_arbctr
088750950104     C*  GIRO UDATE
088760950104     C                   TIME                    W0140            14 0
088770950104     C* UDATE IN GGMMAAAA
088780950104     C                   MOVE      W0140         WDTGIO            8 0
088790950104     C*
088800950104     C* UDATE IN AAAAMMGG
088810950104     C                   Z-ADD     WDTGIO        G02DAT
088820950104     C                   MOVEL     *BLANK        G02ERR
088830950104     C                   CALL      'XSRDA8'
088840950104     C                   PARM                    WLBDAT
088850950104     C                   MOVEL     G02INV        DATEU             8 0
088860941221     C* PRENDO I VALORI ASSOLUTI
088870941221     C                   MLLZO     1             VIDPOR
088880941221     C                   MLLZO     1             VIDIMV
088890941221     C                   MLLZO     1             VIDIAS
088900941221     C                   MLLZO     1             VIDVMD
088910941221     C                   MLLZO     1             VIDQFT
088920941221     C                   MLLZO     1             VIDKLP
088930941221     C                   MLLZO     1             VIDKSC
088940070515     c
088950070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
088960070515     c* iornare direttamente il file
088970070515     c*  solo se cambiati dati su testa bolla
088980070515     c                   clear                   wsiaggio          1
088990070515  2b c                   if        sav_arbias <> vidias or sav_arbvas<>vidvas
089000070515     c                             or
089010070515     c                             sav_arbvmd <> vidvmd or sav_arbvad<>vidvad
089020070515     c                             or
089030070515     c                             sav_arbqft <> vidqft  or
089040070515     c                             sav_arbctr<>vidctr
089050070515     c                   eval      wsiaggio='S'
089060070515     c                   clear                   waltrabolla       1
089070070515     C   10KARB          CHAIN     FNBLP01L                           3091
089080070515     C  n10KARB          CHAIN     FNARB01L                           3091
089090070515     c*
089100070515     c                   if        *in91
089110070515     C                   SETON                                        28
089120070515     C                   MOVEL     MSG(4)        VIDMSG
089130070515     C                   GOTO      ENDRG3
089140070515     c                   else
089150070515     c                   if        *in30
089160070515     c                   eval      waltrabolla='N'
089170070515     c                   else
089180130411     c  n10              eval      wpda_ndc=arbndc
089190130411     c  n10              eval      wpda_ddc=arbddc
089200130411     c  n10              eval      wpda_dcm=arbdcm
089210070515     C   10KARB          CHAIN     FNARB01L                           30
089220070515     C  N10KARB          CHAIN     FNBLP01L                           30
089230070515     c* Alla fine per non sporcarmi i campi del record, richaino il
089240070515     c*  record originale che sto variando
089250130411     c   10              eval      wpda_ndc=arbndc
089260130411     c   10              eval      wpda_ddc=arbddc
089270130411     c   10              eval      wpda_dcm=arbdcm
089280070515     c                   endif
089290070515     c                   endif
089300070515     c                   endif
089310131120    1c                   if        wsiaggio=*blanks and d66pda<>' '
089320130412     c                   clear                   waltrabolla       1
089330130412    2c                   if        not *in10
089340130412     C     KARB          CHAIN(n)  FNARB01L                           30
089350130412    3c                   if        *in30
089360130412     c                   eval      waltrabolla='N'
089370130412    3c                   endif
089380130412    2c                   endif
089390130412     c                   eval      wpda_ndc=arbndc
089400130412     c                   eval      wpda_ddc=arbddc
089410130412     c                   eval      wpda_dcm=arbdcm
089420130412     C  N10KARB          CHAIN     FNBLP01L                           30
089430130412     c                   endif
089440941213     C*
089450941213     C* INVIO PER SEDE
089460170116     C***                MOVEL     'FIARBT0T'    SFILE
089470941213     C* ALLOCO OGGETTI
089480170116     C***                EXSR      ALLOC
089490070515     c
089500170116     c***                if        *in91
089510170116     C** 10              unlock    FNBLP01L
089520170116     C**N10              unlock    FNARB01L
089530170116     C**                 GOTO      ENDRG3
089540170116     c**                 endif
089550941213     C*
089560991001     C                   MOVEL     *BLANKS       WCKINL            1
089570991001     C                   EXSR      CARWAR
089580151020     c* Mmorizzo il totale imponibilie del video prima degli arrotondamenti
089590151020     c****               if        tassatoimv=0
089600151019     c                   movel     vidimv        Tassatoimv
089610151020     c****               endif
089620991006     C* SOLO SE C'E' NUMERO FATTURA CALCOLO
089630991006     C     VIDDFT        IFGT      *ZEROS
089640000105     C                   MOVEL     O17DFA        ARBDFT
089650991006     C                   EXSR      TOTFAT
089660991006     C     D04BOL        IFGT      *ZEROS
089670991006     C                   ADD       1             C
089680991006     C                   MOVEL     §$2BOL        WSV(C)
089690991006     C                   Z-ADD     D04BOL        WVA(C)
089700991006     C                   ENDIF
089710991006     C     D04IVA        IFGT      *ZEROS
089720991006     C                   ADD       1             C
089730991006     C                   MOVEL     §$2IVA        WSV(C)
089740991006     C                   Z-ADD     D04IVA        WVA(C)
089750991006     C                   ENDIF
089760991006     C                   ENDIF
089770941213     C*
089780941213     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
089790950616    1C     C             IFGT      0
089800941213     C                   SETON                                        09
089810941213     C*
089820941213     C* INVIO PER SEDE
089830941213     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
089840170116    2C***  C             IFGT      3
089850991006     C* MI SALVO I CAMPI DELL'FIARBT
089860170116     C***                MOVEL     SOVR(1)       SOARBT
089870170116     C***                MOVEL     SALC(1)       SAARBT
089880170116     C***                MOVEL     SDLC(1)       SDARBT
089890941213     C*
089900170116     C***                MOVEL     'FIARBU0T'    SFILE
089910941213     C* ALLOCO OGGETTI
089920170116     C***                EXSR      ALLOC
089930070515     c
089940170116     c**                 if        *in91
089950170116     C** 10              unlock    FNBLP01L
089960170116     C**N10              unlock    FNARB01L
089970170116     C**                 MOVEL     SDARBT        SDLC(1)
089980170116     c**                 exsr      DLCSED
089990170116     C**                 SETON                                        2891
090000170116     C**                 GOTO      ENDRG3
090010170116     c**                 endif
090020941213     C*
090030170116    2C***                ENDIF
090040950616    1C                   ENDIF
090050950114     C*
090060950114     C                   MOVEL     KNMUS         ARBPRU
090070950114     C                   Z-ADD     DATEU         ARBDTV
090080950114     C                   TIME                    ARBORV
090090950114     C                   MOVEL     VIDCVR        ARBCVB
090100991011     C                   MOVEL     '1'           ARBTRC
090110991011     C*
090120991011     C                   MOVEL     '1'           AR6TRC
090130930511     C*
090140060130     C* SE DA STORICIZZARE
090150060130    1C**   *IN10         IFEQ      *ON
090160060130     C**   D66FSA        ANDEQ     'S'
090170060130     C**   *IN10         OREQ      *OFF
090180060130     C**   D66FSP        ANDEQ     'S'
090190060130     C     D66FSA        ifeq      'S'
090200941213     C                   EXSR      STOARB
090210950616    1C                   ENDIF
090220060302     c* storicizzazione motivazione
090230060302     c     d66mtv        ifeq      'S'
090240060302     c                   exsr      sto_mtv
090250060302     c                   endif
090260941213     C**
090270991006     C* INVIO VARIAZIONE --> FIARBU0T
090280941213     C**
090290950616    1C     *IN09         IFEQ      *ON
090300941213     C     C             ANDGT     3
090310941213     C                   Z-ADD     4             C
090320941213     C*
090330941213    2C     D66FFI        IFEQ      'S'
090340950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
090350950616    3C     §3ABSD        IFEQ      ' '
090360941213     C                   Z-ADD     4             C
090370170116     C****               OPEN      FIARBU0T
090380941213     C*
090390941213     C* NE SCRIVO FIN TANTO CHE CE NE SONO
090400950616    4C     WSV(C)        DOWNE     ' '
090410941213     C                   MOVEL     WSV(C)        ARBSVN
090420941213     C                   Z-ADD     WVA(C)        ARBVAN
090430991006     C                   WRITE     FIARBUTT
090440941213     C                   ADD       1             C
090450950616    4C                   ENDDO
090460941213     C*
090470170116     C***                CLOSE     FIARBU0T
090480941213     C* DISALLOCO OGGETTO MEMBRO PER SEDE
090490950616    3C                   ENDIF
090500170116     C****               EXSR      DLCSED
090510950208    2C                   ENDIF
090520941213     C* REIMPOSTO I CAMPI
090530170116     C***                MOVEL     SOARBT        SOVR(1)
090540170116     C***                MOVEL     SAARBT        SALC(1)
090550170116     C***                MOVEL     SDARBT        SDLC(1)
090560941213    1C                   ENDIF
090570930506     C*
090580930506     C                   Z-ADD     VIDKSC        ARBKSC
090590930506     C                   MOVEL     VIDKLP        ARBKSC
090600941209     C                   MOVEL     VIDIAS        ARBIAS
090610941209     C                   MOVEL     VIDVAS        ARBVAS
090620011017      * Se l'importo da assicurare è a 0 non devo scrivere la divisa
090630011017     C     VIDIAS        IFEQ      *ZEROS
090640011017     C                   CLEAR                   ARBVAS
090650011017     C                   ENDIF
090660011017      *
090670941209     C                   MOVEL     VIDVMD        ARBVMD
090680941215     C                   MOVEL     VIDVAD        ARBVAD
090690011017      * Se il valore merce è a 0 non devo scrivere la divisa
090700011017     C     VIDVMD        IFEQ      *ZEROS
090710011017     C                   CLEAR                   ARBVAD
090720011017     C                   ENDIF
090730011017      *
090740941209     C                   MOVEL     VIDQFT        ARBQFT
090750930506     C                   MOVEL     VIDCTR        ARBCTR
090760991006     C                   MOVEL     VIDDIV        ARBDIV
090770941213     C                   MOVEL     VIDPOR        ARBPOR
090780941213     C                   MOVEL     WSV(1)        ARBSV1
090790941213     C                   MOVEL     WVA(1)        ARBVA1
090800941213     C                   MOVEL     WSV(2)        ARBSV2
090810941213     C                   MOVEL     WVA(2)        ARBVA2
090820941213     C                   MOVEL     WSV(3)        ARBSV3
090830941213     C                   MOVEL     WVA(3)        ARBVA3
090840991006     C                   Z-ADD     VIDIMV        ARBIMV
090850991006     C                   MOVEL     O17DFA        ARBDFT
090860950104     C                   MOVEL     VIDNFT        ARBNFT
090870950104     C                   MOVEL     VIDFIV        ARBFIV
090880991013     C* SE TOTALE IMPONIBILE ERA IMMESSO DALLA PARTENZA ED E'
090890991013     C* STATO MODIFICATO TOLGO LA 'P'
090900991013     C     AR6FIM        IFEQ      'P'
090910991013     C     ARBIMV        ANDNE     AR6IMV
090920991013     C     *IN10         ANDEQ     *ON
090930991013     C                   MOVE      *BLANKS       ARBFIM
090940991013     C                   ELSE
090950991013     C* ALTRIMENTI CI METTO QUELLO CHE C'ERA
090960991013     C                   MOVE      AR6FIM        ARBFIM
090970991013     C* SE NON ESISTE FIAR6 E SONO IN PARTENZA IMPOSTO 'P'
090980991013     C     WAR6          IFNE      'E'
090990991013     C     *IN10         ANDEQ     *OFF
091000991013     C                   MOVEL     'P'           ARBFIM
091010991013     C                   END
091020991013     C                   ENDIF
091030941209     C*
091040941212     C                   MOVEL     VIDCEI        ARBCEI
091050930506     C*
091060950616    1C     ARBDFT        IFGT      0
091070930505     C*
091080930505     C* SE TOTALE FATTURA NON CORRISPONDE AL PRECEDENTE RISTAMPA FATT
091090941209     C     D04IFT        COMP      AR6IFT                             3737
091100930505     C*
091110941212     C                   MOVE      D04ALI        ARBALI
091120941212     C                   MOVE      D04IFT        ARBIFT
091130941212     C                   MOVE      D04IMV        ARBIMV
091140930514     C*
091150970613    2C                   SELECT
091160941221     C     ARBPOR        WHENGT    0
091170941212     C                   Z-ADD     D04POR        ARBPOR
091180941221     C     ARBVA1        WHENGT    0
091190991007     C     ARBSV1        ANDNE     §$2BOL
091200991007     C     ARBSV1        ANDNE     §$2IVA
091210941213     C                   ADD       D04POR        ARBVA1
091220941209     C     ARBVA2        WHENGT    0
091230991007     C     ARBSV2        ANDNE     §$2BOL
091240991007     C     ARBSV2        ANDNE     §$2IVA
091250941213     C                   ADD       D04POR        ARBVA2
091260941209     C     ARBVA3        WHENGT    0
091270941213     C                   ADD       D04POR        ARBVA3
091280970613    2C                   ENDSL
091290941209     C*
091300950616    1C                   ENDIF
091310930511     C*
091320930511     C                   MOVEL     VIDCVR        ARBCVB
091330911218     C*
091340950616    1C     D66FFI        IFEQ      'S'
091350950227     C*
091360950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
091370950616    2C     §3ABSD        IFEQ      ' '
091380170116     C***                OPEN      FIARBT0T
091390991006     C                   WRITE     FIARBTTT
091400170116     C***                CLOSE     FIARBT0T
091410950616    2C                   END
091420941212     C* DISALLOCO OGGETTO MEMBRO PER SEDE
091430170116     C***                EXSR      DLCSED                                       FNARBT0T
091440950616    1C                   END
091450941213     C**
091460941216     C* A G G I O R N O
091470941213     C**
091480051214     c* Semodificato il cod tariffa, verifico se volume automatico
091490051214     c                   if        not *in10 and vidctr<>sav_arbctr
091500051214     C*
091510051214     C                   MOVEL     '7T'          COD
091520051214     C                   MOVEL(P)  ARBFVF        KEY
091530051214     C                   EXSR      CTABEL
091540051214     C                   MOVEL     TBLUNI        DS7T
091550051214     C     §7TRVL        IFEQ      'A'
091560051214     C     §7TRVL        OREQ      'P'
091570051214     c                   movel     'S'           wricvl
091580051214     c                   seton                                        24
091590051214     c                   endif
091600051214     c                   endif
091610051214     c
091620991006     C                   Z-ADD     ARBKSC        AR6KSC
091630991006     C                   Z-ADD     ARBCTR        AR6CTR
091640941213     C                   MOVE      ARBALI        AR6ALI
091650941213     C                   MOVE      ARBIFT        AR6IFT
091660941213     C                   MOVE      ARBIMV        AR6IMV
091670991006     C                   MOVEL     ARBDIV        AR6DIV
091680941213     C                   MOVE      ARBPOR        AR6POR
091690941213     C                   MOVE      ARBSV1        AR6SV1
091700941213     C                   MOVE      ARBVA1        AR6VA1
091710941213     C                   MOVE      ARBSV2        AR6SV2
091720941213     C                   MOVE      ARBVA2        AR6VA2
091730941213     C                   MOVE      ARBSV3        AR6SV3
091740941213     C                   MOVE      ARBVA3        AR6VA3
091750941213     C                   MOVEL     ARBCEI        AR6CEI
091760950104     C                   MOVEL     ARBDFT        AR6DFT
091770950104     C                   MOVEL     ARBNFT        AR6NFT
091780950104     C                   MOVEL     ARBFIV        AR6FIV
091790991013     C                   MOVEL     ARBFIM        AR6FIM
091800941213     C*
091810991006     C* CANCELLO LE VARIE NELL'FIAR7
091820941221    1C     WAR7          IFEQ      'E'
091830991006     C                   MOVEL     '1'           WTRC
091840991006     C     KARB2         SETLL     FIAR7000
091850991006     C     KARB2         READE     FIAR7000                               30
091860941213     C*
091870941213    2C     *IN30         DOWEQ     *OFF
091880991006     C                   DELETE    FIAR7000
091890991006     C     KARB2         READE     FIAR7000                               30
091900941213    2C                   ENDDO
091910941213    1C                   ENDIF
091920941213     C*
091930941213     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
091940941213    1C     WSV(4)        IFNE      ' '
091950941213     C                   Z-ADD     4             C
091960991006     C                   CLEAR                   FIAR7000
091970941213     C                   MOVEL     ARBAAS        AR7AAS
091980941213     C                   MOVEL     ARBLNP        AR7LNP
091990941213     C                   MOVEL     ARBNRS        AR7NRS
092000941213     C                   MOVEL     ARBNSP        AR7NSP
092010991006     C                   MOVEL     '1'           AR7TRC
092020941213    2C     WSV(C)        DOWNE     ' '
092030941213     C                   MOVEL     WSV(C)        AR7SVN
092040941213     C                   MOVEL     WVA(C)        AR7VAN
092050991006     C                   WRITE     FIAR7000
092060941213     C                   ADD       1             C
092070941213    2C                   ENDDO
092080941213    1C                   ENDIF
092090941213     C**
092100941216     C* SE HO ELIMINATO TUTTA LA TASSAZIONE, PER S.F., DELETO RECORD
092110950616    1C     AR6DFT        IFEQ      0
092120941216     C     AR6SV1        ANDEQ     ' '
092130941216     C     AR6SV2        ANDEQ     ' '
092140941216     C     AR6SV3        ANDEQ     ' '
092150941216     C     AR6POR        ANDEQ     0
092160991116     C     AR6CEI        ANDEQ     ' '
092170941216     C*
092180950616    2C     WAR6          IFEQ      'E'
092190041027     C     *IN10         ifeq      *OFF
092200991006     C                   MOVEL     '1'           WTRC
092210991006     C     KARB2         DELETE    FIAR6000                           30
092220991116     C                   ELSE
092230041027     C* SE variaz, in ARRIVO DEVO UGUALMENTE tenere
092240991116     C* FIAR6000 (SERVE PER LA PROCEDURA DEI DANNI)
092250991116     C                   UPDATE    FIAR6000
092260991116     C                   ENDIF
092270991116   X2C                   ELSE
092280041027     C* SE NON ESISTE RECORD TASSAZIONE scrivo record se var.in arrivo
092290041027     C     *IN10         ifeq      *ON
092300991116     C                   MOVEL     ARBAAS        AR6AAS
092310991116     C                   MOVEL     ARBLNP        AR6LNP
092320991116     C                   MOVEL     ARBNRS        AR6NRS
092330991116     C                   MOVEL     ARBNSP        AR6NSP
092340991116     C                   WRITE     FIAR6000
092350041027    3C                   ENDIF
092360041027    2C                   ENDIF
092370941216     C*
092380941216     C                   ELSE
092390941216     C*
092400950616    2C     WAR6          IFEQ      'E'
092410991006     C                   UPDATE    FIAR6000
092420941209     C                   ELSE
092430941209     C                   MOVEL     ARBAAS        AR6AAS
092440941209     C                   MOVEL     ARBLNP        AR6LNP
092450941209     C                   MOVEL     ARBNRS        AR6NRS
092460941209     C                   MOVEL     ARBNSP        AR6NSP
092470991006     C                   WRITE     FIAR6000
092480950616    2C                   ENDIF
092490950616    1C                   ENDIF
092500941209     C*
092510151016     c* Se tassazione fino all'imponibile, aggiorno peso e volume usati
092520151016     c*  per la fatturazione se inserito o variato totale imponibile
092530151016     c                   if        (ar6imv>0 and ar6imv<>savimv_fat) or
092540151016     c                             ar6imv=0
092550151118     c                   exsr      regpesivol
092560151016     c                   endif
092570950113     C*
092580950113     C* RICALCOLO VOLUME AUTOMATICO E POI VADO IN VIDEATA DEL VOLUME
092590950113     C*  SE VOLUME AUTOMATICO O PRESUNTO PER PRECEDENTE VARIAIZONE
092600950113     C*  MITTENTE
092610950616    1C     *IN24         IFEQ      *ON
092620051214     c* Mi salvo i campi che userò per la storicizzazione
092630051214     C                   MOVEL     sav_arbctr    MITCTR
092640051214     C                   MOVEL     ARBFVF        MITFVF
092650051214     C                   MOVEL     ARBVLF        MITVLF
092660051214     c
092670950113     C                   EXSR      CALVOL
092680070907     C**                 MOVEL     'I2'          D48CVB
092690070907     C**                 MOVEL     'E'           WESEG
092700070907     c                   add       1             kk
092710070907     c                   movel     'I2'          kcvb(kk)
092720070911     c                   movel     'V'           kffr(kk)
092730070911     c                   movel     ' '           kf12(kk)
092740070911     c****               clear                   w48f12
092750950114     C* AGGIORNO GIA' IL VOLUME TANTO PASSO DALLA VIDEATA DEL VOLUME
092760950114     C*  COMUNQUE PER LA TRASMISSIONE
092770950114     C                   MOVEL     D18FVF        ARBFVF
092780950114     C                   MOVEL     D18VLF        ARBVLF
092790070911     C*****              MOVEL     'V'           D48FFR
092800950616     C                   END
092810911211     C*
092820950114     C   10              UPDATE    FNARB000
092830950114     C  N10              UPDATE    FNBLP000
092840950114     C*
092850041027  1b c***                if        wstessogas = 'S' and (d66ffi = 'E' or
092860041027     c***                          d66ffi = 'P') and *in10 = *off
092870041027  2b c***                if        sav_vidias <> vidias or sav_vidvas <> vidvas
092880041027     c***                          or
092890041027     c***                          sav_vidvmd <> vidvmd or sav_vidvad <> vidvad
092900041027     c***                          or
092910041027     c***                          sav_vidqft <> vidqft
092920041027     c***                exsr      Agg_ARB
092930041027  2e c***                endif
092940041027  1e c***                endif
092950041027     c
092960041027     c* Aggiorno altra bolla se c'e' e variati i campi sopra
092970041027     c                   if        wsiaggio='S' and waltrabolla=' '
092980041027     c* non aggiono mai il ksc:
092990041027     c* in  partenza perchè il S.F. va in ar6
093000041027     c*  in arrivo perchè il cod mittente se cambiato viene impostato
093010041027     c*  in regis8
093020041027     C   10              except    arbTblp
093030041027     C  N10              except    arbTarb
093040041027     c                   endif
093050130411
093060130411     c* Richiamo routine per invio variazione a pda
093070130411     c                   exsr      sr_pda
093080911212     C     ENDRG3        ENDSR
093090911211     C*
093100970613     C* SE BOLLA SU DISTINTA CHIUSA NON FACCIO NULLA ALTRIMENTI ------*
093110970613     C*  CHIUDO LA BOLLA
093120970613     C     CHIUBO        BEGSR
093130970613     C                   MOVEL     'S'           WAGG              1
093140970613     C*
093150970613     C* NON AGGIORNO COL MANDATO DI INTROITO SE SULLA BOLLA C'E'
093160970613     C*  UN NUMERO DISTINTA ED E' CHIUSA
093170080516     c                   clear                   fvvfcf
093180970613     C     ARBNDC        IFGT      0
093190020911     c                   Z-Add     ArbIfp        Kfgs
093200970613     C                   Z-ADD     ARBNDC        W0050             5 0
093210020911     C     KFVV          CHAIN     FNFVV01L                           30
093220970613     C     *IN30         IFEQ      *OFF
093230970613     C     FVVFCF        ANDEQ     'S'
093240970613     C                   CLEAR                   WAGG
093250970613     C                   ENDIF
093260970613     C                   ENDIF
093270970613     C*
093280970613    4C     WAGG          IFEQ      'S'
093290970613    5C     ARBNMI        IFEQ      0
093300970613     C                   Z-ADD     8888888       ARBNMI
093310970613     C                   Z-ADD     8888888       ARBNDC
093320970613     C                   Z-ADD     DATEU         ARBDDC
093330080516     c                   else
093340080516     c
093350080516     c                   if        arbndc>0 and fvvfcf<>'S'
093360080516     C                   Z-ADD     8888888       ARBNDC
093370080516     c                   endif
093380080516     c
093390970613    5C                   ENDIF
093400970613    4C                   ENDIF
093410970613     C                   ENDSR
093420970613     C*
093430911211     C*--- CONTROLLI FORMATO4 ----------------------------------------*
093440911211     C     CONTR4        BEGSR
093450040123     C**!!!              MOVEL     ' '           CEISV3
093460920319     C                   EXSR      CTRCOM
093470920319     C   90              GOTO      ENDCT4
093480920319     C*
093490911211     C* T A S S A Z I O N E
093500991011     C     *INKN         IFEQ      *ON
093510991011     c* fattura immediata
093520991011     C     *IN14         OREQ      *ON
093530991011     C                   MOVEL     '2'           WBOL              1
093540991011     C                   EXSR      TASSAZ
093550991011     C   KN
093560991011     COR 90              GOTO      ENDCT4
093570991011     C                   END
093580911211     C*
093590990930     C* OBBLIGATORIO IMPORTO DELLA VARIA
093600040123     C**!!!VI2VA1        IFEQ      0
093610040123     C**!!!              MOVEL     MSG(51)       VIDMSG
093620040123     C**!!!              SETON                                        519028
093630040123     C**!!!              GOTO      ENDCT4
093640040123     C**!!!              END
093650991007     C* Se divisa sprotetta significa che manca tariffa: in questo caso
093660991007     C* controllo che la divisa inserita sia = alla divisa della moneta
093670991007     C*  di conto SE CLIENTE  9999
093680991007     C     *IN14         IFEQ      *ON
093690991007     C     *IN07         ANDEQ     *OFF
093700991007     C     VI2DIV        ANDNE     §GEDCN
093710991007     C     VIDKSC        ANDEQ     9999
093720991007     C                   SETON                                        459028
093730991007     C                   MOVEA     MSG(90)       K78
093740991007     C                   MOVEA     §GEDCN        K78(71)
093750991007     C                   MOVEA     K78           VIDMSG
093760991007     C                   GOTO      ENDCT4
093770991007     C                   ENDIF
093780991006     C* DEMANDO IL RESTO DEI CONTROLLI A PGM FNLV16R
093790100120     C                   CLEAR                   dlv1601
093800100120     C                   CLEAR                   Fnlv16DS
093810991006     C                   CLEAR                   DTASV
093820991006     C                   MOVEL     'A'           D17TBO
093830991006     C                   MOVEL     VIDKLP        D17KSC
093840991006     C                   MOVE      VIDKSC        D17KSC
093850991012     C     DFT2BO        IFGT      0
093860991012     C                   Z-ADD     DFT2BO        D17DSP
093870991012     C                   ELSE
093880991006     C                   Z-ADD     ARBDSP        D17DSP
093890991012     C                   ENDIF
093900991006     C                   MOVEL     ARBLNP        D17LNP
093910991006     C                   MOVEL     ARBLNA        D17LNA
093920100120     C                   MOVEL     §3ATB2        D17TBL
093930991006     C                   MOVEL     §3ARBL        D17RBL
093940020503     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
093950040123     C                   MOVEL     §3ASVA        §d17sva
093960040123      * passo che devo controllare la seconda bolla
093970040123     c                   Eval      §d17Tb2 = 'S'
093980100120
093990100120     c* Se si tratta di ffattura immediata , passo cod iso ddestintario
094000100120    1c                   if        *in14
094010100120     c                   Eval      §d17ctriso='S'
094020100120    2c                   if        arbcpi<>*blanks
094030100120     C     'PRIVATO'     SCAN      arbcpi        posiz                    30
094040100120     c* solo numeri
094050100120     c                   select
094060100120    3c                   when      not *in30 and %subst(arbcpi:1:2)>=*zeros
094070100120     c                   eval      §d17iso='IT'
094080100120     c* "PRIVATO" senza iso
094090100120     c                   when      *in30 and posiz<=3
094100100120     c                   eval      §d17iso='$$'
094110100120     c                   other
094120100120     c                   eval      §d17iso= %subst(arbcpi:1:2)
094130100120    3c                   ENDSL
094140100120    2c                   endif
094150100120    1c                   endif
094160100120     c
094170040123     c                   Movel     dlv1601       D17Flo
094180100120     c
094190991006     C                   MOVEL     VI2DIV        D17DIV
094200991006     C                   MOVEL     WDIV          D17DVP
094210991006    1C     §3AFCA        IFEQ      2
094220991006     C                   MOVEL     'S'           D17FCA
094230991006    1C                   ENDIF
094240991006     C                   MOVEL     'E'           D17FIM
094250991006     C                   MOVEL     VI2CEI        D17CEI
094260991006     C                   MOVEL     VI2SV1        VSV(1)
094270040123     C**!!!              MOVEL     VI2VA1        VVA(1)
094280040123     c                   Z-add     Vi2Va1        vva(1)
094290040123     c                   Movel     Vi2Es1        ves(1)
094300040123     c                   Movel     Vi2Sv2        vsv(2)
094310040123     c                   Movel     Vi2Es2        ves(2)
094320040123     c                   Z-add     Vi2Va2        vva(2)
094330040123     c                   MOVEL     Vi2Sv3        vsv(3)
094340040123     c                   Z-add     Vi2Va3        vva(3)
094350040123     c                   Movel     Vi2Es3        ves(3)
094360040123     c                   MOVEL     Vi2Sv4        vsv(4)
094370040123     c                   Z-add     Vi2Va4        vva(4)
094380040123     c                   Movel     Vi2Es4        ves(4)
094390041130     c                   MOVEL     Vi2Sv5        vsv(5)
094400041130     c                   Z-add     Vi2Va5        vva(5)
094410041130     c                   Movel     Vi2Es5        ves(5)
094420041130     c                   MOVEL     Vi2Sv6        vsv(6)
094430041130     c                   Z-add     Vi2Va6        vva(6)
094440041130     c                   Movel     Vi2Es6        ves(6)
094450991006     C                   CALL      'FNLV16R'
094460030612     C                   PARM                    Fnlv16DS
094470991006     C                   PARM                    DTASV
094480991011     C                   MOVEL     D17DIV        VI2DIV
094490991006     C                   MOVEL     D17CEI        VI2CEI
094500991011     C                   MOVEL     O17DEI        DESCEI
094510040123     C**!!!              MOVEL     VSV(1)        VI2SV1
094520040123     C**!!!              MOVEL     VES(1)        CEISV3            1
094530040123      * Ricarico le varie
094540040123     c                   Movel     Vsv(1)        Vi2Sv1
094550040123     c                   Z-add     Vva(1)        Vi2Va1
094560040123     c                   Movel     Ves(1)        Vi2Es1
094570040123     c                   Movel     Vsv(2)        Vi2Sv2
094580040123     c                   Z-add     Vva(2)        Vi2Va2
094590040123     c                   Movel     Ves(2)        Vi2Es2
094600040123     c                   Movel     Vsv(3)        Vi2Sv3
094610040123     c                   Z-add     Vva(3)        Vi2Va3
094620040123     c                   Movel     Ves(3)        Vi2Es3
094630040123     c                   Movel     Vsv(4)        Vi2Sv4
094640040123     c                   Z-add     Vva(4)        Vi2Va4
094650040123     c                   Movel     Ves(4)        Vi2Es4
094660041130     c                   Movel     Vsv(5)        Vi2Sv5
094670041130     c                   Z-add     Vva(5)        Vi2Va5
094680041130     c                   Movel     Ves(5)        Vi2Es5
094690041130     c                   Movel     Vsv(6)        Vi2Sv6
094700041130     c                   Z-add     Vva(6)        Vi2Va6
094710041130     c                   Movel     Ves(6)        Vi2Es6
094720040123      * Controllo se caricata almeno una varia
094730041130do  1c                   Do        6             c
094740040123     c                   If        vsv(c) <> *Blanks
094750040123     c                   Eval      wesvar1 = 'S'
094760040123     c                   EndIf
094770040123    1c                   EndDo
094780040123
094790991006     C     O17ERR        IFNE      *BLANKS
094800991006     C                   SETON                                        90
094810991006     C     O17MSG        IFNE      *BLANKS
094820991006     C                   MOVEL     O17MSG        VIDMSG
094830991006     C                   SETON                                        28
094840991006     C                   ENDIF
094850991006     C     O17ERR        IFEQ      '6'
094860040123     c                   Select
094870040123     c                   When      O17Erv = 1
094880991006     C                   SETON                                        49
094890040123     c                   When      O17Erv = 2
094900040123     C                   SETON                                        63
094910040123     c                   When      O17Erv = 3
094920040123     C                   SETON                                        64
094930040123     c                   When      O17Erv = 4
094940040123     C                   SETON                                        65
094950041130     c                   When      O17Erv = 5
094960041130     C                   SETON                                        66
094970041130     c                   When      O17Erv = 6
094980041130     C                   SETON                                        67
094990040123     c                   EndSl
095000991006     C                   ENDIF
095010991006     C     O17ERR        IFEQ      '1'
095020991006     C                   SETON                                        50
095030991006     C                   ENDIF
095040991006     C     O17ERR        IFEQ      '7'
095050991006     C                   SETON                                        45
095060991006     C                   ENDIF
095070991006     C   90              GOTO      ENDCT4
095080991006     C                   ENDIF
095090991006     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
095100991006    1C  N07VI2DIV        IFNE      WDIV
095110991006     C* CONVERSIONE VARIA
095120991006    3C     VI2VA1        IFEQ      VA2VA1
095130991006     C     VI2VA1        ANDGT     *ZEROS
095140991006     C     WDIV          ANDNE     *BLANKS
095150030612     C                   CLEAR                   YeurcoDS
095160991006     C                   MOVEL     WDIV          YECDVI
095170991006     C                   Z-ADD     VI2VA1        YECIMI
095180991006     C                   MOVEL     VI2DIV        YECDVO
095190991006     C     O17FDC        IFEQ      'S'
095200011114     C                   MOVE      '3'           YECDCO
095210991006     C                   ELSE
095220991006     C                   CLEAR                   YECDCO
095230991006     C                   ENDIF
095240991006     C                   CALL      'YEURCO'
095250030612     C                   PARM                    YeurcoDS
095260991006     C                   Z-ADD     YECIMO        VI2VA1
095270991012     C                   SETON                                        51
095280991006    3C                   ENDIF
095290040123      * Conversione Varia 2
095300040123    2c                   IF        Vi2Va2 = Va2Va2 and Vi2Va2 > *Zeros and
095310040123     c                             wdiv <> *Blanks
095320040123     c                   Clear                   Yeurcods
095330040123     c                   Movel     wdiv          YECDVI
095340040123     c                   Z-add     Vi2Va2        YECIMI
095350040123     c                   Movel     Vi2Div        YECDVO
095360040123     c                   If        O17Fdc = 'S'
095370040123     c                   Movel     '3'           YECDCO
095380040123     c                   EndIf
095390040123     c                   Call      'YEURCO'
095400040123     c                   Parm                    Yeurcods
095410040123     c                   Z-add     YECIMO        Vi2Va2
095420040123     c                   Eval      *In63 = *On
095430040123    2c                   EndIf
095440040123      * Conversione Varia 3
095450040123    2c                   IF        Vi2Va3 = Va2Va3 and Vi2Va3 > *Zeros and
095460040123     c                             wdiv <> *Blanks
095470040123     c                   Clear                   yeurcods
095480040123     c                   Movel     wdiv          YECDVI
095490040123     c                   Z-add     Vi2Va3        YECIMI
095500040123     c                   Movel     Vi2Div        YECDVO
095510040123     c                   If        O17Fdc = 'S'
095520040123     c                   Movel     '3'           YECDCO
095530040123     c                   EndIf
095540040123     c                   Call      'YEURCO'
095550040123     c                   Parm                    yeurcods
095560040123     c                   Z-add     YECIMO        Vi2Va3
095570040123     c                   Eval      *In64 = *On
095580040123    2c                   EndIf
095590040123      * Conversione Varia 4
095600040123    2c                   IF        Vi2Va4 = Va2Va4 and Vi2Va4 > *Zeros and
095610040123     c                             wdiv <> *Blanks
095620040123     c                   Clear                   yeurcods
095630040123     c                   Movel     wdiv          YECDVI
095640040123     c                   Z-add     Vi2Va4        YECIMI
095650040123     c                   Movel     Vi2Div        YECDVO
095660040123     c                   If        O17Fdc = 'S'
095670040123     c                   Movel     '3'           YECDCO
095680040123     c                   EndIf
095690040123     c                   Call      'YEURCO'
095700040123     c                   Parm                    yeurcods
095710040123     c                   Z-add     YECIMO        Vi2Va4
095720040123     c                   Eval      *In65 = *On
095730040123    2c                   EndIf
095740041130      * Conversione Varia 5
095750041130    2c                   IF        Vi2Va5 = Va2Va5 and Vi2Va5 > *Zeros and
095760041130     c                             wdiv <> *Blanks
095770041130     c                   Clear                   yeurcods
095780041130     c                   Movel     wdiv          YECDVI
095790041130     c                   Z-add     Vi2Va5        YECIMI
095800041130     c                   Movel     Vi2Div        YECDVO
095810041130     c                   If        O17Fdc = 'S'
095820041130     c                   Movel     '3'           YECDCO
095830041130     c                   EndIf
095840041130     c                   Call      'YEURCO'
095850041130     c                   Parm                    yeurcods
095860041130     c                   Z-add     YECIMO        Vi2Va5
095870041130     c                   Eval      *In66 = *On
095880041130    2c                   EndIf
095890041130      * Conversione Varia 6
095900041130    2c                   IF        Vi2Va6 = Va2Va6 and Vi2Va6 > *Zeros and
095910041130     c                             wdiv <> *Blanks
095920041130     c                   Clear                   yeurcods
095930041130     c                   Movel     wdiv          YECDVI
095940041130     c                   Z-add     Vi2Va6        YECIMI
095950041130     c                   Movel     Vi2Div        YECDVO
095960041130     c                   If        O17Fdc = 'S'
095970041130     c                   Movel     '3'           YECDCO
095980041130     c                   EndIf
095990041130     c                   Call      'YEURCO'
096000041130     c                   Parm                    yeurcods
096010041130     c                   Z-add     YECIMO        Vi2Va6
096020041130     c                   Eval      *In67 = *On
096030041130    2c                   EndIf
096040040123     C**!!!              Z-ADD     VI2VA1        VA2VA1
096050040123     C**!!!              Z-ADD     VI2VA1        VVA(1)
096060040123     C**!!!              MOVEL     VI2DIV        WDIV
096070991012     C* RIEMETTO COMUNQUE LA VIDEATA SE HANNO VARIATO LA DIVISA
096080991012     C* (POTREI RIEMETTERLA SOLO SE HO CONVERTITO IMPORTI MA LA
096090991012     C* RIEMETTO COMUNQUE PER FARE COME PER LA PRIMA BOLLA)
096100991012     C                   SETON                                        90
096110040123
096120040123     c                   Z-add     Vi2Va1        Va2Va1
096130040123     c                   Z-add     Vi2Va2        Va2Va2
096140040123     c                   Z-add     Vi2Va3        Va2Va3
096150040123     c                   Z-add     Vi2Va4        Va2Va4
096160041130     c                   Z-add     Vi2Va5        Va2Va5
096170041130     c                   Z-add     Vi2Va6        Va2Va6
096180040123     c                   Z-add     Vi2Va1        Vva(1)
096190040123     c                   Z-add     Vi2Va2        Vva(2)
096200040123     c                   Z-add     Vi2Va3        Vva(3)
096210040123     c                   Z-add     Vi2Va4        Vva(4)
096220041130     c                   Z-add     Vi2Va5        Vva(5)
096230041130     c                   Z-add     Vi2Va6        Vva(6)
096240040123     c                   Movel     Vi2Div        Wdiv
096250040123
096260991012     C                   GOTO      ENDCT4
096270991006   X1C                   ELSE
096280991006     C                   Z-ADD     VI2VA1        VA2VA1
096290040123     c                   Z-add     Vi2Va2        Va2Va2
096300040123     c                   Z-add     Vi2Va3        Va2Va3
096310040123     c                   Z-add     Vi2Va4        Va2Va4
096320041130     c                   Z-add     Vi2Va5        Va2Va5
096330041130     c                   Z-add     Vi2Va6        Va2Va6
096340991006    1C                   ENDIF
096350011206      *
096360011206      * Controllo che la somma degli importi immessi non superi il valore
096370011206      * impostato in §GEIMF
096380040123     c**!!!              eval      w0173 = vi2va1
096390040123     c                   xfoot     vva           w0173
096400011206      *      controllo se devo convertire il totale imponibile
096410011206  1b c                   if        vi2div <> §gedcn
096420030612     c                   clear                   YeurcoDS
096430011206     c                   eval      yecdvi = vi2div
096440011206     c                   eval      yecdvo = §gedcn
096450040123     c**!!!              eval      yecimi = vi2va1
096460040123     c                   eval      yecimi = w0173
096470011206     c                   move      decsav        yecdco
096480011206     c                   call      'YEURCO'
096490030612     c                   parm                    YeurcoDS
096500011206  2b c                   if        yecesi = ' '
096510011206     c                   eval      w0173 = yecimo
096520011206  2e c                   endif
096530011206  1e c                   endif
096540011206      *      controllo se l'imponibile supera il limite fatturabile
096550011220  1b c                   if        w0173 > imfsav
096560011206     c                   eval      vidmsg = msg(110)
096570011220     c                   eval      %subst (vidmsg:45:14) = (%editc(imfsav: '4'))
096580011206     c                   eval      %subst (vidmsg:60:3) = §gedcn
096590040123     c**!!!              eval      *in51 = *on
096600011206     c                   eval      *in90 = *on
096610011206     c                   goto      endct4
096620011206  1e c                   endif
096630040123
096640040123     c                   Z-add     w0173         TotImv
096650920319     C*
096660911211     C     ENDCT4        ENDSR
096670911212     C*
096680911212     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE 2X -----------------*
096690911212     C     REGIS4        BEGSR
096700040130     c                   Setoff                                       3709
096710130412     c                   clear                   wpda_ndc
096720130412     c                   clear                   wpda_ddc
096730130412     c                   clear                   wpda_dcm
096740040130
096750950104     C*  GIRO UDATE
096760950104     C                   TIME                    W0140            14 0
096770950104     C* UDATE IN GGMMAAAA
096780950104     C                   MOVE      W0140         WDTGIO            8 0
096790950104     C*
096800950104     C* UDATE IN AAAAMMGG
096810950104     C                   Z-ADD     WDTGIO        G02DAT
096820950104     C                   MOVEL     *BLANK        G02ERR
096830950104     C                   CALL      'XSRDA8'
096840950104     C                   PARM                    WLBDAT
096850950104     C                   MOVEL     G02INV        DATEU             8 0
096860941221     C* PRENDO I VALORI ASSOLUTI
096870941221     C                   MLLZO     1             VIDKLP
096880941221     C                   MLLZO     1             VIDKSC
096890941221     C                   MLLZO     1             VI2VA1
096900040123     C                   MLLZO     1             VI2VA2
096910040123     C                   MLLZO     1             VI2VA3
096920040123     C                   MLLZO     1             VI2VA4
096930041130     C                   MLLZO     1             VI2VA5
096940041130     C                   MLLZO     1             VI2VA6
096950941221     C*
096960950203     C                   MOVEL     KNMUS         ARBPRU
096970920120     C* INVIO PER SEDE
096980170116     C****               MOVEL     'FIARBT0T'    SFILE             8
096990930210     C* ALLOCO OGGETTI
097000170116     C***                EXSR      ALLOC
097010170116     C***91              GOTO      ENDRG4
097020040123      * carico le varie in una sk di comodo
097030040123     c                   ExSr      CarWar1
097040040123      * se c'è n.fattura calcolo
097050040129If  1c                   If        Dft2bo > *Zeros
097060040129     c                   Movel     Dft2bo        ArbDft
097070040123     C                   MOVEL     VI2DIV        ARBDIV
097080040129     C                   MOVEL     VI2DIV        divgei
097090040123     C                   EXSR      RIEGEI
097100040123     C                   CLEAR                   Fnlv04ds
097110040123     C                   MOVEL     'T'           D04FIM
097120040123     C                   Z-ADD     ArbDft        D04DFT
097130040123     C                   MOVEL     §GEAED        D04AED
097140040123     C                   Z-ADD     §GEAVL        D04VAR
097150040123     c                   Z-add     TotImv        D04Imv
097160040123     C                   MOVEL     VI2CEI        D04CEI
097170040123     C                   MOVEL     AR9TIC        D04TIC
097180040123     C                   MOVEL     AR9VCA        D04VCA
097190040123     C                   Z-ADD     AR9CAS        D04CAS
097200040123     C                   MOVEL     '2'           D04SPE
097210040123     C                   MOVEL     §3ATB2        D04TBL
097220040123     C                   MOVEL     VI2DIV        D04DIV
097230040123     C                   MOVEL     '1'           UNO
097240040123     C*
097250040123     C                   CALL      'FNLV04R'
097260040123     C                   PARM                    Fnlv04ds
097270040123     C                   PARM                    DTASV
097280040123     C*
097290040123     C     D04BOL        IFGT      *ZEROS
097300040123     C                   ADD       1             C
097310040123     C                   MOVEL     §$2BOL        WSV(C)
097320040123     C                   Z-ADD     D04BOL        WVA(C)
097330040123     C                   ENDIF
097340040123     C     D04IVA        IFGT      *ZEROS
097350040123     C                   ADD       1             C
097360040123     C                   MOVEL     §$2IVA        WSV(C)
097370040123     C                   Z-ADD     D04IVA        WVA(C)
097380040123     C                   ENDIF
097390040123    1c                   EndIf
097400040123
097410040123     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
097420040123    1C     C             IFGT      0
097430040123     C                   SETON                                        09
097440040123     C*
097450040123     C* INVIO PER SEDE
097460040123     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
097470170116    2C**   C             IFGT      3
097480040123     C* MI SALVO I CAMPI DELL'FIARBT
097490170116     C***                MOVEL     SOVR(1)       SOARBT
097500170116     C***                MOVEL     SALC(1)       SAARBT
097510170116     C***                MOVEL     SDLC(1)       SDARBT
097520040123     C*
097530170116     C***                MOVEL     'FIARBU0T'    SFILE
097540040123     C* ALLOCO OGGETTI
097550170116     C***                EXSR      ALLOC
097560170116     c***                if        *in91
097570170116     C***                MOVEL     SDARBT        SDLC(1)
097580170116     c***                exsr      DLCSED
097590170116     C***                SETON                                        2891
097600170116     C***                GOTO      ENDRG4
097610170116     c***                endif
097620070515     c
097630170116    2C****               ENDIF
097640040123    1C                   ENDIF
097650911212     C*
097660991007     C                   Z-ADD     DATEU         ARBDTV
097670991007     C                   TIME                    ARBORV
097680991007     C                   MOVEL     VIDCVR        ARBCVB
097690991011     C                   MOVEL     '2'           ARBTRC
097700991011     C                   MOVEL     '2'           AR6TRC
097710991007     C                   CLEAR                   ARBFST
097720991007     C                   CLEAR                   ARBDDS
097730991007     C                   CLEAR                   ARBQFT
097740991007     C                   CLEAR                   ARBVMD
097750991007     C                   CLEAR                   ARBVAD
097760991007     C                   CLEAR                   ARBIAS
097770991007     C                   CLEAR                   ARBVAS
097780911212     C*
097790941212     C* SE DA STORICIZZARE --> STORICIZZO
097800991007     C     WAR62         IFEQ      'E'
097810060130    1C     D66FSA        ANDEQ     'S'
097820060130     C**   *IN10         IFEQ      *ON
097830060130    1C**   D66FSA        ANDEQ     'S'
097840060130     C**   *IN10         OREQ      *OFF
097850060130    1C**   D66FSP        ANDEQ     'S'
097860991007     C                   EXSR      STOARB
097870941212     C                   END
097880060130     C**                 END
097890060302     c* storicizzazione motivazione
097900060302     c     d66mtv        ifeq      'S'
097910060302     c                   exsr      sto_mtv
097920060302     c                   endif
097930040123     C**
097940040123     C* INVIO VARIAZIONE --> FIARBU0T
097950040123     C**
097960040123    1C     *IN09         IFEQ      *ON
097970040123     C     C             ANDGT     3
097980040123     C                   Z-ADD     4             C
097990040123     C*
098000040123    3C     D66FFI        IFEQ      'S'
098010040123     C                   Z-ADD     4             C
098020170116     C****               OPEN      FIARBU0T
098030040123     C*
098040040123     C* NE SCRIVO FIN TANTO CHE CE NE SONO
098050040123    4C     WSV(C)        DOWNE     ' '
098060040123     C                   MOVEL     WSV(C)        ARBSVN
098070040123     C                   Z-ADD     WVA(C)        ARBVAN
098080040123     C                   WRITE     FIARBUTT
098090040123     C                   ADD       1             C
098100040123    4C                   ENDDO
098110040123     C*
098120170116     C****               CLOSE     FIARBU0T
098130040123     C* DISALLOCO OGGETTO MEMBRO PER SEDE
098140170116     C****               EXSR      DLCSED                                       FIARBU0T
098150040123    3C                   ENDIF
098160040123     C* REIMPOSTO I CAMPI
098170170116     C***                MOVEL     SOARBT        SOVR(1)
098180170116     C***                MOVEL     SAARBT        SALC(1)
098190170116     C***                MOVEL     SDARBT        SDLC(1)
098200040123    1C                   ENDIF
098210991011     C*
098220991011     C                   MOVEL     VIDKLP        ARBKSC
098230991007     C                   MOVE      VIDKSC        ARBKSC
098240991007     C                   CLEAR                   ARBPOR
098250991007     C                   MOVEL     VI2DIV        ARBDIV
098260040123     C**!!!              MOVEL     VI2SV1        ARBSV1
098270040123     C**!!!              MOVEL     VI2VA1        ARBVA1
098280040123     C                   MOVEL     WSV(1)        ARBSV1
098290040123     C                   Z-ADD     WVA(1)        ARBVA1
098300040123     C                   MOVEL     WSV(2)        ARBSV2
098310040123     C                   Z-ADD     WVA(2)        ARBVA2
098320040123     C                   MOVEL     WSV(3)        ARBSV3
098330040123     C                   Z-ADD     WVA(3)        ARBVA3
098340991007     C                   MOVEL     VI2CEI        ARBCEI
098350991007     C                   MOVEL     VIDCTR        ARBCTR
098360040123     C**!!!              Z-ADD     VI2VA1        ARBIMV
098370040123     C                   Z-ADD     TotImv        ARBIMV
098380040123     C**!!!              CLEAR                   ARBSV2
098390040123     C**!!!              CLEAR                   ARBVA2
098400040123     C**!!!              CLEAR                   ARBSV3
098410040123     C**!!!              CLEAR                   ARBVA3
098420040129     C**!!!              CLEAR                   ARBALI
098430040129     C**!!!              CLEAR                   ARBIFT
098440991007     C                   Z-ADD     VI2NFT        ARBNFT
098450991007     C                   Z-ADD     VI2FIV        ARBFIV
098460991007     C                   Z-ADD     AR6DFT        ARBDFT
098470991013     C* SE TOTALE IMPONIBILE ERA IMMESSO DALLA PARTENZA ED E'
098480991013     C* STATO MODIFICATO TOLGO LA 'P'
098490991013     C     AR6FIM        IFEQ      'P'
098500991013     C     ARBIMV        ANDNE     AR6IMV
098510991013     C     *IN10         ANDEQ     *ON
098520991013     C                   MOVE      *BLANKS       ARBFIM
098530991013     C                   ELSE
098540991013     C* ALTRIMENTI CI METTO QUELLO CHE C'ERA
098550991013     C                   MOVE      AR6FIM        ARBFIM
098560991013     C* SE NON ESISTE FIAR6 E SONO IN PARTENZA IMPOSTO 'P'
098570991013     C     WAR62         IFNE      'E'
098580991013     C     *IN10         ANDEQ     *OFF
098590991013     C                   MOVEL     'P'           ARBFIM
098600991013     C                   END
098610991013     C                   ENDIF
098620040129
098630040130if  1c                   If        Arbdft > 0
098640040129     C* SE CAMBIATO TOTALE FATTURA RISTAMPO IN AUTOMATICO LA BOLLA
098650040129     C     D04IFT        COMP      AR6IFT                             3737
098660040129
098670040129     C                   MOVE      D04ALI        ARBALI
098680040129     C                   Z-ADD     D04IFT        ARBIFT
098690040130     C                   z-add     D04IMV        ARBIMV
098700051111     c                   select
098710051111     C     ARBVA1        WHENGT    0
098720051111     C     ARBSV1        ANDNE     §$2BOL
098730051111     C     ARBSV1        ANDNE     §$2IVA
098740051111     C                   ADD       D04POR        ARBVA1
098750051111     C     ARBVA2        WHENGT    0
098760051111     C     ARBSV2        ANDNE     §$2BOL
098770051111     C     ARBSV2        ANDNE     §$2IVA
098780051111     C                   ADD       D04POR        ARBVA2
098790051111     C     ARBVA3        WHENGT    0
098800051111     C                   ADD       D04POR        ARBVA3
098810051111    3C                   ENDSL
098820040130     c                   EndIf
098830911212     C*
098840070503     C     D66FFI        IFEQ      'S'
098850170116     C***                OPEN      FIARBT0T
098860991007     C                   WRITE     FIARBTTT
098870170116     C***                CLOSE     FIARBT0T
098880911212     C* DISALLOCO OGGETTO
098890170116     C****               EXSR      DLCSED                                       FIARBT0T
098900911212     C                   END
098910911212     C*
098920911212     C* AGGIORNO
098930991013     C                   Z-ADD     ARBKSC        AR6KSC
098940991013     C                   Z-ADD     ARBCTR        AR6CTR
098950991013     C                   MOVEL     ARBDIV        AR6DIV
098960991013     C                   Z-ADD     ARBPOR        AR6POR
098970991013     C                   MOVEL     ARBSV1        AR6SV1
098980991013     C                   MOVEL     ARBSV2        AR6SV2
098990991013     C                   MOVEL     ARBSV3        AR6SV3
099000991013     C                   Z-ADD     ARBVA1        AR6VA1
099010991013     C                   Z-ADD     ARBVA2        AR6VA2
099020991013     C                   Z-ADD     ARBVA3        AR6VA3
099030991013     C                   Z-ADD     ARBIMV        AR6IMV
099040991013     C                   Z-ADD     ARBALI        AR6ALI
099050991013     C                   MOVEL     ARBCEI        AR6CEI
099060991013     C                   Z-ADD     ARBIFT        AR6IFT
099070991013     C                   Z-ADD     ARBFIV        AR6FIV
099080991013     C                   Z-ADD     ARBNFT        AR6NFT
099090991013     C                   Z-ADD     ARBDFT        AR6DFT
099100991013     C                   MOVEL     ARBFIM        AR6FIM
099110040123     C*
099120040123     C* CANCELLO LE VARIE NELL'FIAR7
099130040123    1C     WAR72         IFEQ      'E'
099140040123     C                   MOVEL     '2'           WTRC
099150040123     C     KARB2         SETLL     FIAR7000
099160040123     C     KARB2         READE     FIAR7000                               30
099170040123     C*
099180040123    2C     *IN30         DOWEQ     *OFF
099190040123     C                   DELETE    FIAR7000
099200040123     C     KARB2         READE     FIAR7000                               30
099210040123    2C                   ENDDO
099220040123    1C                   ENDIF
099230040123     C*
099240040123     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
099250040123    1C     WSV(4)        IFNE      ' '
099260040123     C                   Z-ADD     4             C
099270040123     C                   CLEAR                   FIAR7000
099280040123     C                   MOVEL     ARBAAS        AR7AAS
099290040123     C                   MOVEL     ARBLNP        AR7LNP
099300040123     C                   MOVEL     ARBNRS        AR7NRS
099310040123     C                   MOVEL     ARBNSP        AR7NSP
099320040123     C                   MOVEL     '2'           AR7TRC
099330040123    2C     WSV(C)        DOWNE     ' '
099340040123     C                   MOVEL     WSV(C)        AR7SVN
099350040123     C                   MOVEL     WVA(C)        AR7VAN
099360040123     C                   WRITE     FIAR7000
099370040123     C                   ADD       1             C
099380040123    2C                   ENDDO
099390040123    1C                   ENDIF
099400040123
099410990930     C     WAR62         IFEQ      'E'
099420991007     C                   UPDATE    FIAR6000
099430941212     C                   ELSE
099440991013     C                   CLEAR                   AR6ATB
099450991013     C                   Z-ADD     ARBAAS        AR6AAS
099460991013     C                   Z-ADD     ARBLNP        AR6LNP
099470991013     C                   Z-ADD     ARBNRS        AR6NRS
099480991013     C                   Z-ADD     ARBNSP        AR6NSP
099490991013     C                   MOVEL     '2'           AR6TRC
099500991007     C                   WRITE     FIAR6000
099510941212     C                   ENDIF
099520930211     C*
099530991013     C                   UNLOCK    FNARB01L
099540920204     C* RISTAMPO FATTURA
099550070309     c* ristampo fattura immediata se variata(37 on),
099560070309     c*   e se non attiva GEO
099570130321     C***  *IN37         IFEQ      *ON
099580130321     c***  FGSddaSI      andeq     ' '
099590130321     C***                CLEAR                   FNLSB5DS
099600130321     C***                MOVEL     'V'           DB0RIS
099610130321     C***                MOVEL     D48TBO        DB0TBO
099620130321     C***                MOVEL     ARBAAS        DB0AAS
099630130321     C***                MOVEL     ARBLNP        DB0LNP
099640130321     C***                MOVEL     ARBNRS        DB0NRS
099650130321     C***                MOVEL     ARBNSP        DB0NSP
099660130321     C***                CALL      D90PSL
099670130321     C***                PARM                    FNLSB5DS
099680130321     C***                END
099690130411
099700130411     c                   eval      wpda_ndc=arbndc
099710130411     c                   eval      wpda_ddc=arbddc
099720130411     c                   eval      wpda_dcm=arbdcm
099730130411     c* Richiamo routine per invio variazione a pda
099740130411     c                   exsr      sr_pda
099750911212     C*
099760911212     C     ENDRG4        ENDSR
099770920116     C*
099780920116     C*--- CONTROLLI FORMATO5 ----------------------------------------*
099790920117     C     CONTR5        BEGSR
099800920116     C* CODICE BOLLA NUOVO
099810941102    1C     *IN21         IFEQ      *OFF
099820941102     C*
099830941213     C     '?'           SCAN      NEWCBO                                 90
099840941213    2C     *IN90         IFEQ      *ON
099850941213     C                   MOVEL     CODUT         §KUT
099860920116     C                   MOVEL     '3A'          §COD
099870941102     C                   MOVE      *BLANKS       NEWCBO
099880991011     C                   CLEAR                   §KEY
099890920116     C                   CALL      'X§TABER'
099900920116     C                   PARM                    §KUT
099910920116     C                   PARM                    §COD
099920920116     C                   PARM                    §KEY
099930941209     C                   PARM                    §DES
099940941102     C                   MOVEL     §KEY          NEWCBO
099950941102     C                   MOVEL     §DES          DENCBO
099960920116     C                   SETON                                        90
099970920116     C                   GOTO      ENDCT5
099980941102    2C                   END
099990920116     C*
100000920116     C                   MOVEL     '3A'          COD
100010941102     C                   MOVEL(P)  NEWCBO        KEY
100020941212     C                   EXSR      CTABEL
100030920121     C*
100040941212    2C     *IN30         IFEQ      *ON
100050941102     C                   MOVEL     MSG(16)       VIDMSG
100060941102     C                   SETON                                        409028
100070941102     C                   GOTO      ENDCT5
100080941102    2C                   ENDIF
100090920117     C*
100100920116     C* CODICE NUOVO E VECCHIO NON POSSONO ESSERE UGUALI
100110940614     C* SE PGM RICHIAMATO NON CONTROLLO
100120941212    2C  N20OLDCBO        IFEQ      NEWCBO
100130941102     C                   MOVEL     MSG(17)       VIDMSG
100140941102     C                   SETON                                        409028
100150920116     C                   GOTO      ENDCT5
100160941102    2C                   END
100170920116     C*
100180920116     C                   MOVEL     TBLUNI        DS3A
100190920304     C*
100200941102     C                   MOVEL     §3ADES        DENCBO                          DESCRIZIONE
100210941102     C*
100220920116     C* NON POSSO TRASFORMARE IN BOLLA RECUPERO DA NON RECUPERO E VICEV
100230940322     C* SE RICHIAMATO PERMETTO TUTTO
100240941102    2C     *IN20         IFEQ      *OFF
100250940322     C*
100260941102    3C     §3ARBL        IFEQ      'R'
100270941102     C     SAVRBL        ANDNE     'R'
100280941102     C*
100290941102     C     §3ARBL        ORNE      'R'
100300941102     C     SAVRBL        ANDEQ     'R'
100310941102     C                   MOVEL     MSG(18)       VIDMSG
100320941102     C                   SETON                                        409028
100330920116     C                   GOTO      ENDCT5
100340941102    3C                   END
100350941102    2C                   END
100360920128     C*
100370941102     C* NON POSSO FARE VARIAZIONI DI PORTO
100380941212     C                   MOVEL     §3ATB1        NSCTB1            1
100390941212     C                   MOVEL     SAVTB1        OSCTB1            1
100400920116     C*
100410941102    2C     NSCTB1        IFNE      OSCTB1
100420941102     C                   MOVEL     MSG(19)       VIDMSG
100430941213     C                   SETON                                        409028
100440920128     C                   GOTO      ENDCT5
100450941102    2C                   ENDIF
100460150413     c* non ammesso contrasegno se bolle per EXPO
100470151119     c*                  if        §3afca=1 and (arbtc1='Z' or arbtc2='Z')
100480151119     C*                  MOVEL     MSG(159)      VIDMSG
100490151119     C*                  SETON                                        409028
100500151119     C*                  GOTO      ENDCT5
100510151119     c*                  endif
100520000510     C*
100530000510     C* BOLLE LNP POSTE: SOLO SPEDIZIONI FRANCIO
100540131008     C**   LNPNTW        IFEQ      'PPT'
100550131008     C**   NSCTB1        ANDNE     'F'
100560131008     C**                 MOVEL     MSG(30)       VIDMSG
100570131008     C**                 SETON                                        409028
100580131008     C**                 GOTO      ENDCT5
100590131008     C**                 ENDIF
100600000718     C* BOLLE LNP POSTE CON SERIE : NN POSSO TRASFRMARE IN C/S
100610131008     C**   LNPNTW        IFEQ      'PPT'
100620131008     C**   ARBNRS        ANDGT     0
100630131008     C**   §3ARBL        ANDEQ     'C'
100640131008     C**                 MOVEL     MSG(96)       VIDMSG
100650131008     C**                 SETON                                        409028
100660131008     C**                 GOTO      ENDCT5
100670131008     C**                 ENDIF
100680000925     C* BOLLE LNP POSTE: NON POSSO FARE BOLLE DOPPIE
100690131008     C**   LNPNTW        IFEQ      'PPT'
100700131008     C**   §3ATB2        ANDNE     '  '
100710131008     C**                 MOVEL     MSG(97)       VIDMSG
100720131008     C*+                 SETON                                        409028
100730131008     C**                 GOTO      ENDCT5
100740131008     C**                 ENDIF
100750050414     C* Se prepagato, non posso trasformare in C/S
100760050414     C     §3ARBL        ifeq      'C'
100770050414     c     war6          andeq     'E'
100780050414     c     ar6dft        andgt     0
100790060213     c     d48cvb        andne     'KM'
100800050414     C                   MOVEL     MSG(133)      VIDMSG
100810050414     C                   SETON                                        409028
100820050414     C                   GOTO      ENDCT5
100830050414     C                   ENDIF
100840950407     C*
100850950407     C* MEMORIZZO VARIAZIONE CODICE BOLLA
100860950407     C     NEWCBO        IFNE      SAVCBO
100870950407     C                   MOVE      *BLANKS       COM01A
100880950407     C                   MOVEL     NEWCBO        SAVCBO
100890950407     C                   END
100900941102    1C                   ENDIF
100910920120     C*
100920920121     C* CONTROLLO CONTRASSEGNO
100930941102    1C     §3AFCA        IFEQ      0
100940941213     C     VIDCAS        ANDNE     0
100950941213     C                   MOVEL     MSG(20)       VIDMSG
100960941213     C                   SETON                                        429028
100970941213     C                   GOTO      ENDCT5
100980941213    1C                   ENDIF
100990961204     C* NON POSSO INSERIRE IL C/ASSEGNO PER BOLLE DI RESO
101000961204    1C     §3AFCA        IFGT      0
101010980105     C     ARBFBR        IFEQ      'S'
101020961204     C                   MOVEL     MSG(73)       VIDMSG
101030961204     C                   SETON                                        429028
101040961204     C                   GOTO      ENDCT5
101050961204    1C                   ENDIF
101060980105     C* NON POSSO INSERIRE IL C/A SE BOLLE LEGATA CON LNP=LNA
101070980105     C  N21KARB          CHAIN     FNLBL01L                           30
101080980105    4C  N21*IN30         IFEQ      *OFF
101090980105     C     LBLLAP        ANDEQ     LBLLAN
101100980105     C                   MOVEL     MSG(78)       VIDMSG
101110980105     C                   SETON                                        429028
101120980105     C                   GOTO      ENDCT5
101130980105    1C                   ENDIF
101140980105     C                   ENDIF
101150941102     C*
101160941213     C* SE eliminato importo C/A, elimiare anche tutti i campi relativi
101170011018    1C     VIDCAS        IFEQ      0
101180011018    2C     VIDTIC        IFNE      *BLANKS
101190011018     C     VIDGCA        ORNE      '  '
101200011018     C                   MOVEL     MSG(56)       VIDMSG
101210011018     C                   SETON                                        419028
101220011018     C                   GOTO      ENDCT5
101230011018    2C                   END
101240011018    1C                   END
101250941102     C*
101260931118     C* IMPOSSIBILE ELIMINARE O VARIARE C/A SE GIA' INCASSATO
101270941220     C     SAVFCA        IFNE      0
101280941220     C     ARBICC        ANDNE     ' '
101290941220     C     ARBICC        ANDNE     'R'
101300941220     c*
101310941102    2C     VIDCAS        IFNE      AR9CAS
101320941102     C     VIDTIC        ORNE      AR9TIC
101330941102     C     VIDVCA        ORNE      AR9VCA
101340941102     C     VIDGCA        ORNE      AR9GCA
101350941102     C                   MOVEL     MSG(22)       VIDMSG
101360941102     C                   SETON                                        429028
101370931118     C                   GOTO      ENDCT5
101380941102    2C                   ENDIF
101390941102    1C                   ENDIF
101400920120     C*
101410920204     C* CAUSALE KB NON CONTROLLA ESISTENZA O MENO DEL C/A: OK TUTTO
101420941102    1C     VIDCVR        IFNE      'KB'
101430931227     C*
101440941102     C* SE NON RICHIAMATO NON POSSO USARE CERTI TIPI BOLLA
101450971229     C* NON CONTROLLO PER VARIAZIONE SOLO DI IMPORTO
101460971229     C  N21
101470971229    2CANN20§3AUCB        IFEQ      'N'
101480941102     C                   MOVEL     MSG(16)       VIDMSG
101490941102     C                   SETON                                        409028
101500931227     C                   GOTO      ENDCT5
101510941102    2C                   END
101520920204     C*
101530941102    2C     §3AFCA        IFNE      0
101540941212     C     VIDCAS        ANDEQ     0
101550941102     C                   MOVEL     MSG(21)       VIDMSG
101560941102     C                   SETON                                        429028
101570920120     C                   GOTO      ENDCT5
101580941102    2C                   END
101590941102    1C                   END
101600941213     C**
101610941213     C* SE IMMESSO IMPORTO C/A , CONTROLLO I CAMPI RELATIVI
101620941213     C**
101630941213    1C     VIDCAS        IFGT      0
101640950120     C* SE ACCESO INDICATORE DI VARIAZIONE C/ASS LO SALVO
101650950120     C     *IN95         IFEQ      *ON
101660950120     C                   MOVEL     '1'           WIN95             1
101670950120     C                   ENDIF
101680140515     c                   clear                   trulintds
101690140515     c                   eval      iinttip='B'
101700140515     c                   eval      iinttic=vidtic
101710140515     c                   eval      iintrscm=v1crsm
101720140515     c                   eval      iintrsco=arbrmo
101730140515     c                   eval      IINTAAS=d48aas
101740140515     c                   eval      IINTLNP=d48lnp
101750140515     c                   eval      IINTNRS=d48nrs
101760140515     c                   eval      IINTNSP=d48nsp
101770140515     c                   call      'TRULINTR'
101780140515     c                   parm                    trulintds
101790950120     C*
101800920204     C* CONTROLLO TIPO INCASSO C/A
101810060705     C* richiamo pgm TRULTICR
101820060705     c                   clear                   trulticds
101830060705     c                   clear                   ds1a
101840060705     c                   eval      iticTBO='A'
101850060705     c                   eval      iticlnp=arblnp
101860060705     c                   eval      iticlna=arblna
101870060705     c                   eval      iticlnantw=lnantw
101880060705     c                   eval      iticnzd=arbnzd
101890090707     c                   eval      iticprd=arbprd
101900090707     c                   eval      iticcad=arbcad
101910090707     c                   eval      iticlod=arblod
101920090707     c                   eval      iticind=arbind
101930090707     c                   eval      iticrsd=arbrsd
101940060705     c                   eval      iticdsp=arbdsp
101950130827    3C***  ARBCCM        IFGT      0
101960130827     C***                MOVEL     ARBCCM        iticccm
101970130827     C***                ELSE
101980130827     C***                MOVEL     ARBKSC        iticccm
101990130827    3C***                END
102000130827     c* in iticccm passo il MITTENTE: CCM se assegnato, KSC se franco
102010130827     c                   if        flgtb1='F'
102020130827     c                   z-add     arbksc        iticccm
102030130827     c                   else
102040130827     c                   z-add     arbccm        iticccm
102050130827     c                   if        iticccm=0
102060130827     c                   z-add     8888          iticccm
102070130827     c                   movel     arblnp        iticccm
102080130827     c                   endif
102090130827     c                   endif
102100140515     c* se il beneficiario del c/a è l'ordinante allora passo questo in iticccm
102110140515     c                   if        ointtipi='R'
102120140515     c                   move      ointcdi       iticccm
102130140515     c                   endif
102140060705     c                   eval      itictic=vidtic
102150060705     c                   eval      iticticbol=ar9tic
102160060705     c                   eval      iticdiv=vidvca
102170060705     c                   eval      iticcas=vidcas
102180060705     c                   eval      iticpgm='FNLR48R'
102190060705     c                   call      'TRULTICR'
102200060705     c                   parm                    TRULTICDS
102210060705     c                   parm                    ds1a
102220060705     c
102230060705     c                   if        oticric='T'
102240060705     c                   movel     otictic       vidtic
102250060705     C                   SETON                                        4190
102260060705     C                   GOTO      ENDCT5
102270060705     c                   endif
102280060705     c                   if        oticric='D'
102290060705     c                   movel     oticdiv       vidvca
102300060705     C                   SETON                                        4390
102310060705     C                   GOTO      ENDCT5
102320060705     c                   endif
102330060705     c* Errore tipo incasso
102340091211     c                   if        oticerr='T' or oticerr='C'
102350060705     C                   MOVEL     oticmsg       VIDMSG
102360060705     C                   SETON                                        419028
102370060705     C                   GOTO      ENDCT5
102380060705    2C                   ENDIF
102390060705     c* Errore divisa
102400060705     c                   if        oticerr='D'
102410060705     C                   MOVEL     oticmsg       VIDMSG
102420060705     C                   SETON                                        439028
102430060705     C                   GOTO      ENDCT5
102440060705    2C                   ENDIF
102450011010      *
102460941102     C*
102470941102     C* SE HO MODIFICATO LA VALUTA DEVO RIDIGITARE IL C/A
102480941102     C* INDICATORE DI CHANGE 95 ON
102490941213    2C     VARVCA        IFNE      VIDVCA
102500950120     C     WIN95         ANDEQ     *BLANKS
102510950120     C*
102520941219     C     VIDVCA        IFNE      *BLANKS
102530941219     C     VIDCAS        ORNE      0
102540941102     C                   MOVEL     MSG(28)       VIDMSG
102550941215     C                   SETON                                        429028
102560941102     C                   GOTO      ENDCT5
102570941213    2C                   ENDIF
102580941219    2C                   ENDIF
102590941213     C*
102600011206
102610011206      * pulisco il flag x la forzatura c/assegno forzabile
102620011206     c                   if        vidcas <> sav1_vidcas
102630011206     c                   clear                   forzacas
102640011206     c                   endif
102650011206      *-------------------
102660011206  2b c                   if        vidcas <> 0
102670011206     c                   eval      sav1_vidcas = vidcas
102680011206      * Controllo il c/assegno tramite il trul21r
102690011206     c                   clear                   trul21ds
102700011206     c                   eval      i21tla = 'L'
102710011206     c                   if        *in21 = *off
102720011206     c                   eval      i21cbo = newcbo
102730011206     c                   else
102740011206     c                   eval      i21cbo = arbcbo
102750011206     c                   endif
102760011206     c                   eval      i21tsp = arbtsp
102770011206     c                   eval      i21lnp = arblnp
102780011206     c                   eval      i21nzm = arbnzm
102790011206     c                   eval      i21lna = arblna
102800011206     c                   eval      i21nzd = arbnzd
102810130827     c*****              eval      i21ksc = arbksc
102820011206     c                   movel     arbctr        i21ctr
102830011206     c                   eval      i21tic = vidtic
102840011206     c                   eval      i21imp = vidcas
102850011206     c                   eval      i21div = vidvca
102860011206     c                   eval      i21ute = knmus
102870011206     c                   eval      i21pgm = nompgm
102880130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
102890130827     c                   if        flgtb1='F'
102900130827     c                   z-add     arbksc        i21ksc
102910130827     c                   else
102920130827     c                   z-add     arbccm        i21ksc
102930130827     c                   if        i21ksc=0
102940130827     c                   z-add     8888          i21ksc
102950130827     c                   movel     arblnp        i21ksc
102960130827     c                   endif
102970130827     c                   endif
102980011206     c                   call      'TRUL21R'
102990011206     c                   parm                    trul21ds
103000011206      * controllo che non sia minore del limite previsto
103010011206  3b c                   if        o21fmn <> *blanks
103020011206     c                   eval      vidmsg = msg(108)
103030011210     c                   eval      %subst (vidmsg:31:14) = (%editc(o21lmn: '4'))
103040011206     c                   eval      *in42 = *on
103050011206     c                   eval      *in90 = *on
103060011206     c                   eval      *in28 = *on
103070011206     c                   goto      endct5
103080011206  3e c                   endif
103090011206      * controllo che non sia maggiore del limite previsto
103100011206      * non forzabile
103110011206  3b c                   if        o21fx2 <> *blanks
103120011206     c                   eval      vidmsg = msg(91)
103130011210     c                   eval      %subst (vidmsg:47:14) = (%editc(o21lx2: '4'))
103140011206     c                   eval      *in42 = *on
103150011206     c                   eval      *in90 = *on
103160011206     c                   eval      *in28 = *on
103170011206     c                   goto      endct5
103180011206  3e c                   endif
103190050301      * controllo che non sia maggiore del limite per bancanri bartolini no
103200050301      * abilitati da sede, non forzabile
103210050301  3b c                   if        o21fb <> *blanks
103220050301     c                   eval      vidmsg = msg(132)
103230050301     c                   eval      %subst (vidmsg:26:14) = (%editc(o21lb: '4'))
103240050301     c                   eval      *in42 = *on
103250050301     c                   eval      *in90 = *on
103260050301     c                   eval      *in28 = *on
103270050301     c                   goto      endct5
103280050301  3e c                   endif
103290011206      * controllo che non sia maggiore del limite previsto
103300011206      * forzabile
103310011206  3b c                   if        o21fx1 <> *blanks and forzacas = *blanks
103320011206     c                   eval      forzacas = '1'
103330011206     c                   eval      vidmsg = msg(109)
103340011210     c                   eval      %subst (vidmsg:24:14) = (%editc(o21lx1: '4'))
103350011206     c                   eval      *in42 = *on
103360011206     c                   eval      *in90 = *on
103370011206     c                   eval      *in28 = *on
103380011206     c                   goto      endct5
103390011206  3e c                   endif
103400011206  2e c                   endif
103410011206
103420011120      *
103430011120      * Se bolla giacente e modifica 'KA' o 'KP' l'importo immesso (convertito) deve
103440011120      * essere uguale all'originario
103450011120  2b c                   if        *in04 = *ON  and (vidcvr = 'KA' or
103460011120     c                             vidcvr = 'KP')
103470011120     c                   z-add     vidcas        w0173
103480011120      * Cerco i decimali previsti per la divisa originaria
103490011120     c                   movel     'CV'          cod
103500011120     c                   movel(p)  ar9vca        key
103510011120     c                   exsr      ctabel
103520011120     c  n30              movel     tbluni        dscv
103530011120     c   30              clear                   dscv
103540011120      * Controlla la divisa
103550011120  3b c                   if        ar9vca <> vidvca
103560011120      * Converto il contrassegno inserito a video nella divisa originaria
103570030612     c                   clear                   YeurcoDS
103580011120     c                   movel     vidvca        yecdvi
103590011120     c                   movel     ar9vca        yecdvo
103600011120     c                   z-add     vidcas        yecimi
103610011120     c                   move      §cvdec        yecdco
103620011120     c                   call      'YEURCO'
103630030612     c                   parm                    YeurcoDS
103640011120  4b c                   if        yecesi = ' '
103650011120     c                   z-add     yecimo        w0173
103660011120  4e c                   endif
103670011120  3e c                   endif
103680011120      * Calcolo la differenza tra i 2 contrassegni
103690011120     c                   eval      wdiffe = (ar9cas - w0173)
103700011120     c                   mllzo     1             wdiffe
103710020103      * Converto la differenza nella divisa di conto
103720011120  3b c                   if        ar9vca <> §gedcn
103730030612     c                   clear                   YeurcoDS
103740020103     c                   movel     ar9vca        yecdvi
103750020103     c                   movel     §gedcn        yecdvo
103760011120     c                   z-add     wdiffe        yecimi
103770020103     c                   move      decsav        yecdco
103780011120     c                   call      'YEURCO'
103790030612     c                   parm                    YeurcoDS
103800011120  4b c                   if        yecesi = ' '
103810011120     c                   z-add     yecimo        wdiffe
103820011120  4e c                   endif
103830011120  3e c                   endif
103840020103      *
103850020103      * Se differenza maggiore del valore in tabella ERRORE
103860011120  3b c                   if        wdiffe > dfcsav
103870011120     c                   movel     msg(103)      vidmsg
103880011120     c                   seton                                        429028
103890011120     c                   goto      endct5
103900011120  3e c                   endif
103910011120  2e c                   endif
103920011120
103930991021     C**
103940991021     C                   MOVEL     VIDVCA        VARVCA
103950991021     C                   CLEAR                   WIN95
103960941102     c**
103970941102     C* PARTICOLARITA' C/ASSEGNO
103980941102     c**
103990941213    2C     VIDGCA        IFNE      *BLANKS
104000941102     C     VIDGCR        ORNE      *BLANKS
104010941102     C*
104020941102     C* non posso indicarla se NON IMMESSO C/A
104030941213    3C     VIDCAS        IFEQ      0
104040941102     C     VIDGCA        ANDNE     *BLANKS
104050941102     C                   SETON                                        449028
104060941102     C                   MOVEL     MSG(29)       VIDMSG
104070941102     C                   GOTO      ENDCT5
104080941213    3C                   ENDIF
104090941102     C*
104100941102     C* RICHIAMO IL PRG DI RICERCA
104110941213    3C     VIDGCR        IFEQ      '?'
104120030612     C                   CLEAR                   DS7pqrs
104130941102     C                   MOVEL     'S'           DS7RIC
104140941102     C                   MOVEL     'V'           DS7GES
104150941213    4C     ARBCCM        IFGT      0
104160941102     C                   MOVEL     ARBCCM        DS7KSC
104170941102     C                   ELSE
104180941102     C                   MOVEL     ARBKSC        DS7KSC
104190941213    4C                   ENDIF
104200941102     C                   MOVEL     VIDGCA        DS7COD
104210030612     C                   MOVEL     DS7pqrs       KPJBU
104220941102     C                   CALL      'TRTB72R'
104230941102     C                   PARM                    KPJBA
104240030612     C                   MOVEL     KPJBU         DS7pqrs
104250941102     C*
104260941102     C     VIDGCA        IFEQ      *BLANKS
104270941102     C                   MOVEL     DS7COD        VIDGCA
104280941213    3C                   ENDIF
104290941102     C*
104300941102     C                   CLEAR                   VIDGCR
104310941102     C*
104320941102     C                   SETON                                        90
104330941102     C                   GOTO      ENDCT5
104340941102    2C                   ENDIF
104350941102     C* CONTROLLO
104360030612     C                   CLEAR                   DS7pqrs
104370941102     C                   MOVEL     'S'           DS7RIC
104380941102     C                   MOVEL     'C'           DS7GES
104390941213    2C     ARBCCM        IFGT      0
104400941102     C                   MOVEL     ARBCCM        DS7KSC
104410941102     C                   ELSE
104420941102     C                   MOVEL     ARBKSC        DS7KSC
104430941102     C                   ENDIF
104440941102     C                   MOVEL     VIDGCA        DS7COD
104450030612     C                   MOVEL     DS7pqrs       KPJBU
104460941102     C                   CALL      'TRTB72R'
104470941102     C                   PARM                    KPJBA
104480030612     C                   MOVEL     KPJBU         DS7pqrs
104490941102     C* ERRORE
104500941213    3C     DS7ERR        IFEQ      '1'
104510941102     C                   MOVEL     DS7MSG        VIDMSG
104520941102     C                   SETON                                        449028
104530941102     C                   GOTO      ENDCT5
104540941213    3C                   ENDIF
104550941102     C*
104560941213    2C                   ENDIF
104570941213    1C                   ENDIF
104580011010      *-------------------
104590941220     C*
104600941220     c* SE MODIFICATO IK TIPO BOLLA ED ELIMINATA LA 2 MA GIA' INCASS
104610941220     C*  ERRORE
104620941220     C     §3ATB2        IFEQ      *BLANKS
104630941220     C*
104640941220     C     SAVTB2        ANDNE     *BLANKS
104650941220     C     ARBICA        ANDNE     ' '
104660941220     C     ARBICA        ANDNE     'R'
104670991007     C     DFT2BO        ANDGT     0
104680941220     C                   MOVEL     MSG(59)       VIDMSG
104690941220     C                   SETON                                        409028
104700941220     C                   GOTO      ENDCT5
104710941220     C                   END
104720950407     C* SE CAUSALE VARIAZIONE CODICE BOLLA
104730950407    1C     *IN21         IFEQ      *OFF
104740950407     C     COM01A        ANDEQ     *BLANKS
104750950407     C* E NON TROVATI ERRORI BLOCCANTI
104760950407     C     *IN90         ANDEQ     *OFF
104770950407     C* E ASSEGNATO
104780950407     C     FLGTB1        ANDEQ     'A'
104790950407     C* SE AGGIUNTO O TOLTO IL CONTRASSEGNO
104800950407    2C     SAVFCA        IFEQ      *ZEROS
104810950407     C     §3AFCA        ANDNE     *ZEROS
104820950407     C     SAVFCA        ORNE      *ZEROS
104830950407     C     §3AFCA        ANDEQ     *ZEROS
104840991007     C                   MOVEL     '1'           WTRC
104850991007     C     KARB2         CHAIN(N)  FIAR6000                           30
104860950407    3C     *IN30         IFEQ      *OFF
104870950407     C* SE GIA' TASSATO FINO ALL'IMPONIBILE
104880950407     C     AR6IMV        ANDNE     *ZEROS
104890950407     C                   SETON                                        2892
104900950407     C* ERRORE NON BLOCCANTE:MANCA VARIA G
104910950407    4C     §3AFCA        IFNE      *ZEROS
104920950407     C                   MOVEL     MSG(65)       VIDMSG
104930950407     C                   ELSE
104940950407     C* ERRORE NON BLOCCANTE:C'E' VARIA G
104950950407     C                   MOVEL     MSG(66)       VIDMSG
104960950407    4C                   END
104970950407    3C                   END
104980950407     C*
104990950407    2C                   END
105000950407     C                   MOVE      '1'           COM01A            1
105010950407    1C                   END
105020920116     C*
105030920116     C     ENDCT5        ENDSR
105040920116     C*
105050920116     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE KX -----------------*
105060920129     C     REGIS5        BEGSR
105070991001     C                   SETOFF                                       903839
105080130412     c                   clear                   wpda_ndc
105090130412     c                   clear                   wpda_ddc
105100130412     c                   clear                   wpda_dcm
105110071116     C
105120950104     C*  GIRO UDATE
105130950104     C                   TIME                    W0140            14 0
105140950104     C* UDATE IN GGMMAAAA
105150950104     C                   MOVE      W0140         WDTGIO            8 0
105160950104     C*
105170950104     C* UDATE IN AAAAMMGG
105180950104     C                   Z-ADD     WDTGIO        G02DAT
105190950104     C                   MOVEL     *BLANK        G02ERR
105200950104     C                   CALL      'XSRDA8'
105210950104     C                   PARM                    WLBDAT
105220950104     C                   MOVEL     G02INV        DATEU             8 0
105230941221     C* PRENDO I VALORI ASSOLUTI
105240941221     C                   MLLZO     1             VIDCAS
105250941221     C*
105260941213     C                   MOVEL     KNMUS         ARBPRU
105270941220     C                   MOVEL     ARBCPI        SAVCPI
105280070515     c
105290070515     c* Se modificato il C/A, devo allocarmi record "L" per il
105300070515     c*  blocco telefonata C/A
105310070515     c                   clear                   wesar4L           1
105320070515    1C     vidCAS        IFNE      SAVCAS
105330070515     C     vidTIC        ORNE      SAVTIC
105340070515     C     vidVCA        ORNE      SAVVCA
105350070807     C     vidcas        andgt     0
105360070515     c                   eval      Tipoela='C'
105370070515     c                   exsr      GestAR4L
105380070515    2c                   if        *in91
105390070515     C                   GOTO      ENDRG5
105400070515    2c                   endif
105410070515    1c                   endif
105420070515     c
105430070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
105440070515     c* iornare direttamente il file solo se cambiato tipo bolla
105450070515     c                   clear                   waltrabolla       1
105460130412     c                   setoff                                       3091
105470130412     c                   if        oldcbo<>newcbo
105480070515     C   10KARB          CHAIN     FNBLP01L                           3091
105490070515     C  n10KARB          CHAIN     FNARB01L                           3091
105500130412     c                   else
105510130412     c* questa chain mi serve per l'invio dati a PDA
105520131120     c                   if        d66pda<>' '
105530130412     C  n10KARB          CHAIN(n)  FNARB01L                           30
105540130412     c                   endif
105550130412     c                   endif
105560070515     c*
105570070515     c                   if        *in91
105580070515     C                   SETON                                        28
105590070515     C                   MOVEL     MSG(4)        VIDMSG
105600070515     c                   unlock    fiar401l
105610070515     C                   GOTO      ENDRG5
105620070515     c                   else
105630070515     c                   if        *in30
105640070515     c                   eval      waltrabolla='N'
105650070515     c                   else
105660130411     c  n10              eval      wpda_ndc=arbndc
105670130411     c  n10              eval      wpda_ddc=arbddc
105680130411     c  n10              eval      wpda_dcm=arbdcm
105690130412     c                   if        oldcbo<>newcbo
105700070515     C   10KARB          CHAIN     FNARB01L                           30
105710130412     c                   endif
105720131120     c                   if        oldcbo<>newcbo or d66pda<>' '
105730070515     C  N10KARB          CHAIN     FNBLP01L                           30
105740070515     c* Alla fine per non sporcarmi i campi del record, richaino il
105750070515     c*  record originale che sto variando
105760130411     c   10              eval      wpda_ndc=arbndc
105770130411     c   10              eval      wpda_ddc=arbddc
105780130411     c   10              eval      wpda_dcm=arbdcm
105790130412     c                   endif
105800070515     c                   endif
105810070515     c                   endif
105820130412     c*****              endif
105830920123     C*********
105840920116     C* INVIO PER SEDE
105850170116     C*****              MOVEL     'FNARBK0T'    SFILE
105860920116     C* ALLOCO OGGETTI
105870170116     C***                EXSR      ALLOC
105880170116     c***                if        *in91
105890170116     c***                unlock    fiar401l
105900170116     c** 10              unlock    fnblp01l
105910170116     c**n10              unlock    fnarb01l
105920170116     C**                 GOTO      ENDRG5
105930170116     c****               endif
105940930511     C*
105950941212     C* INVIO SOLO IN SEDE L'INDIRIZZO DEL MITTENTE PER C/A
105960941212     C* INVIO SOLO IN SEDE L'INDIRIZZO DEL DESTINATARIO SE DOPPIA BOLLA
105970930511     C                   MOVEL     'S'           §7LFFI
105980930511     C*
105990930210     C********* PER  C O N T R A S S E G N O
106000170116    1C**   AR9CAS        IFEQ      0
106010170116     C**   VIDCAS        ANDNE     0
106020170116     C**                 SETON                                        38
106030930210     C* INVIO PER SEDE
106040170116     C**                 MOVEL     'FNARBM0T'    SFILE
106050930210     C* ALLOCO OGGETTI
106060170116     C**                 EXSR      ALLOC
106070070515     c
106080170116     c***                if        *in91
106090170116     C**                 MOVEA     'FNARBK0T'    C21(13)
106100170116     C**                 MOVEA     VAR           C21(41)
106110170116     C**                 MOVEL     *BLANKS       COMMAN           80
106120170116     C**                 MOVEA     C21           COMMAN
106130170116     C**                 CALL      'QCMDEXC'                            H1
106140170116     C**                 PARM                    COMMAN
106150170116     C**                 PARM                    LUNG
106160170116     c**                 unlock    fiar401l
106170170116     c** 10              unlock    fnblp01l
106180170116     c**n10              unlock    fnarb01l
106190070515     c
106200170116     C**                 GOTO      ENDRG5
106210170116    1C**                 END
106220170116    1C**                 END
106230950324     C*
106240930210     C********* PER  D O P P I A    B O L L A
106250170116    1C**   SAVTB2        IFEQ      '  '
106260170116     C**   §3ATB2        ANDNE     '  '
106270170116     C**                 SETON                                        39
106280930210     C* INVIO PER SEDE
106290170116     C**                 MOVEL     'FNARBD0T'    SFILE
106300930210     C* ALLOCO OGGETTI
106310170116     C**                 EXSR      ALLOC
106320170116     c**                 if        *in91
106330170116     C**                 MOVEA     'FNARBK0T'    C21(13)
106340170116     C**                 MOVEA     VAR           C21(41)
106350170116     C**                 MOVEL     *BLANKS       COMMAN           80
106360170116     C**                 MOVEA     C21           COMMAN
106370170116     C**                 CALL      'QCMDEXC'                            H1
106380170116     C**                 PARM                    COMMAN
106390170116     C**                 PARM                    LUNG
106400070515     c
106410170116     C**                 MOVEA     'FNARBM0T'    C21(13)
106420170116     C**                 MOVEL     *BLANKS       COMMAN           80
106430170116     C**                 MOVEA     C21           COMMAN
106440170116     C**                 CALL      'QCMDEXC'                            H1
106450170116     C***                PARM                    COMMAN
106460170116     C**                 PARM                    LUNG
106470170116     c**                 unlock    fiar401l
106480170116     c** 10              unlock    fnblp01l
106490170116     c**n10              unlock    fnarb01l
106500170116     C**                 GOTO      ENDRG5
106510170116     C**                 ENDIF
106520170116     C**                 ENDIF
106530041022     C*********
106540041022     C                   Z-ADD     DATEU         ARBDTV
106550060320     C                   TIME                    ARBORV
106560920116     C* STORICIZZAZIONE ALL'ARRIVO
106570060130    1C     D66FSA        ifeq      'S'
106580920116     C                   MOVEL     VIDCVR        ARBCVB
106590941213     C                   MOVEL     OLDCBO        ARBCBP
106600941213     C                   MOVEL     NEWCBO        ARBCBN
106610941213     C                   MOVEL     AR9TIC        ARBTIC
106620941213     C                   MOVEL     AR9CAS        ARBCAS
106630941213     C                   MOVEL     AR9VCA        ARBVCA
106640941213     C                   MOVEL     AR9GCA        ARBGCA
106650011108      *
106660011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
106670011108      * è in partenza 'P' o in arrivo 'A'
106680011108     C     *IN10         IFEQ      *ON
106690011108     C                   MOVE      'A'           ARBPRU
106700011108     C                   ENDIF
106710011108     C     *IN10         IFEQ      *OFF
106720011108     C                   MOVE      'P'           ARBPRU
106730011108     C                   ENDIF
106740011108      *
106750941014     C                   WRITE     FNARBK00
106760930210     C                   END
106770060213     c* storicizzazione motivazione
106780060213     c     d66mtv        ifeq      'S'
106790060301     c                   exsr      sto_mtv
106800060213     c                   endif
106810920116     C*
106820920123     C*********
106830920117     C* I N V I O
106840941102     C                   MOVEL     NEWCBO        ARBCBN
106850941213     C* PER IL FILE DI VARIAZIONE
106860941102     C                   MOVEL     VIDCAS        ARBCAS
106870920116     C                   MOVEL     VIDTIC        ARBTIC
106880941212     C                   MOVEL     VIDGCA        ARBGCA
106890941213     C*
106900941215     C*
106910941212     C                   MOVEL     VIDVCA        ARBVCA
106920011017      * Se l'importo del contrassegno è 0 non devo scrivere la divisa
106930011017     C     VIDCAS        IFEQ      *ZEROS
106940011017     C                   CLEAR                   ARBVCA
106950011017     C                   ENDIF
106960920123     C*********
106970041022     C     D66FFI        ifeq      'S'
106980920117     C                   TIME                    ARBORV
106990920127     C                   MOVEL     VIDCVR        ARBCVB
107000170116     C**                 OPEN      FNARBK0T
107010941014     C                   WRITE     FNARBKTT
107020170116     C**                 CLOSE     FNARBK0T
107030920117     C* DISALLOCO
107040170116     C**                 MOVEA     'FNARBK0T'    C21(13)
107050170116     C**                 MOVEA     VAR           C21(41)
107060170116     C**                 MOVEL     *BLANKS       COMMAN           80
107070170116     C**                 MOVEA     C21           COMMAN
107080170116     C**                 CALL      'QCMDEXC'                            H1
107090170116     C**                 PARM                    COMMAN
107100170116     C**                 PARM                    LUNG
107110920212     C*
107120170116     C** 38              DO
107130170116     C**                 OPEN      FNARBM0T
107140170116     C**                 WRITE     FNARBMTT
107150170116     C**                 CLOSE     FNARBM0T
107160920212     C* DISALLOCO
107170170116     C**                 MOVEA     'FNARBM0T'    C21(13)
107180170116     C**                 MOVEA     C21           COMMAN
107190170116     C**                 CALL      'QCMDEXC'                            H1
107200170116     C**                 PARM                    COMMAN
107210170116     C**                 PARM                    LUNG
107220170116     C**                 END
107230920213     C*
107240170116     C** 39              DO
107250170116     C**                 MOVEL     *BLANKS       ARBCPI
107260170116     C**                 ADD       1             ARBORV
107270170116     C**                 MOVEL     VIDCVR        ARBCVB
107280170116     C**                 MOVEL     NEWCBO        ARBCBO
107290170116     C**                 OPEN      FNARBD0T
107300170116     C**                 WRITE     FNARBDTT
107310170116     C**                 CLOSE     FNARBD0T
107320920213     C* DISALLOCO
107330170116     C**                 MOVEA     'FNARBD0T'    C21(13)
107340170116     C**                 MOVEA     C21           COMMAN
107350170116     C**                 CALL      'QCMDEXC'                            H1
107360170116     C**                 PARM                    COMMAN
107370170116     C**                 PARM                    LUNG
107380170116     C**                 END
107390930210     C                   END
107400920117     C*
107410920117     C* A G G I O R N O   F I L E
107420920117     C*
107430051118     c* se variato imposto la data ultima variazione
107440091217    1c                   if        vidvca<>ar9vca or
107450051118     c                             ar9cas<>vidcas or
107460051118     c                             ar9vca<>vidvca or
107470051118     c                             ar9gca<>vidgca
107480051118     c                   movel     dateu         ar9duv
107490091217    1c                   endif
107500091217     c
107510091217     c* Se variato il tipo incasso C/Ass, lo memorizzo
107520140516     c* non lo memorizzo in caso di annullamento altrimenti su internet viene
107530140516     c* visualizzata sia la variazione che l'annullamento
107540140516    1c                   if        savtic<>vidtic and vidcas>0
107550091217     c                   eval      Wtipoagg='TIC'
107560091217     c                   exsr      RegisAR5GEN
107570091217    1c                   EndIf
107580091217     c
107590091217     c
107600941212     C                   MOVEL     NEWCBO        ARBCBO
107610941212     C                   MOVEL     VIDCAS        AR9CAS
107620941212     C                   MOVEL     VIDTIC        AR9TIC
107630941212     C                   MOVEL     VIDGCA        AR9GCA
107640941212     C                   MOVEL     VIDVCA        AR9VCA
107650011017      * Se l'importo del contrassegno è 0 non devo scrivere la divisa
107660011017     C     VIDCAS        IFEQ      *ZEROS
107670011017     C                   CLEAR                   AR9VCA
107680011017     C                   ENDIF
107690011019      *
107700941220     C                   MOVEL     ARBCPI        SAVCPI
107710920121     C*
107720941220     C* E L I M I N A T A   L A   2   B O L L A
107730941220    1C     SAVTB2        IFNE      '  '
107740920603     C     §3ATB2        ANDEQ     '  '
107750941220     C* PULISCO PARTITA IVA
107760930210     C                   MOVEL     *BLANKS       ARBCPI
107770991001     C* SE C'ERA AR6 SECONDA BOLLA LO DELETO
107780990930     C     WAR62         IFEQ      'E'
107790991008     C                   MOVEL     '2'           WTRC
107800170419     C     KARB2         CHAIN(N)  FIAR601l
107810170419     c                   if        %found(fiar601l) and ar6nft>0
107820170419     c                   exsr      sr_fnfta
107830170419     c                   endif
107840991008     C     KARB2         DELETE    FIAR6000                           30
107850941220     C                   ENDIF
107860941220     C*
107870941220     C* SE GIA' CONSEGNATA DO' PER INCASSATO L'ASSEGNATO
107880000925    2C     ARBDCM        IFGT      0
107890000925    3C     ARBICA        IFEQ      'R'
107900000925     C     ARBICA        OREQ      ' '
107910941220     C                   MOVEL     'Y'           ARBICA
107920000925    4C     ARBICC        IFNE      'R'
107930970613     C                   EXSR      CHIUBO
107940000925    5C     ARBNMI        IFNE      8888888
107950971023     C                   MOVEL     'S'           ARBICA
107960000925    5C                   ENDIF
107970000925    4C                   ENDIF
107980000925    3C                   ENDIF
107990000925    2C                   ENDIF
108000941220     C* ABILITO L'ASSEGNATO
108010941220     C                   MOVEL     'S'           ARBACA
108020941220    1C                   ENDIF
108030941220     C*
108040941220     C* A G G I U N T A   2   B O L L A
108050941220     C     SAVTB2        IFEQ      '  '
108060941220     C     §3ATB2        ANDNE     '  '
108070941220     C* PULISCO PARTITA IVA
108080941220     C                   MOVEL     *BLANKS       ARBCPI
108090941220     C*
108100941220     C                   MOVEL     ' '           ARBACA
108110941220     C                   MOVEL     ' '           ARBICA
108120941220     C* SE GIA' CONSEGNATA SI TRATTA DI UN RITORNO ALL'INCASSO
108130941220     C     ARBDCM        IFGT      0
108140941220     C                   MOVEL     'R'           ARBICA
108150941220     C                   ENDIF
108160941220     C                   ENDIF
108170920120     C*
108180920117     C* ABILITAZIONE CONTRASSEGNO
108190950918     C* SE NON HO VARIATO IL C/A RISPETTO A PRIMA, LASCIO ABILITAZIONE
108200950918     C*  E INCASSO C/A COME ERANO
108210950918    1C     AR9CAS        IFNE      SAVCAS
108220950918     C     AR9TIC        ORNE      SAVTIC
108230950918     C     AR9VCA        ORNE      SAVVCA
108240920120     C                   MOVEL     ' '           ARBACC
108250070320     c                   eval      §b4ltca='S'
108260070320     c
108270941220     C*
108280920117     C* SE NON C'E' PIU' IL C/ASSEGNO ABILITO
108290950918    2C     §3AFCA        IFEQ      0
108300950104     C     AR9CAS        OREQ      0
108310920117     C                   MOVEL     'S'           ARBACC
108320070320     c                   eval      §b4ltca=' '
108330960704     C*
108340960909     C* SE IL CONTRASSEGNO E' UN RITORNO ALL'INCASSO, LO TOLGO
108350960704     C* SE CONSEGNATO, E RITORNO ALL'INCASSO LO DO PER INCASSATO
108360960704    3C     ARBDCM        IFGT      0
108370960704     C     ARBICC        ANDEQ     'R'
108380960704     C                   MOVEL     'Y'           ARBICC
108390970613     C* SE L'ASSEGNATO NON E' UN RITORNO ALL'INCASSO VEDO SE CHIUDERE
108400970613     C* LA BOLLA
108410970613     C     ARBICA        IFNE      'R'
108420970613     C                   EXSR      CHIUBO
108430971023     C     ARBNMI        IFNE      8888888
108440971023     C                   MOVEL     'S'           ARBICC
108450971023     C                   ENDIF
108460970613     C                   ENDIF
108470970613     C                   ENDIF
108480960704     C*
108490950918   X2C                   ELSE
108500941220     C*
108510941220     C* SE LA BOLLA E' GIA' CONSEGNA METTO "R" DI RITORNO ALL'INCASSO
108520950918    3C     ARBDCM        IFGT      0
108530941220     C                   MOVEL     'R'           ARBICC
108540950918   X3C                   ELSE
108550941220     C* ALTRIMENTI PULISCO IL FLAG DI INCASSO C/A
108560941220     C                   CLEAR                   ARBICC
108570950918    3C                   ENDIF
108580011031      *
108590070320    3c                   if        o21fca = *blanks
108600020103     c                   eval      arbacc = 'S'
108610070320     c                   clear                   §b4ltca
108620070320    3c                   endif
108630070320    2C                   END
108640070320     c
108650070320     c* Aggiorno record di FIar4
108660070320     c                   eval      TipoEla='A'
108670070320     c                   exsr      GestAR4L
108680070320    1C                   END
108690071116     c*
108700071116     C* M O D I F I C A T O   S O L O   1 °   T I P O   B O L L A
108710071116    1C     SAVTB2        IFEQ      '  '
108720071116     C     §3ATB2        ANDEQ     '  '
108730071116     C* PRENDI IL TIPO BOLLA DEL NUOVO CODICE
108740071116     C                   MOVEL     'TB'          COD
108750071116     C                   MOVEL(P)  §3atb1        KEY
108760071116     C                   EXSR      CTABEL
108770071116     C  N30              MOVEL     TBLUNI        DSTB
108780071116     C   30              CLEAR                   DSTB
108790071116     c                   movel     §tbfcb        NEWtb1FCB         1
108800071116     c
108810071116     C* Se il nuovo tipo bolla è da non contabilizzare, imposto
108820071116     c*  la "S" di abilitazione Assgnato
108830071116    2c                   if        ARBACA=' ' and NEWtb1FCB='0'
108840071116     c                   eval      arbaca='S'
108850071116    2c                   endif
108860071116     c
108870071116     c* Se il nuovo tipo bolla è da contabilizzare, ma non lo era il
108880071116     c*  vecchio, vedo se devo togliere la "S" per l'abilitazione del C/A
108890071116    2c                   if        NEWtb1FCB='1' and Newtb1FCB<>OLDtb1FCB
108900071116     c                             and arbaca='S'
108910071116     C* TASSAZIONE IN FIAR6
108920071116     C                   MOVEL     '1'           WTRC
108930071116     C     KARB2         CHAIN(N)  FIAR6000
108940071116    3c                   if        (%subst(%editc(arbksc: 'X'):4:4)='9999') and
108950071116     c                             (not %found(FIar601l) or ar6dft=0)
108960071116     c                   eval      arbaca=' '
108970071116    3c                   endif
108980071116    2c                   endif
108990170418     c* se il nuovo tipo bolla non è da contabilizzare e il vecchio lo era
109000170418     c* memorizzo eventuale fattura stornata su file FNFTA00F
109010170418     C* TASSAZIONE IN FIAR6
109020170419     c* solo per bolle in assegnato
109030170418    2c                   if        NEWtb1FCB='0' and Newtb1FCB<>OLDtb1FCB
109040170419     c                             and *in06=*off
109050170418     C                   MOVEL     '1'           WTRC
109060170418     C     KARB2         CHAIN(N)  FIAR6000
109070170418     c                   if        %found(fiar601l) and ar6nft>0
109080170418     c                   exsr      sr_fnfta
109090170418     c                   endif
109100170418     c                   endif
109110071116     c
109120071116    1C                   ENDIF
109130170419
109140170419     C* M O D I F I C A T O   IL  2 °   T I P O   B O L L A
109150170419    1C     SAVTB2        IFne      '  '
109160170419     C     §3ATB2        ANDne     '  '
109170170419     c     savtb2        andne     §3atb2
109180170419     C* PRENDo IL TIPO BOLLA DEL NUOVO CODICE
109190170419     C                   MOVEL     'TB'          COD
109200170419     C                   MOVEL(P)  §3atb2        KEY
109210170419     C                   EXSR      CTABEL
109220170419     C  N30              MOVEL     TBLUNI        DSTB
109230170419     C   30              CLEAR                   DSTB
109240170419     c                   movel     §tbfcb        NEWtb2FCB         1
109250170419     c* se il nuovo tipo bolla non è da contabilizzare e il vecchio lo era
109260170419     c* memorizzo eventuale fattura stornata su file FNFTA00F
109270170419     C* TASSAZIONE IN FIAR6
109280170419     c* solo per bolle in assegnato
109290170419    2c                   if        NEWtb2FCB='0' and Newtb2FCB<>OLDtb2FCB
109300170419     C                   MOVEL     '2'           WTRC
109310170419     C     KARB2         CHAIN(N)  FIAR6000
109320170419     c                   if        %found(fiar601l) and ar6nft>0
109330170419     c                   exsr      sr_fnfta
109340170419     c                   endif
109350170419     c                   endif
109360170419     c                   endif
109370920116     C*
109380941216     C     §3AFCA        IFNE      0
109390950104     C     AR9CAS        ANDGT     0
109400941212     C     WAR9          IFEQ      ' '
109410941212     C                   MOVEL     ARBAAS        AR9AAS
109420941212     C                   MOVEL     ARBLNP        AR9LNP
109430941212     C                   MOVEL     ARBNRS        AR9NRS
109440941212     C                   MOVEL     ARBNSP        AR9NSP
109450051118     c                   movel     dateu         ar9duv
109460051118     C                   WRITE     FiAR9000
109470941212     C                   ELSE
109480051118     C                   UPDATE    FiAR9000
109490941212     C                   ENDIF
109500941216     C*
109510941216     C                   ELSE
109520941216     C* SE MI HANNO CANCELLATO L'AR9 --> DELETO
109530941216     C     WAR9          IFEQ      'E'
109540051118     C     KARB          DELETE    FiAR9000                           30
109550941216     C                   ENDIF
109560941216     C                   ENDIF
109570941212     C*
109580060213     C   10              UPDATE    FNARB000
109590041022     c* Se cambiato codice bolla aggiorno fnblp
109600041022     c                   if        waltrabollA=' ' AND oldcbo<>newcbo
109610041022     c                   eval      arbcbo=newcbo
109620041022     c   10              except    arbkblp
109630041022     c  N10              except    arbkarb
109640041022     c                   endif
109650040224
109660040224      * Se c'è la data consegna richiesta controllo se è ancora valida
109670040224      * solo se ho fatto una modifica in arrivo
109680160114     c                   If        ArbDcr > *Zeros and *In10 and wfile='A'
109690040324     c                             and wCa = *Blanks and wgiac = *Blanks
109700040224     c                   ExSr      ContrDcr
109710040224     c                   EndIf
109720950602     C*
109730130411     c* Richiamo routine per invio variazione a pda
109740130411     c                   exsr      sr_pda
109750950104     C*
109760920117     C     ENDRG5        ENDSR
109770911211     C*
109780911211     C*--- CARICO CODICI TARIFFA -------------------------------------*
109790911211     C     CARCTR        BEGSR
109800020305     c**
109810020305     C* Richiamo trul27r per reperimento WFIE e per reperimento tariffa di
109820020305     C* cartello
109830020305     c**
109840160225     c                   clear                   trul27ds1
109850021121     c                   Eval      I27Pkg = ArbPkf
109860020305     c                   eval      i27tsp = arbtsp
109870020305     c                   eval      i27lna = arblna
109880020305     c                   eval      i27lnp = arblnp
109890020305     C                   eval      i27cli = ksc
109900160225     c                   call      'TRUL27R1'
109910160225     c                   parm                    trul27ds1
109920020305     c                   eval      wfie = o27fie
109930020305     c*
109940941209     C                   MOVEL     *BLANKS       DE2CTR
109950050202     C                   CLEAR                   Fnlv59DS
109960941209     C*
109970050202     C                   MOVEL     D48TBO        ilv59tbo
109980040511      * SE BOLLA MONOVARIA NON PASSO IL TIPO BOLLA
109990040511     C                   IF        §3ASVA <> *BLANK
110000050202     C                   CLEAR                   ilv59tbo
110010050202     c                   eval      ilv59prp='P'
110020040511     C                   ENDIF
110030050202     C                   Z-ADD     ARBDSP        ilv59DSP
110040060131     C                   MOVEL     dutkci        ilv59KCI
110050050202     C                   Z-ADD     KSC           ilv59KSC
110060050202     C                   MOVEL     WFIE          ilv59FIE
110070160225     C                   MOVEL     WFIE          ilv59ta3
110080050202     C                   MOVEL     ARBTSP        ilv59TSP
110090050202     C                   MOVEL     VIDCTR        ilv59CTR
110100941209     C*
110110941209     C* SE NON C'E' RICERCA SU COD.TARIFFA E CAMBIATO CODICE CLIENTE
110120941209     C*  --> RICERCO LA TARIFFA
110130941209     C                   SETOFF                                       32
110140941209     C     '?'           SCAN      VIDCTR                                 32
110150941215     C     *IN32         IFEQ      *OFF
110160941215     C     KSC           ANDNE     SAVKSC
110170050202     C     VIDCTR        ANDEQ     SAVCTR
110180160225     C***                CLEAR                   ilv59CTR
110190941223     C                   CLEAR                   OTT
110200160225     C***                MOVEL     'S'           ilv59RIC
110210941215     C                   ELSE
110220060516     c* Se cod cliente e cod tariffa invariati rispetto alla bolla,
110230060516     c*  accetto codici tariffa bloccati (ma quando immessi validi)
110240060516     c                   if        not *in32 and ksc=kscbolla
110250060511     c                             and ctrbolla=vidctr
110260060516     C                   MOVEL     'B'           ilv59ta2
110270941209     C                   ENDIF
110280060511     C                   ENDIF
110290060516     C                   MOVEL     'S'           ilv59CON
110300050202     C
110310050202     C                   MOVEL     KSC           SAVKSC
110320160303     c* se ho abblancato il codice tariffa, lo faccio reimpostare dal pgm
110330160303     c                   if        ilv59ctr=*blanks
110340160303     C                   MOVEL     'S'           ilv59RIC
110350160303     c                   endif
110360941209     C*
110370050202     C                   CALL      'FNLV59R'
110380050202     C                   PARM                    Fnlv59DS
110390050202     c
110400941209     C*
110410941209     C* NON ESISTONO TARIFFE
110420050202     C                   MOVEL     olv59TKS      WTKS              1
110430050202     C     olv59TKS      IFNE      'S'
110440941209     C                   SETON                                        18
110450911219     C                   SETOFF                                       32
110460950114     C                   MOVEL     CNOTAR        DE2CTR
110470941209     C                   ELSE
110480941209     C*
110490941215     C* SE TARIFFE TROVATE,  DEVO REIMPOSTARE LA TARIFFA A VIDEO
110500050202     C     ilv59RIC      IFEQ      'S'
110510050202     C                   MOVEL     olv59CTR      VIDCTR
110520941209     C                   ENDIF
110530941215     C* DECODIFICA
110540050202     C                   MOVEL     olv59DFS      DE2CTR
110550941209     C                   ENDIF
110560050202     c
110570050202     C                   EVAL      SAVCTR=VIDCTR
110580911211     C*
110590911211     C                   ENDSR
110600911211     C*
110610920320     C*--- CONTROLLI CAMPI CONSEGNA RICHIESTA ------------------------*
110620920320     C     CONTR7        BEGSR
110630920320     C                   SETOFF                                       90
110640941102     c**
110650920320     C* CONSEGNA RICHIESTA
110660941102     C**
110670920320     C                   Z-ADD     0             COMDCR
110680920320     C     VIDDCR        IFNE      0
110690941102     C                   Z-ADD     VIDDCR        G02DAT
110700920320     C                   MOVEL     *BLANK        G02ERR
110710941102     C                   CALL      'XSRDA8'
110720920320     C                   PARM                    WLBDAT
110730941102     C* DATA FORMALMENTE ERRATA
110740920320     C     G02ERR        IFEQ      '1'
110750941102     C                   MOVEL     MSG(6)        VIDMSG
110760941102     C                   SETON                                        409028
110770920320     C                   GOTO      ENDCT7
110780920320     C                   END
110790941102     C*
110800941102     C                   Z-ADD     G02DAT        VIDDCR
110810941102     C*
110820920320     C* DATA CONSEGNA NO < DATA SPEDIZIONE
110830930507     C     G02INV        IFLT      ARBDSP
110840941102     C                   MOVEL     MSG(7)        VIDMSG
110850941102     C                   SETON                                        409028
110860920320     C                   GOTO      ENDCT7
110870930507     C                   END
110880941102     C                   Z-ADD     G02INV        COMDCR            8 0
110890941102     C                   ENDIF
110900070308     c
110910070308      * La data non può superare i gg limiti indicati in tab. VPO
110920070308     c                   If        Not *In90
110930070308     c                   Eval      wdataiso = wdataoggi
110940070308     c                   Adddur    §vpodcr:*d    wdataiso
110950070308     c     *iso          move      wdataiso      ds_data
110960070308     c                   Move      §vpodcr       w003a
110970070308     c                   Movea     w003a         ski(13)
110980070308     c                   Z-add     13            i
110990070308     c                   ExSr      abbl
111000070308     c                   Movea     ski(13)       w003a
111010070308     c                   If        ComDcr > ds_data
111020070308     c                   Eval      *In28 = *On
111030070308     c                   Eval      *In40 = *On
111040070308     c                   Eval      *In90 = *On
111050070308     c                   Eval      VidMsg = msg(129)
111060070308     c                   Eval      VidMsg = %replace(w003a:VidMsg:
111070070308     c                                      %scan('___':VidMsg))
111080070308     C                   GOTO      ENDCT7
111090070308     c                   EndIf
111100070308     c                   EndIf
111110141029     c* Se dcr>0 errore forzabile se presente Fermo Deposito
111120141029     c                   if        d48ffr<>'E'
111130141029     c                   if        comdcr>0 and comdcr<>arbdcr and
111140141029     c                             arbffd<>*blanks and wforzdcr=0
111150141029     C                   MOVEL     MSG(154)      VIDMSG
111160141029     C                   SETON                                        409028
111170141029     C                   EVAL      Wforzdcr=1
111180141029     C                   GOTO      ENDCT7
111190141029     c                   endif
111200141029     c                   endif
111210941102     C**
111220941102     C** ORA CONSEGNA RICHIESTA
111230941102     C**
111240920320     C                   MOVEL     VIDHCR        HM                2 0
111250920320     C     HM            IFLT      0
111260920320     C     HM            ORGT      24
111270941102     C                   MOVEL     MSG(8)        VIDMSG
111280941102     C                   SETON                                        419028
111290920320     C                   GOTO      ENDCT7
111300920320     C                   END
111310941102     C**
111320941102     C** MINUTI CONSEGNA RICHISTA
111330941102     C**
111340920320     C                   MOVE      VIDHCR        HM                2 0
111350920320     C     HM            IFLT      0
111360920320     C     HM            ORGT      59
111370941102     C                   MOVEL     MSG(8)        VIDMSG
111380941102     C                   SETON                                        419028
111390920320     C                   GOTO      ENDCT7
111400920320     C                   END
111410930507     C*
111420941102     C* SE IMMESSO IL TIPO IMMETTERE ANCHE L'ORA O LA DATA
111430930507     C     VIDTCR        IFNE      ' '
111440941102     C     VIDDCR        IFEQ      0
111450941102     C     VIDHCR        ANDEQ     0
111460941102     C                   MOVEL     MSG(9)        VIDMSG
111470941102     C                   SETON                                        409028
111480930507     C                   GOTO      ENDCT7
111490930507     C                   END
111500930507     C                   END
111510070308     c
111520070308     c* Se lna  attivata con GEO, non posso inserire solo tipo e ora:
111530070308     c*  data sempre obbligatoria (se inserito in arrivo, accetto
111540070308     c*  dalla partenza)
111550100419     c* Escludo per le causali di ripristino
111560070319     c                   if        FGSgeot='S' and viddcr=0 and vidhcr>0
111570100419     c                             and d48cvb <> 'CA'
111580070308     c                   if        vidtcr<>arbtcr or vidhcr<>arbhcr or
111590070308     c                             comdcr<>arbdcr
111600070308     C                   MOVEL     MSG(140)      VIDMSG
111610070308     C                   SETON                                        409028
111620070308     C                   GOTO      ENDCT7
111630070308     C                   ENDif
111640070308     C                   ENDif
111650930507     C**
111660941102     C* GIORNI DI CHIUSURA
111670941102     C**
111680930507     C     VIDGC1        IFNE      *BLANKS
111690930507     C                   MOVEL     VIDGC1        ACOM              1
111700930507     C                   MOVE      VIDGC1        BCOM              1
111710941212     C                   Z-ADD     1             B                 2 0
111720930507     C     ACOM          LOOKUP    GGG(B)                                 32
111730941102     C     *IN32         IFEQ      *OFF
111740941102     C                   MOVEL     MSG(10)       VIDMSG
111750941102     C                   SETON                                        429028
111760941102     C                   GOTO      ENDCT7
111770941102     C                   ENDIF
111780930507     C*
111790930507     C     BCOM          IFNE      ' '
111800930507     C     BCOM          ANDNE     'P'
111810930507     C     BCOM          ANDNE     'M'
111820941102     C                   MOVEL     MSG(10)       VIDMSG
111830941102     C                   SETON                                        429028
111840930507     C                   GOTO      ENDCT7
111850930507     C                   END
111860041130      * se modifica fatta in arrivo non posso mettere ' M' o ' P'
111870041130if  2c                   If         *In10 and VidGc1 <> ArbGc1 and
111880041130     c                             (VidGc1 = ' M' or VidGc1 = ' P')
111890041130     c                   Eval      *In28 = *On
111900041130     c                   Eval      *In42 = *On
111910041130     c                   Eval      *In90 = *On
111920041130     c                   Eval      VidMsg = msg(10)
111930041130e   2c                   EndIf
111940930507     C                   END
111950930507     C*
111960930507     C*** 2° GIORNO CHIUSURA ***
111970930507     C     VIDGC2        IFNE      *BLANKS
111980930507     C                   MOVEL     VIDGC2        ACOM              1
111990930507     C                   MOVE      VIDGC2        BCOM              1
112000930507     C                   Z-ADD     1             B
112010930507     C     ACOM          LOOKUP    GGG(B)                                 30
112020941102     C     *IN30         IFEQ      *OFF
112030941102     C                   MOVEL     MSG(10)       VIDMSG
112040941102     C                   SETON                                        439028
112050941102     C                   GOTO      ENDCT7
112060941102     C                   ENDIF
112070930507     C*
112080930507     C     BCOM          IFNE      ' '
112090930507     C     BCOM          ANDNE     'P'
112100930507     C     BCOM          ANDNE     'M'
112110941102     C                   MOVEL     MSG(11)       VIDMSG
112120941102     C                   SETON                                        439028
112130930507     C                   GOTO      ENDCT7
112140930507     C                   END
112150930507     C*
112160930507     C* I GIORNI DI CHIUSURA NON POSSO ESSERE UGUALI
112170930507     C     VIDGC1        IFEQ      VIDGC2
112180941102     C                   MOVEL     MSG(12)       VIDMSG
112190941102     C                   SETON                                        439028
112200930507     C                   GOTO      ENDCT7
112210930507     C                   END
112220930507     C* NON POSSONO ESSERE ' P' ' M'
112230930507     C     VIDGC1        IFEQ      ' M'
112240930507     C     VIDGC2        ANDEQ     ' P'
112250930507     C     VIDGC1        OREQ      ' P'
112260930507     C     VIDGC2        ANDEQ     ' M'
112270941102     C                   MOVEL     MSG(13)       VIDMSG
112280941102     C                   SETON                                        429028
112290930507     C                   GOTO      ENDCT7
112300930507     C                   END
112310041130      * se modifica fatta in arrivo non posso mettere ' M' o ' P'
112320041130if  2c                   If         *In10 and VidGc2 <> ArbGc2 and
112330041130     c                             (VidGc2 = ' M' or VidGc2 = ' P')
112340041130     c                   Eval      *In28 = *On
112350041130     c                   Eval      *In43 = *On
112360041130     c                   Eval      *In90 = *On
112370041130     c                   Eval      VidMsg = msg(10)
112380041130e   2c                   EndIf
112390930507     C                   END
112400941102     C*
112410941102     C* CONSEGNE PARTICOLARi
112420020603     c* Verifico se destinatario disagiato
112430020604     c* per le bolle poste non lo faccio
112440020604     c                   clear                   wdisag
112450020604     c     lnpntw        ifne      'PPT'
112460020603     c                   clear                   tisit0ds
112470020603     c                   movel     'E'           it0tla
112480020603     c                   movel     arbnzd        it0naz
112490020603     c                   movel     arbprd        it0prv
112500020603     c                   movel     arbcad        it0cap
112510020603     c                   movel     arblod        it0loc
112520020603     c                   movel     arbind        it0ind
112530020603     c                   movel     arbrsd        it0rag
112540020603     c                   call      'TISIT5R'
112550020603     c                   parm                    tisit0ds
112560020603     c     ot0err        ifeq      *blanks
112570060203   2ac                   if        ot0dos = 'A' or ot0dos = 'D' or
112580060203     c                             ot0dos = 'S'
112590020603     c                   movel     ot0dos        wdisag
112600020603     c     wdisag        ifeq      'D'
112610020603     c                   movel     'X'           wdisag
112620020603     c                   endif
112630060203     c                   endif
112640020603     c                   else
112650020603     c                   clear                   wdisag
112660020603     c                   endif
112670020604     c                   endif
112680020603     c
112690941102     C*
112700941102     C* 11ON - NON POSO CAMBIARE LA 1 CONSEGNA PARTICOLARE
112710070910     c* Se ho modificato il destinatario è non è stata immessa in
112720070910     c*  partenza, devo per forza toglierla
112730070910     c                   if        *in11=*on and wdisag<>vidtc1 and
112740070910     c                             arbtc1=vidtc1 and par5mdp='S' and
112750070910     c                             par5gtc1<>vidtc1 and par5gtc2<>vidtc1
112760070910     C                   MOVEL     MSG(144)      VIDMSG
112770070910     c                   eval      %subst(vidmsg:27:1) = vidtc1
112780070910     c                   eval      %subst(vidmsg:29:8) = v7dtc1
112790070910     C                   SETON                                        449028
112800070910     C                   GOTO      ENDCT7
112810070910     c                   endif
112820070910     c
112830051116     c****               if        *in11 = *on and wdisag <> *blanks
112840070910     c* se consegna particolare non modificabile ma è stato immesso
112850070910     c* unacons.part.disagiato/supemercat... do' lo stesso la
112860070910     c* possibilita di cambiarla se la combinazione fra le due non è
112870070910     c* ammessa
112880051116     c                   clear                   wcomb
112890070910    0c                   if        *in11=*on and par5mdp='S' and
112900070910     c                             (arbtc1=par5gtc1 or arbtc1=par5gtc2)
112910070910     c
112920070910    1c                   if        vidtc2<>*blanks or wdisag<>*blanks
112930070910     c                             or vidtc1<>arbtc1
112940070910     c*
112950070910     c***                if        *in11 = *on and (vidtc2 <> *blanks or
112960070910     c***                          wdisag <> *blanks or vidtc1 <> arbtc1)
112970051114     c                   move      arbtc1        wtc1
112980051117     c
112990051116     c                   if        wdisag <> *blanks
113000051116     c                   move      wdisag        wtc2
113010051117     c                   exsr      sr_chkcomb
113020051117     c                   endif
113030051117     c                   if        wcomb = *blanks and vidtc1 <> arbtc1
113040051117     c                   move      vidtc1        wtc2
113050051117     c                   exsr      sr_chkcomb
113060051117     c                   endif
113070051117     c                   if        wcomb = *blanks and vidtc2 <> *blanks
113080051117     c                   move      vidtc2        wtc2
113090051117     c                   exsr      sr_chkcomb
113100051117     c                   endif
113110070910    1c                   endif
113120070910     c***
113130070910    0C***  *IN11         IFEQ      *ON
113140070910     c***  wdisag        andeq     *blanks
113150070910    0c***                if        *in11 = *on and wcomb = *blanks
113160070910     c
113170070910    1c                   if        wcomb=*blanks
113180941102     C*
113190070910    2C     VIDTC1        IFNE      ARBTC1
113200941102     C                   MOVEL     MSG(15)       VIDMSG
113210051117     c                   eval      %subst(vidmsg:36:1) = arbtc1
113220941102     C                   SETON                                        449028
113230941102     C                   GOTO      ENDCT7
113240070910    2c                   endif
113250070910    1c                   endif
113260070910    0c                   endif
113270070910     c
113280070910     c***                else
113290070910     C***                MOVEL     '1P'          COD
113300070910     C***                MOVEL(P)  VIDTC1        KEY
113310070910     C***                EXSR      CTABEL
113320070910     C***                MOVEL     TBLUNI        V7DTC1
113330070910    1C***                ENDIF
113340941102     C*
113350070910   X0C***                ELSE
113360941102     C*
113370941102     C                   CLEAR                   V7DTC1
113380941102     C*
113390941102    1C     VIDTC1        IFNE      ' '
113400941102     C* RICERCA
113410941102    2C     VIDTC1        IFEQ      '?'
113420941102     C                   MOVEL     CODUT         §KUT
113430941102     C                   MOVEL     '1P'          §COD
113440941102     C                   MOVE      *BLANKS       VIDTC1
113450991011     C                   CLEAR                   §KEY
113460941102     C                   CALL      'X§TABER'
113470941102     C                   PARM                    §KUT
113480941102     C                   PARM                    §COD
113490941102     C                   PARM                    §KEY
113500941209     C                   PARM                    §DES
113510941102     C                   MOVEL     §KEY          VIDTC1
113520941102     C                   MOVEL     §DES          V7DTC1
113530941102     C                   SETON                                        9044
113540941102     C                   GOTO      ENDCT7
113550941102    2C                   ENDIF
113560941102     C*
113570941102     C                   MOVEL     '1P'          COD
113580941102     C                   MOVEL(P)  VIDTC1        KEY
113590941212     C                   EXSR      CTABEL
113600941102     C*
113610941102    2C     *IN30         IFEQ      *ON
113620941102     C                   MOVEL     MSG(11)       VIDMSG
113630941102     C                   SETON                                        449028
113640941102     C                   GOTO      ENDCT7
113650941102    2C                   ENDIF
113660941102     C*
113670941102     C                   MOVEL     TBLUNI        DS1P
113680941102     C*
113690941102     C* NON UTILIZZABILE IN ARRIVO
113700950104    2C   10§1PUAR        IFNE      'S'
113710020318     C     LNPNTW        ANDNE     'PPT'
113720020318     C     LNPNTW        OREQ      'PPT'
113730000510     C     §1PFPC        ANDNE     'E'
113740000510     C     §1PFPC        ANDNE     'A'
113750040705      * e non è quella presa in automatico
113760051114     c                   If        Vidtc1 <> wdisag and vidtc1 <> arbtc1
113770070914     c* Verifico se era stata messa in partenza ma tolta
113780070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
113790070914     c                             <>vidtc1 and par5gtc2<>vidtc1)
113800941102     C                   MOVEL     MSG(11)       VIDMSG
113810941102     C                   SETON                                        449028
113820941102     C                   GOTO      ENDCT7
113830040705     c                   EndIf
113840070914     c                   EndIf
113850941102    2C                   END
113860060131     c
113870060131     c* SE devo considerare le abilitati, verifico se il profilo
113880060131     c*  può inserire la cons particolare, se non è presa in automatico
113890060131     c   10              If        Vidtc1 <> wdisag and vidtc1 <> arbtc1
113900070914     c                             and  §utemfcp=' '
113910070914     c******                       and dateu>=§vpoacp and  §utemfcp=' '
113920070914     c*
113930070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
113940070914     c                             <>vidtc1 and par5gtc2<>vidtc1)
113950060131     C*
113960060131     C                   MOVEL     'SP'          COD
113970060131     C                   MOVEL(P)  VIDTC1        KEY
113980060131     C                   EXSR      CTABEL
113990060131     C*
114000060131    2C     *IN30         IFEQ      *ON
114010060131     c                   clear                   dssp
114020060131     c                   else
114030060131     c                   movel     tbluni        DSSP
114040060131     c                   endif
114050060131     c
114060060131     c                   if        §spaba='S'
114070060131     C                   MOVEL     MSG(138)      VIDMSG
114080060131     c                   eval      %subst(vidmsg:59:1) = vidtc1
114090060131     C                   SETON                                        449028
114100060131     C                   GOTO      ENDCT7
114110060131     c                   endif
114120060131     c                   endif
114130070914     c
114140070914     c                   endif
114150060131     c
114160950104     C* NON UTILIZZABILE IN PARTENZA
114170950104    2C  N10§1PUPR        IFNE      'S'
114180020318     C     LNPNTW        ANDNE     'PPT'
114190020318     C     LNPNTW        OREQ      'PPT'
114200000510     C     §1PFPC        ANDNE     'E'
114210000510     C     §1PFPC        ANDNE     'P'
114220950104     C                   MOVEL     MSG(11)       VIDMSG
114230950104     C                   SETON                                        449028
114240950104     C                   GOTO      ENDCT7
114250950104    2C                   END
114260000510     C* PER LE POSTE CONTROLLO IL SUO FLAG
114270941102     C*
114280941102     C                   MOVEL     TBLUNI        V7DTC1
114290150414     c* controlli per EXPO
114300151119     c*                  if        vidtc1='Z'
114310151119     c*                  exsr      sr_chkexpo
114320151119     c*                  if        *in90
114330151119     c*                  seton                                        44
114340151119     c*                  goto      endct7
114350151119     c*                  endif
114360151119     c*                  endif
114370941102     C*
114380941102     C* LA 1 E LA 2 NON POSSONO ESSERE UGUALI
114390941102    2C     VIDTC2        IFEQ      VIDTC1
114400941102     C                   MOVEL     MSG(14)       VIDMSG
114410941102     C                   SETON                                        459028
114420941102     C                   GOTO      ENDCT7
114430941102    2C                   END
114440020731     C*
114450941102     C*
114460941102    1C                   END
114470070910    0C******             END
114480941102     C*
114490941102     C* 12ON - NON POSO CAMBIARE LA 2 CONSEGNA PARTICOLARE
114500070910     c* Se ho modificato il destinatario è non è stata immessa in
114510070910     c*  partenza, devo per forza toglierla
114520070910    0c                   if        *in12=*on and wdisag<>vidtc2 and
114530070910     c                             arbtc2=vidtc2 and par5mdp='S' and
114540070910     c                             par5gtc1<>vidtc2 and par5gtc2<>vidtc2
114550070910     C                   MOVEL     MSG(144)      VIDMSG
114560070910     c                   eval      %subst(vidmsg:27:1) = vidtc2
114570070910     c                   eval      %subst(vidmsg:29:8) = v7dtc2
114580070910     C                   SETON                                        459028
114590070910     C                   GOTO      ENDCT7
114600070910    0c                   endif
114610070910     c
114620051114     c* se consegna particolare non modificabile ma è stato immesso
114630051114     c* un destinatrio disagiato/supemercat... do' lo stesso la
114640051114     c* possibilita di cambiarla se la combinazione fra le due non è
114650051114     c* ammessa
114660051116     c                   clear                   wcomb
114670070910    0c                   if        *in12=*on and par5mdp='S' and
114680070910     c                             (arbtc2=par5gtc1 or arbtc2=par5gtc2)
114690070910
114700070910    1c                   if        vidtc1 <> *blanks or
114710070910     c                             wdisag <> *blanks or vidtc2 <> arbtc2
114720070910     c
114730051116     c***                if        *in12 = *on and wdisag <> *blanks
114740070910     c***
114750070910     c***                if        *in12 = *on and (vidtc1 <> *blanks or
114760070910     c***                          wdisag <> *blanks or vidtc2 <> arbtc2)
114770051114     c                   move      arbtc2        wtc1
114780051116     c                   if        wdisag <> *blanks
114790051116     c                   move      wdisag        wtc2
114800051117     c                   exsr      sr_chkcomb
114810051117     c                   endif
114820051117     c                   if        wcomb = *blanks and vidtc2 <> arbtc2
114830051117     c                   move      vidtc2        wtc2
114840051117     c                   exsr      sr_chkcomb
114850051117     c                   endif
114860051117     c                   if        wcomb = *blanks and vidtc1 <> *blanks
114870051117     c                   move      vidtc1        wtc2
114880051117     c                   exsr      sr_chkcomb
114890051117     c                   endif
114900070910    1c                   endif
114910051117     c*
114920070910    0C***  *IN12         IFEQ      *ON
114930070910    0c***                if        *in12 = *on and wcomb = *blanks
114940070910    1c                   if        wcomb = *blanks
114950941102     C*
114960070910    2C     VIDTC2        IFNE      ARBTC2
114970941102     C                   MOVEL     MSG(15)       VIDMSG
114980051117     c                   eval      %subst(vidmsg:36:1) = arbtc2
114990941102     C                   SETON                                        459028
115000941102     C                   GOTO      ENDCT7
115010070910    2c                   endif
115020070910    1c                   endif
115030070910    0c                   endif
115040070910     c
115050070910     c****               else
115060070910     C****               MOVEL     '1P'          COD
115070070910     C****               MOVEL(P)  VIDTC2        KEY
115080070910     C****               EXSR      CTABEL
115090070910     C****               MOVEL     TBLUNI        V7DTC2
115100070910    1C****               ENDIF
115110941102     C*
115120070910   X0C****               ELSE
115130941102     C*
115140941102     C                   CLEAR                   V7DTC2
115150941102     C*
115160941102    1C     VIDTC2        IFNE      ' '
115170941102     C* RICERCA
115180941102    2C     VIDTC2        IFEQ      '?'
115190941102     C                   MOVEL     CODUT         §KUT
115200941102     C                   MOVEL     '1P'          §COD
115210941102     C                   MOVE      *BLANKS       VIDTC2
115220991011     C                   CLEAR                   §KEY
115230941102     C                   CALL      'X§TABER'
115240941102     C                   PARM                    §KUT
115250941102     C                   PARM                    §COD
115260941102     C                   PARM                    §KEY
115270941209     C                   PARM                    §DES
115280941102     C                   MOVEL     §KEY          VIDTC2
115290941102     C                   MOVEL     §DES          V7DTC2
115300941102     C                   SETON                                        9045
115310941102     C                   GOTO      ENDCT7
115320941102    2C                   ENDIF
115330941102     C*
115340941102     C                   MOVEL     '1P'          COD
115350941102     C                   MOVEL(P)  VIDTC2        KEY
115360941212     C                   EXSR      CTABEL
115370941102     C*
115380941102    2C     *IN30         IFEQ      *ON
115390941102     C                   MOVEL     MSG(11)       VIDMSG
115400941102     C                   SETON                                        459028
115410941102     C                   GOTO      ENDCT7
115420941102    2C                   ENDIF
115430941102     C*
115440941102     C                   MOVEL     TBLUNI        DS1P
115450941102     C*
115460941102     C* NON UTILIZZABILE IN ARRIVO
115470950104    2C   10§1PUAR        IFNE      'S'
115480020318     C     LNPNTW        ANDNE     'PPT'
115490020318     C     LNPNTW        OREQ      'PPT'
115500000518     C     §1PFPC        ANDNE     'E'
115510000518     C     §1PFPC        ANDNE     'A'
115520040705      * e non è quella presa in automatico
115530051114     c                   If        Vidtc2 <> wdisag  and vidtc2 <> arbtc2
115540070914     c* Verifico se era stata messa in partenza ma tolta
115550070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
115560070914     c                             <>vidtc2 and par5gtc2<>vidtc2)
115570941102     C                   MOVEL     MSG(11)       VIDMSG
115580941102     C                   SETON                                        459028
115590941102     C                   GOTO      ENDCT7
115600040705     c                   EndIf
115610070914     c                   EndIf
115620941102    2C                   END
115630060131     c
115640060131     c* SE devo considerare le abilitati, verifico se il profilo
115650060131     c*  può inserire la cons particolare, se non è presa in automatico
115660060131     c   10              If        Vidtc2 <> wdisag and vidtc2 <> arbtc2
115670070914     c                             and  §utemfcp=' '
115680070514     c****                         and dateu>=§vpoacp and  §utemfcp=' '
115690070914     c* Verifico se era stata messa in partenza ma tolta
115700070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
115710070914     c                             <>vidtc2 and par5gtc2<>vidtc2)
115720060131     C*
115730060131     C                   MOVEL     'SP'          COD
115740060131     C                   MOVEL(P)  VIDTC2        KEY
115750060131     C                   EXSR      CTABEL
115760060131     C*
115770060131    2C     *IN30         IFEQ      *ON
115780060131     c                   clear                   dssp
115790060131     c                   else
115800060131     c                   movel     tbluni        DSSP
115810060131     c                   endif
115820060131     c
115830060131     c                   if        §spaba='S'
115840060131     C                   MOVEL     MSG(138)      VIDMSG
115850060131     c                   eval      %subst(vidmsg:59:1) = vidtc1
115860060131     C                   SETON                                        459028
115870060131     C                   GOTO      ENDCT7
115880060131     c                   endif
115890060131     c                   endif
115900070914     c                   endif
115910070914     c
115920950104     C* NON UTILIZZABILE IN PARTENZA
115930950104    2C  N10§1PUPR        IFNE      'S'
115940020318     C     LNPNTW        ANDNE     'PPT'
115950020318     C     LNPNTW        OREQ      'PPT'
115960000518     C     §1PFPC        ANDNE     'E'
115970000518     C     §1PFPC        ANDNE     'P'
115980950104     C                   MOVEL     MSG(11)       VIDMSG
115990950104     C                   SETON                                        459028
116000950104     C                   GOTO      ENDCT7
116010950104    2C                   END
116020941102     C*
116030941102     C                   MOVEL     TBLUNI        V7DTC2
116040150414     c* controlli per EXPO
116050151119     c*                  if        vidtc2='Z'
116060151119     c*                  exsr      sr_chkexpo
116070151119     c*                  if        *in90
116080151119     c*                  seton                                        45
116090151119     c*                  goto      endct7
116100151119     c*                  endif
116110151119     c*                  endif
116120941102     C*
116130941102    1C                   END
116140070910    0C****               END
116150060131     c
116160020531     c* Se destinatario disagiato o supermercato errore se non indicato
116170020531     c* in nessuno dei due campi di consegna particolare
116180041015     c*
116190041015     c* Se particolarità varia="U" --> consegna ad uffici devo invece
116200041015     c* dare errore se immesso supermercato o appuntamento
116210041015     c                   if        %subst(arbgva:1:1)='U'
116220041015     c                             and (vidtc1='S' or vidtc1='A' or vidtc2='S'
116230041015     c                             or vidtc2='A')
116240041015     C                   MOVEl     msg(131)      vidmsg
116250041015     C                   SETON                                        449028
116260041015     C                   GOTO      ENDCT7
116270041015     c                   endif
116280150210
116290150210     c* se sono in partenza e immessa la "A" e la data --> errore solo la data immettere
116300150210     c                   if        not *in10 and (vidtc1='A' or vidtc2='A') and
116310150210     c                             comdcr>0
116320150210     c                   if        (par5mdp='S' and par5gtc1<>'A' and par5gtc2
116330150210     c                                                    <> 'A')
116340150210     c                             or (par5mdp=' ' and arbtc1<>'A' and arbtc2
116350150210     c                                                    <> 'A')
116360150210     C                   MOVEl     msg(158)      vidmsg
116370150210     C                   SETON                                        449028
116380150210     C                   GOTO      ENDCT7
116390150210     c                   endif
116400150210     c                   endif
116410150119
116420150119     c* se destinatario particolare che prevede appuntamento controllo se presente
116430150119     c* DCR immessa dalla partenza perchè in questo caso non devo dare errore per la
116440150119     c* mancanza della "A" di appuntamento
116450150119     c                   eval      w_noapp=' '
116460150119     c                   if        wdisag='A'
116470150119     c                   if        (par5mdp='S' and par5gdcr>*zeros
116480150119     c                                          and par5gtcr=' ')
116490150119     c                             or (par5mdp=' ' and arbdcr>0 and arbtcr= ' ')
116500150119     c                   eval      w_noapp='1'
116510150119     c                   endif
116520150119     c                   endif
116530041015     c*
116540041015     C***  wdisag        ifne      *blanks
116550041015     c***  wdisag        andne     vidtc1
116560041015     c***  wdisag        andne     vidtc2
116570150119     c                   if        wdisag <> *blanks and w_noapp=' ' and
116580041015     c                             %subst(arbGva:1:1) <> 'U' or
116590041015     c                             wdisag='X' and %subst(arbGva:1:1)='U'
116600041015     c     wdisag        ifne      vidtc1
116610041015     c     wdisag        andne     vidtc2
116620020603     C                   MOVEa     msg(112)      k78
116630020531     c                   select
116640020531     c     wdisag        wheneq    'X'
116650020531     c                   eval      w014a = 'disagiato     '
116660020531     c     wdisag        wheneq    'S'
116670020531     c                   eval      w014a = '"supermercato"'
116680040705     c     wdisag        wheneq    'A'
116690040705     c                   eval      w014a = '"appuntamento"'
116700020531     c                   endsl
116710020531     C                   MOVEa     w014a         k78(14)
116720020531     C                   MOVEA     K78           VIDMSG
116730020603     C                   SETON                                        449028
116740020531     C                   GOTO      ENDCT7
116750020531     c                   endif
116760041015     c                   endif
116770070907     C* Non è possibile indicare contemporaneamente alcune consegne    i
116780070907     C*  particolari
116790051114     c                   move      vidtc1        wtc1
116800051114     c                   move      vidtc2        wtc2
116810051114     c                   exsr      sr_chkcomb
116820051114     c                   if        wcomb = 'S'
116830051114     C                   movel     MSG(113)      VIDmsg
116840051116     c                   eval      %subst(vidmsg:63:1) = vidtc1
116850051116     c                   eval      %subst(vidmsg:69:1) = vidtc2
116860051114     C                   seton                                        459028
116870051114     C                   goto      ENDCT7
116880051114    2C                   ENDIF
116890941102     C*
116900920320     C     ENDCT7        ENDSR
116910021129      **
116920021129      *--- CONTROLLI CAMPI CONSEGNA RICHIESTA SUPERMERCATI -----------*
116930021129     C     CONTR71       BEGSR
116940021129     C                   SETOFF                                       90
116950021129
116960030123      * Se la spedizione non ha già la consegna particolare 'S'
116970030123     c                   If        ArbTc1 <> 'S' and ArbTc2 <> 'S'
116980030123      * E' obbligatorio inserire la consegna
116990030123     c                   If        (VidTc1 = *Blanks and VidTc2 = *Blanks)
117000030123     c                   Eval      VidMsg = Msg(117)
117010030123     c                   Eval      *In44 = *On
117020030123     c                   Eval      *In28 = *On
117030030123     c                   Eval      *In90 = *On
117040030123     c                   GoTo      EndCt71
117050030123     c                   EndIf
117060030127      * Se ci sono già tutte e 2 almeno una deve essere 'S'
117070030127     c                   If        VidTc1 <> 'S' and VidTc2 <> 'S'
117080030127     c                   Eval      VidMsg = Msg(122)
117090030127     c                   Eval      *In44 = *On
117100030127     c                   Eval      *In28 = *On
117110030127     c                   Eval      *In90 = *On
117120030127     c                   GoTo      EndCt71
117130030127     c                   EndIf
117140030123      * Se la prima consegna c'è già e non è supermercato devo mettere 'S' nella seconda
117150030127     c                   If        ArbTc1 <> *Blanks and VidTc1 <> 'S' and
117160030127     c                             VidTc2 = *Blanks
117170030123     c                   Eval      VidMsg = Msg(117)
117180030123     c                   Eval      *In45 = *On
117190030123     c                   Eval      *In28 = *On
117200030123     c                   Eval      *In90 = *On
117210030123     c                   GoTo      EndCt71
117220030123     c                   EndIf
117230030123      * Se la seconda consegna c'è già e non è supermercato devo mettere 'S' nella prima
117240030127     c                   If        ArbTc2 <> *Blanks and VidTc2 <> 'S' and
117250030127     c                             VidTc1 = *Blanks
117260030123     c                   Eval      VidMsg = Msg(117)
117270030123     c                   Eval      *In45 = *On
117280030123     c                   Eval      *In28 = *On
117290030123     c                   Eval      *In90 = *On
117300030123     c                   GoTo      EndCt71
117310030123     c                   EndIf
117320021129     c                   EndIf
117330021129
117340021129      * Se è stata inserita la consegna particolare 'S' si devono
117350021129      * immettere anche la persona da contattare, il telefono e il motivo
117360021129     c                   If        VidTc1 = 'S' or VidTc2 = 'S'
117370021129      * Nominativo
117380021129     c                   If        VidNom = *Blanks
117390021129     c                   Eval      VidMsg = Msg(118)
117400021129     c                   Eval      *In57 = *On
117410021129     c                   Eval      *In28 = *On
117420021129     c                   Eval      *In90 = *On
117430021129     c                   GoTo      EndCt71
117440021129     c                   EndIf
117450021129      * Telefono
117460021129     c                   If        VidTel = *Blanks
117470021129     c                   Eval      VidMsg = Msg(119)
117480021129     c                   Eval      *In58 = *On
117490021129     c                   Eval      *In28 = *On
117500021129     c                   Eval      *In90 = *On
117510021129     c                   GoTo      EndCt71
117520041007     c**                 Else
117530041007     c**                 Clear                   Trul42Ds
117540041007     c**                 Movel     VidTel        D42Fax
117550041007     c**                 Call      'TRUL42R'
117560041007     c***                Parm                    Trul42Ds
117570041007     c***                If        D42Err = '1'
117580041007     c****               Eval      *In58 = *On
117590041007     c**                 Eval      *In28 = *On
117600041007     c**                 Eval      *In90 = *On
117610041007     c**                 Eval      VidMsg = D42Msg
117620041007     c**                 GoTo      EndCt71
117630041007     c**                 EndIf
117640041007     c                   EndIf
117650021129      * Motivo
117660070406      * non più obbligatorio per evitare che scrivano "bagianate"
117670070406     c**                 If        VidMot = *Blanks
117680070406     c***                Eval      VidMsg = Msg(120)
117690070406     c**                 Eval      *In59 = *On
117700070406     c**                 Eval      *In28 = *On
117710070406     c**                 Eval      *In90 = *On
117720070406     c**                 GoTo      EndCt71
117730070406     c**                 EndIf
117740021129     c                   EndIf
117750021129
117760021129     c     EndCt71       EndSr
117770030930      **
117780030930      *--- CONTROLLO SE POSSIBILE LA MANUTENZIONE DELLA DATA CONSEGNA RICHIESTA ---*
117790030930     c     Contr72       BegSr
117800050907
117810030930     c                   Eval      *In90 = *Off
117820070306     c                   clear                   aggioarp          1
117830050907
117840030930      * Se tipo o data o ora consegna richiesta sono state variate
117850031001If  1c                   If        VidTcr <> ArbTcr or
117860031030     c                             ComDcr <> ArbDcr or
117870030930     c                             VidHcr <> ArbHcr
117880150205     c
117890150205     c* permetto sempre la variazione della cons richiesta se sono in partenza
117900160115     c                   if        not *in10 or wfile='P'
117910150205     c                   Goto      EndCt72
117920150205     c                   EndIf
117930150205     c
117940051109      * Se causale 'CS' e giacenza aperta in fase 30 con appuntamento (sulla giacenza)
117950051109      * posso variare la data
117960051109      * quindi bypasso tutti gli altri controlli
117970051109     c                   If        d48cvb = 'CS' and *In04 and wgcfas = 30 and
117980051109     c                             d§gcpapp = 'A'
117990051109     c                   Eval      wgiac = 'S'
118000051109     c                   Goto      EndCt72
118010051109     c                   EndIf
118020040224      * non posso modificare se la bolla ha giacenza aperta (IN04 ON)
118030050907      * e se la fase non è = a 35 o se la bolla è bloccata come giacenza
118040050907if  2c                   If        *In04 and (wgcfas <> 35 or arbfbc = 'G')
118050070312     c                   Eval      VidMsg = Msg(141)
118060040220     c                   Eval      *In28 = *On
118070040220     c                   Eval      *In90 = *On
118080040220     c                   If        VidTcr <> ArbTcr
118090040220     c                   Eval      *In62 = *On
118100040220     c                   GoTo      EndCt72
118110040220     c                   EndIf
118120040220     c                   If        ComDcr <> ArbDcr
118130040220     c                   Eval      *In40 = *On
118140040220     c                   GoTo      EndCt72
118150040220     c                   EndIf
118160040220     c                   If        VidHcr <> ArbHcr
118170040220     c                   Eval      *In41 = *On
118180040220     c                   GoTo      EndCt72
118190040220     c                   EndIf
118200040220    2c                   EndIf
118210040324      * posso modificare se la bolla ha giacenza aperta (IN04 ON) e la fase è = 35
118220040330      * ma solo se metto la data consegna richiesta maggiore/uguale alla data eseguibilità giacenza
118230040324if  2c                   If        *In04 and wgcfas = 35
118240040330if  3c                   If        wgcded > *Zeros and ComDcr >= wgcded
118250040324     c                   Eval      wgiac = 'S'
118260040324     c                   GoTo      EndCt72
118270040324    3c                   EndIf
118280040330if  3c                   If        wgcded = *Zeros and ComDcr >= wgcddm
118290040324     c                   Eval      wgiac = 'S'
118300040324     c                   GoTo      EndCt72
118310040324    3c                   EndIf
118320040324     c                   Eval      VidMsg = Msg(128)
118330040324     c                   Eval      *In28 = *On
118340040324     c                   Eval      *In90 = *On
118350040324     c                   If        VidTcr <> ArbTcr
118360040324     c                   Eval      *In62 = *On
118370040324     c                   GoTo      EndCt72
118380040324     c                   EndIf
118390040324     c                   If        ComDcr <> ArbDcr
118400040324     c                   Eval      *In40 = *On
118410040324     c                   GoTo      EndCt72
118420040324     c                   EndIf
118430040324     c                   If        VidHcr <> ArbHcr
118440040324     c                   Eval      *In41 = *On
118450040324     c                   GoTo      EndCt72
118460040324     c                   EndIf
118470040324    2c                   EndIf
118480050907      * richiamo il controllo della "7Q"
118490050907     c                   ExSr      Contr7q
118500030930      * posso modificare se
118510031001      *                     Peso o volume da fatturare sono >= a quanto tabellato in VPO
118520031001If  2c                   If        V1cPkf >= §VpoPkg or
118530031001     c                             V1cVlf >= §VpoVlm or
118540031001      *                     Destinatario supermercato o Apputamento
118550031001     c                             VidTc1 = 'S' or VidTc1 = 'A' or
118560031001     c                             VidTc2 = 'S' or VidTc2 = 'A' or
118570150506     c                             VidTc1 = 'Z' or VidTc2 = 'Z' or
118580031001      *                     Lasciato avviso
118590031001     c                             ArbFbc = 'A' or
118600031001      *                     Fermo deposito
118610031114     c                             ArbFfd = 'S' or
118620031114      *                     Tipo bolla "FW" Recapito c/assegni
118630050907     c                             ArbCbo = 'FW' or
118640050907      *                     Periodo trazione ridotte e bolla con particolarità "G8"
118650050907     c                             wDCRG8 = 'S'
118660031001     c                   GoTo      EndCt72
118670031001    2c                   EndIf
118680070320     c
118690070320     c* Se FFD immesso dalla partenza, anche se già tolto, posso
118700070320     c*  manutenzionare la spedizione
118710070920     c***                Clear                   dAr5Gen
118720070920     c***                Eval      kAr5Trd = 'GEN'
118730070920     c***  kAr5          Chain(n)  Fiar501l
118740070920     c***                If        %Found(Fiar501l)
118750070920     c***                Movel     Ar5Uni        dAr5Gen
118760070920     c***                if        §ar5MDP='S'  and §ar5FFD='S'
118770070920     c                   if        Wesar5gen='S' and par5mdp='S' and
118780070920     c                             par5FFD='S'
118790070320     c                   GoTo      EndCt72
118800070320    2c                   EndIf
118810070920    2c***                EndIf
118820070312    c
118830070312     c* Se la bolla non è in distinta, non c'e' blocco giacenza e c'e'
118840070312     c*  un evento che prevede la modifica della cons, richiesta
118850070312     c*  allora posso inserirla ed aggiornare direttamente ARB+fiar5
118860070319     c                   if        arbndc=0 and FGSgeot='S' and
118870070312     c                             arbfbc<>'G' and eveADR='S'
118880070312     c                   GoTo      EndCt72
118890070312    3c                   EndIf
118900070312     c
118910031001      *                     C/assegno non abilitato
118920031001If  2c                   If        Ar9Cas > 0
118930031001     c                   Clear                   Trul21ds
118940031001     c                   Eval      I21Tla = 'L'
118950031001     c                   Eval      I21Cbo = ArbCbo
118960031001     c                   Eval      I21Tsp = ArbTsp
118970031001     c                   Eval      I21Lnp = ArbLnp
118980031001     c                   Eval      I21Nzm = ArbNzm
118990031001     c                   Eval      I21Lna = ArbLna
119000031001     c                   Eval      I21Nzd = ArbNzd
119010130827     c****               Eval      I21Ksc = ArbKsc
119020031001     c                   Movel     ArbCtr        I21Ctr
119030031001     c                   Eval      I21Tic = Ar9Tic
119040031001     c                   Eval      I21Imp = Ar9Cas
119050031001     c                   Eval      I21Div = Ar9Vca
119060031001     c                   Eval      I21Ute = Knmus
119070031001     c                   Eval      I21Pgm = nompgm
119080130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
119090130827     c                   if        flgtb1='F'
119100130827     c                   z-add     arbksc        i21ksc
119110130827     c                   else
119120130827     c                   z-add     arbccm        i21ksc
119130130827     c                   if        i21ksc=0
119140130827     c                   z-add     8888          i21ksc
119150130827     c                   movel     arblnp        i21ksc
119160130827     c                   endif
119170130827     c                   endif
119180031001     c                   Call      'TRUL21R'
119190031001     c                   Parm                    Trul21ds
119200031001If  3c                   If        O21Fca <> *Blanks
119210031001     c                   GoTo      EndCt72
119220031001    3c                   EndIf
119230031001    2c                   EndIf
119240070309     c
119250070309     c* Se la bolla è in distinta, non devo però aggiornare
119260070306     c*  fnarb ma FIARP
119270111025    2c                   if        arbndc>0 and FGSgeot='S'
119280111025     c
119290111025     c* Se posso, scrivo FIARP, altrimenti subiro errore
119300111025    3c                   if        DcrinDist<>'S'
119310111025      * erorre
119320111025     c                   Eval      VidMsg = Msg(126)
119330111025     c                   Eval      *In28 = *On
119340111025     c                   Eval      *In90 = *On
119350111025     c                   Eval      *In40 = *On
119360111025     c                   GoTo      EndCt72
119370111025     c
119380111025   x3c                   else
119390070308     c                   eval      Aggioarp='S'
119400111025    4c                   if        comdcr<>savdcr  or vidtcr<>savtcr or
119410070308     c                             vidhcr<>savhcr
119420070309     c* Se richiamo nascosto, imposto solo il campo msgforza,
119430070309     c*  altrimenti emetto il msg
119440070330     c*  non imposto il campo se devo fare solo i controlli (26 OFF)
119450111025    5c                   if        d48ffr<>'E'
119460070308     c                   seton                                        402890
119470070308     c                   Eval      VidMsg = Msg(139)
119480111025   x5c                   else
119490070330     c
119500070330     c                   if        *in26
119510070330     c                   eval      msgforza=142
119520070309     c                   endif
119530111025    5c                   endif
119540070309     c
119550070308     c                   eval      savdcr=comdcr
119560070308     c                   eval      savtcr=vidtcr
119570070308     c                   eval      savhcr=vidhcr
119580111025    4c                   endif
119590111025    3c                   endif
119600070308     c
119610070306     c                   GoTo      EndCt72
119620111025    2c                   endif
119630070306     c
119640040220      * se arrivo qua vuol dire che non posso modificare
119650140806      * Se variazione da internet permetto la modifica
119660140806     c                   if        d48cvb<>'CI'
119670031001     c                   Eval      VidMsg = Msg(126)
119680031001     c                   Eval      *In28 = *On
119690031001     c                   Eval      *In90 = *On
119700031030     c                   If        VidTcr <> ArbTcr
119710031001     c                   Eval      *In62 = *On
119720031001     c                   GoTo      EndCt72
119730031001     c                   EndIf
119740031030     c                   If        ComDcr <> ArbDcr
119750031001     c                   Eval      *In40 = *On
119760031001     c                   GoTo      EndCt72
119770031001     c                   EndIf
119780031001     c                   If        VidHcr <> ArbHcr
119790031001     c                   Eval      *In41 = *On
119800031001     c                   GoTo      EndCt72
119810031001     c                   EndIf
119820140806     c                   endif
119830031001    1c                   EndIf
119840030930
119850030930     c     EndCt72       EndSr
119860920320     C*
119870920320     C*--- AGGIORNAMENTO FILE ----------------------------------------*
119880920320     C     REGIS7        BEGSR
119890070308     c                   clear                   ModconsR          1
119900130412     c                   clear                   wpda_ndc
119910130412     c                   clear                   wpda_ddc
119920130412     c                   clear                   wpda_dcm
119930070308     C* PRENDO I VALORI ASSOLUTI
119940070308     C                   MLLZO     1             VIDHCR
119950070308     c
119960070308     c* Mi salvo i campi della consegna
119970070308     c*  richiesta della bolla per sucessivi aggiornamenti.
119980070308     c                   eval      bolladcr=arbdcr
119990070308     c                   eval      bollahcr=arbhcr
120000070308     c                   eval      bollatcr=arbtcr
120010070308     c
120020070308     c* Mi salvo anche se modificata, solo se c'e' la DATA consegna impostat
120030070308     c                   if        vidtcr<>arbtcr or comdcr<>arbdcr or
120040070308     c                             arbhcr<>vidhcr  and comdcr>0
120050070308     c                   eval      ModconsR='S'
120060070308     c                   endif
120070070308     c
120080950104     C*  GIRO UDATE
120090950104     C                   TIME                    W0140            14 0
120100950104     C* UDATE IN GGMMAAAA
120110950104     C                   MOVE      W0140         WDTGIO            8 0
120120950104     C* UDATE IN AAAAMMGG
120130950104     C                   Z-ADD     WDTGIO        G02DAT
120140950104     C                   MOVEL     *BLANK        G02ERR
120150950104     C                   CALL      'XSRDA8'
120160950104     C                   PARM                    WLBDAT
120170950104     C                   MOVEL     G02INV        DATEU             8 0
120180941213     C                   MOVEL     KNMUS         ARBPRU
120190930507     C*
120200151230     c                   if        not *in10 or wfile='A'
120210930507     C* INVIO PER SEDE
120220170116     C****               MOVEL     'FNARBG0T'    SFILE
120230930507     C* ALLOCO
120240170116     C***                EXSR      ALLOC
120250170116     C** 91              GOTO      ENDRG7
120260070320     c
120270070320     c* Se impostata la "S" o la "A" devo impostare relativo blocco
120280070320     c*  telefonate in DSBL4L
120290070320     c                   clear                   wesar4L           1
120300070320     c                   clear                   BAS               1
120310070320     c                   clear                   TFD               1
120320070320     c                   if        ((vidtc1='S' or vidtc2='S') and
120330070320     c                              arbtc1<>'S' and arbtc2<>'S')  or
120340070320     c                             ((vidtc1='A' or vidtc2='A') and
120350070320     c                              arbtc1<>'A' and arbtc2<>'A')
120360070320     c                   eval      BAS='S'
120370070320     c                   endif
120380070320     c
120390070320     c* se tolto "A" o "S" devo eliminare il blocco
120400070320     c                   if        vidtc1<>'A' and vidtc1<>'S' and
120410070320     c                              vidtc2<>'A' and vidtc2<>'S' and
120420070320     c                             (arbtc1='S' or arbtc1='A' or
120430070320     c                              arbtc2='S' or arbtc2='A')
120440070320     c                   eval      BAS='N'
120450070320     c                   endif
120460161229     c* se aggiunta consegna richiesta devo eliminare il blocco appuntamento
120470161229     c                   if        bolladcr=0 and comdcr>0 and
120480161229     c                             vidtcr=' ' and
120490161229    1c                             AggioARP<>'S' and
120500161229     c                             (vidtc1='A' or vidtc2='A')
120510161229     c                   eval      BAS='N'
120520161229     c                   endif
120530070320     c* Tolgo blocco telefonata FFD se messa consegna richiesta e
120540070320     c*  presente FFD
120550070329     c****               if        arbffd='S' and viddcr>0
120560070329     c****               eval      TFD='N'
120570070329     c****               endif
120580070320     c                   if        bas<>' ' or TFD<>' '
120590070320     c                   eval      TipoEla='C'
120600070320     c                   exsr      Gestar4L
120610070320    1c                   if        *in91
120620170116     c****               exsr      DLCSED
120630070525     C                   SETON                                        2891
120640070320     C                   GOTO      ENDRG7
120650070320    1c                   endif
120660070320     c                   endif
120670041022     c
120680041022     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
120690041022     c* iornare direttamente il file
120700041022     c                   clear                   waltrabolla       1
120710041022     C   10KARB          CHAIN     FNBLP01L                           3091
120720041022     C  n10KARB          CHAIN     FNARB01L                           3091
120730041022     c*
120740070308    1c                   if        *in91
120750170116     c****               exsr      DLCSED
120760070515     c                   unlock    fiar401l
120770070525     C                   SETON                                        2891
120780041022     C                   MOVEL     MSG(4)        VIDMSG
120790041022     C                   GOTO      ENDRG7
120800070308    1c                   endif
120810070308     c
120820070308     c* Se lna attivata con GEO
120830070308     c* Se ho aggiornato FIARP, e devo aggiornare altri dati,
120840070308     c* nei campi della consegna richeista reimposto quelli della bolla
120850070319    1c                   if        FGSgeot='S'and AggioARP='S'
120860070308     c                   eval      comdcr=Bolladcr
120870070308     c                   eval      vidhcr=bollahcr
120880070308     c                   eval      vidtcr=Bollatcr
120890070308     c                   clear                   ModConsR
120900070308     c                   endif
120910070308     c
120920041022     c                   if        *in30
120930041022     c                   eval      waltrabolla='N'
120940041022     c                   else
120950041022     c* memorizzo campi prima della variazione
120960041022     c                   eval      waltrotc1=arbtc1
120970041022     c                   eval      waltrotc2=arbtc2
120980080422     c   10              eval      waltroft3=arbft3
120990130411     c  n10              eval      wpda_ndc=arbndc
121000130411     c  n10              eval      wpda_ddc=arbddc
121010130411     c  n10              eval      wpda_dcm=arbdcm
121020141223     c  n10              eval      bollaFBC=arbfbc
121030041022     C   10KARB          CHAIN     FNARB01L                           30
121040041022     C  N10KARB          CHAIN     FNBLP01L                           30
121050041022     c* Alla fine per non sporcarmi i campi del record, richaino il
121060041022     c*  record originale che sto variando
121070130411     c                   eval      wpda_ndc=arbndc
121080130411     c                   eval      wpda_ddc=arbddc
121090130411     c                   eval      wpda_dcm=arbdcm
121100041022     c                   endif
121110151230     c                   endif
121120930507     C*
121130930507     C                   Z-ADD     DATEU         ARBDTV
121140930507     C                   TIME                    ARBORV
121150930507     C                   MOVEL     VIDCVR        ARBCVB
121160001204     C*
121170001204     C                   MOVEL     ARBTC1        SAVTC1
121180001204     C                   MOVEL     ARBTC2        SAVTC2
121190080415     c*
121200930507     C*
121210060130     C* SE DA STORICIZZARE
121220060130     C**   *IN10         IFEQ      *ON
121230060130    1C**   D66FSA        ANDEQ     'S'
121240060130     C**   *IN10         OREQ      *OFF
121250060130    1C**   D66FSP        ANDEQ     'S'
121260070308     c
121270060130    1C     D66FSA        ifeq      'S'
121280011108      *
121290011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
121300011108      * è in partenza 'P' o in arrivo 'A'
121310011108     C     *IN10         IFEQ      *ON
121320160119     c                   if        wfile='P'
121330160119     C                   MOVE      'a'           ARBPRU
121340160119     c                   else
121350011108     C                   MOVE      'A'           ARBPRU
121360160119     c                   endif
121370011108     C                   ENDIF
121380011108     C     *IN10         IFEQ      *OFF
121390011108     C                   MOVE      'P'           ARBPRU
121400011108     C                   ENDIF
121410011108      *
121420941014     C                   WRITE     FNARBG00
121430930507     C                   END
121440060301     c* storicizzazione motivazione
121450060301     c     d66mtv        ifeq      'S'
121460060301     c                   exsr      sto_mtv
121470060301     c                   endif
121480040224      * se in arrivo ho modificato uno dei dati del Fiar5 rcd GEN:
121490040224      * - Tipo/Data/Ora consegna richiesta
121500040224      * - Giorni di chiusura
121510040224      * - Consegna particolare
121520040224      * devo memorizzare i dati della bolla prima della modifica
121530150129     c******             If        *In10 and
121540150206     c* dal 06/02 momorizziamo anche se varia la partenza, come se lo facesse l'arrivo
121550150129     c                   if         (VidTcr <> ArbTcr or ComDcr <> ArbDcr or
121560040224     c                               VidHcr <> ArbHcr or VidGc1 <> ArbGc1 or
121570040224     c                               VidGc2 <> ArbGc1 or VidTc1 <> ArbTc1 or
121580170405     c                               VidTc2 <> ArbTc2 )
121590041022     c                   ExSr      RegAr5AR
121600170405     c                   Else
121610170405     c* Se tutto uguale memorizzo i dati di ripristino nel fiar5 rec GEN solo se
121620170405     c* già esistente e causale CI oppure CD
121630170405     c                   if        (d48cvb='CI' or d48cvb='CD')  and
121640170405     c                             wesar5gen='S'
121650170405     c                   ExSr      RegAr5AR
121660170405     c                   EndIf
121670170405     c                   EndIf
121680080415     c*
121690080415     c* Se modifico data consegna richiesta verifico se devo sflaggare per
121700080415     c* clienti
121710080415     c                   eval      wsflagga_C=' '
121720080422     c                   if        comdcr<>arbdcr or
121730080415     c                             vidtcr<>arbtcr
121740080415     c                   clear                   wcmitt
121750080415     c                   if        flgtb1='A'
121760080415     c                   move      arbccm        wcmitt
121770080415     c                   else
121780080415     c                   move      arbksc        wcmitt
121790080415     c                   endif
121800080416     c                   if        wcmitt>'0000000'
121810080416     c                             and %subst(wcmitt:4:4)<>'8888'
121820080415     C                   MOVEL     '3K'          COD
121830080415     C                   MOVEL(P)  wcmitt        KEY
121840080415     C                   EXSR      CTABEL
121850080415     C  N30              MOVEL     TBLUNI        DS3K
121860080415     C   30              CLEAR                   DS3K
121870080415     c                   if        §3kcq8='S'
121880080415     c                   eval      wsflagga_C='S'
121890080415     c                   endif
121900080415     c                   endif
121910080415     c                   endif
121920041022     c
121930041022     c* Se variazione in partenza e modifico  le cons.partic
121940041022     c*  sflaggo per clienti
121950041025     c  n10              if        vidtc1<>arbtc1 or vidtc2<>arbtc2
121960080415     c                             or wsflagga_c='S'
121970041022     c                   clear                   arbft3
121980041022     c                   endif
121990070319     c
122000070319     c* Se impostata la "S" o la "A" devo impostare relativo blocco
122010070319     c*  telefonate in DSBL4L
122020161229     c* idem se aggiunta la dcr devo sbloccare eventuale appuntamento
122030151230     c                   if        not *in10 or wfile='A'
122040070320     c                   if        wesar4L<>' '
122050070320     c                   if        BAS='S'
122060070320     c                   eval      §b4lbas='S'
122070070320     c                   endif
122080070320     c                   if        BAS='N'
122090070320     c                   eval      §b4lbas=' '
122100070320     c                   endif
122110070320     c
122120070320     c                   if        TFD='N'
122130070320     c                   eval      §b4lTFD=' '
122140070320     c                   endif
122150070320     c
122160070320     c                   eval      TipoEla='A'
122170070320     c                   exsr      GestAR4L
122180070320     c                   endif
122190151230     c                   endif
122200070320     c
122210920320     C*
122220920320     C                   MOVEL     COMDCR        ARBDCR
122230920320     C                   MOVEL     VIDHCR        ARBHCR
122240920320     C                   MOVEL     VIDTCR        ARBTCR
122250930507     C                   MOVEL     VIDGC1        ARBGC1
122260930507     C                   MOVEL     VIDGC2        ARBGC2
122270941102     C                   MOVEL     VIDTC1        ARBTC1
122280941102     C                   MOVEL     VIDTC2        ARBTC2
122290070308     c
122300070308     c* Se modificata consegna richiesta in arrivo
122310070308     c*  devo anche pulire il flag blocco consegna se = 'A'
122320141223     c                   if        BollaFBC='A' and ModconsR='S'
122330070308     c                             and comdcr>0
122340070308     c                   clear                   arbfbc
122350070419     c                   clear                   arbcmc
122360070308     c                   endif
122370070920     c
122380070920     c* Se modificata la consegna richiesta e bolla già consegnata
122390070920     c* effettuo il ricalcolo della affidabilità
122400070920     c                   if        ModconsR='S' and arbdcr=0 and arbdcm>0
122410070920     c                   clear                   arbdti
122420070920     c                   clear                   arbhti
122430070920     c                   movel     'S'           arbfbs
122440070920     c                   endif
122450041025     c
122460070319     c* Verifico se devo aggiornare record già esistente in ar5
122470150206     c*  ma solo per le consegne particolari e fermo deposito
122480150206     c  N10              exsr      RegAr5PA
122490941212     C*
122500930507     C* PER SEDE
122510151230     c                   if        not *in10 or wfile='A'
122520070503     C     D66FFI        IFEQ      'S'
122530950227     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
122540950413    2C     §3ABSD        IFEQ      ' '
122550930507     C*
122560170116     C***                OPEN      FNARBG0T
122570941014     C                   WRITE     FNARBGTT
122580170116     C***                CLOSE     FNARBG0T
122590950227     C                   END
122600930507     C* DISALLOCO OGGETTO
122610170116     C****               EXSR      DLCSED                                       FNARBG0T
122620950227     C                   END
122630151230     c                   endif
122640041022     c
122650930507     C*
122660151230     c                   if        wfile='A'
122670151230     C                   UPDATE    FNARB000
122680151230     c                   else
122690151230     C                   UPDATE    FNBLP000
122700151230     c                   endif
122710941220     C*
122720941220     C* DISALLOCO I RECORD CHE NON USO
122730160119     C   10              UNLOCK    FIAR601L
122740041022     c* Aggiorno anche l'altra bolla
122750151230     c                   if        not *in10 or wfile='A'
122760041022     c                   if        waltrabolla=' '
122770080422     c   10              eval      arbft3=waltroft3
122780041025     c* SE aggiorno bopartenza sflaggo per clienti se variate le cons.partic
122790041025     c                   if        *in10 and (waltrotc1<>arbtc1 or
122800080415     c                             waltrotc2<>arbtc2 or
122810080415     c                             wsflagga_c='S')
122820041022     c                   clear                   arbft3
122830041022     c                   endif
122840041022     C   10              except    arbGblp
122850041022     C  N10              except    arbGarb
122860041022     c                   endif
122870151230     c                   endif
122880950602     C*
122890070308     c* Se non è previsto l'aggiornamento di FIARP, vedo se lo devo
122900070308     c*  cancellare (se variata la data consegna richiesta)
122910070319     c                   if        FGSgeot='S'  and  ModconsR='S'
122920070308     c     karb          delete    fiarp01l
122930070308     c                   endif
122940070308     c
122950040224      * Se c'è la data consegna richiesta controllo se è ancora valida
122960040224      * solo se ho fatto una modifica in arrivo
122970150129      * da adesso anche se modifica in partenza
122980150206     c                   If        ArbDcr > *Zeros
122990040324     c                             and wCa = *Blanks and wgiac = *Blanks
123000040224     c                   ExSr      ContrDcr
123010040224     c                   EndIf
123020070308     c
123030130411     C*
123040130411     c* Richiamo routine per invio variazione a pda
123050130411     c                   if        D48Cvb<>'CS'
123060151230     c                             and (not *in10 or wfile='A')
123070130411     c                   exsr      sr_pda
123080130411     c                   endif
123090950104     C*
123100930507     C     ENDRG7        ENDSR
123110021129      **
123120021129      *--- AGGIORNAMENTO FILE FIAR5 ----------------------------------*
123130021129     c     Regis71       BegSr
123140021129
123150021129      * Se è stata indicata la consegna particolare 'S' devo scrivere
123160021129      * il file FIAR5
123170021129     c                   If        VidTc1 = 'S' or VidTc2 = 'S'
123180021129     c                   Clear                   Fiar5000
123190021129     c                   Eval      Ar5Aas = ArbAas
123200021129     c                   Eval      Ar5Lnp = ArbLnp
123210021129     c                   Eval      Ar5Nrs = ArbNrs
123220021129     c                   Eval      Ar5Nsp = ArbNsp
123230021129     c                   Eval      Ar5Trd = 'SCR'
123240021129     c                   Move      Dateu         Ar5Dac
123250021129     c                   Movel     W0140         Ar5Orc
123260021129     c                   Clear                   Dar5Scr
123270021129     c                   Eval      §ar5Cvb = D48Cvb
123280021129     c                   Eval      §ar5Pru = Knmus
123290060131     c                   Eval      §ar5Pou = dutPou
123300021129     c                   Eval      §ar5Tcr = VidTcr
123310021129     c                   Eval      §ar5Dcr = ComDcr
123320021129     c                   Eval      §ar5Hcr = VidHcr
123330021129     c                   Eval      §ar5Gc1 = VidGc1
123340021129     c                   Eval      §ar5Gc2 = VidGc2
123350021129     c                   Eval      §ar5Tc1 = VidTc1
123360021129     c                   Eval      §ar5Tc2 = VidTc2
123370021129     c                   Eval      §ar5Nom = VidNom
123380021129     c                   Eval      §ar5Tel = VidTel
123390021129     c                   Eval      §ar5Mot = VidMot
123400021129     c                   Movel     Dar5Scr       Ar5Uni
123410021129     c                   Write     Fiar5000
123420021129     c                   EndIf
123430040705
123440040705      * Aggiorno anche il record 'GEN' di Fiar5 con il nuovo Telefono e Referente
123450091217     c                   eval      Wtipoagg='REF'
123460091217     c                   exsr      RegisAR5GEN
123470091217     c***
123480091217     c***                Eval      kAr5Trd = 'GEN'
123490091217     c***                Clear                   dAr5Gen
123500091217     c***  kAr5          Chain     Fiar501l
123510040705      * Aggiorno Fiar5
123520091217if  1c***                If        %Found(Fiar501l)
123530091217     c***                Eval      dAr5Gen = Ar5Uni
123540091217     c***                Eval      §Ar5Ref = VidNom
123550091217     c***                Eval      §Ar5Teld = VidTel
123560091217     c***                Movel(p)  dAr5Gen       Ar5Uni
123570040705      * se la modifica è fatta in partenza e devo trasmettere all'arrivo
123580040705      * sfleggo per la trasmissione
123590070306     c***                If        Not *In10 and §Ar5Trar = 'S'
123600070306     c***                Clear                   Ar5Ft1
123610070306     c***                EndIf
123620040705      * se la modifica è fatta in arrivo e devo trasmettere alla partenza
123630040705      * sfleggo per la trasmissione
123640070306     c***                If        *In10 and §Ar5Trpa = 'S'
123650070306     c***                Clear                   Ar5Ft1
123660070306     c***                EndIf
123670040705      * se devo trasmettere alla sede sfleggo per la trasmissione
123680091217     c***                If        §Ar5Trse = 'S'
123690091217     c***                Clear                   Ar5Ft2
123700091217     c***                EndIf
123710040705      * sfleggo sempre per la trasmissione al cliente
123720091217     c***                Clear                   Ar5Ft3
123730091217     c***                Update    Fiar5000
123740040705      * Scrivo Fiar5
123750091217   x1c***                Else
123760040705      * se immessi i dati
123770091217     c***                Clear                   Fiar5000
123780091217     c***                Eval      Ar5Aas = ArbAas
123790091217     c***                Eval      Ar5Lnp = ArbLnp
123800091217     c***                Eval      Ar5Nrs = ArbNrs
123810091217     c***                Eval      Ar5Nsp = ArbNsp
123820091217     c***                Eval      Ar5Trd = 'GEN'
123830091217     c***                Move      Dateu         Ar5Dac
123840091217     c***                Movel     W0140         Ar5Orc
123850091217     c***                Clear                   Dar5Gen
123860091217     c***                Eval      §Ar5Ref = VidNom
123870091217     c***                Eval      §Ar5Teld = VidTel
123880091217     c***                Movel(p)  DAr5Gen       Ar5Uni
123890091217     c***                Write     Fiar5000
123900091217    1c***                EndIf
123910130411     c* Richiamo routine per invio variazione a pda
123920130411     c                   exsr      sr_pda
123930021129
123940021129     c                   EndSr
123950040223
123960041022      *--- CONTROLLO E AGGIORNAMENTO FIAR5 RCD GEN se sono in arrivo--*
123970041022     c     RegAr5AR      BegSr
123980070309     c* Se Attiva GEO e bolla non in distnta e non giacente con
123990070309     c*  un evento che prevede la modifica della cons.richista
124000070309     c*  memorizzo la cons richiesta nei campi di FIAR5
124010070309     c                   clear                   AggioAR5M         1
124020070309     c
124030070319     c                   if        arbndc=0 and FGSgeot='S' and
124040070309     c                             arbfbc<>'G' and eveADR='S'
124050070309     c                   eval      AggioAR5M='S'
124060070309     c                   endif
124070160114     c* Se manutenzione in Arrivo su bolla ancora da partire memorizzo
124080160114     c* di aggiornare i dati del ripristino con quelli del video
124090160114     c                   if        *in10 and wfile = 'P'
124100160114     c                   eval      AggioAR5M='S'
124110160114     c                   endif
124120040223
124130040224     c                   Clear                   waggar5
124140040223     c                   Clear                   dAr5Gen
124150040223     c                   Eval      kAr5Trd = 'GEN'
124160040223     c     kAr5          Chain     Fiar501l
124170040223     c                   If        %Found(Fiar501l)
124180040223     c                   Movel     Ar5Uni        dAr5Gen
124190040223     c                   EndIf
124200040223
124210040223      * Dati non memorizzati
124220040223if  1c                   If        §Ar5Mdp <> 'S'
124230040224     c                   Eval      §Ar5Mdp = 'S'
124240040223     c                   Eval      §Ar5gTcr = ArbTcr
124250040223     c                   Move      ArbDcr        §Ar5gDcr
124260040223     c                   Move      ArbHcr        §Ar5gHcr
124270040223     c                   Eval      §Ar5gGc1 = ArbGc1
124280040223     c                   Eval      §Ar5gGc2 = ArbGc2
124290040223     c                   Eval      §Ar5gTc1 = ArbTc1
124300040223     c                   Eval      §Ar5gTc2 = ArbTc2
124310040223     c                   Eval      §Ar5Ffd  = ArbFfd
124320040223      * Causale 'CD' da chiusura distinta
124330140806      *   oppure
124340140806      * Causale 'CI' da indicazioni da internet
124350040223      * imposto i campi a video
124360140806if  2c                   If        d48cvb = 'CD' or d48cvb='CI' or AggioAR5M='S'
124370040223     c                   Eval      §Ar5Tcrm = VidTcr
124380040223     c                   Move      ComDcr        §Ar5Dcrm
124390040223     c                   Move      VidHcr        §Ar5Hcrm
124400040223      * Altra causale
124410040223      * imposto i campi di arb prima della variazione
124420040223   x2c                   Else
124430040223     c                   Eval      §Ar5Tcrm = ArbTcr
124440040223     c                   Move      ArbDcr        §Ar5Dcrm
124450040223     c                   Move      ArbHcr        §Ar5Hcrm
124460040223    2c                   EndIf
124470040224     c                   Eval      waggar5 = 'S'
124480040223      * Dati memorizzati
124490040223   x1c                   Else
124500140806if  2c                   If        d48cvb = 'CD' or d48cvb='CI' or AggioAR5M='S'
124510040223     c                   Eval      §Ar5Tcrm = VidTcr
124520040223     c                   Move      ComDcr        §Ar5Dcrm
124530040223     c                   Move      VidHcr        §Ar5Hcrm
124540040224     c                   Eval      waggar5 = 'S'
124550040223    2c                   EndIf
124560040223    1c                   EndIf
124570150206     c* se si tratta di una modifica di cons richiesta in partenza lo memorizzo se
124580150206     c*  non l'ho già fatto
124590150206     c                   if        not *in10 and comdcr<>arbdcr and
124600150206     c                             §ar5dcrp<>'P'  and comdcr>0
124610150206     c                   eval      §ar5dcrp= 'P'
124620150206     c                   Eval      waggar5 = 'S'
124630150206    2c                   EndIf
124640150206     c                   if        not *in10 and comdcr<>arbdcr and
124650150206     c                             §ar5dcrp= 'P'  and comdcr=0
124660150206     c                   eval      §ar5dcrp= ' '
124670150206     c                   Eval      waggar5 = 'S'
124680150206    2c                   EndIf
124690150206     c
124700040223
124710040224if  1c                   If        waggar5 = 'S'
124720040224      * aggiorno Fiar5
124730040224if  2c                   If        %Found(Fiar501l)
124740040224     c                   Movel(p)  dAr5Gen       Ar5Uni
124750040224      * se devo trasmettere alla sede sfleggo per la trasmissione
124760040224     c                   If        §Ar5Trse = 'S'
124770040224     c                   Clear                   Ar5Ft2
124780040224     c                   EndIf
124790040224      * sfleggo sempre per la trasmissione al cliente
124800040224     c                   Clear                   Ar5Ft3
124810040223     c                   Update    Fiar5000
124820040224      * scrivo Fiar5
124830040224   x2c                   Else
124840040224     c                   Clear                   Fiar5000
124850040223     c                   Eval      Ar5Aas = ArbAas
124860040223     c                   Eval      Ar5Lnp = ArbLnp
124870040223     c                   Eval      Ar5Nrs = ArbNrs
124880040223     c                   Eval      Ar5Nsp = ArbNsp
124890040224     c                   Eval      Ar5Trd = 'GEN'
124900040223     c                   Move      Dateu         Ar5Dac
124910040223     c                   Movel     W0140         Ar5Orc
124920040224     c                   Movel(p)  dAr5Gen       Ar5Uni
124930041007     c* flaggo sempre il flag di filiale tanto non trasmettiamo più
124940040223     c                   Write     Fiar5000
124950040224    2c                   EndIf
124960040224   x1c                   Else
124970040224     c                   Unlock    Fiar501l
124980040224    1c                   EndIf
124990040223
125000040223     c                   EndSr
125010041022
125020041022      *--- CONTROLLO E AGGIORNAMENTO FIAR5 RCD GEN se sono in partenz-*
125030041022     c     RegAr5PA      BegSr
125040041022     c                   Clear                   dAr5Gen
125050041022     c                   Eval      kAr5Trd = 'GEN'
125060041022     c     kAr5          Chain     Fiar501l
125070041022if  2c                   If        %Found(Fiar501l)
125080041022     c                   Movel     Ar5Uni        dAr5Gen
125090041022      * Flag dati memorizzati
125100041022if  3c                   If        §Ar5Mdp = 'S'
125110041022      * Aggiorno i dati di ripristino solo se uguali a quelli salvati
125120150206if  4c***                If        §Ar5gTcr= §Ar5Tcrm and §Ar5gDcr = §Ar5Dcrm
125130150206     c***                          and §Ar5gHcr = §Ar5Hcrm
125140150206     c***                Eval      §Ar5Tcrm = ArbTcr
125150150206     c***                Move      ArbDcr        §Ar5Dcrm
125160150206     c***                Move      ArbHcr        §Ar5Hcrm
125170150206    4c***                EndIf
125180041022      * Memorizzo i dati della bolla
125190150206     c***                Eval      §Ar5gTcr = ArbTcr
125200150206     c***                Move      ArbDcr        §Ar5gdcr
125210150206     c***                Move      ArbHcr        §Ar5gHcr
125220150206     c***                Eval      §Ar5gGc1 = ArbGc1
125230150206     c***                Eval      §Ar5gGc2 = ArbGc2
125240150206     c* Da 02/15
125250150206     c* come partenza aggiorno soltanto i dati del fermo deposito e dei giorni di chiusura
125260041022     c                   Eval      §Ar5gTc1 = ArbTc1
125270041022     c                   Eval      §Ar5gTc2 = ArbTc2
125280041022     c                   Eval      §Ar5Ffd = arbFfd
125290041022    3c                   EndIf
125300041022     c                   Movel     Dar5Gen       Ar5Uni
125310041022     c                   Update    Fiar5000
125320041022    2c                   EndIf
125330041022     c                   EndSr
125340070906
125350070906      *--- CONTROLLO SE ANCORA VALIDA Le consegne particolari --------*
125360070906     c     ContrTCP      BegSr
125370070910      *
125380070906     c                   clear                   eliminatcp        1
125390070911     c                   clear                   wcomb
125400070911     c                   clear                   aggtcp            1
125410070906     c
125420070910    1c                   if        arbtc1=wdisagdest
125430070910if  2c                   If        (par5Mdp = 'S' and
125440070910     c                             par5gtc1<>arbtc1 and par5gtc2<>arbtc1)
125450070911     c*******                      or   par5Mdp=' '
125460070906     c                   eval      eliminatcp='1'
125470070911     c                   eval      wtc1=arbtc1
125480070907    2c                   endif
125490070910    1c                   endif
125500070910    1c                   if        arbtc2=wdisagdest
125510070910if  2c                   If        (par5Mdp = 'S' and
125520070910     c                             par5gtc1<>arbtc2 and par5gtc2<>arbtc2)
125530070911     c********                     or   par5Mdp=' '
125540070906     c                   eval      eliminatcp='2'
125550070911     c                   eval      wtc1=arbtc2
125560070907    2c                   endif
125570070910    1c                   endif
125580070906     c* Se da eliminare
125590070910    1c                   if        eliminatcp<>' '
125600070911     c* Verifico se per le combinazioni devo ripristinare la cons partic
125610070911     c*  della partenza
125620070911     c                   if        par5mdp='S' and par5gtc1<>' '
125630070911     c                   eval      wtc2=par5gtc1
125640070911     c                   exsr      sr_chkcomb
125650070911     c
125660070911     c                   if        wcomb<>' '
125670070911     c                   eval      aggtcp=par5gtc1
125680070911     c                   endif
125690070912     c                   endif
125700070911     c
125710070911     c                   if        wcomb=' ' and par5gtc2<>' '
125720070911     c                   eval      wtc2=par5gtc1
125730070911     c                   exsr      sr_chkcomb
125740070911     c                   if        wcomb<>' '
125750070911     c                   eval      aggtcp=par5gtc2
125760070911     c                   endif
125770070911     c                   endif
125780070911     c
125790070906     c                   Clear                   dsarbg
125800070906     c                   Eval      d48cvb = 'CA'
125810070906     c                   Eval      weseg = 'E'
125820070911     c*****              Clear                   d48ffr
125830070906     c                   Eval      d48Trc = 'G'
125840070906     c                   Eval      §bgTcr = arbtcr
125850070906     c                   Move      arbdcr        §bgDcr
125860070906     c                   Move      arbhcr        §bgHcr
125870070906     c                   Eval      §bgGc1 = Arbgc1
125880070906     c                   Eval      §bgGc2 = ArbGc2
125890070910    2c                   if        eliminatcp='1'
125900070906     c                   clear                   §bgtc1
125910070906     c                   clear                   arbtc1
125920070906     c                   Eval      §bgtc2 = Arbtc2
125930070911     c* ripristino la vecchia consegna partic.
125940070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
125950070911     c                   eval      §bgtc1=aggtcp
125960070911     c                   endif
125970070910   x2c                   else
125980070906     c                   clear                   §bgtc2
125990070906     c                   clear                   arbtc2
126000070906     c                   Eval      §bgtc1 = Arbtc1
126010070911     c* ripristino la vecchia consegna partic.
126020070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
126030070911     c                   if        §bgtc1=' '
126040070911     c                   eval      §bgtc1=aggtcp
126050070911     c                   else
126060070911     c                   eval      §bgtc1=aggtcp
126070070911     c                   endif
126080070911     c                   endif
126090070910    2c                   endif
126100070907     c
126110070907     c* Memorizzo in skiera rielaborazioni
126120070910    2c                   if        kk<=10
126130070907     c                   add       1             kk
126140070907     c                   movel     dsarbg        kdati(KK)
126150070907     c                   movel     d48cvb        kcvb(KK)
126160070907     c                   movel     d48trc        ktrc(KK)
126170070911     c                   movel     ' '           kffr(KK)
126180070911     c                   movel     ' '           kf12(KK)
126190070910    2c                   endif
126200070910    1c                   endif
126210070906     c
126220070906     c                   EndSr
126230040224
126240040224      *--- CONTROLLO SE ANCORA VALIDA LA DATA CONSEGNA RICHIESTA -----*
126250160114     c     ContrDcr      BegSr
126260040225
126270040225     c                   Move      ArbDcr        warbdcr
126280050907      * richiamo il controllo della "7Q"
126290050907     c                   ExSr      Contr7q
126300040225
126310040225      * Controllo se c'è il fiar5 rcd GEN
126320070906     c                   clear                   dar5gen
126330040225     c                   Eval      kAr5Trd = 'GEN'
126340040225     c     kAr5          Chain(n)  Fiar501l
126350040225     c                   If        %Found(Fiar501l)
126360040225     c                   Movel     Ar5Uni        dAr5Gen
126370040225     c                   EndIf
126380040225      * se ho memorizzato i dati
126390040225if  1c                   If        §Ar5Mdp = 'S' and
126400040225      * e la data è diversa
126410040225     c                             (wArbDcr <> §Ar5Dcrm or
126420040225     c                               ArbTcr <> §Ar5Tcrm)
126430040225      * controllo se la data è valida quando:
126440040224      *                     Peso o volume da fatturare sono >= a quanto tabellato in VPO
126450040225if  2c                   If        ArbPkf >= §VpoPkg or
126460040224     c                             ArbVlf >= §VpoVlm or
126470150506      *                     Destinatario supermercato o Appuntamento
126480040224     c                             ArbTc1 = 'S' or ArbTc1 = 'A' or
126490040224     c                             ArbTc2 = 'S' or ArbTc2 = 'A' or
126500150506     c                             ArbTc1 = 'Z' or ArbTc2 = 'Z' or
126510040312      *                     Lasciato avviso
126520070308     c                             BollaFbc = 'A' or
126530040224      *                     Fermo deposito
126540040224     c                             ArbFfd = 'S' or
126550040224      *                     Tipo bolla "FW" Recapito c/assegni
126560040811     c                             ArbCbo = 'FW'or
126570040811      *                     Periodo trazione ridotte e bolla con particolarità "G8"
126580040811     c                             wDCRG8 = 'S'
126590040225     c                   Goto      EndContr
126600040225    2c                   EndIf
126610040224      *                     C/assegno non abilitato
126620040225if  2c                   If        Ar9Cas > 0
126630040224     c                   Clear                   Trul21ds
126640040224     c                   Eval      I21Tla = 'L'
126650040224     c                   Eval      I21Cbo = ArbCbo
126660040224     c                   Eval      I21Tsp = ArbTsp
126670040224     c                   Eval      I21Lnp = ArbLnp
126680040224     c                   Eval      I21Nzm = ArbNzm
126690040224     c                   Eval      I21Lna = ArbLna
126700040224     c                   Eval      I21Nzd = ArbNzd
126710130827     c*****              Eval      I21Ksc = ArbKsc
126720040224     c                   Movel     ArbCtr        I21Ctr
126730040224     c                   Eval      I21Tic = Ar9Tic
126740040224     c                   Eval      I21Imp = Ar9Cas
126750040224     c                   Eval      I21Div = Ar9Vca
126760040224     c                   Eval      I21Ute = Knmus
126770040224     c                   Eval      I21Pgm = nompgm
126780130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
126790130827     c                   if        flgtb1='F'
126800130827     c                   z-add     arbksc        i21ksc
126810130827     c                   else
126820130827     c                   z-add     arbccm        i21ksc
126830130827     c                   if        i21ksc=0
126840130827     c                   z-add     8888          i21ksc
126850130827     c                   movel     arblnp        i21ksc
126860130827     c                   endif
126870130827     c                   endif
126880040224     c                   Call      'TRUL21R'
126890040224     c                   Parm                    Trul21ds
126900040225if  3c                   If        O21Fca <> *Blanks
126910040225     c                   Goto      EndContr
126920040225    3c                   EndIf
126930040225    2c                   EndIf
126940070320     c
126950070320     c* Se FFD immesso dalla partenza, anche se già tolto, posso
126960070320     c*  manutenzionare la spedizione
126970160114    1c                   if        §ar5MDP='S'  and §ar5FFD='S'
126980070320     c                   GoTo      EndContr
126990070911    1c                   EndIf
127000150206     c* se la cons richiesta non è stata immessa dalla partenza e sono in partenza
127010150206     c*  ripristino
127020160114     c                   if        (not *in10 and  §ar5dcrp= 'P')
127030160114     c                             or (*in10 and wfile='P')
127040150206     c                   GoTo      EndContr
127050150206    1c                   EndIf
127060150206     c
127070040225      * in questo caso devo reimpostare la data consegna richiesta come indicato in Fiar5
127080040225      * facendo una variazione di tipo 'CA'
127090040225      * senza farla vedere a video
127100040225     c                   Clear                   dsarbg
127110040225     c                   Eval      d48cvb = 'CA'
127120040225     c                   Eval      weseg = 'E'
127130070911     c*****              Clear                   d48ffr
127140040225     c                   Eval      d48Trc = 'G'
127150040324     c                   Eval      §bgTcr = §Ar5Tcrm
127160040324     c                   Move      §Ar5Dcrm      §bgDcr
127170040324     c                   Move      §Ar5Hcrm      §bgHcr
127180040315     c                   Eval      §bgGc1 = Arbgc1
127190040315     c                   Eval      §bgGc2 = ArbGc2
127200040315     c                   Eval      §bgTc1 = ArbTc1
127210040315     c                   Eval      §bgTc2 = ArbTc2
127220040225     c                   Eval      wCA = 'S'
127230070906     c* Se da eliminare anche la consegna particolare imposto
127240070906     c*  insieme la variazione
127250070911    1c                   if        eliminatcp='1'
127260070906     c                   clear                   §bgtc1
127270070911     c* ripristino la vecchia consegna partic.
127280070911    2c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
127290070911     c                   eval      §bgtc1=aggtcp
127300070911    2c                   endif
127310070911    1c                   endif
127320070911    1c                   if        eliminatcp='2'
127330070906     c                   clear                   §bgtc2
127340070911    2c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
127350070911    3c                   if        §bgtc1=' '
127360070911     c                   eval      §bgtc1=aggtcp
127370070911     c                   else
127380070911     c                   eval      §bgtc2=aggtcp
127390070911    3c                   endif
127400070911    2c                   endif
127410070911    1c                   endif
127420070906     c
127430070906     c* Memorizzo in skiera rielaborazioni
127440070906     c                   if        kk<=10
127450070907     c* se devo eliminare una consegna particolare, memorizzo nella
127460070907     c* stessa variazione
127470070907     c                   if        eliminatcp=' '
127480070907     c                   add       1             kk
127490070907     c                   endif
127500070906     c                   movel     dsarbg        kdati(KK)
127510070906     c                   movel     d48cvb        kcvb(KK)
127520070907     c                   movel     d48trc        ktrc(KK)
127530070911     c                   movel     ' '           kffr(KK)
127540070911     c                   movel     ' '           kf12(KK)
127550070906     c                   endif
127560070906     c
127570040225    1c                   EndIf
127580040224
127590040225     c     EndContr      EndSr
127600040224
127610070307     C*--- AGGIORNAMENTO FILE proposte FIARP00F ----------------------*
127620070307     C     REGISARP      BEGSR
127630070307     C                   MLLZO     1             VIDHCR
127640070307     c     karb          chain     fiarp01l
127650070307     c                   clear                   fiarp000
127660070307     c                   eval      arpaas=arbaas
127670070307     c                   eval      arplnp=arblnp
127680070307     c                   eval      arpnrs=arbnrs
127690070307     c                   eval      arpnsp=arbnsp
127700070307     c                   eval      arptcr=vidtcr
127710070307     c                   eval      arpdcr=comdcr
127720070307     c                   eval      arphcr=vidhcr
127730070308     C*
127740070308     C                   TIME                    W0140            14 0
127750070308     C* UDATE IN AAAAMMGG
127760070308     C                   MOVE      W0140         WDTGIO            8 0
127770070308     C                   Z-ADD     WDTGIO        G02DAT
127780070307     C                   MOVEL     *BLANK        G02ERR
127790070307     C                   CALL      'XSRDA8'
127800070307     C                   PARM                    WLBDAT
127810070307     C                   MOVEL     G02INV        arpdim            8 0
127820070307     c                   time                    arpoim
127830070308     C                   MOVEL     KNMUS         ARPPRU
127840070307     c                   if        %found(fiarp01l)
127850070307     c                   update    fiarp000
127860070307     c                   else
127870070307     c                   write     fiarp000
127880070307     c                   endif
127890070307     c
127900070307     c                   ENDSR
127910941223     C*
127920941223     C*--- CONTROLLI CAMPI MITTENTE ----------------------------------*
127930941223     C     CONTR8        BEGSR
127940941223     C                   SETOFF                                       9028
127950950104     C                   CLEAR                   VIDFAP
127960941223     C*
127970941223     C* CONTROLLO IL CODICE DEL MITTENTE
127980970813     C                   EXSR      CTRCOM
127990970822     C*
128000970822     C* SE PASSO DA CODIFICATO AD 8888 O VICEVERSA SPROTEGGO O PROTEGGO
128010970822     C* CAMPI DELL'INDIRIZZO
128020970822     C     VIDKSC        IFEQ      8888
128030970822     C     MEMKSC        ANDNE     8888
128040970822     C                   MOVE      VIDKSC        MEMKSC
128050970822     C                   SETOFF                                       22
128060970822     C                   SETON                                        90
128070970822     C                   END
128080970822     C*
128090970822     C     VIDKSC        IFNE      8888
128100970822     C     MEMKSC        ANDEQ     8888
128110970822     C                   MOVE      VIDKSC        MEMKSC
128120970822     C                   SETON                                        2290
128130970822     C                   CLEAR                   VIDRSM
128140970822     C                   CLEAR                   VIDINM
128150970822     C                   CLEAR                   VIDCAM
128160970822     C                   CLEAR                   VIDLOM
128170970822     C                   CLEAR                   VIDPRM
128180970822     C                   CLEAR                   VIDNZM
128190970822     C                   CLEAR                   VIDPIM
128200970822     C                   END
128210950113     C**
128220950113     C     *IN90         IFEQ      *ON
128230950113     C  N48              GOTO      ENDCT8
128240950113     C*
128250950113     C* X IL CODICE TARIFFA DO ERRORE SOLO SE NON E' STATO PREMUTO F10
128260950114     C  NKJ
128270950114     CAN 28              GOTO      ENDCT8
128280950113     C                   ENDIF
128290941223     C*
128300020508     C* SE IMMESSO UN CODICE 8888 OBBLIGATORIO l'indirizzo completo
128310020508    1C     VIDKSC        IFEQ      8888
128320970808     C                   MOVEL     VIDRSM        I14RSC
128330970808     C                   CLEAR                   I14RS2
128340970808     C                   MOVEL     VIDINM        I14IND
128350970808     C                   MOVEL     VIDLOM        E14LOC
128360970808     C                   MOVEL     VIDPIM        E14PIP
128370970808     C                   MOVEL     VIDPRM        E14PRV
128380970808     C                   MOVEL     VIDNZM        E14NAR
128390970808     C                   MOVEL     VIDCAM        E14CAP
128400970808     C                   CLEAR                   I14ISO
128410970808     C                   MOVEL     'M'           WCLI              1
128420970808     C                   EXSR      CTRIND
128430941223     C*
128440941223     C* CAMPI CHE POSSO AVER IMPOSTATO
128450970808     C                   MOVEL     E14LOC        VIDLOM
128460970808     C                   MOVEL     E14PIP        VIDPIM
128470970808     C                   MOVEL     E14PRV        VIDPRM
128480970808     C                   MOVEL     E14NAR        VIDNZM
128490970808     C                   MOVEL     E14CAP        VIDCAM
128500950104     C* SALVO IL FLAG ANTEPORTO
128510970808     C                   MOVEL     O95ISO        COMFAP
128520970808     C                   MOVEL     O95ISO        VIDFAP
128530140403     c* salvo pickup 2
128540140403     C                   MOVEL     O95gf2        comgf2
128550941223     C**
128560970808     C* ERRORE
128570970808     C   90              GOTO      ENDCT8
128580950522     C* P.IVA obbligatoria se nazione estera e c'è flag efta in
128590950522     C* tabella nazione
128600950522     C     WDESIE        IFEQ      'E'
128610950522     C     DESEFT        ANDNE     *BLANKS
128620950412     C     VIDPIM        ANDEQ     *BLANKS
128630950412     C                   MOVEL     MSG(67)       VIDMSG
128640950412     C                   SETON                                        284990
128650950412     C                   GOTO      ENDCT8
128660950412     C                   END
128670941223     c*
128680941223   X1C                   ELSE
128690950104     c*
128700950104     C* VEDO SE ESISTE CNIND
128710950104     C     KACO          CHAIN     CNIND000                           32
128720950104     C*
128730950104     C     *IN32         IFEQ      *ON
128740950104     C                   MOVEL     MSG(63)       VIDMSG
128750950104     C                   SETON                                        479028
128760950104     C                   GOTO      ENDCT8
128770950104     C                   ENDIF
128780950104     C*
128790950104     C* CONTROLLO SUL CAP PROVINCIA NAZIONE
128800970423     C     INDSTA        IFEQ      *BLANKS
128810970423     C     INDPRV        ANDEQ     *BLANKS
128820950104     C                   MOVEL     MSG(64)       VIDMSG
128830950104     C                   SETON                                        479028
128840950104     C                   GOTO      ENDCT8
128850950104     C                   ENDIF
128860950104     C* CAP OBBLIGATORIO : CONTROLLO SE CONGRUENZA CON PROVINCIA
128870030612     C                   CLEAR                   Tisi95DS
128880030612     C                   CLEAR                   Fnlv13DS
128890970811     C                   MOVEL     INDSTA        I95NAR
128900970811     C                   MOVEL     INDCAE        I95CAP
128910970811     C                   MOVEL     INDPRV        I95PRV
128920970811     C                   MOVEL     INDCIT        I95LOC
128930970811     C                   MOVEL     '3'           I95TCN
128940970811     C                   MOVEL     ARBDSP        I95DAT
128950970811     C* ERRORE PER AFFIDABILITA' = 0
128960970811     C                   MOVEL     'S'           I13AF0
128970970811     C* CONTROLLO CONGRUENZA CAP/PROVINCIA
128980970811     C                   MOVEL     'S'           I13CNG
128990970811     C* CONTROLLO NON FORZABILE CITTA' CON VIARIO
129000970811     C                   MOVEL     'S'           I13CNV                         CAP CON VIAR
129010020305     c     lnpntw        ifeq      'FED'
129020020305     C                   movel     'S'           i95fi1
129030020305     c                   endif
129040970811     C                   CALL      'FNLV13R'
129050970811     C                   PARM                    KPJBA
129060030612     C                   PARM                    Fnlv13DS
129070030612     C                   PARM                    Tisi95DS
129080950104     C* ERRORE
129090970811     C     O13ERR        IFNE      *BLANKS
129100970811     C                   MOVEL     O13MSG        VIDMSG
129110950104     C                   MOVE      CPDC          VIDMSG
129120950104     C                   SETON                                        904728
129130950104     C                   GOTO      ENDCT8
129140950104    3C                   ENDIF
129150950104     C* SALVO IL FLAG ANTEPORTO
129160970811     C                   MOVEL     O95ISO        COMFAP
129170950104     C* SOLO IN VISUALIZZAZIONE
129180970811     C                   MOVEL     O95ISO        VIDFAP
129190140403     c* salvo pickup 2
129200140403     C                   MOVEL     O95gf2        comgf2
129210950104    2C                   ENDIF
129220941223     C*
129230941227     C     ENDCT8        ENDSR
129240961218     C*
129250941223     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE MT -----------------*
129260941227     C     REGIS8        BEGSR
129270130412     c                   clear                   wpda_ndc
129280130412     c                   clear                   wpda_ddc
129290130412     c                   clear                   wpda_dcm
129300950113     C                   MOVEL     ARBRSM        OLDRSM
129310950113     C                   MOVEL     ARBKSC        OLDKSC
129320950113     C                   MOVEL     ARBCAM        OLDCAM
129330041027     C                   MOVEL     ARBcpi        savCpi
129340950104     C*  GIRO UDATE
129350950104     C                   TIME                    W0140            14 0
129360950104     C* UDATE IN GGMMAAAA
129370950104     C                   MOVE      W0140         WDTGIO            8 0
129380950104     C*
129390950104     C* UDATE IN AAAAMMGG
129400950104     C                   Z-ADD     WDTGIO        G02DAT
129410950104     C                   MOVEL     *BLANK        G02ERR
129420950104     C                   CALL      'XSRDA8'
129430950104     C                   PARM                    WLBDAT
129440950104     C                   MOVEL     G02INV        DATEU             8 0
129450950104     C*
129460941223     C                   MOVEL     KNMUS         ARBPRU
129470950104     C*
129480950104     C* SALVO ARBCCM DELLA BOLLA PERCHE LO USO
129490950104     C                   MOVEL     ARBCCM        W0070             7 0
129500941223     C*
129510941223     C* INVIO PER SEDE
129520170116     C***                MOVEL     'FNARBM0T'    SFILE
129530941223     C* ALLOCO
129540170116     C***                EXSR      ALLOC
129550941223     C* ERRORI IN ALLOCAZIONE
129560170116    1C***  *IN91         IFEQ      *ON
129570170116     C***                GOTO      ENDRG8
129580170116    1C***                ENDIF
129590041025     c
129600041025     c* alloco record bolla in altro file
129610041025     c                   clear                   waltrabolla       1
129620041025     C   10KARB          CHAIN     FNBLP01L                           3091
129630041025     C  n10KARB          CHAIN     FNARB01L                           3091
129640041025     c*
129650041025     c                   if        *in91
129660041025     C                   MOVEL     MSG(4)        VIDMSG
129670170116     c****               exsr      DLCSED
129680070525     C                   SETON                                        2891
129690041025     C                   GOTO      ENDRG8
129700041025     c                   else
129710041025     c                   if        *in30
129720041025     c                   eval      waltrabolla='N'
129730041025     c                   else
129740041025     c                   eval      waltrocpi=arbcpi
129750041025     c                   eval      waltrofvf=arbfvf
129760041025     c                   eval      waltrovlf=arbvlf
129770130411     c  n10              eval      wpda_ndc=arbndc
129780130411     c  n10              eval      wpda_ddc=arbddc
129790130411     c  n10              eval      wpda_dcm=arbdcm
129800041025     C   10KARB          CHAIN     FNARB01L                           30
129810041025     C  N10KARB          CHAIN     FNBLP01L                           30
129820041025     c* Alla fine per non sporcarmi i campi del record, richaino il
129830041025     c*  record originale che sto variando
129840130411     c   10              eval      wpda_ndc=arbndc
129850130411     c   10              eval      wpda_ddc=arbddc
129860130411     c   10              eval      wpda_dcm=arbdcm
129870041025     c                   endif
129880041025     c                   endif
129890941223     C*
129900941223     C                   Z-ADD     DATEU         ARBDTV
129910941223     C                   TIME                    ARBORV
129920941223     C                   MOVEL     VIDCVR        ARBCVB
129930941223     C*
129940060130     C* SE DA STORICIZZARE
129950060130     C**   *IN10         IFEQ      *ON
129960060130    1C**   D66FSA        ANDEQ     'S'
129970060130     C**   *IN10         OREQ      *OFF
129980060130    1C**   D66FSP        ANDEQ     'S'
129990060130    1C     D66FSA        ifeq      'S'
130000941223     C                   Z-ADD     ARBKSC        ARBCCM
130010011108      *
130020011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
130030011108      * è in partenza 'P' o in arrivo 'A'
130040011108     C     *IN10         IFEQ      *ON
130050011108     C                   MOVE      'A'           ARBPRU
130060011108     C                   ENDIF
130070011108     C     *IN10         IFEQ      *OFF
130080011108     C                   MOVE      'P'           ARBPRU
130090011108     C                   ENDIF
130100011108      *
130110941223     C                   WRITE     FNARBM00
130120941223    1C                   END
130130060302     c* storicizzazione motivazione
130140060302     c     d66mtv        ifeq      'S'
130150060302     c                   exsr      sto_mtv
130160060302     c                   endif
130170941223     C*
130180950104     C                   Z-ADD     VIDKSC        ARBKSC
130190950104     C                   MOVEL     VIDKLP        ARBKSC
130200950104     C                   MOVEL     ARBKSC        ARBCCM
130210970613     C     ARBFDN        IFEQ      ' '
130220970613     C                   MOVEL     COMFAP        ARBFAP
130230970613     C                   ELSE
130240970613     C                   MOVEL     'C'           ARBFAP
130250970613     C                   ENDIF
130260970613     C**
130270020508    1C     VIDKSC        IFEQ      8888
130280941223     C                   MOVEL     VIDRSM        ARBRSM
130290941223     C                   MOVEL     VIDINM        ARBINM
130300941223     C                   MOVEL     VIDLOM        ARBLOM
130310941223     C                   MOVEL     VIDCAM        ARBCAM
130320941223     C                   MOVEL     VIDPRM        ARBPRM
130330941223     C                   MOVEL     VIDNZM        ARBNZM
130340941223     C                   CLEAR                   ARBCPI
130350950626     C*
130360950626     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
130370950626     C*     NELLA P.IVA
130380950626     C     VIDISM        IFEQ      '$$'
130390950626     C     VIDCPM        ANDEQ     *BLANKS
130400950626     C                   MOVEL     'PRIVATO'     VIDCPM
130410950626     C                   ENDIF
130420950626     C*
130430950104    2C     VIDISM        IFEQ      *BLANKS
130440941223     C                   MOVEL     VIDCPM        ARBCPI
130450941223     C                   ELSE
130460941223     C                   MOVEL     VIDPIM        ARBCPI
130470950104    2C                   ENDIF
130480950104     C*
130490950104     C                   ELSE
130500950412     C* PRENDO I DATI DA CNACO E CNIND
130510950104     C                   MOVEL     ACORAG        ARBRSM
130520950104     C                   MOVEL     INDVIA        ARBINM
130530950104     C                   MOVEL     INDCIT        ARBLOM
130540950104     C                   MOVEL     INDCAE        ARBCAM
130550950104     C                   MOVEL     INDPRV        ARBPRM
130560950104     C                   MOVEL     INDSTA        ARBNZM
130570950104     C                   MOVEL     INDIVA        ARBCPI
130580941223     C                   ENDIF
130590041027     c* Mi salvo laa new P.iva
130600041027     c                   eval      newcpi=arbcpi
130610041027     c
130620041025     c
130630060223     C* SE ESISTE RECORD P.IVA IN FIAR4  LO AGGIORNO
130640120413     c                   exsr      up_fiar4P
130650941223     C*
130660941223     C* PER SEDE
130670070503     C     D66FFI        IFEQ      'S'
130680950227     C*
130690950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
130700950413    2C     §3ABSD        IFEQ      ' '
130710170116     C****               OPEN      FNARBM0T
130720941227     C                   WRITE     FNARBMTT
130730170116     C***                CLOSE     FNARBM0T
130740950227    2C                   END
130750941223     C* DISALLOCO OGGETTO
130760170116     C***                EXSR      DLCSED
130770950208    1C                   END
130780941223     C*
130790941223     C* AGGIORNO FILE TASSAZIONE PADRONCINI
130800950113     C                   EXSR      AGGPMT
130810950104     C*  REIMPOSTO IL CODICE CLIENTE MITTENTE
130820950104     C                   MOVEL     W0070         ARBCCM
130830950104     C*
130840950113     C* CONTROLLO SE SI TRATTA DI VOLUME AUTOMATICO
130850950113     C* SE HA VARIATO IL MITTENTE
130860950113     C     ARBKSC        IFNE      OLDKSC
130870950113     C*
130880950113     C                   MOVEL     '7T'          COD
130890051214     C                   MOVEL(P)  ARBFVF        KEY
130900950113     C                   EXSR      CTABEL
130910950113     C                   MOVEL     TBLUNI        DS7T
130920950113     C     §7TRVL        IFEQ      'A'
130930950113     C     §7TRVL        OREQ      'P'
130940950114     C                   MOVEL     'S'           WRICVL            1
130950950113     C                   ENDIF
130960950113     C                   ENDIF
130970950113     C*
130980950114     C* SALVO I CAMPI COLLEGATI AL MITTENTE CHE USERO PER STORICIZZARE
130990950114     C                   MOVEL     ARBCTR        MITCTR
131000950114     C                   MOVEL     ARBFVF        MITFVF
131010950114     C                   MOVEL     ARBVLF        MITVLF
131020140403     c*
131030140403     c* aggiorno fiar5 record gen per pickup2
131040140407     c                   if        comgf2<>'01' and arbnzm=*blanks and
131050140407     c                             arbfdn=*blanks
131060140407     c                   eval      wpk2='S'
131070140407     c                   else
131080140407     c                   clear                   wpk2
131090140407     c                   endif
131100140407     c                   if        (wpk2='S' and par5pk2<>'S') or
131110140407     c                              (wpk2<>'S' and par5pk2='S')
131120140403     c                   eval      Wtipoagg='PIK'
131130140403     c                   exsr      regisar5gen
131140140403     c                   endif
131150950114     C*
131160950113     C* SE PREMUTO F10 DEVO FARE IL GIRO CON IL CODICE TARIFFA
131170950104     C     *INKJ         IFEQ      *ON
131180070907     C***                MOVEL     'TP'          D48CVB
131190070907     C***                MOVEL     'E'           WESEG
131200070907     c                   add       1             kk
131210070907     c                   movel     'TP'          kcvb(kk)
131220070911     c                   movel     d48ffr        kffr(kk)
131230070911     c                   movel     w48f12        kf12(kk)
131240070907     c
131250950113     C                   ELSE
131260950114     C*
131270950114     C* SE HO RIPROPOSTO IL CODICE TARIFFA DEL CLIENTE DEVO PASSARE
131280950114     C*  DALLA VIDEATA DELLA TASSAZIONE
131290950114     C                   MOVEL     ARBCTR        W003A             3
131300950114     C     W003A         IFNE      VIDCTR
131310070907     C***                MOVEL     'TP'          D48CVB
131320070907     C***                MOVEL     'E'           WESEG
131330070911     C****               CLEAR                   D48FFR
131340070907     c                   add       1             kk
131350070907     c                   movel     'TP'          kcvb(kk)
131360070911     c                   movel     ' '           kffr(kk)
131370070911     c                   movel     ' '           kf12(kk)
131380950114     C                   ELSE
131390950114     C*
131400950113     C* RICALCOLO VOLUME AUTOMATICO E POI VADO IN VIDEATA DEL VOLUME
131410950114     C     WRICVL        IFEQ      'S'
131420950113     C                   EXSR      CALVOL
131430950114     C                   MOVEL     D18FVF        ARBFVF
131440950114     C                   MOVEL     D18VLF        ARBVLF
131450070911     C***                MOVEL     'I2'          D48CVB
131460070911     C***                MOVEL     'E'           WESEG
131470070911     c***                clear                   w48f12
131480070907     c                   add       1             kk
131490070907     c                   movel     'I2'          kcvb(kk)
131500070911     c                   movel     d48ffr        Kffr(KK)
131510070911     c                   movel     ' '           Kf12(kk)
131520950104     C                   ENDIF
131530950113     C                   ENDIF
131540950114     C                   ENDIF
131550950114     C* MEMORIZZO IL COD TARIFFA SULLA BOLLA, TANTO SE E' CAMBIATO
131560950114     C*  PASSO PER FORZA DALLA VIDEATA DELLA TASSAZIONE PER POTERLO
131570950114     C*  TRASMETTERE
131580950114     C                   MOVEL     VIDCTR        ARBCTR
131590000623     C**
131600120416     c* Controllo e Gestione eventuale record W in fiar4
131610120416     c                   exsr      up_fiar4W
131620041027     c* In partenza memorizzo se 1°bolla franco anche se doppia
131630041027     c* In arrivo   memorizzo se 1°bolla franco e non    doppia
131640041027     c                   if        (*in10 and not *in05) or
131650041027     c                             (not *in10 and not *in06)
131660041027     c                   movel     savcpi        arbcpi
131670041027     c                   endif
131680950114     C*
131690950114     C   10              UPDATE    FNARB000
131700950114     C  N10              UPDATE    FNBLP000
131710041025     c*
131720041025     c* Aggiorno anche l'altra bolla
131730041025     c                   if        waltrabolla=' '
131740041025     c* Se  devo riaggiornare il volume benen altrimenti tengo quello d
131750041025     c*  della bolla
131760041025     c                   if        wricvl<>'S'
131770041025     c                   eval      arbfvf=waltrofvf
131780041027     c                   eval      arbvlf=waltrovlf
131790041025     c                   endif
131800041027     c
131810041027     c* P.IVA  idem come sopra
131820041027     c                   if        (*in10 and *in06) or
131830041027     c                             (not *in10 and *in05)
131840041027     c                   movel     newcpi        arbcpi
131850041027     c                   else
131860041027     c                   movel     waltrocpi     arbcpi
131870041027     c                   endif
131880041025     c
131890041025     C   10              except    arbMblp
131900041025     C  N10              except    arbMarb
131910041025     c                   endif
131920950114     C*
131930950114     C* DISALLOCO I RECORD CHE NON USO
131940991007     C   10              UNLOCK    FIAR601L
131950130411     C*
131960130411     c* Richiamo routine per invio variazione a pda
131970130411     c                   exsr      sr_pda
131980950602     C*
131990941227     C     ENDRG8        ENDSR
132000950113     C*
132010961219     C*--- CONTROLLI RIFERIMENTO MITTENTE ----------------------------*
132020961219     C     CONTR9        BEGSR
132030961219     C                   SETOFF                                       9028
132040061024     C*
132050061024     C* NATURA MERCE SEMPRE OBBLIGATORIA
132060061024     C     VIDNAS        IFEQ      *BLANKS
132070061024     C                   SETON                                        419028
132080061024     C                   MOVEL     MSG(75)       VIDMSG
132090061024     C                   END
132100961219     C* SE DESTINATARIO ESTERO IL RIFERIMENTO E' OBBLIGATORIO
132110061024     c* Solo se c'è attraversamento dogana (come fa FNLS01R)
132120961219     C     WDESIE        IFEQ      'E'
132130970110     C     VIDRMA        ANDEQ     *BLANKS
132140061024     C     deseft        ANDne     *BLANKS
132150961219     C                   SETON                                        409028
132160961219     C                   MOVEL     MSG(74)       VIDMSG
132170961219     C                   END
132180961219     C*
132190961219     C                   ENDSR
132200060316     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE RM -----------------*
132210060316     C     REGIS9        BEGSR
132220130412     c                   clear                   wpda_ndc
132230130412     c                   clear                   wpda_ddc
132240130412     c                   clear                   wpda_dcm
132250060316     C*  GIRO UDATE
132260060316     C                   TIME                    W0140            14 0
132270060316     C* UDATE IN GGMMAAAA
132280060316     C                   MOVE      W0140         WDTGIO            8 0
132290060316     C*
132300060316     C* UDATE IN AAAAMMGG
132310060316     C                   Z-ADD     WDTGIO        G02DAT
132320060316     C                   MOVEL     *BLANK        G02ERR
132330060316     C                   CALL      'XSRDA8'
132340060316     C                   PARM                    WLBDAT
132350060316     C                   MOVEL     G02INV        DATEU             8 0
132360060316     C*
132370060316     C                   MOVEL     KNMUS         ARBPRU
132380060316     C* INVIO PER SEDE
132390170116     C***                MOVEL     'FNARBD0T'    SFILE
132400060316     C* ALLOCO
132410170116     C***                EXSR      ALLOC
132420060316     C* ERRORI IN ALLOCAZIONE
132430170116    1C***  *IN91         IFEQ      *ON
132440170116     C***                GOTO      ENDRG9
132450170116    1C***                ENDIF
132460060316     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
132470060316     c* iornare direttamente il file
132480060316     c                   clear                   waltrabolla       1
132490060316     C   10KARB          CHAIN     FNBLP01L                           3091
132500060316     C  n10KARB          CHAIN     FNARB01L                           3091
132510060316     c*
132520060316     c                   if        *in91
132530060316     C                   MOVEL     MSG(4)        VIDMSG
132540170116     c****               exsr      DLCSED
132550070525     C                   SETON                                        2891
132560060316     C                   GOTO      ENDRG9
132570060316     c                   else
132580060316     c                   if        *in30
132590060316     c                   eval      waltrabolla='N'
132600060316     c                   else
132610130411     c  n10              eval      wpda_ndc=arbndc
132620130411     c  n10              eval      wpda_ddc=arbddc
132630130411     c  n10              eval      wpda_dcm=arbdcm
132640060316     C   10KARB          CHAIN     FNARB01L                           30
132650060316     C  N10KARB          CHAIN     FNBLP01L                           30
132660060316     c* Alla fine per non sporcarmi i campi del record, richaino il
132670060316     c*  record originale che sto variando
132680130411     c   10              eval      wpda_ndc=arbndc
132690130411     c   10              eval      wpda_ddc=arbddc
132700130411     c   10              eval      wpda_dcm=arbdcm
132710060316     c                   endif
132720060316     c                   endif
132730060316     C                   Z-ADD     DATEU         ARBDTV
132740060320     C                   TIME                    ARBORV
132750060410     C                   MOVEL     VIDCVR        ARBCVB
132760060316     c* STORICIZZAZIONE
132770060316    1C     D66FSA        ifeq      'S'
132780060316      *
132790060316      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
132800060316      * è in partenza 'P' o in arrivo 'A'
132810060316     C     *IN10         IFEQ      *ON
132820060316     C                   MOVE      'A'           ARBPRU
132830060316     C                   ENDIF
132840060316     C     *IN10         IFEQ      *OFF
132850060316     C                   MOVE      'P'           ARBPRU
132860060316     C                   ENDIF
132870060316      *
132880060316     c* nella ragione sociale devo mettere rma e nas ma prima devo
132890060316     c* salvare i campi della ragione sociale
132900060316     c                   movel     arbrsd        s_arbrsd
132910060316     c                   movel     arbrd2        s_arbrd2
132920060316     c                   movel(P)  arbrma        arbrsd
132930060316     c                   movel(P)  arbnas        arbrd2
132940060316     c
132950060316     C                   WRITE     FNARBd00
132960060316     c                   movel     s_arbrsd      arbrsd
132970060316     c                   movel     s_arbrd2      arbrd2
132980060316     C                   END
132990060316     c* storicizzazione motivazione
133000060316     c     d66mtv        ifeq      'S'
133010060316     c                   exsr      sto_mtv
133020060316     c                   endif
133030060316     C* Ora invio solo a sede
133040060316    1C     D66FFI        IFEQ      'S'
133050060316     C*
133060060316     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
133070060316    2C     §3ABSD        IFEQ      ' '
133080170116     C***                OPEN      FNARBD0T
133090060316     c                   movel     arbrsd        s_arbrsd
133100060316     c                   movel     arbrd2        s_arbrd2
133110060316     c                   movel(p)  vidrma        arbrsd
133120060316     c                   movel(p)  vidnas        arbrd2
133130060316     C                   WRITE     FNARBDTT
133140060316     c                   movel     s_arbrsd      arbrsd
133150060316     c                   movel     s_arbrd2      s_arbrd2
133160170116     C****               CLOSE     FNARBD0T
133170060316    2C                   END
133180060316     C* DISALLOCO OGGETTO
133190170116     C***                EXSR      DLCSED
133200060316    1C                   END
133210060316     C*
133220060316     C* F6 - AGGIORNAMENTO
133230060316     C                   MOVE      VIDRMA        ARBRMA
133240060316     C                   MOVE      VIDNAS        ARBNAS
133250060316     C   10              UPDATE    FNARB000
133260060316     C  N10              UPDATE    FNBLP000
133270060316     c
133280060316     c                   if        waltrabolla=' '
133290060316     C                   MOVE      VIDRMA        ARBRMA
133300060316     C                   MOVE      VIDNAS        ARBNAS
133310060316     C   10              except    RMblp
133320060316     C  N10              except    RMarb
133330060316     c                   endif
133340130411     C*
133350130411     c* Richiamo routine per invio variazione a pda
133360130411     c                   exsr      sr_pda
133370060316     c     endrg9        endsr
133380030123      *
133390030123      *--- Controllo campi mancato ritiro BANCALI --------------------*
133400030123     c     Contr10       BegSr
133410030123     c                   Eval      *In90 = *Off
133420030127
133430030127      * Il numero dei bancali non ritirati è obbligatorio
133440030127     c                   If        VidRb1 = *Zeros and VidRb2 = *Zeros
133450030127     c                   Eval      VidMsg = Msg(125)
133460030127     c                   Eval      *In60 = *On
133470030127     c                   Eval      *In28 = *On
133480030127     c                   Eval      *In90 = *On
133490030127     c                   GoTo      EndCt10
133500030127     c                   EndIf
133510030124      * Controllo il n. dei bancali non ritirati
133520030124     c                   If        VidRb1 > VidNba
133530030124     c                   Eval      VidMsg = Msg(124)
133540030124     c                   Eval      *In60 = *On
133550030124     c                   Eval      *In28 = *On
133560030124     c                   Eval      *In90 = *On
133570030124     c                   GoTo      EndCt10
133580030124     c                   EndIf
133590030124     c                   If        VidRb2 > VidNb2
133600030124     c                   Eval      VidMsg = Msg(124)
133610030124     c                   Eval      *In61 = *On
133620030124     c                   Eval      *In28 = *On
133630030124     c                   Eval      *In90 = *On
133640030124     c                   GoTo      EndCt10
133650030124     c                   EndIf
133660030123      * Nominativo
133670030123     c                   If        VidNomb = *Blanks
133680030124     c                   Eval      VidMsg = Msg(118)
133690030124     c                   Eval      *In57 = *On
133700030123     c                   Eval      *In28 = *On
133710030123     c                   Eval      *In90 = *On
133720030123     c                   GoTo      EndCt10
133730030123     c                   EndIf
133740030123      * Motivo
133750030123     c                   If        VidMotb = *Blanks
133760030124     c                   Eval      VidMsg = Msg(120)
133770030124     c                   Eval      *In59 = *On
133780030123     c                   Eval      *In28 = *On
133790030123     c                   Eval      *In90 = *On
133800030123     c                   GoTo      EndCt10
133810030123     c                   EndIf
133820030123
133830030123     c     EndCt10       EndSr
133840030123      **
133850030123      *--- AGGIORNAMENTO FILE FIAR5 ----------------------------------*
133860030123     c     Regis10       BegSr
133870130412
133880130412     c                   clear                   wpda_ndc
133890130412     c                   clear                   wpda_ddc
133900130412     c                   clear                   wpda_dcm
133910030123
133920030124     c                   Eval      §Ar5Prub = Knmus
133930060131     c                   Eval      §Ar5Poub = dutPou
133940030124     c                   Move      Dateu         §Ar5Dav
133950030124     c                   Movel     W0140         §Ar5Orv
133960030124     c                   Eval      §Ar5Rb1 = VidRb1
133970030124     c                   Eval      §Ar5Rb2 = VidRb2
133980030124     c                   Eval      §Ar5Nomb = VidNomb
133990030124     c                   Eval      §Ar5Motb = VidMotb
134000030124     c                   Movel(p)  DAr5Ban       Ar5Uni
134010030124     c                   Clear                   Ar5Ft1
134020030124     c                   Clear                   Ar5Ft2
134030030124     c                   Clear                   Ar5Ft3
134040030124     c                   Update    Fiar5000
134050041020     c*
134060041020     c* aggiornamento file fiqdt00f per gestione rientri
134070041020     c                   z-add     arbifp        kfgs
134080041020     c                   z-add     arbndc        w0060             6 0
134090041020     c     kqdt          chain     fiqdt01l
134100041020     c                   if        %found(fiqdt01l)
134110041020     c                   add       vidrb1        qdtnbnbr
134120041020     c                   add       vidrb2        qdtnbnbr
134130041020     c                   except    aggqdt
134140041020     c                   endif
134150060302     c* storicizzazione motivazione
134160060302     c     d66mtv        ifeq      'S'
134170060320     c                   Move      Dateu         arbdtv
134180060320     c                   movel     w0140         arborv
134190060302     c                   exsr      sto_mtv
134200060302     c                   endif
134210130411     C*
134220130412     c                   clear                   waltrabolla       1
134230130412     c                   if        *in10
134240130412     c                   eval      wpda_ndc=arbndc
134250130412     c                   eval      wpda_ddc=arbddc
134260130412     c                   eval      wpda_dcm=arbdcm
134270130412     c                   unlock    fnarb01l
134280130412     c                   else
134290130412     c                   unlock    fnblp01l
134300131120     c                   if        d66pda<>' '
134310130412     C     KARB          CHAIN(n)  FNARB01L                           30
134320130412     c                   if        *in30
134330130412     c                   eval      waltrabolla='N'
134340130412     c                   else
134350130412     c                   eval      wpda_ndc=arbndc
134360130412     c                   eval      wpda_ddc=arbddc
134370130412     c                   eval      wpda_dcm=arbdcm
134380130412     c                   endif
134390130412     c                   endif
134400130412     c                   endif
134410130412     c*
134420130412     c* Richiamo routine per invio variazione a pda
134430130411     c                   exsr      sr_pda
134440030123
134450030123     c                   EndSr
134460061107     C
134470061107     C*--- CONTROLLI FORMATO 11 --------------------------------------*
134480071025     C     CONTR11       BEGSR
134490061107     C                   SETOFF                                       9028
134500151105     c                   clear                   xcfiva1ds
134510151105     c                   clear                   flgiva            1 0
134520151105     c                   move      v11cod        w0040
134530080505     c
134540061107     c* se non previsto inserimento, ERRORE
134550071025    1c                   if        *in76
134560061107     C                   SETON                                        289040
134570071025    2C                   IF        v11MIDE='M'
134580061107     C                   MOVEL     MSG(92)       VIDMSG
134590061107     C                   ELSE
134600061107     C                   MOVEL     MSG(95)       VIDMSG
134610071025    2c                   endif
134620061107     C                   GOTO      ENDCT11
134630071025    1c                   endif
134640061107     c* flag unico se cliente italia
134650071025    1c                   if        v11mide='M'
134660061107     c                   eval      wcliie=wmitie
134670061107     c                   eval      wprv=arbprm
134680151105     c                   eval      wcap=arbcam
134690151105     c                   eval      wloc=arblom
134700061107     c                   else
134710061107     c                   eval      wcliie=wdesie
134720061107     c                   eval      wprv=arbprd
134730151105     c                   eval      wcap=arbcad
134740151105     c                   eval      wloc=arblod
134750071025    1c                   endif
134760071025     c
134770080505     c* Controllo la Partita IVA
134780071025     C* Se P.IVA sprotetta e nazione estera con flag efta la P.IVA è
134790071025     C* obbligatoria
134800071025     c
134810071025    1c                   if        v11pid=*blanks
134820071025     c
134830071025    2c                   if        wdesie='E' and deseft<>*blanks
134840071025     C                   SETON                                        284190
134850071025     C                   MOVEL     MSG(67)       VIDMSG
134860071025     C                   GOTO      ENDCT11
134870120507     c****               else
134880071031     c* solo se non richiamato nascosto
134890120507    3c*****              if        d48ffr='V' or d48ffr='S'
134900120507    4c****               if        wcliie='I' and   forzapid=' '
134910120507     c*****                        and dateu>=§vpopiv
134920120507     C******             SETON                                        289041
134930120507     C*****              MOVEL     MSG(57)       VIDMSG
134940120507     c******             movel     '1'           forzapid
134950120507     C********           GOTO      ENDCT11
134960120507    4c*********          endif
134970120507    3c********           endif
134980071031     c
134990071025    2c                   endif
135000071025    1C                   ENDif
135010071025     C*
135020151105    1c***                if        v11pid<>*blanks
135030071025     c* Se cambiata pulisco la forzatura
135040071025    2c                   if        v11pid<>sav11pid
135050071025     c                   clear                   forzapid2
135060071025     c                   movel     v11pid        sav11pid
135070071025    2c                   endif
135080071025     c
135090151105     c                   clear                   xcfiva1ds
135100071025     c                   clear                   xcfivamod
135110151105     c                   eval      xcfivafori= 'S'
135120100120     c                   eval      xcfivamod = 'P'
135130071025     c                   eval      xcfivapar = v11pid
135140071025     c                   eval      xcfivanar = wnar
135150071025     c                   eval      xcfivaprv = wprv
135160151105     c                   eval      xcfivacap = wcap
135170151105     c                   eval      xcfivaloc = wloc
135180151105     c                   if        w0040=8888 or w0040=9999
135190151105     c                   eval      xcfivap88='8'
135200151105     c                   endif
135210151105     c                   call      'XCFIVAR1'
135220151105     c                   parm                    xcfiva1ds
135230071025     C*
135240071025      * --> errata forzabile
135250100120    2c                   If        xcfivaflg = -9 and forzaPID2 = ' '
135260071025     c                   seton                                        289041
135270071025     c                   Eval      vidmsg = xcfivamsg
135280071025     c                   Eval      vidmsg = %trim(vidmsg) + ' Enter x forzare'
135290100120     c                   eval      forzaPID2= '1'
135300071025     C                   GOTO      ENDCT11
135310071025    2c                   EndIf
135320071025      * --> errore
135330071025    2c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
135340071025     c                   seton                                        289041
135350071025     c                   eval      vidmsg = xcfivamsg
135360071025     C                   GOTO      ENDCT11
135370071025    2c                   EndIf
135380151105
135390151105     c                   eval      flgiva=xcfivaflg
135400071025     c
135410071025      * se non impostato devo riportare il codice iso della partita iva che
135420071025      * mi viene passato dal pgm di controllo
135430151105    2c                   if        v11pid<>*blanks and v11isd <> ivaeu
135440071025     c                   eval      v11isd = ivaeu
135450071025     c                   seton                                          9041
135460071025     C                   GOTO      ENDCT11
135470071025    2c                   endif
135480151105    1c****               endif
135490080505     c
135500080505      * Devo richiamare il nuovo driver fatto per controllare il codice
135510080505      * fiscale
135520151105     c                   clear                   xcfiva1ds
135530080505     c                   clear                   xcfivamod
135540080505     c                   eval      xcfivapar = v11cdfis
135550080505     c                   eval      xcfivanar = wnar
135560080505     c                   eval      xcfivaprv = wprv
135570151105     c                   eval      xcfivacap = wcap
135580151105     c                   eval      xcfivaloc = wloc
135590151105     c                   eval      xcfivapiva= v11pid
135600151105     c                   if        w0040=8888 or w0040=9999
135610151105     c                   eval      xcfivap88='8'
135620151105     c                   endif
135630151105     c                   call      'XCFIVAR1'
135640151105     c                   parm                    xcfiva1ds
135650080505     c*
135660080505    1c                   if        xcfivaflg < *zeros
135670080505     C                   SETON                                        289040
135680080505     C                   MOVEL     xcfivamsg     VIDMSG
135690080505     C                   GOTO      ENDCT11
135700080505    1c                   endif
135710080505     c
135720080505     c* Obbligatorio se cliente italia partita iva o codice fiscale
135730080505     c* solo se non richiamato nascosto
135740120507    1c***                if        d48ffr='V' or d48ffr='S'
135750120507    2c****               if        wcliie='I' and   v11cdfis=*blanks
135760120507     c****                         and forzacdf=' ' and xcfivaflg<>9
135770120507     C****               SETON                                        289040
135780120507     C*****              MOVEL     MSG(23)       VIDMSG
135790120507     c****               movel     '1'           forzacdf
135800120507     C*****              GOTO      ENDCT11
135810120507    2c*****              endif
135820120507    1c******             endif
135830080505     c
135840080505     c* Se entrambe vuote errore
135850080505    1c                   if        v11cdfis=*blanks and v11pid=*blanks
135860080505     C                   SETON                                        284090
135870080505     C                   MOVEL     MSG(145)      VIDMSG
135880080505     C                   GOTO      ENDCT11
135890080505    1c                   endif
135900120508     c* Se fattura immediata errore se partita iva '$$'
135910120508     c                   if        %scan('$$':v11pid)>0  and wcliie='I'
135920120508     c                   move      v11cod        w0040
135930120508     c                   if        w0040=8888 or w0040=9999
135940120507     C                   SETON                                        284190
135950120507     C                   MOVEL     MSG(149)      VIDMSG
135960120507     C                   GOTO      ENDCT11
135970120508     c                   endif
135980120507     c                   endif
135990120508     c* solo se non richiamato nascosto
136000130315    3c                   if        d48ffr='V' or d48ffr='S'
136010120507     c* Se codice fiscale lungo 11 si richiede la partita IVA  - Errore forzabile
136020120507     c                   if        v11cdfis<>*blanks and v11pid=*blanks and
136030120507     c                             %subst(v11cdfis:12:1)=' ' and wcliie='I'
136040120507     c                             and forzapid=' '
136050120507     C                   SETON                                        289041
136060120507     C                   MOVEL     MSG(57)       VIDMSG
136070120507     c                   movel     '1'           forzapid
136080120507     C                   GOTO      ENDCT11
136090120507     c                   endif
136100130315     c                   endif
136110061107     C*
136120061107     C     ENDCT11       ENDSR
136130130927     c
136140130927     C*--- CONTROLLI FORMATO 12 --------------------------------------*
136150130927     C     CONTR12       BEGSR
136160130927     C                   SETOFF                                       9028
136170130930     c* controllo indirizzo email
136180131001     c                   if        *in86  and videml<>*blanks
136190130930     c                   Clear                   dsemail
136200130930     c                   Eval      §emlindi = videml
136210130930     c                   Call      'XEMAIL'
136220130930     c                   Parm                    dsemail
136230130930if  3c                   If        §emlerro <> '0'
136240130930     C                   SETON                                        289041
136250130930     C                   MOVEL     MSG(150)      VIDMSG
136260130930     C                   GOTO      ENDCT12
136270130930     c                   endif
136280130930
136290130930     c                   endif
136300130930     c
136310130927     C     ENDCT12       ENDSR
136320061107     C*
136330061107     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE FI -----------------*
136340061107     C     REGIS11       BEGSR
136350130412
136360130412     c                   clear                   wpda_ndc
136370130412     c                   clear                   wpda_ddc
136380130412     c                   clear                   wpda_dcm
136390061107     C*  GIRO UDATE
136400061107     C                   TIME                    W0140            14 0
136410061107     C* UDATE IN GGMMAAAA
136420061107     C                   MOVE      W0140         WDTGIO            8 0
136430061107     C*
136440061107     C* UDATE IN AAAAMMGG
136450061107     C                   Z-ADD     WDTGIO        G02DAT
136460061107     C                   MOVEL     *BLANK        G02ERR
136470061107     C                   CALL      'XSRDA8'
136480061107     C                   PARM                    WLBDAT
136490061107     C                   MOVEL     G02INV        DATEU             8 0
136500061107     C                   MOVEL     KNMUS         ARBPRU
136510071025     c*
136520071025     C     V11ISD        IFEQ      '$$'
136530071025     C     V11CPD        ANDEQ     *BLANKS
136540071025     C                   MOVEL     'PRIVATO'     V11CPD
136550071025     C                   ENDIF
136560071025     c
136570071025     c* Salvo a parte la partita iva presente in bolla
136580071025     c                   movel     arbcpi        bollacpi
136590061107     C*
136600061129     c* Se la bolla non è trasmessa non devo allocare il file e non devo
136610061129     c*    creare la variazione
136620170116     c***                if        bollaft2<>' '
136630061107     C* INVIO PER SEDE
136640170116     C***                MOVEL     'FNARBD0T'    SFILE
136650061107     C* ALLOCO
136660170116     C***                EXSR      ALLOC
136670061107     C* ERRORI IN ALLOCAZIONE
136680170116    1C**   *IN91         IFEQ      *ON
136690170116     C***                GOTO      ENDRG11
136700170116    1C***                ENDIF
136710170116    1C***                ENDIF
136720061107     c
136730061107     c* Alloco il record fiar4 del codice fiscale                      agg
136740061107     C                   MOVEL     'Q'           WTRC
136750061107     C     KARB2         CHAIN     FiAR4000                           3091
136760061107     c*
136770061107     c                   if        *in91
136780061107     C                   MOVEL     MSG(4)        VIDMSG
136790170116     c****               exsr      DLCSED
136800070525     C                   SETON                                        2891
136810061107     C                   GOTO      ENDRG11
136820061107     c                   endif
136830061107     c
136840061107     c                   if        *in30
136850061107     c                   eval      esiste='N'
136860061116     c                   clear                   ar4not
136870061107     c                   else
136880061107     c                   eval      esiste='S'
136890061107     c                   endif
136900071026     c*
136910071026     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
136920071026     c* iornare direttamente il file solo se non si tratta di bolla doppia
136930071026     c                   clear                   waltrabolla       1
136940130412     c                   setoff                                       3091
136950130412    1c                   if        §3atb2=*blanks and aggiobol<>'N'
136960071026     C   10KARB          CHAIN     FNBLP01L                           3091
136970071026     C  n10KARB          CHAIN     FNARB01L                           3091
136980130412     c                   else
136990131120     c                   if        d66pda<>' '
137000130412     c                   if        not *in10
137010130412     C     KARB          CHAIN(n)  FNARB01L                           30
137020130412     c                   endif
137030130412     c                   eval      wpda_ndc=arbndc
137040130412     c                   eval      wpda_ddc=arbddc
137050130412     c                   eval      wpda_dcm=arbdcm
137060130412     c                   endif
137070130412    1c                   endif
137080071026     c*
137090071026    2c                   if        *in91
137100071026     C                   MOVEL     MSG(4)        VIDMSG
137110071026     c*
137120170116     c****               exsr      DLCSED
137130071026     c                   unlock    fiar401l
137140071026     c
137150071026     C                   SETON                                        2891
137160071026     C                   GOTO      ENDRG11
137170071026   x2c                   else
137180071026    3c                   if        *in30
137190071026     c                   eval      waltrabolla='N'
137200071026   x3c                   else
137210130412    4c                   if        §3atb2=*blanks and aggiobol<>'N'
137220130411     c  n10              eval      wpda_ndc=arbndc
137230130411     c  n10              eval      wpda_ddc=arbddc
137240130411     c  n10              eval      wpda_dcm=arbdcm
137250071026     C   10KARB          CHAIN     FNARB01L                           30
137260071026     C  N10KARB          CHAIN     FNBLP01L                           30
137270071026     c* Alla fine per non sporcarmi i campi del record, richaino il
137280071026     c*  record originale che sto variando
137290130411     c   10              eval      wpda_ndc=arbndc
137300130411     c   10              eval      wpda_ddc=arbddc
137310130411     c   10              eval      wpda_dcm=arbdcm
137320130412    4c                   endif
137330071026    3c                   endif
137340071026    2c                   endif
137350130412    1c****               endif
137360061107     C*
137370061107     C                   Z-ADD     DATEU         ARBDTV
137380061107     C                   TIME                    ARBORV
137390061107     C                   MOVEL     VIDCVR        ARBCVB
137400061107     c* Memorizzo se si tratta del mittente o destinatario
137410061107     c                   movel     v11mide       arbrd2
137420061107     c
137430061107     c* clearo gli altri campi del file che non mi interessano
137440061107     c                   clear                   arbrsd
137450061107     c                   clear                   arbind
137460061107     c                   clear                   arbcad
137470061107     c                   clear                   arblod
137480061107     c                   clear                   arbprd
137490061107     c                   clear                   arbnzd
137500061107     c                   clear                   arbfin
137510061107     c                   clear                   arbpkb
137520061107     c                   clear                   arbpkf
137530061107     c                   clear                   arbvlb
137540061107     c                   clear                   arbvlf
137550061107     c                   clear                   arbfvb
137560061107     c                   clear                   arbfvf
137570061107     c                   clear                   arbffd
137580061107     c                   clear                   arbfst
137590061107     C*
137600061107     C* SE DA STORICIZZARE
137610061107    1C     D66FSA        ifeq      'S'
137620061107      *
137630061107      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
137640061107      * è in partenza 'P' o in arrivo 'A'
137650061107     C     *IN10         IFEQ      *ON
137660061107     C                   MOVE      'A'           ARBPRU
137670061107     C                   ENDIF
137680061107     C     *IN10         IFEQ      *OFF
137690061107     C                   MOVE      'P'           ARBPRU
137700061107     C                   ENDIF
137710061107     c                   if        v11mide='M'
137720061107     c                   movel     ar4not        arbcpi
137730061107     c                   else
137740061107     c                   move      ar4not        arbcpi
137750061107     c                   endif
137760071025     c* Partita iva
137770071025     c                   movel     bollacpi      arbrsd
137780061107      *
137790061107     C                   WRITE     FNARBD00
137800061107    1C                   ENDIF
137810061107    c
137820061107     c* storicizzazione motivazione
137830061107     c     d66mtv        ifeq      'S'
137840061107     c                   exsr      sto_mtv
137850061107     c                   endif
137860071025     c* Aggiorno campo codice fiscale
137870061107     c                   movel     v11cdfis      arbcpi
137880071025     c* Aggiorno campo per la partita iva
137890071025     C                   MOVEL     V11PID        ARBrsd
137900071025     C* LA P.IVA SE HA CODICE ISO VIENE MESSO A SIN ALTRIMENTI SUBITO
137910071025     C*  IL CODICE DELLA P.IVA
137920071025     C     V11CPD        IFNE      *BLANKS
137930071025     C     V11ISD        ANDEQ     *BLANKS
137940071025     C                   CLEAR                   ARBrsd
137950071025     C                   MOVEL     V11CPD        ARBrsd
137960071025     C                   ENDIF
137970130402     c* Se Cd.Fisc. lungo 11 lo metto anche nella P.Iva se vuota
137980130402     c                   if        v11pid=*blanks and %subst(v11cdfis:12:1)=' '
137990130402     c                   movel     v11cdfis      arbrsd
138000130402     c                   endif
138010071025     c
138020071025     C                   MOVEL     'P'           WTRC
138030071025     C     KARB2         CHAIN     FIAR401L                           30
138040071025    1C     *IN30         IFEQ      *OFF
138050071025    2c                   if        v11mide='M'
138060071025     c                   movel     ar4not        wcpifile
138070071025   x2c                   else
138080071025     c                   move      ar4not        wcpifile
138090071025    2c                   endif
138100071025    2c                   if        wcpifile <> arbrsd
138110071025     c                   movel     'T'           ar4ftr
138120071025     c                   z-add     dateu         ar4dtr
138130071025     c                   z-add     dateu         ar4duv
138140071025    3c                   if        v11mide='M'
138150071025     c                   eval      %subst(ar4not:1:16)=%subst(arbrsd:1 :16)
138160071025     c                   else
138170071025     c                   eval      %subst(ar4not:20:16)=%subst(arbrsd:1:16)
138180071025    3c                   endif
138190071025     c
138200071025     C                   UPDATE    FIAR4000
138210071025   x2c                   else
138220071025     c                   unlock    fiar401l
138230071025    2c                   endif
138240071025    1c                   endif
138250061129     c
138260071025    0c                   if        bollaft2<>' '
138270061107     C* Ora invio solo a sede
138280061107     C     D66FFI        IFEQ      'S'
138290061107     C*
138300061107     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
138310061107    2C     §3ABSD        IFEQ      ' '
138320170116     C***                OPEN      FNARBD0T
138330061107     C                   WRITE     FNARBDTT
138340170116     C***                CLOSE     FNARBD0T
138350061107    2C                   END
138360061107     C* DISALLOCO OGGETTO
138370170116     C**                 EXSR      DLCSED
138380061107    1C                   END
138390071025    0C                   END
138400061107     C*
138410061107     C* AGGIORNO  file: 30 on write del record
138420071025     C                   MOVEL     'Q'           WTRC
138430071025     C     KARB2         CHAIN     FiAR4000                           3091
138440061107     c                   if        Esiste='N'
138450061107     c                   movel     arbaas        ar4aas
138460061107     c                   movel     arblnp        ar4lnp
138470061107     c                   movel     arbnrs        ar4nrs
138480061107     c                   movel     arbnsp        ar4nsp
138490061107     c                   movel     'Q'           ar4trc
138500061107     c                   clear                   ar4not
138510061107     c                   clear                   ar4flo
138520061107     c                   endif
138530061107     c                   if        v11mide='M'
138540061107     c                   movel     v11cdfis      ar4not
138550061107     c                   else
138560061107     c                   move      v11cdfis      ar4not
138570061107     c                   endif
138580061107     c                   movel     'T'           ar4ftr
138590061107     c                   z-add     dateu         ar4dtr
138600061107     c                   z-add     dateu         ar4duv
138610061107     c
138620061116     c                   if        Esiste='N'  and ar4not<>*blanks
138630061107     c                   write     fiar4000
138640061116     c                   endif
138650061116     c                   if        Esiste='S'  and ar4not<>*blanks
138660061107     c                   update    fiar4000
138670061107     c                   endif
138680061116     c                   if        Esiste='S'  and ar4not=*blanks
138690061116     c                   delete    fiar4000
138700061116     c                   endif
138710071025     c
138720071026     c* Aggiorno la bolla per la partita iva solo se non richiamamto da I0
138730071026     c*  per cui ho già aggiornato la partita iova sulla bolla
138740071026     c                   if        Aggiobol<>'N'
138750071025     c                   movel     arbrsd        arbcpi
138760071025     c* aggiornamento mirato per non sporcare degli altri campi
138770071025     c  n10              except    SolocpiP
138780071025     c   10              except    SolocpiA
138790071026     c
138800071026     c* Se 'c'e' l'altra ed è bolla singola la aggiorno
138810071026     c                   if        §3atb2=*blanks and waltrabolla=' '
138820071026     c  n10              except    SolocpiA
138830071026     c   10              except    SolocpiP
138840071026     c                   endif
138850071026     c                   endif
138860061107     C*
138870061107     C* DISALLOCO  I FILES CHE NON USO
138880061107     C   10              UNLOCK    FiAR901L
138890071025     c** 10              unlock    fnarb01l
138900071025     c**N10              unlock    fnblp01l
138910130411     C*
138920130411     c* Richiamo routine per invio variazione a pda
138930130411     c                   exsr      sr_pda
138940061107     C*
138950061107     C     ENDRG11       ENDSR
138960130927     C*
138970130930     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE Nx -----------------*
138980130927     C     REGIS12       BEGSR
138990130927     c
139000130927     C*  GIRO UDATE
139010130927     C                   TIME                    W0140            14 0
139020130927     C* UDATE IN GGMMAAAA
139030130927     C                   MOVE      W0140         WDTGIO            8 0
139040130927     C*
139050130927     C* UDATE IN AAAAMMGG
139060130927     C                   Z-ADD     WDTGIO        G02DAT
139070130927     C                   MOVEL     *BLANK        G02ERR
139080130927     C                   CALL      'XSRDA8'
139090130927     C                   PARM                    WLBDAT
139100130927     C                   MOVEL     G02INV        DATEU             8 0
139110130927     C                   MOVEL     KNMUS         ARBPRU
139120130927     C                   Z-ADD     DATEU         ARBDTV
139130130927     C                   TIME                    ARBORV
139140130927     C                   MOVEL     VIDCVR        ARBCVB
139150131001      *
139160131001      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
139170131001      * è in partenza 'P' o in arrivo 'A'
139180131001     C     *IN10         IFEQ      *ON
139190131001     C                   MOVE      'A'           ARBPRU
139200131001     C                   ENDIF
139210131001     C     *IN10         IFEQ      *OFF
139220131001     C                   MOVE      'P'           ARBPRU
139230131001     C                   ENDIF
139240130927     C*
139250130927     C* SE DA STORICIZZARE
139260130927    1C     D66FSA        ifeq      'S'
139270130930
139280131001     c                   if        *in84 and
139290131001     c                                  (vidref<>par5ref or vidteld<>par5teld)
139300130930     c* Referente e consegna
139310130927     c                   clear                   darbn_ref
139320130927     c                   eval      §arbnref = par5ref
139330130927     c                   eval      §arbnteld= par5teld
139340130927     c                   eval      arbuni70 = darbn_ref
139350130927     c                   eval      arbtrd='REF'
139360130927     c                   write     FNARBN00
139370131001     c                   endif
139380131001     c
139390131001     c                   if        *in83 and emd_§ar5tel<>vidtsms
139400130930     c                   clear                   darbn_sms
139410130930     c                   eval      §arbncel = emd_§ar5tel
139420130930     c                   eval      arbuni70 = darbn_sms
139430130927     c                   eval      arbtrd='SMS'
139440130927     c                   write     FNARBN00
139450131001     c                   endif
139460131001     c
139470131001     c                   if        *in86 and emd_§ar5eml<>videml
139480130927     c                   clear                   darbn_eml
139490130930     c                   eval      §arbneml=emd_§ar5eml
139500130927     c                   eval      arbuni70 = darbn_eml
139510130927     c                   eval      arbtrd='EML'
139520130927     c                   write     FNARBN00
139530131001     c                   endif
139540131001
139550131001     c                   if        *in81 and (vidnt1<>v1cnt1 or vidnt2<>v1cnt2)
139560130930     c                   clear                   darbn_not
139570130930     c                   eval      §arbnnot1=v1cnt1
139580130930     c                   eval      §arbnnot2=v1cnt2
139590130930     c                   eval      arbuni70 = darbn_not
139600130930     c                   eval      arbtrd='NOT'
139610130930     c                   write     FNARBN00
139620130930     c                   endif
139630130930
139640130930     c                   endif
139650130930     c
139660131001     c                   if        *in84
139670130930     c                   exsr      AggioREF
139680131001     c                   endif
139690131001     c
139700130930      * EMAIL e cell sms
139710131001     c                   if        *in83 or *in86
139720130930     c                   Eval      kAr5Trd = 'EMD'
139730130930     c     kAr5          Chain     Fiar501l
139740130930      * Aggiorno Fiar5
139750130930if  2c                   If        %Found(Fiar501l)
139760130930     c                   movel     ar5uni        DAr5EMD
139770131001     c   86              eval      emd_§ar5eml=videml
139780131001     c   83              eval      emd_§ar5tel=vidtsms
139790130930     c                   Movel(p)  DAr5emd       Ar5Uni
139800130930     c                   Update    Fiar5000
139810130930      * Scrivo Fiar5
139820130930   x2c                   Else
139830130930      * se immessi i dati
139840130930      * non mi preoccupo per il fermo deposito: il fiar5 record GEN viene sempre creato
139850130930      * in immissione bolle. Non esiste più il caso di fiar5 record GEN inesistente in fase
139860130930      * di manutenzione bolle
139870130930if  3c                   If        Videml <> *Blanks or Vidtsms <> *Blanks
139880130930     c                   Clear                   Fiar5000
139890130930     c                   Eval      Ar5Aas = ArbAas
139900130930     c                   Eval      Ar5Lnp = ArbLnp
139910130930     c                   Eval      Ar5Nrs = ArbNrs
139920130930     c                   Eval      Ar5Nsp = ArbNsp
139930130930     c                   Eval      Ar5Trd = 'EMD'
139940130930     c                   Move      Dateu         Ar5Dac
139950130930     c                   Movel     W0140         Ar5Orc
139960130930     c                   Clear                   Dar5emd
139970131001     c   86              eval      emd_§ar5eml=videml
139980131001     c   83              eval      emd_§ar5tel=vidtsms
139990130930     c                   Movel(p)  DAr5emd       Ar5Uni
140000160426     c                   Eval      Ar5Ft1 = 'T'
140010160426     c                   Eval      Ar5Dt1 = *All'9'
140020160426     c                   Eval      Ar5Ft2 = 'T'
140030160426     c                   Eval      Ar5Dt2 = *All'9'
140040130930     c                   Write     Fiar5000
140050130930    3c                   EndIf
140060130930    2c                   EndIf
140070130930     c
140080131001   x1c                   endif
140090130930
140100131001     c                   if        *in81
140110130930     c* aggiorno le NOTE
140120130930     c* aggiorno/deleto trk 8 e 9 se variati
140130130930     c                   clear                   fnlv19ds
140140130930     C                   MOVEL     'T'           D19ftr
140150130930     C                   MOVEL     arbAAS        D19AAS
140160130930     C                   MOVEL     arbLNP        D19LNP
140170130930     C                   MOVEL     arbNRS        D19NRS
140180130930     C                   MOVEL     arbNSP        D19NSP
140190130930     C                   MOVEL     vidnt1        D19NT1
140200130930     C                   MOVEL     vidnt2        D19NT2
140210130930    2C     vidnt1        IFNE      *BLANKS
140220130930     C     vidnt2        ORNE      *BLANKS
140230130930     C                   MOVEL     '8'           D19TRC
140240130930     C                   CALL      'FNLV19R'
140250130930     C                   PARM                    fnlv19ds
140260130930    2C                   ENDIF
140270130930     c* Se il pgm mi restituisce la seconda riga vuota la deleto perchè
140280130930     c* prima poteva esistere
140290130930    2C     D19ERR        IFEQ      *BLANKS
140300130930     C     D19NT1        ANDNE     *BLANKS
140310130930     C     D19NT2        ANDEQ     *BLANKS
140320130930     C                   MOVEL     '9'           D19TRC
140330130930     C                   CLEAR                   D19NT1
140340130930     C                   CLEAR                   D19NT2
140350130930     C                   CALL      'FNLV19R'
140360130930     C                   PARM                    fnlv19ds
140370130930   x2C                   else
140380130930     C* Se tornato codice di err richiamo il pgm 2 volte:  una per nota
140390130930    3C     D19ERR        IFNE      *BLANKS
140400130930     C     vidnt1        oreq      *BLANKS
140410130930     C     vidnt2        ANDEQ     *BLANKS
140420130930     C* Nota 1
140430130930     C                   MOVEL     '8'           D19TRC
140440130930     C                   MOVEL     vidnt1        D19NT1
140450130930     C                   CLEAR                   D19NT2
140460130930     C                   CALL      'FNLV19R'
140470130930     C                   PARM                    fnlv19ds
140480130930     C* Nota 2
140490130930     C                   MOVEL     '9'           D19TRC
140500130930     C                   MOVEL     vidnt2        D19NT1
140510130930     C                   CLEAR                   D19NT2
140520130930     C                   CALL      'FNLV19R'
140530130930     C                   PARM                    fnlv19ds
140540130930    3C                   ENDIF
140550130930    2C                   ENDIF
140560130930
140570130930    1c                   EndIf
140580130927     c*
140590131008     c   10              unlock    fnarb01l
140600131008     c  n10              unlock    fnblp01l
140610130927     C     ENDRG12       ENDSR
140620961219     C*
140630950113     C*--- CALCOLO IL VOLUME AUTOMATICO ------------------------------*
140640950113     C     CALVOL        BEGSR
140650030612     C                   CLEAR                   Fnlv18DS
140660950113     C   10              MOVEL     'A'           D18TBO
140670950113     C  N10              MOVEL     'P'           D18TBO
140680950113     C                   Z-ADD     ARBKSC        D18KSC
140690950113     C                   Z-ADD     ARBCTR        D18CTR
140700950113     C                   MOVEL     §3ATB1        D18TBL
140710950113     C                   Z-ADD     ARBAAS        D18AAS
140720950113     C                   Z-ADD     ARBMGS        D18MGS
140730950113     C                   Z-ADD     ARBNCL        D18NCL
140740950113     C                   Z-ADD     ARBPKF        D18PKF
140750020311     c                   movel     arbtsp        d18tsp
140760020311     c                   z-add     arblna        d18lna
140770020311     c                   z-add     arblnp        d18lnp
140780950113     C                   CALL      'FNLV18R'
140790030612     C                   PARM                    Fnlv18DS
140800950113     C*
140810950113     C                   ENDSR
140820911212     C*
140830970808     C* CONTROLLO INDIRIZZO COMPLETO MITT/DESTIN ---------------------*
140840970808     C     CTRIND        BEGSR
140850150203     c                   clear                   normlod
140860970808     C* LE DS SONO DA CLEARARE PRIMA ALTRIMENTI DANNO PROBLEMI IN CTRER
140870030612     C                   CLEAR                   Fnlv13DS
140880030612     C                   CLEAR                   Tisi95DS
140890970808     C*
140900100120     c                   movel     'S'           i14forzait
140910970808     C                   MOVEL     ARBDSP        I14DRI
140920020305     c                   movel     'S'           i14cnz
140930970808     C                   CALL      'FNLV14R'
140940970808     C                   PARM                    KPJBA
140950030612     C                   PARM                    Fnlv14DS
140960970808     C                   MOVEL     O14ERR        WERR              1
140970970808     C                   MOVEL     O14MSG        VIDMSG
140980970808     C                   EXSR      CTRERR
140990970808     C*
141000970808     C     *IN90         IFEQ      *ON
141010970808     C                   GOTO      ENDIND
141020970808     C                   ENDIF
141030970808     C*************
141040970808     C* CONTROLLO CAP
141050970808     C                   MOVEL     '7'           I95TCN
141060970808     C                   MOVEL     E14CAP        I95CAP
141070970808     C                   MOVEL     E14LOC        I95LOC
141080970808     C                   MOVEL     E14PRV        I95PRV
141090970808     C                   MOVEL     E14NAR        I95NAR
141100000623     C                   MOVEL     ARBDSP        I95DAT
141110020318     C     LNPNTW        IFEQ      'PPT'
141120000623     C                   MOVEL     ARBLNP        I95TFP
141130000623     C                   ELSE
141140980602     C                   MOVEL     ARBTFP        I95TFP
141150000623     C                   ENDIF
141160971230     C                   Z-ADD     ARBPKF        I95LKG
141170971230     C                   Z-ADD     ARBVLF        I95LMC
141180971230     C                   MOVEL     ARBFFD        I95FFD
141190971201     C                   MOVEL     FLGTB1        I95TPO
141200971230     C                   MOVEL     ARBTSP        I95TSP
141210980602     C                   MOVEL     'S'           I95FRE
141220971201     C     §3AFCA        IFNE      0
141230971201     C                   MOVEL     'S'           I95FCA
141240971201     C                   ENDIF
141250970808     C                   MOVEL     'S'           I13AF0
141260140902     c                   if        d48ffr<>'E'
141270970808     C                   MOVEL     'S'           I13CNV
141280140807     c                   endif
141290970808     C                   MOVEL     'S'           I13SZV
141300141119     c* in caso di chiamata cieca non è da impostare: non si deve aprire la int località
141310141119     c                   if        d48ffr<>'E'
141320970808     C                   MOVEL     'S'           I13AF1
141330141119     c                   else
141340141119     C                   MOVEL     ' '           I13AF1
141350141119     c                   endif
141360970808     C                   MOVEL     'S'           I13SZ2
141370970808     C*
141380970808     C* SE CAMBIATO CAP PULISCO CAMPI PER FORZATURE
141390970808     C* ** DESTINATARIO **
141400970808     C     WCLI          IFEQ      'D'
141410970808    2C     E14CAP        IFNE      VARCAD
141420970808     C                   CLEAR                   WZV
141430970808     C                   CLEAR                   WZ1
141440970808     C                   CLEAR                   WZ2
141450970808     C                   MOVEL     E14CAP        VARCAD
141460970808    2C                   ENDIF
141470970808     C* MODIFICA LA LOCALITA'
141480970808    2C     E14LOC        IFNE      VARLOD
141490970808     C                   MOVEL     E14LOC        VARLOD
141500970808     C                   CLEAR                   WZ1
141510970808     C                   CLEAR                   WZ2
141520970808    2C                   ENDIF
141530970808     C                   MOVEL     WZV           E13FZV
141540970808     C                   MOVEL     WZ1           E13FZ1
141550970808     C                   MOVEL     WZ2           E13FZ2
141560020305     c     lnantw        ifeq      'FED'
141570020305     c                   movel     'S'           i95fi1
141580020305     c                   endif
141590970808     C                   ELSE
141600970808     C* ** MITTENTE **
141610970808    2C     E14CAP        IFNE      VARCAM
141620970808     C                   CLEAR                   WZVM
141630970808     C                   CLEAR                   WZ1M
141640970808     C                   CLEAR                   WZ2M
141650970808     C                   MOVEL     E14CAP        VARCAM
141660970808    2C                   ENDIF
141670970808     C* MODIFICA LA LOCALITA'
141680970808    2C     E14LOC        IFNE      VARLOM
141690970808     C                   MOVEL     E14LOC        VARLOM
141700970808     C                   CLEAR                   WZ1M
141710970808     C                   CLEAR                   WZ2M
141720970808    2C                   ENDIF
141730020305     c     lnpntw        ifeq      'FED'
141740020305     c                   movel     'S'           i95fi1
141750020305     c                   endif
141760970808     C                   MOVEL     WZVM          E13FZV
141770970808     C                   MOVEL     WZ1M          E13FZ1
141780970808     C                   MOVEL     WZ2M          E13FZ2
141790970808     C                   END
141800970808     C*
141810970808     C**
141820970808     C** CALL
141830970808     C                   CALL      'FNLV13R'
141840970808     C                   PARM                    KPJBA
141850030612     C                   PARM                    Fnlv13DS
141860030612     C                   PARM                    Tisi95DS
141870970808     C*
141880970808     C                   MOVEL     O13ERR        WERR              1
141890970808     C                   MOVEL     O13MSG        VIDMSG
141900970808     C                   EXSR      CTRERR
141910970808     C*
141920970808     C* REIMPOSTO LE FORZATURE
141930970808     C     WCLI          IFEQ      'D'
141940970808     C                   MOVEL     E13FZV        WZV
141950970808     C                   MOVEL     E13FZ1        WZ1
141960970808     C                   MOVEL     E13FZ2        WZ2
141970970808     C                   ELSE
141980970808     C                   MOVEL     E13FZV        WZVM
141990970808     C                   MOVEL     E13FZ1        WZ1M
142000970808     C                   MOVEL     E13FZ2        WZ2M
142010970808     C                   END
142020150203     c* memorizzo la località normalizzata per destinatario
142030150203     c                   if        wcli='D'
142040150203     c                   eval      normlod=o95loc
142050150203     c                   endif
142060970808     C     ENDIND        ENDSR
142070070320     C*
142080070320     C*--- Gestisce record"L"di FIAR4 --------------------------------*
142090070320     C     GestAR4L      BEGSR
142100070320     c* lo faccio solo se GEO attiva
142110070320    0c                   if        FGSGEOT='S'
142120070320     c* Se devo chainare
142130070320     C                   MOVEL     'L'           WTRC
142140070320     c
142150070321    1c                   if        TipoEla='C'
142160070320     c                   clear                   Wesar4L
142170070320     C     KARB2         CHAIN     FiAR4000                           3091
142180070320    2c                   if        *in91
142190070320     C                   SETON                                        28
142200070320     C                   MOVEL     MSG(4)        VIDMSG
142210070320     C                   GOTO      ENDar4L
142220070320    2c                   endif
142230070320     c
142240070320    2c                   if        not *in30
142250070320     c                   movel     'E'           WEsar4L           1
142260070320     c                   movel     ar4not        dsbl4L
142270070320     c                   else
142280070320     c                   movel     'N'           WEsar4L
142290070320     c                   clear                   dsbl4L
142300070320     c                   clear                   fiar4000
142310070320    2c                   endif
142320070320    1c                   endif
142330070320     c
142340070320    1c                   if        tipoela='A'
142350070320     c                   movel     dsbl4l        ar4not
142360070320     c                   movel     dateu         ar4duv
142370070320     c                   movel     dateu         ar4DTR
142380070320     c                   movel     'T'           ar4ftr
142390070320     c
142400070320    3c                   if        wesar4L='N'
142410070320     c                   eval      ar4aas=arbaas
142420070320     c                   eval      ar4lnp=arblnp
142430070320     c                   eval      ar4nrs=arbnrs
142440070320     c                   eval      ar4nsp=arbnsp
142450070320     c                   eval      ar4trc='L'
142460070320     c                   write     fiar4000
142470070320   x3c                   else
142480070320     c                   update    fiar4000
142490070320    3c                   endif
142500070320    1c                   endif
142510070320     c
142520070320    0c                   endif
142530070320     c
142540070320     C     ENDar4L       ENDSR
142550970808     C*
142560970808     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
142570970808     C     CTRERR        BEGSR
142580970808     C* IMPOSTO CAMPI PASSATI
142590970808    1C     O13RON        IFEQ      'S'
142600970808     C                   MOVEL     O95NAR        E14NAR
142610150112     c                   if        d48ffr='E'
142620150112     c                   clear                   werr
142630150112     c                   eval      msgforzatxt=o13msg
142640150112     c                   endif
142650970808    1C                   ENDIF
142660970808    1C     O13ROC        IFEQ      'S'
142670970808     C                   MOVEL     O95CAP        E14CAP
142680150112     c                   if        d48ffr='E'
142690150112     c                   clear                   werr
142700150112     c                   eval      msgforzatxt=o13msg
142710150112     c                   endif
142720970808    1C                   ENDIF
142730970808    1C     O13ROP        IFEQ      'S'
142740970808     C                   MOVEL     O95PRV        E14PRV
142750150112     c                   if        d48ffr='E'
142760150112     c                   clear                   werr
142770150112     c                   eval      msgforzatxt=o13msg
142780150112     c                   endif
142790970808    1C                   ENDIF
142800970808    1C     O13ROL        IFEQ      'S'
142810970808     C                   MOVEL     O95LOC        E14LOC
142820150112     c                   if        d48ffr='E'
142830150112     c                   clear                   werr
142840150112     c                   eval      msgforzatxt=o13msg
142850150112     c                   endif
142860970808    1C                   ENDIF
142870970808     C* ERRORE O RICERCA CAMPO
142880970808    1C     WERR          IFNE      ' '
142890970808     C*
142900970808    2C     VIDMSG        IFNE      *BLANKS
142910970808     C                   SETON                                        28
142920970808    2C                   ENDIF
142930970808     C* POSIZIONAMENTO CURSORE
142940970808    2C                   SELECT
142950970808     C     WERR          WHENEQ    '1'                                          R.SOCIALE
142960970808     C                   SETON                                        40
142970970808     C     WERR          WHENEQ    '2'                                          INDIRIZZO
142980970808     C                   SETON                                        41
142990970808     C     WERR          WHENEQ    '5'                                          CAP
143000970808     C     WCLI          IFEQ      'D'
143010970808     C                   SETON                                        43
143020970808     C                   ELSE
143030970808     C                   SETON                                        42
143040970808     C                   END
143050970808     C     WERR          WHENEQ    '3'                                          LOCALITA
143060970808     C                   SETON                                        43
143070970808     C     WERR          WHENEQ    '4'                                          PROVINCIA
143080970808     C     WCLI          IFEQ      'D'
143090970808     C                   SETON                                        50
143100970808     C                   ELSE
143110970808     C                   SETON                                        45
143120970808     C                   END
143130970808     C     WERR          WHENEQ    '6'                                          NAZIONE
143140970808     C                   SETON                                        44
143150970808     C     WERR          WHENEQ    '7'                                          PARTITA IVA
143160970808     C     WCLI          IFEQ      'D'
143170970808     C                   SETON                                        45
143180970808     C                   ELSE
143190970808     C                   SETON                                        49
143200970808     C                   END
143210970808     C     WERR          WHENEQ    '8'                                          COD.ISO
143220970808     C     WCLI          IFEQ      'D'
143230970808     C                   SETON                                        46
143240970808     C                   ELSE
143250970808     C                   SETON                                        50
143260970808     C                   END
143270970808     C     WERR          WHENEQ    '9'                                          INOLTRO
143280970808     C     WCLI          ANDEQ     'D'
143290970808     C                   SETON                                        49
143300020318     c     werr          wheneq    '0'
143310020318     C     WCLI          IFEQ      'D'
143320020318     C                   SETON                                        50
143330020318     C                   ELSE
143340020318     C                   SETON                                        45
143350020318     C                   END
143360020318     c
143370970808    2C                   ENDSL
143380970808     C*
143390970808     C                   SETON                                        90
143400970808    1C                   ENDIF
143410970808     C                   ENDSR
143420091217     c*
143430091217     c* Aggiornamento per record GEN ddel fiar5 --------------------------------*
143440091217     c     RegisAR5GEN   BEGSR
143450091217     c                   clear                   dar5gen
143460091217     c                   Eval      kAr5Trd = 'GEN'
143470091217     c     kAr5          Chain     Fiar501l
143480091217      * Aggiorno Fiar5
143490091217if  1c                   If        %Found(Fiar501l)
143500091217     c                   movel     ar5uni        DAr5Gen
143510091217     c*
143520091217     c* Aggiorno tipo incasso in arrivo
143530091217    2c                   select
143540091217     c                   when      Wtipoagg='TIC'
143550091217     c                   Eval      §Ar5ticmb=vidtic
143560100119     c                   eval      §ar5flgMB='S'
143570091217     c
143580091217     c                   when      Wtipoagg='REF'
143590091217     c                   Eval      §Ar5Ref = VidNom
143600091217     c                   Eval      §Ar5Teld = VidTel
143610140403     c                   when      Wtipoagg='PIK'
143620140407     c                   Eval      §Ar5pk2 = wpk2
143630091217    2c                   ENDSL
143640091217
143650091217     c                   Movel(p)  DAr5Gen       Ar5Uni
143660091217     c
143670091217      * se devo trasmettere alla sede sfleggo per la trasmissione
143680091217    2c                   If        §Ar5Trse = 'S'
143690091217     c                   Clear                   Ar5Ft2
143700091217    2c                   EndIf
143710091217     c                   Clear                   Ar5Ft3
143720091217     c                   Update    Fiar5000
143730091217      * Scrivo Fiar5
143740091217   x1c                   Else
143750091217      * se immessi i dati
143760091217     c                   Clear                   Fiar5000
143770091217     c                   Eval      Ar5Aas = ArbAas
143780091217     c                   Eval      Ar5Lnp = ArbLnp
143790091217     c                   Eval      Ar5Nrs = ArbNrs
143800091217     c                   Eval      Ar5Nsp = ArbNsp
143810091217     c                   Eval      Ar5Trd = 'GEN'
143820091217     c                   Move      Dateu         Ar5Dac
143830091217     c                   Movel     W0140         Ar5Orc
143840091217     c                   Clear                   Dar5Gen
143850091217     c
143860091217    2c                   select
143870091217     c                   when      Wtipoagg='TIC'
143880091217     c                   Eval      §Ar5ticmb=vidtic
143890100119     c                   eval      §ar5flgMB='S'
143900091217     c
143910091217     c                   when      Wtipoagg='REF'
143920091217     c                   Eval      §Ar5Ref = VidNom
143930091217     c                   Eval      §Ar5Teld = VidTel
143940140403     c                   when      Wtipoagg='PIK'
143950140407     c                   Eval      §Ar5pk2 = wpk2
143960091217    2c                   ENDSL
143970091217
143980091217     c                   Movel(p)  DAr5Gen       Ar5Uni
143990091217    2c                   if        ar5uni<>*blanks
144000091217     c                   Write     Fiar5000
144010091217    2c                   EndIf
144020091217    1c                   EndIf
144030091217     c
144040091217     c                   ENDSR
144050091217     c
144060911212     C*--- ALLOCO OGGETTI --------------------------------------------*
144070170116     C**   ALLOC         BEGSR
144080170116     C**                 MOVE      ')'           VAR               7
144090170116     C**                 MOVE      '))'          VAR2              8
144100170116     C**                 Z-ADD     48            LUNG             15 5
144110920120     C*
144120170116     C**                 MOVEA     SFILE         C20(13)
144130170116     C**                 MOVEA     SFILE         C21(13)
144140170116     C**                 MOVEA     SFILE         C22(13)
144150170116     C**                 MOVEA     C22           SOVR(1)
144160170116     C**                 MOVEA     C20           SALC(1)
144170170116     C**                 MOVEA     C21           SDLC(1)
144180920120     C*
144190041027     C* solo PER SEDE
144200170116     C**   §7LFFI        IFEQ      'S'
144210911212     C* CHECK OBJ E ADDPFM
144220170116     C**                 Z-ADD     046           PARMBR            3 0
144230170116     c**                 move      046           $mbr
144240170116     C**                 MOVEL     SFILE         FISICO
144250170116     C**                 CALL      'TRUL50C'
144260170116     C**                 PARM                    FISICO           10
144270170116     C**                 PARM                    MBRFIS           10
144280170116     C**                 PARM                    LIB              10
144290170116     C**                 PARM                    LOGICO           10
144300170116     C**                 PARM                    MBRLOG           10
144310170116     C**                 PARM                    ULFLG             1
144320911212     C** OVRDBF
144330170116     c**                 movel     $mbr          var
144340170116     C**                 MOVE      VAR           SOVR
144350170116     C**                 MOVEL     *BLANKS       COMMAN           80
144360170116     C**                 MOVEA     SOVR(1)       COMMAN
144370170116     C**                 CALL      'QCMDEXC'
144380170116     C**                 PARM                    COMMAN
144390170116     C**                 PARM                    LUNG
144400911212     C* ALLOCO OGGETTO
144410170116     c**                 movel     $mbr          var2
144420170116     C**                 MOVE      VAR2          SALC
144430170116     C**                 MOVEL     *BLANKS       COMMAN           80
144440170116     C**                 MOVEA     SALC(1)       COMMAN
144450170116     C**                 CALL      'QCMDEXC'                            91
144460170116     C**                 PARM                    COMMAN
144470170116     C**                 PARM                    LUNG
144480911212     C*
144490911212     C* 90 ON - MEMBRO ALLOCATO MESSAGGIO DI ATTENDERE
144500170116     C** 91              GOTO      ENDALC
144510170116     C**                 END
144520941212     C*
144530170116     C**   ENDALC        TAG
144540941212     C* 91 ON - ERRORE
144550941212     C*
144560170116     C**   *IN91         IFEQ      *ON
144570170116     C**                 SETON                                        28
144580170116     C**                 MOVEa     MSG(136)      k78
144590170116     C**                 MOVEa     SFILE         k78(68)
144600170116     C**                 MOVEa     k78           vidmsg
144610170116     C**                 ENDIF
144620941212     C*
144630170116     C**                 ENDSR
144640920831     C*
144650940321     C*--- AGGIORNO FILE TASSAZIONE PADRONCINI -----------------------*
144660950113     C     AGGPDS        BEGSR
144670041008     C                   MOVEL     ' '           UNO               1
144680041008     c* richiamo pgm fnlv57ds
144690041008     c                   clear                   fnlv57ds
144700041008     C* 01 ON  - VARIAZIONE DESTINATARIO
144710041008    1c     *in01         ifeq      *on
144720041008    2C     ARBRSD        IFNE      OLDRSD
144730041008     C     ARBCAD        ORNE      OLDCAD
144740041008     C     ARBFIN        ORNE      OLDFIN
144750041008     c                   eval      uno='1'
144760041008    2c                   endif
144770041008    1c                   endif
144780041008     c
144790041008    1c                   if        *in02
144800041008    2c     ARBVLF        IFNE      OLDVLF
144810041008     C     ARBPKF        orne      OLDPKF
144820041008     c                   eval      uno='1'
144830041008    2c                   endif
144840041008    1c                   endif
144850041008     c*
144860041008    1c                   if        uno='1'
144870041008     c                   eval      i57aas=arbaas
144880041008     c                   eval      i57lnp=arblnp
144890041008     c                   eval      i57nrs=arbnrs
144900041008     c                   eval      i57nsp=arbnsp
144910041008     c
144920041008     c                   select
144930041008     c* Se presenti entrambe le bolle
144940041008     c                   when      waltrabolla=' '
144950070320     c                   eval      i57drt=arbdrt
144960070320     c                   eval      i57nrt=arbnrt
144970070320     c                   eval      i57fpp=arbfpp
144980070320     c                   eval      i57prt=arbpdr
144990070320     c                   eval      i57pco=arbpdc
145000041008     c* modifica in arrivo non presente fnblp
145010041008     c                   when      *in10 and waltrabolla='N'
145020041008     c                   eval      i57pdr=arbpdc
145030041008     c                   eval      i57tbo='A'
145040041008     c* modifica in partenza non presente fnarb
145050041008     c                   when      not *in10 and waltrabolla='N'
145060070320     c                   eval      i57pdr=arbpdr
145070041008     c                   eval      i57tbo='P'
145080070320     c                   eval      i57drt=arbdrt
145090070320     c                   eval      i57nrt=arbnrt
145100070320     c                   eval      i57fpp=arbfpp
145110041008     c                   endsl
145120041008     c
145130041008     c                   eval      i57ncl=arbncl
145140041011     c   02              eval      i57tvl='F'
145150041008     c* volume vecchio
145160041008     c                   eval      i57vlc=arbvlc
145170041008     c                   eval      i57ncr=arbncr
145180041008     c                   eval      i57vlf=oldvlf
145190041008     c* volume nuovo
145200041008     c                   eval      i57vcn=arbvlc
145210041008     c                   eval      i57ncn=arbncp
145220041008     c                   eval      i57vfn=arbvlf
145230041008     c* peso vecchio
145240041008     c                   eval      i57pkc=arbpkc
145250041008     c                   eval      i57ncp=arbncp
145260041008     c                   eval      i57pkf=oldpkf
145270041008     c* peso nuovo
145280041008     c                   eval      i57pcn=arbpkc
145290041008     c                   eval      i57npn=arbncp
145300041008     c                   eval      i57pfn=arbpkf
145310041008     c* destinatario vecchio
145320041008     c                   eval      i57rsd=oldrsd
145330041008     c                   eval      i57cad=oldcad
145340041008     c                   eval      i57fin=oldfin
145350041008     c* destinatario nuovo
145360041008     c                   eval      i57rdn=arbrsd
145370041008     c                   eval      i57cdn=arbcad
145380041008     c                   eval      i57fdn=arbfin
145390041008     c
145400041008     c                   call      'FNLV57R'
145410041008     c                   parm                    kpjba
145420041008     c                   parm                    fnlv57ds
145430041008     c                   endif
145440940321     C*
145450940321     C                   ENDSR
145460950113     C*
145470950113     C* AGGIORNA PADRONCINI PER IL MITTENTE --------------------------*
145480950113     C     AGGPMT        BEGSR
145490950113     C*
145500950113    1C     ARBKSC        IFNE      OLDKSC
145510950113     C     ARBRSM        ORNE      OLDRSM
145520950113     C     ARBCAM        ORNE      OLDCAM
145530950113     C*
145540950113     C* CONTROLLO CON QUALE VOLUME HO AGGIORNATO LA BOLLA
145550950113     C                   CLEAR                   WVOL
145560950113     C*
145570950113     C* VOLUME PARZIALE "Z"
145580950113     C*
145590050111     C     ARBNCR        ifne      ARBNCL
145600950113     C*
145610950113    3C     ARBVLC        IFGE      ARBVLF
145620950113     C                   Z-ADD     ARBVLC        WVOL
145630950113   X3C                   ELSE
145640950113     C                   Z-ADD     ARBVLF        WVOL
145650950113    3C                   ENDIF
145660950113     C* VOLUME TOTALE "T"
145670950113   X2C                   ELSE
145680950113     C                   Z-ADD     ARBVLC        WVOL
145690950113    2C                   ENDIF
145700950113     C****
145710950113     C** ESEGUO L'AGGIORNAMENTO DEI FILE PADRONCINI 2 VOLTE:
145720011126     C**  - LA PRIMA   VOLTA PER I FILE fiftt00F/fiftd00F
145730011126     C**  - LA SECONDA VOLTA PER I FILE fifst00F/fifsd00F - SIMULAZIONE
145740950113     C****
145750950113     C* 17 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
145760950113     C                   SETOFF                                       17
145770950113     C*
145780070320     C                   MOVE      arbPDR        WPDR
145790070320     C                   Z-ADD     arbDRT        WDRT
145800950113     C* NUMERO DISTINTA
145810070320    2C     arbFPP        IFEQ      'M'
145820950113     C                   Z-ADD     1000          WNDC
145830950113     C                   ELSE
145840950113     C                   Z-ADD     5000          WNDC
145850950113    2C                   ENDIF
145860070320     C                   ADD       arbNRT        WNDC
145870950113     C*
145880950113     C*
145890950113    2C                   DO        2
145900011126     C   17KFTT2         CHAIN     fifst000                           30
145910011126     C  N17KFTT2         CHAIN     fiftt000                           30
145920950113     C*
145930950113    3C     *IN30         IFEQ      *OFF
145940950113     C*
145950950113     C* SE GIA' CONFERMATA NON AGGIORNO NIENTE
145960950113    4C     FTTFVL        IFNE      'C'
145970020326     c*
145980020326     c  n17kftd5         chain     fiftd005                           30
145990020326     c   17kftd5         chain     fifsd005                           30
146000020326     c     *in30         ifeq      *off
146010020326     c                   z-add     arbpkf        ftdpkl
146020020326     c                   z-add     wvol          ftdvlu
146030020326     c                   movel     arbrsm        ftdrsc
146040020326     c* dò per scontato che la bolla sia un porto franco
146050020326     c                   z-add     arbksc        ftdksc
146060020326     c                   movel     arbcam        ftdcap
146070020326     c                   movel     arbfap        ftdfin
146080020326     c* se è stato modificato codice cliente o ragione sociale devo
146090020326     c* aggiornare anche il numero di stop
146100020327    1C     ARBKSC        IFNE      OLDKSC
146110020327     C     ARBRSM        ORNE      OLDRSM
146120020327     c                   exsr      aggstp
146130020327     c                   endif
146140020327     c  n17              update    fiftd005
146150020327     c   17              update    fifsd005
146160020326     c                   endif
146170950113     C*
146180950113     C* SE VALORIZZATA SVALORIZZO
146190950113     C                   EXSR      SVAFTT
146200950113     C*
146210950113    4C                   ENDIF
146220950113    3C                   ENDIF
146230950113     C*
146240950113     C* 17 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
146250950113     C                   SETON                                        17
146260950113    2C                   ENDDO
146270950113     C*
146280950113    1C                   ENDIF
146290950113     C*
146300950113     C                   ENDSR
146310020327     c*
146320020327     c* AGGSTP - Aggiornamento numero stop a cambio di mittente ------*
146330020327     C     AGGSTP        BEGSR
146340020327     c                   clear                   wstp
146350020327     C* ESCLUDENDO FRANCHI E ASSEGNATI VARI,
146360020327     C*   CHAINO FNCLS00F PER PRENDERE IL NUMERO DEGLI STOP
146370020327     C                   Z-ADD     ftdksc        w0040             4 0
146380020327    5C     w0040         IFNE      *ALL'8'
146390020327     C     w0040         ANDNE     *ALL'9'
146400020327     C     ftdksc        CHAIN     FNCLS000                           31
146410020327     C  N31              z-add     CLSSTP        wstp
146420020327    5C                   ENDIF
146430020327     c     wstp          ifeq      *zeros
146440020327     c* prima di fare la chain mi devo salvare i dati del record di fiftd005
146450020327     c                   movel     dsftd         sav_dsftd
146460020327     C                   MOVEL     ARBKSC        WKSC
146470020327     C                   MOVEL     ARBRSM        WRSM
146480020327     C  N17KFTD          CHAIN(N)  fiftd004                           31
146490020327     C   17KFTD          CHAIN(N)  fifsd004                           31
146500020327     c  n31              z-add     ftdstp        wstp
146510020327     c* Se il numero stop da attribuire è ancora = a 0 devo cercare
146520020327     c* nella distinta il primo numero disponibile
146530020327     c     wstp          ifeq      *zeros
146540020327     C  N17KFTT2         setgt     fiftd001
146550020327     C   17KFTT2         setgt     fifsd001
146560020327     C  N17KFTT2         readpe    fiftd001
146570020327     C   17KFTT2         readpe    fifsd001
146580020327     c                   if        not %eof
146590020327     c                   eval      wstp = (ftdstp+1)
146600020417     c     wstp          iflt      1000
146610020417     c                   z-add     1000          wstp
146620020417     c                   endif
146630020417     c                   else
146640020417     c* qui non dovrebbe mai entrare ma lo gestisco comunque anche
146650020417     c* per unifomrità con fnlsa2r
146660020417     c                   z-add     1000          wstp
146670020327     c                   endif
146680020327     c                   endif
146690020327     c*
146700020327     c* ripristino i dati del record salvato
146710020327     c                   movel     sav_dsftd     dsftd
146720020327     c                   endif
146730020327     c                   z-add     wstp          ftdstp
146740020327     C                   ENDSR
146750020326     c*
146760950113     C* SVALORIZZA UNA DISTINTA --------------------------------------*
146770950113     C     SVAFTT        BEGSR
146780950113     C* SOLO SE VALORIZZATA
146790950113    6C     FTTFVL        IFEQ      'V'
146800950113     C                   MOVEL     FTDPDR        PA3PDR                         *COD. PADRONCINO
146810950113     C                   MOVEL     FTDTSR        PA3TSR                         *TIPO PRESTAZIONE
146820950113     C                   Z-ADD     FTDNDC        PA3NDC                         *NUMERO DISTINTA
146830950113     C                   Z-ADD     FTDDDC        PA3DDC                         *DATA   DISTINTA
146840950113     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
146850950113     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
146860950113     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
146870041122     C  n17              MOVEL     ' '           PA3sml                         *FLG VAL.MANUAL.TEST
146880041122     C   17              MOVEL     'S'           PA3sml                         *FLG VAL.MANUAL.TEST
146890950113     C                   MOVEL     PARAM3        KPJBU
146900011129     C  N17              CALL      'FICNE2R'
146910950113     C                   PARM                    KPJBA
146920011129     C   17              CALL      'FICNE9C'
146930950113     C                   PARM                    KPJBA
146940950113    6C                   ENDIF
146950950113     C                   ENDSR
146960941102     C*
146970941102     C* CONTROLLO DIVISA ---------------------------------------------*
146980941102     C     CTRVLT        BEGSR
146990011011      *
147000011011      * La divisa è obbligatoria se è inserito un importo
147010011011     C     W0133         IFGT      *ZEROS
147020011011     C     WVLT          ANDEQ     *BLANKS
147030011011     C                   MOVEL     MSG(100)      VIDMSG
147040011011     C                   SETON                                        9028
147050011011     C                   GOTO      ENDVLT
147060011011     C                   ENDIF
147070941102     C*
147080941102     C                   MOVEL     'CV'          COD
147090941102     C* RICERCA
147100941102     C     '?'           SCAN      WVLT                                   90
147110941102     C     *IN90         IFEQ      *ON
147120991011     C                   CLEAR                   §KEY
147130941102     C                   CALL      'X§TABER'
147140941102     C                   PARM                    CODUT
147150941102     C                   PARM                    COD
147160941102     C                   PARM                    §KEY
147170991011     C                   PARM                    §DES             25
147180941102     C                   MOVEL     §KEY          WVLT
147190941102     C                   GOTO      ENDVLT
147200941102     C                   ENDIF
147210941102     C*
147220941102     C* CONTROLLO DIVISA
147230941102     C                   MOVEL(P)  WVLT          KEY
147240941213     C                   EXSR      CTABEL
147250941102     C* ERRORE
147260941102    1C     *IN30         IFEQ      *ON
147270941102     C                   MOVEL     MSG(26)       VIDMSG
147280941102     C                   SETON                                        9028
147290941102     C                   GOTO      ENDVLT
147300941102    1C                   ENDIF
147310941102     C                   MOVEL     TBLUNI        DSCV
147320941102     C*
147330941102     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
147340941102    1C     §CVFDC        IFNE      'S'
147350941102     C                   MOVE      W0133         W0033             3 3
147360941102    2C     W0033         IFNE      0
147370941102     C                   MOVEL     MSG(27)       VIDMSG
147380941102     C                   SETON                                        9028
147390941102     C                   GOTO      ENDVLT
147400941102    2C                   ENDIF
147410941102    1C                   ENDIF
147420011010      *
147430011010      * Controllo il numero di decimali immessi
147440011011    1C     §CVFDC        IFEQ      'S'
147450011011     C     W0133         ANDNE     *ZEROS
147460011011      *
147470011011    2C                   SELECT
147480011011     C     §CVDEC        WHENEQ    3
147490011011     C     §CVDEC        WHENEQ    2
147500011011     C                   MOVE      W0133         W0010             1 0
147510011011    3C     W0010         IFNE      *ZEROS
147520011011     C                   MOVEA     MSG(93)       K78
147530011011     C                   MOVE      §CVDEC        WCVDEC            2
147540011011     C                   MOVEA     WCVDEC        K78(29)
147550011011     C                   MOVEA     K78           VIDMSG
147560011011     C                   SETON                                        9028
147570011011    3C                   ENDIF
147580011011     C     §CVDEC        WHENEQ    1
147590011011     C                   MOVE      W0133         W0020             2 0
147600011011    3C     W0020         IFNE      *ZEROS
147610011011     C                   MOVEA     MSG(93)       K78
147620011011     C                   MOVE      §CVDEC        WCVDEC            2
147630011011     C                   MOVEA     WCVDEC        K78(29)
147640011011     C                   MOVEA     K78           VIDMSG
147650011011     C                   SETON                                        9028
147660011011    3C                   ENDIF
147670011011     C     §CVDEC        WHENEQ    0
147680011011    2C                   ENDSL
147690011011      *
147700011011    1C                   ENDIF
147710941102     C*
147720941102     C     ENDVLT        ENDSR
147730941212     c*
147740941212     C*--- CHAIN TABEL00F --------------------------------------------*
147750941212     C     CTABEL        BEGSR
147760941212     C     KTAB2         CHAIN     TABEL                              30
147770941212     C  N30TBLFLG        IFNE      ' '
147780941212     C                   SETON                                        30
147790941212     C                   END
147800941212     C                   ENDSR
147810941212     C*---------------------------------------------------------------
147820941212     C* DISALLOCO OGGETTO MEMBRO PER LA SEDE
147830941212     C*---------------------------------------------------------------
147840170116     C**   DLCSED        BEGSR
147850170116     c**                 move      046           $mbr
147860170116     c**                 movel     $mbr          var2
147870170116     C**                 MOVE      VAR2          SDLC(1)
147880170116     C**                 MOVEL     *BLANKS       COMMAN           80
147890170116     C**                 MOVEA     SDLC(1)       COMMAN
147900170116     C**                 CALL      'QCMDEXC'                            91
147910170116     C**                 PARM                    COMMAN
147920170116     C**                 PARM                    LUNG
147930170116     C**                 ENDSR
147940941213     C*---------------------------------------------------------------
147950941213     C* STORICIZZO LA TASSAZIONE
147960941213     C*---------------------------------------------------------------
147970941213     C     STOARB        BEGSR
147980991006     C* PER LA PRIMA BOLLA KSC E CTR LI PRENDO SEMPRE DALLA BOLLA
147990991006     C* PRINCIPALE PERCHE' SONO SICURA DI AVERLA
148000991006     C* PER LA SECONDA BOLLA LI DEVO PRENDERE DA AR6 PERCHE'
148010991006     C* QUELLI DELLA BOLLA PRINCIPALE SI RIFERISCONO ALLA PRIMA BOLLA
148020991006     C     AR6TRC        IFEQ      '2'
148030991006     C                   Z-ADD     AR6KSC        ARBKSC
148040991006     C                   Z-ADD     AR6CTR        ARBCTR
148050991006     C                   ENDIF
148060991006     C                   MOVEL     AR6DIV        ARBDIV
148070941213     C                   MOVEL     AR6POR        ARBPOR
148080941213     C                   MOVEL     AR6SV1        ARBSV1
148090941213     C                   MOVEL     AR6VA1        ARBVA1
148100941213     C                   MOVEL     AR6SV2        ARBSV2
148110941213     C                   MOVEL     AR6VA2        ARBVA2
148120941213     C                   MOVEL     AR6SV3        ARBSV3
148130941213     C                   MOVEL     AR6VA3        ARBVA3
148140941213     C                   MOVEL     AR6IMV        ARBIMV
148150941213     C                   MOVEL     AR6ALI        ARBALI
148160941213     C                   MOVEL     AR6CEI        ARBCEI
148170941213     C                   MOVEL     AR6IFT        ARBIFT
148180941213     C                   MOVEL     AR6DFT        ARBDFT
148190941213     C                   MOVEL     AR6NFT        ARBNFT
148200941213     C                   MOVEL     AR6FIV        ARBFIV
148210950114     C* SE VENGO DA VARIAIZONE MITTENTE COME CODICE TARIFFA CI METTO
148220950114     C*  QUELLO SALVATO
148230950114     C   24              MOVEL     MITCTR        ARBCTR
148240991013     C                   MOVEL     AR6FIM        ARBFIM
148250011108      *
148260011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
148270011108      * è in partenza 'P' o in arrivo 'A'
148280011108     C     *IN10         IFEQ      *ON
148290011108     C                   MOVE      'A'           ARBPRU
148300011108     C                   ENDIF
148310011108     C     *IN10         IFEQ      *OFF
148320011108     C                   MOVE      'P'           ARBPRU
148330011108     C                   ENDIF
148340011108      *
148350950114     C*
148360991006     C                   WRITE     FIARBT00
148370950114     C*
148380991006     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
148390941221    1C     WAR7          IFEQ      'E'
148400991006    1C     ARBTRC        ANDEQ     '1'
148410991006     C                   MOVEL     '1'           WTRC
148420991006     C     KARB2         SETLL     FIAR7000
148430991006     C     KARB2         READE(N)  FIAR7000                               30
148440941213     C*
148450941213    2C     *IN30         DOWEQ     *OFF
148460941213     C                   MOVEL     AR7SVN        ARBSVN
148470941213     C                   MOVEL     AR7VAN        ARBVAN
148480991006     C                   WRITE     FIARBU00
148490941213     C*
148500991006     C     KARB2         READE(N)  FIAR7000                               30
148510941213    2C                   ENDDO
148520941213     C                   ENDIF
148530041130      *-------
148540041130      * Seconda bolla
148550041130     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
148560041130    1C     WAR72         IFEQ      'E'
148570041130    1C     ARBTRC        ANDEQ     '2'
148580041130     C                   MOVEL     '2'           WTRC
148590041130     C     KARB2         SETLL     FIAR7000
148600041130     C     KARB2         READE(N)  FIAR7000                               30
148610041130     C*
148620041130    2C     *IN30         DOWEQ     *OFF
148630041130     C                   MOVEL     AR7SVN        ARBSVN
148640041130     C                   MOVEL     AR7VAN        ARBVAN
148650041130     C                   WRITE     FIARBU00
148660041130     C*
148670041130     C     KARB2         READE(N)  FIAR7000                               30
148680041130    2C                   ENDDO
148690041130     C                   ENDIF
148700041130      *-------
148710941213     C*
148720941213     C                   ENDSR
148730941214     C*----------------------------------------------------------------
148740941214     C* RICALCOLO IL VOLUME AUTOMATICO
148750941214     C*----------------------------------------------------------------
148760941214     C     VOLAUT        BEGSR
148770941214     C*
148780941214     C* VOLUME AUTOMATICO A PESO: PESO MODIFICATO
148790941214    3C     VIDPKF        IFNE      ARBPKF
148800941214     C     ARBFVF        ANDEQ     'K'
148810941214     C*
148820120702     C     ARBPKF        DIV(H)    ARBVLF        CLIKPM            9 1
148830941214     C     VIDPKF        DIV(H)    CLIKPM        D20VLU
148840941214     C*
148850941214     C     ARBVLB        IFEQ      0
148860941214     C                   Z-ADD     0,001         D20VLU
148870941214     C                   END
148880941214     C                   MOVEL     ARBFVF        D20FVL
148890941214     C*
148900941214    3C                   END
148910941214     C                   ENDSR
148920941215     C**************************************************************************
148930941215     C*  ABBLANCA ZERO NON SIGNIFICATIVI
148940941215     C**************************************************************************
148950941215     C     ABBL          BEGSR
148960941215     C*
148970941215     C                   SETOFF                                       99
148980941215     C     SKI(I)        DOWEQ     '0'
148990941215     C                   MOVEL     ' '           SKI(I)
149000941215     C                   ADD       1             I
149010941215     C     I             COMP      15                                     99
149020941215     C  N99              END
149030941215     C*
149040941215     C                   ENDSR
149050051114     C**************************************************************************
149060051114     C*  ABBLANCA ZERO NON SIGNIFICATIVI
149070051114     C**************************************************************************
149080051114     C     SR_chkcomb    BEGSR
149090051114     c                   clear                   wcomb             1
149100051114     C* Non è possibile indicare contemporaneamente consegne particolari
149110051117     C*  'S' ed 'X' / 'S' ed 'A' / "S" e "P" / "X" e "P"
149120051114    2C                   IF           (wTC1 = 'X'  and  wTC2 = 'S')
149130051114     C                             or (wTC1 = 'S'  and  wTC2 = 'X')
149140051114     C                             or (wTC1 = 'A'  and  wTC2 = 'S')
149150051114     C                             or (wTC1 = 'S'  and  wTC2 = 'A')
149160051114     C                             or (wTC1 = 'S'  and  wTC2 = 'P')
149170051114     C                             or (wTC1 = 'P'  and  wTC2 = 'S')
149180051117     C***                          or (wTC1 = 'X'  and  wTC2 = 'A')
149190051117     C***                          or (wTC1 = 'A'  and  wTC2 = 'X')
149200051114     C                             or (wTC1 = 'X'  and  wTC2 = 'P')
149210051114     C                             or (wTC1 = 'P'  and  wTC2 = 'X')
149220051114     c                   eval      wcomb = 'S'
149230051114     c                   endif
149240051114     C                   ENDSR
149250060227     C**************************************************************************
149260060301     C*  Richiamo pgm di richiesta/controllo motivazione variazione
149270060227     C**************************************************************************
149280060227     C     ges_mtv       BEGSR
149290060227     c                   clear                   fnlv85ds
149300060227     c                   movel     'V'           ilv85tel
149310060227     c                   if        d48ffr = 'S' or d48ffr = 'V'
149320060227     c                   clear                   ilv85wdw
149330060227     c                   else
149340060301     c                   if        d48ffr = *blanks
149350060301     c                   movel     'E'           ilv85wdw
149360060301     c                   else
149370060227     c                   movel     'N'           ilv85wdw
149380060301     c                   endif
149390060227     c                   endif
149400060227     c                   movel     d48cvb        ilv85cvb
149410060227     c                   if        d48mtv <> *blanks
149420060227     c                   movel     d48mtv        ilv85mtv
149430060227     c                   movel     d48not        ilv85nt1
149440060227     c                   move      d48not        ilv85nt2
149450060227     c                   endif
149460060227     c                   movel     fnlv85ds      kpjbu
149470060302     c                   call      'FNLV85R'
149480060227     c                   parm                    kpjba
149490060227     c                   movel     kpjbu         fnlv85ds
149500060227     c                   if        olv85err = '1'
149510060227     c                   seton                                        90
149520060227     C                   eval      vidmsg='Motivazione variazione mancante o er-
149530060227     c                             rata'
149540060227     c                   endif
149550060301     c                   if        olv85f12 = ' ' and olv85err = ' '
149560060301     c                   movel     'N'           w48mct
149570060301     c                   endif
149580060227     c                   endsr
149590060301     C**************************************************************************
149600060301     C*  Richiamo pgm per storicizzazione motivazione variazione
149610060301     C**************************************************************************
149620060301     C     sto_mtv       BEGSR
149630060303     c                   clear                   fnlv85ds
149640060301     c                   movel     ' '           ilv85tla
149650060301     c                   movel     'R'           ilv85tel
149660060301     c                   movel     d48cvb        ilv85cvb
149670060301     c                   z-add     arbdtv        ilv85dac
149680060301     c                   z-add     arborv        ilv85orc
149690060301     c                   z-add     arbaas        ilv85aas
149700060301     c                   z-add     arblnp        ilv85lnp
149710060301     c                   z-add     arbnrs        ilv85nrs
149720060301     c                   z-add     arbnsp        ilv85nsp
149730060303     c                   if        d48mtv <> *blanks and
149740060303     c                             (d48ffr = 'E' or d48mct = 'N')
149750060303     c                   movel     d48mtv        ilv85mtv
149760060303     c                   movel     d48not        ilv85nt1
149770060303     c                   move      d48not        ilv85nt2
149780060303     c                   endif
149790060301     c                   movel     fnlv85ds      kpjbu
149800060301     c                   call      'FNLV85R'
149810060301     c                   parm                    kpjba
149820060301     c                   endsr
149830120330     C**************************************************************************
149840120330     C*  AGGIORNAMENTO DA MANUTENZIONE BOLLE DI SEDE (CAUS.VAR.BOLLA "SD")
149850120330     C**************************************************************************
149860120330     C     GESSD         BEGSR
149870120330     c                   clear                   errp
149880120330     c                   clear                   erra
149890120419     c                   clear                   savcpi_p
149900120419     c                   clear                   savksc_p
149910120419     c                   clear                   savctr_p
149920120419     c                   clear                   savcpi_a
149930120419     c                   clear                   savksc_a
149940120419     c                   clear                   savctr_a
149950120330     c* chaino le bolle in partenza e in arrivo
149960120330     C     KARB          CHAIN(e)  FNBLP01L
149970120402    1c                   if        %error
149980120330     c                   eval      errp='A'
149990120402   x1c                   else
150000120402    2c                   if        not %found(fnblp01l)
150010120330     c                   eval      errp='N'
150020120418     c                   else
150030120418     c                   eval      savcpi_p=arbcpi
150040120418     c                   eval      savksc_p=arbksc
150050120418     c                   eval      savctr_p=arbctr
150060120402    2c                   endif
150070120402    1c                   endif
150080120330     C     KARB          CHAIN(e)  FNARB01L
150090120402    1c                   if        %error
150100120330     c                   eval      erra='A'
150110120402   x1c                   else
150120120402    2c                   if        not %found(fnarb01l)
150130120330     c                   eval      erra='N'
150140120413     c                   else
150150120413     c                   eval      savcpi_a=arbcpi
150160120418     c                   eval      savksc_a=arbksc
150170120418     c                   eval      savctr_a=arbctr
150180120402    2c                   endif
150190120402    1c                   endif
150200120402     c* Una delle due bolle allocate --> restituisco messaggio di avviso
150210120402    1c                   if        errp='A' or erra='A'
150220120402     c                   eval      d48err='A'
150230120402     c                   eval      d48msg='Bolla di filiale allocata da altro l+
150240120418     c                             avoro:NON AGGIORNATA in'
150250120418    2c                   if        errp='A'
150260120418     c                   eval      d48msg=%trim(d48msg)+' Partenza'
150270120418    3c                   if        erra='A'
150280120418     c                   eval      d48msg=%trim(d48msg)+' e'
150290120418    3c                   endif
150300120418    2c                   endif
150310120418    2c                   if        erra='A'
150320120418     c                   eval      d48msg=%trim(d48msg)+' Arrivo'
150330120418    2c                   endif
150340120402    1c                   endif
150350120330     c* se entrambe le bolle allocate/inesistenti vado a fine
150360120402    1c                   if        errp<>*blanks and erra<>*blanks
150370120330     c                   leavesr
150380120402    1c                   endif
150390120404     c*Variazione di PESO
150400120404    1c                   if        §bdpkf<>arbpkf
150410120402     c                   z-add     §bdpkf        arbpkf
150420120404    1c                   endif
150430120404     c*Variazione di VOLUME: il volume, in sede, può essere variato solo da
150440120404     c* profilo di sede. Siccome in questi casi la variazione avviene solitamente
150450120404     c* quando in filiale la bolla non c'è più sostituisco sempre
150460120404    1c                   if        §bdvlf<>arbvlf or §bdfvf<>arbfvf
150470120404     c                   z-add     §bdvlf        arbvlf
150480120411     c                   movel     §bdfvf        arbfvf
150490120404    1c                   endif
150500120529     c* IMOPORTO DA ASSICURARE
150510120529     c                   if        §btias<>arbias or §btvas<>arbvas
150520120529     c                   z-add     §btias        arbias
150530120529     c                   movel     §btvas        arbvas
150540120529     c                   endif
150550120404     c* Variazione TASSAZIONE: vengono caricate solo se c'è corrispodenza
150560120404     c* fra sede e filiale al livello di tipo di fattura: Segue Fattura o
150570120404     c* Fattura Immediata
150580120411     c* Chaino fiar6 per vedere se S.F. o  F.I.:
150590120404     c                   clear                   wfatfil
150600120404     c                   clear                   ar6ksc
150610120411     c                   clear                   war6
150620120416     c                   clear                   fiar6000
150630120416     C                   MOVEL     §bttrc        WTRC
150640120404     c     karb2         chain     fiar601l
150650120411    1c                   if        %found(fiar601l) and ar6nft= 0 or
150660120411     c                             not %found(fiar601l)
150670120404     c                   eval      wfatfil='SF'
150680120411     c                   if        not %found(fiar601l)
150690120411     c                   eval      war6='N'
150700120411     c                   endif
150710120404     c                   else
150720120404     c                   eval      wfatfil='SD'
150730120404    1c                   endif
150740120404    1c                   if        d48cvb=wfatfil
150750120404     c
150760120404     c* . . . . . . . QUANTITA' DA FATTURARE
150770120411    2c                   if        §btqft<>arbqft
150780120411     c                   z-add     §btqft        arbqft
150790120404    2c                   endif
150800120404     c* . . . . . . . CLIENTE DI FATTURAZIONE e COD.TARIFFA
150810120419     C*
150820120404     C                   MOVEL     '3A'          COD
150830120404     C                   MOVEL(P)  ARBCBO        KEY
150840120404     C                   EXSR      CTABEL
150850120404     C  N30              MOVEL     TBLUNI        DS3A
150860120404     C   30              CLEAR                   DS3A
150870120413     c* assegnato
150880120404    2c                   if        §bttrc='2' or %subst(§3atb1:1:1)='A'
150890120404    3c                   if        §btksc<>ar6ksc
150900120404     c                   eval      ar6ksc=§btksc
150910120404    3c                   endif
150920120404    3c                   if        §btctr<>ar6ctr
150930120404     c                   eval      ar6ctr=§btctr
150940120404    3c                   endif
150950120418     c* Se assegnato no seconda bolla devo aggiornare su arb anche il ksc/ctr
150960120418    2c                   if        %subst(§3atb1:1:1)='A'
150970120418    3c                   if        §btksc<>savksc_a
150980120418     c                   eval      savksc_a=§btksc
150990120418    3c                   endif
151000120418    3c                   if        §btctr<>savctr_a
151010120418     c                   eval      savctr_a=§btctr
151020120418    3c                   endif
151030120418     c                   endif
151040120404   x2c                   else
151050120413     c* franco
151060120413     c*  se non codificato non faccio niente
151070120413     c                   z-add     §btksc        w0040
151080120418    3c                   if        w0040<>8888 and §btksc<>arbksc
151090120413     c                   exsr      gessd_ksc
151100120411    3c                   endif
151110120411    3c                   if        §btctr<>arbctr
151120120411     c                   eval      arbctr=§btctr
151130120411    3c                   endif
151140120404    2c                   endif
151150120404     c* . . . . . . . CAMPI DI TASSAZIONE
151160120404     c* . . . . Divisa
151170120404     c                   if        §btdiv<>ar6div
151180120404     c                   eval      ar6div=§btdiv
151190120404     c                   endif
151200120404     c* . . . . Porto
151210120404     c                   if        §btpor<>ar6por
151220120404     c                   eval      ar6por=§btpor
151230120404     c                   endif
151240120411     c* . . . . Imponibile IVA
151250120411     c                   if        §btimv<>ar6imv
151260120411     c                   eval      ar6imv=§btimv
151270120411     c                   endif
151280120411     c* . . . . Causale esenzione
151290120411     c                   if        §btcei<>ar6cei
151300120411     c                   eval      ar6cei=§btcei
151310120411     c                   endif
151320120404     c* . . . . Varie - In §SV e §VA ci sono le varie ricevute dal chiamante
151330120404     c*                 IN WSV e WVA carico le varie presenti sul fiar6/7
151340120411     C                   MOVEL     §sv(1)        ar6sv1
151350120411     C                   Z-ADD     §va(1)        ar6va1
151360120411     C                   MOVEL     §sv(2)        ar6sv2
151370120411     C                   Z-ADD     §va(2)        ar6va2
151380120411     C                   MOVEL     §sv(3)        ar6sv3
151390120411     C                   Z-ADD     §va(3)        ar6va3
151400120411     c* cancello varie su fiar7 se presenti
151410120419     C                   MOVEL     §bttrc        WTRC
151420120411     c     karb2         setll     fiar7000
151430120411     c     karb2         reade     fiar7000
151440120411     c                   dow       not %eof(fiar701l)
151450120411     C                   DELETE    FIAR7000
151460120411     c     karb2         reade     fiar7000
151470120411     c                   enddo
151480120411     c* se presenti più di 3 verie le memorizzo nell'fiar7
151490120411    2c                   if        §sv(4)<>' '
151500120411     c                   z-add     4             inx
151510120411     c                   clear                   AR7ATB
151520120411     c                   z-add     d48aas        AR7AAS
151530120411     c                   z-add     d48lnp        AR7LNP
151540120411     c                   z-add     d48nrs        AR7NRS
151550120411     c                   z-add     d48nsp        AR7NSP
151560120419     c                   movel     §bttrc        AR7TRC
151570120411     c                   dow       §sv(inx)<>*blanks or inx>20
151580120411     C                   MOVEL     §sv(inx)      ar7svn
151590120411     C                   z-add     §va(inx)      ar7van
151600120411     C                   write     fIAR7000
151610120411     c                   add       1             inx
151620120411     c                   enddo
151630120411    2C                   endif
151640120416     c* Aggiornamento FIAR6
151650120416     c* se record non esiste lo creo solo se è pieno almeno uno dei campi
151660120416    2c                   if        war6='N'
151670120416    3c                   if        ar6ksc>0 or ar6por>0 or ar6sv1<>*blanks
151680120416     c                             or ar6sv2<>*blanks or ar6sv3<>*blanks
151690120416     c                             or ar6cei<>*blanks
151700120411     c                   eval      AR6AAS=d48aas
151710120411     c                   eval      AR6LNP=d48lnp
151720120411     c                   eval      AR6NRS=d48nrs
151730120411     c                   eval      AR6NSP=d48nsp
151740120411     c                   eval      AR6TRC=§bttrc
151750120411
151760120411     c                   write     fiar6000
151770120416    3c                   endif
151780120417     c                   else
151790120416    3c                   if        ar6ksc>0 or ar6por>0 or ar6sv1<>*blanks
151800120416     c                             or ar6sv2<>*blanks or ar6sv3<>*blanks
151810120416     c                             or ar6cei<>*blanks
151820120404     c                   except    ar6sd
151830120416     c                   else
151840120416     c                   delete    fiar6000
151850120416     c                   endif
151860120416    2c                   endif
151870120404   x1c                   else
151880120404     c                   unlock    fiar601l
151890120404    1c                   endif
151900120404     c* aggiornamento file
151910120418     c* BLP
151920120404    1c                   if        errp=*blanks
151930120418     c                   eval      arbcpi=savcpi_p
151940120418     c* solo se assegnato rimetto il ksc/ctr che c'era
151950120418     c                   if        %subst(§3atb1:1:1)='A'
151960120418     c                   eval      arbksc=savksc_p
151970120418     c                   eval      arbctr=savctr_p
151980120418     c                   endif
151990120404     c                   except    arbsdblp
152000120404    1c                   endif
152010120418     c* ARB
152020120404    1c                   if        erra=*blanks
152030120416     c* P.iva DEL MITTENTE SU ARB LA AGGIORNO SOLO SE FRANCO NO DOPPIA BOLLA
152040120419     c* in caso contrario tengo la P.IVA che c'era
152050120416     c                   eval      arbcpi=savcpi_a
152060120418     c*
152070120418     c                   if        %subst(§3atb1:1:1)='A'
152080120418     c                   eval      arbksc=savksc_a
152090120418     c                   eval      arbctr=savctr_a
152100120418     c                   endif
152110120404     c                   except    arbsdarb
152120120404    1c                   endif
152130120330     c
152140120330     C                   ENDSR
152150120413     C**************************************************************************
152160120413     C*  AGGIORNAMENTi derivanti da variazioni in sede del ksc di bolla FRANCO
152170120413     C**************************************************************************
152180120413     C     GESSD_ksc     BEGSR
152190120416     C* Controllo se esiste record W di fiar4 per eventuale modifica di mittente
152200120416     C                   MOVEL     'W'           WTRC
152210120416     c                   clear                   wbl4w
152220120416     C     KARB2         CHAIN(N)  Fiar401l
152230120416     c                   if        %found(fiar401l)
152240120416     C                   MOVEL     'S'           WBL4W             1
152250120416     c                   endif
152260120416     c                   z-add     arbksc        oldksc
152270120416     c*
152280120413     c                   eval      arbksc=§btksc
152290120413     c* recupero dati del cliente da anagrafica
152300120413     C                   clear                   TIBS69DS
152310120413     C                   z-add     §btksc        I69kac
152320120413     C                   z-add     §btksc        I69kin
152330120413     C                   call      'TIBS69R'
152340120413     C                   parm                    tibs69DS
152350120413     C                   parm                    ds_cnaco
152360120413     C                   parm                    ds_cnind
152370120413     C                   parm                    ds_cnclp
152380120413     C                   parm                    ds_fncls
152390120413     c                   eval      arbrsm=bs_acorag
152400120413     c                   eval      arbinm=bs_indvia
152410120413     c                   eval      arbLOM=bs_indcit
152420120413     c                   eval      arbCAM=bs_indcae
152430120413     c                   eval      arbPRM=bs_indprv
152440120413     c                   eval      arbNZM=bs_indsta
152450120418     c* P.IVA: non aggiorno arbcpi perchè comune per blp e arb e non va bene:
152460120418     c* se bolla doppia su blp c'è la p.iva del mittente e su arb la p.iva del dest
152470120413     c* dell'aggiornamento di FNARB
152480120419     c                   if        %subst(§3atb1:1:1)='F' and §3atb2=*blanks
152490120419     c                   eval      savcpi_a=bs_indiva
152500120419     c                   endif
152510120419     c                   eval      savcpi_p=bs_indiva
152520120416     c                   exsr      up_fiar4P
152530120416     c                   exsr      up_fiar4w
152540120416     c*
152550120413     c                   endsr
152560120413     C**************************************************************************
152570120413     C* AGGIORNAMENTO RECORD P.IVA IN FIAR4  se esiste
152580120413     C**************************************************************************
152590120413     C     up_fiar4P     BEGSR
152600120413     C                   MOVEL     'P'           WTRC
152610120413     C     KARB2         CHAIN     FiAR401L                           30
152620120413     C     *IN30         IFEQ      *OFF
152630120413     c                   movel     ar4not        wcpifile
152640120413     c                   if        wcpifile <> arbcpi
152650120413     c                   movel     'T'           ar4ftr
152660120413     c                   z-add     dateu         ar4duv
152670120413     c                   z-add     dateu         ar4dtr
152680120413     C                   MOVEl     ARBCPI        AR4NOT
152690120413     C                   UPDATE    FiAR4000
152700120413     c                   else
152710120413     c                   unlock    fiar401l
152720120413     c                   endif
152730120413     C                   END
152740120413     c                   endsr
152750120416     C**************************************************************************
152760120416     C* Gestione record fiar4 record W per DATA TRASMISSIONE VAB
152770120416     C**************************************************************************
152780120416     C     up_fiar4W     BEGSR
152790120416     C* CONTROLLO SE C'E' NUOVO CODICE O VARIATO PER AGGIORNARE O ELIM
152800120416     C*  O SCRIVERE RECORD W IN ar4
152810120416     C                   MOVEL     '3Q'          COD
152820120416     C                   MOVEL(P)  ARBKSC        KEY
152830120416     C     KTAB2         CHAIN     TABEL00F                           30
152840120416     C  N30TBLFLG        IFNE      ' '
152850120416     C                   SETON                                        30
152860120416     C                   ENDIF
152870120416     C*
152880120416     C* C'E' 3Q DEVO CREARE O VARIARE
152890120416    1C  N10*IN30         IFEQ      *OFF
152900120416    2C     WBL4W         IFEQ      ' '
152910120416    3C     ARBKSC        orNE      OLDKSC
152920120416     C                   CLEAR                   DSBL4W
152930120416     C                   CLEAR                   fnlv19ds
152940120416     C                   MOVEL     DSBL4W        d19nt1
152950120416     C                   MOVEL     ARBAAS        d19AAS
152960120416     C                   MOVEL     ARBLNP        d19LNP
152970120416     C                   MOVEL     ARBNRS        d19NRS
152980120416     C                   MOVEL     ARBNSP        d19NSP
152990120416     C                   MOVEL     'W'           d19TRC
153000120416     C                   MOVEL     'T'           d19ftr
153010120416     C                   call      'FNLV19R'
153020120416     c                   parm                    fnlv19ds
153030120416    2C                   ENDIF
153040120416     C*
153050120416   X1C                   ELSE
153060120416     C* SE NO CI DEVE ESERE E PRIMA C'ERA LO DELETO
153070120416    2C     WBL4W         IFEQ      'S'
153080120416     c                   clear                   fnlv19ds
153090120416     C                   clear                   d19nt1
153100120416     c                   clear                   d19nt2
153110120416     C                   MOVEL     arbAAS        d19AAS
153120120416     C                   MOVEL     arbLNP        d19LNP
153130120416     C                   MOVEL     arbNRS        d19NRS
153140120416     C                   MOVEL     arbNSP        d19NSP
153150120416     C                   MOVEL     'W'           d19TRC
153160120416     c                   movel     'T'           d19ftr
153170120416     C                   CALL      'FNLV19R'
153180120416     C                   PARM                    fnlv19ds
153190120416    2C                   ENDIF
153200120416    1C                   ENDIF
153210120416     c                   endsr
153220130321     C**************************************************************************
153230130411     C* Invio Variazione bolla a PDA
153240130321     C**************************************************************************
153250130411     C     sr_pda        BEGSR
153260140908     c* Solo se la chiamata non è cieca:
153270140908     c                   if        d48ffr<>'E'
153280141020     c****               if        d66pda<>' ' and (*in10 or waltrabolla=' ')
153290141020     c                   if        d66pda<>' ' and *in10
153300131227     c                             and wpda_ndc>0 and wpda_dcm=0
153310131227     c* richiamo pgm per verificare che ci sia stata un'effettiva variazione
153320131227     c                   clear                   fnlva1ds
153330131227     c                   eval      IA1AAS=arbaas
153340131227     c                   eval      IA1LNP=arblnp
153350131227     c                   eval      IA1NRS=arbnrs
153360131227     c                   eval      IA1NSP=arbnsp
153370131227     c                   eval      IA1DTV=arbdtv
153380131227     c                   eval      IA1ORV=arborv
153390131227     c                   eval      IA1CVB=arbcvb
153400131227     c                   eval      IA1PRU=arbpru
153410131227     c                   movel     fnlva1ds      kpjbu
153420131227     c                   call      'FNLVA1R'
153430131227     c                   parm                    kpjba
153440131227     c                   movel     kpjbu         fnlva1ds
153450131227     c*
153460131227     c                   if        OA1ESITO='V'
153470140130     c* se filiale partita con nuova gestione telefonate richiamo pgm gestione
153480140130     c* note che si preoccuperà anche del richiamo a fidg42r
153490140130     c                   if        *in87=*off
153500140130     c                   clear                   fnlrn6ds
153510140130     c                   eval      in6mod='I'
153520140130     c                   eval      in6aas=arbaas
153530140130     c                   eval      in6lnp=arblnp
153540140130     c                   eval      in6nrs=arbnrs
153550140130     c                   eval      in6nsp=arbnsp
153560140130     c                   eval      in6pgm='FNLR48R'
153570140130     c                   eval      IN6FGS=arbifp
153580140130     c                   eval      IN6NFV=wpda_ndc
153590140130     c                   eval      IN6DFV=wpda_ddc
153600140130     c                   eval      IN6PDR=arbpdc
153610140130     c                   eval      IN6VAR=d66pda
153620140130     c                   eval      IN6CONS='S'
153630140130     c                   call      'FNLRN6R'
153640140130     C                   parm                    kpjba
153650140130     C                   parm                    fnlrn6ds
153660140130     c                   else
153670140130     c* altrimenti richiamo direttemente fidg42r
153680131227     c
153690130411     c                   z-add     arbifp        w0030             3 0
153700130411     c     w0030         chain     azorg01l
153710130411     c                   if        %found(azorg01l)
153720130411     c                   eval      og148=orgde8
153730131227     c                   if        §ogpdacon<>' '
153740130411     c                   clear                   fidg42ds
153750130411     c                   eval      CO42FGS=arbifp
153760130411     c                   eval      CO42NDC=wpda_ndc
153770130411     c                   eval      CO42DDC=wpda_ddc
153780130411     c                   eval      CO42AAS=arbaas
153790130411     c                   eval      CO42LNP=arblnp
153800130411     c                   eval      CO42NRS=arbnrs
153810130411     c                   eval      CO42NSP=arbnsp
153820130411     c                   eval      CO42CMT='N'
153830130411     c                   movel     fidg42ds      kpjbu
153840130411     c                   call      'FIDG42R'
153850130411     c                   parm                    kpjba
153860130411     c                   endif
153870130411     c                   endif
153880140130     c                   endif
153890131227     c                   endif
153900130411     c                   endif
153910140908     c                   endif
153920130411     c                   endsr
153930141021     C**************************************************************************
153940141021     C* Gestione finestra per richiesta dirottamento
153950141021     C**************************************************************************
153960141021     C     ges_wdw02     BEGSR
153970141021     c                   clear                   winf12
153980141022     c                   eval      atrf21=rimm
153990141125     c                   setoff                                       782879
154000141125     c* iN partenza non si può mai variare senza il dirottamento --> disabilito l'F8
154010141125     c                   if        not *in10
154020141125     c                   seton                                        79
154030141125     c                   endif
154040141022     c                   eval      w02txt='La Linea di Arrivo desunta è diversa-
154050141022     c                              dalla Linea di Arrivo della bolla'
154060141029     c                   exsr      sr_NoAbiDir
154070141021    6c                   do        *hival
154080141021     c                   exfmt     lr48w02
154090141113     c  n78              setoff                                       28
154100141113     c  n78              clear                   w02msg
154110141021     c                   select
154120141021     c* F21=Richiesta dirottamento
154130141021     c                   when      *inkv
154140141021     c                   eval      wdir='1'
154150141021     c* F12=Ritorno
154160141021     c                   when      *inkl
154170141021     c                   eval      winf12='1'
154180141021     c                   endsl
154190141106     c                   if        *inkv or *inkh or *inkl
154200141021     c                   leave
154210141021     c                   endif
154220141021    6c                   enddo
154230141113     c                   setoff                                       7828
154240141021     c                   endsr
154250141029     C**************************************************************************
154260141029     C* Verifica condizioni che disabilitano F21=Dirottamento
154270141029     C**************************************************************************
154280141029     C     sr_NoAbiDir   BEGSR
154290141029
154300141029     c*******************************************
154310141029     c* Casi di disabilitazione del Dirottamento:
154320141029     c*******************************************
154330141029     c* - BOLLA LEGATA
154340141029     c* Se la bolla è legata il dirottamento può essere richiesto solo sull'ultima figlia
154350141029     c     karb          chain     fnlbl02l
154360141029     c                   if        %found(fnlbl02l)
154370141029     c* disattivo l'f21 ed emetto messaggio per avvisare che il dirottamento deve essere
154380141029     c* fatto sulla figlia
154390141029     c                   seton                                        7828
154400141029     c                   eval      w02msg ='Bolla legata:il dirottamento può -
154410141029     c                             essere richiesto solo sulla figlia'
154420141029     c                   clear                   atrf21
154430141029     c                   leavesr
154440141029     c                   endif
154450141029
154460141029     c* - PRESENZA DISPO DA ESAMINARE
154470141029     c**   kidc1         setll     tiidc01l
154480141029     c***                if        %equal(tiidc01l)
154490141029     c                   clear                   fnlry09ds2
154500141029     c                   eval      ILRY09TCH='T'
154510141029     c                   eval      ILRY09AAS=d48aas
154520141029     c                   eval      ILRY09LNP=d48lnp
154530141029     c                   eval      ILRY09NRS=d48nrs
154540141029     c                   eval      ILRY09NSP=d48nsp
154550141029     c                   eval      kpjbu=fnlry09ds2
154560141031     c                   call      'FNLRY09C'
154570141029     c                   parm                    kpjba
154580141029     c                   eval      fnlry09ds2=kpjbu
154590141029     c                   if        OLRY09ESIT='1'
154600141029     c                   seton                                        7828
154610141029     c                   eval      w02msg ='Presenti Disposizioni di Consegna-
154620141029     c                              da elaborare: non è possibile dirottare'
154630141029     c                   clear                   atrf21
154640141029     c                   leavesr
154650141029     c                   endif
154660141029     c
154670141029     c* - PRESENZA FERMO DEPOSITO
154680141029     c                   if        vidffd<>*blanks
154690141029     c                   seton                                        7828
154700141125     c                   if        not *in79
154710141125     c                   eval      w02msg='Presente Fermo Deposito: F8 per non -
154720141029     c                             dirottare, F12 per correggere'
154730141125     c                   else
154740141125     c                   eval      w02msg='Presente Fermo Deposito: F12 per cor-
154750141125     c                             reggere'
154760141125     c                   endif
154770141029     c                   clear                   atrf21
154780141029     c                   endif
154790141112     c* - PRESENZA giacenza aperta
154800141112     c                   if        *in04
154810141112     c                   seton                                        7828
154820141112     c                   eval      w02msg='Presente Giacenza aperta: non è poss-
154830141112     c                             ibile dirottare'
154840141112     c                   clear                   atrf21
154850141112     c                   endif
154860141029     c                   endsr
154870150414     C**************************************************************************
154880150414     C* controlli per cons. particolare "Z"=EXPO
154890150414     C**************************************************************************
154900151119     C*    sr_chkexpo    BEGSR
154910150414     c* Se Expo errore se bolla con contrassegno
154920151119     c*                  if        §3afca=1
154930151119     C*                  MOVEL     MSG(159)      VIDMSG
154940151119     C*                  SETON                                        9028
154950151119     C*                  leavesr
154960151119     c*                  endif
154970150414     c* se expo errore se non è franco
154980151119     c*                  if        %subst(§3atb1:1:1)<>'F'
154990151119     C*                  MOVEL     MSG(160)      VIDMSG
155000151119     C*                  SETON                                        9028
155010151119     C*                  leavesr
155020151119     c*                  endif
155030150414     c* errore se lna non è in tabella
155040151119     c*                  move      v1clna        w003a
155050151119     c*                  if        %lookup(w003a:fil_expo)=0
155060151119     C*                  MOVEL     MSG(161)      VIDMSG
155070151119     c*                  eval      vidmsg=%trim(vidmsg) + ' ' + w003a
155080151119     C*                  SETON                                        9028
155090151119     C*                  leavesr
155100151119     c*                  endif
155110151119     c*                  endsr
155120151016     C*--- Registro peso e volume usati perfatturaz. in FIAR5 --------*
155130151016     C     REGPesivol    BEGSR
155140151016     c                   clear                   dar5fat
155150151016     c                   Eval      kar5trd='FAT'
155160151016     c     kar5          Chain     Fiar531c
155170151016     c                   If        %Found(Fiar531c)
155180151016     c                   Movel     ar5uni        dar5fat
155190151016     c                   endif
155200151016     c* Se tassata
155210151016    1c                   if        ar6imv>0
155220151016     c* Volume
155230170420     c                   movel     arbvlf        §ar5vltas
155240170420     c                   movel     arbfvf        §ar5fvtas
155250170420     c                   if        tasvlt>0  and
155260170420     c                             PesVolIMV=Tassatoimv
155270160530     c                   z-add     tasvlt        §ar5vltas
155280160530     c                   movel     tasfvt        §ar5fvtas
155290170420     c*                  else
155300170420     c*                  movel     arbvlf        §ar5vltas
155310170420     c*                  movel     arbfvf        §ar5fvtas
155320160530     c                   endif
155330151016     c* Peso
155340151016     c                   movel     arbpkf        §ar5pktas
155350151016     c                   movel     'R'           §ar5fptas
155360151016     c
155370151016     c* Se il totale imponibile aggiornato senza arrotondamento
155380151016     c*  è = a quello memorizzato al momento del richiamo al
155390151016     c*   TNSF20R--> memorizzo se usato peso VDL (-tara)
155400151016    2c                   if        PesVolIMV=Tassatoimv  and
155410151016     c                             PesVolspc='S'
155420151016     c                   movel     PesVolPkcn    §ar5pktas
155430151016    3c                   if        arbncp=arbncl
155440151016     c                   movel     'T'           §ar5fptas
155450151016     c                   else
155460151016     c                   movel     'Z'           §ar5fptas
155470151016    3c                   endif
155480151016     c
155490151016    2c                   endif
155500151016     c                   eval      §AR5DATAS=%dec(%date())
155510151016     c                   eval      §AR5HMTAS=%dec(%time())
155520151016     c                   eval      §AR5PRTAS=knmus
155530151016     c                   eval      §AR5NJTAS=knmtd
155540151016     c*
155550151016    1c                   endif
155560151016     c
155570151016      * aggiorno Fiar5 record "FAT"
155580151016if  1c                   If        %Found(Fiar531c)
155590151016     c* Se ho eliminato la tassazione  deleto
155600151016     c                   if        ar6imv=0
155610151016     c                   if        recname='FIAR5000'
155620151016     c                   delete    fiar500s
155630151016     c                   else
155640151016     c                   delete    fiar5p0s
155650151016     c                   endif
155660151016     c                   else
155670151016     c                   Movel(p)  dar5fat       Ar5uni
155680151016     c                   eval      ar5dac=%dec(%date())
155690151016     c                   eval      ar5orc=%dec(%time())
155700151016     c                   if        recname='FIAR5000'
155710151016     c                   Update    Fiar500s
155720151016     c                   else
155730151016     c                   Update    Fiar5p0s
155740151016     c                   endif
155750151016     c                   endif
155760151016      * scrivo   Fiar5 record "FAT"
155770151016   x1c                   Else
155780151016    2c                   if        ar6imv>0
155790151016     c                   Clear                   Fiar500s
155800151016     c                   Clear                   Fiar5p0s
155810151016     c                   Eval      Ar5Aas = ArbAas
155820151016     c                   Eval      Ar5Lnp = ArbLnp
155830151016     c                   Eval      Ar5Nrs = ArbNrs
155840151016     c                   Eval      Ar5Nsp = ArbNsp
155850151016     c                   Eval      Ar5Trd = 'FAT'
155860151016     c                   Movel(p)  dar5fat       Ar5uni
155870151016     c                   eval      ar5dac=%dec(%date())
155880151016     c                   eval      ar5orc=%dec(%time())
155890151016     c                   Eval      AR5FT1='T'
155900151016     c                   Eval      AR5DT1=99999999
155910151016     c                   Eval      AR5FT2='T'
155920151016     c                   Eval      AR5DT2=99999999
155930151016     c                   Eval      AR5FT3='T'
155940151016     c                   Eval      AR5DT3=99999999
155950151016     c* Verifico dove si trova TITAS per la write di fiar5
155960151016     c     karb          chain     titaS30c
155970160104     c                   if        %found(titas30c)
155980151019     c                   if        recnamet='TITAS000'
155990151016     c                   Write     Fiar500s
156000151016     c                   else
156010151016     c                   Write     Fiar5p0s
156020151016     c                   endif
156030160104     c                   else
156040160104     c                   Write     Fiar500s
156050160104     c                   endif
156060151016    2c                   EndIf
156070151016    1c                   EndIf
156080151016     c
156090151016     c                   ENDSR
156100141020     C**************************************************************************
156110141020     C* Scrittura TIIDC per Caricamento Disposizione di dirottamento
156120141020     C**************************************************************************
156130141020     C     sr_tiidc      BEGSR
156140141020      /free
156150141020       clear tiidc000;
156160141020       exsr repnum;
156170141020       if esito = *blanks;
156180141020          IDCPRG=kprg                         ;
156190151201       // Se progressivo non reperito lo lascio a 0:
156200151201       // per le dispo di reso e dirottamento non abbiamo la necessità di
156210151201       // avere il progressivo dal momento che per una stessa spedizione
156220151201       // non è ammessa la presenza di + dispo se una di queste è dirottamento o reso
156230151201       // Abbiamo quindi preferito scrivere a 0 il progressivo piuttosto che perdere la
156240151201       // dispo. Questo discorso non val per le dispo che riceviamo da web
156250151201       //else;
156260151201       //   leavesr;
156270141020       endif;
156280141020       IDCINSDATA   = %int( %subst( %char( %dec( %timestamp() ) )
156290141020                                    : 1 : 14 ) );
156300141020       //IDCtimestn   = ;
156310141020       IDCMODACC='B'                       ;
156320141020       //IDCIP = ;
156330141020       //IDCIDCLI = ;
156340141023       if *in10;
156350141023          IDCTIPOCLI= 'D' ;
156360141023       else;
156370141023          IDCTIPOCLI= 'M' ;
156380141023       endif;
156390141020       IDCWAAS   = arbaas;
156400141020       IDCWLNP   = arblnp;
156410141020       IDCWNRS   = arbnrs;
156420141020       IDCWNSP   = arbnsp;
156430141020       IDCAAS    = arbaas;
156440141020       IDCLNP    = arblnp;
156450141020       IDCNRS    = arbnrs;
156460141020       IDCNSP    = arbnsp;
156470141021       IDCFGS    = lnacons;
156480141020       IDCTIPODIS = '3';
156490141020       //IDCMAILAVV     ;
156500141020       //IDCTELAVV  ;
156510141020       //IDCALERT
156520141021          IDCRSD   = vidrsd;
156530141021          IDCIND   = vidind;
156540141021          IDCCAD   = vidcad;
156550141021          IDCLOD   = vidlod;
156560141021          IDCPRD   = vidprd;
156570141021          IDCNAZ   = vidnzd;
156580141020       // bolla da dirottare
156590141021          IDCNEWLNA=d_o95lna;
156600141021          IDCFLA   ='S';
156610141020       IDCELAFLG='S';
156620141022       IDCELAUTE=knmus;
156630141023       idcref=Vidref;
156640141023       idcteld=vidteld;
156650141020
156660141020         write tiidc000;
156670141020       endsr;
156680141020        //-----------------------------------------------------------------------*
156690141020        // Reperimento progressivo richiesta da numeratore 650
156700141020        //-----------------------------------------------------------------------*
156710141020        begsr RepNum;
156720141020
156730141022          annocur  = %subdt(%date():*years);
156740141020          clear esito;
156750141020          $finenum=*off;
156760141020          clear trul33ds;
156770141020
156780141020          I33TLa = 'L';
156790141020          I33Ope = *ZEROS;
156800141030          I33CNu = 651;
156810141020          I33Num = 1;
156820141020
156830141020          KPJBU = TRUL33DS;
156840141020
156850141020          dow $finenum=*off   ;
156860141020             Trul33R(kpjba);
156870141020
156880141020             TRUL33DS = KPJBU;
156890141020
156900141020             if O33Err <> *zeros;
156910141020               Esito = '2';
156920141020               $finenum=*on;
156930141020             else;
156940141020               w0090=o33nri;
156950141020               kprg=%dec(%subst(%editc(annocur:'X'):3:2):2:0)*1000000000+w0090;
156960141020               setll kprg tiidc02l;
156970141020        // Se numero già presente nel file TIIDC ne cerco un altro
156980141020               if not %equal;
156990141020                  $finenum=*on;
157000141020               endif;
157010141020             endif;
157020141020          enddo;
157030141020
157040141020          endsr;
157050160318        //-----------------------------------------------------------------------*
157060160318        // Aggiornamenti specifici per "VEDI PACCO DPD"
157070160318        //-----------------------------------------------------------------------*
157080160318        begsr sr_VediPDpd ;
157090160318        // Per prima cosa chaino fiar4 record "I" per verificare se si tratta di
157100160318        // "VEDI PACCO"
157110160318             chain(n) (arbaas:arblnp:arbnrs:arbnsp:'I') fiar401l ;
157120160318             if %found(fiar401l);
157130160318                dsbl4i=ar4not;
157140160318              if §b4trd='1';
157150160318        // Se "VEDI PACCO" con giro forzato come da tabella NSD:
157160160318                 chain (arbaas:arblnp:arbnrs:arbnsp) fiarg01l;
157170160318                 if %found(fiarg01l) and argcgi=§nsdcgi;
157180160318        // Memorizzo che, appena aggiornata la bolla, dovrò richiamare il pgm per togliere
157190160318        // la sped. dalla distinta
157200160318        // (Solo se manutenzione fatta in arrivo)
157210160318                  if *in10 and arbndc>0 ;
157220160318                    wtogli='1'      ;
157230160318                  endif;
157240160318        // Cancello il giro forzato
157250160318                    clear argcgi;
157260160318                    clear argtgi;
157270160318                    update fiarg000;
157280160318                 else ;
157290160318                    unlock fiarg01l ;
157300160318                 endif;
157310160318        // Aggiorno il flag Traduttore da 1 a 2
157320160318                 clear  fnlv19ds ;
157330160318                 D19AGG='E';
157340160318                 D19FTR='T';
157350160318                 D19AAS=arbaas;
157360160318                 D19LNP=arblnp;
157370160318                 D19NRS=arbnrs;
157380160318                 D19NSP=arbnsp;
157390160318                 D19TRC='I';
157400160318                 if *in10;
157410160318                    §b4trd='2';
157420160318                 else;
157430160318                    §b4trd='3';
157440160318                 endif;
157450160318                 D19NT1=dsbl4i;
157460160318                 fnlv19r(fnlv19ds);
157470160318              endif;
157480160318             endif;
157490160318        endsr;
157500160318        //-----------------------------------------------------------------------*
157510160318        // Toglie da distinta spedizione "VEDI PACCO DPD"
157520160318        //-----------------------------------------------------------------------*
157530160318        begsr sr_TogliVDpd ;
157540160318                    clear  fnlr98ds;
157550160318                    I98AAS    = arbaas;
157560160318                    I98LNP    = arblnp;
157570160318                    I98NRS    = arbnrs;
157580160318                    I98NSP    = arbnsp;
157590160318                    I98VIDEO  = 'N';
157600160318                    I98npg    = 4  ;
157610160318                    I98fgs    = arbifp ;
157620160318                    I98ndc    = arbndc ;
157630160331                    I98ALCDST = ' ' ;
157640160331                    I98CTLDST = ' ' ;
157650160330                    I98STRCMT = '  ';
157660160318                    kpjbu = fnlr98ds ;
157670160330                    fnlr98c(kpjba);
157680160401        //          if o98esito='1';
157690160401        //              msgforzatxt=O98MSGERR ;
157700160401        //          endif;
157710160318        endsr;
157720141020      /end-free
157730170411        //-----------------------------------------------------------------------*
157740170411        // Memorizzazione dati fattura annullata/stornata
157750170411        //-----------------------------------------------------------------------*
157760170411        begsr sr_Fnfta     ;
157770170411        FTAAAS = ar6aas    ;
157780170411        FTALNP = ar6lnp    ;
157790170411        FTANRS = ar6nrs    ;
157800170411        FTANSP = ar6nsp    ;
157810170411        FTAMGS = arbmgs    ;
157820170411        FTADIV = ar6div    ;
157830170411        FTAALI = ar6ali    ;
157840170411        FTACEI = ar6cei    ;
157850170411        FTAIFT = ar6ift    ;
157860170411        FTAFIV = ar6fiv    ;
157870170411        FTANFT = ar6nft    ;
157880170411        FTADFT = ar6dft    ;
157890170411
157900170505        write(e) fnfta000    ;
157910170411        endsr;
157920170411
157930940321     C*---------------------------------------------------------------*
157940041007     Ofnblp000  E            ARBDblp
157950041007     O                       arbrsd
157960041007     O                       arbind
157970041007     O                       arbcad
157980041007     O                       arblod
157990041007     O                       arbprd
158000041007     O                       arbnzd
158010041007     O                       arbfin
158020041007     O                       arbcpi
158030041007     O                       arbpkb
158040041007     O                       arbpkf
158050041007     O                       arbvlb
158060041007     O                       arbfvb
158070041007     O                       arbvlf
158080041007     O                       arbfvf
158090041007     O                       arbffd
158100041007     O                       arbcts
158110041022     Ofnblp000  E            ARBKblp
158120041022     O                       arbcbo
158130041022     Ofnblp000  E            ARBGblp
158140041022     O                       arbdcr
158150041022     O                       arbhcr
158160041022     O                       arbtcr
158170041022     O                       arbgc1
158180041022     O                       arbgc2
158190041022     O                       arbtc1
158200041022     O                       arbtc2
158210041022     O                       arbft3
158220041025     Ofnblp000  E            ARBMblp
158230041025     O                       arbksc
158240041025     O                       arbrsm
158250041025     O                       arbinm
158260041025     O                       arbcam
158270041025     O                       arblom
158280041025     O                       arbprm
158290041025     O                       arbnzm
158300041025     O                       arbfap
158310041025     O                       arbcpi
158320041025     O                       arbCTR
158330041025     O                       arbfvf
158340041025     O                       arbvlf
158350041027     Ofnblp000  E            ARBTblp
158360041027     O                       arbvas
158370041027     O                       arbias
158380041027     O                       arbvmd
158390041027     O                       arbvad
158400041027     O                       arbqft
158410041027     O                       arbctr
158420041028     Ofnblp000  E            RMblp
158430041028     O                       arbrma
158440041028     O                       arbnas
158450071025     Ofnblp000  E            SoloCPIP
158460071025     O                       arbcpi
158470120404     Ofnblp000  E            ARBsdblp
158480120404     O                       arbpkf
158490120404     O                       arbvlf
158500120404     O                       arbfvf
158510120416     O                       arbksc
158520120416     O                       arbctr
158530120416     O                       arbrsm
158540120416     O                       arbinm
158550120416     O                       arbLOM
158560120416     O                       arbCAM
158570120416     O                       arbPRM
158580120416     O                       arbNZM
158590120416     O                       arbcpi
158600120531     o                       arbias
158610120531     o                       arbvas
158620041007     Ofnarb000  E            ARBDarb
158630041025     O                       arbrsd
158640041007     O                       arbind
158650041007     O                       arbcad
158660041007     O                       arblod
158670041007     O                       arbprd
158680041007     O                       arbnzd
158690041007     O                       arbfin
158700041007     O                       arbcpi
158710041007     O                       arbpkb
158720041007     O                       arbpkf
158730041007     O                       arbvlb
158740041007     O                       arbfvb
158750041007     O                       arbvlf
158760041007     O                       arbfvf
158770041007     O                       arbffd
158780041007     O                       arbcts
158790041022     Ofnarb000  E            ARBKarb
158800041022     O                       arbcbo
158810041025     Ofnarb000  E            ARBGarb
158820041022     O                       arbdcr
158830041022     O                       arbhcr
158840041022     O                       arbtcr
158850041022     O                       arbgc1
158860041022     O                       arbgc2
158870041022     O                       arbtc1
158880041022     O                       arbtc2
158890141223     O                       arbfbc
158900141223     O                       arbcmc
158910041025     Ofnarb000  E            ARBMarb
158920041025     O                       arbksc
158930041025     O                       arbrsm
158940041025     O                       arbinm
158950041025     O                       arbcam
158960041025     O                       arblom
158970041025     O                       arbprm
158980041025     O                       arbnzm
158990041025     O                       arbfap
159000041025     O                       arbcpi
159010041025     O                       arbCTR
159020041025     O                       arbfvf
159030041025     O                       arbvlf
159040041027     Ofnarb000  E            ARBTarb
159050041027     O                       arbvas
159060041027     O                       arbias
159070041027     O                       arbvmd
159080041027     O                       arbvad
159090041027     O                       arbqft
159100041027     O                       arbctr
159110041028     Ofnarb000  E            RMarb
159120041028     O                       arbrma
159130041028     O                       arbnas
159140071025     Ofnarb000  E            SoloCPIA
159150071025     O                       arbcpi
159160120404     Ofnarb000  E            ARBsdarb
159170120404     O                       arbpkf
159180120404     O                       arbvlf
159190120404     O                       arbfvf
159200120416     O                       arbksc
159210120416     O                       arbctr
159220120416     O                       arbrsm
159230120416     O                       arbinm
159240120416     O                       arbLOM
159250120416     O                       arbCAM
159260120416     O                       arbPRM
159270120416     O                       arbNZM
159280120416     O                       arbcpi
159290120531     o                       arbias
159300120531     o                       arbvas
159310120411     ofiar6000  e            AR6SD
159320120411     o                       AR6KSC
159330120411     o                       AR6CTR
159340120411     o                       AR6DIV
159350120411     o                       AR6POR
159360120411     o                       AR6SV1
159370120411     o                       AR6VA1
159380120411     o                       AR6SV2
159390120411     o                       AR6VA2
159400120411     o                       AR6SV3
159410120411     o                       AR6VA3
159420120411     o                       ar6imv
159430120411     o                       ar6cei
159440041020     Ofiqdt000  E            AGGqdt
159450041020     O                       qdtnbnbr
159460930507**         GGG
1594709803091234567
159480050601** cmdt
159490050601OVRDBF FILE(TITA430C) TOFILE(
159500151016OVRDBF FILE(FIAR531C) TOFILE(
159510151016OVRDBF FILE(TITAS30C) TOFILE(
159520941014**
159530941102Spedizione al momento non disponibile: gia' utilizzata da un altro utente     01
159540980406Peso mancante                                                                 02
159550980406Volume mancante                                                               03
159560051129Variazione non eseguibile: bolla allocata da altro utente. Attendi e riprova
159570051129Impossibile inserire un volume inferiore a quello parzialmente rilevato da VDL05
159580941102Data formalmente errata                                                       06
159590941102Data consegna richiesta non puo' essere inferiore della data spedizione       07
159600941102Ora richiesta formalmente errata                                              08
159610941102Se immesso tipo immettere anche ora o data consegna richiesta                 09
159620941102Giorno di chiusura errato                                                     10
159630000510Consegna particolare errata o non utilizzabile                                11
159640941102I giorni di chiusura non possono essere uguali
159650941102Il destinatario non puo' essere sempre chiuso!!!?!!
159660941102Le consegne particolari non possono essere uguali
159670051117Impossibile modificare Cons.Part. " " :immessa da partenza no gestib.in arriv 15
159680941102Codice Bolla errato o non utilizzabile
159690941102Codice bolla esistente e nuovo non possono essere uguali
159700941102Impossibile trasformare una bolla da non recupero in recupero o viceversa
159710941213Impossibile effettuare cambi di porto: utilizzare l'apposito programma
159720941213Il codice bolla non ammette il C/Assegno                                      20
159730941102Il codice bolla vuole il C/Assegno: inserire almeno l'importo
159740941102Impossibile variare i campi relativi al C/Assegno perche' gia' incassato
159750071025Codice Fiscale obbligatorio per cliente ITALIA.  Enter per forzare            23
159760131002Mancano i dati passati per il tipo di aggiornamento richiesto                 24
159770941102Se immessa la valuta immettere anche l'importo o eliminare entrambi           25
159780941102Valuta errata
159790000510Non ammessi importi con decimali per la valuta indicata                       27
159800000510Se modificata la divisa, ridigitare il relativo importo                       28
159810941102Impossibile immettere le particolarita' C/Assegno per bolla senza C/A
159820131008$$Sono ammesse solo spedizioni in  FRANCO  per le  POSTE                        30
159830131008Causale variazione errata per il tipo di dati passati                         31
159840000510Spedizione POSTE : peso superiore a xxxxxx,x kg.  Enter per forzare!          32
159850941103Filiale Cliente inesistente
159860941103Per FATTURA IMMEDIATA impossibile specificare codice di altra filiale
159870941103Codice non ammesso se cliente di altra filiale                                35
159880941103Codice cliente errato o inesistente
159890941103Codice cliente non utilizzabile: bloccato in Piano dei Conti
159900941103Se Segue Fattura impossibile indicare il codice 9999
159910011210Importo da Ass. > del limite imposto xxxxxxxxxx,xxx EUR. TEL.IN SEDE X AUTOR. 39
159920941209Importo da Assicurare = 0 : ENTER per prendere il massimo importo da Tariffa  40
159930941103Impossibile eseguire la tassazione: cliente non dell'area
159940941209Importo da Assicurare obbligatorio, come indicato nella tariffa del cliente   42
159950011205Importo da Assicurare da non immettere, come indicato nella tariffa del cli.  43
159960011205Importo da Assicurare SUPERIORE al mandato esposto in tariffa:ENTER X FORZARE 44
159970011205Valore merce dichiarato obbligatorio                                          45
159980941209Immessa Varia "R" per 8888 o 9999 :obbligatorio l'imp.Assicur.Se 0 da tariffa 46
159990941209Codice cliente di altra filiale: F9 per forzare l'immissione
160000941209Sigla varia inesistente                                                       48
160010941209Varia "G" non ammessa                                                         49
160020941209Varia "G" obbligatoria                                                        50
160030941209Importo varia obbligatorio                                                    51
160040000510Spedizione D.P.D. : non ammesso un peso superiore a xxxxxx,x kg.              52
160050990702Non e' stata trovata la tariffa per la tassazione                             53
160060941212Provvigione a zero: non indicata la percentuale in tariffa                    54
160070941212Errore in tassazione                                                          55
160080011018Se eliminato importo C/A eliminare anche il Tipo Incasso e Particolar.        56
160090071025Partita IVA  obbligatoria per cliente ITALIA.  Enter per forzare              57
160100051129Nuovo volume da FATT. inferiore al volume parziale VDL: sara' sostituito      58
160110950420Non e' possibile modificare il tipo bolla: assegnato gia' incassato           59
160120941222Non e' possibile eliminare i dati della fattura se era un PREPAGATO           60
160130941223Non trovata spedizione                                                        61
160140941223Immesso codice cliente: l'indirizzo sara' ignorato                            62
160150950406Anagrafica mittente non completa in Piano dei Conti                           63
160160950406Manca la provincia in anagrafica mittente: inserirla!!!                       64
160170950407Aggiunto contrassegno: aggiungere anche varia G. Enter per forzatura          65
160180950407Eliminato contrassegno: eliminare anche varia G. Enter per forzatura          66
160190950522Partita IVA obbligatoria se bolla diretta all'estero con attraversam. dogana  67
160200950424Q.ta'da fatturare non inseribile per tariffa a valore:assunto importo da assic68
160210991012Azzerata q.tà da fattur.:azzerare anche gli importi di tassazione             69
160220950426Variato cliente o importo da assic.:azzerare q.tà da fattur. e tot.imponibile.70
160230991012Variato cliente o importo da assic.:azzerare q.tà da fattur. e tassazione     71
160240950426Azzerata q.tà da fattur.e tariffa a valore:azzerare anche il tot. imponibile  72
160250961204Impossibile inserire il C/Assegno in bolle di RESO
160260061024Rif. Mittente obbligatorio se destinatario estero con attraversamento dogana  74
160270970110Natura merce obbligatoria                                                     75
160280970826ATTENZIONE!LArrivo desunta, diversa da bolla ma non verra'variata.ENTER forza
160290970808ATTENZIONE! Inoltro modificato. Enter per continuare                          77
160300980309Impossibile inserire il C/Assegno in questa bolla legata                      78
160310011205Importo da Assicurare non ammesso                                             79
160320011210Importo da Assicurare > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER PER FORZARE  80
160330991112Impossibile fare fattura immediata a cliente di altro network                 81
160340990127Impossibile inserire importo da assicurare:bolla su C.A.in Liquidaz.Transatt. 82
160350990419Aggiunto Imp.Assicur.: aggiungere anche varia R. ENTER per forzare            83
160360990419Eliminato Imp.Assicur.: eliminare anche varia R. ENTER per forzare            84
160370120525Cli 8888/9999:se ImportoAssicurare e TotImponibile InserireVaria"R"
160380990419Modificato Imp.Assicur: controllare varia R . ENTER per forzare               86
160390060221Importo da assicurare INFERIORE a quanto previsto dalla legge                 87
160400990701Per i Prepagati, Numero fattura deve essere maggiore di XXXXXX                88
160410990702Prepagato: premere F14 per tassare o inserire tassazione fino all'imponibile  89
160420991006Per codice cliente 8888 o 9999 senza tariffa è obbligatoria la divisa xxx     90
160430011210Importo C/Assegno SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR              91
160440061107Aggiornamento effettuabile soltanto in PARTENZA su spedizioni in Prepagato
160450011010Ammessi importi con massimo __ decimali per la divisa indicata                93
160460991124Divisa obbligatoria se immesso almeno un importo di tassazione                94
160470061107Aggiornamento effettuabile soltanto in ARRIVO su spedizioni Fattura Immediata 95
160480131008$$Bolla cliente POSTE: non si puo' trasformare in C/Servizio                    96
160490131008$$Bolla cliente POSTE: spese C/A sempre a carico del mittente                   97
160500010412Tipo inoltro "D"=Disagiato non ammesso per questo cap/località                98
160510070508Impostare tipo CONTATTO solo se immesso il Fermo Deposito                     99
160520011011Divisa obbligatoria                                                           00
160530011017Manca la varia 'N'                                                            01
160540011102Divisa scaduta                                                                02
160550011120Spedizione giacente: importo C/A non valido                                   03
160560011210Valore merce dichiarato INFERIORE a xxxxxxxxxx,xxx EUR                        04
160570011210Valore merce dichiarato SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR        05
160580011210Valore merce dichiarato > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE  06
160590011206Gli importi inseriti SUPERANO il limite fatturabile, xxxxxxxxxx,xxx xxx       07
160600011210Importo C/Assegno INFERIORE a xxxxxxxxxx,xxx EUR                              08
160610011210Importo C/Assegno > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE        09
160620011206Importo varia SUPERA il limite fatturabile, xxxxxxxxxx,xxx xxx                10
160630020508Discordanza tra codice bolla e codice cliente mittente
160640020531Destinatario               : immettere la relativa consegna particolare       12
160650051116Non è possibile indicare contemporaneamente cons.particolari 'S' e 'X'        13
160660021121Immettere codice tariffa della relativa cartello FedEx                        14
160670021121Non ammessa tariffa FedEx Documenti: peso superiore a Kg. xxxxxx,x            15
160680021121ATTENZIONE! Bolla con Tar.FedEx Doc.e peso > Kg. xxxxxx,x: ENTER X FORZARE    16
160690021129Immettere consegna particolare "S"                                            17
160700030124Immettere il nominativo                                                       18
160710021129Immettere il n. di telefono della persona contattata                          19
160720030124Immettere il motivo                                                           20
160730051129Peso rilevato totalmente da VDL.  Si può inserire solo il peso = a peso VDL   21
160740030123Almeno una delle consegne particolari deve essere "S"                         22
160750051129Impossibile inserire un peso inferiore a quello parzialmente rilevato da VDL  23
160760030124Il n. dei bancali non ritirati non può superare il n. dei bancali bollettati  24
160770030124Immettere il n. dei bancali non ritirati                                      25
160780031001Non è possibile modificare la consegna richiesta                              26
160790040123Spedizione "monovaria". Non inserire altre varie se non immessa la varia " "  27
160800040330La data consegna richiesta deve essere inferiore alla eseguibilità disp.giac. 28
160810040330Data consegna richiesta superiore a ___ giorni                                29
160820131008Codice cliente o linea cliente non utilizzabile                               30
160830041015Prevista consegna ad Uffici:consegna per Appuntam. o a Supermerc. non ammessa 31
160840110512C/A  SUPERIORE al limite xxxxxxxxxx,xxx EUR non ammesso per bancari BRT       32
160850050414Impossibile trasformare prepagato in C/S                                      33
160860061116Modificata ragione sociale DESTINATARIO: controllare correttezza Cod.FIScale  34
160870050603Non ammessi inserimenti di importo da assicurare: bolla non coperta da mandato35
160880141110Variazione non effettuabile:salvataggio in corso. Riprovare (File "        ") 36
160890051220Non è possibile cambiare il mittente da DPD a non DPD e viceversa             37
160900060131Utente non abilitato ad inserire la consegna particolare " "                  38
160910070312Modifica di DataConsegnaRichiesta POSTICIPATA in chiusura distinta:ENTERavanza
160920070308Data consegna richiesta OBBLIGATORIA insieme al Tipo e/o all'Ora
160930070312Consegna Richiesta non modificabile: giacenza aperta o sped.con blocco "G"    41
160940070330Modifica di DataConsegnaRichiesta POSTICIPATA in chiusura distinta            42
160950070508Impostato Fermo Dep.DA CONTATTARE ma presente ConsegnaRich.in bolla:ENTR forza 43
160960070910Togliere Cons.Particolare X-xxxxxxxx:non immessa in partenza e non da Destinat44
160970071025Immettere codice fiscale o partita IVA o uscire con F12                       45
160980091124Inserimento/aumento ImpAssic x bolla non coperta da mandato:inviare MAIL.Forza46
160990091124Volume rilevato totalmente da VDL. Si può inserire solo il volume = al VDL    47
161000091124Impossibile inserire un volume inferiore a quello parzialmente rilevato da VDL48
161010120508P.Iva "$$" non ammessa per fatture emesse in filiale                          49
161020130930Indirizzo e-mail formalmente errato                                           50
161030131016Eseguita tassazione su bolla contabilizzata:non ammesso F6. F12 per uscire    51
161040131016Non effettuate variazioni: non ammesso F6=Conferma. Premere F12 per uscire    52
161050140408Il Fermo Deposito deve essere autorizzato dal Mittente                        53
161060141029Immessa Data Cons. Richiesta ma presente Fermo Deposito in bolla: ENTER forza 54
161070160204Presenti Dispo di Dirott./Reso ancora da elaborare: Fermo Deposito non ammesso55
161080160204Presente Dispo di Dirott./Reso ancora da elaborare: variazione non ammessa    56
161090141112Presente Giacenza aperta: Enter per Forzare                                   57
161100150210Consegna part. per Appuntamento non valida: immessa data consegna "Tassativa" 58
161110150413Spedizione per EXPO: non ammesso il Contrassegno                              59
161120150414Spedizione per EXPO possibile solo per bolle Franco                           60
161130150414Spedizione per EXPO non ammessa per la linea di arrivo                        61
161140151221Spedizione non manutenzionabile per questa Causale Variazione                 62
161150170130Manca tabella Limiti per Tipo Servizio          - TELEFONARE IN SEDE          63
161160170130ATTENZIONE! Sped."   " di peso > xxxxxx,x Kg. non ammessa                     64
161170170130ATTENZIONE! Sped."   " di volume > xx,xxx mc. non ammessa                     65
