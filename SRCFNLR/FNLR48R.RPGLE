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
000420170922     FFipnd01L  UF   E           K DISK
000430060222     FFiAR401L  UF A E           K DISK
000440051118     FFiAR901L  UF A E           K DISK
000450030612      * BOLLE PARTENZE
000460070320     FFNBLP01L  UF   E           K DISK    prefix(ARB:3)
000470030612      * BOLLE PARTENZE E ARRIVI:TASSAZIONE
000480990930     FFIAR601L  UF A E           K DISK
000490990930     FFIAR701L  UF A E           K DISK
000500130410     F*FIARI01L  IF   E           K DISK
000510160318     FFIARg01L  UF   E           K DISK
000520070308     FFIARP01L  uF a E           K DISK
000530030124      * Estensione bolle
000540030124     fFiar501l  uf a e           k Disk
000550041020     f* Gestione rientri autotrasportatori
000560041020     ffiqdt01l  uf   e           k disk
000570030612      * Conteggi autotrasportatori
000580011126     Ffiftt01L  IF   E           K DISK
000590041008     Ffiftd04L  UF   E           K DISK    RENAME(fiftd000:fiftd004)
000600041008     Ffiftd05L  UF   E           K DISK    RENAME(fiftd000:fiftd005)
000610030612     Ffiftd01L  IF   E           K DISK    RENAME(fiftd000:fiftd001)
000620030612     Ffifst01L  IF   E           K DISK    RENAME(fiftt000:fifst000)
000630041008     Ffifsd04L  UF   E           K DISK    RENAME(fiftd000:fifsd004)
000640041008     Ffifsd05L  UF   E           K DISK    RENAME(fiftd000:fifsd005)
000650030612     Ffifsd01L  IF   E           K DISK    RENAME(fiftd000:fifsd001)
000660030612      *
000670900509     FAZORG01L  IF   E           K DISK
000680910429     FTABEL00F  IF   E           K DISK
000690050331     fTigcp01l  if   e           k Disk
000700050331     FTigcp21L  IF   E           K DISK    RENAME(TIGCP000:TIGCP21)
000710911209     FCNACO00F  IF   E           K DISK
000720941227     FCNIND00F  IF   E           K DISK
000730080701     F****FNDCT01L  IF   E           K DISK
000740990127     FTNTAM01L  IF   E           K DISK
000750990818     FTITPT01L  IF   E           K DISK
000760950208     FFNCLS01L  IF   E           K DISK
000770020911     FFNFVV01L  IF   E           K DISK
000780980105     FFNLBL01L  IF   E           K DISK
000790141022     FFNLBL02L  IF   E           K DISK    rename(fnlbl000:fnlbl02)
000800070309     FFNevb05l  IF   E           K DISK
000810070309     f
000820941014     FFNARBD0F  O  A E             DISK
000830130927     FFNARBN0F  O  A E             DISK
000840990930     FFIARBT0F  O  A E             DISK
000850990930     FFIARBU0F  O  A E             DISK
000860941014     FFNARBK0F  O  A E             DISK
000870941014     FFNARBG0F  O  A E             DISK
000880941227     FFNARBM0F  O  A E             DISK
000890170116     F*FNARBD0T  O  A E             DISK    USROPN  RENAME(FNARBD00:FNARBDTT)
000900170116     F*FIARBT0T  O  A E             DISK    USROPN  RENAME(FIARBT00:FIARBTTT)
000910170116     F*FIARBU0T  O  A E             DISK    USROPN  RENAME(FIARBU00:FIARBUTT)
000920170116     F*FNARBK0T  O  A E             DISK    USROPN  RENAME(FNARBK00:FNARBKTT)
000930170116     F*FNARBM0T  O  A E             DISK    USROPN  RENAME(FNARBM00:FNARBMTT)
000940170116     F*FNARBG0T  O  A E             DISK    USROPN  RENAME(FNARBG00:FNARBGTT)
000950170116     FFNARBD2T  O  A E             DISK    RENAME(FNARBD00:FNARBDTT)
000960170116     FFIARBT2T  O  A E             DISK    RENAME(FIARBT00:FIARBTTT)
000970170116     FFIARBU2T  O  A E             DISK    RENAME(FIARBU00:FIARBUTT)
000980170116     FFNARBK2T  O  A E             DISK    RENAME(FNARBK00:FNARBKTT)
000990170116     FFNARBM2T  O  A E             DISK    RENAME(FNARBM00:FNARBMTT)
001000170116     FFNARBG2T  O  A E             DISK    RENAME(FNARBG00:FNARBGTT)
001010170426     FFNFTA00F  O    E             DISK
001020030722
001030030722     FAZCLN01L  IF   E           K DISK
001040141020     FTIIDC02L  IF A E           K DISK
001050050601      * Bolle sede - Estensione
001060050601     fTita430c  if   e           k Disk    UsrOpn
001070151019     ffiar531c  uf a e           k Disk    UsrOpn infds(infdsar5)
001080151016     f                                     rename(fiar5000:fiar500S)
001090151016     f                                     rename(fiar5p00:fiar5p0S)
001100151019     fTitas30c  if   e           k disk    usropn infds(infdstitas)
001110151016     f                                     prefix(S_)
001120011120
001130160225     d FedD_ksc        s                   like(oc7kscc)
001140160225     d FedD_ctr        s              3
001150160225     d FedM_ksc        s                   like(oc7kscc)
001160160225     d FedM_ctr        s              3
001170160225     d
001180160225     d normlod         s                   like(o95loc)
001190150203     d bollacpi        s                   like(arbcpi)
001200071025     d bollaFGS        s                   like(D48FGS)
001210070319     d sav_arbias      s                   like(vidias)
001220041027     d sav_arbvas      s                   like(vidvas)
001230041027     d sav_arbqft      s                   like(vidqft)
001240041027     d sav_arbvmd      s                   like(vidvmd)
001250041027     d sav_arbvad      s                   like(vidvad)
001260131016     d bol_vidias      s                   like(vidias)
001270131016     d bol_vidvas      s                   like(vidvas)
001280131016     d bol_vidvmd      s                   like(vidvmd)
001290131016     d bol_vidvad      s                   like(vidvad)
001300041027     d sav_arbctr      s                   like(vidctr)
001310020327     d sav_dsftd       s                   like(dsftd)
001320011205     d sav1_vidias     s                   like(vidias)
001330011205     d sav1_vidvmd     s                   like(vidvmd)
001340011205     d sav1_vidcas     s                   like(vidcas)
001350020305     d deseftw         s                   like(§15eft)
001360020305     d desefdw         s                   like(§15efd)
001370020305     d deseffw         s                   like(§15eff)
001380020305     d lnpntw          s                   like(§ogntw)
001390020318     d clintw          s                   like(§ogntw)
001400020305     d lnantw          s                   like(§ogntw)
001410020531     d wdisag          s                   like(ot0dos)
001420070906     d wdisagdest      s                   like(ot0dos)
001430020603     d w48f12          s                   like(d48f12)
001440060301     d w48mct          s                   like(d48mct)
001450020911     d Kfgs            s                   Like(FvvFgs)
001460030612     d kAr5Trd         s                   Like(Ar5Trd)
001470050601     d kta4Trc         s                   Like(ta4trc)
001480021121     d Wctr            s                   Like(VidCtr)
001490040122     d va2va2          s                   Like(Ar6Va1)
001500040122     d va2va3          s                   Like(Ar6Va1)
001510040122     d va2va4          s                   Like(Ar6Va1)
001520041130     d va2va5          s                   Like(Ar6Va1)
001530041130     d va2va6          s                   Like(Ar6Va1)
001540041007     d OLDFFD          s                   Like(Arbffd)
001550041008     d OLDVLC          s                   Like(Arbvlc)
001560050202     d SAVCTR          s                   Like(vidctr)
001570070308     d SAVDCR          s                   Like(comdcr)
001580070308     d SAVTCR          s                   Like(vidtcr)
001590070308     d SAVHCR          s                   Like(vidhcr)
001600070308     d bollaDCR        s                   Like(comdcr)
001610070308     d bollaTCR        s                   Like(vidtcr)
001620070308     d bollaHCR        s                   Like(vidhcr)
001630070308     d bollaFBC        s                   Like(arbfbc)
001640071025     d ar4not_P        s                   Like(ar4not)
001650071025     d sav11pid        s                   Like(v11pid)
001660030612      *
001670030612     d DueCtr          s              2  0
001680070320     d TipoEla         s              1
001690030122     d wI0             s              1
001700030123     d wfpf            s              1
001710070312     d xx              s              3  0
001720090526     d yy              s              3  0
001730030612     d $mbr            s              6
001740030612     d w014a           s             14
001750030612     d wdiffe          s             13  3
001760030612     d w0173           s             17  3
001770030612     d tot_importi     s             17  3
001780100120     d xforzait        s              1
001790100120     d WVariavol       s              1
001800120705     d WVariavolEXP    s              1
001810091106     d Aggiobol        s              1
001820071026     d forzacdf        s              1
001830071025     d forzapid        s              1
001840071025     d forzapid2       s              1
001850071025     d forzaias        s              1
001860080625     d forzaias2       s              1
001870030612     d forzavmd        s              1
001880030612     d forzacas        s              1
001890030709     d flgsf20         s              1
001900030711     d wdataiso        s               d   datfmt(*iso)
001910060201     d wdataLimIas     s               d   datfmt(*iso)
001920030711     d wdataoggi       s               d   datfmt(*iso)
001930030711     d miapsw          s             10
001940030711     d wokpsw          s              1
001950040123     d war72           s              1
001960040122     d contasv         s              2  0
001970040122     d totimv          s                   Like(Ar6Imv)
001980040122     d wesvar1         s              1
001990040122     d wvar3ai         s              1
002000040224     d wffd            s                   Like(ArbFfd)
002010040224     d waggar5         s              1
002020040224     d wCA             s              1
002030040324     d wgiac           s              1
002040040225     d warbdcr         s                   Like(§Ar5gDcr)
002050040316     d wgcfas          s                   Like(GcpFas)
002060040324     d wgcded          s                   Like(GcpDed)
002070040324     d wgcddm          s                   Like(GcpDdm)
002080040608     d wokscr          s              1    inz('0')
002090080416     d wcmitt          s              7
002100080416     d wsflagga_C      s              1
002110110629     d wemd            s              1
002120011205
002130020715     d tbl             s              2    dim(20)                              sk tbl con ass.
002140090526     d ADRs            s              3    dim(50)                              sk tbl con ass.
002150090526     d ADRc            s              3    dim(50)                              sk tbl con ass.
002160170116     D*SOVR            S             48    DIM(1)                               DI COMODO SED
002170170116     D*SALC            S             48    DIM(1)                               DI COMODO SED
002180170116     D*SDLC            S             48    DIM(1)                               DI COMODO SED
002190020805     D PDLC            S             48    DIM(1)                               DI COMODO PAR
002200170116     D*C20             S              1    DIM(48) CTDATA PERRCD(48)
002210170116     D*C21             S              1    DIM(48) CTDATA PERRCD(48)
002220170116     D*C22             S              1    DIM(48) CTDATA PERRCD(48)
002230930507     D GGG             S              1    DIM(8) CTDATA PERRCD(8)              GIORNI CHIUSUR
002240151016     d cmdt            s             60    dim(03) ctdata perrcd(1)
002250170130     D MSG             S             78    DIM(165) CTDATA PERRCD(1)
002260041027     d*cmd             s              1    dim(45) ctdata perrcd(45)            ovrdbf
002270030612
002280990930     D* VARIE DELLA DTASV
002290990930     D* VARIE DELLA DARBT
002300941213     D* VARIE DI COMODO
002310941213     D WSV             S              1    DIM(21)                              SIGLA  VARIE
002320990930     D WVA             S             11  3 DIM(21)                              VALORE VARIE
002330941215     D SKI             S              1    DIM(15)                              COMODO
002340011106     D SKII            S              1    DIM(18)                              COMODO
002350941215     D K78             S              1    DIM(78)                              COMODO
002360990930     D*
002370150119     D SVA             S              1    DIM(100)                             VARIE IN TOT.I
002380070906     d
002390070906     d* Skiere di riesecuzione pgm
002400070906     D kdati           S            250    DIM(10)                              VARIE IN TOT.I
002410070906     D kcvb            S              2    DIM(10)                              VARIE IN TOT.I
002420070907     D ktrc            S              2    DIM(10)                              VARIE IN TOT.I
002430070911     D kffr            S              1    DIM(10)                              VARIE IN TOT.I
002440070911     D kf12            S              1    DIM(10)                              VARIE IN TOT.I
002450150414
002460150414      * filiali abilitate a cons.part "Z"=Expo
002470151119     d*fil_expo        s              3    dim(85)
002480030722
002490030722     D ktfp            S                   LIKE(CLNtfp)
002500030722     D ktfa            S                   LIKE(CLNtfa)
002510030722     D kann            S                   LIKE(CLNann)
002520030722     D kmes            S                   LIKE(CLNmes)
002530050601     d wlib            s             10
002540050601     d commt           s             80
002550050601     d lenght          s             15  5 inz(80)
002560120403     d erra            s              1
002570120403     d errp            s              1
002580120404     d wfatfil         s              2
002590120404     d war6agg         s              1
002600120411     d inx             s              2  0
002610120416     d savcpi_a        s                   like(arbcpi)
002620120418     d savcpi_p        s                   like(arbcpi)
002630120418     d savksc_a        s                   like(arbksc)
002640120418     d savctr_a        s                   like(arbctr)
002650120418     d savksc_p        s                   like(arbksc)
002660120418     d savctr_p        s                   like(arbctr)
002670030722
002680151019     d infdsar5        ds
002690151019     d   recname             261    270
002700151019     d infdstitas      ds
002710151019     d   recnamet            261    270
002720030722     D clnmat          DS
002730030722     D  mat                    1     31    dim(31)
002740030722     D clnpom          DS
002750030722     D  pom                    1     31    dim(31)
002760030722
002770030722     D                 DS
002780030722     D  ds_giorno              7      8  0
002790030722     D  ds_mese                5      6  0
002800030722     D  ds_anno                1      4  0
002810030722     D  ds_data                1      8  0
002820030722
002830941213     D*
002840941213     D* CODICI TARIFFA CON CUI TASSARE GLI ASSEGNATI 9999 - TAB 7W
002850990702     D* TARIFFE PER TASSAZIONE PREPAGATI
002860941222     D*-------------_____------------_______-------------_______-----
002870930505     D                 DS
002880941014     D  ARBAAS                 1      4  0
002890941014     D  ARBMGS                 5      8  0
002900941014     D  ARBDSP                 1      8  0
002910950114     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
002920931210     D PARAM3          DS
002930931210     D  PA3PDR                 1      7  0
002940931210     D  PA3TSR                 8      8
002950931210     D  PA3NDC                 9     15  0
002960931210     D  PA3FCT                16     16
002970931210     D  PA3FVD                17     17
002980931210     D  PA3FVT                18     18
002990950113     D  PA3DDC                19     26  0
003000041122     D  PA3sml               256    256
003010941014     D***
003020941014     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
003030941014     D***
003040941014     D WLBDAT          DS                  INZ
003050941014     D  G02DAT                 1      8  0
003060941014     D  G02INV                 9     16  0
003070941014     D  G02ERR                17     17
003080941014     D  G02TGI                18     22  0
003090941019     D                 DS
003100941019     D  VIDISD                 1      2
003110941019     D  VIDCPD                 3     16
003120941019     D  VIDPID                 1     16
003130071022     D                 DS
003140071022     D  V11ISD                 1      2
003150071022     D  V11CPD                 3     16
003160071022     D  V11PID                 1     16
003170941223     D                 DS
003180941223     D  VIDISM                 1      2
003190941223     D  VIDCPM                 3     16
003200941223     D  VIDPIM                 1     16
003210930210     D                 DS
003220930210     D  EMME                   1      1
003230020805     d  $simfel                2      4  0
003240020805     D  PARMBR                 5      7  0
003250930210     D  MBRFIS                 1     10
003260140130
003270140130      *Schiera per filiali partite con gestione telefonate AUT
003280140130     d Fil816tel       ds           256
003290140130     d  skFil816tel            1    255    dim(85)
003300141029     d TRULVPODS     e ds
003310141029     D  skFilOk               16    765    dim(250)
003320141029
003330141029     D skFilOkdir      s                   like(skFilOk) dim(250) inz
003340140130
003350920211     D DSARBD        E DS
003360930510     D DSARBG        E DS
003370130930     D DSARBN        E DS
003380990930     D DARBT         E DS
003390030612     D  §SV                   44     63    DIM(20)                              SIGLA  VARIE
003400030612     D  §VA                   64    183P 3 DIM(20)                              VALORE VARIE
003410920128     D DSARBK        E DS
003420030612
003430930506     D DSCC          E DS
003440991001     D DS$2          E DS
003450941212     D DSCV          E DS
003460941020     D DSQT1         E DS
003470080416     D DS3K          E DS
003480160915     D DTAS          E DS                  inz
003490060925      ** DS Flag operativi DS DTAS
003500060925     d Dtas01        e ds
003510060925
003520990930     D DTASV         E DS
003530030612     D  VSV                    1     20    DIM(20)                              SIGLA  VARIE
003540030612     D  VVA                   21    140P 3 DIM(20)                              VALORE VARIE
003550030612     D  VES                  141    160    DIM(20)                              ESENZ  VARIE
003560030612                                                                                ESENZ  VARIE
003570030612     D DSBL4W        E DS
003580070320     D DSBL4L        E DS
003590151126     D DSBL4m        E DS
003600170922     D*DSBL4i        E DS
003610170922     D Dpndflo       E DS
003620000623     D DS3IPT        E DS
003630000510     D DS3IDP        E DS
003640000510     D DS1A          E DS
003650930506     D DS1P          E DS
003660060131     D DSSP          E DS
003670941209     D DS15          E DS
003680920117     D DS3A          E DS
003690020715     d dstb          e ds
003700000202     D OG143         E DS
003710070319     D OG146         E DS
003720070306     d Og148         e ds
003730070306     d Og150         e ds
003740000202     D DGEI          E DS
003750991006     D DGED          E DS
003760991028     D DSTA01        E DS
003770930210     D DS7T          E DS
003780021121     d ddfe          e ds
003790030710     d dvpo          e ds
003800071031     d dvpoDECO      e ds
003810160318     d dnsd          e ds
003820050601     d dta4c         e ds
003830151019     D Dar5fat       E DS
003840090604     d ds5e          e ds
003850110906     d FNLVSTDS      e ds
003860130930     d dsemail       e ds
003870130927     d
003880130927     d darbn_ref     e ds
003890130927     d darbn_eml     e ds
003900130930     d darbn_sms     e ds
003910130930     d darbn_not     e ds
003920061107     d
003930151105     d xcfiva1ds     e ds
003940071025     d  ivaeu                  3      4
003950030612      * DS PER YEURCO - EURO CONVERTITORE
003960030612     D YeurcoDS      E DS
003970030612      * DS PER TRUL90R - reperimento stampanti e pgm di stampa
003980130322     D TRUL90DS      E DS
003990030612      * DS PER FNLV13R - CONTROLLO DI UN CAP
004000030612     D Fnlv13DS      E DS
004010030612      * DS PER FNLV14R - CONTROLLO DI INDIRIZZO COMPLETO  SENZA CAP
004020030612     D Fnlv14DS      E DS
004030030612      * DS PER TISI95R - CONTROLLO DI CAP
004040030612     D Tisi95DS      E DS
004050030612      * DS PER TRTB69R - TRTB70R - TRTB72R - TRTB73R - INT E GEST PARTI
004060030612     D DS7pqrs       E DS
004070030612      * DS PER TIBS02R - RICERCA TABELLA SU TNTBE
004080030612     D Tibs02DS      E DS
004090030626      * DS PER STAMPA LDV
004100130321     D*FNLSB5DS      E DS
004110030612      * DS PER FNLV04R - PER CALCOLO TOTALE FATTURA
004120030612     D FNLV04DS      E DS
004130050202      * DS PER FNLV59R - CARICAMENTO E CONTROLLO CODICI TARIFFA
004140050202     D Fnlv59DS      E DS
004150041008      * DS PER FNLV57R - PER aggiornamento files padroncini
004160041008     D FNLV57ds      E DS
004170030612      * DS PER FNLV16R - CONTROLLO TASSAZIONE
004180030612     D Fnlv16DS      E DS
004190040122     d dlv1601       e ds
004200030612      * DS PER FNLV18R - CALCOLO VOLUME AUTOMATICO
004210030612     D Fnlv18DS      E DS
004220060223      * DS PER FNLV19R - SCRITTURA E AGGIORNAMENTO FiAR400F
004230030612     D Fnlv19DS      E DS
004240030612      * DS PER FNLV20R - CALCOLA E SISTENA I VOLUMI SU BOLLA
004250030612     D Fnlv20DS      E DS
004260030612      * DS PER FNLV55R - REPERISCE TERMINAL DI PARTENZA E ARRIVO
004270030612     D Fnlv55DS      E DS
004280050511      * DS PER FNLRa2R - TASSAZIONE PORTI ASSEGNATI
004290050511     D Fnlra2DS      E DS
004300030612      * DS PER FNLR48R - DATI DI ENTRATA
004310030612     D Fnlr48DS      E DS
004320030612      * DS PER FNLR66R - CONTROLLO CAUSALE VARIAZIONE
004330030612     D Fnlr66DS      E DS
004340140130      * DS PER FNLRN6R - note LDV
004350140130     D FnlrN6DS      E DS
004360130321      * DS PER FIDG42R -
004370130321     D Fidg42ds      E DS
004380160318      * DS PER FNLR98R -
004390160318     D Fnlr98ds      E DS
004400141029
004410141029     D fnlry09ds2    E DS
004420150603     d
004430150603     D tnsd99ds      E DS
004440070329     d
004450120413     D TIBS69DS      E DS
004460120413     D ds_cnaco      E DS                  extname(CNACO00F) prefix(BS_)
004470120413     D ds_cnind      E DS                  extname(CNIND00F) prefix(BS_)
004480120413     D ds_cnclp      E DS                  extname(CNCLP00F) prefix(BS_)
004490120413     D ds_fncls      E DS                  extname(FNCLS00F) prefix(BS_)
004500120413
004510070329      * Dati utente
004520060131     d Azuteds       e ds                  extname(Azute00f)
004530060131     d dDatiute      e ds
004540060131     d dute01        e ds
004550060131     D TIBS34DS      E DS
004560060213      * ds x richiamo pgm FNLv85R
004570060213     d Fnlv85ds      e ds
004580060705      * ds x controllo tipo incasso e divisa C/A
004590060705     d TRULTICDS     e ds
004600140515
004610140515     d TRULINTDS     e ds
004620980722      *
004630030612      * DS PER XEDIT - PGM EDITAZIONE
004640011009     D DSEDIT        E DS                  EXTNAME(XEDITDS)
004650011009     D  IEDNUM       E                     EXTFLD(IEDNUMERO)
004660011009     D  IEDNRD       E                     EXTFLD(IEDNRDEC)
004670011009     D  IEDEDT       E                     EXTFLD(IEDEDTCOD)
004680011009     D  IEDLEN       E                     EXTFLD(IEDLENFLD)
004690011009     D  OEDNUM       E                     EXTFLD(OEDNUMAN)
004700011009     D  OEDFLG       E                     EXTFLD(OEDFLGERR)
004710011009     D  OEDCOD       E                     EXTFLD(OEDCODERR)
004720941212      *
004730910429     D KPJBA         E DS
004740930210     D  LIB                   92    101
004750060131     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004760030612     D  DTU                  848    895P 0 DIM(12)    PACKEVEN                  DATE PARAM
004770011205
004780020327     D DSFTD         E DS                  EXTNAME(FIFTD00F)
004790011205     d trul21ds      e ds
004800011205     d trul22ds      e ds
004810011205     d trul23ds      e ds
004820160225     D* DS PER TRUL27R1 - network bolla
004830160225     D trul27ds1     E DS
004840160225     D
004850160225     D trulC7ds      E DS
004860170130     D* DS PER TRUL73R - CONTROLLO LIMITI per tipo servizio
004870170130     D trul73ds      E DS
004880020531     D tisit0ds      E DS
004890131227     D fnlva1ds      E DS
004900041007     d*Trul42Ds      e ds
004910140212     d Trtcp2ds      e ds
004920141021     D TRUL33DS      E DS
004930021129     d Dar5scr       e ds
004940030123     d dAr5Ban       e ds
004950030729     d dAr5Bnb       e ds
004960130924     d dAr5Emd       e ds                  prefix(emd_)
004970031120     d dAr5          e ds
004980031120     d dAr5Gen       e ds
004990040219     d  §Ar5gTcr     e                     Extfld(§Ar5Tcr)
005000040219     d  §Ar5gDcr     e                     Extfld(§Ar5Dcr)
005010040219     d  §Ar5gHcr     e                     Extfld(§Ar5Hcr)
005020040219     d  §Ar5gGc1     e                     Extfld(§Ar5Gc1)
005030040219     d  §Ar5gGc2     e                     Extfld(§Ar5Gc2)
005040040219     d  §Ar5gTc1     e                     Extfld(§Ar5Tc1)
005050040219     d  §Ar5gTc2     e                     Extfld(§Ar5Tc2)
005060070910     d dAr5GenPar    e ds                  extname(dar5gen) prefix(PAR:3)
005070070910     d  PAr5gTcr     e                     Extfld(§Ar5Tcr)
005080070910     d  PAr5gDcr     e                     Extfld(§Ar5Dcr)
005090070910     d  PAr5gHcr     e                     Extfld(§Ar5Hcr)
005100070910     d  PAr5gGc1     e                     Extfld(§Ar5Gc1)
005110070910     d  PAr5gGc2     e                     Extfld(§Ar5Gc2)
005120070910     d  PAr5gTc1     e                     Extfld(§Ar5Tc1)
005130070910     d  PAr5gTc2     e                     Extfld(§Ar5Tc2)
005140030123     d dTba          e ds
005150031028     d dsbl4a        e ds
005160040806
005170070309     d ds2Z          e ds
005180070309     d ds2g2         e ds
005190040806     d ds7q          e ds
005200140408     d ds7r          e ds
005210051109     d dgcpflr       e ds
005220060111     d ddstflo       e ds
005230040809
005240040809      * ds per il controllo della presenza di CA per la spedizione richiesta
005250080701     d****** FIDN12DS      e ds
005260080701     d***  skKey                 26    305    dim(20)
005270080701     d****  skAnn                306    325    dim(20)
005280080701     d****  skDca                326    485  0 dim(20)
005290080701     d****  skFca                486    545  0 dim(20)
005300080701     d****  skDch                546    705  0 dim(20)
005310080701     d****  skCch                706    745    dim(20)
005320040809     d*
005330040809     d dsKey           ds
005340040809     d  dsaac                         4  0
005350040809     d  dsfil                         3  0
005360040809     d  dsnca                         7  0
005370050531     D PARAMT          DS
005380050531     D  PARCA                256    256
005390011205
005400950104     D*
005410950104     D* DEFINIZIONE COSTANTI
005420950104     D*
005430950104     D CPDC            C                   CONST('su Piano dei Conti')
005440950114     D CNOTAR          C                   CONST('NO TARIFFE')
005450061107     D Cdest           C                   CONST('D E S T I N A T A R I O')
005460061107     D CMITT           C                   CONST('--- M I T T E N T E ---')
005470141022     D* CAMPO PER DSPATR "REVERSE IMAGE"        DEI CAMPI A VIDEO
005480141022     D RIMM            C                   CONST(X'21')
005490011205
005500011205     d                sds
005510011205     d  nompgm                 1     10
005520030620
005530030620     d sav_indsin      s                   Like(indsin)
005540041008     d waltrocpi       s                   Like(arbcpi)
005550060223     d wcpifile        s                   Like(arbcpi)
005560041008     d waltrofvf       s                   Like(arbfvf)
005570041008     d waltrovlf       s                   Like(arbvlf)
005580041022     d waltrotc1       s                   Like(arbtc1)
005590041022     d waltrotc2       s                   Like(arbtc2)
005600080422     d waltroft3       s                   Like(arbft3)
005610041027     d newcpi          s                   Like(arbcpi)
005620051114     d wtc1            s                   Like(arbtc1)
005630051114     d wtc2            s                   Like(arbtc2)
005640051220     d memntw          s                   Like(§ogntw)
005650060111     d d_o95lna        s                   like(o95lna)
005660060316     d s_arbrsd        s                   like(arbrsd)
005670060316     d s_arbrd2        s                   like(arbrd2)
005680061107     d wcliie          s              1
005690061107     d wprv            s                   like(arbprd)
005700151105     d wcap            s                   like(arbcad)
005710151105     d wloc            s                   like(arblod)
005720061107     d Esiste          s              1
005730091217     d Wtipoagg        s              3
005740110906     d Lnacons         s              3  0
005750061107     d wnar            s                   Like(arbnzd)
005760130411     d wpda_ndc        s                   Like(arbndc)
005770130411     d wpda_ddc        s                   Like(arbddc)
005780130411     d wpda_dcm        s                   Like(arbdcm)
005790131016     d winf14          s              1
005800131120
005810131120     d sav_ift         s                   Like(ar6ift)
005820140403     d comgf2          s                   Like(o95gf2)
005830140407     d wpk2            s                   Like(§AR5PK2)
005840141020     d wdir            s              1
005850141021     d $Finenum        s               n   inz(*off)
005860141021     d wfgslna         s                   like(d55tfa)
005870141021     D Esito           s              1a
005880141031     d w0090           s              9  0
005890141021     d kprg            s                   like(idcprg)
005900141021     d winf12          s              1
005910141022     d annocur         s              4s 0
005920141029     d w0140i          s             14s 0 inz
005930141203     d savzng          s                   like(vidzng)
005940141212     d wnoindir        s              1
005950150119     d w_noapp         s              1
005960151016     d PesVolpkcn      s                   Like(Taspkcn)
005970151016     d PesVolSpc       s                   Like(TasSpc)
005980151016     d PesVolIMV       s                   Like(Ar6IMV)
005990151019     d Tassatoimv      s                   Like(Ar6IMV)
006000160112     d wfile           s              1
006010160318     d wtogli          s              1
006020170130     d wmsgtsp         s             78
006030170130     d wpes_pre        s                   like(vidpkf)
006040170130     d wvol_pre        s                   like(vidvlf)
006050170131     d wtsp03          s                   like(§5ed03)
006060141021
006070141021     D trul33R         pr                  extpgm('TRUL33R')
006080141021     D  kpjba                              likeds(kpjba)
006090170922     D*fnlv19r         pr                  extpgm('FNLV19R')
006100170922     D* kpjba                              likeds(fnlv19ds)
006110160330     D fnlr98c         pr                  extpgm('FNLR98C')
006120160318     D  kpjba                              likeds(kpjba)
006130070515     i*
006140070515     IFNBLP000
006150070515     I              BLPFST                      ARBFSTDDT
006160910430     C****************************************************************
006170910430     C*  RIEPILOGO INDICATORI
006180910430     C***************************************************************
006190920128     C* 01    - VARIAZIONE DESTINATARIO
006200941209     C* 02    - VARIAZIONE PESO / VOLUME
006210930510     C* 03    - PROGRAMMA RICHIAMATO  DSARBG
006220930505     C* 04    - SPEDIZIONE CON GIACENZA APERTA
006230941222     C* 05    - INDICA SE LA 1 BOLLA O LA 2 E' UN ASSEGNATO X P.IVA
006240941222     C* 06    - 1 BOLLA IN FRANCO
006250991006     C* 07    - PROTEZIONE DIVISA PER PREPAGATI E FATTURE IMMEDIATE
006260991012     C* 08    - PREPAGATO
006270941213     C* 09    - SONO STATE MESSE DELLE VARIE NELLA TASSAZIONE
006280941222     C* 10    - ON -BOLLE IN ARRIVO OFF-BOLLE IN PARTENZA
006290941102     C* 11/12 - PROTEGGO LE CONSEGNE PARTICOLARI SE IMMESSE IN PART E
006300941102     C*         NON UTILIZZABILI IN ARRIVO
006310941102     C* 13    - VARIAZIONE TASSAZIONE S.F.
006320911210     C* 14    - VARIAZIONE TASSAZIONE FATTURA
006330941102     C* 15    - RITASSAZIONE
006340950120     C* 16    - PER CAUSALE CP PROTEGGO CAMPI DELLA CONSEGNA RICH/GG CH
006350940614     C* 17 OFF- FILE TASSAZIONE PADRONCINI
006360940614     C* 17 ON - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
006370911211     C* 18    - NON ESISTONO TARIFFE PER IL CODICE DIGITATO
006380920128     C* 19    - PROGRAMMA RICHIAMATO  DSAR2T
006390920128     C* 20    - PROGRAMMA RICHIAMATO  GENERICO
006400990701     C* 21    - CAUSALE VARIAZIONE MODIFICA CONTRASSEGNO
006410990701     C* 22    - UATO PER PROTEZIONE MITTENTE
006420990701     C* 23    - DATI FATTURA NON MODIFICABILI PER PREPAGATO AUTOMAT
006430950113     C* 24    - ESEGUITA LA VARIAZIONE DEL MITTENTE
006440920128     C* 25    - PROGRAMMA RICHIAMATO  DSARBT
006450950607     C* 26    - ABILITA L'ENTER PER L'AGGIORNAMENTO
006460950607     C* 27    - ABILITA L'F12 PER RITORNO AL PGM CHIAMANTE
006470941212     C* 28    - EMETTO VIDMSG
006480950616     C* 29    - PROTEZIONE CAMPI DEL VIDEO
006490980513     C* 30-33 - COMODO
006500920211     C* 34    - PROGRAMMA RICHIAMATO  DSARBD
006510920129     C* 35    - PROGRAMMA RICHIAMATO  DSARBK
006520920129     C* 36    - VARIA TASSAZIONE FRANCHI: SALTO I CONTROLLI SUL COD.CLI
006530920204     C* 37    - VARIATO TOTALE IMPONIBILE: EFFETTUO RISTAMPA FATTURA
006540941014     C* 38    - CREO RECORD IN FNARBM0T
006550941014     C* 39    - CREO RECORD IN FNARBD0T-CAUS.K5 K8- PER AVERE DESTINAT
006560930507     C* 40-67 - ERRORE
006570030729      * 70    - Visualizzo Colli originali
006580991001     C* 71-72 - PROTEGGO CAMPI DESTINATARIO SE 9999 GIA' CONTABILIZZATO
006590990415     C* 73    - SPROTEGGO IL CAMPO DELL'IMPORTO D'ASSICURARE E DEL VALORE
006600990415     C*         MERCE DICHIARATO
006610060201     C* 74    - protezione campi tassazione
006620060201     C* 75    - protezione campi importo da assicurare
006630140213     C* 77    - condizionamento campo "fd da non contattare"
006640131001     C* 81    - della DSARBN  passato campo uni70 - note
006650990415     C* 82    - ERRORE NON BLOCCANTE PER ELIMINAZIONE O AGGIUNTA IMP.ASSICURAZ.
006660131001     C* 83    - della DSARBN  passato campo cel
006670131001     C* 84    - della DSARBN  passato campo ref/tel
006680131001     C* 85    - SFPDSP
006690131001     C* 86    - della DSARBN  passato campo uni70 - email
006700071022     C* 87    - tasto funzionale"F17-Note ldv"attivo
006710070329     C* 88    - PROTEGGO PESO   SE NON VARIABILE
006720941216     C* 89    - PROTEGGO VOLUME SE NON VARIABILE
006730920220     C* 90    - ERRORE GENERICO
006740911213     C* 91    - ERRORE NON POSSIBILE ALLOCARE FILE DI INVIO
006750950407     C* 92    - ERRORE NON BLOCCANTE PER ELIMINAZIONE O AGGIUNTA C/ASS.
006760941215     C* 93    - INDICATORE DI CHANGE SU VALORE MERCE DICH
006770941215     C* 94    - RI SU VALUTA VAL.MERCE SE PRESA IN AUTOMATICO DA NAZ
006780941102     C* 95    - INDICATORE DI CHANGE SU C/ASSEGNO
006790941215     C* 96    - RI SU VALUTA C/ASSEGNO SE PRESA IN AUTOMATICO DA NAZ
006800941215     C* 97    - INDICATORE DI CHANGE SU IMPORTO DA ASSICURARE
006810941215     C* 98    - RI SU VALUTA IMP DA ASSIC SE PRESA IN AUTOMATICO DA NAZ
006820910430     C*****************************************************************
006830000000     C     *ENTRY        PLIST
006840000000     C                   PARM                    KPJBA
006850920211     C                   PARM                    DSARBD
006860920128     C                   PARM                    DSARBK
006870990930     C                   PARM                    DARBT
006880930510     C                   PARM                    DSARBG
006890030612     C                   PARM                    TRUL90DS
006900130930     C                   PARM                    DSARBn
006910050601     c*
006920050110     c*
006930050110     c                   setoff                                       kl
006940030612      *
006950030612      * Il numero dei parametri ricevuti è testato, in caso di modifica
006960030612      *  ricercare la stringa "%PARMS"
006970030612      *
006980030612     C                   MOVEL     KPJBU         Fnlr48DS
006990120426     C*
007000120426     C                   EXSR      OPN_TITA4
007010950104     C* ESEGUO LA VARIAZIONE
007020950104     C                   MOVEL     'E'           WESEG             1
007030020603     c                   movel     d48f12        w48f12
007040060301     c                   movel     d48mct        w48mct
007050950114     C* SE NON  RICHIAMATO IMPOSTO SEMPRE LA FERMATA NELLE VIDEATE
007060950114     C     D48TRC        IFEQ      *BLANKS
007070950609     C                   MOVEL     'V'           D48FFR
007080950114     C                   ENDIF
007090070907     c* skiere di riesecuzione automatica pgm: max 10 volte
007100070907     c                   clear                   kdati
007110070907     c                   clear                   kcvb
007120070907     c                   clear                   ktrc
007130070911     c                   clear                   kffr
007140070911     c                   clear                   kf12
007150070907     c                   clear                   kK                2 0
007160070907     c                   z-add     1             rr                2 0
007170941014     C*
007180910429     C*---------------------------------------------------------------*
007190041006     C* TIPO LANCIO, D48TLA "C" -           CHIUSO CON %R
007200941209     C* TIPO LANCIO, D48TLA "L" - ELABORO E CHIUDO CON LR
007210941209     C* TIPO LANCIO, D48TLA " " - ELABORO E CHIUDO IN RETRN
007220941209     C*
007230941209    1C     D48TLA        IFNE      'C'
007240001213     C*
007250001213    2C     D48FFR        IFNE      'E'
007260141017    2C     D48FFR        andne     'F'
007270001213    3C     OPN48         IFEQ      '0'
007280001213     C                   OPEN      FNLR48D
007290001213     C                   MOVE      '1'           OPN48
007300001213    3C                   ENDIF
007310001213   X2C                   ELSE
007320001213    3C     OPN48         IFEQ      '1'
007330001213     C                   CLOSE     FNLR48D
007340001213     C                   MOVE      '0'           OPN48
007350001213    3C                   ENDIF
007360001213    2C                   ENDIF
007370040224
007380040224     c                   Clear                   wCa
007390040324     c                   Clear                   wgiac
007400941209     C*
007410950104     C* FINTANTO CHE SI VUOLE SI PUO' RIMANRE DENTRO ALLE VARIAZIONI
007420950104   1AC     WESEG         DOWEQ     'E'
007430950104     C                   CLEAR                   WESEG
007440920123     C*
007450950104     C* CONTROLLI BOLLA CON CAUSALE VARIAZIONE
007460900509     C                   EXSR      CONTR
007470941014     C*
007480941014     C   90              GOTO      FINE
007490911209     C*
007500911210     C* CARICO BOLLA
007510120403     c                   if        flgcvr<>'S'
007520911210     C                   EXSR      CARICA
007530131002     c                   if        d48err='1'
007540131002     C                   GOTO      FINE
007550120403     c                   endif
007560131002     c                   endif
007570911209     C**
007580941019     C** VARIAZIONE: DESTINATARIO PESO VOLUME
007590911209     C**
007600941220     C                   SELECT
007610120330     c* Variazioni da manutenzione bolle di sede
007620120330     C     FLGCVR        WHENEQ    'S'
007630120427     c* Questa routine la posso eseguire solo se pgm richiamato
007640120427     c* Hanno fatto una manutenzione con CVB SD/SF da menù in quanto
007650120427     c* SD e SF non erano stati disabilitati per l'utilizzo da bolle par
007660120427     c* e bolle arrivi. In questo modo essendo azzerava peso,volume, mittente..
007670120427     c                   if        d48trc<>*blanks
007680120330     C                   EXSR      gesSD
007690120427     c                   endif
007700120330
007710941220     C     FLGCVR        WHENEQ    'I'
007720920120     C                   EXSR      EMFOR2
007730911210     C**
007740911211     C** VARIAZIONE TASSAZIONE 1° BOLLA
007750911210     C**
007760941220     C     FLGCVR        WHENEQ    'T'
007770920121     C                   EXSR      EMFOR3
007780911211     C**
007790911211     C** VARIAZIONE TASSAZIONE 2° BOLLA
007800911211     C**
007810941220     C     FLGCVR        WHENEQ    '2'
007820941102     C                   EXSR      EMFOR4
007830920116     C**
007840920116     C** VARIAZIONE: CODICE BOLLA CONTRASSEGNO
007850920116     C**
007860941220     C     FLGCVR        WHENEQ    'K'
007870941102     C                   EXSR      EMFOR5
007880920320     C**
007890920320     C** VARIAZIONE CONSEGNA RICHIESTA
007900920320     C**
007910941220     C     FLGCVR        WHENEQ    'C'
007920021129     c     D48Cvb        AndNe     'CS'
007930941102     C                   EXSR      EMFOR7
007940021129     C**
007950021129     C** VARIAZIONE CONSEGNA RICHIESTA SUPERMERCATI
007960021129     C**
007970021129     c     D48Cvb        WhenEq    'CS'
007980021129     c                   ExSr      Emfor71
007990941223     C**
008000941223     C** VARIAZIONE MITTENTE
008010941223     C**
008020941223     C     FLGCVR        WHENEQ    'M'
008030941223     C                   EXSR      EMFOR8
008040961211     C**
008050130927     C** VARIAZIONE DATI MITTENTE
008060961211     C**
008070961211     C     D48CVB        WHENEQ    'RM'
008080961211     C                   EXSR      EMFOR9
008090030123      **
008100030123      ** Variazione mancato ritiro BANCALI
008110030123      **
008120030123     c     D48Cvb        WhenEq    'BR'
008130030123     c                   ExSr      Emfor10
008140961211     C*
008150061107      **
008160061107      ** Variazione CODICE FISCALE
008170061107      **
008180061107     c     D48Cvb        WhenEq    'FI'
008190061107     c                   ExSr      Emfor11
008200130927      **
008210130927      ** Variazione dati destinatario SMS/mail/referente
008220130927      **
008230130927     C     FLGCVR        WHENEQ    'N'
008240130927     c                   ExSr      Emfor12
008250061107     C*
008260941220     C                   ENDSL
008270070907     c
008280070907     c* Verifico se ho delle riesecuzioni da fare
008290070907     c                   if        RR<=10 and kcvb(RR)<>*blanks
008300070907     c                   eval      weseg='E'
008310070907     c                   eval      d48cvb=kcvb(RR)
008320070907     c                   eval      d48trc=ktrc(RR)
008330070911     c                   eval      d48ffr=kffr(RR)
008340070911     c                   eval      w48f12=kf12(RR)
008350070907     c                   select
008360070907     c                   when      d48trc='D'
008370070907     c                   movel     kdati(RR)     dsarbd
008380070907     c                   when      d48trc='G'
008390070907     c                   movel     kdati(RR)     dsarbg
008400070907     c                   when      d48trc='K'
008410070907     c                   movel     kdati(RR)     dsarbK
008420070907     c                   when      d48trc='T'
008430070907     c                   movel     kdati(RR)     darbt
008440070907     c                   when      d48trc='2'
008450070907     c                   movel     kdati(RR)     darbt
008460070907     c                   ENDSL
008470070907     c
008480070907     c                   add       1             RR
008490070907     c                   endif
008500941209     C*
008510950104   1AC                   ENDDO
008520950104    1C                   ENDIF
008530920121     C*
008540000000     C     FINE          TAG
008550941209     C* CHIUSURA PGM
008560060302     C******             MOVEL     Fnlr48DS      KPJBU
008570950114     C*
008580950114     C* PULISCO IL CAMPO CHER AVEVO IMPOSTATO CON LA CONFERMA DEL MITT
008590950114     C*  CHE DEVE RIMANERE IMPOSTATO FINTANTO CHE SI E' DENTRO AL PGM
008600950114     C                   CLEAR                   WRICVL
008610950114     C                   CLEAR                   MITFVF
008620950114     C                   CLEAR                   MITVLF
008630950114     C                   CLEAR                   MITCTR
008640020722      * PULISCO IL CAMPO CHE AVEVO IMPOSTATO PER IL DESTINATARIO
008650020722      *  DISAGIATO
008660020722     C                   CLEAR                   Wdisag
008670060302     c*
008680060302     c                   Clear                   Fnlv85ds
008690060302     c                   Eval      Ilv85Tla = 'C'
008700060302     c                   movel     fnlv85ds      kpjbu
008710060302     c                   Call      'FNLV85R'
008720060302     c                   Parm                    kpjba
008730060302     c*
008740950114     C* CHIUSURA PGM
008750941216    1C     D48TLA        IFEQ      ' '
008760130417     C                   MOVEL     Fnlr48DS      KPJBU
008770941209     C                   RETURN
008780941209     C                   ELSE
008790941209     C*
008800930510     C* SE APERTI FILE CHIUDO
008810001213     C*
008820001213     C     OPN48         IFEQ      '1'
008830001213     C                   CLOSE     FNLR48D
008840001213     C                   ENDIF
008850001213     C*
008860030612     C                   CLEAR                   FNLV04DS
008870941209     C                   MOVEL     'C'           D04TLA
008880990930     C                   CALL      'FNLV04R'
008890030612     C                   PARM                    FNLV04DS
008900990930     C                   PARM                    DTASV
008910970808     C*
008920970808     C                   MOVEL     'C'           I13TLA
008930970808     C                   CALL      'FNLV13R'
008940970808     C                   PARM                    KPJBA
008950030612     C                   PARM                    Fnlv13DS
008960030612     C                   PARM                    Tisi95DS
008970030612     C                   CLEAR                   Tisi95DS
008980971105     C**
008990971105     C                   MOVEL     'C'           I95TLA
009000971105     C                   CALL      'TISI95R'
009010030612     C                   PARM                    Tisi95DS
009020970808     C*
009030970808     C                   MOVEL     'C'           I14TLA
009040970808     C                   CALL      'FNLV14R'
009050970808     C                   PARM                    KPJBA
009060030612     C                   PARM                    Fnlv14DS
009070971114     C*
009080030612     C                   CLEAR                   Fnlv55DS
009090971114     C                   MOVEL     'C'           D55TLA
009100971114     C                   CALL      'FNLV55R'
009110030612     C                   PARM                    Fnlv55DS
009120050202     c
009130050202     C                   CLEAR                   Fnlv59DS
009140050202     C                   MOVEL     'C'           ilv59tla
009150050202     C                   CALL      'FNLV59R'
009160050202     C                   PARM                    Fnlv59DS
009170020305     c*
009180160225     C                   CLEAR                   trul27ds1
009190020305     C                   MOVEL     'C'           I27TLA
009200160225     C                   CALL      'TRUL27R1'
009210160225     C                   PARM                    TRUL27DS1
009220160225     c
009230060705     C                   CLEAR                   trulTICds
009240060705     C                   MOVEL     'C'           IticTLA
009250060705     C                   CALL      'TRULTICR'
009260060705     C                   PARM                    TRULTICDS
009270060705     C                   PARM                    DS1A
009280140515
009290140515     C                   CLEAR                   trulintds
009300140515     C                   MOVEL     'C'           IintTLA
009310140515     C                   CALL      'TRULINTR'
009320140515     C                   PARM                    TRULintds
009330110906     c
009340110906     C                   CLEAR                   fnlvstds
009350110906     C                   MOVEL     'C'           ILVSTTLA
009360110906     C                   CALL      'FNLVSTR'
009370110906     c                   parm                    kpjba
009380110906     C                   PARM                    fnlvstds
009390941220     C*
009400120426     C                   IF        %OPEN(TITA430C)
009410050601     c                   close     tita430c
009420120426     C                   ENDIF
009430151016     C*
009440151016     C                   IF        %OPEN(FIAR531C)
009450151016     c                   close     fiar531c
009460151016     C                   ENDIF
009470151016     C*
009480151016     C                   IF        %OPEN(TITAS30C)
009490151016     c                   close     titas30c
009500151016     C                   ENDIF
009510130321
009520130411     c                   clear                   fidg42ds
009530130411     C                   MOVEL     'C'           co42tla
009540130411     c                   movel     fidg42ds      kpjbu
009550130411     c                   call      'FIDG42R'
009560130411     c                   parm                    kpjba
009570131227
009580131227     c                   clear                   fnlva1ds
009590131227     C                   MOVEL     'C'           ia1tla
009600131227     c                   movel     fnlva1ds      kpjbu
009610131227     c                   call      'FNLVA1R'
009620131227     c                   parm                    kpjba
009630141029
009640141029     c                   eval      ilry09tla= 'C'
009650141029     c                   eval      kpjbu=fnlry09ds2
009660141031     c                   call      'FNLRY09C'
009670141029     c                   parm                    kpjba
009680160318
009690160318     c                   eval      d19tla  =  'C'
009700160318     c                   call      'FNLV19R'
009710160318     c                   parm                    fnlv19ds
009720130321
009730130417     C                   MOVEL     Fnlr48DS      KPJBU
009740041006     C                   SETON                                        LR
009750941209     C                   END
009760941220     C*
009770941209     C* IMPOSTAZIONI INIZIALI -------------------------------------*
009780941209     C     *INZSR        BEGSR
009790941212     C* MI SALVO LA KPJBU
009800941212     C                   MOVEL     KPJBU         W256A           256
009810060131      *
009820060131     c     *dtaara       define    §azute        azuteds
009830060131     c     *dtaara       define    §datiute      ddatiute
009840060131     c                   in(E)     *dtaara
009850060131    1c                   If        %error  or RSUT = *blanks
009860060131     c                   Clear                   Tibs34ds
009870060131     c                   Call      'TIBS34R'
009880060131     c                   Parm                    Tibs34ds
009890060131     c                   In        *dtaara
009900060131    1c                   EndIf
009910060131
009920060131      * se ho errori nei dati utente esco dal pgm
009930060131w   1c                   if        DutErr = 'E'
009940060131     c                   GoTo      Fine
009950060131x   1c                   else
009960060131     c                   Movel     UteFaf        Dute01
009970060131e   1c                   endif
009980941212     C*
009990941209     C                   Z-ADD     1             CODUT
010000060131     C                   CALL      'X§PARUT'
010010060131     C                   PARM                    UT§DSE
010020060131     C                   MOVEL     rsut          VIDDSA
010030000104     C**
010040000104     C                   MOVEL     DTU(10)       W0020             2 0
010050000104     C                   Z-ADD     DTU(10)       WDTU10
010060000104     C     W0020         IFGE      60
010070000104     C                   MOVEL     19            WDTU10
010080000104     C                   ELSE
010090000104     C                   MOVEL     20            WDTU10            8 0
010100000104     C                   ENDIF
010110941014     C*
010120020805
010130020805     c                   eval      $simfel = simfel
010140020805     c                   movel     simfel        $mbr
010150070329     c
010160070329     c* Per EDP note sempre attive, se lna ha geo attiva
010170070329     c                   if        %subst(knmus:1:3)='EDP'
010180070329     c                   eval      dutgeot='S'
010190070329     c                   else
010200070329     c     dutpou        chain     azorg01l
010210070329     c                   if        %found(azorg01l)
010220070329     c                   movel     orgde8        og148
010230070329     c                   else
010240070329     c                   clear                   og148
010250070329     c                   endif
010260070329     c                   movel     §oggeot       dutgeot           1
010270070329     c                   endif
010280941020     C***
010290941020     C* CARICO CAMPI FISSI PER TASSAZIONE/BOLLETTAZIONE
010300941020     C***
010310941020     C                   MOVEL     'QT'          COD
010320941020     C                   MOVEL     '1       '    KEY
010330941212     C                   EXSR      CTABEL
010340941020     C  N30              MOVEL     TBLUNI        DSQT1
010350941020     C   30              CLEAR                   DSQT1
010360941214     C***
010370941214     C* PRENDO DALLA TABELLA DEI VOLUMI IL FLAG DI SOSTITUZIONE PER"Z"
010380941214     C***
010390941214     C                   MOVEL     '7T'          COD
010400941214     C                   MOVEL(P)  'Z'           KEY
010410941220     C                   EXSR      CTABEL
010420941214     C   30              CLEAR                   DS7T
010430941214     C  N30              MOVEL     TBLUNI        DS7T
010440941214     C*
010450970901     C* FLAG DI SOSTITUZIONE
010460941214     C                   MOVEL     §7TAFA        ZAFA              1
010470941214     C                   MOVEL     §7TAAA        ZAAA              1
010480941214     C                   MOVEL     §7TAFN        ZAFN              1
010490941214     C                   MOVEL     §7TAAN        ZAAN              1
010500970418     C***
010510970418     C* PRENDO DALLA TABELLA DEI VOLUMI IL FLAG DI SOSTITUZIONE PER"T"
010520970418     C***
010530970418     C                   MOVEL     '7T'          COD
010540970418     C                   MOVEL(P)  'T'           KEY
010550970418     C                   EXSR      CTABEL
010560970418     C   30              CLEAR                   DS7T
010570970418     C  N30              MOVEL     TBLUNI        DS7T
010580970418     C*
010590970901     C* FLAG DI SOSTITUZIONE
010600970418     C                   MOVEL     §7TAFA        TAFA              1
010610970418     C                   MOVEL     §7TAAA        TAAA              1
010620970418     C                   MOVEL     §7TAFN        TAFN              1
010630970418     C                   MOVEL     §7TAAN        TAAN              1
010640991001     C*
010650991001     C* CARICO DA SIGLE VARIE
010660991001     C                   MOVEL     '$2'          COD
010670991001     C                   MOVEL(P)  '1'           KEY
010680991001     C                   EXSR      CTABEL
010690991001     C  N30              MOVEL     TBLUNI        DS$2
010700991001     C   30              CLEAR                   DS$2
010710990930     C**
010720990930     C* CARICO VARIE CHE FANNO PARTE DEL TOTALE IMPONIBILE
010730990930     C**
010740990930     C                   Z-ADD     0             II                3 0
010750990930     C                   MOVEL     'CC'          COD
010760990930     C     KTAB1         CHAIN     TABEL                              30
010770990930    1C     *IN30         DOWEQ     *OFF
010780990930    2C     TBLFLG        IFEQ      ' '
010790990930     C                   MOVEL     TBLKEY        W005A             5
010800990930    3C     W005A         IFEQ      'VARIE'
010810990930     C                   MOVEL     TBLUNI        DSCC
010820990930     C* SIGLA VARIA
010830990930    4C     §CCFL6        IFEQ      'S'
010840990930     C                   ADD       1             II
010850990930     C                   MOVE      TBLKEY        SVA(II)
010860990930    4C                   ENDIF
010870990930    3C                   ENDIF
010880990930    2C                   ENDIF
010890990930     C**
010900990930     C     KTAB1         READE     TABEL                                  30
010910990930    1C                   ENDDO
010920991006     c*
010930991117     C* REPERIMENTO DIVISA DELLA MONETA DI CONTO
010940030612     C                   CLEAR                   Tibs02DS
010950991117     C                   CLEAR                   DGED
010960991117     C                   MOVEL     'C'           T02MOD
010970991117     C                   MOVEL     KNSIF         T02SIF
010980991117     C                   MOVEL     'GED'         T02COD
010990991117     C                   MOVEL     '1'           T02KE1
011000991117     C                   CALL      'TIBS02R'
011010991117     C                   PARM                    KPJBA
011020030612     C                   PARM                    Tibs02DS
011030991117     C     T02ERR        IFEQ      *BLANKS
011040991117     C                   MOVEL     T02UNI        DGED
011050991117     C                   ENDIF
011060011009     C* REPERIMENTO importi DELLA MONETA DI CONTO
011070030612     C                   CLEAR                   Tibs02DS
011080011009     C                   CLEAR                   DGEI
011090011009     C                   MOVEL     'C'           T02MOD
011100011009     C                   MOVEL     KNSIF         T02SIF
011110011009     C                   MOVEL     'GEI'         T02COD
011120011009     C                   MOVEL     §GEDCN        T02KE1
011130011009     C                   CALL      'TIBS02R'
011140011009     C                   PARM                    KPJBA
011150030612     C                   PARM                    Tibs02DS
011160011009     C     T02ERR        IFEQ      *BLANKS
011170011009     C                   MOVEL     T02UNI        DGEI
011180011120     C                   Z-ADD     §GEDFC        DFCSAV
011190011220     C                   Z-ADD     §geimf        imfsav
011200011009     C                   ENDIF
011210011009      * Tabella CV della moneta di conto
011220011009     C                   CLEAR                   DSCV
011230011009     C                   MOVEL     'CV'          COD
011240011009     C                   MOVEL(P)  §GEDCN        KEY
011250011009     C                   EXSR      CTABEL
011260011009     C     *IN30         IFEQ      *OFF
011270011009     C                   MOVEL     TBLUNI        DSCV
011280011009     C                   MOVEL     §CVDEC        DECSAV
011290011009     C                   ENDIF
011300000510     C***
011310000510     C* CARIO TABELLA VARIABILI DPD E POSTE
011320000510     C***
011330000510     C                   MOVEL     '3I'          COD
011340000510     C                   MOVEL(P)  'POSTE'       KEY
011350000510     C                   EXSR      CTABEL
011360000510     C  N30              MOVEL     TBLUNI        DS3IPT
011370000510     C   30              CLEAR                   DS3IPT
011380000510     C*
011390000510     C                   MOVEL     '3I'          COD
011400000510     C                   MOVEL(P)  'DPD'         KEY
011410000510     C                   EXSR      CTABEL
011420000510     C  N30              MOVEL     TBLUNI        DS3IDP
011430000510     C   30              CLEAR                   DS3IDP
011440020715
011450020715      * carico sk con tipi bolla che prevedono l'assicurazione
011460020715     c                   eval      cod = 'TB'
011470020715     c                   clear                   key
011480020715     c                   clear                   xx
011490020715     c     ktab1         setll     tabel00f
011500020715     c                   do        *hival
011510020715     c     ktab1         reade     tabel00f
011520020715      * fine file
011530020715     c                   if        %eof(tabel00f)
011540020715     c                   leave
011550020715     c                   endif
011560020715      * record annullato
011570020715     c                   if        tblflg <> *blanks
011580020715     c                   iter
011590020715     c                   endif
011600020715     c                   movel     tbluni        dstb
011610020715     c                   if        §tbfas = '1'
011620020715     c                   add       1             xx
011630020715     c                   eval      tbl(xx) = tblkey
011640020715     c                   endif
011650020715     c                   enddo
011660070309
011670070309      * Carico sk con eventi che prevedono modifica della cons.rich
011680070309     c                   eval      cod = '2Z'
011690070309     c                   clear                   key
011700070309     c                   clear                   xx
011710090526     c                   clear                   yy
011720070309     c     ktab1         setll     tabel00f
011730070309     c                   do        *hival
011740070309     c     ktab1         reade     tabel00f
011750070309      * fine file
011760070309     c                   if        %eof(tabel00f)
011770070309     c                   leave
011780070309     c                   endif
011790070309      * record annullato
011800070309     c                   if        tblflg <> *blanks
011810070309     c                   iter
011820070309     c                   endif
011830070309     c                   movel     tbluni        ds2Z
011840090526     c* x pgm non richiamato--> controllo solo gli S-sempre
011850090526     c                   if        §2ZADR='S'
011860070309     c                   add       1             xx
011870090526     c                   eval      ADRs(xx) = tblkey
011880070309     c                   endif
011890090526     c*
011900090526     c* x pgm     richiamato--> controllo sia S-sempre  che
011910090526     c*                         C-richiamati
011920090526     c                   if        §2ZADR<>' '
011930090526     c                   add       1             yy
011940090526     c                   eval      ADRc(yy) = tblkey
011950090526     c                   endif
011960070309     c                   enddo
011970021121
011980021121      * Aggancio tabella default tariffe fedex
011990030612     c                   Clear                   Tibs02DS
012000021121     c                   Movel     'C'           T02Mod
012010021121     c                   Movel     KNSIF         T02Sif
012020021121     c                   Movel     'DFE'         T02Cod
012030021121     c                   Movel     'FED'         T02Ke1
012040021121     c                   Call      'TIBS02R'
012050021121     c                   Parm                    Kpjba
012060030612     c                   Parm                    Tibs02DS
012070021121     c                   If        T02Err = *Blanks
012080021121     c                   Movel     T02Uni        DDfe
012090021121     c                   Else
012100021121     c                   Clear                   DDfe
012110021121     c                   EndIf
012120030710
012130060131      * Aggancio tabella VPO recpord VPO
012140030710     c                   Clear                   Dvpo
012150030710     c                   Clear                   Tibs02ds
012160030710     c                   Eval      T02Mod = 'C'
012170030710     c                   Eval      T02Sif = knsif
012180030710     c                   Eval      T02Cod = 'VPO'
012190030710     c                   Movel(p)  'VPO'         T02Ke1
012200030710     c                   Call      'TIBS02R'
012210030710     c                   Parm                    Kpjba
012220030710     c                   Parm                    Tibs02ds
012230030710     c                   If        T02Err = *Blanks
012240030710     c                   Movel     T02Uni        Dvpo
012250030710     c                   EndIf
012260060131
012270060131      * Aggancio tabella VPO record DECO
012280071031     c                   Clear                   Dvpodeco
012290071031     c                   Clear                   Tibs02ds
012300071031     c                   Eval      T02Mod = 'C'
012310071031     c                   Eval      T02Sif = knsif
012320071031     c                   Eval      T02Cod = 'VPO'
012330071031     c                   Movel(p)  'DECO'        T02Ke1
012340071031     c                   Call      'TIBS02R'
012350071031     c                   Parm                    Kpjba
012360071031     c                   Parm                    Tibs02ds
012370071031     c                   If        T02Err = *Blanks
012380071031     c                   Movel     T02Uni        Dvpodeco
012390071031     c                   EndIf
012400140130
012410140130      * Aggancio tabella VPO record DECOFI816TEL
012420140130     c                   Clear                   Tibs02ds
012430140130     c                   Clear                   fil816tel
012440140130     c                   Eval      T02Mod = 'C'
012450140130     c                   Eval      T02Sif = knsif
012460140130     c                   Eval      T02Cod = 'VPO'
012470140130     c                   Movel(p)  'DECOFI816TEL'T02Ke1
012480140130     c                   Call      'TIBS02R'
012490140130     c                   Parm                    Kpjba
012500140130     c                   Parm                    Tibs02ds
012510140130     c                   If        T02Err = *Blanks
012520140130     c                   Movel     T02Uni        fil816tel
012530140130     c                   EndIf
012540150414
012550150414      * Aggancio tabella "VPO" "EXPO"
012560151119     C*                  CLEAR                   tibs02ds
012570151119     c*                  clear                   fil_expo
012580151119     C*                  MOVEL     'C'           T02MOD
012590151119     C*                  MOVEL     KNSIF         T02SIF
012600151119     C*                  MOVEL     'VPO'         T02COD
012610151119     c*                  movel(P)  'EXPO'        t02ke1
012620151119     C*                  CALL      'TIBS02R'
012630151119     C*                  PARM                    KPJBA
012640151119     C*                  PARM                    tibs02ds
012650151119    2C*                  IF        T02ERR = *BLANKS
012660151119     C*                  MOVEa     T02uni        fil_expo
012670151119    2C*                  ENDIF
012680141029
012690141029      * Aggancio tabella VPO record DECOFI816DIR
012700141029     c                   clear                   trulvpods
012710141029     c                   eval      ivpoke1 = 'DECOFI816DIR'
012720141029     c                   call      'TRULVPOR'
012730141029     c                   parm                    trulvpods
012740141029     c                   eval      skfilokdir=skfilok
012750030710
012760030710      * Aggancio tabella MIA
012770030710     c                   Clear                   miapsw
012780030710     c                   Clear                   Tibs02ds
012790030710     c                   Eval      T02Mod = 'C'
012800030710     c                   Eval      T02Sif = knsif
012810030710     c                   Eval      T02Cod = 'MIA'
012820030710     c                   Movel(p)  'TUTTI'       T02Ke1
012830030710     c                   Call      'TIBS02R'
012840030710     c                   Parm                    Kpjba
012850030710     c                   Parm                    Tibs02ds
012860030710     c                   If        T02Err = *Blanks
012870030710     c                   Movel     T02Uni        miapsw
012880030710     c                   EndIf
012890160318      * Aggancio tabella NSD (per Import DPD "VEDI PACCO")
012900160318     c                   clear                   dnsd
012910160318     c                   Clear                   Tibs02ds
012920160318     c                   Eval      T02Mod = 'C'
012930160318     c                   Eval      T02Sif = knsif
012940160318     c                   Eval      T02Cod = 'NSD'
012950160318     c                   Movel(p)  '1'           T02Ke1
012960160318     c                   Call      'TIBS02R'
012970160318     c                   Parm                    Kpjba
012980160318     c                   Parm                    Tibs02ds
012990160318     c                   If        T02Err = *Blanks
013000160318     c                   Movel     T02Uni        dnsd
013010160318     c                   EndIf
013020031120
013030031120      * Aggancio tabella AR5 per record 'GEN'
013040031120     c                   Clear                   dAr5
013050031120     c                   Clear                   Tibs02ds
013060031120     c                   Eval      T02Mod = 'C'
013070031120     c                   Eval      T02Sif = knsif
013080031120     c                   Eval      T02Cod = 'AR5'
013090031120     c                   Movel(p)  'GEN'         T02Ke1
013100031120     c                   Call      'TIBS02R'
013110031120     c                   Parm                    Kpjba
013120031120     c                   Parm                    Tibs02ds
013130031120     c                   If        T02Err = *Blanks
013140031120     c                   Movel     T02Uni        dAr5
013150031120     c                   EndIf
013160040806      * Tabella 2G
013170040806     c                   Movel     '2G'          cod
013180040902     c                   Movel(p)  '2'           key
013190040806     c                   ExSr      ctabel
013200040902     c  n30              Movel     TblUni        ds2g2
013210040902     c   30              Clear                   ds2g2
013220050901      *
013230941222     C***
013240931119     C* DEFINIZIONE CAMPI
013250931119     C***
013260130411     C**** *LIKE         DEFINE    D66BDI        §7LFIN
013270991112     C     *LIKE         DEFINE    D66FFI        §7LFFI
013280991112     C     *LIKE         DEFINE    DATEU         WDATA
013290011010     C     *LIKE         DEFINE    §CVDEC        COMDEC
013300990702     C     *LIKE         DEFINE    AR6IMV        SAVIMV
013310151016     C     *LIKE         DEFINE    AR6IMV        SAVIMV_fat
013320990702     C     *LIKE         DEFINE    FVVNPG        KNPG
013330170116     C***  *LIKE         DEFINE    SOVR(1)       SOARBT
013340170116     C***  *LIKE         DEFINE    SOVR(1)       SAARBT
013350170116     C***  *LIKE         DEFINE    SOVR(1)       SDARBT
013360941213     C     *LIKE         DEFINE    AR6SV1        WSV1
013370941212     C     *LIKE         DEFINE    AR6VA1        WVA1
013380941212     C     *LIKE         DEFINE    TAMPPA        WPPA
013390941212     C     *LIKE         DEFINE    TAMMPA        WMPA
013400050202     C     *LIKE         DEFINE    ilv59FIE      WFIE
013410941209     C     *LIKE         DEFINE    §15EFT        DESEFT
013420941209     C     *LIKE         DEFINE    TBLKUT        §KUT
013430911211     C     *LIKE         DEFINE    TBLCOD        §COD
013440911211     C     *LIKE         DEFINE    TBLKEY        §KEY
013450941212     C     *LIKE         DEFINE    ARBIAS        VARIAS
013460950426     C     *LIKE         DEFINE    ARBIAS        COMIAS
013470930507     C     *LIKE         DEFINE    ARBCTR        COMCTR
013480941220     C     *LIKE         DEFINE    ARBKSC        KSC2
013490950104     C     *LIKE         DEFINE    ARBFAP        COMFAP
013500950407     C     *LIKE         DEFINE    NEWCBO        SAVCBO
013510950419     C     *LIKE         DEFINE    VIDPID        WPIVA
013520011009      *
013530011120     C     *LIKE         DEFINE    §GEDFC        DFCSAV
013540011220     C     *LIKE         DEFINE    §GEimf        imfsav
013550011009     C     *LIKE         DEFINE    §CVDEC        DECSAV
013560011009      *
013570941220     C* PER I PADRONCINI
013580950113     C     *LIKE         DEFINE    ARBKSC        WKSC
013590950113     C     *LIKE         DEFINE    ARBRSM        WRSM
013600950113     C     *LIKE         DEFINE    ARBRSD        OLDRSD
013610150206     C     *LIKE         DEFINE    ARBlod        OLDlod
013620950113     C     *LIKE         DEFINE    ARBCAD        OLDCAD
013630950113     C     *LIKE         DEFINE    ARBFIN        OLDFIN
013640941209     C     *LIKE         DEFINE    ARBVLF        OLDVLF
013650950114     C     *LIKE         DEFINE    ARBFVF        OLDFVF
013660941209     C     *LIKE         DEFINE    ARBPKF        OLDPKF
013670950113     C     *LIKE         DEFINE    ARBRSM        OLDRSM
013680950113     C     *LIKE         DEFINE    ARBCAM        OLDCAM
013690950113     C     *LIKE         DEFINE    ARBKSC        OLDKSC
013700941220     C*
013710950113     C     *LIKE         DEFINE    ARBVLF        WVOL
013720950113     C     *LIKE         DEFINE    ARBVLF        WSOTV
013730941212     C     *LIKE         DEFINE    ARBVLF        WADDV
013740941215     C     *LIKE         DEFINE    ARBVLF        VARVLF
013750941215     C     *LIKE         DEFINE    ARBPKF        VARPKF
013760950113     C     *LIKE         DEFINE    ARBPKF        WPKG
013770941220     C* PER FAR RIDIGITARE L'IMPORTO
013780941215     C     *LIKE         DEFINE    ARBVAS        VARVAS
013790941215     C     *LIKE         DEFINE    ARBVAD        VARVAD
013800941213     C     *LIKE         DEFINE    AR9VCA        VARVCA
013810950918     C* PER CONTROLLARE SE E' MODIFICATO
013820950918     C     *LIKE         DEFINE    AR9VCA        SAVVCA
013830950918     C     *LIKE         DEFINE    AR9TIC        SAVTIC
013840950918     C     *LIKE         DEFINE    AR9CAS        SAVCAS
013850970808     C     *LIKE         DEFINE    VIDCAD        SAVCAD
013860970808     C     *LIKE         DEFINE    VIDCAD        WCAD
013870970808     C     *LIKE         DEFINE    VIDCAD        WCAD1
013880970808     C     *LIKE         DEFINE    VIDLOD        SAVLOD
013890970808     C     *LIKE         DEFINE    VIDLOD        WLOD
013900970808     C     *LIKE         DEFINE    VIDLOD        WLOD1
013910060111     c     *LIKE         DEFINE    VIDrsd        Wrsd1
013920060111     c     *LIKE         DEFINE    VIDrsd        savrsd
013930061116     c     *LIKE         DEFINE    VIDrsd        savrsdQ
013940060111     c     *LIKE         DEFINE    VIDind        wind1
013950060111     c     *LIKE         DEFINE    VIDind        savind
013960060111     c     *LIKE         DEFINE    VIDprd        wprd1
013970060111     c     *LIKE         DEFINE    VIDprd        savprd
013980060111     c     *LIKE         DEFINE    VIDnzd        wnzd1
013990060111     c     *LIKE         DEFINE    VIDnzd        savnzd
014000970808     C     *LIKE         DEFINE    E14CAP        VARCAD
014010970808     C     *LIKE         DEFINE    E14LOC        VARLOD
014020970808     C     *LIKE         DEFINE    E14CAP        VARCAM
014030970808     C     *LIKE         DEFINE    E14LOC        VARLOM
014040991006     C     *LIKE         DEFINE    AR6POR        VARPOR
014050991006     C     *LIKE         DEFINE    AR6VA1        VA2VA1
014060941220     C* PER IL CONTROLLO
014070941212     C     *LIKE         DEFINE    AR9VCA        WVLT
014080941212     C     *LIKE         DEFINE    AR4NOT        SAVRD2
014090941019     C     *LIKE         DEFINE    AR4TRC        WTRC
014100950424     C     *LIKE         DEFINE    VIDQFT        VARQFT
014110950424     C     *LIKE         DEFINE    VIDKLP        VARKLP
014120950424     C     *LIKE         DEFINE    VIDKSC        VARKSC
014130970418     C     *LIKE         DEFINE    §7TSOZ        WSOSC
014140970418     C     *LIKE         DEFINE    ARBFVF        WFVF
014150970813     C     *LIKE         DEFINE    VIDKSC        MEMKSC
014160991005     C     *LIKE         DEFINE    VIDDIV        DIVGEI
014170991006     C     *LIKE         DEFINE    VIDDIV        WDIV
014180991007     C     *LIKE         DEFINE    AR6DFT        DFT2BO
014190950113     C*
014200950113     C     *LIKE         DEFINE    FTDDDC        WDRT
014210950113     C     *LIKE         DEFINE    FTDPDR        WPDR
014220950113     C     *LIKE         DEFINE    FTDNDC        WNDC
014230950113     C     *LIKE         DEFINE    FTDDDC        WDDC
014240950113     C     *LIKE         DEFINE    FTDTSR        WTSR
014250020327     C     *LIKE         DEFINE    FTDSTP        wstp
014260950114     C* CAMPI COLLEGATI ALLA VARIAZIONE DEL MITTENTE DA STORICIZZARE
014270950114     C     *LIKE         DEFINE    ARBFVF        MITFVF
014280950114     C     *LIKE         DEFINE    ARBVLF        MITVLF
014290950114     C     *LIKE         DEFINE    ARBCTR        MITCTR
014300950601     C* CAMPI PER GESTIONE VARIAZIONI DI BOLLE ESTERE NON LOCALI
014310950601     C     *LIKE         DEFINE    ARBLNA        WEUV
014320950601     C     *LIKE         DEFINE    ARBCVB        PARCVB
014330950601     C     *LIKE         DEFINE    ARBDTV        PARDTV
014340950601     C     *LIKE         DEFINE    ARBORV        PARORV
014350950601     C     *LIKE         DEFINE    ARBAAS        PARAAS
014360950601     C     *LIKE         DEFINE    ARBLNP        PARLNP
014370950601     C     *LIKE         DEFINE    ARBNRS        PARNRS
014380950601     C     *LIKE         DEFINE    ARBNSP        PARNSP
014390950601     C     *LIKE         DEFINE    ARBCVB        PARCVC
014400950601     C     *LIKE         DEFINE    SIMFEL        PARFEV
014410130410     C***  *LIKE         DEFINE    ARITIP        KTIP
014420001204     C     *LIKE         DEFINE    ARBTC1        SAVTC1
014430001204     C     *LIKE         DEFINE    ARBTC1        SAVTC2
014440930507     C*
014450950113     C                   MOVEL     'R'           WTSR
014460911211     C                   MOVEL     'R'           RIS               1
014470930210     C                   MOVEL     'M'           EMME
014480990818     C                   MOVEL     'C '          FTC               2
014490130410     C***                MOVEL     'A'           KTIP
014500931119     C***
014510911211     C* GIRO UDATE
014520931119     C***
014530941014     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + 3
014540941014     C                   TIME                    W0140            14 0
014550941014     C* UDATE IN GGMMAAAA
014560941014     C                   MOVE      W0140         WDTGIO            8 0
014570941014     C*
014580941014     C* UDATE IN AAAAMMGG
014590941014     C                   Z-ADD     WDTGIO        G02DAT
014600941014     C                   MOVEL     *BLANK        G02ERR
014610941014     C                   CALL      'XSRDA8'
014620941014     C                   PARM                    WLBDAT
014630941014     C                   MOVEL     G02INV        DATEU             8 0
014640030710     c     *iso          Move      dateu         wdataoggi
014650970423     C*
014660970423     C                   MOVEL     4             KNPG
014670941014     C**
014680941014     C*  KEY
014690941014     C**
014700941014     C     KARB          KLIST
014710941212     C                   KFLD                    D48AAS
014720941212     C                   KFLD                    D48LNP
014730941212     C                   KFLD                    D48NRS
014740941212     C                   KFLD                    D48NSP
014750941019     C     KARB2         KLIST
014760941212     C                   KFLD                    D48AAS
014770941212     C                   KFLD                    D48LNP
014780941212     C                   KFLD                    D48NRS
014790941212     C                   KFLD                    D48NSP
014800941019     C                   KFLD                    WTRC
014810130410     C***  KARI          KLIST
014820130410     C***                KFLD                    D48AAS
014830130410     C***                KFLD                    D48LNP
014840130410     C***                KFLD                    D48NRS
014850130410     C***                KFLD                    D48NSP
014860130410     C***                KFLD                    KTIP
014870941014     C     KFTT          KLIST
014880941215     C                   KFLD                    FTDPDR
014890941215     C                   KFLD                    FTDTSR
014900941215     C                   KFLD                    FTDNDC
014910950113     C                   KFLD                    FTDDDC
014920950113     C     KFTT2         KLIST
014930950113     C                   KFLD                    WPDR
014940950113     C                   KFLD                    WTSR
014950950113     C                   KFLD                    WNDC
014960950113     C                   KFLD                    WDRT
014970991005     C     KFTD          KLIST
014980950113     C                   KFLD                    WPDR
014990950113     C                   KFLD                    WTSR
015000950113     C                   KFLD                    WNDC
015010950113     C                   KFLD                    WDRT
015020950113     C                   KFLD                    WKSC
015030950113     C                   KFLD                    WRSM
015040020326     C     KFTD5         KLIST
015050020326     C                   KFLD                    WPDR
015060020326     C                   KFLD                    WTSR
015070020326     C                   KFLD                    WNDC
015080020326     C                   KFLD                    WDRT
015090020326     C                   KFLD                    D48AAS
015100020326     C                   KFLD                    D48LNP
015110020326     C                   KFLD                    D48NRS
015120020326     C                   KFLD                    D48NSP
015130941014     C* ACCESSO   TABEL                                    *
015140990930     C     KTAB1         KLIST                                                  *
015150990930     C                   KFLD                    CODUT                          *
015160990930     C                   KFLD                    COD               2            *
015170941014     C     KTAB2         KLIST                                                  *
015180941014     C                   KFLD                    CODUT                          *
015190941014     C                   KFLD                    COD               2            *
015200941014     C                   KFLD                    KEY               8            *
015210941014     C* ACCESSO   CNACO                                    *
015220941014     C     KACO          KLIST                                                  *
015230941014     C                   KFLD                    CODUT                          *
015240060131     C                   KFLD                    dutkci                         *
015250941014     C                   KFLD                    KSC               7 0          *
015260970423     C     KFVV          KLIST                                                  *
015270970423     C                   KFLD                    KNPG                           *
015280970423     C                   KFLD                    W0050                          *
015290020911     C                   KFLD                    Kfgs
015300941014     C* ACCESSO   TFTAM CON PROGRESSIVO                    *
015310941014     C*
015320941014     C     KTAM4         KLIST                                                  *
015330941014     C                   KFLD                    KSC                            *
015340941014     C                   KFLD                    COMCTR                         *
015350941014     C                   KFLD                    COMPRG            3 0          *
015360941014     C     KTAM6         KLIST                                                  *
015370941014     C                   KFLD                    KSC2                           *
015380941014     C                   KFLD                    COMCTR                         *
015390941014     C                   KFLD                    COMPRG                         *
015400941014     C     KTPT          KLIST                                                  *
015410941014     C                   KFLD                    KSC                            *
015420941014     C                   KFLD                    COMCTR                         *
015430941014     C                   KFLD                    COMPRG                         *
015440990818     C                   KFLD                    FTC               2            *
015450030124
015460030124     c     kAr5          Klist
015470030124     c                   Kfld                    D48Aas
015480030124     c                   Kfld                    D48Lnp
015490030124     c                   Kfld                    D48Nrs
015500030124     c                   Kfld                    D48Nsp
015510030124     c                   Kfld                    kAr5Trd
015520030722
015530030722     C     kazcln        klist
015540030722     C                   kfld                    ktfp
015550030722     C                   kfld                    ktfa
015560030722     C                   kfld                    kann
015570030722     C                   kfld                    kmes
015580040809
015590040809     C* CHIAVE SU FNDCT01L da *PGM utilità FIDN12R - Completa
015600080701     C***  KDCT1_C       KLIST
015610080701     C***                KFLD                    dsaac
015620080701     C***                KFLD                    dsfil
015630080701     C***                KFLD                    dsnca
015640041020     C* CHIAVE SU Fiqdt01L
015650041020     C     Kqdt          KLIST
015660041020     C                   KFLD                    kfgs
015670041020     C                   KFLD                    w0060
015680170627     C                   KFLD                    arbddc
015690050601     c*
015700050601     C* CHIAVE SU Tita430C
015710050601     C     Kta4          KLIST
015720050601     C                   KFLD                    D48AAS
015730050601     C                   KFLD                    D48LNP
015740050601     C                   KFLD                    D48NRS
015750050601     C                   KFLD                    D48NSP
015760050601     c                   kfld                    kta4trc
015770141029
015780141029     C     Kidc1         KLIST
015790141029     C                   KFLD                    D48AAS
015800141029     C                   KFLD                    D48LNP
015810141029     C                   KFLD                    D48NRS
015820141029     C                   KFLD                    D48NSP
015830141029     C                   KFLD                    w0140I
015840030722
015850941212     C**
015860941212     C                   MOVEL     W256A         KPJBU
015870001213     C*
015880001213     C                   MOVE      '0'           OPN48             1
015890911211     C                   ENDSR
015900120426     C*
015910120426      * APERTURA TITA430C
015920120426     c* se provengo da variazione bolla di sede non ho bisogno del tita4
015930120426     C     OPN_TITA4     BEGSR
015940120426     c                   if        %subst(d48cvb:1:1)<>'S'
015950151016     C                   if        NOT %OPEN(TITA430C)
015960120426      * Se sono in ambiente buono - GAITRAGRU
015970141020     c*****              If        knsif = 'FILTRA201 '
015980141020     c                   If        %subst(knsif:7:1) <> 'P'
015990120426     c                   Eval      wlib = 'GAITRAGRU '
016000120426      *  Se sono in ambiente di prova - GAITRAGRPS
016010120426     c                   Else
016020120426     c                   Eval      wlib = 'GAITRAGRPS'
016030120426     c                   EndIf
016040120426     c*
016050120426     c                   Clear                   commt
016060151016     c                   Movel(p)  cmdt(1)       commt
016070120426     c                   Eval      %Subst(commt:30:10) = wlib
016080120426     c                   Eval      commt =
016090151016     c                             %trim(commt) + '/TITA430C)'
016100120426     c                   Call      'QCMDEXC'
016110120426     c                   Parm                    commt
016120120426     c                   Parm                    lenght
016130151016     c                   Open      tita430c
016140120426     c                   endif
016150151016     c* FIAR531C
016160151016     c                   if        not %open(fiar531c)
016170151016     c                   If        %subst(knsif:7:1) <> 'P'
016180151016     c                   Eval      wlib = 'GAITRAGRU '
016190151016      *  Se sono in ambiente di prova - GAITRAGRPS
016200151016     c                   Else
016210151016     c                   Eval      wlib = 'GAITRAGRPS'
016220151016     c                   EndIf
016230151016     c*
016240151016     c                   Clear                   commt
016250151016     c                   Movel(p)  cmdt(2)       commt
016260151016     c                   Eval      %Subst(commt:30:10) = wlib
016270151016     c                   Eval      commt =
016280151016     c                             %trim(commt) + '/FIAR531C)'
016290151016     c                   Call      'QCMDEXC'
016300151016     c                   Parm                    commt
016310151016     c                   Parm                    lenght
016320151016     c                   Open      fiar531c
016330151016     c                   endif
016340151016     c* TITAS30C
016350151016     C                   if        NOT %OPEN(TITAS30C)
016360151016      * Se sono in ambiente buono - GAITRAGRU
016370151016     c                   If        %subst(knsif:7:1) <> 'P'
016380151016     c                   Eval      wlib = 'GAITRAGRU '
016390151016      *  Se sono in ambiente di prova - GAITRAGRPS
016400151016     c                   Else
016410151016     c                   Eval      wlib = 'GAITRAGRPS'
016420151016     c                   EndIf
016430151016     c*
016440151016     c                   Clear                   commt
016450151016     c                   Movel(p)  cmdt(3)       commt
016460151016     c                   Eval      %Subst(commt:30:10) = wlib
016470151016     c                   Eval      commt =
016480151016     c                             %trim(commt) + '/TITAS30C)'
016490151016     c                   Call      'QCMDEXC'
016500151016     c                   Parm                    commt
016510151016     c                   Parm                    lenght
016520151016     c                   Open      titas30c
016530151016     c                   endif
016540151016     c                   endif
016550120426     C                   ENDSR
016560900509     C*
016570050907      *--- CONTROLLO LA PARTICOLARITA' GIACENZA NEL PERIODO DELLE TRAZIONI RIDOTTE ---*
016580050907     C     Contr7q       BEGSR
016590050901      *
016600050907      * Inizializzo un flag che indica se è consentita la variazione della data cons. richiesta
016610050907      * in periodo d trazioni ridotte ed in presenza d particolarità consegna genericamente "G8"
016620050907     c                   clear                   wDCRG8            1
016630050901      * Determino se variazione di bolla con part. giacenza durante
016640050901      *  periodo delle trazioni ridotte
016650050901     c                   If        dateu >= §2g2dri and
016660050901     c                             dateu <= §2g2drf
016670050901      * posso modificare se arbgga <> *Blanks e se in tabella 7Q ha 'R' come cod. disposizione
016680050901     c                   If        ArbGga <> *Blanks
016690050901     c                   Movel     '7Q'          cod
016700050901     c                   Movel(p)  ArbGga        key
016710050901     c                   ExSr      ctabel
016720050901     c  n30              Movel     TblUni        ds7q
016730050901     c   30              Clear                   ds7q
016740050901     c                   If        §7q4d1 = 'R' or §7q4d2 = 'R' or
016750050901     c                             §7q4d3 = 'R' or §7q4d4 = 'R' or
016760050901     c                             §7q4d5 = 'R'
016770050901     c                   Movel     'S'           wDCRG8
016780050901     c                   EndIF
016790050901     c                   EndIF
016800050901     c                   EndIF
016810050901
016820050901     c                   EndSR
016830080201     c
016840900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
016850900509     C     CONTR         BEGSR
016860030122
016870030122     c     Riprova       Tag
016880030122
016890941014     C                   SETOFF                                       90
016900941014     C                   SETOFF                                       203519
016910941014     C                   SETOFF                                       253403
016920950609     C                   SETOFF                                       262729
016930990415     C                   SETOFF                                       73
016940151222     c                   clear                   wfile
016950941014     C***
016960941014     C* VEDO SE E' UN PROGRAMMA RICHIAMATO
016970941014     C***
016980941019     C     D48TRC        IFNE      *BLANKS
016990941019     C                   SETON                                        20
017000950114     C                   ELSE
017010941019     C                   ENDIF
017020950607     C***
017030950607     C* ABILITO L'ENTER COME TASTO DI AGGIORNAMENTO SE RICHIESTO
017040950607     C***
017050950607     C     D48FFR        IFNE      'V'
017060070309     C     D48FFR        andne     'F'
017070950607     C                   SETON                                        26
017080950607     C                   ENDIF
017090070309     c
017100070309     c* Se richiamo con F  --> mi imposto la E  (ma con 26 off non effettua
017110070309     c*    l'aggiornamento per ENTER)
017120070309     c                   if        d48ffr='F'
017130070309     c                   eval      d48ffr='E'
017140070309     c                   endif
017150070309     c
017160950607     C***
017170950607     C* ABILITO F12 SE RICHIESTO
017180950607     C***
017190020603     C     W48F12        IFEQ      'S'
017200950607     C                   SETON                                        27
017210950607     C                   ENDIF
017220950609     C***
017230950609     C* PROTEGGO TUTTI I CAMPI SE RICHIAMATO DA PGM PER ACQUISIZIONE
017240950609     C* AUTOMATICA VARIAZIONI DA PC ESTERO
017250950609     C***
017260140404     C***  D48EUV        IFEQ      'N'
017270140404     C***                SETON                                        29
017280140404     C***                END
017290941222     C**
017300941222     C* APRO I FILE A SECONDA DEL TIPO BOLLA
017310941222     C**
017320941222     C     D48TBO        IFEQ      'A'
017330941223     C                   SETON                                        10
017340941222     C                   ELSE
017350941223     C                   SETOFF                                       10
017360941222     C                   ENDIF
017370941222     C**
017380930505     C* SPEDIZIONE
017390941222     C**
017400151218     c***                if        d48cvb<>'SD' and d48cvb<>'SF'
017410151218     C***10KARB          CHAIN     FNARB01L                           3031
017420151218     C**N10KARB          CHAIN     FNBLP01L                           3031
017430930505     C*
017440930505     C* NON UTILIZZABILE
017450151218     C***  *IN31         IFEQ      *ON
017460151218     C***                MOVEL     MSG(1)        D48MSG
017470151218     C***                MOVEL     '1'           D48ERR
017480151218     C***                SETON                                        90
017490151218     C***                GOTO      ENDCTR
017500151218     C***                ENDIF
017510941223     C* NON TROVATA SPEDIZIONE
017520151218     C***  *IN30         IFEQ      *ON
017530151218     C***                MOVEL     MSG(61)       D48MSG
017540151218     C***                MOVEL     '1'           D48ERR
017550151218     C***                SETON                                        90
017560151218     C***                GOTO      ENDCTR
017570151218     C***                ENDIF
017580151218     c***                endif
017590080201     C*
017600080201     C* SE PGM RICHIAMATO CONTROLLO SOLO LA CORRETTEZZA DELLA CAUSALE
017610080201     C                   CLEAR                   Fnlr66DS
017620080201     C                   MOVEL     'C'           D66GES
017630080201     C                   MOVEL     D48CVB        D66CVB
017640080201     C  N20              MOVEL     D48AAS        D66AAS
017650080201     C  N20              MOVEL     D48LNP        D66LNP
017660080201     C  N20              MOVEL     D48NRS        D66NRS
017670080201     C  N20              MOVEL     D48NSP        D66NSP
017680080201     C  N20              MOVEL     D48TBO        D66TBO
017690160115     c                   eval      d66bnp='S'
017700080201     C                   MOVEL     Fnlr66DS      KPJBU
017710080201     C                   CALL      'FNLR66R'
017720080201     C                   PARM                    KPJBA
017730080201     C                   MOVEL     KPJBU         Fnlr66DS
017740080201     C*
017750080201     C* TROVATO ERRORE
017760080201     C     D66ERR        IFEQ      '1'
017770080201      * se prima ho variato il destinatario ed ora devo aggiornare le consegne particolari
017780080201      * in caso di errore provo a cambiare la causale di modifica da 'CP' a 'CR'
017790080201     c                   If        wI0 = 'S'
017800080201     c                   Movel     'CR'          D48Cvb
017810080201     c                   Clear                   wI0
017820080201     c                   Goto      Riprova
017830080201     c                   Else
017840080201     C                   MOVEL     D66MSG        D48MSG
017850080201     C                   MOVEL     D66ERR        D48ERR
017860080201     C                   SETON                                        90
017870151218     c** 10              unlock    fnarb01l
017880151218     c**n10              unlock    fnblp01l
017890080201     c
017900080201     C                   GOTO      ENDCTR
017910080201     c                   EndIf
017920080201     C                   ENDIF
017930141112     c* Le variazioni I0 non sono ammesse se presenti dispo di dirottamento
017940141112     c                   if        d48cvb='I0' or d48cvb='I7' or
017950141112     c                             d48cvb='I8' or d48cvb='I9'
017960141112     c                   clear                   fnlry09ds2
017970141112     c                   eval      ILRY09TCH='T'
017980141112     c                   eval      ILRY09TDIS='D'
017990141112     c                   eval      ILRY09AAS=d48aas
018000141112     c                   eval      ILRY09LNP=d48lnp
018010141112     c                   eval      ILRY09NRS=d48nrs
018020141112     c                   eval      ILRY09NSP=d48nsp
018030141112     c                   eval      kpjbu=fnlry09ds2
018040141112     c                   call      'FNLRY09C'
018050141112     c                   parm                    kpjba
018060141112     c                   eval      fnlry09ds2=kpjbu
018070141112     c                   if        OLRY09ESIT='1'
018080141112     C                   MOVEL     msg(156)      D48MSG
018090141112     C                   MOVEL     '1'           D48ERR
018100141112     C                   SETON                                        90
018110151218     c** 10              unlock    fnarb01l
018120151218     c**n10              unlock    fnblp01l
018130141112     c
018140141112     C                   GOTO      ENDCTR
018150141112     c                   endif
018160141112     c                   endif
018170151218     C**
018180151218     C* SPEDIZIONE
018190151218     C**
018200151218     c                   if        d48cvb<>'SD' and d48cvb<>'SF'
018210151221     c* Manutenzione bolle in arrivo
018220151221     c                   if        *in10
018230151221     c*
018240151221     c                   setoff                                       30
018250151221     c                   eval      wfile='A'
018260151221     c                   if        d66utp<>'P'
018270151221     C     KARB          CHAIN     FNARB01L                           3031
018280151221     c                   endif
018290151221     c                   if        (*in30 and d66utp='T') or d66utp='P'
018300151221     C     KARB          CHAIN     FNblp01L                           3031
018310160112     c                   if        not *in31 and not*in30  and ARBft1<>*blanks
018320151221     c                   unlock    fnblp01l
018330151221     C                   MOVEL     MSG(162)      D48MSG
018340151221     C                   MOVEL     '1'           D48ERR
018350151221     C                   SETON                                        90
018360151221     C                   GOTO      ENDCTR
018370151221     c                   endif
018380151221     c                   eval      wfile='P'
018390151221     c                   endif
018400151221     c                   else
018410151221     c* Manutenzione in partenza
018420151222     c                   eval      wfile='P'
018430151221     C     KARB          CHAIN     FNBLP01L                           3031
018440151221     c                   endif
018450151218     C*
018460151218     C* NON UTILIZZABILE
018470151218     C     *IN31         IFEQ      *ON
018480151218     C                   MOVEL     MSG(1)        D48MSG
018490151218     C                   MOVEL     '1'           D48ERR
018500151218     C                   SETON                                        90
018510151218     C                   GOTO      ENDCTR
018520151218     C                   ENDIF
018530151218     C* NON TROVATA SPEDIZIONE
018540151218     C     *IN30         IFEQ      *ON
018550151218     C                   MOVEL     MSG(61)       D48MSG
018560151218     C                   MOVEL     '1'           D48ERR
018570151218     C                   SETON                                        90
018580151218     C                   GOTO      ENDCTR
018590151218     C                   ENDIF
018600151218     c                   endif
018610080201     C* KEY SPEDIZIONE
018620080201     C                   MOVEL     D48CVB        VIDCVR
018630080201     C                   MOVEL     D48CVB        FLGCVR            1
018640080201     C*
018650080201     C* IMPOSTO IL CAMPO §7LFFI CHE MI SERVE PER ALLOCARE I FILE
018660080201     C                   MOVEL     D66FFI        §7LFFI
018670080201     C*
018680080201     C* IMPOSTO IL CAMPO §7LFIN CHE MI SERVE PER PROTEGGERE I CAMPI
018690080201     C* CHE NON DEVO VARIARE
018700130411     C***                MOVEL     D66BDI        §7LFIN
018710941014     C**
018720941014     C**
018730900509     C     ENDCTR        ENDSR
018740910429     C*
018750910429     C*--- CARICO DATI BOLLA -----------------------------------------*
018760910429     C     CARICA        BEGSR
018770920123     C                   SETOFF                                       040521
018780911211     C                   SETOFF                                       131415
018790990930     C                   SETOFF                                       0102
018800941220     C                   SETOFF                                       363789
018810941213     C                   SETOFF                                       111209
018820991001     C                   SETOFF                                       882307
018830941222     C                   SETOFF                                       525306
018840991012     C                   SETOFF                                       167308
018850061107     C                   SETOFF                                       747576
018860131001     C                   SETOFF                                       878681
018870140213     C                   SETOFF                                       838477
018880140408     c                   setoff                                       68
018890011018      *
018900011018     C                   CLEAR                   FLGVRN
018910110906     C                   CLEAR                   lnacons
018920941102     C*
018930950120     C                   CLEAR                   WIN95
018940950120     C                   CLEAR                   WIN97
018950950120     C                   CLEAR                   WIN93
018960131016     C                   CLEAR                   WINF14
018970950120     C                   CLEAR                   VARVCA
018980941215     C                   CLEAR                   VARVAS
018990941215     C                   CLEAR                   VARVAD
019000941215     C                   CLEAR                   VARVLF
019010941215     C                   CLEAR                   VARPKF
019020990702     C                   CLEAR                   SAVIMV
019030151016     C                   CLEAR                   SAVIMV_fat
019040991006     C                   CLEAR                   WDIV
019050991006     C                   CLEAR                   VARPOR
019060991006     C                   CLEAR                   VA2VA1
019070991012     C                   CLEAR                   SAVCBO
019080091124     C                   CLEAR                   WPT1              1
019090941209     C                   MOVEL     0             CIN               1 0
019100941209     C                   MOVEL     0             DIE               1 0
019110941212     C                   MOVEL     0             SEI               1 0
019120941214     C                   MOVEL     0             SET               1 0
019130941223     C                   MOVEL     0             OTT               1 0
019140970808     C                   MOVEL     0             WZV               1 0
019150970808     C                   MOVEL     0             WZ1               1 0
019160970808     C                   MOVEL     0             WZ2               1 0
019170070508     C                   MOVEL     0             WZ3               1 0
019180970808     C                   MOVEL     0             WZVM              1 0
019190970808     C                   MOVEL     0             WZ1M              1 0
019200970808     C                   MOVEL     0             WZ2M              1 0
019210141029     C                   MOVEL     0             Wforzdcr          1 0
019220141112     C                   MOVEL     0             Wforzgiac         1 0
019230990930     C                   MOVEL     ' '           WAR62             1
019240941019     C                   MOVEL     ' '           WAR6              1
019250941209     C                   MOVEL     ' '           WAR7              1
019260040123     c                   Clear                   War72
019270941019     C                   MOVEL     ' '           WAR9              1
019280941019     C                   MOVEL     ' '           WAR4D             1
019290150206     C                   MOVEL     ' '           WAR4X             1
019300061116     C                   MOVEL     ' '           WAR4Q             1
019310000623     C                   MOVEL     ' '           WBL4W             1
019320920319     C                   Z-ADD     0             COMKSC
019330930507     C                   Z-ADD     0             COMCTR
019340930507     C                   Z-ADD     0             COMPRG
019350911211     C                   Z-ADD     0             VI2DFT
019360911211     C                   Z-ADD     0             VIDDFT
019370941209     C                   Z-ADD     0             VARIAS
019380930507     C                   MOVEL     *BLANKS       VARCTR            3
019390941214     C                   CLEAR                   WSOST
019400030710     c                   Clear                   wokpsw
019410030729
019420030729     c                   Clear                   V1cBan
019430030729     c                   Clear                   V1cCor
019440030729     c                   Eval      *In70 = *Off
019450040123     c                   Clear                   ContaSv
019460040123     c                   Clear                   TotImv
019470060223     C                   CLEAR                   V1CCAS
019480060223     C                   CLEAR                   V1CTIC
019490060223     C                   CLEAR                   V1CVCA
019500060223     C                   CLEAR                   SAVCAS
019510060223     C                   CLEAR                   SAVTIC
019520060223     C                   CLEAR                   SAVVCA
019530090914     C                   CLEAR                   SAVrd2
019540150203     C                   CLEAR                   normlod
019550941215     C* PULIZIA DEI FORMATI VIDEO
019560950210     C                   CLEAR                   LR48D02
019570941215     C                   CLEAR                   LR48C03
019580941215     C                   CLEAR                   LR48D04
019590941215     C                   CLEAR                   LR48D05
019600941215     C                   CLEAR                   LR48D07
019610950104     C                   CLEAR                   LR48D08
019620961211     C                   CLEAR                   LR48D09
019630030124     c                   Clear                   Lr48d10
019640950213     C                   CLEAR                   V1CTIC
019650950213     C                   CLEAR                   V1CCAS
019660950213     C                   CLEAR                   V1CVCA
019670950213     C                   CLEAR                   V1CNT1
019680950213     C                   CLEAR                   V1CNT2
019690950213     C                   CLEAR                   V1CTC1
019700950213     C                   CLEAR                   V1DTC1
019710950213     C                   CLEAR                   V1CTC2
019720950213     C                   CLEAR                   V1DTC2
019730030124     c                   Clear                   VidNom
019740030124     c                   Clear                   VidTel
019750030124     c                   Clear                   VidMot
019760030122     c                   Clear                   wI0
019770031120     c                   Clear                   VidRef
019780070508     c                   Clear                   Vidtfd
019790031120     c                   Clear                   VidTeld
019800070306     c                   Clear                   AggioARP
019810070308     c                   Clear                   savdcr
019820070308     c                   Clear                   savtcr
019830070308     c                   Clear                   savhcr
019840070308     c                   Clear                   bollafbc
019850070309     c                   Clear                   Msgforza          3 0
019860150112     c                   Clear                   Msgforzatxt      78
019870070309     c                   clear                   eveADR            1
019880070910     c                   clear                   WesAR5GEN         1
019890130930     c                   clear                   WesAR5EMD         1
019900071025     c                   clear                   ForzaCDF
019910071025     c                   clear                   ForzaPID
019920071025     c                   clear                   ForzaPID2
019930071025     c                   clear                   sav11pid
019940071026     c                   clear                   AggioBol
019950111025     c                   clear                   Dcrindist         1
019960151019     c                   clear                   PesVolPkcn
019970151019     c                   clear                   PesVolSpc
019980151019     c                   clear                   PesVolImv
019990151019     c                   clear                   TassatoImv
020000061129     c  n10              movel     'T'           bollaft2
020010061129     c   10              movel     'R'           bollaft2
020020160330     c                   clear                   wtogli
020030111207     c* ES - Per il momento lo imposto sempre = "S" in quanto l'ufficio qualità non è
020040111207     c*  sicuro di voler fare la modifica di bloccare la manut coonsegna richiesta
020050111207     c*  dalla manutenzione normale
020060111207     c                   eval      Dcrindist='S'
020070941223     C*
020080941223     C     D48TBO        IFEQ      'A'
020090941223     C                   SETON                                        10
020100941223     C                   ELSE
020110941223     C                   SETOFF                                       10
020120941223     C                   ENDIF
020130941223     C*
020140941223     C* CLEARO I FORMATI DELLE BOLLE
020150051118     C                   CLEAR                   FiAR9000
020160990930     C                   CLEAR                   FIAR7000
020170990930     C                   CLEAR                   FIAR6000
020180030124     c                   Clear                   Fiar5000
020190941220     C**
020200941220     C* PGM RICHIAMATO
020210941220     C**
020220941220     C                   SELECT
020230941220     C* PROGRAMMA RICHIAMATO    DSARBD
020240941220     C     D48TRC        WHENEQ    'D'
020250941220     C                   SETON                                        2034
020260941220     C* PROGRAMMA RICHIAMATO    DSARBK
020270941220     C     D48TRC        WHENEQ    'K'
020280950616     C                   SETON                                        203529
020290990930     C* PROGRAMMA RICHIAMATO    DARBT TIPO RECORD 1
020300941220     C     D48TRC        WHENEQ    'T'
020310941220     C                   SETON                                        2025
020320990416     C*
020330941220     C* PROGRAMMA RICHIAMATO    DSARBG
020340941220     C     D48TRC        WHENEQ    'G'
020350941220     C                   SETON                                        2003
020360990930     C* PROGRAMMA RICHIAMATO    DARBT TIPO RECORD 2
020370941220     C     D48TRC        WHENEQ    '2'
020380941220     C                   SETON                                        2019
020390130930     C* PROGRAMMA RICHIAMATO    DARBN TIPO RECORD N + parm
020400130930     C     D48TRC        WHENEQ    'N'
020410130930     c                   if        %parms=7
020420130930     C                   SETON                                        20
020430131008     c                   else
020440131008     C                   MOVEL     MSG(24)       D48MSG
020450131008     C                   MOVEL     '1'           D48ERR
020460131008     c                   leavesr
020470131008     c                   endif
020480131008     c
020490941220     C                   ENDSL
020500950607     C*
020510020603     C     W48F12        IFEQ      'S'
020520950607     C                   SETON                                        27
020530950607     C                   ENDIF
020540950609     C*
020550140404     C***  D48EUV        IFEQ      'N'
020560140404     C***                SETON                                        29
020570140404     C***                END
020580950114     C**
020590950114     C* 24 ON - VARIATO IL MITTENTE
020600950114     C**
020610950114     C     WRICVL        IFEQ      'S'
020620950114     C                   SETON                                        24
020630950114     C                   ENDIF
020640941019     C**
020650941019     C* CARICO I DATI DELLA VIDEATA GENERICA DELLA BOLLA
020660941019     C**
020670941019     C* CHAIN CON CODICE BOLLA
020680941019     C                   MOVEL     '3A'          COD
020690941102     C                   MOVEL(P)  ARBCBO        KEY
020700941212     C                   EXSR      CTABEL
020710941019     C  N30              MOVEL     TBLUNI        DS3A
020720941019     C   30              CLEAR                   DS3A
020730941019     C*
020740941019     C                   MOVEL     §3ATB1        FLGTB1            1
020750941019     C                   MOVEL     §3ADES        V1DCBO                         DESCRIZIONE
020760071116     c* Verifico se il 1°tipo bolla è tassabile o no
020770071116     C                   MOVEL     'TB'          COD
020780071116     C                   MOVEL(P)  §3atb1        KEY
020790071116     C                   EXSR      CTABEL
020800071116     C  N30              MOVEL     TBLUNI        DSTB
020810071116     C   30              CLEAR                   DSTB
020820071116     c                   movel     §tbfcb        oldtb1FCB         1
020830170419
020840170419     c                   clear                   oldtb2FCB
020850170419     c                   if        §3atb2<>*blanks
020860170419     C                   MOVEL     'TB'          COD
020870170419     C                   MOVEL(P)  §3atb2        KEY
020880170419     C                   EXSR      CTABEL
020890170419     C  N30              MOVEL     TBLUNI        DSTB
020900170419     C   30              CLEAR                   DSTB
020910170419     c                   movel     §tbfcb        oldtb2FCB         1
020920170419     c                   endif
020930071116     c
020940070308     c
020950070308     c** Blocco consegna (mi serve per la consegna richiesta)
020960151221     c***10              eval      bollaFBC=arbfbc
020970151221     c                   if        *in10
020980151221     c                   if        wfile='A'
020990151221     c                   eval      bollaFBC=arbfbc
021000151221     c                   else
021010151221     c                   clear                   bollaFBC
021020151221     c                   endif
021030151221     c                   endif
021040070308     c  N10              clear                   bollaFBC
021050941019     C*
021060941019     C* SE FRANCO PROTEGGO P.IVA
021070011017     C     FLGTB1        COMP      'F'                                    05
021080941222     C   05              SETON                                            06
021090941019     C*
021100941019     C* SE ESISTE SECONDA BOLLA PERMETTO L'IMMISSIONE
021110061107     c                   clear                   ksc2bol
021120941212    1C     §3ATB2        IFNE      '  '
021130941019     C   05              SETOFF                                       05
021140941019     C* CHAIN SU 2 BOLLA
021150990930     C                   MOVEL     '2'           WTRC
021160070525     c                   if        flgcvr='2'
021170990930     C     KARB2         CHAIN     FIAR6000                           30
021180070525     c                   else
021190070525     C     KARB2         CHAIN(N)  FIAR6000                           30
021200070525     c                   endif
021210990930     C  N30              MOVEL     'E'           WAR62
021220990930     C  N30              MOVEL     AR6KSC        V1CKSC
021230991007     C  N30              Z-ADD     AR6DFT        DFT2BO
021240941019     C   30              Z-ADD     9999          V1CKSC
021250941019     C   30              MOVEL     ARBLNA        V1CLNA
021260991007     C   30              MOVE      *ZEROS        DFT2BO
021270061107     c                   movel     v1cksc        ksc2bol           7 0
021280040127      * Vedo se esiste anche il Fiar7
021290040127     c     Karb2         Setll     Fiar701l
021300040127     c                   If        %Equal(Fiar701l)
021310040127     c                   Eval      war72 = 'E'
021320040127     c                   EndIf
021330941019     C                   ELSE
021340941019     C                   MOVEL     ARBKSC        V1CKSC
021350941212    1C                   END
021360941019     C**
021370051118     C* SE PREVEDE IL C/A CHAI SU FiAR9
021380941019     C**
021390941212    1C     §3AFCA        IFNE      0
021400041022     c                   if        flgcvr='K'
021410051118     C     KARB          CHAIN     FiAR9000                           30
021420041022     c                   else
021430041022     c* non alloco record se non è variazione di C/A
021440051118     C     KARB          CHAIN(n)  Fiar9000                           30
021450041022     c                   endif
021460041022     c
021470941212    2C     *IN30         IFEQ      *OFF
021480941019     C                   MOVEL     'E'           WAR9              1
021490941019     C                   MOVEL     AR9CAS        V1CCAS
021500941019     C                   MOVEL     AR9TIC        V1CTIC
021510941102     C                   MOVEL     AR9VCA        V1CVCA
021520950918     C                   MOVEL     AR9CAS        SAVCAS
021530950918     C                   MOVEL     AR9TIC        SAVTIC
021540950918     C                   MOVEL     AR9VCA        SAVVCA
021550941212    2C                   ENDIF
021560941212    1C                   ENDIF
021570941019     C**
021580941019     C* VEDO SE CI SONO LE NOTE
021590941019     C**
021600941019     C                   MOVEL     '8'           WTRC
021610060222     C     KARB2         CHAIN(N)  FiAR4000                           30
021620941212    1C     *IN30         IFEQ      *OFF
021630941019     C                   MOVEL     AR4NOT        V1CNT1
021640941019     C*
021650941019     C                   MOVEL     '9'           WTRC
021660060222     C     KARB2         CHAIN(N)  FiAR4000                           30
021670941212    2C     *IN30         IFEQ      *OFF
021680941019     C                   MOVEL     AR4NOT        V1CNT2
021690941212    2C                   ENDIF
021700941212    1C                   ENDIF
021710970409     C* SI NO DDT
021720031028     c                   Clear                   dsbl4a
021730970409     C                   MOVEL     'A'           WTRC
021740151222     c                   if        wfile='A'
021750151222     C     KARB2         CHAIN(N)  FiAR4000                           30
021760151222    2C     *IN30         IFEQ      *OFF
021770031028     C**!!!              MOVEL     AR4NOT        V1CDDT
021780031028     c                   Movel     Ar4not        dsbl4a
021790031028     c                   Move      §b4abm        v1cddt
021800970409     C                   ELSE
021810970409     C                   MOVEL     'S'           V1CDDT
021820970409    2C                   ENDIF
021830151222     c                   else
021840151222     C                   MOVEL     ARBFSTddt     V1CDDT            1
021850151222     c                   endif
021860970409     C*
021870970409     C     V1CDDT        IFEQ      'S'
021880970409     C     V1CDDT        OREQ      'C'
021890031028     c     v1cddt        oreq      'P'
021900031028     c     v1cddt        oreq      'Q'
021910970409     C                   MOVEL     'DDT'         V1DDDT
021920970409     C                   ELSE
021930970409     C                   CLEAR                   V1DDDT
021940970409     C                   ENDIF
021950060222     C* CONTROLLO SE ESISTE RECORD W DI ar4 PER EVENTUALE
021960000623     C*   MODIFICA DI MITTENTE
021970000623     C     *IN10         IFEQ      *OFF
021980000623     C                   MOVEL     'W'           WTRC
021990060222     C     KARB2         CHAIN(N)  Fiar4000                           30
022000000623     C  N30              MOVEL     'S'           WBL4W             1
022010000623     C                   ENDIF
022020941019     C* DESCRIZIONE CAUSALE
022030941019     C                   MOVEL     D66DES        V1DCVR
022040941019     C                   MOVEL     ARBLNP        V1CLNP
022050941212     C                   MOVEL     ARBNRS        V1CNRS
022060941019     C                   MOVEL     ARBNSP        V1CNSP
022070941019     C                   MOVEL     ARBLNA        V1CLNA
022080941019     C                   MOVEL     ARBCBO        V1CCBO
022090941019     C                   MOVEL     ARBRSM        V1CRSM
022100941019     C                   MOVEL     ARBCAM        V1CCAM
022110941019     C                   MOVEL     ARBLOM        V1CLOM
022120941019     C                   MOVEL     ARBPRM        V1CPRM
022130941019     C                   MOVEL     ARBNZM        V1CNZM
022140970828     C                   MOVEL     ARBFAP        V1CFAP
022150941019     C                   MOVEL     ARBRSD        V1CRSD
022160941019     C                   MOVEL     ARBIND        V1CIND
022170941019     C                   MOVEL     ARBCAD        V1CCAD
022180941019     C                   MOVEL     ARBLOD        V1CLOD
022190941019     C                   MOVEL     ARBPRD        V1CPRD
022200941019     C                   MOVEL     ARBNZD        V1CNZD
022210970828     C                   MOVEL     ARBFIN        V1CFIN
022220941019     C                   MOVEL     ARBCTS        V1CCTS
022230941019     C                   MOVEL     ARBTC1        V1CTC1
022240941019     C                   MOVEL     ARBTC2        V1CTC2
022250941027     C                   MOVEL     ARBPKF        V1CPKF
022260941019     C                   MOVEL     ARBVLF        V1CVLF
022270941019     C                   MOVEL     ARBFVF        V1CFVF
022280030729     c                   Z-Add     ArbNcl        V1cNcl
022290141029     c                   clear                   v1dffd
022300141029     c                   if        arbffd='S'
022310141029     c                   eval      v1dffd='FermoDeposito'
022320141029     c                   endif
022330100121     c* Partita iva MITTENTE se bolla singola franco
022340100121     c                   select
022350100121     c                   when      flgtb1='F' and §3atb2=*blanks
022360100121     c                   eval      v1cpim=arbcpi
022370100121     c                   when      flgtb1='A' and §3atb2=*blanks
022380100121     c                   eval      v1cpid=arbcpi
022390100121     c* In arrivo   per bolla  doppia --> partita iva dell'assegnato
022400151222     c                   when      wfile='A'   and §3atb2<>*blanks
022410100121     c                   eval      v1cpid=arbcpi
022420100121     c* In partenza per bolla  doppia --> partita iva del mittente
022430151222     c                   when      wfile='P'   and §3atb2<>*blanks
022440100121     c                   eval      v1cpid=arbcpi
022450100121     c                   ENDSL
022460100121
022470950114     C* SALVO IL CODICE CLIENTE DELLA SPEDIZIONE
022480950114     C                   MOVEL     ARBKSC        SAVKSC            7 0
022490060511     C                   MOVEL     ARBKSC        kscbolla          7 0
022500941102     C* PRENDO LA VALUTA DAL DESTINATARIO E MITTENTE
022510941102     C                   MOVEL     '15'          COD
022520941102     C                   MOVEL(P)  ARBNZD        KEY
022530941212     C                   EXSR      CTABEL
022540941102     C  N30              MOVEL     TBLUNI        DS15
022550941102     C   30              CLEAR                   DS15
022560941102     C                   MOVEL     §15VLT        DESVLT            3
022570941212    1C     §15ITA        IFEQ      'I'
022580941102     C                   MOVEL     'I'           WDESIE            1
022590941102     C                   ELSE
022600941102     C                   MOVEL     'E'           WDESIE            1
022610941212    1C                   ENDIF
022620020305     c                   MOVEL     §15EFT        DESEFTW
022630020305     c                   MOVEL     §15EFD        DESEFDW
022640020305     c                   MOVEL     §15EFF        DESEFFW
022650971230     V* CHAIN SU AZORG CON LINEA PARTENZA
022660971230     C     ARBLNP        CHAIN     AZORG01L                           32
022670941209     C*
022680000223     C                   MOVEL     ORGDE3        OG143
022690020305     C                   MOVEL     §OGNTW        LNPNTW
022700941102     C*
022710941102     C                   MOVEL(P)  ARBNZM        KEY
022720941212     C                   EXSR      CTABEL
022730941102     C  N30              MOVEL     TBLUNI        DS15
022740941102     C   30              CLEAR                   DS15
022750941102     C                   MOVEL     §15VLT        MITVLT            3
022760941212    1C     §15ITA        IFEQ      'I'
022770941102     C                   MOVEL     'I'           WMITIE            1
022780941102     C                   ELSE
022790941102     C                   MOVEL     'E'           WMITIE            1
022800941212    1C                   ENDIF
022810001204     C**
022820051129     C* TERMINAL DI PARTENZA DELLA LNP: VEDO SE HO VDL
022830040123     c                   Clear                   og150
022840970901     C     ARBTFP        CHAIN     AZORG                              32
022850051129     C                   CLEAR                   WVDL
022860970901    1C     *IN32         IFEQ      *OFF
022870051129     C**!!!              TESTN                   DATVDL               31
022880040123     c                   Movel     Orgdf0        og150
022890120613     c                   testn                   §ogvol               31
022900970901     C*
022910040123    2C**!!!*IN31         IFEQ      '1'
022920051129     C**!!!DATVDL        ANDGT     *ZEROS
022930120613     c***!!!             If        §ogspv = 'S'
022940120613     c                   if        *in31 and §ogvol>*zeros
022950051129     C                   MOVEL     'S'           WVDL              1
022960970901    2C                   END
022970970901    1C                   END
022980941019     C**
022990941019     C* TIPO SERVIZIO
023000941019     C**
023010090604     c                   clear                   ds5e
023020941014     C                   MOVEL     '5E'          COD
023030941014     C                   MOVEL(P)  ARBTSP        KEY
023040941212     C                   EXSR      CTABEL
023050090604     C  N30              MOVEL     TBLUNI        ds5e
023060090604     C  N30              MOVEL     §5ed08        V1DTSP
023070170131     C  N30              MOVEL     §5ed03        WTSP03
023080941220     C   30              CLEAR                   V1DTSP
023090170131     C   30              CLEAR                   wtsp03
023100941019     C**
023110910429     C* DATA SPEDIZIONE
023120941019     C**
023130941014     C                   MOVEL     '3'           G02ERR
023140941014     C                   Z-ADD     ARBMGS        G02INV
023150941014     C                   MOVEL     ARBAAS        G02INV
023160941014     C                   CALL      'XSRDA8'
023170941014     C                   PARM                    WLBDAT
023180941014     C*
023190941212     C                   Z-ADD     G02DAT        V1CDSP
023200941019     C**
023210930505     C* LINEA DI ARRIVO
023220941019     C**
023230070306     C     ARBLNA        CHAIN     AZORG                              30
023240941019     C  N30              MOVEL     ORGDES        V1DLNA                          L.ARRIVO
023250941019     C   30              MOVEL     *BLANKS       V1DLNA
023260000202     C                   MOVEL     ORGDE3        OG143
023270020305     C                   MOVEL     §OGNTW        LNANTW
023280020305     c                   select
023290020305     c     lnantw        wheneq    'DPD'
023300020305     c                   movel     desefdw       deseft
023310020305     c     lnantw        wheneq    'FED'
023320020305     c                   movel     deseffw       deseft
023330020305     c                   other
023340020305     c                   movel     deseftw       deseft
023350020305     c                   endsl
023360160225
023370160225     c                   if        lnantw='FED'
023380160225     c                   clear                   trulc7ds
023390160225     c                   eval      ic7tntw='FED'
023400160225     c                   eval      ic7tip='FM'
023410160225     c                   call      'TRULC7R'
023420160225     c                   parm                    trulc7ds
023430160225     c                   eval      fedm_ksc=oc7kscc
023440160225     c                   movel     oc7ctrc       fedm_ctr
023450160225     c
023460160225     c                   clear                   trulc7ds
023470160225     c                   eval      ic7tla='L'
023480160225     c                   eval      ic7tntw='FED'
023490160225     c                   eval      ic7tip='FD'
023500160225     c                   call      'TRULC7R'
023510160225     c                   parm                    trulc7ds
023520160225     c
023530160225     c                   eval      fedd_ksc=oc7kscc
023540160225     c                   movel     oc7ctrc       fedd_ctr
023550160225     c                   endif
023560070906     c*****
023570070906     c* Verifico se destinatario disagiato
023580070906     c*****
023590070906     c                   clear                   wdisagdest
023600070906     c                   clear                   tisit0ds
023610070906     c                   movel     'E'           it0tla
023620070906     c                   movel     arbnzd        it0naz
023630070906     c                   movel     arbprd        it0prv
023640070906     c                   movel     arbcad        it0cap
023650070906     c                   movel     arblod        it0loc
023660070906     c                   movel     arbind        it0ind
023670070906     c                   movel     arbrsd        it0rag
023680070906     c                   call      'TISIT5R'
023690070906     c                   parm                    tisit0ds
023700070906    1c     ot0err        ifeq      *blanks
023710070906    2c                   if        ot0dos = 'A' or ot0dos = 'D' or
023720070906     c                             ot0dos = 'S'
023730070906     c                   movel     ot0dos        wdisagdest
023740070907    3c     wdisagdest    ifeq      'D'
023750070906     c                   movel     'X'           wdisagdest
023760070906    3c                   endif
023770070906    2c                   endif
023780070906   x1c                   else
023790070906     c                   clear                   wdisagdest
023800070906    1c                   endif
023810070306     c
023820070910     c****
023830070910      * Reperisco record GEN nel fiar5
023840070910     c                   Clear                   dAr5Genpar
023850070910     c                   Eval      kAr5Trd = 'GEN'
023860070910     c     kAr5          Chain(n)  Fiar501l
023870070910     c                   If        %Found(Fiar501l)
023880070910     c                   Movel     Ar5Uni        dAr5Genpar
023890070910     c                   eval      wesar5gen='S'
023900070910     c                   endif
023910070319     c****
023920070319     c* FILIALE GESTIONE: VEDO SE GEO ATTIVA
023930070319     c****
023940070319     c                   clear                   FGSgeot
023950070319     c                   clear                   FGSddaSI
023960110906     c****               if        *in10
023970070319     c                   clear                   fnlv55ds
023980070319     C                   MOVEL     '6'           D55TPT
023990070319     C                   MOVEL     ARBLNA        D55LIN
024000070319     C                   MOVEL     DATEU         D55DRF
024010070319     C                   CALL      'FNLV55R'
024020070319     C                   PARM                    Fnlv55DS
024030110906     c                   eval      lnacons=d55tfa
024040070319     c
024050070319     c     d55tfa        CHAIN     AZORG                              30
024060070319     c                   if        not *in30
024070070319     C                   MOVEL     ORGDE8        og148                           L.ARRIVO
024080070319     C                   MOVEL     ORGDE6        og146                           L.ARRIVO
024090070319     C
024100070319     c                   movel     §oggeot       FGSgeot           1
024110070319     C
024120070319    1c                   if        §ogdda>*zeros
024130070319     c                   testn                   §ogdda               30
024140070319     c                   move      §ogdda        w001a             1
024150070319    2c                   if        *in30 and w001a>='0'
024160070319     c* Se la data = 20391231, come se non ci fosse
024170070319    3c                   if        §ogdda<>'20391231'
024180070319     c                   movel     §ogdda        w0080             8 0
024190070426     c* Se <=oggi, attiva procedura distinte automatiche
024200070426    4c                   if        w0080<=dateu
024210070319     c                   movel     'S'           FGSddaSI          1
024220070319    4c                   endif
024230070319    3c                   endif
024240070319    2c                   endif
024250070319    1c                   endif
024260070319     c
024270070319     c                   endif
024280110906     c****               endif
024290151222     c** 10              if        fgsgeot='S'  and dutgeot='S'
024300151222     c                   if        wfile='A' and fgsgeot='S'  and dutgeot='S'
024310140213     c                   seton                                        77
024320140213     c                   endif
024330070329     c* Se p.o. utente o lna hanno GEO attiva, propongo F17 note
024340140130     c* non le propongo se bolla in consegna variazione da inviare a AUT e
024350140130     c* filiale partita con la nuova gestione telefonate AUT
024360151222     c** 10              if        fgsgeot='S'  and dutgeot='S'
024370151222     c                   if        wfile='A' and fgsgeot='S'  and dutgeot='S'
024380140130     c                             and (
024390140130     c* non è in consegna
024400140130     c                             (arbndc=0 or arbdcm>0)
024410140130     c* filiale distinta non partita con nuova gestione telefonate AUT
024420140130     c                             or (%lookup(%subst
024430140130     c                                 (%editc(arbifp:'X'):7:3):skFil816tel)=0
024440140130     c                              and skfil816tel(1)<>'999')
024450140130     c* Causale variazione da non trasmettere a AUT
024460140130     c                             or d66pda=*blanks
024470140130     c                             )
024480070329     c                   seton                                        87
024490070329     c                   endif
024500941019     C**
024510941019     C* CONSEGNE PARTICOLARI
024520941019     C**
024530941019     C                   MOVEL     *BLANKS       V1DTC1
024540941019     C                   MOVEL     *BLANKS       V1DTC2
024550070914     C                   MOVEL     *BLANKS       V7DTC1
024560070914     C                   MOVEL     *BLANKS       V7DTC2
024570941212    1C     ARBTC1        IFNE      ' '
024580930505     C                   MOVEL     '1P'          COD
024590941019     C                   MOVEL(P)  ARBTC1        KEY
024600941212     C                   EXSR      CTABEL
024610941102     C  N30              MOVEL     TBLUNI        DS1P
024620941102     C   30              CLEAR                   DS1P
024630941102     C                   MOVEL     §1PDES        V1DTC1
024640941220     C                   MOVEL     §1PDES        V7DTC1
024650941102     C*
024660941102     C* SE IMMESSA DALLA PARTENZA E NON UTILIZZABILE IN ARRIVO:PROTEGGO
024670941222    2C   10§1PUPR        IFEQ      'S'
024680941213     C     §1PUAR        ANDNE     'S'
024690941102     C                   SETON                                        11
024700941212    2C                   ENDIF
024710941222     C* O VICEVERSA
024720941222    2C  N10§1PUAR        IFEQ      'S'
024730941222     C     §1PUPR        ANDNE     'S'
024740941222     C                   SETON                                        11
024750941222    2C                   ENDIF
024760941102     C*
024770941212    1C                   ENDIF
024780941019     C*
024790941212    1C     ARBTC2        IFNE      ' '
024800941019     C                   MOVEL     '1P'          COD
024810941213     C                   MOVEL(P)  ARBTC2        KEY
024820941212     C                   EXSR      CTABEL
024830941102     C  N30              MOVEL     TBLUNI        DS1P
024840941102     C   30              CLEAR                   DS1P
024850941102     C                   MOVEL     §1PDES        V1DTC2
024860941220     C                   MOVEL     §1PDES        V7DTC2
024870941102     C*
024880941102     C* SE IMMESSA DALLA PARTENZA E NON UTILIZZABILE IN ARRIVO:PROTEGGO
024890941222    2C   10§1PUPR        IFEQ      'S'
024900941213     C     §1PUAR        ANDNE     'S'
024910941102     C                   SETON                                        12
024920941212    2C                   ENDIF
024930941222     C* O VICEVERSA
024940941222    2C  N10§1PUAR        IFEQ      'S'
024950941222     C     §1PUPR        ANDNE     'S'
024960941222     C                   SETON                                        12
024970941222    2C                   ENDIF
024980941222     C*
024990941212    1C                   ENDIF
025000941019     C**
025010941019     C* SPEDIZIONE GIACENTE
025020941019     C**
025030040316     c                   Clear                   wgcfas
025040040324     c                   Clear                   wgcded
025050040324     c                   Clear                   wgcddm
025060051109     c                   Clear                   dgcpflr
025070141113if  1C**   *IN10         IFEQ      *ON
025080141113     c**   d48cvb        oreq      'CS'
025090141113     c**   d48cvb        oreq      'CR'
025100050331     C     KARB          CHAIN     Tigcp21L                           30        GIACENZA
025110941212    1C     *IN30         IFEQ      *OFF
025120050331     C     GCPFAS        ANDLT     40
025130941019     C     GCPATB        ANDEQ     ' '
025140040316     c                   Eval      wgcfas = GcpFas
025150040324     c                   Eval      wgcded = GcpDed
025160040324     c                   Eval      wgcddm = GcpDdm
025170051109     c                   Movel     gcpflr        dgcpflr
025180941019     C                   SETON                                        04
025190941212    1C                   ENDIF
025200040219      * se non ho trovato il rcd di giacenza in arrivo provo in partenza
025210040219      * solo se sto facendo una variazione 'CS' o 'CR'
025220051109      * questo solo per accendere l'indicatore 04 di giacenza aperta, non serve per la
025230051109      * variazione della data consegna richiesta, questa è possibile solo se la giacenza
025240051109      * è aperta in arrivo!!!!
025250141113      * Ora lo 04 mi serve anche per il controllo dell'abilitazione dei dirottamenti sia
025260141113      * in partenza che in arrivo e quinidi lo faccio sempre
025270141113if  2c***                If        *In30 and
025280141113     c***                          (d48cvb = 'CS' or d48cvb = 'CR')
025290141113if  2c                   If        *In30
025300050331     c     kArb          Chain     Tigcp01l
025310050331if  3c                   If        %Found(Tigcp01l) and GcpFas < 50 and
025320040219     c                             GcpAtb = *Blanks
025330040219     c                   Seton                                        04
025340040316     c                   Clear                   wgcfas
025350040324     c                   Clear                   wgcded
025360040324     c                   Clear                   wgcddm
025370040219    3c                   EndIf
025380040219    2c                   EndIf
025390141113    1C*****              ENDIF
025400051129     C* DETERMINO SE IL VOLUME VDL (CHE EVENTUALMENTE SOSTITUIRA' IL
025410970418     C* VOLUME DA FATTURARE) E' PARZIALE O TOTALE
025420970418     C                   CLEAR                   WFVF
025430050111     C     arbncl        IFGT      ARBNCR
025440970418     C                   MOVEL     'Z'           WFVF                           VOLUME PARZ.
025450970418     C                   ELSE
025460970418     C                   MOVEL     'T'           WFVF                           VOLUME TOTAL
025470970418     C                   ENDIF
025480051129      * Determino se il peso VDL è parziale o totale
025490030122     c                   Clear                   VidTpk
025500030123     c                   Clear                   wfpf
025510030122     c                   If        ArbNcp > *Zeros
025520050111     c                   If        arbncl > ArbNcp
025530030122     c                   Eval      VidTpk = 'Par.'                              Peso Parziale
025540030123     c                   Eval      wfpf = 'Z'
025550030122     c                   Else
025560030122     c                   Eval      VidTpk = 'Tot.'                              Peso Totale
025570030123     c                   Eval      wfpf = 'T'
025580030122     c                   EndIf
025590030122     c                   EndIf
025600030729      * Recupero i Bancali
025610030729     c                   If        %Subst(ArbGva:1:1) = 'B'
025620030729     c                   Clear                   dAr5Ban
025630030729     c                   Eval      kAr5Trd = 'BAN'
025640030729     c     kAr5          Chain(N)  Fiar501l
025650030729     c                   If        %Found(Fiar501l)
025660030729     c                   Movel     Ar5Uni        dAr5Ban
025670030729     c     §Ar5Nba       Add       §Ar5Nb2       V1cBan
025680030729     c                   EndIf
025690030729     c                   EndIf
025700030729      * Recupero i Bancali e i colli originali
025710030729     c                   If        %Subst(ArbGva:1:1) = 'O'
025720030729     c                   Clear                   dAr5Bnb
025730030729     c                   Eval      kAr5Trd = 'BNB'
025740030729     c     kAr5          Chain(N)  Fiar501l
025750030729     c                   If        %Found(Fiar501l)
025760030729     c                   Movel     Ar5Uni        dAr5Bnb
025770030729     c     §Ar5bNba      Add       §Ar5bNb2      V1cBan
025780030729     c                   Z-Add     §Ar5bCor      V1cCor
025790030729     c                   Eval      *In70 = *On
025800030729     c                   EndIf
025810030729     c                   EndIf
025820130924      * Memorizzo se presente avviso al destinatario per passarlo alla tassazione
025830110629     c                   clear                   wemd
025840130924     c                   Clear                   dAr5emd
025850110629     c                   Eval      kAr5Trd = 'EMD'
025860130924     c     kAr5          chain(N)  Fiar501l
025870130924     c                   if        %found(fiar501l)
025880130924     c                   Movel     Ar5Uni        dAr5EMD
025890130930     c                   eval      wesar5EMD='S'
025900130924     c                   select
025910130924     c                   when      emd_§ar5mmp='S' and emd_§ar5smp=' '
025920110629     c                   eval      wemd='S'
025930130924     c                   when      emd_§ar5mmp='S' and emd_§ar5smp='S'
025940130924     c                   eval      wemd='E'
025950130924     c                   when      emd_§ar5smp='S'
025960130924     c                   eval      wemd='X'
025970130924     c                   endsl
025980110629     c                   endif
025990061107     C**
026000071022     C* F N A R B D 0 F  record del codice fiscale e partita IVA
026010061107     C**
026020061107    1C     VIDCVR        IFEQ      'FI'
026030071022     c*
026040061129     c* Memorizzo se bolla già trasmessa o meno
026050070528     c*  solo in partenza(in arrivo è per forza trasmessa
026060151222     c  n10              movel     arbft2        bollaft2          1
026070151222     c**                 if        wfile='P'
026080151222     c**                 movel     arbft2        bollaft2          1
026090151222     c***                endif
026100071022     c* Dati per partita IVA
026110071022     c
026120071025     c                   clear                   ar4not_P
026130071022     C                   MOVEL     'P'           WTRC
026140061107     C     KARB2         CHAIN(N)  FiAR4000                           30
026150071025     c  n30              movel     ar4not        ar4not_p
026160071022     C                   MOVEL     'Q'           WTRC
026170071022     C     KARB2         CHAIN(N)  FiAR4000                           30
026180061107     c
026190061107     c* Verifico se bolla singola .Se doppia guardo se sono in partenza
026200061107     c*  o arrivo
026210061107     c* carico il mittente se bolla franco in partenza sempre
026220061107     c*  o franco singola in arrivo
026230061107    2c                   if        (flgtb1='F'  and not *in10) or
026240061107     c                             (flgtb1='F'  and §3atb2=*blanks)
026250061107     c                   eval      v11mide='M'
026260061107     c* Mittente
026270061107     c                   eval      v11decmd=CMITT
026280061107     c                   eval      v11cod=arbksc
026290061107     c                   eval      v11rsc=arbrsm
026300061107     c                   eval      wnar=arbnzm
026310071025     c* codice fiscale
026320061107     c                   if        not *in30 and not *in20
026330061107     c                   movel     ar4not        v11cdfis
026340061107     c                   else
026350061107     c                   clear                   v11cdfis
026360061107     c                   endif
026370071025     c* Partita IVA
026380071107     c****               if        not *in20
026390071025     c                   if        %subst(ar4not_P:1:16)<>*blanks
026400071025     c                   movel     ar4not_P      v11pid
026410071025     c                   else
026420071025     c                   movel     arbcpi        v11pid
026430071025     c                   endif
026440071107     c****               endif
026450071025     c
026460061107     c* In arrivo di fatto non posso inserire cod.fiscale su un franco
026470061107     c*    singolo: proteggo il campo
026480061107    3c                   if        *in10 and §3atb2=*blanks
026490061107     c                   seton                                        76
026500061107    3c                   endif
026510061107    2c                   endif
026520061107     c
026530061107     c* Carico il destinatario in arrivo sempre, in partenza se singola
026540061107    2c                   if        (flgtb1='A') or
026550061107     c                             (flgtb1='F'  and §3atb2<>*blanks and
026560061107     c                             *in10)
026570061107     c                   eval      v11mide='D'
026580061107     c                   eval      v11decmd=Cdest
026590061107     c                   eval      v11rsc=arbrsd
026600061107     c                   eval      wnar=arbnzd
026610061107     c                   if        not *in30 and not *in20
026620061107     c                   move      ar4not        v11cdfis
026630061107     c                   else
026640061107     c                   clear                   v11cdfis
026650061107     c                   endif
026660071025     c* Partita IVA
026670071107     c*****              if        not *in20
026680071025     c                   if        %subst(ar4not_P:20:16)<>*blanks
026690071026     c                   move      ar4not_P      v11pid
026700071025     c                   else
026710071107     c                   movel     arbcpi        v11pid
026720071025     c                   endif
026730071107     c*****              endif
026740071025     c* In partenza proteggo sempre
026750061107     c  n10              seton                                        76
026760061107     c* codice destinatario: dal fiar6 prima o seconda bolla
026770061107    3c                   if        §3atb2<>*blanks
026780061107     c                   eval      v11cod=ksc2bol
026790061107   x3c                   else
026800061107     C                   MOVEL     '1'           WTRC
026810061107     C     KARB2         CHAIN(N)  FIAR6000                           30
026820061107     c   30              eval      v11cod=arbksc
026830061107     c  n30              eval      v11cod=ar6ksc
026840061107    3c                   endif
026850061107    2c                   endif
026860061107     C*
026870061107     C* PGM RICHIAMATO
026880061107     c                   if        *in20
026890071025     c* in CPI c'e' il codice fiscale
026900061107     c                   movel     §bdcpi        v11cdfis
026910071107     c* in RSD c'e' la partita IVA solo dalla data attivazione
026920071203     c***                if        dateu>=§vpopiv
026930071025     c                   movel     §bdrsd        v11PID
026940071203     c***                endif
026950071107     c                   endif
026960071025     c
026970071025     c* controllo Partita IVA se privato
026980071025    2c                   if        v11pid<>*blanks
026990071025     C     'PRIVATO'     SCAN      v11pid        posiz                    30
027000071025     c* solo numeri
027010071025     c* "PRIVATO" senza iso
027020071025    3c     *in30         ifeq      *off
027030071025     c     v11isd        andge     *zeros
027040071025     c     *in30         oreq      *on
027050071025     c     posiz         andlt     3
027060071025     c                   movel     v11pid        w016a            16
027070071025     c                   movel     w016a         v11cpd
027080071025     C                   CLEAR                   V11isd
027090071025    3c                   endif
027100071025    2c                   endif
027110061107     c*
027120061107    1c                   endif
027130941019     C**
027140941027     C* F N A R B D 0 F
027150941019     C**
027160941019    1C     FLGCVR        IFEQ      'I'
027170941019     C*
027180941019    2C     VIDCVR        IFEQ      'I0'
027190140407    2C     VIDCVR        oreQ      'I7'
027200140407    2C     VIDCVR        oreQ      'I8'
027210140407    2C     VIDCVR        oreQ      'I9'
027220941019     C                   SETON                                        01          DESTINATAR
027230941019     C                   ELSE
027240941019     C                   SETON                                        02          PESO VOL
027250941212    2C                   END
027260941025     C*
027270941025     C* 2 RAGIONE SOCIALE DESTINATARIO
027280941212     C                   MOVEL     'D'           WTRC
027290060222     C     KARB2         CHAIN(N)  Fiar4000                           30
027300941212     C     *IN30         IFEQ      *OFF
027310941212     C                   MOVEL     'E'           WAR4D             1
027320941212     C* LO SALVO PER LA STORICIZZAZIONE
027330970520     C                   MOVE      AR4NOT        SAVRD2
027340941212     C                   ENDIF
027350061116     C*
027360061116     C* codice fiscale destinatario
027370061116     C                   MOVEL     'Q'           WTRC
027380061116     C     KARB2         CHAIN(N)  Fiar4000                           30
027390061116     C     *IN30         IFEQ      *OFF
027400061116     C                   MOVEL     'E'           WAR4Q
027410061116     C* Se c'e' il cod fiscale del detinatario lo memorizzo
027420061116     C                   MOVE      AR4NOT        vidcdf
027430061116     c                   if        vidcdf<>*blanks
027440061116     C                   MOVEL     'D'           WAR4Q
027450061116     C                   ENDIF
027460061116     C                   ENDIF
027470150206
027480150206     C* LOCALITA' NORMALIZZATA
027490150206     C                   MOVEL     'X'           WTRC
027500150206     C     KARB2         CHAIN(N)  Fiar4000                           30
027510150206     C     *IN30         IFEQ      *OFF
027520150206     C                   MOVEL     'E'           WAR4X             1
027530150206     C                   ENDIF
027540061116     c
027550051129     C* VISUALIZZO ANCHE IL VOLUME VDL
027560941214     C                   MOVEL     ARBVLC        V1CVLC
027570941214     C                   MOVEL     ARBNCR        V1CNCR
027580031120
027590040123      * Reperisco il referente e il telefono e magazzino giacenza
027600070910     c                   If        wesar5gen='S'
027610070910     c                   Eval      VidRef =  PAr5Ref
027620070910     c                   Eval      VidTeld = PAr5Teld
027630070910     c                   Eval      VidZng =  PAr5Zng
027640031120     c                   EndIf
027650141203     c                   eval      savzng = vidzng
027660941019     C*
027670941019     C* PGM NON RICHIAMATO
027680941019    2C     *IN20         IFEQ      *OFF
027690970520     C                   CLEAR                   VIDRSD
027700941025     C                   MOVEL     ARBRSD        VIDRSD
027710941027     C     WAR4D         IFEQ      'E'
027720061116     C                   MOVE      savrd2        VIDRSD
027730941025     C                   ENDIF
027740941019     C                   MOVEL     ARBIND        VIDIND
027750941019     C                   MOVEL     ARBLOD        VIDLOD
027760941019     C                   MOVEL     ARBPRD        VIDPRD
027770941019     C                   MOVEL     ARBCAD        VIDCAD
027780061115     c
027790140428     c*05 off se --> bolla singola in assegnato
027800140428     c*              bolla doppia
027810071025    3C     *IN05         IFEQ      *OFF
027820140428     c* se bolla doppia in partenza la partita iva che ho caricato non è quella del
027830140428     c* destinatario e quindi la carico solo se sono in arrivo (10 on)
027840151222     c***  *in10         ifeq      *on
027850151222     c     wfile         ifeq      'A'
027860140428     c     §3atb2        oreq      *blanks
027870941220     C                   MOVEL     ARBCPI        VIDPID
027880140428     c                   endif
027890071025     C**   'PRIVATO'     SCAN      vidpid        posiz                    30
027900050922     c* solo numeri
027910050922     c* "PRIVATO" senza iso
027920071025    4c**   *in30         ifeq      *off
027930071025     c**   vidisd        andge     *zeros
027940071025     c**   *in30         oreq      *on
027950071025     c**   posiz         andlt     3
027960071025     C**                 CLEAR                   VIDPID
027970071025     c**                 movel     arbcpi        vidcpd
027980071025    4c**                 endif
027990071025    3C                   ENDIF
028000941220     C*
028010030122     c                   Z-Add     ArbPkc        VidPkc
028020941027     C                   MOVEL     ARBPKF        VIDPKF
028030941027     C                   MOVEL     ARBVLF        VIDVLF
028040941027     C                   MOVEL     ARBFVF        VIDFVF
028050941019     C                   MOVEL     ARBFIN        VIDFIN
028060941019     C                   MOVEL     ARBNZD        VIDNZD
028070941019     C* FERMO DEPOSITO
028080941019    3C     ARBFFD        IFEQ      'S'
028090941019     C                   MOVEL     'SI'          VIDFFD
028100941019     C                   ELSE
028110941019     C                   CLEAR                   VIDFFD
028120941019    3C                   ENDIF
028130941019     C**
028140941019   X2C                   ELSE
028150941019     C**
028160941019     C* PROGRAMMA     RICHIAMATO: PRENDO CAMPI DA DSARBD
028170941019     C                   MOVEL     §BDRSD        VIDRSD
028180970520     C                   MOVE      §BDRD2        VIDRSD
028190941019     C                   MOVEL     §BDIND        VIDIND
028200941019     C                   MOVEL     §BDLOD        VIDLOD
028210941019     C                   MOVEL     §BDPRD        VIDPRD
028220941019     C                   MOVEL     §BDCAD        VIDCAD
028230941019     C                   MOVEL     §BDNZD        VIDNZD
028240060308     c                   movel     §bdref        vidref
028250060308     c                   movel     §bdtel        vidteld
028260061115     c                   clear                   vidcdf
028270071025    3C     *IN05         IFEQ      *OFF
028280950102     C                   MOVEL     §BDCPI        VIDPID
028290071025     C**   'PRIVATO'     SCAN      vidpid        posiz                    30
028300050922     c* solo numeri
028310050922     c* "PRIVATO" senza iso
028320071025    4c**   *in30         ifeq      *off
028330071025     c**   vidisd        andge     *zeros
028340071025     c**   *in30         oreq      *on
028350071025     c**   posiz         andlt     3
028360071025     C**                 CLEAR                   VIDPID
028370071025     c**                 movel     §bdcpi        vidcpd
028380071025    4c**                 endif
028390071025    3C                   ENDIF
028400941019     C                   MOVEL     §BDFIN        VIDFIN
028410941027     C                   MOVEL     §BDPKF        VIDPKF
028420941027     C                   MOVEL     §BDVLF        VIDVLF
028430941209     C                   MOVEL     §BDFVF        VIDFVF
028440941019     C* FERMO DEPOSITO
028450141017     c* §BDFFD= 'S' => si fermo deposito; Si alert
028460141017     c* §BDFFD= 'Y' -->si fermo deposito; no alert
028470141017     c* Serve per le chiamate cieche in quanto il campo a video per la gestione
028480141017     c* del contatto (VIDTFD) non è presente nella dsarbd.
028490941019    3C     §BDFFD        IFEQ      'S'
028500141017    3C     §BDFFD        oreq      'Y'
028510941019     C                   MOVEL     'SI'          VIDFFD
028520941019     C                   ELSE
028530941019     C                   CLEAR                   VIDFFD
028540941019    3C                   ENDIF
028550941216    2C                   ENDIF
028560141017      *
028570141017     c                   if        §bdffd='Y'
028580141017     c                   eval      vidtfd='SI'
028590141017     c                   endif
028600071025     c
028610071025     c* controllo Partita IVA se privato
028620071025    2c                   if        vidpid<>*blanks
028630071025     C     'PRIVATO'     SCAN      vidpid        posiz                    30
028640071025     c* solo numeri
028650071025     c* "PRIVATO" senza iso
028660071025    3c     *in30         ifeq      *off
028670071025     c     vidisd        andge     *zeros
028680071025     c     *in30         oreq      *on
028690071025     c     posiz         andlt     3
028700071025     c                   movel     vidpid        w016a
028710071025     c                   movel     w016a         vidcpd
028720071025     C                   CLEAR                   VIDisd
028730071025    3c                   endif
028740071025    2c                   endif
028750991001     C*
028760991001    2C     *IN01         IFEQ      *ON
028770970808     C* MEMORIZZO CAP/PROVINCIA E LOCALITA' DEL DESTINATARIO
028780970808     C* (MI SERVIRA' NELLA ROUTINE DEI CONTROLLI
028790970808     C*
028800970808     C                   MOVE      VIDCAD        SAVCAD                         CAP
028810970808     C                   MOVE      VIDCAD        VARCAD                          "
028820970808     C                   MOVE      VIDCAD        WCAD                            "
028830970808     C                   MOVE      VIDCAD        WCAD1                           "
028840970808     C                   MOVE      VIDLOD        SAVLOD                         LOCALITA'
028850970808     C                   MOVE      VIDLOD        VARLOD                            "
028860970808     C                   MOVE      VIDLOD        WLOD                              "
028870970808     C                   MOVE      VIDLOD        WLOD1                             "
028880060111     c* memorizzo anche gli altri dati del destinatario
028890060111     c                   move      vidrsd        savrsd
028900061116     c                   move      vidrsd        savrsdQ
028910060111     c                   move      vidrsd        wrsd1
028920060111     c                   move      vidind        savind
028930060111     c                   move      vidind        wind1
028940060111     c                   move      vidprd        savprd
028950060111     c                   move      vidprd        wprd1
028960060111     c                   move      vidnzd        savnzd
028970060111     c                   move      vidnzd        wnzd1
028980970808     C*
028990950419     C* SE P.IVA DEL VIDEO = BLANKS LA PRENDO DALL'ESTENSIONE SE
029000950419     C* ESISTE
029010950419     C     VIDPID        IFEQ      *BLANKS
029020950419     C                   MOVEL     'P'           WTRC
029030060222     C     KARB2         CHAIN(N)  Fiar401L                           30
029040950419     C     *IN30         IFEQ      *OFF
029050950419     C                   MOVE      AR4NOT        VIDPID
029060050922     C     'PRIVATO'     SCAN      vidpid        posiz                    30
029070050922     c* solo numeri
029080050922     c* "PRIVATO" senza iso
029090050922     c     *in30         ifeq      *off
029100050922     c     vidisd        andge     *zeros
029110050922     c     *in30         oreq      *on
029120050922     c     posiz         andlt     3
029130050922     C                   CLEAR                   VIDPID
029140050922     c                   move      ar4not        wpiva
029150050922     C                   MOVEL     WPIVA         VIDCPD
029160050922     c                   endif
029170050922     C**   VIDISD        IFGE      *ZEROS
029180050922     C**                 CLEAR                   VIDPID
029190050922     C**                 MOVE      AR4NOT        WPIVA
029200050922     C**                 MOVEL     WPIVA         VIDCPD
029210050922     C**                 ENDIF
029220950419     C                   END
029230950419     C                   END
029240950419     C*
029250160118     C                   SETOFF                                       717276
029260991001     C* SE ASSEGNATO 9999 GIA' CONTABILIZZATO PROTEGGO DATI
029270991001     C* DESTINATARIO (ANCHE LA P.IVA MA SOLO SE DIVERSA DA BLANKS)
029280991001     C                   MOVE      ARBKSC        CO1KSC            4 0
029290991001    3C     CO1KSC        IFEQ      9999
029300991001     C                   CLEAR                   AR6DFT
029310991001     C                   MOVEL     '1'           WTRC
029320991001     C     KARB2         CHAIN(N)  FIAR6000                           30
029330991001    4C     AR6DFT        IFNE      *ZEROS
029340000104     C     AR6DFT        ANDLE     WDTU10
029350991001     C                   SETON                                        71
029360000104    5C     VIDPID        IFNE      *BLANKS
029370000104     C                   SETON                                        72
029380991001    5C                   END
029390991001    4C                   END
029400991001    3C                   END
029410160118     c* Proteggo la partita iva/cod.fiscale anche se dagli arrivi sto manutenzionando BLP
029420160115     c                   if        *in10 and wfile='P'
029430160118     C                   SETON                                        7276
029440160115     c                   endif
029450991001    2C                   END
029460941019     C*
029470941222     C* PROTEGGO VOLUME SE NON VARIABILE
029480941216     C* SE SI TRATTA DI VARIAZIONE DI PESO, PROTEGGO SEMPRE IL VOLUME
029490941216    2C     D48CVB        IFEQ      'I2'
029500941216     C* PROTEGGO IL PESO
029510941216     C                   SETON                                        88
029520941216     C*
029530941019     C                   MOVEL     '7T'          COD
029540051214     C                   MOVEL(P)  VIDFVF        KEY
029550941212     C                   EXSR      CTABEL
029560941212     C   30              SETON                                        89
029570941222     C     *IN30         IFEQ      *OFF
029580941222     C                   MOVEL     TBLUNI        DS7T
029590091106     c
029600151222     c                   if        *in10
029610091106     c                   eval      wvariavol=§7tvra
029620120705     c                   eval      wvariavolEXP=§7tvraEXP
029630091106     c                   else
029640091106     c                   eval      wvariavol=§7tvrp
029650120705     c                   eval      wvariavolEXP=§7tvrPEXP
029660091106     c                   endif
029670091106     c
029680091106     c* Proteggo il volume in arrivo o in partenza se il flag è ' ' oppure
029690091124     c*  non corrisponde il tipo bolla
029700091106     c                   select
029710091106     c                   when      wvariavol=' '
029720091106     C                   SETON                                        89
029730091106     c                   when      Wvariavol='A' and flgtb1='F'
029740091106     C                   SETON                                        89
029750091106     c                   when      Wvariavol='F' and flgtb1='A'
029760091106     C                   SETON                                        89
029770091106     c                   endsl
029780091106     c
029790120705     C* Se  BOLLA EXPORT,  in aggiunta controllo il relativo flag
029800120705     c                   if        *in89 and (lnantw='FED' or
029810120705     c                                        lnantw='EEX' or lnantw='DPD')
029820120705     c                   select
029830120705     c                   when      WvariavolEXP='3'
029840120705     C                   SETOff                                       89
029850120705     c                   when      WvariavolEXP='A' and flgtb1='A'
029860120705     C                   SETOff                                       89
029870120705     c                   when      Wvariavolexp='F' and flgtb1='F'
029880120705     C                   SETOff                                       89
029890120705     c                   endsl
029900120705     c                   endif
029910091106     c
029920941222    3C                   ENDIF
029930941216     C*
029940941216   X2C                   ELSE
029950941216     C                   SETON                                        89
029960941216    2C                   ENDIF
029970030604
029980030604      * se linea arrivo FedEx e il volume è "T" proteggo sempre
029990030604      * (già da tabella "7T" il volume di tipo "T" non è sostituibile,
030000030604      *  faccio lo stesso la modifica nel caso di variazione della tabella)
030010030604     c                   If        *In02 and LnaNtw = 'FED' and vidfvf = 'T'
030020030604     c                   Eval      *In89 = *On
030030030604     c                   EndIf
030040941019    1C                   ENDIF
030050130930     c
030060130930     C**
030070130930     C* F N A R B N 0 F
030080130930     C**
030090130930    1C     FLGCVR        IFEQ      'N'
030100130930
030110130930      * Reperisco il referente e il telefono e magazzino giacenza
030120130930    2c                   if        not  *in20
030130130930
030140130930    3c                   if        d48cvb='NE'
030150131001     c                   seton                                        868483
030160130930     c                   If        wesar5gen='S'
030170130930     c                   Eval      VidRef =  PAr5Ref
030180130930     c                   Eval      VidTeld = PAr5Teld
030190130930     c                   EndIf
030200130930     c                   if        wesar5emd='S'
030210130930     c                   eval      videml = emd_§ar5eml
030220130930     c                   eval      vidtsms= emd_§ar5tel
030230130930     c                   endif
030240130930    3c                   endif
030250130930
030260130930    3c                   if        d48cvb='NT'
030270131001     c                   seton                                        81
030280130930     c                   eval      vidnt1=v1cnt1
030290130930     c                   eval      vidnt2=v1cnt2
030300130930    3c                   endif
030310130930     c
030320130930   x2c                   else
030330130930    3c                   select
030340130930     c
030350131002     c                   when      §bntrd='NOT'
030360131002     c                   if        §bnu_sn='S'
030370131001     c                   seton                                        81
030380130930     c                   movel     §bnuni70      vidnt1
030390130930     c                   move      §bnuni70      vidnt2
030400131002     c                   else
030410131002     C                   MOVEL     MSG(24)       D48MSG
030420131002     C                   MOVEL     '1'           D48ERR
030430131002     c                   leavesr
030440131002     c                   endif
030450131008     c* se la causale variazione non è NT --> errore
030460131008     c                   if        d48cvb<>'NT'
030470131008     C                   MOVEL     MSG(31)       D48MSG
030480131008     C                   MOVEL     '1'           D48ERR
030490131008     c                   leavesr
030500131008     c                   endif
030510131008
030520130930     c                   other
030530131008
030540131008     c* se la causale variazione non è NE --> errore
030550131008     c                   if        d48cvb<>'NE'
030560131008     C                   MOVEL     MSG(31)       D48MSG
030570131008     C                   MOVEL     '1'           D48ERR
030580131008     c                   leavesr
030590131008     c                   endif
030600130930     c
030610131001     c                   if        §bnu_sn='S'
030620130930     c                   eval      videml = §bnuni70
030630131001     c                   seton                                        86
030640131001     c                   endif
030650131001     c                   if        §bnr_sn='S'
030660131001     c                   eval      vidteld= §bntel
030670130930     c                   eval      vidref = §bnref
030680131001     c                   seton                                        84
030690131001     c                   endif
030700131001     c                   if        §bnc_sn='S'
030710131001     c                   eval      vidtsms= §bncel
030720131001     c                   seton                                        83
030730131001     c                   endif
030740131002     c                   if        *in86=*off and *in84=*off and *in83=*off
030750131002     C                   MOVEL     MSG(24)       D48MSG
030760131002     C                   MOVEL     '1'           D48ERR
030770131002     c                   leavesr
030780131002     c                   endif
030790131001     c
030800130930    3c                   endsl
030810130930     c
030820130930    2c                   endif
030830130930    1c                   endif
030840950104     C**
030850990930     C* F I A R B T 0 F
030860950104     C**
030870100120     c
030880941019    1C     FLGCVR        IFEQ      'T'
030890941019     C                   SELECT
030900941019     C*
030910941102     C     D66RTA        WHENEQ    'S'
030920950616     C                   SETON                                        1529        RITASSAZ
030930941102     C*
030940941102     C     D66BSF        WHENEQ    'S'
030950941019     C                   SETON                                        13          VARIA  SF
030960941019     C*
030970941102     C     D66BFI        WHENEQ    'S'
030980991006     C                   SETON                                        1407        VARIA  FAT
030990941019     C*
031000941019     C                   OTHER
031010941019     C                   SETON                                        36          TASS FRANC
031020941019     C                   ENDSL
031030941102     C* PULIZIA SFL
031040941102     C                   SETON                                        85
031050941215     C                   WRITE     LR48C03
031060941102     C                   SETOFF                                       85
031070941215     C*
031080941215     C                   MOVEL     *BLANKS       DESKSC
031090950104     C   10              MOVEL     ARBRSD        W008A             8
031100950104     C   10              MOVEL     W008A         DESKSC
031110950104     C  N10              MOVEL     ARBRSM        DESKSC
031120131120     c   10              clear                   sav_ift
031130941102     C*
031140941102     C*
031150990930     C* TASSAZIONE IN FIAR6
031160990930     C                   MOVEL     '1'           WTRC
031170990930     C     KARB2         CHAIN     FIAR6000                           30
031180990702    2C     *IN30         IFEQ      *OFF
031190990701     C                   MOVEL     'E'           WAR6
031200990701     C                   MOVEL     AR6NFT        VIDNFT
031210990701     C                   MOVEL     AR6FIV        VIDFIV
031220990702     C                   Z-ADD     AR6IMV        SAVIMV
031230151016     C                   Z-ADD     AR6IMV        SAVIMV_fat
031240131120     c   10              z-add     ar6ift        sav_ift
031250990702     C* PROTEZIONE DATI FATTURA PER PREPAGATI
031260990702    3C  N10VIDNFT        IFLT      §QTNFT
031270990719     C     VIDNFT        ANDGT     0
031280990701     C                   SETON                                        23
031290990702     C                   Z-ADD     AR6IFT        VIDIFT
031300990702    3C                   ENDIF
031310991001     C* PROTEZIONE DIVISA PER PREPAGATI
031320991001    3C  N10VIDNFT        IFGT      0
031330991012     C                   SETON                                        0708
031340991001    3C                   ENDIF
031350991108     C* SE PREPAGATO O P.A. FATTURA GIA'CONTAB. MODIFICO SOLO IMP.ASSIC
031360991008     C* E VALORE MERCE DICHIARATO
031370991108    3C     AR6DFT        IFGT      *ZEROS
031380000104     C     AR6DFT        ANDLE     WDTU10
031390991008     C                   SETON                                        2973
031400991008    3C                   ENDIF
031410991029    2C                   ENDIF
031420991029     C**
031430991029     C* SE ASSEGNATO GIA' INCASSATO, FACCIO MODIFICARE SOLO IMP.ASSIC EARIAZIONE
031440991029     C*  VALORE MERCE DICHIARATO
031450130409    1C*  10FLGCVR        IFEQ      'T'
031460130409    2C*    §7LFIN        IFEQ      'S'
031470130409     C*    ARBICA        ANDNE     ' '
031480130409     C*    ARBICA        ANDNE     'R'
031490991029     C* PROTEGGO I CAMPI CHE NON DEVONO ESSERE MANUTENZIONATI
031500130409     C*                  SETON                                        2973
031510130409    2C*                  END
031520991108     C* SE ICA = 'R' E C'E' INCASSO PARZ<IALE IN ARI, PROTEGGO
031530991108     C*  LO STESS, FACCIO SOLO MODIFICARE IAS
031540130409    2C*    ARBICA        IFEQ      'R'
031550130409     C*    KARI          CHAIN     FIARI01L                           30
031560130409    3C* N30ARIATB        IFEQ      ' '
031570991108     C* PROTEGGO I CAMPI CHE NON DEVONO ESSERE MANUTENZIONATI
031580130409     C*                  SETON                                        2973
031590130409    3C*                  ENDIF
031600130409    2C*                  ENDIF
031610130409    1C*                  ENDIF
031620130410     c* PROTEZIONE CAMPI PER POTER VARIARE SOLO IAS E VMD
031630130410     c* Se bolla già contabilizzata e variabile anche dopo contabilizzazione
031640130410     c* oppure se bolla già incassata e variabile anche dopo incasso (V.di fnlr66r)
031650130410     c                   if        d66bdi='S'
031660130409     C                   SETON                                        2973
031670130409     c                   endif
031680990701     C**
031690950807     C* VEDO SE ESISTE AMCHE L'AR7
031700990930     C     KARB2         SETLL     FIAR7000                               30
031710950807     C   30              MOVEL     'E'           WAR7
031720060201     c
031730060201      * Calcolo data limite mod. importo da assicurare
031740060201     c     *iso          Move      ArbDsp        wdataiso
031750060201     c                   adddur    §vpomia:*d    wdataiso
031760060201      *
031770060201     c                   do        *hival
031780060201      * verifico se è un giorno lavorativo
031790060201     c     *iso          move      wdataiso      ds_data
031800060201     C                   eval      kann = ds_anno
031810060201     C                   eval      kmes = ds_mese
031820060201     C     kazcln        chain     azcln01l
031830060201     C                   if        %found(azcln01l)
031840060201     C                   if        MAT(ds_giorno) =  'F'  or
031850060201     C                             POM(ds_giorno) =  'F'
031860060201      * se non va bene aggiungo 1 giorno
031870060201     c                   adddur    1:*d          wdataiso
031880060201     C                   iter
031890060201     c                   else
031900060201     c                   leave
031910060201     C                   endif
031920060201     c                   else
031930060201     c                   leave
031940060201     C                   endif
031950060201
031960060201     c                   enddo
031970060201
031980060201     c                   eval      wdatalimias=wdataiso
031990080625     c                   clear                   forzaias2
032000080625     c                   clear                   forzaias
032010060201     c
032020060201     c* IAS modificabile solo nei primi 2 gg  tassaz sempre
032030060201     c                   if        §utemtf=' '  and
032040060201     c                             wdataoggi>wdatalimIas
032050060201     c                   seton                                        75
032060060201     c                   endif
032070060201     c* solo IAs ma sempre
032080060201     c                   if        §utemtf='I'
032090060201     c                   seton                                        74
032100060201     c                   endif
032110060201     c* Nulla
032120060201     c                   if        §utemtf='N'
032130060201     c                   seton                                        7475
032140060201     c                   endif
032150991012     C*
032160060201     C*
032170060201     C                   CLEAR                   V3CEV1
032180060201     C                   CLEAR                   V3CSV1
032190060201     C                   CLEAR                   V3CVA1
032200060201     C                   CLEAR                   VARVA1
032210060201     C                   Z-ADD     1             NRR
032220060201     C                   DO        20            NRR
032230060201     C                   WRITE     LR48S03
032240060201     C                   ENDDO
032250920128     C* PROGRAMMA NON RICHIAMATO: DATI DA BOLLA
032260941019    2C     *IN20         IFEQ      *OFF
032270941019     C*
032280941019     C                   Z-ADD     ARBIAS        VIDIAS
032290950426     C                   Z-ADD     ARBIAS        COMIAS
032300941209     C                   MOVEL     ARBVAS        VIDVAS
032310941215     C                   MOVEL     ARBVAS        VARVAS
032320941019     C                   MOVEL     ARBKSC        VIDKLP
032330950424     C                   MOVEL     ARBKSC        VARKLP
032340941019     C                   MOVE      ARBKSC        VIDKSC
032350970822     C                   MOVE      ARBKSC        MEMKSC
032360950424     C                   MOVE      ARBKSC        VARKSC
032370941019     C                   MOVEL     ARBCTR        VIDCTR
032380050202     C                   MOVEL     ARBCTR        SAVCTR
032390060511     C                   MOVEL     ARBCTR        CTRBOLLA          3
032400941215     C                   MOVEL     ARBQFT        VIDQFT
032410950424     C                   MOVEL     ARBQFT        VARQFT
032420941215     C                   MOVEL     ARBVMD        VIDVMD
032430941215     C                   MOVEL     ARBVAD        VIDVAD
032440941215     C                   MOVEL     ARBVAD        VARVAD
032450990930     C* TASSAZIONE IN FIAR6
032460941102    3C     WAR6          IFEQ      'E'
032470990930     C                   MOVEL     AR6DIV        VIDDIV
032480991006     C                   MOVEL     AR6DIV        WDIV
032490941019     C                   MOVEL     AR6POR        VIDPOR
032500991006     C                   MOVEL     AR6POR        VARPOR
032510941019     C                   MOVEL     AR6IMV        VIDIMV
032520941019     C                   MOVEL     AR6CEI        VIDCEI
032530941102     C*
032540941102     C                   Z-ADD     0             NRR
032550941209     C                   MOVEL     AR6SV1        WSV1
032560941209     C                   Z-ADD     AR6VA1        WVA1
032570941102     C                   EXSR      CARVAR
032580941209     C                   MOVEL     AR6SV2        WSV1
032590941209     C                   Z-ADD     AR6VA2        WVA1
032600941102     C                   EXSR      CARVAR
032610941212    4C     AR6SV3        IFNE      ' '
032620941209     C                   MOVEL     AR6SV3        WSV1
032630941209     C                   Z-ADD     AR6VA3        WVA1
032640941102     C                   EXSR      CARVAR
032650941102     C*
032660941102     C* VISTO CHE C'E' LA 3 VARIA VEDO SE CE NE SONO ANCORA
032670990930     C     KARB2         READE(N)  FIAR7000                               30
032680941102    5C     *IN30         DOWEQ     *OFF
032690941209     C                   MOVEL     AR7SVN        WSV1
032700941209     C                   MOVEL     AR7VAN        WVA1
032710941102     C                   EXSR      CARVAR
032720990930     C     KARB2         READE(N)  FIAR7000                               30
032730941102    5C                   ENDDO
032740941102    4C                   ENDIF
032750941102    3C                   ENDIF
032760941019     C*
032770941019   X2C                   ELSE
032780920128     C*
032790990930     C* PROGRAMMA     RICHIAMATO: DATI DA DARBT
032800941025     C                   Z-ADD     §BTIAS        VIDIAS
032810950426     C                   Z-ADD     §BTIAS        COMIAS
032820941019     C                   MOVEL     §BTVAS        VIDVAS
032830941220     C                   MOVEL     §BTVAS        VARVAS
032840941209     C                   MOVEL     §BTQFT        VIDQFT
032850950424     C                   MOVEL     §BTQFT        VARQFT
032860941209     C                   MOVEL     §BTVMD        VIDVMD
032870941209     C                   MOVEL     §BTVAD        VIDVAD
032880941220     C                   MOVEL     §BTVAD        VARVAD
032890920128     C                   MOVEL     §BTKSC        VIDKLP
032900950424     C                   MOVEL     §BTKSC        VARKLP
032910920128     C                   MOVE      §BTKSC        VIDKSC
032920970822     C                   MOVE      §BTKSC        MEMKSC
032930950424     C                   MOVE      §BTKSC        VARKSC
032940920128     C                   MOVEL     §BTKSC        SAVKSC            7 0
032950060511     c                   clear                   kscbolla
032960920128     C                   MOVEL     §BTCTR        VIDCTR
032970050202     C                   MOVEL     §BTCTR        SAVCTR
032980060511     c                   clear                   ctrbolla
032990990930     C                   MOVEL     §BTDIV        VIDDIV
033000991006     C                   MOVEL     §BTDIV        WDIV
033010990930     C                   Z-ADD     §BTPOR        VIDPOR
033020991006     C                   Z-ADD     §BTPOR        VARPOR
033030990930     C                   Z-ADD     §BTIMV        VIDIMV
033040920128     C                   MOVEL     §BTCEI        VIDCEI
033050941102     C* CARICO LE VARIE
033060941102     C                   Z-ADD     0             NRR               5 0
033070941209     C                   DO        20            C                 3 0
033080941209     C                   MOVEL     §SV(C)        WSV1
033090941209     C                   Z-ADD     §VA(C)        WVA1
033100941102     C                   EXSR      CARVAR
033110941209     C                   ENDDO
033120941019    2C                   ENDIF
033130131016
033140131016     c                   z-add     vidias        bol_vidias
033150131016     c                   movel     vidvas        bol_vidvas
033160131016     c                   z-add     vidvmd        bol_vidvmd
033170131016     c                   movel     vidvad        bol_vidvad
033180131016     c
033190941215     C*
033200941215     C* CARICO LE TARIFFE E DEOCIFICO IL CTR CHE C'E'I
033210050202     C                   Z-ADD     SAVKSC        KSC
033220941215     C                   EXSR      CARCTR
033230941019     C*
033240920128     C* DECODIFICA ESENZIONE IVA
033250920128     C                   MOVEL     *BLANKS       DESCEI
033260941019    2C     VIDCEI        IFNE      ' '
033270920128     C                   MOVEL     'EI'          COD
033280920128     C                   MOVEL     *BLANKS       KEY
033290051214     C                   MOVEL(P)  VIDCEI        KEY
033300941212     C                   EXSR      CTABEL
033310920128     C  N30              MOVEL     TBLUNI        DESCEI
033320941019    2C                   END
033330920128     C* GIRO DATA FATTURA
033340941102    1C     WAR6          IFEQ      'E'
033350941102     C     AR6DFT        ANDGT     0
033360941019     C                   Z-ADD     AR6DFT        G02INV
033370941019     C                   MOVEL     '3'           G02ERR
033380941019     C                   CALL      'XSRDA8'
033390941019     C                   PARM                    WLBDAT
033400941019     C*
033410941019     C                   Z-ADD     G02DAT        VIDDFT
033420941019    2C                   ENDIF
033430941019    1C                   ENDIF
033440941019     C**
033450991001     C* F I A R B T 0 F   SECONDA BOLLA
033460941019     C**
033470911211     C     FLGCVR        IFEQ      '2'
033480941102     C                   SELECT
033490941102     C     D66RTA        WHENEQ    'S'
033500950616     C                   SETON                                        1529        RITASSAZ
033510941102     C*
033520941102     C     D66BSF        WHENEQ    'S'
033530941102     C                   SETON                                        13          VARIA  SF
033540941102     C*
033550941102     C     D66BFI        WHENEQ    'S'
033560991006     C                   SETON                                        1407        VARIA  FAT
033570941102     C*
033580941102     C                   ENDSL
033590911211     C*
033600911219     C                   MOVEL     *BLANKS       DESKSC
033610941019     C                   MOVEL     ARBRSD        W008A             8
033620941019     C                   MOVEL     W008A         DESKSC
033630131120     c                   clear                   sav_ift
033640941019     C*
033650941019     C* PGM NON RICHIAMATO
033660941019    2C     *IN20         IFEQ      *OFF
033670920128     C* NON ESISTE ESTENSIONE BOLLA
033680990930    3C     WAR62         IFEQ      ' '
033690911211     C                   MOVEL     ARBLNA        VIDKLP
033700911211     C                   MOVEL     9999          VIDKSC
033710911212     C                   MOVEL     *ZEROS        VIDCTR
033720050202     C                   MOVEL     *ZEROS        SAVCTR
033730060511     c                   clear                   ctrbolla
033740991013     C                   MOVE      *ZEROS        SAVKSC            7 0
033750060511     c                   clear                   kscbolla
033760941019     C*
033770941019   X3C                   ELSE
033780941019     C*
033790131120     c                   z-add     ar6ift        sav_ift
033800990930     C                   MOVEL     AR6DIV        VI2DIV
033810020305     C                   MOVEL     AR6DIV        WDIV
033820990930     C                   MOVEL     AR6SV1        VI2SV1
033830040122     C**!!!              Z-ADD     AR6VA1        VI2VA1
033840040122     C**!!!              Z-ADD     AR6VA1        VA2VA1
033850040123      * carico le varie
033860040122     c                   Movel     Ar6Sv1        wsv1
033870040122     c                   Z-add     Ar6Va1        wva1
033880040122     c                   ExSr      CarVar1
033890040122     c                   Movel     Ar6Sv2        wsv1
033900040122     c                   Z-add     Ar6Va2        wva1
033910040122     c                   ExSr      CarVar1
033920040122     c                   Movel     Ar6Sv3        wsv1
033930040122     c                   Z-add     Ar6Va3        wva1
033940040122     c                   ExSr      CarVar1
033950040122      * Verifico se ci sono altre varie
033960040126     c                   Movel     '2'           wtrc
033970040123     c     Karb2         Setll     Fiar701l
033980040122Do  4c                   Do        *Hival
033990040123     c     Karb2         Reade(n)  Fiar701l
034000040122     c                   If        %Eof(Fiar701l)
034010040122     c                   Leave
034020040122     c                   EndIf
034030040122     c                   Movel     Ar7Svn        wsv1
034040040122     c                   Z-add     Ar7Van        wva1
034050040122     c                   ExSr      CarVar1
034060040122    4c                   EndDo
034070990930     C                   MOVEL     AR6CEI        VI2CEI
034080990930     C                   MOVEL     AR6KSC        VIDKLP
034090990930     C                   MOVE      AR6KSC        VIDKSC
034100991013     C                   MOVEL     AR6KSC        SAVKSC
034110060511     C                   MOVEL     AR6KSC        kscbolla
034120990930     C                   MOVEL     AR6CTR        VIDCTR
034130050202     C                   MOVEL     AR6CTR        SAVCTR
034140060511     C                   MOVEL     AR6CTR        ctrbolla
034150941019    3C                   END
034160941019     C*
034170941019   X2C                   ELSE
034180941019     C*
034190990930     C* PROGRAMMA     RICHIAMATO: PRENDO DATI DA DARBT 2BOLLA
034200990930     C                   MOVEL     §BTDIV        VI2DIV
034210991006     C                   MOVEL     §BTDIV        WDIV
034220040123     C**!!!              MOVEL     §BTSV1        VI2SV1
034230040123     C**!!!              Z-ADD     §BTVA1        VI2VA1
034240040123     C**!!!              Z-ADD     §BTVA1        VA2VA1
034250040123      * carico le varie
034260040123     c                   Do        20            c
034270040123     c                   Movel     §sv(c)        wsv1
034280040123     c                   Z-add     §va(c)        wva1
034290040123     c                   ExSr      CarVar1
034300041130     c                   If        ContaSv = 6
034310040123     c                   Leave
034320040123     c                   EndIf
034330040123     c                   EndDo
034340040123
034350990930     C                   MOVEL     §BTCEI        VI2CEI
034360990930     C                   MOVEL     §BTKSC        VIDKLP
034370990930     C                   MOVE      §BTKSC        VIDKSC
034380991013     C                   MOVE      §BTKSC        SAVKSC
034390060511     c                   clear                   kscbolla
034400990930     C                   MOVEL     §BTCTR        VIDCTR
034410050202     C                   MOVEL     §BTCTR        SAVCTR
034420060511     c                   clear                   ctrbolla
034430941019    2C                   END
034440050202     c
034450050202     C* CARICO LE TARIFFE E DEOCIFICO IL CTR CHE C'E'I
034460050202     C                   Z-ADD     SAVKSC        KSC
034470050202     C                   EXSR      CARCTR
034480920128     C*
034490920128     C                   MOVEL     *BLANKS       DESCEI
034500941019    2C     VI2CEI        IFNE      ' '
034510911211     C                   MOVEL     'EI'          COD
034520911211     C                   MOVEL     *BLANKS       KEY
034530051214     C                   MOVEL(P)  VI2CEI        KEY
034540941212     C                   EXSR      CTABEL
034550911211     C  N30              MOVEL     TBLUNI        DESCEI
034560941019    2C                   END
034570941019     C* GIRO DATA FATTURA
034580990930    1C     WAR62         IFEQ      'E'
034590990930     C     AR6DFT        ANDGT     0
034600990930     C                   Z-ADD     AR6DFT        G02INV
034610941019     C                   MOVEL     '3'           G02ERR
034620941019     C                   CALL      'XSRDA8'
034630941019     C                   PARM                    WLBDAT
034640941019     C*
034650941019     C                   Z-ADD     G02DAT        VI2DFT
034660991007     C                   Z-ADD     AR6FIV        VI2FIV
034670991007     C                   Z-ADD     AR6NFT        VI2NFT
034680941019    2C                   ENDIF
034690911211     C                   END
034700941019     C**
034710050202     C* F N A R B K 0 F
034720941019     C**
034730941019    1C     FLGCVR        IFEQ      'K'
034740941019     C*
034750941019     C* CODICE BOLLA PRECEDENTE
034760941019     C                   MOVEL     ARBCBO        OLDCBO
034770941019     C* SALVO CAMPI DEL CODICE BOLLA CHE MI INTERESSANO
034780941019     C                   MOVEL     §3ATB1        SAVTB1            2
034790941102     C                   MOVEL     §3ARBL        SAVRBL            1
034800941019     C                   MOVEL     §3ATB2        SAVTB2            2
034810941019     C                   MOVEL     §3AFCA        SAVFCA            1 0
034820941019     C                   CLEAR                   DENCBO
034830050414     C*
034840050414     C* VERIFICO SE SI TRATTA DI UN PREPAGATO
034850050414     C                   MOVEL     '1'           WTRC
034860050414     C     KARB2         CHAIN(N)  FIAR6000                           30
034870050414     C  N30              MOVEL     'E'           WAR6
034880050414     C*
034890050414     C
034900920128     C*
034910920128     C* PROGRAMMA NON RICHIAMATO: PRENDO DATI DA BOLLA
034920941019    2C     *IN20         IFEQ      *OFF
034930941019     C*
034940941019    3C     WAR9          IFEQ      'E'
034950941019     C                   MOVEL     AR9TIC        VIDTIC
034960941019     C                   MOVEL     AR9CAS        VIDCAS
034970941019     C                   MOVEL     AR9VCA        VIDVCA
034980941215     C                   MOVEL     AR9VCA        VARVCA
034990941102     C                   MOVEL     AR9GCA        VIDGCA
035000941019    3C                   ENDIF
035010941019     C*
035020941019     C                   MOVEL     '?'           NEWCBO
035030920128     C*
035040941019   X2C                   ELSE
035050920128     C*
035060920128     C* PROGRAMMA     RICHIAMATO: PRENDO DATI DA DSARBK
035070920128     C                   MOVEL     §BKTIC        VIDTIC
035080941019     C                   MOVEL     §BKCAS        VIDCAS
035090941102     C                   MOVEL     §BKGCA        VIDGCA
035100941019     C                   MOVEL     §BKCBN        NEWCBO
035110941220     C                   MOVEL     §BKVCA        VARVCA
035120941220     C                   MOVEL     §BKVCA        VIDVCA
035130950420     C* SE NON PASSATO IMPOSTO QUELLO DELLA BOLLA
035140950420     C     NEWCBO        IFEQ      *BLANKS
035150950420     C                   MOVEL     ARBCBO        NEWCBO
035160950420     C                   ENDIF
035170920128     C*
035180920128     C* DECODIFICA NUOVO CODICE BOLLA
035190920128     C                   MOVEL     '3A'          COD
035200950420     C                   MOVEL(P)  NEWCBO        KEY
035210941212     C                   EXSR      CTABEL
035220920128     C  N30              MOVEL     TBLUNI        DS3A
035230941019     C   30              CLEAR                   DS3A
035240941102     C                   MOVEL     §3ADES        DENCBO
035250941019    2C                   END
035260941019     C*
035270920325     C* VARIAZIONE IMPORTO CONTRASSEGNO
035280941019    2C     VIDCVR        IFEQ      'KA'                                          UFFICIO
035290931118     C     VIDCVR        OREQ      'KB'                                          DOPO CONS
035300931118     C     VIDCVR        OREQ      'KP'                                          PADRONCINO
035310920325     C                   SETON                                        21
035320941102     C                   MOVEL     ARBCBO        NEWCBO
035330941019    2C                   END
035340941019    1C                   END
035350941019     C**
035360070309     C* F N A R B G 0 F
035370941019     C**
035380950120     C     FLGCVR        IFEQ      'C'
035390950120     C* SE E' CAUSALE CP PROTEGGO I CAMPI DELLA CONSEGNA RICHIESTA E
035400950120     C*  GG CHIUSURA
035410950120     C     VIDCVR        IFEQ      'CP'
035420950120     C                   SETON                                        16
035430950120     C                   ENDIF
035440941019     C* PGM NON RICHIAMATO
035450941019     C     *IN20         IFEQ      *OFF
035460920320     C* DATA CONSEGNA RICHIESTA
035470941019     C                   Z-ADD     ARBDCR        G02INV
035480920320     C*
035490920320     C                   Z-ADD     ARBHCR        VIDHCR
035500920320     C*
035510920320     C                   MOVEL     ARBTCR        VIDTCR
035520930510     C                   MOVEL     ARBGC1        VIDGC1
035530930510     C                   MOVEL     ARBGC2        VIDGC2
035540941019     C                   MOVEL     ARBTC1        VIDTC1
035550941102     C                   MOVEL     ARBTC2        VIDTC2
035560111025     c* Se il pgm non è richiamato, verifico se nella DS nel campo tipo §BGTCR
035570111025     c*  c'e' una "S" --> significa che posso aggiornare la consegna richiesta
035580111025     c*  in FIARP se bolla in distinta (solo da gestione telefonate o da richiamo
035590111025     c*  con passaggio dati) In caso contrario non posso farlo e sid eve dare errore
035600111207     c* ES - 07/12/11 -  per il momento asterisco
035610111207     c****               eval      Dcrindist=§BGTCR
035620930510     C*
035630941019   X2C                   ELSE
035640930510     C*
035650941019     C                   Z-ADD     §BGDCR        G02INV
035660930510     C*
035670930510     C                   Z-ADD     §BGHCR        VIDHCR
035680930510     C*
035690930510     C                   MOVEL     §BGTCR        VIDTCR
035700930510     C                   MOVEL     §BGGC1        VIDGC1
035710930510     C                   MOVEL     §BGGC2        VIDGC2
035720941019     C                   MOVEL     §BGTC1        VIDTC1
035730941019     C                   MOVEL     §BGTC2        VIDTC2
035740111025     c                   eval      Dcrindist='S'
035750920320     C                   END
035760941019     C*
035770941019     C* GIRO DATA CONSEGNA RICHIESTA
035780941019     C                   MOVEL     '3'           G02ERR
035790941019     C                   CALL      'XSRDA8'
035800941019     C                   PARM                    WLBDAT
035810941019     C                   Z-ADD     G02DAT        VIDDCR
035820070309     c
035830070309     c* Se lna con GEO attiva, verifico per eventuale modifica consegna ric
035840070309     c*  se c'e' sulla bolla un evento per il quale è inseribile la
035850070319     c*  modifica della data consegna richiesta
035860070319     c                   if        FGSgeot='S'
035870070309     c     karb          setgt     fnevb05l
035880070309     c     karb          readpe    fnevb05l
035890070309     c                   dow       not %eof(fnevb05l) and eveADR=' '
035900090526     c* controllo a seconda che sia o meno richiamato il pgm
035910090526     c  n20evbcev        lookup    ADRs                                   30
035920090526     c   20evbcev        lookup    ADRC                                   30
035930070309     c                   if        *in30
035940070309     c                   eval      eveADR='S'
035950070309     c                   else
035960070309     c     karb          readpe    fnevb05l
035970070309     c                   endif
035980070309     c                   enddo
035990070309     c
036000070309     c                   endif
036010070309     c
036020930510     C                   END
036030020531     c* se modificato destinatario in disagiato o supermercato imposto il
036040020531     c* relativo flag nelle cosegne particolari
036050041015     c* Lo faccio senza testare se arbgva = "U" perchè se mi trovo in
036060041015     c* questa condizione è certo che arbgva è <> da "U"
036070070907     c**   wdisag        ifne      *blanks
036080070907     c**                 select
036090070907     c**   vidtc1        wheneq    *blanks
036100070907     c**                 movel     wdisag        vidtc1
036110070907     c**   vidtc2        wheneq    *blanks
036120070907     c**                 movel     wdisag        vidtc2
036130070907     c**                 endsl
036140070907     c**   vidtc1        ifeq      vidtc2
036150070907     c**                 clear                   vidtc2
036160070907     c**                 endif
036170070907     c**                 endif
036180950104     C**
036190070309     C* F N A R B M 0 F
036200950104     C**
036210950104     C     VIDCVR        IFEQ      'MI'
036220950104     C* PGM NON RICHIAMATO: PER ORA NON ESISTE IL RICHIAMO
036230950104     C* DECODIFICO TIPO TARIFFA
036240950104     C                   MOVEL     ARBCTR        VIDCTR
036250050202     C                   MOVEL     ARBCTR        SAVCTR
036260060511     C                   MOVEL     ARBCTR        ctrbolla
036270050202     C                   CLEAR                   Fnlv59DS
036280050202     C                   MOVEL     VIDCTR        ILV59CTR
036290050202     C                   MOVEL     'S'           ILV59CFO
036300050202     C                   CALL      'FNLV59R'
036310050202     C                   PARM                    Fnlv59DS
036320950104     C*
036330050202     C     OLV59ERR      IFEQ      ' '
036340050202     C                   MOVEL     OLV59DFS      DE2CTR
036350950104     C                   ENDIF
036360950104     C                   MOVEL     ARBKSC        VIDKLP
036370950104     C                   MOVE      ARBKSC        VIDKSC
036380970822     C                   MOVE      ARBKSC        MEMKSC
036390950104     C                   MOVEL     ARBRSM        W008A
036400950104     C                   MOVEL     W008A         DESKSC
036410051220     c                   clear                   memntw
036420051220     c     vidklp        chain     azorg01l
036430051220     c                   if        %found(azorg01l)
036440051220     C                   MOVEL     ORGDE3        OG143
036450051220     C                   MOVEL     §OGNTW        memntw
036460051220     c                   endif
036470160225     c* Se lna fedex --> carico le tariffe di cartello FED
036480160225     c
036490970813     C* SE CODICE 8888 CARICO L'INDIRIZZO COMPLETO
036500950104     C     VIDKSC        IFEQ      8888
036510970813     C                   SETOFF                                       22
036520950104     C                   MOVEL     ARBRSM        VIDRSM
036530950104     C                   MOVEL     ARBINM        VIDINM
036540950104     C                   MOVEL     ARBCAM        VIDCAM
036550950104     C                   MOVEL     ARBLOM        VIDLOM
036560970808     C                   MOVEL     ARBCAM        VARCAM
036570970808     C                   MOVEL     ARBLOM        VARLOM
036580950104     C                   MOVEL     ARBPRM        VIDPRM
036590950104     C                   MOVEL     ARBNZM        VIDNZM
036600950104     C                   MOVEL     ARBFAP        VIDFAP
036610950104     C                   MOVEL     ARBCTR        VIDCTR
036620950104     C                   MOVEL     ARBCPI        VIDPIM
036630050922     C     'PRIVATO'     SCAN      vidpim        posiz             2 0    30
036640050922     c* solo numeri
036650050922     c* "PRIVATO senza iso
036660050922     c     *in30         ifeq      *off
036670050922     c     vidism        andge     *zeros
036680050922     c     *in30         oreq      *on
036690050922     c     posiz         andlt     3
036700050922     c                   clear                   vidpim
036710050922     c                   movel     arbcpi        vidcpm
036720050922     c                   endif
036730970813     C                   ELSE
036740970813     C                   SETON                                        22
036750950104     C                   ENDIF
036760950104     C                   ENDIF
036770961211     C**
036780970110     C* VARIAZIONE DEL RIFERIMENTO MITTENTE ALFABETICO E NATURA MERCE
036790961211     C**
036800961211     C     D48CVB        IFEQ      'RM'
036810970110     C                   MOVEL     ARBRMA        VIDRMA
036820970110     C                   MOVEL     ARBNAS        VIDNAS
036830961211     C                   END
036840950104     C*
036850011018      * Se l'importo d'assicurare è a 0 e la divisa non è impostata
036860011018     C     VIDIAS        IFEQ      *ZEROS
036870011018     C     VIDVAS        ANDEQ     *BLANKS
036880011018      * imposto la divisa della nazione del mittente
036890011018      * se estero
036900011018     C     MITVLT        IFNE      *BLANKS
036910011018     C     WMITIE        ANDEQ     'E'
036920011018     C                   MOVEL     MITVLT        VIDVAS
036930011018     C                   MOVEL     MITVLT        VARVAS
036940011018     C                   ELSE
036950011018      * altrimenti imposto la divisa della moneta di conto
036960011017     C                   MOVEL     §GEDCN        VIDVAS
036970011017     C                   MOVEL     §GEDCN        VARVAS
036980011017     C                   ENDIF
036990011018     C                   ENDIF
037000011018      *
037010011017      * Se la divisa e l'importo del valore merce non sono impostati
037020011018     C     VIDVAD        IFEQ      *BLANKS
037030011018     C     VIDVMD        ANDEQ     *ZEROS
037040011018      * imposto la divisa della nazione del mittente
037050011018      * se estero
037060011018     C     MITVLT        IFNE      *BLANKS
037070011018     C     WMITIE        ANDEQ     'E'
037080011018     C                   MOVEL     MITVLT        VIDVAD
037090011018     C                   MOVEL     MITVLT        VARVAD
037100011018     C                   ELSE
037110011018      * altrimenti imposto la divisa della moneta di conto
037120011017     C                   MOVEL     §GEDCN        VIDVAD
037130011017     C                   MOVEL     §GEDCN        VARVAD
037140011017     C                   ENDIF
037150011018     C                   ENDIF
037160011018      *
037170011017      * Se la divisa e l'importo del contrassegno non sono impostati
037180011018     C     VIDVCA        IFEQ      *BLANKS
037190011018     C     VIDCAS        ANDEQ     *ZEROS
037200011018      * imposto la divisa della nazione del destinatario
037210011018      * se estero
037220011018     C     DESVLT        IFNE      *BLANKS
037230011018     C     WDESIE        ANDEQ     'E'
037240011018     C                   MOVEL     DESVLT        VIDVCA
037250011018     C                   MOVEL     DESVLT        VARVCA
037260011018    2C                   ELSE
037270011018      * altrimenti imposto la divisa della moneta di conto
037280011017     C                   MOVEL     §GEDCN        VIDVCA
037290011017     C                   MOVEL     §GEDCN        VARVCA
037300011017     C                   ENDIF
037310011018     C                   ENDIF
037320030203      *------------
037330030124      * Bancali
037340030711      *  solo x causale 'BR'
037350030711If  1c                   If        D48Cvb = 'BR'
037360030124     c                   Clear                   dAr5Ban
037370030124     c                   Eval      kAr5Trd = 'BAN'
037380030725     c     kAr5          Chain     Fiar501l
037390030711     c                   If        %Found(Fiar501l)
037400030124     c                   Movel     Ar5Uni        dAr5Ban
037410030711     c                   EndIf
037420030124     c                   Eval      VidNba = §Ar5Nba
037430030124     c                   Eval      VidNb2 = §Ar5Nb2
037440030124     c                   Eval      VidRb1 = §Ar5Rb1
037450030124     c                   Eval      VidRb2 = §Ar5Rb2
037460030124     c                   Eval      VidNomb = §Ar5Nomb
037470030124     c                   Eval      VidMotb = §Ar5Motb
037480030124If  2c                   If        §Ar5Tba <> *Zeros
037490030612     c                   Clear                   Tibs02DS
037500030124     c                   Clear                   dTba
037510030124     c                   Movel     'C'           T02Mod
037520030124     c                   Movel     Knsif         T02Sif
037530030124     c                   Movel     'TBA'         T02Cod
037540030124     c                   Movel(p)  §Ar5Tba       T02Ke1
037550030124     c                   Call      'TIBS02R'
037560030124     c                   Parm                    Kpjba
037570030612     c                   Parm                    Tibs02DS
037580030124If  3c                   If        T02Err = *Blanks
037590030124     c                   Movel     T02Uni        dTba
037600030124    3c                   EndIf
037610030124     c                   Eval      VidTba = §TbaDesc
037620030124     c                   Eval      VidTbar = §TbaDesc
037630030124    2c                   EndIf
037640030124If  2c                   If        §Ar5Tb2 <> *Zeros
037650030612     c                   Clear                   Tibs02DS
037660030124     c                   Clear                   dTba
037670030124     c                   Movel     'C'           T02Mod
037680030124     c                   Movel     Knsif         T02Sif
037690030124     c                   Movel     'TBA'         T02Cod
037700030124     c                   Movel(p)  §Ar5Tb2       T02Ke1
037710030124     c                   Call      'TIBS02R'
037720030124     c                   Parm                    Kpjba
037730030612     c                   Parm                    Tibs02DS
037740030124If  3c                   If        T02Err = *Blanks
037750030124     c                   Movel     T02Uni        dTba
037760030124    3c                   EndIf
037770030124     c                   Eval      VidTb2 = §TbaDesc
037780030124     c                   Eval      VidTb2r = §TbaDesc
037790030124    2c                   EndIf
037800030711
037810080805     c* Se pgm richiamato, imposto dati dalla DSARBD
037820080805    2c                   if        *in20
037830080805     c                   Eval      VidRb1 = §bdpkf
037840080805     c                   Eval      VidRb2 = §bdvlf
037850080805    3c                   if        §bdrsd<>*blanks
037860080805     c                   Eval      VidNomb = §bdrsd
037870080805    3c                   endif
037880080805    3c                   if        §bdrd2<>*blanks
037890080805     c                   Eval      VidMotb = §bdrd2
037900080805     c                   else
037910080805     c                   Eval      VidMotb = 'PDA  '
037920080805    3c                   EndIf
037930080805
037940080805    2c                   EndIf
037950080805    1c                   EndIf
037960040608      *------------
037970040608      * Supermercati - telefono e referente
037980040608      *  solo x causale 'CS'
037990040608If  1c                   If        D48Cvb = 'CS'
038000040608     c                   Eval      wokscr = *Off
038010040608     c                   Clear                   dAr5Scr
038020040608     c                   Eval      kAr5Trd = 'SCR'
038030040608     c     kAr5          Setgt     Fiar501l
038040040608do  2c                   Do        *Hival
038050040608     c     kAr5          Readpe(n) Fiar501l
038060040608     c                   If        %Eof(Fiar501l)
038070040608     c                   Leave
038080040608     c                   EndIf
038090040608     c                   Movel     Ar5Uni        dAr5Scr
038100040608     c                   Eval      VidNom = §Ar5Nom
038110040608     c                   Eval      VidTel = §Ar5Tel
038120040608     c                   Eval      wokscr = *On
038130040608     c                   Leave
038140040608e   2c                   EndDo
038150040608      * se non ho trovato il rcd SCR cerco i dati sul rcd GEN
038160040608if  2c                   If        wokscr = *Off
038170070910     c***                Clear                   dAr5Gen
038180070910     c***                Eval      kAr5Trd = 'GEN'
038190070910     c***  kAr5          Chain(n)  Fiar501l
038200070910     c***                If        %Found(Fiar501l)
038210070910     c***                Movel     Ar5Uni        dAr5Gen
038220070910     c***                EndIf
038230070910     c***                Eval      VidNom = §Ar5Ref
038240070910     c***                Eval      VidTel = §Ar5Teld
038250070910     c                   Eval      VidNom = PAr5Ref
038260070910     c                   Eval      VidTel = PAr5Teld
038270040608e   2c                   EndIf
038280040608e   1c                   EndIf
038290911209     C*
038300910429     C                   ENDSR
038310941102     C*
038320941102     C* CARICO LA VARIA IN SFL ---------------------------------------*
038330941102     C     CARVAR        BEGSR
038340941102     C*
038350941102     C     WSV1          IFNE      ' '
038360011018      * Controllo se è una bolla di reso e se ha la varia 'N' 88888888
038370011018     C     ARBFBR        IFEQ      'S'
038380011018     C     WSV1          ANDEQ     'N'
038390011018     C     WVA1          ANDEQ     88888888
038400011018     C                   MOVEL     '1'           FLGVRN            1
038410011018     C                   ENDIF
038420990930     C* CARICO SOLO SE VARIA FA PARTE DEL TOTALE IMPONIBILE
038430990930     C     WSV1          LOOKUP    SVA                                    33
038440990930     C     *IN33         IFEQ      *ON
038450941102     C                   ADD       1             NRR
038460941102     C     NRR           CHAIN     LR48S03                            30
038470941102     C                   MOVEL     WSV1          V3CSV1
038480941102     C                   Z-ADD     WVA1          V3CVA1
038490991006     C                   Z-ADD     WVA1          VARVA1
038500941102     C   30              WRITE     LR48S03
038510941102     C  N30              UPDATE    LR48S03
038520941102     C                   ENDIF
038530990930     C                   ENDIF
038540941102     C                   ENDSR
038550040122
038560040122      *---------------------------------------------------------------*
038570040122      * CARICO LA VARIA DELLA SECONDA BOLLA
038580040122      *---------------------------------------------------------------*
038590040122     c     CarVar1       BegSr
038600040122
038610040122if  1c                   If        wsv1 <> *Blanks
038620040122      * Carico la varia solo se fa parte del totale imponibile
038630040122     c     wsv1          Lookup    sva                                    31
038640040122if  2c                   If        *In31
038650040122     c                   add       1             ContaSv
038660040122     c                   add       wva1          TotImv
038670040122
038680040122s   3c                   Select
038690040122     c                   When      ContaSv = 1
038700040122     c                   Movel     wsv1          Vi2Sv1
038710040122     c                   Z-add     wva1          Vi2Va1
038720040122     c                   Z-add     wva1          Va2Va1
038730040122     c                   When      ContaSv = 2
038740040122     c                   Movel     wsv1          Vi2Sv2
038750040122     c                   Z-add     wva1          Vi2Va2
038760040122     c                   Z-add     wva1          Va2Va2
038770040122     c                   When      ContaSv = 3
038780040122     c                   Movel     wsv1          Vi2Sv3
038790040122     c                   Z-add     wva1          Vi2Va3
038800040122     c                   Z-add     wva1          Va2Va3
038810040122     c                   When      ContaSv = 4
038820040122     c                   Movel     wsv1          Vi2Sv4
038830040122     c                   Z-add     wva1          Vi2Va4
038840040122     c                   Z-add     wva1          Va2Va4
038850041130     c                   When      ContaSv = 5
038860041130     c                   Movel     wsv1          Vi2Sv5
038870041130     c                   Z-add     wva1          Vi2Va5
038880041130     c                   Z-add     wva1          Va2Va5
038890041130     c                   When      ContaSv = 6
038900041130     c                   Movel     wsv1          Vi2Sv6
038910041130     c                   Z-add     wva1          Vi2Va6
038920041130     c                   Z-add     wva1          Va2Va6
038930040122    3c                   EndSl
038940040122
038950040122    2c                   EndIf
038960040122
038970040122    1c                   EndIf
038980040122
038990040122     c                   EndSr
039000910429     C*
039010991001     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
039020991001     C     CARWAR        BEGSR
039030991001     C*
039040991001     C                   CLEAR                   WINL
039050991001     C*
039060991001     C                   CLEAR                   WSV
039070991001     C                   CLEAR                   WVA
039080991001     C                   CLEAR                   C
039090991001    1C                   DO        20            NRR
039100991001     C     NRR           CHAIN     LR48S03                            30
039110991001     C*
039120991001    2C     *IN30         IFEQ      *OFF
039130991001     C     V3CSV1        ANDNE     *BLANKS
039140991001     C                   ADD       1             C
039150991001     C                   MOVEL     V3CSV1        WSV(C)
039160991001     C                   Z-ADD     V3CVA1        WVA(C)
039170991001    2C                   ENDIF
039180991001    1C                   ENDDO
039190991001     C* SE RICHIESTA IL CONTROLLO ESISTENZA DELL'INOLTRO PROCEDO
039200991001     C     WCKINL        IFEQ      '1'
039210991001     C                   Z-ADD     1             II
039220991001     C     §$2FIN        LOOKUP    WSV(II)                                30
039230991001     C   30WVA(II)       IFGT      *ZEROS
039240991001     C                   MOVE      '1'           WINL              1
039250991001     C                   ENDIF
039260991001     C                   ENDIF
039270991001     C*
039280991001     C                   ENDSR
039290040123      *---------------------------------------------------------------*
039300040123      * Carico le varie della seconda bolla in una sk di comodo
039310040123      *---------------------------------------------------------------*
039320040123     c     Carwar1       Begsr
039330040123
039340040123     c                   Clear                   wsv
039350040123     c                   Clear                   wva
039360040123     c                   Clear                   c
039370040123
039380040123     c                   If        vi2sv1 <> *Blanks
039390050401     c                   add       1             c
039400050401     c                   Movel     vi2Sv1        wsv(c)
039410050401     c                   Z-add     vi2va1        wva(c)
039420040123     c                   EndIf
039430040123     c                   If        vi2sv2 <> *Blanks
039440050401     c                   add       1             c
039450050401     c                   Movel     vi2Sv2        wsv(c)
039460050401     c                   Z-add     vi2va2        wva(c)
039470040123     c                   EndIf
039480040123     c                   If        vi2sv3 <> *Blanks
039490050401     c                   add       1             c
039500050401     c                   Movel     vi2Sv3        wsv(c)
039510050401     c                   Z-add     vi2va3        wva(c)
039520040123     c                   EndIf
039530040123     c                   If        vi2sv4 <> *Blanks
039540050401     c                   add       1             c
039550050401     c                   Movel     vi2Sv4        wsv(c)
039560050401     c                   Z-add     vi2va4        wva(c)
039570040123     c                   EndIf
039580041130     c                   If        vi2sv5 <> *Blanks
039590050401     c                   add       1             c
039600050401     c                   Movel     vi2Sv5        wsv(c)
039610050401     c                   Z-add     vi2va5        wva(c)
039620041130     c                   EndIf
039630041130     c                   If        vi2sv6 <> *Blanks
039640050401     c                   add       1             c
039650050401     c                   Movel     vi2Sv6        wsv(c)
039660050401     c                   Z-add     vi2va6        wva(c)
039670041130     c                   EndIf
039680040123
039690040123     c                   EndSr
039700991012     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
039710991012     C     RDTASV        BEGSR
039720991012     C*
039730991012     C                   CLEAR                   C
039740030620     C                   CLEAR                   WVAR3A
039750040123     c                   Clear                   wvar3ai
039760991012    1C                   DO        20            NRR
039770991012     C     NRR           CHAIN     LR48S03                            30
039780991012    2C     *IN30         IFEQ      *OFF
039790991012     C                   ADD       1             C
039800020503     c* se la varia è uguale a quella prevista dal tipo bolla, non richiamo
039810020503     c*  piu' la tassazione
039820040123     c                   if        §3asva<>*blanks and v3csv1=§3asva
039830040123     c                   If        vidimv > *Zeros
039840020503     c                   eval      wtassa='N'
039850030620      * mi salvo l'imnporto della varia
039860030620     c                   eval      itassa = v3cva1
039870040123     c                   EndIf
039880030620      * mi salvo un campo di comodo
039890030620     c                   movel     'S'           Wvar3a            1
039900040123      * memorizzo anche se valorizzata
039910040123     c                   If        v3cva1 > 0
039920040123     c                   Eval      wvar3ai = 'S'
039930040123     c                   EndIf
039940030620      *
039950020503     c                   endif
039960030620      * verifico se esiste la varia "D" non la ricalcolo in caso di fattura immediata
039970030620     c                   if        v3csv1 = 'D'
039980030620     c                   eval      wtassad = 'N'
039990030620     c                   endif
040000030620      *
040010030624      * se esiste la varia "&" con tutti 8888888 non la passo al TNSF20R
040020030624      * ma passo un flag  di richiesta calcolo varia "&"
040030030709     C                   if        v3csv1 = '&' and v3cva1 = 88888888 and
040040030709     C                             flgsf20 = 'S'
040050030624     C                   eval      TASFCAA = 'S'
040060030624     C                   else
040070991012     C                   MOVEL     V3CSV1        VSV(C)
040080991012     C                   MOVEL     V3CVA1        VVA(C)
040090030624     c                   endif
040100030624      *
040110991012    2C                   ENDIF
040120991012    1C                   ENDDO
040130991012     C                   ENDSR
040140920120     C*--- EMISSIONE E GESTIONE FORMATO 2 ----------------------------*
040150170130     C     EMFOR2        BEGSR
040160941102     C*
040170170130     c                   clear                   wpes_pre
040180170130     c                   clear                   wvol_pre
040190170130
040200920120     C     FOR02         TAG
040210941102     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
040220950519    1C     *IN90         IFEQ      *ON
040230950114     C     D48FFR        OREQ      'S'
040240950607     C     D48FFR        OREQ      'V'
040250941214     C                   WRITE     LR48T03
040260941014     C                   EXFMT     LR48D02
040270950519    1C                   END
040280941102     C*
040290941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
040300941102     C                   SETOFF                                       2890
040310941102     C                   SETOFF                                       404142
040320941102     C                   SETOFF                                       434445
040330941102     C                   SETOFF                                       464748
040340061115     C                   SETOFF                                       495051
040350140408     C                   SETOFF                                       5268
040360941220     C* F12 - RITORNO
040370941220    1C     *INKL         IFEQ      *OFF
040380070329     c
040390070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
040400070329     c                   if        *in87 and *inkr
040410070329     c                   exsr      CALLNOTELDV
040420070329     c                   goto      for02
040430070329     c                   endif
040440941216     C*
040450941216     C* SE NON POSSO VARIARE NE IL PESO, NE IL VOLUME, NON CONTROLLO
040460941216     C* E NON AGGIORNO
040470941220    2C     *IN88         IFEQ      *OFF
040480941216     C     *IN89         OREQ      *OFF
040490950114     C     *IN24         OREQ      *ON
040500941216     C*
040510941216     C* CONTROLLO FORMATO
040520920120     C                   EXSR      CONTR2
040530941215     C*
040540941220     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
040550941220    3C     *IN90         IFEQ      *ON
040560941215     C     D48FFR        ANDEQ     'E'
040570941215     C                   MOVEL     VIDMSG        D48MSG
040580941215     C                   MOVEL     '2'           D48ERR
040590941215     C                   GOTO      ENDEM2
040600941220    3C                   ENDIF
040610140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
040620140502     c*   (d48ffr='E' e 26 off)
040630140502     c  n90
040640140502     cann26              if        d48ffr='E'
040650140502     C                   GOTO      ENDEM2
040660140502     c                   endif
040670950607     C  NKF
040680950607     CANN26
040690920120     COR 90              GOTO      FOR02
040700060227     c* richiamo routine di richiesta/controllo motivazione
040710060301     c                   if        d66mtv = 'S' and w48mct = *blanks
040720060227     c                   exsr      ges_mtv
040730060301     c                   if        olv85f12 = '1'
040740060301     c                   if        ilv85wdw = ' '
040750060227     c                   goto      for02
040760060301     c                   else
040770060301     c                   eval      *inkl = '1'
040780060301     C                   GOTO      ENDEM2
040790060227     c                   endif
040800060301     c                   endif
040810060227     C     *IN90         IFEQ      *ON
040820060227     C     D48FFR        ANDEQ     'E'
040830060227     C                   MOVEL     VIDMSG        D48MSG
040840060227     C                   MOVEL     '2'           D48ERR
040850060227     C                   GOTO      ENDEM2
040860060227     C                   ENDIF
040870060227     c                   endif
040880920120     C*
040890141020     c* Se Richiesta modifica di indirizzo tale da variare la LNA emetto window
040900141020     c* per segnalare e richiedere una delle seguenti opzioni:
040910141020     c*  -Forzare la variazione senza variare la lna
040920141020     c*  -Richiedere il dirottamento
040930141111    3c                   if        d48ffr<>'E'  and *in01
040940141029     c                             and (%lookup('999':skFilOkdir)>0 or
040950141029     c                             (%lookup(%editc(arblna:'X'):skFilOkdir)>0 and
040960141029     c                              %lookup(%editc(dutpou:'X'):skFilOkdir)>0) )
040970141020     c                   clear                   wdir
040980141020    4C     d_o95lna      ifne      ARBLNA
040990141022     c* le bolle export non le dirottiamo mai da qui
041000141022     c     lnantw        andne     'DPD'
041010141022     c     lnantw        andne     'EEX'
041020141022     c     lnantw        andne     'FED'
041030141219     c* no bolle di recupero
041040141219     c     §3arbl        andne     'R'
041050141204    5C     SAVCAD        IFNE      VIDCAD
041060141204     C     SAVLOD        ORNE      VIDLOD
041070141204     C     SAVrsd        ORNE      VIDrsd
041080141204     C     SAVind        ORNE      VIDind
041090141204     C     SAVprd        ORNE      VIDprd
041100141204     C     SAVnzd        ORNE      VIDnzd
041110141204     c     vidzng        oreq      savzng
041120141203     c     vidffd        andeq     *blanks
041130141021     c                   exsr      ges_wdw02
041140141021     c                   if        winf12='1'
041150141021     c                   goto      for02
041160141021     c                   endif
041170141020    5C                   END
041180141020    4C                   END
041190141020    4c                   if        wdir='1'
041200141212
041210141212     c* se hanno cambiato il Fermo deposito devo anche eseguire la REGIS2
041220141212     c                   if        arbffd<>%subst(vidffD:1:1)
041230141212     c                   eval      wnoindir='S'
041240141212     c                   exsr      regis2
041250141218     C   91              GOTO      FOR02
041260141212     c                   endif
041270141212
041280141020     c                   exsr      sr_tiidc
041290141212
041300141020   x4c                   else
041310141212     c                   eval      wnoindir=' '
041320141020     C                   EXSR      REGIS2
041330141020    4c                   endif
041340141020   x3c                   else
041350141212     c                   eval      wnoindir=' '
041360930517     C                   EXSR      REGIS2
041370141020    3c                   endif
041380991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
041390991130     C     *IN91         IFEQ      *ON
041400991130     C     D48FFR        ANDEQ     'E'
041410991130     C                   MOVEL     VIDMSG        D48MSG
041420991130     C                   MOVEL     '2'           D48ERR
041430991130     C                   GOTO      ENDEM2
041440991130     C                   ENDIF
041450991130     C   91              GOTO      FOR02
041460941216     C*
041470941220   X2C                   ELSE
041480941216     C*
041490941216     C                   GOTO      FOR02
041500941220    2C                   ENDIF
041510941220     C*
041520950518    1C                   ENDIF
041530150112     c* Se non ho passato msg vincolanti, passo msg di avvertimento
041540150112     c*  se ce ne sono
041550150112     c                   if        msgforzatxt<>*blanks
041560150112     C                   MOVEL     msgforzatxt   D48MSG
041570150112     C                   MOVEL     'Z'           D48ERR
041580150112     c                   clear                   msgforzatxt
041590150112     C                   ENDIF
041600950518     C*
041610950518     C     ENDEM2        TAG
041620950518     C*
041630950518    1C     *INKL         IFEQ      *ON
041640950518     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
041650950519    1C     D48FFR        OREQ      'E'
041660950518     C     *IN90         ANDEQ     *ON
041670000303    1C     D48FFR        OREQ      'E'
041680000303     C     *IN91         ANDEQ     *ON
041690941220     C* F12-RITORNO
041700160119    2C     *IN10         IFEQ      *ON
041710941220     C                   UNLOCK    FNARB01L
041720051118     C                   UNLOCK    FiAR901L
041730160119     c                   endif
041740160119     C****               ELSE
041750941222     C                   UNLOCK    FNBLP01L
041760160119    2C****               END
041770991001     C                   UNLOCK    FIAR601L
041780950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
041790950607     C* flag D48F12 = 1
041800950607     C   KL              MOVEL     '1'           D48F12
041810950519    1C                   END
041820941215     C*
041830950518     C                   ENDSR
041840920121     C*
041850920121     C*--- EMISSIONE E GESTIONE FORMATO 3 ----------------------------*
041860920121     C     EMFOR3        BEGSR
041870990415     C*
041880990415     C                   MOVEL     ' '           COMASS
041890941102     C*
041900920121     C     FOR03         TAG
041910941102     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
041920950114     C     *IN90         IFEQ      *ON
041930941212     C     D48FFR        OREQ      'S'
041940950607     C     D48FFR        OREQ      'V'
041950941215     C                   WRITE     LR48T03
041960941212     C                   WRITE     LR48Z03
041970941212     C                   EXFMT     LR48C03
041980941220    1C                   END
041990941102     C*
042000941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
042010941209     C                   SETOFF                                       289052
042020941102     C                   SETOFF                                       404142
042030991006     C                   SETOFF                                       434445
042040941102     C                   SETOFF                                       464748
042050941209     C                   SETOFF                                       495051
042060941215     C                   SETOFF                                       989453
042070961112     C                   SETOFF                                       545556
042080990415     C                   SETOFF                                       82
042090941220     C*
042100941220     C* PER F12-RITORNO O F21 RITASSAZIONE, DISALLOCO I RECORD
042110941220     C* F12-RITORNO
042120941220    1C     *INKL         IFEQ      *ON
042130941220     C     *INKV         OREQ      *ON
042140941222     C*
042150941222     C     *IN10         IFEQ      *ON
042160941220     C                   UNLOCK    FNARB01L
042170051118     C                   UNLOCK    FiAR901L
042180941222     C                   ELSE
042190941222     C                   UNLOCK    FNBLP01L
042200941220    1C                   ENDIF
042210991005     C                   UNLOCK    FIAR601L
042220950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
042230950607     C* flag D48F12 = 1
042240950607     C   KL              MOVEL     '1'           D48F12
042250941222    1C                   ENDIF
042260941220     C* F12 - RITORNO
042270941220    1C  NKL              DO
042280941220     C*
042290941220     C* F21 - RITASSAZIONE BOLLA
042300941220    2C     *INKV         IFEQ      *ON
042310050511     C                   CLEAR                   Fnlra2DS
042320050511     C                   Z-ADD     ARBAAS        ilra2AAS
042330050511     C                   Z-ADD     ARBLNP        ilra2lNP
042340050511     C                   Z-ADD     ARBNRS        ilra2NRS
042350050511     C                   Z-ADD     ARBNSP        ilra2NSP
042360050511     C                   MOVEL     D48FGS        ilra2FGS
042370050511     C                   MOVEL     D48CVB        ilra2CVB
042380050511     C                   MOVEL     Fnlra2DS      KPJBU
042390050511     C                   CALL      'FNLRA2R'
042400941217     C                   PARM                    KPJBA
042410030718     C                   PARM                    TRUL90DS
042420050804     c* Se mi passa errore, lo passo dal lr48 al lr36
042430050804     c                   movel     kpjbu         fnlra2ds
042440050804     C                   MOVEL     olra2msg      D48MSG
042450050804     C                   MOVEL     olra2err      D48ERR
042460170505     c                   if        olra2err=*blanks
042470131119     c                   if        olra2nft>0
042480131119     C                   MOVEL     '1'           WTRC
042490131119     c     karb2         chain(n)  fiar6000
042500131119     c                   endif
042510131120     c* trasmetto variazione a PDA:
042520131120     c* se segue fattura --> solo se prima era una fattura immediata
042530131120     c* se fattura immediata --> solo se variato importo della fattura
042540131119     c                   if        *in10 and ((olra2nft=0  and ar6nft>0)
042550131120     c                              or (olra2nft>0 and (ar6ift<>sav_ift
042560131120     c                              or ar6div<>viddiv)))
042570130416     c                   eval      wpda_ndc=arbndc
042580130416     c                   eval      wpda_ddc=arbddc
042590130416     c                   eval      wpda_dcm=arbdcm
042600140120     c                   eval      arbdtv=olra2dtv
042610140120     c                   eval      arborv=olra2orv
042620140120     c                   eval      arbpru=olra2pru
042630140120     c                   eval      arbcvb=d48cvb
042640130416     c                   exsr      sr_pda
042650130416     c                   endif
042660170411     c* se passato da Fattura Immediata a Segue Fattura memorizzo i dati della vecchia fattura
042670170411     c                   if        olra2nft=0 and ar6nft>0
042680170411     c                   exsr      sr_fnfta
042690170411     c                   endif
042700170505     c                   endif
042710920121     C*
042720941220   X2C                   ELSE
042730920121     C* CONTROLLO FORMATO
042740920121     C                   EXSR      CONTR3
042750941215     C*
042760941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
042770941220    3C     *IN90         IFEQ      *ON
042780941215     C     D48FFR        ANDEQ     'E'
042790941215     C                   MOVEL     VIDMSG        D48MSG
042800941215     C                   MOVEL     '3'           D48ERR
042810941215     C                   GOTO      ENDEM3
042820941220    3C                   ENDIF
042830920320     C*
042840950607     C  NKF
042850950607     CANNKI
042860950607     CANN26
042870990415     COR 82
042880000105     COR KN
042890920320     COR 90              GOTO      FOR03
042900060302     c* richiamo routine di richiesta/controllo motivazione
042910060302     c                   if        d66mtv = 'S' and w48mct = *blanks
042920060302     c                   exsr      ges_mtv
042930060302     c                   if        olv85f12 = '1'
042940060302     c                   if        ilv85wdw = ' '
042950060302     c                   goto      for03
042960060302     c                   else
042970060302     c                   eval      *inkl = '1'
042980060302     C                   GOTO      ENDEM3
042990060302     c                   endif
043000060302     c                   endif
043010060302     C     *IN90         IFEQ      *ON
043020060302     C     D48FFR        ANDEQ     'E'
043030060302     C                   MOVEL     VIDMSG        D48MSG
043040070308     C                   MOVEL     '3'           D48ERR
043050060302     C                   GOTO      ENDEM3
043060060302     C                   ENDIF
043070060302     c                   endif
043080920121     C*
043090920204     C* CMD6 - AGGIORNAMENTO PIU' EVENTUALE STAMPA
043100920204     C                   EXSR      REGIS3
043110991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
043120991130     C     *IN91         IFEQ      *ON
043130991130     C     D48FFR        ANDEQ     'E'
043140991130     C                   MOVEL     VIDMSG        D48MSG
043150991130     C                   MOVEL     '3'           D48ERR
043160991130     C                   GOTO      ENDEM3
043170991130     C                   ENDIF
043180920204     C   91              GOTO      FOR03
043190920204     C*
043200070309     c* ristampo fattura immediata se variata(37 on), se in arrivo
043210070309     c*  (10 on) e se non attiva GEO
043220130321    3C**   *IN37         IFEQ      *ON
043230130321    3C***  *IN10         ANDEQ     *ON
043240130321     c**   FGSddaSI      andeq     ' '
043250130321     C***                CLEAR                   FNLSB5DS
043260130321     C***                MOVEL     'V'           DB0RIS
043270130321     C***                MOVEL     D48TBO        DB0TBO
043280130321     C***                MOVEL     ARBAAS        DB0AAS
043290130321     C***                MOVEL     ARBLNP        DB0LNP
043300130321     C***                MOVEL     ARBNRS        DB0NRS
043310130321     C***                MOVEL     ARBNSP        DB0NSP
043320130321     C***                CALL      D90PSL
043330130321     C***                PARM                    FNLSB5DS
043340130321    3C***                END
043350941212     C*
043360941220    2C                   END
043370941220     C*
043380000303    1C                   END
043390000303     C     ENDEM3        TAG
043400000303     C* SE PGM RICHIAMATO E C'E' ERRORE DISALLOCO
043410000303    1C     D48FFR        IFEQ      'E'
043420000303     C     *IN90         ANDEQ     *ON
043430000303    1C     D48FFR        OREQ      'E'
043440000303     C     *IN91         ANDEQ     *ON
043450000303     C     *IN10         IFEQ      *ON
043460000303     C                   UNLOCK    FNARB01L
043470051118     C                   UNLOCK    FiAR901L
043480000303     C                   ELSE
043490000303     C                   UNLOCK    FNBLP01L
043500000303     C                   ENDIF
043510000303     C                   UNLOCK    FIAR601L
043520000303     C* Comunico al chiamante che è stato premuto F12 valorizzando il
043530000303     C* flag D48F12 = 1
043540000303     C   KL              MOVEL     '1'           D48F12
043550000303    1C                   ENDIF
043560920121     C*
043570000303     C                   ENDSR
043580941102     C*
043590941102     C* EMETTO FORMATO4 ----------------------------------------------*
043600941102     C     EMFOR4        BEGSR
043610941102     C*
043620941102     C     FOR04         TAG
043630941102     C*
043640950114     C     *IN90         IFEQ      *ON
043650941212     C     D48FFR        OREQ      'S'
043660950607     C     D48FFR        OREQ      'V'
043670941102     C                   WRITE     LR48T01
043680941102     C                   EXFMT     LR48D04
043690941102     C                   ENDIF
043700941209     C*
043710941209     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
043720941209     C                   SETOFF                                       289045
043730941209     C                   SETOFF                                       464748
043740991006     C                   SETOFF                                       495051
043750041130     c                   setoff                                       636465
043760041130     c                   setoff                                       6667
043770941220     C*
043780941220     C* PER F12 -RITORNO O F21-RITASSAZIONE SBLOCCO I RECORD
043790941220     C     *INKL         IFEQ      *ON
043800941220     C     *INKV         OREQ      *ON
043810941222     C*
043820941222     C     *IN10         IFEQ      *ON
043830941220     C                   UNLOCK    FNARB01L
043840051118     C                   UNLOCK    FiAR901L
043850941222     C                   ELSE
043860941222     C                   UNLOCK    FNBLP01L
043870941220     C                   ENDIF
043880991005     C                   UNLOCK    FIAR601L
043890950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
043900950607     C* flag D48F12 = 1
043910950607     C   KL              MOVEL     '1'           D48F12
043920941222     C                   ENDIF
043930941220     C* F12 - RITORNO
043940941102     C     *INKL         IFEQ      *OFF
043950050228     C* F21 - RITASSAZIONE BOLLA
043960941220     C     *INKV         IFEQ      *ON
043970050511     C                   CLEAR                   Fnlra2DS
043980050511     C                   Z-ADD     ARBAAS        ilra2AAS
043990050511     C                   Z-ADD     ARBLNP        ilra2LNP
044000050511     C                   Z-ADD     ARBNRS        ilra2NRS
044010050511     C                   Z-ADD     ARBNSP        ilra2NSP
044020050511     C                   MOVEL     D48FGS        ilra2FGS
044030050511     C                   MOVEL     D48CVB        ilra2CVB
044040050511     C                   MOVEL     Fnlra2DS      KPJBU
044050050511     C                   CALL      'FNLRA2R'
044060941102     C                   PARM                    KPJBA
044070030718     C                   PARM                    TRUL90DS
044080050804     c
044090050804     c* Se mi passa errore, lo passo dal lr48 al lr36
044100050804     c                   movel     kpjbu         fnlra2ds
044110050804     C                   MOVEL     olra2msg      D48MSG
044120050804     C                   MOVEL     olra2err      D48ERR
044130170505     c                   if        olra2err=*blanks
044140131119     c                   if        olra2nft>0
044150131119     C                   MOVEL     '2'           WTRC
044160131119     c     karb2         chain(n)  fiar6000
044170131119     c                   endif
044180131120     c* trasmetto variazione a PDA:
044190131120     c* se segue fattura --> solo se prima era una fattura immediata
044200131120     c* se fattura immediata --> solo se variato importo della fattura
044210131120     c                   if        *in10 and ((olra2nft=0  and ar6nft>0)
044220131120     c                              or (olra2nft>0 and (ar6ift<>sav_ift
044230131120     c                              or ar6div<>viddiv)))
044240130416     c                   eval      wpda_ndc=arbndc
044250130416     c                   eval      wpda_ddc=arbddc
044260130416     c                   eval      wpda_dcm=arbdcm
044270140120     c                   eval      arbdtv=olra2dtv
044280140120     c                   eval      arborv=olra2orv
044290140120     c                   eval      arbpru=olra2pru
044300140120     c                   eval      arbcvb=d48cvb
044310130416     c                   exsr      sr_pda
044320130416     c                   endif
044330170411     c* se passato da Fattura Immediata a Segue Fattura memorizzo i dati della vecchia fattura
044340170411     c                   if        olra2nft=0 and ar6nft>0
044350170411     c                   exsr      sr_fnfta
044360170411     c                   endif
044370170505     c                   endif
044380941102     C*
044390941220     C                   ELSE
044400941102     C* CONTROLLO FORMATO
044410941102     C                   EXSR      CONTR4
044420941215     C*
044430941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044440941215     C     *IN90         IFEQ      *ON
044450941215     C     D48FFR        ANDEQ     'E'
044460941215     C                   MOVEL     VIDMSG        D48MSG
044470941215     C                   MOVEL     '4'           D48ERR
044480941215     C                   GOTO      ENDEM4
044490941215     C                   ENDIF
044500941102     C*
044510140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
044520140502     c*   (d48ffr='E' e 26 off)
044530140502     c  n90
044540140502     cann26              if        d48ffr='E'
044550140502     C                   GOTO      ENDEM4
044560140502     c                   endif
044570950607     C  NKF
044580950607     CANNKI
044590950607     CANN26
044600941102     COR 90              GOTO      FOR04
044610060302     c* richiamo routine di richiesta/controllo motivazione
044620060302     c                   if        d66mtv = 'S' and w48mct = *blanks
044630060302     c                   exsr      ges_mtv
044640060302     c                   if        olv85f12 = '1'
044650060302     c                   if        ilv85wdw = ' '
044660060302     c                   goto      for04
044670060302     c                   else
044680060302     c                   eval      *inkl = '1'
044690060302     C                   GOTO      ENDEM4
044700060302     c                   endif
044710060302     c                   endif
044720060302     C     *IN90         IFEQ      *ON
044730060302     C     D48FFR        ANDEQ     'E'
044740060302     C                   MOVEL     VIDMSG        D48MSG
044750070308     C                   MOVEL     '4'           D48ERR
044760060302     C                   GOTO      ENDEM4
044770060302     C                   ENDIF
044780060302     c                   endif
044790941102     C*
044800941102     C* CMD6 - AGGIORNAMENTO PIU' EVENTUALE STAMPA
044810941102     C                   EXSR      REGIS4
044820991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044830991130     C     *IN91         IFEQ      *ON
044840991130     C     D48FFR        ANDEQ     'E'
044850991130     C                   MOVEL     VIDMSG        D48MSG
044860991130     C                   MOVEL     '4'           D48ERR
044870991130     C                   GOTO      ENDEM4
044880991130     C                   ENDIF
044890941102     C   91              GOTO      FOR04
044900941102     C                   END
044910941102     C                   END
044920000303     C*
044930000303     C     ENDEM4        TAG
044940000303    1C     D48FFR        IFEQ      'E'
044950000303     C     *IN90         ANDEQ     *ON
044960000303    1C     D48FFR        OREQ      'E'
044970000303     C     *IN91         ANDEQ     *ON
044980000303     C     *IN10         IFEQ      *ON
044990000303     C                   UNLOCK    FNARB01L
045000051118     C                   UNLOCK    FiAR901L
045010000303     C                   ELSE
045020000303     C                   UNLOCK    FNBLP01L
045030000303     C                   ENDIF
045040000303     C                   UNLOCK    FIAR601L
045050000303     C* Comunico al chiamante che è stato premuto F12 valorizzando il
045060000303     C* flag D48F12 = 1
045070000303     C   KL              MOVEL     '1'           D48F12
045080000303    1C                   ENDIF
045090941102     C*
045100000303     C                   ENDSR
045110941102     C*
045120941102     C* EMETTO E GESTISCO FORMATO5 -----------------------------------*
045130941102     C     EMFOR5        BEGSR
045140950407     C*
045150941102     C     FOR05         TAG
045160941102     C*
045170950114     C     *IN90         IFEQ      *ON
045180941212     C     D48FFR        OREQ      'S'
045190950607     C     D48FFR        OREQ      'V'
045200941102     C                   WRITE     LR48T01
045210941102     C                   EXFMT     LR48D05
045220941102     C                   END
045230941102     C*
045240941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
045250941102     C                   SETOFF                                       289096
045260941102     C                   SETOFF                                       404142
045270991001     C                   SETOFF                                       434492
045280070329     C* F12 - RITORNO
045290941220    1C     *INKL         IFEQ      *OFF
045300070329     c
045310070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
045320070329     c                   if        *in87 and *inkr
045330070329     c                   exsr      CALLNOTELDV
045340070329     c                   goto      for05
045350070329     c                   endif
045360941102     C*
045370941102     C* CONTROLLO FORMATO
045380941213     C                   EXSR      CONTR5
045390941215     C*
045400941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045410941220    2C     *IN90         IFEQ      *ON
045420941215     C     D48FFR        ANDEQ     'E'
045430941215     C                   MOVEL     VIDMSG        D48MSG
045440941215     C                   MOVEL     '5'           D48ERR
045450941215     C                   GOTO      ENDEM5
045460941220    2C                   ENDIF
045470140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
045480140502     c*   (d48ffr='E' e 26 off)
045490140502     c  n90
045500140502     cann26              if        d48ffr='E'
045510140502     C                   GOTO      ENDEM5
045520140502     c                   endif
045530140502     c* Se errore forzabile ma pgm richiamato in modalità 'E' o 'F' cioè
045540140502     c* senza emissione di videata ignoro l'errore forzabile e permetto
045550140502     c* l'aggiornamento
045560140502     c   92              if        d48ffr='E'
045570140502     c                   setoff                                       92
045580140502     c                   endif
045590950607     C  NKF
045600950607     CANN26
045610950407     COR 90
045620950407     COR 92              GOTO      FOR05
045630060302     c* richiamo routine di richiesta/controllo motivazione
045640060302     c                   if        d66mtv = 'S' and w48mct = *blanks
045650060302     c                   exsr      ges_mtv
045660060302     c                   if        olv85f12 = '1'
045670060302     c                   if        ilv85wdw = ' '
045680060302     c                   goto      for05
045690060302     c                   else
045700060302     c                   eval      *inkl = '1'
045710060302     C                   GOTO      ENDEM5
045720060302     c                   endif
045730060302     c                   endif
045740060302     C     *IN90         IFEQ      *ON
045750060302     C     D48FFR        ANDEQ     'E'
045760060302     C                   MOVEL     VIDMSG        D48MSG
045770070308     C                   MOVEL     '5'           D48ERR
045780060302     C                   GOTO      ENDEM5
045790060302     C                   ENDIF
045800060302     c                   endif
045810941102     C*
045820941220     C                   EXSR      REGIS5
045830991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045840991130     C     *IN91         IFEQ      *ON
045850991130     C     D48FFR        ANDEQ     'E'
045860991130     C                   MOVEL     VIDMSG        D48MSG
045870991130     C                   MOVEL     '5'           D48ERR
045880991130     C                   GOTO      ENDEM5
045890991130     C                   ENDIF
045900941213     C   91              GOTO      FOR05
045910941220     C*
045920950518     C                   ENDIF
045930941222     C*
045940950518     C     ENDEM5        TAG
045950950518     C* F12-RITORNO  --> DISALLOCO RECROD
045960950518    1C     *INKL         IFEQ      *ON
045970950518     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045980950518     C     D48FFR        OREQ      'E'
045990950518     C     *IN90         ANDEQ     *ON
046000000303     C     D48FFR        OREQ      'E'
046010000303     C     *IN91         ANDEQ     *ON
046020950518     C*
046030950518    2C     *IN10         IFEQ      *ON
046040941220     C                   UNLOCK    FNARB01L
046050941222     C                   ELSE
046060941222     C                   UNLOCK    FNBLP01L
046070950518    2C                   END
046080051118     C                   UNLOCK    Fiar901L
046090991001     C                   UNLOCK    FIAR601L
046100950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
046110950607     C* flag D48F12 = 1
046120950607     C   KL              MOVEL     '1'           D48F12
046130941222    1C                   END
046140941102     C*
046150950518     C                   ENDSR
046160941102     C*
046170941102     C* EMISSIONE E GESTIONE FORMATO 7 ------------------------------*
046180941102     C     EMFOR7        BEGSR
046190941102     C*
046200941102     C     FOR07         TAG
046210941102     C*
046220950114     C     *IN90         IFEQ      *ON
046230941212     C     D48FFR        OREQ      'S'
046240950607     C     D48FFR        OREQ      'V'
046250941102     C                   WRITE     LR48T01
046260941102     C                   EXFMT     LR48D07
046270941102     C                   END
046280941102     C*
046290941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
046300941102     C                   SETOFF                                       2890
046310941102     C                   SETOFF                                       404142
046320941102     C                   SETOFF                                       434445
046330031001     c                   Setoff                                       62
046340070329     C* F12 - RITORNO
046350941102     C     *INKL         IFEQ      *OFF
046360070329     c
046370070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
046380070329     c                   if        *in87 and *inkr
046390070329     c                   exsr      CALLNOTELDV
046400070329     c                   goto      for07
046410070329     c                   endif
046420941102     C*
046430941102     C* CONTROLLO FORMATO
046440941102     C                   EXSR      CONTR7
046450040219      * questi controlli li devo saltare se il richiamo è stato fatto con causale 'CD'
046460040219      * cioè da chiusura distina
046470040224      * o causale 'CA' creata da LR48 in automatico
046480040224     c                   If        Not *In90 and D48Cvb <> 'CD' and
046490170406     c                             d48cvb <> 'CA'
046500040219     C                   ExSr      Contr72
046510040219     c                   EndIf
046520941215     C*
046530941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
046540070309     c*          non se è forzabile solo allafine dopo la REGIS
046550941215     C     *IN90         IFEQ      *ON
046560941215     C     D48FFR        ANDEQ     'E'
046570070330     c**** Msgforza      andeq     0
046580941215     C                   MOVEL     VIDMSG        D48MSG
046590941215     C                   MOVEL     '7'           D48ERR
046600941215     C                   GOTO      ENDEM7
046610941215     C                   ENDIF
046620070330     c
046630070330     c* Se non ho avuto errori e richiamo solo per controlli, esco
046640070330     c*   (d48ffr='E' e 26 off)
046650070329     c  n90
046660070330     cann26              if        d48ffr='E'
046670070329     C                   GOTO      ENDEM7
046680070329     c                   endif
046690950607     C  NKF
046700950607     CANN26
046710941102     COR 90              GOTO      FOR07
046720060301     c* richiamo routine di richiesta/controllo motivazione
046730060301     c                   if        d66mtv = 'S' and w48mct = *blanks
046740060301     c                   exsr      ges_mtv
046750060301     c* premuto f12-Ritorno
046760060301     c                   if        olv85f12 = '1'
046770060301     c                   if        ilv85wdw = ' '
046780060301     c                   goto      for07
046790060301     c                   else
046800060301     c                   eval      *inkl = '1'
046810060301     C                   GOTO      ENDEM7
046820060301     c                   endif
046830060301     c                   endif
046840060301     c* Errore da ritornare a chiamante
046850060301     C     *IN90         IFEQ      *ON
046860060301     C     D48FFR        ANDEQ     'E'
046870060301     C                   MOVEL     VIDMSG        D48MSG
046880060301     C                   MOVEL     '7'           D48ERR
046890060301     C                   GOTO      ENDEM7
046900060301     C                   ENDIF
046910060301     c                   endif
046920941102     C*
046930070307     C* F6 - AGGIORNAMENTO
046940070307     c* Se devo scrivere file proposte, aggiorno solo FIARP
046950070307     c                   if        AggioARP='S'
046960070307     c                   EXSR      REGISARP
046970070307     c                   endif
046980070308     c
046990070308     c* Aggiorno comunque per gg chiusura e consegne particolari, se
047000070308     c*  modificate, in caso di scrittura di FIARP
047010070308     c                   if        AggioARP=' ' or
047020070308     c                             arbgc1<>vidgc1  or arbgc2<>vidgc2 or
047030151230     c                             arbtc1<>vidtc1 or arbtc2<>vidtc2
047040941102     C                   EXSR      REGIS7
047050070309     c                   else
047060160119     C   10              UNLOCK    FIAR601L
047070160119     C   10              UNLOCK    Fnarb01l
047080160119     C   10              UNLOCK    FiAR901L
047090160119     C                   UNLOCK    Fnblp01l
047100070308     c                   endif
047110070308     c
047120991130     C     *IN91         IFEQ      *ON
047130991130     C     D48FFR        ANDEQ     'E'
047140991130     C                   MOVEL     VIDMSG        D48MSG
047150991130     C                   MOVEL     '7'           D48ERR
047160991130     C                   GOTO      ENDEM7
047170991130     C                   ENDIF
047180991130     C   91              GOTO      FOR07
047190950519     C                   ENDIF
047200070309     c
047210070309     c* Se non ho passato msg vincolanti, passo msg di avvertimento
047220070309     c*  se ce ne sono
047230070309     c                   if        msgforza>0
047240070309     c                   z-aDD     MSGFORZA      XX
047250070309     C                   MOVEL     MSG(XX)       D48MSG
047260070309     C                   MOVEL     'Z'           D48ERR
047270070309     c                   clear                   msgforza
047280070309     C                   ENDIF
047290950519     C*
047300950519     C     ENDEM7        TAG
047310070309     C* F12-RITORNO  --> DISALLOCO REcord
047320941222     C*
047330950519    1C     *INKL         IFEQ      *ON
047340950519     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
047350950519     C     D48FFR        OREQ      'E'
047360950519     C     *IN90         ANDEQ     *ON
047370000303     C     D48FFR        OREQ      'E'
047380000303     C     *IN91         ANDEQ     *ON
047390070329     C     D48FFR        OREQ      'E'
047400070329     c     *in26         andeq     *off
047410950519     C*
047420950519    2C     *IN10         IFEQ      *ON
047430941220     C                   UNLOCK    FNARB01L
047440991005     C                   UNLOCK    FIAR601L
047450051118     C                   UNLOCK    FiAR901L
047460160119     c                   endif
047470160119     C***                ELSE
047480941222     C                   UNLOCK    FNBLP01L
047490160119    2C***                END
047500950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
047510950607     C* flag D48F12 = 1
047520950607     C   KL              MOVEL     '1'           D48F12
047530950519    1C                   END
047540941102     C*
047550950519     C                   ENDSR
047560021129      **
047570021129      * EMISSIONE E GESTIONE FORMATO 7/1 Supermercati ---------------*
047580021129     c     Emfor71       BegSr
047590021129
047600021129     c     For071        Tag
047610021129
047620021129     c                   If        *In90 or D48Ffr = 'S' or D48Ffr = 'V'
047630021129     c                   Write     Lr48T01
047640021129     c                   Exfmt     Lr48D071
047650021129     c                   EndIf
047660021129
047670021129      * Spengo idnicatori di posizionamento cursore
047680021129     c                   Setoff                                       2890
047690021129     c                   Setoff                                       404142
047700021129     c                   Setoff                                       434445
047710021129     c                   Setoff                                       575859
047720031001     c                   Setoff                                       62
047730070329      * F12 - RITORNO
047740021129     c                   IF        Not *InKl
047750070329     c
047760070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
047770070329     c                   if        *in87 and *inkr
047780070329     c                   exsr      CALLNOTELDV
047790070329     c                   goto      for071
047800070329     c                   endif
047810021129
047820021129      * CONTROLLO FORMATO
047830021129     c                   ExSr      Contr7
047840021129
047850021129     c  n90              ExSr      Contr71
047860031028     C  n90              ExSr      Contr72
047870070306     c
047880070306     c* Nel caso dei supermercati devo sempre aggiornare la bolla
047890070306     c*  e mai il file di proposta FIARP per sicurezza lo pulisco
047900070306     c                   clear                   aggioarp
047910070308     c                   clear                   savdcr
047920070308     c                   clear                   savTcr
047930070308     c                   clear                   savHcr
047940070306     c
047950040330      * La data non può superare i gg limiti indicati in tab. VPO
047960040330     c                   If        Not *In90
047970040330     c                   Eval      wdataiso = wdataoggi
047980040330     c                   Adddur    §vpodcr:*d    wdataiso
047990040330     c     *iso          move      wdataiso      ds_data
048000040330     c                   Move      §vpodcr       w003a
048010040330     c                   Movea     w003a         ski(13)
048020040330     c                   Z-add     13            i
048030040330     c                   ExSr      abbl
048040040330     c                   Movea     ski(13)       w003a
048050040330     c                   If        ComDcr > ds_data
048060040330     c                   Eval      *In28 = *On
048070040330     c                   Eval      *In40 = *On
048080040330     c                   Eval      *In90 = *On
048090040330     c                   Eval      VidMsg = msg(129)
048100040330     c                   Eval      VidMsg = %replace(w003a:VidMsg:
048110040330     c                                      %scan('___':VidMsg))
048120040330     c                   EndIf
048130040330     c                   EndIf
048140021129
048150021129      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048160021129     c                   If        *In90 and D48Ffr = 'E'
048170021129     c                   Eval      D48Msg = VidMsg
048180021129     c                   Eval      D48Err = '7'
048190021129     c                   GoTo      EndEm71
048200021129     c                   EndIf
048210140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
048220140502     c*   (d48ffr='E' e 26 off)
048230140502     c  n90
048240140502     cann26              if        d48ffr='E'
048250140502     C                   GOTO      ENDEM71
048260140502     c                   endif
048270021129
048280021129     c  NKF
048290021129     cANN26
048300021129     cOR 90              GoTo      For071
048310060302     c* richiamo routine di richiesta/controllo motivazione
048320060302     c                   if        d66mtv = 'S' and w48mct = *blanks
048330060302     c                   exsr      ges_mtv
048340060302     c                   if        olv85f12 = '1'
048350060302     c                   if        ilv85wdw = ' '
048360060302     c                   goto      for071
048370060302     c                   else
048380060302     c                   eval      *inkl = '1'
048390060302     C                   GOTO      ENDEM71
048400060302     c                   endif
048410060302     c                   endif
048420060302     C     *IN90         IFEQ      *ON
048430060302     C     D48FFR        ANDEQ     'E'
048440060302     C                   MOVEL     VIDMSG        D48MSG
048450070308     C                   MOVEL     '7'           D48ERR
048460060302     C                   GOTO      ENDEM71
048470060302     C                   ENDIF
048480060302     c                   endif
048490021129
048500021129      * CMD6 - AGGIORNAMENTO
048510021129     c                   ExSr      Regis7
048520021129
048530021129     c  n91              ExSr      Regis71
048540021129
048550021129     c                   If        *In91 and D48Ffr = 'E'
048560021129     c                   Eval      D48Msg = VidMsg
048570021129     c                   Eval      D48Err = '7'
048580021129     c                   GoTo      EndEm71
048590021129     c                   EndIf
048600021129
048610021129     c   91              GoTo      For071
048620021129
048630021129     c                   EndIf
048640021129
048650021129     c     EndEm71       Tag
048660021129
048670021129      * F12-RITORNO  --> DISALLOCO RECROD
048680021129
048690021129    1c     *INKL         IFEQ      *ON
048700021129      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048710021129     c     D48FFR        OREQ      'E'
048720021129     c     *IN90         ANDEQ     *ON
048730021129     c     D48FFR        OREQ      'E'
048740021129     c     *IN91         ANDEQ     *ON
048750021129
048760021129    2c     *IN10         IFEQ      *ON
048770021129     c                   UNLOCK    FNARB01L
048780021129     c                   UNLOCK    FIAR601L
048790051118     c                   UNLOCK    FiAR901L
048800021129     c                   ELSE
048810021129     c                   UNLOCK    FNBLP01L
048820021129    2c                   END
048830021129      * Comunico al chiamante che è stato premuto F12 valorizzando il
048840021129      * flag D48F12 = 1
048850021129     c   KL              MOVEL     '1'           D48F12
048860021129    1c                   END
048870021129
048880021129     c                   EndSr
048890941223     C*
048900941223     C* EMISSIONE E GESTIONE FORMATO 8 ------------------------------*
048910941223     C     EMFOR8        BEGSR
048920941223     C*
048930941223     C     FOR08         TAG
048940941223     C*
048950950114     C     *IN90         IFEQ      *ON
048960941223     C     D48FFR        OREQ      'S'
048970950607     C     D48FFR        OREQ      'V'
048980941223     C                   WRITE     LR48T03
048990941223     C                   EXFMT     LR48D08
049000941223     C                   END
049010941223     C*
049020941223     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
049030941223     C                   SETOFF                                       2890
049040941223     C                   SETOFF                                       404142
049050941223     C                   SETOFF                                       434445
049060941223     C                   SETOFF                                       464748
049070941223     C                   SETOFF                                       4950
049080941223     C* CMD12 - RITORNO
049090941223     C     *INKL         IFEQ      *OFF
049100941223     C*
049110941223     C* CONTROLLO FORMATO
049120941223     C                   EXSR      CONTR8
049130941223     C*
049140941223     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049150941223     C     *IN90         IFEQ      *ON
049160941223     C     D48FFR        ANDEQ     'E'
049170941223     C                   MOVEL     VIDMSG        D48MSG
049180941223     C                   MOVEL     '8'           D48ERR
049190941223     C                   GOTO      ENDEM8
049200941223     C                   ENDIF
049210140502
049220140502     c* Non gestita uscita per d48ffr='F' ( n90 and n26 d48ffr='E')
049230140502     c* perchè bisognerebbe verificare bene cosa fare in questo caso
049240140502
049250970110     C  NKF
049260970110     CANNKJ
049270970110     CANN26
049280941223     COR 90              GOTO      FOR08
049290060302     c* richiamo routine di richiesta/controllo motivazione
049300060302     c                   if        d66mtv = 'S' and w48mct = *blanks
049310060302     c                   exsr      ges_mtv
049320060302     c                   if        olv85f12 = '1'
049330060302     c                   if        ilv85wdw = ' '
049340060302     c                   goto      for08
049350060302     c                   else
049360060302     c                   eval      *inkl = '1'
049370060302     C                   GOTO      ENDEM8
049380060302     c                   endif
049390060302     c                   endif
049400060302     C     *IN90         IFEQ      *ON
049410060302     C     D48FFR        ANDEQ     'E'
049420060302     C                   MOVEL     VIDMSG        D48MSG
049430070308     C                   MOVEL     '8'           D48ERR
049440060302     C                   GOTO      ENDEM8
049450060302     C                   ENDIF
049460060302     c                   endif
049470941223     C*
049480970110     C* F6 O F10 - AGGIORNAMENTO
049490941223     C                   EXSR      REGIS8
049500991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049510991130     C     *IN91         IFEQ      *ON
049520991130     C     D48FFR        ANDEQ     'E'
049530991130     C                   MOVEL     VIDMSG        D48MSG
049540991130     C                   MOVEL     '8'           D48ERR
049550991130     C                   GOTO      ENDEM8
049560991130     C                   ENDIF
049570941223     C   91              GOTO      FOR08
049580941223     C*
049590950519     C                   ENDIF
049600150112     c* Se non ho passato msg vincolanti, passo msg di avvertimento
049610150112     c*  se ce ne sono
049620150112     c                   if        msgforzatxt<>*blanks
049630150112     C                   MOVEL     msgforzatxt   D48MSG
049640150112     C                   MOVEL     'Z'           D48ERR
049650150112     c                   clear                   msgforzatxt
049660150112     C                   ENDIF
049670950519     C*
049680950519     C     ENDEM8        TAG
049690950104     C* F12-RITORNO  --> DISALLOCO RECORD
049700950519    1C     *INKL         IFEQ      *ON
049710950519     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049720950519     C     D48FFR        OREQ      'E'
049730950519     C     *IN90         ANDEQ     *ON
049740000303     C     D48FFR        OREQ      'E'
049750000303     C     *IN91         ANDEQ     *ON
049760941223     C*
049770941223     C     *IN10         IFEQ      *ON
049780941223     C                   UNLOCK    FNARB01L
049790991005     C                   UNLOCK    FIAR601L
049800051118     C                   UNLOCK    FiAR901L
049810941223     C                   ELSE
049820941223     C                   UNLOCK    FNBLP01L
049830941223     C                   END
049840950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
049850950607     C* flag D48F12 = 1
049860950607     C   KL              MOVEL     '1'           D48F12
049870941223     C                   END
049880941223     C*
049890950519     C                   ENDSR
049900961211     C* EMISSIONE E GESTIONE FORMATO 9 ------------------------------*
049910961211     C     EMFOR9        BEGSR
049920920120     C*
049930961218     C     FOR09         TAG
049940961218     C*
049950961218     C     *IN90         IFEQ      *ON
049960961218     C     D48FFR        OREQ      'S'
049970961218     C     D48FFR        OREQ      'V'
049980961219     C                   WRITE     LR48T01
049990961218     C                   EXFMT     LR48D09
050000961218     C                   END
050010961218     C*
050020961218     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
050030961218     C                   SETOFF                                       2890
050040970110     C                   SETOFF                                       4041
050050070329     C* F12 - RITORNO
050060961218     C     *INKL         IFEQ      *OFF
050070070329     c
050080070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
050090070329     c                   if        *in87 and *inkr
050100070329     c                   exsr      CALLNOTELDV
050110070329     c                   goto      for09
050120070329     c                   endif
050130070329     C*
050140961218     C*
050150961218     C* CONTROLLO FORMATO
050160961218     C                   EXSR      CONTR9
050170961218     C*
050180961218     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050190961218     C     *IN90         IFEQ      *ON
050200961218     C     D48FFR        ANDEQ     'E'
050210961218     C                   MOVEL     VIDMSG        D48MSG
050220961218     C                   MOVEL     '9'           D48ERR
050230961218     C                   GOTO      ENDEM9
050240961218     C                   ENDIF
050250140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
050260140502     c*   (d48ffr='E' e 26 off)
050270140502     c  n90
050280140502     cann26              if        d48ffr='E'
050290140502     C                   GOTO      ENDEM9
050300140502     c                   endif
050310970110     C  NKF
050320970110     CANN26
050330961218     COR 90              GOTO      FOR09
050340060302     c* richiamo routine di richiesta/controllo motivazione
050350060302     c                   if        d66mtv = 'S' and w48mct = *blanks
050360060302     c                   exsr      ges_mtv
050370060302     c                   if        olv85f12 = '1'
050380060302     c                   if        ilv85wdw = ' '
050390060302     c                   goto      for09
050400060302     c                   else
050410060302     c                   eval      *inkl = '1'
050420060302     C                   GOTO      ENDEM9
050430060302     c                   endif
050440060302     c                   endif
050450060302     C     *IN90         IFEQ      *ON
050460060302     C     D48FFR        ANDEQ     'E'
050470060302     C                   MOVEL     VIDMSG        D48MSG
050480060316     C                   MOVEL     '9'           D48ERR
050490060302     C                   GOTO      ENDEM9
050500060302     C                   ENDIF
050510060302     c                   endif
050520060316     c                   exsr      regis9
050530060316     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050540060316     C     *IN91         IFEQ      *ON
050550060316     C     D48FFR        ANDEQ     'E'
050560060316     C                   MOVEL     VIDMSG        D48MSG
050570060316     C                   MOVEL     '9'           D48ERR
050580060316     C                   GOTO      ENDEM9
050590060316     C                   ENDIF
050600060316     C   91              GOTO      FOR09
050610041028     c
050620961218     C*
050630961218     C                   ENDIF
050640961218     C*
050650961218     C     ENDEM9        TAG
050660961218     C* F12-RITORNO  --> DISALLOCO RECORD
050670961218    1C     *INKL         IFEQ      *ON
050680961218     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050690961218     C     D48FFR        OREQ      'E'
050700961218     C     *IN90         ANDEQ     *ON
050710961218     C*
050720961218     C     *IN10         IFEQ      *ON
050730961218     C                   UNLOCK    FNARB01L
050740991005     C                   UNLOCK    FIAR601L
050750051118     C                   UNLOCK    FiAR901L
050760961218     C                   ELSE
050770961218     C                   UNLOCK    FNBLP01L
050780961218     C                   END
050790961218     C* Comunico al chiamante che è stato premuto F12 valorizzando il
050800961218     C* flag D48F12 = 1
050810961218     C   KL              MOVEL     '1'           D48F12
050820961218     C                   END
050830961211     C*
050840961211     C                   ENDSR
050850030123      **
050860030123      * EMISSIONE E GESTIONE FORMATO 10 Mancato ritiro BANCALI ------*
050870030123     c     Emfor10       BegSr
050880030123
050890030123     c     For10         Tag
050900030123
050910030123     c                   If        *In90 or D48Ffr = 'S' or D48Ffr = 'V'
050920030123     c                   Write     Lr48T01
050930030123     c                   Exfmt     Lr48D10
050940030123     c                   EndIf
050950030123
050960030123      * Spengo indicatori di posizionamento cursore
050970030123     c                   Setoff                                       2890
050980030124     c                   Setoff                                       5759
050990030124     c                   Setoff                                       6061
051000070329      * F12 - RITORNO
051010030123     c                   IF        Not *InKl
051020070329     c
051030070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
051040070329     c                   if        *in87 and *inkr
051050070329     c                   exsr      CALLNOTELDV
051060070329     c                   goto      for10
051070070329     c                   endif
051080030123
051090030123      * CONTROLLO FORMATO
051100030123     c                   ExSr      Contr10
051110030123
051120030123      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051130030123     c                   If        *In90 and D48Ffr = 'E'
051140030123     c                   Eval      D48Msg = VidMsg
051150030123     c                   Eval      D48Err = 'B'
051160030123     c                   GoTo      EndEm10
051170030123     c                   EndIf
051180140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
051190140502     c*   (d48ffr='E' e 26 off)
051200140502     c  n90
051210140502     cann26              if        d48ffr='E'
051220140502     C                   GOTO      ENDEM10
051230140502     c                   endif
051240030123
051250030123     c  NKF
051260030123     cANN26
051270030123     cOR 90              GoTo      For10
051280060302     c* richiamo routine di richiesta/controllo motivazione
051290060302     c                   if        d66mtv = 'S' and w48mct = *blanks
051300060302     c                   exsr      ges_mtv
051310060302     c                   if        olv85f12 = '1'
051320060302     c                   if        ilv85wdw = ' '
051330060302     c                   goto      for10
051340060302     c                   else
051350060302     c                   eval      *inkl = '1'
051360060302     C                   GOTO      ENDEM10
051370060302     c                   endif
051380060302     c                   endif
051390060302     C     *IN90         IFEQ      *ON
051400060302     C     D48FFR        ANDEQ     'E'
051410060302     C                   MOVEL     VIDMSG        D48MSG
051420070308     C                   MOVEL     'B'           D48ERR
051430060302     C                   GOTO      ENDEM10
051440060302     C                   ENDIF
051450060302     c                   endif
051460030123
051470030123      * CMD6 - AGGIORNAMENTO
051480030123     c                   ExSr      Regis10
051490030123
051500030123     c                   If        *In91 and D48Ffr = 'E'
051510030123     c                   Eval      D48Msg = VidMsg
051520030123     c                   Eval      D48Err = 'B'
051530030123     c                   GoTo      EndEm10
051540030123     c                   EndIf
051550030123
051560030123     c   91              GoTo      For10
051570030123
051580030123     c                   EndIf
051590030123
051600030123     c     EndEm10       Tag
051610030123
051620030123      * F12-RITORNO  --> DISALLOCO RECROD
051630030123
051640030123    1c     *INKL         IFEQ      *ON
051650030123      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051660030123     c     D48FFR        OREQ      'E'
051670030123     c     *IN90         ANDEQ     *ON
051680030123     c     D48FFR        OREQ      'E'
051690030123     c     *IN91         ANDEQ     *ON
051700030123
051710030123    2c     *IN10         IFEQ      *ON
051720030123     c                   UNLOCK    FNARB01L
051730030123     c                   UNLOCK    FIAR601L
051740051118     c                   UNLOCK    FiAR901L
051750030124     c                   UNLOCK    Fiar501L
051760030123     c                   ELSE
051770030123     c                   UNLOCK    FNBLP01L
051780030123    2c                   END
051790030123      * Comunico al chiamante che è stato premuto F12 valorizzando il
051800030123      * flag D48F12 = 1
051810030123     c   KL              MOVEL     '1'           D48F12
051820030123    1c                   END
051830030123
051840030123     c                   EndSr
051850061107     C
051860061107     C*--- EMISSIONE E GESTIONE FORMATO 11 ---------------------------*
051870061107     C     EMFOR11       BEGSR
051880061107     C*
051890061107     C     FOR11         TAG
051900061107     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
051910061107    1C     *IN90         IFEQ      *ON
051920061107     C     D48FFR        OREQ      'S'
051930061107     C     D48FFR        OREQ      'V'
051940061107     C                   WRITE     LR48T01
051950061107     C                   EXFMT     LR48D11
051960061107    1C                   END
051970061107     C*
051980061107     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
051990061107     C                   SETOFF                                       2890
052000061107     C                   SETOFF                                       4041
052010061107     C* F12 - RITORNO
052020061107    1C     *INKL         IFEQ      *OFF
052030061107     C*
052040061107     C* CONTROLLO FORMATO
052050061107     C                   EXSR      CONTR11
052060061107     C*
052070061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052080061107    3C     *IN90         IFEQ      *ON
052090061107     C     D48FFR        ANDEQ     'E'
052100061107     C                   MOVEL     VIDMSG        D48MSG
052110061107     C                   MOVEL     'F'           D48ERR
052120061107     C                   GOTO      ENDEM11
052130061107    3C                   ENDIF
052140071025     c
052150071025     c* Se non ho avuto errori e richiamo solo per controlli, esco
052160071025     c*   (d48ffr='E' e 26 off)
052170071025     c  n90
052180071025     cann26              if        d48ffr='E'
052190071025     C                   GOTO      ENDEM11
052200071025     c                   endif
052210061107     C  NKF
052220061107     CANN26
052230061107     COR 90              GOTO      FOR11
052240061107     c* richiamo routine di richiesta/controllo motivazione
052250061107     c                   if        d66mtv = 'S' and w48mct = *blanks
052260061107     c                   exsr      ges_mtv
052270061107     c                   if        olv85f12 = '1'
052280061107     c                   if        ilv85wdw = ' '
052290061107     c                   goto      for11
052300061107     c                   else
052310061107     c                   eval      *inkl = '1'
052320061107     C                   GOTO      ENDEM11
052330061107     c                   endif
052340061107     c                   endif
052350061107     C     *IN90         IFEQ      *ON
052360061107     C     D48FFR        ANDEQ     'E'
052370061107     C                   MOVEL     VIDMSG        D48MSG
052380061107     C                   MOVEL     'F'           D48ERR
052390061107     C                   GOTO      ENDEM11
052400061107     C                   ENDIF
052410061107     c                   endif
052420061107     C*
052430071026     c                   clear                   Aggiobol
052440061107     C                   EXSR      REGIS11
052450061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052460061107     C     *IN91         IFEQ      *ON
052470061107     C     D48FFR        ANDEQ     'E'
052480061107     C                   MOVEL     VIDMSG        D48MSG
052490061107     C                   MOVEL     'F'           D48ERR
052500061107     C                   GOTO      ENDEM11
052510061107     C                   ENDIF
052520061107     C   91              GOTO      FOR11
052530061107     C*
052540061107    1C                   ENDIF
052550061107     C*
052560061107     C     ENDEM11       TAG
052570061107     C*
052580061107    1C     *INKL         IFEQ      *ON
052590061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052600061107    1C     D48FFR        OREQ      'E'
052610061107     C     *IN90         ANDEQ     *ON
052620061107    1C     D48FFR        OREQ      'E'
052630061107     C     *IN91         ANDEQ     *ON
052640061107     C* F12-RITORNO
052650061107    2C     *IN10         IFEQ      *ON
052660061107     C                   UNLOCK    FNARB01L
052670061107     C                   UNLOCK    FiAR901L
052680061107     C                   ELSE
052690061107     C                   UNLOCK    FNBLP01L
052700061107    2C                   END
052710061107     C* Comunico al chiamante che è stato premuto F12 valorizzando il
052720061107     C* flag D48F12 = 1
052730061107     C   KL              MOVEL     '1'           D48F12
052740061107    1C                   END
052750061107     c
052760061107     C                   UNLOCK    Fiar601l
052770061107     C*
052780061107     C                   ENDSR
052790130927     c
052800130927     C*--- EMISSIONE E GESTIONE FORMATO 12  variaz.'"Nx' -------------*
052810130927     C     EMFOR12       BEGSR
052820130927     C*
052830130927     C     FOR12         TAG
052840130927     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
052850130927    1C     *IN90         IFEQ      *ON
052860130927     C     D48FFR        OREQ      'S'
052870130927     C     D48FFR        OREQ      'V'
052880130927     C                   WRITE     LR48T01
052890130927     C                   EXFMT     LR48D12
052900130927    1C                   END
052910130927     C*
052920130927     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
052930130927     C                   SETOFF                                       2890
052940130930     C                   SETOFF                                       41
052950130927     C* F12 - RITORNO
052960130927    1C     *INKL         IFEQ      *OFF
052970130927     c
052980130927     c* F17 - Note LDV   (solo se abilitato geo 87 on)
052990130927     c                   if        *in87 and *inkr
053000130927     c                   exsr      CALLNOTELDV
053010130927     c                   goto      for12
053020130927     c                   endif
053030130927     C*
053040130927     C* CONTROLLO FORMATO
053050130927     C                   EXSR      CONTR12
053060130927     C*
053070130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
053080130927    3C     *IN90         IFEQ      *ON
053090130927     C     D48FFR        ANDEQ     'E'
053100130927     C                   MOVEL     VIDMSG        D48MSG
053110130927     C                   MOVEL     'F'           D48ERR
053120130927     C                   GOTO      ENDEM12
053130130927    3C                   ENDIF
053140130927     c
053150130927     c* Se non ho avuto errori e richiamo solo per controlli, esco
053160130927     c*   (d48ffr='E' e 26 off)
053170130927     c  n90
053180130927     cann26              if        d48ffr='E'
053190130927     C                   GOTO      ENDEM12
053200130927     c                   endif
053210130927     C  NKF
053220130927     CANN26
053230130930     COR 90              GOTO      FOR12
053240130927     c* richiamo routine di richiesta/controllo motivazione
053250130927     c                   if        d66mtv = 'S' and w48mct = *blanks
053260130927     c                   exsr      ges_mtv
053270130927     c                   if        olv85f12 = '1'
053280130927     c                   if        ilv85wdw = ' '
053290130927     c                   goto      for12
053300130927     c                   else
053310130927     c                   eval      *inkl = '1'
053320130927     C                   GOTO      ENDEM12
053330130927     c                   endif
053340130927     c                   endif
053350130927     C     *IN90         IFEQ      *ON
053360130927     C     D48FFR        ANDEQ     'E'
053370130927     C                   MOVEL     VIDMSG        D48MSG
053380130927     C                   MOVEL     'F'           D48ERR
053390130927     C                   GOTO      ENDEM12
053400130927     C                   ENDIF
053410130927     c                   endif
053420130927     C*
053430130927     c                   clear                   Aggiobol
053440130927     C                   EXSR      REGIS12
053450130927     c
053460130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
053470130927     C     *IN91         IFEQ      *ON
053480130927     C     D48FFR        ANDEQ     'E'
053490130927     C                   MOVEL     VIDMSG        D48MSG
053500130927     C                   MOVEL     'F'           D48ERR
053510130927     C                   GOTO      ENDEM12
053520130927     C                   ENDIF
053530130927     C   91              GOTO      FOR12
053540130927     C*
053550130927    1C                   ENDIF
053560130927     C*
053570130927     C     ENDEM12       TAG
053580130927     C*
053590130927    1C     *INKL         IFEQ      *ON
053600130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
053610130927    1C     D48FFR        OREQ      'E'
053620130927     C     *IN90         ANDEQ     *ON
053630130927    1C     D48FFR        OREQ      'E'
053640130927     C     *IN91         ANDEQ     *ON
053650130927     C* F12-RITORNO
053660130927    2C     *IN10         IFEQ      *ON
053670130927     C                   UNLOCK    FNARB01L
053680130927     C                   UNLOCK    FiAR901L
053690130927     C                   ELSE
053700130927     C                   UNLOCK    FNBLP01L
053710130927    2C                   END
053720130927     C* Comunico al chiamante che è stato premuto F12 valorizzando il
053730130927     C* flag D48F12 = 1
053740130927     C   KL              MOVEL     '1'           D48F12
053750130927    1C                   END
053760130927     c
053770130927     C                   UNLOCK    Fiar601l
053780130927     C*
053790130927     C                   ENDSR
053800070329     C
053810070329     C*--- richiama pgm per immettere note LDV -----------------------*
053820070329     C     CALLNoteLDV   BEGSR
053830140130     c                   clear                   fnlrn6ds
053840140130     c                   eval      in6mod='I'
053850140130     c                   eval      in6aas=arbaas
053860140130     c                   eval      in6lnp=arblnp
053870140130     c                   eval      in6nrs=arbnrs
053880140130     c                   eval      in6nsp=arbnsp
053890140130     c                   eval      in6pgm='FNLR48R'
053900140130     c                   eval      IN6FGS=arbifp
053910140130     c                   eval      IN6NFV=arbndc
053920140130     c                   eval      IN6DFV=arbddc
053930140130     c                   eval      IN6PDR=arbpdc
053940140130     c                   if        arbdcm=0  and arbndc>0
053950140130     c                   eval      IN6CONS='S'
053960140130     c                   endif
053970140130     c                   call      'FNLRN6R'
053980070329     c                   parm                    kpjba
053990140130     c                   parm                    fnlrn6ds
054000070329     c
054010070329     c                   ENDSR
054020061107     C
054030920120     C*--- CONTROLLI FORMATO2 ----------------------------------------*
054040920120     C     CONTR2        BEGSR
054050941025     C                   SETOFF                                       9028
054060020531     c                   clear                   wdisag
054070030122      * A T T E N Z I O N E!!!
054080030122      * Per la modifica destinatario
054090030122      * Se il nuovo destinatario necessita della consegna particolare
054100030122      * alla fine della REGIS2 viene impostata la causae 'CP' per
054110030122      * poter effettuare una nuova variazione bolla senza uscire dal pgm
054120941020     C***
054130941020     C*** DESTINATARIO  ***
054140941025    1C     *IN01         IFEQ      '1'
054150141112
054160141112     c* messaggio di avviso se pgm non richiamato e bolla con
054170141120     c* giacenza aperta  - Solo se variati dati del destinatario
054180141112     c                   if        *in20=*off and wforzgiac=0
054190141112     c                             and *in04
054200141120    5C     SAVCAD        IFNE      VIDCAD
054210141120     C     SAVLOD        ORNE      VIDLOD
054220141120     C     SAVind        ORNE      VIDind
054230141120     C     SAVprd        ORNE      VIDprd
054240141120     C     SAVnzd        ORNE      VIDnzd
054250141112     c                   eval      wforzgiac=1
054260141112     C                   SETON                                        2890
054270141112     C                   MOVEL     MSG(157)      VIDMSG
054280141112     C                   GOTO      ENDCT2
054290141112     c                   endif
054300141120     c                   endif
054310141112
054320970808     C                   MOVEL     VIDRSD        I14RSC
054330970808     C                   MOVE      VIDRSD        I14RS2
054340970808     C                   MOVEL     VIDIND        I14IND
054350970808     C                   MOVEL     VIDLOD        E14LOC
054360970808     C                   MOVEL     VIDPID        E14PIP
054370970808     C                   MOVEL     VIDPRD        E14PRV
054380970808     C                   MOVEL     VIDNZD        E14NAR
054390970808     C                   MOVEL     VIDCAD        E14CAP
054400970808     C                   MOVEL     VIDFIN        I14ISO
054410970808     C                   MOVEL     'D'           WCLI              1
054420970808     C                   EXSR      CTRIND
054430941020     C*
054440941020     C* CAMPI CHE POSSO AVER IMPOSTATO
054450970808     C                   MOVEL     E14LOC        VIDLOD
054460970808     C                   MOVEL     E14PIP        VIDPID
054470970808     C                   MOVEL     E14PRV        VIDPRD
054480970808     C                   MOVEL     E14NAR        VIDNZD
054490970808     C                   MOVEL     E14CAP        VIDCAD
054500970808     C* ERRORE
054510970808     C   90              GOTO      ENDCT2
054520060111     c* salvo lna desunta da cappario per non sporcarla se destinatario
054530060111     c* che richiede una particolare lna diversa da quella del cappario
054540060111     c                   move      o95lna        d_o95lna
054550020531     c* Verifico se destinatario disagiato
054560020604     c* Per bolle poste non faccio niente
054570060111     c                   clear                   ddstflo
054580020604     c     lnpntw        ifne      'PPT'
054590040702      * Referente e numero di telefono: se campi a video con '#'
054600040702      * sono quelli automatici (dal tisit5) e quindi li pulisco
054610040702     c                   If        %subst(VidRef:1:1) = '#'
054620040702     c                   Clear                   VidRef
054630040702     c                   EndIf
054640040705     c                   If        %subst(VidTeld:1:1) = '#'
054650040702     c                   Clear                   VidTeld
054660040702     c                   EndIf
054670020531     c                   clear                   tisit0ds
054680020531     c                   movel     'E'           it0tla
054690020531     c                   movel     vidnzd        it0naz
054700020531     c                   movel     vidprd        it0prv
054710020531     c                   movel     vidcad        it0cap
054720020531     c                   movel     vidlod        it0loc
054730020531     c                   movel     vidind        it0ind
054740020531     c                   movel     vidrsd        it0rag
054750020531     c                   call      'TISIT5R'
054760020531     c                   parm                    tisit0ds
054770020531     c     ot0err        ifeq      *blanks
054780060113     c                   movel     ot0flo        ddstflo
054790060111   2ac                   if        ot0dos = 'A' or ot0dos = 'D' or
054800060111     c                             ot0dos = 'S'
054810020531     c                   movel     ot0dos        wdisag
054820020531     c     wdisag        ifeq      'D'
054830020531     c                   movel     'X'           wdisag
054840020531     c                   endif
054850060111     c                   endif
054860020531     c                   else
054870020531     c                   clear                   wdisag
054880020531     c                   endif
054890060327     c***                If        wdisag <> *Blanks
054900060327     c***                          and %subst(arbgva:1:1) <> 'U' or
054910060327     c***                          wdisag='X' and %subst(arbgva:1:1)='U'
054920060327     c                   if        (wdisag = 'X') OR
054930060327     c                             (wdisag='S' and %subst(arbGva:1:1)<>'U') OR
054940060327     c                             (Wdisag='A' and %subst(arbGva:1:1)<>'U' and
054950060327     c                             (arbdcr=*zeros or arbtcr <> *blanks))
054960040702     c                   If        VidTeld = *Blanks
054970040702     c                   Eval      VidTeld = ot0tel
054980040702     c                   EndIf
054990040702     c                   If        VidRef = *Blanks
055000040702     c                   Eval      VidRef =  ot0ref
055010040702     c                   EndIf
055020040702     c                   EndIf
055030020604     c                   endif
055040060113     c*
055050060113     c                   if        §dstlna > *zeros
055060060111     c                   move      §dstlna       d_o95lna
055070060111     c                   endif
055080970808     C*
055090970808     C* MODIFICATI CAP E/O LOCALITA':
055100970808     C* 1) AGGIORNO A VIDEO IL FLAG INOLTRO E AVVISO CON MESSAGGIO
055110970808     C* 2) SE VARIA ANCHE LA LNA E CAP E LOCALITA SONO DIVERSI DALLA
055120970808     C*    BOLLA EMMETTO MESSAGGIO FORZABILE MANTENENDO COMUNQUE LA
055130970808     C*    LINEA DI ARRIVO E ZONA DELLA BOLLA
055140970808     C     WCAD          IFNE      VIDCAD
055150970808     C     WLOD          ORNE      VIDLOD
055160970808     C                   MOVE      VIDCAD        WCAD
055170970808     C                   MOVE      VIDLOD        WLOD
055180970808     C* FLAG INOLTRO
055190970808     C     O95ISO        IFNE      VIDFIN
055200970808     C                   MOVE      O95ISO        VIDFIN
055210140507     c* NON avviso se richiamo cieco
055220140507     c                   if        d48ffr<>'E'
055230970808     C                   SETON                                        289049
055240970808     C                   MOVEL     MSG(77)       VIDMSG
055250970808     C                   GOTO      ENDCT2
055260140507     c                   endif
055270970808     C                   END
055280970808     C                   END
055290970828     C* FLAG INOLTRO
055300970828     C     VIDFIN        IFEQ      ' '
055310970828     C                   MOVE      O95ISO        VIDFIN
055320140507     c* NON avviso se richiamo cieco
055330140507     c                   if        d48ffr<>'E'
055340970828     C                   SETON                                        289049
055350970828     C                   MOVEL     MSG(77)       VIDMSG
055360010412     C                   GOTO      ENDCT2
055370140507     c                   endif
055380970828     C                   END
055390010412     C  N29
055400010412     CANN71VIDFIN        IFEQ      'D'
055410010412     C     O95PID        ANDEQ     'N'
055420010412     C                   SETON                                        289049
055430010412     C                   MOVEL     MSG(98)       VIDMSG
055440010412     C                   GOTO      ENDCT2
055450010412     C                   ENDIF
055460970808     C*
055470140507     c* Non faccio il seguente controllo se richiamo "cieco"
055480141029     c                   if        %lookup('999':skFilOkdir)=0 and
055490141029     c                             (%lookup(%editc(arblna:'X'):skFilOkdir)=0
055500141029     c                             or %lookup(%editc(dutpou:'X'):skFilOkdir)=0)
055510141029     c                   if        d48ffr<>'E'
055520141029     C     WCAD1         IFNE      VIDCAD
055530141029     C     WLOD1         ORNE      VIDLOD
055540141029     C     Wrsd1         ORNE      VIDrsd
055550141029     C     Wprd1         ORNE      VIDprd
055560141029     C     Wnzd1         ORNE      VIDnzd
055570141029     C     Wind1         ORNE      VIDind
055580141029     C                   MOVE      VIDCAD        WCAD1
055590141029     C                   MOVE      VIDLOD        WLOD1
055600141029     C                   MOVE      VIDrsd        Wrsd1
055610141029     C                   MOVE      VIDind        Wind1
055620141029     C                   MOVE      VIDprd        Wprd1
055630141029     C                   MOVE      VIDnzd        Wnzd1
055640141029     C     d_o95lna      ifne      ARBLNA
055650141029     C     SAVCAD        IFNE      VIDCAD
055660060111     C***  O95LNA        ANDNE     ARBLNA
055670141029     C     SAVLOD        ORNE      VIDLOD
055680060111     C***  O95LNA        ANDNE     ARBLNA
055690141029     C     SAVrsd        ORNE      VIDrsd
055700141029     C     SAVind        ORNE      VIDind
055710141029     C     SAVprd        ORNE      VIDprd
055720141029     C     SAVnzd        ORNE      VIDnzd
055730141029     C                   SETON                                        9028
055740141029     C                   MOVEL     MSG(76)       VIDMSG
055750141029     C                   GOTO      ENDCT2
055760141029     C                   END
055770141029     C                   END
055780141029     C                   END
055790141029     c                   endif
055800141029     c                   endif
055810941020     C**
055820950522     C* Se P.IVA sprotetta e nazione estera con flag efta la P.IVA è
055830950522     C* obbligatoria
055840950412     C     *IN05         IFEQ      *OFF
055850950412     C     *IN72         ANDEQ     *OFF
055860950522     C     WDESIE        ANDEQ     'E'
055870950522     C     DESEFT        ANDNE     *BLANKS
055880950412     C     VIDPID        ANDEQ     *BLANKS
055890950412     C                   SETON                                        284590
055900950412     C                   MOVEL     MSG(67)       VIDMSG
055910950412     C                   GOTO      ENDCT2
055920950412     C                   END
055930061116     c
055940061116     c* Se modifica la ragione sociale del destinatario, controllare
055950061116     c*  la partita iva
055960140507     c* Non controllo se chiamata pgm richiamato "cieco"
055970140507     c                   if        d48ffr<>'E'
055980061116     c                   if        vidrsd<>savrsdQ and vidcdf<>*blanks
055990061116     C                   SETON                                        285190
056000061116     C                   MOVEL     MSG(134)      VIDMSG
056010061116     c                   movel     vidrsd        savrsdQ
056020061116     C                   GOTO      ENDCT2
056030061116     C                   END
056040140507     c                   endif
056050061116     c
056060061116     c* La memorizzo anche se cdf=*blanks
056070061116     c                   movel     vidrsd        savrsdQ
056080061115     c
056090061115      * Devo richiamare il nuovo driver fatto per controllare il codice
056100061115      * fiscale
056110061115     c                   if        vidcdf<>*blanks
056120151105     c                   clear                   xcfiva1ds
056130061115     c                   clear                   xcfivamod
056140061115     c                   eval      xcfivapar = vidcdf
056150061115     c                   eval      xcfivanar = vidnzd
056160061115     c                   eval      xcfivaprv = vidprd
056170151105     c                   eval      xcfivacap = vidcad
056180151105     c                   eval      xcfivaloc = vidlod
056190151105     c                   eval      xcfivapiva= vidpid
056200151105     c                   call      'XCFIVAR1'
056210151105     c                   parm                    xcfiva1ds
056220061115     c*
056230061115     c                   if        xcfivaflg < *zeros
056240061115     C                   SETON                                        289051
056250080505     C                   MOVEL     xcfivamsg     VIDMSG
056260061115     C                   GOTO      ENDCT2
056270061115     c                   endif
056280061115     c                   endif
056290120508     c* Parita iva $$ non ammessa per fattura di filiale
056300120508     c                   move      v1cksc        w0040
056310120508     c                   if        %scan('$$':vidpid)>0  and w0040=9999
056320120508     C                   SETON                                        284590
056330120508     C                   MOVEL     MSG(149)      VIDMSG
056340120508     C                   GOTO      ENDCT2
056350120508     c                   endif
056360070508     c
056370140408     c
056380070508     c                   movel     vidffd        wffd
056390141029     c* Il fermo deposito non si può inserire se presente dispo di dirottamento da
056400141029     c* eseguire
056410141029     c                   if        vidffd<>*blanks and arbffd<>wffd
056420141029     c                   clear                   fnlry09ds2
056430141029     c                   eval      ILRY09TCH='T'
056440141029     c                   eval      ILRY09TDIS='D'
056450141029     c                   eval      ILRY09AAS=d48aas
056460141029     c                   eval      ILRY09LNP=d48lnp
056470141029     c                   eval      ILRY09NRS=d48nrs
056480141029     c                   eval      ILRY09NSP=d48nsp
056490141029     c                   eval      kpjbu=fnlry09ds2
056500141031     c                   call      'FNLRY09C'
056510141029     c                   parm                    kpjba
056520141029     c                   eval      fnlry09ds2=kpjbu
056530141029     c                   if        OLRY09ESIT='1'
056540141029     C                   SETON                                        286890
056550141029     C                   MOVEL     MSG(155)      VIDMSG
056560141029     C                   GOTO      ENDCT2
056570141029     c                   endif
056580141029     c                   endif
056590140408     c* Il fermo deposito non si può inserire se presente particolarità consegna
056600140408     c* che prevede che il fermo deposito debba essere autorizzato dal mittente
056610140408     c                   if        vidffd<>*blanks and arbffd<>wffd and
056620140408     c                             arbgma<>*blanks
056630140408     c                   Movel     '7R'          cod
056640140408     c                   Movel(p)  ArbGMa        key
056650140408     c                   ExSr      ctabel
056660140408     c  n30              Movel     TblUni        ds7R
056670140408     c   30              Clear                   ds7R
056680140408     C                   IF        §7RFD='S' AND vidcvr<>'I7'
056690140408     C                   SETON                                        289068
056700140408     C                   MOVEL     MSG(153)      VIDMSG
056710140408     C                   GOTO      ENDCT2
056720140408     C                   ENDIF
056730140408     c                   endif
056740140408     c* Da contattare impostabile solo se immesso il fermo deposito
056750140408     c*  solo se GEO attiva
056760141223     c* NON faccio questo controllo se richiamo cieco
056770141223     c                   if        d48ffr<>'E'
056780140213     c   77              if        (arbffd=wffd or  vidffd='  ') and
056790070508     c                             vidtfd='SI'
056800070508     C                   SETON                                        289052
056810070508     C                   MOVEL     MSG(99)       VIDMSG
056820070508     C                   GOTO      ENDCT2
056830070508     c                   endif
056840141223     c                   endif
056850070508     C
056860070508     c* se immesso fermo deposito e c'e' la consegna richiesta avviso
056870140507     c* NON faccio questo controllo se richiamo cieco
056880140507     c                   if        d48ffr<>'E'
056890140213     c   77              if        vidffd='SI' and arbffd<>wffd and
056900070508     c                             vidtfd='  ' and arbdcr>0  AND WZ3=0
056910070508     C                   SETON                                        289052
056920070508     C                   MOVEL     MSG(143)      VIDMSG
056930070508     C                   EVAL      WZ3=1
056940070508     C                   GOTO      ENDCT2
056950140213     c                   endif
056960140507     c                   endif
056970061115     c
056980941025    1C                   ENDIF
056990941019     C*
057000930505     C***
057010941025    1C     *IN02         IFEQ      '1'
057020930505     C* PESO
057030030203      * Se non è protetto
057040030203   2ac                   If        Not *In88
057050930505     C* NON A 0 O MAGGIORE DEL LIMITE POSTO
057060980406     C     VIDPKF        IFEQ      0
057070941020     C                   SETON                                        479028
057080941020     C                   MOVEL     MSG(2)        VIDMSG
057090911209     C                   GOTO      ENDCT2
057100941025    2C                   END
057110000510     C* DPD NO MAGGIORE DEL LIMITE IMPOSTO
057120020318    2C     LNPNTW        IFEQ      'DPD'
057130020318     C     LNANTW        OREQ      'DPD'
057140000510    3C     VIDPKF        IFGT      §3IPKD
057150000510     C                   SETON                                        479028
057160000510     C                   MOVEA     MSG(52)       K78
057170000510     C                   MOVEL     §3IPKD        W006A             6
057180000510     C                   MOVEA     W006A         SKI(10)
057190000510     C                   Z-ADD     10            I                 3 0
057200000510     C                   EXSR      ABBL
057210000510     C                   MOVEA     SKI(10)       K78(53)
057220000510     C                   MOVE      §3IPKD        W001A
057230000510     C                   MOVEL     W001A         K78(60)
057240000510     C                   MOVEA     K78           VIDMSG
057250000510     C                   GOTO      ENDCT2
057260000510    3C                   END
057270000510    2C                   END
057280000510     C**
057290000510     C**
057300000510     C     VARPKF        IFNE      VIDPKF
057310000510     C                   CLEAR                   WPT1
057320000510     C                   ENDIF
057330000510     C*
057340000510     C     VARPKF        IFNE      VIDPKF
057350000510     C     ARBFVF        ANDEQ     'K'
057360000510     C                   CLEAR                   SET
057370000510     C                   ENDIF
057380000510     C                   MOVEL     VIDPKF        VARPKF
057390000510     C**
057400021121
057410021121     c                   Move      ArbCtr        DueCtr
057420021121
057430021121      * Bolla FedEx con tariffa documenti il peso non può essere superiore a
057440021121      * quanto indicato in tabella "DFE" --> msg forzabile
057450140507     c* Non controllo se progamma richiamato in modalità "cieca"
057460140507     c                   if        d48ffr<>'E'
057470021121If  2c                   If        LnaNtw = 'FED' and DueCtr >= §DfeDalD and
057480021121     c                             DueCtr <= §DfeAlD and VidPkf > §DfePkgD
057490021121     c                             and Wpt1 = *Blanks
057500021121     c                   Eval      *In47 = *On
057510021121     c                   Eval      *In28 = *On
057520021121     c                   Eval      *In90 = *On
057530021121      * Preparo il msg
057540021121     c                   Movea     Msg(116)      K78
057550021121     c                   Movel     §DfePkgD      W006a
057560021121     c                   Movea     W006a         Ski(10)
057570021121     c                   Z-Add     10            i
057580021121     c                   ExSr      Abbl
057590021121     c                   Movea     Ski(10)       K78(50)
057600021121     c                   Move      §DfePkgD      W001a
057610021121     c                   Movel     W001a         K78(57)
057620021121     c                   Movea     K78           VidMsg
057630021121     c                   Eval      Wpt1 = '1'
057640021121     c                   GoTo      EndCt2
057650021121    2c                   EndIf
057660140507     c                   endif
057670051129      * Nel caso di peso totalmente rilevato da VDL posso modificare solo se
057680051129      * inserisco il peso uguale a quello VDL
057690030123     c**!!!              If         ArbNcp = ArbNcl and VidPkc <> VidPkf
057700030123     c                   If         wfpf = 'T' and VidPkc <> VidPkf
057710030122     c                   Seton                                        479028
057720030122     c                   Eval      VidMsg = Msg(121)
057730030122     c                   GoTo      EndCt2
057740030122    2c                   EndIf
057750051129      * Nel caso di peso parzialmente rilevato da VDL posso immettere un peso uguale
057760051129      * o superiore a quello VDL
057770030123     c                   If        wfpf = 'Z' and VidPkf < VidPkc
057780030123     c                   Seton                                        479028
057790030123     c                   Eval      VidMsg = Msg(123)
057800030123     c                   GoTo      EndCt2
057810030123    2c                   EndIf
057820030203   2ac                   EndIf
057830021121
057840930505     C* VOLUME
057850091124      * Se non è protetto
057860091124   2ac                   If        Not *In89
057870091124     c
057880980406     C     VIDVLF        IFEQ      0
057890941215     C                   SETON                                        489028
057900941020     C                   MOVEL     MSG(3)        VIDMSG
057910911209     C                   GOTO      ENDCT2
057920941025    2C                   ENDIF
057930941214     C*
057940941027     C* SE IL TIPO VOLUME E' "Z" NON SI PUO' ABBASSARE
057950941027     C     VIDFVF        IFEQ      'Z'
057960941027     C     VIDVLF        ANDLT     ARBVLF
057970941215     C                   SETON                                        489028
057980941027     C                   MOVEL     MSG(5)        VIDMSG
057990941027     C                   GOTO      ENDCT2
058000941027    2C                   ENDIF
058010091124     c*
058020091124     c* Per volume totaole variabile solo come il totale
058030091124     c                   If         ArbNcr = ArbNcl and Vidvlf <> arbvlc
058040091124     c                   Seton                                        489028
058050091124     c                   Eval      VidMsg = Msg(147)
058060091124     c                   GoTo      EndCt2
058070091124    2c                   EndIf
058080091124     c
058090091124      * Nel caso di vol  parzialmente rilevato da VDL posso immettere un vol  uguale
058100091124      * o superiore a quello VDL
058110091124     c                   If         ArbNcr < ArbNcl and Vidvlf < arbvlc
058120091124     c                              and arbncr>0
058130091124     c                   Seton                                        489028
058140091124     c                   Eval      VidMsg = Msg(148)
058150091124     c                   GoTo      EndCt2
058160091124    2c                   EndIf
058170091124   2ac                   EndIf
058180941214     C*
058190051129     C* SE IL TIPO VOLUME NON E' "Z" MA ESISTE UN VOLUME VDL CHE DIVENT
058200941214     C*  SUPERIORE A QUELLO DA FATTURARE DOPO LA VARIAZIONE, AVVISO E
058210941215     C*  MODIFICO
058220941215     C     VARVLF        IFNE      VIDVLF
058230941215     C                   CLEAR                   SET
058240941215     C                   MOVEL     VIDVLF        VARVLF
058250941215     C                   ENDIF
058260941215     C*
058270051129     C*  POI IMPOSTERO NEL VOLUME DA FATTURARE IL VOLUME VDL
058280941214    2C     SET           IFEQ      0
058290941214     C     ARBVLC        ANDGT     0
058300941214     C     VIDFVF        ANDNE     'Z'
058310941214     C     VIDFVF        ANDNE     'T'
058320941214     C*
058330051129    3C     WVDL          IFEQ      'S'
058340970418    4C     WFVF          IFEQ      'Z'
058350941222     C   06              MOVEL     ZAFA          WSOST             1
058360941222     C  N06              MOVEL     ZAAA          WSOST
058370970418   X4C                   ELSE
058380970418     C   06              MOVEL     TAFA          WSOST             1
058390970418     C  N06              MOVEL     TAAA          WSOST
058400970418    4C                   END
058410970418   X3C                   ELSE
058420970418    4C     WFVF          IFEQ      'Z'
058430941222     C   06              MOVEL     ZAFN          WSOST             1
058440941222     C  N06              MOVEL     ZAAN          WSOST
058450970418     C                   ELSE
058460970418     C   06              MOVEL     TAFN          WSOST             1
058470970418     C  N06              MOVEL     TAAN          WSOST
058480970418    4C                   ENDIF
058490970418    3C                   ENDIF
058500941214     C*
058510941214     C* SE VOLUME SAREBBE DA SOSTITUIRE VEDO SE DIVENTA SUPERIORE
058520941214     C* DEL VOLUME DA FATTURARE
058530941214    3C     WSOST         IFEQ      'S'
058540970418     C                   CLEAR                   WSOSC
058550941214     C*
058560941214    4C     VIDVLF        IFLT      ARBVLF
058570941214     C     VIDVLF        ANDLT     ARBVLC
058580970418     C* PRENDO FLAG DI SOSTITUIBILITA' VOLUME REALE
058590970418     C                   MOVEL     '7T'          COD
058600970418     C                   MOVEL(P)  'R'           KEY
058610970418     C                   EXSR      CTABEL
058620970418     C   30              CLEAR                   DS7T
058630970418     C  N30              MOVEL     TBLUNI        DS7T
058640970418     C     WFVF          IFEQ      'Z'
058650970418     C                   MOVEL     §7TSOZ        WSOSC
058660970418     C                   ELSE
058670970418     C                   MOVEL     §7TSOT        WSOSC
058680970418     C                   END
058690970418     C* (SOLO SE IL VOLUME E' SOSTITUIBILE)
058700970418    5C     WSOSC         IFEQ      'S'
058710140507     C                   ADD       1             SET
058720140507     c* non segnalo se pgm richiamato in modalità "cieca"
058730140507     c                   if        d48ffr<>'E'
058740941214     C                   SETON                                        482890
058750941214     C                   MOVEL     MSG(58)       VIDMSG
058760941214     C                   GOTO      ENDCT2
058770140507     c                   endif
058780970418    5C                   ENDIF
058790941214    4C                   ENDIF
058800941214     C*
058810941214     C* SE VOLUME AUTOMATICO E MODIFICATO IL PESO CONTROLLO
058820970418   4AC     *IN88         IFEQ      *OFF
058830941214     C                   Z-ADD     VIDVLF        D20VLU
058840941214     C                   EXSR      VOLAUT
058850970418    4C     D20VLU        IFLT      ARBVLC
058860970418     C                   MOVEL     '7T'          COD
058870970418     C                   MOVEL(P)  ARBFVF        KEY
058880970418     C                   EXSR      CTABEL
058890970418     C   30              CLEAR                   DS7T
058900970418     C  N30              MOVEL     TBLUNI        DS7T
058910051129     C* FLAG DI SOSTITUIBILITA' DA VOL.VDL
058920970418     C     WFVF          IFEQ      'Z'
058930970418     C                   MOVEL     §7TSOZ        WSOSC
058940970418     C                   ELSE
058950970418     C                   MOVEL     §7TSOT        WSOSC
058960970418     C                   END
058970970418    5C     WSOSC         IFEQ      'S'
058980140507     C                   ADD       1             SET
058990140507     c* non segnalo se pgm richiamato in modalità "cieca"
059000140507     c                   if        d48ffr<>'E'
059010941215     C                   SETON                                        472890
059020941214     C                   MOVEL     MSG(58)       VIDMSG
059030941214     C                   GOTO      ENDCT2
059040140507     c                   endif
059050970418    5C                   ENDIF
059060941214    4C                   ENDIF
059070970418   4AC                   ENDIF
059080941214    3C                   ENDIF
059090941214    2C                   ENDIF
059100170130     c* controllo peso e volume con i limiti previsti in tabella "TSP"
059110170130     c* (limiti per tipo servizio)
059120170131     C                   if        not *in88  or not *in89
059130170130     c                   exsr      sr_limtsp
059140170130     c                   if        *in90
059150170130     c                   seton                                        28
059160170130     C                   MOVEL     wmsgtsp       VIDMSG
059170170130     C                   GOTO      ENDCt2
059180170130     c                   endif
059190170130     c                   endif
059200941214     C*
059210941027    1C                   ENDIF
059220910429     C*
059230910429     C     ENDCT2        ENDSR
059240170130     C**************************************************************************
059250170130     C* Controllo limiti (peso e volume) per tipo servizio
059260170130     C**************************************************************************
059270170130     C     sr_limtsp     BEGSR
059280170130     c                   clear                   wmsgtsp
059290170130     c                   clear                   trul73ds
059300170130     c                   eval      i73tsp=arbtsp
059310170130     c                   eval      i73psk=vidpkf
059320170130     c                   eval      i73vmc=vidvlf
059330170130     c                   eval      i73ncl=arbncl
059340170130     c                   call      'TRUL73R'
059350170130     c                   parm                    trul73ds
059360170130     c                   select
059370170130     c                   when      o73err='E'
059380170130     c                   seton                                        90
059390170130     c                   eval      wmsgtsp=msg(163)
059400170130     c                   eval      %subst(wmsgtsp: 40: 8)=v1dtsp
059410170130     c* peso kg bloccante
059420170130     c                   when      O73FKGM='1'  and not *in88
059430170131     c                                          and wfpf <> 'T' and wfpf<>'Z'
059440170130     c                   seton                                        9047
059450170130     c                   eval      wmsgtsp=msg(164)
059460170131     c                   eval      %subst(wmsgtsp: 19: 3)=wtsp03
059470170130     c                   eval      %subst(wmsgtsp: 34: 8)=%editc(o73lkgm:'4')
059480170130     c*volume mc bloccante
059490170131     c                   when      O73FMCM='1'  and not *in89
059500170131     c                                          and arbncr=0
059510170130     c                   seton                                        9048
059520170130     c                   eval      wmsgtsp=msg(165)
059530170131     c                   eval      %subst(wmsgtsp: 19: 3)=wtsp03
059540170130     c                   eval      %subst(wmsgtsp: 36: 6)=%editc(o73lmcm:'4')
059550170130     c* peso kg forzabile
059560170130     c                   when      O73Fkgf='1' and vidpkf<>wpes_pre
059570170130     c                             and not *in88 and d48ffr<>'E'
059580170131     c                             and wfpf <> 'T' and wfpf<>'Z'
059590170130     c                   seton                                        9047
059600170130     c                   eval      wmsgtsp=msg(164)
059610170131     c                   eval      %subst(wmsgtsp: 19: 3)=wtsp03
059620170130     c                   eval      %subst(wmsgtsp: 34: 8)=%editc(o73lkgf:'4')
059630170130     c                   eval      wmsgtsp=%trim(wmsgtsp)+ ' Avvisare ROM.'
059640170130     c                   eval      wmsgtsp=%trim(wmsgtsp)+ ' ENTER per forzare'
059650170130     c                   eval      wpes_pre=vidpkf
059660170130     c* volume mc forzabile
059670170130     c                   when      O73FmcF='1' and vidvlf<>wvol_pre
059680170130     c                             and not *in89 and d48ffr<>'E'
059690170131     c                             and arbncr=0
059700170130     c                   seton                                        9048
059710170130     c                   eval      wmsgtsp=msg(165)
059720170131     c                   eval      %subst(wmsgtsp: 19: 3)=wtsp03
059730170130     c                   eval      %subst(wmsgtsp: 36: 6)=%editc(o73lmcf:'4')
059740170130     c                   eval      wmsgtsp=%trim(wmsgtsp)+ ' Avvisare ROM.'
059750170130     c                   eval      wmsgtsp=%trim(wmsgtsp)+ ' ENTER per forzare'
059760170130     c                   eval      wvol_pre=vidvlf
059770170130     c                   endsl
059780170130     c                   endsr
059790911212     C*
059800911209     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE IX -----------------*
059810911209     C     REGIS2        BEGSR
059820130412     c                   clear                   wpda_ndc
059830130412     c                   clear                   wpda_ddc
059840130412     c                   clear                   wpda_dcm
059850160318     c* flag per togliere da distinta spedizioni import dpd "VEDI PACCO"
059860160318     c                   clear                   wtogli
059870950104     C*  GIRO UDATE
059880950104     C                   TIME                    W0140            14 0
059890950104     C* UDATE IN GGMMAAAA
059900950104     C                   MOVE      W0140         WDTGIO            8 0
059910950104     C*
059920950104     C* UDATE IN AAAAMMGG
059930950104     C                   Z-ADD     WDTGIO        G02DAT
059940950104     C                   MOVEL     *BLANK        G02ERR
059950950104     C                   CALL      'XSRDA8'
059960950104     C                   PARM                    WLBDAT
059970950104     C                   MOVEL     G02INV        DATEU             8 0
059980941213     C                   MOVEL     KNMUS         ARBPRU
059990941221     C* PRENDO I VALORI ASSOLUTI
060000941221     C                   MLLZO     1             VIDPKF
060010941221     C                   MLLZO     1             VIDVLF
060020041007     C* SALVO VOL/PKG/FTC/RSD/CAP/FIN VECCHIO IN FNARB
060030950114     C                   MOVEL     ARBVLF        OLDVLF
060040950114     C                   MOVEL     ARBFVF        OLDFVF
060050950114     C                   MOVEL     ARBPKF        OLDPKF
060060041007     C                   MOVEL     ARBvlc        OLDvlc
060070950114     C                   MOVEL     ARBRSD        OLDRSD
060080150206     C                   MOVEL     ARBLOD        OLDLOD
060090950114     C                   MOVEL     ARBCAD        OLDCAD
060100950114     C                   MOVEL     ARBFIN        OLDFIN
060110041007     C                   MOVEL     ARBFFD        OLDFFD
060120070320     c                   movel     VIDFFD        WFFD
060130151230     c* (Se variazione fatta da Arrivo su bolla ancora da partire non devo inviare)
060140151222     c                   if        not *in10 or wfile='A'
060150920120     C* INVIO PER SEDE
060160170116     C***                MOVEL     'FNARBD0T'    SFILE
060170920120     C* ALLOCO
060180170116     C****               EXSR      ALLOC
060190941027     C* ERRORI IN ALLOCAZIONE
060200170116    1C***  *IN91         IFEQ      *ON
060210170116     C***                GOTO      ENDRG2
060220170116    1C***                ENDIF
060230070515    c
060240070515     c* Se modificato FERMO DEPOSITO, devo allocare record L di ar4
060250070515     c*  per blocco/sblocco telefonata
060260070515     c                   clear                   wesar4L           1
060270070515    1c                   if        Wffd<>oldFFD
060280070515     c                   eval      TipoEla='C'
060290070515     c                   exsr      GestAR4L
060300070515     c
060310070515    2c                   if        *in91
060320170116     c*****              exsr      DLCSED
060330070525     C                   SETON                                        2891
060340070515     C                   GOTO      ENDRG2
060350070515    2c                   endif
060360070515    1c                   endif
060370070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
060380070515     c* iornare direttamente il file
060390070515     c                   clear                   waltrabolla       1
060400151222     C   10KARB          CHAIN     FNBLP01L                           3091
060410151222     C  n10KARB          CHAIN     FNARB01L                           3091
060420070515     c*
060430070515     c                   if        *in91
060440070515     C                   MOVEL     MSG(4)        VIDMSG
060450070515     c*
060460170116     c****               exsr      DLCSED
060470070515     c                   unlock    fiar401l
060480070515     c
060490070525     C                   SETON                                        2891
060500070515     C                   GOTO      ENDRG2
060510070515     c                   else
060520070515     c                   if        *in30
060530070515     c                   eval      waltrabolla='N'
060540070515     c                   else
060550070515     c* Salvo alcuni campi dell'altro file che potrebbero, per tempi
060560070515     c*  di aggiornamento, essere diversi.
060570070515     c                   eval      waltrofvf=arbfvf
060580070515     c                   eval      waltrovlf=arbvlf
060590070515     c                   eval      waltrocpi=arbcpi
060600130411     c* salvo campi di arb che mi servono per invio dati a pda
060610151222     c  n10              eval      wpda_ndc=arbndc
060620151222     c  n10              eval      wpda_ddc=arbddc
060630151222     c  n10              eval      wpda_dcm=arbdcm
060640151222     C   10KARB          CHAIN     FNARB01L                           30
060650151222     C  n10KARB          CHAIN     FNBLP01L                           30
060660070515     c* Alla fine per non sporcarmi i campi del record, richaino il
060670070515     c*  record originale che sto variando
060680151222     c   10              eval      wpda_ndc=arbndc
060690151222     c   10              eval      wpda_ddc=arbddc
060700151222     c   10              eval      wpda_dcm=arbdcm
060710070515     c                   endif
060720070515     c                   endif
060730151222     c                   endif
060740070207     C*
060750070207     C* SE SI TRATTA DI BOLLA FRANCO, SALVO IL CAMPO DELLA PARTITA IVA
060760151222     C*   10
060770151222     C*AN 05              MOVEL     ARBCPI        SAVCPI           16
060780151222     C*   10
060790151222     C*AN 05              MOVEL     *BLANKS       ARBCPI
060800070207     C*
060810151222     C*  N10
060820151222     C*AN 06              MOVEL     ARBCPI        SAVCPI           16
060830151222     C*  N10
060840151222     C*AN 06              MOVEL     *BLANKS       ARBCPI
060850151222     c                   if        (wfile='A' and *in05) or
060860151222     c                             (wfile='P' and *in06)
060870151222     C                   MOVEL     ARBCPI        SAVCPI           16
060880151222     C                   MOVEL     *BLANKS       ARBCPI
060890151222     c                   endif
060900070207     C*
060910061116     c* Se variazione di destinatario chaino anche fiar4 record Q se
060920061116     c*  mi serve
060930141215     c                   if        wnoindir=' '
060940061116     c                   if        *in01 and not *in05
060950061116     c                   if        (vidcdf=*blanks  and war4Q='D') or
060960061116     c                             (vidcdf<>*blanks and war4Q<>' ')
060970061116     C                   MOVEL     'Q'           WTRC
060980061116     C     KARB2         CHAIN     FiAR4000                           3091
060990061116     c*
061000061116     c                   if        *in91
061010061116     C                   MOVEL     MSG(4)        VIDMSG
061020070515     c
061030151223     c                   if        not *in10 or wfile='A'
061040170116     c******             exsr      DLCSED
061050151223     c   10              unlock    fnblp01l
061060151223     c  N10              unlock    fnarb01l
061070151223     c                   endif
061080070525     C                   SETON                                        2891
061090061116     C                   GOTO      ENDRG2
061100061116     c                   endif
061110061116     c                   endif
061120061116     c                   endif
061130141215     c                   endif
061140061116     c
061150061116     c* Verifico se prima c'era
061160911212     C*
061170911209     C                   Z-ADD     DATEU         ARBDTV
061180911209     C                   TIME                    ARBORV
061190911209     C                   MOVEL     VIDCVR        ARBCVB
061200911209     C*
061210060130     C* SE DA STORICIZZARE
061220060130     C**   *IN10         IFEQ      *ON
061230060130    1C**   D66FSA        ANDEQ     'S'
061240060130     C**   *IN10         OREQ      *OFF
061250060130    1C**   D66FSP        ANDEQ     'S'
061260060130    1C     D66FSA        ifeq      'S'
061270941212     C                   MOVEL     SAVRD2        ARBRD2
061280950114     C* SE VENGO DA VARIAZIONE MITTENTE, IL VOLUME LO STORICIZZO CON I
061290950114     C* CAMPI CHE HO SALVATO
061300950114     C     *IN24         IFEQ      *ON
061310950114     C                   MOVEL     MITFVF        ARBFVF
061320950114     C                   MOVEL     MITVLF        ARBVLF
061330950114     C                   MOVEL     MITFVF        ARBFVB
061340950114     C                   MOVEL     MITVLF        ARBVLB
061350950114    1C                   ENDIF
061360011108      *
061370011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
061380011108      * è in partenza 'P' o in arrivo 'A'
061390011108     C     *IN10         IFEQ      *ON
061400160119     c                   if        wfile='P'
061410160119     C                   MOVE      'a'           ARBPRU
061420160119     c                   else
061430011108     C                   MOVE      'A'           ARBPRU
061440160119     c                   endif
061450011108     C                   ENDIF
061460011108     C     *IN10         IFEQ      *OFF
061470011108     C                   MOVE      'P'           ARBPRU
061480011108     C                   ENDIF
061490011108      *
061500941014     C                   WRITE     FNARBD00
061510130927     c
061520130927     c* Memorizzo anche referente e telefono se si tratta di varazione "I0"
061530131009     c                   if        *in01 and
061540131009     c                                  (vidref<>par5ref or vidteld<>par5teld)
061550131204     c                   clear                   darbn_ref
061560130927     c                   eval      §arbnref = par5ref
061570130927     c                   eval      §arbnteld= par5teld
061580131204     c                   eval      arbuni70 = darbn_ref
061590130930     c                   eval      arbtrd='REF'
061600130927     c                   write     FNARBN00
061610130927     c                   endif
061620950114     C*
061630950114     C     *IN24         IFEQ      *ON
061640950114     C                   MOVEL     OLDFVF        ARBFVF
061650950114     C                   MOVEL     OLDVLF        ARBVLF
061660950114     C                   MOVEL     OLDFVF        ARBFVB
061670950114     C                   MOVEL     OLDVLF        ARBVLB
061680950114    1C                   ENDIF
061690950114    1C                   ENDIF
061700060301     c* storicizzazione motivazione
061710060301     c     d66mtv        ifeq      'S'
061720060301     c                   exsr      sto_mtv
061730060301     c                   endif
061740911209     C*
061750941025    1C     *IN01         IFEQ      *ON
061760040224
061770040224      * se in arrivo ho modificato uno dei dati del Fiar5 rcd GEN:
061780040224      * - Fermo deposito
061790040224      * devo memorizzare i dati della bolla prima della modifica
061800070320if  3c   10              If        wffd <> oldFfd
061810041022     c                   ExSr      RegAr5AR
061820040224    3c                   EndIf
061830941025     C*
061840941025     C* SE IMMESSA ANCHE LA 2 RAGIONE SOCIALE
061850141212     c                   if        wnoindir=' '
061860060223     c                   clear                   d19ftr
061870160115     c                   if        *in10 and wfile='P'
061880160115     c                   eval      d19ftr='T'
061890160115     c                   endif
061900941025     C                   MOVEL     ARBAAS        D19AAS
061910941025     C                   MOVEL     ARBLNP        D19LNP
061920941025     C                   MOVEL     ARBNRS        D19NRS
061930941025     C                   MOVEL     ARBNSP        D19NSP
061940941025     C                   MOVEL     'D'           D19TRC
061950970520     C                   MOVE      VIDRSD        D19NT1
061960941025     C                   CLEAR                   D19NT2
061970051114     c
061980941025     C                   CALL      'FNLV19R'
061990030612     C                   PARM                    Fnlv19DS
062000941025     C*
062010941025     C* ERRORE: RECORD NON VINCOLABILE
062020941212    2C     D19ERR        IFEQ      '2'
062030941025     C                   MOVEL     MSG(4)        VIDMSG
062040151223     c                   if        not *in10 or wfile='A'
062050170116     c****               exsr      dlcsed
062060070515     c   10              unlock    fnblp01l
062070070515     c  N10              unlock    fnarb01l
062080070515     c                   unlock    fiar401l
062090151230     c                   endif
062100151223     C                   SETON                                        2891
062110941025     C                   GOTO      ENDRG2
062120941212    2C                   ENDIF
062130141212     c                   endif
062140041006    c
062150950626     C*
062160950626     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
062170950626     C*     NELLA P.IVA
062180950626     C     VIDISD        IFEQ      '$$'
062190950626     C     VIDCPD        ANDEQ     *BLANKS
062200950626     C                   MOVEL     'PRIVATO'     VIDCPD
062210950626     C                   ENDIF
062220941025     C*
062230141212     c                   if        wnoindir=' '
062240941027     C                   MOVEL     VIDRSD        ARBRSD
062250970520     C                   MOVE      D19NT1        ARBRD2
062260911209     C                   MOVEL     VIDIND        ARBIND
062270911209     C                   MOVEL     VIDLOD        ARBLOD
062280911209     C                   MOVEL     VIDCAD        ARBCAD
062290060705     c* se spedizione export DPD diretta in Portogallo devo aggiungere al
062300060705     c* cap immesso tanti 0 a dx quanti ne servono per arrivare ad una
062310060705     c* lunghezza di cap di 7 byte
062320060705     c                   if        lnantw='DPD' and vidnzd = 'P  '
062330060705     c     ' '           checkr(e) arbcad:7      NN                1 0
062340060705     c                   if        %found
062350060705     c                   eval      %subst(arbcad:nn+1:7-nn)=*all'0'
062360060705     c                   endif
062370060705     c                   endif
062380060705     c*
062390911209     C                   MOVEL     VIDPRD        ARBPRD
062400941220     C                   MOVEL     VIDPID        ARBCPI
062410941220     C* LA P.IVA SE HA CODICE ISO VIENE MESSO A SIN ALTRIMENTI SUBITO
062420941220     C*  IL CODICE DELLA P.IVA
062430941220     C     VIDCPD        IFNE      *BLANKS
062440941220     C     VIDISD        ANDEQ     *BLANKS
062450941220     C                   CLEAR                   ARBCPI
062460941220     C                   MOVEL     VIDCPD        ARBCPI
062470941220     C                   ENDIF
062480041027     c                   eval      newcpi=arbcpi
062490041027     c
062500060222     C* SE ESISTE RECORD P.IVA IN FIAR4 LO AGGIORNO
062510950413     C* SOLO SE P.IVA SPROTETTA
062520950413     C     *IN05         IFEQ      *OFF
062530950413     C     *IN72         ANDEQ     *OFF
062540950413     C                   MOVEL     'P'           WTRC
062550060222     C     KARB2         CHAIN     FIAR401L                           30
062560060222     C     *IN30         IFEQ      *OFF
062570060223     c                   move      ar4not        wcpifile
062580060223     c                   if        wcpifile <> arbcpi
062590060223     c                   movel     'T'           ar4ftr
062600060223     c                   z-add     dateu         ar4dtr
062610060223     c                   z-add     dateu         ar4duv
062620060222     C                   MOVE      ARBCPI        AR4NOT
062630060222     C                   UPDATE    FIAR4000
062640060223     c                   else
062650060223     c                   unlock    fiar401l
062660060223     c                   endif
062670060222     C                   END
062680950413     C                   END
062690141215     c                   endif
062700941212     C*
062710931207    2C     VIDFFD        IFEQ      'SI'
062720950222     C                   MOVEL     'C'           ARBFIN
062730930528     C                   MOVEL     'S'           ARBFFD
062740931207   X2C                   ELSE
062750930528     C                   MOVEL     ' '           ARBFFD
062760141212     c                   if        wnoindir=' '
062770930528     C                   MOVEL     VIDFIN        ARBFIN
062780141212     c                   endif
062790931207    2C                   END
062800160318     c* Se spedizione Import dpd richiamo routine per aggiornamenti per
062810160318     c* spedizione "VEDI PACCO"
062820160318     c* (Aggiornamento del flag traduttore su fiar4 rek "I", pulizia del
062830160318     c* codice giro + memorizzaizone di togliere sped da distinta)
062840160318     c                   if        lnpntw='DPD'
062850160318     c                   exsr      sr_VediPDpd
062860160318     c                   endif
062870110905     c* se attiva procedura centro storico e c'e' il giro sulla bolla --> ricalcolo se
062880110905     c*  non è stato forzato a video
062890151230     c                   if        wnoindir=' '  and
062900151230     c                             (not *in10 or wfile='A')
062910110906     c                   if        vidffd=*blanks
062920110906     c                   if        (oldfin<>arbfin and arbfin=o95iso)
062930110906     c                             or oldffd<>vidffd
062940110906     c*****                        or oldfin=arbfin
062950110906     c
062960160318     c     karb          chain(n)  fiarg01l
062970160318     c                   if        %found(fiarg01l) and argcgi<>*blanks
062980160318     c****                         and olvstprop<>'N'
062990110905     c                   clear                   fnlvstds
063000110905     c                   eval      ilvstaas=arbaas
063010110905     c                   eval      ilvstlnp=arblnp
063020110905     c                   eval      ilvstnrs=arbnrs
063030110905     c                   eval      ilvstnsp=arbnsp
063040110905     c                   eval      ilvstmgs=arbmgs
063050110905     c                   eval      ilvstfin=arbfin
063060110905     c                   eval      ilvstcgi=argcgi
063070110906     c                   if        lnacons>0
063080110906     c                   eval      ilvstpoc=lnacons
063090110906     c                   else
063100110906     c                   eval      ilvstpoc=arblna
063110110906     c                   endif
063120110906     c                   if        arbddc>0
063130110906     c                   eval      ilvstdat=arbddc
063140110906     c                   else
063150110906     c                   eval      ilvstdat=dateu
063160110906     c                   endif
063170110905     c                   call      'FNLVSTR'
063180110905     c                   parm                    kpjba
063190110905     c                   parm                    fnlvstds
063200110906     c
063210110906     c* se aggiornato flag inoltro lo memorizzo nella variazione
063220110906     c                   if        OLVSTFINAG='S'
063230110906     c                   eval      arbfin=olvstfin
063240110905     c                   endif
063250110906     c
063260110906     c                   endif
063270110906     c                   endif
063280110906     c                   endif
063290141212     c                   endif
063300110905
063310970808     C* CODICE TASSAZIONE
063320141212     c                   if        wnoindir=' '
063330970808     C                   MOVEL     O95CTS        ARBCTS
063340141212     c                   endif
063350130606
063360031120
063370130930     c                   exsr      AggioREF
063380130930     c
063390931207    1C                   END
063400941025     C**
063410920319     C* RICALCOLO DEL VOLUME
063420941025     C**
063430941025    1C     *IN02         IFEQ      *ON
063440950114     C*
063450950114     C* SE VARIATO MITTENTE PER I PADRONCINI USO QUELLI SALVATI
063460941102     C* PULIZIA DELLA DS PER IMPOSTARE IL VOLUME DA FATTURARE VARIATO
063470030612     C                   CLEAR                   Fnlv20DS
063480941102     C*
063490941025     C* VOLUME REALE
063500041007    2C     VIDVLF        IFNE      oldVLF
063510941102     C                   MOVEL     'R'           D20FVL
063520941102     C                   Z-ADD     VIDVLF        D20VLU
063530930505     C*
063540931207   X2C                   ELSE
063550941025     C*
063560941025     C* VEDO SE DEVO MODIFICARE IL VOLUME DA FATTURARE
063570941214     C                   EXSR      VOLAUT
063580941102    2C                   ENDIF
063590941102     C**
063600941102     C* SE HO MODIFICATO IL VOLUME DA FATTURARE CALL A FNLV20R
063610941102     C*  PER DETERMINARE IL VOLUME BOLLETTATO
063620941102     C**
063630941102    2C     D20VLU        IFGT      0
063640941222     C                   MOVEL     D48TBO        D20TBO
063650941102     C                   MOVEL     'F'           D20TVL
063660041008     C                   MOVEL     arbFVB        D20FVB
063670041008     C                   MOVEL     arbVLB        D20VLB
063680041007     C                   MOVEL     oldFVF        D20FVF
063690041007     C                   MOVEL     oldVLF        D20VLF
063700030612     C                   MOVEL     Fnlv20DS      KPJBU
063710941102     C                   CALL      'FNLV20R'
063720941102     C                   PARM                    KPJBA
063730941102     C*
063740030612     C                   MOVEL     KPJBU         Fnlv20DS
063750941102     C*
063760941102     C                   MOVEL     D20FVB        ARBFVB
063770941102     C                   MOVEL     D20VLB        ARBVLB
063780941102     C                   MOVEL     D20FVF        ARBFVF
063790941102     C                   MOVEL     D20VLF        ARBVLF
063800941102    2C                   ENDIF
063810941214     C*
063820051129     C* SE DEVO SOSTITUIRE COL VOLUME VDL LO FACCIO
063830941214     C     WSOST         IFEQ      'S'
063840970418     C* WSOSC INDICA SE IL TIPO VOLUME DETERMINATO PER LA BOLLA E'
063850051129     C* SOSTITUIBILE DAL VOLUME VDL.
063860970418     C* IN CASO DI VARIAZIONE VOLUME WSOSC SARA' QUELLO DEL TIPO VOLUME
063870970418     C* "R"; IN CASO DI VARIAZIONE DI PESO WSOSC SARA' QUELLO DEL TIPO
063880970418     C* VOLUME DELLA BOLLA
063890041007     C     oldVLC        ANDGT     ARBVLF
063900970418     C     WSOSC         ANDEQ     'S'
063910041007     C                   Z-ADD     oldVLC        ARBVLF
063920970418     C                   MOVE      WFVF          ARBFVF
063930941214     C                   ENDIF
063940920319     C*
063950990628     C  N10              MOVEL     VIDPKF        ARBPKB
063960941102     C                   MOVEL     VIDPKF        ARBPKF
063970041007     c
063980041007    2C     *IN24         IFEQ      *ON
063990041007     C                   MOVEL     MITFVF        OLDFVF
064000041007     C                   MOVEL     MITVLF        OLDVLF
064010041007    2C                   ENDIF
064020941102    1C                   ENDIF
064030941212     C*
064040041007     C* Ora invio solo a sede
064050151223     c                   if        not *in10 or wfile='A'
064060070503     C     D66FFI        ifeq      'S'
064070950227     C*
064080950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
064090950413    2C     §3ABSD        IFEQ      ' '
064100170116     C***                OPEN      FNARBD2T
064110941014     C                   WRITE     FNARBDTT
064120170116     C***                CLOSE     FNARBD2T
064130950227    2C                   END
064140911212     C* DISALLOCO OGGETTO
064150170116     C****               EXSR      DLCSED
064160950208    1C                   END
064170911209     C*
064180950113     C* AGGIORNO FILE TASSAZIONE PADRONCINI
064190950113     C                   EXSR      AGGPDS
064200151223     c                   endif
064210931207     C*
064220941102     C* AGGIORNO
064230151223     C**   10
064240151223     C**AN 05              MOVEL     SAVCPI        ARBCPI
064250151223     C**  N10
064260151223     C**AN 06              MOVEL     SAVCPI        ARBCPI
064270151223     c                   if        (wfile='A' and *in05) or
064280151223     c                             (wfile='P' and *in06)
064290151223     C                   MOVEL     SAVCPI        ARBCPI
064300151223     c                   endif
064310941212     C*
064320151223     c                   if        wfile='A'
064330151223     C                   UPDATE    FNARB000
064340151223     c                   else
064350151223     C                   UPDATE    FNBLP000
064360151223     c                   endif
064370150206     c
064380160318     c* Tolgo  spedizione "VEDI PACCO DPD" da distinta
064390160318     c                   if        wtogli='1'
064400160318     c                   exsr      sr_TogliVDpd
064410160318     c                   endif
064420150206     c* memorizzo la località normalizzata
064430150206     c                   if        vidlod<>normlod OR WAR4X='E'
064440150206     C
064450150211     c                   eval      d19ftr='T'
064460150206     c                   eval      d19agg='E'
064470150203     C                   MOVEL     ARBAAS        D19AAS
064480150203     C                   MOVEL     ARBLNP        D19LNP
064490150203     C                   MOVEL     ARBNRS        D19NRS
064500150203     C                   MOVEL     ARBNSP        D19NSP
064510150203     C                   MOVEL     'X'           D19TRC
064520150206     C                   IF        VIDLOD<>NORMLOD
064530150203     C                   MOVE      normlod       D19NT1
064540150206     C                   ELSE
064550150206     C                   CLEAR                   D19NT1
064560150206     C                   ENDIF
064570150206     C                   CLEAR                   D19NT2
064580150203     c
064590150203     C                   CALL      'FNLV19R'
064600150203     C                   PARM                    Fnlv19DS
064610150206     c                   endif
064620150203     c
064630151230     c* Salto le seguenti istruzioni se
064640151223     c* dall'arrivo sono in manutenzione di una bolla in partenza ancora da
064650151223     c* partire
064660151223     c                   if        not *in10 or wfile='A'
064670070320     c
064680070320     c* Se aggiunto   Fermo Deposito, senza consegna richiesta,
064690070320     c*  imposto il blocco. conconsegna richiesta lo tolgo
064700070320     c* Se tolgo      Fermo Deposito, tolgo il relativo blocco
064710070320     c
064720070320     c* Devo richainare e aggiornare (perchè nel frattempo ho letto
064730070320     c*  altri record fi FIAR4
064740151223
064750070320    1c                   if        Wffd<> oldffd
064760070320     c                   eval      tipoEla='C'
064770070320     c                   exsr      GestAR4L
064780070320     c
064790070320     c* Se allocato pazienza (non aggiorno ma vado avanti
064800070320     c*  fermarmi in questo punto non è bello,lascerebbe la
064810070320     c*  transazione a metà)
064820070320    2c                   if        not *in91
064830070320     c
064840170113     c* Se aggiunto il fermo deposito tolgo eventuale contatto per l'appuntamento
064850170113     c                   if        wffd='S' and
064860170113     c                             (arbtc1='A' or arbtc2='A')
064870170113     c                   eval      §b4lbas=' '
064880170113     c                   endif
064890170113
064900070508    3c****               if        arbdcr=0 and wffd='S'
064910070508     c* imposto il flag telefonata in base a quanto indicato a video
064920170113     c                   if        vidtfd='SI'  or wffd=' '
064930070508     c                   clear                   §b4ltfd
064940070508     c                   else
064950070320     c                   eval      §b4ltfd='S'
064960070508     c                   endif
064970070508     c
064980070508     c****               else
064990070508     c****               clear                   §b4ltfd
065000070508    3c****               endif
065010070320     c                   eval      tipoEla='A'
065020070320     c                   exsr      GestAR4L
065030140212     c* richiamo pgm per eventuale scrittura/cancellazione file spia
065040150603     c* per aggiungere il file spia per la sped, la bolla non deve essere in stati
065050150603     c*  particolari
065060150603     c                   clear                   tnsd99ds
065070150603     c                   clear                   statofd           1
065080150603     c                   if        wffd='S'
065090150603     C                   MOVEL     'L'           D98tla
065100150603     C                   MOVEL     'A'           D98tbo
065110150603     C                   Z-ADD     arbaas        D98aas
065120150603     C                   Z-ADD     arblnp        D98lnp
065130150603     C                   Z-ADD     arbnrs        D98nrs
065140150603     C                   Z-ADD     arbnsp        D98nsp
065150150603     C*
065160150603     C                   call      'TNSD99R'
065170150603     C                   parm                    tnsd99ds
065180150603     c                   if        D98SPCDEE='FFD' or %subst(D98SPCDEE:1:2)='FD'
065190150603     c                   eval      statofd='S'
065200150603     c                   endif
065210150603     c                   endif
065220150603     c
065230150603     c*****              if        wffd=*blanks or vidtfd=*blanks
065240150603     c                   if        wffd=*blanks or (vidtfd=*blanks and
065250150603     c                             statofd='S')
065260140212     c                   clear                   trtcp2ds
065270140213     c                   if        wffd=*blanks
065280140212     c                   eval      t2chiamata='A'
065290140212     c                   endif
065300140212     c                   eval      t2chi= 'F'
065310140212     c                   eval      T2AAS=d48aas
065320140212     c                   eval      T2LNP=d48lnp
065330140212     c                   eval      T2NRS=d48nrs
065340140212     c                   eval      T2NSP=d48nsp
065350140213     c* passo in t2fgs la lna (serve per far funzionare il chiodo nel trtcp2r)
065360140213     c                   eval      T2fgs=arblna
065370140212     c                   call      'TRTCP2R'
065380140212     c                   parm                    trtcp2ds
065390140213     c                   endif
065400070320     c
065410070320    2C                   END
065420070320    1C                   END
065430070320     c
065440041007     c* Per l'altro file sistemo eventuale volume da fatturare e P.I
065450041007     c*  e aggiorno
065460041007     c                   if        waltrabolla=' '
065470041007     c   02              if        waltrofvf='T'  or
065480041007     c                             (waltrofvf='Z' and arbfvf<>'T' aND
065490041007     C                              WALTROVLF> arbvlf)
065500041008     c                   movel     waltrofvf     arbfvf
065510041008     c                   movel     waltrovlf     arbvlf
065520041007     c                   endif
065530041007     c* Se 1°bolla franco, tengo la p.i. presente nel record
065540151223     c                   if        (*in10 and *in06) or
065550151223     c                             (not *in10 and *in05)
065560041007     c                   eval      arbcpi=waltrocpi
065570041027     c                   else
065580041027     c                   eval      arbcpi=NEWCPI
065590041007     c                   endif
065600041007     c
065610041007     c* aggiornamenti di campi mirati
065620041007     C   10              except    arbdblp
065630041007     C  N10              except    arbdarb
065640041007     c                   endif
065650151223
065660151223     c                   endif
065670950221     C*
065680950221     C* DISALLOCO  I FILES CHE NON USO
065690051118     C   10              UNLOCK    FiAR901L
065700991001     C                   UNLOCK    FIAR601L
065710061116     c
065720061116     c* Se si tratta di bolla in assegnato con possibilità di scrivere il
065730061116     c*  cod fiscale, richiamo la REGIS11 per la memorizzazione
065740151230     c                   if        wnoindir=' ' and
065750151230     c                             (not *in10 or wfile='A')
065760061116     c                   if        *in01 and not *in05
065770061116     c                   if        (vidcdf=*blanks  and war4Q='D') or
065780061116     c                             vidcdf<>*blanks
065790061116     c                   eval      v11mide='D'
065800061116     c                   eval      v11cdfis=vidcdf
065810071102     c* Imposto anche il campo della partita iva di nuovo
065820071102     c                   eval      v11pid=vidpid
065830071102     c
065840061116     c                   movel     'FI'          vidcvr
065850071026     c* Non devo aggiornare la bolla perchè l'ho già fatto qui per la
065860071026     c*  partita IVA
065870130412     c                   if        v11pid<>*blanks or
065880130412     c                              %subst(v11cdfis:12:1)<> ' '
065890071026     c                   eval      Aggiobol='N'
065900130402     c                   endif
065910061116     c                   exsr      REGIS11
065920061116     c                   endif
065930061116     c                   endif
065940141215     c                   endif
065950041007     c*
065960141212     c                   if        wnoindir='S'
065970141212     c                   eval      wdisag=wdisagdest
065980141212     c                   endif
065990041007     c* Riaggancio la bolla in modifica, per controlli sul
066000041007     c*  destinatario disagiato e consegna richiesta rispetto a FFD
066010151223     c                   if        wfile='A'
066020151223     C     KARB          CHAIN(n)  FNARB01L                           30
066030151223     c                   else
066040151223     C     KARB          CHAIN(n)  FNBLP01L                           30
066050151223     c                   endif
066060070906     c* Se in destinatario non è più A D o S tolgo se non immesso
066070070906     c*  dalla partenza
066080070911     c                   if        wdisagdest<>wdisag and wdisagdest<>' ' and
066090070910    2c                             (arbtc1=wdisagdest or arbtc2=wdisagdest)
066100070906     c                   exsr      ContrTCP
066110070906     c                   endif
066120040224
066130950602     C*
066140020531     C* SE destinatario disagiato devo rifare il giro per le consegne
066150020531     c* particolari
066160041015     C***  wdisag        IFne      *blanks
066170041015     c***  arbtc1        andne     wdisag
066180041015     c***  arbtc2        andne     wdisag
066190060327     c***                If        wdisag <> *blanks and
066200060327     c***                          %subst(arbGva:1:1) <> 'U' or
066210060327     c***                          wdisag='X' and %subst(arbGva:1:1)='U'
066220060327     c* Per Disagiato ---->sempre
066230060327     c* per Supermercato ->solo se non è consegna ad uffici
066240060327     c* per Appuntamento ->solo se non è consegna ad uffici e non è
066250070917     c*                    prevista data consegna "Tassativa" da partenza
066260070917     c                   movel     arbdcr        w008a
066270070907    1c                   if        (wdisag = 'X') OR
066280060327     c                             (wdisag='S' and %subst(arbGva:1:1)<>'U') OR
066290060327     c                             (Wdisag='A' and %subst(arbGva:1:1)<>'U' and
066300070917     c                             (arbdcr=*zeros or arbtcr <> *blanks or
066310070917     c                              (par5mdp='S' and w008a<>par5gdcr)))
066320070907    2c     arbtc1        ifne      wdisag
066330041015     c     arbtc2        andne     wdisag
066340030122     c                   Eval      wI0 = 'S'
066350020531     C                   MOVEL     'CP'          D48CVB
066360070907     C                   MOVEL     'D'           D48TRC
066370070911     C***                MOVEL     'E'           WESEG
066380070911     c***                clear                   W48f12
066390070907     c***                if        *in20
066400050502     c                   eval      §bgtcr=arbtcr
066410050502     c                   eval      §bghcr=arbhcr
066420050502     c                   eval      §bgdcr=arbdcr
066430050502     c                   eval      §bggc1=arbgc1
066440050502     c                   eval      §bggc2=arbgc2
066450050502     c                   eval      §bgtc1=arbtc1
066460050502     c                   eval      §bgtc2=arbtc2
066470070907     c***                endif
066480070910     c
066490070910     c* Se da eliminare anche la consegna particolare imposto
066500070910     c*  insieme la variazione
066510070910     c                   if        eliminatcp='1'
066520070910     c                   clear                   §bgtc1
066530070911     c* ripristino la vecchia consegna partic.
066540070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
066550070911     c                   eval      §bgtc1=aggtcp
066560070911     c                   endif
066570070910     c                   endif
066580070910     c                   if        eliminatcp='2'
066590070910     c                   clear                   §bgtc2
066600070911     c* ripristino la vecchia consegna partic.
066610070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
066620070911     c                   if        §bgtc1=' '
066630070911     c                   eval      §bgtc1=aggtcp
066640070911     c                   else
066650070911     c                   eval      §bgtc1=aggtcp
066660070911     c                   endif
066670070911     c                   endif
066680070910     c                   endif
066690070911     c
066700070907     c* Se modificato destinatario in disagiato o supermercato imposto il
066710070907     c* relativo flag nelle cosegne particolari
066720070907    3c     wdisag        ifne      *blanks
066730070907     c                   select
066740070907     c     §bgtc1        wheneq    *blanks
066750070907     c                   movel     wdisag        §bgtc1
066760070907     c     §bgtc2        wheneq    *blanks
066770070907     c                   movel     wdisag        §bgtc2
066780070907     c                   endsl
066790070907    4c     §bgtc1        ifeq      §bgtc2
066800070907     c                   clear                   §bgtc2
066810070907    4c                   endif
066820070907    3c                   endif
066830070907     c* Memorizzo in skiera
066840070907    3c                   if        kk<=10
066850070910     c* se devo eliminare una consegna particolare, memorizzo nella
066860070910     c* stessa variazione
066870070910     c                   if        eliminatcp=' '
066880070907     c                   add       1             kk
066890070910     c                   endif
066900070911     c* Passo dati in DS
066910070907     c                   movel     dsarbg        kdati(KK)
066920070907     c                   movel     d48cvb        kcvb(KK)
066930070907     c                   movel     d48trc        ktrc(KK)
066940070911     c* videata si vede o meno in base al richiamo prevedente
066950070911     c                   movel     d48ffr        kffr(KK)
066960070911     c* Disabilito F12
066970070911     c                   movel     ' '           kF12(KK)
066980070907    3c                   endif
066990070907     c
067000070907    2c                   endif
067010070910     c
067020070910   x1c                   else
067030070910     c* Se non devo aggiungere delle consegne particolari,
067040070910      *  se c'è la data consegna richiesta controllo se è ancora valida
067050070910      *  solo se ho fatto una modifica in arrivo
067060070914     c* Solo se non devo aggiungere delle consegnee particolari
067070070914     c*  altrimenti ilk controllo lo faccio nella regis7
067080070914     c                   if        aggtcp=*blanks
067090071109     c
067100071109     c* Imposto il campo WGIAC in base alla giacenza
067110071109if  2c                   If        *In04 and wgcfas = 35
067120071109if  3c                   If        wgcded > *Zeros and arbdcr >= wgcded
067130071109     c                   Eval      wgiac = 'S'
067140071109    3c                   EndIf
067150071109if  3c                   If        wgcded = *Zeros and arbDcr >= wgcddm
067160071109     c                   Eval      wgiac = 'S'
067170071109    3c                   EndIf
067180071109    3c                   EndIf
067190071109     c
067200160114    2c                   If        ArbDcr > *Zeros and *In10  and wfile='A'
067210070910     c                             and wCa = *Blanks and wgiac = *Blanks
067220070910     c                   ExSr      ContrDcr
067230070910    2c                   EndIf
067240070914    2c                   EndIf
067250070907    1c                   endif
067260070907     c
067270070907     c                   clear                   eliminatcp
067280070911     c                   clear                   aggtcp
067290130411
067300130411     c* Richiamo routine per invio variazione a pda
067310160330     c                   if        wfile='A' and wtogli=*blanks
067320130411     c                   exsr      sr_pda
067330160112     c                   endif
067340911209     C*
067350911212     C     ENDRG2        ENDSR
067360130930     c
067370130930     c* Aggiorno referente -----------------------------------------------------*
067380130930     c     AggioREF      BEGSR
067390130930     c
067400130930      * Referente e Telefono e magazzino giacenza
067410130930     c                   Eval      kAr5Trd = 'GEN'
067420130930     c     kAr5          Chain     Fiar501l
067430130930      * Aggiorno Fiar5
067440130930if  2c                   If        %Found(Fiar501l)
067450130930     c                   movel     ar5uni        DAr5Gen
067460130930     c                   Eval      §Ar5Ref = VidRef
067470130930     c                   Eval      §Ar5Teld = VidTeld
067480130930     c
067490130930     c* Solo per causale I0 aggiorno anche mag giacenza e f.deposito
067500130930     c                   if        flgcvr = 'I'
067510130930     c                   Eval      §Ar5Zng = VidZng
067520130930     c
067530130930     c                   if        wffd<>oldffd
067540130930     c                   if        wffd='S'
067550130930     c                   eval      §ar5dfd=%editc(dateu:'X')
067560130930     c                   else
067570130930     c                   clear                   §ar5dfd
067580130930     c                   endif
067590130930     c                   endif
067600130930     c
067610130930     c                   endif
067620130930     c
067630130930     c                   Movel(p)  DAr5Gen       Ar5Uni
067640130930      * se devo trasmettere alla sede sfleggo per la trasmissione
067650130930     c                   If        §Ar5Trse = 'S'
067660130930     c                   Clear                   Ar5Ft2
067670130930     c                   EndIf
067680130930      * sfleggo sempre per la trasmissione al cliente
067690130930     c                   Clear                   Ar5Ft3
067700130930     c                   Update    Fiar5000
067710130930      * Scrivo Fiar5
067720130930   x2c                   Else
067730130930      * se immessi i dati
067740130930      * non mi preoccupo per il fermo deposito: il fiar5 record GEN viene sempre creato
067750130930      * in immissione bolle. Non esiste più il caso di fiar5 record GEN inesistente in fase
067760130930      * di manutenzione bolle
067770130930if  3c                   If        VidRef <> *Blanks or VidTeld <> *Blanks or
067780130930     c                             VidZng <> *Blanks
067790130930     c                   Clear                   Fiar5000
067800130930     c                   Eval      Ar5Aas = ArbAas
067810130930     c                   Eval      Ar5Lnp = ArbLnp
067820130930     c                   Eval      Ar5Nrs = ArbNrs
067830130930     c                   Eval      Ar5Nsp = ArbNsp
067840130930     c                   Eval      Ar5Trd = 'GEN'
067850130930     c                   Move      Dateu         Ar5Dac
067860130930     c                   Movel     W0140         Ar5Orc
067870130930     c                   Clear                   Dar5Gen
067880130930     c                   Eval      §Ar5Ref = VidRef
067890130930     c                   Eval      §Ar5Teld = VidTeld
067900130930     c                   Eval      §Ar5Zng = VidZng
067910130930     c                   Movel(p)  DAr5Gen       Ar5Uni
067920130930     c                   Write     Fiar5000
067930130930    3c                   EndIf
067940130930    2c                   EndIf
067950130930     c                   ENDSR
067960911210     C*
067970911210     C*--- CONTROLLI FORMATO3 ----------------------------------------*
067980911210     C     CONTR3        BEGSR
067990941209     C*
068000920319     C* CONTROLLI COMUNI CON CONTR4
068010920319     C                   EXSR      CTRCOM
068020941209     C   90              GOTO      ENDC03
068030920319     C*
068040920319     C* T A S S A Z I O N E
068050941222     C* CONTROLLO L'IMPORTO DA ASSICURARE SE SONO IN ARRIVO SOLO PER
068060941222     C*  ASSEGNATO, IN PARTENZA SEMPRE
068070941222    1C     *IN36         IFEQ      *OFF
068080941222     C     *IN10         OREQ      *OFF
068090911211     C*
068100011205     C*** I M P O R T O  D A  A S S I C U R A R E  ***
068110080701      **** OBSOLETO DAL 1 GIUGNO 2008 CHE È STATO INSTALLATO LIQUIDAZIONE TANSATTIVA AUTOMATICA ***
068120990127     C*
068130990127     C* IMPOSSIBILE INSERIRE IMPORTO DA ASSICURARE SE BOLLA SI C.A.
068140990127     C*  IN LIQ.TRANSATTIVA
068150080701    2C***  VIDIAS        IFGT      0
068160990127     C* VEDO SE C'E' C.A.
068170040809     C**""*KARB          CHAIN     FNDCT02L                           30
068180040809    3C**""**IN30         DOWEQ     *OFF
068190040809    4C**""*DCTATB        IFEQ      ' '
068200990127     C*
068210040809    5C**""*DCTFPR        IFEQ      'T'
068220040809     C**""*              MOVEL     MSG(82)       VIDMSG
068230040809     C**""*              SETON                                        30
068240040809     C**""*              SETON                                        409028
068250040809    5C**""*              ENDIF
068260040809    4C**""*              ENDIF
068270990127     C**
068280040809     C**""*  N30KARB          READE     FNDCT02L                               30
068290040809    3C**""*              ENDDO
068300040809      *
068310040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
068320040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
068330040809      * legate alla spedizione che passo
068340080701      **** OBSOLETO DAL 1 GIUGNO 2008 CHE È STATO INSTALLATO LIQUIDAZIONE TANSATTIVA AUTOMATICA ***
068350080701      ***    NON FACCIAMO PIUI' IL CONTROLLO ****
068360080701     c*                  clear                   fidn12ds
068370080701     c*                  eval      i12tla = 'L'
068380080701     c*                  eval      i12aas = D48AAS
068390080701     c*                  eval      i12lnp = D48LNP
068400080701     c*                  eval      i12nrs = D48NRS
068410080701     c*                  eval      i12nsp = D48NSP
068420080701     c*                  eval      i12fel = 'E'
068430080701     c*                  eval      i12fan = 'E'
068440040809     c*
068450080701     c*                  call      'FIDN12R'
068460080701     c*                  parm                    fidn12ds
068470040809     c*
068480080701     c*                  setoff                                       30
068490040809     c*
068500040809     c* se non ci sono errori
068510080701    3C*                  if        o12err <> ' '
068520080701     c*                  seton                                        30
068530080701     c*                  else
068540040809     c* se numero di CA trovate maggiore di zero
068550080701    4C*                  if        o12nca = 0
068560080701     c*                  seton                                        30
068570080701     c*                  else
068580080701     c*                  z-add     1             jj                2 0
068590080701    5c*                  dow       jj <= o12nca and *in30 = *off
068600080701     c*                  movea     skkey(jj)     dskey
068610040809     c*
068620040809     c* Aggancio il FNDCT01L x reperire se la CA è in "Liquidazione Transattiva"
068630080701     c*    KDCT1_C       chain     FNDCT01L
068640080701    6c*                  if        %found(FNDCT01L)
068650080701    7C*    DCTFPR        IFEQ      'T'
068660080701     C*                  MOVEL     MSG(82)       VIDMSG
068670080701     C*                  SETON                                        30
068680080701     C*                  SETON                                        409028
068690080701    7C*                  ENDIF
068700080701    6c*                  endif
068710040809     c*
068720080701     c*                  add       1             jj
068730080701    5c*                  enddo
068740040809     c*
068750040809     c                   seton                                        30
068760040809     c*
068770080701    4c*                  endif
068780080701    3c*                  endif
068790990127     C**
068800080701     C** 90              GOTO      ENDC03
068810080701    2C***                ENDIF
068820941215     C*
068830941215     C* SE IL CAMPO = '   ' PRENDO LA DIVISA DALLA NAZIONE DEL MITTENT
068840941215     C* LO EVIDENZIO A VIDEO
068850950120     C* SE VARIATO L'IMPORTO ME LO SALVO
068860950120     C     *IN97         IFEQ      *ON
068870950120     C                   MOVEL     '1'           WIN97             1
068880950120     C                   ENDIF
068890950120     C*
068900941215     C*
068910941215     C                   MOVEL     VIDVAS        WVLT
068920941215     C                   Z-ADD     VIDIAS        W0133            13 3
068930941215     C* CONTROLLO LA DIVISA
068940941215     C                   EXSR      CTRVLT
068950941215     C                   MOVEL     WVLT          VIDVAS
068960941215     C* ERRORE
068970941215     C   90              SETON                                        41
068980941215     C   90              GOTO      ENDC03
068990011009      *
069000011009      * Controllo se la divisa immessa è scaduta o no
069010011009     C     ARBDSP        IFGT      §CVDLI
069020011105     C     VIDIAS        ANDGT     0
069030011102     C                   MOVEL     MSG(102)      VIDMSG
069040011009     C                   SETON                                        419028
069050011009     C                   GOTO      ENDC03
069060011009     C                   ENDIF
069070941215     C*
069080941215     C* SE VARIATA LA VALUTA FACCIO RIDIGITARE L'IMPORTO
069090941222    2C     VIDVAS        IFNE      VARVAS
069100950120     C     WIN97         ANDEQ     *BLANKS
069110941222    3C     VIDVAS        IFNE      *BLANKS
069120941219     C     VIDIAS        ORNE      0
069130941215     C                   MOVEL     MSG(28)       VIDMSG
069140941215     C                   SETON                                        409028
069150941215     C                   GOTO      ENDC03
069160941222    3C                   ENDIF
069170941219    2C                   ENDIF
069180941215     C*
069190941215     C                   MOVEL     VIDVAS        VARVAS
069200950120     C                   CLEAR                   WIN97
069210941215     C*
069220011205
069230011205      * pulisco il flag x la forzatura importo da assicurare forzabile
069240011205     c                   if        vidias <> sav1_vidias
069250011205     c                   clear                   forzaias
069260080625     c                   clear                   forzaias2
069270011205     c                   endif
069280011205      *---------------------
069290011205  2b c                   if        vidias <> 0
069300011205     c                   eval      sav1_vidias = vidias
069310011205      * Controllo l'importo da assicurare tramite il trul22r
069320011205     c                   clear                   trul22ds
069330011205     c                   eval      i22tla = 'L'
069340011205     c                   eval      i22cbo = arbcbo
069350011205     c                   eval      i22tsp = arbtsp
069360011205     c                   eval      i22lnp = arblnp
069370011205     c                   eval      i22nzm = arbnzm
069380011205     c                   eval      i22lna = arblna
069390011205     c                   eval      i22nzd = arbnzd
069400011205     c                   movel     vidklp        i22ksc
069410011205     c                   move      vidksc        i22ksc
069420011205     c                   eval      i22ctr = vidctr
069430011205     c                   eval      i22imp = vidias
069440011205     c                   eval      i22div = vidvas
069450011205     c                   eval      i22ute = knmus
069460011205     c                   eval      i22pgm = nompgm
069470011205     c                   call      'TRUL22R'
069480011205     c                   parm                    trul22ds
069490011205      * se ho tutti i flag impostati non posso immettere l'importo
069500011205      * da assicurare
069510011205  3b c                   if        o22fmn <> *blanks and o22fx1 <> *blanks
069520011205     c                             and o22fx2 <> *blanks
069530011205     c                   eval      vidmsg = msg(79)
069540011205     c                   eval      *in40 = *on
069550011205     c                   eval      *in90 = *on
069560011205     c                   eval      *in28 = *on
069570011206     c                   goto      endc03
069580011205  3e c                   endif
069590011205      * controllo che non sia minore del limite previsto
069600011205  3b c                   if        o22fmn <> *blanks
069610011205     c                   eval      vidmsg = msg(87)
069620011205     c                   eval      *in40 = *on
069630011205     c                   eval      *in90 = *on
069640011205     c                   eval      *in28 = *on
069650011206     c                   goto      endc03
069660011205  3e c                   endif
069670011205      * controllo che non sia maggiore del limite previsto
069680011205      * non forzabile
069690011205  3b c                   if        o22fx2 <> *blanks
069700011205     c                   eval      vidmsg = msg(39)
069710011210     c                   eval      %subst (vidmsg:38:14) = (%editc(o22lx2: '4'))
069720011205     c                   eval      *in40 = *on
069730011205     c                   eval      *in90 = *on
069740011205     c                   eval      *in28 = *on
069750011206     c                   goto      endc03
069760011205  3e c                   endif
069770011205      * controllo che non sia maggiore del limite previsto
069780011205      * forzabile
069790011205  3b c                   if        o22fx1 <> *blanks and forzaias = *blanks
069800011205     c                   eval      forzaias = '1'
069810011205     c                   eval      vidmsg = msg(80)
069820011210     c                   eval      %subst (vidmsg:28:14) = (%editc(o22lx1: '4'))
069830011205     c                   eval      *in40 = *on
069840011205     c                   eval      *in90 = *on
069850011205     c                   eval      *in28 = *on
069860011206     c                   goto      endc03
069870011205  3e c                   endif
069880011205  2e c                   endif
069890011205
069900941209     C*
069910941209     C* SE HA TARIFFE, CONTROLLO
069920941209    2C     WTKS          IFEQ      'S'
069930941209     C     KTAM4         CHAIN     TNTAM000                           30
069940941209    3C     *IN30         IFEQ      *OFF
069950991028     C                   MOVEL     TAMFLO        DSTA01
069960991028     C                   MOVEL     'CV'          COD
069970991028     C                   MOVEL(P)  §TADIV        KEY
069980991028     C                   EXSR      CTABEL
069990991028     C                   MOVEL     TBLUNI        DSCV
070000011010     C                   MOVEL     §CVDEC        COMDEC
070010941209     C*
070020941215     C* PULISCO CAMPI D FORZATURA
070030011010    4C     W0133         IFNE      VARIAS
070040941209     C                   CLEAR                   DIE
070050941215     C                   CLEAR                   CIN
070060941215    4C                   END
070070020715
070080020715      * Non controlla da tariffa se obbligatorio nel caso di tipo bolla non previsto
070090020715     c     §3atb1        Lookup    tbl                                    30
070100020715
070110020715   4Ac                   If        *in30 = *On
070120941209     C*
070130941209     C* SOLO SE E' ASSICURAZIONE ANCHE PER GLI ARRIVI ( <> P )
070140941209     C*   ED E' ESPRESSO IL VALORE MERCE NELLA BOLLA
070150990818     C     KTPT          CHAIN     TITPT000                           30
070160950810    4C     *IN30         IFEQ      *ON
070170950810     C     TPTATB        ORNE      ' '
070180941222     C*
070190950810     C     *IN10         OREQ      *ON
070200941209     C     TPTAPL        ANDNE     'P'
070210941222     C*
070220941222     C     *IN10         OREQ      *OFF
070230941222     C     TPTAPL        ANDNE     'A'
070240941215     C*
070250011010     C                   Z-ADD     W0133         VARIAS
070260941215     C*
070270941215     C* TAMFIS=1 OBBLIGATORIO     FORZABILE --> CON ENTER PRENDO MAX
070280941215     C*                                         IMPORTO DA TARIFFA
070290941215    5C     TAMFIS        IFEQ      '1'
070300941215     C     CIN           ANDNE     1
070310011010     C     W0133         ANDEQ     0
070320941215     C                   MOVEL     MSG(40)       VIDMSG
070330941215     C                   SETON                                        409028
070340941215     C                   ADD       1             CIN
070350941215     C                   GOTO      ENDC03
070360941215    5C                   ENDIF
070370941215     C*
070380941215     C* TAMFIS=3 OBBLIGATORIO NON FORZABILE --> ERRORE SEMPRE
070390941215    5C     TAMFIS        IFEQ      '3'
070400011010     C     W0133         ANDEQ     0
070410941215     C                   MOVEL     MSG(42)       VIDMSG
070420941215     C                   SETON                                        409028
070430941215     C                   GOTO      ENDC03
070440941215    5C                   ENDIF
070450941215     C*
070460941215     C* TAMFIS=2 NON INSERIBILE             --> ERRORE SEMPRE
070470941215    5C     TAMFIS        IFEQ      '2'
070480011010     C     W0133         ANDNE     0
070490941215     C                   MOVEL     MSG(43)       VIDMSG
070500941215     C                   SETON                                        409028
070510941215     C                   GOTO      ENDC03
070520941215    5C                   ENDIF
070530941215     C*
070540941215     C* SE <> 0 CONTROLLO CHE NON SUPERI IL VALORE IN TARIFFA
070550950810    5C  N30TPTATB        IFEQ      ' '
070560011010     C     W0133         ANDGT     0
070570941215     C     DIE           ANDNE     1
070580941215     C     TPTVLM        ANDGT     0
070590941215     C*
070600941209     C* VALORE A COLLO
070610941215    6C     TPTFVM        IFEQ      '1'
070620991028     C     ARBNCL        MULT(H)   TPTVLM        TPTVAL           13 3
070630941215   X6C                   ELSE
070640941215    7C     TPTFVM        IFEQ      '2'
070650991028     C                   Z-ADD     TPTVLM        TPTVAL
070660941215   X7C                   ELSE
070670941212     C     ARBPKF        ADD       0,9           W0060             6 0
070680991028     C     W0060         MULT(H)   TPTVLM        TPTVAL
070690941215    7C                   END
070700941215    6C                   END
070710991028     C*
070720011205     C                   z-add     vidias        vidval           13 3
070730991028    7C     §TADIV        IFNE      VIDVAS
070740030612     C                   CLEAR                   YeurcoDS
070750991028     C                   MOVEL     VIDVAS        YECDVI
070760991028     C                   MOVEL     §TADIV        YECDVO
070770991028     C                   Z-ADD     VIDIAS        YECIMI
070780011010     C                   MOVE      COMDEC        YECDCO
070790991028     C*
070800991028     C                   CALL      'YEURCO'
070810030612     C                   PARM                    YeurcoDS
070820991028     C*
070830991028    8C     YECESI        IFEQ      ' '
070840991028     C                   Z-ADD     YECIMO        VIDVAL
070850991028    8C                   ENDIF
070860991028    7C                   ENDIF
070870941209     C*
070880991028    6C     VIDVAL        IFGT      TPTVAL
070890941209     C                   ADD       1             DIE
070900011205     c                   eval      vidmsg = msg(44)
070910941209     C                   SETON                                        409028
070920941209     C                   GOTO      ENDC03
070930941215    6C                   ENDIF
070940941209     C*
070950941215    5C                   ENDIF
070960941222    4C                   ENDIF
070970020715   4ac                   EndIf
070980941209    3C                   ENDIF
070990941209    2C                   ENDIF
071000030710
071010030710      * calcolo data limite mod. importo da assicurare
071020060201     c**   *iso          Move      ArbDsp        wdataiso
071030060201     c**                 adddur    §vpomia:*d    wdataiso
071040030722      *
071050060201     c**                 do        *hival
071060030722      * verifico se è un giorno lavorativo
071070060201     c**   *iso          move      wdataiso      ds_data
071080060201     C**                 eval      kann = ds_anno
071090060201     C**                 eval      kmes = ds_mese
071100060201     C**   kazcln        chain     azcln01l
071110060201     C**                 if        %found(azcln01l)
071120060201     C**                 if        MAT(ds_giorno) =  'F'  or
071130060201     C**                           POM(ds_giorno) =  'F'
071140030722      * se non va bene aggiungo 1 giorno
071150060201     c**                 adddur    1:*d          wdataiso
071160060201     C**                 iter
071170060201     c**                 else
071180060201     c**                 leave
071190060201     C**                 endif
071200060201     c**                 else
071210060201     c**                 leave
071220060201     C**                 endif
071230030722
071240060201     c**                 enddo
071250030710      * se viene modificato l'importo da assicurare rispetto a quanto
071260030710      * scritto sul file (stessa cosa anche x la divisa)
071270030710      * devo controllare se i gg. limite x la manutenzione sono trascorsi o meno
071280030710      * se sono già trascorsi emetto una window x richiedere la password
071290060201     c**                 If        (VidIas <> ArbIas or
071300060201     c**                           (VidVas <> ArbVas and ArbVas <> *Blanks)) and
071310060201     c**                            wdataoggi > wdataiso
071320060201     c**                           and wokpsw = *Blanks
071330060201     c**                 Exsr      Sr_Gespsw
071340060201     c** 90              GoTo      EndC03
071350060201     c**                 EndIf
071360941209     C***
071370011206     C*** V A L O R E   M E R C E   D I C H I A R A T O ***
071380941209     C* OBBLIGATORIO SE DESTINATARIO ESTERO E C'E' IN TABELLA NAZIONE
071390941209     C* IL FLAG EFTA
071400950120     C* SE HO VARIATO L'IMPORTO LO SALVO
071410950120     C     *IN93         IFEQ      *ON
071420950120     C                   MOVEL     '1'           WIN93             1
071430950120     C                   ENDIF
071440941209     C*
071450941209     C                   MOVEL     VIDVAD        WVLT
071460941209     C                   Z-ADD     VIDVMD        W0133            13 3
071470941209     C* CONTROLLO LA DIVISA
071480941209     C                   EXSR      CTRVLT
071490941209     C                   MOVEL     WVLT          VIDVAD
071500941209     C* ERRORE
071510941209     C   90              SETON                                        44
071520941209     C   90              GOTO      ENDC03
071530011010      *
071540011010      * Controllo se la divisa immessa è scaduta o no
071550011010     C     ARBDSP        IFGT      §CVDLI
071560011105     C     VIDVMD        ANDGT     0
071570011102     C                   MOVEL     MSG(102)      VIDMSG
071580011010     C                   SETON                                        449028
071590011010     C                   GOTO      ENDC03
071600011010     C                   ENDIF
071610941215     C*
071620941215     C* SE HO MODIFICATO LA VALUTA DEVO RIDIGITARE L'IMPORTO
071630941215     C* INDICATORE DI CHANGE 93 ON
071640941215    2C     VARVAD        IFNE      VIDVAD
071650950120     C     WIN93         ANDEQ     *BLANKS
071660941219     C     VIDVAD        IFNE      *BLANKS
071670941219     C     VIDVMD        ORNE      0
071680941215     C                   MOVEL     MSG(28)       VIDMSG
071690941215     C                   SETON                                        439028
071700941215     C                   GOTO      ENDC03
071710941215    2C                   ENDIF
071720941219     C                   ENDIF
071730941215     C*
071740941215     C                   MOVEL     VIDVAD        VARVAD
071750950120     C                   CLEAR                   WIN93
071760011205
071770011205      * pulisco il flag x la forzatura importo da assicurare forzabile
071780011205     c                   if        vidvmd <> sav1_vidvmd
071790011205     c                   clear                   forzavmd
071800011205     c                   endif
071810011205      *---------------------
071820011205     c                   eval      sav1_vidvmd = vidvmd
071830011205      * Controllo il valore merce tramite il trul23r
071840011205     c                   clear                   trul23ds
071850011205     c                   eval      i23tla = 'L'
071860011205     c                   eval      i23cbo = arbcbo
071870011205     c                   eval      i23tsp = arbtsp
071880011205     c                   eval      i23lnp = arblnp
071890011205     c                   eval      i23nzm = arbnzm
071900011205     c                   eval      i23lna = arblna
071910011205     c                   eval      i23nzd = arbnzd
071920011205     c                   movel     vidklp        i23ksc
071930011205     c                   move      vidksc        i23ksc
071940011205     c                   eval      i23ctr = vidctr
071950011206     c                   eval      i23imp = vidvmd
071960011206     c                   eval      i23div = vidvad
071970011205     c                   eval      i23ute = knmus
071980011205     c                   eval      i23pgm = nompgm
071990011205     c                   call      'TRUL23R'
072000011205     c                   parm                    trul23ds
072010011205      * se ho il flag del limite minimo impostato e l'importo è a zero
072020011205      * è obbligatorio
072030011206  2b c                   if        o23fmn <> *blanks and vidvmd = 0
072040011205     c                   eval      vidmsg = msg(45)
072050011205     c                   eval      *in43 = *on
072060011205     c                   eval      *in90 = *on
072070011205     c                   eval      *in28 = *on
072080011205     c                   goto      endc03
072090011206  2e c                   endif
072100011205      * controllo che non sia minore del limite previsto
072110011206  2b c                   if        o23fmn <> *blanks
072120011205     c                   eval      vidmsg = msg(104)
072130011210     c                   eval      %subst (vidmsg:37:14) = (%editc(o23lmn: '4'))
072140011205     c                   eval      *in43 = *on
072150011205     c                   eval      *in90 = *on
072160011205     c                   eval      *in28 = *on
072170011205     c                   goto      endc03
072180011206  2e c                   endif
072190011205      * controllo che non sia maggiore del limite previsto
072200011205      * non forzabile
072210011206  2b c                   if        o23fx2 <> *blanks
072220011205     c                   eval      vidmsg = msg(105)
072230011210     c                   eval      %subst (vidmsg:53:14) = (%editc(o23lx2: '4'))
072240011205     c                   eval      *in43 = *on
072250011205     c                   eval      *in90 = *on
072260011205     c                   eval      *in28 = *on
072270011205     c                   goto      endc03
072280011206  2e c                   endif
072290011205      * controllo che non sia maggiore del limite previsto
072300011205      * forzabile
072310011206  2b c                   if        o23fx1 <> *blanks and forzavmd = *blanks
072320011205     c                   eval      forzavmd = '1'
072330011205     c                   eval      vidmsg = msg(106)
072340011210     c                   eval      %subst (vidmsg:30:14) = (%editc(o23lx1: '4'))
072350011205     c                   eval      *in43 = *on
072360011205     c                   eval      *in90 = *on
072370011205     c                   eval      *in28 = *on
072380011205     c                   goto      endc03
072390011206  2e c                   endif
072400930507     C*
072410930507    1C                   END
072420950424     C*
072430950426     C*** CONTROLLI PER CLIENTE CON TARIFFA A VALORE CON FLAG = '3' ***
072440950426    1C     WTKS          IFEQ      'S'
072450950424     C                   MOVEL     VIDCTR        W001A             1
072460950426    2C     W001A         IFEQ      '4'
072470950424     C     TAMFVD        ANDEQ     '3'
072480950426     C*
072490950426    3C     VIDKLP        IFNE      VARKLP
072500950426     C     VIDKSC        ORNE      VARKSC
072510950426     C     VIDIAS        ORNE      COMIAS
072520950426     C                   SELECT
072530950426     C* FATTURA IMMEDIATA
072540950426     C* Se modificato il cod.cliente o l'importo da assicurare
072550950426     C* è necessario che la q.tà da fatturare e importi tassazione
072560950426     C* vengano azzerati + F14
072570950426     C     *IN14         WHENEQ    *ON
072580991001     C* Carico varie del video in schiera per vedere se c'è l'inoltro
072590991001     C                   MOVEL     '1'           WCKINL            1
072600991001     C                   EXSR      CARWAR
072610950426    4C     VIDQFT        IFNE      *ZEROS
072620950426     C     VIDIMV        ORNE      *ZEROS
072630950426     C     VIDPOR        ORNE      *ZEROS
072640991001     C     WINL          OREQ      '1'
072650950426     C     *INKN         OREQ      *OFF
072660950426     C                   SETON                                        429028
072670950426     C                   MOVEL     MSG(71)       VIDMSG
072680950426     C                   GOTO      ENDC03
072690950426    4C                   END
072700950426     C* SEGUE FATTURA
072710950426     C* Se modificato il cod.cliente o l'importo da assicurare
072720950426     C* è necessario che la q.tà da fatturare e tot imponibile
072730950426     C* vengano azzerati
072740950426     C     *IN13         WHENEQ    *ON
072750950426    4C     VIDQFT        IFNE      *ZEROS
072760950426     C     VIDIMV        ORNE      *ZEROS
072770950426     C                   SETON                                        429028
072780950426     C                   MOVEL     MSG(70)       VIDMSG
072790950426     C                   GOTO      ENDC03
072800950426    4C                   END
072810950426     C                   ENDSL
072820950426     C                   MOVE      VIDKLP        VARKLP
072830950426     C                   MOVE      VIDKSC        VARKSC
072840950426     C                   MOVE      VIDIAS        COMIAS
072850950426     C                   MOVE      *ZEROS        VARQFT
072860950426    3C                   END
072870950426     C*
072880950426     C*** QUANTITA' DA FATTURARE ***
072890950426     C*
072900950426     C* Errore se modificata e diversa da 0
072910950426    3C     VIDQFT        IFNE      VARQFT
072920950426    4C     VIDQFT        IFNE      *ZEROS
072930950426     C                   MOVEL     MSG(68)       VIDMSG
072940950424     C                   SETON                                        429028
072950950424     C                   GOTO      ENDC03
072960950426    4C                   END
072970950426     C* Se azzerata q.tà da fatturare gli
072980950426     C* importi della tassazione devono essere = 0
072990950426     C                   SELECT
073000950426    4C     *IN14         WHENEQ    *ON
073010991001     C* Carico varie del video in schiera per vedere se c'è l'inoltro
073020991001     C                   MOVEL     '1'           WCKINL            1
073030991001     C                   EXSR      CARWAR
073040950426    5C     VIDIMV        IFNE      *ZEROS
073050950426     C     VIDPOR        ORNE      *ZEROS
073060991001     C     WINL          OREQ      '1'
073070950426     C     *INKN         OREQ      *OFF
073080950426     C                   SETON                                        499028
073090950426     C                   MOVEL     MSG(69)       VIDMSG
073100950426     C                   GOTO      ENDC03
073110950426    5C                   END
073120950426    4C     *IN13         WHENEQ    *ON
073130950426    5C     VIDIMV        IFNE      *ZEROS
073140950426     C                   SETON                                        499028
073150950426     C                   MOVEL     MSG(72)       VIDMSG
073160950426     C                   GOTO      ENDC03
073170950426    5C                   END
073180950426    4C                   ENDSL
073190950424     C                   MOVE      VIDQFT        VARQFT
073200950426    3C                   END
073210950424     C*
073220950426    2C                   END
073230950426    1C                   END
073240950424     C*
073250050601     c* Controlli su aggiunte/aumenti di importo da assicurare
073260080625    1c                   If        (VidIas <> ArbIas and vidias > 0 or
073270050804     c                             (VidVas <> ArbVas and vidVas <> *Blanks
073280050804     c                             and Vidias>0))
073290050601     c* verifico se cliente con mandato
073300050601     c                   exsr      tassar
073310050601     c* cliente senza mandato: si può aggiungere/aumentare l'importo solo
073320050601     c* se inviata e-mail
073330050601     c* idem per cliente con mandato se il valore è oltre mandato
073340080625    2c                   if        task88 = 'S'
073350050601     c* vedo se inviata e-mail
073360050609     c                   movel     'c'           kta4trc
073370050601     c                   clear                   dta4c
073380050601     c     kta4          chain     tita430c
073390080625    3c                   if        %found(tita430c)
073400050601     c                   movel     ta4not        dta4c
073410080625    3c                   endif
073420080625    3c                   if        §ta4mail <> 'S'
073430080625    4c                   if        §utemtf=' '
073440050601     c                   seton                                        409028
073450050601     c                   eval      vidmsg=msg(135)
073460050601     c                   goto      endc03
073470080625    4c                   endif
073480080625    4c                   if        (§utemtf='I' or §utemtf='E') and
073490080625     c                             forzaias2=' '
073500080625     c                   eval      forzaias2='1'
073510080625     c                   seton                                        409028
073520080625     c                   eval      vidmsg=msg(146)
073530080625     c                   goto      endc03
073540080625    4c                   endif
073550080625    3c                   endif
073560080625    2c                   endif
073570080625    1c                   endif
073580941221     C*
073590941221     C* F14 - TASSAZIONE
073600991011     C     *INKN         IFEQ      *ON
073610991029     C* Fattura immediata SE NON CONTABILIZZATA
073620991011     C     *IN14         OREQ      *ON
073630991029     C     *IN29         ANDEQ     *OFF
073640991029     c* Prepagati NON CONTABILIZZATA
073650991011     C     *IN10         OREQ      *OFF
073660991011     C     VIDNFT        ANDGT     *ZEROS
073670991029     C     *IN29         ANDEQ     *OFF
073680980513     C                   MOVEL     '1'           WBOL              1
073690011018      * Se è una bolla di reso con varia 'N' 88888888
073700011017      * tasso la varia 'N'
073710011018     C     FLGVRN        IFEQ      '1'
073720011017      * leggo il sbfl delle varie
073730011017     C                   DO        20            NRR
073740011017     C     NRR           CHAIN     LR48S03                            32
073750011017     C     V3CSV1        IFEQ      'N'
073760011017     C     V3CVA1        ANDEQ     88888888
073770011017     C                   EXSR      TASSAN
073780011017     C  N32              UPDATE    LR48S03
073790011017     C                   ENDIF
073800011017     C                   ENDDO
073810011017     C                   ENDIF
073820011017      *
073830151020     c                   if        vidimv<>PesVolImv
073840151016     c                   clear                   PesVolPkcn
073850151016     c                   clear                   PesVolSpc
073860151016     c                   clear                   PesVolImv
073870151020     c                   endif
073880151020     c***                clear                   Tassatoimv
073890941221     C                   EXSR      TASSAZ
073900151019     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
073910151020     c***                if        vidimv>0 and vidimv<>PesVolImv
073920151020     c***                eval      PesVolPkcn=taspkcn
073930151020     c***                eval      PesVolSpc=tasspc
073940151020     c***                eval      PesVolIMV=vidimv
073950151020     c***                endif
073960990702     C* SE SI TRATTA DI UN PREPAGATO DEVO ANCHE VISUALIZZARE IL TOT
073970990702     C*  IMPONIBILE ARROTONDATO
073980990702     C     *IN23         IFEQ      *ON
073990151020     c****               eval      TassatoImv=vidimv
074000990702     C                   Z-ADD     AR6DFT        ARBDFT
074010990702     C                   EXSR      TOTFAT
074020990702     C                   Z-ADD     D04IFT        VIDIFT
074030990702     C                   Z-ADD     D04IMV        VIDIMV
074040990702     C                   Z-ADD     D04POR        VIDPOR
074050991006     C                   Z-ADD     D04POR        VARPOR
074060990702     C                   Z-ADD     D04IMV        SAVIMV
074070990702     C                   ENDIF
074080151020     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
074090151020     c                   if        vidimv>0 and vidimv<>PesVolImv
074100151020     c                   eval      PesVolPkcn=taspkcn
074110151020     c                   eval      PesVolSpc=tasspc
074120151020     c                   eval      PesVolIMV=vidimv
074130151020     c                   endif
074140131016
074150131016     c                   if        *inkn
074160131016     c                   eval      winf14='1'
074170131016     c                   endif
074180991011     C   KN
074190991011     COR 90              GOTO      ENDC03
074200990702    1C                   END
074210991006     C* Se divisa sprotetta significa che manca tariffa: in questo caso
074220991006     C* controllo che la divisa inserita sia = alla divisa della moneta
074230991006     C*  di conto SE CLIENTE 8888 o 9999
074240991006     C*
074250991006     C* Fattura immediata
074260991006     C     *IN14         IFEQ      *ON
074270991006     C* Prepagati
074280991006     C     *IN10         OREQ      *OFF
074290991006     C     VIDNFT        ANDGT     *ZEROS
074300991006     C* Divisa sprotetta
074310991006     C     *IN07         IFEQ      *OFF
074320991006     C* Divisa diversa dalla divisa della moneta di conto
074330991006     C     VIDDIV        ANDNE     §GEDCN
074340991006     C* Cliente 8888 o 9999
074350991006     C     VIDKSC        IFEQ      8888
074360991006     C     VIDKSC        OREQ      9999
074370991006     C                   SETON                                        459028
074380991006     C                   MOVEA     MSG(90)       K78
074390991006     C                   MOVEA     §GEDCN        K78(71)
074400991006     C                   MOVEA     K78           VIDMSG
074410991006     C                   GOTO      ENDC03
074420991006     C                   ENDIF
074430991006     C                   ENDIF
074440991006     C                   ENDIF
074450941102     C**
074460991124     C* SE ABBLENCATA DIVISA ERRORE SE ESISTE ALMENO UN IMPORTO DI
074470991124     C* TASSAZIONE
074480991124    1C     *IN07         IFEQ      *OFF
074490991124     C     VIDDIV        ANDNE     WDIV
074500991124     C     VIDDIV        ANDEQ     *BLANKS
074510991124    2C     VIDPOR        IFGT      *ZEROS
074520991125     C                   SETON                                        459028
074530991124     C                   MOVEL     MSG(94)       VIDMSG
074540991124     C                   GOTO      ENDC03
074550991124   X2C                   ELSE
074560991124    3C                   DO        20            NRR
074570991124     C     NRR           CHAIN     LR48S03                            30
074580991124    4C     V3CVA1        IFGT      *ZEROS
074590991125     C                   SETON                                        459028
074600991124     C                   MOVEL     MSG(94)       VIDMSG
074610991124     C                   GOTO      ENDC03
074620991124    4C                   ENDIF
074630991124    3C                   ENDDO
074640991124    2C                   ENDIF
074650991124    1C                   ENDIF
074660991005     C* PER CONTROLLARE LA TASSAZIONE RICHIAMO PGM FNLV16R
074670991012     C* SE LA DIVISA E' MODIFICATA RICHIAMO L'LV16 SOLO PER IL
074680991012     C* CONTROLLO DELLA DIVISA
074690991013    0C     *IN07         IFEQ      *OFF
074700991013     C     VIDDIV        ANDNE     WDIV
074710991026     C     WDIV          ANDNE     *BLANKS
074720100120     c
074730030612     C                   CLEAR                   Fnlv16DS
074740991012     C                   CLEAR                   DTASV
074750100120     C                   CLEAR                   dlv1601
074760100120
074770991012     C                   MOVEL     'D'           D17FIM
074780991012     C                   MOVE      VIDDIV        D17DIV
074790991012     C     VIDDFT        IFGT      *ZEROS
074800991012     C                   CLEAR                   WLBDAT
074810991012     C                   Z-ADD     VIDDFT        G02DAT
074820991012     C                   MOVEL     *BLANK        G02ERR
074830991012     C                   CALL      'XSRDA8'
074840991012     C                   PARM                    WLBDAT
074850991012     C                   Z-ADD     G02INV        D17DSP
074860991012     C                   ELSE
074870991012     C                   Z-ADD     ARBDSP        D17DSP
074880991012     C                   END
074890991012     C                   CALL      'FNLV16R'
074900030612     C                   PARM                    Fnlv16DS
074910991012     C                   PARM                    DTASV
074920991014     C                   MOVEL     D17DIV        VIDDIV
074930991012   X0C                   ELSE
074940030612     C                   CLEAR                   Fnlv16DS
074950991005     C                   CLEAR                   DTASV
074960941222     C                   MOVEL     D48TBO        D17TBO
074970991006     C   23              MOVEL     'S'           D17PRP
074980941209     C                   MOVEL     KSC           D17KSC
074990991012     C     VIDDFT        IFGT      *ZEROS
075000991012     C                   CLEAR                   WLBDAT
075010991012     C                   Z-ADD     VIDDFT        G02DAT
075020991012     C                   MOVEL     *BLANK        G02ERR
075030991012     C                   CALL      'XSRDA8'
075040991012     C                   PARM                    WLBDAT
075050991012     C                   Z-ADD     G02INV        D17DSP
075060991012     C                   ELSE
075070991012     C                   Z-ADD     ARBDSP        D17DSP
075080991012     C                   END
075090941209     C                   MOVEL     ARBLNP        D17LNP
075100941209     C                   MOVEL     ARBLNA        D17LNA
075110941209     C                   MOVEL     §3ATB1        D17TBL
075120941209     C                   MOVEL     §3ARBL        D17RBL
075130020503     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
075140100120     C                   MOVEL     §3ASVA        §D17sva
075150100120
075160100120     c* Se fattura immediata o prepagato
075170100120     c*                        controllo evetuale esenzione iva
075180100121     c                   if        *in14   or (not *in10 and vidnft>0)
075190100120     c                   Eval      §d17ctriso='S'
075200100120     c                   if        arbcpi<>*blanks
075210100120     C     'PRIVATO'     SCAN      arbcpi        posiz                    30
075220100120     c* solo numeri
075230100120     c                   select
075240100120     c                   when      not *in30 and %subst(arbcpi:1:2)>=*zeros
075250100120     c                   eval      §d17iso='IT'
075260100120     c* "PRIVATO" senza iso
075270100120     c                   when      *in30 and posiz<=3
075280100120     c                   eval      §d17iso='$$'
075290100120     c                   other
075300100120     c                   eval      §d17iso= %subst(arbcpi:1:2)
075310100120     c                   ENDSL
075320100120     c                   endif
075330100120     c                   endif
075340100120     c
075350100120     c                   Movel     Dlv1601       D17Flo
075360100120     c
075370941209     C                   Z-ADD     VIDIAS        D17IAS
075380941209     C                   Z-ADD     VIDQFT        D17QFT
075390941209     C                   Z-ADD     VIDPOR        D17POR
075400991005     C                   MOVEL     VIDDIV        D17DIV
075410991006     C                   MOVEL     WDIV          D17DVP
075420941209     C                   Z-ADD     VIDIMV        D17IMV
075430990702    1C     §3AFCA        IFEQ      1
075440941209     C                   MOVEL     'S'           D17FCA
075450990702    1C                   ENDIF
075460990702    1C* SE C'E' NUMERO E DATA FATTURA RIPASSO IL TOTALE IMPONIBILE
075470950114     C     *IN14         IFEQ      *ON
075480991005     C                   MOVEL     'E'           D17FIM
075490950114     C*
075500990702   X1C                   ELSE
075510950114     C*
075520950114     C                   MOVEL     'C'           D17FIM
075530990702    2C     VIDFIV        IFGT      0
075540950114     C     VIDNFT        ANDGT     0
075550950114     C     VIDDFT        ANDGT     0
075560991005     C                   MOVEL     'E'           D17FIM
075570990702    2C                   ENDIF
075580990702    1C                   ENDIF
075590941209     C                   MOVEL     VIDCEI        D17CEI
075600991029     C* SE PROTETTA LA TASSAZIONE, NON PASSO I DATI DELLA FATTURA
075610941209     C                   MOVEL     VIDFIV        D17FIV
075620941220     C                   MOVEL     'S'           D17FFI
075630941209     C                   MOVEL     VIDNFT        D17NFT
075640941209     C                   Z-ADD     VIDDFT        D17DFG
075650941209     C*
075660991012     C* RICHIAMO ROUTINE PER CARICAMENTO VARIE DELLA DTASV
075670991012     C                   EXSR      RDTASV
075680941209     C*
075690991005     C                   CALL      'FNLV16R'
075700030612     C                   PARM                    Fnlv16DS
075710991005     C                   PARM                    DTASV
075720941102     C*
075730991011     C                   MOVEL     D17DIV        VIDDIV
075740941209     C                   MOVEL     D17CEI        VIDCEI
075750991005     C                   MOVEL     O17DEI        DESCEI
075760991005     C                   MOVEL     O17DEI        V3DCEI
075770991006     C     D17FIM        IFEQ      'E'
075780991014     C     O17ERR        ANDEQ     *BLANKS
075790030620      * OPPURE SE BOLLA CON 1 SOLA VARIA VALORIZZO L'IMPONIBILE ANCHE SE SI TRATTA DI SEGUE FATTURA
075800030620      * SOLO SE LA VARIA ESISTE
075810040123     C**!!!§3ASVA        ORNE      *BLANKS
075820040123     C**!!!WVAR3A        ANDEQ     'S'
075830040123     C**!!!O17ERR        ANDEQ     *BLANKS
075840040123     C**!!!D17FIM        ANDEQ     'C'
075850991005     C                   MOVEL     O17IMV        VIDIMV
075860991006     C                   ENDIF
075870030620      * se bolla con singola varia e la varia non  c'è azzero l'imponibile per i segue fattura
075880030620     C     §3ASVA        IFNE      *BLANKS
075890030620     C     WVAR3A        ANDNE     'S'
075900030620     C     D17FIM        ANDEQ     'C'
075910030620     C                   Z-ADD     *ZEROS        VIDIMV
075920030620     C                   ENDIF
075930030620      ****
075940991029     C                   MOVEL     D17DFG        VIDDFT
075950941209     C* VARIE
075960990702    1C                   DO        20            NRR
075970941215     C     NRR           CHAIN     LR48S03                            30
075980941209     C                   MOVEL     VSV(NRR)      V3CSV1
075990941209     C                   MOVEL     VES(NRR)      V3CEV1
076000941215     C* SE C'E' ERRORE SU VARIA, ACCENDO INDICATORE DI POSIZIONAMENTO
076010991006    2C     O17ERV        IFEQ      NRR
076020941220     C                   SETON                                        5290
076030941215     C                   ELSE
076040941215     C                   SETOFF                                       52
076050990702    2C                   ENDIF
076060941215     C  N30              UPDATE    LR48S03
076070941215     C   30              WRITE     LR48S03
076080990702    1C                   ENDDO
076090991012    0C                   ENDIF
076100941215     C*
076110941215     C                   SETOFF                                       52
076120941102     C*  ERRORE
076130991006    1C     O17ERR        IFNE      *BLANKS
076140991006     C     O17FAS        OREQ      'S'
076150941102     C*
076160991006    2C     O17MSG        IFNE      *BLANKS
076170991006     C                   MOVEL     O17MSG        VIDMSG
076180941209     C                   SETON                                        28
076190941102    2C                   ENDIF
076200990415     C**
076210990415     C* VARIA R  MSG
076220991006    2C     O17ERR        IFEQ      '6'
076230991006     C     O17MSG        ANDEQ     MSG(85)
076240991006     C     O17FAS        OREQ      'S'
076250990416     C*
076260990415     C     COMASS        IFEQ      ' '
076270990415     C* SE MODIFICATO L'IMPORTO D'ASSICURARE E FATTURA GIA' INCASSATA O TASSATA
076280990415    3C     VIDIAS        IFNE      ARBIAS
076290990415     C     VIDVAS        ORNE      ARBVAS
076300990415     C*
076310990415     C* SE GIA' TASSATO FINO ALL'IMPONIBILE E LA DATA FATTURA > 0
076320990415    4C     AR6IMV        IFNE      *ZEROS
076330990415     C                   SETON                                        2882
076340990415     C                   SETOFF                                       90
076350990415     C* ERRORE NON BLOCCANTE:CORREGGERE  VARIA R
076360990419     C                   SELECT
076370990419    5C     ARBIAS        WHENEQ    0
076380990415     C     VIDIAS        ANDGT     0
076390990415     C                   MOVEL     MSG(83)       VIDMSG
076400990419    5C     ARBIAS        WHENGT    0
076410990419     C     VIDIAS        ANDEQ     0
076420990415     C                   MOVEL     MSG(84)       VIDMSG
076430990419     C                   OTHER
076440990419     C                   MOVEL     MSG(86)       VIDMSG
076450990419    5C                   ENDSL
076460990415    4C                   END
076470990415     C                   MOVEL     '1'           COMASS            1
076480990415    3C                   END
076490990415     C                   ELSE
076500990415     C                   SETOFF                                       9028
076510990415     C                   MOVEL     *BLANKS       VIDMSG
076520990415     C                   END
076530990415    2C                   END
076540990415     C* FATTURA
076550941102     C**
076560941102     C* ESENZIONE IVA
076570991006    2C     O17ERR        IFEQ      '1'
076580941209     C                   SETON                                        5190
076590941102    2C                   ENDIF
076600941102     C**
076610941102     C* IMPONIBILE
076620991006     C     O17ERR        IFEQ      '5'
076630941209     C                   SETON                                        5090
076640941102    2C                   ENDIF
076650941222     C* NUMERO FATTURA
076660991006     C     O17ERR        IFEQ      '2'
076670941222     C                   SETON                                        5590
076680941222    2C                   ENDIF
076690941222     C* FILIALE IVA FATTURA
076700991006     C     O17ERR        IFEQ      '3'
076710941222     C                   SETON                                        5490
076720941222    2C                   ENDIF
076730941222     C* DATA FATTURA
076740991006     C     O17ERR        IFEQ      '4'
076750941222     C                   SETON                                        5690
076760941222    2C                   ENDIF
076770991006     C* DIVISA
076780991006     C     O17ERR        IFEQ      '7'
076790991006     C                   SETON                                        4590
076800991006    2C                   ENDIF
076810991012     C* PORTO
076820991012     C     O17ERR        IFEQ      '8'
076830001116     C                   SETON                                        4990
076840991012    2C                   ENDIF
076850941102     C**
076860941220     C                   GOTO      ENDC03
076870941102    1C                   ENDIF
076880040123      * se tipo bolla 'monovaria' e la varia non è esposta o è negata
076890040123      * non posso avere altre varia con importo > 0
076900040123if  1c                   If        §3asva <> *Blanks
076910040123do  2c                   Do        20            nrr
076920040123     c     nrr           Chain     Lr48s03                            30
076930040123     c                   Eval      *In52 = *Off
076940040123if  3c                   If        wvar3a <> *Blanks  and wvar3ai = *Blanks and
076950040123     c                             V3cSv1 <> *Blanks and V3cVa1 > *Zeros
076960040123     c                   Eval      *In28 = *On
076970040123     c                   Eval      *In52 = *On
076980040123     c                   Eval      *In90 = *On
076990040123     c                   Eval      VidMsg = Msg(127)
077000040123     c                   Eval      %subst(VidMsg:75:1) = §3asva
077010040123    3c                   EndIf
077020040123if  3c                   If        wvar3a = *Blanks  and
077030040123     c                             V3cSv1 <> *Blanks and V3cVa1 > *Zeros
077040040123     c                   Eval      *In28 = *On
077050040123     c                   Eval      *In52 = *On
077060040123     c                   Eval      *In90 = *On
077070040123     c                   Eval      VidMsg = Msg(127)
077080040123     c                   Eval      %subst(VidMsg:75:1) = §3asva
077090040123    3c                   EndIf
077100040123     c  n30              Update    Lr48s03
077110040127     c   90              Goto      Endc03
077120040123    2c                   EndDo
077130040123    1c                   EndIf
077140991006     C*
077150991006     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
077160991006    1C  N07VIDDIV        IFNE      WDIV
077170991006     C* CONVERSIONE PORTO
077180991006    2C     VIDPOR        IFEQ      VARPOR
077190991006     C     VIDPOR        ANDGT     *ZEROS
077200991006     C     WDIV          ANDNE     *BLANKS
077210030612     C                   CLEAR                   YeurcoDS
077220991006     C                   MOVEL     WDIV          YECDVI
077230991006     C                   Z-ADD     VIDPOR        YECIMI
077240991006     C                   MOVEL     VIDDIV        YECDVO
077250991006     C     O17FDC        IFEQ      'S'
077260011114     C                   MOVE      '3'           YECDCO
077270991006     C                   ENDIF
077280991006     C                   CALL      'YEURCO'
077290030612     C                   PARM                    YeurcoDS
077300991006     C                   Z-ADD     YECIMO        VIDPOR
077310991012     C                   SETON                                        49
077320991006    2C                   END
077330991006     C* CONVERSIONE VARIE
077340991006    2C                   DO        20            NRR
077350991006     C     NRR           CHAIN     LR48S03                            30
077360991006    3C     V3CVA1        IFEQ      VARVA1
077370991006     C     V3CVA1        ANDGT     *ZEROS
077380991006     C     WDIV          ANDNE     *BLANKS
077390011019      *
077400011019      * Se la bolla non è tassata fino all'imponibile e c'è la varia 'N' 88888888
077410011019      * non devo convertire la varia 'N' 88888888
077420011019     C     VIDIMV        IFEQ      *ZEROS
077430011019     C     FLGVRN        ANDEQ     '1'
077440011019     C     V3CSV1        ANDEQ     'N'
077450011019     C     V3CVA1        ANDEQ     88888888
077460011019     C  N30              UPDATE    LR48S03
077470011019     C                   ITER
077480011019     C                   ENDIF
077490030624      * Se la bolla non è tassata fino all'imponibile e c'è la varia '&' 88888888
077500030624      * non devo convertire la varia '&' 88888888
077510030624     C     VIDIMV        IFEQ      *ZEROS
077520030624     C     V3CSV1        ANDEQ     '&'
077530030624     C     V3CVA1        ANDEQ     88888888
077540030624     C  N30              UPDATE    LR48S03
077550030624     C                   ITER
077560030624     C                   ENDIF
077570011019      *
077580030612     C                   CLEAR                   YeurcoDS
077590991006     C                   MOVEL     WDIV          YECDVI
077600991006     C                   Z-ADD     V3CVA1        YECIMI
077610991006     C                   MOVEL     VIDDIV        YECDVO
077620991006     C     O17FDC        IFEQ      'S'
077630011114     C                   MOVE      '3'           YECDCO
077640991006     C                   ENDIF
077650991006     C                   CALL      'YEURCO'
077660030612     C                   PARM                    YeurcoDS
077670991006     C                   Z-ADD     YECIMO        V3CVA1
077680991012     C                   SETON                                        53
077690991006    3C                   ENDIF
077700991006     C                   Z-ADD     V3CVA1        VARVA1
077710991006     C  N30              UPDATE    LR48S03
077720991006    2C                   ENDDO
077730991006     C* SE TOTALE IMPONIBILE E' IMPOSTATO LO AZZERO E RICHIAMO
077740991006     C* FNLV16R PER RICALCOLARLO IN BASE ALLA NUOVA DIVISA
077750991006     C     VIDIMV        IFGT      *ZEROS
077760991006     C     WDIV          ANDNE     *BLANKS
077770991006     C                   MOVEL     'T'           D17FIM
077780991006     C                   MOVEL     *ZEROS        D17IMV
077790991006     C                   Z-ADD     VIDPOR        D17POR
077800991012     C                   CLEAR                   DTASV
077810991012     C                   EXSR      RDTASV
077820991006     C                   CALL      'FNLV16R'
077830030612     C                   PARM                    Fnlv16DS
077840991006     C                   PARM                    DTASV
077850991006     C                   Z-ADD     O17IMV        VIDIMV
077860991006     C                   ENDIF
077870991006     C                   Z-ADD     VIDPOR        VARPOR
077880991006     C                   MOVEL     VIDDIV        WDIV
077890991012     C* SE VARIATA DIVISA RIEMETTO IN OGNI CASO LA VIDEATA
077900991029     C* PERCHE' L'LV16 DOVRA' CONTROLLARE LA TASSAZIONE
077910991012     C                   SETON                                        90
077920991012     C                   GOTO      ENDC03
077930991006   X1C                   ELSE
077940991006     C* SE NON VARIATA LA DIVISA DEVO COMUNQUE MEMORIZZARE EVENTUALI
077950991006     C* VARIAZIONI EFFETTUATE SUGLI IMPORTI DI TASSAZIONE
077960991006     C                   Z-ADD     VIDPOR        VARPOR
077970991006    2C                   DO        20            NRR
077980991006     C     NRR           CHAIN     LR48S03                            30
077990991006     C                   Z-ADD     V3CVA1        VARVA1
078000991006     C  N30              UPDATE    LR48S03
078010991006    2C                   ENDDO
078020991006    1C                   ENDIF
078030941102     C*
078040941102     C* PER COD 8888 O 9999 CON VARIA "R" OBBLIGATORIO IMPORTO DA ASSIC
078050941209     C* SE C'E' "S" IN D17FAS INDICA QUESTO TIPO DI ERRORE
078060991006    3C     O17FAS        IFEQ      'S'
078070941102     C*
078080941102     C* SE = 0 E FORZO CON ENTER PRENDO VALORE DA TARIFFA
078090990702    2C     SEI           IFNE      1
078100941209     C                   MOVEL     MSG(46)       VIDMSG
078110941209     C                   SETON                                        5290
078120941102     C                   ADD       1             SEI
078130941102     C                   GOTO      ENDC03
078140990702    2C                   ENDIF
078150990702    1C                   ENDIF
078160941222     C*
078170941222     C* IN PARTENZA SE C'ERA PREPAGATO, DEVO LASCIARLO
078180990702    1C     *IN10         IFEQ      *OFF
078190990702    2C     AR6NFT        IFNE      0
078200941222     C     VIDNFT        ANDEQ     0
078210941222     C                   SETON                                        559028
078220941222     C                   MOVEL     MSG(60)       VIDMSG
078230941222     C                   GOTO      ENDC03
078240941222    2C                   ENDIF
078250990702     C*
078260990702     C* SE PREPAGATO INSERIRE TASSAZIONE O F14 PER TASSARE
078270990702     C  NKL
078280990702    2CANNKN*IN23         IFEQ      *ON
078290990702    3C     VIDIMV        IFEQ      0
078300990702     C                   SETON                                        529028
078310990702     C                   MOVEL     MSG(89)       VIDMSG
078320990702     C                   GOTO      ENDC03
078330990702   X3C                   ELSE
078340990702    4C     VIDIMV        IFNE      SAVIMV
078350990702     C                   Z-ADD     AR6DFT        ARBDFT
078360990702     C                   EXSR      TOTFAT
078370990702     C                   Z-ADD     D04IFT        VIDIFT
078380990702     C                   Z-ADD     D04IMV        VIDIMV
078390990702     C                   Z-ADD     D04POR        VIDPOR
078400991006     C                   Z-ADD     D04POR        VARPOR
078410990702     C                   Z-ADD     VIDIMV        SAVIMV
078420990702     C                   SETON                                        90
078430990702    4C                   ENDIF
078440990702    3C                   ENDIF
078450990702    2C                   ENDIF
078460990702    1C                   ENDIF
078470011018      * Se è bolla di reso con varia 'N' 88888888 deve esserci la varia 'N'
078480011017     C                   SETOFF                                       52
078490011018    1C     FLGVRN        IFEQ      '1'
078500011017    2C                   DO        20            NRR
078510011017     C     NRR           CHAIN     LR48S03                            32
078520011017    3C     *IN32         IFEQ      *OFF
078530011018     C                   SETON                                        52
078540011018    4C     V3CSV1        IFEQ      'N'
078550011017     C                   SETOFF                                       52
078560011017     C                   LEAVE
078570011018    4C                   ENDIF
078580011017    3C                   ENDIF
078590011017    2C                   ENDDO
078600011018    2C     *IN52         IFEQ      '1'
078610011017     C     1             CHAIN     LR48S03                            32
078620011017     C  N32              UPDATE    LR48S03
078630011017     C                   MOVEL     MSG(101)      VIDMSG
078640011105     C                   SETON                                        9028
078650011017     C                   GOTO      ENDC03
078660011017    2C                   ENDIF
078670011017    1C                   ENDIF
078680011206      *
078690011206      * Controllo che la somma degli importi immessi non superi il valore
078700011206      * impostato in §GEIMF
078710011206      *      prima controllo il totale imponibile
078720011206  1b c                   if        vidimv <> 0
078730011206     c                   eval      w0173 = vidimv
078740011206      *      controllo se devo convertire il totale imponibile
078750011206  2b c                   if        viddiv <> §gedcn
078760030612     c                   clear                   YeurcoDS
078770011206     c                   eval      yecdvi = viddiv
078780011206     c                   eval      yecdvo = §gedcn
078790011206     c                   eval      yecimi = vidimv
078800011206     c                   move      decsav        yecdco
078810011206     c                   call      'YEURCO'
078820030612     c                   parm                    YeurcoDS
078830011206  3b c                   if        yecesi = ' '
078840011206     c                   eval      w0173 = yecimo
078850011206  3e c                   endif
078860011206  2e c                   endif
078870011206      *      controllo se l'imponibile supera il limite fatturabile
078880011220  2b c                   if        w0173 > imfsav
078890011206     c                   eval      vidmsg = msg(107)
078900011220     c                   eval      %subst (vidmsg:54:14) = (%editc(imfsav: '4'))
078910011206     c                   eval      %subst (vidmsg:69:3) = §gedcn
078920011206     c                   eval      *in50 = *on
078930011206     c                   eval      *in90 = *on
078940011206     c                   eval      *in28 = *on
078950011206     c                   goto      endc03
078960011206  2e c                   endif
078970011206  1x c                   else
078980011206      *      altrimenti controllo tutti gli altri importi
078990011206     c                   eval      tot_importi = vidpor
079000011206      *      leggo le varie
079010011206  2b c                   do        20            nrr
079020011206     c     nrr           chain     lr48s03                            32
079030011206  3b c                   if        *in32 = *off and v3cva1 <> 0
079040011206  4b c                   if        flgvrn = '1' and v3cva1 = 88888888
079050011206     c                             and v3csv1 = 'N'
079060011206     c                   iter
079070011206  4e c                   endif
079080030624  4b c                   if        v3cva1 = 88888888
079090030624     c                             and v3csv1 = '&'
079100030624     c                   iter
079110030624  4e c                   endif
079120011206     c                   eval      tot_importi = tot_importi + v3cva1
079130011206  3e c                   endif
079140011206  2e c                   enddo
079150011206     c                   eval      w0173 = tot_importi
079160011206      *      controllo se devo convertire il totale importi
079170011206  2b c                   if        viddiv <> §gedcn
079180030612     c                   clear                   YeurcoDS
079190011206     c                   eval      yecdvi = viddiv
079200011206     c                   eval      yecdvo = §gedcn
079210011206     c                   eval      yecimi = tot_importi
079220011206     c                   move      decsav        yecdco
079230011206     c                   call      'YEURCO'
079240030612     c                   parm                    YeurcoDS
079250011206  3b c                   if        yecesi = ' '
079260011206     c                   eval      w0173 = yecimo
079270011206  3e c                   endif
079280011206  2e c                   endif
079290011206      *      controllo se l'imponibile supera il limite fatturabile
079300011220  2b c                   if        w0173 > imfsav
079310131016     c                   eval      vidmsg = msg(107)
079320011220     c                   eval      %subst (vidmsg:54:14) = (%editc(imfsav: '4'))
079330011206     c                   eval      %subst (vidmsg:69:3) = §gedcn
079340011206     c                   eval      *in50 = *on
079350011206     c                   eval      *in90 = *on
079360011206     c                   eval      *in28 = *on
079370011206     c                   goto      endc03
079380011206  2e c                   endif
079390011206  1e c                   endif
079400131016     c
079410131016     c* CONTROLLI PER BOLLA CONTABILIZZATA E PGM NON RICHIAMATO (29 ON)
079420131016     c* se f6=conferma
079430131016     c                   if        *in29 and *inkf
079440131016     c* Se eseguito F14=Tassazione --> errore bolla contabilizzata
079450131016     c                   if        winf14='1'
079460131016     c                   eval      vidmsg = msg(151)
079470131016     c                   eval      *in90 = *on
079480131016     c                   eval      *in28 = *on
079490131016     c                   goto      endc03
079500131016     c                   endif
079510131016     c* Se non variato nè ias nè vmd -> errore premere F12
079520131016     c                   if        vidias=bol_vidias and
079530131016     c                             (vidvas=bol_vidvas or vidias=0) and
079540131016     c                             vidvmd=bol_vidvmd and
079550131016     c                             (vidvad=bol_vidvad or vidvmd=0)
079560131016     c                   eval      vidmsg = msg(152)
079570131016     c                   eval      *in90 = *on
079580131016     c                   eval      *in28 = *on
079590131016     c                   goto      endc03
079600131016     c                   endif
079610131016     c                   endif
079620911210     C*
079630941209     C     ENDC03        ENDSR
079640030710      *---------------------------------------------------------------*
079650030710      * Gestione videata password x importo da assicurare             *
079660030710      *---------------------------------------------------------------*
079670060201     c**   Sr_Gespsw     BegSr
079680060201     C**
079690060201     c**                 Clear                   w01psw
079700060201     c**                 Do        *Hival
079710060201     c**                 Exfmt     lr48w01
079720060201     c**                 Eval      *In28 = *Off
079730060201     c**                 If        *InKl
079740060201     c**                 Eval      *In90 = *On
079750060201     c**                 Leave
079760060201     c**                 EndIf
079770060201     c**                 If        w01psw = *Blanks
079780060201     c**                 Eval      w01msg = 'Immettere la password'
079790060201     c**                 Eval      *In28 = *On
079800060201     c**                 Iter
079810060201     c**                 EndIf
079820060201     c**                 If        w01psw <> miapsw
079830060201     c**                 Eval      w01msg = 'Password errata'
079840060201     c**                 Eval      *In28 = *On
079850060201     c**                 Iter
079860060201     c**                 EndIf
079870060201     c**                 Eval      wokpsw = '1'
079880060201     c**                 Leave
079890060201     c**                 EndDo
079900060201     C**
079910060201     c**                 Endsr
079920920319     C*
079930930505     C* CONTROLLO COMUNI  ALL CONTR3 3 CONTR 4 -----------------------*
079940920319     C     CTRCOM        BEGSR
079950920319     C                   SETOFF                                       90
079960030620     C                   CLEAR                   SAV_INDSIN
079970941103     C**
079980920319     C* CODICE CLIENTE
079990941103     C**
080000950104     C*               SALTO CONTROLLO SE SPEDIZIONE IN FRANCO  IN ARRIV
080010950104     C*               LO ESEGUO SE SONO IN PARTENZA
080020941103    1C     *IN36         IFEQ      *OFF
080030941222     C     *IN10         OREQ      *OFF
080040941103     C*
080050941103     C* F7=RICERCA ALFABETICA
080060941103    2C   KG              DO
080070941212     C                   MOVEL     VIDDSA        PARDUT           30
080080920319     C                   MOVEL     *BLANKS       DESCDS           48
080090920319     C                   MOVEL     DESKSC        DESCDS
080100920319     C* PARSTA=9 ESCLUDO ANNULLATI
080110920319     C                   Z-ADD     9             PARSTA
080120060131     C                   Z-ADD     dutkci        CCC               4 0
080130941103    3C     VIDKLP        IFEQ      0
080140000510     C   10              MOVEL     ARBLNA        PARKSM
080150941103     C                   ELSE
080160000510     C                   MOVEL     VIDKLP        PARKSM
080170941103    3C                   ENDIF
080180991112     C                   CLEAR                   PARDIT
080190000510     C                   Z-ADD     1             PARNUM
080200941103     C*
080210000510     C                   CALL      'XALFA3BR'
080220920319     C                   PARM                    PARDUT
080230920319     C                   PARM                    CODUT
080240920319     C                   PARM                    DESCDS
080250920319     C                   PARM                    CCC
080260920319     C                   PARM                    PARSTA            1 0
080270961016     C                   PARM                    PARFLR           90
080280991112     C                   PARM                    PARDIT            3
080290000510     C                   PARM                    PARNUM            2 0
080300000510     C                   PARM                    PARKCM           80
080310000510     C                   PARM                    PARKSM          140
080320000510     C                   PARM                    PARKDM           60
080330941103    3C     PARSTA        IFEQ      -1
080340920319     C*
080350941103    4C     FLGCVR        IFEQ      '2'
080360941212     C*
080370990930     C     WAR62         IFEQ      ' '
080380941212     C                   MOVEL     ARBLNA        VIDKLP
080390941212     C                   MOVEL     9999          VIDKSC
080400941212     C                   ELSE
080410991007     C                   MOVEL     AR6KSC        VIDKLP
080420991007     C                   MOVE      AR6KSC        VIDKSC
080430941212     C                   ENDIF
080440941212     C*
080450941103   X4C                   ELSE
080460920319     C                   MOVEL     ARBKSC        VIDKLP
080470920319     C                   MOVE      ARBKSC        VIDKSC
080480941103    4C                   END
080490920319     C*
080500941103   X3C                   ELSE
080510020305     C                   MOVEL     PARKSM        ACOKSC
080520920319     C                   MOVEL     ACOKSC        VIDKLP
080530920319     C                   MOVE      ACOKSC        VIDKSC
080540970814     C                   CLEAR                   VIDRSM
080550970814     C                   CLEAR                   VIDINM
080560970814     C                   CLEAR                   VIDCAM
080570970814     C                   CLEAR                   VIDLOM
080580970814     C                   CLEAR                   VIDPRM
080590970814     C                   CLEAR                   VIDNZM
080600970814     C                   CLEAR                   VIDPIM
080610941103    3C                   END
080620941103     C*
080630941103     C                   SETON                                        9047
080640941103     C                   GOTO      ENDCOM
080650941212    2C                   ENDDO
080660920319     C* CONTROLLO
080670941103    2C     VIDKLP        IFEQ      0
080680941103     C                   MOVEL     MSG(33)       VIDMSG
080690941103     C                   SETON                                        469028
080700941103     C                   GOTO      ENDCOM
080710941103    2C                   END
080720980722     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE DELLO STESSO S.I. IN
080730991112     C*  CUI SONO  PER LE FATTURE IMMEDIATE
080740040930     C     VIDKLP        CHAIN     AZORG01L                           30
080750040930     c*
080760020725     C     D66BFI        IFEQ      'S'
080770020725     C**N30ORGDIT        IFNE      O50PRA
080780020725     C**                 MOVEL     MSG(81)       VIDMSG
080790020725     C**                 SETON                                        469028
080800020725     C**                 GOTO      ENDCOM
080810020725     C**                 ENDIF
080820020508     c* sempre per le fatture immediate se cliente 9999 il p.o. cliente
080830020508     c* deve essere = alla linea di arrivo della bolla
080840020508     c     vidksc        ifeq      9999
080850020508     c     vidklp        andne     arblna
080860020508     C                   MOVEL     MSG(35)       VIDMSG
080870020508     C                   SETON                                        479028
080880020508     C                   GOTO      ENDCOM
080890020508     c                   endif
080900020306     C                   ENDIF
080910020306     c* se variazione di mittente e cliente 8888 il p.o. cliente deve
080920020306     c* essere = alla linea di partenza bolla
080930020306     c     flgcvr        ifeq      'M'
080940020306     c     vidksc        andeq     8888
080950020306     c     vidklp        andne     arblnp
080960020306     C                   MOVEL     MSG(35)       VIDMSG
080970020306     C                   SETON                                        479028
080980020306     C                   GOTO      ENDCOM
080990020306     c                   endif
081000020508     c* se variazione di bolla in partenza e cliente 9999 errore
081010020508     c     *in10         ifeq      *off
081020020508     c     vidksc        andeq     9999
081030020508     C                   MOVEL     MSG(111)      VIDMSG
081040020508     C                   SETON                                        479028
081050020508     C                   GOTO      ENDCOM
081060020508     c                   endif
081070930507     C*
081080920319     C                   MOVEL     VIDKLP        KSC
081090920319     C                   MOVE      VIDKSC        KSC
081100000510     C  N30              MOVEL     ORGDE3        OG143
081110020318     C  N30              MOVEL     §OGNTW        CLINTW
081120020318     C   30              CLEAR                   CLINTW
081130040930     c* non accetto clienti con ntw = "LOG" o "XXX'
081140040930     c                   if        clintw='LOG'  or clintw='XXX'
081150040930     C                   MOVEL     MSG(130)      VIDMSG
081160040930     C                   SETON                                        469028
081170040930     C                   GOTO      ENDCOM
081180040930     c                   endif
081190051220     c* non è possibile modificare cliente da dpd a non dpd e viceversa
081200051220     c     flgcvr        ifeq      'M'
081210051220     c                   if        (clintw = 'DPD' and clintw <> memntw) or
081220051220     c                             (memntw = 'DPD' and clintw <> memntw)
081230051220     C                   MOVEL     MSG(137)      VIDMSG
081240051220     C                   SETON                                        469028
081250051220     C                   GOTO      ENDCOM
081260051220     c                   endif
081270051220     c                   endif
081280941103     C**
081290920319     C*
081300941103     C     KACO          CHAIN     CNACO000                           30
081310941103    3C  N30ACOFLG        IFNE      ' '
081320920319     C                   SETON                                        30
081330941103    3C                   END
081340941103     C* ERRORE
081350941103    3C     *IN30         IFEQ      *ON
081360941103     C     VIDKSC        OREQ      8888
081370941222     C     *IN10         ANDEQ     *ON
081380941223     C     VIDKSC        OREQ      9999
081390941223     C     *IN36         ANDEQ     *ON
081400941103     C                   MOVEL     MSG(36)       VIDMSG
081410941103     C                   SETON                                        479028
081420941103     C                   GOTO      ENDCOM
081430941103    3C                   ENDIF
081440930507     C*
081450130315    3C**   ACOABL        IFEQ      '8'
081460130315    3C**   ACOABL        OREQ      '*'
081470130315     c
081480130315    3C     ACOABL        ifne      *blanks
081490130418     c     ksc           andne     kscbolla
081500040930     C                   MOVEL     mSG(37)       VIDMSG
081510941103     C                   SETON                                        479028
081520941103     C                   GOTO      ENDCOM
081530941103    3C                   END
081540930507     C*
081550920319     C* SE SEGUE FATTURA NON SI PUO' METTERE COD 9999
081560941103    3C   13VIDKSC        IFEQ      9999
081570941103     C                   MOVEL     MSG(38)       VIDMSG
081580941103     C                   SETON                                        479028
081590941103     C                   GOTO      ENDCOM
081600941103    3C                   END
081610930507     C*
081620970822     C     VIDKSC        IFNE      9999
081630970822     C     VIDKSC        ANDNE     8888
081640970822     C                   MOVEL     ACORAG        DESKSC
081650941103    2C                   END
081660941220     C*
081670941223     C* SALVO ULTIMO CODICE IMPOSTATO
081680941220    2C     COMKSC        IFNE      KSC
081690941220     C                   CLEAR                   CIN
081700941220     C                   CLEAR                   DIE
081710941220     C                   MOVEL     KSC           COMKSC            7 0
081720941220    2C                   END
081730941103    1C                   END
081740030620     C* CERCO IN CNIND IL DIRITTO DI FATTURAZIONE
081750030620     C*
081760030620     C     KACO          CHAIN     CNIND00F                           30
081770030620    3C                   IF        %FOUND(CNIND00F)
081780030620     C                   EVAL      SAV_INDSIN = INDSIN
081790030620     C                   ELSE
081800030620     C                   CLEAR                   SAV_INDSIN
081810030620     C                   ENDIF
081820941103     C**
081830941215     C* CARICO CODICI TARIFFA DEL CLIENTE ED ESEGUO CONTROLLO
081840941103     C**
081850930507     C                   EXSR      CARCTR
081860941209     C*
081870941209     C* SE HO RICERCATO LA TARIFFA, RIEMETTO LA VIDEATA
081880050202    1C     ilv59RIC      IFEQ      'S'
081890941223     C                   SETON                                        9048
081900941209     C                   GOTO      ENDCOM
081910941209    1C                   ENDIF
081920941209     C*
081930941215     C* ATTIVATO IL "?" SUL CODICE TARIFFA
081940050202    1C     olv59ERR      IFEQ      'R'
081950050202     C                   MOVEL     olv59CTR      VIDCTR
081960050202     C                   MOVEL     olv59DFS      DE2CTR
081970941223     C                   SETON                                        9048
081980941209     C                   GOTO      ENDCOM
081990941209    1C                   ENDIF
082000941209     C* ERRORE
082010050202    1C     olv59MSG      IFNE      *BLANKS
082020060606     c* Nella videata della variazione del cod mittente, non do errore
082030060606     c*  se premuto F10 per la modifica del codice tariffa
082040060606     c*  ma disabilito F12
082050060606     c                   if        *inkj and flgcvr='M'
082060060606     c                   clear                   w48f12
082070060606     c                   else
082080050202     C                   MOVEL     olv59MSG      VIDMSG
082090941223     C                   SETON                                        904828
082100941209     C                   GOTO      ENDCOM
082110941209    1C                   ENDIF
082120060606    1C                   ENDIF
082130930507     C*
082140941209     C                   MOVEL     VIDCTR        COMCTR
082150050202     C                   MOVEL     olv59PRG      COMPRG
082160021121
082170160225      * Per bolla FedEx
082180160225If  1c                   If        lnantW = 'FED'
082190160225     c
082200021122      * --> se cliente 8888
082210160225      *     il CTR a video deve essere uno delle tariffe fedex
082220021122If  2c                   If        VidKsc = 8888
082230021122      * ---> e O27Fie = 'M'
082240021122If  3c                   If        WFie =  'M'
082250021122      * la tariffa deve essere quella della cartello
082260160225If  4c                   If        VidCtr <> FedM_ctr
082270021122     c                   Eval      *In48 = *On
082280021122     c                   Eval      *In28 = *On
082290021122     c                   Eval      *In90 = *On
082300021122     c                   Movel     Msg(114)      VidMsg
082310021122     c                   Goto      EndCom
082320021122    4c                   EndIf
082330021122      * ---> e O27Fie non è = 'M'
082340021122   x3c                   Else
082350160225    4c                   if        Vidctr <> FedD_ctr
082360021121     c                   Eval      *In48 = *On
082370021121     c                   Eval      *In90 = *On
082380021121     c                   Eval      *In28 = *On
082390021121     c                   Movel     Msg(114)      VidMsg
082400021121     c                   Goto      EndCom
082410021122    4c                   EndIf
082420021122    3c                   EndIf
082430160225     c
082440021121      * --> se cliente codificato
082450021121   x2c                   Else
082460021121      * ---> e non sono state trovate tariffe
082470021121If  3c                   If        Wtks <> 'S'
082480021121      *      non posso accettare una tariffa documenti se il peso della
082490021121      *      bolla superare il limite previsto in tabella "DFE"
082500021121     c                   Move      Vidctr        DueCtr
082510021121If  4c                   If        ArbPkf > §DfePkgD and
082520021121     c                             DueCtr >= §DfeDalD and DueCtr <= §DfeAlD
082530021121     c                   Eval      *In48 = *On
082540021121     c                   Eval      *In90 = *On
082550021121     c                   Eval      *In28 = *On
082560021121      * Preparo il msg
082570021121     c                   Movea     Msg(115)      K78
082580021121     c                   Movel     §DfePkgD      W006a
082590021121     c                   Movea     W006a         Ski(10)
082600021121     c                   Z-Add     10            i
082610021121     c                   ExSr      Abbl
082620021121     c                   Movea     Ski(10)       K78(59)
082630021121     c                   Move      §DfePkgD      W001a
082640021121     c                   Movel     W001a         K78(66)
082650021121     c                   Movea     K78           VidMsg
082660021121     c                   Goto      EndCom
082670021121    4c                   EndIf
082680021121    3c                   EndIf
082690021121    2c                   EndIf
082700021121    1c                   EndIf
082710920319     C*
082720941103     C     ENDCOM        ENDSR
082730011017     C*
082740011017      *--- TASSAZIONE VARIA 'N' --------------------------------------*
082750011017     C     TASSAN        BEGSR
082760011017      *
082770011017     C                   CLEAR                   DTAS
082780011017     C                   CLEAR                   DTASV
082790011017     C                   MOVEL     VIDKLP        TASKSC
082800011017     C                   MOVE      VIDKSC        TASKSC
082810011017     C                   MOVEL     VIDCTR        TASCTR
082820011017     C                   Z-ADD     ARBPKF        TASPKF
082830160530     C                   Z-ADD     ARBPKb        TASPKb
082840011017     C                   MOVEL     ARBLNP        TASLNP
082850011017     C                   MOVEL     ARBCTS        TASCTS
082860011017     C                   MOVEL     ARBDSP        TASDSP
082870011017     C                   MOVEL     ARBDSP        TASDCT
082880011017     C                   MOVEL     ARBVLF        TASVLF
082890160530     C                   MOVEL     ARBVLc        TASVLc
082900160530     C                   MOVEL     ARBncr        TASncr
082910160530     C                   MOVEL     ARBVLF        TASVLF
082920160530     C                   MOVEL     ARBfvf        TASfvF
082930011017     C                   MOVEL     ARBTSP        TASTSP
082940011017     C     WBOL          IFEQ      '1'
082950011017     C                   MOVEL     §3ATB1        TASTBL
082960011017     C                   ELSE
082970011017     C                   MOVEL     §3ATB2        TASTBL
082980011017     C                   ENDIF
082990011017     C                   MOVEL     ARBLNA        TASLNA
083000011017     C                   MOVEL     ARBNCL        TASNCL
083010160530     C                   MOVEL     ARBNCL        TASNCLb
083020011017     C                   MOVEL     ARBQFT        TASQFT
083030011017     C     WBOL          IFEQ      '1'
083040011017     C     §3AFCA        ANDEQ     1
083050011017     C     WBOL          OREQ      '2'
083060011017     C     §3AFCA        ANDEQ     2
083070011017     C                   MOVEL     AR9CAS        TASCAS
083080011017     C                   MOVEL     AR9VCA        TASVCA
083090011017     C                   MOVEL     AR9TIC        TASTIC
083100011017     C                   ENDIF
083110011017     C                   MOVEL     ARBIAS        TASIAS
083120011017     C                   MOVEL     ARBVAS        TASVAS
083130011017     C                   MOVEL     ARBCAD        TASCAD
083140011017     C                   MOVEL     ARBNZD        TASNZD
083150011017     C                   MOVEL     ARBFIN        TASFIN
083160011017     C                   MOVEL     ARBCAM        TASCAM
083170011017     C                   MOVEL     ARBNZM        TASNZM
083180011017     C                   MOVEL     ARBFAP        TASFAP
083190011017     C                   MOVEL     ARBTC1        TASTC1
083200011017     C                   MOVEL     ARBTC2        TASTC2
083210011017     C     V1CDDT        IFEQ      'C'                                          SI DDT
083220011017     C     V1CDDT        OREQ      'S'
083230031028     c     v1cddt        oreq      'P'
083240031028     c     v1cddt        oreq      'Q'
083250011017     C                   MOVEL     'S'           TASDDT
083260011017     C                   ENDIF
083270060925     c*****              movel     arbfbr        tasflo
083280060925     c                   clear                   dtas01
083290060925     c                   movel     arbfbr        §asfbr
083300060925     c                   movel     arbcca        §ascca
083310060925     c                   eval      tasflo = dtas01
083320011017      **
083330011017      * SE BOLLA EXPORT--> NON PASSO MAI IL FLAG CONSEGNA A MAGAZZINO
083340011017      *                    IN MODO CHE TASSI SEMPRE LA VARIA 'U'
083350020305     C     LNANTW        ifeq      'COR'
083360020305     c     LNANTW        oreq      'MES'
083370020305     c     LNANTW        oreq      'PPT'
083380011017     C                   MOVEL     ARBFDN        TASFDN
083390011017     C                   ENDIF
083400011017      **
083410011017      * CHAIN SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
083420030612     C                   CLEAR                   Tisi95DS
083430011017     C                   MOVEL     ARBLOM        I95LOC
083440011017     C                   MOVEL     ARBCAM        I95CAP
083450011017     C                   MOVEL     ARBNZM        I95NAR
083460011017     C                   MOVEL     '3'           I95TCN
083470011017     C                   MOVEL     ARBDSP        I95DAT
083480011017     C     §3AFCA        IFNE      0
083490011017     C                   MOVEL     'S'           I95FCA
083500011017     C                   ENDIF
083510011017     C                   MOVEL     §3ATB1        I95TPO
083520020305     c     lnpntw        ifeq      'FED'
083530020305     c                   movel     'S'           i95fi1
083540020305     c                   endif
083550011017     C                   CALL      'TISI95R'
083560030612     C                   PARM                    Tisi95DS
083570011017     C     O95ERR        IFEQ      *BLANKS
083580011017     C                   MOVEL     O95CTS        TASMCT
083590011017     C                   ENDIF
083600011017     C                   MOVEL     'S'           WCAL13            1
083610011017      * ELABORO E CHIUDO CON LR IL PGM
083620011017     C                   MOVEL     ' '           TASTLA
083630020930     c* imposto peso vdl e colli rilevati
083640030203     c**!!!              clear                   dtas01
083650030203     c**!!!              z-add     arbncp        §asncp
083660030203     c**!!!              z-add     arbpkc        §aspkc
083670030203     c**!!!              movel     dtas01        tasflo
083680030203     c                   z-add     arbncp        Tasncp
083690030203     c                   z-add     arbpkc        Taspkc
083700030204      * Bancali
083710030205     c                   If        %Subst(ArbGva:1:1) = 'B'
083720030729     c                   Eval      TasBan = V1cBan
083730030205     c                   EndIf
083740030729      * Colli Originali
083750030729     c                   If        %Subst(ArbGva:1:1) = 'O'
083760030729     c                   Eval      TasNcl = V1cCor
083770030729     c                   EndIf
083780050804     c
083790050804     c                   eval      parca  = ' '
083800050804     c                   movel     paramt        kpjbu
083810011017      *
083820011017     C                   CALL      'TNSF20R'
083830011017     C                   PARM                    KPJBA
083840011017     C                   PARM                    DTAS
083850011017     C                   PARM                    DTASV
083860011018      *
083870011018      * Tassazione andata buon fine
083880011018     C     TASERR        IFEQ      *BLANKS
083890011018      *
083900011018      * Controllo la divisa della tassazione con la divisa del video
083910011018      * se non corrispondono devo convertire la varia 'N' nella divisa
083920011018      * del video
083930011018     C     TASDIV        IFNE      VIDDIV
083940011018      * aggancio la tabella CV per sicurezza
083950011018     C                   CLEAR                   DSCV
083960011018     C                   MOVEL     'CV'          COD
083970011018     C                   MOVEL(P)  VIDDIV        KEY
083980011018     C                   EXSR      CTABEL
083990011018     C  N30              MOVEL     TBLUNI        DSCV
084000011018      *
084010030612     C                   CLEAR                   YeurcoDS
084020011018     C                   MOVEL     TASDIV        YECDVI
084030011018     C                   MOVEL     VIDDIV        YECDVO
084040011018     C                   Z-ADD     TASIMV        YECIMI
084050011018     C     §CVFDC        IFEQ      'S'
084060011018     C                   MOVE      '3'           YECDCO
084070011018     C                   ELSE
084080011018     C                   MOVE      '0'           YECDCO
084090011018     C                   ENDIF
084100011018     C                   CALL      'YEURCO'
084110030612     C                   PARM                    YeurcoDS
084120011018     C     YECESI        IFEQ      ' '
084130011018     C                   Z-ADD     YECIMO        TASIMV
084140011018     C                   ELSE
084150011018     C                   Z-ADD     88888888      TASIMV
084160011018     C                   ENDIF
084170011018     C                   ENDIF
084180011017      *
084190011017     C                   Z-ADD     TASIMV        V3CVA1
084200011017     C                   Z-ADD     TASIMV        VARVA1
084210011017     C                   ENDIF
084220011017      *
084230011017     C                   ENDSR
084240910429     C*
084250050531begsrC*--- TASSAZIONE BOLLA ------------------------------------------*
084260910429     C     TASSAZ        BEGSR
084270991005     C                   CLEAR                   DTAS
084280991005     C                   CLEAR                   DTASV
084290020503     C                   CLEAR                   wtassa            1
084300030620     C                   CLEAR                   wtassad           1
084310030620     C                   CLEAR                   itassa           11 3
084320050601     c*
084330050601     c* richiamo routine di impostazione campi "comuni" da passare a
084340050601     c* pgm tnsf20r
084350050601     c                   exsr      SR_rtassa
084360910429     C*
084370941221     C* CARICO LE VARIE
084380941221     C*
084390980513     C     WBOL          IFEQ      '1'
084400030709     c                   eval      flgsf20 = 'S'
084410991012     C                   EXSR      RDTASV
084420030709     c                   eval      flgsf20 = ' '
084430980513     C*
084440980513     C                   ELSE
084450040123      * carico varie seconda bolla
084460040123     C                   MOVEL     'G'           TASSVA
084470040123     c                   Eval      TascVag = 'S'
084480041202     C**!!!VI2VA1        IFGT      0
084490040123     C                   MOVEL     VI2SV1        VSV(1)
084500040123     C                   Z-ADD     VI2VA1        VVA(1)
084510040123     c                   Movel     Vi2Es1        ves(1)
084520041202     C**!!!              ENDIF
084530041202     c**!!!              If        Vi2Va2 > *Zeros
084540040123     c                   Movel     Vi2Sv2        Vsv(2)
084550040123     c                   Z-add     Vi2Va2        Vva(2)
084560040123     c                   Movel     Vi2Es2        ves(2)
084570041202     c**!!!              EndIf
084580041202     c**!!!              If        Vi2Va3 > *Zeros
084590040123     c                   Movel     Vi2Sv3        Vsv(3)
084600040123     c                   Z-add     Vi2Va3        Vva(3)
084610040123     c                   Movel     Vi2Es3        ves(3)
084620041202     c**!!!              EndIf
084630041202     c**!!!              If        Vi2Va4 > *Zeros
084640040123     c                   Movel     Vi2Sv4        Vsv(4)
084650040123     c                   Z-add     Vi2Va4        Vva(4)
084660040123     c                   Movel     Vi2Es4        ves(4)
084670041202     c**!!               EndIf
084680041202     c**!!!              If        Vi2Va5 > *Zeros
084690041130     c                   Movel     Vi2Sv5        Vsv(5)
084700041130     c                   Z-add     Vi2Va5        Vva(5)
084710041130     c                   Movel     Vi2Es5        ves(5)
084720041202     c**!!!              EndIf
084730041202     c**!!!              If        Vi2Va6 > *Zeros
084740041130     c                   Movel     Vi2Sv6        Vsv(6)
084750041130     c                   Z-add     Vi2Va6        Vva(6)
084760041130     c                   Movel     Vi2Es6        ves(6)
084770041202     c**!!!              EndIf
084780040126     c                   xfoot     vva           TotImv
084790040126     c                   Z-add     TotImv        TasImv
084800980608     C                   ENDIF
084810011211
084820011211      * Data fattura se c'è
084830030203     c**!!!              clear                   dtas01
084840020930     c**                 if        ar6dft <> 0
084850020930     c**                 movel     ar6dft        §asdat
084860020930     c**                 endif
084870020503     c**
084880020503     c                   if        wtassa=' '
084890020503     c* se devo tassare solo una varia, da tipo bolla, la imposto
084900020503     c                   if        §3asva<>*blanks
084910020503     c                   eval      tassva=§3asva
084920020503     c                   endif
084930030620     c                   endif
084940030925     c* se diritto di fatturazione uguale a zero non calcolo la varia D
084950030925     c                   if        sav_indsin = 0
084960030925     c                   eval      wtassad = 'N'
084970030925     c                   endif
084980030623      * se tassazione fattura immediata o ritassazione prepagato
084990030623      * passo al TNSF20R la varia "D" e l'importo del diritto
085000030620      * di fatturazione da sommare che prendo da cnind (INDSIN)
085010030623     c                   if        (*in14 and not *in29  and wtassad = ' ')  or
085020030623     c                             (not *in10 and vidnft > 0 and not *in29 and
085030030623     c                             wtassad = ' ')
085040030925      *
085050030620     c                   move      sav_indsin    tasivdf
085060030620     c                   eval      tassvdf = 'D'
085070030620      * se bolla già tassat devo sommare la sola varia "D"
085080030620      * imponibile > 0 oppure imponibile a zero ma singola varia vaolorizzata x bolle speciali
085090040123     c**!!!              if        tasimv > 0 or (tasimv = 0 and itassa > 0)
085100040123     c                   if        tasimv > 0
085110030620     c                   eval      tassva = 'D'
085120030620     c                   endif
085130030620      * se singola varia per bolla speciale non è da calcolare ed è uguale a zero non calcolo la
085140030620      * varia D
085150030620     c                   if        wtassa = 'N' and tasimv = 0 and itassa = 0
085160030620     c                   eval      wtassad = 'N'
085170030620     c                   endif
085180030620
085190030620     c                   endif
085200011211
085210030620      * se non è tassazione fattura immediata non devo calcolare la varia D
085220030623      * o ritassazione prepagato
085230030623     c*                  if        not *in14
085240030623     c                   if        not *in14  or  *in29 or
085250030623     c                             (not *in10 and vidnft = 0)
085260030620     c                   eval      wtassad = 'N'
085270030620     c                   endif
085280030620      * se non devo calcolare la singola varia e neppure la varia D salto la tassazione
085290030620     c                   if        wtassa = 'N' and wtassad = 'N'
085300030620     c                   goto      endtas
085310030620     c                   endif
085320151127     c* Packing list e orm telefonici solo per la prima bolla in quanto la seconda
085330151127     c* è solo per tassare una specifica varia
085340151126     C     WBOL          IFEQ      '1'
085350151126     c* PACKING LIST
085360151126     c                   if        par5als='S' or par5alx='S'
085370151126     c                   eval      tasspl='S'
085380151126     c                   endif
085390151126     c* ADDEBITO ORM TELEFONICI (diritto di chiamata)
085400151126     C                   MOVEL     'M'           WTRC
085410151126     C     KARB2         CHAIN(N)  FiAR401l
085420151126     c                   if        %found(fiar401l)
085430151126     c                   movel     ar4not        dsbl4m
085440151126     c                   if        §b4ado='S'
085450151126     c                   eval      tasprt='S'
085460151126     c                   endif
085470151126     c                   endif
085480151126
085490151126     c                   endif
085500050804     c
085510050804     c                   eval      parca  = ' '
085520050804     c                   movel     paramt        kpjbu
085530910429     C*
085540941209     C                   CALL      'TNSF20R'
085550910429     C                   PARM                    KPJBA
085560991005     C                   PARM                    DTAS
085570991005     C                   PARM                    DTASV
085580910429     C*
085590941212    1C     TASERR        IFNE      *BLANKS
085600980608     C     TASMSG        IFNE      *BLANKS
085610980608     C                   MOVEL(P)  TASMSG        VIDMSG
085620980608     C                   ELSE
085630941212     C                   MOVEL     MSG(55)       VIDMSG
085640980608     C                   ENDIF
085650941209     C                   SETON                                        499028
085660991006     C* SE ERRORI IN TASSAZIONE SPROTEGGO LA DIVISA
085670991006     C                   SETOFF                                       07
085680941212   X1C                   ELSE
085690980513     C     WBOL          IFEQ      '1'
085700950421     C                   MOVEL     TASQFT        VIDQFT
085710950424     C                   MOVEL     TASQFT        VARQFT
085720910429     C                   MOVEL     TASPOR        VIDPOR
085730991006     C                   MOVEL     TASPOR        VARPOR
085740991025     C* SE DIVISA DIVERSA DA PRECEDENTE E PREMUTO F6 ACCENDO IL 90
085750991025     C* PER RIEMETTERE LA VIDEATA IN MODO DA VISUALIZZARE LA NUOVA
085760991025     C* DIVISA
085770991025     C     VIDDIV        IFNE      TASDIV
085780991025     C     *INKF         ANDEQ     *ON
085790991025     C                   SETON                                        90
085800991025     C                   ENDIF
085810991025     C*
085820991005     C                   MOVEL     TASDIV        VIDDIV
085830991006     C                   MOVEL     TASDIV        WDIV
085840991005     C                   Z-ADD     TASIMV        VIDIMV
085850941209     C* CARICO LE VARIE
085860941212    2C                   DO        20            NRR
085870941209     C     NRR           CHAIN     LR48S03                            30
085880941209     C                   MOVEL     VSV(NRR)      V3CSV1
085890941209     C                   Z-ADD     VVA(NRR)      V3CVA1
085900991006     C                   Z-ADD     VVA(NRR)      VARVA1
085910941209     C                   MOVEL     VES(NRR)      V3CEV1
085920941209     C   30              WRITE     LR48S03
085930941209     C  N30              UPDATE    LR48S03
085940941212    2C                   ENDDO
085950941209     C*
085960980513    XC                   ELSE
085970040123     c                   Clear                   ContaSv
085980040123      * carico le varie seconda bolla
085990040123Do  2c                   Do        20            c
086000040123If  3c                   If        Vsv(c) <> §$2Iva and
086010040123     c                             Vsv(c) <> §$2Bol
086020040123     c                   Add       1             ContaSv
086030040123S   4c                   Select
086040040123     c                   When      ContaSv = 1
086050040123     c                   Movel     Vsv(c)        Vi2Sv1
086060040123     c                   Z-add     Vva(c)        Vi2Va1
086070040123     c                   Movel     Ves(c)        Vi2Es1
086080040123     c                   When      ContaSv = 2
086090040123     c                   Movel     Vsv(c)        Vi2Sv2
086100040123     c                   Z-add     Vva(c)        Vi2Va2
086110040123     c                   Movel     Ves(c)        Vi2Es2
086120040123     c                   When      ContaSv = 3
086130040123     c                   Movel     Vsv(c)        Vi2Sv3
086140040123     c                   Z-add     Vva(c)        Vi2Va3
086150040123     c                   Movel     Ves(c)        Vi2Es3
086160040123     c                   When      ContaSv = 4
086170040123     c                   Movel     Vsv(c)        Vi2Sv4
086180040123     c                   Z-add     Vva(c)        Vi2Va4
086190040123     c                   Movel     Ves(c)        Vi2Es4
086200041130     c                   When      ContaSv = 5
086210041130     c                   Movel     Vsv(c)        Vi2Sv5
086220041130     c                   Z-add     Vva(c)        Vi2Va5
086230041130     c                   Movel     Ves(c)        Vi2Es5
086240041130     c                   When      ContaSv = 6
086250041130     c                   Movel     Vsv(c)        Vi2Sv6
086260041130     c                   Z-add     Vva(c)        Vi2Va6
086270041130     c                   Movel     Ves(c)        Vi2Es6
086280040123    4c                   EndSl
086290040123    3c                   EndIf
086300040123    2c                   EndDo
086310040123     C**!!!              Z-ADD     1             C
086320040123     C**!!!TASSVA        LOOKUP    VSV(C)                                 33
086330040123     C*!!33              Z-ADD     VVA(C)        VI2VA1
086340040123     C*!!33              Z-ADD     VVA(C)        VA2VA1
086350040123     C*!!33              MOVEL     VSV(C)        VI2SV1
086360991025     C* SE DIVISA DIVERSA DA PRECEDENTE E PREMUTO F6 ACCENDO IL 90
086370991025     C* PER RIEMETTERE LA VIDEATA IN MODO DA VISUALIZZARE LA NUOVA
086380991025     C* DIVISA
086390040123     C*!!33VI2DIV        IFNE      TASDIV
086400040123     C     VI2DIV        IFNE      TASDIV
086410991025     C     *INKF         ANDEQ     *ON
086420991025     C                   SETON                                        90
086430991025     C                   ENDIF
086440040123     C*!!33              MOVEL     TASDIV        VI2DIV
086450040123     C*!!33              MOVEL     TASDIV        WDIV
086460040123     C                   MOVEL     TASDIV        VI2DIV
086470040123     C                   MOVEL     TASDIV        WDIV
086480040123     C*!!33VES(C)        IFNE      ' '
086490040123     C**!!!VI2CEI        ANDEQ     ' '
086500040123     C**!!!              MOVEL     VES(C)        VI2CEI
086510040123     C**!!!              ENDIF
086520980513     C                   ENDIF
086530020503     C                   ENDIF
086540980513     C*
086550030620    1C*****              ENDIF
086560941209     C*
086570911211     C     ENDTAS        ENDSR
086580050601     C*
086590050601     C*--- Riempimento campi comuni di passaggio a tnsf20r -----------*
086600050601     C     sr_rtassa     BEGSR
086610050601     C* CODIFICATO
086620050601    1C     VIDKSC        IFNE      9999
086630050601    1C     VIDKSC        ANDNE     8888
086640050601     C                   MOVEL     VIDKLP        TASKSC
086650050601     C                   MOVE      VIDKSC        TASKSC
086660050601     C                   MOVEL     VIDCTR        TASCTR
086670050601   X1C                   ELSE
086680050601     C*
086690050601     C* E HO LA DATA FATTURA USO QUELLA, ALTRIMENTI DATEU
086700050601     C     AR6DFT        IFGT      0
086710050601     C                   Z-ADD     AR6DFT        WDATA
086720050601     C                   ELSE
086730050601     C                   Z-ADD     DATEU         WDATA
086740050601     C                   ENDIF
086750050601     C*
086760050601     C* DAL TIPO SERVIZIO PRENDO LA TARIFFA CON CUI TASSARE
086770050601     C*
086780160225     c**                 eval      tasksc  = o27ksc
086790160225     c**                 eval      tasctr  = o27ctr
086800160225     C                   MOVEL     VIDKLP        TASKSC
086810160225     C                   MOVE      VIDKSC        TASKSC
086820160225     C                   MOVEL     VIDCTR        TASCTR
086830050601     C**
086840050601    1C                   ENDIF
086850050601     C*
086860050601     C                   Z-ADD     ARBPKF        TASPKF
086870160530     C                   Z-ADD     ARBPKb        TASPKb
086880050601     C                   MOVEL     ARBLNP        TASLNP
086890050601     C                   MOVEL     ARBCTS        TASCTS
086900050601     C                   MOVEL     ARBDSP        TASDSP
086910050601     C                   MOVEL     ARBDSP        TASDCT
086920050601     C                   MOVEL     ARBVLF        TASVLF
086930160530     C                   MOVEL     ARBfvF        TASfvf
086940160530     c                   z-add     arbvlc        tasvlc
086950160530     c                   z-add     arbncr        tasncr
086960050601     C                   MOVEL     ARBTSP        TASTSP
086970050601     C     WBOL          IFEQ      '1'
086980050601     C                   MOVEL     §3ATB1        TASTBL
086990050601     C                   MOVEL     VIDDIV        TASDIV
087000050601     C                   Z-ADD     VIDIMV        TASIMV
087010050601     C                   ELSE
087020050601     C                   MOVEL     §3ATB2        TASTBL
087030050601     C                   MOVEL     VI2DIV        TASDIV
087040050601     C**!!!              Z-ADD     VI2VA1        TASIMV
087050050601     C                   END
087060050601     C                   MOVEL     ARBLNA        TASLNA
087070050601     C                   MOVEL     ARBNCL        TASNCL
087080160530     C                   MOVEL     ARBNCL        TASNCLB
087090050601     C                   MOVEL     VIDPOR        TASPOR
087100050601     C                   MOVEL     VIDQFT        TASQFT
087110050601     C     WBOL          IFEQ      '1'
087120050601     C     §3AFCA        ANDEQ     1
087130050601     C     WBOL          OREQ      '2'
087140050601     C     §3AFCA        ANDEQ     2
087150050601     C                   MOVEL     AR9CAS        TASCAS
087160050601     C                   MOVEL     AR9VCA        TASVCA
087170050601     C                   MOVEL     AR9TIC        TASTIC
087180050601     C                   ENDIF
087190050601     C                   MOVEL     VIDIAS        TASIAS
087200050601     C                   MOVEL     VIDVAS        TASVAS
087210050601     C                   MOVEL     ARBCAD        TASCAD
087220050601     C                   MOVEL     ARBNZD        TASNZD
087230050601     C                   MOVEL     ARBCAM        TASCAM
087240050601     C                   MOVEL     ARBNZM        TASNZM
087250050601     C                   MOVEL     ARBFIN        TASFIN
087260050601     C                   MOVEL     ARBFAP        TASFAP
087270050601     C                   MOVEL     ARBTC1        TASTC1
087280050601     C                   MOVEL     ARBTC2        TASTC2
087290050601     C     V1CDDT        IFEQ      'S'
087300050601     C     V1CDDT        OREQ      'C'
087310050601     c     v1cddt        oreq      'P'
087320050601     c     v1cddt        oreq      'Q'
087330050601     C                   MOVEL     'S'           TASDDT
087340050601     C                   ENDIF
087350060925     c****               movel     arbfbr        tasflo
087360060925     c                   clear                   dtas01
087370060925     c                   movel     arbfbr        §asfbr
087380060925     c                   movel     arbcca        §ascca
087390110630     c                   movel     wemd          §asemd
087400150108     c* Pin Code
087410150108     c                   if        arbgma<>*blanks
087420150108     c                   Movel     '7R'          cod
087430150108     c                   Movel(p)  ArbGMa        key
087440150108     c                   ExSr      ctabel
087450150108     c  n30              Movel     TblUni        ds7R
087460150108     c   30              Clear                   ds7R
087470150108     c                   if        §7rpincode='S'
087480150108     c                   movel     'S'           §aspin
087490150108     c                   endif
087500150108     c                   endif
087510060925     c                   eval      tasflo = dtas01
087520050601     C* SE BOLLA ARRIVI IN ASSEGNATO EXPORT, NON PASSO LA CONSEGNA A
087530050601     C*  MAGAZZINO PER FAR CALCOLARE SEMPRE LA VARIA "U"
087540050601     C     *IN10         IFEQ      *OFF
087550050601     C     *IN06         OREQ      *ON
087560050601     C     lnantw        oreq      'COR'
087570050601     c     lnantw        oreq      'MES'
087580050601     c     lnantw        oreq      'PPT'
087590050601     C                   MOVEL     ARBFDN        TASFDN
087600050601     C                   ENDIF
087610050601     C**
087620050601     C* CHAI SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
087630050601     C                   CLEAR                   Tisi95DS
087640050601     C                   MOVEL     ARBCAM        I95CAP
087650050601     C                   MOVEL     ARBNZM        I95NAR
087660050601     C                   MOVEL     '3'           I95TCN
087670050601     C                   MOVEL     ARBDSP        I95DAT
087680050601     C                   MOVEL     FLGTB1        I95TPO
087690050601     C     §3AFCA        IFNE      0
087700050601     C                   MOVEL     'S'           I95FCA
087710050601     C                   ENDIF
087720050601     c     lnpntw        ifeq      'FED'
087730050601     c                   movel     'S'           i95fi1
087740050601     c                   endif
087750050601     C**
087760050601     C                   CALL      'TISI95R'
087770050601     C                   PARM                    Tisi95DS
087780050601     C     O95ERR        IFEQ      *BLANKS
087790050601     C                   MOVEL     O95CTS        TASMCT
087800050601     C                   ENDIF
087810050601
087820050601     c                   z-add     arbncp        Tasncp
087830050601     c                   z-add     arbpkc        Taspkc
087840050601      * Bancali
087850050601     c                   If        %Subst(ArbGva:1:1) = 'B'
087860050601     c                   Eval      TasBan = V1cBan
087870050601     c                   EndIf
087880050601      * Colli Originali
087890050601     c                   If        %Subst(ArbGva:1:1) = 'O'
087900050601     c                   Eval      TasNcl = V1cCor
087910050601     c                   EndIf
087920050601     c                   endsr
087930050531     C*--- TASSAZIONE BOLLA ------------------------------------------*
087940050531     C     TASSAR        BEGSR
087950050601     c                   clear                   dtas
087960050601     c                   clear                   dtasv
087970050601     c* richiamo routine di riempimento campi da passare a tnsf20r
087980050601     c                   eval      wbol = '1'
087990050601     c                   exsr      SR_rtassa
088000050531     c                   eval      tassva = 'R'
088010050804     c
088020050531     c                   eval      parca  = 'S'
088030050531     c                   movel     paramt        kpjbu
088040050531      *
088050050531     C                   CALL      'TNSF20R'
088060050531     C                   PARM                    KPJBA
088070050531     C                   PARM                    DTAS
088080050531     C                   PARM                    DTASV
088090050722      *
088100050804      *
088110050722     c                   eval      parca  = ' '
088120050804     c***                movel     paramt        kpjbu
088130050722      *
088140050531     c                   endsr
088150911211     C*
088160011017     C*
088170991006     C* CALCOLO IL TOTALE FATTURA-------------------------------------*
088180990702     C     TOTFAT        BEGSR
088190030612     C                   CLEAR                   FNLV04DS
088200990702     C* AROTONDO IN ARRIVO O IN PARTENZA SE PREPAGATI NUOVI
088210990702    2C     *IN10         IFEQ      *ON
088220990702     C     *IN23         OREQ      *ON
088230991005     C                   MOVEL     VIDDIV        DIVGEI
088240991005     C                   EXSR      RIEGEI
088250991005     C                   MOVEL     §GEAED        D04AED
088260991005     C                   Z-ADD     §GEAVL        D04VAR
088270990702    2C                   ENDIF
088280991005     C                   MOVEL     'T'           D04FIM
088290990702     C                   MOVEL     ARBDFT        D04DFT
088300990702     C                   MOVEL     VIDIMV        D04IMV
088310990702     C                   MOVEL     VIDCEI        D04CEI
088320990702     C                   MOVEL     AR9TIC        D04TIC
088330990702     C                   MOVEL     AR9VCA        D04VCA
088340990702     C                   Z-ADD     AR9CAS        D04CAS
088350990702     C                   MOVEL     '1'           D04SPE
088360990702     C   23              MOVEL     '8'           D04SPE
088370991005     C                   MOVEL     §3ATB1        D04TBL
088380991005     C                   MOVEL     VIDDIV        D04DIV
088390990702     C                   Z-ADD     VIDPOR        D04POR
088400990702     C                   MOVEL     '1'           UNO               1
088410990702     C*
088420991005     C                   CALL      'FNLV04R'
088430030612     C                   PARM                    FNLV04DS
088440991005     C                   PARM                    DTASV
088450990702     C                   ENDSR
088460990702     C*
088470991005     C**************************************************************************
088480991005     C*  REPERIMENTO TABELLA GEI
088490991005     C**************************************************************************
088500991005     C     RIEGEI        BEGSR
088510030612     C                   CLEAR                   Tibs02DS
088520991117     C                   CLEAR                   DGEI
088530991117     C                   MOVEL     'C'           T02MOD
088540991117     C                   MOVEL     KNSIF         T02SIF
088550991117     C                   MOVEL     'GEI'         T02COD
088560991117     C                   MOVEL     DIVGEI        T02KE1
088570991117     C                   CALL      'TIBS02R'
088580991117     C                   PARM                    KPJBA
088590030612     C                   PARM                    Tibs02DS
088600991117     C     T02ERR        IFEQ      *BLANKS
088610991117     C                   MOVEL     T02UNI        DGEI
088620991117     C                   ENDIF
088630991005     C                   ENDSR
088640911211     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE TX -----------------*
088650911211     C     REGIS3        BEGSR
088660041027     C                   SETOFF                                       3709
088670130412     c                   clear                   wpda_ndc
088680130412     c                   clear                   wpda_ddc
088690130412     c                   clear                   wpda_dcm
088700041027      * salvo i campi relativi al valore da assicurare, qtà da fatturare, valore merce
088710041027     c                   eval      sav_arbias = arbias
088720041027     c                   eval      sav_arbvas = arbvas
088730041027     c                   eval      sav_arbqft = arbqft
088740041027     c                   eval      sav_arbvmd = arbvmd
088750041027     c                   eval      sav_arbvad = arbvas
088760041027     c                   movel     arbctr        sav_arbctr
088770950104     C*  GIRO UDATE
088780950104     C                   TIME                    W0140            14 0
088790950104     C* UDATE IN GGMMAAAA
088800950104     C                   MOVE      W0140         WDTGIO            8 0
088810950104     C*
088820950104     C* UDATE IN AAAAMMGG
088830950104     C                   Z-ADD     WDTGIO        G02DAT
088840950104     C                   MOVEL     *BLANK        G02ERR
088850950104     C                   CALL      'XSRDA8'
088860950104     C                   PARM                    WLBDAT
088870950104     C                   MOVEL     G02INV        DATEU             8 0
088880941221     C* PRENDO I VALORI ASSOLUTI
088890941221     C                   MLLZO     1             VIDPOR
088900941221     C                   MLLZO     1             VIDIMV
088910941221     C                   MLLZO     1             VIDIAS
088920941221     C                   MLLZO     1             VIDVMD
088930941221     C                   MLLZO     1             VIDQFT
088940941221     C                   MLLZO     1             VIDKLP
088950941221     C                   MLLZO     1             VIDKSC
088960070515     c
088970070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
088980070515     c* iornare direttamente il file
088990070515     c*  solo se cambiati dati su testa bolla
089000070515     c                   clear                   wsiaggio          1
089010070515  2b c                   if        sav_arbias <> vidias or sav_arbvas<>vidvas
089020070515     c                             or
089030070515     c                             sav_arbvmd <> vidvmd or sav_arbvad<>vidvad
089040070515     c                             or
089050070515     c                             sav_arbqft <> vidqft  or
089060070515     c                             sav_arbctr<>vidctr
089070070515     c                   eval      wsiaggio='S'
089080070515     c                   clear                   waltrabolla       1
089090070515     C   10KARB          CHAIN     FNBLP01L                           3091
089100070515     C  n10KARB          CHAIN     FNARB01L                           3091
089110070515     c*
089120070515     c                   if        *in91
089130070515     C                   SETON                                        28
089140070515     C                   MOVEL     MSG(4)        VIDMSG
089150070515     C                   GOTO      ENDRG3
089160070515     c                   else
089170070515     c                   if        *in30
089180070515     c                   eval      waltrabolla='N'
089190070515     c                   else
089200130411     c  n10              eval      wpda_ndc=arbndc
089210130411     c  n10              eval      wpda_ddc=arbddc
089220130411     c  n10              eval      wpda_dcm=arbdcm
089230070515     C   10KARB          CHAIN     FNARB01L                           30
089240070515     C  N10KARB          CHAIN     FNBLP01L                           30
089250070515     c* Alla fine per non sporcarmi i campi del record, richaino il
089260070515     c*  record originale che sto variando
089270130411     c   10              eval      wpda_ndc=arbndc
089280130411     c   10              eval      wpda_ddc=arbddc
089290130411     c   10              eval      wpda_dcm=arbdcm
089300070515     c                   endif
089310070515     c                   endif
089320070515     c                   endif
089330131120    1c                   if        wsiaggio=*blanks and d66pda<>' '
089340130412     c                   clear                   waltrabolla       1
089350130412    2c                   if        not *in10
089360130412     C     KARB          CHAIN(n)  FNARB01L                           30
089370130412    3c                   if        *in30
089380130412     c                   eval      waltrabolla='N'
089390130412    3c                   endif
089400130412    2c                   endif
089410130412     c                   eval      wpda_ndc=arbndc
089420130412     c                   eval      wpda_ddc=arbddc
089430130412     c                   eval      wpda_dcm=arbdcm
089440130412     C  N10KARB          CHAIN     FNBLP01L                           30
089450130412     c                   endif
089460941213     C*
089470941213     C* INVIO PER SEDE
089480170116     C***                MOVEL     'FIARBT0T'    SFILE
089490941213     C* ALLOCO OGGETTI
089500170116     C***                EXSR      ALLOC
089510070515     c
089520170116     c***                if        *in91
089530170116     C** 10              unlock    FNBLP01L
089540170116     C**N10              unlock    FNARB01L
089550170116     C**                 GOTO      ENDRG3
089560170116     c**                 endif
089570941213     C*
089580991001     C                   MOVEL     *BLANKS       WCKINL            1
089590991001     C                   EXSR      CARWAR
089600151020     c* Mmorizzo il totale imponibilie del video prima degli arrotondamenti
089610151020     c****               if        tassatoimv=0
089620151019     c                   movel     vidimv        Tassatoimv
089630151020     c****               endif
089640991006     C* SOLO SE C'E' NUMERO FATTURA CALCOLO
089650991006     C     VIDDFT        IFGT      *ZEROS
089660000105     C                   MOVEL     O17DFA        ARBDFT
089670991006     C                   EXSR      TOTFAT
089680991006     C     D04BOL        IFGT      *ZEROS
089690991006     C                   ADD       1             C
089700991006     C                   MOVEL     §$2BOL        WSV(C)
089710991006     C                   Z-ADD     D04BOL        WVA(C)
089720991006     C                   ENDIF
089730991006     C     D04IVA        IFGT      *ZEROS
089740991006     C                   ADD       1             C
089750991006     C                   MOVEL     §$2IVA        WSV(C)
089760991006     C                   Z-ADD     D04IVA        WVA(C)
089770991006     C                   ENDIF
089780991006     C                   ENDIF
089790941213     C*
089800941213     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
089810950616    1C     C             IFGT      0
089820941213     C                   SETON                                        09
089830941213     C*
089840941213     C* INVIO PER SEDE
089850941213     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
089860170116    2C***  C             IFGT      3
089870991006     C* MI SALVO I CAMPI DELL'FIARBT
089880170116     C***                MOVEL     SOVR(1)       SOARBT
089890170116     C***                MOVEL     SALC(1)       SAARBT
089900170116     C***                MOVEL     SDLC(1)       SDARBT
089910941213     C*
089920170116     C***                MOVEL     'FIARBU0T'    SFILE
089930941213     C* ALLOCO OGGETTI
089940170116     C***                EXSR      ALLOC
089950070515     c
089960170116     c**                 if        *in91
089970170116     C** 10              unlock    FNBLP01L
089980170116     C**N10              unlock    FNARB01L
089990170116     C**                 MOVEL     SDARBT        SDLC(1)
090000170116     c**                 exsr      DLCSED
090010170116     C**                 SETON                                        2891
090020170116     C**                 GOTO      ENDRG3
090030170116     c**                 endif
090040941213     C*
090050170116    2C***                ENDIF
090060950616    1C                   ENDIF
090070950114     C*
090080950114     C                   MOVEL     KNMUS         ARBPRU
090090950114     C                   Z-ADD     DATEU         ARBDTV
090100950114     C                   TIME                    ARBORV
090110950114     C                   MOVEL     VIDCVR        ARBCVB
090120991011     C                   MOVEL     '1'           ARBTRC
090130991011     C*
090140991011     C                   MOVEL     '1'           AR6TRC
090150930511     C*
090160060130     C* SE DA STORICIZZARE
090170060130    1C**   *IN10         IFEQ      *ON
090180060130     C**   D66FSA        ANDEQ     'S'
090190060130     C**   *IN10         OREQ      *OFF
090200060130     C**   D66FSP        ANDEQ     'S'
090210060130     C     D66FSA        ifeq      'S'
090220941213     C                   EXSR      STOARB
090230950616    1C                   ENDIF
090240060302     c* storicizzazione motivazione
090250060302     c     d66mtv        ifeq      'S'
090260060302     c                   exsr      sto_mtv
090270060302     c                   endif
090280941213     C**
090290991006     C* INVIO VARIAZIONE --> FIARBU0T
090300941213     C**
090310950616    1C     *IN09         IFEQ      *ON
090320941213     C     C             ANDGT     3
090330941213     C                   Z-ADD     4             C
090340941213     C*
090350941213    2C     D66FFI        IFEQ      'S'
090360950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
090370950616    3C     §3ABSD        IFEQ      ' '
090380941213     C                   Z-ADD     4             C
090390170116     C****               OPEN      FIARBU0T
090400941213     C*
090410941213     C* NE SCRIVO FIN TANTO CHE CE NE SONO
090420950616    4C     WSV(C)        DOWNE     ' '
090430941213     C                   MOVEL     WSV(C)        ARBSVN
090440941213     C                   Z-ADD     WVA(C)        ARBVAN
090450991006     C                   WRITE     FIARBUTT
090460941213     C                   ADD       1             C
090470950616    4C                   ENDDO
090480941213     C*
090490170116     C***                CLOSE     FIARBU0T
090500941213     C* DISALLOCO OGGETTO MEMBRO PER SEDE
090510950616    3C                   ENDIF
090520170116     C****               EXSR      DLCSED
090530950208    2C                   ENDIF
090540941213     C* REIMPOSTO I CAMPI
090550170116     C***                MOVEL     SOARBT        SOVR(1)
090560170116     C***                MOVEL     SAARBT        SALC(1)
090570170116     C***                MOVEL     SDARBT        SDLC(1)
090580941213    1C                   ENDIF
090590930506     C*
090600930506     C                   Z-ADD     VIDKSC        ARBKSC
090610930506     C                   MOVEL     VIDKLP        ARBKSC
090620941209     C                   MOVEL     VIDIAS        ARBIAS
090630941209     C                   MOVEL     VIDVAS        ARBVAS
090640011017      * Se l'importo da assicurare è a 0 non devo scrivere la divisa
090650011017     C     VIDIAS        IFEQ      *ZEROS
090660011017     C                   CLEAR                   ARBVAS
090670011017     C                   ENDIF
090680011017      *
090690941209     C                   MOVEL     VIDVMD        ARBVMD
090700941215     C                   MOVEL     VIDVAD        ARBVAD
090710011017      * Se il valore merce è a 0 non devo scrivere la divisa
090720011017     C     VIDVMD        IFEQ      *ZEROS
090730011017     C                   CLEAR                   ARBVAD
090740011017     C                   ENDIF
090750011017      *
090760941209     C                   MOVEL     VIDQFT        ARBQFT
090770930506     C                   MOVEL     VIDCTR        ARBCTR
090780991006     C                   MOVEL     VIDDIV        ARBDIV
090790941213     C                   MOVEL     VIDPOR        ARBPOR
090800941213     C                   MOVEL     WSV(1)        ARBSV1
090810941213     C                   MOVEL     WVA(1)        ARBVA1
090820941213     C                   MOVEL     WSV(2)        ARBSV2
090830941213     C                   MOVEL     WVA(2)        ARBVA2
090840941213     C                   MOVEL     WSV(3)        ARBSV3
090850941213     C                   MOVEL     WVA(3)        ARBVA3
090860991006     C                   Z-ADD     VIDIMV        ARBIMV
090870991006     C                   MOVEL     O17DFA        ARBDFT
090880950104     C                   MOVEL     VIDNFT        ARBNFT
090890950104     C                   MOVEL     VIDFIV        ARBFIV
090900991013     C* SE TOTALE IMPONIBILE ERA IMMESSO DALLA PARTENZA ED E'
090910991013     C* STATO MODIFICATO TOLGO LA 'P'
090920991013     C     AR6FIM        IFEQ      'P'
090930991013     C     ARBIMV        ANDNE     AR6IMV
090940991013     C     *IN10         ANDEQ     *ON
090950991013     C                   MOVE      *BLANKS       ARBFIM
090960991013     C                   ELSE
090970991013     C* ALTRIMENTI CI METTO QUELLO CHE C'ERA
090980991013     C                   MOVE      AR6FIM        ARBFIM
090990991013     C* SE NON ESISTE FIAR6 E SONO IN PARTENZA IMPOSTO 'P'
091000991013     C     WAR6          IFNE      'E'
091010991013     C     *IN10         ANDEQ     *OFF
091020991013     C                   MOVEL     'P'           ARBFIM
091030991013     C                   END
091040991013     C                   ENDIF
091050941209     C*
091060941212     C                   MOVEL     VIDCEI        ARBCEI
091070930506     C*
091080950616    1C     ARBDFT        IFGT      0
091090930505     C*
091100930505     C* SE TOTALE FATTURA NON CORRISPONDE AL PRECEDENTE RISTAMPA FATT
091110941209     C     D04IFT        COMP      AR6IFT                             3737
091120930505     C*
091130941212     C                   MOVE      D04ALI        ARBALI
091140941212     C                   MOVE      D04IFT        ARBIFT
091150941212     C                   MOVE      D04IMV        ARBIMV
091160930514     C*
091170970613    2C                   SELECT
091180941221     C     ARBPOR        WHENGT    0
091190941212     C                   Z-ADD     D04POR        ARBPOR
091200941221     C     ARBVA1        WHENGT    0
091210991007     C     ARBSV1        ANDNE     §$2BOL
091220991007     C     ARBSV1        ANDNE     §$2IVA
091230941213     C                   ADD       D04POR        ARBVA1
091240941209     C     ARBVA2        WHENGT    0
091250991007     C     ARBSV2        ANDNE     §$2BOL
091260991007     C     ARBSV2        ANDNE     §$2IVA
091270941213     C                   ADD       D04POR        ARBVA2
091280941209     C     ARBVA3        WHENGT    0
091290941213     C                   ADD       D04POR        ARBVA3
091300970613    2C                   ENDSL
091310941209     C*
091320950616    1C                   ENDIF
091330930511     C*
091340930511     C                   MOVEL     VIDCVR        ARBCVB
091350911218     C*
091360950616    1C     D66FFI        IFEQ      'S'
091370950227     C*
091380950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
091390950616    2C     §3ABSD        IFEQ      ' '
091400170116     C***                OPEN      FIARBT0T
091410991006     C                   WRITE     FIARBTTT
091420170116     C***                CLOSE     FIARBT0T
091430950616    2C                   END
091440941212     C* DISALLOCO OGGETTO MEMBRO PER SEDE
091450170116     C***                EXSR      DLCSED                                       FNARBT0T
091460950616    1C                   END
091470941213     C**
091480941216     C* A G G I O R N O
091490941213     C**
091500051214     c* Semodificato il cod tariffa, verifico se volume automatico
091510051214     c                   if        not *in10 and vidctr<>sav_arbctr
091520051214     C*
091530051214     C                   MOVEL     '7T'          COD
091540051214     C                   MOVEL(P)  ARBFVF        KEY
091550051214     C                   EXSR      CTABEL
091560051214     C                   MOVEL     TBLUNI        DS7T
091570051214     C     §7TRVL        IFEQ      'A'
091580051214     C     §7TRVL        OREQ      'P'
091590051214     c                   movel     'S'           wricvl
091600051214     c                   seton                                        24
091610051214     c                   endif
091620051214     c                   endif
091630051214     c
091640991006     C                   Z-ADD     ARBKSC        AR6KSC
091650991006     C                   Z-ADD     ARBCTR        AR6CTR
091660941213     C                   MOVE      ARBALI        AR6ALI
091670941213     C                   MOVE      ARBIFT        AR6IFT
091680941213     C                   MOVE      ARBIMV        AR6IMV
091690991006     C                   MOVEL     ARBDIV        AR6DIV
091700941213     C                   MOVE      ARBPOR        AR6POR
091710941213     C                   MOVE      ARBSV1        AR6SV1
091720941213     C                   MOVE      ARBVA1        AR6VA1
091730941213     C                   MOVE      ARBSV2        AR6SV2
091740941213     C                   MOVE      ARBVA2        AR6VA2
091750941213     C                   MOVE      ARBSV3        AR6SV3
091760941213     C                   MOVE      ARBVA3        AR6VA3
091770941213     C                   MOVEL     ARBCEI        AR6CEI
091780950104     C                   MOVEL     ARBDFT        AR6DFT
091790950104     C                   MOVEL     ARBNFT        AR6NFT
091800950104     C                   MOVEL     ARBFIV        AR6FIV
091810991013     C                   MOVEL     ARBFIM        AR6FIM
091820941213     C*
091830991006     C* CANCELLO LE VARIE NELL'FIAR7
091840941221    1C     WAR7          IFEQ      'E'
091850991006     C                   MOVEL     '1'           WTRC
091860991006     C     KARB2         SETLL     FIAR7000
091870991006     C     KARB2         READE     FIAR7000                               30
091880941213     C*
091890941213    2C     *IN30         DOWEQ     *OFF
091900991006     C                   DELETE    FIAR7000
091910991006     C     KARB2         READE     FIAR7000                               30
091920941213    2C                   ENDDO
091930941213    1C                   ENDIF
091940941213     C*
091950941213     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
091960941213    1C     WSV(4)        IFNE      ' '
091970941213     C                   Z-ADD     4             C
091980991006     C                   CLEAR                   FIAR7000
091990941213     C                   MOVEL     ARBAAS        AR7AAS
092000941213     C                   MOVEL     ARBLNP        AR7LNP
092010941213     C                   MOVEL     ARBNRS        AR7NRS
092020941213     C                   MOVEL     ARBNSP        AR7NSP
092030991006     C                   MOVEL     '1'           AR7TRC
092040941213    2C     WSV(C)        DOWNE     ' '
092050941213     C                   MOVEL     WSV(C)        AR7SVN
092060941213     C                   MOVEL     WVA(C)        AR7VAN
092070991006     C                   WRITE     FIAR7000
092080941213     C                   ADD       1             C
092090941213    2C                   ENDDO
092100941213    1C                   ENDIF
092110941213     C**
092120941216     C* SE HO ELIMINATO TUTTA LA TASSAZIONE, PER S.F., DELETO RECORD
092130950616    1C     AR6DFT        IFEQ      0
092140941216     C     AR6SV1        ANDEQ     ' '
092150941216     C     AR6SV2        ANDEQ     ' '
092160941216     C     AR6SV3        ANDEQ     ' '
092170941216     C     AR6POR        ANDEQ     0
092180991116     C     AR6CEI        ANDEQ     ' '
092190941216     C*
092200950616    2C     WAR6          IFEQ      'E'
092210041027     C     *IN10         ifeq      *OFF
092220991006     C                   MOVEL     '1'           WTRC
092230991006     C     KARB2         DELETE    FIAR6000                           30
092240991116     C                   ELSE
092250041027     C* SE variaz, in ARRIVO DEVO UGUALMENTE tenere
092260991116     C* FIAR6000 (SERVE PER LA PROCEDURA DEI DANNI)
092270991116     C                   UPDATE    FIAR6000
092280991116     C                   ENDIF
092290991116   X2C                   ELSE
092300041027     C* SE NON ESISTE RECORD TASSAZIONE scrivo record se var.in arrivo
092310041027     C     *IN10         ifeq      *ON
092320991116     C                   MOVEL     ARBAAS        AR6AAS
092330991116     C                   MOVEL     ARBLNP        AR6LNP
092340991116     C                   MOVEL     ARBNRS        AR6NRS
092350991116     C                   MOVEL     ARBNSP        AR6NSP
092360991116     C                   WRITE     FIAR6000
092370041027    3C                   ENDIF
092380041027    2C                   ENDIF
092390941216     C*
092400941216     C                   ELSE
092410941216     C*
092420950616    2C     WAR6          IFEQ      'E'
092430991006     C                   UPDATE    FIAR6000
092440941209     C                   ELSE
092450941209     C                   MOVEL     ARBAAS        AR6AAS
092460941209     C                   MOVEL     ARBLNP        AR6LNP
092470941209     C                   MOVEL     ARBNRS        AR6NRS
092480941209     C                   MOVEL     ARBNSP        AR6NSP
092490991006     C                   WRITE     FIAR6000
092500950616    2C                   ENDIF
092510950616    1C                   ENDIF
092520941209     C*
092530151016     c* Se tassazione fino all'imponibile, aggiorno peso e volume usati
092540151016     c*  per la fatturazione se inserito o variato totale imponibile
092550151016     c                   if        (ar6imv>0 and ar6imv<>savimv_fat) or
092560151016     c                             ar6imv=0
092570151118     c                   exsr      regpesivol
092580151016     c                   endif
092590950113     C*
092600950113     C* RICALCOLO VOLUME AUTOMATICO E POI VADO IN VIDEATA DEL VOLUME
092610950113     C*  SE VOLUME AUTOMATICO O PRESUNTO PER PRECEDENTE VARIAIZONE
092620950113     C*  MITTENTE
092630950616    1C     *IN24         IFEQ      *ON
092640051214     c* Mi salvo i campi che userò per la storicizzazione
092650051214     C                   MOVEL     sav_arbctr    MITCTR
092660051214     C                   MOVEL     ARBFVF        MITFVF
092670051214     C                   MOVEL     ARBVLF        MITVLF
092680051214     c
092690950113     C                   EXSR      CALVOL
092700070907     C**                 MOVEL     'I2'          D48CVB
092710070907     C**                 MOVEL     'E'           WESEG
092720070907     c                   add       1             kk
092730070907     c                   movel     'I2'          kcvb(kk)
092740070911     c                   movel     'V'           kffr(kk)
092750070911     c                   movel     ' '           kf12(kk)
092760070911     c****               clear                   w48f12
092770950114     C* AGGIORNO GIA' IL VOLUME TANTO PASSO DALLA VIDEATA DEL VOLUME
092780950114     C*  COMUNQUE PER LA TRASMISSIONE
092790950114     C                   MOVEL     D18FVF        ARBFVF
092800950114     C                   MOVEL     D18VLF        ARBVLF
092810070911     C*****              MOVEL     'V'           D48FFR
092820950616     C                   END
092830911211     C*
092840950114     C   10              UPDATE    FNARB000
092850950114     C  N10              UPDATE    FNBLP000
092860950114     C*
092870041027  1b c***                if        wstessogas = 'S' and (d66ffi = 'E' or
092880041027     c***                          d66ffi = 'P') and *in10 = *off
092890041027  2b c***                if        sav_vidias <> vidias or sav_vidvas <> vidvas
092900041027     c***                          or
092910041027     c***                          sav_vidvmd <> vidvmd or sav_vidvad <> vidvad
092920041027     c***                          or
092930041027     c***                          sav_vidqft <> vidqft
092940041027     c***                exsr      Agg_ARB
092950041027  2e c***                endif
092960041027  1e c***                endif
092970041027     c
092980041027     c* Aggiorno altra bolla se c'e' e variati i campi sopra
092990041027     c                   if        wsiaggio='S' and waltrabolla=' '
093000041027     c* non aggiono mai il ksc:
093010041027     c* in  partenza perchè il S.F. va in ar6
093020041027     c*  in arrivo perchè il cod mittente se cambiato viene impostato
093030041027     c*  in regis8
093040041027     C   10              except    arbTblp
093050041027     C  N10              except    arbTarb
093060041027     c                   endif
093070130411
093080130411     c* Richiamo routine per invio variazione a pda
093090130411     c                   exsr      sr_pda
093100911212     C     ENDRG3        ENDSR
093110911211     C*
093120970613     C* SE BOLLA SU DISTINTA CHIUSA NON FACCIO NULLA ALTRIMENTI ------*
093130970613     C*  CHIUDO LA BOLLA
093140970613     C     CHIUBO        BEGSR
093150970613     C                   MOVEL     'S'           WAGG              1
093160970613     C*
093170970613     C* NON AGGIORNO COL MANDATO DI INTROITO SE SULLA BOLLA C'E'
093180970613     C*  UN NUMERO DISTINTA ED E' CHIUSA
093190080516     c                   clear                   fvvfcf
093200970613     C     ARBNDC        IFGT      0
093210020911     c                   Z-Add     ArbIfp        Kfgs
093220970613     C                   Z-ADD     ARBNDC        W0050             5 0
093230020911     C     KFVV          CHAIN     FNFVV01L                           30
093240970613     C     *IN30         IFEQ      *OFF
093250970613     C     FVVFCF        ANDEQ     'S'
093260970613     C                   CLEAR                   WAGG
093270970613     C                   ENDIF
093280970613     C                   ENDIF
093290970613     C*
093300970613    4C     WAGG          IFEQ      'S'
093310970613    5C     ARBNMI        IFEQ      0
093320970613     C                   Z-ADD     8888888       ARBNMI
093330970613     C                   Z-ADD     8888888       ARBNDC
093340970613     C                   Z-ADD     DATEU         ARBDDC
093350080516     c                   else
093360080516     c
093370080516     c                   if        arbndc>0 and fvvfcf<>'S'
093380080516     C                   Z-ADD     8888888       ARBNDC
093390080516     c                   endif
093400080516     c
093410970613    5C                   ENDIF
093420970613    4C                   ENDIF
093430970613     C                   ENDSR
093440970613     C*
093450911211     C*--- CONTROLLI FORMATO4 ----------------------------------------*
093460911211     C     CONTR4        BEGSR
093470040123     C**!!!              MOVEL     ' '           CEISV3
093480920319     C                   EXSR      CTRCOM
093490920319     C   90              GOTO      ENDCT4
093500920319     C*
093510911211     C* T A S S A Z I O N E
093520991011     C     *INKN         IFEQ      *ON
093530991011     c* fattura immediata
093540991011     C     *IN14         OREQ      *ON
093550991011     C                   MOVEL     '2'           WBOL              1
093560991011     C                   EXSR      TASSAZ
093570991011     C   KN
093580991011     COR 90              GOTO      ENDCT4
093590991011     C                   END
093600911211     C*
093610990930     C* OBBLIGATORIO IMPORTO DELLA VARIA
093620040123     C**!!!VI2VA1        IFEQ      0
093630040123     C**!!!              MOVEL     MSG(51)       VIDMSG
093640040123     C**!!!              SETON                                        519028
093650040123     C**!!!              GOTO      ENDCT4
093660040123     C**!!!              END
093670991007     C* Se divisa sprotetta significa che manca tariffa: in questo caso
093680991007     C* controllo che la divisa inserita sia = alla divisa della moneta
093690991007     C*  di conto SE CLIENTE  9999
093700991007     C     *IN14         IFEQ      *ON
093710991007     C     *IN07         ANDEQ     *OFF
093720991007     C     VI2DIV        ANDNE     §GEDCN
093730991007     C     VIDKSC        ANDEQ     9999
093740991007     C                   SETON                                        459028
093750991007     C                   MOVEA     MSG(90)       K78
093760991007     C                   MOVEA     §GEDCN        K78(71)
093770991007     C                   MOVEA     K78           VIDMSG
093780991007     C                   GOTO      ENDCT4
093790991007     C                   ENDIF
093800991006     C* DEMANDO IL RESTO DEI CONTROLLI A PGM FNLV16R
093810100120     C                   CLEAR                   dlv1601
093820100120     C                   CLEAR                   Fnlv16DS
093830991006     C                   CLEAR                   DTASV
093840991006     C                   MOVEL     'A'           D17TBO
093850991006     C                   MOVEL     VIDKLP        D17KSC
093860991006     C                   MOVE      VIDKSC        D17KSC
093870991012     C     DFT2BO        IFGT      0
093880991012     C                   Z-ADD     DFT2BO        D17DSP
093890991012     C                   ELSE
093900991006     C                   Z-ADD     ARBDSP        D17DSP
093910991012     C                   ENDIF
093920991006     C                   MOVEL     ARBLNP        D17LNP
093930991006     C                   MOVEL     ARBLNA        D17LNA
093940100120     C                   MOVEL     §3ATB2        D17TBL
093950991006     C                   MOVEL     §3ARBL        D17RBL
093960020503     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
093970040123     C                   MOVEL     §3ASVA        §d17sva
093980040123      * passo che devo controllare la seconda bolla
093990040123     c                   Eval      §d17Tb2 = 'S'
094000100120
094010100120     c* Se si tratta di ffattura immediata , passo cod iso ddestintario
094020100120    1c                   if        *in14
094030100120     c                   Eval      §d17ctriso='S'
094040100120    2c                   if        arbcpi<>*blanks
094050100120     C     'PRIVATO'     SCAN      arbcpi        posiz                    30
094060100120     c* solo numeri
094070100120     c                   select
094080100120    3c                   when      not *in30 and %subst(arbcpi:1:2)>=*zeros
094090100120     c                   eval      §d17iso='IT'
094100100120     c* "PRIVATO" senza iso
094110100120     c                   when      *in30 and posiz<=3
094120100120     c                   eval      §d17iso='$$'
094130100120     c                   other
094140100120     c                   eval      §d17iso= %subst(arbcpi:1:2)
094150100120    3c                   ENDSL
094160100120    2c                   endif
094170100120    1c                   endif
094180100120     c
094190040123     c                   Movel     dlv1601       D17Flo
094200100120     c
094210991006     C                   MOVEL     VI2DIV        D17DIV
094220991006     C                   MOVEL     WDIV          D17DVP
094230991006    1C     §3AFCA        IFEQ      2
094240991006     C                   MOVEL     'S'           D17FCA
094250991006    1C                   ENDIF
094260991006     C                   MOVEL     'E'           D17FIM
094270991006     C                   MOVEL     VI2CEI        D17CEI
094280991006     C                   MOVEL     VI2SV1        VSV(1)
094290040123     C**!!!              MOVEL     VI2VA1        VVA(1)
094300040123     c                   Z-add     Vi2Va1        vva(1)
094310040123     c                   Movel     Vi2Es1        ves(1)
094320040123     c                   Movel     Vi2Sv2        vsv(2)
094330040123     c                   Movel     Vi2Es2        ves(2)
094340040123     c                   Z-add     Vi2Va2        vva(2)
094350040123     c                   MOVEL     Vi2Sv3        vsv(3)
094360040123     c                   Z-add     Vi2Va3        vva(3)
094370040123     c                   Movel     Vi2Es3        ves(3)
094380040123     c                   MOVEL     Vi2Sv4        vsv(4)
094390040123     c                   Z-add     Vi2Va4        vva(4)
094400040123     c                   Movel     Vi2Es4        ves(4)
094410041130     c                   MOVEL     Vi2Sv5        vsv(5)
094420041130     c                   Z-add     Vi2Va5        vva(5)
094430041130     c                   Movel     Vi2Es5        ves(5)
094440041130     c                   MOVEL     Vi2Sv6        vsv(6)
094450041130     c                   Z-add     Vi2Va6        vva(6)
094460041130     c                   Movel     Vi2Es6        ves(6)
094470991006     C                   CALL      'FNLV16R'
094480030612     C                   PARM                    Fnlv16DS
094490991006     C                   PARM                    DTASV
094500991011     C                   MOVEL     D17DIV        VI2DIV
094510991006     C                   MOVEL     D17CEI        VI2CEI
094520991011     C                   MOVEL     O17DEI        DESCEI
094530040123     C**!!!              MOVEL     VSV(1)        VI2SV1
094540040123     C**!!!              MOVEL     VES(1)        CEISV3            1
094550040123      * Ricarico le varie
094560040123     c                   Movel     Vsv(1)        Vi2Sv1
094570040123     c                   Z-add     Vva(1)        Vi2Va1
094580040123     c                   Movel     Ves(1)        Vi2Es1
094590040123     c                   Movel     Vsv(2)        Vi2Sv2
094600040123     c                   Z-add     Vva(2)        Vi2Va2
094610040123     c                   Movel     Ves(2)        Vi2Es2
094620040123     c                   Movel     Vsv(3)        Vi2Sv3
094630040123     c                   Z-add     Vva(3)        Vi2Va3
094640040123     c                   Movel     Ves(3)        Vi2Es3
094650040123     c                   Movel     Vsv(4)        Vi2Sv4
094660040123     c                   Z-add     Vva(4)        Vi2Va4
094670040123     c                   Movel     Ves(4)        Vi2Es4
094680041130     c                   Movel     Vsv(5)        Vi2Sv5
094690041130     c                   Z-add     Vva(5)        Vi2Va5
094700041130     c                   Movel     Ves(5)        Vi2Es5
094710041130     c                   Movel     Vsv(6)        Vi2Sv6
094720041130     c                   Z-add     Vva(6)        Vi2Va6
094730041130     c                   Movel     Ves(6)        Vi2Es6
094740040123      * Controllo se caricata almeno una varia
094750041130do  1c                   Do        6             c
094760040123     c                   If        vsv(c) <> *Blanks
094770040123     c                   Eval      wesvar1 = 'S'
094780040123     c                   EndIf
094790040123    1c                   EndDo
094800040123
094810991006     C     O17ERR        IFNE      *BLANKS
094820991006     C                   SETON                                        90
094830991006     C     O17MSG        IFNE      *BLANKS
094840991006     C                   MOVEL     O17MSG        VIDMSG
094850991006     C                   SETON                                        28
094860991006     C                   ENDIF
094870991006     C     O17ERR        IFEQ      '6'
094880040123     c                   Select
094890040123     c                   When      O17Erv = 1
094900991006     C                   SETON                                        49
094910040123     c                   When      O17Erv = 2
094920040123     C                   SETON                                        63
094930040123     c                   When      O17Erv = 3
094940040123     C                   SETON                                        64
094950040123     c                   When      O17Erv = 4
094960040123     C                   SETON                                        65
094970041130     c                   When      O17Erv = 5
094980041130     C                   SETON                                        66
094990041130     c                   When      O17Erv = 6
095000041130     C                   SETON                                        67
095010040123     c                   EndSl
095020991006     C                   ENDIF
095030991006     C     O17ERR        IFEQ      '1'
095040991006     C                   SETON                                        50
095050991006     C                   ENDIF
095060991006     C     O17ERR        IFEQ      '7'
095070991006     C                   SETON                                        45
095080991006     C                   ENDIF
095090991006     C   90              GOTO      ENDCT4
095100991006     C                   ENDIF
095110991006     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
095120991006    1C  N07VI2DIV        IFNE      WDIV
095130991006     C* CONVERSIONE VARIA
095140991006    3C     VI2VA1        IFEQ      VA2VA1
095150991006     C     VI2VA1        ANDGT     *ZEROS
095160991006     C     WDIV          ANDNE     *BLANKS
095170030612     C                   CLEAR                   YeurcoDS
095180991006     C                   MOVEL     WDIV          YECDVI
095190991006     C                   Z-ADD     VI2VA1        YECIMI
095200991006     C                   MOVEL     VI2DIV        YECDVO
095210991006     C     O17FDC        IFEQ      'S'
095220011114     C                   MOVE      '3'           YECDCO
095230991006     C                   ELSE
095240991006     C                   CLEAR                   YECDCO
095250991006     C                   ENDIF
095260991006     C                   CALL      'YEURCO'
095270030612     C                   PARM                    YeurcoDS
095280991006     C                   Z-ADD     YECIMO        VI2VA1
095290991012     C                   SETON                                        51
095300991006    3C                   ENDIF
095310040123      * Conversione Varia 2
095320040123    2c                   IF        Vi2Va2 = Va2Va2 and Vi2Va2 > *Zeros and
095330040123     c                             wdiv <> *Blanks
095340040123     c                   Clear                   Yeurcods
095350040123     c                   Movel     wdiv          YECDVI
095360040123     c                   Z-add     Vi2Va2        YECIMI
095370040123     c                   Movel     Vi2Div        YECDVO
095380040123     c                   If        O17Fdc = 'S'
095390040123     c                   Movel     '3'           YECDCO
095400040123     c                   EndIf
095410040123     c                   Call      'YEURCO'
095420040123     c                   Parm                    Yeurcods
095430040123     c                   Z-add     YECIMO        Vi2Va2
095440040123     c                   Eval      *In63 = *On
095450040123    2c                   EndIf
095460040123      * Conversione Varia 3
095470040123    2c                   IF        Vi2Va3 = Va2Va3 and Vi2Va3 > *Zeros and
095480040123     c                             wdiv <> *Blanks
095490040123     c                   Clear                   yeurcods
095500040123     c                   Movel     wdiv          YECDVI
095510040123     c                   Z-add     Vi2Va3        YECIMI
095520040123     c                   Movel     Vi2Div        YECDVO
095530040123     c                   If        O17Fdc = 'S'
095540040123     c                   Movel     '3'           YECDCO
095550040123     c                   EndIf
095560040123     c                   Call      'YEURCO'
095570040123     c                   Parm                    yeurcods
095580040123     c                   Z-add     YECIMO        Vi2Va3
095590040123     c                   Eval      *In64 = *On
095600040123    2c                   EndIf
095610040123      * Conversione Varia 4
095620040123    2c                   IF        Vi2Va4 = Va2Va4 and Vi2Va4 > *Zeros and
095630040123     c                             wdiv <> *Blanks
095640040123     c                   Clear                   yeurcods
095650040123     c                   Movel     wdiv          YECDVI
095660040123     c                   Z-add     Vi2Va4        YECIMI
095670040123     c                   Movel     Vi2Div        YECDVO
095680040123     c                   If        O17Fdc = 'S'
095690040123     c                   Movel     '3'           YECDCO
095700040123     c                   EndIf
095710040123     c                   Call      'YEURCO'
095720040123     c                   Parm                    yeurcods
095730040123     c                   Z-add     YECIMO        Vi2Va4
095740040123     c                   Eval      *In65 = *On
095750040123    2c                   EndIf
095760041130      * Conversione Varia 5
095770041130    2c                   IF        Vi2Va5 = Va2Va5 and Vi2Va5 > *Zeros and
095780041130     c                             wdiv <> *Blanks
095790041130     c                   Clear                   yeurcods
095800041130     c                   Movel     wdiv          YECDVI
095810041130     c                   Z-add     Vi2Va5        YECIMI
095820041130     c                   Movel     Vi2Div        YECDVO
095830041130     c                   If        O17Fdc = 'S'
095840041130     c                   Movel     '3'           YECDCO
095850041130     c                   EndIf
095860041130     c                   Call      'YEURCO'
095870041130     c                   Parm                    yeurcods
095880041130     c                   Z-add     YECIMO        Vi2Va5
095890041130     c                   Eval      *In66 = *On
095900041130    2c                   EndIf
095910041130      * Conversione Varia 6
095920041130    2c                   IF        Vi2Va6 = Va2Va6 and Vi2Va6 > *Zeros and
095930041130     c                             wdiv <> *Blanks
095940041130     c                   Clear                   yeurcods
095950041130     c                   Movel     wdiv          YECDVI
095960041130     c                   Z-add     Vi2Va6        YECIMI
095970041130     c                   Movel     Vi2Div        YECDVO
095980041130     c                   If        O17Fdc = 'S'
095990041130     c                   Movel     '3'           YECDCO
096000041130     c                   EndIf
096010041130     c                   Call      'YEURCO'
096020041130     c                   Parm                    yeurcods
096030041130     c                   Z-add     YECIMO        Vi2Va6
096040041130     c                   Eval      *In67 = *On
096050041130    2c                   EndIf
096060040123     C**!!!              Z-ADD     VI2VA1        VA2VA1
096070040123     C**!!!              Z-ADD     VI2VA1        VVA(1)
096080040123     C**!!!              MOVEL     VI2DIV        WDIV
096090991012     C* RIEMETTO COMUNQUE LA VIDEATA SE HANNO VARIATO LA DIVISA
096100991012     C* (POTREI RIEMETTERLA SOLO SE HO CONVERTITO IMPORTI MA LA
096110991012     C* RIEMETTO COMUNQUE PER FARE COME PER LA PRIMA BOLLA)
096120991012     C                   SETON                                        90
096130040123
096140040123     c                   Z-add     Vi2Va1        Va2Va1
096150040123     c                   Z-add     Vi2Va2        Va2Va2
096160040123     c                   Z-add     Vi2Va3        Va2Va3
096170040123     c                   Z-add     Vi2Va4        Va2Va4
096180041130     c                   Z-add     Vi2Va5        Va2Va5
096190041130     c                   Z-add     Vi2Va6        Va2Va6
096200040123     c                   Z-add     Vi2Va1        Vva(1)
096210040123     c                   Z-add     Vi2Va2        Vva(2)
096220040123     c                   Z-add     Vi2Va3        Vva(3)
096230040123     c                   Z-add     Vi2Va4        Vva(4)
096240041130     c                   Z-add     Vi2Va5        Vva(5)
096250041130     c                   Z-add     Vi2Va6        Vva(6)
096260040123     c                   Movel     Vi2Div        Wdiv
096270040123
096280991012     C                   GOTO      ENDCT4
096290991006   X1C                   ELSE
096300991006     C                   Z-ADD     VI2VA1        VA2VA1
096310040123     c                   Z-add     Vi2Va2        Va2Va2
096320040123     c                   Z-add     Vi2Va3        Va2Va3
096330040123     c                   Z-add     Vi2Va4        Va2Va4
096340041130     c                   Z-add     Vi2Va5        Va2Va5
096350041130     c                   Z-add     Vi2Va6        Va2Va6
096360991006    1C                   ENDIF
096370011206      *
096380011206      * Controllo che la somma degli importi immessi non superi il valore
096390011206      * impostato in §GEIMF
096400040123     c**!!!              eval      w0173 = vi2va1
096410040123     c                   xfoot     vva           w0173
096420011206      *      controllo se devo convertire il totale imponibile
096430011206  1b c                   if        vi2div <> §gedcn
096440030612     c                   clear                   YeurcoDS
096450011206     c                   eval      yecdvi = vi2div
096460011206     c                   eval      yecdvo = §gedcn
096470040123     c**!!!              eval      yecimi = vi2va1
096480040123     c                   eval      yecimi = w0173
096490011206     c                   move      decsav        yecdco
096500011206     c                   call      'YEURCO'
096510030612     c                   parm                    YeurcoDS
096520011206  2b c                   if        yecesi = ' '
096530011206     c                   eval      w0173 = yecimo
096540011206  2e c                   endif
096550011206  1e c                   endif
096560011206      *      controllo se l'imponibile supera il limite fatturabile
096570011220  1b c                   if        w0173 > imfsav
096580011206     c                   eval      vidmsg = msg(110)
096590011220     c                   eval      %subst (vidmsg:45:14) = (%editc(imfsav: '4'))
096600011206     c                   eval      %subst (vidmsg:60:3) = §gedcn
096610040123     c**!!!              eval      *in51 = *on
096620011206     c                   eval      *in90 = *on
096630011206     c                   goto      endct4
096640011206  1e c                   endif
096650040123
096660040123     c                   Z-add     w0173         TotImv
096670920319     C*
096680911211     C     ENDCT4        ENDSR
096690911212     C*
096700911212     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE 2X -----------------*
096710911212     C     REGIS4        BEGSR
096720040130     c                   Setoff                                       3709
096730130412     c                   clear                   wpda_ndc
096740130412     c                   clear                   wpda_ddc
096750130412     c                   clear                   wpda_dcm
096760040130
096770950104     C*  GIRO UDATE
096780950104     C                   TIME                    W0140            14 0
096790950104     C* UDATE IN GGMMAAAA
096800950104     C                   MOVE      W0140         WDTGIO            8 0
096810950104     C*
096820950104     C* UDATE IN AAAAMMGG
096830950104     C                   Z-ADD     WDTGIO        G02DAT
096840950104     C                   MOVEL     *BLANK        G02ERR
096850950104     C                   CALL      'XSRDA8'
096860950104     C                   PARM                    WLBDAT
096870950104     C                   MOVEL     G02INV        DATEU             8 0
096880941221     C* PRENDO I VALORI ASSOLUTI
096890941221     C                   MLLZO     1             VIDKLP
096900941221     C                   MLLZO     1             VIDKSC
096910941221     C                   MLLZO     1             VI2VA1
096920040123     C                   MLLZO     1             VI2VA2
096930040123     C                   MLLZO     1             VI2VA3
096940040123     C                   MLLZO     1             VI2VA4
096950041130     C                   MLLZO     1             VI2VA5
096960041130     C                   MLLZO     1             VI2VA6
096970941221     C*
096980950203     C                   MOVEL     KNMUS         ARBPRU
096990920120     C* INVIO PER SEDE
097000170116     C****               MOVEL     'FIARBT0T'    SFILE             8
097010930210     C* ALLOCO OGGETTI
097020170116     C***                EXSR      ALLOC
097030170116     C***91              GOTO      ENDRG4
097040040123      * carico le varie in una sk di comodo
097050040123     c                   ExSr      CarWar1
097060040123      * se c'è n.fattura calcolo
097070040129If  1c                   If        Dft2bo > *Zeros
097080040129     c                   Movel     Dft2bo        ArbDft
097090040123     C                   MOVEL     VI2DIV        ARBDIV
097100040129     C                   MOVEL     VI2DIV        divgei
097110040123     C                   EXSR      RIEGEI
097120040123     C                   CLEAR                   Fnlv04ds
097130040123     C                   MOVEL     'T'           D04FIM
097140040123     C                   Z-ADD     ArbDft        D04DFT
097150040123     C                   MOVEL     §GEAED        D04AED
097160040123     C                   Z-ADD     §GEAVL        D04VAR
097170040123     c                   Z-add     TotImv        D04Imv
097180040123     C                   MOVEL     VI2CEI        D04CEI
097190040123     C                   MOVEL     AR9TIC        D04TIC
097200040123     C                   MOVEL     AR9VCA        D04VCA
097210040123     C                   Z-ADD     AR9CAS        D04CAS
097220040123     C                   MOVEL     '2'           D04SPE
097230040123     C                   MOVEL     §3ATB2        D04TBL
097240040123     C                   MOVEL     VI2DIV        D04DIV
097250040123     C                   MOVEL     '1'           UNO
097260040123     C*
097270040123     C                   CALL      'FNLV04R'
097280040123     C                   PARM                    Fnlv04ds
097290040123     C                   PARM                    DTASV
097300040123     C*
097310040123     C     D04BOL        IFGT      *ZEROS
097320040123     C                   ADD       1             C
097330040123     C                   MOVEL     §$2BOL        WSV(C)
097340040123     C                   Z-ADD     D04BOL        WVA(C)
097350040123     C                   ENDIF
097360040123     C     D04IVA        IFGT      *ZEROS
097370040123     C                   ADD       1             C
097380040123     C                   MOVEL     §$2IVA        WSV(C)
097390040123     C                   Z-ADD     D04IVA        WVA(C)
097400040123     C                   ENDIF
097410040123    1c                   EndIf
097420040123
097430040123     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
097440040123    1C     C             IFGT      0
097450040123     C                   SETON                                        09
097460040123     C*
097470040123     C* INVIO PER SEDE
097480040123     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
097490170116    2C**   C             IFGT      3
097500040123     C* MI SALVO I CAMPI DELL'FIARBT
097510170116     C***                MOVEL     SOVR(1)       SOARBT
097520170116     C***                MOVEL     SALC(1)       SAARBT
097530170116     C***                MOVEL     SDLC(1)       SDARBT
097540040123     C*
097550170116     C***                MOVEL     'FIARBU0T'    SFILE
097560040123     C* ALLOCO OGGETTI
097570170116     C***                EXSR      ALLOC
097580170116     c***                if        *in91
097590170116     C***                MOVEL     SDARBT        SDLC(1)
097600170116     c***                exsr      DLCSED
097610170116     C***                SETON                                        2891
097620170116     C***                GOTO      ENDRG4
097630170116     c***                endif
097640070515     c
097650170116    2C****               ENDIF
097660040123    1C                   ENDIF
097670911212     C*
097680991007     C                   Z-ADD     DATEU         ARBDTV
097690991007     C                   TIME                    ARBORV
097700991007     C                   MOVEL     VIDCVR        ARBCVB
097710991011     C                   MOVEL     '2'           ARBTRC
097720991011     C                   MOVEL     '2'           AR6TRC
097730991007     C                   CLEAR                   ARBFST
097740991007     C                   CLEAR                   ARBDDS
097750991007     C                   CLEAR                   ARBQFT
097760991007     C                   CLEAR                   ARBVMD
097770991007     C                   CLEAR                   ARBVAD
097780991007     C                   CLEAR                   ARBIAS
097790991007     C                   CLEAR                   ARBVAS
097800911212     C*
097810941212     C* SE DA STORICIZZARE --> STORICIZZO
097820991007     C     WAR62         IFEQ      'E'
097830060130    1C     D66FSA        ANDEQ     'S'
097840060130     C**   *IN10         IFEQ      *ON
097850060130    1C**   D66FSA        ANDEQ     'S'
097860060130     C**   *IN10         OREQ      *OFF
097870060130    1C**   D66FSP        ANDEQ     'S'
097880991007     C                   EXSR      STOARB
097890941212     C                   END
097900060130     C**                 END
097910060302     c* storicizzazione motivazione
097920060302     c     d66mtv        ifeq      'S'
097930060302     c                   exsr      sto_mtv
097940060302     c                   endif
097950040123     C**
097960040123     C* INVIO VARIAZIONE --> FIARBU0T
097970040123     C**
097980040123    1C     *IN09         IFEQ      *ON
097990040123     C     C             ANDGT     3
098000040123     C                   Z-ADD     4             C
098010040123     C*
098020040123    3C     D66FFI        IFEQ      'S'
098030040123     C                   Z-ADD     4             C
098040170116     C****               OPEN      FIARBU0T
098050040123     C*
098060040123     C* NE SCRIVO FIN TANTO CHE CE NE SONO
098070040123    4C     WSV(C)        DOWNE     ' '
098080040123     C                   MOVEL     WSV(C)        ARBSVN
098090040123     C                   Z-ADD     WVA(C)        ARBVAN
098100040123     C                   WRITE     FIARBUTT
098110040123     C                   ADD       1             C
098120040123    4C                   ENDDO
098130040123     C*
098140170116     C****               CLOSE     FIARBU0T
098150040123     C* DISALLOCO OGGETTO MEMBRO PER SEDE
098160170116     C****               EXSR      DLCSED                                       FIARBU0T
098170040123    3C                   ENDIF
098180040123     C* REIMPOSTO I CAMPI
098190170116     C***                MOVEL     SOARBT        SOVR(1)
098200170116     C***                MOVEL     SAARBT        SALC(1)
098210170116     C***                MOVEL     SDARBT        SDLC(1)
098220040123    1C                   ENDIF
098230991011     C*
098240991011     C                   MOVEL     VIDKLP        ARBKSC
098250991007     C                   MOVE      VIDKSC        ARBKSC
098260991007     C                   CLEAR                   ARBPOR
098270991007     C                   MOVEL     VI2DIV        ARBDIV
098280040123     C**!!!              MOVEL     VI2SV1        ARBSV1
098290040123     C**!!!              MOVEL     VI2VA1        ARBVA1
098300040123     C                   MOVEL     WSV(1)        ARBSV1
098310040123     C                   Z-ADD     WVA(1)        ARBVA1
098320040123     C                   MOVEL     WSV(2)        ARBSV2
098330040123     C                   Z-ADD     WVA(2)        ARBVA2
098340040123     C                   MOVEL     WSV(3)        ARBSV3
098350040123     C                   Z-ADD     WVA(3)        ARBVA3
098360991007     C                   MOVEL     VI2CEI        ARBCEI
098370991007     C                   MOVEL     VIDCTR        ARBCTR
098380040123     C**!!!              Z-ADD     VI2VA1        ARBIMV
098390040123     C                   Z-ADD     TotImv        ARBIMV
098400040123     C**!!!              CLEAR                   ARBSV2
098410040123     C**!!!              CLEAR                   ARBVA2
098420040123     C**!!!              CLEAR                   ARBSV3
098430040123     C**!!!              CLEAR                   ARBVA3
098440040129     C**!!!              CLEAR                   ARBALI
098450040129     C**!!!              CLEAR                   ARBIFT
098460991007     C                   Z-ADD     VI2NFT        ARBNFT
098470991007     C                   Z-ADD     VI2FIV        ARBFIV
098480991007     C                   Z-ADD     AR6DFT        ARBDFT
098490991013     C* SE TOTALE IMPONIBILE ERA IMMESSO DALLA PARTENZA ED E'
098500991013     C* STATO MODIFICATO TOLGO LA 'P'
098510991013     C     AR6FIM        IFEQ      'P'
098520991013     C     ARBIMV        ANDNE     AR6IMV
098530991013     C     *IN10         ANDEQ     *ON
098540991013     C                   MOVE      *BLANKS       ARBFIM
098550991013     C                   ELSE
098560991013     C* ALTRIMENTI CI METTO QUELLO CHE C'ERA
098570991013     C                   MOVE      AR6FIM        ARBFIM
098580991013     C* SE NON ESISTE FIAR6 E SONO IN PARTENZA IMPOSTO 'P'
098590991013     C     WAR62         IFNE      'E'
098600991013     C     *IN10         ANDEQ     *OFF
098610991013     C                   MOVEL     'P'           ARBFIM
098620991013     C                   END
098630991013     C                   ENDIF
098640040129
098650040130if  1c                   If        Arbdft > 0
098660040129     C* SE CAMBIATO TOTALE FATTURA RISTAMPO IN AUTOMATICO LA BOLLA
098670040129     C     D04IFT        COMP      AR6IFT                             3737
098680040129
098690040129     C                   MOVE      D04ALI        ARBALI
098700040129     C                   Z-ADD     D04IFT        ARBIFT
098710040130     C                   z-add     D04IMV        ARBIMV
098720051111     c                   select
098730051111     C     ARBVA1        WHENGT    0
098740051111     C     ARBSV1        ANDNE     §$2BOL
098750051111     C     ARBSV1        ANDNE     §$2IVA
098760051111     C                   ADD       D04POR        ARBVA1
098770051111     C     ARBVA2        WHENGT    0
098780051111     C     ARBSV2        ANDNE     §$2BOL
098790051111     C     ARBSV2        ANDNE     §$2IVA
098800051111     C                   ADD       D04POR        ARBVA2
098810051111     C     ARBVA3        WHENGT    0
098820051111     C                   ADD       D04POR        ARBVA3
098830051111    3C                   ENDSL
098840040130     c                   EndIf
098850911212     C*
098860070503     C     D66FFI        IFEQ      'S'
098870170116     C***                OPEN      FIARBT0T
098880991007     C                   WRITE     FIARBTTT
098890170116     C***                CLOSE     FIARBT0T
098900911212     C* DISALLOCO OGGETTO
098910170116     C****               EXSR      DLCSED                                       FIARBT0T
098920911212     C                   END
098930911212     C*
098940911212     C* AGGIORNO
098950991013     C                   Z-ADD     ARBKSC        AR6KSC
098960991013     C                   Z-ADD     ARBCTR        AR6CTR
098970991013     C                   MOVEL     ARBDIV        AR6DIV
098980991013     C                   Z-ADD     ARBPOR        AR6POR
098990991013     C                   MOVEL     ARBSV1        AR6SV1
099000991013     C                   MOVEL     ARBSV2        AR6SV2
099010991013     C                   MOVEL     ARBSV3        AR6SV3
099020991013     C                   Z-ADD     ARBVA1        AR6VA1
099030991013     C                   Z-ADD     ARBVA2        AR6VA2
099040991013     C                   Z-ADD     ARBVA3        AR6VA3
099050991013     C                   Z-ADD     ARBIMV        AR6IMV
099060991013     C                   Z-ADD     ARBALI        AR6ALI
099070991013     C                   MOVEL     ARBCEI        AR6CEI
099080991013     C                   Z-ADD     ARBIFT        AR6IFT
099090991013     C                   Z-ADD     ARBFIV        AR6FIV
099100991013     C                   Z-ADD     ARBNFT        AR6NFT
099110991013     C                   Z-ADD     ARBDFT        AR6DFT
099120991013     C                   MOVEL     ARBFIM        AR6FIM
099130040123     C*
099140040123     C* CANCELLO LE VARIE NELL'FIAR7
099150040123    1C     WAR72         IFEQ      'E'
099160040123     C                   MOVEL     '2'           WTRC
099170040123     C     KARB2         SETLL     FIAR7000
099180040123     C     KARB2         READE     FIAR7000                               30
099190040123     C*
099200040123    2C     *IN30         DOWEQ     *OFF
099210040123     C                   DELETE    FIAR7000
099220040123     C     KARB2         READE     FIAR7000                               30
099230040123    2C                   ENDDO
099240040123    1C                   ENDIF
099250040123     C*
099260040123     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
099270040123    1C     WSV(4)        IFNE      ' '
099280040123     C                   Z-ADD     4             C
099290040123     C                   CLEAR                   FIAR7000
099300040123     C                   MOVEL     ARBAAS        AR7AAS
099310040123     C                   MOVEL     ARBLNP        AR7LNP
099320040123     C                   MOVEL     ARBNRS        AR7NRS
099330040123     C                   MOVEL     ARBNSP        AR7NSP
099340040123     C                   MOVEL     '2'           AR7TRC
099350040123    2C     WSV(C)        DOWNE     ' '
099360040123     C                   MOVEL     WSV(C)        AR7SVN
099370040123     C                   MOVEL     WVA(C)        AR7VAN
099380040123     C                   WRITE     FIAR7000
099390040123     C                   ADD       1             C
099400040123    2C                   ENDDO
099410040123    1C                   ENDIF
099420040123
099430990930     C     WAR62         IFEQ      'E'
099440991007     C                   UPDATE    FIAR6000
099450941212     C                   ELSE
099460991013     C                   CLEAR                   AR6ATB
099470991013     C                   Z-ADD     ARBAAS        AR6AAS
099480991013     C                   Z-ADD     ARBLNP        AR6LNP
099490991013     C                   Z-ADD     ARBNRS        AR6NRS
099500991013     C                   Z-ADD     ARBNSP        AR6NSP
099510991013     C                   MOVEL     '2'           AR6TRC
099520991007     C                   WRITE     FIAR6000
099530941212     C                   ENDIF
099540930211     C*
099550991013     C                   UNLOCK    FNARB01L
099560920204     C* RISTAMPO FATTURA
099570070309     c* ristampo fattura immediata se variata(37 on),
099580070309     c*   e se non attiva GEO
099590130321     C***  *IN37         IFEQ      *ON
099600130321     c***  FGSddaSI      andeq     ' '
099610130321     C***                CLEAR                   FNLSB5DS
099620130321     C***                MOVEL     'V'           DB0RIS
099630130321     C***                MOVEL     D48TBO        DB0TBO
099640130321     C***                MOVEL     ARBAAS        DB0AAS
099650130321     C***                MOVEL     ARBLNP        DB0LNP
099660130321     C***                MOVEL     ARBNRS        DB0NRS
099670130321     C***                MOVEL     ARBNSP        DB0NSP
099680130321     C***                CALL      D90PSL
099690130321     C***                PARM                    FNLSB5DS
099700130321     C***                END
099710130411
099720130411     c                   eval      wpda_ndc=arbndc
099730130411     c                   eval      wpda_ddc=arbddc
099740130411     c                   eval      wpda_dcm=arbdcm
099750130411     c* Richiamo routine per invio variazione a pda
099760130411     c                   exsr      sr_pda
099770911212     C*
099780911212     C     ENDRG4        ENDSR
099790920116     C*
099800920116     C*--- CONTROLLI FORMATO5 ----------------------------------------*
099810920117     C     CONTR5        BEGSR
099820920116     C* CODICE BOLLA NUOVO
099830941102    1C     *IN21         IFEQ      *OFF
099840941102     C*
099850941213     C     '?'           SCAN      NEWCBO                                 90
099860941213    2C     *IN90         IFEQ      *ON
099870941213     C                   MOVEL     CODUT         §KUT
099880920116     C                   MOVEL     '3A'          §COD
099890941102     C                   MOVE      *BLANKS       NEWCBO
099900991011     C                   CLEAR                   §KEY
099910920116     C                   CALL      'X§TABER'
099920920116     C                   PARM                    §KUT
099930920116     C                   PARM                    §COD
099940920116     C                   PARM                    §KEY
099950941209     C                   PARM                    §DES
099960941102     C                   MOVEL     §KEY          NEWCBO
099970941102     C                   MOVEL     §DES          DENCBO
099980920116     C                   SETON                                        90
099990920116     C                   GOTO      ENDCT5
100000941102    2C                   END
100010920116     C*
100020920116     C                   MOVEL     '3A'          COD
100030941102     C                   MOVEL(P)  NEWCBO        KEY
100040941212     C                   EXSR      CTABEL
100050920121     C*
100060941212    2C     *IN30         IFEQ      *ON
100070941102     C                   MOVEL     MSG(16)       VIDMSG
100080941102     C                   SETON                                        409028
100090941102     C                   GOTO      ENDCT5
100100941102    2C                   ENDIF
100110920117     C*
100120920116     C* CODICE NUOVO E VECCHIO NON POSSONO ESSERE UGUALI
100130940614     C* SE PGM RICHIAMATO NON CONTROLLO
100140941212    2C  N20OLDCBO        IFEQ      NEWCBO
100150941102     C                   MOVEL     MSG(17)       VIDMSG
100160941102     C                   SETON                                        409028
100170920116     C                   GOTO      ENDCT5
100180941102    2C                   END
100190920116     C*
100200920116     C                   MOVEL     TBLUNI        DS3A
100210920304     C*
100220941102     C                   MOVEL     §3ADES        DENCBO                          DESCRIZIONE
100230941102     C*
100240920116     C* NON POSSO TRASFORMARE IN BOLLA RECUPERO DA NON RECUPERO E VICEV
100250940322     C* SE RICHIAMATO PERMETTO TUTTO
100260941102    2C     *IN20         IFEQ      *OFF
100270940322     C*
100280941102    3C     §3ARBL        IFEQ      'R'
100290941102     C     SAVRBL        ANDNE     'R'
100300941102     C*
100310941102     C     §3ARBL        ORNE      'R'
100320941102     C     SAVRBL        ANDEQ     'R'
100330941102     C                   MOVEL     MSG(18)       VIDMSG
100340941102     C                   SETON                                        409028
100350920116     C                   GOTO      ENDCT5
100360941102    3C                   END
100370941102    2C                   END
100380920128     C*
100390941102     C* NON POSSO FARE VARIAZIONI DI PORTO
100400941212     C                   MOVEL     §3ATB1        NSCTB1            1
100410941212     C                   MOVEL     SAVTB1        OSCTB1            1
100420920116     C*
100430941102    2C     NSCTB1        IFNE      OSCTB1
100440941102     C                   MOVEL     MSG(19)       VIDMSG
100450941213     C                   SETON                                        409028
100460920128     C                   GOTO      ENDCT5
100470941102    2C                   ENDIF
100480150413     c* non ammesso contrasegno se bolle per EXPO
100490151119     c*                  if        §3afca=1 and (arbtc1='Z' or arbtc2='Z')
100500151119     C*                  MOVEL     MSG(159)      VIDMSG
100510151119     C*                  SETON                                        409028
100520151119     C*                  GOTO      ENDCT5
100530151119     c*                  endif
100540000510     C*
100550000510     C* BOLLE LNP POSTE: SOLO SPEDIZIONI FRANCIO
100560131008     C**   LNPNTW        IFEQ      'PPT'
100570131008     C**   NSCTB1        ANDNE     'F'
100580131008     C**                 MOVEL     MSG(30)       VIDMSG
100590131008     C**                 SETON                                        409028
100600131008     C**                 GOTO      ENDCT5
100610131008     C**                 ENDIF
100620000718     C* BOLLE LNP POSTE CON SERIE : NN POSSO TRASFRMARE IN C/S
100630131008     C**   LNPNTW        IFEQ      'PPT'
100640131008     C**   ARBNRS        ANDGT     0
100650131008     C**   §3ARBL        ANDEQ     'C'
100660131008     C**                 MOVEL     MSG(96)       VIDMSG
100670131008     C**                 SETON                                        409028
100680131008     C**                 GOTO      ENDCT5
100690131008     C**                 ENDIF
100700000925     C* BOLLE LNP POSTE: NON POSSO FARE BOLLE DOPPIE
100710131008     C**   LNPNTW        IFEQ      'PPT'
100720131008     C**   §3ATB2        ANDNE     '  '
100730131008     C**                 MOVEL     MSG(97)       VIDMSG
100740131008     C*+                 SETON                                        409028
100750131008     C**                 GOTO      ENDCT5
100760131008     C**                 ENDIF
100770050414     C* Se prepagato, non posso trasformare in C/S
100780050414     C     §3ARBL        ifeq      'C'
100790050414     c     war6          andeq     'E'
100800050414     c     ar6dft        andgt     0
100810060213     c     d48cvb        andne     'KM'
100820050414     C                   MOVEL     MSG(133)      VIDMSG
100830050414     C                   SETON                                        409028
100840050414     C                   GOTO      ENDCT5
100850050414     C                   ENDIF
100860950407     C*
100870950407     C* MEMORIZZO VARIAZIONE CODICE BOLLA
100880950407     C     NEWCBO        IFNE      SAVCBO
100890950407     C                   MOVE      *BLANKS       COM01A
100900950407     C                   MOVEL     NEWCBO        SAVCBO
100910950407     C                   END
100920941102    1C                   ENDIF
100930920120     C*
100940920121     C* CONTROLLO CONTRASSEGNO
100950941102    1C     §3AFCA        IFEQ      0
100960941213     C     VIDCAS        ANDNE     0
100970941213     C                   MOVEL     MSG(20)       VIDMSG
100980941213     C                   SETON                                        429028
100990941213     C                   GOTO      ENDCT5
101000941213    1C                   ENDIF
101010961204     C* NON POSSO INSERIRE IL C/ASSEGNO PER BOLLE DI RESO
101020961204    1C     §3AFCA        IFGT      0
101030980105     C     ARBFBR        IFEQ      'S'
101040961204     C                   MOVEL     MSG(73)       VIDMSG
101050961204     C                   SETON                                        429028
101060961204     C                   GOTO      ENDCT5
101070961204    1C                   ENDIF
101080980105     C* NON POSSO INSERIRE IL C/A SE BOLLE LEGATA CON LNP=LNA
101090980105     C  N21KARB          CHAIN     FNLBL01L                           30
101100980105    4C  N21*IN30         IFEQ      *OFF
101110980105     C     LBLLAP        ANDEQ     LBLLAN
101120980105     C                   MOVEL     MSG(78)       VIDMSG
101130980105     C                   SETON                                        429028
101140980105     C                   GOTO      ENDCT5
101150980105    1C                   ENDIF
101160980105     C                   ENDIF
101170941102     C*
101180941213     C* SE eliminato importo C/A, elimiare anche tutti i campi relativi
101190011018    1C     VIDCAS        IFEQ      0
101200011018    2C     VIDTIC        IFNE      *BLANKS
101210011018     C     VIDGCA        ORNE      '  '
101220011018     C                   MOVEL     MSG(56)       VIDMSG
101230011018     C                   SETON                                        419028
101240011018     C                   GOTO      ENDCT5
101250011018    2C                   END
101260011018    1C                   END
101270941102     C*
101280931118     C* IMPOSSIBILE ELIMINARE O VARIARE C/A SE GIA' INCASSATO
101290941220     C     SAVFCA        IFNE      0
101300941220     C     ARBICC        ANDNE     ' '
101310941220     C     ARBICC        ANDNE     'R'
101320941220     c*
101330941102    2C     VIDCAS        IFNE      AR9CAS
101340941102     C     VIDTIC        ORNE      AR9TIC
101350941102     C     VIDVCA        ORNE      AR9VCA
101360941102     C     VIDGCA        ORNE      AR9GCA
101370941102     C                   MOVEL     MSG(22)       VIDMSG
101380941102     C                   SETON                                        429028
101390931118     C                   GOTO      ENDCT5
101400941102    2C                   ENDIF
101410941102    1C                   ENDIF
101420920120     C*
101430920204     C* CAUSALE KB NON CONTROLLA ESISTENZA O MENO DEL C/A: OK TUTTO
101440941102    1C     VIDCVR        IFNE      'KB'
101450931227     C*
101460941102     C* SE NON RICHIAMATO NON POSSO USARE CERTI TIPI BOLLA
101470971229     C* NON CONTROLLO PER VARIAZIONE SOLO DI IMPORTO
101480971229     C  N21
101490971229    2CANN20§3AUCB        IFEQ      'N'
101500941102     C                   MOVEL     MSG(16)       VIDMSG
101510941102     C                   SETON                                        409028
101520931227     C                   GOTO      ENDCT5
101530941102    2C                   END
101540920204     C*
101550941102    2C     §3AFCA        IFNE      0
101560941212     C     VIDCAS        ANDEQ     0
101570941102     C                   MOVEL     MSG(21)       VIDMSG
101580941102     C                   SETON                                        429028
101590920120     C                   GOTO      ENDCT5
101600941102    2C                   END
101610941102    1C                   END
101620941213     C**
101630941213     C* SE IMMESSO IMPORTO C/A , CONTROLLO I CAMPI RELATIVI
101640941213     C**
101650941213    1C     VIDCAS        IFGT      0
101660950120     C* SE ACCESO INDICATORE DI VARIAZIONE C/ASS LO SALVO
101670950120     C     *IN95         IFEQ      *ON
101680950120     C                   MOVEL     '1'           WIN95             1
101690950120     C                   ENDIF
101700140515     c                   clear                   trulintds
101710140515     c                   eval      iinttip='B'
101720140515     c                   eval      iinttic=vidtic
101730140515     c                   eval      iintrscm=v1crsm
101740140515     c                   eval      iintrsco=arbrmo
101750140515     c                   eval      IINTAAS=d48aas
101760140515     c                   eval      IINTLNP=d48lnp
101770140515     c                   eval      IINTNRS=d48nrs
101780140515     c                   eval      IINTNSP=d48nsp
101790140515     c                   call      'TRULINTR'
101800140515     c                   parm                    trulintds
101810950120     C*
101820920204     C* CONTROLLO TIPO INCASSO C/A
101830060705     C* richiamo pgm TRULTICR
101840060705     c                   clear                   trulticds
101850060705     c                   clear                   ds1a
101860060705     c                   eval      iticTBO='A'
101870060705     c                   eval      iticlnp=arblnp
101880060705     c                   eval      iticlna=arblna
101890060705     c                   eval      iticlnantw=lnantw
101900060705     c                   eval      iticnzd=arbnzd
101910090707     c                   eval      iticprd=arbprd
101920090707     c                   eval      iticcad=arbcad
101930090707     c                   eval      iticlod=arblod
101940090707     c                   eval      iticind=arbind
101950090707     c                   eval      iticrsd=arbrsd
101960060705     c                   eval      iticdsp=arbdsp
101970130827    3C***  ARBCCM        IFGT      0
101980130827     C***                MOVEL     ARBCCM        iticccm
101990130827     C***                ELSE
102000130827     C***                MOVEL     ARBKSC        iticccm
102010130827    3C***                END
102020130827     c* in iticccm passo il MITTENTE: CCM se assegnato, KSC se franco
102030130827     c                   if        flgtb1='F'
102040130827     c                   z-add     arbksc        iticccm
102050130827     c                   else
102060130827     c                   z-add     arbccm        iticccm
102070130827     c                   if        iticccm=0
102080130827     c                   z-add     8888          iticccm
102090130827     c                   movel     arblnp        iticccm
102100130827     c                   endif
102110130827     c                   endif
102120140515     c* se il beneficiario del c/a è l'ordinante allora passo questo in iticccm
102130140515     c                   if        ointtipi='R'
102140140515     c                   move      ointcdi       iticccm
102150140515     c                   endif
102160060705     c                   eval      itictic=vidtic
102170060705     c                   eval      iticticbol=ar9tic
102180060705     c                   eval      iticdiv=vidvca
102190060705     c                   eval      iticcas=vidcas
102200060705     c                   eval      iticpgm='FNLR48R'
102210060705     c                   call      'TRULTICR'
102220060705     c                   parm                    TRULTICDS
102230060705     c                   parm                    ds1a
102240060705     c
102250060705     c                   if        oticric='T'
102260060705     c                   movel     otictic       vidtic
102270060705     C                   SETON                                        4190
102280060705     C                   GOTO      ENDCT5
102290060705     c                   endif
102300060705     c                   if        oticric='D'
102310060705     c                   movel     oticdiv       vidvca
102320060705     C                   SETON                                        4390
102330060705     C                   GOTO      ENDCT5
102340060705     c                   endif
102350060705     c* Errore tipo incasso
102360091211     c                   if        oticerr='T' or oticerr='C'
102370060705     C                   MOVEL     oticmsg       VIDMSG
102380060705     C                   SETON                                        419028
102390060705     C                   GOTO      ENDCT5
102400060705    2C                   ENDIF
102410060705     c* Errore divisa
102420060705     c                   if        oticerr='D'
102430060705     C                   MOVEL     oticmsg       VIDMSG
102440060705     C                   SETON                                        439028
102450060705     C                   GOTO      ENDCT5
102460060705    2C                   ENDIF
102470011010      *
102480941102     C*
102490941102     C* SE HO MODIFICATO LA VALUTA DEVO RIDIGITARE IL C/A
102500941102     C* INDICATORE DI CHANGE 95 ON
102510941213    2C     VARVCA        IFNE      VIDVCA
102520950120     C     WIN95         ANDEQ     *BLANKS
102530950120     C*
102540941219     C     VIDVCA        IFNE      *BLANKS
102550941219     C     VIDCAS        ORNE      0
102560941102     C                   MOVEL     MSG(28)       VIDMSG
102570941215     C                   SETON                                        429028
102580941102     C                   GOTO      ENDCT5
102590941213    2C                   ENDIF
102600941219    2C                   ENDIF
102610941213     C*
102620011206
102630011206      * pulisco il flag x la forzatura c/assegno forzabile
102640011206     c                   if        vidcas <> sav1_vidcas
102650011206     c                   clear                   forzacas
102660011206     c                   endif
102670011206      *-------------------
102680011206  2b c                   if        vidcas <> 0
102690011206     c                   eval      sav1_vidcas = vidcas
102700011206      * Controllo il c/assegno tramite il trul21r
102710011206     c                   clear                   trul21ds
102720011206     c                   eval      i21tla = 'L'
102730011206     c                   if        *in21 = *off
102740011206     c                   eval      i21cbo = newcbo
102750011206     c                   else
102760011206     c                   eval      i21cbo = arbcbo
102770011206     c                   endif
102780011206     c                   eval      i21tsp = arbtsp
102790011206     c                   eval      i21lnp = arblnp
102800011206     c                   eval      i21nzm = arbnzm
102810011206     c                   eval      i21lna = arblna
102820011206     c                   eval      i21nzd = arbnzd
102830130827     c*****              eval      i21ksc = arbksc
102840011206     c                   movel     arbctr        i21ctr
102850011206     c                   eval      i21tic = vidtic
102860011206     c                   eval      i21imp = vidcas
102870011206     c                   eval      i21div = vidvca
102880011206     c                   eval      i21ute = knmus
102890011206     c                   eval      i21pgm = nompgm
102900130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
102910130827     c                   if        flgtb1='F'
102920130827     c                   z-add     arbksc        i21ksc
102930130827     c                   else
102940130827     c                   z-add     arbccm        i21ksc
102950130827     c                   if        i21ksc=0
102960130827     c                   z-add     8888          i21ksc
102970130827     c                   movel     arblnp        i21ksc
102980130827     c                   endif
102990130827     c                   endif
103000011206     c                   call      'TRUL21R'
103010011206     c                   parm                    trul21ds
103020011206      * controllo che non sia minore del limite previsto
103030011206  3b c                   if        o21fmn <> *blanks
103040011206     c                   eval      vidmsg = msg(108)
103050011210     c                   eval      %subst (vidmsg:31:14) = (%editc(o21lmn: '4'))
103060011206     c                   eval      *in42 = *on
103070011206     c                   eval      *in90 = *on
103080011206     c                   eval      *in28 = *on
103090011206     c                   goto      endct5
103100011206  3e c                   endif
103110011206      * controllo che non sia maggiore del limite previsto
103120011206      * non forzabile
103130011206  3b c                   if        o21fx2 <> *blanks
103140011206     c                   eval      vidmsg = msg(91)
103150011210     c                   eval      %subst (vidmsg:47:14) = (%editc(o21lx2: '4'))
103160011206     c                   eval      *in42 = *on
103170011206     c                   eval      *in90 = *on
103180011206     c                   eval      *in28 = *on
103190011206     c                   goto      endct5
103200011206  3e c                   endif
103210050301      * controllo che non sia maggiore del limite per bancanri bartolini no
103220050301      * abilitati da sede, non forzabile
103230050301  3b c                   if        o21fb <> *blanks
103240050301     c                   eval      vidmsg = msg(132)
103250050301     c                   eval      %subst (vidmsg:26:14) = (%editc(o21lb: '4'))
103260050301     c                   eval      *in42 = *on
103270050301     c                   eval      *in90 = *on
103280050301     c                   eval      *in28 = *on
103290050301     c                   goto      endct5
103300050301  3e c                   endif
103310011206      * controllo che non sia maggiore del limite previsto
103320011206      * forzabile
103330011206  3b c                   if        o21fx1 <> *blanks and forzacas = *blanks
103340011206     c                   eval      forzacas = '1'
103350011206     c                   eval      vidmsg = msg(109)
103360011210     c                   eval      %subst (vidmsg:24:14) = (%editc(o21lx1: '4'))
103370011206     c                   eval      *in42 = *on
103380011206     c                   eval      *in90 = *on
103390011206     c                   eval      *in28 = *on
103400011206     c                   goto      endct5
103410011206  3e c                   endif
103420011206  2e c                   endif
103430011206
103440011120      *
103450011120      * Se bolla giacente e modifica 'KA' o 'KP' l'importo immesso (convertito) deve
103460011120      * essere uguale all'originario
103470011120  2b c                   if        *in04 = *ON  and (vidcvr = 'KA' or
103480011120     c                             vidcvr = 'KP')
103490011120     c                   z-add     vidcas        w0173
103500011120      * Cerco i decimali previsti per la divisa originaria
103510011120     c                   movel     'CV'          cod
103520011120     c                   movel(p)  ar9vca        key
103530011120     c                   exsr      ctabel
103540011120     c  n30              movel     tbluni        dscv
103550011120     c   30              clear                   dscv
103560011120      * Controlla la divisa
103570011120  3b c                   if        ar9vca <> vidvca
103580011120      * Converto il contrassegno inserito a video nella divisa originaria
103590030612     c                   clear                   YeurcoDS
103600011120     c                   movel     vidvca        yecdvi
103610011120     c                   movel     ar9vca        yecdvo
103620011120     c                   z-add     vidcas        yecimi
103630011120     c                   move      §cvdec        yecdco
103640011120     c                   call      'YEURCO'
103650030612     c                   parm                    YeurcoDS
103660011120  4b c                   if        yecesi = ' '
103670011120     c                   z-add     yecimo        w0173
103680011120  4e c                   endif
103690011120  3e c                   endif
103700011120      * Calcolo la differenza tra i 2 contrassegni
103710011120     c                   eval      wdiffe = (ar9cas - w0173)
103720011120     c                   mllzo     1             wdiffe
103730020103      * Converto la differenza nella divisa di conto
103740011120  3b c                   if        ar9vca <> §gedcn
103750030612     c                   clear                   YeurcoDS
103760020103     c                   movel     ar9vca        yecdvi
103770020103     c                   movel     §gedcn        yecdvo
103780011120     c                   z-add     wdiffe        yecimi
103790020103     c                   move      decsav        yecdco
103800011120     c                   call      'YEURCO'
103810030612     c                   parm                    YeurcoDS
103820011120  4b c                   if        yecesi = ' '
103830011120     c                   z-add     yecimo        wdiffe
103840011120  4e c                   endif
103850011120  3e c                   endif
103860020103      *
103870020103      * Se differenza maggiore del valore in tabella ERRORE
103880011120  3b c                   if        wdiffe > dfcsav
103890011120     c                   movel     msg(103)      vidmsg
103900011120     c                   seton                                        429028
103910011120     c                   goto      endct5
103920011120  3e c                   endif
103930011120  2e c                   endif
103940011120
103950991021     C**
103960991021     C                   MOVEL     VIDVCA        VARVCA
103970991021     C                   CLEAR                   WIN95
103980941102     c**
103990941102     C* PARTICOLARITA' C/ASSEGNO
104000941102     c**
104010941213    2C     VIDGCA        IFNE      *BLANKS
104020941102     C     VIDGCR        ORNE      *BLANKS
104030941102     C*
104040941102     C* non posso indicarla se NON IMMESSO C/A
104050941213    3C     VIDCAS        IFEQ      0
104060941102     C     VIDGCA        ANDNE     *BLANKS
104070941102     C                   SETON                                        449028
104080941102     C                   MOVEL     MSG(29)       VIDMSG
104090941102     C                   GOTO      ENDCT5
104100941213    3C                   ENDIF
104110941102     C*
104120941102     C* RICHIAMO IL PRG DI RICERCA
104130941213    3C     VIDGCR        IFEQ      '?'
104140030612     C                   CLEAR                   DS7pqrs
104150941102     C                   MOVEL     'S'           DS7RIC
104160941102     C                   MOVEL     'V'           DS7GES
104170941213    4C     ARBCCM        IFGT      0
104180941102     C                   MOVEL     ARBCCM        DS7KSC
104190941102     C                   ELSE
104200941102     C                   MOVEL     ARBKSC        DS7KSC
104210941213    4C                   ENDIF
104220941102     C                   MOVEL     VIDGCA        DS7COD
104230030612     C                   MOVEL     DS7pqrs       KPJBU
104240941102     C                   CALL      'TRTB72R'
104250941102     C                   PARM                    KPJBA
104260030612     C                   MOVEL     KPJBU         DS7pqrs
104270941102     C*
104280941102     C     VIDGCA        IFEQ      *BLANKS
104290941102     C                   MOVEL     DS7COD        VIDGCA
104300941213    3C                   ENDIF
104310941102     C*
104320941102     C                   CLEAR                   VIDGCR
104330941102     C*
104340941102     C                   SETON                                        90
104350941102     C                   GOTO      ENDCT5
104360941102    2C                   ENDIF
104370941102     C* CONTROLLO
104380030612     C                   CLEAR                   DS7pqrs
104390941102     C                   MOVEL     'S'           DS7RIC
104400941102     C                   MOVEL     'C'           DS7GES
104410941213    2C     ARBCCM        IFGT      0
104420941102     C                   MOVEL     ARBCCM        DS7KSC
104430941102     C                   ELSE
104440941102     C                   MOVEL     ARBKSC        DS7KSC
104450941102     C                   ENDIF
104460941102     C                   MOVEL     VIDGCA        DS7COD
104470030612     C                   MOVEL     DS7pqrs       KPJBU
104480941102     C                   CALL      'TRTB72R'
104490941102     C                   PARM                    KPJBA
104500030612     C                   MOVEL     KPJBU         DS7pqrs
104510941102     C* ERRORE
104520941213    3C     DS7ERR        IFEQ      '1'
104530941102     C                   MOVEL     DS7MSG        VIDMSG
104540941102     C                   SETON                                        449028
104550941102     C                   GOTO      ENDCT5
104560941213    3C                   ENDIF
104570941102     C*
104580941213    2C                   ENDIF
104590941213    1C                   ENDIF
104600011010      *-------------------
104610941220     C*
104620941220     c* SE MODIFICATO IK TIPO BOLLA ED ELIMINATA LA 2 MA GIA' INCASS
104630941220     C*  ERRORE
104640941220     C     §3ATB2        IFEQ      *BLANKS
104650941220     C*
104660941220     C     SAVTB2        ANDNE     *BLANKS
104670941220     C     ARBICA        ANDNE     ' '
104680941220     C     ARBICA        ANDNE     'R'
104690991007     C     DFT2BO        ANDGT     0
104700941220     C                   MOVEL     MSG(59)       VIDMSG
104710941220     C                   SETON                                        409028
104720941220     C                   GOTO      ENDCT5
104730941220     C                   END
104740950407     C* SE CAUSALE VARIAZIONE CODICE BOLLA
104750950407    1C     *IN21         IFEQ      *OFF
104760950407     C     COM01A        ANDEQ     *BLANKS
104770950407     C* E NON TROVATI ERRORI BLOCCANTI
104780950407     C     *IN90         ANDEQ     *OFF
104790950407     C* E ASSEGNATO
104800950407     C     FLGTB1        ANDEQ     'A'
104810950407     C* SE AGGIUNTO O TOLTO IL CONTRASSEGNO
104820950407    2C     SAVFCA        IFEQ      *ZEROS
104830950407     C     §3AFCA        ANDNE     *ZEROS
104840950407     C     SAVFCA        ORNE      *ZEROS
104850950407     C     §3AFCA        ANDEQ     *ZEROS
104860991007     C                   MOVEL     '1'           WTRC
104870991007     C     KARB2         CHAIN(N)  FIAR6000                           30
104880950407    3C     *IN30         IFEQ      *OFF
104890950407     C* SE GIA' TASSATO FINO ALL'IMPONIBILE
104900950407     C     AR6IMV        ANDNE     *ZEROS
104910950407     C                   SETON                                        2892
104920950407     C* ERRORE NON BLOCCANTE:MANCA VARIA G
104930950407    4C     §3AFCA        IFNE      *ZEROS
104940950407     C                   MOVEL     MSG(65)       VIDMSG
104950950407     C                   ELSE
104960950407     C* ERRORE NON BLOCCANTE:C'E' VARIA G
104970950407     C                   MOVEL     MSG(66)       VIDMSG
104980950407    4C                   END
104990950407    3C                   END
105000950407     C*
105010950407    2C                   END
105020950407     C                   MOVE      '1'           COM01A            1
105030950407    1C                   END
105040920116     C*
105050920116     C     ENDCT5        ENDSR
105060920116     C*
105070920116     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE KX -----------------*
105080920129     C     REGIS5        BEGSR
105090991001     C                   SETOFF                                       903839
105100130412     c                   clear                   wpda_ndc
105110130412     c                   clear                   wpda_ddc
105120130412     c                   clear                   wpda_dcm
105130071116     C
105140950104     C*  GIRO UDATE
105150950104     C                   TIME                    W0140            14 0
105160950104     C* UDATE IN GGMMAAAA
105170950104     C                   MOVE      W0140         WDTGIO            8 0
105180950104     C*
105190950104     C* UDATE IN AAAAMMGG
105200950104     C                   Z-ADD     WDTGIO        G02DAT
105210950104     C                   MOVEL     *BLANK        G02ERR
105220950104     C                   CALL      'XSRDA8'
105230950104     C                   PARM                    WLBDAT
105240950104     C                   MOVEL     G02INV        DATEU             8 0
105250941221     C* PRENDO I VALORI ASSOLUTI
105260941221     C                   MLLZO     1             VIDCAS
105270941221     C*
105280941213     C                   MOVEL     KNMUS         ARBPRU
105290941220     C                   MOVEL     ARBCPI        SAVCPI
105300070515     c
105310070515     c* Se modificato il C/A, devo allocarmi record "L" per il
105320070515     c*  blocco telefonata C/A
105330070515     c                   clear                   wesar4L           1
105340070515    1C     vidCAS        IFNE      SAVCAS
105350070515     C     vidTIC        ORNE      SAVTIC
105360070515     C     vidVCA        ORNE      SAVVCA
105370070807     C     vidcas        andgt     0
105380070515     c                   eval      Tipoela='C'
105390070515     c                   exsr      GestAR4L
105400070515    2c                   if        *in91
105410070515     C                   GOTO      ENDRG5
105420070515    2c                   endif
105430070515    1c                   endif
105440070515     c
105450070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
105460070515     c* iornare direttamente il file solo se cambiato tipo bolla
105470070515     c                   clear                   waltrabolla       1
105480130412     c                   setoff                                       3091
105490130412     c                   if        oldcbo<>newcbo
105500070515     C   10KARB          CHAIN     FNBLP01L                           3091
105510070515     C  n10KARB          CHAIN     FNARB01L                           3091
105520130412     c                   else
105530130412     c* questa chain mi serve per l'invio dati a PDA
105540131120     c                   if        d66pda<>' '
105550130412     C  n10KARB          CHAIN(n)  FNARB01L                           30
105560130412     c                   endif
105570130412     c                   endif
105580070515     c*
105590070515     c                   if        *in91
105600070515     C                   SETON                                        28
105610070515     C                   MOVEL     MSG(4)        VIDMSG
105620070515     c                   unlock    fiar401l
105630070515     C                   GOTO      ENDRG5
105640070515     c                   else
105650070515     c                   if        *in30
105660070515     c                   eval      waltrabolla='N'
105670070515     c                   else
105680130411     c  n10              eval      wpda_ndc=arbndc
105690130411     c  n10              eval      wpda_ddc=arbddc
105700130411     c  n10              eval      wpda_dcm=arbdcm
105710130412     c                   if        oldcbo<>newcbo
105720070515     C   10KARB          CHAIN     FNARB01L                           30
105730130412     c                   endif
105740131120     c                   if        oldcbo<>newcbo or d66pda<>' '
105750070515     C  N10KARB          CHAIN     FNBLP01L                           30
105760070515     c* Alla fine per non sporcarmi i campi del record, richaino il
105770070515     c*  record originale che sto variando
105780130411     c   10              eval      wpda_ndc=arbndc
105790130411     c   10              eval      wpda_ddc=arbddc
105800130411     c   10              eval      wpda_dcm=arbdcm
105810130412     c                   endif
105820070515     c                   endif
105830070515     c                   endif
105840130412     c*****              endif
105850920123     C*********
105860920116     C* INVIO PER SEDE
105870170116     C*****              MOVEL     'FNARBK0T'    SFILE
105880920116     C* ALLOCO OGGETTI
105890170116     C***                EXSR      ALLOC
105900170116     c***                if        *in91
105910170116     c***                unlock    fiar401l
105920170116     c** 10              unlock    fnblp01l
105930170116     c**n10              unlock    fnarb01l
105940170116     C**                 GOTO      ENDRG5
105950170116     c****               endif
105960930511     C*
105970941212     C* INVIO SOLO IN SEDE L'INDIRIZZO DEL MITTENTE PER C/A
105980941212     C* INVIO SOLO IN SEDE L'INDIRIZZO DEL DESTINATARIO SE DOPPIA BOLLA
105990930511     C                   MOVEL     'S'           §7LFFI
106000930511     C*
106010930210     C********* PER  C O N T R A S S E G N O
106020170116    1C**   AR9CAS        IFEQ      0
106030170116     C**   VIDCAS        ANDNE     0
106040170116     C**                 SETON                                        38
106050930210     C* INVIO PER SEDE
106060170116     C**                 MOVEL     'FNARBM0T'    SFILE
106070930210     C* ALLOCO OGGETTI
106080170116     C**                 EXSR      ALLOC
106090070515     c
106100170116     c***                if        *in91
106110170116     C**                 MOVEA     'FNARBK0T'    C21(13)
106120170116     C**                 MOVEA     VAR           C21(41)
106130170116     C**                 MOVEL     *BLANKS       COMMAN           80
106140170116     C**                 MOVEA     C21           COMMAN
106150170116     C**                 CALL      'QCMDEXC'                            H1
106160170116     C**                 PARM                    COMMAN
106170170116     C**                 PARM                    LUNG
106180170116     c**                 unlock    fiar401l
106190170116     c** 10              unlock    fnblp01l
106200170116     c**n10              unlock    fnarb01l
106210070515     c
106220170116     C**                 GOTO      ENDRG5
106230170116    1C**                 END
106240170116    1C**                 END
106250950324     C*
106260930210     C********* PER  D O P P I A    B O L L A
106270170116    1C**   SAVTB2        IFEQ      '  '
106280170116     C**   §3ATB2        ANDNE     '  '
106290170116     C**                 SETON                                        39
106300930210     C* INVIO PER SEDE
106310170116     C**                 MOVEL     'FNARBD0T'    SFILE
106320930210     C* ALLOCO OGGETTI
106330170116     C**                 EXSR      ALLOC
106340170116     c**                 if        *in91
106350170116     C**                 MOVEA     'FNARBK0T'    C21(13)
106360170116     C**                 MOVEA     VAR           C21(41)
106370170116     C**                 MOVEL     *BLANKS       COMMAN           80
106380170116     C**                 MOVEA     C21           COMMAN
106390170116     C**                 CALL      'QCMDEXC'                            H1
106400170116     C**                 PARM                    COMMAN
106410170116     C**                 PARM                    LUNG
106420070515     c
106430170116     C**                 MOVEA     'FNARBM0T'    C21(13)
106440170116     C**                 MOVEL     *BLANKS       COMMAN           80
106450170116     C**                 MOVEA     C21           COMMAN
106460170116     C**                 CALL      'QCMDEXC'                            H1
106470170116     C***                PARM                    COMMAN
106480170116     C**                 PARM                    LUNG
106490170116     c**                 unlock    fiar401l
106500170116     c** 10              unlock    fnblp01l
106510170116     c**n10              unlock    fnarb01l
106520170116     C**                 GOTO      ENDRG5
106530170116     C**                 ENDIF
106540170116     C**                 ENDIF
106550041022     C*********
106560041022     C                   Z-ADD     DATEU         ARBDTV
106570060320     C                   TIME                    ARBORV
106580920116     C* STORICIZZAZIONE ALL'ARRIVO
106590060130    1C     D66FSA        ifeq      'S'
106600920116     C                   MOVEL     VIDCVR        ARBCVB
106610941213     C                   MOVEL     OLDCBO        ARBCBP
106620941213     C                   MOVEL     NEWCBO        ARBCBN
106630941213     C                   MOVEL     AR9TIC        ARBTIC
106640941213     C                   MOVEL     AR9CAS        ARBCAS
106650941213     C                   MOVEL     AR9VCA        ARBVCA
106660941213     C                   MOVEL     AR9GCA        ARBGCA
106670011108      *
106680011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
106690011108      * è in partenza 'P' o in arrivo 'A'
106700011108     C     *IN10         IFEQ      *ON
106710011108     C                   MOVE      'A'           ARBPRU
106720011108     C                   ENDIF
106730011108     C     *IN10         IFEQ      *OFF
106740011108     C                   MOVE      'P'           ARBPRU
106750011108     C                   ENDIF
106760011108      *
106770941014     C                   WRITE     FNARBK00
106780930210     C                   END
106790060213     c* storicizzazione motivazione
106800060213     c     d66mtv        ifeq      'S'
106810060301     c                   exsr      sto_mtv
106820060213     c                   endif
106830920116     C*
106840920123     C*********
106850920117     C* I N V I O
106860941102     C                   MOVEL     NEWCBO        ARBCBN
106870941213     C* PER IL FILE DI VARIAZIONE
106880941102     C                   MOVEL     VIDCAS        ARBCAS
106890920116     C                   MOVEL     VIDTIC        ARBTIC
106900941212     C                   MOVEL     VIDGCA        ARBGCA
106910941213     C*
106920941215     C*
106930941212     C                   MOVEL     VIDVCA        ARBVCA
106940011017      * Se l'importo del contrassegno è 0 non devo scrivere la divisa
106950011017     C     VIDCAS        IFEQ      *ZEROS
106960011017     C                   CLEAR                   ARBVCA
106970011017     C                   ENDIF
106980920123     C*********
106990041022     C     D66FFI        ifeq      'S'
107000920117     C                   TIME                    ARBORV
107010920127     C                   MOVEL     VIDCVR        ARBCVB
107020170116     C**                 OPEN      FNARBK0T
107030941014     C                   WRITE     FNARBKTT
107040170116     C**                 CLOSE     FNARBK0T
107050920117     C* DISALLOCO
107060170116     C**                 MOVEA     'FNARBK0T'    C21(13)
107070170116     C**                 MOVEA     VAR           C21(41)
107080170116     C**                 MOVEL     *BLANKS       COMMAN           80
107090170116     C**                 MOVEA     C21           COMMAN
107100170116     C**                 CALL      'QCMDEXC'                            H1
107110170116     C**                 PARM                    COMMAN
107120170116     C**                 PARM                    LUNG
107130920212     C*
107140170116     C** 38              DO
107150170116     C**                 OPEN      FNARBM0T
107160170116     C**                 WRITE     FNARBMTT
107170170116     C**                 CLOSE     FNARBM0T
107180920212     C* DISALLOCO
107190170116     C**                 MOVEA     'FNARBM0T'    C21(13)
107200170116     C**                 MOVEA     C21           COMMAN
107210170116     C**                 CALL      'QCMDEXC'                            H1
107220170116     C**                 PARM                    COMMAN
107230170116     C**                 PARM                    LUNG
107240170116     C**                 END
107250920213     C*
107260170116     C** 39              DO
107270170116     C**                 MOVEL     *BLANKS       ARBCPI
107280170116     C**                 ADD       1             ARBORV
107290170116     C**                 MOVEL     VIDCVR        ARBCVB
107300170116     C**                 MOVEL     NEWCBO        ARBCBO
107310170116     C**                 OPEN      FNARBD0T
107320170116     C**                 WRITE     FNARBDTT
107330170116     C**                 CLOSE     FNARBD0T
107340920213     C* DISALLOCO
107350170116     C**                 MOVEA     'FNARBD0T'    C21(13)
107360170116     C**                 MOVEA     C21           COMMAN
107370170116     C**                 CALL      'QCMDEXC'                            H1
107380170116     C**                 PARM                    COMMAN
107390170116     C**                 PARM                    LUNG
107400170116     C**                 END
107410930210     C                   END
107420920117     C*
107430920117     C* A G G I O R N O   F I L E
107440920117     C*
107450051118     c* se variato imposto la data ultima variazione
107460091217    1c                   if        vidvca<>ar9vca or
107470051118     c                             ar9cas<>vidcas or
107480051118     c                             ar9vca<>vidvca or
107490051118     c                             ar9gca<>vidgca
107500051118     c                   movel     dateu         ar9duv
107510091217    1c                   endif
107520091217     c
107530091217     c* Se variato il tipo incasso C/Ass, lo memorizzo
107540140516     c* non lo memorizzo in caso di annullamento altrimenti su internet viene
107550140516     c* visualizzata sia la variazione che l'annullamento
107560140516    1c                   if        savtic<>vidtic and vidcas>0
107570091217     c                   eval      Wtipoagg='TIC'
107580091217     c                   exsr      RegisAR5GEN
107590091217    1c                   EndIf
107600091217     c
107610091217     c
107620941212     C                   MOVEL     NEWCBO        ARBCBO
107630941212     C                   MOVEL     VIDCAS        AR9CAS
107640941212     C                   MOVEL     VIDTIC        AR9TIC
107650941212     C                   MOVEL     VIDGCA        AR9GCA
107660941212     C                   MOVEL     VIDVCA        AR9VCA
107670011017      * Se l'importo del contrassegno è 0 non devo scrivere la divisa
107680011017     C     VIDCAS        IFEQ      *ZEROS
107690011017     C                   CLEAR                   AR9VCA
107700011017     C                   ENDIF
107710011019      *
107720941220     C                   MOVEL     ARBCPI        SAVCPI
107730920121     C*
107740941220     C* E L I M I N A T A   L A   2   B O L L A
107750941220    1C     SAVTB2        IFNE      '  '
107760920603     C     §3ATB2        ANDEQ     '  '
107770941220     C* PULISCO PARTITA IVA
107780930210     C                   MOVEL     *BLANKS       ARBCPI
107790991001     C* SE C'ERA AR6 SECONDA BOLLA LO DELETO
107800990930     C     WAR62         IFEQ      'E'
107810991008     C                   MOVEL     '2'           WTRC
107820170419     C     KARB2         CHAIN(N)  FIAR601l
107830170419     c                   if        %found(fiar601l) and ar6nft>0
107840170419     c                   exsr      sr_fnfta
107850170419     c                   endif
107860991008     C     KARB2         DELETE    FIAR6000                           30
107870941220     C                   ENDIF
107880941220     C*
107890941220     C* SE GIA' CONSEGNATA DO' PER INCASSATO L'ASSEGNATO
107900000925    2C     ARBDCM        IFGT      0
107910000925    3C     ARBICA        IFEQ      'R'
107920000925     C     ARBICA        OREQ      ' '
107930941220     C                   MOVEL     'Y'           ARBICA
107940000925    4C     ARBICC        IFNE      'R'
107950970613     C                   EXSR      CHIUBO
107960000925    5C     ARBNMI        IFNE      8888888
107970971023     C                   MOVEL     'S'           ARBICA
107980000925    5C                   ENDIF
107990000925    4C                   ENDIF
108000000925    3C                   ENDIF
108010000925    2C                   ENDIF
108020941220     C* ABILITO L'ASSEGNATO
108030941220     C                   MOVEL     'S'           ARBACA
108040941220    1C                   ENDIF
108050941220     C*
108060941220     C* A G G I U N T A   2   B O L L A
108070941220     C     SAVTB2        IFEQ      '  '
108080941220     C     §3ATB2        ANDNE     '  '
108090941220     C* PULISCO PARTITA IVA
108100941220     C                   MOVEL     *BLANKS       ARBCPI
108110941220     C*
108120941220     C                   MOVEL     ' '           ARBACA
108130941220     C                   MOVEL     ' '           ARBICA
108140941220     C* SE GIA' CONSEGNATA SI TRATTA DI UN RITORNO ALL'INCASSO
108150941220     C     ARBDCM        IFGT      0
108160941220     C                   MOVEL     'R'           ARBICA
108170941220     C                   ENDIF
108180941220     C                   ENDIF
108190920120     C*
108200920117     C* ABILITAZIONE CONTRASSEGNO
108210950918     C* SE NON HO VARIATO IL C/A RISPETTO A PRIMA, LASCIO ABILITAZIONE
108220950918     C*  E INCASSO C/A COME ERANO
108230950918    1C     AR9CAS        IFNE      SAVCAS
108240950918     C     AR9TIC        ORNE      SAVTIC
108250950918     C     AR9VCA        ORNE      SAVVCA
108260920120     C                   MOVEL     ' '           ARBACC
108270070320     c                   eval      §b4ltca='S'
108280070320     c
108290941220     C*
108300920117     C* SE NON C'E' PIU' IL C/ASSEGNO ABILITO
108310950918    2C     §3AFCA        IFEQ      0
108320950104     C     AR9CAS        OREQ      0
108330920117     C                   MOVEL     'S'           ARBACC
108340070320     c                   eval      §b4ltca=' '
108350960704     C*
108360960909     C* SE IL CONTRASSEGNO E' UN RITORNO ALL'INCASSO, LO TOLGO
108370960704     C* SE CONSEGNATO, E RITORNO ALL'INCASSO LO DO PER INCASSATO
108380960704    3C     ARBDCM        IFGT      0
108390960704     C     ARBICC        ANDEQ     'R'
108400960704     C                   MOVEL     'Y'           ARBICC
108410970613     C* SE L'ASSEGNATO NON E' UN RITORNO ALL'INCASSO VEDO SE CHIUDERE
108420970613     C* LA BOLLA
108430970613     C     ARBICA        IFNE      'R'
108440970613     C                   EXSR      CHIUBO
108450971023     C     ARBNMI        IFNE      8888888
108460971023     C                   MOVEL     'S'           ARBICC
108470971023     C                   ENDIF
108480970613     C                   ENDIF
108490970613     C                   ENDIF
108500960704     C*
108510950918   X2C                   ELSE
108520941220     C*
108530941220     C* SE LA BOLLA E' GIA' CONSEGNA METTO "R" DI RITORNO ALL'INCASSO
108540950918    3C     ARBDCM        IFGT      0
108550941220     C                   MOVEL     'R'           ARBICC
108560950918   X3C                   ELSE
108570941220     C* ALTRIMENTI PULISCO IL FLAG DI INCASSO C/A
108580941220     C                   CLEAR                   ARBICC
108590950918    3C                   ENDIF
108600011031      *
108610070320    3c                   if        o21fca = *blanks
108620020103     c                   eval      arbacc = 'S'
108630070320     c                   clear                   §b4ltca
108640070320    3c                   endif
108650070320    2C                   END
108660070320     c
108670070320     c* Aggiorno record di FIar4
108680070320     c                   eval      TipoEla='A'
108690070320     c                   exsr      GestAR4L
108700070320    1C                   END
108710071116     c*
108720071116     C* M O D I F I C A T O   S O L O   1 °   T I P O   B O L L A
108730071116    1C     SAVTB2        IFEQ      '  '
108740071116     C     §3ATB2        ANDEQ     '  '
108750071116     C* PRENDI IL TIPO BOLLA DEL NUOVO CODICE
108760071116     C                   MOVEL     'TB'          COD
108770071116     C                   MOVEL(P)  §3atb1        KEY
108780071116     C                   EXSR      CTABEL
108790071116     C  N30              MOVEL     TBLUNI        DSTB
108800071116     C   30              CLEAR                   DSTB
108810071116     c                   movel     §tbfcb        NEWtb1FCB         1
108820071116     c
108830071116     C* Se il nuovo tipo bolla è da non contabilizzare, imposto
108840071116     c*  la "S" di abilitazione Assgnato
108850071116    2c                   if        ARBACA=' ' and NEWtb1FCB='0'
108860071116     c                   eval      arbaca='S'
108870071116    2c                   endif
108880071116     c
108890071116     c* Se il nuovo tipo bolla è da contabilizzare, ma non lo era il
108900071116     c*  vecchio, vedo se devo togliere la "S" per l'abilitazione del C/A
108910071116    2c                   if        NEWtb1FCB='1' and Newtb1FCB<>OLDtb1FCB
108920071116     c                             and arbaca='S'
108930071116     C* TASSAZIONE IN FIAR6
108940071116     C                   MOVEL     '1'           WTRC
108950071116     C     KARB2         CHAIN(N)  FIAR6000
108960071116    3c                   if        (%subst(%editc(arbksc: 'X'):4:4)='9999') and
108970071116     c                             (not %found(FIar601l) or ar6dft=0)
108980071116     c                   eval      arbaca=' '
108990071116    3c                   endif
109000071116    2c                   endif
109010170418     c* se il nuovo tipo bolla non è da contabilizzare e il vecchio lo era
109020170418     c* memorizzo eventuale fattura stornata su file FNFTA00F
109030170418     C* TASSAZIONE IN FIAR6
109040170419     c* solo per bolle in assegnato
109050170418    2c                   if        NEWtb1FCB='0' and Newtb1FCB<>OLDtb1FCB
109060170419     c                             and *in06=*off
109070170418     C                   MOVEL     '1'           WTRC
109080170418     C     KARB2         CHAIN(N)  FIAR6000
109090170418     c                   if        %found(fiar601l) and ar6nft>0
109100170418     c                   exsr      sr_fnfta
109110170418     c                   endif
109120170418     c                   endif
109130071116     c
109140071116    1C                   ENDIF
109150170419
109160170419     C* M O D I F I C A T O   IL  2 °   T I P O   B O L L A
109170170419    1C     SAVTB2        IFne      '  '
109180170419     C     §3ATB2        ANDne     '  '
109190170419     c     savtb2        andne     §3atb2
109200170419     C* PRENDo IL TIPO BOLLA DEL NUOVO CODICE
109210170419     C                   MOVEL     'TB'          COD
109220170419     C                   MOVEL(P)  §3atb2        KEY
109230170419     C                   EXSR      CTABEL
109240170419     C  N30              MOVEL     TBLUNI        DSTB
109250170419     C   30              CLEAR                   DSTB
109260170419     c                   movel     §tbfcb        NEWtb2FCB         1
109270170419     c* se il nuovo tipo bolla non è da contabilizzare e il vecchio lo era
109280170419     c* memorizzo eventuale fattura stornata su file FNFTA00F
109290170419     C* TASSAZIONE IN FIAR6
109300170419     c* solo per bolle in assegnato
109310170419    2c                   if        NEWtb2FCB='0' and Newtb2FCB<>OLDtb2FCB
109320170419     C                   MOVEL     '2'           WTRC
109330170419     C     KARB2         CHAIN(N)  FIAR6000
109340170419     c                   if        %found(fiar601l) and ar6nft>0
109350170419     c                   exsr      sr_fnfta
109360170419     c                   endif
109370170419     c                   endif
109380170419     c                   endif
109390920116     C*
109400941216     C     §3AFCA        IFNE      0
109410950104     C     AR9CAS        ANDGT     0
109420941212     C     WAR9          IFEQ      ' '
109430941212     C                   MOVEL     ARBAAS        AR9AAS
109440941212     C                   MOVEL     ARBLNP        AR9LNP
109450941212     C                   MOVEL     ARBNRS        AR9NRS
109460941212     C                   MOVEL     ARBNSP        AR9NSP
109470051118     c                   movel     dateu         ar9duv
109480051118     C                   WRITE     FiAR9000
109490941212     C                   ELSE
109500051118     C                   UPDATE    FiAR9000
109510941212     C                   ENDIF
109520941216     C*
109530941216     C                   ELSE
109540941216     C* SE MI HANNO CANCELLATO L'AR9 --> DELETO
109550941216     C     WAR9          IFEQ      'E'
109560051118     C     KARB          DELETE    FiAR9000                           30
109570941216     C                   ENDIF
109580941216     C                   ENDIF
109590941212     C*
109600060213     C   10              UPDATE    FNARB000
109610041022     c* Se cambiato codice bolla aggiorno fnblp
109620041022     c                   if        waltrabollA=' ' AND oldcbo<>newcbo
109630041022     c                   eval      arbcbo=newcbo
109640041022     c   10              except    arbkblp
109650041022     c  N10              except    arbkarb
109660041022     c                   endif
109670040224
109680040224      * Se c'è la data consegna richiesta controllo se è ancora valida
109690040224      * solo se ho fatto una modifica in arrivo
109700160114     c                   If        ArbDcr > *Zeros and *In10 and wfile='A'
109710040324     c                             and wCa = *Blanks and wgiac = *Blanks
109720040224     c                   ExSr      ContrDcr
109730040224     c                   EndIf
109740950602     C*
109750130411     c* Richiamo routine per invio variazione a pda
109760130411     c                   exsr      sr_pda
109770950104     C*
109780920117     C     ENDRG5        ENDSR
109790911211     C*
109800911211     C*--- CARICO CODICI TARIFFA -------------------------------------*
109810911211     C     CARCTR        BEGSR
109820020305     c**
109830020305     C* Richiamo trul27r per reperimento WFIE e per reperimento tariffa di
109840020305     C* cartello
109850020305     c**
109860160225     c                   clear                   trul27ds1
109870021121     c                   Eval      I27Pkg = ArbPkf
109880020305     c                   eval      i27tsp = arbtsp
109890020305     c                   eval      i27lna = arblna
109900020305     c                   eval      i27lnp = arblnp
109910020305     C                   eval      i27cli = ksc
109920160225     c                   call      'TRUL27R1'
109930160225     c                   parm                    trul27ds1
109940020305     c                   eval      wfie = o27fie
109950020305     c*
109960941209     C                   MOVEL     *BLANKS       DE2CTR
109970050202     C                   CLEAR                   Fnlv59DS
109980941209     C*
109990050202     C                   MOVEL     D48TBO        ilv59tbo
110000040511      * SE BOLLA MONOVARIA NON PASSO IL TIPO BOLLA
110010040511     C                   IF        §3ASVA <> *BLANK
110020050202     C                   CLEAR                   ilv59tbo
110030050202     c                   eval      ilv59prp='P'
110040040511     C                   ENDIF
110050050202     C                   Z-ADD     ARBDSP        ilv59DSP
110060060131     C                   MOVEL     dutkci        ilv59KCI
110070050202     C                   Z-ADD     KSC           ilv59KSC
110080050202     C                   MOVEL     WFIE          ilv59FIE
110090160225     C                   MOVEL     WFIE          ilv59ta3
110100050202     C                   MOVEL     ARBTSP        ilv59TSP
110110050202     C                   MOVEL     VIDCTR        ilv59CTR
110120941209     C*
110130941209     C* SE NON C'E' RICERCA SU COD.TARIFFA E CAMBIATO CODICE CLIENTE
110140941209     C*  --> RICERCO LA TARIFFA
110150941209     C                   SETOFF                                       32
110160941209     C     '?'           SCAN      VIDCTR                                 32
110170941215     C     *IN32         IFEQ      *OFF
110180941215     C     KSC           ANDNE     SAVKSC
110190050202     C     VIDCTR        ANDEQ     SAVCTR
110200160225     C***                CLEAR                   ilv59CTR
110210941223     C                   CLEAR                   OTT
110220160225     C***                MOVEL     'S'           ilv59RIC
110230941215     C                   ELSE
110240060516     c* Se cod cliente e cod tariffa invariati rispetto alla bolla,
110250060516     c*  accetto codici tariffa bloccati (ma quando immessi validi)
110260060516     c                   if        not *in32 and ksc=kscbolla
110270060511     c                             and ctrbolla=vidctr
110280060516     C                   MOVEL     'B'           ilv59ta2
110290941209     C                   ENDIF
110300060511     C                   ENDIF
110310060516     C                   MOVEL     'S'           ilv59CON
110320050202     C
110330050202     C                   MOVEL     KSC           SAVKSC
110340160303     c* se ho abblancato il codice tariffa, lo faccio reimpostare dal pgm
110350160303     c                   if        ilv59ctr=*blanks
110360160303     C                   MOVEL     'S'           ilv59RIC
110370160303     c                   endif
110380941209     C*
110390050202     C                   CALL      'FNLV59R'
110400050202     C                   PARM                    Fnlv59DS
110410050202     c
110420941209     C*
110430941209     C* NON ESISTONO TARIFFE
110440050202     C                   MOVEL     olv59TKS      WTKS              1
110450050202     C     olv59TKS      IFNE      'S'
110460941209     C                   SETON                                        18
110470911219     C                   SETOFF                                       32
110480950114     C                   MOVEL     CNOTAR        DE2CTR
110490941209     C                   ELSE
110500941209     C*
110510941215     C* SE TARIFFE TROVATE,  DEVO REIMPOSTARE LA TARIFFA A VIDEO
110520050202     C     ilv59RIC      IFEQ      'S'
110530050202     C                   MOVEL     olv59CTR      VIDCTR
110540941209     C                   ENDIF
110550941215     C* DECODIFICA
110560050202     C                   MOVEL     olv59DFS      DE2CTR
110570941209     C                   ENDIF
110580050202     c
110590050202     C                   EVAL      SAVCTR=VIDCTR
110600911211     C*
110610911211     C                   ENDSR
110620911211     C*
110630920320     C*--- CONTROLLI CAMPI CONSEGNA RICHIESTA ------------------------*
110640920320     C     CONTR7        BEGSR
110650920320     C                   SETOFF                                       90
110660941102     c**
110670920320     C* CONSEGNA RICHIESTA
110680941102     C**
110690920320     C                   Z-ADD     0             COMDCR
110700920320     C     VIDDCR        IFNE      0
110710941102     C                   Z-ADD     VIDDCR        G02DAT
110720920320     C                   MOVEL     *BLANK        G02ERR
110730941102     C                   CALL      'XSRDA8'
110740920320     C                   PARM                    WLBDAT
110750941102     C* DATA FORMALMENTE ERRATA
110760920320     C     G02ERR        IFEQ      '1'
110770941102     C                   MOVEL     MSG(6)        VIDMSG
110780941102     C                   SETON                                        409028
110790920320     C                   GOTO      ENDCT7
110800920320     C                   END
110810941102     C*
110820941102     C                   Z-ADD     G02DAT        VIDDCR
110830941102     C*
110840920320     C* DATA CONSEGNA NO < DATA SPEDIZIONE
110850930507     C     G02INV        IFLT      ARBDSP
110860941102     C                   MOVEL     MSG(7)        VIDMSG
110870941102     C                   SETON                                        409028
110880920320     C                   GOTO      ENDCT7
110890930507     C                   END
110900941102     C                   Z-ADD     G02INV        COMDCR            8 0
110910941102     C                   ENDIF
110920070308     c
110930070308      * La data non può superare i gg limiti indicati in tab. VPO
110940070308     c                   If        Not *In90
110950070308     c                   Eval      wdataiso = wdataoggi
110960070308     c                   Adddur    §vpodcr:*d    wdataiso
110970070308     c     *iso          move      wdataiso      ds_data
110980070308     c                   Move      §vpodcr       w003a
110990070308     c                   Movea     w003a         ski(13)
111000070308     c                   Z-add     13            i
111010070308     c                   ExSr      abbl
111020070308     c                   Movea     ski(13)       w003a
111030070308     c                   If        ComDcr > ds_data
111040070308     c                   Eval      *In28 = *On
111050070308     c                   Eval      *In40 = *On
111060070308     c                   Eval      *In90 = *On
111070070308     c                   Eval      VidMsg = msg(129)
111080070308     c                   Eval      VidMsg = %replace(w003a:VidMsg:
111090070308     c                                      %scan('___':VidMsg))
111100070308     C                   GOTO      ENDCT7
111110070308     c                   EndIf
111120070308     c                   EndIf
111130141029     c* Se dcr>0 errore forzabile se presente Fermo Deposito
111140141029     c                   if        d48ffr<>'E'
111150141029     c                   if        comdcr>0 and comdcr<>arbdcr and
111160141029     c                             arbffd<>*blanks and wforzdcr=0
111170141029     C                   MOVEL     MSG(154)      VIDMSG
111180141029     C                   SETON                                        409028
111190141029     C                   EVAL      Wforzdcr=1
111200141029     C                   GOTO      ENDCT7
111210141029     c                   endif
111220141029     c                   endif
111230941102     C**
111240941102     C** ORA CONSEGNA RICHIESTA
111250941102     C**
111260920320     C                   MOVEL     VIDHCR        HM                2 0
111270920320     C     HM            IFLT      0
111280920320     C     HM            ORGT      24
111290941102     C                   MOVEL     MSG(8)        VIDMSG
111300941102     C                   SETON                                        419028
111310920320     C                   GOTO      ENDCT7
111320920320     C                   END
111330941102     C**
111340941102     C** MINUTI CONSEGNA RICHISTA
111350941102     C**
111360920320     C                   MOVE      VIDHCR        HM                2 0
111370920320     C     HM            IFLT      0
111380920320     C     HM            ORGT      59
111390941102     C                   MOVEL     MSG(8)        VIDMSG
111400941102     C                   SETON                                        419028
111410920320     C                   GOTO      ENDCT7
111420920320     C                   END
111430930507     C*
111440941102     C* SE IMMESSO IL TIPO IMMETTERE ANCHE L'ORA O LA DATA
111450930507     C     VIDTCR        IFNE      ' '
111460941102     C     VIDDCR        IFEQ      0
111470941102     C     VIDHCR        ANDEQ     0
111480941102     C                   MOVEL     MSG(9)        VIDMSG
111490941102     C                   SETON                                        409028
111500930507     C                   GOTO      ENDCT7
111510930507     C                   END
111520930507     C                   END
111530070308     c
111540070308     c* Se lna  attivata con GEO, non posso inserire solo tipo e ora:
111550070308     c*  data sempre obbligatoria (se inserito in arrivo, accetto
111560070308     c*  dalla partenza)
111570100419     c* Escludo per le causali di ripristino
111580070319     c                   if        FGSgeot='S' and viddcr=0 and vidhcr>0
111590100419     c                             and d48cvb <> 'CA'
111600070308     c                   if        vidtcr<>arbtcr or vidhcr<>arbhcr or
111610070308     c                             comdcr<>arbdcr
111620070308     C                   MOVEL     MSG(140)      VIDMSG
111630070308     C                   SETON                                        409028
111640070308     C                   GOTO      ENDCT7
111650070308     C                   ENDif
111660070308     C                   ENDif
111670930507     C**
111680941102     C* GIORNI DI CHIUSURA
111690941102     C**
111700930507     C     VIDGC1        IFNE      *BLANKS
111710930507     C                   MOVEL     VIDGC1        ACOM              1
111720930507     C                   MOVE      VIDGC1        BCOM              1
111730941212     C                   Z-ADD     1             B                 2 0
111740930507     C     ACOM          LOOKUP    GGG(B)                                 32
111750941102     C     *IN32         IFEQ      *OFF
111760941102     C                   MOVEL     MSG(10)       VIDMSG
111770941102     C                   SETON                                        429028
111780941102     C                   GOTO      ENDCT7
111790941102     C                   ENDIF
111800930507     C*
111810930507     C     BCOM          IFNE      ' '
111820930507     C     BCOM          ANDNE     'P'
111830930507     C     BCOM          ANDNE     'M'
111840941102     C                   MOVEL     MSG(10)       VIDMSG
111850941102     C                   SETON                                        429028
111860930507     C                   GOTO      ENDCT7
111870930507     C                   END
111880041130      * se modifica fatta in arrivo non posso mettere ' M' o ' P'
111890041130if  2c                   If         *In10 and VidGc1 <> ArbGc1 and
111900041130     c                             (VidGc1 = ' M' or VidGc1 = ' P')
111910041130     c                   Eval      *In28 = *On
111920041130     c                   Eval      *In42 = *On
111930041130     c                   Eval      *In90 = *On
111940041130     c                   Eval      VidMsg = msg(10)
111950041130e   2c                   EndIf
111960930507     C                   END
111970930507     C*
111980930507     C*** 2° GIORNO CHIUSURA ***
111990930507     C     VIDGC2        IFNE      *BLANKS
112000930507     C                   MOVEL     VIDGC2        ACOM              1
112010930507     C                   MOVE      VIDGC2        BCOM              1
112020930507     C                   Z-ADD     1             B
112030930507     C     ACOM          LOOKUP    GGG(B)                                 30
112040941102     C     *IN30         IFEQ      *OFF
112050941102     C                   MOVEL     MSG(10)       VIDMSG
112060941102     C                   SETON                                        439028
112070941102     C                   GOTO      ENDCT7
112080941102     C                   ENDIF
112090930507     C*
112100930507     C     BCOM          IFNE      ' '
112110930507     C     BCOM          ANDNE     'P'
112120930507     C     BCOM          ANDNE     'M'
112130941102     C                   MOVEL     MSG(11)       VIDMSG
112140941102     C                   SETON                                        439028
112150930507     C                   GOTO      ENDCT7
112160930507     C                   END
112170930507     C*
112180930507     C* I GIORNI DI CHIUSURA NON POSSO ESSERE UGUALI
112190930507     C     VIDGC1        IFEQ      VIDGC2
112200941102     C                   MOVEL     MSG(12)       VIDMSG
112210941102     C                   SETON                                        439028
112220930507     C                   GOTO      ENDCT7
112230930507     C                   END
112240930507     C* NON POSSONO ESSERE ' P' ' M'
112250930507     C     VIDGC1        IFEQ      ' M'
112260930507     C     VIDGC2        ANDEQ     ' P'
112270930507     C     VIDGC1        OREQ      ' P'
112280930507     C     VIDGC2        ANDEQ     ' M'
112290941102     C                   MOVEL     MSG(13)       VIDMSG
112300941102     C                   SETON                                        429028
112310930507     C                   GOTO      ENDCT7
112320930507     C                   END
112330041130      * se modifica fatta in arrivo non posso mettere ' M' o ' P'
112340041130if  2c                   If         *In10 and VidGc2 <> ArbGc2 and
112350041130     c                             (VidGc2 = ' M' or VidGc2 = ' P')
112360041130     c                   Eval      *In28 = *On
112370041130     c                   Eval      *In43 = *On
112380041130     c                   Eval      *In90 = *On
112390041130     c                   Eval      VidMsg = msg(10)
112400041130e   2c                   EndIf
112410930507     C                   END
112420941102     C*
112430941102     C* CONSEGNE PARTICOLARi
112440020603     c* Verifico se destinatario disagiato
112450020604     c* per le bolle poste non lo faccio
112460020604     c                   clear                   wdisag
112470020604     c     lnpntw        ifne      'PPT'
112480020603     c                   clear                   tisit0ds
112490020603     c                   movel     'E'           it0tla
112500020603     c                   movel     arbnzd        it0naz
112510020603     c                   movel     arbprd        it0prv
112520020603     c                   movel     arbcad        it0cap
112530020603     c                   movel     arblod        it0loc
112540020603     c                   movel     arbind        it0ind
112550020603     c                   movel     arbrsd        it0rag
112560020603     c                   call      'TISIT5R'
112570020603     c                   parm                    tisit0ds
112580020603     c     ot0err        ifeq      *blanks
112590060203   2ac                   if        ot0dos = 'A' or ot0dos = 'D' or
112600060203     c                             ot0dos = 'S'
112610020603     c                   movel     ot0dos        wdisag
112620020603     c     wdisag        ifeq      'D'
112630020603     c                   movel     'X'           wdisag
112640020603     c                   endif
112650060203     c                   endif
112660020603     c                   else
112670020603     c                   clear                   wdisag
112680020603     c                   endif
112690020604     c                   endif
112700020603     c
112710941102     C*
112720941102     C* 11ON - NON POSO CAMBIARE LA 1 CONSEGNA PARTICOLARE
112730070910     c* Se ho modificato il destinatario è non è stata immessa in
112740070910     c*  partenza, devo per forza toglierla
112750070910     c                   if        *in11=*on and wdisag<>vidtc1 and
112760070910     c                             arbtc1=vidtc1 and par5mdp='S' and
112770070910     c                             par5gtc1<>vidtc1 and par5gtc2<>vidtc1
112780070910     C                   MOVEL     MSG(144)      VIDMSG
112790070910     c                   eval      %subst(vidmsg:27:1) = vidtc1
112800070910     c                   eval      %subst(vidmsg:29:8) = v7dtc1
112810070910     C                   SETON                                        449028
112820070910     C                   GOTO      ENDCT7
112830070910     c                   endif
112840070910     c
112850051116     c****               if        *in11 = *on and wdisag <> *blanks
112860070910     c* se consegna particolare non modificabile ma è stato immesso
112870070910     c* unacons.part.disagiato/supemercat... do' lo stesso la
112880070910     c* possibilita di cambiarla se la combinazione fra le due non è
112890070910     c* ammessa
112900051116     c                   clear                   wcomb
112910070910    0c                   if        *in11=*on and par5mdp='S' and
112920070910     c                             (arbtc1=par5gtc1 or arbtc1=par5gtc2)
112930070910     c
112940070910    1c                   if        vidtc2<>*blanks or wdisag<>*blanks
112950070910     c                             or vidtc1<>arbtc1
112960070910     c*
112970070910     c***                if        *in11 = *on and (vidtc2 <> *blanks or
112980070910     c***                          wdisag <> *blanks or vidtc1 <> arbtc1)
112990051114     c                   move      arbtc1        wtc1
113000051117     c
113010051116     c                   if        wdisag <> *blanks
113020051116     c                   move      wdisag        wtc2
113030051117     c                   exsr      sr_chkcomb
113040051117     c                   endif
113050051117     c                   if        wcomb = *blanks and vidtc1 <> arbtc1
113060051117     c                   move      vidtc1        wtc2
113070051117     c                   exsr      sr_chkcomb
113080051117     c                   endif
113090051117     c                   if        wcomb = *blanks and vidtc2 <> *blanks
113100051117     c                   move      vidtc2        wtc2
113110051117     c                   exsr      sr_chkcomb
113120051117     c                   endif
113130070910    1c                   endif
113140070910     c***
113150070910    0C***  *IN11         IFEQ      *ON
113160070910     c***  wdisag        andeq     *blanks
113170070910    0c***                if        *in11 = *on and wcomb = *blanks
113180070910     c
113190070910    1c                   if        wcomb=*blanks
113200941102     C*
113210070910    2C     VIDTC1        IFNE      ARBTC1
113220941102     C                   MOVEL     MSG(15)       VIDMSG
113230051117     c                   eval      %subst(vidmsg:36:1) = arbtc1
113240941102     C                   SETON                                        449028
113250941102     C                   GOTO      ENDCT7
113260070910    2c                   endif
113270070910    1c                   endif
113280070910    0c                   endif
113290070910     c
113300070910     c***                else
113310070910     C***                MOVEL     '1P'          COD
113320070910     C***                MOVEL(P)  VIDTC1        KEY
113330070910     C***                EXSR      CTABEL
113340070910     C***                MOVEL     TBLUNI        V7DTC1
113350070910    1C***                ENDIF
113360941102     C*
113370070910   X0C***                ELSE
113380941102     C*
113390941102     C                   CLEAR                   V7DTC1
113400941102     C*
113410941102    1C     VIDTC1        IFNE      ' '
113420941102     C* RICERCA
113430941102    2C     VIDTC1        IFEQ      '?'
113440941102     C                   MOVEL     CODUT         §KUT
113450941102     C                   MOVEL     '1P'          §COD
113460941102     C                   MOVE      *BLANKS       VIDTC1
113470991011     C                   CLEAR                   §KEY
113480941102     C                   CALL      'X§TABER'
113490941102     C                   PARM                    §KUT
113500941102     C                   PARM                    §COD
113510941102     C                   PARM                    §KEY
113520941209     C                   PARM                    §DES
113530941102     C                   MOVEL     §KEY          VIDTC1
113540941102     C                   MOVEL     §DES          V7DTC1
113550941102     C                   SETON                                        9044
113560941102     C                   GOTO      ENDCT7
113570941102    2C                   ENDIF
113580941102     C*
113590941102     C                   MOVEL     '1P'          COD
113600941102     C                   MOVEL(P)  VIDTC1        KEY
113610941212     C                   EXSR      CTABEL
113620941102     C*
113630941102    2C     *IN30         IFEQ      *ON
113640941102     C                   MOVEL     MSG(11)       VIDMSG
113650941102     C                   SETON                                        449028
113660941102     C                   GOTO      ENDCT7
113670941102    2C                   ENDIF
113680941102     C*
113690941102     C                   MOVEL     TBLUNI        DS1P
113700941102     C*
113710941102     C* NON UTILIZZABILE IN ARRIVO
113720950104    2C   10§1PUAR        IFNE      'S'
113730020318     C     LNPNTW        ANDNE     'PPT'
113740020318     C     LNPNTW        OREQ      'PPT'
113750000510     C     §1PFPC        ANDNE     'E'
113760000510     C     §1PFPC        ANDNE     'A'
113770040705      * e non è quella presa in automatico
113780051114     c                   If        Vidtc1 <> wdisag and vidtc1 <> arbtc1
113790070914     c* Verifico se era stata messa in partenza ma tolta
113800070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
113810070914     c                             <>vidtc1 and par5gtc2<>vidtc1)
113820941102     C                   MOVEL     MSG(11)       VIDMSG
113830941102     C                   SETON                                        449028
113840941102     C                   GOTO      ENDCT7
113850040705     c                   EndIf
113860070914     c                   EndIf
113870941102    2C                   END
113880060131     c
113890060131     c* SE devo considerare le abilitati, verifico se il profilo
113900060131     c*  può inserire la cons particolare, se non è presa in automatico
113910060131     c   10              If        Vidtc1 <> wdisag and vidtc1 <> arbtc1
113920070914     c                             and  §utemfcp=' '
113930070914     c******                       and dateu>=§vpoacp and  §utemfcp=' '
113940070914     c*
113950070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
113960070914     c                             <>vidtc1 and par5gtc2<>vidtc1)
113970060131     C*
113980060131     C                   MOVEL     'SP'          COD
113990060131     C                   MOVEL(P)  VIDTC1        KEY
114000060131     C                   EXSR      CTABEL
114010060131     C*
114020060131    2C     *IN30         IFEQ      *ON
114030060131     c                   clear                   dssp
114040060131     c                   else
114050060131     c                   movel     tbluni        DSSP
114060060131     c                   endif
114070060131     c
114080060131     c                   if        §spaba='S'
114090060131     C                   MOVEL     MSG(138)      VIDMSG
114100060131     c                   eval      %subst(vidmsg:59:1) = vidtc1
114110060131     C                   SETON                                        449028
114120060131     C                   GOTO      ENDCT7
114130060131     c                   endif
114140060131     c                   endif
114150070914     c
114160070914     c                   endif
114170060131     c
114180950104     C* NON UTILIZZABILE IN PARTENZA
114190950104    2C  N10§1PUPR        IFNE      'S'
114200020318     C     LNPNTW        ANDNE     'PPT'
114210020318     C     LNPNTW        OREQ      'PPT'
114220000510     C     §1PFPC        ANDNE     'E'
114230000510     C     §1PFPC        ANDNE     'P'
114240950104     C                   MOVEL     MSG(11)       VIDMSG
114250950104     C                   SETON                                        449028
114260950104     C                   GOTO      ENDCT7
114270950104    2C                   END
114280000510     C* PER LE POSTE CONTROLLO IL SUO FLAG
114290941102     C*
114300941102     C                   MOVEL     TBLUNI        V7DTC1
114310150414     c* controlli per EXPO
114320151119     c*                  if        vidtc1='Z'
114330151119     c*                  exsr      sr_chkexpo
114340151119     c*                  if        *in90
114350151119     c*                  seton                                        44
114360151119     c*                  goto      endct7
114370151119     c*                  endif
114380151119     c*                  endif
114390941102     C*
114400941102     C* LA 1 E LA 2 NON POSSONO ESSERE UGUALI
114410941102    2C     VIDTC2        IFEQ      VIDTC1
114420941102     C                   MOVEL     MSG(14)       VIDMSG
114430941102     C                   SETON                                        459028
114440941102     C                   GOTO      ENDCT7
114450941102    2C                   END
114460020731     C*
114470941102     C*
114480941102    1C                   END
114490070910    0C******             END
114500941102     C*
114510941102     C* 12ON - NON POSO CAMBIARE LA 2 CONSEGNA PARTICOLARE
114520070910     c* Se ho modificato il destinatario è non è stata immessa in
114530070910     c*  partenza, devo per forza toglierla
114540070910    0c                   if        *in12=*on and wdisag<>vidtc2 and
114550070910     c                             arbtc2=vidtc2 and par5mdp='S' and
114560070910     c                             par5gtc1<>vidtc2 and par5gtc2<>vidtc2
114570070910     C                   MOVEL     MSG(144)      VIDMSG
114580070910     c                   eval      %subst(vidmsg:27:1) = vidtc2
114590070910     c                   eval      %subst(vidmsg:29:8) = v7dtc2
114600070910     C                   SETON                                        459028
114610070910     C                   GOTO      ENDCT7
114620070910    0c                   endif
114630070910     c
114640051114     c* se consegna particolare non modificabile ma è stato immesso
114650051114     c* un destinatrio disagiato/supemercat... do' lo stesso la
114660051114     c* possibilita di cambiarla se la combinazione fra le due non è
114670051114     c* ammessa
114680051116     c                   clear                   wcomb
114690070910    0c                   if        *in12=*on and par5mdp='S' and
114700070910     c                             (arbtc2=par5gtc1 or arbtc2=par5gtc2)
114710070910
114720070910    1c                   if        vidtc1 <> *blanks or
114730070910     c                             wdisag <> *blanks or vidtc2 <> arbtc2
114740070910     c
114750051116     c***                if        *in12 = *on and wdisag <> *blanks
114760070910     c***
114770070910     c***                if        *in12 = *on and (vidtc1 <> *blanks or
114780070910     c***                          wdisag <> *blanks or vidtc2 <> arbtc2)
114790051114     c                   move      arbtc2        wtc1
114800051116     c                   if        wdisag <> *blanks
114810051116     c                   move      wdisag        wtc2
114820051117     c                   exsr      sr_chkcomb
114830051117     c                   endif
114840051117     c                   if        wcomb = *blanks and vidtc2 <> arbtc2
114850051117     c                   move      vidtc2        wtc2
114860051117     c                   exsr      sr_chkcomb
114870051117     c                   endif
114880051117     c                   if        wcomb = *blanks and vidtc1 <> *blanks
114890051117     c                   move      vidtc1        wtc2
114900051117     c                   exsr      sr_chkcomb
114910051117     c                   endif
114920070910    1c                   endif
114930051117     c*
114940070910    0C***  *IN12         IFEQ      *ON
114950070910    0c***                if        *in12 = *on and wcomb = *blanks
114960070910    1c                   if        wcomb = *blanks
114970941102     C*
114980070910    2C     VIDTC2        IFNE      ARBTC2
114990941102     C                   MOVEL     MSG(15)       VIDMSG
115000051117     c                   eval      %subst(vidmsg:36:1) = arbtc2
115010941102     C                   SETON                                        459028
115020941102     C                   GOTO      ENDCT7
115030070910    2c                   endif
115040070910    1c                   endif
115050070910    0c                   endif
115060070910     c
115070070910     c****               else
115080070910     C****               MOVEL     '1P'          COD
115090070910     C****               MOVEL(P)  VIDTC2        KEY
115100070910     C****               EXSR      CTABEL
115110070910     C****               MOVEL     TBLUNI        V7DTC2
115120070910    1C****               ENDIF
115130941102     C*
115140070910   X0C****               ELSE
115150941102     C*
115160941102     C                   CLEAR                   V7DTC2
115170941102     C*
115180941102    1C     VIDTC2        IFNE      ' '
115190941102     C* RICERCA
115200941102    2C     VIDTC2        IFEQ      '?'
115210941102     C                   MOVEL     CODUT         §KUT
115220941102     C                   MOVEL     '1P'          §COD
115230941102     C                   MOVE      *BLANKS       VIDTC2
115240991011     C                   CLEAR                   §KEY
115250941102     C                   CALL      'X§TABER'
115260941102     C                   PARM                    §KUT
115270941102     C                   PARM                    §COD
115280941102     C                   PARM                    §KEY
115290941209     C                   PARM                    §DES
115300941102     C                   MOVEL     §KEY          VIDTC2
115310941102     C                   MOVEL     §DES          V7DTC2
115320941102     C                   SETON                                        9045
115330941102     C                   GOTO      ENDCT7
115340941102    2C                   ENDIF
115350941102     C*
115360941102     C                   MOVEL     '1P'          COD
115370941102     C                   MOVEL(P)  VIDTC2        KEY
115380941212     C                   EXSR      CTABEL
115390941102     C*
115400941102    2C     *IN30         IFEQ      *ON
115410941102     C                   MOVEL     MSG(11)       VIDMSG
115420941102     C                   SETON                                        459028
115430941102     C                   GOTO      ENDCT7
115440941102    2C                   ENDIF
115450941102     C*
115460941102     C                   MOVEL     TBLUNI        DS1P
115470941102     C*
115480941102     C* NON UTILIZZABILE IN ARRIVO
115490950104    2C   10§1PUAR        IFNE      'S'
115500020318     C     LNPNTW        ANDNE     'PPT'
115510020318     C     LNPNTW        OREQ      'PPT'
115520000518     C     §1PFPC        ANDNE     'E'
115530000518     C     §1PFPC        ANDNE     'A'
115540040705      * e non è quella presa in automatico
115550051114     c                   If        Vidtc2 <> wdisag  and vidtc2 <> arbtc2
115560070914     c* Verifico se era stata messa in partenza ma tolta
115570070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
115580070914     c                             <>vidtc2 and par5gtc2<>vidtc2)
115590941102     C                   MOVEL     MSG(11)       VIDMSG
115600941102     C                   SETON                                        459028
115610941102     C                   GOTO      ENDCT7
115620040705     c                   EndIf
115630070914     c                   EndIf
115640941102    2C                   END
115650060131     c
115660060131     c* SE devo considerare le abilitati, verifico se il profilo
115670060131     c*  può inserire la cons particolare, se non è presa in automatico
115680060131     c   10              If        Vidtc2 <> wdisag and vidtc2 <> arbtc2
115690070914     c                             and  §utemfcp=' '
115700070514     c****                         and dateu>=§vpoacp and  §utemfcp=' '
115710070914     c* Verifico se era stata messa in partenza ma tolta
115720070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
115730070914     c                             <>vidtc2 and par5gtc2<>vidtc2)
115740060131     C*
115750060131     C                   MOVEL     'SP'          COD
115760060131     C                   MOVEL(P)  VIDTC2        KEY
115770060131     C                   EXSR      CTABEL
115780060131     C*
115790060131    2C     *IN30         IFEQ      *ON
115800060131     c                   clear                   dssp
115810060131     c                   else
115820060131     c                   movel     tbluni        DSSP
115830060131     c                   endif
115840060131     c
115850060131     c                   if        §spaba='S'
115860060131     C                   MOVEL     MSG(138)      VIDMSG
115870060131     c                   eval      %subst(vidmsg:59:1) = vidtc1
115880060131     C                   SETON                                        459028
115890060131     C                   GOTO      ENDCT7
115900060131     c                   endif
115910060131     c                   endif
115920070914     c                   endif
115930070914     c
115940950104     C* NON UTILIZZABILE IN PARTENZA
115950950104    2C  N10§1PUPR        IFNE      'S'
115960020318     C     LNPNTW        ANDNE     'PPT'
115970020318     C     LNPNTW        OREQ      'PPT'
115980000518     C     §1PFPC        ANDNE     'E'
115990000518     C     §1PFPC        ANDNE     'P'
116000950104     C                   MOVEL     MSG(11)       VIDMSG
116010950104     C                   SETON                                        459028
116020950104     C                   GOTO      ENDCT7
116030950104    2C                   END
116040941102     C*
116050941102     C                   MOVEL     TBLUNI        V7DTC2
116060150414     c* controlli per EXPO
116070151119     c*                  if        vidtc2='Z'
116080151119     c*                  exsr      sr_chkexpo
116090151119     c*                  if        *in90
116100151119     c*                  seton                                        45
116110151119     c*                  goto      endct7
116120151119     c*                  endif
116130151119     c*                  endif
116140941102     C*
116150941102    1C                   END
116160070910    0C****               END
116170060131     c
116180020531     c* Se destinatario disagiato o supermercato errore se non indicato
116190020531     c* in nessuno dei due campi di consegna particolare
116200041015     c*
116210041015     c* Se particolarità varia="U" --> consegna ad uffici devo invece
116220041015     c* dare errore se immesso supermercato o appuntamento
116230041015     c                   if        %subst(arbgva:1:1)='U'
116240041015     c                             and (vidtc1='S' or vidtc1='A' or vidtc2='S'
116250041015     c                             or vidtc2='A')
116260041015     C                   MOVEl     msg(131)      vidmsg
116270041015     C                   SETON                                        449028
116280041015     C                   GOTO      ENDCT7
116290041015     c                   endif
116300150210
116310150210     c* se sono in partenza e immessa la "A" e la data --> errore solo la data immettere
116320150210     c                   if        not *in10 and (vidtc1='A' or vidtc2='A') and
116330150210     c                             comdcr>0
116340150210     c                   if        (par5mdp='S' and par5gtc1<>'A' and par5gtc2
116350150210     c                                                    <> 'A')
116360150210     c                             or (par5mdp=' ' and arbtc1<>'A' and arbtc2
116370150210     c                                                    <> 'A')
116380150210     C                   MOVEl     msg(158)      vidmsg
116390150210     C                   SETON                                        449028
116400150210     C                   GOTO      ENDCT7
116410150210     c                   endif
116420150210     c                   endif
116430150119
116440150119     c* se destinatario particolare che prevede appuntamento controllo se presente
116450150119     c* DCR immessa dalla partenza perchè in questo caso non devo dare errore per la
116460150119     c* mancanza della "A" di appuntamento
116470150119     c                   eval      w_noapp=' '
116480150119     c                   if        wdisag='A'
116490150119     c                   if        (par5mdp='S' and par5gdcr>*zeros
116500150119     c                                          and par5gtcr=' ')
116510150119     c                             or (par5mdp=' ' and arbdcr>0 and arbtcr= ' ')
116520150119     c                   eval      w_noapp='1'
116530150119     c                   endif
116540150119     c                   endif
116550041015     c*
116560041015     C***  wdisag        ifne      *blanks
116570041015     c***  wdisag        andne     vidtc1
116580041015     c***  wdisag        andne     vidtc2
116590150119     c                   if        wdisag <> *blanks and w_noapp=' ' and
116600041015     c                             %subst(arbGva:1:1) <> 'U' or
116610041015     c                             wdisag='X' and %subst(arbGva:1:1)='U'
116620041015     c     wdisag        ifne      vidtc1
116630041015     c     wdisag        andne     vidtc2
116640020603     C                   MOVEa     msg(112)      k78
116650020531     c                   select
116660020531     c     wdisag        wheneq    'X'
116670020531     c                   eval      w014a = 'disagiato     '
116680020531     c     wdisag        wheneq    'S'
116690020531     c                   eval      w014a = '"supermercato"'
116700040705     c     wdisag        wheneq    'A'
116710040705     c                   eval      w014a = '"appuntamento"'
116720020531     c                   endsl
116730020531     C                   MOVEa     w014a         k78(14)
116740020531     C                   MOVEA     K78           VIDMSG
116750020603     C                   SETON                                        449028
116760020531     C                   GOTO      ENDCT7
116770020531     c                   endif
116780041015     c                   endif
116790070907     C* Non è possibile indicare contemporaneamente alcune consegne    i
116800070907     C*  particolari
116810051114     c                   move      vidtc1        wtc1
116820051114     c                   move      vidtc2        wtc2
116830051114     c                   exsr      sr_chkcomb
116840051114     c                   if        wcomb = 'S'
116850051114     C                   movel     MSG(113)      VIDmsg
116860051116     c                   eval      %subst(vidmsg:63:1) = vidtc1
116870051116     c                   eval      %subst(vidmsg:69:1) = vidtc2
116880051114     C                   seton                                        459028
116890051114     C                   goto      ENDCT7
116900051114    2C                   ENDIF
116910941102     C*
116920920320     C     ENDCT7        ENDSR
116930021129      **
116940021129      *--- CONTROLLI CAMPI CONSEGNA RICHIESTA SUPERMERCATI -----------*
116950021129     C     CONTR71       BEGSR
116960021129     C                   SETOFF                                       90
116970021129
116980030123      * Se la spedizione non ha già la consegna particolare 'S'
116990030123     c                   If        ArbTc1 <> 'S' and ArbTc2 <> 'S'
117000030123      * E' obbligatorio inserire la consegna
117010030123     c                   If        (VidTc1 = *Blanks and VidTc2 = *Blanks)
117020030123     c                   Eval      VidMsg = Msg(117)
117030030123     c                   Eval      *In44 = *On
117040030123     c                   Eval      *In28 = *On
117050030123     c                   Eval      *In90 = *On
117060030123     c                   GoTo      EndCt71
117070030123     c                   EndIf
117080030127      * Se ci sono già tutte e 2 almeno una deve essere 'S'
117090030127     c                   If        VidTc1 <> 'S' and VidTc2 <> 'S'
117100030127     c                   Eval      VidMsg = Msg(122)
117110030127     c                   Eval      *In44 = *On
117120030127     c                   Eval      *In28 = *On
117130030127     c                   Eval      *In90 = *On
117140030127     c                   GoTo      EndCt71
117150030127     c                   EndIf
117160030123      * Se la prima consegna c'è già e non è supermercato devo mettere 'S' nella seconda
117170030127     c                   If        ArbTc1 <> *Blanks and VidTc1 <> 'S' and
117180030127     c                             VidTc2 = *Blanks
117190030123     c                   Eval      VidMsg = Msg(117)
117200030123     c                   Eval      *In45 = *On
117210030123     c                   Eval      *In28 = *On
117220030123     c                   Eval      *In90 = *On
117230030123     c                   GoTo      EndCt71
117240030123     c                   EndIf
117250030123      * Se la seconda consegna c'è già e non è supermercato devo mettere 'S' nella prima
117260030127     c                   If        ArbTc2 <> *Blanks and VidTc2 <> 'S' and
117270030127     c                             VidTc1 = *Blanks
117280030123     c                   Eval      VidMsg = Msg(117)
117290030123     c                   Eval      *In45 = *On
117300030123     c                   Eval      *In28 = *On
117310030123     c                   Eval      *In90 = *On
117320030123     c                   GoTo      EndCt71
117330030123     c                   EndIf
117340021129     c                   EndIf
117350021129
117360021129      * Se è stata inserita la consegna particolare 'S' si devono
117370021129      * immettere anche la persona da contattare, il telefono e il motivo
117380021129     c                   If        VidTc1 = 'S' or VidTc2 = 'S'
117390021129      * Nominativo
117400021129     c                   If        VidNom = *Blanks
117410021129     c                   Eval      VidMsg = Msg(118)
117420021129     c                   Eval      *In57 = *On
117430021129     c                   Eval      *In28 = *On
117440021129     c                   Eval      *In90 = *On
117450021129     c                   GoTo      EndCt71
117460021129     c                   EndIf
117470021129      * Telefono
117480021129     c                   If        VidTel = *Blanks
117490021129     c                   Eval      VidMsg = Msg(119)
117500021129     c                   Eval      *In58 = *On
117510021129     c                   Eval      *In28 = *On
117520021129     c                   Eval      *In90 = *On
117530021129     c                   GoTo      EndCt71
117540041007     c**                 Else
117550041007     c**                 Clear                   Trul42Ds
117560041007     c**                 Movel     VidTel        D42Fax
117570041007     c**                 Call      'TRUL42R'
117580041007     c***                Parm                    Trul42Ds
117590041007     c***                If        D42Err = '1'
117600041007     c****               Eval      *In58 = *On
117610041007     c**                 Eval      *In28 = *On
117620041007     c**                 Eval      *In90 = *On
117630041007     c**                 Eval      VidMsg = D42Msg
117640041007     c**                 GoTo      EndCt71
117650041007     c**                 EndIf
117660041007     c                   EndIf
117670021129      * Motivo
117680070406      * non più obbligatorio per evitare che scrivano "bagianate"
117690070406     c**                 If        VidMot = *Blanks
117700070406     c***                Eval      VidMsg = Msg(120)
117710070406     c**                 Eval      *In59 = *On
117720070406     c**                 Eval      *In28 = *On
117730070406     c**                 Eval      *In90 = *On
117740070406     c**                 GoTo      EndCt71
117750070406     c**                 EndIf
117760021129     c                   EndIf
117770021129
117780021129     c     EndCt71       EndSr
117790030930      **
117800030930      *--- CONTROLLO SE POSSIBILE LA MANUTENZIONE DELLA DATA CONSEGNA RICHIESTA ---*
117810030930     c     Contr72       BegSr
117820050907
117830030930     c                   Eval      *In90 = *Off
117840070306     c                   clear                   aggioarp          1
117850050907
117860030930      * Se tipo o data o ora consegna richiesta sono state variate
117870031001If  1c                   If        VidTcr <> ArbTcr or
117880031030     c                             ComDcr <> ArbDcr or
117890030930     c                             VidHcr <> ArbHcr
117900150205     c
117910150205     c* permetto sempre la variazione della cons richiesta se sono in partenza
117920160115     c                   if        not *in10 or wfile='P'
117930150205     c                   Goto      EndCt72
117940150205     c                   EndIf
117950150205     c
117960051109      * Se causale 'CS' e giacenza aperta in fase 30 con appuntamento (sulla giacenza)
117970051109      * posso variare la data
117980051109      * quindi bypasso tutti gli altri controlli
117990051109     c                   If        d48cvb = 'CS' and *In04 and wgcfas = 30 and
118000051109     c                             d§gcpapp = 'A'
118010051109     c                   Eval      wgiac = 'S'
118020051109     c                   Goto      EndCt72
118030051109     c                   EndIf
118040040224      * non posso modificare se la bolla ha giacenza aperta (IN04 ON)
118050050907      * e se la fase non è = a 35 o se la bolla è bloccata come giacenza
118060050907if  2c                   If        *In04 and (wgcfas <> 35 or arbfbc = 'G')
118070070312     c                   Eval      VidMsg = Msg(141)
118080040220     c                   Eval      *In28 = *On
118090040220     c                   Eval      *In90 = *On
118100040220     c                   If        VidTcr <> ArbTcr
118110040220     c                   Eval      *In62 = *On
118120040220     c                   GoTo      EndCt72
118130040220     c                   EndIf
118140040220     c                   If        ComDcr <> ArbDcr
118150040220     c                   Eval      *In40 = *On
118160040220     c                   GoTo      EndCt72
118170040220     c                   EndIf
118180040220     c                   If        VidHcr <> ArbHcr
118190040220     c                   Eval      *In41 = *On
118200040220     c                   GoTo      EndCt72
118210040220     c                   EndIf
118220040220    2c                   EndIf
118230040324      * posso modificare se la bolla ha giacenza aperta (IN04 ON) e la fase è = 35
118240040330      * ma solo se metto la data consegna richiesta maggiore/uguale alla data eseguibilità giacenza
118250040324if  2c                   If        *In04 and wgcfas = 35
118260040330if  3c                   If        wgcded > *Zeros and ComDcr >= wgcded
118270040324     c                   Eval      wgiac = 'S'
118280040324     c                   GoTo      EndCt72
118290040324    3c                   EndIf
118300040330if  3c                   If        wgcded = *Zeros and ComDcr >= wgcddm
118310040324     c                   Eval      wgiac = 'S'
118320040324     c                   GoTo      EndCt72
118330040324    3c                   EndIf
118340040324     c                   Eval      VidMsg = Msg(128)
118350040324     c                   Eval      *In28 = *On
118360040324     c                   Eval      *In90 = *On
118370040324     c                   If        VidTcr <> ArbTcr
118380040324     c                   Eval      *In62 = *On
118390040324     c                   GoTo      EndCt72
118400040324     c                   EndIf
118410040324     c                   If        ComDcr <> ArbDcr
118420040324     c                   Eval      *In40 = *On
118430040324     c                   GoTo      EndCt72
118440040324     c                   EndIf
118450040324     c                   If        VidHcr <> ArbHcr
118460040324     c                   Eval      *In41 = *On
118470040324     c                   GoTo      EndCt72
118480040324     c                   EndIf
118490040324    2c                   EndIf
118500050907      * richiamo il controllo della "7Q"
118510050907     c                   ExSr      Contr7q
118520030930      * posso modificare se
118530031001      *                     Peso o volume da fatturare sono >= a quanto tabellato in VPO
118540031001If  2c                   If        V1cPkf >= §VpoPkg or
118550031001     c                             V1cVlf >= §VpoVlm or
118560031001      *                     Destinatario supermercato o Apputamento
118570031001     c                             VidTc1 = 'S' or VidTc1 = 'A' or
118580031001     c                             VidTc2 = 'S' or VidTc2 = 'A' or
118590150506     c                             VidTc1 = 'Z' or VidTc2 = 'Z' or
118600031001      *                     Lasciato avviso
118610031001     c                             ArbFbc = 'A' or
118620031001      *                     Fermo deposito
118630031114     c                             ArbFfd = 'S' or
118640031114      *                     Tipo bolla "FW" Recapito c/assegni
118650050907     c                             ArbCbo = 'FW' or
118660050907      *                     Periodo trazione ridotte e bolla con particolarità "G8"
118670050907     c                             wDCRG8 = 'S'
118680031001     c                   GoTo      EndCt72
118690031001    2c                   EndIf
118700070320     c
118710070320     c* Se FFD immesso dalla partenza, anche se già tolto, posso
118720070320     c*  manutenzionare la spedizione
118730070920     c***                Clear                   dAr5Gen
118740070920     c***                Eval      kAr5Trd = 'GEN'
118750070920     c***  kAr5          Chain(n)  Fiar501l
118760070920     c***                If        %Found(Fiar501l)
118770070920     c***                Movel     Ar5Uni        dAr5Gen
118780070920     c***                if        §ar5MDP='S'  and §ar5FFD='S'
118790070920     c                   if        Wesar5gen='S' and par5mdp='S' and
118800070920     c                             par5FFD='S'
118810070320     c                   GoTo      EndCt72
118820070320    2c                   EndIf
118830070920    2c***                EndIf
118840070312    c
118850070312     c* Se la bolla non è in distinta, non c'e' blocco giacenza e c'e'
118860070312     c*  un evento che prevede la modifica della cons, richiesta
118870070312     c*  allora posso inserirla ed aggiornare direttamente ARB+fiar5
118880070319     c                   if        arbndc=0 and FGSgeot='S' and
118890070312     c                             arbfbc<>'G' and eveADR='S'
118900070312     c                   GoTo      EndCt72
118910070312    3c                   EndIf
118920070312     c
118930031001      *                     C/assegno non abilitato
118940031001If  2c                   If        Ar9Cas > 0
118950031001     c                   Clear                   Trul21ds
118960031001     c                   Eval      I21Tla = 'L'
118970031001     c                   Eval      I21Cbo = ArbCbo
118980031001     c                   Eval      I21Tsp = ArbTsp
118990031001     c                   Eval      I21Lnp = ArbLnp
119000031001     c                   Eval      I21Nzm = ArbNzm
119010031001     c                   Eval      I21Lna = ArbLna
119020031001     c                   Eval      I21Nzd = ArbNzd
119030130827     c****               Eval      I21Ksc = ArbKsc
119040031001     c                   Movel     ArbCtr        I21Ctr
119050031001     c                   Eval      I21Tic = Ar9Tic
119060031001     c                   Eval      I21Imp = Ar9Cas
119070031001     c                   Eval      I21Div = Ar9Vca
119080031001     c                   Eval      I21Ute = Knmus
119090031001     c                   Eval      I21Pgm = nompgm
119100130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
119110130827     c                   if        flgtb1='F'
119120130827     c                   z-add     arbksc        i21ksc
119130130827     c                   else
119140130827     c                   z-add     arbccm        i21ksc
119150130827     c                   if        i21ksc=0
119160130827     c                   z-add     8888          i21ksc
119170130827     c                   movel     arblnp        i21ksc
119180130827     c                   endif
119190130827     c                   endif
119200031001     c                   Call      'TRUL21R'
119210031001     c                   Parm                    Trul21ds
119220031001If  3c                   If        O21Fca <> *Blanks
119230031001     c                   GoTo      EndCt72
119240031001    3c                   EndIf
119250031001    2c                   EndIf
119260070309     c
119270070309     c* Se la bolla è in distinta, non devo però aggiornare
119280070306     c*  fnarb ma FIARP
119290111025    2c                   if        arbndc>0 and FGSgeot='S'
119300111025     c
119310111025     c* Se posso, scrivo FIARP, altrimenti subiro errore
119320111025    3c                   if        DcrinDist<>'S'
119330111025      * erorre
119340111025     c                   Eval      VidMsg = Msg(126)
119350111025     c                   Eval      *In28 = *On
119360111025     c                   Eval      *In90 = *On
119370111025     c                   Eval      *In40 = *On
119380111025     c                   GoTo      EndCt72
119390111025     c
119400111025   x3c                   else
119410070308     c                   eval      Aggioarp='S'
119420111025    4c                   if        comdcr<>savdcr  or vidtcr<>savtcr or
119430070308     c                             vidhcr<>savhcr
119440070309     c* Se richiamo nascosto, imposto solo il campo msgforza,
119450070309     c*  altrimenti emetto il msg
119460070330     c*  non imposto il campo se devo fare solo i controlli (26 OFF)
119470111025    5c                   if        d48ffr<>'E'
119480070308     c                   seton                                        402890
119490070308     c                   Eval      VidMsg = Msg(139)
119500111025   x5c                   else
119510070330     c
119520070330     c                   if        *in26
119530070330     c                   eval      msgforza=142
119540070309     c                   endif
119550111025    5c                   endif
119560070309     c
119570070308     c                   eval      savdcr=comdcr
119580070308     c                   eval      savtcr=vidtcr
119590070308     c                   eval      savhcr=vidhcr
119600111025    4c                   endif
119610111025    3c                   endif
119620070308     c
119630070306     c                   GoTo      EndCt72
119640111025    2c                   endif
119650070306     c
119660040220      * se arrivo qua vuol dire che non posso modificare
119670140806      * Se variazione da internet permetto la modifica
119680140806     c                   if        d48cvb<>'CI'
119690031001     c                   Eval      VidMsg = Msg(126)
119700031001     c                   Eval      *In28 = *On
119710031001     c                   Eval      *In90 = *On
119720031030     c                   If        VidTcr <> ArbTcr
119730031001     c                   Eval      *In62 = *On
119740031001     c                   GoTo      EndCt72
119750031001     c                   EndIf
119760031030     c                   If        ComDcr <> ArbDcr
119770031001     c                   Eval      *In40 = *On
119780031001     c                   GoTo      EndCt72
119790031001     c                   EndIf
119800031001     c                   If        VidHcr <> ArbHcr
119810031001     c                   Eval      *In41 = *On
119820031001     c                   GoTo      EndCt72
119830031001     c                   EndIf
119840140806     c                   endif
119850031001    1c                   EndIf
119860030930
119870030930     c     EndCt72       EndSr
119880920320     C*
119890920320     C*--- AGGIORNAMENTO FILE ----------------------------------------*
119900920320     C     REGIS7        BEGSR
119910070308     c                   clear                   ModconsR          1
119920130412     c                   clear                   wpda_ndc
119930130412     c                   clear                   wpda_ddc
119940130412     c                   clear                   wpda_dcm
119950070308     C* PRENDO I VALORI ASSOLUTI
119960070308     C                   MLLZO     1             VIDHCR
119970070308     c
119980070308     c* Mi salvo i campi della consegna
119990070308     c*  richiesta della bolla per sucessivi aggiornamenti.
120000070308     c                   eval      bolladcr=arbdcr
120010070308     c                   eval      bollahcr=arbhcr
120020070308     c                   eval      bollatcr=arbtcr
120030070308     c
120040070308     c* Mi salvo anche se modificata, solo se c'e' la DATA consegna impostat
120050070308     c                   if        vidtcr<>arbtcr or comdcr<>arbdcr or
120060070308     c                             arbhcr<>vidhcr  and comdcr>0
120070070308     c                   eval      ModconsR='S'
120080070308     c                   endif
120090070308     c
120100950104     C*  GIRO UDATE
120110950104     C                   TIME                    W0140            14 0
120120950104     C* UDATE IN GGMMAAAA
120130950104     C                   MOVE      W0140         WDTGIO            8 0
120140950104     C* UDATE IN AAAAMMGG
120150950104     C                   Z-ADD     WDTGIO        G02DAT
120160950104     C                   MOVEL     *BLANK        G02ERR
120170950104     C                   CALL      'XSRDA8'
120180950104     C                   PARM                    WLBDAT
120190950104     C                   MOVEL     G02INV        DATEU             8 0
120200941213     C                   MOVEL     KNMUS         ARBPRU
120210930507     C*
120220151230     c                   if        not *in10 or wfile='A'
120230930507     C* INVIO PER SEDE
120240170116     C****               MOVEL     'FNARBG0T'    SFILE
120250930507     C* ALLOCO
120260170116     C***                EXSR      ALLOC
120270170116     C** 91              GOTO      ENDRG7
120280070320     c
120290070320     c* Se impostata la "S" o la "A" devo impostare relativo blocco
120300070320     c*  telefonate in DSBL4L
120310070320     c                   clear                   wesar4L           1
120320070320     c                   clear                   BAS               1
120330070320     c                   clear                   TFD               1
120340070320     c                   if        ((vidtc1='S' or vidtc2='S') and
120350070320     c                              arbtc1<>'S' and arbtc2<>'S')  or
120360070320     c                             ((vidtc1='A' or vidtc2='A') and
120370070320     c                              arbtc1<>'A' and arbtc2<>'A')
120380070320     c                   eval      BAS='S'
120390070320     c                   endif
120400070320     c
120410070320     c* se tolto "A" o "S" devo eliminare il blocco
120420070320     c                   if        vidtc1<>'A' and vidtc1<>'S' and
120430070320     c                              vidtc2<>'A' and vidtc2<>'S' and
120440070320     c                             (arbtc1='S' or arbtc1='A' or
120450070320     c                              arbtc2='S' or arbtc2='A')
120460070320     c                   eval      BAS='N'
120470070320     c                   endif
120480161229     c* se aggiunta consegna richiesta devo eliminare il blocco appuntamento
120490161229     c                   if        bolladcr=0 and comdcr>0 and
120500161229     c                             vidtcr=' ' and
120510161229    1c                             AggioARP<>'S' and
120520161229     c                             (vidtc1='A' or vidtc2='A')
120530161229     c                   eval      BAS='N'
120540161229     c                   endif
120550070320     c* Tolgo blocco telefonata FFD se messa consegna richiesta e
120560070320     c*  presente FFD
120570070329     c****               if        arbffd='S' and viddcr>0
120580070329     c****               eval      TFD='N'
120590070329     c****               endif
120600070320     c                   if        bas<>' ' or TFD<>' '
120610070320     c                   eval      TipoEla='C'
120620070320     c                   exsr      Gestar4L
120630070320    1c                   if        *in91
120640170116     c****               exsr      DLCSED
120650070525     C                   SETON                                        2891
120660070320     C                   GOTO      ENDRG7
120670070320    1c                   endif
120680070320     c                   endif
120690041022     c
120700041022     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
120710041022     c* iornare direttamente il file
120720041022     c                   clear                   waltrabolla       1
120730041022     C   10KARB          CHAIN     FNBLP01L                           3091
120740041022     C  n10KARB          CHAIN     FNARB01L                           3091
120750041022     c*
120760070308    1c                   if        *in91
120770170116     c****               exsr      DLCSED
120780070515     c                   unlock    fiar401l
120790070525     C                   SETON                                        2891
120800041022     C                   MOVEL     MSG(4)        VIDMSG
120810041022     C                   GOTO      ENDRG7
120820070308    1c                   endif
120830070308     c
120840070308     c* Se lna attivata con GEO
120850070308     c* Se ho aggiornato FIARP, e devo aggiornare altri dati,
120860070308     c* nei campi della consegna richeista reimposto quelli della bolla
120870070319    1c                   if        FGSgeot='S'and AggioARP='S'
120880070308     c                   eval      comdcr=Bolladcr
120890070308     c                   eval      vidhcr=bollahcr
120900070308     c                   eval      vidtcr=Bollatcr
120910070308     c                   clear                   ModConsR
120920070308     c                   endif
120930070308     c
120940041022     c                   if        *in30
120950041022     c                   eval      waltrabolla='N'
120960041022     c                   else
120970041022     c* memorizzo campi prima della variazione
120980041022     c                   eval      waltrotc1=arbtc1
120990041022     c                   eval      waltrotc2=arbtc2
121000080422     c   10              eval      waltroft3=arbft3
121010130411     c  n10              eval      wpda_ndc=arbndc
121020130411     c  n10              eval      wpda_ddc=arbddc
121030130411     c  n10              eval      wpda_dcm=arbdcm
121040141223     c  n10              eval      bollaFBC=arbfbc
121050041022     C   10KARB          CHAIN     FNARB01L                           30
121060041022     C  N10KARB          CHAIN     FNBLP01L                           30
121070041022     c* Alla fine per non sporcarmi i campi del record, richaino il
121080041022     c*  record originale che sto variando
121090130411     c                   eval      wpda_ndc=arbndc
121100130411     c                   eval      wpda_ddc=arbddc
121110130411     c                   eval      wpda_dcm=arbdcm
121120041022     c                   endif
121130151230     c                   endif
121140930507     C*
121150930507     C                   Z-ADD     DATEU         ARBDTV
121160930507     C                   TIME                    ARBORV
121170930507     C                   MOVEL     VIDCVR        ARBCVB
121180001204     C*
121190001204     C                   MOVEL     ARBTC1        SAVTC1
121200001204     C                   MOVEL     ARBTC2        SAVTC2
121210080415     c*
121220930507     C*
121230060130     C* SE DA STORICIZZARE
121240060130     C**   *IN10         IFEQ      *ON
121250060130    1C**   D66FSA        ANDEQ     'S'
121260060130     C**   *IN10         OREQ      *OFF
121270060130    1C**   D66FSP        ANDEQ     'S'
121280070308     c
121290060130    1C     D66FSA        ifeq      'S'
121300011108      *
121310011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
121320011108      * è in partenza 'P' o in arrivo 'A'
121330011108     C     *IN10         IFEQ      *ON
121340160119     c                   if        wfile='P'
121350160119     C                   MOVE      'a'           ARBPRU
121360160119     c                   else
121370011108     C                   MOVE      'A'           ARBPRU
121380160119     c                   endif
121390011108     C                   ENDIF
121400011108     C     *IN10         IFEQ      *OFF
121410011108     C                   MOVE      'P'           ARBPRU
121420011108     C                   ENDIF
121430011108      *
121440941014     C                   WRITE     FNARBG00
121450930507     C                   END
121460060301     c* storicizzazione motivazione
121470060301     c     d66mtv        ifeq      'S'
121480060301     c                   exsr      sto_mtv
121490060301     c                   endif
121500040224      * se in arrivo ho modificato uno dei dati del Fiar5 rcd GEN:
121510040224      * - Tipo/Data/Ora consegna richiesta
121520040224      * - Giorni di chiusura
121530040224      * - Consegna particolare
121540040224      * devo memorizzare i dati della bolla prima della modifica
121550150129     c******             If        *In10 and
121560150206     c* dal 06/02 momorizziamo anche se varia la partenza, come se lo facesse l'arrivo
121570150129     c                   if         (VidTcr <> ArbTcr or ComDcr <> ArbDcr or
121580040224     c                               VidHcr <> ArbHcr or VidGc1 <> ArbGc1 or
121590040224     c                               VidGc2 <> ArbGc1 or VidTc1 <> ArbTc1 or
121600170405     c                               VidTc2 <> ArbTc2 )
121610041022     c                   ExSr      RegAr5AR
121620170405     c                   Else
121630170405     c* Se tutto uguale memorizzo i dati di ripristino nel fiar5 rec GEN solo se
121640170405     c* già esistente e causale CI oppure CD
121650170405     c                   if        (d48cvb='CI' or d48cvb='CD')  and
121660170405     c                             wesar5gen='S'
121670170405     c                   ExSr      RegAr5AR
121680170405     c                   EndIf
121690170405     c                   EndIf
121700080415     c*
121710080415     c* Se modifico data consegna richiesta verifico se devo sflaggare per
121720080415     c* clienti
121730080415     c                   eval      wsflagga_C=' '
121740080422     c                   if        comdcr<>arbdcr or
121750080415     c                             vidtcr<>arbtcr
121760080415     c                   clear                   wcmitt
121770080415     c                   if        flgtb1='A'
121780080415     c                   move      arbccm        wcmitt
121790080415     c                   else
121800080415     c                   move      arbksc        wcmitt
121810080415     c                   endif
121820080416     c                   if        wcmitt>'0000000'
121830080416     c                             and %subst(wcmitt:4:4)<>'8888'
121840080415     C                   MOVEL     '3K'          COD
121850080415     C                   MOVEL(P)  wcmitt        KEY
121860080415     C                   EXSR      CTABEL
121870080415     C  N30              MOVEL     TBLUNI        DS3K
121880080415     C   30              CLEAR                   DS3K
121890080415     c                   if        §3kcq8='S'
121900080415     c                   eval      wsflagga_C='S'
121910080415     c                   endif
121920080415     c                   endif
121930080415     c                   endif
121940041022     c
121950041022     c* Se variazione in partenza e modifico  le cons.partic
121960041022     c*  sflaggo per clienti
121970041025     c  n10              if        vidtc1<>arbtc1 or vidtc2<>arbtc2
121980080415     c                             or wsflagga_c='S'
121990041022     c                   clear                   arbft3
122000041022     c                   endif
122010070319     c
122020070319     c* Se impostata la "S" o la "A" devo impostare relativo blocco
122030070319     c*  telefonate in DSBL4L
122040161229     c* idem se aggiunta la dcr devo sbloccare eventuale appuntamento
122050151230     c                   if        not *in10 or wfile='A'
122060070320     c                   if        wesar4L<>' '
122070070320     c                   if        BAS='S'
122080070320     c                   eval      §b4lbas='S'
122090070320     c                   endif
122100070320     c                   if        BAS='N'
122110070320     c                   eval      §b4lbas=' '
122120070320     c                   endif
122130070320     c
122140070320     c                   if        TFD='N'
122150070320     c                   eval      §b4lTFD=' '
122160070320     c                   endif
122170070320     c
122180070320     c                   eval      TipoEla='A'
122190070320     c                   exsr      GestAR4L
122200070320     c                   endif
122210151230     c                   endif
122220070320     c
122230920320     C*
122240920320     C                   MOVEL     COMDCR        ARBDCR
122250920320     C                   MOVEL     VIDHCR        ARBHCR
122260920320     C                   MOVEL     VIDTCR        ARBTCR
122270930507     C                   MOVEL     VIDGC1        ARBGC1
122280930507     C                   MOVEL     VIDGC2        ARBGC2
122290941102     C                   MOVEL     VIDTC1        ARBTC1
122300941102     C                   MOVEL     VIDTC2        ARBTC2
122310070308     c
122320070308     c* Se modificata consegna richiesta in arrivo
122330070308     c*  devo anche pulire il flag blocco consegna se = 'A'
122340141223     c                   if        BollaFBC='A' and ModconsR='S'
122350070308     c                             and comdcr>0
122360070308     c                   clear                   arbfbc
122370070419     c                   clear                   arbcmc
122380070308     c                   endif
122390070920     c
122400070920     c* Se modificata la consegna richiesta e bolla già consegnata
122410070920     c* effettuo il ricalcolo della affidabilità
122420070920     c                   if        ModconsR='S' and arbdcr=0 and arbdcm>0
122430070920     c                   clear                   arbdti
122440070920     c                   clear                   arbhti
122450070920     c                   movel     'S'           arbfbs
122460070920     c                   endif
122470041025     c
122480070319     c* Verifico se devo aggiornare record già esistente in ar5
122490150206     c*  ma solo per le consegne particolari e fermo deposito
122500150206     c  N10              exsr      RegAr5PA
122510941212     C*
122520930507     C* PER SEDE
122530151230     c                   if        not *in10 or wfile='A'
122540070503     C     D66FFI        IFEQ      'S'
122550950227     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
122560950413    2C     §3ABSD        IFEQ      ' '
122570930507     C*
122580170116     C***                OPEN      FNARBG0T
122590941014     C                   WRITE     FNARBGTT
122600170116     C***                CLOSE     FNARBG0T
122610950227     C                   END
122620930507     C* DISALLOCO OGGETTO
122630170116     C****               EXSR      DLCSED                                       FNARBG0T
122640950227     C                   END
122650151230     c                   endif
122660041022     c
122670930507     C*
122680151230     c                   if        wfile='A'
122690151230     C                   UPDATE    FNARB000
122700151230     c                   else
122710151230     C                   UPDATE    FNBLP000
122720151230     c                   endif
122730941220     C*
122740941220     C* DISALLOCO I RECORD CHE NON USO
122750160119     C   10              UNLOCK    FIAR601L
122760041022     c* Aggiorno anche l'altra bolla
122770151230     c                   if        not *in10 or wfile='A'
122780041022     c                   if        waltrabolla=' '
122790080422     c   10              eval      arbft3=waltroft3
122800041025     c* SE aggiorno bopartenza sflaggo per clienti se variate le cons.partic
122810041025     c                   if        *in10 and (waltrotc1<>arbtc1 or
122820080415     c                             waltrotc2<>arbtc2 or
122830080415     c                             wsflagga_c='S')
122840041022     c                   clear                   arbft3
122850041022     c                   endif
122860041022     C   10              except    arbGblp
122870041022     C  N10              except    arbGarb
122880041022     c                   endif
122890151230     c                   endif
122900950602     C*
122910070308     c* Se non è previsto l'aggiornamento di FIARP, vedo se lo devo
122920070308     c*  cancellare (se variata la data consegna richiesta)
122930070319     c                   if        FGSgeot='S'  and  ModconsR='S'
122940070308     c     karb          delete    fiarp01l
122950070308     c                   endif
122960070308     c
122970040224      * Se c'è la data consegna richiesta controllo se è ancora valida
122980040224      * solo se ho fatto una modifica in arrivo
122990150129      * da adesso anche se modifica in partenza
123000150206     c                   If        ArbDcr > *Zeros
123010040324     c                             and wCa = *Blanks and wgiac = *Blanks
123020040224     c                   ExSr      ContrDcr
123030040224     c                   EndIf
123040070308     c
123050130411     C*
123060130411     c* Richiamo routine per invio variazione a pda
123070130411     c                   if        D48Cvb<>'CS'
123080151230     c                             and (not *in10 or wfile='A')
123090130411     c                   exsr      sr_pda
123100130411     c                   endif
123110950104     C*
123120930507     C     ENDRG7        ENDSR
123130021129      **
123140021129      *--- AGGIORNAMENTO FILE FIAR5 ----------------------------------*
123150021129     c     Regis71       BegSr
123160021129
123170021129      * Se è stata indicata la consegna particolare 'S' devo scrivere
123180021129      * il file FIAR5
123190021129     c                   If        VidTc1 = 'S' or VidTc2 = 'S'
123200021129     c                   Clear                   Fiar5000
123210021129     c                   Eval      Ar5Aas = ArbAas
123220021129     c                   Eval      Ar5Lnp = ArbLnp
123230021129     c                   Eval      Ar5Nrs = ArbNrs
123240021129     c                   Eval      Ar5Nsp = ArbNsp
123250021129     c                   Eval      Ar5Trd = 'SCR'
123260021129     c                   Move      Dateu         Ar5Dac
123270021129     c                   Movel     W0140         Ar5Orc
123280021129     c                   Clear                   Dar5Scr
123290021129     c                   Eval      §ar5Cvb = D48Cvb
123300021129     c                   Eval      §ar5Pru = Knmus
123310060131     c                   Eval      §ar5Pou = dutPou
123320021129     c                   Eval      §ar5Tcr = VidTcr
123330021129     c                   Eval      §ar5Dcr = ComDcr
123340021129     c                   Eval      §ar5Hcr = VidHcr
123350021129     c                   Eval      §ar5Gc1 = VidGc1
123360021129     c                   Eval      §ar5Gc2 = VidGc2
123370021129     c                   Eval      §ar5Tc1 = VidTc1
123380021129     c                   Eval      §ar5Tc2 = VidTc2
123390021129     c                   Eval      §ar5Nom = VidNom
123400021129     c                   Eval      §ar5Tel = VidTel
123410021129     c                   Eval      §ar5Mot = VidMot
123420021129     c                   Movel     Dar5Scr       Ar5Uni
123430021129     c                   Write     Fiar5000
123440021129     c                   EndIf
123450040705
123460040705      * Aggiorno anche il record 'GEN' di Fiar5 con il nuovo Telefono e Referente
123470091217     c                   eval      Wtipoagg='REF'
123480091217     c                   exsr      RegisAR5GEN
123490091217     c***
123500091217     c***                Eval      kAr5Trd = 'GEN'
123510091217     c***                Clear                   dAr5Gen
123520091217     c***  kAr5          Chain     Fiar501l
123530040705      * Aggiorno Fiar5
123540091217if  1c***                If        %Found(Fiar501l)
123550091217     c***                Eval      dAr5Gen = Ar5Uni
123560091217     c***                Eval      §Ar5Ref = VidNom
123570091217     c***                Eval      §Ar5Teld = VidTel
123580091217     c***                Movel(p)  dAr5Gen       Ar5Uni
123590040705      * se la modifica è fatta in partenza e devo trasmettere all'arrivo
123600040705      * sfleggo per la trasmissione
123610070306     c***                If        Not *In10 and §Ar5Trar = 'S'
123620070306     c***                Clear                   Ar5Ft1
123630070306     c***                EndIf
123640040705      * se la modifica è fatta in arrivo e devo trasmettere alla partenza
123650040705      * sfleggo per la trasmissione
123660070306     c***                If        *In10 and §Ar5Trpa = 'S'
123670070306     c***                Clear                   Ar5Ft1
123680070306     c***                EndIf
123690040705      * se devo trasmettere alla sede sfleggo per la trasmissione
123700091217     c***                If        §Ar5Trse = 'S'
123710091217     c***                Clear                   Ar5Ft2
123720091217     c***                EndIf
123730040705      * sfleggo sempre per la trasmissione al cliente
123740091217     c***                Clear                   Ar5Ft3
123750091217     c***                Update    Fiar5000
123760040705      * Scrivo Fiar5
123770091217   x1c***                Else
123780040705      * se immessi i dati
123790091217     c***                Clear                   Fiar5000
123800091217     c***                Eval      Ar5Aas = ArbAas
123810091217     c***                Eval      Ar5Lnp = ArbLnp
123820091217     c***                Eval      Ar5Nrs = ArbNrs
123830091217     c***                Eval      Ar5Nsp = ArbNsp
123840091217     c***                Eval      Ar5Trd = 'GEN'
123850091217     c***                Move      Dateu         Ar5Dac
123860091217     c***                Movel     W0140         Ar5Orc
123870091217     c***                Clear                   Dar5Gen
123880091217     c***                Eval      §Ar5Ref = VidNom
123890091217     c***                Eval      §Ar5Teld = VidTel
123900091217     c***                Movel(p)  DAr5Gen       Ar5Uni
123910091217     c***                Write     Fiar5000
123920091217    1c***                EndIf
123930130411     c* Richiamo routine per invio variazione a pda
123940130411     c                   exsr      sr_pda
123950021129
123960021129     c                   EndSr
123970040223
123980041022      *--- CONTROLLO E AGGIORNAMENTO FIAR5 RCD GEN se sono in arrivo--*
123990041022     c     RegAr5AR      BegSr
124000070309     c* Se Attiva GEO e bolla non in distnta e non giacente con
124010070309     c*  un evento che prevede la modifica della cons.richista
124020070309     c*  memorizzo la cons richiesta nei campi di FIAR5
124030070309     c                   clear                   AggioAR5M         1
124040070309     c
124050070319     c                   if        arbndc=0 and FGSgeot='S' and
124060070309     c                             arbfbc<>'G' and eveADR='S'
124070070309     c                   eval      AggioAR5M='S'
124080070309     c                   endif
124090160114     c* Se manutenzione in Arrivo su bolla ancora da partire memorizzo
124100160114     c* di aggiornare i dati del ripristino con quelli del video
124110160114     c                   if        *in10 and wfile = 'P'
124120160114     c                   eval      AggioAR5M='S'
124130160114     c                   endif
124140040223
124150040224     c                   Clear                   waggar5
124160040223     c                   Clear                   dAr5Gen
124170040223     c                   Eval      kAr5Trd = 'GEN'
124180040223     c     kAr5          Chain     Fiar501l
124190040223     c                   If        %Found(Fiar501l)
124200040223     c                   Movel     Ar5Uni        dAr5Gen
124210040223     c                   EndIf
124220040223
124230040223      * Dati non memorizzati
124240040223if  1c                   If        §Ar5Mdp <> 'S'
124250040224     c                   Eval      §Ar5Mdp = 'S'
124260040223     c                   Eval      §Ar5gTcr = ArbTcr
124270040223     c                   Move      ArbDcr        §Ar5gDcr
124280040223     c                   Move      ArbHcr        §Ar5gHcr
124290040223     c                   Eval      §Ar5gGc1 = ArbGc1
124300040223     c                   Eval      §Ar5gGc2 = ArbGc2
124310040223     c                   Eval      §Ar5gTc1 = ArbTc1
124320040223     c                   Eval      §Ar5gTc2 = ArbTc2
124330040223     c                   Eval      §Ar5Ffd  = ArbFfd
124340040223      * Causale 'CD' da chiusura distinta
124350140806      *   oppure
124360140806      * Causale 'CI' da indicazioni da internet
124370040223      * imposto i campi a video
124380140806if  2c                   If        d48cvb = 'CD' or d48cvb='CI' or AggioAR5M='S'
124390040223     c                   Eval      §Ar5Tcrm = VidTcr
124400040223     c                   Move      ComDcr        §Ar5Dcrm
124410040223     c                   Move      VidHcr        §Ar5Hcrm
124420040223      * Altra causale
124430040223      * imposto i campi di arb prima della variazione
124440040223   x2c                   Else
124450040223     c                   Eval      §Ar5Tcrm = ArbTcr
124460040223     c                   Move      ArbDcr        §Ar5Dcrm
124470040223     c                   Move      ArbHcr        §Ar5Hcrm
124480040223    2c                   EndIf
124490040224     c                   Eval      waggar5 = 'S'
124500040223      * Dati memorizzati
124510040223   x1c                   Else
124520140806if  2c                   If        d48cvb = 'CD' or d48cvb='CI' or AggioAR5M='S'
124530040223     c                   Eval      §Ar5Tcrm = VidTcr
124540040223     c                   Move      ComDcr        §Ar5Dcrm
124550040223     c                   Move      VidHcr        §Ar5Hcrm
124560040224     c                   Eval      waggar5 = 'S'
124570040223    2c                   EndIf
124580040223    1c                   EndIf
124590150206     c* se si tratta di una modifica di cons richiesta in partenza lo memorizzo se
124600150206     c*  non l'ho già fatto
124610150206     c                   if        not *in10 and comdcr<>arbdcr and
124620150206     c                             §ar5dcrp<>'P'  and comdcr>0
124630150206     c                   eval      §ar5dcrp= 'P'
124640150206     c                   Eval      waggar5 = 'S'
124650150206    2c                   EndIf
124660150206     c                   if        not *in10 and comdcr<>arbdcr and
124670150206     c                             §ar5dcrp= 'P'  and comdcr=0
124680150206     c                   eval      §ar5dcrp= ' '
124690150206     c                   Eval      waggar5 = 'S'
124700150206    2c                   EndIf
124710150206     c
124720040223
124730040224if  1c                   If        waggar5 = 'S'
124740040224      * aggiorno Fiar5
124750040224if  2c                   If        %Found(Fiar501l)
124760040224     c                   Movel(p)  dAr5Gen       Ar5Uni
124770040224      * se devo trasmettere alla sede sfleggo per la trasmissione
124780040224     c                   If        §Ar5Trse = 'S'
124790040224     c                   Clear                   Ar5Ft2
124800040224     c                   EndIf
124810040224      * sfleggo sempre per la trasmissione al cliente
124820040224     c                   Clear                   Ar5Ft3
124830040223     c                   Update    Fiar5000
124840040224      * scrivo Fiar5
124850040224   x2c                   Else
124860040224     c                   Clear                   Fiar5000
124870040223     c                   Eval      Ar5Aas = ArbAas
124880040223     c                   Eval      Ar5Lnp = ArbLnp
124890040223     c                   Eval      Ar5Nrs = ArbNrs
124900040223     c                   Eval      Ar5Nsp = ArbNsp
124910040224     c                   Eval      Ar5Trd = 'GEN'
124920040223     c                   Move      Dateu         Ar5Dac
124930040223     c                   Movel     W0140         Ar5Orc
124940040224     c                   Movel(p)  dAr5Gen       Ar5Uni
124950041007     c* flaggo sempre il flag di filiale tanto non trasmettiamo più
124960040223     c                   Write     Fiar5000
124970040224    2c                   EndIf
124980040224   x1c                   Else
124990040224     c                   Unlock    Fiar501l
125000040224    1c                   EndIf
125010040223
125020040223     c                   EndSr
125030041022
125040041022      *--- CONTROLLO E AGGIORNAMENTO FIAR5 RCD GEN se sono in partenz-*
125050041022     c     RegAr5PA      BegSr
125060041022     c                   Clear                   dAr5Gen
125070041022     c                   Eval      kAr5Trd = 'GEN'
125080041022     c     kAr5          Chain     Fiar501l
125090041022if  2c                   If        %Found(Fiar501l)
125100041022     c                   Movel     Ar5Uni        dAr5Gen
125110041022      * Flag dati memorizzati
125120041022if  3c                   If        §Ar5Mdp = 'S'
125130041022      * Aggiorno i dati di ripristino solo se uguali a quelli salvati
125140150206if  4c***                If        §Ar5gTcr= §Ar5Tcrm and §Ar5gDcr = §Ar5Dcrm
125150150206     c***                          and §Ar5gHcr = §Ar5Hcrm
125160150206     c***                Eval      §Ar5Tcrm = ArbTcr
125170150206     c***                Move      ArbDcr        §Ar5Dcrm
125180150206     c***                Move      ArbHcr        §Ar5Hcrm
125190150206    4c***                EndIf
125200041022      * Memorizzo i dati della bolla
125210150206     c***                Eval      §Ar5gTcr = ArbTcr
125220150206     c***                Move      ArbDcr        §Ar5gdcr
125230150206     c***                Move      ArbHcr        §Ar5gHcr
125240150206     c***                Eval      §Ar5gGc1 = ArbGc1
125250150206     c***                Eval      §Ar5gGc2 = ArbGc2
125260150206     c* Da 02/15
125270150206     c* come partenza aggiorno soltanto i dati del fermo deposito e dei giorni di chiusura
125280041022     c                   Eval      §Ar5gTc1 = ArbTc1
125290041022     c                   Eval      §Ar5gTc2 = ArbTc2
125300041022     c                   Eval      §Ar5Ffd = arbFfd
125310041022    3c                   EndIf
125320041022     c                   Movel     Dar5Gen       Ar5Uni
125330041022     c                   Update    Fiar5000
125340041022    2c                   EndIf
125350041022     c                   EndSr
125360070906
125370070906      *--- CONTROLLO SE ANCORA VALIDA Le consegne particolari --------*
125380070906     c     ContrTCP      BegSr
125390070910      *
125400070906     c                   clear                   eliminatcp        1
125410070911     c                   clear                   wcomb
125420070911     c                   clear                   aggtcp            1
125430070906     c
125440070910    1c                   if        arbtc1=wdisagdest
125450070910if  2c                   If        (par5Mdp = 'S' and
125460070910     c                             par5gtc1<>arbtc1 and par5gtc2<>arbtc1)
125470070911     c*******                      or   par5Mdp=' '
125480070906     c                   eval      eliminatcp='1'
125490070911     c                   eval      wtc1=arbtc1
125500070907    2c                   endif
125510070910    1c                   endif
125520070910    1c                   if        arbtc2=wdisagdest
125530070910if  2c                   If        (par5Mdp = 'S' and
125540070910     c                             par5gtc1<>arbtc2 and par5gtc2<>arbtc2)
125550070911     c********                     or   par5Mdp=' '
125560070906     c                   eval      eliminatcp='2'
125570070911     c                   eval      wtc1=arbtc2
125580070907    2c                   endif
125590070910    1c                   endif
125600070906     c* Se da eliminare
125610070910    1c                   if        eliminatcp<>' '
125620070911     c* Verifico se per le combinazioni devo ripristinare la cons partic
125630070911     c*  della partenza
125640070911     c                   if        par5mdp='S' and par5gtc1<>' '
125650070911     c                   eval      wtc2=par5gtc1
125660070911     c                   exsr      sr_chkcomb
125670070911     c
125680070911     c                   if        wcomb<>' '
125690070911     c                   eval      aggtcp=par5gtc1
125700070911     c                   endif
125710070912     c                   endif
125720070911     c
125730070911     c                   if        wcomb=' ' and par5gtc2<>' '
125740070911     c                   eval      wtc2=par5gtc1
125750070911     c                   exsr      sr_chkcomb
125760070911     c                   if        wcomb<>' '
125770070911     c                   eval      aggtcp=par5gtc2
125780070911     c                   endif
125790070911     c                   endif
125800070911     c
125810070906     c                   Clear                   dsarbg
125820070906     c                   Eval      d48cvb = 'CA'
125830070906     c                   Eval      weseg = 'E'
125840070911     c*****              Clear                   d48ffr
125850070906     c                   Eval      d48Trc = 'G'
125860070906     c                   Eval      §bgTcr = arbtcr
125870070906     c                   Move      arbdcr        §bgDcr
125880070906     c                   Move      arbhcr        §bgHcr
125890070906     c                   Eval      §bgGc1 = Arbgc1
125900070906     c                   Eval      §bgGc2 = ArbGc2
125910070910    2c                   if        eliminatcp='1'
125920070906     c                   clear                   §bgtc1
125930070906     c                   clear                   arbtc1
125940070906     c                   Eval      §bgtc2 = Arbtc2
125950070911     c* ripristino la vecchia consegna partic.
125960070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
125970070911     c                   eval      §bgtc1=aggtcp
125980070911     c                   endif
125990070910   x2c                   else
126000070906     c                   clear                   §bgtc2
126010070906     c                   clear                   arbtc2
126020070906     c                   Eval      §bgtc1 = Arbtc1
126030070911     c* ripristino la vecchia consegna partic.
126040070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
126050070911     c                   if        §bgtc1=' '
126060070911     c                   eval      §bgtc1=aggtcp
126070070911     c                   else
126080070911     c                   eval      §bgtc1=aggtcp
126090070911     c                   endif
126100070911     c                   endif
126110070910    2c                   endif
126120070907     c
126130070907     c* Memorizzo in skiera rielaborazioni
126140070910    2c                   if        kk<=10
126150070907     c                   add       1             kk
126160070907     c                   movel     dsarbg        kdati(KK)
126170070907     c                   movel     d48cvb        kcvb(KK)
126180070907     c                   movel     d48trc        ktrc(KK)
126190070911     c                   movel     ' '           kffr(KK)
126200070911     c                   movel     ' '           kf12(KK)
126210070910    2c                   endif
126220070910    1c                   endif
126230070906     c
126240070906     c                   EndSr
126250040224
126260040224      *--- CONTROLLO SE ANCORA VALIDA LA DATA CONSEGNA RICHIESTA -----*
126270160114     c     ContrDcr      BegSr
126280040225
126290040225     c                   Move      ArbDcr        warbdcr
126300050907      * richiamo il controllo della "7Q"
126310050907     c                   ExSr      Contr7q
126320040225
126330040225      * Controllo se c'è il fiar5 rcd GEN
126340070906     c                   clear                   dar5gen
126350040225     c                   Eval      kAr5Trd = 'GEN'
126360040225     c     kAr5          Chain(n)  Fiar501l
126370040225     c                   If        %Found(Fiar501l)
126380040225     c                   Movel     Ar5Uni        dAr5Gen
126390040225     c                   EndIf
126400040225      * se ho memorizzato i dati
126410040225if  1c                   If        §Ar5Mdp = 'S' and
126420040225      * e la data è diversa
126430040225     c                             (wArbDcr <> §Ar5Dcrm or
126440040225     c                               ArbTcr <> §Ar5Tcrm)
126450040225      * controllo se la data è valida quando:
126460040224      *                     Peso o volume da fatturare sono >= a quanto tabellato in VPO
126470040225if  2c                   If        ArbPkf >= §VpoPkg or
126480040224     c                             ArbVlf >= §VpoVlm or
126490150506      *                     Destinatario supermercato o Appuntamento
126500040224     c                             ArbTc1 = 'S' or ArbTc1 = 'A' or
126510040224     c                             ArbTc2 = 'S' or ArbTc2 = 'A' or
126520150506     c                             ArbTc1 = 'Z' or ArbTc2 = 'Z' or
126530040312      *                     Lasciato avviso
126540070308     c                             BollaFbc = 'A' or
126550040224      *                     Fermo deposito
126560040224     c                             ArbFfd = 'S' or
126570040224      *                     Tipo bolla "FW" Recapito c/assegni
126580040811     c                             ArbCbo = 'FW'or
126590040811      *                     Periodo trazione ridotte e bolla con particolarità "G8"
126600040811     c                             wDCRG8 = 'S'
126610040225     c                   Goto      EndContr
126620040225    2c                   EndIf
126630040224      *                     C/assegno non abilitato
126640040225if  2c                   If        Ar9Cas > 0
126650040224     c                   Clear                   Trul21ds
126660040224     c                   Eval      I21Tla = 'L'
126670040224     c                   Eval      I21Cbo = ArbCbo
126680040224     c                   Eval      I21Tsp = ArbTsp
126690040224     c                   Eval      I21Lnp = ArbLnp
126700040224     c                   Eval      I21Nzm = ArbNzm
126710040224     c                   Eval      I21Lna = ArbLna
126720040224     c                   Eval      I21Nzd = ArbNzd
126730130827     c*****              Eval      I21Ksc = ArbKsc
126740040224     c                   Movel     ArbCtr        I21Ctr
126750040224     c                   Eval      I21Tic = Ar9Tic
126760040224     c                   Eval      I21Imp = Ar9Cas
126770040224     c                   Eval      I21Div = Ar9Vca
126780040224     c                   Eval      I21Ute = Knmus
126790040224     c                   Eval      I21Pgm = nompgm
126800130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
126810130827     c                   if        flgtb1='F'
126820130827     c                   z-add     arbksc        i21ksc
126830130827     c                   else
126840130827     c                   z-add     arbccm        i21ksc
126850130827     c                   if        i21ksc=0
126860130827     c                   z-add     8888          i21ksc
126870130827     c                   movel     arblnp        i21ksc
126880130827     c                   endif
126890130827     c                   endif
126900040224     c                   Call      'TRUL21R'
126910040224     c                   Parm                    Trul21ds
126920040225if  3c                   If        O21Fca <> *Blanks
126930040225     c                   Goto      EndContr
126940040225    3c                   EndIf
126950040225    2c                   EndIf
126960070320     c
126970070320     c* Se FFD immesso dalla partenza, anche se già tolto, posso
126980070320     c*  manutenzionare la spedizione
126990160114    1c                   if        §ar5MDP='S'  and §ar5FFD='S'
127000070320     c                   GoTo      EndContr
127010070911    1c                   EndIf
127020150206     c* se la cons richiesta non è stata immessa dalla partenza e sono in partenza
127030150206     c*  ripristino
127040160114     c                   if        (not *in10 and  §ar5dcrp= 'P')
127050160114     c                             or (*in10 and wfile='P')
127060150206     c                   GoTo      EndContr
127070150206    1c                   EndIf
127080150206     c
127090040225      * in questo caso devo reimpostare la data consegna richiesta come indicato in Fiar5
127100040225      * facendo una variazione di tipo 'CA'
127110040225      * senza farla vedere a video
127120040225     c                   Clear                   dsarbg
127130040225     c                   Eval      d48cvb = 'CA'
127140040225     c                   Eval      weseg = 'E'
127150070911     c*****              Clear                   d48ffr
127160040225     c                   Eval      d48Trc = 'G'
127170040324     c                   Eval      §bgTcr = §Ar5Tcrm
127180040324     c                   Move      §Ar5Dcrm      §bgDcr
127190040324     c                   Move      §Ar5Hcrm      §bgHcr
127200040315     c                   Eval      §bgGc1 = Arbgc1
127210040315     c                   Eval      §bgGc2 = ArbGc2
127220040315     c                   Eval      §bgTc1 = ArbTc1
127230040315     c                   Eval      §bgTc2 = ArbTc2
127240040225     c                   Eval      wCA = 'S'
127250070906     c* Se da eliminare anche la consegna particolare imposto
127260070906     c*  insieme la variazione
127270070911    1c                   if        eliminatcp='1'
127280070906     c                   clear                   §bgtc1
127290070911     c* ripristino la vecchia consegna partic.
127300070911    2c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
127310070911     c                   eval      §bgtc1=aggtcp
127320070911    2c                   endif
127330070911    1c                   endif
127340070911    1c                   if        eliminatcp='2'
127350070906     c                   clear                   §bgtc2
127360070911    2c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
127370070911    3c                   if        §bgtc1=' '
127380070911     c                   eval      §bgtc1=aggtcp
127390070911     c                   else
127400070911     c                   eval      §bgtc2=aggtcp
127410070911    3c                   endif
127420070911    2c                   endif
127430070911    1c                   endif
127440070906     c
127450070906     c* Memorizzo in skiera rielaborazioni
127460070906     c                   if        kk<=10
127470070907     c* se devo eliminare una consegna particolare, memorizzo nella
127480070907     c* stessa variazione
127490070907     c                   if        eliminatcp=' '
127500070907     c                   add       1             kk
127510070907     c                   endif
127520070906     c                   movel     dsarbg        kdati(KK)
127530070906     c                   movel     d48cvb        kcvb(KK)
127540070907     c                   movel     d48trc        ktrc(KK)
127550070911     c                   movel     ' '           kffr(KK)
127560070911     c                   movel     ' '           kf12(KK)
127570070906     c                   endif
127580070906     c
127590040225    1c                   EndIf
127600040224
127610040225     c     EndContr      EndSr
127620040224
127630070307     C*--- AGGIORNAMENTO FILE proposte FIARP00F ----------------------*
127640070307     C     REGISARP      BEGSR
127650070307     C                   MLLZO     1             VIDHCR
127660070307     c     karb          chain     fiarp01l
127670070307     c                   clear                   fiarp000
127680070307     c                   eval      arpaas=arbaas
127690070307     c                   eval      arplnp=arblnp
127700070307     c                   eval      arpnrs=arbnrs
127710070307     c                   eval      arpnsp=arbnsp
127720070307     c                   eval      arptcr=vidtcr
127730070307     c                   eval      arpdcr=comdcr
127740070307     c                   eval      arphcr=vidhcr
127750070308     C*
127760070308     C                   TIME                    W0140            14 0
127770070308     C* UDATE IN AAAAMMGG
127780070308     C                   MOVE      W0140         WDTGIO            8 0
127790070308     C                   Z-ADD     WDTGIO        G02DAT
127800070307     C                   MOVEL     *BLANK        G02ERR
127810070307     C                   CALL      'XSRDA8'
127820070307     C                   PARM                    WLBDAT
127830070307     C                   MOVEL     G02INV        arpdim            8 0
127840070307     c                   time                    arpoim
127850070308     C                   MOVEL     KNMUS         ARPPRU
127860070307     c                   if        %found(fiarp01l)
127870070307     c                   update    fiarp000
127880070307     c                   else
127890070307     c                   write     fiarp000
127900070307     c                   endif
127910070307     c
127920070307     c                   ENDSR
127930941223     C*
127940941223     C*--- CONTROLLI CAMPI MITTENTE ----------------------------------*
127950941223     C     CONTR8        BEGSR
127960941223     C                   SETOFF                                       9028
127970950104     C                   CLEAR                   VIDFAP
127980941223     C*
127990941223     C* CONTROLLO IL CODICE DEL MITTENTE
128000970813     C                   EXSR      CTRCOM
128010970822     C*
128020970822     C* SE PASSO DA CODIFICATO AD 8888 O VICEVERSA SPROTEGGO O PROTEGGO
128030970822     C* CAMPI DELL'INDIRIZZO
128040970822     C     VIDKSC        IFEQ      8888
128050970822     C     MEMKSC        ANDNE     8888
128060970822     C                   MOVE      VIDKSC        MEMKSC
128070970822     C                   SETOFF                                       22
128080970822     C                   SETON                                        90
128090970822     C                   END
128100970822     C*
128110970822     C     VIDKSC        IFNE      8888
128120970822     C     MEMKSC        ANDEQ     8888
128130970822     C                   MOVE      VIDKSC        MEMKSC
128140970822     C                   SETON                                        2290
128150970822     C                   CLEAR                   VIDRSM
128160970822     C                   CLEAR                   VIDINM
128170970822     C                   CLEAR                   VIDCAM
128180970822     C                   CLEAR                   VIDLOM
128190970822     C                   CLEAR                   VIDPRM
128200970822     C                   CLEAR                   VIDNZM
128210970822     C                   CLEAR                   VIDPIM
128220970822     C                   END
128230950113     C**
128240950113     C     *IN90         IFEQ      *ON
128250950113     C  N48              GOTO      ENDCT8
128260950113     C*
128270950113     C* X IL CODICE TARIFFA DO ERRORE SOLO SE NON E' STATO PREMUTO F10
128280950114     C  NKJ
128290950114     CAN 28              GOTO      ENDCT8
128300950113     C                   ENDIF
128310941223     C*
128320020508     C* SE IMMESSO UN CODICE 8888 OBBLIGATORIO l'indirizzo completo
128330020508    1C     VIDKSC        IFEQ      8888
128340970808     C                   MOVEL     VIDRSM        I14RSC
128350970808     C                   CLEAR                   I14RS2
128360970808     C                   MOVEL     VIDINM        I14IND
128370970808     C                   MOVEL     VIDLOM        E14LOC
128380970808     C                   MOVEL     VIDPIM        E14PIP
128390970808     C                   MOVEL     VIDPRM        E14PRV
128400970808     C                   MOVEL     VIDNZM        E14NAR
128410970808     C                   MOVEL     VIDCAM        E14CAP
128420970808     C                   CLEAR                   I14ISO
128430970808     C                   MOVEL     'M'           WCLI              1
128440970808     C                   EXSR      CTRIND
128450941223     C*
128460941223     C* CAMPI CHE POSSO AVER IMPOSTATO
128470970808     C                   MOVEL     E14LOC        VIDLOM
128480970808     C                   MOVEL     E14PIP        VIDPIM
128490970808     C                   MOVEL     E14PRV        VIDPRM
128500970808     C                   MOVEL     E14NAR        VIDNZM
128510970808     C                   MOVEL     E14CAP        VIDCAM
128520950104     C* SALVO IL FLAG ANTEPORTO
128530970808     C                   MOVEL     O95ISO        COMFAP
128540970808     C                   MOVEL     O95ISO        VIDFAP
128550140403     c* salvo pickup 2
128560140403     C                   MOVEL     O95gf2        comgf2
128570941223     C**
128580970808     C* ERRORE
128590970808     C   90              GOTO      ENDCT8
128600950522     C* P.IVA obbligatoria se nazione estera e c'è flag efta in
128610950522     C* tabella nazione
128620950522     C     WDESIE        IFEQ      'E'
128630950522     C     DESEFT        ANDNE     *BLANKS
128640950412     C     VIDPIM        ANDEQ     *BLANKS
128650950412     C                   MOVEL     MSG(67)       VIDMSG
128660950412     C                   SETON                                        284990
128670950412     C                   GOTO      ENDCT8
128680950412     C                   END
128690941223     c*
128700941223   X1C                   ELSE
128710950104     c*
128720950104     C* VEDO SE ESISTE CNIND
128730950104     C     KACO          CHAIN     CNIND000                           32
128740950104     C*
128750950104     C     *IN32         IFEQ      *ON
128760950104     C                   MOVEL     MSG(63)       VIDMSG
128770950104     C                   SETON                                        479028
128780950104     C                   GOTO      ENDCT8
128790950104     C                   ENDIF
128800950104     C*
128810950104     C* CONTROLLO SUL CAP PROVINCIA NAZIONE
128820970423     C     INDSTA        IFEQ      *BLANKS
128830970423     C     INDPRV        ANDEQ     *BLANKS
128840950104     C                   MOVEL     MSG(64)       VIDMSG
128850950104     C                   SETON                                        479028
128860950104     C                   GOTO      ENDCT8
128870950104     C                   ENDIF
128880950104     C* CAP OBBLIGATORIO : CONTROLLO SE CONGRUENZA CON PROVINCIA
128890030612     C                   CLEAR                   Tisi95DS
128900030612     C                   CLEAR                   Fnlv13DS
128910970811     C                   MOVEL     INDSTA        I95NAR
128920970811     C                   MOVEL     INDCAE        I95CAP
128930970811     C                   MOVEL     INDPRV        I95PRV
128940970811     C                   MOVEL     INDCIT        I95LOC
128950970811     C                   MOVEL     '3'           I95TCN
128960970811     C                   MOVEL     ARBDSP        I95DAT
128970970811     C* ERRORE PER AFFIDABILITA' = 0
128980970811     C                   MOVEL     'S'           I13AF0
128990970811     C* CONTROLLO CONGRUENZA CAP/PROVINCIA
129000970811     C                   MOVEL     'S'           I13CNG
129010970811     C* CONTROLLO NON FORZABILE CITTA' CON VIARIO
129020970811     C                   MOVEL     'S'           I13CNV                         CAP CON VIAR
129030020305     c     lnpntw        ifeq      'FED'
129040020305     C                   movel     'S'           i95fi1
129050020305     c                   endif
129060970811     C                   CALL      'FNLV13R'
129070970811     C                   PARM                    KPJBA
129080030612     C                   PARM                    Fnlv13DS
129090030612     C                   PARM                    Tisi95DS
129100950104     C* ERRORE
129110970811     C     O13ERR        IFNE      *BLANKS
129120970811     C                   MOVEL     O13MSG        VIDMSG
129130950104     C                   MOVE      CPDC          VIDMSG
129140950104     C                   SETON                                        904728
129150950104     C                   GOTO      ENDCT8
129160950104    3C                   ENDIF
129170950104     C* SALVO IL FLAG ANTEPORTO
129180970811     C                   MOVEL     O95ISO        COMFAP
129190950104     C* SOLO IN VISUALIZZAZIONE
129200970811     C                   MOVEL     O95ISO        VIDFAP
129210140403     c* salvo pickup 2
129220140403     C                   MOVEL     O95gf2        comgf2
129230950104    2C                   ENDIF
129240941223     C*
129250941227     C     ENDCT8        ENDSR
129260961218     C*
129270941223     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE MT -----------------*
129280941227     C     REGIS8        BEGSR
129290130412     c                   clear                   wpda_ndc
129300130412     c                   clear                   wpda_ddc
129310130412     c                   clear                   wpda_dcm
129320950113     C                   MOVEL     ARBRSM        OLDRSM
129330950113     C                   MOVEL     ARBKSC        OLDKSC
129340950113     C                   MOVEL     ARBCAM        OLDCAM
129350041027     C                   MOVEL     ARBcpi        savCpi
129360950104     C*  GIRO UDATE
129370950104     C                   TIME                    W0140            14 0
129380950104     C* UDATE IN GGMMAAAA
129390950104     C                   MOVE      W0140         WDTGIO            8 0
129400950104     C*
129410950104     C* UDATE IN AAAAMMGG
129420950104     C                   Z-ADD     WDTGIO        G02DAT
129430950104     C                   MOVEL     *BLANK        G02ERR
129440950104     C                   CALL      'XSRDA8'
129450950104     C                   PARM                    WLBDAT
129460950104     C                   MOVEL     G02INV        DATEU             8 0
129470950104     C*
129480941223     C                   MOVEL     KNMUS         ARBPRU
129490950104     C*
129500950104     C* SALVO ARBCCM DELLA BOLLA PERCHE LO USO
129510950104     C                   MOVEL     ARBCCM        W0070             7 0
129520941223     C*
129530941223     C* INVIO PER SEDE
129540170116     C***                MOVEL     'FNARBM0T'    SFILE
129550941223     C* ALLOCO
129560170116     C***                EXSR      ALLOC
129570941223     C* ERRORI IN ALLOCAZIONE
129580170116    1C***  *IN91         IFEQ      *ON
129590170116     C***                GOTO      ENDRG8
129600170116    1C***                ENDIF
129610041025     c
129620041025     c* alloco record bolla in altro file
129630041025     c                   clear                   waltrabolla       1
129640041025     C   10KARB          CHAIN     FNBLP01L                           3091
129650041025     C  n10KARB          CHAIN     FNARB01L                           3091
129660041025     c*
129670041025     c                   if        *in91
129680041025     C                   MOVEL     MSG(4)        VIDMSG
129690170116     c****               exsr      DLCSED
129700070525     C                   SETON                                        2891
129710041025     C                   GOTO      ENDRG8
129720041025     c                   else
129730041025     c                   if        *in30
129740041025     c                   eval      waltrabolla='N'
129750041025     c                   else
129760041025     c                   eval      waltrocpi=arbcpi
129770041025     c                   eval      waltrofvf=arbfvf
129780041025     c                   eval      waltrovlf=arbvlf
129790130411     c  n10              eval      wpda_ndc=arbndc
129800130411     c  n10              eval      wpda_ddc=arbddc
129810130411     c  n10              eval      wpda_dcm=arbdcm
129820041025     C   10KARB          CHAIN     FNARB01L                           30
129830041025     C  N10KARB          CHAIN     FNBLP01L                           30
129840041025     c* Alla fine per non sporcarmi i campi del record, richaino il
129850041025     c*  record originale che sto variando
129860130411     c   10              eval      wpda_ndc=arbndc
129870130411     c   10              eval      wpda_ddc=arbddc
129880130411     c   10              eval      wpda_dcm=arbdcm
129890041025     c                   endif
129900041025     c                   endif
129910941223     C*
129920941223     C                   Z-ADD     DATEU         ARBDTV
129930941223     C                   TIME                    ARBORV
129940941223     C                   MOVEL     VIDCVR        ARBCVB
129950941223     C*
129960060130     C* SE DA STORICIZZARE
129970060130     C**   *IN10         IFEQ      *ON
129980060130    1C**   D66FSA        ANDEQ     'S'
129990060130     C**   *IN10         OREQ      *OFF
130000060130    1C**   D66FSP        ANDEQ     'S'
130010060130    1C     D66FSA        ifeq      'S'
130020941223     C                   Z-ADD     ARBKSC        ARBCCM
130030011108      *
130040011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
130050011108      * è in partenza 'P' o in arrivo 'A'
130060011108     C     *IN10         IFEQ      *ON
130070011108     C                   MOVE      'A'           ARBPRU
130080011108     C                   ENDIF
130090011108     C     *IN10         IFEQ      *OFF
130100011108     C                   MOVE      'P'           ARBPRU
130110011108     C                   ENDIF
130120011108      *
130130941223     C                   WRITE     FNARBM00
130140941223    1C                   END
130150060302     c* storicizzazione motivazione
130160060302     c     d66mtv        ifeq      'S'
130170060302     c                   exsr      sto_mtv
130180060302     c                   endif
130190941223     C*
130200950104     C                   Z-ADD     VIDKSC        ARBKSC
130210950104     C                   MOVEL     VIDKLP        ARBKSC
130220950104     C                   MOVEL     ARBKSC        ARBCCM
130230970613     C     ARBFDN        IFEQ      ' '
130240970613     C                   MOVEL     COMFAP        ARBFAP
130250970613     C                   ELSE
130260970613     C                   MOVEL     'C'           ARBFAP
130270970613     C                   ENDIF
130280970613     C**
130290020508    1C     VIDKSC        IFEQ      8888
130300941223     C                   MOVEL     VIDRSM        ARBRSM
130310941223     C                   MOVEL     VIDINM        ARBINM
130320941223     C                   MOVEL     VIDLOM        ARBLOM
130330941223     C                   MOVEL     VIDCAM        ARBCAM
130340941223     C                   MOVEL     VIDPRM        ARBPRM
130350941223     C                   MOVEL     VIDNZM        ARBNZM
130360941223     C                   CLEAR                   ARBCPI
130370950626     C*
130380950626     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
130390950626     C*     NELLA P.IVA
130400950626     C     VIDISM        IFEQ      '$$'
130410950626     C     VIDCPM        ANDEQ     *BLANKS
130420950626     C                   MOVEL     'PRIVATO'     VIDCPM
130430950626     C                   ENDIF
130440950626     C*
130450950104    2C     VIDISM        IFEQ      *BLANKS
130460941223     C                   MOVEL     VIDCPM        ARBCPI
130470941223     C                   ELSE
130480941223     C                   MOVEL     VIDPIM        ARBCPI
130490950104    2C                   ENDIF
130500950104     C*
130510950104     C                   ELSE
130520950412     C* PRENDO I DATI DA CNACO E CNIND
130530950104     C                   MOVEL     ACORAG        ARBRSM
130540950104     C                   MOVEL     INDVIA        ARBINM
130550950104     C                   MOVEL     INDCIT        ARBLOM
130560950104     C                   MOVEL     INDCAE        ARBCAM
130570950104     C                   MOVEL     INDPRV        ARBPRM
130580950104     C                   MOVEL     INDSTA        ARBNZM
130590950104     C                   MOVEL     INDIVA        ARBCPI
130600941223     C                   ENDIF
130610041027     c* Mi salvo laa new P.iva
130620041027     c                   eval      newcpi=arbcpi
130630041027     c
130640041025     c
130650060223     C* SE ESISTE RECORD P.IVA IN FIAR4  LO AGGIORNO
130660120413     c                   exsr      up_fiar4P
130670941223     C*
130680941223     C* PER SEDE
130690070503     C     D66FFI        IFEQ      'S'
130700950227     C*
130710950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
130720950413    2C     §3ABSD        IFEQ      ' '
130730170116     C****               OPEN      FNARBM0T
130740941227     C                   WRITE     FNARBMTT
130750170116     C***                CLOSE     FNARBM0T
130760950227    2C                   END
130770941223     C* DISALLOCO OGGETTO
130780170116     C***                EXSR      DLCSED
130790950208    1C                   END
130800941223     C*
130810941223     C* AGGIORNO FILE TASSAZIONE PADRONCINI
130820950113     C                   EXSR      AGGPMT
130830950104     C*  REIMPOSTO IL CODICE CLIENTE MITTENTE
130840950104     C                   MOVEL     W0070         ARBCCM
130850950104     C*
130860950113     C* CONTROLLO SE SI TRATTA DI VOLUME AUTOMATICO
130870950113     C* SE HA VARIATO IL MITTENTE
130880950113     C     ARBKSC        IFNE      OLDKSC
130890950113     C*
130900950113     C                   MOVEL     '7T'          COD
130910051214     C                   MOVEL(P)  ARBFVF        KEY
130920950113     C                   EXSR      CTABEL
130930950113     C                   MOVEL     TBLUNI        DS7T
130940950113     C     §7TRVL        IFEQ      'A'
130950950113     C     §7TRVL        OREQ      'P'
130960950114     C                   MOVEL     'S'           WRICVL            1
130970950113     C                   ENDIF
130980950113     C                   ENDIF
130990950113     C*
131000950114     C* SALVO I CAMPI COLLEGATI AL MITTENTE CHE USERO PER STORICIZZARE
131010950114     C                   MOVEL     ARBCTR        MITCTR
131020950114     C                   MOVEL     ARBFVF        MITFVF
131030950114     C                   MOVEL     ARBVLF        MITVLF
131040140403     c*
131050140403     c* aggiorno fiar5 record gen per pickup2
131060140407     c                   if        comgf2<>'01' and arbnzm=*blanks and
131070140407     c                             arbfdn=*blanks
131080140407     c                   eval      wpk2='S'
131090140407     c                   else
131100140407     c                   clear                   wpk2
131110140407     c                   endif
131120140407     c                   if        (wpk2='S' and par5pk2<>'S') or
131130140407     c                              (wpk2<>'S' and par5pk2='S')
131140140403     c                   eval      Wtipoagg='PIK'
131150140403     c                   exsr      regisar5gen
131160140403     c                   endif
131170950114     C*
131180950113     C* SE PREMUTO F10 DEVO FARE IL GIRO CON IL CODICE TARIFFA
131190950104     C     *INKJ         IFEQ      *ON
131200070907     C***                MOVEL     'TP'          D48CVB
131210070907     C***                MOVEL     'E'           WESEG
131220070907     c                   add       1             kk
131230070907     c                   movel     'TP'          kcvb(kk)
131240070911     c                   movel     d48ffr        kffr(kk)
131250070911     c                   movel     w48f12        kf12(kk)
131260070907     c
131270950113     C                   ELSE
131280950114     C*
131290950114     C* SE HO RIPROPOSTO IL CODICE TARIFFA DEL CLIENTE DEVO PASSARE
131300950114     C*  DALLA VIDEATA DELLA TASSAZIONE
131310950114     C                   MOVEL     ARBCTR        W003A             3
131320950114     C     W003A         IFNE      VIDCTR
131330070907     C***                MOVEL     'TP'          D48CVB
131340070907     C***                MOVEL     'E'           WESEG
131350070911     C****               CLEAR                   D48FFR
131360070907     c                   add       1             kk
131370070907     c                   movel     'TP'          kcvb(kk)
131380070911     c                   movel     ' '           kffr(kk)
131390070911     c                   movel     ' '           kf12(kk)
131400950114     C                   ELSE
131410950114     C*
131420950113     C* RICALCOLO VOLUME AUTOMATICO E POI VADO IN VIDEATA DEL VOLUME
131430950114     C     WRICVL        IFEQ      'S'
131440950113     C                   EXSR      CALVOL
131450950114     C                   MOVEL     D18FVF        ARBFVF
131460950114     C                   MOVEL     D18VLF        ARBVLF
131470070911     C***                MOVEL     'I2'          D48CVB
131480070911     C***                MOVEL     'E'           WESEG
131490070911     c***                clear                   w48f12
131500070907     c                   add       1             kk
131510070907     c                   movel     'I2'          kcvb(kk)
131520070911     c                   movel     d48ffr        Kffr(KK)
131530070911     c                   movel     ' '           Kf12(kk)
131540950104     C                   ENDIF
131550950113     C                   ENDIF
131560950114     C                   ENDIF
131570950114     C* MEMORIZZO IL COD TARIFFA SULLA BOLLA, TANTO SE E' CAMBIATO
131580950114     C*  PASSO PER FORZA DALLA VIDEATA DELLA TASSAZIONE PER POTERLO
131590950114     C*  TRASMETTERE
131600950114     C                   MOVEL     VIDCTR        ARBCTR
131610000623     C**
131620120416     c* Controllo e Gestione eventuale record W in fiar4
131630120416     c                   exsr      up_fiar4W
131640041027     c* In partenza memorizzo se 1°bolla franco anche se doppia
131650041027     c* In arrivo   memorizzo se 1°bolla franco e non    doppia
131660041027     c                   if        (*in10 and not *in05) or
131670041027     c                             (not *in10 and not *in06)
131680041027     c                   movel     savcpi        arbcpi
131690041027     c                   endif
131700950114     C*
131710950114     C   10              UPDATE    FNARB000
131720950114     C  N10              UPDATE    FNBLP000
131730041025     c*
131740041025     c* Aggiorno anche l'altra bolla
131750041025     c                   if        waltrabolla=' '
131760041025     c* Se  devo riaggiornare il volume benen altrimenti tengo quello d
131770041025     c*  della bolla
131780041025     c                   if        wricvl<>'S'
131790041025     c                   eval      arbfvf=waltrofvf
131800041027     c                   eval      arbvlf=waltrovlf
131810041025     c                   endif
131820041027     c
131830041027     c* P.IVA  idem come sopra
131840041027     c                   if        (*in10 and *in06) or
131850041027     c                             (not *in10 and *in05)
131860041027     c                   movel     newcpi        arbcpi
131870041027     c                   else
131880041027     c                   movel     waltrocpi     arbcpi
131890041027     c                   endif
131900041025     c
131910041025     C   10              except    arbMblp
131920041025     C  N10              except    arbMarb
131930041025     c                   endif
131940950114     C*
131950950114     C* DISALLOCO I RECORD CHE NON USO
131960991007     C   10              UNLOCK    FIAR601L
131970130411     C*
131980130411     c* Richiamo routine per invio variazione a pda
131990130411     c                   exsr      sr_pda
132000950602     C*
132010941227     C     ENDRG8        ENDSR
132020950113     C*
132030961219     C*--- CONTROLLI RIFERIMENTO MITTENTE ----------------------------*
132040961219     C     CONTR9        BEGSR
132050961219     C                   SETOFF                                       9028
132060061024     C*
132070061024     C* NATURA MERCE SEMPRE OBBLIGATORIA
132080061024     C     VIDNAS        IFEQ      *BLANKS
132090061024     C                   SETON                                        419028
132100061024     C                   MOVEL     MSG(75)       VIDMSG
132110061024     C                   END
132120961219     C* SE DESTINATARIO ESTERO IL RIFERIMENTO E' OBBLIGATORIO
132130061024     c* Solo se c'è attraversamento dogana (come fa FNLS01R)
132140961219     C     WDESIE        IFEQ      'E'
132150970110     C     VIDRMA        ANDEQ     *BLANKS
132160061024     C     deseft        ANDne     *BLANKS
132170961219     C                   SETON                                        409028
132180961219     C                   MOVEL     MSG(74)       VIDMSG
132190961219     C                   END
132200961219     C*
132210961219     C                   ENDSR
132220060316     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE RM -----------------*
132230060316     C     REGIS9        BEGSR
132240130412     c                   clear                   wpda_ndc
132250130412     c                   clear                   wpda_ddc
132260130412     c                   clear                   wpda_dcm
132270060316     C*  GIRO UDATE
132280060316     C                   TIME                    W0140            14 0
132290060316     C* UDATE IN GGMMAAAA
132300060316     C                   MOVE      W0140         WDTGIO            8 0
132310060316     C*
132320060316     C* UDATE IN AAAAMMGG
132330060316     C                   Z-ADD     WDTGIO        G02DAT
132340060316     C                   MOVEL     *BLANK        G02ERR
132350060316     C                   CALL      'XSRDA8'
132360060316     C                   PARM                    WLBDAT
132370060316     C                   MOVEL     G02INV        DATEU             8 0
132380060316     C*
132390060316     C                   MOVEL     KNMUS         ARBPRU
132400060316     C* INVIO PER SEDE
132410170116     C***                MOVEL     'FNARBD0T'    SFILE
132420060316     C* ALLOCO
132430170116     C***                EXSR      ALLOC
132440060316     C* ERRORI IN ALLOCAZIONE
132450170116    1C***  *IN91         IFEQ      *ON
132460170116     C***                GOTO      ENDRG9
132470170116    1C***                ENDIF
132480060316     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
132490060316     c* iornare direttamente il file
132500060316     c                   clear                   waltrabolla       1
132510060316     C   10KARB          CHAIN     FNBLP01L                           3091
132520060316     C  n10KARB          CHAIN     FNARB01L                           3091
132530060316     c*
132540060316     c                   if        *in91
132550060316     C                   MOVEL     MSG(4)        VIDMSG
132560170116     c****               exsr      DLCSED
132570070525     C                   SETON                                        2891
132580060316     C                   GOTO      ENDRG9
132590060316     c                   else
132600060316     c                   if        *in30
132610060316     c                   eval      waltrabolla='N'
132620060316     c                   else
132630130411     c  n10              eval      wpda_ndc=arbndc
132640130411     c  n10              eval      wpda_ddc=arbddc
132650130411     c  n10              eval      wpda_dcm=arbdcm
132660060316     C   10KARB          CHAIN     FNARB01L                           30
132670060316     C  N10KARB          CHAIN     FNBLP01L                           30
132680060316     c* Alla fine per non sporcarmi i campi del record, richaino il
132690060316     c*  record originale che sto variando
132700130411     c   10              eval      wpda_ndc=arbndc
132710130411     c   10              eval      wpda_ddc=arbddc
132720130411     c   10              eval      wpda_dcm=arbdcm
132730060316     c                   endif
132740060316     c                   endif
132750060316     C                   Z-ADD     DATEU         ARBDTV
132760060320     C                   TIME                    ARBORV
132770060410     C                   MOVEL     VIDCVR        ARBCVB
132780060316     c* STORICIZZAZIONE
132790060316    1C     D66FSA        ifeq      'S'
132800060316      *
132810060316      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
132820060316      * è in partenza 'P' o in arrivo 'A'
132830060316     C     *IN10         IFEQ      *ON
132840060316     C                   MOVE      'A'           ARBPRU
132850060316     C                   ENDIF
132860060316     C     *IN10         IFEQ      *OFF
132870060316     C                   MOVE      'P'           ARBPRU
132880060316     C                   ENDIF
132890060316      *
132900060316     c* nella ragione sociale devo mettere rma e nas ma prima devo
132910060316     c* salvare i campi della ragione sociale
132920060316     c                   movel     arbrsd        s_arbrsd
132930060316     c                   movel     arbrd2        s_arbrd2
132940060316     c                   movel(P)  arbrma        arbrsd
132950060316     c                   movel(P)  arbnas        arbrd2
132960060316     c
132970060316     C                   WRITE     FNARBd00
132980060316     c                   movel     s_arbrsd      arbrsd
132990060316     c                   movel     s_arbrd2      arbrd2
133000060316     C                   END
133010060316     c* storicizzazione motivazione
133020060316     c     d66mtv        ifeq      'S'
133030060316     c                   exsr      sto_mtv
133040060316     c                   endif
133050060316     C* Ora invio solo a sede
133060060316    1C     D66FFI        IFEQ      'S'
133070060316     C*
133080060316     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
133090060316    2C     §3ABSD        IFEQ      ' '
133100170116     C***                OPEN      FNARBD0T
133110060316     c                   movel     arbrsd        s_arbrsd
133120060316     c                   movel     arbrd2        s_arbrd2
133130060316     c                   movel(p)  vidrma        arbrsd
133140060316     c                   movel(p)  vidnas        arbrd2
133150060316     C                   WRITE     FNARBDTT
133160060316     c                   movel     s_arbrsd      arbrsd
133170060316     c                   movel     s_arbrd2      s_arbrd2
133180170116     C****               CLOSE     FNARBD0T
133190060316    2C                   END
133200060316     C* DISALLOCO OGGETTO
133210170116     C***                EXSR      DLCSED
133220060316    1C                   END
133230060316     C*
133240060316     C* F6 - AGGIORNAMENTO
133250060316     C                   MOVE      VIDRMA        ARBRMA
133260060316     C                   MOVE      VIDNAS        ARBNAS
133270060316     C   10              UPDATE    FNARB000
133280060316     C  N10              UPDATE    FNBLP000
133290060316     c
133300060316     c                   if        waltrabolla=' '
133310060316     C                   MOVE      VIDRMA        ARBRMA
133320060316     C                   MOVE      VIDNAS        ARBNAS
133330060316     C   10              except    RMblp
133340060316     C  N10              except    RMarb
133350060316     c                   endif
133360130411     C*
133370130411     c* Richiamo routine per invio variazione a pda
133380130411     c                   exsr      sr_pda
133390060316     c     endrg9        endsr
133400030123      *
133410030123      *--- Controllo campi mancato ritiro BANCALI --------------------*
133420030123     c     Contr10       BegSr
133430030123     c                   Eval      *In90 = *Off
133440030127
133450030127      * Il numero dei bancali non ritirati è obbligatorio
133460030127     c                   If        VidRb1 = *Zeros and VidRb2 = *Zeros
133470030127     c                   Eval      VidMsg = Msg(125)
133480030127     c                   Eval      *In60 = *On
133490030127     c                   Eval      *In28 = *On
133500030127     c                   Eval      *In90 = *On
133510030127     c                   GoTo      EndCt10
133520030127     c                   EndIf
133530030124      * Controllo il n. dei bancali non ritirati
133540030124     c                   If        VidRb1 > VidNba
133550030124     c                   Eval      VidMsg = Msg(124)
133560030124     c                   Eval      *In60 = *On
133570030124     c                   Eval      *In28 = *On
133580030124     c                   Eval      *In90 = *On
133590030124     c                   GoTo      EndCt10
133600030124     c                   EndIf
133610030124     c                   If        VidRb2 > VidNb2
133620030124     c                   Eval      VidMsg = Msg(124)
133630030124     c                   Eval      *In61 = *On
133640030124     c                   Eval      *In28 = *On
133650030124     c                   Eval      *In90 = *On
133660030124     c                   GoTo      EndCt10
133670030124     c                   EndIf
133680030123      * Nominativo
133690030123     c                   If        VidNomb = *Blanks
133700030124     c                   Eval      VidMsg = Msg(118)
133710030124     c                   Eval      *In57 = *On
133720030123     c                   Eval      *In28 = *On
133730030123     c                   Eval      *In90 = *On
133740030123     c                   GoTo      EndCt10
133750030123     c                   EndIf
133760030123      * Motivo
133770030123     c                   If        VidMotb = *Blanks
133780030124     c                   Eval      VidMsg = Msg(120)
133790030124     c                   Eval      *In59 = *On
133800030123     c                   Eval      *In28 = *On
133810030123     c                   Eval      *In90 = *On
133820030123     c                   GoTo      EndCt10
133830030123     c                   EndIf
133840030123
133850030123     c     EndCt10       EndSr
133860030123      **
133870030123      *--- AGGIORNAMENTO FILE FIAR5 ----------------------------------*
133880030123     c     Regis10       BegSr
133890130412
133900130412     c                   clear                   wpda_ndc
133910130412     c                   clear                   wpda_ddc
133920130412     c                   clear                   wpda_dcm
133930030123
133940030124     c                   Eval      §Ar5Prub = Knmus
133950060131     c                   Eval      §Ar5Poub = dutPou
133960030124     c                   Move      Dateu         §Ar5Dav
133970030124     c                   Movel     W0140         §Ar5Orv
133980030124     c                   Eval      §Ar5Rb1 = VidRb1
133990030124     c                   Eval      §Ar5Rb2 = VidRb2
134000030124     c                   Eval      §Ar5Nomb = VidNomb
134010030124     c                   Eval      §Ar5Motb = VidMotb
134020030124     c                   Movel(p)  DAr5Ban       Ar5Uni
134030030124     c                   Clear                   Ar5Ft1
134040030124     c                   Clear                   Ar5Ft2
134050030124     c                   Clear                   Ar5Ft3
134060030124     c                   Update    Fiar5000
134070041020     c*
134080041020     c* aggiornamento file fiqdt00f per gestione rientri
134090041020     c                   z-add     arbifp        kfgs
134100041020     c                   z-add     arbndc        w0060             6 0
134110041020     c     kqdt          chain     fiqdt01l
134120041020     c                   if        %found(fiqdt01l)
134130041020     c                   add       vidrb1        qdtnbnbr
134140041020     c                   add       vidrb2        qdtnbnbr
134150041020     c                   except    aggqdt
134160041020     c                   endif
134170060302     c* storicizzazione motivazione
134180060302     c     d66mtv        ifeq      'S'
134190060320     c                   Move      Dateu         arbdtv
134200060320     c                   movel     w0140         arborv
134210060302     c                   exsr      sto_mtv
134220060302     c                   endif
134230130411     C*
134240130412     c                   clear                   waltrabolla       1
134250130412     c                   if        *in10
134260130412     c                   eval      wpda_ndc=arbndc
134270130412     c                   eval      wpda_ddc=arbddc
134280130412     c                   eval      wpda_dcm=arbdcm
134290130412     c                   unlock    fnarb01l
134300130412     c                   else
134310130412     c                   unlock    fnblp01l
134320131120     c                   if        d66pda<>' '
134330130412     C     KARB          CHAIN(n)  FNARB01L                           30
134340130412     c                   if        *in30
134350130412     c                   eval      waltrabolla='N'
134360130412     c                   else
134370130412     c                   eval      wpda_ndc=arbndc
134380130412     c                   eval      wpda_ddc=arbddc
134390130412     c                   eval      wpda_dcm=arbdcm
134400130412     c                   endif
134410130412     c                   endif
134420130412     c                   endif
134430130412     c*
134440130412     c* Richiamo routine per invio variazione a pda
134450130411     c                   exsr      sr_pda
134460030123
134470030123     c                   EndSr
134480061107     C
134490061107     C*--- CONTROLLI FORMATO 11 --------------------------------------*
134500071025     C     CONTR11       BEGSR
134510061107     C                   SETOFF                                       9028
134520151105     c                   clear                   xcfiva1ds
134530151105     c                   clear                   flgiva            1 0
134540151105     c                   move      v11cod        w0040
134550080505     c
134560061107     c* se non previsto inserimento, ERRORE
134570071025    1c                   if        *in76
134580061107     C                   SETON                                        289040
134590071025    2C                   IF        v11MIDE='M'
134600061107     C                   MOVEL     MSG(92)       VIDMSG
134610061107     C                   ELSE
134620061107     C                   MOVEL     MSG(95)       VIDMSG
134630071025    2c                   endif
134640061107     C                   GOTO      ENDCT11
134650071025    1c                   endif
134660061107     c* flag unico se cliente italia
134670071025    1c                   if        v11mide='M'
134680061107     c                   eval      wcliie=wmitie
134690061107     c                   eval      wprv=arbprm
134700151105     c                   eval      wcap=arbcam
134710151105     c                   eval      wloc=arblom
134720061107     c                   else
134730061107     c                   eval      wcliie=wdesie
134740061107     c                   eval      wprv=arbprd
134750151105     c                   eval      wcap=arbcad
134760151105     c                   eval      wloc=arblod
134770071025    1c                   endif
134780071025     c
134790080505     c* Controllo la Partita IVA
134800071025     C* Se P.IVA sprotetta e nazione estera con flag efta la P.IVA è
134810071025     C* obbligatoria
134820071025     c
134830071025    1c                   if        v11pid=*blanks
134840071025     c
134850071025    2c                   if        wdesie='E' and deseft<>*blanks
134860071025     C                   SETON                                        284190
134870071025     C                   MOVEL     MSG(67)       VIDMSG
134880071025     C                   GOTO      ENDCT11
134890120507     c****               else
134900071031     c* solo se non richiamato nascosto
134910120507    3c*****              if        d48ffr='V' or d48ffr='S'
134920120507    4c****               if        wcliie='I' and   forzapid=' '
134930120507     c*****                        and dateu>=§vpopiv
134940120507     C******             SETON                                        289041
134950120507     C*****              MOVEL     MSG(57)       VIDMSG
134960120507     c******             movel     '1'           forzapid
134970120507     C********           GOTO      ENDCT11
134980120507    4c*********          endif
134990120507    3c********           endif
135000071031     c
135010071025    2c                   endif
135020071025    1C                   ENDif
135030071025     C*
135040151105    1c***                if        v11pid<>*blanks
135050071025     c* Se cambiata pulisco la forzatura
135060071025    2c                   if        v11pid<>sav11pid
135070071025     c                   clear                   forzapid2
135080071025     c                   movel     v11pid        sav11pid
135090071025    2c                   endif
135100071025     c
135110151105     c                   clear                   xcfiva1ds
135120071025     c                   clear                   xcfivamod
135130151105     c                   eval      xcfivafori= 'S'
135140100120     c                   eval      xcfivamod = 'P'
135150071025     c                   eval      xcfivapar = v11pid
135160071025     c                   eval      xcfivanar = wnar
135170071025     c                   eval      xcfivaprv = wprv
135180151105     c                   eval      xcfivacap = wcap
135190151105     c                   eval      xcfivaloc = wloc
135200151105     c                   if        w0040=8888 or w0040=9999
135210151105     c                   eval      xcfivap88='8'
135220151105     c                   endif
135230151105     c                   call      'XCFIVAR1'
135240151105     c                   parm                    xcfiva1ds
135250071025     C*
135260071025      * --> errata forzabile
135270100120    2c                   If        xcfivaflg = -9 and forzaPID2 = ' '
135280071025     c                   seton                                        289041
135290071025     c                   Eval      vidmsg = xcfivamsg
135300071025     c                   Eval      vidmsg = %trim(vidmsg) + ' Enter x forzare'
135310100120     c                   eval      forzaPID2= '1'
135320071025     C                   GOTO      ENDCT11
135330071025    2c                   EndIf
135340071025      * --> errore
135350071025    2c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
135360071025     c                   seton                                        289041
135370071025     c                   eval      vidmsg = xcfivamsg
135380071025     C                   GOTO      ENDCT11
135390071025    2c                   EndIf
135400151105
135410151105     c                   eval      flgiva=xcfivaflg
135420071025     c
135430071025      * se non impostato devo riportare il codice iso della partita iva che
135440071025      * mi viene passato dal pgm di controllo
135450151105    2c                   if        v11pid<>*blanks and v11isd <> ivaeu
135460071025     c                   eval      v11isd = ivaeu
135470071025     c                   seton                                          9041
135480071025     C                   GOTO      ENDCT11
135490071025    2c                   endif
135500151105    1c****               endif
135510080505     c
135520080505      * Devo richiamare il nuovo driver fatto per controllare il codice
135530080505      * fiscale
135540151105     c                   clear                   xcfiva1ds
135550080505     c                   clear                   xcfivamod
135560080505     c                   eval      xcfivapar = v11cdfis
135570080505     c                   eval      xcfivanar = wnar
135580080505     c                   eval      xcfivaprv = wprv
135590151105     c                   eval      xcfivacap = wcap
135600151105     c                   eval      xcfivaloc = wloc
135610151105     c                   eval      xcfivapiva= v11pid
135620151105     c                   if        w0040=8888 or w0040=9999
135630151105     c                   eval      xcfivap88='8'
135640151105     c                   endif
135650151105     c                   call      'XCFIVAR1'
135660151105     c                   parm                    xcfiva1ds
135670080505     c*
135680080505    1c                   if        xcfivaflg < *zeros
135690080505     C                   SETON                                        289040
135700080505     C                   MOVEL     xcfivamsg     VIDMSG
135710080505     C                   GOTO      ENDCT11
135720080505    1c                   endif
135730080505     c
135740080505     c* Obbligatorio se cliente italia partita iva o codice fiscale
135750080505     c* solo se non richiamato nascosto
135760120507    1c***                if        d48ffr='V' or d48ffr='S'
135770120507    2c****               if        wcliie='I' and   v11cdfis=*blanks
135780120507     c****                         and forzacdf=' ' and xcfivaflg<>9
135790120507     C****               SETON                                        289040
135800120507     C*****              MOVEL     MSG(23)       VIDMSG
135810120507     c****               movel     '1'           forzacdf
135820120507     C*****              GOTO      ENDCT11
135830120507    2c*****              endif
135840120507    1c******             endif
135850080505     c
135860080505     c* Se entrambe vuote errore
135870080505    1c                   if        v11cdfis=*blanks and v11pid=*blanks
135880080505     C                   SETON                                        284090
135890080505     C                   MOVEL     MSG(145)      VIDMSG
135900080505     C                   GOTO      ENDCT11
135910080505    1c                   endif
135920120508     c* Se fattura immediata errore se partita iva '$$'
135930120508     c                   if        %scan('$$':v11pid)>0  and wcliie='I'
135940120508     c                   move      v11cod        w0040
135950120508     c                   if        w0040=8888 or w0040=9999
135960120507     C                   SETON                                        284190
135970120507     C                   MOVEL     MSG(149)      VIDMSG
135980120507     C                   GOTO      ENDCT11
135990120508     c                   endif
136000120507     c                   endif
136010120508     c* solo se non richiamato nascosto
136020130315    3c                   if        d48ffr='V' or d48ffr='S'
136030120507     c* Se codice fiscale lungo 11 si richiede la partita IVA  - Errore forzabile
136040120507     c                   if        v11cdfis<>*blanks and v11pid=*blanks and
136050120507     c                             %subst(v11cdfis:12:1)=' ' and wcliie='I'
136060120507     c                             and forzapid=' '
136070120507     C                   SETON                                        289041
136080120507     C                   MOVEL     MSG(57)       VIDMSG
136090120507     c                   movel     '1'           forzapid
136100120507     C                   GOTO      ENDCT11
136110120507     c                   endif
136120130315     c                   endif
136130061107     C*
136140061107     C     ENDCT11       ENDSR
136150130927     c
136160130927     C*--- CONTROLLI FORMATO 12 --------------------------------------*
136170130927     C     CONTR12       BEGSR
136180130927     C                   SETOFF                                       9028
136190130930     c* controllo indirizzo email
136200131001     c                   if        *in86  and videml<>*blanks
136210130930     c                   Clear                   dsemail
136220130930     c                   Eval      §emlindi = videml
136230130930     c                   Call      'XEMAIL'
136240130930     c                   Parm                    dsemail
136250130930if  3c                   If        §emlerro <> '0'
136260130930     C                   SETON                                        289041
136270130930     C                   MOVEL     MSG(150)      VIDMSG
136280130930     C                   GOTO      ENDCT12
136290130930     c                   endif
136300130930
136310130930     c                   endif
136320130930     c
136330130927     C     ENDCT12       ENDSR
136340061107     C*
136350061107     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE FI -----------------*
136360061107     C     REGIS11       BEGSR
136370130412
136380130412     c                   clear                   wpda_ndc
136390130412     c                   clear                   wpda_ddc
136400130412     c                   clear                   wpda_dcm
136410061107     C*  GIRO UDATE
136420061107     C                   TIME                    W0140            14 0
136430061107     C* UDATE IN GGMMAAAA
136440061107     C                   MOVE      W0140         WDTGIO            8 0
136450061107     C*
136460061107     C* UDATE IN AAAAMMGG
136470061107     C                   Z-ADD     WDTGIO        G02DAT
136480061107     C                   MOVEL     *BLANK        G02ERR
136490061107     C                   CALL      'XSRDA8'
136500061107     C                   PARM                    WLBDAT
136510061107     C                   MOVEL     G02INV        DATEU             8 0
136520061107     C                   MOVEL     KNMUS         ARBPRU
136530071025     c*
136540071025     C     V11ISD        IFEQ      '$$'
136550071025     C     V11CPD        ANDEQ     *BLANKS
136560071025     C                   MOVEL     'PRIVATO'     V11CPD
136570071025     C                   ENDIF
136580071025     c
136590071025     c* Salvo a parte la partita iva presente in bolla
136600071025     c                   movel     arbcpi        bollacpi
136610061107     C*
136620061129     c* Se la bolla non è trasmessa non devo allocare il file e non devo
136630061129     c*    creare la variazione
136640170116     c***                if        bollaft2<>' '
136650061107     C* INVIO PER SEDE
136660170116     C***                MOVEL     'FNARBD0T'    SFILE
136670061107     C* ALLOCO
136680170116     C***                EXSR      ALLOC
136690061107     C* ERRORI IN ALLOCAZIONE
136700170116    1C**   *IN91         IFEQ      *ON
136710170116     C***                GOTO      ENDRG11
136720170116    1C***                ENDIF
136730170116    1C***                ENDIF
136740061107     c
136750061107     c* Alloco il record fiar4 del codice fiscale                      agg
136760061107     C                   MOVEL     'Q'           WTRC
136770061107     C     KARB2         CHAIN     FiAR4000                           3091
136780061107     c*
136790061107     c                   if        *in91
136800061107     C                   MOVEL     MSG(4)        VIDMSG
136810170116     c****               exsr      DLCSED
136820070525     C                   SETON                                        2891
136830061107     C                   GOTO      ENDRG11
136840061107     c                   endif
136850061107     c
136860061107     c                   if        *in30
136870061107     c                   eval      esiste='N'
136880061116     c                   clear                   ar4not
136890061107     c                   else
136900061107     c                   eval      esiste='S'
136910061107     c                   endif
136920071026     c*
136930071026     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
136940071026     c* iornare direttamente il file solo se non si tratta di bolla doppia
136950071026     c                   clear                   waltrabolla       1
136960130412     c                   setoff                                       3091
136970130412    1c                   if        §3atb2=*blanks and aggiobol<>'N'
136980071026     C   10KARB          CHAIN     FNBLP01L                           3091
136990071026     C  n10KARB          CHAIN     FNARB01L                           3091
137000130412     c                   else
137010131120     c                   if        d66pda<>' '
137020130412     c                   if        not *in10
137030130412     C     KARB          CHAIN(n)  FNARB01L                           30
137040130412     c                   endif
137050130412     c                   eval      wpda_ndc=arbndc
137060130412     c                   eval      wpda_ddc=arbddc
137070130412     c                   eval      wpda_dcm=arbdcm
137080130412     c                   endif
137090130412    1c                   endif
137100071026     c*
137110071026    2c                   if        *in91
137120071026     C                   MOVEL     MSG(4)        VIDMSG
137130071026     c*
137140170116     c****               exsr      DLCSED
137150071026     c                   unlock    fiar401l
137160071026     c
137170071026     C                   SETON                                        2891
137180071026     C                   GOTO      ENDRG11
137190071026   x2c                   else
137200071026    3c                   if        *in30
137210071026     c                   eval      waltrabolla='N'
137220071026   x3c                   else
137230130412    4c                   if        §3atb2=*blanks and aggiobol<>'N'
137240130411     c  n10              eval      wpda_ndc=arbndc
137250130411     c  n10              eval      wpda_ddc=arbddc
137260130411     c  n10              eval      wpda_dcm=arbdcm
137270071026     C   10KARB          CHAIN     FNARB01L                           30
137280071026     C  N10KARB          CHAIN     FNBLP01L                           30
137290071026     c* Alla fine per non sporcarmi i campi del record, richaino il
137300071026     c*  record originale che sto variando
137310130411     c   10              eval      wpda_ndc=arbndc
137320130411     c   10              eval      wpda_ddc=arbddc
137330130411     c   10              eval      wpda_dcm=arbdcm
137340130412    4c                   endif
137350071026    3c                   endif
137360071026    2c                   endif
137370130412    1c****               endif
137380061107     C*
137390061107     C                   Z-ADD     DATEU         ARBDTV
137400061107     C                   TIME                    ARBORV
137410061107     C                   MOVEL     VIDCVR        ARBCVB
137420061107     c* Memorizzo se si tratta del mittente o destinatario
137430180212     c                   movel(p)  v11mide       arbrd2
137440061107     c
137450061107     c* clearo gli altri campi del file che non mi interessano
137460061107     c                   clear                   arbrsd
137470061107     c                   clear                   arbind
137480061107     c                   clear                   arbcad
137490061107     c                   clear                   arblod
137500061107     c                   clear                   arbprd
137510061107     c                   clear                   arbnzd
137520061107     c                   clear                   arbfin
137530061107     c                   clear                   arbpkb
137540061107     c                   clear                   arbpkf
137550061107     c                   clear                   arbvlb
137560061107     c                   clear                   arbvlf
137570061107     c                   clear                   arbfvb
137580061107     c                   clear                   arbfvf
137590061107     c                   clear                   arbffd
137600061107     c                   clear                   arbfst
137610061107     C*
137620061107     C* SE DA STORICIZZARE
137630061107    1C     D66FSA        ifeq      'S'
137640061107      *
137650061107      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
137660061107      * è in partenza 'P' o in arrivo 'A'
137670061107     C     *IN10         IFEQ      *ON
137680061107     C                   MOVE      'A'           ARBPRU
137690061107     C                   ENDIF
137700061107     C     *IN10         IFEQ      *OFF
137710061107     C                   MOVE      'P'           ARBPRU
137720061107     C                   ENDIF
137730061107     c                   if        v11mide='M'
137740061107     c                   movel     ar4not        arbcpi
137750061107     c                   else
137760061107     c                   move      ar4not        arbcpi
137770061107     c                   endif
137780071025     c* Partita iva
137790071025     c                   movel     bollacpi      arbrsd
137800061107      *
137810061107     C                   WRITE     FNARBD00
137820061107    1C                   ENDIF
137830061107    c
137840061107     c* storicizzazione motivazione
137850061107     c     d66mtv        ifeq      'S'
137860061107     c                   exsr      sto_mtv
137870061107     c                   endif
137880071025     c* Aggiorno campo codice fiscale
137890061107     c                   movel     v11cdfis      arbcpi
137900071025     c* Aggiorno campo per la partita iva
137910071025     C                   MOVEL     V11PID        ARBrsd
137920071025     C* LA P.IVA SE HA CODICE ISO VIENE MESSO A SIN ALTRIMENTI SUBITO
137930071025     C*  IL CODICE DELLA P.IVA
137940071025     C     V11CPD        IFNE      *BLANKS
137950071025     C     V11ISD        ANDEQ     *BLANKS
137960071025     C                   CLEAR                   ARBrsd
137970071025     C                   MOVEL     V11CPD        ARBrsd
137980071025     C                   ENDIF
137990130402     c* Se Cd.Fisc. lungo 11 lo metto anche nella P.Iva se vuota
138000130402     c                   if        v11pid=*blanks and %subst(v11cdfis:12:1)=' '
138010130402     c                   movel     v11cdfis      arbrsd
138020130402     c                   endif
138030071025     c
138040071025     C                   MOVEL     'P'           WTRC
138050071025     C     KARB2         CHAIN     FIAR401L                           30
138060071025    1C     *IN30         IFEQ      *OFF
138070071025    2c                   if        v11mide='M'
138080071025     c                   movel     ar4not        wcpifile
138090071025   x2c                   else
138100071025     c                   move      ar4not        wcpifile
138110071025    2c                   endif
138120071025    2c                   if        wcpifile <> arbrsd
138130071025     c                   movel     'T'           ar4ftr
138140071025     c                   z-add     dateu         ar4dtr
138150071025     c                   z-add     dateu         ar4duv
138160071025    3c                   if        v11mide='M'
138170071025     c                   eval      %subst(ar4not:1:16)=%subst(arbrsd:1 :16)
138180071025     c                   else
138190071025     c                   eval      %subst(ar4not:20:16)=%subst(arbrsd:1:16)
138200071025    3c                   endif
138210071025     c
138220071025     C                   UPDATE    FIAR4000
138230071025   x2c                   else
138240071025     c                   unlock    fiar401l
138250071025    2c                   endif
138260071025    1c                   endif
138270061129     c
138280071025    0c                   if        bollaft2<>' '
138290061107     C* Ora invio solo a sede
138300061107     C     D66FFI        IFEQ      'S'
138310061107     C*
138320061107     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
138330061107    2C     §3ABSD        IFEQ      ' '
138340170116     C***                OPEN      FNARBD0T
138350061107     C                   WRITE     FNARBDTT
138360170116     C***                CLOSE     FNARBD0T
138370061107    2C                   END
138380061107     C* DISALLOCO OGGETTO
138390170116     C**                 EXSR      DLCSED
138400061107    1C                   END
138410071025    0C                   END
138420061107     C*
138430061107     C* AGGIORNO  file: 30 on write del record
138440071025     C                   MOVEL     'Q'           WTRC
138450071025     C     KARB2         CHAIN     FiAR4000                           3091
138460061107     c                   if        Esiste='N'
138470061107     c                   movel     arbaas        ar4aas
138480061107     c                   movel     arblnp        ar4lnp
138490061107     c                   movel     arbnrs        ar4nrs
138500061107     c                   movel     arbnsp        ar4nsp
138510061107     c                   movel     'Q'           ar4trc
138520061107     c                   clear                   ar4not
138530061107     c                   clear                   ar4flo
138540061107     c                   endif
138550061107     c                   if        v11mide='M'
138560061107     c                   movel     v11cdfis      ar4not
138570061107     c                   else
138580061107     c                   move      v11cdfis      ar4not
138590061107     c                   endif
138600061107     c                   movel     'T'           ar4ftr
138610061107     c                   z-add     dateu         ar4dtr
138620061107     c                   z-add     dateu         ar4duv
138630061107     c
138640061116     c                   if        Esiste='N'  and ar4not<>*blanks
138650061107     c                   write     fiar4000
138660061116     c                   endif
138670061116     c                   if        Esiste='S'  and ar4not<>*blanks
138680061107     c                   update    fiar4000
138690061107     c                   endif
138700061116     c                   if        Esiste='S'  and ar4not=*blanks
138710061116     c                   delete    fiar4000
138720061116     c                   endif
138730071025     c
138740071026     c* Aggiorno la bolla per la partita iva solo se non richiamamto da I0
138750071026     c*  per cui ho già aggiornato la partita iova sulla bolla
138760071026     c                   if        Aggiobol<>'N'
138770071025     c                   movel     arbrsd        arbcpi
138780071025     c* aggiornamento mirato per non sporcare degli altri campi
138790071025     c  n10              except    SolocpiP
138800071025     c   10              except    SolocpiA
138810071026     c
138820071026     c* Se 'c'e' l'altra ed è bolla singola la aggiorno
138830071026     c                   if        §3atb2=*blanks and waltrabolla=' '
138840071026     c  n10              except    SolocpiA
138850071026     c   10              except    SolocpiP
138860071026     c                   endif
138870071026     c                   endif
138880061107     C*
138890061107     C* DISALLOCO  I FILES CHE NON USO
138900061107     C   10              UNLOCK    FiAR901L
138910071025     c** 10              unlock    fnarb01l
138920071025     c**N10              unlock    fnblp01l
138930130411     C*
138940130411     c* Richiamo routine per invio variazione a pda
138950130411     c                   exsr      sr_pda
138960061107     C*
138970061107     C     ENDRG11       ENDSR
138980130927     C*
138990130930     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE Nx -----------------*
139000130927     C     REGIS12       BEGSR
139010130927     c
139020130927     C*  GIRO UDATE
139030130927     C                   TIME                    W0140            14 0
139040130927     C* UDATE IN GGMMAAAA
139050130927     C                   MOVE      W0140         WDTGIO            8 0
139060130927     C*
139070130927     C* UDATE IN AAAAMMGG
139080130927     C                   Z-ADD     WDTGIO        G02DAT
139090130927     C                   MOVEL     *BLANK        G02ERR
139100130927     C                   CALL      'XSRDA8'
139110130927     C                   PARM                    WLBDAT
139120130927     C                   MOVEL     G02INV        DATEU             8 0
139130130927     C                   MOVEL     KNMUS         ARBPRU
139140130927     C                   Z-ADD     DATEU         ARBDTV
139150130927     C                   TIME                    ARBORV
139160130927     C                   MOVEL     VIDCVR        ARBCVB
139170131001      *
139180131001      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
139190131001      * è in partenza 'P' o in arrivo 'A'
139200131001     C     *IN10         IFEQ      *ON
139210131001     C                   MOVE      'A'           ARBPRU
139220131001     C                   ENDIF
139230131001     C     *IN10         IFEQ      *OFF
139240131001     C                   MOVE      'P'           ARBPRU
139250131001     C                   ENDIF
139260130927     C*
139270130927     C* SE DA STORICIZZARE
139280130927    1C     D66FSA        ifeq      'S'
139290130930
139300131001     c                   if        *in84 and
139310131001     c                                  (vidref<>par5ref or vidteld<>par5teld)
139320130930     c* Referente e consegna
139330130927     c                   clear                   darbn_ref
139340130927     c                   eval      §arbnref = par5ref
139350130927     c                   eval      §arbnteld= par5teld
139360130927     c                   eval      arbuni70 = darbn_ref
139370130927     c                   eval      arbtrd='REF'
139380130927     c                   write     FNARBN00
139390131001     c                   endif
139400131001     c
139410131001     c                   if        *in83 and emd_§ar5tel<>vidtsms
139420130930     c                   clear                   darbn_sms
139430130930     c                   eval      §arbncel = emd_§ar5tel
139440130930     c                   eval      arbuni70 = darbn_sms
139450130927     c                   eval      arbtrd='SMS'
139460130927     c                   write     FNARBN00
139470131001     c                   endif
139480131001     c
139490131001     c                   if        *in86 and emd_§ar5eml<>videml
139500130927     c                   clear                   darbn_eml
139510130930     c                   eval      §arbneml=emd_§ar5eml
139520130927     c                   eval      arbuni70 = darbn_eml
139530130927     c                   eval      arbtrd='EML'
139540130927     c                   write     FNARBN00
139550131001     c                   endif
139560131001
139570131001     c                   if        *in81 and (vidnt1<>v1cnt1 or vidnt2<>v1cnt2)
139580130930     c                   clear                   darbn_not
139590130930     c                   eval      §arbnnot1=v1cnt1
139600130930     c                   eval      §arbnnot2=v1cnt2
139610130930     c                   eval      arbuni70 = darbn_not
139620130930     c                   eval      arbtrd='NOT'
139630130930     c                   write     FNARBN00
139640130930     c                   endif
139650130930
139660130930     c                   endif
139670130930     c
139680131001     c                   if        *in84
139690130930     c                   exsr      AggioREF
139700131001     c                   endif
139710131001     c
139720130930      * EMAIL e cell sms
139730131001     c                   if        *in83 or *in86
139740130930     c                   Eval      kAr5Trd = 'EMD'
139750130930     c     kAr5          Chain     Fiar501l
139760130930      * Aggiorno Fiar5
139770130930if  2c                   If        %Found(Fiar501l)
139780130930     c                   movel     ar5uni        DAr5EMD
139790131001     c   86              eval      emd_§ar5eml=videml
139800131001     c   83              eval      emd_§ar5tel=vidtsms
139810130930     c                   Movel(p)  DAr5emd       Ar5Uni
139820130930     c                   Update    Fiar5000
139830130930      * Scrivo Fiar5
139840130930   x2c                   Else
139850130930      * se immessi i dati
139860130930      * non mi preoccupo per il fermo deposito: il fiar5 record GEN viene sempre creato
139870130930      * in immissione bolle. Non esiste più il caso di fiar5 record GEN inesistente in fase
139880130930      * di manutenzione bolle
139890130930if  3c                   If        Videml <> *Blanks or Vidtsms <> *Blanks
139900130930     c                   Clear                   Fiar5000
139910130930     c                   Eval      Ar5Aas = ArbAas
139920130930     c                   Eval      Ar5Lnp = ArbLnp
139930130930     c                   Eval      Ar5Nrs = ArbNrs
139940130930     c                   Eval      Ar5Nsp = ArbNsp
139950130930     c                   Eval      Ar5Trd = 'EMD'
139960130930     c                   Move      Dateu         Ar5Dac
139970130930     c                   Movel     W0140         Ar5Orc
139980130930     c                   Clear                   Dar5emd
139990131001     c   86              eval      emd_§ar5eml=videml
140000131001     c   83              eval      emd_§ar5tel=vidtsms
140010130930     c                   Movel(p)  DAr5emd       Ar5Uni
140020160426     c                   Eval      Ar5Ft1 = 'T'
140030160426     c                   Eval      Ar5Dt1 = *All'9'
140040160426     c                   Eval      Ar5Ft2 = 'T'
140050160426     c                   Eval      Ar5Dt2 = *All'9'
140060130930     c                   Write     Fiar5000
140070130930    3c                   EndIf
140080130930    2c                   EndIf
140090130930     c
140100131001   x1c                   endif
140110130930
140120131001     c                   if        *in81
140130130930     c* aggiorno le NOTE
140140130930     c* aggiorno/deleto trk 8 e 9 se variati
140150130930     c                   clear                   fnlv19ds
140160130930     C                   MOVEL     'T'           D19ftr
140170130930     C                   MOVEL     arbAAS        D19AAS
140180130930     C                   MOVEL     arbLNP        D19LNP
140190130930     C                   MOVEL     arbNRS        D19NRS
140200130930     C                   MOVEL     arbNSP        D19NSP
140210130930     C                   MOVEL     vidnt1        D19NT1
140220130930     C                   MOVEL     vidnt2        D19NT2
140230130930    2C     vidnt1        IFNE      *BLANKS
140240130930     C     vidnt2        ORNE      *BLANKS
140250130930     C                   MOVEL     '8'           D19TRC
140260130930     C                   CALL      'FNLV19R'
140270130930     C                   PARM                    fnlv19ds
140280130930    2C                   ENDIF
140290130930     c* Se il pgm mi restituisce la seconda riga vuota la deleto perchè
140300130930     c* prima poteva esistere
140310130930    2C     D19ERR        IFEQ      *BLANKS
140320130930     C     D19NT1        ANDNE     *BLANKS
140330130930     C     D19NT2        ANDEQ     *BLANKS
140340130930     C                   MOVEL     '9'           D19TRC
140350130930     C                   CLEAR                   D19NT1
140360130930     C                   CLEAR                   D19NT2
140370130930     C                   CALL      'FNLV19R'
140380130930     C                   PARM                    fnlv19ds
140390130930   x2C                   else
140400130930     C* Se tornato codice di err richiamo il pgm 2 volte:  una per nota
140410130930    3C     D19ERR        IFNE      *BLANKS
140420130930     C     vidnt1        oreq      *BLANKS
140430130930     C     vidnt2        ANDEQ     *BLANKS
140440130930     C* Nota 1
140450130930     C                   MOVEL     '8'           D19TRC
140460130930     C                   MOVEL     vidnt1        D19NT1
140470130930     C                   CLEAR                   D19NT2
140480130930     C                   CALL      'FNLV19R'
140490130930     C                   PARM                    fnlv19ds
140500130930     C* Nota 2
140510130930     C                   MOVEL     '9'           D19TRC
140520130930     C                   MOVEL     vidnt2        D19NT1
140530130930     C                   CLEAR                   D19NT2
140540130930     C                   CALL      'FNLV19R'
140550130930     C                   PARM                    fnlv19ds
140560130930    3C                   ENDIF
140570130930    2C                   ENDIF
140580130930
140590130930    1c                   EndIf
140600130927     c*
140610131008     c   10              unlock    fnarb01l
140620131008     c  n10              unlock    fnblp01l
140630130927     C     ENDRG12       ENDSR
140640961219     C*
140650950113     C*--- CALCOLO IL VOLUME AUTOMATICO ------------------------------*
140660950113     C     CALVOL        BEGSR
140670030612     C                   CLEAR                   Fnlv18DS
140680950113     C   10              MOVEL     'A'           D18TBO
140690950113     C  N10              MOVEL     'P'           D18TBO
140700950113     C                   Z-ADD     ARBKSC        D18KSC
140710950113     C                   Z-ADD     ARBCTR        D18CTR
140720950113     C                   MOVEL     §3ATB1        D18TBL
140730950113     C                   Z-ADD     ARBAAS        D18AAS
140740950113     C                   Z-ADD     ARBMGS        D18MGS
140750950113     C                   Z-ADD     ARBNCL        D18NCL
140760950113     C                   Z-ADD     ARBPKF        D18PKF
140770020311     c                   movel     arbtsp        d18tsp
140780020311     c                   z-add     arblna        d18lna
140790020311     c                   z-add     arblnp        d18lnp
140800950113     C                   CALL      'FNLV18R'
140810030612     C                   PARM                    Fnlv18DS
140820950113     C*
140830950113     C                   ENDSR
140840911212     C*
140850970808     C* CONTROLLO INDIRIZZO COMPLETO MITT/DESTIN ---------------------*
140860970808     C     CTRIND        BEGSR
140870150203     c                   clear                   normlod
140880970808     C* LE DS SONO DA CLEARARE PRIMA ALTRIMENTI DANNO PROBLEMI IN CTRER
140890030612     C                   CLEAR                   Fnlv13DS
140900030612     C                   CLEAR                   Tisi95DS
140910970808     C*
140920100120     c                   movel     'S'           i14forzait
140930970808     C                   MOVEL     ARBDSP        I14DRI
140940020305     c                   movel     'S'           i14cnz
140950970808     C                   CALL      'FNLV14R'
140960970808     C                   PARM                    KPJBA
140970030612     C                   PARM                    Fnlv14DS
140980970808     C                   MOVEL     O14ERR        WERR              1
140990970808     C                   MOVEL     O14MSG        VIDMSG
141000970808     C                   EXSR      CTRERR
141010970808     C*
141020970808     C     *IN90         IFEQ      *ON
141030970808     C                   GOTO      ENDIND
141040970808     C                   ENDIF
141050970808     C*************
141060970808     C* CONTROLLO CAP
141070970808     C                   MOVEL     '7'           I95TCN
141080970808     C                   MOVEL     E14CAP        I95CAP
141090970808     C                   MOVEL     E14LOC        I95LOC
141100970808     C                   MOVEL     E14PRV        I95PRV
141110970808     C                   MOVEL     E14NAR        I95NAR
141120000623     C                   MOVEL     ARBDSP        I95DAT
141130020318     C     LNPNTW        IFEQ      'PPT'
141140000623     C                   MOVEL     ARBLNP        I95TFP
141150000623     C                   ELSE
141160980602     C                   MOVEL     ARBTFP        I95TFP
141170000623     C                   ENDIF
141180971230     C                   Z-ADD     ARBPKF        I95LKG
141190971230     C                   Z-ADD     ARBVLF        I95LMC
141200971230     C                   MOVEL     ARBFFD        I95FFD
141210971201     C                   MOVEL     FLGTB1        I95TPO
141220971230     C                   MOVEL     ARBTSP        I95TSP
141230980602     C                   MOVEL     'S'           I95FRE
141240971201     C     §3AFCA        IFNE      0
141250971201     C                   MOVEL     'S'           I95FCA
141260971201     C                   ENDIF
141270970808     C                   MOVEL     'S'           I13AF0
141280140902     c                   if        d48ffr<>'E'
141290970808     C                   MOVEL     'S'           I13CNV
141300140807     c                   endif
141310970808     C                   MOVEL     'S'           I13SZV
141320141119     c* in caso di chiamata cieca non è da impostare: non si deve aprire la int località
141330141119     c                   if        d48ffr<>'E'
141340970808     C                   MOVEL     'S'           I13AF1
141350141119     c                   else
141360141119     C                   MOVEL     ' '           I13AF1
141370141119     c                   endif
141380970808     C                   MOVEL     'S'           I13SZ2
141390970808     C*
141400970808     C* SE CAMBIATO CAP PULISCO CAMPI PER FORZATURE
141410970808     C* ** DESTINATARIO **
141420970808     C     WCLI          IFEQ      'D'
141430970808    2C     E14CAP        IFNE      VARCAD
141440970808     C                   CLEAR                   WZV
141450970808     C                   CLEAR                   WZ1
141460970808     C                   CLEAR                   WZ2
141470970808     C                   MOVEL     E14CAP        VARCAD
141480970808    2C                   ENDIF
141490970808     C* MODIFICA LA LOCALITA'
141500970808    2C     E14LOC        IFNE      VARLOD
141510970808     C                   MOVEL     E14LOC        VARLOD
141520970808     C                   CLEAR                   WZ1
141530970808     C                   CLEAR                   WZ2
141540970808    2C                   ENDIF
141550970808     C                   MOVEL     WZV           E13FZV
141560970808     C                   MOVEL     WZ1           E13FZ1
141570970808     C                   MOVEL     WZ2           E13FZ2
141580020305     c     lnantw        ifeq      'FED'
141590020305     c                   movel     'S'           i95fi1
141600020305     c                   endif
141610970808     C                   ELSE
141620970808     C* ** MITTENTE **
141630970808    2C     E14CAP        IFNE      VARCAM
141640970808     C                   CLEAR                   WZVM
141650970808     C                   CLEAR                   WZ1M
141660970808     C                   CLEAR                   WZ2M
141670970808     C                   MOVEL     E14CAP        VARCAM
141680970808    2C                   ENDIF
141690970808     C* MODIFICA LA LOCALITA'
141700970808    2C     E14LOC        IFNE      VARLOM
141710970808     C                   MOVEL     E14LOC        VARLOM
141720970808     C                   CLEAR                   WZ1M
141730970808     C                   CLEAR                   WZ2M
141740970808    2C                   ENDIF
141750020305     c     lnpntw        ifeq      'FED'
141760020305     c                   movel     'S'           i95fi1
141770020305     c                   endif
141780970808     C                   MOVEL     WZVM          E13FZV
141790970808     C                   MOVEL     WZ1M          E13FZ1
141800970808     C                   MOVEL     WZ2M          E13FZ2
141810970808     C                   END
141820970808     C*
141830970808     C**
141840970808     C** CALL
141850970808     C                   CALL      'FNLV13R'
141860970808     C                   PARM                    KPJBA
141870030612     C                   PARM                    Fnlv13DS
141880030612     C                   PARM                    Tisi95DS
141890970808     C*
141900970808     C                   MOVEL     O13ERR        WERR              1
141910970808     C                   MOVEL     O13MSG        VIDMSG
141920970808     C                   EXSR      CTRERR
141930970808     C*
141940970808     C* REIMPOSTO LE FORZATURE
141950970808     C     WCLI          IFEQ      'D'
141960970808     C                   MOVEL     E13FZV        WZV
141970970808     C                   MOVEL     E13FZ1        WZ1
141980970808     C                   MOVEL     E13FZ2        WZ2
141990970808     C                   ELSE
142000970808     C                   MOVEL     E13FZV        WZVM
142010970808     C                   MOVEL     E13FZ1        WZ1M
142020970808     C                   MOVEL     E13FZ2        WZ2M
142030970808     C                   END
142040150203     c* memorizzo la località normalizzata per destinatario
142050150203     c                   if        wcli='D'
142060150203     c                   eval      normlod=o95loc
142070150203     c                   endif
142080970808     C     ENDIND        ENDSR
142090070320     C*
142100070320     C*--- Gestisce record"L"di FIAR4 --------------------------------*
142110070320     C     GestAR4L      BEGSR
142120070320     c* lo faccio solo se GEO attiva
142130070320    0c                   if        FGSGEOT='S'
142140070320     c* Se devo chainare
142150070320     C                   MOVEL     'L'           WTRC
142160070320     c
142170070321    1c                   if        TipoEla='C'
142180070320     c                   clear                   Wesar4L
142190070320     C     KARB2         CHAIN     FiAR4000                           3091
142200070320    2c                   if        *in91
142210070320     C                   SETON                                        28
142220070320     C                   MOVEL     MSG(4)        VIDMSG
142230070320     C                   GOTO      ENDar4L
142240070320    2c                   endif
142250070320     c
142260070320    2c                   if        not *in30
142270070320     c                   movel     'E'           WEsar4L           1
142280070320     c                   movel     ar4not        dsbl4L
142290070320     c                   else
142300070320     c                   movel     'N'           WEsar4L
142310070320     c                   clear                   dsbl4L
142320070320     c                   clear                   fiar4000
142330070320    2c                   endif
142340070320    1c                   endif
142350070320     c
142360070320    1c                   if        tipoela='A'
142370070320     c                   movel     dsbl4l        ar4not
142380070320     c                   movel     dateu         ar4duv
142390070320     c                   movel     dateu         ar4DTR
142400070320     c                   movel     'T'           ar4ftr
142410070320     c
142420070320    3c                   if        wesar4L='N'
142430070320     c                   eval      ar4aas=arbaas
142440070320     c                   eval      ar4lnp=arblnp
142450070320     c                   eval      ar4nrs=arbnrs
142460070320     c                   eval      ar4nsp=arbnsp
142470070320     c                   eval      ar4trc='L'
142480070320     c                   write     fiar4000
142490070320   x3c                   else
142500070320     c                   update    fiar4000
142510070320    3c                   endif
142520070320    1c                   endif
142530070320     c
142540070320    0c                   endif
142550070320     c
142560070320     C     ENDar4L       ENDSR
142570970808     C*
142580970808     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
142590970808     C     CTRERR        BEGSR
142600970808     C* IMPOSTO CAMPI PASSATI
142610970808    1C     O13RON        IFEQ      'S'
142620970808     C                   MOVEL     O95NAR        E14NAR
142630150112     c                   if        d48ffr='E'
142640150112     c                   clear                   werr
142650150112     c                   eval      msgforzatxt=o13msg
142660150112     c                   endif
142670970808    1C                   ENDIF
142680970808    1C     O13ROC        IFEQ      'S'
142690970808     C                   MOVEL     O95CAP        E14CAP
142700150112     c                   if        d48ffr='E'
142710150112     c                   clear                   werr
142720150112     c                   eval      msgforzatxt=o13msg
142730150112     c                   endif
142740970808    1C                   ENDIF
142750970808    1C     O13ROP        IFEQ      'S'
142760970808     C                   MOVEL     O95PRV        E14PRV
142770150112     c                   if        d48ffr='E'
142780150112     c                   clear                   werr
142790150112     c                   eval      msgforzatxt=o13msg
142800150112     c                   endif
142810970808    1C                   ENDIF
142820970808    1C     O13ROL        IFEQ      'S'
142830970808     C                   MOVEL     O95LOC        E14LOC
142840150112     c                   if        d48ffr='E'
142850150112     c                   clear                   werr
142860150112     c                   eval      msgforzatxt=o13msg
142870150112     c                   endif
142880970808    1C                   ENDIF
142890970808     C* ERRORE O RICERCA CAMPO
142900970808    1C     WERR          IFNE      ' '
142910970808     C*
142920970808    2C     VIDMSG        IFNE      *BLANKS
142930970808     C                   SETON                                        28
142940970808    2C                   ENDIF
142950970808     C* POSIZIONAMENTO CURSORE
142960970808    2C                   SELECT
142970970808     C     WERR          WHENEQ    '1'                                          R.SOCIALE
142980970808     C                   SETON                                        40
142990970808     C     WERR          WHENEQ    '2'                                          INDIRIZZO
143000970808     C                   SETON                                        41
143010970808     C     WERR          WHENEQ    '5'                                          CAP
143020970808     C     WCLI          IFEQ      'D'
143030970808     C                   SETON                                        43
143040970808     C                   ELSE
143050970808     C                   SETON                                        42
143060970808     C                   END
143070970808     C     WERR          WHENEQ    '3'                                          LOCALITA
143080970808     C                   SETON                                        43
143090970808     C     WERR          WHENEQ    '4'                                          PROVINCIA
143100970808     C     WCLI          IFEQ      'D'
143110970808     C                   SETON                                        50
143120970808     C                   ELSE
143130970808     C                   SETON                                        45
143140970808     C                   END
143150970808     C     WERR          WHENEQ    '6'                                          NAZIONE
143160970808     C                   SETON                                        44
143170970808     C     WERR          WHENEQ    '7'                                          PARTITA IVA
143180970808     C     WCLI          IFEQ      'D'
143190970808     C                   SETON                                        45
143200970808     C                   ELSE
143210970808     C                   SETON                                        49
143220970808     C                   END
143230970808     C     WERR          WHENEQ    '8'                                          COD.ISO
143240970808     C     WCLI          IFEQ      'D'
143250970808     C                   SETON                                        46
143260970808     C                   ELSE
143270970808     C                   SETON                                        50
143280970808     C                   END
143290970808     C     WERR          WHENEQ    '9'                                          INOLTRO
143300970808     C     WCLI          ANDEQ     'D'
143310970808     C                   SETON                                        49
143320020318     c     werr          wheneq    '0'
143330020318     C     WCLI          IFEQ      'D'
143340020318     C                   SETON                                        50
143350020318     C                   ELSE
143360020318     C                   SETON                                        45
143370020318     C                   END
143380020318     c
143390970808    2C                   ENDSL
143400970808     C*
143410970808     C                   SETON                                        90
143420970808    1C                   ENDIF
143430970808     C                   ENDSR
143440091217     c*
143450091217     c* Aggiornamento per record GEN ddel fiar5 --------------------------------*
143460091217     c     RegisAR5GEN   BEGSR
143470091217     c                   clear                   dar5gen
143480091217     c                   Eval      kAr5Trd = 'GEN'
143490091217     c     kAr5          Chain     Fiar501l
143500091217      * Aggiorno Fiar5
143510091217if  1c                   If        %Found(Fiar501l)
143520091217     c                   movel     ar5uni        DAr5Gen
143530091217     c*
143540091217     c* Aggiorno tipo incasso in arrivo
143550091217    2c                   select
143560091217     c                   when      Wtipoagg='TIC'
143570091217     c                   Eval      §Ar5ticmb=vidtic
143580100119     c                   eval      §ar5flgMB='S'
143590091217     c
143600091217     c                   when      Wtipoagg='REF'
143610091217     c                   Eval      §Ar5Ref = VidNom
143620091217     c                   Eval      §Ar5Teld = VidTel
143630140403     c                   when      Wtipoagg='PIK'
143640140407     c                   Eval      §Ar5pk2 = wpk2
143650091217    2c                   ENDSL
143660091217
143670091217     c                   Movel(p)  DAr5Gen       Ar5Uni
143680091217     c
143690091217      * se devo trasmettere alla sede sfleggo per la trasmissione
143700091217    2c                   If        §Ar5Trse = 'S'
143710091217     c                   Clear                   Ar5Ft2
143720091217    2c                   EndIf
143730091217     c                   Clear                   Ar5Ft3
143740091217     c                   Update    Fiar5000
143750091217      * Scrivo Fiar5
143760091217   x1c                   Else
143770091217      * se immessi i dati
143780091217     c                   Clear                   Fiar5000
143790091217     c                   Eval      Ar5Aas = ArbAas
143800091217     c                   Eval      Ar5Lnp = ArbLnp
143810091217     c                   Eval      Ar5Nrs = ArbNrs
143820091217     c                   Eval      Ar5Nsp = ArbNsp
143830091217     c                   Eval      Ar5Trd = 'GEN'
143840091217     c                   Move      Dateu         Ar5Dac
143850091217     c                   Movel     W0140         Ar5Orc
143860091217     c                   Clear                   Dar5Gen
143870091217     c
143880091217    2c                   select
143890091217     c                   when      Wtipoagg='TIC'
143900091217     c                   Eval      §Ar5ticmb=vidtic
143910100119     c                   eval      §ar5flgMB='S'
143920091217     c
143930091217     c                   when      Wtipoagg='REF'
143940091217     c                   Eval      §Ar5Ref = VidNom
143950091217     c                   Eval      §Ar5Teld = VidTel
143960140403     c                   when      Wtipoagg='PIK'
143970140407     c                   Eval      §Ar5pk2 = wpk2
143980091217    2c                   ENDSL
143990091217
144000091217     c                   Movel(p)  DAr5Gen       Ar5Uni
144010091217    2c                   if        ar5uni<>*blanks
144020091217     c                   Write     Fiar5000
144030091217    2c                   EndIf
144040091217    1c                   EndIf
144050091217     c
144060091217     c                   ENDSR
144070091217     c
144080911212     C*--- ALLOCO OGGETTI --------------------------------------------*
144090170116     C**   ALLOC         BEGSR
144100170116     C**                 MOVE      ')'           VAR               7
144110170116     C**                 MOVE      '))'          VAR2              8
144120170116     C**                 Z-ADD     48            LUNG             15 5
144130920120     C*
144140170116     C**                 MOVEA     SFILE         C20(13)
144150170116     C**                 MOVEA     SFILE         C21(13)
144160170116     C**                 MOVEA     SFILE         C22(13)
144170170116     C**                 MOVEA     C22           SOVR(1)
144180170116     C**                 MOVEA     C20           SALC(1)
144190170116     C**                 MOVEA     C21           SDLC(1)
144200920120     C*
144210041027     C* solo PER SEDE
144220170116     C**   §7LFFI        IFEQ      'S'
144230911212     C* CHECK OBJ E ADDPFM
144240170116     C**                 Z-ADD     046           PARMBR            3 0
144250170116     c**                 move      046           $mbr
144260170116     C**                 MOVEL     SFILE         FISICO
144270170116     C**                 CALL      'TRUL50C'
144280170116     C**                 PARM                    FISICO           10
144290170116     C**                 PARM                    MBRFIS           10
144300170116     C**                 PARM                    LIB              10
144310170116     C**                 PARM                    LOGICO           10
144320170116     C**                 PARM                    MBRLOG           10
144330170116     C**                 PARM                    ULFLG             1
144340911212     C** OVRDBF
144350170116     c**                 movel     $mbr          var
144360170116     C**                 MOVE      VAR           SOVR
144370170116     C**                 MOVEL     *BLANKS       COMMAN           80
144380170116     C**                 MOVEA     SOVR(1)       COMMAN
144390170116     C**                 CALL      'QCMDEXC'
144400170116     C**                 PARM                    COMMAN
144410170116     C**                 PARM                    LUNG
144420911212     C* ALLOCO OGGETTO
144430170116     c**                 movel     $mbr          var2
144440170116     C**                 MOVE      VAR2          SALC
144450170116     C**                 MOVEL     *BLANKS       COMMAN           80
144460170116     C**                 MOVEA     SALC(1)       COMMAN
144470170116     C**                 CALL      'QCMDEXC'                            91
144480170116     C**                 PARM                    COMMAN
144490170116     C**                 PARM                    LUNG
144500911212     C*
144510911212     C* 90 ON - MEMBRO ALLOCATO MESSAGGIO DI ATTENDERE
144520170116     C** 91              GOTO      ENDALC
144530170116     C**                 END
144540941212     C*
144550170116     C**   ENDALC        TAG
144560941212     C* 91 ON - ERRORE
144570941212     C*
144580170116     C**   *IN91         IFEQ      *ON
144590170116     C**                 SETON                                        28
144600170116     C**                 MOVEa     MSG(136)      k78
144610170116     C**                 MOVEa     SFILE         k78(68)
144620170116     C**                 MOVEa     k78           vidmsg
144630170116     C**                 ENDIF
144640941212     C*
144650170116     C**                 ENDSR
144660920831     C*
144670940321     C*--- AGGIORNO FILE TASSAZIONE PADRONCINI -----------------------*
144680950113     C     AGGPDS        BEGSR
144690041008     C                   MOVEL     ' '           UNO               1
144700041008     c* richiamo pgm fnlv57ds
144710041008     c                   clear                   fnlv57ds
144720041008     C* 01 ON  - VARIAZIONE DESTINATARIO
144730041008    1c     *in01         ifeq      *on
144740041008    2C     ARBRSD        IFNE      OLDRSD
144750041008     C     ARBCAD        ORNE      OLDCAD
144760041008     C     ARBFIN        ORNE      OLDFIN
144770041008     c                   eval      uno='1'
144780041008    2c                   endif
144790041008    1c                   endif
144800041008     c
144810041008    1c                   if        *in02
144820041008    2c     ARBVLF        IFNE      OLDVLF
144830041008     C     ARBPKF        orne      OLDPKF
144840041008     c                   eval      uno='1'
144850041008    2c                   endif
144860041008    1c                   endif
144870041008     c*
144880041008    1c                   if        uno='1'
144890041008     c                   eval      i57aas=arbaas
144900041008     c                   eval      i57lnp=arblnp
144910041008     c                   eval      i57nrs=arbnrs
144920041008     c                   eval      i57nsp=arbnsp
144930041008     c
144940041008     c                   select
144950041008     c* Se presenti entrambe le bolle
144960041008     c                   when      waltrabolla=' '
144970070320     c                   eval      i57drt=arbdrt
144980070320     c                   eval      i57nrt=arbnrt
144990070320     c                   eval      i57fpp=arbfpp
145000070320     c                   eval      i57prt=arbpdr
145010070320     c                   eval      i57pco=arbpdc
145020041008     c* modifica in arrivo non presente fnblp
145030041008     c                   when      *in10 and waltrabolla='N'
145040041008     c                   eval      i57pdr=arbpdc
145050041008     c                   eval      i57tbo='A'
145060041008     c* modifica in partenza non presente fnarb
145070041008     c                   when      not *in10 and waltrabolla='N'
145080070320     c                   eval      i57pdr=arbpdr
145090041008     c                   eval      i57tbo='P'
145100070320     c                   eval      i57drt=arbdrt
145110070320     c                   eval      i57nrt=arbnrt
145120070320     c                   eval      i57fpp=arbfpp
145130041008     c                   endsl
145140041008     c
145150041008     c                   eval      i57ncl=arbncl
145160041011     c   02              eval      i57tvl='F'
145170041008     c* volume vecchio
145180041008     c                   eval      i57vlc=arbvlc
145190041008     c                   eval      i57ncr=arbncr
145200041008     c                   eval      i57vlf=oldvlf
145210041008     c* volume nuovo
145220041008     c                   eval      i57vcn=arbvlc
145230041008     c                   eval      i57ncn=arbncp
145240041008     c                   eval      i57vfn=arbvlf
145250041008     c* peso vecchio
145260041008     c                   eval      i57pkc=arbpkc
145270041008     c                   eval      i57ncp=arbncp
145280041008     c                   eval      i57pkf=oldpkf
145290041008     c* peso nuovo
145300041008     c                   eval      i57pcn=arbpkc
145310041008     c                   eval      i57npn=arbncp
145320041008     c                   eval      i57pfn=arbpkf
145330041008     c* destinatario vecchio
145340041008     c                   eval      i57rsd=oldrsd
145350041008     c                   eval      i57cad=oldcad
145360041008     c                   eval      i57fin=oldfin
145370041008     c* destinatario nuovo
145380041008     c                   eval      i57rdn=arbrsd
145390041008     c                   eval      i57cdn=arbcad
145400041008     c                   eval      i57fdn=arbfin
145410041008     c
145420041008     c                   call      'FNLV57R'
145430041008     c                   parm                    kpjba
145440041008     c                   parm                    fnlv57ds
145450041008     c                   endif
145460940321     C*
145470940321     C                   ENDSR
145480950113     C*
145490950113     C* AGGIORNA PADRONCINI PER IL MITTENTE --------------------------*
145500950113     C     AGGPMT        BEGSR
145510950113     C*
145520950113    1C     ARBKSC        IFNE      OLDKSC
145530950113     C     ARBRSM        ORNE      OLDRSM
145540950113     C     ARBCAM        ORNE      OLDCAM
145550950113     C*
145560950113     C* CONTROLLO CON QUALE VOLUME HO AGGIORNATO LA BOLLA
145570950113     C                   CLEAR                   WVOL
145580950113     C*
145590950113     C* VOLUME PARZIALE "Z"
145600950113     C*
145610050111     C     ARBNCR        ifne      ARBNCL
145620950113     C*
145630950113    3C     ARBVLC        IFGE      ARBVLF
145640950113     C                   Z-ADD     ARBVLC        WVOL
145650950113   X3C                   ELSE
145660950113     C                   Z-ADD     ARBVLF        WVOL
145670950113    3C                   ENDIF
145680950113     C* VOLUME TOTALE "T"
145690950113   X2C                   ELSE
145700950113     C                   Z-ADD     ARBVLC        WVOL
145710950113    2C                   ENDIF
145720950113     C****
145730950113     C** ESEGUO L'AGGIORNAMENTO DEI FILE PADRONCINI 2 VOLTE:
145740011126     C**  - LA PRIMA   VOLTA PER I FILE fiftt00F/fiftd00F
145750011126     C**  - LA SECONDA VOLTA PER I FILE fifst00F/fifsd00F - SIMULAZIONE
145760950113     C****
145770950113     C* 17 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
145780950113     C                   SETOFF                                       17
145790950113     C*
145800070320     C                   MOVE      arbPDR        WPDR
145810070320     C                   Z-ADD     arbDRT        WDRT
145820950113     C* NUMERO DISTINTA
145830070320    2C     arbFPP        IFEQ      'M'
145840950113     C                   Z-ADD     1000          WNDC
145850950113     C                   ELSE
145860950113     C                   Z-ADD     5000          WNDC
145870950113    2C                   ENDIF
145880070320     C                   ADD       arbNRT        WNDC
145890950113     C*
145900950113     C*
145910950113    2C                   DO        2
145920011126     C   17KFTT2         CHAIN     fifst000                           30
145930011126     C  N17KFTT2         CHAIN     fiftt000                           30
145940950113     C*
145950950113    3C     *IN30         IFEQ      *OFF
145960950113     C*
145970950113     C* SE GIA' CONFERMATA NON AGGIORNO NIENTE
145980950113    4C     FTTFVL        IFNE      'C'
145990020326     c*
146000020326     c  n17kftd5         chain     fiftd005                           30
146010020326     c   17kftd5         chain     fifsd005                           30
146020020326     c     *in30         ifeq      *off
146030020326     c                   z-add     arbpkf        ftdpkl
146040020326     c                   z-add     wvol          ftdvlu
146050020326     c                   movel     arbrsm        ftdrsc
146060020326     c* dò per scontato che la bolla sia un porto franco
146070020326     c                   z-add     arbksc        ftdksc
146080020326     c                   movel     arbcam        ftdcap
146090020326     c                   movel     arbfap        ftdfin
146100020326     c* se è stato modificato codice cliente o ragione sociale devo
146110020326     c* aggiornare anche il numero di stop
146120020327    1C     ARBKSC        IFNE      OLDKSC
146130020327     C     ARBRSM        ORNE      OLDRSM
146140020327     c                   exsr      aggstp
146150020327     c                   endif
146160020327     c  n17              update    fiftd005
146170020327     c   17              update    fifsd005
146180020326     c                   endif
146190950113     C*
146200950113     C* SE VALORIZZATA SVALORIZZO
146210950113     C                   EXSR      SVAFTT
146220950113     C*
146230950113    4C                   ENDIF
146240950113    3C                   ENDIF
146250950113     C*
146260950113     C* 17 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
146270950113     C                   SETON                                        17
146280950113    2C                   ENDDO
146290950113     C*
146300950113    1C                   ENDIF
146310950113     C*
146320950113     C                   ENDSR
146330020327     c*
146340020327     c* AGGSTP - Aggiornamento numero stop a cambio di mittente ------*
146350020327     C     AGGSTP        BEGSR
146360020327     c                   clear                   wstp
146370020327     C* ESCLUDENDO FRANCHI E ASSEGNATI VARI,
146380020327     C*   CHAINO FNCLS00F PER PRENDERE IL NUMERO DEGLI STOP
146390020327     C                   Z-ADD     ftdksc        w0040             4 0
146400020327    5C     w0040         IFNE      *ALL'8'
146410020327     C     w0040         ANDNE     *ALL'9'
146420020327     C     ftdksc        CHAIN     FNCLS000                           31
146430020327     C  N31              z-add     CLSSTP        wstp
146440020327    5C                   ENDIF
146450020327     c     wstp          ifeq      *zeros
146460020327     c* prima di fare la chain mi devo salvare i dati del record di fiftd005
146470020327     c                   movel     dsftd         sav_dsftd
146480020327     C                   MOVEL     ARBKSC        WKSC
146490020327     C                   MOVEL     ARBRSM        WRSM
146500020327     C  N17KFTD          CHAIN(N)  fiftd004                           31
146510020327     C   17KFTD          CHAIN(N)  fifsd004                           31
146520020327     c  n31              z-add     ftdstp        wstp
146530020327     c* Se il numero stop da attribuire è ancora = a 0 devo cercare
146540020327     c* nella distinta il primo numero disponibile
146550020327     c     wstp          ifeq      *zeros
146560020327     C  N17KFTT2         setgt     fiftd001
146570020327     C   17KFTT2         setgt     fifsd001
146580020327     C  N17KFTT2         readpe    fiftd001
146590020327     C   17KFTT2         readpe    fifsd001
146600020327     c                   if        not %eof
146610020327     c                   eval      wstp = (ftdstp+1)
146620020417     c     wstp          iflt      1000
146630020417     c                   z-add     1000          wstp
146640020417     c                   endif
146650020417     c                   else
146660020417     c* qui non dovrebbe mai entrare ma lo gestisco comunque anche
146670020417     c* per unifomrità con fnlsa2r
146680020417     c                   z-add     1000          wstp
146690020327     c                   endif
146700020327     c                   endif
146710020327     c*
146720020327     c* ripristino i dati del record salvato
146730020327     c                   movel     sav_dsftd     dsftd
146740020327     c                   endif
146750020327     c                   z-add     wstp          ftdstp
146760020327     C                   ENDSR
146770020326     c*
146780950113     C* SVALORIZZA UNA DISTINTA --------------------------------------*
146790950113     C     SVAFTT        BEGSR
146800950113     C* SOLO SE VALORIZZATA
146810950113    6C     FTTFVL        IFEQ      'V'
146820950113     C                   MOVEL     FTDPDR        PA3PDR                         *COD. PADRONCINO
146830950113     C                   MOVEL     FTDTSR        PA3TSR                         *TIPO PRESTAZIONE
146840950113     C                   Z-ADD     FTDNDC        PA3NDC                         *NUMERO DISTINTA
146850950113     C                   Z-ADD     FTDDDC        PA3DDC                         *DATA   DISTINTA
146860950113     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
146870950113     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
146880950113     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
146890041122     C  n17              MOVEL     ' '           PA3sml                         *FLG VAL.MANUAL.TEST
146900041122     C   17              MOVEL     'S'           PA3sml                         *FLG VAL.MANUAL.TEST
146910950113     C                   MOVEL     PARAM3        KPJBU
146920011129     C  N17              CALL      'FICNE2R'
146930950113     C                   PARM                    KPJBA
146940011129     C   17              CALL      'FICNE9C'
146950950113     C                   PARM                    KPJBA
146960950113    6C                   ENDIF
146970950113     C                   ENDSR
146980941102     C*
146990941102     C* CONTROLLO DIVISA ---------------------------------------------*
147000941102     C     CTRVLT        BEGSR
147010011011      *
147020011011      * La divisa è obbligatoria se è inserito un importo
147030011011     C     W0133         IFGT      *ZEROS
147040011011     C     WVLT          ANDEQ     *BLANKS
147050011011     C                   MOVEL     MSG(100)      VIDMSG
147060011011     C                   SETON                                        9028
147070011011     C                   GOTO      ENDVLT
147080011011     C                   ENDIF
147090941102     C*
147100941102     C                   MOVEL     'CV'          COD
147110941102     C* RICERCA
147120941102     C     '?'           SCAN      WVLT                                   90
147130941102     C     *IN90         IFEQ      *ON
147140991011     C                   CLEAR                   §KEY
147150941102     C                   CALL      'X§TABER'
147160941102     C                   PARM                    CODUT
147170941102     C                   PARM                    COD
147180941102     C                   PARM                    §KEY
147190991011     C                   PARM                    §DES             25
147200941102     C                   MOVEL     §KEY          WVLT
147210941102     C                   GOTO      ENDVLT
147220941102     C                   ENDIF
147230941102     C*
147240941102     C* CONTROLLO DIVISA
147250941102     C                   MOVEL(P)  WVLT          KEY
147260941213     C                   EXSR      CTABEL
147270941102     C* ERRORE
147280941102    1C     *IN30         IFEQ      *ON
147290941102     C                   MOVEL     MSG(26)       VIDMSG
147300941102     C                   SETON                                        9028
147310941102     C                   GOTO      ENDVLT
147320941102    1C                   ENDIF
147330941102     C                   MOVEL     TBLUNI        DSCV
147340941102     C*
147350941102     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
147360941102    1C     §CVFDC        IFNE      'S'
147370941102     C                   MOVE      W0133         W0033             3 3
147380941102    2C     W0033         IFNE      0
147390941102     C                   MOVEL     MSG(27)       VIDMSG
147400941102     C                   SETON                                        9028
147410941102     C                   GOTO      ENDVLT
147420941102    2C                   ENDIF
147430941102    1C                   ENDIF
147440011010      *
147450011010      * Controllo il numero di decimali immessi
147460011011    1C     §CVFDC        IFEQ      'S'
147470011011     C     W0133         ANDNE     *ZEROS
147480011011      *
147490011011    2C                   SELECT
147500011011     C     §CVDEC        WHENEQ    3
147510011011     C     §CVDEC        WHENEQ    2
147520011011     C                   MOVE      W0133         W0010             1 0
147530011011    3C     W0010         IFNE      *ZEROS
147540011011     C                   MOVEA     MSG(93)       K78
147550011011     C                   MOVE      §CVDEC        WCVDEC            2
147560011011     C                   MOVEA     WCVDEC        K78(29)
147570011011     C                   MOVEA     K78           VIDMSG
147580011011     C                   SETON                                        9028
147590011011    3C                   ENDIF
147600011011     C     §CVDEC        WHENEQ    1
147610011011     C                   MOVE      W0133         W0020             2 0
147620011011    3C     W0020         IFNE      *ZEROS
147630011011     C                   MOVEA     MSG(93)       K78
147640011011     C                   MOVE      §CVDEC        WCVDEC            2
147650011011     C                   MOVEA     WCVDEC        K78(29)
147660011011     C                   MOVEA     K78           VIDMSG
147670011011     C                   SETON                                        9028
147680011011    3C                   ENDIF
147690011011     C     §CVDEC        WHENEQ    0
147700011011    2C                   ENDSL
147710011011      *
147720011011    1C                   ENDIF
147730941102     C*
147740941102     C     ENDVLT        ENDSR
147750941212     c*
147760941212     C*--- CHAIN TABEL00F --------------------------------------------*
147770941212     C     CTABEL        BEGSR
147780941212     C     KTAB2         CHAIN     TABEL                              30
147790941212     C  N30TBLFLG        IFNE      ' '
147800941212     C                   SETON                                        30
147810941212     C                   END
147820941212     C                   ENDSR
147830941212     C*---------------------------------------------------------------
147840941212     C* DISALLOCO OGGETTO MEMBRO PER LA SEDE
147850941212     C*---------------------------------------------------------------
147860170116     C**   DLCSED        BEGSR
147870170116     c**                 move      046           $mbr
147880170116     c**                 movel     $mbr          var2
147890170116     C**                 MOVE      VAR2          SDLC(1)
147900170116     C**                 MOVEL     *BLANKS       COMMAN           80
147910170116     C**                 MOVEA     SDLC(1)       COMMAN
147920170116     C**                 CALL      'QCMDEXC'                            91
147930170116     C**                 PARM                    COMMAN
147940170116     C**                 PARM                    LUNG
147950170116     C**                 ENDSR
147960941213     C*---------------------------------------------------------------
147970941213     C* STORICIZZO LA TASSAZIONE
147980941213     C*---------------------------------------------------------------
147990941213     C     STOARB        BEGSR
148000991006     C* PER LA PRIMA BOLLA KSC E CTR LI PRENDO SEMPRE DALLA BOLLA
148010991006     C* PRINCIPALE PERCHE' SONO SICURA DI AVERLA
148020991006     C* PER LA SECONDA BOLLA LI DEVO PRENDERE DA AR6 PERCHE'
148030991006     C* QUELLI DELLA BOLLA PRINCIPALE SI RIFERISCONO ALLA PRIMA BOLLA
148040991006     C     AR6TRC        IFEQ      '2'
148050991006     C                   Z-ADD     AR6KSC        ARBKSC
148060991006     C                   Z-ADD     AR6CTR        ARBCTR
148070991006     C                   ENDIF
148080991006     C                   MOVEL     AR6DIV        ARBDIV
148090941213     C                   MOVEL     AR6POR        ARBPOR
148100941213     C                   MOVEL     AR6SV1        ARBSV1
148110941213     C                   MOVEL     AR6VA1        ARBVA1
148120941213     C                   MOVEL     AR6SV2        ARBSV2
148130941213     C                   MOVEL     AR6VA2        ARBVA2
148140941213     C                   MOVEL     AR6SV3        ARBSV3
148150941213     C                   MOVEL     AR6VA3        ARBVA3
148160941213     C                   MOVEL     AR6IMV        ARBIMV
148170941213     C                   MOVEL     AR6ALI        ARBALI
148180941213     C                   MOVEL     AR6CEI        ARBCEI
148190941213     C                   MOVEL     AR6IFT        ARBIFT
148200941213     C                   MOVEL     AR6DFT        ARBDFT
148210941213     C                   MOVEL     AR6NFT        ARBNFT
148220941213     C                   MOVEL     AR6FIV        ARBFIV
148230950114     C* SE VENGO DA VARIAIZONE MITTENTE COME CODICE TARIFFA CI METTO
148240950114     C*  QUELLO SALVATO
148250950114     C   24              MOVEL     MITCTR        ARBCTR
148260991013     C                   MOVEL     AR6FIM        ARBFIM
148270011108      *
148280011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
148290011108      * è in partenza 'P' o in arrivo 'A'
148300011108     C     *IN10         IFEQ      *ON
148310011108     C                   MOVE      'A'           ARBPRU
148320011108     C                   ENDIF
148330011108     C     *IN10         IFEQ      *OFF
148340011108     C                   MOVE      'P'           ARBPRU
148350011108     C                   ENDIF
148360011108      *
148370950114     C*
148380991006     C                   WRITE     FIARBT00
148390950114     C*
148400991006     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
148410941221    1C     WAR7          IFEQ      'E'
148420991006    1C     ARBTRC        ANDEQ     '1'
148430991006     C                   MOVEL     '1'           WTRC
148440991006     C     KARB2         SETLL     FIAR7000
148450991006     C     KARB2         READE(N)  FIAR7000                               30
148460941213     C*
148470941213    2C     *IN30         DOWEQ     *OFF
148480941213     C                   MOVEL     AR7SVN        ARBSVN
148490941213     C                   MOVEL     AR7VAN        ARBVAN
148500991006     C                   WRITE     FIARBU00
148510941213     C*
148520991006     C     KARB2         READE(N)  FIAR7000                               30
148530941213    2C                   ENDDO
148540941213     C                   ENDIF
148550041130      *-------
148560041130      * Seconda bolla
148570041130     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
148580041130    1C     WAR72         IFEQ      'E'
148590041130    1C     ARBTRC        ANDEQ     '2'
148600041130     C                   MOVEL     '2'           WTRC
148610041130     C     KARB2         SETLL     FIAR7000
148620041130     C     KARB2         READE(N)  FIAR7000                               30
148630041130     C*
148640041130    2C     *IN30         DOWEQ     *OFF
148650041130     C                   MOVEL     AR7SVN        ARBSVN
148660041130     C                   MOVEL     AR7VAN        ARBVAN
148670041130     C                   WRITE     FIARBU00
148680041130     C*
148690041130     C     KARB2         READE(N)  FIAR7000                               30
148700041130    2C                   ENDDO
148710041130     C                   ENDIF
148720041130      *-------
148730941213     C*
148740941213     C                   ENDSR
148750941214     C*----------------------------------------------------------------
148760941214     C* RICALCOLO IL VOLUME AUTOMATICO
148770941214     C*----------------------------------------------------------------
148780941214     C     VOLAUT        BEGSR
148790941214     C*
148800941214     C* VOLUME AUTOMATICO A PESO: PESO MODIFICATO
148810941214    3C     VIDPKF        IFNE      ARBPKF
148820941214     C     ARBFVF        ANDEQ     'K'
148830941214     C*
148840120702     C     ARBPKF        DIV(H)    ARBVLF        CLIKPM            9 1
148850941214     C     VIDPKF        DIV(H)    CLIKPM        D20VLU
148860941214     C*
148870941214     C     ARBVLB        IFEQ      0
148880941214     C                   Z-ADD     0,001         D20VLU
148890941214     C                   END
148900941214     C                   MOVEL     ARBFVF        D20FVL
148910941214     C*
148920941214    3C                   END
148930941214     C                   ENDSR
148940941215     C**************************************************************************
148950941215     C*  ABBLANCA ZERO NON SIGNIFICATIVI
148960941215     C**************************************************************************
148970941215     C     ABBL          BEGSR
148980941215     C*
148990941215     C                   SETOFF                                       99
149000941215     C     SKI(I)        DOWEQ     '0'
149010941215     C                   MOVEL     ' '           SKI(I)
149020941215     C                   ADD       1             I
149030941215     C     I             COMP      15                                     99
149040941215     C  N99              END
149050941215     C*
149060941215     C                   ENDSR
149070051114     C**************************************************************************
149080051114     C*  ABBLANCA ZERO NON SIGNIFICATIVI
149090051114     C**************************************************************************
149100051114     C     SR_chkcomb    BEGSR
149110051114     c                   clear                   wcomb             1
149120051114     C* Non è possibile indicare contemporaneamente consegne particolari
149130051117     C*  'S' ed 'X' / 'S' ed 'A' / "S" e "P" / "X" e "P"
149140051114    2C                   IF           (wTC1 = 'X'  and  wTC2 = 'S')
149150051114     C                             or (wTC1 = 'S'  and  wTC2 = 'X')
149160051114     C                             or (wTC1 = 'A'  and  wTC2 = 'S')
149170051114     C                             or (wTC1 = 'S'  and  wTC2 = 'A')
149180051114     C                             or (wTC1 = 'S'  and  wTC2 = 'P')
149190051114     C                             or (wTC1 = 'P'  and  wTC2 = 'S')
149200051117     C***                          or (wTC1 = 'X'  and  wTC2 = 'A')
149210051117     C***                          or (wTC1 = 'A'  and  wTC2 = 'X')
149220051114     C                             or (wTC1 = 'X'  and  wTC2 = 'P')
149230051114     C                             or (wTC1 = 'P'  and  wTC2 = 'X')
149240051114     c                   eval      wcomb = 'S'
149250051114     c                   endif
149260051114     C                   ENDSR
149270060227     C**************************************************************************
149280060301     C*  Richiamo pgm di richiesta/controllo motivazione variazione
149290060227     C**************************************************************************
149300060227     C     ges_mtv       BEGSR
149310060227     c                   clear                   fnlv85ds
149320060227     c                   movel     'V'           ilv85tel
149330060227     c                   if        d48ffr = 'S' or d48ffr = 'V'
149340060227     c                   clear                   ilv85wdw
149350060227     c                   else
149360060301     c                   if        d48ffr = *blanks
149370060301     c                   movel     'E'           ilv85wdw
149380060301     c                   else
149390060227     c                   movel     'N'           ilv85wdw
149400060301     c                   endif
149410060227     c                   endif
149420060227     c                   movel     d48cvb        ilv85cvb
149430060227     c                   if        d48mtv <> *blanks
149440060227     c                   movel     d48mtv        ilv85mtv
149450060227     c                   movel     d48not        ilv85nt1
149460060227     c                   move      d48not        ilv85nt2
149470060227     c                   endif
149480060227     c                   movel     fnlv85ds      kpjbu
149490060302     c                   call      'FNLV85R'
149500060227     c                   parm                    kpjba
149510060227     c                   movel     kpjbu         fnlv85ds
149520060227     c                   if        olv85err = '1'
149530060227     c                   seton                                        90
149540060227     C                   eval      vidmsg='Motivazione variazione mancante o er-
149550060227     c                             rata'
149560060227     c                   endif
149570060301     c                   if        olv85f12 = ' ' and olv85err = ' '
149580060301     c                   movel     'N'           w48mct
149590060301     c                   endif
149600060227     c                   endsr
149610060301     C**************************************************************************
149620060301     C*  Richiamo pgm per storicizzazione motivazione variazione
149630060301     C**************************************************************************
149640060301     C     sto_mtv       BEGSR
149650060303     c                   clear                   fnlv85ds
149660060301     c                   movel     ' '           ilv85tla
149670060301     c                   movel     'R'           ilv85tel
149680060301     c                   movel     d48cvb        ilv85cvb
149690060301     c                   z-add     arbdtv        ilv85dac
149700060301     c                   z-add     arborv        ilv85orc
149710060301     c                   z-add     arbaas        ilv85aas
149720060301     c                   z-add     arblnp        ilv85lnp
149730060301     c                   z-add     arbnrs        ilv85nrs
149740060301     c                   z-add     arbnsp        ilv85nsp
149750060303     c                   if        d48mtv <> *blanks and
149760060303     c                             (d48ffr = 'E' or d48mct = 'N')
149770060303     c                   movel     d48mtv        ilv85mtv
149780060303     c                   movel     d48not        ilv85nt1
149790060303     c                   move      d48not        ilv85nt2
149800060303     c                   endif
149810060301     c                   movel     fnlv85ds      kpjbu
149820060301     c                   call      'FNLV85R'
149830060301     c                   parm                    kpjba
149840060301     c                   endsr
149850120330     C**************************************************************************
149860120330     C*  AGGIORNAMENTO DA MANUTENZIONE BOLLE DI SEDE (CAUS.VAR.BOLLA "SD")
149870120330     C**************************************************************************
149880120330     C     GESSD         BEGSR
149890120330     c                   clear                   errp
149900120330     c                   clear                   erra
149910120419     c                   clear                   savcpi_p
149920120419     c                   clear                   savksc_p
149930120419     c                   clear                   savctr_p
149940120419     c                   clear                   savcpi_a
149950120419     c                   clear                   savksc_a
149960120419     c                   clear                   savctr_a
149970120330     c* chaino le bolle in partenza e in arrivo
149980120330     C     KARB          CHAIN(e)  FNBLP01L
149990120402    1c                   if        %error
150000120330     c                   eval      errp='A'
150010120402   x1c                   else
150020120402    2c                   if        not %found(fnblp01l)
150030120330     c                   eval      errp='N'
150040120418     c                   else
150050120418     c                   eval      savcpi_p=arbcpi
150060120418     c                   eval      savksc_p=arbksc
150070120418     c                   eval      savctr_p=arbctr
150080120402    2c                   endif
150090120402    1c                   endif
150100120330     C     KARB          CHAIN(e)  FNARB01L
150110120402    1c                   if        %error
150120120330     c                   eval      erra='A'
150130120402   x1c                   else
150140120402    2c                   if        not %found(fnarb01l)
150150120330     c                   eval      erra='N'
150160120413     c                   else
150170120413     c                   eval      savcpi_a=arbcpi
150180120418     c                   eval      savksc_a=arbksc
150190120418     c                   eval      savctr_a=arbctr
150200120402    2c                   endif
150210120402    1c                   endif
150220120402     c* Una delle due bolle allocate --> restituisco messaggio di avviso
150230120402    1c                   if        errp='A' or erra='A'
150240120402     c                   eval      d48err='A'
150250120402     c                   eval      d48msg='Bolla di filiale allocata da altro l+
150260120418     c                             avoro:NON AGGIORNATA in'
150270120418    2c                   if        errp='A'
150280120418     c                   eval      d48msg=%trim(d48msg)+' Partenza'
150290120418    3c                   if        erra='A'
150300120418     c                   eval      d48msg=%trim(d48msg)+' e'
150310120418    3c                   endif
150320120418    2c                   endif
150330120418    2c                   if        erra='A'
150340120418     c                   eval      d48msg=%trim(d48msg)+' Arrivo'
150350120418    2c                   endif
150360120402    1c                   endif
150370120330     c* se entrambe le bolle allocate/inesistenti vado a fine
150380120402    1c                   if        errp<>*blanks and erra<>*blanks
150390120330     c                   leavesr
150400120402    1c                   endif
150410120404     c*Variazione di PESO
150420120404    1c                   if        §bdpkf<>arbpkf
150430120402     c                   z-add     §bdpkf        arbpkf
150440120404    1c                   endif
150450120404     c*Variazione di VOLUME: il volume, in sede, può essere variato solo da
150460120404     c* profilo di sede. Siccome in questi casi la variazione avviene solitamente
150470120404     c* quando in filiale la bolla non c'è più sostituisco sempre
150480120404    1c                   if        §bdvlf<>arbvlf or §bdfvf<>arbfvf
150490120404     c                   z-add     §bdvlf        arbvlf
150500120411     c                   movel     §bdfvf        arbfvf
150510120404    1c                   endif
150520120529     c* IMOPORTO DA ASSICURARE
150530120529     c                   if        §btias<>arbias or §btvas<>arbvas
150540120529     c                   z-add     §btias        arbias
150550120529     c                   movel     §btvas        arbvas
150560120529     c                   endif
150570120404     c* Variazione TASSAZIONE: vengono caricate solo se c'è corrispodenza
150580120404     c* fra sede e filiale al livello di tipo di fattura: Segue Fattura o
150590120404     c* Fattura Immediata
150600120411     c* Chaino fiar6 per vedere se S.F. o  F.I.:
150610120404     c                   clear                   wfatfil
150620120404     c                   clear                   ar6ksc
150630120411     c                   clear                   war6
150640120416     c                   clear                   fiar6000
150650120416     C                   MOVEL     §bttrc        WTRC
150660120404     c     karb2         chain     fiar601l
150670120411    1c                   if        %found(fiar601l) and ar6nft= 0 or
150680120411     c                             not %found(fiar601l)
150690120404     c                   eval      wfatfil='SF'
150700120411     c                   if        not %found(fiar601l)
150710120411     c                   eval      war6='N'
150720120411     c                   endif
150730120404     c                   else
150740120404     c                   eval      wfatfil='SD'
150750120404    1c                   endif
150760120404    1c                   if        d48cvb=wfatfil
150770120404     c
150780120404     c* . . . . . . . QUANTITA' DA FATTURARE
150790120411    2c                   if        §btqft<>arbqft
150800120411     c                   z-add     §btqft        arbqft
150810120404    2c                   endif
150820120404     c* . . . . . . . CLIENTE DI FATTURAZIONE e COD.TARIFFA
150830120419     C*
150840120404     C                   MOVEL     '3A'          COD
150850120404     C                   MOVEL(P)  ARBCBO        KEY
150860120404     C                   EXSR      CTABEL
150870120404     C  N30              MOVEL     TBLUNI        DS3A
150880120404     C   30              CLEAR                   DS3A
150890120413     c* assegnato
150900120404    2c                   if        §bttrc='2' or %subst(§3atb1:1:1)='A'
150910120404    3c                   if        §btksc<>ar6ksc
150920120404     c                   eval      ar6ksc=§btksc
150930120404    3c                   endif
150940120404    3c                   if        §btctr<>ar6ctr
150950120404     c                   eval      ar6ctr=§btctr
150960120404    3c                   endif
150970120418     c* Se assegnato no seconda bolla devo aggiornare su arb anche il ksc/ctr
150980120418    2c                   if        %subst(§3atb1:1:1)='A'
150990120418    3c                   if        §btksc<>savksc_a
151000120418     c                   eval      savksc_a=§btksc
151010120418    3c                   endif
151020120418    3c                   if        §btctr<>savctr_a
151030120418     c                   eval      savctr_a=§btctr
151040120418    3c                   endif
151050120418     c                   endif
151060120404   x2c                   else
151070120413     c* franco
151080120413     c*  se non codificato non faccio niente
151090120413     c                   z-add     §btksc        w0040
151100120418    3c                   if        w0040<>8888 and §btksc<>arbksc
151110120413     c                   exsr      gessd_ksc
151120120411    3c                   endif
151130120411    3c                   if        §btctr<>arbctr
151140120411     c                   eval      arbctr=§btctr
151150120411    3c                   endif
151160120404    2c                   endif
151170120404     c* . . . . . . . CAMPI DI TASSAZIONE
151180120404     c* . . . . Divisa
151190120404     c                   if        §btdiv<>ar6div
151200120404     c                   eval      ar6div=§btdiv
151210120404     c                   endif
151220120404     c* . . . . Porto
151230120404     c                   if        §btpor<>ar6por
151240120404     c                   eval      ar6por=§btpor
151250120404     c                   endif
151260120411     c* . . . . Imponibile IVA
151270120411     c                   if        §btimv<>ar6imv
151280120411     c                   eval      ar6imv=§btimv
151290120411     c                   endif
151300120411     c* . . . . Causale esenzione
151310120411     c                   if        §btcei<>ar6cei
151320120411     c                   eval      ar6cei=§btcei
151330120411     c                   endif
151340120404     c* . . . . Varie - In §SV e §VA ci sono le varie ricevute dal chiamante
151350120404     c*                 IN WSV e WVA carico le varie presenti sul fiar6/7
151360120411     C                   MOVEL     §sv(1)        ar6sv1
151370120411     C                   Z-ADD     §va(1)        ar6va1
151380120411     C                   MOVEL     §sv(2)        ar6sv2
151390120411     C                   Z-ADD     §va(2)        ar6va2
151400120411     C                   MOVEL     §sv(3)        ar6sv3
151410120411     C                   Z-ADD     §va(3)        ar6va3
151420120411     c* cancello varie su fiar7 se presenti
151430120419     C                   MOVEL     §bttrc        WTRC
151440120411     c     karb2         setll     fiar7000
151450120411     c     karb2         reade     fiar7000
151460120411     c                   dow       not %eof(fiar701l)
151470120411     C                   DELETE    FIAR7000
151480120411     c     karb2         reade     fiar7000
151490120411     c                   enddo
151500120411     c* se presenti più di 3 verie le memorizzo nell'fiar7
151510120411    2c                   if        §sv(4)<>' '
151520120411     c                   z-add     4             inx
151530120411     c                   clear                   AR7ATB
151540120411     c                   z-add     d48aas        AR7AAS
151550120411     c                   z-add     d48lnp        AR7LNP
151560120411     c                   z-add     d48nrs        AR7NRS
151570120411     c                   z-add     d48nsp        AR7NSP
151580120419     c                   movel     §bttrc        AR7TRC
151590120411     c                   dow       §sv(inx)<>*blanks or inx>20
151600120411     C                   MOVEL     §sv(inx)      ar7svn
151610120411     C                   z-add     §va(inx)      ar7van
151620120411     C                   write     fIAR7000
151630120411     c                   add       1             inx
151640120411     c                   enddo
151650120411    2C                   endif
151660120416     c* Aggiornamento FIAR6
151670120416     c* se record non esiste lo creo solo se è pieno almeno uno dei campi
151680120416    2c                   if        war6='N'
151690120416    3c                   if        ar6ksc>0 or ar6por>0 or ar6sv1<>*blanks
151700120416     c                             or ar6sv2<>*blanks or ar6sv3<>*blanks
151710120416     c                             or ar6cei<>*blanks
151720120411     c                   eval      AR6AAS=d48aas
151730120411     c                   eval      AR6LNP=d48lnp
151740120411     c                   eval      AR6NRS=d48nrs
151750120411     c                   eval      AR6NSP=d48nsp
151760120411     c                   eval      AR6TRC=§bttrc
151770120411
151780120411     c                   write     fiar6000
151790120416    3c                   endif
151800120417     c                   else
151810120416    3c                   if        ar6ksc>0 or ar6por>0 or ar6sv1<>*blanks
151820120416     c                             or ar6sv2<>*blanks or ar6sv3<>*blanks
151830120416     c                             or ar6cei<>*blanks
151840120404     c                   except    ar6sd
151850120416     c                   else
151860120416     c                   delete    fiar6000
151870120416     c                   endif
151880120416    2c                   endif
151890120404   x1c                   else
151900120404     c                   unlock    fiar601l
151910120404    1c                   endif
151920120404     c* aggiornamento file
151930120418     c* BLP
151940120404    1c                   if        errp=*blanks
151950120418     c                   eval      arbcpi=savcpi_p
151960120418     c* solo se assegnato rimetto il ksc/ctr che c'era
151970120418     c                   if        %subst(§3atb1:1:1)='A'
151980120418     c                   eval      arbksc=savksc_p
151990120418     c                   eval      arbctr=savctr_p
152000120418     c                   endif
152010120404     c                   except    arbsdblp
152020120404    1c                   endif
152030120418     c* ARB
152040120404    1c                   if        erra=*blanks
152050120416     c* P.iva DEL MITTENTE SU ARB LA AGGIORNO SOLO SE FRANCO NO DOPPIA BOLLA
152060120419     c* in caso contrario tengo la P.IVA che c'era
152070120416     c                   eval      arbcpi=savcpi_a
152080120418     c*
152090120418     c                   if        %subst(§3atb1:1:1)='A'
152100120418     c                   eval      arbksc=savksc_a
152110120418     c                   eval      arbctr=savctr_a
152120120418     c                   endif
152130120404     c                   except    arbsdarb
152140120404    1c                   endif
152150120330     c
152160120330     C                   ENDSR
152170120413     C**************************************************************************
152180120413     C*  AGGIORNAMENTi derivanti da variazioni in sede del ksc di bolla FRANCO
152190120413     C**************************************************************************
152200120413     C     GESSD_ksc     BEGSR
152210120416     C* Controllo se esiste record W di fiar4 per eventuale modifica di mittente
152220120416     C                   MOVEL     'W'           WTRC
152230120416     c                   clear                   wbl4w
152240120416     C     KARB2         CHAIN(N)  Fiar401l
152250120416     c                   if        %found(fiar401l)
152260120416     C                   MOVEL     'S'           WBL4W             1
152270120416     c                   endif
152280120416     c                   z-add     arbksc        oldksc
152290120416     c*
152300120413     c                   eval      arbksc=§btksc
152310120413     c* recupero dati del cliente da anagrafica
152320120413     C                   clear                   TIBS69DS
152330120413     C                   z-add     §btksc        I69kac
152340120413     C                   z-add     §btksc        I69kin
152350120413     C                   call      'TIBS69R'
152360120413     C                   parm                    tibs69DS
152370120413     C                   parm                    ds_cnaco
152380120413     C                   parm                    ds_cnind
152390120413     C                   parm                    ds_cnclp
152400120413     C                   parm                    ds_fncls
152410120413     c                   eval      arbrsm=bs_acorag
152420120413     c                   eval      arbinm=bs_indvia
152430120413     c                   eval      arbLOM=bs_indcit
152440120413     c                   eval      arbCAM=bs_indcae
152450120413     c                   eval      arbPRM=bs_indprv
152460120413     c                   eval      arbNZM=bs_indsta
152470120418     c* P.IVA: non aggiorno arbcpi perchè comune per blp e arb e non va bene:
152480120418     c* se bolla doppia su blp c'è la p.iva del mittente e su arb la p.iva del dest
152490120413     c* dell'aggiornamento di FNARB
152500120419     c                   if        %subst(§3atb1:1:1)='F' and §3atb2=*blanks
152510120419     c                   eval      savcpi_a=bs_indiva
152520120419     c                   endif
152530120419     c                   eval      savcpi_p=bs_indiva
152540120416     c                   exsr      up_fiar4P
152550120416     c                   exsr      up_fiar4w
152560120416     c*
152570120413     c                   endsr
152580120413     C**************************************************************************
152590120413     C* AGGIORNAMENTO RECORD P.IVA IN FIAR4  se esiste
152600120413     C**************************************************************************
152610120413     C     up_fiar4P     BEGSR
152620120413     C                   MOVEL     'P'           WTRC
152630120413     C     KARB2         CHAIN     FiAR401L                           30
152640120413     C     *IN30         IFEQ      *OFF
152650120413     c                   movel     ar4not        wcpifile
152660120413     c                   if        wcpifile <> arbcpi
152670120413     c                   movel     'T'           ar4ftr
152680120413     c                   z-add     dateu         ar4duv
152690120413     c                   z-add     dateu         ar4dtr
152700120413     C                   MOVEl     ARBCPI        AR4NOT
152710120413     C                   UPDATE    FiAR4000
152720120413     c                   else
152730120413     c                   unlock    fiar401l
152740120413     c                   endif
152750120413     C                   END
152760120413     c                   endsr
152770120416     C**************************************************************************
152780120416     C* Gestione record fiar4 record W per DATA TRASMISSIONE VAB
152790120416     C**************************************************************************
152800120416     C     up_fiar4W     BEGSR
152810120416     C* CONTROLLO SE C'E' NUOVO CODICE O VARIATO PER AGGIORNARE O ELIM
152820120416     C*  O SCRIVERE RECORD W IN ar4
152830120416     C                   MOVEL     '3Q'          COD
152840120416     C                   MOVEL(P)  ARBKSC        KEY
152850120416     C     KTAB2         CHAIN     TABEL00F                           30
152860120416     C  N30TBLFLG        IFNE      ' '
152870120416     C                   SETON                                        30
152880120416     C                   ENDIF
152890120416     C*
152900120416     C* C'E' 3Q DEVO CREARE O VARIARE
152910120416    1C  N10*IN30         IFEQ      *OFF
152920120416    2C     WBL4W         IFEQ      ' '
152930120416    3C     ARBKSC        orNE      OLDKSC
152940120416     C                   CLEAR                   DSBL4W
152950120416     C                   CLEAR                   fnlv19ds
152960120416     C                   MOVEL     DSBL4W        d19nt1
152970120416     C                   MOVEL     ARBAAS        d19AAS
152980120416     C                   MOVEL     ARBLNP        d19LNP
152990120416     C                   MOVEL     ARBNRS        d19NRS
153000120416     C                   MOVEL     ARBNSP        d19NSP
153010120416     C                   MOVEL     'W'           d19TRC
153020120416     C                   MOVEL     'T'           d19ftr
153030120416     C                   call      'FNLV19R'
153040120416     c                   parm                    fnlv19ds
153050120416    2C                   ENDIF
153060120416     C*
153070120416   X1C                   ELSE
153080120416     C* SE NO CI DEVE ESERE E PRIMA C'ERA LO DELETO
153090120416    2C     WBL4W         IFEQ      'S'
153100120416     c                   clear                   fnlv19ds
153110120416     C                   clear                   d19nt1
153120120416     c                   clear                   d19nt2
153130120416     C                   MOVEL     arbAAS        d19AAS
153140120416     C                   MOVEL     arbLNP        d19LNP
153150120416     C                   MOVEL     arbNRS        d19NRS
153160120416     C                   MOVEL     arbNSP        d19NSP
153170120416     C                   MOVEL     'W'           d19TRC
153180120416     c                   movel     'T'           d19ftr
153190120416     C                   CALL      'FNLV19R'
153200120416     C                   PARM                    fnlv19ds
153210120416    2C                   ENDIF
153220120416    1C                   ENDIF
153230120416     c                   endsr
153240130321     C**************************************************************************
153250130411     C* Invio Variazione bolla a PDA
153260130321     C**************************************************************************
153270130411     C     sr_pda        BEGSR
153280140908     c* Solo se la chiamata non è cieca:
153290140908     c                   if        d48ffr<>'E'
153300141020     c****               if        d66pda<>' ' and (*in10 or waltrabolla=' ')
153310141020     c                   if        d66pda<>' ' and *in10
153320131227     c                             and wpda_ndc>0 and wpda_dcm=0
153330131227     c* richiamo pgm per verificare che ci sia stata un'effettiva variazione
153340131227     c                   clear                   fnlva1ds
153350131227     c                   eval      IA1AAS=arbaas
153360131227     c                   eval      IA1LNP=arblnp
153370131227     c                   eval      IA1NRS=arbnrs
153380131227     c                   eval      IA1NSP=arbnsp
153390131227     c                   eval      IA1DTV=arbdtv
153400131227     c                   eval      IA1ORV=arborv
153410131227     c                   eval      IA1CVB=arbcvb
153420131227     c                   eval      IA1PRU=arbpru
153430131227     c                   movel     fnlva1ds      kpjbu
153440131227     c                   call      'FNLVA1R'
153450131227     c                   parm                    kpjba
153460131227     c                   movel     kpjbu         fnlva1ds
153470131227     c*
153480131227     c                   if        OA1ESITO='V'
153490140130     c* se filiale partita con nuova gestione telefonate richiamo pgm gestione
153500140130     c* note che si preoccuperà anche del richiamo a fidg42r
153510140130     c                   if        *in87=*off
153520140130     c                   clear                   fnlrn6ds
153530140130     c                   eval      in6mod='I'
153540140130     c                   eval      in6aas=arbaas
153550140130     c                   eval      in6lnp=arblnp
153560140130     c                   eval      in6nrs=arbnrs
153570140130     c                   eval      in6nsp=arbnsp
153580140130     c                   eval      in6pgm='FNLR48R'
153590140130     c                   eval      IN6FGS=arbifp
153600140130     c                   eval      IN6NFV=wpda_ndc
153610140130     c                   eval      IN6DFV=wpda_ddc
153620140130     c                   eval      IN6PDR=arbpdc
153630140130     c                   eval      IN6VAR=d66pda
153640140130     c                   eval      IN6CONS='S'
153650140130     c                   call      'FNLRN6R'
153660140130     C                   parm                    kpjba
153670140130     C                   parm                    fnlrn6ds
153680140130     c                   else
153690140130     c* altrimenti richiamo direttemente fidg42r
153700131227     c
153710130411     c                   z-add     arbifp        w0030             3 0
153720130411     c     w0030         chain     azorg01l
153730130411     c                   if        %found(azorg01l)
153740130411     c                   eval      og148=orgde8
153750131227     c                   if        §ogpdacon<>' '
153760130411     c                   clear                   fidg42ds
153770130411     c                   eval      CO42FGS=arbifp
153780130411     c                   eval      CO42NDC=wpda_ndc
153790130411     c                   eval      CO42DDC=wpda_ddc
153800130411     c                   eval      CO42AAS=arbaas
153810130411     c                   eval      CO42LNP=arblnp
153820130411     c                   eval      CO42NRS=arbnrs
153830130411     c                   eval      CO42NSP=arbnsp
153840130411     c                   eval      CO42CMT='N'
153850130411     c                   movel     fidg42ds      kpjbu
153860130411     c                   call      'FIDG42R'
153870130411     c                   parm                    kpjba
153880130411     c                   endif
153890130411     c                   endif
153900140130     c                   endif
153910131227     c                   endif
153920130411     c                   endif
153930140908     c                   endif
153940130411     c                   endsr
153950141021     C**************************************************************************
153960141021     C* Gestione finestra per richiesta dirottamento
153970141021     C**************************************************************************
153980141021     C     ges_wdw02     BEGSR
153990141021     c                   clear                   winf12
154000141022     c                   eval      atrf21=rimm
154010141125     c                   setoff                                       782879
154020141125     c* iN partenza non si può mai variare senza il dirottamento --> disabilito l'F8
154030141125     c                   if        not *in10
154040141125     c                   seton                                        79
154050141125     c                   endif
154060141022     c                   eval      w02txt='La Linea di Arrivo desunta è diversa-
154070141022     c                              dalla Linea di Arrivo della bolla'
154080141029     c                   exsr      sr_NoAbiDir
154090141021    6c                   do        *hival
154100141021     c                   exfmt     lr48w02
154110141113     c  n78              setoff                                       28
154120141113     c  n78              clear                   w02msg
154130141021     c                   select
154140141021     c* F21=Richiesta dirottamento
154150141021     c                   when      *inkv
154160141021     c                   eval      wdir='1'
154170141021     c* F12=Ritorno
154180141021     c                   when      *inkl
154190141021     c                   eval      winf12='1'
154200141021     c                   endsl
154210141106     c                   if        *inkv or *inkh or *inkl
154220141021     c                   leave
154230141021     c                   endif
154240141021    6c                   enddo
154250141113     c                   setoff                                       7828
154260141021     c                   endsr
154270141029     C**************************************************************************
154280141029     C* Verifica condizioni che disabilitano F21=Dirottamento
154290141029     C**************************************************************************
154300141029     C     sr_NoAbiDir   BEGSR
154310141029
154320141029     c*******************************************
154330141029     c* Casi di disabilitazione del Dirottamento:
154340141029     c*******************************************
154350141029     c* - BOLLA LEGATA
154360141029     c* Se la bolla è legata il dirottamento può essere richiesto solo sull'ultima figlia
154370141029     c     karb          chain     fnlbl02l
154380141029     c                   if        %found(fnlbl02l)
154390141029     c* disattivo l'f21 ed emetto messaggio per avvisare che il dirottamento deve essere
154400141029     c* fatto sulla figlia
154410141029     c                   seton                                        7828
154420141029     c                   eval      w02msg ='Bolla legata:il dirottamento può -
154430141029     c                             essere richiesto solo sulla figlia'
154440141029     c                   clear                   atrf21
154450141029     c                   leavesr
154460141029     c                   endif
154470141029
154480141029     c* - PRESENZA DISPO DA ESAMINARE
154490141029     c**   kidc1         setll     tiidc01l
154500141029     c***                if        %equal(tiidc01l)
154510141029     c                   clear                   fnlry09ds2
154520141029     c                   eval      ILRY09TCH='T'
154530141029     c                   eval      ILRY09AAS=d48aas
154540141029     c                   eval      ILRY09LNP=d48lnp
154550141029     c                   eval      ILRY09NRS=d48nrs
154560141029     c                   eval      ILRY09NSP=d48nsp
154570141029     c                   eval      kpjbu=fnlry09ds2
154580141031     c                   call      'FNLRY09C'
154590141029     c                   parm                    kpjba
154600141029     c                   eval      fnlry09ds2=kpjbu
154610141029     c                   if        OLRY09ESIT='1'
154620141029     c                   seton                                        7828
154630141029     c                   eval      w02msg ='Presenti Disposizioni di Consegna-
154640141029     c                              da elaborare: non è possibile dirottare'
154650141029     c                   clear                   atrf21
154660141029     c                   leavesr
154670141029     c                   endif
154680141029     c
154690141029     c* - PRESENZA FERMO DEPOSITO
154700141029     c                   if        vidffd<>*blanks
154710141029     c                   seton                                        7828
154720141125     c                   if        not *in79
154730141125     c                   eval      w02msg='Presente Fermo Deposito: F8 per non -
154740141029     c                             dirottare, F12 per correggere'
154750141125     c                   else
154760141125     c                   eval      w02msg='Presente Fermo Deposito: F12 per cor-
154770141125     c                             reggere'
154780141125     c                   endif
154790141029     c                   clear                   atrf21
154800141029     c                   endif
154810141112     c* - PRESENZA giacenza aperta
154820141112     c                   if        *in04
154830141112     c                   seton                                        7828
154840141112     c                   eval      w02msg='Presente Giacenza aperta: non è poss-
154850141112     c                             ibile dirottare'
154860141112     c                   clear                   atrf21
154870141112     c                   endif
154880141029     c                   endsr
154890150414     C**************************************************************************
154900150414     C* controlli per cons. particolare "Z"=EXPO
154910150414     C**************************************************************************
154920151119     C*    sr_chkexpo    BEGSR
154930150414     c* Se Expo errore se bolla con contrassegno
154940151119     c*                  if        §3afca=1
154950151119     C*                  MOVEL     MSG(159)      VIDMSG
154960151119     C*                  SETON                                        9028
154970151119     C*                  leavesr
154980151119     c*                  endif
154990150414     c* se expo errore se non è franco
155000151119     c*                  if        %subst(§3atb1:1:1)<>'F'
155010151119     C*                  MOVEL     MSG(160)      VIDMSG
155020151119     C*                  SETON                                        9028
155030151119     C*                  leavesr
155040151119     c*                  endif
155050150414     c* errore se lna non è in tabella
155060151119     c*                  move      v1clna        w003a
155070151119     c*                  if        %lookup(w003a:fil_expo)=0
155080151119     C*                  MOVEL     MSG(161)      VIDMSG
155090151119     c*                  eval      vidmsg=%trim(vidmsg) + ' ' + w003a
155100151119     C*                  SETON                                        9028
155110151119     C*                  leavesr
155120151119     c*                  endif
155130151119     c*                  endsr
155140151016     C*--- Registro peso e volume usati perfatturaz. in FIAR5 --------*
155150151016     C     REGPesivol    BEGSR
155160151016     c                   clear                   dar5fat
155170151016     c                   Eval      kar5trd='FAT'
155180151016     c     kar5          Chain     Fiar531c
155190151016     c                   If        %Found(Fiar531c)
155200151016     c                   Movel     ar5uni        dar5fat
155210151016     c                   endif
155220151016     c* Se tassata
155230151016    1c                   if        ar6imv>0
155240151016     c* Volume
155250170420     c                   movel     arbvlf        §ar5vltas
155260170420     c                   movel     arbfvf        §ar5fvtas
155270170420     c                   if        tasvlt>0  and
155280170420     c                             PesVolIMV=Tassatoimv
155290160530     c                   z-add     tasvlt        §ar5vltas
155300160530     c                   movel     tasfvt        §ar5fvtas
155310170420     c*                  else
155320170420     c*                  movel     arbvlf        §ar5vltas
155330170420     c*                  movel     arbfvf        §ar5fvtas
155340160530     c                   endif
155350151016     c* Peso
155360151016     c                   movel     arbpkf        §ar5pktas
155370151016     c                   movel     'R'           §ar5fptas
155380151016     c
155390151016     c* Se il totale imponibile aggiornato senza arrotondamento
155400151016     c*  è = a quello memorizzato al momento del richiamo al
155410151016     c*   TNSF20R--> memorizzo se usato peso VDL (-tara)
155420151016    2c                   if        PesVolIMV=Tassatoimv  and
155430151016     c                             PesVolspc='S'
155440151016     c                   movel     PesVolPkcn    §ar5pktas
155450151016    3c                   if        arbncp=arbncl
155460151016     c                   movel     'T'           §ar5fptas
155470151016     c                   else
155480151016     c                   movel     'Z'           §ar5fptas
155490151016    3c                   endif
155500151016     c
155510151016    2c                   endif
155520151016     c                   eval      §AR5DATAS=%dec(%date())
155530151016     c                   eval      §AR5HMTAS=%dec(%time())
155540151016     c                   eval      §AR5PRTAS=knmus
155550151016     c                   eval      §AR5NJTAS=knmtd
155560151016     c*
155570151016    1c                   endif
155580151016     c
155590151016      * aggiorno Fiar5 record "FAT"
155600151016if  1c                   If        %Found(Fiar531c)
155610151016     c* Se ho eliminato la tassazione  deleto
155620151016     c                   if        ar6imv=0
155630151016     c                   if        recname='FIAR5000'
155640151016     c                   delete    fiar500s
155650151016     c                   else
155660151016     c                   delete    fiar5p0s
155670151016     c                   endif
155680151016     c                   else
155690151016     c                   Movel(p)  dar5fat       Ar5uni
155700151016     c                   eval      ar5dac=%dec(%date())
155710151016     c                   eval      ar5orc=%dec(%time())
155720151016     c                   if        recname='FIAR5000'
155730151016     c                   Update    Fiar500s
155740151016     c                   else
155750151016     c                   Update    Fiar5p0s
155760151016     c                   endif
155770151016     c                   endif
155780151016      * scrivo   Fiar5 record "FAT"
155790151016   x1c                   Else
155800151016    2c                   if        ar6imv>0
155810151016     c                   Clear                   Fiar500s
155820151016     c                   Clear                   Fiar5p0s
155830151016     c                   Eval      Ar5Aas = ArbAas
155840151016     c                   Eval      Ar5Lnp = ArbLnp
155850151016     c                   Eval      Ar5Nrs = ArbNrs
155860151016     c                   Eval      Ar5Nsp = ArbNsp
155870151016     c                   Eval      Ar5Trd = 'FAT'
155880151016     c                   Movel(p)  dar5fat       Ar5uni
155890151016     c                   eval      ar5dac=%dec(%date())
155900151016     c                   eval      ar5orc=%dec(%time())
155910151016     c                   Eval      AR5FT1='T'
155920151016     c                   Eval      AR5DT1=99999999
155930151016     c                   Eval      AR5FT2='T'
155940151016     c                   Eval      AR5DT2=99999999
155950151016     c                   Eval      AR5FT3='T'
155960151016     c                   Eval      AR5DT3=99999999
155970151016     c* Verifico dove si trova TITAS per la write di fiar5
155980151016     c     karb          chain     titaS30c
155990160104     c                   if        %found(titas30c)
156000151019     c                   if        recnamet='TITAS000'
156010151016     c                   Write     Fiar500s
156020151016     c                   else
156030151016     c                   Write     Fiar5p0s
156040151016     c                   endif
156050160104     c                   else
156060160104     c                   Write     Fiar500s
156070160104     c                   endif
156080151016    2c                   EndIf
156090151016    1c                   EndIf
156100151016     c
156110151016     c                   ENDSR
156120141020     C**************************************************************************
156130141020     C* Scrittura TIIDC per Caricamento Disposizione di dirottamento
156140141020     C**************************************************************************
156150141020     C     sr_tiidc      BEGSR
156160141020      /free
156170141020       clear tiidc000;
156180141020       exsr repnum;
156190141020       if esito = *blanks;
156200141020          IDCPRG=kprg                         ;
156210151201       // Se progressivo non reperito lo lascio a 0:
156220151201       // per le dispo di reso e dirottamento non abbiamo la necessità di
156230151201       // avere il progressivo dal momento che per una stessa spedizione
156240151201       // non è ammessa la presenza di + dispo se una di queste è dirottamento o reso
156250151201       // Abbiamo quindi preferito scrivere a 0 il progressivo piuttosto che perdere la
156260151201       // dispo. Questo discorso non val per le dispo che riceviamo da web
156270151201       //else;
156280151201       //   leavesr;
156290141020       endif;
156300141020       IDCINSDATA   = %int( %subst( %char( %dec( %timestamp() ) )
156310141020                                    : 1 : 14 ) );
156320141020       //IDCtimestn   = ;
156330141020       IDCMODACC='B'                       ;
156340141020       //IDCIP = ;
156350141020       //IDCIDCLI = ;
156360141023       if *in10;
156370141023          IDCTIPOCLI= 'D' ;
156380141023       else;
156390141023          IDCTIPOCLI= 'M' ;
156400141023       endif;
156410141020       IDCWAAS   = arbaas;
156420141020       IDCWLNP   = arblnp;
156430141020       IDCWNRS   = arbnrs;
156440141020       IDCWNSP   = arbnsp;
156450141020       IDCAAS    = arbaas;
156460141020       IDCLNP    = arblnp;
156470141020       IDCNRS    = arbnrs;
156480141020       IDCNSP    = arbnsp;
156490141021       IDCFGS    = lnacons;
156500141020       IDCTIPODIS = '3';
156510141020       //IDCMAILAVV     ;
156520141020       //IDCTELAVV  ;
156530141020       //IDCALERT
156540141021          IDCRSD   = vidrsd;
156550141021          IDCIND   = vidind;
156560141021          IDCCAD   = vidcad;
156570141021          IDCLOD   = vidlod;
156580141021          IDCPRD   = vidprd;
156590141021          IDCNAZ   = vidnzd;
156600141020       // bolla da dirottare
156610141021          IDCNEWLNA=d_o95lna;
156620141021          IDCFLA   ='S';
156630141020       IDCELAFLG='S';
156640141022       IDCELAUTE=knmus;
156650141023       idcref=Vidref;
156660141023       idcteld=vidteld;
156670141020
156680141020         write tiidc000;
156690141020       endsr;
156700141020        //-----------------------------------------------------------------------*
156710141020        // Reperimento progressivo richiesta da numeratore 650
156720141020        //-----------------------------------------------------------------------*
156730141020        begsr RepNum;
156740141020
156750141022          annocur  = %subdt(%date():*years);
156760141020          clear esito;
156770141020          $finenum=*off;
156780141020          clear trul33ds;
156790141020
156800141020          I33TLa = 'L';
156810141020          I33Ope = *ZEROS;
156820141030          I33CNu = 651;
156830141020          I33Num = 1;
156840141020
156850141020          KPJBU = TRUL33DS;
156860141020
156870141020          dow $finenum=*off   ;
156880141020             Trul33R(kpjba);
156890141020
156900141020             TRUL33DS = KPJBU;
156910141020
156920141020             if O33Err <> *zeros;
156930141020               Esito = '2';
156940141020               $finenum=*on;
156950141020             else;
156960141020               w0090=o33nri;
156970141020               kprg=%dec(%subst(%editc(annocur:'X'):3:2):2:0)*1000000000+w0090;
156980141020               setll kprg tiidc02l;
156990141020        // Se numero già presente nel file TIIDC ne cerco un altro
157000141020               if not %equal;
157010141020                  $finenum=*on;
157020141020               endif;
157030141020             endif;
157040141020          enddo;
157050141020
157060141020          endsr;
157070160318        //-----------------------------------------------------------------------*
157080160318        // Aggiornamenti specifici per "VEDI PACCO DPD"
157090160318        //-----------------------------------------------------------------------*
157100160318        begsr sr_VediPDpd ;
157110160318        // Per prima cosa chaino fiar4 record "I" per verificare se si tratta di
157120160318        // "VEDI PACCO"
157130170922        //   chain(n) (arbaas:arblnp:arbnrs:arbnsp:'I') fiar401l ;
157140170922        //   if %found(fiar401l);
157150170922        //      dsbl4i=ar4not;
157160170922        //    if §b4trd='1';
157170170922             chain (arbaas:arblnp:arbnrs:arbnsp) fipnd01l ;
157180170922             if %found(fipnd01l);
157190170922                dpndflo=pndflo;
157200170922              if §PNDTRD ='1';
157210160318        // Se "VEDI PACCO" con giro forzato come da tabella NSD:
157220160318                 chain (arbaas:arblnp:arbnrs:arbnsp) fiarg01l;
157230160318                 if %found(fiarg01l) and argcgi=§nsdcgi;
157240160318        // Memorizzo che, appena aggiornata la bolla, dovrò richiamare il pgm per togliere
157250160318        // la sped. dalla distinta
157260160318        // (Solo se manutenzione fatta in arrivo)
157270160318                  if *in10 and arbndc>0 ;
157280160318                    wtogli='1'      ;
157290160318                  endif;
157300160318        // Cancello il giro forzato
157310160318                    clear argcgi;
157320160318                    clear argtgi;
157330160318                    update fiarg000;
157340160318                 else ;
157350160318                    unlock fiarg01l ;
157360160318                 endif;
157370160318        // Aggiorno il flag Traduttore da 1 a 2
157380170922          //     clear  fnlv19ds ;
157390170922          //     D19AGG='E';
157400170922          //     D19FTR='T';
157410170922          //     D19AAS=arbaas;
157420170922          //     D19LNP=arblnp;
157430170922          //     D19NRS=arbnrs;
157440170922          //     D19NSP=arbnsp;
157450170922          //     D19TRC='I';
157460170922          //     if *in10;
157470170922          //        §b4trd='2';
157480170922          //     else;
157490170922          //        §b4trd='3';
157500170922          //     endif;
157510170922          //     D19NT1=dsbl4i;
157520170922          //     fnlv19r(fnlv19ds);
157530170922                 if *in10;
157540170922                    §pndtrd='2';
157550170922                 else;
157560170922                    §pndtrd='3';
157570170922                 endif;
157580170922                 pndflo = dpndflo;
157590170922                 update fipnd000;
157600170922              else ;
157610170922                 unlock fipnd01l  ;
157620160318              endif;
157630160318             endif;
157640160318        endsr;
157650160318        //-----------------------------------------------------------------------*
157660160318        // Toglie da distinta spedizione "VEDI PACCO DPD"
157670160318        //-----------------------------------------------------------------------*
157680160318        begsr sr_TogliVDpd ;
157690160318                    clear  fnlr98ds;
157700160318                    I98AAS    = arbaas;
157710160318                    I98LNP    = arblnp;
157720160318                    I98NRS    = arbnrs;
157730160318                    I98NSP    = arbnsp;
157740160318                    I98VIDEO  = 'N';
157750160318                    I98npg    = 4  ;
157760160318                    I98fgs    = arbifp ;
157770160318                    I98ndc    = arbndc ;
157780160331                    I98ALCDST = ' ' ;
157790160331                    I98CTLDST = ' ' ;
157800160330                    I98STRCMT = '  ';
157810160318                    kpjbu = fnlr98ds ;
157820160330                    fnlr98c(kpjba);
157830160401        //          if o98esito='1';
157840160401        //              msgforzatxt=O98MSGERR ;
157850160401        //          endif;
157860160318        endsr;
157870141020      /end-free
157880170411        //-----------------------------------------------------------------------*
157890170411        // Memorizzazione dati fattura annullata/stornata
157900170411        //-----------------------------------------------------------------------*
157910170411        begsr sr_Fnfta     ;
157920170411        FTAAAS = ar6aas    ;
157930170411        FTALNP = ar6lnp    ;
157940170411        FTANRS = ar6nrs    ;
157950170411        FTANSP = ar6nsp    ;
157960170411        FTAMGS = arbmgs    ;
157970170411        FTADIV = ar6div    ;
157980170411        FTAALI = ar6ali    ;
157990170411        FTACEI = ar6cei    ;
158000170411        FTAIFT = ar6ift    ;
158010170411        FTAFIV = ar6fiv    ;
158020170411        FTANFT = ar6nft    ;
158030170411        FTADFT = ar6dft    ;
158040170411
158050170505        write(e) fnfta000    ;
158060170411        endsr;
158070170411
158080940321     C*---------------------------------------------------------------*
158090041007     Ofnblp000  E            ARBDblp
158100041007     O                       arbrsd
158110041007     O                       arbind
158120041007     O                       arbcad
158130041007     O                       arblod
158140041007     O                       arbprd
158150041007     O                       arbnzd
158160041007     O                       arbfin
158170041007     O                       arbcpi
158180041007     O                       arbpkb
158190041007     O                       arbpkf
158200041007     O                       arbvlb
158210041007     O                       arbfvb
158220041007     O                       arbvlf
158230041007     O                       arbfvf
158240041007     O                       arbffd
158250041007     O                       arbcts
158260041022     Ofnblp000  E            ARBKblp
158270041022     O                       arbcbo
158280041022     Ofnblp000  E            ARBGblp
158290041022     O                       arbdcr
158300041022     O                       arbhcr
158310041022     O                       arbtcr
158320041022     O                       arbgc1
158330041022     O                       arbgc2
158340041022     O                       arbtc1
158350041022     O                       arbtc2
158360041022     O                       arbft3
158370041025     Ofnblp000  E            ARBMblp
158380041025     O                       arbksc
158390041025     O                       arbrsm
158400041025     O                       arbinm
158410041025     O                       arbcam
158420041025     O                       arblom
158430041025     O                       arbprm
158440041025     O                       arbnzm
158450041025     O                       arbfap
158460041025     O                       arbcpi
158470041025     O                       arbCTR
158480041025     O                       arbfvf
158490041025     O                       arbvlf
158500041027     Ofnblp000  E            ARBTblp
158510041027     O                       arbvas
158520041027     O                       arbias
158530041027     O                       arbvmd
158540041027     O                       arbvad
158550041027     O                       arbqft
158560041027     O                       arbctr
158570041028     Ofnblp000  E            RMblp
158580041028     O                       arbrma
158590041028     O                       arbnas
158600071025     Ofnblp000  E            SoloCPIP
158610071025     O                       arbcpi
158620120404     Ofnblp000  E            ARBsdblp
158630120404     O                       arbpkf
158640120404     O                       arbvlf
158650120404     O                       arbfvf
158660120416     O                       arbksc
158670120416     O                       arbctr
158680120416     O                       arbrsm
158690120416     O                       arbinm
158700120416     O                       arbLOM
158710120416     O                       arbCAM
158720120416     O                       arbPRM
158730120416     O                       arbNZM
158740120416     O                       arbcpi
158750120531     o                       arbias
158760120531     o                       arbvas
158770041007     Ofnarb000  E            ARBDarb
158780041025     O                       arbrsd
158790041007     O                       arbind
158800041007     O                       arbcad
158810041007     O                       arblod
158820041007     O                       arbprd
158830041007     O                       arbnzd
158840041007     O                       arbfin
158850041007     O                       arbcpi
158860041007     O                       arbpkb
158870041007     O                       arbpkf
158880041007     O                       arbvlb
158890041007     O                       arbfvb
158900041007     O                       arbvlf
158910041007     O                       arbfvf
158920041007     O                       arbffd
158930041007     O                       arbcts
158940041022     Ofnarb000  E            ARBKarb
158950041022     O                       arbcbo
158960041025     Ofnarb000  E            ARBGarb
158970041022     O                       arbdcr
158980041022     O                       arbhcr
158990041022     O                       arbtcr
159000041022     O                       arbgc1
159010041022     O                       arbgc2
159020041022     O                       arbtc1
159030041022     O                       arbtc2
159040141223     O                       arbfbc
159050141223     O                       arbcmc
159060041025     Ofnarb000  E            ARBMarb
159070041025     O                       arbksc
159080041025     O                       arbrsm
159090041025     O                       arbinm
159100041025     O                       arbcam
159110041025     O                       arblom
159120041025     O                       arbprm
159130041025     O                       arbnzm
159140041025     O                       arbfap
159150041025     O                       arbcpi
159160041025     O                       arbCTR
159170041025     O                       arbfvf
159180041025     O                       arbvlf
159190041027     Ofnarb000  E            ARBTarb
159200041027     O                       arbvas
159210041027     O                       arbias
159220041027     O                       arbvmd
159230041027     O                       arbvad
159240041027     O                       arbqft
159250041027     O                       arbctr
159260041028     Ofnarb000  E            RMarb
159270041028     O                       arbrma
159280041028     O                       arbnas
159290071025     Ofnarb000  E            SoloCPIA
159300071025     O                       arbcpi
159310120404     Ofnarb000  E            ARBsdarb
159320120404     O                       arbpkf
159330120404     O                       arbvlf
159340120404     O                       arbfvf
159350120416     O                       arbksc
159360120416     O                       arbctr
159370120416     O                       arbrsm
159380120416     O                       arbinm
159390120416     O                       arbLOM
159400120416     O                       arbCAM
159410120416     O                       arbPRM
159420120416     O                       arbNZM
159430120416     O                       arbcpi
159440120531     o                       arbias
159450120531     o                       arbvas
159460120411     ofiar6000  e            AR6SD
159470120411     o                       AR6KSC
159480120411     o                       AR6CTR
159490120411     o                       AR6DIV
159500120411     o                       AR6POR
159510120411     o                       AR6SV1
159520120411     o                       AR6VA1
159530120411     o                       AR6SV2
159540120411     o                       AR6VA2
159550120411     o                       AR6SV3
159560120411     o                       AR6VA3
159570120411     o                       ar6imv
159580120411     o                       ar6cei
159590041020     Ofiqdt000  E            AGGqdt
159600041020     O                       qdtnbnbr
159610930507**         GGG
1596209803091234567
159630050601** cmdt
159640050601OVRDBF FILE(TITA430C) TOFILE(
159650151016OVRDBF FILE(FIAR531C) TOFILE(
159660151016OVRDBF FILE(TITAS30C) TOFILE(
159670941014**
159680941102Spedizione al momento non disponibile: gia' utilizzata da un altro utente     01
159690980406Peso mancante                                                                 02
159700980406Volume mancante                                                               03
159710051129Variazione non eseguibile: bolla allocata da altro utente. Attendi e riprova
159720051129Impossibile inserire un volume inferiore a quello parzialmente rilevato da VDL05
159730941102Data formalmente errata                                                       06
159740941102Data consegna richiesta non puo' essere inferiore della data spedizione       07
159750941102Ora richiesta formalmente errata                                              08
159760941102Se immesso tipo immettere anche ora o data consegna richiesta                 09
159770941102Giorno di chiusura errato                                                     10
159780000510Consegna particolare errata o non utilizzabile                                11
159790941102I giorni di chiusura non possono essere uguali
159800941102Il destinatario non puo' essere sempre chiuso!!!?!!
159810941102Le consegne particolari non possono essere uguali
159820051117Impossibile modificare Cons.Part. " " :immessa da partenza no gestib.in arriv 15
159830941102Codice Bolla errato o non utilizzabile
159840941102Codice bolla esistente e nuovo non possono essere uguali
159850941102Impossibile trasformare una bolla da non recupero in recupero o viceversa
159860941213Impossibile effettuare cambi di porto: utilizzare l'apposito programma
159870941213Il codice bolla non ammette il C/Assegno                                      20
159880941102Il codice bolla vuole il C/Assegno: inserire almeno l'importo
159890941102Impossibile variare i campi relativi al C/Assegno perche' gia' incassato
159900071025Codice Fiscale obbligatorio per cliente ITALIA.  Enter per forzare            23
159910131002Mancano i dati passati per il tipo di aggiornamento richiesto                 24
159920941102Se immessa la valuta immettere anche l'importo o eliminare entrambi           25
159930941102Valuta errata
159940000510Non ammessi importi con decimali per la valuta indicata                       27
159950000510Se modificata la divisa, ridigitare il relativo importo                       28
159960941102Impossibile immettere le particolarita' C/Assegno per bolla senza C/A
159970131008$$Sono ammesse solo spedizioni in  FRANCO  per le  POSTE                        30
159980131008Causale variazione errata per il tipo di dati passati                         31
159990000510Spedizione POSTE : peso superiore a xxxxxx,x kg.  Enter per forzare!          32
160000941103Filiale Cliente inesistente
160010941103Per FATTURA IMMEDIATA impossibile specificare codice di altra filiale
160020941103Codice non ammesso se cliente di altra filiale                                35
160030941103Codice cliente errato o inesistente
160040941103Codice cliente non utilizzabile: bloccato in Piano dei Conti
160050941103Se Segue Fattura impossibile indicare il codice 9999
160060011210Importo da Ass. > del limite imposto xxxxxxxxxx,xxx EUR. TEL.IN SEDE X AUTOR. 39
160070941209Importo da Assicurare = 0 : ENTER per prendere il massimo importo da Tariffa  40
160080941103Impossibile eseguire la tassazione: cliente non dell'area
160090941209Importo da Assicurare obbligatorio, come indicato nella tariffa del cliente   42
160100011205Importo da Assicurare da non immettere, come indicato nella tariffa del cli.  43
160110011205Importo da Assicurare SUPERIORE al mandato esposto in tariffa:ENTER X FORZARE 44
160120011205Valore merce dichiarato obbligatorio                                          45
160130941209Immessa Varia "R" per 8888 o 9999 :obbligatorio l'imp.Assicur.Se 0 da tariffa 46
160140941209Codice cliente di altra filiale: F9 per forzare l'immissione
160150941209Sigla varia inesistente                                                       48
160160941209Varia "G" non ammessa                                                         49
160170941209Varia "G" obbligatoria                                                        50
160180941209Importo varia obbligatorio                                                    51
160190000510Spedizione D.P.D. : non ammesso un peso superiore a xxxxxx,x kg.              52
160200990702Non e' stata trovata la tariffa per la tassazione                             53
160210941212Provvigione a zero: non indicata la percentuale in tariffa                    54
160220941212Errore in tassazione                                                          55
160230011018Se eliminato importo C/A eliminare anche il Tipo Incasso e Particolar.        56
160240071025Partita IVA  obbligatoria per cliente ITALIA.  Enter per forzare              57
160250051129Nuovo volume da FATT. inferiore al volume parziale VDL: sara' sostituito      58
160260950420Non e' possibile modificare il tipo bolla: assegnato gia' incassato           59
160270941222Non e' possibile eliminare i dati della fattura se era un PREPAGATO           60
160280941223Non trovata spedizione                                                        61
160290941223Immesso codice cliente: l'indirizzo sara' ignorato                            62
160300950406Anagrafica mittente non completa in Piano dei Conti                           63
160310950406Manca la provincia in anagrafica mittente: inserirla!!!                       64
160320950407Aggiunto contrassegno: aggiungere anche varia G. Enter per forzatura          65
160330950407Eliminato contrassegno: eliminare anche varia G. Enter per forzatura          66
160340950522Partita IVA obbligatoria se bolla diretta all'estero con attraversam. dogana  67
160350950424Q.ta'da fatturare non inseribile per tariffa a valore:assunto importo da assic68
160360991012Azzerata q.tà da fattur.:azzerare anche gli importi di tassazione             69
160370950426Variato cliente o importo da assic.:azzerare q.tà da fattur. e tot.imponibile.70
160380991012Variato cliente o importo da assic.:azzerare q.tà da fattur. e tassazione     71
160390950426Azzerata q.tà da fattur.e tariffa a valore:azzerare anche il tot. imponibile  72
160400961204Impossibile inserire il C/Assegno in bolle di RESO
160410061024Rif. Mittente obbligatorio se destinatario estero con attraversamento dogana  74
160420970110Natura merce obbligatoria                                                     75
160430970826ATTENZIONE!LArrivo desunta, diversa da bolla ma non verra'variata.ENTER forza
160440970808ATTENZIONE! Inoltro modificato. Enter per continuare                          77
160450980309Impossibile inserire il C/Assegno in questa bolla legata                      78
160460011205Importo da Assicurare non ammesso                                             79
160470011210Importo da Assicurare > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER PER FORZARE  80
160480991112Impossibile fare fattura immediata a cliente di altro network                 81
160490990127Impossibile inserire importo da assicurare:bolla su C.A.in Liquidaz.Transatt. 82
160500990419Aggiunto Imp.Assicur.: aggiungere anche varia R. ENTER per forzare            83
160510990419Eliminato Imp.Assicur.: eliminare anche varia R. ENTER per forzare            84
160520120525Cli 8888/9999:se ImportoAssicurare e TotImponibile InserireVaria"R"
160530990419Modificato Imp.Assicur: controllare varia R . ENTER per forzare               86
160540060221Importo da assicurare INFERIORE a quanto previsto dalla legge                 87
160550990701Per i Prepagati, Numero fattura deve essere maggiore di XXXXXX                88
160560990702Prepagato: premere F14 per tassare o inserire tassazione fino all'imponibile  89
160570991006Per codice cliente 8888 o 9999 senza tariffa è obbligatoria la divisa xxx     90
160580011210Importo C/Assegno SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR              91
160590061107Aggiornamento effettuabile soltanto in PARTENZA su spedizioni in Prepagato
160600011010Ammessi importi con massimo __ decimali per la divisa indicata                93
160610991124Divisa obbligatoria se immesso almeno un importo di tassazione                94
160620061107Aggiornamento effettuabile soltanto in ARRIVO su spedizioni Fattura Immediata 95
160630131008$$Bolla cliente POSTE: non si puo' trasformare in C/Servizio                    96
160640131008$$Bolla cliente POSTE: spese C/A sempre a carico del mittente                   97
160650010412Tipo inoltro "D"=Disagiato non ammesso per questo cap/località                98
160660070508Impostare tipo CONTATTO solo se immesso il Fermo Deposito                     99
160670011011Divisa obbligatoria                                                           00
160680011017Manca la varia 'N'                                                            01
160690011102Divisa scaduta                                                                02
160700011120Spedizione giacente: importo C/A non valido                                   03
160710011210Valore merce dichiarato INFERIORE a xxxxxxxxxx,xxx EUR                        04
160720011210Valore merce dichiarato SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR        05
160730011210Valore merce dichiarato > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE  06
160740011206Gli importi inseriti SUPERANO il limite fatturabile, xxxxxxxxxx,xxx xxx       07
160750011210Importo C/Assegno INFERIORE a xxxxxxxxxx,xxx EUR                              08
160760011210Importo C/Assegno > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE        09
160770011206Importo varia SUPERA il limite fatturabile, xxxxxxxxxx,xxx xxx                10
160780020508Discordanza tra codice bolla e codice cliente mittente
160790020531Destinatario               : immettere la relativa consegna particolare       12
160800051116Non è possibile indicare contemporaneamente cons.particolari 'S' e 'X'        13
160810021121Immettere codice tariffa della relativa cartello FedEx                        14
160820021121Non ammessa tariffa FedEx Documenti: peso superiore a Kg. xxxxxx,x            15
160830021121ATTENZIONE! Bolla con Tar.FedEx Doc.e peso > Kg. xxxxxx,x: ENTER X FORZARE    16
160840021129Immettere consegna particolare "S"                                            17
160850030124Immettere il nominativo                                                       18
160860021129Immettere il n. di telefono della persona contattata                          19
160870030124Immettere il motivo                                                           20
160880051129Peso rilevato totalmente da VDL.  Si può inserire solo il peso = a peso VDL   21
160890030123Almeno una delle consegne particolari deve essere "S"                         22
160900051129Impossibile inserire un peso inferiore a quello parzialmente rilevato da VDL  23
160910030124Il n. dei bancali non ritirati non può superare il n. dei bancali bollettati  24
160920030124Immettere il n. dei bancali non ritirati                                      25
160930031001Non è possibile modificare la consegna richiesta                              26
160940040123Spedizione "monovaria". Non inserire altre varie se non immessa la varia " "  27
160950040330La data consegna richiesta deve essere inferiore alla eseguibilità disp.giac. 28
160960040330Data consegna richiesta superiore a ___ giorni                                29
160970131008Codice cliente o linea cliente non utilizzabile                               30
160980041015Prevista consegna ad Uffici:consegna per Appuntam. o a Supermerc. non ammessa 31
160990110512C/A  SUPERIORE al limite xxxxxxxxxx,xxx EUR non ammesso per bancari BRT       32
161000050414Impossibile trasformare prepagato in C/S                                      33
161010061116Modificata ragione sociale DESTINATARIO: controllare correttezza Cod.FIScale  34
161020050603Non ammessi inserimenti di importo da assicurare: bolla non coperta da mandato35
161030141110Variazione non effettuabile:salvataggio in corso. Riprovare (File "        ") 36
161040051220Non è possibile cambiare il mittente da DPD a non DPD e viceversa             37
161050060131Utente non abilitato ad inserire la consegna particolare " "                  38
161060070312Modifica di DataConsegnaRichiesta POSTICIPATA in chiusura distinta:ENTERavanza
161070070308Data consegna richiesta OBBLIGATORIA insieme al Tipo e/o all'Ora
161080070312Consegna Richiesta non modificabile: giacenza aperta o sped.con blocco "G"    41
161090070330Modifica di DataConsegnaRichiesta POSTICIPATA in chiusura distinta            42
161100070508Impostato Fermo Dep.DA CONTATTARE ma presente ConsegnaRich.in bolla:ENTR forza 43
161110070910Togliere Cons.Particolare X-xxxxxxxx:non immessa in partenza e non da Destinat44
161120071025Immettere codice fiscale o partita IVA o uscire con F12                       45
161130091124Inserimento/aumento ImpAssic x bolla non coperta da mandato:inviare MAIL.Forza46
161140091124Volume rilevato totalmente da VDL. Si può inserire solo il volume = al VDL    47
161150091124Impossibile inserire un volume inferiore a quello parzialmente rilevato da VDL48
161160120508P.Iva "$$" non ammessa per fatture emesse in filiale                          49
161170130930Indirizzo e-mail formalmente errato                                           50
161180131016Eseguita tassazione su bolla contabilizzata:non ammesso F6. F12 per uscire    51
161190131016Non effettuate variazioni: non ammesso F6=Conferma. Premere F12 per uscire    52
161200140408Il Fermo Deposito deve essere autorizzato dal Mittente                        53
161210141029Immessa Data Cons. Richiesta ma presente Fermo Deposito in bolla: ENTER forza 54
161220160204Presenti Dispo di Dirott./Reso ancora da elaborare: Fermo Deposito non ammesso55
161230160204Presente Dispo di Dirott./Reso ancora da elaborare: variazione non ammessa    56
161240141112Presente Giacenza aperta: Enter per Forzare                                   57
161250150210Consegna part. per Appuntamento non valida: immessa data consegna "Tassativa" 58
161260150413Spedizione per EXPO: non ammesso il Contrassegno                              59
161270150414Spedizione per EXPO possibile solo per bolle Franco                           60
161280150414Spedizione per EXPO non ammessa per la linea di arrivo                        61
161290151221Spedizione non manutenzionabile per questa Causale Variazione                 62
161300170130Manca tabella Limiti per Tipo Servizio          - TELEFONARE IN SEDE          63
161310170130ATTENZIONE! Sped."   " di peso > xxxxxx,x Kg. non ammessa                     64
161320170130ATTENZIONE! Sped."   " di volume > xx,xxx mc. non ammessa                     65
